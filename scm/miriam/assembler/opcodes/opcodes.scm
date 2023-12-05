(define opcodes-table
  (make-hash-table))

(define-syntax define-opcode
  (syntax-rules ()
    ((_ name _) '()) ;; discard
    ((_ name opcode emitter)
     (hash-table-set! opcodes-table 'name (list opcode emitter)))))

(define-syntax define-instruction
  (syntax-rules (and or)
    ((_ ((name code . locals) ...) ((parser ...) handler) ...)
     (begin
       (define-opcode name code
         (let locals
           (minimeta (or (and parser ... handler) ...)))) ...))))

(define (opcode? sym)
  (let ((entry (hash-table-ref opcodes-table sym (lambda () #f))))
    (and entry (car entry))))

(define (opcode-parser sym)
  (let ((entry (hash-table-ref opcodes-table sym (lambda () #f))))
    (and entry (cadr entry))))

(define (adjust/pcrel n)       (/ (- n 8) 4))
(define (adjust/pcrel-imm12 n) (b& (adjust/pcrel n) #xFFF))

;; ---

(define-instruction
  ((and  #b0000 (s 0))
   (ands #b0000 (s 1))
   (eor  #b0001 (s 0))
   (eors #b0001 (s 1))
   (sub  #b0010 (s 0))
   (subs #b0010 (s 1))
   (rsb  #b0011 (s 0))
   (rsbs #b0011 (s 1))
   (add  #b0100 (s 0))
   (adds #b0100 (s 1))
   (adc  #b0101 (s 0))
   (adcs #b0101 (s 1))
   (sbc  #b0110 (s 0))
   (sbcs #b0110 (s 1))
   (rsc  #b0111 (s 0))
   (rscs #b0111 (s 1))
   (orr  #b1100 (s 0))
   (orrs #b1100 (s 1))
   (bic  #b1110 (s 0))
   (bics #b1110 (s 1)))

  ((opcode? (? condition? #b1110) register? register? shifter?)
   (lambda (t o c rd rn op)
     (case (shifter-mode op)
       ((imm)     (encode/data-processing-immediate       t c o s rd rn (shifter-imm op)))
       ((-imm)    (encode/data-processing-immediate       t c o s rd rn (shifter-imm op)))
       ((reg+imm) (encode/data-processing-immediate-shift t c o s rd rn (shifter-rn op) (shifter-shtyp op) (shifter-shoff op)))
       ((reg-imm) (encode/data-processing-immediate-shift t c o s rd rn (shifter-rn op) (shifter-shtyp op) (shifter-shoff op)))
       ((reg+reg) (encode/data-processing-register-shift  t c o s rd rn (shifter-rn op) (shifter-shtyp op) (shifter-rs op)))))))

(define-instruction
  ((adr  #b0000 (s 0))
   (adrs #b0000 (s 1)))

  ((opcode? (? condition? #b1110) register? symbol-not-register?)
   (lambda (t o c rd label . instr)
     (let* ((pc       (register? 'pc))
            (offset   (emit-relocation t label instr))
            (pcrel    (- offset 8))
            (imm12    (imm12? (abs pcrel)))
            (opcode   (if (<= 0 pcrel) (opcode? 'add) (opcode? 'sub))))
       (if/let ((imm (imm12? (abs pcrel))))
         (encode/data-processing-immediate t c opcode s rd pc imm12)
         (emit-error t "label doesn't fit in adr" instr))))))
       
;; ---

(define-instruction
  ((tst #b1000)
   (teq #b1001)
   (cmp #b1010)
   (cmn #b1011))

  ;; tst rm [+-imm, rm, rm asr +- imm, rm asr rs]
  ((opcode? (? condition? #b1110) register? shifter?)
   (lambda (t o c rn op)
     (case (shifter-mode op)
       ((imm)     (encode/data-processing-immediate       t c o 1 0 rn (shifter-imm op)))
       ((-imm)    (encode/data-processing-immediate       t c o 1 0 rn (shifter-imm op)))
       ((reg+imm) (encode/data-processing-immediate-shift t c o 1 0 rn (shifter-rn op) (shifter-shtyp op) (shifter-shoff op)))
       ((reg-imm) (encode/data-processing-immediate-shift t c o 1 0 rn (shifter-rn op) (shifter-shtyp op) (shifter-shoff op)))
       ((reg+reg) (encode/data-processing-register-shift  t c o 1 0 rn (shifter-rn op) (shifter-shtyp op) (shifter-rs op)))))))

;; ---

(define-instruction
  ((mov  #b1101 (s 0))
   (movs #b1101 (s 1))
   (mvn  #b1111 (s 0))
   (mvns #b1111 (s 1)))

  ;; mov ?c dest [+-imm, rm, rm asr +-imm, rm asr rs]
  ((opcode? (? condition? #b1110) register? shifter?)
   (lambda (t o c rd op)
     (case (shifter-mode op)
       ((imm)     (encode/data-processing-immediate       t c o s rd 0 (shifter-imm op)))
       ((-imm)    (encode/data-processing-immediate       t c o s rd 0 (shifter-imm op)))
       ((reg+imm) (encode/data-processing-immediate-shift t c o s rd 0 (shifter-rn op) (shifter-shtyp op) (shifter-shoff op)))
       ((reg-imm) (encode/data-processing-immediate-shift t c o s rd 0 (shifter-rn op) (shifter-shtyp op) (shifter-shoff op)))
       ((reg+reg) (encode/data-processing-register-shift  t c o s rd 0 (shifter-rn op) (shifter-shtyp op) (shifter-rs op)))))))

(define-instruction
  ((movw #b1000)
   (movt #b1010))

  ((opcode? (? condition? #b1110) register? imm16?)
   (lambda (t o c rd imm)
     (let ((immhi (b>> imm 12))
           (immlo (b&  imm #xFFF)))
       (encode/data-processing-immediate t c o 0 rd immhi immlo)))))

(define-instruction
  ((asr  #b1101 (s 0))
   (asrs #b1101 (s 1))
   (lsl  #b1101 (s 0))
   (lsls #b1101 (s 1))
   (lsr  #b1101 (s 0))
   (lsrs #b1101 (s 1))
   (ror  #b1101 (s 0))
   (rors #b1101 (s 1)))

  ((shift-type? (? condition? #b1110) register? register? imm5?)
   (lambda (t shtyp c rd rm shoff)
     (encode/data-processing-immediate-shift t c (opcode? 'mov) s rd 0 rm shtyp shoff)))

  ((shift-type? (? condition? #b1110) register? register? register?)
   (lambda (t shtyp c rd rm rs)
     (encode/data-processing-register-shift t c (opcode? 'mov) s rd 0 rm shtyp rs))))

;; ---

(define-instruction
  ((b  #b1010 (l? #f))
   (bl #b1010 (l? #t)))

  ;; b ?c +- 
  ((opcode? (? condition? #b1110) simm24?)
   (lambda (t o c imm)
     (let ((adjusted (adjust/pcrel imm)))
       (encode/branch-with-link t c (flag? l?) adjusted))))

  ;; any other number is an error we should report
  ((opcode? (? condition? #b1110) number?)
   (lambda (t o c imm)
     (emit-error t "branch target is too far away, or not word aligned" imm)))
  
  ;; b ?c label
  ((opcode? (? condition? #b1110) symbol-not-register?)
   (lambda (t o c label . instr)
     (let* ((offset   (emit-relocation t label instr))
            (offset   (simm24? offset))
            (adjusted (and offset (adjust/pcrel offset))))
       (log "branch to " label adjusted)
       (if adjusted
           (encode/branch-with-link t c (flag? l?) adjusted)
           (emit-error t "label is too far away, or not word aligned" label))))))

(define-instruction
  ((bx #b1011))

  ;; bx ?c register 
  ((opcode? (? condition? #b1110) register?)
   (lambda (t o c reg)
     (encode/branch-link-exchange t c reg))))

;; ---

(define-instruction
  ((mul  #b000 (s 0))
   (muls #b000 (s 1)))

  ((opcode? (? condition? #b1110) register? register? register?)
   (lambda (t o c rd rn rm)
     (encode/multiply-accumulate t c 0 s rd 0 rm rn))))

(define-instruction
  ((mla  #b000 (s 0))
   (mlas #b000 (s 1)))

  ((opcode? (? condition? #b1110) register? register? register? register?)
   (lambda (t o c rd rn rm ra)
     (encode/multiply-accumulate t c 1 s rd ra rm rn))))

(define-instruction
  ((umull  #b000 (u 0) (a 0) (s 0))
   (umulls #b000 (u 0) (a 0) (s 1))
   (smull  #b000 (u 1) (a 0) (s 0))
   (smulls #b000 (u 1) (a 0) (s 1))
   (umlal  #b000 (u 0) (a 1) (s 0))
   (umlals #b000 (u 0) (a 1) (s 1))
   (smlal  #b000 (u 1) (a 1) (s 0))
   (smlals #b000 (u 1) (a 1) (s 1)))

  ((opcode? (? condition? #b1110) register? register? register? register?)
   (lambda (t o c rdlo rdhi rn rm)
     (encode/multiply-accumulate-long t c u a s rdhi rdlo rn rm))))

;; ---

(define-instruction
  ((clz #b0000))

  ((opcode? (? condition? #b1110) register? register?)
   (lambda (t o c rd rm)
     (encode/clz t c rd rm))))

;; ---

(define-instruction
  ((mrs #b0000))

  ((opcode? (? condition? #b1110) register? sysregister?)
   (lambda (t o c rd spec)
     (encode/move-from-status-register t c spec rd))))

(define-instruction
  ((msr #b0000))

  ((opcode? (? condition? #b1110) sysregister-mask? register?)
   (lambda (t o c mask rn)
     (encode/move-to-status-register t c 0 mask rn)))

  ((opcode? (? condition? #b1110) sysregister-mask? imm12?)
   (lambda (t o c mask imm)
     (encode/move-immediate-to-status-register t c 0 mask imm))))

;; ---

(define-instruction
  ((ldr  #b000 (l? #t) (b? #f))
   (ldrb #b000 (l? #t) (b? #t))
   (str  #b000 (l? #f) (b? #f))
   (strb #b000 (l? #f) (b? #t)))

  ;; ldr r0 start (pc rel label)
  ((opcode? (? condition? #b1110) register? symbol-not-register?)
   (lambda (t o c rd label . instr)
     (let* ((rn     (register? 'pc))
            (offset (emit-relocation t label instr))
            (pcrel  (- offset 8))
            (u?     (<= 0 pcrel))
            (pcrel  (abs pcrel)))
       (encode/load-store-immediate-offset t c #t u? b? #f l? rd rn pcrel))))

  ;; ldr r0 (++ r1) [+-#imm][rn][rn rrx][rn lsl imm][rn lsl rs]
  ((opcode? (? condition? #b1110) register? target? (? shifter? '(imm 0)))
   (lambda (t o c rd target shifter)
     (let ((p? (target-p? target))
           (w? (target-w? target)))
       (case (shifter-mode shifter)
         ((imm)     (encode/load-store-immediate-offset t c p? #t b? w? l? rd (target-rn target) (shifter-imm shifter)))
         ((-imm)    (encode/load-store-immediate-offset t c p? #f b? w? l? rd (target-rn target) (shifter-imm shifter)))
         ((reg+imm) (encode/load-store-register-offset t c p? #t b? w? l? rd (target-rn target) (shifter-rn shifter) (shifter-shtyp shifter) (shifter-shoff shifter)))
         ((reg-imm) (encode/load-store-register-offset t c p? #f b? w? l? rd (target-rn target) (shifter-rn shifter) (shifter-shtyp shifter) (shifter-shoff shifter)))
         ((reg+reg) (encode/load-store-register-offset t c p? #t b? w? l? rd (target-rn target) (shifter-rn shifter) (shifter-shtyp shifter) (shifter-rs shifter)))
         ((reg-reg) (encode/load-store-register-offset t c p? #f b? w? l? rd (target-rn target) (shifter-rn shifter) (shifter-shtyp shifter) (shifter-rs shifter))))))))

;; ---

(define-instruction
  ((ldrt  #b000 (l? #t) (b? #f))
   (ldrbt #b000 (l? #t) (b? #t))
   (strt  #b000 (l? #f) (b? #f))
   (strbt #b000 (l? #f) (b? #t)))
  
  ((opcode? (? condition? #b1110) register? register? (? shifter? '(imm 0)))
   (lambda (t o c rd target shifter . instr)
     (case (shifter-mode shifter)
       ((imm)     (encode/load-store-immediate-offset t c #f #t b? #t l? rd (target-rn target) (shifter-imm shifter)))
       ((-imm)    (encode/load-store-immediate-offset t c #f #f b? #t l? rd (target-rn target) (shifter-imm shifter)))
       ((reg+imm) (encode/load-store-register-offset t c #f #t b? #t l? rd (target-rn target) (shifter-rn shifter) (shifter-shtyp shifter) (shifter-shoff shifter)))
       ((reg-imm) (encode/load-store-register-offset t c #f #f b? #t l? rd (target-rn target) (shifter-rn shifter) (shifter-shtyp shifter) (shifter-shoff shifter)))
       ((reg+reg) (encode/load-store-register-offset t c #f #t b? #t l? rd (target-rn target) (shifter-rn shifter) (shifter-shtyp shifter) (shifter-rs shifter)))
       ((reg-reg) (encode/load-store-register-offset t c #f #f b? #t l? rd (target-rn target) (shifter-rn shifter) (shifter-shtyp shifter) (shifter-rs shifter)))))))
            
;; ---

(define-instruction
  ((swp  #b000 (b? #f))
   (swbp #b000 (b? #t)))

  ((opcode? (? condition? #b1110) register? register? register?)
   (lambda (t o c rd rm rn)
     (encode/swap t c b? rd rn rm))))

;; ---

(define-instruction
  ((bkpt #b000))

  ((opcode? u-hword)
   (lambda (t o imm)
     (encode/software-breakpoint t #b1110 imm))))

(define-instruction
  ((swi #b000)
   (svc #b000))

  ((opcode? (? condition? #b1110) imm24?)
   (lambda (t o c imm)
     (encode/software-interrupt t c imm))))

;; ---

(define-instruction
  ((cdp #b000))

  ((opcode? (? condition? #b1110) copro? imm4? copro-register? copro-register? copro-register? (? imm3? #b000))
   (lambda (t o c cpnum op1 crd crn crm op2)
     (encode/copro-data-processing t c op1 crn crd cpnum op2 crm))))

(define-instruction
  ((cdp2 #b000))

  ((opcode? copro? imm4? copro-register? copro-register? copro-register? (? imm3? #b000))
   (lambda (t o cpnum op1 crd crn crm op2)
     (encode/copro-data-processing t #b1111 op1 crn crd cpnum op2 crm))))

(define-instruction
  ((mrc #b000))

  ((opcode? (? condition? #b1110) copro? imm4? register? copro-register? copro-register? (? imm3? #b000))
   (lambda (t o c cpnum op1 rt crn crm op2)
     (encode/copro-register-transfer t c op1 #t crn rt cpnum op2 crm))))

(define-instruction
  ((mrc2 #b000))

  ((opcode? copro? imm4? register? copro-register? copro-register? (? imm3? #b000))
   (lambda (t o cpnum op1 rt crn crm op2)
     (encode/copro-register-transfer t #b1111 op1 #t crn rt cpnum op2 crm))))

(define-instruction
  ((mcr #b000))

  ((opcode? (? condition? #b1110) copro? imm4? register? copro-register? copro-register? (? imm3? #b000))
   (lambda (t o c cpnum op1 rt crn crm op2)
     (encode/copro-register-transfer t c op1 #f crn rt cpnum op2 crm))))

(define-instruction
  ((mcr2 #b000))

  ((opcode? copro? imm4? register? copro-register? copro-register? (? imm3? #b000))
   (lambda (t o cpnum op1 rt crn crm op2)
     (encode/copro-register-transfer t #b1111 op1 #f crn rt cpnum op2 crm))))

;; (define-opcode bfc     #b0111110   bfc-emit)
;; (define-opcode bfi     #b0111110   bfi-emit)

;; (define-opcode clrex   #b11110101011111111111000000011111 diect-emit)


;; (define-opcode cps     'not-implemented) ;; system
;; (define-opcode cpy     'not-implemented) ;; no support for deprecated synonyms

;; (define-opcode csdb    #b11100011001000001111000000010100 direct-emit)
;; (define-opcode dbg     #b001100100000111100001111         dbg-emit)

;; (define-opcode dmb     #b1111010101111111111100000101     barrier-emit)
;; (define-opcode dsb     #b1111010101111111111100000100     barrier-emit)
;; (define-opcode isb     #b1111010101111111111100000110     barrier-emit)

;; (define-opcode eret    'not-implemented) ;; todo: exception return (system)
;; (define-opcode hvc     'not-implemented) ;; todo: hypervisor call (system)

;; (define-opcode ldc     'not-implemented) ;; todo: coprocessors
;; (define-opcode ldc2    'not-implemented)

;; (define-opcode ldm     #b100010    ldm-emit)
;; (define-opcode ldmia   #b100010    ldm-emit)
;; (define-opcode ldmfd   #b100010    ldm-emit)
;; (define-opcode ldmda   #b100000    ldm-emit)
;; (define-opcode ldmfa   #b100000    ldm-emit)
;; (define-opcode ldmdb   #b100100    ldm-emit)
;; (define-opcode ldmea   #b100100    ldm-emit)
;; (define-opcode ldmib   #b100110    ldm-emit)
;; (define-opcode ldmeb   #b100110    ldm-emit)

;; (define-opcode ldr     #b010       (ldr-emit  '()))
;; (define-opcode ldrb    #b010       (ldr-emit  '(sizemod)))
;; (define-opcode ldrd    #b1101      (ldrx-emit '(sizemod)))
;; (define-opcode ldrh    #b1011      (ldrx-emit '()))
;; (define-opcode ldrsb   #b1101      (ldrx-emit '()))
;; (define-opcode ldrsh   #b1111      (ldrx-emit '()))

;; (define-opcode ldrt    'not-implemented) ;; #b010       (ldr-emit '()))
;; (define-opcode ldrbt   'not-implemented) ;; #b010       (ldr-emit '(sizemod)))
;; (define-opcode ldrht   'not-implemented)
;; (define-opcode ldrsht  'not-implemented)
;; (define-opcode ldrsbt  'not-implemented)

;; (define-opcode ldrex   'not-implemented)
;; (define-opcode ldrexb  'not-implemented)
;; (define-opcode ldrexh  'not-implemented)
;; (define-opcode ldrexd  'not-implemented)

;; (define-opcode mrc     'not-implemented)
;; (define-opcode mrc2    'not-implemented)
;; (define-opcode mrrc    'not-implemented)
;; (define-opcode mrrc2   'not-implemented)
;; (define-opcode mla     'not-implemented)
;; (define-opcode mlas    'not-implemented)
;; (define-opcode mls     'not-implemented)
;; (define-opcode mul     'not-implemented)
;; (define-opcode muls    'not-implemented)
;; (define-opcode qadd    'not-implemented)
;; (define-opcode qdadd   'not-implemented)
;; (define-opcode qdsub   'not-implemented)
;; (define-opcode qsub    'not-implemented)
;; (define-opcode smlabb  'not-implemented)
;; (define-opcode smlabt  'not-implemented)
;; (define-opcode smlal   'not-implemented)
;; (define-opcode smlalbb 'not-implemented)
;; (define-opcode smlalbt 'not-implemented)
;; (define-opcode smlals  'not-implemented)
;; (define-opcode smlaltb 'not-implemented)
;; (define-opcode smlaltt 'not-implemented)
;; (define-opcode smlatb  'not-implemented)
;; (define-opcode smlatt  'not-implemented)
;; (define-opcode smlawb  'not-implemented)
;; (define-opcode smlawt  'not-implemented)
;; (define-opcode smulbb  'not-implemented)
;; (define-opcode smulbt  'not-implemented)
;; (define-opcode smull   'not-implemented)
;; (define-opcode smulls  'not-implemented)
;; (define-opcode smultb  'not-implemented)
;; (define-opcode smultt  'not-implemented)
;; (define-opcode smulwb  'not-implemented)
;; (define-opcode smulwt  'not-implemented)
;; (define-opcode strd  'not-implemented)
;; (define-opcode strex  'not-implemented)
;; (define-opcode strexb 'not-implemented)
;; (define-opcode strexd 'not-implemented)
;; (define-opcode strexh 'not-implemented)
;; (define-opcode strh  'not-implemented)
;; (define-opcode strht  'not-implemented)
;; (define-opcode swp    'not-implemented)
;; (define-opcode swpb   'not-implemented)
;; (define-opcode umaal   'not-implemented)
;; (define-opcode umlal   'not-implemented)
;; (define-opcode umlals  'not-implemented)
;; (define-opcode umull   'not-implemented)
;; (define-opcode umulls  'not-implemented)
