(define opcodes-table
  (make-hash-table))

(define-syntax define-opcode
  (syntax-rules ()
    ((_ name _) '()) ;; discard
    ((_ name opcode emitter)
     (hash-table-set! opcodes-table 'name (list opcode emitter)))))

(define (opcode? sym)
  (let ((entry (hash-table-ref opcodes-table sym (lambda () #f))))
    (and entry (car entry))))

(define (opcode-parser sym)
  (let ((entry (hash-table-ref opcodes-table sym (lambda () #f))))
    (and entry (cadr entry))))

;; ---

(define (parse/data-arity1 s?)
  (minimeta
   (and opcode? (? condition? #b1110) register? operand?
        (lambda (t o c rd op)
          (let ((s (flag? s?)))
            (case (operand-mode op)
              ((imm)     (encode/data-processing-immediate       t c o s rd 0 (operand-imm op)))
              ((reg+imm) (encode/data-processing-immediate-shift t c o s rd 0 (operand-rn op) (operand-shtyp op) (operand-shoff op)))
              ((reg+reg) (encode/data-processing-register-shift  t c o s rd 0 (operand-rn op) (operand-shtyp op) (operand-rs op)))))))))

(define (parse/data-arity2 s?)
  (minimeta
   (and opcode? (? condition? #b1110) register? operand?
        (lambda (t o c rn op)
          (let ((s (flag? s?)))
            (case (operand-mode op)
              ((imm)     (encode/data-processing-immediate       t c o s 0 rn (operand-imm op)))
              ((reg+imm) (encode/data-processing-immediate-shift t c o s 0 rn (operand-rn op) (operand-shtyp op) (operand-shoff op)))
              ((reg+reg) (encode/data-processing-register-shift  t c o s 0 rn (operand-rn op) (operand-shtyp op) (operand-rs op)))))))))

(define (parse/data-arity3 s?)
  (minimeta
   (and opcode? (? condition? #b1110) register? register? operand?
        (lambda (t o c rd rn op)
          (let ((s (flag? s?)))
            (case (operand-mode op)
              ((imm)     (encode/data-processing-immediate       t c o s rd rn (operand-imm op)))
              ((reg+imm) (encode/data-processing-immediate-shift t c o s rd rn (operand-rn op) (operand-shtyp op) (operand-shoff op)))
              ((reg+reg) (encode/data-processing-register-shift  t c o s rd rn (operand-rn op) (operand-shtyp op) (operand-rs op)))))))))

(define-opcode and  #b0000 (parse/data-arity3 #f))
(define-opcode ands #b0000 (parse/data-arity3 #t))
(define-opcode eor  #b0001 (parse/data-arity3 #f))
(define-opcode eors #b0001 (parse/data-arity3 #t))
(define-opcode sub  #b0010 (parse/data-arity3 #f))
(define-opcode subs #b0010 (parse/data-arity3 #t))
(define-opcode rsb  #b0011 (parse/data-arity3 #f))
(define-opcode rsbs #b0011 (parse/data-arity3 #t))
(define-opcode add  #b0100 (parse/data-arity3 #f))
(define-opcode adds #b0100 (parse/data-arity3 #t))
(define-opcode adc  #b0101 (parse/data-arity3 #f))
(define-opcode adcs #b0101 (parse/data-arity3 #t))
(define-opcode sbc  #b0110 (parse/data-arity3 #f))
(define-opcode sbcs #b0110 (parse/data-arity3 #t))
(define-opcode rsc  #b0111 (parse/data-arity3 #f))
(define-opcode rscs #b0111 (parse/data-arity3 #t))
(define-opcode tst  #b1000 (parse/data-arity2 #t))
(define-opcode teq  #b1001 (parse/data-arity2 #t))
(define-opcode cmp  #b1010 (parse/data-arity2 #t))
(define-opcode cmn  #b1011 (parse/data-arity2 #t))
(define-opcode orr  #b1100 (parse/data-arity3 #f))
(define-opcode orrs #b1100 (parse/data-arity3 #t))
(define-opcode mov  #b1101 (parse/data-arity1 #f))
(define-opcode movs #b1101 (parse/data-arity1 #t))
(define-opcode bic  #b1110 (parse/data-arity3 #f))
(define-opcode bics #b1110 (parse/data-arity3 #t))
(define-opcode mvn  #b1111 (parse/data-arity1 #f))
(define-opcode mvns #b1111 (parse/data-arity1 #t))

;; ---

(define (parse/mov-alias s?)
  (let ((s (flag? s?))
        (o (opcode? 'mov)))
    (minimeta
     (and shift-type? (? condition? #b1110) register? register?
          (or (and imm5?
                   (lambda (t shtyp c rd rm shoff)
                     (encode/data-processing-immediate-shift t c o s rd 0 rm shtyp shoff)))
              (and register?
                   (lambda (t shtyp c rd rm rs)
                     (encode/data-processing-register-shift t c o s rd 0 rm shtyp rs))))))))

(define-opcode asr  #b1101 (parse/mov-alias #f))
(define-opcode asrs #b1101 (parse/mov-alias #t))
(define-opcode lsl  #b1101 (parse/mov-alias #f))
(define-opcode lsls #b1101 (parse/mov-alias #t))
(define-opcode lsr  #b1101 (parse/mov-alias #f))
(define-opcode lsrs #b1101 (parse/mov-alias #t))
(define-opcode ror  #b1101 (parse/mov-alias #f))
(define-opcode rors #b1101 (parse/mov-alias #t))

(define parse/movw
  (minimeta
   (and opcode? (? condition? #b1110) register? imm16?
        (lambda (t o c rd imm)
          (let ((immhi (b>> imm 12))
                (immlo (b&  imm #xFFF)))
            (encode/data-processing-immediate t c o 0 rd immhi immlo))))))

(define-opcode movw #b1000 parse/movw)

;; ---

(define (parse/multiply s?)
  (let ((s (flag? s?)))
    (minimeta
     (and opcode? (? condition? #b1110) register? register? register?
          (lambda (t o c rd rn rm)
            (encode/multiply-accumulate t c 0 s rd 0 rm rn))))))

(define (parse/multiply-accum s?)
  (let ((s (flag? s?)))
    (minimeta
     (and opcode? (? condition? #b1110) register? register? register? register?
          (lambda (t o c rd rn rm ra)
            (encode/multiply-accumulate t c 1 s rd ra rm rn))))))

(define-opcode mul  #b000 (parse/multiply #f))
(define-opcode muls #b000 (parse/multiply #t))
(define-opcode mla  #b000 (parse/multiply-accum #f))
(define-opcode mlas #b000 (parse/multiply-accum #t))

(define (parse/multiply-long u? a? s?)
  (let ((u (flag? u?))
        (a (flag? a?))
        (s (flag? s?)))
    (minimeta
     (and opcode? (? condition? #b1110) register? register? register? register?
          (lambda (t o c rdlo rdhi rn rm)
            (encode/multiply-accumulate-long t c u a s rdhi rdlo rn rm))))))

(define-opcode umull  #b000 (parse/multiply-long #f #f #f))
(define-opcode umulls #b000 (parse/multiply-long #f #f #t))
(define-opcode smull  #b000 (parse/multiply-long #t #f #f))
(define-opcode smulls #b000 (parse/multiply-long #t #f #t))
(define-opcode umlal  #b000 (parse/multiply-long #f #t #f))
(define-opcode umlals #b000 (parse/multiply-long #f #t #t))
(define-opcode smlal  #b000 (parse/multiply-long #t #t #f))
(define-opcode smlals #b000 (parse/multiply-long #t #t #t))

;; ---

(define parse/clz
  (minimeta
   (and opcode? (? condition? #b1110) register? register?
        (lambda (t o c rd rm)
          (encode/clz t c rd rm)))))

(define-opcode clz    #b000 parse/clz)

;; ---

(define parse/mrs-read-status
  (minimeta
   (and opcode? (? condition? #b1110) register? sysregister?
        (lambda (t o c rd spec)
          (encode/move-from-status-register t c spec rd)))))

(define parse/msr-write-status
  (minimeta
   (and opcode? (? condition? #b1110) sysregister-mask?
        (or (and register?
                 (lambda (t o c mask rn)
                   (encode/move-to-status-register t c 0 mask rn)))
            (and imm12?
                 (lambda (t o c mask imm)
                   (encode/move-immediate-to-status-register t c 0 mask imm)))))))

(define-opcode mrs #b000 parse/mrs-read-status)
(define-opcode msr #b000 parse/msr-write-status)

;; (define-opcode adr     #b00000     adr-emit) ;; special mnemonics for add/sub + pc-rel
;; (define-opcode adrs    #b00001     adr-emit)
;; (define-opcode b       #b1010      b-emit)
;; (define-opcode bfc     #b0111110   bfc-emit)
;; (define-opcode bfi     #b0111110   bfi-emit)
;; (define-opcode bkpt    #b00010010  bkpt-emit)
;; (define-opcode bl      #b1011      b-emit)

;; (define-opcode cdp     'not-implemented) ;; todo: coprocessors
;; (define-opcode cdp2    'not-implemented)

;; (define-opcode clrex   #b11110101011111111111000000011111 direct-emit)


;; (define-opcode cps     'not-implemented) ;; todo: change processor state (system)
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
