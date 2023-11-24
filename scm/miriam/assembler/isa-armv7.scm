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

(define (opcode-fn sym)
  (let ((entry (hash-table-ref opcodes-table sym (lambda () #f))))
    (and entry (cadr entry))))

;; ---

(define (flag? n) (if n 1 0))

(define (emit-instruction out bytelist)
  (display (list "emit: " (map (lambda (n) (number->string n 16)) bytelist)))
  (newline))

(define (record-relocation out type label)
  (display (list "recording relocation:" type label))
  (newline))

(define b&  bitwise-and)
(define b+  bitwise-ior)
(define b<< arithmetic-shift)
(define b>> (lambda (n amount) (arithmetic-shift n (- amount))))

;; direct emit of full opcodes

(define direct-emit
  (minimeta
   (and opcode?
        (lambda (t o)
          (emit-instruction t (integer->bytelist o 4))))))

;; direct emit of conditional opcodes

(define cond-direct-emit
  (minimeta
   (and opcode? (? condition? #b1110)
        (lambda (t o c)
          (let ((out 0))
            (set! out (b>> (b+ out c) 28))
            (set! out (b+ out o))
            (emit-instruction t (integer->bytelist out 4)))))))

;; data processing (register)
;; 1 30 9 8   7 6 5   4 3 2 1 20   9 8 7 6 5 4 3 2   1 10 9 8 7   6 5    4   3 2 1 0
;; cond     | 0 0 0 | op1        | Rn     | Rd     | imm5       | type | 0 | Rm

;; (ne? add r0 r1 r1 (lsl 3))
(define (data-reg-emit t c op1 rd rn rm shtyp shoff)
  (let ((out #x00000000))
    (set! out (b<< (b+ out c)     8)) ;; add the cond flags
    (set! out (b<< (b+ out op1)   4)) ;; add the opcode + 1 for the S bit
    (set! out (b<< (b+ out rn)    4)) ;; add the input reg
    (set! out (b<< (b+ out rd)    5)) ;; add the dest reg
    (set! out (b<< (b+ out shoff) 2)) ;; add the shift offset
    (set! out (b<< (b+ out shtyp) 5)) ;; add the shift type
    (set! out (b+ out rm))
    (emit-instruction t (integer->bytelist out 4))))

;; data processing (register-shifted register)
;; 1 30 9 8   7 6 5   4 3 2 1 20   9 8 7 6 5 4 3 2 1 10 9 8   7   6 5   4   3 2 1 0
;; cond     | 0 0 0 | op1        | --                       | 0 | op2 | 1 |  --

(define (data-regsr-emit t c op1 rd rn rm shtyp rs)
  (let ((out 0))
    (set! out (b<< (b+ out c)     8)) ;; add the cond flags
    (set! out (b<< (b+ out op1)   4)) ;; add the opcode + 1 for the S bit
    (set! out (b<< (b+ out rn)    4)) ;; add the input reg
    (set! out (b<< (b+ out rd)    4)) ;; add the dest reg
    (set! out (b<< (b+ out rs)    3)) ;; add the shift register
    (set! out (b<< (b+ out shtyp) 1)) ;; add the shift type
    (set! out (b<< (b+ out #b1)   4)) ;; add the rs flag
    (set! out (b+ out rm))
    (emit-instruction t (integer->bytelist out 4))))

;; data processing (immediate 12bit)
;; 1 30 9 8   7 6 5   4 3 2 1 20   9 8 7 6   5 4 3 2   1 10 9 8   7 6 5 4 3 2 1 0
;; cond     | 0 0 1 | op1        | Rn      | --      | rotation | a b c d e f g h

(define (data-regimm12-emit t c op1 rd rn imm)
  (let ((out 0))
    (set! out (b<< (b+ out c)     3)) ;; cond flags
    (set! out (b<< (b+ out #b001) 5)) ;; const
    (set! out (b<< (b+ out op1)   4)) ;; opcode
    (set! out (b<< (b+ out rn)    4))
    (set! out (b<< (b+ out rd)   12))
    (set! out (b+ out imm))
    (emit-instruction t (integer->bytelist out 4))))

;; data processing (immediate 16bit)
;; 1 30 9 8   7 6 5 4   3 2 1 20   9 8 7 6   5 4 3 2   1 10 9 8 7 6 5 4 3 2 1 0
;; cond     | 0 0 1 1 | op1      | imm4    | Rd      | imm12

(define (data-regimm16-emit t c op1 rd rn imm)
  (let ((out  0)
        (imm4  (b>> imm 12)) ; hi-4 bits
        (imm12 (b&  imm #xFFF)))
    (set! out (b<< (b+ out c)      4)) ;; cond flags
    (set! out (b<< (b+ out #b0011) 4)) ;; const
    (set! out (b<< (b+ out op1)    4)) ;; opcode
    (set! out (b<< (b+ out imm4)   4))
    (set! out (b<< (b+ out rd)    12))
    (set! out (b+ out imm12))
    (emit-instruction t (integer->bytelist out 4))))

(define (data-emit flags)
  (minimeta
   (and opcode? (? condition? #b1110) register? register?
        (or (and (! (memv 'imm12 flags)) imm12?
                 (lambda (t o c rd rn imm)
                   (data-regimm12-emit t c o rd rn imm)))
            (and (! (memv 'imms16 flags)) s-hword
                 (lambda (t o c rd rn imm)
                   (data-regimm16-emit t c o rd rn imm)))
            (and register? (or (and shift-w-imm?
                                    (lambda (t o c rd rn rm sh)
                                      (data-reg-emit t c o rd rn rm (car sh) (cdr sh))))
                               (and (! (memv 'rsr flags)) shift-w-reg?
                                    (lambda (t o c rd rn rm sh)
                                      (data-regsr-emit t c o rd rn rm (car sh) (cdr sh))))
                               (lambda (t o c rd rn rm)
                                 (data-reg-emit t c o rd rn rm #b0000 0))))))))

;; effectively (add/sub pc imm) depending on direction
;; symbol emits a relocation pointer
(define adr-emit
  (let ((pc (register? 'pc)))
    (minimeta
     (and opcode? (? condition? #b1110) register?
          (or (and imm12? (lambda (t s c rd off)
                            (let ((o  (opcode? (if (= 0 s) 'add 'adds))))
                              (data-regimm12-emit t c o rd pc off))))
              (and -imm12? (lambda (t s c rd off)
                             (let ((o   (opcode? (if (= 0 s) 'sub 'subs)))
                                   (off (abs off)))
                               (data-regimm12-emit t c o rd pc off))))
              (and symbol? (lambda (t s c rd label)
                             (let ((o   (opcode? (if (= 0 s) 'add 'adds))))
                               (record-relocation t 'pcrel label)
                               (data-regimm12-emit t c o rd pc #b0000)))))))))

;; branch to target address
;; 1 30 9 8   7 6 5 4   3 2 1 20 9 8 7 6 5 4 3 2   1 10 9 8 7 6 5 4 3 2 1 0
;; cond     | op1     | imm24 (pc := imm24 << 2)

(define (b-imm24-emit t c op1 imm)
  (let ((out 0)
        (imm (b& imm #xFFFFFF)))      ;; 24bits, defeat sign extension
    (set! out (b<< (b+ out c)     4)) ;; cond flags
    (set! out (b<< (b+ out op1)  24)) ;; opcode
    (set! out (b+ out imm))
    (emit-instruction t (integer->bytelist out 4))))

(define b-emit
  (minimeta
   (and opcode? (? condition? #b1110) symbol?
        (lambda (t o c label)
          (record-relocation t 'pcrel label)
          (b-imm24-emit t c o (s-relword -3))))))

;; bit-field manipulation
;; 1 30 9 8   7 6 5 4 3 2 1   20 9 8 7 6   5 4 3 2   1 10 9 8 7   6 5 4   3 2 1 0
;; cond     | op1           | msb        | rd      | lsb        | 0 0 1 | rn

(define (bf-imm10-emit t c op1 msb lsb rd rn)
  (let ((out 0))
    (set! out (b<< (b+ out c)     7))
    (set! out (b<< (b+ out op1)   5))
    (set! out (b<< (b+ out msb)   4))
    (set! out (b<< (b+ out rd)    5))
    (set! out (b<< (b+ out lsb)   3))
    (set! out (b<< (b+ out #b001) 4))
    (set! out (b+ out rn))
    (emit-instruction t (integer->bytelist out 4))))

(define bfc-emit
  (let ((lsb?   (lambda (x) (integer-within? 0 x 31)))
        (width? (lambda (x) (integer-within? 1 x 32))))
    (minimeta
     (and opcode? (? condition? #b1110) register? lsb? width?
          (lambda (t o c rd lsb width)
            (and (<= width (- 32 lsb))
                 (let ((msb (+ lsb (- width 1))))
                   (bf-imm10-emit t c o msb lsb rd #b1111))))))))

(define bfi-emit
  (let ((lsb?   (lambda (x) (integer-within? 0 x 31)))
        (width? (lambda (x) (integer-within? 1 x 32))))
    (minimeta
     (and opcode? (? condition? #b1110) register? register? lsb? width?
          (lambda (t o c rd rn lsb width)
            (and (<= width (- 32 lsb))
                 (let ((msb (+ lsb (- width 1))))
                   (bf-imm10-emit t c o msb lsb rd rn))))))))

;; breakpoint - software interrupt
;; 1 30 9 8   7 6 5 4 3 2 1 20   9 8 7 6 5 4 3 2 1 10 9 8   7 6 5 4   3 2 1 0
;; cond     | op1              | imm12                    | 0 1 1 1 | imm4

(define (bkpt-imm16-emit t op1 imm)
  (let* ((out  0)
         (c     #b1110)                 ; breakpoint is always unconditional
         (imm12 (b& (b>> imm 4) #xFFF)) ; hi 12 bits
         (imm4  (b& imm #b1111)))       ; lo-4 bits
    (set! out (b<< (b+ out c)      8)) ;; cond flags
    (set! out (b<< (b+ out op1)   12)) ;; const
    (set! out (b<< (b+ out imm12)  4))
    (set! out (b<< (b+ out #b0111) 4)) ;; opcode
    (set! out (b+ out imm4))
    (emit-instruction t (integer->bytelist out 4))))

(define bkpt-emit
  (minimeta
   (and opcode? u-hword
        (lambda (t o imm) (bkpt-imm16-emit t o imm)))))

;; count leading zeros
;; 1 30 9 8   7 6 5 4 3 2 1 20   9 8 7 6   5 4 3 2   1 10 9 8   7 6 5 4   3 2 1 0
;; cond     | op1              | 1 1 1 1 | rd      | 1 1  1 1 | 0 0 0 1 | rm

(define (clz-reg-emit t c op1 rd rm)
  (let ((out 0))
    (set! out (b<< (b+ out c)          8))
    (set! out (b<< (b+ out op1)        4))
    (set! out (b<< (b+ out #b1111)     4))
    (set! out (b<< (b+ out rd)         8))
    (set! out (b<< (b+ out #b11110001) 4))
    (set! out (b+ out rm))
    (emit-instruction t (integer->bytelist out 4))))

(define clz-emit
  (minimeta
   (and opcode? (? condition? #b1110) register? register?
        (lambda (t o c rd rm)
          (clz-reg-emit t c o rd rm)))))

;; debug hint
;; 1 30 9 8   7 6 5 4 3 2 1 20   9 8 7 6   5 4 3 2   1 10 9 8   7 6 5 4   3 2 1 0
;; cond     | 0 0 1 1 0 0 1  0 | 0 0 0 0 | 1 1 1 1 | 0 0  0 0 | 1 1 1 1 | imm4

(define dbg-emit
  (minimeta
   (and opcode? (? condition? #b1110) u-hbyte
        (lambda (t o c imm4)
          (let ((out 0))
            (set! out (b<< (b+ out c) 24))
            (set! out (b<< (b+ out o)  4))
            (set! out (b+ out imm4))
            (emit-instruction t (integer->bytelist out 4)))))))

;; barrier + options

(define barrier-emit
  (minimeta
   (and opcode? barrier-option?
        (lambda (t o opt)
          (let ((out 0))
            (set! out (b<< (b+ out o) 4))
            (set! out (b+ out opt))
            (emit-instruction t (integer->bytelist out 4)))))))

;; load multiple + action
;; 1 30 9 8   7 6 5 4 3 2   1   20  9 8 7 6   5 4 3 2 1 10 9 8 7 6 5 4 3 2 1 0
;; cond     | op1         | w | 1 | rn      | register-list

(define ldm-emit
  (minimeta
   (and opcode? (? condition? #b1110) wregister? reglist?
        (lambda (t o c wrn rl)
          (let ((out 0)
                (rn (wregister-r  wrn))
                (w? (wregister-w? wrn)))
            (set! out (b<< (b+ out c)          6))
            (set! out (b<< (b+ out o)          1))
            (set! out (b<< (b+ out (flag? w?)) 1))
            (set! out (b<< (b+ out 1)          4))
            (set! out (b<< (b+ out rn)        16))
            (set! out (b+ out rl))
            (emit-instruction t (integer->bytelist out 4)))))))

;; ldr immediate emit
;;                      add
;; 1 30 9 8   7 6 5   4 3 2   1   20  9 8 7 6   5 4 3 2   1 10 9 8 7 6 5 4 3 2 1 0
;; cond     | op1   | P U S | W | 1 | rn      | rt      | imm12
;;                    index   writeback

(define (ldr-imm-emit t c o p? u? s? w? rn rt imm12)
  (let ((out   0))
    (set! out (b<< (b+ out c)          3))
    (set! out (b<< (b+ out o)          1))
    (set! out (b<< (b+ out (flag? p?)) 1))
    (set! out (b<< (b+ out (flag? u?)) 1))
    (set! out (b<< (b+ out (flag? s?)) 1))
    (set! out (b<< (b+ out (flag? w?)) 1))
    (set! out (b<< (b+ out 1)          4))
    (set! out (b<< (b+ out rn)         4))
    (set! out (b<< (b+ out rt)        12))
    (set! out (b+ out imm12))
    (emit-instruction t (integer->bytelist out 4))))

;; ldr regsr emit
;;                     add
;; 1 30 9 8   7 6 5  4 3 2   1   20  9 8 7 6   5 4 3 2   1 10 9 8 7   6 5   4   3 2 1 0
;; cond     | op1  | P U S | W | 1 | rn      | rt      | imm12      |     | 0 | rm
;;                   index   writeback

(define (ldr-regsr-emit t c o p? u? s? w? rn rt rm typ sh)
  (let ((out 0))
    (set! out (b<< (b+ out c)          3))
    (set! out (b<< (b+ out (+ o 1))    1))
    (set! out (b<< (b+ out (flag? p?)) 1))
    (set! out (b<< (b+ out (flag? u?)) 1))
    (set! out (b<< (b+ out (flag? s?)) 1))
    (set! out (b<< (b+ out (flag? w?)) 1))
    (set! out (b<< (b+ out 1)          4))
    (set! out (b<< (b+ out rn)         4))
    (set! out (b<< (b+ out rt)         5))
    (set! out (b<< (b+ out sh)         2))
    (set! out (b<< (b+ out typ)        1))
    (set! out (b<< (b+ out 0)          4))
    (set! out (b+ out rm))
    (emit-instruction t (integer->bytelist out 4))))

(define (ldr-emit flags)
  (let ((s? (memv 'sizemod flags)))
    (minimeta
     (and opcode? (? condition? #b1110) register?
          (or (and register?
                   ;; no pre/post index: p? = #t, w? = #f
                   (or (and (lambda (t o c rt rn)
                              (ldr-imm-emit t c o #t #t s? #f rn rt 0)))
                       (and imm12?
                            (lambda (t o c rt rn imm)
                              (let ((u?  (positive? imm))
                                    (off (abs imm)))
                                (ldr-imm-emit t c o #t u? s? #f rn rt off))))
                       (and sregister? (? shift-w-imm? '(0 . 0))
                            (lambda (t o c rt rn srm sh)
                              (let ((rm  (sregister-r srm))
                                    (u?  (sregister-pos? srm))
                                    (sht (car sh))
                                    (sho (cdr sh)))
                                (ldr-regsr-emit t c o #t u? s? #f rn rt rm sht sho))))))

              ;; pre-index: p? = #t, w? = #t
              (and pre-index-reg?
                   (or (and imm12?
                            (lambda (t o c rt rn imm)
                              (let ((u?  (positive? imm))
                                    (off (abs imm)))
                                (ldr-imm-emit t c o #t u? s? #t rn rt off))))
                       (and sregister? (? shift-w-imm? '(0 . 0))
                            (lambda (t o c rt rn srm sh)
                              (let ((rm  (sregister-r srm))
                                    (u?  (sregister-pos? srm))
                                    (sht (car sh))
                                    (sho (cdr sh)))
                                (ldr-regsr-emit t c o #t u? s? #t rn rt rm sht sho))))))

              ;; post-index: p? = #f, w? = #t
              (and post-index-reg?
                   (or (and imm12?
                            (lambda (t o c rt rn imm)
                              (let ((u?  (positive? imm))
                                    (off (abs imm)))
                                (ldr-imm-emit t c o #f u? s? #t rn rt off))))
                       (and sregister? (? shift-w-imm? '(0 . 0))
                            (lambda (t o c rt rn srm sh)
                              (let ((rm  (sregister-r srm))
                                    (u?  (sregister-pos? srm))
                                    (sht (car sh))
                                    (sho (cdr sh)))
                                (ldr-regsr-emit t c o #f u? s? #t rn rt rm sht sho))))))

              ;; relocated label
              (and symbol? (? imm12? 0)
                   (lambda (t o c rt label imm)
                     (record-relocation t 'pcrel label)
                     (let ((u?  (positive? imm))
                           (off (abs imm))
                           (pc  (register? 'pc)))
                       (ldr-imm-emit t c o #t u? #f #f pc rt off)))))))))

;;        mnemonic     opcode  templates ...

(define-opcode adc     #b01010    (data-emit '(rr rsr imm)))
(define-opcode adcs    #b01011    (data-emit '(rr rsr imm)))
(define-opcode add     #b01000    (data-emit '(rr rsr imm)))
(define-opcode adds    #b01001    (data-emit '(rr rsr imm)))
(define-opcode adr     #b00000     adr-emit) ;; special mnemonics for add/sub + pc-rel
(define-opcode adrs    #b00001     adr-emit)
(define-opcode and     #b00000    (data-emit '(rr rsr imm)))
(define-opcode ands    #b00001    (data-emit '(rr rsr imm)))
(define-opcode asr     #b11010    (data-emit '(rr imm)))
(define-opcode asrs    #b11011    (data-emit '(rr imm)))
(define-opcode b       #b1010      b-emit)
(define-opcode bfc     #b0111110   bfc-emit)
(define-opcode bfi     #b0111110   bfi-emit)
(define-opcode bic     #b11100    (data-emit '(rr rsr imm)))
(define-opcode bics    #b11101    (data-emit '(rr rsr imm)))
(define-opcode bkpt    #b00010010  bkpt-emit)
(define-opcode bl      #b1011      b-emit)

(define-opcode bx      'not-implemented) ;; no support for thumb
(define-opcode blx     'not-implemented) ;; ^
(define-opcode bxj     'not-implemented) ;; no support for jazelle (jvm accel)
(define-opcode cbnz    'not-implemented) ;; no support for thumb
(define-opcode cbz     'not-implemented) ;; ^

(define-opcode cdp     'not-implemented) ;; todo: coprocessors
(define-opcode cdp2    'not-implemented)

(define-opcode chka    'not-implemented) ;; no support for thumbee

(define-opcode clrex   #b11110101011111111111000000011111 direct-emit)

(define-opcode clz     #b00010110  clz-emit)
(define-opcode cmn     #b10111    (data-emit '(rr rsr imm)))
(define-opcode cmp     #b10101    (data-emit '(rr rsr imm)))

(define-opcode cps     'not-implemented) ;; todo: change processor state (system)
(define-opcode cpy     'not-implemented) ;; no support for deprecated synonyms

(define-opcode csdb    #b11100011001000001111000000010100 direct-emit)
(define-opcode dbg     #b001100100000111100001111         dbg-emit)

(define-opcode dmb     #b1111010101111111111100000101     barrier-emit)
(define-opcode dsb     #b1111010101111111111100000100     barrier-emit)
(define-opcode isb     #b1111010101111111111100000110     barrier-emit)

(define-opcode enterx  'not-implemented) ;; no support for thumb

(define-opcode eor     #b00010    (data-emit '(rr rsr imm)))
(define-opcode eors    #b00011    (data-emit '(rr rsr imm)))

(define-opcode eret    'not-implemented) ;; todo: exception return (system)
(define-opcode hb      'not-implemented) ;; no support for thumbee
(define-opcode hbl     'not-implemented)
(define-opcode hblp    'not-implemented)
(define-opcode hbp     'not-implemented)
(define-opcode hvc     'not-implemented) ;; todo: hypervisor call (system)
(define-opcode it      'not-implemented) ;; no support for thumb

(define-opcode ldc     'not-implemented) ;; todo: coprocessors
(define-opcode ldc2    'not-implemented)

(define-opcode ldm     #b100010    ldm-emit)
(define-opcode ldmia   #b100010    ldm-emit)
(define-opcode ldmfd   #b100010    ldm-emit)
(define-opcode ldmda   #b100000    ldm-emit)
(define-opcode ldmfa   #b100000    ldm-emit)
(define-opcode ldmdb   #b100100    ldm-emit)
(define-opcode ldmea   #b100100    ldm-emit)
(define-opcode ldmib   #b100110    ldm-emit)
(define-opcode ldmeb   #b100110    ldm-emit)

(define-opcode ldr     #b010       (ldr-emit '()))
(define-opcode ldrt    #b010       (ldr-emit '()))
(define-opcode ldrb    #b010       (ldr-emit '(sizemod)))
(define-opcode ldrbt   #b010       (ldr-emit '(sizemod)))

;; (define-opcode ldrd    #b000       ldr-emit #b1101)
;; (define-opcode ldrh    #b000       ldr-emit #b1011)
;; (define-opcode ldrsb   #b000       ldr-emit #b1101)
;; (define-opcode ldrsh   #b000       ldr-emit)

;; (define-opcode ldrht   #b000       ldr-emit #b1011)
;; (define-opcode ldrsht  #b000       ldr-emit)
;; (define-opcode ldrsbt  #b000       ldr-emit)

(define-opcode ldrex   'not-implemented)
(define-opcode ldrexb  'not-implemented)
(define-opcode ldrexh  'not-implemented)
(define-opcode ldrexd  'not-implemented)

(define-opcode mrc     'not-implemented)
(define-opcode mrc2    'not-implemented)
(define-opcode mrrc    'not-implemented)
(define-opcode mrrc2   'not-implemented)
(define-opcode mla     'not-implemented)
(define-opcode mlas    'not-implemented)
(define-opcode mls     'not-implemented)
(define-opcode mov     #b11010 (data-emit '(rr rsr imms16)))
(define-opcode movs    #b11011 (data-emit '(rr rsr)))
(define-opcode mul     'not-implemented)
(define-opcode muls    'not-implemented)
(define-opcode mvn     #b11110 (data-emit '(rr imm)))
(define-opcode mvns    #b11111 (data-emit '(rr imm)))
(define-opcode orr     #b11000 (data-emit '(rr rsr imm)))
(define-opcode orrs    #b11001 (data-emit '(rr rsr imm)))
(define-opcode qadd    'not-implemented)
(define-opcode qdadd   'not-implemented)
(define-opcode qdsub   'not-implemented)
(define-opcode qsub    'not-implemented)
(define-opcode rsb     #b00110 (data-emit '(rr rsr imm)))
(define-opcode rsbs    #b00111 (data-emit '(rr rsr imm)))
(define-opcode rsc     #b01110 (data-emit '(rr rsr imm)))
(define-opcode rscs    #b01111 (data-emit '(rr rsr imm)))
(define-opcode sbc     #b01100 (data-emit '(rr rsr imm)))
(define-opcode sbcs    #b01101 (data-emit '(rr rsr imm)))
(define-opcode smlabb  'not-implemented)
(define-opcode smlabt  'not-implemented)
(define-opcode smlal   'not-implemented)
(define-opcode smlalbb 'not-implemented)
(define-opcode smlalbt 'not-implemented)
(define-opcode smlals  'not-implemented)
(define-opcode smlaltb 'not-implemented)
(define-opcode smlaltt 'not-implemented)
(define-opcode smlatb  'not-implemented)
(define-opcode smlatt  'not-implemented)
(define-opcode smlawb  'not-implemented)
(define-opcode smlawt  'not-implemented)
(define-opcode smulbb  'not-implemented)
(define-opcode smulbt  'not-implemented)
(define-opcode smull   'not-implemented)
(define-opcode smulls  'not-implemented)
(define-opcode smultb  'not-implemented)
(define-opcode smultt  'not-implemented)
(define-opcode smulwb  'not-implemented)
(define-opcode smulwt  'not-implemented)
(define-opcode strd  'not-implemented)
(define-opcode strex  'not-implemented)
(define-opcode strexb 'not-implemented)
(define-opcode strexd 'not-implemented)
(define-opcode strexh 'not-implemented)
(define-opcode strh  'not-implemented)
(define-opcode strht  'not-implemented)
(define-opcode sub     #b00100 (data-emit '(rr rsr imm)))
(define-opcode subs    #b00101 (data-emit '(rr rsr imm)))
(define-opcode swp    'not-implemented)
(define-opcode swpb   'not-implemented)
(define-opcode teq     #b10011 (data-emit '(rr rsr imm)))
(define-opcode test    #b10001 (data-emit '(rr rsr imm)))
(define-opcode umaal   'not-implemented)
(define-opcode umlal   'not-implemented)
(define-opcode umlals  'not-implemented)
(define-opcode umull   'not-implemented)
(define-opcode umulls  'not-implemented)

;; multiply and multiply accumulate
;; 1 30 9 8   7 6 5 4   3 2 1 20   9 8 7 6 5 4 3 2 1 10 9 8   7 6 5 4   3 2 1 0
;; cond     | 0 0 0 0 | op       | --                       | 1 0 0 1 | --

;; (define (multiply-emit flags)(const
;; (minimeta
;;  (and opcode? (? condition? #b1110) register? register?
;;       (or (and imm12? (lambda (t o c rd rn imm)
;;                         (and (memv 'imm flags)
;;                              (data-regimm12-emit t c o rd rn imm))))
;;           (and register?
;;                (or (and shift-w-imm? (lambda (t o c rd rn rm sh)
;;                                        (let ((typ (car sh))
;;                                              (off (cdr sh)))
;;                                          (data-reg-emit t c o rd rn rm typ off))))
;;                    (and shift-w-reg? (lambda (t o c rd rn rm sh)
;;                                        (let ((typ (car sh))
;;                                              (rs  (cdr sh)))
;;                                          (and (memv 'rsr flags)
;;                                               (data-regsr-emit t c o rd rn rm typ rs)))))
;;                    (lambda (t o c rd rn rm)
;;                      (data-reg-emit t c o rd rn rm #b0000 0))))))))

;; saturating add/sub
;; 1 30 9 8   7 6 5 4   3 2 1 20   9 8 7 6 5 4 3 2 1 10 9 8   7 6 5 4   3 2 1 0
;; cond     | 0 0 0 1 | 0 op  0  | --                       | 0 1 0 1 | --

;; halfword multiply and multiply accumulate
;; 1 30 9 8   7 6 5 4   3 2 1 20   9 8 7 6 5 4 3 2 1 10 9 8   7 6 5  4   3 2 1 0
;; cond     | 0 0 0 1 | 0 op1 0  | --                       | 1 - op 0 | --

;; extra lod/sto
;; 1 30 9 8   7 6 5   4 3 2 1 20   9 8 7 6   5 4 3 2 1 10 9 8   7   6 5   4 3 2 1 0
;; cond     | 0 0 0 | op1        | Rn      | --               | 1 | op2 | 1 --

;; extra lod/sto (unpriv)
;; 1 30 9 8   7 6 5 4   3 2 1   20   9 8 7 6   5 4 3 2   1 10 9 8   7   6 5   4 3 2 1 0
;; cond     | 0 0 0 0 | --  1 | op | --      | --      | --       | 1 | op2 | 1 --

;; sync primitives
;; 1 30 9 8   7 6 5 4   3 2 1 20   9 8 7 6 5 4 3 2 1 10 9 8   7 6 5 4   3 2 1 0
;; cond     | 0 0 0 1 | op       | op | --                  | 1 0 0 1 | --

;;        mnemonic     opcode  templates ...
