(define registers-table
  (alist->hash-table
   '((r0 . 0)   (r1 . 1)   (r2 . 2)   (r3 . 3)   (r4 . 4)
     (r5 . 5)   (r6 . 6)   (r7 . 7)   (r8 . 8)   (r9 . 9)
     (r10 . 10) (r11 . 11) (r12 . 12) (r13 . 13) (r14 . 14)
     (r15 . 15)

     ;; argument/result or scratch registers, aliases for r0-r3
     (a1 . 0) (a2 . 1) (a3 . 2) (a4 . 3)

     ;; variable registers, aliases for r4-r11
     (v1 . 4) (v2 . 5) (v3 . 6) (v4 . 7)
     (v5 . 8) (v6 . 9) (v7 . 10) (v8 . 11)

     (sb . 9)  ; static base
     (sl . 10) ; stack limit
     (fp . 11) ; frame pointer
     (ip . 12) ; intra-procedure-call scratch
     (sp . 13) ; stack pointer
     (lr . 14) ; link registers
     (pc . 15) ; program counter

     ;; aliases
     (cpsr . cpsr)) eqv?))

(define conditions-table
  (alist->hash-table
   '((?eq . #b0000)               ; Z = 1             (zero)
     (?ne . #b0001)               ; Z = 0
     (?cs . #b0010) (hs . #b0010) ; C = 1             (carry)
     (?cc . #b0011) (lo . #b0011) ; C = 0
     (?mi . #b0100)               ; N = 1             (negative)
     (?pl . #b0101)               ; N = 0
     (?vs . #b0110)               ; V = 1             (oVerflow)
     (?vc . #b0111)               ; V = 0
     (?hi . #b1000)               ; C = 1 AND Z = 0   (unsigned higher)
     (?ls . #b1001)               ; C = 0 OR Z = 1    (unsigned lower or same)
     (?ge . #b1010)               ; N = V             (signed greater or same)
     (?lt . #b1011)               ; N != V            (signed less)
     (?gt . #b1100)               ; Z = 0 AND N = v   (signed greater)
     (?le . #b1101)               ; Z = 1 OR N != V   (signed less or same)
     (?al . #b1110)) eqv?))       ; never conditional (always)

(define shifts-table
  (alist->hash-table
   '((lsl . #b00)                ; logical shift left
     (lsr . #b01)                ; logical shift right
     (asr . #b10)                ; arithmetic shift right
     (ror . #b11)) eqv?))        ; rotate right

(define barrier-options-table
  (alist->hash-table
   '((sy    . #b1111)
     (st    . #b1110)
     (ish   . #b1011)
     (ishst . #b1010)
     (nsh   . #b0111)
     (nshst . #b0110)
     (osh   . #b0011)
     (oshst . #b0010)) eqv?))

(define opcodes-table
  (make-hash-table))

;; parser predicates

(define (register? sym)
  (hash-table-ref registers-table sym (lambda () #f)))

(define (opcode? sym)
  (let ((entry (hash-table-ref opcodes-table sym (lambda () #f))))
    (and entry (car entry))))

(define (opcode-emitter? sym)
  (let ((entry (hash-table-ref opcodes-table sym (lambda () #f))))
    (and entry (cadr entry))))

(define (condition? sym)
  (hash-table-ref conditions-table sym (lambda () #f)))

(define (shift-type? sym)
  (hash-table-ref shifts-table sym (lambda () #f)))

(define (barrier-option? sym)
  (hash-table-ref barrier-options-table sym (lambda () #f)))

(define (shift-w-imm? item)
  (cond
   ;; rrx is special, it's an alias for ror with 0
   ((eq? item 'rrx) (cons #b11 0))
   ;; other shifts are a pair of (name count)
   ((and (simple-pair? item) (number? (cadr item)) (<= 0 (cadr item) 31))
    (let* ((op  (shift-type? (car item)))
           (imm (modulo (cadr item) 32)))
      (and op (cons op imm))))
   (else #f)))

(define (shift-w-reg? item)
  (cond
   ;; rrx is special, it's an alias for ror with 0
   ((eq? item 'rrx) (cons #b11 0))
   ;; other shifts are a pair of (name count)
   ((pair? item)
    (let* ((op    (shift-type? (car item)))
           (rs    (register?   (cadr item))))
      (and op rs (cons op rs))))
   (else #f)))

(define (imm12? x)
  (and (integer? x)
       (positive? x)
       (call/cc
        (lambda (return)
          (do ((rot    0 (+ rot 2))
               (encode x (rotate-bit-field encode 2 0 32)))
              ((>= rot 32) #f)
            (when (zero? (bitwise-and encode -256))
              (let* ((output (arithmetic-shift (/ rot 2) 8))
                     (output (bitwise-ior output encode)))
                (return output))))))))

(define (-imm12? x)
  (and (integer? x)
       (negative? x)
       (call/cc
        (lambda (return)
          (do ((rot    0       (+ rot 2))
               (encode (abs x) (rotate-bit-field encode 2 0 32)))
              ((>= rot 32) #f)
            (when (zero? (bitwise-and encode -256))
              (let* ((output (arithmetic-shift (/ rot 2) 8))
                     (output (bitwise-ior output encode)))
                (return output))))))))

;; opcode data templates & emitters

(define-syntax define-opcode
  (syntax-rules ()
    ((_ name _) '()) ;; discard
    ((_ name opcode emitter)
     (hash-table-set! opcodes-table 'name (list opcode emitter)))))

(define (emit-instruction out bytelist)
  (display (list "emit: " (map (lambda (n) (number->string n 16)) bytelist)))
  (newline))

(define (record-relocation out type label)
  (display (list "recording relocation:" type label))
  (newline))

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
            (set! out (arithmetic-shift (bitwise-ior out c) 28))
            (set! out (bitwise-ior out o))
            (emit-instruction t (integer->bytelist out 4)))))))

;; data processing (register)
;; 1 30 9 8   7 6 5   4 3 2 1 20   9 8 7 6 5 4 3 2   1 10 9 8 7   6 5    4   3 2 1 0
;; cond     | 0 0 0 | op1        | Rn     | Rd     | imm5       | type | 0 | Rm

;; (ne? add r0 r1 r1 (lsl 3))
(define (data-reg-emit t c op1 rd rn rm shtyp shoff)
  (let ((out #x00000000))
    (set! out (arithmetic-shift (bitwise-ior out c)     8)) ;; add the cond flags
    (set! out (arithmetic-shift (bitwise-ior out op1)   4)) ;; add the opcode + 1 for the S bit
    (set! out (arithmetic-shift (bitwise-ior out rn)    4)) ;; add the input reg
    (set! out (arithmetic-shift (bitwise-ior out rd)    5)) ;; add the dest reg
    (set! out (arithmetic-shift (bitwise-ior out shoff) 2)) ;; add the shift offset
    (set! out (arithmetic-shift (bitwise-ior out shtyp) 5)) ;; add the shift type
    (set! out (bitwise-ior out rm))
    (emit-instruction t (integer->bytelist out 4))))

;; data processing (register-shifted register)
;; 1 30 9 8   7 6 5   4 3 2 1 20   9 8 7 6 5 4 3 2 1 10 9 8   7   6 5   4   3 2 1 0
;; cond     | 0 0 0 | op1        | --                       | 0 | op2 | 1 |  --

(define (data-regsr-emit t c op1 rd rn rm shtyp rs)
  (let ((out 0))
    (set! out (arithmetic-shift (bitwise-ior out c)     8)) ;; add the cond flags
    (set! out (arithmetic-shift (bitwise-ior out op1)   4)) ;; add the opcode + 1 for the S bit
    (set! out (arithmetic-shift (bitwise-ior out rn)    4)) ;; add the input reg
    (set! out (arithmetic-shift (bitwise-ior out rd)    4)) ;; add the dest reg
    (set! out (arithmetic-shift (bitwise-ior out rs)    3)) ;; add the shift register
    (set! out (arithmetic-shift (bitwise-ior out shtyp) 1)) ;; add the shift type
    (set! out (arithmetic-shift (bitwise-ior out #b1)   4)) ;; add the rs flag
    (set! out (bitwise-ior out rm))
    (emit-instruction t (integer->bytelist out 4))))

;; data processing (immediate 12bit)
;; 1 30 9 8   7 6 5   4 3 2 1 20   9 8 7 6   5 4 3 2   1 10 9 8   7 6 5 4 3 2 1 0
;; cond     | 0 0 1 | op1        | Rn      | --      | rotation | a b c d e f g h

(define (data-regimm12-emit t c op1 rd rn imm)
  (let ((out 0))
    (set! out (arithmetic-shift (bitwise-ior out c)     3)) ;; cond flags
    (set! out (arithmetic-shift (bitwise-ior out #b001) 5)) ;; const
    (set! out (arithmetic-shift (bitwise-ior out op1)   4)) ;; opcode
    (set! out (arithmetic-shift (bitwise-ior out rn)    4))
    (set! out (arithmetic-shift (bitwise-ior out rd)    12))
    (set! out (bitwise-ior out imm))
    (emit-instruction t (integer->bytelist out 4))))

;; data processing (immediate 16bit)
;; 1 30 9 8   7 6 5 4   3 2 1 20   9 8 7 6   5 4 3 2   1 10 9 8 7 6 5 4 3 2 1 0
;; cond     | 0 0 1 1 | op1      | imm4    | Rd      | imm12

(define (data-regimm16-emit t c op1 rd rn imm)
  (let ((out  0)
        (imm4  (arithmetic-shift imm -12)) ; hi-4 bits
        (imm12 (bitwise-and imm #xFFF)))
    (set! out (arithmetic-shift (bitwise-ior out c)      4)) ;; cond flags
    (set! out (arithmetic-shift (bitwise-ior out #b0011) 4)) ;; const
    (set! out (arithmetic-shift (bitwise-ior out op1)    4)) ;; opcode
    (set! out (arithmetic-shift (bitwise-ior out imm4)   4))
    (set! out (arithmetic-shift (bitwise-ior out rd)    12))
    (set! out (bitwise-ior out imm12))
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
        (imm (bitwise-and imm #xFFFFFF))) ;; 24bits, defeat sign extension
    (set! out (arithmetic-shift (bitwise-ior out c)     4)) ;; cond flags
    (set! out (arithmetic-shift (bitwise-ior out op1)  24)) ;; opcode
    (set! out (bitwise-ior out imm))
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
    (set! out (arithmetic-shift (bitwise-ior out c)     7))
    (set! out (arithmetic-shift (bitwise-ior out op1)   5))
    (set! out (arithmetic-shift (bitwise-ior out msb)   4))
    (set! out (arithmetic-shift (bitwise-ior out rd)    5))
    (set! out (arithmetic-shift (bitwise-ior out lsb)   3))
    (set! out (arithmetic-shift (bitwise-ior out #b001) 4))
    (set! out (bitwise-ior out rn))
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
         (c     #b1110)                     ; breakpoint is always unconditional
         (imm16 (bitwise-and imm #xFFFF))
         (imm12 (arithmetic-shift imm16 -4))  ; hi 12 bits
         (imm4  (bitwise-and imm #b1111)))    ; lo-4 bits
    (set! out (arithmetic-shift (bitwise-ior out c)      8)) ;; cond flags
    (set! out (arithmetic-shift (bitwise-ior out op1)   12)) ;; const
    (set! out (arithmetic-shift (bitwise-ior out imm12)  4))
    (set! out (arithmetic-shift (bitwise-ior out #b0111) 4)) ;; opcode
    (set! out (bitwise-ior out imm4))
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
    (set! out (arithmetic-shift (bitwise-ior out c)          8))
    (set! out (arithmetic-shift (bitwise-ior out op1)        4))
    (set! out (arithmetic-shift (bitwise-ior out #b1111)     4))
    (set! out (arithmetic-shift (bitwise-ior out rd)         8))
    (set! out (arithmetic-shift (bitwise-ior out #b11110001) 4))
    (set! out (bitwise-ior out rm))
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
            (set! out (arithmetic-shift (bitwise-ior out c) 24))
            (set! out (arithmetic-shift (bitwise-ior out o)  4))
            (set! out (bitwise-ior out imm4))
            (emit-instruction t (integer->bytelist out 4)))))))

;; barrier + options

(define barrier-emit
  (minimeta
   (and opcode? barrier-option?
        (lambda (t o opt)
          (let ((out 0))
            (set! out (arithmetic-shift (bitwise-ior out o) 4))
            (set! out (bitwise-ior out opt))
            (emit-instruction t (integer->bytelist out 4)))))))

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

(define-opcode enterx  'not-implemented)
(define-opcode eor     #b00010 (data-emit '(rr imm)))
(define-opcode eors    #b00011 (data-emit '(rr imm)))
(define-opcode eret    'not-implemented)
(define-opcode hb      'not-implemented)
(define-opcode hbl     'not-implemented)
(define-opcode hblp    'not-implemented)
(define-opcode hbp     'not-implemented)
(define-opcode hvc     'not-implemented)
(define-opcode isb     'not-implemented)
(define-opcode it      'not-implemented)
(define-opcode ldc     'not-implemented)
(define-opcode ldc2    'not-implemented)
(define-opcode ldm     'not-implemented)
(define-opcode ldmia   'not-implemented)
(define-opcode ldmfd   'not-implemented)
(define-opcode ldmda   'not-implemented)
(define-opcode ldmfa   'not-implemented)
(define-opcode ldmdb   'not-implemented)
(define-opcode ldmea   'not-implemented)
(define-opcode ldmib   'not-implemented)
(define-opcode ldmeb   'not-implemented)
(define-opcode ldmeb   'not-implemented)
(define-opcode ldr     'not-implemented)
(define-opcode ldrb    'not-implemented)
(define-opcode ldrbt   'not-implemented)
(define-opcode ldrd    'not-implemented)
(define-opcode ldrex   'not-implemented)
(define-opcode ldrexb  'not-implemented)
(define-opcode ldrexh  'not-implemented)
(define-opcode ldrexd  'not-implemented)
(define-opcode ldrh    'not-implemented)
(define-opcode ldrht   'not-implemented)
(define-opcode ldrsb   'not-implemented)
(define-opcode ldrsbt  'not-implemented)
(define-opcode ldrsh   'not-implemented)
(define-opcode ldrsht  'not-implemented)
(define-opcode ldrt    'not-implemented)
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
