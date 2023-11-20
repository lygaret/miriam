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

(define opcodes-table
  (make-hash-table))

;; public api

(define (register? sym)
  (hash-table-ref registers-table sym (lambda () #f)))

(define (opcode? sym)
  (hash-table-ref opcodes-table sym (lambda () #f)))

(define (condition? sym)
  (hash-table-ref conditions-table sym (lambda () #f)))

(define (shift-type? sym)
  (hash-table-ref shifts-table sym (lambda () #f)))

(define (shift-w-imm? item)
  (cond
   ;; rrx is special, it's an alias for ror with 0
   ((eq? item 'rrx) (values #b11 0))
   ;; other shifts are a pair of (name count)
   ((and (simple-pair? item) (number? (cadr item)) (<= 0 (cadr item) 31))
    (let* ((op  (shift-type? (car item)))
           (imm (modulo (cadr item) 32)))
      (and op (values op imm))))
   (else #f)))

(define (shift-w-reg? item)
  (cond
   ;; rrx is special, it's an alias for ror with 0
   ((eq? item 'rrx) (values #b11 0))
   ;; other shifts are a pair of (name count)
   ((pair? item)
    (let* ((op    (shift-type? (car item)))
           (rs    (register?   (cadr item))))
      (and op rs (values op rs))))
   (else #f)))

;; opcode data templates & emitters

(define-syntax define-opcode
  (syntax-rules ()
    ((_ name func rest ...)
     (hash-table-set! opcodes-table 'name func))))

;; CONSTRUCT EVERYTHING LITTLE ENDIAN?

;; data processing (register)
;; 1 30 9 8   7 6 5   4 3 2 1 20   9 8 7 6 5 4 3 2   1 10 9 8 7   6 5    4   3 2 1 0
;; cond     | 0 0 0 | op1        | Rn     | Rd     | imm5       | type | 0 | Rm  

;; (ne? add r0 r1 r1 (lsl 3))
(define (dp-reg-emit out c op1 rd rn rm shtyp shoff)
  (let ((out #x00000000))
    (set! out (arithmetic-shift (bitwise-ior out c)     8)) ;; add the cond flags
    (set! out (arithmetic-shift (bitwise-ior out op1)   4)) ;; add the opcode + 1 for the S bit
    (set! out (arithmetic-shift (bitwise-ior out rn)    4)) ;; add the input reg
    (set! out (arithmetic-shift (bitwise-ior out rd)    5)) ;; add the dest reg
    (set! out (arithmetic-shift (bitwise-ior out shoff) 2)) ;; add the shift offset
    (set! out (arithmetic-shift (bitwise-ior out shtyp) 5)) ;; add the shift type
    (set! out (bitwise-ior out rm))
    (integer->bytelist out 4)))

;; data processing (register-shifted register)
;; 1 30 9 8   7 6 5   4 3 2 1 20   9 8 7 6 5 4 3 2 1 10 9 8   7   6 5   4   3 2 1 0
;; cond     | 0 0 0 | op1        | --                       | 0 | op2 | 1 |  --  

(define (dp-regsr-emit out c op1 rd rn rm shtyp rs)
  (let ((out #x00000000))
    (set! out (arithmetic-shift (bitwise-ior out c)     8)) ;; add the cond flags
    (set! out (arithmetic-shift (bitwise-ior out op1)   4)) ;; add the opcode + 1 for the S bit
    (set! out (arithmetic-shift (bitwise-ior out rn)    4)) ;; add the input reg
    (set! out (arithmetic-shift (bitwise-ior out rd)    4)) ;; add the dest reg
    (set! out (arithmetic-shift (bitwise-ior out rs)    3)) ;; add the shift register
    (set! out (arithmetic-shift (bitwise-ior out shtyp) 1)) ;; add the shift type
    (set! out (arithmetic-shift (bitwise-ior out #b1)   4)) ;; add the rs flag
    (set! out (bitwise-ior out rm))
    (integer->bytelist out 4)))
  
(define dp-emit
  (minimeta-cond
   (and opcode?
        (or (and condition? register? register? register?
                 (or (and shift-w-imm? (lambda (t o c rd rn rm sh)
                                         (let-values (((typ off) sh))
                                           (dp-reg-emit t c o rd rn rm typ off))))
                     (and shift-w-reg? (lambda (t o c rd rn rm sh)
                                         (let-values (((typ rs) sh))
                                           (dp-regsr-emit t c o rd rn rm typ rs))))
                     (lambda (t o c rd rn rm)
                       (dp-reg-emit t c o rd rn rm #b0000 0))))

            (and register? register? register?
                 (or (and shift-w-imm? (lambda (t o rd rn rm sh)
                                         (let-values (((typ off) sh))
                                           (dp-reg-emit t #b1110 o rd rn rm typ off))))
                     (and shift-w-reg? (lambda (t o rd rn rm sh)
                                         (let-values (((typ rs) sh))
                                           (dp-regsr-emit t #b1110 o rd rn rm typ rs))))
                     (lambda (t o rd rn rm)
                       (dp-reg-emit t #b1110 o rd rn rm #b0000 0)) ))))))

;; data processing (immediate)
;; 1 30 9 8   7 6 5   4 3 2 1 20   9 8 7 6   5 4 3 2   1 10 9 8   7 6 5 4 3 2 1 0
;; cond     | 0 0 1 | op1        | Rn      | --      | rotation | a b c d e f g h    

;; multiply and multiply accumulate
;; 1 30 9 8   7 6 5 4   3 2 1 20   9 8 7 6 5 4 3 2 1 10 9 8   7 6 5 4   3 2 1 0
;; cond     | 0 0 0 0 | op       | --                       | 1 0 0 1 | --

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

;; truthy is the encoded immediate value
;; falsey because immediates are weird in arm
(define (modified-immedate? x) #f)

;;        mnemonic     opcode  templates ...
(define-opcode and     #b00000 rr rsr imm)
(define-opcode ands    #b00001 rr rsr imm)
(define-opcode eor     #b00010 rr imm)
(define-opcode eors    #b00011 rr imm)
(define-opcode sub     #b00100 rr rsr imm pcrel)
(define-opcode subs    #b00101 rr rsr imm pcrel)
(define-opcode rsb     #b00110 rr rsr imm)
(define-opcode rsbs    #b00111 rr rsr imm)
(define-opcode add     #b01000 rr rsr imm pcrel)
(define-opcode adds    #b01001 rr rsr imm pcrel)
(define-opcode adc     #b01010 rr rsr imm)
(define-opcode adcs    #b01011 rr rsr imm)
(define-opcode sbc     #b01100 rr rsr imm)
(define-opcode sbcs    #b01101 rr rsr imm)
(define-opcode rsc     #b01110 rr rsr imm)
(define-opcode rscs    #b01111 rr rsr imm)
(define-opcode test    #b10001 rr rsr imm)
(define-opcode teq     #b10011 rr rsr imm)
(define-opcode cmp     #b10101 rr rsr imm)
(define-opcode cmn     #b10111 rr rsr imm)
(define-opcode orr     #b11000 rr rsr imm)
(define-opcode orrs    #b11001 rr rsr imm)
(define-opcode mov     #b11010 rr rsr)
(define-opcode movs    #b11011 rr rsr)
(define-opcode bic     #b11100 rr rsr imm)
(define-opcode bics    #b11101 rr rsr imm)
(define-opcode mvn     #b11110 rr imm)
(define-opcode mvns    #b11111 rr imm)

(define-opcode mul     #b0000 mulacc)
(define-opcode muls    #b0001 mulacc)
(define-opcode mla     #b0010 mulacc)
(define-opcode mlas    #b0011 mulacc)
(define-opcode umaal   #b0000 mulacc)
(define-opcode mls     #b0110 mulacc)
(define-opcode umull   #b1000 mulacc)
(define-opcode umulls  #b1001 mulacc)
(define-opcode umlal   #b1010 mulacc)
(define-opcode umlals  #b1011 mulacc)
(define-opcode smull   #b1100 mulacc)
(define-opcode smulls  #b1101 mulacc)
(define-opcode smlal   #b1110 mulacc)
(define-opcode smlals  #b1111 mulacc)

(define-opcode qadd    #b00   sataddsub)
(define-opcode qsub    #b01   sataddsub)
(define-opcode qdadd   #b10   sataddsub)
(define-opcode qdsub   #b11   sataddsub)

(define-opcode smlabb  #b00 - hmulacc)
(define-opcode smlabt  #b00 - hmulacc)
(define-opcode smlatb  #b00 - hmulacc)
(define-opcode smlatt  #b00 - hmulacc)
(define-opcode smlawb  #b01 0 hmulacc)
(define-opcode smlawt  #b01 0 hmulacc)
(define-opcode smulwb  #b01 1 hmulacc)
(define-opcode smulwt  #b01 1 hmulacc)
(define-opcode smlalbb #b10 - hmulacc)
(define-opcode smlalbt #b10 - hmulacc)
(define-opcode smlaltb #b10 - hmulacc)
(define-opcode smlaltt #b10 - hmulacc)
(define-opcode smulbb  #b11 - hmulacc)
(define-opcode smulbt  #b11 - hmulacc)
(define-opcode smultb  #b11 - hmulacc)
(define-opcode smultt  #b11 - hmulacc)

(define-opcode ldrsb #b00 extralodsto reg imm lit)
(define-opcode strh  #b00 extralodsto reg imm)
(define-opcode ldrh  #b00 extralodsto reg imm lit)
(define-opcode ldrsh #b00 extralodsto reg imm lit)
(define-opcode strd  #b00 extralodsto reg imm)
(define-opcode ldrd  #b00 extralodsto reg imm lit)

(define-opcode strht  #b00 unprivextralodsto #b01 #b0)
(define-opcode ldrht  #b00 unprivextralodsto #b01 #b1)
(define-opcode ldrsbt #b00 unprivextralodsto #b10 #b1)
(define-opcode ldrsht #b00 unprivextralodsto #b11 #b1)

(define-opcode swp    #b0000 syncprim)
(define-opcode swpb   #b0000 syncprim)
(define-opcode strex  #b1000 syncprim)
(define-opcode ldrex  #b1001 syncprim)
(define-opcode strexd #b1010 syncprim)
(define-opcode ldrexd #b1011 syncprim)
(define-opcode strexb #b1100 syncprim)
(define-opcode ldrexb #b1101 syncprim)
(define-opcode strexh #b1110 syncprim)
(define-opcode ldrexh #b1111 syncprim)

