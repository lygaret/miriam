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

(define (condition? sym)
  (hash-table-ref conditions-table sym (lambda () #f)))

;; ----

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

;; lookup the register number by symbol
(define (register? sym)
  (hash-table-ref registers-table sym (lambda () #f)))

;; (+ rx), (- rx), rx
(define (sregister? form)
  (cond
   ((register? form) => (lambda (r) (cons r '+)))
   ((simple-pair? form (lambda (r) (memv r '(+ -))))
    (let ((reg (register? (cadr form))))
      (and reg (cons reg (car form)))))
   (else #f)))

(define (sregister-r sr)    (car sr))
(define (sregister-pos? sr) (eqv? '+ (cdr sr)))

;; (! rx), rx
(define (wregister? form)
  (cond
   ((register? form) => (lambda (r) (cons r #f)))
   ((simple-pair? form '!)
    (let ((reg (register? (cadr form))))
      (and reg (cons reg #t))))
   (else #f)))

(define (wregister-r wr)  (car wr))
(define (wregister-w? wr) (cdr wr))

;; (++ rx) => register num
(define (pre-index-reg? form)
  (and (simple-pair? form)
       (eqv? '++ (car form))
       (register? (cadr form))))

;; (rx ++) => register num
(define (post-index-reg? form)
  (and (simple-pair? form)
       (eqv? '++ (cadr form))
       (register? (car form))))

;; encodes a list of registers a a 16bit bit field
(define (reglist? form)
  (and (list? form)
       (let/cc (return)
         (let iter ((accum 0) (next form))
           (cond ((null? next)
                  (return accum))
                 ((register? (car next)) =>
                  (lambda (reg)
                    (iter (copy-bit reg accum #t) (cdr next))))
                 (else
                  (return #f)))))))

;; -----

(define shifts-table
  (alist->hash-table
   '((lsl . #b00)                ; logical shift left
     (lsr . #b01)                ; logical shift right
     (asr . #b10)                ; arithmetic shift right
     (ror . #b11)) eqv?))        ; rotate right

(define (shift-type? sym)
  (hash-table-ref shifts-table sym (lambda () #f)))

;; rrx is special, it's an alias for ror with 0
;; other shifts are a pair of (name imm/reg)

(define (shift-w-imm? item)
  (cond ((eq? item 'rrx)
         (cons #b11 0))
        ((and (simple-pair? item) (integer-within? 0 (cadr item) 31))
         (let* ((op  (shift-type? (car item)))
                (imm (modulo (cadr item) 32)))
           (and op (cons op imm))))
        (else #f)))

(define (shift-w-reg? item)
  (cond ((pair? item)
         (let* ((op    (shift-type? (car item)))
                (rs    (register?   (cadr item))))
           (and op rs (cons op rs))))
        (else #f)))

;; -----

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

(define (barrier-option? sym)
  (hash-table-ref barrier-options-table sym (lambda () #f)))

;; -----
;; immediate helpers

;; imm12 is an 8-bit immediate, rotated up to 15 positions
;; not all values can be expressed this way

;; this algorithem just brute force checks for a valid encoding

(define (imm12? x)
  (and (positive-integer? x)
       (let/cc (return)
         (do ((rot    0 (+ rot 2))
              (encode x (rotate-bit-field encode 2 0 32)))
             ((>= rot 32) #f)
           (when (zero? (bitwise-and encode -256))
             (let* ((output (arithmetic-shift (/ rot 2) 8))
                    (output (bitwise-ior output encode)))
               (return output)))))))

(define (-imm12? x)
  (and (negative-integer? x)
       (let/cc (return)
         (do ((rot    0       (+ rot 2))
              (encode (abs x) (rotate-bit-field encode 2 0 32)))
             ((>= rot 32) #f)
           (when (zero? (bitwise-and encode -256))
             (let* ((output (arithmetic-shift (/ rot 2) 8))
                    (output (bitwise-ior output encode)))
               (return output)))))))
