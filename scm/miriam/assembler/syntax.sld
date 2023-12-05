(define-library (miriam assembler syntax)
  (export b& b+ b<< b>> flag?
          register?
          sregister? sregister-r sregister-pos?
          wregister? wregister-r wregister-w?
          symbol-not-register?
          pre-index-reg?
          post-index-reg?
          reglist?
          sysregister?
          sysregister-mask?
          copro?
          copro-register?
          condition?
          shift-type?
          shift-w-imm?
          shift-w-reg?
          target?
          target-mode
          target-rn
          target-p?
          target-w?
          shifter?
          shifter-mode   
          shifter-rn     
          shifter-shtyp  
          shifter-shoff  
          shifter-rs     
          shifter-imm    
          barrier-option?
          imm3? imm4? imm5? imm16? imm24? simm24?
          imm12? -imm12? u/s-imm12?)

  (import (scheme base))
  (import (scheme cxr))
  (import (scheme write))

  (import (miriam logger))
  (import (miriam prelude))
  (import (miriam assembler numbers))

  (import (srfi 69)) ; hash-tables
  (import (srfi 60)) ; integers-as-bits

  (begin
    (define-syntax b&  (syntax-rules () ((_ rest ...) (bitwise-and rest ...))))
    (define-syntax b+  (syntax-rules () ((_ rest ...) (bitwise-ior rest ...))))
    (define-syntax b<< (syntax-rules () ((_ rest ...) (arithmetic-shift rest ...))))
    (define-syntax b>> (syntax-rules () ((_ n amt)    (arithmetic-shift n (- amt)))))

    (define (flag? n) (if n 1 0))
    
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

         (sb . 9)   ; static base
         (sl . 10)  ; stack limit
         (fp . 11)  ; frame pointer
         (ip . 12)  ; intra-procedure-call scratch
         (sp . 13)  ; stack pointer
         (lr . 14)  ; link registers
         (pc . 15)) ; program counter
       eqv?))

    (define sysregisters-table
      '((cpsr . 0) (apsr . 0) (spsr . 1)))

    (define (sysregister? sym)
      (when/let ((ent (assv sym sysregisters-table)))
        (cdr ent)))

    (define sysregisters-masks-table
      '((cpsr_c      . #b0001)
        (cpsr_x      . #b0010)
        (cpsr_xc     . #b0011)
        (cpsr_s      . #b0100)
        (cpsr_sc     . #b0101)
        (cpsr_sxc    . #b0111)
        (cpsr_f      . #b1000)
        (cpsr_fc     . #b1001)
        (cpsr_fx     . #b1010)
        (cpsr_fxc    . #b1011)
        (cpsr_fs     . #b1100)
        (cpsr_fsc    . #b1101)
        (cpsr_fsx    . #b1110)
        (cpsr_fsxc   . #b1111)))

    (define (sysregister-mask? sym)
      (when/let ((ent (assv sym sysregisters-masks-table)))
        (cdr ent)))

    ;; lookup the register number by symbol
    (define (register? sym)
      (hash-table-ref registers-table sym (lambda () #f)))

    (define (symbol-not-register? sym)
      (and (symbol? sym) (not (register? sym)) sym)) 

    ;; lookup the register number by symbol
    (define (-register? sym)
      (and (simple-pair? sym '-) (hash-table-ref registers-table (cadr sym) (lambda () #f))))

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

    ;; ----

    (define (target? form)
      (let/cc (return)
        (when/let ((rn (register? form)))
          (return (cons 'offset rn)))

        (when (simple-pair? form)
          (when (eqv? (car form) '++)
            (let ((rn (register? (cadr form))))
              (return (and rn (cons 'pre rn)))))
          (when (eqv? (cadr form) '++)
            (let ((rn (register? (car form))))
              (return (and rn (cons 'post rn))))))

        (return #f)))

    (define (target-mode t) (car t))
    (define (target-rn t)   (cdr t))
    (define (target-p? t)   (memv (target-mode t) '(offset pre)))
    (define (target-w? t)   (memv (target-mode t) '(pre)))

    ;; -----

    (define shifts-table
      (alist->hash-table
       '((lsl . #b00)                ; logical shift left
         (lsr . #b01)                ; logical shift right
         (asr . #b10)                ; arithmetic shift right
         (ror . #b11)) eqv?))        ; rotate right

    (define (shift-type? sym)
      (hash-table-ref shifts-table sym (lambda () #f)))

    ;; imm12          imm
    ;; rn             reg + imm lsl 0
    ;; (rn rrx)       reg + imm ror 0
    ;; (rn lsl imm )  reg + imm shift imm
    ;; (rn lsl rs)    reg + reg shift rn

    (define (shifter? form)
      (let/cc (return)
        (when/let ((imm   (imm12? form)))     (return (list 'imm imm)))
        (when/let ((imm   (-imm12? form)))    (return (list '-imm imm)))
        (when/let ((rn    (register? form)))  (return (list 'reg+imm rn (shift-type? 'lsl) 0)))

        (unless (pair? form) (return #f))

        (when/let ((rn (register? (car form))))
          (when (null? (cdr form))
            (return #f))

          (when (and (eqv?  (cadr form) 'rrx)
                     (null? (cddr form)))
            (return (list 'reg+imm rn (shift-type? 'ror) 0)))

          (when/let ((shtyp (shift-type? (cadr form))))
            (when (null? (cddr form))
              (return #f))
            
            (when/let ((imm (imm12?     (caddr form)))) (return (list 'reg+imm rn shtyp imm)))
            (when/let ((imm (-imm12?    (caddr form)))) (return (list 'reg-imm rn shtyp imm)))
            (when/let ((rs  (register?  (caddr form)))) (return (list 'reg+reg rn shtyp rs)))
            (when/let ((rs  (-register? (caddr form)))) (return (list 'reg-reg rn shtyp rs))))

          ;; fall through
          (return #f))))

    (define (shifter-mode   op) (car op))
    (define (shifter-rn     op) (and (pair? op) (memv (car op) '(reg+imm reg-imm reg+reg reg-reg)) (cadr op)))
    (define (shifter-shtyp  op) (and (pair? op) (memv (car op) '(reg+imm reg-imm reg+reg reg-reg)) (caddr op)))
    (define (shifter-shoff  op) (and (pair? op) (memv (car op) '(reg+imm reg-imm)) (cadddr op)))
    (define (shifter-rs     op) (and (pair? op) (memv (car op) '(reg+reg reg-reg)) (cadddr op)))
    (define (shifter-imm    op) (and (pair? op) (memv (car op) '(imm -imm)) (cadr op)))

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

    ;; ----

    (define coprocessors-table
      (alist->hash-table
       '((p0  .  0) (p1  .  1) (p2  .  2) (p3  .  3)
         (p4  .  4) (p5  .  5) (p6  .  6) (p7  .  7)
         (p8  .  8) (p9  .  9) (p10 . 10) (p11 . 11)
         (p12 . 12) (p13 . 13) (p14 . 14) (p15 . 15))
       eqv?))

    (define coprocessors-registers-table
      (alist->hash-table
       '((cr0  .  0) (cr1  .  1) (cr2  .  2) (cr3  .  3)
         (cr4  .  4) (cr5  .  5) (cr6  .  6) (cr7  .  7)
         (cr8  .  8) (cr9  .  9) (cr10 . 10) (cr11 . 11)
         (cr12 . 12) (cr13 . 13) (cr14 . 14) (cr15 . 15))
       eqv?))

    (define (copro? sym)
      (hash-table-ref coprocessors-table sym (lambda () #f)))

    (define (copro-register? sym)
      (hash-table-ref coprocessors-registers-table sym (lambda () #f)))

    ;; -----
    ;; immediate helpers

    (define (imm3? x)
      (integer-within? 0 x 7))

    (define (imm4? x)
      (integer-within? 0 x 15))

    (define (imm5? x)
      (integer-within? 0 x 31))

    (define (imm16? x)
      (log "imm16? " x (u-hword x))
      (u-hword x))

    (define (imm24? x)
      (integer-within? 0 x #x00FFFFFF))

    (define (simm24? x)
      (integer-within? (- 33554432) x 33554428))

    ;; imm12 is an 8-bit immediate, rotated up to 15 positions
    ;; not all values can be expressed this way

    ;; this algorithem just brute force checks for a valid encoding

    (define (u/s-imm12? x)
      (or (imm12? x)
          (and (-imm12? x) (- x))))

    (define (imm12? x)
      (cond
       ((and (integer? x) (zero? x)) x)
       ((positive-integer? x)
        (let/cc (return)
          (do ((rot    0 (+ rot 2))
               (encode x (rotate-bit-field encode 2 0 32)))
              ((>= rot 32) #f)
            (when (zero? (b& encode -256))
              (let* ((output (b<< (/ rot 2) 8))
                     (output (b+  output encode)))
                (return output))))))
       (else #f)))

    (define (-imm12? x)
      (and
       (negative-integer? x)
       (let/cc (return)
         (do ((rot    0       (+ rot 2))
              (encode (abs x) (rotate-bit-field encode 2 0 32)))
             ((>= rot 32) #f)
           (when (zero? (b& encode -256))
             (let* ((output (b<< (/ rot 2) 8))
                    (output (b+ output encode)))
               (return output)))))))))

