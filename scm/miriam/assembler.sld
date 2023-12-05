(define-library (miriam assembler)

  (import
   (scheme base)
   (scheme cxr)
   (ice-9 match)

   (miriam prelude)
   (miriam logger)

   (miriam langs minimeta)

   (miriam assembler syntax)
   (miriam assembler object)
   (miriam assembler opcodes)
   (miriam assembler numbers)

   (miriam structs buffer)
   (miriam structs append-list)
   (srfi 69)) ; hash tablse

  (export assemble)

  (begin

    ;; instruction = (a form with an opcode for a car)
    ;; evaluates the given opcode, and emits it's bytes to the code stream
    (define (assemble-instruction out form)
      (if/let ((code (eval-opcode out form)))
        (emit-bytevector out code)
        (emit-error out (list "problem assembling" form))))

    ;; associates the given name at the current offset (and scope)
    (define (assemble-label out form)
      (if/let ((label (symbol-not-register? (cadr form))))
        (emit-label out label)
        (emit-error "invalid label:" (cadr form))))

    ;; assembles the body code inside a new scope, allowing arbitrary labels
    (define (assemble-block out form)
      (match-let (((name args . rest) (cdr form)))
        (emit-label out name)
        (emit-push-scope out)
        (emit-label out '$enter)
        (assemble-forms out rest)
        (emit-label out '$exit)
        (emit-pop-scope out)))

    ;; reserve some amount of space, setting data
    ;; ie. (res "this is a utf-8 string")   <- the utf-8 bytes of the string
    ;;     (res (word 16))                  <- 16 words (4 * 16 = 64 bytes)
    (define (assemble-reserve out form)
      (define reserve-matcher
        (minimeta
         (or (and u/s-byte   (lambda (t value) (integer->bytevector value 1)))
             (and u/s-hword  (lambda (t value) (integer->bytevector value 2)))
             (and u/s-word   (lambda (t value) (integer->bytevector value 4)))
             (and u/s-dword  (lambda (t value) (integer->bytevector value 8)))
             (and string?    (lambda (t value) (string->utf8 value))))))

      (if/let ((value (reserve-matcher '() (cdr form))))
        (emit-bytevector out value)
        (emit-error out (list "couldn't parse reservation" form))))

    ;; align the current offset with the named alignment size
    ;; after running, the offset will have been moved to the next multiple of the given size
    ;; ie. (align word) ;; offset moves to 4-byte boundary
    ;;     (align page) ;; offset moves to page alignment boundary
    (define (assemble-align out form)
      (define align-matcher
        (minimeta
         (or (and 'byte   (lambda (t)      1))
             (and 'hword  (lambda (t)      2))
             (and 'word   (lambda (t)      4))
             (and 'dword  (lambda (t)      8))
             (and 'page   (lambda (t) #x1000));
             (and number? (lambda (t v) v)))))

      (if/let ((align (align-matcher '() (cdr form))))
        (emit-align out align)
        (emit-error out (list "couldn't parse alignment" form))))

    ;; dispatch over forms
    (define (assemble-form out code)
      (if (opcode? (car code))
          (assemble-instruction out code)
          (case (car code)
            ((label)  (assemble-label out code))
            ((block)  (assemble-block out code))
            ((resv)   (assemble-reserve out code))
            ((align)  (assemble-align out code))
            (else
             (emit-error out (list "unexpected form" code))))))

    ;; iterate over forms
    (define (assemble-forms out forms)
      (do ((next forms (cdr next)))
          ((null? next))
        (assemble-form out (car next))))

    ;; don't require quoting the code
    (define-syntax assemble
      (syntax-rules ()
        ((_ body ...)
         (let ((out (make-asm-object)))
           (assemble-forms out '(body ...))
           (let ((output (buffer-bytevector (asm-code out))))
             (fixup-relocs out output)
             (values output out))))))

    ;; --

    (define (eval-opcode out form)
      (let ((parser (opcode-parser (car form))))
        (and parser (parser out form))))

    (define (fixup-relocs out bytes)
      (do ((next (asm-relocs out) (cdr next)))
          ((null? next))
        (let ((label  (caar next))
              (form   (cadar next))
              (offset (caddar next))
              (scopes (car (cdddar next))))
          (if (lookup-label out label scopes)
              (call-in-scope
               out offset scopes
               (lambda ()
                 (let ((patch (eval-opcode out form)))
                   (bytevector-copy! bytes offset patch 0 4))))
              (emit-error out (list "undefined label:" label))))))))
