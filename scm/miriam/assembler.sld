(define-library (miriam assembler)

  (import
   (scheme base)
   (scheme eval)
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

    (define (unquoted? form)
      (and (pair? form) (eqv? 'unquote (car form))))

    (define eval-environment
      (environment '(scheme base) '(miriam assembler numbers) '(miriam assembler eval)))

    (define (eval-quote out form)
      (let ((code `(let (($ ,(asm-fillptr out))) ,@form)))
        (log "eval-quote" code)
        (let ((res (eval code eval-environment)))
          (log "eval-quote-out:" res)
          res)))

    (define (expand-form out form)
      (if (null? form)
          '()
          (let iter ((next form) (accum '()))
            (log "expand-form iter:" next)
            (cond
             ((null? next)
              (if (null? (cdr accum))
                  (car accum)
                  (reverse accum)))
             ((unquoted? (car next)) (iter (cdr next) (cons (eval-quote out (cdar next)) accum)))
             (else                   (iter (cdr next) (cons (car next) accum)))))))

    (define *pseudos* '())

    (define (assemble-define out form)
      (eval form eval-environment))

    (define (assemble-define-pseudo out form)
      (let* ((name (caadr form))
             (args (cdadr form))
             (body  `(lambda ,args ,@(cddr form)))
             (macro (eval body eval-environment)))
        (set! *pseudos* (cons (cons name macro) *pseudos*))))

    (define (assemble-pseudo out form)
      (if/let ((entry (assoc (car form) *pseudos*)))
              (let* ((args  (expand-form out (cdr form)))
                     (insts (apply (cdr entry) args)))
                (if (list? (car insts))
                    (assemble-forms out insts)
                    (assemble-form out insts)))
              #f))

    ;; instruction = (a form with an opcode for a car)
    ;; evaluates the given opcode, and emits it's bytes to the code stream
    (define (assemble-instruction out form)
      (let ((expanded (expand-form out form)))
        (log "assemble instruction expanded" expanded)
        (if/let ((code (eval-opcode out expanded)))
                (emit-bytevector out code)
                (emit-error out (list "problem assembling" form)))))

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
             (and string?    (lambda (t value) (string->utf8 value)))
             (and unquoted?  (lambda (t value . form) (expand-form out form))))))

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
            ((define) (assemble-define out code))
            ((pseudo) (assemble-define-pseudo out code))
            ((label)  (assemble-label out code))
            ((block)  (assemble-block out code))
            ((resv)   (assemble-reserve out code))
            ((align)  (assemble-align out code))
            (else
             (when (not (assemble-pseudo out code))
               (emit-error out (list "unexpected form" code)))))))

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
