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

    (define (make-asm-context)
      (let ((out (make-asm-object))
            (env (environment '(scheme base)
                              '(miriam logger)
                              '(miriam assembler numbers)
                              '(miriam assembler eval))))
        `((out     . ,out)
          (env     . ,env)
          (pseudos . ()))))

    (define-syntax ctx-out     (syntax-rules () ((_ ctx) (cdr (assoc 'out ctx)))))
    (define-syntax ctx-env     (syntax-rules () ((_ ctx) (cdr (assoc 'env ctx)))))
    (define-syntax ctx-pseudos (syntax-rules () ((_ ctx) (cdr (assoc 'pseudos ctx)))))

    ;; define a pseudo instruction
    (define (define-pseudo ctx form)
      (let* ((name  (caadr form))
             (args  (cdadr form))
             (body  `(lambda ,args ,@(cddr form)))
             (macro (eval body (ctx-env ctx))))
        (set-cdr!
         (assoc 'pseudos ctx)
         (cons (cons name macro) (ctx-pseudos ctx)))))

    ;; is the form a pseudo?
    (define (pseudo? ctx sym)
      (let ((entry (assoc sym (ctx-pseudos ctx))))
        (and entry (cdr entry))))

    ;; define a value in the context environment
    (define (define-value ctx form)
      (eval form (ctx-env ctx)))

    ;; execute some arbitrary code in the context environment
    (define (context-eval ctx form)
      (eval `(begin ,@form) (ctx-env ctx)))

    ;; ---
    ;; macro expansion + pseudos

    ;; actually execute the macro
    (define (eval-unquote ctx form)
      (let ((out (ctx-out ctx))
            (env (ctx-env ctx)))
        (eval `(let (($ ,(asm-fillptr out))) ,@form) env)))

    ;; iterate through each item in the list, and possibly expand it if it's a quote
    (define (expand-form ctx form)
      (let iter ((next form) (accum '()))
        (cond ((null? next)
               (unwrap-singleton (reverse accum)))
              ((unquote? (car next))
               (iter (cdr next) (cons (eval-unquote ctx (cdar next)) accum)))
              ((unquote-splice? (car next))
               (iter (cdr next) (append (eval-unquote ctx (cdar next)) accum)))
              (else (iter (cdr next) (cons (car next) accum))))))

    ;; ---

    ;; evaluates the given opcode, and emits it's bytes to the code stream
    (define (assemble-instruction ctx form)
      (let ((expanded (expand-form ctx form)))
        (if/let ((code (eval-opcode ctx expanded)))
                (emit-bytevector (ctx-out ctx) code)
                (emit-error (ctx-out ctx) "problem assembling" form))))

    ;; actually execute the opcode
    ;; assumes the opcode exists (has been checked already)
    (define (eval-opcode ctx form)
      (let ((parser (opcode-parser (car form))))
        (and parser (parser (ctx-out ctx) form))))

    ;; expand into opcodes and assemble those in place
    (define (assemble-pseudo ctx form)
      (let* ((name   (car form))
             (macro  (pseudo? ctx name))
             (args   (expand-form ctx (cdr form)))
             (code   (apply macro args)))
        (if (list? (car code))
            (assemble-forms ctx code)
            (assemble-form  ctx code))))

    ;; associates the given name at the current offset (and scope)
    (define (assemble-label ctx form)
      (if/let ((label (symbol-not-register? (cadr form))))
        (emit-label (ctx-out ctx) label)
        (emit-error (ctx-out ctx) "invalid label:" (cadr form))))

    ;; assembles the body code inside a new scope, allowing arbitrary labels
    (define (assemble-block ctx form)
      (match-let (((name args . rest) (cdr form)))
        (emit-label (ctx-out ctx) name)
        (emit-push-scope (ctx-out ctx))
        (emit-label (ctx-out ctx) '$enter)
        (assemble-forms ctx rest)
        (emit-label (ctx-out ctx) '$exit)
        (emit-pop-scope (ctx-out ctx))))

    ;; reserve some amount of space, setting data
    ;; ie. (res "this is a utf-8 string")   <- the utf-8 bytes of the string
    ;;     (res (word 16))                  <- 16 words (4 * 16 = 64 bytes)
    (define (assemble-reserve ctx form)
      (define reserve-matcher
        (minimeta
         (or (and u/s-byte        (lambda (t value) (integer->bytevector value 1)))
             (and u/s-hword       (lambda (t value) (integer->bytevector value 2)))
             (and u/s-word        (lambda (t value) (integer->bytevector value 4)))
             (and u/s-dword       (lambda (t value) (integer->bytevector value 8)))
             (and string?         (lambda (t value) (string->utf8 value)))
             (and unquote?        (lambda (t value . form) (expand-form ctx form))))))

      (if/let ((value (reserve-matcher '() (cdr form))))
        (emit-bytevector (ctx-out ctx) value)
        (emit-error (ctx-out ctx) "couldn't parse reservation" form)))

    ;; align the current offset with the named alignment size
    ;; after running, the offset will have been moved to the next multiple of the given size
    ;; ie. (align word) ;; offset moves to 4-byte boundary
    ;;     (align page) ;; offset moves to page alignment boundary
    (define (assemble-align ctx form)
      (define align-matcher
        (minimeta
         (or (and 'byte    (lambda (t)      1))
             (and 'hword   (lambda (t)      2))
             (and 'word    (lambda (t)      4))
             (and 'dword   (lambda (t)      8))
             (and 'page    (lambda (t) #x1000))
             (and number?  (lambda (t v)    v))
             (and unquote? (lambda (t v . form) (expand-form ctx form))))))

      (if/let ((align (align-matcher '() (cdr form))))
        (emit-align (ctx-out ctx) align)
        (emit-error (ctx-out ctx) (list "couldn't parse alignment" form))))

    ;; dispatch over forms
    (define (assemble-form ctx code)
      (cond
       ((opcode? (car code))     (assemble-instruction ctx code))
       ((pseudo? ctx (car code)) (assemble-pseudo ctx code))
       (#t
        (case (car code)
          ((define) (define-value  ctx code))
          ((pseudo) (define-pseudo ctx code))
          ((!)      (context-eval  ctx (cdr code)))

          ((label)  (assemble-label ctx code))
          ((block)  (assemble-block ctx code))
          ((resv)   (assemble-reserve ctx code))
          ((align)  (assemble-align ctx code))
          (else
           (emit-error (ctx-out ctx) "unknown form" code))))))

    ;; iterate over forms
    (define (assemble-forms ctx forms)
      (do ((next forms (cdr next)))
          ((null? next))
        (assemble-form ctx (car next))))

    (define (assemble code)
      (let ((ctx (make-asm-context)))
        (assemble-forms ctx code)
        (let ((output (buffer-bytevector (asm-code (ctx-out ctx)))))
          (fixup-relocs ctx output)
          (cons output (ctx-out ctx)))))

    ;; iterate through each reloc and reassemble the form, so that the labels can resolve
    (define (fixup-relocs ctx bytes)
      (let ((out (ctx-out ctx)))
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
                   (let ((patch (eval-opcode ctx form)))
                     (bytevector-copy! bytes offset patch 0 4))))
                (emit-error out (list "undefined label:" label)))))))))
