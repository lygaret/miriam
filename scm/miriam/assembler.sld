(define-library (miriam assembler)

  (import
   (scheme base)
   (scheme cxr)
  
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
          
  ;; loop over forms
  ;;   each form, execute the parser
  ;;   each form writes to the output buffer
  ;; loop over added relocations
  ;;   each relocation should now be resolvable
  ;;   external references get included in final output, for dynamic linking
  ;;   resolved references patch the output buffer

  (begin

    (define (assemble-opcode out form)
      (if/let ((parser (opcode-parser (car form))))
        (parser out form)
        (emit-error out (list "problem assembling" form))))

    (define (assemble-instruction out form)
      (if/let ((code (assemble-opcode out form)))
        (emit-bytevector out code)
        (emit-error out (list "problem assembling" form))))

    (define reserve-matcher
      (minimeta
       (or (and u/s-byte   (lambda (t value) (integer->bytevector value 1)))
           (and u/s-hword  (lambda (t value) (integer->bytevector value 2)))
           (and u/s-word   (lambda (t value) (integer->bytevector value 4)))
           (and u/s-dword  (lambda (t value) (integer->bytevector value 8)))
           (and string?    (lambda (t value) (string->utf8 value))))))

    (define (assemble-reserve out form)
      (if/let ((value (reserve-matcher out (cdr form))))
        (begin
          (emit-bytevector out value))
        (emit-error out (list "couldn't parse reservation" form))))

    (define align-matcher
      (minimeta
       (or (and 'byte   (lambda (t) 1))
           (and 'hword  (lambda (t) 2))
           (and 'word   (lambda (t) 4))
           (and 'dword  (lambda (t) 8))
           (and number? (lambda (t v) v)))))

    (define (assemble-align out form)
      (if/let ((align (align-matcher out (cdr form))))
        (begin
          (emit-align out align))
        (emit-error out (list "couldn't parse alignment" form))))
        
    (define (assemble-label out form)
      (if/let ((label (symbol-not-register? (cadr form))))
        (begin
          (emit-label out label)
          (unless (null? (cddr form))
            (assemble-forms out (cddr form))))
        (emit-error "invalid label:" (cadr form))))

    (define (assemble-form out code)
      (if (opcode? (car code))
          (assemble-instruction out code)
          (case (car code)
            ((label)  (assemble-label out code))
            ((res)    (assemble-reserve out code))
            ((align)  (assemble-align out code))
            (else
             (emit-error out (list "unexpected form" code))))))
      
    (define (assemble-forms out forms)
      (do ((next forms (cdr next)))
          ((null? next))
        (assemble-form out (car next))))

    (define-syntax assemble
      (syntax-rules ()
        ((_ body ...)
         (let ((out (make-asm-object)))
           (assemble-forms out '(body ...))
           (let ((output (buffer-bytevector (asm-code out))))
             (fixup-relocs out output)
             (values output out))))))

    ;; --

    (define (fixup-reloc-patches out bytes addr patches)
      (let iter ((patches patches))
        (unless (null? patches)
          (let ((form          (caar patches))
                (offset        (cdar patches))
                (saved-fillptr (asm-fillptr out)))
            (set-asm-fillptr! out offset)
            (let ((patch (assemble-opcode out form)))
              (bytevector-copy! bytes offset patch 0 4)
              (set-asm-fillptr! out saved-fillptr))

            (iter (cdr patches))))))

    (define (fixup-relocs out bytes)
      (let iter ((rest (hash-table->alist (asm-relocs out))))
        (unless (null? rest)
          (let ((name    (caar rest))
                (patches (cdar rest)))
            (if/let ((offset (lookup-label out name)))
              (fixup-reloc-patches out bytes offset patches)
              (emit-error out (list "undefined label:" name)))
            (iter (cdr rest))))))))
