(define-library (miriam assembler opcodes)
  (export debug-emit)

  (import (scheme base))
  (import (scheme cxr))
  (import (scheme write))

  (import (miriam prelude))
  (import (miriam langs minimeta))

  (import (miriam assembler numbers))
  (import (miriam assembler syntax))

  (import (srfi 69)) ; hash-tables
  (import (srfi 60)) ; integers-as-bits

  (begin
    (define (emit-error out err)
      (display (list "error: " err))
      (newline))

    (define (emit-instruction out bytelist)
      (display (list "emit: " (map (lambda (n) (number->string n 16)) bytelist)))
      (newline))

    (define (record-relocation out type label)
      (display (list "recording relocation:" type label))
      (newline)))

  (include "encodings.scm")
  (include "opcodes.scm")

  (begin
    (define-syntax debug-emit
      (syntax-rules ()
        ((_ form)
         (let ((asmform 'form))
           (and (pair? asmform)
                (let* ((opcode (car asmform))
                       (parser (opcode-parser opcode)))
                  (and parser (parser '() asmform))))))))))
