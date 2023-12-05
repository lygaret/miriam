(define-library (miriam assembler opcodes)
  (export
    opcode?
    opcode-parser
    opcode-debug-emit)

  (import (scheme base))
  (import (scheme cxr))
  (import (scheme write))

  (import (miriam logger))
  (import (miriam prelude))
  (import (miriam langs minimeta))

  (import (miriam assembler numbers))
  (import (miriam assembler syntax))
  (import (miriam assembler object))

  (import (miriam assembler output))

  (import (srfi 69)) ; hash-tables
  (import (srfi 60)) ; integers-as-bits

  (include "opcodes/encodings.scm")
  (include "opcodes/opcodes.scm")

  (begin
    (define-syntax debug-emit
      (syntax-rules ()
        ((_ form ...)
         (let iter ((next '(form ...)))
           (cond
            ((null? next)
             #t)
            ((not (opcode? (caar next)))
             (iter (cdr next)))
            ((opcode-parser (caar next)) =>
             (lambda (parser)
               (let ((bv (parser cdr next)))
                 (display (bytevector->hex-string bv))
                 (newline)
                 (iter (cdr next)))))
            (else
             (iter (cdr next))))))))))
