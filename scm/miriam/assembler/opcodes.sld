(define-library (miriam assembler opcodes)
  (export debug-emit)

  (import (scheme base))
  (import (scheme write))

  (import (miriam prelude))
  (import (miriam langs minimeta))

  (import (miriam assembler numbers))
  (import (miriam assembler syntax))

  (import (srfi 69)) ; hash-tables
  (import (srfi 60)) ; integers-as-bits

  (include "isa-armv7.scm")

  (begin
    (define-syntax debug-emit
      (syntax-rules ()
        ((_ form)
         (let ((asmform 'form))
           (and (pair? asmform)
                (let* ((opcode  (car asmform))
                       (emitter (opcode-fn opcode)))
                  (and opcode (emitter '() asmform))))))))))
