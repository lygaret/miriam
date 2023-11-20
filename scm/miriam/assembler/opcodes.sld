(define-library (miriam assembler opcodes)
  (export debug-emit)

  (import (scheme base))
  (import (scheme show))

  (import (miriam prelude))
  (import (miriam langs minimeta))

  (import (miriam assembler numbers))

  (import (srfi 69)) ; hash-tables
  (import (srfi 151)) ; integers-as-bits

  (include "opcodes-armv7.scm")

  (begin
    (define-syntax debug-emit 
      (syntax-rules ()
        ((_ forms)
         (let ((val (dp-emit '() 'forms)))
           (show #t (with ((radix 16)) val) "\n")))))))
