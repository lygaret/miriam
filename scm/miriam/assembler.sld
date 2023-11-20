(define-library (miriam assembler)
  (export assemble)

  (import (scheme base))

  (import (miriam structs dequeue))

  (import (miriam assembler opcodes-arm))
  (import (miriam assembler operands-arm))

  (begin

    (define-record-type <code-block>
      (alloc-code-block a b c)
      code-block?
      (a code-text    set-text!)
      (b code-symbols set-symbols!)
      (c code-relocs  set-relocs!))


    (define (assemble code)
      #f)))
