(define-library (miriam assembler syntax)
  (export register?
          sregister? sregister-r sregister-pos?
          wregister? wregister-r wregister-w?
          pre-index-reg?
          post-index-reg?
          reglist?
          condition?
          shifts-type?
          shift-w-imm?
          shift-w-reg?
          barrier-option?
          imm12? -imm12?)

  (import (scheme base))
  (import (scheme write))

  (import (miriam prelude))
  (import (miriam assembler numbers))

  (import (srfi 69)) ; hash-tables
  (import (srfi 60)) ; integers-as-bits

  (include "syntax.scm"))
