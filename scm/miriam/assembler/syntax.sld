(define-library (miriam assembler syntax)
  (export b& b+ b<< b>> flag?
          register?
          sregister? sregister-r sregister-pos?
          wregister? wregister-r wregister-w?
          pre-index-reg?
          post-index-reg?
          reglist?
          sysregister?
          sysregister-mask?
          copro?
          copro-register?
          condition?
          shift-type?
          shift-w-imm?
          shift-w-reg?
          operand?
          operand-mode   
          operand-rn     
          operand-shtyp  
          operand-shoff  
          operand-rs     
          operand-imm    
          barrier-option?
          imm3? imm4? imm5? imm16? imm24?
          imm12? -imm12?)

  (import (scheme base))
  (import (scheme cxr))
  (import (scheme write))

  (import (miriam prelude))
  (import (miriam assembler numbers))

  (import (srfi 69)) ; hash-tables
  (import (srfi 60)) ; integers-as-bits

  (include "syntax.scm"))
