(define-library (miriam assembler numbers)
  (export
   s-byte s-hword s-word s-dword
   u-byte u-hword u-word u-dword
   u/s-byte u/s-hword u/s-word u/s-dword

   number->bytelist
   integer->bytelist
   float32->bytelist
   float64->bytelist)

  (import (scheme base))
  (import (miriam prelude))
  (import (srfi 151)) ; integers-as-bits

  (include "numbers.scm"))
