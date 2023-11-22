(define-library (miriam assembler numbers)
  (export
   s-hbyte s-byte s-hword s-relword s-word s-dword
   u-hbyte u-byte u-hword u-word u-dword
   u/s-hbyte u/s-byte u/s-hword u/s-word u/s-dword

   number->bytelist
   integer->bytelist
   float32->bytelist
   float64->bytelist)

  (import (scheme base))
  (import (miriam prelude))
  (import (srfi 60)) ; integers-as-bits

  (include "numbers.scm"))
