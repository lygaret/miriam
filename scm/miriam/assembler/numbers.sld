(define-library (miriam assembler numbers)
  (export
   s-hbyte s-byte s-hword s-relword s-word s-dword
   u-hbyte u-byte u-hword u-word u-dword
   u/s-hbyte u/s-byte u/s-hword u/s-word u/s-dword

   sx-hbyte sx-byte sx-hword sx-relword sx-word sx-dword
   ux-hbyte ux-byte ux-hword ux-word ux-dword
   u/sx-hbyte u/sx-byte u/sx-hword u/sx-word u/sx-dword

   u/sx-number

   unsigned-x?
   signed-x?

   number->bytevector
   integer->bytevector
   float32->bytevector
   float64->bytevector)

  (import (scheme base))
  (import (miriam prelude))
  (import (srfi 60)) ; integers-as-bits

  (include "numbers.scm"))
