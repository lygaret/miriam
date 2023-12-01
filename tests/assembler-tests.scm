(import (scheme base))
(import (scheme write))

(import (miriam assembler))
(import (miriam assembler output))

(call-with-values
    (lambda () 
      (assemble
       (adr r1 start)
       (mov r0 r1)
       (label start)
       (adr r1 start)
       (mov r0 r1)
       (adr r1 start)))

  (lambda (bytes out)
    (display (bytevector->hex-string bytes))
    (newline)))
