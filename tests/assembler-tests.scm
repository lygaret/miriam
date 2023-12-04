(import (scheme base))
(import (scheme write))

(import (miriam assembler))
(import (miriam assembler output))

(call-with-values
    (lambda () 
      (assemble
       (label $enter)
       (adr r1 $enter)
       (adr r1 $exit)
       (mov r0 r1)
       (block something ()
              (adr r2 $enter)
              (mov r3 r4)
              (mov r4 r5)
              (adr r3 $exit))
       (adr r1 $enter)
       (adr r1 $exit)
       (label $exit)))
  (lambda (bytes out)
    (display (bytevector->hex-string bytes))
    (newline)))
