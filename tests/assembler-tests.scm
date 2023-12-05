(import (scheme base))
(import (scheme write))
(import (ice-9 pretty-print))

(import (miriam assembler))
(import (miriam assembler output))

(call-with-values
    (lambda () 
      (assemble

       (label main)
       (adr r0 table)    ;; load address of table
       (eor r1 r1 r1)    ;; clear sum
       (ldr r2 length)   ;; init the count

       (block loop ()
         (ldr r3 (r0 ++))
         (add r1 r1 r3)
         (add r0 r0 4)
         (subs r2 r2 1)
         (b ?ne $enter)
         (str r1 result)
         (swi 11))

       (label table)
       (res (word #x2040))(align word)
       (res (word #x1C22))(align word)
       (res (word #x0242))(align word)
       (label table-end)

       (label length)
       (res (word 3))

       (label result)
       (res (word 0))

       (align 510)
       (res (hword #xAA55))))
  (lambda (bytes out)
    (call-with-output-file "a.bin"
      (lambda (port)
        (write-bytevector bytes port)))
    (pretty-print out)(newline)))
