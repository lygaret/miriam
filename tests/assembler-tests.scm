(import (scheme base))
(import (scheme write))

(import (srfi 1))
(import (ice-9 pretty-print))

(import (miriam assembler))
(import (miriam assembler object))
(import (miriam assembler output))

(define (read-input path)
  (with-input-from-file path
    (lambda () 
      (unfold eof-object? values (lambda (x) (read)) (read)))))

(define (write-rawbin bytes path)
  (with-output-to-file path
    (lambda ()
      (write-bytevector bytes))))

(let* ((code    (read-input "../tests/uart-test.ssm"))
       (results (assemble code))
       (bytes   (car results))
       (results (cdr results)))

  ;; write the binary
  (write-rawbin bytes "uart.bin")

  ;; display errors
  (newline)(display "errors:")(newline)
  (do ((next (asm-errors results) (cdr next)))
      ((null? next))
    (display (car next)) (newline))

  ;; display symbol offsets
  (newline)(display "labels:")(newline)
  (do ((next (asm-labels results) (cdr next)))
      ((null? next))
    (display (car next)) (newline)))
