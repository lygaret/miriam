(define-library (miriam assembler output)
  (export bytevector->hex-string)

  (import (scheme base))
  (import (scheme write))

  (begin
    (define (integer->hex-string n)
      (let* ((res (number->string n 16))
             (len (string-length res)))
        (if (even? len)
            res
            (string-append "0" res))))

    (define (bytevector->hex-string bv)
      (let ((out (open-output-string))
            (len (bytevector-length bv)))
        (let lp ((i 0))
          (cond
           ((>= i len)
            (get-output-string out))
           (else
            (write-string (integer->hex-string (bytevector-u8-ref bv i)) out)
            (lp (+ i 1)))))))))
