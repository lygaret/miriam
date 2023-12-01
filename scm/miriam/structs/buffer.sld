(define-library (miriam structs buffer)
  (export make-buffer
          buffer?
          buffer-port
          buffer-bytevector)

  (import (scheme base))

  ;; a buffer is just a boxed up bytevector output port

  (begin
    (define (make-buffer)
      (cons make-buffer (open-output-bytevector)))

    (define (buffer? obj)
      (and (pair? obj) (eq? make-buffer (car obj))))

    (define (buffer-port obj)
      (and (buffer? obj) (cdr obj)))

    (define (buffer-bytevector obj)
      (and (buffer? obj) (get-output-bytevector (cdr obj))))))
