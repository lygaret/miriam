(define-library (miriam prelude)

  (export ->>
          simple-pair?
          singleton-pair?
          full?
          positive-integer?
          integer-within?)

  (import (scheme base))
  (import (scheme write))
  (import (scheme case-lambda))

  (begin
    (define (full? x)
      (not (null? x)))

    (define simple-pair?
      (case-lambda
        ((x)     (and (pair? x) (full? (cdr x)) (null? (cddr x))))
        ((x key) (and (simple-pair? x)
                      (cond ((procedure? key) (key (car x)))
                            (else             (eq? key (car x))))
                      (cadr x)))))

    (define (singleton-pair? x)
      (and (pair? x) (null? (cdr x)) x))

    (define (positive-integer? x)
      (and (integer? x) (positive? x) x))

    (define (integer-within? min x max)
      (and (integer? x) (<= min x max) x))

    (define-syntax ->>
      (syntax-rules ()
        ((->> value)
         value)
        ((->> value (fn args ...) rest ...)
         (->> (fn args ... value) rest ...))
        ((->> value fn rest ...)
         (->> (fn value) rest ...))))))
