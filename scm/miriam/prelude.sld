(define-library (miriam prelude)

  (export ->>
          ensure-list
          unwrap-singleton
          simple-pair?
          singleton-pair?
          full?
          positive-integer?
          negative-integer?
          integer-within?

          ->bin
          ->hex

          let/cc
          if/let
          when/let)

  (import (scheme base))
  (import (scheme write))
  (import (scheme case-lambda))

  (begin
    (define (->hex n) (number->string n 16))
    (define (->bin n) (number->string n 2))

    (define (full? x)
      (not (null? x)))

    (define (ensure-list x)
      (if (list? x) x (list x)))

    (define simple-pair?
      (case-lambda
        ((x)     (and (pair? x) (full? (cdr x)) (null? (cddr x))))
        ((x key) (and (simple-pair? x)
                      (cond ((procedure? key) (key (car x)))
                            (else             (eq? key (car x))))
                      (cadr x)))))

    (define (singleton-pair? x)
      (and (pair? x) (null? (cdr x)) x))

    (define (unwrap-singleton x)
      (if (singleton-pair? x) (car x) x))

    (define (positive-integer? x)
      (and (integer? x) (positive? x) x))

    (define (negative-integer? x)
      (and (integer? x) (negative? x) x))

    (define (integer-within? min x max)
      (and (integer? x) (<= min x max) x))

    (define-syntax let/cc
      (syntax-rules ()
        ((_ (k) rest ...)
         (call/cc (lambda (k) rest ...)))))

    (define-syntax when/let
      (syntax-rules ()
        ((_ ((name test) ...) body ...)
         (let ((name test) ...)
           (and name ...
                (begin body ...))))))

    (define-syntax if/let
      (syntax-rules ()
        ((_ ((name test) ...) then else)
         (let ((name test) ...)
           (if (and name ...) then else)))))

    (define-syntax ->>
      (syntax-rules ()
        ((->> value)
         value)
        ((->> value (fn args ...) rest ...)
         (->> (fn args ... value) rest ...))
        ((->> value fn rest ...)
         (->> (fn value) rest ...))))))
