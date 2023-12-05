(define-library (miriam assembler eval)
  (export
   b& b+ b<< b>>)

  (import
   (scheme base)
   (srfi 60))

  (begin
    (define b&      bitwise-and)
    (define b+      bitwise-ior)
    (define b<<     arithmetic-shift)
    (define (b>> n s) (arithmetic-shift n (- s)))))
