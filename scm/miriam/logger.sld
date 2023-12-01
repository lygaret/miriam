(define-library (miriam logger)
  (export
    log)

  (import
   (scheme base)
   (scheme write))

  (begin
    (define-syntax log
      (syntax-rules ()
        ((_ msg item ...)
         (begin
           (display msg)
           (display " -")
           (begin
             (display " ")
             (display 'item)
             (display ": ")
             (display item)) ...
           (newline)))))))
