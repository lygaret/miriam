(define-library (miriam structs append-list)
  (export make-append-list
          aplist?
          aplist-push
          aplist-list)

  (import (scheme base))

  ;; a buffer is just a boxed up bytevector output port

  (begin
    (define (make-append-list)
      (cons make-append-list '()))

    (define (aplist? obj)
      (and (pair? obj) (eq? make-append-list (car obj))))

    (define (aplist-push obj item)
      (and (aplist? obj) (set-cdr! obj (cons item (cdr obj)))))

    (define (aplist-pop obj)
      (and (aplist? obj)
           (let ((item (cadr obj)))
             (set-cdr! obj (cddr obj)))))

    (define (aplist-list obj)
      (and (aplist? obj) (cdr obj)))))
