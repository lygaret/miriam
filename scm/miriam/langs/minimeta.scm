;; https://web.archive.org/web/20190123025828/http://home.pipeline.com/~hbaker1/Prag-Parse.html

(define-syntax minimeta
  (syntax-rules (or and lambda)
    ((_ (or x ...))
     (lambda (tt forms)
       (gen-parse forms forms (tt) (and (or x ...)))))

    ((_ (and x ...))
     (lambda (tt forms)
       (gen-parse forms forms (tt) (and x ...))))

    ((_ (lambda (t) x ...))
     (lambda (tt forms)
       (gen-parse forms forms (tt) (and (lambda (t) x ...)))))

    ((_ (lambda (t . rest) x ...))
     (lambda (tt forms)
       (gen-parse forms forms (tt) (and (lambda (t . rest) x ...)))))))

(define-syntax minimeta-cond
  (syntax-rules ()
    ((_ forms ...)
     (minimeta (or forms ...)))))

(define-syntax gen-parse
  (syntax-rules (and call lambda or and begin quote ?* ? !)

    ((_ _ forms (tt) (and (lambda (t1) bdy ...)))
     (and (null? forms)
          (let ((t1 tt))
            (begin bdy ...))))

    ((_ top forms (tt) (and (lambda (t1 . rest) bdy ...)))
     (and (null? forms)
          (let ((t1   tt)
                (rest top)
            (begin bdy ...)))))

    ((_ _ forms (m1 tt) (and (lambda (t1 a1) bdy ...)))
     (and (null? forms)
          (let ((a1 m1)
                (t1 tt))
            (begin bdy ...))))

    ((_ top forms (m1 tt) (and (lambda (t1 a1 . rest) bdy ...)))
     (and (null? forms)
          (let ((a1   m1)
                (t1   tt)
                (rest top))
            (begin bdy ...))))

    ((_ _ forms (m2 m1 tt) (and (lambda (t1 a1 a2) bdy ...)))
     (and (null? forms)
          (let ((a1 m1) (a2 m2) (t1 tt))
            (begin bdy ...))))

    ((_ top forms (m2 m1 tt) (and (lambda (t1 a1 a2 . rest) bdy ...)))
     (and (null? forms)
          (let ((a1 m1) (a2 m2) (t1 tt) (rest top))
            (begin bdy ...))))

    ((_ _ forms (m3 m2 m1 tt) (and (lambda (t1 a1 a2 a3) bdy ...)))
     (and (null? forms)
          (let ((a1 m1) (a2 m2) (a3 m3) (t1 tt))
            (begin bdy ...))))

    ((_ top forms (m3 m2 m1 tt) (and (lambda (t1 a1 a2 a3 . rest) bdy ...)))
     (and (null? forms)
          (let ((a1 m1) (a2 m2) (a3 m3) (t1 tt) (rest top))
            (begin bdy ...))))

    ((_ _ forms (m4 m3 m2 m1 tt) (and (lambda (t1 a1 a2 a3 a4) bdy ...)))
     (and (null? forms)
          (let ((a1 m1) (a2 m2) (a3 m3) (a4 m4) (t1 tt))
            (begin bdy ...))))

    ((_ top forms (m4 m3 m2 m1 tt) (and (lambda (t1 a1 a2 a3 a4 . rest) bdy ...)))
     (and (null? forms)
          (let ((a1 m1) (a2 m2) (a3 m3) (a4 m4) (t1 tt) (rest top))
            (begin bdy ...))))

    ((_ _ forms (m5 m4 m3 m2 m1 tt) (and (lambda (t1 a1 a2 a3 a4 a5) bdy ...)))
     (and (null? forms)
          (let ((a1 m1) (a2 m2) (a3 m3) (a4 m4) (a5 m5) (t1 tt))
            (begin bdy ...))))

    ((_ top forms (m5 m4 m3 m2 m1 tt) (and (lambda (t1 a1 a2 a3 a4 a5 . rest) bdy ...)))
     (and (null? forms)
          (let ((a1 m1) (a2 m2) (a3 m3) (a4 m4) (a5 m5) (t1 tt) (rest top))
            (begin bdy ...))))

    ((_ _ forms (m6 m5 m4 m3 m2 m1 tt) (and (lambda (t1 a1 a2 a3 a4 a5 a6) bdy ...)))
     (and (null? forms)
          (let ((a1 m1) (a2 m2) (a3 m3) (a4 m4) (a5 m5) (a6 m6) (t1 tt))
            (begin bdy ...))))

    ((_ top forms (m6 m5 m4 m3 m2 m1 tt) (and (lambda (t1 a1 a2 a3 a4 a5 a6 . rest) bdy ...)))
     (and (null? forms)
          (let ((a1 m1) (a2 m2) (a3 m3) (a4 m4) (a5 m5) (a6 m6) (t1 tt) (rest top))
            (begin bdy ...))))

    ((_ _ forms (m7 m6 m5 m4 m3 m2 m1 tt) (and (lambda (t1 a1 a2 a3 a4 a5 a6 a7) bdy ...)))
     (and (null? forms)
          (let ((a1 m1) (a2 m2) (a3 m3) (a4 m4) (a5 m5) (a6 m6) (a7 m7) (t1 tt))
            (begin bdy ...))))

    ((_ top forms (m7 m6 m5 m4 m3 m2 m1 tt) (and (lambda (t1 a1 a2 a3 a4 a5 a6 a7 . rest) bdy ...)))
     (and (null? forms)
          (let ((a1 m1) (a2 m2) (a3 m3) (a4 m4) (a5 m5) (a6 m6) (a7 m7) (t1 tt) (rest top))
            (begin bdy ...))))

    ((_ _ forms (m8 m7 m6 m5 m4 m3 m2 m1 tt) (and (lambda (t1 a1 a2 a3 a4 a5 a6 a7 a8) bdy ...)))
     (and (null? forms)
          (let ((a1 m1) (a2 m2) (a3 m3) (a4 m4) (a5 m5) (a6 m6) (a7 m7) (a8 m8) (t1 tt))
            (begin bdy ...))))

    ((_ top forms (m8 m7 m6 m5 m4 m3 m2 m1 tt) (and (lambda (t1 a1 a2 a3 a4 a5 a6 a7 a8 . rest) bdy ...)))
     (and (null? forms)
          (let ((a1 m1) (a2 m2) (a3 m3) (a4 m4) (a5 m5) (a6 m6) (a7 m7) (a8 m8) (t1 tt) (rest top))
            (begin bdy ...))))

    ((_ _ forms matches (and (begin bdy ...)))
     (and (null? forms) (begin bdy ...)))

    ((_ top forms matches (and (quote blah) rst ...))
     (and (not (null? forms))
          (equal? 'blah (car forms))
          (gen-parse top (cdr forms) matches (and rst ...))))

    ((_ top forms matches (and (?* (quote blah)) rst ...))
     (cond ((null? forms) (gen-parse top forms matches (and rst ...)))
           ((equal? (car forms) 'blah)
            (let ((next (cdr forms)))
              (gen-parse top next matches (and rst ...))))
           (else (gen-parse top forms matches (and rst ...)))))

    ((_ top forms matches (and (or c1 ...) . rst))
     (or (gen-parse top forms matches (and c1 . rst))
         ...))

    ((_ top forms matches (and (and y ...)))
     (gen-parse top forms matches (and y ...)))

    ((_ top forms (x ...) (and ? rst ...))
     (let ((t (car forms)))
       (let ((next (cdr forms)))
         (gen-parse top next (t x ...) (and rst ...)))))

    ((_ top forms matches (and (! c) rst ...))
     (and c (gen-parse top forms matches (and rst ...))))

    ((_ top forms (x ...) (and (? c1 default) rst ...))
     (let ((g (and (not (null? forms)) (c1 (car forms)))))
       (gen-parse top (if g (cdr forms) forms) ((or g default) x ...) (and rst ...))))

    ((_ top forms (x ...) (and pred rst ...))
     (let ((t (and (not (null? forms))
                   (let ((g (pred (car forms))))
                     (and g (if (eq? g #t)
                                (car forms)
                                g))))))
       (and t (let ((next (cdr forms)))
                (gen-parse top next (t x ...) (and rst ...))))))))
