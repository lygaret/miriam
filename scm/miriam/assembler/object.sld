(define-library (miriam assembler object)
  (export <asm-object>
          make-asm-object
          asm-object?
          asm-fillptr
          set-asm-fillptr!
          asm-code
          asm-labels
          asm-relocs
          asm-errors

          lookup-label

          emit-error
          emit-align
          emit-bytevector
          emit-relocation
          emit-label

          emit-push-scope
          emit-pop-scope

          call-in-scope)

  (import (scheme base))
  (import (scheme cxr))
  (import (scheme write))

  (import (srfi 69)) ; hash tablse

  (import (miriam prelude))
  (import (miriam logger))
  (import (miriam structs buffer))
  (import (miriam structs append-list))

  (begin

    (define-record-type <asm-object>
      (alloc-asm-object fillptr code labels relocs errors scopes)
      asm-object?
      (fillptr asm-fillptr set-asm-fillptr!)
      (code    asm-code    set-asm-code!)
      (labels  asm-labels  set-asm-labels!)
      (relocs  asm-relocs  set-asm-relocs!)
      (errors  asm-errors  set-asm-errors!)
      (scopes  asm-scopes  set-asm-scopes!))

    (define (make-asm-object)
      (alloc-asm-object #x0000 (make-buffer) '() '() '() '()))

    ;; scopes is a stack of generated symbols
    ;; labels get associated with the top of the scope stack
    ;; relocation get associated with the whole stack (structure sharing)
    ;;   -- walk the stack when looking up labels during relocation

    ;; ---

    (define genscope
      (let ((current 0))
        (lambda ()
          (set! current (+ current 1))
          (string->symbol (string-append "genscope~" (number->string current))))))
          
    (define (push-scope out)
      (set-asm-scopes! out (cons (genscope) (asm-scopes out)))
      (car (asm-scopes out)))

    (define (peek-scope out)
      (if (null? (asm-scopes out))
          '()
          (car (asm-scopes out))))

    (define (pop-scope out)
      (if/let ((retval (peek-scope out)))
        (begin
          (set-asm-scopes! out (cdr (asm-scopes out)))
          retval)
        (error "not in a scope")))

    ;; ---

    (define (push-label out label offset)
      (let ((entry (assoc label (asm-labels out))))
        (when (not entry)
          (set! entry (cons label '()))
          (set-asm-labels! out (cons entry (asm-labels out))))

        (let ((scoped-entry (assoc (peek-scope out) (cdr entry))))
          (when scoped-entry
            (error "label already exists in this scope!" (list label scoped-entry)))

          (set! scoped-entry (cons (peek-scope out) offset))
          (set-cdr! entry (cons scoped-entry (cdr entry))))))

    (define (lookup-scoped-label entry scope)
      (let ((scoped-entry (assoc scope entry)))
        (and scoped-entry (cdr scoped-entry))))

    (define (lookup-label out label scopes)
      (if/let ((entry (assoc label (asm-labels out))))
        (let iter ((next scopes))
          (if (null? next)
              (lookup-scoped-label (cdr entry) '())
              (or
               (lookup-scoped-label (cdr entry) (car next))
               (iter (cdr next)))))
        #f))

    ;; ---

    (define (bump-fillptr out amt)
      (let ((fillptr (+ (asm-fillptr out) amt)))
        (set-asm-fillptr! out (+ (asm-fillptr out) amt))
        fillptr))

    (define (call-with-fillptr out fillptr body)
      (let ((old-fillptr (asm-fillptr out)))
        (dynamic-wind
          (lambda () (set-asm-fillptr! out fillptr))
          body
          (lambda () (set-asm-fillptr! out old-fillptr)))))

    (define (call-in-scope out fillptr scopes body)
      (let ((old-scopes  (asm-scopes out))
            (old-fillptr (asm-fillptr out)))
        (dynamic-wind
          (lambda ()
            (set-asm-fillptr! out fillptr)
            (set-asm-scopes! out scopes))
          body
          (lambda ()
            (set-asm-fillptr! out old-fillptr)
            (set-asm-scopes! out old-scopes)))))

    (define (push-reloc out label form offset)
      (set-asm-relocs! out (cons (list label form offset (asm-scopes out)) (asm-relocs out)))
      0)

    ;; ---

    (define emit-push-scope push-scope)
    (define emit-pop-scope pop-scope)

    (define (emit-error out . err)
      (display (list "emit-error: " err))(newline)
      (aplist-push (asm-errors out) err)
      #f) ; return false so it can be used in bool chains

    (define (emit-label out label)
      (push-label out label (asm-fillptr out)))

    (define (emit-bytevector out bv)
      (write-bytevector bv (buffer-port (asm-code out)))
      (bump-fillptr out (bytevector-length bv)))

    (define (emit-align out alignment)
      (let* ((fillptr (asm-fillptr out))
             (rem     (modulo fillptr alignment)))
        (cond
         ((zero?     rem) fillptr)
         ((negative? rem) (emit-error out "alignment is negative"))
         ((positive? rem)
          (let ((space (make-bytevector (- alignment rem) 0)))
            (emit-bytevector out space))))))

    (define (emit-relocation out label form)
      (if/let ((offset (lookup-label out label (asm-scopes out))))
        (- offset (asm-fillptr out))
        (push-reloc out label form (asm-fillptr out))))))

    (define (test)
      (let ((out (make-asm-object)))
        (emit-label      out '$start)
        (emit-bytevector out #vu8(15 15 15 15 15 15 15 15))
        (emit-relocation out '$start '())
        (emit-relocation out 'foo '())
        (emit-relocation out 'bar '())
        (emit-relocation out '$end '())
        (emit-bytevector out #vu8(0 0 0 0 0 0 0 0))
        (emit-label out 'foo)
        (emit-bytevector out #vu8(0 0 0 0 0 0 0 0))
        (push-scope out)
        (emit-label out '$start)
        (emit-bytevector out #vu8(0 0 0 0 0 0 0 0))
        (emit-relocation out '$start '())
        (emit-relocation out 'foo '())
        (emit-relocation out 'bar '())
        (emit-relocation out '$end '())
        (emit-bytevector out #vu8(0 0 0 0 0 0 0 0))
        (emit-label out '$end)
        (push-scope out)
        (emit-label out '$start)
        (emit-bytevector out #vu8(0 0 0 0 0 0 0 0))
        (emit-relocation out '$start '())
        (emit-relocation out 'foo '())
        (emit-relocation out 'bar '())
        (emit-relocation out '$end '())
        (emit-bytevector out #vu8(0 0 0 0 0 0 0 0))
        (emit-label out '$end)
        (pop-scope out)
        (pop-scope out)
        (emit-bytevector out #vu8(0 0 0 0 0 0 0 0))
        (emit-relocation out '$start '())
        (emit-relocation out 'foo '())
        (emit-relocation out 'bar '())
        (emit-relocation out '$end '())
        (emit-bytevector out #vu8(0 0 0 0 0 0 0 0))
        (emit-label out '$end)
        (emit-bytevector out #vu8(0 0 0 0 0 0 0 0))
        (emit-bytevector out #vu8(0 0 0 0 0 0 0 0))
        (emit-label out 'bar)
        out))
