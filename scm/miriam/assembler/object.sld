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
          patch-reloc

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

  (import (srfi 1))  ; lists
  (import (srfi 8))  ; recieve
  (import (srfi 13)) ; strings
  (import (srfi 69)) ; hash tablse
  (import (rnrs bytevectors))

  (import (miriam prelude))
  (import (miriam logger))
  (import (miriam assembler syntax))
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

    (define (label-descoped? label)
      (let* ((name  (symbol->string label))
             (count (string-suffix-length name "^^^^^^^^^^^^^^^^")))
        (and (< 0 count)
             (cons count (string->symbol (string-drop-right name count))))))

    (define (lookup-scoped-label out label scopes)
      (if/let ((entry (assoc label (asm-labels out))))
              (let iter ((next scopes))
                (if (null? next)
                    #f
                    (let ((scoped-entry (assq (car next) (cdr entry))))
                      (or (and scoped-entry (cdr scoped-entry))
                          (iter (cdr next))))))
              #f))

    (define (lookup-descoped-label out label scopes)
      (if/let ((descope (label-descoped? label)))
              (let ((count (car descope))
                    (label (cdr descope)))
                (lookup-scoped-label out label (drop scopes count)))
              #f))

    (define (lookup-label out label scopes)
      (or (lookup-descoped-label out label scopes)
          (lookup-scoped-label out label scopes)))

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

    (define (push-reloc out label type offset)
      (set-asm-relocs! out (cons (list label type offset (asm-scopes out)) (asm-relocs out)))
      0)

    (define (resolve-reloc out type fillptr offset)
      (case type
        ((pc24)  (simm24?    (/ (- offset fillptr 8) 4)))
        ((pc13)  (u/s-imm12? (- offset fillptr 8)))
        ((abs32) offset)
        (else    0)))

    (define (eval-reloc type fillptr offset)
      (case type
        ((abs32)
         (values #x00000000 (b& #xFFFFFFFF offset)))
        ((pc24)
         (values #xFF000000 (b& #xFFFFFFFF (/ (- offset fillptr 8) 4))))
        ((pc13)
         (let* ((pcrel (/ (- offset fillptr 8) 4))
                (u     (if (<= 0 pcrel) 1 0))
                (pcrel (b& #xFFF (abs pcrel))))
           (values #xFFEFF000 (b+ (b<< u 20) (b& #xFFF (abs pcrel))))))))

    (define (patch-reloc bytes type fillptr offset)
      (let* ((little (endianness little))
             (word   (bytevector-u32-ref bytes fillptr little)))
        (let-values (((mask value) (eval-reloc type fillptr offset)))
          (log "patch reloc" (number->string mask 16) (number->string value 16) (number->string word 16) type fillptr offset)
          (bytevector-u32-set! bytes fillptr (b+ (b& (b~ mask) value) (b& mask word)) little))))

    ;; ---

    (define emit-push-scope push-scope)
    (define emit-pop-scope pop-scope)

    (define (emit-error out . err)
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

    (define (emit-relocation out label type)
      (if/let ((offset (lookup-label out label (asm-scopes out))))
              (resolve-reloc out type (asm-fillptr out) offset)
              (push-reloc out label type (asm-fillptr out))))

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
        out))))
