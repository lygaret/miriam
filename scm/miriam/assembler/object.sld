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
          lookup-reloc

          emit-error
          emit-align
          emit-bytevector
          emit-relocation
          emit-label)

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
      (alloc-asm-object fillptr code labels relocs errors)
      asm-object?
      (fillptr asm-fillptr set-asm-fillptr!)
      (code    asm-code)
      (labels  asm-labels)
      (relocs  asm-relocs)
      (errors  asm-errors))

    (define (make-asm-object)
      (alloc-asm-object
       #x0000 ; offset
       (make-buffer)           ; code (output port)
       (make-hash-table eqv?)  ; labels
       (make-hash-table eqv?)  ; relocs
       (make-append-list)))    ; errors

    (define (lookup-label out label)
      (hash-table-ref (asm-labels out) label (lambda () #f)))

    (define (push-label out label offset)
      (hash-table-set! (asm-labels out) label offset))

    (define (lookup-reloc out label)
      (hash-table-ref (asm-relocs out) label (lambda () #f)))

    (define (push-reloc out label form offset)
      (let ((entry (or (lookup-reloc out label) '())))
        (hash-table-set! (asm-relocs out) label (cons (cons form offset) entry))))

    (define (bump-fillptr out amt)
      (let ((fillptr (+ (asm-fillptr out) amt)))
        (set-asm-fillptr! out (+ (asm-fillptr out) amt))
        fillptr))

    (define (emit-error out . err)
      (display (list "emit-error: " err))(newline)
      (aplist-push (asm-errors out) err)
      #f) ; return false so it can be used in bool chains

    (define (emit-label out label)
      (if (lookup-label out label)
          (emit-error out (list "label already defined" label))
          (push-label out label (asm-fillptr out))))

    (define (emit-bytevector out bv)
      (write-bytevector bv (buffer-port (asm-code out)))
      (bump-fillptr out (bytevector-length bv)))

    (define (emit-align out alignment)
      (let* ((fillptr (asm-fillptr out))
             (rem     (modulo fillptr alignment)))
        (cond
         ((zero?     rem) fillptr)
         ((positive? rem) (bump-fillptr out (- alignment rem)))
         ((negative? rem) (emit-error out "alignment is negative")))))

    (define (emit-relocation out label form)
      (if/let ((offset (lookup-label out label)))
        (- offset (asm-fillptr out))
        (begin
          (push-reloc out label form (asm-fillptr out))
          0)))))
