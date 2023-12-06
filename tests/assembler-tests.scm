(import (scheme base))
(import (scheme write))
(import (ice-9 pretty-print))

(import (miriam assembler))
(import (miriam assembler output))

(let ((result
       (assemble
        (define PBASE                    #x3F000000)
        (define GPFSEL1         (+ PBASE #x00200004))
        (define GPSET0          (+ PBASE #x0020001C))
        (define GPCLR0          (+ PBASE #x00200028))
        (define GPPUD           (+ PBASE #x00200094))
        (define GPPUDCLK0       (+ PBASE #x00200098))
        (define AUX_ENABLES     (+ PBASE #x00215004))
        (define AUX_MU_IO_REG   (+ PBASE #x00215040))
        (define AUX_MU_IER_REG  (+ PBASE #x00215044))
        (define AUX_MU_IIR_REG  (+ PBASE #x00215048))
        (define AUX_MU_LCR_REG  (+ PBASE #x0021504C))
        (define AUX_MU_MCR_REG  (+ PBASE #x00215050))
        (define AUX_MU_LSR_REG  (+ PBASE #x00215054))
        (define AUX_MU_MSR_REG  (+ PBASE #x00215058))
        (define AUX_MU_SCRATCH  (+ PBASE #x0021505C))
        (define AUX_MU_CNTL_REG (+ PBASE #x00215060))
        (define AUX_MU_STAT_REG (+ PBASE #x00215064))
        (define AUX_MU_BAUD_REG (+ PBASE #x00215068))

        (pseudo (ret)
          `(mov pc lr))

        (pseudo (mov32 reg imm)
          `((movw ,reg ,(b& imm #xFFFF))
            (movt ,reg ,(b>> imm 16))))

        (block main ()
          (bl uart-wait)
          (b $enter))

        (block uart-wait ()
          (mov32 r9 ,AUX_MU_LSR_REG)
          (ldr   r10 r9)
          (cmp   r10 0)
          (b ?eq $enter)
          (ret))

        (align 510)
        (resv (hword #xAA55)))))
  (let ((bytes (car result))
        (rest  (cdr result)))
    (call-with-output-file "a.bin"
      (lambda (port)
        (write-bytevector bytes port)))
    (pretty-print rest)(newline)))
