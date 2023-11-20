(reserve uart-tx-buffer (dwords 256))
(reserve uart-rx-buffer (dwords 256))

(const   uart-base-addr (dword #x12345678))

(block uart-init (export)
  (mov r0 (imm 10))
  (mov r1 (imm  3))
  (bl (local doadd))

  (label stop
    (mov r0 (imm #x18))
    (ldr r1 (ref #x20026))
    (svc #x123456))

  (label doadd
    (add r0 r0 r1)))
