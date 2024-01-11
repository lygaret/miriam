        MMIO_BASE     = 0x3f000000

        VC_MBOX_BASE  = (MMIO_BASE + 0x0000b880)
        MBOX_READ     = 0x00
        MBOX_POLL     = 0x10
        MBOX_SENDER   = 0x14
        MBOX_STATUS   = 0x18
        MBOX_CONFIG   = 0x1C
        MBOX_WRITE    = 0x20

        .comm mbox_buffer, (36 * 4), 16

        .section ".text"
        .global mbox_read
        // x0 contains the channel as a byte
mbox_read:
        stp  x29, x30, [sp, #-16]!

        movz x10, (VC_MBOX_BASE & 0xFFFF)
        movk x10, (VC_MBOX_BASE >> 16), lsl 16
        movz x11, MBOX_STATUS

        // wait until the mailbox is not empty
1:      ldr  w12, [x10, x11]
        tbnz x12, #30, 1b

        // read data
        ldr  w12, [x10]

        // check that it's for us
        and  x13, x12, #0xF
        sub  x13, x13, x0
        cbnz x13, 2f

        // it is for us, strip the channel and return
        and  x0, x12, #0xFFFFFFF0
        ldp  x29, x30, [sp], #16
        ret

        // it is not for us, ignore it and return null
2:      mov  x0, xzr
        ldp  x29, x30, [sp], #16
        ret


        .global mbox_write
        // w0 contains the channel
        // w1 contains the data
mbox_write:
        stp  x29, x30, [sp, #-16]!

        movz x10, (VC_MBOX_BASE & 0xFFFF)
        movk x10, (VC_MBOX_BASE >> 16), lsl 16
        movz x11, MBOX_STATUS

        // wait until the mailbox is not full
1:      ldr  w12, [x10, x11]
        tbnz x12, #31, 1b

        // shift the channel into the data
        adr  x1, mbox_buffer
        and  x1, x1, #0xFFFFFFF0
        orr  x0, x0, x1

        movz x11, MBOX_WRITE
        str  w0, [x10, x11]

        ldp  x29, x30, [sp], #16
        ret
