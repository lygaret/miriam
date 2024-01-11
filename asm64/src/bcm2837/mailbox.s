        .include "common.inc"
        .include "bcm2837/gpio.inc"

        VC_MBOX_BASE  = (MMIO_BASE + 0x0000b880)

        MBOX_READ     = 0x00
        MBOX_POLL     = 0x10
        MBOX_SENDER   = 0x14
        MBOX_STATUS   = 0x18
        MBOX_CONFIG   = 0x1C
        MBOX_WRITE    = 0x20


        /* shared mailbox, up to 36 words */
        .comm mbox_buffer, (36 * 4), 16



        /* read from the mailbox given in w0 */
        .global mbox_read
        .func   mbox_read
mbox_read:
        subr_enter 0

        movz t1, (VC_MBOX_BASE & 0xFFFF)
        movk t1, (VC_MBOX_BASE >> 16), lsl 16

        // wait until the mailbox is not empty
1:      ldr  tw3, [t1, MBOX_STATUS]
        tbnz tw3, #30, 1b

        // read data
        ldr  tw3, [t1, MBOX_READ]

        // check that it's for us
        and  tw4, tw3, #0xF
        sub  tw4, tw4, w0
        cbnz tw4, 2f

        // it is for us, strip the channel and return
        and  w0, tw3, #0xFFFFFFF0
        subr_ret 0

        // it is not for us, ignore it and return null
2:      mov  w0, wzr
        subr_ret 0
        .endfunc



        /* write to the mailbox given in w0, payload in mbox_buffer */
        .global mbox_write
        .func   mbox_write
mbox_write:
        subr_enter 0

        movz t1, (VC_MBOX_BASE & 0xFFFF)
        movk t1, (VC_MBOX_BASE >> 16), lsl 16

        // wait until the mailbox is not full
1:      ldr  tw2, [t1, MBOX_STATUS]
        tbnz tw2, #31, 1b

        // shift the channel into the data
        adr  t2, mbox_buffer
        and  t2, t2, #0xFFFFFFF0
        orr  x0, x0, t2
        str  w0, [t1, MBOX_WRITE]

        subr_ret 0
        .endfunc
