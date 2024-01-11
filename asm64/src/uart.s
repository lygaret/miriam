        MMIO_BASE     = 0x3F000000

        GPFBASE       = (MMIO_BASE + 0x00200000)
        GPFSEL0       = 0x0000
        GPFSEL1       = 0x0004
        GPFSEL2       = 0x0008
        GPFSEL3       = 0x000C
        GPFSEL4       = 0x0010
        GPFSEL5       = 0x0014
        GPSET0        = 0x001C
        GPSET1        = 0x0020
        GPCLR0        = 0x0028
        GPLEV0        = 0x0034
        GPLEV1        = 0x0038
        GPEDS0        = 0x0040
        GPEDS1        = 0x0044
        GPHEN0        = 0x0064
        GPHEN1        = 0x0068
        GPPUD         = 0x0094
        GPPUDCLK0     = 0x0098
        GPPUDCLK1     = 0x009C

        UART_BASE      = (MMIO_BASE + 0x00210000)
        AUX_ENABLE     = 0x5004
        AUX_MU_IO      = 0x5040
        AUX_MU_IER     = 0x5044
        AUX_MU_IIR     = 0x5048
        AUX_MU_LCR     = 0x504C
        AUX_MU_MCR     = 0x5050
        AUX_MU_LSR     = 0x5054
        AUX_MU_MSR     = 0x5058
        AUX_MU_SCRATCH = 0x505C
        AUX_MU_CNTL    = 0x5060
        AUX_MU_STAT    = 0x5064
        AUX_MU_BAUD    = 0x5068

        .section ".text"
        .global uart_init
uart_init:
        stp  x29, x30, [sp, #-16]!

        // map the uart to gpio pins
        mov  x10, GPFBASE
        movz x11, GPFSEL1
        ldr  w12, [x10, x11]

        // GPFSEL1 &= 0xfffc0fff (7 << 12 | 7 << 15)
        movz w13, #0x0fff
        movk w13, #0xfffc, lsl 16
        and  w12, w12, w13

        // GPFSEL1 |= 0x00012000 (2 << 12 | 2 << 15)
        movz w13, #0x2000
        movk w13, #0x0001, lsl 16
        orr  w12, w12, w13
        strh w12, [x10, x11]

        // disable gpio pull-up/down
        movz x11, GPPUD
        str xzr, [x10, x11] /* *GPPUD = 0 */

        // delay for a bit
        mov  x12, 150
1:      subs x12, x12, #1
        b.ne 1b

        // assert clock lines 14 & 15
        movz x11, GPPUDCLK0
        movz x12, #0xc000
        str  x12, [x10, x11]

        // delay for a bit
        mov  x12, 150
1:      subs x12, x12, #1
        b.ne 1b

        // flush
        str xzr, [x10, x11]

        // enable the miniuart
        mov  x10, UART_BASE

        // AUX_ENABLE |= 1
        movz x11, AUX_ENABLE
        ldr  x12, [x10, x11]
        orr  x12, x12, #1
        str  x12, [x10, x11]

        // disable interrupts
        movz x11, AUX_MU_IER
        str xzr, [x10, x11]

        // disable tx/rx
        movz x11, AUX_MU_CNTL
        str xzr, [x10, x11]

        // set 8bit comms
        movz x11, AUX_MU_LCR
        movz x12, #3
        str  x12, [x10, x11]

        // set the reset line high
        movz x11, AUX_MU_MCR
        str xzr, [x10, x11]

        // leave interrupts disabled
        movz x11, AUX_MU_IER
        str xzr, [x10, x11]

        // clear the io buffers
        movz x11, AUX_MU_IIR
        movz x12, #198
        str  x12, [x10, x11]

        movz x11, AUX_MU_BAUD
        movz x21, #270
        str  x21, [x10, x11]

        // enable tx, rx
        mov  x10, UART_BASE
        movz x11, AUX_MU_CNTL
        movz x12, #3
        str  x12, [x10, x11]

        ldp  x29, x30, [sp], #16
        ret


        /* uart-putb
                w0 - character to put byte to put on the uart
        */
        .global uart_putb
uart_putb:
        stp  x29, x30, [sp, #-16]!

        mov  x10, UART_BASE
        movz x11, AUX_MU_LSR

        // wait until there's nothing in the buffer
        // looping on bit 6 of the LSR register
1:      ldr x12, [x10, x11]
        tbz x12, #6, 1b

        // store the input to the io register
        movz x11, AUX_MU_IO
        strb w0, [x10, x11]

        ldp  x29, x30, [sp], #16
        ret


        /* uart-ready
        ret - sets the condition code nz if the uart is able to read, z otherwise
        */
        .global uart_ready
uart_ready:
        stp  x29, x30, [sp, #-16]!

        mov  x10, UART_BASE
        movz x11, AUX_MU_LSR

        // set conditions on bit 1
        ldr  x12, [x10, x11]
        ands x0, x12, #1

        ldp  x29, x30, [sp], #16
        ret


        /* uart-getb
                ret - a character read from the uart */
        .global uart_getb
uart_getb:
        stp  x29, x30, [sp, #-16]!

        mov  x10, UART_BASE
        movz x11, AUX_MU_LSR

        // wait until there's something available
        // looping on bit 1 of the LSR register
1:      ldr x12, [x10, x11]
        tbz x12, #0, 1b

        // fetch from the io register
        movz x11, AUX_MU_IO
        ldrb w0, [x10, x11]

        ldp  x29, x30, [sp], #16
        ret


        /* uart-putz
                x0 - pointer to zero-terminated string to put byte to put on the uart
        */
        .global uart_putz
uart_putz:
        stp  x29, x30, [sp, #-16]!

        mov  x1, x0
1:      ldrb w0, [x1], #1
        bl   uart_putb
        cbnz w0, 1b

        ldp  x29, x30, [sp], #16
        ret
