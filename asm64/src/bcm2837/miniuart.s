        .include "common.inc"
        .include "bcm2837/gpio.inc"

        UART_BASE      = (MMIO_BASE + 0x00215000)

        AUX_ENABLE     = 0x04
        AUX_MU_IO      = 0x40
        AUX_MU_IER     = 0x44
        AUX_MU_IIR     = 0x48
        AUX_MU_LCR     = 0x4C
        AUX_MU_MCR     = 0x50
        AUX_MU_LSR     = 0x54
        AUX_MU_MSR     = 0x58
        AUX_MU_SCRATCH = 0x5C
        AUX_MU_CNTL    = 0x60
        AUX_MU_STAT    = 0x64
        AUX_MU_BAUD    = 0x68

        .func   init_miniuart
        .global init_miniuart
init_miniuart:
        subr_enter 0

        // map the uart to gpio pins
        mov  t1,  GPFBASE

        //// GPFSEL1 selecting pins 14/15, alt-mode 0
        ldr  tw2, [t1, GPFSEL1]
        movz tw3, #0x0fff  //// &= 0xfffc0fff (7 << 12 | 7 << 15)
        movk tw3, #0xfffc, lsl 16
        and  tw2, tw2, tw3
        movz tw3, #0x2000  //// |= 0x00012000 (2 << 12 | 2 << 15)
        movk tw3, #0x0001, lsl 16
        orr  tw2, tw2, tw3
        str  tw2, [t1, GPFSEL1]

        //// disable gpio pull-up/down
        str  wzr, [t1, GPPUD]

        //// strobe the clock lines 14/15
        movz tw2, #0xc000
        delay 150
        str  tw2, [t1, GPPUDCLK0]
        delay 150
        str  wzr, [t1, GPPUDCLK0]

        // configure the miniuart
        movz t1, (UART_BASE & 0xFFFF)
        movk t1, (UART_BASE >> 16), lsl 16

        //// AUX_ENABLE |= 1
        ldr  tw2, [t1, AUX_ENABLE]
        orr  tw2, tw2, #1
        str  tw2, [t1, AUX_ENABLE]

        //// disable interrupts
        str  wzr, [t1, AUX_MU_IER]

        //// disable tx/rx
        str  wzr, [t1, AUX_MU_CNTL]

        //// set 8bit comms
        movz tw2, #3
        str  tw2, [t1, AUX_MU_LCR]

        //// set the reset line high
        str  wzr, [t1, AUX_MU_MCR]

        //// leave interrupts disabled
        str  wzr, [t1, AUX_MU_IER]

        //// clear the io buffers
        movz tw2, #198
        str  tw2, [t1, AUX_MU_IIR]

        //// set 115200 baud
        movz tw2, #270
        str  tw2, [t1, AUX_MU_BAUD]

        //// enable tx, rx
        movz tw2, #3
        str  tw2, [t1, AUX_MU_CNTL]

        subr_ret 0
        .endfunc



        /* uart-ready
                takes nothing
                returns ready bit if read is possible
        */
        .global uart_ready
        .func   uart_ready
uart_ready:
        subr_enter 0

        movz t1, (UART_BASE & 0xFFFF)
        movk t1, (UART_BASE >> 16), lsl 16

        ldr  x0, [t1, AUX_MU_LSR] // set conditions on bit 1
        ands x0, x0, #1

        subr_ret 0
        .endfunc



        /* uart-getb
                takes nothing
                waits until a byte is available on the uart
                returns that byte, zero-extended
        */
        .global uart_getb
        .func   uart_getb
uart_getb:
        subr_enter 0

        movz t1, (UART_BASE & 0xFFFF)
        movk t1, (UART_BASE >> 16), lsl 16

        // wait until there's something available
        // looping on bit 1 of the LSR register
1:      ldr  tw2, [t1, AUX_MU_LSR]
        tbz  tw2, #0, 1b

        // fetch from the io register
        ldrb w0, [t1, AUX_MU_IO]

        subr_ret 0
        .endfunc



        /* uart-putb
                takes byte to put to the uart
                returns byte that was sent (non-volatile)
        */
        .global uart_putb
        .func   uart_putb
uart_putb:
        subr_enter 0
        movz t1, (UART_BASE & 0xFFFF)
        movk t1, (UART_BASE >> 16), lsl 16

        // loop on bit 6 of the LSR register
        // wait until there's nothing in the buffer
1:      ldr tw2, [t1, AUX_MU_LSR]
        tbz tw2, #6, 1b

        // store the input to the io register
        strb w0, [t1, AUX_MU_IO]
        subr_ret 0

        .endfunc



        /* uart-putz
                takes pointer to zero-terminated byte-string
                sends one byte at a time to the uart
        */
        .global uart_putz
        .func   uart_putz
uart_putz:
        subr_enter 0

        movz t1, (UART_BASE & 0xFFFF)
        movk t1, (UART_BASE >> 16), lsl 16

        // loop on bit 6 of the LSR register
        // wait until there's nothing in the buffer
1:      ldr tw2, [t1, AUX_MU_LSR]
        tbz tw2, #6, 1b

        // store the input to the io register
        ldrb tw2, [x0], #1
        strb tw2, [t1, AUX_MU_IO]

        // keep going until zero byte
        cbnz tw2, 1b

        subr_ret 0
        .endfunc
