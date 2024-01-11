        .include "common.inc"

        .global kmain
        .global kexception

kmain:
        bl init_miniuart

hello:
        adr x0, string
        bl  uart_putz

serial:
        adr  x10, mbox_buffer

        movz x12, #(4 * 8)        // size of the message in bytes (8 32bit words)
        str  w12, [x10], #4

        str  wzr, [x10], #4       // request (0)

        movz x12, #0x0003         // tag: get attached display size
        movk x12, #0x0004, lsl 16
        str  w12, [x10], #4

        movz w12, #8              // buffer size
        str  w12, [x10], #4
        str  w12, [x10], #4

        str  wzr, [x10], #4       // clear output buffer
        str  wzr, [x10], #4

        str  wzr, [x10]           // tag last

        mov  x0,  #8              // read from property channel
        bl   mbox_write

        mov  x0,  #8
        bl   mbox_read

echo:   mov x0, '\n'
        bl  uart_putb
        bl  uart_getb
echo2:  sub x20, x0, #113
        cbz x20, exit
        bl  uart_putb
        b   echo

exit:   svc #4

kexception:
        mov x20, x0
        adr x0, string
        bl  uart_putz
        add x0, x20, #48          // offset in ascii for digits
        bl  uart_putb
        mov x0, '\n'
        bl  uart_putb
        mov x0, '\r'
        bl  uart_putb

        wfe
        b kexception

        .section ".data"
string:
        .asciz "hello, world!\n\r"
excstring:
        .asciz "exception: "
