        .section ".text.boot"
        .global _start

_start:
        // read cpu id and stop cores that aren't core 0
        mrs x1, mpidr_el1
        and x1, x1, #3
        cbz x1, 2f

1:      // cpuid > 0, stop the slave core
        wfe
        b 1b

2:      // cpuid == 0, setup and run initialization
        // stack is just before our code
        ldr x1, =_start
        mov sp, x1

        // clear bss
        ldr x1, =__bss_start
        ldr w2, =__bss_size
3:      cbz w2, 4f
        str xzr, [x1], #8
        sub w2, w2, #1
        cbnz w2, 3b

4:      // jump into c, shouldnt return
        // halt this core too as failsafe
        bl kmain
        b  1b

