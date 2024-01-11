        .section ".text.boot"
        .global _start

_start:
        // read cpu id and stop cores that aren't core 0
        mrs x1, mpidr_el1
        and x1, x1, #3
        cbz x1, 2f

1:      // cpuid > 0, stop the other cores
        wfe
        b 1b

        // cpuid == 0, setup and run initialization
        // stack is just before our code
2:      ldr x1, =_start
        mov sp, x1

        // get into a known exception level state
        mrs x0, CurrentEl
        and x0, x0, #0b1100

        //// check EL3? or jump to 1
        cmp x0, #0b1100
        bne 1f

        //// configure el2 before jumping down into it
        ////// el2 stack pointer to _start (via x1)
        msr sp_el2, x1

        //// configure el3 to basically be hidden away
        ////// disable el3 interrupts, allow 64-bit el2
        mov x2, #0b010110110001
        msr scr_el3, x2

        //// spsr set to return to el2h / all events masked
        mov x2, #0b1111001001
        msr spsr_el3, x2

        //// put label 1f in the return addr for the exception return
        adr x2, 1f
        msr elr_el3, x2

        //// jump into el2
        eret

        //// check EL2?, or jump forward
1:      cmp x0, #0b0100
        bne 1f

        //// setup el2 (hyp) level interrupt
        ldr x2, =_vectors
        msr vbar_el2, x2

        //// allow cntp for el1 (counter timer hypervisor control)
        mrs x0, cnthctl_el2
        orr x0, x0, #0b0011
        msr cnthctl_el2, x0

        //// disable the el2 physical timer
        msr cnthp_ctl_el2, xzr

        //// since we're loading at the hypervisor level
        //// ensure lower levels can see the processor id
        mrs x0, midr_el1
        mrs x2, mpidr_el1
        msr vpidr_el2, x0
        msr vmpidr_el2, x2

        //// disable coprocessor traps
        mov x0, #0x33ff
        msr cptr_el2, x0
        msr hstr_el2, xzr
        mov x0, #(3 << 20)
        msr cpacr_el1, x0

        //// configure el1 before jumping down into it

        ////// el1 stack pointer to _start (via x1)
        msr sp_el1, x1

        ////// el1 runs in aarch64
        mov x0, #(1 << 31)
        orr x0, x0, #(1 << 1)
        msr hcr_el2, x0
        mrs x0, hcr_el2

        ////// setup sctlr access
        mov  x2, #0x0800
        movk x2, #0x30d0, lsl #16
        msr  sctlr_el1, x2

        //// spsr set to return to el2h / all events masked
        mov x2, #0b1111000100
        msr spsr_el2, x2

        //// put label 1f in the return addr for the exception return
        adr x2, 1f
        msr elr_el2, x2

        //// jump into el1
        eret

        // EL1
1:      //// setup exception handlers
        ldr x2, =_vectors
        msr vbar_el1, x2

        // disable execution level stacks, so that we only have 4 vectors
        msr SPSel, #0

        // clear bss
        ldr x2, =__bss_start
        ldr w3, =__bss_size
1:      cbz w3, 2f
        str xzr, [x2], #8
        sub w3, w3, #1
        cbnz w3, 1b

        // jump into main, shouldnt return
        // halt this core too as failsafe
2:      mov sp, x1
        bl kmain
        b  1b

        .align 11
_vectors:
        .align 7
        mov x0, #0
        mrs x1, esr_el1
        mrs x2, elr_el1
        mrs x3, spsr_el1
        mrs x4, far_el1
        mrs x5, sctlr_el1
        mrs x6, tcr_el1
        b   kexception
        .align 7
        mov x0, #1
        mrs x1, esr_el1
        mrs x2, elr_el1
        mrs x3, spsr_el1
        mrs x4, far_el1
        mrs x5, sctlr_el1
        mrs x6, tcr_el1
        b   kexception
        .align 7
        mov x0, #2
        mrs x1, esr_el1
        mrs x2, elr_el1
        mrs x3, spsr_el1
        mrs x4, far_el1
        mrs x5, sctlr_el1
        mrs x6, tcr_el1
        b   kexception
        .align 7
        mov x0, #3
        mrs x1, esr_el1
        mrs x2, elr_el1
        mrs x3, spsr_el1
        mrs x4, far_el1
        mrs x5, sctlr_el1
        mrs x6, tcr_el1
        b   kexception
