        /*
        a vague attempt at adhering to the aarch64 procedure call abi
        since at some point I'll likely want to use compiled code from somewhere.

        paramater/result registers don't get renamed

        x0-x8,  params/results - volatile over procedure calls
        t1-t7,  temp, caller saved
        c1-c10, temp, callee saved

        x28 is reserved for psuedops
        */

        t1   .req x9
        t2   .req x10
        t3   .req x11
        t4   .req x12
        t5   .req x13
        t6   .req x14
        t7   .req x15

        tw1  .req w9
        tw2  .req w10
        tw3  .req w11
        tw4  .req w12
        tw5  .req w13
        tw6  .req w14
        tw7  .req w15

        c1   .req x19
        c2   .req x20
        c3   .req x21
        c4   .req x22
        c5   .req x23
        c6   .req x24
        c7   .req x25
        c8   .req x26
        c9   .req x27

        cw1  .req w19
        cw2  .req w20
        cw3  .req w21
        cw4  .req w22
        cw5  .req w23
        cw6  .req w24
        cw7  .req w25
        cw8  .req w26
        cw9  .req w27

        /* delay for \count cycles */
        .macro delay cycles
        mov  x28, \cycles
1:      subs x28, x28, #1
        b.ne 1b
        .endm

        /* enter a subroutine
           save off \count (rounded up to even) callee-saved registers
        */
        .macro subr_enter count
        regcount = (\count + (\count & 1))
        regsave  = ((regcount + 2) / 2) * 0x10

        .if ((\count > 10) || (\count < 0))
        .fail "subr takes 0-10 callee-saved registers"
        .endif
        stp  fp, lr, [sp, #-regsave]!
        .if  regcount > 0
        stp  c1, c2, [sp, #0x10]
        .endif
        .if  regcount > 2
        stp  c3, c4, [sp, #0x20]
        .endif
        .if  regcount > 4
        stp  c4, c5, [sp, #0x30]
        .endif
        .if  regcount > 6
        stp  c6, c7, [sp, #0x40]
        .endif
        .if  regcount > 8
        stp  c8, c9, [sp, #0x50]
        .endif
        mov  fp, sp
        .endm

        /* return from a subroutine
           restore \count (rounded up to even) callee-saved registers
        */
        .macro subr_ret count
        regcount = (\count + (\count & 1))
        regsave  = ((regcount + 2) / 2) * 0x10

        .if ((\count > 10) || (\count < 0))
        .fail "subr takes 0-10 callee-saved registers"
        .endif
        .if  regcount > 8
        ldp  c8, c9, [sp, #0x50]
        .endif
        .if  regcount > 6
        ldp  c6, c7, [sp, #0x40]
        .endif
        .if  regcount > 4
        ldp  c4, c5, [sp, #0x30]
        .endif
        .if  regcount > 2
        ldp  c3, c4, [sp, #0x20]
        .endif
        .if  regcount > 0
        ldp  c1, c2, [sp, #0x10]
        .endif
        ldp  fp, lr, [sp], #regsave
        ret
        .endm
