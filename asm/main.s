        .org 0x10000
        .section .text.boot
        .global _start

        rpc      .req r1
        rop      .req r2
        rstop    .req r3

        rr1      .req r5
        rr2      .req r6

_start:
        movt sp, #0x00F0
        adr rpc, prog

next:
        /*
        opcode must be a multiple of four, to allow
        for 24bit payload, 64 unique opcodes
        t t t t t t 0 0 d d d d d d d d d d d d d d d d

        load the opcode into the op register from pc, and incr
        then, load from the table below; offset is the opcode type
        */
        ldr rop, [rpc], #4
        ldr pc, [pc, rop, lsr #24]

        .word 0 /* align pc */
        .word op_halt
        .word op_push_immediate
        .word op_push_fixnum
        .word op_fixnum_add
        .word op_fixnum_sub
        .word op_fixnum_incr
        .word op_fixnum_decr

error:
        bkpt
        b error

op_halt:
        b error

op_push_immediate:
        push {rop}
        b next

op_push_fixnum:
        lsl rr1, rop, #8
        lsr rr1, rr1, #6
        add  rr1, rr1, #1
        push {rr1}
        b next

op_fixnum_add:
        pop {rr1, rr2}
	add rr1, rr1, rr2
        ubfx rr2, rr1, #0, #2
        teq rr2, #2
	bne error
	sub rr1, #1
	push {rr1}
        b next

op_fixnum_sub:
        pop {rr1, rr2}
        eor rr1, rr1, #3
	sub rr1, rr1, rr2
        and rr1, rr1, #-3
        ubfx rr2, rr1, #0, #3
        teq rr2, #01
	bne error
	push {rr1}
        b next

op_fixnum_incr:
        pop {rr1}
	add rr1, rr1, #4
	push {rr1}
        b next

op_fixnum_decr:
        pop {rr1}
	sub rr1, rr1, #4
	push {rr1}
        b next

prog:
        /*
        10 op_halt
        04 op_push_immediate
        08 op_push_fixnum
        0c op_fixnum_add
        10 op_fixnum_sub
        14 op_fixnum_incr
        18 op_fixnum_decr
        */

        .word 0x00000001

        .word 0x0800FFFF
        .word 0x18000000
        .word 0x18000000
        .word 0x00000000
