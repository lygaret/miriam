        .org 0x10000
        .section .text.boot
        .global _start

        rpc      .req r1
        rop      .req r2
        rsptop   .req r3
        roptable .req r4

        rr1      .req r5
        rr2      .req r6

        /* push the register onto the stack, caching the top */
        .macro rpush reg
        push {rsptop}
        mov rsptop, \reg
        .endm

        /* push two registers onto the stack, caching the top */
        .macro rpush2 ra, rb
        push {rsptop}
        push {\ra}
        mov rsptop, \rb
        .endm

        .macro rpop ra
        mov \ra, {rsptop}
        pop {rsptop}
        .endm

        /* jump to the next instruction in the program */
        .macro rnext
        ldr rop, [rpc], #4
        ldr pc, [roptable, rop, lsr #24]
        .endm

_start:
        /* todo: should it be possible to add opcodes? */
        adr roptable, optable

        /* todo: memory management - stack, gc, etc. */
        ldr sp, =#0x00F00004

        /* todo: initial loader - what does raspi pass us? */
        /* todo: initial runtime env - load modules, etc. */
        adr rpc, prog
        rnext

error:
        /* todo: runtime env allows log/debug/uart/pause, etc */
        bkpt
        b error

optable:
        /*
        low byte of opcode is index into this table
        index should be word-aligned
        */
        .word op_halt
        /* todo: system & runtime services */
        /*
        .word op_push_random
        .word op_push_tick
        */
        /* todo: debug & statistics */
        /*
        .word op_tick_counter
        .word op_push_counters
        */
        .word op_push_immediate
        /* todo: constant pool */
        /*
        .word op_push_constant
        */
        /* todo: scopes */
        /*
        .word op_push_global
        .word op_push_upref
        .word op_push_localref
        .word op_enter_scope
        .word op_exit_scope
        */
        /* todo: conditions & equality */
        /*
        .word op_pred_eq
        .word op_pred_eqv
        .word op_pred_equal
        .word op_pred_immediate
        .word op_pred_pointer
        .word op_pred_false
        .word op_pred_void
        */
        /* todo: jumps
        condition register & jump+cond?
        or boolean on stack & jump+pop?
        or many jumps+condition?
        /*
        .word op_jump
        .word op_jump_when
        .word op_jump_unless
        */
        /* todo: machine numbers (first?) explicitly sized ints with bitwise ops */
        .word op_push_fixnum
        .word op_fixnum_add
        .word op_fixnum_sub
        /*
        .word op_fixnum_mul
        .word op_fixnum_divrem
        .word op_fixnum_comp
        */
        /* todo: immediate encoded instructions; no need to push/pop for +1, -4 etc. */
        /*
        .word op_fixnum_immadd
        .word op_fixnum_immsub
        */
        /* todo: fixnum promotion to bignum */
        /* todo: hash tables */
        /* todo: vectors - dynamically sized object arrays with O(1) indexed access */
        /*
        .word op_vector_alloc
        .word op_vector_ref
        .word op_vector_set
        */
        /* todo: buffers - (optionally) statically sized byte-buffers */
        /*
        .word op_buffer_alloc
        .word op_buffer_ref
        .word op_buffer_set
        */
        /* todo: channels - synchronous rendezvous (optionally) buffered data exchange */
        /* todo: processes - threads with message queues */

op_halt:
        b error

op_push_immediate:
        lsl rr1, rop, #8

        rpush rop
        rnext

op_push_fixnum:
        lsl rr1, rop, #8
        lsr rr1, rr1, #6
        add  rr1, rr1, #1
        rpush rr1
        rnext

op_fixnum_add:
        pop {rr1}

op_fixnum_add_:
	add rsptop, rsptop, rr1
        ubfx rr1, rsptop, #0, #2
        teq rr1, #2
	bne error
	sub rsptop, #1
        rnext

op_fixnum_immadd:
        lsl rr1, rop, #8
        lsr rr1, rr1, #6
        add rr1, rr1, #1
        b   op_fixnum_add_

op_fixnum_sub:
        pop {rr1}

op_fixnum_sub_:
	sub rsptop, rsptop, rr1
        ubfx rr1, rsptop, #0, #2
        teq rr1, #2
	bne error
	sub rsptop, #1
        rnext

op_fixnum_immsub:
        lsl rr1, rop, #8
        lsr rr1, rr1, #6
        add rr1, rr1, #1
        b   op_fixnum_sub_

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
        .word 0x0800FFFF
        .word 0x18000000
        .word 0x18000000
        .word 0x00000000

        /*
        push 0
        push 1
        rotate
        push 0

        fib n
        res = 1
        a = 0
        b = 1


        while (n > 1) {
        res = a + b
        a = b
        b = res
        n--
        }
        return result
        */
