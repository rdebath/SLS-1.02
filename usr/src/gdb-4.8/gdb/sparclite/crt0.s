! C startup code for the Fujitsu SPARClite demo board

	.text
	.align 8

win_ovf_trap:
	sethi %hi(win_ovf), %l3
	jmpl %lo(win_ovf)+%l3, %g0
	mov %wim, %l0
	nop

win_unf_trap:
	sethi %hi(win_unf), %l3
	jmpl %lo(win_unf)+%l3, %g0
	mov %wim, %l0
	nop

	.globl start

start:
! First, copy prom & trap vectors to sram

	set 0x30000000, %l0
	set 0xfff8, %l1

copyloop:
	ldd [%l1], %l2
	std %l2, [%l0 + %l1]
	subcc %l1, 8, %l1
	bge copyloop
	nop

	set 0x30000000, %l0
	mov %l0, %tbr			! Install the new tbr

	set win_ovf_trap, %l1		! Setup window overflow trap
	ldd [%l1], %l2
	std %l2, [%l0 + 5 * 16]
	ld [%l1 + 8], %l2
	st %l2, [%l0 + 5 * 16 + 8]

	set win_unf_trap, %l1		! Setup window underflow trap
	ldd [%l1], %l2
	std %l2, [%l0 + 6 * 16]
	ld [%l1 + 8], %l2
	st %l2, [%l0 + 6 * 16 + 8]

	set _main, %g1
	jmp %g1
	nop
