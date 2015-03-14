/* _port_out(value, port) for Coherent 4.0 by Harry C. Pulley, IV 

.text
port_out:
.globl port_out
	push	%ebp
	mov	%esp,%ebp
	push	%edi
	push	%ebx

	mov	8(%ebp),%eax
	mov	12(%ebp),%edx

	outb	(%dx)

	pop	%ebx
	pop	%edi
	pop	%ebp
	ret
