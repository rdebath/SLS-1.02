/ Modified for Coherent 4.0 by Harry C. Pulley, IV

/* _port_in(port) for Coherent 4.0 by Harry C. Pulley, IV 

.text
port_in:
.globl port_in
	push	%ebp
	mov	%esp,%ebp
	push	%edi
	push	%ebx

	mov	8(%ebp),%edx

	sub	%eax,%eax

	inb	(%dx)

	pop	%ebx
	pop	%edi
	pop	%ebp
	ret
