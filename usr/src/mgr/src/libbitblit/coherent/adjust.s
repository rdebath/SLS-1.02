/* adjust modified for Coherent 4.0 by Harry C. Pulley, IV
/  static DATA inline *adjust(p,graph_mem) DATA *p,*graph_mem;
/  {
/    register DATA *p1 asm("eax");

/*
/*   p=(DATA*)((long)p-(long)graph_mem);
/*   return (char*)((((((unsigned int)p / 90) & 3)<<13) 
/*                  + ((unsigned int)p / 360) * 90 
/*                  + (unsigned int)p % 90)
/*                  + ((unsigned int)graph_mem));
/*
/*   p1 = p - graph_mem;
/*   return (char*) ((((q=((unsigned int)p / 90)) & 3)<<13)
/*                  + ((q >> 2) * 90)
/*                  + ((unsigned int)p % 90)
/*    	            + ((unsigned int)graph_mem));
/*/
 
.text
adjust:
.globl adjust
	push	%ebp
	mov	%esp,%ebp
	push	%edi
	push	%ebx
/ change suggested by Grant Edwards; corrected placement by Vance Petree
	push	%esi

	movl	8(%ebp),%eax
	subl	12(%ebp),%eax

	movl	$90,%esi
	xorl	%edx,%edx
	divl	%esi
	movl	%eax,%esi             / esi is q=p/90 
	andl	$3,%eax               / 
	sall	$13,%eax              /
	addl	%edx,%eax             / add reminder
	sarl	$2,%esi               / q >> 2
	leal	(%esi,%esi,8),%esi    / * 90
	addl	%esi,%eax
	leal	(%esi,%esi,8),%esi
	addl	%esi,%eax
	addl	12(%ebp),%eax

/ change suggested by Grant Edwards; corrected placement by Vance Petree
	pop	%esi
	pop	%ebx
	pop	%edi
	pop	%ebp
	ret
