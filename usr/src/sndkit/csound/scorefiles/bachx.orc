sr = 8192
kr = 256
ksmps = 32
nchnls = 1

	instr 1,2,3,4
icps	=	cpsoct(p4)
kenv	envlpx	p5, .02, p3, .1, 2, .3, .01
a1	oscil	kenv, icps, 3
	out	a1
	endin

	instr 5,6,7
icps	=	cpspch(p4)
kenv	envlpx	p5, .02, p3, .1, 2, .3, .01
a1	oscil	kenv, icps, 3
	out	a1
	endin

	instr 8
kx,ky	xyin	.1, 100, 400, 0, 1, 250
	tempo	kx, 250
	endin
