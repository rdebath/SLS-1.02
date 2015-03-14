	sr = 8000
	kr = 50
	ksmps = 160
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
kx,ky	xyin	.02, 0, 1, 0, 10000, .5, 0
kdy	diff	ky
kd2y	diff	kdy
ktemp	tempest	kd2y, .02, .1, 3, 2, 750, .005, 0, 256, 4, .1, .995
	display ky, 1
	display kdy,  1
	display kd2y,  1
	display ktemp, 1
	tempo	ktemp, 240
	endin
