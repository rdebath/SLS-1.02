	sr=10000
	kr=1000
	ksmps=10
	nchnls=4
	instr	1
kx	line	p4, p3, p5
ky	line	p6, p3, p7
asig	oscil	10000, 440, 1
a1,a2,a3,a4 pan	asig, kx, ky, 2, 1, 0
	outq	a1,a2,a3,a4
;	outq	asig, asig, asig, asig
	endin
