	sr = 8192
	kr = 512
	ksmps = 16

	instr	1
i1	=	cpspch(p5)
k1	expon	i1, p3, i1*2
asig	oscil	10000, k1, 1
	octdown asig, 6, 100, .1
	noctdft  .01, 24, 16, 0, 0, 2, 1
	specdisp .1, 0
;	dispfft	asig, .2, 2048, 1, 0
	out	asig
	endin
