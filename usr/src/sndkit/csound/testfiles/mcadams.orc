	sr = 20000
	kr = 2000
	ksmps = 10

	instr	1
kamp	randh	p6, p7
kfrq	randh	p8, p9
af0	oscil	p4+kamp, p5+kfrq, 1
af1	reson	af0, p10, p11, 1
af2	reson	af1, p12, p13, 1
af3	reson	af2, p14, p15, 1
af4	reson	af3, p16, p17, 1
arms	gain	af4, p18
aout	linen	arms, .2, p3, .4
	out	aout
	endin
