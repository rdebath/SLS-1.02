	sr = 10000
	kr = 10000
	ksmps = 1
	nchnls = 1

	instr	1, 2
i1=	cpspch (p5)
kamp	linseg	0,p3/4,p4,p3*3/4,0
kind	linseg	p8,p3/2,p9,p3/2,p8
a1	foscili	kamp,i1,p6,p7,kind,1
	out	a1
	endin

