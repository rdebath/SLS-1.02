	sr = 10000
	kr = 10000
	ksmps = 1

	instr	1
k1	init	0
a1	= 	(k1 < p5 ? p4 : 0)
a2	delay	a1, p7
	out	a1 + a2
k1	=	(k1 < p6 ? k1 + .0001 : 0)
	endin
