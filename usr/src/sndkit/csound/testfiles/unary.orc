	sr = 10000
	kr = 1000
	ksmps = 10
	instr	1
i1	=	1
i1	=	-i1
i2	=	-i1
i2	=	-i1
k2	line	0, p3, exp(-i1)
	display k2, p3
	endin
