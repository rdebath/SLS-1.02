	sr = 10000
	kr = 1000
	ksmps = 10
	instr	1
	if	p3 > 2 igoto nxt
k1	=	p3
nxt:
;kcps	=	(p3 > 2) ? k1/(1+2) : ((p3 > 1) ? 2*k1 : p3 )
kcps	=	(p3 > 2) ? k1+1 : p3+1
	endin
