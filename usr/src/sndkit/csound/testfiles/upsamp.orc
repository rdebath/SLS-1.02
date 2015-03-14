	sr=10000
	kr=1000
	ksmps=10
	instr 	1
ksig	oscil	1000,440,1
;asig	=	ksig
asig	interp	ksig
	out	asig
;ksig	=	asig
	endin
