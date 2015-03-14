	sr=10000
	kr=1000
	ksmps=10

	instr	1
atime	line	0,p3,p3*sr
audio	table	atime,2
	out	audio
	endin

	instr	2
kcps	line	1,p3,50
aphs1	phasor	kcps,0
aphs2	phasor	kcps,.5
asin1	table	aphs1,1,1
asin2	table	aphs2,1,1
atime	line	0,p3,p3*sr
atran	line	0,p3,.5*sr
ashift	=	atran/kcps
audio1	table	atime+aphs1*ashift,2
audio2	table	atime+aphs2*ashift,2
	out	audio1*asin1 + audio2*asin2
	endin
