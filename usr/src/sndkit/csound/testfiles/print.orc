	sr=10000
	kr=1000
	ksmps = 10
	instr 1
i1 = p1
i2 = p2
	print	i1, i2, p2, p3, sr, kr, sr/kr
a1	linseg	1,2,3,4,5,6,7
	out	a1
	endin
