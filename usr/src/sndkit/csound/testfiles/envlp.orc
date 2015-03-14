	sr = 10000
	kr = 1000
	ksmps = 10

	instr 1
ksig	envlpx	1000, .1, 1, .1, 1, .5, .05, p4
asig	oscil	ksig, 440, 2
	display	ksig, 1
	out	asig
	endin
