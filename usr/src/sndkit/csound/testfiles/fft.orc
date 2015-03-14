	sr = 25600
	kr = 256
	ksmps = 100

	instr	1
asig	buzz	10000, 400, 10, 1
;	dispdft	asig, .01, 4, 12, 15, p4, 0
	dispfft	asig, .01, 128, p4, 0
	out	asig
	endin
