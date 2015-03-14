	sr = 25600
	kr = 256
	ksmps = 100

	instr	1
abuzz	buzz	10000, 200, 64, 1
	dispfft	abuzz, .01, 512, 0, 0
adiff	diff	abuzz
	dispfft	adiff, .01, 512, 0, 0
	out	adiff
	endin
