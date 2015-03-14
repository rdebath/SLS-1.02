	sr = 20000
	kr = 1000
	ksmps = 20

	instr	1
asource buzz	1, 440, 20, 1
atime	linseg	1, p3/2, .01, p3/2, 1	;trace a distance in secs
ampfac	=	1/atime/atime		;  and calc an amp factor
adump	delayr	1
amove	deltapi	atime			;move sound source past
	delayw	asource			;  the listener
	out	amove * ampfac
	display	atime, 1
	display	ampfac, 1
	endin
