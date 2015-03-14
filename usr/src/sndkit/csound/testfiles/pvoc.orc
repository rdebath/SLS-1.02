			sr =   8000
			kr =     80
			ksmps = 100

			instr	1
	ioct	= 	octpch(p5)
	ktime	line	0, p3, 1.1	; changing speed
	kptvh   expon   .1, p3, .1+p4
	aout	pvoc	ktime, cpsoct(ioct+.2-.3*kptvh)/440, 0, 2
	;	dispdft aout, .2, 4, 12, 16, 0, 0
	;        dispfft aout, .2, 2048, 1, 0 
		out	aout
		endin
