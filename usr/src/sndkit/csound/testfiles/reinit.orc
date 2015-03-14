		sr	= 20000
		kr	= 1000
		ksmps	= 20
		nchnls 	= 1

		instr	1
reset:		timout	0, p3/10, contin	;after p3/10 seconds,
		reinit	reset			; reinit both timout 
contin:	k1	expon	440, p3/10, 880         ; and expon 
		rireturn			; then resume perf
		endin
