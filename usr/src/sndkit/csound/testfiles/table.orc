sr = 8000    			;audio sampling rate is 8k Hz
kr = 500			;control rate is 512 Hz
ksmps = 16			;number of samples in a control period (cr/kr)
nchnls = 1			;number of channels of audio output


	instr	1	
a1	line	0, p3, p3 * sr  ;provide indexes to table over proper 
				;time interval
a2	table 	a1, 1          ;output contents of table
	out 	a2
	endin


