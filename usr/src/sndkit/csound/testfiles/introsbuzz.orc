sr = 8000    			;audio sampling rate is 8k Hz
kr = 500			;control rate is 512 Hz
ksmps = 16			;number of samples in a control period (cr/kr)
nchnls = 1			;number of channels of audio output

	instr	1		
kctrl 	line 	10000, p3, 0	
asig	pluck	20000, p5, p5, 1, 4, .2, 5	;audio oscillator
;af1	reson	asig, 110, 80
;af2	reson	asig, 220, 100
;af3	reson	asig, 440, 80
;aout	balance 0.6*af1+af2+0.6*af3+0.4*asig, asig
;	out 	aout
	out 	asig
	endin

	instr 	2
kctrl 	line 	10000, p3, 0	
asig	pluck	20000, p5, p5, 1, 3, .5	;audio oscillator
;af1	reson	asig, 110, 80
;af2	reson	asig, 220, 100
;af3	reson	asig, 440, 80
;aout	balance 0.6*af1+af2+0.6*af3+0.4*asig, asig
;	out 	aout
	out 	asig
	endin

	instr	3		
kctrl 	line 	10000, p3, 0	
asig	pluck	20000, p5, p5, 1, 3, .5	;audio oscillator
;af1	reson	asig, 110, 80
;af2	reson	asig, 220, 100
;af3	reson	asig, 440, 80
;aout	balance 0.6*af1+af2+0.6*af3+0.4*asig, asig
;	out 	aout
	out 	asig
	endin

; oscil  volume, pitch, function table (f's in score)
; line (amplitude modifier) start_value, over_time, end_value
