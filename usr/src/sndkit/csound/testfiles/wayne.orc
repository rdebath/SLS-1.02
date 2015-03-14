;		The idea is to use 
;			p7=0 : an isolated note.
;			p7=1 : the beginning of a series of tied notes.
;			p7=2 :	the middle of tied note series
;			p7>2 : end of a tied note series.

	sr = 25000
	kr = 1000
	ksmps = 25
	nchnls = 1

	instr	1
	if	p7 != 0 goto  tistart
			; no tied note, normal atk, dcy.
		isst	=	p3  - .3 ; chop with kgate 
		kgate	linseg  0.,.1,10000,isst,10000,.1,0.,.1,0.
			goto	sing
tistart: if	p7 != 1 goto  tikeep
			; hold the final amplitudes, no init
		ihold
		isst	=	p3  - .1 ; chop with kgate 
		kgate	linseg  0,.1,10000,isst,10000
			goto	sing
tikeep: if	p7 != 2 goto tistop
			; middle of a tied (held) series
		ihold
		kgate	=	10000
			goto	sing
tistop: isst	=	p3  - .2 ; chop with kgate 
	; end a tied series
		kgate	linseg  10000,isst,10000,.1,0,.1,0
			goto	sing
sing: kf0	expseg p4,p3*.25,p4,p3*.5,p5,p3*.25,p5
gasig	oscili	kgate,kf0,1,-p7               ; p7 for init skip
	out	gasig
	endin

	instr	2
	display	gasig,p3/10
	endin

;The idea is to change the amplitude envelope and keep the
;oscili running without a glitch when p7 is 1 or 2.



