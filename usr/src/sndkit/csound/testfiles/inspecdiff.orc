	sr = 8000
	kr =  500
	ksmps = 16

	instr	1
asig	in
dsig	octdown  asig, 6, 178, 0
wsig1	noctdft  dsig, .02, 12, 33, 0, 1, 1
wsig2	specscal wsig1, 3, 4
wsig3	specdiff wsig2, 0
;wsig4	specaddm wsig2, wsig3, 10
wsig5	persist	 wsig3, 5
	specdisp wsig1, .1, 0
	specdisp wsig2, .1, 0
	specdisp wsig3, .1, 0
;	specdisp wsig4, .1, 0
	specdisp wsig5, .1, 0
ksum5	specsum	 wsig5, 1
ksum5	=	 ksum5 * .1
ktempo	tempest  ksum5, .01, .1, 3, 2, 30, .005, .2, 120, 2, .1, 1
;	display	 ktempo,1
	out	 asig
	endin




	instr	2
;k1	oscil	.1, .1, 3
;ksum2	oscil    1000, 2+k1, 1
;ksum2	specsum	 wsig2, 1
;	display  kout, 2
;	dispfft	 asig, .2, 2048, 1, 0
	endin