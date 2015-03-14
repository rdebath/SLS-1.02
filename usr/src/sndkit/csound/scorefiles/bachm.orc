	sr = 8000
	kr = 100
	ksmps = 80
	nchnls = 1

	instr 1,2,3,4
icps	=	cpsoct(p4)
kenv	envlpx	p5, .02, p3, .1, 2, .3, .01
a1	oscil	kenv, icps, 3
	out	a1
	endin

	instr 5,6,7
icps	=	cpspch(p4)
kenv	envlpx	p5, .02, p3, .1, 2, .3, .01
a1	oscil	kenv, icps, 3
	out	a1
	endin

	instr 8
;kx,ky	xyin	.1, 100, 400, 0, 1, 250
asig	in
krms	rms	asig, 5
	if krms > 2  kgoto db
	krms = 10
db:	
krmsdb	= dbamp(krms)
;dsig	octdown	asig, 5, 178, 0
;wsig	noctdft	dsig, .05, 12, 33, 0, 1, 1
;wsig2	specscal wsig, 5, 6
;	specdisp wsig2, .1, 0
;ksum	specsum	wsig2, 1
ktemp	tempest	krmsdb, .01, .1, 3, 2, 30, .005, 0, 256, 4, .1, .995
;	display krms,  1
;	display krmsdb,1
;	display ktemp, .5
	tempo	ktemp, 240
	endin
