			sr = 10000
			kr = 1000
			ksmps = 10

			instr	1
		ktime	line	0, p3, p3	; i.e. no time warping
krmsr,krmso,kerr,kcps	lpread	ktime, 1	; you'll need npoles, frmrate
;			display	krmsr, .2
;			display	kerr, .2
			if	kerr > .3 kgoto unvoc
		asig	buzz	krmsr, 220, int(sr/880), 1
			kgoto	res
	unvoc:	asig	rand	krmsr
	res:	aout	lpreson	asig
			out	aout
			endin
