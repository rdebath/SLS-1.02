; Jim Dashow:  orchestra for beginning of In Winter Shine

sr=20000
kr=1000
ksmps=20
nchnls=2

ga15	init	0
ga16	init	0

			 instr	16    ;oscilm...3, 5, or 7 oscils

	  i7 = int(frac(p12)*100+.5)	  ;nf sig init
	  i8 = (p6<0? (cpspch(abs(p6))/1.021975)-1 : p6)   ;addintrval min
	  p7 = (p6<0? (cpspch(p7)/1.021975)-1 : p7)  ;addint max
	  kk7  init   i8		  ;addhz init
	  i22 = p7-i8			  ;addhz dif
	  p28 = (p28=0?  2/int(p12) : p28/int(p12))  ;amp adjust
	  p4 = p4*p28
	  i21 = int(p12+.5)		  ;nosc


     ;	this code puts together zvfmult pch table, rand control and
     ;	zvfmult rthm table, output of this is aimed at timout,envlpx,
     ;	and linseg; timout sets reinit. i5 thru i20, i35-i38,
     ;	kk1 thru kk6 are reserved for these units.  kk3, kk4 now unused.
     ;	kk5=stereo left, kk6=stereo right, i9=zvfmult output, a5 is envlpx
     ;	output;  k1 thru k4 reserved.


;j60:	 if p9=0   goto j70
    i1	 =	  int(frac(p9)*100+.5)	      ;nf
    i2	 =	  ftlen(i1)
;print i1,i2
    k5	 phasor	  p8/p3,int(p9)/i2
;    k5	 =	  (p8=0?  k2 : k5)
;display k5,.05
    k5	 table	  k5*i2,i1
	  endin

