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

       i35 = int(frac(abs(p10))*100+.5)
       i20 = (i35-int(abs(p10))-.02)*.5 	       ;rand range init
       i5 = int(abs(p10))+i20			       ;rnd center init
       i20 = i20+1
       i6 = i35+1   ;old rnd num init
       i16 = p3/int(p14)    ;atax init
       i19 = p4     ;amp
       kk1   randi  i20,431,frac(p11)
	 ;series zvfmult inits
       i10 = int(abs(p10))-1	 ;loc1 pointer
       it1 = int(frac(abs(p11))*100+.5)
       i11 = (it1=0? i10 : it1-1) ;recyc init
	 ;rthm zvfmult inits
       i12 = int(p20)-1 	;loc2 pointer
       i13 = (p21=0? i12 : p21-1) ;recyc init
       i14 = 0			;atax
       i15 = 0			;total
	 ;zvamph inits
	    if p24=0 igoto  env
       i18 = int(p24)-1    ;pointer init
       i17 = ftlen(int(frac(p24)*100+.5))
       kk2   init    int(p24)/i17
       kk2   phasor   p23/p3,i(kk2)    ;for pos p23 cps
	 ;envlpx init
env:   i36 = (frac(p15)=0?  1 : frac(p15)*10)  ;atten factor of ss
	 ;nf zvfmult inits
       i23 = int(p25)-1
       i24 = (p26=0? i23 : p26-1)  ;recyc


rinit:

	i9 = p10			;constant pch init
	if p11=0 igoto j30
	if p11>0 igoto skp1
rnd: i10 = int(i5+i(kk1))
     it1 = int(abs(p9))
     i10 = (i10<it1? it1 : i10)
     i10 = i10+(i10=i6? 1 : 0)
     i10 = (i10>i35?  it1 : i10)
     i6 =  i10
skp1: i9  table   i10,int(abs(p11))
    i10 = i10+1
	if i10<i35   igoto j30
    i10 = i11

j30: i9 = cpspch(i9)
     it1 = i8*i9
       if p6>=0  igoto j33
     kk7  init	 it1	      ;min interval
     i22 = p7*i9-it1	      ;intrval dif
j33: kk8  init	 i9+p5*it1    ;init real pch+offset
    
      ;nfsig zvfmult

       if p27=0  igoto nt1
     i7  table	 i23,p27
       i23 = i23+1
       i23 = (i23<int(frac(abs(p25))*100+.5)?  i23 : i24)

   ;rthm zvfmult

nt1:  if p22=0	igoto nt2
    ;i16 must be init to p14 before here
skp2: it3  table   i12,p22
    i12 = i12+1
      if i12<int(frac(p20)*100+.5)   igoto to2
    i12 = i13
to2:  if i14>=int(p14)	igoto j35
    ; loop for rthm values
    i15 = i15+it3
    i14 = i14+1
      if i14<int(p14)  igoto skp2
    ; exit from loop and set loc pointer for first attack
    i12 = int(p20)-1
      igoto skp2
j35: i16 = p3*it3/i15	   ;dur value sent to envlpx, timout, linseg

nt2:  if p24=0	igoto nt3  ;zvamph
     i18 = i18+1
      if i18<abs(p23) igoto j40
     i18 = int(p24)
j40: i18 = (p23<0? i18 : i(kk2)*i17)
    i19  table	  i18,frac(p24)*100+.5
    i19 = i19*p4    ;final amp

	;envlop
nt3: k2   linseg  0,p13,.25,i16-p13,1	    ;env follower control
     a5   envlpx  i19,p13,i16,frac(p14)*10,p15,i36,1/p16  ;env table
	  timout  0,i16,j50

       reinit  rinit
       rireturn

j50:	  ;stereo control

    kk5   init	  p17		   ;left prop
    kk6   init	  1-p17 	   ;right prop
       if p19=0   goto	j60
    it1   =	  (p17=1?  1 : int(p17)*.1)
    i37   =	  frac(p17)	   ;min
    i38   =	  it1-i37	   ;dif
    k4	  oscil   i38,p18/p3,frac(p19)*100+.5,int(p19)	;wrong phase
    kk5   =       i37+k4
    kk6   =	  1-kk5
  ;at end of instr, write outs sig*kk5,sig*kk6

j60:	 if p9=0   goto j70
    i1	 =	  int(frac(p9)*100+.5)	      ;nf
    i2	 =	  ftlen(i1)
;print i1,i2
    k5	 phasor	  p8/p3,int(p9)/i2
    k5	 =	  (p8=0?  k2 : k5)
;display k5,.05
    k5	 table	  k5*i2,i1
    kk7  =	  i8+i22*k5
    kk8  =	  i9+p5*kk7

j70: a1   oscili   a5,kk8,i7,0
     a2   oscili   a5,kk8+kk7,i7,.3/i21
     a3   oscili   a5,kk8+kk7+kk7,i7,.6/i21
     a4   =	   a1+a2+a3
	 if i21<5  goto j80
     a1   oscili   a5,kk8+3*kk7,i7,.9/i21
     a2   oscili   a5,kk8+4*kk7,i7,1.2/i21
     a4   =	   a4+a1+a2
         if i21<7   goto j80
     a1   oscili   a5,kk8+5*kk7,i7,1.5/i21
     a2   oscili   a5,kk8+6*kk7,i7,1.8/i21
     a4   =	   a4+a1+a2

j80: ga16 = ga16+a4
	  outs	a4*kk5,a4*kk6
	  endin

