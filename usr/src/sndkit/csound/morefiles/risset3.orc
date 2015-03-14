sr=20000
kr=1000
ksmps=20
nchnls=1

;			Risset's Additive Bell Instrument                 ;


instr 6
   i1  = p5		    ; p3 = duration
   i2  = p5*0.67	    ; p4 = freq in Hz
   i3  = p5*1.35	    ; p5 = amplitude
   i4  = p5*1.80
   i5  = p5*2.67
   i6  = p5*1.67
   i7  = p5*1.46
   i8  = p5*1.33	    ; Peak amps of the partials
   i9  = p5*1.33	    ; are a function of the amp
   i10 = p5*0.75	    ; of the lowest partial
   i11 = p5*1.33

   i12 = p3
   i13 = p3*.9
   i14 = p3*.65
   i15 = p3*.55 	    ; Durations of the partials are a function of 
   i16 = p3*.325	    ; the duration of the lowest partial
   i17 = p3*.35
   i18 = p3*.25
   i19 = p3*.2
   i20 = p3*.15
   i21 = p3*.1
   i22 = p3*.075
  
   i23 = p4*.56
   i24 = (p4*.56)+1
   i25 = p4*.92 	   ; Frequencies of the partials are a function of 
   i26 = (p4*.92)+1.7	   ; the frequency of the fundamental
   i27 = p4*1.19
   i28 = p4*1.7
   i29 = p4*2
   i30 = p4*2.74
   i31 = p4*3
   i32 = p4*3.75
   i33 = p4*4.07

   k1 oscil1 0,i1,i12,2
   a1 oscili k1,i23,1	; The instrumentconsists of pairs of oscil1/oscili
   k2 oscil1 0,i2,i13,2   ; where oscil1 provides the envelope of the partial
   a2 oscili k1,i24,1	     ; and oscili the partial itself

   k3 oscil1 0,i3,i14,2
   a3 oscili k1,i25,1

   k4 oscil1 0,i4,i15,2
   a4 oscili k1,i26,1

   k5 oscil1 0,i5,i16,2
   a5 oscili k1,i27,1

   k6 oscil1 0,i6,i17,2
   a6 oscili k1,i28,1

   k7 oscil1 0,i7,i18,2
   a7 oscili k1,i29,1

   k8 oscil1 0,i8,i19,2
   a8 oscili k1,i30,1

   k9 oscil1 0,i9,i20,2
   a9 oscili k1,i31,1

   k10 oscil1 0,i10,i21,2
   a10 oscili k1,i32,1

   k11 oscil1 0,i11,i22,2
   a11 oscili k1,i33,1

   out a1+a2+a3+a4+a5+a6+a7+a8+a9+a10+a11
endin
