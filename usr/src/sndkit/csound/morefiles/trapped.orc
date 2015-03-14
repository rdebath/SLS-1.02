
;---------------------------------------------

; TRAPPED IN CONVERT - Richard Boulanger

; written July 1979 in music11
; M.I.T. Experimental Music Studio

; revised June 1986 in Csound
; M.I.T. Media Lab

; revised  May 1989 in PCsound
; for the M.T.U. Digisound 16

;----------------------------------

   sr=16000
   kr=1000
   ksmps=16
   nchnls=2

   ga1 init 0
   ga2 init 0

instr 1
   i1=cpspch(p5)			 ;p6=amp
   a19 init 0				 ;p7=vib rate
   a19 expseg 1,p8,1,p3-p8,p9		 ;p8=del time (default < 1)
   k1 line 0,p3,5			 ;p9=freq drop
   k2 oscili k1,p7,1
   k3 linseg 0,p3*.7,p6,p3*.3,0
   a1 oscili k3,(i1+k2)*a19,1
   k4 linseg 0,p3*.6,6,p3*.4,0
   k5 oscili k4,p7*.9,1,1.4
   k6 linseg 0,p3*.9,p6,p3*.1,0
   a3 oscili k6,((i1+.009)+k5)*a19,9,.2
   k7 line 9,p3*.7,1
   k8 oscili k7,p7*1.2,1,.7
   k9 linen p6,p3/2,p3,p3/3
   a5 oscili k9,((i1+.007)+k8)*a19,10,.3
   k10 expon 1,p3*.99,3.1
   k11 oscili k10,p7*.97,1,.6
   k12 expseg .001,p3*.8,p6,p3*.2,.001
   a7 oscili k12,((i1+.005)+k11)*a19,11,.5
   k13 expseg 1,p3*.4,3,p3*.6,.02
   k14 oscili k13,p7*.99,1,.4
   k15 expseg .001,p3*.5,p6,p3*.1,p6*.6,p3*.2,p6*.97,p3*.2,.001
   a9 oscili k15,((i1+.003)+k14)*a19,12,.8
   k16 expon 4,p3*.91,1
   k17 oscili k16,p7*1.4,1,.2
   k18 expseg .001,p3*.6,p6,p3*.2,p6*.8,p3*.1,p6*.98,p3*.1,.001
   a11 oscili k18,((i1+.001)+k17)*a19,13,1.3
   outs a1+a3+a5,a7+a9+a11
endin

instr 2 				 ;p9=number of harmonics
   i1=cpspch(p5)			 ;p10=swp rt
   k1 randi 1,30			 ;p8=lfo frq
   k2 line p9,p3/p10,1			 ;p7=reverb attenuation
   k3 linseg 0,p3/2,1,p3/2,0		 ;p6=amp
   k4 expseg .001,p3*.01,p6,p3*.99,.001
   k5 linseg .005,p3*.71,.015,p3*.27,.01
   k6 oscili k3,p8,1,.2
   k7=k6+2
   a1 gbuzz k4,i1+k5,k7,k2,k1,15
   ga1=ga1+(p7*a1)
   outs a1,a1
endin

instr 3
   i1=cpspch(p5)			 ;p7=rvb attn
   k3 expseg 1,p3/2,30,p3/2,2		 ;p8=rand frq
   k4 expseg 10,p3*.7,p8,p3*.3,6	 ;p6=amp
   k8 linen p6,p3/3,p3,p3/3
   k13 line 0,p3*.92,-1
   k14 randh k3,k4,.5
   a1 oscili k8,i1+(p5*.05)+k14+k13,1,.1
   k1 expseg 1,p3*.8,6,p3*.2,1
   k6 linseg .4,p3*.9,p8*.96,p3*.1,0
   k7 linseg 8,p3*.2,10,p3*.76,2
   k10 expseg .001,p3*.4,p6*.99,p3*.6,.0001
   k15 randh k6,k7
   a2 buzz k10,i1+(p5*.009)+k15+k13,k1,1,.2
   k9 linen p6,p3/4,p3,p3/1.8
   k16 randh k4*1.4,k7*2.1,.2
   a3 oscili k9,i1+(p5*.1)+k16+k13,16,.3
   ga1=ga1+(p7*(a1+a2+a3))
   outs a3+a1,a2+a3
endin

instr 4 				 ;p10=rvb attn
   k1 expon p7,p3,p8			 ;p9=bdwth
   a1 rand  10000			 ;p7=strt frqval of fltr swp
   a2 reson a1,k1,k1/p9,1		 ;p8=end frqval of filter swp
   k2 oscili .6,11.3,1,.1		 ;p6=amp
   k3 envlpx p6,p3*.1,p3*.6,p3*.4,18,.5,.01,-.8
   a3 oscili k3,cpspch(p5)+k2,15
   ga1=ga1+(p10*a3)
   outs (a2*.8)+a3,(a2*.6)+(.7*a3)
endin

instr 5 				 ;p4=cps of pan
   ga1 init 0
   k1 oscili .5,p4,1
   k2 = .5+k1
   k3 = 1-k2
   a1 reverb ga1,2.1
   outs k2*a1,(k3*a1)*(-1)
   ga1=0
endin

instr 6 				 ;p9=car freq
   i1 = cpspch(p5)			 ;p10=mod freq
   k1 line p9,p3*1.01,1 		 ;p11=modindex
   k2 line 1,p3,p10			 ;p12=rand frq
   k4 expon 2,p3,p12			 ;p6=amp
   k5 line 0,p3*.8,8			 ;p7=reverb atten
   k3 envlpx p6,.1,p3,.5,17,2,.5,.05	 ;p8=balance
   k7 randh p11,k4
   k6 oscili k4,k5,1,.3
   k8 expseg p6*.78,p3*.48,p6*1.04,p3*.52,p6*.80
   k9 line int(p8)/4,p3,frac(p8)/.4
   a1 foscili k3,i1+k6,k1,k2,k7,1
   a2 oscili k8,i1*1.001,1
   k10 linen 1,.01,p3,.01
   ga1=ga1+(p7*(a1+a2))
   outs k10*((1-k9)*(a1+a2)),k10*(k9*(a1+a2))
endin

instr 7 				 ;p9=amp
   k1 phasor p4 			 ;p8=rvb attn
   k2 table k1*8,19			 ;p7=bndwth
   a1 rand  10000			 ;p6=swp frq:end val
   k3 expon p5,p3,p6			 ;p5=swp frq:strt val
   a2 reson a1,k3*k2,k3/p7,1
   k5 linen p9,.01,p3,.05
   a3=k5*a2
   ga1=ga1+(p8*a2)
   outs a3,a3
endin

instr 8
   ifunc=512
   i1=cpspch(p5)			 ;p4=amp
   a1 oscili 1,i1,p10			 ;p6=begin phase pt
   k1 linseg p6,p3*.5,p7,p3*.5,p6	 ;p7=end phase pt
   a3 oscili p8,i1+k1,p9		 ;p8=ctrl osc amp (.1-1)
   a4 phasor i1 			 ;p9=ctrl osc func
   a5 table (a4+a3)*ifunc,p10		 ;p10=main osc func (f2 or f3)
   k2 linen p4,p3*.4,p3,p3*.5		 ;p10=function length (must be 512!)
   a6 = k2*((a1+a5)*.2) 		 ;p11=reverb attenuation
   ga1=ga1+(p11*a6)			 ;p5=frq
   outs a6,a6
endin

instr 9 				 ;p7=rvb atten
   i1=cpspch(p5)			 ;p8=rand amp
   k1 linen p6,p3*.1,p3,p3*.8		 ;p9=rand frq
   k2 randh p8,p9,.1			 ;p10=1 for space
   k3 randh p8*.98,p9*.91,.2		 ;p6=amp
   k4 randh p8*1.2,p9*.96,.3		 ;p4=delay atten
   k5 randh p8*.9,p9*1.3
   a1 oscili k1,i1+k2,1,.2
   a3 oscili k1*.91,(i1+.004)+k3,2,.3
   a5 oscili k1*.85,(i1+.006)+k4,3,.5
   a7 oscili k1*.95,(i1+.009)+k5,4,.8
   a8 = a1+a3+a5+a7
   ga1=ga1+(p7*a8)
   ga2=ga2+(p4*a8)
   outs a1+a5,a3+a7
endin

instr 10
   a1 rand  10000			 ;p4=amp
   a100 oscil 1000,1000,1		 ;p5=begin sweep freq
   k1 expon p5,p3,p6			 ;p6=end sweep freq
   k2 line p8,p3,p8*.93 		 ;p7=bandwidth
   k3 phasor k2 			 ;p8=cps of rand1
   k4 tablei k3*16,20			 ;p9=cps of rand2
   a2 reson a1,k1,20+(k4*k1/p7),1	 ;p10=rvb attenuation
   a3 balance a2,a100
   k5 line p6*.9,p3*.8,p5*1.4
   k6 expon p9*.97,p3,p9
   k7 phasor k6
   k8 tablei k7*16,21
   a4 reson a1,k5,30+(k8*k5/p7*.9),1
   a5 balance a4,a100
   k9 randh 1,k2
   k10 randh 1,k6
   ga1=ga1+(p10*a3)
   k11 linen p4,.25,p3,.5
   outs k11*(.7*(k9*a3))+.3*(k9*a5),k11*(k10*a5)+.3*(k10*a3)
endin

instr 11
   ga2 init 0
   a1 delay ga2,.05
   outs a1,a1
   ga2=0
endin

instr 12
   i1 = cpspch(p5)			 ;p6=amp
   k1 expseg 1,p3/2,40,p3/2,2
   k2 expseg 10,p3*.7,35,p3*.28,6
   k3 linen p6,p3/3,p3,p3/3
   k4 randh k1,k2,.5
   a4 oscili k3,i1+(p5*.05)+k4,1,.1
   k5 linseg .4,p3*.9,26,p3*.1,0
   k6 linseg 8,p3*.2,20,p3*.76,2
   k7 linen p6,p3/2,p3,p3/1.8
   k8 randh k5,k6,.4
   a3 oscili k7,i1+(p5*.03)+k8,14,.3
   k9 expseg 1,p3*.7,50,p3*.3,2
   k10 expseg 10,p3*.3,45,p3*.7,6
   k11 linen p6,p3/4,p3,p3/4
   k12 randh k9,k10,.5
   a2 oscili k11,i1+(p5*.02)+k12,1,.1
   k13 linseg .4,p3*.6,46,p3*.4,0
   k14 linseg 18,p3*.1,50,p3*.9,2
   k15 linen p6,p3/5,p3,p3/3.8
   k16 randh k13,k14,.8
   a1 oscili k15,i1+(p5*.01)+k16,14,.3
   a5 = a1+a2+a3+a4
   ga1=ga1+(a5*.3)
   ga2=ga2+(p7*a5)
   outs a1+a2,a3+a4
endin

instr 13
   i1=octpch(p5)
   k1 linseg  0,p3*.8,9,p3*.2,1 	 ;p10=rvb attn
   k2 phasor  k1			 ;p7=strt of flt sweep
   k3 table   k2*8,22			 ;p9=fltr bandwdth
   k4 expseg  p7,p3*.7,p8,p3*.3,p7*.9	 ;p8=peak of fltr sweep
   a1 rand    10000			 ;p6=amp
   a2 reson   a1,k4,k4/p9,1
   k5 expseg  .1,p3*.1,p6,p3*.1,p6*.5,p3*.3,p6*.8,p3*.5,.01
   a3 oscili  k5,cpsoct(i1+k3)+a2*.8,1
   ga1=ga1+(p10*a1)
   outs a3,(.98*a3)+(.3*a2)
endin

instr 14
   i1 = octpch(p5)
   k1 line 0,p3,p8
   k2 oscili k1,p7,1			     ;p6=amp
   k3 linseg 0,p3*.7,p6,p3*.3,1 	     ;p7=vib rate
   a1 oscili k3,cpsoct(i1+k2),1 	     ;p8=frq of drop
   k4 linseg 0,p3*.6,p8*.995,p3*.4,0
   k5 oscili k4,p7*.9,1,.1
   k6 linseg 0,p3*.9,p6,p3*.1,3
   a3 oscili k6,cpsoct((i1+.009)+k5),4,.2
   k7 line p8*.985,p3*.7,0
   k8 oscili k7,p7*1.2,1,.7
   k9 linen p6,p3/2,p3,p3/3
   a5 oscili k6,cpsoct((i1+.007)+k8),5,.5
   k10 expon .001,p3*.99,p8*.97
   k11 oscili k10,p7*.97,1,.6
   k12 expseg 1,p3*.8,p6,p3*.2,4
   a7 oscili k12,cpsoct((i1+.005)+k11),6,.8
   k13 expon .002,p3*.91,p8*.99
   k14 oscili k13,p7*.99,1,.4
   k15 expseg 1,p3*.5,p6,p3*.1,p6*.6,p3*.2,p6*.97,p3*.2,2
   a9 oscili k15,cpsoct((i1+.003)+k14),7,.9
   k16 expon p8*.98,p3*.91,.003
   k17 oscili k16,p7*1.4,1,.2
   k18 expseg 1,p3*.6,p6,p3*.2,p6*.8,p3*.1,p6*.98,p3*.1,2
   a11 oscili k18,cpsoct((i1+.001)+k17),8,.1
   outs a1+a5+a9,a3+a7+a11
endin
