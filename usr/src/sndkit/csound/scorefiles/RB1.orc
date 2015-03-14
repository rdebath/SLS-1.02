;Richard Boulanger - orchestra from 'Trapped in Convert'

sr=20000
kr=1000
ksmps=20
nchnls=2

ga1 init 0
ga2 init 0

instr 1 				       ;p8=del time (<1=default)
i1=cpspch(p5)				       ;p9=frq drop
					       ;p7=vib rate
a19 init 0
a19 expseg  1,p8,1,p3-p8,p9

k1 line 0,p3,5
k2 oscili k1,p7,1
k3 linseg 0,p3*.7,p6,p3*.3,0
a1 oscili k3,(i1+k2)*a19,1

k4 linseg 0,p3*.6,6,p3*.4,0
k5 oscili k4,p7*.9,1,1.4
k6 linseg 0,p3*.9,p6,p3*.1,0
a3 oscili k6,((i1+.009)+k5)*a19,33,.2

k7 line 9,p3*.7,1
k8 oscili k7,p7*1.2,1,.7
k9 linen p6,p3/2,p3,p3/3
a5 oscili k9,((i1+.007)+k8)*a19,34,.3

k10 expon 1,p3*.99,3.1
k11 oscili k10,p7*.97,1,.6
k12 expseg .001,p3*.8,p6,p3*.2,.001
a7 oscili k12,((i1+.005)+k11)*a19,35,.5

k13 expseg 1,p3*.4,3,p3*.6,.02
k14 oscili k13,p7*.99,1,.4
k15 expseg .001,p3*.5,p6,p3*.1,p6*.6,p3*.2,p6*.97,p3*.2,.001
a9 oscili k15,((i1+.003)+k14)*a19,36,.8

k16 expon 4,p3*.91,1
k17 oscili k16,p7*1.4,1,.2
k18 expseg .001,p3*.6,p6,p3*.2,p6*.8,p3*.1,p6*.98,p3*.1,.001
a11 oscili k18,((i1+.001)+k17)*a19,37,1.3

outs a1+a3+a5,a7+a9+a11
endin

instr 2 			   ;p9=# of harm
i1=cpspch(p5)			   ;p10=swp rt
				   ;p7=rvb atn
k1 randi 1,30			   ;p8=lfo frq
k2 line p9,p3/p10,1
k3 linseg 0,p3/2,1,p3/2,0
k4 expseg .001,p3*.01,p6,p3*.99,.001
k5 linseg .005,p3*.71,.015,p3*.27,.01
k6 oscili k3,p8,1,.2
k7=k6+2
k8 line 1,p3,0
a1 gbuzz k4,i1+k5,k7,k2,k1,12
ga1=(k8*p7*a1)+ga1
outs a1,a1
endin

instr 3
i1=cpspch(p5)			  ;p7=rvb attn
k3 expseg 1,p3/2,30,p3/2,2	  ;p8=rand frq
k4 expseg 10,p3*.7,p8,p3*.3,6
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
a3 oscili k9,i1+(p5*.1)+k16+k13,13,.3
k17 line 1,p3,0
ga1=(k17*p7*(a1+a2+a3))+ga1
outs a3+a1,a2+a3
endin

instr 4 		   ;p10 rvb atten
k1 expon p7,p3,p8	   ;p9 = bdwth
a1 rand 10000		   ;p7=strt frqval of fltr swp
a2 reson a1,k1,k1/p9,1	   ;p8=end frqval of filter swp
k2 envlpx p6,p3*.1,p3*.6,p3*.4,17,.5,.01,-.8
k3 oscili .6,11.3,1,.1
a3 oscili k2,cpspch(p5)+k3,19
k4 line 1,p3,0
ga1=(k4*p10*a3)+ga1
outs (a2*.8)+a3,a2*.6+(.7*a3)
endin

instr 6				   ;p9 car
i1 = cpspch(p5) 		   ;p10 mod
k1 line p9,p3*1.01,1 		   ;p11 index
k2 line 1,p3,p10		   ;p12 rand frq
k4 expon 2,p3,p12
k5 line 0,p3*.8,8
k3 envlpx p6,.1,p3,.5,8,2,.5,.05
k7 randh p11,k4
k6 oscili k4,k5,1,.3
k8 expseg p6*.78,p3*.48,p6*1.04,p3*.52,p6*.80
k9 line int(p8)/4,p3,frac(p8)/.4
k10 line 1,p3,0
a1 foscili k3,i1+k6,k1,k2,k7,1
a2 oscili k8,i1*1.001,1
ga1=(k10*p7*(a1+a2))+ga1
outs  k10*(a1+a2)*(1-k9),k10*(a1+a2)*k9
endin

instr 7 		;p9=amp
k1 phasor p4
k2 table k1*8,7
a1 rand 10000
k3 expon p5,p3,p6
a2 reson a1,k3*k2,k3/p7,1
k4 line 1,p3,0
ga1=(k4*p8*a2)+ga1
outs p9*k4*a2,p9*k4*a2
endin

instr 9 				    ;p7 rvb atten
i1=cpspch(p5)				    ;p8 rand amp
k1 linen p6,p3*.1,p3,p3*.8		    ;p9 rand frq
k2 randh p8,p9,.1			    ;p10 1 for space
k3 randh p8*.98,p9*.91,.2
k4 randh p8*1.2,p9*.96,.3
k5 randh p8*.9,p9*1.3
a1 oscili k1,i1+k2,1
a3 oscili k1*.91,(i1+.004)+k3,19,.3
a5 oscili k1*.85,(i1+.006)+k4,20,.5
a7 oscili k1*.95,(i1+.009)+k5,21,.8
k6 line 1,p3,0
ga2=(k6*p4*(a1+a3))+ga2
ga1=(k6*p7*(a5+a7))+ga1
outs a1+a5,a3+a7
endin

instr 50 		   ;p4=cps of pan
ga1 init 0
k1 oscili .5,p4,1
k2 = .5+k1
k3 = 1-k2
a1 reverb ga1,2.1
outs k2*a1,a1*k3*(-1)
ga1=0
endin

instr 60
ga2 init 0
a1 delay ga2,.05
outs a1,a1
ga2=0
endin
