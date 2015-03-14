sr=30000
kr=3000
ksmps=10
nchnls=2

instr 1         ;cheby bass p4=pch p5=amp p6=fn p7=chnl1amp

i1      =       cpspch(p4)
		;long or short cheby variant
if p3>.7 goto long
k1      linen   .5,.03,p3,.07           ;driving oscil env
k2      expon   1,p3,.2
k1      =       k2*k1
k10     expseg  .0001,.03,p5,p3-.1,p5*.5,.07,.0001      ;amp env
goto final
long:
k1      linen   .5,p3*.4,p3,p3*.2       ;driving oscil env
k2      line    1,p3,.5
k1      =       k2*k1
k10     expseg  .0001,p3/20,p5,.85*p3,p5*.8,p3/10,.0001 ;amp env
final:
a1      oscili  k1,i1,3
a2      tablei  a1,p6,1,.5      ;tables a1 to fn13, others normalize
a3      balance a2,a1
a4      comb    a3,1,1/i1
a5      =       ((a3*.15)+(a4*.25))*k10
ga1     =       a5*p7
ga2     =       a5*(1-p7)
	outs    ga1,ga2
	endin

instr 3         ;short attack instr
		;p4=pch p5=amp p6=chnl1amp p7=function

i1      =       cpspch(p4)
k1      expseg  .001,.01,1.3,p3-.01,.001
k2      expseg  .001,.03,1.3,p3-.03,.001
k1      =       k1*k2
k3      randi   3,3.7
k4      randi   3,4,.1
k5      line    5,p3,2
k6      line    8,p3,4
k7      line    3,p3,5.5
k8      oscil   6,k5,1
k9      oscil   4,k6,1
k10     oscil   9,k7,1
a1      oscili  1,i1+k8,p7
a2      oscili  1,i1+k3+k9,p7
a3      oscili  1,i1+k4+k10,p7
a1      =       (a1+a2+a3)*(k1*p5)
ga1     =       a1*p6
ga2     =       a1*(1-p6)
	outs    ga1,ga2
	endin

instr 6     ;shimmering instr #1
	    ;variable #partials, power to partials, starting partial, etc
	    ;p4=pch p5=amp p6=power p7=chnl1amp

i1      =       cpspch(p4-2)
k1      linseg  0,p3/2,p5,p3/2,0    ;env
k2      randi   2,25
k3      oscil   .03,4+k2,1              ;rand trem
a1      gbuzz   .33+k3,i1,p6,1,50,4
k4      randi   1.2,15
k5      oscil   .03,4+k2,1              ;rand trem
a2      gbuzz   .33+k5,i1+(i1*.02),p6,1,50,4
k6      randi   1.5,20
k7      oscil   .03,4+k2,1              ;rand trem
a3      gbuzz   .33+k7,i1-(i1*.021),p6,1,50,4
a4      =       (a1+a2+a3)*k1
ga1     =       a4*p7
ga2     =       a4*(1-p7)
	outs    ga1,ga2
	endin

instr 7         ;finger cymbals when high F, lower pitches interesting too
		;p4=pch p5=amp p6=chnl1amp
                                          
i1      =       cpspch(p4)
k1      expseg  .0001,.05,p5,p3-.05,.0001       ;env
k2      linseg  1.69,.1,1.75,p3-.1,1.69         ;power to partials
a1      foscil  k1,i1,1,2.01,k2,1
ga1     =       a1*p6
ga2     =       a1*(1-p6)
	outs    ga1,ga2
	endin

instr 8         ;melody instrA p4=pch p5=amp p6=cheby# p7=choice p8=chnl1amp
		;cheby/fm additive synthesis

i1      =       cpspch(p4)
k100    randi   1,10
k101    oscili  i1/65,5+k100,1
k102    linseg  0,.5,1,p3-.5,1
k100    =       i1+(k101*k102)
k1      linen   .5,p3*.4,p3,p3*.2    ;driving oscil env
k2      line    1,p3,.5
k1      =       k2*k1
if p7=2 goto cresc
if p7=3 goto dim
if p7=4 goto sfz
if p7=5 goto slow
k10     expseg  .0001,.05,p5,p3-.15,p5*.8,.1,.0001      ;amp env
k20     linseg  1.485,.05,1.5,p3-.05,1.485              ;power to partials
     goto next
     cresc:
k8      expseg  .0001,.05,p5*.25,p3-.15,p5,.1,.0001     ;amp env
k9      linseg  1,p3-.15,1.5,.1,0
k10     =       (k8*k9)/2
k20     line    1.475,p3,1.5                            ;power to partials
     goto next
     dim:
k10     linseg  0,.05,p5,.05,p5*.8,p3*.33,p5*.7,(p3*.66)-.1,0   ;amp env
k20     line    1.5,p3,1.475                            ;power to partials
     goto next
     sfz:
k10     linseg  0,.03,p5,.04,p5,.03,p5*.3,p3-.15,p5*.3,.05,0    ;amp env
k20     linseg  1.4,.03,1.7,.04,1.7,.03,1.4,p3-.1,1.385 ;power to partials
     goto next
     slow:
k10     linseg  0,.06,p5*.45,.04,p5*.2,(p3/3)-.1,p5,p3/3,p5*.9,p3/3,0 ;amp env
k20     linseg  1.475,p3/3,1.5,p3/3,1.499,p3/3,1.475    ;power to partials
     next:      ;a1-3 are for cheby with p6=1-4
a1      oscili  k1,k100-.025,3
a2      tablei  a1,p6,1,.5      ;tables a1 to fn13, others normalize
a3      balance a2,a1
a4      foscil  1,k100+.04,1,2.005,k20,1   ;try other fn as well
a5      oscili  1,k100,1
a6      =       ((a3*.1)+(a4*.1)+(a5*.8))*k10
a7      comb    a6,.5,1/i1
a6      =       (a6*.9)+(a7*.1)
	outs    a6*p8,a6*(1-p8)
	endin

instr 11        ;simple marimba
		;p4=pch p5=amp p6=chnl1amp

i1      =       cpspch(p4)
k1      expseg  .0001,.03,p5,p3-.03,.001      ;env
k25     linseg  1,.03,1,p3-.03,3
k1      =       k25*k1
k10     linseg  2.25,.03,3,p3-.03,2     ;power to partials
a1      gbuzz   k1,i1,k10,0,35,4
a2      reson   a1,500,50,1     ;filt
a3      reson   a2,1500,100,1   ;filt
a4      reson   a3,2500,150,1   ;filt
a5      reson   a4,3500,150,1   ;filt
a6      balance a5,a1
ga1     =       a6*p6
ga2     =       a6*(1-p6)
	outs    ga1,ga2
	endin

instr 12        ;simple 8va harmonic instr      p4=pch p5=amp p6=chnl1amp

i1      =       cpspch(p4-1)
k1      linseg  0,p3/2,p5,p3/2,0
k9      rand    .75,10
k10     oscil   p5/10,1.5+k9,1          ;tremelo
a1      gbuzz   k1+k10,i1,2,2,0,4
ga1     =       a1*p6
ga2     =       a1*(1-p6)
	outs    ga1,ga2
	endin

instr 15        ;ring mod instr p4=pch p5=amp p6=chnl1amp

i1      =       cpspch(p4)
k1      linseg  0,p3/4,p5,p3/2,p5*.9,p3/4,0     ;env
a1      oscili  1,i1*.9983,3
a2      oscili  1,i1,3
a3      =       a1*a2
a1      =       a3*k1
ga1     =       a1*p6
ga2     =       a1*(1-p6)
	outs    ga1,ga2
	endin

instr 16        ;sharp attack instr a p4=pch p5=amp p6=chnl1amp p7=0=down
		;quick gliss

i1      =       cpspch(p4)
k1      expseg  1,.05,1,p3-.05,1.35     ;gliss up
if p7=0 goto next
k1      =       2-k1
next:
a1      oscili  1,i1*k1,8
k5      expseg  .00001,.03,p5,.02,p5,p3-.05,.00001      ;env amp
k6      linseg  1,.05,1,p3-.05,2.5
k5      =       k5*k6
a1      =       a1*k5
ga1     =       a1*p6
ga2     =       a1*(1-p6)
	outs    ga1,ga2
	endin

instr 18        ;noise generator p4=pch p5=amp p6=chnl1amp

i1      =       cpspch(p4)
k1      randi   i1/3.5,500
k2      linseg  0,p3/3,p5,p3/3,p5,p3/3,0
a1      oscili  k2,i1+k1,1
	outs    a1*p6,a1*(1-p6)
	endin

instr 20        ;accepts global outputs and adds reverb p4=rvbtime

a1      reverb  ga1,p4
a2      reverb  ga2,p4
	outs    (a1*.1),(a2*.1)
	endin
