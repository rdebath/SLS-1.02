sr=20000
kr=1000
ksmps=20
nchnls=1

;			      Risset's Flute-like                         ;

instr 1
  if p12 = 12 igoto dc1
	i1 = .6
  goto start

  dc1:
	i1 = .74
  goto start

  start:
	k1	 randi (p4*.01),p9
	k1 = k1 + p4
	k2	 oscil k1,1/p11,p12
	k2 = k2 + i1
	k3	 oscil k2,1/p6,p9
	a1	 oscili k3,p5,p10
	out  a1*10
endin

instr 2
	k1 oscil p4,1/p6,p7
	k2 oscil p5,1/p6,p8
	a1 oscili k1,k2,1
	out a1*10
endin

;		      Risset's Waveshaping Clarinet                       ;

instr 3
	i1 = cpspch(p4)
	i2 = .64
  if p3 >.75 igoto start
	i2 = p3 - .085
  start:
	a1  linen   255,.085,p3,i2
	a1  oscili  a1,i1,1
	a1  tablei  a1+256,2
	out a1*p5
endin

;		Another of Risset's Waveshaping Instruments               ;

instr 4
   i1=1/p3
   i2=cpspch(p4)

   a1 oscili p5,i1,2		 ;scaling factor code
   a2 oscili a1,i2,1

   a3 linseg 1,.04,0,p3-.04,0
   a4 oscili a3,i2*.7071,1	 ; audio code

; inline code for transfer function:
; f(x)=1+.841x-.707x**2-.595x**3+.5x**4+.42x**5-;.354x**6.279x**7+.25x**8+.21x**9

   a5=a4*a4
   a6=a5*a4
   a7=a5*a5
   a8=a7*a4
   a9=a6*a6
   a10=a9*a4
   a11=a10*a4
   a12=a11*a4

   a13=1+.841*a4-.707*a5-.595*a6+.5*a7+.42*a8-.354*a9-.297*a10+.25*a11+.21*a12

   a14=a13*a2
   out a14
endin

;			Risset's Arpeggio Instrument                      ;


instr 8
   i1 = p6			   ;init values correspond to freq.
   i2 = 2*p6			   ;offsets for oscillators based on original p6
   i3 = 3*p6
   i4 = 4*p6

   ampenv  linen   p5,.01,p3,.02   ;a simple envelope to prevent clicking.

   a1	   oscili  ampenv,p4,2
   a2	   oscili  ampenv,p4+i1,2  ;nine oscillators with the same ampenv
   a3	   oscili  ampenv,p4+i2,2  ;and waveform, but slightly different
   a4	   oscili  ampenv,p4+i3,2  ;frequencies to create the beating effect
   a5	   oscili  ampenv,p4+i4,2
   a6	   oscili  ampenv,p4-i1,2  ;p4 = freq of fundamental (Hz)
   a7	   oscili  ampenv,p4-i2,2  ;p5 = amp
   a8	   oscili  ampenv,p4-i3,2  ;p6 = initial offset of freq - .03 Hz
   a9	   oscili  ampenv,p4-i4,2
      out     a1+a2+a3+a4+a5+a6+a7+a8+a9
endin
