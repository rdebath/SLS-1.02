sr = 16000
kr = 1000
ksmps = 16
nchnls = 2

instr 1,4			      ; p4  = amplitude of output wave
    i1 = 1/p3			      ; p5  = carrier frequency specified in Hz
    idev1 = p7 * p6		      ; p6  = modulating frequency in Hz
    idev2 = (p8-p7) * p6	      ; p7  = modulation index 1
				      ; p8  = modulation index 2
    ampcar  oscil   p4,i1,p9	      ; p9  = carrier envelope function
    ampmod  oscil   idev2,i1,p10      ; p10 = modulator envelope function

    amod    oscili  ampmod+idev1,p6,1
    gasig   oscili  ampcar,p5+amod,1

   outs1 gasig *.25
   outs2 gasig *.25
endin

instr 2
   i1 = 1/p3			      ; p4  = amplitude of output wave
   idev1 = p7 * p6		      ; p5  = carrier frequency specified in Hz
   idev2 = (p8-p7) * p6 	      ; p6  = modulating frequency in Hz
   k1 randi  120,10		      ; p7  = modulation index 1
   k2 randi  200,20		      ; p8  = modulation index 2
   ampcar  oscil   p4,i1,p9	      ; p9  = carrier envelope function
   ampmod  oscil   idev2,i1,p10       ; p10 = modulator envelope function

   amod    oscili  ampmod+idev1,p6+k1,1
   gasig   oscili  ampcar,k2+p5+amod,1
   outs1   gasig * .25
   outs2   gasig * .25
endin

instr 3 			      ; paner
   k1	   oscil  1,1/p3,p4
   outs1   gasig*k1
   outs2   gasig*(1-k1)
endin

instr 5
   k1 linseg p5,p3.4,p5,p3.6,p5*.9
   a1 pluck  p4,cpspch(p5),cpspch(p5),0,1,0,0
   outs a1,a1
endin

instr 6
	k1 expseg p4*.8,p3*.25,p4,p3*.6,p4*.2,p3*.15,.01
	a1 foscili k1,1,p5,p6,p7,1,0
	outs a1,a1
endin

instr 7
	k1 expseg p4*.001,p3*.2,p4,p3*.7,p4,p3*.1,p4*.001
	k2 expseg p7,p3*.02,p7*.5
	k3 oscil 10,p8,1
	k4 oscil 1,1/p3,2
	a1 foscili k1,1,p5+k3,p6,k2,1,0
	outs a1,a1
endin
