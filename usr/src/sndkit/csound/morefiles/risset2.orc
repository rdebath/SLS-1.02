sr=20000
kr=1000
ksmps=20
nchnls=1

;			  Risset's Drum Instrument                        ;


instr 7
   i1=p5*.3
   i2=p4*.1
   i3=1/p3
   i4=p5*.8
   i5=p4

   a1 randi p5,4000
   a1 oscil a1,i3,2
   a1 oscil a1,3000,1

   a2 oscil i1,i3,2
   a2 oscil a2,i2,3

   a3 oscil i4,i3,4
   a3 oscil a3,i5,1

   out a1+a2+a3
endin
