; drum.o
; This is the Csound code for the Drum Instrument based on
; Risset's Introductory Catalog of Computer Synthesized Sounds
; see pp.93-94 and figure 3.26 in "Computer Music" - Dodge

sr = 16000
kr = 1000
ksmps = 16
nchnls = 1

instr 1
                                ; initialization
i1 = 1/p3                       ; once per duration - for envelopes
i2 = cpspch(p4)                 ; convert oct. point pch-class notation to Hz
i3 = p5/2                       ; these three assignments balance the three
i4 = p5/6                       ; branches of the drum instrument
i5 = p5/2.5

                                ; branch 1 - NOISE
a1      oscili  i3,i1,2         ; generate steep exponential envelope
a1      randi   a1,p6           ; generate band of noise with freq. given by p6
a1      oscili  a1,500,4        ; use noise band for amp input - ring mod.

                                ; branch 2 - INHARM
a2      oscili  i4,i1,2         ; steep envelope with lower amplitude than a1
a2      oscili  a2,i2*.1,3      ; generate inharmonic partials - 1,1.6,2.2,2.3

                                ; branch 3 - FUND
a3      oscili  i5,i1,1         ; decay of f1 is less steep than f2
a3      oscili  a3,i2,4         ; generates fundamental tone

                                ; global envelope to prevent clicking
a4      linseg  1,p3-.05,1,.05,0,.01,0
        out     a4*(a1+a2+a3)
endin
