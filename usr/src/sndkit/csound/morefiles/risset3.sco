  ; Waveforms: Instrument 1

f1 0 2048 10 1					 ; fundamental
f2 0 2048 10 1 .2 .08 .07			 ; four harmonics
f3 0 2048 10 1 .4 .2 .1 .1 .05			 ; six harmonics

   ; Amplitude Envelope Functions: Instrument 1

f4 0 512 7 0 1 0 49 .2 90 .6 40 .99 25 .9 45 .5 50 .25 50 .12 50 .06 50 .02 62 0
f5 0 512 7 0 1 0 49 .2 100 .6 50 .99 150 .2 162 0
f6 0 512 7 0 1 0 49 .2 200 .5 100 .2 162 0
f7 0 512 7 0 1 0 79 .5 60 .5 20 .99 120 .4 140 .6 92 0

   ; Amplitude Envelope Fynctions: Instrument 2

f8 0 512 7 0 1 0 149 .4 200 .99 50 .5 50 .24 62 0

   ; Pitch Envelope Functions: Instrument 2

f9 0 512 7 0 1 .895 511 .99
f10 0 512 7 0 1 .99 511 .99

	; DC bias functions

f12 0 512 9 1 .26 0
f13 0 512 9 1 .3 0

;   ============    Score for Risset's "Bell" Instrument     ==========   ;

f2 0 513 5 1024 512 1	; Amplitude envelope with extended guard point

;       start   dur     freq    amp

i6	1	4	633	2500
i.      +       .       211
i.      +       .       999

s

i6	1	4	633	1500
i6	1	4	211	1500
i6	1	4	999	1500
i6	1	4	80	1500

e
