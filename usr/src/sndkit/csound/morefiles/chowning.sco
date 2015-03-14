        ; Sine Wave
f1 0 1024 9 1 1 0

        ; ADSR Trumpet Envelope  - fig 1.11
f2 0 513 7 0 85.33 1 85.33 .75 85.33 .65  170.66 .50  85.33 0

        ; AR Woodwind Envelope for Carrier - fig 1.12
f3 0 513 7 0 100 1 392 .9 20 0

        ; Gated Woodwind Envelope for Modulator - fig 1.13
f4 0 513 7 0 100 1 412 1

        ; Exponential decaying envelope for bell-like timbres.
f5 0 513 5 1 512 .001

        ; Modification of Exponential envelope for drumlike sounds
f6 0 513 5 .7 16 .8 48 1 64 .8 128 .2 256 .001

        ; Modulator envelope for wood-drum sounds
f7 0 513 7 0 12 1 52 0 460 0


        ; f0 statement is used to extend the section's duration
        ; empty pfields will carry the value from above
        ; the + symbol in p2 will give the value of p2+p3 from the previous note


;  st  dur  amp     carFreq modFreq Index1  Index2  CarEG   ModEg

        ; Brass Timbre
f0 5
i1 0   .6   10000   440     440     0       5       2       2
i. +
i1 2   .    .       220     220
i. +
s
        ; Woodwind Timbre
f0 5
i1 0   .5   10000   900     300     0       2       3       4
i. +
i1 1.5 .    .       300     100
i. +
s
        ; Bassoon Timbre
f0 5
i1 0   .5   10000   500     100     0       1.5     3       4
i. +
i1 1.5 .    .       1000    200
i. +
s
        ; Reed Timbre
f0 5
i1 0   .5   10000   900     600     4       2       3       4
i. +
i1 1.5 .    .       300     200
i. +
s
        ; Bell Timbre
f0 12
i1 0    10  10000   200     280     0       10      5       5
s
        ; Drum Timbre
f0 4
i1 0   .2   10000   80      55      0       25      6       6
i. +
i. +
i. +
i1 1   .2   10000   160     110
i. +
i. +
i. +
s
        ; Wood-drum Timbre
f0 4
i1 0   .2   10000   80      55      0       25      6       7
i. +
i. +
i. +
i1 1   .2   10000   160     110
i. +
i. +
i. +
s
        ; Double-carrier Instrument
        ; Amplitude of 2100Hz formant increases

;  st dur C1amp C1frq Mfrq Indx1 Indx2 Cenv Menv C2amp C2indx C2frq

f0 8
i2 0  .6  10000 300   300  1     3     2    4    .2    .5     2100
i. +
i. +  .   .     .     .    .     .     .    .    .4
i. +
i. +  .   .     .     .    .     .     .    .    .6
i. +
i. +  .   .     .     .    .     .     .    .    .8
i. +
i. +  .   .     .     .    .     .     .    .    1
i. +
s
        ; Double-carrier Instrument
        ; Bandwidth of 2100Hz formant increases

;  st dur C1amp C1frq Mfrq Indx1 Indx2 Cenv Menv C2amp C2indx C2frq

f0 8
i2 0  .6  10000 300   300  1     3     2    4    .5    .2     2100
i. +
i. +  .   .     .     .    .     .     .    .    .     .3
i. +
i. +  .   .     .     .    .     .     .    .    .     .5
i. +
i. +  .   .     .     .    .     .     .    .    .     .7
i. +
i. +  .   .     .     .    .     .     .    .    .     .9
i. +
s
        ; Double-carrier Instrument
        ; Frequency of formant changes

;  st dur C1amp C1frq Mfrq Indx1 Indx2 Cenv Menv C2amp C2indx C2frq

i2 0  .6  10000 150   150  1     3     2    4    .5    .5     600
i. +
i. +  .   .     .     .    .     .     .    .    .     .      1200
i. +
i. +  .   .     .     .    .     .     .    .    .     .      1800
i. +
i. +  .   .     .     .    .     .     .    .    .     .      2100
i. +
i. +  .   .     .     .    .     .     .    .    .     .      2400
i. +
i. +  .   .     .     .    .     .     .    .    .     .      2700
i. +
i. +  .   .     .     .    .     .     .    .    .     .      3000
i. +
i. +  .   .     .     .    .     .     .    .    .     .      3300
i. +
i. +  .   .     .     .    .     .     .    .    .     .      3600
i. +
s

f0 2

s

	; test score for chowning soprano instrument

f01        0   512    10     1
; fmt amplitude rise function
f02        0   513     7     0   256    .2   256     1
; portamento function
f03        0   513     7    -1   200     1   100     0   212     0
; fmt freq lookup function
f04        0   513     7     1    80     1   200    .9   200    .6    32    .6
; fmt amplitude factor lookup function
f05        0   513     7    .4   100    .2   412     1
; index 1 lookup function
f06        0   513     7     1   100     1   112    .4   300   .15
; index 2 lookup function
f07        0   513     7     1   100    .5    80   .25   132    .5   100    .7
   100    .4

; play some notes
; p4 = amp; p5 = fund; p6 = 0 < ampfac < 1; p7 = fmt env func
; p8 = port func; p9 = fmt hz func; p10 = fmt amp func; p11, 12 = i1,i2 fns

i3	  0	1 20000  7.07	 .5	2     3     4	  5	6     7
i3	  1	.     .  8.04
i3	  2	.     .  9.01
i3	  3	.     .  9.10
i3	  4	2     . 10.07

e
