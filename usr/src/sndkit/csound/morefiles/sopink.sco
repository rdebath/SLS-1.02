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
i01        0     1 20000  7.07    .5     2     3     4     5     6     7
i01        1     .     .  8.04
i01        2     .     .  9.01
i01        3     .     .  9.10
i01        4     2     . 10.07 
e
