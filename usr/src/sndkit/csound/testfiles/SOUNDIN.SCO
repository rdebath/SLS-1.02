;===========================================;
;       Sample Score for soundin.orc        ;
;===========================================;
;                          2:52PM  3/23/1989;
;===========================================;
;       Straight Mixing Instrument          ;
;                                           ;
;       p4 = File A     p5 = File B         ;
;       p6 = A rise     p7 = A decay        ;
;       p8 = B rise     p9 = B decay        ;
;===========================================;
;                       soundA  soundB  riseA   decA    riseB   decB
i01     0       4       5       5       .01     3.99    3       1
s
; Sample File is Soundin.5 -- no rescaling
f1	0	32768  -1	5	0
;===========================================;
;        Pitch Changing Instrument          ;
;                                           ;
;       p4 = Soundin #  p5 = desired pitch  ;
;       p6 = old pitch  p7 = original sr    ;
;===========================================;
i02	0	4	5	5.01	6.00	18900
i02     4       2       5       7.00
i02	6	3	5	4.06
e
