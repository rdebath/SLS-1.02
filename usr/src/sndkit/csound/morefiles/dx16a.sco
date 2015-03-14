;       test score for DX7 alg 16 orc                                ;
;       (imitates the PLUK16.1 voice...sort of)                      ;

f01     0       512     10      1
; operator output level to amp scale function (data from Chowning/Bristow)
f02     0       128     7       0       10      .003    10      .013
        10      .031    10      .079    10      .188    10      .446
        5       .690    5       1.068   5       1.639   5       2.512
        5       3.894   5       6.029   5       9.263   4       13.119
        29      13.119
; rate scaling function
f03     0       128     7       0       128     1
; eg rate rise function for lvl change between 0 and 99 (data from Opcode)
f04     0       128     -7      38      5       22.8    5       12      5
        7.5     5       4.8     5       2.7     5       1.8     5       1.3
        8       .737    3       .615    3       .505    3       .409    3       
        .321    6       .080    6       .055    2       .032    3       .024
        3       .018    3       .014    3       .011    3       .008    3       
        .008    3       .007    3       .005    3       .003    32      .003
; eg rate rise percentage function
f05     0       128     -7      .00001  31      .00001  4       .02     5
        .06     10      .14     10      .24     10      .35     10      .50
        10      .70     5       .86     4       1.0     29      1.0
; eg rate decay function for lvl change between 0 and 99
f06     0       128     -7      318     4       181     5       115     5
        63      5       39.7    5       20      5       11.2    5       7       
        8       5.66    3       3.98    6       1.99    3       1.34    3       
        .99     3       .71     5       .41     3       .15     3       .081
        3       .068    3       .047    3       .037    3       .025    3
        .02     3       .013    3       .008    36      .008
; eg rate decay percentage function
f07     0       128     -7      .00001  10      .25     10      .35     10
        .43     10      .52     10      .59     10      .70     10      .77
        10      .84     10      .92     9       1.0     29      1.0
; eg level to amp factor mapping function (index in radians = Index / 2PI)
f08     0       128     -7      0       10      .000477 10      .002
        10      .00493  10      .01257  10      .02992  10      .07098
        5       .10981  5       .16997  5       .260855 5       .39979
        5       .61974  5       .95954  5       1.47425 4       2.08795
        29      2.08795
; velocity to amp factor mapping function (rough guess)
f09     0       129     9       .25     1       0
; velocity sensitivity scaling function
f10     0       8       -7      0       8       1
; feedback scaling function
f11     0       8       -7      0       8       7            
; operator 1 parameters:    OutLvl KeyVel     EGR1    EGR2    EGR3    EGR4
f12     0       32      -2      99      1       99      33      14     38
;                                             EGL1    EGL2    EGL3    EGL4
                                                99      80      0      0
;                                              AMS  FIXED?    FREQ     DET
                                                0       1       1      0
;                                              RSS
                                                2
; operator 2 parameters
f13     0       32      -2      67      6       75      45      36      19
                                                99      87      0       0
                                                0       0       11.22   -2
                                                2
; operator 3 parameters
f14     0       32      -2      99      7       99      30      34      46
                                                99      80      0       0       
                                                0       0       .5      0
                                                0
; operator 4 parameters
f15     0       32      -2      78      7       90      67      21      82
                                                99      85      0       0
                                                0       0       7       0
                                                0
; operator 5 parameters
f16     0       32      -2      99      4       99      64      0       8
                                                85      48      0       0
                                                0       0       3       0
                                                0
; operator 6 parameters
f17     0       32      -2      99      1       99      82      75      0
                                                99      87      0       0
                                                0       1       2570    0
                                                0
;====================================================================;
;                       Yamaha DX7 Algorithm 16                      ;
;                                                                    ;
;       p02 = start     p03 = dur       p04 = pch       p05 = vel    ;
;       p06 = panfac    p07 = vibdel    p08 = vibwth    p09 = vibhz  ;
;       p10 = op1fn     p11 = op2fn     p12 = op3fn     p13 = op4fn  ;
;       p14 = op5fn     p15 = op6fn     p16 = ampfn     p17 = pkamp  ;
;       p18 = rsfn      p19 = devfn     p20 = erisfn    p21 = edecfn ;
;       p22 = vsfn      p23 = velfn     p24 = feedfn    p25 = feedbk ;
;                                                                    ;   
;       4PM 7-7-89                                                   ;   
;====================================================================

i01     0    .49   6.00    87    0       0       0       0
        12    13    14      15    16      17      2       4000
        3     8     4       6     10      9       11      7
i 1.1   1.00  .54  6.07    87
i 1.2   2.00  .16  6.08    75
i 1.3   2.49  .20  7.03    90
i 1.4   3.01  .20  7.04    74
i 1.5   3.50  .18  7.11    90
i 1     4.01  .15  8.00    75
i 1.1   4.24  .13  8.07    69
i 1.2   4.50  .07  8.08    72
i 1.3   4.72  .09  9.03    90
i 1.4   4.98  .16  9.00    79
e
