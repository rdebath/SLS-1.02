;====================================================================;
;       test score for DX7 alg 16 orc                                ;
;       7:59AM  3/07/1989                                            ;
;====================================================================;
f01     0       512     10      1
; operator output level to amp scale function (use curve from Chowning)
f02     0       128     7       0       10      .003    10      .013
        10      .031    10      .079    10      .188    10      .446
        5       .690    5       1.068   5       1.639   5       2.512
        5       3.894   5       6.029   5       9.263   4       13.119
        29      13.119
; rate scaling function
f03     0       128     7       0       128     1
; eg rate rise function for lvl change between 0 and 99
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
; eg level to peakdev mapping function (Index/2pi)
f08     0       128     -7      0       10      .000477 10      .002
        10      .00493  10      .01257  10      .02992  10      .07098
        5       .10981  5       .16997  5       .260855 5       .39979
        5       .61974  5       .95954  5       1.47425 4       2.08795
        29      2.08795
; velocity to amp factor mapping function
;f09     0       129     5       .01     128     1
;f09     0       129     7       0       128     1
f09     0       129     9       .25     1       0
; velocity sensitivity scaling function
f10     0       8       -7      0       8       1
; feedback scaling function
f11     0       8       -7      0       8       7            
; operator 1 parameters:    OutLvl KeyVel     EGR1    EGR2    EGR3    EGR4
f12     0       32      -2      99      6       58      40      26     56
;                                             EGL1    EGL2    EGL3    EGL4
                                                99      92      30     0
;                                              AMS  FIXED?    FREQ     DET
                                                0       0       .5     -1
;                                              RSS
                                                0
; operator 2 parameters
f13     0       32      -2      81      2       58	 68	    55	  53
                                                93      43      0       0
                                                0       0       2.26    -7
                                                0
; operator 3 parameters
f14     0       32      -2      99      7       58      46      25      55
                                                99      95      88      0       
                                                0       0       .5      -7
                                                0
; operator 4 parameters
f15     0       32      -2      91      0       73      66      99      53
                                                93      17      38      0
                                                0       0       1.3     -1
                                                1
; operator 5 parameters
f16     0       32      -2      95      7       99      99      91      92
                                                99      99      75      0
                                                0       1       10      0
                                                0
; operator 6 parameters
f17     0       32      -2      80      0       99      99      51      99
                                                90      80      0       0
                                                0       1       97.72   -4
                                                0
;====================================================================;
;                       Yamaha DX7 Algorithm  5                      ;
;                                                                    ;
;       p02 = start     p03 = dur       p04 = pch  	 p05 = vel    ;
;       p06 = panfac    p07 = vibdel    p08 = vibwth    p09 = vibhz  ;
;       p10 = op1fn     p11 = op2fn     p12 = op3fn     p13 = op4fn  ;
;       p14 = op5fn     p15 = op6fn     p16 = ampfn     p17 = pkamp  ;
;       p18 = rsfn      p19 = devfn     p20 = erisfn    p21 = edecfn ;
;       p22 = vsfn      p23 = velfn     p24 = feedfn    p25 = feedbk ;
;                                                                    ;   
;       4PM 7-7-89                                                   ;   
;====================================================================;
i01     0    -1         8.00    87    0       0       0       0
        12    13    14      15    16      17      2       8000
        3     8     4       6     10      9       11      4
i01.1   1.00 -1     8.07    87
i01.2   2.00 -1     8.08    75
i01.3   2.49 -.5    9.03    90
i01.4   3.01 -.5    8.10    74
i01.5   3.50 -.5    9.05    90
i01.6   4.01 -3     9.00    75
i -1       8
e
