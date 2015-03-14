; Simple Sine Function
f01     0       512    10       1 
; Ramp
f02     0       513     7       0       512     1
; Exponential rise
f03     0       513     5       .001    512     1
; Quarter Sine Wave in 128 locs + extended guard point
f04     0       129     9       .25     1       0
; Quarter Cosine
f05     0       129     9       .25     1       90
; Triangular Wave
f06     0       512     10      1   0 .111   0  .04    0  .02    0 .012
; Sawtooth Wave
f07     0       512     10      1  .5   .3 .25   .2 .167  .14 .125 .111
; Square Wave
f08     0       512     10      1   0   .3   0   .2    0  .14    0 .111
; Narrow Pulse
f09     0       512     10      1 1  1   1 .7 .5 .3 .1 
; Exponential rise and decay
f10     0       513     5       .1      32      1       480     .01
; Reverse pyramid
f11     0       513     7       1       256     0       256     1

;====================================================================;
;                    Portamento/Panning Instrument                   ;               
;                                                                    ;
; p4=amp  p5=pch p6=portsize (oct)  p7=delay  p8=ofn  p9=p3 in beats ; 
;====================================================================;

; Tempo = 72 beats/min
t00     72

;p1     p2      p3      p4      p5      p6      p7      p8      p9      p10

i1	0	1	15000	8.00	.5	.5	6	np10	0
i1	.5	.	.	.	-.5	.	.	1	pp3

f00	4

s

;=========================================================================;
;                 Simple Gating Instrument with Chorus                    ;
;                                                                         ;
; p4=amp      p5=pch1       p6=pch2       p7=risefac      p8=decfac       ;
; p9=ofn1    p10=ofn2      p11=gatefn    p12=beathz      p13=gatehz       ; 
;=========================================================================;

;p1     p2      p3      p4      p5      p6      p7  p8 p9 p10 p11 p12 p13

i2	0     1.5	15000	7.07   8.07    .02  .4	9   6  10   1	0
i2	2	.	.	7.01   7.01	.    .	7   1  11   0	1.3

f00	6

s

;==========================================================================;
;               Basic FM Instrument with Variable Vibrato                  ;
;                                                                          ;
;  p4=amp       p5=pch(fund)    p6=vibdel       p7=vibrate      p8=vibwth  ;
;  p9=rise     p10=decay       p11=max index   p12=car fac     p13=mod fac ;
; p14=index rise  p15=index decay  p16=left channel factor p17=original p3 ;
;==========================================================================;

;p1     p2      p3  p4      p5   p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17  p18

i3	0	4   25000  7.05 2  7 .02 1   1	   4   1   1 0	 0   0	 np18
i3	4	.   .	   .	.  .  .  .   .	   3   3   2 .	 .   .	 .    pp3
i3	8	8   .	   .	0  0  0  .01 7.9  10 1.4   1 .	 .   .	 8

f00    18

s

;==========================================================================;
;	   Bach Chorale Instrument - Designed for Legato Phrasing	   ;
;                                                                          ;
;	p3 = duration in seconds	     p4 = amplitude		   ;
;	p5 = pitch in 8ve.pc		     p6 = rise in seconds	   ;
;	p7 = decay in seconds		     p8 = function # for oscili    ;
;	p9 = left channel factor (between 0 and 1) for stereo placement    ;
;      p10 = previous p5		    p11 = phrase dur in seconds    ;
;==========================================================================;

; bach chorale no. 228
; soprano function
f01        0   513    10     1     0   .33     0    .2     0   .15     0    .1
     0   .05
t00       60     0   120
; soprano part with legato on notes 2 - 6:
;      start   dur   amp   pch  rise   dec    fn  lfac  pch1  phrasedur
i04	   0	 1 20000  8.09	  .1	.1     1    .4	   0	0
i .        1     .     .  9.00     .     .     .     .     .    3
i .        2     .     .  8.11     .     .     .     .   pp5   -1
i .        3     .     .  9.00
i .        4     .     .  9.02
i .        5     2     .  9.04

f00	   9
s

;==========================================================================;
;			Delay Line Instrument				   ;
;                                                                          ;
;       p4 = ampfac     p5 = soundin#   p6 = maxdel     p7 = basedel       ;
;       p8 = pkvardel   p9 = vardelhz  p10 = vardelfn  p11 = feedfac       ;
;==========================================================================;

f01     0       513     10      1
;       start   dur     ampfac  soundin maxdel  basedel pkvar   varhz   varfn
i05	0	3	1	1	.25	.005	.004	2	1
;       feedfac srcfac  delfac
	0       .5      .5
i05	+	3	1	2	.25	.005	.004	2	1
	.25     .5      .5
i05	+	3	1	3	.25	.005	.004	2	1
	.5      .5      .5
i05	+	3	1	4	.25	.005	.004	2	1
	.9      .5      .5
i05	+	5	1	5	.25	.005	.004	2	1
	.5	.5	.5

f00     17

s

;==========================================================================;
;			   Vocoder Instrument				   ;
;==========================================================================;


;    start  dur    mod  car  skt   pdepth  pdel  prise  pdur  pdec  ptim noise

i06   0      4	     5	  4    0     .7      .5    .5	  1.8	1     2     6

f00  6

s

; sine wave for foscil
f01     0       512     10      1
; sequence function -- format:  pch1, amp1, dur1, pch2, amp2, dur2,...
f02     0       32      -2      8.00  .75   .1    7.01  .25   .1
        7.04  .3  .1  6.10  .25  .2  7.09 .5 .5
; interval function (with some locations zero for rests)
f03     0       16      -2      .01     -.01    .11     -.11    1.01    -1.01
       .06      -.06    .07     -.07    .05     -.05
; amp factor function (quarter sine wave)
f04	0	129	9	.25	1	0
; duration function 1
f05     0       8       -2      .5      .25     .25     .125    .125
        .125    .0625   .0625
; duration function 2
f06     0       8       -2      .5      .33333  .33333  .16667  .16667
        .16667  .08333  .08333
; envlpx rise func
f07     0       513     5       .01     512     1

;======================================================================;
;                     Basic "Sequencer" Instrument                     ;
;                                                                      ;
; p3 = sequence dur  p4 = peak amp       p5 = fno for note information ;
; p6 = overall rise  p7 = overall decay  p8 = note rise  p9 = note dec ;
; p10 = ndx rise p11 = ndx dec p12 = max ndx p13 = carfac p14 = modfac ;
; note info format: pch, amp, dur; pch, amp, dur; ...                  ;
;======================================================================;

;       st      dur     pkamp   fno     rise    decay   nrise   ndec
i7	0	1	20000  2	.01	.2	.1	.1
;       irise   idec    pkndx   carfac  modfac
        .2      .2      5       1       1

;======================================================================;
;               Controlled Random Sequence Instrument                  ;
;                                                                      ;
; p3 = sequence dur  p4 = duration fno p5 = interval fno  p6 = amp fno ;
; p7 = seq rise p8 = seq decay p9 = noteris p10 = notedec p11 = ndxmax ;
; p12 = pkamp p13 = carfac p14 = modfac p15 = dur seed p16 = pch seed  ;
; p17 = pan seed p18 = seed pitch                                      ;                                                             ;
;======================================================================;
;======================================================================;
;    This will generate a short musical abomination featuring the      ;
;     intervals 1,6,7. Change the seed values and/or contents of the   ;
;     dur, intvl, and amp fns for new and different abominations.      ;
;     Still, something good might come of it...?  Russell Pinkston     ;
;======================================================================;

;       st      dur     durfn   intfn   ampfn   seqrise seqdec
i8	3	5	5	3	4	.5	.5
;       notris  notdec  ndxmax  pkamp   carfac  modfac  durseed pchseed panseed seedpch
        .05    .25      4       16000   1       1       .010149 .071983 .022186 7.00
;       st      dur     durfn   intfn   ampfn   seqrise seqdec
i8	3	5	6	3	4      .5      .5
;       notris  notdec  ndxmax  pkamp   carfac  modfac  durseed pchseed panseed seedpch
        .1     .25      4       16000   3       2      .070703 .012719 .030251  8.00

f0  11
s

;====================================================================;
;		  Score for DX7 algorithm 16			     ;
;                                                                    ;
;	    (imitates the PLUK16.1 voice...sort of)		     ;
;====================================================================;
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
;f08     0       128     6       .001    96      .5      32      1
;f08	0	128	-6	 0	64	.0318	0	.0318	64	2.08795
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
;    p02 = start     p03 = dur	     p04 = pch	     p05 = vel	     ;
;    p06 = panfac    p07 = vibdel    p08 = vibwth    p09 = vibhz     ;
;    p10 = op1fn     p11 = op2fn     p12 = op3fn     p13 = op4fn     ;
;    p14 = op5fn     p15 = op6fn     p16 = ampfn     p17 = pkamp     ;
;    p18 = rsfn      p19 = devfn     p20 = erisfn    p21 = edecfn    ;
;    p22 = vsfn      p23 = velfn     p24 = feedfn    p25 = feedbk    ;
;====================================================================;
;  Play a short passage; all notes are turned on indefinitely, via   ;
;  the negative duration arg, but the absolute value of P3 is used   ;
;  within the instrument as the time from "note on" til "note off"   ;
;  ...which is when final decay begins.  Instrument 7 gets turned    ;
;  off at time 9 by the negative P2 of the penultimate card in the   ;
;  score.  All other instruments get turned of by the e-card.	     ;
;====================================================================;

i09	0    -.49   6.00    87	  0	  0	  0	  0
        12    13    14      15    16      17      2       4000
        3     8     4       6     10      9       11      7

i 9	1.00  -.54  6.07    87
i 9	2.00  -.16  6.08    75
i 9	2.49  -.20  7.03    90
i 9	3.01  -.20  7.04    74
i 9	3.50  -.18  7.11    90
i 9	4.01  -.15  8.00    75
i 9	4.24  -.13  8.07    69
i 9	4.50  -.07  8.08    72
i 9	4.72  -.09  9.03    90
i 9	4.98  -.16  9.00    79
i -9	   9				;allow for a 4-beat final decay.
e
