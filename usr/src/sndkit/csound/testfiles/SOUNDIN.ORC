;====================================================================;
;       Example Orchestra for playing SOUNDIN files                  ;
;====================================================================;

	sr	=	18900
	kr	=	1890
	ksmps	=	10
        nchnls  =       1

;===========================================;
;       Straight Mixing Instrument          ;
;                                           ;
;       p4 = File A     p5 = File B         ;
;       p6 = A rise     p7 = A decay        ;
;       p8 = B rise     p9 = B decay        ;
;===========================================;
                instr   1
                
        asiga   soundin p4
        asiga   linen   asiga,p6,p3,p7

        asigb   soundin p5
        asigb   linen   asigb,p8,p3,p9
   
                out     asiga+asigb
                endin

;===========================================;
;        Pitch Changing Instrument          ;
;                                           ;
;       p4 = Soundin #  p5 = desired pitch  ;
;       p6 = old pitch  p7 = original sr    ;
;===========================================;

                instr   2
        icpsnew =       cpspch(p5)
        icpsold =       cpspch(p6)
        ioldsr  =       p7
        incr    =       ioldsr/sr * icpsnew/icpsold
        kphase  init    0                       ;initialize phase
        aphase  interp  kphase                  ;convert to arate
        asig    tablei  aphase,1                ;resample the sound
        kphase  =       kphase+incr*ksmps       ;update for next k
                out     asig
                endin

