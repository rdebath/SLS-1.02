;==========================================================================;
;                Schottstaedt FM String Instrument from Dodge              ;
;                                                                          ;
;p4 = amp p5 = pch p6 = rise p7 = dec p8 = vibdel p9 = vibwth p10 = vibrte ;
;==========================================================================;
        sr      =       16000
        kr      =       1000
        ksmps   =       16
        nchnls  =       1

                instr   1
        iamp    =       p4
        ifc     =       cpspch(p5)      ;S = fc +- ifm1 +- kfm2 +- lfm3
        ifm1    =       ifc
        ifm2    =       ifc*3
        ifm3    =       ifc*4
        indx1   =       7.5/log(ifc)    ;range from ca 2 to 1
        indx2   =       15/sqrt(ifc)    ;range from ca 2.6 to .5
        indx3   =       1.25/sqrt(ifc)  ;range from ca .2 to .038
        kvib    init    0                

                timout  0,p8,transient  ;delays vibrato for p8 seconds
        kvbctl  linen   1,.5,p3-p8,.1   ;vibrato control envelope
        krnd    randi   .0075,15        ;random deviation in vib width        
        kvib    oscili  kvbctl*p9+krnd,p10*kvbctl,1 ;vibrato generator
        
transient:
        timout  .2,p3,continue          ;execute for .2 secs only
        ktrans  linseg  1,.2,0,1,0      ;transient envelope 
        anoise  randi   ktrans,.2*ifc   ;noise... 
        attack  oscil   anoise,2000,1   ;...centered around 2kHz

continue:      
        amod1   oscili  ifm1*(indx1+ktrans),ifm1,1
        amod2   oscili  ifm2*(indx2+ktrans),ifm2,1
        amod3   oscili  ifm3*(indx3+ktrans),ifm3,1
        asig    oscili  iamp,(ifc+amod1+amod2+amod3)*(1+kvib),1
        asig    linen   asig+attack,p6,p3,p7
                out     asig
        
                endin
