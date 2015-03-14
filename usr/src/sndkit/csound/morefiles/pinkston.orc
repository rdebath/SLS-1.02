;			  Nine Csound Instruments			
;			       designed by				  
; Professor Russell Pinkston at the University of Texas in Austin       
 
        sr      =       16000
       kr      =       1000
       ksmps   =       16
       nchnls  =       2

;                    Portamento/Panning Instrument                   ;               
; p4=amp  p5=pch p6=portsize (oct)  p7=delay  p8=ofn  p9=p3 in beats 

instr	1
   ipitch      =       octpch(p5)                 ;center pitch in oct
   itempo      =       p3/p9                      ;ratio seconds/beats
   idelay      =       p7*itempo                  ;cnvt beats to secs
   iport       =       p6                         ;p6 in oct
   ilfn        =       (iport > 0 ? 4 : 5)        ;fn 4 = 1/4 sine
   irfn        =       (iport > 0 ? 5 : 4)        ;fn 5 = 1/4 cos

   kport       init    0                          ;initialize kvars
   kleft       init    .707                       ;sqrt(.5) for mono
   kright      init    .707  
 
               if      (iport == 0) goto continue ;skip if not needed

   kport       oscil1i  idelay,iport,p3-idelay,2  ;fn 2 is linear ramp
   kleft       oscil1i  idelay,1,p3-idelay,ilfn  
   kright      oscil1i  idelay,1,p3-idelay,irfn

continue:
   kgate       envlpx  p4,p3*.1,p3,p3/4,3,1,.01   ;fn 3 is exponential
  
   asig        oscili  kgate,cpsoct(ipitch+kport),p8
               outs    asig*kleft,asig*kright     ;panning
endin

 ;                 Simple Gating Instrument with Chorus                                                                                            ; p4=amp      p5=pch1       p6=pch2       p7=risefac      p8=decfac       
 ; p9=ofn1    p10=ofn2      p11=gatefn    p12=beathz      p13=gatehz       

instr   2
   igatehz      =       (p13 == 0 ? 1/p3 : p13)   ;default to once per note
   ihalfamp     =       p4/2
   ipitch1      =       cpspch(p5)
   ipitch2      =       cpspch(p6)
   ibeatfreq    =       p12     
   idetune2     =       ipitch2 + ibeatfreq

   kgate        oscili  1,igatehz,p11            ;p11 has gating control fn#
   kenvlp       linen   1,p3*p7,p3,p3*p8      ;p7,p8 are rise, decay facs

   asig1        oscili  p4,ipitch1,p9            ;sound one
   astraight2   oscili  ihalfamp,ipitch2,p10     ;straight sound two
   adetune2     oscili  ihalfamp,idetune2,p10    ;detuned sound two
   asig2        =       astraight2 + adetune2    ;sound two 

   aout1        =       asig1 * kgate
   aout2        =       asig2 * (1-kgate)
   aoutsig      =       (aout1 + aout2) * kenvlp ;output the sum         
                
                outs    aoutsig,aoutsig
endin

;               Basic FM Instrument with Variable Vibrato                 
;  p4=amp       p5=pch(fund)    p6=vibdel       p7=vibrate      p8=vibwth  ; p9=rise     p10=decay    p11=max index   p12=car fac   p13=mod fac ; p14=index rise  p15=index dec  p16=left channel fac p17=orig p3 

instr   3
;------------------------------------------------;initialization block:
        kpitch  init    cpspch (p5)     

        itempo  =       p3/p17                   ;ratio seconds/beats
        idelay  =       p6 * itempo              ;convert beats to secs
        irise   =       p9 * itempo
        idecay  =       p10 * itempo
        indxris =       (p14 == 0 ? irise : p14 * itempo)
        indxdec =       (p15 == 0 ? idecay : p15 * itempo)
               
                if      (p16 != 0) igoto panning ;if a panfac, use it, else
        ilfac   =       .707                     ;default is mono (sqrt(.5))
        irfac   =       .707    
                igoto   perform  
panning:        
        ilfac   =       sqrt(p16)        
        irfac   =       sqrt(1-p16)
;------------------------------------------;performance block:
perform:
                if      (p7 == 0 || p8 == 0) goto continue
        kontrol oscil1  idelay,1,.5,2            ;vib control
        kvib    oscili  p8*kontrol,p7*kontrol,1  ;vibrato unit
        kpitch  =       cpsoct(octpch(p5)+kvib)  ;varying fund pitch in hz
continue:
        kamp    linen   p4,irise,p3,idecay
        kindex  linen   p11,indxris,p3,indxdec
 
        asig    foscili kamp,kpitch,p12,p13,kindex,1  										;p12=carfac,p13=modfac
                outs    asig*ilfac,asig*irfac
endin

;                      -- THE BACH BOX --                             ;
;	This is a basic orchestra for playing a Bach Chorale.	      ;
;	       It is a simple gated sine generator with an optional   ;
; "legato" function, which utilizes the pp (previous pfield) feature  ;
; of the score to find the previous pitch played and generate a short ;
; portamento to the new pitch at the beginning of each note.  This    ;
; results in a vocal-style legato, as opposed to a piano-style legato ;
; which consists of simply overlapping starting and ending times of   ; successive notes.  (The latter is probably more appropriate in most ; cases, by the way, but here's how to do it like a bad singer.)      ;
;   p3 = duration in seconds             p4 = amplitude               ;
;   p5 = pitch in 8ve.pc                 p6 = rise in seconds         ;
;   p7 = decay in seconds                p8 = function # for oscili   ;
;   p9 = left channel factor (between 0 and 1) for stereo placement   ;
;  p10 = previous p5                    p11 = phrase dur in seconds   ;

instr	4
;-----------------------------------------;initialization block:
        idur    =       (p11 != 0 ? p11 : p3)   ;phrase duration
        iphase  =       (idur > 0 ? 0 : -1)     ;neg phase -> skip init
        ioct    =       octpch(p5)              ;current pitch
        ioct1   =       (p10 != 0 ? octpch(p10) : ioct)  ;previous pitch       
                
                if      (p9 != 0) igoto panning ;if a panfac, use it, else
        ilfac   =       .707                    ;default is mono (sqrt(.5))
        irfac   =       .707    
                igoto   continue  
panning:        
        ilfac   =       sqrt(p9)        
        irfac   =       sqrt(1-p9)
;------------------------------------------;performance block:
continue:
        kgate   linen   p4,p6,idur,p7           ;update gate at control rate

        koct    linseg  ioct1,.02,ioct,1,ioct   ;port from ioct1 to ioct
        asig    oscili  kgate,cpsoct(koct),p8,iphase ;tone at audio rate
                outs    asig*ilfac,asig*irfac   ;stereo placement and out
endin

;			Delay Line Instrument				   ;
;       p4 = ampfac     p5 = soundin#   p6 = maxdel     p7 = basedel       ;
;       p8 = pkvardel   p9 = vardelhz  p10 = vardelfn  p11 = feedfac       

instr	5
        iampfac =       (p4 == 0 ? 1 : p4)
        imaxdel =       p6
        ibase   =       p7
        ivary   =       p8
        ivarhz  =       p9
        ivarfn  =       p10
        ifeed   =       p11
        isrcfac =       (p12 == 0 ? 1 : p12)
        idelfac =       (p13 == 0 ? 1 : p13)

        avarsig init    0
 
	avardel oscili  ivary,ivarhz,ivarfn
	avardel =       ibase+avardel
        asource soundin p5
 
        ainsig  =       asource+avarsig*ifeed
       
	adelsig delayr  imaxdel
	avarsig deltapi avardel
		delayw  ainsig
	aoutsig =       asource*isrcfac+avarsig*idelfac

		outs	 aoutsig*iampfac,aoutsig*iampfac
endin

;			Vocodor instrument			       ;
;	p4 = modulator file	   p9 = pan rise		       ;
;	p5 = carrier file	   p10 = pan dur		       ;
;	p6 = skptime		   p11= pan decay		       ;
;	p7 = pan depth		   p12= pan time		       ;
;	p8 = pan delay		   p13= noise factor		       ;

instr	6

        iq      =       75
        icf1    =       140
        icf2    =       180
        icf3    =       270
        icf4    =       360
        icf5    =       440
        icf6    =       540
        icf7    =       650
        icf8    =       790
        icf9    =       960
        icf10   =       1160
        icf11   =       1400
        icf12   =       1700
        icf13   =       2050
        icf14   =       2700
        icf15   =       4000
        icf16   =       5400

        imodrise =      .07
        imodec  =       .07
        icarise =       .07
        icardec =       .07    

        iskptim =       p6
        ipandpth =      p7 
        ipandel =       p8 
        ipanrise =      p9 
        ipandur =       p10
        ipandec =       p11
        ipantm  =       p12
        inoise  =       p13

	ivoicescale    =  6
;----------------       soundin 

        asigm   soundin p4                      ;voice
	asigm	linen	asigm*ivoicescale,imodrise,p3,imodec

        asigc   soundin p5,iskptim              ;source
        asigc   linen   asigc,icarise,p3,icardec

        if (inoise==0) kgoto pan1               ;inject noise into system

        anoise  rand    3000*inoise
        asigc   =       asigc+anoise 

;----------------       panning
pan1:
                timout 0,ipandel,filters
        kctl    linen   .5,ipanrise,ipandur,ipandec
        kpan    randi   kctl*ipandpth,(1/ipantm)*kctl,.5
        kpan = abs(kpan)
;----------------       bank 'o filters

filters:
        asigm1  reson   asigm,icf1,icf1/iq,1
        asigc1  reson   asigc,icf1,icf1/iq,1
        asig1   balance asigc1,asigm1

        asigm2  reson   asigm,icf2,icf2/iq,1
        asigc2  reson   asigc,icf2,icf2/iq,1
        asig2   balance asigc2,asigm2

        asigm3  reson   asigm,icf3,icf3/iq,1
        asigc3  reson   asigc,icf3,icf3/iq,1
        asig3   balance asigc3,asigm3

        asigm4  reson   asigm,icf4,icf4/iq,1
        asigc4  reson   asigc,icf4,icf4/iq,1
        asig4   balance asigc4,asigm4

        asigm5  reson   asigm,icf5,icf5/iq,1
        asigc5  reson   asigc,icf5,icf5/iq,1
        asig5   balance asigc5,asigm5

        asigm6  reson   asigm,icf6,icf6/iq,1
        asigc6  reson   asigc,icf6,icf6/iq,1
        asig6   balance asigc6,asigm6

        asigm7  reson   asigm,icf7,icf7/iq,1
        asigc7  reson   asigc,icf7,icf7/iq,1
        asig7   balance asigc7,asigm7

        asigm8  reson   asigm,icf8,icf8/iq,1
        asigc8  reson   asigc,icf8,icf8/iq,1
        asig8   balance asigc8,asigm8

        asigm9  reson   asigm,icf9,icf9/iq,1
        asigc9  reson   asigc,icf9,icf9/iq,1
        asig9   balance asigc9,asigm9

        asigm10 reson   asigm,icf10,icf10/iq,1
        asigc10 reson   asigc,icf10,icf10/iq,1
        asig10  balance asigc10,asigm10

        asigm11 reson   asigm,icf11,icf11/iq,1
        asigc11 reson   asigc,icf11,icf11/iq,1
        asig11  balance asigc11,asigm11                               

        asigm12 reson   asigm,icf12,icf12/iq,1
        asigc12 reson   asigc,icf12,icf12/iq,1
        asig12  balance asigc12,asigm12

        asigm13 reson   asigm,icf13,icf13/iq,1
        asigc13 reson   asigc,icf13,icf13/iq,1
        asig13  balance asigc13,asigm13

        asigm14 reson   asigm,icf14,icf14/iq,1
        asigc14 reson   asigc,icf14,icf14/iq,1
        asig14  balance asigc14,asigm14

        asigm15 reson   asigm,icf15,icf15/iq,1
        asigc15 reson   asigc,icf15,icf15/iq,1
        asig15  balance asigc15,asigm15

        asigm16 reson   asigm,icf16,icf16/iq,1
        asigc16 reson   asigc,icf16,icf16/iq,1
        asig16  balance asigc16,asigm16

asigt1  = (asig1+asig2+asig3+asig4+asig5+asig6+asig7+asig8)  ;sum up signal
asigt2  = (asig9+asig10+asig11+asig12+asig13+asig14+asig15+asig16)        
asigt   = asigt1+asigt2

;----------------       voice/unvoiced logic

        asigma  tone    asigm,500      ;LP filter
        asigmb  tone    asigma,500
         

        asigmc  atone   asigm,1000      ;HP filter
        asigmd  atone   asigmc,1000
         
        ksiglp  rms     asigmb
        ksighp  rms     asigmd

        if (ksiglp > ksighp) kgoto pan2

        asigt  =       asigt+asigm      ;if it is voiced, add the 
                                        ;the voice to the filtered
                                        ;sound
        
;---------------        more panning
pan2:
        klfac   =       sqrt(.5+kpan)          ;left

        krfac   =       sqrt(.5-kpan)          ;right

        asigt   balance asigt,asigm

        asigl   = klfac*asigt
        asigr   = krfac*asigt   

;------------------     finally send it out

                outs    asigl,asigr
endin

;                     Basic "Sequencer" Instrument                     ;
;                                                                      ;
; p3 = sequence dur  p4 = peak amp       p5 = fno for note information ;
; p6 = overall rise  p7 = overall decay  p8 = note rise  p9 = note dec ;
; p10 = ndx rise p11 = ndx dec p12 = max ndx p13 = carfac 
; p14 = modfac 
; note info format: pch, amp, dur; pch, amp, dur; ...                  ;

instr   7
;-----------------------------------------;initialization block:
        ipkamp  =       p4
        iseqfn  =       p5
        irise   =       p6
        idecay  =       p7
        inrise  =       p8
        indec   =       p9
        indxris =       (p10 == 0 ? inrise : p10)
        indxdec =       (p11 == 0 ? indec : p11)
        indxmax =       p12
        icfac   =       p13
        imfac   =       p14
        inext   =       0                       ;initialize table index
noteinit:
        ipch    table   inext,iseqfn
        iampfac table   inext+1,iseqfn
        idur    table   inext+2,iseqfn
icps    =       cpspch(ipch)
        iamp    =       ipkamp*iampfac
        indx    =       indxmax*iampfac         ;map index to amp
        inext   =       inext + 3               ;set for next note
                print   ipch,iamp,idur,icps,indx
        kenv    linen   iamp,inrise*idur,idur,indec*idur
        kindex  linen	indx,indxris*idur,idur,indxdec*idur
        asig    foscili kenv,icps,icfac,imfac,kindex,1
                outs    asig,asig
                timout  0,idur,continue
                reinit  noteinit
                rireturn
continue:
                endin

;               Controlled Random Sequence Instrument                  ;
; p3 = seq dur  p4 = dur fno   p5 = pch fno   p6 = amp fno             ;
; p7 = seq rise p8 = seq decay p9 = noteris p10 = notdec p11 = ndxmax 
; p12 = pkamp p13 = carfac p14 = modfac p15 = durseed p16=pch seed  ; p17 = pan seed p18 = seed pch                                        ;

instr	8
;-----------------------------------------;initialization block:
	idurfn	=	p4
        iintfn  =       p5
	iampfn	=	p6
	isrise	=	p7			;sequence rise time
	isdec	=	p8 			;sequence decay time
 	inrise	=	p9 			;note amp and ndx rise
	indec 	=	p10			;note amp and ndx decay
	ipkndx	=	p11
	ipkamp	=	p12
	icfac	=	p13
	imfac	=	p14
	iseed1	=	p15
	iseed2	=	p16
	iseed3	=	p17
        ioct    =       (p18 == 0 ? 8.00 : octpch(p18))  ;seed pitch
	ibase	=	6.00			;lowest expected pch
        irange  =       11.00 - ibase           ;range of expected pchs
        ipkdur  =       .5001                   ;used in amp mapping
;-----------------------------------------------------------;
	kphrase	linen	1,isrise,p3,isdec	;phrase envelope
;-----------------------------------------------------------;
noteinit:
        kdurloc rand    .5,iseed1               ;-.5 < kdurloc < +.5
        idurloc =       i(kdurloc)+.5           ;force an i-time value
        idur    table   idurloc,idurfn,1        ;select from dur table
        iamp    table   idur/ipkdur,iampfn,1    ;relate amp to dur
        kintloc rand    .5,iseed2               ;-.5 < kintloc < +.5
        iintloc =       i(kintloc)+.5           ;= 0 < kintloc < 1
        intrvl  table   iintloc,iintfn,1        ;get interval to next pitch
        ioct    =       ioct + octpch(intrvl)   ;convert to oct notation
        icps    =       cpsoct(ioct)            ;and cps for foscil
        kpan    rand    .5,iseed3               ;-.5 < kpan < +.5
        ipan    =       i(kpan)+.5
        iseed1  =       -1              ;after first note, don't restart rands
        iseed2  =       -1
        iseed3  =       -1

        ilfac   =       ipan
        ileft   =       sqrt(ilfac)
        iright  =       sqrt(1-ilfac)
        indx    =       (irange-(ioct-ibase))*ipkndx ;map ndx inv of pch
continue:
                if      (intrvl == 0) goto rest
        knote   envlpx  1,inrise*idur,idur,indec*idur,7,.5,.01,-.9
        asig    foscili knote*iamp*ipkamp,icps,icfac,imfac,knote*indx,1
        asig    =       asig*kphrase
                outs    asig*ileft,asig*iright
rest:           timout  0,idur,exit             ;wait idur secs, then reinit...
                reinit  noteinit
                print   idur,iamp,intrvl,ioct,ipan
                rireturn
exit:
                endin

;                       Yamaha DX7 Algorithm 16                      ;
;    p02 = start     p03 = dur	     p04 = pch	     p05 = vel	     ;
;    p06 = panfac    p07 = vibdel    p08 = vibwth    p09 = vibhz     ;
;    p10 = op1fn     p11 = op2fn     p12 = op3fn     p13 = op4fn     ;
;    p14 = op5fn     p15 = op6fn     p16 = ampfn     p17 = pkamp     ;
;    p18 = rsfn      p19 = devfn     p20 = erisfn    p21 = edecfn    ;
;    p22 = vsfn      p23 = velfn     p24 = feedfn    p25 = feedbk    ;

instr	9
;               ihold                           ;not available in mtu csound...
        koff    init    0                       ;...use off flag instead, and
                                                ;use negative p3s in score.
        idur    =       abs(p3)
        ibase   =       cpspch(p4)              ;p4 is keyboard pitch
        iroct   =       octpch(p4)
        irbase  =       octpch(4.09)            ;base of rate scl table
        irrange =       octpch(13.06)-irbase
        iveloc  =       p5                      ;0 <= p5 <= 127
        iop1fn  =       p10                     ;param tables for ops
        iop2fn  =       p11
        iop3fn  =       p12
        iop4fn  =       p13
        iop5fn  =       p14
        iop6fn  =       p15
        iampfn  =       p16                     ;amp/level map function
        ipkamp  =       p17                     ;scale for converter
        irsfn   =       p18                     ;rate scaling function
        idevfn  =       p19                     ;level/pkdev map func
        irisefn =       p20                     ;eg rise rate fn
        idecfn  =       p21                     ;eg decay rate fn
        ivsfn   =       p22                     ;vel sensitivity fn
        ivelfn  =       p23                     ;vel/amp fac map fn
        iveloc  table   iveloc,ivelfn           ;map this note's veloc
        iveloc  =       iveloc * 1.10           ;range 0 -> 110 %
        ifeedfn =       p24
        ifeed   table   p25,ifeedfn             ;0 <= p25 <= 7 (feedbk)
        ifeed   =       ifeed/(2 * 3.14159)     ;dev in radians
        idetfac =       4                       ;max detuning divisor
        imap128 =       127/99                  ;mapping constant 99->127
        irscl   table   (iroct-irbase)/irrange*127,irsfn
        irscl   =       irscl*6
        iop     =       1                       ;start loop with op1
        iopfn   =       iop1fn
loop:
;---------------------------------read operator parameters
        ilvl    table   0,iopfn
        ivel    table   1,iopfn
        iegr1   table   2,iopfn
        iegr2   table   3,iopfn
        iegr3   table   4,iopfn
        iegr4   table   5,iopfn
        iegl1   table   6,iopfn
        iegl2   table   7,iopfn
        iegl3   table   8,iopfn
        iegl4   table   9,iopfn
        iams    table   10,iopfn
        imode   table   11,iopfn
        ifreq   table   12,iopfn
        idet    table   13,iopfn
        irss    table   14,iopfn
;----------------------------------initialize operator
        ihz     =       (imode > 0 ? ifreq : ibase * ifreq) + idet/idetfac
;       iamp    table   ilvl,iampfn
 iamp	=	ilvl/99		;rescale to 0 -> 1
        ivfac   table   ivel,ivsfn

        iegl1   =       iamp*iegl1
        iegl2   =       iamp*iegl2
        iegl3   =       iamp*iegl3
        iegl4   =       iamp*iegl4

        iegl1   =       iegl1*(1-ivfac)+iegl1*ivfac*iveloc
        iegl2   =       iegl2*(1-ivfac)+iegl2*ivfac*iveloc
        iegl3   =       iegl3*(1-ivfac)+iegl3*ivfac*iveloc
        iegl4   =       iegl4*(1-ivfac)+iegl4*ivfac*iveloc

        irs     =       irscl*irss
        iegr1   =       (iegr1+irs > 99 ? 99 : iegr1+irs)
        iegr2   =       (iegr2+irs > 99 ? 99 : iegr2+irs)
        iegr3   =       (iegr3+irs > 99 ? 99 : iegr3+irs)
        iegr4   =       (iegr4+irs > 99 ? 99 : iegr4+irs)

        irfn    =       (iegl1 > iegl4 ? irisefn : idecfn)
        iegd1   table   iegr1,irfn               ;convert rate->dur
        ipct1   table   iegl4,irfn+1             ;pct fn is next one
        ipct2   table   iegl1,irfn+1
        iegd1   =       abs(iegd1*ipct1-iegd1*ipct2)
        iegd1   =       (iegd1 == 0 ? .001 : iegd1)

        irfn    =       (iegl2 > iegl1 ? irisefn : idecfn)
        iegd2   table   iegr2,irfn
        ipct1   table   iegl1,irfn+1
        ipct2   table   iegl2,irfn+1
        iegd2   =       abs(iegd2*ipct1-iegd2*ipct2)
        iegd2   =       (iegd2 == 0 ? .001 : iegd2)

        irfn    =       (iegl3 > iegl2 ? irisefn : idecfn)
        iegd3   table   iegr3,irfn
        ipct1   table   iegl2,irfn+1
        ipct2   table   iegl3,irfn+1
        iegd3   =       abs(iegd3*ipct1-iegd3*ipct2)
        iegd3   =       (iegd3 == 0 ? .001 : iegd3)

        iegd4   table   iegr4,idecfn
                if      (iegl3 <= iegl4) igoto continue
        ipct1   table   iegl3,irfn+1
        ipct2   table   iegl4,irfn+1
        iegd4   =       abs(iegd4*ipct1-iegd4*ipct2)
        iegd4   =       (iegd4 == 0 ? .001 : iegd4)
continue:
                if      (iop > 1) igoto op2
op1:
        i1egd1  =       iegd1
        i1egd2  =       iegd2
        i1egd3  =       iegd3
        i1egd4  =       iegd4
        i1egl1  =       iegl1
        i1egl2  =       iegl2
        i1egl3  =       iegl3
        i1egl4  =       iegl4
        i1ams   =       iams
        i1hz    =       ihz
        iop     =       iop + 1
        iopfn   =       iop2fn
                igoto   loop

op2:            if      (iop > 2) igoto op3
        i2egd1  =       iegd1
        i2egd2  =       iegd2
        i2egd3  =       iegd3
        i2egd4  =       iegd4
        i2egl1  =       iegl1
        i2egl2  =       iegl2
        i2egl3  =       iegl3
        i2egl4  =       iegl4
        i2ams   =       iams
        i2hz    =       ihz
        iop     =       iop + 1
        iopfn   =       iop3fn
                igoto   loop

op3:            if      (iop > 3) igoto op4
        i3egd1  =       iegd1
        i3egd2  =       iegd2
        i3egd3  =       iegd3
        i3egd4  =       iegd4
        i3egl1  =       iegl1
        i3egl2  =       iegl2
        i3egl3  =       iegl3
        i3egl4  =       iegl4
        i3ams   =       iams
        i3hz    =       ihz
        iop     =       iop + 1
        iopfn   =       iop4fn
                igoto   loop

op4:            if      (iop > 4) igoto op5
        i4egd1  =       iegd1
        i4egd2  =       iegd2
        i4egd3  =       iegd3
        i4egd4  =       iegd4
        i4egl1  =       iegl1
        i4egl2  =       iegl2
        i4egl3  =       iegl3
        i4egl4  =       iegl4
        i4ams   =       iams
        i4hz    =       ihz
        iop     =       iop + 1
        iopfn   =       iop5fn
                igoto   loop

op5:            if      (iop > 5) igoto op6
        i5egd1  =       iegd1
        i5egd2  =       iegd2
        i5egd3  =       iegd3
        i5egd4  =       iegd4
        i5egl1  =       iegl1
        i5egl2  =       iegl2
        i5egl3  =       iegl3
        i5egl4  =       iegl4
        i5ams   =       iams
        i5hz    =       ihz
        iop     =       iop + 1
        iopfn   =       iop6fn
                igoto   loop

op6:
        i6egd1  =       iegd1
        i6egd2  =       iegd2
        i6egd3  =       iegd3
        i6egd4  =       iegd4
        i6egl1  =       iegl1
        i6egl2  =       iegl2
        i6egl3  =       iegl3
        i6egl4  =       iegl4
        i6ams   =       iams
        i6hz    =       ihz
;===========================================================
                if      koff > 1 kgoto exit   ;use this for mtu csound
                timout  idur,999,final      ;skip during final decay
        k1sus   linseg  i1egl4,i1egd1,i1egl1,i1egd2,i1egl2,i1egd3,i1egl3,1,i1egl3
        k2sus   linseg  i2egl4,i2egd1,i2egl1,i2egd2,i2egl2,i2egd3,i2egl3,1,i2egl3
        k3sus   linseg  i3egl4,i3egd1,i3egl1,i3egd2,i3egl2,i3egd3,i3egl3,1,i3egl3
        k4sus   linseg  i4egl4,i4egd1,i4egl1,i4egd2,i4egl2,i4egd3,i4egl3,1,i4egl3
        k5sus   linseg  i5egl4,i5egd1,i5egl1,i5egd2,i5egl2,i5egd3,i5egl3,1,i5egl3
        k6sus   linseg  i6egl4,i6egd1,i6egl1,i6egd2,i6egl2,i6egd3,i6egl3,1,i6egl3
        k1phs   =       k1sus
        k2phs   =       k2sus
        k3phs   =       k3sus
        k4phs   =       k4sus
        k5phs   =       k5sus
        k6phs   =       k6sus
                kgoto   output
final:
        k1fin   linseg  1,i1egd4,0,1,0
                if      k1fin > 0 kgoto perform
;               turnoff                             ;n/a in mtu version
        koff    =       1 ;                         ;use off flag instead
perform:        
        k1phs   =       i1egl4+(k1sus-i1egl4)*k1fin
        k2fin   linseg  1,i2egd4,0,1,0
        k2phs   =       i2egl4+(k2sus-i2egl4)*k2fin
        k3fin   linseg  1,i3egd4,0,1,0
        k3phs   =       i3egl4+(k3sus-i3egl4)*k3fin
        k4fin   linseg  1,i4egd4,0,1,0
        k4phs   =       i4egl4+(k4sus-i4egl4)*k4fin
        k5fin   linseg  1,i5egd4,0,1,0
        k5phs   =       i5egl4+(k5sus-i5egl4)*k5fin
        k6fin   linseg  1,i6egd4,0,1,0
        k6phs   =       i6egl4+(k6sus-i6egl4)*k6fin
output:
	k1gate 	tablei	k1phs,iampfn
	k2gate	tablei	k2phs,idevfn
 	k3gate	tablei	k3phs,idevfn
 	k4gate	tablei	k4phs,idevfn
 	k5gate	tablei	k5phs,idevfn
 	k6gate	tablei	k6phs,idevfn
	
        a6sig   init    0               ;initialize for feedback
        a6phs   phasor  i6hz
        a6sig   tablei  a6phs+a6sig*ifeed,1,1,0,1
        a6sig   =       a6sig*k6gate

        a5phs   phasor  i5hz
        a5sig   tablei  a5phs+a6sig,1,1,0,1
        a5sig   =       a5sig*k5gate

        a4sig   oscili  k4gate,i4hz,1

        a3phs   phasor  i3hz
        a3sig   tablei  a3phs+a4sig,1,1,0,1
        a3sig   =       a3sig*k3gate

        a2sig   oscili  k2gate,i2hz,1

        a1phs   phasor  i1hz
        a1sig   tablei  a1phs+a2sig+a3sig+a5sig,1,1,0,1
        a1sig   =       a1sig*k1gate
	aout	=	a1sig*ipkamp
		outs  aout,aout
exit:
endin 
