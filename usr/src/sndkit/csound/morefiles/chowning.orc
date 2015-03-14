; =========================================================================== ;
;									      ;
;			    Three FM Instruments			      ;
;				    by					      ;
;			       John M. Chowning 			      ;
;									      ;
; =========================================================================== ;

   sr = 16000
   kr = 1000
   ksmps = 16
   nchnls = 1

; =========================================================================== ;
;			     Single Carrier FM				      ;
; =========================================================================== ;
;    Specific values and basic design come from John Chowning's article       ;
;  "The Synthesis of Complex Audio Spectra by Means of Frequency Modulation"  ;
; =========================================================================== ;
;	   coded by Richard Boulanger - Berklee College of Music	      ;
; =========================================================================== ;
; Selected from the Csound Anthology of Instruments, Orchestras, and Scores   ; 									   ;
;		 compiled and edited by Dr. Richard Boulanger		      ;
; =========================================================================== ;

instr 1
   ; p4  = amplitude of output wave
   ; p5  = carrier frequency in Hz
   ; p6  = modulating frequency in Hz
   ; p7  = modulation index 1
   ; p8  = modulation index 2
   ; p9  = carrier envelope function
   ; p10 = modulator envelope function

   i1 = 1/p3			     ; one cycle per duration of note
   i2 = p7 * p6 		     ; calculates deviation for index 1
   i3 = (p8-p7) * p6		     ; calculates deviation for index 2

   ampcar  oscil   p4,i1,p9	     ; amplitude envelope for the carrier
   ampmod  oscil   i3,i1,p10	     ; amplitude envelope for the modulator

   amod    oscili  ampmod+i2,p6,1    ; modulating oscillator
   asig    oscili  ampcar,p5+amod,1  ; carrier oscillator
        out     asig
endin


; =========================================================================== ;
;			     Double Carrier FM				      ;
; =========================================================================== ;
;    Places a formant peak in the Spectrum: from John Chowning's article      ;
;  "The Synthesis of Complex Audio Spectra by Means of Frequency Modulation"  ;
; =========================================================================== ;
;	   coded by Richard Boulanger - Berklee College of Music	      ;
; =========================================================================== ;
; Selected from the Csound Anthology of Instruments, Orchestras, and Scores   ; 									   ;
;		 compiled and edited by Dr. Richard Boulanger		      ;
; =========================================================================== ;

instr 2
   ; p4  = amplitude of output wave
   ; p5  = carrier frequency specified in Hz
   ; p6  = modulating frequency specified in Hz
   ; p7  = modulation index 1
   ; p8  = modulation index 2
   ; p9  = carrier envelope function
   ; p10 = modulator envelope function
   ; p11 = amplitude scaler for second carrier
   ; p12 = modulation index scaler for second carrier
   ; p13 = frequency of second carrier specified in Hz

   i1 = 1/p3				      ; one cycle per duration of note
   i2 = p7 * p6 			      ; calculates deviation for index 1
   i3 = (p8-p7) * p6			      ; calculates deviation for index 2

   ampcar  oscil   p4,i1,p9		      ; amp envelope for the carrier
   ampmod  oscil   i3,i1,p10		      ; amp envelope for the modulator

   amod    oscili  ampmod+i2,p6,1	      ; modulating oscillator
   asig1   oscili  ampcar,p5+amod,1	      ; carrier oscillator
   asig2   oscili  ampcar*p11,p13+(amod*p12),1; second carrier oscillator
	   out	   asig1+asig2
endin

;=========================================================================;
;									  ;
;		     Chowning's FM Soprano Instrument                     ;
;									  ;
;=========================================================================;
;  Coded by Professor Russell Pinkston - University of Texas at Austin	  ;
;=========================================================================;
; from the Csound Anthology of Instruments, Orchestras, and Scores (1990) ;
;	       compiled and edited by Dr. Richard Boulanger		  ;
;=========================================================================;


instr	3

;**********************initialization...
        iamp    =       p6                      ;range 0 < AMP < 1
        ifenvfn =       p7
        iportfn =       p8
        ifmtfn1 =       p9
        ifmtfn2 =       p10
        ifmtfn3 =       p11
        ifundfn =       p12
        irange  =       3                       ;max range of 3 octaves
        ibase   =       octpch(7.07)            ;lowest note = g3
        icaroct =       octpch(p5)
        icarhz  =       cpspch(p5)
        ipoint  =       (icaroct-ibase)/irange*511.999

        ifmthz  table   ipoint,ifmtfn1          ;relative pos. of formant
        ifmthz  =       ifmthz*3000             ;map onto frequency range
        ifmthz  =       int(ifmthz/icarhz+.5)*icarhz ;as nearest harmonic

        ifmtfac table   ipoint,ifmtfn2          ;relative amp. of formant
        ifmtfac =       ifmtfac*.1              ;max value = .1
        ifndfac =       1-ifmtfac               ;relative amp. of fund

        ifmtndx table   ipoint,ifmtfn3          ;relative index of formant
        ifmtndx =       ifmtndx*5               ;max value = 5

        ifndndx table   ipoint,ifundfn          ;relative index of fund
        ifndndx =       ifndndx*.25             ;max value = .25

        ifndamp =       ifndfac * sqrt(iamp)            ;AMP**.5 
        ifmtamp =       ifmtfac * iamp * sqrt(iamp)     ;AMP**1.5

        imodhz  =       icarhz                  ;calculate modulator and
        ipkdev1 =       ifndndx*imodhz          ;peak deviation

; compute vibrato parameters:
        ilog2pch =      log(icarhz)/log(2)      
        ivibwth  =      .002*ilog2pch           ;relate width to fund pch
        ivibhz   =      5                       ;from 5 to 6.5 hz average
        irandhz  =      125                     ;from morrill trumpet design
        iportdev =      .05                     ; "     "       "      "

;************************* performance...
; vibrato
        krand   randi   ivibwth,irandhz
        kvibwth linen   ivibwth,.6,p3,.1        ;gate vibrato width
        kport   oscil1  0,iportdev,.2,iportfn   ;initial portamento
        kvib    oscili  kvibwth,ivibhz,1        ;fn1 = sine
        kv      =       1+kvib+kport+krand      ;vibrato factor always ca 1
; fm
        adev1   oscili  ipkdev1,imodhz*kv,1     ;modulator
        adev2   =       adev1*ifmtndx/ifndndx   ;rescale for formant carrier
        afundhz =       (icarhz+adev1)*kv       ;vib the modulated fund...
        aformhz =       (ifmthz+adev2)*kv       ;...and modulated formant

        afund   linen   ifndamp,.1,p3,.08
        afund   oscili  afund,afundhz,1
        aform   envlpx  ifmtamp,.1,p3,.08,ifenvfn,1,.01  
        aform   oscili  aform,aformhz,1

        asig    =       (afund+aform)*p4        ;scale to peak amp here
                out     asig
endin
