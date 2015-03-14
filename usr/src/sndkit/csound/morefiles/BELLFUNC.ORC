; bellfunc.orc
;
; Even though csound only provides a limited number of stock gen subroutines,
; it includes one (gen 1) which can read in an external file.  Hence, you
; can use a standalone program to generate functions that aren't standard.
; In fact, as this example shows, you can use csound itself to create them.
; You just have to remember to perf with the floating point option (-f)...
;
; This orchestra will generate a bell-shaped function with max amp of 1.0 --
; required by endless.orc for an amplitude control function.
; Say "perf -f bellfunc.orc bellfunc.sco" to run it, then rename the output
; test.sf file to soundin.10 so that endless.sco can read it in via Gen 1.  rp
;
        sr      =       1       ;N.B. we're not generating audio here, just...
        kr      =       1       ;a bunch of values for an array, so the...
        ksmps   =       1       ;sr and kr don't really matter.  However,...
        ksmps   =       1       ;it's convenient to make them = 1, so that...
        nchnls  =       1       ;p3 can be used to specify the number of...
                                ;values to compute directly.

                instr   1
        ipi     =       3.14159
        isize   =       p3-1    ;since sr=1, p3 has number of locs to generate
        kx      init    0

; Risset's formula (from Dodge):  exp(-4.8283*(1-cos(2*pi*(x-255.5)/511)))

	aval	=	exp(-4.8283*(1-cos(2*ipi*(kx-(isize*.5))/isize)))
	kx	=	kx+1
	display aval,p3*.5
	display aval,p3
                out     aval
                endin
