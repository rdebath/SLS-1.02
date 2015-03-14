sr=8192
kr=512
ksmps=16

;;; function table 1 -- pure sine
;;; function table 2 -- skewed triangle for strings
;;; function table 3 -- triangle
;;; function table 4 -- oboe-like :-)
;;; function table 5 -- flute-like :-)

;;;
;;; Flute
;;;

instr 1 ;;; flute
        icpsp   = cpspch(p5)

        if (p4 == int(p4/2) * 2) goto initslurs
                ihold

initslurs:
        iatttm  = 0.09
        idectm  = 0.1
        isustm  = p3 - iatttm - idectm
        idec    = p6
        ireinit = -1

        if (p4 > 1) goto checkafterslur
        ilast   = 0

checkafterslur:
        if (p4 == 1 || p4 == 3) goto doneslurs
        idec = 0
        ireinit = 0

doneslurs:
        if (isustm <= 0) goto simpleenv
        kamp    linseg ilast,iatttm,p6,isustm,p6,idectm,idec,0,idec
        goto doneenv

simpleenv:
        kamp    linseg ilast,p3 / 2,p6,p3 / 2,idec,0,idec

doneenv:
        ilast = p6

;;; Some vibrato.
        ivamp   = icpsp * 0.01
        ivfreq  = 6.0
        kvbra   oscil ivamp,ivfreq,1,ireinit
        kfreq   = icpsp + kvbra

;;; Noise for burst at beginning of note.
        knseenv expon p6 / 3,.3,1
        anoise1 rand knseenv
        anoise  tone anoise1,200

;;; The note.
        a1      oscili kamp,kfreq,5,ireinit

        a2      = a1 + anoise

        out a2
endin

;;;
;;; Oboe
;;;

instr 2 ;;; oboe
        icpsp   = cpspch(p5)

        if (p4 == int(p4/2) * 2) goto initslurs
                ihold

initslurs:
        iatttm  = 0.06
        idectm  = 0.05
        isustm  = p3 - iatttm - idectm
        idec    = p6
        ireinit = -1

        if (p4 > 1) goto checkafterslur
        ilast   = 0

checkafterslur:
        if (p4 == 1 || p4 == 3) goto doneslurs
        idec = 0
        ireinit = 0

doneslurs:
        if (isustm <= 0) goto simpleenv
        kamp    linseg ilast,iatttm,p6,isustm,p6,idectm,idec,0,idec
        goto doneenv

simpleenv:
        kamp    linseg ilast,p3 / 2,p6,p3 / 2,idec,0,idec

doneenv:
        ilast = p6

;;; Some vibrato.
        ivamp   = icpsp * 0.015
        ivfreq  = 4.5
        kvbra   oscil ivamp,ivfreq,1,ireinit
        kfreq   = icpsp + kvbra

;;; The note.
        a1      oscili kamp,kfreq,4,ireinit

                out a1
endin

;;;
;;; Violin
;;;

instr 3 ;;; violin
        ; p3 = duration
        ; p4 = slur information
        ; p5 = pitch
        ; p6 = amplitude

        icpsp   = cpspch(p5)

        if (p4 == int(p4/2) * 2) goto initslurs
                ihold
initslurs:
        iatttm  = 0.075
        idectm  = 0.1
        isustm  = p3 - iatttm - idectm
        idec    = p6
        ireinit = -1

        if (p4 > 1) goto checkafterslur
        ilast   = 0

checkafterslur:
        if (p4 == 1 || p4 == 3) goto doneslurs
        idec = 0
        ireinit = 0

doneslurs:
        if (isustm <= 0) goto simpleenv
        kamp    linseg ilast,iatttm,p6,isustm,p6,idectm,idec,0,idec
        goto doneenv

simpleenv:
        kamp    linseg ilast,p3 / 2,p6,p3 / 2,idec,0,idec

doneenv:
        ilast = p6

;;; Some randomness for fun.
        krnd    randi .1,10
        krnd    = krnd + 1
        kamp    = kamp * krnd

;;; Some vibrato.
        ivamp   = icpsp * 0.025
        ivfreq  = 5
        kvbra   oscil ivamp,ivfreq,1,ireinit
        kfreq   = icpsp + kvbra

;;; The note.
        a1      oscili kamp,kfreq,2,ireinit

;;; Some resonance cavities.
        a2      tone a1,3500,ireinit
        a3      comb a2,.15,.01,ireinit
        a4      comb a3,.15,.02,ireinit

                out a1
endin

;;;
;;; Cello
;;;

instr 4 ;;; cello
        ; p3 = duration
        ; p4 = slur information
        ; p5 = pitch
        ; p6 = amplitude

        p5      = p5 - 1
        icpsp   = cpspch(p5)

        if (p4 == int(p4/2) * 2) goto initslurs
                ihold
initslurs:
        iatttm  = 0.075
        idectm  = 0.1
        isustm  = p3 - iatttm - idectm
        idec    = p6
        ireinit = -1

        if (p4 > 1) goto checkafterslur
        ilast   = 0

checkafterslur:
        if (p4 == 1 || p4 == 3) goto doneslurs
        idec = 0
        ireinit = 0

doneslurs:
        if (isustm <= 0) goto simpleenv
        kamp    linseg ilast,iatttm,p6,isustm,p6,idectm,idec,0,idec
        goto doneenv

simpleenv:
        kamp    linseg ilast,p3 / 2,p6,p3 / 2,idec,0,idec

doneenv:
        ilast = p6

;;; Some randomness for fun.
        krnd    randi .1,10
        krnd    = krnd + 1
        kamp    = kamp * krnd

;;; Some vibrato.
        ivamp   = icpsp * 0.025
        ivfreq  = 5
        kvbra   oscil ivamp,ivfreq,1,ireinit
        kfreq   = icpsp + kvbra

;;; The note.
        a1      oscili kamp,kfreq,2,ireinit

;;; Some resonance cavities.
        a2      tone a1,3500,ireinit
        a3      comb a2,.15,.01,ireinit
        a4      comb a3,.15,.02,ireinit

                out a1
endin

;;;
;;; Harpsichord 1
;;;

instr 5 ;;; harpichord
        icpsp   = cpspch(p5)
        a1      pluck p6,icpsp,icpsp,0,1
        ka2     expon p6,p3,1
        a2      oscil ka2,icpsp,3
        a4      = a2 + a1
                out a4
endin

;;;
;;; Harpsichord 2
;;;

instr 6 ;;; harpichord
        icpsp   = cpspch(p5)
        a1      pluck p6,icpsp,icpsp,0,1
        ka2     expon p6,p3,1
        a2      oscil ka2,icpsp,3
        a4      = a2 + a1
                out a4
endin
