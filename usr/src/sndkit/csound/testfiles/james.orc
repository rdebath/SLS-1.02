
sr = 45000
kr = 2500
ksmps = 18

        instr 2  ;(orc.pluck)
a1      pluck    3500, cpspch(p5), cpspch(p5), p13, p14
;a2      tablei   a1, p12
;afilt   tone     a2, cpspch(p5)*5
;abal    balance  afilt, a2*p6
        out	 a1
        endin

		 ;p6 = output amplitude
		 ;p7 = envelope - linseg amplitude 1 of 5
		 ;p8 = linseg amplitude 2
		 ;p9 = linseg amplitude 3
		 ;p10 = linseg amplitude 4
		 ;p11 = linseg amplitude 5
		 ;p12 = function table read by tablei
		 ;p13 = function table initializing pluck decay buffer 
		    ;(0 - random, or 7 only possible values)
		 ;p14 = type of pluck (p14 = 1, 2, 3, or 4)
		 ;p16 = stereo panning
		 ;NOTE: In order to slur pitches together, each note (includ-
		    ;ing the first, but not the last) should extend 25% beyond 
		    ;the beginning of the next note

