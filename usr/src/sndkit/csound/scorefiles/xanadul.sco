;		Score for final project in Digital Audio Processing
;		---------------------------------------------------

;			Piece entitled : X A N A D U (long version)
;				Joseph T. Kung, 12/12/88

;          	The first part of the score will specify all function
;		tables used in the piece. The second part specifies
;		the instruments and notes. The latter is divided into
;		7 sections, each playing a chord on a different instrument.
;		The chords are uncommon guitar chords that use the open
;		B and E strings often. These will be transposed by
;		octaves on some chords.

;		Each instrument will play a chord. The timbre
;		of the instrument will change in that interval and join
;		with the next instrument/chord sequence. Instrument 3
;		uses a modified FM synthesis technique. This is joined
;		by an additional plucked-string instrument
;		(instruments 1 and 2). The results are effected using
;		choruses, delays, and echoes. Reverb will be suppplied
;		by an external source (Lexicon PCM70).

;	The Function Tables
;	-------------------
;All functions are post-normalized (max value is 1) if p4 is POSITIVE.

f1 0 8192 10 1		;sine wave
f2 0 8192 11 1		;cosine wave
f3 0 8192 -12 20.0	;unscaled ln(I(x)) from 0 to 20.0

;-----------------------------------------------------------------------------

;----------- This section comprises all the new FM sounds -----------------

;F#7addB chord on a guitar
i3 0 15 0 7.06 2.0 0.2	;F#
i3 . . . 8.01 . .	;C# above
i3 . . . 8.06 . .	;F# octave above 1st one
i3 . . . 8.10 . .	;Bb next one up
i3 . . . 8.11 . .	;B
i3 . . . 9.04 . .	;E

;D6add9 chord on a guitar
i3 7.5 22.5 0 6.02 1.7 0.5	;D
i3 . . . 6.09 . .	;A above
i3 . . . 7.02 . .	;D octave above 1st one
i3 . . . 7.06 . .	;F# next one up
i3 . . . 6.11 . .	;B
i3 . . . 7.04 . .	;E

;Bmajadd11 chord on a guitar
i3 15 30 0 7.11 1.4 0.8	;B
i3 . . . 8.06 . .	;F# above
i3 . . . 8.11 . .	;B octave above 1st one
i3 . . . 9.03 . .	;D# next one up
i3 . . . 8.11 . .	;B
i3 . . . 9.04 . .	;E;

;Amajadd9 chord on a guitar
i3 22.5 37.5 0 6.09 1.1 1.1	;A
i3 . . . 7.04 . .	;E above
i3 . . . 8.09 . . 	;A octave above 1st one
i3 . . . 8.01 . .	;C# next one up
i3 . . . 7.11 . .	;B
i3 . . . 8.04 . .	;E

;Bmajadd11 chord on a guitar
i3 30 45 0 6.11 0.8 1.4	;B
i3 . . . 7.06 . .	;F# above
i3 . . . 7.11 . .	;B octave above 1st one
i3 . . . 8.03 . .	;D# next one up
i3 . . . 7.11 . .	;B
i3 . . . 8.04 . .	;E;

;Gmaj6 chord on a guitar
i3 37.5 52.5 0 5.07 0.5 1.7	;G
i3 . . . 6.02 . .	;D above
i3 . . . 6.07 . .	;G octave above 1st one
i3 . . . 6.11 . .	;B on G string
i3 . . . 6.11 . .	;B
i3 . . . 7.04 . .	;E

;F#7addB chord on a guitar
i3 45 60 0 7.06 0.2 2.0	;F#
i3 . . . 8.01 . .	;C# above
i3 . . . 8.06 . .	;F# octave above 1st one
i3 . . . 8.10 . .	;Bb next one up
i3 . . . 8.11 . .	;B
i3 . . . 9.04 . .	;E

; This section adds the plucked chords to the beginning and
; end of each section.

;F#7addB chord on a guitar (start)
i1 0 10 0 8.06	;F#
i1 0.1 . . 9.01	;C# above
i1 0.2 . . 9.06	;F# octave above 1st one
i1 0.3 . . 9.10	;Bb next one up
i1 0.4 . . 9.11	;B
i1 0.5 . . 10.04	;E

;D6add9 chord on a guitar
i2 7.5 10 0 8.02 	;D
i2 7.6 . . 8.09		;A above
i2 7.7 . . 9.02		;D octave above 1st one
i2 7.8 . . 9.06		;F# next one up
i2 7.9 . . 9.11		;B
i2 8.0 . . 10.04 	;E

;Bmajadd11 chord on a guitar
i2 15 10 0 8.11 	;B
i2 15.1 . . 9.06	;F# above
i2 15.2 . . 9.11	;B octave above 1st one
i2 15.3 . . 10.03	;D# next one up
i2 15.4 . . 9.11 	;B
i2 15.5 . . 10.04	;E;

;Amajadd9 chord on a guitar
i2 22.5 10 0 8.09	;A
i2 22.6 . . 9.04 	;E above
i2 22.7 . . 10.09	;A octave above 1st one
i2 22.8 . . 10.01	;C# next one up
i2 22.9 . . 9.11	;B
i2 23.0 . . 10.04	;E

;Bmajadd11 chord on a guitar
i2 30 10 0 8.11		;B
i2 30.1 . . 9.06	;F# above
i2 30.2 . . 9.11	;B octave above 1st one
i2 30.3 . . 10.03	;D# next one up
i2 30.4 . . 9.11	;B
i2 30.5 . . 10.04	;E;

;Gmaj6 chord on a guitar
i2 37.5 10 0 8.07 	;G
i2 37.6 . . 9.02	;D above
i2 37.7 . . 9.07	;G octave above 1st one
i2 37.8 . . 9.11	;B on G string
i2 37.9 . . 9.11	;B
i2 38.0 . . 10.04	;E

;F#7addB chord on a guitar
i2 45 10 0 9.06		;F#
i2 45.1 . . 10.01	;C# above
i2 45.2 . . 10.06	;F# octave above 1st one
i2 45.3 . . 10.10	;Bb next one up
i2 45.4 . . 10.11	;B
i2 45.5 . . 11.04	;E

;Bmajadd11 chord on a guitar (reverse order)
i2 60.5 10 0 8.11		;B
i2 60.4 . . 9.06	;E above
i2 60.3 . . 9.11	;B octave above 1st one
i2 60.2 . . 10.03	;D# next one up
i2 60.1 . . 9.11	;B
i2 60 . . 10.04	;E;

;Gmaj6 chord on a guitar (reverse order)
i2 75.5 10 0 8.07 	;G
i2 75.4 . . 9.02	;D above
i2 75.3 . . 9.07	;G octave above 1st one
i2 75.2 . . 9.11	;B on G string
i2 75.1 . . 9.11	;B
i2 75.0 . . 10.04	;E

;F#7addB chord on a guitar (reverse order)
i1 90.5 10 0 9.06		;F#
i1 90.4 . . 10.01	;C# above
i1 90.3 . . 10.06	;F# octave above 1st one
i1 90.2 . . 10.10	;Bb next one up
i1 90.1 . . 10.11	;B
i1 90.0 . . 11.04	;E

e

