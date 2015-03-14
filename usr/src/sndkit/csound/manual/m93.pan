.nf
a1, a2, a3, a4	\fBpan\fR		asig, kx, ky, ifn[, imode][, ioffset]
.fi

Distribute an audio signal amongst four channels with localization control.


INITIALIZATION

\fIifn\fR - function table number of a stored pattern describing the
amplitude growth in a speaker channel as sound moves towards it from an
adjacent speaker.  Requires extended guard-point.

\fIimode\fR (optional) - mode of the \fIkx, ky\fR position values. 0
signifies raw index mode, 1 means the inputs are normalized (0-1). The
default value is 0.

\fIioffset\fR (optional) - offset indicator for \fIkx, ky\fR.  0 infers the
origin to be at channel 3 (left rear); 1 requests an axis shift to the
quadraphonic center.  The default value is 0.


PERFORMANCE

\fBpan\fR takes an input signal \fIasig\fR and distributes it amongst four
outputs (essentially quad speakers) according to the controls \fIkx\fR and
\fIky\fR.  For normalized input (mode=1) and no offset, the four output
locations are in order:  left-front at (0,1), right-front at (1,1),
left-rear at the origin (0,0), and right-rear at (1,0).
In the notation \fI(kx, ky)\fR, the coordinates \fIkx\fR and \fIky\fR, each
ranging 0-1, thus control the 'rightness' and 'forwardness' of a sound location.

Movement between speakers is by amplitude variation, controlled by the
stored function table \fIifn\fR.  As \fIkx\fR goes from 0 to 1, the strength
of the right-hand signals will grow from the left-most table value to the
right-most, while that of the left-hand signals will progress from the
right-most table value to the left-most.  For a simple linear pan, the
table might contain the linear function 0-1.  A more correct pan that maintains
constant power would be obtained by storing the first quadrant of a sinusoid.
Since \fBpan\fR will scale and truncate \fIkx\fR and \fIky\fR in simple
table lookup, a medium-large table (say 8193) should be used.

\fIkx, ky\fR values are not restricted to 0-1.  A circular motion passing
through all four speakers (escribed) would have a diameter of root 2, and
might be defined by a circle of radius R = root 1/2 with center at (.5,.5).
\fIkx, ky\fR would then come from Rcos(angle), Rsin(angle), with an
implicit origin at (.5,.5) (i.e.  ioffset=1).  Unscaled raw values operate
similarly.  Sounds can thus be located anywhere in the polar or cartesian
plane; points lying outside the speaker square are projected correctly onto
the square's perimeter as for a listener at the center.

.nf 
Example:
		instr	1
	k1	phasor	1/p3			;fraction of circle
	k2	tablei	k1, 1, 1			;sin of angle (sinusoid in f1)
	k3	tablei	k1, 1, 1, .25, 1		;cos of angle (sin offset 1/4 circle)
	a1	oscili	10000, 440, 1		;audio signal ..
   a1,a2,a3,a4	pan	a1, k2/2, k3/2, 2, 1, 1	;  sent in a circle (f2=1st quad sin)
		outq	a1, a2, a3, a4
		endin
.fi
.bp
