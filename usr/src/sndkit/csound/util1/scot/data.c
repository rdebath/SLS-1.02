/* initialization of global storage */

#include "y.tab.h"
#include "data.h"

char *pin = inbuf;	/* pointer in input line */
char need_carry = FALSE;/* TRUE if carry is needed to process output */
char carry_accidentals = TRUE;	/* TRUE if accidentals valid */
				/* for whole measure */
char carry_vertically = TRUE;	/* TRUE if parameters p6 and beyond */
				/* carry thru a backup list. Even */
				/* FALSE, they will carry 1st time. */
char follow_octaves = TRUE;	/* TRUE when octave following done */
char doing_trans = FALSE;	/* TRUE while computing transposition */
char backing_up = FALSE;	/* TRUE after backup character parsed */
int section = 0;		/* number of score section */
int measure = 1;		/* measure or bar number */
int tindex = 0; 		/* index to tempi array */
int start_index = 0;		/* index to start_times array */
int insindex = 0;		/* index to instrs array */
int copy_depth = 1;		/* number of copies for time span */
char on[] = "on";		/* for switches */
char off[] = "off";		/* for switches */
char dot[] = "."; 		/* for use in ramping */
char scrnam[] = "score";	/* output filename */
char scrtmp[] = "score.tmp";	/* temporary filename */
char ramper[] = "/usr/bin/ramp";/* ramp program */

char *delimiters [] = {
	"orchestra",
	"functions",
	"score",
	"key",
	"time",
	"transpose",
	"next",
	"end",
	"vertical",
	"accidentals",
	"octaves",
	0
};

int delimtokens [] = {		/* corresponds to delimiters array */
	ORCHESTRA,
	FUNCTIONS,
	SCORE,
	KEY,
	TIME,
	TRANSPOSE,
	NEXTV,
	ENDV,
	VERT,
	ACC,
	OCT,
};

struct keydata key[7] = {
	0, 0, 0,  0, { -1,1 },
	0, 0, 2,  0, { -1,1 },
	0, 0, 4,  0, { -1,1 },
	0, 0, 5,  0, { -1,1 },
	0, 0, 7,  0, { -1,1 },
	0, 0, 9,  0, { -1,1 },
	0, 0, 11, 0, { -1,1 },
};

INSTRUMENT ins1 = {
	1, 1, 0, 0, 8, 8, 0, 0, 8, 0, 0, FALSE, FALSE, FALSE, -1,
	FALSE, FALSE, FALSE, NUL
	};	/* provides a starting point for list of instruments */
INSTRUMENT *insptr = {&ins1};	/* pointer to current instrument */
