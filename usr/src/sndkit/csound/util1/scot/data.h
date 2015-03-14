#include <stdio.h>

/* global data declarations */

#define TOKENLEN 20		/* maximum number of characters per token: */
				/* primarily affects instrument names */
#define MAXDEPTH 10		/* maximum nesting depth for groups */
#define NOINSTRS 50		/* number of instruments allowed */
#define TEMPOVALS 48		/* (number of tempos per section - 1 ) * 2 */
#define GROUPS 6		/* max groupette embedding */
#define PLEN 11 		/* maximum length of p-field - should be odd */
#define PLENs " %.11s"		/* for printf */
#define RESERVED 5		/* number of p-fields Scot reserves */
#define PMAX 45 		/* maximum number of p-fields - RESERVED */
#define COMMENT ';'		/* rest of line is a comment */
#define TRUE 1
#define FALSE 0
#define NUL 0
#define ERROR 1 		/* exit status if errors detected */
#define OK 0			/* exit status if carry is not needed */

typedef struct rat {		/* rational number */
	long num;
	long den;
} RAT;

struct symbol {
	struct symbol *s_nxt;
	char	*s_name;
	char	*s_txt;
};

struct symbol *symgbl;		/* global define chain */
struct symbol *symins[NOINSTRS];/* per instr define chain */
#define TOKALO 400
char *tokpol,*tokpox;		/* allocation area for token strings */
struct symbol *sympol,*sympox;	/* allocation area for symbol blocks */
#define SYMALO 40

/* input file stuff */
#define INBUFL 200
char	inbuf[INBUFL];	/* input line buffer */
#define PBACK 80	/* max chars unput by lexer */
char	pbackb[PBACK];	/* lex unput buffer */
int	pback;		/* unput index */
#define EXPTXT 80	/* macro expansion size */
char	exptxt[EXPTXT]; /* macro expansion text */
char	*expr,*expw;	/* pointers into macro expansion */
int	exping; 	/* TRUE if expanding macro text */
char	*pin;		/* pointer in input line */
int	yylineno;	/* line counter within current file */
int	s_ac;		/* global argc countdown */
int	s_ac0;		/* global original argc */
char	**s_av; 	/* global argv */
FILE *outfil;		/* output file */
int errcnt;		/* number of errors so far */
char scoresec;		/* true if in score section */
char need_carry;	/* TRUE if carry is needed to process output */
char carry_accidentals; 	/* TRUE if accidentals valid */
				/* for whole measure */
char carry_vertically;		/* TRUE if parameters p6 and beyond */
				/* carry thru a backup list.  Even */
				/* FALSE, they will carry 1st time. */
char follow_octaves;		/* TRUE when octave following done */
char doing_trans;		/* TRUE while computing transposition */
char backing_up;		/* TRUE after backup character parsed */
RAT bar_start;			/* time current bar started */
RAT bar_dur;			/* duration of bar, according to time */
				/* signature--0 if no time signature */
RAT temp_dur;			/* temporary */
RAT sect_dur;			/* maximum extent of section (for f0) */
int section;			/* number of score section */
int measure;			/* measure or bar number */
double tempi[TEMPOVALS];	/* even indices: time in beats */
				/* odd indices: tempi at related times */
int tindex;			/* index to tempi array */
RAT start_times[MAXDEPTH];	/* starting times of groups */
int igroup;			/* groupette stack index */
RAT groupstk[GROUPS];		/* groupette stack */
int start_index;		/* index to start_times array */
RAT last_start; 		/* value most recently popped from */
				/* start_times array */
int whichpf;			/* pfield currently being parsed */
				/* refers to pfields array */
char *instrptr; 		/* pointer into instrs array */
int insnum[NOINSTRS];		/* output instr number for music11 */
int insindex;			/* index to instrs array */
int curins;			/* index to current instrument */
int maxcopy[NOINSTRS];		/* highest copy number in current section */
int copy_depth; 		/* number of copies for last note */
extern char on[];			/* for switches */
extern char off[];			/* for switches */
/* char tee[];			/* for error checking in globals */
extern char dot[];			/* for use in ramping */
char tokenval[TOKENLEN+1];	/* value of token */

#ifdef SYS5
extern unsigned char yytext[];			/* lexer token return */
#else
extern char yytext[];			/* lexer token return */
#endif

char instrs[NOINSTRS][TOKENLEN+1]; /* instrument name array */
char remark[500];		/* remark storage for current note */

extern char scrnam[];			/* output filename */
extern char scrtmp[];			/* temporary filename */
extern char ramper[];			/* ramp program */

extern char	*delimiters[];
extern int	delimtokens[];

struct keydata {
	int sig;	/* 0 = n, -1 = flat, +1 = sharp, etc. */
	int acc;	/* = sig at start of bar, then modified */
	int nat;	/* PCH for all naturals in key signature */
	int changed;	/* TRUE if this pitch is in key signature */
	RAT when;	/* Time that acc field was set */
} key [7];

typedef struct instrument {
	int ins_number; 	/*   0 instrument number */
	int ins_copy;		/*   2 copy number--used with backups */
	int trans_octave;	/*   4 octave offset for transposition */
	int trans_pitch;	/*   6 pitch offset for transposition */
	int octave;		/*  10 current octave for instrument */
	int nom_octave; 	/*  12 nominal--for octave following */
	int pitch_deg;		/*  14 current PCH part for instrument */
	int scale_deg;		/*  16 scale degree--index for key array */
	int old_octave; 	/*  20 octave of previous note */
	int old_pitch_deg;	/*  22 pitch_degree of previous note */
	int old_scale_deg;	/*  24 scale_degree of previous note */
	int pmax;		/*  26 how many pfields have been used */
	char last_slurred;	/*  30 TRUE if slurred to previous note */
	char this_slurred;	/*  31 TRUE if slurred to next note */
	char tied;		/*  32 TRUE if tied to next note */
	char transposing;	/*  33 TRUE if transposition specified */
	char rest;		/*  34 TRUE if this is a rest */
	char accidental;	/*  35 TRUE if note has an accidental */
	struct instrument *next_ins; /* 36 next copy in backup chain */
	RAT start;		/*  40 start time for current note */
	RAT event_start;	/*  50 start time for current event */
				/*     differs from start for tied notes */
	RAT dur;		/*  60 duration of current note */
	RAT total_dur;		/*  70 duration including ties */
	RAT old_dur;		/* 100 duration of previous note */
	char pflds[PMAX][(PLEN+1)/2]; /* 110 pfields from p6 */
	char pfldg[PMAX][(PLEN+1)/2]; /* global pfield values */
} INSTRUMENT;

INSTRUMENT ins1;		/* instrument chain anchor */
INSTRUMENT *insptr;		/* pointer to current instrument */
