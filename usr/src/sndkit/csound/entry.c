#include "cs.h"			/*					ENTRY.C		*/
#include "insert.h"
#include "aops.h"
#include "ugens1.h"
#include "ugens2.h"
#include "ugens3.h"
#include "ugens4.h"
#include "ugens5.h"
#include "ugens6.h"
#include <dsputil.h>
#include "ugens7.h"
#include "window.h"
#include "rtctl.h"
#include "disprep.h"
#include "soundio.h"

#define S	sizeof

int	ihold(), turnoff();
int	assign(), rassign(), aassign();
int	init(), ainit();
int	gt(), ge(), lt(), le(), eq(), ne(), and(), or();
int	conval(), aconval();
int	addkk(), subkk(), mulkk(), divkk();
int	addka(), subka(), mulka(), divka();
int	addak(), subak(), mulak(), divak();
int	addaa(), subaa(), mulaa(), divaa();
int	int1(), frac1();
int	abs1(), exp01(), log01(), sqrt1(), sin1(), cos1();
int	absa(), expa(), loga(), sqrta(), sina(), cosa();
int	dbamp(), ampdb(), aampdb();
int	ftlen();
int	cpsoct(), octpch(), cpspch();
int	pchoct(), octcps(), acpsoct();
int	linset(), kline(), aline();
int	expset(), kexpon(), expon();
int	lsgset(), klnseg(), linseg();
int	xsgset(), kxpseg(), expseg();
int	lnnset(), klinen(), linen();
int	evxset(), knvlpx(), envlpx();
int	phsset(), kphsor(), phsor();
int	itable();
int	tblset(), ktable(), table(), ktabli(), tabli();
int	ko1set(), kosc1(),  kosc1i();
int	oscset(), koscil(), osckk(), oscka(), oscak(), oscaa();
int	koscli(), osckki(), osckai(), oscaki(), oscaai();
int	foscset(), foscil(), foscili(), adset(), adsyn();
int	fofset(), fof(), pvset(), pvoc();
int	bzzset(), buzz();
int	gbzset(), gbuzz();
int	plukset(), pluck();
int	rndset(), krand(), arand();
int	rhset(), krandh(), randh();
int	riset(), krandi(), randi();
int	porset(), port();
int	tonset(), tone(), atone();
int	rsnset(), reson(), areson();
int	lprdset(), lpread(), lprsnset(), lpreson(), lpfrsnset(), lpfreson();
int	rmsset(), rms(), gainset(), gain(), balnset(), balance();
int	downset(), downsamp(), upsamp();
int	indfset(), interp(), kntegrate(), integrate(), kdiff(), diff();
int	samphset(), ksmphold(), samphold();
int	delset(),  delay(),  del1set(), delay1();
int	delrset(), delayr(), delwset(), delayw();
int	tapset(), deltap(), deltapi();
int	cmbset(), comb(), alpass(), rvbset(), reverb();
int	panset(), pan();
int	sndinset(), soundin();
int	in(),  ins(), inq();
int	out(),  outs(), outs1(), outs2();
int	outq(), outq1(), outq2(), outq3(), outq4();
int	igoto(), kgoto(), icgoto(), kcgoto();
int	timset(), timout();
int	reinit(), rigoto(), rireturn();
int	tigoto(), tival();
int	printv(), dspset(), kdsplay(), dsplay(), fftset(), dspfft();
int	octdwnset(), octdown(), nocdfset(), noctdft();
int	spsclset(), specscal(), spsumset(), specsum(), spdifset(), specdiff();
int	spadmset(), specaddm(), spfilset(), specfilt(), spdspset(), specdisp();
int	xyinset(), xyin(), tempeset(), tempest(), tempset(), tempo();

/* thread vals, where isub=1, ksub=2, asub=4:
		0 =	1  OR	2  (B out only)
		1 =	1
		2 =		2
		3 =	1  AND	2
		4 =			4
		5 =	1  AND		4
		7 =	1  AND (2  OR	4)			*/
		
/* inarg types include the following:
		m	begins an indef list of iargs (any count)
		n	begins an indef list of iargs (nargs odd)
		o	optional, defaulting to 0
		p	   "		"	1
		q	   "		"	10
		v	   "		"	.5
   outarg types include:
		m	multiple outargs (1 to 4 allowed)
   (these types must agree with rdorch.c)			*/

OENTRY opcodlst[] = {
/* opcode   dspace	thread	outarg	inargs	isub	ksub	asub	*/
{ ""									},
{ "instr",  0					 			},
{ "endin",  0								},
{ "$label", S(LBLBLK)							},
{ "=",	    0								},
{ "init",   0	/* base names for later prefixes,suffixes */		},
{ "oscil",  0								},
{ "oscili", 0								},
{ "ihold",  S(LINK),	1,	"",	"",	ihold			},
{ "turnoff",S(LINK),	2,	"",	"",	NULL,	turnoff		},
{ "r=",	    S(ASSIGN),	1,	"r",	"i",	rassign			},
{ "i=",	    S(ASSIGN),	1,	"i",	"i",	assign			},
{ "k=",	    S(ASSIGN),	2,	"k",	"k",	NULL,	assign		},
{ "a=",	    S(ASSIGN),	4,	"a",	"x",	NULL,	NULL,	aassign	},
{ "iinit",  S(ASSIGN),	1,	"i",	"i",	init			},
{ "kinit",  S(ASSIGN),	1,	"k",	"i",	init			},
{ "ainit",  S(ASSIGN),	1,	"a",	"i",	ainit			},
{ ">",      S(RELAT),	0,	"B",	"kk",	gt,	gt		},
{ ">=",     S(RELAT),	0,	"B",	"kk",	ge,	ge		},
{ "<",      S(RELAT),	0,	"B",	"kk",	lt,	lt		},
{ "<=",     S(RELAT),	0,	"B",	"kk",	le,	le		},
{ "==",     S(RELAT),	0,	"B",	"kk",	eq,	eq		},
{ "!=",     S(RELAT),	0,	"B",	"kk",	ne,	ne		},
{ "&&",     S(LOGCL),	0,	"B",	"BB",	and,	and		},
{ "||",     S(LOGCL),	0,	"B",	"BB",	or,	or		},
{ ":i",     S(CONVAL),	1,	"i",	"bii",	conval			},
{ ":k",     S(CONVAL),	2,	"k",	"Bkk",	NULL,	conval		},
{ ":a",     S(CONVAL),	4,	"a",	"Bxx",	NULL,	NULL,	aconval	},
{ "addii",  S(AOP),	1,	"i",	"ii",	addkk			},
{ "subii",  S(AOP),	1,	"i",	"ii",	subkk			},
{ "mulii",  S(AOP),	1,	"i",	"ii",	mulkk			},
{ "divii",  S(AOP),	1,	"i",	"ii",	divkk			},
{ "addkk",  S(AOP),	2,	"k",	"kk",	NULL,	addkk		},
{ "subkk",  S(AOP),	2,	"k",	"kk",	NULL,	subkk		},
{ "mulkk",  S(AOP),	2,	"k",	"kk",	NULL,	mulkk		},
{ "divkk",  S(AOP),	2,	"k",	"kk",	NULL,	divkk		},
{ "addka",  S(AOP),	4,	"a",	"ka",	NULL,	NULL,	addka	},
{ "subka",  S(AOP),	4,	"a",	"ka",	NULL,	NULL,	subka	},
{ "mulka",  S(AOP),	4,	"a",	"ka",	NULL,	NULL,	mulka	},
{ "divka",  S(AOP),	4,	"a",	"ka",	NULL,	NULL,	divka	},
{ "addak",  S(AOP),	4,	"a",	"ak",	NULL,	NULL,	addak	},
{ "subak",  S(AOP),	4,	"a",	"ak",	NULL,	NULL,	subak	},
{ "mulak",  S(AOP),	4,	"a",	"ak",	NULL,	NULL,	mulak	},
{ "divak",  S(AOP),	4,	"a",	"ak",	NULL,	NULL,	divak	},
{ "addaa",  S(AOP),	4,	"a",	"aa",	NULL,	NULL,	addaa	},
{ "subaa",  S(AOP),	4,	"a",	"aa",	NULL,	NULL,	subaa	},
{ "mulaa",  S(AOP),	4,	"a",	"aa",	NULL,	NULL,	mulaa	},
{ "divaa",  S(AOP),	4,	"a",	"aa",	NULL,	NULL,	divaa	},
{ "iint",   S(EVAL),	1,	"i",	"i",	int1			},
{ "ifrac",  S(EVAL),	1,	"i",	"i",	frac1			},
{ "iabs",   S(EVAL),	1,	"i",	"i",	abs1			},
{ "iexp",   S(EVAL),	1,	"i",	"i",	exp01			},
{ "ilog",   S(EVAL),	1,	"i",	"i",	log01			},
{ "isqrt",  S(EVAL),	1,	"i",	"i",	sqrt1			},
{ "isin",   S(EVAL),	1,	"i",	"i",	sin1			},
{ "icos",   S(EVAL),	1,	"i",	"i",	cos1			},
{ "kint",   S(EVAL),	2,	"k",	"k",	NULL,	int1		},
{ "kfrac",  S(EVAL),	2,	"k",	"k",	NULL,	frac1		},
{ "kabs",   S(EVAL),	2,	"k",	"k",	NULL,	abs1		},
{ "kexp",   S(EVAL),	2,	"k",	"k",	NULL,	exp01		},
{ "klog",   S(EVAL),	2,	"k",	"k",	NULL,	log01		},
{ "ksqrt",  S(EVAL),	2,	"k",	"k",	NULL,	sqrt1		},
{ "ksin",   S(EVAL),	2,	"k",	"k",	NULL,	sin1		},
{ "kcos",   S(EVAL),	2,	"k",	"k",	NULL,	cos1		},
{ "aabs",   S(EVAL),	4,	"a",	"a",	NULL,	NULL,	absa	},
{ "aexp",   S(EVAL),	4,	"a",	"a",	NULL,	NULL,	expa	},
{ "alog",   S(EVAL),	4,	"a",	"a",	NULL,	NULL,	loga	},
{ "asqrt",  S(EVAL),	4,	"a",	"a",	NULL,	NULL,	sqrta	},
{ "asin",   S(EVAL),	4,	"a",	"a",	NULL,	NULL,	sina	},
{ "acos",   S(EVAL),	4,	"a",	"a",	NULL,	NULL,	cosa	},
{ "idbamp", S(EVAL),	1,	"i",	"i",	dbamp			},
{ "iampdb", S(EVAL),	1,	"i",	"i",	ampdb			},
{ "kdbamp", S(EVAL),	2,	"k",	"k",	NULL,	dbamp		},
{ "kampdb", S(EVAL),	2,	"k",	"k",	NULL,	ampdb		},
{ "aampdb", S(EVAL),	4,	"a",	"a",	NULL,	NULL,	aampdb	},
{ "iftlen", S(EVAL),	1,	"i",	"i",	ftlen			},
{ "ki",	    S(ASSIGN),	1,	"i",	"k",	init			},
{ "icpsoct",S(EVAL),	1,	"i",	"i",	cpsoct			},
{ "ioctpch",S(EVAL),	1,	"i",	"i",	octpch			},
{ "icpspch",S(EVAL),	1,	"i",	"i",	cpspch			},
{ "ipchoct",S(EVAL),	1,	"i",	"i",	pchoct			},
{ "ioctcps",S(EVAL),	1,	"i",	"i",	octcps			},
{ "kcpsoct",S(EVAL),	2,	"k",	"k",	NULL,	cpsoct		},
{ "koctpch",S(EVAL),	2,	"k",	"k",	NULL,	octpch		},
{ "kcpspch",S(EVAL),	2,	"k",	"k",	NULL,	cpspch		},
{ "kpchoct",S(EVAL),	2,	"k",	"k",	NULL,	pchoct		},
{ "koctcps",S(EVAL),	2,	"k",	"k",	NULL,	octcps		},
{ "acpsoct",S(EVAL),	4,	"a",	"a",	NULL,	NULL,	acpsoct	},
{ "line",   S(LINE),	7,	"s",	"iii",	linset,	kline,	aline	},
{ "expon",  S(EXPON),	7,	"s",	"iii",	expset,	kexpon,	expon	},
{ "linseg", S(LINSEG),  7,	"s",	"iin",	lsgset,	klnseg,	linseg	},
{ "expseg", S(EXPSEG),  7,	"s",	"iin",	xsgset,	kxpseg,	expseg	},
{ "linen",  S(LINEN),	7,	"s",	"xiii",	lnnset,	klinen,	linen	},
{ "envlpx", S(ENVLPX),	7,	"s",  "xiiiiiio",evxset,knvlpx,	envlpx	},
{ "phasor", S(PHSOR),	7,	"s",	"xo",	phsset,	kphsor,	phsor	},
{ "itable", S(TABLE),	1,	"i",	"iiooo",itable	 		},
{ "table",  S(TABLE),	7,	"s",	"xiooo",tblset,	ktable,	table	},
{ "tablei", S(TABLE),	7,	"s",	"xiooo",tblset,	ktabli,	tabli	},
{ "oscil1", S(OSCIL1),	3,	"k",	"ikii", ko1set,	kosc1   	},
{ "oscil1i",S(OSCIL1),	3,	"k",	"ikii", ko1set,	kosc1i  	},
{ "oscilkk",S(OSC),	7,	"s",	"kkio",	oscset,	koscil,	osckk	},
{ "oscilka",S(OSC),	5,	"a",	"kaio",	oscset,	NULL,	oscka	},
{ "oscilak",S(OSC),	5,	"a",	"akio",	oscset,	NULL,	oscak	},
{ "oscilaa",S(OSC),	5,	"a",	"aaio",	oscset,	NULL,	oscaa	},
{ "oscilikk",S(OSC),	7,	"s",	"kkio",	oscset,	koscli,	osckki	},
{ "oscilika",S(OSC),	5,	"a",	"kaio",	oscset,	NULL,	osckai	},
{ "osciliak",S(OSC),	5,	"a",	"akio",	oscset,	NULL,	oscaki	},
{ "osciliaa",S(OSC),	5,	"a",	"aaio",	oscset,	NULL,	oscaai	},
{ "foscil", S(FOSC),	5,	"a",  "xkkkkio",foscset,NULL,	foscil	},
{ "foscili",S(FOSC),	5,	"a",  "xkkkkio",foscset,NULL,	foscili	},
{ "adsyn",  S(ADSYN),	5,	"a",	"kki",	adset,	NULL,	adsyn	},
{ "fof",    S(FOFS),	5,	"a","xxxxkkkkkiiiiop",fofset,NULL,fof	},
{ "pvoc",   S(PVOC),	5,	"a",	"kkii",	pvset,	NULL,	pvoc	},
{ "buzz",   S(BUZZ),	5,	"a",  "xxkio",  bzzset,	NULL,	buzz	},
{ "gbuzz",  S(GBUZZ),	5,	"a",  "xxkkkio",gbzset,	NULL,	gbuzz	},
{ "pluck",  S(PLUCK),	5,	"a",  "kkiiioo",plukset,NULL,	pluck	},
{ "rand",   S(RAND),	7,	"s",	"xv",	rndset,	krand,	arand	},
{ "randh",  S(RANDH),	7,	"s",	"xxv",	rhset,	krandh,	randh	},
{ "randi",  S(RANDI),	7,	"s",	"xxv",	riset,	krandi,	randi	},
{ "port",   S(PORT),	3,	"k",	"kio",	porset,	port		},
{ "tone",   S(TONE),	5,	"a",	"ako",	tonset,	NULL,	tone	},
{ "atone",  S(TONE),	5,	"a",	"ako",	tonset,	NULL,	atone	},
{ "reson",  S(RESON),	5,	"a",	"akkoo",rsnset,	NULL,	reson	},
{ "areson", S(RESON),	5,	"a",	"akkoo",rsnset,	NULL,	areson	},
{ "lpread", S(LPREAD),	3,	"kkkk",	"kioo",	lprdset,lpread		},
{ "lpreson",S(LPRESON),	5,	"a",	"a",	lprsnset,NULL,	lpreson	},
{ "lpfreson",S(LPFRESON),5,	"a",	"ak",	lpfrsnset,NULL,	lpfreson},
{ "rms",    S(RMS),	3,	"k",	"aqo",	rmsset,	rms		},
{ "gain",   S(GAIN),	5,	"a",	"akqo",	gainset,NULL,	gain	},
{ "balance",S(BALANCE),	5,	"a",	"aaqo",	balnset,NULL,	balance	},
{ "downsamp",S(DOWNSAMP),3,	"k",	"ao",	downset,downsamp	},
{ "upsamp", S(UPSAMP),	4,	"a",	"k",	NULL,	NULL,	upsamp	},
{ "interp", S(INDIFF),	5,	"a",	"ko",	indfset,NULL,	interp	},
{ "integ",  S(INDIFF),	7,	"s",	"xo",	indfset,kntegrate,integrate},
{ "diff",   S(INDIFF),	7,	"s",	"xo",	indfset,kdiff,	diff	},
{ "samphold",S(SAMPHOLD),7,	"s",	"xxoo",	samphset,ksmphold,samphold},
{ "delay",  S(DELAY),	5,	"a",	"aio",	delset,	NULL,	delay	},
{ "delayr", S(DELAYR),	5,	"a",	"io",	delrset,NULL,	delayr	},
{ "delayw", S(DELAYW),	5,	"",	"a",	delwset,NULL,	delayw	},
{ "delay1", S(DELAY1),	5,	"a",	"ao",	del1set,NULL,	delay1	},
{ "deltap", S(DELTAP),	5,	"a",	"k",	tapset, NULL,	deltap	},
{ "deltapi",S(DELTAP),	5,	"a",	"x",	tapset, NULL,	deltapi	},
{ "comb",   S(COMB),	5,	"a",	"akio",	cmbset,	NULL,	comb	},
{ "alpass", S(COMB),	5,	"a",	"akio",	cmbset,	NULL,	alpass	},
{ "reverb", S(REVERB),	5,	"a",	"ako",	rvbset,	NULL,	reverb	},
{ "pan",    S(PAN),	5,	"aaaa",	"akkioo",panset,NULL,	pan	},
{ "soundin",S(SOUNDIN),	5,	"mmmm",	"ioo",	sndinset,NULL,	soundin	},
{ "in",     S(IN),	4,	"a",	"",	NULL,	NULL,	in	},
{ "ins",    S(INS),	4,	"aa",	"",	NULL,	NULL,	ins	},
{ "inq",    S(INQ),	4,	"aaaa",	"",	NULL,	NULL,	inq	},
{ "out",    S(OUT),	4,	"",	"a",	NULL,	NULL,	out	},
{ "outs",   S(OUTS),	4,	"",	"aa",	NULL,	NULL,	outs	},
{ "outq",   S(OUTQ),	4,	"",	"aaaa",	NULL,	NULL,	outq	},
{ "outs1",  S(OUT),	4,	"",	"a",	NULL,	NULL,	outs1	},
{ "outs2",  S(OUT),	4,	"",	"a",	NULL,	NULL,	outs2	},
{ "outq1",  S(OUT),	4,	"",	"a",	NULL,	NULL,	outq1	},
{ "outq2",  S(OUT),	4,	"",	"a",	NULL,	NULL,	outq2	},
{ "outq3",  S(OUT),	4,	"",	"a",	NULL,	NULL,	outq3	},
{ "outq4",  S(OUT),	4,	"",	"a",	NULL,	NULL,	outq4	},
{ "igoto",  S(GOTO),	1,	"",	"l",	igoto			},
{ "kgoto",  S(GOTO),	2,	"",	"l",	NULL,	kgoto		},
{ "goto",   S(GOTO),	3,	"",	"l",	igoto,	kgoto		},
{ "icgoto", S(CGOTO),	1,	"",	"Bl",	icgoto			},
{ "kcgoto", S(CGOTO),	2,	"",	"Bl",	NULL,	kcgoto		},
{ "gcgoto", S(CGOTO),	3,	"",	"Bl",	icgoto,	kcgoto		},
{ "timout", S(TIMOUT),	3,	"",	"iil",	timset,	timout		},
{ "reinit", S(GOTO),	2,	"",	"l",	NULL,	reinit		},
{ "rigoto", S(GOTO),	1,	"",	"l",	rigoto			},
{ "rireturn",S(GOTO),	1,	"",	"",	rireturn		},
{ "tigoto", S(GOTO),	1,	"",	"l",	tigoto			},
{ "tival",  S(EVAL),	1,	"i",	"",	tival			},
{ "print",  S(PRINTV),	1,	"",	"m",	printv			},
{ "display",S(DSPLAY),	7,	"",	"sio",	dspset, kdsplay,dsplay	},
{ "dispfft",S(DSPFFT),	5,	"",	"aiiooo",fftset,NULL,	dspfft	},
{ "octdown",S(OCTDOWN),	5,	"d",	"aiio", octdwnset,NULL,	octdown	},
{ "noctdft",S(NOCTDFT), 5,	"w",	"diiqooo",nocdfset,NULL,noctdft },
{ "specscal",S(SPECSCAL),5,	"w",	"wii",  spsclset,NULL,	specscal},
{ "specdiff",S(SPECDIFF),5,	"w",	"wi",   spdifset,NULL,	specdiff},
{ "specaddm",S(SPECADDM),5,	"w",	"wwp",  spadmset,NULL,	specaddm},
{ "specsum",S(SPECSUM), 5,	"k",	"wo",   spsumset,NULL,	specsum },
{ "specfilt",S(SPECFILT),5,	"w",	"wi",   spfilset,NULL,	specfilt},
{ "specdisp",S(SPECDISP),5,	"",	"wii",  spdspset,NULL,	specdisp},
{ "xyin",   S(XYIN),    3,      "kk",   "iiiiioo",xyinset,xyin          },
{ "tempest",S(TEMPEST), 5,	"k",	"kiiiiiiiiiop",tempeset,NULL,tempest},
{ "tempo",  S(TEMPO),   3,      "",     "ki",   tempset,tempo           }
};

OENTRY *oplstend = opcodlst + sizeof(opcodlst)/sizeof(OENTRY);
