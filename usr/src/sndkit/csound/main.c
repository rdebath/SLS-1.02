#include "cs.h"			/*				 MAIN.C	*/
#include "soundio.h"

#ifdef THINK_C
#define main tc_main
#endif

int	odebug = 0;		/* orch control switches	*/
int	initonly = 0;		/*   and default values		*/
int	sfread = 0;
int	sfwrite = 1;
int	sfheader = 1;
int	iobufsamps = IOBUFSAMPS;
int	informat, outformat = 0;  /* set below */
int	insampsiz, outsampsiz;    /* set below */
int	displays = 1;
int	graphsoff = 0;
int	msglevel = 7;
int	Beatmode = 0;
int	Linein = 0, Midiin = 0;
int	RTevents = 0, ksensing = 0;

char	*orchname;
char	*infilename;
char	*outfilename;
char	*Linename, *Midiname;
int	infd = 0;
int	outfd = 0;

static  int	ScotScore = 0, stdinassgn = 0;
static  char	*scorename;
static  char	*xfilename;
static  char	*sortedscore = "score.srt";
static  char	*xtractedscore = "score.xtr";
static  char	*playscore = "score.srt";     /* unless we extract */
static  FILE    *scorin, *scorout, *xfile;
        FILE    *scfp;

main(argc,argv)
 int argc;
 char **argv;
{
	register char c, *s;
	char  outformch;
	int   n;
	extern int getsizformat();
	extern void RTinit();

	if (!(--argc))
	    dieu("insufficient arguments");
	do {
	    s = *++argv;
	    if (*s++ == '-')    		      /* read all flags:  */
	        while ((c = *s++) != '\0')
		    switch(c) {
		    case 'I': initonly = 1; 		/* I-only implies */
		    case 'n': sfwrite = 0;  		/* nosound	  */
		              break;
		    case 'i': if (*s == '\0') {
				  if (!(--argc)) dieu("no infilename");
				  s = *++argv;
			      }
		              infilename = s;		/* soundin name */
		              while (*++s);
		              if (strcmp(infilename,"stdout") == 0)
				  dieu("-i cannot be stdout");
		              if (strcmp(infilename,"stdin") == 0)
#ifdef THINK_C
				  dieu("stdin audio not supported");
#else
		              {
				  if (stdinassgn)
				      dieu("-i: stdin previously assigned");
				  stdinassgn = 1;
			      }
#endif
		              sfread = 1;
		              break;
		    case 'o': if (*s == '\0') {
				  if (!(--argc)) dieu("no outfilename");
				  s = *++argv;
			      }
		              outfilename = s;		/* soundout name */
		              while (*++s);
		              if (strcmp(outfilename,"stdin") == 0)
				  dieu("-o cannot be stdin");
		              if (strcmp(outfilename,"stdout") == 0) {
#ifdef THINK_C
				  dieu("stdout audio not supported");
#else
				  if ((outfd = dup(1)) < 0)    /* redefine stdout */
				      dies("too many open files");
				  dup2(2,1);              /* & send 1's to stderr */
#endif
			      }
		              break;
		    case 'b': if (*s == '\0') {
				  if (!(--argc)) dieu("no iobufsamps");
				  s = *++argv;
			      }
		              sscanf(s,"%d",&iobufsamps);
		              while (*++s);
		              break;
		    case 'h': sfheader = 0;             /* skip sfheader  */
		              break;
		    case 'c': if (outformat) goto outform;
		              outformch = c;
		              outformat = AE_CHAR;	/* 8-bit char soundfile */
		              break;
		    case 'a': if (outformat) goto outform;
		              outformch = c;
		              outformat = AE_ALAW;	/* a-law soundfile */
		              break;
		    case 'u': if (outformat) goto outform;
		              outformch = c;
		              outformat = AE_ULAW;	/* mu-law soundfile */
		              break;
		    case 's': if (outformat) goto outform;
		              outformch = c;
		              outformat = AE_SHORT;	/* short_int soundfile */
		              break;
		    case 'l': if (outformat) goto outform;
		              outformch = c;
		              outformat = AE_LONG;	/* long_int soundfile */
		              break;
		    case 'f': if (outformat) goto outform;
		              outformch = c;
		              outformat = AE_FLOAT;	/* float soundfile */
		              break;
		    case 'v': odebug = 1;   		/* verbose otran  */
		              break;
		    case 'm': if (*s == '\0') {
				  if (!(--argc)) dieu("no msglevel");
				  s = *++argv;
			      }
		              sscanf(s,"%d",&msglevel);
		              while (*++s);
		              break;
		    case 'd': displays = 0;         	/* no func displays */
		              break;
		    case 'g': graphsoff = 1;         	/* don't use graphics */
		              break;
		    case 'S': ScotScore++;
		              break;
		    case 'x': if (*s == '\0') {
				  if (!(--argc)) dieu("no xfilename");
				  s = *++argv;
			      }
		              xfilename = s;  		/* extractfile name */
		              while (*++s);
		              break;
		    case 'B': Beatmode = 1;         	/* use Beat fields of    */
		              break;            	/*   uninterpreted score */
		    case 'R': if (*s == '\0') {
				  if (!(--argc)) dieu("no RTscore device_name");
				  s = *++argv;
			      }
		              Linename = s;  		/* Linein device name */
		              while (*++s);
		              if (!strcmp(Linename,"stdin")) {
				  if (stdinassgn)
				      dieu("-R: stdin previously assigned");
				  stdinassgn = 1;
			      }
	                      Linein = 1;
		              break;
		    case 'M': if (*s == '\0') {
				  if (!(--argc)) dieu("no midi device_name");
				  s = *++argv;
			      }
		              Midiname = s;  		/* Midi device name */
		              while (*++s);
		              if (!strcmp(Midiname,"stdin")) {
				  if (stdinassgn)
				      dieu("-M: stdin previously assigned");
				  stdinassgn = 1;
				}
	                      Midiin = 1;
		              break;
		    default:  usage();
		    }
	    else {
	        if (orchname == NULL)
		    orchname = --s;
	        else if (scorename == NULL)
		    scorename = --s;
		else dieu("too many arguments");
	    }
	} while (--argc);

	if (!outformat)                         /* if no audioformat yet  */
	    outformat = AE_SHORT;               /*  default to short_ints */
	outsampsiz = getsizformat(outformat);
	informat = outformat;    /* informat defaults; resettable by readinheader */
	insampsiz = outsampsiz;

	if (orchname == NULL)
	    dieu("no orchestra name");
	else fprintf(stderr,"orchname: %s", orchname);
	if (scorename != NULL)
	    fprintf(stderr,", scorename: %s", scorename);
	if (xfilename != NULL)
	    fprintf(stderr,", xfilename: %s", xfilename);
	fputc('\n', stderr);
#ifdef SYS5
	{
	  static  char  buf[80];
	  if (odebug)	setvbuf(stdout,buf,_IOLBF,80);
	}
#else
#ifndef THINK_C
	if (odebug)	setlinebuf(stdout);
#endif
#endif
	if (Linein || Midiin) {              /* if realtime input expected */
	    RTinit();                        /*    alloc bufs & open files */
	    RTevents = 1;
	}
	if (RTevents || sfread)
	    ksensing = 1;

	if (scorename == NULL) {
	    if (!RTevents)
	        dieu("no score event path");
	    fprintf(stderr,"performing with no concurrent scorefile\n");
	    goto perf;
	}
	if (ScotScore)
	    die("Scot scores not yet integrated.  Use pre-process for now");
	if ((n = strlen(scorename)) > 4            /* if score ?.srt or ?.xtr */
	  && (!strcmp(scorename+n-4,".srt")
	      || !strcmp(scorename+n-4,".xtr"))) {
	    fprintf(stderr,"using previous %s\n",scorename);
	    playscore = sortedscore = scorename;            /*   use that one */
	}
	else {
	    if (!(scorin = fopen(scorename, "r")))          /* else sort it   */
	        dies("cannot open scorefile %s", scorename);
	    if (!(scorout = fopen(sortedscore, "w")))
	        dies("cannot open %s for writing", sortedscore);
	    fprintf(stderr,"sorting score ...\n");
	    scsort(scorin, scorout);
	    fclose(scorin);
	    fclose(scorout);
	}
	if (xfilename != NULL) {                        /* optionally extract */
	    if (!strcmp(scorename,"score.xtr"))
	        dies("cannot extract %s, name conflict",scorename);
	    if (!(xfile = fopen(xfilename, "r")))
	        dies("cannot open extract file %s", xfilename);
	    if (!(scorin = fopen(sortedscore, "r")))
	        dies("cannot reopen %s", sortedscore);
	    if (!(scorout = fopen(xtractedscore, "w")))
	        dies("cannot open %s for writing", xtractedscore);
	    fprintf(stderr,"  ... extracting ...\n");
	    scxtract(scorin, scorout, xfile);
	    fclose(scorin);
	    fclose(scorout);
	    playscore = xtractedscore;
	}	    
	if (!(scfp = fopen(playscore, "r")))
	    dies("cannot reopen %s", playscore);
	fprintf(stderr,"\t... done\n");

perf:	fprintf(stderr,"MIT Csound:\n");
	dispinit();	       /* initialise graphics or character display */
	musmon();              /* play the score using current orchestra   */
	return dispexit();     /* hold or terminate the display output     */
				/* for Mac, dispexit returns 0 to exit immediately  */

outform: sprintf(errmsg,"sound output format cannot be both -%c and -%c",
		outformch, c);
	dieu(errmsg);
}

/*	if(args.scores) {
	    sprintf(command,"scot %s",args.scorename);
	    systemcall(command,"SCOT failed");
	    args.scorename = "score";
        }
	sprintf(command,"scsort < %s > score.srt", args.scorename);
	systemcall(command,"score sort failed");
*/

dieu(s)
 char *s;
{
        fprintf(stderr,"Csound Command ERROR:\t%s\n",s);
	usage();
}

usage()
{
fprintf(stderr,"Usage:\tcsound [-flags] orchfile scorefile\n");
fprintf(stderr, "Legal flags are:\n");
fprintf(stderr,"-I\tI-time only orch run\n");
fprintf(stderr,"-n\tno sound onto disk\n");
fprintf(stderr,"-i fnam\tsound input filename\n");
fprintf(stderr,"-o fnam\tsound output filename\n");
fprintf(stderr,"-b N\tsamples per soundio buffer\n");
fprintf(stderr,"-h\tno header on soundout file\n");
fprintf(stderr,"-c\t8-bit signed_char sound samples\n");
fprintf(stderr,"-a\talaw sound samples\n");
fprintf(stderr,"-u\tulaw sound samples\n");
fprintf(stderr,"-s\tshort_int sound samples\n");
fprintf(stderr,"-l\tlong_int sound samples\n");
fprintf(stderr,"-f\tfloat sound samples\n");
fprintf(stderr,"-v\tverbose orch translation\n");
fprintf(stderr,"-m N\ttty message level. Sum of: 1=note amps, 2=out-of-range msg, 4=warnings\n");
fprintf(stderr,"-d\tsuppress all displays\n");
fprintf(stderr,"-g\tsuppress graphics, use ascii displays\n");
fprintf(stderr,"-S\tscore is in Scot format\n");
fprintf(stderr,"-x fnam\textract from score.srt using extract file 'fnam'\n");
fprintf(stderr,"-B\tuse Beats of uninterpreted score for this performance\n");
fprintf(stderr,"-R dnam\tread Realtime Line-oriented score events from device 'dnam'\n");
fprintf(stderr,"-M dnam\tread MIDI events from device 'dnam'\n");
fprintf(stderr,"flag defaults: csound -s -otest -b%d -m7\n",IOBUFSAMPS);
        exit(1);
}
