#include "cs.h"			/*				RTEVENTS.C	*/
#ifdef THINK_C
#include <console.h>
#else
#include <fcntl.h>
#endif

#define LBUFSIZ   200
#define MBUFSIZ   100
#define LF        '\n'

static char *Linebuf, *Linep, *Linebufend;
static char *Midibuf, *Midip, *Midibufend;
static int  Linefd = 0, Midifd = 0;
static FILE *Linecons, *Midicons;

       EVTBLK *RTevtblk;
static EVTBLK *Linevtblk, *Midevtblk;  /* RTevtblk gets one of these on event rtn */

void RTinit()        /* set up Linebuf, Midibuf & ready the input files */
{                    /*     callable once from main.c         */
extern int  Linein, Midiin;
extern char *Linename, *Midiname;

    if (Linein) {
        Linevtblk = (EVTBLK *) mcalloc((long)sizeof(EVTBLK));
	Linebuf = mcalloc((long)LBUFSIZ);
	Linebufend = Linebuf + LBUFSIZ;
	Linep = Linebuf;
	if (strcmp(Linename,"stdin") == 0) {
#ifdef THINK_C
	    console_options.top += 10;
	    console_options.left += 10;
	    console_options.title = "\pRT Line_events";
	    console_options.nrows = 10;
	    console_options.ncols = 50;
	    Linecons = fopenc();
	    cshow(Linecons);
#else
	    if (fcntl(Linefd, F_SETFL, fcntl(Linefd, F_GETFL, 0) | O_NDELAY) < 0)
	        die("-R stdin fcntl failed");
#endif
	}
	else if ((Linefd = open(Linename, O_RDONLY | O_NDELAY)) < 0)
	    dies("cannot open %s", Linename);
    }
    if (Midiin) {
        Midevtblk = (EVTBLK *) mcalloc((long)sizeof(EVTBLK));
	Midibuf = mcalloc((long)MBUFSIZ);
	Midibufend = Midibuf + MBUFSIZ;
	Midip = Midibuf;
	if (strcmp(Midiname,"stdin") == 0) {
#ifdef THINK_C
	    dieu("RT Midi_event Console not implemented");
#else
	    if (fcntl(Midifd, F_SETFL, fcntl(Midifd, F_GETFL, 0) | O_NDELAY) < 0)
	        die("-M stdin fcntl failed");
#endif
	}
	else if ((Midifd = open(Midiname, O_RDONLY | O_NDELAY)) < 0)
	    dies("cannot open %s", Midiname);
     }
}

void RTclose()
{
#ifdef THINK_C
    if (Linecons != NULL) fclose(Linecons);
    if (Midicons != NULL) fclose(Midicons);
#else
    if (Linefd) close(Linefd);
    if (Midifd) close(Midifd);
#endif
}

static int containsLF(cp,endp)    /* does this string segment contain LF ? */
 register char *cp, *endp;
{
    do { if (*cp++ == LF)  return(1); }
    while (cp < endp);
    return(0);
}

int sensLine()   /* accumlate RT Linein buffer, & place completed events in EVTBLK */
{                /* does more syntax checking than rdscor, since not preprocessed  */
    register int c;
    register char *cp;
    EVTBLK *e;
    int   n, pcnt;
    float *fp;
    char  *Linend;
static char prvop = ' ';
static int prvpcnt = 0;
static float prvp1 = 0.;

#ifdef THINK_C
    if ((n = fread((void *)Linep, (size_t)1, (size_t)(Linebufend-Linep), Linecons)) < 0 && Linep == Linebuf)
#else
    if ((n = read(Linefd, Linep, Linebufend-Linep)) < 0 && Linep == Linebuf)
#endif
        return(0);
    if (n || Linep > Linebuf) {
/*      printf("sensLine %d CHARS\n",n);  */
	Linend = Linep + n;
	if ((c = *(Linend - 1)) == LF || containsLF(Linep,Linend)) {
	    cp = Linebuf;
	    e = Linevtblk;
	    while ((c = *cp++) == ' ' || c == '\t')    /* skip initial white space */
	        ;
	    if (c == LF) return(0);                    /* if null line, return     */
	    switch(c) {                                /* look for legal opcode    */
	    case 'i':
	    case 'f': e->opcod = c;
	              break;
	    default:  fprintf(stderr,"unknown opcode %c\n");
	              goto Lerr;
	    }                                         /* for params that follow:   */
	    for (fp = &e->p[1], pcnt = 0; c != LF && pcnt<PMAX; pcnt++) {
	        register long val = 0, scale = 1;
		while ((c = *cp++) == ' ' || c == '\t')    /*  skip white space */
		    ;
		if (c == LF) break;
		if (!(c>='0' && c<='9' || c=='+' || c=='-' || c=='.'))
		    goto Lerr;
		if (c == '.'                               /*  if lone dot,       */
		  && ((n = *cp)==' ' || n=='\t' || n==LF)) {
		    fp++;                                  /*        pfld carry   */
		    continue;
		}
		if (c == '-') {scale = -1; c = *cp++;}      /*  valid float: eval  */
		if (c == '+' || c == '0')  c = *cp++;
		while (c >= '0' && c <= '9') {
		    val *= 10;
		    val += c - '0';
		    c = *cp++;
		}
		if (c == ' ' || c == '\t' || c == LF)       /* simple decimal val? */
		    goto pfstor;                            /*   quick store       */
		if (c == '.')
		    c = *cp++;
		while (c >= '0' && c <= '9') {              /* else read full float */
		    val *= 10;
		    val += c - '0';
		    scale *= 10;
		    c = *cp++;
		}
		if (c != ' ' && c != '\t' && c != LF)  /*     chk delimiter legal   */
		    goto Lerr;
	pfstor:	*fp++ = (float) val/scale;             /*     & stor pval in EVTBLK */
	    }
	    if (e->opcod == 'i' && prvop == 'i')       /* do carries for instr data */
	        if (!pcnt || pcnt < prvpcnt && e->p[1] == prvp1)
		    pcnt =  prvpcnt;
	    if (pcnt < 3) {
	        fprintf(stderr,"too few pfields\n");   /* check sufficient pfields  */
	        goto Lerr;
	    }
	    e->pcnt = pcnt;                            /*   &  record pfld count    */
	    prvop = e->opcod;                          /* preserv the carry sensors */
	    prvpcnt = pcnt;
	    prvp1 = e->p[1];
	    if (pcnt == PMAX && c != LF) {
	        printf("too many pfields\n");
	        while (*cp++ != LF)                    /* flush any excess data     */
		    ;
	    }
	    Linep = Linebuf;
	    while (cp < Linend)                        /* copy remaining data to    */
		*Linep++ = *cp++;                      /*     beginning of Linebuf  */
	    RTevtblk = Linevtblk;                      /* rtn this EVTBLK to insert */
	    return(1);
	}
	else Linep += n;           /* else just accum the chars */
    }
    return(0);                     /* & deal with them later    */

 Lerr:
    n = cp - Linebuf - 1;               /* error position */
    while (*cp++ != LF);                /* go on to LF    */
    *(cp-1) = '\0';                     /*  & insert NULL */
    fprintf(stderr,"illegal RT scoreline:\n%s\n", Linebuf);
    while (n--)
        fputc(' ',stderr);
    fprintf(stderr,"^\n");              /* mark the error */
    Linep = Linebuf;
    while (cp < Linend)
        *Linep++ = *cp++;         /* mov rem data forward */
    return(0);
}

int sensMidi()
{
    printf("looking for M");
    if (1) {
        RTevtblk = Midevtblk;
	return(1);
    }
    return(0);
}
