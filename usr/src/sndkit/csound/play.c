/*******************************************************************\
*   play.c							    *
*   Play via LoFi 56k & Sony D/a on DecStation for dspBench	    *
*   07aug90 dpwe						    *
\*******************************************************************/

#include <stdio.h>
/* #define AES    1       	/* output via tom's mac aes board */

#ifdef LOFI	/* output available on DEC lofi turbo channel card */

#include "LoFi.h"
/* #include "LoFiMap.h" */

/* #define DEBUG */

#ifdef DEBUG
#define DBGSIZ 1024L
static long dumb1[DBGSIZ];
static long dumb2[DBGSIZ];
#endif

/* constants 		- but see MyBootLofi() for LOD file filenames & path */
#define MAXWAIT     1000000L	/* maximum number of polls before giving up */

#define ALIVEF	    0x0FF0	/* dsp var: flag set when other regs ready */
#define SRATE	    0X0FF1	/* host notified DSP of sampling rate requested */
#define HBSIZE	    0x0FF2	/* dsp var holds total size of host buffer (samples) */
#define HBBLCK	    0x0FF3	/* dsp var holds min block size by which HB advances */
#define HOBBASE	    0x0FF4	/* dsp var holds base of host output buffer */
#define HOBPNTR	    0x0FF5	/* dsp var holds current upper limit on host buffer */
#define HIBBASE	    0x0FF6	/* dsp var holds base of host input buffer */
#define HIBPNTR	    0x0FF7	/* dsp var holds current upper limit on host buffer */

/* static globals */
static	int		hbSize;
static	int		hbBlck;
static	unsigned long	*hobBase;
static	unsigned long	*phobPtr;
static	int		hobPtr = 0;
static	unsigned long	*hibBase;
static	unsigned long	*phibPtr;
static	int		hibPtr = 0;

static	char		lofion = 0;	/* LoFi booted flag */

/* #define RdHoBPtr()       ( (int)((*phobPtr)>>8L) ) */
/* #define RDHOBP       ((*phobPtr)>>8L)	*/
/* MACRO to read & scale the host output buf pointer */

int RdHoBPtr()
    {
    return( (int)((*phobPtr)>>8L) );
    }

static dumyct;
int Dummy(i)
    int i;
    {
/*    if(i == 0)
	dumyct = 0;
    else
*/	dumyct += i;
/*    if(dumyct == 1000000)
	{
	LoFiSetCSR(FED, 0);
	exit(-1);		
	}	/* kill dsp mid stream to examine w/ hwddt */
    return(0);
    }

/* #define RdHiBPtr(a)       ( (int)((*phibPtr)>>8L) )	*/
/* MACRO to read & scale the host input buf pointer */
/* somehow doesn't work in line -- Ultrix scheduler? Tying up t/chan? */
int RdHiBPtr()
    {
    return( (int)((*phibPtr)>>8L) );
    }


long RdDSPadr(int adr)		/* read long from DSP valid address */
    {
#ifdef DEBUG
    return( DBGSIZ );
#else
    return( (*(LoFiMap(RRAM, adr, 0)))>>8L );
#endif
    }

void WrDSPadr(int adr, long data)
    {
#ifdef DEBUG
    return();
#else
    *(LoFiMap(RRAM, adr, 1)) = data<<8L;
#endif
    }

old_play(stt,end,chans,dsize,srate,scale)	/* called from main place */
    short   *stt;
    short   *end;
    int     chans;
    int     dsize;	/* ignored - assumed = sizeof(short) in what follows */
    float   srate;
    int     scale;	/* crude gain - ignored here */
    {
    register unsigned long *b1;
    register int     chuSize,offs;
    register short   *src;
    int     hbRun;	/* running index into host buffer */
    int     s2;
    long    timeout;
    int	    i;

    if(lofion == 0)
	if(MyBootLoFi((float)srate)==0)	return;	    /* LoFi boot failed */
    if(chans == 1) offs = 0;  else offs = 1;

    src   = stt;
    hbRun = hobPtr;		/* right after last location we wrote */
    while(src < end)		/* samples still to send */
	{
	s2 = end-src;

	hobPtr = RdHoBPtr();	/* right way */
	if(hobPtr == hbRun)
	    {
	    timeout = MAXWAIT;
	    do  {
	    	hobPtr = RdHoBPtr(); /* right way */
		} while(hobPtr == hbRun && --timeout);
	    if(timeout == 0L)
		{
		fprintf(stderr,"Timeout waiting for DSP to advance\n");
		break;
		}
	    }
	chuSize = hobPtr - hbRun;		    /* how big is the gap ? */
	if(chuSize < 0)     chuSize += hbSize;	    /* modulo arithmetic */
/*	if(chuSize >s2)     chuSize = s2;   */

	b1 = &(hobBase[hbRun]);
	s2 =chuSize;
	chuSize /= 2;    /* write 2 samples per loop */
	while(chuSize--)
	    {
	    *b1++ = ((long)*src)<<16;     /* o/p assu ilvd stereo */
	    *b1++ = ((long)*(src+offs))<<16; /* (mono op: L&R 1 skew) */
	    src += chans;
	    }
        if(hbRun >= hbSize)     hbRun -= hbSize;	/* check o/s loop? */
	hbRun = hobPtr;		/* that's what we were trying for anyway */
	}
    }

play(stt,end,chans,dsize,srate,scale)	/* called from main place */
    short   *stt;
    short   *end;
    int     chans;
    int     dsize;	/* ignored - assumed = sizeof(short) in what follows */
    float   srate;
    int     scale;	/* crude gain - ignored here */
    {
    short 	*src;
    int		chunkSamps;
    int		bestSize;

    play_set(chans,dsize,srate,scale);

    src = stt;
    bestSize = (hbBlck/2)*chans; /* hblck/2 is frames per blk */
    while(src < end)		/* samples still to send */
	{
	chunkSamps = end-src;
	if(chunkSamps>bestSize) chunkSamps=bestSize;
	play_on(src,chunkSamps);
	src += (chunkSamps);	/* cs/2 is num of frames */
	}
    LoFiClose();		/* release for parallel code */
    }

static int opbsiz;
static int ourpos;
static int bufspc;
static int schans;
static int soffs;

static int ropbsiz;
static int rourpos;
static int rbufspc;
static int rschans;
static int rsoffs;

play_set(chans,dsize,srate,scale)	/* set up for fixed size calls  */
    int     chans;
    int     dsize;	/* ignored - assumed = sizeof(short) in what follows */
    float   srate;	/* passed to LoFi boot if required */
    int     scale;	/* crude gain - ignored here */
    {
    if(lofion == 0)
	if(MyBootLoFi((float)srate)==0)	return;	    /* LoFi boot failed */
    opbsiz = hbSize;
    schans = chans;
    soffs  = (chans == 1)? 0 : 1;		    /* mono/stereo o/p */
    bufspc = 0;
    ourpos = -1; /* RdHoBPtr(); */
    }

play_on(buf,siz)	/* just write next buffer, length implicit */
    short *buf;
    int   siz;		/* must be multiple of block size & nchans! */
    {
    register unsigned long 	*hbptr;
    register short		*src;
    register int		frames;
    register int		offs;
    register int		chs;
    register int		dsppos;
    register int		samps;

    int	   dbgcnt = 0;		/* count # lofi accesses */

    /* ourpos already set up ? */
    if(ourpos < 0)
	{
	do  {
	    dsppos = RdHoBPtr();
	    ourpos = dsppos;
	    } while(ourpos/* %siz */);	/* must align - TO 0! */
	bufspc = 0;
	rourpos = RdHiBPtr();		/* record too */
	rbufspc = 0;
	}
    src   = buf;
    chs   = schans;
    frames= siz/chs;	/* use up a frame each loop, so ..  */
    samps = frames * 2;	/* always output stereo - 2 samps/frame */
    offs  = soffs;
    hbptr = &hobBase[ourpos];
    Dummy(0);
    while(bufspc < samps)
	{
	dsppos = RdHoBPtr();
	bufspc = (dsppos-ourpos+opbsiz)%opbsiz;
	Dummy(1);
	++dbgcnt;
	}
    do	{
	*hbptr++ = ((long)*src)<<16;
	if(offs)
	    *hbptr++ = ((long)*(src+offs))<<16;
	else
	    hbptr++;
	src += chs;
	} while(--frames);
    ourpos = (ourpos + samps)%opbsiz;
    bufspc -= samps;	
/*    dsppos = RdHoBPtr();
    bufspc = (dsppos-ourpos+opbsiz)%opbsiz;	*/

/*    printf(" %d",dbgcnt);	*/
    }

rec_set(chans,dsize,srate,scale)	/* set up for fixed size calls  */
    int     chans;
    int     dsize;	/* ignored - assumed = sizeof(short) in what follows */
    float   srate;	/* passed to LoFi boot if required */
    int     scale;	/* crude gain - ignored here */
    {
    if(lofion == 0)
	if(MyBootLoFi((float)srate)==0)	return;	    /* LoFi boot failed */
    ropbsiz = hbSize;
    rschans = chans;
    rsoffs  = (chans == 1)? 0 : 1;		    /* mono/stereo o/p */
    rbufspc = 0;
    rourpos = 0; /* RdHiBPtr(); */
    printf("rec: chans %d offs %d bsiz %d\n",rschans,rsoffs,ropbsiz);
    }

rec_on(buf,siz)	/* just write next buffer, length implicit */
    short *buf;
    int   siz;		/* must be multiple of block size & nchans! */
    {
    register unsigned long 	*hbptr;
    register short		*src;
    register int		frames;
    register int		offs;
    register int		chs;
    register int		dsppos;
    register int		samps;

    int	   dbgcnt = 0;		/* count # lofi accesses */

    /* ourpos already set up */
    src   = buf;
    chs   = rschans;
    frames= siz/chs;	/* use up a frame each loop, so ..  */
    samps = frames * 2;	/* always output stereo - 2 samps/frame */
    offs  = rsoffs;
    hbptr = &hibBase[rourpos];
    Dummy(0);
    while(rbufspc < samps)
	{
	dsppos = RdHiBPtr();
	rbufspc = (dsppos-rourpos+ropbsiz)%ropbsiz;
	Dummy(1);
	++dbgcnt;
	}
    do	{
	if(offs)
	    *src = (((long)*hbptr++)>>16L);
	else
	    hbptr++;	/* only mono */
	*(src+offs) = (((long)*hbptr++)>>16L);	/* R overwrites mono */
	src += chs;
	} while(--frames);
    rourpos = (rourpos + samps)%ropbsiz;
    rbufspc -= samps;	
/*    dsppos = RdHiBPtr();
    rbufspc = (dsppos-rourpos+ropbsiz)%ropbsiz;	*/

/*    printf(" %d",dbgcnt);	*/
    }


record(stt,end,chans,dsize,srate,scale)	/* called from main place */
    short   *stt;
    short   *end;
    int     chans;
    int     dsize;	/* ignored - assumed = sizeof(short) in what follows */
    float   srate;
    int     scale;	/* crude gain - ignored here */
    {
    register unsigned long *b1,*b2;
    register int smpSize,offs;
    register short   *src;
    int     hbRun;	/* running index into host buffer */
    int     s2;
    long    timeout;

    if(lofion == 0)
	if(MyBootLoFi((float)srate)==0)	return;	    /* LoFi boot failed */
    src = stt;
    if(chans == 1) offs = 0;  else offs = 1;
    hbRun = hibPtr;		/* right after last location we wrote */
    while(src < end)		/* samples still to send */
	{
	timeout = MAXWAIT;
	s2 = end-src;
	while( ((hibPtr = RdHiBPtr()) == hbRun) && --timeout) /* wait for space to open */
	    ;
	if(timeout == 0L)
	    {
	    fprintf(stderr,"Timeout waiting for DSP to advance\n");
	    break;
	    }
	smpSize = hibPtr - hbRun;		    /* how big is the gap ? */
	if(smpSize < 0)     smpSize += hbSize;	    /* modulo arithmetic */
	/*	if(smpSize >s2)     smpSize = s2;   */

	b1 = &(hibBase[hbRun]);
	b2 = &(hibBase[hbSize]);
	s2 =smpSize;
	smpSize /= 2;    /* read 2 samples per loop */
	while(smpSize--)
	    {
	    *src = (((long)*b1++)>>16L);
	    *(src+offs) = (((long)*b1++)>>16L);	/* R overwrites mono */
/*	    *b1++ = ((long)*src)<<16;     /* o/p assu ilvd stereo */
/*	    *b1++ = ((long)*(src+offs))<<16; /* (mono op: L&R 1 skew) */
	    src += chans;
	    }
/*	if(b1 >= b2)     b1 -= hbSize;	/* check o/s loop? */
/*	smpSize = s2;
	hbRun += smpSize;
        if(hbRun >= hbSize)     hbRun -= hbSize;	/* check o/s loop? */
	hbRun = hibPtr;
	}
/*    hibPtr = hbRun;		/* remember where we got to */	
    }

MyBootLoFi(srate)
     float srate;
    {
    long timeout;
    char *bootFile;
    char bootPath[128];
    char *loddir, *getenv();

    if((void *)LoFiOpen("/dev/lofi") == NULL)
	{
	fprintf(stderr,"Cannot open LoFi\n");
	return(0);
	}
    loddir = getenv("LODDIR");
    if(loddir == NULL)
	{
	fprintf(stderr,"no LODDIR env variable - can't find .LOD files\n");
	return(0);
	}
    if(srate > 36000)        bootFile = "play42.lod";
    else if(srate > 26000)   bootFile = "play30.lod";
    else if(srate > 18000)   bootFile = "play21.lod";
    else if(srate > 13000)   bootFile = "play15.lod";
    else if(srate > 9500)    bootFile = "play11.lod";
    else 		     bootFile = "play8.lod";

#ifdef AES	/* output via tom's mac aes board */
    bootFile = "playaes.lod";
#endif

    sprintf(bootPath,"%s/%s",loddir,bootFile);
    printf("bootfile: %s\n",bootPath);

    LoFiSetCSR(FED, 0);		/* disable DSP */
    dspLoad(bootPath);		/* load code */
    WrDSPadr(ALIVEF, 0L);	/* mark DSP as unbooted */
    WrDSPadr(SRATE, (long)srate);   /* notify requested sample rate */
    LoFiSetCSR(FED, 1);		/* reboot DSP */
    timeout = MAXWAIT;
    while( RdDSPadr(ALIVEF) == 0L && --timeout)
	;
    if(timeout == 0L)
	{
	fprintf(stderr,"Timeout waiting for DSP to boot\n");
	return(0);
	}
    hobBase = LoFiMap(RRAM,(int)RdDSPadr(HOBBASE),1);
    hibBase = LoFiMap(RRAM,(int)RdDSPadr(HIBBASE),1);
    hbSize = (int)RdDSPadr(HBSIZE);
    hbBlck = (int)RdDSPadr(HBBLCK);	 /* only set these once at boot */
    phobPtr = LoFiMap(RRAM, HOBPNTR, 0); /* set up address to read ptr from */
    phibPtr = LoFiMap(RRAM, HIBPNTR, 0);

    printf("hobBase 0x%lx, hbSize %d, hbBlck %d\n",hobBase,hbSize,hbBlck);
    lofion = 1;
    return(1);
    }

/* dummys so that LoFiMap.c will link */
void msg(char *s)
    {
    printf("%s\n",s);
    }
GetAndPrintEvent(long *m,FILE *n)
    {
    }

#else /* LOFI not defined, just placeholders */

play_set(chans,dsize,srate,scale)	/* set up for fixed size calls  */
    int     chans;
    int     dsize;	/* ignored - assumed = sizeof(short) in what follows */
    float   srate;	/* passed to LoFi boot if required */
    int     scale;	/* crude gain - ignored here */
    {
    }

play_on(buf,siz)	/* just write next buffer, length implicit */
    short *buf;
    int   siz;		/* must be multiple of block size & nchans! */
    {
    }

rec_set(chans,dsize,srate,scale)
    int     chans;
    int     dsize;
    float   srate;
    int     scale;
    {
    }

rec_on(buf,siz)
    short *buf;
    int   siz;
    {
    }

LoFiClose()
    {
    }

#endif
