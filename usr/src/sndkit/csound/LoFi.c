/*******************************************************\
*	LoFi.c 						*
* All the .c code needed to drive the LoFi soundout     *
* grouped by dpwe 05oct90				*
\*******************************************************/

#ifdef LOFI	/* lofi output is available */

/***** was LoFiMap.c ******/

#include	<stdio.h>
#include	<ctype.h>
#include	<strings.h>
/*  #include	<time.h>	*/

#include	<sys/file.h>
#include	<sys/ioctl.h>

/*  #include	"lofi_reg.h"	*/
/*  #include	"lofi.h"	*/
/*  #include	"LoFiMap.h"	*/
/***** above all grouped in.. ****/
#include	"LoFi.h"

#define	NBUF	1024
#define	NWTOP	(sizeof(struct lofi_reg) / 4)
#define	NWDROM	(32*1024)
#define	NWDRAM	(32*1024)
#define	NWDCODEC (8)	
#define	NWDHOST	 (8)
#define	NWDCSR	 (1)
#define	NWDOPTION (16)	

/* static */
struct lofi_info *lofi;
struct lofi_reg *lp;
extern int errno;

unsigned long int lcount=0;

/* forwards */
long	*check_bounds();

static int	fd = -1;

struct lofi_reg *
LoFiOpen(devp)
char	*devp;
{
    if ((fd = open(devp, O_RDWR, 0)) < 0) {
	fprintf(stderr, "can't open %s, errno = %d\n", devp, errno);
	exit(1);
    }
    if (ioctl(fd, QIOLOFIINFO, &lofi) < 0) {
	fprintf(stderr, "QIOLOFIINFO failed, errno = %d\n", errno);
	exit(1);
    }
    lp = lofi->us_reg;
    return(lp);
}

void
LoFiClose()	/* dpwe 08sep90 : release the lofi as opened */
    {
    if( !(fd < 0) )
	close(fd);
    }

unsigned long *
LoFiMap(region, offset, rwflag)
int	region;
int	offset;
int	rwflag;
{
   unsigned long *p;

    p = (unsigned long *)lp;
    switch(region){
    case RPHYSICAL:
	if ((offset) >= NWTOP) return(NULL);
	return(&p[offset]);
	break;
    case RRAM:
	p = (unsigned long*) &lp->sram_map(0);
	if ((offset) > NWDRAM) return(NULL);
	return(&p[offset]);
	break;
    case RROM:
	p = (unsigned long*) &lp->rom_map(0);
	if ((offset) > NWDROM) return(NULL);
	return(&p[offset]);
	break;
    case RCSR:
	p = (unsigned long*) &lp->rd_csr;
	if ((offset) > NWDCSR) return(NULL);
	return(&p[offset]);
	break;
    case RCODEC0:
	p = (unsigned long*) &lp->codec0(0);
	if ((offset) > NWDCODEC) return(NULL);
	return(&p[offset]);
	break;
    case RCODEC1:
	p = (unsigned long*) &lp->codec1(0);
	if ((offset) > NWDCODEC) return(NULL);
	return(&p[offset]);
	break;
    case ROPTION:
	p = (unsigned long*) &lp->woption(0);
	if ((offset) > NWDOPTION) return(NULL);
	return(&p[offset]);
	break;
    case RHOST:
	if (rwflag == 1) p = (unsigned long*) &lp->rd_host(0);
	else p = (unsigned long*) &lp->wr_host(0);
	if ((offset) > NWDHOST) return(NULL);
	return(&p[offset]);
	break;
    default:
	return(NULL);
    }
}

int
LoFiRead(p, length, buf)
unsigned long *p;
int	length;
unsigned long	*buf;
{

    if(p==NULL) return (0);
#if	0
    if((&p[length] - (unsigned long *)&lp) > (sizeof(struct lofi_reg))) return (0);
#endif
    LoFiReadBlock(p, length, buf);
    return(p- (unsigned long*) lp);
}

int
LoFiWrite(p, length, buf)
unsigned long *p;
int	length;
unsigned long	*buf;
{
    if(p == (unsigned long *)NULL) return(0);
#if	0
    if((&p[length] - (unsigned long *)&lp) > (sizeof(struct lofi_reg))) return (0);
#endif
    LoFiWriteBlock(p, length, buf);
}

void
LoFiSetCSR(field, val)
int	field;
int 	val;
{
    int	    fa;
    long    image;

    switch(field){
    case FCA:
	fa = 0;
	break;
    case FHS:
	fa = 1;
	break;
    case FEA:
	fa = 2;
	break;
    case FED:
	fa = 3;
	break;
    case FMD:
	fa = 4;
	break;
    case FIE:
	fa = 5;
	break;
    case FGC:
	fa = 6;
	break;
    default:
	msg("unknown csr field"); return;
    }
    image = (fa<<28) | (val << 24);
    lp->wr_csr = image;
}

long
LoFiReadCSR()
{
	return(lp->rd_csr);

}


void
LoFiPrintEvent()
{
	GetAndPrintEvent(lofi,stdout);
}

void
LoFiReadBlock(p, size, buf)
unsigned long	*p, *buf;
int	size;
{
    int	i;
    for(i=0;i<size;++i)
	buf[i] = p[i];

}

void
LoFiWriteBlock(p, size, buf)
unsigned long	*p,*buf;
int	size;
{
    int	i;
    for(i=0;i<size;++i)
	p[i] = buf[i];

}

/******* was dsp.c *******/

/*  #include	<stdio.h>	*/
/*  #include	<ctype.h>	*/
/*  #include	<strings.h>	*/
/*  #include	"hwddt.h"	*/

#define	EOS	'\0'
#define	NEWREC	'_'
#define	NIBUF	256

#define	FIELD	0
#define	RECORD	1

#define	RNULL	0
#define	RSTART	1
#define	RDATA	2
#define	RBDATA	3
#define	REND	4
#define	RCOMMENT 5
#define	RSYMBOL	6

#if	DEBUG
main(argc,argv)
int	argc;
char	**argv;
{
    dspLoad(argv[1],0);
}
#endif

void
dspLoad(file)
char	*file;
{
    FILE	*fp;
    int		type;
    char	ibuf[NIBUF];
    int		current=RNULL;
    int		lda, ldd;
    unsigned long	*p;

    if ((fp=fopen(file,"r"))==NULL){
	printf("cannot read %s\n",file);
	return;
    } 

    p = LoFiMap(RRAM,0,0);
    for(;;){
	if(current == RNULL){
	    if((type = get_field(fp,ibuf)) == EOF) return;
	    if(type == RECORD)
		current = RecordType(ibuf);
	}

	switch(current){
	case RSTART:
	    if(get_field(fp,ibuf) == EOF) return; /* id */
	    if(get_field(fp,ibuf) == EOF) return; /* version */
	    if(get_field(fp,ibuf) == EOF) return; /* revision */
	    if(get_comment(fp, ibuf) == EOF) return;
	    current = RNULL;
	    break;
	case REND:
	    if((type=get_field(fp,ibuf)) == EOF) return; /* optional addr */
	    if(type == RECORD)
	        current = RecordType(ibuf);
	    break;
	case RSYMBOL:
	    fprintf(stderr,"do not understand symbols\n");
	    return;
	    break;
	case RDATA:
	    if(get_field(fp,ibuf) == EOF) return; /* memory */
	    if(get_field(fp,ibuf) == EOF) return; /* addr */
	    sscanf(ibuf,"%X",&lda);
	    while((type=get_field(fp, ibuf))!=EOF && type != RECORD){
		sscanf(ibuf,"%X",&ldd);
		/*	printf("%08x: %08x\n",lda,ldd);    */
		if(lda > 0x8000) lda -= 0x8000;
		p[lda] = ldd<<8;		
		++lda;
	    }
	    current = RecordType(ibuf);
	    break;
	case RBDATA:
	    if(get_field(fp,ibuf) == EOF) return; /* memory */
	    if(get_field(fp,ibuf) == EOF) return; /* addr */
	    if(get_field(fp,ibuf) == EOF) return; /* count */
	    if(get_field(fp,ibuf) == EOF) return; /* value */
	    fprintf(stderr,"Do not understand block data yet.\n");
	    current = RNULL;
	    break;
	case RCOMMENT:
	    if (get_comment(fp,ibuf) == EOF) return;
	    current = RNULL;
	    break;
	case RNULL:
	default:
	    fprintf(stderr,"null record type, %s\n",ibuf);
	    exit(1);
	}
    }
}

RecordType(buf)
char	*buf;
{
    if (strcmp("_START",buf)==0) return(RSTART);
    if (strcmp("_SYMBOL",buf)==0) return(RSYMBOL);
    if (strcmp("_END",buf)==0) return(REND);
    if (strcmp("_DATA",buf)==0) return(RDATA);
    if (strcmp("_BLOCKDATA",buf)==0) return(RBDATA);
    if (strcmp("_COMMENT",buf)==0) return(RCOMMENT);

    return(RNULL);
}

/* 
* EOF, 0, or 1
*/ 
get_field(fp, buf)
FILE	*fp;
char	*buf;
{
    register int	c;
    register char	*p;

    while((c=fgetc(fp))!=EOF && isspace(c))
	    ;
    if (c == EOF) return (EOF);
    for(p=buf, *p++=c; (c=fgetc(fp))!=EOF && !isspace(c); *p++=c)
	    ;
    *p = EOS;
    if (c!=EOF) ungetc(c, fp);
    return(*buf == NEWREC ? RECORD : FIELD);	
}

get_comment(fp, buf)
FILE	*fp;
char	*buf;
{
    register int	c;
    register char	*p;

    while((c=fgetc(fp))!=EOF && c!='\n' && isspace(c))
	    ;
    if (c == EOF || c!='\n') return (EOF);
    for(p=buf; (c=fgetc(fp))!=EOF && c!='\n'; *p++=c)
	    ;
    *p = EOS;
    return(0);
}

#endif /* def LOFI */
