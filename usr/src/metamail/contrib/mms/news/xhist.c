#ifndef lint
static char	*sccsid = "@(#)xhist.c	1.1	(ETL) 9/20/90";
#endif

/*#####################################################################
 *	Bnews(2.11.17-19) history scanner
 *		1990 Yutaka Sato <ysato@etl.go.jp>
 *#####################################################################
 * (This program can be put into NNTP server)
 *
 * XHIST [<Message-ID>|selector]
 *
 *	selector is list of:
 *		YYMMDD:HHMMSS[-YYMMDD:HHMMSS]	History range
 *		$				Last(latest) history line
 *		/regular-expression/		History lines matches the exp.
 *		+nnn				Ignore arts before nnn'th art.
 *		-nnn				Ignore arts after nnn'th art.
 *		! selector
 * e.g.,
 * XHIST <123@etl.go.jp>	retrieve subject of <123@etl.go.jp>
 * XHIST 900701			retrieve since 1990 Jul. 1
 *
 * This command is an extention, and not included in RFC 977.
 */

int IS_CNEWS;

#ifdef CLIENT
#   include <dbm.h>
#   undef NULL
#   include <stdio.h>
#else
#ifndef HISTORY_SERVER
#   include "../common/conf.h"
#   include "common.h"
#endif
#   ifdef CNEWS
int IS_CNEWS = 1;
#   endif
#endif

#define LASTLINE_COM '$'
char LASTLINE_COMMAND[] = {LASTLINE_COM,0};

#ifndef CLIENT

static char *usage =
"Usage: XHIST {<Message-Id>|YYMMDD:HHMMSS[-YYMMDD:HHMMSS]|$}";
static putusg(){ printf("%d %s\r\n",ERR_CMDSYN,usage); }

xhist(ac,av)
	char *av[];
{	int i;

	if(ac <= 1){
		putusg();
		fflush(stdout);
	}else
	switch(av[1][0]){
		default:
			xhist_by_date_range(av[1]);
			break;
		case '<':
			xhist_by_message_id(av[1]);
			break;
		case '/':
			putusg();
			break;
		case LASTLINE_COM:
			xhist_tail();
			break;
	}
	fflush(stdout);
}

static xhist_tail(){
	char line[2048];

	Last_history_line(line);
	if( *line )
		printf("%d %s\r\n",OK_HEAD,line);
	else	printf("%d Cant access last article\r\n",ERR_NOART);
}

static xhist_by_date_range(drange)
	char *drange;
{
	int fromdate,fromtime,todate,totime;
	char line[1000],hbuf[512];
	FILE *fp,*xfp;
	int offset;

	fromdate = 0; fromtime = 0;
	todate = 991231; totime = 246000;
	if(sscanf(drange,"%d:%d-%d:%d",&fromdate,&fromtime,&todate,&totime)==0){
		putusg();
		return;
	}

	fp = fopen(HISTORY_FILE,"r");
	if(fp == NULL){
		putusg();
		return;
	}

	printf("%d selected history lines follow\r\n", OK_HEAD);
	setbuffer(fp,hbuf,sizeof(hbuf));
	offset = history_bsearchx(fp,fromdate,fromtime);
	xfp = fdopen(fileno(fp),"r");
	fseek(xfp,offset,0);
	while( fgets(line,sizeof(line),xfp) != NULL )
		fputs(line,stdout);
	puts(".");
	fclose(xfp);
	fclose(fp);
}
static history_bsearchx(fp,xdate,xtime)
	FILE *fp;
{	int xclock;

	xclock = to_gmt(xdate/10000,(xdate/100)%100,xdate%100,
		      xtime/10000,(xtime/100)%100,xtime%100);
	return history_bsearch(fp,xclock);
}

static xhist_by_message_id(mid)
	char *mid;
{	char xmid[1000],line[10000];

	if(*mid != '<')
		sprintf(xmid,"<%s>",mid);
	else	strcpy(xmid,mid);

	if( 0 < MessageID_to_history_line(xmid,line,sizeof(line)))
		printf("%d %s\r\n",OK_HEAD,line);
	else	printf("%d No article by message-id %s\r\n",ERR_NOART,xmid);
}

#endif	/* !CLIENT */

static history_bsearch(fp,date)
	FILE *fp;
{	int fsize,offs,offset;
	int itime;
	char line[1000];

	fsize = FILE_SIZE(fp);
	offs = fsize / 2;
	fseek(fp,offs,0);
	offset = 0;

	while( 80 < offs ){
		fgets(line,sizeof(line),fp);
		offset = ftell(fp);
		fgets(line,sizeof(line),fp);
		itime = decomp_history_line(line,0,0);
		if( itime <= 0 )
			return fsize;

		offs /= 2;
		if( date < itime )
			fseek(fp,-offs,1);
		else	fseek(fp, offs,1);
	}
	return offset;
}

#ifdef _DBM_RDONLY /* maybe ndbm */
DBM *Hdbm;
#define	DBMINIT(name)	(Hdbm = dbm_open(name,0,0))
#define	DBMCLOSE(name)	dbm_close(Hdbm)
#define DBMFETCH(key)	dbm_fetch(Hdbm,key)
#else
#define DBMINIT(name)	dbminit(name)
#define	DBMCLOSE(name)	dbmclose()
#define DBMFETCH(key)	fetch(key)
datum fetch();
#endif

MessageID_to_history_line(mid,line,linesize)
	char *mid,*line;
{	FILE *HFP;	/* History File		*/
	datum key,data;
	int offset;
	unsigned char *cp,midb[1000];

	if( DBMINIT(HISTORY_FILE) < 0 )
		return 0;

	mid = (char*)strcpy(midb,mid);
	if( IS_CNEWS ){
		if( cp = (unsigned char *)rindex(mid,'@') )
			tolowers(cp);
	}else	tolowers(mid);

	key.dptr = mid;
	key.dsize = strlen(key.dptr) + 1;
	data.dptr = 0;
	data = DBMFETCH(key);
	DBMCLOSE();

	if(data.dptr == 0)
		return -1;

	if(cp = (unsigned char*)data.dptr){
		if((HFP = fopen(HISTORY_FILE,"r")) == NULL)
			return 0;
		offset = cp[0]<<24 | cp[1]<<16 | cp[2]<<8 | cp[3];
		fseek(HFP,offset,0);
		fgets(line,linesize,HFP);
		fclose(HFP);
		linesize = strlen(line);
		if(linesize && line[linesize-1] == '\n')
			line[linesize-1] = 0;
		return 1;
	}
	return 0;
}
Last_history_line(line)
	char *line;
{	FILE *fp;
	char buf[2048],*bp;

	*line = 0;
	fp = fopen(HISTORY_FILE,"r");
	if( fp != NULL ){
		fseek(fp,-sizeof(buf)+2,2);
		fscanf(fp,"%[^\377]",buf);
		fclose(fp);

		buf[strlen(buf)-1] = 0; 
		if( bp = (char*)rindex(buf,'\n') )
			strcpy(line,bp+1);
	}
}


/*
 *	Decomposit 4-formats of history line
 */
static decomp_history_line(line,id,xref)
	char *line,*id,*xref;
{	int day,month,year,hour,minute,itime;
	char tid[1000],txref[5000],*dp;
	int nitem,optdate;

	if( id == 0 ) id = tid;
	if( xref == 0 ) xref = txref;

	year = 0;
	*xref = 0;

	nitem = sscanf(line,"%s %d/%d/%d %d:%d %d %[^\n\r]",
		id,&month,&day,&year,&hour,&minute,&optdate,xref);
	if( 7 <= nitem ){
		itime = to_gmt(year,month,day,hour,minute,0);
		return itime;
	}

	nitem = sscanf(line,"%s %d/%d/%d %d:%d %[^\n\r]",
		id,&month,&day,&year,&hour,&minute,xref);
	if( 7 == nitem ){
		itime = to_gmt(year,month,day,hour,minute,0);
		return itime;
	}

/* new Bnews or Cnews */
	nitem = sscanf(line,"%s %d %[^\n\r]",id,&itime,xref);
	if( nitem == 3 ){
		if( *xref != '~' )
			return itime;
		if( dp = (char*)index(xref,'\t') )
			strcpy(xref,dp+1);
		else	*xref = 0;
		return itime;
	}

	return 0;
}

/*
#include <time.h>
static to_gmt(year,month,mday,hour,min,sec){
	static struct tm tm;

	tm.tm_year = year;
	tm.tm_mon = month - 1;
	tm.tm_mday = mday;
	tm.tm_hour = hour;
	tm.tm_min = min;
	tm.tm_sec = sec;
	return timegm(&tm);
}
*/
/*
 *	LOCAL-TIME TO GMT-CLOCK (unreliable coding ;-)
 */
static int month_base[] =
	{ 0,31,59,90,120,151,181,212,243,273,304,334,1000 };
static to_gmt(year,month,mday,hour,min,sec){
	int md;

	if( month < 1 || 12 < month )
		return 0;
	year -= 70; month -= 1; mday -= 1;
	md = ((((year-2) % 4) == 0) && (1 < month)) ? 1 : 0;
	return ( ( ( year*365 + month_base[month] + md + mday + (year+1)/4
	    	    ) * 24 + hour ) * 60 + min) * 60 + sec;
}

#ifndef CLIENT
#include <ctype.h>
static tolowers(str)
	char *str;
{	register char *s;

	for(s = str; *s; s++)
		if(isupper(*s))
			*s = tolower(*s);
}

#include <sys/types.h>
#include <sys/stat.h>
static FILE_SIZE(fp)
	FILE *fp;
{	static struct stat status;

	if( fstat(fileno(fp),&status) < 0 )
		return 0;
	else	return status.st_size;
}
#endif	/* !CLIENT */
