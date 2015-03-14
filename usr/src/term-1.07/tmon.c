/*
From hendrick@edmund.cs.andrews.edu Tue Dec 29 10:02:07 1992
Return-Path: <hendrick@edmund.cs.andrews.edu>
Received: from edmund.cs.andrews.edu by tartarus.uwa.edu.au (5.65c/SMI-4.1)
	id AA07713; Tue, 29 Dec 1992 10:02:02 +0800
Received: by edmund.cs.andrews.edu (5.57/Ultrix3.0-C)
	id AA11714; Mon, 28 Dec 92 21:01:39 -0500
From: hendrick@edmund.cs.andrews.edu (John Hendrickson)
Message-Id: <9212290201.AA11714@edmund.cs.andrews.edu>
Subject: tmon
To: oreillym
Date: Mon, 28 Dec 92 21:01:37 EST
X-Mailer: ELM [version 2.3 PL11]
Status: O
*/

#include <stdio.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/times.h>
#include <sys/time.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <ctype.h>
#include <signal.h>
#if !defined(NeXT)
#  include <termio.h>
#endif
#include <memory.h>

#define I_SYS

#include "includes.h"
#include "client.h"

#ifndef HZ
#define HZ 60
#endif

#define STAT(local, st) \
  if (send_command(s, C_STATS, local, "%d",st)< 0) { \
     fprintf(stderr,"C_STATS command failed. Abort.\n"); \
     exit(1); \
     }

#define NULLS ((char *)0)
int	LI,	/* One less than screen length in termcap entry */
	CO;	/* Screen width */

int debug = 0;

static char tc[2048];	/* termcap buffer */
static char tbuf[128], PC, *CD, *CE, *CF, *CL, *CM, *CN, *SO, *SE;

char *tgetstr ();
char *tgoto ();
int tgetent ();
int tgetnum ();
char *getenv ();
void tputs();

#define Tgetstr(code) (char *)((s = (char *)tgetstr(code,&p)) ? s : "")

/*	initialize termcap stuff */
void get_ttype()
{
	char *ttytype;
	char *p = tbuf;
	char *s;

	if ((ttytype = getenv("TERM")) == NULLS) {
		printf ("TERM not set\n");
		exit(6);
	}
	if (tgetent(tc, ttytype) != 1) {
		printf("Can't load %s", ttytype);
		exit(7);
	}
	LI = tgetnum("li") - 1;
	CO = tgetnum("co");
	if ((s=Tgetstr("pc")) == NULLS)
		PC = '\0';
	else
		PC = (char)*s;
	
	CD = Tgetstr("cd");  /* clear display after cursor */
	CE = Tgetstr("ce");  /* clear to end of line */
	CL = Tgetstr("cl");  /* clear screen */
	CM = Tgetstr("cm");  /* move cursor sequence */
	SE = Tgetstr("se");  /* standout mode off */
	SO = Tgetstr("so");  /* standout mode on */
	CF = Tgetstr("vi");  /* cursor off string */
	CN = Tgetstr("ve");  /* cursor on string */
}

void putchr ( c )
char c;
{
	fputc ( c, stdout );
}

void cls()		{ tputs(CL,LI,putchr); }

void cur_on()	{ tputs(CN,1,putchr); }

void cur_off()	{ tputs(CF,1,putchr); }


void cl_line()	{ tputs(CE,1,putchr); }

void cl_end()	{ tputs(CD,LI,putchr); }

void ttgoto(row, col)
int row, col;
{
	tputs(tgoto(CM, col, row),1,putchr);
}

void drawline(row, col, len)
int row, col, len;
{

	ttgoto(row, col);
	while (len--)
		fputc('-', stdout);
}

void main(int argc, char *argv[]) 
{
  struct timeval timeout;
  int first;
  int last_in, last_out, d1, d2, i, x, y, ihist[33],ohist[33];
  int scale=1024, timer=5;
  int s, done, stim;
  int cooked_out=0;
  fd_set reads;
  char line[35];
  float lcomp,rcomp,dtim;
  struct tms Tbuf;

  timeout.tv_sec = 0;
  timeout.tv_usec = 0; 

  /* set_nonblock(0); */
  priority = 3;
  first = client_options(argc, argv,"",NULL);

  s = connect_server(term_server);
  terminal_save(0);
  terminal_raw(0);
  STAT ( 1, -5);

  scale = atoi(command_result) / 10 + 10;

  STAT ( 1, -3);
  sscanf ( command_result, "%d %d", &last_in, &last_out );
  stim=times(&Tbuf);
  STAT ( 1, -8);
  cooked_out = atoi(command_result);

  memset ( line, 0, sizeof (line) );
  memset ( ihist, 0, sizeof (ihist) );
  memset ( ohist, 0, sizeof (ohist) );

  get_ttype ();
  cls ();

  ttgoto ( 1, 1 );
  printf ( "TERM link statistics monitor v.01" );
  for (i=3;i<22;i++){ ttgoto(i,1); putchr('|'); }
  for (i=3;i<22;i++){ ttgoto(i,41); putchr('|'); }
  for (i=3;i<22;i++){ ttgoto(i,35); putchr('|'); }
  for (i=3;i<22;i++){ ttgoto(i,75); putchr('|'); }
  drawline ( 3,1,35 );
  drawline ( 3,41,35 );
  drawline ( 21,1,35 );
  drawline ( 21,41,35 );

  done = 0;
  while ( !done )
  { 
				/* Get the local compression stats. */
  STAT ( 1, -2 );
  sscanf ( command_result, "%d %d", &d1, &d2 );
  if (d1 != 0)
    lcomp = ((float) d1 - d2) / ((float) d1) * 100.0;
  else
    lcomp  = 0;
				/* Get the remote compression stats. */
  STAT ( 0, -2 );
  sscanf ( command_result, "%d %d", &d1, &d2 );
  if (d1!=0)
    rcomp=((float) d1-d2)/((float)d1) * 100.0;
  else rcomp = 0;

  ttgoto ( 1, 41);
  printf ( "Local %5.2f%%    Remote %5.2f%%", lcomp, rcomp );

				/* Get the number of local clients */
				/* waiting to send. */
  STAT (1, -7);
  ttgoto ( 2, 4);
  printf ( "Clients waiting %s.", command_result);
  STAT (0, -7);
  printf ( " Remote clients waiting %s.", command_result);

  STAT ( 1, -3);
  sscanf ( command_result, "%d %d", &d1, &d2 );

  for (i=1;i<33;i++) {
    ihist[i-1]=ihist[i];
    ohist[i-1]=ohist[i];
  }
  dtim=((float)(times(&Tbuf)-stim) * HZ ) / 10000.0;
  stim=times(&Tbuf);

  ttgoto(2, 60);
  
  ihist[32]=(d1-last_in)/dtim;
  ohist[32]=(d2-last_out)/dtim;
  
  i=scale/18;
  for (y=20;y>3;y--) {
    for (x=0;x<33;x++) {
       if ( ihist[x] > i )
           line[x]='#';
       else
           line[x]=' ';
    }
    ttgoto(y,2);
    printf ( "%s", line );
    i += (scale/18);
  }

  i=scale/18;
  for (y=20;y>3;y--) {
    for (x=0;x<33;x++)
       if ( ohist[x] > i )
           line[x]='#';
       else
           line[x]=' ';
    ttgoto(y,42);
    printf ( "%s", line );
    i += (scale/18);
  }

  ttgoto ( 22, 1 );
  printf ( "Incoming CPS : %4d", (int)((d1-last_in)/dtim) );
  ttgoto ( 22, 41 );
  printf ( "Outgoing CPS : %4d\n", (int)((d2-last_out)/dtim) );
  last_in=d1;last_out=d2;

  timeout.tv_sec = timer;
  timeout.tv_usec = 0; 
  FD_ZERO ( &reads );
  FD_SET ( 0, &reads );
  select ( 1, &reads, 0, 0, &timeout);
  if ( FD_ISSET ( 0, &reads ) ) done=1;
  }
  /* set_block(0); */
  close(s);
  read(0, (char *) &s, 1);
  terminal_restore(0);
}



