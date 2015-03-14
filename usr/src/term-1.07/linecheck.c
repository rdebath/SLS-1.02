#include <stdio.h>
#include <string.h>
#include <ctype.h>
#if !defined(NeXT)
#  include <unistd.h>
#endif
#include <stdlib.h>
#include "terminal.h"

#ifndef STDIN_FILENO
#define STDIN_FILENO 0
#endif
/*
 
$Header: /u/jefftep/src/linecheck/RCS/linecheck.c,v 0.14 1993/01/18 04:01:09 jefftep Exp jefftep $

 intially written by micheal o'reilly
 *seriously* bashed about by by quarters
 hell, I wonder if diff would find more than 5 matching lines anymore...
 jefftep@cs.utexas.edu
 jeff grills

 to use, run remotely like:   bovina% linecheck 2> remote.output
             locally:         linecheck < /dev/modem > /dev/modem 2> local.output

 hack to be equivalent in your preferred shell, where 2> redirects stderr.

 you can tell linecheck not to test certain chars by listing their decimal number
 on the command line.  for instance, if you know flow-control will get eaten,
 you can use "linecheck 17 19", and it won't test those chars.  this is also
 useful for testing a set of escape sequences, to make sure it makes your line
 clean.

 if it says something needs escaped, that means it didn't get through okay
 this time. if you get an invalid packet printed, it means the packet wasn't
 sent by the linecheck on the other side, and may either be line static,
 or some very braindead terminal response to a (possibly series) of characters
 to what was printed over the line.  in this case, it's your responsibilty
 to determine which, and escape the previously sent char if needed.  There is
 no way this program can identity a braindead terminal server from line static,
 so this is the way it has to be.

 if, for some reason, you get stuck out in lala land, and can't kill the program,
 try typing "00000".  That should kill it, and restore your terminal.

 It'll print "### sending char" and "### received valid".  Don't worry if these
 two number are out of sync.  That's fine.  Just worry, on either side, if you
 get some "Invalid packet: " lines.  Look at them closely, and see if it's line
 static, or a real problem.

 At the end, it'll print out a summary of what it thinks you should escape.
 This just means these chars didn't get recieved correctly this time.  Again,
 if line static munched something, some of these may be valid. 

 *** IF *** your terminal server generates extra responses for odd chars,
 then you may not be told to escape something, but need to anyway.  This will
 be evident from a "Invalid packet: " on the local side, after attempting to
 send a character.  Again, it may be line static. You have to make the call.

 if you're running it locally in a xterm, I suggest you turn on window logging.

 if you have problems with this program, and want me to look at it, mail me
 *both* the local and remote output, and label them appropriately.

programmers notes/future directions:

 maybe do a fork() and process the two sides independently, so it never hangs.
 would cause minor quitting problems, but may be worth it.

history:

$Log: linecheck.c,v $
 * Revision 0.14  1993/01/18  04:01:09  jefftep
 * fgets includes \n in string, fixed
 * seperate skip & valid to avoid race condition
 * print out invalid packets a little better
 *
 * Revision 0.13  1993/01/18  01:43:29  jefftep
 * print out unknown packets while handshaking
 * bug fix: fgets() includes the \n at the end of the string, so strip it
 *
 * Revision 0.12  1993/01/18  01:31:06  jefftep
 * allow bail in the handshaking
 *
 * Revision 0.11  1993/01/18  00:48:58  jefftep
 * previous history unrecorded
 *
 */

#define START_AT 0
#define STOP_AT  256

#define START      'A'
#define STOP       'B'
#define END_PACKET 'C'
#define XON        '\021'
#define QUIT       "quit"
#define SLEEP      1
#define TRIES      1
#define BUMS       1

#define BUFFSIZE  200

/* -------------------------------------------------- */

char buff[BUFFSIZE];
int valid[STOP_AT+1], skip[STOP_AT];

/* -------------------------------------------------- */

int mgets(char *buff, int len)
{
  char ch;
  int i, zcount;
  
  i = 0;
  zcount = 0;
  
  while(1)
  {
		read(STDIN_FILENO, &ch, 1);

		/* check if we get five 0's and abort */
		if ( ch == '0' )
		{
			if (++zcount == 5)
			{
				terminal_restore(0);
				exit(0);
			}
		}
		else
			zcount = 0;
		
		/* handle next char */
		if (ch == '\n')

			/* check if this is really inside the packet */
			if ( (i == 4) && (buff[0] == '1') && (buff[1] == '0') && (buff[2] == ' ') &&
					(buff[3] == START) )
				buff[i++] = ch;
			else
			{
				buff[i] = '\0';
				return i;
			}

		else
		{
			/* store this char in the bufffer */
			buff[i++] = ch;
			
			/* don't let packet overflow */
			if ( (i+1) == len)
			{
			  buff[i] = '\0';
				return i;
			}
		}
	}
}

/* -------------------------------------------------- */

void skipchars(char **s)
{
	while(*s)
	{
		skip[atoi(*(s++))] = 1;
	}
}

/* -------------------------------------------------- */

void debug(char *s)
{
  while (*s)
  {
    if ( isprint(*s) )
      fprintf(stderr, "%c", *(s++));
    else      
      fprintf(stderr, "<%d>", (int) *(s++));
  }
  fprintf(stderr, "\n");
}

/* -------------------------------------------------- */

void bum(char *s)
{
  int i;
	for (i=0; i<BUMS; i++)
	{
		if ( s == NULL )
			printf("\n");
		else
			printf("\n%s\n", s);
		sleep(SLEEP);
	}
}

/* -------------------------------------------------- */

void check(char *s)
{
  int k;
  char *t;
  t = s;

	/* convert int to number */
	k = 0;
	while ( isdigit(*s) )
		k = (k * 10) + (*(s++) - '0');
	
	/* verify this is a valid packet */
	if ( (s[0] == ' ') && (s[1] == START) && (s[2] == (char)k) && (s[3] == STOP) )
	{
		fprintf(stderr, "%3d received valid\n", k);
		valid[k] = 1;
	}
	else
	{
		fprintf(stderr, "invalid packet: ");
		debug(t);
	}
}

/* -------------------------------------------------- */

int handle_incoming(void)
{
  /* get a non-empty line */
	while( !mgets(buff, BUFFSIZE) );

	/* see if they are done sending test packets */
	if (strcmp(buff,QUIT) == 0)
		return 1;
	
	check(buff);
	return 0;
}

/* -------------------------------------------------- */

void handshake()
{
  int i;

	fprintf(stderr, "Handshaking\n");

	printf("\n%c\n", START);
	i = 0;
	while ( ! i )
	{
		mgets(buff, BUFFSIZE);

		if ( (buff[0] == START) && (buff[1] == '\0') )
		{
			i = 1;
			printf("\n%c\n", STOP);
		}
		else
			if ( (buff[0] == STOP) && (buff[1] == '\0') )
				i = 1;
			else
				if (buff[0] != '\0' )
				{
					fprintf(stderr, "unexpected packet: ");
					debug(buff);
				}
	}
	
	fprintf(stderr, "Handshaking sucessful\n");
}

/* -------------------------------------------------- */

void print_esc(void)
{
  int i, j;

  /* make printing at the end easier by making it more generic */
	valid[STOP_AT] = 1;

  /* print out the valid list */
  j = -1;
	fprintf(stderr, "\n");
  for (i = START_AT; i <= STOP_AT; i++)
		if ( valid[i] && (j != -1) )
		{
			if ((j <= 128) && (i == STOP_AT))
			{
				fprintf(stderr, "sevenbit\n");
				if ( j < 127 )
					fprintf(stderr, "escape %d-%d\n", j, 127);
				if ( j == 127 )
					fprintf(stderr, "escape 127\n");
			}
			else
				if (j == (i-1))
					fprintf(stderr, "escape %d\n", j);
				else
					fprintf(stderr, "escape %d-%d\n", j, i-1);
			j = -1;
		}
		else
			if ( (!valid[i]) && (j == -1) )
				j = i; 
}

/* -------------------------------------------------- */

void main(int argc, char **argv)
{
  int i, k, quit;
	
	quit = 0;

  /* nothing has gotten through okay yet */	
  for (i = START_AT; i < STOP_AT; i++)
	{
		valid[i] = 0;
		skip[i] = 0;
	}

  skipchars(argv+1);

	terminal_save(0);
	terminal_raw(0);

  setbuf(stdout,NULL);
	
  handshake();

  /* test all the chars */
  for (i = START_AT; i < STOP_AT; i++ )
  {
		/* don't send chars we are told to skip */
    if ( skip[i] )
      continue;
		
    fprintf(stderr, "%3d sending char\n", i);

    /* attempt to send this char across, in a cute little packet */
		for (k=0; k<TRIES; k++)
		{
			if ( !skip[(int)XON] )
				printf("\n%d %c%c%c%c%c\n", i, START, (char)i, STOP, XON, END_PACKET );
			else
				printf("\n%d %c%c%c%c\n", i, START, (char)i, STOP, END_PACKET );
			
			bum(NULL);

 			/* while we're at it, take a look to the other side */
			if (!quit)
				quit = handle_incoming();
		}
	}

  bum(QUIT);

  /* let the other side finish */
	while (!quit)
		quit = handle_incoming();
	
  print_esc();
	
	terminal_restore(0);
}
