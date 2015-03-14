/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/*
**	hp2621 terminal emulator
*/
#ifndef lint
static char rcsid[] = "$Header: hpmgr.c,v 4.2 88/06/22 14:37:42 bianchi Exp $";
#endif

#include "term.h"
#include <ctype.h>

#define MAX_ROW		24
#define MAX_COL		80

#define C_X	(MAX_COL*f_w)		/* width of the screen */
#define C_Y	(MAX_ROW*f_h)		/* height of the screen */
/*
FILE *outfile;
*/

extern int more_out;
static char *m_fields[16];

int cur_x = 0;		/* cursor location */
int cur_y = 0;

extern char *shiftline();
extern char *del_line();
int banner_space;
int x,y,w,h,f_w,f_h;

int in_mode = 0;	/* insert mode flag */

#define	min(a,b)	((a)<(b)?(a):(b))
#define	max(a,b)	((a)>(b)?(a):(b))

int margin;

main(argc,argv)
int argc;
char **argv;
{
	extern int verboseflag;
	int dummy;

	ckmgrterm( *argv );

	if (argc > 1 && !(strcmp(argv[1],"-v")))
	{
		verboseflag = 1;
	}

/*
	if ((outfile = fopen("hpmgr.out","w+")) == NULL)
	{
		printf("can't create logfile\n");
		exit(1);
	}
*/

	m_termin = stdin;
	m_termout = stdout;

	m_push(P_MENU|P_POSITION|P_EVENT|P_FLAGS);
	m_setmode(M_ABS);
	get_size(&x,&y,&w,&h);
	get_font(&f_w,&f_h);
	get_margin(&margin);

	banner_space = f_h+2;

	m_setmode(M_BACKGROUND);

	if ((w != C_X ) || ((C_Y + banner_space )!= h))
	{
		m_shapewindow(x,y,C_X+(2*margin),
				  C_Y + banner_space + (2*margin));
	}

	get_size(&x,&y,&w,&h);
	m_clear();

	/* print the banner line */
	m_printstr("                   HP2621 EMULATOR");
	m_flush();
	m_func(B_INVERT);
	m_bitwrite(0,0,w,banner_space-1);
	m_flush();
	m_func(B_OR);

	m_textregion(0,banner_space,C_X,C_Y);
	m_clear();
	m_flush();

	do_env("TERM=","h2");
	do_env("TERMCAP=","");
	getpty((char**)0);
	printf("\n\rPANIC -- getpty failed!! something is wrong!!\n\r");
}



int
get_margin(x)
int *x;

   { 
   register int count;

   if ((count = get_info(G_SYSTEM,m_fields)) >= 4) {
      *x = atoi(m_fields[3]); 
      return(1);
      }
   else 
	return(-count);
   }

inmassage(ptr,cnt)
char *ptr; int cnt;
{
	return(cnt);
}

outmassage(ptr,cnt)
char *ptr; int cnt;
{
	char *start = ptr;
	char *to, *sptr, *shiftptr;
	int newcnt = 0;
	char newbuf[1024];
	char smallb[1024];
	static char holdb[1024];
	
	static int gotesc = 0;
	static int gotamp = 0;
	static int gota = 0;
	static int gotc = 0;
	static int goty = 0;
	static int gotr = 0;
	static int gotd = 0;
	static int gotj = 0;

	static int xval = 0;
	static int yval = 0;

/*
fprintf(outfile,"outmassage --  more = %d  cnt = %d\n",more_out,cnt);
fflush(outfile);
*/
	/*
	**	check to see if we have more data on hold
	*/
	if (cnt == -2)
	{
		if (more_out)
		{
			to = ptr;
			mystrncpy(&to,holdb,more_out);
			cnt = more_out;
			more_out = 0;
		}
		else
		{
			printf("panic -- outmassage got cnt = %d and more_out = %d\n",
							cnt,more_out);
			exit(1);
		}
	}
	else
	{
		if (cnt <= 0)
			return(cnt);
	}

	for(to = newbuf;(cnt > 0) && (newcnt < (1024-100));ptr++,cnt--)
	{

		*ptr &= 0177;
/*
fputc(*ptr,outfile);
*/

		if (gotesc)
		{
			switch (*ptr)
			{
				case '\000':
					goto out;
				case ESC :
					gotamp = gota = gotc = gotj = gotd  =
						goty = gotr = 0;
					goto out;
				case '&' :
					gotamp = 1;
					goto out;
				case 'a' :
					gota = 1;
					goto out;
				case 'y' :
					goty = 1;
					goto out;
				case 'r' :
					gotr = 1;
					goto out;
				case 'c' :
					gotc = 1;
					goto out;

				case 'd' :
					gotd = 1;
					goto out;
				case '1' :
				case '2' :
				case '3' :
					/*
					**	skip tabs for now
					*/
					if (!gotamp)
					{
						gotesc = 0;
						goto out;
					}
				case '0' :
				case '4' :
				case '5' :
				case '6' :
				case '7' :
				case '8' :
				case '9' :
					if (gotc || goty || gotr)
					{
						yval *=10;
						yval += (*ptr) - '0';
					}
					else
					{
						xval *=10;
						xval += (*ptr) - '0';
					}
					goto out;
				case 'j' :
					gotj = 1;
					goto out;
				/*
				**	begin standout mode
				*/
				case 'D' :
					if (gotamp && gotd)
					{
						*to++ = ESC;
						*to++ = 'i';
						newcnt += 2;
					}
					gotesc = gotamp = gotj = gotd = 0;
					goto out;
				/*
				**	skip keyboard mode for now
				**	but  handle end of standout mode
				*/
				case '@' :
					if (gotamp && gotd)
					{
						*to++ = ESC;
						*to++ = 'n';
						newcnt += 2;
					}
					gotesc = gotamp = gotj = gotd = 0;
					goto out;
				case 'B' :
					if (gotamp)
					{
						gotesc = gotamp = gotj = 0;
						goto out;
					}
					goto parsepanic;
				
				/*
				**	clear screen
				*/
				case 'J' :
					*to++ = ESC;
					*to++ = 'C';
					newcnt += 2;
					gotesc = 0;
					goto out;

				/*
				**	home cursor
				*/
				case 'H' :
					sprintf(smallb,"%c%d,%dM",ESC,0,0);	
					mystrncpy(&to,smallb,strlen(smallb));
					newcnt += strlen(smallb);
					cur_x = cur_y = gotesc = 0;
					goto out;

				/*
				**	clear to eol
				*/
				case 'K' :
					*to++ = ESC;
					*to++ = 'c';
					newcnt += 2;
					gotesc =0;
					goto out;
				
				/*
				**	delete line
				*/
				case 'M' :
					*to++ = ESC;
					*to++ = 'd';
					newcnt += 2;
					gotesc = 0;
					goto out;
					
				/*
				**	end insert mode 
				*/
				case 'R' :
					if (gota && gotamp)
					{
						goto vertaddr;
					}
					in_mode = 0;
					gotesc = 0;
					goto out;
				/*
				**	start insert mode 
				*/
				case 'Q' :
					in_mode = 1;
					gotesc = 0;
					goto out;

				/*
				**	open line 
				*/
				case 'L' :
					*to++ = ESC;
					*to++ = 'a';
					newcnt += 2;
					gotesc=0;
					goto out;
					
				/*
				**	up line 
				*/
				case 'A' :
					*to++ = ESC;
					*to++ = 'u';
					newcnt += 2;
					gotesc=0;
					goup();
					goto out;
					
				case 'i' :
					cur_x -= (cur_x%8?cur_x%8:8);
					sprintf(smallb,"%c%d,%dM", ESC,cur_x,cur_y);	
					mystrncpy(&to,smallb,strlen(smallb));
					newcnt += strlen(smallb);
					gotesc = 0;
					goto out;

				/*
				**	delete char
				*/
				case 'P' :
					shiftptr = del_line();
					mystrncpy(&to,shiftptr,strlen(shiftptr));
					newcnt += strlen(shiftptr);
					gotesc=0;
					goto out;
					
				/*
				**	non destructive space
				**	and horizontal motion
				*/
				case 'C' :
					if (gotamp)
					{
						if (gota)
						{
							if(gotr||goty)
							{
								goto fulladdr;
							}
							sprintf(smallb,
								"%c%d,%dM",ESC,xval,cur_y);	
							mystrncpy(&to,smallb,strlen(smallb));
							newcnt += strlen(smallb);
							cur_x = xval;
							gotesc = gotamp = gota =
								gotc = xval = yval = 0;
						}
						else
						{
							goto parsepanic;
						}
					}
					else
					{
						*to++ = ESC;
						*to++ = 'r';
						newcnt += 2;
						gotesc=0;
						goright();
					}
					goto out;
					

				case 'Y' :
			vertaddr:
					if (gotamp && gota)
					{
			fulladdr:
						if (goty || gotr)
						{
							int tmp;
							tmp = xval;
							xval = yval;
							yval = tmp;
						}
						if (gotc || goty || gotr)
						{
							/*
							**	2-D cursor motion
							*/
/*
fprintf(outfile,"xval = %d, yval = %d",xval,yval);
fflush(outfile);
*/
							if (yval > MAX_ROW-1)
							{
								fprintf(stderr,
								"PANIC -- got address > 23 = %d", yval);
								sleep(10);
							}
							sprintf(smallb,"%c%d,%dM",
								ESC,xval,yval);	
							mystrncpy(&to,smallb,strlen(smallb));
							newcnt += strlen(smallb);
							cur_x = xval;
							cur_y = yval;
							gotesc = gotamp = gota = gotc =
								xval = yval = goty = gotr = 0;
						}
						else
						{
							/*
							**	vertical motion
							*/
							if (xval > MAX_ROW-1)
							{
								fprintf(stderr,
								"PANIC -- got address > 23 = %d", xval);
								sleep(10);
							}
							sprintf(smallb,
								"%c%d,%dM",ESC,cur_x,xval);	
							mystrncpy(&to,smallb,strlen(smallb));
							newcnt += strlen(smallb);
							cur_y = xval;
							gotesc = gotamp = gota =
								gotr = goty = gotc = xval = yval = 0;
						}
						goto out;
					}
					else
					{
						goto parsepanic;
					}
				default : 
		  parsepanic:;
/*
					fprintf(outfile,"|BAD ESCAPE CODE esc = %d gotamp = %d gota = %d gotc = %d gotj = %d goty = %d gotr = %d code = %o|\n",
					gotesc,gotamp,gota,gotc,
						gotj,goty,gotr,*ptr);
*/
					printf("|BAD ESCAPE CODE esc = %d gotamp = %d gota = %d gotc = %d gotj = %d goty = %d gotr = %d code = %o|\n",
					gotesc,gotamp,gota,gotc,
						gotj,goty,gotr,*ptr);
					sleep(10);
					gotesc = gotamp = gota = gotc = xval = yval = 0;
					goto out;
			}
		}

		switch (*ptr)
		{
			case '\000' :
				break;
			case ESC:
				gotamp = gota = gotc = gotj =
					goty = gotr = xval = yval = 0;
				gotesc = 1;
				break;
			case '\010' :
				goleft();
				goto dochar;
			case '\012' :
				godown();
				goto dochar;
			case '\015' :
				cur_x = 0;
				goto dochar;
			case '\011' :
				cur_x += (8 - (cur_x%8));
				goto dochar;
			default:
				if (in_mode && isprint(*ptr))
				{
					shiftptr = shiftline();
					mystrncpy(&to,shiftptr,strlen(shiftptr));
					newcnt += strlen(shiftptr);
				}
				goright();
		dochar :
				*to++ = *ptr;
				newcnt++;
				break;
		}
	out:;
	}

	/*
	**	make sure we didn't over run the end of the buffer
	*/
	if ((newcnt < 0) || (newcnt >= 1024))
	{
		printf("panic: newcnt = %d, out of range\n",newcnt);
	}
	/*
	**	did we run out of buffer space before finishing the input?
	*/
	if (cnt)
	{
		to = holdb;
		mystrncpy(&to,ptr,cnt);
		more_out = cnt;
	}
	strncpy(start,newbuf,newcnt);
/*
	fprintf(outfile,"newcnt = %d\n",newcnt);
	if ((newcnt > 0) && (newcnt < 1024))
		fwrite(start,sizeof(char),newcnt,outfile);
*/
	return(newcnt);
}

goright()
{
	if (cur_x == (MAX_COL-1))
	{
		cur_x = 0;
		godown();
	}
	else
	{
		cur_x++;
	}
}

goleft()
{
	cur_x = max(0,cur_x - 1);
}

goup()
{
	cur_y = max(0,cur_y - 1);
}

godown()
{
	cur_y = min(MAX_ROW - 1, cur_y + 1);
}

char *
shiftline()
{
	static char shiftbuf[1024];
	static char buf2[1024];

/*
fprintf(outfile,"cur_x = %d cur_y = %d\n\n",cur_x, cur_y);
fflush(outfile);
*/

	/*	m_func	 */
	sprintf(shiftbuf,"%c%d%c",ESC,B_COPY,E_BITBLT);

	/*	m_bitcopy */
	sprintf(buf2,"%c%d,%d,%d,%d,%d,%d%c",
		  ESC,
		  (cur_x+1)*f_w,			/* dest  x */
		  banner_space + (cur_y*f_h),		/* dest  y */
		  f_w*((MAX_COL-1)-cur_x),		/* width  */
		  f_h,					/* height */
		  cur_x*f_w,				/* src  x */
		  banner_space + (cur_y*f_h),		/* src  y */
		  E_BITBLT);
	strcat(shiftbuf,buf2);

	/*	m_func	    */
	sprintf(buf2,"%c%d%c",ESC,B_CLEAR,E_BITBLT);
	strcat(shiftbuf,buf2);

/*
	sprintf(buf2,"%c%d,%d,%d,%d%c",
		  ESC,
		  cur_x*f_w,			 x
		  banner_space + (cur_y*f_h),	 y 
		  f_w,				width 
		  f_h,				height
		  E_BITBLT);
	strcat(shiftbuf,buf2);

*/
	/*	m_func	    */
	sprintf(buf2,"%c%d%c",ESC,B_OR,E_BITBLT);
	strcat(shiftbuf,buf2);
	return(shiftbuf);
}

char *
del_line()
{
	static char shiftbuf[1024];
	static char buf2[1024];
	/*	m_func	 */
	sprintf(shiftbuf,"%c%d%c",ESC,B_COPY,E_BITBLT);

	/*	m_bitcopy */
	sprintf(buf2,"%c%d,%d,%d,%d,%d,%d%c",
		  ESC,
		  cur_x*f_w,				/* dest x */
		  banner_space + (cur_y*f_h),		/* dest y */
		  f_w*((MAX_COL-1)-cur_x),		/* width  */
		  f_h,					/* height */
		  (cur_x+1)*f_w,			/* src  x */
		  banner_space + (cur_y*f_h),		/* src  y */
		  E_BITBLT);
	strcat(shiftbuf,buf2);

	/*	m_func	    */
	sprintf(buf2,"%c%d%c",ESC,B_CLEAR,E_BITBLT);
	strcat(shiftbuf,buf2);

	sprintf(buf2,"%c%d,%d,%d,%d%c",
		  ESC,
		  C_X-f_w,			/*  x */
		  banner_space + (cur_y*f_h),	/*  y */
		  f_w,				/* width  */
		  f_h,				/* height */
		  E_BITBLT);
	strcat(shiftbuf,buf2);

	/*	m_func	    */
	sprintf(buf2,"%c%d%c",ESC,B_OR,E_BITBLT);
	strcat(shiftbuf,buf2);
	return(shiftbuf);
}

mystrncpy(to,from,cnt)
char **to, *from; int cnt;
{
	while(cnt > 0)
	{
		**to = *from;
		(*to)++;	/* increment the pointer,NOT the pointer to the pointer */
		from++;
		cnt--;
	}
}
cleanup()
{
	m_pop();
	m_textreset();
	m_clear();
	exit(0);
}
