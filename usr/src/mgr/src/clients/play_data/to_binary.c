/* convert ascii back into binary */

#include "bitmap.h"
#include "share.h"

#define A(x)		args[(x)+1]		/* short hand */
#define dprintf		if(debug) fprintf

unsigned short args[10];	/* arg buffer */
char line[80];		/* line buffer */
int debug=0;
char *index();
char *digits = "0123456789ABCDEF";

int t_offset = 0;		/* time offset */

main()
	{
	int c, c1, c2;
	char *p;
	unsigned int time;

	debug = getenv("DEBUG");
	setbuf(stderr,NULL);

	while(gets(line)) {
		if (debug) fprintf(stderr,"[%s]\n",line);
		switch (*line) {
			case 'B':		/* bit-blit */
				sscanf(line+2,"%hu %hu %hu %hu %hu %d %hu %hu %hu",
					&A(0),&A(2),&A(3),&A(4),&A(5),&c,&A(1),&A(6),&A(7));
				*args = (c&0xF) | T_BLIT;
				fwrite(args,2,9,stdout);
				break;
			case 'W':
				sscanf(line+2,"%hu %hu %hu %hu %hu %d",
					&A(0),&A(1),&A(2),&A(3),&A(4),&c);
				*args = (c&0xF) | T_WRITE;
				fwrite(args,2,6,stdout);
				break;
			case 'S':
				sscanf(line+2,"%hu %hu %hu %hu",
					&A(0),&A(1),&A(2),&A(3));
				*args = T_SCREEN;
				fwrite(args,2,5,stdout);
				break;
			case '.':			/* get data */
				p = line+2;
				while (*p) {
					c1 = index(digits,*p++) - digits;
					c2 = index(digits,*p++) - digits;
					putchar(((c1&0xf)<<4) + (c2&0xf));
					}
				break;
			case 'O':			/* offset time */
				sscanf(line+2,"%d.%d",
					&c1,&c2);
				t_offset = c1*100 + c2;
				break;
			case 'N':			/* no-op ignore (for now) */
				break;
			case 'L':
				sscanf(line+2,"%hu %hu %hu %hu %hu %hu",
					&A(0),&A(1),&A(2),&A(3),&A(4),&c);
				*args = (c&0xF) | T_LINE;
				fwrite(args,2,6,stdout);
				break;
			case 'D':
				sscanf(line+2,"%hu %hu %hu %hu",
					&A(0),&A(1),&A(2),&A(3));
				*args = T_DATA;
				fwrite(args,2,5,stdout);
				break;
			case 'T':
				sscanf(line+2,"%d.%d",
					&c1,&c2);
				time = t_offset + (c1*100 + c2);
				A(0) = (time>>16)&0xffff;
				A(1) = time&0xffff;
				*args = T_TIME;
				fwrite(args,2,3,stdout);
				break;
			case 'P':
				sscanf(line+2,"%hu %hu %hu %hu",
					&A(0),&A(1),&A(2),&c);
				*args = (c&0xF) | T_POINT;
				fwrite(args,2,4,stdout);
				break;
			case 'M':
				sscanf(line+2,"%hu",
					&A(0));
				*args = T_MARK;
				fwrite(args,2,2,stdout);
				break;
			case 'K':
				sscanf(line+2,"%hu",
					&A(0));
				*args = T_KILL;
				fwrite(args,2,2,stdout);
				break;
			default:
				fprintf(stderr,"OOps, got 0x%x\n",*line);
				break;
			}
		}
	}
