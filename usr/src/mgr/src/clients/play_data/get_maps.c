/* read op file (binary format) , strip out images */
/* hack -f flag to generate 1st image only */

#include "bitmap.h"
#include "share.h"

#define A(x)		args[x]		/* short hand */
#define Max(a,b)	((a)>(b)?(a):(b))
#define NAME		"map."

char path[512];				/* path name for bitmap */
unsigned short args[10];	/* arg buffer */
char line[80];					/* line buffer */

main(argc,argv)
int argc;
char **argv;
	{
	int count=0;
	char *name;							/* name prefix to bitmap */
	register int c;					/* item type */
	register int time = 0;			/* got a time */
	int first = 0;						/* get first map only */
	int id=0;							/* id of first map */

	if (argc>1 && strcmp(argv[1],"-f")==0) {
		first++;
		argc--, argv++;
		}

	if (argc>1)
		name = argv[1];
	else
		name = NAME;

	while((c=getshort()) != EOF) {

		switch (c&TYPE_MASK) {
			case T_BLIT:
				getargs(args,8);
				break;
			case T_WRITE:
				getargs(args,5);
				break;
			case T_SCREEN:
				getargs(args,4);
				if (A(3) && first) {
					write_map(name,A(1),A(2));
					exit(1);
					}
				else if (A(3)) {
					sprintf(path,"%s%03d:%dx%d",name,count++,A(1),A(2));
					write_map(path,A(1),A(2));
					}
				else if (first)
					id=A(0);
				break;
			case T_NOP:
				c &= 0xF;		/* # of bytes to skip */
				while(c-- > 0)
					getchar();
				break;
			case T_LINE:
				getargs(args,5);
				break;
			case T_DATA:
				getargs(args,4);
				if (A(3) && first && A(0)==id) {
					write_map(name,A(1),A(2));
					exit(1);
					}
				if (A(3) && first) {
					write_map("/dev/null",A(1),A(2));
					}
				else if (A(3)) {
					sprintf(path,"%s%03d:%dx%d",name,count++,A(1),A(2));
					write_map(path,A(1),A(2));
					}
				break;
			case T_TIME:
				getargs(args,2);
				time = (args[0]<<16|args[1])/100;
				break;
			case T_POINT:
				getargs(args,3);
				break;
			case T_KILL:
				getargs(args,1);
				break;
			case T_MARK:
				getargs(args,1);
				break;
			default:
				fprintf(stderr,"OOps, got 0x%x\n",c);
				break;
			}
		}
	}

int
getshort()
	{
	register int a = getchar();
	register int b = getchar();
	register int result = (a<<8) + b;

	if (a==EOF || b==EOF)
		return(EOF);
	else {
		return(result);
		}
	}

getargs(args,count)
short *args;
int count;
	{
	while (count-- > 0)
		*args++ = getshort();
	return(0);
	}

int
write_map(path,w,h)
char *path;					/* name of bitmap */
int w,h;						/* size of bitmap */
	{
	int bytes = BIT_Size(w,h,1);
	BITMAP *map = bit_alloc(w,h,NULL,1);
	FILE *file = fopen(path,"w");

	printf("Writing map [%s]\n",path);
	fread(BIT_DATA(map),bytes,1,stdin);
	bitmapwrite(file,map,1);
	fclose(file);
	bit_destroy(map);
	};
