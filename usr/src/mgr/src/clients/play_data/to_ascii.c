/*{{{}}}*/
/*{{{  Notes*/
/* read op file (binary format) convert to ascii */
/*}}}  */
/*{{{  #includes*/
#include "bitblit.h"
#include "share.h"
/*}}}  */
/*{{{  #defines*/
#define A(x)		args[x]		/* short hand */
#define Max(a,b)	((a)>(b)?(a):(b))
/*}}}  */

/*{{{  variables*/
unsigned short args[10];	/* arg buffer */
char line[80];		/* line buffer */
/*}}}  */

/*{{{  getshort*/
int
getshort()
	{
#if 0
	register int a = getchar();
	register int b = getchar();
	register int result = (a<<8) + b;

	if (a==EOF || b==EOF)
		return(EOF);
	else {
		return(result);
		}
#endif
#if 1
  short int n;

  if (fread(&n,sizeof(n),1,stdin)!=1) return EOF;
  else return n;
#endif
	}
/*}}}  */
/*{{{  getargs*/
getargs(args,count)
short *args;
int count;
	{
	while (count-- > 0)
		*args++ = getshort();
	return(0);
	}
/*}}}  */
/*{{{  print_data*/
/* print the data in ascii */

int
print_data(w,h,out)
int w,h;				/* bitmap size */
int out;				/* print output */
	{
	int bytes = bit_size(w,h,1);
	register int i,c;
	printf(". ");
	for(i=1;i<=bytes && (c=getchar()) != EOF;i++)
		if (out) printf("%c%c%s","0123456789ABCDEF"[(c>>4)&0xF],
						"0123456789ABCDEF"[c&0xF],
						i%30==0  && i<bytes ? "\n. " : "");
	printf("\n");
	}
/*}}}  */

/*{{{  main*/
int main(argc,argv)
	{
	register int c;
	register int type;
	register int time = 0;			/* got a time */
	int no_data=0;

	if (argc>1)
		no_data++;

	while((c=getshort()) != EOF) {

		if (time && (c&TYPE_MASK) != T_TIME)		/* flush previous time */
			fputs(line,stdout);
		time=0;

		switch (c&TYPE_MASK) {
			case T_BLIT:
				getargs(args,8);
				printf("B %d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\n",
					A(0),A(2),A(3),A(4),A(5),c&0xf,A(1),A(6),A(7));
				break;
			case T_WRITE:
				getargs(args,5);
				printf("W %d\t%d\t%d\t%d\t%d\t%d\n",
					A(0),A(1),A(2),A(3),A(4),c&0xf);
				break;
			case T_SCREEN:
				getargs(args,4);
				printf("S %d\t%d\t%d\t%d\n",
					A(0),A(1),A(2),A(3)?1:0);
				if (A(3))
					print_data(A(1),A(2),!no_data);
				break;
			case T_NOP:
				c &= 0xF;		/* # of bytes to skip */
				while(c-- > 0)
					getchar();
				break;
			case T_LINE:
				getargs(args,5);
				printf("L %d\t%d\t%d\t%d\t%d\t%d\n",
					A(0),A(1),A(2),A(3),A(4),c&0xf);
				break;
			case T_DATA:
				getargs(args,4);
				printf("D %d\t%d\t%d\t%d\n",
					A(0),A(1),A(2),A(3)?1:0);
				if (A(3))
					print_data(A(1),A(2),!no_data);
				break;
			case T_TIME:
				getargs(args,2);
				sprintf(line,"T %d.%d\n",
					(args[0]<<16|args[1])/100,(args[0]<<16|args[1])%100);
				time++;
				break;
			case T_POINT:
				getargs(args,3);
				printf("P %d\t%d\t%d\t%d\n",
					A(0),A(1),A(2),c&0xf);
				break;
			case T_KILL:
				getargs(args,1);
				printf("K %d\n",
					args[0]);
				break;
			case T_MARK:
				getargs(args,1);
				printf("M %d\n",
					args[0]);
				break;
			default:
				fprintf(stderr,"OOps, got 0x%x\n",c);
				break;
			}
		}
	}
/*}}}  */
