/* shrink a MGR bitmap by 2  (sau) 11/90 */
/* Compile with -DTEST to make a test program */

#include <stdio.h>
#include "bitmap.h"

/* retrieve bitmap data, reduce it by 2x2 to 1x1 */

unsigned char *build_table();		/* to build a threshold table */
static char *print_byte();			/* for printing data in binary (debugging) */

#ifdef TEST

BITMAP *bitmapread();

main(argc,argv)
int argc;
char **argv;
	{
	BITMAP *map;
	unsigned char *table;
	int threshold;

	if (argc<2) {
		fprintf(stderr,"Usage: %s 1|2|3|4\n",*argv);
		exit(1);
		}

	threshold = atoi(argv[1]);

	table = build_table(NULL,threshold);

	map=bitmapread(stdin);
	fprintf(stderr,"Read map %d x %d\n",BIT_WIDE(map),BIT_HIGH(map));
	shrink_map(table,map);
	fprintf(stderr,"Shrunk map %d x %d\n",BIT_WIDE(map),BIT_HIGH(map));
	bitmapwrite(stdout,map,0);
	exit(0);
	}	

#endif

/* build a theshold table (calls compute_threshold for historical reasons) */

unsigned char *
build_table(table,t)
char *table;	/* an existing table (or NULL to malloc one) */
int t;			/* threshold t=[1234] # of bits on in original 2x2 patch) */
	{
	unsigned char *compute_threshold();
	return(compute_threshold(table,t));
	}

/* shrink a bitmap by 2 */

int
shrink_map(table,map)
unsigned char *table;	/* threshold conversion table */
BITMAP *map;				/* bitmap to 1/2 */
	{
	register int row;						/* current row */
	register int col;						/* current column */
	register unsigned char *even;		/* current char (even row) */
	register unsigned char *odd;		/* current char (odd row) */
	register unsigned char *dst;		/* destination byte */
	register int ind;						/* index into lookup table */
	int bytes;								/* bytes per scan line (in) */
	int rows = BIT_HIGH(map)&~1;		

	bytes = BIT_LINE(map)<<(LOGBITS-3);		/* bytes per scan line */
	even = (unsigned char *) BIT_DATA(map);
	odd = even + bytes;
	dst = even;
	for(row=0;row<rows;row+=2) {
		for(col=0;col<bytes;col++) {
			ind = (*even>*odd) ?	/* this could be made faster */
						((*even*(*even+1)>>1)+*odd) : ((*odd*(*odd+1)>>1)+*even);
			if (col&1) {		/* odd columns */
				*dst++ |= ind&1 ? 0xf&table[ind>>1] : 0xf&(table[ind>>1]>>4);
				}
			else {				/* even columns */
				*dst = ind&1 ? 0xf&table[ind>>1] : 0xf&(table[ind>>1]>>4);
				*dst <<=4;
				}	
			even++;
			odd++;
			}
		even += bytes;
		odd += bytes;
		}
	BIT_HIGH(map)/=2;
	BIT_WIDE(map)/=2;
	return(0);
	}

/* take 2 8 bit quantities and generate a 4 bit result */

#define T_SIZE		(((256*256)>>1)+256)

unsigned char *
compute_threshold(tbl,n)
unsigned char *tbl;		/* space to hold table (or null) */
int n;		/* threshold  1, or 2, or 3 */
	{
	char *malloc();
	register int i,j,nibble,index=0;

	if (tbl==NULL && (tbl = (unsigned char *) malloc(T_SIZE)) == NULL)
		return(NULL);

	bzero(tbl,T_SIZE);
	fprintf(stderr,"Building conversion table\n");

	for(i=0;i<256;i++) 
		for(j=0;j<=i;j++) {
			nibble = do_nibble(i,j,n);
			tbl[index>>1] |= index&1 ? nibble<<4 : nibble;
			index++;
			}
	return(tbl);
	}

/* compute resulting bit from 2x2 grid (gory version) */

char t1[16] =
	{0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
char t2[16] = 
	{0,0,0,1,0,1,1,1,0,1,1,1,1,1,1,1};
char t3[16] =
	{0,0,0,0,0,0,0,1,0,0,0,1,0,1,1,1};
char t4[16] = 
	{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1};

char *t[] =
	{t1, t2, t3, t4};

/* convert a pair of bytes (top and bottom) to a compacted nibble) */

int
do_nibble(i,j,n)
register int i;				/* top top, 8 bits */
register int j;				/* bottom byte, 8 bits */
register int n;				/* threshold #bit on in 2x2 grid for "on" out */
	{
	char *tbl = t[(n-1)&3];		/* pick the right threshold table */
	char *print_byte();
	char *a, *b, *c;
	register int result = 
		((tbl[(i>>0)&3 | (j&3)   <<2]) <<0) +
		((tbl[(i>>2)&3 | (j&0xc) <<0]) <<1) +
		((tbl[(i>>4)&3 | (j&0x30)>>2]) <<2) +
		((tbl[(i>>6)&3 | (j&0xc0)>>4]) <<3);
/*
	fprintf(stderr,"Converting (%d,%d) %s,%s to %s\n",
			i,j,(a=print_byte(i)),(b=print_byte(j)),(c=print_byte(result)));
	free(a), free(b), free(c);
*/
	return(result);
	}
	
/* binary table for print byte */

char *bin[] = {
	"0000", "0001", "0010", "0011", "0100", "0101", "0110", "0111",
	"1000", "1001", "1010", "1011", "1100", "1101", "1110", "1111",
	};

static char *
print_byte(x)
	{
	char *malloc();
	char *foo = malloc(9);

	strcpy(foo,bin[(x>>4)&0xf]);
	strcpy(foo+4,bin[x&0xf]);
	return(foo);
	}
