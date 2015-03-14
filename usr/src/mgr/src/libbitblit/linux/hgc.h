#define DATA unsigned char

#include <bitblit.h>

extern DATA *graph_mem;

#define LOGBITS 3
#define BITS (~(~(unsigned)0<<LOGBITS))

#define BIT_OFFSET(w) (((BIT_WIDE(w)+BITS)&~BITS)*BIT_HIGH(w)>>3)
#define BIT_SIZE(m) Bit_Size(BIT_WIDE(m), BIT_HIGH(m), BIT_DEPTH(m))
#define BIT_Size(wide,high,depth) ((((depth*wide+BITS)&~BITS)*high)>>3)
#define BIT_LINE(x) ((((x)->primary->depth*(x)->primary->wide+BITS)&~BITS)>>LOGBITS)

#define screen_width() 720
#define screen_height() 348
#define screen_depth() 1

DATA *initscreen(char*);
void setscreen();
void resetscreen();

#define NEED_ADJUST

/*
 * Adjust the screen address for hercules monochrome card
 * From: Chi-Ting Lam <chiting@u.washington.edu>
 */

static DATA inline *adjust(p) DATA *p;
{
  register DATA *p1 asm("eax");

 /*
  * p=(DATA*)((long)p-(long)graph_mem);
  * return (char*)((((((unsigned int)p / 90) & 3)<<13) + ((unsigned int)p / 360) * 90 + (unsigned int)p % 90)+graph_mem);
  *
  * p1 = p - graph_mem;
  * return (char*) ((((q=((unsigned int)p / 90)) & 3)<<13)
  *                + ((q >> 2) * 90)
  *                + ((unsigned int)p % 90)
  *		   + ((unsigned int)graph_mem));
  */

  __asm__ volatile ("movl %2,%%eax
		     subl %1,%%eax
                     movl $90,%%esi
                     xorl %%edx,%%edx
                     divl %%esi
                     movl %%eax,%%esi            / esi is q=p/90 
                     andl $3,%%eax               / 
                     sall $13,%%eax              /
                     addl %%edx,%%eax            / add reminder
                     sarl $2,%%esi               / q >> 2
                     leal (%%esi,%%esi,8),%%esi  / * 90
                     addl %%esi,%%eax
                     leal (%%esi,%%esi,8),%%esi
                     addl %%esi,%%eax
                     addl %1,%%eax"
		    : "=a" ((int) p1)
		    : "g1" ((int) graph_mem), "g2" ((int) p)
                    : "eax", "edx", "esi");

  return p1;
}
