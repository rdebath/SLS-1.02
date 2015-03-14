#define DATA unsigned int

#include <bitblit.h>

extern DATA *graph_mem;

#define LOGBITS 5
#define BITS (~(~(unsigned)0<<LOGBITS))

#define BIT_OFFSET(w) (((BIT_WIDE(w)+BITS)&~BITS)*BIT_HIGH(w)>>3)
#define BIT_Size(wide,high,depth) ((((depth*wide+BITS)&~BITS)*high)>>3)
#define BIT_SIZE(m) BIT_Size(BIT_WIDE(m), BIT_HIGH(m), BIT_DEPTH(m))
#define BIT_LINE(x) ((((x)->primary->depth*(x)->primary->wide+BITS)&~BITS)>>LOGBITS)
