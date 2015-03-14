#define DATA unsigned char

#include <bitblit.h>

extern DATA *graph_mem;

#define LOGBITS 3
#define BITS (~(~(unsigned)0<<LOGBITS))

#define BIT_OFFSET(w) (((BIT_WIDE(w)+BITS)&~BITS)*BIT_HIGH(w)>>3)
#define BIT_SIZE(m) Bit_Size(BIT_WIDE(m), BIT_HIGH(m), BIT_DEPTH(m))
#define BIT_Size(wide,high,depth) ((((depth*wide+BITS)&~BITS)*high)>>3)
#define BIT_LINE(x) ((((x)->primary->depth*(x)->primary->wide+BITS)&~BITS)>>LOGBITS)

int screen_width(void);
int screen_heigth(void);
int screen_depth(void);

void setplane(int plane);
void setmapmask(int mask);
void write_mode(int mode);
/*{{{}}}*/
