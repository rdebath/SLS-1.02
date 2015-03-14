/*{{{}}}*/
/*{{{  #includes*/
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>

#include <linux/mman.h>
#include <linux/mm.h>
#include <linux/kd.h>

#include "bitblit.h"
/*}}}  */

/*{{{  #defines*/
#define LOGBITS 3
#define BITS (~(~(unsigned)0<<LOGBITS))

#define GRAPH_BASE 0xA0000
#define GRAPH_SIZE 0x10000
#define TEXT_BASE  0xB8000
#define TEXT_SIZE  0x8000
#define FONT_BASE  0xA0000
#define FONT_SIZE  0x2000

/* VGA index register ports */
#define CRT_I   0x3D4   /* CRT Controller Index (mono: 0x3B4) */
#define ATT_IW  0x3C0   /* Attribute Controller Index & Data Write Register */
#define GRA_I   0x3CE   /* Graphics Controller Index */
#define SEQ_I   0x3C4   /* Sequencer Index */
#define PEL_IW  0x3C8   /* PEL Write Index */

/* VGA data register ports */
#define CRT_D   0x3D5   /* CRT Controller Data Register (mono: 0x3B5) */
#define ATT_R   0x3C1   /* Attribute Controller Data Read Register */
#define GRA_D   0x3CF   /* Graphics Controller Data Register */
#define SEQ_D   0x3C5   /* Sequencer Data Register */
#define MIS_R   0x3CC   /* Misc Output Read Register */
#define MIS_W   0x3C2   /* Misc Output Write Register */
#define IS1_R   0x3DA   /* Input Status Register 1 (mono: 0x3BA) */
#define PEL_D   0x3C9   /* PEL Data Register */

/* VGA indexes max counts */
#define CRT_C   24      /* 24 CRT Controller Registers */
#define ATT_C   21      /* 21 Attribute Controller Registers */
#define GRA_C   9       /* 9  Graphics Controller Registers */
#define SEQ_C   5       /* 5  Sequencer Registers */
#define MIS_C   1       /* 1  Misc Output Register */

/* VGA registers saving indexes */
#define CRT     0               /* CRT Controller Registers start */
#define ATT     CRT+CRT_C       /* Attribute Controller Registers start */
#define GRA     ATT+ATT_C       /* Graphics Controller Registers start */
#define SEQ     GRA+GRA_C       /* Sequencer Registers */
#define MIS     SEQ+SEQ_C       /* General Registers */
#define END     MIS+MIS_C       /* last */

#define SETGRA(x,y)	{port_out(x,GRA_I);port_out(y,GRA_D);}
#define SETSEQ(x,y)	{port_out(x,SEQ_I);port_out(y,SEQ_D);}
#define SETATT(x,y)	{port_in(IS1_R);port_out(x,ATT_IW),port_out(y,ATT_IW);}
/*}}}  */

/*{{{  port_out*/
static void inline port_out(char value, unsigned short port)
{
__asm__ volatile ("outb %0,%1"
		::"a" ((char) value),"d" ((unsigned short) port));
}
/*}}}  */
/*{{{  port_in*/
static unsigned char inline port_in(unsigned short port)
{
	unsigned char _v;
__asm__ volatile ("inb %1,%0"
		:"=a" (_v):"d" ((unsigned short) port));
	return _v;
}
/*}}}  */

/*{{{  vga mode data*/
struct mode_record {
  char *name;
  int width,height,depth;
  char reg[60];
};

static struct mode_record mode_regs[] = {
  { "640x200",640,200,4,
      {
	0x5F,0x4F,0x50,0x82,0x54,0x80,0xBF,0x1F,0x00,0xC0,0x00,0x00, 
	0x00,0x00,0x00,0x00,0x9C,0x8E,0x8F,0x28,0x00,0x96,0xB9,0xE3, 
	0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x10,0x11,0x12,0x13, 
	0x14,0x15,0x16,0x17,0x01,0x00,0x0F,0x00,0x00, 
	0x00,0x00,0x00,0x00,0x00,0x00,0x05,0x0F,0xFF, 
	0x03,0x01,0x0F,0x00,0x06, 
	0x63
     }
  },
  {
    "640x350",640,350,4,
    {
      0x5F,0x4F,0x50,0x82,0x54,0x80,0xBF,0x1F,0x00,0x40,0x00,0x00, 
      0x00,0x00,0x00,0x00,0x83,0x85,0x5D,0x28,0x0F,0x63,0xBA,0xE3, 
      0x00,0x01,0x02,0x03,0x04,0x05,0x14,0x07,0x38,0x39,0x3A,0x3B, 
      0x3C,0x3D,0x3E,0x3F,0x01,0x00,0x0F,0x00,0x00, 
      0x00,0x00,0x00,0x00,0x00,0x00,0x05,0x0F,0xFF, 
      0x03,0x01,0x0F,0x00,0x06, 
      0xA3
    }
  },
  {
    "640x480",640,480,4,
    {
      0x5F,0x4F,0x50,0x82,0x54,0x80,0x0B,0x3E,0x00,0x40,0x00,0x00,
      0x00,0x00,0x00,0x00,0xEA,0x8C,0xDF,0x28,0x00,0xE7,0x04,0xE3,
      0x00,0x01,0x02,0x03,0x04,0x05,0x14,0x07,0x38,0x39,0x3A,0x3B,
      0x3C,0x3D,0x3E,0x3F,0x01,0x00,0x0F,0x00,0x00,
      0x00,0x00,0x00,0x00,0x00,0x00,0x05,0x0F,0xFF,
      0x03,0x01,0x0F,0x00,0x06,
      0xE3
    }
  },
  {
    "800x600",800,600,4,
    {
#include "800x600.h"
    }
  },
  {
    "1024x768",1024,768,4,
    {
#include "1024x768.h"
    }
  },
  {
    "768x1024",768,1024,4,
    {
#include "768x1024.h"
    }
  },
};
/*}}}  */

/*{{{  variables*/
static struct mode_record *cur_mode;
static int mem_fd;
static char *graph_mem,*text_mem;
int console_fd;

/* Save buffers */
static char text_regs[60];
static char text_buf[TEXT_SIZE];   /* saved text mode memory */  
static char font_buf1[FONT_SIZE];   /* saved font data        */
static char font_buf2[FONT_SIZE];
/*}}}  */

/*{{{  screen_width*/
int screen_width() { return( cur_mode->width); }
/*}}}  */
/*{{{  screen_heigh*/
int screen_height() { return(cur_mode->height); }
/*}}}  */
/*{{{  screen_depth*/
/* For now, only use monochrome */
int screen_depth() { return(/* cur_mode->depth*/ 1); }
/*}}}  */
/*{{{  screenon*/
void screenon(){  port_in(IS1_R);  port_out(0x20,ATT_IW);}
/*}}}  */
/*{{{  screenoff*/
void screenoff(){  port_in(IS1_R);  port_out(0x00,ATT_IW);}
/*}}}  */
/*{{{  setplane*/
void setplane(int plane){  SETSEQ(0x02,1<<plane);  SETGRA(0x04,plane);}
/*}}}  */
/*{{{  setrplane*/
void setrplane(int plane){SETGRA(0x04,plane);}
/*}}}  */
/*{{{  setwplane*/
void setwplane(int plane){SETSEQ(0x02,1<<plane);}
/*}}}  */
/*{{{  set_regs*/
static int set_regs(char regs[])
{
    int i;

    port_out(0x00,GRA_I); 
    port_out(0x00,GRA_D);  		/* set/reset                        */
    port_in(IS1_R);  	 		/* clear flip-flop                  */
    port_out(0x00,SEQ_I); 
    port_out(0x01,SEQ_D); 		/* synchronous reset on             */
    port_out(regs[MIS+0], MIS_W); 	/* update misc output register      */
    port_out(0x1, SEQ_I); 
    port_out(regs[SEQ+1], SEQ_D);  	/* update clocking mode             */
    for (i = 2; i < SEQ_C; i++) {	/* sequencer registers              */
        port_out(i, SEQ_I); 
        port_out(regs[SEQ+i], SEQ_D); 
    }
    port_out(0x11, CRT_I); 		  
    port_out(regs[CRT+0x11]&0x7F, CRT_D);   /* deprotect registers 0-7      */
    for (i = 0; i < CRT_C; i++) { 	/* CRT controller registers 	    */
        port_out(i, CRT_I); 
        port_out(regs[CRT+i], CRT_D); 
    }
    for (i = 0; i < GRA_C; i++) { 	/* graphics controller registers    */
        port_out(i, GRA_I); 
        port_out(regs[GRA+i], GRA_D); 
    }
    for (i = 0; i < ATT_C; i++) {       /* attribute controller registers   */
        port_in(IS1_R);          	/* reset flip-flop                  */
        port_out(i, ATT_IW);
        port_out(regs[ATT+i],ATT_IW);
    }
    port_out(0x00, SEQ_I); 
    port_out(0x03, SEQ_D);   		/* synchronous reset off            */
    return 0;
}
/*}}}  */
/*{{{  write_mode*/
write_mode(mode)
     int mode;
{
  SETGRA(0x05,mode);
}
/*}}}  */
/*{{{  setmapmask*/
setmapmask(mask)
     int mask;
{
  SETSEQ(0x02,mask);
}
/*}}}  */
/*{{{  BIT_OFFSET*/
int BIT_OFFSET(BITMAP *w){ return (((BIT_WIDE(w)+BITS)&~BITS)*BIT_HIGH(w)>>3); }
/*}}}  */
/*{{{  BIT_SIZE*/
int BIT_SIZE(BITMAP *m){return BIT_Size(BIT_WIDE(m), BIT_HIGH(m), BIT_DEPTH(m)); }
/*}}}  */
/*{{{  BIT_Size*/
int BIT_Size(int wide,int high,int d){return ((((d*wide+BITS)&~BITS)*high)>>3); }
/*}}}  */
/*{{{  BIT_LINE*/
int BIT_LINE(BITMAP *x){ 
if (IS_SCREEN(x)) return ((((x)->primary->wide+BITS)&~BITS)>>LOGBITS);
else return ((((x)->primary->depth*(x)->primary->wide+BITS)&~BITS)>>LOGBITS);
}
/*}}}  */

/*{{{  void setscreen()*/
void setscreen()
{
  int i;

  ioctl(console_fd,KDSETMODE,KD_GRAPHICS);
  screenoff();
  for (i = 0; i < CRT_C; i++) {
    port_out(i, CRT_I); 
    text_regs[CRT+i] = port_in(CRT_D); 
  }
  for (i = 0; i < ATT_C; i++) {
    port_in(IS1_R);
    port_out(i, ATT_IW); 
    text_regs[ATT+i] = port_in(ATT_R); 
  }
  for (i = 0; i < GRA_C; i++) {
    port_out(i, GRA_I); 
    text_regs[GRA+i] = port_in(GRA_D); 
  }
  for (i = 0; i < SEQ_C; i++) {
    port_out(i, SEQ_I); 
    text_regs[SEQ+i] = port_in(SEQ_D); 
  }
  text_regs[MIS] = port_in(MIS_R); 
  memcpy(text_buf,text_mem,TEXT_SIZE);
  set_regs(cur_mode->reg);
  /* save font data */
  port_out(0x04, GRA_I); 
  port_out(0x02, GRA_D); 
  memcpy(font_buf1, graph_mem, FONT_SIZE);
  port_out(0x04, GRA_I); 
  port_out(0x03, GRA_D); 
  memcpy(font_buf2, graph_mem, FONT_SIZE);
  /* restore map mask register */
  port_out(0x04, GRA_I);
  port_out(0x00, GRA_D); 
  SETGRA(0x01,0x00); /* Disable Set/Reset */
  SETGRA(0x03,0x00); /* Replace/No Rotate */
  write_mode(0x00);  /* Set write mode/read mode 0 */
  SETGRA(0x08,0xff); /* No bit masking */
  setmapmask(0x0f);  /* Doens't really matter */
  screenon();
}
/*}}}  */
/*{{{  void resetscreen()*/
void resetscreen()
{
  unsigned char tmp;
  screenoff();
  /* write to all bits */
  port_out(0x08, GRA_I ); 
  port_out(0xFF, GRA_D );   
  /* disable Set/Reset Register */
  port_out(0x01, GRA_I ); 
  port_out(0x00, GRA_D );   
  port_out(0x04,SEQ_I);
  tmp = port_in(SEQ_D) & 0xf7;
  port_out(0x04,SEQ_I);
  port_out(tmp,SEQ_D);
  /* restore character map in plane 2 */
  port_out(0x02, SEQ_I ); 
  port_out(0x04, SEQ_D ); 
  memcpy(graph_mem, font_buf1, FONT_SIZE);
  port_out(0x02, SEQ_I );
  port_out(0x08, SEQ_D );
  memcpy(graph_mem, font_buf2, FONT_SIZE);
  /* restore text mode VGA registers */
  set_regs(text_regs);
  /* restore contents of text mode memory */
  memcpy(text_mem, text_buf, TEXT_SIZE);
  screenon();
  ioctl(console_fd,KDSETMODE,KD_TEXT);
}  
/*}}}  */
/*{{{  char *initscreen(name)*/
char *initscreen(name)
     char *name;
{
    struct mode_record *p;
  
    console_fd = open("/dev/tty0",O_RDWR);
    if (console_fd == -1) {
      printf("init: can't open /dev/tty0 (console)\n");
      exit(1);
    }
  		    
    cur_mode = mode_regs;	/* Figure out what mode we want. */
    for (p = mode_regs; p->name; p++) {
      if (!strcmp(p->name,name)) {
        cur_mode = p;
        break;
      }
    }
    /* Set permissions and memory maps */
  
    /* get I/O permissions for VGA registers */
    if (ioperm(CRT_I, 1, 1)) {
      printf("init: can't get I/O permissions \n");
      exit (-1);
    }
    ioperm(ATT_IW, 1, 1);
    ioperm(GRA_I,  1, 1);
    ioperm(SEQ_I,  1, 1);
    ioperm(PEL_IW, 1, 1);
    ioperm(CRT_D,  1, 1);
    ioperm(ATT_R,  1, 1);
    ioperm(GRA_D,  1, 1);
    ioperm(SEQ_D,  1, 1);
    ioperm(MIS_R,  1, 1);
    ioperm(MIS_W,  1, 1);
    ioperm(IS1_R,  1, 1);
    ioperm(PEL_D,  1, 1);
  
      /* open /dev/mem */
    if ((mem_fd = open("/dev/mem", O_RDWR) ) < 0) {
      printf("init: can't open /dev/mem \n");
      exit (-1);
    }
    /* mmap graphics memory */
    if (((cur_mode->width * cur_mode->height)>>3) <= GRAPH_SIZE) {
      if ((graph_mem = malloc(GRAPH_SIZE + (PAGE_SIZE-1))) == NULL) {
        printf("init: allocation error \n");
        exit (-1);
      }
      if ((unsigned long)graph_mem % PAGE_SIZE)
        graph_mem += PAGE_SIZE - ((unsigned long)graph_mem % PAGE_SIZE);
      graph_mem = (unsigned char *)mmap((caddr_t)graph_mem, 
  				      GRAPH_SIZE,
  				      PROT_READ|PROT_WRITE,
  				      MAP_SHARED|MAP_FIXED,
  				      mem_fd, 
  				      GRAPH_BASE);
      if ((long)graph_mem < 0) {
        printf("init: mmap error \n");
        exit (-1);
      }
  
      /* mmap text memory */
      if ((text_mem = malloc(TEXT_SIZE + (PAGE_SIZE-1))) == NULL) {
        printf("init: allocation error \n");
        exit (-1);
      }
      if ((unsigned long)text_mem % PAGE_SIZE)
        text_mem += PAGE_SIZE - ((unsigned long)text_mem % PAGE_SIZE);
      text_mem = (unsigned char *)mmap((caddr_t)text_mem, 
  				     TEXT_SIZE,
  				     PROT_READ|PROT_WRITE,
  				     MAP_SHARED|MAP_FIXED,
  				     mem_fd, 
  				     TEXT_BASE);
      if ((long)text_mem < 0) {
        printf("init: mmap error \n");
        exit (-1);
      }
    }
    else {	/* Special TVGA initalization -- PART 1 !!!! */
      		/* Allocate 128K for Trident SVGA card... */
      if ((graph_mem = malloc(GRAPH_SIZE*2 + (PAGE_SIZE-1))) == NULL) {
        printf("init: allocation error\n");
        exit(-1);
      }
      if ((unsigned long)graph_mem % PAGE_SIZE) 
        graph_mem += PAGE_SIZE - ((unsigned long)graph_mem % PAGE_SIZE);
      graph_mem = (unsigned char *)mmap((caddr_t)graph_mem, 
  				      GRAPH_SIZE*2,
  				      PROT_READ|PROT_WRITE,
  				      MAP_SHARED|MAP_FIXED,
  				      mem_fd, 
  				      GRAPH_BASE);
      if ((long)graph_mem < 0) {
        printf("init: mmap error \n");
        exit (-1);
      }
      /* Initialize text_memory pointers */
      text_mem = graph_mem + (TEXT_BASE - GRAPH_BASE);
      /* Enable 128K mode for TVGA */
      port_out(0x0b,SEQ_I);
    }
  return graph_mem;
}
/*}}}  */
