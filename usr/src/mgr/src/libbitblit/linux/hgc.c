/*{{{}}}*/
/*{{{  #includes*/
#include <sys/mman.h>
#include <sys/kd.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>

#include "screen.h"
/*}}}  */
/*{{{  #defines*/
/* video ports */
#define INDEX_REG  0x3b4
#define DATA_REG   0x3b5
#define MODE_REG   0x3b8
#define CONFIG_REG 0x3bf

/* configurations */
#define TEXT_CONF 1
#define GRAPH_CONF 3

/* modes */
#define ACTIVE_MODE 0x08
#define TEXT_MODE 0x20
#define GRAPH_MODE0 0x02
#define GRAPH_MODE1 0x82

/* memory addresses */
#define GRAPH_MODE0_BASE 0xB0000
#define GRAPH_MODE1_BASE 0xB8000
#define GRAPH_SIZE 0x8000
/*}}}  */

/*{{{  port access*/
static void inline port_out(char value, unsigned short port)
{
__asm__ volatile ("outb %0,%1"
		::"a" ((char) value),"d" ((unsigned short) port));
}


static unsigned char inline port_in(unsigned short port)
{
	unsigned char _v;
__asm__ volatile ("inb %1,%0"
		:"=a" (_v):"d" ((unsigned short) port));
	return _v;
}
/*}}}  */
/*{{{  variables*/
int console_fd;
static int mem_fd;
DATA *graph_mem;
/*}}}  */

/*{{{  initscreen*/
DATA *initscreen(char *name)
{
  /*{{{  open console*/
  console_fd = open("/dev/console",O_RDWR);
  if (console_fd == -1) {
    write(2,"Can't open /dev/console\n",24);
    return((DATA*)0);
  }
  /*}}}  */
  /*{{{  open /dev/mem*/
  if ((mem_fd = open("/dev/mem", O_RDWR) ) < 0) {
    write(2,"Can't open /dev/mem\n",20);
    return((DATA*)0);
  }
  /*}}}  */
  /*{{{  mmap screen*/
  if ((graph_mem = malloc(GRAPH_SIZE + (getpagesize()-1))) == NULL) {
    write(2,"Can't alloc memory for screen\n",30);
    return((DATA*)0);
  }
  if ((unsigned long)graph_mem % getpagesize()) graph_mem += getpagesize() - ((unsigned long)graph_mem % getpagesize());
  graph_mem = (DATA*)mmap((caddr_t)graph_mem,
    				    GRAPH_SIZE,
    				    PROT_READ|PROT_WRITE,
    				    MAP_SHARED|MAP_FIXED,
    				    mem_fd,
    				    GRAPH_MODE1_BASE);
  if ((long)graph_mem < 0) {
    write(2,"Can't map screen to memory\n",27);
    return((DATA*)0);
  }
  /*}}}  */
  /*{{{  get I/O permissions for HGC registers*/
  if (ioperm(INDEX_REG, 1, 1)) {
    write(2,"Can't get I/O permissions\n",26);
    return((DATA*)0);
  }
  ioperm(DATA_REG, 1, 1);
  ioperm(MODE_REG,  1, 1);
  ioperm(CONFIG_REG,  1, 1);
  /*}}}  */
  return graph_mem;
}
/*}}}  */
/*{{{  setscreen*/
void setscreen(void)
{
  /*{{{  variables*/
  static char grfval[12]={0x35,0x2d,0x2e,0x7,0x5b,0x02,0x57,0x57,0x2,0x3,0x0,0x0};
  int regsel;
  /*}}}  */

  /*{{{  block vc*/
  ioctl(console_fd,KDSETMODE,KD_GRAPHICS);
  /*}}}  */
  /*{{{  program 6845 for graphics mode*/
  port_out(GRAPH_MODE1,MODE_REG);
  for (regsel=0; regsel<12; regsel++)
  {
    port_out(regsel,INDEX_REG);
    port_out(grfval[regsel],DATA_REG);
  }
  port_out(GRAPH_CONF,CONFIG_REG);
  port_out(GRAPH_MODE1|ACTIVE_MODE,MODE_REG);
  /*}}}  */
}
/*}}}  */
/*{{{  resetscreen*/
void resetscreen()
{
  static char txtval[12]={0x61,0x50,0x52,0xf,0x19,0x06,0x19,0x19,0x2,0xd,0xb,0xc};
  int regsel;

  /*{{{  switch 6845 to text mode*/
  port_out(TEXT_MODE,MODE_REG);
  for (regsel=0; regsel<12; regsel++)
  {
    port_out(regsel,INDEX_REG);
    port_out(txtval[regsel],DATA_REG);
  }
  port_out(TEXT_CONF,CONFIG_REG);
  port_out(TEXT_MODE|ACTIVE_MODE,MODE_REG);
  /*}}}  */
  ioctl(console_fd,KDSETMODE,KD_TEXT);
}
/*}}}  */
