/*  SUN version of portable bit-blt code.  (S. A. Uhler) 
 *  The data on each line is padded to (BITS+1) bits
 */

#ifdef sun
#  ifdef _POSIX_SOURCE
#  define NBBY 8
#  define u_char unsigned char
#  define u_short unsigned short
#  define u_long unsigned long
#  endif
#  include <sys/ioctl.h>
#  include <sun/fbio.h>
#  include <sys/file.h>
#  include <sys/mman.h>
#endif
#include <stdlib.h>
#include <stdio.h>
#include "sun.h"

#define dprintf	if(bit_debug)fprintf
int bit_debug;
static int _s_start;		/* for our "vfree" */
static _s_len;

/* open the display; it looks like memory */

BITMAP *
bit_open(name)
char *name;			/* name of frame buffer */
{
   BITMAP *result = BIT_NULL;
#ifdef sun
   int fd;					/* file descriptor of frame buffer */
   register DATA *addr;	/* address of frame buffer */
   struct fbtype buff;	/* frame buffer parameters */
   int pagesize;	

   /* open the SUN display */

   if ((fd = open(name, O_RDWR)) < 0)
      return (BIT_NULL);

   /* get the frame buffer size */

   if (ioctl(fd, FBIOGTYPE, &buff) < 0)
      return (BIT_NULL);

   /* malloc space for frame buffer */

   pagesize = getpagesize();
   if ((_s_start = (int) malloc(buff.fb_size + pagesize)) == 0)
      return (BIT_NULL);

   /* align space (and fb size) on a page boundary */

   buff.fb_size = (buff.fb_size+pagesize-1) &~ (pagesize-1);
   addr = (DATA *) ((_s_start + pagesize - 1) & ~(pagesize - 1));

   /* map the frame buffer into malloc'd space */

   addr = (DATA*) mmap(addr, _s_len=buff.fb_size,
                   PROT_READ|PROT_WRITE, _MAP_NEW|MAP_SHARED, fd, 0);
   if ((int)addr == -1)
      return (BIT_NULL);

   if ((result = (BITMAP *) malloc(sizeof(BITMAP))) == (BITMAP *) 0)
      return (BIT_NULL);

   result->primary = result;
   result->data = addr;
   result->x0 = 0;
   result->y0 = 0;
   result->wide = buff.fb_width;
   result->high = buff.fb_height;
   result->type = _SCREEN;
   result->depth = 1;
#endif sun
   return (result);
}

/* destroy a bitmap, free up space */

void bit_destroy(bitmap) BITMAP *bitmap;
{
   if (bitmap == (BITMAP *) 0)
      return ;
   if (IS_MEMORY(bitmap) && IS_PRIMARY(bitmap))
      free(bitmap->data);
   else if (IS_SCREEN(bitmap) && IS_PRIMARY(bitmap)) {
      munmap(BIT_DATA(bitmap), _s_len);
      free(_s_start);
		}
   free(bitmap);
   return ;
}
