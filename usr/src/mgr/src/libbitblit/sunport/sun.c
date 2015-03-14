/* SUN dependent part of the generic color bitblit code */

#undef _POSIX_SOURCE

#include <sys/ioctl.h>
#include <sun/fbio.h>
#include <sys/file.h>
#include <sys/mman.h>
#include <stdio.h>
#include "sun.h"

#define MONO_LEN	32768		/* size of overlay and enable planes */
#define COLOR_SIZE(w,h)    ((8*(w) + 31)/32 * (h))

#define dprintf	if(bit_debug)fprintf
extern int bit_debug;
static int _s_len;		/* length of fb to map */

/* open the display; it looks like memory */

BITMAP *
display_open(name)
char *name;			/* name of frame buffer */
{
   static BITMAP *result = BIT_NULL;
   int fd;					/* file descriptor of frame buffer */
   register DATA *addr;	/* address of frame buffer */
   struct fbtype buff;	/* frame buffer parameters */
   int pagesize;	
	int on = FBVIDEO_ON;
   char *malloc();
	int size;				/* size of fb to mmap (in bytes) */

   /* open the SUN display */

	bit_debug = getenv("BIT_DEBUG");

   if ((fd = open(name, O_RDWR)) < 0) {
		dprintf(stderr,"Cant open frame buffer %s\n",name);
      return (BIT_NULL);
		}

   /* get the frame buffer size */

   if (ioctl(fd, FBIOGTYPE, &buff) < 0) {
		dprintf(stderr,"FBIOGTYPE ioctl\n");
      return (BIT_NULL);
		}

   if (ioctl(fd, FBIOSVIDEO, &on) < 0) {
		dprintf(stderr,"cant enable video ioctl\n");
      return(BIT_NULL);
		}

   /* align space (and fb size) on a page boundary */

   pagesize = getpagesize();
   buff.fb_size = (buff.fb_size+pagesize-1) &~ (pagesize-1);
	if (buff.fb_depth > 1) {	/* hope its cg3 */
		size = 2*MONO_LEN + COLOR_SIZE(buff.fb_width, buff.fb_height);
		size = ((4*size) + pagesize - 1) &~ (pagesize-1);
		}
	else
		size = buff.fb_size;

	dprintf(stderr,"size: %d x %d x %d (%d bytes)\r\n",
		buff.fb_width, buff.fb_height, buff.fb_depth,size);

   /* map the frame buffer into malloc'd space */

   addr = (DATA *) mmap(NULL, _s_len=size,
                   PROT_READ|PROT_WRITE, _MAP_NEW|MAP_SHARED, fd, 0);
   if ((int)addr == -1) {
		dprintf(stderr,"mapped failed\r\n");
      return (BIT_NULL);
		}

   if ((result = (BITMAP *) malloc(sizeof(BITMAP))) == (BITMAP *) 0)
      return (BIT_NULL);

	/* setup color or mono display */

	if (buff.fb_depth > 1) 	/* hope its cg3 */
		result->data = addr + 2*MONO_LEN;
	else
		result->data = addr;
		
   result->primary = result;
   result->x0 = 0,
   result->y0 = 0,
   result->wide = buff.fb_width;
   result->high = buff.fb_height;
   result->depth = buff.fb_depth;
   result->type = _SCREEN;
	result->cache = NULL;
	result->color = 0;

   return (result);
}

/* free resources required by the display */

int
display_close(bitmap)
BITMAP *bitmap;
	{
   if (IS_SCREEN(bitmap) && IS_PRIMARY(bitmap)) {
		if (BIT_DEPTH(bitmap)>1) 	/* cgthree */
			munmap(BIT_DATA(bitmap) - 2*MONO_LEN, _s_len);
		else
			munmap(BIT_DATA(bitmap), _s_len);
		}
	return(0);
	}
