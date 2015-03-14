/*
 * $Header: /home/x_cvs/mit/server/ddx/x386/vga256/drivers/gvga/driver.c,v 1.10 1992/09/11 13:37:01 dawes Exp $
 * Copyright 1990,91 by Thomas Roell, Dinkelscherben, Germany.
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Thomas Roell not be used in
 * advertising or publicity pertaining to distribution of the software without
 * specific, written prior permission.  Thomas Roell makes no representations
 * about the suitability of this software for any purpose.  It is provided
 * "as is" without express or implied warranty.
 *
 * THOMAS ROELL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL THOMAS ROELL BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Thomas Roell, roell@informatik.tu-muenchen.de
 *
 * $Header: /proj/X11/mit/server/ddx/x386/drivers/gvga/RCS/driver.c,v 1.2 1991/06/27 00:03:49 root Exp $
 */


#include "X.h"
#include "input.h"
#include "screenint.h"

#include "compiler.h"

#include "x386.h"
#include "x386Priv.h"
#include "x386OSD.h"
#include "vga.h"


typedef struct {
  vgaHWRec      std;          /* good old IBM VGA */
  unsigned char ExtCtrlReg1;
  unsigned char ExtCtrlReg2;
  unsigned char ExtCtrlReg3;
  unsigned char ExtCtrlReg4;
  unsigned char ExtCtrlReg5;
  unsigned char MemSegReg;
  } vgaGVGARec, *vgaGVGAPtr;


static Bool     GVGAProbe();
static char *   GVGAIdent();
static void     GVGAClockSelect();
static void     GVGAEnterLeave();
static void     GVGAInit();
static void *   GVGASave();
static void     GVGARestore();
static void     GVGAAdjust();
#ifdef MONOVGA
extern void     NoopDDA();
#else
extern void     GVGASetRead();
extern void     GVGASetWrite();
extern void     GVGASetReadWrite();
#endif

vgaVideoChipRec GVGA = {
  GVGAProbe,
  GVGAIdent,
  GVGAEnterLeave,
  GVGAInit,
  GVGASave,
  GVGARestore,
  GVGAAdjust,
#ifdef MONOVGA
  NoopDDA,
  NoopDDA,
  NoopDDA,
#else
  GVGASetRead,
  GVGASetWrite,
  GVGASetReadWrite,
#endif
  0x10000,
  0x10000,
  16,
  0xFFFF,
  0x00000, 0x10000,
  0x00000, 0x10000,
  TRUE                                  /* Uses 2 banks */
};


#define new ((vgaGVGAPtr)vgaNewVideoState)


/*
 * GVGAIdent
 */

static char *
GVGAIdent()
{
  return("gvga");
}


/*
 * GVGAClockSelect --
 *      select one of the possible clocks ...
 */

static void
GVGAClockSelect(no)
     int no;
{
  unsigned char temp;

  temp = inb(0x3CC);
  outb(0x3C2, ( temp & 0xf3) | ((no << 2) & 0x0C));
  outb(0x3C4, 0x07);
  temp = inb(0x3C5);
  outb(0x3C5, (temp & 0xFE) | ((no & 0x04) >> 2));
}



/*
 * GVGAProbe --
 *      check up whether a GVGA based board is installed
 */

static Bool
GVGAProbe()
{
  if (vga256InfoRec.chipset)
    {
      if (strcmp(vga256InfoRec.chipset, GVGAIdent()))
	return (FALSE);

      if (!vga256InfoRec.videoRam) vga256InfoRec.videoRam = 512;
    }
  else
    {
      unsigned char offset, signature[4];
      int           fd;
#ifdef MACH386
#define C 0xc0000
#define S 0x20000
      int screen_addr;
      int ret;

	if ((fd = open("/dev/iopl", O_RDWR, 0)) < 0) {
		ErrorF("Failed to open /dev/iopl\n");
		return FALSE;
	}
#define KERN_SUCESS 0
	if (KERN_SUCESS != vm_allocate(task_self(), &screen_addr, S, TRUE)) {
		ErrorF("Failed vmallocate %x\n", S);
		close(fd);
		return FALSE;
	}
	if (mmap(screen_addr, S, 3, 1, fd, C) < 0) {
		ErrorF("Failed to mmap %x at %x\n", S, C);
		vm_deallocate(task_self(), screen_addr, S);
		close(fd);
		return FALSE;
	}
	offset = *(u_char *)(screen_addr + 0x37);
	*(u_int *)signature = *(u_int *)(screen_addr + offset);
	if (KERN_SUCESS != vm_deallocate(task_self(), screen_addr, S)) {
		ErrorF("Failed vmdeallocate %x\n", S);
		close(fd);
		return FALSE;
	}
	close(fd);
#undef C
#undef S
#else /* MACH386 */

      if ((fd = open("/dev/mem", O_RDONLY)) == -1 ||
	  lseek(fd, 0xC0037, SEEK_SET) == -1 ||
	  read(fd, &offset, 1) != 1 ||
	  lseek(fd, 0xC0000+offset, SEEK_SET) == -1 ||
	  read(fd, signature, 4) != 4)
	{
	  close(fd);
	  ErrorF("Failed to read VGA Bios.  (X386 must be installed as suid root.)\n");
	  return(FALSE);
	}

      close(fd);
#endif /* MACH386 */

      if (signature[0] != 0x77 ||
	  signature[1] == 0x33 ||
	  signature[1] == 0x55 ||
	  signature[2] != 0x66 ||
	  signature[3] != 0x99)
	return(FALSE);

      if (!vga256InfoRec.videoRam)

	switch(signature[1]) {
	case 0x00:
	case 0x22: 
	  vga256InfoRec.videoRam = 256;
	  break;

	case 0x11:
	default:  
	  vga256InfoRec.videoRam = 512;
	  break;
	}
    }

  GVGAEnterLeave(ENTER);

  if (!vga256InfoRec.clocks) vgaGetClocks(8, GVGAClockSelect);

  vga256InfoRec.chipset = GVGAIdent();
  return(TRUE);
}


/*
 * GVGAEnterLeave --
 *      enable/disable io-mapping
 */

static void 
GVGAEnterLeave(enter)
     Bool enter;
{
  unsigned char temp;

  if (enter)
    {
#ifdef HAS_USL_VTS
      ioctl(x386Info.consoleFd, KDENABIO, 0);
#endif
      outb(vgaIOBase + 4, 0x11); temp = inb(vgaIOBase + 5);
      outb(vgaIOBase + 5, temp & 0x7F);
    }
#ifdef HAS_USL_VTS
  else
    ioctl(x386Info.consoleFd, KDDISABIO, 0);
#endif
}



/*
 * GVGARestore --
 *      restore a video mode
 */

static void 
GVGARestore(restore)
  vgaGVGAPtr restore;
{
  outw(0x3C4, 0x0006);  /* segment select */

  vgaHWRestore(restore);

#ifndef MONOVGA
  outw(vgaIOBase + 4, (restore->ExtCtrlReg1 << 8) | 0x2F);
  outw(0x3C4, (restore->MemSegReg << 8)   | 0x06);
#endif
  outw(0x3C4, (restore->ExtCtrlReg2 << 8) | 0x07);
#ifndef MONOVGA
  outw(0x3C4, (restore->ExtCtrlReg3 << 8) | 0x08);
  outw(0x3C4, (restore->ExtCtrlReg4 << 8) | 0x10);
  outw(0x3CE, (restore->ExtCtrlReg5 << 8) | 0x09);
#endif

  outw(0x3C4, 0x0300); /* now reenable the timing sequencer */
}



/*
 * GVGASave --
 *      save the current video mode
 */

static void *
GVGASave(save)
     vgaGVGAPtr save;
{
  unsigned char             temp;

  outb(0x3C4, 0x06); temp = inb(0x3C5); outb(0x3C5, 0x00); /* segment select */

  save = (vgaGVGAPtr)vgaHWSave(save, sizeof(vgaGVGARec));
#ifndef MONOVGA
  save->MemSegReg = temp;

  outb(vgaIOBase + 4, 0x2f); save->ExtCtrlReg1 = inb(vgaIOBase + 5);
#endif
  outb(0x3C4, 0x07); save->ExtCtrlReg2 = inb(0x3C5);
#ifndef MONOVGA
  outb(0x3C4, 0x08); save->ExtCtrlReg3 = inb(0x3C5);
  outb(0x3C4, 0x10); save->ExtCtrlReg4 = inb(0x3C5);
  outb(0x3CE, 0x09); save->ExtCtrlReg5 = inb(0x3CF);
#endif
  
  return ((void *) save);
}



/*
 * GVGAInit --
 *      Handle the initialization, etc. of a screen.
 */

static void
GVGAInit(mode)
     DisplayModePtr mode;
{
  vgaHWInit(mode,sizeof(vgaGVGARec));

#ifndef MONOVGA
  new->std.Sequencer[4] = 0x06;  /* use the FAST 256 Color Mode */
  new->std.Attribute[16] = 0x01;
  new->ExtCtrlReg1 = 0x02;
#endif
  new->ExtCtrlReg2 = 0x06 | ((int)(new->std.NoClock & 0x04) >> 2);
#ifndef MONOVGA
  new->ExtCtrlReg3 = 0x20;
  new->ExtCtrlReg4 = 0x24;
  new->ExtCtrlReg5 = 0x08;
#endif
}



/*
 * GVGAAdjust --
 *      adjust the current video frame to display the mousecursor
 */

static void 
GVGAAdjust(x, y)
     int x, y;
{
  int Base =(y * vga256InfoRec.virtualX + x) >> 3;

  outw(vgaIOBase + 4, (Base & 0x00FF00)      | 0x0C);
  outw(vgaIOBase + 4, ((Base & 0x00FF) << 8) | 0x0D);
}

