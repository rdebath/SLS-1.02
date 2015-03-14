/*
 * $Header: /home/x_cvs/mit/server/ddx/x386/vga2/drivers/generic/driver.c,v 1.4 1992/09/21 08:22:50 root Exp $
 */

/*
 * Generic VGA driver for mono operation.  This driver doesn't do much since
 * most of the generic stuff is done in vgaHW.c
 *
 * David Dawes August 1992
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
  vgaHWRec std;               /* good old IBM VGA */
  } vgaGENERICRec, *vgaGENERICPtr;

static Bool     GENERICProbe();
static char *   GENERICIdent();
static void     GENERICClockSelect();
static void     GENERICEnterLeave();
static void     GENERICInit();
static void *   GENERICSave();
static void     GENERICRestore();
static void     GENERICAdjust();
extern void	NoopDDA();

vgaVideoChipRec GENERIC = {
  GENERICProbe,
  GENERICIdent,
  GENERICEnterLeave,
  GENERICInit,
  GENERICSave,
  GENERICRestore,
  GENERICAdjust,
  NoopDDA,
  NoopDDA,
  NoopDDA,
  0x10000,
  0x10000,
  16,
  0xFFFF,
  0x00000, 0x10000,
  0x00000, 0x10000,
};

#define new ((vgaGENERICPtr)vgaNewVideoState)


/*
 * GENERICIdent
 */

char *
GENERICIdent()

{
  return("generic");
}


/*
 * GENERICClockSelect --
 *      select one of the possible clocks ...
 */

static void
GENERICClockSelect(no)
     int no;
{
  unsigned char temp;

  temp = inb(0x3CC);
  outb(0x3C2, ( temp & 0xf3) | ((no << 2) & 0x04));
}


/*
 * GENERICProbe --
 *      check up whether a Et4000 based board is installed
 */

static Bool
GENERICProbe()
{
  int numClocks;

  if (vga2InfoRec.chipset)
    {
      if (strcmp(vga2InfoRec.chipset, GENERICIdent()))
	return (FALSE);
      else
	GENERICEnterLeave(ENTER);
    }
  else
    {
      unsigned char temp, origVal, newVal;

      GENERICEnterLeave(ENTER);

      /*
       * Check if there is a VGA.  VGA has one more attribute register
       * than EGA, so see if we can read/write it.
       */

      temp = inb(vgaIOBase + 0x0A); /* reset ATC flip-flop */
      outb(0x3C0, 0x14 | 0x20); origVal = inb(0x3C1);
      outb(0x3C0, origVal ^ 0x0F);
      outb(0x3C0, 0x14 | 0x20); newVal = inb(0x3C1);
      outb(0x3C0, origVal);
      if (newVal != (origVal ^ 0x0F))
	{
	  GENERICEnterLeave(LEAVE);
	  return(FALSE);
	}
    }

  /*
   * We only need the minimum 64k of memory, so don't try to check.
   */
  if (!vga2InfoRec.videoRam)
    {
      vga2InfoRec.videoRam = 64;
    }
  if (!vga2InfoRec.clocks)
    {
      vga2InfoRec.clocks = 2;
      vga2InfoRec.clock[0] = 25;
      vga2InfoRec.clock[1] = 28;
    }

  vga2InfoRec.chipset = GENERICIdent();
  return(TRUE);
}


/*
 * GENERICEnterLeave --
 *      enable/disable io-mapping
 */

static void 
GENERICEnterLeave(enter)
     Bool enter;
{
  unsigned char temp;

  if (enter)
    {
#ifdef HAS_USL_VTS
      ioctl(x386Info.consoleFd, KDENABIO, 0);
#endif

      vgaIOBase = (inb(0x3CC) & 0x01) ? 0x3D0 : 0x3B0;
      /* Unprotect CRTC[0-7] */
      outb(vgaIOBase + 4, 0x11); temp = inb(vgaIOBase + 5);
      outb(vgaIOBase + 5, temp & 0x7F);
    }
  else
    {
#ifdef HAS_USL_VTS
      ioctl(x386Info.consoleFd, KDDISABIO, 0);
#endif
    }
}



/*
 * GENERICRestore --
 *      restore a video mode
 */

static void 
GENERICRestore(restore)
  vgaGENERICPtr restore;
{
  vgaHWRestore(restore);

  outw(0x3C4, 0x0300); /* now reenable the timing sequencer */
}



/*
 * GENERICSave --
 *      save the current video mode
 */

static void *
GENERICSave(save)
     vgaGENERICPtr save;
{
  save = (vgaGENERICPtr)vgaHWSave(save, sizeof(vgaGENERICRec));

  return ((void *) save);
}



/*
 * GENERICInit --
 *      Handle the initialization of the VGAs registers
 */

static void
GENERICInit(mode)
     DisplayModePtr mode;
{
  vgaHWInit(mode,sizeof(vgaGENERICRec));
}



/*
 * GENERICAdjust --
 *      adjust the current video frame to display the mousecursor
 */

static void 
GENERICAdjust(x, y)
     int x, y;
{
  int Base = (y * vga2InfoRec.virtualX + x) >> 3;

  outw(vgaIOBase + 4, (Base & 0x00FF00) | 0x0C);
  outw(vgaIOBase + 4, ((Base & 0x00FF) << 8) | 0x0D);
  outw(vgaIOBase + 4, ((Base & 0x030000) >> 8) | 0x33);
}
