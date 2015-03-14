/*
 * $Header: /home/x_cvs/mit/server/ddx/x386/vga256/drivers/et3000/driver.c,v 1.9 1992/09/11 13:36:53 dawes Exp $
 *
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
 * $Header: /proj/X11/mit/server/ddx/x386/drivers/et3000/RCS/driver.c,v 1.2 1991/06/27 00:03:27 root Exp $
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
  unsigned char ExtStart;     /* Tseng ET3000 specials   CRTC 0x23/0x24/0x25 */
  unsigned char CRTCControl;
  unsigned char Overflow;
  unsigned char ZoomControl;    /* TS 6 & 7 */
  unsigned char AuxillaryMode;
  unsigned char Misc;           /* ATC 0x16 */
  unsigned char SegSel;
  } vgaET3000Rec, *vgaET3000Ptr;


static Bool     ET3000Probe();
static char *   ET3000Ident();
static void     ET3000ClockSelect();
static void     ET3000EnterLeave();
static void     ET3000Init();
static void *   ET3000Save();
static void     ET3000Restore();
static void     ET3000Adjust();
#ifdef MONOVGA
extern void     NoopDDA();
#else
extern void     ET3000SetRead();
extern void     ET3000SetWrite();
extern void     ET3000SetReadWrite();
#endif

vgaVideoChipRec ET3000 = {
  ET3000Probe,
  ET3000Ident,
  ET3000EnterLeave,
  ET3000Init,
  ET3000Save,
  ET3000Restore,
  ET3000Adjust,
#ifdef MONOVGA
  NoopDDA,
  NoopDDA,
  NoopDDA,
#else
  ET3000SetRead,
  ET3000SetWrite,
  ET3000SetReadWrite,
#endif
  0x10000,
  0x10000,
  16,
  0xFFFF,
  0x00000, 0x10000,
  0x00000, 0x10000,
  TRUE                                /* Uses 2 banks */
};


#define new ((vgaET3000Ptr)vgaNewVideoState)



/*
 * ET3000ClockSelect --
 *      select one of the possible clocks ...
 */

static void
ET3000ClockSelect(no)
     int no;
{
  unsigned char temp;


  temp = inb(0x3CC);
  outb(0x3C2, ( temp & 0xf3) | ((no << 2) & 0x0C));
  outw(vgaIOBase+0x04, 0x24 | ((no & 0x04) << 7));

}


/*
 * ET3000Ident --
 */

static char *
ET3000Ident()
{
  return("et3000");
}


/*
 * ET3000Probe --
 *      check up whether a Et3000 based board is installed
 */

static Bool
ET3000Probe()
{
  if (vga256InfoRec.chipset)
    {
      if (strcmp(vga256InfoRec.chipset, ET3000Ident()))
	return (FALSE);
      else
	ET3000EnterLeave(ENTER);
    }
  else
    {
      unsigned char temp, origVal, newVal;

      ET3000EnterLeave(ENTER);
      /*
       * Check first that there is a ATC[16] register and then look at
       * CRTC[23]. If both are R/W correctly it's a ET3000 !
       */
      temp = inb(vgaIOBase+0x0A); 
      outb(0x3C0, 0x16 | 0x20); origVal = inb(0x3C1);
      outb(0x3C0, origVal ^ 0x10);
      outb(0x3C0, 0x16 | 0x20); newVal = inb(0x3C1);
      outb(0x3C0, origVal);
      if (newVal != (origVal ^ 0x10))
	{
	  ET3000EnterLeave(LEAVE);
	  return(FALSE);
	}

      outb(vgaIOBase+0x04, 0x23);          origVal = inb(vgaIOBase+0x05);
      outb(vgaIOBase+0x05, origVal ^ 0x07); newVal = inb(vgaIOBase+0x05);
      outb(vgaIOBase+0x05, origVal);
      if (newVal != (origVal ^ 0x07))
	{
	  ET3000EnterLeave(LEAVE);
	  return(FALSE);
	}
    }

  if (!vga256InfoRec.videoRam) vga256InfoRec.videoRam = 512;
  if (!vga256InfoRec.clocks) vgaGetClocks(8 , ET3000ClockSelect);

  vga256InfoRec.chipset = ET3000Ident();
  return(TRUE);
}



/*
 * ET3000EnterLeave --
 *      enable/disable io-mapping
 */

static void 
ET3000EnterLeave(enter)
     Bool enter;
{
  unsigned char temp;

  if (enter)
    {
#ifdef HAS_USL_VTS
      ioctl(x386Info.consoleFd, KDADDIO, 0x3BF);
      ioctl(x386Info.consoleFd, KDENABIO, 0);
#endif

      vgaIOBase = (inb(0x3CC) & 0x01) ? 0x3D0 : 0x3B0;
      outb(0x3BF, 0x03);                           /* unlock ET3000 special */
      outb(vgaIOBase + 8, 0xA0);
      outb(vgaIOBase + 4, 0x11); temp = inb(vgaIOBase + 5);
      outb(vgaIOBase + 5, temp & 0x7F);
    }
  else
    {
      outb(0x3BF, 0x01);                           /* relock ET3000 special */
      outb(vgaIOBase + 8, 0xA0);

#ifdef HAS_USL_VTS
      ioctl(x386Info.consoleFd, KDDISABIO, 0);
      ioctl(x386Info.consoleFd, KDDELIO, 0x3BF);
#endif
    }
}



/*
 * ET3000Restore --
 *      restore a video mode
 */

static void 
ET3000Restore(restore)
  vgaET3000Ptr restore;
{
  unsigned char i;

  outb(0x3CD,0x00); /* segment select */

  vgaHWRestore(restore);

  outw(0x3C4, (restore->ZoomControl << 8)   | 0x06);
  outw(0x3C4, (restore->AuxillaryMode << 8) | 0x07);
  i = inb(vgaIOBase + 0x0A); /* reset flip-flop */
  outb(0x3C0, 0x36); outb(0x3C0, restore->Misc);
  outw(vgaIOBase + 4, (restore->ExtStart << 8)    | 0x23);
  outw(vgaIOBase + 4, (restore->CRTCControl << 8) | 0x24);
  outw(vgaIOBase + 4, (restore->Overflow << 8)    | 0x25);
  outb(0x3CD, restore->SegSel);

  outw(0x3C4, 0x0300); /* now reenable the timing sequencer */
}



/*
 * ET3000Save --
 *      save the current video mode
 */

static void *
ET3000Save(save)
     vgaET3000Ptr save;
{
  unsigned char             i;
  unsigned char             temp1, temp2;

  /*
   * we need this here , cause we MUST disable the ROM SYNC feature
   */
  vgaIOBase = (inb(0x3CC) & 0x01) ? 0x3D0 : 0x3B0;
  outb(vgaIOBase + 4, 0x24); temp1 = inb(vgaIOBase + 5);
  outb(vgaIOBase + 5, temp1 & 0x0f);
  temp2 = inb(0x3CD); outb(0x3CD,0x00); /* segment select */

  save = (vgaET3000Ptr)vgaHWSave(save, sizeof(vgaET3000Rec));
  save->CRTCControl = temp1;
  save->SegSel = temp2;

  outb(vgaIOBase + 4, 0x23); save->ExtStart = inb(vgaIOBase + 5);
  outb(vgaIOBase + 4, 0x25); save->Overflow = inb(vgaIOBase + 5);
  outb(0x3C4, 6); save->ZoomControl = inb(0x3C5);
  outb(0x3C4, 7); save->AuxillaryMode = inb(0x3C5);
  i = inb(vgaIOBase + 0x0A); /* reset flip-flop */
  outb(0x3C0, 0x36); save->Misc = inb(0x3C1); outb(0x3C0, save->Misc);
  
  return ((void *) save);
}



/*
 * ET3000Init --
 *      Handle the initialization, etc. of a screen.
 */

static void
ET3000Init(mode)
     DisplayModePtr mode;
{
  vgaHWInit(mode,sizeof(vgaET3000Rec));

#ifndef MONOVGA
  new->std.Sequencer[4] = 0x06;  /* use the FAST 256 Color Mode */
#endif
  new->ZoomControl   = 0x00; 
  new->AuxillaryMode = 0x88 ;
  new->ExtStart      = 0x00;
  new->CRTCControl   = (int)(new->std.NoClock & 0x04) >> 1 ;
  new->Overflow      = 0x10
    | ((mode->VSyncStart & 0x400) >> 7 )
      | (((mode->VDisplay -1) & 0x400) >> 8 )
	| (((mode->VTotal -2) & 0x400) >> 9 )
	  | ((mode->VSyncStart & 0x400) >> 10 );

#ifdef MONOVGA
  new->Misc          = 0x00;
#else
  new->Misc          = 0x10;
#endif
}



/*
 * ET3000Adjust --
 *      adjust the current video frame to display the mousecursor
 */

static void 
ET3000Adjust(x, y)
     int x, y;
{
  int Base =(y * vga256InfoRec.virtualX + x) >> 3;

  outw(vgaIOBase + 4, (Base & 0x00FF00)        | 0x0C);
  outw(vgaIOBase + 4, ((Base & 0x00FF) << 8)   | 0x0D);
  outw(vgaIOBase + 4, ((Base & 0x010000) >> 7) | 0x23);
}



