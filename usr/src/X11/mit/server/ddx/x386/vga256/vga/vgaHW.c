/*
 * $Header: /home/x_cvs/mit/server/ddx/x386/vga256/vga/vgaHW.c,v 1.14 1992/09/20 03:58:44 dawes Exp $
 * $XConsortium: vgaHW.c,v 1.3 91/08/26 15:40:56 gildea Exp $
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
 */

#define _NEED_SYSI86

#ifndef MONOVGA
#ifndef SAVE_FONT1
#define SAVE_FONT1
#endif
#if defined(__386BSD__) || defined(MACH386) || defined(linux)
#ifndef NEED_SAVED_CMAP
#define NEED_SAVED_CMAP
#endif
#ifndef SAVE_TEXT
#define SAVE_TEXT
#endif
#endif
#endif

#if defined(__386BSD__) || defined(MACH386) || defined(linux)
#ifndef SAVE_FONT2
#define SAVE_FONT2
#endif
#endif

#include "compiler.h"

#include "x386OSD.h"
#include "vga.h"
#include "scrnintstr.h"

#ifdef MONOVGA
#define BIT_PLANE 3
#endif

#define new ((vgaHWPtr)vgaNewVideoState)

#ifdef NEED_SAVED_CMAP
/* This default colourmap is used only when it can't be read from the VGA */

unsigned char defaultDAC[768] =
{
     0,  0,  0,    0,  0, 42,    0, 42,  0,    0, 42, 42,
    42,  0,  0,   42,  0, 42,   42, 21,  0,   42, 42, 42,
    21, 21, 21,   21, 21, 63,   21, 63, 21,   21, 63, 63,
    63, 21, 21,   63, 21, 63,   63, 63, 21,   63, 63, 63,
     0,  0,  0,    5,  5,  5,    8,  8,  8,   11, 11, 11,
    14, 14, 14,   17, 17, 17,   20, 20, 20,   24, 24, 24,
    28, 28, 28,   32, 32, 32,   36, 36, 36,   40, 40, 40,
    45, 45, 45,   50, 50, 50,   56, 56, 56,   63, 63, 63,
     0,  0, 63,   16,  0, 63,   31,  0, 63,   47,  0, 63,
    63,  0, 63,   63,  0, 47,   63,  0, 31,   63,  0, 16,
    63,  0,  0,   63, 16,  0,   63, 31,  0,   63, 47,  0,
    63, 63,  0,   47, 63,  0,   31, 63,  0,   16, 63,  0,
     0, 63,  0,    0, 63, 16,    0, 63, 31,    0, 63, 47,
     0, 63, 63,    0, 47, 63,    0, 31, 63,    0, 16, 63,
    31, 31, 63,   39, 31, 63,   47, 31, 63,   55, 31, 63,
    63, 31, 63,   63, 31, 55,   63, 31, 47,   63, 31, 39,
    63, 31, 31,   63, 39, 31,   63, 47, 31,   63, 55, 31,
    63, 63, 31,   55, 63, 31,   47, 63, 31,   39, 63, 31,
    31, 63, 31,   31, 63, 39,   31, 63, 47,   31, 63, 55,
    31, 63, 63,   31, 55, 63,   31, 47, 63,   31, 39, 63,
    45, 45, 63,   49, 45, 63,   54, 45, 63,   58, 45, 63,
    63, 45, 63,   63, 45, 58,   63, 45, 54,   63, 45, 49,
    63, 45, 45,   63, 49, 45,   63, 54, 45,   63, 58, 45,
    63, 63, 45,   58, 63, 45,   54, 63, 45,   49, 63, 45,
    45, 63, 45,   45, 63, 49,   45, 63, 54,   45, 63, 58,
    45, 63, 63,   45, 58, 63,   45, 54, 63,   45, 49, 63,
     0,  0, 28,    7,  0, 28,   14,  0, 28,   21,  0, 28,
    28,  0, 28,   28,  0, 21,   28,  0, 14,   28,  0,  7,
    28,  0,  0,   28,  7,  0,   28, 14,  0,   28, 21,  0,
    28, 28,  0,   21, 28,  0,   14, 28,  0,    7, 28,  0,
     0, 28,  0,    0, 28,  7,    0, 28, 14,    0, 28, 21,
     0, 28, 28,    0, 21, 28,    0, 14, 28,    0,  7, 28,
    14, 14, 28,   17, 14, 28,   21, 14, 28,   24, 14, 28,
    28, 14, 28,   28, 14, 24,   28, 14, 21,   28, 14, 17,
    28, 14, 14,   28, 17, 14,   28, 21, 14,   28, 24, 14,
    28, 28, 14,   24, 28, 14,   21, 28, 14,   17, 28, 14,
    14, 28, 14,   14, 28, 17,   14, 28, 21,   14, 28, 24,
    14, 28, 28,   14, 24, 28,   14, 21, 28,   14, 17, 28,
    20, 20, 28,   22, 20, 28,   24, 20, 28,   26, 20, 28,
    28, 20, 28,   28, 20, 26,   28, 20, 24,   28, 20, 22,
    28, 20, 20,   28, 22, 20,   28, 24, 20,   28, 26, 20,
    28, 28, 20,   26, 28, 20,   24, 28, 20,   22, 28, 20,
    20, 28, 20,   20, 28, 22,   20, 28, 24,   20, 28, 26,
    20, 28, 28,   20, 26, 28,   20, 24, 28,   20, 22, 28,
     0,  0, 16,    4,  0, 16,    8,  0, 16,   12,  0, 16,
    16,  0, 16,   16,  0, 12,   16,  0,  8,   16,  0,  4,
    16,  0,  0,   16,  4,  0,   16,  8,  0,   16, 12,  0,
    16, 16,  0,   12, 16,  0,    8, 16,  0,    4, 16,  0,
     0, 16,  0,    0, 16,  4,    0, 16,  8,    0, 16, 12,
     0, 16, 16,    0, 12, 16,    0,  8, 16,    0,  4, 16,
     8,  8, 16,   10,  8, 16,   12,  8, 16,   14,  8, 16,
    16,  8, 16,   16,  8, 14,   16,  8, 12,   16,  8, 10,
    16,  8,  8,   16, 10,  8,   16, 12,  8,   16, 14,  8,
    16, 16,  8,   14, 16,  8,   12, 16,  8,   10, 16,  8,
     8, 16,  8,    8, 16, 10,    8, 16, 12,    8, 16, 14,
     8, 16, 16,    8, 14, 16,    8, 12, 16,    8, 10, 16,
    11, 11, 16,   12, 11, 16,   13, 11, 16,   15, 11, 16,
    16, 11, 16,   16, 11, 15,   16, 11, 13,   16, 11, 12,
    16, 11, 11,   16, 12, 11,   16, 13, 11,   16, 15, 11,
    16, 16, 11,   15, 16, 11,   13, 16, 11,   12, 16, 11,
    11, 16, 11,   11, 16, 12,   11, 16, 13,   11, 16, 15,
    11, 16, 16,   11, 15, 16,   11, 13, 16,   11, 12, 16,
     0,  0,  0,    0,  0,  0,    0,  0,  0,    0,  0,  0,
     0,  0,  0,    0,  0,  0,    0,  0,  0,    0,  0,  0,
};
#endif /* NEED_SAVED_CMAP */


/*
 * vgaSaveScreen -- 
 *      Disable the video on the frame buffer to save the screen.
 */
Bool
vgaSaveScreen (pScreen, on)
     ScreenPtr     pScreen;
     Bool          on;
{
  unsigned char   state;

  if (on)
    SetTimeSinceLastInputEvent();
  if (x386VTSema) {
    outb(0x3C4,1);
    state = inb(0x3C5);
  
    if (on) state &= 0xDF;
    else    state |= 0x20;
    
    /*
     * turn off srceen if necessary
     */
    outw(0x3C4, 0x0100);              /* syncronous reset */
    outw(0x3C4, (state << 8) | 0x01); /* change mode */
    outw(0x3C4, 0x0300);              /* syncronous reset */

  } else {
    if (on)
      ((vgaHWPtr)vgaNewVideoState)->Sequencer[1] &= 0xDF;
    else
      ((vgaHWPtr)vgaNewVideoState)->Sequencer[1] |= 0x20;
  }

  return(TRUE);
}



/*
 * vgaHWRestore --
 *      restore a video mode
 */

void
vgaHWRestore(restore)
     vgaHWPtr restore;
{
  int i,tmp;

  tmp = inb(vgaIOBase + 0x0A);				/* Reset flip-flop */
  outb(0x3C0, 0x00);					/* Disable video */

  /*
   * This here is a workaround a bug in the kd-driver. We MUST explicitely
   * restore the font we got, when we entered graphics mode.
   * The bug was seen on ESIX, and ISC 2.0.2 when using a monochrome
   * monitor. 
   *
   * BTW, also GIO_FONT seems to have a bug, so we cannot use it, to get
   * a font.
   */
  
  if(restore->FontInfo1 || restore->FontInfo2 || restore->TextInfo) {
    /*
     * here we switch temporary to 16 color-plane-mode, to simply
     * copy the font-info and saved text
     *
     * BUGALLERT: The vga's segment-select register MUST be set appropriate !
     */
    tmp = inb(vgaIOBase + 0x0A); /* reset flip-flop */
    outb(0x3C0,0x30); outb(0x3C0, 0x01); /* graphics mode */
    if (restore->FontInfo1) {
      outw(0x3C4, 0x0402);    /* write to plane 2 */
      outw(0x3C4, 0x0604);    /* enable plane graphics */
      outw(0x3CE, 0x0204);    /* read plane 2 */
      outw(0x3CE, 0x0005);    /* write mode 0, read mode 0 */
      outw(0x3CE, 0x0506);    /* set graphics */
      bcopy(restore->FontInfo1, vgaBase, 8192);
    }
    if (restore->FontInfo2) {
      outw(0x3C4, 0x0802);    /* write to plane 3 */
      outw(0x3C4, 0x0604);    /* enable plane graphics */
      outw(0x3CE, 0x0304);    /* read plane 3 */
      outw(0x3CE, 0x0005);    /* write mode 0, read mode 0 */
      outw(0x3CE, 0x0506);    /* set graphics */
      bcopy(restore->FontInfo2, vgaBase, 8192);
    }
    if (restore->TextInfo) {
      outw(0x3C4, 0x0102);    /* write to plane 0 */
      outw(0x3C4, 0x0604);    /* enable plane graphics */
      outw(0x3CE, 0x0004);    /* read plane 0 */
      outw(0x3CE, 0x0005);    /* write mode 0, read mode 0 */
      outw(0x3CE, 0x0506);    /* set graphics */
      bcopy(restore->TextInfo, vgaBase, 4096);
      outw(0x3C4, 0x0202);    /* write to plane 1 */
      outw(0x3C4, 0x0604);    /* enable plane graphics */
      outw(0x3CE, 0x0104);    /* read plane 1 */
      outw(0x3CE, 0x0005);    /* write mode 0, read mode 0 */
      outw(0x3CE, 0x0506);    /* set graphics */
      bcopy(restore->TextInfo + 4096, vgaBase, 4096);
    }
  }

  tmp = inb(vgaIOBase + 0x0A);				/* Reset flip-flop */
  outb(0x3C0, 0x00);					/* Disable video */

#if 0
  outw(0x3C4, ((restore->Sequencer[0] & 0xFD) << 8) | 0x00);
#endif

  if (vgaIOBase == 0x3B0)
    restore->MiscOutReg &= 0xFE;
  else
    restore->MiscOutReg |= 0x01;

  outb(0x3C2, restore->MiscOutReg);

  outw(0x3C4, 0x0100);				/* disable timing sequencer */
  
  for (i=0; i<5;  i++) outw(0x3C4, (restore->Sequencer[i] << 8) | i);

  outw(0x3C4, 0x0300);				/* reenable timing sequencer */
  
  /* Ensure CRTC registers 0-7 are unlocked by clearing bit 7 or CRTC[17] */

  outw(vgaIOBase + 4, ((restore->CRTC[17] & 0x7F) << 8) | 17);

  for (i=0; i<25; i++) outw(vgaIOBase + 4,(restore->CRTC[i] << 8) | i);

  for (i=0; i<9;  i++) outw(0x3CE, (restore->Graphics[i] << 8) | i);

  for (i=0; i<21; i++) {
    tmp = inb(vgaIOBase + 0x0A);
    outb(0x3C0,i); outb(0x3C0, restore->Attribute[i]);
  }

  
#ifndef MONOVGA
  outb(0x3C6,0xFF);
  outb(0x3C8,0x00);
  for (i=0; i<768; i++) outb(0x3C9, restore->DAC[i]);
#endif

  /* Turn on PAS bit */
  tmp = inb(vgaIOBase + 0x0A);
  outb(0x3C0, 0x20);

}



/*
 * vgaHWSave --
 *      save the current video mode
 */

void *
vgaHWSave(save, size)
     vgaHWPtr save;
     int          size;
{
  int           i,tmp;
#ifndef MONOVGA
  Bool	        first_time = FALSE;
#endif

  if (save == NULL) {
    save = (vgaHWPtr)Xcalloc(size);
    /*
     * Here we are, when we first save the videostate. This means we came here
     * to save the original Text mode. Because some drivers may depend
     * on NoClock we set it here to a resonable value.
     */
#ifndef MONOVGA
    first_time = TRUE;
#endif
    save->NoClock = (inb(0x3CC) >> 2) & 3;
  }

  /*
   * now get the fuck'in register
   */
  save->MiscOutReg = inb(0x3CC);
  vgaIOBase = (save->MiscOutReg & 0x01) ? 0x3D0 : 0x3B0;

  tmp = inb(vgaIOBase + 0x0A); /* reset flip-flop */
  outb(0x3C0, 0x00);

#ifdef NEED_SAVED_CMAP
  /*
   * Some recent (1991) ET4000 chips have a HW bug that prevents the reading
   * of the color lookup table.  Mask rev 9042EAI is known to have this bug.
   *
   * X386 already keeps track of the contents of the color lookup table so
   * reading the HW isn't needed.  Therefore, as a workaround for this HW
   * bug, the following (correct) code has been #ifdef'ed out.  This is also
   * a valid change for ET4000 chips that don't have the HW bug.  The code
   * is really just being retained for reference.  MWS 22-Aug-91
   *
   * This is *NOT* true for 386BSD, Mach -- the initial colour map must be
   * restored.  When saving the text mode, we check if the colourmap is
   * readable.  If so we read it.  If not, we set the saved map to a
   * default map (taken from Ferraro's "Programmer's Guide to the EGA and
   * VGA Cards" 2nd ed).
   */

  if (first_time)
  {
    int read_error = 0;

    outb(0x3C6,0xFF);
    /*
     * check if we can read the lookup table
     */
    outb(0x3C7,0x00);
    for (i=0; i<3; i++) save->DAC[i] = inb(0x3C9);
    outb(0x3C8,0x00);
    for (i=0; i<3; i++) outb(0x3C9, ~save->DAC[i]);
    outb(0x3C7,0x00);
    for (i=0; i<3; i++)
    {
      unsigned char tmp = inb(0x3C9);
      if (tmp != (~save->DAC[i]&0x3F)) read_error++;
    }
  
    if (read_error)
    {
      /*			 
       * save the default lookup table
       */
      bcopy(defaultDAC, save->DAC, 768);
      ErrorF("VGA256: Cannot read colourmap from VGA.");
      ErrorF("  Will restore with default\n");
    }
    else
    {
      /*			 
       * save the colorlookuptable 
       */
      outb(0x3C7,0x01);
      for (i=3; i<768; i++) save->DAC[i] = inb(0x3C9); 
    }
  }
#endif /* NEED_SAVED_CMAP */

  for (i=0; i<25; i++) { outb(vgaIOBase + 4,i);
			 save->CRTC[i] = inb(vgaIOBase + 5); }

  for (i=0; i<21; i++) {
    tmp = inb(vgaIOBase + 0x0A);
    outb(0x3C0,i);
    save->Attribute[i] = inb(0x3C1);
  }

  for (i=0; i<9;  i++) { outb(0x3CE,i); save->Graphics[i]  = inb(0x3CF); }

  for (i=0; i<5;  i++) { outb(0x3C4,i); save->Sequencer[i]   = inb(0x3C5); }
  
  /* Do we need this? (DHD) */
  outb(0x3C2, save->MiscOutReg | 0x01);		/* shift to colour emulation */

  /*
   * get the character sets, and text screen if required
   */
  if (((save->Attribute[0x10] & 0x01) == 0)) {
#ifdef SAVE_FONT1
    if (save->FontInfo1 == NULL) {
      save->FontInfo1 = (pointer)xalloc(8192);
      /*
       * Here we switch temporary to 16 color-plane-mode, to simply
       * copy the font-info
       *
       * BUGALLERT: The vga's segment-select register MUST be set appropriate !
       */
      tmp = inb(vgaIOBase + 0x0A); /* reset flip-flop */
      outb(0x3C0,0x30); outb(0x3C0, 0x01); /* graphics mode */
      outw(0x3C4, 0x0402);    /* write to plane 2 */
      outw(0x3C4, 0x0604);    /* enable plane graphics */
      outw(0x3CE, 0x0204);    /* read plane 2 */
      outw(0x3CE, 0x0005);    /* write mode 0, read mode 0 */
      outw(0x3CE, 0x0506);    /* set graphics */
      bcopy(vgaBase, save->FontInfo1, 8192);
    }
#endif /* SAVE_FONT1 */
#ifdef SAVE_FONT2
    if (save->FontInfo2 == NULL) {
      save->FontInfo2 = (pointer)xalloc(8192);
      tmp = inb(vgaIOBase + 0x0A); /* reset flip-flop */
      outb(0x3C0,0x30); outb(0x3C0, 0x01); /* graphics mode */
      outw(0x3C4, 0x0802);    /* write to plane 3 */
      outw(0x3C4, 0x0604);    /* enable plane graphics */
      outw(0x3CE, 0x0304);    /* read plane 3 */
      outw(0x3CE, 0x0005);    /* write mode 0, read mode 0 */
      outw(0x3CE, 0x0506);    /* set graphics */
      bcopy(vgaBase, save->FontInfo2, 8192);
    }
#endif /* SAVE_FONT2 */
#ifdef SAVE_TEXT
    if (save->TextInfo == NULL) {
      save->TextInfo = (pointer)xalloc(8192);
      tmp = inb(vgaIOBase + 0x0A); /* reset flip-flop */
      outb(0x3C0,0x30); outb(0x3C0, 0x01); /* graphics mode */
      /*
       * This is a quick hack to save the text screen for system that don't
       * restore it automatically.
       */
      outw(0x3C4, 0x0102);    /* write to plane 0 */
      outw(0x3C4, 0x0604);    /* enable plane graphics */
      outw(0x3CE, 0x0004);    /* read plane 0 */
      outw(0x3CE, 0x0005);    /* write mode 0, read mode 0 */
      outw(0x3CE, 0x0506);    /* set graphics */
      bcopy(vgaBase, save->TextInfo, 4096);
      outw(0x3C4, 0x0202);    /* write to plane 1 */
      outw(0x3C4, 0x0604);    /* enable plane graphics */
      outw(0x3CE, 0x0104);    /* read plane 1 */
      outw(0x3CE, 0x0005);    /* write mode 0, read mode 0 */
      outw(0x3CE, 0x0506);    /* set graphics */
      bcopy(vgaBase, save->TextInfo + 4096, 4096);
    }
#endif /* SAVE_TEXT */
  }

  /* Turn on PAS bit */
  tmp = inb(vgaIOBase + 0x0A);
  outb(0x3C0, 0x20);
  
  return ((void *) save);
}



/*
 * vgaHWInit --
 *      Handle the initialization, etc. of a screen.
 */

void
vgaHWInit(mode,size)
     int             size;
     DisplayModePtr      mode;
{
  int                i;

  if (vgaNewVideoState == NULL) {
    vgaNewVideoState = (void *)Xcalloc(size);

#ifndef MONOVGA
    /*
     * initialize default colormap for monochrome
     */
    for (i=0; i<3;   i++) new->DAC[i] = 0x00;
    for (i=3; i<768; i++) new->DAC[i] = 0x3F;
#endif

  }

  /*
   * Get NoClock
   */
  new->NoClock   = mode->Clock;

  /*
   * compute correct Hsync & Vsync polarity 
   */
  if ((mode->Flags & (V_PHSYNC | V_NHSYNC))
      && (mode->Flags & (V_PVSYNC | V_NVSYNC)))
      {
	new->MiscOutReg = 0x23;
	if (mode->Flags & V_NHSYNC) new->MiscOutReg |= 0x40;
	if (mode->Flags & V_NVSYNC) new->MiscOutReg |= 0x80;
      }
      else
      {
	if      (mode->VDisplay < 400) new->MiscOutReg = 0xA3;
	else if (mode->VDisplay < 480) new->MiscOutReg = 0x63;
	else if (mode->VDisplay < 768) new->MiscOutReg = 0xE3;
	else                           new->MiscOutReg = 0x23;
      }
  new->MiscOutReg |= (new->NoClock & 0x03) << 2;
  
  /*
   * Time Sequencer
   */
  new->Sequencer[0] = 0x00;
  new->Sequencer[1] = 0x01;
#ifdef MONOVGA
  new->Sequencer[2] = 1 << BIT_PLANE;
#else
  new->Sequencer[2] = 0x0F;
#endif
  new->Sequencer[3] = 0x00;                             /* Font select */
#ifdef MONOVGA
  new->Sequencer[4] = 0x06;                             /* Misc */
#else
  new->Sequencer[4] = 0x0E;                             /* Misc */
#endif

  /*
   * CRTC Controller
   */
  new->CRTC[0]  = (mode->HTotal >> 3) - 5;
  new->CRTC[1]  = (mode->HDisplay >> 3) - 1;
  new->CRTC[2]  = (mode->HSyncStart >> 3) -1;
  new->CRTC[3]  = ((mode->HSyncEnd >> 3) & 0x1F) | 0x80;
  new->CRTC[4]  = (mode->HSyncStart >> 3);
  new->CRTC[5]  = (((mode->HSyncEnd >> 3) & 0x20 ) << 2 )
    | (((mode->HSyncEnd >> 3)) & 0x1F);
  new->CRTC[6]  = (mode->VTotal - 2) & 0xFF;
  new->CRTC[7]  = (((mode->VTotal -2) & 0x100) >> 8 )
    | (((mode->VDisplay -1) & 0x100) >> 7 )
      | ((mode->VSyncStart & 0x100) >> 6 )
	| (((mode->VSyncStart) & 0x100) >> 5 )
	  | 0x10
	    | (((mode->VTotal -2) & 0x200)   >> 4 )
	      | (((mode->VDisplay -1) & 0x200) >> 3 )
		| ((mode->VSyncStart & 0x200) >> 2 );
  new->CRTC[8]  = 0x00;
  new->CRTC[9]  = ((mode->VSyncStart & 0x200) >>4) | 0x40;
  new->CRTC[10] = 0x00;
  new->CRTC[11] = 0x00;
  new->CRTC[12] = 0x00;
  new->CRTC[13] = 0x00;
  new->CRTC[14] = 0x00;
#ifdef MONOVGA
  new->CRTC[15] = 0x59;
#else
  new->CRTC[15] = 0x00;
#endif
  new->CRTC[16] = mode->VSyncStart & 0xFF;
  new->CRTC[17] = (mode->VSyncEnd & 0x0F) | 0x20;
  new->CRTC[18] = (mode->VDisplay -1) & 0xFF;
  new->CRTC[19] = vga256InfoRec.virtualX >> 4;  /* just a guess */
  new->CRTC[20] = 0x00;
  new->CRTC[21] = mode->VSyncStart & 0xFF; 
  new->CRTC[22] = (mode->VSyncStart +1) & 0xFF;
#ifdef MONOVGA
  new->CRTC[23] = 0xE3;
#else
  new->CRTC[23] = 0xC3;
#endif
  new->CRTC[24] = 0xFF;

  /*
   * Graphics Display Controller
   */
  new->Graphics[0] = 0x00;
  new->Graphics[1] = 0x00;
  new->Graphics[2] = 0x00;
  new->Graphics[3] = 0x00;
#ifdef MONOVGA
  new->Graphics[4] = BIT_PLANE;
  new->Graphics[5] = 0x00;
#else
  new->Graphics[4] = 0x00;
  new->Graphics[5] = 0x40;
#endif
  new->Graphics[6] = 0x05;   /* only map 64k VGA memory !!!! */
  new->Graphics[7] = 0x0F;
  new->Graphics[8] = 0xFF;
  
#ifdef MONOVGA
  /* Initialise the Mono map according to which bit-plane gets written to */

#define WHITE_VALUE 0x3F
#define BLACK_VALUE 0x00

  for (i=0; i<16; i++)
    if (i & (1<<BIT_PLANE))
      new->Attribute[i] = WHITE_VALUE;
    else
      new->Attribute[i] = BLACK_VALUE;

  new->Attribute[16] = 0x01;  /* -VGA2- */ /* wrong for the ET4000 */
  new->Attribute[17] = 0x00;  /* -VGA2- */
#else
  new->Attribute[0]  = 0x00; /* standart colormap translation */
  new->Attribute[1]  = 0x01;
  new->Attribute[2]  = 0x02;
  new->Attribute[3]  = 0x03;
  new->Attribute[4]  = 0x04;
  new->Attribute[5]  = 0x05;
  new->Attribute[6]  = 0x06;
  new->Attribute[7]  = 0x07;
  new->Attribute[8]  = 0x08;
  new->Attribute[9]  = 0x09;
  new->Attribute[10] = 0x0A;
  new->Attribute[11] = 0x0B;
  new->Attribute[12] = 0x0C;
  new->Attribute[13] = 0x0D;
  new->Attribute[14] = 0x0E;
  new->Attribute[15] = 0x0F;
  new->Attribute[16] = 0x41; /* wrong for the ET4000 */
  new->Attribute[17] = 0x01;
#endif
  new->Attribute[18] = 0x0F;
  new->Attribute[19] = 0x00;
  new->Attribute[20] = 0x00;
}


/*
 * vgaGetClocks --
 *      get the dot-clocks via a BIG BAD hack ...
 */

void
vgaGetClocks(num, ClockFunc)
     int num;
     void (*ClockFunc)();
{
  int          norm;
  register int status = vgaIOBase + 0x0A;
  unsigned long       i, j, cnt, rcnt, sync;
  unsigned char tmp;

  /* First save MiscOutReg */
  tmp = inb(0x3CC);

#ifdef SYSV386
  sysi86(SI86V86, V86SC_IOPL, PS_IOPL);
#endif

  for (i = 1; i < num; i++) {
    
    (*ClockFunc)(i);

    cnt  = 0;
    sync = 200000;

    intr_disable();
    while ((inb(status) & 0x08) == 0x00) if (sync-- == 0) goto finish;
    while ((inb(status) & 0x08) == 0x08) if (sync-- == 0) goto finish;
    while ((inb(status) & 0x08) == 0x00) if (sync-- == 0) goto finish;
    
    for (rcnt = 0; rcnt < 5; rcnt++) {
      while (!(inb(status) & 0x08)) cnt++;
      while ((inb(status) & 0x08)) cnt++;
    }
    
  finish:
    intr_enable();

    vga256InfoRec.clock[i] = cnt ? cnt : 1000000;
  }

  for (i = 2; i < num; i++)
    vga256InfoRec.clock[i] = (int)(0.5 +
      (28.322 * vga256InfoRec.clock[1]) / (vga256InfoRec.clock[i]));

  vga256InfoRec.clock[0] = 25;
  vga256InfoRec.clock[1] = 28;

  for (i=0; i < num; i++)
    for (j=i+1; j < num; j++)
      if (vga256InfoRec.clock[i] == vga256InfoRec.clock[j]) 
	vga256InfoRec.clock[j] = 0;

  vga256InfoRec.clocks = num;
  (ClockFunc)(0);

  /* Restore MiscOutReg */
  outb(0x3C2,tmp);
}


