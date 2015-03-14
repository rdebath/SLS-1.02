/*
 * $Header: /home/x_cvs/mit/server/ddx/x386/vga256/drivers/tvga8900/driver.c,v 1.11 1992/09/29 10:33:25 dawes Exp $
 * Copyright 1992 by Alan Hourihane, Wigan, England.
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Alan Hourihane not be used in
 * advertising or publicity pertaining to distribution of the software without
 * specific, written prior permission.  Alan Hourihane makes no representations
 * about the suitability of this software for any purpose.  It is provided
 * "as is" without express or implied warranty.
 *
 * ALAN HOURIHANE DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL ALAN HOURIHANE BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Alan Hourihane, alanh@logitek.co.uk, version 0.1beta
 * 	    David Wexelblat, added ClockSelect logic. version 0.2.
 *	    Alan Hourihane, tweaked Init code (5 reg hack). version 0.2.1.
 *	    Alan Hourihane, removed ugly tweak code. version 0.3
 *          		    changed vgaHW.c to accomodate changes.
 * 	    Alan Hourihane, fix Restore called incorrectly. version 0.4
 *
 *	    Alan Hourihane, sent to x386beta team, version 1.0
 *
 *	    David Wexelblat, edit for comments.  Support 8900C only, dual
 *			     bank mode.  version 2.0
 *
 *	    Alan Hourihane, move vgaHW.c changes here for now. version 2.1
 *	    David Wexelblat, fix bank restoration. version 2.2
 *	    David Wexelblat, back to single bank mode.  version 2.3
 *	    Alan Hourihane, fix monochrome text restoration. version 2.4
 *
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
	vgaHWRec std;          		/* std IBM VGA register 	*/
	unsigned char ConfPort;		/* For memory selection 	*/
	unsigned char OldMode2;		/* To enable memory banks 	*/
	unsigned char NewMode2;		/* For 3rd clock select 	*/
	unsigned char NewMode1;		/* For write bank select 	*/
	unsigned char CRTCModuleTest;	/* For interlace mode 		*/
} vgaTVGA8900Rec, *vgaTVGA8900Ptr;

static void TVGA8900ClockSelect();
static char *TVGA8900Ident();
static Bool TVGA8900Probe();
static void TVGA8900EnterLeave();
static void TVGA8900Init();
static void *TVGA8900Save();
static void TVGA8900Restore();
static void TVGA8900Adjust();
#ifdef MONOVGA
extern void NoopDDA();
#else
extern void TVGA8900SetRead();
extern void TVGA8900SetWrite();
extern void TVGA8900SetReadWrite();
#endif

vgaVideoChipRec TVGA8900 = {
  TVGA8900Probe,
  TVGA8900Ident,
  TVGA8900EnterLeave,
  TVGA8900Init,
  TVGA8900Save,
  TVGA8900Restore,
  TVGA8900Adjust,
#ifdef MONOVGA
  NoopDDA,
  NoopDDA,
  NoopDDA,
#else
  TVGA8900SetRead,
  TVGA8900SetWrite,
  TVGA8900SetReadWrite,
#endif
  0x10000,
  0x10000,
  16,
  0xffff,
  0x00000, 0x10000,
  0x00000, 0x10000,
  FALSE                                /* Uses a single bank */
};

#define new ((vgaTVGA8900Ptr)vgaNewVideoState)

/*
 * TVGA8900Ident --
 */
char *
TVGA8900Ident()
{
  	return("tvga8900");
}

/*
 * TVGA8900ClockSelect --
 * 	select one of the possible clocks ...
 */

static void
TVGA8900ClockSelect(no)
	int no;
{
	unsigned char temp;

	temp = inb(0x3CC);
	outb(0x3C2, (temp & 0xF3) | ((no << 2) & 0x0C));

	/* 
	 * High bit of clock select is bit 0 of New Mode Control Register 2.
	 */
	outw(0x3C4, 0x000B);
	inb(0x3C5);
	outb(0x3C4, 0x0D);
	outb(0x3C5, (no & 0x04) >> 2);
}

/* 
 * TVGA8900Probe --
 * 	check up whether a Trident 8900 based board is installed
 */

static Bool
TVGA8900Probe()
{
  	int numClocks;
  	unsigned char temp;

  	if (vga256InfoRec.chipset)
    	{
		/*
		 * If chipset from Xconfig matches...
		 */
      		if (strcmp(vga256InfoRec.chipset, TVGA8900Ident()))
			return (FALSE);
      		else
			TVGA8900EnterLeave(ENTER);
    	}
  	else
    	{
      		unsigned char origVal, newVal;
	
      		TVGA8900EnterLeave(ENTER);

      		/* 
       		 * Check first that we have a Trident card and then make sure
       		 * its an 8900C.
       		 */
      		outb(0x3C4, 0x0E);
      		origVal = inb(0x3C5);
      		outb(0x3C5, 0x00);
      		newVal = inb(0x3C5) & 0x0F;
      		outb(0x3C5, (origVal ^ 0x02));

		/* 
		 * Is it a Trident card ?? 
		 */
      		if (newVal != 2)
		{
			/*
			 * Nope, so quit
			 */
	  		TVGA8900EnterLeave(LEAVE);
	  		return(FALSE);
		}

      		/* 
		 * We've found a Trident card, now check the version.
		 */
      		outw(0x3C4, 0x000B);
      		temp = inb(0x3C5);
		ErrorF("Trident chipset version: 0x%02x\n", temp);
#ifdef MONOVGA
		switch (temp)
		{
		case 0x03:		/* TVGA8900B */
		case 0x04:		/* TVGA8900C */
		case 0x13:		/* TVGA8900C */
		case 0x23:		/* TVGA9000 */
			break;
		default:
	  		TVGA8900EnterLeave(LEAVE);
	  		return(FALSE);
		}
#else
      		if ((temp != 0x04) && (temp != 0x13))				
		{
			ErrorF("Trident driver only supports TVGA8900C\n");
	  		TVGA8900EnterLeave(LEAVE);
	  		return(FALSE);
		}
#endif

      		/* 
		 * We've got an 8900C.. Yeah ! (colour)
		 * We've got an 8900 or 9000.. Yeah ! (mono)
		 * The previous inb(0x3C5) put us in New Mode.  Go back
		 * to Old Mode for now.
		 */
      		outb(0x3C4, 0x0B);
      		inb(0x3C5);	

    	}
 
	/*
	 * TVGA8900C supports 8 clocks.
	 */
    	numClocks = 8;

 	/* 
	 * How much Video Ram has that 8900C got? 
	 */
    	if (!vga256InfoRec.videoRam)
    	{
		unsigned char temp;

		outb(vgaIOBase + 4, 0x1F); 
		temp = inb(vgaIOBase + 5);

		switch (temp & 0x03) 
		{
		case 0: 
			vga256InfoRec.videoRam = 256; 
			break;
		case 1: 
			vga256InfoRec.videoRam = 512; 
			break;
		case 2: 
			vga256InfoRec.videoRam = 768; 
			break;
		case 3: 
			vga256InfoRec.videoRam = 1024; 
			break;
		}
     	}

	/*
	 * If clocks are not specified in Xconfig file, probe for them
	 */
    	if (!vga256InfoRec.clocks) 
		vgaGetClocks(numClocks, TVGA8900ClockSelect); 

	/*
	 * Identify chipset as TVGA8900C
	 */
    	vga256InfoRec.chipset = TVGA8900Ident();

    	return(TRUE);
}

/*
 * TVGA8900EnterLeave --
 * 	enable/disable io-mapping
 */

static void
TVGA8900EnterLeave(enter)
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
  	else
    	{
#ifdef HAS_USL_VTS
      		ioctl(x386Info.consoleFd, KDDISABIO, 0);
#endif
    	}
}

/*
 * TVGA8900Restore --
 *      restore a video mode
 */

static void
TVGA8900Restore(restore)
     	vgaTVGA8900Ptr restore;
{
	unsigned char temp;
	int i;

#ifndef MONOVGA
	/*
	 * Go to Old Mode.
	 */
	outw(0x3C4, 0x000B);
	
	/*
	 * Restore Old Mode Control Register 2.
	 */
	outw(0x3C4, ((restore->OldMode2) << 8) | 0x0D); 
#endif

	/*
	 * Now go to New Mode
	 */
	outb(0x3C4, 0x0B);
	inb(0x3C5);

	/*
	 * Unlock Configuration Port Register, then restore:
	 *	Configuration Port Register 1
	 *	New Mode Control Register 2
	 *	New Mode Control Register 1
	 *	CRTC Module Testing Register
	 */
#ifndef MONOVGA
	outw(0x3C4, 0x820E);
	outw(0x3C4, ((restore->ConfPort) << 8) | 0x0C);
#endif
	outw(0x3C4, ((restore->NewMode2) << 8) | 0x0D);
#ifndef MONOVGA
	outw(0x3C4, ((restore->NewMode1 ^ 0x02) << 8) | 0x0E);
#endif
	outw(vgaIOBase + 4, ((restore->CRTCModuleTest) << 8) | 0x1E);

	/*
	 * Now restore generic VGA Registers
	 */

	vgaHWRestore(restore);
		
	/*
	 * Now reenable the timing sequencer
	 */
	outw(0x3C4, 0x0300);
}

/*
 * TVGA8900Save --
 *      save the current video mode
 */
static void *
TVGA8900Save(save)
     	vgaTVGA8900Ptr save;
{
	unsigned char temp;
	int i;
	
#ifndef MONOVGA
	/*
	 * Get current bank
	 */
	outb(0x3C4, 0x0e); temp = inb(0x3C5);
#endif

	/*
	 * Save generic VGA registers
	 */

  	save = (vgaTVGA8900Ptr)vgaHWSave(save, sizeof(vgaTVGA8900Rec));

	/*
	 * Go to Old Mode.
	 */
	outw(0x3C4, 0x000B);

	/*
	 * Save Old Mode Control Register 2.
	 */
	outb(0x3C4, 0x0D);
	save->OldMode2 = inb(0x3C5); 
	
	/*
	 * Now go to New Mode
	 */
	outb(0x3C4, 0x0B);
	inb(0x3C5);

	/*
	 * Unlock Configuration Port Register, then save:
	 *	Configuration Port Register 1
	 *	New Mode Control Register 2
	 *	New Mode Control Register 1
	 *	CRTC Module Testing Register
	 */
#ifndef MONOVGA
	outw(0x3C4, 0x820E);
	outb(0x3C4, 0x0C); save->ConfPort = inb(0x3C5);
	save->NewMode1 = temp;
#endif
	outb(0x3C4, 0x0D); save->NewMode2 = inb(0x3C5);
	outb(vgaIOBase + 4, 0x1E); save->CRTCModuleTest = inb(vgaIOBase + 5);

  	return ((void *) save);
}

/*
 * TVGA8900Init --
 *      Handle the initialization, etc. of a screen.
 */

static void
TVGA8900Init(mode)
    	DisplayModePtr mode;
{
	/*
	 * Initialize generic VGA Registers
	 */
	vgaHWInit(mode, sizeof(vgaTVGA8900Rec));

	/*
	 * Now do Trident-specific stuff.
	 */
	new->std.CRTC[19] = vga256InfoRec.virtualX >> 
		(mode->Flags & V_INTERLACE ? 3 : 4);
#ifndef MONOVGA
	new->std.CRTC[20] = 0x40;
	new->std.CRTC[23] = 0xA3;
#endif

	new->CRTCModuleTest = (mode->Flags & V_INTERLACE ? 0x84 : 0x80); 

#ifndef MONOVGA
	new->OldMode2 = 0x10;
	new->NewMode1 = 0x02;
	if (vga256InfoRec.videoRam > 512)
		new->ConfPort = 0xEC;
	else
		new->ConfPort = 0xCC;
#endif
	new->NewMode2 = (int)(new->std.NoClock & 0x04) >> 2;
}

/*
 * TVGA8900Adjust --
 *      adjust the current video frame to display the mousecursor
 */

static void 
TVGA8900Adjust(x, y)
	int x, y;
{
	int base = (y * vga256InfoRec.virtualX + x) >> 3;

  	outw(vgaIOBase + 4, (base & 0x00FF00) | 0x0C);
	outw(vgaIOBase + 4, ((base & 0x00FF) << 8) | 0x0D);
	outb(vgaIOBase + 4, 0x1E);
	outb(vgaIOBase + 5, (inb(vgaIOBase + 5) & 0xDF) | 
				(base > 0xFFFF ? 0x20 : 0x00));
}


