/*
 * $Header: /home/x_cvs/mit/server/ddx/x386/vga256/vga/vga.c,v 1.25 1992/09/26 08:03:04 dawes Exp $
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
 * $Header: /proj/X11/mit/server/ddx/x386/vga/RCS/vga.c,v 1.2 1991/06/27 00:02:49 root Exp $
 */


#include "X.h"
#include "input.h"
#include "scrnintstr.h"
#include "pixmapstr.h"
#include "regionstr.h"
#include "mipointer.h"
#include "cursorstr.h"

#include "compiler.h"

#include "x386.h"
#include "x386Priv.h"
#include "x386OSD.h"
#include "vga.h"
#ifndef MONOVGA
#include "cfb.h"
#endif

#ifdef __386BSD__
#include <sys/types.h>
#endif
#if defined(__386BSD__) || defined(MACH386)
#include <sys/mman.h>
#endif
#include "gcstruct.h"

#ifndef MONOVGA
unsigned long useSpeedUp = 0;
extern void stdcfbFillBoxSolid();
extern void speedupcfbFillBoxSolid();
extern void (*ourcfbFillBoxSolid)();
extern void stdcfbFillRectSolidCopy();
extern void speedupcfbFillRectSolidCopy();
extern void cfbFillRectSolidCopy();
extern void (*ourcfbFillRectSolidCopy)();
extern int stdcfbDoBitbltCopy();
extern int speedupcfbDoBitbltCopy();
extern int (*ourcfbDoBitbltCopy)();
extern void stdcfbLineSS(), speedupcfbLineSS(), (*ourcfbLineSS)();
extern void stdcfbSegmentSS(), speedupcfbSegmentSS(), (*ourcfbSegmentSS)();
extern void stdcfbTEGlyphBlt8(), speedupcfbTEGlyphBlt8();
extern void (*ourcfbTEGlyphBlt8)();
extern void speedupcfb8FillRectOpaqueStippled32();
extern void stdcfb8FillRectOpaqueStippled32();
extern void (*ourcfb8FillRectOpaqueStippled32)();
extern void speedupcfb8FillRectTransparentStippled32();
extern void stdcfb8FillRectTransparentStippled32();
extern void (*ourcfb8FillRectTransparentStippled32)();
extern void vgaBitBlt(), OneBankvgaBitBlt();
extern void (*ourvgaBitBlt)();

extern GCOps cfbTEOps1Rect, cfbTEOps, cfbNonTEOps1Rect, cfbNonTEOps;
#endif

extern Bool x386Exiting, x386Resetting;
extern void NoopDDA();

ScrnInfoRec vga256InfoRec = {
  FALSE,		/* Bool configured */
  -1,			/* int index */
  vgaProbe,		/* Bool (* Probe)() */
  vgaScreenInit,	/* Bool (* Init)() */
  vgaEnterLeaveVT,	/* void (* EnterLeaveVT)() */
  NoopDDA,		/* void (* EnterLeaveMonitor)() */
  NoopDDA,		/* void (* EnterLeaveCursor)() */
  vgaAdjustFrame,	/* void (* AdjustFrame)() */
  vgaSwitchMode,	/* void (* SwitchMode)() */
  vgaPrintIdent,        /* void (* PrintIdent)() */
#ifdef MONOVGA
  1,			/* int depth */
  1,			/* int bitsPerPixel */
  StaticGray,		/* int defaultVisual */
#else
  8,			/* int depth */
  8,			/* int bitsPerPixel */
  PseudoColor,		/* int defaultVisual */
#endif
  -1, -1,		/* int virtualX,virtualY */
  -1, -1, -1, -1,	/* int frameX0, frameY0, frameX1, frameY1 */
  NULL,			/* char *vendor */
  NULL,			/* char *chipset */
  0,			/* int clocks */
  {0, },		/* int clock[MAXCLOCKS] */
  0,			/* int videoRam */
  240, 180,		/* int width, height */
  0,			/* unsigned long speedup */
  NULL,			/* DisplayModePtr modes */
};


pointer vgaOrigVideoState = NULL;
pointer vgaNewVideoState = NULL;
pointer vgaBase = NULL;
#ifdef HAS_USL_VTS
#ifndef linux
struct kd_memloc vgaDSC;
#endif /* !linux */
#endif

void (* vgaEnterLeaveFunc)();
void (* vgaInitFunc)();
void * (* vgaSaveFunc)();
void (* vgaRestoreFunc)();
void (* vgaAdjustFunc)();
#ifndef MONOVGA
void (* vgaSetReadFunc)();
void (* vgaSetWriteFunc)();
void (* vgaSetReadWriteFunc)();
int vgaMapSize;
int vgaSegmentSize;
int vgaSegmentShift;
int vgaSegmentMask;
void *vgaReadBottom;
void *vgaReadTop;
void *vgaWriteBottom;
void *vgaWriteTop =    (pointer)&writeseg; /* dummy for linking */
Bool vgaReadFlag;
Bool vgaWriteFlag;
Bool vgaUse2Banks;
#endif

int vgaIOBase;

static Bool saveFuncs = FALSE;
static void (* saveInitFunc)();
static void * (* saveSaveFunc)();
static void (* saveRestoreFunc)();
static void (* saveAdjustFunc)();
#ifndef MONOVGA
static void (* saveSetReadFunc)();
static void (* saveSetWriteFunc)();
static void (* saveSetReadWriteFunc)();
#endif

extern miPointerScreenFuncRec x386PointerScreenFuncs;

#define Drivers vgaDrivers

extern vgaVideoChipPtr Drivers[];

/*
 * vgaPrintIdent
 *     Prints out identifying strings for drivers included in the server
 */
void
vgaPrintIdent()
{
  int            i;

#ifdef MONOVGA
  ErrorF("  VGA2 (monochrome VGA):\n      ");
#else
  ErrorF("  VGA256 (256 colour SVGA):\n      ");
#endif
  for (i=0; Drivers[i]; i++)
  {
    if (i)
    {
      ErrorF(",");
      if (i > 6)
        ErrorF("\n      ");
      else
        ErrorF(" ");
    }
    ErrorF("%s",(Drivers[i]->ChipIdent)());
  }
  ErrorF("\n");
}


/*
 * vgaProbe --
 *     probe and initialize the hardware driver
 */
Bool
vgaProbe()
{
  int            i, j;
  DisplayModePtr pMode, pEnd;
  int            maxX, maxY;

#ifdef MACH386
  /* Enable IO privledges. */
  {
    int ioplfd;
    if ((ioplfd = open ("/dev/iopl", 0)) < 0)
      FatalError ("failed to gain IO privledges.");
  }

#endif/* MACH386 */

  for (i=0; Drivers[i]; i++)

    if ((Drivers[i]->ChipProbe)())
      {
#ifdef MONOVGA
	ErrorF("VGA2: %s (mem: %dk (using 64k) numclocks: %d)",
#else
	ErrorF("VGA256: %s (mem: %dk numclocks: %d)",
#endif
	       vga256InfoRec.chipset,
	       vga256InfoRec.videoRam,
	       vga256InfoRec.clocks);
	for (j=0; j < vga256InfoRec.clocks; j++)
	  {
	    if ((j % 16) == 0)
#ifdef MONOVGA
	      ErrorF("\nVGA2:   clocks:");
#else
	      ErrorF("\nVGA256:   clocks:");
#endif
	    ErrorF(" %2d", vga256InfoRec.clock[j]);
	  }
	ErrorF("\n");

	vgaEnterLeaveFunc = Drivers[i]->ChipEnterLeave;
	vgaInitFunc = Drivers[i]->ChipInit;
	vgaSaveFunc = Drivers[i]->ChipSave;
	vgaRestoreFunc = Drivers[i]->ChipRestore;
	vgaAdjustFunc = Drivers[i]->ChipAdjust;
#ifndef MONOVGA
	vgaSetReadFunc = Drivers[i]->ChipSetRead;
	vgaSetWriteFunc = Drivers[i]->ChipSetWrite;
	vgaSetReadWriteFunc = Drivers[i]->ChipSetReadWrite;
	vgaMapSize = Drivers[i]->ChipMapSize;
	vgaSegmentSize = Drivers[i]->ChipSegmentSize;
	vgaSegmentShift = Drivers[i]->ChipSegmentShift;
	vgaSegmentMask = Drivers[i]->ChipSegmentMask;
	vgaReadBottom = (pointer)Drivers[i]->ChipReadBottom;
	vgaReadTop = (pointer)Drivers[i]->ChipReadTop;
	vgaWriteBottom = (pointer)Drivers[i]->ChipWriteBottom;
	vgaWriteTop = (pointer)Drivers[i]->ChipWriteTop;
	vgaUse2Banks = Drivers[i]->ChipUse2Banks;
#endif

#ifdef MONOVGA
	if (vga256InfoRec.virtualX > 0 &&
	    vga256InfoRec.virtualX * vga256InfoRec.virtualY >
	    64 * 1024 * 8)
	  {
	    ErrorF("VGA2: Too little memory for virtual resolution\n");
	    return(FALSE);
	  }
#else
	if (vga256InfoRec.virtualX > 0 &&
	    vga256InfoRec.virtualX * vga256InfoRec.virtualY >
	    vga256InfoRec.videoRam * 1024)
	  {
	    ErrorF("VGA256: Too little memory for virtual resolution\n");
	    return(FALSE);
	  }
#endif


        maxX = maxY = -1;
	pMode = pEnd = vga256InfoRec.modes;
	do {
	  x386LookupMode(pMode, &vga256InfoRec);
#ifdef MONOVGA
	  if (pMode->HDisplay * pMode->VDisplay > 64 * 1024 * 8)
	    {
	      ErrorF("VGA2: Too little memory for all resolutions\n");
	      return(FALSE);
	    }
#else
	  if (pMode->HDisplay * pMode->VDisplay > vga256InfoRec.videoRam*1024)
	    {
	      ErrorF("VGA256: Too little memory for all resolutions\n");
	      return(FALSE);
	    }
#endif
          maxX = max(pMode->HDisplay, maxX);
          maxY = max(pMode->VDisplay, maxY);
	  pMode = pMode->next;
	}
	while (pMode != pEnd);

        vga256InfoRec.virtualX = max(maxX, vga256InfoRec.virtualX);
        vga256InfoRec.virtualY = max(maxY, vga256InfoRec.virtualY);

#ifdef MONOVGA
	if (vga256InfoRec.virtualX % 32)
	  {
	    vga256InfoRec.virtualX -= vga256InfoRec.virtualX % 32;
            if (vga256InfoRec.virtualX < maxX)
              {
                ErrorF(
                 "VGA2: Rounded down virtual width is too small for all modes");
                return(FALSE);
              }
	    ErrorF(
	     "VGA2: Virtual width rounded down to a multiple of 32 (%d)\n",
	     vga256InfoRec.virtualX);
	  }
#else
	if (vga256InfoRec.virtualX % 4)
	  {
	    vga256InfoRec.virtualX -= vga256InfoRec.virtualX % 4;
            if (vga256InfoRec.virtualX < maxX)
              {
                ErrorF(
                 "VGA256: Rounded down virtual width is too small for all modes");
                return(FALSE);
              }
	    ErrorF(
	     "VGA256: Virtual width rounded down to a multiple of four (%d)\n",
	     vga256InfoRec.virtualX);
	  }
#endif

#ifdef MONOVGA
	if ( vga256InfoRec.virtualX * vga256InfoRec.virtualY > 64 * 1024 * 8)
	  {
	    ErrorF("VGA2: Too little memory to accomodate all modes\n");
	    return(FALSE);
	  }
#else
	if ( vga256InfoRec.virtualX * vga256InfoRec.virtualY >
	    vga256InfoRec.videoRam * 1024)
	  {
	    ErrorF("VGA256: Too little memory to accomodate all modes\n");
	    return(FALSE);
	  }
#endif

#ifndef MONOVGA
	if (vga256InfoRec.speedup && strcmp(vga256InfoRec.chipset,"et4000"))
	  {
	    ErrorF("VGA256: SpeedUp code selection modified because chipset != et4000\n");
	    vga256InfoRec.speedup &= SPEEDUP_ANYCHIPSET;
	  }

	if (vga256InfoRec.speedup && vga256InfoRec.virtualX != 1024)
	  {
	    ErrorF("VGA256: SpeedUp code selection modified because virtualX != 1024\n");
	    vga256InfoRec.speedup &= SPEEDUP_ANYWIDTH;
	  }

        useSpeedUp = vga256InfoRec.speedup;
        if (useSpeedUp)
          ErrorF("VGA256: SpeedUp mode selected (Flags=0x%x)\n",useSpeedUp);

	if (useSpeedUp & SPEEDUP_FILLBOX)
          ourcfbFillBoxSolid = speedupcfbFillBoxSolid;
	else
          ourcfbFillBoxSolid = stdcfbFillBoxSolid;

	if (useSpeedUp & SPEEDUP_FILLRECT)
          ourcfbFillRectSolidCopy = speedupcfbFillRectSolidCopy;
	else
          ourcfbFillRectSolidCopy = stdcfbFillRectSolidCopy;

	if (useSpeedUp & SPEEDUP_BITBLT)
          ourcfbDoBitbltCopy = speedupcfbDoBitbltCopy;
        else
          ourcfbDoBitbltCopy = stdcfbDoBitbltCopy;

	if (useSpeedUp & SPEEDUP_LINE)
	{
	  ourcfbLineSS = speedupcfbLineSS;
	  ourcfbSegmentSS = speedupcfbSegmentSS;
	  cfbTEOps1Rect.Polylines = speedupcfbLineSS;
	  cfbTEOps1Rect.PolySegment = speedupcfbSegmentSS;
	  cfbTEOps.Polylines = speedupcfbLineSS;
	  cfbTEOps.PolySegment = speedupcfbSegmentSS;
	  cfbNonTEOps1Rect.Polylines = speedupcfbLineSS;
	  cfbNonTEOps1Rect.PolySegment = speedupcfbSegmentSS;
	  cfbNonTEOps.Polylines = speedupcfbLineSS;
	  cfbNonTEOps.PolySegment = speedupcfbSegmentSS;
	}
	else
	{
	  ourcfbLineSS = stdcfbLineSS;
	  ourcfbSegmentSS = stdcfbSegmentSS;
	  /* Note cfbTE... default to std... */
	}

	if (useSpeedUp & SPEEDUP_TEGBLT8)
	{
	  ourcfbTEGlyphBlt8 = speedupcfbTEGlyphBlt8;
	  cfbTEOps1Rect.ImageGlyphBlt = speedupcfbTEGlyphBlt8;
	  cfbTEOps.ImageGlyphBlt = speedupcfbTEGlyphBlt8;
	}
	else
	{
	  ourcfbTEGlyphBlt8 = stdcfbTEGlyphBlt8;
	}

	if (useSpeedUp & SPEEDUP_RECTSTIP)
	{
	  ourcfb8FillRectOpaqueStippled32 = 
	    speedupcfb8FillRectOpaqueStippled32;
	  ourcfb8FillRectTransparentStippled32 = 
	    speedupcfb8FillRectTransparentStippled32;
	}
	else
	{
	  ourcfb8FillRectOpaqueStippled32 = stdcfb8FillRectOpaqueStippled32;
	  ourcfb8FillRectTransparentStippled32 = 
	    stdcfb8FillRectTransparentStippled32;
	}

	if (!vgaUse2Banks)
	{
	  ourvgaBitBlt = OneBankvgaBitBlt;
	}
	else
	{
	  ourvgaBitBlt = vgaBitBlt;
	}
#endif

	return TRUE;
      }
  
  return FALSE;
}


/*
 * vgaScreenInit --
 *      Attempt to find and initialize a VGA framebuffer
 *      Most of the elements of the ScreenRec are filled in.  The
 *      video is enabled for the frame buffer...
 */

Bool
vgaScreenInit (index, pScreen, argc, argv)
    int            index;        /* The index of pScreen in the ScreenInfo */
    ScreenPtr      pScreen;      /* The Screen to initialize */
    int            argc;         /* The number of the Server's arguments. */
    char           **argv;       /* The arguments themselves. Don't change! */
{
  if (vgaBase == NULL) {
#ifdef SCO
    /*
     * To map the video-memory, we use the MAPCONS ioctl. First the screen
     * must be in graphics mode, hence the SW_CG640x350 (older SVR3.2 have
     * no VGA support, thus a EGA mode here !!!
     */
    if (ioctl(x386Info.consoleFd, SW_CG640x350, 0) != 0 ||
         (int)(vgaBase=(pointer)ioctl(x386Info.consoleFd, MAPCONS, 0)) == -1 )
      FatalError("failed to map the video memory\n");
#else /* SCO */
#ifdef __386BSD__
    vgaBase = (pointer) mmap(0, 0x20000, PROT_READ|PROT_WRITE, MAP_FILE,
			     x386Info.screenFd, 0);
    if (vgaBase == -1)
      FatalError("Could not memory map /dev/vga (the video memory)\n");
#else /* __386BSD__ */
    vgaBase = (pointer)(((unsigned int)xalloc(0x11000) & ~0xFFF) + 0x1000);
#ifdef MACH386
#ifndef FAKEIT
    if (mmap (vgaBase, 0x10000, PROT_READ|PROT_WRITE, MAP_SHARED,
              x386Info.consoleFd, 0) < 0)
      FatalError ("failed to map frame buffer.");
#endif /* FAKEIT */
#else /* MACH386 */
#ifdef linux
    {
	    int fd;

	    if ((fd = open("/dev/mem", O_RDWR)) < 0)
		    FatalError("failed to open /dev/mem\n");
	    vgaBase = (pointer)mmap((caddr_t)vgaBase, 0x10000,
					PROT_READ|PROT_WRITE,
					MAP_SHARED|MAP_FIXED, fd, 0xA0000);
	    close(fd);
	    if ((long)vgaBase < 0)
		    FatalError("failed to mmap vga mem\n");
    }
#else /* !linux */
    vgaDSC.vaddr    = (char*)vgaBase;
    vgaDSC.physaddr = (char*)0xA0000;
    vgaDSC.length   = 0x10000;
    vgaDSC.ioflg   = 1;
    if (ioctl(x386Info.consoleFd, KDMAPDISP, &vgaDSC) < 0)
      FatalError("failed to map the video memory\n");
#endif /* linux */
#endif /* MACH386 */
#endif /* __386BSD__ */
#endif /* SCO */
#ifndef MONOVGA
    vgaReadBottom  = (void *)((unsigned int)vgaReadBottom
			      + (unsigned int)vgaBase); 
    vgaReadTop     = (void *)((unsigned int)vgaReadTop
			      + (unsigned int)vgaBase); 
    vgaWriteBottom = (void *)((unsigned int)vgaWriteBottom
			      + (unsigned int)vgaBase); 
    vgaWriteTop    = (void *)((unsigned int)vgaWriteTop
			      + (unsigned int)vgaBase); 
#endif
  }

  (vgaInitFunc)(vga256InfoRec.modes);

  /*
   * This function gets called while in graphics mode during a server
   * reset, and this causes the original video state to be corrupted.
   * So, only initialise vgaOrigVideoState if it hasn't previously been done
   * DHD Dec 1991.
   */
  if (!vgaOrigVideoState)
    vgaOrigVideoState = (pointer)(vgaSaveFunc)(vgaOrigVideoState);
  (vgaRestoreFunc)(vgaNewVideoState);
  (vgaAdjustFunc)(vga256InfoRec.frameX0, vga256InfoRec.frameY0);

  /*
   * Inititalize the dragon to color display
   */
#ifdef MONOVGA
  if (!mfbScreenInit(pScreen,
		     (pointer) VGABASE,
		     vga256InfoRec.virtualX,
		     vga256InfoRec.virtualY,
		     75, 75,
		     vga256InfoRec.virtualX))
#else
  if (!cfbScreenInit(pScreen,
		     (pointer) VGABASE,
		     vga256InfoRec.virtualX,
		     vga256InfoRec.virtualY,
		     75, 75,
		     vga256InfoRec.virtualX))
#endif
    return(FALSE);

    pScreen->CloseScreen = vgaCloseScreen;
    pScreen->SaveScreen = vgaSaveScreen;
#ifdef MONOVGA
    pScreen->whitePixel = 1;
    pScreen->blackPixel = 0;
#else
    pScreen->InstallColormap = vgaInstallColormap;
    pScreen->UninstallColormap = vgaUninstallColormap;
    pScreen->ListInstalledColormaps = vgaListInstalledColormaps;
    pScreen->StoreColors = vgaStoreColors;
#endif
  
  miDCInitialize (pScreen, &x386PointerScreenFuncs);
#ifdef MONOVGA
  return (mfbCreateDefColormap(pScreen));
#else
  return (cfbCreateDefColormap(pScreen));
#endif

}



static void saveDummy() {}

/*
 * vgaEnterLeaveVT -- 
 *      grab/ungrab the current VT completely.
 */

void
vgaEnterLeaveVT(enter)
     Bool enter;
{
  pointer p;
  BoxRec  pixBox;
  RegionRec pixReg;
  DDXPointRec pixPt;
  PixmapPtr   pspix;
  static PixmapPtr ppix = NULL;
  ScreenPtr   pScreen = screenInfo.screens[0];

  if (!x386Resetting && !x386Exiting)
    {
      pixBox.x1 = 0; pixBox.x2 = pScreen->width;
      pixBox.y1 = 0; pixBox.y2 = pScreen->height;
      pixPt.x = 0; pixPt.y = 0;
      (pScreen->RegionInit)(&pixReg, &pixBox, 1);
      pspix = (PixmapPtr)pScreen->devPrivate;
    }

  if (enter)
    {
      vgaInitFunc = saveInitFunc;
      vgaSaveFunc = saveSaveFunc;
      vgaRestoreFunc = saveRestoreFunc;
      vgaAdjustFunc = saveAdjustFunc;
#ifndef MONOVGA
      vgaSetReadFunc = saveSetReadFunc;
      vgaSetWriteFunc = saveSetWriteFunc;
      vgaSetReadWriteFunc = saveSetReadWriteFunc;
#endif
      
#if defined(HAS_USL_VTS) && !defined(SCO) && !defined(linux)
      ioctl(x386Info.consoleFd, KDMAPDISP, &vgaDSC);
#endif
      (vgaEnterLeaveFunc)(ENTER);
      vgaOrigVideoState = (pointer)(vgaSaveFunc)(vgaOrigVideoState);
      (vgaRestoreFunc)(vgaNewVideoState);

      /*
       * point pspix back to VGABASE, and copy the dummy buffer to the
       * real screen.
       */
      if (!x386Resetting)
        if ((pointer)pspix->devPrivate.ptr != (pointer)VGABASE && ppix)
        {
	  pspix->devPrivate.ptr = (pointer)VGABASE;
#ifdef MONOVGA
	  mfbDoBitblt(&ppix->drawable, &pspix->drawable, GXcopy, &pixReg,
                      &pixPt, 0xFF);
#else
	  cfbDoBitblt(&ppix->drawable, &pspix->drawable, GXcopy, &pixReg,
                      &pixPt, 0xFF);
#endif
	  (pScreen->DestroyPixmap)(ppix);
        }


      saveFuncs = FALSE;
    }
  else
    {
      /*
       * Create a dummy pixmap to write to while VT is switched out.
       * Copy the screen to that pixmap
       */
      if (!x386Exiting)
      {
        ppix = (pScreen->CreatePixmap)(pScreen, pScreen->width,
                                        pScreen->height, pScreen->rootDepth);
        if (ppix)
        {
#ifdef MONOVGA
	  mfbDoBitblt(&pspix->drawable, &ppix->drawable, GXcopy, &pixReg,
                      &pixPt, 0xFF);
#else
	  cfbDoBitblt(&pspix->drawable, &ppix->drawable, GXcopy, &pixReg,
                      &pixPt, 0xFF);
#endif
	  pspix->devPrivate.ptr = ppix->devPrivate.ptr;
        }
      }
      (vgaSaveFunc)(vgaNewVideoState);
      /*
       * We come here in many cases, but one is special: When the server aborts
       * abnormaly. Therefore there MUST be a check whether vgaOrigVideoState
       * is valid or not.
       */
      if (vgaOrigVideoState)
	(vgaRestoreFunc)(vgaOrigVideoState);

      (vgaEnterLeaveFunc)(LEAVE);
#if defined(HAS_USL_VTS) && !defined(SCO) && !defined(linux)
      ioctl(x386Info.consoleFd, KDUNMAPDISP, 0);
#endif

      saveInitFunc = vgaInitFunc;
      saveSaveFunc = vgaSaveFunc;
      saveRestoreFunc = vgaRestoreFunc;
      saveAdjustFunc = vgaAdjustFunc;
#ifndef MONOVGA
      saveSetReadFunc = vgaSetReadFunc;
      saveSetWriteFunc = vgaSetWriteFunc;
      saveSetReadWriteFunc = vgaSetReadWriteFunc;
#endif
      
      vgaInitFunc = saveDummy;
      vgaSaveFunc = (void * (*)())saveDummy;
      vgaRestoreFunc = saveDummy;
      vgaAdjustFunc = saveDummy;
#ifndef MONOVGA
      vgaSetReadFunc = saveDummy;
      vgaSetWriteFunc = saveDummy;
      vgaSetReadWriteFunc = saveDummy;
#endif
      
      saveFuncs = TRUE;
    }
}



/*
 * vgaCloseScreen --
 *      called to ensure video is enabled when server exits.
 */

Bool
vgaCloseScreen()
{
  /*
   * Hmm... The server may shut down even if it is not running on the
   * current vt. Let's catch this case here.
   */
  x386Exiting = TRUE;
  if (x386VTSema) vgaEnterLeaveVT(LEAVE);
  return(TRUE);
}



/*
 * vgaAdjustFrame --
 *      Set a new viewport
 */
void
vgaAdjustFrame(x, y)
     int x, y;
{
  (vgaAdjustFunc)(x, y);
}


/*
 * vgaSwitchMode --
 *     Set a new display mode
 */
void
vgaSwitchMode(mode)
     DisplayModePtr mode;
{
  (vgaInitFunc)(mode);
  (vgaRestoreFunc)(vgaNewVideoState);
}
