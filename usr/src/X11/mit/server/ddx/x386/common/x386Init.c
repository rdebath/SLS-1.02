/*
 * $Header: /home/x_cvs/mit/server/ddx/x386/common/x386Init.c,v 1.32 1992/09/29 15:26:47 dawes Exp $
 * $XConsortium: x386Init.c,v 1.2 91/08/20 15:39:58 gildea Exp $
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
 */

#include "X.h"
#include "Xmd.h"
#include "input.h"
#include "servermd.h"
#include "scrnintstr.h"
#include "site.h"

#include "compiler.h"

#include "x386Procs.h"
#include "x386OSD.h"
#include "x386Version.h"

#ifndef PATH_MAX
#include <sys/param.h>
#ifdef MAXPATHLEN
#define PATH_MAX MAXPATHLEN
#else
#define PATH_MAX 1024
#endif
#endif

#define X386_COLOUR_SERVER_NAME "X386"
#define X386_MONO_SERVER_NAME "X386mono"

#ifdef XTESTEXT1
#include "atKeynames.h"
extern int xtest_command_key;
#endif /* XTESTEXT1 */

/* x386Exiting is set while the screen is shutting down (even on a reset) */
Bool x386Exiting = FALSE;
Bool x386Resetting = FALSE;
char x386ConfigFile[PATH_MAX] = "";

static void x386PrintConfig();

extern ScrnInfoRec vga2InfoRec;
extern ScrnInfoRec vga256InfoRec;

#ifdef X386MONOVGA
ScrnInfoPtr x386Screens[] =
{
        NULL,
        &vga2InfoRec
};
#else
ScrnInfoPtr x386Screens[] =
{
        &vga256InfoRec,
        NULL
};
#endif

int     x386MaxScreens = sizeof(x386Screens) / sizeof(ScrnInfoPtr);

x386InfoRec x386Info;
int         x386ScreenIndex;
int         VTnum = -1;


/*
 * InitOutput --
 *	Initialize screenInfo for all actually accessible framebuffers.
 *      That includes vt-manager setup, querying all possible devices and
 *      collecting the pixmap formats.
 */

void
InitOutput(pScreenInfo, argc, argv)
     ScreenInfo	*pScreenInfo;
     int     	argc;
     char    	**argv;
{
  int                    i, j, index, fd;
#ifdef HAS_USL_VTS
#ifndef linux
  struct vt_mode         VT;
#endif /* !linux */
#endif
  char                   vtname1[10],vtname2[10];
  static int             numFormats = 0;
  static PixmapFormatRec formats[MAXFORMATS];
  static unsigned long   generation = 0;
  int                    any_screens = 0;
   

  if (serverGeneration == 1) {

    x386PrintConfig();

#ifdef HAS_USL_VTS
#ifdef linux
    /*
     * we can only run off the console, so choose the current one.
     * /dev/tty0 is the same as /dev/console on other systems
     */
    if ((x386Info.consoleFd = open("/dev/console", O_RDWR | O_NDELAY)) < 0)
	    FatalError("Can't open /dev/console\n");
    x386Config(FALSE); /* Read Xconfig */
#else
    /*
     * setup the virtual terminal manager
     */
    if (VTnum != -1) {
      x386Info.vtno = VTnum;
    }
    else {
      if ((fd = open("/dev/console",O_WRONLY,0)) <0) 
        FatalError("Cannot open /dev/console (errno=%d)\n",errno);

      if (ioctl(fd, VT_OPENQRY, &x386Info.vtno) < 0 || x386Info.vtno == -1) 
        FatalError("Cannot find a free VT\n");

      close(fd);
    }
    ErrorF("(using VT number %d)\n\n", x386Info.vtno);

    sprintf(vtname1,"/dev/vc%02d",x386Info.vtno); /* ESIX */
    sprintf(vtname2,"/dev/vt%02d",x386Info.vtno); /* rest of the world */

    x386Config(FALSE); /* Read Xconfig */

    setpgrp();

    if ( (x386Info.consoleFd = open(vtname1, O_RDWR | O_NDELAY, 0)) < 0 &&
	 (x386Info.consoleFd = open(vtname2, O_RDWR | O_NDELAY, 0)) < 0 )
      FatalError("Cannot open %s (%s) (errno=%d)\n",vtname2, vtname1,errno);
#endif /* linux */
#else
#ifdef __386BSD__
#define CONSOLE_DEV "/dev/vga"

    /* detaching the controlling tty solves problems of kbd character loss */
    setpgrp(0, getpid());
    if ((i = open("/dev/tty",O_RDWR)) >= 0)
    {
      ioctl(i,TIOCNOTTY,(char *)0);
      close(i);
    }
    if ((fd = open(CONSOLE_DEV,O_RDWR | O_NDELAY,0)) < 0) 
      FatalError("Cannot open %s (errno=%d)\n",CONSOLE_DEV,errno);
    x386Info.consoleFd = fd;
    x386Info.screenFd = fd;

    x386Config(FALSE); /* Read Xconfig */
#else /* __386BSD__ */
#ifdef MACH386
    x386Config(FALSE); /* Read Xconfig */
    if ((x386Info.consoleFd = open("/dev/console",O_RDWR,0)) < 0) 
      FatalError("Cannot open /dev/console (errno=%d)\n",errno);
#endif /* MACH386 */
#endif /* __386BSD__ */
#endif /* HAS_USL_VTS */
    
#ifdef HAS_USL_VTS
#ifndef linux
    if (ioctl(x386Info.consoleFd, VT_GETMODE, &VT) < 0) 
      FatalError ("VT_GETMODE failed\n");
    
    signal(SIGUSR1, x386VTRequest);
    
    VT.mode = VT_PROCESS;
    VT.relsig = SIGUSR1;
    VT.acqsig = SIGUSR1;
    if (ioctl(x386Info.consoleFd, VT_SETMODE, &VT) < 0) 
      FatalError ("VT_SETMODE VT_PROCESS failed\n");
#endif /* !linux */
    
    if (ioctl(x386Info.consoleFd, KDSETMODE, KD_GRAPHICS) < 0)
      FatalError ("KDSETMODE KD_GRAPHICS failed\n");
#else
#ifdef __386BSD__
    if (ioctl (x386Info.consoleFd, CONSOLE_X_MODE_ON, 0) < 0)
      FatalError ("%s failed.  Your kernel lacks X server support\n",
                  "CONSOLE_X_MODE");
#endif
#endif

    x386Config(TRUE); /* Probe displays, and resolve modes */


    /*
     * collect all possible formats
     */
    formats[0].depth = 1;
    formats[0].bitsPerPixel = 1;
    formats[0].scanlinePad = BITMAP_SCANLINE_PAD;
  
    for ( i=0;
          i < x386MaxScreens && x386Screens[i] && x386Screens[i]->configured;
          i++ )
      { 
	/*
	 * At least one probe function succeeded.
	 */
	any_screens = 1;

	/*
	 * add new pixmap format
	 */
	for ( j=0; j <= numFormats; j++ ) {
	  
	  if (formats[j].depth == x386Screens[i]->depth &&
	      formats[j].bitsPerPixel == x386Screens[i]->bitsPerPixel)
	    break; /* found */
	  
	  if ( j == numFormats ) {
	    formats[j].depth = x386Screens[i]->depth;
	    formats[j].bitsPerPixel = x386Screens[i]->bitsPerPixel;
	    formats[j].scanlinePad = BITMAP_SCANLINE_PAD;
	    numFormats++;
	    if ( numFormats > MAXFORMATS )
	      FatalError( "WSGO!! Too many formats! Exiting\n" );
	    
	    break; /* added */
	  }
	}
      }
    if (!any_screens)
      ErrorF("\n\t*** None of the configured devices was detected.***\n\n");
  }

#ifdef HAS_USL_VTS
#ifndef linux
  /*
   * now force to get the VT
   */
  if (ioctl(x386Info.consoleFd, VT_ACTIVATE, x386Info.vtno) != 0)
    ErrorF("VT_ACTIVATE failed\n");
  if (ioctl(x386Info.consoleFd, VT_WAITACTIVE, x386Info.vtno) != 0)
    ErrorF("VT_WAITACTIVE failed\n");
#endif /* !linux */
#endif

  /*
   * Install signal handler for unexpected signals
   */
  if (!x386Info.notrapSignals)
  {
     x386Info.caughtSignal=FALSE;
     signal(SIGSEGV,x386SigHandler);
     signal(SIGILL,x386SigHandler);
#ifdef SIGEMT
     signal(SIGEMT,x386SigHandler);
#endif
     signal(SIGFPE,x386SigHandler);
#ifdef SIGBUS
     signal(SIGBUS,x386SigHandler);
#endif
#ifdef SIGSYS
     signal(SIGSYS,x386SigHandler);
#endif
#ifdef SIGXCPU
     signal(SIGXCPU,x386SigHandler);
#endif
#ifdef SIGXFSZ
     signal(SIGXFSZ,x386SigHandler);
#endif
  }


  /*
   * Use the previous collected parts to setup pScreenInfo
   */
  pScreenInfo->imageByteOrder = IMAGE_BYTE_ORDER;
  pScreenInfo->bitmapScanlineUnit = BITMAP_SCANLINE_UNIT;
  pScreenInfo->bitmapScanlinePad = BITMAP_SCANLINE_PAD;
  pScreenInfo->bitmapBitOrder = BITMAP_BIT_ORDER;
  pScreenInfo->numPixmapFormats = numFormats;
  for ( i=0; i < numFormats; i++ ) pScreenInfo->formats[i] = formats[i];

  if (generation != serverGeneration)
    {
      x386ScreenIndex = AllocateScreenPrivateIndex();
      generation = serverGeneration;
    }


  for ( i=0;
        i < x386MaxScreens && x386Screens[i] && x386Screens[i]->configured;
        i++ )
    {    
      /*
       * On a server-reset, we have explicitely to remap all stuff ...
       * (At startuptime this is implicitely done by probing the device
       */
      if (serverGeneration != 1)
        {
          x386Resetting = TRUE;
          x386Exiting = FALSE;
#ifdef HAS_USL_VTS
#ifndef linux
          if (!x386VTSema)
            ioctl(x386Info.consoleFd,VT_RELDISP,VT_ACKACQ);
#endif /* !linux */
#endif
          x386VTSema = TRUE;
          (x386Screens[i]->EnterLeaveVT)(ENTER);
          x386Resetting = FALSE;
        }
      index = AddScreen(x386Screens[i]->Init, argc, argv);
      if (index > -1)
	screenInfo.screens[index]->devPrivates[x386ScreenIndex].ptr
	  = (pointer)x386Screens[i];

      /*
       * Here we have to let the driver getting access of the VT. Note that
       * this doesn't mean that the graphics board may access automatically
       * the monitor. If the monitor is shared this is done in x386CrossScreen!
       */
      if (!x386Info.sharedMonitor) (x386Screens[i]->EnterLeaveMonitor)(ENTER);
    }

  RegisterBlockAndWakeupHandlers(x386Block, x386Wakeup, (void *)0);
}



/*
 * InitInput --
 *      Initialize all supported input devices...what else is there
 *      besides pointer and keyboard? Two DeviceRec's are allocated and
 *      registered as the system pointer and keyboard devices.
 */

void
InitInput(argc, argv)
     int     	  argc;
     char    	  **argv;
{
  x386Info.vtRequestsPending = FALSE;
  x386Info.inputPending = FALSE;
#ifdef XTESTEXT1
  xtest_command_key = KEY_SysReqest + MIN_KEYCODE;
#endif /* XTESTEXT1 */

  x386Info.pKeyboard = AddInputDevice(x386Info.kbdProc, TRUE); 
  x386Info.pPointer =  AddInputDevice(x386Info.mseProc, TRUE);
  RegisterKeyboardDevice(x386Info.pKeyboard); 
  RegisterPointerDevice(x386Info.pPointer); 

  miRegisterPointerDevice(screenInfo.screens[0], x386Info.pPointer);
  mieqInit (x386Info.pKeyboard, x386Info.pPointer);
}



/*
 * ddxGiveUp --
 *      Device dependent cleanup. Called by by dix before normal server death.
 *      For SYSV386 we must switch the terminal back to normal mode. No error-
 *      checking here, since there should be restored as much as possible.
 */

void
ddxGiveUp()
{
#ifdef HAS_USL_VTS
#ifndef linux
  struct vt_mode   VT;

  ioctl(x386Info.consoleFd, VT_ACTIVATE, x386Info.vtno);
  ioctl(x386Info.consoleFd, VT_WAITACTIVE, 0);
#endif /* !linux */
  ioctl(x386Info.consoleFd, KDSETMODE, KD_TEXT);  /* Back to text mode  ... */
#ifndef linux
  if (ioctl(x386Info.consoleFd, VT_GETMODE, &VT) != -1)
    {
      VT.mode = VT_AUTO;
      ioctl(x386Info.consoleFd, VT_SETMODE, &VT); /* set default vt handling */
    }
#endif /* !linux */

#else
#ifdef __386BSD__
  ioctl (x386Info.consoleFd, CONSOLE_X_MODE_OFF, 0);
  if (x386Info.screenFd != x386Info.consoleFd)
    {
      close(x386Info.screenFd);
      close(x386Info.consoleFd);
      if ((x386Info.consoleFd = open("/dev/console",O_RDONLY,0)) <0)
        FatalError("Cannot open /dev/console\n");
    }
#endif /* __386BSD__ */
#endif /* HAS_USL_VTS */
  close(x386Info.consoleFd);                    /* make the vt-manager happy */

  /* If a unexpected signal was caught, dump a core for debugging */
  if (x386Info.caughtSignal)
    abort();
}



/*
 * AbortDDX --
 *      DDX - specific abort routine.  Called by AbortServer(). The attempt is
 *      made to restore all original setting of the displays. Also all devices
 *      are closed.
 */

void
AbortDDX()
{
  int i;

  x386Exiting = TRUE;

  /*
   * try to deinitialize all input devices
   */
  if (x386Info.pPointer) (x386Info.mseProc)(x386Info.pPointer, DEVICE_CLOSE);
  if (x386Info.pKeyboard) (x386Info.kbdProc)(x386Info.pKeyboard, DEVICE_CLOSE);

  /*
   * try to restore the original video state
   */
#ifdef HAS_USL_VTS
#ifndef linux
  if (x386VTSema)
#endif /* !linux */
#endif
    for ( i=0; i < screenInfo.numScreens; i++ )
      (X386SCRNINFO(screenInfo.screens[i])->EnterLeaveVT)( LEAVE );

  /*
   * This is needed for a abnormal server exit, since the normal exit stuff
   * MUST also be performed (i.e. the vt must be left in a defined state)
   */
  ddxGiveUp();
}



/*
 * ddxProcessArgument --
 *	Process device-dependent command line args. Returns 0 if argument is
 *      not device dependent, otherwise Count of number of elements of argv
 *      that are part of a device dependent commandline option.
 */

/* ARGSUSED */
int
ddxProcessArgument (argc, argv, i)
     int argc;
     char *argv[];
     int i;
{

  if (!strcmp(argv[i], "-mono")) {
#ifdef X386MONOVGA
	return 1;
#else
	argv[0] = X386_MONO_SERVER_NAME;
    if (execvp(X386_MONO_SERVER_NAME, argv) < 0)
      FatalError("Cannot exec %s (errno=%d)\n", X386_MONO_SERVER_NAME, errno);
#endif
  }
  if (!strcmp(argv[i], "-color") || !strcmp(argv[i], "-colour")) {
#ifdef X386MONOVGA
	argv[0] = X386_COLOUR_SERVER_NAME;
    if (execvp(X386_COLOUR_SERVER_NAME, argv) < 0)
      FatalError("Cannot exec %s (errno=%d)\n", X386_COLOUR_SERVER_NAME, errno);
#else
	return 1;
#endif
  }
	
#ifdef HAS_USL_VTS
#ifndef linux
  if ((argv[i][0] == 'v') && (argv[i][1] == 't')) {	/* vtXX */
    if (sscanf(argv[i], "vt%2d", &VTnum) == 0) {
      UseMsg();
      VTnum = -1;
    }
    return 1;
  }
#endif /* !linux */
#endif
  if (!strcmp(argv[i], "-xconfig"))
  {
    if (!argv[i+1])
      return 0;
    if (strlen(argv[i+1]) >= PATH_MAX)
      FatalError("Xconfig path name too long\n");
    strcpy(x386ConfigFile, argv[i+1]);
    return 2;
  }
  if (!strcmp(argv[i],"-showconfig"))
  {
    x386PrintConfig();
    exit(0);
  }
  return 0;
}


/*
 * ddxUseMsg --
 *	Print out correct use of device dependent commandline options.
 *      Maybe the user now knows what really to do ...
 */

void
ddxUseMsg()
{
  ErrorF("\n");
  ErrorF("\n");
  ErrorF("Device Dependent Usage\n");
#ifdef HAS_USL_VTS
#ifndef linux
  ErrorF("vtXX                   use the specified VT number\n");
#endif /* !linux */
#endif
  ErrorF("-xconfig file          specify a configuration file\n");
  ErrorF(
   "-showconfig            show which drivers are included in the server\n");
  ErrorF("\n");
}


static void
x386PrintConfig()
{
  int i;

  ErrorF("XFree86 Version%s/ X Window System\n",X386_VERSION);
  ErrorF("(protocol Version %d, revision %d, vendor release %d)\n",
         X_PROTOCOL, X_PROTOCOL_REVISION, VENDOR_RELEASE );
  ErrorF("Configured drivers:\n");
  for (i = 0; i < x386MaxScreens; i++)
    if (x386Screens[i])
      (x386Screens[i]->PrintIdent)();
}
