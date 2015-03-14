/************************************************************
Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

********************************************************/

#ifndef SITE_H
#define SITE_H
/*
 * The vendor string identifies the vendor responsible for the
 * server executable.
 */
#ifndef VENDOR_STRING
#define VENDOR_STRING "MIT X Consortium"
#endif

/*
 * The vendor release number identifies, for the purpose of submitting
 * traceable bug reports, the release number of software produced
 * by the vendor.
 */
#ifndef VENDOR_RELEASE
#define VENDOR_RELEASE	5000
#endif

/*
 * The following constants are provided solely as a last line of defense.  The
 * normal build ALWAYS overrides them using a special rule given in
 * server/dix/Imakefile.  If you want to change either of these constants, 
 * you should set the DefaultFontPath or DefaultRGBDatabase configuration 
 * parameters.
 * DO NOT CHANGE THESE VALUES OR THE DIX IMAKEFILE!
 */
#ifndef COMPILEDDEFAULTFONTPATH
#define COMPILEDDEFAULTFONTPATH	"/usr/lib/X11/fonts/misc/"
#endif
#ifndef RGB_DB
#define RGB_DB			"/usr/lib/X11/rgb"
#endif

/*
 * The following constants contain default values for all of the variables 
 * that can be initialized on the server command line or in the environment.
 */
#define COMPILEDDEFAULTFONT	"fixed"
#define COMPILEDCURSORFONT	"cursor"
#ifndef COMPILEDDISPLAYCLASS
#define COMPILEDDISPLAYCLASS	"MIT-unspecified"
#endif
#define DEFAULT_TIMEOUT		60	/* seconds */
#define DEFAULT_KEYBOARD_CLICK 	0
#define DEFAULT_BELL		50
#define DEFAULT_BELL_PITCH	400
#define DEFAULT_BELL_DURATION	100
#define DEFAULT_AUTOREPEAT	FALSE
#define DEFAULT_AUTOREPEATS	{\
	0, 0, 0, 0, 0, 0, 0, 0,\
	0, 0, 0, 0, 0, 0, 0, 0,\
	0, 0, 0, 0, 0, 0, 0, 0,\
	0, 0, 0, 0, 0, 0, 0, 0 }
#define DEFAULT_LEDS		0x0        /* all off */
#define DEFAULT_LEDS_MASK	0xffffffff /* 32 */
#define DEFAULT_INT_RESOLUTION		1000
#define DEFAULT_INT_MIN_VALUE		0
#define DEFAULT_INT_MAX_VALUE		100
#define DEFAULT_INT_DISPLAYED		0

#define DEFAULT_PTR_NUMERATOR	2
#define DEFAULT_PTR_DENOMINATOR	1
#define DEFAULT_PTR_THRESHOLD	4

#define DEFAULT_SCREEN_SAVER_TIME (10 * (60 * 1000))
#define DEFAULT_SCREEN_SAVER_INTERVAL (10 * (60 * 1000))
#define DEFAULT_SCREEN_SAVER_BLANKING PreferBlanking
#define DEFAULT_SCREEN_SAVER_EXPOSURES AllowExposures
#ifndef NOLOGOHACK
#define DEFAULT_LOGO_SCREEN_SAVER 1
#endif
#ifndef DEFAULT_ACCESS_CONTROL
#define DEFAULT_ACCESS_CONTROL TRUE
#endif

#endif /* SITE_H */
