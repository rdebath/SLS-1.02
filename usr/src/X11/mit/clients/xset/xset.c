/* 
 * $XConsortium: xset.c,v 1.59 91/07/22 18:32:33 keith Exp $ 
 */

/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission.  M.I.T. makes no representations about the
suitability of this software for any purpose.  It is provided "as is"
without express or implied warranty.
*/

/* $XConsortium: xset.c,v 1.59 91/07/22 18:32:33 keith Exp $ */

#include <stdio.h>
#include <ctype.h>

#include <X11/Xos.h>
#include <X11/Xfuncs.h>
#include <X11/Xlib.h>
/*  #include <X11/Xlibwm.h>  [Doesn't exist yet  5-14-87]  %*/
#include <X11/keysym.h>
#include <X11/Xproto.h>
#include <X11/Xutil.h>
#include <X11/Xmu/Error.h>
#ifdef MITMISC
#include <X11/extensions/MITMisc.h>
#endif

#define ON 1
#define OFF 0

#define SERVER_DEFAULT (-1)
#define DONT_CHANGE -2

#define ALL -1
#define TIMEOUT 1
#define INTERVAL 2
#define PREFER_BLANK 3
#define ALLOW_EXP 4

#define	nextarg(i, argv) \
	argv[i]; \
	if (i >= argc) \
		break; \

char *progName;

int local_xerror();

main(argc, argv)
int argc;
char **argv;
{
register char *arg;
register int i;
int percent;
int acc_num, acc_denom, threshold;
int key, auto_repeat_mode;
XKeyboardControl values;
#define MAX_PIXEL_COUNT 512
unsigned long pixels[MAX_PIXEL_COUNT];
caddr_t colors[MAX_PIXEL_COUNT];
int numpixels = 0;
char *disp = NULL;
Display *dpy;
Bool hasargs = False;

progName = argv[0];
for (i = 1; i < argc; i++) {
  arg = argv[i];
  if (strcmp (arg, "-display") == 0 || strcmp (arg, "-d") == 0) {
    if (++i >= argc) usage ("missing argument to -display", NULL);
    disp = argv[i];
  } else {
    hasargs = True;
  }
}
if (!hasargs) {
    usage (NULL, NULL);			/* replace with window interface */
}

dpy = XOpenDisplay(disp);  /*  Open display and check for success */
if (dpy == NULL) {
  fprintf(stderr, "%s:  unable to open display \"%s\"\n",
	  argv[0], XDisplayName (disp));
  exit(1);
}
XSetErrorHandler (local_xerror);
for (i = 1; i < argc; ) {
  arg = argv[i++];
  if (strcmp (arg, "-display") == 0 || strcmp (arg, "-d") == 0) {
    ++i;					/* already dealt with */
    continue;
  } else if (*arg == '-' && *(arg + 1) == 'c'){ /* Does arg start with "-c"? */
    set_click(dpy, 0);           /* If so, turn click off and  */
  } 
  else if (*arg == 'c') {         /* Well, does it start with "c", then? */
    percent = SERVER_DEFAULT;		/* Default click volume. */
    if (i >= argc) {
	set_click (dpy, percent);	/* set click to default */
	break;
    }
    arg = nextarg(i, argv);
    if (strcmp(arg, "on") == 0) {               /* Let click be default. */
      i++;
    } 
    else if (strcmp(arg, "off") == 0) {  
      percent = 0;       /* Turn it off.          */
      i++;
    } 
    else if (isnumber(arg, 100)) {
      percent = atoi(arg);  /* Set to spec. volume */
      i++;
    }
    set_click(dpy, percent);
  } 
  else if (strcmp(arg, "-b") == 0) {
    set_bell_vol(dpy, 0);           /* Then turn off bell.    */
  } 
  else if (strcmp(arg, "b") == 0) {
    percent = SERVER_DEFAULT;		/* Set bell to default. */
    if (i >= argc) {
	set_bell_vol (dpy, percent);	/* set bell to default */
	break;
    }
    arg = nextarg(i, argv);
    if (strcmp(arg, "on") == 0) {               /* Let it stay that way.  */
      set_bell_vol(dpy, percent);
      i++;
    } 
    else if (strcmp(arg, "off") == 0) {
      percent = 0;            /* Turn the bell off.     */
      set_bell_vol(dpy, percent);
      i++;
    } 
    else if (isnumber(arg, 100)) {              /* If volume is given:    */
      percent = atoi(arg);    /* set bell appropriately.*/
      set_bell_vol(dpy, percent);
      i++;
      arg = nextarg(i, argv);

      if (isnumber(arg, 20000)) {               /* If pitch is given:     */
	set_bell_pitch(dpy, atoi(arg));    /* set the bell.           */
	i++;

	arg = nextarg(i, argv);
	if (isnumber(arg, 1000)) {              /* If duration is given:  */
	  set_bell_dur(dpy, atoi(arg));  /*  set the bell.      */
	  i++;
	}
      }
    }
    else
      set_bell_vol (dpy, percent);		/* set bell to default */
  }
#ifdef MITMISC
  else if (strcmp(arg, "bc") == 0) {
      int dummy;
      if (XMITMiscQueryExtension(dpy, &dummy, &dummy))
	  XMITMiscSetBugMode(dpy, True);
      else
	  fprintf(stderr, "server does not have extension for bc option\n");
  }
  else if (strcmp(arg, "-bc") == 0) {
      int dummy;
      if (XMITMiscQueryExtension(dpy, &dummy, &dummy))
	  XMITMiscSetBugMode(dpy, False);
      else
	  fprintf(stderr, "server does not have extension for -bc option\n");
  }
#endif
  else if (strcmp(arg, "fp") == 0) {	       /* set font path */
    if (i >= argc) {
	arg = "default";
    } else {
	arg = nextarg(i, argv);
    }
    set_font_path(dpy, arg, 1, 0, 0);	/* special argument */
    i++;
  }
  else if (strcmp(arg, "fp=") == 0) {	/* unconditionally set */
    if (i >= argc) {
	usage ("missing fp= argument", NULL);
    } else {
	arg = nextarg(i, argv);
    }
    set_font_path(dpy, arg, 0, 0, 0);	/* not special, set */
    i++;
  }
  else if (strcmp(arg, "+fp") == 0) {	       /* set font path */
    if (i >= argc) usage ("missing +fp argument", NULL);
    arg = nextarg(i, argv);
    set_font_path(dpy, arg, 0, 1, 0);	/* not special, prepend */
    i++;
  }
  else if (strcmp(arg, "fp+") == 0) {	       /* set font path */
    if (i >= argc) usage ("missing fp+ argument", NULL);
    arg = nextarg(i, argv);
    set_font_path(dpy, arg, 0, 0, 1);	/* not special, append */
    i++;
  }
  else if (strcmp(arg, "-fp") == 0) {	       /* set font path */
    if (i >= argc) usage ("missing -fp argument", NULL);
    arg = nextarg(i, argv);
    set_font_path(dpy, arg, 0, -1, 0);	/* not special, preremove */
    i++;
  }
  else if (strcmp(arg, "fp-") == 0) {	       /* set font path */
    if (i >= argc) usage ("missing fp- argument", NULL);
    arg = nextarg(i, argv);
    set_font_path(dpy, arg, 0, 0, -1);	/* not special, postremove */
    i++;
  }
  else if (strcmp(arg, "-led") == 0) {         /* Turn off one or all LEDs */
    values.led_mode = OFF;
    values.led = ALL;        /* None specified */
    arg = nextarg(i, argv);
    if (isnumber(arg, 32) && atoi(arg) > 0) {
      values.led = atoi(arg);
      i++;
    }
    set_led(dpy, values.led, values.led_mode);
  } 
  else if (strcmp(arg, "led") == 0) {         /* Turn on one or all LEDs  */
    values.led_mode = ON;
    values.led = ALL;
    if (i >= argc) {
	set_led (dpy, values.led, values.led_mode);	/* set led to def */
	break;
    }
    arg = nextarg(i, argv);
    if (strcmp(arg, "on") == 0) {
      i++;
    } 
    else if (strcmp(arg, "off") == 0) {       /*  ...except in this case. */
       values.led_mode = OFF;
      i++;
    }
    else if (isnumber(arg, 32) && atoi(arg) > 0) {
      values.led = atoi(arg);
      i++;
    }
    set_led(dpy, values.led, values.led_mode);
  }
/*  Set pointer (mouse) settings:  Acceleration and Threshold. */
  else if (strcmp(arg, "m") == 0 || strcmp(arg, "mouse") == 0) {
    acc_num = SERVER_DEFAULT;		/* restore server defaults */
    acc_denom = SERVER_DEFAULT;
    threshold = SERVER_DEFAULT;
    if (i >= argc){
      set_mouse(dpy, acc_num, acc_denom, threshold);
      break;
    }
    arg = argv[i];
    if (strcmp(arg, "default") == 0) {
      i++;
    } 
    else if (*arg >= '0' && *arg <= '9') {
      acc_denom = 1;
      sscanf(arg, "%d/%d", &acc_num, &acc_denom);
      i++;
      if (i >= argc) {
	set_mouse(dpy, acc_num, acc_denom, threshold);
	break;
      }
      arg = argv[i];
      if (*arg >= '0' && *arg <= '9') {
	threshold = atoi(arg);  /* Set threshold as user specified.  */
	i++;
      }
    }
    set_mouse(dpy, acc_num, acc_denom, threshold);
  } 
  else if (strcmp(arg, "s") == 0) {
    if (i >= argc) {
      set_saver(dpy, ALL, 0);  /* Set everything to default  */
      break;
    }
    arg = argv[i];
    if (strcmp(arg, "blank") == 0) {       /* Alter blanking preference. */
      set_saver(dpy, PREFER_BLANK, PreferBlanking);
      i++;
    }
    else if (strcmp(arg, "noblank") == 0) {     /*  Ditto.  */
      set_saver(dpy, PREFER_BLANK, DontPreferBlanking);
      i++;
    }
    else if (strcmp(arg, "expose") == 0) {       /* Alter exposure preference. */
      set_saver(dpy, ALLOW_EXP, AllowExposures);
      i++;
    }
    else if (strcmp(arg, "noexpose") == 0) {     /*  Ditto.  */
      set_saver(dpy, ALLOW_EXP, DontAllowExposures);
      i++;
    }
    else if (strcmp(arg, "off") == 0) {
      set_saver(dpy, TIMEOUT, 0);   /*  Turn off screen saver.  */
      i++;
      if (i >= argc)
	break;
      arg = argv[i];
      if (strcmp(arg, "off") == 0) {
	set_saver(dpy, INTERVAL, 0);
	i++;
      }
    }
    else if (strcmp(arg, "default") == 0 ||
	     strcmp(arg, "on") == 0) {    /*  Leave as default.       */
      set_saver(dpy, ALL, 0);
      i++;
    } 
    else if (*arg >= '0' && *arg <= '9') {  /*  Set as user wishes.   */
      set_saver(dpy, TIMEOUT, atoi(arg));
      i++;
      if (i >= argc)
	break;
      arg = argv[i];
      if (*arg >= '0' && *arg <= '9') {
	set_saver(dpy, INTERVAL, atoi(arg));
	i++;
      }
    }
  } 
  else if (strcmp(arg, "-r") == 0) {        /* Turn off one or all autorepeats */
    auto_repeat_mode = OFF;
    key = ALL;          /* None specified */
    arg = argv[i];
    if (i < argc)
    if (isnumber(arg, 255)) {
      key = atoi(arg);
      i++;
  } 
    set_repeat(dpy, key, auto_repeat_mode);
    }
  else if (strcmp(arg, "r") == 0) {         /* Turn on one or all autorepeats */
    auto_repeat_mode = ON;
    key = ALL;          /* None specified */
    arg = argv[i];
    if (i < argc)
    if (strcmp(arg, "on") == 0) {
      i++;
    } 
    else if (strcmp(arg, "off") == 0) {       /*  ...except in this case. */
      auto_repeat_mode = OFF;
      i++;
    }
    else if (isnumber(arg, 255)) {
      key = atoi(arg);
      i++;
  } 
    set_repeat(dpy, key, auto_repeat_mode);
  } 
  else if (strcmp(arg, "p") == 0) {
    if (i + 1 >= argc)
      usage ("missing argument to p", NULL);
    arg = argv[i];
    if (numpixels >= MAX_PIXEL_COUNT)
	usage ("more than %d pixels specified", MAX_PIXEL_COUNT);
    if (*arg >= '0' && *arg <= '9')
      pixels[numpixels] = atoi(arg);
    else
      usage ("invalid pixel number %s", arg);
    i++;
    colors[numpixels] = argv[i];
    i++;
    numpixels++;
  }
  else if (strcmp(arg, "-k") == 0) {
    set_lock(dpy, OFF);
  }
  else if (strcmp(arg, "k") == 0) {
    set_lock(dpy, ON);
  }
  else if (strcmp(arg, "q") == 0 || strcmp(arg, "-q") == 0) {
    query(dpy);
  }
  else
    usage ("unknown option %s", arg);
}

if (numpixels)
    set_pixels(dpy, pixels, colors, numpixels);

XCloseDisplay (dpy);

exit(0);    /*  Done.  We can go home now.  */
}


isnumber(arg, maximum)
	char *arg;
	int maximum;
{
	register char *p;

	if (arg[0] == '-' && arg[1] == '1' && arg[2] == '\0')
		return(1);
	for (p=arg; isdigit(*p); p++);
	if (*p || atoi(arg) > maximum)
		return(0); 
	return(1);
}

/*  These next few functions do the real work (xsetting things).
 */
set_click(dpy, percent)
Display *dpy;
int percent;
{
XKeyboardControl values;
values.key_click_percent = percent;
XChangeKeyboardControl(dpy, KBKeyClickPercent, &values);
return;
}

set_bell_vol(dpy, percent)
Display *dpy;
int percent;
{
XKeyboardControl values;
values.bell_percent = percent;
XChangeKeyboardControl(dpy, KBBellPercent, &values);
return;
}

set_bell_pitch(dpy, pitch)
Display *dpy;
int pitch;
{
XKeyboardControl values;
values.bell_pitch = pitch;
XChangeKeyboardControl(dpy, KBBellPitch, &values);
return;
}

set_bell_dur(dpy, duration)
Display *dpy;
int duration;
{
XKeyboardControl values;
values.bell_duration = duration;
XChangeKeyboardControl(dpy, KBBellDuration, &values);
return;
}

/*
 * Set, add, or subtract the path according to before and after flags:
 *
 *	before	after	action
 *
 *	   0      0	FontPath := path
 *	  -1      0	FontPath := current - path
 *	   0     -1	FontPath := current - path
 *	   1      0	FontPath := path + current
 *	   0      1	FontPath := current + path
 */

set_font_path(dpy, path, special, before, after)
    Display *dpy;
    char *path;
    int special, before, after;
{
    char **directoryList = NULL; int ndirs = 0;
    char **currentList = NULL; int ncurrent = 0;

    if (special) {
	if (strcmp (path, "default") == 0) {
	    XSetFontPath (dpy, NULL, 0);
	    return;
	}
	if (strcmp (path, "rehash") == 0) {
	    currentList = XGetFontPath (dpy, &ncurrent);
	    if (!currentList) {
		fprintf (stderr, "%s:  unable to get current font path.\n",
			 progName);
		return;
	    }
	    XSetFontPath (dpy, currentList, ncurrent);
	    XFreeFontPath (currentList);
	    return;
	} 
	/*
	 * for now, fall though and process keyword and directory list for
	 * compatibility with previous versions.
	 */
    }

    /*
     * parse the path list.  If before or after is non-zero, we'll need 
     * the current value.
     */

    if (before != 0 || after != 0) {
	currentList = XGetFontPath (dpy, &ncurrent);
	if (!currentList) {
	    fprintf (stderr, "%s:  unable to get old font path.\n",
		     progName);
	    before = after = 0;
	}
    }

    {
	/* count the number of directories in path */
	register char *cp = path;

	ndirs = 1;
	while ((cp = index (cp, ',')) != NULL) {
	    ndirs++;
	    cp++;
	}
    }

    directoryList = (char **) malloc (ndirs*sizeof (char *));
    if (!directoryList) error ("out of memory for font path directory list");

    {
	/* mung the path and set directoryList pointers */
	int i = 0;
	char *cp = path;

	directoryList[i++] = cp;
	while ((cp = index (cp, ',')) != NULL) {
	    directoryList[i++] = cp + 1;
	    *cp++ = '\0';
	}
	if (i != ndirs) {
	    fprintf (stderr, 
		     "%s: internal error, only parsed %d of %d directories.\n",
		     progName, i, ndirs);
	    exit (1);
	}
    }
	    
    /*
     * now we have have parsed the input path, so we can set it
     */

    if (before == 0 && after == 0) {
	XSetFontPath (dpy, directoryList, ndirs);
    }

    /* if adding to list, build a superset */
    if (before > 0 || after > 0) {
	int nnew = ndirs + ncurrent;
	char **newList = (char **) malloc (nnew * sizeof (char *));
	
	if (!newList) error ("out of memory");
	if (before > 0) {		/* new + current */
	    bcopy ((char *) directoryList, (char *) newList, 
		   (unsigned) (ndirs*sizeof (char *)));
	    bcopy ((char *) currentList, (char *) (newList + ndirs),
			(unsigned) (ncurrent*sizeof (char *)));
	    XSetFontPath (dpy, newList, nnew);
	} else if (after > 0) {
	    bcopy ((char *) currentList, (char *) newList,
		   (unsigned) (ncurrent*sizeof (char *)));
	    bcopy ((char *) directoryList, (char *) (newList + ncurrent),
		   (unsigned) (ndirs*sizeof (char *)));
	    XSetFontPath (dpy, newList, nnew);
	} 
	free ((char *) newList);
    }

    /* if deleting from list, build one the same size */
    if (before < 0 || after < 0) {
	int i, j;
	int nnew = 0;
	char **newList = (char **) malloc (ncurrent * sizeof (char *));
	
	if (!newList) error ("out of memory");
	for (i = 0; i < ncurrent; i++) {
	    for (j = 0; j < ndirs; j++) {
		if (strcmp (currentList[i], directoryList[j]) == 0)
		  break;
	    }
	    /* if we ran out, then insert into new list */
	    if (j == ndirs) newList[nnew++] = currentList[i];
	}
	if (nnew == ncurrent) {
	    fprintf (stderr,
		     "%s:  warning, no entries deleted from font path.\n",
		     progName);
	}
	XSetFontPath (dpy, newList, nnew);
	free ((char *) newList);
    }

    if (directoryList) free ((char *) directoryList);
    if (currentList) XFreeFontPath (currentList);

    return;
}


set_led(dpy, led, led_mode)
Display *dpy;
int led, led_mode;
{
  XKeyboardControl values;
  values.led_mode = led_mode;
  if (led != ALL) {
    values.led = led;
    XChangeKeyboardControl(dpy, KBLed | KBLedMode, &values);
  }
  else {
    XChangeKeyboardControl(dpy, KBLedMode, &values);
  }
  return;
}

set_mouse(dpy, acc_num, acc_denom, threshold)
Display *dpy;
int acc_num, acc_denom, threshold;
{
int do_accel = True, do_threshold = True;

if (acc_num == DONT_CHANGE)		/* what an incredible crock... */
  do_accel = False;
if (threshold == DONT_CHANGE)
  do_threshold = False;
if (acc_num < 0) 			/* shouldn't happen */
  acc_num = SERVER_DEFAULT;
if (acc_denom <= 0)			/* prevent divide by zero */
  acc_denom = SERVER_DEFAULT;
if (threshold < 0) threshold = SERVER_DEFAULT;
XChangePointerControl(dpy, do_accel, do_threshold, acc_num,
		      acc_denom, threshold);
return;
}

set_saver(dpy, mask, value)
Display *dpy;
int mask, value;
{
  int timeout, interval, prefer_blank, allow_exp;
  XGetScreenSaver(dpy, &timeout, &interval, &prefer_blank, 
		  &allow_exp);
  if (mask == TIMEOUT) timeout = value;
  if (mask == INTERVAL) interval = value;
  if (mask == PREFER_BLANK) prefer_blank = value;
  if (mask == ALLOW_EXP) allow_exp = value;
  if (mask == ALL) {  /* "value" is ignored in this case.  (defaults) */
    timeout = SERVER_DEFAULT;
    interval = SERVER_DEFAULT;
    prefer_blank = DefaultBlanking;
    allow_exp = DefaultExposures;
  }
      XSetScreenSaver(dpy, timeout, interval, prefer_blank, 
		      allow_exp);
  return;
}

set_repeat(dpy, key, auto_repeat_mode)
Display *dpy;
int key, auto_repeat_mode;
{
  XKeyboardControl values;
  values.auto_repeat_mode = auto_repeat_mode;
  if (key != ALL) {
    values.key = key;
    XChangeKeyboardControl(dpy, KBKey | KBAutoRepeatMode, &values);
  }
  else {
    XChangeKeyboardControl(dpy, KBAutoRepeatMode, &values);
  }
  return;
}

set_pixels(dpy, pixels, colors, numpixels)
Display *dpy;
unsigned long *pixels;
caddr_t *colors;
int numpixels;
{
  XColor def;
  int scr = DefaultScreen (dpy);
  Visual *visual = DefaultVisual (dpy, scr);
  Colormap cmap = DefaultColormap (dpy, scr);
  unsigned long max_cells = DisplayCells(dpy, scr);
  XVisualInfo viproto, *vip;
  int nvisuals = 0;
  char *visual_type = NULL;
  int i;

  viproto.visualid = XVisualIDFromVisual (visual);
  vip = XGetVisualInfo (dpy, VisualIDMask, &viproto, &nvisuals);
  if (!vip) {
      fprintf (stderr, "%s: Can't get visual for visualID 0x%x\n",
	       progName, viproto.visualid);
      return;
  }

  switch (vip->class) {
      case GrayScale:
      case PseudoColor:
	break;
      case TrueColor:
	visual_type = "TrueColor";
	/* fall through */
      case DirectColor:
	max_cells *= max_cells * max_cells;
	break;
      case StaticGray:
	visual_type = "StaticGray";
	break;
      case StaticColor:
	visual_type = "StaticColor";
	break;
      default:
	fprintf (stderr, "%s:  unknown visual class type %d\n",
		 progName, vip->class);
	numpixels = 0;
  }

  if (visual_type) {
    fprintf (stderr, "%s:  cannot set pixel values in read-only %s visuals\n",
	     progName, visual_type);
  } else {
    for (i = 0; i < numpixels; i++) {
      def.pixel = pixels[i];
      if (def.pixel >= max_cells)
	fprintf(stderr, 
		"%s:  pixel value %d out of colormap range 0 through %d\n",
		progName, def.pixel, max_cells - 1);
      else {
        if (XParseColor (dpy, cmap, colors[i], &def))
	  XStoreColor(dpy, cmap, &def);
        else
	  fprintf (stderr, "%s:  invalid color \"%s\"\n", progName, colors[i]);
      }
    }
  }

  XFree ((char *) vip);

  return;
}

set_lock(dpy, onoff)
Display *dpy;
Bool onoff;
{
  XModifierKeymap *mods;
  mods = XGetModifierMapping(dpy);

  if (onoff)
    mods = XInsertModifiermapEntry(mods, XK_Caps_Lock, LockMapIndex);
  else
    mods = XDeleteModifiermapEntry(mods, XK_Caps_Lock, LockMapIndex);
  XSetModifierMapping(dpy, mods);
  return;
}

char *on_or_off (val, onval, onstr, offval, offstr, buf)
    int val, onval, offval;
    char *onstr, *offstr;
    char buf[];
{
    if (val == onval) 
      return onstr;
    else if (val == offval)
      return offstr;

    buf[0] = '\0';
    sprintf (buf, "<%d>", val);
    return buf;
}


/*  This is the information-getting function for telling the user what the
 *  current "xsettings" are.
 */
query(dpy)
Display *dpy;
{
int scr = DefaultScreen (dpy);
XKeyboardState values;
int acc_num, acc_denom, threshold;
int timeout, interval, prefer_blank, allow_exp;
char **font_path; int npaths;
int i, j;
char buf[20];				/* big enough for 16 bit number */

XGetKeyboardControl(dpy, &values);
XGetPointerControl(dpy, &acc_num, &acc_denom, &threshold);
XGetScreenSaver(dpy, &timeout, &interval, &prefer_blank, &allow_exp);
font_path = XGetFontPath(dpy, &npaths);

printf ("Keyboard Control:\n");
printf ("  auto repeat:  %s    key click percent:  %d    LED mask:  %08lx\n", 
	on_or_off (values.global_auto_repeat,
		   AutoRepeatModeOn, "on", AutoRepeatModeOff, "off", buf),
	values.key_click_percent, values.led_mask);
printf ("  auto repeating keys:  ");
for (i = 0; i < 4; i++) {
    if (i) printf ("                        ");
    for (j = 0; j < 8; j++) {
	printf ("%02x", (unsigned char)values.auto_repeats[i*8 + j]);
    }
    printf ("\n");
}
printf ("  bell percent:  %d    bell pitch:  %d    bell duration:  %d\n",
	values.bell_percent, values.bell_pitch, values.bell_duration);

printf ("Pointer Control:\n");
printf ("  acceleration:  %d/%d    threshold:  %d\n",
	acc_num, acc_denom, threshold);

printf ("Screen Saver:\n");
printf ("  prefer blanking:  %s    ",
	on_or_off (prefer_blank, PreferBlanking, "yes",
		   DontPreferBlanking, "no", buf));
printf ("allow exposures:  %s\n",
	on_or_off (allow_exp, AllowExposures, "yes",
		   DontAllowExposures, "no", buf));
printf ("  timeout:  %d    cycle:  %d\n", timeout, interval);

printf ("Colors:\n");
printf ("  default colormap:  0x%lx    BlackPixel:  %d    WhitePixel:  %d\n",
	DefaultColormap (dpy, scr), 
	BlackPixel (dpy, scr), WhitePixel (dpy, scr));

printf ("Font Path:\n");
if (npaths) {
    printf( "  %s", *font_path++ );
    for( --npaths; npaths; npaths-- )
        printf( ",%s", *font_path++ );
    printf( "\n" );
} else {
    printf ("  (empty)\n");
}

#ifdef MITMISC
{
    int dummy;
    if (XMITMiscQueryExtension(dpy, &dummy, &dummy)) {
	if (XMITMiscGetBugMode(dpy))
	    printf ("Bug Mode: compatibility mode is enabled\n");
	else
	    printf ("Bug Mode: compatibility mode is disabled\n");
    }
}
#endif

return;
}


/*  This is the usage function */

usage (fmt, arg)
    char *fmt;
    char *arg;
{
    if (fmt) {
	fprintf (stderr, "%s:  ", progName);
	fprintf (stderr, fmt, arg);
	fprintf (stderr, "\n\n");
    }

    fprintf (stderr, "usage:  %s [-display host:dpy] option ...\n", progName);
    fprintf (stderr, "    To turn bell off:\n");
    fprintf (stderr, "\t-b                b off               b 0\n");
    fprintf (stderr, "    To set bell volume, pitch and duration:\n");
    fprintf (stderr, "\t b [vol [pitch [dur]]]          b on\n");
#ifdef MITMISC
    fprintf (stderr, "    To disable bug compatibility mode:\n");
    fprintf (stderr, "\t-bc\n");
    fprintf (stderr, "    To enable bug compatibility mode:\n");
    fprintf (stderr, "\tbc\n");
#endif
    fprintf (stderr, "    To turn keyclick off:\n");
    fprintf (stderr, "\t-c                c off               c 0\n");
    fprintf (stderr, "    To set keyclick volume:\n");
    fprintf (stderr, "\t c [0-100]        c on\n");
    fprintf (stderr, "    To set the font path:\n" );
    fprintf (stderr, "\t fp= path[,path...]\n" );
    fprintf (stderr, "    To restore the default font path:\n");
    fprintf (stderr, "\t fp default\n");
    fprintf (stderr, "    To have the server reread font databases:\n");
    fprintf (stderr, "\t fp rehash\n");
    fprintf (stderr, "    To remove elements from font path:\n");
    fprintf (stderr, "\t-fp path[,path...]  fp- path[,path...]\n");
    fprintf (stderr, "    To prepend or append elements to font path:\n");
    fprintf (stderr, "\t+fp path[,path...]  fp+ path[,path...]\n");
    fprintf (stderr, "    To set LED states off or on:\n");
    fprintf (stderr, "\t-led [1-32]         led off\n");
    fprintf (stderr, "\t led [1-32]         led on\n");
    fprintf (stderr, "    To set mouse acceleration and threshold:\n");
    fprintf (stderr, "\t m [acc_mult[/acc_div] [thr]]    m default\n");
    fprintf (stderr, "    To set pixel colors:\n");
    fprintf (stderr, "\t p pixel_value color_name\n");
    fprintf (stderr, "    To turn auto-repeat off or on:\n");
    fprintf (stderr, "\t-r [keycode]        r off\n");
    fprintf (stderr, "\t r [keycode]        r on\n");
    fprintf (stderr, "    For screen-saver control:\n");
    fprintf (stderr, "\t s [timeout [cycle]]  s default    s on\n");
    fprintf (stderr, "\t s blank              s noblank    s off\n");
    fprintf (stderr, "\t s expose             s noexpose\n");
    fprintf (stderr, "    For status information:  q\n");
    exit(0);
}

error( message )
    char *message;
{
    fprintf( stderr, "%s: %s\n", progName, message );
    exit( 1 );
}


int local_xerror (dpy, rep)
    Display *dpy;
    XErrorEvent *rep;
{
    char *errname = NULL;

    if (rep->request_code == X_StoreColors) {
	switch (rep->error_code) {
	  case BadAccess:
	    errname = "BadAccess, pixel not allocated read/write";
	    break;
	  case BadValue:
	    errname = "BadValue, invalid pixel number";
	    break;
	}
    }

    if (errname) {
	fprintf (stderr, "%s:  error in storing color:  %s\n",
		 progName, errname);
    } else {
	XmuPrintDefaultErrorMessage (dpy, rep, stderr);
    }
    return (0);
}
