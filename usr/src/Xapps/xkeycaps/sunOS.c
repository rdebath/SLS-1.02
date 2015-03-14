/* xkeycaps, Copyright (c) 1991, 1992 Jamie Zawinski <jwz@lucid.com>
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation.  No representations are made about the suitability of this
 * software for any purpose.  It is provided "as is" without express or 
 * implied warranty.
 */

/* SunOS-specific stuff: if we're on console, we can query the keyboard
   hardware directly to find out what kind it is.  I would have just put
   this code in guess.c, but vuid_event.h defines a `struct keyboard' 
   that conflicts with our own...
 */

#if __STDC__
#include <stdlib.h>
#include <unistd.h>
extern char *strdup (const char *);
#endif

#include <stdio.h>
#include <sys/types.h>
#include <string.h>
#include <sys/time.h>
#include <sys/stream.h>
#include <sys/stropts.h>
#include <sys/fcntl.h>
#include <sys/ioctl.h>
#include <sundev/vuid_event.h>
#include <sundev/kbio.h>
#include <sundev/kbd.h>

char *
xkeycaps_sunOS_guess_local_keyboard_type ()
{
  int type = -1, layout = 0;
  int kbdfd;

  if ((kbdfd = open ("/dev/kbd", O_WRONLY)) <= 0)
    return 0;
  if (ioctl (kbdfd, KIOCTYPE, &type))
    {
      close (kbdfd);
      return 0;
    }
  ioctl (kbdfd, KIOCLAYOUT, &layout);
  close (kbdfd);
  switch (type) {
  case -1:	  return 0;
  case KB_ASCII:  return "SunASCII";	/* Ascii terminal */
  case KB_KLUNK:  return "MS103SD32-2";	/* Micro Switch 103SD32-2 */
  case KB_VT100:  return "SunVT100";	/* Keytronics VT100 compatible */
  case KB_VT220:  return "SunVT220";	/* vt220 Emulation */
  case KB_VT220I: return "SunVT220i";	/* International vt220 Emulation */
  case KB_SUN2:   return "Sun2";
  case KB_SUN3:   return "Sun3";
  case KB_SUN4:
    switch (layout) {
    case  0: return "Sun4";
    case 33: return "Sun5PC";
    case 34: return "Sun5";
    case 35: return "Sun5-French";
    case 36: return "Sun5-Danish";
    case 37: return "Sun5-German";
    case 38: return "Sun5-Italian";
    case 39: return "Sun5-Dutch";
    case 40: return "Sun5-Norwegian";
    case 41: return "Sun5-Portuguese";
    case 42: return "Sun5-Spanish";
    case 43: return "Sun5-Swedish/Finnish";
    case 44: return "Sun5-Swiss/French";
    case 45: return "Sun5-Swiss/German";
    case 46: return "Sun5-UK";
    case 47: return "Sun5-Korean";
    case 48: return "Sun5-Taiwanese";
    case 49: return "Sun5-Nihon-go";
    default:
      {
	char buf [255];
	sprintf (buf, "Sun4_%d", layout);
	return strdup (buf);
      }
    }
  default:
    {
      char buf [255];
      if (layout)
	sprintf (buf, "Sun_%d_%d", type, layout);
      else
	sprintf (buf, "Sun_%d", type);
      return strdup (buf);
    }
  }
}


#if 0


KeySym xkeycaps_sun_map [256][8];

static int
parse_map_file (file)
     char *file;
{
  FILE fd = fopen (file, "r");
  char buf [1024];
  
  if (! fd) return 0;
  while (getl (fd, buf))
    {
      int code;
      char sym1 [255], sym2 [255], sym3 [255], sym4 [255];
      char junk1, junk2;
      int count;

      if (buf [0] == '#')
	continue;

      count = sscanf (buf, "%d %c%c %s %s %s %s",
		      &code, &junk1, &junk2, &sym1, &sym2, &sym3, &sym4);
      if (count < 4)
	continue;
      
    }
  fclose (fd);
}

KeySym **
xkeycaps_sunOS_lookup_map ()
{
}


#endif
