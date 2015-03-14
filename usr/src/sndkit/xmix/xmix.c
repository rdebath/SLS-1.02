/*======================================================================

   xmix: X interface to the Sound Blaster mixer.
   [ This file is a part of SBlast-BSD-1.5 ]

   Steve Haehnichen <shaehnic@ucsd.edu>
 
   xmix.c,v 1.5 1992/09/14 03:17:21 steve Exp

   Copyright (C) 1992 Steve Haehnichen.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 * xmix.c,v
 * Revision 1.5  1992/09/14  03:17:21  steve
 * Released with driver v1.5.
 * Converted over to stereo CD levels.
 *
 * Revision 1.4  1992/06/12  22:33:39  steve
 * Much better.
 * Moved many resources into source code.
 * Ready for release in v1.4
 *
 * Revision 1.3  1992/06/08  04:36:26  steve
 * Works fine.
 *

======================================================================*/

#include <stdio.h>
#include <sys/errno.h>
#include <fcntl.h>

/*
 * Include files required for all Toolkit programs
 */
#include <X11/Intrinsic.h>	/* Intrinsics Definitions */
#include <X11/StringDefs.h>	/* Standard Name-String definitions */
#include <X11/Shell.h>		/* Shell Definitions */

/*
 * Public include file for widgets we actually use in this file.
 */
#include <X11/Xaw/Command.h>	/* Athena Command Widget */
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Scrollbar.h>
#include <X11/Xaw/Label.h>

#include "xmix.icon.bit"	/* The icon, of course */
#include "stereo.bit"		/* Stereo sound icon */
#include "mono.bit"		/* Mono sound icon */
#include "locked.bit"		/* L & R locked together (From dirt) */
#include "unlocked.bit"		/* L & R free to be different (From dirt)*/
#include "circle_on.bit"
#include "circle_off.bit"
#include "square_empty.bit"
#include "square_with_x.bit"

/*
 * Sound Blaster include file for ioctls and such
 */
#ifdef linux
#include <linux/soundcard.h>
#else
#include <i386at/sblast.h>
#endif

/*
 * Here's the debugging macro I use here and there, so I can turn
 * them all on or off in one place.
 */
/* #define DEBUG */
#ifdef DEBUG
#define DPRINTF(x)      printf x
#else
#define DPRINTF(x)
#endif

/*
 * Handy macros to update the mixer.
 */
#define SET_PARAMS \
  do { \
    if (ioctl (mixer_fd, MIXER_IOCTL_SET_PARAMS, &params) == -1) \
      { \
        perror ("Error setting Mixer parameters"); \
	exit (1); \
      } \
  } while (0)

#define SET_LEVELS \
  do { \
    if (ioctl (mixer_fd, MIXER_IOCTL_SET_LEVELS, &levels) == -1) \
      { \
        perror ("Error setting Mixer levels"); \
	exit (1); \
      } \
  } while (0)

/*
 * Some of these Xt names are too long for nice formatting
 */
#define MW 		XtVaCreateManagedWidget
#define PORTION(val)	(1.0 - (float)val / 15.0)
#define MK_BITMAP(bits, width, height) \
  XCreateBitmapFromData (XtDisplay (topLevel), \
			 RootWindowOfScreen (XtScreen (topLevel)),\
			 bits, width, height)
#define SCROLLBAR_RES	 XtNwidth, 23, XtNheight, 100
#define CENTER(widg)	 XtVaSetValues (widg, XtNwidth,\
					centering_width, NULL)

struct slider_info		/* Client data structure for faders */
{
  Widget other_side;
  BYTE *volume;
  FLAG *linked;
};

struct lock_info		/* Client data structure for locks */
{
  struct stereo_vol *pair;
  FLAG *linked;
};

/*
 * I'm lazy.  Everything is global today.
 */
Pixmap icon_pixmap, stereo_pixmap, mono_pixmap, locked_pixmap, unlocked_pixmap;
Pixmap circle_on_pixmap, circle_off_pixmap;
Pixmap square_empty_pixmap, square_with_x_pixmap;
Widget topLevel, quit, whole, buttons, sliders;
Widget master_l, master_r, line_l, line_r, dsp_l, dsp_r;
Widget fm_l, fm_r, cd_l, cd_r, mic_level;
Widget master_form, line_form, dsp_form, fm_form, cd_form, mic_form;
Widget master_label, line_label, dsp_label, fm_label, cd_label, mic_label;
Widget master_lock, line_lock, dsp_lock, fm_lock, cd_lock;
struct lock_info master_lock_info, line_lock_info;
struct lock_info dsp_lock_info, fm_lock_info, cd_lock_info;
Widget channels, channels_form;
Widget filters, out_filt, in_filt, filt_freq, filters_label;
Widget out_filt_label, in_filt_label;
Widget sources, line_src, mic_src, cd_src, source_label;
Widget line_src_label, mic_src_label, cd_src_label;
int centering_width;
int mixer_fd;
struct sb_mixer_params params;
struct sb_mixer_levels levels;
struct slider_info master_info_l, line_info_l, dsp_info_l, fm_info_l;
struct slider_info master_info_r, line_info_r, dsp_info_r, fm_info_r;
struct slider_info cd_info_l, cd_info_r, mic_info;
FLAG master_linked, line_linked, dsp_linked, fm_linked, cd_linked;

void install_pixmaps (void);
void sync_display (void);
void sync_channel_display (void);
void sync_filter_display (void);
void sync_source_display (void);
void Quit (Widget w, XtPointer client_data, XtPointer call_data);
void Handle_slider (Widget w, XtPointer client_data, XtPointer call_data);
void Handle_slam_slider (Widget w, XtPointer client_data, XtPointer call_data);
void Handle_toggle (Widget w, XtPointer client_data, XtPointer call_data);
void Handle_source (Widget w, XtPointer client_data, XtPointer call_data);
void Handle_lock  (Widget w, XtPointer client_data, XtPointer call_data);
void sync_lock (int flag, Widget w);
void equalize (struct stereo_vol *lev);
void sync_sliders (void);
void sync_params_display (void);

void
main (int argc, char **argv)
{
  XtAppContext    app_context;
  int scroll_sep;
  
  topLevel = XtVaAppInitialize (&app_context,
     "XMix",			/* Application class */
     NULL, 0,			/* command line option list */
     &argc, argv,		/* command line args */
     NULL,			/* for missing app-defaults file */
     NULL);			/* terminate varargs list */

  whole = MW("whole", formWidgetClass, topLevel, NULL);
  
  sliders = MW("sliders", formWidgetClass, whole, NULL);
  
  master_form = MW ("master_form", formWidgetClass, sliders, NULL);
  master_label =  MW ("master_label", labelWidgetClass, master_form,
		      XtNlabel, "Master", NULL);
  master_l = MW ("master_l", scrollbarWidgetClass, master_form, SCROLLBAR_RES,
		 XtNfromVert, master_label, NULL);
  master_r = MW ("master_r", scrollbarWidgetClass, master_form, SCROLLBAR_RES,
		 XtNfromHoriz, master_l,
		 XtNfromVert, master_label, NULL);
  master_info_l.other_side = master_r;
  master_info_r.other_side = master_l;
  master_info_l.volume = &levels.master.l;
  master_info_r.volume = &levels.master.r;
  master_info_l.linked = master_info_r.linked = &master_linked;
  master_lock = MW ("master_lock", commandWidgetClass, master_form,
		    XtNfromVert, master_l,
		    NULL);
  master_lock_info.linked = &master_linked;
  master_lock_info.pair = &levels.master;
    
  line_form = MW ("line_form", formWidgetClass, sliders,
		  XtNfromHoriz, master_form, NULL);
  line_label =  MW ("line_label", labelWidgetClass, line_form,
		    XtNlabel, "Line", NULL);
  line_l = MW ("line_l", scrollbarWidgetClass, line_form, SCROLLBAR_RES,
	       XtNfromVert, line_label, NULL);
  line_r = MW ("line_r", scrollbarWidgetClass, line_form, SCROLLBAR_RES,
	       XtNfromHoriz, line_l,
	       XtNfromVert, line_label, NULL);
  line_lock = MW ("line_lock", commandWidgetClass, line_form,
		  XtNfromVert, line_l, NULL);
  line_info_l.other_side = line_r;
  line_info_r.other_side = line_l;
  line_info_l.volume = &levels.line.l;
  line_info_r.volume = &levels.line.r;
  line_info_l.linked = line_info_r.linked = &line_linked;
  line_lock_info.linked = &line_linked;
  line_lock_info.pair = &levels.line;
  
  dsp_form = MW ("dsp_form", formWidgetClass, sliders,
		 XtNfromHoriz, line_form, NULL);
  dsp_label =  MW ("dsp_label", labelWidgetClass, dsp_form,
		   XtNlabel, "DSP", NULL);
  dsp_l = MW ("dsp_l", scrollbarWidgetClass, dsp_form, SCROLLBAR_RES,
	      XtNfromVert, dsp_label, NULL);
  dsp_r = MW ("dsp_r", scrollbarWidgetClass, dsp_form, SCROLLBAR_RES,
	      XtNfromVert, dsp_label,
	      XtNfromHoriz, dsp_l, NULL);
  dsp_lock = MW ("dsp_lock", commandWidgetClass, dsp_form,
		 XtNfromVert, dsp_l, NULL);
  dsp_info_l.other_side = dsp_r;
  dsp_info_r.other_side = dsp_l;
  dsp_info_l.volume = &levels.voc.l;
  dsp_info_r.volume = &levels.voc.r;
  dsp_info_l.linked = dsp_info_r.linked = &dsp_linked;
  dsp_lock_info.linked = &dsp_linked;
  dsp_lock_info.pair = &levels.voc;

  fm_form = MW ("fm_form", formWidgetClass, sliders,
		XtNfromHoriz, dsp_form, NULL);
  fm_label =  MW ("fm_label", labelWidgetClass, fm_form,
		  XtNlabel, "FM", NULL);
  fm_l = MW ("fm_l", scrollbarWidgetClass, fm_form, SCROLLBAR_RES,
	     XtNfromVert, fm_label, NULL);
  fm_r = MW ("fm_r", scrollbarWidgetClass, fm_form, SCROLLBAR_RES,
	     XtNfromVert, fm_label,
	     XtNfromHoriz, fm_l, NULL);
  fm_lock = MW ("fm_lock", commandWidgetClass, fm_form,
		XtNfromVert, fm_l, NULL);
  fm_info_l.other_side = fm_r;
  fm_info_r.other_side = fm_l;
  fm_info_l.volume = &levels.fm.l;
  fm_info_r.volume = &levels.fm.r;
  fm_info_l.linked = fm_info_r.linked = &fm_linked;
  fm_lock_info.linked = &fm_linked;
  fm_lock_info.pair = &levels.fm;

  cd_form = MW ("cd_form", formWidgetClass, sliders,
		XtNfromHoriz, fm_form, NULL);
  cd_label =  MW ("cd_label", labelWidgetClass, cd_form,
		  XtNlabel, "CD", NULL);
  cd_l = MW ("cd_l", scrollbarWidgetClass, cd_form, SCROLLBAR_RES,
	     XtNfromVert, cd_label, NULL);
  cd_r = MW ("cd_r", scrollbarWidgetClass, cd_form, SCROLLBAR_RES,
	     XtNfromVert, cd_label,
	     XtNfromHoriz, cd_l, NULL);
  cd_lock = MW ("cd_lock", commandWidgetClass, cd_form,
		XtNfromVert, cd_l, NULL);
  cd_info_l.other_side = cd_r;
  cd_info_r.other_side = cd_l;
  cd_info_l.volume = &levels.cd.l;
  cd_info_r.volume = &levels.cd.r;
  cd_info_l.linked = cd_info_r.linked = &cd_linked;
  cd_lock_info.linked = &cd_linked;
  cd_lock_info.pair = &levels.cd;

  mic_form = MW ("mic_form", formWidgetClass, sliders,
		 XtNfromHoriz, cd_form, NULL);
  mic_label =  MW ("mic_label", labelWidgetClass, mic_form,
		   XtNlabel, "Mic", NULL);
  mic_level = MW ("mic_level", scrollbarWidgetClass, mic_form, SCROLLBAR_RES,
		  XtNfromVert, mic_label, NULL);
  mic_info.linked = FALSE;
  mic_info.volume = &levels.mic;

  buttons = MW ("buttons", formWidgetClass, whole,
		XtNfromVert, sliders, NULL);

  channels = MW ("channels", commandWidgetClass, buttons,
		 XtNwidth, stereo_width, NULL);
  quit = MW ("quit", commandWidgetClass, buttons,
	     XtNfromVert, channels,
	     XtNlabel, "Quit",
	     XtNwidth, 50,
	     XtNheight, 20,
	     NULL);

  filters = MW ("filters", formWidgetClass, buttons,
		XtNfromHoriz, channels, NULL);
  filters_label = MW ("filters_label", labelWidgetClass, filters,
		      XtNlabel, "Filters", NULL);
  in_filt = MW ("in_filt", commandWidgetClass, filters,
		XtNfromVert, filters_label, NULL);
  in_filt_label = MW ("in_filt_label", labelWidgetClass, filters,
		      XtNlabel, "Input",
		      XtNfromHoriz, in_filt,
		      XtNfromVert, filters_label, NULL);
  out_filt = MW ("out_filt", commandWidgetClass, filters,
		 XtNfromVert, in_filt, NULL);
  out_filt_label = MW ("out_filt_label", labelWidgetClass, filters,
		       XtNfromHoriz, out_filt,
		       XtNfromVert, in_filt,
		       XtNlabel, "Output", NULL);
  filt_freq = MW ("filt_freq", commandWidgetClass, filters,
		  XtNfromVert, filters_label,
		  XtNfromHoriz, in_filt_label, NULL);

  sources = MW ("sources", formWidgetClass, buttons,
		XtNfromHoriz, filters, NULL);
  source_label = MW ("source_label", labelWidgetClass, sources,
		     XtNlabel, "Recording Source", NULL);
  line_src = MW ("line_src", commandWidgetClass, sources,
		 XtNfromVert, source_label, NULL);
  line_src_label = MW ("line_src_label", labelWidgetClass, sources,
		       XtNlabel, "Line In",
		       XtNfromVert, source_label,
		       XtNfromHoriz, line_src, NULL);
  mic_src = MW ("mic_src", commandWidgetClass, sources,
		XtNfromVert, line_src, NULL);
  mic_src_label = MW ("mic_src_label", labelWidgetClass, sources,
		      XtNlabel, "Microphone",
		      XtNfromVert, line_src,
		      XtNfromHoriz, mic_src, NULL);
  cd_src = MW ("cd_src", commandWidgetClass, sources,
	       XtNfromVert, mic_src, NULL);
  cd_src_label = MW ("cd_src_label", labelWidgetClass, sources,
		     XtNlabel, "CD",
		     XtNfromVert, mic_src,
		     XtNfromHoriz, cd_src, NULL);

  XtVaGetValues (mic_level, XtNwidth, &centering_width, NULL);
  CENTER (mic_label);
  XtVaGetValues (master_form, XtNhorizDistance, &scroll_sep, NULL);
  XtVaGetValues (master_l, XtNwidth, &centering_width, NULL);
  centering_width = centering_width * 2 + scroll_sep;
  CENTER (master_label);
  CENTER (line_label);
  CENTER (dsp_label);
  CENTER (fm_label);
  CENTER (cd_label);

  XtAddEventHandler (topLevel, EnterWindowMask, FALSE,
		     (XtEventHandler) sync_display, NULL);
  XtAddCallback (quit, XtNcallback, Quit, 0 /* client_data */ );
  XtAddCallback (master_l, XtNjumpProc, Handle_slider, &master_info_l);
  XtAddCallback (master_l, XtNscrollProc, Handle_slam_slider, 0);
  XtAddCallback (master_r, XtNjumpProc, Handle_slider, &master_info_r);
  XtAddCallback (master_r, XtNscrollProc, Handle_slam_slider, 0);
  XtAddCallback (line_l, XtNjumpProc, Handle_slider, &line_info_l);
  XtAddCallback (line_l, XtNscrollProc, Handle_slam_slider, 0);
  XtAddCallback (line_r, XtNjumpProc, Handle_slider, &line_info_r);
  XtAddCallback (line_r, XtNscrollProc, Handle_slam_slider, 0);
  XtAddCallback (dsp_l, XtNjumpProc, Handle_slider, &dsp_info_l);
  XtAddCallback (dsp_l, XtNscrollProc, Handle_slam_slider, 0);
  XtAddCallback (dsp_r, XtNjumpProc, Handle_slider, &dsp_info_r);
  XtAddCallback (dsp_r, XtNscrollProc, Handle_slam_slider, 0);
  XtAddCallback (fm_l, XtNjumpProc, Handle_slider, &fm_info_l);
  XtAddCallback (fm_l, XtNscrollProc, Handle_slam_slider, 0);
  XtAddCallback (fm_r, XtNjumpProc, Handle_slider, &fm_info_r);
  XtAddCallback (fm_r, XtNscrollProc, Handle_slam_slider, 0);
  XtAddCallback (cd_l, XtNjumpProc, Handle_slider, &cd_info_l);
  XtAddCallback (cd_l, XtNscrollProc, Handle_slam_slider, 0);
  XtAddCallback (cd_r, XtNjumpProc, Handle_slider, &cd_info_r);
  XtAddCallback (cd_r, XtNscrollProc, Handle_slam_slider, 0);
  XtAddCallback (mic_level, XtNjumpProc, Handle_slider, &mic_info);
  XtAddCallback (mic_level, XtNscrollProc, Handle_slam_slider, 0);
  XtAddCallback (channels, XtNcallback, Handle_toggle, &params.dsp_stereo);
  XtAddCallback (in_filt, XtNcallback, Handle_toggle, &params.filter_input);
  XtAddCallback (out_filt, XtNcallback, Handle_toggle, &params.filter_output);
  XtAddCallback (filt_freq, XtNcallback, Handle_toggle, &params.hifreq_filter);
  XtAddCallback (line_src, XtNcallback, Handle_source, (XtPointer)SRC_LINE);
  XtAddCallback (mic_src, XtNcallback, Handle_source, (XtPointer)SRC_MIC);
  XtAddCallback (cd_src, XtNcallback, Handle_source, (XtPointer)SRC_CD);
  XtAddCallback (master_lock, XtNcallback, Handle_lock, &master_lock_info);
  XtAddCallback (line_lock, XtNcallback, Handle_lock, &line_lock_info);
  XtAddCallback (fm_lock, XtNcallback, Handle_lock, &fm_lock_info);
  XtAddCallback (cd_lock, XtNcallback, Handle_lock, &cd_lock_info);
  XtAddCallback (dsp_lock, XtNcallback, Handle_lock, &dsp_lock_info);

  install_pixmaps ();

  /* Open the mixer device */
  #ifdef linux
  mixer_fd = open ("/dev/mixer", O_RDWR, 0);
  #else
  mixer_fd = open ("/dev/sb_mixer", O_RDWR, 0);
  #endif
  if (mixer_fd < 0)
      perror ("Error opening mixer device"), exit (1);

  /*
   * Match the display settings to the current mixer configuration.
   */
  sync_display();

  /*
   * Pick some reasonable lock settings to start with.
   * Two equal volume levels start off with that pair linked.
   */
  master_linked = (levels.master.l == levels.master.r);
  line_linked = (levels.line.l == levels.line.r);
  dsp_linked = (levels.voc.l == levels.voc.r);
  fm_linked = (levels.fm.l == levels.fm.r);
  cd_linked = (levels.cd.l == levels.cd.r);

  /*
   * Update the lock bitmaps to reflect linking
   */
  sync_lock (master_linked, master_lock);
  sync_lock (line_linked, line_lock);
  sync_lock (dsp_linked, dsp_lock);
  sync_lock (fm_linked, fm_lock);
  sync_lock (cd_linked, cd_lock);

  XtRealizeWidget (topLevel);	/* Action! */
  XtAppMainLoop (app_context);	/* Loop for events */
}

/*
 * Convert all the pixmap data into Pixmap objects.
 */
void install_pixmaps (void)
{
  /* The icon bitmap */
  icon_pixmap = MK_BITMAP (xmix_bits, xmix_width, xmix_height);
  XtVaSetValues (topLevel, XtNiconPixmap, icon_pixmap, NULL);

  stereo_pixmap = MK_BITMAP (stereo_bits, stereo_width, stereo_height);
  mono_pixmap = MK_BITMAP (mono_bits, mono_width, mono_height);
  locked_pixmap = MK_BITMAP (locked_bits, locked_width, locked_height);
  unlocked_pixmap = MK_BITMAP (unlocked_bits, unlocked_width, unlocked_height);
  circle_on_pixmap =
    MK_BITMAP (circle_on_bits, circle_on_width, circle_on_height);
  circle_off_pixmap =
    MK_BITMAP (circle_off_bits, circle_off_width, circle_off_height);
  square_empty_pixmap =
    MK_BITMAP (square_empty_bits, square_empty_width, square_empty_height);
  square_with_x_pixmap =
    MK_BITMAP (square_with_x_bits, square_with_x_width, square_with_x_height);
}

/*
 * Rescan the mixer settings and make all the indicators reflect
 * the current values.
 */
void
sync_display (void)
{
  DPRINTF(("Updating..\n"));
  if (ioctl (mixer_fd, MIXER_IOCTL_READ_LEVELS, &levels) < 0
      || ioctl (mixer_fd, MIXER_IOCTL_READ_PARAMS, &params) < 0)
    perror ("Error reading mixer settings"), exit(1);

  sync_params_display ();
  sync_source_display ();
  sync_sliders ();
}

void
sync_sliders (void)
{
  XawScrollbarSetThumb (master_l, PORTION(levels.master.l), -1.0);
  XawScrollbarSetThumb (master_r, PORTION(levels.master.r), -1.0);
  XawScrollbarSetThumb (dsp_l, PORTION(levels.voc.l), -1.0);
  XawScrollbarSetThumb (dsp_r, PORTION(levels.voc.r), -1.0);
  XawScrollbarSetThumb (fm_l, PORTION(levels.fm.l), -1.0);
  XawScrollbarSetThumb (fm_r, PORTION(levels.fm.r), -1.0);
  XawScrollbarSetThumb (line_l, PORTION(levels.line.l), -1.0);
  XawScrollbarSetThumb (line_r, PORTION(levels.line.r), -1.0);
  XawScrollbarSetThumb (cd_l, PORTION(levels.cd.l), -1.0);
  XawScrollbarSetThumb (cd_r, PORTION(levels.cd.r), -1.0);
  XawScrollbarSetThumb (mic_level, (1.0 - (float)levels.mic / 7.0), -1.0);
}

void
sync_lock (int flag, Widget w)
{
  if (flag)
      XtVaSetValues (w, XtNbitmap, locked_pixmap, NULL);
  else
      XtVaSetValues (w, XtNbitmap, unlocked_pixmap, NULL);
  CENTER (w);
}

void
Handle_lock  (Widget w, XtPointer client_data, XtPointer call_data)
{
  struct lock_info *info = client_data;
  
  *info->linked = !(*info->linked);
  if (info->linked && info->pair->l != info->pair->r)
    {
      info->pair->l = info->pair->r = ((info->pair->l + info->pair->r)/2) | 1;
      SET_LEVELS;
      sync_sliders();
    }
  sync_lock (*info->linked, w);
}

/*
 * Callback function for any Command widget that toggles bitmaps.
 * (filters and stereo switches for now.)
 */
void
Handle_toggle (Widget w, XtPointer client_data, XtPointer call_data)
{
  FLAG *flip = client_data;
  *flip = !*flip;
  SET_PARAMS;
  sync_params_display();
}

/*
 * Match the filter and stereo indicators to the mixer values.
 */
void
sync_params_display (void)
{
  XtVaSetValues (channels, XtNbitmap,
		 params.dsp_stereo ? stereo_pixmap : mono_pixmap,
		 NULL);
  XtVaSetValues (filt_freq, XtNlabel,
		 params.hifreq_filter ? "Hi    Freq  " : "Low Freq",
		 NULL);
  XtVaSetValues (in_filt, XtNbitmap,
		 params.filter_input ?
		 square_with_x_pixmap : square_empty_pixmap,
		 NULL);
  XtVaSetValues (out_filt, XtNbitmap,
		 params.filter_output ?
		 square_with_x_pixmap : square_empty_pixmap,
		 NULL);
}

/*
 * Callback for selecting a new source.
 * Basically, we set the mixer to the new source, and then
 * let sync_source_display handle the visual feedback.
 * (Easier than radioGroups.)
 */
void
Handle_source (Widget w, XtPointer client_data, XtPointer call_data)
{
  params.record_source = (int) client_data;
  SET_PARAMS;
  sync_source_display ();
}

/*
 * Match the shown button to the mixer's idea of the recording source.
 */
void
sync_source_display (void)
{
  switch (params.record_source)
    {
    case SRC_MIC:
      XtVaSetValues (mic_src, XtNbitmap, circle_on_pixmap, NULL);
      XtVaSetValues (cd_src, XtNbitmap, circle_off_pixmap, NULL);
      XtVaSetValues (line_src, XtNbitmap, circle_off_pixmap, NULL);
      break;
    case SRC_CD:
      XtVaSetValues (mic_src, XtNbitmap, circle_off_pixmap, NULL);
      XtVaSetValues (cd_src, XtNbitmap, circle_on_pixmap, NULL);
      XtVaSetValues (line_src, XtNbitmap, circle_off_pixmap, NULL);
      break;
    case SRC_LINE:
      XtVaSetValues (mic_src, XtNbitmap, circle_off_pixmap, NULL);
      XtVaSetValues (cd_src, XtNbitmap, circle_off_pixmap, NULL);
      XtVaSetValues (line_src, XtNbitmap, circle_on_pixmap, NULL);
      break;
    default:
      fprintf (stderr, "Invalid recording source!\n");
      exit(1);
    }
}

/*
 * Quit button callback function
 */
void 
  Quit (Widget w, XtPointer client_data, XtPointer call_data)
{
  DPRINTF (("Exiting...\n"));
  exit (0);
}

/*
 * This is for pushing a slider to MAX or MIN position.
 * Of questionable utility, yes..
 */
void
Handle_slam_slider (Widget w, XtPointer client_data, XtPointer call_data)
{
  float pos;
  
  pos = (float)((int)call_data < 0);
  XtCallCallbacks (w, XtNjumpProc, &pos);
}

/*
 * XtNjumpProc callback for volume fader scrollbar widgets.
 * Great pains are taken to make the slider accurately reflect
 * the granular mixer setting, without actually querying the mixer
 * device.  (This is faster.)
 */
void
Handle_slider (Widget w, XtPointer client_data, XtPointer call_data)
{
  int val;
  struct slider_info *info = client_data;
  static FLAG child = 0;

  /*
   * The mixer value is 0-15, with the low bit always set. (stupid)
   */
  DPRINTF (("Got %f\n", *(float*)call_data));
  val = (int)(.5 + (15.0 * (1.0 - *(float*)call_data))) | 1;

  /*
   * (Microphone level is even worse: 0-7, with the low bit set.)
   * Shove the Thumb to the granular value that we will actually get.
   */
  if (w == mic_level)
    {
      val = (val / 2) | 1;
      XawScrollbarSetThumb (w, (1.0 - (float)val / 7.0), -1.0);
    }
  else
    XawScrollbarSetThumb (w, PORTION(val), -1.0);
    

  /*
   * If we are linked to the other channel, and we are the first
   * to do this, then call the other side's callback giving and
   * identical thumb position.
   */
  if (info->linked && *info->linked == TRUE)
    {
      if (!child)
	{
	  child = TRUE;
	  XtCallCallbacks (info->other_side, XtNjumpProc, call_data);
	  child = FALSE;
	}
    }

  /*
   * If the new value is at a different notch than the current setting,
   * then inform the mixer, and adopt the setting.
   * Otherwise, don't waste time setting the mixer.
   */
  if (*info->volume != val)
    {
      *info->volume = val;
      SET_LEVELS;
    }
}
