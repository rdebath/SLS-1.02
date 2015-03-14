/****************************************************************************/
/* Module FmBitmaps                                                         */
/*                                                                          */
/* Functions & data for handling the bitmaps and cursors.                   */
/****************************************************************************/

#include <X11/Intrinsic.h>

#include "bitmaps/l_line"
#include "bitmaps/t_line"
#include "bitmaps/f_line"
#include "bitmaps/c_line"
#include "bitmaps/l_arrow"
#include "bitmaps/r_arrow"
#include "bitmaps/dir_def"
#include "bitmaps/file_def"
#include "bitmaps/command_def"
#include "bitmaps/no_entry"
#include "bitmaps/xfm_icon"
#include "bitmaps/tick_bm"
#include "bitmaps/no_tick"
#include "bitmaps/files_def"
#include "bitmaps/exclamation"
#include "bitmaps/file_mask"
#include "bitmaps/dir_mask"
#include "bitmaps/command_mask"
#include "bitmaps/wavy_arrow"
#include "bitmaps/symlink"
#include "bitmaps/watch"
#include "bitmaps/watchmsk"
#include "bitmaps/dirlink"
#include "bitmaps/execlink"
#include "bitmaps/app_default"
#include "bitmaps/appmgr"

#include "Am.h"
#include "Fm.h"

/****************************************************************************/
/*                           STATIC DATA                                    */
/****************************************************************************/

typedef struct {
  char *bits;
  int width, height;
} BitmapRec;

static BitmapRec bitmaps[] = {
  { l_line_bits, l_line_width, l_line_height },
  { t_line_bits, t_line_width, t_line_height },
  { f_line_bits, f_line_width, f_line_height },
  { c_line_bits, c_line_width, c_line_height },
  { l_arrow_bits, l_arrow_width, l_arrow_height },
  { r_arrow_bits, r_arrow_width, r_arrow_height },
  { dir_def_bits, dir_def_width, dir_def_height },
  { file_def_bits, file_def_width, file_def_height },
  { no_entry_bits, no_entry_width, no_entry_height  },
  { xfm_icon_bits, xfm_icon_width, xfm_icon_height },
  { tick_bm_bits, tick_bm_width, tick_bm_height },
  { no_tick_bits, no_tick_width, no_tick_height },
  { command_def_bits, command_def_width, command_def_height },
  { files_def_bits, files_def_width, files_def_height },
  { exclamation_bits, exclamation_width, exclamation_height },
  { file_mask_bits, file_mask_width, file_mask_height },
  { dir_mask_bits, dir_mask_width, dir_mask_height },
  { command_mask_bits, command_mask_width, command_mask_height },
  { wavy_arrow_bits, wavy_arrow_width, wavy_arrow_height },
  { symlink_bits, symlink_width, symlink_height },
  { watch_bits, watch_width, watch_height },
  { watchmsk_bits, watchmsk_width, watchmsk_height },
  { dirlink_bits, dirlink_width, dirlink_height },
  { execlink_bits, execlink_width, execlink_height },
  { app_default_bits, app_default_width, app_default_height },
  { appmgr_bits, appmgr_width, appmgr_height },
};

typedef struct {
  int source, mask;
} CursorRec;

static CursorRec cursors[] = {
  { FILE_BM, FILEMSK_BM },
  { FILES_BM, FILES_BM },
  { NOENTRY_BM, NOENTRY_BM },
  { DIR_BM, DIRMSK_BM },
  { COMMAND_BM, COMMANDMSK_BM },
  { WATCH_BM, WATCHMSK_BM }
};

/****************************************************************************/
/*                           PUBLIC DATA                                    */
/****************************************************************************/

Pixmap *bm;
Cursor *curs;

/****************************************************************************/
/*                         PUBLIC FUNCTIONS                                 */
/****************************************************************************/

void readBitmaps()
{
  register int i;
  Display *dpy;
  int scrn;
  Colormap cmp;
  Window win;
  XColor black, white;

  dpy = XtDisplay(aw.shell);
  win = DefaultRootWindow(dpy);
  scrn = DefaultScreen(dpy);
  cmp = DefaultColormap(dpy, scrn);

  black.pixel = BlackPixel(dpy, scrn);
  XQueryColor(dpy, cmp, &black);
  white.pixel = WhitePixel(dpy, scrn);
  XQueryColor(dpy, cmp, &white);

  bm = (Pixmap *) XtMalloc(XtNumber(bitmaps) * sizeof(Pixmap *));
  curs = (Cursor *) XtMalloc(XtNumber(cursors) * sizeof(Cursor *));

  /* create the Pixmaps needed */
  for (i=0; i<XtNumber(bitmaps); i++)
    bm[i] = XCreateBitmapFromData(dpy, win, bitmaps[i].bits,
				  bitmaps[i].width, bitmaps[i].height);

  /* create the Cursors needed */
  for (i=0; i<XtNumber(cursors); i++)
    curs[i] = XCreatePixmapCursor(dpy, bm[cursors[i].source], 
				  bm[cursors[i].mask], &black, &white, 16, 16);
}

/*----------------------------------------------------------------------------*/

void freeBitmaps()
{
  register int i;
  Display *dpy;

  dpy = XtDisplay(aw.shell);

  for (i=0; i<XtNumber(bitmaps); i++)
    XFreePixmap(dpy, bm[i]);

  for (i=0; i<XtNumber(cursors); i++)
    XFreeCursor(dpy, curs[i]);
}




