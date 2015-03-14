/*
 * xman - X window system manual page display program.
 *
 * $XConsortium: ScrollByL.h,v 1.6 91/07/12 15:49:38 dave Exp $
 * $Athena: ScrollByL.h,v 4.0 88/08/31 22:11:16 kit Exp $
 *
 * Copyright 1987, 1988 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * Author:    Chris D. Peterson, MIT Project Athena
 * Created:   December 5, 1987
 */

#ifndef _XtScrollByLine_h
#define _XtScrollByLine_h

/***********************************************************************
 *
 * ScrollByLine Widget (subclass of Simple)
 *
 ***********************************************************************/

/*
 * The default fonts.
 */

#ifdef ATHENA
#define MANPAGE_NORMAL   "fixed"
#define MANPAGE_BOLD     "helvetica-bold12"
#define MANPAGE_ITALIC   "helvetica-boldoblique12"
#define MANPAGE_SYMBOL   "symbol-medium12"
#else
#define MANPAGE_NORMAL   "*-new century schoolbook-medium-r-normal--*-120-*"
#define MANPAGE_BOLD     "*-new century schoolbook-bold-r-normal--*-120-*"
#define MANPAGE_ITALIC   "*-new century schoolbook-bold-i-normal--*-120-*"
#define MANPAGE_SYMBOL   "*-symbol-medium-r-normal--*-120-*"
#endif /* ATHENA */

#define XtNindent           "indent"
#define XtNforceVert        "forceVert"
#define XtNmanualFontNormal "manualFontNormal"
#define XtNmanualFontBold   "manualFontBold"
#define XtNmanualFontItalic "manualFontItalic"
#define XtNmanualFontSymbol "manualFontSymbol"

#define XtCIndent           "Indent"

/* Class record constants */

extern WidgetClass scrollByLineWidgetClass;

typedef struct _ScrollByLineClassRec *ScrollByLineWidgetClass;
typedef struct _ScrollByLineRec      *ScrollByLineWidget;

#endif /* _XtScrollByLine_h --- DON'T ADD STUFF AFTER THIS LINE */
