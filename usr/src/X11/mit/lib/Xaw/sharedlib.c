/*
 * $XConsortium: sharedlib.c,v 1.1 91/06/29 12:40:18 rws Exp $
 * 
 * Copyright 1991 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 */

#if defined(SUNSHLIB) && !defined(SHAREDCODE)

#include <X11/IntrinsicP.h>
#include <X11/Xaw/AsciiSinkP.h>
#include <X11/Xaw/AsciiSrcP.h>
#include <X11/Xaw/AsciiTextP.h>
#include <X11/Xaw/BoxP.h>
#include <X11/Xaw/ClockP.h>
#include <X11/Xaw/CommandP.h>
#include <X11/Xaw/DialogP.h>
#include <X11/Xaw/FormP.h>
#include <X11/Xaw/GripP.h>
#include <X11/Xaw/LabelP.h>
#include <X11/Xaw/ListP.h>
#include <X11/Xaw/LogoP.h>
#include <X11/Xaw/MailboxP.h>
#include <X11/Xaw/MenuButtoP.h>
#include <X11/Xaw/PanedP.h>
#include <X11/Xaw/PannerP.h>
#include <X11/Xaw/PortholeP.h>
#include <X11/Xaw/RepeaterP.h>
#include <X11/Xaw/ScrollbarP.h>
#include <X11/Xaw/SimpleP.h>
#include <X11/Xaw/SimpleMenP.h>
#include <X11/Xaw/SmeP.h>
#include <X11/Xaw/SmeBSBP.h>
#include <X11/Xaw/SmeLineP.h>
#include <X11/Xaw/StripCharP.h>
#include <X11/Xaw/TextP.h>
#include <X11/Xaw/TextSinkP.h>
#include <X11/Xaw/TextSrcP.h>
#include <X11/Xaw/ToggleP.h>
#include <X11/Xaw/TreeP.h>
#include <X11/VendorP.h>
#include <X11/Xaw/ViewportP.h>

extern AsciiSinkClassRec asciiSinkClassRec;
WidgetClass asciiSinkObjectClass = (WidgetClass)&asciiSinkClassRec;

extern AsciiSrcClassRec asciiSrcClassRec;
WidgetClass asciiSrcObjectClass = (WidgetClass)&asciiSrcClassRec;

extern AsciiTextClassRec asciiTextClassRec;
WidgetClass asciiTextWidgetClass = (WidgetClass)&asciiTextClassRec;

#ifdef ASCII_STRING
extern AsciiStringClassRec asciiStringClassRec;
WidgetClass asciiStringWidgetClass = (WidgetClass)&asciiStringClassRec;
#endif

#ifdef ASCII_DISK
extern AsciiDiskClassRec asciiDiskClassRec;
WidgetClass asciiDiskWidgetClass = (WidgetClass)&asciiDiskClassRec;
#endif

extern BoxClassRec boxClassRec;
WidgetClass boxWidgetClass = (WidgetClass)&boxClassRec;

extern ClockClassRec clockClassRec;
WidgetClass clockWidgetClass = (WidgetClass) &clockClassRec;

extern CommandClassRec commandClassRec;
WidgetClass commandWidgetClass = (WidgetClass) &commandClassRec;

extern DialogClassRec dialogClassRec;
WidgetClass dialogWidgetClass = (WidgetClass)&dialogClassRec;

extern FormClassRec formClassRec;
WidgetClass formWidgetClass = (WidgetClass)&formClassRec;

extern GripClassRec gripClassRec;
WidgetClass gripWidgetClass = (WidgetClass) &gripClassRec;

extern LabelClassRec labelClassRec;
WidgetClass labelWidgetClass = (WidgetClass)&labelClassRec;

extern ListClassRec listClassRec;
WidgetClass listWidgetClass = (WidgetClass)&listClassRec;

extern LogoClassRec logoClassRec;
WidgetClass logoWidgetClass = (WidgetClass) &logoClassRec;

extern MailboxClassRec mailboxClassRec;
WidgetClass mailboxWidgetClass = (WidgetClass) &mailboxClassRec;

extern MenuButtonClassRec menuButtonClassRec;
WidgetClass menuButtonWidgetClass = (WidgetClass) &menuButtonClassRec;

extern PanedClassRec panedClassRec;
WidgetClass panedWidgetClass = (WidgetClass) &panedClassRec;
WidgetClass vPanedWidgetClass = (WidgetClass) &panedClassRec;

extern PannerClassRec pannerClassRec;
WidgetClass pannerWidgetClass = (WidgetClass) &pannerClassRec;

extern PortholeClassRec portholeClassRec;
WidgetClass portholeWidgetClass = (WidgetClass) &portholeClassRec;

extern RepeaterClassRec repeaterClassRec;
WidgetClass repeaterWidgetClass = (WidgetClass) &repeaterClassRec;

extern ScrollbarClassRec scrollbarClassRec;
WidgetClass scrollbarWidgetClass = (WidgetClass)&scrollbarClassRec;

extern SimpleClassRec simpleClassRec;
WidgetClass simpleWidgetClass = (WidgetClass)&simpleClassRec;

extern SimpleMenuClassRec simpleMenuClassRec;
WidgetClass simpleMenuWidgetClass = (WidgetClass)&simpleMenuClassRec;

extern SmeClassRec smeClassRec;
WidgetClass smeObjectClass = (WidgetClass) &smeClassRec;

extern SmeBSBClassRec smeBSBClassRec;
WidgetClass smeBSBObjectClass = (WidgetClass) &smeBSBClassRec;

extern SmeLineClassRec smeLineClassRec;
WidgetClass smeLineObjectClass = (WidgetClass) &smeLineClassRec;

extern StripChartClassRec stripChartClassRec;
WidgetClass stripChartWidgetClass = (WidgetClass) &stripChartClassRec;

extern TextClassRec textClassRec;
WidgetClass textWidgetClass = (WidgetClass)&textClassRec;

unsigned long FMT8BIT = 0L;

extern TextSinkClassRec textSinkClassRec;
WidgetClass textSinkObjectClass = (WidgetClass)&textSinkClassRec;

extern TextSrcClassRec textSrcClassRec;
WidgetClass textSrcObjectClass = (WidgetClass)&textSrcClassRec;

extern ToggleClassRec toggleClassRec;
WidgetClass toggleWidgetClass = (WidgetClass) &toggleClassRec;

extern TreeClassRec treeClassRec;
WidgetClass treeWidgetClass = (WidgetClass) &treeClassRec;

extern VendorShellClassRec vendorShellClassRec;
WidgetClass vendorShellWidgetClass = (WidgetClass) &vendorShellClassRec;

extern ViewportClassRec viewportClassRec;
WidgetClass viewportWidgetClass = (WidgetClass)&viewportClassRec;

#endif /* SUNSHLIB */
