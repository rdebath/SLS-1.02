/*
 *      (c) Copyright 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)resources.h	26.16	91/09/14 SMI"

#ifndef _OLWM_RESOURCES_H
#define _OLWM_RESOURCES_H

#include <X11/Xresource.h>

/* maximum length of fully-specified instance/class name */
#define	MAX_NAME	100
#define	MAX_CLASS	100

#define BASICPTR	0
#define MOVEPTR		1
#define BUSYPTR		2
#define ICONPTR		3
#define RESIZEPTR	4
#define MENUPTR		5
#define QUESTIONPTR	6
#define TARGETPTR	7
#define PANPTR		8
#define NUM_CURSORS	9		/* number of pointers supported */

extern XrmQuark TopClassQ;
extern XrmQuark TopInstanceQ;
extern XrmQuark OpenWinQ;
extern XrmDatabase OlwmDB;

extern void InitGlobals();
extern void UpdateGlobals();

#ifdef OW_I18N_L3
extern void EffectOLLC();
#endif /* OW_I18N_L3 */

#endif /* _OLWM_RESOURCES_H */
