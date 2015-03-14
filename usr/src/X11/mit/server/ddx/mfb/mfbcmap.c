/***********************************************************
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

******************************************************************/
/* $XConsortium: mfbcmap.c,v 5.3 89/07/19 15:48:00 rws Exp $ */
#include "X.h"
#include "scrnintstr.h"
#include "colormapst.h"
#include "resource.h"

extern int	TellLostMap(), TellGainedMap();
/* A monochrome frame buffer is a static gray colormap with two entries.
 * We have a "required list" of length 1.  Because we can only support 1
 * colormap, we never have to change it, but we may have to change the 
 * name we call it.  If someone installs a new colormap, we know it must
 * look just like the old one (because we've checked in dispatch that it was
 * a valid colormap identifier, and all the colormap IDs for this device
 * look the same).  Nevertheless, we still have to uninstall the old colormap
 * and install the new one.  Similarly, if someone uninstalls a colormap,
 * we have to install the default map, even though we know those two looked
 * alike.  
 * The required list concept is pretty much irrelevant when you can only
 * have one map installed at a time.  
 */
static ColormapPtr InstalledMaps[MAXSCREENS];

int
mfbListInstalledColormaps(pScreen, pmaps)
    ScreenPtr	pScreen;
    Colormap	*pmaps;
{
    /* By the time we are processing requests, we can guarantee that there
     * is always a colormap installed */
    *pmaps = InstalledMaps[pScreen->myNum]->mid;
    return (1);
}


void
mfbInstallColormap(pmap)
    ColormapPtr	pmap;
{
    int index = pmap->pScreen->myNum;
    ColormapPtr oldpmap = InstalledMaps[index];

    if(pmap != oldpmap)
    {
	/* Uninstall pInstalledMap. No hardware changes required, just
	 * notify all interested parties. */
	if(oldpmap != (ColormapPtr)None)
	    WalkTree(pmap->pScreen, TellLostMap, (pointer)&oldpmap->mid);
	/* Install pmap */
	InstalledMaps[index] = pmap;
	WalkTree(pmap->pScreen, TellGainedMap, (pointer)&pmap->mid);

    }
}

void
mfbUninstallColormap(pmap)
    ColormapPtr	pmap;
{
    int index = pmap->pScreen->myNum;
    ColormapPtr curpmap = InstalledMaps[index];

    if(pmap == curpmap)
    {
	if (pmap->mid != pmap->pScreen->defColormap)
	{
	    curpmap = (ColormapPtr) LookupIDByType(pmap->pScreen->defColormap,
						   RT_COLORMAP);
	    (*pmap->pScreen->InstallColormap)(curpmap);
	}
    }
}

/*ARGSUSED*/
void
mfbResolveColor (pred, pgreen, pblue, pVisual)
    unsigned short	*pred;
    unsigned short	*pgreen;
    unsigned short	*pblue;
    VisualPtr		pVisual;
{
    /* 
     * Gets intensity from RGB.  If intensity is >= half, pick white, else
     * pick black.  This may well be more trouble than it's worth.
     */
    *pred = *pgreen = *pblue = 
        (((30L * *pred +
           59L * *pgreen +
           11L * *pblue) >> 8) >= (((1<<8)-1)*50)) ? ~0 : 0;
}

Bool
mfbCreateColormap(pMap)
    ColormapPtr	pMap;
{
    ScreenPtr	pScreen;
    unsigned short  red0, green0, blue0;
    unsigned short  red1, green1, blue1;
    unsigned long   pix;
    
    pScreen = pMap->pScreen;
    if (pScreen->whitePixel == 0)
    {
	red0 = green0 = blue0 = ~0;
	red1 = green1 = blue1 = 0;
    }
    else
    {
	red0 = green0 = blue0 = 0;
	red1 = green1 = blue1 = ~0;
    }

    /* this is a monochrome colormap, it only has two entries, just fill
     * them in by hand.  If it were a more complex static map, it would be
     * worth writing a for loop or three to initialize it */

    /* this will be pixel 0 */
    pix = 0;
    if (AllocColor(pMap, &red0, &green0, &blue0, &pix, 0) != Success)
	return FALSE;

    /* this will be pixel 1 */
    if (AllocColor(pMap, &red1, &green1, &blue1, &pix, 0) != Success)
	return FALSE;
    return TRUE;
}

/*ARGSUSED*/
void
mfbDestroyColormap (pMap)
    ColormapPtr	pMap;
{
    return;
}

Bool
mfbCreateDefColormap (pScreen)
    ScreenPtr	pScreen;
{
    VisualPtr	pVisual;
    ColormapPtr	pColormap;
    
    for (pVisual = pScreen->visuals;
	 pVisual->vid != pScreen->rootVisual;
	 pVisual++)
	;
    if (CreateColormap (pScreen->defColormap, pScreen, pVisual,
			&pColormap, AllocNone, 0) != Success)
    {
	return FALSE;
    }
    (*pScreen->InstallColormap) (pColormap);
    return TRUE;
}
