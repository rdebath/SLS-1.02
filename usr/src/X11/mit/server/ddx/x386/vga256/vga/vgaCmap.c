/* $Header: /home/x_cvs/mit/server/ddx/x386/vga256/vga/vgaCmap.c,v 1.5 1992/08/29 11:14:30 dawes Exp $ */
/*
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
 *
 * $Header: /proj/X11/mit/server/ddx/x386/vga/RCS/vgaCmap.c,v 1.2 1991/06/27 00:03:01 root Exp $
 */

#include "X.h"
#include "Xproto.h"
#include "scrnintstr.h"
#include "colormapst.h"
#include "windowstr.h"
#include "compiler.h"

#include "vga.h"

#define NOMAPYET        (ColormapPtr) 0

static ColormapPtr InstalledMaps[MAXSCREENS];
				/* current colormap for each screen */

int
vgaListInstalledColormaps(pScreen, pmaps)
     ScreenPtr	pScreen;
     Colormap	*pmaps;
{
  /* By the time we are processing requests, we can guarantee that there
   * is always a colormap installed */
  
  *pmaps = InstalledMaps[pScreen->myNum]->mid;
  return(1);
}


void
vgaStoreColors(pmap, ndef, pdefs)
     ColormapPtr	pmap;
     int		ndef;
     xColorItem	        *pdefs;
{
  int		i;
  unsigned char *cmap;
  xColorItem	directDefs[256];

  if (pmap != InstalledMaps[pmap->pScreen->myNum])
    return;

  if ((pmap->pVisual->class | DynamicClass) == DirectColor)
    {
      ndef = cfbExpandDirectColors (pmap, ndef, pdefs, directDefs);
      pdefs = directDefs;
    }

  for(i = 0; i < ndef; i++)
    {
      cmap = &((vgaHWPtr)vgaNewVideoState)->DAC[pdefs[i].pixel*3];
      cmap[0] = pdefs[i].red   >> 10;
      cmap[1] = pdefs[i].green >> 10;
      cmap[2] = pdefs[i].blue  >> 10;
      
      if (x386VTSema)
	{
	  outb(0x3C8, pdefs[i].pixel);
	  outb(0x3C9, cmap[0]);
	  outb(0x3C9, cmap[1]);
	  outb(0x3C9, cmap[2]);
	}
    }	
}


void
vgaInstallColormap(pmap)
     ColormapPtr	pmap;
{
  ColormapPtr oldmap = InstalledMaps[pmap->pScreen->myNum];
  int         entries;
  Pixel *     ppix;
  xrgb *      prgb;
  xColorItem *defs;
  int         i;


  if (pmap == oldmap)
    return;

  if ((pmap->pVisual->class | DynamicClass) == DirectColor)
    entries = (pmap->pVisual->redMask |
	       pmap->pVisual->greenMask |
	       pmap->pVisual->blueMask) + 1;
  else
    entries = pmap->pVisual->ColormapEntries;

  ppix = (Pixel *)ALLOCATE_LOCAL( entries * sizeof(Pixel));
  prgb = (xrgb *)ALLOCATE_LOCAL( entries * sizeof(xrgb));
  defs = (xColorItem *)ALLOCATE_LOCAL(entries * sizeof(xColorItem));

  if ( oldmap != NOMAPYET)
    WalkTree( pmap->pScreen, TellLostMap, &oldmap->mid);

  InstalledMaps[pmap->pScreen->myNum] = pmap;

  for ( i=0; i<entries; i++) ppix[i] = i;

  QueryColors( pmap, entries, ppix, prgb);

  for ( i=0; i<entries; i++) /* convert xrgbs to xColorItems */
    {
      defs[i].pixel = ppix[i];
      defs[i].red = prgb[i].red;
      defs[i].green = prgb[i].green;
      defs[i].blue = prgb[i].blue;
      defs[i].flags =  DoRed|DoGreen|DoBlue;
    }

  vgaStoreColors( pmap, entries, defs);

  WalkTree(pmap->pScreen, TellGainedMap, &pmap->mid);
  
  DEALLOCATE_LOCAL(ppix);
  DEALLOCATE_LOCAL(prgb);
  DEALLOCATE_LOCAL(defs);
}


void
vgaUninstallColormap(pmap)
     ColormapPtr pmap;
{
  ColormapPtr defColormap;
  
  if ( pmap != InstalledMaps[pmap->pScreen->myNum] )
    return;

  defColormap = (ColormapPtr) LookupIDByType( pmap->pScreen->defColormap,
					      RT_COLORMAP);

  if (defColormap == InstalledMaps[pmap->pScreen->myNum])
    return;

  (*pmap->pScreen->InstallColormap) (defColormap);
}

