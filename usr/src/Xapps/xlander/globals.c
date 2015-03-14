/******************************************************************************
** XLander - A three-dimensional view-oriented lunar landing simulation for X
**
** Authors:
** Paul Riddle (paulr@umbc3.umbc.edu)
** Mike Friedman (mikef@umbc3.umbc.edu)
**
** University of Maryland, Baltimore Campus
**
** This program may be freely distributed in any form, providing the authors'
** names stay with it.  If you use any portion of this code, please give us
** credit.  Let us know if you like it!
******************************************************************************/

#include "xlander.h"

Display *d;                /* Connection to X server */
Window viewWin;            /* Main game window */
Window instrWin;           /* Instrument / Control panel window */
GC gcView;                 /* Graphics context for drawing in window */
GC gcInstr;                /* Graphics context for instruments window */
GC gcPanel;                /* Graphics context for instrument window */
GC gcXor;
Pixmap buffer;             /* Background buffer for updating view */
Pixmap instrBuffer;        /* Background buffer for updating instruments */
XFontStruct *font;
XrmDatabase resources = (XrmDatabase) 0;
                           /* X Resource database */
float acceleration = 0.0;  /* Acceleration due to gravity */
