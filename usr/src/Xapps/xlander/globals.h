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

#ifndef _globals_h_
#define _globals_h_

extern Display *d;
extern Window viewWin;
extern Window instrWin;
extern GC gcView;
extern GC gcInstr;
extern GC gcPanel;
extern GC gcXor;
extern Pixmap buffer;
extern Pixmap instrBuffer;
extern XFontStruct *font;
extern XrmDatabase resources;

extern int px, py, pz;
extern int roll, pitch, yaw;
extern float acceleration;

#endif _globals_h_
