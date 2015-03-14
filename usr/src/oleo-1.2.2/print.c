/*	Copyright (C) 1992, 1993 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "display.h"
#include <stdio.h>
#include "font.h"
#include "global.h"

static void 
put_eps_header (dpy, scale, wid, hgt, fp)
     struct display *dpy;
     float scale, wid, hgt;
     FILE *fp;
{
  int dpy_wid = display_width (dpy);
  int dpy_hgt = display_height (dpy);
  struct font_memo *fm;
  fputs ("%!PS-Adobe-2.0 EPSF-2.0\n", fp);
  fprintf (fp, "%%%%BoundingBox: %d %d %d %d\n",
	   0, 0,
	   (int) ((float) dpy_wid * scale),
	   (int) ((float) (dpy_hgt + 1) * scale));
  fputs ("%%Creator: oleo\n", fp);
  fputs ("%%DocumentFonts: Times-Roman", fp);
  for (fm = font_list; fm; fm = fm->next)
    fprintf (fp, " %s", fm->psname);
  fputc ('\n', fp);
  {
    time_t the_record_of_the_time = time (0);
    fprintf (fp, "%%%%CreationDate: %s\n", ctime (&the_record_of_the_time));
  }
  fputs ("%%EndComments\n", fp);
}

void 
put_ps_string (str, fp)
     char *str;
     FILE *fp;
{
  fputc ('(', fp);
  while (*str)
    {
      if (*str == ')')
	fputs ("\\)", fp);
      else if (*str == '(')
	fputs ("\\(", fp);
      else
	fputc (*str, fp);
      ++str;
    }
  fputc (')', fp);
}

#ifdef __STDC__
void 
psprint_region (FILE * fp, struct rng * rng, float wid, float hgt, char * font)
#else
void 
psprint_region (fp, rng, wid, hgt, font)
     FILE *fp;
     struct rng *rng;
     float wid, hgt;
     char * font;
#endif
{
  struct display dpy;
  int rows = rng->hr - rng->lr + 1;
  int cols = rng->hc - rng->lc + 1;
  int dpy_wid;
  int dpy_hgt;
  float scale;

  build_unscaled_display (&dpy, rng, 0, 0);
  dpy_wid = display_width (&dpy);
  dpy_hgt = display_height (&dpy);
  scale = wid / (float) dpy_wid;
  if (scale * dpy_hgt > hgt)
    scale = hgt / (float) dpy_hgt;

  put_eps_header (&dpy, scale, wid, hgt, fp);
  fprintf (fp, "/min { 2 copy lt { pop } if } def\n");
  fprintf (fp, "/max { 2 copy lt { exch pop } if } def\n");
  fprintf (fp, "/fed { exch def } def\n");
  fprintf (fp, "/gget { cvi get } def\n");
  fprintf (fp, "%% Some basic facts about the region being printed. \n");
  fprintf (fp, "/CharsWide %d def\n", dpy_wid);
  fprintf (fp, "/CharsTall %d def\n", dpy_hgt);
  fprintf (fp, "/Cols	   %d def\n", cols);
  fprintf (fp, "/Rows      %d def\n", rows);
  fprintf (fp, "%% The scale is just the point size of the default font. \n");
  fprintf (fp, "%% The value here is just an initial approximation.  The real value\n");
  fprintf (fp, "%% is computed below.\n");
  fprintf (fp, "/Scale     %f def\n", scale);
  fprintf (fp, "/PointsWide CharsWide Scale mul def\n");
  fprintf (fp, "/PointsTall CharsTall Scale mul def\n");
  fprintf (fp, "\n");
  fprintf (fp, "%% font setup\n");
  fprintf (fp, "/pair-sub \n");
  fprintf (fp, "{\n");
  fprintf (fp, "  3 -1 roll exch sub 3 1 roll sub exch\n");
  fprintf (fp, "} def\n");
  fprintf (fp, "/font-box\n");
  fprintf (fp, "{\n");
  fprintf (fp, "  dup /FontBBox get /font-box-box fed\n");
  fprintf (fp, "  /FontMatrix get /font-box-matrix fed\n");
  fprintf (fp, "  font-box-box font-box-matrix transform\n");
  fprintf (fp, "  4 2 roll font-box-matrix transform\n");
  fprintf (fp, "  pair-sub  \n");
  fprintf (fp, "} def\n");
  fprintf (fp, "/DefaultFontName /%s def\n", default_font()->psname);
  fprintf (fp, "/BasicDefaultFont DefaultFontName findfont def\n");
  fprintf (fp, "/DefaultFontSize Scale round def\n");
  fprintf (fp, "/SizeDir 1 def\n");
  fprintf (fp, "{ \n");
  fprintf (fp, "  DefaultFontSize 4 le { exit } if\n");
  fprintf (fp, "  /DefaultFont BasicDefaultFont DefaultFontSize scalefont def\n");
  fprintf (fp, "  DefaultFont setfont \n");
  fprintf (fp, "  DefaultFont font-box\n");
  fprintf (fp, "  Scale le exch Scale le and not\n");
  fprintf (fp, "  { /SizeDir -1 def } { SizeDir 0 le { exit } if } ifelse\n");
  fprintf (fp, "  /DefaultFontSize DefaultFontSize SizeDir add def\n");
  fprintf (fp, "} loop\n");
  fprintf (fp, "/DefaultFontSize DefaultFontSize def\n");
  fprintf (fp, "/DefaultFont BasicDefaultFont DefaultFontSize scalefont def\n");
  fprintf (fp, "/Scale DefaultFontSize def\n");
  fprintf (fp, "\n");
  fprintf (fp, "DefaultFont font-box\n");
  fprintf (fp, "/Scaleh fed\n");
  fprintf (fp, "/Scalew fed\n");
  fprintf (fp, "\n");
  fprintf (fp, "/CellsUsed Rows Cols mul array def\n");
  fprintf (fp, "0 1 Rows Cols mul 1 sub { CellsUsed exch false put } for\n");
  fprintf (fp, "\n");
  fprintf (fp, "/checkused   %% r c -- bool\n");
  fprintf (fp, "{\n");
  fprintf (fp, "  exch Cols mul add CellsUsed exch gget\n");
  fprintf (fp, "} def\n");
  fprintf (fp, "\n");
  fprintf (fp, "/setused   %% r c -- \n");
  fprintf (fp, "{\n");
  fprintf (fp, "  exch Cols mul add CellsUsed exch true put\n");
  fprintf (fp, "} def\n");
  fprintf (fp, "\n");
  {
    int ri, ci;
    for (ri = 0; ri < rows; ++ri)
      for (ci = 0; ci < cols; ++ci)
	{
	  struct cell_display * cd = dpy.cells + ri * cols + ci;
	  if (cd->unclipped)
	    fprintf (fp, "%d %d setused\n", ri, ci);
	}
    fprintf (fp, "/Rowy [ ");
    for (ri = 0; ri < rows; ++ri)
      fprintf (fp, "%d ", dpy.rowy[ri]);
    fprintf (fp, "] def\n/Colx [ ");
    for (ci = 0; ci < cols; ++ci)
      fprintf (fp, "%d ", dpy.colx[ci]);
    fprintf (fp, "] def\n");
    fprintf (fp, "/Heights [ ");
    for (ri = 0; ri < rows; ++ri)
      fprintf (fp, "%d ", dpy.heights[ri]);
    fprintf (fp, "] def\n/Widths [ ");
    for (ci = 0; ci < cols; ++ci)
      fprintf (fp, "%d ", dpy.widths[ci]);
    fprintf (fp, "] def\n");
  }
  fprintf (fp, "\n");
  fprintf (fp, "/to-oleo-matrix\n");
  fprintf (fp, "    0 PointsTall neg matrix translate \n");
  fprintf (fp, "    1 Scalew div 1 Scaleh div neg matrix scale\n");
  fprintf (fp, " matrix concatmatrix def\n");
  fprintf (fp, "\n");
  fprintf (fp, "/from-oleo-matrix to-oleo-matrix matrix invertmatrix def\n");
  fprintf (fp, "\n");
  fprintf (fp, "/rc-to-oleo-xy { Colx exch gget exch Rowy exch gget } def\n");
  fprintf (fp, "/rc-to-oleo-wh { Widths exch gget exch Heights exch gget } def\n");
  fprintf (fp, "/oleo-xy-to-ps { from-oleo-matrix transform } def\n");
  fprintf (fp, "/rc-to-xy { rc-to-oleo-xy oleo-xy-to-ps } def\n");
  fprintf (fp, "/rc-to-wh { rc-to-oleo-wh Scaleh mul exch Scalew mul exch } def\n");
  fprintf (fp, "/oleo-box-to-ps\n");
  fprintf (fp, "{ \n");
  fprintf (fp, "  dup dup 0 gget exch 1 gget oleo-xy-to-ps\n");
  fprintf (fp, "  3 -1 roll dup dup 2 gget exch 3 gget oleo-xy-to-ps\n");
  fprintf (fp, "  3 -1 roll astore\n");
  fprintf (fp, "} def\n");
  fprintf (fp, "\n");
  fprintf (fp, "\n");
  fprintf (fp, "/cell-goal   %% string just r c -- [x y x' y']\n");
  fprintf (fp, "{\n");
  fprintf (fp, "  3 index stringwidth pop /cell-goal-w exch def\n");
  fprintf (fp, "  /cell-goal-h DefaultFontSize 2 add def\n");
  fprintf (fp, "  2 copy rc-to-xy   %% str j r c xcel ycel\n");
  fprintf (fp, "  5 -1 roll dup 0 lt \n");
  fprintf (fp, "  { pop }  %% str r c xstr ycel\n");
  fprintf (fp, "  { 0 eq 	    %% str r c xcel ycel bool\n");
  fprintf (fp, "    Widths 4 index gget Scalew mul exch  %% str r c xcel ycel wcell bool\n");
  fprintf (fp, "    { cell-goal-w sub 2 div 3 -1 roll add exch }\n");
  fprintf (fp, "    { 2 index add cell-goal-w sub 3 -1 roll pop exch } ifelse \n");
  fprintf (fp, "    %% str r c xstr ycel \n");
  fprintf (fp, " } ifelse      \n");
  fprintf (fp, " %% str r c xstr ycel \n");
  fprintf (fp, " 5 -3 roll pop pop pop\n");
  fprintf (fp, " 2 copy  cell-goal-h add exch cell-goal-w add exch\n");
  fprintf (fp, " 4 array astore  \n");
  fprintf (fp, "} def\n");
  fprintf (fp, "\n");
  fprintf (fp, "/layout		%%  [x y x' y'] r c -- [rlo clo rhi chi]\n");
  fprintf (fp, "{\n");
  fprintf (fp, "  2 copy \n");
  fprintf (fp, "  /layout_c fed\n");
  fprintf (fp, "  /layout_r fed\n");
  fprintf (fp, "  exch 2 copy 4 array astore 	%% [x y x' y'] [c r c r]\n");
  fprintf (fp, "  /layout_answer fed\n");
  fprintf (fp, "  /layout_goal fed\n");
  fprintf (fp, "\n");
  fprintf (fp, "  layout_answer 0 gget	\n");
  fprintf (fp, "  {\n");
  fprintf (fp, "    dup 0 eq { exit } if\n");
  fprintf (fp, "    dup Colx exch gget Scalew mul layout_goal 0 gget le { exit } if\n");
  fprintf (fp, "    1 sub\n");
  fprintf (fp, "  } loop\n");
  fprintf (fp, "  layout_answer exch 0 exch put\n");
  fprintf (fp, "\n");
  fprintf (fp, "  layout_answer 1 gget	\n");
  fprintf (fp, "  {\n");
  fprintf (fp, "    dup 0 eq { exit } if\n");
  fprintf (fp, "    dup Rowy exch gget Scaleh mul layout_goal 1 gget le { exit } if\n");
  fprintf (fp, "    1 sub\n");
  fprintf (fp, "  } loop\n");
  fprintf (fp, "  layout_answer exch 3 exch put\n");
  fprintf (fp, "\n");
  fprintf (fp, "  layout_answer 2 gget	\n");
  fprintf (fp, "  {\n");
  fprintf (fp, "    dup Cols 1 sub eq { exit } if\n");
  fprintf (fp, "    dup dup Colx exch gget exch Widths exch gget add Scalew mul\n");
  fprintf (fp, "    layout_goal 2 gget gt { exit } if\n");
  fprintf (fp, "    1 add \n");
  fprintf (fp, "  } loop\n");
  fprintf (fp, "  layout_answer exch 2 exch put\n");
  fprintf (fp, "\n");
  fprintf (fp, "  layout_answer dup 0 gget exch 1 gget \n");
  fprintf (fp, "  layout_answer exch 0 exch put layout_answer exch 1 exch put\n");
  fprintf (fp, "  layout_answer dup 2 gget exch 3 gget \n");
  fprintf (fp, "  layout_answer exch 2 exch put layout_answer exch 3 exch put\n");
  fprintf (fp, "  layout_answer 1 gget 1 layout_answer 3 gget\n");
  fprintf (fp, "  {\n");
  fprintf (fp, "    /layout_ct fed\n");
  fprintf (fp, "    layout_r 1 layout_answer 2 gget \n");
  fprintf (fp, "     {\n");
  fprintf (fp, "       /layout_rt fed\n");
  fprintf (fp, "       layout_rt layout_ct checkused \n");
  fprintf (fp, "	{\n");
  fprintf (fp, "	  layout_ct layout_c lt\n");
  fprintf (fp, "	    { layout_ct 1 add layout_answer 1 gget max layout_answer exch 1 exch put }\n");
  fprintf (fp, "	    if\n");
  fprintf (fp, "	  layout_ct layout_c gt\n");
  fprintf (fp, "	    { layout_ct 1 sub layout_answer 3 gget min layout_answer exch 3 exch put }\n");
  fprintf (fp, "	    if\n");
  fprintf (fp, "	  layout_rt layout_r lt\n");
  fprintf (fp, "	    { layout_rt 1 add layout_answer 0 gget max layout_answer exch 0 exch put }\n");
  fprintf (fp, "	    if\n");
  fprintf (fp, "	} if\n");
  fprintf (fp, "     } for\n");
  fprintf (fp, "  } for\n");
  fprintf (fp, "\n");
  fprintf (fp, "  layout_answer 0 gget 1 layout_answer 2 gget \n");
  fprintf (fp, "  {\n");
  fprintf (fp, "    /layout_rt fed\n");
  fprintf (fp, "    layout_answer 1 gget 1 layout_answer 3 gget\n");
  fprintf (fp, "    {\n");
  fprintf (fp, "      layout_rt exch setused\n");
  fprintf (fp, "    } for \n");
  fprintf (fp, "  } for\n");
  fprintf (fp, "\n");
  fprintf (fp, "  layout_answer\n");
  fprintf (fp, "} def\n");
  fprintf (fp, "\n");
  fprintf (fp, "\n");
  fprintf (fp, "/add-pair\n");
  fprintf (fp, "{\n");
  fprintf (fp, "  3 -1 roll add 3 1 roll add exch\n");
  fprintf (fp, "} def\n");
  fprintf (fp, "\n");
  fprintf (fp, "/layout-to-oleo-box\n");
  fprintf (fp, "{\n");
  fprintf (fp, "  dup dup 0 gget exch 1 gget rc-to-oleo-xy 3 -1 roll\n");
  fprintf (fp, "  dup 0 gget exch 1 gget \n");
  fprintf (fp, "  2 copy rc-to-oleo-xy 4 2 roll\n");
  fprintf (fp, "  rc-to-oleo-wh add-pair\n");
  fprintf (fp, "  4 array astore\n");
  fprintf (fp, "} def\n");
  fprintf (fp, "\n");
  fprintf (fp, "/layout-to-bbox\n");
  fprintf (fp, "{\n");
  fprintf (fp, "  layout-to-oleo-box oleo-box-to-ps\n");
  fprintf (fp, "} def\n");
  fprintf (fp, "\n");
  fprintf (fp, "/boxpath\n");
  fprintf (fp, "{\n");
  fprintf (fp, "  newpath\n");
  fprintf (fp, "  dup dup 0 gget exch 1 gget moveto\n");
  fprintf (fp, "  dup dup 0 gget exch 3 gget lineto\n");
  fprintf (fp, "  dup dup 2 gget exch 3 gget lineto\n");
  fprintf (fp, "  dup 2 gget exch 1 gget lineto\n");
  fprintf (fp, "  closepath\n");
  fprintf (fp, "} def\n");
  fprintf (fp, "\n");
  fprintf (fp, "/use-font\n");
  fprintf (fp, "{\n");
  fprintf (fp, "  /CellFont fed\n");
  fprintf (fp, "  CellFont setfont\n");
  fprintf (fp, "  CellFont /FontBBox get\n");
  fprintf (fp, "   dup dup 0 gget exch 1 gget CellFont /FontMatrix get transform\n");
  fprintf (fp, "  neg /CellFontDescent fed\n");
  fprintf (fp, "  neg /CellFontLbearing fed\n");
  fprintf (fp, "   3 gget 0 exch CellFont /FontMatrix get transform exch pop\n");
  fprintf (fp, "  /CellFontAscent fed\n");
  fprintf (fp, "} def\n");
  fprintf (fp, "DefaultFont use-font\n");
  fprintf (fp, "\n");
  fprintf (fp, "/cell %% str just r c\n");
  fprintf (fp, "{\n");
  fprintf (fp, "  /cell-c fed\n");
  fprintf (fp, "  /cell-r fed\n");
  fprintf (fp, "  /cell-j fed\n");
  fprintf (fp, "  /cell-s fed\n");
  fprintf (fp, "  \n");
  fprintf (fp, "  /cell-g cell-s cell-j cell-r cell-c cell-goal def\n");
  fprintf (fp, "  /cell-l cell-g cell-r cell-c layout layout-to-bbox def\n");
  fprintf (fp, "\n");
  fprintf (fp, "  cell-g 0 get cell-g 1 get moveto\n");
  fprintf (fp, "  CellFontLbearing\n");
  fprintf (fp, "  CellFontAscent neg rmoveto\n");
  fprintf (fp, "  cell-s show\n");
  fprintf (fp, "} def\n");
  fprintf (fp, "\n");
  {
    int ri, ci;
    struct font_memo * last_font = default_font ();
    for (ri = 0; ri < rows; ++ri)
      for (ci = 0; ci < cols; ++ci)
	{
	  struct cell_display * cd = dpy.cells + ri * cols + ci;
	  if (cd->unclipped)
	    {
	      fprintf (fp, "/S ");
	      put_ps_string (cd->unclipped, fp);
	      fprintf (fp, " def \nS ");
	      switch (cd->justification)
		{
		default:
		case JST_LFT:
		  fprintf (fp, " -1 ");
		  break;
		case JST_CNT:
		  fprintf (fp, " 0 ");
		  break;
		case JST_RGT:
		  fprintf (fp, " 1 ");
		  break;
		};

	      {
		struct font_memo * font = cd->font;
		if (!font)
		  font = default_font ();
		if (font != last_font)
		  {
		    fprintf (fp, "/%s findfont %f DefaultFontSize mul",
			     font->psname, font->scale);
		    fprintf (fp, " scalefont use-font\n");
		    last_font = font;
		  }
		fprintf (fp, "%d %d cell\n", ri, ci);
	      }
	    }
	}
  }
  free_display (&dpy);
}
