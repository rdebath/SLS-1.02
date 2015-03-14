
/*
 * Copyright (C) 1992  Board of Regents of the University of Wisconsin
 * on behalf of the Department of Electrical Engineering and Computer
 * Science, University of Wisconsin-Milwaukee, Milwaukee, WI 53201.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * The programs in this directory were developed by software engineering
 * teams as part of the course "Introduction to Software Engineering"
 * under the supervision of Professor G. Davida.
 *
 * Please send all changes, enhancements, and other comments about this
 * software to
 *
 *     		soft-eng@cs.uwm.edu
 *
 *			or
 *		
 *		Software Engineering Coordinator
 *		Computer Science
 *   		Department of EECS
 *		University of Wisconsin - Milwaukee
 *		Milwaukee, WI  53201
 *		414-229-4677
 */

/*
 ******************************************************************************
    This function interfaces the spread sheet (sc) program and plots x-y graph 
  in the X enviroment. Up to six curves can be plotted at the same time.      
  Different curves can be plotted with different symbles. Plotting style can be
  symble or line alone, or both together. Grid lines can be plotted horizotally
  or vertically, or both at the same time. There are four titles that can be  
  put on the screen. Two titles will be printed on the top of the graph, and a 
  x-title and y-title will be printed at sides of the graph. Both the horizontal
  and vertical scale can be set to automatic or manual, in first case the scale 
  will be determined by the data range provided, in the second case both the   
  upper and the lower bounds can be changed by the user. There one legend with
  a symble for each of the curves that will be appear at the bottom of the   
  graph once they are set. Null string that entered to either the titles    
  or the legends will erase them on the graph.                             
     This function was written by using the X liberiry X11 Release 3 by SWLin,
  at the University of Wisconsin-Milwaukee, August, 1990.                    

  Modified 12/01/91 - Dan Gruber.  I corrected the problem with plot_XY so that
		      it is now able to display legends with this type of graph.
 ****************************************************************************** 
*/                                   

#include <math.h>
#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "plot.h" 
#include "graphic_gvar.h"
#include "sc.h"
#include "scXstuff.h"
#define TITLE "XSPREAD-Grapher (XY Plot)"

/*
 * This structure forms the WM_HINTS property of the window,
 * letting the window manager know how to handle this window.
 * See Section 9.1 of the Xlib manual.
 */

   XWMHints     xwmh = {
	  (InputHint|StateHint),   /* flags         */
          False,                   /* input         */
          NormalState,             /* initial_state */
	  0,                       /* icon pixmap   */
	  0,                       /* icon window   */
          0, 0,                    /* icon location */
          0,                       /* icon mask     */
          0,                       /* Window group  */
   };


plot_XY()
{   double x_max, x_min, y_max, y_min; 
    double x_range, y_range;
    double y_power, x_power; 
   
    int  i, j, k, len;
    int  first_y_datum;
    int  grid_l_x, grid_l_y, grid_w, grid_h;
    int  font_w, text_w;
    int  legend_n;
    int  x, y, x1, y1;
    int  looping = 1; /* true if we shouldn't exit yet */
    int  draw = 0;  /* true if we should paint the window */
    char format;    /* graph format specifier, (L)ine, (S)ymbol, (B)oth */
    int  first_point; /* true when getting coords for 1st point in curve */
    struct ent *p;
    struct ent *xent, *yent;  /* table entries for x and y values */
    int cells=0; 
    int first_x_datum=1;
    
    char str[100];
    register int c;

    int argc = 0;
    char **argv;
/* check that we have enough valid x values.  While we are looping,
 * determine the minimum and maximum x values, in case we need them
 */
   for (i=graphic_range[0].r1; i <= graphic_range[0].r2; i++) { 
     p = lookat( i, graphic_range[0].c);
     if (!(p->flags & is_valid)) continue;
       cells++;
       if (first_x_datum){
         x_max = p->v;
         x_min = p->v;
         first_x_datum = 0;
       } else {
         x_max = Max(x_max, p->v); 
         x_min = Min(x_min, p->v);
       }
   } /* for i */
   if (cells < 2) {
     fprintf(stderr,"\007");
     message("Not enough valid X values");
     return;
   }
/* 
 * The plotting job now
 */

    /*
     * Decide the scale for x and y axes
     */
     /* if x-scale is manual, override limits calculated above*/
     if (g_auto_man[0] == 'M'){
        x_max = graphic_scale[0][1];
        x_min = graphic_scale[0][0];
     }

     /* calculate y limits automatically or manually */
     if (g_auto_man[1] == 'M'){
        y_max = graphic_scale[1][1];
        y_min = graphic_scale[1][0];
     } else {
        first_y_datum = 1;
	for (j = 1; j < GRAPHRANGES; j++) { 

           if (*(graphic_range[j].col) == '\0') continue;

           for (i=graphic_range[j].r1; i <= graphic_range[j].r2; i++) { 
	     p = lookat(i, graphic_range[j].c);
	     if ( !(p->flags & is_valid)) continue; /* ignore invalid */
             if (first_y_datum) {
               y_max = p->v;
               y_min = p->v;
               first_y_datum = 0;
             } else {
               y_max = Max(y_max, p->v); 
               y_min = Min(y_min, p->v);
	     }
           }
        }
     } 
   if (x_min == x_max) {              /* protection for the case max == min */
       x_max = 1.01 * x_max;
       x_min = 0.99 * x_min;
    }
   if (y_min == y_max) {
       y_max = 1.01 * y_max;
       y_min = 0.99 * y_min;
    }
   
    x_power = - floor(log10((x_max - x_min)));     /* try to print nice lable */
    y_power = - floor(log10((y_max - y_min)));
    x_power = pow10(x_power);
    y_power = pow10(y_power);
    x_min = floor(x_min * x_power) / x_power;
    x_max = ceil(x_max * x_power) / x_power;

    y_min = floor(y_min * y_power) / y_power;
    y_max = ceil(y_max * y_power) / y_power;
    x_range = x_max - x_min;
    y_range = y_max - y_min;
    
    /* 
     * Open a new display window
     */

    /*
     * Select colors for the border,  the window background,  and the
     * foreground.
     */

    bd = WhitePixel(dpy, DefaultScreen(dpy));
    bg = BlackPixel(dpy, DefaultScreen(dpy));
    fg = WhitePixel(dpy, DefaultScreen(dpy));

    /*
     * Set the border width of the window,  and the gap between the text
     * and the edge of the window, "pad".
     */

    pad = BORDER;
    bw = 1;

    /*
     * Deal with providing the window with an initial position & size.
     * Fill out the XSizeHints struct to inform the window manager.
     */

    xsh.width = XTextWidth(curfont, TITLE, 80 + pad * 2);

    xsh.flags = (PPosition | PSize);
    xsh.height = WIN_H; 
    xsh.width = WIN_W; 
    xsh.x = (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh.width) / 2;
    xsh.y = (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh.height) / 2;

    /*
     * Create the Window with the information in the XSizeHints, the
     * border width,  and the border & background pixels.
     */

    win = XCreateSimpleWindow(dpy, DefaultRootWindow(dpy),
                  xsh.x, xsh.y, xsh.width, xsh.height,
                  bw, bd, bg);

    /*
     * Set the standard properties for the window managers. 
     */

    XSetStandardProperties(dpy, win, TITLE, TITLE, 
			   None, argv, argc, &xsh);
    XSetWMHints(dpy, win, &xwmh);

    /*
     * Ensure that the window's colormap field points to the default
     * colormap,  so that the window manager knows the correct colormap to
     * use for the window.   Also,  set the window's Bit Gravity to reduce 
     * Expose events.
     */

    xswa.colormap = DefaultColormap(dpy, DefaultScreen(dpy));
    xswa.bit_gravity = CenterGravity;
    XChangeWindowAttributes(dpy, win, (CWColormap | CWBitGravity), &xswa);


    /* 
     * Map the window to make it visible.  
     */
   
     XMapWindow(dpy, win);

    /*
     * Find out the dimension of the window.
     */

    if (XGetWindowAttributes(dpy, win, &xwa) == 0){
       fprintf(stderr,"plot_XY: Can't access attributes for graph window\n");
       exit(1);    
    }

    XClearWindow(dpy, win);
    XSelectInput(dpy,win, ButtonPressMask|KeyPressMask|ExposureMask);

while(looping)
 {   

    XEvent event;

    XNextEvent(dpy,&event);
    switch (event.type){
      case KeyPress: 
      case ButtonPress: looping = 0; 
			break;
      case Expose: draw = 1;
    }


    /* if not drawing, go back and get another event */
    if (!draw) continue;

    /*
     * Draw the coordinate box
     */

    XDrawRectangle(dpy, win, maingcreversed, RECT_X, RECT_Y, RECT_W, RECT_H); 
    
    /*
     * Draw grids
     */
    /* In case it is as unobvious to future programmers as it was to this
     * one, 8 is the length of a hash-mark, as opposed to a full grid line
     */
    if (graphic_grid == 'H') {              /* horizontal grid     */
       grid_l_x = RECT_W;
       grid_l_y = 8;
    }
    else if (graphic_grid == 'V') {         /* vertical grid       */
       grid_l_x = 8;
       grid_l_y = RECT_H;
    }
    else if (graphic_grid == 'B') {         /* both direction grid */
       grid_l_x = RECT_W;
       grid_l_y = RECT_H;
    }
    else {
       grid_l_x = 8;                        /* no grid             */
       grid_l_y = 8;
    }
       

    grid_w = RECT_W / 10;                   /* grid seperation     */
    grid_h = RECT_H / 10;

    for (i = 0; i < 11; i++) {
       XDrawLine(dpy, win, maingcreversed, 
		 RECT_X, RECT_Y + i * grid_h, 
		 RECT_X + grid_l_x, RECT_Y + i * grid_h); 
       XDrawLine(dpy, win, maingcreversed, 
		 RECT_X + RECT_W, RECT_Y + i * grid_h, 
                 RECT_X + RECT_W - grid_l_x, RECT_Y + i * grid_h); 
       XDrawLine(dpy, win, maingcreversed, 
		 RECT_X + i * grid_w, RECT_Y, 
		 RECT_X + i * grid_w, RECT_Y + grid_l_y); 
       XDrawLine(dpy,win,maingcreversed,RECT_X + i * grid_w, RECT_Y + RECT_H, 
                 RECT_X + i * grid_w, RECT_Y + RECT_H- grid_l_y); 

                 sprintf(str,"%.1f",(y_max - y_range*i/10));
                 len = strlen(str);
                 text_w = XTextWidth(curfont,str,len+2);
                 XDrawImageString(dpy,win,maingcreversed,
                            RECT_X - text_w,
                            RECT_Y + i*grid_h + curfontheight/3,
                            str,len);

                 sprintf(str,"%.1f",(x_max - x_range*i/10));
                 len = strlen(str);
                 text_w = XTextWidth(curfont,str,len);
                 XDrawImageString(dpy,win,maingcreversed,
		            RECT_X+RECT_W-i*grid_w-text_w/2,RECT_H+RECT_Y*7/6, 
                            str,len);

    }

    /*
     * Draw titles
     */
    /* first graph title */
    len = strlen(graphic_title[0]);   
    text_w= XTextWidth(curfont, graphic_title[0], len);
    XDrawImageString(dpy, win, maingcreversed, (WIN_W - text_w) / 2  , RECT_Y / 3,
                     graphic_title[0], len);
    /* second graph title */
    len = strlen(graphic_title[1]);
    text_w= XTextWidth(curfont, graphic_title[1], len);
    XDrawImageString(dpy, win, maingcreversed, 
		     (WIN_W - text_w) / 2  , RECT_Y * 2 / 3,
                     graphic_title[1], len);

    /* x-axis title */
    len = strlen(graphic_title[2]);
    text_w= XTextWidth(curfont, graphic_title[2], len);
    XDrawImageString(dpy, win, maingcreversed, 
		     (WIN_W - text_w) / 2  , RECT_H + RECT_Y * 3 / 2, 
		     graphic_title[2], len);
  
    /* y-axis title */
    len = strlen(graphic_title[3]);
    text_w=XTextWidth(curfont,graphic_title[3],len);
    if ((RECT_X-text_w) < 0)
       x = curfontwidth;
    else
       x = RECT_X-text_w;
    XDrawImageString(dpy,win,maingcreversed,
                     x, RECT_Y - curfontheight,
                     graphic_title[3],len);


    /*
     * Draw legends
     */

    legend_n = 0;
    for (i = 0;  i < GRAPHLEGENDS; i++) {
      if ((*(graphic_range[i + 1].col) == '\0') || (curves_n < 1) || 
           (graphic_legend[i][0] == '\0')) 
          continue; 

      len = strlen(graphic_legend[i]);
          text_w = XTextWidth(curfont, graphic_legend[i], len);
      if (curves_n == 1) { 
                                /* Draw the legend symble */
        if (graphic_format[i] == 'L') {
           XDrawLine(dpy, win, maingcreversed, 
		     RECT_X +(RECT_W - text_w) / 2  - 15, 
                     RECT_Y * 7 / 4 +RECT_H, RECT_X +(RECT_W - text_w) / 2 - 5, 
                     RECT_Y * 7 / 4 +RECT_H); 
        }
        else if ((graphic_format[i] == 'S') || (graphic_format[i] == 'B')) {
	   x = RECT_X + (RECT_W - text_w) / 2 - 10;
	   y = RECT_Y * 7 / 4 + RECT_H;
           switch (i) {
             case 0: DrawOpenSquare(x,y); 	break;
             case 1: DrawOpenTriangle(x,y); 	break;
             case 2: DrawOpenDiamon(x,y); 	break;
             case 3: DrawCross(x,y); 		break;
             case 4: DrawCloseSquare(x,y); 	break;
             case 5: DrawCloseDiamon(x,y); 	break;
           }
           if (graphic_format[i] == 'B') 
              XDrawLine(dpy, win, maingcreversed, 
			x-5, y,
			x+5, y); 
        } 
                                 /* Draw the legend title */

        XDrawImageString(dpy, win, maingcreversed, 
			 RECT_X + (RECT_W - text_w)/2, RECT_Y*7/4 + RECT_H, 
			 graphic_legend[i], len);
        break;
     }
     else {

                 /* Draw the legend symbols */

        if (graphic_format[i] == 'L') {
           XDrawLine(dpy, win, maingcreversed ,
		     RECT_X/2 + legend_n*(WIN_W/curves_n - 10) - 15, 
		     RECT_Y * 7 / 4 + RECT_H, 
		     RECT_X / 2 + legend_n * (WIN_W / curves_n - 10) - 5, 
		     RECT_Y *7 / 4 + RECT_H); 
        }
        else if ((graphic_format[i] == 'S') || (graphic_format[i] == 'B')) {
           switch (i) {
           case 0:
              DrawOpenSquare(RECT_X/2 + legend_n*(WIN_W / curves_n - 10) - 10, 
                             RECT_Y * 7 / 4 + RECT_H);
              break;
           case 1:
              DrawOpenTriangle(RECT_X/2 + legend_n*(WIN_W/curves_n - 10) - 10, 
                               RECT_Y * 7 / 4 + RECT_H);
              break;
           case 2:
              DrawOpenDiamon(RECT_X/2 + legend_n * (WIN_W/curves_n - 10) - 10, 
                             RECT_Y*7/4 + RECT_H);
              break;
           case 3:
              DrawCross(RECT_X/2 + legend_n*(WIN_W/curves_n - 10) - 10, 
                        RECT_Y*7/4 + RECT_H);
              break;
           case 4:
              DrawCloseSquare(RECT_X/2 + legend_n*(WIN_W/curves_n - 10) - 10, 
                              RECT_Y*7/4 + RECT_H);
              break;
           case 5:
              DrawCloseDiamon(RECT_X/2 + legend_n*(WIN_W/curves_n - 10) - 10, 
                              RECT_Y*7/4 + RECT_H);
              break;
           }
           if (graphic_format[i] == 'B')
              XDrawLine(dpy, win, maingcreversed ,
			RECT_X/2 + legend_n*(WIN_W/curves_n - 10) - 15, 
			RECT_Y*7/4 + RECT_H, 
			RECT_X/2 + legend_n*(WIN_W/curves_n - 10) - 5, 
			RECT_Y*7/4 + RECT_H); 
          
        }

                                   /* Draw the legend titles */
        XDrawImageString(dpy, win, maingcreversed, 
			 RECT_X/2 + legend_n*(WIN_W/curves_n - 10), 
			 RECT_Y*7/4 + RECT_H, 
                         graphic_legend[i], len);
        legend_n ++;
      }
   }


/* 
 * Draw curves
 */

        for (i = 1; i < GRAPHRANGES; i++) { 
           if (*(graphic_range[i].col) == '\0') continue;
	   
	   first_point = 1;
	   format = graphic_format[i];

           for (j = graphic_range[0].r1, k = graphic_range[i].r1;
               (j <= graphic_range[0].r2) && (k <= graphic_range[i].r2); j++, k++) {

	      xent = lookat(j,graphic_range[0].c);
	      if (!(xent->flags & is_valid) || (xent->v < x_min) 
		  || (xent->v > x_max))
		continue;
	      yent = lookat(j, graphic_range[i].c);
	      if (!(yent->flags & is_valid) || (yent->v < y_min) 
		  || (yent->v > y_max))
		continue;

	      if (first_point){
                x1 = RECT_X + RECT_W * (xent->v - x_min) / x_range;  
           	y1 = RECT_Y + RECT_H - RECT_H * (yent->v - y_min) / y_range;   
	        if ((format == 'S') || (format == 'B'))
	  	  switch(i){
		    case 1: DrawOpenSquare(x1, y1); 	break;
                    case 2: DrawOpenTriangle(x1, y1); 	break;
                    case 3: DrawOpenDiamon(x1, y1); 	break;
              	    case 4: DrawCross(x1, y1);      	break;
                    case 5: DrawCloseSquare(x1, y1);	break;
                    case 6: DrawCloseDiamon(x1, y1);	break;
                  }
		first_point = 0;
		continue;
	      }

              x = RECT_X + RECT_W * (xent->v - x_min) / x_range;   
              y = RECT_Y + RECT_H - RECT_H * (yent->v - y_min) / y_range;   

	      if ((format == 'L') || (format == 'B')) 
                XDrawLine(dpy, win, maingcreversed, x1, y1, x, y);
	      if ((format == 'S') || (format == 'B'))
		switch(i){
		  case 1: DrawOpenSquare(x, y); 	break;
                  case 2: DrawOpenTriangle(x, y); 	break;
                  case 3: DrawOpenDiamon(x, y); 	break;
              	  case 4: DrawCross(x, y);      	break;
                  case 5: DrawCloseSquare(x, y);	break;
                  case 6: DrawCloseDiamon(x, y);	break;
                }
              x1 = x;
              y1 = y;
           }
        }      
     }  /* end of while */
    XUnmapWindow(dpy, win);
    XDestroyWindow(dpy, win);
    /*XCloseDisplay(dpy); */  /* NO. don't do this anymore */
}  /* end of plot_line */


/*
 * The following Drawing functions draw diffrent symbols. 
 */

void DrawOpenDiamon(x, y)
short x, y;
{
  XPoint points[5];

  points[0].x = x; 
  points[0].y = y - 3; 
  points[1].x = 3; 
  points[1].y = 3; 
  points[2].x = -3; 
  points[2].y = 3; 
  points[3].x = -3; 
  points[3].y = -3; 
  points[4].x = 3; 
  points[4].y = -3; 
  XDrawLines(dpy, win, maingcreversed, points, 5, CoordModePrevious);
}

void DrawCloseDiamon(x, y)
short x, y;
{
  XPoint points[12];

  points[0].x = x; 
  points[0].y = y; 
  points[1].x = -2; 
  points[1].y = 0; 
  points[2].x = 4; 
  points[2].y = 0; 
  points[3].x = -2; 
  points[3].y = 0; 
  points[4].x = 0; 
  points[4].y = -2; 
  points[5].x = 0; 
  points[5].y = 4; 
  points[6].x = 0; 
  points[6].y = -2; 
  points[7].x = 1; 
  points[7].y = 1; 
  points[8].x = -2; 
  points[8].y = -2; 
  points[9].x = 1; 
  points[9].y = 1; 
  points[10].x = -1; 
  points[10].y = 1; 
  points[11].x = 2; 
  points[11].y = -2; 
  XDrawLines(dpy, win, maingcreversed, points, 12, CoordModePrevious);
}

void DrawOpenSquare(x, y)
short x, y;
{
  XPoint points[5];
  points[0].x = x - 2; 
  points[0].y = y - 2; 
  points[1].x = 4; 
  points[1].y = 0; 
  points[2].x = 0; 
  points[2].y = 4; 
  points[3].x = -4; 
  points[3].y = 0; 
  points[4].x = 0; 
  points[4].y = -4; 
  XDrawLines(dpy, win, maingcreversed, points, 5, CoordModePrevious);
}

void DrawCloseSquare(x, y)
short x, y;
{
  XPoint points[9];

  points[0].x = x - 2; 
  points[0].y = y - 2; 
  points[1].x = 4; 
  points[1].y = 0; 
  points[2].x = 0; 
  points[2].y = 4; 
  points[3].x = -4; 
  points[3].y = 0; 
  points[4].x = 0; 
  points[4].y = -4; 
  points[5].x = 4; 
  points[5].y = 4; 
  points[6].x = -2; 
  points[6].y = -2; 
  points[7].x = -2; 
  points[7].y = 2; 
  points[8].x = 4; 
  points[8].y = -4; 
  XDrawLines(dpy, win, maingcreversed, points, 9, CoordModePrevious);
}

void DrawOpenTriangle(x, y)
short x, y;
{ 
  XPoint points[4];

  points[0].x = x; 
  points[0].y = y - 3; 
  points[1].x = 2; 
  points[1].y = 5; 
  points[2].x = -4; 
  points[2].y = 0; 
  points[3].x = 2; 
  points[3].y = -5; 
  XDrawLines(dpy, win, maingcreversed, points, 4, CoordModePrevious);
}


void DrawCross(x, y)
short x, y;
{
  XPoint points[6];

  points[0].x = x; 
  points[0].y = y; 
  points[1].x = -2; 
  points[1].y = 0; 
  points[2].x = 4; 
  points[2].y = 0; 
  points[3].x = -2; 
  points[3].y = 0; 
  points[4].x = 0; 
  points[4].y = -2; 
  points[5].x = 0; 
  points[5].y = 4; 
  XDrawLines(dpy, win, maingcreversed, points, 6, CoordModePrevious);
}

/*
 * pow10 calculates the power function of base 10
 */

double pow10(p)
   double p;
{
   double q;
 
   p = floor(p);
   if (p >= 0) {
      for (q = 1; p > 0; --p)
         q = q * 10;
   }
   else {
      p = -p;
      for (q = 1; p > 0; --p)
         q = q / 10;
   }
   return q;
}


/*
 * rm_tail_zero removes heading white spaces, trailing white spaces, 
 * zero '0' and '.' of a string for nice printing of graph lable 
 *
 */

char *rm_tail_zero(s)
   char  *s;
{  
   char *t;
   /* Get rid of heading white spaces */
   for (t = s; *t == ' ' || *t == '\t'; t++);
   strcpy(s, t);

   /* Get rid of trailing white spaces and zeros */
   t = s;
   while ((*t) && (*t != '\n')) /* while not null or newline */
      t++;
   t--;
   while ((t != s) && ((*t == ' ') || (*t == '0')))
      t--;
   if (*t != '.')     /* Get rid of trailing '.' */
      t++;
   *t = '\0';
   return s;
}
