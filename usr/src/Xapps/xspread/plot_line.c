
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
 * This procedure will display the line graph given the values provided by    *
 * the user.  It evenly spaces out the columns depending on the number of     *
 * X-Values which are used.                                                   * 
 * Dan Gruber - October 1991                                                  *
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
#define TITLE "XSPREAD-Grapher (Line Plot)"
#define PIX_SEP 3

#ifndef irint
#define irint(x) floor((x)+0.5)
#endif

/*
 * This structure forms the WM_HINTS property of the window,
 * letting the window manager know how to handle this window.
 * See Section 9.1 of the Xlib manual.
 */

   XWMHints     XWMH = {
	  (InputHint|StateHint),   /* flags         */
          False,                   /* input         */
          NormalState,             /* initial_state */
	  0,                       /* icon pixmap   */
	  0,                       /* icon window   */
          0, 0,                    /* icon location */
          0,                       /* icon mask     */
          0,                       /* Window group  */
   };


plot_line()
{   double max_y, min_y; 
    double Y_Range;
    double y_power, x_power; 
    double grid_w;
   
    int  i, j, k, len;
    int  first_y_datum=1;
    int  grid_l_x, grid_l_y, grid_h;
    int  font_w, text_w;
    int  legend_n;
    int  x, y, x1, y1;
    int  looping = 1;  /* true if we shouldn't exit yet                    */
    int  draw = 0;     /* true if we should paint the window               */
    char format;       /* graph format specifier, (L)ine, (S)ymbol, (B)oth */
    int  first_point;  /* true when getting coords for 1st point in curve  */
    struct ent *p;
    struct ent *yent;  /* table entries y values                           */
    int cells=0; 
    int items=0;
    int number_of_items=0;
    int width;         /* used to calculate the Labelsize */
    int labelsize;     /* The size of the Label to be drawn */
    
    char str[100];
    register int c;

    int argc = 0;
    char **argv;

/* check that we have enough valid x values.  While we are looping,
 * determine the minimum and maximum x values, in case we need them
 */

   for (i=graphic_range[0].r1; i <= graphic_range[0].r2; i++) { 
     p = lookat( i, graphic_range[0].c);
     if (!(p->label)) continue;
       cells++;
   } /* for i */

   if (cells < 2) {
     fprintf(stderr,"\007");
     message("Not enough valid X values");
     return;
   }

   number_of_items = cells;

/* 
 * The plotting job now
 */

    /*
     *  Decide the scale for x and y axes
     */

     /* calculate y limits automatically or manually */
     if (g_auto_man[1] == 'M'){
        max_y = graphic_scale[1][1];
        min_y = graphic_scale[1][0];
     } else {
        first_y_datum = 1;
	for (j = 1; j < GRAPHRANGES; j++) { 

           if (*(graphic_range[j].col) == '\0') continue;

           for (i=graphic_range[j].r1; i <= graphic_range[j].r2; i++) { 
	     p = lookat(i, graphic_range[j].c);
	     if ( !(p->flags & is_valid)) continue; /* ignore invalid */
             if (first_y_datum) {
               max_y = p->v;
               min_y = p->v;
               first_y_datum = 0;
             } else {
               max_y = Max(max_y, p->v); 
               min_y = Min(min_y, p->v);
	     }
           }
        }
     } 
   /* protection for the case max == min */
   if (min_y == max_y) {
       max_y = 1.01 * max_y;
       min_y = 0.99 * min_y;
    }
   
    y_power = - floor(log10((max_y - min_y)));
    y_power = Lpow10(y_power);

    min_y = floor(min_y * y_power) / y_power;
    max_y = ceil(max_y * y_power) / y_power;
    Y_Range = max_y - min_y;
    
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
    XSetWMHints(dpy, win, &XWMH);

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
    
    /*  grid_l_y and grid_l_x are the length of the grid lines.  
     *  Only tick marks are drawn when the value is '8'.
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
       

    grid_w = RECT_W / (double)(number_of_items + 1);  /* grid seperation */
    grid_h = RECT_H / 10;

    /*
     * Draw grids along with the labels on the Y-Axis
     */

    for (i = 0; i < 11; i++) {
       XDrawLine(dpy, win, maingcreversed, 
		 RECT_X, RECT_Y + i * grid_h, 
		 RECT_X + grid_l_x, RECT_Y + i * grid_h); 
       XDrawLine(dpy, win, maingcreversed, 
		 RECT_X + RECT_W, RECT_Y + i * grid_h, 
                 RECT_X + RECT_W - grid_l_x, RECT_Y + i * grid_h); 

                 sprintf(str,"%.1f",(max_y - Y_Range*i/10));
                 len = strlen(str);
                 text_w = XTextWidth(curfont,str,len+2);
                 XDrawImageString(dpy,win,maingcreversed,
                            RECT_X - text_w,
                            RECT_Y + i*grid_h + curfontheight/3,
                            str,len);

       } /*end for loop*/


    for (i = 1; i < (number_of_items + 1); i++) {
       x = RECT_X + i * grid_w;
       XDrawLine(dpy, win, maingcreversed, 
		 x, RECT_Y, 
		 x, RECT_Y + grid_l_y); 
       XDrawLine(dpy, win, maingcreversed,
                 x, RECT_Y + RECT_H, 
                 x, RECT_Y + RECT_H - grid_l_y); 
    }

    /*
     * Draw titles
     */

    /* first graph title */
    len = strlen(graphic_title[0]);   
    text_w= XTextWidth(curfont, graphic_title[0], len);
    XDrawImageString(dpy, win, maingcreversed, (WIN_W - text_w)/2, RECT_Y/3,
                     graphic_title[0], len);

    /* second graph title */
    len = strlen(graphic_title[1]);
    text_w= XTextWidth(curfont, graphic_title[1], len);
    XDrawImageString(dpy, win, maingcreversed, (WIN_W - text_w)/2, RECT_Y*2/3, 
                     graphic_title[1], len);

    /* x-axis title */
    len = strlen(graphic_title[2]);
    text_w= XTextWidth(curfont, graphic_title[2], len);
    XDrawImageString(dpy, win, maingcreversed, 
		     (WIN_W - text_w)/2, RECT_H + RECT_Y * 3/2, 
		     graphic_title[2], len);
  
    /* y-axis title */
    len = strlen(graphic_title[3]);
    text_w=XTextWidth(curfont,graphic_title[3],len);
       if ((RECT_X-text_w) < 0)
           x = curfontwidth;
       else
           x = RECT_X-text_w;
    XDrawImageString(dpy, win, maingcreversed,
       x, RECT_Y - curfontheight,
       graphic_title[3],len);


    /*
     * Draw Labels on the X-Axis
     */

    width = (RECT_W - (PIX_SEP * (number_of_items + 1)))/number_of_items;
    labelsize = irint(floor(width/(double)curfontwidth)-1);
    y = RECT_Y + RECT_H + curfontheight;
    items = 0; 
    for (i=graphic_range[0].r1; i<=graphic_range[0].r2; i++)
    {
        p = lookat(i,graphic_range[0].c);
        if (!(p->label)) continue;

        x1 = RECT_X + ((items+1)*(grid_w));
        items++;
        strncpy(str, p->label, labelsize);
        str[labelsize] = '\0';
        len = strlen(str);
        text_w = XTextWidth(curfont, str, len);
        
        x = x1-(text_w / 2);
        XDrawImageString(dpy,win,maingcreversed,
                    x,y,str,len);
    } 


    /*
     *  Draw legends
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
             case 0: LDrawOpenSquare(x,y); 	break;
             case 1: LDrawOpenTriangle(x,y); 	break;
             case 2: LDrawOpenDiamon(x,y); 	break;
             case 3: LDrawCross(x,y); 		break;
             case 4: LDrawCloseSquare(x,y); 	break;
             case 5: LDrawCloseDiamon(x,y); 	break;
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
              LDrawOpenSquare(RECT_X/2 + legend_n*(WIN_W / curves_n - 10) - 10, 
                             RECT_Y * 7 / 4 + RECT_H);
              break;
           case 1:
              LDrawOpenTriangle(RECT_X/2 + legend_n*(WIN_W/curves_n - 10) - 10, 
                               RECT_Y * 7 / 4 + RECT_H);
              break;
           case 2:
              LDrawOpenDiamon(RECT_X/2 + legend_n * (WIN_W/curves_n - 10) - 10, 
                             RECT_Y*7/4 + RECT_H);
              break;
           case 3:
              LDrawCross(RECT_X/2 + legend_n*(WIN_W/curves_n - 10) - 10, 
                        RECT_Y*7/4 + RECT_H);
              break;
           case 4:
              LDrawCloseSquare(RECT_X/2 + legend_n*(WIN_W/curves_n - 10) - 10, 
                              RECT_Y*7/4 + RECT_H);
              break;
           case 5:
              LDrawCloseDiamon(RECT_X/2 + legend_n*(WIN_W/curves_n - 10) - 10, 
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
           items = 0; 

           for (j = graphic_range[0].r1, k = graphic_range[i].r1;
               (j <= graphic_range[0].r2) && (k <= graphic_range[i].r2); j++, k++) {
              if (!((p=lookat(j,graphic_range[0].c))->label)) continue;
              x = RECT_X + (items+1)*grid_w ;
              items++;

	      yent = lookat(j, graphic_range[i].c);
	      if (!(yent->flags & is_valid) || (yent->v < min_y) 
		  || (yent->v > max_y))
		continue;

	      if (first_point){
                x1 = x;
           	y1 = RECT_Y + RECT_H - RECT_H * (yent->v - min_y) / Y_Range;   
	        if ((format == 'S') || (format == 'B'))
	  	  switch(i){
		    case 1: LDrawOpenSquare(x1, y1); 	break;
                    case 2: LDrawOpenTriangle(x1, y1); 	break;
                    case 3: LDrawOpenDiamon(x1, y1); 	break;
              	    case 4: LDrawCross(x1, y1);      	break;
                    case 5: LDrawCloseSquare(x1, y1);	break;
                    case 6: LDrawCloseDiamon(x1, y1);	break;
                  }
		first_point = 0;
		continue;
	      }

              y = RECT_Y + RECT_H - RECT_H * (yent->v - min_y) / Y_Range;   

	      if ((format == 'L') || (format == 'B')) 
                XDrawLine(dpy, win, maingcreversed, x1, y1, x, y);
	      if ((format == 'S') || (format == 'B'))
		switch(i){
		  case 1: LDrawOpenSquare(x, y); 	break;
                  case 2: LDrawOpenTriangle(x, y); 	break;
                  case 3: LDrawOpenDiamon(x, y); 	break;
              	  case 4: LDrawCross(x, y);      	break;
                  case 5: LDrawCloseSquare(x, y);	break;
                  case 6: LDrawCloseDiamon(x, y);	break;
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

void LDrawOpenDiamon(x, y)
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

void LDrawCloseDiamon(x, y)
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

void LDrawOpenSquare(x, y)
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

void LDrawCloseSquare(x, y)
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

void LDrawOpenTriangle(x, y)
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


void LDrawCross(x, y)
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
 * Lpow10 calculates the power function of base 10
 */

double Lpow10(p)
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
 * Lrm_tail_zero removes heading white spaces, trailing white spaces, 
 * zero '0' and '.' of a string for nice printing of graph lable 
 *
 */

char *Lrm_tail_zero(s)
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
