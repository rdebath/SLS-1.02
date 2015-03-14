
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


/*************************************************************************
 *  This procedure displays a stacked bar graph for the values chosen    *
 *  by the user.  It displays one bar for each row chosen and 'stacks'   *
 *  one section for each column chosen.  It uses patterns located in     *
 *  the file pattern.h                                                   *
 *  Dan Gruber - December 1991                                           *
 *************************************************************************
 */

#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "plot.h"
#include "pattern.h"
#include "graphic_gvar.h"
#include "sc.h"
#include "scXstuff.h"
#include <math.h>
#define TITLE "XSPREAD-Grapher (Stacked Bar Plot)"
#define PIX_SEP 5

#ifdef mips
#define irint(x) rint(x)
#endif

#ifndef irint
#define irint(x) floor((x)+0.5)
#endif

  XWMHints   SBwmh = {
     (InputHint|StateHint),  /*flags*/
     False,                  /*input*/
     NormalState,            /*initial_state*/
     0,                      /*icon pixmap*/
     0,                      /*icon window*/
     0, 0,                   /*icon location*/
     0,                      /*icon mask*/
     0,                      /*Window group*/
  };

plot_stacked_bar()
   { int argc = 0;
    char **argv;

  
      double  max_y, min_y, Y_Range;

      int i,j,k;            /* Iteration variables                  */
      int grid_l_x;         /* The size of the grid separators      */
      int grid_l_y;
      int grid_h;
      int height, width;    /* Height and width of a bar            */
      char str[100];        /* The label to be drawn                */
      int text_w;           /* The width of a label in pixels       */
      int len;              /* The length of a label                */
      int x,y;              /* X and Y coordinates for the bar      */ 
      struct ent *p, *val;
      int looping = 1;      /* Continue looping variable            */
      int draw = 0;         /* Continue drawing variable            */
      int first_bar = 1;    /* first bar flag                       */
      int number_of_bars;   /* Total number of bars                 */
      int bars = 0;         /* Current bar                          */
      int labelsize;        /* Maximum size of label to be drawn    */ 
      double Row_Total[20]; /* Row Total                            */
      int Y_Position[20];   /* Position to draw next portion of bar */
      Pixmap Pattern;       /* Type of pattern to be drawn          */

    /*
     * Decide the scale for x and y axes
     */
  
  for (i=graphic_range[0].r1; i<=graphic_range[0].r2; i++) 
  {
     p=lookat(i,graphic_range[0].c);
     if(!(p->label)) continue;
     bars++;
     Row_Total[bars] = 0;
     Y_Position[bars] = 0;
  }

  if (bars < 1) 
  {
     fprintf(stderr,"\007");
     message("Not enough valid X labels");
     return;
  }

  number_of_bars = bars;

    if (g_auto_man[1] == 'M') {
       max_y = graphic_scale[1][1];
       min_y = graphic_scale[1][0];
    }    
    else {
       for (j=1; j < GRAPHRANGES; j++){
          if (*(graphic_range[j].col) == '\0') continue;
          bars = 0;
          for (i=graphic_range[j].r1; i<=graphic_range[j].r2; i++) 
	  {
             p=lookat(i,graphic_range[j].c);
            /* if(!(p->flags & is_valid)) continue; */
             bars++;
             Row_Total[bars] = Row_Total[bars] + (p->v);
             if (first_bar){
                 min_y = p->v;
                 first_bar = 0;
             } else 
                 min_y = Min(min_y, p->v);
          }
       }
       first_bar = 1;
       for (i=1; i<=bars; i++){
          if (first_bar){
             max_y = Row_Total[i];
             first_bar = 0;
          } else {
               max_y = Max(max_y, Row_Total[i]);
            }
       }
       min_y -= 0.1*(fabs(min_y));
       max_y += 0.1*(fabs(max_y));
    }
    max_y = ceil(max_y);
    min_y = floor(min_y);     

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
     * Fill out the XSizeHints struct to inform the window manager. See
     * Sections 9.1.6 & 10.3.
     */

    xsh.width = XTextWidth(curfont, TITLE, 80 + pad * 2);   
    xsh.flags = (PPosition | PSize);
    xsh.height = WIN_H;
    xsh.width =  WIN_W; 
    xsh.x = (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh.width) / 2;
    xsh.y = (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh.height) / 2;

    /*
     * Create the Window with the information in the XSizeHints, the
     * border width,  and the border & background pixels. See Section 3.3.
     */

    win = XCreateSimpleWindow(dpy, DefaultRootWindow(dpy),
			      xsh.x, xsh.y, xsh.width, xsh.height,
			      bw, bd, bg);

    /*
     * Set the standard properties for the window managers. See Section
     * 9.1.
     */

    XSetStandardProperties(dpy, win, TITLE, TITLE, None, argv,
			   argc, &xsh);
    XSetWMHints(dpy, win, &SBwmh);

    /*
     * Ensure that the window's colormap field points to the default
     * colormap,  so that the window manager knows the correct colormap to
     * use for the window.  See Section 3.2.9. Also,  set the window's Bit
     * Gravity to reduce Expose events.
     */

    xswa.colormap = DefaultColormap(dpy, DefaultScreen(dpy));
    xswa.bit_gravity = CenterGravity;
    XChangeWindowAttributes(dpy, win, (CWColormap | CWBitGravity), &xswa);

    /*
     * Map the window to make it visible.  See Section 3.5.
     */
   
     XMapWindow(dpy, win); 


     /*
     * Find out how big the window is now,  so that we can center
     * the text in it.
     */

    if (XGetWindowAttributes(dpy, win, &xwa) == 0){
       fprintf(stderr,"Stacked Bar Plot: Can't access attributes for graph window\n");
       exit(1);	
    }

    XClearWindow(dpy, win);
    XSelectInput(dpy,win, ButtonPressMask|KeyPressMask|ExposureMask);


    while (looping) {
	    XEvent event;

	    XNextEvent(dpy,&event);

	    switch(event.type)
	    {
		case KeyPress:
		case ButtonPress: looping = 0;
				  break;
		case Expose: draw = 1;
	    }
		       

	   if (!draw) continue;

           XDrawRectangle(dpy,win,maingcreversed,
			  RECT_X,RECT_Y,RECT_W,RECT_H);

          /* 
	   *  Calculate the length of the grid lines.  grid_l_x and 
	   *  grid_l_y are set to '0' if a line should not be drawn.
           */
 
           if  (graphic_grid == 'H') 
           {
             grid_l_x = RECT_W;
             grid_l_y = 0;
           }
           else if (graphic_grid == 'V')
           {
             grid_l_x = 0;
             grid_l_y = RECT_H;
           }  
           else if (graphic_grid == 'B')
           {
             grid_l_x = RECT_W;
             grid_l_y = RECT_H;
           }  
           else 
           {
             grid_l_x = 0;
             grid_l_y = 0;
           }  

           Y_Range = max_y - min_y; 

           /* The width of each bar */
           width = (RECT_W - (PIX_SEP * (number_of_bars)))/number_of_bars;

           grid_h = RECT_H /10;
         
	 
	 /* Draw the grid along with the labels on the Y-Axis */

	   for (i=0; i<11; i++) {
             if (i > 0 && i < 10)
	         XDrawLine(dpy,win,maingcreversed,
		       RECT_X-4, RECT_Y+i*grid_h,
		       RECT_X, RECT_Y+i*grid_h);
	     XDrawLine(dpy,win,maingcreversed,
		       RECT_X, RECT_Y+i*grid_h,
		       RECT_X+grid_l_x, RECT_Y+i*grid_h);
	     XDrawLine(dpy,win,maingcreversed,
		       RECT_X + RECT_W,RECT_Y+i*grid_h,
		       RECT_X+RECT_W - grid_l_x, RECT_Y+i*grid_h);

             sprintf(str,"%.1f",(max_y - Y_Range*i/10));
             len = strlen(str);
             text_w = XTextWidth(curfont,str,len+2);
             XDrawImageString(dpy,win,maingcreversed,
                       RECT_X - text_w,
                       RECT_Y + i*grid_h + curfontheight/3,
                       str,len);

	   } /*end for loop*/

	   for (i=1; i < number_of_bars; i++) {
             x = RECT_X + (PIX_SEP * i) + (width * i);
	     XDrawLine(dpy,win,maingcreversed,
		       x, RECT_Y, x, RECT_Y+grid_l_y);
	     XDrawLine(dpy,win,maingcreversed,
		       x, RECT_Y+RECT_H, x, RECT_Y+RECT_H-grid_l_y);
	   } /*end for loop*/

          /*
           *
           * Draw titles at top of graph 
           *
           */

	   len = strlen(graphic_title[0]);    
	   text_w = XTextWidth(curfont, graphic_title[0], len);
	   XDrawImageString(dpy,win,maingcreversed, 
			    (WIN_W - text_w)/2, RECT_Y/3, 
			    graphic_title[0], len);
	 

	   len = strlen(graphic_title[1]);
	   text_w = XTextWidth(curfont, graphic_title[1],len);
	   XDrawImageString(dpy,win,maingcreversed, 
			   (WIN_W - text_w)/2, RECT_Y*2/3,
			   graphic_title[1],len);

	   len = strlen(graphic_title[2]);
	   text_w=XTextWidth(curfont,graphic_title[2],len);
	   XDrawImageString(dpy,win,maingcreversed, 
			    (WIN_W-text_w)/2, RECT_H+RECT_Y*3/2,
			    graphic_title[2],len);

	   len = strlen(graphic_title[3]);
	   text_w=XTextWidth(curfont,graphic_title[3],len);
           if ((RECT_X-text_w) < 0) 
               x = curfontwidth;
           else
               x = RECT_X-text_w;
	   XDrawImageString(dpy,win,maingcreversed,
			    x, RECT_Y - curfontheight,
			    graphic_title[3],len);        

  /* --------------------------------------------------------------------*/
  /* This next section will read values of the x and y center points     */
  /* and find the maximum and minimum values for each, then set the      */
  /* width and height paramters, convert x and y so that the origin      */
  /* is at the lower left corner of the window, and then finally         */
  /* draw and fill the bars.                                             */
  /* ------------------------------------------------------------------- */

/*
 *  Draw Labels on the X-Axis
 */

           labelsize = irint(floor(width/(double)curfontwidth)-1);
	   y = RECT_Y + RECT_H + curfontheight;
	   bars = 0;
	   for (i=graphic_range[0].r1;i<=graphic_range[0].r2; i++)
	   {
	      p = lookat(i,graphic_range[0].c);
	      if (!(p->label)) continue;
              strncpy(str, p->label, labelsize);
              str[labelsize] = '\0';
	      x = RECT_X+(PIX_SEP*bars)+(width*bars)+(width/2);
              x = x - (curfontwidth*strlen(str))/2;
	      bars++;
	      XDrawImageString(dpy,win,maingcreversed,
		   	       x,y,
			       str,strlen(str));
	   }

        bars = 0;
        for (i=graphic_range[0].r1; i<=graphic_range[0].r2; i++) 
        {
           bars++;
           Y_Position[bars] = 0;
        }
 
        
        for (i = 1; i < GRAPHRANGES; i++){
            if (*(graphic_range[i].col) == '\0') continue;

            bars = 0;
            for(j=graphic_range[0].r1, k=graphic_range[i].r1;
	       (j<=graphic_range[0].r2) && (k<=graphic_range[i].r2); j++, k++){ 

	      if (!((p=lookat(j,graphic_range[0].c))->label)) continue;

	      x = RECT_X + ((PIX_SEP)*(bars)) + (width*bars);
	      bars++;


	      if (!((val=lookat(j,graphic_range[i].c))->flags & is_valid) 
	      || (val->v < min_y) || (val->v > max_y)) continue;

              /* 
	       *  Calculate the height of the bar.  If it is the 'lowest'
	       *  peice of the bar, subtract off the value of the min_y
	       *  for an accurate height.
               */

              if (i == 1) 
	         height = irint((val->v - min_y) / Y_Range * RECT_H);
              else 
	         height = irint(val->v / Y_Range * RECT_H);
           
	      y = RECT_Y + RECT_H - height - Y_Position[bars];  

              Y_Position[bars] = Y_Position[bars] + height;

              /* clear the area before drawing the bar */ 
              if (graphic_grid != 'C') 
                 XClearArea(dpy,win,x,y,width,height,False); 

              /* choose a pattern to be used */
              switch (i) {
                case 1 : Pattern = XCreateBitmapFromData(dpy,win,white_bits,
                             white_width, white_height);
                             break;
                case 2 : Pattern = XCreateBitmapFromData(dpy,win,gray3_bits,
                             gray3_width, gray3_height);
                             break;
                case 3 : Pattern = XCreateBitmapFromData(dpy,win,gray5_bits,
                             gray5_width, gray5_height);
                             break;
                case 4 : Pattern = XCreateBitmapFromData(dpy,win,gray2_bits,
                             gray2_width, gray2_height);
                             break;
                case 5 : Pattern = XCreateBitmapFromData(dpy,win,gray4_bits,
                             gray4_width, gray4_height);
                             break;
                case 6 : Pattern = XCreateBitmapFromData(dpy,win,gray1_bits,
                             gray1_width, gray1_height);
                             break; }

              /* Display a filled portion of the bar */
              XSetStipple(dpy, maingcreversed, Pattern); 
              XSetFillStyle(dpy, maingcreversed, FillStippled);
	      XFillRectangle(dpy,win,maingcreversed,x,y,width,height); 
              
	      /* Draw a solid outline around the bar */
              XSetFillStyle(dpy, maingcreversed, FillSolid);
	      XDrawRectangle(dpy,win,maingcreversed,x,y,width,height);
        }
     }
   }
   XUnmapWindow(dpy, win);
   XDestroyWindow(dpy, win);

}
