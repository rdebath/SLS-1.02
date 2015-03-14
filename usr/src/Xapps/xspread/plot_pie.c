
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
 *******************************************************************************
 * This procedure produces a Pie Graph for the selected values in the current  *
 * spreadsheet.  It calculates the percentage of each cell, and then draws a   *
 * Pie slice the size according to the percentage.  It uses patterns located   *
 * in the file pattern.h which were created using the bitmap editor.           * 
 * Dan Gruber - November 1991                                                  * 
 ******************************************************************************* 
 */

#include <math.h>
#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "plot.h" 
#include "graphic_gvar.h"
#include "sc.h"
#include "scXstuff.h"
#include "pattern.h"
#define TITLE "XSPREAD-Grapher (Pie Plot)"
#define PIE_D 300   /* Pie Diameter */
#define PIE_R 150   /* Pie Radius   */
#define PI       3.14159265358979323846

#ifdef mips
#define irint(x) rint(x)
#endif

#ifndef irint
#define irint(x) floor((x)+0.5)
#endif

/*
 * This structure forms the WM_HINTS property of the window,
 * letting the window manager know how to handle this window.
 * See Section 9.1 of the Xlib manual.
 */

   XWMHints     MyWMHints = {
	  (InputHint|StateHint),   /* flags         */
          False,                   /* input         */
          NormalState,             /* initial_state */
	  0,                       /* icon pixmap   */
	  0,                       /* icon window   */
          0, 0,                    /* icon location */
          0,                       /* icon mask     */
          0,                       /* Window group  */
   };


plot_pie()
{   
    int i, j, k, len;
    int text_w;
    int X, Y, X1, Y1, Temp_X,Temp_Y;
    struct ent *p, *val, *next_val;
    int  looping = 1;             /* true if we shouldn't exit yet           */
    int draw  = 0;                /* true if we should paint the window      */
    int cells = 0; 
    int slice;                    /* Slice number                            */
    double Range_Total = 0;       /* Total of all of the cell values         */
    double Percentage = 0;        /* Percentage of the slice                 */ 
    double Angle_Size = 0;        /* Angle size of the slice                 */
    double Starting_Angle = 0;    /* Starting position for each slice        */
    double Rad_Angle = 0;         /* The angle in radians                    */ 
    double Bisector_Angle = 0;    /* The 'middle' of the angle               */
    double Label_R = 0;           /* Radius of the circle for drawing labels */
    Pixmap Pattern;               /* Pattern for a given slice               */
          
    char str[100];
    char per_str[10];
    register int c;

    int argc = 0;
    char **argv;



/* check that we have enough valid x values.  While we are looping,
 * determine the minimum and maximum x values, in case we need them
 */

   Range_Total = 0;
   for (i=graphic_range[0].r1; i <= graphic_range[0].r2; i++) { 
     p = lookat( i, graphic_range[0].c);
     val = lookat( i, graphic_range[1].c);
     if (!(p->label) || !(val->v)) continue;
       {
           Range_Total = Range_Total + val->v;
           cells++;
       } 
   } /* for i */

   if (cells < 1) {
     fprintf(stderr,"\007");
     message("Not enough valid Labels (X values)");
     return;
   }

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
   XSetWMHints(dpy, win, &MyWMHints);

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
      fprintf(stderr,"plot_pie: Can't access attributes for graph window\n");
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


    /* 
     * if not drawing, go back and get another event 
     */

    if (!draw) continue;

    /*
     * Draw titles
     */

    /* first graph title */
    len = strlen(graphic_title[0]);   
    text_w= XTextWidth(curfont, graphic_title[0], len);
    XDrawImageString(dpy, win, maingcreversed, (WIN_W-text_w)/2, RECT_Y/3,
                     graphic_title[0], len);

    /* second graph title */
    len = strlen(graphic_title[1]);
    text_w= XTextWidth(curfont, graphic_title[1], len);
    XDrawImageString(dpy, win, maingcreversed, 
		     (WIN_W - text_w)/2  , RECT_Y * 2/3,
                     graphic_title[1], len);

    Starting_Angle = 0;
    slice = 1;
    for (i=graphic_range[0].r1,j=graphic_range[1].r1; 
        (i<=graphic_range[0].r2) && (j<=graphic_range[1].r2); i++, j++)
    {  
       
      if (!((p=lookat(i,graphic_range[0].c))->label))
        continue;

      val=lookat(i,graphic_range[1].c);

      /* Calculate the percentage of the slice */
      Percentage = (val->v / Range_Total);
     
      /* 
       * If this is the last slice to be drawn, take '360 - Starting_Angle'
       * in case there is round-off errors.   
       */

      if (i == graphic_range[0].r2) 
         Angle_Size = 360 - irint(Starting_Angle);
      else 
         Angle_Size = Percentage * 360;

      /* Choose a pattern for drawing (They are located in pattern.h) */
      switch (slice) {
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
                     break;
        case 7 : Pattern = XCreateBitmapFromData(dpy,win,black_bits,
                     black_width, black_height);
                     break;
        case 8 : Pattern = XCreateBitmapFromData(dpy,win,diag_r_bits,
                     diag_r_width, diag_r_height);
                     break;
        case 9 : Pattern = XCreateBitmapFromData(dpy,win,dark_wide_weave_bits,
                     dark_wide_weave_width, dark_wide_weave_height);
                     break;
        case 10: Pattern = XCreateBitmapFromData(dpy,win,light_diamond_bits,
                     light_diamond_width, light_diamond_height);
                     break;
        case 11: Pattern = XCreateBitmapFromData(dpy,win,diag_l_bits,
                     diag_l_width, diag_l_height);
                     break;
        case 12: Pattern = XCreateBitmapFromData(dpy,win,light_root_weave_bits,
                     light_root_weave_width, light_root_weave_height);
                     break;
        case 13: Pattern = XCreateBitmapFromData(dpy,win,vert_lines_bits,
                     vert_lines_width, vert_lines_height);
                     break;
        case 14: Pattern = XCreateBitmapFromData(dpy,win,dark_root_weave_bits,
                     dark_root_weave_width, dark_root_weave_height);
                     break;
        case 15: Pattern = XCreateBitmapFromData(dpy,win,checker_bits,
                     checker_width, checker_height);
                     break;
        case 16: Pattern = XCreateBitmapFromData(dpy,win,dark_cross_weave_bits,
                     dark_cross_weave_width, dark_cross_weave_height);
                     break;
        case 17: Pattern = XCreateBitmapFromData(dpy,win,dark_diamond_bits,
                     dark_diamond_width, dark_diamond_height);
                     break;
        case 18: Pattern = XCreateBitmapFromData(dpy,win,light_cross_weave_bits,
                     light_cross_weave_width, light_cross_weave_height);
                     break;
        case 19: Pattern = XCreateBitmapFromData(dpy,win,horiz_lines_bits,
                     horiz_lines_width, horiz_lines_height);
                     break;
        case 20: Pattern = XCreateBitmapFromData(dpy,win,light_wide_weave_bits,
                     light_wide_weave_width, light_wide_weave_height);
                     break;
        
      };
      
      /* Set the fill style to allow for drawing patterns */ 
      XSetStipple(dpy,maingcreversed,Pattern);
      XSetFillStyle(dpy, maingcreversed, FillStippled);
      
      /* Draw a single pie slice */
      XFillArc(dpy, win, maingcreversed,
            (WIN_W/2)-(PIE_R), (WIN_H/2)-(PIE_R)*4/5, PIE_D, PIE_D,
            irint(Starting_Angle)*64,irint(Angle_Size)*64); 

      /* Set fill style back to solid for drawing lines */
      XSetFillStyle(dpy, maingcreversed, FillSolid); 
       
      /* 
       * This section converts the Starting_Angle from degrees to radians 
       * and then calculates the proper positions for drawing the line
       * separators, and then draws them.
       */ 

      Rad_Angle = irint(Starting_Angle) * PI / 180;
      Temp_X = (PIE_R) * cos(Rad_Angle);
      Temp_Y = -((PIE_R) * sin(Rad_Angle));
      X = Temp_X + (WIN_W/2);
      Y = Temp_Y + (WIN_H/2) + (PIE_R/5);
      XDrawLine(dpy,win,maingcreversed,WIN_W/2,(WIN_H/2 + PIE_R/5),X,Y); 

      /* 
       * This section draws the label for the given slice by calculating the
       * angle bisector of the slice and then draws the Labels outside the
       * Pie a distance of one fifth the radius of the Pie.
       */

      Label_R = PIE_R + (PIE_R/5);
      Bisector_Angle = irint(Starting_Angle) + irint(Angle_Size/2);
      Rad_Angle = Bisector_Angle * PI / 180;
      Temp_X = (Label_R) * cos(Rad_Angle);
      Temp_Y = -((Label_R) * sin(Rad_Angle));
      X1 = Temp_X + (WIN_W/2);
      Y1 = Temp_Y + (WIN_H/2) + (PIE_R/5);
      strncpy(str,p->label,strlen(p->label));
      len = strlen(p->label); 
      str[len] = '\0';
      sprintf(per_str,"%.1f",(Percentage * 100));
      strcat(str, " (");
      strcat(str,per_str);
      strcat(str,"%)");
      len = strlen(str);
      text_w = XTextWidth(curfont, str, len);
      
      /* If the slice is on the left side of the Pie, make proper adjustment */
      if ((Bisector_Angle > 90) && (Bisector_Angle <= 270))
          X = X1 - text_w;
      else
          X = X1;
      Y = Y1 + curfontheight/2;
      XDrawImageString(dpy,win,maingcreversed,X,Y,str,len);
  
      /* Calculate the next Starting_Angle */
      Starting_Angle = irint(Starting_Angle) + irint(Angle_Size);
      slice++;
    } 

    /* Draw a complete circle around the Pie */
    XSetFillStyle(dpy, maingcreversed, FillSolid); 
    XDrawArc(dpy, win, maingcreversed,(WIN_W/2)-(PIE_R),(WIN_H/2)-(PIE_R)*4/5, 
             PIE_D, PIE_D, 0, 360*64); 
  } /* end of while */  

  XUnmapWindow(dpy, win);
  XDestroyWindow(dpy, win);
}
