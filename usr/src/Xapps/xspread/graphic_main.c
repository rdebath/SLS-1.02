
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

/* Modified by Dan Gruber to run Line, Bar, XY, Stack Bar and
   Pie graphs.  Mike Frey modified the Graph Menu so that if
   <ESC> is pressed, it will bounce up one.  December 1991 */

/* B. Backman 7-29-91.  Appended other graphic calls to this
 * file instead of (ugh) #include-ing (choke, choke) them.
 * Also, fixed the atrocious spelling and grammar of Tuan Tang! */

/* ************************************************** */
/* Programer: <Tuan Tang>    Date: July 24, 1990      */
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
/* This is the main function of graphics program.     */
/* This function will display  the main graphic menu, */
/* and branch to the appropriate function once the    */
/* item in the menu is selected, It also include a    */
/* list of      */
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

/* modified 7-24-91, B.Backman.  Commented out curses
   calls and replaced them with X equivalents and 
   functions defined in scXstuff.c */
/* 7-31-91 B. Backman -- Removed illegal comparisons of char-pointers to 
   chars */
#include <signal.h>
/*#include <curses.h> */


#ifdef BSD42
#include <strings.h>
#else
#ifndef SYSIII
#include <string.h>
#endif
#endif

#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include <stdio.h>
#include <ctype.h> 
#include "sc.h"  
#include "scXstuff.h" 
#include "graphic_gvar.h"

extern plot_XY();
extern plot_bar();
extern plot_line();
extern plot_pie();
extern plot_stacked_bar();

Graph_Menu()
{
     static  char *mainmenu[] =
                   {
                    "Type",
                    "X","A","B","C","D","E","F",
                    "Reset",
                    "View",
                    "Options",
                   };

     static  char *help[] =
                   {
/* Type */          "Type of the graph",
/* X */             "Set data range for X",
/* A */             "Set data range for A",
/* B */             "Set data range for B",
/* C */             "Set data range for C",
/* D */             "Set data range for D",
/* E */             "Set data range for E",
/* F */             "Set data range for F",
/* Reset */         "Cancel graph or range settings",
/* View */          "View the graph on the monitor",
/* Options */       "Graph output options",
                   };

/* auto. */  unsigned int  choice=0;
/* auto. */  char msg[100];

    do {     /* do until ESC is selected */
       choice = menu(GRAPHITEMS,mainmenu,help); 
       switch(choice) {    
                      case  0: Main_Menu();
			       break; /* exit */
       /* Type */     case  1: graphic_typefn();
                               break;
       /* X */        case  2: 
       /* A */        case  3:
       /* B */        case  4: 
       /* C */        case  5:
       /* D */        case  6: 
       /* E */        case  7:
       /* F */        case  8: graphic_grangefn(choice,mainmenu); 
                               break;
       /* Reset */    case  9: graphic_resetfn();
                               break;
       /* View */     case 10: graphic_view();                              
                               break;
       /* Options */  case 11: graphic_optionsfn(); 
                               break;
                      default: sprintf(msg,"Invalid data arrived here !");
                               (void) message(msg);
                               break;
                      } /* end of switch */
      } while ( choice != NULL );  /* end of do */
}


/* =============================================== */
/* This function displays the input data or message*/
/* at the first line of the spread sheet screen    */
/* for approximately one second.                   */
/* =============================================== */

message(x)  

char *x;        /* string for display */
{
   clearlines(0,0);
   XDrawImageString(dpy, mainwin, maingc,
		    textcol(0), textrow(0),
		    x, strlen(x));
   XFlush(dpy);      /* force it onto the screen */
   sleep(1);         /* delay 1 second */
   clearlines(0,0);
} /* end of message */

/* ******************************************************* */
/* Programer: <Tuan Tang>  Date: August 05, 1990           */
/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* This function sets the numeric scales for the X axis    */
/* and Y axis, Automatic is the default scale while manual */
/* allows the user specified output data range.            */
/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */

graphic_XYscalefn(c,m) 

unsigned int  c;    /* 1 for X scale, 2 for Y scale */
         char *m[]; /* array of itmes from graphic_scalefn */
{
     static char *submenu[] =
                 {
                  "Automatic",
                  "Manual", 
                  "Lower",
		  "Upper",
                 };

     static char *help[] =
                 {
/* Automatic */  "Use the max. and min. values of data ranges to set the scale",
/* Manual */     "Allow the user to specify the range of data for display",
/* Lower */      "Set lower limit",                        
/* Upper */      "Set upper limit",
                 };

/* auto. */ unsigned int  choice;
/* auto. */ char msg[100];

   while ( (choice = menu(GRAPHXYSCALES,submenu,help)) != NULL )
         switch(choice) {
			case  0: graphic_optionsfn();
				 break;
			case  1: g_auto_man[c-1] = 'A';
                                 sprintf(msg,"Graph scale is Automatic"); 
                                 (void) message(msg);
				 break;
			case  2: g_auto_man[c-1] = 'M';
                                 sprintf(msg,"Graph scale is Manual");
                                 (void) message(msg);
				 break;
                        case  3: 
                        case  4: graphic_srangefn(c,m,choice);
				 break;
                        default: sprintf(msg,"Invalid data arrived here!");
                                 (void) message(msg);
				 break;
                        }  /* end of switch */
} /* end of function */


double gatofn(s)    /* convert string s to double */
/* *********************************** */
/* This was copied from system library */
/* *********************************** */

char  s[];
{
    double val, power;
    int i, sign;

    for (i=0; s[i]==' ' || s[i]=='\n' || s[i]=='\t'; i++)
        ;           /* skip white space */
    sign = 1;
    if (s[i] == '+' || s[i] == '-')   /* sign */
         sign = (s[i++]=='+') ? 1 : -1;
    for (val = 0; s[i] >= '0' && s[i] <= '9'; i++)
         val = 10 * val + s[i] - '0';
    if (s[i] == '.')
       i++;
    for (power = 1; s[i] >= '0' && s[i] <= '9'; i++) {
         val = 10 * val + s[i] - '0';
         power *= 10;
        }
    return(sign * val / power);
}


/* ************************************************** */
/* Programer: <Tuan Tang>    Date: August 04, 1990    */
/* -------------------------------------------------- */
/* Funtion graphic_formatfn sets the output format    */
/* of XY & line graphs. The format can be set for     */
/* each data range or for all data ranges.            */
/* To set one data range at a time select A,B,...,    */
/* or F.  And of all data ranges select Graph.        */
/* -------------------------------------------------- */

graphic_formatfn()

{

   static char *submenu[] =
                {
                 "Graph",
                 "A","B","C","D","E","F",
                };

   static char *help[] =
                {
/* Graph */      "Change the format of the entire graph",
/* A */          "Change the format of A range",                     
/* B */          "Change the format of B range",                     
/* C */          "Change the format of C range",                     
/* D */          "Change the format of D range",                     
/* E */          "Change the format of E range",                     
/* F */          "Change the format of F range",                     
                 };

   unsigned int   choice;
	    char  msg[100];

   while ( (choice = menu(GRAPHFORMATS,submenu,help) ) != NULL )
         {            
           if ( choice > GRAPHFORMATS )
	      {
               sprintf(msg,"0 <= Input Data < %1d ",GRAPHFORMATS);
	       (void) message(msg);
	      }
           else
              graphic_formatst(choice-1);   /* index of array */
          } /* end of while */

}  /* end of function */
/* ************************************************ */
/* Programer: <Tuan Tang>    Date: August 04, 1990  */
/* ------------------------------------------------ */
/* Funtion graphic_formatst sets the output format  */
/* of line & XY graphs. There are four options.     */
/*    1. Lines  :this option connects each data     */
/*                point with a straight line.       */
/*    2. Symbol :display each range data point with */
/*               the same symbol.  There is a       */
/*               different symbol for each range.   */
/*    3. Both   :select both Lines and Symbols.     */
/*    4. Neither:Will allow you to select the label */
/*               for data range.                    */
/* ------------------------------------------------ */

graphic_formatst(idx)

unsigned int  idx;    /* type of data range(s) */

{

   static char *submenu[] =
                {
                 "Lines",
                 "Symbols",
                 "Both",
                 "Neither",
                };

   static char *help[] =
                {
/* Lines */      "To connect each range data point with a straight line",
/* Symbols */    "To display each range data points with a symbol",
/* Both */       "To select both Lines and Symbols",
/* Neither */    "Allow you to enter the label for each data range",
                 };

   unsigned int   choice;
            char  msg[100];
            int   i;
   choice = menu(GRAPHSYMBOLS,submenu,help);
         switch(choice) {
			case  0: graphic_optionsfn();
				 break;
                        case  1: graphic_format[idx] = 'L';
                                 break;
                        case  2: graphic_format[idx] = 'S';
                                 break;
                        case  3: graphic_format[idx] = 'B';
                                 break;
                        case  4: graphic_format[idx] = 'N';
                                 break;
                        default: sprintf(msg,"Input error for format!");  
				 message(msg);
                                 break;
                        }    /* end switch */
   if ( 1 <= choice && choice <= GRAPHSYMBOLS )  /* print message */
      {
	sprintf(msg,"The graph format is %s",submenu[choice-1]);
	(void) message(msg);
       }	 
    
   /* set the format for the entire graph by setting each */
   /* data range to the same type of graph format         */
   if ( idx == 0 )   
      for (i=1; i< GRAPHFORMATS; i++){
          graphic_format[i] = graphic_format[idx];
      }
  
}  /* end of function */





/* *************************************************** */
/* Programer: <Tuan Tang>     Date: July 24, 1990      */
/* ``````````````````````````````````````````````````` */
/* Sets the range for a particular data range. There   */
/* are seven different data ranges allowed.  They are  */
/* labeled as X, A, B, C, D, E, and F.                 */
/* ``````````````````````````````````````````````````` */

#define  CLIM 3   /* characters allow for column address */
#define  RLIM 4   /* digits allow to enter for row # */

graphic_grangefn(c,m)

 int    c;    /* an integer, index to a data set */
 char   *m[]; /* array of items */

{
  char s[100];   /* stored number to be converted */
  char msg[100]; /* message for display */
  int  ierr;     /* 1 = No error on input */
  int  colm;     /* column number from label entered */
  int  i;        /* miscellaneous index */
  
      /* accept column address for graph range */
      do {
          ierr = 1;
          sprintf(s,"Input column label for range %s-- 2 character max.: ",m[c-1]);
          strcpy(graphic_range[c-2].col,get_str(s,CLIM));

	  /* convert label in "base 26" to column number */
	  /* set ierr=0 if an error occurs */
	  for(i=colm=0; (i<CLIM) && (s[i] != '\0') && ierr ; i++){
	    if ( !isalpha(s[i]) && !isspace(s[i])) {
	      ierr = 0;
	      continue;
            }
	    if (isspace(s[i])) continue;
	    if ( islower(s[i]))
	      s[i] = toupper(s[i]);
	    colm = (colm * 26) + (s[i] - 'A' + 1);
	  }
	  if (colm == 0)
	    ierr = 0;  /* input was all spaces */
          else
	    colm--;    /* adjust to zero-based system */

          if (ierr == 0 )
            {
             sprintf(msg,"Input must be character(s)");
             (void) message(msg);
             graphic_range[c-2].col[0] = '\0';
            }
         } while ( ierr == 0 );
      graphic_range[c-2].c = colm;
     
      do {           
	 clearlines(0,0);  /* clear from row 0 to row 0 */
         ierr = 1;
         sprintf(s,"Input starting row number, 200 max., for range %s : ",m[c-1]);
         graphic_range[c-2].r1 = atoi( get_str(s,RLIM) );
         if ( (isdigit(s[0]) == NULL) || (graphic_range[c-2].r1 > 200) )     
           {
            sprintf(msg,"Input error !");
            (void) message(msg);
            ierr = NULL;
           } /* end if */
         } while ( ierr  == NULL );  /* end do */

      do {
         ierr = 1;   /* No input error */
         sprintf(s,"Input ending row number >= %2d for range %s ",
                    graphic_range[c-2].r1,m[c-1]);
         graphic_range[c-2].r2 = atoi( get_str(s,RLIM) );
         if ( (isdigit(s[0]) == NULL) || (graphic_range[c-2].r2 < 
               graphic_range[c-2].r1) )
           {
            sprintf(msg,"Input error !");
            (void) message(msg);
            ierr = NULL;
           } /* end if */
         } while ( ierr == NULL );  /* end do */
   /* B. Backman 7-31-91  The following won't work if user enters a 
      capital letter or a double-letter column such as 'aa'.  It will 
      need to be fixed if we have time */
}
  

/* ********************************************* */
/* Programer: <Tuan Tang>  Date: August 04, 1990 */
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
/* Function graphic_gridfn adds or removes grid  */
/* lines on the graph display.  Grid lines can   */
/* not be used for pie charts.                   */
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

#define  GRIDS   4

graphic_gridfn()
{
     static  char *submenu[] =  /* stored reset options */
                   {
                    "Horizontal",
                    "Vertical",                 
                    "Both",
                    "Clear",
                   };
                   
     static  char *help[] = /* help menu for reset options */
                   {
/* Horizontal */    "Horizontal grid lines appear across the graph",
/* Vertical */      "Vertical grid lines appear across the graph",
/* Both */          "Both horizontal and vertical lines appear",
/* Clear */         "Erases all the grid lines"
                   };

    unsigned int  choice;
             char msg[100];

    choice = menu(GRIDS,submenu,help);
    switch(choice) {
/* Exit */	   case  0: graphic_optionsfn();
			    break;
/* Horizontal */   case  1: graphic_grid = 'H';
                            sprintf(msg,"Add Horizontal grids on the graph");
                            (void) message(msg);
                            break;
/* Vertical */     case  2: graphic_grid = 'V';
                            sprintf(msg,"Add Vertical grids on the graph");
                            (void) message(msg);
                            break;
/* Both */         case  3: graphic_grid = 'B';
                            sprintf(msg,"Add grids on X & Y axes");
                            (void) message(msg);
                            break;
/* Clear */        case  4: graphic_grid = 'C';
                            sprintf(msg,"Remove grids on the graph");
                            (void) message(msg);
                            break;
                   default: sprintf(msg,"0 <= Input Data <= %1d ",GRIDS);
                            (void) message(msg);
                            break;
                   } /* end of switch */
} /* end of function */


/* ********************************************** */
/* Programer: <Tuan Tang>   Date: August 09, 1990 */
/* ---------------------------------------------- */
/* Function graphic_init initializes the          */
/* global variables use by the graphic functions  */
/* These variable could be found in graphic**.h   */
/* ********************************************** */

graphic_init()

{
  int i,j;

  graphic_type = 'L';  /* graph format is line initialy */
  graphic_grid = 'C';  /* clear grid line */
  graphic_skip = 0;    /* do not skip points */
   
  for (i=0; i < GRAPHLEGENDS; i++)
      graphic_legend[i][0] = '\0';   /* no legend assign to them yet */

  for (i=0; i < GRAPHFORMATS; i++)   /* Line & Symbol */
      graphic_format[i] = 'B';
  
  for (i=0; i < GRAPHTITLES; i++)    /* no title been assigned to them */
      graphic_title[i][0] = '\0';

  g_auto_man[0] = 'A';  /* Scaling is automatic */
  g_auto_man[1] = 'A';

  for (i=0; i< (GRAPHSCALES-1); i++) /* none */
      for (j=0; j < 2; j++)
          graphic_scale[i][j] = (double) 0.0;

  for (i=0; i<GRAPHRANGES; i++)
    graphic_range[i].col[0] = '\0';
}   


/* ***************************************************** */
/* Programer: <Tuan Tang>  Date: August 01, 1990         */
/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* This function allows you to enter the label for each  */
/* graph. The label is used to identify what each symbol */
/* color, or crosshatching represents in the graph.      */
/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */

#define  MAXLEN  40 

graphic_legendfn()

{
     static char *submenu[] =
                 {
                   "A","B","C","D","E","F", 
                 };

     static char *help[] =
                 {
                  "Set legend for A",
                  "Set legend for B",
                  "Set legend for C",
                  "Set legend for D", 
                  "Set legend for E",
                  "Set legend for F",
                 };

/* auto. */ unsigned int  choice;
/* auto. */          char msg[100];
    
   while ( (choice = menu(GRAPHLEGENDS,submenu,help)) != NULL ) 
     {
       (void) sprintf (msg,"Enter the legend for %s : ",submenu[choice-1]);
       strcpy(graphic_legend[choice-1],get_str(msg, MAXLEN)); 
       sprintf(msg,"The legend for %s  is:  %s ",submenu[choice-1],
               graphic_legend[choice-1]); 
       (void) message(msg); 
     }
}
/* ******************************************************* */
/* Programer: <Tuan Tang>     Date: August 01, 1990        */
/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */
/* Sets the options for graphs.  There are eight options   */
/* and they are Legend, Format, Titles, Grid, Scale, Color */
/* B&W and Data-Labels.                                    */
/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

graphic_optionsfn()
{
     static char *submenu[] =
                  {
                   "Legend",
                   "Format",
                   "Titles",
                   "Grid",
                   "Scale",
                  };

     static char *help[] =
                  {
/* Legend */       "Label to identify what each symbol represents",
/* Format */       "Use to change the appearance of line and XY graphs",
/* Title */        "Assigns a title to each axis or to an entire graph",
/* Grid */         "Adds or removes grid lines on the graph display",
/* Scale */        "Set numeric scales for X axis and Y axis",
                  };

/* auto. */ unsigned int  choice;
/* auto. */          char msg[100];

    do {  /* do until escape is selected */
        choice = menu(GRAPHOPTIONS,submenu,help);
        switch(choice) {
/* Legend */           case  1: graphic_legendfn();
                                break;
/* Format */           case  2: graphic_formatfn(); 
                                break;
/* Title */            case  3: graphic_titlesfn(); 
                                break;
/* Grid */             case  4: graphic_gridfn(); 
                                break;
/* Scale */            case  5: graphic_scalefn();
                                break;
/* Quit */             case  0: break;    /* exit do loop */
                       default: sprintf(msg,"Invalid data arrived here!");
                                (void) message(msg);
                                break;
                      }
       } while ( choice != 0 ); 
 }


/* ********************************************* */
/* Programer: <Tuan Tang>  Date: July 25, 1990   */
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
/* This function sets the range  for the graph.  */
/* The range  can be set for the entire graph    */
/* or for each individual set of value.          */
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

graphic_resetfn()
{
     static  char *submenu[] =  /* stored reset options */
                   {
                    "Graph",
                    "X","A","B","C","D","E","F",
                   };
                   
     static  char *help[] = /* help menu for reset options */
                   {
/* Graph */         "Cancel the range settings for current graph",
/* X */             "Reset the range for X",
/* A */             "Reset the range for A",
/* B */             "Reset the range for B",
/* C */             "Reset the range for C",
/* D */             "Reset the range for D",
/* E */             "Reset the range for E",
/* F */             "Reset the range for F",
                   };

/* auto. */  unsigned int  choice;
/* auto. */           char msg[100];
/* auto. */           int  i;

    do {
       choice = menu(GRAPHRESETS,submenu,help);
       if ( choice > GRAPHRESETS )
          {
          sprintf(msg,"Invalid data arrived here !");
          (void) message(msg);
          }
       else if ( choice != 0 )
          {
          if ( choice == 1 ) {
            for (i=1; i< GRAPHRESETS; i++)
                graphic_range[i].col[0] = '\0'; /* set all ranges to NULL */
            sprintf(msg,"Clear the ranges for entire graph !");
          }
          else {
            graphic_range[choice-2].col[0] = '\0'; /* set data range to NULL */
            sprintf(msg,"Clear %s data range ",submenu[choice-1]); 
	  }
          (void) message(msg);
         }
      } while ( choice != NULL );
}


/* ******************************************************* */
/* Programer: <Tuan Tang>  Date: August 05, 1990           */
/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* This function sets the numeric scales for the X axis    */
/* and Y axis, and specifies the skip factor for X axis    */
/* labels.  The skip factor n is the number of points skip */
/* for every point appears on the graph.                   */
/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */

graphic_scalefn()

{
     static char *submenu[] =
                 {
                  "X scale",
                  "Y Scale",
                  "Skip",
                 };

     static char *help[] =
                 {
/* X scale */     "Alter the scale of X axis",                        
/* Y scale */     "Alter the scale of Y axis",
/* Skip */        "Skip n points for every point plotted",
                 };

/* auto. */ unsigned int  choice;
/* auto. */          char msg[100];

   while ( (choice = menu(GRAPHSCALES,submenu,help)) != NULL )
         switch(choice) {
		 case  0: graphic_optionsfn();
			  break;
	         case  1: 
		 case  2: graphic_XYscalefn(choice,submenu);
		          break;
                 case  3: graphic_skipscale();
		          break;
                 default: sprintf(msg,"0<= Input Value <=%1d",GRAPHSCALES);
                          (void) message(msg);
			  break;
                        }  /* end of switch */
} /* end of function */

/* ************************************************ */
/* Programer: <Tuan Tang>   Date: August 05, 1990   */
/* ================================================ */
/* This function accepts a number.  This is the     */
/* number of points will be skip between the two    */
/* points that are plotted.                         */
/* ================================================ */


graphic_skipscale()

{
  char msg[100];
  int  ierr = 1;
  while ( ierr == NULL )
  {
   ierr = 1;
   sprintf(msg,"Input skip factor ##, positive integer only : ");
   get_str(msg,3);
   if ( isdigit(msg[0]) == NULL )
      {
        message("Input Error!");
        ierr = NULL;
       }
   else
      graphic_skip = atoi(get_str(msg,3));
  }
  sprintf(msg," %2d ",graphic_skip);
  message(msg);

}


/* *************************************************** */
/* Programer: <Tuan Tang>     Date: July 24, 1990      */
/* ``````````````````````````````````````````````````` */
/* Sets the range of X scale or Y scale.  It is being  */
/* called from graphic_scalefn().                      */
/* ``````````````````````````````````````````````````` */

#define  GMAXLEN  40

graphic_srangefn(c1,m,c2)

int    c1;    /* indicate whether it is X scale or Y scale */
char   *m[];  /* array of items: X, Y scale and Skip */
int    c2;    /* indicate whether it is Lower or Upper limit */

{
  double gatofn();
  char s[GMAXLEN];   /* stored number to be convert */

  if ( c2 == 3 )   /* Lower limit */
     {
     sprintf(s,"Input Lower limit for %s ",m[c1-1]);
     get_str(s,GMAXLEN);
     graphic_scale[c1-1][0] = gatofn(s);
     }
  else if ( c2 == 4 ) /* Upper limit */
     {
     sprintf(s,"Input Upper limit for %s ",m[c1-1]);
     get_str(s,GMAXLEN);
     graphic_scale[c1-1][1] = gatofn(s);
     }
}
  

/* ******************************************************* */
/* Programer: <Tuan Tang>  Date: August 04, 1990           */
/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */
/* This function allows you to enter four different titles */
/* for the graph.  The First(main) title and Second(sub)   */
/* title will appear on the top of the graph. The X-Axis   */
/* title will appear on the x-axis and Y-Axis title on     */
/* the y-axis of the graph.                                */
/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */

#define  MAXLEN  40 

graphic_titlesfn()

{
     static char *submenu[] =
                 {
                  "First",
                  "Second",
                  "X-Axis",
                  "Y-Axis",
                 };

     static char *help[] =
                 {
/* First */       "Main title, will show up on the top of the graph",
/* Second */      "Subtitle, will appear below the Main title",
/* X-Axis */      "Label for x-axis",
/* Y-Axis */      "Label for y-axis",
                 };

/* auto. */ unsigned int  choice;
/* auto. */          char s[MAXLEN];
/* auto. */          char msg[100];

   while ( (choice = menu(GRAPHTITLES,submenu,help)) != NULL )
     {
      if ( choice > GRAPHTITLES )
        {
         sprintf(msg,"0 <= Input Value < %1d ",GRAPHTITLES);
         (void) message(msg);
         }
      else {
           sprintf(s,"Enter %s  title: ",submenu[choice-1]);
           strcpy(graphic_title[choice-1],get_str(s,MAXLEN));
           }  /* end of else */
      }  /* end of while */
} /* end of function */


/* ************************************************** */
/* Programer: <Tuan Tang>     Date: July 24, 1990     */
/* -------------------------------------------------- */
/* This header file declares and initializes the type */
/* of the graph and it is executed only during the    */
/* compilation of the program                         */
/* -------------------------------------------------- */

graphic_typefn()
{
     static  char *submenu[] = 
                  {
                   "Line",
                   "Bar",
                   "XY",
                   "Stack Bar",
                   "Pie",
                  };


    static   char *help[] = 
                  {
                   "Represents each value with a point",
                   "Represents each value with a bar of varying height",
                   "Plot a pair of xy values with a point",
                   "The value of one data range stack on the top of the other",
                   "The pie chart compares parts to the whole--a circle shape",
                  };

/* auto. */  unsigned int choice;
/* auto. */           char msg[100];

   choice = menu(GRAPHTYPES,submenu,help);
/* the following ifdef is to "comment out" something w/ comments in it */
#ifdef NEVER_DEFINED
   switch(choice){
                 case  0: break; /* exit */
                 case  1: graphic_type = 'L'; 
                          break;
                 case  2: graphic_type = 'B'; 
                          break;
                 case  3: graphic_type = 'X';
                          break;
                 case  4: graphic_type = 'S';
                          break;
                 case  5: graphic_type = 'P';
                          break;
                 default: sprintf(msg,"Invalid data arrived here!");
                          (void) message(msg);
                          break;
                 }
#endif /* NEVER_DEFINED */
/* now the replacement for the ifdef'ed code: */
   switch(choice){
		 case 1: graphic_type = 'L'; 	break;
		 case 2: graphic_type = 'B'; 	break;
		 case 3: graphic_type = 'X';	break;
		 case 4: graphic_type = 'S';	break;
                 case 5: graphic_type = 'P';    break;
		 case 0:          		break;
		 default: sprintf(msg,"Sorry, %s is not implemented yet",
				  submenu[choice-1]);
			  (void) message(msg);
			  choice = 0;
			  break;
	         }
    if ( NULL < choice && choice <= GRAPHTYPES )
       {
       sprintf(msg,"You have selected %s graph ",submenu[choice-1]);
       (void) message(msg);
       }
}

graphic_view()
{
   unsigned int getout, i, j;
   double       x;
   struct ent *p;

   getout = 0;
   if (*(graphic_range[0].col) == '\0') { 
      fprintf(stderr,"\007");
      message("X range not defined yet.");
      getout = 1;  
   }

   curves_n = 0;
   for (i = 1; i < GRAPHRANGES; i++) {
      int cells = 0; /* valid numeric cells in this range */

      if (*(graphic_range[i].col) == '\0') continue;
      for (j=graphic_range[i].r1; j<=graphic_range[i].r2; j++){
	if ((lookat(j, graphic_range[i].c))->flags & is_valid)
	  cells++;
        else {
	  sprintf(stringbuf,"Invalid numeric cell in range %c\n",'A'+i);
	  fprintf(stderr,"\007");
	  message(stringbuf);
	}
      }
      if (cells < 1){
	sprintf(stringbuf,"Range %c ignored.\007\n",'A'+i);
	fprintf(stderr,"\007");
	message(stringbuf);
	graphic_range[i].col[0] = '\0';
      } else 
        curves_n ++;
   }  
   if (curves_n < 1) {
      getout = 1;
      message("Not enough valid ranges defined.");
   }
 
   if (graphic_scale[0][0] > graphic_scale[0][1]) {
      x = graphic_scale[0][0]; 
      graphic_scale[0][0] = graphic_scale[0][1]; 
      graphic_scale[0][1] = x; 
   }

   if (graphic_scale[1][0] > graphic_scale[1][1]) {
      x = graphic_scale[1][0]; 
      graphic_scale[1][0] = graphic_scale[1][1]; 
      graphic_scale[1][1] = x; 
   }
   
   if (!getout) {
      switch(graphic_type) {
      
      case 'L':
         plot_line();
         break; 
      case 'B':
         plot_bar();
         break; 
      case 'X':
         plot_XY();
         break;
      case 'S':
         plot_stacked_bar();
         break;
      case 'P':
         plot_pie();
         break;
      }
   }
   FullUpdate++; /* in case window becomes obscured */
   update();
}
