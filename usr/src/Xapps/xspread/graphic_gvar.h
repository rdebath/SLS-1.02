
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

/* ****************************************************************** */
/* Programer: <Tuan Tang>     Date: July 24, 1990                     */
/* ------------------------------------------------------------------ */
/* This header file declares all the variables and arrays used by     */
/* spread sheet to store the parameters for graphics program.         */
/* ------------------------------------------------------------------ */

#define GRAPHITEMS   11  /* items in the main menu */
#define GRAPHOPTIONS  5  /* number of items in graph options */
#define GRAPHTYPES    5  /* five different types of graphs */
#define GRAPHRANGES   7  /* different types of data range */
#define GRAPHRESETS   8  /* reset options = 8 */
#define GRAPHLEGENDS  6  /* one legend for each data type */
#define GRAPHFORMATS  7  /* ways to set the format of XY & line graphs */
#define GRAPHSYMBOLS  4  /* different output format of XY & line graphs */ 
#define GRAPHTITLES   4  /* labels for xy axes and titles */
#define GRAPHSCALES   3  /* one for x and one for y scale */
#define GRAPHXYSCALES 4  /* no. of items of XY scale */

char  graphic_type;      /* store the type of the graph */
char  graphic_legend[GRAPHLEGENDS][100];  /* symbol use for each graph */
char  graphic_format[GRAPHFORMATS];      /* output format of each graph */
char  graphic_title[GRAPHTITLES][50];   /* labels for the graph */

char  graphic_grid;  /* condition for drawing line parallel to axes */
char  g_auto_man[2]; /* for auto or manual scaling for x and y axes */
unsigned int graphic_skip;  /* skip factor */               
unsigned int curves_n;                    

double graphic_scale[GRAPHSCALES-1][2];   /* 1st element for min. & 2nd for max. */

struct g_range {
       char col[3];     /* name of column */
       int r1, r2, c;   /* start row, end row, column number */
       };
struct g_range graphic_range[GRAPHRANGES];

