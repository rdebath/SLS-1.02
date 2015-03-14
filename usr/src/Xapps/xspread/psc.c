
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
 * This is a modification of a program written or modified by
 * others.  The original copyrights, as per GNU General Public License,
 * may still be applicable.  The UWM copyright is applicable only
 * the those parts generated at UWM.
 *
 * Please send all changes, enhancements, and other comments about this
 * software to
 *     		soft-eng@cs.uwm.edu
 *
 * No Warranty, expressed or implied, comes with this software.
 * This software is intended to be used by not-for-profit
 * organizations. Selling this software for profit without
 * the permission of the Board of Regents of the University
 * of Wisconsin is strictly forbidden. 
 *
 * Contact:	soft-eng@cs.uwm.edu
 *			or
 *		
 *		Software Engineering Coordinator
 *		Computer Science
 *    		Department of EECS
 *		University of Wisconsin - Milwaukee
 *		Milwaukee, WI  53201
 *		414-229-4677
 *
 *		HISTORY,CLAIMS and CONTRIBUTIONS
 */

/*    I'm not sure what this file is... I think it might be an old
	version of sc but it looks so much different.. so
	I'm leaving it here for someone else to figure out
	what the heck this file is....

*/
/* Sc parse routine
 *
 * usage psc options
 * options:
 *   -L		Left justify strings.  Default is right justify.
 *   -r		Assemble data into rows first, not columns.
 *   -R	n	Increment by n between rows 
 *   -C n	Increment by n between columns
 *   -n n	Length of the row (column) should be n.
 *   -s v	Top left location in the spreadsheet should be v; eg, k5
 *   -d c       Use c as the delimiter between the fields.
 *   -k         Keep all delimiters - Default is strip multiple delimiters to 1.
 *   
 *  Author: Robert Bond
 *		$Revision: 6.1 $
 */

#include <ctype.h>
#include <stdio.h>
#include "sc.h"

#define END 0
#define NUM 1
#define ALPHA 2
#define SPACE 3
#define EOL 4

extern char *optarg;
extern int   optind;
char *coltoa();
char *progname;

#ifdef SYSV3
extern void exit();
#else
extern int exit();
#endif

int colfirst = 0;
int r0 = 0;
int c0 = 0;
int rinc = 1;
int cinc = 1;
int leftadj = 0;
int len = 20000;
char delim1 = ' ';
char delim2 = '\t';
int strip_delim = 1;
int fwidth[MAXCOLS];
int precision[MAXCOLS];

char token[1000];

main(argc, argv)
int argc;
char **argv;
{
    int curlen;
    int curcol, coff;
    int currow, roff;
    int first;
    int c;
    register effr, effc;
    int i,j;
    register char *p;

    progname = argv[0];
    while ((c = getopt(argc, argv, "rLks:R:C:n:d:")) != EOF) {
	switch(c) {
	case 'r':
	    colfirst = 1;
	    break;
	case 'L':
	    leftadj = 1;
	    break;
	case 's':
	    c0 = getcol(optarg);
	    r0 = getrow(optarg);
	    break;
	case 'R':
	    rinc = atoi(optarg);
	    break;
	case 'C':
	    cinc = atoi(optarg);
	    break;
	case 'n':
	    len = atoi(optarg);
	    break;
	case 'd':
	    delim1 = optarg[0];
	    delim2 = 0;
	    break;
	case 'k':
	    strip_delim = 0;
	    break;
	default:
	    (void) fprintf(stderr,"Usage: %s [-rkL] [-s v] [-R i] [-C i] [-n i] [-d c]\n", progname);
	    exit(1);
        }
    }

    if (optind < argc) {
	    (void) fprintf(stderr,"Usage: %s [-rL] [-s v] [-R i] [-C i] [-n i] [-d c]\n", progname);
	    exit(1);
    }

    curlen = 0;
    curcol = c0; coff = 0;
    currow = r0; roff = 0;
    first = 1;

    while(1) {

	effr = currow+roff;
	effc = curcol+coff;

	switch(scan()) {
	case END:
	    for (i = 0; i<MAXCOLS; i++) {
		if (precision[i])
		    (void) printf("format %s %d %d\n", coltoa(i), precision[i]+1,
			fwidth[i]);
	    }
	    exit(0);
	case NUM:
	    first = 0;
	    (void) printf("let %s%d = %s\n", coltoa(effc), effr, token);
	    if (effc > MAXCOLS-1)
		(void) fprintf(stderr, "Invalid column used: %s\n", coltoa(effc));
	    else {
		i = 0;
		j = 0;
		p = token;
		while (*p && *p != '.') {
		    p++; i++;
		}
		if (*p) {
		    p++; i++;
		}
		while (*p) {
		    p++; i++; j++;
		}
		if (precision[effc] < i)
		    precision[effc] = i;
		if (fwidth[effc] < j)
		    fwidth[effc] = j;
	    }
	    break;
	case ALPHA:
	    first = 0;
	    if (leftadj)
		(void) printf("leftstring %s%d = \"%s\"\n", coltoa(effc),effr,token); 
	    else
		(void) printf("rightstring %s%d = \"%s\"\n",coltoa(effc),effr,token); 
	    if (effc > MAXCOLS-1)
		(void) fprintf(stderr, "Invalid column used: %s\n", coltoa(effc));
	    else {
		i = strlen(token);
		if (i > precision[effc])
		    precision[effc] = i;
	    }
	    break;
	case SPACE:
	    if (first && strip_delim)
		break;
	    if (colfirst)
		roff++;
	    else
		coff++;
	    break;
	case EOL:
	    curlen++;
	    roff = 0;
	    coff = 0;
	    first = 1;
	    if (colfirst) {
		if (curlen >= len) {
		    curcol = c0;
		    currow += rinc;
		    curlen = 0;
		} else {
		    curcol += cinc;
		}
	    } else {
		if (curlen >= len) {
		    currow = r0;
		    curcol += cinc;
		    curlen = 0;
		} else {
		    currow += rinc;
		}
	    }
	    break;
	}
    }
}

scan()
{
    register int c;
    register char *p;

    p = token;
    c = getchar();

    if (c == EOF)
	return(END);

    if (c == '\n')
	return(EOL);

    if (c == delim1 || c == delim2) {
        if (strip_delim) {
	    while ((c = getchar()) && (c == delim1 || c == delim2))
	        ;
	    (void)ungetc(c, stdin);
	} 
	return(SPACE);
    }

    if (c == '\"') {
	while ((c = getchar()) && c != '\"' && c != '\n' && c != EOF)
	    *p++ = c;
	if (c != '\"')
	    (void)ungetc(c, stdin);
	*p = 0;
	return(ALPHA);
    }

    while (c != delim1 && c != delim2 && c!= '\n' && c != EOF) {
	*p++ = c;
	c = getchar();
    }
    *p = 0;
    (void)ungetc(c, stdin);

    p = token;
    c = *p;
    if (isdigit(c) || c == '.' || c == '-' || c == '+') {
	while(isdigit(c) || c == '.' || c == '-' || c == '+' || c == 'e'
	    || c == 'E') {
		c = *p++;
	}
	if (c == 0)
	    return(NUM);
	else
	    return(ALPHA);
    }

    return(ALPHA);
}
    
getcol(p)
char *p;
{
    register  col;

    if (!p)
	return(0);
    while(*p && !isalpha(*p)) 
	p++; 
    if (!*p)
	return(0);
    col = ((*p & 0137) - 'A');
    if (isalpha(*++p)) 
	col = (col + 1)*26 + ((*p & 0137) - 'A');
    return(col);
}

getrow(p)
char *p;
{
    int row;

    if (!p)
	return(0);
    while(*p && !isdigit(*p))
	p++; 
    if (!*p)
	return(0);
    if (sscanf(p, "%d", &row) != 1)
	return(0);
    return(row);
}

