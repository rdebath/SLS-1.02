/*
  This file is part of the NetFax system.

  (c) Copyright 1989 by David M. Siegel and Sundar Narasimhan.
      All rights reserved.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation.

    This program is distributed in the hope that it will be useful, 
    but WITHOUT ANY WARRANTY; without even the implied warranty of 
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include <stdio.h>

#include "show.h"

#define MORE "/usr/ucb/more"

int
show_file(filename, fp)
char *filename;
FILE *fp;
{
    FILE *f;
    char buf[BUFSIZ];

    if ((f = fopen(filename, "r")) == NULL)
      return (-1);

    while (!feof(f)) {
	int count = fread(buf, sizeof(char), sizeof(buf), f);
	if (count > 0)
	  fwrite(buf, sizeof(char), count, fp);
    }
    
    fclose(f);

    return (0);
}

int
show_file_with_more(filename)
char *filename;
{
    FILE *p;
    FILE *f;
    char buf[BUFSIZ];

    if ((p = popen(MORE, "w")) == NULL)
      return (-1);

    if ((f = fopen(filename, "r")) == NULL) {
	pclose(p);
	return (-1);
    }

    while (!feof(f)) {
	int count = fread(buf, sizeof(char), sizeof(buf), f);
	if (count > 0)
	  fwrite(buf, sizeof(char), count, p);
    }
    
    pclose(p);
    fclose(f);

    return (0);
}
