/*
  This file is part of the NetFax system.

  (c) Copyright 1989 by David M. Siegel. 
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
#include <unistd.h>
#include <sys/param.h>

#include "../../lib/libfax/libfax.h"

/*
 * Count the number of pages that the postscript converter generated.
 */
static int count_pages(base)
     char *base;
{
    int pages = 1;

    for (;;) {
	char filename[MAXPATHLEN];
	sprintf(filename, "%s.g3.%d", base, pages);
	if (access(filename, R_OK) != 0)
	  return (pages-1);
	pages++;
    }
}


/*
 * Convert a postscript file to a set of G3 format files, using the digifax
 * driver in Ghostscript.
 */
int cvt_postscript_to_g3(in_file, out_base)
     char *in_file;
     char *out_base;
{

    char program[128];
    FILE *fp;

    /*
     * Open ghostscript converter.
     */
    sprintf(program, "%s -sOUTPUTFILE=%s.g3.%%d %s", PS_PROG, out_base, in_file);
    if ((fp = popen(program, "w")) == NULL)
      return (-1);
    
    /*
     * We`re all done! That was simple.
     */
    fprintf(fp, "quit\n");

    /*
     * All done, wait for the conversion to complete.
     */
    pclose(fp);

    /*
     * Now count the number of pages that ghostscript wrote out.
     */
    return (count_pages(out_base));

}




int cvt_coversheet_to_g3(out_base, recipient, sender, sender_fax, pages)
     char *out_base;
     char *recipient;
     char *sender;
     char *sender_fax;
     int pages;
{
    char ps_file[MAXPATHLEN];
    FILE *fp;

    sprintf(ps_file, "%s.ps", out_base);
    if ((fp = fopen(ps_file, "w+")) == NULL)
      return (-1);
    
    fprintf(fp, "/recipient (%s) def\n", recipient);
    fprintf(fp, "/sender (%s) def\n", sender);
    fprintf(fp, "/returnfaxnum (%s) def\n", sender_fax);
    fprintf(fp, "/pages (%d) def\n", pages+1);
    fprintf(fp, "(%s) run\n", PS_TO_COVER_PROG);

    fclose(fp);

    if (cvt_postscript_to_g3(ps_file, out_base) < 0) {
	unlink(ps_file);
	return (-1);
    }

    unlink(ps_file);

    return (0);
}
