/* Directory and path functions
   Copyright (C) 1992 Joseph H. Allen

This file is part of JOE (Joe's Own Editor)

JOE is free software; you can redistribute it and/or modify it under the 
terms of the GNU General Public License as published by the Free Software 
Foundation; either version 1, or (at your option) any later version.  

JOE is distributed in the hope that it will be useful, but WITHOUT ANY 
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more 
details.  

You should have received a copy of the GNU General Public License along with 
JOE; see the file COPYING.  If not, write to the Free Software Foundation, 
675 Mass Ave, Cambridge, MA 02139, USA.  */ 

#ifndef _Ipathfunc
#define _Ipathfunc

#include "config.h"

/* These comments aren't quite right: no drive and no conversion to lower
 * case for UNIX systems...
 */

/* char *abspth(char *path);
 * Convert given path into an absolute path (a path beginning with a drive
 * letter and ":\" and with no "."s or ".."s).
 *
 * This can be used to check if two paths refer to the same file:  Convert the
 * two paths into absolute paths and then compare the absolute paths.
 *
 * Also, this can be used to get the current drive and directory:
 * Use abspath("")
 *
 * Or, to get the current directory on another drive:  Use abspath("a:")
 *
 * Returns an malloc block containing the absolute path string or
 * 0 if the given path was in error or if couldn't read the current
 * directory (because the floppy was removed from the drive).
 */
char *abspth();

/* char *namprt(char *path);
 * Return name part of a path.  There may not be one if the last character
 * in the path is '/'.
 */
char *namprt();

/* char *dirprt(char *path);
 * Return directory and drive part of a path.  I.E., everything to the
 * left of the name part.
 */
char *dirprt();

char *begprt();
char *endprt();

/* int mkpath(char *path);
 * Make sure path exists.  If it doesn't, try to create it
 *
 * Returns 1 for error or 0 for success.  The current directory
 * and drive will be at the given path if successful, otherwise
 * the drive and path will be elsewhere (not necessarily where they
 * were before mkpath was called).
 */
int mkpath();

/* char *mktmp(char *);
 * Create an empty temporary file.  The file name created is the string passed
 * to this function postfixed with JXXXXXX.tmp, where XXXXXX is some number.
 */
char *mktmp();

/* Change drive:directory
 */
#define chddir chdir

#endif
