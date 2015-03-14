#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)xv_version.c 1.10 91/09/14";
#endif
#endif

/*
 *      (c) Copyright 1989 Sun Microsystems, Inc. Sun design patents 
 *      pending in the U.S. and foreign countries. See LEGAL NOTICE 
 *      file for terms of the license.
 */

 /*
  *  XView version number:
  *
  *  thousands digit signifies major release number
  *  hundreds digit signifies minor release number
  *  tens digit signifies patch release number
  *  ones digit signifies release number for specials
  *
  */

unsigned short xview_version;
char *xv_version;
