/*	@(#)cms.h 20.15 91/09/14 SMI	*/

/*
 *	(c) Copyright 1989 Sun Microsystems, Inc. Sun design patents 
 *	pending in the U.S. and foreign countries. See LEGAL NOTICE 
 *	file for terms of the license.
 */

#ifndef xview_cms_DEFINED
#define	xview_cms_DEFINED

/*
 ***********************************************************************
 *                      Definitions and Macros
 ***********************************************************************
 */

/*
 * PUBLIC #defines
 */
#define CMS_NAMESIZE            25

/*
 ***********************************************************************
 *		Typedefs, enumerations, and structs
 ***********************************************************************
 */
enum Xv_cmstype	{XV_STATIC_CMS = 1, XV_DYNAMIC_CMS = 2};

typedef struct xv_cmsdata {
    enum Xv_cmstype	 type;
    short                size;
    short                index;
    short                rgb_count;
    unsigned char       *red;
    unsigned char       *green;
    unsigned char       *blue;
} Xv_cmsdata;

typedef struct xv_singlecolor {
    u_char 	red, green, blue;
} Xv_singlecolor;

#endif	~xview_cms_DEFINED
