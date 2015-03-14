/*	@(#)txt_18impl.h 1.3 91/09/14 SMI	*/
 
/*
 *	(c) Copyright 1989 Sun Microsystems, Inc. Sun design patents 
 *	pending in the U.S. and foreign countries. See LEGAL NOTICE 
 *	file for terms of the license.
 */

#ifndef txt_18impl_h_DEFINED
#define txt_18impl_h_DEFINED

#ifdef OW_I18N

#define		SIZEOF(_obj)	(sizeof(_obj) / sizeof(CHAR))
#define		MALLOC(_size)	((CHAR *)malloc((_size) * sizeof(CHAR)))
#define		BCOPY(_from, _to, _len) \
			(XV_BCOPY((_from), (_to), ((_len) * sizeof(CHAR))))

#else OW_I18N

#define		SIZEOF		sizeof
#define		MALLOC		malloc
#define		BCOPY		XV_BCOPY

#endif OW_I18N

#endif txt_18impl_h_DEFINED
