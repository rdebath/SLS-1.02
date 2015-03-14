/*
 *	(c) Copyright 1990 Sun Microsystems, Inc. Sun design patents 
 *	pending in the U.S. and foreign countries. See LEGAL NOTICE 
 *	file for terms of the license.
 */

#ident	"@(#)i18n.h	1.6	91/09/14 SMI"

#ifndef i18n_DEFINED
#define i18n_DEFINED

#ifdef OW_I18N_L3

extern	char	*gettext();

#else

#define 	gettext(s)	s

#endif

#endif i18n_DEFINED


