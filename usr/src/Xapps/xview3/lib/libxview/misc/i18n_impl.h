/*      @(#)i18n_impl.h 1.11 91/09/14 SMI	*/
/*
 *      (c) Copyright 1989 Sun Microsystems, Inc. Sun design patents 
 *      pending in the U.S. and foreign countries. See LEGAL NOTICE 
 *      file for terms of the license.
 */

#ifndef i18n_impl_h_DEFINED
#define i18n_impl_h_DEFINED

#ifndef OS_HAS_LOCALE
#define OS_HAS_LOCALE
#endif

#if defined(SVR4) && defined(OW_I18N)
#include <stdlib.h>
#endif /* SVR4 && OW_I18N */

#ifdef OS_HAS_LOCALE

#include  <locale.h>

extern char	*dgettext();

#define XV_I18N_MSG(d,s)	(dgettext(d,s))

#ifdef XGETTEXT
#define xv_domain		"xv_messages"
#else  /* XGETTEXT */
extern char			*xv_domain;
#endif /* XGETTEXT */

#define XV_MSG(s)		(dgettext(xv_domain, s))

#else  /* OS_HAS_LOCALE */

#define XV_I18N_MSG(d,s)	((s))

#define XV_MSG(s)		((s))

#endif /* OS_HAS_LOCALE */

#ifdef OW_I18N
	/*
	 * This is only for the level four i18n (ie, supporting the
	 * Asian locales).
	 */

#	include	<xview/xv_i18n.h>

	/*
	 * Those macros has been defined to reduce the many of "#ifdef
	 * OW_I18N" inside the source code.  Using those macros should
	 * improve the readbility of the source code.
	 */
#	define	ATOI		watoi
#	define	CHAR		wchar_t
#	define	INDEX		STRCHR
#	define	RINDEX		STRRCHR
#	define	SPRINTF		wsprintf
#	define	STRCAT		wscat
#	define	STRCHR		wschr
#	define	STRCMP		wscmp
#	define	STRCPY		wscpy
#ifdef notdef
	/* Conflict with sun.h's define */
#	define	STRDUP		wsdup
#endif
#	define	STRLEN		wslen
#	define	STRNCAT		wsncat
#	define	STRNCMP		wsncmp
#	define	STRNCPY		wsncpy
#	define	STRRCHR		wsrchr

	/*
	 * Character convert and dup functions.
	 */
	extern wchar_t	*mbstowcsdup();
	extern char	*wcstombsdup();
	extern wchar_t	*ctstowcsdup();
	extern char	*wcstoctsdup();

#else OW_I18N

#	define	ATOI		atoi
#	define	CHAR		char
#	define	INDEX		STRCHR
#	define	RINDEX		STRRCHR
#	define	SPRINTF		sprintf
#	define	STRCAT		strcat
#	define	STRCHR		strchr
#	define	STRCMP		strcmp
#	define	STRCPY		strcpy
#ifdef notdef
	/* Conflict with sun.h's define */
#	define	STRDUP		strdup
#endif
#	define	STRLEN		strlen
#	define	STRNCAT		strncat
#	define	STRNCMP		strncmp
#	define	STRNCPY		strncpy
#	define	STRRCHR		strrchr

#endif OW_I18N

#define XV_STRSAVE(s)	 \
	STRCPY((CHAR *)xv_malloc((STRLEN(s)+1) * sizeof(CHAR)), (s))

#endif i18n_impl_h_DEFINED
