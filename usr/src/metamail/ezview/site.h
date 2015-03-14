/*
Copyright (c) 1991 Bell Communications Research, Inc. (Bellcore)

Permission to use, copy, modify, and distribute this material 
for any purpose and without fee is hereby granted, provided 
that the above copyright notice and this permission notice 
appear in all copies, and that the name of Bellcore not be 
used in advertising or publicity pertaining to this 
material without the specific, prior written permission 
of an authorized representative of Bellcore.  BELLCORE 
MAKES NO REPRESENTATIONS ABOUT THE ACCURACY OR SUITABILITY 
OF THIS MATERIAL FOR ANY PURPOSE.  IT IS PROVIDED "AS IS", 
WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.
*/
/* Empty site.h file.  Use this file to hold all */
/* your site's changes to allsys.h and system.h. */
/* THIS FILE CONTAINS NO CONFIGURATION DEPENDENCIES */

/* don't build ams delivery stuff, the oda stuff, or whitepages */
#ifdef AMS_DELIVERY_ENV
#undef AMS_DELIVERY_ENV
#endif

#ifdef ODA_ENV
#undef ODA_ENV
#endif

#ifdef WHITEPAGES_ENV
#undef WHITEPAGES_ENV
#endif

/* do build the stuff in contrib */
#define CONTRIB_ENV 1

/* Do build console, ezprint, champ & preview */
#define MK_BASIC_UTILS 1

/* Do build Adew & Ness */
#define MK_AUTHORING 1

/* Do build datacat & toez */
#define MK_AUX_UTILS 1

/* don't build gob or the examples */
#ifdef MK_AUX_INSETS
#undef MK_AUX_INSETS
#endif

#ifdef MK_EXAMPLES
#undef MK_EXAMPLES
#endif

/* turn off the resolver */
#ifdef RESOLVER_ENV
#undef RESOLVER_ENV
#endif

/* Lets give this a try */
#define SNAP_ENV 1

/* place where users will access andrew */
#undef DEFAULT_ANDREWDIR_ENV
#undef DEFAULT_LOCALDIR_ENV
#define DEFAULT_ANDREWDIR_ENV /usr/local/pkg/X11/andrew

/* site-specific stuff will be found here */
#define DEFAULT_LOCALDIR_ENV /usr/local/pkg/X11/andrew/local
