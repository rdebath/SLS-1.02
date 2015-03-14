#ifndef CLOSESTR_H
#define CLOSESTR_H

#define	NEED_REPLIES
#include "Xproto.h"
#include "closure.h"
#include "dix.h"
#include "misc.h"

/* closure structures */
typedef struct _OFclosure {
    ClientPtr   client;
    short       current_fpe;
    short       num_fpes;
    FontPathElementPtr *fpe_list;
    Mask        flags;
    Bool        slept;

/* XXX -- get these from request buffer instead? */
    char       *origFontName;
    int		origFontNameLen;
    XID         fontid;
    char       *fontname;
    int         fnamelen;
}           OFclosureRec;

typedef struct _LFclosure {
    ClientPtr		client;
    short		current_fpe;
    short		num_fpes;
    FontPathElementPtr	*fpe_list;
    FontNamesPtr	names;
    char		*pattern;
    int			max_names;
    int			patlen;
    Bool		slept;
}           LFclosureRec;

typedef struct _LFWIstate {
    char	*pattern;
    int		patlen;
    int		current_fpe;
    int		max_names;
    Bool	list_started;
    pointer	private;
} LFWIstateRec, *LFWIstatePtr;

typedef struct _LFWIclosure {
    ClientPtr		client;
    int			num_fpes;
    FontPathElementPtr	*fpe_list;
    xListFontsWithInfoReply *reply;
    int			length;
    LFWIstateRec	current;
    LFWIstateRec	saved;
    int			savedNumFonts;
    Bool		haveSaved;
    Bool		slept;
    char		*savedName;
} LFWIclosureRec;
#endif				/* CLOSESTR_H */
