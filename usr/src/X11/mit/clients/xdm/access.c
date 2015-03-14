/*
 * $XConsortium: access.c,v 1.13 92/04/21 11:33:37 gildea Exp $
 *
 * Copyright 1990 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Keith Packard, MIT X Consortium
 */

/*
 * Access control for XDMCP - keep a database of allowable display addresses
 * and (potentially) a list of hosts to send ForwardQuery packets to
 */

# include   "dm.h"

#ifdef XDMCP

# include   <X11/Xos.h>
# include   <X11/Xdmcp.h>
# include   <X11/X.h>
# include   <stdio.h>
# include   <ctype.h>
# include   <netinet/in.h>
# include   <netdb.h>
# include   <sys/socket.h>

#define ALIAS_CHARACTER	    '%'
#define NEGATE_CHARACTER    '!'
#define CHOOSER_STRING	    "CHOOSER"
#define BROADCAST_STRING    "BROADCAST"

#define HOST_ALIAS	0
#define HOST_ADDRESS	1
#define HOST_BROADCAST	2
#define HOST_CHOOSER	3

typedef struct _hostEntry {
    struct _hostEntry	*next;
    int	    type;
    union _hostOrAlias {
	char	*aliasName;
	ARRAY8	hostAddress;
    } entry;
} HostEntry;

#define DISPLAY_ALIAS	    0
#define DISPLAY_PATTERN	    1
#define DISPLAY_ADDRESS	    2

typedef struct _displayEntry {
    struct _displayEntry    *next;
    int			    type;
    int			    notAllowed;
    int			    chooser;
    union _displayType {
	char		    *aliasName;
	char		    *displayPattern;
	struct _display {
	    ARRAY8	    clientAddress;
	    CARD16	    connectionType;
	} displayAddress;
    } entry;
    HostEntry		    *hosts;
} DisplayEntry;

static DisplayEntry	*database;

static ARRAY8		localAddress;

ARRAY8Ptr
getLocalAddress ()
{
    static int	haveLocalAddress;
    
    if (!haveLocalAddress)
    {
	struct hostent	*hostent;

	hostent = gethostbyname (localHostname());
	XdmcpAllocARRAY8 (&localAddress, hostent->h_length);
	bcopy (hostent->h_addr, localAddress.data, hostent->h_length);
    }
    return &localAddress;
}

static void
FreeHostEntry (h)
    HostEntry	    *h;
{
    switch (h->type) {
    case HOST_ALIAS:
	free (h->entry.aliasName);
	break;
    case HOST_ADDRESS:
	XdmcpDisposeARRAY8 (&h->entry.hostAddress);
	break;
    case HOST_CHOOSER:
	break;
    }
    free ((char *) h);
}

static void
FreeDisplayEntry (d)
    DisplayEntry    *d;
{
    HostEntry	*h, *next;
    switch (d->type) {
    case DISPLAY_ALIAS:
	free (d->entry.aliasName);
	break;
    case DISPLAY_PATTERN:
	free (d->entry.displayPattern);
	break;
    case DISPLAY_ADDRESS:
	XdmcpDisposeARRAY8 (&d->entry.displayAddress);
	break;
    }
    for (h = d->hosts; h; h = next) {
	next = h->next;
	FreeHostEntry (h);
    }
    free ((char *) d);
}

static void
FreeAccessDatabase ()
{
    DisplayEntry    *d, *next;

    for (d = database; d; d = next)
    {
	next = d->next;
	FreeDisplayEntry (d);
    }
    database = 0;
}

#define WORD_LEN    256
static char	wordBuffer[WORD_LEN];
static int	nextIsEOF;

static char *
ReadWord (file, EOFatEOL)
    FILE    *file;
    int	    EOFatEOL;
{
    int	    c;
    char    *wordp;
    int	    quoted;

    wordp = wordBuffer;
    if (nextIsEOF)
    {
	nextIsEOF = FALSE;
	return NULL;
    }
    quoted = FALSE;
    for (;;) {
	c = getc (file);
	switch (c) {
	case '#':
	    if (quoted)
	    {
		*wordp++ = c;
		break;
	    }
	    while ((c = getc (file)) != EOF && c != '\n')
		;
	case '\n':
	case EOF:
	    if (c == EOF || (EOFatEOL && !quoted))
	    {
		ungetc (c, file);
		if (wordp == wordBuffer)
		    return NULL;
		*wordp = '\0';
		nextIsEOF = TRUE;
		return wordBuffer;
	    }
	case ' ':
	case '\t':
	    if (wordp != wordBuffer)
	    {
		ungetc (c, file);
		*wordp = '\0';
		return wordBuffer;
	    }
	    break;
	case '\\':
	    if (!quoted)
	    {
		quoted = TRUE;
		continue;
	    }
	default:
	    *wordp++ = c;
	    break;
	}
	quoted = FALSE;
    }
}

static HostEntry *
ReadHostEntry (file)
    FILE    *file;
{
    char	    *hostOrAlias;
    HostEntry	    *h;
    struct hostent  *hostent;

tryagain:
    hostOrAlias = ReadWord (file, TRUE);
    if (!hostOrAlias)
	return NULL;
    h = (HostEntry *) malloc (sizeof (DisplayEntry));
    if (*hostOrAlias == ALIAS_CHARACTER)
    {
	h->type = HOST_ALIAS;
	h->entry.aliasName = malloc (strlen (hostOrAlias) + 1);
	if (!h->entry.aliasName) {
	    free ((char *) h);
	    return NULL;
	}
	strcpy (h->entry.aliasName, hostOrAlias);
    }
    else if (!strcmp (hostOrAlias, CHOOSER_STRING))
    {
	h->type = HOST_CHOOSER;
    }
    else if (!strcmp (hostOrAlias, BROADCAST_STRING))
    {
	h->type = HOST_BROADCAST;
    }
    else
    {
	h->type = HOST_ADDRESS;
	hostent = gethostbyname (hostOrAlias);
	if (!hostent)
	{
	    Debug ("No such host %s\n", hostOrAlias);
	    LogError ("Access file \"%s\", host \"%s\" not found\n", accessFile, hostOrAlias);
	    free ((char *) h);
	    goto tryagain;
	}
	if (!XdmcpAllocARRAY8 (&h->entry.hostAddress, hostent->h_length))
	{
	    LogOutOfMem ("ReadHostEntry\n");
	    free ((char *) h);
	    return NULL;
	}
	bcopy (hostent->h_addr, h->entry.hostAddress.data, hostent->h_length);
    }
    return h;
}

static int
HasGlobCharacters (s)
    char    *s;
{
    for (;;)
	switch (*s++) {
	case '?':
	case '*':
	    return 1;
	case '\0':
	    return 0;
	}
}

static DisplayEntry *
ReadDisplayEntry (file)
    FILE    *file;
{
    char	    *displayOrAlias;
    DisplayEntry    *d;
    struct _display *display;
    HostEntry	    *h, **prev;
    struct hostent  *hostent;
    
    displayOrAlias = ReadWord (file, FALSE);
    if (!displayOrAlias)
    	return NULL;
    d = (DisplayEntry *) malloc (sizeof (DisplayEntry));
    d->notAllowed = 0;
    d->chooser = 0;
    if (*displayOrAlias == ALIAS_CHARACTER)
    {
	d->type = DISPLAY_ALIAS;
	d->entry.aliasName = malloc (strlen (displayOrAlias) + 1);
	if (!d->entry.aliasName)
	{
	    free ((char *) d);
	    return NULL;
	}
	strcpy (d->entry.aliasName, displayOrAlias);
    }
    else
    {
	if (*displayOrAlias == NEGATE_CHARACTER)
	{
	    d->notAllowed = 1;
	    ++displayOrAlias;
	}
    	if (HasGlobCharacters (displayOrAlias))
    	{
	    d->type = DISPLAY_PATTERN;
	    d->entry.displayPattern = malloc (strlen (displayOrAlias) + 1);
	    if (!d->entry.displayPattern)
	    {
	    	free ((char *) d);
	    	return NULL;
	    }
	    strcpy (d->entry.displayPattern, displayOrAlias);
    	}
    	else
    	{
	    if ((hostent = gethostbyname (displayOrAlias)) == NULL)
	    {
		LogError ("Access file %s, display %s unknown\n", accessFile, displayOrAlias);
		free ((char *) d);
		return NULL;
	    }
	    d->type = DISPLAY_ADDRESS;
	    display = &d->entry.displayAddress;
	    if (!XdmcpAllocARRAY8 (&display->clientAddress, hostent->h_length))
	    {
	    	free ((char *) d);
	    	return NULL;
	    }
	    bcopy (hostent->h_addr, display->clientAddress.data, hostent->h_length);
	    switch (hostent->h_addrtype)
	    {
#ifdef AF_UNIX
	    case AF_UNIX:
	    	display->connectionType = FamilyLocal;
	    	break;
#endif
#ifdef AF_INET
	    case AF_INET:
	    	display->connectionType = FamilyInternet;
	    	break;
#endif
#ifdef AF_DECnet
	    case AF_DECnet:
	    	display->connectionType = FamilyDECnet;
	    	break;
#endif
	    default:
	    	display->connectionType = FamilyLocal;
	    	break;
	    }
    	}
    }
    prev = &d->hosts;
    while (h = ReadHostEntry (file))
    {
	if (h->type == HOST_CHOOSER)
	{
	    FreeHostEntry (h);
	    d->chooser = 1;
	} else {
	    *prev = h;
	    prev = &h->next;
	}
    }
    *prev = NULL;
    return d;
}

static
ReadAccessDatabase (file)
    FILE    *file;
{
    DisplayEntry    *d, **prev;

    prev = &database;
    while (d = ReadDisplayEntry (file))
    {
	*prev = d;
	prev = &d->next;
    }
    *prev = NULL;
}

ScanAccessDatabase ()
{
    FILE	*datafile;

    FreeAccessDatabase ();
    if (*accessFile)
    {
    	datafile = fopen (accessFile, "r");
    	if (!datafile)
	{
	    LogError ("Cannot open access control file %s, no XDMCP reqeusts will be granted\n", accessFile);
	    return 0;
	}
	ReadAccessDatabase (datafile);
	fclose (datafile);
    }
    return 1;
}

/*
 * calls the given function for each valid indirect entry.  Returns TRUE if
 * the local host exists on any of the lists, else FALSE
 */

#define MAX_DEPTH   32

static int indirectAlias ();

static int
scanHostlist (h, clientAddress, connectionType, function, closure, depth, broadcast)
    HostEntry	*h;
    ARRAY8Ptr	clientAddress;
    CARD16	connectionType;
    int		(*function)();
    char	*closure;
    int		depth;
    int		broadcast;
{
    int	haveLocalhost = 0;

    for (; h; h = h->next)
    {
	switch (h->type) {
	case HOST_ALIAS:
	    if (indirectAlias (h->entry.aliasName, clientAddress,
			       connectionType, function, closure, depth,
			       broadcast))
		haveLocalhost = 1;
	    break;
	case HOST_ADDRESS:
	    if (XdmcpARRAY8Equal (getLocalAddress(), &h->entry.hostAddress))
		haveLocalhost = 1;
	    else if (function)
		(*function) (connectionType, &h->entry.hostAddress, closure);
	    break;
	case HOST_BROADCAST:
	    if (broadcast)
	    {
		ARRAY8	temp;

		if (function)
		{
		    temp.data = (BYTE *) BROADCAST_STRING;
		    temp.length = strlen ((char *)temp.data);
		    (*function) (connectionType, &temp, closure);
		}
	    }
	    break;
	}
    }
    return haveLocalhost;
}

/* Returns non-0 iff string is matched by pattern.  Does case folding.
 */
static int
patternMatch (string, pattern)
    char    *string, *pattern;
{
    int	    p, s;

    if (!string)
	string = "";

    for (;;)
    {
	s = *string++;
	switch (p = *pattern++) {
	case '*':
	    if (!*pattern)
		return 1;
	    for (string--; *string; string++)
		if (patternMatch (string, pattern))
		    return 1;
	    return 0;
	case '?':
	    if (s == '\0')
		return 0;
	    break;
	case '\0':
	    return s == '\0';
	case '\\':
	    p = *pattern++;
	    /* fall through */
	default:
	    if (isupper(p)) p = tolower(p);
	    if (isupper(s)) s = tolower(s);
	    if (p != s)
		return 0;
	}
    }
}

static int
indirectAlias (alias, clientAddress, connectionType, function, closure, depth,
	       broadcast)
    char	*alias;
    ARRAY8Ptr	clientAddress;
    CARD16	connectionType;
    int		(*function)();
    char	*closure;
    int		depth;
    int		broadcast;
{
    DisplayEntry    *d;
    int		    haveLocalhost = 0;

    if (depth == MAX_DEPTH)
	return 0;
    for (d = database; d; d = d->next)
    {
	if (d->type != DISPLAY_ALIAS || !patternMatch (alias, d->entry.aliasName))
	    continue;
	if (scanHostlist (d->hosts, clientAddress, connectionType,
			  function, closure, depth + 1, broadcast))
	{
	    haveLocalhost = 1;
	}
    }
    return haveLocalhost;
}

ARRAY8Ptr IndirectChoice ();

ForEachMatchingIndirectHost (clientAddress, connectionType, function, closure)
    ARRAY8Ptr	clientAddress;
    CARD16	connectionType;
    int		(*function)();
    char	*closure;
{
    int		    haveLocalhost = 0;
    DisplayEntry    *d;
    char	    *clientName = 0, *NetworkAddressToHostname ();

    for (d = database; d; d = d->next)
    {
    	switch (d->type) {
    	case DISPLAY_ALIAS:
	    continue;
    	case DISPLAY_PATTERN:
	    if (!clientName)
		clientName = NetworkAddressToHostname (connectionType,
						       clientAddress);
	    if (!patternMatch (clientName, d->entry.displayPattern))
		continue;
	    break;
    	case DISPLAY_ADDRESS:
	    if (d->entry.displayAddress.connectionType != connectionType ||
	    	!XdmcpARRAY8Equal (&d->entry.displayAddress.clientAddress,
				  clientAddress))
	    {
		continue;
	    }
	    break;
    	}
	if (!d->hosts)
	    continue;
	if (d->notAllowed)
	    break;
	if (d->chooser)
	{
	    ARRAY8Ptr	choice;

	    choice = IndirectChoice (clientAddress, connectionType);
	    if (!choice || XdmcpARRAY8Equal (getLocalAddress(), choice))
		haveLocalhost = 1;
	    else
		(*function) (connectionType, choice, closure);
	}
	else if (scanHostlist (d->hosts, clientAddress, connectionType,
			  function, closure, 0, FALSE))
	{
	    haveLocalhost = 1;
	}
	break;
    }
    if (clientName)
	free (clientName);
    return haveLocalhost;
}

UseChooser (clientAddress, connectionType)
    ARRAY8Ptr	clientAddress;
    CARD16	connectionType;
{
    DisplayEntry    *d;
    char	    *clientName = 0, *NetworkAddressToHostname ();

    for (d = database; d; d = d->next)
    {
    	switch (d->type) {
    	case DISPLAY_ALIAS:
	    continue;
    	case DISPLAY_PATTERN:
	    if (!clientName)
		clientName = NetworkAddressToHostname (connectionType,
						       clientAddress);
	    if (!patternMatch (clientName, d->entry.displayPattern))
		continue;
	    break;
    	case DISPLAY_ADDRESS:
	    if (d->entry.displayAddress.connectionType != connectionType ||
	    	!XdmcpARRAY8Equal (&d->entry.displayAddress.clientAddress,
				  clientAddress))
	    {
		continue;
	    }
	    break;
    	}
	if (!d->hosts)
	    continue;
	if (d->notAllowed)
	    break;
	if (d->chooser && !IndirectChoice (clientAddress, connectionType))
	    return 1;
	break;
    }
    return 0;
}

ForEachChooserHost (clientAddress, connectionType, function, closure)
    ARRAY8Ptr	clientAddress;
    CARD16	connectionType;
    int		(*function)();
    char	*closure;
{
    int		    haveLocalhost = 0;
    DisplayEntry    *d;
    char	    *clientName = 0, *NetworkAddressToHostname ();

    for (d = database; d; d = d->next)
    {
    	switch (d->type) {
    	case DISPLAY_ALIAS:
	    continue;
    	case DISPLAY_PATTERN:
	    if (!clientName)
		clientName = NetworkAddressToHostname (connectionType,
						       clientAddress);
	    if (!patternMatch (clientName, d->entry.displayPattern))
		continue;
	    break;
    	case DISPLAY_ADDRESS:
	    if (d->entry.displayAddress.connectionType != connectionType ||
	    	!XdmcpARRAY8Equal (&d->entry.displayAddress.clientAddress,
				  clientAddress))
	    {
		continue;
	    }
	    break;
    	}
	if (!d->hosts)
	    continue;
	if (d->notAllowed)
	    break;
	if (!d->chooser)
	    break;
	if (scanHostlist (d->hosts, clientAddress, connectionType,
			  function, closure, 0, TRUE))
	{
	    haveLocalhost = 1;
	}
	break;
    }
    if (clientName)
	free (clientName);
    if (haveLocalhost)
	(*function) (connectionType, getLocalAddress(), closure);
}

/*
 * returns TRUE if the given client is acceptable to the local host.  The
 * given display client is acceptable if it occurs without a host list.
 */

AcceptableDisplayAddress (clientAddress, connectionType, type)
    ARRAY8Ptr	clientAddress;
    CARD16	connectionType;
    xdmOpCode	type;
{
    DisplayEntry    *d;
    char	    *clientName = 0, *NetworkAddressToHostname ();

    if (!*accessFile)
	return 1;
    if (type == INDIRECT_QUERY)
	return 1;
    for (d = database; d; d = d->next)
    {
	if (d->hosts)
	    continue;
    	switch (d->type) {
    	case DISPLAY_ALIAS:
	    continue;
    	case DISPLAY_PATTERN:
	    if (!clientName)
		clientName = NetworkAddressToHostname (connectionType,
						       clientAddress);
	    if (!patternMatch (clientName, d->entry.displayPattern))
		continue;
	    break;
    	case DISPLAY_ADDRESS:
	    if (d->entry.displayAddress.connectionType != connectionType ||
	    	!XdmcpARRAY8Equal (&d->entry.displayAddress.clientAddress,
				  clientAddress))
	    {
		continue;
	    }
	    break;
    	}
	break;
    }
    if (clientName)
	free (clientName);
    return (d != 0) && (d->notAllowed == 0);
}

#endif /* XDMCP */
