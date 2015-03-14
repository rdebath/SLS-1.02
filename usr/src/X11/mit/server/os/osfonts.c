/************************************************************************
Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

************************************************************************/

/* $XConsortium: osfonts.c,v 1.37 91/07/02 09:14:47 rws Exp $ */

#include <stdio.h>
#include "Xos.h"
#include <sys/dir.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <errno.h>

#include "X.h"
#include "Xmd.h"
#include "Xproto.h"
#include "dixfontstr.h"
#include "fontstruct.h"
#include "osstruct.h"
#include "misc.h"
#include "opaque.h"
#include "dixstruct.h"

#include "fonttype.h"
#include "fontdir.h"

#ifndef X_NOT_POSIX
#ifdef _POSIX_SOURCE
#include <limits.h>
#else
#define _POSIX_SOURCE
#include <limits.h>
#undef _POSIX_SOURCE
#endif
#endif
#ifndef PATH_MAX
#ifdef MAXPATHLEN
#define PATH_MAX MAXPATHLEN
#else
#define PATH_MAX 1024
#endif
#endif

#define MAXFNAMELEN 1024

extern int errno;

extern void CopyISOLatin1Lowered();
extern Atom	MakeAtom();
extern char *defaultFontPath;

static int	ReadFontAlias ();
FontFile	FindFontFile ();
static int	ReadFontDir ();

FontPathPtr		GetFontPath ();

static FontPathPtr searchList = (FontPathPtr)NULL;

extern void FreeScaledFonts();
extern FontFile FindScaledFont();

void
FreeFontRecord(pFP)
    FontPathPtr pFP;
{
    int i;
    for (i=0; i<pFP->npaths; i++) {
        xfree(pFP->paths[i]);
    }
    xfree(pFP->paths);
    xfree(pFP->length);
    xfree(pFP->osPrivate);
    xfree(pFP);
}

static FontPathPtr
MakeFontPathRecord(size)
    unsigned	size;
{
    FontPathPtr fp;    

    fp = (FontPathPtr)xalloc(sizeof(FontPathRec));
    if (fp) {
	fp->npaths = 0;
	fp->size = size;
	fp->length = (int *)xalloc(size * sizeof(int));
	fp->paths = (char **)xalloc(size * sizeof(char *));
	fp->osPrivate = (pointer *)xalloc(size * sizeof(pointer));
	if (!fp->length || !fp->paths || !fp->osPrivate) {
	    xfree(fp->length);
	    xfree(fp->paths);
	    xfree(fp->osPrivate);
	    xfree(fp);
	    fp = (FontPathPtr)NULL;
	}
    }
    return fp;
}

static int
AddFontPathElement(path, element, length, fontDir)
    FontPathPtr path;
    char *element;
    int  length;
    Bool fontDir;
{
    int index = path->npaths;
    FontTable table;
    char *nelt;
    int status;

    if (fontDir)
    {
	status = ReadFontDir(element, &table, path);
	if (status != Success)
	    return status;
    }
    nelt = (char *)xalloc(length + 1);
    if (!nelt)
	return BadAlloc;
    if (index >= path->size)
    {
	int size = path->size << 1;
	int *nlength;
	char **npaths;
	pointer *npriv;

	nlength = (int *)xrealloc(path->length, size*sizeof(int));
	npaths = (char **)xrealloc(path->paths, size*sizeof(char *));
	npriv = (pointer *)xrealloc(path->osPrivate, size * sizeof(pointer));
	if (nlength && npaths && npriv)
	{
	    path->size = size;
	    path->length = nlength;
	    path->paths = npaths;
	    path->osPrivate = npriv;
	}
	else
	{
	    xfree(nelt);
	    xfree(nlength);
	    xfree(npaths);
	    xfree(npriv);
	    return BadAlloc;
	}
    }
    path->length[index] = length;
    path->paths[index] = nelt;
    strncpy(nelt, element, length);
    nelt[length] = '\0';
    path->osPrivate[index] = (pointer)table;
    path->npaths++;
    return Success;
}

static void
FreeFontPath(path)
    FontPathPtr path;
{
    int     i, j;
    FontPtr font;
    FontTable table;

    /* 
     * First all the back pointers for the outstanding fonts must be smashed
     * to NULL so that when the font is freed, the removal from the (now
     * freed) previous table can be skipped.
     */
    if (path) {
	for (i = 0; i < path->npaths; i++) {
	    table = (FontTable) path->osPrivate[i];
	    for (j = 0; j < table->file.used; j++) {
		if (!table->file.ff[j].alias &&
		    (font = (FontPtr) table->file.ff[j].private))
		    font->osPrivate = NULL;
	    }
	    FreeFontTable (table);
	}
	FreeFontRecord (path);
    }
}

/*
 * Font paths are not smashed to lower case. (Note "/usr/lib/X11/fonts")
 *
 * Allow initial font path to have names separated by spaces tabs or commas
 */
int
SetDefaultFontPath(name)
    char *	name;
{
    register char *start, *end;
    char dirName[PATH_MAX];
    FontPathPtr path;
    int status;

    path = MakeFontPathRecord(3);
    if (!path)
	return BadAlloc;
    end = name;
    for (;;) {
	start = end;
	while ((*start == ' ') || (*start == '\t') || (*start == ','))
	    start++;
	if (*start == '\0')
	    break;
	end = start;
	while ((*end != ' ') && (*end != '\t') && (*end != ',') &&
		(*end != '\0'))
	   end++;
	strncpy(dirName, start, end - start);
	dirName[end - start] = '\0';
	if (dirName[strlen(dirName) - 1] != '/')
	    strcat(dirName, "/");
	status = AddFontPathElement(path, dirName, strlen (dirName), True);
	if (status != Success)
	{
	    FreeFontPath(path);
	    return status;
	}
    }
    FreeFontPath(searchList);
    searchList = path;
    FreeScaledFonts();
    return Success;
}

int
SetFontPath(client, npaths, countedStrings)
    ClientPtr	client;
    unsigned	npaths;
    char *	countedStrings;
{
    int i;
    unsigned char * bufPtr = (unsigned char *)countedStrings;
    char dirName[PATH_MAX];
    unsigned int n;
    FontPathPtr path;
    int status;

    if (npaths == 0)
	return SetDefaultFontPath(defaultFontPath); /* this frees old paths */

    path = MakeFontPathRecord(npaths);
    if (!path)
	return BadAlloc;
    for (i=0; i<npaths; i++) {
	n = (unsigned int)(*bufPtr++);
	strncpy(dirName, (char *) bufPtr, (int) n);
	dirName[n] = '\0';
	if (dirName[n - 1] != '/')
	    strcat(dirName, "/");
	status = AddFontPathElement(path, dirName, strlen (dirName), True);
	if (status != Success)
	{
	    FreeFontPath(path);
	    client->errorValue = i;
	    return status;
	}
	bufPtr += n;
    }
    FreeFontPath(searchList);
    searchList = path;
    FreeScaledFonts();
    return Success;
}

FontPathPtr
GetFontPath()
{
    return(searchList);
}

static int
FindFileType(name)
    char *name;
{
    register int i, j, k;
    char *ext;

    k = strlen(name);
    for (i = 0; fontFileReaders[i].extension; i++) {
	ext = fontFileReaders[i].extension;
	j = strlen(ext);
	if ((k > j) && (strcmp(ext, name + k - j) == 0))
	    return i;
    }
    return -1;
}

static int
ReadFontDir(directory, ptable, path)
    char	*directory;
    FontTable	*ptable;
    FontPathPtr path;
{
    char file_name[PATH_MAX];
    char font_name[MAXFNAMELEN];
    char dir_file[PATH_MAX];
    char BUF[BUFSIZ];
    FILE *file;
    int count, i, status;
    FontTable matchTable;
    FontTable table = NullTable;

    strcpy(dir_file, directory);
    if (directory[strlen(directory) - 1] != '/')
 	strcat(dir_file, "/");
    strcat(dir_file, FontDirFile);
    file = fopen(dir_file, "r");
    if (file)
    {
	setbuf (file, BUF);
	count = fscanf(file, "%d\n", &i);
	if ((count == EOF) || (count != 1)) {
	    fclose(file);
	    return BadValue;
	}
	table = MakeFontTable(directory, i);
	for (;;) {
	    count = fscanf(file, "%s %[^\n]\n", file_name, font_name);
	    if (count == EOF)
		break;
	    if (count != 2) {
	        fclose(file);
		return BadValue;
	    }
	    if (!FindFontFile (path, font_name, 0, FALSE, &matchTable) &&
		(FindFileType (file_name) >= 0)) {
		i = AddFileEntry(table, file_name, False);
		if (i < 0) {
		    fclose(file);
		    return BadAlloc;
		}
		i = AddNameEntry(table, font_name, i);
		if (i <= 0) {
		    fclose(file);
		    return (i ? BadAlloc : BadValue);
		}
	    }
	}
	fclose(file);
    }
    else if (errno != ENOENT)
    {
	return BadValue;
    }
    status = ReadFontAlias(directory, FALSE, &table, path);
    if (status != Success)
    {
	FreeFontTable(table);
	return status;
    }
    if (!table)
	return BadValue;
    /*
     * At this point, no more entries will be made in file table. This means
     * that it will not be realloced and we can put pointers (rather than
     * indices into the table.
     */
    for (i = 0; i < table->name.used; i++)
	table->name.fn[i].u.ff = &table->file.ff[table->name.fn[i].u.index];

    *ptable = table;
    return Success;
}

/* 
 * Make each of the file names an automatic alias for each of the files.
 * This assumes that all file names are of the form <root>.<extension>.
 */

static Bool
AddFileNameAliases(table, path)
    FontTable table;
    FontPathPtr path;
{
    int i, typeIndex;
    Boolean found;
    FontTable	matchTable;

    char copy[PATH_MAX];

    for (i = 0; i < table->file.used; i++) {
	if (table->file.ff[i].alias)
	    continue;
	strcpy(copy, table->file.ff[i].name);
	typeIndex = FindFileType(copy);
	copy[strlen(copy) - strlen(fontFileReaders[typeIndex].extension)] = NUL;
	CopyISOLatin1Lowered ((unsigned char *)copy, (unsigned char *)copy,
			      strlen (copy));
	(void)  FindNameInFontTable(table, copy, &found);
	if (!found && !FindFontFile (path, copy, 0, FALSE, &matchTable)) {
	    if (AddNameEntry(table, copy, i) < 0)
		return FALSE;
	}
    }
    return TRUE;
}

/*
 * parse the font.aliases file.  Format is:
 *
 * alias font-name
 *
 * To imbed white-space in an alias name, enclose it like "font name" 
 * in double quotes.  \ escapes and character, so
 * "font name \"With Double Quotes\" \\ and \\ back-slashes"
 * works just fine.
 */

/*
 * token types
 */

static int	lexAlias (), lexc ();

# define NAME		0
# define NEWLINE	1
# define DONE		2
# define EALLOC		3

static int
ReadFontAlias(directory, isFile, ptable, path)
    char      *directory;
    Bool      isFile;
    FontTable *ptable;
    FontPathPtr path;
{
    char alias[MAXFNAMELEN];
    char font_name[MAXFNAMELEN];
    char alias_file[PATH_MAX];
    char buf[BUFSIZ];
    FILE *file;
    int i;
    FontTable matchTable;
    FontTable table;
    int	token;
    Bool found;
    char *lexToken;
    int status = Success;

    table = *ptable;
    strcpy(alias_file, directory);
    if (!isFile)
    {
	if (directory[strlen(directory) - 1] != '/')
	    strcat(alias_file, "/");
	strcat(alias_file, AliasFile);
    }
    file = fopen(alias_file, "r");
    if (!file)
	return ((errno == ENOENT) ? Success : BadValue);
    setbuf (file, buf);
    if (!table)
	*ptable = table = MakeFontTable (directory, 10);
    if (!table)
	return BadAlloc;
    while (status == Success) {
	token = lexAlias (file, &lexToken);
	switch (token) {
	case NEWLINE:
	    break;
	case DONE:
	    fclose (file);
	    return Success;
	case EALLOC:
	    status = BadAlloc;
	    break;
	case NAME:
	    strcpy (alias, lexToken);
	    token = lexAlias (file, &lexToken);
	    switch (token) {
	    case NEWLINE:
		if (strcmp (alias, "FILE_NAMES_ALIASES"))
		    status = BadValue;
		else if (!AddFileNameAliases (table, path))
		    status = BadAlloc;
		break;
	    case DONE:
		status = BadValue;
		break;
	    case EALLOC:
		status = BadAlloc;
		break;
	    case NAME:
		CopyISOLatin1Lowered ((unsigned char *)alias,
				      (unsigned char *)alias,
				      strlen (alias));
		CopyISOLatin1Lowered ((unsigned char *)font_name,
				      (unsigned char *)lexToken,
				      strlen (lexToken));
		(void) FindNameInFontTable (table, alias, &found);
		if (!found &&
		    !FindFontFile (path, alias, 0, FALSE, &matchTable)) {
		    i = AddFileEntry(table, font_name, True);
		    if (i < 0) {
			status = BadAlloc;
		    } else {
			i = AddNameEntry(table, alias, i);
			if (i <= 0)
			    status = (i ? BadAlloc : BadValue);
		    }
		}
		break;
	    }
	}
    }
    fclose(file);
    return status;
}

# define QUOTE		0
# define WHITE		1
# define NORMAL		2
# define END		3
# define NL		4

static int	charClass;

static int
lexAlias (file, lexToken)
FILE	*file;
char	**lexToken;
{
	int		c;
	char		*t;
	enum state { Begin, Normal, Quoted } state;
	int		count;

	static char	*tokenBuf = (char *)NULL;
	static int	tokenSize = 0;

	t = tokenBuf;
	count = 0;
	state = Begin;
	for (;;) {
		if (count == tokenSize) {
		    int nsize;
		    char *nbuf;

		    nsize = tokenSize ? (tokenSize << 1) : 64;
		    nbuf = (char *)xrealloc(tokenBuf, nsize);
		    if (!nbuf)
			return EALLOC;
		    tokenBuf = nbuf;
		    tokenSize = nsize;
		    t = tokenBuf + count;
		}
		c = lexc (file);
		switch (charClass) {
		case QUOTE:
			switch (state) {
			case Begin:
			case Normal:
				state = Quoted;
				break;
			case Quoted:
				state = Normal;
				break;
			}
			break;
		case WHITE:
			switch (state) {
			case Begin:
				continue;
			case Normal:
				*t = '\0';
				*lexToken = tokenBuf;
				return NAME;
			case Quoted:
				break;
			}
			/* fall through */
		case NORMAL:
			switch (state) {
			case Begin:
				state = Normal;
			}
			*t++ = c;
			++count;
			break;
		case END:
		case NL:
			switch (state) {
			case Begin:
				*lexToken = (char *)NULL;
				return charClass == END ? DONE : NEWLINE;
			default:
				*t = '\0';
				*lexToken = tokenBuf;
				ungetc (c, file);
				return NAME;
			}
		}
	}
}

static int
lexc (file)
FILE	*file;
{
	int	c;
	c = getc (file);
	switch (c) {
	case EOF:
		charClass = END;
		break;
	case '\\':
		c = getc (file);
		if (c == EOF)
			charClass = END;
		else
			charClass = NORMAL;
		break;
	case '"':
		charClass = QUOTE;
		break;
	case ' ':
	case '\t':
		charClass = WHITE;
		break;
	case '\n':
		charClass = NL;
		break;
	default:
		charClass = NORMAL;
		break;
	}
	return c;
}

static Bool
SearchDirectory(index, pat, fontList, limit)
    int index;
    char *pat;
    FontPathPtr fontList;
    unsigned limit;
{
    int i, res, len, head, tail;
    FontTable table;

    if (!searchList)
	return TRUE;
    table = (FontTable)searchList->osPrivate[index];
    i = SetupWildMatch(table, pat, (char *)NULL, &head, &tail, &len);
    while (i < table->name.used)
    {
	res = Match(table->name.fn[i].name, pat, head, tail, len);
	if (res > 0)
	{
	    if (AddFontPathElement(fontList, table->name.fn[i].name,
				   strlen(table->name.fn[i].name), False))
		return FALSE;
	    if (fontList->npaths >= limit)
		break;
	}
	else if (res < 0)
	    break;
	i++;
    }
    return TRUE;
}

/*******************************************************************
 *  ExpandFontNamePattern
 *
 *	Returns a FontPathPtr with at most max-names, of names of fonts
 *      matching
 *	the pattern.  The pattern should use the ASCII encoding, and
 *      upper/lower case does not matter.  In the pattern, the '?' character
 *	(octal value 77) will match any single character, and the character '*'
 *	(octal value 52) will match any number of characters.  The return
 *	names are in lower case.
 *
 *      Used only by protocol request ListFonts & ListFontsWithInfo
 *******************************************************************/

FontPathPtr
ExpandFontNamePattern(lenpat, countedPattern, maxNames)
    unsigned	lenpat;
    char	*countedPattern;
    unsigned	maxNames;
{
    char	*pattern;
    int		i;
    FontPathPtr	fpr;

    if (!searchList)
	return (FontPathPtr)NULL;
    /* random number, this is a guess, but it hardly matters. */
    fpr = MakeFontPathRecord ((unsigned) 100);
    if (!fpr)
	return (FontPathPtr)NULL;
    pattern = (char *)ALLOCATE_LOCAL (lenpat + 1);
    if (!pattern)
    {
	FreeFontRecord(fpr);
	return (FontPathPtr)NULL;
    }
    /*
     * make a pattern which is guaranteed NULL-terminated
     */
    CopyISOLatin1Lowered((unsigned char *)pattern,
			 (unsigned char *)countedPattern,
			 (int) lenpat);

    for ( i=0; i<searchList->npaths; i++)
    {
	if (!SearchDirectory(i, pattern, fpr, maxNames))
	{
	    DEALLOCATE_LOCAL(pattern);
	    FreeFontRecord(fpr);
	    return (FontPathPtr)NULL;
	}
	if (fpr->npaths >= maxNames)
	    break;
    }
    DEALLOCATE_LOCAL(pattern);
    return fpr;
}

/*
 * OS interface to reading font files. This is not called by the dix routines
 * but rather by any of the pseudo-os-independent font internalization
 * routines.
 */

int
FontFileRead(buf, itemsize, nitems, fid)
    char	*buf;
    unsigned	itemsize;
    unsigned	nitems;
    FID		fid;
{
    FontFileReadFunc	reader;

    if (reader = fontFileReaders[((FontFIDPtr) fid)->type].read)
	return reader ( buf, (int) itemsize, (int) nitems, ((FontFIDPtr)fid)->fid);
    return (int) fread ( buf, (int) itemsize, (int) nitems, ((FontFIDPtr)fid)->fid);
}

int
FontFileSkip(bytes, fid)
    unsigned	bytes;
    FID		fid;
{
    struct stat	stats;
    int c;
    FontFileSkipFunc	skipper;

    if ((skipper = fontFileReaders[((FontFIDPtr) fid)->type].skip))
	return skipper ( bytes, ((FontFIDPtr)fid)->fid);

    fid = ((FontFIDPtr)fid)->fid;
    fstat(fileno((FILE *)fid), &stats);
    if ((stats.st_mode & S_IFMT) == S_IFREG)
	c = fseek((FILE *) fid, (long) bytes, 1);
    else
	while (bytes-- && ((c = getc((FILE *)fid)) != EOF))
	    ;
    return c;
}

/*
 * When a font is unloaded (destroyed) it is necessary to remove the pointer
 * in the font table since it is no longer valid. This means that the
 * "obvious" technique of putting the FontPtr into the table when an aliased
 * font is loaded would mean that the table would have to be searched for
 * any matching entry. To avoid scanning all the tables when a font is FontPtr
 * destroyed, the Font has a back pointer to the FontFile (in the table) where
 * it was entered.
 *
 * So that aliases will not also keep a copy of the FontPtr in their FontTable
 * entries, a pointer to the "resolved" FontTable is actually kept and the
 * indirection is taken.
 *
 * A slight non-conformance to the protocol is possible here. If any
 * FontTable is for a file that does not load (i.e. was changed since the
 * font.dirs was created), then that font name will not work even though it
 * should because of wild carding or the use of a search path. The moral is
 * is that the font.dirs should be correct.
 *
 * To prevent circular aliases from crashing the server (due to the recursive
 * nature of FindFontFile) a limit of MAX_LINKS is put on the length of a
 * chain that will be followed.
 */

#define MAX_LINKS 20

FontFile 
FindFontFile(path, fontname, depth, followAliases, table)
    FontPathPtr path;
    char *	fontname;
    int		depth;
    Bool	followAliases;	
    FontTable	*table; 	/* returned */
{
    int     nameIndex, i;
    Bool found;
    FontFile ff, resolved;

    if (!path || (depth >= MAX_LINKS))
	return NullFontFile;
    for (i = 0; i < path->npaths; i++)
    {
	*table = (FontTable) path->osPrivate[i];
	nameIndex = FindNameInFontTable (*table, fontname, &found);
	if (!found)
	    continue;
	ff = (*table)->name.fn[nameIndex].u.ff;
	if (!ff->alias || !followAliases)
	    return ff;
	/* since the cached FontFile might be from a different table, ensure
	 * that the font is still loaded, otherwise OpenFontFile will fail */
	if (ff->private && ((FontFile)ff->private)->private)
	    return (FontFile) ff->private;
	if (resolved = FindFontFile (path, ff->name, depth + 1, TRUE, table))
	{
	    if (!resolved->alias) /* alias here means a scaled font */
		ff->private = (Opaque) resolved;
	    return resolved;
	}
    }
    if (followAliases)
	return FindScaledFont(path, fontname, table);
    return NullFontFile;
}

#if defined(SYSV) && !defined(hpux)		/* hpux does have vfork */
#define vfork() fork()
#endif

#ifdef apollo               /* Domain/OS has vfork but it breaks the graphics layer */
#define vfork() fork()
#endif

static int
FontFilter(fp, filter)
    FILE *fp;
    char **filter;
{
    int pfd[2];
    int pid;

    if (pipe(pfd) < 0) {
	fclose(fp);
	return (-1);
    }
    switch(pid = vfork()) {
    case 0:
	dup2(fileno(fp), 0);
	close(fileno(fp));
	dup2(pfd[1], 1);
	close(pfd[0]);
	close(pfd[1]);
	execvp(filter[0], filter);
	_exit(127);
    case -1:
	close(pfd[0]);
	close(pfd[1]);
	fclose(fp);
	return(-1);
    default:
	dup2(pfd[0], fileno(fp));
	close(pfd[0]);
	close(pfd[1]);
	return(pid);
    }    
}

static FILE *
OpenFontFile(table, name, typeIndex, pid)
    FontTable table;
    char     *name;
    int	     *typeIndex;
    int      *pid;
{
    char    pathName[PATH_MAX];
    FILE   *fp;
    char **filter;

    strcpy (pathName, table->directory);
    strcat (pathName, name);
    if ((fp = fopen (pathName, "r")) == (FILE *)NULL)
	return fp;
    *pid = 0;
    *typeIndex = FindFileType (name);
    filter = fontFileReaders[*typeIndex].filter;
    if (filter) {
	*pid = FontFilter(fp, filter);
	if (*pid < 0)
	    return (FILE *)NULL;
    }
    return fp;
}

static void
CloseFontFile(fp, pid)
    FILE *fp;
    int pid;
{
    int child;

    fclose (fp);
    if (pid > 0)
       do { child = wait(0); } while (child != pid && child != -1);
}

FontPtr 
FontFileLoad(fontname, length)
    unsigned	length;
    char *	fontname;
{
    FontPtr	font;
    char	fName[MAXFNAMELEN];
    char	buf[BUFSIZ];
    FILE	* fp;
    FontTable	table;
    FontFile	ff;
    int		typeIndex;
    int		cookie;
    FontFIDRec	fid;

    CopyISOLatin1Lowered((unsigned char *)fName, (unsigned char *)fontname,
			 (int) length);
    ff = FindFontFile (searchList, fName, 1, TRUE, &table);
    if (!ff)
	return NullFont;
    if (ff->private != NULL)
	return (FontPtr) ff->private;	/* already loaded */
    if ((fp = OpenFontFile(table, ff->name, &typeIndex, &cookie)) == NULL)
	return NullFont;
    setbuf (fp, buf);
    fid.type = typeIndex;
    fid.fid = (FID) fp;
    if (fontFileReaders[typeIndex].init)
    {
	fid.fid = (FID) (*fontFileReaders[typeIndex].init) (fp);
	if (!fid.fid)
	{
	    fclose (fp);
	    return NullFont;
	}
    }
    font = (fontFileReaders[typeIndex].loadFont) (&fid);
    if (fontFileReaders[typeIndex].done)
	(*fontFileReaders[typeIndex].done) (fid.fid);
    CloseFontFile(fp, cookie);
    if (font == NullFont)
	return NullFont;
    ff->private = (Opaque) font;
    font->refcnt = 0;
    font->fileType = typeIndex;
    font->osPrivate = (pointer) ff;
    return font;
}

/*
 * This either returns an existing font, or if that can't be found,
 * then fills in the FontInfo and FontProp by reading from the disk.
 */

Bool
FontFilePropLoad(fontname, length, font, fi, props)
    char	*fontname;
    unsigned int length;
    FontInfoPtr fi;
    DIXFontPropPtr *props;	/* return */
    FontPtr	*font;		/* return */
{
    char	fName[MAXFNAMELEN];
    FILE *	fp;
    FontTable	table;
    FontFile	ff;
    Bool	found;
    int		typeIndex;
    char	buf[BUFSIZ];
    int		cookie;
    FontFIDRec	fid;

    CopyISOLatin1Lowered((unsigned char *)fName, (unsigned char *)fontname,
			 (int) length);
    ff = FindFontFile (searchList, fName, 1, TRUE, &table);
    if (!ff)
	return FALSE;
    if (ff->private != NULL) {
	*font = (FontPtr) ff->private;
	return TRUE;
    }
    *font = NullFont;
    if ((fp = OpenFontFile(table, ff->name, &typeIndex, &cookie)) == NULL)
	return FALSE;
    setbuf (fp, buf);
    fid.type = typeIndex;
    fid.fid = (FID) fp;
    if (fontFileReaders[typeIndex].init)
    {
	fid.fid = (FID) (*fontFileReaders[typeIndex].init) (fp);
	if (!fid.fid)
	{
	    fclose (fp);
	    return FALSE;
	}
    }
    found = (*fontFileReaders[typeIndex].loadProperties) (&fid, fi, props);
    if (fontFileReaders[typeIndex].done)
	(*fontFileReaders[typeIndex].done) (fid.fid);
    CloseFontFile(fp, cookie);
    return found;
}

void FontUnload(font)
    FontPtr font;
{
    FontFile ff;
    if ((ff = (FontFile)font->osPrivate) != NULL)
	ff->private = NULL;
    (*fontFileReaders[font->fileType].freeFont)(font);
}
