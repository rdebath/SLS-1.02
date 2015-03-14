#define d_namlen d_reclen
/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)ftw.c	5.3 (Berkeley) 8/5/88";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <sys/dir.h>
#include <sys/stat.h>
#include <errno.h>
#include "ftw.h"

#define	NODESC	-1

#define	ISLINK(sb)	((sb.st_mode&S_IFMT) == S_IFLNK)
#define	ISDIR(sb)	((sb.st_mode&S_IFMT) == S_IFDIR)
#define	ISDOT(dp) \
	(dp->d_name[0] == '.' && (!dp->d_name[1] || \
	    dp->d_name[1] == '.' && !dp->d_name[2]))

extern int errno;
static int g_fds, (*g_fn)(), g_opts;
static char *bp;

/* S5 compatible ftw(BA_LIB) */
ftw(path, fn, maxfds)
	char *path;
	int (*fn)(), maxfds;
{
	return(treewalk(path, fn, maxfds, 0));
}

treewalk(path, fn, maxfds, opts)
	char *path;
	int (*fn)(), maxfds, opts;
{
	struct stat sb;
	int rval;
	char *pwd, *getwd(), *malloc(), *strcpy();

	if (lstat(path, &sb))
		return(errno == EACCES ? (*fn)(path, &sb, FTW_NS) : -1);

	pwd = NULL;
	if (ISLINK(sb) && opts&FTW_SYMLINK) {
		if (stat(path, &sb))
			return(0);
		if (ISDIR(sb)) {
			/* NOSTRICT */
			if (!(pwd = malloc((u_int)MAXPATHLEN)))
				return(-1);
			if (!getwd(pwd))
				return(-1);
		}
	}

	if (!ISDIR(sb))
		return((*fn)(path, &sb, FTW_F));

	if (!maxfds)
		return(-1);
	g_fds = maxfds == -1 ? getdtablesize() : maxfds;
	g_fn = fn;
	g_opts = opts;

	if (!(opts&FTW_CHDIR) && !(bp = malloc((u_int)MAXPATHLEN))) {
		errno = ENOMEM;
		return(-1);
	}

	rval = (*fn)(path, &sb, FTW_D);
	if (rval == NODESC)
		rval = 0;
	else if (!rval) {
		if (opts&FTW_CHDIR)
			rval = chwalk(path);
		else
			rval = walk(strcpy(bp, path));
		if (!rval && opts&FTW_DIRLAST) {
			rval = (*fn)(path, &sb, FTW_D2);
			if (rval == NODESC)
				rval = 0;
		}
	}
	if (pwd && chdir(pwd))
		return(-1);
	return(rval);
}

/*
 * cycle through the directories at the top of the tree, otherwise, once
 * you run out of descriptors you have to keep reusing the same one and
 * it gets *real* slow.
 */
typedef struct d_fd {
	struct d_fd *next;
	DIR *dirp;
	off_t off;
} FD;

static FD *freep, *node;

static
walk(name)
	register char *name;
{
	register struct direct *dp;
	register int rval;
	struct stat sb;
	FD cur;
	char *save, *strcpy();

	if (!freep)
		freep = &cur;
	else
		node->next = &cur;
	node = &cur;
	cur.off = 0;

getfd:	if (!g_fds) {
		freep->off = telldir(freep->dirp);
		closedir(freep->dirp);
		freep = freep->next;
		++g_fds;
	}
	if (!(cur.dirp = opendir(bp))) {
		if (errno == EMFILE) {
			g_fds = 0;
			goto getfd;
		}
		return(errno == EACCES ? (*g_fn)(bp, &sb, FTW_DNR) : -1);
	}
	else
		--g_fds;

	for (; *name; ++name);
	*name++ = '/';
	for (rval = 0, dp = readdir(cur.dirp); dp; dp = readdir(cur.dirp)) {
		if (ISDOT(dp))
			continue;
		(void)strcpy(name, dp->d_name);
		if (lstat(bp, &sb)) {
			rval = errno == EACCES ?
			    (*g_fn)(bp, &sb, FTW_NS) : -1;
			if (rval)
				break;
		}
		if (ISLINK(sb) && g_opts&FTW_SYMLINK)
			if (stat(bp, &sb))
				continue;
		if (!ISDIR(sb)) {
			rval = (*g_fn)(bp, &sb, FTW_F);
			if (rval)
				break;
			continue;
		}
		if (g_opts&FTW_DIRLAST)
			save = name + dp->d_namlen;
		rval = (*g_fn)(bp, &sb, FTW_D);
		if (rval && rval != NODESC || (rval = walk(name)))
			break;
		if (g_opts&FTW_DIRLAST) {
			*save = '\0';
			rval = (*g_fn)(dp->d_name, &sb, FTW_D2);
			if (rval)
				if (rval == NODESC)
					rval = 0;
				else
					break;
		}
		if (cur.off) {
			*name = NULL;
			if (cur.dirp = opendir(bp)) {
				seekdir(cur.dirp, cur.off);
				/*
				 * tricky; if we have to reset the directory
				 * pointer we know it's the next one to reuse
				 */
				freep = &cur;
				--g_fds;
			}
			/* directory moved from under us!!! */
			else {
				rval = -1;
				break;
			}
		}
	}
	closedir(cur.dirp);
	++g_fds;
	return(rval);
}

static
chwalk(name)
	register char *name;
{
	register struct direct *dp;
	register int rval;
	struct stat sb;
	FD cur;
	char *pwd, *getwd(), *malloc(), *strcpy();

	if (!freep)
		freep = &cur;
	else
		node->next = &cur;
	node = &cur;
	cur.off = 0;

	if (chdir(name))
		return(errno == EACCES ? (*g_fn)(name, &sb, FTW_DNR) : -1);

getfd:	if (!g_fds) {
		freep->off = telldir(freep->dirp);
		closedir(freep->dirp);
		freep = freep->next;
		++g_fds;
	}
	if (!(cur.dirp = opendir("."))) {
		if (errno == EMFILE) {
			g_fds = 0;
			goto getfd;
		}
		return(errno == EACCES ? (*g_fn)(".", &sb, FTW_DNR) : -1);
	}
	else
		--g_fds;

	for (rval = 0, dp = readdir(cur.dirp); dp; dp = readdir(cur.dirp)) {
		if (ISDOT(dp))
			continue;
		if (lstat(dp->d_name, &sb)) {
			rval = errno == EACCES ?
			    (*g_fn)(dp->d_name, &sb, FTW_NS) : -1;
			if (rval)
				break;
		}
		pwd = NULL;
		if (ISLINK(sb) && g_opts&FTW_SYMLINK) {
			if (stat(dp->d_name, &sb))
				continue;
			if (ISDIR(sb)) {
				/* NOSTRICT */
				if (!(pwd = malloc((u_int)MAXPATHLEN))) {
					rval = -1;
					break;
				}
				if (!getwd(pwd)) {
					rval = -1;
					break;
				}
			}
		}
		if (!ISDIR(sb)) {
			rval = (*g_fn)(dp->d_name, &sb, FTW_F);
			if (rval)
				break;
			continue;
		}
		rval = (*g_fn)(dp->d_name, &sb, FTW_D);
		if (rval && rval != NODESC || (rval = chwalk(dp->d_name)))
			break;
		if (g_opts&FTW_DIRLAST) {
			rval = (*g_fn)(dp->d_name, &sb, FTW_D2);
			if (rval)
				if (rval == NODESC)
					rval = 0;
				else
					break;
		}
		if (pwd && chdir(pwd)) {
			rval = -1;
			break;
		}
		if (cur.off) {
			if (cur.dirp = opendir(".")) {
				seekdir(cur.dirp, cur.off);
				/*
				 * tricky; if we have to reset the directory
				 * pointer we know it's the next one to reuse
				 */
				freep = &cur;
				--g_fds;
			}
			/* directory moved from under us!!! */
			else {
				rval = -1;
				break;
			}
		}
	}
	closedir(cur.dirp);
	++g_fds;
	if (chdir(".."))
		return(-1);
	return(rval);
}
