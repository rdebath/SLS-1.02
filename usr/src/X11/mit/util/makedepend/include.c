/*
 * $XConsortium: include.c,v 1.11 91/04/05 17:33:48 rws Exp $
 */
#include "def.h"

extern struct	inclist	inclist[ MAXFILES ],
			*inclistp;
extern char	*includedirs[ ];
extern char	*notdotdot[ ];
extern boolean show_where_not;

struct inclist *inc_path(file, include, dot)
	register char	*file,
			*include;
	boolean	dot;
{
	static char	path[ BUFSIZ ];
	register char		**pp, *p;
	register struct inclist	*ip;
	struct stat	st;
	boolean	found = FALSE;

	/*
	 * Check all previously found include files for a path that
	 * has already been expanded.
	 */
	for (ip = inclist; ip->i_file; ip++)
	    if ((strcmp(ip->i_incstring, include) == 0) && !ip->i_included_sym)
	    {
		found = TRUE;
		break;
	    }

	/*
	 * If the path was surrounded by "", then check the absolute
	 * path provided.
	 */
	if (!found && dot) {
		if (stat(include, &st) == 0) {
			ip = newinclude(include, include);
			found = TRUE;
		}
		else if (show_where_not)
			warning1("\tnot in %s\n", include);
	}

	/*
	 * See if this include file is in the directory of the
	 * file being compiled.
	 */
	if (!found) {
		for (p=file+strlen(file); p>file; p--)
			if (*p == '/')
				break;
		if (p == file)
			strcpy(path, include);
		else {
			strncpy(path, file, (p-file) + 1);
			path[ (p-file) + 1 ] = '\0';
			strcpy(path + (p-file) + 1, include);
		}
		remove_dotdot(path);
		if (stat(path, &st) == 0) {
			ip = newinclude(path, include);
			found = TRUE;
		}
		else if (show_where_not)
			warning1("\tnot in %s\n", path);
	}

	/*
	 * Check the include directories specified. (standard include dir
	 * should be at the end.)
	 */
	if (!found)
		for (pp = includedirs; *pp; pp++) {
			sprintf(path, "%s/%s", *pp, include);
			remove_dotdot(path);
			if (stat(path, &st) == 0) {
				ip = newinclude(path, include);
				found = TRUE;
				break;
			}
			else if (show_where_not)
				warning1("\tnot in %s\n", path);
		}

	if (!found)
		ip = NULL;
	return(ip);
}

/*
 * Ocaisionally, pathnames are created that look like ../x/../y
 * Any of the 'x/..' sequences within the name can be eliminated.
 * (but only if 'x' is not a symbolic link!!)
 */
remove_dotdot(path)
	char	*path;
{
	register char	*end, *from, *to, **cp;
	char		*components[ MAXFILES ],
			newpath[ BUFSIZ ];
	boolean		component_copied;

	/*
	 * slice path up into components.
	 */
	to = newpath;
	if (*path == '/')
		*to++ = '/';
	*to = '\0';
	cp = components;
	for (from=end=path; *end; end++)
		if (*end == '/') {
			while (*end == '/')
				*end++ = '\0';
			if (*from)
				*cp++ = from;
			from = end;
		}
	*cp++ = from;
	*cp = NULL;

	/*
	 * Now copy the path, removing all 'x/..' components.
	 */
	cp = components;
	component_copied = FALSE;
	while(*cp) {
		if (!isdot(*cp) && !isdotdot(*cp) && isdotdot(*(cp+1))) {
			if (issymbolic(newpath, *cp))
				goto dont_remove;
			cp++;
		} else {
		dont_remove:
			if (component_copied)
				*to++ = '/';
			component_copied = TRUE;
			for (from = *cp; *from; )
				*to++ = *from++;
			*to = '\0';
		}
		cp++;
	}
	*to++ = '\0';

	/*
	 * copy the reconstituted path back to our pointer.
	 */
	strcpy(path, newpath);
}

isdot(p)
	register char	*p;
{
	if(p && *p++ == '.' && *p++ == '\0')
		return(TRUE);
	return(FALSE);
}

isdotdot(p)
	register char	*p;
{
	if(p && *p++ == '.' && *p++ == '.' && *p++ == '\0')
		return(TRUE);
	return(FALSE);
}

issymbolic(dir, component)
	register char	*dir, *component;
{
#ifdef S_IFLNK
	struct stat	st;
	char	buf[ BUFSIZ ], **pp;

	sprintf(buf, "%s%s%s", dir, *dir ? "/" : "", component);
	for (pp=notdotdot; *pp; pp++)
		if (strcmp(*pp, buf) == 0)
			return (TRUE);
	if (lstat(buf, &st) == 0
	&& (st.st_mode & S_IFMT) == S_IFLNK) {
		*pp++ = copy(buf);
		if (pp >= &notdotdot[ MAXDIRS ])
			fatal("out of .. dirs, increase MAXDIRS\n");
		return(TRUE);
	}
#endif
	return(FALSE);
}

/*
 * Add an include file to the list of those included by 'file'.
 */
struct inclist *newinclude(newfile, incstring)
	register char	*newfile, *incstring;
{
	register struct inclist	*ip;

	/*
	 * First, put this file on the global list of include files.
	 */
	ip = inclistp++;
	if (inclistp == inclist + MAXFILES - 1)
		fatal("out of space: increase MAXFILES\n");
	ip->i_file = copy(newfile);
	ip->i_included_sym = FALSE;
	if (incstring == NULL)
		ip->i_incstring = ip->i_file;
	else
		ip->i_incstring = copy(incstring);

	return(ip);
}

included_by(ip, newfile)
	register struct inclist	*ip, *newfile;
{
	register i;

	if (ip == NULL)
		return;
	/*
	 * Put this include file (newfile) on the list of files included
	 * by 'file'.  If 'file' is NULL, then it is not an include
	 * file itself (i.e. was probably mentioned on the command line).
	 * If it is already on the list, don't stick it on again.
	 */
	if (ip->i_list == NULL)
		ip->i_list = (struct inclist **)
			malloc(sizeof(struct inclist *) * ++ip->i_listlen);
	else {
		for (i=0; i<ip->i_listlen; i++)
			if (ip->i_list[ i ] == newfile) {
			    i = strlen(newfile->i_file);
			    if (!ip->i_included_sym &&
				!(i > 2 &&
				  newfile->i_file[i-1] == 'c' &&
				  newfile->i_file[i-2] == '.'))
			    {
				/* only bitch if ip has */
				/* no #include SYMBOL lines  */
				/* and is not a .c file */
				warning("%s includes %s more than once!\n",
					ip->i_file, newfile->i_file);
				warning1("Already have\n");
				for (i=0; i<ip->i_listlen; i++)
					warning1("\t%s\n", ip->i_list[i]->i_file);
			    }
			    return;
			}
		ip->i_list = (struct inclist **) realloc(ip->i_list,
			sizeof(struct inclist *) * ++ip->i_listlen);
	}
	ip->i_list[ ip->i_listlen-1 ] = newfile;
}

inc_clean ()
{
	register struct inclist *ip;

	for (ip = inclist; ip < inclistp; ip++) {
		ip->i_marked = FALSE;
	}
}
