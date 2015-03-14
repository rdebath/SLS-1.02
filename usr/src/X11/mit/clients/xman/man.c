/*
 * xman - X Window System manual page display program.
 *
 * $Header: /home/x_cvs/mit/clients/xman/man.c,v 1.2 1992/08/25 08:52:47 dawes Exp $
 * $XConsortium: man.c,v 1.29 91/07/30 22:03:20 rws Exp $
 *
 * Copyright 1987, 1988 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * Author:    Chris D. Peterson, MIT Project Athena
 * Created:   August 10, 1987
 */

#include "globals.h"
#include "vendor.h"		/* vendor-specific defines and data */

#ifndef X_NOT_POSIX
#include <dirent.h>
#else
#ifdef SYSV
#include <dirent.h>
#else
#ifdef USG
#include <dirent.h>
#else
#include <sys/dir.h>
#ifndef dirent
#define dirent direct
#endif
#endif
#endif
#endif

#ifdef DEBUG
static char error_buf[BUFSIZ];		/* The buffer for error messages. */
#endif /* DEBUG */

static void SortList(), ReadMandescFile(), SortAndRemove(), InitManual();
static void AddToCurrentSection();
static void ReadCurrentSection();

#ifdef MANCONF
Bool ReadManConfig();
#endif

#define SECT_ERROR -1

/*	Function Name: Man
 *	Description: Builds a list of all manual directories and files.
 *	Arguments: none. 
 *	Returns: the number of manual sections.
 */

int
Man()
{
  SectionList *list = NULL;
  char *ptr, manpath[BUFSIZ], *path, *current_label;
  int sect, num_alloced;

/* 
 * Get the environment variable MANPATH, and if it doesn't exist then use
 * SYSMANPATH and LOCALMANPATH.
 */

  ptr = getenv("MANPATH");
  if (ptr == NULL || streq(ptr , "") ) {
#ifdef MANCONF
    if (!ReadManConfig(manpath))
#endif
    {
      strcpy(manpath, SYSMANPATH);
#ifdef LOCALMANPATH
      strcat(manpath, ":");
      strcat(manpath, LOCALMANPATH);
#endif
    }
  } else {
    strcpy(manpath, ptr);
  }

/*
 * Get the list of manual directories in the users MANPATH that we should
 * open to look for manual pages.  The ``mandesc'' file is read here.
 */

  for ( path = manpath ; (ptr = index(path , ':')) != NULL ; path = ++ptr) { 
    *ptr = '\0';
    ReadMandescFile(&list, path);
  }
  ReadMandescFile(&list, path);

  SortList(&list);
  
  sect = 0;
  num_alloced = SECTALLOC;
  manual = (Manual *) XtMalloc( sizeof(Manual) * num_alloced );
  InitManual( manual, list->label );
  manual[sect].flags = list->flags;
  current_label = NULL;

  while ( list != NULL ) {
    SectionList * old_list;

    if ( current_label == NULL || streq(list->label, current_label) )
      AddToCurrentSection( manual + sect, list->directory);
    else {
      if (manual[sect].nentries == 0) {	/* empty section, re-use it. */
	XtFree(manual[sect].blabel);
	manual[sect].blabel = list->label;
	manual[sect].flags = list->flags;
      }
      else {
	if ( ++sect >= num_alloced ) {
	  num_alloced += SECTALLOC;
	  manual = (Manual *) realloc ( (char *) manual,
				        (sizeof(Manual) * num_alloced));
	  if (manual == NULL) 
	    PrintError("Could not allocate memory for manual sections.");
	}
	InitManual( manual + sect, list->label );
	manual[sect].flags = list->flags;
      }
      AddToCurrentSection( manual + sect, list->directory);
    }
    /* Save label to see if it matches next entry. */
    current_label = list->label; 
    old_list = list;
    list = list->next;
    XtFree((char *) old_list);		/* free what you allocate. */
  }
  if (manual[sect].nentries != 0)
    sect++;			/* don't forget that last section. */
  
  SortAndRemove(manual, sect);

#ifdef notdef			/* dump info. */
  DumpManual(sect);
#endif
  
/*
 * realloc manual to be minimum space necessary.
 */

  manual = (Manual *) realloc( (char *) manual, (sizeof(Manual) * sect));
  if (manual == NULL) 
    PrintError("Could not allocate memory for manual sections.");

  return(sect);		/* return the number of man sections. */
}    

/*	Function Name: SortList
 *	Description: Sorts the list of sections to search.
 *	Arguments: list - a pointer to the list to sort.
 *	Returns: a sorted list.
 *
 * This is the most complicated part of the entire operation.
 * all sections with the same label must by right next to each other,
 * but the sections that are in the standard list have to come first.
 */

static void
SortList(list)
SectionList ** list;
{
  SectionList * local;
  SectionList *head, *last, *inner, *old;
  
  if (*list == NULL)
    PrintError("No manual sections to read, exiting.");

/* 
 * First step 
 * 
 * Look for standard list items, and more them to the top of the list.
 */

  last = NULL;			/* keep Saber happy. */
  for ( local = *list ; local->next != NULL ; local = local->next) {
    if ( local->flags ) {
      if ( local == *list )	/* top element is already standard. */
	break;
      head = local;

      /* Find end of standard block */
      for ( ; (local->next != NULL) && (local->flags) 
	   ; old = local, local = local->next); 

      last->next = old->next; /* Move the block. */
      old->next = *list;
      *list = head;

      break;			/* First step accomplished. */
    }
    last = local;
  }

/*
 *  Second step
 *
 *  Move items with duplicate labels right next to each other.
 */

  local = *list;
  for ( local = *list ; local->next != NULL ; local = local->next) {
    inner = local->next;
    while ( inner != NULL) {
      if ( streq(inner->label, local->label) && (inner != local->next)) {
	last->next = inner->next; /* Move it to directly follow local. */
	inner->next = local->next;
	local->next = inner;
	inner = last;		/* just so that we keep marching down the
				   tree (this keeps us from looping). */
      }
      last = inner;
      inner = inner->next;
    }
  }
}	

/*	Function Name: ReadMandescFile
 *	Description: Reads the mandesc file, and adds more sections as 
 *                   nescessary.
 *	Arguments: path - path name if the current search directory.
 *                 section_list - pointer to the list of sections.
 *	Returns: TRUE in we should use default sections
 */
  
static void
ReadMandescFile( section_list, path )
SectionList ** section_list;
char * path;
{
  char mandesc_file[BUFSIZ];	/* full path to the mandesc file. */
  FILE * descfile;
  char string[BUFSIZ], local_file[BUFSIZ];
  Boolean use_defaults = TRUE;
  char *cp;

  sprintf(mandesc_file, "%s/%s", path, MANDESC);
  if ( (descfile = fopen(mandesc_file, "r")) != NULL) {
    while ( fgets(string, BUFSIZ, descfile) != NULL) {
      string[strlen(string)-1] = '\0';        /* Strip off the CR. */

      if ( streq(string, NO_SECTION_DEFAULTS) ) {
	use_defaults = FALSE;
	continue;
      }

      if ((cp = index(string,'\t')) != NULL) {
	char *s;
	*cp++ = '\0';
	strcpy(local_file, MAN);
	strcat(local_file, string);
	if ((s = index(cp,'\t')) != NULL) {
	  *s++ = '\0';
	  if (streq(s, SUFFIX))
	    AddNewSection(section_list, path, local_file, cp, MSUFFIX);
	  else if (streq(s, FOLD))
	    AddNewSection(section_list, path, local_file, cp, MFOLD);
	  else if (streq(s, FOLDSUFFIX))
	    AddNewSection(section_list, path, local_file, cp, MFOLDSUFFIX);
	  else
	    AddNewSection(section_list, path, local_file, cp, MNULL);
        } else
	    AddNewSection(section_list, path, local_file, cp, MNULL);
      } else {
	sprintf(local_file, "%s%c", MAN, string[0]);
	AddNewSection(section_list, path, local_file, (string + 1), FALSE );
      }
    }

    fclose(descfile);
  }
  if (use_defaults)
    AddStandardSections(section_list, path);
}

/*	Function Name: AddNewSection
 *	Description: Adds the new section onto the current section list.
 *	Arguments: list - pointer to the section list.
 *                 path - the path to the current manual section.
 *                 file - the file to save.
 *                 label - the current section label.
 *                 flags = 1 - add a suffix
 *			 = 2 - fold to lower case
 *	Returns: none.
 */

void
AddNewSection(list, path, file, label, flags)
SectionList **list;
char * path, * label, * file;
int flags;
{
  SectionList * local_list, * end;
  char full_path[BUFSIZ];

/* Allocate a new list element */

  local_list = (SectionList *) XtMalloc(sizeof(SectionList));

  if (*list != NULL) {
    for ( end = *list ; end->next != NULL ; end = end->next );
    end->next = local_list;
  }
  else 
    *list = local_list;

  local_list->next = NULL;
  local_list->label = StrAlloc(label);
  sprintf(full_path, "%s/%s", path, file);
  local_list->directory = StrAlloc(full_path);
  local_list->flags = flags;
}  

/*	Function Name: AddToCurrentSection
 *	Description: This function gets the names of the manual page
 *                   directories, then closes the directory.
 *	Arguments:  local_manual - a pointer to a manual pages structure.
 *                  path - the path to this directory.
 *	Returns: none.
 */

static void
AddToCurrentSection(local_manual, path)
Manual * local_manual;
char * path;
{
  char temp_path[BUFSIZ];

  ReadCurrentSection(local_manual, path);
  sprintf(temp_path, "%s.%s", path, COMPRESSION_EXTENSION);
  ReadCurrentSection(local_manual, temp_path);
}

/*	Function Name: ReadCurrentSection
 *	Description: Actually does the work of adding entries to the 
 *                   new section
 *	Arguments:  local_manual - a pointer to a manual pages structure.
 *                  path - the path to this directory.
 *                  compressed - Is this a compressed directory?
 *	Returns: TRUE if any entries are found.
 */

static void
ReadCurrentSection(local_manual, path)
Manual * local_manual;
char * path;
{
  DIR * dir;

  register struct dirent *dp;

  register int nentries;
  register int nalloc;
  char full_name[BUFSIZ], *ptr;

  if((dir = opendir(path)) == NULL) {	
#ifdef DEBUG
    sprintf(error_buf,"Can't open directory %s", path);
    PopupWarning(NULL, error_buf);
#endif /* DEBUG */
    return;
  }

/*
 * Remove the compression extension from the path name.
 */

  if ( (ptr = rindex(path, '.')) != NULL) 
    if (streq(ptr + 1, COMPRESSION_EXTENSION)) 
      *ptr = '\0';
  
  nentries = local_manual->nentries;
  nalloc = local_manual->nalloc;

  while( (dp = readdir(dir)) != NULL ) {
    char * name = dp->d_name;
    if (name[0] == '.')
      continue;
#ifndef CRAY
    if (index(name, '.') == NULL)
      continue;
#endif
    if( nentries >= nalloc ) {
      nalloc += ENTRYALLOC;
      local_manual->entries =(char **) XtRealloc((char *)local_manual->entries,
						 nalloc * sizeof(char *));
      local_manual->entries_less_paths =
	(char **) XtRealloc((char *)local_manual->entries_less_paths,
			    nalloc * sizeof(char *));
    }

    sprintf(full_name, "%s/%s", path, name);
/*
 * Remove the compression extension from the entry name.
 */

    if ( (ptr = rindex(full_name, '.')) != NULL) 
      if (streq(ptr + 1, COMPRESSION_EXTENSION)) 
	*ptr = '\0';
    local_manual->entries[nentries] = StrAlloc(full_name);
    local_manual->entries_less_paths[nentries] = 
      rindex(local_manual->entries[nentries], '/');
    if ( local_manual->entries_less_paths[nentries] == NULL )
      PrintError("Internal error while cataloging manual pages.");
    ++ nentries;
  }
  
  local_manual->nentries = nentries;
  local_manual->nalloc = nalloc;

  closedir(dir);
}

/*	Function Name: SortAndRemove
 *	Description: This function sorts all the entry names and
 *                   then removes all the duplicate entries.
 *	Arguments: man - a pointer to the manual structure.
 *                 number - the number of manual sections.
 *	Returns: an improved manual stucure
 */

static void
SortAndRemove(man, number)
Manual *man;
int number;
{
  int i,j;
  char *l1, *l2, **s1;
  
  for ( i = 0; i < number; man++, i++) { /* sort each section */
    register int j = 0;      
    
#ifdef DEBUG
    printf("sorting section %d - %s\n", i, man->blabel);
#endif /* DEBUG */

    s1 = (char **)malloc(man->nentries * sizeof(char *));
    
    /* temporarily remove suffixes of entries, preventing them from */
    /* being used in alpabetic comparison ie sccs-delta.1 vs sccs.1 */
    for (j=0; j<man->nentries; j++)
      if ((s1[j] = rindex(man->entries_less_paths[j], '.')) != NULL)
	*s1[j] = '\0';  

    sortstrs ( man->entries_less_paths, man->nentries, man->entries );

    /* put back suffixes */
    for (j=0; j<man->nentries; j++) 
      if (s1[j] != NULL) *s1[j] = '.';      

    free(s1); 
    
#ifdef DEBUG
    printf("removing from section %d.\n", i);
#endif /* DEBUG */
    
    {
      register int   j, k, nent, nentm1;
      int     j2;
      nent   = man -> nentries;
      nentm1 = nent - 1;
      j = 0;
      l2 = man->entries_less_paths[j++];
      if ( l2 == NULL )
        PrintError("Internal error while removing duplicate manual pages.");
      while ( j < nentm1 )
	{
	  l1 = l2;
	  l2 = man->entries_less_paths[j++];
	  if ( l2 == NULL )
	    PrintError("Internal error while removing duplicate manual pages."
		       );
	  if ( streq(l1,l2) )
	    {
	      j2 = j-1;
	      k  = j2;
	      while ( j < nent )
                {
		  man -> entries_less_paths[k] = man -> entries_less_paths[j];
                man -> entries[k++] = man -> entries[j++];
                }
	      j = j2;
	      -- man -> nentries;
	      -- nent;
	      -- nentm1;
	    }
	}
    }
  }
}

 /*
       *******  Replacement for qsort to keep
       *******  identical entries in order
 
       A somewhat ugly hack of something that was once simpler...
 */
 /*
       Sort an array of pointers to strings, keeping it
       in ascending order by (1) string comparison and
       (2) original entry order in the pointer array.
 
       This is a modified radix exchange algorithm.
 
       In case there's insufficient memory for a temporary copy
       of the pointer array, the original order of identical strings
       isn't preserved.
 */
 
#ifndef       Byte
#define       Byte    unsigned char
#endif
 
#ifndef       reg
#define       reg     register
#endif
 
 
 
 sortstrs ( data, size, otherdata )    /*  Sort an array of string ptrs  */
 
       Byte    *data[];
       int      size;
       Byte    *otherdata[];
 
 {
       Byte   **sp, **ep;
       Byte   **othersp, **otherep;
       int     *origorder;
 
 origorder = (int *) calloc (size, sizeof(int));
 if ( origorder )
    {
    reg int     i;
 
    for ( i=0; i < size; ++i )
       origorder[i] = i;
    }
 
 sp = data;
 ep = &data[size-1];
 othersp = otherdata;
 otherep = &otherdata[size-1];
 if ( origorder )
    {
    sortstrs_block_oo ( sp, ep, 0, 0x80, origorder, &origorder[size-1],
       othersp, otherep );
    free (origorder);
    }
 else
    sortstrs_block ( sp, ep, 0, 0x80, othersp, otherep );
 }
 
 
 
 /*---------------------------------*/
 /*  Sort 1 block of data on 1 bit  */
 /*---------------------------------*/
 
 sortstrs_block ( start, end, offset, mask, otherstart, otherend )
 
       Byte   **start;
       Byte   **end;
       int      offset;
       Byte     mask;
       Byte   **otherstart;
       Byte   **otherend;
 
 {
 reg   Byte   **sp, **ep;
 reg   Byte     m;
 reg   int      off;
 reg   Byte    *t;
 reg   int      curstrlen;
       int      maxstrlen;
       Byte   **othersp, **otherep;
 
 
#define       newstring(ptr) \
 { \
 t = *ptr; \
 curstrlen = 0; \
 while ( *t++ ) ++ curstrlen; \
 if ( curstrlen > maxstrlen ) maxstrlen = curstrlen; \
 t = *ptr; \
 }
 
 
 maxstrlen = 0;
 sp  = start;
 ep  = end;
 off = offset;
 m   = mask;
 othersp = otherstart;
 otherep = otherend;
 
 while (1)
     {
     newstring(sp)
     while (((sp != ep) && ((curstrlen < off) || ((t[off] & m) == 0))))
       {
       ++ sp;
       ++ othersp;
       newstring(sp)
       }
     if ( sp == ep )
       break;
 
     newstring(ep);
     while (((sp != ep) && (curstrlen >= off) && ((t[off] & m) != 0)))
       {
       -- ep;
       -- otherep;
       newstring(ep)
       }
     if ( sp == ep )
       break;
 
     t = *sp;
     *sp = *ep;
     *ep = t;
 
     t      = *othersp;
     *othersp = *otherep;
     *otherep = t;
     }
 
 t = *sp;
 if ((curstrlen < off) || ((t[off] & m) == 0))
    {
    if ( ep != end )
       {
       ++ ep;
       ++ otherep;
       }
    }
 else
    {
    if ( sp != start )
       {
       -- sp;
       -- othersp;
       }
    }
 
 m >>= 1;
 if ( m == 0 )
    {
    m = 0x80;
    if ( ++off >= maxstrlen )
       return;
    }
 
 
 if ( sp != start )
    sortstrs_block ( start, sp, off, m, otherstart, othersp );
 if ( ep != end )
    sortstrs_block ( ep, end, off, m, otherep, otherend );
 }
 
 
 
 /*-----------------------------------------------------------------*/
 /*  Sort 1 block of data on 1 bit; check for out-of-order entries  */
 /*-----------------------------------------------------------------*/
 
 sortstrs_block_oo ( start, end, offset, mask, ostart, oend,
                        otherstart, otherend )
 
       Byte   **start;
       Byte   **end;
       int      offset;
       Byte     mask;
       int     *ostart;
       int     *oend;
       Byte   **otherstart;
       Byte   **otherend;
 
 {
 reg   Byte   **sp, **ep;
 reg   int     *osp, *oep;
 reg   Byte     m;
 reg   int      off;
 reg   Byte    *t;
 reg   int      u;
 reg   int      curstrlen;
       int      maxstrlen;
       Byte   **othersp, **otherep;
 
 
#define       newstring(ptr) \
 { \
 t = *ptr; \
 curstrlen = 0; \
 while ( *t++ ) ++ curstrlen; \
 if ( curstrlen > maxstrlen ) maxstrlen = curstrlen; \
 t = *ptr; \
 }
 
 
 maxstrlen = 0;
 sp  = start;
 ep  = end;
 osp = ostart;
 oep = oend;
 off = offset;
 m   = mask;
 othersp = otherstart;
 otherep = otherend;
 
 while (1)
     {
     newstring(sp)
     while (((sp != ep) && ((curstrlen < off) || ((t[off] & m) == 0))))
       {
       ++ sp;
       ++ osp;
       ++ othersp;
       newstring(sp)
       }
     if ( sp == ep )
       break;
 
     newstring(ep);
     while (((sp != ep) && (curstrlen >= off) && ((t[off] & m) != 0)))
       {
       -- ep;
       -- oep;
       -- otherep;
       newstring(ep)
       }
     if ( sp == ep )
       break;
 
     t   = *sp;
     *sp = *ep;
     *ep = t;
 
     t      = *othersp;
     *othersp = *otherep;
     *otherep = t;
 
     u    = *osp;
     *osp = *oep;
     *oep = u;
     }
 
 t = *sp;
 if ((curstrlen < off) || ((t[off] & m) == 0))
    {
    if ( ep != end )
       {
       ++ ep;
       ++ oep;
       ++ otherep;
       }
    }
 else
    {
    if ( sp != start )
       {
       -- sp;
       -- osp;
       -- othersp;
       }
    }
 
 m >>= 1;
 if ( m == 0 )
    {
    m = 0x80;
    if ( ++off >= maxstrlen )  /*  Finished sorting block of strings:    */
       {                               /*  Restore duplicates to
riginal order  */
       reg Byte **cp;
       reg int *ocp;
         Byte **othercp;
 
 
       if ( sp != start )
        {
        cp  = start;
        ocp = ostart;
        othercp = otherstart;
        while ( cp != sp )
           {
           if ( *ocp > *(ocp+1) )
               {
               t       = *(cp+1);
               *(cp+1) = *cp;
               *cp     = t;
 
               t               = *(othercp+1);
               *(othercp+1)    = *othercp;
               *othercp        = t;
 
               u        = *(ocp+1);
               *(ocp+1) = *ocp;
               *ocp     = u;
 
               if ( cp != start )
                  {
                  -- cp;
                  -- ocp;
                  -- othercp;
                  continue;
                  }
               }
           ++ cp;
           ++ ocp;
           ++ othercp;
           }
        }
       if ( ep != end )
        {
        cp  = ep;
        ocp = oep;
        othercp = otherep;
        while ( cp != end )
           {
           if ( *ocp > *(ocp+1) )
               {
               t       = *(cp+1);
               *(cp+1) = *cp;
               *cp     = t;
 
               t               = *(othercp+1);
               *(othercp+1)    = *othercp;
               *othercp        = t;
 
               u        = *(ocp+1);
               *(ocp+1) = *ocp;
               *ocp     = u;
 
               if ( cp != ep )
                  {
                  -- cp;
                  -- ocp;
                  -- othercp;
                  continue;
                  }
               }
           ++ cp;
           ++ ocp;
           ++ othercp;
           }
        }
       return;
       }
    }
 
 
 if ( sp != start )
    sortstrs_block_oo ( start, sp, off, m, ostart, osp, otherstart, othersp );
 if ( ep != end )
    sortstrs_block_oo ( ep, end, off, m, oep, oend, otherep, otherend );
 }


/*	Function Name: InitManual
 *	Description: Initializes this manual section.
 *	Arguments: l_manual - local copy of the manual structure.
 *                 label - the button label for this section.
 *	Returns: none.
 */

static void
InitManual(l_manual, label)
Manual * l_manual;
char * label;
{
  bzero( l_manual, sizeof(Manual) );	        /* clear it. */
  l_manual->blabel = label;	                /* set label. */
}
  
#if defined(DEBUG)

/*	Function Name: DumpManual
 *	Description: Debugging function that dumps the entire manual page
 *                   structure.
 *	Arguments: number - the number of sections.
 *	Returns: none.
 */

DumpManual(number)
{
  register int i,j;
  
  for ( i = 0; i < number; i++) {
    printf("label: %s\n", manual[i].blabel);
    for (j = 0; j < manual[i].nentries; j++) 
      printf("%s\n", manual[i].entries[j]);
  }
}

#endif /* DEBUG */

#ifdef MANCONF

/*	Function Name: ReadManConfig
 *	Description: Reads man.conf file used by BSD 4.4
 *      Argument: manpath - char array to return path in.
 *	Returns: TRUE if read was successful.
 */

Bool
ReadManConfig(manpath)

char	manpath[];

{
  FILE	*fp;
  char	line[BUFSIZ];
  char	*path;
  Bool  firstpath = TRUE;

  if (!(fp = fopen(MANCONF, "r")))
    return(FALSE);

  while (fgets(line, sizeof(line), fp)) {
    path = strtok(line, " \t\n");
    if (!path || *path == '#' || strcmp(path, "_default"))
      continue;
    while (path = strtok((char *)NULL, " \t\n")) {
      if (firstpath) {
        strcpy(manpath, path);
        firstpath = FALSE;
      }
      else {
        strcat(manpath, ":");
        strcat(manpath, path);
      }
    }
  }
  fclose(fp);
  return(!firstpath);
}
#endif /* MANCONF */
