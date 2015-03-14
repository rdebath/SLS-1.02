/*****************************************************************************/
/* Module FmDirs.c                                                           */
/*                                                                           */
/* functions for manipulating directory lists, and some other utilities      */
/* related to the file system.                                               */
/*****************************************************************************/

#include <memory.h>
#include <sys/types.h>
#include <sys/param.h>
#include <string.h>

#include <X11/Intrinsic.h>

#include "Fm.h"

/*****************************************************************************/
/*                          STATIC DATA                                      */
/*****************************************************************************/

static SortType sort_type;
static Boolean dirs_first;

/*****************************************************************************/
/*                         PRIVATE FUNCTIONS                                 */
/*****************************************************************************/

static int comp(FileRec **fr1, FileRec **fr2)
{
  FileRec *fl1 = *fr1, *fl2 = *fr2;

  if (dirs_first) {
    if (S_ISDIR(fl1->stats.st_mode)) {
      if (!S_ISDIR(fl2->stats.st_mode))
	return -1;
    }
    else if (S_ISDIR(fl2->stats.st_mode))
      return 1;
  }
	
  switch (sort_type) {
  case SortByName:
    return strcmp(fl1->name, fl2->name);
  case SortBySize:
    return (int)(fl2->stats.st_size - fl1->stats.st_size);
  case SortByMTime:
    return (int)(fl2->stats.st_mtime - fl1->stats.st_mtime);
  }

  return 0;
}

/*****************************************************************************/
/*                          PUBLIC FUNCTIONS                                 */
/*****************************************************************************/

/*****************************************************************************/
/* Read a directory, and create a FileList structure                         */
/*****************************************************************************/

int readDirectory(FileList *fl_return, int *n_return, String path)
{
  FileList fl = NULL;
  DIR *dir;
  struct dirent *entry;
  register int n, m;

  if (chdir(path))
    goto error2;

  if (!(dir = opendir(".")))
    goto error2;

  for(n = 0; entry = readdir(dir); n++) {
    fl = (FileRec **) XTREALLOC(fl, (n+1)*sizeof(FileRec *));
    fl[n] = (FileRec *) XtMalloc(sizeof(FileRec));
    strcpy(fl[n]->name, entry->d_name);
    if (lstat(entry->d_name, &(fl[n]->stats)))
      goto error1;
    if (S_ISLNK(fl[n]->stats.st_mode)) {
      fl[n]->sym_link = True;
      if (stat(entry->d_name, &(fl[n]->stats)))
	goto error1;
    }
    else
      fl[n]->sym_link = False;
    fl[n]->selected = False;
  }

  if (closedir(dir))
    goto error1;

  *fl_return = fl;
  *n_return = n;
  return 0;

 error1:
  for(m = 0; m <= n; m++)
    XTFREE(fl[m]);
  XTFREE(fl);

 error2:
  *fl_return = NULL;
  *n_return = 0;
  return -1;
}


/*****************************************************************************/
/* Remove either files or directories from a FileList                        */
/*****************************************************************************/

void filterDirectory(FileList *fl_return, int *n_return, FilterType type)
{
  FileList fl = NULL, oldfl = *fl_return;
  int n = 0, m = 0;

  for (; m < *n_return; m++)
    if (S_ISDIR(oldfl[m]->stats.st_mode) && type == Directories ||
	!S_ISDIR(oldfl[m]->stats.st_mode) && type == Files ||
	(type == All && strcmp(oldfl[m]->name,".") &&
	 strcmp(oldfl[m]->name,".."))) {
      fl = (FileList) XTREALLOC(fl, (n+1)*sizeof(FileRec *));
      fl[n] = oldfl[m];
      n++;
    }
    else
      XTFREE(oldfl[m]);
  
  XTFREE(oldfl);
  
  *fl_return = fl;
  *n_return = n;
}  


/*****************************************************************************/
/* Sort a directory according to the sort type and dfirst flag               */
/*****************************************************************************/

void sortDirectory(FileList fl, int n, SortType type, Boolean dfirst)
{
  sort_type = type;
  dirs_first = dfirst;
  qsort(fl, n, sizeof(FileRec *), comp);
}


/*****************************************************************************/
/* Check permission for an operation, equivalent to UNIX access()            */
/*****************************************************************************/

int permission(FileRec *file, int perms)
{
  int mode = file->stats.st_mode;
  int result = 0;

  if (user.uid == file->stats.st_uid) {
    if (mode & S_IRUSR)
      result |= P_READ;
    if (mode & S_IWUSR)
      result |= P_WRITE;
    if (mode & S_IXUSR)
      result |= P_EXECUTE;
  } 

  else if (user.gid == file->stats.st_gid) {
    if (mode & S_IRGRP)
      result |= P_READ;
    if (mode & S_IWGRP)
      result |= P_WRITE;
    if (mode & S_IXGRP)
      result |= P_EXECUTE;
  } 

  else {
    if (mode & S_IROTH)
      result |= P_READ;
    if (mode & S_IWOTH)
      result |= P_WRITE;
    if (mode & S_IXOTH)
      result |= P_EXECUTE;
  } 

  return (result & perms) == perms;
}

/*****************************************************************************/
/* Filename substitution                                                     */
/*****************************************************************************/

void fnSub(char *fn)
{
  register char *s = fn;
  int l = strlen(user.home);

  if (user.home)
    for (;*s ;s++)
      if (*s == '~') {
	strcpy(s+l, s+1);
	memcpy(s, user.home, l);
	s += l;
      }
}	

char  *fnSubA(char *fn)
{
  register int i;
  int l = strlen(user.home);

  if (user.home)
    for (i=0; fn[i]; i++)
      if (fn[i] == '~') {
	fn = XTREALLOC(fn, (strlen(fn) + l + 1) * sizeof(char));
	strcpy(fn+l, fn+1);
	memcpy(fn+i, user.home, l);
	i += l;
      }
  return fn;
}

void makePermissionsString(char *s, int perms)
{
  s[0] = perms & S_IRUSR ? 'r' : '-'; 
  s[1] = perms & S_IWUSR ? 'w' : '-'; 
  s[2] = perms & S_IXUSR ? 'x' : '-'; 

  s[3] = perms & S_IRGRP ? 'r' : '-'; 
  s[4] = perms & S_IWGRP ? 'w' : '-'; 
  s[5] = perms & S_IXGRP ? 'x' : '-'; 

  s[6] = perms & S_IROTH ? 'r' : '-'; 
  s[7] = perms & S_IWOTH ? 'w' : '-'; 
  s[8] = perms & S_IXOTH ? 'x' : '-'; 
  
  s[9] = '\0';
}
