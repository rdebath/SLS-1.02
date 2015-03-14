/****************************************************************************/
/* Module FmDelete                                                          */
/*                                                                          */
/* Functions for implementing the delete operation                          */
/****************************************************************************/

#include <X11/Intrinsic.h>

#include "Fm.h"

/****************************************************************************/
/*                         PUBLIC FUNCTIONS                                 */
/****************************************************************************/

void deleteItems(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  char error_string[0xff];
  register int i;
  char *s, path[MAXPATHLEN];

  clearUpdateMarks();
  markForUpdate(fw->directory);

  strcpy(path, fw->directory);
  s = path+strlen(path);
  if (*(s-1) != '/') {
    *s++ = '/';
    *s = '\0';
  }

  if (resources.confirm_deletes) {
    sprintf(error_string, "Deleting %d item%c from", fw->n_selections,
	    fw->n_selections > 1 ? 's' : ' ' );
    if (!confirm(error_string, fw->directory, ""))
      return;
  }

  chdir(fw->directory);

    for (i=0; i < fw->n_files; i++)
      if (fw->files[i]->selected) {
	if (S_ISDIR(fw->files[i]->stats.st_mode)) {
	  if (rmdir(fw->files[i]->name)) {
	    sprintf(error_string, "Can't remove directory %s:", 
		    fw->files[i]->name);
	    sysError(error_string);
	  }
	  else {
	    strcpy(s, fw->files[i]->name);
	    markForUpdate(path);
	  }
	}

	else if (unlink(fw->files[i]->name)) {
	  sprintf(error_string, "Can't remove file %s:", 
		  fw->files[i]->name);
	  sysError(error_string);
	}
      }
  
  intUpdate();
}

/****************************************************************************/
