/****************************************************************************/
/* Module FmAwActions                                                       */
/*                                                                          */
/* Action procedures for widgets in the application window                  */
/****************************************************************************/

#include <string.h>

#include <X11/Intrinsic.h>

#include "Am.h"

/****************************************************************************/
/*                        PRIVATE FUNCTIONS                                 */
/****************************************************************************/

int findAppWidget(Widget w)
{
  register int i;

  for (i=0; i<aw.n_apps; i++)
    if (aw.apps[i].toggle == w)
      return i;
  return -1;
}

/****************************************************************************/
/*                         PUBLIC FUNCTIONS                                 */
/****************************************************************************/

void appMaybeHighlight(Widget w, XEvent *event, String *params, 
		       Cardinal *num_params)
{
  if (!dragging)
    return;
  
  XtCallActionProc(w, "highlight", event, NULL, 0);
}

/*****************************************************************************/

void runApp(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
  register int i;
  char **argv;

  i = findAppWidget(w);
  argv = makeArgv(i);
  executeApplication(user.shell, argv);
  freeArgv(argv);
}

/*****************************************************************************/

void appEndMove(int i)
{
  char **argv;

  argv = makeArgv(i);
  argv = expandArgv(argv);
  executeApplication(user.shell, argv);
  freeArgv(argv);
}

/*****************************************************************************/

void appEndMoveInBox(void)
{
  register int i;
  char path[MAXAPPSTRINGLEN];

  if (move_info.type != Executable) {
    error("You can only drag executables","into the application manager");
    return;
  }

  strcpy(path, move_info.fw->directory);

  for (i=0; !move_info.fw->files[i]->selected && i<move_info.fw->n_files; i++);

  strcat(path, "/");
  strcat(path, move_info.fw->files[i]->name);
  installApplication(move_info.fw->files[i]->name, path, "");

  updateApplicationDisplay();
}





