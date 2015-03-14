/****************************************************************************/
/* Module FmFwActions                                                       */
/*                                                                          */
/* Action procedures for widgets in a file window                           */
/****************************************************************************/

#include <stdio.h>
#include <sys/param.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Toggle.h>

#include "Am.h"
#include "Fm.h"

/****************************************************************************/
/*                           PUBLIC DATA                                    */
/****************************************************************************/

MoveInfo move_info;
Boolean dragging = False;

/****************************************************************************/
/*                           STATIC DATA                                    */
/****************************************************************************/

static char from[MAXPATHLEN], to[MAXPATHLEN];
static int fromi, toi;

/****************************************************************************/
/*                        PRIVATE FUNCTIONS                                 */
/****************************************************************************/

static int findWidget(Widget w, FileWindowRec **fw_ret)
{
  register int i;
  register FileWindowRec *fw;

  for (fw = file_windows; fw; fw = fw->next) {
    if (fw->icon_box == w) {
      *fw_ret = fw;
      return -1;
    }
    for (i = 0; i < fw->n_files; i++)
      if (fw->files[i]->icon.toggle == w) {
	*fw_ret = fw;
	return i;
      }
  }
  *fw_ret = NULL;
  return 0;
/*  error("Internal error:", "Can't find destination widget");
  exit(1);*/
}

/*----------------------------------------------------------------------------*/

static void copyFiles()
{
  register int i;
  int src, dst, n;
  char err[0xff];
  char buf[BUFSIZ];

  for (i=0; i<move_info.fw->n_files; i++)
    if (move_info.fw->files[i]->selected) {
      if (S_ISDIR(move_info.fw->files[i]->stats.st_mode)) {
	error("I can't copy directories","");
	continue;
      }

      sprintf(err, "Error copying %s:", move_info.fw->files[i]->name);

      strcpy(from+fromi, move_info.fw->files[i]->name);
      strcpy(to+toi, move_info.fw->files[i]->name);

      if ( (src = open(from, O_RDONLY)) == -1) {
	sysError(err);
	continue;
      }
      
      if ( (dst = open(to, O_WRONLY|O_CREAT|O_EXCL, 
		       move_info.fw->files[i]->stats.st_mode)) == -1) {
	if (errno == EEXIST) {
	  char s[0xff];
	  sprintf(s, "File %s already exists at destination", 
		  move_info.fw->files[i]->name);
	  if (!confirm("Name conflict during copy", s, 
		       "Overwrite it or not?"))
	    continue;
	  else if ((dst = open(to, O_WRONLY|O_CREAT|O_TRUNC, 
			       move_info.fw->files[i]->stats.st_mode)) == -1) {
	    sysError(err);
	    close(src);
	    continue;
	  }
	}
	else {
	  sysError(err);
	  close(src);
	  continue;
	}
      }
	
      while ( (n = read(src, buf, BUFSIZ)) != 0)
	if ( n == -1 || write(dst, buf, n) != n ) {
	  sysError(err);
	  close(src);
	  close(dst);
	  continue;
	}

      if (close(src) == -1)
	sysError(err);

      if (close(dst) == -1)
	sysError(err);
    }

  intUpdate();
}

/****************************************************************************/
/*                         PUBLIC FUNCTIONS                                 */
/****************************************************************************/

void maybeToggle(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
  register int i;
  FileWindowRec *fw;

  XtCallActionProc(w, "toggle", event, NULL, 0);

  i = findWidget(w, &fw);

  if (fw->files[i]->selected) {
    fw->files[i]->selected = False;
    fw->n_selections--;
  }
  else {
    fw->files[i]->selected = True;
    fw->n_selections++;
  }
  updateMenus(fw);
}

/*----------------------------------------------------------------------------*/

void maybeHighlight(Widget w, XEvent *event, String *params, 
		     Cardinal *num_params)
{
  if (!dragging)
    return;

  XtCallActionProc(w, "highlight", event, NULL, 0);
}

/*----------------------------------------------------------------------------*/

void fileOpenDir(Widget w, XEvent *event, String *params,Cardinal *num_params)
{
  FileWindowRec *fw;
  register int i;
  char path[MAXPATHLEN];

  i = findWidget(w, &fw);
  if (chdir(fw->directory) || chdir(fw->files[i]->name))
    sysError("Can't change directory:");
  else if (!getwd(path))
    error("System error:", path);
  else {
    strcpy(fw->directory, path);
    updateFileDisplay(fw);
  }
}

/*----------------------------------------------------------------------------*/

void setupMoveCopy(FileWindowRec *fw, int i)
{
  strcpy(from, move_info.fw->directory);
  strcpy(to, fw->directory);
  toi = strlen(to);
  fromi = strlen(from);

  if (i != -1 && S_ISDIR(fw->files[i]->stats.st_mode)
      && strcmp(fw->files[i]->name, ".")) {
    if (to[toi-1] != '/') {
      to[toi++] = '/';
      to[toi] = '\0';
    }
    strcat(to, fw->files[i]->name);
    toi += strlen(fw->files[i]->name);
  }
    
  clearUpdateMarks();
  markForUpdate(from);
  markForUpdate(to);

  if (from[fromi-1] != '/') {
    from[fromi++] = '/';
    from[fromi] = '\0';
  }
  if (to[toi-1] != '/') {
    to[toi++] = '/';
    to[toi] = '\0';
  }
}  

/*----------------------------------------------------------------------------*/

static void fileEndDrag(FileWindowRec *fw, int i)
{
  setupMoveCopy(fw, i);

  if (access(to, W_OK)) {
    error("No write access to this directory","");
    return;
  }

  if (resources.confirm_moves) {
    char s1[0xff], s2[0xff], s3[0xff];
    sprintf(s1, "Moving %d item%c", move_info.fw->n_selections,
	    move_info.fw->n_selections > 1 ? 's' : ' ');
    sprintf(s2, "from: %s", from);
    sprintf(s3, "to: %s", to);
    if (!confirm(s1, s2, s3))
      return;
  }

  for (i=0; i < move_info.fw->n_files; i++)

    if (move_info.fw->files[i]->selected) {
      strcpy(from+fromi, move_info.fw->files[i]->name);
      strcpy(to+toi, move_info.fw->files[i]->name);
      if (rename(from,to)) {
	char s[0xff];
	sprintf(s, "Can't move %s:", move_info.fw->files[i]->name);
	sysError(s);
      }
    }

  intUpdate();
}

/*----------------------------------------------------------------------------*/

static void fileEndCopy(FileWindowRec *fw, int i)
{
  setupMoveCopy(fw, i);
  
  if (access(to, W_OK)) {
    error("No write access to this directory","");
    return;
  }
  
  if (resources.confirm_copies) {
    char s1[0xff], s2[0xff], s3[0xff];
    sprintf(s1, "Copying %d item%c", move_info.fw->n_selections,
	    move_info.fw->n_selections > 1 ? 's' : ' ');
    sprintf(s2, "from: %s", from);
    sprintf(s3, "to %s", to);
    if (confirm(s1, s2, s3))
      copyFiles();
  }
  else 
    copyFiles();
}

/*----------------------------------------------------------------------------*/

static void fileExecEndDrag(FileWindowRec *fw, int i)
{
  char path[MAXPATHLEN];
  char **argv;

  argv = (char **) XtMalloc(2 * sizeof(char *));
  argv[0] = XtNewString(fw->files[i]->name);
  argv[1] = NULL;

  strcpy(path, fw->directory);
  if (path[strlen(path)-1] != '/')
    strcat(path, "/");
  strcat(path, fw->files[i]->name);

  argv = expandArgv(argv);
  executeApplication(path, argv);
  freeArgv(argv);
}

/*----------------------------------------------------------------------------*/

/* perform a click-drag, returning the widget that the user released the 
   button on */

static Widget drag(Widget w, int button)
{
  Cursor c;
  XEvent e;
  int i;

  i =  findWidget(w, &move_info.fw);
  if (!move_info.fw) {
    error("internal error:","widget not found in drag");
    return NULL;;
  }
  
  if (i != -1) {
    if (!move_info.fw->files[i]->selected) {
      move_info.fw->n_selections++;
      move_info.fw->files[i]->selected = True;
      XtVaSetValues(move_info.fw->files[i]->icon.toggle, XtNstate, (XtArgVal) 
		    True, NULL );
      updateMenus(move_info.fw);
    }
  }
  else if (move_info.fw->n_selections == 0)
    return NULL;
  
  if (move_info.fw->n_selections == 1) {
    for (i=0; !move_info.fw->files[i]->selected && i<move_info.fw->n_files;
	 i++);
    if (S_ISDIR(move_info.fw->files[i]->stats.st_mode)) {
      move_info.type = Directory;
      c = curs[DIR_CUR];
    }
    else if (move_info.fw->files[i]->stats.st_mode & 
	     (S_IXUSR | S_IXGRP | S_IXOTH)) {
      move_info.type = Executable;
      c = curs[COMMAND_CUR];
    }
    else {
      move_info.type = SingleFile;
      c = curs[FILE_CUR];
    }
  }
  else {
    move_info.type = MultipleFiles;
    c = curs[FILES_CUR];
  }
  
  /* Grab that pointer thing, yeah...., still doesn't work
     If i do it with owner events on, then *all* events are still on,
     you can still use buttons etc. If owner events is off, then I have no
     idea where the button was released, and enter/leave events are only
     reported for the grabbing widget so no highlights are possible.
     Uggh. */
  
  XtGrabPointer(w, True,
		EnterWindowMask | LeaveWindowMask | ButtonReleaseMask, 
		GrabModeAsync, GrabModeAsync, None, c, CurrentTime);
  
  move_info.dragged_from = w;
  dragging = True;
  
  for (;;) {
    XtAppNextEvent(app_context, &e);
    switch (e.type) {
    case ButtonPress:        /* Ignore button presses */
      continue;
    case ButtonRelease:      /* Ignore button releases except 2 */
      if (e.xbutton.button != button)
	continue;
      break;
    default:
      XtDispatchEvent(&e);
      continue;
    }
    break;
  }
  
  XtUngrabPointer(aw.shell,CurrentTime);
  dragging = False;
  
  return XtWindowToWidget(XtDisplay(w),e.xbutton.window);
}

/*----------------------------------------------------------------------------*/

void fileBeginCopy(Widget w, XEvent *event, String *params, 
		   Cardinal *num_params)
{
  int i;
  FileWindowRec *fw;

  if (!(w = drag(w,3)))
      return;

  if (move_info.dragged_from == w)
    return;

  i = findWidget(w,&fw);
  if (!fw) {
    if (w == aw.icon_box)
      appEndMoveInBox();
    else if ((i = findAppWidget(w)) != -1)
      appEndMove(i);
    else {
      error("Internal error:","widget not found in fileBeginCopy");
      return;
    }
  }

  /* if dragged onto a window or a directory, do the move */
  else if (i == -1 || S_ISDIR(fw->files[i]->stats.st_mode))
    fileEndCopy(fw,i);
  
  /* If dragged onto an executable, run the program */
  else if (fw->files[i]->stats.st_mode & (S_IXUSR | S_IXGRP | S_IXOTH))
    fileExecEndDrag(fw,i);
  
  /* otherwise, it must be a normal file so just do the move */
  else 
    fileEndCopy(fw,i);
}

/*----------------------------------------------------------------------------*/

void fileBeginDrag(Widget w, XEvent *event, String *params, 
		   Cardinal *num_params)
{
  register int i;
  FileWindowRec *fw;

  if (!(w = drag(w,2)))
      return;
  
  if (move_info.dragged_from == w)
    return;
  
  i = findWidget(w,&fw);
  if (!fw) {
    if (w == aw.icon_box)
      appEndMoveInBox();
    else if ((i = findAppWidget(w)) != -1)
      appEndMove(i);
    else {
      error("Internal error:","widget not found in fileBeginDrag");
      return;
    }
  }
  
  /* if dragged onto a window or a directory, do the move */
  else if (i == -1 || S_ISDIR(fw->files[i]->stats.st_mode))
    fileEndDrag(fw,i);
  
  /* If dragged onto an executable, run the program */
  else if (fw->files[i]->stats.st_mode & (S_IXUSR | S_IXGRP | S_IXOTH))
    fileExecEndDrag(fw,i);
  
  /* otherwise, it must be a normal file so just do the move */
  else 
    fileEndDrag(fw,i);
}
  
/*----------------------------------------------------------------------------*/

void fileOpenFile(Widget w, XEvent *event, String *params, 
		  Cardinal *num_params)
{
  register int i;
  char path[MAXPATHLEN];
  char **argv;
  FileWindowRec *fw;

  i = findWidget(w, &fw);

  argv = (char **) XtMalloc(2 * sizeof(char *));
  argv[0] = XtNewString(fw->files[i]->name);
  argv[1] = NULL;

  strcpy(path, fw->directory);
  if (path[strlen(path)-1] != '/')
    strcat(path, "/");
  strcat(path, fw->files[i]->name);

  executeApplication(path, argv);
  
  XTFREE(argv[0]);
  XTFREE(argv);
}

/******************************************************************************/

void renameObject(Widget w, XEvent *event, String *params, 
		     Cardinal *num_params)
{
  FileWindowRec *fw;
  int i;

  if ((i = findWidget(w,&fw)) == -1)
    error("Internal error: ", "renameObject");
  else 
    renamePopup(fw,i);
}
