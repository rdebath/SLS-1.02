/****************************************************************************/
/* Module FmExec                                                            */
/*                                                                          */
/* Procedures for executing files                                           */
/****************************************************************************/

#include <memory.h>
#include <string.h>

#include <X11/Intrinsic.h>

#include "Am.h"
#include "Fm.h"


/*****************************************************************************/
/*                       PUBLIC FUNCTIONS                                    */
/*****************************************************************************/

char **makeArgv(int i)
{
  char **argv;

  argv = (char **) XtMalloc(4 * sizeof(char *));
  argv[0] = XtNewString(user.shell);
  argv[1] = XtNewString("-c");
  argv[2] = XtNewString(aw.apps[i].cmd);
  argv[3] = NULL;

  return argv;
}

char **expandArgv(char **argv)
{
  int i, j;
  FileList files = move_info.fw->files;
  char path[MAXPATHLEN], *s;

  for (i=0; argv[i]; i++);
  i++;

  strcpy(path, move_info.fw->directory);
  strcat(path, "/");
  s = path + strlen(path);

  for (j=0; j<move_info.fw->n_files; j++) {
    if (files[j]->selected) {
      argv = (char**) XTREALLOC(argv, ++i * sizeof(char *));
      strcpy(s,files[j]->name);
      argv[i-2] = XtNewString(path);
    }
  }

  argv[i-1] = NULL;
  return argv;
}
/*****************************************************************************/

void freeArgv(char **argv)
{
  register int j;

  for (j=0; argv[j]; j++)
    XTFREE(argv[j]);
  XTFREE(argv);
}

/*****************************************************************************/

void executeApplication(char *path, char **argv)
{
  int pid;

  if ((pid = fork()) == -1)
    sysError("Can't fork:");
  else {
    if (!pid) {
      execvp(path, argv);
      perror("Exec failed");
      exit(1);
    }
  }    
}



