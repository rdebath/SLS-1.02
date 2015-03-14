#include <stdio.h>
#include <stdlib.h>
#include "popen.h"
#include <string.h>
#include "config.h"

Prototype FILE *popen(char *command, char *mode);
Prototype int pclose(FILE * current);

#ifndef AMIGA
static char template[] = "piXXXXXX";
#else
static void mkcmd(char *, char *, char, char *);
extern char *mktemp(char *);
#endif

#ifdef _DCC
#define _NFILE FOPEN_MAX
#endif

typedef enum { unopened = 0, reading, writing } pipemode;
static
struct {
    char *command;
    char *name;
    pipemode pmode;
} pipes[_NFILE];

FILE *
popen( char *command, char *mode ) {
    FILE *current;
    char *name;
    int cur;
    pipemode curmode;
#ifdef AMIGA
char *template = "T:piXXXXXX";
#endif
    /*
    ** decide on mode.
    */
    if(strcmp(mode,"r") == 0)
        curmode = reading;
    else if(strcmp(mode,"w") == 0)
        curmode = writing;
    else
        return NULL;
    /*
    ** get a name to use.
    */
#ifdef AMIGA
    name = mktemp(template);
#else
    if((name = tempnam(".","pip"))==NULL)
        return NULL;
#endif
    /*
    ** If we're reading, just call system to get a file filled with
    ** output.
    */
    if(curmode == reading) {
        char cmd[256];
#ifdef AMIGA
        mkcmd(cmd, command, '>', name);
#else
        sprintf(cmd,"%s > %s",command,name);
#endif
        system(cmd);
        if((current = fopen(name,"r")) == NULL)
            return NULL;
    } else {
        if((current = fopen(name,"w")) == NULL)
            return NULL;
    }
    cur = fileno(current);
    pipes[cur].name = strdup(name);
    pipes[cur].pmode = curmode;
    pipes[cur].command = strdup(command);
    return current;
}

int
pclose( FILE * current) {
    int cur = fileno(current),rval;
    /*
    ** check for an open file.
    */
    if(pipes[cur].pmode == unopened)
        return -1;
    if(pipes[cur].pmode == reading) {
        /*
        ** input pipes are just files we're done with.
        */
        rval = fclose(current);
        unlink(pipes[cur].name);
    } else {
        /*
        ** output pipes are temporary files we have
        ** to cram down the throats of programs.
        */
        char command[256];
        fclose(current);
#ifdef AMIGA
        mkcmd(command, pipes[cur].command, '<', pipes[cur].name);
#else
        sprintf(command,"%s < %s",pipes[cur].command,pipes[cur].name);
#endif
        rval = system(command);
        unlink(pipes[cur].name);
    }
    /*
    ** clean up current pipe.
    */
    pipes[cur].pmode = unopened;
    free(pipes[cur].name);
    free(pipes[cur].command);
    return rval;
}
#ifdef AMIGA
/* ^%#@%! Amiga CLI requires I/O redirection immediately after command name.
   Assume command name ends at first space and place redirection there */

static void
mkcmd(char *cmd, char *command, char op, char *name)
{
  while (*command == ' ') {
    command++;
  }
  while (*command != ' ' && *command != '\000') {
    *cmd++ = *command++;
  }
  *cmd++ = ' ';
  *cmd++ = op;
  strcpy(cmd, name);
  cmd += strlen(name);
  while (*command != '\000') {
    *cmd++ = *command++;
  }
  *cmd = '\000';
  return;
}
#endif
