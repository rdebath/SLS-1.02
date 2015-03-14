/* tools.c */

/* standard routines for use in tracker. Used to be in main.c
 */

/* $Id: tools.c,v 3.1 1992/11/19 20:44:47 espie Exp espie $
 * $Log: tools.c,v $
 * Revision 3.1  1992/11/19  20:44:47  espie
 * Protracker commands.
 *
 * Revision 3.0  1992/11/18  16:08:05  espie
 * New release.
 */
     

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
     
#include "defs.h"
#include "extern.h"
     
LOCAL char *id = "$Id: tools.c,v 3.1 1992/11/19 20:44:47 espie Exp espie $";


/* v = read_env(name, default): reads the scalar value v
 * in the environment, supplies a defaults.
 */
int read_env(name, def)
char *name;
int def;
    {
    char *var;
    int value;

    var = getenv(name);
    if (!var)
        return def;
    if (sscanf(var, "%d", &value) == 1)
        return value;
    else
        return def;
    }

