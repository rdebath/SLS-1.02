
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
 *	comp_error.c -- Error message routines
 *
 */

#include <stdarg.h>
#include "compiler.h"

extern char *string_table;
extern short term_names;

void warning(const char *fmt, ...)
{
    va_list argp;

    va_start(argp,fmt);
    fprintf (stderr, "compile: Warning: near line %d: ", curr_line);
    fprintf (stderr, "terminal '%s', ", string_table+term_names);
    fprintf (stderr, fmt, argp);
    fprintf (stderr, "\n");
    va_end(argp);
}


void err_abort(const char *fmt, ...)
{
    va_list argp;

    va_start(argp,fmt);
    fprintf (stderr, "compile: Line %d: ", curr_line);
    fprintf (stderr, "terminal '%s', ", string_table+term_names);
    fprintf (stderr, fmt, argp);
    fprintf (stderr, "\n");
    va_end(argp);
    exit(1);
}


void syserr_abort(const char *fmt, ...)
{
    va_list argp;

    va_start(argp,fmt);
    fprintf (stderr, "PROGRAM ERROR: Line %d: ", curr_line);
    fprintf (stderr, "terminal '%s', ", string_table+term_names);
    fprintf (stderr, fmt, argp);
    fprintf (stderr, "\n");
    va_end(argp);
    abort();
}
