/*
 * This file is part of the Minicom Communications Program,
 * written by Miquel van Smoorenburg 1991/1992.
 */

/*
 * fastsystem.c - scan command for shell metacharacters.
 *		  If they aren't there, just do a path-searching
 *		  exec. Elsewise, fork a shell.
 *
 *		  Miquel van Smoorenburg, June 1991.
 */
 
#include <sys/types.h>
#include <string.h>
#ifdef _MINIX
#  undef NULL
#endif
#include <fcntl.h>
#ifdef _POSIX_SOURCE
#  include <unistd.h>
#endif
#include <stdio.h>
#include <setjmp.h>
#include "window.h"
#include "minicom.h"

#define CNULL ((char *)0)

extern int errno;

/*
 * A modified version of the getargs routine.
 */
static int getargs(s, arps, maxargs)
register char *s;
char *arps[];
int maxargs;
{
	register int i;
	register char *sp;
	register char qchar;
	int literal = 0;

	i = 0;
	while (i < maxargs) {
		while (*s == ' ' || *s == '\t')
			++s;
		if (*s == '\n' || *s == '\0')
			break;
		arps[i++] = sp = s;
		qchar = 0;
		while(*s != '\0'  &&  *s != '\n') {
			if (literal) {
				literal = 0;
				*sp++ = *s++;
				continue;
			}
			literal = 0;
			if (qchar == 0 && (*s == ' ' || *s == '\t')) {
				++s;
				break;
			}
			switch(*s) {
			default:
				*sp++ = *s++;
				break;
			case '\\':
				literal = 1;
				s++;
				break;	
			case '"':
			case '\'':
				if(qchar == *s) {
					qchar = 0;
					++s;
					break;
				}
				if(qchar)
					*sp++ = *s++;
				else
					qchar = *s++;
				break;
			}
		}
		*sp++ = 0;
	}
	if (i >= maxargs)
		return -1;
	arps[i] = (char *)0;
	return i;
}

/*
 * Is a character from s2 in s1?
 */
static int anys(s1, s2)
char *s1, *s2;
{
  while(*s2)
  	if (strchr(s1, *s2++) != CNULL) return(1);
  return(0);
}

/*
 * If there is a shell-metacharacter in "cmd",
 * call a shell to do the dirty work.
 */
int fastexec(cmd)
char *cmd;
{
  char *words[128];
  char *p;

  if (anys(cmd, "~`$&*()=|{};?><"))
  	return(execl("/bin/sh", "sh", "-c", cmd, (char *)0));

  /* Delete escape-characters ment for the shell */
  p = cmd;
  while((p = strchr(p, '\\')) != CNULL)
  	strcpy(p, p + 1);

  /* Split line into words */
  if (getargs(cmd, words, 127) < 0) {
  	return(-1);
  }
  return (execvp(words[0], words));
}

/*
 * Fork, then redirect I/O if neccesary.
 * in    : new stdin
 * out   : new stdout
 * err   : new stderr
 * Returns exit status of "cmd" on success, -1 on error.
 */
int fastsystem(cmd, in, out, err)
char *cmd, *in, *out, *err;
{
  int pid;
  int st;
  int async = 0;
  char *p;

  /* If the command line ends with '&', don't wait for child. */
  p = strrchr(cmd, '&');
  if (p != (char *)0 && !p[1]) {
  	*p = 0;
  	async = 1;
  }
  
  /* Fork. */
  if ((pid = fork()) == 0) { /* child */
  	if (in != CNULL) {
  		close(0);
  		if (open(in, O_RDONLY) < 0) exit(-1);
  	}
  	if (out != CNULL) {
  		close(1);
  		if (open(out, O_WRONLY) < 0) exit(-1);
  	}
  	if (err != CNULL) {
  		close(2);
  		if (open(err, O_RDWR) < 0) exit(-1);
  	}
  	exit(fastexec(cmd));
  } else if (pid > 0) { /* parent */
  	if (async) return(0);
  	pid = m_wait(&st);
  	if (pid < 0) return(-1);
  	return(st);
  }
  return(-1);
}
