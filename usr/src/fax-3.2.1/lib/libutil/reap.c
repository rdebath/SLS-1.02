/*
  This file is part of the NetFax system.

  (c) Copyright 1989 by David M. Siegel and Sundar Narasimhan.
      All rights reserved.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation.

    This program is distributed in the hope that it will be useful, 
    but WITHOUT ANY WARRANTY; without even the implied warranty of 
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include <stdio.h>
#include <signal.h>
#include <sys/wait.h>

#include "hash.h"
#include "reap.h"

#define HT_SIZE 128

static HTTABLE *reap_ht;

typedef struct _child_data {
    int (*routine)();
    char *data;
} CHILD_DATA;

static
reap_children()
{
    CHILD_DATA *child;
    int statusp;
    int pid;

    for (;;) {
	if ((pid = wait3(&statusp, WNOHANG, NULL)) <= 0)
	  return;

	if (reap_ht != NULL) {
	    if ((child = (CHILD_DATA *)htgetdata((char *)&pid, reap_ht)) 
		!= NULL) {
		if (child->routine != NULL)
		  (*(child->routine))(pid, statusp, child->data);
		htdelete((char *)&pid, reap_ht);
		cfree(child);
	    }
	}
    }
}

int
reap_fork(routine, data)
int (*routine)();
char *data;
{
    CHILD_DATA *child;
    int mask;
    int pid;

    if (reap_ht == NULL) {
	if ((reap_ht = htinit(HT_SIZE, sizeof(int))) == NULL)
	  return (-1);

	signal(SIGCHLD, reap_children);
    }

    if ((child = (CHILD_DATA *)calloc(1, sizeof(CHILD_DATA))) == NULL)
      return (-1);

    child->routine = routine;
    child->data = data;

    mask = sigblock(sigmask(SIGCHLD));
    switch (pid = fork()) {
      case -1:
      case 0:
	sigsetmask(mask);
	return (pid);

      default:
	htadd((char *)&pid, reap_ht, (char *)child);
	sigsetmask(mask);
	reap_children();
	return (pid);
    }
}
