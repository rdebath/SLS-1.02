/***********************************************************************
 *
 *	Provide access to some UNIX system calls from Smalltalk.
 *	See up.st for details.
 *
 ***********************************************************************/


/***********************************************************************
 *
 * Copyright (C) 1990, 1991 Free Software Foundation, Inc.
 * Written by Steve Byrne.
 *
 * This file is part of GNU Smalltalk.
 *
 * GNU Smalltalk is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 1, or (at your option) any later 
 * version.
 * 
 * GNU Smalltalk is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 * 
 * You should have received a copy of the GNU General Public License along with
 * GNU Smalltalk; see the file COPYING.  If not, write to the Free Software
 * Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  
 *
 ***********************************************************************/



extern int getpid(), kill(), killpg(), getpgrp(), setpgrp(), getppid(),
  	   nice(), fork(), vfork(), execve(), execl(), execle(), execlp(),
  	   execvp();

definePidFuncs()
{
  defineCFunc("getpid", getpid);
  defineCFunc("kill", kill);
  defineCFunc("killpg", killpg);
  defineCFunc("getpgrp", getpgrp);
  defineCFunc("setpgrp", setpgrp);
  defineCFunc("getppid", getppid);
  defineCFunc("nice", nice);
  defineCFunc("fork", fork);
  defineCFunc("vfork", vfork);
  defineCFunc("execve", execve);
  defineCFunc("execl", execl);
  defineCFunc("execle", execle);
  defineCFunc("execlp", execlp);
  defineCFunc("execvp", execvp);
}


