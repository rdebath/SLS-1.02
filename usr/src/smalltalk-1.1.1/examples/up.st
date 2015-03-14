"Link in pids.c, and install a call to definePidFuncs in initCFuncs  in
mstcint.c.  This will provide access to some process related UNIX system 
calls from within GNU Smalltalk."

"======================================================================
|
| Copyright (C) 1990, 1991 Free Software Foundation, Inc.
| Written by Steve Byrne.
|
| This file is part of GNU Smalltalk.
|
| GNU Smalltalk is free software; you can redistribute it and/or modify it
| under the terms of the GNU General Public License as published by the Free
| Software Foundation; either version 1, or (at your option) any later version.
| 
| GNU Smalltalk is distributed in the hope that it will be useful, but WITHOUT
| ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
| FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
| details.
| 
| You should have received a copy of the GNU General Public License along with
| GNU Smalltalk; see the file LICENSE.  If not, write to the Free Software
| Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  
|
 ======================================================================"



Object subclass: #UnixProcess
    instanceVariableNames: 'pid'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Cool hacking'!

Behavior defineCFunc: 'getpid'
         withSelectorArgs: 'getpid'
	 forClass: UnixProcess class
	 returning: #long
	 args: nil.

Behavior defineCFunc: 'getpgrp'
         withSelectorArgs: 'getpgrp: pid'
	 forClass: UnixProcess
	 returning: #long
	 args: #(long).
Behavior defineCFunc: 'fork'
    	 withSelectorArgs: 'fork'
	 forClass: UnixProcess class
	 returning: #long
	 args: nil.
	 
Behavior defineCFunc: 'kill'
    	 withSelectorArgs: 'kill: pid signal: sig'
	 forClass: UnixProcess
	 returning: #int
	 args: #(int int).

Behavior defineCFunc: 'vfork'
         withSelectorArgs: 'vfork'
	 forClass: UnixProcess class
	 returning: #long
	 args: nil.

Behavior defineCFunc: 'execlp'
    	 withSelectorArgs: 'execlp: name args: argsArray'
	 forClass: UnixProcess class
	 returning: #long
	 args: #(string variadic)!


"  defineCFunc('fork', fork);
  defineCFunc('kill', kill);
  defineCFunc('killpg', killpg);
  defineCFunc('getpgrp', getpgrp);
  defineCFunc('setpgrp', setpgrp);
  defineCFunc('getppid', getppid);
  defineCFunc('nice', nice);
  defineCFunc('vfork', vfork);
  defineCFunc('execve', execve);
  defineCFunc('execl', execl);
  defineCFunc('execle', execle);
  defineCFunc('execlp', execlp);
  defineCFunc('execvp', execvp);"

!UnixProcess class methodsFor: 'test'!

myProc
    ^self new initPid: (self getpid)
!

for: aPid
    ^self new initPid: aPid
!!


!UnixProcess methodsFor: 'accessing'!

processGroup
    ^self getpgrp: pid

!!

!UnixProcess methodsFor: 'private'!

initPid: aPid
    pid _ aPid

!!

| forkPid |
    forkPid _ UnixProcess vfork.
    'Hello from process: ' print.
    forkPid printNl.
    forkPid = 0
    	ifTrue: [ ^UnixProcess execlp: 'dectalk'
	    	    	       args: #(dectalk I am 'sorry,' I do not know what I am saying 0) ]. 
    forkPid printNl!

"| me |
me _ UnixProcess myProc.
me inspect!"
