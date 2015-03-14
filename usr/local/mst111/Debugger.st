"======================================================================
|
|   Experimental Debugger
|
 ======================================================================"


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
| GNU Smalltalk; see the file COPYING.  If not, write to the Free Software
| Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  
|
 ======================================================================"


"
|     Change Log
| ============================================================================
| Author       Date       Change 
| sbyrne     25 Dec 89	  created.
|
"

Object subclass: #Debugger
       instanceVariableNames: ''
       classVariableNames: 'Breakpoints'
       poolDictionaries: ''
       category: 'Method debugging'!

Debugger comment:
'I provide a simple breakpoint facility for GNU Smalltalk.  In the future,
I may be expanded to perform inspection, and other useful functions.
One idea would be to have an instance of a debugger created for each method
that has breakpoints, or somehow have the debugger run from a separate
processes that has a higher priority.'!

!Debugger class methodsFor: 'breakpoint management'!

recordOldByte: byte atIndex: anIndex forMethod: aMethod
    "I preserve the original byte code from a compiled method, for restoration
    after the breakpoint is removed.  My implementation does not notice if
    the compiled method is shared; so a breakpoint in a compiled method will 
    cause a break in whichever classes share this method.  This does not occur
    at all in practice, so it's probably not an issue."
    | breakpointDict |
    byte == self debugByte
	ifTrue: [ ^self ].
    breakpointDict _ Breakpoints at: aMethod
				 ifAbsent: [ Dictionary new ].
    breakpointDict at: anIndex put: byte
!

origByteAt: anIndex forMethod: aMethod
    "I return the original byte code at the given index for the given method.
    If there is no breakpoint for that method at that index, I return nil."
    breakpointDict _ Breakpoints at: aMethod
				 ifAbsent: [ ^nil ].
    ^breakpointDict at: anIndex
		    ifAbsent: [ ^nil ]
!!



!Debugger class methodsFor: 'private'!

debugByte
    "Answer the byte code that the debugger uses for breakpoints"
    ^127
!!
