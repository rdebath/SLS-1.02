"======================================================================
|
|   SymLink Method Definitions
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
| sbyrne     22 Dec 89	  Created.
|
"

Link subclass: #SymLink
     instanceVariableNames: 'symbol'
     classVariableNames: ''
     poolDictionaries: ''
     category: nil.

SymLink comment: 
'I am used to implement the Smalltalk symbol table.  My instances are
links that contain symbols, and the symbol table basically a hash table that
points to chains of my instances.' !


!SymLink methodsFor: 'iteration'!

do: aBlock
    aBlock value: symbol.
    nextLink notNil ifTrue: [ nextLink do: aBlock ]
!!



!SymLink methodsFor: 'printing'!

printOn: aStream
    aStream nextPut: $[.
    self do: 
	[ :elt | elt printOn: aStream.
		 aStream nextPut: Character space ].
    aStream nextPut: $].
!!
