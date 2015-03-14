"======================================================================
|
|   ByteArray Method Definitions
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
| sbyrne     25 Apr 89	  created.
|
"

ArrayedCollection variableByteSubclass: #ByteArray
		  instanceVariableNames: ''
		  classVariableNames: ''
		  poolDictionaries: ''
		  category: nil.

ByteArray comment: 
'My instances are similar to strings in that they are both represented as
a sequence of bytes, but my individual elements are integers, where as
a String''s elements are characters.' !

!ByteArray methodsFor: 'copying'!

shallowCopy
    | newArray |
    newArray _ self species new: self size.
    1 to: self size do:
    	[ :i | newArray at: i put: (self at: i) ].
    ^newArray
!

deepCopy
    ^self shallowCopy

!!
