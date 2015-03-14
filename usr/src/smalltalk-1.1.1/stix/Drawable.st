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
| sbyrne     24 May 90	  created.
|
"


Object subclass: #Drawable
       instanceVariableNames: 'display id'
       classVariableNames: ''
       poolDictionaries: 'XGlobals'
       category: 'X hacking'
!

Object subclass: #GC		"should this be folded into some higher level class?"
       instanceVariableNames: 'display drawable id'
       classVariableNames: ''
       poolDictionaries: 'XGlobals'
       category: 'X hacking'
!

!Drawable methodsFor: 'X hacking'!

createGC: aBlock
    | packet gc |
    
    packet _ XGCAttrPacket command: 55.

    gc _ GC new: display drawable: self.
    packet long: gc id; long: self id.
    aBlock notNil
	ifTrue: [ aBlock value: packet ]
	ifFalse: [ packet noBits ].
    
    
    display socket bytes: packet done.
    ^gc
!

id
    ^id
!!
