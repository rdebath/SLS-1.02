"======================================================================
|
|   Process Method Definitions
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
| sbyrne     19 Sep 89	  Converted to use real method categories
|
| sbyrne     25 Apr 89	  created.
|
"

Link subclass: #Process
     instanceVariableNames: 'suspendedContext priority myList'
     classVariableNames: ''
     poolDictionaries: ''
     category: nil.

Process comment: 
'I represent a unit of computation.  My instances are independantly
executable blocks that have a priority associated with them, and they
can suspend themselves and resume themselves however they wish.' !

!Process class methodsFor: 'basic'!

on: aBlock at: aPriority
    ^self new onBlock: aBlock at: aPriority
!!


!Process methodsFor: 'basic'!

terminate
    | processList priority process |
    suspendedContext _ nil.
    myList notNil
	ifTrue: [myList remove: self.
		 myList _ nil].
    Processor activeProcess == self
	ifTrue: [self suspend]
"    myList remove: self"
!!



!Process methodsFor: 'accessing'!

priority
    ^priority
!

priority: anInteger
    priority _ anInteger
!!



!Process methodsFor: 'private'!

onBlock: aBlock at: aPriority
    suspendedContext _ aBlock.
    priority _ aPriority

!!
