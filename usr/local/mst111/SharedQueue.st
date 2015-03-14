"======================================================================
|
|   SharedQueue Method Definitions
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

Object subclass: #SharedQueue
       instanceVariableNames: 'queueSem valueReady queue'
       classVariableNames: ''
       poolDictionaries: ''
       category: nil!

SharedQueue comment: 
'My instances provide a guaranteed safe mechanism to allow for communication
between processes.  All access to the underlying data structures is
controlled with critical sections so that things proceed smoothly.' !


!SharedQueue class methodsFor: 'instance creation'!

new
    ^super new init
!!



!SharedQueue methodsFor: 'accessing'!

next
    | result |
    valueReady wait.
    queueSem critical: [ result _ queue removeFirst ].
    ^result
!

nextPut: value
    queueSem critical: [ queue addLast: value ].
    valueReady signal.
    ^value
!!



!SharedQueue methodsFor: 'private'!

init
    queue _ OrderedCollection new: 10.
    valueReady _ Semaphore new.
    queueSem _ Semaphore forMutualExclusion
!!
