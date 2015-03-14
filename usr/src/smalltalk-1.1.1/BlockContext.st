"======================================================================
|
|   BlockContext Method Definitions
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

Object variableSubclass: #BlockContext
       instanceVariableNames: 'caller ip sp numArgs initialIP selector home'
       classVariableNames: ''
       poolDictionaries: ''
       category: nil.

BlockContext comment: 
'My instances represent Smalltalk blocks, which are portions of executeable
code that have access to the environment that they were declared in, take
parameters, and can be passed around as objects to be executed by methods
outside the current class.  Block contexts are sent a message to compute
their value; this property can be used in the construction of control
flow methods.  I also provide some methods that are used in the creation
of Processes from blocks.' !


!BlockContext methodsFor: 'basic'!

whileTrue: aBlock
    [ self value ] whileTrue: [ aBlock value ].
    ^nil
!

whileFalse: aBlock
    [ self value ] whileFalse: [ aBlock value ].
    ^nil
!

whileTrue
    ^[ self value ] whileTrue: []
!

whileFalse
    ^[ self value ] whileFalse: []
!!



!BlockContext methodsFor: 'multiple process'!

fork
    self newProcess resume
!

newProcess
    | block |
    block _ [ self value.
    	      Processor terminateActive ].
    block initBlock.	      
    ^Process on: block at: Processor activePriority
!

newProcessWith: anArray
    | block |
    block _ [ self valueWithArguments: anArray.
    	      Processor terminateActive ].
    block initBlock.	      
    ^Process on: block at: Processor activePriority
!!



!BlockContext methodsFor: 'scheduling'!

forkAt: priority
    (self newProcess priority: priority) resume
!!



!BlockContext methodsFor: 'private'!

initBlock 
    ip _ initialIP.
    sp _ 0.
    caller _ nil
!!
