"======================================================================
|
|   ProcessorScheduler Method Definitions
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

Object subclass: #ProcessorScheduler
       instanceVariableNames: 'processLists activeProcess'
       classVariableNames: ''
       poolDictionaries: ''
       category: nil.

ProcessorScheduler comment: 
'I provide methods that control the execution of processes.' !

!ProcessorScheduler methodsFor: 'basic'!

activeProcess
    ^activeProcess
!

terminateActive
    activeProcess terminate
    "Poor man's terminate"
"    [true] whileTrue: [ Processor yield ]"
!

yield
    | process priority processList |
    priority _ activeProcess priority.
    processList _ processLists at: priority.
    processList isEmpty
        ifFalse: [ processList addLast: activeProcess.
    	    	   activeProcess suspend ]
!

activePriority
    ^self activeProcess priority
!

timingPriority
    8
!

highIOPriority
    7
!

lowIOPriority
    6
!

userInterruptPriority
    5
!

userSchedulingPriority
    4
!

userBackgroundPriority
    3
!

systemBackgroundPriority
    2

!!
