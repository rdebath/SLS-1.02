"======================================================================
|
|   False Method Definitions
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
| sbb	     12 Sep 91	  Fixed ifFalse: to evaluate its block.
|
| sbyrne     25 Apr 89	  created.
|
"

Boolean subclass: #False
	instanceVariableNames: 'truthValue' " ### What's the real ST-80 name? "
	classVariableNames: ''
	poolDictionaries: ''
	category: nil.

False comment: 
'I always tell lies. 
I have a single instance in the system, which represents the value false.' !

!False methodsFor: 'basic'!

ifTrue: trueBlock ifFalse: falseBlock
    ^falseBlock value
!

ifFalse: falseBlock ifTrue: trueBlock
    ^falseBlock value
!

ifTrue: trueBlock
    ^nil
!

ifFalse: falseBlock
    ^falseBlock value
!

not
    ^true
!

& aBoolean
    ^false
!

| aBoolean
    ^aBoolean
!

eqv: aBoolean
    ^aBoolean not
!

xor: aBoolean
    ^aBoolean
!

and: aBlock
    ^false
!

or: aBlock
    ^aBlock value
!!



!False methodsFor: 'printing'!

printOn: aStream
    'false' printOn: aStream

!!
