"======================================================================
|
|   True Method Definitions
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
| sbyrne     19 Sep 89	  Changed to use real method categories.
|
| sbyrne     25 Apr 89	  created.
|
"

Boolean subclass: #True
	instanceVariableNames: 'truthValue'
	classVariableNames: ''
	poolDictionaries: ''
	category: nil.

True comment: 
'I represent truth and justice in the world.  My motto is "semper veritatis".'
!


!True methodsFor: 'basic'!

ifTrue: trueBlock ifFalse: falseBlock
    ^trueBlock value
!

ifFalse: falseBlock ifTrue: trueBlock
    ^trueBlock value
!

ifTrue: trueBlock
    ^trueBlock value
!

ifFalse: falseBlock
    ^nil
!

not
    ^false
!

& aBoolean
    ^aBoolean
!

| aBoolean
    ^true
!

eqv: aBoolean
    ^aBoolean
!

xor: aBoolean
    ^aBoolean not
!

and: aBlock
    ^aBlock value
!

or: aBlock
    ^true
!!



!True methodsFor: 'printing'!

printOn: aStream
    'true' printOn: aStream


!!
