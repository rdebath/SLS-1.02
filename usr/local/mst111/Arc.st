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

Object subclass: #Arc
       instanceVariableNames: 'origin size angles'
       classVariableNames: ''
       poolDictionaries: ''
       category: 'X hacking'
!

!Arc class methodsFor: 'instance creation'!

new: originPoint size: sizePoint angles: anglesPoint
    ^self new origin: originPoint size: sizePoint angles: anglesPoint
!!


!Arc methodsFor: 'accessing'!

origin
    ^origin
!

size
    ^size
!

angles
    ^angles
!!


!Arc methodsFor: 'writing'!

emitTo: aPacket
    aPacket point: origin.
    aPacket point: size.
    aPacket point: angles
!!


!Arc methodsFor: 'private'!

origin: originPoint size: sizePoint angles: anglesPoint
    origin _ originPoint.
    size _ sizePoint.
    angles _ anglesPoint
!!
	 

