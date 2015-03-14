"======================================================================
|
|   FileSegment Method Definitions
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
| sbyrne      3 Sep 89	  created.
|
"

Object subclass: #FileSegment
       instanceVariableNames: 'fileName startPos length'
       classVariableNames: ''
       poolDictionaries: ''
       category: nil.

FileSegment comment: 
'My instances represent sections of files.  I am primarily used by the
compiler to record source code locations.  I am not a part of the normal
Smalltalk-80 kernel; I am specific to the GNU Smalltalk implementation.' !


!FileSegment class methodsFor: 'basic'!

on: aFile startingAt: startPos for: lengthInteger
    ^self new setFile: aFile start: startPos length: lengthInteger
!!



!FileSegment methodsFor: 'basic'!

asString
    | str file |
    str _ String new: length.
    file _ FileStream open: fileName mode: 'r'.
    file position: startPos.
    1 to: length do: [ :i | str at: i put: (file next) ].
    file close.
    ^str
!

fileName
    ^fileName
!

filePos
    ^startPos
!!



!FileSegment methodsFor: 'private'!

setFile: aFileName start: startingPos length: lengthInteger
    fileName _ aFileName.
    startPos _ startingPos.
    length _ lengthInteger
!!
