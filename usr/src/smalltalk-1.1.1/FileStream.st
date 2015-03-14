"======================================================================
|
|   FileStream Method Definitions
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
| sbyrne     19 May 90	  Rewrite contents to take advantage of the new
|			  FileStream>>size method.
|
| sbyrne     19 Dec 89	  added fileIn: and primitive file in.
|
| sbyrne     21 May 89	  created.
|
"

ReadWriteStream subclass: #FileStream
		instanceVariableNames: 'file name'
		classVariableNames: ''
		poolDictionaries: ''
		category: nil.

FileStream comment: 
'My instances are what conventional programmers think of as files.
My instance creation methods accept the name of a disk file (or any named
file object, such as /dev/rmt0 on UNIX or MTA0: on VMS).' !


!FileStream class methodsFor: 'basic'!

open: fileName mode: fileMode
    | file |
    file _ FileStream new.
    file fileOp: 0 with: fileName with: fileMode.
    ^file
!

popen: commandName dir: direction
    | file |
    file _ FileStream new.
    file fileOp: 7 with: commandName with: direction.
    ^file
!

fileIn: aFileName
    | fileStream |
    fileStream _ self open: aFileName mode: 'r'.
    fileStream fileIn.
    fileStream close
!!

!FileStream methodsFor: 'basic'!

close
    self fileOp: 1
!

next
    ^self fileOp: 2
!

nextPut: aChar
    self fileOp: 3 with: aChar
!

position: bytePosition
    self fileOp: 4 with: bytePosition
! 

position
    ^self fileOp: 5
!

contents
    | str size |			"this is a ### HACK"
    size _ self size.
    str _ String new: size.
    1 to: size do:
	[ :i | str at: i put: (self next) ].
    ^str
!

size
    "Return the current size of the file, in bytes"
    ^self fileOp: 8

!!



!FileStream methodsFor: 'overriding inherited methods'!

reset
    self position: 0
!

setToEnd
    self position: self size
!

skip: anInteger
    | pos |
    pos _ ((self position + anInteger) max: 0) min: self size - 1.
    self position: pos
!

reverseContents
    ^(ReadStream on: self contents) reverseContents
!

isEmpty
    ^self size == 0
!!


!FileStream methodsFor: 'testing'!

atEnd
    ^self fileOp: 6


!!
