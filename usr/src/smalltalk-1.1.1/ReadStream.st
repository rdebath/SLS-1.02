"======================================================================
|
|   ReadStream Method Definitions
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
| sbyrne     19 Sep 89	  Converted to use real method categories.
|
| sbyrne     25 Apr 89	  created.
|
"

PositionableStream subclass: #ReadStream
		   instanceVariableNames: ''
		   classVariableNames: ''
		   poolDictionaries: ''
		   category: nil.

ReadStream comment: 
'I implement the set of read-only stream objects.  You may read from
my objects, but you may not write to them.' !

!ReadStream class methodsFor: 'instance creation'!

on: aCollection
    ^(self new initCollection: aCollection) access: 1
!!



!ReadStream methodsFor: 'accessing-reading'!

reverseContents
    "May be faster than generic stream reverseContents."
    | aCollection i numElts |
    numElts _ collection size.
    aCollection _ collection species new: numElts.
    i _ 1.
    [ i < endPtr ] whileTrue:
    	[ aCollection at: numElts - i + 1
	    	      put: (collection at: i) ].
    ^aCollection
!!



!ReadStream methodsFor: 'private methods'!

initCollection: aCollection
    collection _ aCollection.
    ptr _ 1.
    endPtr _ collection size

!!
