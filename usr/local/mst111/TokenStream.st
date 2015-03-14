"======================================================================
|
|   Token stream Method Definitions
|
|   A token stream is a stream that's defined in terms of a string.
|   It basically parses off whitespace separated tokens as substrings
|   and returns them (next).  If the entire contents of the string is 
|   requested, it is returned as an Array containing the individual strings.
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
| sbyrne     14 May 90	  removed isWhiteSpace:;replaced uses with Character
|			  isSeparator.
|
| sbyrne     19 Sep 89	  Changed to use real method categories.
|
| sbyrne     12 Jul 89	  created.
|
"

Stream subclass: #TokenStream
       instanceVariableNames: 'charStream'
       classVariableNames: ''
       poolDictionaries:''
       category: nil.

TokenStream comment:
'I am not a typical part of the Smalltalk kernel class hierarchy.\n\
I operate on a stream of characters and return distinct \n\
(whitespace-delimited) groups of characters.' !


!TokenStream class methodsFor: 'instance creation'!

on: aString
    ^self new setStream: (ReadStream on: aString)

!!



!TokenStream methodsFor: 'basic'!

next
    | char tokStream |
    self atEnd ifTrue: [ ^nil ]. "has the nice side effect of skipping
    	    	    	    	  leading white space."
    tokStream _ WriteStream on: (String new: 1).
    [ char _ charStream peek.
      (char notNil) and: [ (char isSeparator) not ] ]
        whileTrue: [ tokStream nextPut: (charStream next) ].
    ^tokStream contents
!

atEnd
    | char |
    [ char _ charStream peek.
      char isNil ] whileFalse:
    	[ (char isSeparator) ifFalse: [ ^false ].
	 charStream next ].
    ^true
!

do: aBlock
    [ self atEnd ] whileFalse:
    	[ aBlock value: self next ]
!

contents
    | arrayStream |
    arrayStream _ WriteStream on: (Array new: 0).
    self do: [ :aToken | arrayStream nextPut: aToken ].
    ^arrayStream contents
!!



!TokenStream methodsFor: 'write methods'!

nextPut: anObject
    self shouldNotImplement
!!



!TokenStream methodsFor: 'private'!

setStream: aStream
    charStream _ aStream
!!
