"======================================================================
|
|   Word oriented memory definitions
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
| sbyrne     29 May 89	  created.
|
"

Memory variableWordSubclass: #WordMemory
       instanceVariableNames: ''
       classVariableNames: ''
       poolDictionaries: ''
       category: nil.

WordMemory comment: 
'I have no instances.  I provide messages to my class that access real memory 
as 32-bit words (well, really 31 bit integers).  An alternative implementation 
would be to have a single instance of word memory that represented all memory, 
and at: and at:put: accessor methods, but since you''d typically refer to that
instance via a global variable, and since the global variable would probably be
named WordMemory, the actual method invocations are exactly the same in 
either case.' !


!WordMemory class methodsFor: 'accessing'!

at: address
    | value |
    "### This is dependent on big-endian byte ordering..."
    Bigendian 
	ifTrue:
	    [ value _ ByteMemory at: address.
	      value >= 64 
		  ifTrue: [ ^self error: 'Word at ' , 
				address radix: 8 ,
				' is not representable yet' ].
	      address + 1 to: address + 3 do:
		  [ :addr | value _ value * 256 + (ByteMemory at: addr) ].
	      ^value ]
	ifFalse:
	    [ ^self error:
		  'I''m not preprared to handle little endian just yet' ].
!

at: address put: value
    Bigendian
	ifTrue:
	    [ address + 3 to: address + 1 by: -1 do:
		  [ :addr | ByteMemory at: addr
				put: ( value \\ 256).
			    value _ value // 256 ].
	      value < 0
		  ifTrue: [ value _ value negated.
			    value _ value + 64.
			    ByteMemory at: address put: value ]
		  ifFalse: [ ByteMemory at: address put: value ] ]
	ifFalse:
	    [ ^self error:
		  'I''m not preprared to handle little endian just yet' ].
!!
