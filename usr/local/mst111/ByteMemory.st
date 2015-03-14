"======================================================================
|
|   Byte oriented memory definitions
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

Memory variableByteSubclass: #ByteMemory
       instanceVariableNames: ''
       classVariableNames: ''
       poolDictionaries: ''
       category: nil.

ByteMemory comment: 
'I have no instances.  I provide messages to my class that access real memory 
as bytes.  An alternative implementation would be to have a single instance
of byte memory that represented all memory, and at: and at:put: accessor
methods, but since you''d typically refer to that instance via a global
variable, and since the global variable would probably be named ByteMemory,
the actual method invocations are exactly the same in either case.' !


!ByteMemory methodsFor: 'basic'!

!
