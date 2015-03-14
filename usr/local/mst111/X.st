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

Object subclass: #X
    instanceVariableNames: 'socket'
    classVariableNames: ''
    poolDictionaries: 'XGlobals'
    category: 'X hacking'
!

#(BaseMask BaseId RootWindow RootWindowID BlackPixel WhitePixel VisualId)
    do: [ :var | Smalltalk at: var put: nil ].



Behavior defineCFunc: 'connectToServer'
	 withSelectorArgs: 'connectToServer: hostName display: displayNum'
	 forClass: X class
	 returning: #int
	 args: #(string int)
.

Behavior defineCFunc: 'waitForSocket'
	 withSelectorArgs: 'waitForSocket: socket timeOut: anInteger'
	 forClass: X
	 returning: #int	"need a boolean type"
	 args: #(int int)
.

Behavior defineCFunc: 'byte'
	 withSelectorArgs: 'byteFrom: socket'
	 forClass: X
	 returning: #int
	 args: #(int)
.

Behavior defineCFunc: 'word'
	 withSelectorArgs: 'wordFrom: socket'
	 forClass: X
	 returning: #int
	 args: #(int)
.

Behavior defineCFunc: 'long'
	 withSelectorArgs: 'longFrom: socket'
	 forClass: X
	 returning: #int
	 args: #(int)
.

Behavior defineCFunc: 'putByte'
	 withSelectorArgs: 'putByteOn: socket byte: aByte'
	 forClass: X
	 returning: #void
	 args: #(int int)
.

Behavior defineCFunc: 'putWord'
	 withSelectorArgs: 'putWordOn: socket word: aWord'
	 forClass: X
	 returning: #void
	 args: #(int int)
.

Behavior defineCFunc: 'putLong'
	 withSelectorArgs: 'putLongOn: socket long: aLong'
	 forClass: X
	 returning: #void
	 args: #(int int)
!

Behavior defineCFunc: 'putBytes'
	 withSelectorArgs: 'putBytesOn: socket numBytes: n bytes: byteArray'
	 forClass: X
	 returning: #void
	 args: #(int int byteArray)
!


!X class methodsFor: 'instance creation'!

connectTo: server display: displayNum
    | x |
    x _ self new.
    x init: (self connectToServer: server display: displayNum).
    ^x
!!
	

!X methodsFor: 'low level protocol stream interface'!

byte
    ^self byteFrom: socket
!

ubyte
    ^(self byteFrom: socket) bitAnd: 16rFF
!

word
    ^self wordFrom: socket
!

uword
    ^(self wordFrom: socket) bitAnd: 16rFFFF
!

long
    ^self longFrom: socket
!

ulong
    ^self longFrom: socket	"what if it's negative????"
!

getString: len
    | str pad |
    str _ String new: len.
    pad _ (4 - len) bitAnd: 3.

    1 to: len do: 
	[ :i | str at: i put: (Character value: self byte) ].
    pad timesRepeat: [ self byte ]. "pad to 4 byte boundary"
    ^str
!

getUnpaddedString: len
    | str |
    str _ String new: len.
    1 to: len do: 
	[ :i | str at: i put: (Character value: self byte) ].
    ^str
!

mappedId
    | id |
    id _ self long.
    ^Registry at: id
	      ifAbsent: [ nil ]
!

maybeMappedId: symbolArray
    | id |
    id _ self long.
    id < symbolArray size
	ifTrue: [ ^symbolArray at: id + 1 ]
	ifFalse: [ ^Registry at: id
		       ifAbsent: [ nil ] ]
!

skipBytes: len
    | pad |
    len timesRepeat: [ self byte ] "not terribly optimal"
!

byte: aByte
    self putByteOn: socket byte: aByte
!

char: aChar
    self putByteOn: socket byte: aChar asciiValue
!

word: aWord
    self putWordOn: socket word: aWord
!

long: aLong
    self putLongOn: socket long: aLong
!

bytes: byteArray
    self putBytesOn: socket numBytes: byteArray basicSize bytes: byteArray
!

putString: aString
    aString do: [ :char | self byte: char asciiValue ]
!

padBytes: len
    ((4 - len) bitAnd: 3) timesRepeat: [ self byte: 0 ]
!

wait: anInteger			"maybe a Delay at some point?, or a Time"
    ^(self waitForSocket: socket timeOut: anInteger) = 1
!!


!X class methodsFor: 'foo'!	"this shouldn't be X class"

map: aSymbol into: anArray
    ^(anArray indexOf: aSymbol 
	      ifAbsent: [ ^self error: 'Can''t map ', 
			      aSymbol printString, ' into', 
			      anArray printString])
	- 1
!

maybeMap: aSymbol into: anArray
    ^(anArray indexOf: aSymbol 
	      ifAbsent: [ ^aSymbol id ])
	- 1
!

declareBitNames: bitArray inDictionary: aDict
    | bit |
    bit _ 1.
    bitArray do:
	[ :sym | sym notNil
		     ifTrue: [ aDict at: sym put: bit ].
		 bit _ bit bitShift: 1 ]
!!



!X methodsFor: 'private'!

init: socketFD
    socket _ socketFD
!!

