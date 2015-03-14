"======================================================================
|
|   CompiledMethod Method Definitions
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
| sbyrne     27 Dec 89	  Added real print method for compiled methods.
|
| sbyrne      6 Sep 89	  Added lots of methods: inspect, =, hash, method
|			  cateogry, methodSourceCode, methodSourceString, and
|			  some private accessors such as bytecodeAt:.
|
| sbyrne     25 Apr 89	  created.
|
"

ArrayedCollection variableByteSubclass: #CompiledMethod
		  instanceVariableNames: 'descriptor methodHeader'
		  classVariableNames: ''
		  poolDictionaries: ''
		  category: nil.

CompiledMethod comment:
'I represent methods that have been compiled.  I can recompile
methods from their source code, I can invoke Emacs to edit the source code
for one of my instances, and I know how to access components of my
instances.' !


"Make sure that this symbol is defined, even if it doesn't work just
yet."
Smalltalk at: #Debugger put: nil!

!CompiledMethod methodsFor: 'basic'!

methodCategory
    ^descriptor category
!

methodCategory: aCategory
    ^descriptor category: aCategory
!

methodSourceCode
    ^descriptor sourceCode
!

methodSourceString
    ^descriptor sourceString
!

methodSourceFile
    ^descriptor sourceFile
!

methodSourcePos
    ^descriptor sourcePos
!

= aMethod
    descriptor = aMethod getDescriptor ifFalse: [ ^false ].
    methodHeader = aMethod getHeader ifFalse: [ ^false ].
    1 to: self numLiterals do:
    	[ :i |  (self literalAt: i) = (aMethod literalAt: i)
	    	    ifFalse: [ ^false ] ].
    1 to: self numBytecodes do:
    	[ :i | (self bytecodeAt: i) = (aMethod bytecodeAt: i)
	    	    ifFalse: [ ^false ] ].
    ^true
!

hash
    | hashValue |
    hashValue _ descriptor hash.
    hashValue _ ((hashValue bitShift: 1)
    	          bitXor: methodHeader hash)
		  bitAnd: 16r1FFFFFFF.
    1 to: self numLiterals do:
    	[ :i | hashValue _ ((hashValue bitShift: 1)
    	   	    	     bitXor: (self literalAt: i) hash)
		    	     bitAnd: 16r1FFFFFFF ].
    1 to: self numBytecodes do:
    	[ :i | hashValue _ ((hashValue bitShift: 1)
    	    	    	     bitXor: (self bytecodeAt: i) hash)
		    	     bitAnd: 16r1FFFFFFF ].
    ^hashValue
!!



!CompiledMethod methodsFor: 'method header accessors'!

"The structure of a method header is as follows (from mstinterp.h)

   3                   2                   1 
 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|1|.|.|.|.|.|flg| prim index    | #args   | #temps  | #literals |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

literals 6 0..5
temporarycount 5 6..10
args 5 11..15
primitiveIndex 8 16..23
flags 2 24-25
flags 0 -- use arguments as they are, ignore prim index
flags 1 -- return self
flags 2 -- return instance variable
flags 3 -- call the primitive indexed by primIndex
"

flags
    ^(methodHeader bitShift: -24) bitAnd: 16r3
!

primitive
    ^(methodHeader bitShift: -16) bitAnd: 16rFF
!

numArgs
    | flags |
    flags _ self flags.
    (flags = 2) | (flags = 3)
    	ifTrue: [ ^0 ].
    ^(methodHeader bitShift: -11) bitAnd: 16r1F
!

numTemps
    self flags = 0
    	ifFalse: [ ^0 ].
    ^(methodHeader bitShift: -6) bitAnd: 16r1F
!

numLiterals
    self flags = 0
    	ifFalse: [ ^0 ].
    ^methodHeader bitAnd: 16r3F
!!



!CompiledMethod methodsFor: 'copying'!

shallowCopy
    ^(CompiledMethod newMethod: self basicSize
		     header: methodHeader) shallowCopyMethodContents: self
!

deepCopy
    ^(CompiledMethod newMethod: self basicSize
		     header: methodHeader) deepCopyMethodContents: self
!!



!CompiledMethod methodsFor: 'debugging'!

inspect
    | class instVars |
    class _ self class.
    instVars _ class instVarNames.
    'An instance of ' print.
    class printNl.
    1 to: (instVars size - 1) do: "assumes methodHeader is last inst var"
    	[ :i | '  ' print.
	       (instVars at: i) print.
	       ': ' print.
	       (self objectAt: i)  printNl ].
    '  Header Flags: ' printNl.
    '    flags: ' print.
    self flags printNl.
    '    primitive index: ' print.
    self primitive printNl.
    '    number of arguments: ' print.
    self numArgs printNl.
    '    number of temporaries: ' print.
    self numTemps printNl.
    '    number of literals: ' print.
    self numLiterals printNl.
    self numLiterals > 0
    	ifTrue: [ '  literals: [' printNl.
		  1 to: self numLiterals do:
		      [ :i | '    ' print.
			     i print.
			     ': ' print.
			     (self literalAt: i) storeNl ].
		  '  ]' printNl ]
!!



!CompiledMethod methodsFor: 'debugging'!
breakpointAt: byteIndex
    "self notYetImplemented"
    Debugger recordOldByte: (self bytecodeAt: byteIndex)
	     atIndex: byteIndex
	     forMethod: self.
    self bytecodeAt: byteIndex put: Debugger debugByte
!

breakAtLine: lineNumber
    self notYetImplemented
!

removeBreakpointAt: byteIndex
    | oldByte |
    oldByte _ Debugger origByteAt: byteIndex forMethod: self.
    oldByte notNil 
	ifTrue: [ self bytecodeAt: byteIndex put: oldByte ]
!!



!CompiledMethod methodsFor: 'printing'!

printOn: aStream
    "### This could be more interesting, such as calling the decompiler, or
     printing out the byte codes, or ... yeah, yeah, that's it, the byte
     codes...also need to decode the header information to display that
     interesting information"
    | primIndex numLits |
    true 
	ifTrue: [ 'a CompiledMethod' printOn: aStream. ]
	ifFalse: 
	    [ 'Header Info: ' printOn: aStream.
	      (primIndex _ self primitive) > 0 
		  ifTrue: [ 'Primitive: ' printOn: aStream ].
	      numLits _ self numLiterals.
	      ' # Args: ' printOn: aStream.
	      (self numArgs) printOn: aStream.
	      ' # Temps: ' printOn: aStream.
	      (self numTemps) printOn: aStream.
	      ' # Literals: ' printOn: aStream.
	      numLits printOn: aStream. aStream nl.
	      numLits > 0 
		  ifTrue: [ 'Literals' printOn: aStream. aStream nl.
			    '--------' printOn: aStream. aStream nl.
			    1 to: numLits do:
				[ :i | '    [' printOn: aStream.
				       i printOn: aStream.
				       ']: ' printOn: aStream.
				       (self literalAt: i) storeOn: aStream.
				       aStream nl ] ].
	      " Emit header info here too "
   	      'Byte codes' printOn: aStream. aStream nl.
	      '----------' printOn: aStream. aStream nl.
	      self printByteCodesOn: aStream ]
					     
!

storeOn: aStream
    self printOn: aStream
!!



!CompiledMethod methodsFor: 'private'!

shallowCopyMethodContents: aMethod 
    "Don't need to copy the method header; it's already done"
    descriptor _ aMethod getDescriptor.
    1 to: aMethod numLiterals do:
    	[ :i | self literalAt: i put: (aMethod literalAt: i) ].
    1 to: aMethod numBytecodes do:
        [ :i | self bytecodeAt: i put: (aMethod bytecodeAt: i) ]
!

deepCopyMethodContents: aMethod
    "Don't need to copy the method header; it's already done"
    descriptor _ aMethod getDescriptor deepCopy.
    1 to: aMethod numLiterals do:
    	[ :i | self literalAt: i put: (aMethod literalAt: i) deepCopy ].
    1 to: aMethod numBytecodes do:
        [ :i | self bytecodeAt: i put: (aMethod bytecodeAt: i) ]
!

printByteCodesOn: aStream
    | numBytes i |
    i _ 1.
    numBytes _ self numBytecodes.
    [ i <= numBytes ] whileTrue:
	[ i _ i + (self printByteAt: i on: aStream) ]
!


printByteAt: anIndex on: aStream
    | byte nextByte skip |
    byte _ self bytecodeAt: anIndex.
    byte == 127 "Debugger debugByte"
	ifTrue: [ byte _ Debugger origByteAt: anIndex forMethod: self ].
    skip _ 1.
    '    [' printOn: aStream.
    anIndex printOn: aStream.
    ']: ' printOn: aStream.
    byte < 95 ifTrue:
	[ self printIndexedAt: anIndex on: aStream ].
    (byte between: 96 and: 111) ifTrue:
	[ self emitSimplePop: byte on: aStream ].
    (byte between: 112 and: 125) ifTrue: 
	[ self emitBuiltin: byte on: aStream ].
    "127 is the debugger breakpoint and we don't get it here"
    byte == 128 ifTrue: 
	[ skip _ 2.
	  self print2BytePush: (self bytecodeAt: anIndex + 1) on: aStream ].
    byte == 129 ifTrue: 
	[ skip _ 2.
	  self print2ByteStackOp: 'store' at: anIndex on: aStream ].
    byte == 130 ifTrue: 
	[ skip _ 2.
	  self print2ByteStackOp: 'pop and store' at: anIndex on: aStream ].
    (byte between: 131 and: 134) ifTrue: 
	[ skip _ self emitIndexedSend: anIndex on: aStream ].
    byte == 135 ifTrue:
	[ 'pop stack top ' printOn: aStream ].
    byte == 136 ifTrue:
	[ 'duplicate stack top' printOn: aStream ].
    byte == 137 ifTrue: 
	[ 'push current context' printOn: aStream ].
    (byte between: 138 and: 143) ifTrue:
	[ 'ILLEGAL bytecode ' printOn: aStream.
	  byte printOn: aStream ].
    (byte between: 144 and: 175) ifTrue:
	[ skip _ self printJump: anIndex on: aStream ].
    (byte between: 176 and: 191) ifTrue:
	[ 'send arithmetic message "' printOn: aStream.
	  (#(+	    	-   	    <	    	>
	     <=     	>=  	    =	    	~=
	     *	    	/   	    \\	    	@
	     bitShift:	//  	    bitAnd: 	bitOr:)
	       at: (byte bitAnd: 15) + 1) printOn: aStream.
	  '"' printOn: aStream ].
    (byte between: 192 and: 207) ifTrue:
	[ 'send special message "' printOn: aStream.
	  (#(at:    	at:put:	    size    	next
    	    nextPut:	atEnd	    ==	    	class
    	    blockCopy:	value	    value:  	do:
	    new	    	new:	    x	    	y)
	       at: (byte bitAnd: 15) + 1) printOn: aStream.
	  '"' printOn: aStream ].
    (byte between: 208 and: 255) ifTrue:
	[ self printSmallArgSend: byte on: aStream ].
    aStream nextPut: (Character nl).
    ^skip
!	

printIndexedAt: anIndex on: aStream
    | byte index |
    byte _ self bytecodeAt: anIndex.
    byte <= 15 ifTrue:
    	[ ^self pushIndexed: 'Instance Variable' 
		withIndex: (byte bitAnd: 15)
		on: aStream ].
    byte <= 31 ifTrue:
	[ ^self pushIndexed: 'Temporary'
		withIndex: (byte bitAnd: 15)
		on: aStream ].
    byte <= 63 ifTrue:
	[ ^self pushIndexed: 'Literal'
		withIndex: (byte bitAnd: 31)
		on: aStream ].

    " >= 64 case here "
    'push Global Variable[' printOn: aStream.
    (byte bitAnd: 31) printOn: aStream.
    '] = ' printOn: aStream.
    self printAssociationKeyFor: (byte bitAnd: 31) on: aStream
!

pushIndexed: indexLabel withIndex: anIndex on: aStream
    'push ' printOn: aStream.
    indexLabel printOn: aStream.
    '[' printOn: aStream.
    anIndex printOn: aStream.
    ']' printOn: aStream
!

emitSimplePop: byte on: aStream
    (byte between: 96 and: 103) ifTrue:
	[ aStream nextPutAll: 'pop and store instance variable['.
	  (byte bitAnd: 7) printOn: aStream.
	  aStream nextPut: $] ].
    (byte between: 104 and: 111) ifTrue:
	[ aStream nextPutAll: 'pop and store Temporary['.
	  (byte bitAnd: 7) printOn: aStream.
	  aStream nextPut: $] ].
!

emitBuiltin: byte on: aStream
    byte == 112 ifTrue: [ 'push self' printOn: aStream ].
    byte == 113 ifTrue: [ 'push true' printOn: aStream ].
    byte == 114 ifTrue: [ 'push false' printOn: aStream ].
    byte == 115 ifTrue: [ 'push nil' printOn: aStream ].
    byte == 116 ifTrue: [ 'push -1' printOn: aStream ].
    byte == 117 ifTrue: [ 'push 0' printOn: aStream ].
    byte == 118 ifTrue: [ 'push 1' printOn: aStream ].
    byte == 119 ifTrue: [ 'push 2' printOn: aStream ].
    byte == 120 ifTrue: [ 'return self' printOn: aStream ].
    byte == 121 ifTrue: [ 'return true' printOn: aStream ].
    byte == 122 ifTrue: [ 'return false' printOn: aStream ].
    byte == 123 ifTrue: [ 'return nil' printOn: aStream ].
    byte == 124 ifTrue: [ 'return Message stack top' printOn: aStream ].
    byte == 125 ifTrue: [ 'return Block stack top' printOn: aStream ].
    byte == 126 ifTrue: [ '### ILLEGAL BYTE CODE 126 ###' printOn: aStream ].
!

print2BytePush: byte on: aStream
    self printIndexedPush: (byte bitAnd: 63)
	 type: (byte bitShift: -6)
	 on: aStream
!

printIndexedPush: index type: typeIndex on: aStream
    | typeName |
    typeName _ self indexedLocationName: typeIndex.
    'push ' , typeName , '[' printOn: aStream.
    index printOn: aStream.
    ']' printOn: aStream.
    typeIndex = 3 ifTrue:
	[ ' = ' printOn: aStream.
	  self printAssociationKeyFor: index
	       on: aStream ]
!

indexedLocationName: locIndex
    ^#('Instance Variable' 'Temporary' 'Literal' 'Global Variable')
	at: locIndex + 1
!

print2ByteStackOp: opName at: anIndex on: aStream
    | nextByte locationName locIndex |
    nextByte _ self bytecodeAt: anIndex + 1.
    locIndex _ nextByte bitShift: -6.
    locationName _ self indexedLocationName: locIndex.
    locIndex == 2 ifTrue: [ 'ILLEGAL ' printOn: aStream ].
    opName , locationName , '[' printOn: aStream.
    (nextByte bitAnd: 63) printOn: aStream.
    ']' printOn: aStream.
    locIndex == 3 ifTrue:
	[ ' = ' printOn: aStream.
	  self printAssociationKeyFor: (nextByte bitAnd: 63) on: aStream ]
!

emitIndexedSend: anIndex on: aStream
    | byte byte1 byte2 toSuper |
    byte _ self bytecodeAt: anIndex.
    byte _ byte - 131.		"transform to 0..3"
    byte <= 1 ifTrue: [ toSuper _ '' ]
	      ifFalse: [ toSuper _ 'to Super ' ].
    (byte == 0) | (byte == 2)
	ifTrue:
	    [ byte1 _ self bytecodeAt: anIndex + 1.
	      self emitGenericSend: toSuper index: (byte1 bitAnd: 31)
		   args: (byte1 bitShift: -5) on: aStream.
	      ^2 ]
	ifFalse:
	    [ byte1 _ self bytecodeAt: anIndex + 1.
	      byte2 _ self bytecodeAt: anIndex + 2.
	      self emitGenericSend: toSuper index: byte2
		   args: byte1 on: aStream.
	      ^3]
!


emitGenericSend: toSuper index: anIndex args: numArgs on: aStream
    'send ' , toSuper , 'selector ' printOn: aStream.
    anIndex printOn: aStream.
    ', ' printOn: aStream.
    numArgs printOn: aStream.
    ' args = ' printOn: aStream.
    self printLiteralSymbolAt: anIndex on: aStream
!

printJump: anIndex on: aStream
    | byte |
    byte _ self bytecodeAt: anIndex.
    byte <= 151 ifTrue:
	[ 'jump to ' printOn: aStream.
	  ((byte bitAnd: 7) + anIndex + 1 + 1 ) printOn: aStream.
	  ^1 ].
    byte <= 159 ifTrue:
	[ 'jump to ' printOn: aStream.
	  ((byte bitAnd: 7) + anIndex + 1 + 1 ) printOn: aStream.
	  ' if false' printOn: aStream.
	  ^1 ].
    byte <= 167 ifTrue:
	[ 'jump to ' printOn: aStream.
	  (((byte bitAnd: 7) - 4) * 256 + (self bytecodeAt: anIndex + 1)
	      + anIndex + 2) printOn: aStream.
	  ^2 ].
    byte <= 171 ifTrue:
	[ 'pop and jump to ' printOn: aStream.
	  ((byte bitAnd: 3) * 256 + (self bytecodeAt: anIndex + 1)
	      + anIndex + 2) printOn: aStream.
	  ' if true' printOn: aStream.
	  ^2 ].
    byte <= 175 ifTrue:
	[ 'pop and jump to ' printOn: aStream.
	  ((byte bitAnd: 3) * 256 + (self bytecodeAt: anIndex + 1)
	      + anIndex + 2) printOn: aStream.
	  ' if false' printOn: aStream.
	  ^2 ]
!

printSmallArgSend: byte on: aStream
    | numArgs |
    byte _ byte - 208.
    numArgs _ byte // 16.
    'send selector ' printOn: aStream.
    (byte bitAnd: 15) printOn: aStream.
    ', ' printOn: aStream.
    numArgs printOn: aStream.
    numArgs == 1 
	ifTrue: [ ' arg' printOn: aStream ]
	ifFalse: [ ' args' printOn: aStream ].
    ' = ' printOn: aStream.
    self printLiteralSymbolAt: (byte bitAnd: 15) on: aStream
!

printAssociationKeyFor: anIndex on: aStream
    | assoc |
    assoc _ self literalAt: anIndex + 1.
    assoc key printOn: aStream
!

printLiteralSymbolAt: anIndex on: aStream
    (self literalAt: anIndex + 1) printOn: aStream
!

getDescriptor
    ^descriptor
!

getHeader
    ^methodHeader
!

literalAt: anIndex
    ^self objectAt: (anIndex + 2)
!

literalAt: anInteger put: aValue
    self objectAt: anInteger + 2 put: aValue
!

numBytecodes
    ^(self basicSize) - (self bytecodeStart)
!

bytecodeAt: anIndex
    ^self basicAt: (anIndex + self bytecodeStart)
!

bytecodeAt: anIndex put: aValue
    ^self basicAt: (anIndex + self bytecodeStart) put: aValue
!

bytecodeStart
    ^4 * self numLiterals
!!
