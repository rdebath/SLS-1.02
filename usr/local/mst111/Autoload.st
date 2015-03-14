Object subclass: #Autoload
       instanceVariableNames: 'className fileName'
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Cool hacks' !

Autoload comment:
'I am not a part of the normal Smalltalk kernel class system.  I provide the
ability to do late-loading or "on demand loading" of class definitions.
Through me, you can define any class to be loaded when any message is sent to
the class itself (such as to create an instance).' !

!Autoload class methodsFor: 'instance creation'!

class: classNameString from: fileNameString
    ^Autoload new autoloadInitClass: classNameString 
		  initFile: fileNameString
!!


!Autoload methodsFor: 'accessing'!

doesNotUnderstand: aMessage
    | s |
    Smalltalk removeKey: className.
    FileStream fileIn: fileName.
    ^aMessage reinvokeFor: (Smalltalk at: className
				      ifAbsent: [ ^Autoload error: 
'Autoloaded file should have defined class "', className, '" but didn''t' ])
!!



!Autoload methodsFor: 'private'!

autoloadInitClass: aClassName initFile: aFileName
    className _ aClassName asSymbol.
    Smalltalk at: className put: self.
    fileName _ aFileName.
!!

Autoload superclass: nil!	"force undefined methods"
