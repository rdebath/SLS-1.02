"Tests the class hierarchy"

"======================================================================
|
| Copyright (C) 1988, 1989  Steven B. Byrne.
| All rights reserved.
|
 ======================================================================"

^Array new: 5!

^Array!

^Metaclass!	"should be Metaclass"
^Metaclass class!
^Metaclass class class!	"should be Metaclass, since the metaclass of metaclass
			 is a metaclass"
^Object!
^Object class!		"should be Object class"
^Object class class! 	"should be MetaClass"

^nil!
^nil class!

^true!
^true class!


"Test creating classes at runtime

I apologize for the apparent lack of professionalism in the choice
of variable and method names here."

Object subclass: #Rambo
	instanceVariableNames: 'foo bar'
	classVariableNames: 'guinea pigs'
	poolDictionaries: ''
	category: ''!

!Rambo methodsFor: 'test'!

"Assign some instance variables and return a result"
ramboTest
    foo _ 3.
    bar _ 7.
    ^foo + bar
!

"Assign to class variables"
initPigs: guineaArg and: pigsArg
    guinea _ guineaArg.
    pigs _ pigsArg
!


"inspect instance variables"

foof
    ^foo
!

barf
    ^bar
!


"inspect class variables"

returnGuinea
    ^guinea
!

returnPigs
    ^pigs

!!



Smalltalk at: #testVar put: (Rambo new)!

^testVar foof!			"should be nil (it hasn't been initialized)"
^testVar barf!			"should be nil (it hasn't been initialized)"
^testVar returnGuinea!		"should be nil (it hasn't been initialized)"
^testVar returnPigs!		"should be nil (it hasn't been initialized)"
^Rambo new returnPigs!		"should be nil"
^Rambo new returnGuinea!	"should be nil"

^testVar ramboTest!		"should be 10"

^testVar barf!			"should now be set to 7"

^testVar foof!			"should new be set to 3"

testVar initPigs: 'squeeky' and: 'junior'!
				"nil is returned, we just set some global
				 variables"

^testVar returnPigs!		"should return 'junior'"
^testVar returnGuinea!		"should return 'squeeky'"

"Test that class variables really affect all instances"
^Rambo new returnPigs!		"all instances now return 'junior'"
^Rambo new returnGuinea!	"all instances now return 'squeeky'"



"Create a subclass of a created class to test variable and method inheritance"

Rambo subclass: #Rocky
	instanceVariableNames: 'quem juma'
	classVariableNames: ''
	poolDictionaries: ''
	category: ''!

!Rocky methodsFor: 'test'!

ramboTest
    foo _ 12.
    bar _ 3.
    ^foo + bar
!


quem: arg
    quem _ arg
!

quem
    ^quem
!

juma: arg
   juma _ arg
!

juma
    ^juma

!!

^Rocky new returnGuinea!	"should return 'squeeky' by inheritance"
^Rocky new returnPigs!		"should return 'junior' by inheritance"

^Rocky new quem!		"should return nil (not initialized)"
^Rocky new juma!		"should return nil (not initialized)"

"Test overriding of methods"
testVar _ Rocky new.
^testVar ramboTest!		"should return 15, and set some inst vars"

"Set the instance variables"
testVar quem: 'zoneball'.	
testVar juma: #theJumaSymbol!	"should return nil"

^testVar foof!			"should return 12"
^testVar barf!			"should return 3"
^testVar quem!			"should return 'zoneball'"
^testVar juma!			"should return #theJumaSymbol"

"Test setting class variables from subclass"
^(Rocky new) initPigs: 'frisky' and: 'radar'!
				"should return instance of Rocky"

"+++ work in tests involving Dudley (Milkdud) and Speedy too+++"

"Test subclass access to class variables"
^Rocky new returnGuinea!	"should return 'frisky'"
^Rocky new returnPigs!		"should return 'radar'"

"Test class access to class variables that were modified from subclass"
^Rambo new returnGuinea!	"should return 'frisky'"
^Rambo new returnPigs!		"should return 'radar'"

"Make sure that the existing instance also sees the change in class vars"
^testVar returnPigs!		"should return 'radar'"

