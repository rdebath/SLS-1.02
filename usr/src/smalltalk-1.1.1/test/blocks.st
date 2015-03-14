"Test out block operations"

"======================================================================
|
| Copyright (C) 1988, 1989  Steven B. Byrne.
| All rights reserved.
|
 ======================================================================"

[45]!		"should return nil"

^[45]!			"should return a block"

[^#quem] value!		"should return #quem"

^['foo'] value!		"should return 'foo'"

^[:i | i] value: 'juma'!	"should return 'juma'"

^[:i :j| j] value: 12 value: 17! 	"should return 17"

!Object methodsFor: 'testing'!

blockTest1
    [#foo]
!

blockTest2
    [^#foo]
!

blockTest3
    ^[#bar]
!

blockTest4
    ^[^#bar]
!

blockTest5: arg
    ^[arg]
!

blockTest6: arg
    ^[:i | arg at: i]
!

blockTest7: arg
    | temp |
    temp _ (arg at: 4) + 8.
    ^[temp]
!

blockTest8: which
    | first second |
    first _ nil blockTest7: #('one' two 3.0 4 $5).
    second _ nil blockTest7: #("You are[,] number" 'six' seven 8.0 9 $A).
    which ifTrue: [ ^first value ]
	  ifFalse: [ ^second value]
!

"Implements a 'closure'!!!  Smalltalk is AMAZING!!!"
blockTest9: initialValue
    | counter |
    counter _ initialValue.
    ^[:incr | counter _ counter + incr. 
              counter]
!

blockTest10: initialValue
    ^[^initialValue]

!!

^nil blockTest1!		"should return nil"

^nil blockTest2!		"should return nil"

^nil blockTest3!		"should return a block context"
^nil blockTest3 value!		"should return #bar"

^nil blockTest4 value!		"should issue an error, we're returning to
				 a non-existent context"

^(nil blockTest5: 'Smalltalk!') value!
				"should return 'Smalltalk!'"

^(nil blockTest6: #('one' two 3.0 4 $5)) value: 2!
				"should return #two"

^(nil blockTest7: #('you' are number 6)) value!
				"should return 14"

^nil blockTest8: true!		"should return 12"
^nil blockTest8: false!		"should return 17"

"Create a block with the initial value of 2"
Smalltalk at: #summingBlock put: (nil blockTest9: 2)!

^summingBlock value: 3!		"should return 5"
^summingBlock value: 6!		"should return 11"
^summingBlock value: 2!		"should return 13"

^(nil blockTest10: 3) value!	"should be illegal; we're returning to non-
				 existent parent"
