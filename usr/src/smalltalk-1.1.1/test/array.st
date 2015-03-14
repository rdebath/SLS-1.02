"Test the array routines"

"======================================================================
|
| Copyright (C) 1988, 1989  Steven B. Byrne.
| All rights reserved.
|
 ======================================================================"

!Object methodsFor: 'testing'!

arrayConstTest1
    | a |
    a _ #(foo bar 'quem').
    ^a
!

arrayConstTest2
    | a |
    a _ #(foo bar 'quem').
    ^a class
!

arrayConstTest3
    | a |
    a _ #(foo bar 'quem').
    ^a size
!

arrayConstTest4
    | a |
    a _ #(foo (a sub array) bar 'quem' 3.4 1 $C barn:yard:owl: baz).
    ^a class
!

arrayConstSubscriptTest
    | a |
    a _ #(foo (a sub array) bar 'quem' 3.4 1 $C barn:yard:owl: baz).
    ^a at: self
!

newArray1
    | a |
    a _ Array new: 10.
    ^a class
!

newArray2
    | a |
    a _ Array new: 10.
    a at: self put: self.
    ^a at: self
!!

^nil arrayConstTest1!

^nil arrayConstTest2!

^nil arrayConstTest3!

^nil arrayConstTest4!

^1 arrayConstSubscriptTest!
^2 arrayConstSubscriptTest!
^3 arrayConstSubscriptTest!
^4 arrayConstSubscriptTest!
^5 arrayConstSubscriptTest!
^6 arrayConstSubscriptTest!
^7 arrayConstSubscriptTest!
^8 arrayConstSubscriptTest!
^9 arrayConstSubscriptTest!

^nil newArray1!

^1 newArray2!
^5 newArray2!
^10 newArray2!
