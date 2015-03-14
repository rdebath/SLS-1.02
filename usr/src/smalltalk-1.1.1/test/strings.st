"Test string operations"

"======================================================================
|
| Copyright (C) 1988, 1989  Steven B. Byrne.
| All rights reserved.
|
 ======================================================================"

!Object methodsFor: 'testing'!

strTest
    | str |
    str _ 'quem'.
    str at: 1 put: $Q.
    ^str

!!

^'foo'!

^'foo on you'!

^'foo on
multiple line string
you'!

^'test embedded quote marks, can''t I do this?'!

^'FUBAR' at: 3!

^'quem' at: 1 put: $Q!

^nil strTest!
