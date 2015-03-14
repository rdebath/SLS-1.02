"Test integer math operations"

"======================================================================
|
| Copyright (C) 1988, 1989  Steven B. Byrne.
| All rights reserved.
|
 ======================================================================"

^3!

^-3!

"Base tests"
^2r1010!
^8r377!
^16rFE!

"Arithmetic operation tests"

^3 + 4!				"should return 7"

^3 - 4!				"should return -1"

^3 < 4!				"should return true"

^17 > 18!			"should return false"
^17 > 17!			"should return false"

^23 <= 23!			"true"
^23 <= -45!			"false"
^18 >= 21!			"false"
^19 >= 18!			"true"

^23 = 23!			"true"
^23 = 24!			"false"

^45 ~= 89!			"true"
^45 ~= 45!			"false"

^3 * 4!				"should return 12"

^12 / 3!			"should return 4"

^12 / 5!			"should return 2"

^-12 / 5!			"should return -3?"
^12 / -5!			"should return -3?"

" ### need more tests"
