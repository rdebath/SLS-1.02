"===========================================================================
| testing script for basic Point class primitives
| tests the basic primitives to verify that they work
| the test isn't exhaustive so there could be errors that do not show up
 ==========================================================================="

"Creation methods and printing"
| p1 p2 |
  p1 _ Point x: 123 y: 456.
  p2 _ 123@456.
  'p1 = ' print.
  p1 printNl.
  'p2 = ' print.
  p2 printNl!

"accessing"

| p |
  p _ 123@456.
  '(p x) = ' print.
  (p x) printNl.
  '(p y) = ' print.
  (p y) printNl.
  p x: 321.
  'p = ' print.
  p printNl.
  p y: 654.
  'p = ' print.
  p printNl!

"comparing"
| A B C |
  A _ 45@230.
  B _ 175@270.
  C _ 175@200.
  'A < B = ' print.
  (A < B) printNl.
  'A < C = ' print.
  (A < C) printNl.
  'A > C = ' print.
  (A > C) printNl.
  'B > A = ' print.
  (B > A) printNl.
  'A max: B = ' print.
  (A max: B) printNl.
  'A min: B = ' print.
  (A min: B) printNl!

"arithmetic"

| A B C D|
  A _ 45@230.
  B _ 175@300.
  C _ 50@50.
  D _ 160@240.
  'A + B = ' print.
  (A + B) printNl.
  'A + 175 = ' print.
  (A + 175) printNl.
  'A - B = ' print.
  (A - B) printNl.
  'D / 50 = ' print.
  (D / 50) printNl.
  'D // 50 = ' print.
  (D // 50) printNl.
  'D // C = ' print.
  (D // C) printNl.
  '(A - B) abs = ' print.
  ((A - B) abs) printNl.
  '120.5@220.7 rounded = ' print.
  ((120.5@220.7) rounded) printNl.
  'D truncateTo: 50 = ' print.
  (D truncateTo: 50) printNl!

"point functions"

| A B C D |
  A _ 45@230.
  B _ 175@270.
  C _ 160@240.
  D _ 50@50.
  'A dist: B = ' print.
  (A dist: B) printNl.
  'C dotProduct: D = ' print.
  (C dotProduct: D) printNl.
  'C grid: D = ' print.
  (C grid: D) printNl.
  'C normal = ' print.
  (C normal) printNl.
  'C truncatedGrid: D = ' print.
  (C truncatedGrid: D) printNl.
  '175@300 transpose = ' print.
  ((175@300) transpose) printNl!

