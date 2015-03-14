"creation and printing"
| A |
    '5 lines should be the same: A = 100@100 corner: 200@200' printNl.
    A _ Rectangle left: 100 right: 200 top: 100 bottom: 200.
    'A = ' print.
    A printNl.
    A _ Rectangle origin: 100@100 corner: 200@200.
    'A = ' print.
    A printNl.
    A _ Rectangle origin: 100@100 extent: 100@100.
    'A = ' print.
    A printNl.
    A _ (100@100) corner: 200@200.
    'A = ' print.
    A printNl.
    A _ (100@100) extent: 100@100.
    'A = ' print.
    A printNl!

"accessing"
| A |
    A _ Rectangle origin: 100@100 extent: 150@150.
    'A = ' print.
    A printNl.
    'topLeft = ' print.
    (A topLeft) printNl.
    'top = ' print.
    (A top) printNl.
    'rightCenter = ' print.
    (A rightCenter) printNl.
    'bottom = ' print.
    (A bottom) printNl.
    'center = ' print.
    (A center) printNl.
    'extent = ' print.
    (A extent) printNl.
    'area = ' print.
    (A area) printNl!

"rectangle functions"
| A B C D |
    A _ 50@50 corner: 200@200.
    B _ 120@120 corner: 260@240.
    C _ 100@300 corner: 300@400.
    D _ 20@20 corner: 400@400.
    (A amountToTranslateWithin: C) printNl.
    (A intersect: B) printNl.
    (D intersect: C) printNl.
    (A areasOutside: B) printNl.
    (D areasOutside: C) printNl.
    (D areasOutside: B) printNl.
    (C expandBy: 10) printNl.
    (C insetBy: 10@20) printNl.
    (B merge: C) printNl!

"testing"
| A B C |
    A _ 50@50 corner: 200@200.
    B _ 120@120 corner: 260@240.
    C _ 100@300 corner: 300@400.
    (A contains: B) printNl.
    (C containsPoint: 200@320) printNl.
    (A intersects: B) printNl!

"truncation and round off and transforming"
| A B C |
    A _ 50@50 corner: 200@200.
    B _ 120@120 corner: 260@240.
    C _ 100@300 corner: 300@400.
    (A moveBy: 50@50) printNl.
    (A moveTo: 200@300) printNl.
    (A scaleBy: 2) printNl.
    (A translateBy: -100) printNl!
