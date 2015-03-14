
(CHAR  "abcdef-dg1ndh" 0)
#\a

(CHAR  "abcdef-dg1ndh" 1)
#\b

(CHAR  "abcdef-dg1ndh" 6)
#\-

(CHAR  "abcdef-dg1ndh" 20)
error

(CHAR  "abcdef-dg1ndh")
error

(CHAR  "abcdef-dg1ndh" -3)
error

(CHAR)
error

(CHAR 2)
error

(CHAR  "abcde" 2 4)
error

(CHAR 'A 0)
#+XCL #\A #-XCL ERROR

(CHAR 'ANNA 0)
#+XCL #\A #-XCL ERROR

(SCHAR 'A 0)
#+XCL #\A #-XCL ERROR

(SCHAR 'ANNA 0)
#+XCL #\A #-XCL ERROR

(SCHAR  "abcdef-dg1ndh" 0)
#\a

(SCHAR  "abcdef-dg1ndh" 1)
#\b

(SCHAR  "abcdef-dg1ndh" 6)
#\-

(SCHAR  "abcdef-dg1ndh" 20)
error

(SCHAR  "abcdef-dg1ndh")
error

(SCHAR  "abcdef-dg1ndh" -3)
error

(SCHAR2)
error

(SCHAR2 2)
error

(SCHAR  "abcde" 2 4)
error

(STRING=  "foo" "foo")
T

(STRING=  "foo" "Foo")
NIL

(STRING=  "foo" "FOO")
NIL

(STRING=  "foo" "bar")
NIL

(STRING=  "together" "frog" :START1 1 :END1 3 :START2 2)
T

(STRING=  "abcdef" "defghi" :START1 3 :END2 3)
T

(STRING=  "abcdefghi" "uvdefmgnj" :START1 3 :END1 6 :START2 2 :END2
5)
T

(STRING=  "abcdefg" "abcdefg" :END2 4)
NIL

(STRING=  "abcdef" "abcdef" :START1 1 :END1 4 :START2 4 :END2 1)
error

(STRING-EQUAL  "foo" "foo")
T

(STRING-EQUAL  "foo" "Foo")
T

(STRING-EQUAL  "foo" "FOO")
T

(STRING-EQUAL  "foo" "bar")
NIL

(STRING-EQUAL  "absDEfg-HijM1#r" "udEFG-hIfvd" :START1 3 :END1 10 :START2
1 :END2
8)
T

(STRING-EQUAL  "ABCdefg" "abcDEFG")
T

(STRING-EQUAL  "ABCdefg" "abcDEFG" :START1 3)
NIL

(STRING-EQUAL  "AbCdEf" "aBcDeF" :START1 5 :END1 3)
error

(STRING<  "" "abcdefgh")
#+XCL 0 #-XCL T

(STRING<  "a" "abcdefgh")
#+XCL 1 #-XCL T

(STRING<  "abc" "abcdefgh")
#+XCL 3 #-XCL T

(STRING<  "cabc" "abcdefgh")
NIL

(STRING<  "abcdefgh" "abcdefgh")
NIL

(STRING<  "xyzabc" "abcdefgh")
NIL

(STRING<  "abc" "xyzabcdefgh")
#+XCL 0 #-XCL T

(STRING<  "abcdefgh" "abcdefgh" :END1 4)
#+XCL 4 #-XCL T

(STRING<  "xyzabc" "abcdefgh" :START1 3)
#+XCL 6 #-XCL T

(STRING<  "abc" "xyzabcdefgh" :START2 3)
#+XCL 3 #-XCL T

(STRING<  "abc" "xyzabcdefgh" :START2 3 :END2 8)
#+XCL 3 #-XCL T

(STRING<  "abc" "xyzabcdefgh" :START2 3 :END2 5)
NIL

(STRING<  "abcdefgh" "")
NIL

(STRING<  "abcdefgh" "a")
NIL

(STRING<  "abcdefgh" "abc")
NIL

(STRING<  "abcdefgh" "cabc")
#+XCL 0 #-XCL T

(STRING<  "abcdefgh" "xyzabc")
#+XCL 0 #-XCL T

(STRING<  "xyzabcdefgh" "abc")
NIL

(STRING<  "abcdefgh" "abcdefgh" :END2 4)
NIL

(STRING<  "xyzabc" "abcdefgh" :START2 3)
NIL

(STRING<  "abc" "xyzabcdefgh" :START2 3)
#+XCL 3 #-XCL T

(STRING<  "abc" "xyzabcdefgh" :START2 3 :END2 8)
#+XCL 3 #-XCL T

(STRING<  "abc" "xyzabcdefgh" :START2 3 :END2 5)
NIL

(STRING<  "abcdef" "bcdefgh")
#+XCL 0 #-XCL T

(STRING<  "abcdef" "abcdefgh" :START2 2)
#+XCL 0 #-XCL T

(STRING<  "abcdef" "bngdabcdef" :START2 9 :END2 5)
error

(STRING>  "" "abcdefgh")
NIL

(STRING>  "a" "abcdefgh")
NIL

(STRING>  "abc" "abcdefgh")
NIL

(STRING>  "cabc" "abcdefgh")
#+XCL 0 #-XCL T

(STRING>  "abcdefgh" "abcdefgh")
NIL

(STRING>  "xyzabc" "abcdefgh")
#+XCL 0 #-XCL T

(STRING>  "abc" "xyzabcdefgh")
NIL

(STRING>  "abcdefgh" "abcdefgh" :END1 4)
NIL

(STRING>  "xyzabc" "abcdefgh" :START1 3)
NIL

(STRING>  "abc" "xyzabcdefgh" :START2 3)
NIL

(STRING>  "abc" "xyzabcdefgh" :START2 3 :END2 8)
NIL

(STRING>  "abc" "xyzabcdefgh" :START2 3 :END2 5)
#+XCL 2 #-XCL T

(STRING>  "abcdefgh" "")
#+XCL 0 #-XCL T

(STRING>  "abcdefgh" "a")
#+XCL 1 #-XCL T

(STRING>  "abcdefgh" "abc")
#+XCL 3 #-XCL T

(STRING>  "abcdefgh" "cabc")
NIL

(STRING>  "abcdefgh" "xyzabc")
NIL

(STRING>  "xyzabcdefgh" "abc")
#+XCL 0 #-XCL T

(STRING>  "abcdefgh" "abcdefgh" :END2 4)
#+XCL 4 #-XCL T

(STRING>  "xyzabc" "abcdefgh" :START2 3)
#+XCL 0 #-XCL T

(STRING>  "abc" "xyzabcdefgh" :START2 3)
NIL

(STRING>  "abc" "xyzabcdefgh" :START2 3 :END2 8)
NIL

(STRING>  "abc" "xyzabcdefgh" :START2 3 :END2 5)
#+XCL 2 #-XCL T

(STRING>  "abcde" "bc")
NIL

(STRING>  "bcdef" "abcde")
#+XCL 0 #-XCL T

(STRING>  "bcdef" "abcdef")
#+XCL 0 #-XCL T

(STRING>  "abcdefghij" "abcdefgh" :START1 1)
#+XCL 1 #-XCL T

(STRING>  "ghijkl" "xyzabcd" :START2 6 :END2 4)
error

(STRING<  "" "abcdefgh")
#+XCL 0 #-XCL T

(STRING<=  "a" "abcdefgh")
#+XCL 1 #-XCL T

(STRING<=  "abc" "abcdefgh")
#+XCL 3 #-XCL T

(STRING<=  "aaabce" "aaabcdefgh")
NIL

(STRING<=  "cabc" "abcdefgh")
NIL

(STRING<=  "abcdefgh" "abcdefgh")
#+XCL 8 #-XCL T

(STRING<=  "xyzabc" "abcdefgh")
NIL

(STRING<=  "abc" "xyzabcdefgh")
#+XCL 0 #-XCL T

(STRING<=  "abcdefgh" "abcdefgh" :END1 4)
#+XCL 4 #-XCL T

(STRING<=  "xyzabc" "abcdefgh" :START1 3)
#+XCL 6 #-XCL T

(STRING<=  "abc" "xyzabcdefgh" :START2 3)
#+XCL 3 #-XCL T

(STRING<=  "abc" "xyzabcdefgh" :START2 3 :END2 8)
#+XCL 3 #-XCL T

(STRING<=  "abc" "xyzabcdefgh" :START2 3 :END2 5)
NIL

(STRING<=  "abcdefgh" "")
NIL

(STRING<=  "abcdefgh" "a")
NIL

(STRING<=  "abcdefgh" "abc")
NIL

(STRING<=  "abcdefgh" "cabc")
#+XCL 0 #-XCL T

(STRING<=  "abcdefgh" "xyzabc")
#+XCL 0 #-XCL T

(STRING<=  "xyzabcdefgh" "abc")
NIL

(STRING<=  "abcdefgh" "abcdefgh" :END2 4)
NIL

(STRING<=  "xyzabc" "abcdefgh" :START2 3)
NIL

(STRING<=  "abc" "xyzabcdefgh" :START2 3)
#+XCL 3 #-XCL T

(STRING<=  "abc" "xyzabcdefgh" :START2 3 :END2 8)
#+XCL 3 #-XCL T

(STRING<=  "abc" "xyzabcdefgh" :START2 3 :END2 5)
NIL

(STRING<=  "abcdef" "bcdefgh")
#+XCL 0 #-XCL T

(STRING<=  "abcdef" "abcdefgh" :START2 2)
#+XCL 0 #-XCL T

(STRING<=  "abcdef" "bngdabcdef" :START2 9 :END2 5)
error


(STRING>= "" "abcdefgh")
NIL

(STRING>= "a" "abcdefgh")
NIL

(STRING>= "abc" "abcdefgh")
NIL

(STRING>= "cabc" "abcdefgh")
#+XCL 0 #-XCL T

(STRING>= "abcdefgh" "abcdefgh")
#+XCL 8 #-XCL T

(STRING>= "xyzabc" "abcdefgh")
#+XCL 0 #-XCL T

(STRING>= "abc" "xyzabcdefgh")
NIL

(STRING>= "abcdefgh" "abcdefgh" :END1 4)
NIL

(STRING>= "xyzabc" "abcdefgh" :START1 3)
NIL

(STRING>= "abc" "xyzabcdefgh" :START2 3)
NIL

(STRING>= "abc" "xyzabcdefgh" :START2 3 :END2 8)
NIL

(STRING>= "abc" "xyzabcdefgh" :START2 3 :END2 5)
#+XCL 2 #-XCL T

(STRING>= "abcdefgh" "")
#+XCL 0 #-XCL T

(STRING>= "abcdefgh" "a")
#+XCL 1 #-XCL T

(STRING>= "abcdefgh" "abc")
#+XCL 3 #-XCL T

(STRING>= "abcdefgh" "cabc")
NIL

(STRING>= "abcdefgh" "xyzabc")
NIL

(STRING>= "xyzabcdefgh" "abc")
#+XCL 0 #-XCL T

(STRING>= "abcdefgh" "abcdefgh" :END2 4)
#+XCL 4 #-XCL T

(STRING>= "xyzabc" "abcdefgh" :START2 3)
#+XCL 0 #-XCL T

(STRING>= "xyzabc" "abcdefgh" :START1 3)
NIL

(STRING>= "abc" "xyzabcdefgh" :START2 3)
NIL

(STRING>= "abc" "xyzabcdefgh" :START2 3 :END2 8)
NIL

(STRING>= "abc" "xyzabcdefgh" :START2 3 :END2 5)
#+XCL 2 #-XCL T

(STRING>= "bcdef" "abcdef")
#+XCL 0 #-XCL T

(STRING>= "abcdefghij" "abcdefgh" :START1 1)
#+XCL 1 #-XCL T

(STRING>= "ghijkl" "xyzabcd" :START2 6 :END2 4)
ERROR

(STRING/= "" "abcdefgh")
#+XCL 0 #-XCL T

(STRING/= "a" "abcdefgh")
#+XCL 1 #-XCL T

(STRING/= "abc" "abcdefgh")
#+XCL 3 #-XCL T

(STRING/= "cabc" "abcdefgh")
#+XCL 0 #-XCL T

(STRING/= "abcdefgh" "abcdefgh")
NIL

(STRING/= "xyzabc" "abcdefgh")
#+XCL 0 #-XCL T

(STRING/= "abc" "xyzabcdefgh")
#+XCL 0 #-XCL T

(STRING/= "abcdefgh" "abcdefgh" :END1 4)
#+XCL 4 #-XCL T

(STRING/= "xyzabc" "abcdefgh" :START1 3)
#+XCL 6 #-XCL T

(STRING/= "abc" "xyzabcdefgh" :START2 3)
#+XCL 3 #-XCL T

(STRING/= "abc" "xyzabcdefgh" :START2 3 :END2 8)
#+XCL 3 #-XCL T

(STRING/= "abc" "xyzabcdefgh" :START2 3 :END2 5)
#+XCL 2 #-XCL T

(STRING/= "abcdefgh" "")
#+XCL 0 #-XCL T

(STRING/= "abcdefgh" "a")
#+XCL 1 #-XCL T

(STRING/= "abcdefgh" "abc")
#+XCL 3 #-XCL T

(STRING/= "abcdefgh" "cabc")
#+XCL 0 #-XCL T

(STRING/= "abcdefgh" "xyzabc")
#+XCL 0 #-XCL T

(STRING/= "xyzabcdefgh" "abc")
#+XCL 0 #-XCL T

(STRING/= "abcdefgh" "abcdefgh" :END2 4)
#+XCL 4 #-XCL T

(STRING/= "xyzabc" "abcdefgh" :START2 3)
#+XCL 0 #-XCL T

(STRING/= "abc" "xyzabcdefgh" :START2 3)
#+XCL 3 #-XCL T

(STRING/= "abc" "xyzabcdefgh" :START2 3 :END2 8)
#+XCL 3 #-XCL T

(STRING/= "abc" "xyzabcdefgh" :START2 3 :END2 5)
#+XCL 2 #-XCL T

(STRING/= "abcdefghi" "uvdefmgnj" :START1 3 :END1 6 :START2 2 :END2 5)
NIL

(STRING/= "abcdefg" "abcdefg" :END2 4)
#+XCL 4 #-XCL T

(STRING/= "abcdef" "abcdef" :START1 1 :END1 4 :START2 4 :END2 1)
ERROR

(STRING-LESSP "" "abcDEFgh")
#+XCL 0 #-XCL T

(STRING-LESSP "a" "Abcdefgh")
#+XCL 1 #-XCL T

(STRING-LESSP "abc" "aBcDEfgh")
#+XCL 3 #-XCL T

(STRING-LESSP "cABc" "aBCDefgh")
NIL

(STRING-LESSP "abCDeFgh" "abCDEfgh")
NIL

(STRING-LESSP "xyzAbc" "ABcCDfgh")
NIL

(STRING-LESSP "aBC" "xYZAbcdEfgh")
#+XCL 0 #-XCL T

(STRING-LESSP "abcDEfgh" "abcDEfgh" :END1 4)
#+XCL 4 #-XCL T

(STRING-LESSP "XYZabc" "ABcdefgh" :START1 3)
#+XCL 6 #-XCL T

(STRING-LESSP "aBc" "xyZABcdefgh" :START2 3)
#+XCL 3 #-XCL T

(STRING-LESSP "abc" "xyzabCDEcdefgh" :START2 3 :END2 8)
#+XCL 3 #-XCL T

(STRING-LESSP "abc" "xyzABcdefgh" :START2 3 :END2 5)
NIL

(STRING-LESSP "abcdefgh" "")
NIL

(STRING-LESSP "Abcdefgh" "a")
NIL

(STRING-LESSP "ABCdefgh" "abc")
NIL

(STRING-LESSP "ABCdefgh" "cabc")
#+XCL 0 #-XCL T

(STRING-LESSP "abcdefgh" "xyzABC")
#+XCL 0 #-XCL T

(STRING-LESSP "xyzABCdefgh" "abc")
NIL

(STRING-LESSP "abcdEFgh" "abcdeFGh" :END2 4)
NIL

(STRING-LESSP "xyzaBC" "abCDefgh" :START2 3)
NIL

(STRING-LESSP "ABC" "xyzabcdefgh" :START2 3)
#+XCL 3 #-XCL T

(STRING-LESSP "ABC" "xyzabcdefgh" :START2 3 :END2 8)
#+XCL 3 #-XCL T

(STRING-LESSP "ABC" "xyzabcdefgh" :START2 3 :END2 5)
NIL

(STRING-LESSP "aBCDef" "bcdefgh")
#+XCL 0 #-XCL T

(STRING-LESSP "aBCDef" "abcdefgh" :START2 2)
#+XCL 0 #-XCL T

(STRING-LESSP "aBCDef" "bngdabcdef" :START2 9 :END2 5)
ERROR

(STRING-GREATERP "" "abcdefgh")
NIL

(STRING-GREATERP "A" "abcdefgh")
NIL

(STRING-GREATERP "ABc" "abcdefgh")
NIL

(STRING-GREATERP "CAbc" "abcdefgh")
#+XCL 0 #-XCL T

(STRING-GREATERP "abcdefgh" "abcDEFgh")
NIL

(STRING-GREATERP "xyzabc" "abCDEfgh")
#+XCL 0 #-XCL T

(STRING-GREATERP "ABC" "xyzabcdefgh")
NIL

(STRING-GREATERP "ABCdefgh" "abcdefgh" :END1 4)
NIL

(STRING-GREATERP "xyzaBc" "ABCdefgh" :START1 3)
NIL

(STRING-GREATERP "abc" "xyzABcdefgh" :START2 3)
NIL

(STRING-GREATERP "abc" "xyzABcdefgh" :START2 3 :END2 8)
NIL

(STRING-GREATERP "abc" "xyZAbcdefgh" :START2 3 :END2 5)
#+XCL 2 #-XCL T

(STRING-GREATERP "abcdefgh" "")
#+XCL 0 #-XCL T

(STRING-GREATERP "Abcdefgh" "a")
#+XCL 1 #-XCL T

(STRING-GREATERP "ABCdefgh" "abc")
#+XCL 3 #-XCL T

(STRING-GREATERP "ABCdefgh" "cabc")
NIL

(STRING-GREATERP "ABCdefgh" "xyzabc")
NIL

(STRING-GREATERP "xyzabcdefgh" "Abc")
#+XCL 0 #-XCL T

(STRING-GREATERP "abcdefgh" "aBCDefgh" :END2 4)
#+XCL 4 #-XCL T

(STRING-GREATERP "xyzabc" "abcdEFgh" :START2 3)
#+XCL 0 #-XCL T

(STRING-GREATERP "ABC" "xyzabcdefgh" :START2 3)
NIL

(STRING-GREATERP "ABC" "xyzabcdefgh" :START2 3 :END2 8)
NIL

(STRING-GREATERP "ABC" "xyzabcdefgh" :START2 3 :END2 5)
#+XCL 2 #-XCL T

(STRING-GREATERP "bCDEf" "abcde")
#+XCL 0 #-XCL T

(STRING-GREATERP "bcDEF" "abcdef")
#+XCL 0 #-XCL T

(STRING-GREATERP "abCDEfghij" "abcdefgh" :START1 1)
#+XCL 1 #-XCL T

(STRING-GREATERP "ghijKl" "xyzabcd" :START2 6 :END2 4)
ERROR

(STRING-NOT-GREATERP  "" "abcdefgh")
#+XCL 0 #-XCL T

(STRING-NOT-GREATERP  "A" "abcdefgh")
#+XCL 1 #-XCL T

(STRING-NOT-GREATERP  "aBC" "abcdefgh")
#+XCL 3 #-XCL T

(STRING-NOT-GREATERP  "CABc" "abcdefgh")
NIL

(STRING-NOT-GREATERP  "abcDEFgh" "abcdefgh")
#+XCL 8 #-XCL T

(STRING-NOT-GREATERP  "xyzabc" "ABcdefgh")
NIL

(STRING-NOT-GREATERP  "abc" "xyzABcdefgh")
#+XCL 0 #-XCL T

(STRING-NOT-GREATERP  "ABCDEFgh" "abcdefgh" :END1 4)
#+XCL 4 #-XCL T

(STRING-NOT-GREATERP  "xyzabc" "aBCDefgh" :START1 3)
#+XCL 6 #-XCL T

(STRING-NOT-GREATERP  "ABC" "xyzabcdefgh" :START2 3)
#+XCL 3 #-XCL T

(STRING-NOT-GREATERP  "ABC" "xyzabcdefgh" :START2 3 :END2 8)
#+XCL 3 #-XCL T

(STRING-NOT-GREATERP  "ABC" "xyzabcdefgh" :START2 3 :END2 5)
NIL

(STRING-NOT-GREATERP  "abcdefgh" "")
NIL

(STRING-NOT-GREATERP  "Abcdefgh" "a")
NIL

(STRING-NOT-GREATERP  "ABCdefgh" "abc")
NIL

(STRING-NOT-GREATERP  "ABCdefgh" "cabc")
#+XCL 0 #-XCL T

(STRING-NOT-GREATERP  "ABCdefgh" "xyzabc")
#+XCL 0 #-XCL T

(STRING-NOT-GREATERP  "xyzABCdefgh" "abc")
NIL

(STRING-NOT-GREATERP  "abcdeFgh" "abcdefgh" :END2 4)
NIL

(STRING-NOT-GREATERP  "xyzABC" "abcdefgh" :START2 3)
NIL

(STRING-NOT-GREATERP  "ABC" "xyzabcdefgh" :START2 3)
#+XCL 3 #-XCL T

(STRING-NOT-GREATERP  "ABC" "xyzabcdefgh" :START2 3 :END2 8)
#+XCL 3 #-XCL T

(STRING-NOT-GREATERP  "ABC" "xyzabcdefgh" :START2 3 :END2 5)
NIL

(STRING-NOT-GREATERP  "abcDEF" "bcdefgh")
#+XCL 0 #-XCL T

(STRING-NOT-GREATERP  "abcDEF" "abcdefgh" :START2 2)
#+XCL 0 #-XCL T

(STRING-NOT-GREATERP  "abcdef" "bngDAbcdef" :START2 9 :END2 5)
error

(STRING-NOT-LESSP  "" "abcdefgh")
NIL

(STRING-NOT-LESSP  "a" "Abcdefgh")
NIL

(STRING-NOT-LESSP  "ABC" "abcdefgh")
NIL

(STRING-NOT-LESSP  "CABc" "abcdefgh")
#+XCL 0 #-XCL T

(STRING-NOT-LESSP  "ABCdefgh" "abcdefgh")
#+XCL 8 #-XCL T

(STRING-NOT-LESSP  "xyzABC" "abcdefgh")
#+XCL 0 #-XCL T

(STRING-NOT-LESSP  "ABC" "xyzabcdefgh")
NIL

(STRING-NOT-LESSP  "ABCdefgh" "abcdefgh" :END1 4)
NIL

(STRING-NOT-LESSP  "xyzABC" "abcdefgh" :START1 3)
NIL

(STRING-NOT-LESSP  "ABC" "xyzabcdefgh" :START2 3)
NIL

(STRING-NOT-LESSP  "ABC" "xyzabcdefgh" :START2 3 :END2 8)
NIL

(STRING-NOT-LESSP  "ABC" "xyzabcdefgh" :START2 3 :END2 5)
#+XCL 2 #-XCL T

(STRING-NOT-LESSP  "abcdefgh" "")
#+XCL 0 #-XCL T

(STRING-NOT-LESSP  "Abcdefgh" "a")
#+XCL 1 #-XCL T

(STRING-NOT-LESSP  "ABCdefgh" "abc")
#+XCL 3 #-XCL T

(STRING-NOT-LESSP  "abCDEfgh" "cabc")
NIL

(STRING-NOT-LESSP  "aBCdefgh" "xyzabc")
NIL

(STRING-NOT-LESSP  "xyzABcdefgh" "abc")
#+XCL 0 #-XCL T

(STRING-NOT-LESSP  "abCDEfgh" "abcdefgh" :END2 4)
#+XCL 4 #-XCL T

(STRING-NOT-LESSP  "xyzABc" "abcdefgh" :START2 3)
#+XCL 0 #-XCL T

(STRING-NOT-LESSP  "ABC" "xyzabcdefgh" :START2 3)
NIL

(STRING-NOT-LESSP  "ABC" "xyzabcdefgh" :START2 3 :END2 8)
NIL

(STRING-NOT-LESSP  "ABC" "xyzabcdefgh" :START2 3 :END2 5)
#+XCL 2 #-XCL T

(STRING-NOT-LESSP  "bCDef" "abcdef")
#+XCL 0 #-XCL T

(STRING-NOT-LESSP  "ABCdefghij" "abcdefgh" :START1 1)
#+XCL 1 #-XCL T

(STRING-NOT-LESSP  "ghIjkl" "xyzabcd" :START2 6 :END2 4)
error

(STRING-NOT-EQUAL  "" "abcdefgh")
#+XCL 0 #-XCL T

(STRING-NOT-EQUAL  "A" "abcdefgh")
#+XCL 1 #-XCL T

(STRING-NOT-EQUAL  "ABc" "abcdefgh")
#+XCL 3 #-XCL T

(STRING-NOT-EQUAL  "cABc" "abcdefgh")
#+XCL 0 #-XCL T

(STRING-NOT-EQUAL  "ABCdefgh" "abcdefgh")
NIL

(STRING-NOT-EQUAL  "xyzABc" "abcdefgh")
#+XCL 0 #-XCL T

(STRING-NOT-EQUAL  "ABC" "xyzabcdefgh")
#+XCL 0 #-XCL T

(STRING-NOT-EQUAL  "ABCdefgh" "abcdefgh" :END1 4)
#+XCL 4 #-XCL T

(STRING-NOT-EQUAL  "xyzaBC" "abcdefgh" :START1 3)
#+XCL 6 #-XCL T

(STRING-NOT-EQUAL  "ABC" "xyzabcdefgh" :START2 3)
#+XCL 3 #-XCL T

(STRING-NOT-EQUAL  "ABC" "xyzabcdefgh" :START2 3 :END2 8)
#+XCL 3 #-XCL T

(STRING-NOT-EQUAL  "ABC" "xyzabcdefgh" :START2 3 :END2 5)
#+XCL 2 #-XCL T

(STRING-NOT-EQUAL  "abcdefgh" "")
#+XCL 0 #-XCL T

(STRING-NOT-EQUAL  "Abcdefgh" "a")
#+XCL 1 #-XCL T

(STRING-NOT-EQUAL  "aBCdefgh" "abc")
#+XCL 3 #-XCL T

(STRING-NOT-EQUAL  "abcdefgh" "cABc")
#+XCL 0 #-XCL T

(STRING-NOT-EQUAL  "abcdefgh" "xyzAbc")
#+XCL 0 #-XCL T

(STRING-NOT-EQUAL  "xyzabcdefgh" "ABC")
#+XCL 0 #-XCL T

(STRING-NOT-EQUAL  "abcdefgh" "abcDEFgh" :END2 4)
#+XCL 4 #-XCL T

(STRING-NOT-EQUAL  "xyzabc" "aBCDefgh" :START2 3)
#+XCL 0 #-XCL T

(STRING-NOT-EQUAL  "abc" "xyzABCdefgh" :START2 3)
#+XCL 3 #-XCL T

(STRING-NOT-EQUAL  "abc" "xyzABCdefgh" :START2 3 :END2 8)
#+XCL 3 #-XCL T

(STRING-NOT-EQUAL  "abc" "xyzABCdefgh" :START2 3 :END2 5)
#+XCL 2 #-XCL T

(STRING/=  "abcdefghi" "uvdEFmgnj" :START1 3 :END1 6 :START2 2 :END2 5)
#+XCL 4 #-XCL T

(STRING/=  "abcdefg" "abcDEfg" :END2 4)
#+XCL 3 #-XCL T

(STRING/=  "abcdef" "abCDef" :START1 1 :END1 4 :START2 4 :END2 1)
error

(STRING-TRIM   (QUOTE (#\SPACE #\TAB #\NEWLINE)) " garbanzo beans
   ")
"garbanzo beans"

(STRING-TRIM   " (*)" " ( *three(siily) words* ) ")
"three(siily) words"

(STRING-TRIM   (QUOTE A) "ababa")
error

(STRING-TRIM   (QUOTE (A)) "ababa")
#+XCL error #+CLISP "ababa"

(STRING-TRIM   "a" "ababa")
"bab"

(STRING-TRIM   "c e" "    ceabceabce    c")
"abceab"

(STRING-TRIM   (QUOTE (#\a)) "abcd")
"bcd"

(STRING-TRIM   (QUOTE (#\a)) "xyzabcd")
"xyzabcd"

(STRING-TRIM   (QUOTE (#\a)) "abcda")
"bcd"

(STRING-LEFT-TRIM   (QUOTE (#\SPACE #\TAB #\NEWLINE)) " garbanzo beans
   ")
"garbanzo beans
   "

(STRING-LEFT-TRIM   " (*)" " ( *three(siily) words* ) ")
"three(siily) words* ) "

(STRING-LEFT-TRIM   (QUOTE A) "ababa")
error

(STRING-LEFT-TRIM   (QUOTE (A)) "ababa")
#+XCL error #+CLISP "ababa"

(STRING-LEFT-TRIM   "a" "ababa")
"baba"

(STRING-LEFT-TRIM   "c e" "    ceabceabce    c")
"abceabce    c"

(STRING-LEFT-TRIM   (QUOTE (#\a)) "abcd")
"bcd"

(STRING-LEFT-TRIM   (QUOTE (#\a)) "xyzabcd")
"xyzabcd"

(STRING-LEFT-TRIM   (QUOTE (#\a)) "abcda")
"bcda"

(STRING-RIGHT-TRIM   (QUOTE (#\SPACE #\TAB #\NEWLINE)) " garbanzo beans
   ")
" garbanzo beans"

(STRING-RIGHT-TRIM   " (*)" " ( *three(siily) words* ) ")
" ( *three(siily) words"

(STRING-RIGHT-TRIM   (QUOTE A) "ababa")
error

(STRING-RIGHT-TRIM   (QUOTE (A)) "ababa")
#+XCL error #+CLISP "ababa"

(STRING-RIGHT-TRIM   "a" "ababa")
"abab"

(STRING-RIGHT-TRIM   "c e" "    ceabceabce    c")
"    ceabceab"

(STRING-RIGHT-TRIM   (QUOTE (#\a)) "abcd")
"abcd"

(STRING-RIGHT-TRIM   (QUOTE (#\a)) "xyzabcd")
"xyzabcd"

(STRING-RIGHT-TRIM   (QUOTE (#\a)) "abcda")
"abcd"

(STRING-UPCASE  "abCD efGh-ij")
"ABCD EFGH-IJ"

(STRING-UPCASE  "abCD efGh-ij" :START 5)
"abCD EFGH-IJ"

(STRING-UPCASE  "abCD efGh-ij" :END 5)
"ABCD efGh-ij"

(STRING-UPCASE  "abCD efGh-ij" :START 1 :END 6)
"aBCD EfGh-ij"

(STRING-UPCASE  "abCD efGh-ij" :START 6 :END 1)
error

(STRING-UPCASE  "abCD efGh-ij" :START 3 :END 3)
"abCD efGh-ij"

(STRING-DOWNCASE  "abCD efGh-ij")
"abcd efgh-ij"

(STRING-DOWNCASE  "abCD efGh-ij" :START 3)
"abCd efgh-ij"

(STRING-DOWNCASE  "abCD efGh-ij" :END 3)
"abcD efGh-ij"

(STRING-DOWNCASE  "abCD efGh-ij" :START 3 :END 3)
"abCD efGh-ij"

(STRING-DOWNCASE  "abCD efGh-ij" :START 1 :END 6)
"abcd efGh-ij"

(STRING-DOWNCASE  "abCD efGh-ij" :START 6 :END 1)
error

(STRING-CAPITALIZE  "abcd def g hi")
"Abcd Def G Hi"

(STRING-CAPITALIZE  "abCd dEf G hi")
"Abcd Def G Hi"

(STRING-CAPITALIZE  "Abcd Def G Hi")
"Abcd Def G Hi"

(STRING-CAPITALIZE  "abcd def g hi" :START 6)
"abcd dEf G Hi"

(STRING-CAPITALIZE  "abcd def g hi" :END 6)
"Abcd Def g hi"

(STRING-CAPITALIZE  "abcd def g hi" :START 2 :END 10)
"abCd Def G hi"

(STRING-CAPITALIZE  "abcd def g hi" :START 10 :END 2)
error

(STRING-CAPITALIZE  "don't")
"Don'T"

(STRING-CAPITALIZE  "DON'T")
"Don'T"

(STRING-CAPITALIZE  "34a 5BC")
"34a 5bc"

(STRING  1)
error

(STRING  (QUOTE A))
"A"

(STRING  #\a)
"a"

(STRING  "abc")
"abc"

(NSTRING-UPCASE  "abCD efGh-ij")   "ABCD EFGH-IJ"

(NSTRING-UPCASE  "abCD efGh-ij" :START 5)   "abCD EFGH-IJ"

(NSTRING-UPCASE  "abCD efGh-ij" :END 5)   "ABCD efGh-ij"

(NSTRING-UPCASE  "abCD efGh-ij" :START6 :END 1)   ERROR

(NSTRING-UPCASE  "abCD efGh-ij" :START 3 :END 3)   "abCD efGh-ij"

(NSTRING-DOWNCASE  "abCD efGh-ij")   "abcd efgh-ij"

(NSTRING-DOWNCASE  "abCD efGh-ij" :START 3)   "abCd efgh-ij"

(NSTRING-UPCASE  "abCD efGh-ij" :START 1 :END 6)   "aBCD EfGh-ij"

(NSTRING-DOWNCASE  "abCD efGh-ij" :END 3)   "abcD efGh-ij"

(NSTRING-DOWNCASE  "abCd efGh-ij" :START 3 :END 3)   "abCd efGh-ij"

(NSTRING-DOWNCASE  "abCd efGh-ij" :START 1 :END 6)   "abcd efGh-ij"

(NSTRING-DOWNCASE  "abCD efGh-ij" :START 6 :END 1)   ERROR

(NSTRING-DOWNCASE  "abCD efGh-ij" :START NIL :END NIL)
#+XCL "abcd efgh-ij" #-XCL ERROR

(NSTRING-UPCASE  "abDC efGh-oj")   "ABDC EFGH-OJ"

(NSTRING-UPCASE "abCD efGh-ij" :START 1 :END 6)   "aBCD EfGh-ij"

(NSTRING-UPCASE  "abCD efGh-fg" :START 1 :END 6)   "aBCD EfGh-fg"

(NSTRING-UPCASE "abCD efGh-ef" :START 3 :END 3)   "abCD efGh-ef"

(NSTRING-UPCASE  "abCD efGh-ef" :START 3 :END 3)   "abCD efGh-ef"

(NSTRING-UPCASE  "abCD efGh-ef" :START 3 :END 3)   "abCD efGh-ef"

(NSTRING-UPCASE  "abCD efGh-ef" :START 3 :END 1)   ERROR

(NSTRING-UPCASE  "abCD efGh-ef" :START NIL :END NIL)
#+XCL "ABCD EFGH-EF" #-XCL ERROR

(NSTRING-DOWNCASE  "saBG efGh-ef")   "sabg efgh-ef"

(NSTRING-DOWNCASE  "dfGV efGh-ef" :START 1 :END 6)   "dfgv efGh-ef"

(NSTRING-DOWNCASE  "fgCD efGf-ef" :START 1 :END 3)   "fgcD efGf-ef"

(NSTRING-DOWNCASE  "dfCF edFg-fg" :START NIL :END NIL)
#+XCL "dfcf edfg-fg" #-XCL ERROR

(NSTRING-DOWNCASE  "fgHG edgf-fg" :START 5 :END 1)   ERROR

(NSTRING-DOWNCASE  "scDF edFG-ef" :START 1)   "scdf edfg-ef"

(NSTRING-DOWNCASE  "fgHG edFG-ef" :END 4)   "fghg edFG-ef"

(NSTRING-CAPITALIZE  "fg hgf fgh")   "Fg Hgf Fgh"

(LET ((X "ABCDEF"))
     (NSTRING-DOWNCASE X)
     X)
"abcdef"

