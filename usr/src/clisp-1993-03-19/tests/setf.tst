(SETf LI1 '(A (B) ((C) (D)) )  VEC1 '#(0 1 2 3))
#(0 1 2 3)

(setf pa 'old)
old

(psetf pa 'new pao pa)
nil

pa
new

pao
old

(SETF (NTH 1 LI1) (QUOTE UU))
UU

(EVAL (QUOTE LI1))
(A UU ((C) (D)))

(SETF (ELT LI1 1) (QUOTE OO))
OO

(SETF (ELT VEC1 1) (QUOTE OO))
OO

(EVAL (QUOTE LI1))
(A OO ((C) (D)))

(EVAL (QUOTE VEC1))
#(0 OO 2 3)

(SETF (REST LI1) (QUOTE ((WW))))
((WW))

(EVAL (QUOTE LI1))
(A (WW))

(SETF (FIRST LI1) (QUOTE AA))
AA

(FIRST LI1)
AA

(SETF (SECOND LI1) (QUOTE BB))
BB

(EVAL (QUOTE LI1))
(AA BB)

(SETF (THIRD LI1) (QUOTE BB))
ERROR

(EVAL (QUOTE LI1))
(AA BB)


(SETF (REST LI1) (QUOTE (2 3 4 5 6 7 8 9 10)))
(2 3 4 5 6 7 8 9 10)

(SETF (SECOND LI1) 22)
22

(EVAL (QUOTE LI1))
(AA 22 3 4 5 6 7 8 9 10)

(SETF (THIRD LI1) (QUOTE 33))
33

(SETF (FOURTH LI1) (QUOTE 44))
44

(SETF (FIFTH LI1) (QUOTE 55))
55

(SETF (SIXTH LI1) (QUOTE 66))
66

(SETF (SEVENTH LI1) (QUOTE 77))
77

(SETF (EIGHTH LI1) (QUOTE 88))
88

(SETF (NINTH LI1) (QUOTE 99))
99

(SETF (TENTH LI1) (QUOTE 1010))
1010

(EVAL (QUOTE LI1))
(AA 22 33 44 55 66 77 88 99 1010)

(SETF (FIRST LI1) (QUOTE (((A)))))
(((A)))

(SETF (CAAAR LI1) (QUOTE UU))
UU

(CAAAR LI1)
UU

(CAR LI1)
((UU))

(SETF (CAAR LI1) (QUOTE OO))
OO

(EVAL (QUOTE LI1))
((OO) 22 33 44 55 66 77 88 99 1010)

(SETF (CAR LI1) (QUOTE II))
II

(EVAL (QUOTE LI1))
(II 22 33 44 55 66 77 88 99 1010)

(SETF (CDDDR LI1) (QUOTE PP))
PP

(EVAL (QUOTE LI1))
(II 22 33 . PP)

(SETF (CADDR LI1) (QUOTE 333))
333

(EVAL (QUOTE LI1))
(II 22 333 . PP)

(SETF (SVREF VEC1 2) (QUOTE KK))
KK

(EVAL (QUOTE VEC1))
#(0 OO KK 3)

(SETF (GET (QUOTE A) (QUOTE B)) (QUOTE UU))
UU

(GET (QUOTE A) (QUOTE B))
UU

(SETF (GETF (CADR (SETQ XX (QUOTE (AAA (I1 V1 I2 V2))))) (QUOTE I2))

(QUOTE V222))
V222

(EVAL (QUOTE XX))
(AAA (I1 V1 I2 V222))

(GETF (CADR XX) (QUOTE I2))
V222

(GETF (CADR XX) (QUOTE I1))
V1

(SETF (DOCUMENTATION (QUOTE BEISPIEL) (QUOTE TYP1)) "doc 1")
"doc 1"

(SETF (DOCUMENTATION (QUOTE BEISPIEL) (QUOTE TYP2)) "doc 2")
"doc 2"

(DOCUMENTATION (QUOTE BEISPIEL) (QUOTE TYP2))
#+XCL (TYP2 . "doc 2") #-XCL "doc 2"

(SETF (DOCUMENTATION (QUOTE BEISPIEL) (QUOTE TYP2)) "doc 3")
"doc 3"

(DOCUMENTATION (QUOTE BEISPIEL) (QUOTE TYP2))
#+XCL (TYP2 . "doc 3") #-XCL "doc 3"

(symbol-plist 'beispiel)
#+XCL (DOCUMENTATION ((TYP2 . "doc 3") (TYP1 . "doc 1")))
#+CLISP (SYSTEM::DOCUMENTATION-STRINGS (TYP2 "doc 3" TYP1 "doc 1"))

(SETF (SYMBOL-VALUE (QUOTE XX)) (QUOTE VOELLIGNEU))
VOELLIGNEU

(EVAL (QUOTE XX))
VOELLIGNEU

(SETF (SYMBOL-FUNCTION (QUOTE FF))
      (QUOTE (LAMBDA (X) (PRINT X) (QUOTE HELLO))))
#+XCL FF
#-XCL (LAMBDA (X) (PRINT X) 'HELLO)

(FF 5)
HELLO

(defun xx nil 'a)
xx

(progn (setf (symbol-function 'xx1) (symbol-function 'xx)) nil)
nil

(xx1)
a

(setq l '(a 1 c d))
(a 1 c d)

(setf (the integer (cadr l)) 100)
100

l
(a 100 c d)

(progn (setf a (make-hash-table)) t)
t

(setf (gethash 'color a) 'brown)
brown

(gethash 'color a)
brown

(defstruct schiff masse)
schiff

(progn (setf s1 (make-schiff)) nil)
nil

(setf (schiff-masse s1) 500)
500

(schiff-masse s1)
500

(defmacro setf-test (v) `(svref ,v 3))
setf-test

(progn (setf (macro-function 'setf-test1) (macro-function 'setf-test)) nil)
nil

(setf (setf-test vec1) 'oho)
oho

(eval 'vec1)
#(0 OO KK oho)

(setf (setf-test1 vec1) 'hihi)
hihi

(eval 'vec1)
#(0 OO KK hihi)

; (setf (displace ?? (svref vec1 3)) "aha")
; aha

; (eval 'vec1)
; #(0 oo KK aha)

(progn (setf a (make-array '(4 3))) nil)
nil

(aref a 2 2)
#+XCL 0 #+CLISP NIL

(setf (apply #'aref a '(2 2)) 'xxxx)
xxxx

(aref a 2 2)
xxxx

(SETF (AREF '#(A B C) 1) (QUOTE II))
II

(setf b #*101010)
#*101010

(bit b 2)
1

(setf (bit b 2) 0)
0

(bit b 2)
0

(setf (sbit b 2) 1)
1

(sbit b 2)
1

(progn (setf a (make-array 5 :fill-pointer t)) t)
t

(fill-pointer a)
5

(setf (fill-pointer a) 3)
3

(fill-pointer a)
3

(setf str "hose")
"hose"

(setf (char str 0) #\d)
#\d

str
"dose"

(setf str "aaaxxxccc")
"aaaxxxccc"

(setf (subseq str 3 6) "bbb")
"bbb"

str
"aaabbbccc"

(setq x (list 'a 'b 'c))
(a b c)

(shiftf (cadr x) 'z)
b

x
(a z c)

(shiftf (cadr x) (cddr x) 'q)
z

x
(a (c) . q)

(progn (defsetf subseq (sequence start &optional end) (new-sequence)
                       `(progn (replace ,sequence ,new-sequence
                                        :start1 ,start :end1 ,end)
                       ,new-sequence)) t)
t

(setf s "asdfg" (subseq s 1 3) "xy")
"xy"

s
"axyfg"

