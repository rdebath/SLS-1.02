
(MEMBER 'A
       '((A)
         (B)
         (A)
         (C)))
NIL

(MEMBER 'A
       '((A)
         (B)
         (A)
         (C))
       :KEY 'CAR)
((A)
 (B)
 (A)
 (C))

(MEMBER-IF 'NUMBERP
       '((A)
         (B)
         (3)
         (C))
       :KEY 'CAR)
((3)
 (C))

(MEMBER-IF-NOT 'NUMBERP
       '((8)
         (A)
         (B)
         (3)
         (C))
       :KEY 'CAR)
((A)
 (B)
 (3)
 (C))

(TAILP '(A B)
       '(U A B))
NIL

(TAILP (CDDR (SETQ XX
                   '(U I A B)))
       XX)
T

(TAILP (CDDR (SETQ XX
                   '(U I A B)))
       XX)
T

(ADJOIN 'A
       '(A B C))
(A B C)

(ADJOIN 'A
       '((A)
         B C)
       :TEST 'EQUAL)
(A (A)
   B C)

(ADJOIN 'A
       '((A)
         B C)
       :TEST 'EQUAL)
(A (A)
   B C)

(UNION '(A B C D)
       '(A D I V))
#+XCL (V I A B C D)
#+CLISP (B C A D I V)

(NUNION '(A B C D)
       '(U I B A))
#+XCL (A B C D U I)
#+CLISP (C D U I B A)

(NINTERSECTION '(A B C D)
       '(C D E F G))
(C D)

(NINTERSECTION '(A B C D)
       '(C D E F G)
       :TEST-NOT 'EQL)
(A B C D)

(SET-DIFFERENCE '(A B C D E)
       '(D B E))
#+XCL (C A)
#+CLISP (A C)

(SET-DIFFERENCE '(AUTO ANTON BERTA BERLIN)
       '(A)
       :TEST
       #'(LAMBDA (X Y)
                (EQL (ELT (SYMBOL-NAME X)
                          1)
                     (ELT (SYMBOL-NAME Y)
                          1))))
#+XCL (BERLIN BERTA ANTON AUTO)
#-XCL ERROR

(SET-DIFFERENCE '(ANTON BERTA AUTO BERLIN)
       '(AMERILLA)
       :TEST
       #'(LAMBDA (X Y)
                (EQL (ELT (SYMBOL-NAME X)
                          0)
                     (ELT (SYMBOL-NAME Y)
                          0))))
#+XCL (BERLIN BERTA)
#+CLISP (BERTA BERLIN)

(NSET-DIFFERENCE '(A B C D)
       '(I J C))
(A B D)

(SET-EXCLUSIVE-OR '(A B C D)
       '(C A I L))
#+XCL (D B L I)
#+CLISP (B D I L)

(SET-EXCLUSIVE-OR '(ANTON ANNA EMIL)
       '(BERTA AUTO AUGUST)
       :TEST
       #'(LAMBDA (X Y)
                (EQL (ELT (SYMBOL-NAME X)
                          0)
                     (ELT (SYMBOL-NAME Y)
                          0))))
(EMIL BERTA)

(NSET-EXCLUSIVE-OR '(A B C)
       '(I A D C))
(B I D)

(SUBSETP '(A B)
       '(B U I A C D))
T

(SUBSETP '(A B)
       '(B U I C D))
NIL

(SUBSETP '(A B)
       '(B A U I C D))
T

(SUBSETP '(A B)
       '(A U I C D))
NIL

