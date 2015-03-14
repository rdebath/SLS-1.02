
(SUBST 'A 'B
       '(U B
           (B)
           C))
(U A
   (A)
   C)

(SUBST 'A 'B
       '(U B
           (B)
           C)
       :TEST-NOT
       #'(LAMBDA (X Y)
                (IF (ATOM Y)
                    (EQL X Y)
                    T)))
(A B
   (B . A)
   A . A)

(SUBST 'A 'B
       '(U B
           (B)
           C)
       :TEST
       #'(LAMBDA (X Y)
                (NOT (EQL X Y))))
A

(SUBST 'A 'B
       '(U B
           (B)
           C)
       :TEST-NOT
       #'(LAMBDA (X Y)
                (NOT (EQL X Y))))
(U A
   (A)
   C)

(SUBST 'A 'B
       '(U B
           (B)
           C)
       :TEST-NOT
       #'(LAMBDA (X Y)
                (NOT (EQL X Y)))
       :KEY
       #'(LAMBDA (U)
                (IF (LISTP U)
                    (CAR U))))
(U . A)

(SUBST-IF 'NUMMMER 'NUMBERP
       '((A (7 (V 6)))))
((A (NUMMMER (V NUMMMER))))

(SUBST-IF-NOT 'NUMMMER 'NUMBERP
       '((A (7 (V 6)))))
NUMMMER

(SUBST-IF-NOT 'NUMMMER
       #'(LAMBDA (X)
                (AND (LISTP X)
                     (NUMBERP X)))
       '((A (7 (V 6)))))
NUMMMER

(SUBST-IF-NOT 'NUMMMER
       #'(LAMBDA (X)
                (OR (LISTP X)
                    (NUMBERP X)))
       '((A (7 (V 6)))))
((NUMMMER (7 (NUMMMER 6))))

(NSUBST 'A 'B
       '(U B
           (B)
           C)
       :TEST-NOT
       #'(LAMBDA (X Y)
                (IF (ATOM Y)
                    (EQL X Y)
                    T)))
(A B
   (B . A)
   A . A)

(NSUBST 'A 'B
       '(U B
           (B)
           C)
       :TEST-NOT
       #'(LAMBDA (X Y)
                (NOT (EQL X Y))))
(U A
   (A)
   C)

(NSUBST 'A 'B
       '(U B
           (B)
           C)
       :TEST
       #'(LAMBDA (X Y)
                (NOT (EQL X Y))))
A

(NSUBST-IF 'OO 'NUMBERP
       '(A B C
           (3 (4)
              0)))
(A B C
   (OO (OO)
       OO))

(NSUBST-IF-NOT 'OO 'NUMBERP
       '(A B C
           (3 (4)
              0)))
OO

(NSUBST-IF-NOT 'OO
       #'(LAMBDA (X)
                (OR (ATOM X)
                    (NUMBERP X)))
       '(A B C
           (3 (4)
              0)))
OO

(NSUBST-IF-NOT 'OO
       #'(LAMBDA (X)
                (AND (ATOM X)
                     (NUMBERP X)))
       '(A B C
           (3 (4)
              0)))
OO

(NSUBST-IF-NOT 'OO
       #'(LAMBDA (X)
                (OR (LIST X)
                    (NUMBERP X)))
       '(A B C
           (3 (4)
              0)))
(A B C
   (3 (4)
      0))

(NSUBST-IF-NOT 'OO
       #'(LAMBDA (X)
                (OR (LIST X)
                    (SYMBOLP X)))
       '(A B C
           (3 (4)
              0)))
(A B C
   (3 (4)
      0))

(SUBLIS '((A . A1)
          (B . B1))
       '(A B))
(A1 B1)

(SUBLIS '((A . A1)
          (B . B1))
       '(A B
           (B . C)))
(A1 B1
    (B1 . C))

(SUBLIS '((A . A1)
          (B . B1)
          (NIL . NIL1))
       '(A B
           (B . C)))
(A1 B1
    (B1 . C) .
    NIL1)

(SUBLIS '((A . A1)
          (B . B1)
          (NIL . NIL1))
       '(A B
           (B C)))
(A1 B1
    (B1 C . NIL1) .
    NIL1)

(SUBLIS '((A . A1)
          (B . B1)
          (NIL . NIL1))
       '(A B
           (B C))
       :TEST-NOT 'EQL)
A1

(SUBLIS '((A . A1)
          (B . B1)
          (NIL . NIL1))
       '(A B
           (B C))
       :TEST-NOT
       #'(LAMBDA (X Y)
                (IF (ATOM Y)
                    (EQL X Y))))
A1

(SUBLIS '(((A) .
           UU)
          (A . II))
       '(I (A)
           A))
(I (II)
   II)

(SUBLIS '(((A) . UU) (A . II))
       '(I (A) A)
       :KEY #'(LAMBDA (X) (IF (LISTP X) (CAR X))))
(I II . II) ; KEY wird angewandt auf: X ein Blatt des Baumes

(SUBLIS '(((A) . UU) (A . II))
       '(I (A) A)
       :TEST #'(LAMBDA (X Y) (IF (LISTP Y) (EQL X (CAR Y)))))
#+(or XCL LUCID)       (I II . II) ; X aus der Aliste, Y ein Blatt des Baumes
#+(or CLISP CMU)       (I (UU) UU) ; X ein Blatt, Y aus der Aliste

(NSUBLIS '(((A) . UU) (A . II))
       '(I (A) A)
       :KEY #'(LAMBDA (X) (IF (LISTP X) (CAR X))))
(I II . II) ; KEY wird angewandt auf: X ein Blatt des Baumes

(NSUBLIS '(((A) . UU) (A . II))
       '(I (A) A)
       :TEST #'(LAMBDA (X Y) (IF (LISTP X) (EQUAL X Y))))
(I UU . UU)

(NSUBLIS '(((A) . UU) (A . II))
       '(I (A) A)
       :TEST #'(LAMBDA (X Y) (IF (LISTP Y) (EQUAL X Y))))
(I UU . UU)

(NSUBLIS '(((A) . UU) (A . II))
       '(I (A) A)
       :TEST #'(LAMBDA (X Y) (IF (LISTP Y) (EQL X (CAR Y)))))
#+XCL                  (I II . II) ; X aus der Aliste, Y ein Blatt des Baumes
#+(or CLISP CMU LUCID) (I (UU) UU) ; X ein Blatt, Y aus der Aliste

