;; packages-test
;  -------------

;;testfile fuer kapitel 11

(packagep  *package*)
T
;;list-all-packages und typtest
(let ((p (list-all-packages)))
     (every #'packagep p))
T

;;11.6 obligate Paketnamen u. deren Abkuerzungen

;;vorhandensein der standardpakete und find-package dafuer

(and (find-package 'lisp) t)
T
(and (find-package 'user) t)
T
(and (find-package 'keyword) t)
T
(and (find-package 'system) t)
T
(and (find-package 'sys) t)
T
(and (find-package "sys") t)
NIL
(and (find-package "sys") t)
NIL
(and (find-package "system") t)
NIL
(and (find-package "SYSTEM") t)
T
(and (find-package "SYS") t)
T

;nicknames
(find "SYS" (package-nicknames 'sys) :test #'string=)
"SYS"

;package-name
(package-name 'sys)
"SYSTEM"
(package-name 'system)
"SYSTEM"
(package-name "USER")
"USER"
(package-name "SYS")
"SYSTEM"


;;; 11.7 anlegen von paketen, export import ...

  ;package-funktionen mit nutzerdefinierten paketen

;falls test1 bereits existiert
(and (find-package 'test1)
     (in-package 'test1)
     (rename-package (find-package 'test1) 'test1-old)
     nil)
nil

;make-package
(package-name (make-package 'test1 :nicknames '(t1 tst1)))
"TEST1"

;package-use-list
;(package-use-list (find-package 'test1))
;("LISP")


(and (in-package 'test1) T)
T


(export  '(test1::test1-y test1::test1-z)(find-package
'"TEST1"))
T

(export  '(test1::test1-a test1::test1-b test1::test1-c) (find-package
'test1))
T

(setf test1-a -2
      test1-b -1
      test1-c  0
      test1-x  1
      test1-y  2
      test1-z  3)
3

;falls test2 bereits existiert
(and
        (find-package 'test2)
        (rename-package (find-package 'test2) 'test2-old)
        nil)
nil

(package-name (in-package 'test2 :nicknames '("T2" "TST2") :use 'test1))
"TEST2"

(lisp:package-name (lisp:find-package 'test2))
"TEST2"

(lisp:package-name lisp:*package*)
"TEST2"

(lisp:import '(lisp:error) (lisp:find-package 'test2))
LISP:T

(lisp:and (lisp:boundp 'test1-x) test1-x)
LISP:NIL

(lisp:unintern 'test1-x)
LISP:T

(eval (read-from-string "(lisp:and (lisp:boundp 'test1:test1-x) test1:test1-x)"))
#+XCL 1 #-XCL ERROR

(lisp:and (lisp:boundp 'test1::test1-x) test1::test1-x)
1

(lisp:and (lisp:boundp 'test1-y) test1-y)
#+XCL LISP:NIL #-XCL 2

(lisp:unintern 'test1-y)
#+XCL LISP:T #-XCL LISP:NIL

(lisp:and (lisp:boundp 'test1:test1-y) test1:test1-y)
#+XCL ERROR #-XCL 2

(lisp:and (lisp:boundp 'test1::test1-y) test1::test1-y)
2

(lisp:import  '(test1::test1-x test1::test1-y) (lisp:find-package 'test2))
LISP:T

(lisp:and (lisp:boundp 'test1-x) test1-x)
1

(eval (read-from-string "(lisp:and (lisp:boundp 'test1:test1-x) test1:test1-x)"))
#+XCL 1 #-XCL ERROR

(lisp:and (lisp:boundp 'test1::test1-x) test1::test1-x)
1

(lisp:and (lisp:boundp 'test1-z) test1-z)
#+XCL LISP:NIL #-XCL 3

(lisp:unintern 'test1-z (lisp:find-package 'test2))
#+XCL LISP:T #-XCL LISP:NIL

(lisp:and (lisp:boundp 'test1:test1-z) test1:test1-z)
#+XCL ERROR #-XCL 3

test1::test1-z
3

(lisp:unexport  '(test1::test1-x test1::test1-y) (lisp:find-package 'test1))
LISP:T

(lisp:and (lisp:boundp 'test1-x) test1-x)
1

(lisp:and (lisp:boundp 'test1-y) test1-y)
#+XCL LISP:NIL #-XCL 2

(lisp:unintern 'test1-x (lisp:find-package 'test2))
LISP:T

(eval (read-from-string "test1:test1-x"))
ERROR

test1::test1-x
1

test1-z
3

(lisp:unintern 'test1-z (lisp:find-package 'test2))
#+XCL LISP:T #-XCL LISP:NIL

test1:test1-z
3

test1::test1-z
3

(lisp:import 'test1::test1-z (lisp:find-package 'test2))
LISP:T

test1-z
3

test1:test1-z
3

test1::test1-z
3

test1-c
#+XCL ERROR #-XCL 0

(lisp:unintern 'test-c (lisp:find-package 'test2))
LISP:T

test1:test1-c
0

test1::test1-c
0

(lisp:import '(test1::test1-a test1::test1-b test1::test1-c)
             (lisp:find-package 'test2))
LISP:T

test1-c
0

test1:test1-c
0

test1::test1-c
0

(lisp:eq 'test1-c 'test1::test1-c)
LISP:T

  ;Ende nutzerdefinierte Pakete

;; test in standardmaessig vorgegebenen paketen

; export | import | unintern

(lisp:and (lisp:in-package 'user) lisp:T)
LISP:T

(setf x 1 y 2 z 3)
3

(and(in-package 'editor)T)
T

(unintern 'x)
T

(unintern 'y)
T

(unintern 'z)
T

user::x
1

(eval (read-from-string "user:x"))
ERROR

x
error

(eq 'x 'user::x)
NIL

(unintern 'x)
T

(export '(user::x user::y) (find-package 'user))
T

user::x
1

user:x
1

x
error

(unintern 'x)
T

(import 'user:x (find-package 'editor))
T

x
1

(eq 'x 'user::x)
t

(eq 'x 'user:x)
t

(eq 'editor::x 'user::x)
t

;; unexport

(and (in-package 'user) T)
T

(unexport 'y)
T

(and (in-package 'editor) T)
T

y
ERROR

(eval (read-from-string "user:y"))
ERROR

user::y
2

;; shadowing-import -- zunaechst ohne geerbte symbole!!

(and (in-package 'user)(package-name *package*))
"USER"

(setf d 4 e 5 f 6 y 111 x 222)
222

(export '(user::a user::b user::c user::y user::x) (find-package 'user))
T

(import '(user::a user::b user::c user::y) (find-package 'editor))
ERROR

(and (make-package 'shadow-test)(in-package 'shadow-test)t)
T

(setf x 'shadow-test)
shadow-test

(shadowing-import '(user::d user::e user::f user::x)(find-package 'shadow-test))
T

x
222

(eq user::x x)
T

; shadow

(shadow '(e f) (find-package 'shadow-test))
t

(setf e 'shadow-test-e)
shadow-test-e

(eq 'e 'user::e)
#+XCL nil #-XCL t

e
shadow-test-e

(eval (read-from-string "user:e"))
error

user::e
#+XCL 5 #-XCL shadow-test-e

; use-package | unuse-package

(and (make-package 'use-test)(in-package 'use-test) t)
t

(use-package '(user))
T

user::d
4

(eval (read-from-string "user:d"))
#+XCL 4 #-XCL ERROR

d
ERROR

(unuse-package 'user)
T

user::d
4

(eval (read-from-string "user:d"))
ERROR

d
ERROR

;make-package mit beutzung eines paketes, dass geerbte symbole enthaelt

(and (make-package 'inherit :nicknames '(inh i) )(in-package 'inherit) T)
T

(setf a 'inherita b 'inheritb)
inheritb

(export '(a b) (find-package 'inherit))
T

(and (make-package 'inherit1 :use 'inherit)(in-package 'inherit1) T)
T

a
inherit::inherita

b
inherit::inheritb

(lisp:setf c 'inherit1c)
inherit1c

(lisp:and (lisp:make-package 'inherit2 :use 'inherit1)
          (lisp:in-package 'inherit2) lisp:T)
LISP:T

a
#+XCL inherita #-XCL LISP:ERROR

b
#+XCL inheritb #-XCL LISP:ERROR

c
#+XCL inherit1c #-XCL LISP:ERROR

(eval (read-from-string "(lisp:eq 'c 'inherit1:c)"))
#+XCL LISP:T #-XCL LISP:ERROR

(eval (read-from-string "(lisp:eq 'a 'inherit:a)"))
#+XCL LISP:T #-XCL LISP:ERROR

(eval (read-from-string "(lisp:eq 'b 'inherit:b)"))
#+XCL LISP:T #-XCL LISP:ERROR

(lisp:eq 'c 'inherit1::c)
#+XCL LISP:T #-XCL LISP:NIL

(lisp:eq 'a 'inherit::a)
#+XCL LISP:T #-XCL LISP:NIL

(lisp:eq 'b 'inherit::b)
#+XCL LISP:T #-XCL LISP:NIL

;find-all-symbols

(lisp:and (lisp:in-package 'user) lisp:T)
LISP:T

; find-all-symbols fehlerhaft
(and (member 'user::x (setf s (find-all-symbols 'x)))T)
T

(eval (read-from-string "(and (member 'editor:x s) t)"))
#+XCL T #-XCL ERROR

(and (member 'user::x (setf s1 (find-all-symbols 'x)))T)
T

(set-difference s s1)
nil                              ;Ende Kommentar

;do-symbols | do-external-symbols | do-all-symbols

(setf sym nil
      esym nil
      asym nil
)
nil

(do-symbols (s (find-package 'user))(push (symbol-name s) sym))
nil

(do-external-symbols (s (find-package 'user))(push (symbol-name s) esym))
nil

(do-all-symbols (s)(push (symbol-name s) asym))
nil

(find "ESYM" sym :test #'string=)
"ESYM"

(find "ESYM" esym :test #'string=)
nil

(find "LAMBDA-LIST-KEYWORDS" esym :test #'string=)
#+XCL "LAMBDA-LIST-KEYWORDS" #-XCL NIL

;(count "LAMBDA-LIST-KEYWORDS" asym :test #'string=)
;T                                                  ;viel zu lang

; modules | provide | (require nicht getestet !)

(and *modules* T)
#+XCL T #+CLISP NIL

(and (provide 'provide-test) t)
t

(find "PROVIDE-TEST" *modules* :test #'string=)
"PROVIDE-TEST"

(format t "End of file")
nil

