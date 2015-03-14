(progn (in-package 'user)nil)
NIL
;; Test der neuen Valuezelle

;;; 1. ungebundenes Symbol

(defun testvar (var)
   (list (boundp var)                                ; gebunden
         (if (boundp var) (symbol-value var) nil)    ; Wert/nil
         (constantp var)                             ; Konstante
         #+XCL (eq (sys::%p-get-cdr var 0) sys::%cdr-specsym) ; specvar
         #+CLISP (and (sys::special-variable-p var) (not (constantp var))) ; specvar
         (fboundp var)                               ; funktion. Eigenschaft
         (and (fboundp var) (macro-function var) t)  ; Macro?
         (and (fboundp var) (special-form-p var) t)  ; Spezialform?
         #-CLISP (and (symbol-plist var) t)          ; p-Liste?
         #+CLISP (and (or (get var 'i1) (get var 'i2) (get var 'i3)) t) ; p-Liste?
         (get var 'i1)                               ; i1
         (get var 'i2)                               ; i2
         (get var 'i3)                               ; i3
)  )
testvar

(defun clrvar (var)
   #+XCL(subr 84 ;sys::%p-set-cdr-content
              var 0 (sys::%p-get-content 'sys::%void-value 0) 0)
   #+CLISP (progn (makunbound var) (fmakunbound var)
                  (setf (symbol-plist var) '())
           )
   var)
clrvar

#+CLISP (progn (setf (symbol-function 'sys::setf-get) (symbol-function 'sys::%put)) t)
#+CLISP T

;;; Begin Breitentest

(clrvar 'v1)
v1

;;;; value - umbinden - macro - umbinden - props - umbinden

;;; value

(testvar 'v1)
;geb val konst svar func mac spec plist i1  i2  i3
(nil nil nil   nil  nil  nil nil  nil   nil nil nil)

(setq v1 'val)
val

(testvar 'v1)
;geb val konst svar func mac spec plist i1  i2  i3
(t   val nil   nil  nil  nil nil  nil   nil nil nil)

;;; umbinden

(makunbound 'v1)
v1

(testvar 'v1)
;geb val konst svar func mac spec plist i1  i2  i3
(nil nil nil   nil  nil  nil nil  nil   nil nil nil)

(setq v1 'val2)
val2

(testvar 'v1)
;geb val  konst svar func mac spec plist i1  i2  i3
(t   val2 nil   nil  nil  nil nil  nil   nil nil nil)

;;; macro

(defmacro v1 (x) (list 'quote x))
v1

(testvar 'v1)
;geb val  konst svar func mac spec plist i1  i2  i3
(t   val2 nil   nil  t    t   nil  nil   nil nil nil)

;;; umbinden

(fmakunbound 'v1)
v1

(testvar 'v1)
;geb val  konst svar func mac spec plist i1  i2  i3
(t   val2 nil   nil  nil  nil nil  nil   nil nil nil)

(defmacro v1 (x) (list 'quote (list x x)))
v1

(v1 33)
(33 33)

(testvar 'v1)
;geb val  konst svar func mac spec plist i1  i2  i3
(t   val2 nil   nil  t    t   nil  nil   nil nil nil)

(makunbound 'v1)
v1

(testvar 'v1)
;geb val  konst svar func mac spec plist i1  i2  i3
(nil nil  nil   nil  t    t   nil  nil   nil nil nil)

(setq v1 'val3)
val3

(testvar 'v1)
;geb val  konst svar func mac spec plist i1  i2  i3
(t   val3 nil   nil  t    t   nil  nil   nil nil nil)

;;; props

(sys::setf-get 'v1 'i1 11)
11

(sys::setf-get 'v1 'i2 22)
22

(sys::setf-get 'v1 'i3 33)
33

(testvar 'v1)
;geb val  konst svar func mac spec plist i1  i2  i3
(t   val3 nil   nil  t    t   nil  t     11  22  33)

;;; umbinden

(remprop 'v1 'i2)
t
(remprop 'v1 'i1)
t
(remprop 'v1 'i3)
t
(fmakunbound 'v1)
v1
(makunbound 'v1)
v1

(testvar 'v1)
;geb val  konst svar func mac spec plist i1  i2  i3
(nil nil  nil   nil  nil  nil nil  nil   nil nil nil)

(sys::setf-get 'v1 'i1 99)
99
(defmacro v1 (x) (list 'quote (list x x x)))
v1
(v1 a)
(a a a)
(setq v1 'val4)
val4

(testvar 'v1)
;geb val  konst svar func mac spec plist i1  i2  i3
(t   val4 nil   nil  t    t   nil  t     99  nil nil)

;;; --- Ende Test1 -----

(clrvar 'v2)
v2

;;; specvar - props - rebind - function

(defvar v2 'v2a)
v2

(testvar 'v2)
;geb val  konst svar func mac spec plist i1  i2  i3
(t   v2a  nil   t    nil  nil nil  nil   nil nil nil)

(sys::setf-get 'v2 'i3 33)
33
(sys::setf-get 'v2 'i2 22)
22
(sys::setf-get 'v2 'i1 11)
11

(testvar 'v2)
;geb val  konst svar func mac spec plist i1  i2  i3
(t   v2a  nil   t    nil  nil nil  t     11  22  33)

;;; rebind

(makunbound 'v2)
v2
(remprop 'v2 'i1)
t
(remprop 'v2 'i2)
t
(remprop 'v2 'i3)
t

(testvar 'v2)
;geb val  konst svar func mac spec plist i1  i2  i3
#+XCL
(nil nil  nil   nil  nil  nil nil  nil   nil nil nil)
#+CLISP
(nil nil  nil   t    nil  nil nil  nil   nil nil nil)

(defvar v2 'v2b)
v2
(sys::setf-get 'v2 'i1 111)
111
(sys::setf-get 'v2 'i2 222)
222
(sys::setf-get 'v2 'i3 333)
333

(testvar 'v2)
;geb val  konst svar func mac spec plist i1  i2  i3
(t   v2b  nil   t    nil  nil nil  t     111 222 333)

;;; function

(defun v2 (x) (list x x))
v2
(v2 44)
(44 44)

(testvar 'v2)
;geb val  konst svar func mac spec plist i1  i2  i3
(t   v2b  nil   t    t    nil nil  t     111 222 333 )


(clrvar 'v3)
v3

;;;;; function - con - rebind - prop

;;; function

(defun v3 (x y) (list x y))
v3

(testvar 'v3)
;geb val  konst svar func mac spec plist i1  i2  i3
(nil nil  nil   nil  t    nil nil  nil   nil nil nil)

;;; constant

(defconstant v3 99)
v3

v3
99
(v3 'a 'b)
(a b)

(testvar 'v3)
;geb val  konst svar func mac spec plist i1  i2  i3
(t    99  t     nil  t    nil nil  nil   nil nil nil)

;;; rebind

(makunbound 'v3)
#+XCL v3 #+CLISP ERROR
(fmakunbound 'v3)
v3

#+XCL
(testvar 'v3)
#+XCL
;geb val  konst svar func mac spec plist i1  i2  i3
(nil nil  nil   nil  nil  nil nil  nil   nil nil nil)

(defconstant v3 999)
v3

(defun v3 (x) (list x x))
v3
(v3 'c)
(c c)
v3
999

(testvar 'v3)
;geb val  konst svar func mac spec plist i1  i2  i3
(t   999  t     nil  t    nil nil  nil   nil nil nil)

;;;defparameter

(defparameter var33)
ERROR

(defparameter var3 99)
var3

var3
99

(testvar 'var3)
;geb val  konst svar func mac spec plist i1  i2  i3
(t    99  nil   T    nil  nil nil  nil   nil nil nil)

;;; rebind

(makunbound 'var3)
var3

(testvar 'var3)
;geb val  konst svar func mac spec plist i1  i2  i3
#+XCL
(nil nil  nil   nil  nil  nil nil  nil   nil nil nil)
#+CLISP
(nil nil  nil   t    nil  nil nil  nil   nil nil nil)

;;; props

(sys::setf-get 'v3 'i2 222)
222

(sys::setf-get 'v3 'i1 111)
111

(testvar 'v3)
;geb val  konst svar func mac spec plist i1  i2  i3
(t   999  t     nil  t    nil nil  t     111 222 nil)


(clrvar 'v4)
v4

;;;;  function - rebind - prop - rebind - specvar

(defun v4 (x) x)
v4
(v4 55)
55

(testvar 'v4)
;geb val  konst svar func mac spec plist i1  i2  i3
(nil nil  nil   nil  t    nil nil  nil   nil nil nil)

;;; rebind

(fmakunbound 'v4)
v4
(testvar 'v4)
;geb val  konst svar func mac spec plist i1  i2  i3
(nil nil  nil   nil  nil  nil nil  nil   nil nil nil)

(defun v4 (x) (list x))
v4
(v4 88)
(88)

(testvar 'v4)
;geb val  konst svar func mac spec plist i1  i2  i3
(nil nil  nil   nil  t    nil nil  nil   nil nil nil)

(sys::setf-get 'v4 'i1 11)
11
(sys::setf-get 'v4 'i2 22)
22

(testvar 'v4)
;geb val  konst svar func mac spec plist i1  i2  i3
(nil nil  nil   nil  t    nil nil  t     11  22  nil)

;;; rebind

(fmakunbound 'v4)
v4
(remprop 'v4 'i1)
t
(remprop 'v4 'i2)
t
(testvar 'v4)
;geb val  konst svar func mac spec plist i1  i2  i3
(nil nil  nil   nil  nil  nil nil  nil   nil nil nil)

(defun v4 (x) (list x x x))
v4
(v4 44)
(44 44 44)
(sys::setf-get 'v4 'i2 222)
222
(sys::setf-get 'v4 'i3 333)
333

(testvar 'v4)
;geb val  konst svar func mac spec plist i1  i2  i3
(nil nil  nil   nil  t    nil nil  t     nil 222 333)

(defvar v4 'v4-value)
v4

(testvar 'v4)
;geb val     konst svar func mac spec plist i1  i2  i3
(t  v4-value nil   t    t    nil nil  t     nil 222 333)

(clrvar 'v5)
v5

;;;;; prop - rebind - con - rebind - fun

(sys::setf-get 'v5 'i1 1)
1
(sys::setf-get 'v5 'i2 2)
2

(testvar 'v5)
;geb val  konst svar func mac spec plist i1  i2  i3
(nil nil  nil   nil  nil  nil nil  t     1   2  nil)

;;; rebind

(remprop 'v5 'i1)
t
(remprop 'v5 'i2)
t

(testvar 'v5)
;geb val  konst svar func mac spec plist i1  i2  i3
(nil nil  nil   nil  nil  nil nil  nil   nil nil nil)

(sys::setf-get 'v5 'i1 11)
11
(sys::setf-get 'v5 'i2 22)
22

(testvar 'v5)
;geb val  konst svar func mac spec plist i1  i2  i3
(nil nil  nil   nil  nil  nil nil  t     11  22  nil)

;;; con

(defconstant v5 '123)
v5

(testvar 'v5)
;geb val  konst svar func mac spec plist i1  i2  i3
(t   123  t     nil  nil  nil nil  t     11  22  nil)

;;; rebind

(makunbound 'v5)
#+XCL v5 #+CLISP ERROR
(remprop 'v5 'i2)
t
(remprop 'v5 'i1)
t

#+XCL
(testvar 'v5)
#+XCL
;geb val  konst svar func mac spec plist i1  i2  i3
(nil nil  nil   nil  nil  nil nil  nil   nil nil nil)

;;; das ging schief !!

(defconstant v5 321)
v5
(sys::setf-get 'v5 'i3 333)
333
(sys::setf-get 'v5 'i2 222)
222

(testvar 'v5)
;geb val  konst svar func mac spec plist i1  i2  i3
(t   321  t     nil  nil  nil nil  t     nil 222 333)

(defun v5 (x) x)
v5

(v5 666)
666

(testvar 'v5)
;geb val  konst svar func mac spec plist i1  i2  i3
(t   321  t     nil  t    nil nil  t     nil 222 333)

(clrvar 'v6)
v6

;;;;; prop mac con

(sys::setf-get 'v6 'i1 1)
1
(sys::setf-get 'v6 'i3 3)
3

(testvar 'v6)
;geb val  konst svar func mac spec plist i1  i2  i3
(nil nil  nil   nil  nil  nil nil  t     1   nil 3)

(defmacro v6 (x) (list 'quote x))
v6
(v6 a)
a
(testvar 'v6)
;geb val  konst svar func mac spec plist i1  i2  i3
(nil nil  nil   nil  t    t   nil  t     1   nil 3)

(defconstant v6 234)
v6

(testvar 'v6)
;geb val  konst svar func mac spec plist i1  i2  i3
(t   234  t     nil  t    t   nil  t     1   nil 3)


