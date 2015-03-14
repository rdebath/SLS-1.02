;; testen abschitt 20


; eval

(eval (list 'cdr (car '((quote (a . b)) c))))
b

(makunbound 'x)
x

(eval 'x)
ERROR

(setf x 3)
3

(eval 'x)
3

;constantp

(constantp 2)
T

(constantp #\r)
T

(constantp "max")
T

(constantp '#(110))
#+XCL T #-XCL NIL

(constantp :max)
T
(constantp T)
T
(constantp NIL)
T
(constantp 'PI)
#-CLISP T #+CLISP NIL
(constantp '(quote foo))
T

