;;; testen von backquote


(setf x '(a b c))
(a b c)

`(x ,x ,@x foo ,(cadr  x) bar ,(cdr x) baz ,@(cdr x) ,. x)
(X (A B C) A B C FOO B BAR (B C) BAZ B C A B C)

(read-from-string "`,@x")
ERROR

`(,x . ,x)      ; = (append (list x) x)
((a b c) a b c)


(read-from-string "`(,x . ,@x)")
ERROR


(read-from-string ",x")
ERROR

`#(1 2 3 4)
#(1 2 3 4)

