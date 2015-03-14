
;; testen der macrofunktionen kapitel 8
;; ------------------------------------


;; 8.1
;macro-function | defmacro


(and (macro-function 'push) T)
T

(and (macro-function 'member) T)
NIL

(defmacro arithmetic-if (test neg-form zero-form pos-form)
          (let ((var (gensym)))
               `(let ((,var ,test))
                     (cond ((< ,var 0) ,neg-form)
                           ((= ,var 0) ,zero-form)
                           (T ,pos-form)))))
arithmetic-if


(and (macro-function 'arithmetic-if) T)
T

(setf x 8)
8

(arithmetic-if (- x 4)(- x)(LIST "ZERO") x)
8


(setf x 4)
4

(arithmetic-if (- x 4)(- x)(LIST "ZERO")x)
("ZERO")


(setf x 3)
3

(arithmetic-if (- x 4)(- x)(LIST "ZERO")x)
-3



(defmacro arithmetic-if (test neg-form &optional zero-form pos-form)
          (let ((var (gensym)))
               `(let ((,var ,test))
                     (cond ((< ,var 0) ,neg-form)
                           ((= ,var 0) ,zero-form)
                           (T ,pos-form)))))
arithmetic-if


(setf x 8)
8

(arithmetic-if (- x 4)(- x))
nil


(setf x 4)
4

(arithmetic-if (- x 4)(- x))
NIL


(setf x 3)
3

(arithmetic-if (- x 4)(- x))
-3

(defmacro halibut ((mouth eye1 eye2)
                   ((fin1 length1)(fin2 length2))
                   tail)
        `(list ,mouth ,eye1 ,eye2 ,fin1 ,length1 ,fin2 ,length2 ,tail))
halibut

(setf m 'red-mouth
      eyes '(left-eye . right-eye)
      f1 '(1 2 3 4 5)
      f2 '(6 7 8 9 0)
      my-favorite-tail '(list of all parts of tail))
(list of all parts of tail)



(halibut (m (car eyes)(cdr eyes))
         ((f1 (length f1))(f2 (length f2)))
         my-favorite-tail)
(RED-MOUTH LEFT-EYE RIGHT-EYE (1 2 3 4 5) 5 (6 7 8 9 0) 5
(LIST OF ALL PARTS OF TAIL))

;; 8.2
; macroexpand | macroexpand-1

