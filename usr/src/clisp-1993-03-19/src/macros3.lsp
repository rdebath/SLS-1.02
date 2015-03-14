(in-package "LISP")
(export '(ethe nth-value letf letf*))
(in-package "SYSTEM")
;-------------------------------------------------------------------------------
; Wie THE, nur daß auch im compilierten Code der Typtest durchgeführt wird.
(defmacro ethe (typespec form)
  (let ((g (gensym)))
    `(THE ,typespec
       (LET ((,g (MULTIPLE-VALUE-LIST ,form)))
         (IF (SYS::%THE ,g ',typespec)
           (VALUES-LIST ,g)
           (ERROR #+DEUTSCH "Die Form ~S lieferte ~:[keine Werte~;~:*~{~S~^ ; ~}~] ,~@
                             das ist nicht vom Typ ~S."
                  #+ENGLISH "The form ~S yielded ~:[no values~;~:*~{~S~^ ; ~}~] ,~@
                             that's not of type ~S."
                  #+FRANCAIS "La forme ~S a rendu ~:[aucune valeur~;~:*~{~S~^ ; ~}~] ,~@
                              ceci n'est pas de type ~S."
                  ',form ,g ',typespec
) )  ) ) ) )
;-------------------------------------------------------------------------------
; Macro (nth-value n form) == (nth n (multiple-value-list form))
(defmacro nth-value (n form)
  (if (and (integerp n) (>= n 0))
    (if (< n (1- multiple-values-limit))
      (if (= n 0)
        `(PROG1 ,form)
        (let ((resultvar (gensym)))
          (do ((vars (list resultvar))
               (ignores nil)
               (i n (1- i)))
              ((zerop i)
               `(MULTIPLE-VALUE-BIND ,vars ,form
                  (DECLARE (IGNORE ,@ignores))
                  ,resultvar
              ) )
            (let ((g (gensym))) (push g vars) (push g ignores))
      ) ) )
      `(PROGN ,form NIL)
    )
    `(NTH ,n (MULTIPLE-VALUE-LIST ,form))
) )
;-------------------------------------------------------------------------------
; Macro LETF / LETF* wie LET, LET*, nur daß als "Variable" beliebige Places
; (wie bei SETF) zugelassen sind, inklusive VALUES, VALUES-LIST.

; (LETF ((A form)) ...) --> (LET ((A form)) ...)

; (LETF (((CAR A) form)) ...)
;   --> (LET* ((#:G1 A)
;              (#:G2 (CAR #:G1))
;              (#:G3 form))
;         (UNWIND-PROTECT
;           (PROGN (SYSTEM::%RPLACA #:G1 #:G3) ...)
;           (SYSTEM::%RPLACA #:G1 #:G2)
;       ) )

; (LETF (((VALUES A B) form)) ...) --> (MULTIPLE-VALUE-BIND (A B) form ...)

; (LETF (((VALUES (CAR A) (CDR B)) form)) ...)
;   --> (LET* ((#:G1 A)
;              (#:G2 (CAR #:G1))
;              (#:G3 B)
;              (#:G4 (CDR #:G3)))
;         (MULTIPLE-VALUE-BIND (#:G5 #:G6) form
;           (UNWIND-PROTECT
;             (PROGN (SYSTEM::%RPLACA #:G1 #:G5) (SYSTEM::%RPLACD #:G3 #:G6)
;                    ...
;             )
;             (SYSTEM::%RPLACA #:G1 #:G2) (SYSTEM::%RPLACA #:G3 #:G4)
;       ) ) )

; (LETF (((VALUES-LIST A) form)) ...)
;   --> (LET ((A (MULTIPLE-VALUE-LIST form))) ...)

(defmacro LETF* (bindlist &body body &environment env)
  (multiple-value-bind (body-rest declarations)
      (SYSTEM::PARSE-BODY body nil env)
    (let ((declare (if declarations `((DECLARE ,@declarations)) '())))
      (values (expand-LETF* bindlist declare body-rest))
) ) )

; expandiert ein LETF*, liefert die Expansion und
; T, falls diese Expansion mit einem LET* anfaengt, dessen Bindungsliste
; erweitert werden darf.
(defun expand-LETF* (bindlist declare body)
  (if (atom bindlist)
    (if bindlist
      (error #+DEUTSCH "Dotted List im Code von LETF*, endet mit ~S"
             #+ENGLISH "LETF* code contains a dotted list, ending with ~S"
             #+FRANCAIS "Dans le code de LETF*, occurence d'une paire pointée terminée en ~S"
             bindlist
      )
      (values `(LET* () ,@declare ,@body) t)
    )
    (let ((bind (car bindlist)) place form)
      (if (atom bind) (setq place bind form nil)
        (if (and (consp (cdr bind)) (null (cddr bind)))
          (progn
            (setq place (car bind) form (cadr bind))
            (when (and (consp place) (eq (car place) 'VALUES-LIST) (eql (length place) 2))
              (setq place (second place) form `(MULTIPLE-VALUE-LIST ,form))
            )
            (loop
              (if (and (consp place) (eq (car place) 'THE) (eql (length place) 3))
                (setq place (third place) form `(THE ,(second place) ,form))
                (return)
          ) ) )
          (error #+DEUTSCH "Falsche Syntax in Bindung zu LETF* : ~S"
                 #+ENGLISH "illegal syntax in LETF* binding: ~S"
                 #+FRANCAIS "Syntaxe illégale dans une liaison pour LETF* : ~S"
                 bind
      ) ) )
      (multiple-value-bind (rest-expanded flag)
          (expand-LETF* (cdr bindlist) declare body)
        (if (atom place)
          (values
            (if flag
              `(LET* ,(cons (list place form) (cadr rest-expanded))
                 ,@(cddr rest-expanded)
               )
              `(LET* ((,place ,form)) ,@declare ,rest-expanded)
            )
            t
          )
          (if (eq (car place) 'VALUES)
            (if (every #'symbolp place)
              (values
                `(MULTIPLE-VALUE-BIND ,(cdr place) ,form ,@declare ,rest-expanded)
                nil
              )
              (values
                (do ((bindlist nil)
                     (storetemps nil)
                     (stores1 nil)
                     (stores2 nil)
                     (subplacesr (cdr place)))
                    ((atom subplacesr)
                     `(LET* ,(nreverse bindlist)
                        ,@declare
                        (MULTIPLE-VALUE-BIND ,(nreverse storetemps) ,form
                          ,@declare
                          (UNWIND-PROTECT
                            (PROGN ,@(nreverse stores1) ,rest-expanded)
                            ,@(nreverse stores2)
                    ) ) ) )
                  (multiple-value-bind (SM1 SM2 SM3 SM4 SM5)
                      (get-setf-method (pop subplacesr))
                    (setq bindlist
                      (cons (list (first SM3) SM5)
                            (nreconc (mapcar #'list SM1 SM2) bindlist)
                    ) )
                    (let ((storetemp (gensym)))
                      (setq storetemps (cons storetemp storetemps))
                      (setq stores1 (cons (subst storetemp (first SM3) SM4) stores1))
                    )
                    (setq stores2 (cons SM4 stores2))
                ) )
                t
            ) )
            (multiple-value-bind (SM1 SM2 SM3 SM4 SM5) (get-setf-method place)
              (let ((formvar (gensym)))
                (values
                  `(LET* (,.(mapcar #'list SM1 SM2)
                          (,(first SM3) ,SM5)
                          (,formvar ,form))
                     ,@declare
                     (UNWIND-PROTECT
                       (PROGN ,(subst formvar (first SM3) SM4) ,rest-expanded)
                       ,SM4
                   ) )
                  t
            ) ) )
) ) ) ) ) )

(defmacro LETF (bindlist &body body &environment env)
  (multiple-value-bind (body-rest declarations)
      (SYSTEM::PARSE-BODY body nil env)
    (let ((declare (if declarations `((DECLARE ,@declarations)) '()))
          (let-list nil))
      (multiple-value-bind (let*-list let/let*-list uwp-store1 uwp-store2)
          (expand-LETF bindlist)
        ; mehrfach folgendes anwenden:
        ; endet let*-list mit (#:G form) und kommt in let/let*-list (var #:G)
        ; vor, so duerfen beide gestrichen werden, und dafuer kommt (var form)
        ; an den Anfang von let-list.
        (setq let*-list (nreverse let*-list))
        (loop
          (unless (and (consp let*-list)
                       (let ((last (caar let*-list)))
                         (and (symbolp last) (null (symbol-package last))
                              (dolist (bind let/let*-list nil)
                                (when (eq (second bind) last)
                                  (push (list (first bind) (second (car let*-list)))
                                        let-list
                                  )
                                  (setq let/let*-list
                                    (delete last let/let*-list :key #'second
                                            :test #'eq :count 1
                                  ) )
                                  (setq let*-list (cdr let*-list))
                                  (return t)
                  )    ) )    ) )
            (return)
        ) )
        (setq let*-list (nreverse let*-list))
        ; Nun muß folgendes gemacht werden:
        ; 1. Die Bindungen von let*-list mit LETF* aktivieren,
        ; 2. die Bindungen von let-list mit LET aktivieren,
        ; 3. in beliebiger Reihenfolge:
        ;    a. die Bindungen von let/let*-list mit LET oder LET* aktivieren,
        ;    b. die Bindungen von uwp-store1 mit UNWIND-PROTECT aktivieren
        ;       und danach mit uwp-store2 deaktivieren.
        ; Beispielsweise:
#|      `(LETF* ,let*-list
           ,@declare
           (LET ,let-list
             ,@declare
             (LET* ,let/let*-list
               ,@declare
               `(UNWIND-PROTECT (PROGN ,@uwp-store1 ,@body-rest) ,@uwp-store2)
         ) ) )
|#
        (let ((body body-rest) ; eine Formenliste ohne Deklarationen
              (1form nil)) ; zeigt an, ob body aus einer einzigen Form besteht
          (when uwp-store1
            (setq body `((UNWIND-PROTECT (PROGN ,@uwp-store1 ,@body) ,@uwp-store2))
                  1form t
          ) )
          (when let/let*-list
            (setq body `((LET* ,let/let*-list ,@declare ,@body)) 1form t)
          )
          (when let-list
            (setq body `((LET ,let-list ,@declare ,@body)) 1form t)
          )
          (when let*-list
            (setq body `((LETF* ,let*-list ,@declare ,@body)) 1form t)
          )
          (cond ((and (not 1form) (null declare)) `(PROGN ,@body))
                ; ab hier gilt (or 1form declare)
                ((null declare) ; eine Form, keine weiteren Deklarationen
                 (car body)
                )
                ; ab hier gilt (not (null declare))
                ((and 1form (not (eq (caar body) 'unwind-protect)))
                 ; eine Form, faengt mit letf*/let/let* an, hat Deklarationen
                 (car body)
                )
                (t `(LOCALLY ,@declare ,@body))
) ) ) ) ) )

; expandiert ein LETF, liefert:
; eine Bindungsliste fuer LETF*,
; eine Bindungsliste fuer LET/LET* (Reihenfolge der Bindung darin beliebig),
; eine Liste von Bindungsanweisungen, eine Liste von Entbindungsanweisungen
; (beide gleich lang).
(defun expand-LETF (bindlist)
  (if (atom bindlist)
    (if bindlist
      (error #+DEUTSCH "Dotted List im Code von LETF, endet mit ~S"
             #+ENGLISH "LETF code contains a dotted list, ending with ~S"
             #+FRANCAIS "Dans le code de LETF, occurence d'une paire pointée terminée en ~S"
             bindlist
      )
      (values '() '() '() '())
    )
    (let ((bind (car bindlist)) place form)
      (if (atom bind) (setq place bind form nil)
        (if (and (consp (cdr bind)) (null (cddr bind)))
          (progn
            (setq place (car bind) form (cadr bind))
            (when (and (consp place) (eq (car place) 'VALUES-LIST) (eql (length place) 2))
              (setq place (second place) form `(MULTIPLE-VALUE-LIST ,form))
            )
            (loop
              (if (and (consp place) (eq (car place) 'THE) (eql (length place) 3))
                (setq place (third place) form `(THE ,(second place) ,form))
                (return)
          ) ) )
          (error #+DEUTSCH "Falsche Syntax in Bindung zu LETF : ~S"
                 #+ENGLISH "illegal syntax in LETF binding: ~S"
                 #+FRANCAIS "Syntaxe illégale dans une liaison pour LETF : ~S"
                 bind
      ) ) )
      (multiple-value-bind (L1 L2 L3 L4) (expand-LETF (cdr bindlist))
        (if (atom place)
          (let ((g (gensym)))
            (values (cons (list g form) L1) (cons (list place g) L2) L3 L4)
          )
          (if (eq (car place) 'VALUES)
            (if (every #'symbolp place)
              (let ((gs (mapcar #'(lambda (subplace)
                                    (declare (ignore subplace))
                                    (gensym)
                                  )
                                (cdr place)
                   ))   )
                (values
                  (cons (list (cons 'VALUES gs) form) L1)
                  (nconc (mapcar #'list (cdr place) gs) L2)
                  L3
                  L4
              ) )
              (do ((bindlist nil)
                   (storetemps nil)
                   (stores1 nil)
                   (stores2 nil)
                   (subplacesr (cdr place)))
                  ((atom subplacesr)
                   (values
                     (nreconc bindlist
                              (cons (list (cons 'VALUES storetemps) form) L1)
                     )
                     L2
                     (nreconc stores1 L3)
                     (nreconc stores2 L4)
                  ))
                (multiple-value-bind (SM1 SM2 SM3 SM4 SM5)
                    (get-setf-method (pop subplacesr))
                  (setq bindlist
                    (cons (list (first SM3) SM5)
                          (nreconc (mapcar #'list SM1 SM2) bindlist)
                  ) )
                  (let ((storetemp (gensym)))
                    (setq storetemps (cons storetemp storetemps))
                    (setq stores1 (cons (subst storetemp (first SM3) SM4) stores1))
                  )
                  (setq stores2 (cons SM4 stores2))
            ) ) )
            (multiple-value-bind (SM1 SM2 SM3 SM4 SM5) (get-setf-method place)
              (let ((g (gensym)))
                (values
                  `(,.(mapcar #'list SM1 SM2) (,(first SM3) ,SM5) (,g ,form))
                  L2
                  (cons (subst g (first SM3) SM4) L3)
                  (cons SM4 L4)
            ) ) )
) ) ) ) ) )

