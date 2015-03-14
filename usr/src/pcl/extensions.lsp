;;;-*-Mode:LISP; Package: PCL; Base:10; Syntax:Common-lisp; -*-

;;;
;;; *************************************************************************
;;;
;;;   File: extensions.lisp.
;;;
;;;     by Trent E. Lange, Effective Date 04-23-92
;;;
;;;
;;;  This file contains a small set of useful extensions to PCL. 
;;;
;;; Permission is granted to any individual or institution to use, copy,
;;; modify and distribute this document.
;;;
;;; Suggestions, bugs, criticism and questions to lange@cs.ucla.edu
;;; *************************************************************************
;;;

(in-package 'pcl)

(eval-when (compile load eval)

(defvar *extensions-exports*
        '(set-standard-instance-access
          set-funcallable-instance-access

          funcallable-instance-slot-value
          set-funcallable-instance-slot-value
          funcallable-instance-slot-boundp
          standard-instance-slot-value
          set-standard-instance-slot-value
          standard-instance-slot-boundp
          structure-instance-slot-value
          set-structure-instance-slot-value
          structure-instance-slot-boundp

          #+pcl-user-instances
          user-instance-slot-value
          #+pcl-user-instances
          set-user-instance-slot-value
          #+pcl-user-instances
          user-instance-slot-boundp

          with-optimized-slots
          with-standard-instance-slots

          method-needs-next-methods-p
          map-all-classes
          finalize-all-classes

          updater
          record-updater))
)

(defclass updater ()
  ((dependent :initarg :dependent :reader dependent)))

(defun record-updater (class dependee dependent &rest initargs)
  (let ((updater
         (apply #'make-instance class :dependent dependent initargs)))
    (add-dependent dependee updater)
    updater))


(defun finalize-all-classes (&optional (root-name 't))
  "Makes sure that all classes are finalized.  If Root-Name is supplied,
   then finalizes Root-Name and all of its subclasses and their subclasses."
  (map-all-classes #'(lambda (class)
                       (unless (class-finalized-p class)
                         (finalize-inheritance class)))
                   root-name))


;;;
;;;
;;;


(defmacro slot-value-from-index (instance wrapper slot-name slots index)
  "Returns instance's slot-value given slot-name's index."
  (once-only (index)
    `(if ,index
         (let ((val (%svref ,slots ,index)))
           (if (eq val ',*slot-unbound*)
               (slot-unbound (wrapper-class ,wrapper) ,instance ,slot-name)
             val))
         (if *safe-to-use-slot-value-wrapper-optimizations-p*
             (get-class-slot-value-1 ,instance ,wrapper ,slot-name)
             (accessor-slot-value ,instance ,slot-name)))))

(defmacro set-slot-value-from-index
          (instance wrapper slot-name slots index new-value)
  "Sets instance's slot-value to new-value given slot-name's index."
  (once-only (index)
    `(if ,index
          (setf (%svref ,slots ,index) ,new-value)
          (if *safe-to-use-set-slot-value-wrapper-optimizations-p*
              (set-class-slot-value-1 ,instance ,wrapper ,slot-name ,new-value)
              (setf (accessor-slot-value ,instance ,slot-name) ,new-value)))))

(defsetf slot-value-from-index set-slot-value-from-index)

(defmacro with-slots-slot-value-from-index
          (instance wrapper slot-name slots index variable-instance)
  "Returns instance's slot-value given slot-name's index."
  (cond
   ((consp wrapper)
    `(let ((wrapper ,wrapper))
       (unless (eq (wrapper-state wrapper) 't)
         (setf wrapper (wrapper-state-trap wrapper ,instance)))
       (with-slots-slot-value-from-index
         ,instance wrapper ,slot-name ,slots ,index ,variable-instance)))
   (variable-instance
    `(let ((,instance ,variable-instance))
       (with-slots-slot-value-from-index
         ,instance ,wrapper ,slot-name ,slots ,index NIL)))
   (T `(slot-value-from-index ,instance ,wrapper ,slot-name ,slots ,index))))

(defmacro set-with-slots-slot-value-from-index
          (instance wrapper slot-name slots index variable-instance new-value)
  "Sets instance's slot-value to new-value given slot-name's index."
  (cond
   ((consp wrapper)
    `(let ((wrapper ,wrapper))
       (unless (eq (wrapper-state wrapper) 't)
         (setf wrapper (wrapper-state-trap wrapper ,instance)))
       (set-with-slots-slot-value-from-index
         ,instance wrapper ,slot-name ,slots ,index ,variable-instance
         ,new-value)))
   (variable-instance
    `(let ((,instance ,variable-instance))
       (set-with-slot-slots-value-from-index
         ,instance ,wrapper ,slot-name ,slots ,index NIL ,new-value)))
   (T
    `(setf (slot-value-from-index ,instance ,wrapper ,slot-name ,slots ,index)
           ,new-value))))

(defsetf with-slots-slot-value-from-index
         set-with-slots-slot-value-from-index)

(defmacro with-slots-slot-value-from-wrapper-and-slots
    (instance slot-name wrapper slots-layout slots variable-instance)
  (cond
   (variable-instance
    `(let ((,instance ,variable-instance))
       (with-slots-slot-value-from-wrapper-and-slots
         ,instance ,slot-name ,wrapper ,slots-layout ,slots NIL)))
   ((consp wrapper)
    `(if *safe-to-use-slot-value-wrapper-optimizations-p*
         (let ((wrapper ,wrapper))
           (unless (eq (wrapper-state wrapper) 't)
             (setf wrapper (wrapper-state-trap wrapper ,instance)))
           (slot-value-from-wrapper-and-slots ,instance ,slot-name
             wrapper ,slots-layout ,slots NIL))
         (accessor-slot-value ,instance ,slot-name)))
   (T
    `(if *safe-to-use-slot-value-wrapper-optimizations-p*
         (slot-value-from-wrapper-and-slots
           ,instance ,slot-name ,wrapper ,slots-layout ,slots NIL)
         (accessor-slot-value ,instance ,slot-name)))))

(defmacro set-with-slots-slot-value-from-wrapper-and-slots
    (instance slot-name wrapper slots-layout slots variable-instance new-value)
  (cond
   (variable-instance
    `(let ((,instance ,variable-instance))
       (set-with-slots-slot-value-from-wrapper-and-slots
         ,instance ,slot-name ,wrapper ,slots-layout ,slots NIL ,new-value)))
   ((consp wrapper)
    `(if *safe-to-use-set-slot-value-wrapper-optimizations-p*
         (let ((wrapper ,wrapper))
           (unless (eq (wrapper-state wrapper) 't)
             (setf wrapper (wrapper-state-trap wrapper ,instance)))
           (setf (slot-value-from-wrapper-and-slots ,instance ,slot-name
                    wrapper ,slots-layout ,slots NIL)
                 ,new-value))
         (setf (accessor-slot-value ,instance ,slot-name) ,new-value)))
   (T
    `(if *safe-to-use-set-slot-value-wrapper-optimizations-p*
         (setf (slot-value-from-wrapper-and-slots
                 ,instance ,slot-name ,wrapper ,slots-layout ,slots NIL)
               ,new-value)
         (setf (accessor-slot-value ,instance ,slot-name) ,new-value)))))

(defsetf with-slots-slot-value-from-wrapper-and-slots
         set-with-slots-slot-value-from-wrapper-and-slots)

(defun tree-memq-p (item form)
  (cond ((consp form)
         (or (tree-memq-p item (car form))
             (tree-memq-p item (cdr form))))
        (T (eq item form)))) 

(defmacro with-optimized-slots (slot-entries instance-form &body body)
  "Optimized version of With-Slots that is faster because it factors out
   functions common to all slot accesses on the instance.  It has two
   extensions to With-Slots: (1) the second value of slot-entries are
   evaluated as forms rather than considered to be hard slot-names, allowing
   access of variable slot-names.  (2) if a :variable-instance keyword is
   the first part of the body, then the instance-form is treated as a variable
   form, which is always expected to return an instance of the same class.
   The value of the keyword must be an instance that is the same class as
   instance-form will always return."
  ;;  E.g. (with-optimized-slots (foo-slot
  ;;                         (foo-slot-accessor     'foo-slot)
  ;;                         (variable-slot-accessor variable-slot))
  ;;                        instance
  ;;                        :instance-form (car instances-of-same-class)
  ;;         (loop for instance in objects-of-same-class
  ;;               as  variable-slot in variable-slots
  ;;               collect (list foo-slot
  ;;                             foo-slot-accessor
  ;;                             variable-slot-accessor)))
  ;;   ==> (loop for instance in objects-of-same-class
  ;;             as  variable-slot in variable-slots
  ;;             collect (list (slot-value instance 'foo-slot)
  ;;                           (slot-value instance 'foo-slot)
  ;;                           (slot-value instance variable-slot)))
  (build-with-optimized-slots-form slot-entries instance-form body))

(defmacro with-standard-instance-slots (slot-entries instance-form &body body)
  "Optimized version of With-Slots that assumes that the instance-form
   evaluates to a standard-instance.  The result is undefined if it does not.
   With-standard-instance-slots is faster than With-Slots because it factors
   out functions common to all slot accesses on the instance.  It has two
   extensions to With-Slots: (1) the second value of slot-entries are
   evaluated as forms rather than considered to be hard slot-names, allowing
   access of variable slot-names.  (2) if a :variable-instance keyword is
   the first part of the body, then the instance-form is treated as a variable
   form, which is always expected to return an instance of the same class.
   The value of the keyword must be an instance that is the same class as
   instance-form will always return."
  (build-with-optimized-slots-form slot-entries instance-form body 'std-instance))

(defun build-with-optimized-slots-form (slot-entries instance-form body
                                        &optional instance-type)
  (let* ((variable-instance
           (if (eq (car body) :variable-instance)
               (prog1
                 (cadr body)
                 (setf body (cddr body)))))
         (hard-accessors
           (let ((collect NIL))
             (dolist (slot-entry slot-entries (nreverse collect))
               (when (and (symbolp slot-entry)
                          (tree-memq-p slot-entry body))
                 (push (cons slot-entry slot-entry) collect))
               (when (and (consp slot-entry)
                          (constantp   (second slot-entry))
                          (tree-memq-p (car slot-entry) body))
                 (push (cons (car slot-entry) (second (second slot-entry)))
                       collect)))))
         (variable-accessors
           (let ((collect NIL))
             (dolist (slot-entry slot-entries (nreverse collect))
               (when (and (consp slot-entry)
                          (not (constantp (second slot-entry)))
                          (tree-memq-p (car slot-entry) body))
                 (push slot-entry collect))))))
    (if *safe-to-use-slot-wrapper-optimizations-p*
        (build-maybe-safe-w-o-s-v hard-accessors variable-accessors
                                  instance-form body variable-instance
                                  instance-type)
        (build-with-accessor-s-v  hard-accessors variable-accessors
                                  instance-form body variable-instance))))

(defun build-maybe-safe-w-o-s-v (hard-accessors variable-accessors
                                 instance-form body variable-instance
                                 instance-type)
  (let* ((instance-string
           (if (symbolp instance-form) (symbol-name instance-form) ""))
         (instance-form-var
           (if (and variable-instance (simple-eval-access-p instance-form))
               instance-form
             (gensym
               (concatenate 'simple-string instance-string "-INSTANCE-FORM"))))
         (prototype-form
           (if variable-instance
               (if (simple-eval-access-p variable-instance)
                   variable-instance
                 (gensym (concatenate 'simple-string "VARIABLE-INSTANCE"
                                                     instance-string)))
               instance-form-var))
         (wrapper-var
           (gensym (concatenate 'simple-string instance-string "-WRAPPER")))
         (slots-var
           (unless variable-instance
             (gensym (concatenate 'simple-string instance-string "-SLOTS"))))
         (type-var
           (when (and variable-instance (not instance-type))
             (gensym (concatenate 'simple-string instance-string "-TYPE"))))
         (type-var-std 1)
         (type-var-fsc 2)
         #+pcl-user-instances
         (type-var-user 3)
         (slot-index-vars
           (mapcar #'(lambda (slot-entry)
                         (list (car slot-entry)
                               (cdr slot-entry)
                               (gensym (concatenate
                                         'simple-string
                                         (if (string= instance-string "")
                                             "INSTANCE-FORM-"
                                           instance-string)
                                         (symbol-name (cdr slot-entry))
                                         "-INDEX"))))
                   (remove-duplicates hard-accessors :key #'cdr)))
         (slots-layout-var
           (gensym (concatenate 'simple-string "SLOTS-LAYOUT-" instance-string)))
         (runtime-slots-form
           (if variable-instance
               (ecase instance-type
                 (std-instance  `(std-instance-slots ,instance-form-var))
                 (fsc-instance  `(fsc-instance-slots ,instance-form-var))
                 #+pcl-user-instances
                 (user-instance `(get-user-instance-slots ,instance-form-var))
                 ((nil)
                  `(case ,type-var
                     (,type-var-std  (std-instance-slots ,instance-form-var))
                     (,type-var-fsc  (fsc-instance-slots ,instance-form-var))
                     #+pcl-user-instances
                     (,type-var-user (get-user-instance-slots ,instance-form-var)))))
                slots-var))
         (runtime-wrapper-form
           (if variable-instance
               (ecase instance-type
                 (std-instance  `(std-instance-wrapper ,instance-form-var))
                 (fsc-instance  `(fsc-instance-wrapper ,instance-form-var))
                 #+pcl-user-instances
                 (user-instance `(get-user-instance-wrapper ,instance-form-var))
                 ((nil)
                  `(case ,type-var
                     (,type-var-std  (std-instance-wrapper ,instance-form-var))
                     (,type-var-fsc  (fsc-instance-wrapper ,instance-form-var))
                     #+pcl-user-instances
                     (,type-var-user (get-user-instance-wrapper ,instance-form-var)))))
               wrapper-var)))
    (declare (type simple-string instance-string)
             (type list          slot-index-vars))
    `(let (,@(unless variable-instance
              `((,instance-form-var ,instance-form)))
           ,@(when (and variable-instance
                        (not (eq prototype-form variable-instance)))
               `((,prototype-form ,variable-instance)))
           ,wrapper-var ,slots-layout-var
           ,@(if variable-instance
                 (if type-var `((type-var 0)))
                 (list slots-var))
           ,@(mapcar #'third slot-index-vars))
       ,@(when type-var `((declare (type index ,type-var))))
       (when *safe-to-use-slot-wrapper-optimizations-p*
         ,@(ecase instance-type
             (std-instance
              `((setf ,wrapper-var (std-instance-wrapper ,prototype-form))
                ,@(unless variable-instance
                   `((setf ,slots-var (std-instance-slots ,prototype-form))))))
             (fsc-instance
              `((setf ,wrapper-var (fsc-instance-wrapper ,prototype-form))
                ,@(unless variable-instance
                   `((setf ,slots-var (fsc-instance-slots ,prototype-form))))))
             #+pcl-user-instances
             (user-instance
              `((setf ,wrapper-var (get-user-instance-wrapper ,prototype-form))
                ,@(unless variable-instance
                   `((setf ,slots-var (get-user-instance-slots ,prototype-form))))))
             ((nil)
             `((cond
                ((std-instance-p ,prototype-form)
                 (setf ,wrapper-var (std-instance-wrapper ,prototype-form))
                 ,(if variable-instance
                      `(setf ,type-var ,type-var-std)
                    `(setf ,slots-var (std-instance-slots ,prototype-form))))
                ((fsc-instance-p ,prototype-form)
                 (setf ,wrapper-var (fsc-instance-wrapper ,prototype-form))
                 ,(if variable-instance
                      `(setf ,type-var ,type-var-fsc)
                    `(setf ,slots-var (fsc-instance-slots ,prototype-form))))
                #+pcl-user-instances
                ((get-user-instance-p ,prototype-form)
                 (setf ,wrapper-var (get-user-instance-wrapper ,prototype-form))
                 ,(if variable-instance
                      `(setf ,type-var ,type-var-user)
                    `(setf ,slots-var (get-user-instance-slots ,prototype-form))))))))
         ,@(if instance-type
               (build-w-s-v-find-slot-indices wrapper-var slots-layout-var
                  prototype-form slot-index-vars)
               `((when ,wrapper-var
                  ,@(build-w-s-v-find-slot-indices wrapper-var slots-layout-var
                       prototype-form slot-index-vars)))))
       (symbol-macrolet
         (,@(mapcar
              #'(lambda (slot-cons)
                  `(,(car slot-cons)
                     (with-slots-slot-value-from-index
                        ,instance-form-var
                        ,runtime-wrapper-form
                        ',(cdr slot-cons)
                        ,runtime-slots-form
                        ,(third (assoc (car slot-cons) slot-index-vars
                                       :test #'eq))
                        ,(when (and variable-instance
                                    (not (eq variable-instance
                                             instance-form-var)))
                           variable-instance))))
              hard-accessors)
          ,@(mapcar
              #'(lambda (variable-cons)
                  `(,(car variable-cons)
                    (with-slots-slot-value-from-wrapper-and-slots
                      ,instance-form-var
                      ,(second variable-cons)
                      ,runtime-wrapper-form
                      ,slots-layout-var
                      ,runtime-slots-form
                      ,(when (and variable-instance
                                  (not (eq variable-instance
                                           instance-form-var)))
                         variable-instance))))
              variable-accessors))
         ,@body))))

(defun build-w-s-v-find-slot-indices (wrapper-var slots-layout-var
                                      prototype-form
                                      slot-index-vars)
  (declare (type list slot-index-vars))
  `((unless (eq (wrapper-state ,wrapper-var) 't)
      (setf ,wrapper-var
            (wrapper-state-trap ,wrapper-var ,prototype-form)))
    (setf ,slots-layout-var (wrapper-instance-slots-layout ,wrapper-var))
    ,@(if (<= (length slot-index-vars) 2)
          (mapcar
            #'(lambda (slot-cons)
                `(setf ,(third slot-cons)
                       (instance-slot-index-from-slots-layout
                         ,slots-layout-var ',(second slot-cons))))
            slot-index-vars)
          ;; More than two slots, so more efficient to search slots-layout-var
          ;; only once, rather than once for each with instance-slot-index.
          (labels
            ((build-comps (slot-vars index)
               (if slot-vars
                   `(if (eq slot-name ',(second (car slot-vars)))
                        (progn
                          (setf ,(third (car slot-vars)) ,index)
                          (if (= matches ,(1- (length slot-index-vars)))
                              (go end-loop)
                            (setf matches (the fixnum (1+ matches)))))
                     ,(build-comps (cdr slot-vars) index)))))
            `((block nil
                (let ((slots-left ,slots-layout-var)
                      (slot-name  NIL)
                      (index      0)
                      (matches    0))
                  (declare (type fixnum index matches))
                  (when slots-left
                    (tagbody
                      begin-instance-slots-loop
                        (setf slot-name (car slots-left))
                        ,(build-comps slot-index-vars 'index)
                        (setf index (the fixnum (1+ index)))
                        (if (null (setf slots-left (cdr slots-left)))
                            (go end-loop))
                        (go begin-instance-slots-loop)
                      end-loop)))))))))

(defun build-with-accessor-s-v (hard-accessors variable-accessors
                                instance-form body variable-instance)
  ;; Build the body for with-optimized-slot-value when it is unsafe
  ;; and accessor-slot-value must be used.
  (let ((instance-form-var
          (if variable-instance instance-form (gensym "INSTANCE-FORM"))))
  `(let (,@(unless variable-instance
            `((,instance-form-var ,instance-form))))
     (symbol-macrolet
       (,@(mapcar
            #'(lambda (slot-cons)
                `(,(car slot-cons)
                  (accessor-slot-value ,instance-form-var
                                       ',(cdr slot-cons))))
            hard-accessors)
        ,@(mapcar
            #'(lambda (variable-cons)
                `(,(car variable-cons)
                  (accessor-slot-value ,instance-form-var
                                       ,(second variable-cons))))
            variable-accessors))
       ,@body))))


#-(or KCL IBCL)
(export *extensions-exports* *the-pcl-package*)

#+(or KCL IBCL)
(mapc 'export (list *extensions-exports*) (list *the-pcl-package*))

