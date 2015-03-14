(setf string "test-pathname.abc" symbol 'test-pathname.abc)
test-pathname.abc

;;pathname -mögl. Argumenttypen: pathname,string,symbol,stream
;;         -resultat: pathname

(SETF PATHSTRING (PATHNAME STRING))
#+XCL
#S(PATHNAME SYSTEM::HOST NIL
SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST-PATHNAME"
TYPE "ABC" SYSTEM::VERSION NIL)
#+CLISP
#S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
            :NAME "test-pathname" :TYPE "abc" :VERSION NIL)

(SETF PATHSYMBOL (PATHNAME symbol))
#+XCL
#S(PATHNAME SYSTEM::HOST
NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME
"TEST-PATHNAME" TYPE "ABC" SYSTEM::VERSION NIL)
#+CLISP
#S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
            :NAME "test-pathname" :TYPE "abc" :VERSION NIL)

(SETF PATHPATH (PATHNAME PATHSYMBOL))
#+XCL
#S(PATHNAME SYSTEM::HOST NIL
SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST-PATHNAME"
TYPE "ABC" SYSTEM::VERSION NIL)
#+CLISP
#S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
            :NAME "test-pathname" :TYPE "abc" :VERSION NIL)

(SETF STREAM (OPEN STRING :DIRECTION :OUTPUT)
      a nil)
nil

;(SETF PATHSTREAM (PATHNAME STREAM))
;"test-pathname.lsp"

(MAPCAR (FUNCTION PATHNAMEP)
        (LIST PATHSTRING PATHSYMBOL PATHPATH ;PATHSTREAM
))
(T T T ;T
)


;; funktion truename liefert filename fuer pathname oder stream
;;                   einen Pfadnamen
;
;(MAPCAR (FUNCTION TRUENAME) (LIST PATHSTRING PATHSYMBOL PATHPATH STREAM
;                                                               ;PATHSTREAM
;                                                                 ))
;  ERROR



(PARSE-NAMESTRING STRING)
#+XCL
#S(PATHNAME SYSTEM::HOST NIL
SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST-PATHNAME"
TYPE "ABC" SYSTEM::VERSION NIL)
#+CLISP
#S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
            :NAME "test-pathname" :TYPE "abc" :VERSION NIL)

(PARSE-NAMESTRING SYMBOL)
#+XCL
#S(PATHNAME SYSTEM::HOST NIL
SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST-PATHNAME"
TYPE "ABC" SYSTEM::VERSION NIL)
#+CLISP
#S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
            :NAME "test-pathname" :TYPE "abc" :VERSION NIL)

#+XCL
(PARSE-NAMESTRING "bab:test-pathname.abc")
#+XCL
#S(PATHNAME SYSTEM::HOST NIL
SYSTEM::DEVICE "$1$DUA70" DIRECTORY "43.BABYLON.REL2" SYSTEM::NAME
"TEST-PATHNAME" TYPE "ABC" SYSTEM::VERSION NIL)

#+XCL
(PARSE-NAMESTRING "bab:test-pathname.abc;3")
#+XCL
#S(PATHNAME SYSTEM::HOST NIL
SYSTEM::DEVICE "$1$DUA70" DIRECTORY "43.BABYLON.REL2" SYSTEM::NAME
"TEST-PATHNAME" TYPE "ABC" SYSTEM::VERSION 3)

(PARSE-NAMESTRING PATHSTRING)
#+XCL
#S(PATHNAME SYSTEM::HOST NIL SYSTEM::DEVICE
"DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST-PATHNAME" TYPE "ABC"
SYSTEM::VERSION NIL)
#+CLISP
#S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
            :NAME "test-pathname" :TYPE "abc" :VERSION NIL)

(PARSE-NAMESTRING "test-pathname.abc" NIL)
#+XCL
#S(PATHNAME SYSTEM::HOST NIL
SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST-PATHNAME"
TYPE "ABC" SYSTEM::VERSION NIL)
#+CLISP
#S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
            :NAME "test-pathname" :TYPE "abc" :VERSION NIL)

#+XCL
(PARSE-NAMESTRING "sirius::disk00$abt43:[heicking]test-pathname.abc")
#+XCL
#S(PATHNAME
SYSTEM::HOST "SIRIUS" SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "HEICKING"
SYSTEM::NAME "TEST-PATHNAME" TYPE "ABC" SYSTEM::VERSION NIL)

#+XCL
(PARSE-NAMESTRING "sirius::disk00$abt43:[heicking]test-pathname.abc" "sirius")
#+XCL
#S(PATHNAME
SYSTEM::HOST "SIRIUS" SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "HEICKING"
SYSTEM::NAME "TEST-PATHNAME" TYPE "ABC" SYSTEM::VERSION NIL)

#+XCL
(PARSE-NAMESTRING "sirius::disk00$abt43:[heicking]test-pathname.abc" "orion")
#+XCL
ERROR

(PARSE-NAMESTRING "abc.123" NIL NIL :START 0 :END 5)
#+XCL
#S(PATHNAME SYSTEM::HOST
NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "ABC" TYPE
"1" SYSTEM::VERSION NIL)
#+CLISP
#S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
            :NAME "abc" :TYPE "1" :VERSION NIL)

(PARSE-NAMESTRING "abc.123" NIL NIL :START 2 :END 5)
#+XCL
#S(PATHNAME SYSTEM::HOST
NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "C" TYPE "1"
SYSTEM::VERSION NIL)
#+CLISP
#S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
            :NAME "c" :TYPE "1" :VERSION NIL)

#+XCL
(PARSE-NAMESTRING "babylon" NIL NIL :START 0 :END 3)
#+XCL
#S(PATHNAME SYSTEM::HOST
NIL SYSTEM::DEVICE "$1$DUA70" DIRECTORY "43.BABYLON.REL2" SYSTEM::NAME NIL TYPE
NIL SYSTEM::VERSION NIL)

#+XCL
(PARSE-NAMESTRING "babylon" NIL NIL :START 0 :END 7)
#+XCL
#S(PATHNAME SYSTEM::HOST
NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "BABYLON"
TYPE NIL SYSTEM::VERSION NIL)

#+XCL
(PARSE-NAMESTRING "babylon" NIL *DEFAULT-PATHNAME-DEFAULTS* :START 0 :END 7)
#+XCL
#S(PATHNAME
SYSTEM::HOST NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME
"BABYLON" TYPE NIL SYSTEM::VERSION NIL)

*DEFAULT-PATHNAME-DEFAULTS*
#+XCL
#S(PATHNAME SYSTEM::HOST NIL SYSTEM::DEVICE NIL
DIRECTORY NIL SYSTEM::NAME NIL TYPE "lsp" SYSTEM::VERSION :NEWEST)
#+CLISP
#S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
            :NAME NIL :TYPE NIL :VERSION NIL)

#+XCL
(PARSE-NAMESTRING "babylon" NIL *DEFAULT-PATHNAME-DEFAULTS* :START 0 :END 3)
#+XCL
#S(PATHNAME
SYSTEM::HOST NIL SYSTEM::DEVICE "$1$DUA70" DIRECTORY "43.BABYLON.REL2"
SYSTEM::NAME NIL TYPE NIL SYSTEM::VERSION NIL)

;(PARSE-NAMESTRING "babylon.c.c" NIL NIL :JUNK-ALLOWED T)
;#S(PATHNAME
;SYSTEM::HOST NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME
;"BABYLON" TYPE "C" SYSTEM::VERSION NIL)

;(PARSE-NAMESTRING "babylon;c.c" NIL NIL :JUNK-ALLOWED T)
;#S(PATHNAME
;SYSTEM::HOST NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME
;"BABYLON" TYPE NIL SYSTEM::VERSION NIL)

#+XCL
(PARSE-NAMESTRING "babylon;c.c" NIL NIL :JUNK-ALLOWED NIL)
#+XCL
ERROR

#+XCL
(PARSE-NAMESTRING "babylon.c.c" NIL NIL :JUNK-ALLOWED NIL)
#+XCL
ERROR

#+XCL
(PARSE-NAMESTRING "babylon.c;c" NIL NIL :JUNK-ALLOWED NIL)
#+XCL
ERROR

#+XCL
(PARSE-NAMESTRING "babylon.c;" NIL NIL :JUNK-ALLOWED NIL)
#+XCL
#S(PATHNAME
SYSTEM::HOST NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME
"BABYLON" TYPE "C" SYSTEM::VERSION NIL)

#+XCL
(PARSE-NAMESTRING "babylon.c;5" NIL NIL :JUNK-ALLOWED NIL)
#+XCL
#S(PATHNAME
SYSTEM::HOST NIL SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME
"BABYLON" TYPE "C" SYSTEM::VERSION 5)

;(MERGE-PATHNAME "test$$" SYMBOL 10)   ERROR
;;
;(MERGE-PATHNAME "test$$" SYMBOL)   ERROR
;
;(MERGE-PATHNAME "test$$" PATH)   ERROR
;
;(MERGE-PATHNAME "test$$")   ERROR

#+XCL
(MERGE-PATHNAMES "test$$")
#+XCL
#S(PATHNAME SYSTEM::HOST NIL SYSTEM::DEVICE
"DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST$$" TYPE "lsp"
SYSTEM::VERSION :NEWEST)

#+XCL
(MERGE-PATHNAMES "test$$" SYMBOL)
#+XCL
#S(PATHNAME SYSTEM::HOST NIL SYSTEM::DEVICE
"DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST$$" TYPE "ABC"
SYSTEM::VERSION :NEWEST)

#+XCL
(MERGE-PATHNAMES "test$$" SYMBOL 2)
#+XCL
#S(PATHNAME SYSTEM::HOST NIL
SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST$$" TYPE
"ABC" SYSTEM::VERSION 2)

#+XCL
(MERGE-PATHNAMES "test$$" (PATHNAME SYMBOL) 2)
#+XCL
#S(PATHNAME SYSTEM::HOST NIL
SYSTEM::DEVICE "DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST$$" TYPE
"ABC" SYSTEM::VERSION 2)

#+XCL
(MERGE-PATHNAMES "test$$" STREAM 2)
#+XCL
#S(PATHNAME SYSTEM::HOST 16 SYSTEM::DEVICE
"DISK00$ABT43" DIRECTORY "XCL.MAIN" SYSTEM::NAME "TEST$$" TYPE :ESCAPE
SYSTEM::VERSION 2)


;(MERGE-PATHNAME STRING SYMBOL)   ERROR

#+XCL
(MAKE-PATHNAME :NAME "a" :HOST (QUOTE ORION))
#+XCL
#S(PATHNAME SYSTEM::HOST ORION
SYSTEM::DEVICE NIL DIRECTORY NIL SYSTEM::NAME "a" TYPE NIL SYSTEM::VERSION
:NEWEST)

#+XCL
(DEFMACRO TEST (&REST BODY) (\` (APPLY (FUNCTION MAKE-PATHNAME) (\,@ BODY))))
#+XCL
TEST

#+XCL
(setf a '(:host "sirius" :name "a"))
#+XCL
(:host "sirius" :name "a")

#+XCL
(TEST A)
#+XCL
#S(PATHNAME SYSTEM::HOST "sirius" SYSTEM::DEVICE NIL DIRECTORY NIL
SYSTEM::NAME "a" TYPE NIL SYSTEM::VERSION :NEWEST)

#+XCL
(SETF A (LIST* :DEVICE "disk00$abt43" A))
#+XCL
(:DEVICE "disk00$abt43" :HOST "sirius" :NAME "a")

#+XCL
(TEST A)
#+XCL
#S(PATHNAME SYSTEM::HOST "sirius" SYSTEM::DEVICE "disk00$abt43"
DIRECTORY NIL SYSTEM::NAME "a" TYPE NIL SYSTEM::VERSION :NEWEST)

#+XCL
(SETF A (LIST* :DIRECTORY "[heicking.comlisp]" A))
#+XCL
(:DIRECTORY
"[heicking.comlisp]" :DEVICE "disk00$abt43" :HOST "sirius" :NAME "a")

#+XCL
(TEST A)
#+XCL
#S(PATHNAME SYSTEM::HOST "sirius" SYSTEM::DEVICE "disk00$abt43"
DIRECTORY "[heicking.comlisp]" SYSTEM::NAME "a" TYPE NIL SYSTEM::VERSION
:NEWEST)

#+XCL
(SETF A (LIST* :TYPE "raf" A))
#+XCL
(:TYPE "raf" :DIRECTORY "[heicking.comlisp]"
:DEVICE "disk00$abt43" :HOST "sirius" :NAME "a")

#+XCL
(TEST A)
#+XCL
#S(PATHNAME SYSTEM::HOST "sirius" SYSTEM::DEVICE "disk00$abt43"
DIRECTORY "[heicking.comlisp]" SYSTEM::NAME "a" TYPE "raf" SYSTEM::VERSION
:NEWEST)

#+XCL
(SETF A (LIST* :VERSION 3 A))
#+XCL
(:VERSION 3 :TYPE "raf" :DIRECTORY
"[heicking.comlisp]" :DEVICE "disk00$abt43" :HOST "sirius" :NAME "a")

#+XCL
(TEST A)
#+XCL
#S(PATHNAME SYSTEM::HOST "sirius" SYSTEM::DEVICE "disk00$abt43"
DIRECTORY "[heicking.comlisp]" SYSTEM::NAME "a" TYPE "raf" SYSTEM::VERSION 3)

(MAPCAR (FUNCTION PATHNAMEP) (LIST PATHSYMBOL PATHPATH PATHSTRING))
(T T T)

#+XCL
(SETF PATH (TEST A))
#+XCL
#S(PATHNAME SYSTEM::HOST "sirius" SYSTEM::DEVICE
"disk00$abt43" DIRECTORY "[heicking.comlisp]" SYSTEM::NAME "a" TYPE "raf"
SYSTEM::VERSION 3)

#+XCL
(MAPCAR (FUNCTION PATHNAME-HOST) (LIST SYMBOL STRING STREAM PATH))
#+XCL
(NIL NIL NIL NIL)

#+XCL
(MAPCAR (FUNCTION PATHNAME-DEVICE) (LIST SYMBOL STRING STREAM PATH))
#+XCL
("DISK00$ABT43" "DISK00$ABT43" "DISK00$ABT43" "DISK00$ABT43")

#+XCL
(MAPCAR (FUNCTION PATHNAME-DIRECTORY) (LIST SYMBOL STRING STREAM PATH))
#+XCL
("XCL.MAIN" "XCL.MAIN" "XCL.MAIN" "XCL.MAIN")

(PROGN (CLOSE STREAM) T)
T

#+XCL
(USER-HOMEDIR-PATHNAME)
#+XCL
#S(PATHNAME SYSTEM::HOST NIL SYSTEM::DEVICE
"DISK00$ABT43" DIRECTORY "HEICKING" SYSTEM::NAME NIL TYPE NIL SYSTEM::VERSION
NIL)

(PATHNAME "*.*")
#+XCL
#S(PATHNAME SYSTEM::HOST NIL SYSTEM::DEVICE "DISK00$ABT43"
DIRECTORY "HEICKING" SYSTEM::NAME "*" TYPE :WILD SYSTEM::VERSION NIL)
#+CLISP
#S(PATHNAME :HOST NIL :DEVICE NIL :DIRECTORY (:RELATIVE)
            :NAME "*" :TYPE "*" :VERSION NIL)

(progn (setf file (open "nicht-vorhandenes-file.non"
                        :direction :input
                        :element-type 'string-char
                        :if-does-not-exist :create)) t)
t

(null (probe-file "nicht-vorhandenes-file.non"))
NIL

(progn (close file) t)
t

(setf file (open "nicht-vorhandenes-file.non"
                        :direction :io
                        :element-type 'string-char
                        :if-exists :error))
error

(progn (close file) t)
t

(null (setf file (open "nicht-vorhandenes-file.non"
                        :direction :io
                        :element-type 'string-char
                        :if-exists :new-version)))
nil

(progn (close file) t)
t

(null (setf file (open "nicht-vorhandenes-file.non"
                        :direction :io
                        :element-type 'string-char
                        :if-exists :rename)))
nil

(progn (close file) t)
t

(null (setf file (open "nicht-vorhandenes-file.non"
                        :direction :io
                        :element-type 'string-char
                        :if-exists :rename-and-delete)))
nil

(progn (close file) t)
t

(null (setf file (open "nicht-vorhandenes-file.non"
                        :direction :io
                        :element-type 'string-char
                        :if-exists :overwrite)))
nil

(progn (close file) t)
t

(null (setf file (open "nicht-vorhandenes-file.non"
                        :direction :io
                        :element-type 'string-char
                        :if-exists :append)))
nil

(progn (close file) t)
t

(null (setf file (open "nicht-vorhandenes-file.non"
                        :direction :io
                        :element-type 'string-char
                        :if-exists :supersede)))
nil

(progn (close file) t)
t

(setf file (open "nicht-vorhandenes-file.non"
                        :direction :io
                        :element-type 'string-char
                        :if-exists nil))
nil

(progn (close file) t)
error

(setf file (open "nicht-vorhandenes-file.new"
                        :direction :io
                        :element-type 'string-char
                        :if-does-not-exist :error))
error

(progn (close file) t)
error

(null (setf file (open "nicht-vorhandenes-file.new"
                        :direction :io
                        :element-type 'string-char
                        :if-does-not-exist :create)))
nil

(progn (close file) t)
t

(null (setf file (open "nicht-vorhandenes-file.non"
                        :direction :io
                        :element-type 'string-char
                        :if-does-not-exist nil)))
nil

(progn (close file) t)
t

(namestring
  (multiple-value-setq (new-name pathname truename)
                     (rename-file "nicht-vorhandenes-file.non" "file.da")))
"file.da"

(namestring new-name)
"file.da"

(null pathname)
nil

(null truename)
nil

(progn (delete-file "test-pathname.abc") t)
t

(progn (mapc #'delete-file (directory "nicht-vorhandenes-file.*")) t)
t

(progn (delete-file "file.da") t)
t

