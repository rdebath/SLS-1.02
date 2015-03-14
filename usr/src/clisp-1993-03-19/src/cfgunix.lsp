;;; ENGLISH: Site specific definitions, to be modified on installation
;;; DEUTSCH: Funktionen, die beim Transportieren zu ändern sind
;;; FRANCAIS: Fonctions dépendantes de l'installation

(in-package "LISP")
(mapcar #'fmakunbound '(short-site-name long-site-name
                        edit-file editor-tempfile))

(defun short-site-name () "Karlsruhe")
(defun long-site-name () "Augartenstraße 40, D-W7500 Karlsruhe 1, Deutschland")

;; ENGLISH: The name of the editor:
;; DEUTSCH: Der Name des Editors:
;; FRANCAIS: Nom de l'éditeur :
(defparameter *editor* "vi")

;; ENGLISH: (edit-file file) edits a file.
;; DEUTSCH: (edit-file file) editiert eine Datei.
;; FRANCAIS: (edit-file file) permet l'édition d'un fichier.
(defun edit-file (file)
  (shell (format nil "~A ~A" *editor* (truename file)))
)

;; ENGLISH: The temporary file LISP creates for editing:
;; DEUTSCH: Das temporäre File, das LISP beim Editieren anlegt:
;; FRANCAIS: Fichier temporaire créé par LISP pour l'édition :
(defun editor-tempfile ()
  (merge-pathnames "lisptemp.lsp" (user-homedir-pathname))
)

;; ENGLISH: The list of directories where programs are searched on LOAD etc.:
;; DEUTSCH: Die Liste von Directories, in denen Programme bei LOAD etc. gesucht
;;          werden:
;; FRANCAIS: Liste de répertoires où chercher un fichier programme:
(defparameter *load-paths* '(#"./" #"./**/" #"~/**/"))

