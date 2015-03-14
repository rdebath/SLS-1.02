;;; ENGLISH: Site specific definitions, to be modified on installation
;;; DEUTSCH: Funktionen, die beim Transportieren zu ändern sind
;;; FRANCAIS: Fonctions dépendantes de l'installation

(in-package "LISP")
(mapcar #'fmakunbound '(machine-type machine-version machine-instance
                        short-site-name long-site-name editor-tempfile))

(defun machine-type () "PC/486")
(defun machine-version () "486/33")
(defun machine-instance () "Heimgerät Bruno Haible")

(defun short-site-name () "Karlsruhe")
(defun long-site-name () "Augartenstraße 40, W7500 Karlsruhe 1, Deutschland")

;; ENGLISH: The name of the editor:
;; DEUTSCH: Der Name des Editors:
;; FRANCAIS: Nom de l'éditeur :
(defparameter *editor* "C:\\UTIL\\PRODIT.EXE")

;; ENGLISH: The temporary file LISP creates for editing:
;; DEUTSCH: Das temporäre File, das LISP beim Editieren anlegt:
;; FRANCAIS: Fichier temporaire créé par LISP pour l'édition :
(defun editor-tempfile ()
  #+DOS "LISPTEMP.LSP"
  #+OS/2 "lisptemp.lsp"
)

;; ENGLISH: The list of directories where programs are searched on LOAD etc.
;;          if device and directory are unspecified:
;; DEUTSCH: Die Liste von Directories, in denen Programme bei LOAD etc. gesucht
;;          werden, wenn dabei Laufwerk und Directory nicht angegeben ist:
;; FRANCAIS: Liste de répertoires où chercher un fichier lorsqu'un répertoire
;;           particulier n'est pas indiqué :
(defparameter *load-paths*
  '(#"C:"               ; erst im Current-Directory von Laufwerk C:
    #"C:\\CLISP\\...\\" ; dann in allen Directories unterhalb C:\CLISP
   )
)

;; ENGLISH: Also set the variable *default-time-zone* in DEFS1.LSP according
;;          to your time zone.
;; DEUTSCH: Setzen Sie auch die Variable *default-time-zone* in DEFS1.LSP
;;          auf die bei Ihnen gültige Zeitzone.
;; FRANCAIS: Dans DEFS1.LSP, affectez à *default-time-zone* la valeur
;;           correspondante à votre fuseau horaire.

