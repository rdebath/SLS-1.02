;;; ENGLISH: Site specific definitions, to be modified on installation
;;; DEUTSCH: Funktionen, die beim Transportieren zu ändern sind
;;; FRANCAIS: Fonctions dépendantes de l'installation

(in-package "LISP")
(mapcar #'fmakunbound '(machine-type machine-version machine-instance
                        short-site-name long-site-name editor-tempfile))

(defun machine-type () "Atari ST")
(defun machine-version () "Mega ST 4")
(defun machine-instance () "Instituts-Maschine Math. Institut II")

(defun short-site-name () "Karlsruhe")
(defun long-site-name () "Zimmer 320, Mathe-Bau, Universität Karlsruhe, W7500 Karlsruhe 1, Deutschland")

;; ENGLISH: The name of the editor:
;; DEUTSCH: Der Name des Editors:
;; FRANCAIS: Nom de l'éditeur :
(defparameter *editor* "E:\\LISP\\TEMPUS.PRG")

;; ENGLISH: The temporary file LISP creates for editing:
;; DEUTSCH: Das temporäre File, das LISP beim Editieren anlegt:
;; FRANCAIS: Fichier temporaire créé par LISP pour l'édition :
(defun editor-tempfile () "LISPTEMP.LSP")

;; ENGLISH: The list of directories where programs are searched on LOAD etc.
;;          if device and directory are unspecified:
;; DEUTSCH: Die Liste von Directories, in denen Programme bei LOAD etc. gesucht
;;          werden, wenn dabei Laufwerk und Directory nicht angegeben ist:
;; FRANCAIS: Liste de répertoires où chercher un fichier lorsqu'un répertoire
;;           particulier n'est pas indiqué :
(defparameter *load-paths*
  '(#"E:\\LISP\\...\\" ; in allen Directories unterhalb von E:\LISP
    #"A:"        ; erst im Current-Directory von Laufwerk A:
    #"A:\\"      ; dann im Root-Directory von Laufwerk A:
    #"A:\\...\\" ; dann in allen Directories von Laufwerk A:
   )
)

;; ENGLISH: Also set the variable *default-time-zone* in DEFS1.LSP according
;;          to your time zone.
;; DEUTSCH: Setzen Sie auch die Variable *default-time-zone* in DEFS1.LSP
;;          auf die bei Ihnen gültige Zeitzone.
;; FRANCAIS: Dans DEFS1.LSP, affectez à *default-time-zone* la valeur
;;           correspondante à votre fuseau horaire.

