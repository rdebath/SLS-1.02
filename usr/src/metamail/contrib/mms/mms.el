;;##################################################################;;
;;                                                                  ;;
;;    MMS client functions for Emacs/Lisp                           ;;
;;                                                                  ;;
;;        May 1992,   Yutaka Sato <ysato@etl.go.jp>                 ;;
;;                                                                  ;;
;;__________________________________________________________________;;

(setq *mms-service-name* "mms")
(setq mms_server_host (getenv "MMSERVER"))
(setq mms_user_command "USER ysato .mmspasswd ahobaka\n")
(setq mms_enclose_demo
      "ENCLOSE audio/basic /usr/demo/SOUND/sounds/crash.au\n")

;;//////////////////////////////////////////////////////////////////
;;    (mms-open SERVER)
;;//////////////////////////////////////////////////////////////////
(defun mms-open (server-name)
    (setq mms-server (open-network-stream
                    " MMS CLIENT "
                    "mms-server-response"
                    server-name
                    *mms-service-name*
                 )
    )
)

(setq mms-menu
  "[MMS] h:elp  q:uit  p:rint  v:iew  s:keleton  a:dd-text  e:nclose")

(defvar mms-mode-map nil "Local keymap for mms-mode buffers")
(setq mms-mode-map (make-keymap))
(suppress-keymap mms-mode-map)
(define-key mms-mode-map "s"  'mms-show)

;;(put `mms-mode `mode-class `special)

(defun mms-mode (&optional server)
  "Mode for handle MIME message" (interactive)
;;  (setq mms-previous-buffer (buffer-name))
;;  (switch-to-buffer (process-buffer mms-server))
  (setq mode-line-buffer-identification "Mms")
  (use-local-map mms-mode-map)
;;  (switch-to-buffer mms-previous-buffer)
)

(defun mms-show () (interactive)
    (message "show pressed")
    (mms-skeleton)
)


(defun mms-help ()    (process-send-string mms-server "HELP\n"))
(defun mms-print ()   (process-send-string mms-server "PRINT\n"))
(defun mms-view ()    (process-send-string mms-server "VIEW\n"))
(defun mms-skeleton() (process-send-string mms-server "SKELETON\n"))
(defun mms-enclose () (process-send-string mms-server mms_enclose_demo )
)
(defun mms-addtext () (process-send-string mms-server
  "ADDTEXT bold\ntest1\ntest2\n.\n"))

(defun mms-user (user) (process-send-string mms-server user))


;;(accept-process-output)


;;//////////////////////////////////////////////////////////////////
;;    SIMPLE MMS INTERFACE
;;//////////////////////////////////////////////////////////////////
(setq mms-server nil)
(defun mms ()
  "simple MIME server interface"
  (interactive)

  (if mms-server nil
     (mms-open mms_server_host)
     (mms-user mms_user_command)
  )

  (setq mms-previous-buffer (buffer-name))
  (switch-to-buffer (process-buffer mms-server))

  (message mms-menu)
  (setq mms-done 'c)
  (while (eq mms-done 'c)
         (setq comch (char-to-string (read-char)))
         (cond ((equal comch "h") (mms-help))
               ((equal comch "a") (mms-addtext))
               ((equal comch "s") (mms-skeleton))
               ((equal comch "v") (mms-view))
               ((equal comch "p") (mms-print))
               ((equal comch "q") (setq mms-done 'q))
               ((equal comch "e") (mms-enclose))
          )
	 (end-of-buffer)
         (message mms-help)
  )
  (message "MMS DONE")

  (switch-to-buffer mms-previous-buffer)
) 

;;//////////////////////////////////////////////////////////////////
(mms)
