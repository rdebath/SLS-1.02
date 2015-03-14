;; Test-Suiten ablaufen lassen:

#+CLISP
(defmacro with-ignored-errors (&rest forms)
  (let ((b (gensym)))
    `(BLOCK ,b
       (LET ((*ERROR-HANDLER*
               #'(LAMBDA (&REST ARGS) (RETURN-FROM ,b 'ERROR))
            ))
         ,@forms
     ) )
) )

(defun run-test (testname
                 &aux (logname (merge-pathnames ".erg" testname)) log-empty-p)
  (with-open-file (s (merge-pathnames ".tst" testname) :direction :input)
    (with-open-file (log logname :direction :output)
      (let ((*package* *package*)
            (*print-pretty* nil)
            (eof "EOF"))
        (loop
          (let ((form (read s nil eof))
                (result (read s nil eof)))
            (when (or (eq form eof) (eq result eof)) (return))
            (print form)
            (let ((my-result (with-ignored-errors (eval form))))
              (cond ((eql result my-result)
                     (format t "~%EQL-OK: ~S" result)
                    )
                    ((equal result my-result)
                     (format t "~%EQUAL-OK: ~S" result)
                    )
                    ((equalp result my-result)
                     (format t "~%EQUALP-OK: ~S" result)
                    )
                    (t
                     (format t "~%FEHLER!! ~S sollte ~S sein!" my-result result)
                     (format log "~%Form: ~S~%SOLL: ~S~%CLISP: ~S~%" form result my-result)
                    )
        ) ) ) )
      )
      (setq log-empty-p (zerop (file-length log)))
  ) )
  (when log-empty-p (delete-file logname))
  (values)
)

(defun run-all-tests ()
  (mapc #'run-test
        '("alltest" "array" "backquot" "characters" "eval20" "format"
          "hashlong" "iofkts" "lambda" "lists151" "lists152" "lists153"
          "lists154" "lists155" "lists156" "macro8" "map" "pack11" "path"
          "setf" "steele7" "streams" "streamslong" "strings" "symbol10"
          "symbols" "type"
  )      )
  t
)

