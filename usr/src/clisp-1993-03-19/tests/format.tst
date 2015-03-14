;*******************************************************************************
;*      Rosenmueller            format.tst                                     *
;*******************************************************************************

; ~< ---------------------------------------------------------------------------
(format nil "~10<foo~;bar~>")
"foo    bar"

(format nil "~10:<foo~;bar~>")
"  foo  bar"

(format nil "~10@<foo~;bar~>")
"foo  bar  "

(format nil "~10:@<foo~;bar~>")
"  foo bar "

(format nil "~10<foobar~>")
"    foobar"

(format nil "~10:<foobar~>")
"    foobar"

(format nil "~10@<foobar~>")
"foobar    "

(format nil "~10:@<foobar~>")
"  foobar  "

; ~< ~s ~^ ---------------------------------------------------------------------
(format nil "~15<~S~>" 'foo)
"            foo"

(format nil "~15<~S~;~^~S~>" 'foo)
"            foo"

(format nil "~15<~S~;~^~S~;~^~S~>" 'foo)
"            foo"

(format nil "~15<~S~;~^~S~>" 'foo 'bar)
"foo         bar"

(format nil "~15<~S~;~^~S~;~^~S~>" 'foo 'bar)
"foo         bar"

(format nil "~15<~S~;~^~S~;~^~S~>" 'foo 'bar 'baz)
"foo   bar   baz"

(progn
(setq liste '(aaaaaaa bbbbbb cccccccccccc dddddddddddddd eeee fffffffff
gggggggg
 hhhhh iiii j kk lll mmmm nnnnnn oooooooooo ppppppppppppppp qqqqqqq
rrrrrrrrrrrr
s ttt uuuuuuuuu vvvvvvv wwwwwwwwww xxxxx yyyyyy zzzzzzzz))              ;26
T)
T

(format nil "~%;; ~<~%;; ~1:; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~;~
 ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~;~
 ~s~; ~s~; ~s~; ~s~;~>~%"                               ; 2!
'aaaaaaa 'bbbbbb 'cccccccccccc 'dddddddddddddd 'eeee 'fffffffff 'gggggggg
'hhhhh 'iiii 'j 'kk 'lll 'mmmm 'nnnnnn 'oooooooooo 'ppppppppppppppp
'qqqqqqq
'rrrrrrrrrrrr 's 'ttt 'uuuuuuuuu 'vvvvvvv 'wwwwwwwwww 'xxxxx 'yyyyyy
'zzzzzzzz)
#+XCL
"
;;  AAAAAAA  BBBBBB CCCCCCCCCCCC DDDDDDDDDDDDDD EEEE FFFFFFFFF GGGGGGGG
;;  HHHHH  IIII  JKK LLL MMMM NNNNNN OOOOOOOOOO PPPPPPPPPPPPPPP QQQQQQQ
;;  RRRRRRRRRRRR    S    TTT   UUUUUUUUU   VVVVVVV   WWWWWWWWWW   XXXXX
"
;23456789;123456789;123456789;123456789;123456789;123456789;123456789;12
#-XCL
"
;; 
;;  AAAAAAA BBBBBB CCCCCCCCCCCC DDDDDDDDDDDDDD EEEE FFFFFFFFF GGGGGGGG HHHHH IIII JKK LLL MMMM NNNNNN OOOOOOOOOO PPPPPPPPPPPPPPP QQQQQQQ RRRRRRRRRRRR S TTTUUUUUUUUU VVVVVVV WWWWWWWWWW XXXXX
"

(format nil "~%;; ~<~%;; ~1,50:; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~;~
 ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~;~
 ~s~; ~s~; ~s~; ~s~;~>~%"                               ; 2!
'aaaaaaa 'bbbbbb 'cccccccccccc 'dddddddddddddd 'eeee 'fffffffff 'gggggggg
'hhhhh 'iiii 'j 'kk 'lll 'mmmm 'nnnnnn 'oooooooooo 'ppppppppppppppp
'qqqqqqq
'rrrrrrrrrrrr 's 'ttt 'uuuuuuuuu 'vvvvvvv 'wwwwwwwwww 'xxxxx 'yyyyyy
'zzzzzzzz)
#+XCL
"
;;  AAAAAAA  BBBBBB  CCCCCCCCCCCC  DDDDDDDDDDDDDD
;;  EEEE  FFFFFFFFF  GGGGGGGG  HHHHH IIII JKK LLL
;;  MMMM    NNNNNN   OOOOOOOOOO   PPPPPPPPPPPPPPP
;;  QQQQQQQ  RRRRRRRRRRRR  S TTTUUUUUUUUU VVVVVVV
;;  WWWWWWWWWW                              XXXXX
"
;23456789;123456789;123456789;123456789;123456789;
#-XCL
"
;; 
;;  AAAAAAA BBBBBB CCCCCCCCCCCC DDDDDDDDDDDDDD EEEE FFFFFFFFF GGGGGGGG HHHHH IIII JKK LLL MMMM NNNNNN OOOOOOOOOO PPPPPPPPPPPPPPP QQQQQQQ RRRRRRRRRRRR S TTTUUUUUUUUU VVVVVVV WWWWWWWWWW XXXXX
"

(defun format-blocksatz (stream parts prefix &optional line-length start-p end-p)
  (if (null stream)
    (let ((stream (make-string-output-stream)))
      (format-blocksatz stream parts prefix line-length start-p end-p)
      (get-output-stream-string stream)
    )
    (unless (endp parts)
      (setq line-length (or line-length #|(sys::line-length stream)|# 72))
      (when start-p (format stream prefix))
      (loop
        ; Hier ist parts /= NIL
        (let ((pos (sys::line-position stream))
              (parts-now '()))
          (let ((pos-now pos))
            (loop
              (when (endp parts) (return))
              (let* ((part (first parts))
                     (part-length (length part)))
                (unless (null parts-now)
                  (when (> (+ pos-now part-length) line-length)
                    (return)
                ) )
                (pop parts)
                (push part parts-now)
                (incf pos-now part-length)
          ) ) )
          ; Hier ist parts-now /= NIL
          (apply #'format
                 stream
                 (if (and (endp parts) (not end-p))
                   (apply #'concatenate 'string
                     (make-list (length parts-now) :initial-element "~A")
                   )
                   (concatenate 'string
                     "~"
                     (write-to-string (max 0 (- line-length pos))
                                      :radix nil :base 10
                     )
                     (if (= (length parts-now) 1) "@" "")
                     "<"
                     (apply #'concatenate 'string
                       "~A"
                       (make-list (1- (length parts-now)) :initial-element "~;~A")
                     )
                     "~>"
                 ) )
                 (nreverse parts-now)
        ) )
        (when (endp parts) (return))
        (format stream prefix)
) ) ) )
FORMAT-BLOCKSATZ

(format-blocksatz nil
  (mapcar #'(lambda (x) (format nil " ~A" x))
          '(aaaaaaa bbbbbb cccccccccccc dddddddddddddd eeee fffffffff
            gggggggg hhhhh iiii j kk lll mmmm nnnnnn oooooooooo
            ppppppppppppppp qqqqqqq rrrrrrrrrrrr s ttt uuuuuuuuu vvvvvvv
            wwwwwwwwww xxxxx yyyyyy zzzzzzzz)
  )
  "~%;; "
  nil t nil
)
"
;;  AAAAAAA  BBBBBB  CCCCCCCCCCCC DDDDDDDDDDDDDD EEEE FFFFFFFFF GGGGGGGG
;;  HHHHH  IIII  J KK LLL MMMM NNNNNN OOOOOOOOOO PPPPPPPPPPPPPPP QQQQQQQ
;;  RRRRRRRRRRRR   S  TTT  UUUUUUUUU  VVVVVVV  WWWWWWWWWW  XXXXX  YYYYYY
;;  ZZZZZZZZ"
;123456789;123456789;123456789;123456789;123456789;123456789;123456789;12

(format-blocksatz nil
  (mapcar #'(lambda (x) (format nil " ~A" x))
          '(aaaaaaa bbbbbb cccccccccccc dddddddddddddd eeee fffffffff
            gggggggg hhhhh iiii j kk lll mmmm nnnnnn oooooooooo
            ppppppppppppppp qqqqqqq rrrrrrrrrrrr s ttt uuuuuuuuu vvvvvvv
            wwwwwwwwww xxxxx yyyyyy zzzzzzzz)
  )
  "~%;; "
  50 t t
)
"
;;  AAAAAAA   BBBBBB  CCCCCCCCCCCC  DDDDDDDDDDDDDD
;;  EEEE  FFFFFFFFF  GGGGGGGG  HHHHH IIII J KK LLL
;;  MMMM NNNNNN OOOOOOOOOO PPPPPPPPPPPPPPP QQQQQQQ
;;  RRRRRRRRRRRR    S    TTT   UUUUUUUUU   VVVVVVV
;;  WWWWWWWWWW      XXXXX      YYYYYY     ZZZZZZZZ"
;123456789;123456789;123456789;123456789;123456789;

;;; unklare Bedeutung (Fehler in Sprachbeschreibung?)
;;; (format nil "~%;; ~{~<~%;; ~1:; ~s~>~^,~}.~%" liste) ""
;;; (format nil "~%;; ~{~<~%;; ~1,50:; ~s~>~^,~}.~%" liste) ""

; ~f ---------------------------------------------------------------------------
; Format F

(DEFUN FOO (X)
       (FORMAT NIL "~6,2F|~6,2,1,'*F|~6,2,,'?F|~6F|~,2F|~F" X X X X
X X))
FOO

(FOO 3.14159)
;       "  3.14| 31.42|  3.14|3.1416|3.14|3.141590116672995328"
"  3.14| 31.42|  3.14|3.1416|3.14|3.14159"

(FOO -3.14159)
;       " -3.14|-31.42| -3.14|-3.142|-3.14|-3.141590116672995328"
" -3.14|-31.42| -3.14|-3.142|-3.14|-3.14159"

(FOO 100.0)
"100.00|******|100.00| 100.0|100.00|100.0"

(FOO 1234.0)
"1234.00|******|??????|1234.0|1234.00|1234.0"

(FOO 0.006)
"  0.01|  0.06|  0.01| 0.006|0.01|0.006"

(format nil "~5,2,-13f" 1.1e13)
" 1.10"

(format nil "~9,0,6f" 3.14159)
" 3141590."

(FORMAT NIL "~5,3F" (QUOTE A))
"A"

(FORMAT NIL "~5,3F" #C(1.2 0.3))
"#C(1.2 0.3)"

(FORMAT NIL "~5,3F" 2/3)
"0.667"

; ~e ---------------------------------------------------------------------------
; Format E

(defun foo (x)
  (format nil
          "~9,2,1,,'*E|~10,3,2,2,'?,,'$E|~9,3,2,-2,'%@e|~9,2E"
          x x x x))
FOO

(foo 3.14159)
"  3.14E+0| 31.42$-01|+.003E+03|  3.14E+0"

(foo -3.14159)
" -3.14E+0|-31.42$-01|-.003E+03| -3.14E+0"

(foo 1100.0)
"  1.10E+3| 11.00$+02|+.001E+06|  1.10E+3"

(foo 1100.0L0)
#+XCL "  1.10D+3| 11.00$+02|+.001D+06|  1.10D+3"
#+CLISP "  1.10L+3| 11.00$+02|+.001L+06|  1.10L+3"

(foo 1.1E13)
"*********| 11.00$+12|+.001E+16| 1.10E+13"

; ERROR beim read der zahl (foo 1.1L120)

(FORMAT NIL "_~10,4E_" 1.2)
"_ 1.2000E+0_"

(format nil "~9,2,1E" 0.0314159)
"  3.14E-2"

; ~% ~d ~e (v) -----------------------------------------------------------------
(let (x)
 (dotimes (k 13 x)
  (setq x (cons (format nil "~%Scale factor ~2D: |~13,6,2,VE|"
          (- k 5) (- k 5) 3.14159) x))))
(
"
Scale factor  7: | 3141590.E-06|" "
Scale factor  6: | 314159.0E-05|" "
Scale factor  5: | 31415.90E-04|" "
Scale factor  4: | 3141.590E-03|" "
Scale factor  3: | 314.1590E-02|" "
Scale factor  2: | 31.41590E-01|" "
Scale factor  1: | 3.141590E+00|" "
Scale factor  0: | 0.314159E+01|" "
Scale factor -1: | 0.031416E+02|" "
Scale factor -2: | 0.003142E+03|" "
Scale factor -3: | 0.000314E+04|" "
Scale factor -4: | 0.000031E+05|" "
Scale factor -5: | 0.000003E+06|")


; ~g ---------------------------------------------------------------------------
(defun foo (x)
  (format nil "~9,2,1,,'*G|~9,3,2,3,'?,,'$G|~9,3,2,0,'%G|~9,2G"
          x x x x))
foo

(foo 0.0314159)
"  3.14E-2|314.2$-04|0.314E-01|  3.14E-2"

(foo 0.314159)
"  0.31   |0.314    |0.314    | 0.31    "

(foo 3.14159)
"   3.1   | 3.14    | 3.14    |  3.1    "

(foo 31.4159)
"   31.   | 31.4    | 31.4    |  31.    "

(foo 314.159)
"  3.14E+2| 314.    | 314.    |  3.14E+2"

(foo 3141.59)
"  3.14E+3|314.2$+01|0.314E+04|  3.14E+3"

(foo 3141.59L0)
#+XCL   "  3.14D+3|314.2$+01|0.314D+04|  3.14D+3"
#+CLISP "  3.14L+3|314.2$+01|0.314L+04|  3.14L+3"

(foo 3.14E12)
"*********|314.0$+10|0.314E+13| 3.14E+12"

;(foo 3.14L120 und L1200) fehler in numerik

; ~a ---------------------------------------------------------------------------

(FORMAT NIL "foo")
"foo"

(FORMAT NIL "format-a:--~a--ende" (QUOTE AB\c))
"format-a:--ABc--ende"

(SETQ Y "elephant")
"elephant"

(FORMAT NIL "Look at the ~A!" Y)
"Look at the elephant!"

(FORMAT NIL "format-%:--~%--1-newline-*")
"format-%:--
--1-newline-*"

(FORMAT NIL "format-%:--~3%--3-newline-*")
"format-%:--


--3-newline-*"

(FORMAT NIL "format-a:--~5a--ende-*" (QUOTE AB\c))
"format-a:--ABc  --ende-*"

(FORMAT NIL "format-a:--~5,2a--ende-*" (QUOTE AB\c))
"format-a:--ABc  --ende-*"

(FORMAT NIL "format-a:--~5,2,3a--ende-*" (QUOTE AB\c))
"format-a:--ABc   --ende-*"

(FORMAT NIL "format-a:--~5,2,3,'*a--ende-*" (QUOTE AB\c))
"format-a:--ABc***--ende-*"

(FORMAT NIL "format-a:--~@a--ende-*" (QUOTE AB\c))
"format-a:--ABc--ende-*"

(FORMAT NIL "format-a:--~5@a--ende-*" (QUOTE AB\c))
"format-a:--  ABc--ende-*"

(FORMAT NIL "format-a:--~5,2@a--ende-*" (QUOTE AB\c))
"format-a:--  ABc--ende-*"

(FORMAT NIL "format-a:--~5,2,3@a--ende-*" (QUOTE AB\c))
"format-a:--   ABc--ende-*"

(FORMAT NIL "format-a:--~5,2,3,'*@a--ende-*" (QUOTE AB\c))
"format-a:--***ABc--ende-*"

(FORMAT NIL "format-a:--~:a--ende-*" (QUOTE (AB\c NIL XYZ)))
"format-a:--(ABc NIL XYZ)--ende-*"

(FORMAT NIL "format-s:--~s--ende-*" (QUOTE AB\c))
#+XCL "format-s:--AB\\c--ende-*"
#+CLISP "format-s:--|ABc|--ende-*"

(FORMAT NIL "format-s:--~5s--ende-*" (QUOTE AB\c))
#+XCL "format-s:--AB\\c --ende-*"
#+CLISP "format-s:--|ABc|--ende-*"

(FORMAT NIL "format-s:--~5,2s--ende-*" (QUOTE AB\c))
#+XCL "format-s:--AB\\c  --ende-*"
#+CLISP "format-s:--|ABc|--ende-*"

(FORMAT NIL "format-s:--~5,2,3s--ende-*" (QUOTE AB\c))
#+XCL "format-s:--AB\\c   --ende-*"
#+CLISP "format-s:--|ABc|   --ende-*"

(FORMAT NIL "format-s:--~5,2,3,'*s--ende-*" (QUOTE AB\c))
#+XCL "format-s:--AB\\c***--ende-*"
#+CLISP "format-s:--|ABc|***--ende-*"

(FORMAT NIL "format-s:--~@s--ende-*" (QUOTE AB\c))
#+XCL "format-s:--AB\\c--ende-*"
#+CLISP "format-s:--|ABc|--ende-*"

(FORMAT NIL "format-s:--~5@s--ende-*" (QUOTE AB\c))
#+XCL "format-s:-- AB\\c--ende-*"
#+CLISP "format-s:--|ABc|--ende-*"

(FORMAT NIL "format-s:--~5,2@s--ende-*" (QUOTE AB\c))
#+XCL "format-s:--  AB\\c--ende-*"
#+CLISP "format-s:--|ABc|--ende-*"

(FORMAT NIL "format-s:--~5,2,3@s--ende-*" (QUOTE AB\c))
#+XCL "format-s:--   AB\\c--ende-*"
#+CLISP "format-s:--   |ABc|--ende-*"

(FORMAT NIL "format-s:--~5,2,3,'*@s--ende-*" (QUOTE AB\c))
#+XCL "format-s:--***AB\\c--ende-*"
#+CLISP "format-s:--***|ABc|--ende-*"

(FORMAT NIL "format-s:--~:s--ende-*" (QUOTE (AB\c NIL XYZ)))
#+XCL "format-s:--(AB\\c NIL XYZ)--ende-*"
#+CLISP "format-s:--(|ABc| NIL XYZ)--ende-*"

(SETQ X 5)
5

(FORMAT NIL "The answer is ~D." X)
"The answer is 5."

(FORMAT NIL "The answer is ~3D." X)
"The answer is   5."

(FORMAT NIL "The answer is ~3,'0D." X)
"The answer is 005."

(FORMAT NIL "The answer is ~:D." (EXPT 47 X))
"The answer is 229,345,007."

(FORMAT NIL "decimal:~d, width=5:~5d-*" 10 10)
"decimal:10, width=5:   10-*"

(FORMAT NIL "format-d:--~d--ende-*" 123)
"format-d:--123--ende-*"

(FORMAT NIL "format-d:--~10d--ende-*" 123)
"format-d:--       123--ende-*"

(FORMAT NIL "format-d:--~10,'?d--ende-*" 123)
"format-d:--???????123--ende-*"

(FORMAT NIL "format-d:--~@d--ende-*" 123)
"format-d:--+123--ende-*"

(FORMAT NIL "format-d:--~10@d--ende-*" 123)
"format-d:--      +123--ende-*"

(FORMAT NIL "format-d:--~10,'?@d--ende-*" 123)
"format-d:--??????+123--ende-*"

(FORMAT NIL "format-b:--~b--ende-*" 123)
"format-b:--1111011--ende-*"

(FORMAT NIL "format-b:--~10b--ende-*" 123)
"format-b:--   1111011--ende-*"

(FORMAT NIL "format-b:--~10,'?b--ende-*" 123)
"format-b:--???1111011--ende-*"

(FORMAT NIL "format-b:--~:b--ende-*" 123)
"format-b:--1,111,011--ende-*"

(FORMAT NIL "format-b:--~10:b--ende-*" 123)
"format-b:-- 1,111,011--ende-*"

(FORMAT NIL "format-b:--~10,'?:b--ende-*" 123)
"format-b:--?1,111,011--ende-*"

(FORMAT NIL "format-b:--~10,'?,'.:b--ende-*" 123)
"format-b:--?1.111.011--ende-*"

(FORMAT NIL "format-b:--~@b--ende-*" 123)
"format-b:--+1111011--ende-*"

(FORMAT NIL "format-b:--~10@b--ende-*" 123)
"format-b:--  +1111011--ende-*"

(FORMAT NIL "format-b:--~10,'?@b--ende-*" 123)
"format-b:--??+1111011--ende-*"

(FORMAT NIL "format-b:--~:@b--ende-*" 123)
"format-b:--+1,111,011--ende-*"

(FORMAT NIL "format-o:--~o--ende-*" 123)
"format-o:--173--ende-*"

(FORMAT NIL "format-o:--~10o--ende-*" 123)
"format-o:--       173--ende-*"

(FORMAT NIL "format-o:--~10,'?o--ende-*" 123)
"format-o:--???????173--ende-*"

(FORMAT NIL "format-o:--~@o--ende-*" 123)
"format-o:--+173--ende-*"

(FORMAT NIL "format-o:--~10@o--ende-*" 123)
"format-o:--      +173--ende-*"

(FORMAT NIL "format-x:--~x--ende-*" 123)
"format-x:--7B--ende-*"

(FORMAT NIL "format-x:--~10x--ende-*" 123)
"format-x:--        7B--ende-*"

(FORMAT NIL "format-x:--~10,'?x--ende-*" 123)
"format-x:--????????7B--ende-*"

(FORMAT NIL "format-x:--~10:x--ende-*" 123)
"format-x:--        7B--ende-*"

(FORMAT NIL "format-x:--~@x--ende-*" 123)
"format-x:--+7B--ende-*"

(FORMAT NIL "format-x:--~10@x--ende-*" 123)
"format-x:--       +7B--ende-*"

(FORMAT NIL "format-r:--~20r--ende-*" 123)
"format-r:--63--ende-*"

(FORMAT NIL "format-r:--~20,10r--ende-*" 123)
"format-r:--        63--ende-*"

(FORMAT NIL "format-r:--~20@r--ende-*" 123)
"format-r:--+63--ende-*"

(FORMAT NIL "format-r:--~r--ende-*" 9)
"format-r:--nine--ende-*"

(FORMAT NIL "format-r:--~:r--ende-*" 9)
"format-r:--ninth--ende-*"

(FORMAT NIL "format-r:--~@r--ende-*" 9)
"format-r:--IX--ende-*"

(FORMAT NIL "format-r:--~:@r--ende-*" 9)
"format-r:--VIIII--ende-*"

(FORMAT NIL "format-p:--~d  object~p-*" 1 1)
"format-p:--1  object-*"

(FORMAT NIL "format-p:--~d  object~p-*" 2 2)
"format-p:--2  objects-*"

(FORMAT NIL "format-p:--~d  bab~@p-*" 1 1)
"format-p:--1  baby-*"

(FORMAT NIL "format-p:--~d  bab~@p-*" 2 2)
"format-p:--2  babies-*"

(FORMAT NIL "format-p:--~d  object~:p-*" 1)
"format-p:--1  object-*"

(FORMAT NIL "format-p:--~d  object~:p-*" 2)
"format-p:--2  objects-*"

(FORMAT NIL "format-p:--~d  bab~:@p-*" 1)
"format-p:--1  baby-*"

(FORMAT NIL "format-&:--~%~&--1-newline-*")
"format-&:--
--1-newline-*"

(FORMAT NIL "format-&:--~%~3&--3-newline-*")
"format-&:--


--3-newline-*"

(FORMAT NIL "format-tilde:--~~--1-tilde-*")
"format-tilde:--~--1-tilde-*"

(FORMAT NIL "format-tilde:--~3~--3-tilden-*")
"format-tilde:--~~~--3-tilden-*"

(FORMAT NIL "format-|:--~|--1-ff-*")
"format-|:----1-ff-*"

(FORMAT NIL "format-|:--~2|--2-ff-*")
"format-|:----2-ff-*"

(FORMAT NIL
"format-<nl>:~
                         gl. zeile gl. angeschlossen trotz 2*<tab> und sp-*")
"format-<nl>:gl. zeile gl. angeschlossen trotz 2*<tab> und sp-*"

(FORMAT NIL "format-<nl>:~@
                         neue Zeile Anfang trotz <tab> + sp-*")
"format-<nl>:
neue Zeile Anfang trotz <tab> + sp-*"

(FORMAT NIL "format-<nl>:~:
	gleiche Zeile aber ein tab vor Anfang-*")
"format-<nl>:	gleiche Zeile aber ein tab vor Anfang-*"

(FORMAT NIL "format-?:***~a***~?***~a***-*" 1 "+++~s+++~s+++" (QUOTE
(A B)) 2)
"format-?:***1***+++A+++B+++***2***-*"

(FORMAT NIL "format-?:***~a***~?***~a***-*" 1 "+++++++++++++" NIL 2)
"format-?:***1***+++++++++++++***2***-*"

(FORMAT NIL "~(AAAAAAAA BBBBBB ccccccc dddddddd~)")
"aaaaaaaa bbbbbb ccccccc dddddddd"

(FORMAT NIL "~:(AAAAAAAA BBBBBB ccccccc dddddddd~)")
"Aaaaaaaa Bbbbbb Ccccccc Dddddddd"

(FORMAT NIL "~@(AAAAAAAA BBBBBB ccccccc dddddddd~)")
"Aaaaaaaa bbbbbb ccccccc dddddddd"

(FORMAT NIL "~:@(AAAAAAAA BBBBBB ccccccc dddddddd~)")
"AAAAAAAA BBBBBB CCCCCCC DDDDDDDD"

(FORMAT NIL "++~{-=~s=-~}++" (QUOTE (1 2 3)))
"++-=1=--=2=--=3=-++"

(FORMAT NIL "++~2{-=~s=-~}++" (QUOTE (1 2 3)))
"++-=1=--=2=-++"

(FORMAT NIL "++~@{-=~s=-~}++" 1 2 3)
"++-=1=--=2=--=3=-++"

(FORMAT NIL "++~:{-=~s=~s=-~}++" (QUOTE ((1 2) (3 4 5) (6 7))))
"++-=1=2=--=3=4=--=6=7=-++"

(FORMAT NIL "++~:@{-=~s=~s=-~}++" (QUOTE (1 2)) (QUOTE (3 4 5)) (QUOTE
(6 7)))
"++-=1=2=--=3=4=--=6=7=-++"

(FORMAT NIL "~{abc~:}")
#+XCL "abc" #-XCL ERROR

(FORMAT NIL "~{~:}" "xyz")
#+XCL "xyz" #-XCL ERROR

(FORMAT NIL "~1{~:}" "-~s-" (QUOTE (1 2)) 3)
"-1-"

(FORMAT NIL "123456789012345678901234567890
~10,4txx~10,4ty~10,4tzzz~10,4tende")
#+XCL
"123456789012345678901234567890
         xx  y   zzz ende"
#-XCL
"123456789012345678901234567890
          xx  y   zzz ende"

(FORMAT NIL "123456789012345678901234567890
~3,4@txx~3,4@ty~3,4@tzzz~3,4@tende")
#+XCL
"123456789012345678901234567890
   xx      y   zzz     ende"
#-XCL
"123456789012345678901234567890
    xx      y   zzz     ende"

(FORMAT NIL "-~a-~a-~a-~a-" 1 2 3 4 5 6 7 8 9)
"-1-2-3-4-"

(FORMAT NIL "-~a-~a-~*~a-~a-" 1 2 3 4 5 6 7 8 9)
"-1-2-4-5-"

(FORMAT NIL "-~a-~a-~3*~a-~a-" 1 2 3 4 5 6 7 8 9)
"-1-2-6-7-"

(FORMAT NIL "-~a-~a-~:*~a-~a-" 1 2 3 4 5 6 7 8 9)
"-1-2-2-3-"

(FORMAT NIL "-~a-~a-~2:*~a-~a-" 1 2 3 4 5 6 7 8 9)
"-1-2-1-2-"

(FORMAT NIL "-~a-~a-~@*~a-~a-" 1 2 3 4 5 6 7 8 9)
"-1-2-1-2-"

(FORMAT NIL "-~a-~a-~6@*~a-~a-" 1 2 3 4 5 6 7 8 9)
"-1-2-7-8-"

(FORMAT NIL "~[aa~;bb~;cc~]" 1)
"bb"

(FORMAT NIL "~[aa~;bb~;cc~]" 10)
""

(FORMAT NIL "~2[aa~;bb~;cc~]" 10)
"cc"

(FORMAT NIL "~@[aaa~]" NIL 10)
""

(FORMAT NIL "~@[aaa~]" 20 10)
"aaa"

(FORMAT NIL "~@[aaa~d~]" NIL 10)
""

(FORMAT NIL "~@[aaa~d~]" 20 10)
"aaa20"

(FORMAT NIL "~@[aaa~d~]bbb~d" NIL 10 30)
"bbb10"

(FORMAT NIL "~@[aaa~d~]bbb~d" 20 10 30)
"aaa20bbb10"

(FORMAT NIL "~:[-nil-~;-true-~d~]-ende~d" NIL 10 20)
"-nil--ende10"

(FORMAT NIL "~:[-nil-~;-true-~d~]-ende~d" T 10 20)
"-true-10-ende20"

(FORMAT NIL "Start test, newline:~%freshline:~&")
"Start test, newline:
freshline:
"

(FORMAT NIL "decimal pad with period:~10,vd-*" #\. 12)
"decimal pad with period:........12-*"

(FORMAT NIL "char normal:~c, as # would read:~@c, human read:~:c-*"
#\SPACE
#\SPACE #\SPACE)
#+(or XCL CMU)     "char normal: , as # would read:#\\Space, human read:Space-*"
#+(or CLISP LUCID) "char normal:Space, as # would read:#\\Space, human read:Space-*"

(FORMAT NIL
"cardinal:~r, roman new:~@r, roman-old:~:@r~
                <same line I hope>~@
                new line but at beginning~:
   same line, but spaced out~@
        new line and over two tabs-*" 4 4 4)
"cardinal:four, roman new:IV, roman-old:IIII<same line I hope>
new line but at beginning   same line, but spaced out
new line and over two tabs-*"

(FORMAT NIL "Type ~:C to ~A." (SET-CHAR-BIT #\D :CONTROL T)
"delete all your files")
"Type Control-D to delete all your files."

(SETQ N 3)
3

(FORMAT NIL "~D item~:P found." N)
"3 items found."

(FORMAT NIL "~R dog~:[s are~; is~] here." N (= N 1))
"three dogs are here."

(FORMAT NIL "~R dog~:*~[s are~; is~:;s are~] here." N)
"three dogs are here."

(FORMAT NIL "Here ~[are~;is~:;are~] ~:*~R pupp~:@p." N)
"Here are three puppies."

(SETQ N 1)
1

(FORMAT NIL "~D item~:P found." N)
"1 item found."

(FORMAT NIL "~R dog~:[s are~; is~] here." N (= N 1))
"one dog is here."

(FORMAT NIL "~R dog~:*~[s are~; is~:;s are~] here." N)
"one dog is here."

(FORMAT NIL "Here ~[are~;is~:;are~] ~:*~R pupp~:@p." N)
"Here is one puppy."

(SETQ N 0)
0

(FORMAT NIL "~D item~:P found." N)
"0 items found."

(FORMAT NIL "~R dog~:[s are~; is~] here." N (= N 1))
"zero dogs are here."

(FORMAT NIL "~R dog~:*~[s are~; is~:;s are~] here." N)
"zero dogs are here."

(FORMAT NIL "Here ~[are~;is~:;are~] ~:*~R pupp~:@p." N)
"Here are zero puppies."

(FORMAT NIL "~D tr~:@p/~D win~:P" 7 1)
"7 tries/1 win"

(FORMAT NIL "~D tr~:@p/~D win~:P" 1 0)
"1 try/0 wins"

(FORMAT NIL "~D tr~:@p/~D win~:P" 1 3)
"1 try/3 wins"

(DEFUN TYPE-CLASH-ERROR (FN NARGS ARGNUM RIGHT-TYPE WRONG-TYPE) (FORMAT
NIL
"~&~S requires itts ~:[~:R~;~*~] ~
           argument to be of type ~S,~%but it was called ~
           with an argument of type ~S.-*" FN (EQL NARGS 1) ARGNUM
RIGHT-TYPE
WRONG-TYPE))
TYPE-CLASH-ERROR

(TYPE-CLASH-ERROR (QUOTE AREF) NIL 2 (QUOTE INTEGER) (QUOTE VECTOR))
"AREF requires itts second argument to be of type INTEGER,
but it was called with an argument of type VECTOR.-*"

(TYPE-CLASH-ERROR (QUOTE CAR) 1 1 (QUOTE LIST) (QUOTE SHORT-FLOAT))
"CAR requires itts  argument to be of type LIST,
but it was called with an argument of type SHORT-FLOAT.-*"

(FORMAT NIL "~? ~D" "<~A ~D>" (QUOTE ("Foo" 5)) 7)
"<Foo 5> 7"

(FORMAT NIL "~? ~D" "<~A ~D>" (QUOTE (" Foo" 5 14)) 7)
"< Foo 5> 7"

(FORMAT NIL "~@? ~d" "<~A ~D>" "Foo" 5 7)
"<Foo 5> 7"

(FORMAT NIL "~@? ~D" "<~A ~D>" "Foo" 5 14 7)
"<Foo 5> 14"

(FORMAT NIL "~@R ~(~@R~)" 14 14)
"XIV xiv"

(DEFUN F (N) (FORMAT NIL "~@(~R~) error~:P detected." N))
F

(F 0)
"Zero errors detected."

(F 1)
"One error detected."

(F 23)
"Twenty-three errors detected."

(SETQ *PRINT-LEVEL* NIL *PRINT-LENGTH* 5)
5

(FORMAT NIL "~@[ print level = ~D~]~@[ print length = ~D~]" *PRINT-LEVEL*

*PRINT-LENGTH*)
" print length = 5"

(SETQ *PRINT-LENGTH* NIL)
NIL

(SETQ FOO
"Items:~#[none~; ~s~; ~S and ~S~
          ~:;~@{~#[~; and~] ~S~^,~}~].")
"Items:~#[none~; ~s~; ~S and ~S~
          ~:;~@{~#[~; and~] ~S~^,~}~]."

(FORMAT NIL FOO)
"Items:none."

(FORMAT NIL FOO (QUOTE FOO))
"Items: FOO."

(FORMAT NIL FOO (QUOTE FOO) (QUOTE BAR))
"Items: FOO and BAR."

(FORMAT NIL FOO (QUOTE FOO) (QUOTE BAR) (QUOTE BAZ))
"Items: FOO, BAR, and BAZ."

(FORMAT NIL FOO (QUOTE FOO) (QUOTE BAR) (QUOTE BAZ) (QUOTE QUUX))
"Items: FOO, BAR, BAZ, and QUUX."

(FORMAT NIL "The winners are:~{ ~S~}." (QUOTE (FRED HARRY JILL)))
"The winners are: FRED HARRY JILL."

(FORMAT NIL "Pairs:~{ <~S,~S>~}." (QUOTE (A 1 B 2 C 3)))
"Pairs: <A,1> <B,2> <C,3>."

(FORMAT NIL "Pairs:~:{ <~S,~S>~}." (QUOTE ((A 1) (B 2) (C 3))))
"Pairs: <A,1> <B,2> <C,3>."

(FORMAT NIL "Pairs:~@{ <~S,~S>~}." (QUOTE A) 1 (QUOTE B) 2 (QUOTE C)
3)
"Pairs: <A,1> <B,2> <C,3>."

(FORMAT NIL "Pairs:~:@{ <~S,~S>~}." (QUOTE (A 1)) (QUOTE (B 2)) (QUOTE
(C 3)))
"Pairs: <A,1> <B,2> <C,3>."

(SETQ DONESTR "done.~^ ~D warning~:P.~^ ~D error~:P.")
"done.~^ ~D warning~:P.~^ ~D error~:P."

(FORMAT NIL DONESTR)
"done."

(FORMAT NIL DONESTR 3)
"done. 3 warnings."

(FORMAT NIL DONESTR 1 5)
"done. 1 warning. 5 errors."

(SETQ TELLSTR "~@(~@[~R~]~^ ~A.~)")
"~@(~@[~R~]~^ ~A.~)"

(FORMAT NIL TELLSTR 23)
"Twenty-three"

(FORMAT NIL TELLSTR NIL "losers")
" Losers."

(FORMAT NIL TELLSTR 23 "losers")
"Twenty-three losers."

(FORMAT NIL "**~c**" #\SPACE)
#+(or XCL CMU)     "** **"
#+(or CLISP LUCID) "**Space**"

(FORMAT NIL "**~:c**" #\SPACE)
"**Space**"

(FORMAT NIL "**~:@c**" #\SPACE)
"**Space**"

(FORMAT NIL "**~@c**" #\SPACE)
"**#\\Space**"

(FORMAT NIL "**~c**" #\C-SPACE)
#+XCL "** **"
#+(or CLISP LUCID) "**C-Space**"

(FORMAT NIL "**~:c**" #\C-SPACE)
"**Control-Space**"

(FORMAT NIL "**~:@c**" #\C-SPACE)
"**Control-Space**"

(FORMAT NIL "**~@c**" #\C-SPACE)
"**#\\Control-Space**"

(FORMAT NIL "**~c**" #\C-A)
#+XCL "**A**"
#+(or CLISP LUCID) "**C-A**"

(FORMAT NIL "**~:c**" #\C-A)
"**Control-A**"

(FORMAT NIL "**~:@c**" #\C-A)
"**Control-A**"

(FORMAT NIL "**~@c**" #\C-A)
"**#\\Control-A**"

(FORMAT NIL "**~c**" #\A)
"**A**"

(FORMAT NIL "**~:c**" #\A)
"**A**"

(FORMAT NIL "**~:@c**" #\A)
"**A**"

(FORMAT NIL "**~@c**" #\A)
"**#\\A**"

#+XCL (FORMAT NIL "**~c**" (CODE-CHAR 26))
#+XCL "****"
#+CLISP (FORMAT NIL "**~c**" (CODE-CHAR 27))
#+CLISP "**Escape**"

#+XCL (FORMAT NIL "**~:c**" (CODE-CHAR 26))
#+XCL "**Z**"
#+CLISP (FORMAT NIL "**~:c**" (CODE-CHAR 27))
#+CLISP "**Escape**"

#+XCL (FORMAT NIL "**~:@c**" (CODE-CHAR 26))
#+XCL "**^Z**"
#+CLISP (FORMAT NIL "**~:@c**" (CODE-CHAR 27))
#+CLISP "**Escape**"

#+XCL (FORMAT NIL "**~@c**" (CODE-CHAR 26))
#+XCL "**#\\**"
#+CLISP (FORMAT NIL "**~@c**" (CODE-CHAR 27))
#+CLISP "**#\\Escape**"

#+XCL (FORMAT NIL "**~c**" (CODE-CHAR 26 1))
#+XCL "****"
#+CLISP (FORMAT NIL "**~c**" (CODE-CHAR 27 1))
#+CLISP "**C-Escape**"

#+XCL (FORMAT NIL "**~:c**" (CODE-CHAR 26 1))
#+XCL "**Control-**"
#+CLISP (FORMAT NIL "**~:c**" (CODE-CHAR 27 1))
#+CLISP "**Control-Escape**"

#+XCL (FORMAT NIL "**~:@c**" (CODE-CHAR 26 1))
#+XCL "**Control-**"
#+CLISP (FORMAT NIL "**~:@c**" (CODE-CHAR 27 1))
#+CLISP "**Control-Escape**"

#+XCL (FORMAT NIL "**~@c**" (CODE-CHAR 26 1))
#+XCL "**#\\Control-**"
#+CLISP (FORMAT NIL "**~@c**" (CODE-CHAR 27 1))
#+CLISP "**#\\Control-Escape**"

(progn (fmakunbound 'foo)
       (makunbound 'liste)
t)
T

