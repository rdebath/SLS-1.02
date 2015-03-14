if EXIST count.obj goto l1
cc1 COUNT
if errorlevel 1 goto done
cc2 COUNT
if errorlevel 1 goto done
cc3 COUNT
if errorlevel 1 goto done
cc4 COUNT
if errorlevel 1 goto done
:l1
if EXIST countlib.obj goto l2
cc1 COUNTLIB
if errorlevel 1 goto done
cc2 COUNTLIB
if errorlevel 1 goto done
cc3 COUNTLIB
if errorlevel 1 goto done
cc4 COUNTLIB
if errorlevel 1 goto done
:l2
if EXIST endgame.obj goto l3
cc1 ENDGAME
if errorlevel 1 goto done
cc2 ENDGAME
if errorlevel 1 goto done
cc3 ENDGAME
if errorlevel 1 goto done
cc4 ENDGAME
if errorlevel 1 goto done
:l3
if EXIST eval.obj goto l4
cc1 EVAL
if errorlevel 1 goto done
cc2 EVAL
if errorlevel 1 goto done
cc3 EVAL
if errorlevel 1 goto done
cc4 EVAL
if errorlevel 1 goto done
:l4
if EXIST exambord.obj goto l5
cc1 EXAMBORD
if errorlevel 1 goto done
cc2 EXAMBORD
if errorlevel 1 goto done
cc3 EXAMBORD
if errorlevel 1 goto done
cc4 EXAMBORD
if errorlevel 1 goto done
:l5
if EXIST findcolr.obj goto l6
cc1 FINDCOLR
if errorlevel 1 goto done
cc2 FINDCOLR
if errorlevel 1 goto done
cc3 FINDCOLR
if errorlevel 1 goto done
cc4 FINDCOLR
if errorlevel 1 goto done
:l6
if EXIST findnext.obj goto l7
cc1 FINDNEXT
if errorlevel 1 goto done
cc2 FINDNEXT
if errorlevel 1 goto done
cc3 FINDNEXT
if errorlevel 1 goto done
cc4 FINDNEXT
if errorlevel 1 goto done
:l7
if EXIST findopen.obj goto l8
cc1 FINDOPEN
if errorlevel 1 goto done
cc2 FINDOPEN
if errorlevel 1 goto done
cc3 FINDOPEN
if errorlevel 1 goto done
cc4 FINDOPEN
if errorlevel 1 goto done
:l8
if EXIST findpatn.obj goto l9
cc1 FINDPATN
if errorlevel 1 goto done
cc2 FINDPATN
if errorlevel 1 goto done
cc3 FINDPATN
if errorlevel 1 goto done
cc4 FINDPATN
if errorlevel 1 goto done
:l9
if EXIST findsavr.obj goto l10
cc1 FINDSAVR
if errorlevel 1 goto done
cc2 FINDSAVR
if errorlevel 1 goto done
cc3 FINDSAVR
if errorlevel 1 goto done
cc4 FINDSAVR
if errorlevel 1 goto done
:l10
if EXIST findwinr.obj goto l11
cc1 FINDWINR
if errorlevel 1 goto done
cc2 FINDWINR
if errorlevel 1 goto done
cc3 FINDWINR
if errorlevel 1 goto done
cc4 FINDWINR
if errorlevel 1 goto done
:l11
if EXIST fioe.obj goto l12
cc1 FIOE
if errorlevel 1 goto done
cc2 FIOE
if errorlevel 1 goto done
cc3 FIOE
if errorlevel 1 goto done
cc4 FIOE
if errorlevel 1 goto done
:l12
if EXIST genmove.obj goto l13
cc1 GENMOVE
if errorlevel 1 goto done
cc2 GENMOVE
if errorlevel 1 goto done
cc3 GENMOVE
if errorlevel 1 goto done
cc4 GENMOVE
if errorlevel 1 goto done
:l13
if EXIST getij.obj goto l14
cc1 GETIJ
if errorlevel 1 goto done
cc2 GETIJ
if errorlevel 1 goto done
cc3 GETIJ
if errorlevel 1 goto done
cc4 GETIJ
if errorlevel 1 goto done
:l14
if EXIST getmove.obj goto l15
cc1 GETMOVE
if errorlevel 1 goto done
cc2 GETMOVE
if errorlevel 1 goto done
cc3 GETMOVE
if errorlevel 1 goto done
cc4 GETMOVE
if errorlevel 1 goto done
:l15
if EXIST initmark.obj goto l16
cc1 INITMARK
if errorlevel 1 goto done
cc2 INITMARK
if errorlevel 1 goto done
cc3 INITMARK
if errorlevel 1 goto done
cc4 INITMARK
if errorlevel 1 goto done
:l16
if EXIST main.obj goto l17
cc1 MAIN
if errorlevel 1 goto done
cc2 MAIN
if errorlevel 1 goto done
cc3 MAIN
if errorlevel 1 goto done
cc4 MAIN
if errorlevel 1 goto done
:l17
if EXIST matchpat.obj goto l18
cc1 MATCHPAT
if errorlevel 1 goto done
cc2 MATCHPAT
if errorlevel 1 goto done
cc3 MATCHPAT
if errorlevel 1 goto done
cc4 MATCHPAT
if errorlevel 1 goto done
:l18
if EXIST opening.obj goto l19
cc1 OPENING
if errorlevel 1 goto done
cc2 OPENING
if errorlevel 1 goto done
cc3 OPENING
if errorlevel 1 goto done
cc4 OPENING
if errorlevel 1 goto done
:l19
if EXIST openregn.obj goto l20
cc1 OPENREGN
if errorlevel 1 goto done
cc2 OPENREGN
if errorlevel 1 goto done
cc3 OPENREGN
if errorlevel 1 goto done
cc4 OPENREGN
if errorlevel 1 goto done
:l20
if EXIST random.obj goto l21
cc1 RANDOM
if errorlevel 1 goto done
cc2 RANDOM
if errorlevel 1 goto done
cc3 RANDOM
if errorlevel 1 goto done
cc4 RANDOM
if errorlevel 1 goto done
:l21
if EXIST seed.obj goto l22
cc1 SEED
if errorlevel 1 goto done
cc2 SEED
if errorlevel 1 goto done
cc3 SEED
if errorlevel 1 goto done
cc4 SEED
if errorlevel 1 goto done
:l22
if EXIST sethand.obj goto l23
cc1 SETHAND
if errorlevel 1 goto done
cc2 SETHAND
if errorlevel 1 goto done
cc3 SETHAND
if errorlevel 1 goto done
cc4 SETHAND
if errorlevel 1 goto done
:l23
if EXIST showbord.obj goto l24
cc1 SHOWBORD
if errorlevel 1 goto done
cc2 SHOWBORD
if errorlevel 1 goto done
cc3 SHOWBORD
if errorlevel 1 goto done
cc4 SHOWBORD
if errorlevel 1 goto done
:l24
if EXIST showinst.obj goto l25
cc1 SHOWINST
if errorlevel 1 goto done
cc2 SHOWINST
if errorlevel 1 goto done
cc3 SHOWINST
if errorlevel 1 goto done
cc4 SHOWINST
if errorlevel 1 goto done
:l25
if EXIST suicide.obj goto l26
cc1 SUICIDE
if errorlevel 1 goto done
cc2 SUICIDE
if errorlevel 1 goto done
cc3 SUICIDE
if errorlevel 1 goto done
cc4 SUICIDE
if errorlevel 1 goto done
:l26
link @objs, gnugo,,d:\lang\c\c86\c86sas.lib
if errorlevel 1 goto err
goto allok
:done
pause error in compilation
goto allok
:err
pause error in linking
:allok
