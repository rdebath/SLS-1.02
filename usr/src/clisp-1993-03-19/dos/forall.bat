@echo off
rem Usage: FORALL command arg-with-wildcards other-args...
rem to run commands that expect single file names and no wildcards
rem on a set of files specified by a wildcard expression
rem Example: forall lzexe *.exe  -->  lzexe x1.exe ; lzexe x2.exe ; ...
rem Juergen Weber 19.5.1991
if %2X==X goto USAGE
if %1X==X goto USAGE
for %%f in (%2) do call %1 %%f %3 %4
goto EXIT
:USAGE
 echo usage:  forall FILEtoRUN WildArg
:EXIT
