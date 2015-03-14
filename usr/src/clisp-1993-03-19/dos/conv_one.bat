@echo off
rem converts its argument from Latin-1 character set to PC character set
cv_lt_pc < %1 > convert.tmp
copy convert.tmp %1 > NUL
del convert.tmp
