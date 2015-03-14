#set show_mem   1
#set dir_cache   1
set verbosity 2
set prompt      '$cwd 257'
set prompt_tail ' '
#set baud_rate  9600
set sz_rs232_buffer     4096
set sx_remote_cmd       'rz -a'
set rx_remote_cmd       'sz -X'
set gulam_help_file e:\dgulam.hlp
#set histfile   e:\history.g

set path e:\lisp,e:\gnu,e:\gnu\exec

alias rm        'rm -i'
alias ll        'ls -l'
alias dir       'ls -l'

alias edit e:\lisp\tempus.prg
alias dbg e:\profimat\gfa-dbga.prg

#
# gnu ?? setup
#
alias cpp e:\gnu\exec\gcc-cpp
setenv cpp e:\gnu\exec\gcc-cpp.ttp
alias cc1 e:\gnu\exec\gcc-cc1
setenv cc1 e:\gnu\exec\gcc-cc1.ttp
alias gas e:\gnu\exec\gcc-as
setenv gas e:\gnu\exec\gcc-as.ttp
alias as e:\gnu\exec\gcc-as
setenv as e:\gnu\exec\gcc-as.ttp
alias ar e:\gnu\exec\gcc-ar
setenv ar e:\gnu\exec\gcc-ar.ttp
alias ld e:\gnu\exec\gcc-ld
setenv ld e:\gnu\exec\gcc-ld.ttp
alias gcc e:\gnu\exec\gcc
setenv gcc e:\gnu\exec\gcc.ttp
alias cc e:\gnu\exec\gcc
setenv cc e:\gnu\exec\gcc.ttp
alias nm e:\gnu\exec\gcc-nm
setenv nm e:\gnu\exec\gcc-nm.ttp
#
setenv GNULIB e:\gnu\lib
setenv GNUINC e:\gnu\include
setenv GCCEXEC e:\gnu\exec\gcc-
setenv GCC_EXEC_PREFIX e:\gnu\exec\gcc-

setenv TEMP e:\gnu\temp

set env_style mw

alias gdb e:\gnu\exec\gdb
alias make e:\gnu\exec\make

