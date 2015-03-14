	.file	"curses.c"
gcc2_compiled.:
.globl __echoit
.data
__echoit:
	.byte 1
.globl __rawmode
__rawmode:
	.byte 0
.globl _My_term
_My_term:
	.byte 0
.globl __endwin
__endwin:
	.byte 0
.globl _ttytype
_ttytype:
	.ascii "\0"
	.space 49
.globl _Def_term
.text
LC0:
	.ascii "unknown\0"
.data
	.align 2
_Def_term:
	.long LC0
.globl __tty_ch
	.align 2
__tty_ch:
	.long 0
.globl _LINES
	.align 2
_LINES:
	.long 0
.globl _COLS
	.align 2
_COLS:
	.long 0
.globl __res_iflg
	.align 2
__res_iflg:
	.long 0
.globl __res_lflg
	.align 2
__res_lflg:
	.long 0
.globl _stdscr
	.align 2
_stdscr:
	.long 0
.globl _curscr
	.align 2
_curscr:
	.long 0
.globl __tty
	.align 2
__tty:
	.long 0
	.long 0
	.long 0
	.long 0
	.byte 0
	.ascii "\0"
	.space 16
	.space 2
.globl _AM
_AM:
	.byte 0
.globl _BS
_BS:
	.byte 0
.globl _CA
_CA:
	.byte 0
.globl _DA
_DA:
	.byte 0
.globl _DB
_DB:
	.byte 0
.globl _EO
_EO:
	.byte 0
.globl _HC
_HC:
	.byte 0
.globl _HZ
_HZ:
	.byte 0
.globl _IN
_IN:
	.byte 0
.globl _MI
_MI:
	.byte 0
.globl _MS
_MS:
	.byte 0
.globl _NC
_NC:
	.byte 0
.globl _NS
_NS:
	.byte 0
.globl _OS
_OS:
	.byte 0
.globl _UL
_UL:
	.byte 0
.globl _XB
_XB:
	.byte 0
.globl _XN
_XN:
	.byte 0
.globl _XT
_XT:
	.byte 0
.globl _XS
_XS:
	.byte 0
.globl _XX
_XX:
	.byte 0
.globl _AL
	.align 2
_AL:
	.long 0
.globl _BC
	.align 2
_BC:
	.long 0
.globl _BT
	.align 2
_BT:
	.long 0
.globl _CD
	.align 2
_CD:
	.long 0
.globl _CE
	.align 2
_CE:
	.long 0
.globl _CL
	.align 2
_CL:
	.long 0
.globl _CM
	.align 2
_CM:
	.long 0
.globl _CR
	.align 2
_CR:
	.long 0
.globl _CS
	.align 2
_CS:
	.long 0
.globl _DC
	.align 2
_DC:
	.long 0
.globl _DL
	.align 2
_DL:
	.long 0
.globl _DM
	.align 2
_DM:
	.long 0
.globl _DO
	.align 2
_DO:
	.long 0
.globl _ED
	.align 2
_ED:
	.long 0
.globl _EI
	.align 2
_EI:
	.long 0
.globl _K0
	.align 2
_K0:
	.long 0
.globl _K1
	.align 2
_K1:
	.long 0
.globl _K2
	.align 2
_K2:
	.long 0
.globl _K3
	.align 2
_K3:
	.long 0
.globl _K4
	.align 2
_K4:
	.long 0
.globl _K5
	.align 2
_K5:
	.long 0
.globl _K6
	.align 2
_K6:
	.long 0
.globl _K7
	.align 2
_K7:
	.long 0
.globl _K8
	.align 2
_K8:
	.long 0
.globl _K9
	.align 2
_K9:
	.long 0
.globl _HO
	.align 2
_HO:
	.long 0
.globl _IC
	.align 2
_IC:
	.long 0
.globl _IM
	.align 2
_IM:
	.long 0
.globl _IP
	.align 2
_IP:
	.long 0
.globl _KD
	.align 2
_KD:
	.long 0
.globl _KE
	.align 2
_KE:
	.long 0
.globl _KH
	.align 2
_KH:
	.long 0
.globl _KL
	.align 2
_KL:
	.long 0
.globl _KR
	.align 2
_KR:
	.long 0
.globl _KS
	.align 2
_KS:
	.long 0
.globl _KU
	.align 2
_KU:
	.long 0
.globl _LL
	.align 2
_LL:
	.long 0
.globl _MA
	.align 2
_MA:
	.long 0
.globl _ND
	.align 2
_ND:
	.long 0
.globl _NL
	.align 2
_NL:
	.long 0
.globl _RC
	.align 2
_RC:
	.long 0
.globl _SC
	.align 2
_SC:
	.long 0
.globl _SE
	.align 2
_SE:
	.long 0
.globl _SF
	.align 2
_SF:
	.long 0
.globl _SO
	.align 2
_SO:
	.long 0
.globl _SR
	.align 2
_SR:
	.long 0
.globl _TA
	.align 2
_TA:
	.long 0
.globl _TE
	.align 2
_TE:
	.long 0
.globl _TI
	.align 2
_TI:
	.long 0
.globl _UC
	.align 2
_UC:
	.long 0
.globl _UE
	.align 2
_UE:
	.long 0
.globl _UP
	.align 2
_UP:
	.long 0
.globl _US
	.align 2
_US:
	.long 0
.globl _VB
	.align 2
_VB:
	.long 0
.globl _VS
	.align 2
_VS:
	.long 0
.globl _VE
	.align 2
_VE:
	.long 0
.globl _AL_PARM
	.align 2
_AL_PARM:
	.long 0
.globl _DL_PARM
	.align 2
_DL_PARM:
	.long 0
.globl _UP_PARM
	.align 2
_UP_PARM:
	.long 0
.globl _DOWN_PARM
	.align 2
_DOWN_PARM:
	.long 0
.globl _LEFT_PARM
	.align 2
_LEFT_PARM:
	.long 0
.globl _RIGHT_PARM
	.align 2
_RIGHT_PARM:
	.long 0
.globl _PC
_PC:
	.byte 0
.globl _GT
_GT:
	.byte 0
.globl _NONL
_NONL:
	.byte 0
.globl _UPPERCASE
_UPPERCASE:
	.byte 0
.globl _normtty
_normtty:
	.byte 0
.globl __pfast
__pfast:
	.byte 0
