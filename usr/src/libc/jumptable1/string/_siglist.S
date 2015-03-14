	.file	"_siglist.c"
.globl __sys_siglist
.text	1
LC0:
	.ascii "Unknown signal\0"
LC1:
	.ascii "Hangup\0"
LC2:
	.ascii "Interrupt\0"
LC3:
	.ascii "Quit\0"
LC4:
	.ascii "Illegal instruction\0"
LC5:
	.ascii "Trace/breakpoint trap\0"
LC6:
	.ascii "IOT trap/Abort\0"
LC7:
	.ascii "Unused signal\0"
LC8:
	.ascii "Floating point exception\0"
LC9:
	.ascii "Killed\0"
LC10:
	.ascii "User defined signal 1\0"
LC11:
	.ascii "Segmentation fault\0"
LC12:
	.ascii "User defined signal 2\0"
LC13:
	.ascii "Broken pipe\0"
LC14:
	.ascii "Alarm clock\0"
LC15:
	.ascii "Terminated\0"
LC16:
	.ascii "Stack fault\0"
LC17:
	.ascii "Child exited\0"
LC18:
	.ascii "Continued\0"
LC19:
	.ascii "Stopped (signal)\0"
LC20:
	.ascii "Stopped\0"
LC21:
	.ascii "Stopped (tty input)\0"
LC22:
	.ascii "Stopped (tty output)\0"
LC23:
	.ascii "Possible I/O\0"
LC24:
	.ascii "CPU time limit exceeded\0"
LC25:
	.ascii "File size limit exceeded\0"
LC26:
	.ascii "Virtual time alarm\0"
LC27:
	.ascii "Profile signal\0"
LC28:
	.ascii "Window size changed\0"
LC29:
	.ascii "File lock lost\0"
.text
	.align 2
__sys_siglist:
	.long LC0
	.long LC1
	.long LC2
	.long LC3
	.long LC4
	.long LC5
	.long LC6
	.long LC7
	.long LC8
	.long LC9
	.long LC10
	.long LC11
	.long LC12
	.long LC13
	.long LC14
	.long LC15
	.long LC16
	.long LC17
	.long LC18
	.long LC19
	.long LC20
	.long LC21
	.long LC22
	.long LC23
	.long LC24
	.long LC25
	.long LC26
	.long LC27
	.long LC28
	.long LC29
	.long LC7
	.long LC7
	.long 0
	.space	0x40,0x90	# for future use
