	.file	"_res.c"
gcc2_compiled.:
.data
	.align 2
__res:
	.long 5
	.long 4
	.long 704
	.long 1
	.space 336
	.space 0x20	# for future use
.data

_h_errno:
	.long 0
#ifndef NO_JUMP_TABLE
.data

_rexecoptions:
	.long 0
#endif
