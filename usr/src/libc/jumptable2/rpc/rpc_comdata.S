	.file	"rpc_comdata.c"
gcc2_compiled.:
.globl _svc_fdset
.data
	.align 2
_svc_fdset:
	.long 0
.globl _rpc_createerr
	.align 2
_rpc_createerr:
	.long 0
	.space 12
	.space 16	# room for future use
.globl __null_auth
	.align 2
__null_auth:
	.long 0
	.space 8
