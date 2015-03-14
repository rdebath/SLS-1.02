/*
 * asm.h -- definitions for preprocessing .S assembler files
 */

#define BPW     4			/* bytes per word */
#define LBPW    2
#define ARG(x)  BPW * (x + 1)(%ebp)	/* macro to access arguments */
