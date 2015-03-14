/* emulate BSD union wait */

#include_next <sys/wait.h>

union wait {
	unsigned int w_status;
	struct {
		unsigned int __w_termsig:7;
		unsigned int __w_coredump:1;
		unsigned int __w_retcode:8;
		unsigned int __w_padding:16;
	} __wait_bits;
};

#define w_termsig	__wait_bits.__w_termsig
#define w_coredump	__wait_bits.__w_coredump
#define w_retcode	__wait_bits.__w_retcode

/* dirty trick to allow W* macros to work with union wait or integer */

#define _wait_val(s)	({ typeof(s) _s_tmp = (s); *(int *)&_s_tmp; })

#undef WIFEXITED
#define WIFEXITED(s)	(!(_wait_val(s)&0xFF))
#undef WIFSTOPPED
#define WIFSTOPPED(s)	((_wait_val(s)&0xFF)==0x7F)
#undef WEXITSTATUS
#define WEXITSTATUS(s)	((_wait_val(s)>>8)&0xFF)
#undef WTERMSIG
#define WTERMSIG(s)	(_wait_val(s)&0x7F)
#undef WCOREDUMP
#define WCOREDUMP(s)	(_wait_val(s)&0x80)
#undef WSTOPSIG
#define WSTOPSIG(s)	((_wait_val(s)>>8)&0xFF)
#undef WIFSIGNALED
#define WIFSIGNALED(s)	(_wait_val(s)-1 & 0xFFFF) < 0xFF)

