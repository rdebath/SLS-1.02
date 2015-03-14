/* fake_wait.h - emulate bsd union wait - rick sladkey */

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

