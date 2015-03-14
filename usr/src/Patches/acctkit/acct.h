#ifndef __LINUX_ACCT_H
#define __LINUX_ACCT_H

struct acct
{
	long ac_exitcode;
	unsigned long ac_utime;
	unsigned long ac_stime;
	unsigned long ac_btime;
	unsigned short ac_tty;
	unsigned short ac_uid;
	unsigned short ac_gid;
	char ac_comm[9];
	char ac_flag;
#define ASU	1
#define AFORK	2
};

#define AHZ     100

#endif
