/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)authenc.c	5.1 (Berkeley) 3/1/91";
#endif /* not lint */

#if	defined(ENCRYPT) || defined(AUTHENTICATE)
#include <sys/types.h>
#include <arpa/telnet.h>
#include <libtelnet/encrypt.h>
#include <libtelnet/misc.h>

#include "general.h"
#include "ring.h"
#include "externs.h"
#include "defines.h"
#include "types.h"

	int
net_write(str, len)
	unsigned char *str;
	int len;
{
	if (NETROOM() > len) {
		ring_supply_data(&netoring, str, len);
		if (str[0] == IAC && str[1] == SE)
			printsub('>', &str[2], len-2);
		return(len);
	}
	return(0);
}

	void
net_encrypt()
{
#if	defined(ENCRYPT)
	if (encrypt_output)
		ring_encrypt(&netoring, encrypt_output);
	else
		ring_clearto(&netoring);
#endif
}

	int
telnet_spin()
{
	return(-1);
}

	char *
telnet_getenv(val)
	char *val;
{
	return((char *)env_getvalue((unsigned char *)val));
}

	char *
telnet_gets(prompt, result, length, echo)
	char *prompt;
	char *result;
	int length;
	int echo;
{
	extern char *getpass();
	extern int globalmode;
	int om = globalmode;
	char *res;

	TerminalNewMode(-1);
	if (echo) {
		printf("%s", prompt);
		res = fgets(result, length, stdin);
	} else if (res = getpass(prompt)) {
		strncpy(result, res, length);
		res = result;
	}
	TerminalNewMode(om);
	return(res);
}
#endif
