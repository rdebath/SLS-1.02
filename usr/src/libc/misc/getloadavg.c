From daemon Fri Oct 23 02:19 PDT 1992
Received: from sun2.nsfnet-relay.ac.uk by dns1.eecs.wsu.edu (16.6/5.910402)
	id AA22066; Fri, 23 Oct 92 02:19:18 -0700
Via: uk.ac.cambridge.mrc-applied-psychology; Fri, 23 Oct 1992 10:18:32 +0100
Received: from menkar.mrc-apu.cam.ac.uk by sirius.mrc-apu.cam.ac.uk 
          with UK-Sendmail (4.1/UK-2.1-APU); Fri, 23 Oct 92 10:18:05 BST
Message-Id: <4517.9210230918@menkar.mrc-apu.cam.ac.uk>
To: hlu@eecs.wsu.edu
Subject: Re: mkstemp
In-Reply-To: Your message of "Thu, 22 Oct 92 13:44:50 PDT." <9210222044.AA25828@yardbird>
Date: Fri, 23 Oct 92 10:18:04 +0100
From: Mitchum DSouza <mitchum.dsouza@mrc-apu.cam.ac.uk>
Status: OR

Hi,

Thank you, mkstemp() works fine. I tried it with sendmail and it works....
well somewhat.

I've got a few questions about debugging and shared libraries.
I don't know if you know much about sendmail but here goes.
When I build sendmail with shared libraries (jump 4.1), and attempt to build a
frozen configuration file and the send some mail, sendmail coredumps. However
if I make it a static binary everything works fine. The question is is how do
I debug the shared version ?? I can't debug a shared version as -g forces
static ?? or am i wrong ? I know quite a bit about C but not much about how to
debug a core dump ??

Also if you put sendmail in deamon mode, (sendmail -bd), and incoming mail is
recieved from say another Sun, then the forked process becomes defunct (as
shown by ps) and a coredump is left in /usr/spool/mqueue. This happens for
either the shared or static binary. So this question is - how do I debug a
process that forks ?? i.e. a deamon ?

Any ideas would be greatly appreciated.

Thanx in advance.

P.S. here is getloadavg() from sendmail (but doesn't work on Linux)
I don't know much about the kernel to make this work. Any ideas ??



/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getloadavg.c	6.1 (Berkeley) 5/29/89";
#endif LIBC_SCCS and not lint

#include <sys/param.h>
#include <sys/types.h>
#include <sys/file.h>

/*#include <nlist.h>*/
#include <paths.h>
#define _PATH_KMEM "/dev/kmem"
#define _PATH_UNIX "/dos/image"

static char *kmem = _PATH_KMEM;
static char *vmunix = _PATH_UNIX;

static struct nlist nl[] = {
	{ "_averunnable" },
#define	X_AVERUNNABLE	0
	{ "_fscale" },
#define	X_FSCALE	1
	{ "" },
};

/*
 *  getloadavg() -- Get system load averages.
 *
 *  Put `nelem' samples into `loadavg' array.
 *  Return number of samples retrieved, or -1 on error.
 */
getloadavg(loadavg, nelem)
	double loadavg[];
	int nelem;
{
	off_t lseek();
	static int need_nlist = 1;
	u_long averunnable[3];
	int fscale, kmemfd, i;

	/* nlist() is slow; cache result */
	if (need_nlist) {
		if (nlist(vmunix, nl) != 0)
			return (-1);
		if (nl[X_AVERUNNABLE].n_type == 0 || nl[X_FSCALE].n_type == 0)
			return (-1);
		need_nlist = 0;
	}

	if ((kmemfd = open(kmem, O_RDONLY, 0)) < 0)
		return (-1);
	if (lseek(kmemfd, (off_t)nl[X_AVERUNNABLE].n_value, L_SET) == -1)
		goto bad;
	if (read(kmemfd, (char *)averunnable, sizeof(averunnable)) < 0)
		goto bad;
	if (lseek(kmemfd, (off_t)nl[X_FSCALE].n_value, L_SET) == -1)
		goto bad;
	if (read(kmemfd, (char *)&fscale, sizeof(fscale)) < 0)
		goto bad;
	(void) close(kmemfd);

	nelem = MIN(nelem, sizeof(averunnable) / sizeof(averunnable[0]));
	for (i = 0; i < nelem; i++)
		loadavg[i] = (double) averunnable[i] / fscale;
	return (nelem);

bad:
	(void) close(kmemfd);
	return (-1);
}

