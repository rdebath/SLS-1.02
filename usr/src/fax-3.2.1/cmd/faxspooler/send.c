/*
  This file is part of the NetFax system.

  (c) Copyright 1989 by David M. Siegel. 
      All rights reserved.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation.

    This program is distributed in the hope that it will be useful, 
    but WITHOUT ANY WARRANTY; without even the implied warranty of 
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include <stdio.h>
#include <fcntl.h>
#include <sys/param.h>

#include "../../lib/libfax/libfax.h"
#include "spooler.h"
#include "send.h"

/*
 * This function actually sends off a fax.
 *
 * Return codes:
 *	0	no hard errors, check recip flags for more details.
 *     -1	something failed
 */
int send_fax(fmp, r, qdir, base_filename, pages)
     FaxModem *fmp;
     Recip *r;
     char *qdir;
     char *base_filename;
     int pages;
{
    time_t start;
    int page;
    char *phone;

    /*
     * Delete the privacy notice.
     */
    if (r->phone[0] == '@')
      phone = &r->phone[1];
    else
      phone = r->phone;

    /*
     * Attempt to call the recipient.
     */
    if (faxmodem_initiate_call(fmp, phone) < 0) {
	r->status = SEND_STATUS_FAILED;
	return (0);
    }

    /*
     * If we didn't connect, save the dialer code for possible
     * queries, and return.
     */
    if (!FAX_CONNECTED(fmp)) {
	r->status = SEND_STATUS_DIALER;
	r->dialer_code = fmp->dialer_code;
	return (0);
    }
    start = time(0);

    /*
     * Assume status is OK.  Use this as a flag to break out of the
     * sending loop as well.
     */
    r->status = SEND_STATUS_SENT;

    /*
     * Send all the pages, stopping when done or when an error is
     * detected.
     */
    for (page = 0; page < pages && r->status != SEND_STATUS_FAILED; page++) {
	char pathname[MAXPATHLEN];
	int fd;

	sprintf(pathname, "%s/%s/%d", qdir, base_filename, page);
	if ((fd = open(pathname, O_RDONLY)) < 0)
	  log(L_WARNING, "can't access file: %s\n", pathname);
	else {
	    if (faxmodem_send_page(fmp, fd, page+1 == pages, MAX_PAGE_RETRIES)
		< 0) {
		log(L_WARNING, "send of page %d failed", page);
		r->status = SEND_STATUS_FAILED;
	    }
	    close(fd);
	}
    } 

    /*
     * Force the modem to hangup, though this shouldn't have any
     * effect on whether the fax was sent.
     */
    if (faxmodem_hangup(fmp) < 0)
      log(L_WARNING, "hangup failed");

    /*
     * Record the transmission time.
     */
    r->total_time = time(0) - start;

    return (0);
}
