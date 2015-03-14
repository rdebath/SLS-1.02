/*
  This file is part of the NetFax system.

  (c) Copyright 1989 by David M. Siegel and Sundar Narasimhan.
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
#include <unistd.h>
#include <termios.h>
#include <fcntl.h>

#include "log.h"
#include "c2proto.h"
#include "response.h"
#include "swap.h"
#include "read.h"
#include "write.h"
#include "tty.h"
#include "recv.h"

/*
 * Answer a call: ATA
 *
 * Return codes:
 *	 0	ok, call has been answered.
 *	-1	answer failed.
 */
int faxmodem_answer(f)
     FaxModem *f;
{  
    log(L_NOTICE, "answering phone");

    init_modem_response(f);

    /* flush any echoes or return codes (from the RING) */
    tcflush(f->fd, TCIFLUSH);

    if (fdprintf(f->fd, "ATA\r") < 0)
      return (-1);

    if (get_modem_response(f, TIMEOUT_ANSWER) < 0)
      return (-1);

    if (faxmodem_bit_reverse(f, 0) < 0)
      return (-1);

    return (0);
}

/* 
 * Receive a page: +FDR
 *
 * The FDR command can return a couple of possible results.  If the
 * connection is valid, it returns at least the messages listed here:
 *   +FCFR
 *   +FDCS: <params>
 *   CONNECT
 * If the other machine has hung up, it returns:
 *   +FHNG: <code>
 * If the connection was not made at all, it returns:
 *    ERROR (numeric code 4)
 *
 * Return codes:
 *     RECV_OK		page data is ready to come
 *     RECV_FAILED	can't receive the page
 *     RECV_DONE	no more pages
 */
recv_code faxmodem_start_recv(f)
     FaxModem *f;
{
    log(L_NOTICE, "asking for remote to send data");

    if (fdprintf(f->fd, "AT+FDR\r") < 0)
      return (RECV_FAILED);

    /*
     * This flags get reset for every connection.
     */
    f->flags &= ~(FAX_F_CONNECT|FAX_F_FDCS);

    /*
     * Make sure that we get the connect message from the modem.
     */
    if (get_modem_response(f, 60) < 0) {
	log(L_NOTICE, "faxmodem_start_recv: didn't receive connect");
	return (RECV_FAILED);
    }

    /*
     * Did we connect, hangup, or fail?
     */
    if (FAX_ISSET(f, FAX_F_CONNECT))
      return (RECV_OK);
    else if (FAX_ISSET(f, FAX_F_FHNG))
      return (RECV_DONE);
    else
      return (RECV_FAILED);
}

static void add_padding(fd)
     int fd;
{
    /*SUPPRESS 442*/
    char *pad = "\000\020\001\000\020\001\000\020\001";

    write(fd, pad, 9);
}

/* 
 * Reads a data stream from the faxmodem, unstuffing DLE characters.
 * Returns the +FET value (2 if no more pages).  Write the stream 
 * out to the given fp.  Returns at the end of a page.
 *
 * Return codes:
 *	0	check modem result code for results
 *     -1	failure, check modem status
 */
int faxmodem_recv_page(f, fd)
     FaxModem *f;
     int fd;
{ 
    int escaped = FALSE;

    log(L_INFO, "receiving a page");

    /*
     * Add a delay, for good measures.  May not be needed.
     */
    sleep(1);

    /* 
     * Send XON to restart, and disable flow control from modem
     * to the Sun so we can receive all 8 bits.  We can still
     * stop the modem if it is sending us data too fast, though.
     */
    tty_fc(f->fd, FC_INPUT_ON);
    tcflow(f->fd, TCION);
    
    /*
     * Reset the post page response flag, since we are now
     * moving to a new page.
     */
    f->flags &= ~FAX_F_FET;

    /*
     * Process the stream, until we get an end of page escape.
     */
    for (;;) {
	unsigned char buf[BUFSIZ];
	unsigned char buf_copy[BUFSIZ];
	int nchars;
	int ochars;
	int i;

	/*
	 * Get a chunk of data.  Note that it is possible for us
	 * to read past the end of the fax data steam.  That is,
	 * we might read some of the modem response.  Thus, we must
	 * pass the remaining stuff to get_modem_response.
	 */
	switch(nchars = tread(f->fd, (char *)buf, sizeof(buf), 120)) {
	  case -1:
	    log(L_ALERT, "read failed in page recv: %m");
	    f->status = MODEM_STATUS_FAILED;
	    tty_fc(f->fd, FC_BOTH_ON);
	    return (-1);
	  case 0:
	    log(L_NOTICE, "read timed out in page recv: %m");
	    f->status = MODEM_STATUS_TIMEOUT;
	    tty_fc(f->fd, FC_BOTH_ON);
	    return (-1);
	  default:
	    break;
	}

	/*
	 * Reverse and unstuff buffer characters.  Check for the DLE
	 * escape.  If a DLE ETX is received, we've got the end  of
	 * the page.  A DLE DLE means to stuff a DLE.
	 */
	ochars = 0;
	for (i = 0; i < nchars; i++) {
	    if (escaped) {
		escaped = FALSE;
		switch (buf[i]) {
		  case DLE:
		    buf_copy[ochars++] = swap_bits(DLE);
		    break;
		  case ETX:
		    log(L_INFO, "received end of page marker");
		    if (ochars > 0)
		      write(fd, (char *)buf_copy, ochars);
		    add_padding(fd);
		    tty_fc(f->fd, FC_BOTH_ON);
		    return (get_modem_response_from_buf(f, TIMEOUT_RECV_PAGE, 
							(char *)&buf[i+1], 
							nchars-i-1));
		  default:
		    log(L_INFO, "dropping: %d", buf[i]);
		    break;
		}
	    } else {
		switch (buf[i]) {
		  case DLE:
		    escaped = TRUE;
		    break;
		  default:
		    buf_copy[ochars++] = swap_bits(buf[i]);
		    break;
		}
	    }
	}
	if (ochars > 0)
	  write(fd, (char *)buf_copy, ochars);
    }
}
