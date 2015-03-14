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
#include <termios.h>
#include <fcntl.h>

#include "log.h"
#include "c2proto.h"
#include "response.h"
#include "swap.h"
#include "read.h"
#include "write.h"
#include "tty.h"
#include "send.h"

/*
  Maximum amount of data to send to the modem in one write call:
*/
#define SEND_CHUNK 1024

/*
 * Dial a number: ATDT
 *
 * Return codes:
 *	 0	ok, number has been dialed.
 *	-1	dial failed.
 */
static int dial(f, phonenum)
     FaxModem *f;
     char *phonenum;
{ 
    log(L_NOTICE, "dialing: %s", phonenum);

    init_modem_response(f);

    if (fdprintf(f->fd, "ATDT%s\r", phonenum) < 0)
      return (-1);

    if (get_modem_response(f, TIMEOUT_CONNECT) < 0)
      return (-1);

    /*
     * Save away the last numeric result as the result of this
     * dial operation.  This makes it handy to check what
     * happened on the last dial.
     */
    f->dialer_code = f->result;

    return (0);
}

/*
 * Sync and dial to a remote fax machine.
 *
 * Return codes:
 *	 0	connection to remote modem has been established.
 *	-1	connection failed.
 */
int faxmodem_initiate_call(f, dialstring)
     FaxModem *f;
     char *dialstring;
{
    if (faxmodem_sync(f, 10) < 0)
      return (-1);

    /* 
     * Set phase C data bit order.
     *
     * The interfax firmware has a bug with received data in
     * bitreverse=1 mode, so I will leave it in bor=0, and fudge the
     * data in software.
     */
    if (faxmodem_bit_reverse(f, 0) < 0) {
	log(L_NOTICE, "setting modem to bit reverse failed");
	return (-1);
    }

    /*
     * Initiate the call to the remote modem.
     */
    if (dial(f, dialstring) < 0) {
	log(L_NOTICE, "dial failed");
	return (-1);
    }

    return (0);
}

/* 
 * Begin or continue sending: +FDT
 *
 * Send the appropriate commands to the fax modem to start transmission
 * of a page.  After sending the start commands, the modem will respond
 * with a message that ends with CONNECT.  For now, we don't parse any
 * of this message, we just look for the CONNECT.  In addition, the
 * modem will signal to us that we should start sending G3 data by
 * issuing an XON.  Thus, we force an XOFF.  THe subsequent writes
 * to the modem will not start to drain until the XON occurs.  Note
 * that this part of the protocol may change, according to the most
 * recent documention.
 *
 * Set desired transmission params with +FDT=DF,VR,WD,LN
 *   DF = Data Format : 	0  [1-d huffman]
 *   VR = Vertical Res : 	1  [196 dpi (fine)]
 *   WD = width : 		0  [ 1728 pixels]
 *   LN = page length :		2  [ Unlimited ]
 *
 * Return codes:
 *	 0	ok, proceed with sending data
 *	-1	setup failed, not ready to send.
 */
static int start_xmit(f, df, vr, wd, ln)
     FaxModem *f;
     int df, vr, wd, ln;
{
    log(L_NOTICE, "setting xmit params: %d %d %d %d", df, vr, wd, ln);

    if (fdprintf(f->fd, "AT+FDT=%d,%d,%d,%d\r", df, vr, wd, ln) < 0)
      return (-1);

    /*
     * Wait for the output to the modem to complete, and then stop
     * all further output (via an XOFF).  The modem will eventually
     * respond with an XON, allowing the data transmission to begin.
     */
    tcdrain(f->fd);
    ioctl(f->fd, TCXONC, TCOOFF);

    /*
     * Make sure that we get the CONNECT message from the modem.
     */
    return (get_modem_response(f, 120));
}

/*
 * end of page, another page from same document is coming: +FET=0
 * 
 * This transmit page punctuation command is issued after a +FDT has
 * been issued, and indicates that the current page has completed,
 * and another page from the same document is about to be sent.
 *
 * Return codes:
 *	 0	all ok
 *	-1	modem response error
 */
static int end_page(f)
     FaxModem *f;
{
    log(L_NOTICE, "end of current page, another page will be sent");

    if (fdprintf(f->fd, "AT+FET=0\r") < 0)
      return (-1);

    return (get_modem_response(f, TIMEOUT_END_PAGE));
}

/*
 * End of page and end of document: +FET=2
 *
 * This transmit page punctuation command is issued after a +FDT has
 * been issued, and incates that the current page has completed,
 * and the document is complete as well.
 *
 * Return codes:
 *	 0	all ok
 *	-1	modem response error
 */
static int end_xmit(f)
     FaxModem *f;
{ 
    log(L_NOTICE, "end of current page, document is now complete");

    if (fdprintf(f->fd, "AT+FET=2\r") < 0)
      return (-1);

    return (get_modem_response(f, TIMEOUT_END_XMIT));
}

/*
 * When sending data, it is possible to get a CAN message from
 * the remote fax.  This message indicates that the send should
 * be aborted.  The DLE ETX sequence should be sent to the DCE
 * to ack the abort.
 *
 * Return codes:
 *	0	all ok, proceed with sending
 *     -1	send failed, check modem flags for more details
 */
static int process_send_response(f)
     FaxModem *f;
{
    char resp[128];
    int resp_len;
    int i;

    /*
     * We poll the fd for any input.  If none is available, all
     * is fine.  If we get any input at all, it should be a CAN
     * command, as that is all that the protocol allows.  However,
     * to be safe we toss out any other junk that may come in.
     */
    switch (resp_len = pread(f->fd, resp, sizeof(resp))) {
      case -1:
	log(L_ERR, "send_page: poll failed: %m");
	f->status = MODEM_STATUS_FAILED;
	return (-1);
      case 0:
	return (0);
      default:
	log(L_INFO, "process_send_response: got \"%.*s\"", resp_len, resp);
	for (i = 0; i < resp_len; i++) {
	    if ((resp[i] & 0x7f) == CAN) {
		log(L_NOTICE, "remote fax has canceled transmission");
		fdprintf(f->fd, "%c%c", DLE, ETX);
		f->flags |= FAX_F_CANCELED;
		return (-1);
	    }
	}
	return (0);
    }
}

/*
 * Actually send a page.  We assume that we have dialed and connected
 * to the remote fax machine, and that all is ready for starting
 * the actually transmission.
 *
 * Return codes:
 *	 0	page has been succesfully sent
 *	-1	send of page failed.
 */
static int send_page(f, fd, last_page)
     FaxModem *f;
     int fd;
     int last_page;
{
    log(L_INFO, "sending g3 file");

    /*
     * Setup things for transmission.
     */
    if (start_xmit(f, DF_1DHUFFMAN, VR_FINE, WD_1728, LN_UNLIMITED) < 0)
      return (-1);

    /*
     * Allow modem to send ^S to us.  Don't use flow control in the
     * other direction, since we need to pass 8 bits.
     */
    tty_fc(f->fd, FC_OUTPUT_ON);

    /*
     * Now send the page.
     */
    for (;;) {
	unsigned char buf[SEND_CHUNK];
	unsigned char buf_copy[SEND_CHUNK*2];
	int nchars, ochars;
	int i;

	/*
	 * Read in a hunk of data.
	 */
	if ((nchars = read(fd, buf, sizeof(buf))) < 0) {
	    log(L_ERR, "send_page: read failed: %m");
	    tty_fc(f->fd, FC_BOTH_ON);
	    return (-1);
	}
	if (nchars == 0)
	  break;

	/*
	 * Reverse and stuff buffer characters:
	 */
	ochars = 0;
	for (i = 0; i < nchars; i++) {
	    buf_copy[ochars] = swap_bits(buf[i]);
	    if (buf_copy[ochars++] == DLE)
	      buf_copy[ochars++] = DLE;
	}

	/*
	 * Write out the buffer to the modem.
	 */
	if (nwrite(f->fd, (char *)buf_copy, ochars) != ochars) {
	    log(L_ERR, "send_page: write to modem failed: %m");
	    tty_fc(f->fd, FC_BOTH_ON);
	    return (-1);

	}

	/*
	 * Check for any response from the modem.
	 */
	if (process_send_response(f) < 0) {
	    tty_fc(f->fd, FC_BOTH_ON);
	    return (-1);
	}
    }

    fdprintf(f->fd, "%c%c", DLE, ETX);

    tty_fc(f->fd, FC_BOTH_ON);

    if (get_modem_response(f, TIMEOUT_SEND_PAGE) < 0) {
	log(L_NOTICE, "an error sending the file has occured");
	return (-1);
    } else
      log(L_INFO, "send of file has completed");

    if (last_page)
      return (end_xmit(f));
    else
      return (end_page(f));
}

/*
 * Send off a page.  If a retransmit is requested, try up to tries number
 * of times.  If this is the last page of the transmission, set last_page
 * to be true.  The G3 file to send is opened on the given fd.
 *
 * Return codes:
 *	0	the page was successfully sent
 *     -1	the send failed, see the modem flags for more details
 */
int faxmodem_send_page(f, fd, last_page, tries)
     FaxModem *f;
     int fd;
     int last_page;
     int tries;
{
    int i;

    for (i = 0; i < tries; i++) {
	if (send_page(f, fd, last_page) < 0)
	  return (-1);

	/*
	 * If hangup was detected and not last page, then return
	 * with an error condition.
	 */
	if (last_page) {
	    if (FAX_ISSET(f, FAX_F_FHNG) && (f->hangup_code != 0)) {
		log(L_NOTICE, "hangup on last page: %d", f->hangup_code);
		return (-1);
	    }
	} else {
	    if (FAX_ISSET(f, FAX_F_FHNG)) {
		log(L_NOTICE, "hangup code received while sending pages");
		return (-1);
	    }
	}

	/*
	 * Make sure we got a post page response.
	 */
	if (!FAX_ISSET(f, FAX_F_FPTS)) {
	    log(L_NOTICE, "didn't receive post page response");
	    return (-1);
	}

	switch (f->ppr_code) {
	  case PPR_MCF:
	    /* page good */
	    return (0);

	  case PPR_RTN:
	  case PPR_RTP:
	    /* retrans requested for both these cases */
	    break;

	  case PPR_PIN:
	  case PPR_PIP:
	    /* interrupt requested, what do we do here? assume failure */
	    return (-1);

	  default:
	    log(L_ERR, "unknown ppr code received");
	    return (-1);
	}
    }
    
    f->flags |= FAX_F_RETRIES;

    log(L_NOTICE, "failed to successfully send page");

    return (-1);
}

#ifdef DEBUG
FaxModem fm;

int send_test(phone)
     char *phone;
{
    int fd;

    log_set_level(LOG_INFO);
    
    if (faxmodem_open(&fm, "/dev/ttyb") < 0) {
	fprintf(stderr, "open failed\n");
	return (-1);
    }

    if (faxmodem_initiate_call(&fm, phone) < 0 || !FAX_CONNECTED(&fm)) {
	fprintf(stderr, "connection failed\n");
	return (-1);
    }

    if ((fd = open("test.g3.0", O_RDONLY)) < 0) {
	fprintf(stderr, "can't open test file\n");
	faxmodem_close(&fm);
	return (-1);
    }

    if (faxmodem_send_page(&fm, fd, TRUE, 3) < 0)
      fprintf(stderr, "send failed\n");
    else
      fprintf(stderr, "send was successful\n");

    faxmodem_hangup(&fm);
    faxmodem_close(&fm);

    return (0);
}
#endif DEBUG
