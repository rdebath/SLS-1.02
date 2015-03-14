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

#include "log.h"
#include "c2proto.h"
#include "response.h"
#include "read.h"
#include "write.h"
#include "gen.h"

int faxmodem_open(f, filename)
     FaxModem *f;
     char *filename;
{
    return (f->fd = tty_open(filename));
}

int faxmodem_close(f)
     FaxModem *f;
{
    return (tty_close(f->fd));
}

int faxmodem_sync(f, total_tries)
     FaxModem *f;
     int total_tries;
{
    int tries;

    log(L_INFO, "syncing with %d tries", total_tries);

    for (tries = 0; tries < total_tries; tries++) {
	char buf[128];
	int count;

	/* Send XON just in case data stream was left off from last page */
	tcflow(f->fd, TCOON);
	tcflow(f->fd, TCION);

	/*
	 * Clean out the fd.
	 */
	tcflush(f->fd, TCIOFLUSH);

	/*
	 * Command +FCLASS=2 puts us in class 2 faxmodem mode
	 * and supposedly resets other parameters
	 *
	 *	V0	  - result codes are digits
	 *	Q0	  - result codes are sent
	 *	E0	  - do not echo
	 *      M0        - speaker off
	 *	S0=0	  - dont automatically answer phone
	 *	S2=255	  - disable escape character
	 *	S12=255	  - longest possible escape guard time
	 *      +FCLASS=2 - enable faxmodem commands
	 *      S7=120    - wait 120 seconds for carrier event
	 *	+FCR=1    - enable fax reception
	 */
	fdprintf(f->fd, "ATZ\r");
	tcdrain(f->fd); sleep(1);
	fdprintf(f->fd, "ATV0Q0E0M0S0=0S2=255S12=255 +FCLASS=2\r");
	tcdrain(f->fd); sleep(1);
	fdprintf(f->fd, "ATS7=120 +FCR=1\r");
	tcdrain(f->fd); sleep(1);

	/* flush any echoes or return codes */
	tcflush(f->fd, TCIFLUSH);

	/* now see if the modem is talking to us properly */
	fdprintf(f->fd, "AT\r");
	count = tfdgets(f->fd, buf, sizeof(buf), 2);
	if (count < 0) {
	    log(L_ERR, "read failed: %m");
	    return (-1);
	} else if (count == 0) {
	    log(L_NOTICE, "read timeout, tries=%d", tries);
	    continue;
	} else {
	    if (strncmp(buf, "0\r", 2) != 0) {
		log(L_NOTICE, "bad modem response: \"%.*s\", tries=%d", 
		    count, buf, tries);
	    } else {
		log(L_INFO, "modem is now in sync");
		return (0);
	    }
	}

	/* wait a little while between tries */
	sleep(5);
    }

    log(L_NOTICE, "cannot sync with fax modem");

    return (-1);
}

/*
 * Force the modem to hangup: ATH
 *
 * Usually, this would be called at the very end of a session,
 * to insure that the modem goes back on hook.
 *
 * Return codes:
 *	 0	ok, hangup has been issued.
 *	-1	hangup failed.
 */
int faxmodem_hangup(f)
     FaxModem *f;
{
    log(L_NOTICE, "hanging up the modem");
    
    if (fdprintf(f->fd, "ATH\r") < 0)
      return (-1);

    return (get_modem_response(f, TIMEOUT_HANGUP));
}

/*
 * Bit reversal options: +FBOR
 */
int faxmodem_bit_reverse(f, code)
     FaxModem *f;
     int code;
{
    log(L_NOTICE, "enabling bit reversal");

    if (fdprintf(f->fd, "AT+FBOR=%d\r", code) < 0)
      return (-1);
  
    return (get_modem_response(f, TIMEOUT_BIT_REVERSE));
}

/* 
 * Bit reversal options: +FREL
 *   0 = data is bit aligned as received
 *   1 = dat is byte aligned at EOLS
 */
int faxmodem_byte_align(f, code)
     FaxModem *f;
     int code;
{
    log(L_NOTICE, "setting byte alignment to mode %d", code);

    if (fdprintf(f->fd, "AT+FREL=%d\r", code) < 0)
      return (-1);
  
    return (get_modem_response(f, TIMEOUT_BYTE_ALIGN));
}

/* 
 * 8.5.1.1 DCE capabilities parameters, +FDCC
 *   Write Syntax: +FDCC=VR,BR,LN,DF,EC,BF,ST
 *   Default Values: 1,3,2,2,0,0,0,0
 *
 * This just sets the VR and BR, currently.
 */
int faxmodem_set_capabilities(f, vr, br)
     FaxModem *f;
     int vr, br;
{
    log(L_NOTICE, "setting DCE capabilities: %d %d", vr, br);

    if (fdprintf(f->fd, "AT+FDCC=%d,%d\r", vr, br) < 0)
      return (-1);
  
    return (get_modem_response(f, TIMEOUT_SET_CAPABILITIES));
}  
