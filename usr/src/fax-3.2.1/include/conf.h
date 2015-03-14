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

/*
 * The default serial port that the faxmodem is connected to:
 */
#define FAX_DEVICE	    "/dev/fax"

/*
 * The service that clients use for connecting to the daemon:
 */
#define FAX_SERVICE	    "1122"

/*
 * The default fax spooler host:
 */
#define FAX_HOST	    "wheat-chex.ai.mit.edu"

/*
 * The queue directories:
 */
#define OUTGOING_QUEUE 	    "/com/fax/outgoing"
#define INCOMING_QUEUE      "/com/fax/incoming"

/*
 * Base filename for sequence files:
 */
#define SEQ_FILE   	    ".seq"

/*
 * Maximum time to try to deliver a fax, in seconds:
 */
#define MAX_DELIVERY_TIME   60*60*24

/*
 * Minimum number of seconds to wait between delivery attempts:
 */
#define MIN_RETRY_WAIT	    60*5

/*
 * Number of retrys (requested by remote fax) allowed for each page:
 */
#define MAX_PAGE_RETRIES    3

/*
 * Mail notification of incoming faxes to this address.
 */
#define INCOMING_EMAIL_ADDR "incoming-fax-notification@ai.mit.edu"

/*
 * How many times should I try to sync the modem?
 */
#define FAXMODEM_SYNC_TRIES 1000

/*
 * Command for enqueuing faxes:
 */
#define FAX_ENQ_PROG	"/usr/local/lib/fax/faxenq"

/*
 * Command for spooling a Postscript file to the fax system:
 */
#define FAX_PS_PROG	"/usr/local/bin/faxps"

/*
 * Command for postscript interperter:
 *
 * This depends on the digifax driver being compiled into Ghostscript.
 */
#define PS_PROG  "/usr/local/bin/gs -dNOPAUSE -q -sDEVICE=dfaxhigh "

/*
 * Postscript program to convert to ppm format files:
 */
#define PS_TO_PPM_PROG	"/usr/local/lib/fax/pstofaxbits.ps"

/*
 * Postscript program for generating a cover sheet:
 */
#define PS_TO_COVER_PROG "/usr/local/lib/fax/coverpage.ps"

/*
 * Command for converting from ppm to G3 format files:
 */
#define PPM_TO_G3_PROG	"/usr/local/lib/fax/ppmtog3"
