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
#include <strings.h>

#include "log.h"
#include "c2proto.h"
#include "read.h"
#include "response.h"

static int parse_int_response(), parse_id_response(), parse_t30_response();

#define arg(f)  ((int)(&(((FaxModem *)NULL)->f)))
#define sarg(f) ((int)(((FaxModem *)NULL)->f))

static struct _parse_switch {
    int flag;
    char *string;
    int (*func)();
    int offset;
} parse_switch[] = {
    {FAX_F_FCON, "+FCON", NULL, NULL},
    {FAX_F_FNSF, "+FNSF", NULL, NULL},
    {FAX_F_FCFR, "+FCFR", NULL, NULL},
    {FAX_F_FHNG, "+FHNG", parse_int_response, arg(hangup_code)},
    {FAX_F_FPTS, "+FPTS", parse_int_response, arg(ppr_code)},
    {FAX_F_FET,  "+FET",  parse_int_response, arg(ppm_code)},
    {FAX_F_FTSI, "+FTSI", parse_id_response,  sarg(ftsi_id)},
    {FAX_F_FCSI, "+FCSI", parse_id_response,  sarg(fcsi_id)},
    {FAX_F_FCSI, "+FCIG", parse_id_response,  sarg(fcig_id)},
    {FAX_F_FDCS, "+FDCS", parse_t30_response, arg(fdcs_params)},
    {FAX_F_FDCS, "+FDIS", parse_t30_response, arg(fdis_params)},
    {FAX_F_FDCS, "+FDTC", parse_t30_response, arg(fdtc_params)},
};
#define parse_switch_len (sizeof(parse_switch)/sizeof(struct _parse_switch))

#undef arg
#undef sarg

static int parse_t30_response(reply, offset)
     char *reply;
     char *offset;
{
    T30params *params = (T30params *)offset;

    if (strlen(reply) < 6)
      return (-1);

    if (sscanf(reply+6, "%d,%d,%d,%d,%d,%d,%d,%d",
	       &params->vr, &params->br, &params->wd, &params->ln,
	       &params->df, &params->ec, &params->bf, &params->st) != 8)
      return (-1);
    
    return (0);
}

static int parse_id_response(reply, str)
     char *reply;
     char *str;
{
    char *fquote, *lquote;

    if ((fquote = index(reply, '"')) == NULL)
      return (-1);

    if ((lquote = rindex(reply, '"')) == NULL)
      return (-1);

    if (fquote == lquote)
      return (-1);

    *lquote = '\0';
    strcpy(str, fquote+1);

    return (0);
}

static int parse_int_response(reply, offset)
     char *reply;
     char *offset;
{
    int *value = (int *)offset;
    char *start = index(reply, ':');

    if (start == NULL)
      return (-1);

    if (sscanf(start+1, "%d", value) != 1)
      return (-1);

    return (0);
}

/*
  Parse a text response from the modem.  Update the flag for the response,
  and process the message according to the switch, as defined above.
*/
static int parse_text_response(f, reply)
     FaxModem *f;
     char *reply;
{
    int i;

    log(L_INFO, "parse_text_response: parsing: \"%s\"", reply);

    for (i = 0; i < parse_switch_len; i++) {
	struct _parse_switch *p = &parse_switch[i];
	if (strncmp(p->string, reply, strlen(p->string)) == 0) {
	    f->flags |= p->flag;
	    if (p->func != NULL) {
		if ((*p->func)(reply, &((char *)f)[p->offset]) < 0)
		  log(L_NOTICE, "parse_text_response: error processing \"%s\"",
		      reply);
	    }
	    return (0);
	}
    }
    
    log(L_NOTICE, "parse_text_response: parse failed: \"%s\"", reply);
    
    return (-1);
}

typedef enum {
    RESPONSE_START,
    RESPONSE_NUMERIC,
    RESPONSE_TEXT,
    RESPONSE_CONNECT,
    RESPONSE_DONE,
} response_state;

/*
 * This function parses numeric and connect responses from the faxmodem.
 * It is assumed that the modem is in numeric response mode.  We first
 * parse the response from the passed in buf, and then we read more
 * from the modem, if necessary.  We do this since some routines do
 * read ahead.
 *
 * INPUTS: serial_fd   a serial stream file descriptor from which to read
 *         response    a faxmodem_response structure
 *
 * It fills in any relevant slots of the faxmodem_response structure
 * which is passed to it. 
 *
 * Responses are a single digit followed by <CR>.
 */
int get_modem_response_from_buf(f, timeout, buf, bufsize)
     FaxModem *f;
     int timeout;
     char *buf;
     int bufsize;
{
    response_state state = RESPONSE_START;
    char reply[1024];
    char *reply_ptr;
    char c;

    log(L_DEBUG, "get_modem_reponse: entering with timeout %d", timeout);

    /* start with a clean status */
    f->status = MODEM_STATUS_OK;

    /*
     * Read in all responses, and figure out if we are getting a number
     * result code (RESPONSE_NUMERIC) or text (RESPONSE_TEXT).
     */
    while (state != RESPONSE_DONE) {
	int count;

	if (bufsize > 0) {
	    c = *(buf++);
	    bufsize--;
	    count = 1;
	} else
	  count = tread(f->fd, &c, 1, timeout);
	
	switch (count) {
	  case -1:
	    f->status = MODEM_STATUS_FAILED;
	    return (-1);
	  case 0:
	    f->status = MODEM_STATUS_TIMEOUT;
	    return (-1);
	  default:
	    break;
	}
	
	switch (state) {
	  case RESPONSE_START: 
	    reply_ptr = reply;
	    if (isdigit(c))  {
		f->result = (int)(c - '0');
		state = RESPONSE_NUMERIC;
	    } else if (c == '\r' || c == '\n') {
		state = RESPONSE_START;
	    } else {
		*reply_ptr++ = c;
		state = RESPONSE_TEXT;
	    }
	    break;

	  case RESPONSE_NUMERIC: 
	    if (c == '\r') {
		log(L_INFO, "numeric response code: %d", f->result);
		state = RESPONSE_DONE;
	    } else {
		*reply_ptr++ = c;
		state = RESPONSE_TEXT;
	    }
	    break;

	  case RESPONSE_TEXT:
	    if (c == '\r' || c == '\n') {
		state = RESPONSE_START;
		*reply_ptr++ = '\0';
		if (strncmp(reply, "CONNECT", 7) == 0)
		  state = RESPONSE_CONNECT;
		else
		  parse_text_response(f, reply);
	    } else
	      *reply_ptr++ = c;
	    break;

	  case RESPONSE_CONNECT:
	    if (c != '\n')
	      log(L_WARNING, "invalid connect message");
	    else
	      log(L_INFO, "received connect response");
	    f->flags |= FAX_F_CONNECT;
	    f->result = 0;
	    state = RESPONSE_DONE;
	    break;

	  default: 
	    log(L_EMERG, "get_modem_response: illegal state");
	    abort();
	}
    }

    return (0);
}

/*
 * Since most routines are in sync with the modem, they simply call
 * this convenience function.
 */
int get_modem_response(f, timeout)
     FaxModem *f;
     int timeout;
{
    return (get_modem_response_from_buf(f, timeout, NULL, 0));
}

/*
 * This function clears the state of the modem, and should only
 * be called when starting a new fax session.  Other than that,
 * the FaxModem structure will always hold the most complete
 * information available for the current fax session.
 */
void init_modem_response(f)
     FaxModem *f;
{
    f->status = MODEM_STATUS_OK;
    f->flags = 0;
    f->result = 0;
    f->dialer_code = 0;
}

/* ARGSUSED */
void faxmodem_print_status(f)
     FaxModem *f;
{
}
