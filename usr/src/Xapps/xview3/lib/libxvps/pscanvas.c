#ifndef lint
#ifdef sccs
static char sccsid[] = "@(#)pscanvas.c	1.25 11/30/90 Copyright 1989, 1990 Sun Microsystems, Inc.";
#endif
#endif lint
/*
 *	(c) Copyright 1989 Sun Microsystems, Inc. Sun design patents 
 *	pending in the U.S. and foreign countries. See LEGAL NOTICE 
 *	file for terms of the license.
 */
#include <xview/notify.h>
#include <xview/xview.h>
#include <xview/textsw.h>
#include "pscan_impl.h"
#include <sys/param.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

int		n_pscans = 0;
char		localhost[MAXHOSTNAMELEN + 10];

extern Display *xv_default_display;


/* Routine below taken from the Wire Service --hjk */
/* Given a hostname, produce a string which can be parsed by the
   ps_open_server() routine.  Returns NULL if there is an error, and sets
   wire_Errno appropriately. */

#define DEFAULT_SERVER_PORT	2000

static char *
make_newshoststring(hostname)
char	*hostname;
{
	static char		server_string[128];
	char			host[128];
	register struct hostent	*hp;
	u_int			port;
	u_long			hostnum;
	
	/* Check NEWSSERVER style.  2173708678.2000;flam. */
	if (sscanf(hostname, "%lu.%u;%s", &hostnum, &port, host) != 3) {
		/* Check for DISPLAY style.  flam:0. */
		if (sscanf(hostname, "%[^:]:%u", host, &port) != 2) {
			strcpy(host, hostname);
			port = DEFAULT_SERVER_PORT;
		} else
			port += DEFAULT_SERVER_PORT;
		if ( (! strcmp(host,"unix") )
		    || (host == NULL))
		  strcpy(host,"localhost");
		if ((hp = gethostbyname(host)) == NULL) {
			return NULL;
		      }
		(void) sprintf(server_string, "%lu.%u;%s\n",
			       ntohl(*(u_long *)hp->h_addr), port,
			       host);
	} else
		strcpy(server_string, hostname);

	return server_string;
}

Pkg_private int
xv_pscanvas_init (owner, pscan_public, avlist)
Xv_Window		owner;
Xv_Window		pscan_public;
Attr_avlist		avlist;
{
	PScanvas_info  *pscan;
	Xv_pscanvas    *pscan_object = (Xv_pscanvas *) pscan_public;
	Display	       *dpy;

	dpy = (Display *) xv_get(owner, XV_DISPLAY);

	if (!PostScript) {
		/* Changed this to open the current X server */
		char	       *server = DisplayString(dpy);

		gethostname(localhost, MAXHOSTNAMELEN);
		if ((server == NULL) || (server[0] == ':')) {
			strcat(localhost, server);
			server = localhost;
		}
		else if (strncmp("unix:", server, 5) == NULL) {
			strcat(localhost, server + 4);
			server = localhost;
		}
		if ((server = make_newshoststring(server)) == NULL)
			return XV_ERROR;
		if ((PostScriptInput = ps_open_server(server)) == 0)
			return XV_ERROR;
		PostScript = psio_getassoc(PostScriptInput);
	}

	/*
	 * Establish our package stack.
	 */
	ps_init_connection();

	/*
	 * Down-load some PS code (see pscan_ps.cps for more info)
	 */
	pscanvas_init_canvas();
	/*
	 * Start up the print loop 
	 */
	pscanvas_start_server_loop();

	if (!(pscan = xv_alloc(PScanvas_info)))
		return XV_ERROR;

	pscan->sync = TRUE;
	pscan->flags = 0;
	pscan->NeWS_canvas = -1;
	pscan->input_proc = NULL;
	pscan->repaint_proc = NULL;
	pscan->resize_proc = NULL;
	pscan->scroll_proc = NULL;
	pscan->public_self = pscan_public;

	pscan_object->private_data = (Xv_opaque) pscan;

	xv_set(pscan_public, WIN_INHERIT_COLORS, TRUE,
	       OPENWIN_AUTO_CLEAR, FALSE, NULL);

	n_pscans++;

	return XV_OK;
}

Pkg_private int
xv_psview_init (owner, psview_public, avlist)
    Xv_Window		owner;
    Xv_Window		psview_public;
    Attr_avlist		avlist;
{
    PSview_info	*psview;
    Xv_pscanvas	*pscan_object = (Xv_pscanvas *) owner;
    Xv_psview		*psview_object = (Xv_psview *) psview_public;
    Window		 xid;

    extern Notify_value psview_event();

    if (! (psview = xv_alloc(PSview_info)))
	return XV_ERROR;

    psview->public_self = psview_public;
    psview->private_pscanvas = PSCANVAS_PRIVATE(pscan_object);

    psview_object->private_data = (Xv_opaque) psview;

    /*
     * REMIND: why do we need both save and immediate here?
     * Will set WIN_CONSUME_EVENTS, WIN_RESIZE later at XV_END_CREATE time.
     */
    xv_set(psview_public,
           WIN_NOTIFY_SAFE_EVENT_PROC, psview_event,
           WIN_NOTIFY_IMMEDIATE_EVENT_PROC, psview_event,
	   OPENWIN_AUTO_CLEAR, FALSE,
	   OPENWIN_SHOW_BORDERS, FALSE,
           0);

    return XV_OK;
}

int ps_token_from_xid(xid, newstoken)
     int xid;
     NeWStoken *newstoken;
{
  pscanvas_token_from_xid(xid, newstoken);
  if ( *newstoken >= 0 ) return TRUE;
  else  return FALSE;
}

int ps_token_from_code(code, newstoken)
     char *code;
     NeWStoken *newstoken;
{
  pscanvas_token_from_code(code, newstoken);
  if ( *newstoken >= 0 ) return TRUE;
  else  return FALSE;
}

void pscanvas_sync()
{
	int		flag = 0;
	char		error[BUFSIZ];

	pscanvas_return_error(error, &flag);

	if (flag)
		xv_error(NULL,
			 ERROR_STRING, error,
			 NULL);
}
