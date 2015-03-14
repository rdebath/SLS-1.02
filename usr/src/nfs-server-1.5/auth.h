/*
 * auth		This module takes care of request authorization.
 *
 * Version:	@(#)auth.h	1.5	93/04/10
 *
 * Authors:	Mark A. Shand, May 1988
 *		Rick Sladkey, <jrs@world.std.com>
 *		Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *
 *		Copyright 1988 Mark A. Shand
 *		This software maybe be used for any purpose provided
 *		the above copyright notice is retained.  It is supplied
 *		as is, with no warranty expressed or implied.
 */


/* Global AUTH variables. */
extern int promiscuous;
extern int allow_non_root;
extern int auth_initialized;
extern exportnode *export_list;
extern options default_options;
extern clnt_param *clients;
extern clnt_param *unknown_clients;
extern clnt_param *default_client;


/* Global Function prototypes. */
extern _PRO( void auth_init, (char *fname)				);
extern _PRO( clnt_param *auth_clnt, (struct svc_req *rqstp, char *path)	);

/* End of AUTH.H */
