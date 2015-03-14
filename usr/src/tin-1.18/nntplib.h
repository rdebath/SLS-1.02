/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : nntplib.h
 *  Author    : I.Lea
 *  Created   : 01-04-91
 *  Updated   : 18-11-92
 *  Notes     : nntp.h 1.5.11/1.6 with extensions for tin & CD-ROM
 *  Copyright : You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#ifndef	NNTP_SERVER_FILE
#	define	NNTP_SERVER_FILE	"/etc/nntpserver"
#endif

#define	NNTP_TCP_NAME	"nntp"
#define	NNTP_TCP_PORT	((unsigned short) 119)

#ifndef	SMTP_SERVER_FILE
#	define	SMTP_SERVER_FILE	"/etc/smtpserver"
#endif

#define	SMTP_TCP_NAME	"smtp"
#define	SMTP_TCP_PORT	((unsigned short) 25)

/*
 *  External routine declarations
 */

extern char *getserverbyfile();
/* extern int server_init(); */
extern int get_tcp_socket();
extern int handle_server_response();
extern void put_server();
extern int get_server();
extern void close_server();

/*
 *  External file descriptors for the server connection
 */

extern FILE *nntp_wr_fp;

/*
 * Response codes for NNTP server
 *
 * @(#)Header: nntp.h,v 1.81 92/03/12 02:08:31 sob Exp $
 *
 * First digit:
 *
 *	1xx	Informative message
 *	2xx	Command ok
 *	3xx	Command ok so far, continue
 *	4xx	Command was correct, but couldn't be performed
 *		for some specified reason.
 *	5xx	Command unimplemented, incorrect, or a
 *		program error has occured.
 *
 * Second digit:
 *
 *	x0x	Connection, setup, miscellaneous
 *	x1x	Newsgroup selection
 *	x2x	Article selection
 *	x3x	Distribution
 *	x4x	Posting
 */

#define	CHAR_INF	'1'
#define	CHAR_OK		'2'
#define	CHAR_CONT	'3'
#define	CHAR_ERR	'4'
#define	CHAR_FATAL	'5'

#define	INF_HELP		100	/* Help text on way */
#define	INF_AUTH		180	/* Authorization capabilities */
#define	INF_DEBUG		199	/* Debug output */

#define	OK_CANPOST		200	/* Hello; you can post */
#define	OK_NOPOST		201	/* Hello; you can't post */
#define	OK_SLAVE		202	/* Slave status noted */
#define	OK_GOODBYE		205	/* Closing connection */
#define	OK_GROUP		211	/* Group selected */
#define	OK_GROUPS		215	/* Newsgroups follow */

#define OK_XMOTD		217	/* News motd follows */
#define OK_XINDEX		218	/* Tin style index follows */
#define OK_XOVERVIEW		219	/* .overview style index follows */

#define	OK_ARTICLE		220	/* Article (head & body) follows */
#define	OK_HEAD			221	/* Head follows */
#define	OK_BODY			222	/* Body follows */
#define	OK_NOTEXT		223	/* No text sent -- stat, next, last */
#define	OK_NEWNEWS		230	/* New articles by message-id follow */
#define	OK_NEWGROUPS		231	/* New newsgroups follow */
#define	OK_XFERED		235	/* Article transferred successfully */
#define	OK_POSTED		240	/* Article posted successfully */
#define	OK_AUTHSYS		280	/* Authorization system ok */
#define	OK_AUTH			281	/* Authorization (user/pass) ok */
#define OK_BIN			282	/* binary data follows */
#define OK_SPLIST		283	/* spooldir list follows */
#define OK_SPSWITCH		284	/* Switching to a different spooldir */
#define OK_SPNOCHANGE		285	/* Still using same spooldir */
#define OK_SPLDIRCUR		286	/* Current spooldir */
#define OK_SPLDIRAVL		287	/* Available spooldir */
#define OK_SPLDIRERR		288	/* Unavailable spooldir or invalid entry */

#define CONT_XFER		335	/* Continue to send article */
#define	CONT_POST		340	/* Continue to post article */
#define	NEED_AUTHINFO		380	/* authorization is required */
#define	NEED_AUTHDATA		381	/* <type> authorization data required */

#define	ERR_GOODBYE		400	/* Have to hang up for some reason */
#define	ERR_NOGROUP		411	/* No such newsgroup */
#define	ERR_NCING		412	/* Not currently in newsgroup */

#define ERR_XMOTD		417	/* No news motd file */
#define ERR_XINDEX		418	/* No tin index for this group */
#define ERR_XOVERVIEW		419	/* No .overview index for this group */

#define	ERR_NOCRNT		420	/* No current article selected */
#define	ERR_NONEXT		421	/* No next article in this group */
#define	ERR_NOPREV		422	/* No previous article in this group */
#define	ERR_NOARTIG		423	/* No such article in this group */
#define ERR_NOART		430	/* No such article at all */
#define ERR_GOTIT		435	/* Already got that article, don't send */
#define ERR_XFERFAIL		436	/* Transfer failed */
#define	ERR_XFERRJCT		437	/* Article rejected, don't resend */
#define	ERR_NOPOST		440	/* Posting not allowed */
#define	ERR_POSTFAIL		441	/* Posting failed */
#define	ERR_NOAUTH		480	/* authorization required for command */
#define	ERR_AUTHSYS		481	/* Authorization system invalid */
#define	ERR_AUTHREJ		482	/* Authorization data rejected */
#define ERR_INVALIAS		483	/* Invalid alias on spooldir cmd */
#define ERR_INVNOSPDIR		484	/* No spooldir file found */

#define	ERR_COMMAND		500	/* Command not recognized */
#define	ERR_CMDSYN		501	/* Command syntax error */
#define	ERR_ACCESS		502	/* Access to server denied */
#define ERR_FAULT		503	/* Program fault, command not performed */
#define	ERR_AUTHBAD		580	/* Authorization Failed */
	
/*
 * RFC 977 defines this; don't change it.
 */

#define	NNTP_STRLEN		512
