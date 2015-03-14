/*
 * dip		A program for handling dialup IP connecions.
 *
 * Author:      Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *		Copyright 1988-1992 MicroWalt Corporation
 *		This program is free software; you can redistribute it and/or
 *		modify it under the terms of the GNU General Public License
 *		as published by the Free Software Foundation; either version
 *		2 of the License, or (at your option) any later version.
 */
/*
#include <limits.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <arpa/nameser.h>
#include <netinet/in.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <netdb.h>
#include <pwd.h>
#include <resolv.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>
#include <sys/time.h>
#include <syslog.h>
#include <stdio.h>
*/
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/file.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/resource.h>

#include <netinet/in.h>
#include <arpa/inet.h>

#include <errno.h>
#include <signal.h>
#include <netdb.h>
#include <syslog.h>
#include <pwd.h>
#include <stdio.h>
#include <string.h>


/* DIP definitions. */
#define DIP_SFX		".dip"
#define DIP_HOSTS	"/etc/net/diphosts"
#define PID_PATH	"/etc/dip.pid"
#define DIP_SLIP	1		/* we support the SLIP protocol	*/
#define DIP_PPP		0		/* we support the PPP protocol	*/

/* SLIP/PPP/ASK initial protocol configuration. */
#define DEF_MTU		600
#define DEF_PROT	"SLIP"
#define DEF_MODEM	"HAYES"
#define DEF_SPEED	19200
#define DEF_PORT	""

/* Function prototyping according to ANSI. */
#ifndef _PROTOTYPE
#    if _MINIX
#	include <ansi.h>
#    else
#	define _ANSI		0
#	ifdef __STDC__		
#	    if (__STDC__ == 1)	
#	    	undef _ANSI
#	    	define _ANSI	1
#	    endif
#	endif
#	if _ANSI
#	    define	_PROTOTYPE(function, params)	function params
#	else
#	    define	_PROTOTYPE(function, params)	function()
#	endif /* _ANSI */
#    endif /* _MINIX */
#endif	/* _PROTOTYPE */

/* DIP protocol switcher definitions. */
struct protosw {
  char	*name;
  int	type;
  _PROTOTYPE( int (*func), (int, struct in_addr, int) );
};

/* Define some IP layer stuff.  Not all systems have it. */
#define	IP_VERSION	4		/* version# of our IP software	*/
#define	IPF_F_OFFSET	0x1fff		/* Offset field			*/
#define	IPF_DF		0x4000		/* Don't fragment flag		*/
#define	IPF_MF		0x2000		/* More Fragments flag		*/

typedef struct ip_header {
  u_char	v_ihl;			/* Version + IP header length	*/
  u_char	tos;			/* Type of service		*/
  u_short	length;			/* Total length			*/
  u_short	id;			/* Identification		*/
  u_short	fl_offs;		/* Flags + fragment offset	*/
  u_char	ttl;			/* Time to live			*/
  u_char	protocol;		/* Protocol			*/
  u_short	checksum;		/* Header checksum		*/
  u_long	source;			/* Source address		*/
  u_long	dest;			/* Destination address		*/
} IP;
#define	IP_OF_COPIED	0x80		/* Copied-on-fragmentation flag	*/
#define	IP_OF_CLASS	0x60		/* Option class			*/
#define	IP_OF_NUMBER	0x1f		/* Option number		*/
#define	IPO_EOL		0		/* End of options list		*/
#define	IPO_NOOP	1		/* No Operation			*/
#define	IPO_SECURITY	2		/* Security parameters		*/
#define	IPO_LSROUTE	3		/* Loose Source Routing		*/
#define	IPO_TIMESTAMP	4		/* Internet Timestamp		*/
#define	IPO_RROUTE	7		/* Record Route			*/
#define	IPO_STREAMID	8		/* Stream ID			*/
#define	IPO_SSROUTE	9		/* Strict Source Routing	*/
#define	IP_TS_ONLY	0		/* Time stamps only		*/
#define	IP_TS_ADDRESS	1		/* Addresses + Time stamps	*/
#define	IP_TS_PRESPEC	3		/* Prespecified addresses only	*/

/* DIP global variables. */
#ifndef GLOBAL
#   define GLOBAL	extern
#endif
GLOBAL int	opt_i;			/* flag: incoming connection	*/
GLOBAL int	opt_t;			/* flag: dump incoming IP packs */
GLOBAL int	opt_v;			/* flag: be verbose!		*/
GLOBAL char	var_host[128];		/* other side's host name	*/
GLOBAL char	var_modem[128];		/* name of modem we are using	*/
GLOBAL char	var_port[PATH_MAX];	/* terminal line used		*/
GLOBAL long	var_speed;		/* terminal line speed		*/
GLOBAL int	var_prot;		/* which protocol are we using	*/
GLOBAL int	var_mtu;		/* which MTU are we using	*/
GLOBAL struct in_addr var_him;		/* other side's IP address	*/
extern struct protosw protosw[];	/* table with protocol info	*/

/* Global functions. */
_PROTOTYPE( int  do_command, (FILE *file)				);
_PROTOTYPE( int  do_login, (char *user)					);
_PROTOTYPE( int	 do_ppp, (int fd, struct in_addr addr, int mtu)		);
_PROTOTYPE( int	 do_slip, (int fd, struct in_addr addr, int mtu)	);
_PROTOTYPE( void do_term, (void)					);
_PROTOTYPE( int  get_prot, (char *name)					);
_PROTOTYPE( void ip_dump, (char *ptr, int len)				);
_PROTOTYPE( int  mdm_dial, (char *number)				);
_PROTOTYPE( int  mdm_reset, (void)					);
_PROTOTYPE( int  mdm_type, (char *modem)				);
_PROTOTYPE( int  rt_add, (char *name, struct in_addr *dest)		);
_PROTOTYPE( void rt_del, (char *name)					);
_PROTOTYPE( int  tty_askfd, (void)					);
_PROTOTYPE( int  tty_can, (int fd)					);
_PROTOTYPE( int  tty_getc, (void)					);
_PROTOTYPE( int  tty_init, (char *tty_name)				);
_PROTOTYPE( int  tty_putc, (int c)					);
_PROTOTYPE( void tty_puts, (char *bufp)					);
_PROTOTYPE( int  tty_raw, (int fd, int speed, char *mode)		);
_PROTOTYPE( int  tty_speed, (int speed)					);
_PROTOTYPE( void tty_stop, (void)					);

/* End of DIP.H */
