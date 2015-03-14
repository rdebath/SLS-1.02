/*
 * dip		A program for handling dialup IP connecions.
 *		This program handles the connections needed for dialup
 *		IP links, like SLIP or PPP.  It can handle both incoming
 *		and outgoing connections, using password security for
 *		incoming connections.  The outgoing connections use the
 *		system's dial(3) library if possible.
 *
 * Usage:	dip [-itv] [-m mtu] [-p proto] [telno | script]
 *
 * Version:	3.1.0	12/10/92
 *
 * Author:      Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *		Copyright 1988-1992 MicroWalt Corporation
 *		This program is free software; you can redistribute it and/or
 *		modify it under the terms of the GNU General Public License
 *		as published by the Free Software Foundation; either version
 *		2 of the License, or (at your option) any later version.
 */
#define GLOBAL
#include "dip.h"


#define VERSION	"3.1.0"


static char *Version = "@(#) dip 3.1.0 (12/10/92)";


struct protosw protosw[] = {
#if DIP_SLIP
  { "SLIP",	1,	do_slip		},
#endif
#if DIP_PPP
  { "PPP",	3,	do_ppp		},
#endif
  (char *)NULL
};


static _PROTOTYPE( void usage, (void)					);


extern int getopt(), opterr, optind;
extern char *optarg;


int get_prot(name)
char *name;
{
  register int i, j;

  i = 0;
  j = atoi(name);
  while(protosw[i].name != (char *)NULL) {
	if (!strcmp(protosw[i].name, name)) return(protosw[i].type);
	if (protosw[i].type == j) return(protosw[i].type);
	i++;
  }
  return(0);
}


void usage()
{
  fprintf(stderr, "Usage: dip [-itv] [-m mtu] [-p proto] [telno | script]\n");
  exit(-1);
}


void main(argc, argv)
int argc;
char *argv[];
{
  char path[128];
  struct passwd *pw;
  FILE *fp;
  int i, pid;
  register int s;
  register char *sp;

  /* Set program defaults. */
  strcpy(path, "");
  opt_t = 0;
  opt_v = 0;
  opt_i = 0;
  strcpy(var_modem, DEF_MODEM);
  strcpy(var_port, DEF_PORT);
  var_speed = DEF_SPEED;
  var_prot = get_prot(DEF_PROT);
  var_mtu = DEF_MTU;

  /* Scan command line for any arguments. */
  opterr = 0;
  while ((s = getopt(argc, argv, "im:p:tv")) != EOF) switch(s) {
	case 'i':
		opt_i = 1 - opt_i;
		break;
	case 'm':
		var_mtu = atoi(optarg);
		if (var_mtu <= 0 || var_mtu > 32767) usage();
		break;
	case 'p':
		var_prot = get_prot(optarg);
		if (var_prot == 0) usage();
		break;
	case 't':
		opt_t = 1 - opt_t;
		break;
	case 'v':
		opt_v = 1 - opt_v;
		break;
	default:
		usage();
  }
  if (opt_v) {
	  printf("DIP: Dialup IP Protocol Driver version %s\n", VERSION);
	  printf("Written by Fred N. van Kempen, MicroWalt Corporation.\n\n");
  }

  openlog("slip", LOG_PID|LOG_NOWAIT, LOG_DAEMON);

  if (opt_t == 1) {
	if (optind != argc) usage();
	s = do_command(stdin);
  } else if (opt_i == 1) {
	if (optind == argc) {
		pw = getpwuid(getuid());
		if (pw == (struct passwd *)NULL) {
			syslog(LOG_WARNING, "You don't exist. Go away!\n");
			exit(-1);
		} else sp = pw->pw_name;
	} else sp = argv[optind];
	s = do_login(sp);
  } else {
	if (optind != (argc - 1)) usage();
	strncpy(path, argv[optind], 128);
	if ((sp = strrchr(path, '/')) != (char *)NULL) sp++;
	  else sp = path;
	if (strchr(sp, '.') == (char *)NULL) strcat(path, DIP_SFX);

	if ((fp = fopen(path, "r")) == (FILE *)NULL) {
		syslog(LOG_WARNING, "dip: %s: %s\n", path, strerror(errno));
		exit(-1);
	}
	(void) setbuf(fp, (char *)NULL);
	s = do_command(fp);
	(void) fclose(fp);
  }

  /* All done.  Close up and exit. */
  tty_stop();

  exit(0);
}
