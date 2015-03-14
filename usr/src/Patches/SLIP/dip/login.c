/*
 * dip		A program for handling dialup IP connecions.
 *		Incoming connection handling module.
 *
 * Author:      Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *		Copyright 1988-1992 MicroWalt Corporation
 *		This program is free software; you can redistribute it and/or
 *		modify it under the terms of the GNU General Public License
 *		as published by the Free Software Foundation; either version
 *		2 of the License, or (at your option) any later version.
 */
#include "dip.h"


struct dip {
  char			name[16];		/* login name of host	*/
  char			passwd[10];		/* any external passwd	*/
  char			host[128];		/* hostname of host	*/
  struct in_addr	ip;			/* mapped IP address	*/
  char			comment[128];		/* any comments		*/
  char			protocol[16];		/* protocol to use	*/
  int			mtu;			/* MTU to use for conn.	*/
};


static _PROTOTYPE( struct dip *getdipnam, (char *)			);


static struct dip *getdipnam(who)
char *who;
{
  static struct dip dip;
  char buff[1024];
  register FILE *fp;
  register char *sp;
  char *bp;

  if ((fp = fopen(DIP_HOSTS, "r")) == (FILE *)NULL) {
	fprintf(stderr, "dip: cannot open %s\n", DIP_HOSTS);
	syslog(LOG_ERR, "cannot open %s", DIP_HOSTS);
	return((struct dip *)NULL);
  }

  while(fgets(buff, 1024, fp) != (char *)NULL) {
	if ((sp = strchr(buff, '\n')) != (char *)NULL) *sp = '\0';
	if (buff[0] == '#' || buff[0] == '\0') continue;
	sp = buff;
	bp = sp;
	while (*sp && *sp != ':') sp++;
	*sp++ = '\0';
	if (strcmp(bp, who)) continue;
	strncpy(dip.name, bp, 16);
	bp = sp;
	while (*sp && *sp != ':') sp++;
	*sp++ = '\0';
	strncpy(dip.passwd, bp, 8);
	bp = sp;
	while (*sp && *sp != ':') sp++;
	*sp++ = '\0';
	strncpy(dip.host, bp, 128);
	bp = sp;
	while (*sp && *sp != ':') sp++;
	*sp++ = '\0';
	strncpy(dip.comment, bp, 128);
	bp = sp;
	while (*sp && *sp != ':' && *sp != ',') sp++;
	*sp++ = '\0';
	strncpy(dip.protocol, bp, 16);
	bp = sp;
	while (*sp && *sp != ',') sp++;
	*sp++ = '\0';
	dip.mtu = atoi(bp);

	(void) fclose(fp);
	return(&dip);
  }
  (void) fclose(fp);
  return((struct dip *)NULL);
}


int do_login(name)
char *name;
{
  struct dip *dip;
  struct hostent *hp;
  int i;
  extern char *h_errlist[];
  extern int h_errno;

  (void) openlog("dip", LOG_PID, LOG_DAEMON);

  dip = getdipnam(name);
  if (dip == (struct dip *)NULL) {
	fprintf(stderr, "You do not have DIP access.  Go away.\n");
	syslog(LOG_WARNING, "%s tried to access DIP: no access!", name);
	return(-1);
  }

  /* Resolve the this caller's host name to an IP address. */
  if ((hp = gethostbyname(dip->host)) == (struct hostent *)NULL) {
	syslog(LOG_ERR, "unknown host %s: %s\n",
			dip->host, h_errlist[-h_errno]);
	herror(var_host);
	return(-1);
  }
  strncpy(dip->host, hp->h_name, 128);
  memcpy((char *) &dip->ip, (char *) hp->h_addr_list[0], hp->h_length);

  /* Find out which protocol we have to use. */
  if ((i = get_prot(dip->protocol)) == 0) {
	fprintf(stderr, "dip: unknown protocol %s\n", dip->protocol);
	syslog(LOG_ERR, "%s wants unknown protocol %s",
					dip->host, dip->protocol);
	return(-1);
  }

  /* Show some info. */
  if (opt_v == 1) {
	printf("Hostname: \"%s\" [%s]\n", dip->host, inet_ntoa(dip->ip));
	printf("Comments: \"%s\"\n", dip->comment);
	printf("Protocol: \"%s\"\n", dip->protocol);
	printf("IP MTU  : %d\n", dip->mtu);
  }
  syslog(LOG_INFO, "%s connected as %s (%s) with %s/%d",
	dip->name, dip->host, inet_ntoa(dip->ip), dip->protocol, dip->mtu);

  printf("starting %s\n", dip->protocol);
  (void) fflush(stdout);
  i = (*protosw[i - 1].func)(0, dip->ip, dip->mtu);
  return(i);
}
