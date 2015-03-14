/*
 * Copyright (c) 1985, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)gethostnamadr.c	6.39 (Berkeley) 1/4/90";
#endif /* LIBC_SCCS and not lint */

#include "inetprivate.h"
#include <syslog.h>

#define	MAXALIASES	35
#define	MAXADDRS	35
#define MAXTRIMDOMAINS  4

#define _PATH_HOSTCONF	__PATH_ETC_INET"/host.conf"

#define SERVICE_NONE	0
#define SERVICE_BIND	1
#define SERVICE_HOSTS	2
#define SERVICE_NIS	3
#define SERVICE_MAX	3

#define CMD_ORDER	"order"
#define CMD_TRIMDOMAIN	"trim"
#define CMD_HMA		"multi"
#define CMD_SPOOF	"nospoof"
#define CMD_SPOOFALERT	"alert"
#define CMD_ON		"on"
#define CMD_OFF		"off"
#define CMD_WARN	"warn"
#define CMD_NOWARN	"warn off"

#define ORD_BIND	"bind"
#define ORD_HOSTS	"hosts"
#define ORD_NIS		"nis"

#define ENV_HOSTCONF	"RESOLV_HOST_CONF"
#define ENV_SERVORDER	"RESOLV_SERV_ORDER"
#define ENV_SPOOF	"RESOLV_SPOOF_CHECK"
#define ENV_TRIM_OVERR	"RESOLV_OVERRIDE_TRIM_DOMAINS"
#define ENV_TRIM_ADD	"RESOLV_ADD_TRIM_DOMAINS"
#define ENV_HMA		"RESOLV_MULTI"

#define TOKEN_SEPARATORS " ,;:"

static int service_order[SERVICE_MAX + 1];
static int service_done = 0;

static char *h_addr_ptrs[MAXADDRS + 1];

static struct hostent host;
static char *host_aliases[MAXALIASES];
static char hostbuf[BUFSIZ+1];
static struct in_addr host_addr;
static char HOSTDB[] = _PATH_HOSTS;
static FILE *hostf = NULL;
static char hostaddr[MAXADDRS];
static char *host_addrs[2];
static int stayopen = 0;
static int hosts_multiple_addrs = 0;
static int spoof = 0;
static int spoofalert = 0;
static char *trimdomain[MAXTRIMDOMAINS];
static char trimdomainbuf[BUFSIZ];
static int numtrimdomains = 0;
#ifdef NIS
static struct hostent *_getnishost();
#endif

#if PACKETSZ > 1024
#define	MAXPACKET	PACKETSZ
#else
#define	MAXPACKET	1024
#endif

typedef union {
    HEADER hdr;
    u_char buf[MAXPACKET];
} querybuf;

typedef union {
    long al;
    char ac;
} align;

#ifdef SHLIB
extern int h_errno;
#else
extern int h_errno;
#endif
extern int errno;

static void
dotrimdomain(c)
char *c;
{
	/* assume c points to the start of a host name; trim off any 
	   domain name matching any of the trimdomains */
	int d,l1,l2;
	
	for(d=0;d<numtrimdomains;d++){
		l1=strlen(trimdomain[d]);
		l2=strlen(c);
		if(l2>l1 && !strcasecmp(c+l2-l1,trimdomain[d]))
			*(c+(strlen(c)-l1))='\0';
	}
}

static struct hostent *
trim_domains(h)
struct hostent *h;
{
	if(numtrimdomains){
		int i;
		dotrimdomain(h->h_name);
		for(i=0;h->h_aliases[i];i++)
			dotrimdomain(h->h_aliases[i]);
	}
	return(h);
}

static void
init_services()
{
	char *cp, *dp, buf[BUFSIZ];
	register int cc = 0;
	FILE *fd;
	char *tdp = trimdomainbuf;
	char *hostconf;

	if(NULL==(hostconf=getenv(ENV_HOSTCONF))){
		hostconf=_PATH_HOSTCONF;
	}
	if ((fd = (FILE *)fopen(hostconf, "r")) == NULL) {
				/* make some assumptions */
		service_order[0] = SERVICE_BIND;
		service_order[1] = SERVICE_NONE;
	} else {
		while (fgets(buf, BUFSIZ, fd) != NULL) {
			if ((cp = rindex(buf, '\n')) != NULL)
				*cp = '\0';
			if (buf[0] == '#')
				continue;

#define checkbuf(b, cmd) (!strncasecmp(b, cmd, strlen(cmd)))

			if (checkbuf(buf, CMD_ORDER)) {
				cp = strpbrk(buf, " \t");
				do {
					while (*cp == ' ' || *cp == '\t')
						cp++;
					dp = strpbrk(cp, TOKEN_SEPARATORS);
					if (dp) *dp = '\0';
					if (checkbuf(cp, ORD_BIND))
						service_order[cc++] = SERVICE_BIND;
					else if (checkbuf(cp, ORD_HOSTS))
						service_order[cc++] = SERVICE_HOSTS;
					else if (checkbuf(cp, ORD_NIS))
						service_order[cc++] = SERVICE_NIS;
					if (dp) cp = ++dp;
				} while (dp != NULL);
			} else if (checkbuf(buf, CMD_HMA)) {
				cp = strpbrk(buf, " \t");
				while (*cp == ' ' || *cp == '\t') cp++;
				if (checkbuf(cp, CMD_ON))
					hosts_multiple_addrs = 1;
			} else if (checkbuf(buf, CMD_SPOOF)) {
				cp = strpbrk(buf, " \t");
				while (*cp == ' ' || *cp == '\t') cp++;
				if (checkbuf(cp, CMD_ON))
					spoof = 1;
			} else if (checkbuf(buf, CMD_SPOOFALERT)) {
				cp = strpbrk(buf, " \t");
				while (*cp == ' ' || *cp == '\t') cp++;
				if (checkbuf(cp, CMD_ON))
					spoofalert = 1;
			} else if (checkbuf(buf, CMD_TRIMDOMAIN)) {
				if(numtrimdomains<MAXTRIMDOMAINS){
					cp = strpbrk(buf, " \t");
					while (*cp == ' ' || *cp == '\t') cp++;
					(void) strcpy(tdp,cp);	
					trimdomain[numtrimdomains++]=tdp;
					tdp += strlen(cp)+1;
				}
			}
		}
		
		service_order[cc] = SERVICE_NONE;
	}
	fclose(fd);
	/* override service_order if environment variable */
	if(NULL!=(cp=getenv(ENV_SERVORDER))){
		cc=0;
		if(NULL!=(cp=strtok(cp, TOKEN_SEPARATORS))){
			do{
				if(checkbuf(cp, ORD_BIND))
					service_order[cc++] = SERVICE_BIND;
				else if (checkbuf(cp, ORD_HOSTS))
					service_order[cc++] = SERVICE_HOSTS;
				else if (checkbuf(cp, ORD_NIS))
					service_order[cc++] = SERVICE_NIS;
			} while(cp=strtok(NULL, TOKEN_SEPARATORS));
		service_order[cc] = SERVICE_NONE;
		}
	}
	/* override spoof if environment variable */
	if(NULL!=(cp=getenv(ENV_SPOOF))){
		if(checkbuf(cp, CMD_WARN)){
			spoof=1;
			spoofalert=1;
		} else if (checkbuf(cp, CMD_OFF)){
			spoof=0;
			spoofalert=0;
		} else if (checkbuf(cp, CMD_NOWARN)){
			spoof=1;
			spoofalert=0;
		} else {
			spoof=1;
		}
	}

	/* override hma if environment variable */
	if(NULL!=(cp=getenv(ENV_HMA)))
		if(checkbuf(cp, CMD_OFF))
			hosts_multiple_addrs=0;
		else
			hosts_multiple_addrs=1;

	/* add trimdomains from environment variable */
	if(NULL!=(cp=getenv(ENV_TRIM_ADD))){
		if(NULL!=(cp=strtok(cp, TOKEN_SEPARATORS))){
			do{
				if(numtrimdomains<MAXTRIMDOMAINS){
					(void)strcpy(tdp, cp);
					trimdomain[numtrimdomains++]=tdp;
					tdp += strlen(cp)+1;
				}
			} while(cp=strtok(NULL, TOKEN_SEPARATORS));
		}
	}

	/* override trimdomains from environment variable */
	if(NULL!=(cp=getenv(ENV_TRIM_OVERR))){
		numtrimdomains=0;
		tdp=trimdomainbuf;
		if(NULL!=(cp=strtok(cp, TOKEN_SEPARATORS))){
			do{
				if(numtrimdomains<MAXTRIMDOMAINS){
					(void)strcpy(tdp, cp);
					trimdomain[numtrimdomains++]=tdp;
					tdp += strlen(cp)+1;
				}
			} while(cp=strtok(NULL, TOKEN_SEPARATORS));
		}
	}
	
	service_done = 1;
}

static struct hostent *
getanswer(answer, anslen, iquery)
	querybuf *answer;
	int anslen;
	int iquery;
{
	register HEADER *hp;
	register u_char *cp;
	register int n;
	u_char *eom;
	char *bp, **ap;
	int type, class, buflen, ancount, qdcount;
	int haveanswer, getclass = C_ANY;
	char **hap;

	eom = answer->buf + anslen;
	/*
	 * find first satisfactory answer
	 */
	hp = &answer->hdr;
	ancount = ntohs(hp->ancount);
	qdcount = ntohs(hp->qdcount);
	bp = hostbuf;
	buflen = sizeof(hostbuf);
	cp = answer->buf + sizeof(HEADER);
	if (qdcount) {
		if (iquery) {
			if ((n = dn_expand((char *)answer->buf, eom,
			     cp, bp, buflen)) < 0) {
				h_errno = NO_RECOVERY;
				return ((struct hostent *) NULL);
			}
			cp += n + QFIXEDSZ;
			host.h_name = bp;
			n = strlen(bp) + 1;
			bp += n;
			buflen -= n;
		} else
			cp += dn_skipname(cp, eom) + QFIXEDSZ;
		while (--qdcount > 0)
			cp += dn_skipname(cp, eom) + QFIXEDSZ;
	} else if (iquery) {
		if (hp->aa)
			h_errno = HOST_NOT_FOUND;
		else
			h_errno = TRY_AGAIN;
		return ((struct hostent *) NULL);
	}
	ap = host_aliases;
	*ap = NULL;
	host.h_aliases = host_aliases;
	hap = h_addr_ptrs;
	*hap = NULL;
#if BSD >= 43 || defined(h_addr)	/* new-style hostent structure */
	host.h_addr_list = h_addr_ptrs;
#endif
	haveanswer = 0;
	while (--ancount >= 0 && cp < eom) {
		if ((n = dn_expand((char *)answer->buf, eom, cp, bp, buflen)) < 0)
			break;
		cp += n;
		type = _getshort(cp);
 		cp += sizeof(u_short);
		class = _getshort(cp);
 		cp += sizeof(u_short) + sizeof(u_long);
		n = _getshort(cp);
		cp += sizeof(u_short);
		if (type == T_CNAME) {
			cp += n;
			if (ap >= &host_aliases[MAXALIASES-1])
				continue;
			*ap++ = bp;
			n = strlen(bp) + 1;
			bp += n;
			buflen -= n;
			continue;
		}
		if (iquery && type == T_PTR) {
			if ((n = dn_expand((char *)answer->buf, eom,
			    cp, bp, buflen)) < 0) {
				cp += n;
				continue;
			}
			cp += n;
			host.h_name = bp;
			return(&host);
		}
		if (iquery || type != T_A)  {
#ifdef DEBUG
			if (_res.options & RES_DEBUG)
				printf("unexpected answer type %d, size %d\n",
					type, n);
#endif
			cp += n;
			continue;
		}
		if (haveanswer) {
			if (n != host.h_length) {
				cp += n;
				continue;
			}
			if (class != getclass) {
				cp += n;
				continue;
			}
		} else {
			host.h_length = n;
			getclass = class;
			host.h_addrtype = (class == C_IN) ? AF_INET : AF_UNSPEC;
			if (!iquery) {
				host.h_name = bp;
				bp += strlen(bp) + 1;
			}
		}

		bp += sizeof(align) - ((u_long)bp % sizeof(align));

		if (bp + n >= &hostbuf[sizeof(hostbuf)]) {
#ifdef DEBUG
			if (_res.options & RES_DEBUG)
				printf("size (%d) too big\n", n);
#endif
			break;
		}
		bcopy((char *)cp, *hap++ = bp, n);
		bp +=n;
		cp += n;
		haveanswer++;
	}
	if (haveanswer) {
		*ap = NULL;
#if BSD >= 43 || defined(h_addr)	/* new-style hostent structure */
		*hap = NULL;
#else
		host.h_addr = h_addr_ptrs[0];
#endif
		return (&host);
	} else {
		h_errno = TRY_AGAIN;
		return ((struct hostent *) NULL);
	}
}

struct hostent *
gethostbyname(const char *name)
{
	querybuf buf;
	register char *cp;
	register int cc;
	int n;
	struct hostent *hp;
	extern struct hostent *_gethtbyname();

	/*
	 * disallow names consisting only of digits/dots, unless
	 * they end in a dot.
	 */
	if (isdigit(name[0]))
		for (cp = name;; ++cp) {
			if (!*cp) {
				if (*--cp == '.')
					break;
				/*
				 * All-numeric, no dot at the end.
				 * Fake up a hostent as if we'd actually
				 * done a lookup.  What if someone types
				 * 255.255.255.255?  The test below will
				 * succeed spuriously... ???
				 */
				if ((host_addr.s_addr = inet_addr(name)) == -1) {
					h_errno = HOST_NOT_FOUND;
					return((struct hostent *) NULL);
				}
				host.h_name = name;
				host.h_aliases = host_aliases;
				host_aliases[0] = NULL;
				host.h_addrtype = AF_INET;
				host.h_length = sizeof(u_long);
				h_addr_ptrs[0] = (char *)&host_addr;
				h_addr_ptrs[1] = (char *)0;
#if BSD >= 43 || defined(h_addr)	/* new-style hostent structure */
				host.h_addr_list = h_addr_ptrs;
#else
				host.h_addr = h_addr_ptrs[0];
#endif
				return (&host);
			}
			if (!isdigit(*cp) && *cp != '.') 
				break;
		}

	if (!service_done)
		init_services();

	for (cc = 0; service_order[cc] != SERVICE_NONE &&
	     cc <= SERVICE_MAX; cc++) {
		switch (service_order[cc]) {
		case SERVICE_BIND:
			if ((n = res_search(name, C_IN, T_A,
					    buf.buf, sizeof(buf))) < 0) {
#ifdef DEBUG
				if (_res.options & RES_DEBUG)
					printf("res_search failed\n");
#endif
			}
			hp = getanswer(&buf, n, 0);
			if (hp) 
				return trim_domains(hp);
			break;
		case SERVICE_HOSTS:
			hp = _gethtbyname(name);
			if (hp)
				return hp;
			h_errno = HOST_NOT_FOUND;
			break;
#ifdef NIS
		case SERVICE_NIS:
			hp = _getnishost(name, "hosts.byname");
			if (hp)
				return hp;
			h_errno = HOST_NOT_FOUND;
			break;
#endif /* NIS */
		}
	}
	return ((struct hostent *) NULL);
}

struct hostent *
gethostbyaddr(const char *addr, int len, int type)
{
	int n;
	querybuf buf;
	register int cc;
	register struct hostent *hp;
	char qbuf[MAXDNAME];
	extern struct hostent *_gethtbyaddr();
	
	if (type != AF_INET)
		return ((struct hostent *) NULL);

	if (!service_done)
	  init_services();

	cc = 0;
	while (service_order[cc] != SERVICE_NONE) {
	        switch (service_order[cc]) {
		case SERVICE_BIND:
			(void)sprintf(qbuf, "%u.%u.%u.%u.in-addr.arpa",
				      ((unsigned)addr[3] & 0xff),
				      ((unsigned)addr[2] & 0xff),
				      ((unsigned)addr[1] & 0xff),
				      ((unsigned)addr[0] & 0xff));
			n = res_query(qbuf, C_IN, T_PTR, (char *)&buf,
				      sizeof(buf));
			if (n < 0) {
#ifdef DEBUG
				if (_res.options & RES_DEBUG)
					printf("res_query failed\n");
#endif
				break;
			}
			hp = getanswer(&buf, n, 1);
			if (hp) {
				if(spoof){
					/* Spoofing check code by
					 * Caspar Dik <casper@fwi.uva.nl> 
					 */
					char nambuf[MAXDNAME+1];
					int ntd, namelen = strlen(hp->h_name);
					char **addrs;
					
					if (namelen >= MAXDNAME)
						return (struct hostent *)NULL;
					(void) strcpy(nambuf,hp->h_name);
					nambuf[namelen] = '.';
					nambuf[namelen+1] = '\0';

					/* 
					 * turn off domain trimming,
					 * call gethostbyname(), then turn	
					 * it back on, if applicable. This
					 * prevents domain trimming from
					 * making the name comparison fail.
					 */
					ntd=numtrimdomains; 
					numtrimdomains=0;
					hp=gethostbyname(nambuf);
					numtrimdomains=ntd;
					nambuf[namelen] = 0;
					/*
					* the name must exist and the name 
					* returned by gethostbyaddr must be 
					* the canonical name and therefore 
					* identical to the name returned by 
					* gethostbyname()
					*/
					if (!hp || strcmp(nambuf, hp->h_name)){
						h_errno = HOST_NOT_FOUND;
						return (struct hostent *)NULL;
					}
					/*
					* now check the addresses
					*/
#if defined(h_addr) || BSD >= 43
					for (addrs = hp->h_addr_list; 
						*addrs; addrs++){
						if (!bcmp(addrs[0], addr, len))
							return trim_domains(hp);
					}
#else
					if (!bcmp(hp->h_addr, addr, len)))
						return trim_domains(hp);
#endif
					/* We've been spoofed */
					h_errno = HOST_NOT_FOUND;
					if(spoofalert){
						openlog("resolv", LOG_PID,
						    LOG_AUTH);
						syslog(LOG_NOTICE,
						    "gethostbyaddr: %s != %u.%u.%u.%u, possible spoof attempt",
						    hp->h_name,
						    ((unsigned)addr[0]&0xff),
						    ((unsigned)addr[1]&0xff),
						    ((unsigned)addr[2]&0xff),
						    ((unsigned)addr[3]&0xff));
					} 
					return (struct hostent *)NULL;
				}
				hp->h_addrtype = type;
				hp->h_length = len;
				h_addr_ptrs[0] = (char *)&host_addr;
				h_addr_ptrs[1] = (char *)0;
				host_addr = *(struct in_addr *)addr;
#if BSD < 43 && !defined(h_addr)	/* new-style hostent structure */
				hp->h_addr = h_addr_ptrs[0];
#endif
				return trim_domains(hp);
			}
			h_errno = HOST_NOT_FOUND;
			break;
		case SERVICE_HOSTS:
			hp = _gethtbyaddr(addr, len, type);
			if (hp) 
				return hp;
			h_errno = HOST_NOT_FOUND;
			break;
#ifdef NIS
		case SERVICE_NIS:
			(void)sprintf(qbuf, "%u.%u.%u.%u",
				      ((unsigned)addr[0] & 0xff),
				      ((unsigned)addr[1] & 0xff),
				      ((unsigned)addr[2] & 0xff),
				      ((unsigned)addr[3] & 0xff));
			hp = _getnishost(qbuf, "hosts.byaddr");
			if (hp)
				return hp;
			h_errno = HOST_NOT_FOUND;
			break;
#endif /* NIS */
		}
		cc++;
	}
	return ((struct hostent *)NULL);
}

void
_sethtent(f)
	int f;
{
	if (hostf == NULL)
		hostf = fopen(HOSTDB, "r" );
	else
		rewind(hostf);
	stayopen |= f;
}

void
_endhtent()
{
	if (hostf && !stayopen) {
		(void) fclose(hostf);
		hostf = NULL;
	}
}

struct hostent *
_gethtent()
{
	char *p;
	register char *cp, **q;

	if (hostf == NULL && (hostf = fopen(HOSTDB, "r" )) == NULL)
		return (NULL);
again:
	if ((p = fgets(hostbuf, BUFSIZ, hostf)) == NULL)
		return (NULL);
	if (*p == '#')
		goto again;
	cp = strpbrk(p, "#\n");
	if (cp == NULL)
		goto again;
	*cp = '\0';
	cp = strpbrk(p, " \t");
	if (cp == NULL)
		goto again;
	*cp++ = '\0';
	/* THIS STUFF IS INTERNET SPECIFIC */
#if BSD >= 43 || defined(h_addr)	/* new-style hostent structure */
	host.h_addr_list = host_addrs;
#endif
	host.h_addr = hostaddr;
	*((u_long *)host.h_addr) = inet_addr(p);
	host.h_length = sizeof (u_long);
	host.h_addrtype = AF_INET;
	while (*cp == ' ' || *cp == '\t')
		cp++;
	host.h_name = cp;
	q = host.h_aliases = host_aliases;
	cp = strpbrk(cp, " \t");
	if (cp != NULL) 
		*cp++ = '\0';
	while (cp && *cp) {
		if (*cp == ' ' || *cp == '\t') {
			cp++;
			continue;
		}
		if (q < &host_aliases[MAXALIASES - 1])
			*q++ = cp;
		cp = strpbrk(cp, " \t");
		if (cp != NULL)
			*cp++ = '\0';
	}
	*q = NULL;
	return (&host);
}

/* if hosts_multiple_addrs set, then gethtbyname behaves as follows:
 *  - for hosts with multiple addresses, return all addresses, such that
 *  the first address is most likely to be one on the same net as the
 *  host we're running on, if one exists. 
 *  - like the dns version of gethostsbyname, the alias field is empty
 *  unless the name being looked up is an alias itself, at which point the
 *  alias field contains that name, and the name field contains the primary
 *  name of the host. Unlike dns, however, this behavior will still take place
 *  even if the alias applies only to one of the interfaces. 
 *  - determining a "local" address to put first is dependant on the netmask 
 *  being such that the least significant network bit is more significant 
 *  than any host bit. Only strange netmasks will violate this. 
 *  - we assume addresses fit into u_longs. That's quite internet specific.
 *  - if the host we're running on is not in the host file, the address 
 *  shuffling will not take place.
 *                     - John DiMarco <jdd@cdf.toronto.edu>
 */ 
struct hostent *
_gethtbyname(const char *name)
{
	register struct hostent *p;
	register char **cp;
	char **hap, **lhap, *bp, *lbp;
	int htbuflen, locbuflen;
	int found=0, localfound=0;
	char localname[MAXHOSTNAMELEN];

	static char htbuf[BUFSIZ+1]; /* buffer for host addresses */
	static char locbuf[BUFSIZ+1]; /* buffer for local hosts's addresses */
	static char *ht_addr_ptrs[MAXADDRS+1];
	static char *loc_addr_ptrs[MAXADDRS+1];
	static struct hostent ht;
	static char *aliases[MAXALIASES];
	static char namebuf[MAXHOSTNAMELEN];
	
	hap = ht_addr_ptrs;
	lhap = loc_addr_ptrs;
	*hap = NULL;
	*lhap = NULL;
	bp=htbuf;
	lbp=locbuf;
	htbuflen = sizeof(htbuf);
	locbuflen = sizeof(locbuf);

	aliases[0]=NULL;
	aliases[1]=NULL;
	(void) strcpy(namebuf, name);

	(void)gethostname(localname, sizeof(localname));

	_sethtent(0);
	while (p = _gethtent()) {
		if (strcasecmp(p->h_name, name) == 0) 
			found++;
		else 
			for (cp = p->h_aliases; *cp != 0; cp++)
				if (strcasecmp(*cp, name) == 0){ 
					found++;
					aliases[0]=name;
					(void) strcpy(namebuf, p->h_name);
				}
		if (strcasecmp(p->h_name, localname) == 0)
			localfound++;
		else
			for (cp=p->h_aliases; *cp != 0; cp++)
				if (strcasecmp(*cp, localname) == 0)
					localfound++;

		if(found){
			int n;

			if(!hosts_multiple_addrs){
				/* original behaviour requested */
				_endhtent();
				return(p);
			}
			n = p->h_length;

			ht.h_addrtype = p->h_addrtype;
			ht.h_length = p->h_length;

			if(n<=htbuflen){
				/* add the found address to the list */
				bcopy(p->h_addr_list[0], bp, n);
				*hap++=bp;
				*hap=NULL;
				bp+=n;
				htbuflen-=n;
			}
			found=0;
		}
		if(localfound){
			int n = p->h_length;
			if(n<=locbuflen){
				/* add the found local address to the list */
				bcopy(p->h_addr_list[0], lbp, n);
				*lhap++=lbp;
				*lhap=NULL;
				lbp+=n;
				locbuflen-=n;
			}
			localfound=0;
		}
	}
	_endhtent();

	if(NULL==ht_addr_ptrs[0]){
		return((struct hostent *)NULL);
	}

	ht.h_aliases = aliases; 
	ht.h_name = namebuf;

	/* shuffle addresses around to ensure one on same net as local host 
	   is first, if exists */
	{
		/* "best" address is assumed to be the one with the greatest
		   number of leftmost bits matching any of the addresses of
		   the local host. This assumes a netmask in which all net
		   bits precede host bits. Usually but not always a fair 
		   assumption. */
 
		/* portability alert: assumption: iaddr fits in u_long.
		   This is really internet specific. */
		int i,j, best=0;
		u_long bestval = (u_long)~0;
		
		for(i=0;loc_addr_ptrs[i];i++){
			for(j=0;ht_addr_ptrs[j];j++){
				u_long t, l, h;
				/* assert(sizeof(u_long)>=ht.h_length); */
				bcopy(loc_addr_ptrs[i], (char *)&t,
					ht.h_length);
				l=ntohl(t);
				bcopy(ht_addr_ptrs[j], (char *)&t, 
					ht.h_length);
				t=l^h;

				if(t<bestval){
					best=j;
					bestval=t;
				}
			}
		}
		if(best){
			char *tmp;

			/* swap first and best address */
			tmp=ht_addr_ptrs[0];
			ht_addr_ptrs[0]=ht_addr_ptrs[best];
			ht_addr_ptrs[best]=tmp;
		}
	}
	
	ht.h_addr_list = ht_addr_ptrs;
	return (&ht);
}

#ifdef NIS
static struct hostent *
_getnishost(name, map)
	char *name, *map;
{
	register char *cp, *dp, **q;
	char *result;
	int resultlen;
	static struct hostent h;
	static char *domain = (char *)NULL;

	if (domain == (char *)NULL)
		if (yp_get_default_domain (&domain))
			return ((struct hostent *)NULL);

	if (yp_match(domain, map, name, strlen(name), &result, &resultlen))
		return ((struct hostent *)NULL);

	if (cp = index(result, '\n'))
		*cp = '\0';

	cp = strpbrk(result, " \t");
	*cp++ = '\0';
#if BSD >= 43 || defined(h_addr)	/* new-style hostent structure */
	h.h_addr_list = host_addrs;
#endif
	h.h_addr = hostaddr;
	*((u_long *)h.h_addr) = inet_addr(result);
	h.h_length = sizeof(u_long);
	h.h_addrtype = AF_INET;
	while (*cp == ' ' || *cp == '\t')
		cp++;
	h.h_name = cp;
	q = h.h_aliases = host_aliases;
	cp = strpbrk(cp, " \t");
	if (cp != NULL)
		*cp++ = '\0';
	while (cp && *cp) {
		if (*cp == ' ' || *cp == '\t') {
			cp++;
			continue;
		}
		if (q < &host_aliases[MAXALIASES - 1])
			*q++ = cp;
		cp = strpbrk(cp, " \t");
		if (cp != NULL)
			*cp++ = '\0';
	}
	*q = NULL;
	return (&h);
}
#endif /* NIS */

struct hostent *
_gethtbyaddr(const char *addr, int len, int type)
{
	register struct hostent *p;

	_sethtent(0);
	while (p = _gethtent())
		if (p->h_addrtype == type && !bcmp(p->h_addr, addr, len))
			break;
	_endhtent();
	return (p);
}
