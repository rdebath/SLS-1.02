/*
5/6/91 DJB baseline authuser 3.1. Public domain.
*/

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <errno.h>
#include <ctype.h>
extern int errno;
#include "authuser.h"

unsigned short auth_tcpport = 113;

#define SIZ 500 /* various buffers */

static int usercmp(u,v)
register char *u;
register char *v;
{
 /* is it correct to consider Foo and fOo the same user? yes */
 /* but the function of this routine may change later */
 while (*u && *v)
   if (tolower(*u) != tolower(*v))
     return tolower(*u) - tolower(*v);
   else
     ++u,++v;
 return *u || *v;
}

static char authline[SIZ];

char *auth_xline(user,fd,in)
register char *user; /* the supposed name of the user, NULL if unknown */
register int fd; /* the file descriptor of the connection */
register unsigned long *in;
{
 unsigned short local;
 unsigned short remote;
 register char *ruser;

 if (auth_fd(fd,in,&local,&remote) == -1)
   return 0;
 ruser = auth_tcpuser(*in,local,remote);
 if (!ruser)
   return 0;
 if (!user)
   user = ruser; /* forces X-Auth-User */
 (void) sprintf(authline,
	 (usercmp(ruser,user) ? "X-Forgery-By: %s" : "X-Auth-User: %s"),
	 ruser);
 return authline;
}

int auth_fd(fd,in,local,remote)
register int fd;
register unsigned long *in;
register unsigned short *local;
register unsigned short *remote;
{
 struct sockaddr_in sa;
 int dummy;

 dummy = sizeof(sa);
 if (getsockname(fd,&sa,&dummy) == -1)
   return -1;
 if (sa.sin_family != AF_INET)
  {
   errno = EAFNOSUPPORT;
   return -1;
  }
 *local = ntohs(sa.sin_port);
 dummy = sizeof(sa);
 if (getpeername(fd,&sa,&dummy) == -1)
   return -1;
 *remote = ntohs(sa.sin_port);
 *in = sa.sin_addr.s_addr;
 return 0;
}

static char ruser[SIZ];
static char realbuf[SIZ];
static char *buf;

char *auth_tcpuser(in,local,remote)
register unsigned long in;
register unsigned short local;
register unsigned short remote;
{
 struct sockaddr_in sa;
 register int s;
 register int buflen;
 register int w;
 register int saveerrno;
 char ch;
 unsigned short rlocal;
 unsigned short rremote;

 if ((s = socket(AF_INET,SOCK_STREAM,0)) == -1)
   return 0;
 sa.sin_family = AF_INET;
 sa.sin_port = htons(auth_tcpport);
 sa.sin_addr.s_addr = in;
 if (connect(s,&sa,sizeof(sa)) == -1)
  {
   saveerrno = errno;
   (void) close(s);
   errno = saveerrno;
   return 0;
  }

 buf = realbuf;
 (void) sprintf(buf,"%u , %u\r\n",(unsigned int) remote,(unsigned int) local);
 /* note the reversed order---the example in the RFC is misleading */
 buflen = strlen(buf);
 while ((w = write(s,buf,buflen)) < buflen)
   if (w == -1) /* should we worry about 0 as well? */
    {
     saveerrno = errno;
     (void) close(s);
     errno = saveerrno;
     return 0;
    }
   else
    {
     buf += w;
     buflen -= w;
    }
 buf = realbuf;
 while ((w = read(s,&ch,1)) == 1)
  {
   *buf = ch;
   if ((ch != ' ') && (ch != '\t') && (ch != '\r'))
     ++buf;
   if ((buf - realbuf == sizeof(realbuf) - 1) || (ch == '\n'))
     break;
  }
 if (w == -1)
  {
   saveerrno = errno;
   (void) close(s);
   errno = saveerrno;
   return 0;
  }
 *buf = '\0';

 if (sscanf(realbuf,"%hd,%hd: USERID :%*[^:]:%s",&rremote,&rlocal,ruser) < 3)
  {
   (void) close(s);
   errno = EIO;
   /* makes sense, right? well, not when USERID failed to match ERROR */
   /* but there's no good error to return in that case */
   return 0;
  }
 if ((remote != rremote) || (local != rlocal))
  {
   (void) close(s);
   errno = EIO;
   return 0;
  }
 /* XXX: we're not going to do any backslash processing */
 (void) close(s);
 return ruser;
}
