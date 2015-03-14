#include "stdio.h"
#include "string.h"
#include "authuser.h"
#include "authenticate.h"

#define	AUTHNAMESIZE 100

char	authuser[AUTHNAMESIZE];
int		authenticated;

authenticate()
{
extern char		*auth_tcpuser();
unsigned long	in;
unsigned short	local, remote;
char			*user;

/* Ideally more authentication schemes would be called from here, with the
 * strongest called first.  One possible double-check would be to verify
 * that the results of all authentication calls (returning identical data!)
 * are checked against each other.
 */

   authenticated = 0;		/* this is a bitmask, one bit per method */

   user = "*";

#if		USE_A_RFC931
   if (auth_fd(0, &in, &local, &remote) == -1)
      user = "?"; /* getpeername/getsockname failure */
   else {
      if (!(user = auth_tcpuser(in, local, remote))) {
         user = "*"; /* remote host doesn't support RFC 931 */
      } else {
         authenticated |= A_RFC931;
      }
   }
#endif

   strncpy(authuser, user, sizeof(authuser));
   authuser[AUTHNAMESIZE-1] = '\0';
}
