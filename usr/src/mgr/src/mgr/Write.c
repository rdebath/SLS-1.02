/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* Long writes to ptty's don't always work */

#include <errno.h>
#include <unistd.h>
#include <stdlib.h>

#include "bitblit.h"

#include "defs.h"

#define MAX_RETRY	3		/* max retries after EWOULDBLOCK */
#define TTYMAX		100		/* max chunk size in write */

int
Write(fd,buff,len)
register int fd, len;
register char *buff;
   {
   register int count = 0;
   register int code;
   register int retry=0;

   while (count < len) {
      code = write(fd,buff+count,Min(TTYMAX,len-count));
      if (code > 0)
         count += code;
      else if (errno == EWOULDBLOCK) {
         if (retry++ > MAX_RETRY)
            break;
         sleep(1);
         continue;
         }
      else 
         break;
      }
   return(count);
   }
