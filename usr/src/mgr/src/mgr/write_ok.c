/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* check to make sure it is ok to read/write a file */

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <limits.h>
#include <unistd.h>
#include <string.h>

#define Type(file)		(stat(file,&buff) ? 0 : buff.st_mode)

/* ok for user to write this file */
int
write_ok(name)
register char *name;		/* path name of file */
   {
   struct stat buff;
   char dir[_POSIX_PATH_MAX];
   char *strcpy(), *rindex();
   char *ptr;
   int result;

   if (access(name,F_OK)==0) {
      result = (S_ISREG(Type(name)) && access(name,W_OK)==0);
      }
   else if (ptr=rindex(strcpy(dir,name),'/')) {
      *ptr = '\0';
      result = (access(dir,W_OK)==0 && S_ISDIR(Type(dir)));
      }
   else {
      result = (access(".",W_OK)==0);
      }
   return(result);
   }

/* see if ok to read a file */

int
read_ok(name)
register char *name;		/* path name of file */
   {
   struct stat buff;
   extern char *icon_dir;
   char *rindex();

   if (access(name,R_OK)==0) {
      return(1);
      }
   if (strncmp(name,icon_dir,strlen(icon_dir)) == 0 &&
            rindex(name,'.')==(char *) 0 && stat(name,&buff)==0)
      return(S_ISREG(buff.st_mode));
   }

/* make sure tty mode is ok for message passing */

int
mode_ok(name,mask)
char *name;		/* file to check mode for */
int mask;		/* these bits must be turned off */
   {
   struct stat buff;
   if (stat(name,&buff) < 0)
      return(0);
   return((buff.st_mode&mask) == 0);
   }
