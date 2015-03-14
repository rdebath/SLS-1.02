#include <stdio.h>
#include <errno.h>
#include <malloc.h>
#include <string.h>
#include <syslog.h>
#include <stdlib.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>

#include "pathnames.h"
#include "extensions.h"

static	char	*aclbuf = NULL;
static	struct	aclmember	*aclmembers;

/*************************************************************************/
/* FUNCTION  : getaclentry                                               */
/* PURPOSE   : Retrieve a named entry from the ACL                       */
/* ARGUMENTS : pointer to the keyword and a handle to the acl members    */
/* RETURNS   : pointer to the acl member containing the keyword or NULL  */
/*************************************************************************/

struct	aclmember *
getaclentry(keyword, next)
char	*keyword;
struct	aclmember	**next;

{

   do {
      if (!*next)
         *next = aclmembers;
      else
         *next = (*next)->next;

   } while (*next && strcmp((*next)->keyword, keyword));

   return(*next);

}

/*************************************************************************/
/* FUNCTION  : parseacl                                                  */
/* PURPOSE   : Parse the acl buffer into its components                  */
/* ARGUMENTS : A pointer to the acl file                                 */
/* RETURNS   : nothing                                                   */
/*************************************************************************/

void
parseacl()

{
char	*ptr,
		*aclptr = aclbuf,
		*line;
int		cnt;
struct	aclmember	*member,
					*acltail;

   if (!aclbuf || !(*aclbuf)) return;

   aclmembers = (struct aclmember *) NULL;
   acltail = (struct aclmember *) NULL;

   while (*aclptr != NULL) {
      line = aclptr;
      while (*aclptr && *aclptr != '\n') aclptr++;
      *aclptr++ = (char) NULL;

      /* deal with comments */
      if ((ptr = strchr(line, '#')) != NULL) *ptr = NULL;

      member = (struct aclmember *) calloc(1, sizeof(struct aclmember));

      ptr = strtok(line, " \t");
      if (ptr) {
         (void) strcpy(member->keyword, ptr);
         cnt = 0;
         while ((ptr = strtok(NULL, " \t")) != NULL)
            member->arg[cnt++] = ptr;
         if (acltail) acltail->next = member;
         acltail = member;
         if (!aclmembers) aclmembers = member;
      }
   }
}

/*************************************************************************/
/* FUNCTION  : readacl                                                   */
/* PURPOSE   : Read the acl into memory                                  */
/* ARGUMENTS : The pathname of the acl                                   */
/* RETURNS   : 0 if error, 1 if no error                                 */
/*************************************************************************/

int
readacl(aclpath)
char	*aclpath;

{
FILE	*aclfile;
struct	stat	finfo;
extern	int		use_accessfile;

   if (stat(aclpath, &finfo) != 0) {
      if (use_accessfile) {
         syslog(LOG_ERR, "cannot stat access file %s: %s", aclpath,
            strerror(errno));
      }
      return;
   }

   if ((aclfile = fopen(aclpath, "r")) == NULL) {
      if (errno != ENOENT) syslog(LOG_ERR, "cannot open access file %s: %s",
         aclpath, strerror(errno));
      return;
   }

   if (finfo.st_size == 0) {
      aclbuf = (char *) calloc(1, 1);
   } else {
      if (!(aclbuf = malloc((unsigned) finfo.st_size + 1))) {
         syslog(LOG_ERR, "could not malloc aclbuf (%d bytes)", finfo.st_size+1);
         return;
      }
      if (!fread(aclbuf, (size_t) finfo.st_size, 1, aclfile)) {
         syslog(LOG_ERR, "error reading acl file %s: %s", aclpath,
            strerror(errno));
      }
      *(aclbuf+finfo.st_size) = '\0';
   }
}
