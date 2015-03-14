#ifndef	NO_PRIVATE

#include <stdio.h>
#include <errno.h>
#include <malloc.h>
#include <string.h>
#include <syslog.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <grp.h>


#include "pathnames.h"
#include "extensions.h"

#define	MAXGROUPLEN	100
char	*passbuf = NULL;
char	groupname[MAXGROUPLEN];
int		group_given = 0;

struct	acgrp {
		char	gname[MAXGROUPLEN];		/* access group name */
		char	gpass[MAXGROUPLEN];		/* access group password */
		char	gr_name[MAXGROUPLEN];	/* group to setgid() to */
};

extern	int	lgi_failure_threshold, autospout_free;
extern	char	remotehost[], remoteaddr[], *autospout;
int		group_attempts;

/*************************************************************************/
/* FUNCTION  : priv_setup                                                */
/* PURPOSE   : Set things up to use the private access password file.    */
/* ARGUMENTS : path, the path to the private access password file        */
/*************************************************************************/

void
priv_setup(path)
char	*path;

{
FILE	*prvfile;
struct	stat	finfo,group_tmp;

   passbuf = (char *) NULL;

   if (stat(path, &finfo) != 0) {
      syslog(LOG_ERR, "cannot stat private access file %s: %s", path,
         strerror(errno));
      return;
   }

   if ((prvfile = fopen(path, "r")) == NULL) {
      if (errno != ENOENT)
         syslog(LOG_ERR, "cannot open private access file %s: %s",
            path, strerror(errno));
      return;
   }

   if (finfo.st_size == 0) {
      passbuf = (char *) calloc(1, 1);
   } else {
      if (!(passbuf = malloc((unsigned) finfo.st_size + 1))) {
         (void) syslog(LOG_ERR, "could not malloc passbuf (%d bytes)",
            finfo.st_size + 1);
         return;
      }
      if (!fread(passbuf, (size_t) finfo.st_size, 1, prvfile)) {
         (void) syslog(LOG_ERR, "error reading private access file %s: %s",
            path, strerror(errno));
      }
      *(passbuf+finfo.st_size) = '\0';
   }

/*   if (setgroupent(1) == 0) {*/
   if (stat("/etc/group", &group_tmp) != 0) {
      (void) syslog(LOG_ERR, "error opening group file");
      (void) free(passbuf);
      passbuf = (char *) NULL;
   }
}

/*************************************************************************/
/* FUNCTION  : priv_getent                                               */
/* PURPOSE   : Retrieve an entry from the in-memory copy of the group    */
/*             access file.                                              */
/* ARGUMENTS : pointer to group name                                     */
/*************************************************************************/

struct acgrp *
priv_getent(group)
char	*group;

{
char	*ptr = passbuf,
		*cr,
		*data;
static	struct	acgrp	grp;
char			linebuf[1024];

   while (*ptr) {
      if ((cr = index(ptr, '\n')) == NULL) return(NULL);
      *cr = '\0';
      strncpy(linebuf, ptr, 1024);
      *cr = '\n';

      if ((cr - ptr) < 1024) {
         if ((data = strtok(linebuf, ":")) != NULL)
            (void) strncpy(grp.gname, data, MAXGROUPLEN);
         else goto next; /* bad entry -- skip it */

         if (strncmp(group, grp.gname, MAXGROUPLEN) != NULL) goto next;

         if ((data = strtok(NULL, ":")) != NULL)
            (void) strncpy(grp.gpass, data, MAXGROUPLEN);
         else goto next; /* bad entry -- skip it */

         if ((data = strtok(NULL, ":")) != NULL)
            (void) strncpy(grp.gr_name, data, MAXGROUPLEN);
         else goto next; /* bad entry -- skip it */

         return(&grp);
      }
next:   ptr = ++cr;
   }
   return(NULL);
}

/*************************************************************************/
/* FUNCTION  : priv_group                                                */
/* PURPOSE   :                                                           */
/* ARGUMENTS :                                                           */
/*************************************************************************/

void
priv_group(group)
char	*group;

{
/*	char *mypass=malloc(20);
	char pass_line[256];
*/   if (strlen(group) < MAXGROUPLEN) {
      strncpy(groupname, group, MAXGROUPLEN);
      group_given = 1;
      reply(200, "Request for access to group %s accepted.", group);
   } else {
      group_given = 0;
      reply(500, "Illegal group name");
	return;
   }
/*sprintf(pass_line,"Password for group %s:",group);
(void)priv_gpass(getpass(pass_line));
*/
}

/*************************************************************************/
/* FUNCTION  : priv_gpass                                                */
/* PURPOSE   : validate the group access request, and if OK place user   */
/*             in the proper group.                                      */
/* ARGUMENTS : group access password                                     */
/*************************************************************************/

void
priv_gpass(gpass)
char	*gpass;

{
char	*xgpass, *salt;
#ifndef SHADOW_PWD
char *crypt();
#endif
struct	acgrp	*grp;
struct	group	*gr;
uid_t	uid;
gid_t	gid;

   if (group_given == 0) {
      reply(503, "Give group name with SITE GROUP first.");
      return;
   }

   if (passbuf != NULL) {
      grp = priv_getent(groupname);

      if (grp == NULL)
         salt = "xx";
      else
         salt = grp->gpass;

      xgpass = crypt(gpass, salt);
   } else
      grp = NULL;

   /* The strcmp does not catch null passwords! */
   if (grp == NULL || *grp->gpass == '\0' || strcmp(xgpass, grp->gpass)) {
      reply(530, "Group access request incorrect.");
      grp = NULL;
      if (++group_attempts >= lgi_failure_threshold) {
         syslog(LOG_NOTICE,
            "repeated group access failures from %s [%s], group %s",
            remotehost, remoteaddr, groupname);
         exit(0);
      }
      sleep(group_attempts); /* slow down password crackers */
      return;
   }

/* THIS CODE NEEDS CHECKING BY INFORMED, SECURITY-CONSCIOUS PEOPLE */
   if ((gr = getgrnam(grp->gr_name)) != NULL) {
      gid = gr->gr_gid;
   } else {
      lreply(500, "Configuration error -- group access not granted");
      syslog(LOG_ERR, "group %s does not exist in group file", grp->gr_name);
      return;
   }

   uid = geteuid();
   seteuid(0);
   setegid(gid);
   seteuid(uid);
/* END CODE */

   reply(200, "Group access enabled.");
   group_attempts = 0;
}

#endif	/* !NO_PRIVATE */
