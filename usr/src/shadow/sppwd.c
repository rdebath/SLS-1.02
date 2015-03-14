#undef SHADOW_PWD
#include <stdio.h>
#include <sys/types.h>
#include <shadow.h>
#include <pwd.h>
#include <grp.h>
#include <unistd.h>

struct passwd *_spsubspwd(struct passwd *pw)
{
	struct spwd *spw; 
	if (pw)
	{	spw = getspnam(pw->pw_name);
		if (spw)
			pw->pw_passwd = spw->sp_pwdp;
	}
	return(pw);
}

struct group *_spsubsgrp(struct group *gr)
{
	struct sgrp *sgr; 
	if (gr)
	{	sgr = getsgnam(gr->gr_name);
		if (sgr)
			gr->gr_passwd = sgr->sg_passwd;
	}
	return(gr);
}

struct passwd *_spgetpwuid(uid_t  uid) { return(_spsubspwd(getpwuid(uid))); }

struct passwd *_spgetpwnam(char  *name) { return(_spsubspwd(getpwnam(name))); }

struct passwd *_spgetpwent(void) { return(_spsubspwd(getpwent())); }

struct passwd *_spfgetpwent(FILE *F) { return(_spsubspwd(fgetpwent(F))); }

struct group *_spgetgrgid(gid_t  gid) { return(_spsubsgrp(getgrgid(gid))); }

struct group *_spgetgrnam(char  *name) { return(_spsubsgrp(getgrnam(name))); }

struct group *_spgetgrent(void) { return(_spsubsgrp(getgrent())); }

struct group *_spfgetgrent(FILE *F) { return(_spsubsgrp(fgetgrent(F))); }

char *_spcrypt(char clear[], char salt[] )
{
	static char buffer[30];
	char *cp = crypt (clear, salt);
	strcpy (buffer, cp);
	if (strlen (clear) > 8) {
		cp = crypt (clear + 8, salt);
		strcat (buffer, cp + 2);
	}
	return(buffer);
}

