/*
 * This file is part of the Minicom Communications Program,
 * written by Miquel van Smoorenburg 1991/1992.
 *
 * rwconf.c	- routines to deal with ASCII configuration files.
 *
 */
#include <string.h>
#undef NULL
#include <stdio.h>
#include "config.h"
#include "configsym.h"

#define ADM_CHANGE	1
#define USR_CHANGE	2

#ifdef _ACK
/*
 * The Minix ACK compiler has problems with the initialization of
 * fixed - size character arrays, such as the 'value[64]' array in
 * the struct mpars. This array is _not_ given a length of 64, but
 * the lenght of the initialization string - ofcourse this screws
 * up the struct beyond human understanding :-).
 * Therefore we invented a new structure, and in readpars() we copy
 * the values to the (until then uninitialized) struct mpars...
 */
struct pars mpars[58];
struct {
  char *value;
  int flags;
  char *desc;
} tmppars[] = {
#else
struct pars mpars[] = {
#endif
  /* Protocols */
  /* Warning: minicom assumes the first 12 entries are these proto's ! */
  "YUzmodem",		PUBLIC,   "pname1",
  "YUymodem",		PUBLIC,   "pname2",
  "YUxmodem",		PUBLIC,   "pname3",
  "NDzmodem",		PUBLIC,   "pname4",
  "NDymodem",		PUBLIC,   "pname5",
  "YDxmodem",		PUBLIC,   "pname6",
  "",			PUBLIC,   "pname7",
  "",			PUBLIC,   "pname8",
  "",			PUBLIC,   "pname9",
  "",			PUBLIC,   "pname10",
  "",			PUBLIC,   "pname11",
  "",			PUBLIC,   "pname12",
  "/usr/bin/sz",	PUBLIC,   "pprog1",
  "/usr/bin/sb",	PUBLIC,   "pprog2",
  "/usr/bin/sx",	PUBLIC,   "pprog3",
  "/usr/bin/rz",	PUBLIC,   "pprog4",
  "/usr/bin/rb",	PUBLIC,   "pprog5",
  "/usr/bin/rx",	PUBLIC,   "pprog6",
  "",			PUBLIC,	  "pprog7",
  "",			PUBLIC,	  "pprog8",
  "",			PUBLIC,   "pprog9",
  "",			PUBLIC,   "pprog10",
  "",			PUBLIC,   "pprog11",
  "",			PUBLIC,   "pprog12",
  /* Serial port & friends */
  DFL_PORT,		PRIVATE,  "port",
  CALLIN,		PRIVATE,  "callin",
  CALLOUT,		PRIVATE,  "callout",
  UUCPLOCK,		PRIVATE,  "lock",
  DEF_BAUD,		PUBLIC,   "baudrate",
  "8",			PUBLIC,   "bits",
  "N",			PUBLIC,   "parity",
  /* Kermit the frog */
  KERMIT,		PRIVATE,  "kermit",
  "Yes",		PRIVATE,  "kermallow",
  "No",			PRIVATE,  "kermreal",
  "3",			PUBLIC,   "colusage",
  /* The script program */
  SCRIPTPROG,		PUBLIC,   "scriptprog",
  /* Modem parameters */
  "^MAT S7=45 S0=0 L1 V1 X4 &c1 E1 Q0^M",   PUBLIC,   "minit",
  "^M~ATZ^M~",		PUBLIC,   "mreset",
  "ATDP",		PUBLIC,   "mdialpre",
  "^M",			PUBLIC,   "mdialsuf",
  "CONNECT",		PUBLIC,   "mconnect",
  "NO CARRIER",		PUBLIC,   "mnocon1",
  "BUSY",		PUBLIC,   "mnocon2",
  "NO DIALTONE",	PUBLIC,   "mnocon3",
  "VOICE",		PUBLIC,   "mnocon4",
  "~~+++~~ATH^M",	PUBLIC,   "mhangup",
  "^M",			PUBLIC,   "mdialcan",
  "45",			PUBLIC,   "mdialtime",
  "60",			PUBLIC,   "mrdelay",
  "1",			PUBLIC,   "mretries",
  "Yes",		PUBLIC,   "mautobaud",
  "No",			PUBLIC,   "mdropdtr",
  "",			PUBLIC,   "updir",
  "",			PUBLIC,   "downdir",
  "",			PUBLIC,   "scriptdir",
  "^A",			PUBLIC,   "escape-key",
  "BS",			PUBLIC,   "backspace",
  "enabled",		PUBLIC,   "statusline",
  "No",			PUBLIC,   "hasdcd",
  /* That's all folks */
  "",			0,	   (char *)NULL,
};


/*
 * Write the parameters to a file.
 */
int writepars(fp, all)
FILE *fp;
int all;
{
  struct pars *p;

  for(p = mpars; p->desc; p++)
  	if ((all && (p->flags & ADM_CHANGE)) ||
  	   ((p->flags & PUBLIC) && (p->flags & USR_CHANGE)))
  		fprintf(fp, "%s %-16.16s %s\n",
  			p->flags & PUBLIC ? "pu" : "pr", p->desc, p->value);
  return(0);
}

/*
 * Read the parameters from a file.
 */
int readpars(fp, init)
FILE *fp;
int init;
{
  struct pars *p;
  char line[80];
  char *s;
  int public;

#ifdef _ACK
  int i;

  if (init) {
  	for(i = 0; tmppars[i].desc != (char *)0; i++) {
  		strcpy(mpars[i].value, tmppars[i].value);
  		mpars[i].flags = tmppars[i].flags;
  		mpars[i].desc = tmppars[i].desc;
  	}
  	mpars[i].desc = (char *)0;
  }
  if (fp == (FILE *)NULL) return(0);
#endif

  while(fgets(line, 80, fp) != (char *)0) {
  	s = strtok(line, "\n\t ");
  	/* Here we have pr for private and pu for public */
  	public = 0;
  	if (strcmp(s, "pr") == 0) {
  		public = 0;
  		s = strtok((char *)NULL, "\n\t ");
  	}
  	if (strcmp(line, "pu") == 0) {
  		public = 1;
  		s = strtok((char *)NULL, "\n\t ");
  	}
  	/* Don't read private entries if prohibited */
  	if (!init && public == 0) continue;

  	for(p = mpars; p->desc != (char *)0; p++) {
  		if (strcmp(p->desc, s) == 0) {
  				;
  			/* Set value */
  			if ((s = strtok((char *)NULL, "\n")) == (char *)0)
  				s = "";
  			while(*s && (*s == '\t' || *s == ' ')) s++;

  			/* If the same as default, don't mark as changed */
  			if (strcmp(p->value, s) == 0) {
  				p->flags = 0;
  			} else {
				if (init)
					p->flags = ADM_CHANGE;
				else
					p->flags = USR_CHANGE;
  				strcpy(p->value, s);
  			}
  			/* Set flags */
  			p->flags |= (public ? PUBLIC : PRIVATE);
  			break;
  		}
  	}
  }
  return(0);
}


