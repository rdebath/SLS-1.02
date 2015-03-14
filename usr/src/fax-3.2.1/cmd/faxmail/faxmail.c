/*
  This file is part of the NetFax system.

  (c) Copyright 1989 by David M. Siegel. 
      All rights reserved.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation.

    This program is distributed in the hope that it will be useful, 
    but WITHOUT ANY WARRANTY; without even the implied warranty of 
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include <stdio.h>
#include <strings.h>

#define FALSE 0
#define TRUE  1

struct fax_headers {
    char *from;
    char *return_path;
    char *sender;
    char *sender_phone;
    char *phone;
    char *recipient;
    char *font;
    char *no_cover;
};

#define h_offset(h) ((int)(&(((struct fax_headers *)NULL)->h)))
static struct _parse_switch {
    char *string;
    int offset;
} parse_switch[] = {
    {"from",    	h_offset(from)},
    {"return-path", 	h_offset(return_path)},
    {"sender",  	h_offset(sender)},
    {"sender-phone",	h_offset(sender_phone)},
    {"phone",		h_offset(phone)},
    {"recipient",	h_offset(recipient)},
    {"font",		h_offset(font)},
    {"no-cover",	h_offset(no_cover)},
    /* 
     * For backwards compatibility:
     */
    {"faxrecipient",	h_offset(recipient)},
    {"faxfont",		h_offset(font)},
    {"faxphone",	h_offset(phone)},
    {"faxreturn",	h_offset(sender_phone)},
};
#define parse_switch_len (sizeof(parse_switch)/sizeof(struct _parse_switch))
#undef h_offset

static void parse_header(h, header)
     struct fax_headers *h;
     char *header;
{
    int i;

    for (i = 0; i < parse_switch_len; i++) {
	struct _parse_switch *p = &parse_switch[i];
	if (strncasecmp(p->string, header, strlen(p->string)) == 0) {
	    char **field = (char **)&((char *)h)[p->offset];
	    char *colon = index(header, ':');
	    if (colon++ == NULL)
	      return;
	    if (*field == NULL) {
		if ((*field = (char *)calloc(1, BUFSIZ)) == NULL) {
		    perror("calloc");
		    exit(1);
		}
	    }
	    while (*colon != '\0' && *colon == ' ')
	      colon++;
	    strcpy(*field, colon);
	    (*field)[strlen(*field)-1] = '\0';
	    return;
	}
    }
}

static char *rm_braces(addr)
     char *addr;
{
    char *left_brace = index(addr, '<');
    char *right_brace = index(addr, '>');
    
    if (left_brace != NULL && right_brace != NULL) {
	*right_brace = '\0';
	return (left_brace+1);
    }

    return (addr);
}

static FILE *open_fax(h)
     struct fax_headers *h;
{
    char cmd[1024];

    strcpy(cmd, "/usr/local/bin/fax");

    if (h->phone != NULL)
      sprintf(&cmd[strlen(cmd)], " -p \"%s\"", h->phone);
    else {
	fprintf(stderr, "missing a fax phone\n");
	return (NULL);
    }

    if (h->return_path != NULL)
      sprintf(&cmd[strlen(cmd)], " -m -u \"%s\"", rm_braces(h->return_path));
    else if (h->from != NULL)
      sprintf(&cmd[strlen(cmd)], " -m -u \"%s\"", rm_braces(h->from));
    else
      sprintf(&cmd[strlen(cmd)], " -u \"mailer\"");

    if (h->no_cover == NULL)
      sprintf(&cmd[strlen(cmd)], " -c");

    if (h->sender != NULL)
      sprintf(&cmd[strlen(cmd)], " -s \"%s\"", h->sender);
    else if (h->from != NULL)
      sprintf(&cmd[strlen(cmd)], " -s \"%s\"", h->from);

    if (h->sender_phone != NULL)
      sprintf(&cmd[strlen(cmd)], " -S \"%s\"", h->sender_phone);

    if (h->recipient != NULL)
      sprintf(&cmd[strlen(cmd)], " -r \"%s\"", h->recipient);

    return (popen(cmd, "w"));
}

main(argc,argv)
     int argc;
     char *argv[];
{
    struct fax_headers headers;
    int in_header = TRUE;
    char buf[BUFSIZ];
    FILE *fp;
    int fd;

    int c;
    int errflg = FALSE;
    extern char *optarg;
    extern int optind;

    chdir("/tmp");

    while ((c = getopt(argc, argv, "")) != -1)
      switch (c) {
	case '?':
	  errflg = TRUE;;
	  break;
      }

    if (errflg) {
	fprintf(stderr, "usage: %s", argv[0]);
	exit(1);
    }

    /*
     * Make sure the header address are all NULL.
     */
    memset(&headers, 0, sizeof(struct fax_headers));

    /*
     * Read the mail message.
     */
    while (fgets(buf, sizeof(buf), stdin) != NULL) {
	if (in_header) {
	    /*
	     * Did we get the end of the header (just a NL)?
	     */
	    if (strlen(buf) == 1) {
		in_header = FALSE;
		if ((fp = open_fax(&headers)) == NULL)
		  exit(1);
	    } else
	      parse_header(&headers, buf);
	} else
	  fputs(buf, fp);
    }

    pclose(fp);

    exit(0);
}
