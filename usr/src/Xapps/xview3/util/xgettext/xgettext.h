/* @(#)xgettext.h 1.6 91/09/14 */

#ifndef XGETTEXT_H_DEFINED
#define XGETTEXT_H_DEFINED

#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include <sys/types.h>

#ifndef SVR4
#include <sys/dir.h>
#else
#include <dirent.h>
#endif SVR4

#include <sys/param.h>
#include <sys/stat.h>

#define DOMAIN_TOKEN    "domain"
#define MSGID_TOKEN     "msgid"
#define MSGSTR_TOKEN    "msgstr"

#define MAX_VALUE_LEN           2047
#define MAX_DOMAIN_LENGTH       255

#define LINEBUF_SIZ 4096
#define COMMENTBUF_SIZ 4096
#define MAX_MSGID_LEN 2047
#define MAX_MSGSTR_LEN 2047
#define SYMBOL_CHAR(c) \
	(c == '$' || c == '_' || (c > '/' && c < ':') ||\
	(c > '@' && c < '[' ) || (c > '`' && c < '{'))

struct comment_str {
    char 		*str;
    struct comment_str  *next;
};

struct  list_element {
    char                *msgid;
    char		*msgstr;
    struct list_element *next;
    struct comment_str  *comments;
    struct comment_str  *last_comment;
    short               new;
};

struct list_head {
    char		*domain;
    struct list_element *first_element;
    struct comment_str	*comments;
    struct comment_str	*last_comment;
    struct list_head	*next_list;
};

#endif XGETTEXT_H_DEFINED
