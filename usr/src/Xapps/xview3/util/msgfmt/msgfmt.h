
#define USE_MMAP

#include <locale.h>
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/param.h>
#include <signal.h>

#define	DOMAIN_TOKEN	"domain"
#define	MSGID_TOKEN	"msgid"
#define MSGSTR_TOKEN	"msgstr"
#define DEFAULT_DOMAIN	"default"
#define LOCALEDIR	"./"


#define MAX_VALUE_LEN		2047
#define MAX_DOMAIN_LENGTH	255
#define LC_NAMELEN		255

#ifdef USE_MMAP
struct  list_struct {
        off_t   hpt;
        int     msgid_offset, msg_offset;
        char    *msgid;
        char    *msg;
        struct  list_struct *hnext;
        short   hnew;
} list;

#else

struct list_struct {
    off_t    hpt;
    char    *msgid;
    char    *msg;
    struct    list_struct *hnext;
    short    hnew;
};
#endif
