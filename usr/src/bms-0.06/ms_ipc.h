/* ms_ipc.h - layout of messages passed between Mama and the slave, */
/*            and other network stuff */
/* Copyright (C) 1990, 1991 Andreas Gustafsson */

#ifndef _ms_ipc_h
#define _ms_ipc_h

#ifndef __TYPES__
/* __TYPES__ is defined by X headers that include <sys/types.h> */
#include <sys/types.h>
#include <sys/file.h>	/* HP-UX needs this for FNDELAY */
#endif

#include <fcntl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

/* network to host byteorder, long (signed) integer (ntohl may return an */
/* unsigned long) */
#define ntohli(x) ((long) ntohl(x))


/* update these whenever changes have been made to the layout of */
/* any of the structures defined below */

#define VERSION		5	/* major version */
#define DATA_FORMAT 	7	/* minor version (name is historical) */

/* miscellaneous magic constants */

#define MAX_DATAGRAM	8192	/* maximum datagram size */
#define MAGIC 		0x9872	/* magic number */
#define DEFAULT_PORT	9359	/* default UDP port */

/* message types */

#define WHIP_MESSAGE 0
#define REPLY_MESSAGE 1
#define WHO_R_U_MESSAGE 2
#define I_AM_MESSAGE 3


typedef struct
{ unsigned short magic;		/* magic number */
  unsigned short type;		/* packet type */
  unsigned short version;	/* protocol version */
  unsigned short format; 	/* data format (= minor protocol version) */
} MessageHeader;

typedef struct
{ unsigned short pid;		/* process id of requesting process */
  unsigned short seq;		/* sequence number */
  unsigned short chunk_no;	/* chunk number within current sequence */
  unsigned short slave_no;	/* index into the client's slave table */
} MessageId;


/* C doesn't support zero-sized arrays; it's a shame. */
#ifdef __GNUCC__
#define VARIES 0
#else
#define VARIES 1
#endif

/* Work request message */
typedef struct
{ MessageHeader header;
  MessageId id;
  char data[VARIES];		/* this really is a ms_job, but pretend */
} WhipMessage;			/* not to know that */


/* Reply message */
/* the reply message need not contain any corner, delta, or rectangle field */
/* because these are saved in the chunk */
typedef struct
{ MessageHeader header;
  MessageId id;
  unsigned long mi_count;
  union
  { unsigned char chars[1];
    unsigned short shorts[1];
  } data;
} ReplyHeader;

/* Slave PID inquiry message */
typedef struct
{ MessageHeader header;
  unsigned short port;
  unsigned short pad;
} WhoAreYouMessage;

/* Reply to PID inquiry */
typedef struct
{ MessageHeader header;
  short pid;
  short pad;
} IAmMessage;

/* Used when we don't know the message type yet */
typedef struct
{ MessageHeader header;
} GenericMessage;

typedef union
{ char c[MAX_DATAGRAM];
  GenericMessage generic;
  ReplyHeader reply;
  WhipMessage whip;
  WhoAreYouMessage who;
  IAmMessage iam;
} Message;
 
#endif /* _ms_ipc_h */
