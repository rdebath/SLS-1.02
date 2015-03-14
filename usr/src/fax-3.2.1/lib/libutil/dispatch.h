/*
  This file is part of the NetFax system.

  (c) Copyright 1989 by David M. Siegel and Sundar Narasimhan.
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

#ifndef INCdispatchh
#define INCdispatchh 1

#include <sys/types.h>
#include <sys/time.h>

#define DIO_READ     00001
#define DIO_WRITE    00002
#define DIO_EXCEPT   00004
#define DIO_TIMEOUT  00010
#define DIO_REPEAT   00020
#define DIO_DELETED  00100
#define DIO_RPC	     00200

#define DIO_FD_EVENT(x) ((x)&(DIO_READ|DIO_WRITE|DIO_EXCEPT))
#define DIO_FD_MASK(x)  ((DIO_FD_EVENT(x))>>1)

typedef struct _dio {
    int dtype;
    int fd;
    struct timeval time_next;
    struct timeval time_interval;
    int (*routine)();
    char *data;
} DIO;

/*
  Prototypes:
*/

int dispatch_unregister(
#ifdef _PROTO
    DIO *d
#endif
);

DIO *dispatch_register_fd(
#ifdef _PROTO
    int dtype,
    int priority,
    int fd,
    int (*routine)(),
    char *data
#endif
);

DIO *dispatch_register_fd_timeout(
#ifdef _PROTO
    int dtype,
    int priority,
    int fd,
    double timeout,
    int (*routine)(),
    char *data
#endif
);

DIO *dispatch_register_alarm(
#ifdef _PROTO
    int priority,
    double period,
    int (*routine)(),
    char *data
#endif
);

DIO *dispatch_register_periodic(
#ifdef _PROTO
    int priority,
    double period,
    int (*routine)(),
    char *data
#endif
);

DIO *dispatch_register_rpc(
#ifdef _PROTO
    int priority
#endif
);

int dispatch_unregister_fd(
#ifdef _PROTO
    int dtype,
    int fd
#endif
);

DIO *dispatch_change_handler(
#ifdef _PROTO
    DIO *d,
    int (*routine)()
#endif
);

DIO *dispatch_change_priority(
#ifdef _PROTO
    DIO *d,
    int priority
#endif
);

int dispatch_print_queues(
#ifdef _PROTO
    void
#endif
);

struct timeval *dispatch_timeval(
#ifdef _PROTO
    struct timeval *tp
#endif
);

time_t dispatch_time(
#ifdef _PROTO
    time_t *tloc
#endif
);

int dispatch_run(
#ifdef _PROTO
    void
#endif
);

#endif
