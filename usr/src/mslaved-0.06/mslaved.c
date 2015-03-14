/* mslaved.c - MandelSpawn computation server deamon */

/*  This file is part of MandelSpawn, a parallel Mandelbrot program for
    the X window system.

    Copyright (C) 1990 Andreas Gustafsson

    MandelSpawn is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License, version 1,
    as published by the Free Software Foundation.

    MandelSpawn is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License,
    version 1, along with this program; if not, write to the Free 
    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/


#include <stdio.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <signal.h>
#include <syslog.h>

#include "ms_real.h"
#include "ms_ipc.h"
#include "ms_job.h"
#include "ms_real.c"

/* Note that the timeout default below is overridden by mslavedc so that */
/* manually started servers will persist throughout a typical session. */
#define DEFAULT_SLAVE_TIMEOUT (60)   /* by default time out in 60 seconds */
#define DEFAULT_NICE (10)	     /* use nice 10 by default */

char *me; 				/* name of program */
int timeout=DEFAULT_SLAVE_TIMEOUT;	/* timeout */
int niceval=DEFAULT_NICE;	 	/* nice value */

/* Log an error message and exit */
error(s)
     char *s;
{ syslog(LOG_ERR, "%s: %s (%m)", me, s);
  exit(1);
}


/* Do the Mandelbrot calculation; return the size of the output message */
/* (in bytes) */

unsigned int calculate(in, out)
     Message *in, *out;
{ register real x_re, x_im;
  register real c_re, c_im;
  register real xresq, ximsq;
  int julia; 		/* true if calculating a Julia set */
  int show_interior; 	/* true if displaying interior structure */
  complex corner;
  complex delta;
  complex z0;
  
  int xc, yc, xmin, xmax, xsize, ymin, ymax, ysize;
  unsigned int maxiter, count;
  unsigned long mi_count=0;
  unsigned int datasize;

  ms_job *job;
  job=(ms_job *)in->whip.data;
  
  /* check that the format is supported */
  if(ntohs(in->whip.header.format) != DATA_FORMAT)
    return(0);

  /* convert values in the message to host byte order and precalculate some */
  /* useful values */
  xmin=ntohs(job->s.x);
  xsize=ntohs(job->s.width);
  xmax=xmin+xsize;
  ymin=ntohs(job->s.y);
  ysize=ntohs(job->s.height);
  ymax=ymin+ysize;

  show_interior = !!(ntohs(job->j.flags) & MS_OPT_INTERIOR);
  julia = ntohs(job->j.julia);
  corner.re = net_to_real(ntohli(job->j.corner.re));
  corner.im = net_to_real(ntohli(job->j.corner.im));
  z0.re = net_to_real(ntohli(job->j.z0.re));
  z0.im = net_to_real(ntohli(job->j.z0.im));
  delta.re = net_to_real(ntohli(job->j.delta.re));
  delta.im = net_to_real(ntohli(job->j.delta.im));
  
  maxiter = ntohli(job->j.iteration_limit);

  datasize = (maxiter > 256) ?
    ((char *)(&(out->reply.data.shorts[xsize*ysize]))
     -(char *)(&(out->reply))) :
       ((char *)(&(out->reply.data.chars[xsize*ysize]))
	-(char *)(&(out->reply)));
  
  /* Perform a simple sanity check to avoid getting into semi-infinite */
  /* loops because of malicious or erroneous messages.  */
  if(datasize > MAX_DATAGRAM) 
    error("data too large");

  /* Make sure the iteration counts can be represented in the result */
  /* packet; 65536 iterations can still take a long time and if we */
  /* keep calculating the same packet for too long the timeout alarm will */
  /* get us.  Those who do lots of iterations should use smaller chunks. */
  if(maxiter >= 65536)
    error("iteration count too large");
  
  for(yc=ymin; yc<ymax; yc++)
  { real var_re, var_im;
    real old_re=zero_real(), old_im=zero_real();
    var_im=add_real(julia ? z0.im : corner.im, mul_real_int(delta.im, yc));
    for(xc=xmin; xc<xmax; xc++)
    { var_re=add_real(julia ? z0.re : corner.re, mul_real_int(delta.re, xc));
      if(julia)
      {	c_re=corner.re; c_im=corner.im;
	x_re=var_re; x_im=var_im;
      }
      else /* Mandelbrot */
      {	c_re=var_re; c_im=var_im;
	x_re=z0.re; x_im=z0.im;
      }
      /* The following loop is where the Real Work gets done. */
      count=0;
      while(count < maxiter-1)
      {	/* The following if statement implements limit cycle detection */
	/* to speed up calculation of areas inside the Mandelbrot set. */
	if((count & (count-1)) == 0)
	{ /* "count" is zero or a power of two; save the current position */
	  old_re=x_re;
	  old_im=x_im;
	}
	else
	{ /* check if we have returned to a previously saved position; */
	  /* if so, the iteration has converged to a limit cycle => we */
	  /* are inside the Mandlebrot set and need iterate no further */
	  if(x_re==old_re && x_im==old_im)
	  { if(! show_interior)
	      count = maxiter-1;
	    break;
	  }
	}
	/* this is the familiar "z := z^2 + c; abort if |z| > 2" */
	/* Mandelbrot iteration, with the arithmetic operators hidden */
	/* in macros so that the same code can be compiled for either */
        /* fixed-point or floating-point arithmetic. */
	/* The macros (mul_real(), etc.) are defined in ms_real.h. */
	xresq=mul_real(x_re, x_re);
	ximsq=mul_real(x_im, x_im);
	if(gteq_real(add_real(xresq, ximsq), four_real()))
	  break;
	x_im=add_real(twice_mul_real(x_re, x_im), c_im);
	x_re=add_real(sub_real(xresq, ximsq), c_re);
	count++;
      }
      if(maxiter > 256)
	out->reply.data.shorts[(yc-ymin)*xsize+(xc-xmin)]=htons(count);
      else
	out->reply.data.chars[(yc-ymin)*xsize+(xc-xmin)]=count;
      mi_count+=count;
    }
  }
  out->reply.mi_count=htonl(mi_count);
  return(datasize);
}


void serve()
{ Message in;
  Message out;
  int isock;
  
  struct sockaddr_in oname;
  isock=0; /* use standard input */

  while(1)
  { unsigned int bytes;
    int onamelen=sizeof(oname);
    int version;
    if(timeout != 0)
      alarm(timeout);
    /* receive from anywhere, save the address of the caller in oname */
    if(recvfrom(isock, (char *)&in, sizeof(in), 0,
		(struct sockaddr *)&oname, &onamelen) < 0) 
      error("receiving datagram packet");
    version=ntohs(in.generic.header.version);
    if(ntohs(in.generic.header.magic)==MAGIC && version==VERSION)
    { switch(ntohs(in.generic.header.type))
      {	case WHIP_MESSAGE:
	  /* copy the header and id structures as such while still in */
	  /* network byte order (not strictly portable but probably works) */
	  out.reply.header=in.whip.header;
	  out.reply.id=in.whip.id;
	  /* just the message type needs to be changed */
	  out.reply.header.type=htons(REPLY_MESSAGE);
	  /* calculate() sets the data and mi_count fields */
	  bytes=calculate(&in, &out);
	  if(bytes)
	    if(sendto(isock, (char *)&out, (int)bytes, 0,
		      (struct sockaddr *)&oname, sizeof(oname)) < 0)
	      error("sending calculated data");
	  break;
	case WHO_R_U_MESSAGE:
	  oname.sin_port=in.who.port; /* in network byte order already */
	  out.iam.header=in.who.header;
	  out.iam.pid=htons(getpid());
	  if(sendto(isock, (char *)&out, sizeof(IAmMessage), 0,
		    (struct sockaddr *)&oname, sizeof(oname)) < 0)
	    error("sending response to pid inquiry");
	  break;
	default: ; /* ignore other messages */
      }
    }
  }
}


/* This function is called when we get a SIGALRM so that we die gracefully */
/* with exit status 0; otherwise inetd would log a message about us getting */
/* an alarm signal */

int die()
{ exit(0);
}

int main(argc, argv)
     int argc; char **argv;
{ int i;
  me=argv[0];
  signal(SIGALRM, die);
  while(--argc)
  { char *s= *++argv;
    if(*s++ != '-')
      goto usage;
    switch(*s)
    { 
    case 'n': /* nice */
      niceval=atoi(++s);
      break;
    case 't': /* timeout */
      timeout=atoi(++s);
      break;
    case 'i': /* accept for backwards compatibility but ignore */
      break;
    default: 
      goto usage;
    }
  }
  nice(niceval);
  serve(); /* never returns */
 usage:
  exit(1);
}
