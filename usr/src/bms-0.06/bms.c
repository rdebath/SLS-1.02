/* bms.c - batch-mode MandelSpawn main program */

/*  This file is part of MandelSpawn, a parallel Mandelbrot program.

    Copyright (C) 1990, 1991 Andreas Gustafsson

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

/* bsd43 is defined on Sonys, but not on MtXinu more/bsd.  You can't win. */
#ifdef bsd43
#ifndef NO_MALLOC_H
#define NO_MALLOC_H
#endif
#endif

#ifdef _AIX
#ifndef NEED_SYS_SELECT_H
#define NEED_SYS_SELECT_H
#endif
#endif

/* 4.3 BSD wants these two for select() */
#include <sys/types.h>
#include <sys/time.h>

#include <stdio.h>
#include <string.h>
#include <errno.h>
extern int errno; /* argh */
#ifndef NO_MALLOC_H
#include <malloc.h>
#else
char *malloc(), *realloc(); void free(); /* double argh */
#endif
#include <math.h> /* for atof() */
#ifdef NEED_SYS_SELECT_H
#include <sys/select.h>
#endif

#include "work.h"
#include "mspawn.h"
#include "version.h"

#define TIMEOUT 15000	/* default computation server timeout in millisecs */

/* number of pixels to print on each line in the portable graymap file */
/* (watch out for line length limit of 70 characters) */
#define PGM_NPL 8 


char *me;	/* name of program */


/* various global mode flags */

int done = 0;		/* set when all chunks have arrived */
int verbose = 0;	/* verbosity level */
int statistics = 0;	/* print statistics if set */
int pgm_ascii = 0;	/* force ASCII graymap format */
int nooutput = 0;	/* generate no output (for testing only) */


/* structure for global data */     

typedef struct bms_state
{ ms_state ms;
  union
  { unsigned char *bytes;
    unsigned short *shorts;
  } frame;
} bms_state;


/* report a fatal error and die */

void error(s) char *s;
{ fprintf(stderr, "%s: %s\n", me, s);
  exit(1);
}


/* handle input from a computation server by drawing in an in-core buffer */

void ms_draw(client, client_data, data) /*ARGSUSED*/
     char *client;	/* unused */
     char *client_data;
     char *data;
{ ms_client_info *the_info = (ms_client_info *) client_data;
  bms_state *b = (bms_state *) client;
  unsigned int x, y, width, height;
  unsigned int i, j;
  if(verbose)
  { putc('.', stderr);
    fflush(stderr);
  }
  x=the_info->s.x; 
  y=the_info->s.y; 
  width=the_info->s.width;
  height=the_info->s.height;

  switch(b->ms.bytes_per_count)
  { case 1:
      { unsigned char *datap = (unsigned char *) data;
	unsigned char *bufp_left= (unsigned char *)
	  b->frame.bytes + b->ms.width * y + x;
	for(j=0; j<height; j++)
	{ unsigned char *bufp= bufp_left;
	  for(i=0; i<width; i++)
	  { *bufp++ = *datap++;
	  }
	  bufp_left += b->ms.width;
	}
      }
      break;
    case 2:
      { unsigned short *datap = (unsigned short *) data;
	unsigned short *bufp_left= (unsigned short *)
	  b->frame.shorts + b->ms.width * y + x;
	for(j=0; j<height; j++)
	{ unsigned short *bufp= bufp_left;
	  for(i=0; i<width; i++)
	  { *bufp++ = *datap++;
	  }
	  bufp_left += b->ms.width;
	}
      }
      break;
    default:
      error("bad iteration count size"); 
      break;
  }

  b->ms.chunks_out--; /* one less to go */
  
  if(b->ms.chunks_out == 0)
    done=1;
}


/* provide memory allocation and error reporting services to the */
/* workforce module */

char *wf_alloc(size) unsigned size;
{ char *r = malloc(size);
  if(!r)
    error("malloc");
  return(r);
}

char *wf_realloc(p, size) char *p; unsigned size;
{ char *r = realloc(p, size);
  if(!r)
    error("realloc");
  return(r);
}

void wf_free(p) char *p;
{ free(p);
}

void wf_error(s) char *s;
{ error(s);
}

void wf_warn(s) char *s;
{ fprintf(stderr, "warning: %s\n", s);
}


/* unique values to identify options */  

enum bms_opt
{ opt_height, opt_width, opt_x, opt_y, opt_range,
    opt_julia, opt_colours, opt_cx, opt_cy,
    opt_chunk_width, opt_chunk_height, opt_verbose,
    opt_nooutput, opt_statistics, opt_version, opt_ascii
};

struct option
{ enum bms_opt id;
  char *name;
  int has_arg;
};

struct option options[] = 
{ { opt_height, "height", 1 }, 
  { opt_width, "width", 1 },
  { opt_x, "x", 1 },
  { opt_y, "y", 1 },
  { opt_range, "range", 1 },
  { opt_julia, "julia", 0 },
  { opt_colours, "colours", 1 },
  { opt_colours, "colors", 1 },
  { opt_colours, "iterations", 1 },
  { opt_cx, "cx", 1 },
  { opt_cy, "cy", 1 },
  { opt_chunk_width, "chunk_width", 1 },
  { opt_chunk_height, "chunk_height", 1 },
  { opt_verbose, "verbose", 0 },
  { opt_nooutput, "nooutput", 0 },
  { opt_statistics, "statistics", 0 },
  { opt_version, "version", 0 },
  { opt_ascii, "ascii", 0 }
};


int main(argc, argv)
     int argc; char **argv;
{ bms_state bms;
  wf_state *workforce;
  int isock;
  int i,j; 
  char *optarg;
  fd_set readfds;
  struct timeval tv, zero_tv;

  me=argv[0];

  /* defaults */
  bms.ms.width = 64;
  bms.ms.height = 24;
  bms.ms.center_x = -0.5;
  bms.ms.center_y = 0.0;
  bms.ms.xrange = 4.0;
  bms.ms.julia = 0;
  /* bms.ms.c_x...*/
  bms.ms.job.iteration_limit=250;

  bms.ms.chunk_height = bms.ms.chunk_width = 32;

  for(i=1; i<argc; i++)
  { char *arg = argv[i];
    if(*arg == '-')
      for(j=0; j<sizeof(options)/sizeof(options[0]); j++)
      { if(strcmp(arg+1, options[j].name) == 0)
	{ if(options[j].has_arg)
	  { if(argc < i)
	      error("option requires argument");
	    optarg = argv[++i];
	  }
	  switch(options[j].id)
	  { case opt_width: bms.ms.width=atoi(optarg); break;
	    case opt_height: bms.ms.height=atoi(optarg); break;
	    case opt_x: bms.ms.center_x=atof(optarg); break;
	    case opt_y: bms.ms.center_y=atof(optarg); break;
	    case opt_range: bms.ms.xrange=atof(optarg); break;
 	    case opt_julia: bms.ms.julia=1; break;
	    case opt_colours: bms.ms.job.iteration_limit=atoi(optarg); break;
	    case opt_cx: bms.ms.c_x=atof(optarg); break;
	    case opt_cy: bms.ms.c_y=atof(optarg); break;
	    case opt_chunk_width: bms.ms.chunk_width=atoi(optarg); break;
	    case opt_chunk_height: bms.ms.chunk_height=atoi(optarg); break;
	    case opt_verbose: verbose++; break;
	    case opt_nooutput: nooutput++; break;
	    case opt_statistics: statistics++; break;
	    case opt_version:
	      printf("bms version %s\n", ms_version);
	      exit(0);
	      break;
	    case opt_ascii: pgm_ascii++; break;
            default: error("internal option procesing error");
	  }
	  goto nextopt;
	} /* if !strcmp */
      } /* for j */
    error("unrecognized option");
   nextopt: ;
  } /* for i */

  bms.ms.bytes_per_count = (bms.ms.job.iteration_limit > 256 ? 2 : 1);

  ms_init(&bms.ms, workforce = wf_init(TIMEOUT));
  isock=wf_socket(workforce);

  ms_calculate_job_parameters(&bms.ms, &bms.ms.job);

  /* allocate memory for frame buffer */
  switch(bms.ms.bytes_per_count)
  { case 1:
      bms.frame.bytes = (unsigned char *)
	malloc(bms.ms.width * bms.ms.height * sizeof(unsigned char));
      break;
    case 2:
      bms.frame.shorts = (unsigned short *)
	malloc(bms.ms.width * bms.ms.height * sizeof(unsigned short));
      break;
    default:
      error("bad iteration count size"); break;
  }
  
  ms_dispatch_rect(&bms.ms, (char *) &bms, 0, 0, bms.ms.width, bms.ms.height);

  zero_tv.tv_sec=0;
  zero_tv.tv_usec=0;
  tv.tv_sec=1;
  tv.tv_usec=0;

  /* main loop: receive replies and build the picture in memory */
  while(! done)
  { int nready;

    /* keep polling as long as there is data; it's no use bothering */
    /* with timeouts if we're fully occupied handling incoming data */
    do
    {
    redo_1:
      FD_ZERO(&readfds);
      FD_SET(isock, &readfds);
      nready=select(isock+1, &readfds, NULL, NULL, &zero_tv);
      if(nready== -1)
      { if(errno==EINTR)
	  goto redo_1;
	error("select");
      }
      else if(nready>0 && FD_ISSET(isock, &readfds))
      	wf_handle_socket_input(workforce, (char *) 0);
    } while(nready);

    /* no more data; handle any pending timeouts and then go to sleep for */
    /* a second or until new data arrive */
    wf_tick(workforce);

  redo_2:
    FD_ZERO(&readfds);
    FD_SET(isock, &readfds);
    nready=select(isock+1, &readfds, NULL, NULL, &tv);
    if(nready== -1)
    { if(errno==EINTR)
	goto redo_2;
      error("select");
    }
    else if(nready>0 && FD_ISSET(isock, &readfds))
      wf_handle_socket_input(workforce, (char *) 0);
  }

  /* we have received all the replies; write out the finished picture */
  if(! nooutput)
  { 
    if(verbose)
    { putc('\n', stderr);
    }
    if(bms.ms.bytes_per_count > 1 || pgm_ascii) /* ASCII pgm format */
    { unsigned i, j;
      printf("P2\n%d %d\n%d\n",
	     bms.ms.width, bms.ms.height, bms.ms.job.iteration_limit-1);
      for(j=0; j<bms.ms.height; j++)
      {	for(i=0; i<bms.ms.width; i++)
	  printf("%d%c",
		 bms.ms.bytes_per_count == 2 ? 
  		   bms.frame.shorts[j*bms.ms.width+i] :
		   bms.frame.bytes[j*bms.ms.width+i],
		 i%PGM_NPL==(PGM_NPL-1) ? '\n' : ' ');
	if(i%PGM_NPL)
	  putchar('\n');
      }
    }
    else /* binary pgm format */
    { unsigned i, j;
      printf("P5\n%d %d\n%d\n", 
	     bms.ms.width, bms.ms.height, bms.ms.job.iteration_limit-1);
#ifdef PARANOIA
      for(j=0; j<bms.ms.height; j++)
	for(i=0; i<bms.ms.width; i++)
	  putchar(bms.frame.bytes[j*bms.ms.width+i]);
#else
      /* we happen to use the same internal representation for the image */
      /* as the data part of a binary pgm file, so we can dump it as such */
      fwrite(bms.frame.bytes, sizeof(char),
	     bms.ms.width*bms.ms.height, stdout);
#endif
    }
  }
  if(statistics)
  { wf_print_stats(workforce, stderr);
  }
  exit(0);
}
