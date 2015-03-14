/* mspawn.h - define X-independent MandelSpawn state */
   
#ifndef _mspawn_h
#define _mspawn_h

#include "ms_job.h"
#include "ms_real.h" /* for "complex" declaration */

#ifndef MIN
#define MIN(x,y) ((x) < (y) ? (x) : (y))
#endif
#ifndef MAX
#define MAX(x,y) ((x) > (y) ? (x) : (y))
#endif
#ifndef ABS
#define ABS(x) ((x) < 0 ? -(x) : (x))
#endif

/* This is what is stored in the client_data field in the chunk; */
/* basically this contains all the information that is needed to */
/* know what to do when a reply returns from a slave */
typedef struct
{ unsigned int configuration;	/* used to check for obsolete replies */
  ms_rectangle s;		/* rectangle being updated */
} ms_client_info;

typedef struct ms_state
{ unsigned height;
  unsigned width;
  double center_x;		/* x coord. of view center */
  double center_y;		/* y coord. of view center */
  double xrange;		/* real axis interval */
  double yrange;		/* imaginary axis interval */
  int julia;			/* Julia set mode (used as a Bool by Ms.c) */
  double c_x;			/* c value for Julia set only, real part */
  double c_y;			/*   imaginary part */
  unsigned long mi_count;	/* total no. of iterations done */
  struct static_job_info job;	/* buffer for data passed to the slave */
  unsigned bytes_per_count;	/* number of bytes in iteration count */
  unsigned chunks_out;		/* number of chunks being calculated */
  unsigned configuration;	/* serial no. of current setup */
  unsigned chunk_height;	/* height of pixel block */
  unsigned chunk_width;		/* width of pixel block */
  struct wf_state *workforce;	/* pointer to workforce data */
  int show_interior;		/* flag: display interior structure */
} ms_state;

void ms_init();
void ms_calculate_job_parameters();
void ms_dispatch_chunk();
void ms_dispatch_rect();

#endif /* _mspawn_h */
