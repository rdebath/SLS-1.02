/* mspawn.c -- Mandelbrot-specific, X-independent MandelSpawn code */
   
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


#include "mspawn.h"
#include "ms_ipc.h" /* for byteorder conversion functions */

void ms_init(ms, wf)
     ms_state *ms;
     struct wf_state *wf;
{ ms->mi_count = 0L; /* done no iterations yet */
  ms->chunks_out=0;
  ms->configuration=0;
  ms->workforce=wf;
}


/* Calculate the various parameters that go into ms.xi.job.  This includes */
/* host to network real number format conversion. */

void 
ms_calculate_job_parameters(ms, j)
     ms_state *ms; struct static_job_info *j;
{ ms->yrange=ms->xrange *
    ((double) ms->height / (double) ms->width);
  j->julia=ms->julia;
  if(j->julia) /* Julia mode */
  { j->z0.re=double_to_net(ms->center_x - ms->xrange / 2.0);
    j->z0.im=double_to_net(ms->center_y - ms->yrange / 2.0);
    j->corner.re=double_to_net(ms->c_x);
    j->corner.im=double_to_net(ms->c_y);
  }
  else /* Mandelbrot */
  { j->corner.re=double_to_net(ms->center_x - ms->xrange / 2.0);
    j->corner.im=double_to_net(ms->center_y - ms->yrange / 2.0);
    j->z0.re=double_to_net(0.0);
    j->z0.im=double_to_net(0.0);
  }
  j->delta.re=double_to_net(ms->xrange/ms->width);
  j->delta.im=double_to_net(ms->yrange/ms->height);
}


void ms_dispatch_chunk(ms, client, rect)
     ms_state *ms; char *client; ms_rectangle rect;
{ ms_job j;
  ms_client_info client_info;
  client_info.configuration=ms->configuration;
  client_info.s=rect; /* structure assignment */
  /* build a job structure in network byte order */
  j.j.flags=htons(ms->show_interior ? MS_OPT_INTERIOR : 0);
  j.j.julia=htons(ms->job.julia);
  j.j.corner.re=htonl(ms->job.corner.re);
  j.j.corner.im=htonl(ms->job.corner.im);
  j.j.z0.re=htonl(ms->job.z0.re);
  j.j.z0.im=htonl(ms->job.z0.im);
  j.j.delta.re=htonl(ms->job.delta.re);
  j.j.delta.im=htonl(ms->job.delta.im);
  j.j.iteration_limit=htonl(ms->job.iteration_limit);
  j.s.x=htons(rect.x);
  j.s.width=htons(rect.width);
  j.s.y=htons(rect.y);
  j.s.height=htons(rect.height);
  /* ..and put it on the work queue */
  wf_dispatch_chunk(ms->workforce, client,
		(char *) &client_info, sizeof(ms_client_info),
		(char *) &j, sizeof(j));
  ms->chunks_out++; /* one more to wait for */
}


/* take a rectangular area, split it into pieces and send the pices */
/* out to be calculated */

void ms_dispatch_rect(ms, client, rx, ry, rwidth, rheight)
     ms_state *ms;
     char *client;
     unsigned rx, ry, rwidth, rheight; 
{ ms_rectangle r;
  unsigned int right_edge = rx+rwidth;
  unsigned int bottom_edge = ry+rheight;
  unsigned int x, y;
  
  for(y=ry; y < bottom_edge; y+=ms->chunk_height) 
    for(x=rx; x < right_edge; x+=ms->chunk_width) 
    { r.x = x;
      r.y = y;
      r.width = MIN(ms->chunk_width, right_edge-x);
      r.height = MIN(ms->chunk_height, bottom_edge-y);
      ms_dispatch_chunk(ms, client, r);
    }
  wf_restart(ms->workforce);
}


void wf_draw(client, client_data, data)
     char *client;
     char *client_data;
     char *data; 
{ ms_draw(client, client_data, data);
}

