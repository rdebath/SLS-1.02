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

#ifndef in_faxspooler_queue_h
#define in_faxspooler_queue_h 1

/*
  Prototypes:
*/

int queue_read(
#ifdef _PROTO
     LIST *q,
     char *qdir
#endif
);

int queue_add_file(
#ifdef _PROTO
     LIST *q,
     char *qdir,
     char *file
#endif
);

int queue_delete_file(
#ifdef _PROTO
     LIST *q,
     char *file
#endif
);

#endif
