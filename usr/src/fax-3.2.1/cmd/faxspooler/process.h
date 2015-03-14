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

#ifndef in_faxspooler_process_h
#define in_faxspooler_process_h 1

/*
  Prototypes:
*/

int process_queue(
#ifdef _PROTO
     char *out_qdir,
     char *in_qdir,
     char *in_email,
     FaxModem *fmp,
     int numb_fm,
     int max_delivery_time,
     int min_retry_wait
#endif
);

int run_queue(
#ifdef _PROTO
     spooler_data *sd,
     DIO *dio
#endif
);

#endif
