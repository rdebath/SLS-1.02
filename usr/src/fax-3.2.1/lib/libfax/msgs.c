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

#include <stdio.h>

#include "c2proto.h"
#include "msgs.h"

static char *modem_result_strings[] = {
    "ok", "connect", "ring", "no carrier", "error", "no dialtone", "busy",
    "no answer",
};

char *hayes_result_msg(result)
     int result;
{
    if (result < 0 || result > 8)
      return ("unknown");
    else
      return (modem_result_strings[result]);
}

char *faxmodem_result_msg(f)
     FaxModem *f;
{
    return (hayes_result_msg(f->result));
}

void faxmodem_print_id_strings(f, fp)
     FaxModem *f;
     FILE *fp;
{
    if (FAX_ISSET(f, FAX_F_FTSI))
      fprintf(fp, "tsi: %s\n", f->ftsi_id);
    if (FAX_ISSET(f, FAX_F_FCSI))
      fprintf(fp, "csi: %s\n", f->fcsi_id);
    if (FAX_ISSET(f, FAX_F_FCIG))
      fprintf(fp, "cig: %s\n", f->fcig_id);
}

char *t30_vr_string(p)
     T30params *p;
{
    switch (p->vr) {
      case VR_NORMAL:
	return ("normal");
      case VR_FINE:
	return ("fine");
      default:
	return ("unknown");
    }
}

char *t30_br_string(p)
     T30params *p;
{
    switch (p->br) {
      case BR_2400:
	return ("2400");
      case BR_4800:
	return ("4800");
      case BR_7200:
	return ("7200");
      case BR_9600:
	return ("9600");
      default:
	return ("unknown");
    }
}

char *t30_wd_string(p)
     T30params *p;
{
    switch (p->wd) {
      case WD_1728:
	return ("215");
      case WD_2048:
	return ("255");
      case WD_2432:
	return ("303");
      default:
	return ("unknown");
    }
}

char *t30_ln_string(p)
     T30params *p;
{
    switch (p->ln) {
      case LN_A4:
	return ("A4");
      case LN_B4:
	return ("B4");
      case LN_UNLIMITED:
	return ("unlimited");
      default:
	return ("unknown");
    }
}

char *t30_df_string(p)
     T30params *p;
{
    switch (p->df) {
      case DF_1DHUFFMAN:
	return ("1D huffman");
      case DF_2DMREAD:
	return ("2D read");
      case DF_2DUNCOMP:
	return ("2D uncompressed");
      default:
	return ("unknown");
    }
}

char *t30_ec_string(p)
     T30params *p;
{
    switch (p->ec) {
      case EC_DA_ECM:
	return ("disabled");
      case EC_EN_ECM_64:
	return ("ECM 64");
      case EC_EN_ECM_256:
	return ("ECM_256");
      default:
	return ("unknown");
    }
}

char *t30_bf_string(p)
     T30params *p;
{
    switch(p->bf) {
      case BF_DISABLED:
	return ("disabled");
      case BF_ENABLED:
	return ("enabled");
      default:
	return ("unknown");
    }
}

char *t30_st_string(p)
     T30params *p;
{
    switch(p->st) {
      case ST_0:
	return("N-0 F-0");
      case ST_1:
	return("N-5 F-5");
      case ST_2:
	return("N-10 F-5");
      case ST_3:
	return("N-10 F-10");
      case ST_4:
	return("N-20 F-10");
      case ST_5:
	return("N-20 F-20");
      case ST_6:
	return("N-40 F-20");
      case ST_7:
	return("N-40 F-40");
      default:
	return ("unknown");
    }
}
