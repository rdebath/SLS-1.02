/*	Copyright (C) 1990, 1992, 1993 Free Software Foundation, Inc.

This file is part of Oleo, the GNU Spreadsheet.

Oleo is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Oleo is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Oleo; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "funcdef.h"
#include <stdio.h>
#include <ctype.h>
#include "sysdef.h"
#include "global.h"
#include "cell.h"
#include "io-generic.h"
#include "io-abstract.h"
#include "io-utils.h"
#include "lists.h"
#include "ref.h"
#include "parse.h"
#include "regions.h"



/* This reads/writes a subset of the SC public-domain spreadsheet's
   file-format.  Note that since SC has no way of encoding some information
   about a cell, writing a spread out in SC format, then reading it back in
   will result in the loss of some information (most important:  cell formats)
 */

static int
get_range (pp,rp)
     char ** pp;
      struct rng * rp;
{
	int byte;
	char *p;
	struct var *v;


	while(isspace(**pp))
		(*pp)++;
	byte=parse_cell_or_range(pp,rp);
	if(byte)
		return 0;
	for(p= *pp;*p && !isspace(*p);p++)
		;
	v=find_var(*pp,p-*pp);
	if(!v)
		return 1;
	*pp=p;
	*rp=v->v_rng;
	return 0;
}

void
sc_read_file (fp,ismerge)
     FILE * fp;
      int ismerge;
{
	char buf[2048];
	int lineno;
	char *ptr;
	int n;
	struct rng rng;
	int olda0;

	olda0=a0;
	a0=1;
	lineno=0;
	if(!ismerge)
		clear_spreadsheet();
	while(fgets(buf,sizeof(buf),fp)) {
		lineno++;
		if(lineno%50==0)
			io_info_msg("Line %d",lineno);
		if(buf[0]=='#' || buf[0]=='\n')
			continue;
		if(!strncmp(buf,"set ",4)) {
			/* ... */
		} else if(!strncmp(buf,"format ",7)) {
			ptr=buf+7;
			if(get_range(&ptr,&rng))
				continue;
			n=astol(&ptr);
			set_width(rng.lc,n);
		} else if(!strncmp(buf,"hide ",5)) {
			ptr=buf+5;
			if(get_range(&ptr,&rng))
				continue;
			set_width(rng.lc,0);
		} else if(!strncmp(buf,"mdir ",5)) {
			/* ... */
		} else if(!strncmp(buf,"define ",7)) {
			char *eptr;

			ptr=buf+7;
			while(isspace(*ptr))
				ptr++;
			if(*ptr!='"') {
				io_error_msg("Line %d: No starting \" in define",lineno);
				continue;
			}
			ptr++;
			for(eptr=ptr; *eptr && *eptr!='"';eptr++)
				;
			if(!*eptr) {
				io_error_msg("Line %d: No starting \" in define",lineno);
				continue;
			}
			ptr=new_var_value(ptr,eptr-ptr,eptr+1);
			if(ptr)
				io_error_msg("Line %d: %s",ptr);
		} else if(!strncmp(buf,"leftstring ",11) ||
			  !strncmp(buf,"rightstring ",12)) {
			CELL *cp;

			ptr=buf+11;
			if(get_range(&ptr,&rng))
				continue;
			while(isspace(*ptr))
				ptr++;
			if(*ptr=='=')
				ptr++;
			new_value(rng.lr,rng.lc,ptr);
			cp=find_cell(rng.lr,rng.lc);
			if(buf[0]=='l')
				SET_JST(cp,JST_LFT);
			else
				SET_JST(cp,JST_RGT);

		} else if(!strncmp(buf,"let ",4)) {
			ptr=buf+4;
			if(get_range(&ptr,&rng))
				continue;
			while(isspace(*ptr))
				ptr++;
			if(*ptr=='=')
				ptr++;
			new_value(rng.lr,rng.lc,ptr);
		} else
			io_error_msg("Line %d: Can't parse %s",lineno,buf);
	}
	a0=olda0;
	io_recenter_all_win();
}

static FILE *sc_fp;
static struct rng *sc_rng;
static void
sc_write_var (name,var)
     char * name;
      struct var * var;
{
	if(var->var_flags==VAR_UNDEF && (!var->var_ref_fm || var->var_ref_fm->refs_used==0))
		return;
	switch(var->var_flags) {
	case VAR_UNDEF:
		break;
	case VAR_CELL:
		if(var->v_rng.lr>=sc_rng->lr && var->v_rng.lr<=sc_rng->hr && var->v_rng.lc>=sc_rng->lc && var->v_rng.lc<=sc_rng->hc)
			(void)fprintf(sc_fp,"define \"%s\" %s\n",var->var_name,cell_name(var->v_rng.lr,var->v_rng.lc));
		break;
	case VAR_RANGE:
		if(var->v_rng.lr<sc_rng->lr || var->v_rng.hr>sc_rng->hr || var->v_rng.lc<sc_rng->lc || var->v_rng.hc>sc_rng->hc)
			break;

		(void)fprintf(sc_fp,"define \"%s\" %s\n",var->var_name,range_name(&(var->v_rng)));
		break;
#ifdef TEST
	default:
		panic("Unknown var type %d",var->var_flags);
		break;
#endif
	}
}
	
void
sc_write_file (fp,rng)
     FILE * fp;
      struct rng * rng;
{
	unsigned short w;
	CELLREF r,c;
	CELL *cp;
	char *ptr;
	int olda0;

	if(!rng)
		rng= &all_rng;

	olda0=a0;
	a0=1;
	(void)fprintf(fp,"# This file was created by Oleo, for use by the Spreadsheet Calculator\n");
	(void)fprintf(fp,"# You probably don't want to edit it.\n\n");

	find_widths(rng->lc,rng->hc);
	while(w=next_width(&c))
		fprintf(fp,"format %s %d ???\n",cell_name(MIN_ROW,c),w);
	sc_fp=fp;
	sc_rng=rng;
	for_all_vars(sc_write_var);
	find_cells_in_range(rng);
	while(cp=next_row_col_in_range(&r,&c)) {
		switch(GET_TYP(cp)) {
		case TYP_STR:
			if((GET_JST(cp)==JST_DEF && default_jst==JST_RGT) || GET_JST(cp)==JST_RGT)
				ptr="right";
			else ptr="left";
			fprintf(fp,"%sstring %s = %s\n",ptr,cell_name(r,c),decomp(r,c,cp));
			decomp_free();
			break;
		case 0:
			break;
		default:
			fprintf(fp,"let %s = %s\n",cell_name(r,c),decomp(r,c,cp));
			decomp_free();
			break;
		}
	}
	a0=olda0;
}

int
sc_set_options (set_opt,option)
     int set_opt;
      char * option;
{
	return -1;
}

void
sc_show_options ()
{
	io_text_line("File format: sc  (Public domain spreadsheet calculator)");
}
