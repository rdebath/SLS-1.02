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

#include <ctype.h>
#include "funcdef.h"
#define obstack_chunk_alloc ck_malloc
#define obstack_chunk_free free
#include "obstack.h"
#include "sysdef.h"

#include "global.h"
#include "cell.h"
#include "eval.h"
#include "errors.h"

struct value {
	int	type;
	union vals x;
};

#define Float	x.c_d
#define String	x.c_s
#define Int	x.c_l
#define Value	x.c_i
#define Rng	x.c_r

#define ERROR(x)	\
 {			\
	p->Value=x;	\
	p->type=TYP_ERR;\
	return;		\
 }
	

extern struct obstack tmp_mem;

extern char *flt_to_str();

static void
do_edit (numarg,p)
     int numarg;
      struct value * p;
{
	int mm;
	int add_len;
	int tmp_len;
	char *ptr1,*ptr2,*retp;
	int off1,off2;

	if(numarg<3)
		ERROR(BAD_INPUT);
	for(mm=3,add_len=0;mm<numarg;mm++)
		add_len+=strlen((p+mm)->String);
	tmp_len=strlen(p->String);
	off1=(p+1)->Int;
	off2=(p+2)->Int;
	if(off1==0 || tmp_len < ((off1<0) ? -off1 : off1) ||
	   off2==0 || tmp_len < ((off2<0) ? -off2 : off2))
		ERROR(OUT_OF_RANGE);
	ptr1=p->String + (off1>0 ? off1-1 : tmp_len+off1);
	ptr2=p->String + 1 + (off2>0 ? off2-1 : tmp_len+off2);
	if(ptr1>ptr2)
		ERROR(OUT_OF_RANGE);
	retp=obstack_alloc(&tmp_mem,add_len+tmp_len-(ptr2-ptr1));
	strncpy(retp,p->String,ptr1-p->String);
	retp[ptr1-p->String]='\0';
	for(mm=3;mm<numarg;mm++)
		strcat(retp,(p+mm)->String);
	strcat(retp,ptr2);
	p->String=retp;
	p->type=TYP_STR;
}

static void
do_repeat (p)
     struct value * p;
{
	char *str = (p  )->String;
	long num  = (p+1)->Int;

	char *ret;
	char *strptr;
	int len;

	if(num<0)
		ERROR(OUT_OF_RANGE);
	len=strlen(str);
	ret=strptr=obstack_alloc(&tmp_mem,len*num+1);
	while(num--) {
		bcopy(str,strptr,len);
		strptr+=len;
	}
	*strptr=0;
	p->String=ret;
}

static void
do_len (p)
     struct value * p;
{
	long ret;
	char *ptr;

	for(ret=0,ptr=p->String;*ptr;ret++,ptr++)
		;
	p->Int=ret;
	p->type=TYP_INT;
}

static void
do_up_str (p)
     struct value * p;
{
	char *s1,*s2;
	char *strptr;

	strptr=obstack_alloc(&tmp_mem,strlen(p->String)+1);
	for(s1=strptr,s2=p->String;*s2;s2++)
		*s1++ = (islower(*s2) ? toupper(*s2) : *s2);
	*s1=0;
	p->String=strptr;
}

static void
do_dn_str (p)
     struct value * p;
{
	char *s1,*s2;
	char *strptr;

	strptr=obstack_alloc(&tmp_mem,strlen(p->String)+1);
	for(s1=strptr,s2=p->String;*s2;s2++)
		*s1++ = (isupper(*s2) ? tolower(*s2) : *s2);
	*s1=0;
	p->String=strptr;
}

static void
do_cp_str (p)
     struct value * p;
{
	char *strptr;
	char *s1,*s2;
	int wstart=1;

	strptr=obstack_alloc(&tmp_mem,strlen(p->String)+1);
	for(s1=strptr,s2=p->String;*s2;s2++) {
		if(!isalpha(*s2)) {
			wstart=1;
			*s1++= *s2;
		} else if(wstart) {
			*s1++ = (islower(*s2) ? toupper(*s2) : *s2);
			wstart=0;
		} else
			*s1++ = (isupper(*s2) ? tolower(*s2) : *s2);
	}
	*s1=0;
	p->String=strptr;
}

static void
do_trim_str (p)
     struct value * p;
{
	char *s1,*s2;
	int sstart=0;
	char *strptr;

	strptr=obstack_alloc(&tmp_mem,strlen(p->String)+1);
	for(s1=strptr,s2=p->String;*s2;s2++) {
		if(!isascii(*s2) || !isprint(*s2))
			continue;
		if(*s2==' ') {
				if(sstart) {
				*s1++= *s2;
				sstart=0;
			}
		} else {
			sstart=1;
			*s1++= *s2;
		}
	}
	*s1=0;
	p->String=strptr;
}

static void
do_concat ( numarg,p)
     int  numarg;
      struct value * p;
{
	int cur_string;
	char *s;
	char buf[40];
	CELLREF crow,ccol;
	CELL *cell_ptr;

	for(cur_string=0;cur_string<numarg;cur_string++) {
		switch(p[cur_string].type) {
		case 0:
			continue;
		case TYP_RNG:
			for(crow=p[cur_string].Rng.lr;crow<=p[cur_string].Rng.hr;crow++)
				for(ccol=p[cur_string].Rng.lc;ccol<=p[cur_string].Rng.hc;ccol++) {
					if(!(cell_ptr=find_cell(crow,ccol)))
						continue;
					switch(GET_TYP(cell_ptr)) {
					case 0:
						break;
					case TYP_STR:
						(void)obstack_grow(&tmp_mem,cell_ptr->cell_str,strlen(cell_ptr->cell_str));
						break;
					case TYP_INT:
						sprintf(buf,"%ld",cell_ptr->cell_int);
						(void)obstack_grow(&tmp_mem,buf,strlen(buf));
						break;
					case TYP_FLT:
						s=flt_to_str(cell_ptr->cell_flt);
						(void)obstack_grow(&tmp_mem,s,strlen(s));
						break;
					default:
						(void)obstack_finish(&tmp_mem);
						ERROR(NON_STRING);
					}
			}
			break;
		case TYP_STR:
			s=p[cur_string].String;
			(void)obstack_grow(&tmp_mem,s,strlen(s));
			break;
		case TYP_INT:
			sprintf(buf,"%ld",p[cur_string].Int);
			(void)obstack_grow(&tmp_mem,buf,strlen(buf));
			break;
		case TYP_FLT:
			s=flt_to_str(p[cur_string].Float);
			(void)obstack_grow(&tmp_mem,s,strlen(s));
			break;
		default:
			(void)obstack_finish(&tmp_mem);
			ERROR(NON_STRING);
		}
	}
	(void)obstack_1grow(&tmp_mem,0);
	p->type=TYP_STR;
	p->String=(char *)obstack_finish(&tmp_mem);
}


static void
do_mid (p)
     struct value * p;
{
	char *str = (p  )->String;
	long from = (p+1)->Int;
	long len =  (p+2)->Int;

	char	*ptr1;
	int tmp;

	tmp=strlen(str);

	if(from<0 || len<0)
		ERROR(OUT_OF_RANGE);
	ptr1=(char *)obstack_alloc(&tmp_mem,len+1);
	if(from>=tmp || len==0)
		ptr1[0]='\0';
	else {
		strncpy(ptr1,str+from,len);
		ptr1[len]='\0';
	}
	p->String=ptr1;
}


static void
do_substr (p)
     struct value * p;
{
	long off1 = (p  )->Int;
	long off2 = (p+1)->Int;
	char *str = (p+2)->String;

	char	*ptr1,	*ptr2;
	int tmp;
	char *ret;

	tmp=strlen(str);
	if(off1==0 || tmp < ((off1<0) ? -off1 : off1) ||
	   off2==0 || tmp < ((off2<0) ? -off2 : off2))
		ERROR(OUT_OF_RANGE);
	ptr1=str + (off1>0 ? off1-1 : tmp+(off1));
	ptr2=str + (off2>0 ? off2-1 : tmp+(off2));

	if(ptr1>ptr2)
		ERROR(OUT_OF_RANGE);
	tmp=(ptr2-ptr1)+1;
	ret=(char *)obstack_alloc(&tmp_mem,tmp+1);
	strncpy(ret,ptr1,tmp);
	ret[tmp]=0;
	p->String=ret;
	p->type=TYP_STR;
}

static void
do_strstr (p)
     struct value * p;
{
	char *str1	= (p  )->String;
	char *strptr	= (p+1)->String;
	long off	= (p+2)->Int;
	char *ret;

	if(off<1 || strlen(strptr)<=off-1)
		ERROR(OUT_OF_RANGE);
	ret=(char *)strstr(strptr+off-1,str1);
	p->Value= ret ? 1 + ret-strptr : 0;
	p->type=TYP_INT;
}

struct function string_funs[] = {
{ C_FN1,	X_A1,	"S",    do_len,		"len" },
{ C_FN3,	X_A3,	"SSI",  do_strstr,	"find" },

{ C_FN1,	X_A1,	"S",    do_up_str,	"strupr" },
{ C_FN1,	X_A1,	"S",    do_dn_str,	"strlwr" },
{ C_FN1,	X_A1,	"S",    do_cp_str,	"strcap" },
{ C_FN1,	X_A1,	"S",    do_trim_str,	"trim" },

{ C_FN3,	X_A3,	"IIS",  do_substr,	"substr" },
{ C_FN3,	X_A3,	"SII",  do_mid,		"mid" },

{ C_FN2,	X_A2,	"SI",   do_repeat,	"repeat" },
{ C_FNN,	X_AN,	"EEEE", do_concat,	"concat" },
{ C_FNN,	X_AN,	"SIIS", do_edit,	"edit" },
{ 0,		0,	0,	0,		0 },
};

