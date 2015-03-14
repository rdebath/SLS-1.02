/*////////////////////////////////////////////////////////////////////////
Copyright (c) 1992 Electrotechnical Laboratry (ETL)

Permission to use, copy, modify, and distribute this material
for any purpose and without fee is hereby granted, provided
that the above copyright notice and this permission notice
appear in all copies, and that the name of ETL not be
used in advertising or publicity pertaining to this
material without the specific, prior written permission
of an authorized representative of ETL.
ETL MAKES NO REPRESENTATIONS ABOUT THE ACCURACY OR SUITABILITY
OF THIS MATERIAL FOR ANY PURPOSE.  IT IS PROVIDED "AS IS",
WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.
/////////////////////////////////////////////////////////////////////////
ontent-Type: program/C; charset=US-ASCII
Program:      codess.h
Author:       Yutaka Sato <ysato@etl.go.jp>
Description:

     This program redirects the file I/O of codes.c
     from/to strings on memory.

History:
	92.05.18   created
///////////////////////////////////////////////////////////////////////*/
#include <stdio.h>

/*
main(ac,av)
	char *av[];
{	char in[0x10000],out[0x10000];
	int size;

	size = fread(in,1,sizeof in,stdin);
	in[size] = 0;
	if( strcmp(av[1],"eb") == 0 )
		str_to64(in,strlen(in),out,sizeof(out));
	if( strcmp(av[1],"ub") == 0 )
		str_from64(in,strlen(in),out,sizeof(out));
	if( strcmp(av[1],"eq") == 0 )
		str_toqp(in,strlen(in),out,sizeof(out));
	if( strcmp(av[1],"uq") == 0 )
		str_fromqp(in,strlen(in),out,sizeof(out));
	fprintf(stderr,"%s",out);
}
*/

int _to64(), _from64(), _toqp(), _fromqp();
static
str_callfunc(func,in,isize,out,osize,arg3,arg4)
	int (*func)();
	unsigned char *in,*out;
{	int In,Out;
	int rcode;
	int len;

	In = str_fopen(in,isize);
	Out = str_fopen(out,osize);
out[0] = 0;
	rcode = (*func)(In,Out,arg3,arg4);
	len = str_ftell(Out);
	out[len] = 0;
	str_fflush(Out);
	str_fclose(In);
	str_fclose(Out);
	return len;
}
str_to64(in,isize,out,osize)
	unsigned char *in,*out;
{	int len;
	len = str_callfunc(_to64,in,isize,out,osize);
	return len;
}
str_from64(in,isize,out,osize)
	unsigned char *in,*out;
{	int len;

	return str_callfunc(_from64,in,isize,out,osize,0,NULL);
}
str_toqp(in,isize,out,osize)
	unsigned char *in,*out;
{	int len;

	len = str_callfunc(_toqp,in,isize,out,osize);
	if( 2 < len && out[len-2] == '=' && out[len-1] == '\n' ){
		out[len-2] = 0;
		len -= 2;
	}
	return len;
}
str_fromqp(in,isize,out,osize)
	unsigned char *in,*out;
{
	return str_callfunc(_fromqp,in,isize,out,osize,0,NULL);
}

#include "str_stdio.h"

#define basis_64	_basis_64
#define to64		_to64
#define output64chunk	_output64chunk
#define PendingBoundary	_PendingBoundary
#define from64		_from64
#define toqp		_toqp
#define fromqp		_fromqp
#define char64		_char64
#define	basis_hex	_basis_hex
#define	hexchar		_hexchar
#define	toqp		_toqp
#define	fromqp		_fromqp

#include "codes.c"

