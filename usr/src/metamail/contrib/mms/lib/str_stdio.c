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
Content-Type: program/C; charset=US-ASCII
Program:      str_stdio.h
Author:       Yutaka Sato <ysato@etl.go.jp>
Description:

     This program redirects the file I/O from/to strings on memory.
     Include "str_stdio.h" file after <stdio.h>

History:
	92.05.18   created
///////////////////////////////////////////////////////////////////////*/
#include <stdio.h>
typedef unsigned char Uchar;

#define Str_MAGIC 0x12345678
typedef struct {
	FILE	s_FILE;
	int	s_magic;
	Uchar  *s_base;
	int	s_peak;
	int	s_maxsize;		/* limit of auto expansion */
	int	s_size;
} String;

str_isStr(Str)
	String *Str;
{
	if( Str->s_magic == Str_MAGIC )
		return 1;
	return 0;
}
String *
str_fopen(buf,size)
	unsigned char *buf;
{	String *Str;

	Str = (String*)calloc(1,sizeof(String));
	Str->s_magic = Str_MAGIC;
	Str->s_base = buf;
	Str->s_size = size;
	Str->s_peak = 0;
	return Str;
}
str_fclose(Str)
	String *Str;
{
	if( !str_isStr(Str) )
		return fclose(Str);

	str_fflush(Str);
	free(Str);
	return 0;
}

str_getc(Str)
	String *Str;
{
	if( !str_isStr(Str) )
		return fgetc(Str);

	if( Str->s_size <= Str->s_peak )
		return EOF;

	return Str->s_base[Str->s_peak++];
}
str_ungetc(ch,Str)
	String *Str;
{
	if( !str_isStr(Str) )
		return ungetc(ch,Str);

	if( Str->s_peak <= 0)
		return EOF;

	Str->s_base[--Str->s_peak] = ch;
	return ch;
}
char *
str_fgets(buf,size,Str)
	char *buf;
	String *Str;
{	int rsize;

	if( !str_isStr(Str) )
		return fgets(buf,size,Str);

	rsize = Str->s_size - Str->s_peak;
	if( rsize <= 0 )
		return NULL;
	if( rsize < size )
		size = rsize;

	strncpy(buf,Str->s_peak,size);
	Str->s_peak += size;
	return buf;
}

str_putc(ch,Str)
	String *Str;
{
	if( !str_isStr(Str) )
		return fputc(ch,Str);

	if( Str->s_size <= Str->s_peak )
		return EOF;

	Str->s_base[Str->s_peak++] = ch;

	return ch;
}
str_fputs(buf,Str)
	char *buf;
	String *Str;
{	int size,rsize;

	if( !str_isStr(Str) )
		return fputs(buf,Str);

	rsize = Str->s_size - Str->s_peak;
	if( rsize <= 0 )
		return EOF;

	size = strlen(buf);
	if( size == 0 )
		return;
	if( rsize < size )
		size = rsize;

	strncpy(&Str->s_base[Str->s_peak],buf,size);
	Str->s_peak += size;

	return 0;
}

str_fflush(Str)
	String *Str;
{
	if( !str_isStr(Str) )
		return fflush(Str);

	Str->s_base[Str->s_peak] = 0;
	return 0;
}
str_fprintf(Str,form,a,b,c,d,e,f)
	String *Str;
{	int wlen;
	unsigned char *peakp;

	if( !str_isStr(Str) )
		return fprintf(Str,form,a,b,c,d,e,f);

	peakp = &Str->s_base[Str->s_peak];
	sprintf(peakp,form,a,b,c,d,e,f);
	wlen = strlen(peakp);

	Str->s_peak += wlen;
	return  wlen;
}
str_fseek(Str,off,where)
	String *Str;
{	int noff;

	if( !str_isStr(Str) )
		return fseek(Str,off,where);

	switch( where ){
		case 0: noff = off; break;
		case 1: noff = Str->s_peak + off; break;
		case 2: noff = Str->s_size-1 + off; break;
		default: return -1;
	}

	if( noff < 0 || Str->s_size <= noff )
		return -1;
	Str->s_peak = noff;
}
str_ftell(Str)
	String *Str;
{
	if( !str_isStr(Str) )
		return ftell(Str);

	return Str->s_peak;
}
