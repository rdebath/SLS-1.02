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
Program:      mmsencode.c (encoder)
Author:       Yutaka Sato <ysato@etl.go.jp>

History:
        92.05.15     extracted from mmsclient.c
        92.05.16     added MIME header encoder/decoder for ISO-2022-JP
///////////////////////////////////////////////////////////////////////*/

#include <stdio.h>
#include "str_stdio.h"
FILE *str_fopen();
char *index(),*getenv();
#define MAX_LNSIZE		512

/*//////////////////////////////////////////////////////////////////////*/

MMS_E_head_ENCODER(ac,av,in,out){
/* test
	char ins[100000],outs[100000];
	int size;

	size = fread(ins,1,sizeof(ins),in);
	ins[size] = 0;
	MIME_strHeaderEncode(ins,outs,sizeof(outs));
	printf("%s\n",outs);
*/
	MIME_headerEncode(in,out);
}
MMS_E_head_DECODER(ac,av,in,out){
/* test
	char ins[100000],outs[100000];
	int size;

	size = fread(ins,1,sizeof(ins),in);
	ins[size] = 0;
	MIME_strHeaderDecode(ins,outs,sizeof(outs));
	printf("%s\n",outs);
*/
	MIME_headerDecode(in,out);
}
MMS_E_decodeBASE16(ac,av,in,out){
	MMS_decodeBASE16(in,out);
}
MMS_E_encodeBASE16(ac,av,in,out){
	MMS_encodeBASE16(in,out);
}
int (*MMS_E_HEAD_ENCODER)() = MMS_E_head_ENCODER;
int (*MMS_E_HEAD_DECODER)() = MMS_E_head_DECODER;

int (*MMS_E_DECODEBASE16)() = MMS_E_decodeBASE16;
int (*MMS_E_ENCODEBASE16)() = MMS_E_encodeBASE16;

/*//////////////////////////////////////////////////////////////////////*/
#define MAXCOL			71
#define SWCODE_LENG		4 /*length(encoded(charset sw ESC seq))*/
#define PAYLOAD			2

#define ENCODE_BEGIN		"=?"
#define CHARSET_DONE		'?'
#define ENCODING_DONE		'?'
#define ENCODE_DONE		"?="

#define NL			'\n'
#define CR			'\r'
#define TAB			'\t'
#define SPACE			' '
#define ESC			033
#define NLNL			(NL<<8|NL)

#define GOTO1BCODE		'('
#define GOTO2BCODE		'$'
#define GOTO_ASCII_SEQ		"\033(B"

char US_ASCII[]		=	"US-ASCII";
char ISO_2022_JP[]	=	"ISO-2022-JP";

#define ENCODE_NONE		 0
#define ENCODE_BASE64		"B"
#define	ENCODE_QP		"Q"

typedef struct {
	int	local;
	char	codesw;
	char   *charset;
	char   *encoding;
} CodeSwitch;

static
CodeSwitch Codes1[16] = {
	{1,	'B',	US_ASCII,	ENCODE_NONE	},
	{1,	'J',	US_ASCII,	ENCODE_NONE	},
	0
};

static
CodeSwitch Codes2[16] = {
	{1,	'@',	ISO_2022_JP,	ENCODE_BASE64	},
	{1,	'B',	ISO_2022_JP,	ENCODE_BASE64	},
	0
};
MMS_localCharset(charset)
	char *charset;
{	int csi;
	char *cs;

	for(csi = 0; cs = Codes1[csi].charset; csi++)
		if( strcasecmp(cs,charset) == 0 )
			return Codes1[csi].local;

	for(csi = 0; cs = Codes2[csi].charset; csi++)
		if( strcasecmp(cs,charset) == 0 )
			return Codes2[csi].local;
	return 0;
}


typedef struct {
	FILE	*in_file;
	char	*in_charset;
	char	*in_encoding;
	int	 in_codesw_seq[4];
	int	 in_prevch;

	FILE	*out_file;
	char	*out_prevcharset;
	char	 out_codesw_seq[4];
	int	 out_column;
} INOUT;

static
INOUT_init(io,in,out)
	INOUT *io;
	FILE *in,*out;
{
	io->in_file = in;
	io->in_charset = US_ASCII;
	io->in_encoding = ENCODE_NONE;
	io->in_codesw_seq[0] = 0;
	io->in_prevch = EOF;

	io->out_file = out;
	io->out_prevcharset = US_ASCII;
	io->out_codesw_seq[0] = 0;
	io->out_column = 0;
}


static
NLfgetc(in)
	FILE *in;
{	int ch;

	ch = fgetc(in);
	if( ch == CR ){
		ch = fgetc(in);
		if( ch != NL ){
			if( ch != EOF )
				ungetc(ch,in);
			ch = CR;
		}
	}
	return ch;
}


static
EN_FGETC(io)
	INOUT *io;
{	int ch;
	CodeSwitch *csw;
	int ci;
	char *charset;
	char swseq[4],*sw;
	FILE *in = io->in_file;

GET1:
	ch = NLfgetc(io->in_file);
GOT1:
	if( ch != ESC )
		goto exit;

	if( (ch = fgetc(in)) == EOF )
		goto exit;

	if( io->in_prevch == NL )
		if( ch == TAB || ch == SPACE )
			goto GET1;

	sw = swseq;
	*sw++ = ESC;
	*sw++ = ch;

	switch( ch ){
		default:	goto exit;
		case GOTO1BCODE: csw = Codes1; break;
		case GOTO2BCODE: csw = Codes2; break;
	}
	if( (ch = fgetc(in)) == EOF )
		goto exit;

	*sw++ = ch;
	*sw++ = 0;

	for( ci = 0; charset = csw[ci].charset; ci++ )
		if( ch == csw[ci].codesw ){
			io->in_charset = charset;
			io->in_encoding = csw[ci].encoding;
			strcpy(io->in_codesw_seq,swseq);
		}

	ch = NLfgetc(io->in_file);
	if( ch == ESC )
		goto GOT1;
exit:
	io->in_prevch = ch;
	return ch;
}

static
ew_overhead(charset,encoding)
	char *charset,*encoding;
{	char overhead[128];

	sprintf(overhead,"=?%s?%s??= ",charset,encoding);
	return strlen(overhead);
}

static EN_FPUTC(ch,io,charset,encoding)
	INOUT *io;
	char *charset,*encoding;
{
	if( charset != io->out_prevcharset ){
		if( io->out_prevcharset != US_ASCII ){
			int len;
			len = fprintf(io->out_file,
				(ch==NL?"%s":"%s "), ENCODE_DONE);
			io->out_column += len;
		}
		if( charset != US_ASCII ){
			int reqlen,remlen,len;

			reqlen = ew_overhead(charset,encoding);
			remlen = MAXCOL - (io->out_column + reqlen);

			if( (remlen-SWCODE_LENG) < PAYLOAD ){
				fprintf(io->out_file,"%c%c",NL,TAB);
				io->out_column = 1;
			}
			len = fprintf(io->out_file,"=?%s?%s?",
				charset,encoding);
			io->out_column += len;
		}
		io->out_prevcharset = charset;
	}

	if( ch != EOF ){
		if( ch == NL )
			io->out_column = 0;
		else{
/*
SHOULD SPLIT AT WHIT CHARACTER
			if( !encoding )
			if( MAXCOL < io->out_column ){
				fputs("\n\t",io->out_file);
				io->out_column = 1;
			}
*/
			io->out_column++;
		}
		fputc(ch,io->out_file);
	}
}

MIME_headerEncode0(in,out)
	FILE *in,*out;
{	char *ip,*op;
	INOUT iob,*io = &iob;
	int ch,prev_ch;

	INOUT_init(io,in,out);
	prev_ch = 0;

	for(;;){
		ch = EN_FGETC(io);
		if( io->in_charset == US_ASCII ){
			if( ch == EOF )
				break;
			if( ch == NL && prev_ch == NL )
				break;
			EN_FPUTC(ch,io,US_ASCII,ENCODE_NONE);
			prev_ch = ch;
			continue;
		}
		ungetc(ch,io->in_file);

		for(;;){
			ch = encode_word(io);
			if( io->in_charset == US_ASCII )
				break;
		}
		if( ch == EOF )
			break;
		ungetc(ch,io->in_file);
	}
	if( ch == EOF )
		EN_FPUTC(ch,io,US_ASCII,ENCODE_NONE);
	return ch;
}
MIME_headerEncode(in,out)
	FILE *in,*out;
{	int ch;

	ch = MIME_headerEncode0(in,out);
	if( ch != EOF ){
		fputc(NL,out);
		while( (ch = NLfgetc(in)) != EOF )
			fputc(ch,out);
	}
}

static
encode_one(encoding,ins,ilen,outs,osize)
	char *encoding,*ins,*outs;
{	int len;

	if( strcasecmp(encoding,ENCODE_QP) == 0 )
		len = str_toqp(ins,ilen,outs,osize);
	else
	if( strcasecmp(encoding,ENCODE_BASE64) == 0 )
		len = str_to64(ins,ilen,outs,osize);
	else{
		strncpy(outs,ins,ilen);
		len = ilen;
	}
	outs[len] = 0;
	return len;
}

static
encode_word(io)
	INOUT *io;
{	char *charset;
	char *encoding;
	char ins[0x200],outs[0x200];
	int inx,outx;
	int nchar,reqlen,remlen;
	int len;
	char ch,encoded_ch;

	charset = io->in_charset;
	encoding = io->in_encoding;
	sprintf(ins,"%s",io->in_codesw_seq);
	inx = strlen(ins);
	len = encode_one(encoding,ins,inx,outs,sizeof(outs));

	reqlen = ew_overhead(charset,encoding);
	remlen = MAXCOL - (io->out_column + reqlen);

	if( (remlen-len) < PAYLOAD ){
		EN_FPUTC(NL,io,US_ASCII,ENCODE_NONE);
		EN_FPUTC(TAB,io,US_ASCII,ENCODE_NONE);
		remlen = MAXCOL - (io->out_column + reqlen);
	}
/*
More DESIREBLE: putting NL when an encoded-word will splitted and
the whole length of it is shorter than MAXCOL.
 */

	for(nchar = 0; ;nchar++){
		len = encode_one(encoding,ins,inx,outs,sizeof(outs));
			if( (remlen <= len) && ((nchar%2)==0))
				break;

		ch = EN_FGETC(io);
		if( ch == EOF ){
			strcpy(&ins[inx],GOTO_ASCII_SEQ);
			inx += strlen(&ins[inx]);
			break;
		}
		if( io->in_charset != charset ){
			strcpy(&ins[inx],io->in_codesw_seq);
			inx += strlen(&ins[inx]);
			break;
		}
		ins[inx++] = ch;
		ins[inx] = 0;
	}
	len = encode_one(encoding,ins,inx,outs,sizeof(outs));

	for(outx = 0; outx < len; outx++){
		encoded_ch = outs[outx];
		if( encoded_ch == NL )
			continue;
		EN_FPUTC(encoded_ch,io,charset,encoding);
	}

	if( remlen <= len ){
		EN_FPUTC(NL,io,US_ASCII,ENCODE_NONE);
		EN_FPUTC(TAB,io,US_ASCII,ENCODE_NONE);
	}
	return ch;
}

static
decode_word(io)
	INOUT *io;
{	char reads[256],charset[128],encoding[128],itext[256],dtext[256];
	int ilen,dch,dsize,len,pad;
	int eow;

	*charset = *encoding = *itext = 0;
	eow = scan_encoded_word(io->in_file,reads,charset,encoding,itext);

	if( eow == NL || eow == EOF ){
		fprintf(io->out_file,"=?%s",reads);
		if(eow != EOF)
		fprintf(io->out_file,"%c",eow);
		return eow;
	}

	if( !MMS_localCharset(charset) ){
		fprintf(io->out_file,"=?%s?%s?%s?=",charset,encoding,itext);
		if( eow )
			fprintf(io->out_file,"%c",eow);
		return 0;
	}

	ilen = strlen(itext);
	dsize = sizeof(dtext);
	if( strcasecmp(encoding,ENCODE_QP) == 0 )
		len = str_fromqp(itext,ilen,dtext,dsize);
	else
	if( strcasecmp(encoding,ENCODE_BASE64) == 0 )
		len = str_from64(itext,ilen,dtext,dsize);
	else{
		strcpy(dtext,itext);
		len = ilen;
	}

	if( 0 < len ){
		FILE *Dout;
		INOUT tinb,*tin = &tinb;
		char *charset1;

		Dout = str_fopen(dtext,len);
		INOUT_init(tin,Dout,NULL);
		charset1 = US_ASCII;

		while((dch = EN_FGETC(tin)) != EOF ){
			if( tin->in_charset != charset1 ){
				set_outcodesw_seq(io,tin->in_codesw_seq);
				charset1 = tin->in_charset;
			}
			DE_FPUTC(dch,io);
		}
		str_fclose(Dout);
		if( charset1 != US_ASCII )
			if( tin->in_charset == US_ASCII )
				set_outcodesw_seq(io,tin->in_codesw_seq);
			else	set_outcodesw_seq(io,GOTO_ASCII_SEQ);
	}
	return 0;
}

static
DE_FPUTC(ch,io)
	INOUT *io;
{	char *seq;

	seq = io->out_codesw_seq;
	if( ch == EOF ){
		/* FLUSH PENDING CODE SWITCH SEQUENCE IF EXISTS */
		if( seq[0] )
			fputs(seq,io->out_file);
	}else{
		if( seq[0] ){
			fputs(seq,io->out_file);
			seq[0] = 0;
		}
		fputc(ch,io->out_file);
	}
}
set_outcodesw_seq(io,seq)
	INOUT *io;
	char *seq;
{
	strcpy(io->out_codesw_seq,seq);
}

static
DE_FGETC(in,unfold)
	FILE *in;
{	int ch;

	ch = NLfgetc(in);
GOT1:
	if( ch == NL ){
		ch = NLfgetc(in);
		if( ch == NL ){
			ch = NLNL;
			goto EXIT;
		}
		if(unfold){
			if( ch == TAB || ch == SPACE ){
				do{	ch = NLfgetc(in);
				}while( ch == TAB || ch == SPACE );
				goto GOT1;
			}
		}
		ungetc(ch,in);
		ch = NL;
	}
EXIT:
	return ch;
}
MIME_headerDecode(in,out)
	FILE *in,*out;
{	int ch,next_ch;
	int unfold = 0;
	INOUT iob,*io = &iob;

	INOUT_init(io,in,out);

	for(;;){
		ch = DE_FGETC(in,unfold);
		if( ch == EOF )
			break;
		if( ch == NL )
			unfold = 0;

		if( ch == ENCODE_BEGIN[0] ){
			ch = NLfgetc(in);
			if( ch == EOF )
				break;
			if( ch == ENCODE_BEGIN[1] ){
				if( decode_word(io) == EOF )
					break;
				unfold = 1;
			}else{
				DE_FPUTC(ENCODE_BEGIN[0],io);
				ungetc(ch,in);
			}
		}else{
			if( ch == NLNL ){
				DE_FPUTC(NL,io);
				DE_FPUTC(NL,io);
				break;
			}
			DE_FPUTC(ch,io);
		}
	}
	if( ch != EOF )
		while( (ch = NLfgetc(in)) != EOF )
			DE_FPUTC(ch,io);
	DE_FPUTC(EOF,io);
}

MIME_strHeaderDecode(ins,outs,osize)
	char *ins,*outs;
{	FILE *In,*Out;
	int oi;

	In = str_fopen(ins,strlen(ins));
	Out = str_fopen(outs,osize);
	MIME_headerDecode(In,Out);
	fflush(Out);
	for(oi = 0; outs[oi]; oi++)
		if((outs[oi] & 0xFF) == 0xFF)
			strcpy(&outs[oi],&outs[oi+1]);
	str_fclose(In);
	str_fclose(Out);
}
MIME_strHeaderEncode(ins,outs,osize)
	char *ins,*outs;
{	FILE *In,*Out;

	In = str_fopen(ins,strlen(ins));
	Out = str_fopen(outs,osize);
	MIME_headerEncode(In,Out);
	fflush(Out);
	str_fclose(In);
	str_fclose(Out);
}

is_MIME_header(fp)
	FILE *fp;
{	char line[256];
	int off;

	off = ftell(fp);
	while( fgets(line,sizeof(line),fp) != NULL ){
		if( *line == NL )
			break;
		if( *line == CR && line[1] == NL )
			break;
		if( strstr(line,ENCODE_BEGIN) ){
			fseek(fp,off,0);
			return 1;
		}
	}
	fseek(fp,off,0);
	return 0;
}

FILE *
MIME_tmpHeaderDecode(fp)
	FILE *fp;
{	FILE *tfp;

	if( fp == NULL )
		return NULL;

	/* if( MMS_seekable(fp) ) */
	if( fseek(fp,0,1) == 0 )
		if( !is_MIME_header(fp) )
			return NULL;

	tfp = tmpfile();
	MIME_headerDecode(fp,tfp);
	fflush(tfp);
	fseek(tfp,0,0);
	return tfp;
}
FILE *
MIME_tmpHeaderEncode(fp,savFILE)
	FILE *fp,savFILE;
{	FILE *tin,*tfp;
	char line[256];
	int ch;

	if( fp == NULL )
		return;
	tin = tmpfile();
	while( fgets(line,sizeof(line),fp) != NULL ){
		fputs(line,tin);
		if(strcmp(line,".\n")==0 || strcmp(line,".\r\n")==0)
			break;
	}
	fflush(tin);
	fseek(tin,0,0);

	tfp = tmpfile();
	ch = MIME_headerEncode0(tin,tfp);
	if( ch == NL ){
		fputs("\r\n",tfp);
		while( fgets(line,sizeof(line),tin) != NULL )
			fputs(line,tfp);
	}
	fputs(".\r\n",tfp);
	fflush(tfp);
	fseek(tfp,0,0);

	fclose(tin);
	return tfp;
}

static
scan_encoded_word(in,reads,charset,encoding,text)
	FILE *in;
	char *reads,*charset,*encoding,*text;
{	int i,cs;

	for(i = 0; ;i++){
		cs = NLfgetc(in);
		*reads++ = cs;
		if(cs==NL || cs==EOF) goto error;
		if(cs==CHARSET_DONE) break;
		charset[i] = cs;
		charset[i+1] = 0;
	}
	for(i = 0; ;i++){
		cs = NLfgetc(in);
		*reads++ = cs;
		if(cs==NL || cs==EOF) goto error;
		if(cs==ENCODING_DONE) break;
		encoding[i] = cs;
		encoding[i+1] = 0;
	}
	for(i = 0; i < 80; i++ ){
		cs = NLfgetc(in);
		*reads++ = cs;
		if(cs==NL || cs==EOF) goto error;
		if(cs == ENCODE_DONE[0]){
			cs = NLfgetc(in);
			*reads++ = cs;
			if(cs==NL || cs==EOF) goto error;
			if( cs == ENCODE_DONE[1] ){
				cs = NLfgetc(in);
				if(cs == SPACE || cs == TAB || cs == EOF)
					break;
				else	ungetc(cs,in);
				text[i] = 0;
				break;
			}
			ungetc(cs,in);
			cs = ENCODE_DONE[0];
		}
		text[i] = cs;
		text[i+1] = 0;
	}
	return 0;
error:
	*reads = 0;
	return cs;
}


/*//////////////////////////////////////////////////////////////////////*/
MMS_encodeBASE16(ifp,ofp)
	register FILE *ifp,*ofp;
{	char ibuf[32],obuf[64+2];
	register char *op;
	register int bytes,rc,ci,ch;

	bytes = 0;
	while( 0 < (rc = fread(ibuf,1,sizeof(ibuf),ifp)) ){
		op = obuf;
		for( ci = 0; ci < rc; ci++ ){
			ch = ibuf[ci];
			*op++ = 'A' + (0xF & (ch>>4));
			*op++ = 'A' + (0xF &  ch    );
			bytes++;
		}
		*op++ = '\n';
		fwrite(obuf,rc*2+1,1,ofp);
	}
	fwrite(".\n",2,1,ofp);
	return bytes;
}

MMS_decodeBASE16(ifp,ofp)
	register FILE *ifp,*ofp;
{	char ibuf[64+2],obuf[32];
	register char *op;
	register int bytes,ci,c1,c2;

	bytes = 0;
	while( fgets(ibuf,sizeof(ibuf),ifp) != NULL ){
		if( ibuf[0] == '.' && ibuf[1] == '\n' )
			break;
		op = obuf;
		for( ci = 0; c1 = ibuf[ci]; ci += 2 ){
			if( c1 < 'A' || 'Z' < c1 )
				break;
			c1 -= 'A';
			c2 = ibuf[ci+1] - 'A';
			*op++ = c1 << 4 | c2;
			bytes++;
		}
		fwrite(obuf,ci/2,1,ofp);
	}
	fflush(ofp);
	return bytes;
}

/*//////////////////////////////////////////////////////////////////////*/

MIME_Decode(in,out,encode)
	FILE *in,*out;
	char *encode;
{
	if( strcasecmp(encode,"base64") == 0 )
		from64(in,out,NULL,0);
	else
	if( strcasecmp(encode,"quoted-printable") == 0 )
		fromqp(in,out,NULL,0);
	else{
		fprintf(out,"unknown encoding: %s\n",encode);
	}
	fflush(out);
}

/*//////////////////////////////////////////////////////////////////////*/

#include <ctype.h>

MMS_touppers(str) char *str; {
	char *cp;

	for( cp = str; *cp; cp++ )
		if( islower(*cp) )
			*cp = toupper(*cp);
}
MMS_tolowers(str) char *str; {
	char *cp;

	for( cp = str; *cp; cp++ )
		if( isupper(*cp) )
			*cp = tolower(*cp);
}
