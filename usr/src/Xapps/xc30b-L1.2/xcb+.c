/*	xcb+.c -- CIS B+ Protocol module for XC
	This file uses 4-character tabstops
 */

#ifdef DEBUG
#undef DEBUG
#endif
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include <time.h>
#include <fcntl.h>
#include <ctype.h>
#include "xc.h"

#define min(x,y)	((int)(x)<(int)(y)?(x):(y))
#define max(x,y)	((int)(x)>(int)(y)?(x):(y))
#define MaskLowRange 0x01
#define MaskHiRange 0x10

#define Check_B			0
#define Check_CRC		1
#define Send_Ahead_Buffers	5

#define Quote_Default		0
#define Quote_Not_NULL		1
#define Quote_Extended		2
#define Quote_Full		3
#define Quote_Mask		4

#define Overwrite	0
#define Resume		1

#define Resume_Allowed		0
#define Resume_Not_Allowed	1
#define Resume_Failed		2
#define Resume_Denied		3

extern short cr_add;
extern void cl_line();
static char S_Buffer[1033], R_Buffer[1033], tdir[32];
char   Name[SM_BUFF];
static unsigned char Mask[32];
static unsigned Checksum;
static int	Ch,				/* last char read from remote */
			Quoting,		/* quoting level requested by the user */
			Window_Size,	/* Send size of send ahead window */
			PackeT_Size,	/* Maximum block size. */
			R_BUffer_Len, S_Bytes, R_Bytes, Seq_Num, PendinG_Count,
			Next_Packet, Packets_Btwn_ACKs, Last_ACK, textmode, Last_Chr,
			Send_Errors, Read_Errors;
static short Max_Errors=10, Abort_Flag, Not_Masked, Sent_ENQ, Actual_Check,
			Valid_To_Resume_Download, ValiD_To_Resume_Upload,
			Send_FIle_Information, Packet_Received, Result;
static FILE *Data_File;
static long	already_have, data, total_read, total_sent,
			fsize, start, carriage_return;

typedef enum {
	S_Get_DLE,
	S_DLE_Seen,
	S_DLE_B_Seen,
	S_Get_Data,
	S_Get_check,
	S_Get_CRC,
	S_Verify_CRC,
	S_VErify_CKS,
	S_VerIfy_Packet,
	S_Send_NAK,
	S_SenD_ACK,
	S_SEnd_ENQ,
	S_Resend_Packets,
} Sender_Action;

typedef struct {
	int Seq;
	int PackeT_Size;
	char *packet;
} PEnding_Element;

PEnding_Element Pending[Send_Ahead_Buffers];

extern unsigned short crc_xmodem_tab[256];

static void init_check()
{
	Checksum=Actual_Check ? 0xffff : 0;
}

static void do_checksum(ch)
unsigned ch;
{
	if (Actual_Check==Check_B){
		Checksum<<=1;
		if (Checksum>255)
			Checksum=(Checksum&0xFF)+1;
		Checksum+=ch&0xFF;
		if (Checksum>255)
			Checksum=(Checksum&0xFF)+1;
	} else
	Checksum=(crc_xmodem_tab[((Checksum>>8)^ch)&0xff]^(Checksum<<8))&0xffff;
}

/* #define CIS_DEBUG /* for B+ record; use only on "fast" hardware */

#ifdef CIS_DEBUG
static FILE *bfp;
static void xclog(dir, val)
char	dir;
int	val;
{
	static int cnt, lastdir;

	if (bfp==NULLF)
		bfp=fopen("xc.log","w"),
		cnt=0,
		lastdir=dir;

	if (++cnt>20||lastdir!=dir)
		fprintf(bfp,"\n"),
		cnt=1;

	if (lastdir!=dir)
		fprintf(bfp,"\n");

	if (val>'~'||val<' ')
		fprintf(bfp,"%c%1x%1x ",dir,val/16,val%16);
	else
		fprintf(bfp,"%c%c  ",dir,val);

	lastdir=dir;
}

static void Why_NAK(reason)
char *reason;
{
	sprintf(Msg,"Sending NAK, %s",reason);
	show(0,Msg);
}

#else
#define xclog(dir,val)
#define Why_NAK(reason)
#endif

static void stats(count)
int count;
{
	int rate, minutes, sec, data_percent, rate_percent;
	long chars, elapsed, now, rem;

	data+=count;

	if (fsize==0)
		data_percent=0;
	else
		data_percent=100*(data+carriage_return)/fsize;

	if (data_percent>100)
		data_percent=100;

	time(&now);

	elapsed=now-start;
	chars=data+carriage_return-already_have-(tdir[0]=='T'?PackeT_Size-1:0);
	if (elapsed<5||chars==0)
		ttgoto(LI-6,26),
		fprintf(tfp,"estimating");
	else {
		rate=chars/elapsed;
		rem=(fsize-(data+carriage_return-already_have))/rate;
		minutes=rem/60;
		sec=rem%60;
		rate_percent=100*rate/mrate(NULLS);

		ttgoto(LI-6,26);
		fprintf(tfp,"%8.1d:%2.2d",minutes,sec);

		minutes=elapsed/60;
		sec=elapsed%60;
		ttgoto(LI-6,61);
		fprintf(tfp,"%8.1d:%2.2d",minutes,sec);

		ttgoto(LI-4,14);
		fprintf(tfp,"Rate is %d chars per second (%d %% of nominal bps) \n",
			rate,rate_percent);
/*
		ttgoto(LI-4,26);
		fprintf(tfp,"Rate is %d chars per second \n",
			rate,rate_percent);
*/
	}

	ttgoto(LI-8,0),
	fprintf(tfp,"%8.1ld",total_sent),
	ttgoto(LI-8,20),
	fprintf(tfp,"%8.1ld",total_read),
	ttgoto(LI-8,40);
	if (data_percent==0)
		fprintf(tfp,"%8.1ld",data);
	else
		fprintf(tfp,"%8.1ld %3.1u %%",data,data_percent);
	if (carriage_return)
		ttgoto(LI-8,60),
		fprintf(tfp,"%+7.1ld",carriage_return);
}

static void showmode()
{
	int l;
	sprintf(Msg,"%s %s (%ld bytes) as %s",tdir,Name,fsize,
		textmode?"ASCII":"BINARY");

	ttgoto(LI-12,0);
	cl_line();
	if ((l=strlen(Msg)) < CO)
		ttgoto(LI-12,(CO-l)/2 -1);
	fprintf(tfp,Msg);

	time(&start);
}

static void Discard_ACKed_Packets()
{
	int i, n;
	unsigned short Packet_Acked=FALSE;

	Last_ACK=Ch;
	n=(Next_Packet+PendinG_Count)%Send_Ahead_Buffers;

	for (i=PendinG_Count;i>0;i--){
		n--;
		if (n<0)
			n+=5;

		if (Pending[n].Seq==Ch-'0')
			Packet_Acked=TRUE,
			Next_Packet=(n+1)%Send_Ahead_Buffers;

		if (Packet_Acked==TRUE)
			free(Pending[n].packet),
			Pending[n].packet=NULLS,
			PendinG_Count--;
	}
}

static void Send_Byte(ch)
int ch;
{
	sendbyte(ch);
	total_sent++;
	xclog('>',ch);
}

static void Send_Masked_Byte(ch)
int ch;
{
	if (ch<0x20){
		if (Quoting==Quote_Full||(Mask[ch]&MaskLowRange))
			Send_Byte(DLE),
			ch+='@';
	} else if (ch>=0x80&&ch<0xA0&&
		(Quoting==Quote_Full||(Mask[ch-0x80]&MaskHiRange)))
			Send_Byte(DLE),
			ch=ch+'`'-0x80;

	Send_Byte(ch);
}

static Read_Byte()
{
	if ((Ch=readbyte(10))== -1)
		return FAILURE;
	total_read++;
	xclog('<',Ch);
	return SUCCESS;
}

static Read_Masked_Byte()
{
	Not_Masked=TRUE;

	if (!Read_Byte())
		return FAILURE;

	if (Ch==DLE){
		if (!Read_Byte())
			return FAILURE;

		Not_Masked=FALSE;

		if (Ch>='`')
			Ch+=0x80;

		Ch&=0x9F;
	}
	return SUCCESS;
}

static void Send_ACK()
{
	Send_Byte(DLE);
	Send_Byte(Seq_Num+'0');
}

static void Init()
{
	int i;

	R_BUffer_Len=Window_Size=PendinG_Count=Next_Packet=
		R_Bytes=S_Bytes=Seq_Num=Packets_Btwn_ACKs=Last_ACK=0;

	PackeT_Size=(10*mrate(NULLS)) > 1200 ? 1025 : 513;

	Quoting=Quote_Mask;

	for (i=0;i<Send_Ahead_Buffers;i++)
		Pending[i].packet=NULLS;

	Actual_Check=Check_B;
	Abort_Flag=Sent_ENQ=FALSE;

	memset(Mask,0,32);

	Mask[ETX]=Mask[ENQ]=Mask[DLE]=Mask[NAK]=Mask[XON]=Mask[XOFF]=MaskLowRange;

	total_sent=total_read=data=fsize=Read_Errors=Send_Errors=
		already_have=carriage_return=0;
	fprintf(tfp,"\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n");
	ttgoto(LI-14,25);
	show(1,"CIS B-Plus Protocol Transfer");

	ttgoto(LI-10,0);
	fprintf(tfp,"B+ Bytes Sent       B+ Bytes Rcvd");
	ttgoto(LI-10,40);
	fprintf(tfp,"Data Bytes          Carriage Returns");
	ttgoto(LI-6,10);
	fprintf(tfp,"Time Remaining:");
	ttgoto(LI-6,48);
	fprintf(tfp,"Elapsed Time:");
}

static void Xmit_Packet(Size,Seq,Packet)
int Size, Seq;
unsigned char *Packet;
{
	register I;

	init_check();
	Send_Byte(DLE);
	Send_Byte('B');
	Send_Byte(Seq+'0');
	do_checksum(Seq+'0');

	for (I=0;I<Size;I++)
		Send_Masked_Byte(Packet[I]),
		do_checksum(Packet[I]);

	Send_Byte(ETX);
	do_checksum(ETX);
	if (Actual_Check==Check_B)
		Send_Masked_Byte(Checksum);
	else
		Send_Masked_Byte(Checksum>>8),
		Send_Masked_Byte(Checksum&0xff);
}

static void Send_Failure(Code,Text)
char Code, *Text;
{
	int Len, Seq;

	S_Buffer[0]='F';
	S_Buffer[1]=Code;
	Len=2;
	while (*Text)
		S_Buffer[Len++]= *Text++;

	Seq=(Seq_Num+1)%10;

	while (PendinG_Count&&Wait_For_ACK(FALSE,FALSE,FALSE))
		;

	Xmit_Packet(Len,Seq,S_Buffer);

	do
		Wait_For_ACK(FALSE,FALSE,FALSE);
	while (Packet_Received);
}

static Flush_Pending()
{
	while (PendinG_Count)
		if (!Wait_For_ACK(FALSE,TRUE,TRUE))
			return FAILURE;

	return SUCCESS;
}

static Wait_For_ACK(Have_DLE_B,Acknowledge,Resend)
unsigned short Have_DLE_B, Acknowledge, Resend;
{
	Sender_Action Action;

	int i=0, n, RCV_Num, Errors=0;

	R_BUffer_Len=0;
	Packet_Received=FALSE;

	if (Have_DLE_B)
		Action=S_DLE_B_Seen;
	else
		Action=S_Get_DLE;

	while (Errors<Max_Errors)
		switch (Action){
		case S_Get_Data:
			if (Read_Masked_Byte()==FAILURE){
				Action=S_Send_NAK;
				Why_NAK("couldn't read next data byte");
			}
			else if (Not_Masked && Ch==ETX)
				Action=S_Get_check;
			else if (Not_Masked && Ch==ENQ)
				Action=S_SenD_ACK;
			else if (i==PackeT_Size){
				Action=S_Send_NAK;
				Why_NAK("incoming buffer overflow");
				} else
					R_Buffer[i++]=Ch,
					do_checksum(Ch);
			break;

		case S_Get_DLE:
			if (Packets_Btwn_ACKs>Window_Size+2&&PendinG_Count){
				Packets_Btwn_ACKs=0;
				Action=S_SEnd_ENQ;
				continue;
			}
			if (!Read_Byte())
				Action=S_SEnd_ENQ;
			else if (Ch==DLE)
				Action=S_DLE_Seen;
			else if (Ch==NAK)
				Action=S_SEnd_ENQ;
			else if (Ch==ENQ)
				Action=S_SenD_ACK;
			else if (Ch==ETX){
				Action=S_Send_NAK;
				Why_NAK("awaiting DLE, got ETX");
			}
			break;

		case S_DLE_Seen:
			if (!Read_Byte())
				Action=S_SEnd_ENQ;
			else if (Ch>='0'&&Ch<='9')
				if (Sent_ENQ&&Ch==Last_ACK){
					Sent_ENQ=FALSE;

					if (!PendinG_Count)
						return SUCCESS;
					else
						Action=S_Resend_Packets;
				} else {
					Discard_ACKed_Packets();
					if (Sent_ENQ)
						Action=S_Get_DLE;
					else
						return SUCCESS;
				}
			else if (Ch==';')
				Action=S_Get_DLE;
			else if (Ch=='B')
				Action=S_DLE_B_Seen;
			else if (Ch==ENQ)
				Action=S_SenD_ACK;
			else
				Action=S_Get_DLE;
			break;

		case S_DLE_B_Seen:
			if (!Read_Byte()){
				Action=S_Send_NAK;
				Why_NAK("no data byte after DLE-B");
			} else if (Ch==ENQ)
				Action=S_SenD_ACK;
			else
				init_check(),
				RCV_Num=Ch-'0',
				do_checksum(Ch),
				i=0,
				Action=S_Get_Data;
			break;

		case S_Get_check:
			do_checksum(ETX);

			if (Read_Masked_Byte()==FAILURE){
				Action=S_Send_NAK;
				Why_NAK("no incoming checksum");
			} else if (Not_Masked&&Ch==ENQ)
				Action=S_SenD_ACK;
			else if (Actual_Check==Check_CRC)
				Action=S_Get_CRC;
			else
				Action=S_VErify_CKS;
			break;

		case S_Get_CRC:
			do_checksum(Ch);

			if (Read_Masked_Byte()==FAILURE){
				Action=S_Send_NAK;
				Why_NAK("no incoming CRC value");
			} else if (Not_Masked&&Ch==ENQ)
				Action=S_SenD_ACK;
			else
				Action=S_Verify_CRC;
			break;

		case S_Verify_CRC:
			do_checksum(Ch);

			if (Checksum==0)
				Action=S_VerIfy_Packet;
			else {
				Action=S_Send_NAK;
				Why_NAK("CRC error");
			}
			break;

		case S_VErify_CKS:
			if (Checksum==Ch)
				Action=S_VerIfy_Packet;
			else {
				Action=S_Send_NAK;
				Why_NAK("Checksum error");
			}
			break;

		case S_VerIfy_Packet:
			if (RCV_Num==((Seq_Num+1)%10)||R_Buffer[0]=='F'){
				Packets_Btwn_ACKs++;
				Seq_Num=RCV_Num;
				if (Acknowledge)
					Send_ACK();
				R_BUffer_Len=i;
				Packet_Received=TRUE;
				return FAILURE;
			} else if (RCV_Num==Seq_Num)
				Action=S_SenD_ACK;
			else {
				Action=S_Send_NAK;
				Why_NAK("packet out of sequence");
			}
			break;

		case S_Send_NAK:
			ttgoto(LI-2,20);
			sprintf(Msg,"Read Errors: %2.1d",++Read_Errors);
			S;
			Errors++;
			Send_Byte(NAK);
			Action=S_Get_DLE;
			break;

		case S_SenD_ACK:
			Send_ACK();
			Action=S_Get_DLE;
			break;

		case S_SEnd_ENQ:
			ttgoto(LI-2,40);
			sprintf(Msg,"Send Errors: %2.1d",++Send_Errors);
			S;
			Errors++;
			Sent_ENQ=TRUE;
			Send_Byte(ENQ);
			Send_Byte(ENQ);
			Action=S_Get_DLE;
			break;

		case S_Resend_Packets:
			if (Resend){
				for (i=0;i<PendinG_Count;i++)
					n=(Next_Packet+i)%Send_Ahead_Buffers,
					Xmit_Packet(
						Pending[n].PackeT_Size,
						Pending[n].Seq,
						Pending[n].packet);
			}else
				return FAILURE;

			Action=S_Get_DLE;
			break;
		}
	return FAILURE;
}

static void Send_Abort()
{
	fclose(Data_File);
	sprintf(Msg,"Transfer abort requested");
	show(0,Msg);
	Send_Failure('A',Msg);
}

static Send_Packet(Size)
int Size;
{
	register i;
	int  Next, Next_Seq;
	char *ptr;

	while ((PendinG_Count>Window_Size))
		if (!Wait_For_ACK(FALSE,TRUE,TRUE)){
			Send_Abort();
			return FAILURE;
		}

	if ((ptr=(char *)malloc(Size))==NULLS){
		Send_Abort();
		return FAILURE;
	}

	Next=(Next_Packet+PendinG_Count)%Send_Ahead_Buffers;
	PendinG_Count++;

	Next_Seq=Seq_Num=(Seq_Num+1)%10;
	Pending[Next].Seq=Next_Seq;
	Pending[Next].packet=ptr;
	Pending[Next].PackeT_Size=Size;
	Packets_Btwn_ACKs=0;

	Xmit_Packet(Size,Next_Seq,S_Buffer);

	for (i=0;i<Size;i++)
		ptr[i]=S_Buffer[i];

	return SUCCESS;
}

static unsigned long cnvAtoL(ptr)
char *ptr;
{
	unsigned short sign=FALSE;
	char ch;
	unsigned long result=0;

	ch= *ptr++;

	if (ch=='-')
		sign=TRUE,
		ch= *ptr++;

	while (ch>='0'&&ch<='9')
		result=result*10+(ch-'0'),
		ch= *ptr++;

	return(sign?-result:result);
}

static char *cnvLtoA(ptr,n)
char *ptr;
unsigned long n;
{
	char tmp1[11], *tmp2=tmp1;

	if (n==0){
		*ptr++ ='0';
		return ptr;
	}

	*tmp2++ =0;
	do {
		*tmp2++ =((char)(n%10))+'0';
		n/=10;
	} while (n>0);

	tmp2--;
	while (*tmp2)
		*ptr++ = *tmp2--;

	return ptr;
}

static void Send_Unexpected_Packet()
{
	sprintf(Msg,"Unexpected packet type");
	show(0,Msg);
	Send_Failure('N',Msg);
}

FILE *QueryCreate(Offer_Resume)
short Offer_Resume;
{
	int key;
	short Condition;
	FILE *fileptr;

	Condition = isregfile(Name) ? Offer_Resume : Resume_Denied;

	if (access(Name,0)&&(fileptr=fopen(Name,"w"))){
		Result=Overwrite;
		return fileptr;
	} else if (access(Name,2))
		Condition = Resume_Denied;

	switch(Condition){
	case Resume_Allowed:
		sprintf(Msg,"'%s' exists; Overwrite, Resume, reName, or Abort?",Name);
		break;

	case Resume_Not_Allowed:
		sprintf(Msg,"'%s' exists; Overwrite, reName, or Abort?",Name);
		break;

	case Resume_Failed:
		sprintf(Msg,"'%s' CRC error; Overwrite, reName or Abort?",Name);
		break;

	case Resume_Denied:
		sprintf(Msg,"Permission denied for '%s'; reName, or Abort?",Name);
		break;
	}
	show(0,Msg);

	for (;;){
		beep();
		key=toupper(dgetch());
		if (isupper(key)){
			fputc(key,tfp);

			switch(key){
			case 'O':
				if (Condition!=Resume_Denied){
					Result=Overwrite;
					return fopen(Name,"w");
				}
				break;

			case 'N':
				fputc('\r',tfp);
				cl_line();
				show(-1,"Enter New Name:");
				getline();
				getword();
				strcpy(Name,word);
				return QueryCreate(Offer_Resume);

			case 'A':
				return NULLF;

			case 'R':
				if (Condition==Resume_Allowed){
					Result=Resume;
					return fopen(Name,"r+");
				}
				break;
			}
			fputc('\b',tfp);
		}
	}
}

static Receive_File()
{
	char *ptr;
	int N, i;
	short Request_Resume;

	Result=Overwrite;
	if (Valid_To_Resume_Download==2)
		Request_Resume=Resume_Allowed;
	else
		Request_Resume=Resume_Not_Allowed;

	if ((Data_File=QueryCreate(Request_Resume))==NULLF){
		Send_Abort();
		return FAILURE;
	}

	chown(Name,getuid(),getgid());

	if (Result==Resume){
		strcpy(tdir,"Attempting receive resume of");
		showmode();

		init_check();

		do {
			S_Buffer[0]='N';
			N=Read(Data_File,&S_Buffer[0],PackeT_Size-1);

			if (N>0){
				for (i=0;i<N;i++)
					do_checksum(S_Buffer[i]);
				if (Abort_Flag){
					Send_Abort();
					return FAILURE;
				}

				already_have+=N;
			}
		} while (N>0);

		ptr= &S_Buffer[0];

		*ptr++ ='T';
		*ptr++ ='r';

		ptr=cnvLtoA(ptr,(unsigned long)already_have);
		*ptr++ =' ';
		ptr=cnvLtoA(ptr,(unsigned long)Checksum);

		if (!Send_Packet(ptr- &S_Buffer[0])||!Flush_Pending()){
			fclose(Data_File);
			show(0,"Can't resume transfer");
			return FAILURE;
		}

		fseek(Data_File,(long)0,2);

		strcpy(tdir,"Resuming receive of");
		data=already_have-carriage_return;
		carriage_return= -carriage_return;
		showmode();
	} else
		Send_ACK(),
		strcpy(tdir,"Receiving"),
		already_have=0;

	for (;;){
		if (Abort_Flag){
			Send_Abort();
			return FAILURE;
		}

		Wait_For_ACK(FALSE,TRUE,TRUE);

		if (Packet_Received)
			switch(R_Buffer[0]){
			case 'N':
				if ((N=Write(Data_File,&R_Buffer[1],R_BUffer_Len-1))== -1){
					sprintf(Msg,"Disk write error");
					show(0,Msg);
					Send_Failure('I',Msg);
					fclose(Data_File);
					return FAILURE;
				}

				stats(N);
				break;

			case 'T':
				switch(R_Buffer[1]){
				case 'I':
					fsize=cnvAtoL(&R_Buffer[4]);
					showmode();
					break;

				case 'C':
					fclose(Data_File);
					return SUCCESS;

				case 'f':
					fclose(Data_File);

					if ((Data_File=QueryCreate(Resume_Failed))==NULLF){
						Send_Abort();
						return FAILURE;
					}

					chown(Name,getuid(),getgid());
					strcpy(tdir,"Receiving");
					data=already_have=carriage_return=0;
					showmode();
					break;

				default:
					Send_Unexpected_Packet();
					fclose(Data_File);
					return FAILURE;
				}
				break;

			case 'F':
				fclose(Data_File);
				R_Buffer[R_BUffer_Len]=0;

				if (Result==Resume)
					sprintf(Msg,"Can't resume transfer: %s",&R_Buffer[3]);
				else
					sprintf(Msg,"B protocol Failure: %s",&R_Buffer[3]);

				show(0,Msg);
				return FAILURE;

			default:
				Send_Unexpected_Packet();
				fclose(Data_File);
				return FAILURE;
			}
		else {
			fclose(Data_File);
			return FAILURE;
		}
	}
}

static char *Handle_Send_Failure()
{
	if (R_BUffer_Len==0)
		return("Remote is not responding");
	else {
		if (R_Buffer[0]=='F'){
			if (R_BUffer_Len>=2){
					R_Buffer[min(81,R_BUffer_Len)]='\0';
					return(&R_Buffer[1]);
				} else
					return("No reason given by remote");
		} else {
			Send_Failure('E',"Unexpected packet type");
			return("Unexpected packet type");
		}
	}
}

static Send_File()
{
	int N;
	struct stat statbuf;

	if ((Data_File=fopen(Name,"r"))==NULLF){
		sprintf(Msg,"Can't access '%s'",Name);
		show(0,Msg);
		Send_Failure('M',Msg);
		return FAILURE;
	}

	fstat(fileno(Data_File),&statbuf);
	fsize=statbuf.st_size;

	strcpy(tdir,"Transmitting");
	showmode();

	do {
		S_Buffer[0]='N';
		N=Read(Data_File,&S_Buffer[1],PackeT_Size-1);

		if (N>0){
			if (!Send_Packet(N+1)){
				fclose(Data_File);
				show(0,Handle_Send_Failure());
				return FAILURE;
			}

			if (Abort_Flag){
				Send_Abort();
				return FAILURE;
			}

			stats(N);
		}
	} while (N>0);

	if (N==0){
		fclose(Data_File);
		S_Buffer[0]='T';
		S_Buffer[1]='C';

		if (!Send_Packet(2)){
			show(0,Handle_Send_Failure());
			return FAILURE;
		}

		return Flush_Pending();
	} else {
		sprintf(Msg,"Disk read error");
		show(0,Msg);
		Send_Failure('I',Msg);
		return FAILURE;
	}
}

static Do_Transfer()
{
	int I, N;
	short Have_DLE_B=TRUE;

	for (;;){
		Wait_For_ACK(Have_DLE_B,FALSE,TRUE);
		if (Packet_Received){
			if (R_Buffer[0]=='T'){
				if (R_Buffer[1]!='D'&&R_Buffer[1]!='U'){
					show(0,"Invalid transfer direction");
					Send_Failure('N',"Not implemented");
					return FAILURE;
				}
				if (R_Buffer[2]!='A'&&R_Buffer[2]!='B'){
					show(0,"Invalid transfer type");
					Send_Failure('N',"Not implemented");
					return FAILURE;
				}
				N=min(R_BUffer_Len-3,SM_BUFF-1);
				for (I=0;I<N;I++)
					Name[I]=R_Buffer[I+3];
				Name[I]='\0';
				textmode=(R_Buffer[2]=='A');

				if (R_Buffer[1]=='U') {
					Send_ACK();
					return Send_File();
				} else
					return Receive_File();

			} else if (R_Buffer[0]=='+'){
				if (Plus_Respond())
					Have_DLE_B=FALSE;
				else {
					show(0,"Could not negotiate B-Plus parameters");
					return FAILURE;
				}
			} else {
				Send_Unexpected_Packet();
				return FAILURE;
			}
		} else {
			show(0,"Remote is not responding");
			return FAILURE;
		}
	}
}

#define Plus_PackeT_Size	18
#define LowRange		7
#define HiRange			11

#define My_Send_Window_Size	1
#define My_Recv_Window_Size	1
#define My_Buffer_Size		8
#define My_Check_Method		Check_CRC
#define My_Download_Resume	2
#define My_Upload_Resume	0
#define My_File_Information	1

static char Quote_Level_Select_Low[]={
	1, 3, 3, 0, 3, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	0, 0, 3, 0, 3, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3
};


static char QuotE_Level_select_Hi[]={
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 2, 3, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3
};

static char QUote_Level_Mapping[]={
	Quote_Not_NULL,
	Quote_Default,
	Quote_Extended,
	Quote_Full
};

static Plus_Respond()
{
	int Status, temp_window_size, temp_method, temp_size, MaskByte, Bit, i;
	char Estimated_Quote_Level=0;

	S_Buffer[0]='+';
	S_Buffer[1]=My_Send_Window_Size;
	S_Buffer[2]=My_Recv_Window_Size;

	S_Buffer[3]=PackeT_Size/128;

	S_Buffer[4]=My_Check_Method;

	S_Buffer[5]=Quote_Default;
	S_Buffer[6]=FALSE;

	S_Buffer[15]=My_Download_Resume;
	S_Buffer[16]=My_Upload_Resume;
	S_Buffer[17]=My_File_Information;

	for (i=0;i<8;i++)
		S_Buffer[i+LowRange]=0;

	for (MaskByte=0;MaskByte<4;MaskByte++)
		for (Bit=0;Bit<8;Bit++){
			if (Mask[MaskByte*8+Bit]&MaskLowRange)
				S_Buffer[MaskByte+LowRange]|=0x80>>Bit;

			if (Mask[MaskByte*8+Bit]&MaskHiRange)
				S_Buffer[MaskByte+HiRange]|=0x80>>Bit;
		}

	for (i=R_BUffer_Len;i<Plus_PackeT_Size;i++)
		R_Buffer[i]=0;

	if (R_Buffer[3]<S_Buffer[3])
		temp_size=(R_Buffer[3]*128)+1;
	else
		temp_size=(S_Buffer[3]*128)+1;


	temp_window_size=min(R_Buffer[2],My_Send_Window_Size);
	temp_method=min(R_Buffer[4],My_Check_Method);
	Valid_To_Resume_Download=min(R_Buffer[15],My_Download_Resume);
	ValiD_To_Resume_Upload=min(R_Buffer[16],My_Upload_Resume);
	Send_FIle_Information=min(R_Buffer[17],My_File_Information);

	if (R_BUffer_Len>=Plus_PackeT_Size)
		for (MaskByte=0;MaskByte<4;MaskByte++)
			for (Bit=0;Bit<8;Bit++) {
				if (R_Buffer[LowRange+MaskByte]&(0x80>>Bit))
					Mask[MaskByte*8+Bit]|=MaskLowRange;

				if (R_Buffer[HiRange+MaskByte]&(0x80>>Bit))
					Mask[MaskByte*8+Bit]|=MaskHiRange;
			}
	else {
		for (i=0;i<32&&Estimated_Quote_Level<3;i++){
			if (Mask[i]&MaskLowRange)
				Estimated_Quote_Level=
					max(Quote_Level_Select_Low[i],Estimated_Quote_Level);

			if (Mask[i]&MaskHiRange)
				Estimated_Quote_Level=
					max(QuotE_Level_select_Hi[i],Estimated_Quote_Level);
		}
	}

	Quoting=Quote_Full;
	S_Buffer[5]=QUote_Level_Mapping[Estimated_Quote_Level];

	if (Status=Send_Packet(Plus_PackeT_Size))
		if (Status=Flush_Pending()){
			Actual_Check=temp_method;
			PackeT_Size=temp_size;
			Window_Size=temp_window_size;
		}
	Quoting=Quote_Mask;

	return Status;
}

static Read(fp,buf,want)
FILE *fp;
char *buf;
register want;
{
	register c;
	int read=0;

	while (want--)
		switch(c=getc(fp)){
		case EOF:
			return read;

		case '\n':
			if (cr_add&&textmode&&Last_Chr!='\r')
				ungetc(c,fp),
				carriage_return++,
				c='\r';

		default:
			Last_Chr= *buf++ =c;
			read++;
		}

	return read;
}

static Write(fp,buf,want)
FILE *fp;
char *buf;
register want;
{
	int written=0;

	for (;want-->0;buf++){
		if (textmode){
			if (*buf=='\r'){
				Last_Chr= *buf;
				continue;
			}
			if (Last_Chr=='\r')
				if (*buf=='\n')
					carriage_return--;
				else
					if (fputc('\r',fp)== -1)
						return -1;
					else
						written++;

			Last_Chr= *buf;
		}

	if (fputc(*buf,fp)== -1)
		return -1;
	else
		written++;
	}

	return written;
}

static void cisbsigint()
{
	signal(SIGINT,cisbsigint);
	Abort_Flag=TRUE;
}

void B_Transfer()
{
	short Status=FALSE;
	cur_off();
	xc_setflow(FALSE);
	intdel(TRUE);
	signal(SIGINT,cisbsigint);
	Init();
	Send_Byte(DLE);
	Send_Byte('+');
	Send_Byte('+');
	Send_ACK();

	Read_Byte();
	switch(Ch){
	case DLE:
		Read_Byte();
		if (Ch=='B')
			Status=Do_Transfer();
		break;

	default:
		fputc(Ch,tfp);
		break;
	}

	sprintf(Msg,"File Transfer %s",Status?"Succeeded":"Failed");
	show(0,Msg);
	beep();

	if (Abort_Flag){
		while (Read_Byte() && Ch==ENQ){
			Seq_Num=0;
			Send_Byte(DLE);
			Send_Byte('+');
			Send_Byte('+');
			Send_ACK();
		}
	}

	intdel(FALSE);
	xc_setflow(flowflag);
	cur_on();
	signal(SIGINT,SIG_IGN);
}
