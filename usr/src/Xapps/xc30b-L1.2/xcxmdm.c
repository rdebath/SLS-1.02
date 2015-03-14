
/**************************************************************************/
/*  this is the KI4N  02 Apr 92 version of xcxmdm.c
    fixes include correct updating of crc in send mode, and
    better handshaking during protocol negotiations.
    additions include 1K XMODEM (old YMODEM) protocol
    the crc table has been moved to xcrctbl.h   */


/*	xcxmdm.c -- XMODEM Protocol module for XC
	This file uses 4-character tabstops
*/

char 
syncem(void);

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include <setjmp.h>
#include "xc.h"
#include "xcrctbl.h"

#define FALLBACK  16
#define CPMEOF	032
#define WANTCRC 'C'
#define OK		 0
#define TIMEOUT	-1
#define ERROR	-2
#define WCEOT	-3
#define RETRYMAX 10
#define SECSIZ	1024
#define Resume_Not_Allowed	1
#define RUpdcrc(c,crc) (crc_xmodem_tab[((crc>>8)&0xff)]^(crc<<8)^c)&0xffff
#define TUpdcrc(c,crc) (crc_xmodem_tab[((crc>>8)^c)&0xff]^(crc<<8))

extern short badline;

static FILE *xfp;		/* buffered file pointer        */
static short firstsec,		/* first packet of file or not? */
 ksecsize,			/* for xmodem or ymodem packet  */
 stx_soh,			/* STX = ymodem ; SOH = xmodem  */
 amdone = FALSE,		/* flag: xfer almost finished   */
 sync,				/* flag for negotiating reception crc|chk */
 textmode = FALSE;		/* Text translations enabled?   */

static short crcheck = TRUE;

static char wcbuff[SECSIZ];	/* Ward Christensen packet buffer */
static char *p, sendchar;

static 
wcrx(), wctx(), putsec(), wcputsec(), getsec(), wcgetsec(), setmode();
static jmp_buf our_env;

/*	send 10 CAN's to try to get the other end to shut up */
static void 
canit()
{
	int i;

	for (i = 0; i < 20; i++)
		sendbyte(CAN);

}

static void 
xmsigint()
{				/* Our SIGINT handler */
	show_abort();
	signal(SIGINT, SIG_IGN);/* Ignore subsequent DEL's */
	canit();		/* Abort the transmission */
	longjmp(our_env, 1);
}

void 
xreceive(c)
	int c;
{
	xc_setflow(FALSE);
	signal(SIGINT, xmsigint);

	if (setmode(c)) {
		if (setjmp(our_env) == 0) {

			sprintf(Msg, "Ready to receive single file %s", word);
			S;
			if (wcrx() == ERROR)
				canit();
			return;
		}
	}

	signal(SIGINT, SIG_IGN);
	intdel(FALSE);
	xc_setflow(flowflag);
}

void 
xsend(c)
	int c;
{
	xc_setflow(FALSE);
	signal(SIGINT, xmsigint);

	if (setmode(c)) {
		if (setjmp(our_env) == 0) {
			if (wctx() == ERROR) {
				sprintf(Msg, "Error transmitting file %s", word);
				S;
				return;
			}
		}
	}

	signal(SIGINT, SIG_IGN);
	intdel(FALSE);
	xc_setflow(flowflag);
}

/* Receive a file using XMODEM protocol */
static 
wcrx()
{
	register sectnum, sectcurr;

	strcpy(Name, word);
	if ((xfp = QueryCreate(Resume_Not_Allowed)) == NULLF)
		return (ERROR);

	firstsec = TRUE;
	sectnum = 0;
	sendchar = WANTCRC;
	sync = FALSE;
	crcheck = TRUE;

	show(2, "Sync...");

	while (TRUE) {
		if (badline)
			purge();
		if (sync)
			sendbyte(sendchar);
		sectcurr = wcgetsec(6);
		if (sectcurr == ((sectnum + 1) & 0xff)) {
			sectnum++;
			putsec();
			fprintf(tfp, "Received packet #%d\r", sectnum);
			sendchar = ACK;
			continue;
		}

		if (sectcurr == (sectnum & 0xff)) {
			sprintf(Msg, "Received duplicate packet #%d", sectnum);
			S2;
			sendchar = ACK;
			continue;
		}

		fclose(xfp);

		if (sectcurr == WCEOT) {
			show(1, "File received OK");
			sendbyte(ACK);
			return (OK);
		}

		if (sectcurr == ERROR)
			return (ERROR);


		sprintf(Msg, "Sync error ... expected %d(%d),got %d",
			(sectnum + 1) & 0xff, sectnum, sectcurr);
		S;
		return (ERROR);
	}
}

/* Transmit a file using XMODEM protocol */
static 
wctx()
{
	register sectnum, eoflg;
	int tmpsize, tmpbdln, attempts, c, terr;

	if ((xfp = fopen(word, "r")) == NULLF) {
		sprintf(Msg, "Can't open `%s' for input", word);
		S;
		return (ERROR);
	}
	firstsec = TRUE;
	amdone = FALSE;
	tmpsize = ksecsize;	/* saves sector size for later restoration */
    tmpbdln = badline;  /* save state of badline flag */
	attempts = 0;
	show(1, "Sync...");

	while ((c = readbyte(30)) != NAK && c != WANTCRC && c != CAN)
		if (c == TIMEOUT && ++attempts > RETRYMAX) {
			show(1, "Receiver not responding");
			fclose(xfp);
			return (ERROR);
		}
	if (c == CAN) {
		show(1, "Receiver CANcelled");
		fclose(xfp);
		return (ERROR);
	}
	if ((crcheck = (c == WANTCRC)) == FALSE)
		show(1, "ATTENTION: CHECKSUM packet validation requested");
	else
		show(1, "CRC packet validation enabled ");
	sectnum = 1;

	do {
		eoflg = getsec();
		fprintf(tfp, "Transmitting packet #%d\r", sectnum);

		if ((terr = wcputsec(sectnum)) == ERROR) {
			fclose(xfp);
			return (ERROR);
		}
		else if (terr == FALLBACK) {
			if (stx_soh == STX) {
				amdone = TRUE;
			}
			else
				badline = TRUE;
			sectnum--;
		}
		sectnum++;
	} while (eoflg);

	ksecsize = tmpsize;
    badline = tmpbdln ;
	fclose(xfp);
	attempts = 0;
	sendbyte(EOT);
	while (readbyte(5) != ACK && attempts++ < RETRYMAX)
		sendbyte(EOT);
	if (attempts >= RETRYMAX) {
		show(1, "Receiver not responding to completion");
		return (ERROR);
	}

	show(1, "Transmission complete");
	return (OK);
}

/*	wcgetsec() inputs an XMODEM packet.
	This routine returns the packet number encountered, or ERROR if a valid
	packet is not received or CAN received; or WCEOT if EOT packet.
  
	Maxtime is the timeout for the first character, set to 6 seconds for
	retries. No ACK is sent if the packet is received ok. This must be
	done by the caller when it is ready to receive the next packet.
*/
static 
wcgetsec(maxtime)
	unsigned maxtime;

{
	register unsigned oldcrc;
	register checksum, j, c;
	int sectcurr, sectcomp, attempts;

	if (sync == FALSE) {	/* negotiate crc or checksum error detection */
		c = syncem();
		if (crcheck == FALSE)
			show(1, "ATTENTION: sender demands CHECKSUM error detection ");
		else
			show(1, "CRC packet validation enabled ");

		if (c == SOH)
			show(1, "Sender using 128 byte packets ");

		if (c == STX)
			show(1, "Sender using 1K byte packets ");

	}
	for (attempts = 0; attempts < RETRYMAX; attempts++) {
		if (sync == TRUE) {
			do {
				c = readbyte(maxtime);
			} while (c != STX && c != SOH && c != EOT && c != CAN && c != TIMEOUT);
		}
		else
			sync = TRUE;

		switch (c) {
		case STX:
		case SOH:
			if (c == SOH)
				ksecsize = 128;	/* xmodem packet size  */
			else
				ksecsize = 1024;	/* ymodem packet size  */
			sectcurr = readbyte(3);
			sectcomp = readbyte(3);
			if ((sectcurr + sectcomp) == 0xff) {
				oldcrc = checksum = 0;
				for (j = 0; j < ksecsize; j++) {
					if ((c = readbyte(3)) == TIMEOUT)
						goto timeout;
					wcbuff[j] = c;
					oldcrc = RUpdcrc(c, oldcrc);
					checksum += c;
				}
				if ((c = readbyte(3)) == TIMEOUT)
					goto timeout;
				if (crcheck) {
					oldcrc = RUpdcrc(c, oldcrc);
					if ((c = readbyte(3)) == TIMEOUT)
						goto timeout;
					if (RUpdcrc(c, oldcrc)) {
						show(2, "CRC error");
						break;
					}
				}
				else if (((checksum - c) & 0xff) != 0) {
					show(2, "Checksum error");
					break;
				}
				firstsec = FALSE;
				return (sectcurr);
			}
			else
				sprintf(Msg, "Packet number garbled 0%03d 0%03d",
					sectcurr, sectcomp);
			S2;
			break;

		case EOT:
			if (readbyte(3) == TIMEOUT)
				return (WCEOT);
			break;
		case CAN:
			show(2, "Sender CANcelled");
			return (ERROR);
		case TIMEOUT:
	timeout:
			show(2, "Timeout");
			break;
		}
		show(2, "Trying again on this packet");
		purge();
		maxtime = 6;
		sendbyte(NAK);

	}
	show(2, "Retry count exceeded");
	canit();
	return (ERROR);
}

/*	wcputsec outputs a Ward Christensen type packet.
	it returns OK or ERROR
*/

static 
wcputsec(sectnum)
	int sectnum;
{
	register short oldcrc;
	register short checksum, j, c, attempts, errct = 0;

	oldcrc = checksum = 0;
	for (j = 0; j < ksecsize; j++) {
		c = wcbuff[j];
		oldcrc = TUpdcrc(c, oldcrc);
		checksum += c;
	}

	for (attempts = 0; attempts < RETRYMAX; attempts++) {
		sendbyte(stx_soh);
		sendbyte(sectnum);
		sendbyte(-sectnum - 1);
		for (j = 0; j < ksecsize; j++)
			sendbyte(wcbuff[j]);
		if (firstsec == TRUE || badline == TRUE)
			purge();
		if (crcheck) {
			sendbyte((int) (oldcrc >> 8));
			sendbyte((int) oldcrc);
		}
		else
			sendbyte(checksum);

		c = readbyte(10);

		switch (c) {
		case CAN:
			show(2, "Receiver CANcelled");
			return (ERROR);
		case ACK:
			firstsec = FALSE;
			return (OK);
		case NAK:
			show(2, "Got a NAK on packet acknowledge");
			break;
		case TIMEOUT:
			show(2, "Timeout on packet acknowledge");
			break;
		default:
			sprintf(Msg, "Got %#x for packet acknowledge", c);
			S2;
			do {
				if ((c = readbyte(3)) == CAN) {
					show(2, "Receiver CANcelled");
					return (ERROR);
				}
			} while (c != TIMEOUT);

			break;
		}
		errct++;
		if ((errct == 3) && (badline == FALSE || stx_soh == STX)) {
			show(2, "ATTEMPTING RECOVERY ");
			return (FALLBACK);
		}
	}
	show(2, "Retry count exceeded");
	return (ERROR);
}


/* parse the the type of transfer requested */

static 
setmode(c)
	int c;
{
	char *xfrtype;

	intdel(TRUE);

	switch (tolower(c)) {
	case 'x':
		textmode = FALSE;	/* the usual case */
	case 't':
		ksecsize = 128;	/* default for xmodem */
		stx_soh = SOH;	/* default for xmodem */
		if (c == 't')
			textmode = TRUE;
		else
			xfrtype = "128 byte small packet";
		break;
	case 'o':
		xfrtype = "1K byte packet (old YMODEM)";
		textmode = FALSE;
		ksecsize = 1024;/* old ymodem sector size */
		stx_soh = STX;	/* transmit flag for 1k byte packet size */
		break;
	case ' ':
		break;

	default:
		return FAILURE;
	}

	sprintf(Msg, "XMODEM %s file transfer mode", textmode ? "Text" : xfrtype);
	S;

	return SUCCESS;
}

/*	fill the CP/M packet buffer from the UNIX file
	do text adjustments if necessary
	return 1 if more packets are to be read, or 0 if this is the last
*/

static 
getsec()
{
	int i;
	static int j, fbinit, fback;
	register c;

	if (amdone == FALSE) {
		i = 0;
		j = 8;
		fbinit = TRUE;
		fback = TRUE;
		while (i < ksecsize && (c = getc(xfp)) != EOF) {
			if (textmode && c == '\n') {
				wcbuff[i++] = '\r';
				if (i >= ksecsize) {	/* handle a newline on
							 * the last byte */
					ungetc(c, xfp);	/* of the sector	                 */
					return (1);
				}
			}
			wcbuff[i++] = c;
		}
		/* make sure that an extra blank sector is not sent */
		if (c != EOF && (c = getc(xfp)) != EOF) {
			ungetc(c, xfp);
			return (1);
		}

		/*
		 * fill up the last sector with ^Z's if text mode or 0's if
		 * binary mode 
		 */

		j = i;
		while (i < ksecsize)
			wcbuff[i++] = '\0';
		textmode ? CPMEOF : '\0';
		amdone = TRUE;
		fback = FALSE;


		if (stx_soh == SOH || j > 896)	/* done, send whole sector */
			return (0);

		j += 127;
		j >>= 7;	/* calculate number of 128 bytes sectors to
				 * send */
		fbinit = TRUE;

	}			/* end of "amdone" */

	if (fbinit) {
		p = wcbuff;	/* save old buffer base address */
		p -= 128;
		stx_soh = SOH;	/* switch to 128 byte blocks    */
		ksecsize = 128;
		fbinit = FALSE;

	}			/* end of "fbinit" */

	p += 128;
	memcpy(wcbuff, p, 128);

	if (--j > 0)		/* more packets remain */
		return (1);
	else {
		if (fback) {
			amdone = FALSE;
			return (1);
		}
		else
			return (0);
	}


}				/* end of getsec */



/*	Put received WC packet into a UNIX file
	using text translations if neccesary.
*/

static 
putsec()
{
	int i;
	register c;

	for (i = 0; i < ksecsize; i++) {
		c = wcbuff[i];
		if (textmode) {
			if (c == CPMEOF)
				return;
			if (c == '\r')
				continue;
		}
		fputc(c, xfp);
	}
	return (0);

}

/* automatically negotiate crc or checksum reception */

char 
syncem(void)
{
	int ct = 2;
	char c;

	purge();

	while (ct) {		/* try to negotiate a CRC transfer */
		sendbyte('C');
		c = readbyte(10);
		if (c != CAN && c != SOH && c != STX)
			ct--;
		else {
			sendchar = ACK;
			crcheck = TRUE;
			return (c);
		}
	}
	ct = 2;
	purge();
	while (ct) {		/* maybe we can initiate a checksum transfer */
		sendbyte(NAK);
		c = readbyte(10);
		if (c != CAN && c != SOH && c != STX)
			ct--;
		else {
			sendchar = ACK;
			crcheck = FALSE;
			return (c);
		}
	}
	purge()
		return (TIMEOUT);

}
