/*
  c2proto.h

  (c) Copyright 1991 by David M. Siegel.
      All rights reserved.

  %W% %G% %U%
*/

#ifndef in_libfax_c2proto_h
#define in_libfax_c2proto_h 1

#define TRUE  1
#define FALSE 0

#define ETX  '\003'
#define DLE  '\020'
#define XON  '\021'	/* DC1 */
#define XOFF '\023'	/* DC3 */
#define CAN  '\030'

/*
 * Various timeouts are defined here, in seconds:
 */
#define TIMEOUT_CONNECT	    60	/* wait for a connection		*/
#define TIMEOUT_HANGUP	    5	/* wait for hangup to complete		*/
#define TIMEOUT_ANSWER	    15	/* wait for phone to be answered	*/
#define TIMEOUT_END_PAGE    120	/* wait for end of page to be acked	*/
#define TIMEOUT_END_XMIT    120	/* wait for end of xmit to be acked	*/
#define TIMEOUT_BIT_REVERSE 5	/* wait for bit reverse to be acked	*/
#define TIMEOUT_BYTE_ALIGN  5   /* wait for byte align to be acked	*/
#define TIMEOUT_SET_CAPABILITIES  5 /* wait for set capabilities ack	*/
#define TIMEOUT_SEND_PAGE   60	/* final send of page timeout		*/
#define TIMEOUT_RECV_PAGE   60	/* final recv page ack			*/

/* 
 * Standard Hayes-style modem result codes:
 */
#define RESULT_OK      		0
#define RESULT_CONNECT 		1
#define RESULT_RING    		2
#define RESULT_NO_CARRIER 	3
#define RESULT_ERROR 		4
#define RESULT_NO_DIALTONE 	6
#define RESULT_BUSY		7	
#define RESULT_NO_ANSWER 	8

/*
 * Post page response codes:
 */
#define PPR_MCF		1	/* page good				*/
#define PPR_RTN		2	/* page bad; retrans requested		*/
#define PPR_RTP		3	/* page good; retrans requested		*/
#define PPR_PIN		4	/* page bad; interrupt requested       	*/
#define PPR_PIP		5	/* page good; interrupt requested	*/

/* vertical resolution */
#define VR_NORMAL	0
#define VR_FINE		1

/* baud rate */
#define BR_2400    	0
#define BR_4800    	1
#define BR_7200    	2
#define BR_9600    	3

/* width */
#define WD_1728		0
#define WD_2048		1
#define WD_2432		2

/* page length */
#define LN_A4		0
#define LN_B4		1
#define LN_UNLIMITED	2

/* data format */
#define DF_1DHUFFMAN 	0
#define DF_2DMREAD	1
#define DF_2DUNCOMP	2

/* error correction */
#define EC_DA_ECM	0
#define EC_EN_ECM_64	1
#define EC_EN_ECM_256	2

/* binary file */
#define BF_DISABLED	0
#define BF_ENABLED	1

/* scan time */
#define ST_0		0
#define ST_1		1
#define ST_2		2
#define ST_3		3
#define ST_4		4
#define ST_5		5
#define ST_6		6
#define ST_7		7

typedef struct _T30params {
    int vr;			/* vertical resolution		*/
    int br;			/* bit rate			*/
    int wd;			/* page width			*/
    int ln;			/* page length			*/
    int df;			/* data compression format	*/
    int ec;			/* error correction		*/
    int bf;			/* binary file transfer		*/
    int st;			/* scan time			*/
} T30params;

/*
  Fax flags:
*/
#define FAX_F_SYNC	0000001	/* modem is in sync		*/
#define FAX_F_FCON	0000002 /* modem is connected		*/
#define FAX_F_FDCS	0000004 /* recved neg session params	*/
#define FAX_F_FDIS	0000010	/* recved remote capabilities	*/
#define FAX_F_FHNG	0000020	/* recved hangup code		*/
#define FAX_F_FNSF 	0000040	/* recved nsf frame FAX		*/
#define FAX_F_FPTS	0000100	/* recved post page resp (ppr)	*/
#define FAX_F_FET	0000200	/* recved post page msg (ppm)	*/
#define FAX_F_FTSI 	0000400	/* recved remote TSI string	*/
#define FAX_F_FCSI 	0001000 /* recved remote CSI string	*/
#define FAX_F_FCIG 	0002000 /* recved remote CIG string	*/
#define FAX_F_CANCELED	0004000 /* the remote end canceled	*/
#define FAX_F_RETRIES	0010000	/* too many retries sending page*/
#define FAX_F_FCFR	0020000	/* recved fcfr response		*/
#define FAX_F_CONNECT	0040000 /* recved connect response	*/

/*
  Modem error conditions.  When get_modem_response returns -1,
  one of these error conditions will result.  This are hard 
  errors, that may not be possible to recover from.  The idea
  is for protocol errors to be reported in the above flags.
*/
typedef enum {
    MODEM_STATUS_OK,		/* all ok			*/
    MODEM_STATUS_FAILED,	/* a system called failed	*/
    MODEM_STATUS_TIMEOUT,	/* the modem timed out		*/
} modem_status;

/*
  Page reception codes.  Returned by faxmodem_recv_page().
*/  
typedef enum {
    RECV_OK,
    RECV_DONE,
    RECV_FAILED,
} recv_code;

#define MAX_ID_LEN	64

/*
  This structure holds the current state of the faxmodem.  Various
  fields are filled at different times in a fax session.  Not all
  fields are valid at all times.  Check flags to see what field
  currently hold valid information.
*/
typedef struct _fax_modem {
    int fd;			/* fd of the fax modem 		*/
    modem_status status;	/* status for last modem op	*/
    int flags;			/* fax modem flags (see above)	*/
    int result;			/* last numeric result code	*/
    int dialer_code;		/* result from last ATDT op 	*/
    int hangup_code;		/* +FHNG code; See table 8.3	*/
    int ppr_code;		/* +FPTS code; See table 8.2	*/
    int ppm_code;		/* +FET  code; See table 8.1	*/
    char ftsi_id[MAX_ID_LEN];	/* +FTSI id string		*/
    char fcsi_id[MAX_ID_LEN];	/* +FCSI id string		*/
    char fcig_id[MAX_ID_LEN];	/* +FCIG id string		*/
    T30params fdcs_params; 	/* current session params	*/
    T30params fdis_params;	/* remote identification params	*/
    T30params fdtc_params;	/* request for polling params	*/
} FaxModem;

#define FAX_ISSET(x,y)	 (((x)->flags)&y)
#define FAX_CONNECTED(x) (FAX_ISSET((x),FAX_F_FCON) && \
			  !FAX_ISSET((x),FAX_F_FHNG))

#endif
