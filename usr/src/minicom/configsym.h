/*
 * This file is part of the Minicom Communications Program,
 * written by Miquel van Smoorenburg 1991/1992.
 *
 * configsym.h	- Offsets into the mpars structure
 *		  When the mpars structure is changed,
 *		  change these define's too.
 */

struct pars {
  /* value is first, so that (char *)mpars[0] == mpars[0].value */
  /* Try doing this in PASCAL !! :-) */
  char value[64];
  int flags;
  char *desc;
};
extern struct pars mpars[];

#define CHANGED	3
#define PRIVATE	4
#define PUBLIC	8

#define PROTO_BASE	0
#define MAXPROTO	12
#define PROG_BASE	12

#define P_PNN(n)	(mpars[PROTO_BASE + n].value[0])
#define P_PUD(n)	(mpars[PROTO_BASE + n].value[1])
#define P_PNAME(n)	(&mpars[PROTO_BASE + n].value[2])
#define P_PPROG(n)	mpars[PROG_BASE + n].value

#define P_PORT		mpars[24].value
#define P_CALLIN	mpars[25].value
#define P_CALLOUT	mpars[26].value
#define P_LOCK		mpars[27].value
#define P_BAUDRATE	mpars[28].value
#define P_BITS		mpars[29].value
#define P_PARITY	mpars[30].value
#define P_KERMIT	mpars[31].value
#define P_KERMALLOW	mpars[32].value
#define P_KERMREAL	mpars[33].value
#define P_COLUSAGE	mpars[34].value
#define P_SCRIPTPROG	mpars[35].value
/* The next entries must be kept in order */
#define P_MINIT		mpars[36].value
#define P_MRESET	mpars[37].value
#define P_MDIALPRE	mpars[38].value
#define P_MDIALSUF	mpars[39].value
#define P_MCONNECT	mpars[40].value
#define P_MNOCON1	mpars[41].value
#define P_MNOCON2	mpars[42].value
#define P_MNOCON3	mpars[43].value
#define P_MNOCON4	mpars[44].value
#define P_MHANGUP	mpars[45].value
#define P_MDIALCAN	mpars[46].value
#define P_MDIALTIME	mpars[47].value
#define P_MRDELAY	mpars[48].value
#define P_MRETRIES	mpars[49].value
/* Yup, until here. */
#define P_MAUTOBAUD	mpars[50].value
#define P_MDROPDTR	mpars[51].value
#define P_UPDIR		mpars[52].value
#define P_DOWNDIR	mpars[53].value
#define P_SCRIPTDIR	mpars[54].value
#define P_ESCAPE	mpars[55].value
#define P_BACKSPACE	mpars[56].value
#define P_STATLINE	mpars[57].value
#define P_HASDCD	mpars[58].value
