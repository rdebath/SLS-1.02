/*
 * dip		A program for handling dialup IP connecions.
 *		Modem driving module.  On systems that support the
 *		dial(3) package, we (should) use that.  Otherwise,
 *		we use a very rudimentary HAYES-type dialer.
 *
 * Author:      Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *		Copyright 1988-1992 MicroWalt Corporation
 *		This program is free software; you can redistribute it and/or
 *		modify it under the terms of the GNU General Public License
 *		as published by the Free Software Foundation; either version
 *		2 of the License, or (at your option) any later version.
 */
#include "dip.h"


/* Here are some HAYES modem commands. */
#define AT_ATTN		"+++"			/* "attention" string	*/
#define AT_RSET		"ATZ"			/* reset the modem	*/
#define AT_INIT		"ATE0 Q0 V1 X1"		/* setup for dialing	*/
#define AT_DIAL		"ATD"			/* start a dial		*/
#define AT_EOL		"\r"			/* AT "CRLF" mark	*/


/* Set the desired type of modem. */
int mdm_set(modem)
char *modem;
{
  if (strcmp(modem, DEF_MODEM)) {
	fprintf(stderr, "dip: I only understand %s modems...\n", DEF_MODEM);
	return(-1);
  }
  strcpy(var_modem, modem);
  if (opt_v == 1) printf("Modem set to \"%s\".\n", var_modem);
  return(0);
}


/* Dial a phone number. */
int mdm_dial(number)
char *number;
{
  char buff[128];

  /* Setup to dial out. */
  sprintf(buff, "%s%s", AT_INIT, AT_EOL);
  tty_puts(buff);
  (void) sleep(1);

  /* Dial the phone number. */
  sprintf(buff, "%s%s%s", AT_DIAL, number, AT_EOL);
  tty_puts(buff);

  return(0);
}


int mdm_reset()
{
  char buff[128];

  /* Make the modem listen to us. */
  (void) sleep(1);
  tty_puts(AT_ATTN);
  (void) sleep(1);

  /* Reset the modem. */
  sprintf(buff, "%s%s", AT_RSET, AT_EOL);
  tty_puts(buff);
  (void) sleep(3);
  return(0);
}
