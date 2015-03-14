/*
 * dip		A program for handling dialup IP connecions.
 *		This file implements a very rudimentary terminal
 *		emulator, which can be used to log into some host
 *		manually, or to debug the connection.
 *
 * Author:      Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *		Copyright 1988-1992 MicroWalt Corporation
 *		This program is free software; you can redistribute it and/or
 *		modify it under the terms of the GNU General Public License
 *		as published by the Free Software Foundation; either version
 *		2 of the License, or (at your option) any later version.
 */
#include "dip.h"


void do_term()
{
  struct termios otty, ntty;
  char buff[1024];
  _PROTOTYPE( void (*old_sigs[32]), (int) );
  fd_set mask, rmask;
  register int i;
  int tty;

  /* First off, ask the SERIAL driver for its fd. */
  tty = tty_askfd();

  printf("[ Entering TERMINAL mode.  Use CTRL-] to get back ]\n");

  /* Set up the keyboard. */
  if (ioctl(0, TCGETS, &otty)  < 0) {
	perror("term ioctl TCGETS");
	return;
  }
  ntty = otty;
  ntty.c_lflag &= ~(ECHO | ICANON);
  ntty.c_iflag &= ~(ICRNL | ISIG | IXON | IXANY);
  ntty.c_oflag &= ~(ONLCR | OPOST);
  if (ioctl(0, TCSETS, &ntty)  < 0) {
	perror("term ioctl TCSETS");
	return;
  }
  
  /* Set up the correct descriptor masks for select(2). */
  FD_ZERO(&mask);
  FD_SET(0, &mask);
  FD_SET(tty, &mask);

  /* Disable all signals, saving them for later. */
  for (i = 0; i < 32; i++) old_sigs[i] = signal(i + 1, SIG_IGN);

  /* Go into an endless terminal loop. */
  while(1) {
	rmask = mask;
	i = select(32, &rmask, (fd_set *)NULL, (fd_set *)NULL,
					(struct timeval *)NULL);
	if (i <= 0) break;
	if (FD_ISSET(0, &rmask)) {
		i = read(0, buff, 1);
		if (i > 0) {
			if (buff[0] == (']' & 037)) break;
			(void) write(tty, buff, i);
		}
	}

	if (FD_ISSET(tty, &rmask)) {
		i = read(tty, buff, 1024);
		if (i > 0) (void) write(1, buff, i);
	};
  }

  if (ioctl(0, TCSETS, &otty)  < 0) {
	perror("term ioctl TCSETS");
	return;
  }

  /* Restore all signals. */
  for (i = 0; i < 32; i++) (void) signal(i + 1, old_sigs[i]);

  printf("\n[ Back to LOCAL mode. ]\n");
}
