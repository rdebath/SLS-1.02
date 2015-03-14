/* Copyright (C) 1992 Free Software Foundation, Inc.
This file is part of the GNU C Library.

The GNU C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU C Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */
/* Modified by Al Clark 11/12/92 to no assume any initial conditions.
 * Also, compile time variations "#define USEPARITY" to leave parity alone,
 * and "define NOINTR" to disable interrupt on BREAK condition.
 *  GNU C Library terms still apply.
 */

#include <ansidecl.h>
#include <termios.h>

void
DEFUN(cfmakeraw, (t), struct termios *t)
{
                /*  VMIN = 0 means non-blocking for Linux */
    t->c_cc[VMIN] = 1; t->c_cc[VTIME] = 1;
                /* clear some bits with &= ~(bits), set others with |= */
#ifdef USEPARITY  /* leave parity alone */
    t->c_cflag &= ~(CSTOPB|CLOCAL|CRTSCTS);
    t->c_cflag |=  (HUPCL|CREAD);
#else             /* normal full eight bit case */
    t->c_cflag &= ~(CSIZE|PARENB|PARODD|CSTOPB|CLOCAL|CRTSCTS);
    t->c_cflag |=  (CS8|HUPCL|CREAD);
#endif
    t->c_iflag &= ~(IGNBRK|BRKINT|PARMRK|INPCK|ISTRIP|INLCR|IGNCR|ICRNL|IXON|IXOFF);
#ifdef NOINTR
    t->c_iflag |=  (IGNPAR);
#else
    t->c_iflag |=  (BRKINT|IGNPAR);
#endif
    t->c_oflag &= ~(OPOST|OLCUC|OCRNL|ONOCR|ONLRET|OFILL|OFDEL);
    t->c_oflag &= ~(NLDLY|CRDLY|TABDLY|BSDLY|VTDLY|FFDLY);
    t->c_oflag |=  (ONLCR|NL0|CR0|TAB3|BS0|VT0|FF0);
    t->c_lflag &= ~(ISIG|ICANON|IEXTEN|ECHO|ECHOE|ECHOK|ECHONL);
    t->c_lflag &= ~(NOFLSH|XCASE|TOSTOP);
    t->c_lflag |=  (ECHOPRT|ECHOCTL|ECHOKE);
}
