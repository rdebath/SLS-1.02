/* TTY interface for MSDOS using TURBO-C
   Copyright (C) 1991 Joseph H. Allen

This file is part of JOE (Joe's Own Editor)

JOE is free software; you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software
Foundation; either version 1, or (at your option) any later version. 

JOE is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
A PARTICULAR PURPOSE.  See the GNU General Public License for more details.  

You should have received a copy of the GNU General Public License
along with JOE; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <stdio.h>
#include <conio.h>
#include <bios.h>
#include "config.h"
#include "tty.h"

unsigned baud=38400;
unsigned long upc;
int have=0;
int leave=0;
static int ttymode=0;

void ttputs(s)
char *s;
{
fputs(s,stdout);
if(!have) have=bioskey(1);
}

void ttputc(c)
char c;
{
putchar(c);
if(!have) have=bioskey(1);
}

void sigjoe()
{
}

void signrm()
{
}

void ttopen()
{
ttopnn();
}

void ttopnn()
{
fflush(stdout);
{
char *bs=getenv("BAUD");
if(bs)
 {
 sscanf(bs,"%u",&baud);
 }
}
upc=DIVIDEND/baud;
}

void ttclose()
{
ttclsn();
}

void ttclsn()
{
ttflsh();
}

void ttflsh()
{
fflush(stdout);
if(!have) have=bioskey(1);
}

int ttgetc()
{
char c;
have=0;
c=getch();
return c;
}

void ttgtsz(x,y)
int *x, *y;
{
}

void ttshell(cmd)
char *cmd;
{
char *s=getenv("COMSPEC");
if(cmd) system(cmd);
else if(s) system(s);
}

void ttsusp()
{
ttshell(NULL);
}

char *getcwd();
char *pwd()
{
static char buf[1024];
return getcwd(buf,1024);
}
