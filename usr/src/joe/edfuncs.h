/* User edit functions
   Copyright (C) 1992 Joseph H. Allen

This file is part of JOE (Joe's Own Editor)

JOE is free software; you can redistribute it and/or modify it under the 
terms of the GNU General Public License as published by the Free Software 
Foundation; either version 1, or (at your option) any later version.  

JOE is distributed in the hope that it will be useful, but WITHOUT ANY 
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more 
details.  

You should have received a copy of the GNU General Public License along with 
JOE; see the file COPYING.  If not, write to the Free Software Foundation, 
675 Mass Ave, Cambridge, MA 02139, USA.  */ 

#ifndef _Iedfncs
#define _Iedfncs 1

#include "config.h"

extern char *msgs[];

void umarkb();
void umarkk();
void ublkdel();
void ublkcpy();
void ublkmove();
void ublksave();

void ufilt();
void ulindent();
void urindent();

void uarg();

void ugroww();
void ushrnk();
void unextw();
void uprevw();
void uexpld();

void ushell();

void uretyp();

void uexsve();
void usave();
void uedit();
void uinsf();

void ubof();
void ueof();

void ubol();
void ueol();
void upedge();
void unedge();

void ultarw();
void urtarw();

void uprvwrd();
void unxtwrd();

void uuparw();
void udnarw();

void upgup();
void upgdn();
void uupslide();
void udnslide();

void uline();

void utomatch();

void uopen();
void uinsc();

void udelch();
void ubacks();

void udelw();
void ubackw();

void udelel();
void udelbl();
void udelln();

void uquote();
void uquote8();
void urtn();
void utype();

void ucenter();
void uformat();

void uirmargin();
void uilmargin();
void uitype();
void uiwrap();
void uiindent();
void uipgamnt();
void uimid();
void uiforce();
void uictrl();
void uiasis();
void uistarow();
void uistacol();

#endif
