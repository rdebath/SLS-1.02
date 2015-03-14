#    reversi - play a game of reversi against the computer or a human
#    Copyright (C) 1992  Elias Martenson
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
#    Contact me by email at elias@proxxi.se

EXEC = reversi
GAMEDIR = /usr/games

OBJS = reversi.o graphics.o move.o comp.o menu.o

CC = gcc

#  Put -DHAVE_VOID_SIGNALS in OPTFLAGS if Your compiler requres it

#OPTFLAGS = -g -DDEBUG -DHAVE_VOID_SIGNALS
OPTFLAGS = -O -DHAVE_VOID_SIGNALS

CFLAGS = -I/usr/local/include $(OPTFLAGS)

all:		$(EXEC)

$(EXEC):	$(OBJS)
		$(CC) $(OPTFLAGS) -o $(EXEC) $(OBJS) -lncurses 

install:	$(EXEC)
		cp $(EXEC) $(GAMEDIR)

clean:
		rm -f *.o *~ $(EXEC) core
