#
# Makefile for /etc/magic file for Ian Darwin's file(1) command.
# Copyright (c) 1989 Ian F. Darwin, Toronto, Canada.
# $Id: Makefile,v 1.7 92/09/08 14:51:14 ian Exp $
#
#
SHELL		= /bin/sh
#HEADER		= Header
#LOCALSTUFF	= Localstuff

ALL		=  $(HEADER) $(LOCALSTUFF) [a-z]*

install:	../magic
../magic:	$(ALL)
		echo "# Magic file created `date` by `who am i`" > $@
		cat $(ALL) >> $@
