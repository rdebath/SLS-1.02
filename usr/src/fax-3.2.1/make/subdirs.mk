#  subdirs.mk
#
#   (c) Copyright 1992 by David M. Siegel.
#       All rights reserved.
#
#  $Id$

all clean world:
	@for i in $(SUBDIRS) ; do \
		echo "making $@ in $$i with $(MAKE)..." ; \
		( cd $$i; $(MAKE) $@ ) ; \
	done
