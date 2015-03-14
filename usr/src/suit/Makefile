##################################################
#
# Change these three lines to 
# reflect the directories at your site.
# 
# Then type :
#	make install
# 
##################################################

# Change this to be the directory that contains 
# the top level README file
SUIT=/usr/src/suit

# Change this line to be the location of libX11.a
XLIBS=

# Change this line to be the location of X.h
X_INC_DIR=/usr/X386/include/X11

#######################
# change NOTHING below this line. 
# STAY AWAY. GO. NOW. LOOK THE OTHER WAY. FLEE.
# RUN.
# FAR...
#######################

# do this for the SGI platforms
SHELL=/bin/sh

#############
# these incantations look bad, but they do the job.
#############



install:
	make fix DIR=src/examples FILE=Makefile 		> /dev/null
	make fix DIR=src/suit	  FILE=Makefile 		> /dev/null
	make fix DIR=tutorial	  FILE=Makefile			> /dev/null
	make fix DIR=tutorial	  FILE=install_tutorial		> /dev/null

	
	@echo ""
	@echo "Installing shared libraries in /lib and stubs in /usr/lib"
	@echo ""
	cp lib/libsrgp.sa /usr/lib/libsrgp.sa
	cp lib/libsrgp.so.2.3 /lib/libsrgp.so.2.3
	ln -sf /lib/libsrgp.so.2.3 /lib/libsrgp.so.2
	cp lib/libsuit.sa /usr/lib/libsuit.sa
	cp lib/libsuit.so.2.3 /lib/libsuit.so.2.3
	ln -sf /lib/libsuit.so.2.3 /lib/libsuit.so.2
	@echo ""
	@echo "Building programs necessary for the tutorial"
	@echo ""
	cd src/examples; make demo; make poly; make 3cell;     	> /dev/null
	mv src/examples/demo bin				> /dev/null
	mv src/examples/poly bin				> /dev/null
	mv src/examples/3cell bin				> /dev/null
	strip bin/*						> /dev/null

	cd tutorial; chmod ugo+x install_tutorial; 	 	> /dev/null
	@echo ""
	@echo "Installation Complete"

fix:
	echo "Changing " $(DIR)/$(FILE)
	cd $(DIR); \
	sed -e "s!^\(SUIT[ 	]*=\).*!\1$(SUIT)!"   \
	    -e "s!^\(XLIBS[ 	]*=\).*!\1$(XLIBS)!"       \
	    -e "s!^\(X_INC_DIR[ 	]*=\).*!\1$(X_INC_DIR)!" $(FILE) > /tmp/groo ; \
	/bin/mv /tmp/groo $(FILE)
	/bin/rm -f /tmp/groo
	
