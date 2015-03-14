include ../../Configfile

CFLAGS=		$(FLAGS) $(MGRFLAGS) \
		-DFONTDIR=\"$(FONTDIR)\" -DICONDIR=\"$(ICONDIR)\" \
                -DSCREEN_DEV=\"$(SCREEN_DEV)\" -DMOUSE_DEV=\"$(MOUSE_DEV)\"

OBJS=		Write.o border.o copyright.o cut.o data.o debug_flags.o \
		destroy.o do_buckey.o do_button.o do_event.o do_menu.o \
                down_load.o erase_win.o font_subs.o get_font.o get_info.o \
		get_menus.o get_rect.o get_text.o getshell.o graph_subs.o \
		icon_server.o intersect.o kbd.o mgr.o mouse_get.o move.o \
                move_box.o new_window.o print.o put_window.o scroll.o \
                set_mode.o shape.o sigdata.o startup.o subs.o update.o utmp.o \
                win_make.o win_stack.o win_subs.o write_ok.o mouse.o

all:		mgr

mgr:		$(OBJS)
		$(CC) $(LDFLAGS) -o mgr $(OBJS) $(LIBDIR)/libbitblit.a $(LIBEMU)

default_font.h:	$(DEFAULT_FONT) fonttoc
		./fonttoc < $(DEFAULT_FONT) default_font > default_font.h

fonttoc:
		if [ -x ../clients/misc/fonttoc ]; \
		then \
		  rm -f fonttoc; \
		  ln -s ../clients/misc/fonttoc fonttoc; \
		else \
                  if [ -x $(BINDIR)/fonttoc ]; \
                  then \
                    ln -s $(BINDIR)/fonttoc fonttoc; \
                  else \
                    (cd ../clients/misc; make fonttoc); ln -s ../clients/misc/fonttoc fonttoc; \
                  fi; \
		fi \

icon_server.h:
		echo '/* bitmaps used by mgr server, file is created by make(1) */' > $@
		for i in $(ICONDIR)/server/* ; do echo extern BITMAP `basename $$i`';' >> $@ ; done

icon_server.c:	bitmaptoc
		echo '/* bitmaps used by mgr server, file is created by make(1) */' > $@
		echo '#include "bitmap.h"' >> $@
		./bitmaptoc $(ICONDIR)/server/* >> $@

bitmaptoc:
		if [ -x ../clients/icon/bitmaptoc ]; \
		then \
		  rm -f bitmaptoc; \
		  ln -s ../clients/icon/bitmaptoc bitmaptoc; \
		else \
                  if [ -x $(BINDIR)/bitmaptoc ]; \
                  then \
                    ln -s $(BINDIR)/bitmaptoc bitmaptoc; \
                  else \
                    (cd ../clients/icon; make bitmaptoc); ln -s ../clients/icon/bitmaptoc bitmaptoc; \
                  fi; \
		fi \

version.h:
		echo "/* compilation info for mgr, file is created by make(1) */" > version.h
		echo >> version.h
		echo "char version[]=\"MGR version $(VERSION) compiled by `whoami` at `date`.\\\nSystem: `uname -a`\\\n\";" >> version.h

depend:         default_font.h icon_server.c icon_server.h version.h
		$(CC) -M $(CFLAGS) *.c > dependencies

clean:
		rm -f $(OBJS)

clobber:	clean
		rm -f core mgr default_font.h icon_server.[ch] version.h fonttoc bitmaptoc
		cat /dev/null > dependencies

install:	mgr
		install -s -m $(SERVER_PERM) mgr $(BINDIR)
		install -c -m 644 mgr.1 $(MANDIR)/man1

include dependencies
