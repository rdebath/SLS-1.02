SUBDIRS = libtelnet telnet telnetd

all:

.DEFAULT:
	@for i in $(SUBDIRS); do (cd ./$$i && echo $$i: && $(MAKE) $@) || exit 1; done

