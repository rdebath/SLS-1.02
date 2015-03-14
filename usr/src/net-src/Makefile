SUBDIRS = libbsd ftp ftpd telnet inetd ping named \
	rcp rsh rshd rlogin rlogind \
	finger fingerd talk talkd tftp tftpd

all:

.DEFAULT:
	@for i in $(SUBDIRS); do (cd ./$$i && echo $$i: && $(MAKE) $@) || exit 1; done

