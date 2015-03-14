# nfs-server.mk - makefile for nfsd and mountd

CC = gcc
LDLIBS = # -lrpclib

DEBUGFLAGS = # -g -DRPC_SVC_FG

DEFINES = # -DEXPORTSFILE=\"/etc/exports\"
CFLAGS = $(DEBUGFLAGS) -O2 -Wall -DDEBUG $(DEFINES)

LDFLAGS = -N -s

RPCGEN = rpcgen
RPCSVCDIR = /usr/include/rpcsvc
#TRANSPORTFLAGS = -I -s udp -s tcp
TRANSPORTFLAGS = -s udp -s tcp

NFSD = nfsd
MOUNTD = mountd
PROGS = $(NFSD) $(MOUNTD)

BININSTALL = install -m 755
MANINSTALL = install -m 644
ETCDIR = /etc
MAN5DIR = /usr/man/man5
MAN8DIR = /usr/man/man8
MAN5FILES = exports.5
MAN8FILES = mountd.8 nfsd.8

CFILES = nfsd.c dispatch.c getattr.c fh.c auth_clnt.c \
	logging.c init_auth.c main.c mountd.c
HFILES = nfsd.h fh.h auth.h

XDRFILES = mount.x nfs_prot.x
GENCFILES = mount_svc.c mount_xdr.c nfs_prot_xdr.c
GENHFILES = mount.h nfs_prot.h
GENFILES = $(XDRFILES) $(GENCFILES) $(GENHFILES)

OBJS = init_auth.o auth_clnt.o fh.o
NFSD_OBJS = nfsd.o main.o dispatch.o getattr.o logging.o nfs_prot_xdr.o $(OBJS)
MOUNTD_OBJS = mountd.o mount_svc.o mount_xdr.o $(OBJS)

all: $(PROGS)

$(NFSD): $(NFSD_OBJS)
	$(CC) $(LDFLAGS) -o $@ $(NFSD_OBJS) $(LDLIBS)

$(MOUNTD): $(MOUNTD_OBJS)
	$(CC) $(LDFLAGS) -o $@ $(MOUNTD_OBJS) $(LDLIBS)

nfs_prot.x:
	cp $(RPCSVCDIR)/nfs_prot.x .

mount.x:
	cp $(RPCSVCDIR)/mount.x .

nfs_prot.h: nfs_prot.x
	$(RPCGEN) -h -o $@ $?

nfs_prot_xdr.c: nfs_prot.x
	$(RPCGEN) -c -o $@ $?

mount.h: mount.x
	$(RPCGEN) -h -o $@ $?

mount_xdr.c: mount.x
	$(RPCGEN) -c -o $@ $?

mount_svc.c: mount.x
	$(RPCGEN) $(TRANSPORTFLAGS) $? | \
		sed 's/svc_run/mountd_init(argc, argv); &/' >$@

install:
	$(BININSTALL) $(PROGS) $(ETCDIR)
	$(MANINSTALL) $(MAN5FILES) $(MAN5DIR)
	$(MANINSTALL) $(MAN8FILES) $(MAN8DIR)

rpcsvc: $(GENFILES)
	cp -p $(GENFILES) rpcsvc

dep: $(CFILES) $(HFILES) $(GENFILES)
	sed '/^### Dep/q' Makefile >tmp_make
	$(CC) -M $(CFLAGS) $(CFILES) $(GENCFILES) >> tmp_make
	mv Makefile Makefile~
	mv tmp_make Makefile

clean:
	rm -f *.o *.b *.bak *~ \#* tmp_make $(GENFILES)

clobber: clean
	rm -f $(PROGS)

### Dependencies: do not delete this line
nfsd.o : nfsd.c /usr/include/sys/vfs.h /usr/include/features.h /usr/include/sys/cdefs.h \
  /usr/include/linux/vfs.h /usr/include/sys/types.h /usr/include/linux/types.h \
  /usr/include/strings.h /usr/include/string.h /usr/lib/gcc-lib/i386-linux/2.3.3/include/stddef.h \
  /usr/include/sys/file.h /usr/include/fcntl.h /usr/include/linux/fcntl.h /usr/include/sys/dir.h \
  /usr/include/dirent.h /usr/include/gnu/types.h /usr/lib/gcc-lib/i386-linux/2.3.3/include/limits.h \
  /usr/lib/gcc-lib/i386-linux/2.3.3/include/syslimits.h /usr/include/limits.h \
  /usr/include/posix1_lim.h /usr/include/linux/limits.h /usr/include/posix2_lim.h \
  /usr/include/linux/dirent.h nfsd.h /usr/include/sys/socket.h /usr/include/linux/socket.h \
  /usr/include/rpc/rpc.h /usr/include/sys/time.h /usr/include/linux/time.h /usr/include/time.h \
  /usr/include/rpc/types.h /usr/include/unistd.h /usr/include/posix_opt.h /usr/include/stdlib.h \
  /usr/include/errno.h /usr/include/linux/errno.h /usr/lib/gcc-lib/i386-linux/2.3.3/include/float.h \
  /usr/include/alloca.h /usr/include/netinet/in.h /usr/include/rpc/xdr.h /usr/include/stdio.h \
  /usr/include/_G_config.h /usr/include/rpc/auth.h /usr/include/rpc/clnt.h /usr/include/rpc/rpc_msg.h \
  /usr/include/rpc/auth_unix.h /usr/include/rpc/svc.h /usr/include/rpc/svc_auth.h \
  nfs_prot.h libc.h /usr/include/ctype.h /usr/include/sys/stat.h /usr/include/linux/stat.h \
  fh.h 
dispatch.o : dispatch.c /usr/include/stdio.h /usr/include/features.h /usr/include/sys/cdefs.h \
  /usr/include/_G_config.h /usr/include/syslog.h /usr/include/sys/syslog.h /usr/lib/gcc-lib/i386-linux/2.3.3/include/stdarg.h \
  nfsd.h /usr/include/sys/types.h /usr/include/linux/types.h /usr/include/sys/socket.h \
  /usr/include/linux/socket.h /usr/include/rpc/rpc.h /usr/include/sys/time.h /usr/include/linux/time.h \
  /usr/include/time.h /usr/include/rpc/types.h /usr/include/unistd.h /usr/include/posix_opt.h \
  /usr/include/gnu/types.h /usr/lib/gcc-lib/i386-linux/2.3.3/include/stddef.h \
  /usr/include/stdlib.h /usr/include/errno.h /usr/include/linux/errno.h /usr/lib/gcc-lib/i386-linux/2.3.3/include/float.h \
  /usr/include/alloca.h /usr/include/string.h /usr/include/netinet/in.h /usr/include/rpc/xdr.h \
  /usr/include/rpc/auth.h /usr/include/rpc/clnt.h /usr/include/rpc/rpc_msg.h /usr/include/rpc/auth_unix.h \
  /usr/include/rpc/svc.h /usr/include/rpc/svc_auth.h nfs_prot.h libc.h /usr/include/ctype.h \
  /usr/include/sys/stat.h /usr/include/linux/stat.h /usr/include/fcntl.h /usr/include/linux/fcntl.h 
getattr.o : getattr.c nfsd.h /usr/include/sys/types.h /usr/include/linux/types.h \
  /usr/include/sys/socket.h /usr/include/features.h /usr/include/sys/cdefs.h /usr/include/linux/socket.h \
  /usr/include/rpc/rpc.h /usr/include/sys/time.h /usr/include/linux/time.h /usr/include/time.h \
  /usr/include/rpc/types.h /usr/include/unistd.h /usr/include/posix_opt.h /usr/include/gnu/types.h \
  /usr/lib/gcc-lib/i386-linux/2.3.3/include/stddef.h /usr/include/stdlib.h /usr/include/errno.h \
  /usr/include/linux/errno.h /usr/lib/gcc-lib/i386-linux/2.3.3/include/float.h \
  /usr/include/alloca.h /usr/include/string.h /usr/include/netinet/in.h /usr/include/rpc/xdr.h \
  /usr/include/stdio.h /usr/include/_G_config.h /usr/include/rpc/auth.h /usr/include/rpc/clnt.h \
  /usr/include/rpc/rpc_msg.h /usr/include/rpc/auth_unix.h /usr/include/rpc/svc.h \
  /usr/include/rpc/svc_auth.h nfs_prot.h libc.h /usr/include/ctype.h /usr/include/sys/stat.h \
  /usr/include/linux/stat.h /usr/include/fcntl.h /usr/include/linux/fcntl.h fh.h 
fh.o : fh.c /usr/include/sys/param.h /usr/lib/gcc-lib/i386-linux/2.3.3/include/limits.h \
  /usr/lib/gcc-lib/i386-linux/2.3.3/include/syslimits.h /usr/include/limits.h \
  /usr/include/features.h /usr/include/sys/cdefs.h /usr/include/posix1_lim.h /usr/include/linux/limits.h \
  /usr/include/posix2_lim.h /usr/include/linux/param.h /usr/include/sys/types.h \
  /usr/include/linux/types.h /usr/include/sys/file.h /usr/include/fcntl.h /usr/include/linux/fcntl.h \
  /usr/include/rpc/rpc.h /usr/include/sys/time.h /usr/include/linux/time.h /usr/include/time.h \
  /usr/include/rpc/types.h /usr/include/unistd.h /usr/include/posix_opt.h /usr/include/gnu/types.h \
  /usr/lib/gcc-lib/i386-linux/2.3.3/include/stddef.h /usr/include/stdlib.h /usr/include/errno.h \
  /usr/include/linux/errno.h /usr/lib/gcc-lib/i386-linux/2.3.3/include/float.h \
  /usr/include/alloca.h /usr/include/string.h /usr/include/netinet/in.h /usr/include/sys/socket.h \
  /usr/include/linux/socket.h /usr/include/rpc/xdr.h /usr/include/stdio.h /usr/include/_G_config.h \
  /usr/include/rpc/auth.h /usr/include/rpc/clnt.h /usr/include/rpc/rpc_msg.h /usr/include/rpc/auth_unix.h \
  /usr/include/rpc/svc.h /usr/include/rpc/svc_auth.h /usr/include/sys/dir.h /usr/include/dirent.h \
  /usr/include/linux/dirent.h /usr/include/signal.h /usr/include/linux/signal.h \
  /usr/include/strings.h /usr/include/syslog.h /usr/include/sys/syslog.h /usr/lib/gcc-lib/i386-linux/2.3.3/include/stdarg.h \
  nfs_prot.h libc.h /usr/include/ctype.h /usr/include/sys/stat.h /usr/include/linux/stat.h \
  fh.h 
auth_clnt.o : auth_clnt.c /usr/include/netdb.h /usr/include/features.h /usr/include/sys/cdefs.h \
  /usr/include/syslog.h /usr/include/sys/syslog.h /usr/lib/gcc-lib/i386-linux/2.3.3/include/stdarg.h \
  /usr/include/arpa/inet.h /usr/include/netinet/in.h /usr/include/sys/socket.h \
  /usr/include/linux/socket.h nfsd.h /usr/include/sys/types.h /usr/include/linux/types.h \
  /usr/include/rpc/rpc.h /usr/include/sys/time.h /usr/include/linux/time.h /usr/include/time.h \
  /usr/include/rpc/types.h /usr/include/unistd.h /usr/include/posix_opt.h /usr/include/gnu/types.h \
  /usr/lib/gcc-lib/i386-linux/2.3.3/include/stddef.h /usr/include/stdlib.h /usr/include/errno.h \
  /usr/include/linux/errno.h /usr/lib/gcc-lib/i386-linux/2.3.3/include/float.h \
  /usr/include/alloca.h /usr/include/string.h /usr/include/rpc/xdr.h /usr/include/stdio.h \
  /usr/include/_G_config.h /usr/include/rpc/auth.h /usr/include/rpc/clnt.h /usr/include/rpc/rpc_msg.h \
  /usr/include/rpc/auth_unix.h /usr/include/rpc/svc.h /usr/include/rpc/svc_auth.h \
  nfs_prot.h libc.h /usr/include/ctype.h /usr/include/sys/stat.h /usr/include/linux/stat.h \
  /usr/include/fcntl.h /usr/include/linux/fcntl.h auth.h mount.h 
logging.o : logging.c /usr/include/stdio.h /usr/include/features.h /usr/include/sys/cdefs.h \
  /usr/include/_G_config.h nfsd.h /usr/include/sys/types.h /usr/include/linux/types.h \
  /usr/include/sys/socket.h /usr/include/linux/socket.h /usr/include/rpc/rpc.h \
  /usr/include/sys/time.h /usr/include/linux/time.h /usr/include/time.h /usr/include/rpc/types.h \
  /usr/include/unistd.h /usr/include/posix_opt.h /usr/include/gnu/types.h /usr/lib/gcc-lib/i386-linux/2.3.3/include/stddef.h \
  /usr/include/stdlib.h /usr/include/errno.h /usr/include/linux/errno.h /usr/lib/gcc-lib/i386-linux/2.3.3/include/float.h \
  /usr/include/alloca.h /usr/include/string.h /usr/include/netinet/in.h /usr/include/rpc/xdr.h \
  /usr/include/rpc/auth.h /usr/include/rpc/clnt.h /usr/include/rpc/rpc_msg.h /usr/include/rpc/auth_unix.h \
  /usr/include/rpc/svc.h /usr/include/rpc/svc_auth.h nfs_prot.h libc.h /usr/include/ctype.h \
  /usr/include/sys/stat.h /usr/include/linux/stat.h /usr/include/fcntl.h /usr/include/linux/fcntl.h \
  fh.h 
init_auth.o : init_auth.c /usr/include/netdb.h /usr/include/features.h /usr/include/sys/cdefs.h \
  /usr/include/stdio.h /usr/include/_G_config.h /usr/include/syslog.h /usr/include/sys/syslog.h \
  /usr/lib/gcc-lib/i386-linux/2.3.3/include/stdarg.h /usr/include/ctype.h /usr/include/strings.h \
  /usr/include/string.h /usr/lib/gcc-lib/i386-linux/2.3.3/include/stddef.h nfsd.h \
  /usr/include/sys/types.h /usr/include/linux/types.h /usr/include/sys/socket.h \
  /usr/include/linux/socket.h /usr/include/rpc/rpc.h /usr/include/sys/time.h /usr/include/linux/time.h \
  /usr/include/time.h /usr/include/rpc/types.h /usr/include/unistd.h /usr/include/posix_opt.h \
  /usr/include/gnu/types.h /usr/include/stdlib.h /usr/include/errno.h /usr/include/linux/errno.h \
  /usr/lib/gcc-lib/i386-linux/2.3.3/include/float.h /usr/include/alloca.h /usr/include/netinet/in.h \
  /usr/include/rpc/xdr.h /usr/include/rpc/auth.h /usr/include/rpc/clnt.h /usr/include/rpc/rpc_msg.h \
  /usr/include/rpc/auth_unix.h /usr/include/rpc/svc.h /usr/include/rpc/svc_auth.h \
  nfs_prot.h libc.h /usr/include/sys/stat.h /usr/include/linux/stat.h /usr/include/fcntl.h \
  /usr/include/linux/fcntl.h mount.h 
main.o : main.c /usr/include/stdio.h /usr/include/features.h /usr/include/sys/cdefs.h \
  /usr/include/_G_config.h /usr/include/syslog.h /usr/include/sys/syslog.h /usr/lib/gcc-lib/i386-linux/2.3.3/include/stdarg.h \
  /usr/include/sys/ioctl.h /usr/include/linux/ioctl.h /usr/include/termios.h /usr/include/sys/types.h \
  /usr/include/linux/types.h /usr/include/linux/termios.h /usr/include/rpc/rpc.h \
  /usr/include/sys/time.h /usr/include/linux/time.h /usr/include/time.h /usr/include/rpc/types.h \
  /usr/include/unistd.h /usr/include/posix_opt.h /usr/include/gnu/types.h /usr/lib/gcc-lib/i386-linux/2.3.3/include/stddef.h \
  /usr/include/stdlib.h /usr/include/errno.h /usr/include/linux/errno.h /usr/lib/gcc-lib/i386-linux/2.3.3/include/float.h \
  /usr/include/alloca.h /usr/include/string.h /usr/include/netinet/in.h /usr/include/sys/socket.h \
  /usr/include/linux/socket.h /usr/include/rpc/xdr.h /usr/include/rpc/auth.h /usr/include/rpc/clnt.h \
  /usr/include/rpc/rpc_msg.h /usr/include/rpc/auth_unix.h /usr/include/rpc/svc.h \
  /usr/include/rpc/svc_auth.h /usr/include/getopt.h /usr/include/signal.h /usr/include/linux/signal.h \
  nfsd.h nfs_prot.h libc.h /usr/include/ctype.h /usr/include/sys/stat.h /usr/include/linux/stat.h \
  /usr/include/fcntl.h /usr/include/linux/fcntl.h fh.h auth.h mount.h 
mountd.o : mountd.c /usr/include/errno.h /usr/include/features.h /usr/include/sys/cdefs.h \
  /usr/include/linux/errno.h /usr/include/sys/types.h /usr/include/linux/types.h \
  /usr/include/sys/stat.h /usr/include/linux/stat.h /usr/include/rpc/rpc.h /usr/include/sys/time.h \
  /usr/include/linux/time.h /usr/include/time.h /usr/include/rpc/types.h /usr/include/unistd.h \
  /usr/include/posix_opt.h /usr/include/gnu/types.h /usr/lib/gcc-lib/i386-linux/2.3.3/include/stddef.h \
  /usr/include/stdlib.h /usr/lib/gcc-lib/i386-linux/2.3.3/include/float.h /usr/include/alloca.h \
  /usr/include/string.h /usr/include/netinet/in.h /usr/include/sys/socket.h /usr/include/linux/socket.h \
  /usr/include/rpc/xdr.h /usr/include/stdio.h /usr/include/_G_config.h /usr/include/rpc/auth.h \
  /usr/include/rpc/clnt.h /usr/include/rpc/rpc_msg.h /usr/include/rpc/auth_unix.h \
  /usr/include/rpc/svc.h /usr/include/rpc/svc_auth.h /usr/include/sys/ioctl.h \
  /usr/include/linux/ioctl.h /usr/include/termios.h /usr/include/linux/termios.h \
  /usr/include/fcntl.h /usr/include/linux/fcntl.h /usr/include/syslog.h /usr/include/sys/syslog.h \
  /usr/lib/gcc-lib/i386-linux/2.3.3/include/stdarg.h mount.h nfs_prot.h fh.h nfsd.h \
  auth.h /usr/include/ctype.h 
mount_svc.o : mount_svc.c mount.h /usr/include/rpc/rpc.h /usr/include/sys/time.h \
  /usr/include/features.h /usr/include/sys/cdefs.h /usr/include/linux/types.h \
  /usr/include/linux/time.h /usr/include/time.h /usr/include/sys/types.h /usr/include/rpc/types.h \
  /usr/include/unistd.h /usr/include/posix_opt.h /usr/include/gnu/types.h /usr/lib/gcc-lib/i386-linux/2.3.3/include/stddef.h \
  /usr/include/stdlib.h /usr/include/errno.h /usr/include/linux/errno.h /usr/lib/gcc-lib/i386-linux/2.3.3/include/float.h \
  /usr/include/alloca.h /usr/include/string.h /usr/include/netinet/in.h /usr/include/sys/socket.h \
  /usr/include/linux/socket.h /usr/include/rpc/xdr.h /usr/include/stdio.h /usr/include/_G_config.h \
  /usr/include/rpc/auth.h /usr/include/rpc/clnt.h /usr/include/rpc/rpc_msg.h /usr/include/rpc/auth_unix.h \
  /usr/include/rpc/svc.h /usr/include/rpc/svc_auth.h /usr/include/rpc/pmap_clnt.h \
  /usr/include/signal.h /usr/include/linux/signal.h /usr/include/sys/ioctl.h /usr/include/linux/ioctl.h \
  /usr/include/termios.h /usr/include/linux/termios.h /usr/include/sys/stat.h \
  /usr/include/linux/stat.h /usr/include/fcntl.h /usr/include/linux/fcntl.h /usr/include/memory.h \
  /usr/include/syslog.h /usr/include/sys/syslog.h /usr/lib/gcc-lib/i386-linux/2.3.3/include/stdarg.h 
mount_xdr.o : mount_xdr.c mount.h /usr/include/rpc/rpc.h /usr/include/sys/time.h \
  /usr/include/features.h /usr/include/sys/cdefs.h /usr/include/linux/types.h \
  /usr/include/linux/time.h /usr/include/time.h /usr/include/sys/types.h /usr/include/rpc/types.h \
  /usr/include/unistd.h /usr/include/posix_opt.h /usr/include/gnu/types.h /usr/lib/gcc-lib/i386-linux/2.3.3/include/stddef.h \
  /usr/include/stdlib.h /usr/include/errno.h /usr/include/linux/errno.h /usr/lib/gcc-lib/i386-linux/2.3.3/include/float.h \
  /usr/include/alloca.h /usr/include/string.h /usr/include/netinet/in.h /usr/include/sys/socket.h \
  /usr/include/linux/socket.h /usr/include/rpc/xdr.h /usr/include/stdio.h /usr/include/_G_config.h \
  /usr/include/rpc/auth.h /usr/include/rpc/clnt.h /usr/include/rpc/rpc_msg.h /usr/include/rpc/auth_unix.h \
  /usr/include/rpc/svc.h /usr/include/rpc/svc_auth.h 
nfs_prot_xdr.o : nfs_prot_xdr.c nfs_prot.h /usr/include/rpc/rpc.h /usr/include/sys/time.h \
  /usr/include/features.h /usr/include/sys/cdefs.h /usr/include/linux/types.h \
  /usr/include/linux/time.h /usr/include/time.h /usr/include/sys/types.h /usr/include/rpc/types.h \
  /usr/include/unistd.h /usr/include/posix_opt.h /usr/include/gnu/types.h /usr/lib/gcc-lib/i386-linux/2.3.3/include/stddef.h \
  /usr/include/stdlib.h /usr/include/errno.h /usr/include/linux/errno.h /usr/lib/gcc-lib/i386-linux/2.3.3/include/float.h \
  /usr/include/alloca.h /usr/include/string.h /usr/include/netinet/in.h /usr/include/sys/socket.h \
  /usr/include/linux/socket.h /usr/include/rpc/xdr.h /usr/include/stdio.h /usr/include/_G_config.h \
  /usr/include/rpc/auth.h /usr/include/rpc/clnt.h /usr/include/rpc/rpc_msg.h /usr/include/rpc/auth_unix.h \
  /usr/include/rpc/svc.h /usr/include/rpc/svc_auth.h 
