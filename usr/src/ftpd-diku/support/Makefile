CFLAGS= -DSETPROCTITLE -DDEBUG -s -O6 -fstrength-reduce  -DNBBY=8
CC=gcc -w
all:

clean:
	rm *.o

ftp.h:		FRC
	install -c -m 444 ftp.h /usr/include/arpa

paths.h:	FRC
	install -c -m 444 paths.h /usr/include

fnmatch.o:	FRC
	${CC} ${CFLAGS} -c fnmatch.c
	cp fnmatch.o ..

getusershell.o:	FRC
	${CC} ${CFLAGS} -c getusershell.c
	cp getusershell.o ..

strerror.o:	FRC
	${CC} ${CFLAGS} -c strerror.c
	cp strerror.o ..

strcasestr.o:	FRC
	${CC} ${CFLAGS} -c strcasestr.c
	cp strcasestr.o ..

strsep.o:	FRC
	${CC} ${CFLAGS} -c strsep.c
	cp strsep.o ..

getgrent.o:	FRC
	${CC} ${CFLAGS} -c getgrent.c
	cp getgrent.o ..

authuser.o:	FRC
	${CC} ${CFLAGS} -c authuser.c
	cp authuser.o ..

ftw.o:	FRC
	${CC} ${CFLAGS} -c ftw.c
	cp ftw.o ..

FRC:
