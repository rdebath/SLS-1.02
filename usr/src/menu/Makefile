
CC     = gcc
CFLAGS = -O

# Some Systems may not require "-ltermcap" and may instead need "-ltermlib"

LIBS   = -lcurses -ltermcap
DEFS   =
TARGET = menu

# Uncomment the DSYS line if you are Running SYSTEM FIVE Unix.
# The System V Implementation is not complete at this point.
# Uncomment the appropriate line for your system.
# If using HP-UX add -DHPUX to the Following line.

#DSYS = -DSYSTEM_FIVE  -DHPUX
#DSYS = -DSUN
#DSYS = -DBSD43
DSYS = -DSYSTEM_FIVE

# Uncomment the TERMINFO line if your system uses TERMINFO instead of 
# TERMCAP.
#TERMINFO = -DTERM_INFO 
 
HDRS  = menu.h utils.h dir.h
SRCS  = menu.c appl.c utils.c files.c signals.c dir.c
OBJS  = menu.o appl.o utils.o files.o signals.o dir.o

BUILDOBJS = build_menu.o utils.o signals.o
BUILDSRCS = build_menu.c utils.c signals.c

$(TARGET): $(OBJS) $(SRCS) $(HDRS)
	$(CC) -s -N $(CFLAGS) $(DEFS) $(OBJS) $(DFLAGS) $(DSYS) $(TERMINFO) -o $(TARGET) $(LIBS)

menu.o  : menu.c menu.h
	$(CC) $(CFLAGS) $(DEFS) $(DFLAGS) $(DSYS) -c menu.c

appl.o  : appl.c menu.h
	$(CC) $(CFLAGS) $(DEFS) $(DFLAGS) $(DSYS) -c appl.c

utils.o : utils.c utils.h
	$(CC) $(CFLAGS) $(DEFS) $(DFLAGS) $(DSYS) $(TERMINFO) -c utils.c

files.o : files.c menu.h
	$(CC) $(CFLAGS) $(DEFS) $(DFLAGS) $(DSYS) -c files.c

signals.o : signals.c 
	$(CC) $(CFLAGS) $(DEFS) $(DFLAGS) $(DSYS) -c signals.c

dir.o : dir.c dir.h
	$(CC) $(CFLAGS) $(DEFS) $(DFLAGS) $(DSYS) -c dir.c

build_menu.o : build_menu.c menu.h
	$(CC) $(CFLAGS) $(DEFS) $(DFLAGS) $(DSYS) -c build_menu.c

build: signals.o build_menu.o utils.o menu.h
	$(CC) $(CFLAGS) $(DEFS) $(DFLAGS) $(DSYS) $(BUILDOBJS) -o build $(LIBS)
	strip build
	chmod 711 build

install:
	-chmod 711 /usr/bin/menu
	install -c -m 1711 -o root -g sys -s menu /usr/bin/menu

clean:
	rm -f $(OBJS) ./menu

lint:
	lint -DDFLAGS $(SRCS) > LINT.OUT
