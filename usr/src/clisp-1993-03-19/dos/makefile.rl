CC=gcc
CFLAGS=-O -g
#CFLAGS=-O6 -fomit-frame-pointer

libreadline.a : readline.o tilde.o history.o keymaps.o funmap.o vi_mode.o xmalloc.o
	del libreadline.a
	ar qv libreadline.a readline.o tilde.o history.o keymaps.o funmap.o vi_mode.o xmalloc.o
	ar rvs libreadline.a

funmap.o : funmap.c
	$(CC) $(CFLAGS) -c funmap.c

history.o : history.c
	$(CC) $(CFLAGS) -c history.c

keymaps.o : keymaps.c emacs_keymap.c vi_keymap.c
	$(CC) $(CFLAGS) -c keymaps.c

readline.o : readline.c
	$(CC) $(CFLAGS) -c readline.c

tilde.o : ..\glob\tilde.c
	$(CC) $(CFLAGS) -I. -c ../glob/tilde.c -o tilde.o

vi_mode.o : vi_mode.c
	$(CC) $(CFLAGS) -c vi_mode.c

xmalloc.o : xmalloc.c
	$(CC) $(CFLAGS) -c xmalloc.c
