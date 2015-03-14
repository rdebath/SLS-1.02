# program to play the game of Go (Wei-Chi)

SRC = count.c countlib.c endgame.c eval.c exambord.c findcolr.c \
      findnext.c findopen.c findpatn.c findsavr.c findwinr.c \
      fioe.c genmove.c getij.c getmove.c initmark.c main.c matchpat.c \
      opening.c openregn.c random.c seed.c sethand.c \
      showbord.c showinst.c suicide.c

PRG = gnugo

OBJ = $(SRC:.c=.o)

CFLAGS = -O

$(PRG) : $(OBJ)
	$(LINK.c) $(OBJ) -o $@

matchpat.o : patterns.c
