CC = cc
CFLAGS = 
PROGRAM = xcolormap 
OBJS = xcolormap.o

LIBS = -lm -lXt -lX11

all:	$(PROGRAM) 

$(PROGRAM) :  $(OBJS)
	      $(CC) -o $@ $(OBJS) $(LIBS)

clean  : 
	 rm -f $(OBJS) $(PROGRAM)
