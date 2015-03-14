#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <stdio.h>

#define Dynamic 1
#define MAXCOLORS 256
#define MAXCMD 128

typedef unsigned char Byte;
typedef unsigned long Pixel;
typedef char *String;

String *index();
XImage *ProcessImage();
char *calloc(), *malloc();
unsigned long DataSize();
