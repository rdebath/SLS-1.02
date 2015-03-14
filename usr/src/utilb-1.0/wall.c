/*
 * wall.c	Write to all users logged in.
 *
 * Version:	1.01
 *
 * Usage:	wall [text]
 *
 * Author:	Miquel van Smoorenburg, miquels@drinkel.nl.mugnet.org
 */

#include <string.h>
#include <stdio.h>

char *Version = "@(#) wall 1.01 18-11-1992 MvS";

extern void wall();

int main(argc, argv)
int argc;
char **argv;
{
  char buf[1024];
  char line[83];
  int f;
  int len = 0;
  char *p;
  
  buf[0] = 0;

  if (argc > 1) {
  	for(f = 1; f < argc; f++) {
  		len += strlen(argv[f]) + 1;
  		if (len > 1023) break;
  		strcat(buf, argv[f]);
  		strcat(buf, " ");
  	}
  	strcat(buf, "\r\n");
  } else {
  	while(fgets(line, 80, stdin)) {
  		/* Take care that line ends in \r\n */
  		for(p = line; *p && *p != '\n'; p++)
  			;
  		strcpy(p, "\r\n");
  		len += strlen(line);
  		if (len > 1023) break;
  		strcat(buf, line);
  	}
  }
  wall(buf);
  return(0);
}
