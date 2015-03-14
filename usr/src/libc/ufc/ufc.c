/*
 * UFC-crypt: ultra fast crypt(3) implementation
 *
 * Copyright (C) 1991, 1992, Michael Glad, email: glad@daimi.aau.dk
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * @(#)ufc.c	2.4 12/30/91
 *
 * Stub main program for debugging
 * and benchmarking.
 *
 */

#include <stdio.h>

char *crypt();

main(argc, argv)
  int argc;
  char **argv;
  { char *s;
    unsigned long i,iterations;

    if(argc != 2) {
      fprintf(stderr, "usage: ufc iterations\n");
      exit(1);
    }
    argv++;
    iterations = atoi(*argv);
    printf("ufc: running %d iterations\n", iterations);

    for(i=0; i<iterations; i++)
      s=crypt("foob","ar");
    if(strcmp(s, "arlEKn0OzVJn.") == 0)
      printf("OK\n");
    else {
      printf("wrong result: %s!!\n", s);
      exit(1);
    }
    exit(0);
  }
