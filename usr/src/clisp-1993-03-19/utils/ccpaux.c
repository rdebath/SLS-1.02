/* C-Programm-Präprozessor-Hilfe:
   Entfernt die Spaces und Tabs am Beginn jeder Zeile, die mit # anfängt.
   Bruno Haible 17.1.1991
*/

#include <stdio.h>

#define SPACE ' '
#define TAB '\t'
#define NL 10

void n_spaces (n) /* n Spaces ausgeben */
  int n;
  { while (!(n==0)) { putchar(SPACE); n--; } }

main ()
  { int c;
    int spacecount;
    zeilenanfang:
      spacecount=0;
      looking_for_space:
        c = getchar(); if (c==EOF) { n_spaces(spacecount); goto eof; }
        if (c==SPACE) { spacecount++; goto looking_for_space; }
        if (c==TAB)
          { do { spacecount++; } while (!((spacecount%8)==0));
            goto looking_for_space;
          }
      /* c ist kein Space */
      if (c=='#') { spacecount=0; }
      n_spaces(spacecount);
      /* Rest der Zeile unverändert übernehmen: */
      rest:
        putchar(c);
        if (c==NL) goto zeilenanfang;
        c = getchar(); if (c==EOF) { goto eof; }
        goto rest;
    eof: ;
    exit(0);
  }

