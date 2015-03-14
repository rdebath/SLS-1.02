/* C-Programm-Präprozessor-Hilfe:
   Ersetzt die #error-Anweisungen am Beginn jeder Zeile durch ERROR.
   Bruno Haible 4.11.1992
*/

#include <stdio.h>

#define NL 10

main ()
  { int c;
    zeilenanfang:
      c = getchar(); if (c==EOF) { goto eof; }
      if (c=='#')
        { c = getchar(); if (c==EOF) { putchar('#'); goto eof; }
          if (c=='e')
            { c = getchar(); if (c==EOF) { putchar('#'); putchar('e'); goto eof; }
              if (c=='r')
                { c = getchar(); if (c==EOF) { putchar('#'); putchar('e'); putchar('r'); goto eof; }
                  if (c=='r')
                    { c = getchar(); if (c==EOF) { putchar('#'); putchar('e'); putchar('r'); putchar('r'); goto eof; }
                      if (c=='o')
                        { c = getchar(); if (c==EOF) { putchar('#'); putchar('e'); putchar('r'); putchar('r'); putchar('o'); goto eof; }
                          if (c=='r')
                            { putchar('E'); putchar('R'); putchar('R'); putchar('O'); putchar('R');
                              c = getchar();
                            }
                            else
                            { putchar('#'); putchar('e'); putchar('r'); putchar('r'); putchar('o'); }
                        }
                        else
                        { putchar('#'); putchar('e'); putchar('r'); putchar('r'); }
                    }
                    else
                    { putchar('#'); putchar('e'); putchar('r'); }
                }
                else
                { putchar('#'); putchar('e'); }
            }
            else
            { putchar('#'); }
        }
      /* Rest der Zeile unverändert übernehmen: */
      rest:
        putchar(c);
        if (c==NL) goto zeilenanfang;
        c = getchar(); if (c==EOF) { goto eof; }
        goto rest;
    eof: ;
    exit(0);
  }

