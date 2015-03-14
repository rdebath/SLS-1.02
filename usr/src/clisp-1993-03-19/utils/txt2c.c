/* Wandelt ein Textfile mit eingestreuten #if - Bedingungen
   in ein C-Programm um, das dieses Textfile ausgibt.
   Bruno Haible 21.2.1993
*/

#include <stdio.h>

int main ()
  { int c;
#ifdef CROSS
    printf("#include \"clisp.h\"\n");
#else
    printf("#include \"lispbibl.c\"\n");
#endif
    printf("#include <stdio.h>\n");
    printf("\n");
    printf("int main () {\n");
    while (1)
      { c = getchar(); if (c==EOF) goto eof;
        if (c=='#')
          { c = getchar(); if (c==EOF) { putchar('#'); goto eof; }
            if ((c=='i') || (c=='e')) /* Heuristik für #if, #else, #endif */
              /* Zeile unverändert durchlassen */
              { putchar('#');
                while (1)
                  { putchar(c);
                    if (c=='\n') break;
                    c = getchar(); if (c==EOF) goto eof;
                  }
                goto line_ok;
              }
            printf("printf(\"#\");\n");
          }
        /* Zeile in eine printf()-Anweisung umwandeln */
        printf("  printf(\"");
        while (1)
          { if ((c=='\\') || (c=='\"'))
              { putchar('\\'); putchar(c); }
#if defined(sun) && !defined(__GNUC__)
            else if (c==0377)
              { putchar('\\'); putchar(c); }
#endif
#ifdef QUOTE_QUOTES
            else if (c=='\'')
              { # statt "'" ein "\047" ausgeben:
                out_char('\\');
                out_char('0'+((((unsigned char)'\'')/64)%8));
                out_char('0'+((((unsigned char)'\'')/8)%8));
                out_char('0'+(((unsigned char)'\'')%8));
              }
#endif
            else if (c=='%')
              { putchar(c); putchar(c); }
            else if (c!='\n')
              { putchar(c); }
            else
              { putchar('\\'); putchar('n'); break; }
            c = getchar(); if (c==EOF) { printf("\");\n"); goto eof; }
          }
        printf("\");\n");
        line_ok: ;
      }
    eof:
    printf("  exit(0);\n");
    printf("}\n");
    exit(0);
  }

