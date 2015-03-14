# Programm zum Eliminieren von '#elif'-Präprozessor-Anweisungen in C-Programmen
# Bruno Haible 30.8.1991

# Methode:
# Erkenne die Präprozessor-Anweisungen #if, #else, #endif, #elif, #line.
# Bei '#if' wird ein neuer Zähler aufgemacht und auf 1 gesetzt.
# Bei '#elif' wird '#else #if' ausgegeben und der letzte Zähler um 1 erhöht.
# Bei '#endif' werden so viele '#endif's ausgegeben, wie der Zähler angibt.

#define local static
#define global
#define var
#define loop  while (1)
#define until(exp)  while (!(exp))
typedef unsigned char  uintB;
typedef unsigned short  uintW;
typedef unsigned long  uintL;
typedef int  boolean;
#define FALSE 0
#define TRUE 1

#include <stdio.h>

typedef unsigned char Char;

global int main(argc,argv)
  var int argc;
  var char** argv;
{ var FILE* infile = stdin;
  var FILE* outfile = stdout;
  #define get() getc(infile)
  #define put(x) putc(x,outfile)
  #define puts(x) fputs(x,outfile)
  register int c;
  var int line_number = 0;
  var int counterstack[1000];
  var int* counterptr = &counterstack[0];
  L1: # zu Beginn einer Zeile
      line_number++;
      c = get(); if (c==EOF) goto L3;
      if (!(c=='#'))
        { put(c); if (c=='\n') goto L1;
          L2a:# innerhalb einer Zeile
              c = get();
          L2: if (c==EOF) goto L3;
              put(c);
              if (c=='\n') goto L1; else goto L2a;
        }
      # innerhalb einer Präprozessor-Anweisung
      # Whitespace innerhalb der Zeile überlesen:
      #define read_whitespace()  \
        loop                                     \
          { put(c);                              \
            c = get();                           \
            if (c==EOF) goto L3;                 \
            if (!((c==' ') || (c=='\t'))) break; \
          }
      read_whitespace();
      # Token lesen:
      #define MAXTOKENLEN 1000
      #define alphanumericp(c)  \
        (((c>='0') && (c<='9')) || ((c>='A') && (c<='Z')) || ((c>='a') && (c<='z')) || (c=='_'))
      { var Char token_buffer[MAXTOKENLEN+1];
        var int token_length;
        #define flush_token_buffer()  \
          {var int i = 0; until (i==token_length) { put(token_buffer[i++]); } }
        #define read_token()  \
          { token_length = 0;                                      \
            while (alphanumericp(c) && (token_length<MAXTOKENLEN)) \
              { token_buffer[token_length++] = c; c = get(); }     \
          }
        read_token();
        if ((token_length >= 1) && (token_buffer[0]>='0') && (token_buffer[0]<='9'))
          goto line; # Zeilennummern-Angabe
        if ((token_length == 4)
            && (token_buffer[0]=='l')
            && (token_buffer[1]=='i')
            && (token_buffer[2]=='n')
            && (token_buffer[3]=='e')
           )
          # '#line' -Anweisung
          { flush_token_buffer();
            read_whitespace();
            read_token();
            line:
            if (token_length>0)
              { token_buffer[token_length] = '\0';
               {var int token_value = atoi(token_buffer);
                if (token_value>0)
                  { line_number = token_value - 1; } # Zeilennummer neu setzen
              }}
            flush_token_buffer();
            goto L2;
          }
        if (   ((token_length == 2)
                && (token_buffer[0]=='i')
                && (token_buffer[1]=='f')
               )
            || ((token_length == 5)
                && (token_buffer[0]=='i')
                && (token_buffer[1]=='f')
                && (token_buffer[2]=='d')
                && (token_buffer[3]=='e')
                && (token_buffer[4]=='f')
               )
            || ((token_length == 6)
                && (token_buffer[0]=='i')
                && (token_buffer[1]=='f')
                && (token_buffer[2]=='n')
                && (token_buffer[3]=='d')
                && (token_buffer[4]=='e')
                && (token_buffer[5]=='f')
               )
           )
          # '#if' oder '#ifdef' oder '#ifndef' -Anweisung
          { *++counterptr = 1; flush_token_buffer(); goto L2; }
        if ((token_length == 4)
            && (token_buffer[0]=='e')
            && (token_buffer[1]=='l')
            && (token_buffer[2]=='s')
            && (token_buffer[3]=='e')
           )
          # '#else' -Anweisung
          { if (counterptr == &counterstack[0])
              { fprintf(stderr,"#else ohne #if in Zeile %d\n",line_number); }
            flush_token_buffer(); goto L2;
          }
        if ((token_length == 5)
            && (token_buffer[0]=='e')
            && (token_buffer[1]=='n')
            && (token_buffer[2]=='d')
            && (token_buffer[3]=='i')
            && (token_buffer[4]=='f')
           )
          # '#endif' -Anweisung
          { flush_token_buffer();
            if (counterptr == &counterstack[0])
              { fprintf(stderr,"#endif ohne #if in Zeile %d\n",line_number); }
              else
              {var int count = *counterptr-- - 1;
               if (count > 0)
                 { sprintf(token_buffer,"#line %d\n",line_number);
                   do { put('\n'); puts(token_buffer); puts("#endif"); count--; }
                      until (count == 0);
              }  }
            goto L2;
          }
        if ((token_length == 4)
            && (token_buffer[0]=='e')
            && (token_buffer[1]=='l')
            && (token_buffer[2]=='i')
            && (token_buffer[3]=='f')
           )
          # '#elif' -Anweisung
          { if (counterptr == &counterstack[0])
              { fprintf(stderr,"#elif ohne #if in Zeile %d\n",line_number); }
              else
              { (*counterptr)++; }
            puts("else"); put('\n');
            sprintf(token_buffer,"#line %d\n",line_number);
            puts(token_buffer);
            puts("#if");
            goto L2;
          }
        flush_token_buffer();
        goto L2;
      }
  L3: # am File-Ende
  # Files schließen:
  fclose(infile);
  fclose(outfile);
  exit(0); # alles OK
}

