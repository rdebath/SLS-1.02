# Programm zum Zusammenfassen von adjazenten Strings in C-Programmen
# Bruno Haible 6.9.1991

# Ziel:
# Adjazente Strings wie "abc" "def" in "abcdef" umwandeln.

# Methode:
# Mit Kenntnis der Begriffe "Präprozessor-Kommando", "Kommentar", "Token".
# Aufeinanderfolgende String-Tokens werden zusammengemerged.
# Kommentare können getrost weggeworfen werden, und auf Strings innerhalb von
# Präprozessor-Anweisungen braucht nicht geachtet zu werden, da dieser Pass
# nach dem C-Präprozessor stattfindet.


#define MAXSTRINGLEN  5000 /* maximale Länge eines Strings incl. Zwischenraum */

#define local static
#define global
#define var
#define loop  while (1)
#define until(exp)  while (!(exp))
#define elif  else if
typedef unsigned char  uintB;
typedef unsigned short  uintW;
typedef unsigned long  uintL;
typedef int  boolean;
#define FALSE 0
#define TRUE 1
#define NULL ((void*)0)

#include <stdio.h>

local FILE* infile;
local FILE* outfile;

# Input
# =====

local uintL input_line;

local int in_char ()
  { var int c = getc(infile);
    if (c=='\n') { input_line++; }
    return c;
  }

local int peek_char ()
  { var int c = getc(infile);
    if (!(c==EOF)) { ungetc(c,infile); }
    return c;
  }

# Output
# ======

# Output kann immer ein wenig gepuffert werden:
local struct { enum { direct, buffered } mode; # Output-Modus
               uintB buffer[MAXSTRINGLEN]; # Buffer
               uintL buffindex; # Index in den Buffer
             }
      out;

#define char_out(char)  putc(char,outfile)

# Output-Bufferung ausschalten:
local void outbuffer_off ()
  { if (out.mode==buffered)
      { var uintL index = 0;
        while (index < out.buffindex)
          { char_out(out.buffer[index]); index++; }
        out.mode = direct;
  }   }

# Output-Bufferung einschalten:
local void outbuffer_on ()
  { if (out.mode==direct)
      { out.buffindex = 0;
        out.mode = buffered;
  }   }

# Character ausgeben:
local void out_char (c)
  var char c;
  { if (out.mode==buffered)
      { if (out.buffindex < MAXSTRINGLEN)
          { out.buffer[out.buffindex++] = c; }
          else
          # Buffer voll -> Buffer abschalten
          { outbuffer_off(); char_out(c); }
      }
      else
      { char_out(c); }
  }

# lexikalische Analyse
# ====================

# Holt das nächste Character:
local int next_char ()
  { var int c = in_char();
    if (!(c==EOF))
      { out_char(c); } # c auch ausgeben
    return c;
  }

# Für unsere Zwecke brauchen ++ -> != usw. nicht als eigene Token betrachtet
# zu werden, wir kennen also nur: EOF, String-Konstanten, andere.
typedef enum { eof, eol, ident, number, charconst, stringconst, sep } Token;
#define MAXTOKENLEN 1000
local char token_buffer[MAXTOKENLEN+1]; # Inhalt des Tokens, falls ident oder number
local int token_length; # dazu die Länge
local uintL out_buffindex_stringstart; # out.buffindex beim Stringanfang

# Holt das nächste Token:
# (Innerhalb von Präprozessor-Direktiven zählt Zeilenende als eigenes Token,
# und '#' leitet keine verschachtelte Präprozessor-Direktive ein.)
local Token nexttoken (within_prep_directive)
  var boolean within_prep_directive;
  { restart:
    { var int c = next_char();
      switch (c)
        { case EOF:
            # EOF
            return eof;
          case ' ': case '\v': case '\t':
            # Whitespace. überlesen
            goto restart;
          case '\n':
            # Zeilenende
            if (within_prep_directive)
              { return eol; } # als Token zurück
              else
              { goto restart; } # überlesen
          case '\\':
            if (peek_char()=='\n')
              # Zeilenende nach '\'. überlesen
              { next_char(); goto restart; }
              else
              goto separator;
          case '/':
            if (peek_char() == '*')
              # Kommentar
              { next_char();
                loop { c = next_char();
                       if (c==EOF) { fprintf(stderr,"Unbeendeter Kommentar\n"); break; }
                       if ((c=='*') && (peek_char()=='/')) { next_char(); break; }
                     }
                goto restart;
              }
              else
              goto separator;
          case '*':
            if (peek_char() == '/')
              # illegales Kommentar-Ende
              { fprintf(stderr,"Kommentar-Ende außerhalb Kommentar in Zeile %lu\n",input_line); }
            goto separator;
          case '#':
            if (within_prep_directive)
              { goto separator; }
              else
              { # Präprozessor-Anweisung.
                # Bis Zeilenende oder EOF lesen.
                # Dabei aber '#line' - Anweisungen verarbeiten.
                var uintL new_input_line = 0; # vorerst
                var Token subtoken;
                subtoken = nexttoken(TRUE);
                if ((subtoken == eof) || (subtoken == eol)) goto end_directive;
                if (subtoken == number) goto line;
                if ((subtoken == ident)
                    && (token_length == 4)
                    && (token_buffer[0] == 'l')
                    && (token_buffer[1] == 'i')
                    && (token_buffer[2] == 'n')
                    && (token_buffer[3] == 'e')
                   )
                  { # '#line'-Anweisung -> erst noch weiterlesen:
                    subtoken = nexttoken(TRUE);
                    if ((subtoken == eof) || (subtoken == eol)) goto end_directive;
                    if (subtoken == number)
                      { line:
                        if (token_length>0)
                          { token_buffer[token_length] = '\0';
                           {var int token_value = atoi(token_buffer);
                            if (token_value>0)
                              { new_input_line = token_value; } # Zeilennummer neu setzen
                          }}
                  }   }
                loop
                  { subtoken = nexttoken(TRUE);
                    if ((subtoken == eof) || (subtoken == eol)) goto end_directive;
                  }
                end_directive:
                if (new_input_line>0) { input_line = new_input_line; }
                goto restart; # und überlesen
              }
          case '.':
            c = peek_char();
            if (!((c>='0') && (c<='9'))) goto separator;
          case '0': case '1': case '2': case '3': case '4':
          case '5': case '6': case '7': case '8': case '9':
            # Zahl. Weiterlesen, solange alphanumerisches Zeichen oder '.':
            { var char* ptr = &token_buffer[0];
              var uintL len = 0;
              loop
                { *ptr++ = c; len++;
                  c = peek_char();
                  if (((c>='0') && (c<='9'))
                      || ((c>='A') && (c<='Z')) || ((c>='a') && (c<='z'))
                      || (c=='.')
                     )
                    { next_char(); }
                    else
                    break;
                }
              token_length = len;
            }
            return number;
          case '\'':
            # Character-Konstante
            loop
              { c = next_char();
                if (c==EOF) { fprintf(stderr,"Unbeendete Character-Konstante"); break; }
                if (c=='\'') break;
                if (c=='\\') { c = next_char(); }
              }
            return  charconst;
          case '\"':
            # String-Konstante
            if (within_prep_directive)
              # String-Konstanten in Präprozessor-Direktiven unverändert lassen
              { loop
                  { c = next_char();
                    if (c==EOF) { fprintf(stderr,"Unbeendete String-Konstante"); break; }
                    if (c=='\"') break;
                    if (c=='\\') { c = next_char(); }
              }   }
              else
              # fürs Anhängen weiterer Strings vorbereiten
              { out_buffindex_stringstart = out.buffindex;
                loop
                  { c = peek_char();
                    if (c==EOF) { fprintf(stderr,"Unbeendete String-Konstante"); break; }
                    if (c=='\"') break;
                    next_char();
                    if (c=='\\') { c = next_char(); }
                  }
                outbuffer_on(); # Ab jetzt alles in den Buffer
                next_char(); # Anführungszeichen zu loswerden
              }
            return stringconst;
          case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
          case 'G': case 'H': case 'I': case 'J': case 'K': case 'L':
          case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R':
          case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
          case 'Y': case 'Z':
          case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
          case 'g': case 'h': case 'i': case 'j': case 'k': case 'l':
          case 'm': case 'n': case 'o': case 'p': case 'q': case 'r':
          case 's': case 't': case 'u': case 'v': case 'w': case 'x':
          case 'y': case 'z':
          case '_':
            # Identifier. alles alphanumerische überlesen.
            { var char* ptr = &token_buffer[0];
              var uintL len = 0;
              loop
                { *ptr++ = c; len++;
                  c = peek_char();
                  if (   ((c>='0') && (c<='9'))
                      || ((c>='A') && (c<='Z')) || ((c>='a') && (c<='z'))
                      || (c=='_')
                     )
                    { next_char(); }
                    else
                    break;
                }
              token_length = len;
            }
            return ident;
          default:
          separator:
            return sep;
  } }   }
#define next_token() nexttoken(FALSE)


int main ()
  { infile = stdin;
    outfile = stdout;
    input_line = 1; out.mode = direct;
    loop
      { # Hier ist out.mode = direct.
        var Token token = next_token();
        if (token==stringconst)
          # weitere Strings zu lesen versuchen:
          { var boolean something_merged = FALSE;
            loop
              { # Hier ist out.mode = buffered und out.buffindex = 1.
                token = next_token();
                if (!(token==stringconst)) break;
                if (out.mode==direct) break; # Buffer übergelaufen?
                # out.buffer enthält alles vom Anführungszeichen zu des 1. Strings
                # bis zum Anführungszeichen zu des 2. Strings.
                # Von 0 (= Index des Anführungszeichen zu des 1. Strings) bis
                # out_buffindex_stringstart-1 (= Index des Anführungszeichen auf
                # des 2. Strings) (incl.) wird der Bufferinhalt vergessen, von
                # out_buffindex_stringstart bis out.buffindex-2 (incl.) wird der
                # Inhalt sofort ausgegeben, und das Anführungszeichen zu des 2.
                # Strings wandert von Position buffindex-1 nach Position 0.
                { var uintL i;
                  for (i=out_buffindex_stringstart; i<out.buffindex-1; i++)
                    { char_out(out.buffer[i]); }
                }
                out.buffindex = 1; something_merged = TRUE;
              }
            outbuffer_off();
            if (something_merged)
              # neue Zeilennummer ausgeben:
              #if 0 # so ist ANSI-Standard:
              fprintf(outfile,"\n%s %lu\n","#line",input_line);
              #else # so gefällt es 'cc' besser:
              fprintf(outfile,"\n%s %lu\n","#",input_line);
              #endif
          }
          else
          { outbuffer_off();
            if (token==eof) break;
      }   }
    exit(0);
  }

