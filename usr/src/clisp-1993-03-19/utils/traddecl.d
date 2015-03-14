# Programm zum Umwandeln von Funktionsdeklarationen von der ANSI-Syntax in
# den traditionellen Stil bei nicht allzu übel geformten C-Programmen
# Bruno Haible 16.2.1993

# Ziel:
# 1. Tokens ## umwandeln in / * * /
# 2. Zahl-Token-Endung 'UL' oder 'LU' in 'L' umwandeln.
# 3. Token &! streichen.
# 4. Deklarationen
#        storspec ... tspec (arg1,...,argk);
#    umwandeln in
#        storspec ... tspec ();
#    wobei storspec eines der Wörter 'typedef', 'extern', 'local', 'global'.

# Methode:
# Mit Kenntnis der Begriffe "Präprozessor-Kommando", "Kommentar", "Token".
# 1. Zwei aufeinanderfolgende '#' werden durch '/ * * /' ersetzt.
# 2. Aus 'LU' oder 'UL' am Zahl-Token-Ende das 'U' streichen.
# 3. Aufeinanderfolgende '&' und '!' werden gestrichen.
# 4. Es wird nach typedef-Tokens gesucht. Kommt vor dem nächsten Strichpunkt
# ';' des äußersten Schachtelungslevels eine Klammer zu ')', so bis zur
# vorigen Klammer auf '(' des äußersten Schachtelungslevels alles durch
# Leerstellen ersetzt.


#define MAXARGCOUNT  50  # maximale Anzahl Argumente einer Funktionsdefinition
#define MAXHEADERLEN  5000  # maximale Länge eines Funktionsdefinitions-Kopfes

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

#if defined(unix) || defined(__unix)
#include "unixconf.h"
#endif
#ifdef __TURBOC__
#define STDC_HEADERS 1
#endif
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif

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
               uintB buffer[MAXHEADERLEN]; # Buffer
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
      { if (out.buffindex < MAXHEADERLEN)
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
# zu werden, wir kennen also nur:
#   EOF
#   Identifier
#   Zahl-Konstanten
#   Character-Konstanten
#   String-Konstanten
#   Operator/Separator
# Verallgemeinerte Tokens: Expressions (mit balancierten Klammern)
#define MAXIDENTLEN  7
typedef struct { enum { eof, eol, ident, number, charconst, stringconst, sep, expr } type;
                 # falls Bufferung aktiv:
                 uintL startindex; # Startindex im Buffer
                 uintL endindex; # Endindex im Buffer
                 # bei sep (Operator/Separator):
                 uintB ch;
                 # bei ident (Identifier):
                 uintL len; # = min(Länge,MAXIDENTLEN+1)
                 uintB name[MAXIDENTLEN+1];
               }
        token_;
typedef token_* Token;

# globale Token-Tabelle (Inhalt nur während einer Bufferungsperiode gültig):
#define MAXTOKENS  20000
local struct { uintL index;
               token_ data[MAXTOKENS];
             }
      tokens;

# Holt das nächste Token:
# (Innerhalb von Präprozessor-Direktiven zählt Zeilenende als eigenes Token,
# und '#' leitet keine verschachtelte Präprozessor-Direktive ein.)
local Token nexttoken (within_prep_directive)
  var boolean within_prep_directive;
  { if (tokens.index == MAXTOKENS)
      # kein Platz mehr in der Token-Tabelle -> nicht mehr buffern
      { outbuffer_off(); tokens.index = 0;
        fprintf(stderr,"Token-Tabelle übergelaufen in Zeile %d.\n",input_line);
        exit(1);
      }
    # Nun ist tokens.index < MAXTOKENS .
   {var Token token = &tokens.data[tokens.index]; # Platz fürs nächste Token
    tokens.index++;
    restart:
    token->startindex = out.buffindex;
#ifndef FOR_ANSI_PREPROCESSOR
    if (peek_char() == '#')
      { in_char(); # erstes '#' überlesen
        if (peek_char() == '#')
          # '##'
          { in_char(); # zweites '#' überlesen
            # stattdessen '/ * * /' durchreichen:
            out_char('/'); out_char('*'); out_char('*'); out_char('/');
            token->type = sep; token->ch = '#'; goto fertig;
          }
          else
          # auch ausgeben, normal weiter
          { out_char('#'); goto prep_directive; }
      }
#endif
    if (peek_char() == '&')
      { in_char(); # '&' überlesen
        if (peek_char() == '!')
          # '&!'
          { in_char(); goto restart; } # '!' überlesen
          else
          { out_char('&'); token->type = sep; token->ch = '&'; goto fertig; }
      }
    { var int c = next_char();
      switch (c)
        { case EOF:
            # EOF
            token->type = eof; goto fertig;
          case ' ': case '\v': case '\t':
            # Whitespace. überlesen
            goto restart;
          case '\n':
            # Zeilenende
            if (within_prep_directive)
              { token->type = eol; goto fertig; } # als Token zurück
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
          case '#': prep_directive:
            if (within_prep_directive)
              { c = '#'; goto separator; }
              else
              { # Präprozessor-Anweisung.
                # Bis Zeilenende oder EOF lesen.
                loop
                  { var Token subtoken = nexttoken(TRUE);
                    if ((subtoken->type == eof) || (subtoken->type == eol))
                      break;
                  }
                goto restart; # und überlesen
              }
          case '.':
            c = peek_char();
            if (!(((c>='0') && (c<='9')) || (c=='.'))) goto separator;
          case '0': case '1': case '2': case '3': case '4':
          case '5': case '6': case '7': case '8': case '9':
            # Zahl. Weiterlesen, solange alphanumerisches Zeichen oder '.':
            loop
              { c = peek_char();
                if (((c>='0') && (c<='9'))
                    || ((c>='A') && (c<='Z')) || ((c>='a') && (c<='z'))
                    || (c=='.')
                   )
                  { if ((c=='U') || (c=='u')
#ifdef CUT_U_AND_L
                        || (c=='L') || (c=='l')
#endif
                       )
                      in_char(); # 'U' innerhalb einer Zahl streichen
                      else
                      next_char();
                  }
                  else
                  break;
              }
            token->type = number; goto fertig;
          case '\'':
            # Character-Konstante
            loop
              { c = next_char();
                if (c==EOF) { fprintf(stderr,"Unbeendete Character-Konstante"); break; }
                if (c=='\'') break;
                if (c=='\\') { c = next_char(); }
              }
            token->type = charconst; goto fertig;
          case '\"':
            # String-Konstante
            loop
              {
#ifndef QUOTE_QUOTES
                c = next_char();
                if (c==EOF) { fprintf(stderr,"Unbeendete String-Konstante"); break; }
#else # muß Single-Quotes in Strings quotieren:
                c = in_char();
                if (c==EOF) { fprintf(stderr,"Unbeendete String-Konstante"); break; }
                if (c=='\'')
                  { # statt "'" ein "\047" ausgeben:
                    out_char('\\');
                    out_char('0'+((((unsigned char)'\'')/64)%8));
                    out_char('0'+((((unsigned char)'\'')/8)%8));
                    out_char('0'+(((unsigned char)'\'')%8));
                    continue;
                  }
                out_char(c);
#endif
                if (c=='\"') break;
                if (c=='\\') { c = next_char(); }
              }
            token->type = stringconst; goto fertig;
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
            { var uintB* ptr = &token->name[0];
              var uintL len = 0;
              loop
                { if (len <= MAXIDENTLEN) { *ptr++ = c; len++; }
                  c = peek_char();
                  if (   ((c>='0') && (c<='9'))
                      || ((c>='A') && (c<='Z')) || ((c>='a') && (c<='z'))
                      || (c=='_')
                     )
                    { next_char(); }
                    else
                    break;
                }
              token->type = ident; token->len = len; goto fertig;
            }
          default:
          separator:
            token->type = sep; token->ch = c; goto fertig;
    }   }
    fertig:
    token->endindex = out.buffindex;
    return token;
  }}
#define next_token() nexttoken(FALSE)

# Klammern mitzählen:
#define MAXBRACES 1000 # maximale Verschachtelungstiefe von Klammern
local struct { uintL count;
               struct { uintB brace_type; uintL input_line; } opening[MAXBRACES];
             }
      open_braces;

# Mitzählen einer öffnenden Klammer:
local void handle_opening_token (token)
  var Token token;
  { if (open_braces.count < MAXBRACES)
      { open_braces.opening[open_braces.count].brace_type = token->ch;
        open_braces.opening[open_braces.count].input_line = input_line;
      }
    open_braces.count++;
  }

# Mitzählen einer schließenden Klammer (ohne Überprüfung der Verschachtelung):
# local void handle_closing_token (Token token)
#   { open_braces.count--; }
# oder als Macro
#define handle_closing_token(token)  { open_braces.count--; }

# nächste Expression mit balancierten Klammern '()', '{}', '[]' lesen:
# (Dabei ist auf das Niveau open_braces.count=0 zu kommen,
#  evtl. ist jetzt schon open_braces.count>0.)
local Token next_balanced_token (start_token)
  var Token start_token;
  { var uintL open_braces_start = 0; # Ziel-Niveau: 0
    var Token token = (start_token==NULL ? next_token() : start_token);
    var uintL startindex = token->startindex;
    loop
      { # Hier stets  open_braces.count >= open_braces_start .
        switch (token->type)
          { case eof:
              if (open_braces.count > open_braces_start)
                { if (open_braces.count <= MAXBRACES)
                    fprintf(stderr,"Nicht geschlossene '%c' in Zeile %lu\n",
                                   open_braces.opening[open_braces.count-1].brace_type,
                                   open_braces.opening[open_braces.count-1].input_line
                           );
                    else
                    fprintf(stderr,"Nicht geschlossene '(' oder '{' oder '['\n");
                }
              return token; # EOF-Token als Ergebnis
            case sep:
              switch (token->ch)
                { case '(': case '{': case '[':
                    handle_opening_token(token);
                    break;
                  case ')': case '}': case ']':
                    if (open_braces.count > open_braces_start)
                      { open_braces.count--;
                        if (open_braces.count < MAXBRACES)
                          { var uintB opening_ch = open_braces.opening[open_braces.count].brace_type;
                            var uintB closing_ch = token->ch;
                            if (!(   ((opening_ch == '(') && (closing_ch == ')'))
                                  || ((opening_ch == '{') && (closing_ch == '}'))
                                  || ((opening_ch == '[') && (closing_ch == ']'))
                               ) )
                              { fprintf(stderr,"Öffnende Klammer '%c' in Zeile %lu\n und schließende Klammer '%c'\n in Zeile %lu passen nicht zusammen.\n",
                                        opening_ch,open_braces.opening[open_braces.count].input_line,
                                        closing_ch,input_line
                                       );
                          }   }
                      }
                      else
                      { fprintf(stderr,"Nicht geöffnete '%c' in Zeile %lu\n",
                                token->ch,input_line
                               );
                        goto fertig;
                      }
                    break;
                  default:
                    break;
                }
            default: ;
              # alles andere ist ausbalanciert
          }
        if (open_braces.count == open_braces_start) break; # fertig ausbalanciert?
        token = next_token(); # nein -> nächstes Token lesen
      }
    fertig:
    token->startindex = startindex;
    return token;
  }

# Umwandlung der Funktionsdefinitions-Deklarationen auf dem ganzen File
# vornehmen:
local void convert ()
  { input_line = 1;
    out.mode = direct;
    open_braces.count = 0;
    restart1: # Hier out.mode=direct, neues Token lesen:
    tokens.index = 0; # Token-Tabelle leeren
   {var Token token = next_token(); # nächstes Token lesen
    restart2: # Hier out.mode=direct, neues Token gelesen.
    if ((token->type == ident)
        && (   ((token->len == 7) # 'typedef' ?
                && (token->name[0] == 't')
                && (token->name[1] == 'y')
                && (token->name[2] == 'p')
                && (token->name[3] == 'e')
                && (token->name[4] == 'd')
                && (token->name[5] == 'e')
                && (token->name[6] == 'f')
               )
            || ((token->len == 6) # 'extern' ?
                && (token->name[0] == 'e')
                && (token->name[1] == 'x')
                && (token->name[2] == 't')
                && (token->name[3] == 'e')
                && (token->name[4] == 'r')
                && (token->name[5] == 'n')
               )
            || ((token->len == 5) # 'local' ?
                && (token->name[0] == 'l')
                && (token->name[1] == 'o')
                && (token->name[2] == 'c')
                && (token->name[3] == 'a')
                && (token->name[4] == 'l')
               )
            || ((token->len == 6) # 'global' ?
                && (token->name[0] == 'g')
                && (token->name[1] == 'l')
                && (token->name[2] == 'o')
                && (token->name[3] == 'b')
                && (token->name[4] == 'a')
                && (token->name[5] == 'l')
               )
       )   )
      # ja -> aufpassen:
      { outbuffer_on(); # Buffer einschalten
        # Ein Attribut nach dem anderen lesen, bis zum Strichpunkt.
        # Dabei speichern, ob das letzte von Klammern begrenzt war.
        { var boolean last_parenthesized; # ob letztes Attribut geklammert war
          var uintL last_startindex; # Falls ja: Index nach der Klammer auf
          var uintL last_endindex; # Falls ja: Index vor der Klammer zu
          last_parenthesized = FALSE;
          token = next_token();
          loop
            { if (token->type == eof) goto nix; # EOF?
              elif ((token->type == sep) && (token->ch == '(')) # Klammer auf?
                { last_parenthesized = TRUE;
                  last_startindex = out.buffindex;
                  token = next_balanced_token(token); # Attribut überlesen
                  if ((out.mode==buffered) && (out.buffer[out.buffindex-1] == ')')) # sollte mit ')' enden
                    { last_endindex = out.buffindex-1; }
                    else
                    { last_parenthesized = FALSE; } # sonst war's wohl nix
                  # Wenn's jetzt mit '{' weitergeht, ist das wohl eine
                  # Funktionsdefinition und keine Funktionsdeklaration:
                  token = next_token();
                  if ((token->type == sep) && (token->ch == '{')) goto nix;
                }
              elif ((token->type == sep) && (token->ch == ';')) # Strichpunkt?
                break;
              else
                { token = next_balanced_token(token); # Attribut überlesen
                  last_parenthesized = FALSE;
                  token = next_token();
                }
            }
          # Am Strichpunkt angelangt
          if (last_parenthesized)
            # Teilstück des Buffers durch Leerstellen überschreiben:
            { var uintL index = last_startindex;
              until (index==last_endindex) { out.buffer[index++] = ' '; }
            }
        }
        outbuffer_off(); # Buffer wieder ausschalten
      }
    else
      { nix: # keine umwandelbare Funktionsdeklaration
        outbuffer_off();
        token = next_balanced_token(token);
      }
    end:
    # Hier ist wieder out.mode=direct.
    if (!(token->type==eof)) goto restart1;
    # Bei EOF am Input: fertig (da outfile ungebuffert).
  }}

int main ()
  { infile = stdin;
    outfile = stdout;
    convert();
    exit(0);
  }

