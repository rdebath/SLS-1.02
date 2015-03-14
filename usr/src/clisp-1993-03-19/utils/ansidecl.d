# Programm zum Umwandeln von Funktionsdeklarationen vom traditionellen Stil
# in die ANSI-Syntax bei nicht allzu übel geformten C-Programmen
# Bruno Haible 16.2.1993

# Ziel:
# 1. Token &! streichen.
# 2. Deklarationen
#           funname(vname1,...,vnamek)
#             sspec1 tspec1 vname1 ;
#             ...;
#             sspeck tspeck vnamek ;
#             {
#    umwandeln in
#           funname ( sspec1 tspec1 vname1 , ..., sspeck tspeck vnamek ) {
#    unter Beibehaltung aller Kommentare, Präprozessor-Kommandos usw.

# Methode:
# Mit Kenntnis der Begriffe "Präprozessor-Kommando", "Kommentar", "Token"
# wird nach den öffnenden geschweiften Klammern '{' des äußersten
# Schachtelungslevels gesucht, denen ein Strichpunkt ';' oder
# eine Klammer zu ')' unmittelbar vorangeht.
# Von dort aus wird (unter Mitzählen der Strichpunkte im äußersten
# Schachtelungslevel) die vorige Klammer zu ')' des äußersten Schachtelungs-
# levels gesucht. Die Strichpunkte werden in Kommata bzw. eine Klammer zu
# umgewandelt. Bis zur vorigen Klammer auf '(' des äußersten Schachtelungs-
# levels wird alles durch Leerstellen ersetzt.


#define MAXARGCOUNT  50  # maximale Anzahl Argumente einer Funktionsdefinition
#define MAXHEADERLEN  5000  # maximale Länge eines Funktionsdefinitions-Kopfes

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

local int in_char (void)
  { var int c = getc(infile);
    if (c=='\n') { input_line++; }
    return c;
  }

local int peek_char (void)
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
local void outbuffer_off (void)
  { if (out.mode==buffered)
      { var uintL index = 0;
        while (index < out.buffindex)
          { char_out(out.buffer[index]); index++; }
        out.mode = direct;
  }   }

# Output-Bufferung ausschalten und dabei an einer Stelle einen String einfügen:
local void outbuffer_off_insert (uintL insertpoint, char* insert)
  { if (out.mode==buffered)
      { var uintL index = 0;
        loop
          { if (index==insertpoint)
              { while (!(*insert==0)) { char_out(*insert++); } }
            if (index == out.buffindex) break;
            char_out(out.buffer[index]); index++;
          }
        out.mode = direct;
  }   }

# Output-Bufferung einschalten:
local void outbuffer_on (void)
  { if (out.mode==direct)
      { out.buffindex = 0;
        out.mode = buffered;
  }   }

# Character ausgeben:
local void out_char (int c)
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
local int next_char (void)
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
typedef struct { enum { eof, eol, ident, number, charconst, stringconst, sep, expr } type;
                 # falls Bufferung aktiv:
                 uintL startindex; # Startindex im Buffer
                 uintL endindex; # Endindex im Buffer
                 # bei sep (Operator/Separator):
                 uintB ch;
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
local Token nexttoken (boolean within_prep_directive)
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
          case '#':
            if (within_prep_directive)
              { goto separator; }
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
                  { next_char(); }
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
            loop
              { c = peek_char();
                if (   ((c>='0') && (c<='9'))
                    || ((c>='A') && (c<='Z')) || ((c>='a') && (c<='z'))
                    || (c=='_')
                   )
                  { next_char(); }
                  else
                  break;
              }
            token->type = ident; goto fertig;
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
local void handle_opening_token (Token token)
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
local Token next_balanced_token (Token start_token)
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
local void convert (void)
  { input_line = 1;
    out.mode = direct;
    open_braces.count = 0;
    restart1: # Hier out.mode=direct, neues Token lesen:
    tokens.index = 0; # Token-Tabelle leeren
   {var Token token = next_token(); # nächstes Token lesen
    restart2: # Hier out.mode=direct, neues Token gelesen.
    if ((token->type == sep) && (token->ch == '(')) # Klammer auf?
      # ja -> aufpassen:
      { handle_opening_token(token);
        outbuffer_on(); # Buffer einschalten
        # Es sollten nun k Identifier, getrennt durch k-1 Kommata, kommen:
        token = next_token();
        if ((token->type == sep) && (token->ch == ')')) # Klammer zu?
          # ja -> in eine leere Parameterliste evtl. 'void' einfügen:
          { var uintL insertpoint = token->startindex;
            handle_closing_token(token);
            token = next_token();
            if (!((token->type == sep) && (token->ch == '{'))) # geschweifte Klammer auf?
              { outbuffer_off(); goto restart2; }
            # token = '{'
            # Umwandeln: "void" einfügen.
            outbuffer_off_insert(insertpoint,"void");
            token = next_balanced_token(token); # Funktionsdefinition überlesen
          }
          else
          # nein -> nichtleere Parameterliste verarbeiten:
          { var Token param_names[MAXARGCOUNT];
            var Token param_comma[MAXARGCOUNT];
            var uintL param_count = 0;
            loop
              { if (param_count==MAXARGCOUNT) goto nix; # kein Platz mehr?
                if (!(token->type == ident)) # Identifier?
                  goto nix;
                param_names[param_count] = token; # ja -> abspeichern
                token = next_token();
                if (!(   ((token->type == sep) && (token->ch == ')')) # Klammer zu?
                      || ((token->type == sep) && (token->ch == ',')) # oder Komma?
                   ) )
                  goto nix;
                param_comma[param_count] = token; # ja -> abspeichern
                param_count++;
                if ((token->type == sep) && (token->ch == ')')) # Klammer zu?
                  break;
                token = next_token();
              }
            # token = ')'
            handle_closing_token(token);
            # Parameter-Deklarationen verarbeiten:
           {var Token paramdecl_semicolons[MAXARGCOUNT];
            var uintL paramdecl_count = 0;
            loop
              { token = next_token();
                if ((token->type == sep) && (token->ch == '(')) # Klammer auf?
                  # Entscheidung der Zweideutigkeit (Beispiel:
                  # " macro(macroarg) (x) int x; {los();} "
                  # -> zugunsten der Erkennung von "macro(macroarg)" und nicht
                  # "macro":
                  { outbuffer_off(); goto restart2; }
                if ((token->type == sep) && (token->ch == '{')) # geschweifte Klammer auf?
                  break;
                loop
                  { token = next_balanced_token(token);
                    if (token->type==eof) goto nix;
                    if ((token->type == sep) && (token->ch == ';')) # Semikolon?
                      break;
                    # Entscheidung der Zweideutigkeit (Beispiel:
                    # " foo(a,b) int a; bar(x,y) int x; int y; {los();} "
                    # -> zugunsten der Erkennung von "bar" und nicht "foo"):
                    token = next_token();
                    if ((token->type == sep) && (token->ch == '(')) # Klammer auf?
                      { outbuffer_off(); goto restart2; }
                  }
                if (paramdecl_count==MAXARGCOUNT) goto nix; # kein Platz mehr?
                paramdecl_semicolons[paramdecl_count] = token;
                paramdecl_count++;
              }
            # token = '{'
            if ((param_count == paramdecl_count) # gleichviele Variablen wie Deklarationen?
                && (out.mode==buffered) # und Buffer noch da?
               )
              # ja -> Modifikation durchführen:
              # Alle param_names und param_comma durch Leerstellen und
              # alle paramdecl_semicolons durch Komma bzw. Klammer zu
              # überschreiben:
              { do { param_count--;
                    {var Token name = param_names[param_count];
                     var uintL index = name->startindex;
                     var uintL endindex = name->endindex;
                     until (index==endindex) { out.buffer[index++] = ' '; }
                    }
                    {var Token comma = param_comma[param_count];
                     var uintL index = comma->startindex;
                     var uintL endindex = comma->endindex;
                     until (index==endindex) { out.buffer[index++] = ' '; }
                   }}
                   until (param_count==0);
                out.buffer[paramdecl_semicolons[--paramdecl_count]->startindex] = ')';
                until (paramdecl_count==0)
                  { out.buffer[paramdecl_semicolons[--paramdecl_count]->startindex] = ','; }
              }
            outbuffer_off();
            token = next_balanced_token(token); # Funktionsdefinition überlesen
          }}
      }
      else
      { nix: # keine umwandelbare Funktionsdefinition
        outbuffer_off();
        token = next_balanced_token(token);
      }
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

