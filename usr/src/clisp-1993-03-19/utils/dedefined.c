/* Programm zum Eliminieren von defined(...)-Ausdrücken in #if -
   Präprozessor-Anweisungen in C-Programmen, für VAX C.
   Bruno Haible 26.2.1993
*/

/* Methode:
   Erkenne die Präprozessor-Anweisung #if.
   Der folgende Ausdruck wird in seine Bestandteile zerlegt. Liefert einen
   Syntaxbaum. Für jeden Teilbaum, der einen defined(...)-Ausdruck enthält,
   wird eine temporäre Präprozessor-Variable eingeführt.
   Die Ausgabe besteht aus "#undef tmp_var_xx" zu Beginn, dann kurzen
   #if's bzw. #ifdef's für jeden nächstumfangreicheren Teilbaum.

   Dieser Präprozessor setzt deelif.d voraus.
*/

/* Noch zu tun: #line-Anweisungen erkennen und selber emittieren. */

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
#include <stdlib.h> /* für malloc(), realloc(), exit() */
#include <setjmp.h> /* für setjmp(), longjmp() */

/* Einfache Ein-/Ausgaben: */

local var FILE* infile;
local var FILE* outfile;

#define get() getc(infile)
#define put(x) putc(x,outfile)
#define puts(x) fputs(x,outfile)

#define NL 10

/* Es wird gebuffert: */

local char* buffer;
local uintL bufsize;
local char* bufend;

local void putbuffer(c)
  var char c;
  { if (bufend == buffer+bufsize)
      { buffer = realloc(buffer,2*bufsize);
        bufend = buffer+bufsize;
        bufsize = 2*bufsize;
      }
    *bufend++ = c;
  }

/* Syntax-Analyse im Buffer zwischen buffer und bufend: */

local boolean whitespacep(c)
  var char c;
  { return (c==' ') || (c=='\v') || (c=='\t'); }

/* Beenden der Analyse */
local jmp_buf abort_line_jmpbuf;
local void abort_line()
  { longjmp(abort_line_jmpbuf,1); }

/* Syntax konstanter C-Expressions:
   16  name
   16  defined(...)
   16  f(...)
   14  ~ ...
   14  ! ...
   14  - ...
   13L * / %
   12L + -
   11L << >>
   10L < > <= >=
    9L == !=
    8L &
    7L ^
    6L |
    5L &&
    4L ||
    3R ? :
    1L ,
*/

/* Lexikalische Analyse: */
typedef enum { TT_ident, /* identifier */
               TT_number, /* number */
               TT_lparen, /* ( */
               TT_rparen, /* ) */
               TT_plus, /* + */
               TT_minus, /* - */
               TT_mul, /* * */
               TT_div, /* / */
               TT_mod, /* % */
               TT_lshift, /* << */
               TT_rshift, /* >> */
               TT_less, /* < */
               TT_greater, /* > */
               TT_less_equal, /* <= */
               TT_greater_equal, /* >= */
               TT_equal, /* == */
               TT_not_equal, /* != */
               TT_not, /* ~ */
               TT_and, /* & */
               TT_or, /* | */
               TT_xor, /* ^ */
               TT_lognot, /* ! */
               TT_logand, /* && */
               TT_logor, /* || */
               TT_if, /* ? */
               TT_else, /* : */
               TT_comma, /* , */
               TT_eof
             }
        tokentype;
typedef struct { tokentype type; /* Typ */
                 char* startptr; char* endptr; /* Pointer in den Buffer */
               }
        token_;
typedef token_* Token;
local token_ token_eof = {TT_eof,NULL,NULL};
#define Token_EOF  &token_eof

local Token make_token(type,startptr,endptr)
  var tokentype type;
  var char* startptr;
  var char* endptr;
  { var Token token = malloc(sizeof(token_));
    token->type = type;
    token->startptr = startptr;
    token->endptr = endptr;
    return token;
  }

local void free_token(token)
  var Token token;
  { free(token); }

/* Pointer in den Buffer, so weit die lexikalische Analyse gelangt ist. */
local char* bufptr;

local int peek_char()
  { if (bufptr==bufend) return EOF; else return *bufptr; }

local int next_char()
  { if (bufptr==bufend) return EOF; else return *bufptr++; }

/* Holt das nächste Token, rückt bufptr weiter: */
local Token nexttoken()
  { var int c;
    restart:
   {var char* startptr = bufptr;
    c = next_char();
    switch (c)
      { case ' ': case '\v': case '\t':
          goto restart; /* Whitespace überlesen */
        case '/':
          if (peek_char() == '*')
            /* Kommentar */
            { next_char();
              loop
                { c = next_char();
                  if (c==EOF) { fprintf(stderr,"Unbeendeter Kommentar\n"); abort_line(); }
                  if ((c=='*') && (peek_char()=='/')) { next_char(); break; }
                }
              goto restart;
            }
            else
            return make_token(TT_div,startptr,bufptr);
        case '*':
          if (peek_char() == '/')
            /* illegales Kommentar-Ende */
            { fprintf(stderr,"Kommentar-Ende außerhalb Kommentar\n"); abort_line(); }
          return make_token(TT_mul,startptr,bufptr);
        case '(': return make_token(TT_lparen,startptr,bufptr);
        case ')': return make_token(TT_rparen,startptr,bufptr);
        case '+': return make_token(TT_plus,startptr,bufptr);
        case '-': return make_token(TT_minus,startptr,bufptr);
        case '%': return make_token(TT_mod,startptr,bufptr);
        case '<':
          c = peek_char();
          if (c=='<') { next_char(); return make_token(TT_lshift,startptr,bufptr); }
          if (c=='=') { next_char(); return make_token(TT_less_equal,startptr,bufptr); }
          return make_token(TT_less,startptr,bufptr);
        case '>':
          c = peek_char();
          if (c=='>') { next_char(); return make_token(TT_rshift,startptr,bufptr); }
          if (c=='=') { next_char(); return make_token(TT_greater_equal,startptr,bufptr); }
          return make_token(TT_greater,startptr,bufptr);
        case '=':
          c = peek_char();
          if (c=='=') { next_char(); return make_token(TT_equal,startptr,bufptr); }
          abort_line();
        case '~': return make_token(TT_not,startptr,bufptr);
        case '!':
          c = peek_char();
          if (c=='=') { next_char(); return make_token(TT_not_equal,startptr,bufptr); }
          return make_token(TT_lognot,startptr,bufptr);
        case '&':
          c = peek_char();
          if (c=='&') { next_char(); return make_token(TT_logand,startptr,bufptr); }
          return make_token(TT_and,startptr,bufptr);
        case '|':
          c = peek_char();
          if (c=='|') { next_char(); return make_token(TT_logor,startptr,bufptr); }
          return make_token(TT_or,startptr,bufptr);
        case '^': return make_token(TT_xor,startptr,bufptr);
        case '?': return make_token(TT_if,startptr,bufptr);
        case ':': return make_token(TT_else,startptr,bufptr);
        case ',': return make_token(TT_comma,startptr,bufptr);
        case '.':
          c = peek_char();
          if (!(((c>='0') && (c<='9')) || (c=='.'))) { abort_line(); }
        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
          /* Zahl. Weiterlesen, solange alphanumerisches Zeichen oder '.': */
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
          return make_token(TT_number,startptr,bufptr);
        case '\'':
          /* Character-Konstante */
          loop
            { c = next_char();
              if (c==EOF) { fprintf(stderr,"Unbeendete Character-Konstante"); abort_line(); }
              if (c=='\'') break;
              if (c=='\\') { c = next_char(); }
            }
          return make_token(TT_number,startptr,bufptr);
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
        case '_': case '$':
          /* Identifier. alles alphanumerische überlesen. */
          loop
            { c = peek_char();
              if (   ((c>='0') && (c<='9'))
                  || ((c>='A') && (c<='Z')) || ((c>='a') && (c<='z'))
                  || (c=='_') || (c=='$')
                 )
                { next_char(); }
                else
                break;
            }
          return make_token(TT_ident,startptr,bufptr);
        case EOF:
          return Token_EOF;
        default:
          abort_line();
      }
  }}

local void print_token(token)
  var Token token;
  { var char* ptr = token->startptr;
    var char* endptr = token->endptr;
    until (ptr==endptr) { char c = *ptr++; put(c); }
  }

/* Ausdrucks-Analyse: */
typedef enum { ET_literal, /* identifier, number, literal */
               ET_paren, /* (expr) */
               ET_call, /* expr1(expr2) */
               ET_unary, /* ~ expr, ! expr, - expr */
               ET_mulop, /* expr1 * expr2, expr1 / expr2, expr1 % expr2 */
               ET_addop, /* expr1 + expr2, expr1 - expr2 */
               ET_shiftop, /* expr1 << expr2, expr1 >> expr2 */
               ET_cmpop, /* expr1 < expr2, expr1 > expr2 etc. */
               ET_relop, /* expr1 == expr2, expr1 != expr2 */
               ET_andop, /* expr1 & expr2 */
               ET_xorop, /* expr1 ^ expr2 */
               ET_orop, /* expr1 | expr2 */
               ET_logandop, /* expr1 && expr2 */
               ET_logorop, /* expr1 || expr2 */
               ET_condop, /* expr1 ? expr2 : expr3 */
               ET_seqop, /* expr1 , expr2 */
               ET_defined /* DEFINED_xxx */
             }
        exprtype;
typedef struct expr_ * Expr;
typedef struct expr_ { exprtype type; /* Typ */
                       Token token1, token2;
                       Expr expr1, expr2, expr3;
                       uintL counter;
                     }
        expr_;

local Expr make_expr(type,token1,token2,expr1,expr2,expr3)
  var exprtype type;
  var Token token1;
  var Token token2;
  var Expr expr1;
  var Expr expr2;
  var Expr expr3;
  { var Expr expr = malloc(sizeof(expr_));
    expr->type = type;
    expr->token1 = token1;
    expr->token2 = token2;
    expr->expr1 = expr1;
    expr->expr2 = expr2;
    expr->expr3 = expr3;
    return expr;
  }

local void free_expr(expr)
  var Expr expr;
  { if (expr->token1) free_token(expr->token1);
    if (expr->token2) free_token(expr->token2);
    if (expr->expr1) free_expr(expr->expr1);
    if (expr->expr2) free_expr(expr->expr2);
    if (expr->expr3) free_expr(expr->expr3);
    free(expr);
  }

local Token last_token;

local Token peek_token()
  { if (last_token) return last_token;
    return last_token = nexttoken(); /* Token_EOF bedeutet EOF */
  }

local Token next_token()
  { if (last_token)
      { var Token result = last_token; last_token = NULL; return result; }
    return nexttoken(); /* Token_EOF bedeutet EOF */
  }

local Expr parse_expr();

local Expr parse_expr_16()
  { var Token token = next_token();
    switch (token->type)
      { case TT_ident:
          { var Expr expr = make_expr(ET_literal, token,NULL, NULL,NULL,NULL);
            var Token token2 = peek_token();
            if (token2->type==TT_lparen)
              { next_token();
               {var Expr expr2 = parse_expr();
                var Token token3 = next_token();
                if (!(token3->type==TT_rparen)) abort_line();
                return make_expr(ET_call, token2,token3, expr,expr2,NULL);
              }}
              else
              return expr;
          }
        case TT_number:
          return make_expr(ET_literal, token,NULL, NULL,NULL,NULL);
        case TT_lparen:
          { var Expr expr = parse_expr();
            var Token token2 = next_token();
            if (!(token2->type==TT_rparen)) abort_line();
            return make_expr(ET_paren, token,token2, expr,NULL,NULL);
          }
        default:
          abort_line();
  }   }

local Expr parse_expr_14()
  { var Token token = peek_token();
    switch (token->type)
      { case TT_not:
        case TT_lognot:
        case TT_minus:
          next_token();
          { var Expr expr = parse_expr_14();
            return make_expr(ET_unary, token,NULL, expr,NULL,NULL);
          }
        default:
          return parse_expr_16();
  }   }

local Expr parse_expr_13()
  { var Expr expr = parse_expr_14();
    loop
      { var Token token = peek_token();
        switch (token->type)
          { default:
              return expr;
            case TT_mul:
            case TT_div:
            case TT_mod:
              next_token();
              expr = make_expr(ET_mulop, token,NULL, expr,parse_expr_14(),NULL);
      }   }
  }

local Expr parse_expr_12()
  { var Expr expr = parse_expr_13();
    loop
      { var Token token = peek_token();
        switch (token->type)
          { default:
              return expr;
            case TT_plus:
            case TT_minus:
              next_token();
              expr = make_expr(ET_addop, token,NULL, expr,parse_expr_13(),NULL);
      }   }
  }

local Expr parse_expr_11()
  { var Expr expr = parse_expr_12();
    loop
      { var Token token = peek_token();
        switch (token->type)
          { default:
              return expr;
            case TT_lshift:
            case TT_rshift:
              next_token();
              expr = make_expr(ET_shiftop, token,NULL, expr,parse_expr_12(),NULL);
      }   }
  }

local Expr parse_expr_10()
  { var Expr expr = parse_expr_11();
    loop
      { var Token token = peek_token();
        switch (token->type)
          { default:
              return expr;
            case TT_less:
            case TT_greater:
            case TT_less_equal:
            case TT_greater_equal:
              next_token();
              expr = make_expr(ET_cmpop, token,NULL, expr,parse_expr_11(),NULL);
      }   }
  }

local Expr parse_expr_9()
  { var Expr expr = parse_expr_10();
    loop
      { var Token token = peek_token();
        switch (token->type)
          { default:
              return expr;
            case TT_equal:
            case TT_not_equal:
              next_token();
              expr = make_expr(ET_relop, token,NULL, expr,parse_expr_10(),NULL);
      }   }
  }

local Expr parse_expr_8()
  { var Expr expr = parse_expr_9();
    loop
      { var Token token = peek_token();
        switch (token->type)
          { default:
              return expr;
            case TT_and:
              next_token();
              expr = make_expr(ET_andop, token,NULL, expr,parse_expr_9(),NULL);
      }   }
  }

local Expr parse_expr_7()
  { var Expr expr = parse_expr_8();
    loop
      { var Token token = peek_token();
        switch (token->type)
          { default:
              return expr;
            case TT_xor:
              next_token();
              expr = make_expr(ET_xorop, token,NULL, expr,parse_expr_8(),NULL);
      }   }
  }

local Expr parse_expr_6()
  { var Expr expr = parse_expr_7();
    loop
      { var Token token = peek_token();
        switch (token->type)
          { default:
              return expr;
            case TT_or:
              next_token();
              expr = make_expr(ET_orop, token,NULL, expr,parse_expr_7(),NULL);
      }   }
  }

local Expr parse_expr_5()
  { var Expr expr = parse_expr_6();
    loop
      { var Token token = peek_token();
        switch (token->type)
          { default:
              return expr;
            case TT_logand:
              next_token();
              expr = make_expr(ET_logandop, token,NULL, expr,parse_expr_6(),NULL);
      }   }
  }

local Expr parse_expr_4()
  { var Expr expr = parse_expr_5();
    loop
      { var Token token = peek_token();
        switch (token->type)
          { default:
              return expr;
            case TT_logor:
              next_token();
              expr = make_expr(ET_logorop, token,NULL, expr,parse_expr_5(),NULL);
      }   }
  }

local Expr parse_expr_3()
  { var Expr expr = parse_expr_4();
    var Token token = peek_token();
    if (token->type==TT_if)
      { next_token();
       {var Expr expr2 = parse_expr_3();
        var Token token2 = next_token();
        if (!(token2->type==TT_else)) abort_line();
        return make_expr(ET_condop, token,token2, expr,expr2,parse_expr_3());
      }}
      else
      return expr;
  }

local Expr parse_expr_1()
  { var Expr expr = parse_expr_3();
    loop
      { var Token token = peek_token();
        switch (token->type)
          { default:
              return expr;
            case TT_comma:
              next_token();
              expr = make_expr(ET_seqop, token,NULL, expr,parse_expr_3(),NULL);
      }   }
  }

local Expr parse_expr()
  { return parse_expr_1(); }

/* Ausdruck ausgeben: */
local void print_expr(expr)
  var Expr expr;
  { switch (expr->type)
      { case ET_literal:
          print_token(expr->token1); break;
        case ET_paren:
          print_token(expr->token1); print_expr(expr->expr1); print_token(expr->token2); break;
        case ET_call:
          print_expr(expr->expr1); print_token(expr->token1); print_expr(expr->expr2); print_token(expr->token2); break;
        case ET_unary:
          print_token(expr->token1); print_expr(expr->expr1); break;
        case ET_mulop:
        case ET_addop:
        case ET_shiftop:
        case ET_cmpop:
        case ET_relop:
        case ET_andop:
        case ET_xorop:
        case ET_orop:
        case ET_logandop:
        case ET_logorop:
        case ET_seqop:
           print_expr(expr->expr1); print_token(expr->token1); print_expr(expr->expr2); break;
        case ET_condop:
           print_expr(expr->expr1); print_token(expr->token1); print_expr(expr->expr2); print_token(expr->token2); print_expr(expr->expr3); break;
        case ET_defined:
           fprintf(outfile,"DEFINED_%ld",expr->counter); break;
  }   }

/* Ausdruck rekursiv durchgehen und defined(...)-Ausdrücke ersetzen: */
local uintL defined_counter;
local boolean definedp(expr)
  var Expr expr;
  { if (!(expr->type==ET_literal)) return FALSE;
   {var Token token = expr->token1;
    if (!(token->type==TT_ident)) return FALSE;
    {var char* ptr = token->startptr;
     if (!(token->endptr == ptr+7)) return FALSE;
     if (   (ptr[0]=='d') && (ptr[1]=='e') && (ptr[2]=='f') && (ptr[3]=='i')
         && (ptr[4]=='n') && (ptr[5]=='e') && (ptr[6]=='d')
        )
       return TRUE;
       else
       return FALSE;
  }}}
local void defined_replace(expr)
  var Expr expr;
  { if (expr->type==ET_call)
      if (definedp(expr->expr1))
        if (expr->expr2->type==ET_literal)
          if (expr->expr2->token1->type==TT_ident)
            { var Token ident = expr->expr2->token1;
              /* expr vom Typ ET_call in Typ ET_defined umwandeln: */
              expr->expr2->token1 = NULL;
              free_expr(expr->expr1); free_expr(expr->expr2);
              free_token(expr->token1); free_token(expr->token2);
              expr->type = ET_defined; expr->counter = ++defined_counter;
              puts("#undef "); print_expr(expr); puts("\n");
              puts("#ifdef "); print_token(ident); puts("\n");
              puts("#define "); print_expr(expr); puts(" 1\n");
              puts("#else\n");
              puts("#define "); print_expr(expr); puts(" 0\n");
              puts("#endif\n");
              free_token(ident);
              return;
            }
    if (expr->expr1) defined_replace(expr->expr1);
    if (expr->expr2) defined_replace(expr->expr2);
    if (expr->expr3) defined_replace(expr->expr3);
  }

/* Zeile im Buffer behandeln: Falls sie eine #if-Anweisung ist, werden alle
   defined(...) durch Tokens ersetzt, die vorher entsprechend definiert werden.
*/
local void process_line()
  { if (setjmp(abort_line_jmpbuf)==0)
      { bufptr = buffer;
        if ((next_char()=='#') && (next_char()=='i') && (next_char()=='f')
            && whitespacep(next_char())
           )
          { /* Rest der Zeile lesen: */
            last_token = NULL;
           {var Expr expr = parse_expr();
            if (!(peek_token()==Token_EOF)) abort_line();
            /* defined(...) ersetzen: */
            defined_counter = 0; defined_replace(expr);
            /* Zeile ausgeben: */
            puts("#if "); print_expr(expr); puts("\n");
            return;
      }   }}
    /* Keine passende Präprozessor-Zeile oder Abbruch wegen Syntaxfehler -> */
    /* Zeile unverändert ausgeben: */
   {var char* ptr = buffer;
    until (ptr==bufend) { var char c = *ptr++; put(c); }
    put('\n');
  }}

global int main(argc,argv)
  var int argc;
  var char** argv;
  { /* Argumente behandeln: */
    if (argc > 3)
      { fprintf(stderr,"Usage: dedefined [infile [outfile]]\n"); return 1; }
    if (argc >= 2)
      { if ((infile = fopen(argv[1],"r")) == NULL)
          { fprintf(stderr,"dedefined: unable to open input file `%s'",argv[1]);
            return 1;
      }   }
      else
      { infile = stdin; }
    if (argc >= 3)
      { if ((outfile = fopen(argv[2],"w")) == NULL)
          { fprintf(stderr,"dedefined: unable to open output file `%s'",argv[2]);
            return 1;
      }   }
      else
      { outfile = stdout; }
    /* Buffer initialisieren: */
    buffer = malloc(bufsize = 1024);
    /* Zeilen lesen: */
    loop
      { var int c = get();
        if (c==EOF) break;
        if (c=='#')
          /* mögliche Präprozessor-Anweisung */
          { bufend = buffer;
            loop
              { putbuffer(c);
                c = get();
                if ((c==EOF) || (c==NL)) break;
              }
            process_line();
          }
          else
          /* normale Zeile, unverändert durchlassen */
          { loop
              { put(c);
                c = get();
                if ((c==EOF) || (c==NL)) break;
              }
            put(NL);
            if (c==EOF) break;
      }   }
    /* Files schließen: */
    fclose(infile);
    fclose(outfile);
    exit(0);
  }

