
# line 38 "./c-exp.y"

#include "defs.h"
#include "expression.h"
#include "parser-defs.h"
#include "value.h"
#include "language.h"
#include "c-lang.h"

/* Remap normal yacc parser interface names (yyparse, yylex, yyerror, etc),
   as well as gratuitiously global symbol names, so we can have multiple
   yacc generated parsers in gdb.  Note that these are only the variables
   produced by yacc.  If other parser generators (bison, byacc, etc) produce
   additional global names that conflict at link time, then those parser
   generators need to be fixed instead of adding those names to this list. */

#define	yymaxdepth c_maxdepth
#define	yyparse	c_parse
#define	yylex	c_lex
#define	yyerror	c_error
#define	yylval	c_lval
#define	yychar	c_char
#define	yydebug	c_debug
#define	yypact	c_pact	
#define	yyr1	c_r1			
#define	yyr2	c_r2			
#define	yydef	c_def		
#define	yychk	c_chk		
#define	yypgo	c_pgo		
#define	yyact	c_act		
#define	yyexca	c_exca
#define yyerrflag c_errflag
#define yynerrs	c_nerrs
#define	yyps	c_ps
#define	yypv	c_pv
#define	yys	c_s
#define	yy_yys	c_yys
#define	yystate	c_state
#define	yytmp	c_tmp
#define	yyv	c_v
#define	yy_yyv	c_yyv
#define	yyval	c_val
#define	yylloc	c_lloc
#define yyreds	c_reds		/* With YYDEBUG defined */
#define yytoks	c_toks		/* With YYDEBUG defined */

#ifndef YYDEBUG
#define	YYDEBUG	0		/* Default to no yydebug support */
#endif

int
yyparse PARAMS ((void));

static int
yylex PARAMS ((void));

void
yyerror PARAMS ((char *));


# line 102 "./c-exp.y"
typedef union 
  {
    LONGEST lval;
    unsigned LONGEST ulval;
    struct {
      LONGEST val;
      struct type *type;
    } typed_val;
    double dval;
    struct symbol *sym;
    struct type *tval;
    struct stoken sval;
    struct ttype tsym;
    struct symtoken ssym;
    int voidval;
    struct block *bval;
    enum exp_opcode opcode;
    struct internalvar *ivar;

    struct type **tvec;
    int *ivec;
  } YYSTYPE;

# line 126 "./c-exp.y"
/* YYSTYPE gets defined by %union */
static int
parse_number PARAMS ((char *, int, int, YYSTYPE *));
# define INT 257
# define FLOAT 258
# define STRING 259
# define NAME 260
# define TYPENAME 261
# define NAME_OR_INT 262
# define STRUCT 263
# define CLASS 264
# define UNION 265
# define ENUM 266
# define SIZEOF 267
# define UNSIGNED 268
# define COLONCOLON 269
# define TEMPLATE 270
# define ERROR 271
# define SIGNED_KEYWORD 272
# define LONG 273
# define SHORT 274
# define INT_KEYWORD 275
# define CONST_KEYWORD 276
# define VOLATILE_KEYWORD 277
# define LAST 278
# define REGNAME 279
# define VARIABLE 280
# define ASSIGN_MODIFY 281
# define THIS 282
# define ABOVE_COMMA 283
# define OROR 284
# define ANDAND 285
# define EQUAL 286
# define NOTEQUAL 287
# define LEQ 288
# define GEQ 289
# define LSH 290
# define RSH 291
# define UNARY 292
# define INCREMENT 293
# define DECREMENT 294
# define ARROW 295
# define BLOCKNAME 296
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern int yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
YYSTYPE yylval, yyval;
# define YYERRCODE 256

# line 943 "./c-exp.y"


/* Take care of parsing a number (anything that starts with a digit).
   Set yylval and return the token type; update lexptr.
   LEN is the number of characters in it.  */

/*** Needs some error checking for the float case ***/

static int
parse_number (p, len, parsed_float, putithere)
     register char *p;
     register int len;
     int parsed_float;
     YYSTYPE *putithere;
{
  register LONGEST n = 0;
  register LONGEST prevn = 0;
  register int i;
  register int c;
  register int base = input_radix;
  int unsigned_p = 0;
  int long_p = 0;
  LONGEST high_bit;
  struct type *signed_type;
  struct type *unsigned_type;

  if (parsed_float)
    {
      /* It's a float since it contains a point or an exponent.  */
      putithere->dval = atof (p);
      return FLOAT;
    }

  /* Handle base-switching prefixes 0x, 0t, 0d, 0 */
  if (p[0] == '0')
    switch (p[1])
      {
      case 'x':
      case 'X':
	if (len >= 3)
	  {
	    p += 2;
	    base = 16;
	    len -= 2;
	  }
	break;

      case 't':
      case 'T':
      case 'd':
      case 'D':
	if (len >= 3)
	  {
	    p += 2;
	    base = 10;
	    len -= 2;
	  }
	break;

      default:
	base = 8;
	break;
      }

  while (len-- > 0)
    {
      c = *p++;
      if (c >= 'A' && c <= 'Z')
	c += 'a' - 'A';
      if (c != 'l' && c != 'u')
	n *= base;
      if (c >= '0' && c <= '9')
	n += i = c - '0';
      else
	{
	  if (base > 10 && c >= 'a' && c <= 'f')
	    n += i = c - 'a' + 10;
	  else if (len == 0 && c == 'l') 
            long_p = 1;
	  else if (len == 0 && c == 'u')
	    unsigned_p = 1;
	  else
	    return ERROR;	/* Char not a digit */
	}
      if (i >= base)
	return ERROR;		/* Invalid digit in this base */

      /* Portably test for overflow (only works for nonzero values, so make
	 a second check for zero).  */
      if((prevn >= n) && n != 0)
	 unsigned_p=1;		/* Try something unsigned */
      /* If range checking enabled, portably test for unsigned overflow.  */
      if(RANGE_CHECK && n!=0)
      {	
	 if((unsigned_p && (unsigned)prevn >= (unsigned)n))
	    range_error("Overflow on numeric constant.");	 
      }
      prevn=n;
    }
 
     /* If the number is too big to be an int, or it's got an l suffix
	then it's a long.  Work out if this has to be a long by
	shifting right and and seeing if anything remains, and the
	target int size is different to the target long size. */

    if ((TARGET_INT_BIT != TARGET_LONG_BIT && (n >> TARGET_INT_BIT)) || long_p)
      {
         high_bit = ((LONGEST)1) << (TARGET_LONG_BIT-1);
	 unsigned_type = builtin_type_unsigned_long;
	 signed_type = builtin_type_long;
      }
    else 
      {
	 high_bit = ((LONGEST)1) << (TARGET_INT_BIT-1);
	 unsigned_type = builtin_type_unsigned_int;
	 signed_type = builtin_type_int;
      }    

   putithere->typed_val.val = n;

   /* If the high bit of the worked out type is set then this number
      has to be unsigned. */

   if (unsigned_p || (n & high_bit)) 
     {
        putithere->typed_val.type = unsigned_type;
     }
   else 
     {
        putithere->typed_val.type = signed_type;
     }

   return INT;
}

struct token
{
  char *operator;
  int token;
  enum exp_opcode opcode;
};

static const struct token tokentab3[] =
  {
    {">>=", ASSIGN_MODIFY, BINOP_RSH},
    {"<<=", ASSIGN_MODIFY, BINOP_LSH}
  };

static const struct token tokentab2[] =
  {
    {"+=", ASSIGN_MODIFY, BINOP_ADD},
    {"-=", ASSIGN_MODIFY, BINOP_SUB},
    {"*=", ASSIGN_MODIFY, BINOP_MUL},
    {"/=", ASSIGN_MODIFY, BINOP_DIV},
    {"%=", ASSIGN_MODIFY, BINOP_REM},
    {"|=", ASSIGN_MODIFY, BINOP_BITWISE_IOR},
    {"&=", ASSIGN_MODIFY, BINOP_BITWISE_AND},
    {"^=", ASSIGN_MODIFY, BINOP_BITWISE_XOR},
    {"++", INCREMENT, BINOP_END},
    {"--", DECREMENT, BINOP_END},
    {"->", ARROW, BINOP_END},
    {"&&", ANDAND, BINOP_END},
    {"||", OROR, BINOP_END},
    {"::", COLONCOLON, BINOP_END},
    {"<<", LSH, BINOP_END},
    {">>", RSH, BINOP_END},
    {"==", EQUAL, BINOP_END},
    {"!=", NOTEQUAL, BINOP_END},
    {"<=", LEQ, BINOP_END},
    {">=", GEQ, BINOP_END}
  };

/* Read one token, getting characters through lexptr.  */

static int
yylex ()
{
  int c;
  int namelen;
  unsigned int i;
  char *tokstart;
  char *tokptr;
  int tempbufindex;
  static char *tempbuf;
  static int tempbufsize;
  
 retry:

  tokstart = lexptr;
  /* See if it is a special token of length 3.  */
  for (i = 0; i < sizeof tokentab3 / sizeof tokentab3[0]; i++)
    if (STREQN (tokstart, tokentab3[i].operator, 3))
      {
	lexptr += 3;
	yylval.opcode = tokentab3[i].opcode;
	return tokentab3[i].token;
      }

  /* See if it is a special token of length 2.  */
  for (i = 0; i < sizeof tokentab2 / sizeof tokentab2[0]; i++)
    if (STREQN (tokstart, tokentab2[i].operator, 2))
      {
	lexptr += 2;
	yylval.opcode = tokentab2[i].opcode;
	return tokentab2[i].token;
      }

  switch (c = *tokstart)
    {
    case 0:
      return 0;

    case ' ':
    case '\t':
    case '\n':
      lexptr++;
      goto retry;

    case '\'':
      /* We either have a character constant ('0' or '\177' for example)
	 or we have a quoted symbol reference ('foo(int,int)' in C++
	 for example). */
      lexptr++;
      c = *lexptr++;
      if (c == '\\')
	c = parse_escape (&lexptr);

      yylval.typed_val.val = c;
      yylval.typed_val.type = builtin_type_char;

      c = *lexptr++;
      if (c != '\'')
	{
	  namelen = skip_quoted (tokstart) - tokstart;
	  if (namelen > 2)
	    {
	      lexptr = tokstart + namelen;
	      namelen -= 2;
	      tokstart++;
	      goto tryname;
	    }
	  error ("Invalid character constant.");
	}
      return INT;

    case '(':
      paren_depth++;
      lexptr++;
      return c;

    case ')':
      if (paren_depth == 0)
	return 0;
      paren_depth--;
      lexptr++;
      return c;

    case ',':
      if (comma_terminates && paren_depth == 0)
	return 0;
      lexptr++;
      return c;

    case '.':
      /* Might be a floating point number.  */
      if (lexptr[1] < '0' || lexptr[1] > '9')
	goto symbol;		/* Nope, must be a symbol. */
      /* FALL THRU into number case.  */

    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      {
	/* It's a number.  */
	int got_dot = 0, got_e = 0, toktype;
	register char *p = tokstart;
	int hex = input_radix > 10;

	if (c == '0' && (p[1] == 'x' || p[1] == 'X'))
	  {
	    p += 2;
	    hex = 1;
	  }
	else if (c == '0' && (p[1]=='t' || p[1]=='T' || p[1]=='d' || p[1]=='D'))
	  {
	    p += 2;
	    hex = 0;
	  }

	for (;; ++p)
	  {
	    if (!hex && !got_e && (*p == 'e' || *p == 'E'))
	      got_dot = got_e = 1;
	    else if (!hex && !got_dot && *p == '.')
	      got_dot = 1;
	    else if (got_e && (p[-1] == 'e' || p[-1] == 'E')
		     && (*p == '-' || *p == '+'))
	      /* This is the sign of the exponent, not the end of the
		 number.  */
	      continue;
	    /* We will take any letters or digits.  parse_number will
	       complain if past the radix, or if L or U are not final.  */
	    else if ((*p < '0' || *p > '9')
		     && ((*p < 'a' || *p > 'z')
				  && (*p < 'A' || *p > 'Z')))
	      break;
	  }
	toktype = parse_number (tokstart, p - tokstart, got_dot|got_e, &yylval);
        if (toktype == ERROR)
	  {
	    char *err_copy = (char *) alloca (p - tokstart + 1);

	    memcpy (err_copy, tokstart, p - tokstart);
	    err_copy[p - tokstart] = 0;
	    error ("Invalid number \"%s\".", err_copy);
	  }
	lexptr = p;
	return toktype;
      }

    case '+':
    case '-':
    case '*':
    case '/':
    case '%':
    case '|':
    case '&':
    case '^':
    case '~':
    case '!':
    case '@':
    case '<':
    case '>':
    case '[':
    case ']':
    case '?':
    case ':':
    case '=':
    case '{':
    case '}':
    symbol:
      lexptr++;
      return c;

    case '"':

      /* Build the gdb internal form of the input string in tempbuf,
	 translating any standard C escape forms seen.  Note that the
	 buffer is null byte terminated *only* for the convenience of
	 debugging gdb itself and printing the buffer contents when
	 the buffer contains no embedded nulls.  Gdb does not depend
	 upon the buffer being null byte terminated, it uses the length
	 string instead.  This allows gdb to handle C strings (as well
	 as strings in other languages) with embedded null bytes */

      tokptr = ++tokstart;
      tempbufindex = 0;

      do {
	/* Grow the static temp buffer if necessary, including allocating
	   the first one on demand. */
	if (tempbufindex + 1 >= tempbufsize)
	  {
	    tempbuf = (char *) xrealloc (tempbuf, tempbufsize += 64);
	  }
	switch (*tokptr)
	  {
	  case '\0':
	  case '"':
	    /* Do nothing, loop will terminate. */
	    break;
	  case '\\':
	    tokptr++;
	    c = parse_escape (&tokptr);
	    if (c == -1)
	      {
		continue;
	      }
	    tempbuf[tempbufindex++] = c;
	    break;
	  default:
	    tempbuf[tempbufindex++] = *tokptr++;
	    break;
	  }
      } while ((*tokptr != '"') && (*tokptr != '\0'));
      if (*tokptr++ != '"')
	{
	  error ("Unterminated string in expression.");
	}
      tempbuf[tempbufindex] = '\0';	/* See note above */
      yylval.sval.ptr = tempbuf;
      yylval.sval.length = tempbufindex;
      lexptr = tokptr;
      return (STRING);
    }

  if (!(c == '_' || c == '$'
	|| (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')))
    /* We must have come across a bad character (e.g. ';').  */
    error ("Invalid character '%c' in expression.", c);

  /* It's a name.  See how long it is.  */
  namelen = 0;
  for (c = tokstart[namelen];
       (c == '_' || c == '$' || (c >= '0' && c <= '9')
	|| (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'));
       c = tokstart[++namelen])
    ;

  /* The token "if" terminates the expression and is NOT 
     removed from the input stream.  */
  if (namelen == 2 && tokstart[0] == 'i' && tokstart[1] == 'f')
    {
      return 0;
    }

  lexptr += namelen;

  /* Handle the tokens $digits; also $ (short for $0) and $$ (short for $$1)
     and $$digits (equivalent to $<-digits> if you could type that).
     Make token type LAST, and put the number (the digits) in yylval.  */

  tryname:
  if (*tokstart == '$')
    {
      register int negate = 0;
      c = 1;
      /* Double dollar means negate the number and add -1 as well.
	 Thus $$ alone means -1.  */
      if (namelen >= 2 && tokstart[1] == '$')
	{
	  negate = 1;
	  c = 2;
	}
      if (c == namelen)
	{
	  /* Just dollars (one or two) */
	  yylval.lval = - negate;
	  return LAST;
	}
      /* Is the rest of the token digits?  */
      for (; c < namelen; c++)
	if (!(tokstart[c] >= '0' && tokstart[c] <= '9'))
	  break;
      if (c == namelen)
	{
	  yylval.lval = atoi (tokstart + 1 + negate);
	  if (negate)
	    yylval.lval = - yylval.lval;
	  return LAST;
	}
    }

  /* Handle tokens that refer to machine registers:
     $ followed by a register name.  */

  if (*tokstart == '$') {
    for (c = 0; c < NUM_REGS; c++)
      if (namelen - 1 == strlen (reg_names[c])
	  && STREQN (tokstart + 1, reg_names[c], namelen - 1))
	{
	  yylval.lval = c;
	  return REGNAME;
	}
    for (c = 0; c < num_std_regs; c++)
     if (namelen - 1 == strlen (std_regs[c].name)
	 && STREQN (tokstart + 1, std_regs[c].name, namelen - 1))
       {
	 yylval.lval = std_regs[c].regnum;
	 return REGNAME;
       }
  }
  /* Catch specific keywords.  Should be done with a data structure.  */
  switch (namelen)
    {
    case 8:
      if (STREQN (tokstart, "unsigned", 8))
	return UNSIGNED;
      if (current_language->la_language == language_cplus
	  && STREQN (tokstart, "template", 8))
	return TEMPLATE;
      if (STREQN (tokstart, "volatile", 8))
	return VOLATILE_KEYWORD;
      break;
    case 6:
      if (STREQN (tokstart, "struct", 6))
	return STRUCT;
      if (STREQN (tokstart, "signed", 6))
	return SIGNED_KEYWORD;
      if (STREQN (tokstart, "sizeof", 6))      
	return SIZEOF;
      break;
    case 5:
      if (current_language->la_language == language_cplus
	  && STREQN (tokstart, "class", 5))
	return CLASS;
      if (STREQN (tokstart, "union", 5))
	return UNION;
      if (STREQN (tokstart, "short", 5))
	return SHORT;
      if (STREQN (tokstart, "const", 5))
	return CONST_KEYWORD;
      break;
    case 4:
      if (STREQN (tokstart, "enum", 4))
	return ENUM;
      if (STREQN (tokstart, "long", 4))
	return LONG;
      if (current_language->la_language == language_cplus
	  && STREQN (tokstart, "this", 4))
	{
	  static const char this_name[] =
				 { CPLUS_MARKER, 't', 'h', 'i', 's', '\0' };

	  if (lookup_symbol (this_name, expression_context_block,
			     VAR_NAMESPACE, 0, NULL))
	    return THIS;
	}
      break;
    case 3:
      if (STREQN (tokstart, "int", 3))
	return INT_KEYWORD;
      break;
    default:
      break;
    }

  yylval.sval.ptr = tokstart;
  yylval.sval.length = namelen;

  /* Any other names starting in $ are debugger internal variables.  */

  if (*tokstart == '$')
    {
      yylval.ivar =  lookup_internalvar (copy_name (yylval.sval) + 1);
      return VARIABLE;
    }

  /* Use token-type BLOCKNAME for symbols that happen to be defined as
     functions or symtabs.  If this is not so, then ...
     Use token-type TYPENAME for symbols that happen to be defined
     currently as names of types; NAME for other symbols.
     The caller is not constrained to care about the distinction.  */
  {
    char *tmp = copy_name (yylval.sval);
    struct symbol *sym;
    int is_a_field_of_this = 0;
    int hextype;

    sym = lookup_symbol (tmp, expression_context_block,
			 VAR_NAMESPACE,
			 current_language->la_language == language_cplus
			 ? &is_a_field_of_this : NULL,
			 NULL);
    if ((sym && SYMBOL_CLASS (sym) == LOC_BLOCK) ||
        lookup_partial_symtab (tmp))
      {
	yylval.ssym.sym = sym;
	yylval.ssym.is_a_field_of_this = is_a_field_of_this;
	return BLOCKNAME;
      }
    if (sym && SYMBOL_CLASS (sym) == LOC_TYPEDEF)
        {
	  yylval.tsym.type = SYMBOL_TYPE (sym);
	  return TYPENAME;
        }
    if ((yylval.tsym.type = lookup_primitive_typename (tmp)) != 0)
	return TYPENAME;

    /* Input names that aren't symbols but ARE valid hex numbers,
       when the input radix permits them, can be names or numbers
       depending on the parse.  Note we support radixes > 16 here.  */
    if (!sym && 
        ((tokstart[0] >= 'a' && tokstart[0] < 'a' + input_radix - 10) ||
         (tokstart[0] >= 'A' && tokstart[0] < 'A' + input_radix - 10)))
      {
 	YYSTYPE newlval;	/* Its value is ignored.  */
	hextype = parse_number (tokstart, namelen, 0, &newlval);
	if (hextype == INT)
	  {
	    yylval.ssym.sym = sym;
	    yylval.ssym.is_a_field_of_this = is_a_field_of_this;
	    return NAME_OR_INT;
	  }
      }

    /* Any other kind of symbol */
    yylval.ssym.sym = sym;
    yylval.ssym.is_a_field_of_this = is_a_field_of_this;
    return NAME;
  }
}

void
yyerror (msg)
     char *msg;
{
  error (msg ? msg : "Invalid syntax in expression.");
}
int yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
-1, 45,
	269, 66,
	-2, 129,
-1, 129,
	269, 95,
	-2, 126,
-1, 181,
	269, 67,
	-2, 68,
	};
# define YYNPROD 130
# define YYLAST 1038
int yyact[]={

     9,    57,   203,   185,    53,     7,    55,    15,   182,     6,
    51,    56,     8,   167,   103,   105,   106,    32,   109,    37,
    38,    39,    40,   184,    36,   183,    42,   192,    41,    34,
    35,    33,    43,    44,   168,   158,   108,    53,   107,   101,
     9,   179,    87,    51,   113,     7,    76,    86,   113,     6,
   104,   202,     8,    98,    99,    52,   120,   121,   114,   173,
   110,   111,   114,    95,    95,    97,    97,    94,    94,    93,
    76,    47,   160,    95,   161,    97,   186,    94,    47,   214,
     9,   176,   200,    91,     2,   168,   205,    15,    52,   160,
    27,   207,   209,    10,    29,    76,   193,   199,    57,    68,
   200,    53,   211,    55,    58,   210,    59,    51,    56,   165,
   100,   198,    47,    76,   164,    76,   100,   100,   167,   191,
   189,    66,    74,    67,    73,    54,   100,   134,    28,    31,
    27,   162,    25,    10,    57,    68,   133,    53,   112,    55,
    58,   132,    59,    51,    56,   127,   131,   103,   105,   106,
   171,   172,    52,   162,    96,    69,    14,    66,    74,    67,
    73,    54,    19,   163,   169,   170,    57,   174,     1,    53,
    27,    55,    58,    10,    59,    51,    56,   190,     3,     0,
   119,     0,     0,   104,     0,    70,     0,     0,    52,     0,
     0,    69,     0,    54,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,   180,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    52,    70,     0,     0,    16,    18,    23,    46,    32,    17,
    37,    38,    39,    40,    13,    36,    30,    42,     0,    41,
    34,    35,    33,    43,    44,    20,    21,    22,     0,    24,
     0,     0,   103,   105,   106,     0,     0,    48,    49,    50,
    11,    12,     0,    45,    16,    18,    23,    46,    32,    17,
    37,    38,    39,    40,    13,    36,    30,    42,     0,    41,
    34,    35,    33,    43,    44,    20,    21,    22,   104,    24,
    48,    49,    50,   213,   201,    92,     0,     0,     0,     0,
    11,    12,     0,    45,    16,    18,    23,    46,    32,    17,
    37,    38,    39,    40,    13,    36,    30,    42,     0,    41,
    34,    35,    33,    43,    44,    20,    21,    22,     0,    24,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    11,    12,    75,    45,   128,    72,    71,    62,    63,    64,
    65,    60,    61,     0,    48,    49,    50,     0,     0,   103,
   129,   106,    37,    38,    39,    40,     0,    36,     0,    42,
     0,    41,    34,    35,    33,    43,    44,   212,    75,     0,
     0,    72,    71,    62,    63,    64,    65,    60,    61,     0,
    48,    49,    50,    57,    68,   104,    53,     0,    55,    58,
     0,    59,    51,    56,    57,    68,     0,    53,     0,    55,
    58,     0,    59,    51,    56,     0,    66,     0,    67,    73,
    54,     0,    48,    49,    50,     0,     0,    66,     0,    67,
     0,    54,    57,    68,     0,    53,     0,    55,    58,     0,
    59,    51,    56,    57,    68,     0,    53,    52,    55,    58,
    69,    59,    51,    56,     0,    66,     0,    67,    52,    54,
     0,    69,     0,     0,     0,     0,    66,     0,    67,    57,
    54,     0,    53,     0,    55,    58,     0,    59,    51,    56,
    70,    95,     0,    97,   175,    94,    52,     0,     0,    69,
     0,    70,     0,    57,    68,     0,    53,    52,    55,    58,
    69,    59,    51,    56,    57,     0,     0,    53,     0,    55,
    58,     0,    59,    51,    56,     0,    66,     0,    67,    70,
    54,     0,     0,    52,     0,     0,     0,    66,     0,    67,
    57,    54,     0,    53,   100,    55,    58,     0,    59,    51,
    56,     0,     0,   175,     0,     0,     0,    52,     0,     0,
     0,     0,     0,    66,     0,    67,     0,    54,    52,     0,
     0,     0,   103,   129,   106,    37,    38,    39,    40,     0,
    36,     0,    42,     0,    41,    34,    35,    33,    43,    44,
    57,     0,     0,    53,    52,    55,    58,     0,    59,    51,
    56,     0,     0,     0,     0,     0,     0,    32,   104,    37,
    38,    39,    40,     0,    36,     0,    42,    54,    41,    34,
    35,    33,    43,    44,     0,     0,     0,     0,     0,     0,
     0,     0,     0,   177,     5,     0,     0,     0,   166,     0,
     0,     0,     0,     0,    52,     0,     0,     0,    88,    90,
    72,    71,    62,    63,    64,    65,    60,    61,     0,    48,
    49,    50,    71,    62,    63,    64,    65,    60,    61,   102,
    48,    49,    50,     0,     0,     0,   115,   116,   117,   118,
     0,   122,     0,     0,     0,     0,     0,     0,     0,   126,
   130,    62,    63,    64,    65,    60,    61,     0,    48,    49,
    50,     0,    62,    63,    64,    65,    60,    61,     0,    48,
    49,    50,     0,     0,    32,     0,    37,    38,    39,    40,
   159,    36,     0,    42,     0,    41,    34,    35,    33,    43,
    44,     0,     0,     0,     0,    48,    49,    50,     0,     0,
   181,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,    62,    63,    64,    65,    60,    61,     0,    48,
    49,    50,     0,    62,    63,    64,    65,    60,    61,     0,
    48,    49,    50,    32,     0,    37,    38,    39,    40,     0,
    36,     0,    42,     0,    41,    34,    35,    33,    43,    44,
     0,    64,    65,    60,    61,     0,    48,    49,    50,     0,
     0,     4,     0,     0,     0,     0,   197,     0,    77,    79,
    80,    81,    82,    83,    84,    85,    89,     0,     0,     0,
   204,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,   208,    78,    26,     0,     0,     0,
     0,     0,     0,    60,    61,     0,    48,    49,    50,   125,
    26,    26,     0,     0,     0,     0,   135,   136,   137,   138,
   139,   140,   141,   142,   143,   144,   145,   146,   147,   148,
   149,   150,   151,   152,   153,   154,   155,   156,     0,   123,
   124,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,   157,     0,     0,     0,     0,     0,     0,     0,
     0,     0,    26,     0,     0,     0,     0,     0,     0,     0,
   187,     0,     0,   178,   188,     0,    89,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,   194,     0,     0,   195,   196,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,   206,     0,   196,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   178,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,   178,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,   178,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,   178 };
int yypact[]={

   -33, -1000,    34, -1000,    97,    75,   -33,   -33,   -33,   -33,
   -33,   -33,   -33,     7,   -33,   -33, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000,    26, -1000,  -230, -1000,
  -246, -1000, -1000, -1000,  -237,  -257,  -213,  -246,  -246,  -246,
  -246,  -217,  -246,  -244,  -244, -1000, -1000,   -33, -1000, -1000,
   302,    99,   -33, -1000,   -33,   -33,   -33,   -33,   -33,   -33,
   -33,   -33,   -33,   -33,   -33,   -33,   -33,   -33,   -33,   -33,
   -33,   -33,   -33,   -33,   -33,   -33,  -244,    -3,  -234,    -3,
    -3,    -3,    -3,    -3,    -3,    -3,   -33,    28,     6,    97,
    73,    68,    -8, -1000,    35,    35,    19,   443, -1000, -1000,
   -52,  -246, -1000, -1000, -1000, -1000, -1000, -1000,  -267, -1000,
  -250,  -272, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000,    16, -1000, -1000,    97, -1000, -1000,   -33, -1000,
 -1000, -1000,   -33,    27,   -33,   432,    -3,    -3,    -3,   -36,
   -36,   129,   129,   493,   493,   543,   543,   543,   543,   467,
   456,   406,   395,   367,    61,    97,    97,  -242,  -113,    55,
   -33, -1000, -1000,   -33,   -33, -1000, -1000,  -246, -1000, -1000,
 -1000, -1000, -1000,   502,    70, -1000,    56,    75,    25, -1000,
   -42, -1000, -1000, -1000,  -273, -1000,  -244,    -3,    -3, -1000,
    45,   -33,    49,    47,    97,    -3,    -3, -1000, -1000, -1000,
  -244,    43, -1000, -1000,    30, -1000,   356,    64,    75, -1000,
    62,   336, -1000,    38, -1000 };
int yypgo[]={

     0,   791,    83,   178,   168,   162,    94,   156,    74,   623,
   825,    81,    54,   154,    69,   132,    53,   628,   129,   138,
   128,   127,    42 };
int yyr1[]={

     0,     4,     4,     3,     2,     2,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,    21,     1,     7,    22,    22,    22,     8,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,    20,    20,     5,     6,
     6,     5,     5,     5,    15,    15,    14,    14,    14,    14,
    14,    13,    13,    13,    13,    13,    16,    16,    12,    12,
     9,     9,     9,     9,     9,    10,    10,    10,    10,    10,
    10,    10,    10,    10,    10,    10,    10,    10,    10,    10,
    10,    10,    10,    10,    10,    10,    10,    10,    19,    19,
    19,    19,    11,    11,    17,    17,    17,    17,    18,    18 };
int yyr2[]={

     0,     2,     2,     3,     2,     7,     5,     5,     5,     5,
     5,     5,     5,     5,     5,     5,     7,     7,     9,     7,
     7,     9,     9,     1,    11,     3,     0,     3,     7,     3,
     7,     9,     9,     7,     7,     7,     7,     7,     7,     7,
     7,     7,     7,     7,     7,     7,     7,     7,     7,     7,
     7,     7,     7,    11,     7,     7,     3,     3,     3,     2,
     3,     3,     3,     9,     3,     3,     3,     7,     7,     7,
     9,     2,     5,     3,     2,     5,     3,     5,     3,     5,
     2,     7,     5,     3,     5,     3,     5,     7,     5,     7,
     2,     7,    13,    17,    19,     3,     3,     3,     3,     5,
     7,     5,     7,     7,     9,     5,     7,     5,     5,     5,
     5,     5,     3,     5,     3,    11,     5,     5,     2,     3,
     3,     3,     3,     7,     3,     3,     3,     3,     2,     2 };
int yychk[]={

 -1000,    -4,    -2,    -3,    -1,    -9,    42,    38,    45,    33,
   126,   293,   294,   267,    -7,    40,   257,   262,   258,    -5,
   278,   279,   280,   259,   282,   -15,   -10,   123,   -20,    -6,
   269,   -18,   261,   275,   273,   274,   268,   263,   264,   265,
   266,   272,   270,   276,   277,   296,   260,    44,   293,   294,
   295,    46,    91,    40,    64,    42,    47,    37,    43,    45,
   290,   291,   286,   287,   288,   289,    60,    62,    38,    94,
   124,   285,   284,    63,    61,   281,    40,    -1,   -10,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    40,   -22,    -9,    -1,
    -9,    -2,   269,   -14,    42,    38,   -13,    40,   -16,   -12,
    91,   269,   -17,   260,   296,   261,   262,   275,   273,   275,
   273,   274,   -19,   261,   275,   -17,   -17,   -17,   -17,   -19,
   273,   274,   -17,   -10,   -10,    -1,   -17,    -6,    42,   261,
   -17,    -6,    42,    -2,   -21,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,   -10,   269,    -9,
    44,    -8,   125,    -8,    41,    41,   -17,   126,    42,   -14,
   -14,   -16,   -12,    40,   -14,    41,   -11,    -9,   -10,    93,
   257,   -17,   275,   275,   273,   275,    60,    -1,    -1,    93,
   -22,    58,   269,    41,    -1,    -1,    -1,   -17,    41,    41,
    44,   269,    93,   275,    -9,    41,    -1,    42,    -9,    62,
    41,    40,    41,   -11,    41 };
int yydef[]={

     0,    -2,     1,     2,     4,     3,     0,     0,     0,     0,
     0,     0,     0,     0,    26,     0,    56,    57,    58,    59,
    60,    61,    62,    64,    65,    90,    74,    25,     0,    71,
     0,    73,    95,    96,    97,    98,   112,     0,     0,     0,
     0,   114,     0,     0,     0,    -2,   128,     0,    13,    14,
     0,     0,     0,    23,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     6,     0,     7,
     8,     9,    10,    11,    12,    15,     0,     0,     0,    27,
     0,     0,     0,    75,    76,    78,    80,     0,    83,    85,
     0,     0,    72,   124,   125,   126,   127,    99,   101,   105,
   120,   121,   111,   118,   119,   107,   108,   109,   110,   113,
   120,   121,     0,   116,   117,     5,    16,    17,     0,    -2,
    19,    20,     0,     0,    26,    34,    35,    36,    37,    38,
    39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
    49,    50,    51,    52,     0,    54,    55,     0,     0,     0,
     0,    30,    29,     0,     0,    33,    69,     0,    91,    77,
    79,    82,    84,     0,     0,    88,     0,   122,    74,    86,
     0,    -2,   102,   100,   103,   106,     0,    18,    21,    22,
     0,     0,     0,    63,    28,    31,    32,    70,    81,    89,
     0,     0,    87,   104,     0,    24,    53,     0,   123,   115,
    92,     0,    93,     0,    94 };
typedef struct { char *t_name; int t_val; } yytoktype;
#ifndef YYDEBUG
#	define YYDEBUG	0	/* don't allow debugging */
#endif

#if YYDEBUG

yytoktype yytoks[] =
{
	"INT",	257,
	"FLOAT",	258,
	"STRING",	259,
	"NAME",	260,
	"TYPENAME",	261,
	"NAME_OR_INT",	262,
	"STRUCT",	263,
	"CLASS",	264,
	"UNION",	265,
	"ENUM",	266,
	"SIZEOF",	267,
	"UNSIGNED",	268,
	"COLONCOLON",	269,
	"TEMPLATE",	270,
	"ERROR",	271,
	"SIGNED_KEYWORD",	272,
	"LONG",	273,
	"SHORT",	274,
	"INT_KEYWORD",	275,
	"CONST_KEYWORD",	276,
	"VOLATILE_KEYWORD",	277,
	"LAST",	278,
	"REGNAME",	279,
	"VARIABLE",	280,
	"ASSIGN_MODIFY",	281,
	"THIS",	282,
	",",	44,
	"ABOVE_COMMA",	283,
	"=",	61,
	"?",	63,
	"OROR",	284,
	"ANDAND",	285,
	"|",	124,
	"^",	94,
	"&",	38,
	"EQUAL",	286,
	"NOTEQUAL",	287,
	"<",	60,
	">",	62,
	"LEQ",	288,
	"GEQ",	289,
	"LSH",	290,
	"RSH",	291,
	"@",	64,
	"+",	43,
	"-",	45,
	"*",	42,
	"/",	47,
	"%",	37,
	"UNARY",	292,
	"INCREMENT",	293,
	"DECREMENT",	294,
	"ARROW",	295,
	".",	46,
	"[",	91,
	"(",	40,
	"BLOCKNAME",	296,
	"-unknown-",	-1	/* ends search */
};

char * yyreds[] =
{
	"-no such reduction-",
	"start : exp1",
	"start : type_exp",
	"type_exp : type",
	"exp1 : exp",
	"exp1 : exp1 ',' exp",
	"exp : '*' exp",
	"exp : '&' exp",
	"exp : '-' exp",
	"exp : '!' exp",
	"exp : '~' exp",
	"exp : INCREMENT exp",
	"exp : DECREMENT exp",
	"exp : exp INCREMENT",
	"exp : exp DECREMENT",
	"exp : SIZEOF exp",
	"exp : exp ARROW name",
	"exp : exp ARROW qualified_name",
	"exp : exp ARROW '*' exp",
	"exp : exp '.' name",
	"exp : exp '.' qualified_name",
	"exp : exp '.' '*' exp",
	"exp : exp '[' exp1 ']'",
	"exp : exp '('",
	"exp : exp '(' arglist ')'",
	"lcurly : '{'",
	"arglist : /* empty */",
	"arglist : exp",
	"arglist : arglist ',' exp",
	"rcurly : '}'",
	"exp : lcurly arglist rcurly",
	"exp : lcurly type rcurly exp",
	"exp : '(' type ')' exp",
	"exp : '(' exp1 ')'",
	"exp : exp '@' exp",
	"exp : exp '*' exp",
	"exp : exp '/' exp",
	"exp : exp '%' exp",
	"exp : exp '+' exp",
	"exp : exp '-' exp",
	"exp : exp LSH exp",
	"exp : exp RSH exp",
	"exp : exp EQUAL exp",
	"exp : exp NOTEQUAL exp",
	"exp : exp LEQ exp",
	"exp : exp GEQ exp",
	"exp : exp '<' exp",
	"exp : exp '>' exp",
	"exp : exp '&' exp",
	"exp : exp '^' exp",
	"exp : exp '|' exp",
	"exp : exp ANDAND exp",
	"exp : exp OROR exp",
	"exp : exp '?' exp ':' exp",
	"exp : exp '=' exp",
	"exp : exp ASSIGN_MODIFY exp",
	"exp : INT",
	"exp : NAME_OR_INT",
	"exp : FLOAT",
	"exp : variable",
	"exp : LAST",
	"exp : REGNAME",
	"exp : VARIABLE",
	"exp : SIZEOF '(' type ')'",
	"exp : STRING",
	"exp : THIS",
	"block : BLOCKNAME",
	"block : block COLONCOLON name",
	"variable : block COLONCOLON name",
	"qualified_name : typebase COLONCOLON name",
	"qualified_name : typebase COLONCOLON '~' name",
	"variable : qualified_name",
	"variable : COLONCOLON name",
	"variable : name_not_typename",
	"ptype : typebase",
	"ptype : typebase abs_decl",
	"abs_decl : '*'",
	"abs_decl : '*' abs_decl",
	"abs_decl : '&'",
	"abs_decl : '&' abs_decl",
	"abs_decl : direct_abs_decl",
	"direct_abs_decl : '(' abs_decl ')'",
	"direct_abs_decl : direct_abs_decl array_mod",
	"direct_abs_decl : array_mod",
	"direct_abs_decl : direct_abs_decl func_mod",
	"direct_abs_decl : func_mod",
	"array_mod : '[' ']'",
	"array_mod : '[' INT ']'",
	"func_mod : '(' ')'",
	"func_mod : '(' nonempty_typelist ')'",
	"type : ptype",
	"type : typebase COLONCOLON '*'",
	"type : type '(' typebase COLONCOLON '*' ')'",
	"type : type '(' typebase COLONCOLON '*' ')' '(' ')'",
	"type : type '(' typebase COLONCOLON '*' ')' '(' nonempty_typelist ')'",
	"typebase : TYPENAME",
	"typebase : INT_KEYWORD",
	"typebase : LONG",
	"typebase : SHORT",
	"typebase : LONG INT_KEYWORD",
	"typebase : UNSIGNED LONG INT_KEYWORD",
	"typebase : LONG LONG",
	"typebase : LONG LONG INT_KEYWORD",
	"typebase : UNSIGNED LONG LONG",
	"typebase : UNSIGNED LONG LONG INT_KEYWORD",
	"typebase : SHORT INT_KEYWORD",
	"typebase : UNSIGNED SHORT INT_KEYWORD",
	"typebase : STRUCT name",
	"typebase : CLASS name",
	"typebase : UNION name",
	"typebase : ENUM name",
	"typebase : UNSIGNED typename",
	"typebase : UNSIGNED",
	"typebase : SIGNED_KEYWORD typename",
	"typebase : SIGNED_KEYWORD",
	"typebase : TEMPLATE name '<' type '>'",
	"typebase : CONST_KEYWORD typebase",
	"typebase : VOLATILE_KEYWORD typebase",
	"typename : TYPENAME",
	"typename : INT_KEYWORD",
	"typename : LONG",
	"typename : SHORT",
	"nonempty_typelist : type",
	"nonempty_typelist : nonempty_typelist ',' type",
	"name : NAME",
	"name : BLOCKNAME",
	"name : TYPENAME",
	"name : NAME_OR_INT",
	"name_not_typename : NAME",
	"name_not_typename : BLOCKNAME",
};
#endif /* YYDEBUG */
#line 1 "/usr/lib/yaccpar"
/*	@(#)yaccpar 1.10 89/04/04 SMI; from S5R3 1.10	*/

/*
** Skeleton parser driver for yacc output
*/

/*
** yacc user known macros and defines
*/
#define YYERROR		goto yyerrlab
#define YYACCEPT	{ free(yys); free(yyv); return(0); }
#define YYABORT		{ free(yys); free(yyv); return(1); }
#define YYBACKUP( newtoken, newvalue )\
{\
	if ( yychar >= 0 || ( yyr2[ yytmp ] >> 1 ) != 1 )\
	{\
		yyerror( "syntax error - cannot backup" );\
		goto yyerrlab;\
	}\
	yychar = newtoken;\
	yystate = *yyps;\
	yylval = newvalue;\
	goto yynewstate;\
}
#define YYRECOVERING()	(!!yyerrflag)
#ifndef YYDEBUG
#	define YYDEBUG	1	/* make debugging available */
#endif

/*
** user known globals
*/
int yydebug;			/* set to 1 to get debugging */

/*
** driver internal defines
*/
#define YYFLAG		(-1000)

/*
** static variables used by the parser
*/
static YYSTYPE *yyv;			/* value stack */
static int *yys;			/* state stack */

static YYSTYPE *yypv;			/* top of value stack */
static int *yyps;			/* top of state stack */

static int yystate;			/* current state */
static int yytmp;			/* extra var (lasts between blocks) */

int yynerrs;			/* number of errors */

int yyerrflag;			/* error recovery flag */
int yychar;			/* current input token number */


/*
** yyparse - return 0 if worked, 1 if syntax error not recovered from
*/
int
yyparse()
{
	register YYSTYPE *yypvt;	/* top of value stack for $vars */
	unsigned yymaxdepth = YYMAXDEPTH;

	/*
	** Initialize externals - yyparse may be called more than once
	*/
	yyv = (YYSTYPE*)xmalloc(yymaxdepth*sizeof(YYSTYPE));
	yys = (int*)xmalloc(yymaxdepth*sizeof(int));
	if (!yyv || !yys)
	{
		yyerror( "out of memory" );
		return(1);
	}
	yypv = &yyv[-1];
	yyps = &yys[-1];
	yystate = 0;
	yytmp = 0;
	yynerrs = 0;
	yyerrflag = 0;
	yychar = -1;

	goto yystack;
	{
		register YYSTYPE *yy_pv;	/* top of value stack */
		register int *yy_ps;		/* top of state stack */
		register int yy_state;		/* current state */
		register int  yy_n;		/* internal state number info */

		/*
		** get globals into registers.
		** branch to here only if YYBACKUP was called.
		*/
	yynewstate:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;
		goto yy_newstate;

		/*
		** get globals into registers.
		** either we just started, or we just finished a reduction
		*/
	yystack:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;

		/*
		** top of for (;;) loop while no reductions done
		*/
	yy_stack:
		/*
		** put a state and value onto the stacks
		*/
#if YYDEBUG
		/*
		** if debugging, look up token value in list of value vs.
		** name pairs.  0 and negative (-1) are special values.
		** Note: linear search is used since time is not a real
		** consideration while debugging.
		*/
		if ( yydebug )
		{
			register int yy_i;

			(void)printf( "State %d, token ", yy_state );
			if ( yychar == 0 )
				(void)printf( "end-of-file\n" );
			else if ( yychar < 0 )
				(void)printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				(void)printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ++yy_ps >= &yys[ yymaxdepth ] )	/* room on stack? */
		{
			/*
			** xreallocate and recover.  Note that pointers
			** have to be reset, or bad things will happen
			*/
			int yyps_index = (yy_ps - yys);
			int yypv_index = (yy_pv - yyv);
			int yypvt_index = (yypvt - yyv);
			yymaxdepth += YYMAXDEPTH;
			yyv = (YYSTYPE*)xrealloc((char*)yyv,
				yymaxdepth * sizeof(YYSTYPE));
			yys = (int*)xrealloc((char*)yys,
				yymaxdepth * sizeof(int));
			if (!yyv || !yys)
			{
				yyerror( "yacc stack overflow" );
				return(1);
			}
			yy_ps = yys + yyps_index;
			yy_pv = yyv + yypv_index;
			yypvt = yyv + yypvt_index;
		}
		*yy_ps = yy_state;
		*++yy_pv = yyval;

		/*
		** we have a new state - find out what to do
		*/
	yy_newstate:
		if ( ( yy_n = yypact[ yy_state ] ) <= YYFLAG )
			goto yydefault;		/* simple state */
#if YYDEBUG
		/*
		** if debugging, need to mark whether new token grabbed
		*/
		yytmp = yychar < 0;
#endif
		if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
			yychar = 0;		/* reached EOF */
#if YYDEBUG
		if ( yydebug && yytmp )
		{
			register int yy_i;

			(void)printf( "Received token " );
			if ( yychar == 0 )
				(void)printf( "end-of-file\n" );
			else if ( yychar < 0 )
				(void)printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				(void)printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ( ( yy_n += yychar ) < 0 ) || ( yy_n >= YYLAST ) )
			goto yydefault;
		if ( yychk[ yy_n = yyact[ yy_n ] ] == yychar )	/*valid shift*/
		{
			yychar = -1;
			yyval = yylval;
			yy_state = yy_n;
			if ( yyerrflag > 0 )
				yyerrflag--;
			goto yy_stack;
		}

	yydefault:
		if ( ( yy_n = yydef[ yy_state ] ) == -2 )
		{
#if YYDEBUG
			yytmp = yychar < 0;
#endif
			if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
				yychar = 0;		/* reached EOF */
#if YYDEBUG
			if ( yydebug && yytmp )
			{
				register int yy_i;

				(void)printf( "Received token " );
				if ( yychar == 0 )
					(void)printf( "end-of-file\n" );
				else if ( yychar < 0 )
					(void)printf( "-none-\n" );
				else
				{
					for ( yy_i = 0;
						yytoks[yy_i].t_val >= 0;
						yy_i++ )
					{
						if ( yytoks[yy_i].t_val
							== yychar )
						{
							break;
						}
					}
					(void)printf( "%s\n", yytoks[yy_i].t_name );
				}
			}
#endif /* YYDEBUG */
			/*
			** look through exception table
			*/
			{
				register int *yyxi = yyexca;

				while ( ( *yyxi != -1 ) ||
					( yyxi[1] != yy_state ) )
				{
					yyxi += 2;
				}
				while ( ( *(yyxi += 2) >= 0 ) &&
					( *yyxi != yychar ) )
					;
				if ( ( yy_n = yyxi[1] ) < 0 )
					YYACCEPT;
			}
		}

		/*
		** check for syntax error
		*/
		if ( yy_n == 0 )	/* have an error */
		{
			/* no worry about speed here! */
			switch ( yyerrflag )
			{
			case 0:		/* new error */
				yyerror( "syntax error" );
				goto skip_init;
			yyerrlab:
				/*
				** get globals into registers.
				** we have a user generated syntax type error
				*/
				yy_pv = yypv;
				yy_ps = yyps;
				yy_state = yystate;
				yynerrs++;
			skip_init:
			case 1:
			case 2:		/* incompletely recovered error */
					/* try again... */
				yyerrflag = 3;
				/*
				** find state where "error" is a legal
				** shift action
				*/
				while ( yy_ps >= yys )
				{
					yy_n = yypact[ *yy_ps ] + YYERRCODE;
					if ( yy_n >= 0 && yy_n < YYLAST &&
						yychk[yyact[yy_n]] == YYERRCODE)					{
						/*
						** simulate shift of "error"
						*/
						yy_state = yyact[ yy_n ];
						goto yy_stack;
					}
					/*
					** current state has no shift on
					** "error", pop stack
					*/
#if YYDEBUG
#	define _POP_ "Error recovery pops state %d, uncovers state %d\n"
					if ( yydebug )
						(void)printf( _POP_, *yy_ps,
							yy_ps[-1] );
#	undef _POP_
#endif
					yy_ps--;
					yy_pv--;
				}
				/*
				** there is no state on stack with "error" as
				** a valid shift.  give up.
				*/
				YYABORT;
			case 3:		/* no shift yet; eat a token */
#if YYDEBUG
				/*
				** if debugging, look up token in list of
				** pairs.  0 and negative shouldn't occur,
				** but since timing doesn't matter when
				** debugging, it doesn't hurt to leave the
				** tests here.
				*/
				if ( yydebug )
				{
					register int yy_i;

					(void)printf( "Error recovery discards " );
					if ( yychar == 0 )
						(void)printf( "token end-of-file\n" );
					else if ( yychar < 0 )
						(void)printf( "token -none-\n" );
					else
					{
						for ( yy_i = 0;
							yytoks[yy_i].t_val >= 0;
							yy_i++ )
						{
							if ( yytoks[yy_i].t_val
								== yychar )
							{
								break;
							}
						}
						(void)printf( "token %s\n",
							yytoks[yy_i].t_name );
					}
				}
#endif /* YYDEBUG */
				if ( yychar == 0 )	/* reached EOF. quit */
					YYABORT;
				yychar = -1;
				goto yy_newstate;
			}
		}/* end if ( yy_n == 0 ) */
		/*
		** reduction by production yy_n
		** put stack tops, etc. so things right after switch
		*/
#if YYDEBUG
		/*
		** if debugging, print the string that is the user's
		** specification of the reduction which is just about
		** to be done.
		*/
		if ( yydebug )
			(void)printf( "Reduce by (%d) \"%s\"\n",
				yy_n, yyreds[ yy_n ] );
#endif
		yytmp = yy_n;			/* value to switch over */
		yypvt = yy_pv;			/* $vars top of value stack */
		/*
		** Look in goto table for next state
		** Sorry about using yy_state here as temporary
		** register variable, but why not, if it works...
		** If yyr2[ yy_n ] doesn't have the low order bit
		** set, then there is no action to be done for
		** this reduction.  So, no saving & unsaving of
		** registers done.  The only difference between the
		** code just after the if and the body of the if is
		** the goto yy_stack in the body.  This way the test
		** can be made before the choice of what to do is needed.
		*/
		{
			/* length of production doubled with extra bit */
			register int yy_len = yyr2[ yy_n ];

			if ( !( yy_len & 01 ) )
			{
				yy_len >>= 1;
				yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
				yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
					*( yy_ps -= yy_len ) + 1;
				if ( yy_state >= YYLAST ||
					yychk[ yy_state =
					yyact[ yy_state ] ] != -yy_n )
				{
					yy_state = yyact[ yypgo[ yy_n ] ];
				}
				goto yy_stack;
			}
			yy_len >>= 1;
			yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
			yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
				*( yy_ps -= yy_len ) + 1;
			if ( yy_state >= YYLAST ||
				yychk[ yy_state = yyact[ yy_state ] ] != -yy_n )
			{
				yy_state = yyact[ yypgo[ yy_n ] ];
			}
		}
					/* save until reenter driver code */
		yystate = yy_state;
		yyps = yy_ps;
		yypv = yy_pv;
	}
	/*
	** code supplied by user is placed in this switch
	*/
	switch( yytmp )
	{
		
case 3:
# line 212 "./c-exp.y"
{ write_exp_elt_opcode(OP_TYPE);
			  write_exp_elt_type(yypvt[-0].tval);
			  write_exp_elt_opcode(OP_TYPE);} break;
case 5:
# line 220 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_COMMA); } break;
case 6:
# line 225 "./c-exp.y"
{ write_exp_elt_opcode (UNOP_IND); } break;
case 7:
# line 228 "./c-exp.y"
{ write_exp_elt_opcode (UNOP_ADDR); } break;
case 8:
# line 231 "./c-exp.y"
{ write_exp_elt_opcode (UNOP_NEG); } break;
case 9:
# line 235 "./c-exp.y"
{ write_exp_elt_opcode (UNOP_LOGICAL_NOT); } break;
case 10:
# line 239 "./c-exp.y"
{ write_exp_elt_opcode (UNOP_COMPLEMENT); } break;
case 11:
# line 243 "./c-exp.y"
{ write_exp_elt_opcode (UNOP_PREINCREMENT); } break;
case 12:
# line 247 "./c-exp.y"
{ write_exp_elt_opcode (UNOP_PREDECREMENT); } break;
case 13:
# line 251 "./c-exp.y"
{ write_exp_elt_opcode (UNOP_POSTINCREMENT); } break;
case 14:
# line 255 "./c-exp.y"
{ write_exp_elt_opcode (UNOP_POSTDECREMENT); } break;
case 15:
# line 259 "./c-exp.y"
{ write_exp_elt_opcode (UNOP_SIZEOF); } break;
case 16:
# line 263 "./c-exp.y"
{ write_exp_elt_opcode (STRUCTOP_PTR);
			  write_exp_string (yypvt[-0].sval);
			  write_exp_elt_opcode (STRUCTOP_PTR); } break;
case 17:
# line 269 "./c-exp.y"
{ /* exp->type::name becomes exp->*(&type::name) */
			  /* Note: this doesn't work if name is a
			     static member!  FIXME */
			  write_exp_elt_opcode (UNOP_ADDR);
			  write_exp_elt_opcode (STRUCTOP_MPTR); } break;
case 18:
# line 276 "./c-exp.y"
{ write_exp_elt_opcode (STRUCTOP_MPTR); } break;
case 19:
# line 280 "./c-exp.y"
{ write_exp_elt_opcode (STRUCTOP_STRUCT);
			  write_exp_string (yypvt[-0].sval);
			  write_exp_elt_opcode (STRUCTOP_STRUCT); } break;
case 20:
# line 286 "./c-exp.y"
{ /* exp.type::name becomes exp.*(&type::name) */
			  /* Note: this doesn't work if name is a
			     static member!  FIXME */
			  write_exp_elt_opcode (UNOP_ADDR);
			  write_exp_elt_opcode (STRUCTOP_MEMBER); } break;
case 21:
# line 294 "./c-exp.y"
{ write_exp_elt_opcode (STRUCTOP_MEMBER); } break;
case 22:
# line 298 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_SUBSCRIPT); } break;
case 23:
# line 304 "./c-exp.y"
{ start_arglist (); } break;
case 24:
# line 306 "./c-exp.y"
{ write_exp_elt_opcode (OP_FUNCALL);
			  write_exp_elt_longcst ((LONGEST) end_arglist ());
			  write_exp_elt_opcode (OP_FUNCALL); } break;
case 25:
# line 312 "./c-exp.y"
{ start_arglist (); } break;
case 27:
# line 319 "./c-exp.y"
{ arglist_len = 1; } break;
case 28:
# line 323 "./c-exp.y"
{ arglist_len++; } break;
case 29:
# line 327 "./c-exp.y"
{ yyval.lval = end_arglist () - 1; } break;
case 30:
# line 330 "./c-exp.y"
{ write_exp_elt_opcode (OP_ARRAY);
			  write_exp_elt_longcst ((LONGEST) 0);
			  write_exp_elt_longcst ((LONGEST) yypvt[-0].lval);
			  write_exp_elt_opcode (OP_ARRAY); } break;
case 31:
# line 337 "./c-exp.y"
{ write_exp_elt_opcode (UNOP_MEMVAL);
			  write_exp_elt_type (yypvt[-2].tval);
			  write_exp_elt_opcode (UNOP_MEMVAL); } break;
case 32:
# line 343 "./c-exp.y"
{ write_exp_elt_opcode (UNOP_CAST);
			  write_exp_elt_type (yypvt[-2].tval);
			  write_exp_elt_opcode (UNOP_CAST); } break;
case 33:
# line 349 "./c-exp.y"
{ } break;
case 34:
# line 355 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_REPEAT); } break;
case 35:
# line 359 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_MUL); } break;
case 36:
# line 363 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_DIV); } break;
case 37:
# line 367 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_REM); } break;
case 38:
# line 371 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_ADD); } break;
case 39:
# line 375 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_SUB); } break;
case 40:
# line 379 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_LSH); } break;
case 41:
# line 383 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_RSH); } break;
case 42:
# line 387 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_EQUAL); } break;
case 43:
# line 391 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_NOTEQUAL); } break;
case 44:
# line 395 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_LEQ); } break;
case 45:
# line 399 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_GEQ); } break;
case 46:
# line 403 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_LESS); } break;
case 47:
# line 407 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_GTR); } break;
case 48:
# line 411 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_BITWISE_AND); } break;
case 49:
# line 415 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_BITWISE_XOR); } break;
case 50:
# line 419 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_BITWISE_IOR); } break;
case 51:
# line 423 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_LOGICAL_AND); } break;
case 52:
# line 427 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_LOGICAL_OR); } break;
case 53:
# line 431 "./c-exp.y"
{ write_exp_elt_opcode (TERNOP_COND); } break;
case 54:
# line 435 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_ASSIGN); } break;
case 55:
# line 439 "./c-exp.y"
{ write_exp_elt_opcode (BINOP_ASSIGN_MODIFY);
			  write_exp_elt_opcode (yypvt[-1].opcode);
			  write_exp_elt_opcode (BINOP_ASSIGN_MODIFY); } break;
case 56:
# line 445 "./c-exp.y"
{ write_exp_elt_opcode (OP_LONG);
			  write_exp_elt_type (yypvt[-0].typed_val.type);
			  write_exp_elt_longcst ((LONGEST)(yypvt[-0].typed_val.val));
			  write_exp_elt_opcode (OP_LONG); } break;
case 57:
# line 452 "./c-exp.y"
{ YYSTYPE val;
			  parse_number (yypvt[-0].ssym.stoken.ptr, yypvt[-0].ssym.stoken.length, 0, &val);
			  write_exp_elt_opcode (OP_LONG);
			  write_exp_elt_type (val.typed_val.type);
			  write_exp_elt_longcst ((LONGEST)val.typed_val.val);
			  write_exp_elt_opcode (OP_LONG);
			} break;
case 58:
# line 463 "./c-exp.y"
{ write_exp_elt_opcode (OP_DOUBLE);
			  write_exp_elt_type (builtin_type_double);
			  write_exp_elt_dblcst (yypvt[-0].dval);
			  write_exp_elt_opcode (OP_DOUBLE); } break;
case 60:
# line 473 "./c-exp.y"
{ write_exp_elt_opcode (OP_LAST);
			  write_exp_elt_longcst ((LONGEST) yypvt[-0].lval);
			  write_exp_elt_opcode (OP_LAST); } break;
case 61:
# line 479 "./c-exp.y"
{ write_exp_elt_opcode (OP_REGISTER);
			  write_exp_elt_longcst ((LONGEST) yypvt[-0].lval);
			  write_exp_elt_opcode (OP_REGISTER); } break;
case 62:
# line 485 "./c-exp.y"
{ write_exp_elt_opcode (OP_INTERNALVAR);
			  write_exp_elt_intern (yypvt[-0].ivar);
			  write_exp_elt_opcode (OP_INTERNALVAR); } break;
case 63:
# line 491 "./c-exp.y"
{ write_exp_elt_opcode (OP_LONG);
			  write_exp_elt_type (builtin_type_int);
			  write_exp_elt_longcst ((LONGEST) TYPE_LENGTH (yypvt[-1].tval));
			  write_exp_elt_opcode (OP_LONG); } break;
case 64:
# line 498 "./c-exp.y"
{ /* C strings are converted into array constants with
			     an explicit null byte added at the end.  Thus
			     the array upper bound is the string length.
			     There is no such thing in C as a completely empty
			     string. */
			  char *sp = yypvt[-0].sval.ptr; int count = yypvt[-0].sval.length;
			  while (count-- > 0)
			    {
			      write_exp_elt_opcode (OP_LONG);
			      write_exp_elt_type (builtin_type_char);
			      write_exp_elt_longcst ((LONGEST)(*sp++));
			      write_exp_elt_opcode (OP_LONG);
			    }
			  write_exp_elt_opcode (OP_LONG);
			  write_exp_elt_type (builtin_type_char);
			  write_exp_elt_longcst ((LONGEST)'\0');
			  write_exp_elt_opcode (OP_LONG);
			  write_exp_elt_opcode (OP_ARRAY);
			  write_exp_elt_longcst ((LONGEST) 0);
			  write_exp_elt_longcst ((LONGEST) (yypvt[-0].sval.length));
			  write_exp_elt_opcode (OP_ARRAY); } break;
case 65:
# line 523 "./c-exp.y"
{ write_exp_elt_opcode (OP_THIS);
			  write_exp_elt_opcode (OP_THIS); } break;
case 66:
# line 530 "./c-exp.y"
{
			  if (yypvt[-0].ssym.sym != 0)
			      yyval.bval = SYMBOL_BLOCK_VALUE (yypvt[-0].ssym.sym);
			  else
			    {
			      struct symtab *tem =
				  lookup_symtab (copy_name (yypvt[-0].ssym.stoken));
			      if (tem)
				yyval.bval = BLOCKVECTOR_BLOCK
					 (BLOCKVECTOR (tem), STATIC_BLOCK);
			      else
				error ("No file or function \"%s\".",
				       copy_name (yypvt[-0].ssym.stoken));
			    }
			} break;
case 67:
# line 548 "./c-exp.y"
{ struct symbol *tem
			    = lookup_symbol (copy_name (yypvt[-0].sval), yypvt[-2].bval,
					     VAR_NAMESPACE, 0, NULL);
			  if (!tem || SYMBOL_CLASS (tem) != LOC_BLOCK)
			    error ("No function \"%s\" in specified context.",
				   copy_name (yypvt[-0].sval));
			  yyval.bval = SYMBOL_BLOCK_VALUE (tem); } break;
case 68:
# line 558 "./c-exp.y"
{ struct symbol *sym;
			  sym = lookup_symbol (copy_name (yypvt[-0].sval), yypvt[-2].bval,
					       VAR_NAMESPACE, 0, NULL);
			  if (sym == 0)
			    error ("No symbol \"%s\" in specified context.",
				   copy_name (yypvt[-0].sval));

			  write_exp_elt_opcode (OP_VAR_VALUE);
			  write_exp_elt_sym (sym);
			  write_exp_elt_opcode (OP_VAR_VALUE); } break;
case 69:
# line 571 "./c-exp.y"
{
			  struct type *type = yypvt[-2].tval;
			  if (TYPE_CODE (type) != TYPE_CODE_STRUCT
			      && TYPE_CODE (type) != TYPE_CODE_UNION)
			    error ("`%s' is not defined as an aggregate type.",
				   TYPE_NAME (type));

			  write_exp_elt_opcode (OP_SCOPE);
			  write_exp_elt_type (type);
			  write_exp_string (yypvt[-0].sval);
			  write_exp_elt_opcode (OP_SCOPE);
			} break;
case 70:
# line 584 "./c-exp.y"
{
			  struct type *type = yypvt[-3].tval;
			  struct stoken tmp_token;
			  if (TYPE_CODE (type) != TYPE_CODE_STRUCT
			      && TYPE_CODE (type) != TYPE_CODE_UNION)
			    error ("`%s' is not defined as an aggregate type.",
				   TYPE_NAME (type));

			  if (!STREQ (type_name_no_tag (type), yypvt[-0].sval.ptr))
			    error ("invalid destructor `%s::~%s'",
				   type_name_no_tag (type), yypvt[-0].sval.ptr);

			  tmp_token.ptr = (char*) alloca (yypvt[-0].sval.length + 2);
			  tmp_token.length = yypvt[-0].sval.length + 1;
			  tmp_token.ptr[0] = '~';
			  memcpy (tmp_token.ptr+1, yypvt[-0].sval.ptr, yypvt[-0].sval.length);
			  tmp_token.ptr[tmp_token.length] = 0;
			  write_exp_elt_opcode (OP_SCOPE);
			  write_exp_elt_type (type);
			  write_exp_string (tmp_token);
			  write_exp_elt_opcode (OP_SCOPE);
			} break;
case 72:
# line 610 "./c-exp.y"
{
			  char *name = copy_name (yypvt[-0].sval);
			  struct symbol *sym;
			  struct minimal_symbol *msymbol;

			  sym =
			    lookup_symbol (name, 0, VAR_NAMESPACE, 0, NULL);
			  if (sym)
			    {
			      write_exp_elt_opcode (OP_VAR_VALUE);
			      write_exp_elt_sym (sym);
			      write_exp_elt_opcode (OP_VAR_VALUE);
			      break;
			    }

			  msymbol = lookup_minimal_symbol (name,
				      (struct objfile *) NULL);
			  if (msymbol != NULL)
			    {
			      write_exp_elt_opcode (OP_LONG);
			      write_exp_elt_type (builtin_type_int);
			      write_exp_elt_longcst ((LONGEST) SYMBOL_VALUE_ADDRESS (msymbol));
			      write_exp_elt_opcode (OP_LONG);
			      write_exp_elt_opcode (UNOP_MEMVAL);
			      if (msymbol -> type == mst_data ||
				  msymbol -> type == mst_bss)
				write_exp_elt_type (builtin_type_int);
			      else if (msymbol -> type == mst_text)
				write_exp_elt_type (lookup_function_type (builtin_type_int));
			      else
				write_exp_elt_type (builtin_type_char);
			      write_exp_elt_opcode (UNOP_MEMVAL);
			    }
			  else
			    if (!have_full_symbols () && !have_partial_symbols ())
			      error ("No symbol table is loaded.  Use the \"file\" command.");
			    else
			      error ("No symbol \"%s\" in current context.", name);
			} break;
case 73:
# line 652 "./c-exp.y"
{ struct symbol *sym = yypvt[-0].ssym.sym;

			  if (sym)
			    {
			      switch (SYMBOL_CLASS (sym))
				{
				case LOC_REGISTER:
				case LOC_ARG:
				case LOC_REF_ARG:
				case LOC_REGPARM:
				case LOC_LOCAL:
				case LOC_LOCAL_ARG:
				  if (innermost_block == 0 ||
				      contained_in (block_found, 
						    innermost_block))
				    innermost_block = block_found;
				case LOC_UNDEF:
				case LOC_CONST:
				case LOC_STATIC:
				case LOC_TYPEDEF:
				case LOC_LABEL:
				case LOC_BLOCK:
				case LOC_CONST_BYTES:

				  /* In this case the expression can
				     be evaluated regardless of what
				     frame we are in, so there is no
				     need to check for the
				     innermost_block.  These cases are
				     listed so that gcc -Wall will
				     report types that may not have
				     been considered.  */

				  break;
				}
			      write_exp_elt_opcode (OP_VAR_VALUE);
			      write_exp_elt_sym (sym);
			      write_exp_elt_opcode (OP_VAR_VALUE);
			    }
			  else if (yypvt[-0].ssym.is_a_field_of_this)
			    {
			      /* C++: it hangs off of `this'.  Must
			         not inadvertently convert from a method call
				 to data ref.  */
			      if (innermost_block == 0 || 
				  contained_in (block_found, innermost_block))
				innermost_block = block_found;
			      write_exp_elt_opcode (OP_THIS);
			      write_exp_elt_opcode (OP_THIS);
			      write_exp_elt_opcode (STRUCTOP_PTR);
			      write_exp_string (yypvt[-0].ssym.stoken);
			      write_exp_elt_opcode (STRUCTOP_PTR);
			    }
			  else
			    {
			      struct minimal_symbol *msymbol;
			      register char *arg = copy_name (yypvt[-0].ssym.stoken);

			      msymbol = lookup_minimal_symbol (arg,
					  (struct objfile *) NULL);
			      if (msymbol != NULL)
				{
				  write_exp_elt_opcode (OP_LONG);
				  write_exp_elt_type (builtin_type_int);
				  write_exp_elt_longcst ((LONGEST) SYMBOL_VALUE_ADDRESS (msymbol));
				  write_exp_elt_opcode (OP_LONG);
				  write_exp_elt_opcode (UNOP_MEMVAL);
				  if (msymbol -> type == mst_data ||
				      msymbol -> type == mst_bss)
				    write_exp_elt_type (builtin_type_int);
				  else if (msymbol -> type == mst_text)
				    write_exp_elt_type (lookup_function_type (builtin_type_int));
				  else
				    write_exp_elt_type (builtin_type_char);
				  write_exp_elt_opcode (UNOP_MEMVAL);
				}
			      else if (!have_full_symbols () && !have_partial_symbols ())
				error ("No symbol table is loaded.  Use the \"file\" command.");
			      else
				error ("No symbol \"%s\" in current context.",
				       copy_name (yypvt[-0].ssym.stoken));
			    }
			} break;
case 75:
# line 740 "./c-exp.y"
{
		  /* This is where the interesting stuff happens.  */
		  int done = 0;
		  int array_size;
		  struct type *follow_type = yypvt[-1].tval;
		  struct type *range_type;
		  
		  while (!done)
		    switch (pop_type ())
		      {
		      case tp_end:
			done = 1;
			break;
		      case tp_pointer:
			follow_type = lookup_pointer_type (follow_type);
			break;
		      case tp_reference:
			follow_type = lookup_reference_type (follow_type);
			break;
		      case tp_array:
			array_size = pop_type_int ();
			if (array_size != -1)
			  {
			    range_type =
			      create_range_type ((struct type *) NULL,
						 builtin_type_int, 0,
						 array_size - 1);
			    follow_type =
			      create_array_type ((struct type *) NULL,
						 follow_type, range_type);
			  }
			else
			  follow_type = lookup_pointer_type (follow_type);
			break;
		      case tp_function:
			follow_type = lookup_function_type (follow_type);
			break;
		      }
		  yyval.tval = follow_type;
		} break;
case 76:
# line 783 "./c-exp.y"
{ push_type (tp_pointer); yyval.voidval = 0; } break;
case 77:
# line 785 "./c-exp.y"
{ push_type (tp_pointer); yyval.voidval = yypvt[-0].voidval; } break;
case 78:
# line 787 "./c-exp.y"
{ push_type (tp_reference); yyval.voidval = 0; } break;
case 79:
# line 789 "./c-exp.y"
{ push_type (tp_reference); yyval.voidval = yypvt[-0].voidval; } break;
case 81:
# line 794 "./c-exp.y"
{ yyval.voidval = yypvt[-1].voidval; } break;
case 82:
# line 796 "./c-exp.y"
{
			  push_type_int (yypvt[-0].lval);
			  push_type (tp_array);
			} break;
case 83:
# line 801 "./c-exp.y"
{
			  push_type_int (yypvt[-0].lval);
			  push_type (tp_array);
			  yyval.voidval = 0;
			} break;
case 84:
# line 807 "./c-exp.y"
{ push_type (tp_function); } break;
case 85:
# line 809 "./c-exp.y"
{ push_type (tp_function); } break;
case 86:
# line 813 "./c-exp.y"
{ yyval.lval = -1; } break;
case 87:
# line 815 "./c-exp.y"
{ yyval.lval = yypvt[-1].typed_val.val; } break;
case 88:
# line 819 "./c-exp.y"
{ yyval.voidval = 0; } break;
case 89:
# line 821 "./c-exp.y"
{ free ((PTR)yypvt[-1].tvec); yyval.voidval = 0; } break;
case 91:
# line 826 "./c-exp.y"
{ yyval.tval = lookup_member_type (builtin_type_int, yypvt[-2].tval); } break;
case 92:
# line 828 "./c-exp.y"
{ yyval.tval = lookup_member_type (yypvt[-5].tval, yypvt[-3].tval); } break;
case 93:
# line 830 "./c-exp.y"
{ yyval.tval = lookup_member_type
			    (lookup_function_type (yypvt[-7].tval), yypvt[-5].tval); } break;
case 94:
# line 833 "./c-exp.y"
{ yyval.tval = lookup_member_type
			    (lookup_function_type (yypvt[-8].tval), yypvt[-6].tval);
			  free ((PTR)yypvt[-1].tvec); } break;
case 95:
# line 840 "./c-exp.y"
{ yyval.tval = yypvt[-0].tsym.type; } break;
case 96:
# line 842 "./c-exp.y"
{ yyval.tval = builtin_type_int; } break;
case 97:
# line 844 "./c-exp.y"
{ yyval.tval = builtin_type_long; } break;
case 98:
# line 846 "./c-exp.y"
{ yyval.tval = builtin_type_short; } break;
case 99:
# line 848 "./c-exp.y"
{ yyval.tval = builtin_type_long; } break;
case 100:
# line 850 "./c-exp.y"
{ yyval.tval = builtin_type_unsigned_long; } break;
case 101:
# line 852 "./c-exp.y"
{ yyval.tval = builtin_type_long_long; } break;
case 102:
# line 854 "./c-exp.y"
{ yyval.tval = builtin_type_long_long; } break;
case 103:
# line 856 "./c-exp.y"
{ yyval.tval = builtin_type_unsigned_long_long; } break;
case 104:
# line 858 "./c-exp.y"
{ yyval.tval = builtin_type_unsigned_long_long; } break;
case 105:
# line 860 "./c-exp.y"
{ yyval.tval = builtin_type_short; } break;
case 106:
# line 862 "./c-exp.y"
{ yyval.tval = builtin_type_unsigned_short; } break;
case 107:
# line 864 "./c-exp.y"
{ yyval.tval = lookup_struct (copy_name (yypvt[-0].sval),
					      expression_context_block); } break;
case 108:
# line 867 "./c-exp.y"
{ yyval.tval = lookup_struct (copy_name (yypvt[-0].sval),
					      expression_context_block); } break;
case 109:
# line 870 "./c-exp.y"
{ yyval.tval = lookup_union (copy_name (yypvt[-0].sval),
					     expression_context_block); } break;
case 110:
# line 873 "./c-exp.y"
{ yyval.tval = lookup_enum (copy_name (yypvt[-0].sval),
					    expression_context_block); } break;
case 111:
# line 876 "./c-exp.y"
{ yyval.tval = lookup_unsigned_typename (TYPE_NAME(yypvt[-0].tsym.type)); } break;
case 112:
# line 878 "./c-exp.y"
{ yyval.tval = builtin_type_unsigned_int; } break;
case 113:
# line 880 "./c-exp.y"
{ yyval.tval = lookup_signed_typename (TYPE_NAME(yypvt[-0].tsym.type)); } break;
case 114:
# line 882 "./c-exp.y"
{ yyval.tval = builtin_type_int; } break;
case 115:
# line 884 "./c-exp.y"
{ yyval.tval = lookup_template_type(copy_name(yypvt[-3].sval), yypvt[-1].tval,
						    expression_context_block);
			} break;
case 116:
# line 888 "./c-exp.y"
{ yyval.tval = yypvt[-0].tval; } break;
case 117:
# line 889 "./c-exp.y"
{ yyval.tval = yypvt[-0].tval; } break;
case 119:
# line 894 "./c-exp.y"
{
		  yyval.tsym.stoken.ptr = "int";
		  yyval.tsym.stoken.length = 3;
		  yyval.tsym.type = builtin_type_int;
		} break;
case 120:
# line 900 "./c-exp.y"
{
		  yyval.tsym.stoken.ptr = "long";
		  yyval.tsym.stoken.length = 4;
		  yyval.tsym.type = builtin_type_long;
		} break;
case 121:
# line 906 "./c-exp.y"
{
		  yyval.tsym.stoken.ptr = "short";
		  yyval.tsym.stoken.length = 5;
		  yyval.tsym.type = builtin_type_short;
		} break;
case 122:
# line 915 "./c-exp.y"
{ yyval.tvec = (struct type **) xmalloc (sizeof (struct type *) * 2);
		  yyval.ivec[0] = 1;	/* Number of types in vector */
		  yyval.tvec[1] = yypvt[-0].tval;
		} break;
case 123:
# line 920 "./c-exp.y"
{ int len = sizeof (struct type *) * (++(yypvt[-2].ivec[0]) + 1);
		  yyval.tvec = (struct type **) xrealloc ((char *) yypvt[-2].tvec, len);
		  yyval.tvec[yyval.ivec[0]] = yypvt[-0].tval;
		} break;
case 124:
# line 926 "./c-exp.y"
{ yyval.sval = yypvt[-0].ssym.stoken; } break;
case 125:
# line 927 "./c-exp.y"
{ yyval.sval = yypvt[-0].ssym.stoken; } break;
case 126:
# line 928 "./c-exp.y"
{ yyval.sval = yypvt[-0].tsym.stoken; } break;
case 127:
# line 929 "./c-exp.y"
{ yyval.sval = yypvt[-0].ssym.stoken; } break;
	}
	goto yystack;		/* reset registers in driver code */
}
