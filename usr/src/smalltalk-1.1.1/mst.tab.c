
/*  A Bison parser, made from mst.y  */

#define	BANG	258
#define	COLON	259
#define	UPARROW	260
#define	DOT	261
#define	ASSIGN	262
#define	SHARP	263
#define	SEMICOLON	264
#define	OPEN_PAREN	265
#define	CLOSE_PAREN	266
#define	OPEN_BRACKET	267
#define	CLOSE_BRACKET	268
#define	PRIMITIVE_START	269
#define	INTERNAL_TOKEN	270
#define	IDENTIFIER	271
#define	KEYWORD	272
#define	STRING_LITERAL	273
#define	SYMBOL_KEYWORD	274
#define	BINOP	275
#define	VERTICAL_BAR	276
#define	INTEGER_LITERAL	277
#define	FLOATING_LITERAL	278
#define	CHAR_LITERAL	279

#line 26 "mst.y"

#include "mst.h"
#include "mstsym.h"
#include "msttree.h"
#include "mstdict.h"
#include <stdio.h>
#ifdef HAS_ALLOCA_H
#include <alloca.h>
#endif

#define YYDEBUG 1

extern Boolean		quietExecution;


#line 44 "mst.y"
typedef union{
  char		cval;
  double	fval;
  long		ival;
  char		*sval;
  TreeNode	node;
} YYSTYPE;

#ifndef YYLTYPE
typedef
  struct yyltype
    {
      int timestamp;
      int first_line;
      int first_column;
      int last_line;
      int last_column;
      char *text;
   }
  yyltype;

#define YYLTYPE yyltype
#endif

#include <stdio.h>

#ifndef __STDC__
#define const
#endif



#define	YYFINAL		146
#define	YYFLAG		-32768
#define	YYNTBASE	25

#define YYTRANSLATE(x) ((unsigned)(x) <= 279 ? yytranslate[x] : 71)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     2,     3,     4,     5,
     6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
    16,    17,    18,    19,    20,    21,    22,    23,    24
};

static const short yyprhs[] = {     0,
     0,     2,     5,     7,     9,    12,    16,    19,    22,    26,
    29,    33,    35,    38,    42,    45,    49,    53,    58,    60,
    63,    65,    67,    69,    71,    73,    75,    78,    82,    84,
    88,    91,    95,    97,   100,   101,   103,   106,   108,   112,
   116,   118,   121,   124,   128,   130,   132,   134,   136,   138,
   140,   144,   146,   148,   150,   152,   154,   156,   158,   161,
   163,   165,   167,   169,   171,   173,   176,   179,   183,   185,
   188,   190,   192,   194,   196,   198,   203,   204,   207,   210,
   214,   216,   218,   220,   223,   225,   227,   231,   233,   235,
   238,   241,   245,   248,   251,   255,   257,   260
};

static const short yyrhs[] = {    27,
     0,    26,    32,     0,    15,     0,    28,     0,    27,    28,
     0,    29,    31,     3,     0,    29,     3,     0,    43,     3,
     0,    40,    43,     3,     0,     1,     3,     0,     3,    30,
     3,     0,    46,     0,    32,     3,     0,    31,    32,     3,
     0,    33,    42,     0,    33,    40,    42,     0,    33,    39,
    42,     0,    33,    40,    39,    42,     0,    34,     0,    35,
    36,     0,    37,     0,     1,     0,    16,     0,    20,     0,
    21,     0,    16,     0,    38,    36,     0,    37,    38,    36,
     0,    17,     0,    14,    22,    20,     0,    21,    21,     0,
    21,    41,    21,     0,    36,     0,    41,    36,     0,     0,
    43,     0,     5,    44,     0,    44,     0,    44,     6,    42,
     0,     1,     6,    42,     0,    46,     0,    45,    46,     0,
    36,     7,     0,    45,    36,     7,     0,    47,     0,    61,
     0,    68,     0,    36,     0,    48,     0,    58,     0,    10,
    44,    11,     0,    49,     0,    50,     0,    52,     0,    53,
     0,    54,     0,    22,     0,    23,     0,     8,    51,     0,
    16,     0,    35,     0,    19,     0,    17,     0,    24,     0,
    18,     0,     8,    55,     0,    10,    11,     0,    10,    56,
    11,     0,    57,     0,    56,    57,     0,    49,     0,    51,
     0,    53,     0,    52,     0,    55,     0,    12,    59,    42,
    13,     0,     0,    60,    21,     0,     4,    36,     0,    60,
     4,    36,     0,    62,     0,    64,     0,    66,     0,    63,
    34,     0,    47,     0,    62,     0,    65,    35,    63,     0,
    63,     0,    64,     0,    65,    67,     0,    38,    65,     0,
    67,    38,    65,     0,    61,    69,     0,     9,    70,     0,
    69,     9,    70,     0,    34,     0,    35,    63,     0,    67,
     0
};

#if YYDEBUG != 0
static const short yyrline[] = { 0,
    79,    81,    84,    87,    89,    92,    94,    95,   101,   108,
   111,   115,   148,   156,   165,   168,   170,   172,   177,   179,
   181,   182,   187,   191,   193,   196,   200,   202,   207,   211,
   219,   221,   225,   227,   231,   233,   236,   240,   241,   244,
   251,   253,   256,   258,   263,   265,   266,   269,   271,   272,
   273,   276,   278,   279,   280,   281,   284,   286,   289,   293,
   295,   296,   297,   301,   305,   309,   313,   315,   319,   321,
   326,   328,   329,   330,   331,   334,   339,   341,   347,   349,
   354,   356,   357,   360,   364,   366,   369,   374,   376,   379,
   384,   387,   392,   397,   399,   404,   406,   408
};

static const char * const yytname[] = {   "$",
"error","$illegal.","BANG","COLON","UPARROW","DOT","ASSIGN","SHARP","SEMICOLON","OPEN_PAREN",
"CLOSE_PAREN","OPEN_BRACKET","CLOSE_BRACKET","PRIMITIVE_START","INTERNAL_TOKEN","IDENTIFIER","KEYWORD","STRING_LITERAL","SYMBOL_KEYWORD","BINOP",
"VERTICAL_BAR","INTEGER_LITERAL","FLOATING_LITERAL","CHAR_LITERAL","program","internal_marker","class_definition_list","class_definition","class_header","class_specification",
"method_list","method","message_pattern","unary_selector","binary_selector","variable_name","keyword_variable_list","keyword","primitive","temporaries",
"variable_names","statements","non_empty_statements","expression","assigns","simple_expression","primary","literal","number","symbol_constant",
"symbol","character_constant","string","array_constant","array","array_constant_list","array_constant_elt","block","opt_block_variables","block_variable_list",
"message_expression","unary_expression","unary_object_description","binary_expression","binary_object_description","keyword_expression","keyword_binary_object_description_list","cascaded_message_expression","semi_message_list","message_elt",
""
};
#endif

static const short yyr1[] = {     0,
    25,    25,    26,    27,    27,    28,    28,    28,    28,    28,
    29,    30,    31,    31,    32,    32,    32,    32,    33,    33,
    33,    33,    34,    35,    35,    36,    37,    37,    38,    39,
    40,    40,    41,    41,    42,    42,    43,    43,    43,    43,
    44,    44,    45,    45,    46,    46,    46,    47,    47,    47,
    47,    48,    48,    48,    48,    48,    49,    49,    50,    51,
    51,    51,    51,    52,    53,    54,    55,    55,    56,    56,
    57,    57,    57,    57,    57,    58,    59,    59,    60,    60,
    61,    61,    61,    62,    63,    63,    64,    65,    65,    66,
    67,    67,    68,    69,    69,    70,    70,    70
};

static const short yyr2[] = {     0,
     1,     2,     1,     1,     2,     3,     2,     2,     3,     2,
     3,     1,     2,     3,     2,     3,     3,     4,     1,     2,
     1,     1,     1,     1,     1,     1,     2,     3,     1,     3,
     2,     3,     1,     2,     0,     1,     2,     1,     3,     3,
     1,     2,     2,     3,     1,     1,     1,     1,     1,     1,
     3,     1,     1,     1,     1,     1,     1,     1,     2,     1,
     1,     1,     1,     1,     1,     2,     2,     3,     1,     2,
     1,     1,     1,     1,     1,     4,     0,     2,     2,     3,
     1,     1,     1,     2,     1,     1,     3,     1,     1,     2,
     2,     3,     2,     2,     3,     1,     2,     1
};

static const short yydefact[] = {     0,
     0,     0,     0,     0,     0,    77,     3,    26,    65,     0,
    57,    58,    64,     0,     0,     4,     0,    48,     0,     0,
    38,     0,    41,    45,    49,    52,    53,    54,    55,    56,
    50,    46,    81,    88,    82,     0,    83,    47,    10,     0,
     0,    48,    12,    37,     0,    60,    63,    62,    24,    25,
    61,    59,    66,     0,     0,     0,     0,    31,    33,     0,
    22,    23,    29,     2,     0,    19,     0,    21,     0,     5,
     7,     0,     0,    43,     0,     0,     8,     0,    48,    42,
     0,    93,    84,     0,     0,    90,    40,    36,    11,    67,
    71,    72,    74,    73,    75,     0,    69,    51,    79,     0,
     0,    78,    32,    34,     0,     0,     0,    15,    20,     0,
    27,     6,     0,    13,     9,    39,    44,    96,     0,    98,
    94,     0,    85,    86,    87,    89,    91,     0,    68,    70,
    76,    80,     0,    17,     0,    16,    28,    14,    97,    95,
    92,    30,    18,     0,     0,     0
};

static const short yydefgoto[] = {   144,
    14,    15,    16,    17,    41,    72,    64,    65,    66,    51,
    18,    68,    69,   106,    19,    60,    87,    88,    21,    22,
    23,    24,    25,    26,    27,    92,    28,    29,    30,    95,
    96,    97,    31,    56,    57,    32,    33,    34,    35,    36,
    37,   120,    38,    82,   121
};

static const short yypact[] = {   247,
    44,   294,   294,   339,   294,    36,-32768,-32768,-32768,    22,
-32768,-32768,-32768,    98,   148,-32768,    15,    39,   291,    49,
    64,   294,-32768,    57,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,    56,    59,    66,    90,   142,-32768,-32768,-32768,   173,
    82,-32768,-32768,-32768,   309,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,    77,    74,   271,    16,-32768,-32768,    28,
-32768,-32768,-32768,-32768,   123,-32768,    74,    81,    74,-32768,
-32768,    80,    88,-32768,    96,    91,-32768,   173,    97,-32768,
   257,   100,-32768,   294,   294,    81,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,   324,-32768,-32768,-32768,    92,
    74,-32768,-32768,-32768,    94,   223,   198,-32768,-32768,    74,
-32768,-32768,   114,-32768,-32768,-32768,-32768,-32768,   294,    81,
-32768,   257,-32768,-32768,    66,-32768,    13,   294,-32768,-32768,
-32768,-32768,   102,-32768,   223,-32768,-32768,-32768,    66,-32768,
    13,-32768,-32768,   120,   125,-32768
};

static const short yypgoto[] = {-32768,
-32768,-32768,   117,-32768,-32768,-32768,   -11,-32768,   -33,   -14,
     2,-32768,   -27,    27,    71,-32768,   -51,    11,    18,-32768,
    17,   -77,-32768,   -43,-32768,   134,   -32,   -30,-32768,   136,
-32768,    33,-32768,-32768,-32768,-32768,   -56,   -74,   -68,   -60,
-32768,   106,-32768,-32768,    21
};


#define	YYLAST		360


static const short yytable[] = {    67,
    83,    91,    67,    42,   100,    73,   123,   123,    85,   125,
    20,    59,    93,   108,    94,    61,   126,    71,    43,   101,
    44,    84,    54,    79,   127,    20,   116,   124,   124,    76,
    62,    63,    49,    50,    49,    50,   102,     8,    80,    55,
   110,   123,    58,     8,   139,    74,    39,   118,   103,    40,
   123,    77,    91,    85,   134,   136,    99,    67,   128,   126,
   113,   104,   124,    93,    81,    94,   119,   141,   109,    78,
   111,   124,   -85,   -85,   -86,   -86,   -85,   -85,   -86,   -86,
    61,    62,   112,   143,    89,    42,    42,    98,   118,     8,
   114,    83,   128,   115,    85,    62,    63,    63,    61,    49,
    50,    40,   132,   117,   131,    83,   -89,   119,   122,   -89,
   -89,   137,    84,    62,    63,   133,   138,    49,    50,   145,
    42,   142,   -35,    75,   146,   -35,    84,     3,   130,    42,
     4,    70,     5,   135,     6,   107,   105,    52,     8,    53,
     9,    86,   140,    10,    11,    12,    13,    -1,     1,     0,
     2,     0,     3,     0,     0,     4,     0,     5,    63,     6,
     0,    49,    50,     8,     0,     9,     0,     0,    10,    11,
    12,    13,   -35,    75,     0,   -35,     0,     3,     0,     0,
     4,     0,     5,     0,     6,   -35,     0,     0,     8,     0,
     9,     0,     0,     0,    11,    12,    13,   -35,    75,     0,
   -35,     0,     3,     0,     0,     4,     0,     5,     0,     6,
     0,   105,     0,     8,     0,     9,     0,     0,     0,    11,
    12,    13,   -35,    75,     0,   -35,     0,     3,     0,     0,
     4,     0,     5,     0,     6,     0,     0,     0,     8,     0,
     9,     0,     0,     0,    11,    12,    13,     1,     0,     2,
     0,     3,     0,     0,     4,     0,     5,     0,     6,     0,
     0,     7,     8,     0,     9,     0,     0,    10,    11,    12,
    13,    75,    62,    63,     0,     3,    49,    50,     4,     0,
     5,     0,     6,   -35,     0,     0,     8,     0,     9,     0,
     0,    75,    11,    12,    13,     3,     0,     0,     4,     0,
     5,     4,     6,     5,     0,     6,     8,     0,     9,     8,
     0,     9,    11,    12,    13,    11,    12,    13,    45,    90,
     0,     0,     0,     0,    46,    47,     9,    48,    49,    50,
    11,    12,    13,    45,   129,     0,     0,     0,     0,    46,
    47,     9,    48,    49,    50,    11,    12,    13,    45,     0,
     0,     0,     0,     0,    46,    47,     0,    48,    49,    50
};

static const short yycheck[] = {    14,
    34,    45,    17,     2,    56,    17,    84,    85,    36,    84,
     0,    10,    45,    65,    45,     1,    85,     3,     2,     4,
     3,    36,     5,    22,    85,    15,    78,    84,    85,    19,
    16,    17,    20,    21,    20,    21,    21,    16,    22,     4,
    68,   119,    21,    16,   119,     7,     3,    81,    21,     6,
   128,     3,    96,    81,   106,   107,    55,    72,    86,   128,
    72,    60,   119,    96,     9,    96,    81,   128,    67,     6,
    69,   128,    16,    17,    16,    17,    20,    21,    20,    21,
     1,    16,     3,   135,     3,    84,    85,    11,   122,    16,
     3,   125,   120,     3,   122,    16,    17,    17,     1,    20,
    21,     6,   101,     7,    13,   139,    17,   122,     9,    20,
    21,   110,   127,    16,    17,    22,     3,    20,    21,     0,
   119,    20,     0,     1,     0,     3,   141,     5,    96,   128,
     8,    15,    10,   107,    12,    65,    14,     4,    16,     4,
    18,    36,   122,    21,    22,    23,    24,     0,     1,    -1,
     3,    -1,     5,    -1,    -1,     8,    -1,    10,    17,    12,
    -1,    20,    21,    16,    -1,    18,    -1,    -1,    21,    22,
    23,    24,     0,     1,    -1,     3,    -1,     5,    -1,    -1,
     8,    -1,    10,    -1,    12,    13,    -1,    -1,    16,    -1,
    18,    -1,    -1,    -1,    22,    23,    24,     0,     1,    -1,
     3,    -1,     5,    -1,    -1,     8,    -1,    10,    -1,    12,
    -1,    14,    -1,    16,    -1,    18,    -1,    -1,    -1,    22,
    23,    24,     0,     1,    -1,     3,    -1,     5,    -1,    -1,
     8,    -1,    10,    -1,    12,    -1,    -1,    -1,    16,    -1,
    18,    -1,    -1,    -1,    22,    23,    24,     1,    -1,     3,
    -1,     5,    -1,    -1,     8,    -1,    10,    -1,    12,    -1,
    -1,    15,    16,    -1,    18,    -1,    -1,    21,    22,    23,
    24,     1,    16,    17,    -1,     5,    20,    21,     8,    -1,
    10,    -1,    12,    13,    -1,    -1,    16,    -1,    18,    -1,
    -1,     1,    22,    23,    24,     5,    -1,    -1,     8,    -1,
    10,     8,    12,    10,    -1,    12,    16,    -1,    18,    16,
    -1,    18,    22,    23,    24,    22,    23,    24,    10,    11,
    -1,    -1,    -1,    -1,    16,    17,    18,    19,    20,    21,
    22,    23,    24,    10,    11,    -1,    -1,    -1,    -1,    16,
    17,    18,    19,    20,    21,    22,    23,    24,    10,    -1,
    -1,    -1,    -1,    -1,    16,    17,    -1,    19,    20,    21
};
#define YYPURE 1

/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/home3/sbb/gnu/lib/bison.simple"

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Bob Corbett and Richard Stallman

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


#ifndef alloca
#ifdef __GNUC__
#define alloca __builtin_alloca
#else /* Not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__)
#include <alloca.h>
#else /* Not sparc */
#ifdef MSDOS
#include <malloc.h>
#endif /* MSDOS */
#endif /* Not sparc.  */
#endif /* Not GNU C.  */
#endif /* alloca not defined.  */

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	return(0)
#define YYABORT 	return(1)
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(token, value) \
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    { yychar = (token), yylval = (value);			\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { yyerror ("syntax error: cannot back up"); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYPURE
#define YYLEX		yylex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#define YYLEX		yylex(&yylval, &yylloc)
#else
#define YYLEX		yylex(&yylval)
#endif
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int	yychar;			/*  the lookahead symbol		*/
YYSTYPE	yylval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef YYLSP_NEEDED
YYLTYPE yylloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int yynerrs;			/*  number of parse errors so far       */
#endif  /* not YYPURE */

#if YYDEBUG != 0
int yydebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_bcopy (from, to, count)
     char *from;
     char *to;
     int count;
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_bcopy (char *from, char *to, int count)
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif

#line 160 "/home3/sbb/gnu/lib/bison.simple"
int
yyparse()
{
  register int yystate;
  register int yyn;
  register short *yyssp;
  register YYSTYPE *yyvsp;
  int yyerrstatus;	/*  number of tokens to shift before error messages enabled */
  int yychar1;		/*  lookahead token as an internal (translated) token number */

  short	yyssa[YYINITDEPTH];	/*  the state stack			*/
  YYSTYPE yyvsa[YYINITDEPTH];	/*  the semantic value stack		*/

  short *yyss = yyssa;		/*  refer to the stacks thru separate pointers */
  YYSTYPE *yyvs = yyvsa;	/*  to allow yyoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;
  YYLTYPE yylsa[YYINITDEPTH];	/*  the location stack			*/

#define YYPOPSTACK   (yyvsp--, yysp--, yylsp--)
#else
#define YYPOPSTACK   (yyvsp--, yysp--)
#endif

  int yystacksize = YYINITDEPTH;

#ifdef YYPURE
  int yychar;
  YYSTYPE yylval;
  int yynerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE yylloc;
#endif
#endif

  YYSTYPE yyval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int yylen;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Starting parse\n");
#endif

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.  */

  yyssp = yyss - 1;
  yyvsp = yyvs;
#ifdef YYLSP_NEEDED
  yylsp = yyls;
#endif

/* Push a new state, which is found in  yystate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
yynewstate:

  *++yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *yyvs1 = yyvs;
      short *yyss1 = yyss;
#ifdef YYLSP_NEEDED
      YYLTYPE *yyls1 = yyls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = yyssp - yyss + 1;

#ifdef yyoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
#ifdef YYLSP_NEEDED
		 &yyls1, size * sizeof (*yylsp),
#endif
		 &yystacksize);

      yyss = yyss1; yyvs = yyvs1;
#ifdef YYLSP_NEEDED
      yyls = yyls1;
#endif
#else /* no yyoverflow */
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	{
	  yyerror("parser stack overflow");
	  return 2;
	}
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;
      yyss = (short *) alloca (yystacksize * sizeof (*yyssp));
      __yy_bcopy ((char *)yyss1, (char *)yyss, size * sizeof (*yyssp));
      yyvs = (YYSTYPE *) alloca (yystacksize * sizeof (*yyvsp));
      __yy_bcopy ((char *)yyvs1, (char *)yyvs, size * sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) alloca (yystacksize * sizeof (*yylsp));
      __yy_bcopy ((char *)yyls1, (char *)yyls, size * sizeof (*yylsp));
#endif
#endif /* no yyoverflow */

      yyssp = yyss + size - 1;
      yyvsp = yyvs + size - 1;
#ifdef YYLSP_NEEDED
      yylsp = yyls + size - 1;
#endif

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Stack size increased to %d\n", yystacksize);
#endif

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Entering state %d\n", yystate);
#endif

 yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Reading a token: ");
#endif
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      yychar1 = YYTRANSLATE(yychar);

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Next token is %d (%s)\n", yychar, yytname[yychar1]);
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting token %d (%s), ", yychar, yytname[yychar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (yyerrstatus) yyerrstatus--;

  yystate = yyn;
  goto yynewstate;

/* Do the default action for the current state.  */
yydefault:

  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;

/* Do a reduction.  yyn is the number of a rule to reduce with.  */
yyreduce:
  yylen = yyr2[yyn];
  yyval = yyvsp[1-yylen]; /* implement default value of the action */

#if YYDEBUG != 0
  if (yydebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
	       yyn, yyrline[yyn]);

      /* Print the symboles being reduced, and their result.  */
      for (i = yyprhs[yyn]; yyrhs[i] > 0; i++)
	fprintf (stderr, "%s ", yytname[yyrhs[i]]);
      fprintf (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif


  switch (yyn) {

case 2:
#line 81 "mst.y"
{ compileMethod(yyvsp[0].node); ;
    break;}
case 3:
#line 85 "mst.y"
{ clearMethodStartPos(); ;
    break;}
case 8:
#line 95 "mst.y"
{ if (!hadError) {
					    executeStatements(nil, yyvsp[-1].node,
							    quietExecution); 
					  }
					  hadError = false;
					;
    break;}
case 9:
#line 102 "mst.y"
{ if (!hadError) {
					    executeStatements(yyvsp[-2].node, yyvsp[-1].node,
							    quietExecution); 
                                          }
					  hadError = false;
                                        ;
    break;}
case 10:
#line 108 "mst.y"
{ hadError = false; ;
    break;}
case 11:
#line 112 "mst.y"
{ clearMethodStartPos(); ;
    break;}
case 12:
#line 116 "mst.y"
{ executeStatements(nil, 
				    makeStatementList(yyvsp[0].node, nil), true); ;
    break;}
case 13:
#line 149 "mst.y"
{ if (!hadError) {
					    compileMethod(yyvsp[-1].node);
					    clearMethodStartPos();
					  } else {
					    hadError = false;
					  }
					;
    break;}
case 14:
#line 156 "mst.y"
{ if (!hadError) {
					    compileMethod(yyvsp[-1].node);
					    clearMethodStartPos();
					  } else {
					    hadError = false;
					  }
					;
    break;}
case 15:
#line 167 "mst.y"
{ yyval.node = makeMethod(yyvsp[-1].node, nil, 0, yyvsp[0].node); ;
    break;}
case 16:
#line 169 "mst.y"
{ yyval.node = makeMethod(yyvsp[-2].node, yyvsp[-1].node, 0, yyvsp[0].node); ;
    break;}
case 17:
#line 171 "mst.y"
{ yyval.node = makeMethod(yyvsp[-2].node, nil, yyvsp[-1].ival, yyvsp[0].node); ;
    break;}
case 18:
#line 173 "mst.y"
{ yyval.node = makeMethod(yyvsp[-3].node, yyvsp[-2].node, yyvsp[-1].ival, yyvsp[0].node); ;
    break;}
case 19:
#line 178 "mst.y"
{ yyval.node = makeUnaryExpr(nil, yyvsp[0].sval); ;
    break;}
case 20:
#line 179 "mst.y"
{ yyval.node = makeBinaryExpr(nil, yyvsp[-1].sval,
						              yyvsp[0].node); ;
    break;}
case 21:
#line 181 "mst.y"
{ yyval.node = makeKeywordExpr(nil, yyvsp[0].node); ;
    break;}
case 22:
#line 182 "mst.y"
{ errorf("Invalid message pattern");
					  hadError = true;
					  yyval.node = nil; ;
    break;}
case 26:
#line 197 "mst.y"
{ yyval.node = makeVariable(yyvsp[0].sval); ;
    break;}
case 27:
#line 201 "mst.y"
{ yyval.node = makeKeywordList(yyvsp[-1].sval, yyvsp[0].node); ;
    break;}
case 28:
#line 203 "mst.y"
{ addNode(yyvsp[-2].node, makeKeywordList(yyvsp[-1].sval, yyvsp[0].node));
					  yyval.node = yyvsp[-2].node; ;
    break;}
case 30:
#line 213 "mst.y"
{ yyval.ival = yyvsp[-1].ival;
					  if (strcmp(yyvsp[0].sval, ">") != 0) {
					    YYERROR;
					  }
					;
    break;}
case 31:
#line 220 "mst.y"
{ yyval.node = nil; ;
    break;}
case 32:
#line 222 "mst.y"
{ yyval.node = yyvsp[-1].node; ;
    break;}
case 33:
#line 226 "mst.y"
{ yyval.node = makeVariableList(yyvsp[0].node); ;
    break;}
case 34:
#line 227 "mst.y"
{ addNode(yyvsp[-1].node, makeVariableList(yyvsp[0].node));
					  yyval.node = yyvsp[-1].node; ;
    break;}
case 35:
#line 232 "mst.y"
{ yyval.node = nil; ;
    break;}
case 37:
#line 238 "mst.y"
{ yyval.node = makeStatementList(makeReturn(yyvsp[0].node),
				       			nil); ;
    break;}
case 38:
#line 240 "mst.y"
{ yyval.node = makeStatementList(yyvsp[0].node, nil); ;
    break;}
case 39:
#line 243 "mst.y"
{ yyval.node = makeStatementList(yyvsp[-2].node, yyvsp[0].node); ;
    break;}
case 40:
#line 244 "mst.y"
{ yyval.node = yyvsp[0].node;
				  yyerrok;
				  errorf("Error in expression");
				  hadError = true;
				;
    break;}
case 42:
#line 253 "mst.y"
{ yyval.node = makeAssign(yyvsp[-1].node, yyvsp[0].node); ;
    break;}
case 43:
#line 257 "mst.y"
{ yyval.node = makeVariableList(yyvsp[-1].node); ;
    break;}
case 44:
#line 259 "mst.y"
{ addNode(yyvsp[-2].node, makeVariableList(yyvsp[-1].node));
					  yyval.node = yyvsp[-2].node; ;
    break;}
case 51:
#line 273 "mst.y"
{ yyval.node = yyvsp[-1].node; ;
    break;}
case 57:
#line 285 "mst.y"
{ yyval.node = makeIntConstant(yyvsp[0].ival); ;
    break;}
case 58:
#line 286 "mst.y"
{ yyval.node = makeFloatConstant(yyvsp[0].fval); ;
    break;}
case 59:
#line 290 "mst.y"
{ yyval.node = makeSymbolConstant(yyvsp[0].node); ;
    break;}
case 60:
#line 294 "mst.y"
{ yyval.node = internIdent(yyvsp[0].sval); ;
    break;}
case 61:
#line 295 "mst.y"
{ yyval.node = internBinOP(yyvsp[0].sval); ;
    break;}
case 62:
#line 296 "mst.y"
{ yyval.node = internIdent(yyvsp[0].sval); ;
    break;}
case 63:
#line 297 "mst.y"
{ yyval.node = internIdent(yyvsp[0].sval); ;
    break;}
case 64:
#line 302 "mst.y"
{ yyval.node = makeCharConstant(yyvsp[0].cval); ;
    break;}
case 65:
#line 306 "mst.y"
{ yyval.node = makeStringConstant(yyvsp[0].sval); ;
    break;}
case 66:
#line 310 "mst.y"
{ yyval.node = makeArrayConstant(yyvsp[0].node); ;
    break;}
case 67:
#line 314 "mst.y"
{ yyval.node = nil; ;
    break;}
case 68:
#line 316 "mst.y"
{ yyval.node = yyvsp[-1].node; ;
    break;}
case 69:
#line 320 "mst.y"
{ yyval.node = makeArrayElt(yyvsp[0].node); ;
    break;}
case 70:
#line 322 "mst.y"
{ addNode(yyvsp[-1].node, makeArrayElt(yyvsp[0].node));
					  yyval.node = yyvsp[-1].node; ;
    break;}
case 76:
#line 336 "mst.y"
{ yyval.node = makeBlock(yyvsp[-2].node, yyvsp[-1].node); ;
    break;}
case 77:
#line 340 "mst.y"
{ yyval.node = nil; ;
    break;}
case 79:
#line 348 "mst.y"
{ yyval.node = makeVariableList(yyvsp[0].node); ;
    break;}
case 80:
#line 350 "mst.y"
{ addNode(yyvsp[-2].node, makeVariableList(yyvsp[0].node));
					  yyval.node = yyvsp[-2].node; ;
    break;}
case 84:
#line 361 "mst.y"
{ yyval.node = makeUnaryExpr(yyvsp[-1].node, yyvsp[0].sval); ;
    break;}
case 87:
#line 371 "mst.y"
{ yyval.node = makeBinaryExpr(yyvsp[-2].node, yyvsp[-1].sval, yyvsp[0].node); ;
    break;}
case 90:
#line 381 "mst.y"
{ yyval.node = makeKeywordExpr(yyvsp[-1].node, yyvsp[0].node); ;
    break;}
case 91:
#line 386 "mst.y"
{ yyval.node = makeKeywordList(yyvsp[-1].sval, yyvsp[0].node); ;
    break;}
case 92:
#line 388 "mst.y"
{ addNode(yyvsp[-2].node, makeKeywordList(yyvsp[-1].sval, yyvsp[0].node));
					  yyval.node = yyvsp[-2].node; ;
    break;}
case 93:
#line 394 "mst.y"
{ yyval.node = makeCascadedMessage(yyvsp[-1].node, yyvsp[0].node); ;
    break;}
case 94:
#line 398 "mst.y"
{ yyval.node = makeMessageList(yyvsp[0].node); ;
    break;}
case 95:
#line 400 "mst.y"
{ addNode(yyvsp[-2].node, makeMessageList(yyvsp[0].node));
					  yyval.node = yyvsp[-2].node; ;
    break;}
case 96:
#line 405 "mst.y"
{ yyval.node = makeUnaryExpr(nil, yyvsp[0].sval); ;
    break;}
case 97:
#line 407 "mst.y"
{ yyval.node = makeBinaryExpr(nil, yyvsp[-1].sval, yyvsp[0].node); ;
    break;}
case 98:
#line 409 "mst.y"
{ yyval.node = makeKeywordExpr(nil, yyvsp[0].node); ;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 423 "/home3/sbb/gnu/lib/bison.simple"

  yyvsp -= yylen;
  yyssp -= yylen;
#ifdef YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;

#ifdef YYLSP_NEEDED
  yylsp++;
  if (yylen == 0)
    {
      yylsp->first_line = yylloc.first_line;
      yylsp->first_column = yylloc.first_column;
      yylsp->last_line = (yylsp-1)->last_line;
      yylsp->last_column = (yylsp-1)->last_column;
      yylsp->text = 0;
    }
  else
    {
      yylsp->last_line = (yylsp+yylen-1)->last_line;
      yylsp->last_column = (yylsp+yylen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;

yyerrlab:   /* here on detecting error */

  if (! yyerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  for (x = 0; x < (sizeof(yytname) / sizeof(char *)); x++)
	    if (yycheck[x + yyn] == x)
	      size += strlen(yytname[x]) + 15, count++;
	  msg = (char *) xmalloc(size + 15);
	  strcpy(msg, "parse error");

	  if (count < 5)
	    {
	      count = 0;
	      for (x = 0; x < (sizeof(yytname) / sizeof(char *)); x++)
		if (yycheck[x + yyn] == x)
		  {
		    strcat(msg, count == 0 ? ", expecting `" : " or `");
		    strcat(msg, yytname[x]);
		    strcat(msg, "'");
		    count++;
		  }
	    }
	  yyerror(msg);
	  free(msg);
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror("parse error");
    }

yyerrlab1:   /* here on error raised explicitly by an action */

  if (yyerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Discarding token %d (%s).\n", yychar, yytname[yychar1]);
#endif

      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;

yyerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  yyn = yydefact[yystate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (yyn) goto yydefault;
#endif

yyerrpop:   /* pop the current state because it cannot handle the error token */

  if (yyssp == yyss) YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#ifdef YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

yyerrhandle:

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;
}
#line 413 "mst.y"

/*     
ADDITIONAL C CODE
*/

