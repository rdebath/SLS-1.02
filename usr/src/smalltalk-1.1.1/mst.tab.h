typedef union{
  char		cval;
  double	fval;
  long		ival;
  char		*sval;
  TreeNode	node;
} YYSTYPE;
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

