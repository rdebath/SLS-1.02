
# line 68 "gram.y"
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "sc.h"

#define ENULL (struct enode *)0

char *strcpy();

# line 77 "gram.y"
typedef union  {
    int ival;
    double fval;
    struct ent_ptr ent;
    struct enode *enode;
    char *sval;
    struct range_s rval;
} YYSTYPE;
# define STRING 257
# define NUMBER 258
# define FNUMBER 259
# define RANGE 260
# define VAR 261
# define WORD 262
# define COL 263
# define S_FORMAT 264
# define S_LABEL 265
# define S_LEFTSTRING 266
# define S_RIGHTSTRING 267
# define S_GET 268
# define S_PUT 269
# define S_MERGE 270
# define S_LET 271
# define S_WRITE 272
# define S_TBL 273
# define S_COPY 274
# define S_SHOW 275
# define S_ERASE 276
# define S_FILL 277
# define S_GOTO 278
# define S_DEFINE 279
# define S_UNDEFINE 280
# define S_VALUE 281
# define S_MDIR 282
# define S_HIDE 283
# define S_SET 284
# define K_FIXED 285
# define K_SUM 286
# define K_PROD 287
# define K_AVG 288
# define K_STDDEV 289
# define K_COUNT 290
# define K_ACOS 291
# define K_ASIN 292
# define K_ATAN 293
# define K_ATAN2 294
# define K_CEIL 295
# define K_COS 296
# define K_EXP 297
# define K_FABS 298
# define K_FLOOR 299
# define K_HYPOT 300
# define K_LN 301
# define K_LOG 302
# define K_PI 303
# define K_POW 304
# define K_SIN 305
# define K_SQRT 306
# define K_TAN 307
# define K_DTR 308
# define K_RTD 309
# define K_MAX 310
# define K_MIN 311
# define K_RND 312
# define K_PV 313
# define K_FV 314
# define K_PMT 315
# define K_HOUR 316
# define K_MINUTE 317
# define K_SECOND 318
# define K_MONTH 319
# define K_DAY 320
# define K_YEAR 321
# define K_NOW 322
# define K_DATE 323
# define K_FMT 324
# define K_SUBSTR 325
# define K_STON 326
# define K_EQS 327
# define K_EXT 328
# define K_NVAL 329
# define K_SVAL 330
# define K_LOOKUP 331
# define K_INDEX 332
# define K_STINDEX 333
# define K_AUTO 334
# define K_AUTOCALC 335
# define K_BYROWS 336
# define K_BYCOLS 337
# define K_BYGRAPH 338
# define K_ITERATIONS 339
# define K_NUMERIC 340
# define K_PRESCALE 341
# define K_EXTFUN 342
# define K_CELLCUR 343
# define K_TOPROW 344
# define K_TBLSTYLE 345
# define K_TBL 346
# define K_LATEX 347
# define K_TEX 348
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern short yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 600
#endif
YYSTYPE yylval, yyval;
# define YYERRCODE 256
short yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
-1, 218,
	60, 0,
	61, 0,
	62, 0,
	33, 0,
	-2, 106,
-1, 220,
	60, 0,
	61, 0,
	62, 0,
	33, 0,
	-2, 107,
-1, 221,
	60, 0,
	61, 0,
	62, 0,
	33, 0,
	-2, 108,
-1, 276,
	60, 0,
	61, 0,
	62, 0,
	33, 0,
	-2, 111,
-1, 277,
	60, 0,
	61, 0,
	62, 0,
	33, 0,
	-2, 113,
-1, 278,
	60, 0,
	61, 0,
	62, 0,
	33, 0,
	-2, 112,
-1, 286,
	41, 125,
	-2, 38,
	};
# define YYNPROD 158
# define YYLAST 2717
short yyact[]={

 378, 152, 153, 154, 155, 156, 159, 160, 161, 162,
 163, 164, 165, 166, 167, 168, 169, 170,  89, 171,
 172, 173, 174, 175, 176, 157, 158, 177, 178, 179,
 180, 181, 182, 183, 184, 185, 186, 187, 190, 191,
 198, 188, 189, 195, 196, 197, 193, 192, 194, 377,
 128, 129, 126, 127, 123, 121, 130, 131, 132, 133,
 134,  46,  67,  44, 100, 207,  45,  29,  43, 118,
 119, 120, 102,  34, 117,  26,  26,  26,  26, 274,
  37,  37,  37,  37,  37,  37,  66,  29,  48,  26,
  26,  26,  26,  37,  26, 206, 205, 204,  71, 124,
  56, 122,  55, 115,  24, 142, 199, 225,  31,  32,
  33,  88,  64,  48,  48,  48, 136, 135,  29,  70,
  26,  49,  50,  52,  57, 141,  61,  69,  68,  63,
 139,  77,  76,  84, 408, 140, 407, 114, 211, 212,
 213, 214, 215, 216, 217, 218, 220, 221, 223, 224,
  23, 226,  78, 208, 209, 210, 406, 336,   6,   3,
   4,   5,   7,  10,   8,   2,  11,  12,  15,  13,
  16,  18,  19,  20,  21,  17,   9,  14,  22, 334,
 397, 113, 142, 396,  29, 333, 395, 332, 105, 396,
 331, 106, 330, 107, 113, 329, 272,  29, 271, 270,
 269, 105, 268, 267, 106, 266, 107,  29, 265, 264,
 263,  85, 104, 262,  56, 261,  55, 260, 259, 258,
 276, 257, 222, 277, 256, 104, 278, 255, 254, 253,
 252, 251, 250, 285, 288, 289, 290, 291, 292, 293,
 294, 295, 296, 297, 298, 299, 300, 301, 302, 303,
 304, 305, 306, 307, 308, 309, 310, 311, 312, 313,
 314, 315, 316, 317, 318, 319, 320, 321, 322, 323,
 324, 325, 326, 327, 112, 101, 328, 249, 248, 113,
 247, 246,  29, 245, 244, 243, 105, 112,  29, 106,
 242, 107,  30, 241,  28,  56, 116,  55,  72, 240,
  26,  26,  26,  26,  26, 286, 286, 219,  65, 239,
 104,  27,  30, 238,  28,  53,  54, 237, 236,  86,
  87,  91,  90,  92,  98,  93,  94,  95,  96,  97,
  99, 235, 279, 280, 281, 282, 283, 284, 287,  36,
 234, 233, 380,  30, 232,  28, 231, 230, 381,  25,
 229, 382, 228, 227, 113,  62,   1,  29, 383, 384,
 385, 105,   0,   0, 106,  47, 107,   0, 386,   0,
 387,   0, 112,   0, 391, 392, 393, 394,   0, 151,
   0,   0, 200, 201,   0, 104,   0, 379, 202, 203,
  73,  74,  75,   0,   0,   0,   0, 413,   0,   0,
   0,   0, 414, 415, 416, 111, 108, 109,  27,  30,
  83,  28,   0, 417,   0,   0,  51,   0, 111, 108,
 109,   0,  30,   0,  28,   0,   0,   0,  59,  53,
  54,  27,  30, 103,  28,   0,  58,   0,   0,   0,
   0,   0,   0,  26,  26,  26, 103, 112,   0,   0,
   0, 110,   0, 149,   0, 150,   0, 141, 147,   0,
   0, 421, 139, 137, 110, 138,   0, 140,  79,  80,
   0,   0,  81,  82,   0, 388, 389, 390,   0,   0,
 144, 145, 146, 143, 150,   0, 141,   0,   0,   0,
   0, 139, 137,   0, 138,   0, 140, 125,   0,   0,
   0,   0,   0, 111, 108, 109,   0,  30,   0,  28,
  53,  54,  27,  30, 142,  28,   0,   0,   0,   0,
   0,   0,   0,   0, 149,   0, 150,   0, 141, 147,
   0, 103, 420, 139, 137,   0, 138,   0, 140,   0,
   0,   0,   0, 142, 148,   0,   0,   0,   0, 110,
   0, 144, 145, 146, 143,   0,   0,   0,   0, 149,
   0, 150,   0, 141, 147,   0,   0, 419, 139, 137,
   0, 138,   0, 140,   0,   0,   0,   0, 111, 108,
 109,  35,  30,   0,  28, 142, 144, 145, 146, 143,
  38,  39,  40,  41,  42,   0,   0,   0,   0, 149,
   0, 150,  60, 141, 147,   0, 103, 418, 139, 137,
   0, 138,   0, 140,   0, 148,   0,   0,   0,   0,
 142,   0,   0,   0, 110,   0, 144, 145, 146, 143,
   0,   0,   0,   0, 149,   0, 150,   0, 141, 147,
   0,   0,   0, 139, 137, 412, 138,   0, 140,   0,
 148,   0,   0,   0,   0,   0,   0,   0,   0,   0,
 142, 144, 145, 146, 143,   0,   0,   0,   0, 149,
   0, 150,   0, 141, 147,   0,   0, 411, 139, 137,
   0, 138,   0, 140,   0,   0,   0,   0,   0,   0,
 148,   0,   0,   0,   0, 142, 144, 145, 146, 143,
   0,   0,   0,   0, 149,   0, 150,   0, 141, 147,
   0,   0, 410, 139, 137,   0, 138,   0, 140,   0,
   0,   0,   0,   0,   0, 148,   0,   0,   0,   0,
 142, 144, 145, 146, 143,   0,   0,   0,   0, 149,
   0, 150,   0, 141, 147,   0,   0, 409, 139, 137,
   0, 138,   0, 140,   0,   0,   0,   0,   0,   0,
 148,   0,   0,   0,   0, 142, 144, 145, 146, 143,
   0,   0,   0,   0, 149,   0, 150,   0, 141, 147,
   0,   0, 405, 139, 137,   0, 138,   0, 140,   0,
   0,   0,   0,   0,   0, 148,   0,   0,   0,   0,
 142, 144, 145, 146, 143,   0,   0,   0,   0, 149,
   0, 150,   0, 141, 147,   0,   0, 404, 139, 137,
   0, 138,   0, 140,   0,   0,   0,   0,   0,   0,
 148,   0,   0,   0,   0, 142, 144, 145, 146, 143,
   0,   0,   0,   0, 149,   0, 150,   0, 141, 147,
   0,   0,   0, 139, 137, 403, 138,   0, 140,   0,
   0,   0,   0,   0,   0, 148,   0,   0,   0,   0,
 142, 144, 145, 146, 143,   0,   0,   0,   0, 149,
   0, 150,   0, 141, 147,   0,   0,   0, 139, 137,
 402, 138,   0, 140,   0,   0,   0,   0,   0,   0,
 148,   0,   0,   0,   0, 142, 144, 145, 146, 143,
   0,   0,   0,   0, 149,   0, 150,   0, 141, 147,
   0,   0,   0, 139, 137, 401, 138,   0, 140,   0,
   0,   0,   0,   0,   0, 148,   0,   0,   0,   0,
 142, 144, 145, 146, 143,   0,   0,   0,   0, 149,
   0, 150,   0, 141, 147,   0,   0, 400, 139, 137,
   0, 138,   0, 140,   0,   0,   0,   0,   0,   0,
 148,   0,   0,   0,   0, 142, 144, 145, 146, 143,
   0,   0,   0,   0, 149,   0, 150,   0, 141, 147,
   0,   0, 399, 139, 137,   0, 138,   0, 140,   0,
   0,   0,   0,   0,   0, 148,   0,   0,   0,   0,
 142, 144, 145, 146, 143,   0,   0,   0,   0, 149,
   0, 150,   0, 141, 147,   0,   0, 398, 139, 137,
   0, 138,   0, 140,   0,   0,   0,   0,   0,   0,
 148,   0,   0,   0,   0, 142, 144, 145, 146, 143,
   0,   0,   0,   0, 149,   0, 150,   0, 141, 147,
   0,   0,   0, 139, 137, 376, 138,   0, 140,   0,
   0,   0,   0,   0,   0, 148,   0,   0,   0,   0,
 142, 144, 145, 146, 143,   0,   0,   0,   0, 149,
   0, 150,   0, 141, 147,   0,   0,   0, 139, 137,
 375, 138,   0, 140,   0,   0,   0,   0,   0,   0,
 148,   0,   0,   0,   0, 142, 144, 145, 146, 143,
   0,   0,   0,   0, 149,   0, 150,   0, 141, 147,
   0,   0,   0, 139, 137, 374, 138,   0, 140,   0,
   0,   0,   0,   0,   0, 148,   0,   0,   0,   0,
 142, 144, 145, 146, 143,   0,   0,   0,   0, 149,
   0, 150,   0, 141, 147,   0,   0,   0, 139, 137,
 373, 138,   0, 140,   0,   0,   0,   0,   0,   0,
 148,   0,   0,   0,   0, 142, 144, 145, 146, 143,
   0,   0,   0,   0, 149,   0, 150,   0, 141, 147,
   0,   0,   0, 139, 137, 372, 138,   0, 140,   0,
   0,   0,   0,   0,   0, 148,   0,   0,   0,   0,
 142, 144, 145, 146, 143,   0,   0,   0,   0, 149,
   0, 150,   0, 141, 147,   0,   0,   0, 139, 137,
 371, 138,   0, 140,   0,   0,   0,   0,   0,   0,
 148,   0,   0,   0,   0, 142, 144, 145, 146, 143,
   0,   0,   0,   0, 149,   0, 150,   0, 141, 147,
   0,   0,   0, 139, 137, 370, 138,   0, 140,   0,
   0,   0,   0,   0,   0, 148,   0,   0,   0,   0,
 142, 144, 145, 146, 143,   0,   0,   0,   0, 149,
   0, 150,   0, 141, 147,   0,   0,   0, 139, 137,
 369, 138,   0, 140,   0,   0,   0,   0,   0,   0,
 148,   0,   0,   0,   0, 142, 144, 145, 146, 143,
   0,   0,   0,   0, 149,   0, 150,   0, 141, 147,
   0,   0, 368, 139, 137,   0, 138,   0, 140,   0,
   0,   0,   0,   0,   0, 148,   0,   0,   0,   0,
 142, 144, 145, 146, 143,   0,   0,   0,   0, 149,
   0, 150,   0, 141, 147,   0,   0,   0, 139, 137,
 367, 138,   0, 140,   0,   0,   0,   0,   0,   0,
 148,   0,   0,   0,   0, 142, 144, 145, 146, 143,
   0,   0,   0,   0, 149,   0, 150,   0, 141, 147,
   0,   0, 366, 139, 137,   0, 138,   0, 140,   0,
   0,   0,   0,   0,   0, 148,   0,   0,   0,   0,
 142, 144, 145, 146, 143,   0,   0,   0,   0, 149,
   0, 150,   0, 141, 147,   0,   0, 365, 139, 137,
   0, 138,   0, 140,   0,   0,   0,   0,   0,   0,
 148,   0,   0,   0,   0, 142, 144, 145, 146, 143,
   0,   0,   0,   0, 149,   0, 150,   0, 141, 147,
   0,   0, 364, 139, 137,   0, 138,   0, 140,   0,
   0,   0,   0,   0,   0, 148,   0,   0,   0,   0,
 142, 144, 145, 146, 143,   0,   0,   0,   0, 149,
   0, 150,   0, 141, 147,   0,   0, 363, 139, 137,
   0, 138,   0, 140,   0,   0,   0,   0,   0,   0,
 148,   0,   0,   0,   0, 142, 144, 145, 146, 143,
   0,   0,   0,   0, 149,   0, 150,   0, 141, 147,
   0,   0, 362, 139, 137,   0, 138,   0, 140,   0,
   0,   0,   0,   0,   0, 148,   0,   0,   0,   0,
 142, 144, 145, 146, 143,   0,   0,   0,   0, 149,
   0, 150,   0, 141, 147,   0,   0, 361, 139, 137,
   0, 138,   0, 140,   0,   0,   0,   0,   0,   0,
 148,   0,   0,   0,   0, 142, 144, 145, 146, 143,
   0,   0,   0,   0, 149,   0, 150,   0, 141, 147,
   0,   0, 360, 139, 137,   0, 138,   0, 140,   0,
   0,   0,   0,   0,   0, 148,   0,   0,   0,   0,
 142, 144, 145, 146, 143,   0,   0,   0,   0, 149,
   0, 150,   0, 141, 147,   0,   0,   0, 139, 137,
 359, 138,   0, 140,   0,   0,   0,   0,   0,   0,
 148,   0,   0,   0,   0, 142, 144, 145, 146, 143,
   0,   0,   0,   0, 149,   0, 150,   0, 141, 147,
   0,   0,   0, 139, 137, 358, 138,   0, 140,   0,
   0,   0,   0,   0,   0, 148,   0,   0,   0,   0,
 142, 144, 145, 146, 143,   0,   0,   0,   0, 149,
   0, 150,   0, 141, 147,   0,   0,   0, 139, 137,
 357, 138,   0, 140,   0,   0,   0,   0,   0,   0,
 148,   0,   0,   0,   0, 142, 144, 145, 146, 143,
   0,   0,   0,   0, 149,   0, 150,   0, 141, 147,
   0,   0, 356, 139, 137,   0, 138,   0, 140,   0,
   0,   0,   0,   0,   0, 148,   0,   0,   0,   0,
 142, 144, 145, 146, 143,   0,   0,   0,   0, 149,
   0, 150,   0, 141, 147,   0,   0, 355, 139, 137,
   0, 138,   0, 140,   0,   0,   0,   0,   0,   0,
 148,   0,   0,   0,   0, 142, 144, 145, 146, 143,
   0,   0,   0,   0, 149,   0, 150,   0, 141, 147,
   0,   0, 354, 139, 137,   0, 138,   0, 140,   0,
   0,   0,   0,   0,   0, 148,   0,   0,   0,   0,
 142, 144, 145, 146, 143,   0,   0,   0,   0, 149,
   0, 150,   0, 141, 147,   0,   0, 353, 139, 137,
   0, 138,   0, 140,   0,   0,   0,   0,   0,   0,
 148,   0,   0,   0,   0, 142, 144, 145, 146, 143,
   0,   0,   0,   0, 149,   0, 150,   0, 141, 147,
   0,   0, 352, 139, 137,   0, 138,   0, 140,   0,
   0,   0,   0,   0,   0, 148,   0,   0,   0,   0,
 142, 144, 145, 146, 143,   0,   0,   0,   0, 149,
   0, 150,   0, 141, 147,   0,   0, 351, 139, 137,
   0, 138,   0, 140,   0,   0,   0,   0,   0,   0,
 148,   0,   0,   0,   0, 142, 144, 145, 146, 143,
   0,   0,   0,   0, 149,   0, 150,   0, 141, 147,
   0,   0,   0, 139, 137, 350, 138,   0, 140,   0,
   0,   0,   0,   0,   0, 148,   0,   0,   0,   0,
 142, 144, 145, 146, 143,   0,   0,   0,   0, 149,
   0, 150,   0, 141, 147,   0,   0, 349, 139, 137,
   0, 138,   0, 140,   0,   0,   0,   0,   0,   0,
 148,   0,   0,   0,   0, 142, 144, 145, 146, 143,
   0,   0,   0,   0, 149,   0, 150,   0, 141, 147,
   0,   0, 348, 139, 137,   0, 138,   0, 140,   0,
   0,   0,   0,   0,   0, 148,   0,   0,   0,   0,
 142, 144, 145, 146, 143,   0,   0,   0,   0, 149,
   0, 150,   0, 141, 147,   0,   0,   0, 139, 137,
 347, 138,   0, 140,   0,   0,   0,   0,   0,   0,
 148,   0,   0,   0,   0, 142, 144, 145, 146, 143,
   0,   0,   0,   0, 149,   0, 150,   0, 141, 147,
   0,   0, 346, 139, 137,   0, 138,   0, 140,   0,
   0,   0,   0,   0,   0, 148,   0,   0,   0,   0,
 142, 144, 145, 146, 143,   0,   0,   0,   0, 149,
   0, 150,   0, 141, 147,   0,   0, 345, 139, 137,
   0, 138,   0, 140,   0,   0,   0,   0,   0,   0,
 148,   0,   0,   0,   0, 142, 144, 145, 146, 143,
   0,   0,   0,   0, 149,   0, 150,   0, 141, 147,
   0,   0, 344, 139, 137,   0, 138,   0, 140,   0,
   0,   0,   0,   0,   0, 148,   0,   0,   0,   0,
 142, 144, 145, 146, 143,   0,   0,   0,   0, 149,
   0, 150,   0, 141, 147,   0,   0, 343, 139, 137,
   0, 138,   0, 140,   0,   0,   0,   0,   0,   0,
 148,   0,   0,   0,   0, 142, 144, 145, 146, 143,
   0,   0,   0,   0, 149,   0, 150,   0, 141, 147,
   0,   0, 342, 139, 137,   0, 138,   0, 140,   0,
   0,   0,   0,   0,   0, 148,   0,   0,   0,   0,
 142, 144, 145, 146, 143,   0,   0,   0,   0, 149,
   0, 150,   0, 141, 147,   0,   0,   0, 139, 137,
 341, 138,   0, 140,   0,   0,   0,   0,   0,   0,
 148,   0,   0,   0,   0, 142, 144, 145, 146, 143,
   0,   0,   0,   0, 149,   0, 150,   0, 141, 147,
   0,   0, 340, 139, 137,   0, 138,   0, 140,   0,
   0,   0,   0,   0,   0, 148,   0,   0,   0,   0,
 142, 144, 145, 146, 143,   0,   0,   0,   0, 149,
   0, 150,   0, 141, 147,   0,   0, 339, 139, 137,
   0, 138,   0, 140,   0,   0,   0,   0,   0,   0,
 148,   0,   0,   0,   0, 142, 144, 145, 146, 143,
   0,   0,   0,   0, 149,   0, 150,   0, 141, 147,
   0,   0, 338, 139, 137,   0, 138,   0, 140,   0,
   0,   0,   0,   0,   0, 148,   0,   0,   0,   0,
 142, 144, 145, 146, 143,   0,   0,   0,   0, 149,
   0, 150,   0, 141, 147,   0,   0,   0, 139, 137,
 337, 138,   0, 140,   0,   0,   0,   0,   0,   0,
 148,   0,   0,   0,   0, 142, 144, 145, 146, 143,
   0,   0,   0,   0, 149,   0, 150,   0, 141, 147,
   0,   0,   0, 139, 137, 335, 138,   0, 140,   0,
   0,   0,   0,   0,   0, 148,   0,   0,   0,   0,
 142, 144, 145, 146, 143,   0,   0,   0,   0, 149,
   0, 150,   0, 141, 147,   0,   0,   0, 139, 137,
   0, 138,   0, 140,   0,   0,   0,   0,   0,   0,
 148,   0,   0,   0, 275, 142, 144, 145, 146, 143,
   0,   0,   0,   0, 149,   0, 150,   0, 141, 147,
   0,   0, 273, 139, 137,   0, 138,   0, 140,   0,
   0,   0,   0,   0,   0, 148,   0,   0,   0,   0,
 142, 144, 145, 146, 143,   0,   0,   0,   0, 149,
   0, 150,   0, 141, 147,   0,   0,   0, 139, 137,
   0, 138,   0, 140,   0,   0,   0,   0,   0,   0,
 148,   0,   0,   0,   0, 142, 144, 145, 146, 143,
 149,   0, 150,   0, 141, 147,   0,   0,   0, 139,
 137,   0, 138,   0, 140,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0, 148,   0, 144, 145, 146,
 142,   0,   0,   0,   0, 149,   0, 150,   0, 141,
 147,   0,   0,   0, 139, 137,   0, 138,   0, 140,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
 148, 142, 144, 145, 146, 149,   0, 150,   0, 141,
   0,   0,   0,   0, 139, 137,   0, 138,   0, 140,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0, 148, 144, 145, 146,   0, 142,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0, 142 };
short yypact[]={

-106,-1000,  51,  51,  51,  51,-190,  82,  82,  82,
  82,  82,  82,-195,-197,  51,  51,  51, 252, 171,
  82,  51,-1000,-1000,  68,-1000,  54,-1000,  50,-201,
-1000,  67,  66,  58,  40,-1000,-1000,-1000,-1000,-1000,
  51,  51,  51,  74,  73,-1000,-1000,  51,  54,-1000,
-1000,  57,  57,-1000,-1000,  57,  57,-1000,-1000,-1000,
  51,-1000, -15, 321,  31,-1000,-155,  38, 321, 321,
 321,-208,-157,-1000,-1000,-1000,-209,-159,-1000,-1000,
  57,-1000,-1000,-1000,  54,-1000,-1000,-1000,-282,-284,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,  56,  55,
2526,-1000,-1000, 321,-285, 321, 321, 321,-1000,-1000,
-1000,-1000, 321, 321,-1000,-1000,-1000,-161,2526,2526,
2526,-162,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-163,-193, 321, 321, 321,
 321, 321, 321, 321, 246, 321, 161, 321, 321,  46,
 321,-1000, 313, 312, 310, 307, 306, 304, 301, 300,
 291, 278, 277, 273, 269, 259, 253, 250, 245, 244,
 243, 241, 240, 238, 237, 192, 191, 190, 189, 188,
 187, 184, 181, 179, 178, 177, 175,-1000, 173, 170,
 169, 168, 165, 163, 162, 160, 159, 158, 156,2491,
-1000,-1000,-1000,-1000,-1000,-179,-1000,-1000,-1000,-1000,
-1000,  88,  88,  11,  11,  11,-1000,2456, 449, 321,
 449, 449, 321,2622,2592, 321,  88,  51,  51,  51,
  51,  51, 148, 148, 321, 321, 321, 321, 321, 321,
 321, 321, 321, 321, 321, 321, 321, 321, 321, 321,
 321, 321, 321, 321, 321, 321, 321, 321, 321, 321,
 321, 321, 321, 321, 321, 321, 321, 321, 321, 321,
 321, 321, 321,-1000,-1000, 321, 449, 449, 449, 154,
 151, 149, 146, 144, 138,2421,  54, 116,2386,2351,
2316,2281,2246,2211,2176,2141,2106,2071,2036,2001,
1966,1931,1896,1861,1826,1791,1756,1721,1686,1651,
1616,1581,1546,1511,1476,1441,1406,1371,1336,1301,
1266,1231,1196,1161,1126,1091,1056,1021,2557,-1000,
-1000,-1000,-1000,-1000,-1000, 321,-1000, 321,-1000,-1000,
-1000, 321,-1000,-1000,-1000,-1000,-1000, 321,-1000,-1000,
 321,-1000,-1000,-1000,-1000,-1000,-1000, 321, 321, 321,
-1000,-1000,-1000,-1000,-1000,-1000,-1000, 321,-1000, 321,
  51,  51,  51, 321, 321, 321, 321, 145,2526, 139,
 986, 951, 916, 881, 846, 811, 776, 741, 115,  95,
  93, 706, 671, 636, 601,-1000, 321,-1000,-1000,-1000,
-1000, 321, 321, 321,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000, 321,2526, 566, 526, 491, 420,-1000,-1000,
-1000,-1000 };
short yypgo[]={

   0,  72, 416, 349, 104, 581,   0, 275,  49, 356,
 355, 211 };
short yyr1[]={

   0,   9,   9,   9,   9,   9,   9,   9,   9,   9,
   9,   9,   9,   9,   9,   9,   9,   9,   9,   9,
   9,   9,   9,   9,   9,   9,   9,   9,   9,   9,
   9,   9,   9,   9,   9,   9,   9,   9,   7,   7,
   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,
   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,
   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,
   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,
   7,   7,   7,   7,   7,   7,   7,   7,   7,   7,
   7,   7,   7,   7,   7,   7,   7,   7,   6,   6,
   6,   6,   6,   6,   6,   6,   6,   6,   6,   6,
   6,   6,   6,   6,   6,   8,   8,   3,   3,   1,
   1,   1,   1,   1,   4,   4,   2,   2,   2,   2,
   5,   5,  10,  10,  11,  11,  11,  11,  11,  11,
  11,  11,  11,  11,  11,  11,  11,  11,  11,  11,
  11,  11,  11,  11,  11,  11,  11,  11 };
short yyr2[]={

   0,   4,   4,   4,   4,   6,   4,   2,   2,   2,
   3,   2,   3,   2,   3,   2,   4,   4,   2,   2,
   3,   1,   2,   1,   2,   3,   4,   2,   2,   2,
   1,   2,   3,   3,   2,   2,   0,   1,   1,   2,
   5,   5,   5,   5,   5,   5,   7,   5,   7,   5,
   5,   5,   7,   5,   5,   5,   5,   5,   7,   5,
   5,   7,   5,   5,   5,   5,   5,   5,   9,   9,
   9,   5,   5,   5,   5,   5,   5,   2,   5,   7,
   5,   7,   7,   7,   7,   7,   7,   7,   9,   3,
   2,   2,   1,   1,   1,   1,   2,   2,   3,   3,
   3,   3,   3,   3,   1,   5,   3,   3,   3,   3,
   3,   4,   4,   4,   3,   1,   3,   3,   1,   2,
   3,   3,   4,   1,   1,   1,   1,   1,   2,   2,
   1,   1,   0,   2,   1,   1,   2,   2,   2,   2,
   1,   1,   1,   1,   2,   1,   2,   1,   2,   1,
   2,   1,   2,   3,   3,   3,   3,   3 };
short yychk[]={

-1000,  -9, 271, 265, 266, 267, 264, 268, 270, 282,
 269, 272, 273, 275, 283, 274, 276, 281, 277, 278,
 279, 280, 284, 256,  -4,  -3,  -1, 260, 263,  36,
 261,  -4,  -4,  -4, 263,  -5, 257,  -1,  -5,  -5,
  -5,  -5,  -5, 263, 258, 263, 258,  -3,  -1,  -4,
  -4,  -2,  -4, 258, 259,  45,  43,  -4,  -2, 257,
  -5,  -4, -10,  61,  58, 258,  36, 263,  61,  61,
  61,  58, 258,  -3,  -3,  -3,  58,  58,  -4,  -2,
  -2,  -2,  -2,  -3,  -1, -11, 334, 335, 126,  33,
 337, 336, 338, 340, 341, 342, 343, 344, 339, 345,
  -6,  -7,  -1, 285,  64,  40,  43,  45, 258, 259,
 303, 257, 126,  33,  -1, 258, 258,  36,  -6,  -6,
  -6, 263, 258, 263, 258,  -2, 334, 335, 334, 335,
 340, 341, 342, 343, 344,  61,  61,  43,  45,  42,
  47,  37,  94,  63,  60,  61,  62,  38, 124,  33,
  35,  -7, 286, 287, 288, 289, 290, 310, 311, 291,
 292, 293, 294, 295, 296, 297, 298, 299, 300, 301,
 302, 304, 305, 306, 307, 308, 309, 312, 313, 314,
 315, 316, 317, 318, 319, 320, 321, 322, 326, 327,
 323, 324, 332, 331, 333, 328, 329, 330, 325,  -6,
  -7,  -7,  -7,  -7, 258, 258, 258, 258, 346, 347,
 348,  -6,  -6,  -6,  -6,  -6,  -6,  -6,  -6,  61,
  -6,  -6,  61,  -6,  -6,  61,  -6,  40,  40,  40,
  40,  40,  40,  40,  40,  40,  40,  40,  40,  40,
  40,  40,  40,  40,  40,  40,  40,  40,  40,  40,
  40,  40,  40,  40,  40,  40,  40,  40,  40,  40,
  40,  40,  40,  40,  40,  40,  40,  40,  40,  40,
  40,  40,  40,  41, 258,  58,  -6,  -6,  -6,  -4,
  -4,  -4,  -4,  -4,  -4,  -6,  -1,  -4,  -6,  -6,
  -6,  -6,  -6,  -6,  -6,  -6,  -6,  -6,  -6,  -6,
  -6,  -6,  -6,  -6,  -6,  -6,  -6,  -6,  -6,  -6,
  -6,  -6,  -6,  -6,  -6,  -6,  -6,  -6,  -6,  -6,
  -6,  -6,  -6,  -6,  -6,  -6,  -6,  -6,  -6,  41,
  41,  41,  41,  41,  41,  44,  41,  44,  41,  41,
  41,  44,  41,  41,  41,  41,  41,  44,  41,  41,
  44,  41,  41,  41,  41,  41,  41,  44,  44,  44,
  41,  41,  41,  41,  41,  41,  41,  44,  41,  44,
  44,  44,  44,  44,  44,  44,  44,  -8,  -6,  -8,
  -6,  -6,  -6,  -6,  -6,  -6,  -6,  -6,  -4,  -4,
  -4,  -6,  -6,  -6,  -6,  41,  44,  41,  41,  41,
  41,  44,  44,  44,  41,  41,  41,  41,  41,  41,
  41,  41,  44,  -6,  -6,  -6,  -6,  -6,  41,  41,
  41,  41 };
short yydef[]={

  36,  -2,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,  21,  23,   0,  30,
   0,   0, 132,  37,   0, 124, 125, 118,   0,   0,
 123,   0,   0,   0,   0,   7, 130, 131,   8,   9,
  11,  13,  15,   0,   0,  18,  19,   0,   0,  22,
  24,   0,   0, 126, 127,   0,   0,  27,  28,  29,
  31,  34,  35,   0,   0, 119,   0,   0,   0,   0,
   0,   0,   0,  10,  12,  14,   0,   0,  20,  25,
   0, 128, 129,  32,  33, 133, 134, 135,   0,   0,
 140, 141, 142, 143, 145, 147, 149, 151,   0,   0,
   1, 104,  38,   0,   0,   0,   0,   0,  92,  93,
  94,  95,   0,   0, 117, 121, 120,   0,   2,   3,
   4,   0,   6,  16,  17,  26, 136, 137, 138, 139,
 144, 146, 148, 150, 152,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,  39,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,  77,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
  90,  91,  96,  97, 122,   0, 153, 154, 155, 156,
 157,  98,  99, 100, 101, 102, 103,   0,  -2,   0,
  -2,  -2,   0, 109, 110,   0, 114,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,  89,   5,   0,  -2,  -2,  -2,   0,
   0,   0,   0,   0,   0,   0,  -2,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0, 105,  40,
  41,  42,  43,  44,  45,   0,  47,   0,  49,  50,
  51,   0,  53,  54,  55,  56,  57,   0,  59,  60,
   0,  62,  63,  64,  65,  66,  67,   0,   0,   0,
  71,  72,  73,  74,  75,  76,  78,   0,  80,   0,
   0,   0,   0,   0,   0,   0,   0,   0, 115,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,  46,   0,  48,  52,  58,
  61,   0,   0,   0,  79,  81,  82,  83,  84,  85,
  86,  87,   0, 116,   0,   0,   0,   0,  68,  69,
  70,  88 };
#ifndef lint
static	char yaccpar_sccsid[] = "@(#)yaccpar 1.6 88/02/08 SMI"; /* from UCB 4.1 83/02/11 */
#endif

#
# define YYFLAG -1000
# define YYERROR goto yyerrlab
# define YYACCEPT return(0)
# define YYABORT return(1)
# define YYMAXDEPTH 600

/*	parser for yacc output	*/

#ifdef YYDEBUG
int yydebug = 0; /* 1 for debugging */
#endif
YYSTYPE yyv[YYMAXDEPTH]; /* where the values are stored */
int yychar = -1; /* current input token number */
int yynerrs = 0;  /* number of errors */
short yyerrflag = 0;  /* error recovery flag */

yyparse() {

	short yys[YYMAXDEPTH];
	short yyj, yym;
	register YYSTYPE *yypvt;
	register short yystate, *yyps, yyn;
	register YYSTYPE *yypv;
	register short *yyxi;

	yystate = 0;
	yychar = -1;
	yynerrs = 0;
	yyerrflag = 0;
	yyps= &yys[-1];
	yypv= &yyv[-1];

 yystack:    /* put a state and value onto the stack */

#ifdef YYDEBUG
	if( yydebug  ) printf( "state %d, char 0%o\n", yystate, yychar );
#endif
		if( ++yyps>= &yys[YYMAXDEPTH] ) { yyerror( "yacc stack overflow" ); return(1); }
		*yyps = yystate;
		++yypv;
		*yypv = yyval;

 yynewstate:

	yyn = yypact[yystate];

	if( yyn<= YYFLAG ) goto yydefault; /* simple state */

	if( yychar<0 ) if( (yychar=yylex())<0 ) yychar=0;
	if( (yyn += yychar)<0 || yyn >= YYLAST ) goto yydefault;

	if( yychk[ yyn=yyact[ yyn ] ] == yychar ){ /* valid shift */
		yychar = -1;
		yyval = yylval;
		yystate = yyn;
		if( yyerrflag > 0 ) --yyerrflag;
		goto yystack;
		}

 yydefault:
	/* default state action */

	if( (yyn=yydef[yystate]) == -2 ) {
		if( yychar<0 ) if( (yychar=yylex())<0 ) yychar = 0;
		/* look through exception table */

		for( yyxi=yyexca; (*yyxi!= (-1)) || (yyxi[1]!=yystate) ; yyxi += 2 ) ; /* VOID */

		while( *(yyxi+=2) >= 0 ){
			if( *yyxi == yychar ) break;
			}
		if( (yyn = yyxi[1]) < 0 ) return(0);   /* accept */
		}

	if( yyn == 0 ){ /* error */
		/* error ... attempt to resume parsing */

		switch( yyerrflag ){

		case 0:   /* brand new error */

			yyerror( "syntax error" );
		yyerrlab:
			++yynerrs;

		case 1:
		case 2: /* incompletely recovered error ... try again */

			yyerrflag = 3;

			/* find a state where "error" is a legal shift action */

			while ( yyps >= yys ) {
			   yyn = yypact[*yyps] + YYERRCODE;
			   if( yyn>= 0 && yyn < YYLAST && yychk[yyact[yyn]] == YYERRCODE ){
			      yystate = yyact[yyn];  /* simulate a shift of "error" */
			      goto yystack;
			      }
			   yyn = yypact[*yyps];

			   /* the current yyps has no shift onn "error", pop stack */

#ifdef YYDEBUG
			   if( yydebug ) printf( "error recovery pops state %d, uncovers %d\n", *yyps, yyps[-1] );
#endif
			   --yyps;
			   --yypv;
			   }

			/* there is no state on the stack with an error shift ... abort */

	yyabort:
			return(1);


		case 3:  /* no shift yet; clobber input char */

#ifdef YYDEBUG
			if( yydebug ) printf( "error recovery discards char %d\n", yychar );
#endif

			if( yychar == 0 ) goto yyabort; /* don't discard EOF, quit */
			yychar = -1;
			goto yynewstate;   /* try again in the same state */

			}

		}

	/* reduction by production yyn */

#ifdef YYDEBUG
		if( yydebug ) printf("reduce %d\n",yyn);
#endif
		yyps -= yyr2[yyn];
		yypvt = yypv;
		yypv -= yyr2[yyn];
		yyval = yypv[1];
		yym=yyn;
			/* consult goto table to find next state */
		yyn = yyr1[yyn];
		yyj = yypgo[yyn] + *yyps + 1;
		if( yyj>=YYLAST || yychk[ yystate = yyact[yyj] ] != -yyn ) yystate = yyact[yypgo[yyn]];
		switch(yym){
			
case 1:
# line 198 "gram.y"
{ let(yypvt[-2].rval.left.vp, yypvt[-0].enode); } break;
case 2:
# line 200 "gram.y"
{ slet(yypvt[-2].rval.left.vp, yypvt[-0].enode, 0); } break;
case 3:
# line 202 "gram.y"
{ slet(yypvt[-2].rval.left.vp, yypvt[-0].enode, -1); } break;
case 4:
# line 204 "gram.y"
{ slet(yypvt[-2].rval.left.vp, yypvt[-0].enode, 1); } break;
case 5:
# line 206 "gram.y"
{ doformat(yypvt[-4].ival,yypvt[-2].ival,yypvt[-1].ival,yypvt[-0].ival); } break;
case 6:
# line 208 "gram.y"
{ doformat(yypvt[-2].ival,yypvt[-2].ival,yypvt[-1].ival,yypvt[-0].ival); } break;
case 7:
# line 209 "gram.y"
{  /* This tmp hack is because readfile
				    * recurses back through yyparse. */
				  char *tmp;
				  tmp = yypvt[-0].sval;
				  readfile (tmp, 1);
				  xfree(tmp);
				} break;
case 8:
# line 216 "gram.y"
{
				  char *tmp;
				  tmp = yypvt[-0].sval;
				  readfile (tmp, 0);
				  xfree(tmp);
				} break;
case 9:
# line 223 "gram.y"
{ if (mdir) xfree(mdir); mdir = yypvt[-0].sval; } break;
case 10:
# line 225 "gram.y"
{ (void) writefile(yypvt[-1].sval, (yypvt[-0].rval.left.vp)->row, 
			 	(yypvt[-0].rval.left.vp)->col, (yypvt[-0].rval.right.vp)->row,
			 	(yypvt[-0].rval.right.vp)->col);
			 	xfree(yypvt[-1].sval); } break;
case 11:
# line 230 "gram.y"
{ (void) writefile (yypvt[-0].sval, 0, 0, maxrow, maxcol);
			 	xfree(yypvt[-0].sval); } break;
case 12:
# line 232 "gram.y"
{ (void) printfile(yypvt[-1].sval, (yypvt[-0].rval.left.vp)->row, 
			 (yypvt[-0].rval.left.vp)->col, (yypvt[-0].rval.right.vp)->row,
			 (yypvt[-0].rval.right.vp)->col);
			 xfree(yypvt[-1].sval); } break;
case 13:
# line 236 "gram.y"
{ (void) printfile (yypvt[-0].sval, 0, 0, maxrow, maxcol);
			 xfree(yypvt[-0].sval); } break;
case 14:
# line 238 "gram.y"
{ (void) tblprintfile(yypvt[-1].sval, (yypvt[-0].rval.left.vp)->row, 
			 (yypvt[-0].rval.left.vp)->col, (yypvt[-0].rval.right.vp)->row,
			 (yypvt[-0].rval.right.vp)->col);
			 xfree(yypvt[-1].sval); } break;
case 15:
# line 242 "gram.y"
{ (void)tblprintfile (yypvt[-0].sval, 0, 0, maxrow, maxcol);
			 xfree(yypvt[-0].sval); } break;
case 16:
# line 245 "gram.y"
{ showcol( yypvt[-2].ival, yypvt[-0].ival); } break;
case 17:
# line 247 "gram.y"
{ showrow( yypvt[-2].ival, yypvt[-0].ival); } break;
case 18:
# line 249 "gram.y"
{ hide_col( yypvt[-0].ival ); } break;
case 19:
# line 251 "gram.y"
{ hide_row( yypvt[-0].ival ); } break;
case 20:
# line 253 "gram.y"
{ copy(yypvt[-1].rval.left.vp,yypvt[-1].rval.right.vp,
					yypvt[-0].rval.left.vp,yypvt[-0].rval.right.vp); } break;
case 21:
# line 256 "gram.y"
{ eraser(lookat(showsr, showsc),
				        lookat(currow, curcol)); } break;
case 22:
# line 259 "gram.y"
{ eraser(yypvt[-0].rval.left.vp, yypvt[-0].rval.right.vp); } break;
case 23:
# line 260 "gram.y"
{ valueize_area(showsr, showsc, currow, curcol);
				 modflg++; } break;
case 24:
# line 262 "gram.y"
{ valueize_area((yypvt[-0].rval.left.vp)->row,
				(yypvt[-0].rval.left.vp)->col,
				(yypvt[-0].rval.right.vp)->row,
				(yypvt[-0].rval.right.vp)->col); modflg++; } break;
case 25:
# line 266 "gram.y"
{ fill(lookat(showsr, showsc),
				      lookat(currow, curcol), yypvt[-1].fval, yypvt[-0].fval); } break;
case 26:
# line 269 "gram.y"
{ fill(yypvt[-2].rval.left.vp, yypvt[-2].rval.right.vp, yypvt[-1].fval, yypvt[-0].fval); } break;
case 27:
# line 270 "gram.y"
{moveto(yypvt[-0].rval.left.vp->row, yypvt[-0].rval.left.vp->col);} break;
case 28:
# line 271 "gram.y"
{num_search(yypvt[-0].fval);} break;
case 29:
# line 272 "gram.y"
{str_search(yypvt[-0].sval);} break;
case 30:
# line 273 "gram.y"
{go_last();} break;
case 31:
# line 274 "gram.y"
{ struct ent_ptr arg1, arg2;
					arg1.vp = lookat(showsr, showsc);
					arg1.vf = 0;
					arg2.vp = lookat(currow, curcol);
					arg2.vf = 0;
					add_range(yypvt[-0].sval, arg1, arg2, 1); } break;
case 32:
# line 281 "gram.y"
{ add_range(yypvt[-1].sval, yypvt[-0].rval.left, yypvt[-0].rval.right, 1); } break;
case 33:
# line 282 "gram.y"
{ add_range(yypvt[-1].sval, yypvt[-0].ent, yypvt[-0].ent, 0); } break;
case 34:
# line 283 "gram.y"
{ del_range(yypvt[-0].rval.left.vp, yypvt[-0].rval.right.vp); } break;
case 38:
# line 288 "gram.y"
{ yyval.enode = new_var('v', yypvt[-0].ent); } break;
case 39:
# line 289 "gram.y"
{ yyval.enode = new ('f', ENULL, yypvt[-0].enode); } break;
case 40:
# line 291 "gram.y"
{ yyval.enode = new_range(REDUCE | '+', yypvt[-1].rval); } break;
case 41:
# line 293 "gram.y"
{ yyval.enode = new_range (REDUCE | '*', yypvt[-1].rval); } break;
case 42:
# line 295 "gram.y"
{ yyval.enode = new_range (REDUCE | 'a', yypvt[-1].rval); } break;
case 43:
# line 297 "gram.y"
{ yyval.enode = new_range (REDUCE | 's', yypvt[-1].rval); } break;
case 44:
# line 299 "gram.y"
{ yyval.enode = new_range (REDUCE | 'c', yypvt[-1].rval); } break;
case 45:
# line 301 "gram.y"
{ yyval.enode = new_range (REDUCE | MAX, yypvt[-1].rval); } break;
case 46:
# line 303 "gram.y"
{ yyval.enode = new(LMAX, yypvt[-1].enode, yypvt[-3].enode); } break;
case 47:
# line 305 "gram.y"
{ yyval.enode = new_range (REDUCE | MIN, yypvt[-1].rval); } break;
case 48:
# line 307 "gram.y"
{ yyval.enode = new(LMIN, yypvt[-1].enode, yypvt[-3].enode); } break;
case 49:
# line 309 "gram.y"
{ yyval.enode = new(ACOS, ENULL, yypvt[-1].enode); } break;
case 50:
# line 310 "gram.y"
{ yyval.enode = new(ASIN, ENULL, yypvt[-1].enode); } break;
case 51:
# line 311 "gram.y"
{ yyval.enode = new(ATAN, ENULL, yypvt[-1].enode); } break;
case 52:
# line 312 "gram.y"
{ yyval.enode = new(ATAN2, yypvt[-3].enode, yypvt[-1].enode); } break;
case 53:
# line 313 "gram.y"
{ yyval.enode = new(CEIL, ENULL, yypvt[-1].enode); } break;
case 54:
# line 314 "gram.y"
{ yyval.enode = new(COS, ENULL, yypvt[-1].enode); } break;
case 55:
# line 315 "gram.y"
{ yyval.enode = new(EXP, ENULL, yypvt[-1].enode); } break;
case 56:
# line 316 "gram.y"
{ yyval.enode = new(FABS, ENULL, yypvt[-1].enode); } break;
case 57:
# line 317 "gram.y"
{ yyval.enode = new(FLOOR, ENULL, yypvt[-1].enode); } break;
case 58:
# line 318 "gram.y"
{ yyval.enode = new(HYPOT, yypvt[-3].enode, yypvt[-1].enode); } break;
case 59:
# line 319 "gram.y"
{ yyval.enode = new(LOG, ENULL, yypvt[-1].enode); } break;
case 60:
# line 320 "gram.y"
{ yyval.enode = new(LOG10, ENULL, yypvt[-1].enode); } break;
case 61:
# line 321 "gram.y"
{ yyval.enode = new(POW, yypvt[-3].enode, yypvt[-1].enode); } break;
case 62:
# line 322 "gram.y"
{ yyval.enode = new(SIN, ENULL, yypvt[-1].enode); } break;
case 63:
# line 323 "gram.y"
{ yyval.enode = new(SQRT, ENULL, yypvt[-1].enode); } break;
case 64:
# line 324 "gram.y"
{ yyval.enode = new(TAN, ENULL, yypvt[-1].enode); } break;
case 65:
# line 325 "gram.y"
{ yyval.enode = new(DTR, ENULL, yypvt[-1].enode); } break;
case 66:
# line 326 "gram.y"
{ yyval.enode = new(RTD, ENULL, yypvt[-1].enode); } break;
case 67:
# line 327 "gram.y"
{ yyval.enode = new(RND, ENULL, yypvt[-1].enode); } break;
case 68:
# line 329 "gram.y"
{ yyval.enode = new(PV,  yypvt[-5].enode,new(':',yypvt[-3].enode,yypvt[-1].enode)); } break;
case 69:
# line 330 "gram.y"
{ yyval.enode = new(FV,  yypvt[-5].enode,new(':',yypvt[-3].enode,yypvt[-1].enode)); } break;
case 70:
# line 331 "gram.y"
{ yyval.enode = new(PMT, yypvt[-5].enode,new(':',yypvt[-3].enode,yypvt[-1].enode)); } break;
case 71:
# line 333 "gram.y"
{ yyval.enode = new(HOUR,ENULL, yypvt[-1].enode); } break;
case 72:
# line 334 "gram.y"
{ yyval.enode = new(MINUTE,ENULL, yypvt[-1].enode); } break;
case 73:
# line 335 "gram.y"
{ yyval.enode = new(SECOND,ENULL, yypvt[-1].enode); } break;
case 74:
# line 336 "gram.y"
{ yyval.enode = new(MONTH,ENULL,yypvt[-1].enode); } break;
case 75:
# line 337 "gram.y"
{ yyval.enode = new(DAY, ENULL, yypvt[-1].enode); } break;
case 76:
# line 338 "gram.y"
{ yyval.enode = new(YEAR, ENULL, yypvt[-1].enode); } break;
case 77:
# line 339 "gram.y"
{ yyval.enode = new(NOW, ENULL, ENULL);} break;
case 78:
# line 340 "gram.y"
{ yyval.enode = new(STON, ENULL, yypvt[-1].enode); } break;
case 79:
# line 341 "gram.y"
{ yyval.enode = new (EQS, yypvt[-3].enode, yypvt[-1].enode); } break;
case 80:
# line 342 "gram.y"
{ yyval.enode = new(DATE, ENULL, yypvt[-1].enode); } break;
case 81:
# line 343 "gram.y"
{ yyval.enode = new(FMT, yypvt[-3].enode, yypvt[-1].enode); } break;
case 82:
# line 345 "gram.y"
{ yyval.enode = new(INDEX, yypvt[-3].enode, new_range(REDUCE | INDEX, yypvt[-1].rval)); } break;
case 83:
# line 347 "gram.y"
{ yyval.enode = new(LOOKUP, yypvt[-3].enode, new_range(REDUCE | LOOKUP, yypvt[-1].rval)); } break;
case 84:
# line 349 "gram.y"
{ yyval.enode = new(STINDEX, yypvt[-3].enode, new_range(REDUCE | STINDEX, yypvt[-1].rval)); } break;
case 85:
# line 350 "gram.y"
{ yyval.enode = new(EXT, yypvt[-3].enode, yypvt[-1].enode); } break;
case 86:
# line 351 "gram.y"
{ yyval.enode = new(NVAL, yypvt[-3].enode, yypvt[-1].enode); } break;
case 87:
# line 352 "gram.y"
{ yyval.enode = new(SVAL, yypvt[-3].enode, yypvt[-1].enode); } break;
case 88:
# line 354 "gram.y"
{ yyval.enode = new(SUBSTR, yypvt[-5].enode, new(',', yypvt[-3].enode, yypvt[-1].enode)); } break;
case 89:
# line 355 "gram.y"
{ yyval.enode = yypvt[-1].enode; } break;
case 90:
# line 356 "gram.y"
{ yyval.enode = yypvt[-0].enode; } break;
case 91:
# line 357 "gram.y"
{ yyval.enode = new ('m', ENULL, yypvt[-0].enode); } break;
case 92:
# line 358 "gram.y"
{ yyval.enode = new_const('k', (double) yypvt[-0].ival); } break;
case 93:
# line 359 "gram.y"
{ yyval.enode = new_const('k', yypvt[-0].fval); } break;
case 94:
# line 360 "gram.y"
{ yyval.enode = new_const('k', (double)3.14159265358979323846); } break;
case 95:
# line 361 "gram.y"
{ yyval.enode = new_str(yypvt[-0].sval); } break;
case 96:
# line 362 "gram.y"
{ yyval.enode = new ('~', ENULL, yypvt[-0].enode); } break;
case 97:
# line 363 "gram.y"
{ yyval.enode = new ('~', ENULL, yypvt[-0].enode); } break;
case 98:
# line 366 "gram.y"
{ yyval.enode = new ('+', yypvt[-2].enode, yypvt[-0].enode); } break;
case 99:
# line 367 "gram.y"
{ yyval.enode = new ('-', yypvt[-2].enode, yypvt[-0].enode); } break;
case 100:
# line 368 "gram.y"
{ yyval.enode = new ('*', yypvt[-2].enode, yypvt[-0].enode); } break;
case 101:
# line 369 "gram.y"
{ yyval.enode = new ('/', yypvt[-2].enode, yypvt[-0].enode); } break;
case 102:
# line 370 "gram.y"
{ yyval.enode = new ('%', yypvt[-2].enode, yypvt[-0].enode); } break;
case 103:
# line 371 "gram.y"
{ yyval.enode = new ('^', yypvt[-2].enode, yypvt[-0].enode); } break;
case 105:
# line 373 "gram.y"
{ yyval.enode = new ('?', yypvt[-4].enode, new(':', yypvt[-2].enode, yypvt[-0].enode)); } break;
case 106:
# line 374 "gram.y"
{ yyval.enode = new ('<', yypvt[-2].enode, yypvt[-0].enode); } break;
case 107:
# line 375 "gram.y"
{ yyval.enode = new ('=', yypvt[-2].enode, yypvt[-0].enode); } break;
case 108:
# line 376 "gram.y"
{ yyval.enode = new ('>', yypvt[-2].enode, yypvt[-0].enode); } break;
case 109:
# line 377 "gram.y"
{ yyval.enode = new ('&', yypvt[-2].enode, yypvt[-0].enode); } break;
case 110:
# line 378 "gram.y"
{ yyval.enode = new ('|', yypvt[-2].enode, yypvt[-0].enode); } break;
case 111:
# line 379 "gram.y"
{ yyval.enode = new ('~', ENULL, new ('>', yypvt[-3].enode, yypvt[-0].enode)); } break;
case 112:
# line 380 "gram.y"
{ yyval.enode = new ('~', ENULL, new ('=', yypvt[-3].enode, yypvt[-0].enode)); } break;
case 113:
# line 381 "gram.y"
{ yyval.enode = new ('~', ENULL, new ('<', yypvt[-3].enode, yypvt[-0].enode)); } break;
case 114:
# line 382 "gram.y"
{ yyval.enode = new ('#', yypvt[-2].enode, yypvt[-0].enode); } break;
case 115:
# line 385 "gram.y"
{ yyval.enode = new(ELIST, ENULL, yypvt[-0].enode); } break;
case 116:
# line 386 "gram.y"
{ yyval.enode = new(ELIST, yypvt[-2].enode, yypvt[-0].enode); } break;
case 117:
# line 389 "gram.y"
{ yyval.rval.left = yypvt[-2].ent; yyval.rval.right = yypvt[-0].ent; } break;
case 118:
# line 390 "gram.y"
{ yyval.rval = yypvt[-0].rval; } break;
case 119:
# line 393 "gram.y"
{ yyval.ent.vp = lookat(yypvt[-0].ival , yypvt[-1].ival); yyval.ent.vf = 0;} break;
case 120:
# line 394 "gram.y"
{ yyval.ent.vp = lookat(yypvt[-0].ival , yypvt[-1].ival);
					yyval.ent.vf = FIX_COL;} break;
case 121:
# line 396 "gram.y"
{ yyval.ent.vp = lookat(yypvt[-0].ival , yypvt[-2].ival);
					yyval.ent.vf = FIX_ROW;} break;
case 122:
# line 398 "gram.y"
{ yyval.ent.vp = lookat(yypvt[-0].ival , yypvt[-2].ival);
					yyval.ent.vf = FIX_ROW | FIX_COL;} break;
case 123:
# line 400 "gram.y"
{ yyval.ent = yypvt[-0].rval.left; } break;
case 124:
# line 403 "gram.y"
{ yyval.rval = yypvt[-0].rval; } break;
case 125:
# line 404 "gram.y"
{ yyval.rval.left = yypvt[-0].ent; yyval.rval.right = yypvt[-0].ent; } break;
case 126:
# line 407 "gram.y"
{ yyval.fval = (double) yypvt[-0].ival; } break;
case 127:
# line 408 "gram.y"
{ yyval.fval = yypvt[-0].fval; } break;
case 128:
# line 409 "gram.y"
{ yyval.fval = -yypvt[-0].fval; } break;
case 129:
# line 410 "gram.y"
{ yyval.fval = yypvt[-0].fval; } break;
case 130:
# line 413 "gram.y"
{ yyval.sval = yypvt[-0].sval; } break;
case 131:
# line 414 "gram.y"
{
				    char *s, *s1;
				    s1 = yypvt[-0].ent.vp->label;
				    if (!s1)
					s1 = "NULL_STRING";
				    s = xmalloc((unsigned)strlen(s1)+1);
				    (void) strcpy(s, s1);
				    yyval.sval = s;
				} break;
case 134:
# line 429 "gram.y"
{ setauto(1); } break;
case 135:
# line 430 "gram.y"
{ setauto(1); } break;
case 136:
# line 431 "gram.y"
{ setauto(0); } break;
case 137:
# line 432 "gram.y"
{ setauto(0); } break;
case 138:
# line 433 "gram.y"
{ setauto(0); } break;
case 139:
# line 434 "gram.y"
{ setauto(0); } break;
case 140:
# line 435 "gram.y"
{ setorder(BYCOLS); } break;
case 141:
# line 436 "gram.y"
{ setorder(BYROWS); } break;
case 142:
# line 437 "gram.y"
{ setorder(BYGRAPH); } break;
case 143:
# line 438 "gram.y"
{ numeric = 1; } break;
case 144:
# line 439 "gram.y"
{ numeric = 0; } break;
case 145:
# line 440 "gram.y"
{ prescale = 0.01; } break;
case 146:
# line 441 "gram.y"
{ prescale = 1.0; } break;
case 147:
# line 442 "gram.y"
{ extfunc = 1; } break;
case 148:
# line 443 "gram.y"
{ extfunc = 0; } break;
case 149:
# line 444 "gram.y"
{ showcell = 1; } break;
case 150:
# line 445 "gram.y"
{ showcell = 0; } break;
case 151:
# line 446 "gram.y"
{ showtop = 1; } break;
case 152:
# line 447 "gram.y"
{ showtop = 0; } break;
case 153:
# line 448 "gram.y"
{ setiterations(yypvt[-0].ival); } break;
case 154:
# line 449 "gram.y"
{ tbl_style = yypvt[-0].ival; } break;
case 155:
# line 450 "gram.y"
{ tbl_style = TBL; } break;
case 156:
# line 451 "gram.y"
{ tbl_style = LATEX; } break;
case 157:
# line 452 "gram.y"
{ tbl_style = TEX; } break;
		}
		goto yystack;  /* stack new state and value */

	}
