char *malloc();

extern int rows, columns, vnum;
extern int row_col, row_1_col, row_2_col;
extern int *brett, *weiss, *schwarz, **freip, **doublesp;
extern int frei[], reihenfolge[], doubles[];
extern int (*pu)[4];
extern int *_p_h_, **pp;
extern struct oldv {
  int *pos, value;
} *stack, *sp, **zugstack;
