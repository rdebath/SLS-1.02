#include <stdio.h>

#include "xvier.h"
#include "vier.h"

void vierinit()
{
  int i, j, pui;

  vnum = rows * (columns - 3) + columns * (rows - 3) +
    2 * (columns - 3) * (rows - 3);
  row_col = rows * columns;
  row_1_col = row_col - columns;
  row_2_col = row_1_col - columns;
  brett = (int *)malloc(row_col * sizeof(int));
  for (i = 0; i < (row_col); i++)
    brett[i] = 0;
  weiss = (int *)malloc(vnum * sizeof(int));
  schwarz = (int *)malloc(vnum * sizeof(int));
  for (i = 0; i < vnum; i++)
    weiss[i] = schwarz[i] = 1;
  for (i = 0; i < columns; i++) {
    frei[i] = i;
    doubles[i] = 0;
  }
  freip = (int **)malloc(row_col * sizeof(int *));
  for (j = 0; j < rows; j++)
    for (i = 0; i < columns; i++)
      freip[j * columns + i] = frei + i;
  doublesp = (int **)malloc(row_col * sizeof(int *));
  for (j = 0; j < rows; j++)
    for (i = 0; i < columns; i++)
      doublesp[j * columns + i] = doubles + i;
  j = 0;
  i = ((columns - 1) >> 1);
  if (columns & 1)
    reihenfolge[j++] = i;
  else {
    reihenfolge[j++] = i;
    reihenfolge[j++] = i + 1;
  }
  for (i--; i >= 0; i--) {
    reihenfolge[j++] = i;
    reihenfolge[j++] = columns - 1 - i;
  }
  pu = (int (*)[4])malloc(vnum * sizeof(*pu));
  pui = 0;
  for (i = (rows - 1) * columns; i >= 0; i -= columns)
    for (j = 0; j <= columns - 4; j++) {
      pu[pui][0] = i + j;
      pu[pui][1] = i + j + 1;
      pu[pui][2] = i + j + 2;
      pu[pui][3] = i + j + 3;
      pui++;
    }
  for (i = (rows - 4) * columns; i >= 0; i -= columns)
    for (j = 0; j < columns; j++) {
      pu[pui][0] = i + j;
      pu[pui][1] = i + j + columns;
      pu[pui][2] = i + j + 2 * columns;
      pu[pui][3] = i + j + 3 * columns;
      pui++;
    }
  for (i = (rows - 4) * columns; i >= 0; i -= columns)
    for (j = 3; j < columns; j++) {
      pu[pui][0] = i + j;
      pu[pui][1] = i + j + columns - 1;
      pu[pui][2] = i + j + 2 * (columns - 1);
      pu[pui][3] = i + j + 3 * (columns - 1);
      pui++;
    }
  for (i = (rows - 4) * columns; i >= 0; i -= columns)
    for (j = 0; j <= columns - 4; j++) {
      pu[pui][0] = i + j;
      pu[pui][1] = i + j + columns + 1;
      pu[pui][2] = i + j + 2 * (columns + 1);
      pu[pui][3] = i + j + 3 * (columns + 1);
      pui++;
    }
  _p_h_ = (int *)malloc((row_col + 4 * vnum) * sizeof(int));
  pp = (int **)malloc(row_col * sizeof(int));
  for (pui = i = 0; i < row_col; i++) {
    pp[i] = _p_h_ + pui;
    for (j = 0; j < vnum; j++)
      if (pu[j][0] == i || pu[j][1] == i || pu[j][2] == i || pu[j][3] == i)
	_p_h_[pui++] = j;
    _p_h_[pui++] = -1;
  }
  sp = stack = (struct oldv *)
    malloc(4 * (vnum + row_col) * sizeof(struct oldv));
  zugstack = (struct oldv **)malloc(row_col * sizeof(struct oldv *));
}
