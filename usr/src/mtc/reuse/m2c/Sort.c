#include "SYSTEM_.h"

#ifndef DEFINITION_Sort
#include "Sort.h"
#endif


static void QuickSort ARGS((INTEGER Lwb, INTEGER Upb));

static Sort_tProcIntInt *G_1_Swap;
static Sort_tProcIntIntBool *G_2_IsLess;

static void QuickSort
# ifdef __STDC__
(INTEGER Lwb, INTEGER Upb)
# else
(Lwb, Upb)
INTEGER Lwb, Upb;
# endif
{
  INTEGER i, j;

  for (;;) {
    if (Lwb >= Upb) {
      return;
    }
    i = Lwb + 1;
    j = Upb;
    do {
      while (i < Upb && (**G_2_IsLess)(i, Lwb)) {
        INC(i);
      }
      while (Lwb < j && (**G_2_IsLess)(Lwb, j)) {
        DEC(j);
      }
      if (i < j) {
        (**G_1_Swap)(i, j);
      }
    } while (!(i >= j));
    (**G_1_Swap)(Lwb, j);
    QuickSort(Lwb, j - 1);
    Lwb = j + 1;
  } EXIT_1:;
}

void Sort_Sort
# ifdef __STDC__
(INTEGER Lwb, INTEGER Upb, Sort_tProcIntIntBool IsLess, Sort_tProcIntInt Swap)
# else
(Lwb, Upb, IsLess, Swap)
INTEGER Lwb, Upb;
Sort_tProcIntIntBool IsLess;
Sort_tProcIntInt Swap;
# endif
{
  Sort_tProcIntInt *L_1;
  Sort_tProcIntIntBool *L_2;

  L_1 = G_1_Swap;
  G_1_Swap = &Swap;
  L_2 = G_2_IsLess;
  G_2_IsLess = &IsLess;
  QuickSort(Lwb, Upb);
  G_1_Swap = L_1;
  G_2_IsLess = L_2;
}

void BEGIN_Sort()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;


  }
}
