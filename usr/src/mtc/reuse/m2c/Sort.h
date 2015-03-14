#define DEFINITION_Sort

typedef BOOLEAN (*Sort_tProcIntIntBool) ARGS((INTEGER, INTEGER));
typedef void (*Sort_tProcIntInt) ARGS((INTEGER, INTEGER));
extern void Sort_Sort ARGS((INTEGER Lwb, INTEGER Upb, Sort_tProcIntIntBool IsLess, Sort_tProcIntInt Swap));
extern void BEGIN_Sort();
