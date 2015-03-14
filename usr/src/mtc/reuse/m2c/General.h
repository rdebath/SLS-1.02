#define DEFINITION_General

extern INTEGER General_MaxAlign;
extern struct General_1 {
    BITSET A[8 + 1];
} General_AlignMasks;
extern INTEGER General_Min ARGS((INTEGER a, INTEGER b));
extern INTEGER General_Max ARGS((INTEGER a, INTEGER b));
extern CARDINAL General_Log2 ARGS((LONGINT x));
extern LONGINT General_Exp2 ARGS((CARDINAL x));
extern CARDINAL General_AntiLog ARGS((LONGINT x));
extern REAL General_Exp10 ARGS((INTEGER x));
extern void BEGIN_General();
