#define DEFINITION_DynArray

extern void DynArray_MakeArray ARGS((ADDRESS *ArrayPtr, LONGINT *ElmtCount, LONGINT ElmtSize));
extern void DynArray_ExtendArray ARGS((ADDRESS *ArrayPtr, LONGINT *ElmtCount, LONGINT ElmtSize));
extern void DynArray_ReleaseArray ARGS((ADDRESS *ArrayPtr, LONGINT *ElmtCount, LONGINT ElmtSize));
extern void BEGIN_DynArray();
