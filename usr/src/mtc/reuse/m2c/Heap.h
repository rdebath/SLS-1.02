#define DEFINITION_Heap

extern LONGCARD Heap_HeapUsed;
extern ADDRESS Heap_Alloc ARGS((LONGINT ByteCount));
extern void Heap_Free ARGS(());
extern void BEGIN_Heap();
