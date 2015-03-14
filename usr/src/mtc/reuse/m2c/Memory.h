#define DEFINITION_Memory

extern LONGCARD Memory_MemoryUsed;
extern ADDRESS Memory_Alloc ARGS((LONGINT ByteCount));
extern void Memory_Free ARGS((LONGINT ByteCount, ADDRESS a));
extern void BEGIN_Memory();
