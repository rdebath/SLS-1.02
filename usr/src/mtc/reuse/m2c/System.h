#define DEFINITION_System

#define System_cMaxFile	32
#define System_StdInput	0
#define System_StdOutput	1
#define System_StdError	2
typedef INTEGER System_tFile;
extern System_tFile OpenInput ARGS((CHAR FileName[], LONGCARD ));
extern System_tFile OpenOutput ARGS((CHAR FileName[], LONGCARD ));
extern INTEGER Read ARGS((System_tFile File, ADDRESS Buffer, INTEGER Size));
extern INTEGER Write ARGS((System_tFile File, ADDRESS Buffer, INTEGER Size));
extern void Close ARGS((System_tFile File));
extern BOOLEAN IsCharacterSpecial ARGS((System_tFile File));
extern ADDRESS SysAlloc ARGS((LONGINT ByteCount));
extern LONGINT Time ARGS(());
extern CARDINAL GetArgCount ARGS(());
extern void GetArgument ARGS((INTEGER ArgNum, CHAR Argument[], LONGCARD ));
extern void PutArgs ARGS((INTEGER Argc, ADDRESS Argv));
extern INTEGER ErrNum ARGS(());
extern INTEGER System ARGS((CHAR String[], LONGCARD ));
extern void Exit ARGS((INTEGER Status));
extern void BEGIN_System();
