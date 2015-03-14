# Include-File für AMIGA-Version von CLISP
# Bruno Haible, Jörg Höhle 9.3.1993


# Konstanten für Steuerzeichen:

#define BEL  7              # Ton ausgeben
#define CSI  (0x9B)         # Control-Sequence-Introducer (Meta-Escape)
#define RUBOUT BS           # Rubout = Backspace
#define CRLFstring  "\n"    # C-String, der OS-Newline enthält


# Macros, um Longword-aligned Strukturen (z.B. im Stack) zu erzeugen:
# Statt
#   { var struct FileInfoBlock fib;
#     var struct FileInfoBlock * fibptr = &fib;
#     ...
#   }
# schreibe
#   { var LONGALIGNTYPE(struct FileInfoBlock) fib;
#     var struct FileInfoBlock * fibptr = LONGALIGN(&fib);
#     ...
#   }
  #define LONGALIGNTYPE(type)  struct { type dummy1; uintW dummy2; }
  #define LONGALIGN(addr)      ((void*)(floor(((uintL)(addr)+2),4)*4))
# wird verwendet von PATHNAME


# Wir definieren dieses selbst, brauchen nur Teile von <exec/types.h>:
#define BYTE   OS_BYTE
#define UBYTE  OS_UBYTE
#define WORD   OS_WORD
#define UWORD  OS_UWORD
#define LONG   OS_LONG
#define ULONG  OS_ULONG
#include <exec/types.h>
#undef ULONG
#undef LONG
#undef UWORD
#undef WORD
#undef UBYTE
#undef BYTE

#include <exec/memory.h>         # für Allocate()-Deklaration, MEMF_24BITDMA
#include <libraries/dos.h>
#include <libraries/dosextens.h>

#ifdef ANSI
  #include <stdlib.h>
  #if defined(GNU_INLINES)
    # M. Wilds Prototypen inline/*.h weichen von den offiziellen ab!
    #define ASTRING  STRPTR
  #else
    # das sind die offiziellen Prototypen:
    #define ASTRING  UBYTE*
    #include <clib/dos_protos.h>
    #include <clib/exec_protos.h>
  #endif
#endif


# BCPL-Pointer (hier BCPL* genannt, Typ BPTR) sind durch 4 teilbare Adressen,
# die durch 4 dividiert wurden.
  #define BPTR_NULL  ((BPTR)0)     # das ist ein spezieller, aber gültiger BPTR
  #define BPTR_NONE  ((BPTR)(-1))  # das ist ein ungültiger BPTR, Erkennungszeichen


# Typ eines Handle:
  #define Handle  BPTR
  #define Handle_NULL  BPTR_NULL
  #define FOREIGN_HANDLE  # verpacke die Handles sicherheitshalber
# wird verwendet von PATHNAME, SPVW


# Typ diverser Pointer, die - weil nicht notwendig Adressen - als Foreign
# verpackt werden müssen:
  #define FOREIGN  void*


# Für asciz_out() und *terminal-io*, initialisiert von SPVW:
  extern Handle Input_handle;     # low-level stdin Eingabekanal
  extern Handle Output_handle;    # low-level stdout Ausgabekanal


# Deklaration von Typen von Ein-/Ausgabe-Parametern von Betriebssystemfunktionen
  #define CONST const


# Programm verlassen und beenden.
# exit(returncode);
# > LONG returncode: Rückgabewert an den Aufrufer des Programms
#                    (z.B. RETURN_OK, RETURN_WARN, RETURN_ERROR, RETURN_FAIL)
  extern nonreturning void exit (int returncode);
# wird verwendet von SPVW


# Holt das aktuelle Datum und die aktuelle Uhrzeit.
# DateStamp(&datestamp);
# < struct DateStamp datestamp: aktuelle Zeit
#     LONG datestamp.ds_Days   : Anzahl Tage seit 1.1.1978
#     LONG datestamp.ds_Minute : Anzahl Minuten seit Tagesanfang
#     LONG datestamp.ds_Tick   : Anzahl Ticks (1/50 sec) seit Minutenanfang
  extern struct DateStamp * DateStamp (struct DateStamp * datestamp); # siehe dos.library/DateStamp
# wird verwendet von SPVW

# Wartet eine bestimmte Zeit.
# Delay(ticks);
# > ULONG ticks: Anzahl (>0) von Ticks (1/50 sec), die zu warten ist
  extern void Delay (long ticks); # siehe dos.library/Delay
# wird verwendet von MISC


# Holt die letzte Fehlernummer.
# errno = IoErr();
  extern LONG IoErr (void);
# wird verwendet von MISC, PATHNAME


# Öffnet eine Datei.
# handle = Open(filename,mode)
# > filename: Name eines Files oder Device (ASCIZ-String)
# > mode: Zugriffsmodus, z.B. MODE_OLDFILE oder MODE_NEWFILE oder MODE_READWRITE
# < handle: Handle oder NULL bei Fehler.
  extern Handle Open (CONST ASTRING filename, long mode); # siehe dos.library/Open
  #define Open(filename,mode)  (Open)((CONST ASTRING)(filename),mode)
# wird verwendet von SPVW, PATHNAME, STREAM

# Schließt eine Datei.
# Close(handle)
# > handle: Handle eines offenen Files
# < ergebnis: keins!
  extern LONG Close (Handle handle); # siehe dos.library/Close
# wird verwendet von SPVW, PATHNAME, STREAM

# Liest von einer Datei.
# ergebnis = Read(handle,bufferaddr,bufferlen);
# > handle: Handle eines offenen Files
# > bufferaddr: Adresse eines Buffers mit bufferlen Bytes
# < ergebnis: Anzahl der gelesenen Bytes (0 bei EOF) oder -1 bei Fehler.
  extern LONG Read (Handle handle, void* bufferaddr, long bufferlen); # siehe dos.library/Read
# wird verwendet von SPVW, STREAM

# Schreibt auf eine Datei.
# ergebnis = Write(handle,bufferaddr,bufferlen);
# > handle: Handle eines offenen Files
# > bufferaddr: Adresse eines Buffers mit bufferlen Bytes
# < ergebnis: Anzahl der geschriebenen Bytes oder -1 bei Fehler.
  extern LONG Write (Handle handle, CONST void* bufferaddr, long bufferlen); # siehe dos.library/Write
# wird verwendet von SPVW, STREAM

# Dateizeiger positionieren.
# Seek(handle,pos,mode)
# > Handle handle : Handle eines (offenen) Files.
# > LONG pos : Position in Bytes (muß >=0 bei Modus OFFSET_BEGINNING
#                                 bzw. <=0 bei Modus OFFSET_END sein)
# > LONG mode : Modus (OFFSET_BEGINNING = ab Fileanfang,
#                      OFFSET_CURRENT = ab momentan, OFFSET_END = ab Fileende)
# < LONG ergebnis : alte(!) Position ab Fileanfang oder -1 bei Fehler.
  extern LONG Seek (Handle handle, long pos, long mode); # siehe dos.library/Seek
# wird verwendet von STREAM


# Holt den Eingabe-Handle
# Input()
# < ergebnis: Eingabe-Handle, NULL bei WorkBench-Aufruf des Programms
  extern Handle Input (void); # siehe dos.library/Input
# verwendet von SPVW

# Holt den Ausgabe-Handle
# Output()
# < ergebnis: Ausgabe-Handle, NULL bei WorkBench-Aufruf des Programms
  extern Handle Output (void); # siehe dos.library/Output
# verwendet von SPVW


# Legt den Finger auf eine Datei oder ein Directory.
# lock = Lock(name,mode);
# > name: Name der Datei bzw. des Directory (ASCIZ-String)
# > mode: Zugriffsmodus, z.B. ACCESS_READ (= SHARED_LOCK) oder ACCESS_WRITE (= EXCLUSIVE_LOCK)
# < struct FileLock BCPL* lock: Lock oder NULL bei Fehler.
  extern BPTR Lock (CONST ASTRING name, long mode); # siehe dos.library/Lock
  #define Lock(name,mode)  (Lock)((CONST ASTRING)(name),mode)
# wird verwendet von PATHNAME

# Läßt eine Datei oder ein Directory wieder los.
# UnLock(lock);
# > struct FileLock BCPL* lock: Lock oder NULL.
  extern void UnLock (BPTR lock); # siehe dos.library/UnLock
# wird verwendet von PATHNAME

# Holt Informationen zu einer Datei oder einem Directory.
# ergebnis = Examine(lock,&fib);
# > struct FileLock BCPL* lock: Lock.
# > struct FileInfoBlock fib: Platz für die Informationen, LONGALIGNED,
#     LONG fib.fib_DirEntryType     : >0 bei Directory, <0 bei normaler Datei
#     char fib.fib_Filename []      : Filename, ein ASCIZ-String
#     LONG fib.fib_Size             : Größe des Files in Bytes
#     struct DateStamp fib.fib_Date : Datum der letzten Modifikation
#     char fib.fib_Comment []       : Kommentar
# < ergebnis: NULL bei Fehler
  extern LONG Examine (BPTR lock, struct FileInfoBlock * fib); # siehe dos.library/Examine
# wird verwendet von PATHNAME

# Holt Informationen zu einem Directory-Eintrag.
# ergebnis = ExNext(lock,&fib);
# > struct FileLock BCPL* lock: Lock auf ein Directory.
# > struct FileInfoBlock fib: Platz für die Informationen, wie bei Examine()
# < ergebnis: NULL bei Fehler, und IoErr()=ERROR_NO_MORE_ENTRIES am Schluß.
  extern LONG ExNext (BPTR lock, struct FileInfoBlock * fib); # siehe dos.library/ExNext
# wird verwendet von PATHNAME

# Durchsuchen von Directories nach Dateien:
# 1. Lock fürs Directory besorgen: Lock().
# 2. Examine() dieses Lock in einen FIB.
# 3. ExNext() dieses Lock und denselben FIB solange bis ERROR_NO_MORE_ENTRIES.
# 4. Lock zurückgeben: UnLock().

# Besorgt das Parent Directory zu einem Lock auf ein Directory.
# parentlock = ParentDir(lock);
# > struct FileLock BCPL* lock: Lock auf ein Directory
# < struct FileLock BCPL* parentlock: Lock aufs Parent Directory davon,
#     oder NULL bei Fehler oder bei Versuch, Parent vom Root Dir zu nehmen.
  extern BPTR ParentDir (BPTR lock); # siehe dos.library/ParentDir
# wird verwendet von PATHNAME


# Datei löschen.
# DeleteFile(filename)
# > filename : Filename, ein ASCIZ-String
# < ergebnis : NULL bei Fehler.
  extern LONG DeleteFile (CONST ASTRING filename); # siehe dos.library/DeleteFile
  #define DeleteFile(filename)  (DeleteFile)((CONST ASTRING)(filename))
# wird verwendet von PATHNAME

# Datei umbenennen.
# Rename(oldname,newname)
# > oldname : alter Filename, ein ASCIZ-String
# > newname : neuer Filename, ein ASCIZ-String
# < ergebnis : NULL bei Fehler.
  extern LONG Rename (CONST ASTRING oldname, CONST ASTRING newname); # siehe dos.library/Rename
  #define Rename(oldname,newname)  (Rename)((CONST ASTRING)(oldname),(CONST ASTRING)(newname))
# wird verwendet von PATHNAME

# Neues Subdirectory anlegen.
# lock = CreateDir(name);
# > name: Pfadname, ein ASCIZ-String
# < struct FileLock BCPL* lock: Lock auf das neue Subdirectory, NULL bei Fehler.
  extern BPTR CreateDir (CONST ASTRING name); # siehe dos.library/CreateDir
  #define CreateDir(name)  (CreateDir)((CONST ASTRING)(name))
# wird verwendet von PATHNAME

# Subdirectory löschen.
# DeleteFile(name)
# > name : Pfadname, ein ASCIZ-String
# < ergebnis : NULL bei Fehler.
  extern LONG DeleteFile (CONST ASTRING filename); # siehe dos.library/DeleteFile
# wird verwendet von PATHNAME

# Current Directory (des Prozesses) neu setzen.
# CurrentDir(lock)
# > FileLock BCPL* lock: Lock auf ein Directory, wird zum neuen current directory
# < FileLock BCPL* ergebnis: Lock aufs alte current directory
# Beim Programmende muß das Current Directory Lock wieder dasselbe wie beim
# Programmstart sein. Alle anderen Locks sind freizugeben.
  extern BPTR CurrentDir (BPTR lock); # siehe dos.library/CurrentDir
# wird verwendet von PATHNAME, SPVW


# Stellt fest, ob ein File interaktiv (ein Terminal o.ä.) ist.
# IsInteractive(handle)
# > handle: Handle eines (offenen) Files
# < ergebnis: gibt an, ob das File interaktiv ist
  extern LONG IsInteractive (Handle handle); # siehe dos.library/IsInteractive
# wird verwendet von STREAM, SPVW

# Stellt fest, ob innerhalb einer gewissen Zeit ein Zeichen anliegt.
# WaitForChar(handle,timeout)
# > handle: Handle eines (offenen) interaktiven Files
# > timeout: maximale Wartezeit (>0), in Mikrosekunden
# < ergebnis: gibt an, ob bis zum Ende der Wartezeit ein Zeichen lesbar ist.
  extern LONG WaitForChar (Handle handle, long timeout);
# wird verwendet von STREAM


# Fordert ein Stück Speicher beim Betriebssystem an.
# AllocMem(size,preference)
# > size: angeforderte Größe in Bytes
# > preference: MEMF_ANY oder MEMF_24BITDMA
# < ergebnis: Anfangsadresse des allozierten Bereichs, oder NULL
  extern APTR AllocMem (unsigned long size, unsigned long flags); # siehe exec.library/AllocMem
# wird verwendet von SPVW

# Gibt dem Betriebssystem ein Stück Speicher zurück.
# FreeMem(address,size);
# > address: Anfangsadresse des allozierten Bereichs
# > size: Größe dieses Bereichs in Bytes
  extern void FreeMem (APTR address, unsigned long size); # siehe exec.library/FreeMem
# wird verwendet von SPVW


# Liefert einen Pointer auf die eigene Task.
# FindTask(NULL)
# < ergebnis: Pointer auf den Deskriptor der eigenen Task
  extern struct Task * FindTask (CONST UBYTE* taskname); # siehe exec.library/FindTask
# wird verwendet von MISC, LISPARIT

# Fragt die aktuelle Signal-Maske ab und modifiziert sie.
# (Die Signal-Maske gibt an, welche Signale bei der eigenen Task angekommen
# sind, aber noch nicht verarbeitet wurden. Wird vom Betriebssystem
# asynchron verändert.)
# SetSignal(signals_to_set,signals_to_change)
# < ergebnis: ehemalige Signal-Maske signals
# < neue Signal-Maske
#     signals := (signals & ~signals_to_change) | (signals_to_set & signals_to_change)
#              = signals ^ ((signals ^ signals_to_set) & signals_to_change)
  extern ULONG SetSignal (unsigned long signals_to_set, unsigned long signals_to_change); # siehe exec.library/SetSignal
# wird verwendet von Macro interruptp

# Wartet auf ein oder mehrere Signal(e).
# Wait(signals)
# > signals: Signale, auf die gewartet werden soll
# < ergebnis: Signale, die eingetreten sind
  extern ULONG Wait (unsigned long signals); # siehe exec.library/Wait
# wird verwendet von REXX, Macro abort


# Programme aufrufen.
# Execute(command,ihandle,ohandle)
# > command: Kommandozeile, wie man sie im Kommandozeilen-Interpreter eintippt
# > ihandle: Handle für weitere Kommandos, nachdem command abgearbeitet ist,
#            bei ihandle = 0 werden keine weiteren Kommandos ausgeführt.
# > ohandle: Handle für Ausgaben der Kommandos,
#            bei ohandle = 0 gehen Ausgaben ins aktuelle Fenster.
# < ergebnis: Flag, ob erfolgreich aufgerufen.
  extern LONG Execute (CONST ASTRING command, BPTR ihandle, BPTR ohandle); # siehe dos.library/Execute
  #define Execute(command,ihandle,ohandle)  (Execute)((CONST ASTRING)(command),ihandle,ohandle)
# wird verwendet von PATHNAME


# Springt in den Debugger.
# Debug(0);
  extern void Debug (unsigned long flags); # siehe exec.library/Debug
# wird verwendet von DEBUG

# Sofortiger Programmabbruch, Sprung in den Debugger
  #if defined(GNU) && 0 # Jörg mag das nicht so sehr bis überhaupt nicht
    #define abort()  __asm__ __volatile__ (" .word 0x4AFC ") # illegaler Befehl
  #elif 1
    # Je préfère Wait(0L) car ainsi le programme se met en attente infinie
    # et on peut essayer de savoir pourquoi en analysant la mémoire. Je ne
    # considère pas qu'une sortie de programme soit sûre puisque la mémoire
    # peut se trouver dans un mauvais état, il peut y avoir des fichiers
    # non fermés, des «Lock» alloués, etc.                       Jörg 7.1.1993
    #define abort()  \
      { asciz_out(CRLFstring "CLISP panic! (going into endless loop)" CRLFstring); \
        Wait(0L);                                                                  \
      }
  #else
    #define abort()  # ich mach mal ein wenig auf Unix...
      { asciz_out(CRLFstring "Signal SIGABRT: Abort (core dumped)" CRLFstring); \
        quit_sofort(1);                                                         \
      }
  #endif
# wird verwendet von EVAL, IO


# STREAM.D : Terminal-Stream, finish_output_file, Pipe-Streams?
# PATHNAME.D : Wildcards mit regular expressions

