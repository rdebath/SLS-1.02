# Include-File: Amiga-Spezifisches, das nur von wenigen Modulen benötigt wird
# Jörg Höhle 8.3.1993


# Verhindert Multitasking kurzzeitig.
# Forbid(); ... Permit();
# Aufrufe können geschachtelt werden.
  extern void Forbid (void); # siehe exec.library/Forbid
  extern void Permit (void); # siehe exec.library/Permit
# wird verwendet von REXX


# Öffnet eine 'shared library'.
# OpenLibrary(name,version)
# > name: Name als ASCIZ-String, mit .library am Schluß
# > version: kleinste erwünschte Versionsnummer, 0 bedeutet "egal"
# < struct Library * ergebnis: Library base Zeiger oder NULL
  extern struct Library * OpenLibrary (CONST UBYTE* name, ULONG version); # siehe exec.library/OpenLibrary
# wird verwendet von REXX

# Schließt eine geöffnete 'shared library'.
# CloseLibrary(library)
# > library: 'library base' Zeiger
  extern void CloseLibrary (struct Library * library);
# wird verwendet von REXX


# "Port"s sind Einheiten zum Austausch von Nachrichten ("Messages").
# (Wir haben's hier nur mit den sogenannten "öffentlichen Ports".)
# Sie tragen einen Namen.

# Sucht einen Port gegebenen Namens.
# FindPort(name)
# > name: Name des Ports
# < ergebnis: Port-Pointer oder NULL falls es keinen gibt
# Muß von Forbid()/Permit() umrahmt sein.
  extern struct MsgPort * FindPort (CONST UBYTE* name); # siehe exec.library/FindPort
# wird verwendet von REXX

# Alloziert einen neuen Port.
# CreatePort(name,priority)
# > name: Name des Ports
# > priority: Priorität
# < ergebnis: Port-Pointer
  extern struct MsgPort * CreatePort (UBYTE* name, LONG priority); # siehe amiga.lib/CreatePort
# wird verwendet von REXX

# Meldet einen Port ab (vor dem Freigeben nötig).
# RemPort(port)
# > port: Port-Pointer
  extern void RemPort (struct MsgPort * port); # siehe exec.library/RemPort
# wird verwendet von REXX

# Gibt einen Port wieder frei.
# DeletePort(port)
# > port: Port-Pointer
  extern void DeletePort (struct MsgPort * port); # siehe amiga.lib/DeletePort
# wird verwendet von REXX

# Holt eine Message an einem Port ab.
# GetMsg(port)
# > port: Port-Pointer
# < ergebnis: Message-Pointer oder NULL falls gerade keine Message anliegt.
# Die abgeholte Message wird aus der Warteschlange von Messages entfernt.
  extern struct Message * GetMsg (struct MsgPort * port); # siehe exec.library/GetMsg
# wird verwendet von REXX

# Beendet die Bearbeitung einer abgeholten Message.
# ReplyMsg(message);
# > message: Message-Pointer
  extern void ReplyMsg (struct Message * message); # siehe exec.library/ReplyMsg
# wird verwendet von REXX

# Schickt eine Message an einen Port.
# PutMsg(port,message);
# > port: Port-Pointer
# > message: Message-Pointer
# Der von der Message beanspruchte Speicher bleibt bis zu ihrer Beantwortung
# reserviert!
  extern void PutMsg (struct MsgPort * port, struct Message * message); # siehe exec.library/PutMsg
# wird verwendet von REXX

#if 0 # Von der CLISP-Library intern benutzte Funktionen
# extern struct List * NewList (struct List * list);
# extern long AllocSignal (long);
# extern void FreeSignal (long);
#endif


#ifdef REXX

# ARexx (= Amiga-Rexx) ist ein auf obigen Ports aufbauendes Kommunikations-
# system zwischen Applikationen.

#include <rexx/rxslib.h>
#if defined(GNU_INLINES)
  #include <inline/rexxsyslib.h>
#else
  #ifdef ANSI
    #include <clib/rexxsyslib_protos.h>
  #endif
#endif

# Arexx-Messages haben ein spezielles Aussehen:

# Erzeugt eine Message-Hülle für ARexx.
# CreateRexxMsg(msgport,ext,host)
# > msgport: Adresse des ARexx message Ports, der die Empfangsbestätigung bekommt
# > ext: (ASCIZ) Extension für aufzurufende Dateien, NULL bedeutet "REXX"
# > host: Name des ARexx message Ports, der externe Kommandos abarbeitet
# < ergebnis: ARexx Message oder NULL bei Fehler
  extern struct RexxMsg * CreateRexxMsg (struct MsgPort* msgport, UBYTE* extension, UBYTE* hostname); # siehe rexxsyslib/CreateRexxMsg
# wird verwendet von REXX

# Eine Message-Hülle hat Platz für 1 bis MAXRMARG Argument-Strings.

# Gibt die Argument-Strings in einer Message-Hülle wieder frei.
# ClearRexxMsg(msg,argcount);
# > msg: ARexx Message
# > argcount: Anzahl Argumente
  extern void ClearRexxMsg (struct RexxMsg * msg, ULONG argcount); # siehe rexxsyslib/ClearRexxMsg
# wird verwendet von REXX

# Gibt eine Message-Hülle wieder frei.
# DeleteRexxMsg(msg);
# > msg: ARexx Message
  extern void DeleteRexxMsg (struct RexxMsg * message); # siehe rexxsyslib/DeleteRexxMsg
# wird verwendet von REXX

# Außerdem müssen auch Argument-Strings speziell verpackt werden:

# Erzeugt Argument-String Struktur für ARexx.
# CreateArgstring(string,length)
# > [string..string+length-1]: angesprochener Speicherbereich
# > length: Länge, <2^16
# < ergebnis: verpackter, kopierter Argument-String
  extern UBYTE* CreateArgstring (UBYTE* string, ULONG length); # siehe rexxsyslib/CreateArgstring
# wird verwendet von REXX

# Gibt Argument-String Struktur wieder frei.
# DeleteArgstring(argstring);
# > argstring: verpackter Argument-String
  extern void DeleteArgstring (UBYTE* argstring); # siehe rexxsyslib/DeleteArgstring
# wird verwendet von REXX

# Liefert die Länge eines verpackten Argument-Strings.
# LengthArgstring(argstring)
# > argstring: verpackter Argument-String
# < ergebnis: Länge
  extern ULONG LengthArgstring (UBYTE* argstring); # siehe rexxsyslib/LengthArgstring
# wird verwendet von REXX

# Verpackt eine ganze Argumentliste.
# FillRexxMsg(msg,argcount,mask)
# > msg: Message-Hülle
# > argcount: Anzahl Argumente (>=1, <=16)
# > mask: Bitmaske für Argumenttyp in der Hülle (jeweils 0 = ASCIZ, 1 = LONG)
# < ergebnis: /=0 falls OK
  extern BOOL FillRexxMsg (struct RexxMsg * msg, ULONG argcount, ULONG mask); # siehe rexxsyslib.library/FillRexxMsg
# wird verwendet von

#define RXERRORIMGONE 100L
#define RXADIR "AREXX"  # MsgPort für Asynchrone Bearbeitung

#endif # REXX

