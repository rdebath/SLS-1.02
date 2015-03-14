# Include-File für VMS-Version von CLISP
# Bruno Haible 13.3.1993


# Konstanten für Steuerzeichen:

#define BEL  7              # Ton ausgeben
# define NL  10             # New line, siehe LISPBIBL.D
#define RUBOUT 127          # Rubout = Delete
#define CRLFstring  "\r\n"  # C-String, der BS-Newline enthält

#define stdin_handle   0  # File-Handle von Standard-Input
#define stdout_handle  1  # File-Handle von Standard-Output

# Deklaration von Typen von Ein-/Ausgabe-Parametern von Betriebssystemfunktionen
  #include <stdlib.h>
  #include <types.h>
  #include <unixlib.h>
  #include <unixio.h> # enthält ein #include <stat.h> # ??

# Tabelle der System-Fehlermeldungen
  #include <errno.h>
  # extern int errno; # letzter Fehlercode
  # extern int sys_nerr; # Anzahl der Betriebssystem-Fehlermeldungen
  # extern char* sys_errlist[]; # Betriebssystem-Fehlermeldungen
  # extern int vaxc$errno; # VMS-Fehlercode bei errno = EVMSERR
  # siehe PERROR(3)
# wird verwendet von MISC, STREAM, PATHNAME

# Bereitstellen des Arbeitsspeichers
  extern void* malloc (size_t size); # siehe MALLOC(3V)
  extern void free (void* ptr); # siehe MALLOC(3V)
# wird verwendet von SPVW, STREAM

# Normales Programmende
# Unter VMS ist Beendigung von main() mit "return status;" *nicht*
# äquivalent zu "exit(status);".
# Normales Programmende daher mit "exit(0);" statt "return 0;".
  extern nonreturning void exit (int status); # siehe EXIT(2V)
  extern nonreturning void _exit (int status); # siehe EXIT(2V)
# Die folgenden Status-Werte sind für "return status;" geeignet:
  #define SUCCESS_EXIT_CODE  1
  #define FATAL_EXIT_CODE 0x1000002C  # Failure, and no DCL message
# wird verwendet von SPVW, PATHNAME, STREAM

# Sofortiger Programmabbruch, Sprung in den Debugger
  extern void abort (void); # Programmabbruch
  # define abort()  LIB$SIGNAL(SS$_DEBUG)  # Sprung in den Debugger
# wird verwendet von SPVW, DEBUG, EVAL, IO

# Signalbehandlung
  #include <signal.h>
  # Ein Signal-Handler ist eine Funktion ohne Ergebnis.
  typedef void (*signal_handler) ();
  extern signal_handler signal (int sig, signal_handler handler); # siehe SIGNAL(3V)
  #if 0 # unbenutzt
    # Ein Signal blockieren und wieder freigeben: (vgl. SIGNALBLOCK_BSD)
    #define sigmask(sig)  (1L<<(sig-1))
    extern int sigblock (int mask); # siehe SIGBLOCK(2)
    extern int sigsetmask (int mask); # siehe SIGSETMASK(2)
    #define signalblock_on(sig)  \
      { var int old_sigblock_mask = sigblock(sigmask(sig));
    #define signalblock_off(sig)  \
        sigsetmask(old_sigblock_mask); \
      }
  #endif
  # Ein Signal erst eine bestimmte Zeit später ausliefern:
  extern int alarm (unsigned int seconds); # siehe ALARM(3V)
  # Die Ankunft eines Signals quittieren (aus dem Signal-Handler heraus):
  #define signal_acknowledge(sig,handler)  signal(sig,handler) # Handler bleibt weiter aktiv
# wird verwendet von SPVW

# Environment-Variablen abfragen:
  extern char* getenv (const char* name); # siehe GETENV(3V)
  # einige vordefinierte Werte:
  # getenv("USER") -> "HAIBLE"
  # getenv("HOME") -> "mike$duc0:[clisp-1993-02-03]"
  # getenv("PATH") -> "mike$duc0:[clisp-1993-02-03.utils]"
  # getenv("TERM") -> "vt100-80"
  # getenv("SYS$NODE") -> "MIKE::"
  # getenv("SYS$LOGIN") -> "MIKE$DUC0:[CLISP-1993-02-03]"
# wird verwendet von PATHNAME, SPVW, MISC, VMSAUX

# Working Directory setzen:
  extern int chdir (char* path); # siehe CHDIR(2V)
# wird verwendet von PATHNAME

# Working Directory abfragen:
  # Maximale Pfadlänge (incl. Nullbyte am Schluß), die von getwd geliefert wird:
  #define MAXPATHLEN  1024
  extern char* getcwd (char* buf, unsigned int bufsize);
  #define getwd(buf)  getcwd(buf,MAXPATHLEN)
# wird verwendet von PATHNAME

# Information zu einem File erfragen:
  # include <stat.h> # s.o.
  extern int stat (STAT_CONST char* path, struct stat * buf); # siehe STAT(2V)
  #define lstat  stat
  extern int fstat (int fd, struct stat * buf); # siehe STAT(2V)
  #define S_ISDIR(m)  (((m)&S_IFMT) == S_IFDIR)
  #define S_ISLNK(m)  FALSE
  #define S_ISREG(m)  (((m)&S_IFMT) == S_IFREG)
# wird verwendet von PATHNAME, STREAM

# File löschen:
  # remove() und delete() sind identisch, mit der Semantik von unlink().
  #define unlink  remove
  extern int unlink (const char* path); # siehe UNLINK(2V)
# wird verwendet von PATHNAME

# File umbenennen:
  extern int rename (const char* oldpath, const char* newpath); # siehe RENAME(2V)
# wird verwendet von PATHNAME

# Directory-Suche:
  #define MAXNAMLEN    255 # NAM$C_MAXRSS aus <nam.h>
  # data from readdir():
    struct direct
      { struct direct * d_next;
        unsigned short d_namlen;            # length of string in d_name
        char           d_name[MAXNAMLEN+1]; # full truename of file
      };
  # stream data from opendir():
    typedef struct
      { struct direct * dd_all;  # linked list of records of type "struct direct"
        struct direct * dd_next; # pointer to next field in this list
      } DIR;
  #define SDIRENT  struct direct
  extern DIR* opendir (char* filename); # filename muß auf "*.*;*" enden
  extern SDIRENT* readdir (DIR* dirp);
  extern void closedir (DIR* dirp);
  #define CLOSEDIR(dirp)  (closedir(dirp),0)
# wird verwendet von PATHNAME

# Directory anlegen:
  extern int mkdir (char* path, unsigned int mode); # siehe MKDIR(2V)
# wird verwendet von PATHNAME

# Directory löschen: ??
# wird verwendet von PATHNAME

# Arbeiten mit offenen Files:
  # include <unixio.h> # s.o.
  extern int open (char* path, int flags, unsigned int mode, ...); # siehe OPEN(2V)
  extern int creat (char* path, unsigned int mode, ...);
  extern int access (char* path, int amode);
  #define my_open_mask  0644
  #define Handle  uintW  # Typ eines File-Deskriptors
  extern int lseek (int fd, int offset, int whence); # siehe LSEEK(2V)
  extern int read (int fd, char* buf, int nbyte); # siehe READ(2V)
  extern int write (int fd, void* buf, int nbyte); # siehe WRITE(2V)
  extern int close (int fd); # siehe CLOSE(2V)
# wird verwendet von STREAM, PATHNAME, SPVW, MISC

# Terminal-Abfragen, Abfragen der Fenster-Größe:
  extern int isatty (int fd); # siehe TTYNAME(3V)
  extern int isapipe (int fd);
  extern char* getname (int fd, char* buffer, ...);
  extern char* ttyname (int fd); # siehe VMSAUX.D
  extern void vms_scrsize (int* scrsize); # siehe VMSAUX.D ??
# wird verwendet von SPVW, STREAM

# Datum/Uhrzeit:
#   SYS$ASCTIM wandelt Binärzeit in Asciizeit um.
#   SYS$BINTIM wandelt Asciizeit in Binärzeit um.
#   SYS$GETTIM liefert die aktuelle Uhrzeit als 64-Bit-Zahl (100ns-Intervalle
#              seit 17.11.1858 00:00:00.00).
#   SYS$NUMTIM wandelt Binärzeit in die 7 Einzelkomponenten um.
#   LIB$ADD_TIMES addiert zwei Uhrzeiten als 64-Bit-Zahlen.
#   LIB$CONVERT_DATE_STRING wandelt eine Asciizeit in 64-Bit-Zahl um.
#   LIB$CVT_FROM_INTERNAL_TIME wandelt Binärzeit in externe Binärzeit um.
#   LIB$CVT_TO_INTERNAL_TIME wandelt externe Binärzeit in Binärzeit um.
#   LIB$CVT_VECTIM wandelt 7 Einzelkomponenten in eine Binärzeit um.
#   LIB$DATE_TIME liefert die aktuelle Uhrzeit als String.
#   LIB$DAY liefert die Anzahl der Tage seit 17.11.1858.
#   LIB$DAY_OF_WEEK berchnet den Wochentag einer Tagesangabe.
#   LIB$FORMAT_DATE_TIME legt das Format für Asciizeiten fest.
#   LIB$GET_DATE_FORMAT liefert das Format für Asciizeiten.
#   LIB$GET_MAXIMUM_DATE_LENGTH Hilfsfunktion für LIB$FORMAT_DATE_TIME.
#   LIB$INIT_DATE_TIME_CONTEXT Vorbesetzung des Formats für Asciizeiten.
#   LIB$MULT_DELTA_TIME Skalarmultiplikation einer 64-Bit-Zahl.
#   LIB$SUB_TIMES subtrahiert zwei Uhrzeiten als 64-Bit-Zahlen.
#   LIB$SYS_ASCTIM wie SYS$ASCTIM.
#   asctime    wandelt tm_t in Ascii um.
#   clock      liefert verbrauchte CPU-Zeit seit Programmstart in 1/100 s.
#   ctime      wandelt time_t in Ascii um.
#   difftime   subtrahiert zwei time_t.
#   ftime      liefert Uhrzeit als timeb_t.
#   gmtime     wandelt time_t (GMT) in tm_t um.
#   localtime  wandelt time_t in tm_t um.
#   time       liefert Uhrzeit als time_t.
#   times      liefert verbrauchte CPU-Zeit seit Programmstart.

# Datum/Uhrzeit verarbeiten:
  #include <time.h>
  extern struct tm * localtime (const time_t* clock); # siehe CTIME(3V)
# wird verwendet von SPVW

# Datum/Uhrzeit abfragen:
  #include <time.h>
  extern void ftime (struct timeb * tp); # siehe TIME(3V)
# wird verwendet von SPVW, MISC

# vom Prozeß verbrauchte Zeit erfragen:
  #include <time.h>
  extern /* unsigned */ long clock (void); # siehe CLOCK(3)
  # extern void times (tbuffer_t* buffer); # siehe TIMES(3V)
# wird verwendet von SPVW

# Eine bestimmte Zeit Pause machen:
  # extern int sleep (unsigned int seconds); # siehe SLEEP(3V)
  extern void vms_sleep (int amount); # siehe VMSAUX.D
# wird verwendet von MISC

# Programme aufrufen:
  #include <processes.h>
  extern int pipe (int fd[2]); # siehe PIPE(2V)
  extern int vfork (void); # siehe VFORK(2)
  extern int dup2 (int fd1, int fd2); # siehe DUP(2V)
  extern int execv (char* path, char* argv[]); # siehe EXECL(3V)
  extern int execl (char* path, char* arg, ...); # siehe EXECL(3V)
  extern int wait (int* statusp); # siehe WAIT(2V)
  extern int system (const char* command); # siehe SYSTEM(3)
# wird verwendet von STREAM, PATHNAME

# Zufallszahlen besorgen:
  extern int rand (void); # siehe RAND(3V)
  extern int getpid (void); # siehe GETPID(2V)
# wird verwendet von LISPARIT


# Zu tun:
# lispbibl, pathname, constobj: Pathnames
#
# CR/LF-Umwandlung
#
# UNIX:
# constsym
# lispbibl
# misc
# pathname
# pseudofun
# spvw
# stream
# subr

