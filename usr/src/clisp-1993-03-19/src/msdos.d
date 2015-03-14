# Include-File für MSDOS-(DJUNIX/EMUNIX)-Version von CLISP
# Bruno Haible 19.3.1993


# Konstanten für Steuerzeichen:

#define BEL  7              # Ton ausgeben
# define NL  10             # New line, siehe LISPBIBL.D
#define RUBOUT 127          # Rubout = Delete
#define CRLFstring  "\r\n"  # C-String, der BS-Newline enthält

#define stdin_handle  0  # File-Handle von Standard-Input
#define stdout_handle  1  # File-Handle von Standard-Output

# Deklaration von Typen von Ein-/Ausgabe-Parametern von Betriebssystemfunktionen
  #ifdef DJUNIX
    #include <std.h>
    #define CONST const
    #define CONST1 const
    #define CONST2
  #endif
  #ifdef EMUNIX
    #include <sys/emx.h> # muß als erstes kommen
    #include <stdlib.h>
    #define CONST const
    #define CONST1
    #define CONST2 const
    #ifdef unix
      #define EMUNIX_OLD_8d  # emx <= 0.8d
    #else
      #define EMUNIX_NEW_8e  # emx >= 0.8e
    #endif
    #ifndef _SIZE_T
      #define EMUNIX_OLD_8e  # emx <= 0.8e definiert _SIZE_T_DEFINED
    #else
      #define EMUNIX_NEW_8f  # emx >= 0.8f definiert _SIZE_T
    #endif
  #endif

# Tabelle der System-Fehlermeldungen
  #include <errno.h>
  #define ENOMORE  18  # Fehlercode bei findfirst/findnext, fehlt in errno.h
  #ifdef EMUNIX
    #define ENOTTY  EBADF  # Fehlercode bei ioctl
  #endif
  extern int errno; # letzter Fehlercode
  #ifdef EMUNIX
    extern CONST int sys_nerr; # Anzahl der Betriebssystem-Fehlermeldungen
    #ifdef EMUNIX_OLD_8d
      extern CONST char* sys_errlist[]; # Betriebssystem-Fehlermeldungen
    #else
      extern CONST char* CONST sys_errlist[]; # Betriebssystem-Fehlermeldungen
    #endif
    # siehe PERROR(3)
  #endif
# wird verwendet von MISC, STREAM, PATHNAME

# Bereitstellen des Arbeitsspeichers
  extern void* malloc (size_t size); # siehe MALLOC(3V)
  extern void free (void* ptr); # siehe MALLOC(3V)
# wird verwendet von SPVW

# Normales Programmende
  extern nonreturning void _exit (int status); # siehe EXIT(2V)
# wird verwendet von SPVW

# Sofortiger Programmabbruch, Sprung in den Debugger
  #ifdef DJUNIX
    extern nonreturning void abort (void); # siehe ABORT(3)
  #endif
  #ifdef EMUNIX
    extern void abort (void); # siehe ABORT(3)
  #endif
# wird verwendet von DEBUG, EVAL, IO

# Signalbehandlung
  #ifdef EMUNIX
    #include <signal.h>
    # Ein Signal-Handler ist eine Funktion ohne Ergebnis.
    typedef SIGTY (*signal_handler) ();
    extern signal_handler signal (int sig, signal_handler handler); # siehe SIGNAL(3V)
    # Ein Signal erst eine bestimmte Zeit später ausliefern:
    extern unsigned int alarm (unsigned int seconds); # siehe ALARM(3V)
    # Die Ankunft eines Signals quittieren (aus dem Signal-Handler heraus):
    #define signal_acknowledge(sig,handler)  signal(sig,SIG_ACK)
  #endif
# wird verwendet von SPVW

# Environment-Variablen abfragen:
  extern char* getenv (CONST char* name); # siehe GETENV(3V)
# wird verwendet von PATHNAME, MISC

# Working Directory setzen:
  extern int chdir (CONST char* path); # siehe CHDIR(2V)
# wird verwendet von PATHNAME

# Working Directory abfragen:
  # Maximale Pfadlänge (incl. Nullbyte am Schluß), die von getwd geliefert wird:
    #define MAXPATHLEN  1024  # siehe <sys/param.h>
  #ifdef DJUNIX
    extern char* getwd (char* pathname); # siehe GETWD(3)
  #endif
  #ifdef EMUNIX
    extern char _getdrive (void);
    extern int _chdrive (char drive);
    extern char* getcwd (char* buf, int bufsize);
    extern int _getcwd1 (char* buf, char drive);
  #endif
# wird verwendet von PATHNAME

# Working Directory auf einem gegebenen Drive abfragen:
  # getwdof(&buf,drive)
  # > uintB* &buf: Adresse eines Path-Buffers
  # > uintB drive: Laufwerk (0=A, 1=B, ...)
  # < ergebnis: <0 falls Fehler
  #ifdef DJUNIX
    #define getwdof(buf,drive)  \
      ({__asm__ (# DOS Function 47H                                                         \
                 " movb $0x47,%%ah ; int $0x21 "                                            \
                 :                                                                # OUT     \
                 : "S" /* %esi */ ((uintB*)(buf)), "d" /* %dl */ ((uintB)(drive)) # IN      \
                 : "ax","bx","cx","di" /* %eax, %ebx, %ecx, %edi */               # CLOBBER \
                );                                                                          \
        0;                                                                                  \
       })
  #endif
  #ifdef EMUNIX
    #define getwdof(buf,drive)  _getcwd(buf,drive)
  #endif
# wird verwendet von PATHNAME

# Information zu einem File erfragen:
  extern int access (CONST char* path, int amode);
  #ifdef EMUNIX
    #include <io.h>
  #endif
  #include <sys/types.h>
  #include <sys/stat.h>
  extern int stat (CONST char* path, struct stat * buf); # siehe STAT(2V)
  # Test auf Directory:
    #undef S_ISDIR
    #define S_ISDIR(m)  (((m)&S_IFMT) == S_IFDIR)
  # Test auf reguläres File:
    #undef S_ISREG # unser Macro hierfür ist effizienter
    #define S_ISREG(m)  (((m)&(S_IFMT&~S_IFREG)) == 0)
  # Links gibt es keine:
    #define S_ISLNK(m)  FALSE
  # siehe auch PATHNAME:get_file_write_datetime()
# wird verwendet von PATHNAME

# File löschen:
  extern int unlink (CONST char* path); # siehe UNLINK(2V)
# wird verwendet von PATHNAME

# File umbenennen:
  extern int rename (CONST char* oldpath, CONST char* newpath); # siehe RENAME(2V)
# wird verwendet von PATHNAME

# Directory-Suche:
  #ifdef DJUNIX
    #include <dir.h>
    extern int findfirst (const char* pathname, struct ffblk * ffblk, int attrib);
    extern int findnext (struct ffblk * ffblk);
  #endif
  #ifdef EMUNIX
    #include <dirent.h>
    #if 0
      struct ffblk # gleich aufgebaut wie struct _find in <sys/emx.h>
                   { char ff_reserved[21];
                     unsigned char ff_attrib;
                     unsigned short ff_ftime;
                     unsigned short ff_fdate;
                     unsigned short ff_fsize[2]; # Vorsicht Alignment!
                     char ff_name[16];
                   };
    #else # so ist's eleganter:
      # include <sys/emx.h> # siehe oben
      #define ffblk  _find
      #define ff_reserved  reserved
      #define ff_attrib    attr
      #define ff_ftime     time
      #define ff_fdate     date
      #define ff_fsize     size_lo
      #define ff_name      name
    #endif
    extern int __findfirst (const char* pathname, int attrib, struct ffblk * ffblk);
    extern int __findnext (struct ffblk * ffblk);
    #define findfirst(path,buf,attr)  __findfirst(path,attr,buf)
    #define findnext(buf)  __findnext(buf)
    #define FA_DIREC   A_DIR
    #define FA_ARCH    A_ARCHIVE
    #define FA_RDONLY  A_RONLY
    #if 0
      # Kompatibel zu UNIX:
      #define SDIRENT  struct dirent
      extern DIR* opendir (CONST char* dirname); # siehe DIRECTORY(3V)
      extern SDIRENT* readdir (DIR* dirp); # siehe DIRECTORY(3V)
      extern void closedir (DIR* dirp); # siehe DIRECTORY(3V)
      #define CLOSEDIR(dirp)  (closedir(dirp),0)
    #endif
  #endif
# wird verwendet von PATHNAME

# Directory anlegen:
  #ifdef DJUNIX
    extern int mkdir (CONST char* path, int attrib);
  #endif
  #ifdef EMUNIX
    #ifdef EMUNIX_OLD_8e
      extern int mkdir (CONST char* path); # siehe MKDIR(2V)
      #define mkdir(path,attrib) (mkdir)(path)
    #else
      extern int mkdir (CONST char* path, long attrib);
    #endif
  #endif
# wird verwendet von PATHNAME

# Directory löschen:
  extern int rmdir (CONST char* path); # siehe RMDIR(2V)
# wird verwendet von PATHNAME

# Arbeiten mit offenen Files:
  #include <sys/types.h>
  #include <sys/file.h>
  #include <io.h>
  extern int open (CONST char* path, int flags, ...); # siehe OPEN(2V)
  #ifdef DJUNIX
    extern int creat (CONST char* path, unsigned long mode); # ignoriert das 2. Argument
  #endif
  #ifdef EMUNIX
    # definiert creat(path,mode) == open(path,O_WRONLY|O_TRUNC|O_CREAT,mode),
    # was wir wegen unserer Bufferung in STREAM nicht brauchen können.
    #define creat(path,mode)  open(path,O_RDWR|O_TRUNC|O_CREAT,mode)
  #endif
  #define my_open_mask  0644
  extern int setmode (int fd, int mode); # mode = O_TEXT oder O_BINARY
  # File-Modus: i.a. unbestimmt. (Nur bei EMUNIX kann man bei open() O_TEXT
  #   bzw. O_BINARY im 2. Argument angeben.) Daher ein setmode() nötig.
  #define Handle  uintW  # Typ eines File-Deskriptors
  extern off_t lseek (int fd, off_t offset, int whence); # siehe LSEEK(2V)
  #ifdef DJUNIX
    #define SEEK_SET  L_SET
    #define SEEK_CUR  L_CURR
    #define SEEK_END  L_XTND
  #endif
  #define RW_BUF_T  void*
  #ifdef EMUNIX
    extern int read (int fd, RW_BUF_T buf, size_t nbyte); # siehe READ(2V)
    extern int write (int fd, CONST RW_BUF_T buf, size_t nbyte); # siehe WRITE(2V)
  #else
    extern int read (int fd, RW_BUF_T buf, unsigned int nbyte); # siehe READ(2V)
    extern int write (int fd, CONST RW_BUF_T buf, unsigned int nbyte); # siehe WRITE(2V)
  #endif
  extern int close (int fd); # siehe CLOSE(2V)
  extern int dup (int fd); # siehe DUP(2V)
  #if !defined(DJUNIX)
    extern int fsync (int fd); # siehe FSYNC(2)
  #endif
  #ifdef EMUNIX_PORTABEL
    #include <sys/time.h>
    extern int select (int width, fd_set* readfds, fd_set* writefds, fd_set* exceptfds,
                       struct timeval * timeout); # siehe SELECT(2)
  #endif
# wird verwendet von STREAM, PATHNAME, SPVW, MISC

# Terminal-Abfragen:
  extern int isatty (int fd); # siehe TTYNAME(3V)
  #ifdef EMUNIX
    extern int ioctl (int fd, int request, ...); # siehe IOCTL(2)
    extern int __ioctl1 (int fd, int code); # führt einen INT 21,44,code aus
  #endif
  #ifdef EMUNIX_PORTABEL
    #include <sys/ioctl.h>
  #else
    # get_handle_input_status(handle,status);
    # > handle
    # < status: INT 21,44,06
    #ifdef DJUNIX
      #define get_handle_input_status(handle,status)  \
        __asm__ (# DOS-Funktion 44H, Code 06H                                        \
                 " movw $0x4406,%%ax ; int $0x21 "                                   \
                 : "=a" /* %al */ (status)                                 # OUT     \
                 : "b" /* %bx */ ((uintW)(handle))                         # IN      \
                 : "bx","cx","dx","si","di" /* %ebx,%ecx,%edx,%esi,%edi */ # CLOBBER \
                )
    #endif
    #ifdef EMUNIX
      #define get_handle_input_status(handle,status)  \
        __asm__ (# DOS-Funktion 44H, Code 06H                                        \
                 " movw $0x4406,%%ax ; call ___syscall "                             \
                 : "=a" /* %al */ (status)                                 # OUT     \
                 : "b" /* %bx */ ((uintW)(handle))                         # IN      \
                 : "bx","cx","dx","si","di" /* %ebx,%ecx,%edx,%esi,%edi */ # CLOBBER \
                )
    #endif
  #endif
  # get_handle_info(handle)
  # > handle
  # < ergebnis: Handle-Info (INT 21,44,00)
  #ifdef DJUNIX
    #define get_handle_info(handle)  \
      ({ var reg1 uintW __info;                                                       \
         __asm__ (# DOS-Funktion 44H, Code 00H                                        \
                  " movw $0x4400,%%ax ; int $0x21 "                                   \
                  : "=d" /* %dx */ (__info)                                 # OUT     \
                  : "b" /* %bx */ ((uintW)(handle))                         # IN      \
                  : "ax","bx","cx","si","di" /* %eax,%ebx,%ecx,%esi,%edi */ # CLOBBER \
                 );                                                                   \
         __info;                                                                      \
       })
  #endif
  #ifdef EMUNIX
    #define get_handle_info(handle)  __ioctl1(handle,0x00)
  #endif
  #ifdef EMUNIX_NEW_8e
    extern int eof (int fd); # meldet, ob EOF erreicht
    extern void _scrsize (int* dst); # dst[0]:=columns, dst[1]:=rows
  #endif
  #ifdef EMUNIX
    # vgl. UNIX_TERM_TERMIO
    #include <sys/termio.h> # siehe TERMIO(4)
    extern int tgetent (char* bp, char* name); # siehe TERMCAP(3X)
    extern int tgetnum (char* id); # siehe TERMCAP(3X)
    #ifdef EMUNIX_NEW_8f
      # "There's a bug in ioctl (..., TCSETAF, ...), which causes the new mode
      # flags to be ignored.  Use TCSETA instead.  If you have to flush the
      # buffer, use TCSETAF, then TCSETA." Eberhard Mattes 9.3.1993
      #undef TCSETAF
      #define TCSETAF  TCSETA
    #endif
  #endif
# wird verwendet von SPVW, STREAM

# Tastatur abfragen, direkte Bildschirm-Ausgabe
  #ifndef EMUNIX_PORTABEL
    #include <dos.h>
  #endif
  #ifdef DJUNIX
  # #include <pc.h>
  # kbhit() [TP: KeyPressed] sagt, ob ein Tastendruck wartet.
  # getch() [TP: ReadKey] liefert den letzten Tastendruck, wartet evtl.
  # Da getch() nicht den Scan-Code liefert - wie ein Blick in libsrc\c\dos\*k*.s
  # verrät - benutzen wir diese Funktionen aber nicht, sondern programmieren
  # das Nötige selber.
  #endif
  #if defined(EMUNIX_PORTABEL) && defined(EMUNIX_NEW_8f)
    #include <sys/video.h>
    extern int v_init (void);
    extern int v_hardware (void);
    extern void v_dimen (int* width, int* height);
    extern void v_getctype (int* start, int* end);
    extern void v_ctype (int start, int end);
    extern void v_attrib (int attrib);
    extern void v_putc (char c);
    extern void v_putn (char c, int count);
    extern void v_getxy (int* x, int* y);
    extern void v_gotoxy (int x, int y);
    extern void v_clear (void);
    extern void v_clreol (void);
    extern void v_delline (int count);
    extern void v_insline (int count);
    extern void v_scroll (int x_left, int y_top, int x_right, int y_bottom, int count, int direction);
  #endif
# wird verwendet von STREAM

# Datum/Uhrzeit verarbeiten:
  #include <time.h>
  extern struct tm * localtime (CONST time_t* clock); # siehe CTIME(3V)
# wird verwendet von SPVW

# Datum/Uhrzeit abfragen:
  #ifdef DJUNIX
    #include <sys/time.h>
    extern int gettimeofday (struct timeval * tp, struct timezone * tzp); # siehe GETTIMEOFDAY(2)
  #endif
  #ifdef EMUNIX_OLD_8d
    #include <sys/emx.h>
    extern void __ftime (struct _dtd * date_time);
  #endif
  #ifdef EMUNIX_NEW_8e
    #include <sys/timeb.h>
    extern void __ftime (struct timeb * time); # ftime() ohne Zeitzone
  #endif
  # siehe auch SPVW.D:get_time() und SPVW.D:get_decoded_time()
# wird verwendet von SPVW

# Programme aufrufen:
  #ifdef EMUNIX
    #include <process.h>
    extern int spawnv (int pmode, CONST char* path, CONST char* CONST argv[]);
  #endif
  extern int system (CONST char* command);
  # system(NULL) stellt fest, ob ein Kommandoprozessor zur Verfügung steht.
  # system(command) übergibt dem Kommandoprozessor einen Befehl.
# wird verwendet von PATHNAME

# Programme aufrufen:
  #ifdef EMUNIX_PORTABEL
    #include <stdio.h>
    extern FILE* popen (CONST char* command, CONST char* mode);
    extern int pclose (FILE* f);
    extern int fileno (FILE* f);
  #endif
# wird verwendet von STREAM

# "Zufalls"zahlen besorgen:
  #include <time.h>
  extern time_t time (time_t* tloc); # siehe TIME(3V)
# wird verwendet von LISPARIT


# Versionsabfragen:
  #ifdef EMUNIX
    #include <stdlib.h>
    extern const unsigned char _osmode;
    extern const unsigned char _osmajor;
    extern const unsigned char _osminor;
  #endif
# wird verwendet von SPVW, PATHNAME, STREAM, MISC


# Umgehen eines lästigen ENAMETOOLONG Errors bei Benutzung von langen
# Filenamen auf FAT-Drives unter OS/2:
#ifdef EMUNIX_PORTABEL
  #define chdir  my_chdir
  #define access  my_access
  #define stat(x,y)  my_stat(x,y)
  #define unlink  my_unlink
  #define rename  my_rename
  #define __findfirst  my___findfirst
  #undef mkdir
  #define mkdir  my_mkdir
  #define open(x,y)  my_open(x,y)
  #undef creat
  #define creat  my_creat
  #define spawnv  my_spawnv
  extern int chdir (CONST char* path); # siehe CHDIR(2V)
  extern int access (CONST char* path, int amode);
  extern int stat (CONST char* path, struct stat * buf); # siehe STAT(2V)
  extern int unlink (CONST char* path); # siehe UNLINK(2V)
  extern int rename (CONST char* oldpath, CONST char* newpath); # siehe RENAME(2V)
  extern int __findfirst (const char* pathname, int attrib, struct ffblk * ffblk);
  extern int mkdir (CONST char* path, long attrib);
  extern int open (CONST char* path, int flags); # siehe OPEN(2V)
  extern int creat (CONST char* path, int pmode);
  extern int spawnv (int pmode, CONST char* path, CONST char* CONST argv[]);
#endif


#ifdef DJUNIX
# good_drive, default_drive, default_directory_of austesten : pathname.
#endif
#ifdef EMUNIX
# In der OS/2 - Version kann man folgendes verwenden:
# - _sleep2()
# - Kommando-Interpreter: cmd.exe, nicht command.com .
#endif
# File-Funktionen austesten!!
