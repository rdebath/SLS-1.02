# Include-File für UNIX-Version von CLISP
# Bruno Haible 18.3.1993


# Konstanten für Steuerzeichen:

#define BEL  7              # Ton ausgeben
# define NL  10             # New line, siehe LISPBIBL.D
#define RUBOUT 127          # Rubout = Delete
#define CRLFstring  "\n"    # C-String, der BS-Newline enthält

#define stdin_handle   0  # File-Handle von Standard-Input
#define stdout_handle  1  # File-Handle von Standard-Output

# Deklaration von Typen von Ein-/Ausgabe-Parametern von Betriebssystemfunktionen
  #ifdef STDC_HEADERS
    #include <stdlib.h>
  #endif
  #ifdef HAVE_UNISTD_H
    #include <sys/types.h>
    #include <unistd.h>
  #endif

# Tabelle der System-Fehlermeldungen
  #include <errno.h>
  extern int errno; # letzter Fehlercode
  extern int sys_nerr; # Anzahl der Betriebssystem-Fehlermeldungen
  extern SYS_ERRLIST_CONST char* SYS_ERRLIST_CONST sys_errlist[]; # Betriebssystem-Fehlermeldungen
  # siehe PERROR(3)
# wird verwendet von MISC, SPVW, STREAM, PATHNAME

# Bereitstellen des Arbeitsspeichers
  #ifdef HAVE_GETPAGESIZE
    extern RETGETPAGESIZETYPE getpagesize (void); # siehe GETPAGESIZE(2)
  #endif
  #ifndef malloc
    extern RETMALLOCTYPE malloc (unsigned int size); # siehe MALLOC(3V)
  #endif
  #ifndef free
    extern RETFREETYPE free (RETMALLOCTYPE ptr); # siehe MALLOC(3V)
  #endif
  #ifdef HAVE_MMAP
    #include <sys/types.h>
    #include <sys/mman.h>
    extern caddr_t mmap (caddr_t addr, MMAP_SIZE_T len, int prot, int flags, int fd, off_t off);
    #ifdef UNIX_SUNOS4
      #include <sys/vfs.h>
      extern int fstatfs (int fd, struct statfs * buf); # siehe STATFS(2)
    #endif
  #endif
  #ifdef HAVE_SHM
    #include <sys/types.h>
    #include <sys/ipc.h>
    #include <sys/shm.h>
    #ifdef HAVE_SYS_SYSMACROS_H
      #include <sys/sysmacros.h>
    #endif
    #ifdef UNIX_HPUX
      #include <sys/vmmac.h> # für SHMLBA
    #endif
    #ifdef UNIX_SUNOS4
      #define SHMMAX  0x100000 # maximale Shared-Memory-Segment-Größe = 1 MB
    #endif
    #ifndef SHMMAX
      #define SHMMAX  0xFFFFFFFFUL # maximale Shared-Memory-Semgment-Größe wird als unendlich angenommen
    #endif
    extern int shmget (key_t key, int size, int shmflg); # siehe SHMGET(2)
    extern RETSHMATTYPE shmat (int shmid, SHMAT_CONST RETSHMATTYPE shmaddr, int shmflg); # siehe SHMOP(2)
    #ifdef SHMCTL_DOTS
      extern int shmctl (int shmid, int cmd, ...); # siehe SHMCTL(2)
    #else
      extern int shmctl (int shmid, int cmd, struct shmid_ds * buf); # siehe SHMCTL(2)
    #endif
  #endif
# wird verwendet von SPVW, STREAM

# Steuerung der Pagingverhaltens
  #ifdef HAVE_VADVISE
    #include <sys/vadvise.h> # Steuercodes
    extern void vadvise (int param); # Paging-System steuern # siehe VADVISE(2)
  #endif
# wird verwendet von SPVW

# Normales Programmende
  extern nonreturning void _exit (int status); # siehe EXIT(2V)
# wird verwendet von SPVW, PATHNAME, STREAM

# Sofortiger Programmabbruch, Sprung in den Debugger
  extern RETABORTTYPE abort (void); # siehe ABORT(3)
# wird verwendet von SPVW, DEBUG, EVAL, IO

# Signalbehandlung
  #include <signal.h>
  # Ein Signal-Handler ist eine Funktion ohne Ergebnis.
  typedef RETSIGTYPE (*signal_handler) ();
  extern signal_handler signal (int sig, signal_handler handler); # siehe SIGNAL(3V)
  # Ein Signal blockieren und wieder freigeben:
  #if defined(SIGNALBLOCK_POSIX)
    extern int sigprocmask (int how, SIGPROCMASK_CONST sigset_t* set, sigset_t* oset); # siehe SIGPROCMASK(2V)
    #ifndef sigemptyset # UNIX_LINUX definiert dies manchmal als Macro
      extern int sigemptyset (sigset_t* set); # siehe SIGSETOPS(3V)
    #endif
    #ifndef sigaddset # UNIX_LINUX definiert dies manchmal als Macro
      extern int sigaddset (sigset_t* set, int signo); # siehe SIGSETOPS(3V)
    #endif
    #define signalblock_on(sig)  \
      { var sigset_t sigblock_mask;                                 \
        sigemptyset(&sigblock_mask); sigaddset(&sigblock_mask,sig); \
        sigprocmask(SIG_BLOCK,&sigblock_mask,NULL);
    #define signalblock_off(sig)  \
        sigprocmask(SIG_UNBLOCK,&sigblock_mask,NULL); \
      }
  #elif defined(SIGNALBLOCK_SYSV)
    extern int sighold (int sig);
    extern int sigrelse (int sig);
    #define signalblock_on(sig)  sighold(sig);
    #define signalblock_off(sig)  sigrelse(sig);
  #elif defined(SIGNALBLOCK_BSD)
    extern int sigblock (int mask); # siehe SIGBLOCK(2)
    extern int sigsetmask (int mask); # siehe SIGSETMASK(2)
    #define signalblock_on(sig)  \
      { var int old_sigblock_mask = sigblock(sigmask(sig));
    #define signalblock_off(sig)  \
        sigsetmask(old_sigblock_mask); \
      }
  #else
    #error "Wie blockiert man Signale?"
  #endif
  # Ein Signal erst eine bestimmte Zeit später ausliefern:
  # extern {unsigned|} int alarm ({unsigned|} int seconds); # siehe ALARM(3V)
  #if !defined(HAVE_UALARM) && defined(HAVE_SETITIMER)
    #define NEED_OWN_UALARM # mit setitimer() kann man ualarm() selber schreiben
    #include <sys/time.h>
    extern int setitimer (int which, SETITIMER_CONST struct itimerval * ivalue, struct itimerval * ovalue); # siehe SETITIMER(2)
    #define HAVE_UALARM
  #endif
  #ifdef HAVE_UALARM
    extern unsigned int ualarm (unsigned int value, unsigned int interval); # siehe UALARM(3)
  #endif
  # Die Ankunft eines Signals quittieren (aus dem Signal-Handler heraus):
  #ifdef SIGNAL_NEED_REINSTALL # UNIX_SYSV || UNIX_LINUX || ...
    #define signal_acknowledge(sig,handler)  signal(sig,handler) # Handler bleibt weiter aktiv
  #else # Signalverwaltung nach BSD hat das nicht nötig
    #define signal_acknowledge(sig,handler)
  #endif
  # Das Signal, das man bekommt, wenn ein Tochterprozeß beendet wird: SIGCLD
  #if defined(SIGCHLD) && !defined(SIGCLD)
    #define SIGCLD  SIGCHLD
  #endif
# wird verwendet von SPVW

# Environment-Variablen abfragen:
  extern char* getenv (GETENV_CONST char* name); # siehe GETENV(3V)
# wird verwendet von PATHNAME, SPVW, MISC

# Home-Directory eines Benutzers holen:
  #include <pwd.h>
  extern struct passwd * getpwnam (GETPWNAM_CONST char* name); # siehe GETPWENT(3V)
  #if defined(UNIX_SUNOS4)
    extern struct passwd * getpwuid (int /* really uid_t */ uid); # siehe GETPWENT(3V)
  #else
    extern struct passwd * getpwuid (uid_t uid); # siehe GETPWENT(3V)
  #endif
  extern uid_t getuid (void); # siehe GETUID(2V)
  extern char* getlogin (void); # siehe GETLOGIN(3V)
# wird verwendet von PATHNAME

# Working Directory setzen:
  extern int chdir (CHDIR_CONST char* path); # siehe CHDIR(2V)
# wird verwendet von PATHNAME

# Working Directory abfragen:
  #include <sys/param.h>
  # Maximale Pfadlänge (incl. Nullbyte am Schluß), die von getwd geliefert wird:
  #ifndef MAXPATHLEN
    #define MAXPATHLEN  1024  # siehe <sys/param.h>
  #endif
  #ifndef HAVE_GETWD
    extern char* getcwd (char* buf, GETCWD_SIZE_T bufsize);
    #define getwd(buf)  getcwd(buf,MAXPATHLEN)
  #else
    extern char* getwd (char* pathname); # siehe GETWD(3)
  #endif
# wird verwendet von PATHNAME

# Maximalzahl symbolischer Links, die nacheinander aufgelöst werden:
  #ifndef MAXSYMLINKS
    #define MAXSYMLINKS  8  # siehe <sys/param.h>
  #endif
# wird verwendet von PATHNAME

# Auflösen symbolischer Links in Pfadnamen:
  extern int readlink (READLINK_CONST char* path, READLINK_BUF_T buf, READLINK_SIZE_T bufsiz); # siehe READLINK(2)
# wird verwendet von PATHNAME

# Information zu einem File erfragen:
  #include <sys/types.h>
  #include <sys/stat.h>
  extern int stat (STAT_CONST char* path, struct stat * buf); # siehe STAT(2V)
  extern int lstat (LSTAT_CONST char* path, struct stat * buf); # siehe STAT(2V)
  extern int fstat (int fd, struct stat * buf); # siehe STAT(2V)
  #ifndef S_ISDIR
    #define S_ISDIR(m)  (((m)&S_IFMT) == S_IFDIR)
  #endif
  #ifndef S_ISLNK
    #define S_ISLNK(m)  (((m)&S_IFMT) == S_IFLNK)
  #endif
  #ifndef S_ISREG
    #define S_ISREG(m)  (((m)&S_IFMT) == S_IFREG)
  #endif
# wird verwendet von PATHNAME, STREAM

# File löschen:
  extern int unlink (UNLINK_CONST char* path); # siehe UNLINK(2V)
# wird verwendet von PATHNAME

# File umbenennen:
  extern int rename (RENAME_CONST char* oldpath, RENAME_CONST char* newpath); # siehe RENAME(2V)
  # Alternative: link(oldpath,newpath), dann unlink(oldpath) ??
# wird verwendet von PATHNAME

# Directory-Suche:
  #if defined(DIRENT) || defined(_POSIX_VERSION)
    #include <dirent.h>
    #define SDIRENT  struct dirent
  #else
    #if defined(UNIX_SYSV)
      #ifdef SYSNDIR
        #include <sys/ndir.h>
      #else
        #include <ndir.h>
      #endif
    #else
      #ifdef SYSDIR
        #include <sys/dir.h>
      #else
        #include <dir.h>
      #endif
    #endif
    #define SDIRENT  struct direct
  #endif
  extern DIR* opendir (OPENDIR_CONST char* dirname); # siehe DIRECTORY(3V)
  extern SDIRENT* readdir (DIR* dirp); # siehe DIRECTORY(3V)
  #ifdef VOID_CLOSEDIR
    extern void closedir (DIR* dirp); # siehe DIRECTORY(3V)
    #define CLOSEDIR(dirp)  (closedir(dirp),0)
  #else
    extern int closedir (DIR* dirp); # siehe DIRECTORY(3V)
    #define CLOSEDIR  closedir
  #endif
# wird verwendet von PATHNAME

# Directory anlegen:
  extern int mkdir (MKDIR_CONST char* path, MODE_T mode); # siehe MKDIR(2V)
# wird verwendet von PATHNAME

# Directory löschen:
  extern int rmdir (RMDIR_CONST char* path); # siehe RMDIR(2V)
# wird verwendet von PATHNAME

# Arbeiten mit offenen Files:
  #include <sys/types.h>
  # include <unistd.h> # siehe oben
  #include <fcntl.h>
  #ifdef NEED_SYS_FILE_H
    #include <sys/file.h>
  #endif
  #ifdef OPEN_DOTS
    extern int open (OPEN_CONST char* path, int flags, ...); # siehe OPEN(2V)
  #else
    extern int open (OPEN_CONST char* path, int flags, MODE_T mode); # siehe OPEN(2V)
  #endif
  #define my_open_mask  0644
  #define Handle  uintW  # Typ eines File-Deskriptors
  extern off_t lseek (int fd, off_t offset, int whence); # siehe LSEEK(2V)
  extern RETRWTYPE read (int fd, RW_BUF_T buf, RW_SIZE_T nbyte); # siehe READ(2V)
  extern RETRWTYPE write (int fd, WRITE_CONST RW_BUF_T buf, RW_SIZE_T nbyte); # siehe WRITE(2V)
  extern int close (int fd); # siehe CLOSE(2V)
  #ifdef HAVE_FSYNC
    extern int fsync (int fd); # siehe FSYNC(2)
  #endif
  #ifdef HAVE_SELECT
    #include <sys/time.h>
    #ifndef FD_SETSIZE
      # Definition des Typs fd_set, vgl. <sys/types.h> :
      #ifdef UNIX_HPUX # dort ist fd_set bereits definiert, aber FD_SETSIZE nicht
        #define fd_set  my_fd_set
      #endif
      #define FD_SETSIZE  256  # Maximalzahl von File-Deskriptoren
      typedef int  fd_mask;  # eine Bitgruppe
      #define NFDBITS  (sizeof(fd_mask) * 8)  # Anzahl Bits in einer Bitgruppe
      typedef struct fd_set { fd_mask fds_bits[ceiling(FD_SETSIZE,NFDBITS)]; }
              fd_set;
      #define FD_SET(n,p)  ((p)->fds_bits[(n)/NFDBITS] |= bit((n)%NFDBITS))
      #define FD_CLR(n,p)  ((p)->fds_bits[(n)/NFDBITS] &= ~bit((n)%NFDBITS))
      #define FD_ISSET(n,p)  ((p)->fds_bits[(n)/NFDBITS] & bit((n)%NFDBITS))
      #define FD_ZERO(p)  bzero((char*)(p),sizeof(*(p)))
      #ifdef USG
        #include <string.h>
        extern void* memset (void* ptr, int c, size_t len); # siehe MEMORY(3)
        #define bzero(ptr,len)  memset(ptr,0,len)
      #else
        extern void bzero (void* ptr, int len); # siehe BZERO(3)
      #endif
    #endif
    extern int select (SELECT_WIDTH_T width, SELECT_SET_T* readfds,
                       SELECT_SET_T* writefds, SELECT_SET_T* exceptfds,
                       SELECT_CONST struct timeval * timeout); # siehe SELECT(2)
  #endif
# wird verwendet von STREAM, PATHNAME, SPVW, MISC

# Terminal-Abfragen, Abfragen der Fenster-Größe:
  extern int isatty (int fd); # siehe TTYNAME(3V)
  extern char* ttyname (int fd); # siehe TTYNAME(3V)
  #ifdef IOCTL_DOTS
    extern int ioctl (int fd, IOCTL_REQUEST_T request, ...); # siehe IOCTL(2)
  #else
    extern int ioctl (int fd, IOCTL_REQUEST_T request, caddr_t arg); # siehe IOCTL(2)
    #ifdef ANSI
      # 3. Argument stets zum Typ caddr_t casten:
      #define ioctl(fd,request,arg)  (ioctl)(fd,request,(caddr_t)(arg))
    #endif
  #endif
  #if defined(HAVE_TERMIOS_H) && defined(HAVE_TCSAFLUSH)
    #define UNIX_TERM_TERMIOS
    #include <termios.h> # siehe TERMIOS(3V)
    extern int tcdrain (int fd); # siehe TERMIOS(3V)
    extern int tcflush (int fd, int flag); # siehe TERMIOS(3V)
    #ifndef NCCS
      #define NCCS  sizeof(((struct termios *)0)->c_cc)
    #endif
  #elif defined(HAVE_SYS_TERMIO_H) || defined(HAVE_TERMIO_H)
    #define UNIX_TERM_TERMIO
    #if defined(HAVE_SYS_TERMIO_H)
      #include <sys/termio.h> # siehe TERMIO(4)
    #elif defined(HAVE_TERMIO_H)
      #include <termio.h>
    #endif
    #ifndef NCCS
      #define NCCS  sizeof(((struct termio *)0)->c_cc)
    #endif
  #elif defined(HAVE_SGTTY_H)
    # kompatibel zu V7 oder 4BSD, ioctls der Form TIOC....
    #define UNIX_TERM_SGTTY
    #include <sgtty.h>
    #include <sys/ioctl.h> # siehe TTY(4)
  #endif
  #if defined(NEED_SYS_FILIO_H)
    #include <sys/filio.h>
  #elif defined(NEED_SYS_IOCTL_H)
    #include <sys/ioctl.h>
  #endif
  #if (defined(UNIX_TERM_TERMIOS) || defined(UNIX_TERM_TERMIO)) && !(defined(TCIFLUSH) && defined(TCOFLUSH))
    #define TCIFLUSH 0
    #define TCOFLUSH 1
  #endif
  extern int tgetent (char* bp, char* name); # siehe TERMCAP(3X)
  extern int tgetnum (char* id); # siehe TERMCAP(3X)
  extern int tgetflag (char* id); # siehe TERMCAP(3X)
  extern char* tgetstr (char* id, char** area); # siehe TERMCAP(3X)
# wird verwendet von SPVW, STREAM

# Datum/Uhrzeit verarbeiten:
  #ifdef TM_IN_SYS_TIME
    #include <sys/time.h>
  #else
    #include <time.h>
  #endif
  extern struct tm * localtime (LOCALTIME_CONST time_t* clock); # siehe CTIME(3V)
# wird verwendet von SPVW

# Datum/Uhrzeit abfragen:
  #include <sys/time.h>
  extern int gettimeofday (struct timeval * tp, struct timezone * tzp); # siehe GETTIMEOFDAY(2)
  # Alternative:
  # #include <sys/timeb.h>
  # extern int ftime (struct timeb * tp); # siehe TIME(3V)
# wird verwendet von SPVW, MISC

# vom Prozeß verbrauchte Zeit erfragen:
  #if defined(HAVE_GETRUSAGE)
    #include <sys/time.h>
    #include <sys/resource.h>
    extern int getrusage (int who, struct rusage * rusage); # siehe GETRUSAGE(2)
    # Prototyp wertlos, da 'struct rusage' /= 'struct rusage' - verkorxtes ANSI!
  #elif defined(HAVE_SYS_TIMES_H)
    #include <sys/types.h>
    #include <sys/param.h> # definiert HZ, Maßeinheit ist 1/HZ Sekunden
    #include <sys/times.h>
    extern clock_t times (struct tms * buffer); # siehe TIMES(3V)
  #endif
  # Alternative:
  # #include <??>
  # extern ?? vtimes (struct vtimes * par_vm, struct vtimes * ch_vm); # siehe VTIMES(3C)
# wird verwendet von SPVW

# Eine bestimmte Zeit Pause machen:
  extern unsigned int sleep (unsigned int seconds); # siehe SLEEP(3V)
  #ifdef HAVE_USLEEP
    # extern {int|void} usleep (unsigned int useconds); # siehe USLEEP(3)
  #endif
# wird verwendet von MISC

# Programme aufrufen:
  extern int pipe (int fd[2]); # siehe PIPE(2V)
  #ifdef HAVE_VFORK_H
    #include <vfork.h>
  #endif
  extern RETVFORKTYPE vfork (void); # siehe VFORK(2)
  extern int dup2 (int fd1, int fd2); # siehe DUP(2V)
  extern int execv (EXECV_CONST char* path, EXECV1_CONST char* EXECV2_CONST argv[]); # siehe EXECL(3V)
  #ifdef EXECL_DOTS
    extern int execl (EXECV_CONST char* path, EXECL_CONST char* arg, ...); # siehe EXECL(3V)
  #else
    extern int execl (EXECV_CONST char* path, EXECL_CONST char* arg0, EXECL_CONST char* arg1, EXECL_CONST char* arg2, EXECL_CONST char* arg3); # siehe EXECL(3V)
  #endif
  #ifdef HAVE_WAITPID
    #include <sys/wait.h>
    extern pid_t waitpid (PID_T pid, int* statusp, int options); # siehe WAIT(2V)
  #else
    #include <sys/wait.h>
    extern int wait (int* statusp); # siehe WAIT(2V)
  #endif
# wird verwendet von STREAM, PATHNAME

# Zufallszahlen besorgen:
  #ifndef rand # Manche definieren rand() als Macro...
    extern int rand (void); # siehe RAND(3V)
  #endif
  extern pid_t getpid (void); # siehe GETPID(2V)
# wird verwendet von LISPARIT

# MACHINE-TYPE und MACHINE-VERSION und evtl. MACHINE-INSTANCE bestimmen:
  #ifdef HAVE_SYS_UTSNAME_H
    #include <sys/utsname.h>
    extern int uname (struct utsname * buf); # siehe UNAME(2V)
  #endif
# wird verwendet von MISC

# MACHINE-INSTANCE bestimmen:
  #ifdef HAVE_GETHOSTNAME
    extern int gethostname (char* name, GETHOSTNAME_SIZE_T namelen); # siehe GETHOSTNAME(2)
  #endif
  #ifdef HAVE_GETHOSTBYNAME
    #include <sys/types.h>
    #ifdef HAVE_NETDB_H
      #include <sys/socket.h>
      #include <netdb.h>
    #else
      #include <sun/netdb.h>
    #endif
    extern struct hostent * gethostbyname (GETHOSTBYNAME_CONST char* name); # siehe GETHOSTENT(3)
  #endif
  #ifndef MAXHOSTNAMELEN
    #define MAXHOSTNAMELEN 64 # siehe <sys/param.h>
  #endif
# wird verwendet von MISC

