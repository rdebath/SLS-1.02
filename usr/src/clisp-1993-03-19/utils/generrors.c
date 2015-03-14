/* Hilfsprogramm zum Erstellen der Fehlermeldungs-Tabelle ERRORS.C
   für CLISP unter UNIX.
   Bruno Haible 6.3.1993

   Problem: viele verschiedene UNIX-Versionen, jede wieder mit anderen
   Fehlermeldungen.
   Abhilfe: Die Fehlernamen sind einigermaßen portabel. Die englische
   Fehlermeldung übernehmen wir, die Übersetzungen machen wir selbst.
*/

#if defined(unix) || defined(__unix) || defined(vms)
  #include "unixconf.h"
  #if defined(sun) && (defined(sun386) || defined(sparc)) && defined(HAVE_VADVISE)
    #define UNIX_SUNOS4  /* SUN OS Version 4 */
  #endif
  #if defined(SIGNALBLOCK_BSD)
    #define UNIX_BSD
  #endif
#endif
#ifdef __TURBOC__
  #define STDC_HEADERS 1
#endif
#ifdef STDC_HEADERS
  #include <stdlib.h> /* definiert MALLOC(3V) */
#endif

#include <stdio.h> /* definiert PRINTF(3) */

/* Tabelle der System-Fehlermeldungen, siehe PERROR(3) */
#include <errno.h>
#if !(defined(linux) || defined(vms))
extern int sys_nerr; /* Anzahl der Betriebssystem-Fehlermeldungen */
extern char* sys_errlist[]; /* Betriebssystem-Fehlermeldungen */
#endif

typedef struct { char* name; /* Name des Errors */
                 char* english; /* Fehlermeldung auf ENGLISH */
                 char* deutsch; /* Fehlermeldung auf DEUTSCH */
               }
        error;

int main ()
{ int anzahl = sys_nerr; /* Gesamtzahl von Fehlermeldungen */
  error* tabelle = (error*)malloc(anzahl*sizeof(error)); /* Tabelle von anzahl Meldungen */
  int i;
  /* Tabelle vor-initialisieren: */
  for (i=0; i<anzahl; i++)
    { tabelle[i].name = "";
      #ifndef vms
      tabelle[i].english = sys_errlist[i];
      #else /* vms: Zugriff auf sys_errlist[i] funktioniert für i>=36 nicht. */
      tabelle[i].english = "";
      #endif
      tabelle[i].deutsch = "";
    }
  /* Übersetzungen der Meldungen eintragen: */
  for (i=0; i<anzahl; i++)
    { char* name = NULL;
      char* english = NULL;
      char* deutsch = NULL;
      /* allgemein verbreitete UNIX-Errors: */
      #ifdef EPERM
      if (i == EPERM) { name = "EPERM";
        english = "Not owner";
        deutsch = "Keine Berechtigung dazu";
        } else
      #endif
      #ifdef ENOENT
      if (i == ENOENT) { name = "ENOENT";
        english = "No such file or directory";
        deutsch = "File oder Directory existiert nicht";
        } else
      #endif
      #ifdef ESRCH
      if (i == ESRCH) { name = "ESRCH";
        english = "No such process";
        deutsch = "Dieser Prozeß existiert nicht (mehr)";
        } else
      #endif
      #ifdef EINTR
      if (i == EINTR) { name = "EINTR";
        english = "Interrupted system call";
        deutsch = "Unterbrechung während Betriebssystem-Aufruf";
        } else
      #endif
      #ifdef EIO
      if (i == EIO) { name = "EIO";
        english = "I/O error";
        deutsch = "Fehler bei Schreib-/Lesezugriff";
        } else
      #endif
      #ifdef ENXIO
      if (i == ENXIO) { name = "ENXIO";
        english = "No such device or address";
        deutsch = "Gerät existiert nicht oder Laufwerk leer";
        } else
      #endif
      #ifdef E2BIG
      if (i == E2BIG) { name = "E2BIG";
        english = "Arg list too long";
        deutsch = "Zu lange Argumentliste";
        } else
      #endif
      #ifdef ENOEXEC
      if (i == ENOEXEC) { name = "ENOEXEC";
        english = "Exec format error";
        deutsch = "Kein ausführbares Programm";
        } else
      #endif
      #ifdef EBADF
      if (i == EBADF) { name = "EBADF";
        english = "Bad file number";
        deutsch = "File-Descriptor wurde nicht für diese Operation geöffnet";
        } else
      #endif
      #ifdef ECHILD
      if (i == ECHILD) { name = "ECHILD";
        english = "No child processes";
        deutsch = "Worauf warten?";
        } else
      #endif
      #ifdef EAGAIN
      if (i == EAGAIN) { name = "EAGAIN";
        english = "No more processes";
        deutsch = "Kann keinen weiteren Prozeß erzeugen";
        } else
      #endif
      #ifdef ENOMEM
      if (i == ENOMEM) { name = "ENOMEM";
        english = "Not enough memory";
        #if !defined(UNIX_SUNOS4)
        deutsch = "Hauptspeicher oder Swapspace reicht nicht";
        #else
        deutsch = "Speicher-Adreßbereich oder Swapspace reicht nicht";
        #endif
        } else
      #endif
      #ifdef EACCES
      if (i == EACCES) { name = "EACCES";
        english = "Permission denied";
        deutsch = "Keine Berechtigung";
        } else
      #endif
      #ifdef EFAULT
      if (i == EFAULT) { name = "EFAULT";
        english = "Bad address";
        deutsch = "Ungültige Adresse";
        } else
      #endif
      #ifdef ENOTBLK
      if (i == ENOTBLK) { name = "ENOTBLK";
        english = "Block device required";
        deutsch = "Nur block-strukturierte Geräte erlaubt";
        } else
      #endif
      #ifdef EBUSY
      if (i == EBUSY) { name = "EBUSY";
        #if !defined(UNIX_SUNOS4)
        english = "Mount device busy";
        deutsch = "Gerät enthält Einheit und darf sie nicht auswerfen";
        #else
        english = "Device busy";
        deutsch = "Filesystem darf nicht gekappt werden";
        #endif
        } else
      #endif
      #ifdef EEXIST
      if (i == EEXIST) { name = "EEXIST";
        english = "File exists";
        deutsch = "File existiert schon";
        } else
      #endif
      #ifdef EXDEV
      if (i == EXDEV) { name = "EXDEV";
        english = "Cross-device link";
        deutsch = "Links können nur aufs selbe Gerät gehen";
        } else
      #endif
      #ifdef ENODEV
      if (i == ENODEV) { name = "ENODEV";
        english = "No such device";
        deutsch = "Gerät nicht da oder unpassend";
        } else
      #endif
      #ifdef ENOTDIR
      if (i == ENOTDIR) { name = "ENOTDIR";
        english = "Not a directory";
        deutsch = "Das ist kein Directory";
        } else
      #endif
      #ifdef EISDIR
      if (i == EISDIR) { name = "EISDIR";
        english = "Is a directory";
        deutsch = "Das ist ein Directory";
        } else
      #endif
      #ifdef EINVAL
      if (i == EINVAL) { name = "EINVAL";
        english = "Invalid argument";
        deutsch = "Ungültiger Parameter";
        } else
      #endif
      #ifdef ENFILE
      if (i == ENFILE) { name = "ENFILE";
        english = "File table overflow";
        deutsch = "Tabelle der offenen Files ist voll";
        } else
      #endif
      #ifdef EMFILE
      if (i == EMFILE) { name = "EMFILE";
        english = "Too many open files";
        deutsch = "Zu viele offene Files";
        } else
      #endif
      #ifdef ENOTTY
      if (i == ENOTTY) { name = "ENOTTY";
        english = "Inappropriate ioctl for device";
        deutsch = "Falscher Gerätetyp";
        } else
      #endif
      #ifdef ETXTBSY
      if (i == ETXTBSY) { name = "ETXTBSY";
        english = "Text file busy";
        deutsch = "Programm wird gerade geändert oder ausgeführt";
        } else
      #endif
      #ifdef EFBIG
      if (i == EFBIG) { name = "EFBIG";
        english = "File too large";
        deutsch = "Zu großes File";
        } else
      #endif
      #ifdef ENOSPC
      if (i == ENOSPC) { name = "ENOSPC";
        english = "No space left on device";
        deutsch = "Platte oder Diskette voll";
        } else
      #endif
      #ifdef ESPIPE
      if (i == ESPIPE) { name = "ESPIPE";
        english = "Illegal seek";
        deutsch = "Nicht positionierbares File";
        } else
      #endif
      #ifdef EROFS
      if (i == EROFS) { name = "EROFS";
        english = "Read-only file system";
        deutsch = "Dieses Filesystem erlaubt keinen Schreibzugriff";
        } else
      #endif
      #ifdef EMLINK
      if (i == EMLINK) { name = "EMLINK";
        english = "Too many links";
        deutsch = "Zu viele Links auf ein File";
        } else
      #endif
      #ifdef EPIPE
      if (i == EPIPE) { name = "EPIPE";
        english = "Broken pipe";
        deutsch = "Output versackt";
        } else
      #endif
      /* Errors bei mathematischen Funktionen: */
      #ifdef EDOM
      if (i == EDOM) { name = "EDOM";
        english = "Argument out of domain";
        deutsch = "Argument zu mathematischer Funktion außerhalb des Definitionsbereichs";
        } else
      #endif
      #ifdef ERANGE
      if (i == ERANGE) { name = "ERANGE";
        english = "Result too large";
        deutsch = "Ergebnis mathematischer Funktion zu groß";
        } else
      #endif
      /* Errors bei Non-Blocking I/O und Interrupt I/O: */
      #ifdef EWOULDBLOCK
      if (i == EWOULDBLOCK) { name = "EWOULDBLOCK";
        english = "Operation would block";
        deutsch = "Darauf müßte gewartet werden";
        } else
      #endif
      #ifdef EINPROGRESS
      if (i == EINPROGRESS) { name = "EINPROGRESS";
        english = "Operation now in progress";
        deutsch = "Das kann lange dauern";
        } else
      #endif
      #ifdef EALREADY
      if (i == EALREADY) { name = "EALREADY";
        english = "Operation already in progress";
        deutsch = "Es läuft schon eine Operation";
        } else
      #endif
      /* weitere allgemein übliche Errors: */
      #ifdef ELOOP
      if (i == ELOOP) { name = "ELOOP";
        english = "Too many levels of symbolic links";
        deutsch = "Zu viele symbolische Links in einem Pathname";
        } else
      #endif
      #ifdef ENAMETOOLONG
      if (i == ENAMETOOLONG) { name = "ENAMETOOLONG";
        english = "File name too long";
        deutsch = "Zu langer Filename";
        } else
      #endif
      #ifdef ENOTEMPTY
      if (i == ENOTEMPTY) { name = "ENOTEMPTY";
        english = "Directory not empty";
        deutsch = "Directory ist nicht leer";
        } else
      #endif
      /* Errors im Zusammenhang mit Network File System (NFS): */
      #ifdef ESTALE
      if (i == ESTALE) { name = "ESTALE";
        english = "Stale NFS file handle";
        deutsch = "Offenes File auf entferntem Filesystem wurde gelöscht";
        } else
      #endif
      #ifdef EREMOTE
      if (i == EREMOTE) { name = "EREMOTE";
        english = "Too many levels of remote in path";
        deutsch = "Mount läuft nicht auf entfernten Filesystemen";
        } else
      #endif
      /* Errors im Zusammenhang mit Sockets, IPC und Netzwerk: */
      #ifdef ENOTSOCK
      if (i == ENOTSOCK) { name = "ENOTSOCK";
        english = "Socket operation on non-socket";
        deutsch = "Socket-Operation und kein Socket";
        } else
      #endif
      #ifdef EDESTADDRREQ
      if (i == EDESTADDRREQ) { name = "EDESTADDRREQ";
        english = "Destination address required";
        deutsch = "Operation braucht Zieladresse";
        } else
      #endif
      #ifdef EMSGSIZE
      if (i == EMSGSIZE) { name = "EMSGSIZE";
        english = "Message too long";
        deutsch = "Zu lange Nachricht";
        } else
      #endif
      #ifdef EPROTOTYPE
      if (i == EPROTOTYPE) { name = "EPROTOTYPE";
        english = "Protocol wrong type for socket";
        deutsch = "Dieses Protokoll paßt nicht zu diesem Socket";
        } else
      #endif
      #ifdef ENOPROTOOPT
      if (i == ENOPROTOOPT) { name = "ENOPROTOOPT";
        #if defined(UNIX_SUNOS4)
        english = "Option not supported by protocol";
        #else
        #if defined(UNIX_BSD)
        english = "Bad protocol option";
        #else /* UNIX_HPUX, VMS */
        english = "Protocol not available";
        #endif
        #endif
        deutsch = "Fehlerhafte Option zu Protokoll auf Socket";
        } else
      #endif
      #ifdef EPROTONOSUPPORT
      if (i == EPROTONOSUPPORT) { name = "EPROTONOSUPPORT";
        english = "Protocol not supported";
        deutsch = "Protokoll nicht implementiert";
        } else
      #endif
      #ifdef ESOCKTNOSUPPORT
      if (i == ESOCKTNOSUPPORT) { name = "ESOCKTNOSUPPORT";
        english = "Socket type not supported";
        deutsch = "Socket-Typ nicht implementiert";
        } else
      #endif
      #ifdef EOPNOTSUPP
      if (i == EOPNOTSUPP) { name = "EOPNOTSUPP";
        english = "Operation not supported on socket";
        deutsch = "Operation auf diesem Socket nicht implementiert";
        } else
      #endif
      #ifdef EPFNOSUPPORT
      if (i == EPFNOSUPPORT) { name = "EPFNOSUPPORT";
        english = "Protocol family not supported";
        deutsch = "Protokoll-Familie nicht implementiert";
        } else
      #endif
      #ifdef EAFNOSUPPORT
      if (i == EAFNOSUPPORT) { name = "EAFNOSUPPORT";
        english = "Address family not supported by protocol family";
        deutsch = "Adressen-Familie paßt nicht zu diesem Protokoll";
        } else
      #endif
      #ifdef EADDRINUSE
      if (i == EADDRINUSE) { name = "EADDRINUSE";
        english = "Address already in use";
        deutsch = "Adresse schon belegt";
        } else
      #endif
      #ifdef EADDRNOTAVAIL
      if (i == EADDRNOTAVAIL) { name = "EADDRNOTAVAIL";
        english = "Can't assign requested address";
        deutsch = "Adresse nicht (auf diesem Rechner) verfügbar";
        } else
      #endif
      #ifdef ENETDOWN
      if (i == ENETDOWN) { name = "ENETDOWN";
        english = "Network is down";
        deutsch = "Netz streikt";
        } else
      #endif
      #ifdef ENETUNREACH
      if (i == ENETUNREACH) { name = "ENETUNREACH";
        english = "Network is unreachable";
        deutsch = "Netz unbekannt und außer Sichtweite";
        } else
      #endif
      #ifdef ENETRESET
      if (i == ENETRESET) { name = "ENETRESET";
        english = "Network dropped connection on reset";
        deutsch = "Rechner bootete, Verbindung gekappt";
        } else
      #endif
      #ifdef ECONNABORTED
      if (i == ECONNABORTED) { name = "ECONNABORTED";
        english = "Software caused connection abort";
        deutsch = "Mußte diese Verbindung kappen";
        } else
      #endif
      #ifdef ECONNRESET
      if (i == ECONNRESET) { name = "ECONNRESET";
        english = "Connection reset by peer";
        deutsch = "Gegenseite kappte die Verbindung";
        } else
      #endif
      #ifdef ENOBUFS
      if (i == ENOBUFS) { name = "ENOBUFS";
        english = "No buffer space available";
        deutsch = "Nicht genügend Platz für einen Buffer";
        } else
      #endif
      #ifdef EISCONN
      if (i == EISCONN) { name = "EISCONN";
        english = "Socket is already connected";
        deutsch = "Socket ist bereits verbunden";
        } else
      #endif
      #ifdef ENOTCONN
      if (i == ENOTCONN) { name = "ENOTCONN";
        english = "Socket is not connected";
        deutsch = "Socket hat keine Verbindung";
        } else
      #endif
      #ifdef ESHUTDOWN
      if (i == ESHUTDOWN) { name = "ESHUTDOWN";
        english = "Can't send after socket shutdown";
        deutsch = "Shutdown hat den Socket schon deaktiviert";
        } else
      #endif
      #ifdef ETOOMANYREFS
      if (i == ETOOMANYREFS) { name = "ETOOMANYREFS";
        english = "Too many references: can't splice";
        } else
      #endif
      #ifdef ETIMEDOUT
      if (i == ETIMEDOUT) { name = "ETIMEDOUT";
        english = "Connection timed out";
        deutsch = "Verbindung nach Timeout gekappt";
        } else
      #endif
      #ifdef ECONNREFUSED
      if (i == ECONNREFUSED) { name = "ECONNREFUSED";
        english = "Connection refused";
        deutsch = "Gegenseite verweigert die Verbindung";
        } else
      #endif
      #if 0
      if (i == ) { name = "";
        english = "Remote peer released connection";
        } else
      #endif
      #ifdef EHOSTDOWN
      if (i == EHOSTDOWN) { name = "EHOSTDOWN";
        english = "Host is down";
        deutsch = "Gegenseite ist wohl abgeschaltet";
        } else
      #endif
      #ifdef EHOSTUNREACH
      if (i == EHOSTUNREACH) { name = "EHOSTUNREACH";
        #ifndef vms
        english = "Host is unreachable";
        #else /* VMS */
        english = "No route to host";
        #endif
        deutsch = "Gegenseite nicht in Sichtweite, nicht erreichbar";
        } else
      #endif
      #if 0
      if (i == ) { name = "";
        english = "Networking error";
        } else
      #endif
      /* Quotas: */
      #ifdef EPROCLIM
      if (i == EPROCLIM) { name = "EPROCLIM";
        english = "Too many processes";
        deutsch = "Zu viele Prozesse am Laufen";
        } else
      #endif
      #ifdef EUSERS
      if (i == EUSERS) { name = "EUSERS";
        english = "Too many users";
        deutsch = "Zu viele Benutzer aktiv";
        } else
      #endif
      #ifdef EDQUOT
      if (i == EDQUOT) { name = "EDQUOT";
        english = "Disk quota exceeded";
        deutsch = "Plattenplatz rationiert, Ihr Anteil ist erschöpft";
        } else
      #endif
      /* Errors im Zusammenhang mit STREAMS: */
      #ifdef ENOSTR
      if (i == ENOSTR) { name = "ENOSTR";
        english = "Not a stream device";
        deutsch = "Das ist kein STREAM";
        } else
      #endif
      #ifdef ETIME
      if (i == ETIME) { name = "ETIME";
        english = "Timer expired";
        deutsch = "STREAM braucht länger als erwartet";
        } else
      #endif
      #ifdef ENOSR
      if (i == ENOSR) { name = "ENOSR";
        english = "Out of stream resources";
        deutsch = "Kein Platz für weiteren STREAM";
        } else
      #endif
      #ifdef ENOMSG
      if (i == ENOMSG) { name = "ENOMSG";
        english = "No message of desired type";
        deutsch = "Nachrichten dieses Typs gibt es hier nicht";
        } else
      #endif
      #ifdef EBADMSG
      if (i == EBADMSG) { name = "EBADMSG";
        english = "Not a data message";
        deutsch = "Nachricht von unbekanntem Typ angekommen";
        } else
      #endif
      /* Errors bei SystemV IPC: */
      #ifdef EIDRM
      if (i == EIDRM) { name = "EIDRM";
        english = "Identifier removed";
        deutsch = "Name (einer Semaphore) wurde gelöscht";
        } else
      #endif
      /* Errors bei SystemV Record-Locking: */
      #ifdef EDEADLK
      if (i == EDEADLK) { name = "EDEADLK";
        english = "Resource deadlock would occur";
        deutsch = "Das würde zu einem Deadlock führen";
        } else
      #endif
      #ifdef ENOLCK
      if (i == ENOLCK) { name = "ENOLCK";
        english = "No record locks available";
        deutsch = "Zu viele Zugriffsvorbehalte auf einmal";
        } else
      #endif
      /* Errors bei Remote File System (RFS): */
      #ifdef ENONET
      if (i == ENONET) { name = "ENONET";
        english = "Machine is not on the network";
        deutsch = "Rechner nicht übers Netz erreichbar";
        } else
      #endif
      #ifdef ERREMOTE
      if (i == ERREMOTE) { name = "ERREMOTE";
        english = "Object is remote";
        deutsch = "Das kann nur der dortige Rechner";
        } else
      #endif
      #ifdef ENOLINK
      if (i == ENOLINK) { name = "ENOLINK";
        english = "Link has been severed";
        deutsch = "Verbindung ist zusammengebrochen";
        } else
      #endif
      #ifdef EADV
      if (i == EADV) { name = "EADV";
        english = "Advertise error";
        deutsch = "Andere Rechner benutzen noch unsere Ressourcen";
        } else
      #endif
      #ifdef ESRMNT
      if (i == ESRMNT) { name = "ESRMNT";
        english = "Srmount error";
        deutsch = "Andere Rechner benutzen noch unsere Ressourcen";
        } else
      #endif
      #ifdef ECOMM
      if (i == ECOMM) { name = "ECOMM";
        english = "Communication error on send";
        deutsch = "Beim Senden: Rechner nicht erreichbar";
        } else
      #endif
      #ifdef EPROTO
      if (i == EPROTO) { name = "EPROTO";
        english = "Protocol error";
        deutsch = "Protokoll klappt nicht";
        } else
      #endif
      #ifdef EMULTIHOP
      if (i == EMULTIHOP) { name = "EMULTIHOP";
        english = "Multihop attempted";
        deutsch = "Ressourcen nicht direkt erreichbar";
        } else
      #endif
      #ifdef EDOTDOT
      if (i == EDOTDOT) { name = "EDOTDOT";
        } else
      #endif
      #ifdef EREMCHG
      if (i == EREMCHG) { name = "EREMCHG";
        english = "Remote address changed";
        deutsch = "Rechner hat jetzt eine andere Adresse";
        } else
      #endif
      /* Errors von POSIX: */
      #ifdef ENOSYS
      if (i == ENOSYS) { name = "ENOSYS";
        english = "Function not implemented";
        deutsch = "POSIX-Funktion hier nicht implementiert";
        } else
      #endif
      /* Sonstige: */
      #ifdef EMSDOS /* emx 0.8e */
      if (i == EMSDOS) { name = "EMSDOS";
        english = "Not supported under MS-DOS";
        deutsch = "Das geht unter MS-DOS nicht";
        } else
      #endif
      ;
      if (!(name==NULL)) { tabelle[i].name = name; }
      if (!(english==NULL))
        if (tabelle[i].english[0]=='\0') /* System-Meldung nicht überschreiben */
          { tabelle[i].english = english; }
      if (!(deutsch==NULL)) { tabelle[i].deutsch = deutsch; }
    }
  /* Ist keine Übersetzung vorhanden, so nehmen wir die englische Meldung: */
  for (i=0; i<anzahl; i++)
    { if (tabelle[i].deutsch[0]=='\0')
        { tabelle[i].deutsch = tabelle[i].english; }
    }
  /* Fehlermeldungen ausgeben: */
  printf("#define errcode_limit  %d\n",anzahl);
  printf("local char* errormsg_table[2*errcode_limit] = {\n");
  for (i=0; i<anzahl; i++)
    { printf("/* %d */ \"%s\",\n",i,tabelle[i].name);
      printf("  ENGLISH ? \"%s\" :\n",tabelle[i].english);
      printf("  DEUTSCH ? \"%s\" :\n",tabelle[i].deutsch);
      printf("  \"\",\n");
    }
  printf("};\n");
  exit(0);
}

