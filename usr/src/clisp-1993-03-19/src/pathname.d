# Pathnames für CLISP
# Bruno Haible 19.3.1993

#include "lispbibl.c"


#ifdef UNIX
  # Library-Funktion realpath implementieren:
  # [Copyright: SUN Microsystems, B. Haible]
  # TITLE
  #   REALPATH(3)
  # SYNOPSIS
  #   char* realpath (char* path, char resolved_path[MAXPATHLEN]);
  # DESCRIPTION
  #   realpath() expands all symbolic links  and  resolves  refer-
  #   ences  to '/./', '/../' and extra '/' characters in the null
  #   terminated string named by path and stores the canonicalized
  #   absolute pathname in the buffer named by resolved_path.  The
  #   resulting path will have no symbolic links  components,  nor
  #   any '/./' or '/../' components.
  # RETURN VALUES
  #   realpath() returns a pointer to the  resolved_path  on  suc-
  #   cess.   On  failure, it returns NULL, sets errno to indicate
  #   the error, and places in resolved_path the absolute pathname
  #   of the path component which could not be resolved.
  #define realpath  my_realpath  # Consensys deklariert realpath() bereits...
  local char* realpath (char* path, char* resolved_path);
  # Methode: benutze getwd und readlink.
  local char* realpath(path,resolved_path)
    char* path;
    char* resolved_path;
    { char mypath[MAXPATHLEN];
      int symlinkcount = 0; # Anzahl bisher aufgetretener symbolischer Links
      char* resolved_limit = &resolved_path[MAXPATHLEN-1];
      # Gültige Pointer sind die mit resolved_path <= ptr <= resolved_limit.
      # In *resolved_limit darf höchstens noch ein Nullbyte stehen.
      # (Analog mit mypath.)
      char* resolve_start;
      { char* resolved_ptr = resolved_path; # (bleibt stets <= resolved_limit)
        # evtl. Working-Directory benutzen:
        if (!(path[0]=='/')) # kein absoluter Pathname ?
          { if (getwd(resolved_path) == NULL) { return NULL; }
            resolved_ptr = resolved_path;
            while (*resolved_ptr) { resolved_ptr++; }
            if (resolved_ptr < resolved_limit) { *resolved_ptr++ = '/'; }
            resolve_start = resolved_ptr;
          }
          else
          { resolve_start = resolved_ptr = &resolved_path[0]; }
        # Dann path selber einkopieren:
       {char* path_ptr = path;
        while ((resolved_ptr < resolved_limit) && *path_ptr)
          { *resolved_ptr++ = *path_ptr++; }
        # Mit '/' und einem Nullbyte abschließen:
        if (resolved_ptr < resolved_limit) { *resolved_ptr++ = '/'; }
        *resolved_ptr = 0;
      }}
      # Los geht's nun in resolved_path ab resolve_start.
      { char* from_ptr = resolve_start;
        char* to_ptr = resolve_start;
        while ((to_ptr < resolved_limit) && (*from_ptr))
          # Bis hierher hat der Pfad in  resolved_path[0]...to_ptr[-1]
          # die Gestalt '/subdir1/subdir2/.../txt',
          # wobei 'txt' evtl. leer, aber kein subdir leer.
          { char next = *from_ptr++; *to_ptr++ = next;
            if ((next == '/') && (to_ptr > resolved_path+1))
              # to_ptr[-1]='/'  ->  Directory ...to_ptr[-2] auflösen:
              { char* last_subdir_end = &to_ptr[-2];
                switch (*last_subdir_end)
                  { case '/':
                      # '//' wird zu '/' vereinfacht:
                      to_ptr--;
                      break;
                    case '.':
                      { char* last_subdir_ptr = &last_subdir_end[-1];
                        switch (*last_subdir_ptr)
                          { case '.':
                              if (*--last_subdir_ptr == '/')
                                # letztes subdir war '/../'
                                # Dafür das subdir davor entfernen:
                                { while ((last_subdir_ptr > resolved_path) && !(*--last_subdir_ptr == '/'));
                                  to_ptr = last_subdir_ptr+1;
                                }
                              break;
                            case '/':
                              # letztes subdir war '/./'
                              # entfernen:
                              to_ptr = last_subdir_end;
                              break;
                      }   }
                      break;
                    default:
                      # nach einem normalen subdir
                      # symbolischen Link lesen:
                      to_ptr[-1]=0; # '/' durch 0 ersetzen
                      { int linklen = readlink(resolved_path,mypath,sizeof(mypath)-1);
                        if (linklen >=0)
                          # war ein symbolisches Link
                          { if (++symlinkcount > MAXSYMLINKS) { errno = ELOOP; return NULL; }
                            # noch aufzulösenden path-Anteil an den Link-Inhalt anhängen:
                            { char* mypath_ptr = &mypath[linklen]; # ab hier ist Platz
                              char* mypath_limit = &mypath[MAXPATHLEN-1]; # bis hierher
                              if (mypath_ptr < mypath_limit) { *mypath_ptr++ = '/'; } # erst ein '/' anhängen
                              # dann den Rest:
                              while ((mypath_ptr <= mypath_limit) && (*mypath_ptr = *from_ptr++)) { mypath_ptr++; }
                              *mypath_ptr = 0; # und mit 0 abschließen
                            }
                            # Dies ersetzt bzw. ergänzt den path:
                            if (mypath[0] == '/')
                              # ersetzt den path:
                              { from_ptr = &mypath[0]; to_ptr = resolved_path;
                                while (*to_ptr++ = *from_ptr++);
                                from_ptr = resolved_path;
                              }
                              else
                              # ergänzt den path:
                              { # Linknamen streichen. Dazu bis zum letzten '/' suchen:
                                { char* ptr = &to_ptr[-1];
                                  while ((ptr > resolved_path) && !(*--ptr == '/'));
                                  from_ptr = &ptr[1];
                                }
                                { char* mypath_ptr = &mypath[0]; to_ptr = from_ptr;
                                  while ((to_ptr <= resolved_limit) && (*to_ptr++ = *mypath_ptr++));
                              } }
                            to_ptr = from_ptr;
                          }
                          else
                          if (!(errno == EINVAL)) { return NULL; } # Fehler?
                          else
                          # kein symbolisches Link
                          { to_ptr[-1] = '/'; } # wieder den '/' eintragen
                      }
                      break;
              }   }
          } # dann zum nächsten subdir
        # ein '/' am Ende streichen:
        if ((to_ptr[-1] == '/') && (to_ptr > resolved_path+1)) { to_ptr--; }
        to_ptr[0] = 0; # durch 0 abschließen
        return resolved_path; # fertig
    } }
#endif


# ==============================================================================
#                         P A T H N A M E S

#ifdef PATHNAME_ATARI
# Komponenten:
# HOST          stets NIL
# DEVICE        NIL oder :WILD oder "A"|...|"Z"
# DIRECTORY     (Disknummer Startpoint . Subdirs) wobei
#                Disknummer = NIL oder die Seriennummer der Diskette ist,
#                Startpoint = :RELATIVE | :ABSOLUTE
#                Subdirs = () | (subdir . Subdirs)
#                subdir = :CURRENT (bedeutet ".") oder
#                subdir = :PARENT (bedeutet "..") oder
#                subdir = :WILD (bedeutet "...", alle Subdirectories) oder
#                subdir = (name . type)
#                 name = :WILD oder Simple-String mit max. 8 Zeichen
#                 type = :WILD oder Simple-String mit max. 3 Zeichen
# NAME          NIL oder :WILD oder Simple-String mit max. 8 Zeichen
# TYPE          NIL oder :WILD oder Simple-String mit max. 3 Zeichen
# VERSION       stets NIL (auch :WILD oder :NEWEST bei Eingabe)
# Wenn ein Pathname vollständig spezifiziert sein muß (keine Wildcards),
# ist :WILD nicht erlaubt, bei NAME evtl. auch nicht NIL.
# Externe Notation: A123456:\sub1.typ\sub2.typ\name.typ
# mit Defaults:             \sub1.typ\sub2.typ\name.typ
# oder                                         name.typ
# oder                    *:\sub1.typ\*.*\name.*
# oder Ähnliches.
#endif

#ifdef PATHNAME_MSDOS
# Komponenten:
# HOST          stets NIL
# DEVICE        NIL oder :WILD oder "A"|...|"Z"
# DIRECTORY     (Startpoint . Subdirs) wobei
#                Startpoint = :RELATIVE | :ABSOLUTE
#                Subdirs = () | (subdir . Subdirs)
#                subdir = :CURRENT (bedeutet ".") oder
#                subdir = :PARENT (bedeutet "..") oder
#                subdir = :WILD (bedeutet "...", alle Subdirectories) oder
#                subdir = (name . type)
#                 name = :WILD oder Simple-String mit max. 8 Zeichen
#                 type = :WILD oder Simple-String mit max. 3 Zeichen
# NAME          NIL oder :WILD oder Simple-String mit max. 8 Zeichen
# TYPE          NIL oder :WILD oder Simple-String mit max. 3 Zeichen
# VERSION       stets NIL (auch :WILD oder :NEWEST bei Eingabe)
# Wenn ein Pathname vollständig spezifiziert sein muß (keine Wildcards),
# ist :WILD nicht erlaubt, bei NAME evtl. auch nicht NIL.
# Externe Notation:       A:\sub1.typ\sub2.typ\name.typ
# mit Defaults:             \sub1.typ\sub2.typ\name.typ
# oder                                         name.typ
# oder                    *:\sub1.typ\*.*\name.*
# oder Ähnliches.
# Statt '\' ist - wie unter DOS üblich - auch '/' erlaubt.
#endif

#ifdef PATHNAME_AMIGAOS
# Komponenten:
# HOST          stets NIL
# DEVICE        NIL oder Simple-String
# DIRECTORY     (Startpoint . Subdirs) wobei
#                Startpoint = :RELATIVE | :ABSOLUTE
#                Subdirs = () | (subdir . Subdirs)
#                subdir = :WILD (bedeutet "**" oder "...", alle Subdirectories) oder
#                subdir = :PARENT (bedeutet "/" statt "subdir/") oder
#                subdir = Simple-String, evtl. mit Wildcard-Zeichen ? und *
# NAME          NIL oder
#               Simple-String, evtl. mit Wildcard-Zeichen ? und *
#               (auch :WILD bei der Eingabe)
# TYPE          NIL oder
#               Simple-String, evtl. mit Wildcard-Zeichen ? und *
#               (auch :WILD bei der Eingabe)
# VERSION       stets NIL (auch :WILD oder :NEWEST bei Eingabe)
# Constraint: Startpoint = :RELATIVE nur, falls Device = NIL;
#             bei angegebenem Device gibt es also nur absolute Pathnames!
# Ein AMIGAOS-Filename wird folgendermaßen in Name und Typ aufgespalten:
#   falls kein '.' im Filename: Name = alles, Typ = NIL,
#   falls '.' im Filename: Name = alles vor, Typ = alles nach dem letzten '.' .
# Groß-/Klein-Schreibung innerhalb der Strings wird bei Vergleichen ignoriert,
# aber ansonsten findet keine Groß/Klein-Umwandlung statt.
# Wenn ein Pathname vollständig spezifiziert sein muß (keine Wildcards),
# ist :WILD nicht erlaubt, keine Wildcard-Zeichen in den Strings,
# bei NAME evtl. auch nicht NIL.
# Externe Notation:  device:sub1.typ/sub2.typ/name.typ
# mit Defaults:             sub1.typ/sub2.typ/name.typ
# oder                                        name.typ
# oder                      sub1.typ/ ** /sub3.typ/x*.lsp  (ohne Spaces!)
# oder Ähnliches.
# Formal:
#   ch ::= beliebgiges Character außer ':','/' und '*','?'
#   name ::= {ch}+
#   device ::= [ <leer> | ':' | name ':' ]
#              ; leer = aktuelles Device, relativ ab aktuellem Directory
#              ; ':'  = aktuelles Device, absolut (ab root bei Disks)
#              ; name ':' = angegebenes Device, absolut (ab root bei Disks)
#   subdir ::= [ <leer> | name ]                ; leer = '..'
#   pathname ::= device { subdir '/' }* name
# Beispiele:
#   String        Device    Directory                unser Pathname
#   ------        ------    ---------                --------------
#   'c:foo'       'C',     device->foo               "c" (:ABSOLUTE "foo")
#   'c:foo/'      'C',     device->foo               "c" (:ABSOLUTE "foo")
#   'c:foo/bar'   'C',     device->foo->bar          "c" (:ABSOLUTE "foo" "bar")
#   'c:/foo'      'C',     device->up->foo           "c" (:ABSOLUTE :PARENT "foo")
#   'c:'          'C',     device                    "c" (:ABSOLUTE)
#   ':foo'        current, device->root->foo         NIL (:ABSOLUTE "foo")
#   'foo'         current, device->foo               NIL (:RELATIVE "foo")
#   '/foo'        current, device->up->foo           NIL (:RELATIVE :PARENT "foo")
#   '//foo/bar'   current, device->up->up->foo->bar  NIL (:RELATIVE :PARENT :PARENT "foo" "bar")
#   ''            current, device                    NIL (:RELATIVE)
# An einen Pathstring, der nichtleer ist und der nicht mit ':' oder '/'
# endet, kann ein '/' angehängt werden, ohne seine Semantik zu verändern.
# Dieser '/' muß angehängt werden, bevor man eine weitere nichtleere
# Komponente anhängen kann.
# An einen Pathstring, der leer ist oder mit ':' oder '/' endet, ein '/'
# anzuhängen, bedeutet aber, zum Parent Directory aufzusteigen!
# Bei uns wird jeder Pathstring, der leer ist oder mit ':' oder '/' endet,
# als Directory-Pathname (mit Name=NIL und Type=NIL) interpretiert.
#endif

#ifdef PATHNAME_UNIX
# Komponenten:
# HOST          stets NIL
# DEVICE        stets NIL
# DIRECTORY     (Startpoint . Subdirs) wobei
#                Startpoint = :RELATIVE | :ABSOLUTE
#                Subdirs = () | (subdir . Subdirs)
#                subdir = :WILD (bedeutet "**" oder "...", alle Subdirectories) oder
#                subdir = Simple-String, evtl. mit Wildcard-Zeichen ? und *
# NAME          NIL oder
#               Simple-String, evtl. mit Wildcard-Zeichen ? und *
#               (auch :WILD bei der Eingabe)
# TYPE          NIL oder
#               Simple-String, evtl. mit Wildcard-Zeichen ? und *
#               (auch :WILD bei der Eingabe)
# VERSION       stets NIL (auch :WILD oder :NEWEST bei Eingabe)
# Ein UNIX-Filename wird folgendermaßen in Name und Typ aufgespalten:
#   falls kein '.' im Filename: Name = alles, Typ = NIL,
#   falls '.' im Filename: Name = alles vor, Typ = alles nach dem letzten '.' .
# Wenn ein Pathname vollständig spezifiziert sein muß (keine Wildcards),
# ist :WILD nicht erlaubt, keine Wildcard-Zeichen in den Strings,
# bei NAME evtl. auch nicht NIL.
# Externe Notation:  server:/sub1.typ/sub2.typ/name.typ
# mit Defaults:             /sub1.typ/sub2.typ/name.typ
# oder                                         name.typ
# oder                      /sub1.typ/ ** /sub3.typ/x*.lsp  (ohne Spaces!)
# oder Ähnliches.
#endif

#ifdef PATHNAME_OS2
# Komponenten:
# HOST          stets NIL
# DEVICE        NIL oder :WILD oder "A"|...|"Z"
# DIRECTORY     (Startpoint . Subdirs) wobei
#                Startpoint = :RELATIVE | :ABSOLUTE
#                Subdirs = () | (subdir . Subdirs)
#                subdir = :WILD (bedeutet "**" oder "...", alle Subdirectories) oder
#                subdir = Simple-String, evtl. mit Wildcard-Zeichen ? und *
# NAME          NIL oder
#               Simple-String, evtl. mit Wildcard-Zeichen ? und *
#               (auch :WILD bei der Eingabe)
# TYPE          NIL oder
#               Simple-String, evtl. mit Wildcard-Zeichen ? und *
#               (auch :WILD bei der Eingabe)
# VERSION       stets NIL (auch :WILD oder :NEWEST bei Eingabe)
# Ein OS/2-Filename wird folgendermaßen in Name und Typ aufgespalten:
#   falls kein '.' im Filename: Name = alles, Typ = NIL,
#   falls '.' im Filename: Name = alles vor, Typ = alles nach dem letzten '.' .
# Wenn ein Pathname vollständig spezifiziert sein muß (keine Wildcards),
# ist :WILD nicht erlaubt, keine Wildcard-Zeichen in den Strings,
# bei NAME evtl. auch nicht NIL.
# Externe Notation:       A:\sub1.typ\sub2.typ\name.typ
# mit Defaults:             \sub1.typ\sub2.typ\name.typ
# oder                                         name.typ
# oder                    *:\sub1.typ\**\sub3.typ\x*.lsp
# oder Ähnliches.
# Statt '\' ist - wie unter DOS üblich - auch '/' erlaubt.
#endif

#ifdef PATHNAME_VMS
# Komponenten:
# HOST          NIL oder Simple-String
# DEVICE        NIL oder Simple-String
# DIRECTORY     (Startpoint . Subdirs) wobei
#                Startpoint = :RELATIVE | :ABSOLUTE
#                Subdirs = () | (subdir . Subdirs)
#                subdir = :PARENT (bedeutet "-") oder
#                subdir = :WILD (bedeutet "**", alle Subdirectories) oder
#                subdir = Simple-String, evtl. mit Wildcard-Zeichen ? und *
# NAME          NIL oder
#               Simple-String, evtl. mit Wildcard-Zeichen ? und *
#               (auch :WILD bei der Eingabe)
# TYPE          NIL oder
#               Simple-String, evtl. mit Wildcard-Zeichen ? und *
#               (auch :WILD bei der Eingabe)
# VERSION       NIL oder :WILD oder :NEWEST oder ein Fixnum > 0
# Wenn ein Pathname vollständig spezifiziert sein muß (keine Wildcards),
# ist :WILD nicht erlaubt, keine Wildcard-Zeichen in den Strings,
# bei NAME evtl. auch nicht NIL.
# Externe Notation:    server::device:[sub1.sub2]name.typ;3
# oder                 server::device:<sub1.sub2>name.typ.3
# mit Defaults:                device:[sub1.sub2]name.typ
# oder                                [sub1.sub2]name.typ
# oder                                           name.typ
# oder                                [sub1.**.sub3]x*.lsp
# oder Ähnliches.
#endif

#if HAS_HOST
# UP: Überprüft ein optionales Host-Argument.
# test_optional_host(host)
# > host: Host-Argument
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: gültige Host-Komponente
# kann GC auslösen
  local object test_optional_host (object host);
  local object test_optional_host(host)
    var reg4 object host;
    { if (eq(host,unbound)) { return NIL; } # nicht angegeben -> NIL
      if (nullp(host)) goto OK; # NIL ist OK
      # Sonst muß host ein String sein, dessen Zeichen alphanumerisch sind:
      if (!stringp(host))
        { pushSTACK(host);
          pushSTACK(TheSubr(subr_self)->name);
          fehler(
                 DEUTSCH ? "~: Host muß NIL oder ein String sein, nicht ~" :
                 ENGLISH ? "~: host should be NIL or a string, not ~" :
                 FRANCAIS ? "~ : Le nom de machine hôte doit être NIL ou de type STRING et non ~" :
                 ""
                );
        }
      host = coerce_ss(host); # als Simple-String
      { var reg3 uintL len = TheSstring(host)->length;
        var reg2 uintB* charptr = &TheSstring(host)->data[0];
        dotimesL(len,len,
          { var reg1 uintB ch = *charptr++;
            if (!alphanumericp(ch)) goto badhost;
          });
      }
      OK: return host;
      badhost:
        { pushSTACK(host);
          pushSTACK(TheSubr(subr_self)->name);
          fehler(
                 DEUTSCH ? "~: syntaktisch illegaler Hostname ~" :
                 ENGLISH ? "~: illegal hostname ~" :
                 FRANCAIS ? "~ : Syntaxe incorrecte pour un nom de machine hôte: ~" :
                 ""
                );
        }
    }
#else
# UP: Überprüft ein optionales Host-Argument.
# test_optional_host(host);
# > host: Host-Argument
# > subr_self: Aufrufer (ein SUBR)
  local void test_optional_host (object host);
  local void test_optional_host(host)
    var reg1 object host;
    { if (!eq(host,unbound)) # nicht angegeben -> OK
        { if (!nullp(host)) # angegeben -> sollte =NIL sein
            { pushSTACK(host);
              pushSTACK(TheSubr(subr_self)->name);
              fehler(
                     DEUTSCH ? "~: Host muß NIL sein, nicht ~" :
                     ENGLISH ? "~: host should be NIL, not ~" :
                     FRANCAIS ? "~ : Le nom de machine hôte doit être NIL et non ~" :
                     ""
                    );
            }
    }   }
#endif

# Stellt fest, ob zwei Characters als Zeichen in Pathnames als gleich gelten.
# equal_pathchar(ch1,ch2)
# > uintB ch1,ch2: Character-Codes
# < ergebnis: TRUE falls gleich, FALSE sonst
  #if !(defined(PATHNAME_AMIGAOS) || defined(PATHNAME_OS2))
    #define equal_pathchar(ch1,ch2)  ((ch1)==(ch2))
  #else # defined(PATHNAME_AMIGAOS) || defined(PATHNAME_OS2)
    # Case-insensitive, aber normalerweise ohne Konversion
    #define equal_pathchar(ch1,ch2)  (up_case(ch1)==up_case(ch2))
  #endif

# UP: Stellt fest, ob ein Character als Zeichen im Namens-/Typ-Teil eines
# Namestring erlaubt ist.
# legal_namechar(ch)
# > uintB ch: Character-Code
# < ergebnis: TRUE falls erlaubt, FALSE sonst
  local boolean legal_namechar (uintB ch);
  local boolean legal_namechar(ch)
    var reg1 uintB ch;
    {
      #if defined(PATHNAME_ATARI) || defined(PATHNAME_MSDOS)
      return ((ch=='_') || (ch=='-') || alphanumericp(ch));
      #endif
      #ifdef PATHNAME_AMIGAOS
      return (graphic_char_p(ch) && !(ch=='/') && !(ch==':'));
      #endif
      #ifdef PATHNAME_UNIX
      return ((ch>=' ') && (ch<='~') && !(ch=='/'));
      #endif
      #ifdef PATHNAME_OS2
      return (graphic_char_p(ch) && !(ch=='\\') && !(ch=='/') && !(ch==':'));
      #endif
      #ifdef PATHNAME_VMS
      return ((ch=='_') || (ch=='-') || (ch=='$') || alphanumericp(ch));
      #endif
    }

# Wandelt ein Objekt in einen Pathname um.
  local object coerce_pathname (object obj); # später

# Fehlermeldung wegen illegalem Pathname-Argument.
# fehler_thing(thing);
# > thing: (fehlerhaftes) Argument
# > subr_self: Aufrufer (ein SUBR)
  local nonreturning void fehler_thing (object thing);
  local nonreturning void fehler_thing(thing)
    var reg1 object thing;
    { pushSTACK(thing);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(
             DEUTSCH ? "~: Argument muß ein String, Symbol, File-Stream oder Pathname sein, nicht ~" :
             ENGLISH ? "~: argument should be a string, symbol, file stream or pathname, not ~" :
             FRANCAIS ? "~ : L'argument doit être une chaîne, un symbole, un «stream» de fichier ou un «pathname» et non ~" :
             ""
            );
    }

#if defined(UNIX) || defined(MSDOS)

# UP: Wandelt eine Unix-Directory-Angabe in ein Pathname um.
# asciz_dir_to_pathname(path)
# > const char* path: path als ASCIZ-String
# < ergebnis: als Pathname ohne Name und Typ
  local object asciz_dir_to_pathname (const char* path);
  local object asciz_dir_to_pathname(path)
    var reg4 const char* path;
     { var reg1 const char* pathptr = path;
       var reg2 uintL len = 0; # Stringlänge
       until (*pathptr == 0) { pathptr++; len++; } # ASCIZ-Stringende suchen
       # Sofern der String nicht schon mit '/' endet, wird ein '/' angefügt:
       if (!((len>0) && (pathptr[-1]=='/'))) { len++; }
       # und in einen String umwandeln:
      {var reg3 object pathname = make_string((const uintB*)path,len);
       TheSstring(pathname)->data[len-1] = '/'; # abschließendes '/' unterbringen
       # und in ein Pathname umwandeln:
       return coerce_pathname(pathname);
     }}

#endif

#ifdef VMS

# UP: Wandelt eine VMS-Directory-Angabe in ein Pathname um.
# asciz_dir_to_pathname(path)
# > const char* path: path als ASCIZ-String
# < ergebnis: als Pathname ohne Name und Typ
  local object asciz_dir_to_pathname (const char* path);
  local object asciz_dir_to_pathname(path)
    var reg1 const char* path;
    { return coerce_pathname(asciz_to_string(path)); }

#endif

# Typ für PARSE-NAMESTRING:
# Der String wird durchlaufen.
  typedef struct { uintL index; # Index (incl. Offset)
                   object FNindex; # Index als Fixnum
                   uintL count; # Anzahl der verbleibenden Characters
                 }
          zustand;

#ifdef PATHNAME_EXT83
# Hilfsfunktion für PARSE-NAMESTRING:
# Parst einen Namens- oder Typteil.
# parse_name_or_type(&z,stdlen,def)
# > stdlen: Standard-Länge des Teils
# > def: Defaultwert
# > STACK_3: Datenvektor des Strings
# > z: Zustand
# < z: Zustand
# < ergebnis: Namens- oder Typteil (=default, falls leer)
# kann GC auslösen
  local object parse_name_or_type (zustand* z, uintL stdlen, object def);
  local object parse_name_or_type(z,stdlen,def)
    var reg1 zustand* z;
    var reg5 uintL stdlen;
    var reg6 object def;
    { var reg3 uintL z_start_index = z->index; # Index beim Start des Namens
      loop
        { var reg2 uintB ch;
          if (z->count == 0) break;
          ch = TheSstring(STACK_3)->data[z->index]; # nächstes Character
          ch = up_case(ch); # als Großbuchstabe
          if (ch == '.') break;
          if (ch == '*')
            # '*' angetroffen.
            { # nicht am Anfang des Namens -> beendet den Namen:
              if (!(z->index == z_start_index)) break;
              # Character übergehen:
              z->index++; z->FNindex = fixnum_inc(z->FNindex,1); z->count--;
              return S(Kwild); # Name := :WILD
            }
          if (!legal_namechar(ch)) break; # gültiges Character ?
          # ja -> Teil des Namens
          # Character übergehen:
          z->index++; z->FNindex = fixnum_inc(z->FNindex,1); z->count--;
        }
      # Ende des Namens erreicht.
      # Name := Teilstring von STACK_3 von z_start_index (einschließlich)
      #                                bis z->index (ausschließlich).
     {var reg3 uintL len = z->index - z_start_index;
      # kein Name angegeben -> default zurück:
      if (len==0) { return def; }
      #ifndef EMUNIX_PORTABEL # unter OS/2 gilt die 8+3-Regel nicht mehr
      # bei len > stdlen setze len:=stdlen :
      if (len > stdlen) { len = stdlen; }
      #endif
      {var reg4 object string = allocate_string(len); # String der Länge len
       # füllen:
       var reg1 uintB* ptr1 = &TheSstring(STACK_3)->data[z_start_index];
       var reg2 uintB* ptr2 = &TheSstring(string)->data[0];
       dotimespL(len,len, { *ptr2++ = up_case(*ptr1++); });
       # Name fertig.
       return string;
    }}}
#endif

#ifdef PATHNAME_VMS
# Hilfsfunktion für PARSE-NAMESTRING:
# Parst einen Namens- oder Typteil.
# parse_name_or_type(&z,def)
# > def: Defaultwert
# > STACK_3: Datenvektor des Strings
# > z: Zustand
# < z: Zustand
# < ergebnis: Namens- oder Typteil (=default, falls leer)
# kann GC auslösen
  local object parse_name_or_type (zustand* z, object def);
  local object parse_name_or_type(z,def)
    var reg1 zustand* z;
    var reg6 object def;
    { var reg3 uintL z_start_index = z->index; # Index beim Start des Namens
      loop
        { var reg2 uintB ch;
          if (z->count == 0) break;
          ch = TheSstring(STACK_3)->data[z->index]; # nächstes Character
          ch = up_case(ch); # als Großbuchstabe
          # gültiges oder Wildcard-Character ?
          if (/* (ch=='.') || */ !(legal_namechar(ch)||(c=='%')||(c=='?')||(c=='*')))
            break;
          # ja -> Teil des Namens
          # Character übergehen:
          z->index++; z->FNindex = fixnum_inc(z->FNindex,1); z->count--;
        }
      # Ende des Namens erreicht.
      # Name := Teilstring von STACK_3 von z_start_index (einschließlich)
      #                                bis z->index (ausschließlich).
     {var reg4 uintL len = z->index - z_start_index;
      # kein Name angegeben -> default zurück:
      if (len==0) { return def; }
      {var reg5 object string = allocate_string(len); # String der Länge len
       # füllen:
       var reg2 uintB* ptr1 = &TheSstring(STACK_3)->data[z_start_index];
       var reg3 uintB* ptr2 = &TheSstring(string)->data[0];
       dotimespL(len,len,
         { var reg1 uintB ch = *ptr1++;
           *ptr2++ = (ch=='%' ? '?' : up_case(ch)); # Wildcard % in ? umwandeln
         });
       # Name fertig.
       return string;
    }}}
# Hilfsfunktion für PARSE-NAMESTRING:
# Stellt fest, ob eine Zeichenfolge nur aus Bindestrichen besteht
# only_dashes_p(ptr,len)
# > [ptr..ptr+len-1]: nichtleere Zeichenfolge
# < ergebnis: TRUE falls es nur Bindestriche sind, FALSE sonst
  local boolean only_dashes_p (uintB* ptr, uintL len);
  local boolean only_dashes_p(ptr,len)
    var reg1 uintB* ptr;
    var reg2 uintL len; # len > 0
    { dotimespL(len,len, { if (!(*ptr++ == '-')) return FALSE; });
      return TRUE;
    }
#endif

#ifdef PATHNAME_NOEXT
# Hilfsfunktion für PARSE-NAMESTRING:
# Spaltet einen String (beim letzten Punkt) in Name und Typ auf.
# split_name_type(skip);
# > STACK_0: Simple-String
# > skip: 1 falls ein Punkt an erster Stelle nicht aufspaltend wirken soll, 0 sonst
# < STACK_1: Name
# < STACK_0: Typ
# Erniedrigt STACK um 1
# kann GC auslösen
  local void split_name_type (uintL skip);
  local void split_name_type(skip)
    var reg2 uintL skip;
    { var reg5 object string = STACK_0;
      var reg7 uintL length = TheSstring(string)->length;
      # Nach dem letzten Punkt suchen:
      var reg4 uintL index = length;
      { var reg1 uintB* ptr = &TheSstring(string)->data[index];
        while (index>skip)
          { if (*--ptr == '.') goto punkt;
            index--;
      }   }
      # kein Punkt gefunden -> Typ := NIL
      pushSTACK(NIL);
      goto name_type_ok;
      punkt: # Punkt bei index gefunden
      { # type := (substring string index)
        var reg3 uintL count = length-index;
        var reg6 object type = allocate_string(count);
        var reg1 uintB* ptr2 = &TheSstring(type)->data[0];
        var reg2 uintB* ptr1 = &TheSstring(STACK_0)->data[index];
        dotimesL(count,count, { *ptr2++ = *ptr1++; } );
        pushSTACK(type);
      }
      { # name := (substring string 0 (1- index))
        var reg3 uintL count = index-1;
        var reg6 object name = allocate_string(count);
        var reg1 uintB* ptr2 = &TheSstring(name)->data[0];
        var reg2 uintB* ptr1 = &TheSstring(STACK_1)->data[0];
        dotimesL(count,count, { *ptr2++ = *ptr1++; } );
        STACK_1 = name;
      }
      name_type_ok: ;
    }
#endif

LISPFUN(parse_namestring,1,2,norest,key,3,\
        (kw(start),kw(end),kw(junk_allowed)) )
# (PARSE-NAMESTRING thing [host [defaults [:start] [:end] [:junk-allowed]]]),
# CLTL S. 414
  { # Stackaufbau: thing, host, defaults, start, end, junk-allowed.
    var reg6 boolean junk_allowed;
    # 1. junk-allowed überprüfen:
    { var reg1 object obj = popSTACK(); # junk-allowed-Argument
      if (eq(obj,unbound))
        { junk_allowed = FALSE; }
        else
        if (nullp(obj)) { junk_allowed = FALSE; } else { junk_allowed = TRUE; }
    }
    # Stackaufbau: thing, host, defaults, start, end.
    # 2. Default-Wert für start ist 0:
    { if (eq(STACK_1,unbound)) { STACK_1 = Fixnum_0; }}
    # 3. host überprüfen:
    #if HAS_HOST
    { var reg2 object host = test_optional_host(STACK_3);
      if (nullp(host))
        { # host := (PATHNAME-HOST defaults)
          var reg1 defaults = STACK_2;
          if (eq(defaults,unbound))
            { defaults = defaults_pathname(); }
            else
            { defaults = coerce_pathname(defaults); }
          host = ThePathname(defaults)->pathname_host;
        }
      STACK_3 = host;
    }
    #else
    { test_optional_host(STACK_3); }
    #endif
    # 4. thing muß ein String sein:
    { var reg5 object thing = STACK_4;
      if (pathnamep(thing)) # Pathname?
        { value1 = thing; # 1. Wert thing
          fertig:
          value2 = STACK_1; mv_count=2; # 2. Wert start
          skipSTACK(5); return;
        }
      if (streamp(thing)) # Stream?
        { if_strm_file_p(thing, ; , fehler_thing(thing); );
          value1 = TheStream(thing)->strm_file_name; # 1. Wert: Filename
          goto fertig; # 2. Wert wie oben
        }
      # thing sollte nun wenigstens ein String oder Symbol sein:
      if (!stringp(thing))
        { if (!symbolp(thing)) { fehler_thing(thing); }
          thing = Symbol_name(thing); # Symbol -> Symbolname verwenden
          #if defined(PATHNAME_UNIX) || defined(PATHNAME_OS2) # Betriebssystem mit Vorzug für Kleinbuchstaben?
          thing = copy_string(thing); # ja -> mit STRING-DOWNCASE umwandeln
          nstring_downcase(&TheSstring(thing)->data[0],TheSstring(thing)->length);
          #endif
          #ifdef PATHNAME_AMIGAOS # Betriebssystem mit Vorzug für Capitalize?
          thing = copy_string(thing); # ja -> mit STRING-CAPITALIZE umwandeln
          nstring_capitalize(&TheSstring(thing)->data[0],TheSstring(thing)->length);
          #endif
          STACK_4 = thing; # und in den Stack zurückschreiben
        }
      # thing = STACK_4 ist jetzt ein String.
      { # Er wird durchlaufen.
        var zustand z; # laufender Zustand
       {var object string; # String thing
        # Grenzen überprüfen, mit thing, start, end als Argumenten:
        pushSTACK(thing); pushSTACK(STACK_(1+1)); pushSTACK(STACK_(0+2));
        test_string_limits(&string,&z.index,&z.count);
        # z.index = Wert des start-Arguments,
        # z.count = Anzahl der Characters.
        z.FNindex = fixnum(z.index);
        # z.FNindex = start-Index als Fixnum.
        string = array_displace_check(string,z.count,&z.index); # Datenvektor holen,
        # z.index = Offset + Startindex = Startoffset
        pushSTACK(string);
        pushSTACK(allocate_pathname());
       }# Stackaufbau: ..., Datenvektor, Pathname.
        #if HAS_HOST
          # Host-Specification parsen:
          {var reg3 object host;
           # Kommt eine Folge von alphanumerischen Zeichen und dann ein ':' bzw. '::' ?
           { var zustand startz = z; # Start-Zustand
             var reg1 uintB ch;
             loop
               { if (z.count==0) goto no_hostspec; # String schon zu Ende -> kein Host
                 ch = TheSstring(STACK_1)->data[z.index]; # nächstes Character
                 if (!alphanumericp(ch)) break;
                 # alphanumerisches Character übergehen:
                 z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
               }
             if (!(ch==':')) goto no_hostspec; # kein ':' -> kein Host
             #ifdef PATHNAME_VMS
             if (!((z.count>=2) && (TheSstring(STACK_1)->data[z.index+1]==':')))
               goto no_hostspec; # kein '::' -> kein Host
             #endif
             # Host-String bilden:
             { var reg4 uintL len = z.index - startz.index;
               host = allocate_string(len);
               # und füllen:
              {var reg2 uintB* ptr1 = &TheSstring(STACK_1)->data[startz.index];
               var reg3 uintB* ptr2 = &TheSstring(host)->data[0];
               #ifdef PATHNAME_VMS
               dotimesL(len,len, { *ptr2++ = up_case(*ptr1++); });
               #else
               dotimesL(len,len, { *ptr2++ = *ptr1++; });
               #endif
             }}
             # Character ':' bzw. '::' übergehen:
             #ifdef PATHNAME_VMS
             z.index+=2; z.FNindex = fixnum_inc(z.FNindex,2); z.count-=2;
             #else
             z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
             #endif
             goto hostspec_ok;
             no_hostspec: # keine Host-Specification
               z = startz; # zum Start zurück
               host = STACK_(3+2); # Default-Host
           }
           hostspec_ok:
           # Host eintragen:
           ThePathname(STACK_0)->pathname_host = host;
          }
        #endif
        #if HAS_DEVICE
         #if defined(PATHNAME_ATARI) || defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2)
          # Einbuchstabige Device-Specification und evtl. Seriennummer parsen:
          {var reg3 object device = NIL; # Device := NIL
           #if HAS_SERNR
           var reg4 object seriennummer = NIL; # Seriennummer := NIL
           #endif
           # Drive-Specification parsen:
           # Kommt evtl. ein Buchstabe ('*','A'-'Z','a'-'z'), evtl. eine
           # Seriennummer (Integer >=0, <2^24) und dann ein ':' ?
           { var zustand startz = z; # Start-Zustand
             var reg1 uintB ch;
             if (z.count==0) goto no_drivespec; # String schon zu Ende ?
             ch = TheSstring(STACK_1)->data[z.index]; # nächstes Character
             ch = up_case(ch); # als Großbuchstabe
             if (ch == '*')
               # ch = '*' -> Device := :WILD
               { device = S(Kwild); }
             elif ((ch >= 'A') && (ch <= 'Z'))
               # 'A' <= ch <= 'Z' -> Device := "ch"
               { var reg1 object string = allocate_string(1); # String der Länge 1
                 TheSstring(string)->data[0] = ch; # mit ch als einzigem Buchstaben
                 device = string;
               }
             else goto no_device;
             # Device OK, Character übergehen:
             z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
             if (z.count==0) goto no_drivespec; # String schon zu Ende ?
             ch = TheSstring(STACK_1)->data[z.index]; # nächstes Character
             ch = up_case(ch); # als Großbuchstabe
             no_device:
             #if HAS_SERNR
             # Kommt eventuell eine Seriennummer ?
             if ((ch >= '0') && (ch <= '9'))
               { # ja.
                 var reg2 uintL akku = 0; # Hilfsregister zum Aufbau der Seriennummer
                 loop
                   { ch = ch - '0'; # Wert der Ziffer
                     akku = 10*akku+ch; # Seriennummer um eine Ziffer erweitern
                     if (akku > (uintL)(bitm(oint_addr_len)-1))
                       goto no_drivespec; # >=2^24 geworden -> ist wohl keine Seriennummer
                     # Character übergehen:
                     z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                     if (z.count==0) goto no_drivespec; # String schon zu Ende ?
                     ch = TheSstring(STACK_1)->data[z.index]; # nächstes Character
                     ch = up_case(ch); # als Großbuchstabe
                     # noch eine Ziffer -> zählt zur Seriennummer
                     if (!((ch >= '0') && (ch <= '9'))) break;
                   }
                 # Seriennummer zu Ende
                 seriennummer = fixnum(akku); # akku als Fixnum
               }
             #endif
             # mit Doppelpunkt abgeschlossen?
             if (!(ch == ':')) goto no_drivespec;
             # Character übergehen:
             z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
             goto drivespec_ok;
             no_drivespec:
             # Es ist nicht gelungen, eine Drive-Specification zu parsen.
             z = startz; # Start-Zustand wiederherstellen
             device = NIL; # Device := NIL
             #if HAS_SERNR
             seriennummer = NIL; # Seriennummer := NIL
             #endif
           }
           drivespec_ok:
           ThePathname(STACK_0)->pathname_device = device; # Device eintragen
           #if HAS_SERNR
           if (!NIL_IS_CONSTANT) { pushSTACK(seriennummer); } # Seriennummer retten (kann NIL oder ein Fixnum sein)
           { var reg1 object new_cons = allocate_cons(); # neues Cons
             if (!NIL_IS_CONSTANT) { seriennummer = popSTACK(); } # Seriennummer zurück
             Car(new_cons) = seriennummer; # = (cons Seriennummer NIL)
             ThePathname(STACK_0)->pathname_directory = new_cons;
             pushSTACK(new_cons);
             new_cons = allocate_cons(); # neues Cons für Startpoint
             Cdr(STACK_0) = new_cons; # verlängert (pathname-directory Pathname)
             STACK_0 = new_cons; # neues (last (pathname-directory Pathname))
           }
           #define HAVE_Startpoint_Cons
           #endif
          }
         #endif
         #if defined(PATHNAME_VMS) || defined(PATHNAME_AMIGAOS)
          # Device-Specification parsen:
          {var reg3 object device;
           # Kommt eine nichtleere Folge von alphanumerischen Zeichen und dann ein ':' ?
           { var zustand startz = z; # Start-Zustand
             var reg1 uintB ch;
             loop
               { if (z.count==0) goto no_devicespec; # String schon zu Ende -> kein Device
                 ch = TheSstring(STACK_1)->data[z.index]; # nächstes Character
                 if (!legal_namechar(ch)) break;
                 # alphanumerisches Character übergehen:
                 z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
               }
             if (!(ch==':')) goto no_devicespec; # kein ':' -> kein Device
             if (z.index==startz.index) goto no_devicespec; # ':' am Anfang ist kein Device
             # Device-String bilden:
             { var reg4 uintL len = z.index - startz.index;
               device = allocate_string(len);
               # und füllen:
              {var reg2 uintB* ptr1 = &TheSstring(STACK_1)->data[startz.index];
               var reg3 uintB* ptr2 = &TheSstring(device)->data[0];
               #ifdef PATHNAME_VMS
               dotimesL(len,len, { *ptr2++ = up_case(*ptr1++); });
               #else
               dotimesL(len,len, { *ptr2++ = *ptr1++; });
               #endif
             }}
             #ifdef PATHNAME_AMIGAOS
             # Character ':' nicht übergehen; das ergibt dann :ABSOLUTE.
             #else # PATHNAME_VMS
             # Character ':' übergehen:
             z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
             #endif
             goto devicespec_ok;
             no_devicespec: # keine Device-Specification
               z = startz; # zum Start zurück
               device = NIL; # Device NIL
           }
           devicespec_ok:
           # Device eintragen:
           ThePathname(STACK_0)->pathname_device = device;
          }
         #endif
        #endif
        #ifndef HAVE_Startpoint_Cons # falls nicht oben schon erledigt
        # Directory-Start eintragen:
        { var reg1 object new_cons = allocate_cons(); # neues Cons für Startpoint
          ThePathname(STACK_0)->pathname_directory = new_cons;
          pushSTACK(new_cons); # neues (last (pathname-directory Pathname))
        }
        #endif
        # Stackaufbau: ..., Datenvektor, Pathname, (last (pathname-directory Pathname)).
        #ifndef PATHNAME_VMS
          # Subdirectories parsen:
          # Trennzeichen zwischen subdirs ist unter MSDOS sowohl '\' als auch '/':
          #ifdef PATHNAME_ATARI
           #define slashp(c)  ((c) == '\\')
          #endif
          #if defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2)
           #define slashp(c)  (((c) == '\\') || ((c) == '/'))
          #endif
          #if defined(PATHNAME_UNIX) || defined(PATHNAME_AMIGAOS)
           #define slashp(c)  ((c) == '/')
          #endif
          {
            #if defined(USER_HOMEDIR) && defined(PATHNAME_UNIX)
            # Falls sofort ein '~' kommt, wird bis zum nächsten '/' oder Stringende
            # ein Username gelesen und das Home-Directory dieses Users eingesetzt:
            if ((!(z.count == 0)) && (TheSstring(STACK_2)->data[z.index] == '~'))
              # Es kommt sofort ein '~'.
              { # Character übergehen:
                z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
               {var reg6 object userhomedir; # Pathname des User-Homedir
                # nächsten '/' suchen:
                var reg2 uintB* charptr = &TheSstring(STACK_2)->data[z.index];
                var reg3 uintL charcount = 0;
                { var reg4 uintL count;
                  dotimesL(count,z.count,
                    { if (*charptr++ == '/') break;
                      charcount++;
                    });
                }
                # Username hat charcount Zeichen
                if (charcount==0)
                  { userhomedir = O(user_homedir); } # nur '~' -> User-Homedir
                  else
                  { # Username als ASCIZ-String bauen:
                    var reg5 object username = allocate_string(charcount+1);
                    { var reg1 uintB* charptr2 = &TheSstring(username)->data[0];
                      var reg4 uintL count;
                      charptr = &TheSstring(STACK_2)->data[z.index];
                      dotimespL(count,charcount, { *charptr2++ = *charptr++; } );
                      *charptr2 = '\0';
                    }
                    # Dessen Home-Directory aus dem Passwort-File holen:
                    begin_system_call();
                    errno = 0;
                   {var reg1 struct passwd * userpasswd = getpwnam(TheAsciz(username));
                    end_system_call();
                    if (userpasswd == (struct passwd *)NULL) # erfolglos?
                      { if (!(errno==0)) { OS_error(); } # Error melden
                        # sonst: Fehler
                        pushSTACK(username);
                        pushSTACK(S(parse_namestring));
                        fehler(
                               DEUTSCH ? "~: Es gibt keinen Benutzer mit Namen ~." :
                               ENGLISH ? "~: there is no user named ~" :
                               FRANCAIS ? "~ : Il n'y a pas d'utilisateur de nom ~." :
                               ""
                              );
                      }
                    userhomedir = asciz_dir_to_pathname(userpasswd->pw_dir); # Homedir als Pathname
                  }}
                # Directory aus dem Pathname userhomedir kopieren:
                # (copy-list dir) = (nreconc (reverse dir) nil),
                # dabei dessen letztes Cons merken.
                userhomedir = reverse(ThePathname(userhomedir)->pathname_directory);
                STACK_0 = userhomedir; userhomedir = nreconc(userhomedir,NIL);
                ThePathname(STACK_1)->pathname_directory = userhomedir;
                # username-Characters übergehen:
                z.index += charcount; z.FNindex = fixnum_inc(z.FNindex,charcount); z.count -= charcount;
                # Falls der String zu Ende ist: fertig,
                # sonst kommt sofort ein '/', es wird übergangen:
                if (z.count==0)
                  { pushSTACK(NIL); pushSTACK(NIL); goto after_name_type; } # Name und Typ := NIL
                # Character übergehen:
                z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
              }}
            else
            #endif
            #if defined(PATHNAME_UNIX) || defined(PATHNAME_ATARI) || defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2)
            # Falls sofort ein '\' bzw. '/' kommt, wird er übergangen, und es kommt
            # :ABSOLUTE (sonst :RELATIVE) als erstes subdir:
            if ((!(z.count == 0)) && slashp(TheSstring(STACK_2)->data[z.index]))
              # Es kommt sofort ein '\' bzw. '/'.
              { # Character übergehen:
                z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                Car(STACK_0) = S(Kabsolute); # Startpoint = :ABSOLUTE
              }
              else
              # Es kommt nicht sofort ein '\' bzw. '/'.
              { Car(STACK_0) = S(Krelative); } # Startpoint = :RELATIVE
            #endif
            #ifdef PATHNAME_AMIGAOS
            # Falls sofort ein ':' kommt, wird er übergangen, und es kommt
            # :ABSOLUTE (sonst :RELATIVE) als erstes subdir:
            if ((!(z.count == 0)) && (TheSstring(STACK_2)->data[z.index] == ':'))
              # Es kommt sofort ein ':'.
              { # Character übergehen:
                z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                Car(STACK_0) = S(Kabsolute); # directory = (:ABSOLUTE)
              }
              else
              # Es kommt nicht sofort ein ':'.
              { Car(STACK_0) = S(Krelative); } # directory = (:RELATIVE)
            #endif
            loop
              { # Versuche, ein weiteres Unterdirectory zu parsen.
                #ifdef PATHNAME_EXT83
                  # Kommt '.\' oder '..\' oder '...\' ?
                  if ((!(z.count == 0)) && (TheSstring(STACK_2)->data[z.index] == '.'))
                    { # nächstes Character ist ein '.'.
                      var zustand subdirz = z; # Zustand beim Start des Subdirectories
                      # Character übergehen:
                      z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                      if (z.count == 0) goto no_dots; # String schon zu Ende ?
                     {var reg1 uintB ch = TheSstring(STACK_2)->data[z.index]; # nächstes Character
                      if (slashp(ch))
                        # '.\' angetroffen -> (cons :CURRENT NIL) bauen
                        { pushSTACK(S(Kcurrent)); goto dots; }
                      if (!(ch == '.')) goto no_dots;
                      # zweites Character war auch ein '.'.
                      # Character übergehen:
                      z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                      if (z.count == 0) goto no_dots; # String schon zu Ende ?
                      ch = TheSstring(STACK_2)->data[z.index]; # nächstes Character
                      if (slashp(ch))
                        # '..\' angetroffen -> (cons :PARENT NIL) bauen
                        { pushSTACK(S(Kparent)); goto dots; }
                      if (!(ch == '.')) goto no_dots;
                      # drittes Character war auch ein '.'.
                      # Character übergehen:
                      z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                      if (z.count == 0) goto no_dots; # String schon zu Ende ?
                      ch = TheSstring(STACK_2)->data[z.index]; # nächstes Character
                      if (slashp(ch))
                        # '...\' angetroffen -> (cons :WILD NIL) bauen
                        { pushSTACK(S(Kwild)); goto dots; }
                      goto no_dots;
                     }
                      dots:
                      # '.\' oder '..\' oder '...\' angetroffen, Keyword im Stack.
                      # Character '\' übergehen:
                      z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                      goto subdir_ok;
                      no_dots:
                      z = subdirz; # Zustand wiederherstellen
                    }
                  # Versuche, normale 'name.typ'-Syntax zu parsen:
                  pushSTACK(NIL); # dummy
                  { # Name, hat max. 8 Buchstaben:
                    var reg1 object name = parse_name_or_type(&z,8,NIL);
                    STACK_0 = name;
                  }
                  # Versuche, '.typ'-Syntax zu parsen:
                  { var reg1 object type;
                    if ((!(z.count==0)) && (TheSstring(STACK_3)->data[z.index] == '.'))
                      { # Es kommt ein '.'. Character übergehen:
                        z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                        # Typ, hat max. 3 Buchstaben:
                        type = parse_name_or_type(&z,3,O(leer_string));
                      }
                      else
                      { type = NIL; }
                    pushSTACK(type);
                  }
                  # Stackaufbau: ...,
                  #   Datenvektor, Pathname, (last (pathname-directory Pathname)),
                  #   name, type.
                  # Kommt sofort ein '\', so war es ein Unterdirectory,
                  # sonst ist der Pathname beendet:
                  if ((z.count==0) || !slashp(TheSstring(STACK_4)->data[z.index])) break;
                  # Es kommt ein '\'. Character übergehen:
                  z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                  # name=NIL -> durch "" ersetzen:
                  if (eq(STACK_1,NIL)) { STACK_1 = O(leer_string); }
                  # type=NIL -> durch "" ersetzen:
                  if (eq(STACK_0,NIL)) { STACK_0 = O(leer_string); }
                  { var reg1 object new_cons = allocate_cons(); # neues Cons
                    Cdr(new_cons) = popSTACK(); # type
                    Car(new_cons) = popSTACK(); # name
                    # new_cons = (cons name type)
                    pushSTACK(new_cons);
                  }
                  subdir_ok:
                #endif
                #ifdef PATHNAME_NOEXT
                  { var reg3 uintL z_start_index = z.index; # Index beim Start
                    loop
                      { var reg2 uintB ch;
                        if (z.count == 0) break;
                        ch = TheSstring(STACK_2)->data[z.index]; # nächstes Character
                        if (!legal_namechar(ch)) break; # gültiges Character ?
                        # ja -> Teil des Namens
                        # Character übergehen:
                        z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                      }
                    # Ende des Namens erreicht.
                    # Name := Teilstring von STACK_2 von z_start_index (einschließlich)
                    #                                bis z.index (ausschließlich).
                   {var reg3 uintL len = z.index - z_start_index;
                    var reg4 object string = allocate_string(len); # String der Länge len
                    # füllen:
                    var reg1 uintB* ptr1 = &TheSstring(STACK_2)->data[z_start_index];
                    var reg2 uintB* ptr2 = &TheSstring(string)->data[0];
                    dotimesL(len,len, { *ptr2++ = *ptr1++; });
                    # Name fertig.
                    pushSTACK(string);
                  }}
                  # Kommt sofort ein '/' bzw. '\', so war es ein Unterdirectory,
                  # sonst ist der Pathname beendet:
                  if ((z.count==0) || !slashp(TheSstring(STACK_3)->data[z.index]))
                    # Nein -> war der Name und kein Subdir.
                    break;
                  # Es kommt ein '/' bzw. '\'. Character übergehen:
                  z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                  # Stackaufbau: ...,
                  #   Datenvektor, Pathname, (last (pathname-directory Pathname)),
                  #   subdir.
                  #ifdef PATHNAME_AMIGAOS
                  # War es '' ?
                  if (equal(STACK_0,O(leer_string)))
                    { STACK_0 = S(Kparent); } # ja -> durch :PARENT ersetzen
                  else
                  #endif
                  # War es '**' oder '...' ?
                  if (equal(STACK_0,O(wildwild_string)) || equal(STACK_0,O(punktpunktpunkt_string)))
                    { STACK_0 = S(Kwild); } # ja -> durch :WILD ersetzen
                #endif
                # (pathname-directory pathname) um Subdir STACK_0 verlängern:
                { var reg1 object new_cons = allocate_cons(); # neues Cons
                  Car(new_cons) = popSTACK(); # = (cons subdir NIL)
                  Cdr(STACK_0) = new_cons; # verlängert (pathname-directory Pathname)
                  STACK_0 = new_cons; # neues (last (pathname-directory Pathname))
                }
              }
            #ifdef PATHNAME_EXT83
            # Stackaufbau: ...,
            #   Datenvektor, Pathname, (last (pathname-directory Pathname)),
            #   name, type.
            # Name und Typ in Pathname eintragen:
            { var reg3 object type = popSTACK();
              var reg2 object name = popSTACK();
              skipSTACK(1); # Directory ist schon eingetragen
             {var reg1 object pathname = STACK_0;
              ThePathname(pathname)->pathname_name = name;
              ThePathname(pathname)->pathname_type = type;
            }}
            #endif
            #ifdef PATHNAME_NOEXT
            # Stackaufbau: ..., Datenvektor, Pathname, (last (pathname-directory Pathname)),
            #              string.
            split_name_type(0); # String STACK_0 in Name und Typ aufspalten
            after_name_type:
            # Stackaufbau: ..., Datenvektor, Pathname, (last (pathname-directory Pathname)),
            #              name, type.
            # Name und Typ in Pathname eintragen:
            { var reg3 object type = popSTACK();
              var reg2 object name = popSTACK();
              skipSTACK(1); # Directory ist schon eingetragen
              # name="" durch Name=NIL ersetzen:
              if (equal(name,O(leer_string))) { name = NIL; }
             {var reg1 object pathname = STACK_0;
              ThePathname(pathname)->pathname_name = name;
              ThePathname(pathname)->pathname_type = type;
            }}
            #endif
          }
          #undef slashp
        #else # PATHNAME_VMS
          # Unter VMS gibt es keine "relativen" Pathnames: Überall, wo ein
          # Device festgelegt ist, liegt damit auch das Directory fest.
          Car(STACK_0) = (nullp(ThePathname(STACK_1)->pathname_device)
                          ? S(Krelative) # :RELATIVE nur falls Device nicht angegeben
                          : S(Kabsolute) # sonst immer :ABSOLUTE
                         );
          # Subdirectories parsen:
          loop
            { if (z.count == 0) break;
             {var reg4 uintB delimiter = TheSstring(STACK_2)->data[z.index];
              if (!((delimiter=='[') || (delimiter=='<'))) break;
              # Es kommt sofort ein '[' oder '<'.
              delimiter += 2; # Aus '[' mache ']', aus '<' mache '>'.
              # Character übergehen:
              z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
              # Kommt sofort ein Punkt?
              if ((!(z.count == 0)) && (TheSstring(STACK_2)->data[z.index] == '.'))
                { # ja -> aus einem absoluten Pathname wird ein relativer
                  if (eq(Car(STACK_0),S(Kabsolute))) { Car(STACK_0) = S(Krelative); }
                  # Character übergehen:
                  z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                }
              loop
                # Subdirectory-Namen parsen:
                { var reg3 uintL z_start_index = z.index; # Index beim Start
                  loop
                    { var reg2 uintB ch;
                      if (z.count == 0) break;
                      ch = TheSstring(STACK_2)->data[z.index]; # nächstes Character
                      if (!legal_namechar(ch)) break; # gültiges Character ?
                      # ja -> Teil des Namens
                      # Character übergehen:
                      z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                    }
                  # Ende des Namens erreicht.
                  # Name := Teilstring von STACK_2 von z_start_index (einschließlich)
                  #                                bis z.index (ausschließlich).
                  { var reg3 uintL len = z.index - z_start_index;
                    if (!(len==0)) # leere Subdirectory-Namen ignorieren
                      { if (only_dashes_p(&TheSstring(STACK_2)->data[z_start_index],len))
                          # [----] oder [.----] (n Bindestriche) bedeutet n mal :PARENT
                          { # len mal (pathname-directory pathname) um Subdir :PARENT verlängern:
                            dotimespL(len,len,
                              { var reg1 object new_cons = allocate_cons(); # neues Cons
                                Car(new_cons) = S(Kparent); # = (cons :PARENT NIL)
                                Cdr(STACK_0) = new_cons; # verlängert (pathname-directory Pathname)
                                STACK_0 = new_cons; # neues (last (pathname-directory Pathname))
                              });
                          }
                          else
                          { if ((len==2) && (TheSstring(STACK_2)->data[z_start_index]=='*')
                                         && (TheSstring(STACK_2)->data[z_start_index+1]=='*')
                               )
                              # [.**] bedeutet :WILD
                              { pushSTACK(S(Kwild)); }
                              else
                              { var reg4 object string = allocate_string(len); # String der Länge len
                                # füllen:
                                var reg1 uintB* ptr1 = &TheSstring(STACK_2)->data[z_start_index];
                                var reg2 uintB* ptr2 = &TheSstring(string)->data[0];
                                dotimesL(len,len, { *ptr2++ = up_case(*ptr1++); });
                                # Name fertig.
                                pushSTACK(string);
                              }
                            # (pathname-directory pathname) um Subdir STACK_0 verlängern:
                           {var reg1 object new_cons = allocate_cons(); # neues Cons
                            Car(new_cons) = popSTACK(); # = (cons subdir NIL)
                            Cdr(STACK_0) = new_cons; # verlängert (pathname-directory Pathname)
                            STACK_0 = new_cons; # neues (last (pathname-directory Pathname))
                          }}
                  }   }
                  # Kommt ein Punkt oder ein ']' bzw. '>'?
                  if (z.count == 0)
                    # Ist der String schon zu Ende, dann heucheln wir die schließende ']' bzw. '>':
                    break;
                  if (TheSstring(STACK_2)->data[z.index] == '.')
                    # Character übergehen:
                    { z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--; }
                  elif (TheSstring(STACK_2)->data[z.index] == delimiter)
                    # Character ']' bzw. '>' übergehen, Schleife beenden:
                    { z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                      break;
                    }
                }
            }
          # Normale 'name.typ'-Syntax parsen:
          pushSTACK(NIL); # dummy
          { # Name:
            var reg1 object name = parse_name_or_type(&z,NIL);
            STACK_0 = name;
          }
          # Folgende '.typ'-Syntax parsen:
          { var reg1 object type;
            if ((!(z.count==0)) && (TheSstring(STACK_3)->data[z.index] == '.'))
              { # Es kommt ein '.'. Character übergehen:
                z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                # Typ:
                type = parse_name_or_type(&z,O(leer_string));
              }
              else
              { type = NIL; }
            pushSTACK(type);
          }
          # Folgende '.version' oder ';version' Syntax parsen:
          { var reg4 object version;
            var zustand startz = z; # Start-Zustand
            if (!(z.count==0))
              { var reg3 uintB ch = TheSstring(STACK_4)->data[z.index];
                if ((ch=='.') || (ch==';'))
                  { z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                    # Versionsnummer (:WILD oder :NEWEST oder Fixnum > 0) parsen:
                    if (z.count==0) goto no_version;
                    ch = TheSstring(STACK_4)->data[z.index]; # nächstes Character
                    if (ch=='*')
                      { # Character übergehen:
                        z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                        version = S(Kwild);
                      }
                      else
                      { var reg2 uintL akku = 0; # Hilfsregister zum Aufbau der Versionsnummer
                        loop
                          { if (!((ch >= '0') && (ch <= '9'))) break; # Ziffer?
                            ch = ch - '0'; # Wert der Ziffer
                            akku = 10*akku+ch; # Versionsnummer um eine Ziffer erweitern
                            if (akku > (uintL)(bitm(oint_addr_len)-1))
                              goto no_version; # >=2^24 geworden -> ist wohl keine Versionsnummer
                            # Character übergehen:
                            z.index++; z.FNindex = fixnum_inc(z.FNindex,1); z.count--;
                            if (z.count==0) break;
                            ch = TheSstring(STACK_4)->data[z.index]; # nächstes Character
                          }
                        # Versionsnummer zu Ende
                        if (akku==0)
                          { version = S(Knewest); } # 0 steht für :NEWEST
                          else
                          { version = fixnum(akku); } # akku als Fixnum
                      }
                    goto version_ok;
              }   }}
            no_version:
            # Es ist nicht gelungen, eine Versions-Spezifikation zu parsen.
            z = startz; # Start-Zustand wiederherstellen
            version = NIL; # Version := NIL
            version_ok:
            # Stackaufbau: ...,
            #   Datenvektor, Pathname, (last (pathname-directory Pathname)),
            #   name, type.
            # Name, Typ und Version in Pathname eintragen:
            { var reg3 object type = popSTACK();
              var reg2 object name = popSTACK();
              skipSTACK(1); # Directory ist schon eingetragen
             {var reg1 object pathname = STACK_0;
              ThePathname(pathname)->pathname_name = name;
              ThePathname(pathname)->pathname_type = type;
              ThePathname(pathname)->pathname_version = version;
          } }}
        #endif
        # Pathname fertig.
        # Stackaufbau: ..., Datenvektor, Pathname.
        if (!junk_allowed)
          # Überprüfen, ob keine Zeichen mehr übrig sind:
          if (!(z.count == 0))
            { pushSTACK(z.FNindex); # letzter Index
              pushSTACK(STACK_(4+2+1)); # thing
              pushSTACK(S(parse_namestring));
              fehler(
                     DEUTSCH ? "~: Syntax Error im Dateinamen ~ an Position ~." :
                     ENGLISH ? "~: syntax error in filename ~ at position ~" :
                     FRANCAIS ? "~ : Erreur de syntaxe dans le nom de fichier ~, à la position ~." :
                     ""
                    );
            }
        value1 = STACK_0; # Pathname als 1. Wert
        value2 = z.FNindex; # Index als 2. Wert
        mv_count=2; # 2 Werte
        skipSTACK(5+2); return;
  } } }

#ifdef PATHNAME_ATARI

# Suchbuffer für GEMDOS:
  local DTA DTA_buffer;

# Es wird ein Default-Drive geführt: DEFAULT_DRIVE = O(default_drive).

# Die Variable *DEFAULT-PATHNAME-DEFAULTS* enthält (als Pathname) den
# Defaultwert für jede MERGE-Operation. Dies ist derjenige, den das System
# in vom Benutzer eingegebene Pathnames "hineininterpretiert".
# Er wird auf dem neuesten Stand des DEFAULT_DRIVE gehalten: bei der
# Initialisierung das aktuelle Device (im Sinne von GEMDOS), bei der
# Änderung von DEFAULT_DRIVE mittels CD.

# Es wird eine Liste aller angeschlossenen Laufwerke und der Default-
# Directories bei allen bekannten Disketten geführt. Aufbau:
# ( { (device . ( { (Seriennummer . default-directory) } ) ) } )
# also eine Aliste, die abbildet:
# device (String der Länge 1) -> Aliste (Seriennummer -> default-directory),
# wobei die default-directories Pathnames sind, bei denen nur Device und
# Directory gefüllt sind, keine Wildcards vorkommen und das Directory
# nur subdirs der Form (name . type) enthält.
# Variable DRIVE_ALIST = O(drive_alist).

#endif # PATHNAME_ATARI

#if defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2)

# Das Betriebssystem verwaltet ein Default-Drive.
#ifdef DJUNIX
# Es kann leider nicht verändert und nur mit Mühe abgefragt werden.
#endif
# Das Betriebssystem verwaltet auf jedem Drive ein Default-Directory. Dieses
# kann sich allerdings ändern, wenn eine andere Diskette eingelegt wird.

# Es wird ein Default-Drive geführt: DEFAULT_DRIVE = O(default_drive).

# Die Variable *DEFAULT-PATHNAME-DEFAULTS* enthält (als Pathname) den
# Defaultwert für jede MERGE-Operation. Dies ist derjenige, den das System
# in vom Benutzer eingegebene Pathnames "hineininterpretiert".
# Er wird auf dem neuesten Stand des DEFAULT_DRIVE gehalten: bei der
# Initialisierung das aktuelle Device (im Sinne von DOS), bei der
# Änderung von DEFAULT_DRIVE mittels CD.

#endif # PATHNAME_MSDOS || PATHNAME_OS2

#if defined(PATHNAME_UNIX) || defined(PATHNAME_AMIGAOS) || defined(PATHNAME_VMS)

# Die Variable *DEFAULT-PATHNAME-DEFAULTS* enthält (als Pathname) den
# Defaultwert für jede MERGE-Operation. Dies ist derjenige, den das System
# in vom Benutzer eingegebene Pathnames "hineininterpretiert".

#endif

#if defined(UNIX) || defined(VMS)

# Das Betriebssystem verwaltet ein Default-Directory ("working directory")
# für diesen Prozeß. Es kann mit chdir verändert und mit getwd abgefragt
# werden. Siehe CHDIR(2) und GETWD(3).

#endif

#ifdef AMIGAOS

# Das Betriebssystem verwaltet ein Default-Directory ("current directory")
# für diesen Prozeß. Es kann mit CurrentDir verändert und mit einer
# Kombination aus Examine und ParentDir abgefragt werden.

#endif

# UP: Wandelt ein Objekt in einen Pathname um.
# coerce_pathname(object)
# > object: Objekt
# < ergebnis: (PATHNAME Objekt)
# kann GC auslösen
  local object coerce_pathname (object obj);
  local object coerce_pathname(obj)
    var reg1 object obj;
    { if (pathnamep(obj))
        # Bei Pathnames ist nichts zu tun.
        { return obj; }
        else
        # sonst: PARSE-NAMESTRING aufrufen:
        { pushSTACK(subr_self); # subr_self retten (für spätere Fehlermeldungen)
          pushSTACK(obj); funcall(L(parse_namestring),1);
          subr_self = popSTACK();
          return value1;
        }
    }

# UP: Wandelt ein Argument in einen Pathname um.
# coerce_pathname_arg()
# > STACK_0: Objekt
# < ergebnis: (PATHNAME Objekt)
# Erhöht STACK um 1.
# kann GC auslösen
  # local object coerce_pathname_arg (void);
  # local object coerce_pathname_arg()
  #   { return coerce_pathname(popSTACK()); }
  #define coerce_pathname_arg()  coerce_pathname(popSTACK())

LISPFUNN(pathname,1)
# (PATHNAME pathname), CLTL S. 413
  { value1 = coerce_pathname_arg(); mv_count=1; }

LISPFUNN(pathnamehost,1)
# (PATHNAME-HOST pathname), CLTL S. 417
  { var reg1 object pathname = coerce_pathname_arg();
    #if HAS_HOST
    value1 = ThePathname(pathname)->pathname_host; mv_count=1; # host als Wert
    #else
    value1 = NIL; mv_count=1; # NIL als Wert
    #endif
  }

LISPFUNN(pathnamedevice,1)
# (PATHNAME-DEVICE pathname), CLTL S. 417
  { var reg1 object pathname = coerce_pathname_arg();
    #if HAS_DEVICE
    value1 = ThePathname(pathname)->pathname_device; mv_count=1; # device als Wert
    #else
    value1 = NIL; mv_count=1; # NIL als Wert
    #endif
  }

LISPFUNN(pathnamedirectory,1)
# (PATHNAME-DIRECTORY pathname), CLTL S. 417
  { var reg1 object pathname = coerce_pathname_arg();
    value1 = ThePathname(pathname)->pathname_directory; mv_count=1; # directory als Wert
  }

LISPFUNN(pathnamename,1)
# (PATHNAME-NAME pathname), CLTL S. 417
  { var reg1 object pathname = coerce_pathname_arg();
    value1 = ThePathname(pathname)->pathname_name; mv_count=1; # name als Wert
  }

LISPFUNN(pathnametype,1)
# (PATHNAME-TYPE pathname), CLTL S. 417
  { var reg1 object pathname = coerce_pathname_arg();
    value1 = ThePathname(pathname)->pathname_type; mv_count=1; # type als Wert
  }

LISPFUNN(pathnameversion,1)
# (PATHNAME-VERSION pathname), CLTL S. 417
  { var reg1 object pathname = coerce_pathname_arg();
    #if HAS_VERSION
    value1 = ThePathname(pathname)->pathname_version; mv_count=1; # version als Wert
    #else
    value1 = NIL; mv_count=1; # NIL als Wert
    #endif
  }

# UP: Legt Teilstrings für STRING_CONCAT auf den STACK, die zusammen den
# String für ein Subdirectory (car path) ergeben.
# subdir_namestring_parts(path)
# > path: ein Cons
# < ergebnis: Anzahl der auf den Stack gelegten Strings
# verändert STACK
  local uintC subdir_namestring_parts (object path);
  local uintC subdir_namestring_parts(path)
    var reg4 object path;
    { var reg1 object subdir = Car(path);
      #if defined(PATHNAME_ATARI) || defined(PATHNAME_MSDOS)
      if (eq(subdir,S(Kcurrent))) # :CURRENT ?
        { pushSTACK(O(punkt_string)); return 1; }
      elif (eq(subdir,S(Kparent))) # :PARENT ?
        { pushSTACK(O(punktpunkt_string)); return 1; }
      elif (eq(subdir,S(Kwild))) # :WILD ?
        { pushSTACK(O(punktpunktpunkt_string)); return 1; }
      else
        # normales subdir (name . type)
        { var reg3 object name = Car(subdir);
          var reg2 object type = Cdr(subdir);
          # name = :WILD -> String "*"
          if (eq(name,S(Kwild))) { name = O(wild_string); }
          pushSTACK(name);
          # type = :WILD -> String "*"
          if (eq(type,S(Kwild))) { type = O(wild_string); }
          if (TheSstring(type)->length == 0)
            # type = "" -> nicht auszugeben
            { return 1+0; }
            else
            { pushSTACK(O(punkt_string)); # "."
              pushSTACK(type);
              return 1+2;
            }
        }
      #endif
      #ifdef PATHNAME_AMIGAOS
      if (eq(subdir,S(Kparent))) # :PARENT ?
        { return 0; } # Leerstring
      elif (eq(subdir,S(Kwild))) # :WILD ?
        { pushSTACK(O(wildwild_string)); return 1; }
      else
        # normales subdir
        { pushSTACK(subdir); return 1; }
      #endif
      #if defined(PATHNAME_UNIX) || defined(PATHNAME_OS2)
      if (eq(subdir,S(Kwild))) # :WILD ?
        { pushSTACK(O(wildwild_string)); return 1; }
        else
        # normales subdir
        { pushSTACK(subdir); return 1; }
      #endif
      #ifdef PATHNAME_VMS
      if (eq(subdir,S(Kwild))) # :WILD ?
        { pushSTACK(O(wildwild_string)); return 1; }
      elif (eq(subdir,S(Kparent))) # :PARENT ?
        { pushSTACK(O(parent_string)); return 1; }
      else
        # normales subdir
        { pushSTACK(subdir); return 1; }
      #endif
    }

# UP: Legt Teilstrings für STRING_CONCAT auf den STACK, die zusammen den
# String für den Host des Pathname pathname ergeben.
# host_namestring_parts(pathname)
# > pathname: Pathname
# < ergebnis: Anzahl der auf den Stack gelegten Strings
# verändert STACK
#if HAS_HOST
  local uintC host_namestring_parts (object pathname);
  local uintC host_namestring_parts(pathname)
    var reg1 object pathname;
    { var reg2 object host = ThePathname(pathname)->pathname_host;
      if (nullp(host))
        { return 0; } # kein String
        else
        { pushSTACK(host);
          #ifdef PATHNAME_VMS
          pushSTACK(O(zweidoppelpunkt_string)); # "::"
          #else
          pushSTACK(O(doppelpunkt_string)); # ":"
          #endif
          return 2;
    }   }
#else
  #define host_namestring_parts(pathname)  (pathname,0)  # keine Strings
#endif

# UP: Legt Teilstrings für STRING_CONCAT auf den STACK, die zusammen den
# String fürs Device und Directory des Pathname pathname ergeben.
#if HAS_SERNR
# directory_namestring_parts(pathname,skipSN)
# > skipSN: Flag, ob Seriennummer unterdrückt werden soll
# > pathname: Pathname
# < ergebnis: Anzahl der auf den Stack gelegten Strings
# falls skipSN: verändert STACK
# falls !skipSN: verändert STACK, kann GC auslösen
#else
# directory_namestring_parts(pathname)
# > pathname: Pathname
# < ergebnis: Anzahl der auf den Stack gelegten Strings
# verändert STACK
#endif
  #if HAS_SERNR
  local uintC directory_namestring_parts (object pathname, boolean skipSN);
  #define directory_namestring_parts_(pathname)  \
    directory_namestring_parts(pathname,TRUE)
  local uintC directory_namestring_parts(pathname,skipSN)
    var reg4 object pathname;
    var reg5 boolean skipSN;
  #else
  local uintC directory_namestring_parts (object pathname);
  #define directory_namestring_parts_(pathname)  \
    directory_namestring_parts(pathname)
  local uintC directory_namestring_parts(pathname)
    var reg4 object pathname;
  #endif
    { var reg3 uintC stringcount = 0; # bisherige Stringzahl = 0
      #ifdef PATHNAME_VMS
        # Device:
        { var reg1 object device = ThePathname(pathname)->pathname_device;
          if (!(nullp(device))) # NIL -> kein String
            { pushSTACK(device); stringcount++; # Device auf den Stack
              pushSTACK(O(doppelpunkt_string)); stringcount++; # ":" auf den Stack
        }   }
        # Directory:
        { var reg2 object directory = ThePathname(pathname)->pathname_directory;
          # Ist das erste subdir = :ABSOLUTE oder = :RELATIVE ?
          var reg3 boolean skipdot = eq(Car(directory),S(Kabsolute));
          directory = Cdr(directory); # übergehen
          # weitere subdirs auf den Stack:
          if (consp(directory))
            { pushSTACK(O(bracket1_string)); stringcount++; # "[" auf den Stack
              do { if (!skipdot)
                     { pushSTACK(O(punkt_string)); stringcount++; } # "." auf den Stack
                   stringcount += subdir_namestring_parts(directory);
                   skipdot = FALSE;
                   directory = Cdr(directory);
                 }
                 while (consp(directory));
              pushSTACK(O(bracket2_string)); stringcount++; # "]" auf den Stack
            }
        }
      #else
        #if defined(PATHNAME_ATARI) || defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2)
        # Device:
        { var reg1 object device = ThePathname(pathname)->pathname_device;
          if (!(nullp(device))) # NIL -> kein String
            { if (eq(device,S(Kwild))) { device = O(wild_string); } # :WILD -> String "*"
              pushSTACK(device); # Device auf den Stack
              stringcount++; # und mitzählen
        }   }
        #endif
        #ifdef PATHNAME_AMIGAOS
        # Device:
        { var reg1 object device = ThePathname(pathname)->pathname_device;
          if (!(nullp(device))) # NIL -> kein String
            { pushSTACK(device); # Device auf den Stack
              stringcount += 1; # und mitzählen
              # Wegen :ABSOLUTE kommt gleich danach ein ":" auf den Stack.
        }   }
        #endif
        # Directory:
        { var reg2 object directory = ThePathname(pathname)->pathname_directory;
          #if defined(PATHNAME_ATARI) || defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2)
          #if HAS_SERNR
          if (!skipSN)
            { var reg1 object seriennummer = Car(directory);
              if (!nullp(seriennummer))
                { # Seriennummer in dezimal umwandeln:
                  pushSTACK(directory); # directory retten
                  # (WRITE-TO-STRING Seriennummer :BASE 10 :RADIX NIL) ausführen:
                  pushSTACK(seriennummer); # 1. Argument
                  apply(L(write_to_string),1,O(base10_radixnil));
                  directory = popSTACK();
                  pushSTACK(value1); # Ergebnis-String in den Stack
                  stringcount++; # und mitzählen
            }   }
          directory = Cdr(directory); # restliche subdirs
          #endif
          # evtl. Doppelpunkt:
          if (!(stringcount == 0)) # nur falls schon was auf dem Stack
            { pushSTACK(O(doppelpunkt_string)); stringcount++; } # ":" auf den Stack
          #endif
          # Ist das erste subdir = :ABSOLUTE oder = :RELATIVE ?
          if (eq(Car(directory),S(Kabsolute)))
            #if defined(PATHNAME_ATARI) || defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2)
            { pushSTACK(O(backslash_string)); stringcount++; } # "\\" auf den Stack
            #endif
            #ifdef PATHNAME_AMIGAOS
            { pushSTACK(O(doppelpunkt_string)); stringcount++; } # ":" auf den Stack
            #endif
            #ifdef PATHNAME_UNIX
            { pushSTACK(O(slash_string)); stringcount++; } # "/" auf den Stack
            #endif
          directory = Cdr(directory); # übergehen
          # weitere subdirs auf den Stack:
          while (consp(directory))
            { stringcount += subdir_namestring_parts(directory);
              #if defined(PATHNAME_ATARI) || defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2)
              pushSTACK(O(backslash_string)); stringcount++; # "\\" auf den Stack
              #endif
              #if defined(PATHNAME_UNIX) || defined(PATHNAME_AMIGAOS)
              pushSTACK(O(slash_string)); stringcount++; # "/" auf den Stack
              #endif
              directory = Cdr(directory);
            }
        }
      #endif
      return stringcount;
    }

# UP: Legt Teilstrings für STRING_CONCAT auf den STACK, die zusammen den
# String für Name und Typ des Pathname ergeben.
# nametype_namestring_parts(name,type,version)
# > name, type, evtl. version: Komponenten des Pathname
# < ergebnis: Anzahl der auf den Stack gelegten Strings
# kann GC auslösen
# verändert STACK
  #if HAS_VERSION
  local uintC nametype_namestring_parts (object name, object type, object version);
  local uintC nametype_namestring_parts(name,type,version)
    var reg2 object name;
    var reg1 object type;
    var reg4 object version;
  #else
  local uintC nametype_namestring_parts (object name, object type);
  local uintC nametype_namestring_parts(name,type)
    var reg2 object name;
    var reg1 object type;
  #define nametype_namestring_parts(n,t,v)  (nametype_namestring_parts)(n,t)
  #endif
    { var reg3 uintC stringcount = 0;
      # Name:
      if (!nullp(name)) # name=NIL -> nicht ausgeben
        {
          #if defined(PATHNAME_ATARI) || defined(PATHNAME_MSDOS)
          if (eq(name,S(Kwild))) { name = O(wild_string); } # :WILD -> String "*"
          #endif
          pushSTACK(name); # Name auf den Stack
          stringcount++; # und mitzählen
        }
      # Typ:
      if (!nullp(type)) # type=NIL -> nicht ausgeben
        { pushSTACK(O(punkt_string)); # "." auf den Stack
          stringcount++; # und mitzählen
          #if defined(PATHNAME_ATARI) || defined(PATHNAME_MSDOS)
          if (eq(type,S(Kwild))) { type = O(wild_string); } # :WILD -> String "*"
          #endif
          pushSTACK(type); # Typ auf den Stack
          stringcount++; # und mitzählen
        }
      #if HAS_VERSION
      if (!nullp(version)) # version=NIL -> nicht ausgeben
        { pushSTACK(O(strichpunkt_string)); # ";" auf den Stack
          stringcount++; # und mitzählen
          #if defined(PATHNAME_VMS)
          if (eq(version,S(Kwild)))
            { pushSTACK(O(wild_string)); } # :WILD -> String "*"
            else
          #endif
          if (eq(version,S(Knewest)))
            { pushSTACK(O(zero_string)); } # :NEWEST -> String "0"
            else
          # Version (Integer >0) in String umwandeln: (sys::decimal-string version)
          { pushSTACK(version);
            C_decimal_string(); # == funcall(L(decimal_string),1);
            pushSTACK(value1);
          }
          stringcount++; # und mitzählen
        }
      #endif
      return stringcount;
    }

# UP: Legt Teilstrings für STRING_CONCAT auf den STACK, die zusammen den
# String für Name und Typ des Pathname ergeben.
# file_namestring_parts(pathname)
# > pathname: Pathname
# < ergebnis: Anzahl der auf den Stack gelegten Strings
# kann GC auslösen
# verändert STACK
  local uintC file_namestring_parts (object pathname);
  local uintC file_namestring_parts(pathname)
    var reg1 object pathname;
    { return nametype_namestring_parts(ThePathname(pathname)->pathname_name,
                                       ThePathname(pathname)->pathname_type,
                                       ThePathname(pathname)->pathname_version);
    }

# UP: Wandelt Pathname in String um.
# whole_namestring(pathname)
# > pathname: Pathname
# < ergebnis: Simple-String
# kann GC auslösen
  local object whole_namestring (object pathname);
  local object whole_namestring(pathname)
    var reg1 object pathname;
    { var reg2 uintC stringcount;
      #if HAS_SERNR
      pushSTACK(pathname); # pathname retten
      {var reg3 object* pathname_ = &STACK_0;
       stringcount = host_namestring_parts(pathname); # Strings für den Host
       stringcount += directory_namestring_parts(pathname,FALSE); # Strings fürs Directory
       pathname = *pathname_;
      }
      #else
      stringcount = host_namestring_parts(pathname); # Strings für den Host
      stringcount += directory_namestring_parts(pathname); # Strings fürs Directory
      #endif
      stringcount += file_namestring_parts(pathname); # Strings für den Filename
      subr_self = L(namestring); # ("aktuelles" SUBR für Fehlermeldung)
     {var reg3 object ergebnis = string_concat(stringcount); # zusammenhängen
      #if HAS_SERNR
      skipSTACK(1); # pathname wieder vergessen
      #endif
      return ergebnis;
    }}

LISPFUNN(file_namestring,1)
# (FILE-NAMESTRING pathname), CLTL S. 417
  { var reg1 object pathname = coerce_pathname_arg();
    var reg2 uintC stringcount = file_namestring_parts(pathname); # Strings für den Filename
    value1 = string_concat(stringcount); mv_count=1; # zusammenhängen
  }

# UP: Liefert den String zum Directory eines Pathname.
# directory_namestring(pathname)
# > pathname: Pathname
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: Simple-String
# kann GC auslösen
  local object directory_namestring (object pathname);
  local object directory_namestring(pathname)
    var reg1 object pathname;
    { var reg2 uintC stringcount =
        #if HAS_SERNR
        directory_namestring_parts(pathname,FALSE); # Strings fürs Directory
        #else
        directory_namestring_parts(pathname); # Strings fürs Directory
        #endif
      return string_concat(stringcount); # zusammenhängen
    }

LISPFUNN(directory_namestring,1)
# (DIRECTORY-NAMESTRING pathname), CLTL S. 417
  { var reg1 object pathname = coerce_pathname_arg();
    value1 = directory_namestring(pathname); mv_count=1;
  }

LISPFUNN(host_namestring,1)
# (HOST-NAMESTRING pathname), CLTL S. 417
  { var reg1 object pathname = coerce_pathname_arg();
    #if HAS_HOST
    value1 = host_namestring(pathname); mv_count=1;
    #else
    value1 = O(leer_string); mv_count=1; # "" als Wert
    #endif
  }

#if HAS_VERSION
# UP: Überprüft ein optionales VERSION-Argument.
# test_optional_version(def);
# > STACK_0: VERSION-Argument
# > def: Defaultwert dafür
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: gültige Version-Komponente
  local object test_optional_version (object def);
  local object test_optional_version(def)
    var reg2 object def;
    { var reg1 object version = STACK_0;
      if (eq(version,unbound)) { return def; } # nicht angegeben -> Default
      elif (nullp(version)) {} # NIL ist OK
      elif (eq(version,S(Kwild))) {} # :WILD ist OK
      elif (eq(version,S(Knewest))) {} # :NEWEST ist OK
      elif (posfixnump(version) && !eq(version,Fixnum_0)) {} # Fixnum >0 ist OK
      elif (pathnamep(version)) # Pathname -> dessen Version
        { STACK_0 = ThePathname(version)->pathname_version; }
      else # Keiner der gewünschten Fälle -> Fehler:
        { pushSTACK(version);
          pushSTACK(TheSubr(subr_self)->name);
          fehler(
                 DEUTSCH ? "~: :VERSION-Argument muß NIL oder ein Fixnum >0 oder :WILD oder :NEWEST sein, nicht ~" :
                 ENGLISH ? "~: :VERSION-argument should be NIL or a positive fixnum or :WILD or :NEWEST, not ~" :
                 FRANCAIS ? "~ : L'argument pour :VERSION doit être NIL, un petit nombre entier positif, :WILD ou :NEWEST mais non ~" :
                 ""
                );
        }
      return version;
    }
#else
# UP: Überprüft ein optionales VERSION-Argument.
# test_optional_version();
# > STACK_0: VERSION-Argument
# > subr_self: Aufrufer (ein SUBR)
# Erhöht STACK um 1.
# verändert STACK
  local void test_optional_version (void);
  local void test_optional_version()
    { var reg1 object version = popSTACK();
      if (eq(version,unbound) # nicht angegeben?
          || nullp(version)         # oder NIL ?
          || eq(version,S(Kwild))   # oder :WILD ?
          || eq(version,S(Knewest)) # oder :NEWEST ?
         )
        { return; } # ja -> OK
        else
        { pushSTACK(version);
          pushSTACK(TheSubr(subr_self)->name);
          fehler(
                 DEUTSCH ? "~: :VERSION-Argument muß NIL oder :WILD oder :NEWEST sein, nicht ~" :
                 ENGLISH ? "~: :VERSION-argument should be NIL or :WILD or :NEWEST, not ~" :
                 FRANCAIS ? "~ : L'argument pour :VERSION doit être NIL, :WILD ou :NEWEST mais non ~" :
                 ""
                );
    }   }
#endif

# UP: Neuberechnung von *DEFAULT-PATHNAME-DEFAULTS*
#if defined(PATHNAME_ATARI) || defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2)
# aus DEFAULT_DRIVE
#endif
# recalc_defaults_pathname();
# < ergebnis: Wert von *DEFAULT-PATHNAME-DEFAULTS*, ein Pathname
# kann GC auslösen
  local object recalc_defaults_pathname (void);
  local object recalc_defaults_pathname()
    {
      #if defined(PATHNAME_ATARI) || defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2)
      # (MAKE-PATHNAME :DEVICE default-drive) ausführen:
      pushSTACK(S(Kdevice)); pushSTACK(O(default_drive));
      funcall(L(make_pathname),2);
      #endif
      #if defined(PATHNAME_UNIX) || defined(PATHNAME_AMIGAOS) || defined(PATHNAME_VMS)
      # (MAKE-PATHNAME) ausführen:
      funcall(L(make_pathname),0);
      #endif
      # und *DEFAULT-PATHNAME-DEFAULTS* zuweisen:
      return Symbol_value(S(default_pathname_defaults)) = value1;
    }

# UP: Liefert den Default-Pathname.
# defaults_pathname()
# < ergebnis: Wert von *DEFAULT-PATHNAME-DEFAULTS*, ein Pathname
# kann GC auslösen
  local object defaults_pathname (void);
  local object defaults_pathname()
    { var reg1 object pathname = Symbol_value(S(default_pathname_defaults)); # Wert von *DEFAULT-PATHNAME-DEFAULTS*
      if (pathnamep(pathname))
        # ist Pathname -> OK
        { return pathname; }
        else
        # sonst Warnung:
        { # (WARN "Der Wert von ~S war kein Pathname. ~:*~S wird zurückgesetzt." ...)
          pushSTACK(O(defaults_warn_string));
          pushSTACK(S(default_pathname_defaults));
          funcall(S(warn),2);
          # und neuberechnen:
          return recalc_defaults_pathname();
    }   }

# UP: Überprüft ein Pathname-Argument und ein optionales DEFAULTS-Argument
# und alloziert einen neuen Pathname.
# > STACK_1 : PATHNAME-Argument
# > STACK_0 : DEFAULTS-Argument
# < STACK_1 : PATHNAME-Argument als Pathname
# < STACK_0 : DEFAULTS-Argument als Pathname
# < ergebnis : neuer Pathname
# kann GC auslösen
  local object prepare_for_merge (void);
  local object prepare_for_merge()
    # Stackaufbau: pathname, defaults.
    { # pathname zu einem Pathname machen:
      STACK_1 = coerce_pathname(STACK_1);
      # defaults zu einem Pathname machen:
      { var reg1 object defaults = STACK_0;
        if (!eq(defaults,unbound))
          # angegeben -> zu einem Pathname machen:
          { defaults = coerce_pathname(defaults); }
          else
          # nicht angegeben -> Wert von *DEFAULT-PATHNAME-DEFAULTS* nehmen:
          { defaults = defaults_pathname(); }
        STACK_0 = defaults;
      }
      # neuen Pathname holen:
      return allocate_pathname();
    }

LISPFUN(merge_pathnames,1,2,norest,nokey,0,NIL)
# (MERGE-PATHNAMES pathname [defaults [default-version]]), CLTL S. 415
# (defun merge-pathnames (pathname &optional (defaults *default-pathname-defaults*) default-version)
#   (setq pathname (pathname pathname))
#   (setq defaults (pathname defaults))
#   (multiple-value-call #'make-pathname
#if HAS_HOST
#     (if (or (equal (pathname-host pathname) (pathname-host defaults))
#             (null (pathname-host pathname))
#         )
#       (values
#         :host (or (pathname-host pathname) (pathname-host defaults))
#endif
#if HAS_DEVICE
#     (if (or (equal (pathname-device pathname) (pathname-device defaults))
#             (null (pathname-device pathname))
#         )
#       (values
#         :device (or (pathname-device pathname) (pathname-device defaults))
#endif
#         :directory
#           (let ((pathname-dir (pathname-directory pathname))
#                 (defaults-dir (pathname-directory defaults)))
#if HAS_SERNR
#             (if (or (eql (car pathname-dir) (car defaults-dir))
#                     (null (car pathname-dir))
#                 )
#               (cons (or (car pathname-dir) (car defaults-dir))
#                 (progn (pop pathname-dir) (pop defaults-dir)
#endif
#                   (if (eq (car pathname-dir) ':RELATIVE)
#                     (cond ((null (cdr pathname-dir)) defaults-dir)
#                           ((not (eq (car defaults-dir) ':RELATIVE))
#                            (append defaults-dir (cdr pathname-dir))
#                           )
#                           (t pathname-dir)
#                     )
#                     pathname-dir
#                   )
#if HAS_SERNR
#               ) )
#               pathname-dir
#             )
#endif
#           )
#       )
#       (values
#if HAS_HOST
#         :host (pathname-host pathname)
#endif
#if HAS_DEVICE
#         :device (pathname-device pathname)
#endif
#         :directory (pathname-directory pathname)
#     ) )
#     :name (or (pathname-name pathname) (pathname-name defaults))
#     :type (or (pathname-type pathname) (pathname-type defaults))
# ) )
  { # default-version überprüfen:
    #if HAS_VERSION
    {var reg9 object v = test_optional_version(S(Knewest)); # Default ist :NEWEST
     STACK_0 = STACK_1; STACK_1 = STACK_2; STACK_2 = v;
    }# Stackaufbau: default-version, pathname, defaults.
    #else
     test_optional_version();
     # Stackaufbau: pathname, defaults.
    #endif
   {# pathname und defaults überprüfen:
    var reg6 object new = prepare_for_merge();
    var reg8 object d = popSTACK(); # defaults
    var reg7 object p = popSTACK(); # pathname
    #if HAS_HOST
    # Hosts matchen:
    { var reg1 object p_host = ThePathname(p)->pathname_host;
      var reg2 object d_host = ThePathname(d)->pathname_host;
      ThePathname(new)->pathname_host = p_host; # erstmal new-host := pathname-host
      # beide Hosts gleich -> Devices matchen:
      if (equal(p_host,d_host)) goto match_devices;
      if (nullp(p_host))
        { # pathname-host nicht angegeben, aber defaults-host angegeben:
          ThePathname(new)->pathname_host = d_host; # new-host := defaults-host
          goto match_devices;
        }
      goto notmatch_devices;
    }
    #endif
    match_devices:
    #if HAS_DEVICE
    # Devices matchen:
    { var reg1 object p_device = ThePathname(p)->pathname_device;
      var reg2 object d_device = ThePathname(d)->pathname_device;
      ThePathname(new)->pathname_device = p_device; # erstmal new-device := pathname-device
      # beide Devices gleich -> Directories matchen:
      if (equal(p_device,d_device)) goto match_directories;
      if (nullp(p_device))
        { # pathname-device nicht angegeben, aber defaults-device angegeben:
          ThePathname(new)->pathname_device = d_device; # new-device := defaults-device
          goto match_directories;
        }
      goto notmatch_directories;
    }
    #endif
    # Directories matchen:
    match_directories:
    { var reg2 object p_directory = ThePathname(p)->pathname_directory; # pathname-directory
      var reg3 object d_directory = ThePathname(d)->pathname_directory; # defaults-directory
      #if HAS_SERNR
      var reg5 object new_seriennummer = Car(p_directory); # pathname-Seriennummer
      # beide Seriennummern gleich -> Subdirectories matchen:
      if (eq(Car(p_directory),Car(d_directory))) goto match_subdirs;
      if (nullp(Car(p_directory)))
        { # pathname-Seriennummer nicht angegeben, aber defaults-Seriennummer angegeben:
          new_seriennummer = Car(d_directory); # new-Seriennummer := defaults-Seriennummer
          goto match_subdirs;
        }
      goto notmatch_directories;
      # Subdirectories matchen:
      match_subdirs:
      p_directory = Cdr(p_directory); # pathname-subdirs = (cdr pathname-directory)
      d_directory = Cdr(d_directory); # defaults-subdirs = (cdr defaults-directory)
      #endif
     {var reg4 object new_subdirs = p_directory;
      # Fängt pathname-subdirs mit :RELATIVE an?
      if (eq(Car(p_directory),S(Krelative)))
        # ja.
        { # Endet pathname-subdirs danach?
          if (matomp(Cdr(p_directory)))
            # ja -> verwende defaults-subdirs:
            { new_subdirs = d_directory; }
            else
            # nein.
            { # Fängt defaults-subdirs mit :RELATIVE an?
              if (eq(Car(d_directory),S(Krelative)))
                # ja -> Ersetzen von :RELATIVE in pathname-subdirs
                # durch das gesamte defaults-subdirs ist nicht sinnvoll
                # (da nicht klar ist, auf was das dabei entstehende
                # Default-Directory sich beziehen soll). Daher nichts tun:
                {}
                else
                # nein -> Um :RELATIVE aufzulösen: ersetze :RELATIVE
                # in pathname-subdirs durch defaults-subdirs, d.h.
                # bilde (append defaults-subdirs (cdr pathname-subdirs)) =
                # (nreconc (reverse defaults-subdirs) (cdr pathname-subdirs)) :
                { pushSTACK(p); pushSTACK(d); pushSTACK(new);
                  #if HAS_SERNR
                  pushSTACK(new_seriennummer);
                  #endif
                  pushSTACK(Cdr(p_directory));
                  {var reg1 object temp = reverse(d_directory);
                   new_subdirs = nreconc(temp,popSTACK());
                  }
                  #if HAS_SERNR
                  new_seriennummer = popSTACK();
                  #endif
                  new = popSTACK(); d = popSTACK(); p = popSTACK();
                }
        }   }
      #if HAS_SERNR
      # new-directory aus new-Seriennummer und new-subdirs zusammensetzen:
      { pushSTACK(p); pushSTACK(d); pushSTACK(new);
        pushSTACK(new_seriennummer); pushSTACK(new_subdirs);
       {var reg1 object new_cons = allocate_cons(); # neues Cons
        Cdr(new_cons) = popSTACK(); Car(new_cons) = popSTACK();
        # new_cons = (cons new-Seriennummer new-subdirs)
        new = popSTACK(); d = popSTACK(); p = popSTACK();
        new_subdirs = new_cons;
      }}
      #endif
      ThePathname(new)->pathname_directory = new_subdirs; # new-directory := new-subdirs
    }}
    goto directories_OK;
    # Devices nicht matchen:
    notmatch_devices:
    #if HAS_DEVICE
    { # new-device := pathname-device :
      ThePathname(new)->pathname_device = ThePathname(p)->pathname_device;
    }
    #endif
    # Directories nicht matchen:
    notmatch_directories:
    { # new-directory := pathname-directory :
      ThePathname(new)->pathname_directory = ThePathname(p)->pathname_directory;
    }
    directories_OK:
    # Nun sind die Directories OK.
    # Name matchen:
    # Verwende pathname-name, falls angegeben, und defaults-name sonst.
    { var reg1 object p_name = ThePathname(p)->pathname_name;
      ThePathname(new)->pathname_name =
        (!nullp(p_name) ? p_name : ThePathname(d)->pathname_name);
    }
    # Typ matchen:
    # Verwende pathname-type, falls angegeben, und defaults-type sonst.
    { var reg1 object p_type = ThePathname(p)->pathname_type;
      ThePathname(new)->pathname_type =
        (!nullp(p_type) ? p_type : ThePathname(d)->pathname_type);
    }
    #if HAS_VERSION
    # Version matchen:
    # Verwende pathname-version, falls angegeben, und default-version sonst.
    { var reg1 object p_version = ThePathname(p)->pathname_version;
      ThePathname(new)->pathname_version =
        (!nullp(p_version) ? p_version : STACK_0);
      skipSTACK(1);
    }
    #endif
    # new als Wert:
    value1 = new; mv_count=1;
  }}

LISPFUN(enough_namestring,1,1,norest,nokey,0,NIL)
# (ENOUGH-NAMESTRING pathname [defaults]), CLTL S. 417
# (defun enough-namestring (pathname &optional (defaults *default-pathname-defaults*))
#   (setq pathname (pathname pathname))
#   (setq defaults (pathname defaults))
#   (namestring
#     (multiple-value-call #'make-pathname
#if HAS_HOST
#       (if (equal (pathname-host pathname) (pathname-host defaults))
#         (values
#           :host nil
#endif
#if HAS_DEVICE
#       (if (equal (pathname-device pathname) (pathname-device defaults))
#         (values
#           :device nil
#endif
#           :directory
#             (let ((pathname-dir (pathname-directory pathname))
#                   (defaults-dir (pathname-directory defaults)))
#if HAS_SERNR
#               (if (equal (car pathname-dir) (car defaults-dir))
#                 (cons nil
#                   (progn (pop pathname-dir) (pop defaults-dir)
#endif
#                     (if (equal pathname-dir defaults-dir)
#                       (list ':RELATIVE)
#                       (if (and (not (eq (car pathname-dir) ':RELATIVE))
#                                (not (eq (car defaults-dir) ':RELATIVE))
#                                (equal (subseq pathname-dir 0 (min (length pathname-dir) (length defaults-dir)))
#                                       defaults-dir
#                           )    )
#                         (cons ':RELATIVE (nthcdr (length defaults-dir) pathname-dir))
#                         pathname-dir
#                     ) )
#if HAS_SERNR
#                 ) )
#                 pathname-dir
#               )
#endif
#             )
#         )
#         (values
#if HAS_HOST
#           :host (pathname-host pathname)
#endif
#if HAS_DEVICE
#           :device (pathname-device pathname)
#endif
#           :directory (pathname-directory pathname)
#       ) )
#       :name (if (equal (pathname-name pathname) (pathname-name defaults))
#               nil
#               (pathname-name pathname)
#             )
#       :type (if (equal (pathname-type pathname) (pathname-type defaults))
#               nil
#               (pathname-type pathname)
#             )
# ) ) )
  { # pathname und defaults überprüfen:
    var reg6 object new = prepare_for_merge();
    pushSTACK(new);
    # Stackaufbau: pathname, defaults, new.
    #if HAS_HOST
    # Hosts vergleichen:
    { var reg7 object p_host = ThePathname(STACK_2)->pathname_host; # pathname-host
      var reg8 object d_host = ThePathname(STACK_1)->pathname_host; # defaults-host
      if (equal(p_host,d_host)) # beide Hosts gleich ?
        # ja.
        { ThePathname(new)->pathname_host = NIL; # new-host := NIL
    #endif
    #if HAS_DEVICE
    # Devices vergleichen:
    { var reg7 object p_device = ThePathname(STACK_2)->pathname_device; # pathname-device
      var reg8 object d_device = ThePathname(STACK_1)->pathname_device; # defaults-device
      if (equal(p_device,d_device)) # beide Devices gleich ?
        # ja.
        { ThePathname(new)->pathname_device = NIL; # new-device := NIL
    #endif
         {var reg3 object p_directory = ThePathname(STACK_2)->pathname_directory; # pathname-directory
          var reg4 object d_directory = ThePathname(STACK_1)->pathname_directory; # defaults-directory
          #if HAS_SERNR
          if (eq(Car(p_directory),Car(d_directory))) # gleiche Seriennummern ?
            # ja -> verwende NIL als Seriennummer
            { p_directory = Cdr(p_directory); # pathname-subdirs
              d_directory = Cdr(d_directory); # defaults-subdirs
          #endif
             {var reg5 object new_subdirs;
              # vergleiche pathname-subdirs und defaults-subdirs:
              if (equal(p_directory,d_directory))
                # gleich -> verwende (cons :RELATIVE nil) :
                { new_subdirs = NIL; goto insert_RELATIVE; }
                else
                { # Fängt weder pathname-subdirs noch defaults-subdirs
                  # mit :RELATIVE an?
                  if (   (!eq(Car(p_directory),S(Krelative)))
                      && (!eq(Car(d_directory),S(Krelative)))
                     )
                    # ja -> testen, ob defaults-subdirs ein Anfangsstück
                    # der Liste pathname-subdirs ist:
                    { # (Die CARs sind beide = :ABSOLUTE, also gleich.)
                      var reg1 object Lp = Car(p_directory);
                      var reg2 object Ld = Car(d_directory);
                      # Ist Ld ein Anfangsstück von Lp ?
                      loop
                        { if (atomp(Ld)) # Ld zu Ende -> ja
                            { new_subdirs = Lp; goto insert_RELATIVE; }
                          if (atomp(Lp)) break; # Lp zu Ende -> nein
                          if (!equal(Car(Ld),Car(Lp))) # verschiedene Listenelemente?
                            break; # -> nein
                          Ld = Cdr(Ld); Lp = Cdr(Lp); # Listen weiterrücken
                        }
                    }
                  new_subdirs = p_directory; # new-subdirs := pathname-subdirs
                  goto subdirs_ok;
                }
              insert_RELATIVE:
              # new-subdirs := (cons :RELATIVE new-subdirs) :
              { pushSTACK(new_subdirs);
                new_subdirs = allocate_cons();
                Cdr(new_subdirs) = popSTACK(); Car(new_subdirs) = S(Krelative);
              }
              subdirs_ok: # new-subdirs ist die neue Subdir-Liste.
              #if HAS_SERNR
              # new-subdirs := (cons NIL new-subdirs) :
              { pushSTACK(new_subdirs);
               {var reg1 object new_cons = allocate_cons();
                Cdr(new_cons) = popSTACK();
                new_subdirs = new_cons;
              }}
              #endif
              # new-directory := new-subdirs :
              ThePathname(new=STACK_0)->pathname_directory = new_subdirs;
             }
            #if HAS_SERNR
            }
            else
            # verschiedene Seriennummern -> new-directory := pathname-directory :
            { ThePathname(new)->pathname_directory = p_directory; }
            #endif
         }
    #if HAS_DEVICE
        }
        else
        # verschiedene Devices
        { # new-device := pathname-device :
          ThePathname(new)->pathname_device = p_device;
          # new-directory := pathname-directory :
          ThePathname(new)->pathname_directory = ThePathname(STACK_2)->pathname_directory;
        }
    }
    #endif
    #if HAS_HOST
        }
        else
        # verschiedene Hosts
        { # new-host := pathname-host :
          ThePathname(new)->pathname_host = p_host;
          #if HAS_DEVICE
          # new-device := pathname-device :
          ThePathname(new)->pathname_device = ThePathname(STACK_2)->pathname_device;
          #endif
          # new-directory := pathname-directory :
          ThePathname(new)->pathname_directory = ThePathname(STACK_2)->pathname_directory;
        }
    }
    #endif
    # name einfüllen:
    { var reg1 object p_name = ThePathname(STACK_2)->pathname_name; # pathname-name
      var reg2 object d_name = ThePathname(STACK_1)->pathname_name; # defaults-name
      ThePathname(new)->pathname_name = (equal(p_name,d_name) ? NIL : p_name);
    }
    # type einfüllen:
    { var reg1 object p_type = ThePathname(STACK_2)->pathname_type; # pathname-type
      var reg2 object d_type = ThePathname(STACK_1)->pathname_type; # defaults-type
      ThePathname(new)->pathname_type = (equal(p_type,d_type) ? NIL : p_type);
    }
    skipSTACK(3);
    # (namestring new) bilden:
    value1 = whole_namestring(new); mv_count=1;
  }

#ifdef PATHNAME_EXT83

# UP: Überprüft, ob object ein zulässiger Name oder Typ ist: :WILD oder
# ein Simple-String mit max. stdlen Zeichen, alle alphabetisch und Up-case.
# legal_name_or_type(object,stdlen)
  local boolean legal_name_or_type (object obj, uintL stdlen);
  local boolean legal_name_or_type(obj,stdlen)
    var reg3 object obj;
    var reg4 uintL stdlen;
    { if (eq(obj,S(Kwild))) { return TRUE; } # :WILD ist OK
      if (!simple_string_p(obj)) { return FALSE; } # sonst: Simple-String ?
     {var reg2 uintL len = TheSstring(obj)->length;
      #ifndef EMUNIX_PORTABEL
      if (!(len <= stdlen)) { return FALSE; } # und Länge <=stdlen ?
      #endif
      # Jedes einzelne Zeichen überprüfen:
      {var reg1 uintB* ptr = &TheSstring(obj)->data[0];
       dotimesL(len,len,
         { var reg1 uintB ch = *ptr++;
           if (!(legal_namechar(ch) # zulässiges Zeichen ?
                 && (up_case(ch)==ch) # und Großbuchstabe ?
              ) )
             { return FALSE; }
         });
      }
      return TRUE;
    }}

# UP: Überprüft, ob object ein zulässiger Name ist: :WILD oder
# ein Simple-String mit max. 8 Zeichen, alle alphabetisch und Up-case.
# legal_name(object)
  #define legal_name(obj)  legal_name_or_type(obj,8)

# UP: Überprüft, ob object ein zulässiger Typ ist: :WILD oder
# ein Simple-String mit max. 3 Zeichen, alle alphabetisch und Up-case.
# legal_type(object)
  #define legal_type(obj)  legal_name_or_type(obj,3)

#endif # PATHNAME_EXT83

#ifdef PATHNAME_VMS

# UP: Überprüft, ob object ein zulässiger Name oder Typ ist:
# ein Simple-String, alle Zeichen alphabetisch und Up-case.
# legal_name_or_type(object)
  local boolean legal_name_or_type (object obj);
  local boolean legal_name_or_type(obj)
    var reg3 object obj;
    { if (!simple_string_p(obj)) { return FALSE; } # Simple-String ?
     {var reg2 uintL len = TheSstring(obj)->length;
      # Jedes einzelne Zeichen überprüfen:
      {var reg1 uintB* ptr = &TheSstring(obj)->data[0];
       dotimesL(len,len,
         { var reg1 uintB ch = *ptr++;
           if (!(legal_namechar(ch) # zulässiges Zeichen ?
                 && (up_case(ch)==ch) # und Großbuchstabe ?
              ) )
             { return FALSE; }
         });
      }
      return TRUE;
    }}

# UP: Überprüft, ob object ein zulässiger Name ist:
# ein Simple-String, alle Zeichen alphabetisch und Up-case.
# legal_name(object)
  #define legal_name(obj)  legal_name_or_type(obj)

# UP: Überprüft, ob object ein zulässiger Typ ist:
# ein Simple-String, alle Zeichen alphabetisch und Up-case.
# legal_type(object)
  #define legal_type(obj)  legal_name_or_type(obj)

#endif # PATHNAME_VMS

#ifdef PATHNAME_NOEXT

# UP: Überprüft, ob object ein zulässiger Name ist:
# ein Simple-String aus gültigen Zeichen
# legal_name(object)
  local boolean legal_name (object obj);
  local boolean legal_name(obj)
    var reg3 object obj;
    { if (!simple_string_p(obj)) { return FALSE; }
     {var reg2 uintL len = TheSstring(obj)->length;
      var reg1 uintB* charptr = &TheSstring(obj)->data[0];
      dotimesL(len,len, { if (!legal_namechar(*charptr++)) { return FALSE; } } );
      return TRUE;
    }}

# UP: Überprüft, ob object ein zulässiger Name ist:
# ein Simple-String aus gültigen Zeichen, ohne '.'
# legal_type(object)
  local boolean legal_type (object obj);
  local boolean legal_type(obj)
    var reg4 object obj;
    { if (!simple_string_p(obj)) { return FALSE; }
     {var reg3 uintL len = TheSstring(obj)->length;
      var reg2 uintB* charptr = &TheSstring(obj)->data[0];
      dotimesL(len,len,
        { var reg1 uintB ch = *charptr++;
          if ((ch=='.') || (!legal_namechar(ch))) { return FALSE; }
        });
      return TRUE;
    }}

#endif # PATHNAME_NOEXT

LISPFUN(make_pathname,0,0,norest,key,7,\
        (kw(defaults),kw(host),kw(device),kw(directory),kw(name),kw(type),kw(version)) )
# (MAKE-PATHNAME [:host] [:device] [:directory] [:name] [:type] [:version]
#                [:defaults]),
# CLTL S. 416
  # Stackaufbau: defaults, host, device, directory, name, type, version.
  { # 1. host überprüfen:
    #if HAS_HOST
    STACK_5 = test_optional_host(STACK_5);
    #else
    test_optional_host(STACK_5);
    #endif
    # 2. device überprüfen:
    #if HAS_DEVICE
    { var reg1 object device = STACK_4;
      if (eq(device,unbound)) # angegeben ?
        { STACK_4 = NIL; } # nein -> verwende NIL
        else
        { if (nullp(device)) goto device_ok; # = NIL ?
          #if defined(PATHNAME_ATARI) || defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2)
          elif (eq(device,S(Kwild))) goto device_ok; # = :WILD ?
          elif (simple_string_p(device)) # Simple-String ?
            { if (TheSstring(device)->length == 1) # der Länge 1 ?
                { var reg2 uintB ch = TheSstring(device)->data[0];
                  if ((ch >= 'A') && (ch <= 'Z')) # mit Buchstaben >='A' und <='Z' ?
                    goto device_ok;
            }   }
          #endif
          #ifdef PATHNAME_AMIGAOS
          elif (simple_string_p(device)) # Simple-String ?
            { var reg1 uintB* ptr = &TheSstring(device)->data[0];
              var reg2 uintL count;
              dotimesL(count,TheSstring(device)->length,
                { if (!legal_namechar(*ptr++)) goto device_not_ok; }
                );
              goto device_ok;
              device_not_ok: ;
            }
          #endif
          #ifdef PATHNAME_VMS
          elif (simple_string_p(device)) # Simple-String ?
            { if (legal_name_or_type(device)) # alle Zeichen alphabetisch und Up-case?
                goto device_ok;
            }
          #endif
          elif (pathnamep(device)) # Pathname -> dessen Device
            { STACK_4 = ThePathname(device)->pathname_device; goto device_ok; }
          # Keiner der gewünschten Fälle -> Fehler:
          pushSTACK(STACK_4); pushSTACK(S(Kdevice)); goto fehler_arg;
          device_ok: ;
    }   }
    #else
    { var reg1 object device = STACK_4;
      if (!eq(device,unbound)) # angegeben ?
        if (!(nullp(device) || pathnamep(device))) # NIL oder Pathname -> OK
          # Keiner der gewünschten Fälle -> Fehler:
          { pushSTACK(STACK_4); pushSTACK(S(Kdevice)); goto fehler_arg; }
    }
    #endif
    # 3. directory überprüfen:
    { var reg1 object directory = STACK_3;
      if (eq(directory,unbound) || nullp(directory)) # nicht angegeben oder =NIL ?
        { STACK_3 = O(directory_default); # Default ist (NIL :RELATIVE)
          goto directory_ok;
        }
      elif (consp(directory)) # ein Cons?
        {
          #if HAS_SERNR
          # Der CAR entweder NIL oder ein Fixnum >=0 ?
          if (!(nullp(Car(directory)) || mposfixnump(Car(directory))))
            goto directory_bad;
          directory = Cdr(directory); # subdir-Liste
          #endif
          # Der CAR entweder :RELATIVE oder :ABSOLUTE ?
          if (!consp(directory)) goto directory_bad;
          { var reg1 object startpoint = Car(directory);
            if (!(eq(startpoint,S(Krelative)) || eq(startpoint,S(Kabsolute))))
              goto directory_bad;
          }
          directory = Cdr(directory);
          # Subdir-Liste überprüfen:
          while (consp(directory))
            { # nächstes subdir überprüfen:
              var reg1 object subdir = Car(directory);
              #ifdef PATHNAME_EXT83
              if (consp(subdir))
                { # subdir ist ein Cons
                  if (!(legal_name(Car(subdir)) && legal_type(Cdr(subdir))))
                    goto directory_bad;
                }
                else
                { # subdir ist ein Atom
                  if (!(eq(subdir,S(Kcurrent)) # = :CURRENT ?
                        || eq(subdir,S(Kparent)) # = :PARENT ?
                        || eq(subdir,S(Kwild)) # = :WILD ?
                     ) )
                    goto directory_bad;
                }
              #endif
              #if defined(PATHNAME_NOEXT) || defined(PATHNAME_VMS)
              #if defined(PATHNAME_AMIGAOS) || defined(PATHNAME_VMS)
              if (!(eq(subdir,S(Kwild)) || eq(subdir,S(Kparent))
                    || legal_name(subdir)
                 ) )
                goto directory_bad;
              #endif
              #if defined(PATHNAME_UNIX) || defined(PATHNAME_OS2)
              if (!(eq(subdir,S(Kwild)) || legal_name(subdir)))
                goto directory_bad;
              #endif
              #endif
              directory = Cdr(directory);
            }
          goto directory_ok;
        }
      elif (pathnamep(directory)) # Pathname -> dessen Directory
        { STACK_3 = ThePathname(directory)->pathname_directory; goto directory_ok; }
      # Keiner der gewünschten Fälle -> Fehler:
      directory_bad:
      pushSTACK(STACK_3); pushSTACK(S(Kdirectory)); goto fehler_arg;
      directory_ok: ;
      #ifdef PATHNAME_AMIGAOS
      # Bei device /= NIL muß directory mit :ABSOLUTE anfangen:
      if (!nullp(STACK_4) && !eq(Car(STACK_3),S(Kabsolute))) goto directory_bad;
      #endif
    }
    # 4. name überprüfen:
    { var reg1 object name = STACK_2;
      if (eq(name,unbound))
        { STACK_2 = NIL; } # nicht angegeben -> verwende NIL
      elif (nullp(name)) {} # NIL ist OK
      #if defined(PATHNAME_NOEXT) || defined(PATHNAME_VMS)
      elif (eq(name,S(Kwild))) { STACK_2 = O(wild_string); } # aus :WILD mache "*"
      #endif
      elif (equal(name,O(leer_string))) # name = "" ?
        { STACK_2 = NIL; } # ja -> verwende NIL
      elif (legal_name(name)) {} # zulässiger Name ist OK
      elif (pathnamep(name)) # Pathname -> dessen Name
        { STACK_2 = ThePathname(name)->pathname_name; }
      else # Keiner der gewünschten Fälle -> Fehler:
        { pushSTACK(STACK_2); pushSTACK(S(Kname)); goto fehler_arg; }
    }
    # 5. type überprüfen:
    { var reg1 object type = STACK_1;
      if (eq(type,unbound))
        { STACK_1 = NIL; } # nicht angegeben -> verwende NIL
      elif (nullp(type)) {} # NIL ist OK
      #if defined(PATHNAME_NOEXT) || defined(PATHNAME_VMS)
      elif (eq(type,S(Kwild))) { STACK_1 = O(wild_string); } # aus :WILD mache "*"
      #endif
      elif (legal_type(type)) {} # zulässiger Typ ist OK
      elif (pathnamep(type)) # Pathname -> dessen Typ
        { STACK_1 = ThePathname(type)->pathname_type; }
      else # Keiner der gewünschten Fälle -> Fehler:
        { pushSTACK(STACK_1); pushSTACK(S(Ktype)); goto fehler_arg; }
    }
    # 6. version überprüfen:
    #if HAS_VERSION
    STACK_0 = test_optional_version(NIL); # Default ist NIL
    #else
    test_optional_version();
    #endif
    # 7. Pathname bauen:
    {var reg1 object pathname = allocate_pathname(); # neuer Pathname
     #if HAS_VERSION
     ThePathname(pathname)->pathname_version   = popSTACK();
     #endif
     ThePathname(pathname)->pathname_type      = popSTACK();
     ThePathname(pathname)->pathname_name      = popSTACK();
     ThePathname(pathname)->pathname_directory = popSTACK();
     #if HAS_DEVICE
     ThePathname(pathname)->pathname_device    = popSTACK();
     #else
     skipSTACK(1);
     #endif
     #if HAS_HOST
     ThePathname(pathname)->pathname_host      = popSTACK();
     #else
     skipSTACK(1);
     #endif
    # 8. evtl. Defaults hineinmergen:
     {var reg2 object defaults = popSTACK();
      if (eq(defaults,unbound))
        # keine Defaults angegeben -> pathname als Wert
        { value1 = pathname; }
        else
        # (MERGE-PATHNAMES pathname defaults [nil]) aufrufen:
        { pushSTACK(pathname); pushSTACK(defaults); pushSTACK(NIL);
          funcall(L(merge_pathnames),3);
        }
      mv_count=1;
      return;
    }}
    # Fehlermeldung:
    fehler_arg:
    pushSTACK(TheSubr(subr_self)->name);
    fehler(
           DEUTSCH ? "~: Unzulässiges ~-Argument ~" :
           ENGLISH ? "~: illegal ~ argument ~" :
           FRANCAIS ? "~ : Argument incorrect pour ~ : ~" :
           ""
          );
  }

#ifdef USER_HOMEDIR
LISPFUN(user_homedir_pathname,0,1,norest,nokey,0,NIL)
# (USER-HOMEDIR-PATHNAME [host]), CLTL S. 418
  {
    #if HAS_HOST
    STACK_0 = test_optional_host(STACK_0); # Host überprüfen
    #ifdef VMS
    # Überprüfe, ob (OR (NULL host) (EQUAL host (MACHINE-INSTANCE))):
    if (nullp(STACK_0)
        || (funcall(L(machine_instance),0), equal(value1,STACK_0))
       )
      { value1 = O(user_homedir); } # User-Homedir-Pathname
      else
      { value1 = NIL; } # unbekannt
    #else
    ??
    #endif
    #else
    test_optional_host(popSTACK()); # Host überprüfen und ignorieren
    value1 = O(user_homedir); # User-Homedir-Pathname
    #endif
    mv_count=1; # als Wert
  }
#endif

#if defined(PATHNAME_NOEXT) || defined(PATHNAME_VMS)
# UP: Testet, ob ein Simple-String Wildcards enthält.
# has_wildcards(string)
# > string: Simple-String
# < ergebnis: TRUE wenn string Wildcard-Zeichen enthält
  local boolean has_wildcards (object string);
  local boolean has_wildcards(string)
    var reg4 object string;
    { var reg3 uintL len = TheSstring(string)->length;
      var reg2 uintB* charptr = &TheSstring(string)->data[0];
      dotimesL(len,len,
        { var reg1 uintB ch = *charptr++;
          if ((ch=='*') || (ch=='?')) { return TRUE; }
        });
      return FALSE;
    }
#endif

# UP: Überprüft, ob ein Pathname keine Wildcards enthält.
# check_no_wildcards(pathname);
# > pathname: Pathname
  local void check_no_wildcards (object pathname);
  local void check_no_wildcards(pathname)
    var reg2 object pathname;
    { # Host kann keine Wildcards enthalten.
      #if defined(PATHNAME_ATARI) || defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2)
      # Device überprüfen: = :WILD ?
      if (eq(ThePathname(pathname)->pathname_device,S(Kwild))) goto wildcards;
      #endif
      # Directory überprüfen:
      { var reg1 object directory = ThePathname(pathname)->pathname_directory;
        #if HAS_SERNR
        directory = Cdr(directory); # Seriennummer übergehen
        #endif
        while (consp(directory = Cdr(directory)))
          { var reg2 object subdir = Car(directory);
            #ifdef PATHNAME_EXT83
            if (consp(subdir))
              { # subdir ist ein Cons. name oder type = :WILD ?
                if (eq(Car(subdir),S(Kwild)) || eq(Cdr(subdir),S(Kwild)))
                  goto wildcards;
              }
              else
              { # subdir ist ein Atom. = :WILD ?
                if (eq(subdir,S(Kwild))) goto wildcards;
              }
            #endif
            #if defined(PATHNAME_NOEXT) || defined(PATHNAME_VMS)
            if (simple_string_p(subdir))
              { if (has_wildcards(subdir)) goto wildcards; }
              else
              { if (eq(subdir,S(Kwild))) goto wildcards; }
            #endif
      }   }
      # Name überprüfen:
      #ifdef PATHNAME_EXT83
      if (eq(ThePathname(pathname)->pathname_name,S(Kwild))) # Name = :WILD ?
        goto wildcards;
      #endif
      #if defined(PATHNAME_NOEXT) || defined(PATHNAME_VMS)
      { var reg1 object name = ThePathname(pathname)->pathname_name;
        if (simple_string_p(name))
          { if (has_wildcards(name)) goto wildcards; }
      }
      #endif
      # Typ überprüfen:
      #ifdef PATHNAME_EXT83
      if (eq(ThePathname(pathname)->pathname_type,S(Kwild))) # Typ = :WILD ?
        goto wildcards;
      #endif
      #if defined(PATHNAME_NOEXT) || defined(PATHNAME_VMS)
      { var reg1 object type = ThePathname(pathname)->pathname_type;
        if (simple_string_p(type))
          { if (has_wildcards(type)) goto wildcards; }
      }
      #endif
      #if HAS_HOST
      # Version überprüfen:
      #ifdef PATHNAME_VMS
      if (eq(ThePathname(pathname)->pathname_version,S(Kwild))) goto wildcards;
      #endif
      #endif
      # Keine Wildcards gefunden.
      return;
      # Fehlermeldung, wenn der Pathname Wildcards enthält:
      wildcards:
        pushSTACK(pathname);
        fehler(
               DEUTSCH ? "Hier sind keine Wildcards (Dateiquantoren) erlaubt: ~" :
               ENGLISH ? "wildcards are not allowed here: ~" :
               FRANCAIS ? "Les caractères joker ne sont pas permis ici : ~" :
               ""
              );
    }

# UP: Kopiert einen Pathname.
# copy_pathname(pathname)
# > pathname: Pathname
# < ergebnis: Kopie des Pathname, mit denselben Komponenten
# kann GC auslösen
  local object copy_pathname (object pathname);
  local object copy_pathname(pathname)
    var reg2 object pathname;
    { pushSTACK(pathname);
     {var reg1 object new = allocate_pathname();
      pathname = popSTACK();
      #if HAS_HOST
      ThePathname(new)->pathname_host      = ThePathname(pathname)->pathname_host     ;
      #endif
      #if HAS_DEVICE
      ThePathname(new)->pathname_device    = ThePathname(pathname)->pathname_device   ;
      #endif
      ThePathname(new)->pathname_directory = ThePathname(pathname)->pathname_directory;
      ThePathname(new)->pathname_name      = ThePathname(pathname)->pathname_name     ;
      ThePathname(new)->pathname_type      = ThePathname(pathname)->pathname_type     ;
      #if HAS_VERSION
      ThePathname(new)->pathname_version   = ThePathname(pathname)->pathname_version  ;
      #endif
      return new;
    }}

# UP: Stellt fest, ob der Name eines Pathname =NIL ist.
# namenullp(pathname)
  # local boolean namenullp (object pathname);
  # local boolean namenullp(pathname)
  #   { return nullp(ThePathname(pathname)->pathname_name); }
  #define namenullp(path)  (nullp(ThePathname(path)->pathname_name))

# Fehler, wenn ein Directory nicht existiert
# > obj: Pathname oder (besser) fehlerhafte Komponente
  local nonreturning void fehler_dir_not_exists (object obj);
  local nonreturning void fehler_dir_not_exists(obj)
    var reg1 object obj;
    { pushSTACK(obj);
      fehler(
             DEUTSCH ? "Directory existiert nicht: ~" :
             ENGLISH ? "nonexistent directory: ~" :
             FRANCAIS ? "Le répertoire ~ n'existe pas." :
             ""
            );
    }

# Fehler, wenn eine Datei bereits existiert
# > caller: Aufrufer (ein Symbol)
# > pathname: Pathname
  local nonreturning void fehler_file_exists (object caller, object pathname);
  local nonreturning void fehler_file_exists(caller,pathname)
    var reg2 object caller;
    var reg1 object pathname;
    { pushSTACK(pathname);
      pushSTACK(caller);
      fehler(
             DEUTSCH ? "~: Eine Datei ~ existiert bereits." :
             ENGLISH ? "~: File ~ already exists" :
             FRANCAIS ? "~ : Le fichier ~ existe déjà." :
             ""
            );
    }

#if defined(PATHNAME_ATARI) || defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2)

#ifdef PATHNAME_ATARI
# Ein "absoluter Pathname" ist ein Pathname, bei dem Device ein überprüfter
# String ist und Directory die Seriennummer, aber kein :RELATIVE, :CURRENT,
# :PARENT enthält.
#endif
#if defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2)
# Ein "absoluter Pathname" ist ein Pathname, bei dem Device ein überprüfter
# String ist und Directory kein :RELATIVE, :CURRENT, :PARENT enthält.
#endif

# UP: Liefert den Namestring eines Pathname als ASCIZ-String.
# namestring_asciz(dir_namestring)
# > STACK_0: Pathname
# > dir_namestring: Directory-Namestring (für DOS bzw. GEMDOS, ohne Seriennummer)
# < ergebnis: Namestring (für DOS bzw. GEMDOS, ohne Seriennummer, mit Nullbyte am Schluß)
# kann GC auslösen
  local object namestring_asciz (object dir_namestring);
  local object namestring_asciz(dir_namestring)
    var reg1 object dir_namestring;
    { var reg1 uintC stringcount;
      pushSTACK(dir_namestring); # Directory-Namestring als 1. String
      stringcount = file_namestring_parts(STACK_(0+1)); # Strings zum Filenamen
      pushSTACK(O(null_string)); # String mit Nullbyte
      return string_concat(1+stringcount+1); # zusammenhängen
    }

#ifdef PATHNAME_ATARI

#if HAS_SERNR && defined(ATARI)
# UP: Holt die Seriennummer einer Diskette.
# get_disk_number(drive)
# > uintW drive : Laufwerksbuchstabe ('A', 'B', ...)
# < object ergebnis : Seriennummer als Fixnum >=0
# kann GC auslösen
  local object get_disk_number (uintW drive);
  local uintB volume_path[] = "?:\\*.*";
  local DTA dta_buf;
  local object get_disk_number(drive)
    var reg2 uintW drive;
    # Methode:
    # Damit GEMDOS einen eventuellen Diskettenwechsel erkennt (und dann seine
    # internen Buffer leert, so daß korrekter Zugriff auf die Unterdirectories
    # möglich wird), darf GEMDOS vor dem BIOS die Diskette ansehen. Dazu darf
    # es erst einmal das Volume-Label suchen.
    # Danach holen wir uns mit einem BIOS-Zugriff die Seriennummer.
    { var reg3 uintL sectorlength;
      volume_path[0] = (uintB)drive; # Pfad für Volume-Label-Suche 'X:\*.*'
                              # mit korrektem Laufwerksbuchstaben versehen
      GEMDOS_SetDTA(&dta_buf); # DTA-Buffer setzen
      { var reg1 WORD erg = GEMDOS_Sfirst(&volume_path,8); # Suche des Volume-Label beginnen
                               # (8 = Attributmaske für "nur Volume-Label suchen")
        if (!(erg == GEMDOS_Sfirst_notfound) && (erg<0))
          OS_error(erg); # wesentlicher Fehler aufgetreten -> melden
      }
      # Volume-Label ist OK.
      drive = drive - 'A'; # drive = Laufwerksnummer
      { var reg1 ULONG erg = BIOS_GetBPB(drive); # Disk-Parameter holen
        if (erg==0) # Fehler?
          OS_error(-1); # ja -> erzeuge Fehler 'Allgemeiner Fehler'
        sectorlength = (uintL)(((BPB*)erg)->recsiz);
      }
      # sectorlength = Länge (in Bytes) der Sectoren auf dieser Diskette
      { var reg4 DYNAMIC_ARRAY(bootsector,BYTE,sectorlength);
        # Bootsector lesen:
        { var reg1 WORD erg = BIOS_ReadAbs(bootsector,1,0,drive); # 1 Sector ab Sector 0 lesen
          if (erg<0) # Fehler aufgetreten -> melden
            { FREE_DYNAMIC_ARRAY(bootsector); OS_error(erg); }
        }
        # Bytes 8,9,10 ist die Disknummer:
        { var reg1 uintL seriennummer = *(ULONG*)(bootsector+8) >> 8;
          FREE_DYNAMIC_ARRAY(bootsector);
          return(fixnum(seriennummer)); # als Fixnum
    } } }
#endif

# UP: Bestimmt den Alisteneintrag in DRIVE_ALIST zu einem gegebenen Drive.
# get_drive_alist(pathname)
# > pathname: Pathname mit String als Device
# < ergebnis: Alisteneintrag
  local object get_drive_alist (object pathname);
  local object get_drive_alist(pathname)
    var reg5 object pathname;
    { var reg4 object device = ThePathname(pathname)->pathname_device;
      var reg3 uintB drive = TheSstring(device)->data[0]; # Laufwerksbuchstabe
      # Drive-Aliste durchlaufen:
      var reg2 object alistr = O(drive_alist);
      while (consp(alistr))
        { var reg1 object entry = Car(alistr); # Alisteneintrag
          # (car entry) ist ein Simple-String der Länge 1. Mit drive vergleichen:
          if (TheSstring(Car(entry))->data[0] == drive) { return entry; }
          alistr = Cdr(alistr);
        }
      # Liste zu Ende -> nicht existentes Laufwerk
      pushSTACK(device);
      fehler(
             DEUTSCH ? "Ein Laufwerk ~ gibt es nicht." :
             ENGLISH ? "drive ~ does not exist" :
             FRANCAIS ? "Le disque ~ n'existe pas." :
             ""
            );
    }

#endif

#if defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2)

# Liefert das aktuelle Directory auf einem Laufwerk.
# getwd_of(path,drive)
# > uintB drive: Laufwerks-(groß-)buchstabe
# > uintB* path: Platz fürs aktuelle Directory
# < path: Pfad des aktuellen Directories, mit '/' als Trennzeichen und als Anfang
# < ergebnis: <0 falls Fehler
  #ifdef DJUNIX
    #define getwd_of(path,drive)  ((path)[0] = '/', getwdof(&(path)[1],(drive)-'A'+1))
  #endif
  #ifdef EMUNIX
    #define getwd_of(path,drive)  _getcwd1(path,drive)
  #endif

# UP: Stellt fest, ob ein Laufwerk existiert.
# > uintB drive: Laufwerks-(groß-)buchstabe
# < boolean ergebnis: ob dieses Laufwerk existiert und ansprechbar ist
  local boolean good_drive (uintB drive);
  #ifdef DJUNIX
  local boolean good_drive(drive)
    var reg1 uintB drive;
    { # Methode:
      # getwd auf dem Drive probieren. (Das liefert leider keinen Errorcode.)
      # chdir('/') auf dem Drive probieren.
      # Error ENOTDIR -> Drive existiert nicht, fertig.
      # Sonst: Mit chdir wieder aufs alte Directory zurück.
      var char path_buffer[3+MAXPATHLEN];
      begin_system_call();
      # 1. getwd probieren:
      path_buffer[0] = drive; path_buffer[1] = ':';
      # Working Directory in path_buffer ablegen:
      if (getwd_of(&path_buffer[2],drive) < 0)
        { end_system_call(); return FALSE; }
      # 2. chdir probieren:
      {var char buffer[4];
       buffer[0] = drive; buffer[1] = ':'; buffer[2] = '/'; buffer[3] = 0;
       if (chdir(&buffer[0]) < 0)
         { if (!(errno == ENOTDIR)) { OS_error(); }
           end_system_call(); return FALSE;
      }  }
      # 3. chdir zurück:
      if (chdir(&path_buffer[0]) < 0) { OS_error(); }
      end_system_call();
      return TRUE;
    }
  #endif
  #ifdef EMUNIX
  local boolean good_drive(drive)
    var reg1 uintB drive;
    { # Methode (siehe HELPPC/misc.txt):
      # 1. save current drive  (INT 0x21,0x19)
      # 2. set current drive  (INT 0x21,0xE)
      # 3. get current drive  (INT 0x21,0x19)
      # 4. if current drive == drive requested
      #       then drive exists
      #       else drive doesn't exist
      # 5. reset original drive  (INT 0x21,0xE)
      var reg3 boolean result;
      begin_system_call();
     {var reg2 uintB orig_drive = _getdrive();
      _chdrive(drive);
      result = (_getdrive() == drive);
      _chdrive(orig_drive);
     }
      end_system_call();
      return result;
      # Alternative:
      # { var uintB drv[3];
      #   var uintB fsys[16];
      #   drv[0] = drive; drv[1] = ':'; drv[2] = '\0';
      #   begin_system_call();
      #  {var int result = _filesys(drv,&fsys,sizeof(fsys));
      #   end_system_call();
      #   return (result==0);
      # }}
    }
  #endif

# UP: Liefert das aktuelle Drive.
# < uintB drive: Laufwerks-(groß-)buchstabe
  local uintB default_drive (void);
 #ifdef DJUNIX
  local uintB default_drive()
    { # Die DOS-Funktion 0x19 können wir leider nicht benutzen, sie wird
      # in EXPHDLR.C nicht durchgereicht. Daher ein kleiner Hack:
      var struct ffblk DTA_buffer;
      set_break_sem_4(); # wegen DTA-Buffer gegen Unterbrechungen sperren
      # Suche nach allem:
      { var reg2 uintB attrib = FA_LABEL|FA_DIREC|FA_ARCH|FA_SYSTEM|FA_HIDDEN|FA_RDONLY;
        if (findfirst("\\*.*",&DTA_buffer,attrib) <0)
          { if (!((errno==ENOENT) || (errno==ENOMORE))) { OS_error(); } }
        clr_break_sem_4();
        # Undokumentiert: Von den ersten beiden Bytes enthält das eine
        # den Laufwerksbuchstaben, das andere evtl. attrib.
        # Das scheint nicht zu funktionieren??
       {var reg1 uintB drive =
          (DTA_buffer.ff_reserved[0] < 63 ? DTA_buffer.ff_reserved[1]
                                          : DTA_buffer.ff_reserved[0]
          );
        if ((drive >= 'A') && (drive <= 'Z')) { return drive; }
      }}
      # Ist das nicht gelungen, versuche alle Drives von 'C' bis 'Z':
      {var reg1 uintB drive;
       for (drive = 'C'; drive<='Z'; drive++)
         { if (good_drive(drive)) return drive; }
      }
      # Ist das auch nicht gelungen, so ist wohl 'A' das Default-Drive:
      return 'A';
    }
 #endif
 #ifdef EMUNIX
  local uintB default_drive()
    { var reg1 uintB result;
      begin_system_call();
      result = _getdrive();
      end_system_call();
      return result;
    }
 #endif

# UP: Liefert das aktuelle Directory auf einem gegebenen Drive.
# > uintB drive: Laufwerks-(groß-)buchstabe
# < ergebnis: aktuelles Directory (als Pathname)
# kann GC auslösen
  local object default_directory_of (uintB drive);
  local object default_directory_of(drive)
    var reg1 uintB drive;
    # Working Directory (von DOS) ist das aktuelle Directory:
    { var char path_buffer[3+MAXPATHLEN]; # vgl. GETWD(3)
      path_buffer[0] = drive; path_buffer[1] = ':';
      # Working Directory in path_buffer ablegen:
      begin_system_call();
      getwd_of(&path_buffer[2],drive);
      end_system_call();
      # Hack von DJ (siehe GO32/EXPHDLR.C) und EM (siehe LIB/MISC/_GETCWD1.C):
      # wandelt alle '\' in '/' und alle Groß- in Kleinbuchstaben (nur Kosmetik,
      # da DOS und unser PARSE-NAMESTRING auch Filenamen mit '/' statt '\'
      # verstehen).
      # in Pathname umwandeln:
      return asciz_dir_to_pathname(&path_buffer[0]);
    }

#endif

# UP: Füllt Default-Drive und Default-Directory in einen Pathname ein.
# use_default_dir(pathname)
# > pathname: Pathname mit Device /= :WILD
# < ergebnis: neuer absoluter Pathname
# kann GC auslösen
  local object use_default_dir (object pathname);
  local object use_default_dir(pathname)
    var reg4 object pathname;
    { # erst den Pathname kopieren:
      pathname = copy_pathname(pathname);
      pushSTACK(pathname);
      # Stackaufbau: pathname.
     #ifdef PATHNAME_ATARI
      # Bestimme das betreffende Laufwerk und die Seriennummer der
      # darin eingelegten Diskette, und prüfe, ob sie mit der Seriennummer
      # im Pathname zusammenpaßt:
     {var reg5 object disk_seriennummer; # Seriennummer der im Laufwerk drive
                                         # gerade eingelegten Diskette.
      retry_disk:
      pathname = STACK_0;
      # evtl. das Default-Drive nehmen:
      if (nullp(ThePathname(pathname)->pathname_device))
        { ThePathname(pathname)->pathname_device = O(default_drive); }
      { # Laufwerksbuchstaben holen:
        var reg7 uintB drive = TheSstring(ThePathname(pathname)->pathname_device)->data[0];
        # Alisteneintrag zum Drive holen:
        pushSTACK(get_drive_alist(pathname));
        # Stackaufbau: pathname, Alisteneintrag.
        # Dann die Seriennummer der eingelegten Diskette holen:
        disk_seriennummer = get_disk_number(drive);
        # Dann die Seriennummern vergleichen:
       {var reg1 object path_seriennummer = Car(ThePathname(pathname=STACK_1)->pathname_directory);
        # path_seriennummer = in Pathname verlangte Seriennummer
        if (!(nullp(path_seriennummer) # keine verlangt ?
              || (eq(path_seriennummer,disk_seriennummer)) # oder beide gleich ?
           ) )
          { # Nein: Die verlangte Diskette ist nicht die eingelegte Diskette.
            # -> Continuable Error liefern:
            skipSTACK(1); # Alisteneintrag vergessen
            # (CERROR "Es geht weiter." "Legen Sie bitte die Diskette mit der Nummer ~D in Laufwerk ~A." Seriennummer Laufwerk)
            pushSTACK(O(otherdisk_string1)); # "Es geht weiter."
            pushSTACK(O(otherdisk_string2)); # "Legen Sie bitte die Diskette mit der Nummer ~D in Laufwerk ~A."
            pushSTACK(path_seriennummer); # Seriennummer
            pushSTACK(ThePathname(pathname)->pathname_device); # Laufwerk als String
            funcall(S(cerror),4);
            # und erneut versuchen:
            goto retry_disk;
      }}  }
      # Stackaufbau: pathname, Alisteneintrag.
      # Dann das Default-Directory zu dieser Seriennummer bestimmen:
      { var reg6 object default_dir; # Default-Directory (ein Pathname)
        {var reg1 object alistr = STACK_0; # Alisteneintrag durchlaufen
         while (consp(alistr=Cdr(alistr)))
           { var reg1 object entry = Car(alistr);
             if (eq(Car(entry),disk_seriennummer))
               { default_dir = Cdr(entry); goto default_dir_ok; }
        }  }
        # Bisher (auf diesem Drive) unbekannte Diskette.
        # Default-Directory := '\' :
        {var reg1 object new_cons = allocate_cons(); # Neues Cons
         Car(new_cons) = disk_seriennummer; # (cons seriennummer nil)
         pushSTACK(new_cons); # retten
         default_dir = allocate_pathname(); # neuer Pathname mit Name=NIL und Typ=NIL
         ThePathname(default_dir)->pathname_directory = popSTACK(); # mit Directory (cons seriennummer nil)
         ThePathname(default_dir)->pathname_device = ThePathname(STACK_1)->pathname_device;
         pushSTACK(default_dir); # retten
         new_cons = allocate_cons(); # neues Cons
         Car(new_cons) = disk_seriennummer; Cdr(new_cons) = STACK_0;
         pushSTACK(new_cons); # (cons seriennummer default_dir) retten
         new_cons = allocate_cons(); # neues Cons
         Car(new_cons) = popSTACK();
         # new_cons = (list (cons seriennummer default_dir))
         default_dir = popSTACK();
         # Alisteneintrag um die 1-elementige Liste new_cons erweitern:
         Cdr(new_cons) = Cdr(STACK_0); Cdr(STACK_0) = new_cons;
        }
        default_dir_ok:
        skipSTACK(1); # Alisteneintrag vergessen
        # Stackaufbau: pathname.
        # default_dir (ein Pathname) und disk_seriennummer (ein Fixnum) sind fertig.
        # Dann das Default-Directory in den Pathname einbauen:
        { var reg3 object subdirs = Cdr(ThePathname(STACK_0)->pathname_directory);
          # Fängt (CDR pathname-directory) mit :RELATIVE an?
          if (eq(Car(subdirs),S(Krelative)))
            { # ja -> Ersetze :RELATIVE durch default-subdirs, d.h.
              # bilde  (append default-subdirs (cdr subdirs))
              #      = (nreconc (reverse default-subdirs) (cdr subdirs))
              pushSTACK(Cdr(subdirs));
             {var reg1 object temp = Cdr(ThePathname(default_dir)->pathname_directory);
              temp = reverse(temp);
              subdirs = nreconc(temp,popSTACK());
            }}
     #endif
     #if defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2)
      # Default fürs Device:
      if (nullp(ThePathname(pathname)->pathname_device)) # kein Device angegeben?
        # Nimm das Default-Drive stattdessen:
        { ThePathname(pathname)->pathname_device = O(default_drive); }
      # Default fürs Directory:
        { var reg3 object subdirs = ThePathname(pathname)->pathname_directory;
          # Fängt pathname-directory mit :RELATIVE an?
          if (eq(Car(subdirs),S(Krelative)))
            # ja -> Ersetze :RELATIVE durch das Default-Directory:
            { var reg5 uintB drive = TheSstring(ThePathname(pathname)->pathname_device)->data[0];
              var reg2 object default_dir = default_directory_of(drive);
              # default_dir (ein Pathname) ist fertig.
              # Ersetze :RELATIVE durch default-subdirs, d.h.
              # bilde  (append default-subdirs (cdr subdirs))
              #      = (nreconc (reverse default-subdirs) (cdr subdirs))
              pushSTACK(Cdr(subdirs));
             {var reg1 object temp = ThePathname(default_dir)->pathname_directory;
              temp = reverse(temp);
              subdirs = nreconc(temp,popSTACK());
            }}
     #endif
          # Liste durchgehen und dabei neu aufconsen, dabei '.\' und '..\'
          # und '...\' verarbeiten (nicht dem DOS bzw. GEMDOS überlassen):
          pushSTACK(subdirs);
          pushSTACK(NIL);
          # Stackaufbau: Pathname, subdir-oldlist, subdir-newlist.
          while (mconsp(STACK_1)) # Bis oldlist am Ende ist:
            { var reg2 object subdir = Car(STACK_1); # nächstes subdir
              if
                 #if defined(PATHNAME_ATARI) || defined(PATHNAME_MSDOS)
                 (eq(subdir,S(Kcurrent)))
                 #else
                 (equal(subdir,O(punkt_string)))
                 #endif
                # = :CURRENT -> newlist unverändert lassen
                {}
              elif
                   #if defined(PATHNAME_ATARI) || defined(PATHNAME_MSDOS)
                   (eq(subdir,S(Kparent)))
                   #else
                   (equal(subdir,O(punktpunkt_string)))
                   #endif
                # = :PARENT -> newlist um eins verkürzen:
                { if (matomp(Cdr(STACK_0))) # newlist (bis auf :ABSOLUTE) leer ?
                    { # :PARENT von "\" aus liefert Error
                      pushSTACK(O(backslash_string)); # "\\"
                      pushSTACK(directory_namestring(STACK_(2+1))); # Directory von pathname
                      fehler(
                             DEUTSCH ? "Directory ~ oberhalb ~ existiert nicht." :
                             ENGLISH ? "no directory ~ above ~" :
                             FRANCAIS ? "Il n'y a pas de répertoire ~ au delà de ~." :
                             ""
                            );
                    }
                  if (eq(Car(STACK_0),S(Kwild))) # newlist fängt mit '...\' an ?
                    { # :PARENT von "...\" aus liefert Error
                      pushSTACK(directory_namestring(STACK_2)); # Directory von pathname
                      fehler( # '"..\\" nach "...\\" ist unzulässig: ~'
                             DEUTSCH ? "\"..\\\\\" nach \"...\\\\\" ist unzulässig: ~" :
                             ENGLISH ? "\"..\\\\\" after \"...\\\\\" is invalid: ~" :
                             FRANCAIS ? "\"..\\\\\" après \"...\\\\\" n'est pas permis : ~" :
                             ""
                            );
                    }
                  STACK_0 = Cdr(STACK_0);
                }
              else # (auch wenn :ABSOLUTE !)
                { # newlist um eins verlängern:
                  pushSTACK(subdir);
                 {var reg1 object new_cons = allocate_cons();
                  Car(new_cons) = popSTACK();
                  Cdr(new_cons) = STACK_0;
                  STACK_0 = new_cons;
                }}
              STACK_1 = Cdr(STACK_1);
            }
          subdirs = nreverse(popSTACK()); # newlist, wieder umdrehen
     #ifdef PATHNAME_ATARI
          STACK_0 = subdirs; # und retten
      } }
      # Stackaufbau: pathname, subdirs.
      {var reg1 object new_cons = allocate_cons(); # neues Cons
       Car(new_cons) = disk_seriennummer; Cdr(new_cons) = popSTACK();
       # new_cons = (cons disk_seriennummer subdirs)
       # in den Pathname eintragen:
       pathname = popSTACK();
       ThePathname(pathname)->pathname_directory = new_cons;
     }}
     #endif
     #if defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2)
          skipSTACK(1);
          # Stackaufbau: pathname.
          pathname = popSTACK();
          ThePathname(pathname)->pathname_directory = subdirs; # in den Pathname eintragen
        }
     #endif
      return pathname;
    }

#ifdef PATHNAME_ATARI

# UP: Stellt sicher, daß das Directory eines Pathname existiert.
# Sonst Fehlermeldung.
# Grund:
#   GEMDOS ist dazu nach Diskettenwechsel nicht in der Lage und interpretiert
#   manchmal (nach Diskettenwechsel) beliegige Dateiinhalte als Directories.
#   Daher müssen wir GEMDOS auf die Sprünge helfen und bei jedem Unter-
#   Directory selber testen, ob es existiert.
# assure_dir_exists()
# > STACK_0: absoluter Pathname ohne Wildcards im Directory
# < ergebnis:
#     falls Name=NIL: Directory-Namestring (für GEMDOS, ohne Seriennummer)
#     falls Name/=NIL: Namestring (für GEMDOS, ohne Seriennummer, mit Nullbyte am Schluß)
# kann GC auslösen
  local object assure_dir_exists (void);
  local object assure_dir_exists()
    { {var reg1 object pathname = STACK_0;
       pushSTACK(Cdr(ThePathname(pathname)->pathname_directory)); # subdir-list (ohne Seriennummer)
       {pushSTACK(ThePathname(pathname)->pathname_device); # Device
        pushSTACK(O(doppelpunkt_string)); # ":"
        pushSTACK(O(backslash_string)); # "\\"
        pushSTACK(string_concat(3)); # zusammenhängen
      }}
      # Stackaufbau: Pathname, verlängerte subdir-list, Directory-Namestring.
      while (consp(STACK_1=Cdr(STACK_1)))
        { # Unterdirectory-Namestring aufbauen:
          {# (Erster String bereits in STACK_0, weitere zum subdir folgen:)
           var reg1 uintC stringcount = 1 + subdir_namestring_parts(STACK_1);
           pushSTACK(string_concat(stringcount));
          }
          {# in ASCIZ-String umwandeln:
           var reg2 uintB* asciz = TheAsciz(string_to_asciz(STACK_0));
           # Dateisuche gemäß GEMDOS-Konvention:
           var reg1 sintW errorcode;
           set_break_sem_4(); # wegen DTA-Buffer gegen Unterbrechungen sperren
           GEMDOS_SetDTA(&DTA_buffer); # DTA-Buffer setzen
           errorcode =
             GEMDOS_Sfirst(asciz,0x10); # Suchanfang, die Maske 0x10 sucht
                                        # nach Ordnern und normalen Dateien.
           if (errorcode == GEMDOS_Sfirst_notfound) # 'Keine Datei gefunden' ?
             goto not_exists;
           if (errorcode < 0) { OS_error(errorcode); } # sonstigen Error melden
           if (!(DTA_buffer.d_attrib & 0x10)) # gefundene Datei kein Unterdirectory ?
             { not_exists:
               clr_break_sem_4();
               fehler_dir_not_exists(STACK_2);
             }
           clr_break_sem_4();
          }
          # Directory-Namestring zu Ende aufbauen:
          pushSTACK(O(backslash_string)); # "\\" an STACK_0 anhängen
          pushSTACK(string_concat(2));
        }
      {var reg1 object dir_namestring = popSTACK(); # Directory-Namestring
       skipSTACK(1);
       if (namenullp(STACK_0))
         { return dir_namestring; }
         else
         { return namestring_asciz(dir_namestring); }
    } }

#endif

#if defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2)

# UP: Stellt sicher, daß das Directory eines Pathname existiert.
# Sonst Fehlermeldung.
# assure_dir_exists()
# > STACK_0: absoluter Pathname ohne Wildcards im Directory
# < ergebnis:
#     falls Name=NIL: Directory-Namestring (für DOS)
#     falls Name/=NIL: Namestring (für DOS, mit Nullbyte am Schluß)
# kann GC auslösen
  local object assure_dir_exists (void);
  local object assure_dir_exists()
    { var reg2 uintC stringcount = directory_namestring_parts(STACK_0); # Strings fürs Directory
      var reg1 object dir_namestring = string_concat(stringcount); # zusammenhängen
      # Existenztest:
      if (!nullp(Cdr(ThePathname(STACK_0)->pathname_directory))) # Subdir-List leer -> OK
        # (Muß abgefangen werden, denn stat() auf Rootdir liefert Fehler.)
        {var struct stat statbuf;
         var reg3 uintB* endptr = &TheSstring(dir_namestring)->data[TheSstring(dir_namestring)->length-1];
         *endptr = '\0'; # '\' am Schluß durch Nullbyte ersetzen
         if (stat(TheAsciz(dir_namestring),&statbuf) < 0) { OS_error(); }
         *endptr = '\\'; # wieder mit '\' abschließen
         if (!S_ISDIR(statbuf.st_mode)) # gefundene Datei kein Unterdirectory ?
           { fehler_dir_not_exists(dir_namestring); }
        }
      if (namenullp(STACK_0))
        { return dir_namestring; }
        else
        { return namestring_asciz(dir_namestring); }
    }

#endif

# UP: Liefert den Directory-Namestring eines Pathname unter der Annahme,
#     daß das Directory dieses Pathname existiert.
# assume_dir_exists()
# > STACK_0: absoluter Pathname ohne Wildcards im Directory
# < ergebnis:
#     falls Name=NIL: Directory-Namestring (für DOS bzw. GEMDOS, ohne Seriennummer)
#     falls Name/=NIL: Namestring (für DOS bzw. GEMDOS, ohne Seriennummer, mit Nullbyte am Schluß)
# kann GC auslösen
  global object assume_dir_exists (void);
  global object assume_dir_exists()
    { var reg2 uintC stringcount =
        #if HAS_SERNR
        directory_namestring_parts(STACK_0,TRUE); # Strings fürs Directory
        #else
        directory_namestring_parts(STACK_0); # Strings fürs Directory
        #endif
      var reg1 object dir_namestring = string_concat(stringcount); # zusammenhängen
      if (namenullp(STACK_0))
        { return dir_namestring; }
        else
        { return namestring_asciz(dir_namestring); }
    }

#endif

#ifdef PATHNAME_AMIGAOS

# UP: Liefert den Truename eines Directory-Locks.
# > set_break_sem_4(): schon ausgeführt
# > lock: Directory-Lock, wird freigegeben
# < ergebnis: Directory (als Pathname)
# kann GC auslösen
  local object directory_truename (BPTR lock);
  local object directory_truename(lock)
    var reg6 BPTR lock;
    { # Von hier aus hochhangeln:
      pushSTACK(NIL); # Subdir-Liste := NIL
      { var LONGALIGNTYPE(struct FileInfoBlock) fib;
        var reg5 struct FileInfoBlock * fibptr = LONGALIGN(&fib);
        loop
          { # Directory selbst ansehen:
            begin_system_call();
           {var reg1 LONG ergebnis = Examine(lock,fibptr);
            end_system_call();
            if (!ergebnis) { OS_error(); }
           }
            # seinen Namen verwenden:
           {var reg4 object name = asciz_to_string(&fibptr->fib_FileName[0]);
            # zum Parent-Directory hochsteigen:
            var reg3 BPTR parentlock;
            begin_system_call();
            parentlock = ParentDir(lock);
            UnLock(lock);
            end_system_call();
            if (!(parentlock==BPTR_NULL))
              # name ist der Name eines Subdirectories
              { # vor die Subdir-Liste pushen:
                pushSTACK(name);
               {var reg1 object new_cons = allocate_cons();
                Car(new_cons) = popSTACK();
                Cdr(new_cons) = STACK_0;
                STACK_0 = new_cons;
               }
                lock = parentlock; # und vom Parent Directory aus weitermachen
              }
              else
              { begin_system_call();
                if (IoErr()) { OS_error(); } # Fehler aufgetreten?
                end_system_call();
                # name ist der Name eines DOS-Volumes.
                pushSTACK(name);
                break;
              }
      }   }}
      clr_break_sem_4(); # Unterbrechungen wieder zulassen
      # Stackaufbau: subdirs, devicename.
     {# subdirs mit :ABSOLUTE anfangen lassen:
      var reg1 object new_cons = allocate_cons();
      Car(new_cons) = S(Kabsolute); Cdr(new_cons) = STACK_1;
      STACK_1 = new_cons;
     }
     {var reg1 object default_dir = allocate_pathname(); # neuer Pathname mit Name=NIL und Typ=NIL
      ThePathname(default_dir)->pathname_device = popSTACK();
      ThePathname(default_dir)->pathname_directory = popSTACK();
      return default_dir;
    }}

# UP: Liefert das aktuelle Directory.
# < ergebnis: aktuelles Directory (als Pathname)
# kann GC auslösen
  local object default_directory (void);
  local object default_directory()
    { # Lock fürs aktuelle Directory holen:
      set_break_sem_4(); # Unterbrechungen währenddessen verhindern
      begin_system_call();
     {var reg1 BPTR lock = Lock("",ACCESS_READ);
      if (lock==BPTR_NULL)
        { if (!(IoErr()==ERROR_OBJECT_NOT_FOUND)) { OS_error(); }
          fehler(
                 DEUTSCH ? "Zugriff auf aktuelles Verzeichnis nicht möglich." :
                 ENGLISH ? "Couldn't access current directory" :
                 FRANCAIS ? "Le répertoire courant n'est pas accessible." :
                 ""
                );
        }
      end_system_call();
      return directory_truename(lock); # macht clr_break_sem_4(); und UnLock(lock);
    }}

# UP: Füllt Default-Directory in einen Pathname ein.
# use_default_dir(pathname)
# > pathname: Pathname
# < ergebnis: neuer absoluter Pathname
# kann GC auslösen
  local object use_default_dir (object pathname);
  local object use_default_dir(pathname)
    var reg3 object pathname;
    { # erst den Pathname kopieren:
      pathname = copy_pathname(pathname);
      # Dann das Default-Directory in den Pathname einbauen:
      { var reg2 object subdirs = ThePathname(pathname)->pathname_directory;
        # Fängt pathname-directory mit :RELATIVE an?
        if (eq(Car(subdirs),S(Krelative)))
          { # ja -> Ersetze :RELATIVE durch default-subdirs, d.h.
            # bilde  (append default-subdirs (cdr subdirs))
            #      = (nreconc (reverse default-subdirs) (cdr subdirs))
            pushSTACK(pathname);
            pushSTACK(Cdr(subdirs));
           {var reg1 object temp = default_directory();
            temp = ThePathname(temp)->pathname_directory;
            temp = reverse(temp);
            subdirs = nreconc(temp,popSTACK());
            pathname = popSTACK();
            # in den Pathname eintragen:
            ThePathname(pathname)->pathname_directory = subdirs;
          }}
      }
      return pathname;
    }

# UP: Macht aus einem Directory-Namestring einen, der für AMIGAOS geeignet ist.
# OSnamestring(namestring)
# > namestring: neu erzeugter Directory-Namestring, mit '/' oder ':' am
#               Schluß, ein Simple-String
# < ergebnis: Namestring zu diesem Directory, im AmigaOS-Format: letzter '/'
#             gestrichen, falls überflüssig, ASCIZ-String
# kann GC auslösen
  local object OSnamestring (object namestring);
  local object OSnamestring(namestring)
    var reg1 object namestring;
    { var reg2 uintL len = TheSstring(namestring)->length;
      if (len==0) goto ok; # Leerstring -> nichts streichen
     {var reg3 uintB ch = TheSstring(namestring)->data[len-1];
      if (!(ch=='/')) goto ok; # kein '/' am Schluß -> nichts streichen
      if (len==1) goto ok; # "/" bedeutet Parent -> nicht streichen
      ch = TheSstring(namestring)->data[len-2];
      if ((ch=='/') || (ch==':')) # davor ein '/' oder ':'
        goto ok; # -> bedeutet Parent -> nicht streichen
      # '/' am Schluß streichen, dann string_to_asciz:
        namestring = copy_string(namestring); # Länge bleibt dabei gleich!
        TheSstring(namestring)->data[len-1] = '\0';
        return namestring;
      ok: # nichts streichen
        return string_to_asciz(namestring);
    }}

# UP: Stellt sicher, daß das Directory eines Pathname existiert.
# assure_dir_exists()
# > STACK_0: Pathname, bei dem Directory kein :RELATIVE enthält.
# > subr_self: Aufrufer (ein SUBR)
# < STACK_0: (evtl. derselbe) Pathname, aber aufgelöst.
# < ergebnis:
#     falls Name=NIL: Directory-Namestring (für AMIGAOS, mit '/' am Schluß)
#     falls Name/=NIL: Namestring (für AMIGAOS, mit Nullbyte am Schluß)
# < filestatus: Falls Name/=NIL: NULL falls das File nicht existiert,
#                                sonst ein Pointer auf eine STAT-Information.
# kann GC auslösen
  local var struct FileInfoBlock * filestatus;
  local object assure_dir_exists (void);
  local object assure_dir_exists()
    { # Zur Auflösung von :PARENTs, die über Root hinaussteigen,
      # müssen wir das Betriebssystem bemühen. Daher:
      var reg3 object dir_namestring;
      {var reg1 uintC stringcount = directory_namestring_parts(STACK_0); # Strings fürs Directory
       dir_namestring = string_concat(stringcount);
      }
      pushSTACK(dir_namestring);
      dir_namestring = OSnamestring(dir_namestring); # ohne überflüssigen '/' am Schluß
      # Lock für dieses Directory holen:
      set_break_sem_4(); # Unterbrechungen währenddessen verhindern
      begin_system_call();
     {var reg4 BPTR lock = Lock(TheAsciz(dir_namestring),ACCESS_READ);
      if (lock==BPTR_NULL)
        { var reg2 LONG errcode = IoErr();
          end_system_call();
          switch (errcode)
            { case ERROR_OBJECT_NOT_FOUND:
                fehler_dir_not_exists(STACK_0);
              case ERROR_ACTION_NOT_KNOWN:
                # Ein Device, bei dem man keine Locks für Subdirectories holen
                # kann! Hierbei muß es sich wohl um ein spezielles Device handeln
                # (PIPE, CON, AUX, etc.).
                # Wir stoppen die Subdirectory-Überprüfungen. Nicht einmal mehr
                # Examine() rufen wir auf. Wir gehen im Gegenteil davon aus, daß
                # das File im gewöhnlichen Sinne (noch) nicht existiert.
                clr_break_sem_4(); # Unterbrechungen zulassen, da wir nun doch kein Lock belegt haben
                if (namenullp(STACK_(0+1))) # kein File angesprochen?
                  { return popSTACK(); } # ja -> fertig
                  else
                  { var reg1 uintC stringcount = 1; # directory_namestring schon auf dem STACK
                    stringcount += file_namestring_parts(STACK_(0+1)); # Strings für den Filename
                    pushSTACK(O(null_string)); stringcount++; # und Nullbyte
                   {var reg2 object namestring = string_concat(stringcount); # zusammenhängen
                    filestatus = (struct FileInfoBlock *)NULL; # File existiert nicht, sagen wir
                    return namestring;
                  }}
              default:
                OS_error();
        }   }
      end_system_call();
      dir_namestring = popSTACK();
      # und überprüfen, ob's ein Directory ist:
      { var LONGALIGNTYPE(struct FileInfoBlock) fib;
        var reg2 struct FileInfoBlock * fibptr = LONGALIGN(&fib);
        begin_system_call();
       {var reg1 LONG ergebnis = Examine(lock,fibptr);
        if (!ergebnis) { UnLock(lock); OS_error(); }
        if (!(fibptr->fib_DirEntryType > 0)) # etwa kein Directory?
          { UnLock(lock);
            end_system_call();
            pushSTACK(dir_namestring);
            pushSTACK(TheSubr(subr_self)->name);
            fehler(
                   DEUTSCH ? "~: ~ ist ein File und kein Directory." :
                   ENGLISH ? "~: ~ names a file, not a directory" :
                   FRANCAIS ? "~ : ~ est un fichier et non un répertoire." :
                   ""
                  );
          }
        end_system_call();
      }}
      # Lock zum Truename machen:
      {var reg1 object new_pathname = directory_truename(lock); # macht clr_break_sem_4();
       var reg2 object old_pathname = STACK_0;
       ThePathname(new_pathname)->pathname_name = ThePathname(old_pathname)->pathname_name;
       ThePathname(new_pathname)->pathname_type = ThePathname(old_pathname)->pathname_type;
       STACK_0 = new_pathname;
     }}
     {var reg4 object pathname = STACK_0;
      # Information zum angesprochenen File holen:
      if (namenullp(pathname)) # kein File angesprochen?
        { return directory_namestring(pathname); } # ja -> fertig
      { var reg2 uintC stringcount = 0;
        stringcount += directory_namestring_parts(pathname); # Strings fürs Directory
        stringcount += file_namestring_parts(pathname); # Strings für den Filename
        pushSTACK(O(null_string)); stringcount++; # und Nullbyte
       {var reg1 object namestring = string_concat(stringcount); # zusammenhängen
        # Lock für dieses File holen:
          begin_system_call();
        { var reg3 BPTR lock = Lock(TheAsciz(namestring),ACCESS_READ);
          if (lock==BPTR_NULL)
            { if (!(IoErr()==ERROR_OBJECT_NOT_FOUND)) { OS_error(); }
              end_system_call();
              # File existiert nicht.
              filestatus = (struct FileInfoBlock *)NULL; return namestring;
            }
          end_system_call();
          # File existiert.
          # Information holen:
         {local var LONGALIGNTYPE(struct FileInfoBlock) status;
          var reg1 struct FileInfoBlock * statusptr = LONGALIGN(&status);
          begin_system_call();
          if (! Examine(lock,statusptr) ) { UnLock(lock); OS_error(); }
          UnLock(lock);
          end_system_call();
          if (statusptr->fib_DirEntryType > 0) # Ist es ein Directory?
            { pushSTACK(TheSubr(subr_self)->name);
              STACK_1 = whole_namestring(STACK_1);
              fehler(
                     DEUTSCH ? "~: ~ ist ein Directory und kein File." :
                     ENGLISH ? "~: ~ names a directory, not a file" :
                     FRANCAIS ? "~ : ~ désigne un répertoire et non un fichier." :
                     ""
                    );
            }
            else
            # normales File
            { filestatus = statusptr; return namestring; }
     }}}}}
    }

# Dasselbe unter der Annahme, daß das Directory bereits existiert.
# (Keine Vereinfachung, da wir ja den Truename bestimmen müssen.)
  global object assume_dir_exists (void);
  global object assume_dir_exists()
    { subr_self = L(open); return assure_dir_exists(); }

#endif

#ifdef PATHNAME_UNIX

# UP: Liefert das aktuelle Directory.
# < ergebnis: aktuelles Directory (als Pathname)
# kann GC auslösen
  local object default_directory (void);
  local object default_directory()
    # Working Directory (von UNIX) ist das aktuelle Directory:
    { var char path_buffer[MAXPATHLEN]; # vgl. GETWD(3)
      # Working Directory in path_buffer ablegen:
      begin_system_call();
      if ( getwd(&path_buffer[0]) ==NULL)
        { pushSTACK(asciz_to_string(&path_buffer[0])); # Meldung
          fehler(
                 DEUTSCH ? "UNIX-Fehler bei GETWD: ~" :
                 ENGLISH ? "UNIX error while GETWD: ~" :
                 FRANCAIS ? "Erreur UNIX pendant GETWD : ~" :
                 "~"
                );
        }
      end_system_call();
      # Es muß mit '/' anfangen:
      if (!(path_buffer[0] == '/'))
        { pushSTACK(asciz_to_string(&path_buffer[0]));
          fehler(
                 DEUTSCH ? "UNIX GETWD lieferte ~" :
                 ENGLISH ? "UNIX GETWD returned ~" :
                 FRANCAIS ? "GETWD d'UNIX a retourné ~" :
                 ""
                );
        }
      # in Pathname umwandeln:
      return asciz_dir_to_pathname(&path_buffer[0]);
    }

# UP: Füllt Default-Directory in einen Pathname ein.
# use_default_dir(pathname)
# > pathname: Pathname
# < ergebnis: neuer Pathname, bei dem Directory kein :RELATIVE enthält.
#             (kurz: "absoluter Pathname")
# kann GC auslösen
  local object use_default_dir (object pathname);
  local object use_default_dir(pathname)
    var reg3 object pathname;
    { # erst den Pathname kopieren:
      pathname = copy_pathname(pathname);
      # Dann das Default-Directory in den Pathname einbauen:
      { var reg2 object subdirs = ThePathname(pathname)->pathname_directory;
        # Fängt pathname-directory mit :RELATIVE an?
        if (eq(Car(subdirs),S(Krelative)))
          { # ja -> Ersetze :RELATIVE durch default-subdirs, d.h.
            # bilde  (append default-subdirs (cdr subdirs))
            #      = (nreconc (reverse default-subdirs) (cdr subdirs))
            pushSTACK(pathname);
            pushSTACK(Cdr(subdirs));
           {var reg1 object temp = default_directory();
            temp = ThePathname(temp)->pathname_directory;
            temp = reverse(temp);
            subdirs = nreconc(temp,popSTACK());
            pathname = popSTACK();
            # in den Pathname eintragen:
            ThePathname(pathname)->pathname_directory = subdirs;
          }}
      }
      return pathname;
    }

# UP: Stellt sicher, daß das Directory eines Pathname existiert, und löst
# dabei symbolische Links auf.
# assure_dir_exists()
# > STACK_0: Pathname, bei dem Directory kein :RELATIVE enthält.
# > subr_self: Aufrufer (ein SUBR)
# < STACK_0: (evtl. derselbe) Pathname, wobei weder fürs Directory noch
#            für den Filenamen ein symbolisches Link zu verfolgen ist.
# < ergebnis:
#     falls Name=NIL: Directory-Namestring (für UNIX, mit '/' am Schluß)
#     falls Name/=NIL: Namestring (für UNIX, mit Nullbyte am Schluß)
# < filestatus: Falls Name/=NIL: NULL falls das File nicht existiert,
#                                sonst ein Pointer auf eine STAT-Information.
# kann GC auslösen
  local var struct stat * filestatus;
  local object assure_dir_exists (void);
  local object assure_dir_exists()
    { var reg6 uintC allowed_links = MAXSYMLINKS; # Anzahl der noch erlaubten symbolischen Links
      loop # Schleife über die aufzulösenden symbolischen Links
        { # Truepath des Directory bestimmen:
          var char path_buffer[MAXPATHLEN]; # vgl. REALPATH(3)
          { var reg2 uintC stringcount = directory_namestring_parts(STACK_0); # Strings zum Directory
            pushSTACK(O(punkt_string)); # und "."
            pushSTACK(O(null_string)); # und Nullbyte
           {var reg1 object string = string_concat(stringcount+1+1); # zusammenhängen
            # symbolische Links darin auflösen:
            begin_system_call();
            if ( realpath(TheAsciz(string),&path_buffer[0]) ==NULL)
              { end_system_call();
                if (!(errno==ENOENT)) { OS_error(); }
                fehler_dir_not_exists(asciz_dir_to_pathname(&path_buffer[0])); # fehlerhafte Komponente
              }
            end_system_call();
          }}
          # Neuer Directory-Path muß mit '/' anfangen:
          if (!(path_buffer[0] == '/'))
            { pushSTACK(asciz_to_string(&path_buffer[0]));
              fehler(
                     DEUTSCH ? "UNIX REALPATH lieferte ~" :
                     ENGLISH ? "UNIX REALPATH returned ~" :
                     FRANCAIS ? "REALPATH d'UNIX a retourné ~" :
                     ""
                    );
            }
          # Am Schluß evtl. ein '/' anfügen:
          {var reg1 char* pathptr = &path_buffer[0];
           var reg2 uintL len = 0; # Stringlänge
           until (*pathptr == 0) { pathptr++; len++; } # ASCIZ-Stringende suchen
           if (!((len>0) && (pathptr[-1]=='/')))
             { *pathptr = '/'; len++; } # ein '/' anfügen
          # und in einen String umwandeln:
           { var reg4 object new_string = make_string((uintB*)(&path_buffer[0]),len);
          # Pathname draus machen und dessen Directory verwenden:
            {var reg3 object new_pathname = coerce_pathname(new_string);
             ThePathname(STACK_0)->pathname_directory
               = ThePathname(new_pathname)->pathname_directory;
          }}}
          # Information zum angesprochenen File holen:
          if (namenullp(STACK_0)) # kein File angesprochen?
            { return directory_namestring(STACK_0); } # ja -> fertig
          { var reg5 object pathname = STACK_0;
            var reg2 uintC stringcount = 0;
            stringcount += directory_namestring_parts(pathname); # Strings fürs Directory
            stringcount += file_namestring_parts(pathname); # Strings für den Filename
            pushSTACK(O(null_string)); stringcount++; # und Nullbyte
           {var reg1 object namestring = string_concat(stringcount); # zusammenhängen
            # Information holen:
            local struct stat status;
            begin_system_call();
            if (!( lstat(TheAsciz(namestring),&status) ==0))
              { if (!(errno==ENOENT)) { OS_error(); }
                # File existiert nicht.
                end_system_call();
                filestatus = (struct stat *)NULL; return namestring;
              }
            end_system_call();
            # File existiert.
            if (S_ISDIR(status.st_mode)) # Ist es ein Directory?
              { pushSTACK(TheSubr(subr_self)->name);
                STACK_1 = whole_namestring(STACK_1);
                fehler(
                       DEUTSCH ? "~: ~ ist ein Directory und kein File." :
                       ENGLISH ? "~: ~ names a directory, not a file" :
                       FRANCAIS ? "~ : ~ est un répertoire et non un fichier." :
                       ""
                      );
              }
            elif (S_ISLNK(status.st_mode)) # Ist es ein symbolisches Link?
              # ja -> weiterverfolgen:
              {  if (allowed_links==0) # keine Links mehr erlaubt?
                   { errno = ELOOP; OS_error(); } # ja -> UNIX-Error ELOOP simulieren
                 allowed_links--; # danach ist ein Link weniger erlaubt
               { var reg4 uintL linklen = status.st_size; # Länge des Link-Inhalts
                 pushSTACK(namestring); # Namestring retten
                {var reg3 object linkbuf = allocate_string(linklen); # Buffer für den Link-Inhalt
                 namestring = popSTACK();
                 # Link-Inhalt lesen:
                 begin_system_call();
                 if (!( readlink(TheAsciz(namestring),TheAsciz(linkbuf),linklen) ==linklen))
                   { OS_error(); }
                 end_system_call();
                 # Daraus ein Pathname machen:
                 # (MERGE-PATHNAMES (PARSE-NAMESTRING linkbuf) pathname)
                 pushSTACK(linkbuf); funcall(L(parse_namestring),1);
                 pushSTACK(value1); pushSTACK(STACK_(0+1)); funcall(L(merge_pathnames),2);
                 STACK_0 = value1;
              }}}
            else
              # normales File
              { filestatus = &status; return namestring; }
          }}
    }   }

# Dasselbe unter der Annahme, daß das Directory bereits existiert.
# (Keine Vereinfachung, da das File ein symbolisches Link in ein anderes
# Directory sein kann, und dieses muß dann als existent überprüft werden.)
  global object assume_dir_exists (void);
  global object assume_dir_exists()
    { subr_self = L(open); return assure_dir_exists(); }

#endif

#ifdef PATHNAME_VMS

# UP: Liefert das aktuelle Directory.
# < ergebnis: aktuelles Directory (als Pathname)
# kann GC auslösen
  local object default_directory (void);
  local object default_directory()
    # Working Directory ist das aktuelle Directory:
    { var char path_buffer[MAXPATHLEN]; # vgl. GETWD(3)
      # Working Directory in path_buffer ablegen:
      begin_system_call();
      if ( getwd(&path_buffer[0]) ==NULL)
        { pushSTACK(asciz_to_string(&path_buffer[0])); # Meldung
          fehler(
                 DEUTSCH ? "Fehler bei GETWD: ~" :
                 ENGLISH ? "error while GETWD: ~" :
                 FRANCAIS ? "Erreur pendant GETWD : ~" :
                 "~"
                );
        }
      end_system_call();
      # in Pathname umwandeln:
      return asciz_dir_to_pathname(&path_buffer[0]);
    }

# UP: Füllt Default-Directory in einen Pathname ein.
# use_default_dir(pathname)
# > pathname: Pathname
# < ergebnis: neuer Pathname, bei dem Directory kein :RELATIVE enthält.
#             (kurz: "absoluter Pathname")
# kann GC auslösen
  local object use_default_dir (object pathname);
  local object use_default_dir(pathname)
    var reg3 object pathname;
    { # (merge-pathnames pathname default-directory) ausführen:
      pushSTACK(pathname); pushSTACK(default_directory());
      funcall(L(merge_pathnames),2);
      return value1;
    }

# UP: Stellt sicher, daß das Directory eines Pathname existiert.
# assure_dir_exists()
# > STACK_0: Pathname, bei dem Directory kein :RELATIVE enthält.
# > subr_self: Aufrufer (ein SUBR)
# < STACK_0: (evtl. derselbe) Pathname.
# < ergebnis:
#     falls Name=NIL: Host+Directory-Namestring
#     falls Name/=NIL: Namestring (für VMS, mit Nullbyte am Schluß)
# < filestatus: Falls Name/=NIL: NULL falls das File nicht existiert,
#                                sonst ein Pointer auf eine STAT-Information.
# kann GC auslösen
  local var struct stat * filestatus;
  local object assure_dir_exists (void);
  local object assure_dir_exists()
    { # Wir tun hier in Wirklichkeit gar nichts. Besser machen??
      # Information zum angesprochenen File holen:
      if (namenullp(STACK_0)) # kein File angesprochen?
        { var reg2 uintC stringcount = host_namestring_parts(pathname); # Strings für den Host
          stringcount += directory_namestring_parts(pathname); # Strings fürs Directory
          return string_concat(stringcount); # zusammenhängen und fertig
        }
      { var reg5 object pathname = STACK_0;
        var reg2 uintC stringcount = host_namestring_parts(pathname); # Strings für den Host
        stringcount += directory_namestring_parts(pathname); # Strings fürs Directory
        stringcount += file_namestring_parts(pathname); # Strings für den Filename
        pushSTACK(O(null_string)); stringcount++; # und Nullbyte
       {var reg1 object namestring = string_concat(stringcount); # zusammenhängen
        # Information holen:
        local struct stat status;
        begin_system_call();
        if (!( stat(TheAsciz(namestring),&status) ==0))
          { if (!(errno==ENOENT)) { OS_error(); }
            # File existiert nicht.
            end_system_call();
            filestatus = (struct stat *)NULL; return namestring;
          }
        end_system_call();
        # File existiert.
        if (S_ISDIR(status.st_mode)) # Ist es ein Directory?
          { pushSTACK(TheSubr(subr_self)->name);
            STACK_1 = whole_namestring(STACK_1);
            fehler(
                   DEUTSCH ? "~: ~ ist ein Directory und kein File." :
                   ENGLISH ? "~: ~ names a directory, not a file" :
                   FRANCAIS ? "~ : ~ est un répertoire et non un fichier." :
                   ""
                  );
          }
          else
          # normales File
          { # Truename bilden:
            { var char esn[NAM$C_MAXRSS];
              var char rsn[NAM$C_MAXRSS];
              var struct NAM nam = cc$rms_nam;
              nam.nam$l_esa = esn; nam.nam$b_ess = sizeof(esn);
              nam.nam$l_rsa = rsn; nam.nam$b_rss = sizeof(rsn);
             {var struct FAB fab = cc$rms_fab;
              fab.fab$l_fna = TheAsciz(namestring); fab.fab$b_fns = asciz_length(TheAsciz(namestring));
              fab.fab$l_nam = &nam;
              fab.fab$l_fop = FAB$M_NAM;
              { var reg1 int status;
                status = SYS$PARSE(&fab);
                if (!(status&1)) # Error?
                  { errno = EVMSERR; vaxc$errno = status; OS_error(); }
                #if 0 # unnötig
                status = SYS$SEARCH(&fab);
                if (!(status&1)) # Error?
                  { errno = EVMSERR; vaxc$errno = status; OS_error(); }
                #endif
              }
              # Namen namptr->nam$l_rsa[0..namptr->nam$b_rsl-1] herauskopieren:
              namestring = make_string((uintB*)&nam.nam$l_rsa[0],nam.nam$b_rsl);
            }}
            # und in Pathname umwandeln:
            { var reg5 object pathname = STACK_0 = coerce_pathname(namestring);
              var reg2 uintC stringcount = host_namestring_parts(pathname); # Strings für den Host
              stringcount += directory_namestring_parts(pathname); # Strings fürs Directory
              stringcount += file_namestring_parts(pathname); # Strings für den Filename
              pushSTACK(O(null_string)); stringcount++; # und Nullbyte
             {var reg1 object namestring = string_concat(stringcount); # zusammenhängen
              filestatus = &status;
              return namestring;
          } }}
    } }}

# Dasselbe unter der Annahme, daß das Directory bereits existiert.
  global object assume_dir_exists (void);
  global object assume_dir_exists()
    { subr_self = L(open); return assure_dir_exists(); }

#endif

#ifdef PATHNAME_ATARI
# UP: Setzt das Default-Drive und sein Default-Directory neu.
# change_default();
# > STACK_0: absoluter Pathname, bei dem Name und Typ =NIL sind.
# kann GC auslösen
  local void change_default (void);
  local void change_default()
    { # Default-Directory zu diesem Drive neu setzen:
      var reg3 object alist = get_drive_alist(STACK_0); # Alisteneintrag zu diesem Device
      var reg2 object seriennummer = Car(ThePathname(STACK_0)->pathname_directory);
      {var reg1 object alistr = alist; # Alisteneintrag durchlaufen
       while (consp(alistr=Cdr(alistr)))
         { var reg1 object entry = Car(alistr);
           if (eq(Car(entry),seriennummer))
             { # die gegebene Seriennummer im Alisteneintrag gefunden.
               Cdr(entry) = STACK_0; # (car entry) = seriennummer, (cdr entry) := pathname
               goto default_dir_ok;
             }
      }  }
      # Bisher (auf diesem Drive) unbekannte Diskette.
      pushSTACK(alist); # Alisteneintrag retten
      {var reg1 object new_cons = allocate_cons(); # Neues Cons
       Car(new_cons) = seriennummer; Cdr(new_cons) = STACK_1;
       pushSTACK(new_cons); # (cons seriennummer pathname) retten
       new_cons = allocate_cons(); # neues Cons
       Car(new_cons) = popSTACK();
       # new_cons = (list (cons seriennummer pathname))
       alist = popSTACK();
       # Alisteneintrag um die 1-elementige Liste new_cons erweitern:
       Cdr(new_cons) = Cdr(alist); Cdr(alist) = new_cons;
      }
      default_dir_ok:
      # Default-Drive neu setzen:
      O(default_drive) = ThePathname(STACK_0)->pathname_device;
      # *DEFAULT-PATHNAME-DEFAULTS* neu setzen:
      recalc_defaults_pathname();
    }
#endif
#if defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2)
#if 0 # unbenutzt
# UP: Macht aus einem Directory-Namestring einen, der für DOS geeignet ist.
# OSnamestring(namestring)
# > namestring: neu erzeugter Directory-Namestring, mit '\' am Schluß,
#               ein Simple-String
# < ergebnis: Namestring zu diesem Directory, im DOS-Format: letzter '\'
#             gestrichen, falls überflüssig, ASCIZ-String
# kann GC auslösen
  local object OSnamestring (object namestring);
  local object OSnamestring(namestring)
    var reg1 object namestring;
    { var reg2 uintL len = TheSstring(namestring)->length;
      if (len==0) goto ok; # Leerstring -> nichts streichen
     {var reg3 uintB ch = TheSstring(namestring)->data[len-1];
      if (!(ch=='\\')) goto ok; # kein '\' am Schluß -> nichts streichen
      if (len==1) goto ok; # "\" bedeutet Root -> nicht streichen
      ch = TheSstring(namestring)->data[len-2];
      if ((ch=='\\') || (ch==':')) # davor ein '\' oder ':'
        goto ok; # -> bedeutet Parent -> nicht streichen
      # '\' am Schluß streichen, dann string_to_asciz:
        namestring = copy_string(namestring); # Länge bleibt dabei gleich!
        TheSstring(namestring)->data[len-1] = '\0';
        return namestring;
      ok: # nichts streichen
        return string_to_asciz(namestring);
    }}
#endif
# UP: Setzt das Default-Drive und sein Default-Directory neu.
# change_default();
# > STACK_0: Pathname, bei dem Device ein String ist und Directory kein
#        :RELATIVE, :CURRENT, :PARENT enthält, und Name und Typ =NIL sind.
# kann GC auslösen
  local void change_default (void);
  local void change_default()
    { # Default-Directory zu diesem Drive neu setzen:
      { var reg1 object pathname = STACK_0;
        var reg3 uintC stringcount =
          directory_namestring_parts(pathname); # Strings fürs Directory
        # ohne überflüssiges '\' am Schluß, aber mit Nullbyte am Schluß
        if (mconsp(Cdr(ThePathname(pathname)->pathname_directory)))
          { STACK_0 = O(null_string); }
          else
          { pushSTACK(O(null_string)); stringcount++; }
       {var reg2 object string = string_concat(stringcount); # zusammenhängen
        # Default-Directory ändern:
        begin_system_call();
        if (!( chdir(TheAsciz(string)) ==0)) { OS_error(); }
        end_system_call();
      }}
      # Default-Drive neu setzen:
      O(default_drive) = ThePathname(STACK_0)->pathname_device;
      # *DEFAULT-PATHNAME-DEFAULTS* neu setzen:
      recalc_defaults_pathname();
    }
#endif
#ifdef PATHNAME_AMIGAOS
# UP: Setzt das Default-Directory neu.
# change_default();
# > STACK_0: Pathname, bei dem Directory kein :RELATIVE, :CURRENT, :PARENT
#        enthält, und Name und Typ =NIL sind.
# kann GC auslösen
  local void change_default (void);
  extern BPTR orig_dir_lock; # Lock auf das ursprüngliche Verzeichnis
                             # (das gehört nicht uns, nicht freigeben!)
  local void change_default()
    { var reg3 uintC stringcount =
        directory_namestring_parts(STACK_0); # Strings fürs Directory
      var reg2 object dir_namestring = string_concat(stringcount);
      dir_namestring = OSnamestring(dir_namestring); # Asciz, ohne überflüssigen '/' am Schluß
      # Default-Directory ändern:
      set_break_sem_4();
      begin_system_call();
      {var reg1 BPTR lock = Lock(TheAsciz(dir_namestring),ACCESS_READ);
       if (lock==BPTR_NULL) { OS_error(); }
       lock = CurrentDir(lock); # current directory neu setzen
       # Lock zum alten current directory merken bzw. aufgeben:
       if (orig_dir_lock == BPTR_NONE)
         { orig_dir_lock = lock; }
         else
         { UnLock(lock); }
      }
      end_system_call();
      clr_break_sem_4();
    }
#endif
#if defined(PATHNAME_UNIX) || defined(PATHNAME_VMS)
# UP: Setzt das Default-Directory neu.
# change_default();
# > STACK_0: Pathname, bei dem Directory kein :RELATIVE, :CURRENT, :PARENT
#        enthält, und Name und Typ =NIL sind.
# kann GC auslösen
  local void change_default (void);
  local void change_default()
    { var reg2 uintC stringcount = host_namestring_parts(STACK_0); # Strings für den Host
      stringcount +=  directory_namestring_parts(STACK_0); # Strings fürs Directory
      pushSTACK(O(null_string)); # und Nullbyte
     {var reg1 object string = string_concat(stringcount+1); # zusammenhängen
      # Default-Directory ändern:
      begin_system_call();
      if (!( chdir(TheAsciz(string)) ==0)) { OS_error(); }
      end_system_call();
    }}
#endif

#ifdef PATHNAME_ATARI
LISPFUN(namestring,1,1,norest,nokey,0,NIL)
# (NAMESTRING pathname), CLTL S. 417
# (NAMESTRING pathname t) -> Namestring im GEMDOS-Format (ohne Seriennummer)
  { var reg2 object flag = popSTACK(); # optionales Argument flag
    var reg1 object pathname = coerce_pathname_arg(); # Argument zu einem Pathname machen
    if (eq(flag,unbound) || nullp(flag))
      # normal
      { value1 = whole_namestring(pathname); mv_count=1; }
      else
      # flag /= NIL -> für GEMDOS:
      { check_no_wildcards(pathname); # mit Wildcards -> Fehler
        pathname = use_default_dir(pathname); # Default-Directory einfügen
        # (da GEMDOS das Default-Directory von LISP nicht kennt)
       {var reg2 uintC stringcount;
        stringcount = directory_namestring_parts(pathname,TRUE); # Strings fürs Directory
        stringcount += file_namestring_parts(pathname); # Strings für den Filename
        value1 = string_concat(stringcount); mv_count=1; # zusammenhängen
      }}
  }
#else
LISPFUNN(namestring,1)
# (NAMESTRING pathname), CLTL S. 417
  { var reg1 object pathname = coerce_pathname_arg(); # Argument zu einem Pathname machen
    value1 = whole_namestring(pathname); mv_count=1;
  }
#endif

# Fehlermeldung wegen fehlendem Dateinamen
# fehler_noname(pathname);
# > pathname: Pathname
  local nonreturning void fehler_noname (object pathname);
  local nonreturning void fehler_noname(pathname)
    var reg1 object pathname;
    { pushSTACK(pathname);
      fehler(
             DEUTSCH ? "Dateiname muß angegeben werden: ~" :
             ENGLISH ? "no file name given: ~" :
             FRANCAIS ? "Un nom de fichier doit être fourni : ~" :
             ""
            );
    }

# Test, ob ein File existiert:
# if_file_exists(namestring,statement1,statement2);
# > vorausgegangen: assure_dir_exists()
# > im STACK: Pathname, wie nach Ausführung von assure_dir_exists(), Name/=NIL
# > namestring: dessen Namestring als ASCIZ-String
# Falls das File existiert, wird statement1 ausgeführt, sonst statement2.
  #ifdef ATARI
    #define if_file_exists(namestring,statement1,statement2)  \
      {{var reg2 sintW errorcode;                                               \
        errorcode = # Datei zu öffnen versuchen, Modus 0 (Read)                 \
          GEMDOS_open(TheAsciz(namestring),0);                                  \
        if (errorcode == GEMDOS_open_NotFound) # nicht gefunden?                \
          goto not_exists;                                                      \
        if (errorcode < 0) { OS_error(errorcode); } # sonstigen Error melden   \
        # Nun enthält errorcode das Handle des geöffneten Files.                \
        errorcode = # Datei gleich wieder schließen                             \
          GEMDOS_close(errorcode);                                              \
        if (errorcode < 0) { OS_error(errorcode); } # Error melden             \
       }                                                                        \
       if (TRUE) { statement1; } else { not_exists: statement2; }               \
      }
  #else
    #define if_file_exists(namestring,statement1,statement2)  \
      { if (file_exists(namestring)) { statement1; } else { statement2; } }
    #ifdef MSDOS
      #define file_exists(namestring)  (access(TheAsciz(namestring),0)==0)
    #endif
    #ifdef AMIGAOS
      #define file_exists(namestring)  (!(filestatus == (struct FileInfoBlock *)NULL))
    #endif
    #if defined(UNIX) || defined(VMS)
      #define file_exists(namestring)  (!(filestatus == (struct stat *)NULL))
    #endif
  #endif

# Fehlermeldung wegen nicht existenter Datei
# fehler_file_not_exists();
# > STACK_0: Pathname
# > subr_self: Aufrufer (ein SUBR)
  local nonreturning void fehler_file_not_exists (void);
  local nonreturning void fehler_file_not_exists()
    { # Pathname schon in STACK_0
      pushSTACK(TheSubr(subr_self)->name);
      fehler(
             DEUTSCH ? "~: Datei ~ existiert nicht." :
             ENGLISH ? "~: file ~ does not exist" :
             FRANCAIS ? "~ : Le fichier ~ n'existe pas." :
             ""
            );
    }

LISPFUNN(truename,1)
# (TRUENAME pathname), CLTL S. 413
  { var reg1 object pathname = popSTACK(); # pathname-Argument
    if (streamp(pathname))
      # Stream -> extra behandeln:
      { # muß File-Stream sein:
        if_strm_file_p(pathname, ; , fehler_thing(pathname); );
        # Streamtyp File-Stream
        value1 = TheStream(pathname)->strm_file_truename;
      }
      else
      { pathname = coerce_pathname(pathname); # zu einem Pathname machen
        check_no_wildcards(pathname); # mit Wildcards -> Fehler
        pathname = use_default_dir(pathname); # Default-Directory einfügen
        pushSTACK(pathname);
       {# Directory muß existieren:
        var reg3 object namestring = assure_dir_exists(); # Filename als ASCIZ-String
        if (namenullp(STACK_0))
          # Kein Name angegeben
          { if (!nullp(ThePathname(STACK_0)->pathname_type))
              { # Pathname schon in STACK_0
                pushSTACK(TheSubr(subr_self)->name);
                fehler(
                       DEUTSCH ? "~: Pathname mit TYPE, aber ohne NAME sinnlos: ~" :
                       ENGLISH ? "~: pathname with type but without name makes no sense: ~" :
                       FRANCAIS ? "~ : Un PATHNAME avec TYPE mais sans NAME est insensé: ~" :
                       ""
                      );
              }
            # Kein Name und kein Typ angegeben -> pathname als Ergebnis
          }
          else
          # Name angegeben.
          { # Überprüfe, ob die Datei existiert:
            if_file_exists(namestring, ; , { fehler_file_not_exists(); } );
            # Datei existiert -> Pathname als Wert
          }
        value1 = popSTACK();
      }}
    mv_count=1;
  }

LISPFUNN(probe_file,1)
# (PROBE-FILE filename), CLTL S. 424
  { var reg1 object pathname = popSTACK(); # pathname-Argument
    if (streamp(pathname))
      # Stream -> extra behandeln:
      { # muß File-Stream sein:
        if_strm_file_p(pathname, ; , fehler_thing(pathname); );
        # Streamtyp File-Stream -> Truename nehmen:
       {var reg1 uintB flags = TheStream(pathname)->strmflags;
        pathname = TheStream(pathname)->strm_file_truename;
        if (flags & strmflags_open_B) # Datei geöffnet ?
          # ja -> Truename sofort als Ergebnis:
          { value1 = pathname; mv_count=1; return; }
        # nein -> noch testen, ob die Datei zum Truename existiert.
      }}
      else
      { pathname = coerce_pathname(pathname); } # zu einem Pathname machen
    # pathname ist jetzt ein Pathname.
    check_no_wildcards(pathname); # mit Wildcards -> Fehler
    pathname = use_default_dir(pathname); # Default-Directory einfügen
    if (namenullp(pathname)) { fehler_noname(pathname); } # Kein Name angegeben -> Fehler
    # Name angegeben.
    pushSTACK(pathname);
   {# Directory muß existieren:
    var reg3 object namestring = assure_dir_exists(); # Filename als ASCIZ-String
    # Überprüfe, ob die Datei existiert:
    if_file_exists(namestring,
      { value1 = popSTACK(); mv_count=1; }, # Datei existiert -> Pathname als Wert
      { skipSTACK(1); value1 = NIL; mv_count=1; return; } # sonst NIL als Wert
      );
  }}

# UP: Stellt fest, ob eine Datei geöffnet ist.
# openp(pathname)
#if defined(PATHNAME_ATARI) || defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2)
# > pathname: absoluter Pathname, ohne Wildcards.
#endif
#if defined(PATHNAME_AMIGAOS) || defined(PATHNAME_VMS)
# > pathname: absoluter Pathname, ohne Wildcards, ohne :PARENT
#endif
#ifdef PATHNAME_UNIX
# > pathname: absoluter Pathname, ohne Wildcards, nach Auflösung
#             symbolischer Links
#endif
# < ergebnis: TRUE, falls ein geöffneter File-Stream auf diese Datei existiert.
  local boolean openp (object pathname);
  local boolean openp(pathname)
    var reg2 object pathname;
    { var reg1 object flist = O(open_files); # Liste aller offenen Files durchlaufen
      while (consp(flist))
        { var reg3 object f = Car(flist); # nächster offener Stream
          if_strm_file_p(f, # File-Stream ?
            { if (equal(TheStream(f)->strm_file_truename,pathname))
                { return TRUE; }
            },
            ; );
          flist = Cdr(flist);
        }
      return FALSE;
    }

# Fehlermeldung wegen Löschversuch auf geöffnete Datei
# fehler_delete_open(pathname);
# > pathname: Truename der Datei
  local nonreturning void fehler_delete_open (object pathname);
  local nonreturning void fehler_delete_open(pathname)
    var reg1 object pathname;
    { pushSTACK(pathname);
      fehler(
             DEUTSCH ? "Datei ~ kann nicht gelöscht werden, weil ein File-Stream auf sie geöffnet wurde." :
             ENGLISH ? "cannot delete file ~ since there is file stream open to it" :
             FRANCAIS ? "Le fichier ~ ne peut pas être effacé car il est encore ouvert comme «stream»." :
             ""
            );
    }

LISPFUNN(delete_file,1)
# (DELETE-FILE filename), CLTL S. 424
  { var reg1 object pathname = popSTACK(); # pathname-Argument
    if (streamp(pathname))
      # Stream -> extra behandeln:
      { var object stream = pathname;
        # muß File-Stream sein:
        if_strm_file_p(pathname, ; , fehler_thing(pathname); );
        # Streamtyp File-Stream.
        # Falls Datei geöffnet, erst Datei schließen:
        if (TheStream(pathname)->strmflags & strmflags_open_B) # Datei geöffnet ?
          { stream_close(&stream); }
        # Dann den Truename als zu löschende Datei nehmen:
        pathname = TheStream(stream)->strm_file_truename;
      }
      else
      { pathname = coerce_pathname(pathname); } # zu einem Pathname machen
    # pathname ist jetzt ein Pathname.
    check_no_wildcards(pathname); # mit Wildcards -> Fehler
    pathname = use_default_dir(pathname); # Default-Directory einfügen
    if (namenullp(pathname)) { fehler_noname(pathname); } # Kein Name angegeben -> Fehler
    # Name angegeben.
    pushSTACK(pathname);
   {# Directory muß existieren:
    var reg3 object namestring = assure_dir_exists(); # Filename als ASCIZ-String
    if (openp(STACK_0)) { fehler_delete_open(STACK_0); } # Keine offenen Dateien löschen!
    # Datei löschen:
    #ifdef ATARI
    {var reg2 sintW errorcode;
     errorcode = # Datei zu löschen versuchen
       GEMDOS_unlink(TheAsciz(namestring));
     if (errorcode == GEMDOS_open_NotFound) # nicht gefunden -> Wert NIL
       { skipSTACK(1); value1 = NIL; mv_count=1; return; }
     if (errorcode < 0) { OS_error(errorcode); } # sonstigen Error melden
    }
    #endif
    #ifdef AMIGAOS
    if (!file_exists(namestring))
      { skipSTACK(1); value1 = NIL; mv_count=1; return; } # File existiert nicht -> Wert NIL
    begin_system_call();
    if (! DeleteFile(TheAsciz(namestring)) ) { OS_error(); }
    end_system_call();
    #endif
    #if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX) || defined(VMS)
    begin_system_call();
    if (!( unlink(TheAsciz(namestring)) ==0))
      { if (!(errno==ENOENT)) { OS_error(); }
        end_system_call();
        # File existiert nicht -> Wert NIL
        skipSTACK(1); value1 = NIL; mv_count=1; return;
      }
    end_system_call();
    #endif
    # Datei existierte, wurde gelöscht -> Pathname (/=NIL) als Wert
    value1 = popSTACK(); mv_count=1;
  }}

# Fehlermeldung wegen Umbenennungsversuch einer geöffneten Datei
# fehler_rename_open(pathname);
# > pathname: Truename der Datei
  local nonreturning void fehler_rename_open (object pathname);
  local nonreturning void fehler_rename_open(pathname)
    var reg1 object pathname;
    { pushSTACK(pathname);
      fehler(
             DEUTSCH ? "Datei ~ kann nicht umbenannt werden, weil ein File-Stream auf sie geöffnet wurde." :
             ENGLISH ? "cannot rename file ~ since there is file stream open to it" :
             FRANCAIS ? "Le fichier ~ ne peut pas être renommé car il est encore ouvert comme «stream»." :
             ""
            );
    }

# UP: Führt eine Datei-Umbenennung durch.
# rename_file();
# > Stackaufbau: filename, newname, oldpathname.
# < Stackaufbau: filename, newname, oldpathname, newpathname,
#                oldtruename, oldnamestring, newtruename, newnamestring.
  local void rename_file (void);
  local void rename_file()
    { # 1. newpathname := (MERGE-PATHNAMES newname oldpathname)
      { pushSTACK(STACK_1); # newname als 1. Argument
        pushSTACK(STACK_(0+1)); # oldpathname als 2. Argument
        funcall(L(merge_pathnames),2);
        pushSTACK(value1);
      }
      # Stackaufbau: filename, newname, oldpathname, newpathname.
      # 2. oldpathname überprüfen:
      { var reg1 object oldpathname = STACK_1;
        check_no_wildcards(oldpathname); # mit Wildcards -> Fehler
        oldpathname = use_default_dir(oldpathname); # Default-Directory einfügen
        if (namenullp(oldpathname)) { fehler_noname(oldpathname); } # Kein Name angegeben -> Fehler
        # Name angegeben.
        pushSTACK(oldpathname);
       {# Directory muß existieren:
        var reg2 object old_namestring = assure_dir_exists(); # Filename als ASCIZ-String
        if (openp(STACK_0)) { fehler_rename_open(STACK_0); } # Keine offenen Dateien umbenennen!
        pushSTACK(old_namestring);
      }}
      # Stackaufbau: filename, newname, oldpathname, newpathname,
      #              oldtruename, oldnamestring.
      # 3. newpathname überprüfen:
      { var reg1 object newpathname = STACK_2;
        check_no_wildcards(newpathname); # mit Wildcards -> Fehler
        newpathname = use_default_dir(newpathname); # Default-Directory einfügen
        if (namenullp(newpathname)) { fehler_noname(newpathname); } # Kein Name angegeben -> Fehler
        # Name angegeben.
        pushSTACK(newpathname);
       {# Directory muß existieren:
        var reg2 object new_namestring = assure_dir_exists(); # Filename als ASCIZ-String
        pushSTACK(new_namestring);
      }}
      # Stackaufbau: filename, newname, oldpathname, newpathname,
      #              oldtruename, oldnamestring, newtruename, newnamestring.
      # 4. Datei umbenennen:
      #ifdef ATARI
      {var reg1 sintW errorcode;
       errorcode = # Datei umzubenennen versuchen
         GEMDOS_rename(TheAsciz(STACK_2),TheAsciz(STACK_0));
       if (errorcode == GEMDOS_rename_exists) # 'Zugriff verweigert' ?
         # ja -> Datei existiert bereits
         { fehler_file_exists(S(rename_file),STACK_1); }
       if (errorcode < 0) { OS_error(errorcode); } # sonstigen Error melden
      }
      #endif
      #if defined(UNIX) || defined(AMIGAOS) || defined(DJUNIX) || defined(EMUNIX) || defined(VMS)
      if (file_exists(STACK_0))
        # Datei existiert bereits -> nicht ohne Vorwarnung löschen
        { fehler_file_exists(S(rename_file),STACK_1); }
      # Nun kann gefahrlos umbenannt werden:
      begin_system_call();
      #ifdef AMIGAOS
      if (! Rename(TheAsciz(STACK_2),TheAsciz(STACK_0)) ) { OS_error(); }
      #endif
      #if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX) || defined(VMS) # VMS??
      if (!( rename(TheAsciz(STACK_2),TheAsciz(STACK_0)) ==0))
        { OS_error(); }
      #endif
      end_system_call();
      #endif
    }

LISPFUNN(rename_file,2)
# (RENAME-FILE filename newname), CLTL S. 423
  { var reg1 object filename = STACK_1; # filename-Argument
    if (streamp(filename))
      # Stream -> extra behandeln:
      { # muß File-Stream sein:
        if_strm_file_p(filename, ; , fehler_thing(filename); );
        # Streamtyp File-Stream -> Truename verwenden:
        filename = TheStream(filename)->strm_file_truename;
        pushSTACK(filename);
        # Umbenennen:
        rename_file();
        # Stream aktualisieren:
        filename = STACK_7;
        TheStream(filename)->strm_file_name = STACK_4; # newpathname als neuer Name
        TheStream(filename)->strm_file_truename = STACK_1; # newtruename als neuer Truename
        # Handle etc. unverändert lassen
      }
      else
      { filename = coerce_pathname(filename); # zu einem Pathname machen
        pushSTACK(filename);
        # Umbenennen:
        rename_file();
      }
    value1 = STACK_4; # newpathname als 1. Wert
    value2 = STACK_3; # oldtruename als 2. Wert
    value3 = STACK_1; # newtruename als 3. Wert
    mv_count=3; skipSTACK(8); # 3 Werte
  }

# UP: erzeugt ein File-Stream
# open_file(filename,direction,if_exists,if_not_exists,type,eltype_size)
# > filename: Filename, ein Pathname
# > direction: Modus (0 = :PROBE, 1 = :INPUT, 2 = :OUTPUT, 3 = :IO)
# > if_exists: :IF-EXISTS-Argument
#         (0 = nichts, 1 = :ERROR, 2 = NIL,
#          3 = :RENAME,:RENAME-AND-DELETE, 4 = :NEW-VERSION,:SUPERSEDE,
#          5 = :APPEND, 6 = :OVERWRITE)
# > if_not_exists: :IF-DOES-NOT-EXIST-Argument
#         (0 = nichts, 1 = :ERROR, 2 = NIL, 3 = :CREATE)
# > type: nähere Typinfo
#         (STRMTYPE_SCH_FILE oder STRMTYPE_CH_FILE oder
#          STRMTYPE_IU_FILE oder STRMTYPE_IS_FILE)
# > eltype_size: (bei Integer-Streams) Größe der Elemente in Bits,
#         ein Fixnum >0 und <intDsize*uintC_max
# < ergebnis: Stream oder NIL
# kann GC auslösen
  local object open_file (object filename, uintB direction, uintB if_exists, uintB if_not_exists,
                          uintB type, object eltype_size);
  local object open_file(filename,direction,if_exists,if_not_exists,type,eltype_size)
    var reg5 object filename;
    var reg8 uintB direction;
    var reg3 uintB if_exists;
    var reg4 uintB if_not_exists;
    var reg7 uintB type;
    var reg9 object eltype_size;
    { pushSTACK(filename); # Filename retten
      check_no_wildcards(filename); # mit Wildcards -> Fehler
      filename = use_default_dir(filename); # Default-Directory einfügen
      if (namenullp(filename)) { fehler_noname(filename); } # Kein Name angegeben -> Fehler
      pushSTACK(filename); # absPathname retten
      # Stackaufbau: Pathname, absPathname.
      { # Directory muß existieren:
        var reg3 object namestring = assure_dir_exists(); # Filename als ASCIZ-String
        # Stackaufbau: Pathname, Truename.
        # Filename überprüfen und Handle holen:
        var reg2 object handle;
        var reg6 boolean append_flag = FALSE;
        switch (direction)
          { case 0: # Modus ist :PROBE
              #ifdef ATARI
              { # erst mit GEMDOS_open erfragen, ob die Datei existiert:
                var reg1 sintW errorcode;
                errorcode = # Datei zu öffnen versuchen, Modus 0 (Read)
                  GEMDOS_open(TheAsciz(namestring),0);
                if (errorcode == GEMDOS_open_NotFound) # nicht gefunden?
                  # Datei existiert nicht
                  { # :IF-DOES-NOT-EXIST-Argument entscheidet:
                    if (if_not_exists==1) # :ERROR -> Error
                      goto fehler_notfound;
                    if (!(if_not_exists==3)) # nichts oder NIL -> NIL
                      goto ergebnis_NIL;
                    # :CREATE -> Datei mit GEMDOS_create erzeugen (Attribute=0) und schließen:
                    errorcode = GEMDOS_create(TheAsciz(namestring),0);
                  }
                if (errorcode<0) { OS_error(errorcode); } # sonstigen Error melden
                # Datei existiert, errorcode ist das Handle
                # Datei wieder schließen:
                errorcode = GEMDOS_close(errorcode);
                if (errorcode<0) { OS_error(errorcode); } # Error melden
                handle = NIL; # Handle := NIL
                break;
              }
              #endif
              #if defined(UNIX) || defined(AMIGAOS) || defined(DJUNIX) || defined(EMUNIX) || defined(VMS)
              if (!file_exists(namestring))
                # Datei existiert nicht
                { # :IF-DOES-NOT-EXIST-Argument entscheidet:
                  if (if_not_exists==1) # :ERROR -> Error
                    goto fehler_notfound;
                  if (!(if_not_exists==3)) # nichts oder NIL -> NIL
                    goto ergebnis_NIL;
                 {# :CREATE -> Datei mit open erzeugen und schließen:
                  #ifdef AMIGAOS
                  var reg1 Handle handle;
                  begin_system_call();
                  handle = Open(TheAsciz(namestring),MODE_NEWFILE);
                  if (handle == Handle_NULL) { OS_error(); } # Error melden
                  # Datei wurde erzeugt, handle ist das Handle.
                  # Datei wieder schließen:
                  (void) Close(handle);
                  end_system_call();
                  #endif
                  #if defined(DJUNIX) || defined(EMUNIX)
                  var reg1 int ergebnis;
                  begin_system_call();
                  ergebnis = creat(TheAsciz(namestring),my_open_mask);
                  if (ergebnis<0) { OS_error(); } # Error melden
                  setmode(ergebnis,O_BINARY);
                  # Datei wurde erzeugt, ergebnis ist das Handle.
                  # Datei wieder schließen:
                  ergebnis = close(ergebnis);
                  if (!(ergebnis==0)) { OS_error(); } # Error melden
                  end_system_call();
                  #endif
                  #if defined(UNIX) || defined(VMS)
                  var reg1 int ergebnis;
                  begin_system_call();
                  ergebnis = open(TheAsciz(namestring),
                                  O_WRONLY | O_CREAT | O_TRUNC,
                                  my_open_mask
                                 );
                  if (ergebnis<0) { OS_error(); } # Error melden
                  # Datei wurde erzeugt, ergebnis ist das Handle.
                  # Datei wieder schließen:
                  ergebnis = close(ergebnis);
                  if (!(ergebnis==0)) { OS_error(); } # Error melden
                  end_system_call();
                  #endif
                }}
              handle = NIL; # Handle := NIL
              break;
              #endif
            case 1: # Modus ist :INPUT
              #ifdef ATARI
              { # erst mit GEMDOS_open erfragen, ob die Datei existiert:
                var reg1 sintW errorcode;
                errorcode = # Datei zu öffnen versuchen, Modus 0 (Read)
                  GEMDOS_open(TheAsciz(namestring),0);
                if (errorcode == GEMDOS_open_NotFound) # nicht gefunden?
                  # Datei existiert nicht
                  { # :IF-DOES-NOT-EXIST-Argument entscheidet:
                    if (if_not_exists==2) # NIL -> NIL
                      goto ergebnis_NIL;
                    if (!(if_not_exists==3)) # nichts oder :ERROR -> Error
                      goto fehler_notfound;
                    # :CREATE -> Datei mit GEMDOS_create erzeugen (Attribute=0):
                    errorcode = GEMDOS_create(TheAsciz(namestring),0);
                  }
                if (errorcode<0) { OS_error(errorcode); } # sonstigen Error melden
                # Datei existiert, errorcode ist das Handle
                handle = allocate_handle(errorcode); # Handle
                break;
              }
              #endif
              #if defined(DJUNIX) || defined(EMUNIX)
              { # erst mit open erfragen, ob die Datei existiert:
                var reg1 sintW ergebnis;
                # Datei zu öffnen versuchen:
                begin_system_call();
                ergebnis = open(TheAsciz(namestring),O_RDONLY);
                if (ergebnis<0)
                  { if (errno == ENOENT) # nicht gefunden?
                      # Datei existiert nicht
                      { # :IF-DOES-NOT-EXIST-Argument entscheidet:
                        if (if_not_exists==2) # NIL -> NIL
                          goto ergebnis_NIL;
                        if (!(if_not_exists==3)) # nichts oder :ERROR -> Error
                          goto fehler_notfound;
                        # :CREATE -> Datei mit creat erzeugen:
                        ergebnis = creat(TheAsciz(namestring),my_open_mask);
                        if (ergebnis<0) { OS_error(); }
                      }
                      else
                      { OS_error(); } # sonstigen Error melden
                  }
                setmode(ergebnis,O_BINARY);
                end_system_call();
                # Datei existiert, ergebnis ist das Handle
                handle = allocate_handle(ergebnis); # Handle
                break;
              }
              #endif
              #ifdef AMIGAOS
              { # erst mit Open erfragen, ob die Datei existiert:
                var reg1 Handle handl;
                begin_system_call();
                handl = Open(TheAsciz(namestring),MODE_OLDFILE);
                if (handl==Handle_NULL)
                  { if (IoErr()==ERROR_OBJECT_NOT_FOUND)
                      # Datei existiert nicht
                      { # :IF-DOES-NOT-EXIST-Argument entscheidet:
                        if (if_not_exists==2) # NIL -> NIL
                          goto ergebnis_NIL;
                        if (!(if_not_exists==3)) # nichts oder :ERROR -> Error
                          goto fehler_notfound;
                        # :CREATE -> Datei mit Open erzeugen:
                        handl = Open(TheAsciz(namestring),MODE_READWRITE);
                  }   }
                if (handl==Handle_NULL) { OS_error(); } # Error melden
                end_system_call();
                # Datei existiert, handle ist das Handle
                handle = allocate_handle(handl); # Handle als Lisp-Objekt
                break;
              }
              #endif
              #if defined(UNIX) || defined(VMS)
              { var reg2 int o_flags = O_RDONLY;
                if (!file_exists(namestring))
                  # Datei existiert nicht
                  { # :IF-DOES-NOT-EXIST-Argument entscheidet:
                    if (if_not_exists==2) # NIL -> NIL
                      goto ergebnis_NIL;
                    if (!(if_not_exists==3)) # nichts oder :ERROR -> Error
                      goto fehler_notfound;
                    # :CREATE -> Datei mit open erzeugen
                    o_flags |= O_CREAT;
                  }
               {var reg1 int ergebnis;
                begin_system_call();
                ergebnis = open(TheAsciz(namestring),
                                o_flags, # O_RDONLY bzw. O_RDONLY | O_CREAT
                                my_open_mask
                               );
                if (ergebnis<0) { OS_error(); } # Error melden
                end_system_call();
                # Datei existiert, ergebnis ist das Handle
                handle = allocate_handle(ergebnis); # Handle
              }}
              break;
              #endif
            default: # Modus ist :OUTPUT oder :IO
              { # Defaultwert für if_not_exists ist von if_exists abhängig:
                if (if_not_exists==0) # falls if_not_exists nicht angegeben:
                  { if (if_exists<5) # if_exists = :APPEND oder :OVERWRITE -> if_not_exists unverändert
                      { if_not_exists = 3; } # weder :APPEND noch :OVERWRITE -> Default ist :CREATE
                  }
                # Defaultwert für if_exists ist :NEW-VERSION :
                if (if_exists==0) { if_exists = 4; }
                #ifdef ATARI
                # Bei if_exists=4 und if_not_exists=3 kann man sofort
                # CREATE ansteuern, sonst muß man vorher OPEN versuchen:
                if (!((if_exists==4) && (if_not_exists==3)))
                  { var reg1 sintW errorcode;
                    errorcode = # Datei zu öffnen versuchen, Modus 2 (Read/Write)
                      GEMDOS_open(TheAsciz(namestring),2);
                    if (errorcode == GEMDOS_open_NotFound) # nicht gefunden?
                      # Datei existiert nicht
                      { # :IF-DOES-NOT-EXIST-Argument entscheidet:
                        if (if_not_exists<2) # (Default bei :APPEND oder :OVERWRITE) oder :ERROR ?
                          goto fehler_notfound;
                        if (if_not_exists==2) # NIL -> NIL
                          goto ergebnis_NIL;
                        # :CREATE
                      }
                      else
                      if (errorcode<0) { OS_error(errorcode); } # sonstigen Error melden
                      else
                      # Datei existiert, errorcode ist das Handle
                      { # :IF-EXISTS-Argument entscheidet:
                        switch (if_exists)
                          { case 1: # :ERROR -> schließen und Error
                              { errorcode = GEMDOS_close(errorcode);
                                if (errorcode<0) { OS_error(errorcode); } # Error melden
                                goto fehler_exists;
                              }
                            case 2: # NIL -> schließen und NIL
                              { errorcode = GEMDOS_close(errorcode);
                                if (errorcode<0) { OS_error(errorcode); } # Error melden
                                goto ergebnis_NIL;
                              }
                            case 5: # :APPEND
                              append_flag = TRUE; # am Schluß ans Ende positionieren
                            case 6: # :OVERWRITE -> bestehende Datei benutzen
                              handle = allocate_handle(errorcode);
                              goto handle_ok;
                            default: ;
                              # :RENAME, :RENAME-AND-DELETE -> Datei umbenennen und dann neu eröffnen.
                              # :NEW-VERSION, :SUPERSEDE -> Datei auf Länge 0 kürzen.
                          }
                        # In beiden Fällen erst die Datei schließen:
                        errorcode = GEMDOS_close(errorcode);
                        if (errorcode<0) { OS_error(errorcode); } # Error melden
                        if (if_exists==3)
                          # :RENAME oder :RENAME-AND-DELETE -> umbenennen:
                          { # Truename mit ".BAK" erweitern:
                            var reg1 object filename = STACK_0;
                            if (openp(filename)) { fehler_rename_open(filename); } # Keine offenen Dateien umbenennen!
                            pushSTACK(namestring); # namestring retten
                            # filename := (merge-pathnames ".BAK" filename) :
                            filename = copy_pathname(filename); # kopieren
                            ThePathname(filename)->pathname_type = O(backuptype_string); # mit Extension "BAK"
                            if (openp(filename)) { fehler_delete_open(filename); } # Keine offenen Dateien löschen!
                            pushSTACK(filename);
                           {# Directory existiert schon:
                            var reg3 object new_namestring = assume_dir_exists(); # Filename als ASCIZ-String
                            # Datei mit diesem Namen löschen, falls vorhanden:
                            {var reg2 sintW errorcode = # Datei zu löschen versuchen
                               GEMDOS_unlink(TheAsciz(new_namestring));
                             if (!(errorcode == GEMDOS_open_NotFound)) # nicht gefunden -> OK
                               if (errorcode<0) { OS_error(errorcode); } # sonstigen Error melden
                            }
                            # Datei vom alten auf diesen Namen umbenennen:
                            skipSTACK(1);
                            namestring = popSTACK(); # namestring zurück
                            {var reg2 sintW errorcode = # Datei umbenennen
                               GEMDOS_rename(TheAsciz(namestring),TheAsciz(new_namestring));
                             if (errorcode<0) { OS_error(errorcode); } # Error melden
                          }}}
                      }
                  }
                # Datei mit CREATE erzeugen:
                { var reg1 sintW errorcode = # erzeugen (Attribute=0)
                    GEMDOS_create(TheAsciz(namestring),0);
                  if (errorcode<0) { OS_error(errorcode); } # Error melden
                  # Datei neu erzeugt, errorcode ist das Handle
                  handle = allocate_handle(errorcode);
                }
                #endif
                #if defined(DJUNIX) || defined(EMUNIX)
                # Bei if_exists=4 und if_not_exists=3 kann man sofort
                # CREAT ansteuern, sonst muß man vorher OPEN versuchen:
                if (!((if_exists==4) && (if_not_exists==3)))
                  { var reg1 sintW ergebnis = # Datei zu öffnen versuchen
                      open(TheAsciz(namestring),O_RDWR);
                    if (ergebnis<0)
                      { if (errno == ENOENT) # nicht gefunden?
                          # Datei existiert nicht
                          { # :IF-DOES-NOT-EXIST-Argument entscheidet:
                            if (if_not_exists<2) # (Default bei :APPEND oder :OVERWRITE) oder :ERROR ?
                              goto fehler_notfound;
                            if (if_not_exists==2) # NIL -> NIL
                              goto ergebnis_NIL;
                            # :CREATE
                          }
                          else
                          { OS_error(); } # sonstigen Error melden
                      }
                      else
                      # Datei existiert, ergebnis ist das Handle
                      { # :IF-EXISTS-Argument entscheidet:
                        switch (if_exists)
                          { case 1: # :ERROR -> schließen und Error
                              { if (close(ergebnis) < 0) { OS_error(); } # Error melden
                                goto fehler_exists;
                              }
                            case 2: # NIL -> schließen und NIL
                              { if (close(ergebnis) < 0) { OS_error(); } # Error melden
                                goto ergebnis_NIL;
                              }
                            case 5: # :APPEND
                              append_flag = TRUE; # am Schluß ans Ende positionieren
                            case 6: # :OVERWRITE -> bestehende Datei benutzen
                              setmode(ergebnis,O_BINARY);
                              handle = allocate_handle(ergebnis);
                              goto handle_ok;
                            default: ;
                              # :RENAME, :RENAME-AND-DELETE -> Datei umbenennen und dann neu eröffnen.
                              # :NEW-VERSION, :SUPERSEDE -> Datei auf Länge 0 kürzen.
                          }
                        # In beiden Fällen erst die Datei schließen:
                        if (close(ergebnis) < 0) { OS_error(); } # Error melden
                        if (if_exists==3)
                          # :RENAME oder :RENAME-AND-DELETE -> umbenennen:
                          { # Truename mit ".BAK" erweitern:
                            var reg1 object filename = STACK_0;
                            if (openp(filename)) { fehler_rename_open(filename); } # Keine offenen Dateien umbenennen!
                            pushSTACK(namestring); # namestring retten
                            # filename := (merge-pathnames ".BAK" filename) :
                            filename = copy_pathname(filename); # kopieren
                            ThePathname(filename)->pathname_type = O(backuptype_string); # mit Extension "BAK"
                            if (openp(filename)) { fehler_delete_open(filename); } # Keine offenen Dateien löschen!
                            pushSTACK(filename);
                           {# Directory existiert schon:
                            var reg3 object new_namestring = assume_dir_exists(); # Filename als ASCIZ-String
                            # Datei mit diesem Namen löschen, falls vorhanden:
                            if ( unlink(TheAsciz(new_namestring)) <0) # Datei zu löschen versuchen
                              { if (!(errno==ENOENT)) # nicht gefunden -> OK
                                  { OS_error(); } # sonstigen Error melden
                              }
                            # Datei vom alten auf diesen Namen umbenennen:
                            skipSTACK(1);
                            namestring = popSTACK(); # namestring zurück
                            if ( rename(TheAsciz(namestring),TheAsciz(new_namestring)) <0) # Datei umbenennen
                              { OS_error(); } # Error melden
                          }}
                      }
                  }
                # Datei mit CREAT erzeugen:
                { var reg1 sintW ergebnis = # erzeugen
                    creat(TheAsciz(namestring),my_open_mask);
                  if (ergebnis<0) { OS_error(); } # Error melden
                  setmode(ergebnis,O_BINARY);
                  # Datei neu erzeugt, ergebnis ist das Handle
                  handle = allocate_handle(ergebnis);
                }
                #endif
                #if defined(UNIX) || defined(AMIGAOS) || defined(VMS)
                if (file_exists(namestring))
                  # Datei existiert
                  { # :IF-EXISTS-Argument entscheidet:
                    switch (if_exists)
                      { case 1: # :ERROR -> Error
                          goto fehler_exists;
                        case 2: # NIL -> NIL
                          goto ergebnis_NIL;
                        case 3: # :RENAME oder :RENAME-AND-DELETE -> umbenennen:
                          #if defined(UNIX) || defined(AMIGAOS)
                          { # Truename mit "%" bzw. ".bak" erweitern:
                            var reg1 object filename = STACK_0;
                            if (openp(filename)) { fehler_rename_open(filename); } # Keine offenen Dateien umbenennen!
                            pushSTACK(namestring); # namestring retten
                            # filename := (parse-namestring (concatenate 'string (namestring filename) "%")) :
                            filename = whole_namestring(filename); # als String
                            pushSTACK(filename); pushSTACK(O(backupextend_string)); # "%"
                            filename = string_concat(2); # dazuhängen
                            pushSTACK(filename); # retten
                            filename = coerce_pathname(filename); # wieder als Filename
                            if (openp(filename)) { fehler_delete_open(filename); } # Keine offenen Dateien löschen!
                            # Directory existiert schon. Hier keine weiteren Links verfolgen.
                           {var reg3 object new_namestring = string_to_asciz(popSTACK()); # Filename als ASCIZ-String
                            # Datei (oder Link) mit diesem Namen löschen, falls vorhanden:
                            #if defined(AMIGAOS)
                            begin_system_call();
                            if (! DeleteFile(TheAsciz(new_namestring)) )
                              { if (!(IoErr()==ERROR_OBJECT_NOT_FOUND)) { OS_error(); } # Error melden
                                # nicht gefunden -> OK
                              }
                            end_system_call();
                            #endif
                            #if defined(UNIX) && 0 # Das tut UNIX nachher automatisch
                            begin_system_call();
                            if (!( unlink(TheAsciz(new_namestring)) ==0))
                              { if (!(errno==ENOENT)) { OS_error(); } # Error melden
                                # nicht gefunden -> OK
                              }
                            end_system_call();
                            #endif
                            # Datei vom alten auf diesen Namen umbenennen:
                            namestring = popSTACK(); # namestring zurück
                            begin_system_call();
                            #ifdef AMIGAOS
                            if (! Rename(TheAsciz(namestring),TheAsciz(new_namestring)) )
                              { OS_error(); }
                            #endif
                            #ifdef UNIX
                            if (!( rename(TheAsciz(namestring),TheAsciz(new_namestring)) ==0))
                              { OS_error(); }
                            #endif
                            end_system_call();
                          }}
                          #endif
                          break;
                        case 5: # :APPEND
                          append_flag = TRUE; # am Schluß ans Ende positionieren
                        default: ;
                          # :OVERWRITE -> bestehende Datei benutzen
                          # :NEW-VERSION, :SUPERSEDE -> Datei auf Länge 0 kürzen.
                  }   }
                  else
                  # Datei existiert nicht
                  { # :IF-DOES-NOT-EXIST-Argument entscheidet:
                    if (if_not_exists<2) # (Default bei :APPEND oder :OVERWRITE) oder :ERROR ?
                      goto fehler_notfound;
                    if (if_not_exists==2) # NIL -> NIL
                      goto ergebnis_NIL;
                    # :CREATE
                  }
                # Datei mit open öffnen:
                { # if-exists-Handling: bei if_exists<=4 Inhalt löschen,
                  # sonst (bei :APPEND, :OVERWRITE) bestehenden Inhalt lassen.
                  # if-not-exists-Handling: neue Datei erzeugen.
                  #ifdef AMIGAOS
                  var reg1 Handle handl;
                  begin_system_call();
                  handl = Open(TheAsciz(namestring),
                               (if_exists<=4 ? MODE_NEWFILE : MODE_READWRITE)
                              );
                  if (handl==Handle_NULL) { OS_error(); } # Error melden
                  end_system_call();
                  handle = allocate_handle(handl);
                  #endif
                  #if defined(UNIX) || defined(VMS)
                  var reg1 int ergebnis;
                  begin_system_call();
                  ergebnis = open(TheAsciz(namestring),
                                  (if_exists<=4 ? O_RDWR | O_CREAT | O_TRUNC
                                                : O_RDWR | O_CREAT
                                  ),
                                  my_open_mask
                                 );
                  if (ergebnis<0) { OS_error(); } # Error melden
                  end_system_call();
                  # Datei wurde geöffnet, ergebnis ist das Handle.
                  handle = allocate_handle(ergebnis);
                  #endif
                }
                #endif
                break;
              }
            ergebnis_NIL: # Ergebnis NIL
              skipSTACK(2); # beide Pathnames vergessen
              return NIL;
            fehler_notfound: # Fehler, da Datei nicht gefunden
              # STACK_0 = Truename
              fehler(
                     DEUTSCH ? "Eine Datei mit Namen ~ existiert nicht." :
                     ENGLISH ? "file ~ does not exist" :
                     FRANCAIS ? "Un fichier de nom ~ n'existe pas." :
                     ""
                    );
            fehler_exists: # Fehler, da Datei bereits existiert
              # STACK_0 = Truename
              fehler(
                     DEUTSCH ? "Eine Datei mit Namen ~ existiert bereits." :
                     ENGLISH ? "a file named ~ already exists" :
                     FRANCAIS ? "Un fichier de nom ~ existe déjà." :
                     ""
                    );
          }
        handle_ok:
        # handle und append_flag sind jetzt fertig.
        # Stream erzeugen:
        return make_file_stream(handle,direction,type,eltype_size,append_flag);
    } }

LISPFUN(open,1,0,norest,key,4,\
        (kw(direction),kw(element_type),kw(if_exists),kw(if_does_not_exist)) )
# (OPEN filename :direction :element-type :if-exists :if-does-not-exist),
# CLTL S. 418
  { var reg2 object filename = STACK_4; # filename
    if (streamp(filename))
      { # muß File-Stream sein:
        if_strm_file_p(filename, ; , fehler_thing(filename); );
        # Streamtyp File-Stream -> Truename verwenden:
        filename = TheStream(filename)->strm_file_truename;
      }
      else
      { filename = coerce_pathname(filename); } # zu einem Pathname machen
    # filename ist jetzt ein Pathname.
   {var reg3 uintB direction;
    var reg4 uintB if_exists;
    var reg5 uintB if_not_exists;
    var reg6 uintB type;
    var reg7 object eltype_size = NIL;
    # :direction überprüfen und in direction übersetzen:
    { var reg1 object arg = STACK_3;
      if (eq(arg,unbound) || eq(arg,S(Kinput))) { direction = 1; }
      elif (eq(arg,S(Koutput))) { direction = 2; }
      elif (eq(arg,S(Kio))) { direction = 3; }
      elif (eq(arg,S(Kprobe))) { direction = 0; }
      else
      { pushSTACK(arg); pushSTACK(S(open));
        fehler(
               DEUTSCH ? "~: Als :DIRECTION-Argument ist ~ unzulässig." :
               ENGLISH ? "~: illegal :DIRECTION argument ~" :
               FRANCAIS ? "~ : ~ n'est pas permis comme argument pour :DIRECTION." :
               ""
              );
    } }
    # :element-type überprüfen und in type und eltype_size übersetzen:
    { var reg1 object arg = STACK_2;
      if (eq(arg,unbound) || eq(arg,S(string_char)) || eq(arg,S(Kdefault))) # STRING-CHAR, :DEFAULT
        { type = strmtype_sch_file; }
      elif (eq(arg,S(character))) # CHARACTER
        { type = strmtype_ch_file; }
      elif (eq(arg,S(bit))) # BIT
        { type = strmtype_iu_file; eltype_size = Fixnum_1; }
      elif (eq(arg,S(unsigned_byte))) # UNSIGNED-BYTE
        { type = strmtype_iu_file; eltype_size = fixnum(8); }
      elif (eq(arg,S(signed_byte))) # SIGNED-BYTE
        { type = strmtype_is_file; eltype_size = fixnum(8); }
      elif (consp(arg) && mconsp(Cdr(arg)) && nullp(Cdr(Cdr(arg)))) # zweielementige Liste
        { var reg2 object h = Car(arg);
          if (eq(h,S(mod))) # (MOD n)
            { type = strmtype_iu_file;
              h = Car(Cdr(arg)); # n
              # muß ein Integer >0 sein:
              if (!(integerp(h) && positivep(h) && !eq(h,Fixnum_0)))
                goto bad_eltype;
              # eltype_size := (integer-length (1- n)) bilden:
              pushSTACK(filename); # filename retten
              pushSTACK(h); funcall(L(einsminus),1); # (1- n)
              pushSTACK(value1); funcall(L(integer_length),1); # (integer-length (1- n))
              eltype_size = value1;
              filename = popSTACK(); # filename zurück
            }
          elif (eq(h,S(unsigned_byte))) # (UNSIGNED-BYTE n)
            { type = strmtype_iu_file;
              eltype_size = Car(Cdr(arg));
            }
          elif (eq(h,S(signed_byte))) # (SIGNED-BYTE n)
            { type = strmtype_is_file;
              eltype_size = Car(Cdr(arg));
            }
          else goto bad_eltype;
          # eltype_size überprüfen:
          if (!(posfixnump(eltype_size) && !eq(eltype_size,Fixnum_0)
                && ((oint_addr_len < log2_intDsize+intCsize) # (Bei oint_addr_len <= log2(intDsize)+intCsize-1
                    # ist stets eltype_size < 2^oint_addr_len < intDsize*(2^intCsize-1).)
                    || ((oint)eltype_size < (oint)fixnum(intDsize*(uintL)(bitm(intCsize)-1)))
             ) )   )
            goto bad_eltype;
        }
      else
        { bad_eltype:
          pushSTACK(STACK_2); pushSTACK(S(open));
          fehler(
                 DEUTSCH ? "~: Als :ELEMENT-TYPE-Argument ist ~ unzulässig." :
                 ENGLISH ? "~: illegal :ELEMENT-TYPE argument ~" :
                 FRANCAIS ? "~ : ~ n'est pas permis comme argument pour :ELEMENT-TYPE." :
                 ""
                );
    }   }
    # :if-exists überprüfen und in if_exists übersetzen:
    { var reg1 object arg = STACK_1;
      if (eq(arg,unbound)) { if_exists = 0; }
      elif (eq(arg,S(Kerror))) { if_exists = 1; }
      elif (eq(arg,NIL)) { if_exists = 2; }
      elif (eq(arg,S(Krename)) || eq(arg,S(Krename_and_delete))) { if_exists = 3; }
      elif (eq(arg,S(Knew_version)) || eq(arg,S(Ksupersede))) { if_exists = 4; }
      elif (eq(arg,S(Kappend))) { if_exists = 5; }
      elif (eq(arg,S(Koverwrite))) { if_exists = 6; }
      else
      { pushSTACK(arg); pushSTACK(S(open));
        fehler(
               DEUTSCH ? "~: Als :IF-EXISTS-Argument ist ~ unzulässig." :
               ENGLISH ? "~: illegal :IF-EXISTS argument ~" :
               FRANCAIS ? "~ : ~ n'est pas permis comme argument pour :IF-EXISTS." :
               ""
              );
    } }
    # :if-does-not-exist überprüfen und in if_not_exists übersetzen:
    { var reg1 object arg = STACK_0;
      if (eq(arg,unbound)) { if_not_exists = 0; }
      elif (eq(arg,S(Kerror))) { if_not_exists = 1; }
      elif (eq(arg,NIL)) { if_not_exists = 2; }
      elif (eq(arg,S(Kcreate))) { if_not_exists = 3; }
      else
      { pushSTACK(arg); pushSTACK(S(open));
        fehler(
               DEUTSCH ? "~: Als :IF-DOES-NOT-EXIST-Argument ist ~ unzulässig." :
               ENGLISH ? "~: illegal :IF-DOES-NOT-EXIST argument ~" :
               FRANCAIS ? "~ : ~ n'est pas permis comme argument pour :IF-DOES-NOT-EXIST." :
               ""
              );
    } }
    # File öffnen:
    skipSTACK(5);
    value1 = open_file(filename,direction,if_exists,if_not_exists,type,eltype_size);
    mv_count=1;
  }}

# UP: Liefert eine Liste aller matchenden Pathnames.
# directory_search(pathname)
# > pathname: Pathname mit Device /= :WILD
# > STACK_0: Full-Flag
# < ergebnis:
#     Falls name=NIL und type=NIL:     Liste aller matchenden Directories,
#     sonst (name=NIL -> name=:WILD):  Liste aller matchenden Dateien.
#     Jeweils als absoluter Pathname ohne Wildcards,
#     bzw. bei Dateien und Full-Flag /=NIL als Liste
#          (Pathname Write-Date Length)
#          mit  Pathname ohne :WILD-Komponenten,
#               Write-Date = Datum der Dateierstellung (ss mm hh dd mm yy),
#                 als Decoded-Time passend für ENCODE-UNIVERSAL-TIME,
#               Length = Länge der Datei (in Bytes).
# kann GC auslösen
  local object directory_search (object pathname);
  # Methode: Breadth-first-search, damit nur eine Suchoperation gleichzeitig
  # läuft (und auf dem ATARI nur 1 DTA-Buffer gebraucht wird).
  #
  #ifdef PATHNAME_EXT83
  #
  # UP: Extrahiert Name und Typ aus dem DTA-Buffer.
  # Es wird angenommen, daß Name und Typ aus zulässigen Großbuchstaben
  # bestehen und eine Länge <= 8 bzw. 3 haben.
  # > asciz: Adresse des ASCIZ-Strings im DTA-Buffer
  # > def: Default-Typ
  # < -(STACK): Typ
  # < -(STACK): Name
  # Erniedrigt STACK um 2.
  # kann GC auslösen
    local void extract (const uintB* asciz, object def);
    local void extract(asciz,def)
      var reg3 const uintB* asciz;
      var reg4 object def;
      { pushSTACK(def); # Default-Typ in den Stack
       {# in Name.Typ aufspalten:
        var reg1 const uintB* ptr = asciz;
        var reg2 uintL count = 0;
        loop
          { var reg3 uintB ch = *ptr; # nächstes Zeichen
            if ((ch == 0) || (ch == '.')) # bei Nullbyte oder '.'
              break; # ist der Name zu Ende
            ptr++; count++; # weiterrücken
          }
        pushSTACK(make_string(asciz,count)); # String für Name erzeugen
        if (*ptr++ == 0) # mit Nullbyte beendet ?
          ; # ja -> Typ bleibt Default
          else
          { asciz = ptr; count = 0;
            until (*ptr++ == 0) { count++; } # bei Nullbyte ist der Typ zu Ende
            STACK_1 = make_string(asciz,count); # String für Typ erzeugen
          }
      }}
  #
  # UP: Sucht Subdirectories eines gegebenen Pathname.
  # subdirs(pathstring)
  # STACK_0 = Pathname, dessen Subdirectories zu suchen sind
  # STACK_1 = Liste, auf die die Pathnames der matchenden Subdirectories
  #           gepusht werden
  # > pathstring: Suchpfad als fertiger ASCIZ-String
  # verändert STACK_1, kann GC auslösen
    local void subdirs (object pathstring);
    local void subdirs(pathstring)
      var reg3 object pathstring;
      {
       #ifdef ATARI
        # Dateisuche gemäß GEMDOS-Konvention:
        var reg2 sintW errorcode;
        set_break_sem_4(); # wegen DTA-Buffer gegen Unterbrechungen sperren
        GEMDOS_SetDTA(&DTA_buffer); # DTA-Buffer setzen
        # Suchanfang, die Maske 0x10 sucht nach Ordnern und normalen Dateien:
        errorcode =
          GEMDOS_Sfirst(TheAsciz(pathstring),0x10);
        if (!(errorcode == GEMDOS_Sfirst_notfound)) # 'Keine Datei gefunden' ?
          # ja -> Schleife nicht durchlaufen
          loop
            { if (errorcode < 0) { OS_error(errorcode); } # sonstigen Error melden
              # Stackaufbau: new-pathname-list, pathname.
              # gefundene Datei untersuchen:
              if (DTA_buffer.d_attrib & 0x10) # sollte ein Unterdirectory sein
                if (!(DTA_buffer.d_fname[0] == '.')) # sollte nicht mit '.' anfangen
                  # (sonst ist es wohl '.' oder '..', wird übergangen)
                  { # in Name.Typ aufspalten, Default-Typ "" :
                    extract(&DTA_buffer.d_fname[0],O(leer_string));
                   {var reg1 object new_cons = allocate_cons();
                    Car(new_cons) = popSTACK(); Cdr(new_cons) = popSTACK();
                    # new_cons = (name . type)
                    pushSTACK(new_cons);
                    new_cons = allocate_cons();
                    Car(new_cons) = popSTACK();
                    # in ein-elementiger Liste new_cons = list1 = ((name . type))
                    pushSTACK(new_cons);
                   }# Stackaufbau: new-pathname-list, pathname, list1.
                    # letzten Pathname kopieren:
                   {var reg1 object temp = copy_pathname(STACK_1);
                    pushSTACK(temp);
                    # und darin Directory um list1 = ((name . type)) verlängern:
                    # (append pathname-dir list1) = (nreconc (reverse pathname-dir) list1)
                    temp = reverse(ThePathname(temp)->pathname_directory);
                    temp = nreconc(temp,STACK_1);
                    ThePathname(STACK_0)->pathname_directory = temp;
                   }# Stackaufbau: new-pathname-list, pathname, list1, newpathname.
                    # newpathname auf die Liste new-pathname-list pushen:
                   {var reg1 object new_cons = allocate_cons();
                    Car(new_cons) = popSTACK(); skipSTACK(1);
                    Cdr(new_cons) = STACK_1; STACK_1 = new_cons;
                  }}
              # nächstes File:
              errorcode = GEMDOS_Snext();
              if (errorcode == GEMDOS_Snext_notfound) # 'Keine weitere Datei gefunden' ?
                break; # ja -> Schleifenende
            }
        clr_break_sem_4();
       #endif
       #ifdef MSDOS
        # Dateisuche gemäß DOS-Konvention:
        var struct ffblk DTA_buffer;
        set_break_sem_4(); # wegen DTA-Buffer gegen Unterbrechungen sperren
        # Suchanfang, suche nach Ordnern und normalen Dateien:
        if (findfirst(TheAsciz(pathstring),&DTA_buffer,FA_DIREC|FA_ARCH|FA_RDONLY) <0)
          { if (!((errno==ENOENT) || (errno==ENOMORE))) { OS_error(); } }
          else # Keine Datei gefunden -> Schleife nicht durchlaufen
          loop
            { # Stackaufbau: new-pathname-list, pathname.
              # gefundene Datei untersuchen:
              if (DTA_buffer.ff_attrib & FA_DIREC) # sollte ein Unterdirectory sein
                if (!(DTA_buffer.ff_name[0] == '.')) # sollte nicht mit '.' anfangen
                  # (sonst ist es wohl '.' oder '..', wird übergangen)
                  { # in Name.Typ aufspalten, Default-Typ "" :
                    extract(&DTA_buffer.ff_name[0],O(leer_string));
                   {var reg1 object new_cons = allocate_cons();
                    Car(new_cons) = popSTACK(); Cdr(new_cons) = popSTACK();
                    # new_cons = (name . type)
                    pushSTACK(new_cons);
                    new_cons = allocate_cons();
                    Car(new_cons) = popSTACK();
                    # in ein-elementiger Liste new_cons = list1 = ((name . type))
                    pushSTACK(new_cons);
                   }# Stackaufbau: new-pathname-list, pathname, list1.
                    # letzten Pathname kopieren:
                   {var reg1 object temp = copy_pathname(STACK_1);
                    pushSTACK(temp);
                    # und darin Directory um list1 = ((name . type)) verlängern:
                    # (append pathname-dir list1) = (nreconc (reverse pathname-dir) list1)
                    temp = reverse(ThePathname(temp)->pathname_directory);
                    temp = nreconc(temp,STACK_1);
                    ThePathname(STACK_0)->pathname_directory = temp;
                   }# Stackaufbau: new-pathname-list, pathname, list1, newpathname.
                    # newpathname auf die Liste new-pathname-list pushen:
                   {var reg1 object new_cons = allocate_cons();
                    Car(new_cons) = popSTACK(); skipSTACK(1);
                    Cdr(new_cons) = STACK_1; STACK_1 = new_cons;
                  }}
              # nächstes File:
              if (findnext(&DTA_buffer) <0)
                { if (!((errno==ENOENT) || (errno==ENOMORE))) { OS_error(); }
                  break; # Keine weitere Datei -> Schleifenende
                }
            }
        clr_break_sem_4();
       #endif
      }
  #
  # UP: Sucht alle Subdirectories (beliebiger Tiefe) eines gegebenen Pathname.
  # allsubdirs(pathnamelist)
  # > pathnamelist: Liste, dessen CAR der gegebene Pathname ist.
  # Die Pathnames aller echten Subdirectories (beliebiger Tiefe) werden als
  # Liste destruktiv zwischen pathnamelist und (cdr pathnamelist) gehängt.
  # < ergebnis: das ursprüngliche (cdr pathnamelist)
  # kann GC auslösen
    local object allsubdirs (object pathnamelist);
    local object allsubdirs(pathnamelist)
      var reg1 object pathnamelist;
      { pushSTACK(pathnamelist);
        pushSTACK(NIL); # new-pathname-list := NIL
        {var reg2 object pathname = Car(pathnamelist);
         pushSTACK(pathname);
         # Stackaufbau: pathnamelist, new-pathname-list, pathname.
         {var reg3 uintC stringcount =
            directory_namestring_parts_(pathname); # Directory-Namestring-Teile,
          pushSTACK(O(wild_wild_string)); # "*.*"
          pushSTACK(O(null_string)); # und Nullbyte
          {var reg4 object pathstring = string_concat(stringcount+1+1); # zusammenhängen
           subdirs(pathstring); # alle subdirs auf new-pathname-list pushen
        }}}
        skipSTACK(1); # pathname vergessen
        { var reg2 object new_pathname_list = popSTACK();
          pathnamelist = popSTACK();
          # Stackaufbau: (leer).
          # Mit  (setf (cdr pathnamelist)
          #            (nreconc new-pathname-list (cdr pathnamelist))
          #      )
          # die new-pathname-list umdrehen und gleichzeitig einhängen:
          new_pathname_list = nreconc(new_pathname_list,Cdr(pathnamelist));
          pushSTACK(Cdr(pathnamelist)); Cdr(pathnamelist) = new_pathname_list;
          pathnamelist = new_pathname_list;
        }
        # Stackaufbau: ursprüngliches (cdr pathnamelist).
        # Liste pathnamelist durchlaufen, bis bei STACK_0 angelangt,
        # und rekursiv alle Subdirectories bestimmen und einhängen:
        until (eq(pathnamelist,STACK_0))
          { pathnamelist = allsubdirs(pathnamelist); }
        skipSTACK(1);
        return pathnamelist;
      }
  #
  local object directory_search(pathname)
    var reg4 object pathname;
    { pathname = use_default_dir(pathname); # Default-Directory einfügen
      # pathname ist jetzt ein Pathname, bei dem Device ein überprüfter
      # String ist und Directory [die Seriennummer, aber] kein
      # :RELATIVE, :CURRENT, :PARENT enthält.
      pushSTACK(pathname);
      #if HAS_SERNR
      pushSTACK(Cdr(ThePathname(pathname)->pathname_directory)); # subdir-list
      #else
      pushSTACK(ThePathname(pathname)->pathname_directory); # subdir-list
      #endif
      # pathname kopieren:
      pushSTACK(copy_pathname(pathname));
      # und dessen Directory auf ([Seriennummer] :ABSOLUTE) verkürzen:
      {var reg1 object new_cons = allocate_cons(); # neues Cons mit CDR=NIL
       Car(new_cons) = S(Kabsolute); # :ABSOLUTE als CAR
       #if HAS_SERNR
       pushSTACK(new_cons);
       new_cons = allocate_cons(); # neues Cons
       Cdr(new_cons) = popSTACK(); # mit anderem Cons als CDR
       Car(new_cons) = Car(ThePathname(STACK_0)->pathname_directory); # und Seriennummer als CAR
       #endif
       ThePathname(STACK_0)->pathname_directory = new_cons;
      }
      # und in einelementige Liste packen:
      {var reg1 object new_cons = allocate_cons();
       Car(new_cons) = STACK_0;
       STACK_0 = new_cons;
      }
      while
        # Stackaufbau: pathname, subdir-list, pathname-list.
        # Dabei enthalten die Pathnames aus pathname-list das Directory
        # nur so tief, daß es danach mit (cdr subdir-list) weitergeht.
        # Nächste subdir-Ebene abarbeiten:
        (consp (STACK_1 = Cdr(STACK_1))) # subdir-list verkürzen
        { # pathname-list durchgehen und dabei neue Liste aufbauen:
          pushSTACK(STACK_0); pushSTACK(NIL);
          loop
            { # Stackaufbau: ..., pathname-list-rest, new-pathname-list.
              var reg2 object pathname_list_rest = STACK_1;
              if (atomp(pathname_list_rest)) break;
              STACK_1 = Cdr(pathname_list_rest);
             {var reg5 object next_pathname = Car(pathname_list_rest); # nächster Pathname
              var reg3 object subdir_list = STACK_(1+2);
              pushSTACK(next_pathname); # in den Stack
              if (!eq(Car(subdir_list),S(Kwild))) # nächstes subdir = :WILD ?
                { # normales subdir:
                  var reg1 uintC stringcount =
                    directory_namestring_parts_(next_pathname); # Directory-Namestring-Teile (keine GC!)
                  stringcount +=
                    subdir_namestring_parts(subdir_list); # und Strings zum nächsten subdir
                  pushSTACK(O(null_string)); stringcount += 1; # und Nullbyte
                 {var reg6 object pathstring = string_concat(stringcount); # zusammenhängen
                  subdirs(pathstring); # alle subdirs auf new-pathname-list pushen
                  skipSTACK(1); # next-pathname vergessen
                }}
                else
                { # subdir = :WILD -> alle Subdirs bestimmen:
                  {var reg1 object list1 = allocate_cons();
                   Car(list1) = STACK_0;
                   STACK_0 = list1; # einelementige Liste (next-pathname)
                   allsubdirs(list1); # alle Subdirectories bestimmen
                  }
                  # Liste aller Subdirectories vor new-pathname-list
                  # in umgekehrter Reihenfolge davorhängen:
                  # (nreconc subdirlist new-pathname-list)
                  {var reg1 object newsubdirlist = popSTACK();
                   STACK_0 = nreconc(newsubdirlist,STACK_0);
                } }
              # nächsten Pathname aus pathname-list-rest nehmen
            }}
         {var reg1 object new_pathname_list = popSTACK(); skipSTACK(1);
          # umdrehen und als nächste pathname-list verwenden:
          STACK_0 = nreverse(new_pathname_list);
        }}
      # Stackaufbau: pathname, nix, pathname-list.
      pathname = STACK_2;
      {var reg2 object name = ThePathname(pathname)->pathname_name;
       var reg3 object type = ThePathname(pathname)->pathname_type;
       if (nullp(name)) # Name=NIL ?
         { if (nullp(type)) # auch Typ=NIL ?
             { var reg1 object new_pathname_list = popSTACK(); # ja ->
               skipSTACK(2); return new_pathname_list; # schon fertig
             }
             else
             # nein -> verwende :WILD (statt NIL) als Name
             { name = S(Kwild); }
         }
       # Alle Files name.type in den gegebenen Subdirectories suchen:
       { var reg1 uintC stringcount =
           nametype_namestring_parts(name,type,ThePathname(pathname)->pathname_version); # Teilstrings zu Name und Typ
         pushSTACK(O(null_string)); stringcount++; # und Nullbyte
        {var reg2 object name_type_asciz = string_concat(stringcount);
         STACK_2 = name_type_asciz;
      }}}
      STACK_1 = STACK_0; # pathname-list
      STACK_0 = NIL; # new-pathname-list := NIL
      # Stackaufbau: name-type-asciz, pathname-list, new-pathname-list.
      loop
        { var reg5 object pathname_list_rest = STACK_1;
          if (atomp(pathname_list_rest)) break;
          STACK_1 = Cdr(pathname_list_rest);
         {var reg8 object next_pathname = Car(pathname_list_rest); # nächster Pathname
          var reg7 object name_type_asciz = STACK_2;
          pushSTACK(next_pathname); # in den Stack
          {var reg3 uintC stringcount =
             directory_namestring_parts_(next_pathname); # Directory-Namestring-Teile (keine GC!)
           pushSTACK(name_type_asciz); stringcount += 1; # und name-type-asciz
           {var reg6 object pathstring = string_concat(stringcount); # zusammenhängen
            #ifdef ATARI
             # Dateisuche gemäß GEMDOS-Konvention:
             var reg2 sintW errorcode;
             set_break_sem_4(); # wegen DTA-Buffer gegen Unterbrechungen sperren
             GEMDOS_SetDTA(&DTA_buffer); # DTA-Buffer setzen
             # Suchanfang, die Maske 0x00 sucht nur nach normalen Dateien:
             errorcode =
               GEMDOS_Sfirst(TheAsciz(pathstring),0x00);
             if (!(errorcode == GEMDOS_Sfirst_notfound)) # 'Keine Datei gefunden' ?
               # ja -> Schleife nicht durchlaufen
               loop
                 { if (errorcode < 0) { OS_error(errorcode); } # sonstigen Error melden
                   # Stackaufbau: ..., next-pathname.
                   # gefundene Datei untersuchen:
                   { # in Name.Typ aufspalten, Default-Typ NIL :
                     extract(&DTA_buffer.d_fname[0],NIL);
                    {# letzten Pathname kopieren und Name und Typ eintragen:
                     var reg1 object new = copy_pathname(STACK_2);
                     ThePathname(new)->pathname_name = popSTACK();
                     ThePathname(new)->pathname_type = popSTACK();
                     # Full-Flag abtesten und evtl. mehr Information besorgen:
                     if (!nullp(STACK_(0+3+1)))
                       { pushSTACK(new); # newpathname als 1. Listenelement
                         pushSTACK(new); # newpathname als 2. Listenelement
                         { # Uhrzeit und Datum von Atari-Format in Decoded-Time umwandeln:
                           var decoded_time timepoint;
                           convert_timedate(DTA_buffer.d_time,DTA_buffer.d_date,
                                        &timepoint);
                           pushSTACK(timepoint.Sekunden);
                           pushSTACK(timepoint.Minuten);
                           pushSTACK(timepoint.Stunden);
                           pushSTACK(timepoint.Tag);
                           pushSTACK(timepoint.Monat);
                           pushSTACK(timepoint.Jahr);
                           new = listof(6); # 6-elementige Liste bauen
                         }
                         pushSTACK(new); # als 3. Listenelement
                         pushSTACK(UL_to_I(DTA_buffer.d_length)); # Länge als 4. Listenelement
                         new = listof(4); # 4-elementige Liste bauen
                       }
                     # new auf die Liste new-pathname-list pushen:
                      pushSTACK(new);
                     {var reg1 object new_cons = allocate_cons();
                      Car(new_cons) = popSTACK();
                      Cdr(new_cons) = STACK_(0+1);
                      STACK_(0+1) = new_cons;
                   }}}
                   # nächstes File:
                   errorcode = GEMDOS_Snext();
                   if (errorcode == GEMDOS_Snext_notfound) # 'Keine weitere Datei gefunden' ?
                     break; # ja -> Schleifenende
                 }
             clr_break_sem_4();
            #endif
            #ifdef MSDOS
             # Dateisuche gemäß DOS-Konvention:
             var struct ffblk DTA_buffer;
             set_break_sem_4(); # wegen DTA-Buffer gegen Unterbrechungen sperren
             # Suchanfang, suche nur nach normalen Dateien:
             if (findfirst(TheAsciz(pathstring),&DTA_buffer,FA_ARCH|FA_RDONLY) <0)
               { if (!((errno==ENOENT) || (errno==ENOMORE))) { OS_error(); } }
               else # Keine Datei gefunden -> Schleife nicht durchlaufen
               loop
                 { # Stackaufbau: ..., next-pathname.
                   # gefundene Datei untersuchen:
                   { # in Name.Typ aufspalten, Default-Typ NIL :
                     extract(&DTA_buffer.ff_name[0],NIL);
                    {# letzten Pathname kopieren und Name und Typ eintragen:
                     var reg1 object new = copy_pathname(STACK_2);
                     ThePathname(new)->pathname_name = popSTACK();
                     ThePathname(new)->pathname_type = popSTACK();
                     # Full-Flag abtesten und evtl. mehr Information besorgen:
                     if (!nullp(STACK_(0+3+1)))
                       { pushSTACK(new); # newpathname als 1. Listenelement
                         pushSTACK(new); # newpathname als 2. Listenelement
                         { # Uhrzeit und Datum von DOS-Format in Decoded-Time umwandeln:
                           var decoded_time timepoint;
                           convert_timedate((uintW)DTA_buffer.ff_ftime,(uintW)DTA_buffer.ff_fdate,
                                        &timepoint);
                           pushSTACK(timepoint.Sekunden);
                           pushSTACK(timepoint.Minuten);
                           pushSTACK(timepoint.Stunden);
                           pushSTACK(timepoint.Tag);
                           pushSTACK(timepoint.Monat);
                           pushSTACK(timepoint.Jahr);
                           new = listof(6); # 6-elementige Liste bauen
                         }
                         pushSTACK(new); # als 3. Listenelement
                         pushSTACK(UL_to_I(*(uintL*)(&DTA_buffer.ff_fsize))); # Länge als 4. Listenelement
                         new = listof(4); # 4-elementige Liste bauen
                       }
                     # new auf die Liste new-pathname-list pushen:
                      pushSTACK(new);
                     {var reg1 object new_cons = allocate_cons();
                      Car(new_cons) = popSTACK();
                      Cdr(new_cons) = STACK_(0+1);
                      STACK_(0+1) = new_cons;
                   }}}
                   # nächstes File:
                   if (findnext(&DTA_buffer) <0)
                     { if (!((errno==ENOENT) || (errno==ENOMORE))) { OS_error(); }
                       break; # Keine weitere Datei -> Schleifenende
                     }
                 }
             clr_break_sem_4();
            #endif
          }}
          skipSTACK(1); # next-pathname vergessen
        }}
      {# new-pathname-list wieder umdrehen:
       var reg1 object new_pathname_list = nreverse(popSTACK());
       skipSTACK(2); return new_pathname_list;
    } }
  #
  #endif # PATHNAME_EXT83
  #
  #if defined(PATHNAME_NOEXT) || defined(PATHNAME_VMS)
  #
  # UP: Matcht einen Wildcard-String ("Muster") mit einem "Beispiel".
  # > muster: Simple-String, mit Platzhaltern
  #           '?' für genau 1 Zeichen
  #           '*' für beliebig viele Zeichen
  # > beispiel: Simple-String, der damit zu matchen ist
  local boolean wildcard_match (object muster, object beispiel);
  # rekursive Implementation wegen Backtracking:
  local boolean wildcard_match_ab (uintL m_count, uintB* m_ptr, uintL b_count, uintB* b_ptr);
  local boolean wildcard_match(muster,beispiel)
    var reg2 object muster;
    var reg1 object beispiel;
    { return wildcard_match_ab(
                               /* m_count = */ TheSstring(muster)->length,
                               /* m_ptr   = */ &TheSstring(muster)->data[0],
                               /* b_count = */ TheSstring(beispiel)->length,
                               /* b_ptr   = */ &TheSstring(beispiel)->data[0]
                              );
    }
  local boolean wildcard_match_ab(m_count,m_ptr,b_count,b_ptr)
    var reg5 uintL m_count;
    var reg2 uintB* m_ptr;
    var reg4 uintL b_count;
    var reg1 uintB* b_ptr;
    { var reg3 uintB c;
      loop
        { if (m_count==0)
            { return (b_count==0 ? TRUE : FALSE); } # "" matcht nur ""
          m_count--;
          c = *m_ptr++; # nächstes Match-Zeichen
          if (c=='?') # Wildcard '?'
            { if (b_count==0) return FALSE; # mindestens ein Zeichen muß noch kommen
              b_count--; b_ptr++; # es wird ignoriert
            }
          elif (c=='*') break; # Wildcard '*' später
          else # alles andere muß genau matchen:
            { if (b_count==0) return FALSE;
              b_count--; if (!equal_pathchar(*b_ptr++,c)) return FALSE;
            }
        }
      # Wildcard '*': Suche nächstes non-Wildcard-Zeichen und zähle die '?'
      # mit (denn eine Folge '*??*???***?' matcht alles, was mindestens so
      # lang ist, wie die Folge Fragezeichen enthält). Man kann die '?' auch
      # gleich verwerten, denn '*??*???***?' ist zu '??????*' äquivalent.
      loop
        { if (m_count==0) return TRUE; # Wildcard am Ende matcht den Rest.
          m_count--;
          c = *m_ptr++; # nächstes Match-Zeichen
          if (c=='?') # Fragezeichen: nach vorne ziehen, sofort abarbeiten
            { if (b_count==0) return FALSE;
              b_count--; b_ptr++;
            }
          elif (!(c=='*')) break;
        }
      # c = nächstes non-Wildcard-Zeichen. Suche es.
      loop
        { if (b_count==0) return FALSE; # c nicht gefunden
          b_count--;
          if (equal_pathchar(*b_ptr++,c))
            { if (wildcard_match_ab(m_count,m_ptr,b_count,b_ptr))
                return TRUE;
        }   }
    }
  #
  # UP: Erweitert das Directory eines Pathname um eine Komponente.
  # > STACK_1: ein Pathname
  # > STACK_0: neue Subdir-Komponente, ein Simple-String
  # < ergebnis: neuer Pathname mit um subdir verlängertem Directory
  # Erhöht STACK um 2
  # kann GC auslösen
  local object pathname_add_subdir (void);
  local object pathname_add_subdir()
    { # Pathname kopieren und dessen Directory gemäß
      # (append x (list y)) = (nreverse (cons y (reverse x))) verlängern:
      var reg2 object pathname = copy_pathname(STACK_1);
      STACK_1 = pathname;
      pushSTACK(reverse(ThePathname(pathname)->pathname_directory));
     {var reg1 object new_cons = allocate_cons();
      Cdr(new_cons) = popSTACK();
      Car(new_cons) = popSTACK();
      new_cons = nreverse(new_cons);
      pathname = popSTACK();
      ThePathname(pathname)->pathname_directory = new_cons;
      return pathname;
    }}
  #
  #if defined(UNIX) || defined(VMS) || defined(AMIGAOS)
  # UP: Erweitert einen Pathname um die File-Information.
  # > STACK_1: absoluter Pathname
  # > STACK_0: absoluter Pathname, Links aufgelöst
  # > *filestatus: dessen stat-Info
  # < STACK_0: Liste (Pathname Truename Write-Date Length [Kommentar]) im :FULL-Format
  local void with_stat_info (void);
  local void with_stat_info()
    { var reg2 object new;
      #if defined(UNIX) || defined(VMS)
      var reg3 uintL size = filestatus->st_size;
      #endif
      #ifdef AMIGAOS
      var reg3 uintL size = filestatus->fib_Size;
      #endif
      # Pathname schon in STACK_1, als 1. Listenelement
      # Truename schon in STACK_0, als 2. Listenelement
      { var decoded_time timepoint; # Write-Date in decodierter Form
        #if defined(UNIX) || defined(VMS)
        convert_time(&filestatus->st_mtime,&timepoint);
        #endif
        #ifdef AMIGAOS
        convert_time(&filestatus->fib_Date,&timepoint);
        #endif
        pushSTACK(timepoint.Sekunden);
        pushSTACK(timepoint.Minuten);
        pushSTACK(timepoint.Stunden);
        pushSTACK(timepoint.Tag);
        pushSTACK(timepoint.Monat);
        pushSTACK(timepoint.Jahr);
        new = listof(6); # 6-elementige Liste bauen
      }
      pushSTACK(new); # als 3. Listenelement
      pushSTACK(UL_to_I(size)); # Länge als 4. Listenelement
      #if defined(UNIX) || defined(VMS)
      new = listof(4); # 4-elementige Liste bauen
      #endif
      #ifdef AMIGAOS
      pushSTACK(asciz_to_string(&filestatus->fib_Comment[0])); # Kommentar als 5. Listenelement
      new = listof(5); # 5-elementige Liste bauen
      #endif
      pushSTACK(Car(new)); # pathname wieder in den Stack
      pushSTACK(new); # Liste in den Stack
    }
  #endif
  #
  local object directory_search(pathname)
    var reg9 object pathname;
    { pathname = use_default_dir(pathname); # Default-Directory einfügen
      # pathname ist jetzt neu und ein absoluter Pathname.
      pushSTACK(NIL); # result-list := NIL
      pushSTACK(pathname);
      # Falls name=NIL und type/=NIL: Setze name := "*".
      if (nullp(ThePathname(pathname)->pathname_name)
          && !nullp(ThePathname(pathname)->pathname_type)
         )
        { ThePathname(pathname)->pathname_name = O(wild_string); }
      # Zum Matchen: Name und Typ zu einem String zusammenfassen:
      if (nullp(ThePathname(pathname)->pathname_name))
        { pushSTACK(NIL); } # name=NIL -> auch type=NIL -> keine Files suchen
        else
        {var reg1 uintC stringcount = file_namestring_parts(pathname);
         var reg1 object nametype_string = string_concat(stringcount);
         pathname = STACK_0;
         pushSTACK(nametype_string);
        }
      pushSTACK(ThePathname(pathname)->pathname_directory); # subdir-list
      # pathname kopieren und dabei Name und Typ streichen und
      # Directory zu (:ABSOLUTE) verkürzen:
      pathname = copy_pathname(pathname);
      ThePathname(pathname)->pathname_name = NIL;
      ThePathname(pathname)->pathname_type = NIL;
      ThePathname(pathname)->pathname_directory = O(directory_absolute);
      pushSTACK(pathname);
      # und in einelementige Liste packen:
      {var reg1 object new_cons = allocate_cons();
       Car(new_cons) = STACK_0;
       STACK_0 = new_cons;
      }
     {var reg7 boolean recursively = # Flag, ob die nächste Operation auf
        FALSE;                       # alle Subdirectories anzuwenden ist.
      loop
        # Stackaufbau: result-list, pathname, name&type, subdir-list, pathname-list.
        # result-list = Liste der fertigen Pathnames/Listen, umgedreht.
        # name&type = NIL oder Simple-String, gegen den die Filenamen zu matchen sind.
        # pathname-list = Liste der noch abzuarbeitenden Directories.
        # Dabei enthalten die Pathnames aus pathname-list das Directory
        # nur so tief, daß es danach mit (cdr subdir-list) weitergeht.
        { # Nächste subdir-Ebene abarbeiten:
          STACK_1 = Cdr(STACK_1); # subdir-list verkürzen
         {var reg6 signean next_task; # Was mit den Dirs aus pathname-list zu tun ist:
            # 0: nichts, fertig
            # 1: nach einem File gegebenen Namens/Typs sehen
            # -1: nach einem Subdirectory gegebenen Namens sehen
            # 2: nach allen Files suchen, die gegebenen Namen/Typ matchen
            # -2: nach allen Subdirectories suchen, die gegebenen Namen matchen
          if (matomp(STACK_1)) # subdir-list zu Ende?
            { var reg1 object nametype = STACK_2;
              if (nullp(nametype)) # name=NIL und type=NIL -> keine Files suchen
                { next_task = 0; }
              #ifndef MSDOS
              elif (!has_wildcards(nametype))
                   # === !(has_wildcards(name) || ((!nullp(type)) && has_wildcards(type)))
                { next_task = 1; } # File suchen
              #endif
              else
                { next_task = 2; } # Files mit Wildcards suchen
            }
            else
            { var reg1 object next_subdir = Car(STACK_1);
              if (eq(next_subdir,S(Kwild))) # '...' ?
                # wird erst beim nächsten Durchlauf behandelt
                { recursively = TRUE; goto passed_subdir; }
              #ifndef MSDOS
              if (
                  #if defined(PATHNAME_AMIGAOS) || defined(PATHNAME_VMS)
                  eq(next_subdir,S(Kparent)) ||
                  #endif
                  !has_wildcards(next_subdir)
                 )
                { next_task = -1; } # Subdir suchen
                else
              #endif
                { next_task = -2; } # Subdirs mit Wildcards suchen
            }
          # pathname-list durchgehen und dabei neue Liste aufbauen:
          pushSTACK(NIL); pushSTACK(STACK_(0+1));
          loop
            { # Stackaufbau: ..., new-pathname-list, pathname-list-rest.
              var reg9 object pathname_list_rest = STACK_0;
              if (atomp(pathname_list_rest)) break;
              STACK_0 = Cdr(pathname_list_rest); # Liste verkürzen
              pushSTACK(NIL); # pathnames-to-insert := NIL
              # Stackaufbau: ..., new-pathname-list, pathname-list-rest, pathnames-to-insert.
             {var reg9 object pathname = Car(pathname_list_rest); # nächstes Directory
              pushSTACK(pathname); # in den Stack
              # Versuche, die Task ein wenig abzukürzen:
              if (!recursively)
                { switch (next_task)
                    { case 0: # Dieses pathname liefern
                        #ifdef UNIX
                        assure_dir_exists(); # erst noch Links auflösen
                        #endif
                        # und STACK_0 vor result-list pushen:
                        {var reg1 object new_cons = allocate_cons();
                         Car(new_cons) = popSTACK();
                         Cdr(new_cons) = STACK_(4+3);
                         STACK_(4+3) = new_cons;
                        }
                        goto next_pathname;
                      #ifndef MSDOS
                      case 1: # In diesem pathname nach einem File sehen
                        ThePathname(pathname)->pathname_name = # Name (/=NIL) einsetzen
                          ThePathname(STACK_(3+3+1))->pathname_name;
                        ThePathname(pathname)->pathname_type = # Typ einsetzen
                          ThePathname(STACK_(3+3+1))->pathname_type;
                        pushSTACK(pathname);
                        assure_dir_exists(); # Links auflösen, File suchen
                        if (file_exists(_EMA_)) # falls File existiert
                          { if (!nullp(STACK_(0+5+3+2))) # :FULL gewünscht?
                              { with_stat_info(); } # ja -> STACK_0 erweitern
                            # und STACK_0 vor result-list pushen:
                           {var reg1 object new_cons = allocate_cons();
                            Car(new_cons) = STACK_0;
                            Cdr(new_cons) = STACK_(4+3+2);
                            STACK_(4+3+2) = new_cons;
                          }}
                        skipSTACK(2);
                        goto next_pathname;
                      case -1: # In diesem pathname nach einem Subdirectory sehen
                        { var reg2 object namestring = assure_dir_exists(); # Links auflösen, Directory-Namestring
                          pushSTACK(namestring); # Directory-Namestring
                          {var reg1 object subdir = Car(STACK_(1+3+1+1)); # (car subdir-list)
                           #if defined(PATHNAME_AMIGAOS) || defined(PATHNAME_VMS)
                           if (eq(subdir,S(Kparent))) # für Parent-Directory
                             #ifdef PATHNAME_AMIGAOS
                             { pushSTACK(O(slash_string)); } # zusätzliches "/" ans Ende
                             #endif
                             #ifdef PATHNAME_VMS
                             { pushSTACK(O(parentdir_string)); } # zusätzliches "[.-]" ans Ende
                             #endif
                             else
                           #endif
                           pushSTACK(subdir);
                          }
                          pushSTACK(O(null_string)); # und Nullbyte
                          namestring = string_concat(3); # zusammenhängen
                          # Information holen:
                         #if defined(UNIX) || defined(VMS)
                         {var struct stat status;
                          begin_system_call();
                          if (!( stat(TheAsciz(namestring),&status) ==0))
                            { if (!(errno==ENOENT)) { OS_error(); }
                              end_system_call();
                              # Subdirectory existiert nicht -> OK.
                            }
                            else
                            # File existiert.
                            { end_system_call();
                              if (S_ISDIR(status.st_mode)) # Ist es ein Directory?
                                # ja -> neuen Pathname dazu bilden:
                                { # pathname kopieren und dessen Directory um
                                  # (car subdir-list) verlängern:
                                  pushSTACK(Car(STACK_(1+3+1)));
                                 {var reg1 object pathname = pathname_add_subdir();
                                  pushSTACK(pathname);
                                 }# Diesen neuen Pathname vor new-pathname-list pushen:
                                 {var reg1 object new_cons = allocate_cons();
                                  Car(new_cons) = STACK_0;
                                  Cdr(new_cons) = STACK_(2+1);
                                  STACK_(2+1) = new_cons;
                                }}
                         }  }
                         #endif
                         #ifdef AMIGAOS
                         { var LONGALIGNTYPE(struct FileInfoBlock) fib;
                           var reg1 struct FileInfoBlock * fibptr = LONGALIGN(&fib);
                           set_break_sem_4();
                           begin_system_call();
                          {var reg2 BPTR lock = Lock(TheAsciz(namestring),ACCESS_READ);
                           if (lock==BPTR_NULL)
                             { if (!(IoErr()==ERROR_OBJECT_NOT_FOUND)) { OS_error(); }
                               end_system_call();
                               clr_break_sem_4();
                               # Subdirectory existiert nicht -> OK.
                             }
                             else
                             # File existiert.
                             { if (! Examine(lock,fibptr) ) { UnLock(lock); OS_error(); }
                               UnLock(lock);
                               end_system_call();
                               clr_break_sem_4();
                               if (fibptr->fib_DirEntryType > 0) # Ist es ein Directory?
                                 # ja -> neuen Pathname dazu bilden:
                                 { # pathname kopieren und dessen Directory um
                                   # (car subdir-list) verlängern:
                                   pushSTACK(Car(STACK_(1+3+1)));
                                  {var reg1 object pathname = pathname_add_subdir();
                                   pushSTACK(pathname);
                                  }# Diesen neuen Pathname vor new-pathname-list pushen:
                                  {var reg1 object new_cons = allocate_cons();
                                   Car(new_cons) = STACK_0;
                                   Cdr(new_cons) = STACK_(2+1);
                                   STACK_(2+1) = new_cons;
                                 }}
                         }}  }
                         #endif
                        }
                        skipSTACK(1);
                        goto next_pathname;
                        #endif
                }   }
              # Um die Task zu erledigen, müssen alle Einträge dieses
              # Directory abgesucht werden:
              {{var reg1 object dir_namestring = assure_dir_exists(); # Links auflösen, Directory-Name bilden
                pushSTACK(dir_namestring); # retten
               }# Stackaufbau: ..., pathname, dir_namestring.
                if (next_task==0)
                  # Pathname STACK_1 vor result-list pushen:
                  {var reg1 object new_cons = allocate_cons();
                   Car(new_cons) = STACK_1;
                   Cdr(new_cons) = STACK_(4+3+2);
                   STACK_(4+3+2) = new_cons;
                  }
               #if defined(UNIX) || defined(VMS)
                pushSTACK(STACK_0); # Directory-Name
                #ifdef UNIX
                pushSTACK(O(punkt_string)); # und "."
                #endif
                #ifdef VMS
                pushSTACK(O(wild_wild_string)); # und "*.*;*"
                #endif
                pushSTACK(O(null_string)); # und Nullbyte
               {var reg8 object namestring = string_concat(3); # zusammenhängen
                # Directory absuchen:
                var reg5 DIR* dirp;
                set_break_sem_4();
                begin_system_call();
                dirp = opendir(TheAsciz(namestring)); # Directory öffnen
                if (dirp == (DIR*)NULL) { OS_error(); }
                end_system_call();
                loop
                  { var reg2 SDIRENT* dp;
                    errno = 0;
                    begin_system_call();
                    dp = readdir(dirp); # nächsten Directory-Eintrag holen
                    end_system_call();
                    if (dp == (SDIRENT*)NULL) # Error oder Directory zu Ende
                      { if (!(errno==0)) { OS_error(); } else break; }
                    # Directory-Eintrag in String umwandeln:
                   {var reg4 object direntry;
                    {var reg3 uintL direntry_len;
                     #ifdef DIRENT_WITHOUT_NAMLEN
                     # Unter UNIX_LINUX reicht direntry_len := dp->d_reclen, aber i.a. ist
                     # direntry_len := min(dp->d_reclen,strlen(dp->d_name))  nötig:
                     {var reg1 const uintB* ptr = (const uintB*)(&dp->d_name[0]);
                      var reg1 uintL count;
                      direntry_len = 0;
                      dotimesL(count,dp->d_reclen,
                        { if (*ptr == '\0') break;
                          ptr++; direntry_len++;
                        });
                     }
                     #else
                     direntry_len = dp->d_namlen;
                     #endif
                     direntry = make_string((const uintB*)(&dp->d_name[0]),direntry_len);
                    }
                    #ifdef UNIX
                    # "." und ".." übergehen:
                    if (!(equal(direntry,O(punkt_string))
                          || equal(direntry,O(punktpunkt_string))
                       ) )
                    #endif
                      { pushSTACK(direntry);
                        # Stackaufbau: ..., pathname, dir_namestring, direntry.
                        # Feststellen, ob es ein Directory oder ein File ist:
                        pushSTACK(STACK_1); # Directory-Namestring
                        pushSTACK(direntry); # direntry
                        pushSTACK(O(null_string)); # und Nullbyte
                       {var reg3 object namestring = string_concat(3); # zusammenhängen
                        # Information holen:
                        var struct stat status;
                        begin_system_call();
                        if (!( stat(TheAsciz(namestring),&status) ==0))
                          { if (!(errno==ENOENT)) { OS_error(); }
                            end_system_call();
                            # Eintrag existiert doch nicht (das kann uns
                            # wohl nur bei symbolischen Links passieren)
                            # -> wird übergangen
                          }
                          else
                          { end_system_call();
                            # Eintrag existiert (welch Wunder...)
                            if (S_ISDIR(status.st_mode)) # Ist es ein Directory?
                              # Eintrag ist ein Directory.
                              { if (recursively) # alle rekursiven Subdirectories gewünscht?
                                  # ja -> zu einem Pathname machen und auf
                                  # pathnames-to-insert pushen (wird nachher
                                  # vor pathname-list-rest eingefügt):
                                  { pushSTACK(STACK_2); pushSTACK(STACK_(0+1)); # pathname und direntry
                                   {var reg1 object pathname = pathname_add_subdir();
                                    pushSTACK(pathname);
                                   }# Diesen neuen Pathname vor pathname-to-insert pushen:
                                   {var reg1 object new_cons = allocate_cons();
                                    Car(new_cons) = popSTACK();
                                    Cdr(new_cons) = STACK_(0+3);
                                    STACK_(0+3) = new_cons;
                                  }}
                                if (next_task<0)
                                  { # (car subdir-list) mit direntry matchen:
                                    if (wildcard_match(Car(STACK_(1+3+3)),STACK_0))
                                      # Subdirectory matcht -> zu einem Pathname
                                      # machen und auf new-pathname-list pushen:
                                      { pushSTACK(STACK_2); pushSTACK(STACK_(0+1)); # pathname und direntry
                                       {var reg1 object pathname = pathname_add_subdir();
                                        pushSTACK(pathname);
                                       }# Diesen neuen Pathname vor new-pathname-list pushen:
                                       {var reg1 object new_cons = allocate_cons();
                                        Car(new_cons) = popSTACK();
                                        Cdr(new_cons) = STACK_(2+3);
                                        STACK_(2+3) = new_cons;
                                  }   }}
                              }
                              else
                              # Eintrag ist ein (halbwegs) normales File.
                              { if (next_task>0)
                                  { # name&type mit direntry matchen:
                                    if (wildcard_match(STACK_(2+3+3),STACK_0))
                                      # File matcht -> zu einem Pathname machen
                                      # und auf result-list pushen:
                                      { pushSTACK(STACK_0); # direntry
                                        split_name_type(1); # in Name und Typ aufspalten
                                       {var reg1 object pathname = copy_pathname(STACK_(2+2));
                                        ThePathname(pathname)->pathname_type = popSTACK(); # Typ einsetzen
                                        ThePathname(pathname)->pathname_name = popSTACK(); # Name einsetzen
                                        pushSTACK(pathname);
                                        pushSTACK(pathname);
                                       }# Truename bilden (symbolische Links auflösen):
                                        assure_dir_exists();
                                        if (file_exists(_EMA_)) # falls File (immer noch...) existiert
                                          { if (!nullp(STACK_(0+5+3+3+2))) # :FULL gewünscht?
                                              with_stat_info(); # ja -> STACK_0 erweitern
                                            # und STACK_0 vor result-list pushen:
                                           {var reg1 object new_cons = allocate_cons();
                                            Car(new_cons) = STACK_0;
                                            Cdr(new_cons) = STACK_(4+3+3+2);
                                            STACK_(4+3+3+2) = new_cons;
                                          }}
                                        skipSTACK(2);
                                  }   }
                          }   }
                        skipSTACK(1); # direntry vergessen
                  }}  }}
                begin_system_call();
                if (CLOSEDIR(dirp)) { OS_error(); }
                end_system_call();
                clr_break_sem_4();
               }
               #endif
               #ifdef AMIGAOS
                # Directory absuchen:
               { var reg7 object namestring = OSnamestring(STACK_0);
                 set_break_sem_4();
                 begin_system_call();
                {var reg6 BPTR lock = Lock(TheAsciz(namestring),ACCESS_READ);
                 var LONGALIGNTYPE(struct FileInfoBlock) fib;
                 var reg5 struct FileInfoBlock * fibptr = LONGALIGN(&fib);
                 if (lock==BPTR_NULL) { OS_error(); }
                 if (! Examine(lock,fibptr) ) { OS_error(); }
                 end_system_call();
                 loop
                   { begin_system_call();
                     if (! ExNext(lock,fibptr) ) # Error oder Directory zu Ende?
                       break;
                     end_system_call();
                     # Directory-Eintrag in String umwandeln:
                    {var reg4 object direntry = asciz_to_string(&fibptr->fib_FileName[0]);
                     pushSTACK(direntry);
                     # Stackaufbau: ..., pathname, dir_namestring, direntry.
                     # Feststellen, ob es ein Directory oder ein File ist:
                     if (fibptr->fib_DirEntryType > 0) # Ist es ein Directory?
                       # Eintrag ist ein Directory.
                       { if (recursively) # alle rekursiven Subdirectories gewünscht?
                           # ja -> zu einem Pathname machen und auf
                           # pathnames-to-insert pushen (wird nachher
                           # vor pathname-list-rest eingefügt):
                           { pushSTACK(STACK_2); pushSTACK(STACK_(0+1)); # pathname und direntry
                            {var reg1 object pathname = pathname_add_subdir();
                             pushSTACK(pathname);
                            }# Diesen neuen Pathname vor pathname-to-insert pushen:
                            {var reg1 object new_cons = allocate_cons();
                             Car(new_cons) = popSTACK();
                             Cdr(new_cons) = STACK_(0+3);
                             STACK_(0+3) = new_cons;
                           }}
                         if (next_task<0)
                           { # (car subdir-list) mit direntry matchen:
                             if (wildcard_match(Car(STACK_(1+3+3)),STACK_0))
                               # Subdirectory matcht -> zu einem Pathname
                               # machen und auf new-pathname-list pushen:
                               { pushSTACK(STACK_2); pushSTACK(STACK_(0+1)); # pathname und direntry
                                {var reg1 object pathname = pathname_add_subdir();
                                 pushSTACK(pathname);
                                }# Diesen neuen Pathname vor new-pathname-list pushen:
                                {var reg1 object new_cons = allocate_cons();
                                 Car(new_cons) = popSTACK();
                                 Cdr(new_cons) = STACK_(2+3);
                                 STACK_(2+3) = new_cons;
                           }   }}
                       }
                       else
                       # Eintrag ist ein (halbwegs) normales File.
                       { if (next_task>0)
                           { # name&type mit direntry matchen:
                             if (wildcard_match(STACK_(2+3+3),STACK_0))
                               # File matcht -> zu einem Pathname machen
                               # und auf result-list pushen:
                               { pushSTACK(STACK_0); # direntry
                                 split_name_type(1); # in Name und Typ aufspalten
                                {var reg1 object pathname = copy_pathname(STACK_(2+2));
                                 ThePathname(pathname)->pathname_type = popSTACK(); # Typ einsetzen
                                 ThePathname(pathname)->pathname_name = popSTACK(); # Name einsetzen
                                 pushSTACK(pathname);
                                 pushSTACK(pathname);
                                }
                                assure_dir_exists(); # Truename bilden (symbolische Links auflösen)
                                { if (!nullp(STACK_(0+5+3+3+2))) # :FULL gewünscht?
                                    with_stat_info(); # ja -> STACK_0 erweitern
                                  # und STACK_0 vor result-list pushen:
                                 {var reg1 object new_cons = allocate_cons();
                                  Car(new_cons) = STACK_0;
                                  Cdr(new_cons) = STACK_(4+3+3+2);
                                  STACK_(4+3+3+2) = new_cons;
                                }}
                                skipSTACK(2);
                       }   }   }
                     skipSTACK(1); # direntry vergessen
                   }}
                 UnLock(lock);
                 if (!(IoErr()==ERROR_NO_MORE_ENTRIES)) { OS_error(); }
                 end_system_call();
                 clr_break_sem_4();
               }}
               #endif
               #ifdef MSDOS
                pushSTACK(STACK_0); # Directory-Name
                pushSTACK(O(wild_wild_string)); # und "*.*"
                pushSTACK(O(null_string)); # und Nullbyte
               {var reg8 object namestring = string_concat(3); # zusammenhängen
                # Directory absuchen, gemäß DOS-Konvention:
                var struct ffblk DTA_buffer;
                set_break_sem_4(); # wegen DTA-Buffer gegen Unterbrechungen sperren
                # Suchanfang, suche nach Ordnern und normalen Dateien:
                if (findfirst(TheAsciz(namestring),&DTA_buffer,FA_DIREC|FA_ARCH|FA_RDONLY) <0)
                  { if (!((errno==ENOENT) || (errno==ENOMORE))) { OS_error(); } }
                  else # Keine Datei gefunden -> Schleife nicht durchlaufen
                  loop
                    { # Directory-Eintrag in String umwandeln:
                      var reg4 object direntry = asciz_to_string(&DTA_buffer.ff_name[0]);
                      # "." und ".." übergehen:
                      if (!(equal(direntry,O(punkt_string))
                            || equal(direntry,O(punktpunkt_string))
                         ) )
                        { pushSTACK(direntry);
                          # Stackaufbau: ..., pathname, dir_namestring, direntry.
                          if (DTA_buffer.ff_attrib & FA_DIREC) # Ist es ein Directory?
                            # Eintrag ist ein Directory.
                            { if (recursively) # alle rekursiven Subdirectories gewünscht?
                                # ja -> zu einem Pathname machen und auf
                                # pathnames-to-insert pushen (wird nachher
                                # vor pathname-list-rest eingefügt):
                                { pushSTACK(STACK_2); pushSTACK(STACK_(0+1)); # pathname und direntry
                                 {var reg1 object pathname = pathname_add_subdir();
                                  pushSTACK(pathname);
                                 }# Diesen neuen Pathname vor pathname-to-insert pushen:
                                 {var reg1 object new_cons = allocate_cons();
                                  Car(new_cons) = popSTACK();
                                  Cdr(new_cons) = STACK_(0+3);
                                  STACK_(0+3) = new_cons;
                                }}
                              if (next_task<0)
                                { # (car subdir-list) mit direntry matchen:
                                  if (wildcard_match(Car(STACK_(1+3+3)),STACK_0))
                                    # Subdirectory matcht -> zu einem Pathname
                                    # machen und auf new-pathname-list pushen:
                                    { pushSTACK(STACK_2); pushSTACK(STACK_(0+1)); # pathname und direntry
                                     {var reg1 object pathname = pathname_add_subdir();
                                      pushSTACK(pathname);
                                     }# Diesen neuen Pathname vor new-pathname-list pushen:
                                     {var reg1 object new_cons = allocate_cons();
                                      Car(new_cons) = popSTACK();
                                      Cdr(new_cons) = STACK_(2+3);
                                      STACK_(2+3) = new_cons;
                                }   }}
                            }
                            else
                            # Eintrag ist ein (halbwegs) normales File.
                            { if (next_task>0)
                                { # name&type mit direntry matchen:
                                  if (wildcard_match(STACK_(2+3+3),STACK_0))
                                    # File matcht -> zu einem Pathname machen
                                    # und auf result-list pushen:
                                    { pushSTACK(STACK_0); # direntry
                                      split_name_type(1); # in Name und Typ aufspalten
                                     {var reg1 object new = copy_pathname(STACK_(2+2));
                                      ThePathname(new)->pathname_type = popSTACK(); # Typ einsetzen
                                      ThePathname(new)->pathname_name = popSTACK(); # Name einsetzen
                                      # Full-Flag abtesten und evtl. mehr Information besorgen:
                                      if (!nullp(STACK_(0+5+3+3))) # :FULL gewünscht?
                                        { pushSTACK(new); # newpathname als 1. Listenelement
                                          pushSTACK(new); # newpathname als 2. Listenelement
                                          { # Uhrzeit und Datum von DOS-Format in Decoded-Time umwandeln:
                                            var decoded_time timepoint;
                                            convert_timedate((uintW)DTA_buffer.ff_ftime,(uintW)DTA_buffer.ff_fdate,
                                                             &timepoint);
                                            pushSTACK(timepoint.Sekunden);
                                            pushSTACK(timepoint.Minuten);
                                            pushSTACK(timepoint.Stunden);
                                            pushSTACK(timepoint.Tag);
                                            pushSTACK(timepoint.Monat);
                                            pushSTACK(timepoint.Jahr);
                                            new = listof(6); # 6-elementige Liste bauen
                                          }
                                          pushSTACK(new); # als 3. Listenelement
                                          pushSTACK(UL_to_I(*(uintL*)(&DTA_buffer.ff_fsize))); # Länge als 4. Listenelement
                                          new = listof(4); # 4-elementige Liste bauen
                                        }
                                      pushSTACK(new);
                                     }# und STACK_0 vor result-list pushen:
                                     {var reg1 object new_cons = allocate_cons();
                                      Car(new_cons) = popSTACK();
                                      Cdr(new_cons) = STACK_(4+3+3);
                                      STACK_(4+3+3) = new_cons;
                            }   }   }}
                          skipSTACK(1); # direntry vergessen
                        }
                      # nächstes File:
                      if (findnext(&DTA_buffer) <0)
                        { if (!((errno==ENOENT) || (errno==ENOMORE))) { OS_error(); }
                          break; # Keine weitere Datei -> Schleifenende
                        }
                    }
                clr_break_sem_4();
               }
               #endif
              }
              skipSTACK(2); # pathname und dir-namestring vergessen
              next_pathname: ;
             }# Stackaufbau: ..., new-pathname-list, pathname-list-rest, pathnames-to-insert.
              # Vor dem Weiterrücken mit pathname-list-rest :
              # pathname-list-rest := (nreconc pathnames-to-insert pathname-list-rest) :
             {var reg1 object pathnames_to_insert = popSTACK();
              STACK_0 = nreconc(pathnames_to_insert,STACK_0);
            }}
          skipSTACK(1); # leere pathname-list-rest vergessen
          # new-pathname-list umdrehen, ersetzt die geleerte pathname-list:
          {var reg1 object new_pathname_list = popSTACK();
           STACK_0 = nreverse(new_pathname_list); # neue pathname-list
          }
          # Mit dieser Subdir-Stufe sind wir fertig.
          if (matomp(STACK_1)) break; # (atom subdir-list) -> fertig.
          recursively = FALSE; # die nächste (vorläufig) nicht-rekursiv
          passed_subdir: ;
        }}
      # Stackaufbau: result-list, pathname, name&type, subdir-list, pathname-list.
      # subdir-list ist =NIL geworden, auch pathname-list = NIL (denn beim
      # letzten Schleifendurchlauf ist immer next_task=0,1,2, und dadurch
      # wurde nichts auf new-pathname-list gepusht).
      skipSTACK(4);
      return popSTACK(); # result-list als Ergebnis
    }}
  #
  #endif # PATHNAME_NOEXT

LISPFUN(directory,0,1,norest,key,1, (kw(full)) )
# (DIRECTORY [pathname [:full]]), CLTL S. 427
  { # Stackaufbau: pathname, full.
    # :FULL-Argument hat Defaultwert NIL:
    if (eq(STACK_0,unbound)) { STACK_0 = NIL; }
    # Pathname-Argument überprüfen:
   {var reg1 object pathname = STACK_1;
    if (eq(pathname,unbound))
      {
        #if defined(PATHNAME_EXT83) || defined(PATHNAME_VMS)
        pathname = O(wild_wild_string); # Default ist "*.*" bzw. "*.*;*"
        #endif
        #ifdef PATHNAME_NOEXT
        pathname = O(wild_string); # Default ist "*"
        #endif
      }
    pathname = coerce_pathname(pathname); # zu einem Pathname machen
    # Los geht's:
    #ifdef PATHNAME_ATARI
    if (eq(ThePathname(pathname)->pathname_device,S(Kwild))) # Device = :WILD ?
      # alle Devices abzusuchen
      { STACK_1 = pathname;
        pushSTACK(O(drive_alist)); # Aliste aller Drives
        pushSTACK(NIL); # bisherige Pathname-Liste := NIL
        pushSTACK(STACK_(0+2)); # full (für directory_search)
        # Stackaufbau: pathname, full, Drive-Alist, pathname-list, full.
        while (mconsp(STACK_2)) # alle Drives durchlaufen
          { var reg3 object newpathname = copy_pathname(STACK_(1+3)); # Pathname kopieren
           {var reg2 object alistr = STACK_2; # restliche Drive-Aliste
            STACK_2 = Cdr(alistr); # verkürzen
            alistr = Car(alistr); # Alisteneintrag
            ThePathname(newpathname)->pathname_device = Car(alistr); # Drive übernehmen
           }# innerhalb eines Laufwerks suchen:
           {var reg2 object newpathnames = directory_search(newpathname);
            # und Pathname-Liste vor STACK_1 hängen:
            STACK_1 = nreconc(newpathnames,STACK_1);
          }}
        value1 = nreverse(STACK_1); # Pathname-Liste wieder umdrehen
        skipSTACK(2+3);
      }
      else
      # nur ein Device abzusuchen
    #endif
    #if defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2)
    if (eq(ThePathname(pathname)->pathname_device,S(Kwild))) # Device = :WILD ?
      # alle Devices abzusuchen
      { STACK_1 = pathname;
        pushSTACK(NIL); # bisherige Pathname-Liste := NIL
        pushSTACK(STACK_(0+1)); # full (für directory_search)
        # Stackaufbau: pathname, full, pathname-list, full.
        {var uintB drive;
         for (drive='A'; drive<='Z'; drive++) # alle Drives durchlaufen
           if (good_drive(drive))
             { pushSTACK(make_string(&drive,1)); # Device, einelementiger String
              {var reg2 object newpathname = copy_pathname(STACK_(1+2+1)); # Pathname kopieren
               ThePathname(newpathname)->pathname_device = popSTACK(); # Drive übernehmen
               # innerhalb eines Laufwerks suchen:
               {var reg3 object newpathnames = directory_search(newpathname);
                # und Pathname-Liste vor STACK_1 hängen:
                STACK_1 = nreconc(newpathnames,STACK_1);
        }    }}}
        value1 = nreverse(STACK_1); # Pathname-Liste wieder umdrehen
        skipSTACK(2+2);
      }
      else
      # nur ein Device abzusuchen
    #endif
      { value1 = directory_search(pathname); # matchende Pathnames bilden
        skipSTACK(2);
      }
    mv_count=1;
  }}

LISPFUN(cd,0,1,norest,nokey,0,NIL)
# (CD [pathname]) setzt das aktuelle Laufwerk und das aktuelle Directory.
  { var reg1 object pathname = popSTACK();
    if (eq(pathname,unbound)) { pathname = O(leer_string); } # "" als Default
    pathname = coerce_pathname(pathname); # zu einem Pathname machen
    # kopieren und Name und Typ auf NIL setzen:
    pathname = copy_pathname(pathname);
    ThePathname(pathname)->pathname_name = NIL;
    ThePathname(pathname)->pathname_type = NIL;
    check_no_wildcards(pathname); # mit Wildcards -> Fehler
    pathname = use_default_dir(pathname); # Pathname mit Seriennummer draus machen
    pushSTACK(pathname);
    assure_dir_exists(); # Directory muß existieren
    change_default(); # Default-Drive, Default-Directory setzen
    value1 = popSTACK(); mv_count=1; # neuer pathname als Wert
  }

# UP: Überprüft ein Pathname-Argument, ob Name und Typ beide =NIL sind,
# und ob das Directory "fast" existiert.
# shorter_directory_arg()
# > STACK_0 : Pathname-Argument
#if defined(ATARI) || defined(MSDOS)
# < ergebnis: Directory-Namestring (für DOS, ASCIZ, ohne '\' am Schluß)
#endif
#if defined(UNIX) || defined(AMIGAOS)
# < ergebnis: Directory-Namestring (fürs OS, ASCIZ, ohne '/' am Schluß)
#endif
#ifdef VMS
# < ergebnis: Directory-Namestring (für VMS, ASCIZ)
#endif
# Erhöht STACK um 1.
# kann GC auslösen
  local object shorter_directory_arg (void);
  local object shorter_directory_arg()
    { var reg1 object pathname = coerce_pathname_arg(); # Argument zu einem Pathname machen
      check_no_wildcards(pathname); # mit Wildcards -> Fehler
      pathname = use_default_dir(pathname); # Default-Directory einfügen
      # Überprüfe, ob Name=NIL und Typ=NIL :
      if (!(nullp(ThePathname(pathname)->pathname_name)
            && nullp(ThePathname(pathname)->pathname_type)
         ) )
        { pushSTACK(pathname);
          fehler(
                 DEUTSCH ? "Das ist keine Directory-Angabe: ~" :
                 ENGLISH ? "not a directory: ~" :
                 FRANCAIS ? "Ceci ne désigne pas un répertoire : ~" :
                 ""
                );
        }
      pushSTACK(pathname); # neuen Pathname retten
     #ifdef VMS
      {var reg3 object dir_namestring = assure_dir_exists(); # nicht wirklich überprüfen!??
       skipSTACK(1);
       return string_to_asciz(dir_namestring); # Directory-Namestring als ASCIZ
      }
     #else
      # verkürze das Directory:
      {var reg2 object subdirs = ThePathname(pathname)->pathname_directory;
       #if HAS_SERNR
       if (nullp(Cdr(Cdr(subdirs)))) # Root-Directory ?
       #else
       if (nullp(Cdr(subdirs))) # Root-Directory ?
       #endif
         { baddir:
           # STACK_0 = pathname
           fehler(
                  DEUTSCH ? "Hier sind nur echte Unterdirectories zulässig, nicht ~" :
                  ENGLISH ? "root directory not allowed here: ~" :
                  FRANCAIS ? "Le répertoire racine n'est pas permis ici : ~" :
                  ""
                 );
         }
       subdirs = reverse(subdirs); # Liste kopieren und dabei umdrehen
       #ifdef AMIGAOS
       if (eq(Car(subdirs),S(Kparent))) # letztes Subdir muß /= :PARENT sein
         goto baddir;
       #endif
       pushSTACK(subdirs); # Cons mit letztem Subdir als CAR retten
       subdirs = Cdr(subdirs); # alle Subdirs bis aufs letzte
       subdirs = nreverse(subdirs); # wieder in die richtige Reihenfolge bringen
       pathname = STACK_1;
       ThePathname(pathname)->pathname_directory = subdirs; # und in den Pathname setzen
       # Dieses Directory muß existieren:
       pushSTACK(pathname);
       # Stackaufbau: pathname, subdircons, pathname.
       {var reg3 object dir_namestring = assure_dir_exists();
        # Baue ASCIZ-String des Subdir für OS:
        STACK_0 = dir_namestring; # bisheriger Directory-Namestring als 1. String
        {var reg4 uintC stringcount =
           subdir_namestring_parts(STACK_1); # und Strings zum letzten Subdir
         # und kein '\' am Schluß (für DOS)
         # und kein '/' am Schluß (fürs OS)
         pushSTACK(O(null_string)); # und Nullbyte als letzten String
         {var reg5 object dirstring = string_concat(1+stringcount+1); # zusammenhängen
          skipSTACK(2);
          return dirstring;
      }}}}
     #endif
    }

LISPFUNN(make_dir,1)
# (MAKE-DIR pathname) legt ein neues Unterdirectory pathname an.
  { var reg1 object pathstring = shorter_directory_arg();
    #ifdef ATARI
    var reg2 sintW errorcode =
      GEMDOS_mkdir(TheAsciz(pathstring)); # Unterdirectory erzeugen
    if (errorcode < 0) { OS_error(errorcode); } # Error melden
    #endif
    #ifdef AMIGAOS
    set_break_sem_4();
    begin_system_call();
    {var reg2 BPTR lock = CreateDir(TheAsciz(pathstring)); # Unterdirectory erzeugen
     if (lock==BPTR_NULL) { OS_error(); }
     UnLock(lock); # Lock freigeben
    }
    end_system_call();
    clr_break_sem_4();
    #endif
    #if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX) || defined(VMS)
    begin_system_call();
    if (mkdir(TheAsciz(pathstring),0777)) # Unterdirectory erzeugen
      { OS_error(); }
    end_system_call();
    #endif
    value1 = T; mv_count=1; # 1 Wert T
  }

LISPFUNN(delete_dir,1)
# (DELETE-DIR pathname) entfernt das Unterdirectory pathname.
  { var reg1 object pathstring = shorter_directory_arg();
    #ifdef ATARI
    var reg2 sintW errorcode =
      GEMDOS_rmdir(TheAsciz(pathstring)); # Unterdirectory löschen
    if (errorcode < 0) { OS_error(errorcode); } # Error melden
    #endif
    #ifdef AMIGAOS
    # Noch Test, ob's auch ein Directory und kein File ist??
    begin_system_call();
    if (! DeleteFile(TheAsciz(pathstring)) ) # Unterdirectory löschen
      { OS_error(); }
    end_system_call();
    #endif
    #if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX)
    begin_system_call();
    if (rmdir(TheAsciz(pathstring))) # Unterdirectory löschen
      { OS_error(); }
    end_system_call();
    #endif
    #ifdef VMS
    ??
    #endif
    value1 = T; mv_count=1; # 1 Wert T
  }

# UP: Initialisiert das Pathname-System.
# init_pathnames();
# kann GC auslösen
  global void init_pathnames (void);
  global void init_pathnames()
    {
      #ifdef PATHNAME_ATARI
      # Default-Drive initialisieren:
      { var reg2 sintW errorcode = GEMDOS_CurrentDisk();
        if (errorcode<0) { OS_error(errorcode); }
        # errorcode ist jetzt die Nummer des aktuellen Laufwerks.
       {var reg1 object string = allocate_string(1); # String der Länge 1
        TheSstring(string)->data[0] = 'A' + errorcode; # mit Laufwerksbuchstaben
        O(default_drive) = string;
      }}
      # Drive-Alist initialisieren:
      { var reg3 uint16 drivebits = # Bitvektor aller angeschlossenen Laufwerke
          BIOS_DriveMap();
        pushSTACK(NIL); # Liste := NIL
       {var reg4 uintB bitnr = 16;
        # Bits 15..0 durchlaufen (abwärts):
        do { bitnr--;
             if (drivebits & bit(15)) # Bit bitnr im Bitvektor drivebits gesetzt ?
               { var reg2 object string = allocate_string(1); # String der Länge 1
                 TheSstring(string)->data[0] = 'A' + bitnr; # mit Laufwerksbuchstaben
                 pushSTACK(string);
                {var reg1 object new_cons = allocate_cons(); # neues Alisten-Cons
                 Car(new_cons) = popSTACK(); # new_cons = (drive . NIL)
                 # auf STACK_0 consen:
                 pushSTACK(new_cons);
                 new_cons = allocate_cons();
                 Car(new_cons) = popSTACK(); Cdr(new_cons) = STACK_0;
                 STACK_0 = new_cons;
               }}
             drivebits <<= 1;
           }
           until (bitnr == 0);
        O(drive_alist) = popSTACK();
      }}
      # Aktuellen Pfad (von GEMDOS) zum aktuellen Directory auf der
      # im Default-Drive befindlichen Diskette erklären:
      { # Buffer für GEMDOS-Interaktion, vgl. GEMDOS_GetDir:
        var uintB path_buffer[64+1];
        var reg3 object pathname;
        { # aktuelles Directory im aktuellen Drive in path_buffer ablegen:
         {var reg2 sintW errorcode = GEMDOS_GetDir(&path_buffer[0],0);
          if (errorcode<0) { OS_error(errorcode); }
         }# Am Schluß ein '\' anfügen:
         {var reg1 uintB* pathptr = &path_buffer[0];
          var reg2 uintL len = 0; # Stringlänge
          until (*pathptr == 0) { pathptr++; len++; } # ASCIZ-Stringende suchen
          *pathptr = '\\'; len++; # ein '\' anfügen
          # und in einen String umwandeln:
          pathname = make_string(&path_buffer[0],len);
        }}
        pathname = coerce_pathname(pathname); # Pathname draus machen
        # Pathname enthält jetzt das aktuelle Directory.
        pathname = use_default_dir(pathname);
        # Pathname enthält jetzt das aktuelle Laufwerk und von der darin
        # befindlichen Diskette die Seriennummer und das aktuelle Directory.
        pushSTACK(pathname);
        change_default(); # Default-Drive, Default-Directory setzen,
                          # *DEFAULT-PATHNAME-DEFAULTS* initialisieren
        skipSTACK(1);
      }
      #else
      #if defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2)
      { # Default-Drive initialisieren:
        var uintB drive = default_drive();
        O(default_drive) = make_string(&drive,1);
      }
      #endif
      # *DEFAULT-PATHNAME-DEFAULTS* initialisieren:
      recalc_defaults_pathname();
      #endif
      #ifdef USER_HOMEDIR
      #ifdef UNIX
      # Wir ziehen uns das Home-Directory und die benutzbare Shell aus dem
      # Environment. Es enthält (fast) immer mindestens folgende Variablen:
      #   LOGNAME = Username beim ersten Einloggen ("wahre" Identität des Benutzers)
      #   USER    = aktueller Username
      #   HOME    = aktuelles Home-Directory, aus /etc/passwd geholt
      #   SHELL   = aktuelle Standard-Shell, aus /etc/passwd geholt
      #   PATH    = Suchpfad bei Programmaufruf
      #   TERM    = Terminalemulation
      # Wir holen uns HOME (für "~" - Übersetzung) und SHELL (für EXECUTE).
      # Bei "~username" müssen wir das /etc/passwd - File absuchen.
      { # Im Environment nach Variable HOME suchen:
        begin_system_call();
       {var reg1 const char* homedir = getenv("HOME");
        end_system_call();
        if (!(homedir==NULL)) # gefunden?
          { O(user_homedir) = asciz_dir_to_pathname(homedir); } # ja -> eintragen
          else
          # nein -> Home-Directory aus dem Passwort-File holen:
          { # empfohlene Methode (siehe GETLOGIN(3V)): erst
            # getpwnam(getlogin()), dann getpwuid(getuid()) probieren.
            var reg2 const char* username;
            var reg1 struct passwd * userpasswd;
            begin_system_call();
            # 1. Versuch: getpwnam(getenv("USER"))
            username = getenv("USER"); # Username aus dem Environment holen
            if (!(username==NULL))
              { errno = 0; userpasswd = getpwnam(username); # passwd-Eintrag dazu
                if (!(userpasswd==NULL)) goto userpasswd_ok; # gefunden -> ok
                if (!(errno==0)) { OS_error(); } # Error melden
              }
            # 2. Versuch: getpwnam(getlogin())
            errno = 0; username = getlogin(); # Username aus /etc/utmp holen
            if (username==NULL)
              { if (!(errno==0)) { OS_error(); } } # Error melden
              else
              { errno = 0; userpasswd = getpwnam(username); # passwd-Eintrag dazu
                if (!(userpasswd==NULL)) goto userpasswd_ok; # gefunden -> ok
                if (!(errno==0)) { OS_error(); } # Error melden
              }
            # 3. Versuch: getpwuid(getuid())
            errno = 0; userpasswd = getpwuid(getuid());
            if (!(userpasswd==NULL)) # gefunden?
              { userpasswd_ok:
                end_system_call();
                O(user_homedir) = asciz_dir_to_pathname(userpasswd->pw_dir); # ja -> Homedir als Pathname eintragen
              }
              else
              { if (!(errno==0)) { OS_error(); } # Error melden
                end_system_call();
                # nein -> aktuelles Directory nehmen:
                O(user_homedir) = default_directory();
      }}  }   }
      #endif
      #ifdef VMS
      { # Im Environment nach Variable HOME bzw. SYS$LOGIN suchen.
        # (Diese scheinen bis auf Klein-/Groß-Schreibung dasselbe zu enthalten.)
        begin_system_call();
       {var reg1 const char* homedir = getenv("HOME");
        if (!(homedir==NULL)) goto homedir_ok;
        homedir = getenv("SYS$LOGIN");
        if (!(homedir==NULL))
          { homedir_ok:
            end_system_call();
            O(user_homedir) = asciz_dir_to_pathname(homedir);
          }
          else
          { end_system_call();
            O(user_homedir) = default_directory(); # aktuelles Directory nehmen
      }}  }
      #endif
      #endif
      #if defined(HAVE_SHELL) && !defined(ATARI) && !defined(AMIGAOS) && !defined(VMS)
      { # Im Environment nach Variable SHELL bzw. COMSPEC suchen:
        begin_system_call();
       {var reg1 const char* shell =
          #ifdef UNIX
            getenv("SHELL")
          #endif
          #ifdef MSDOS
            getenv("COMSPEC")
          #endif
          ;
        end_system_call();
        if (!(shell==NULL)) # gefunden?
          { O(user_shell) = asciz_to_string(shell); } # ja -> eintragen
          # sonst bleibt O(user_shell) auf dem Defaultwert "/bin/csh" bzw. "\\COMMAND.COM".
      }}
      #endif
    }

#if defined(ATARI) || defined(DJUNIX) || defined(EMUNIX_OLD_8d)
# UP: Legt Datum/Uhrzeit der Datei mit dem Handle handle im 4-Byte-Buffer ab.
# get_file_write_datetime(handle);
# > handle: Handle eines (offenen) Files
# < file_datetime: Datum und Uhrzeit der Datei
  local var struct { uintW time; uintW date; } file_datetime; # Buffer fürs Ergebnis
  local void get_file_write_datetime (uintW handle);
  #ifdef ATARI
  local void get_file_write_datetime(handle)
    var reg1 uintW handle;
    { # Datum und Uhrzeit der Datei in den Buffer:
      var reg2 sintW errorcode = GEMDOS_GSDTOF(&file_datetime,handle);
      if (errorcode<0) { OS_error(errorcode); } # Fehler aufgetreten?
    }
  #endif
  #ifdef DJUNIX
  #include <dos.h>
  local void get_file_write_datetime(handle)
    var reg1 uintW handle;
    {
     #ifndef GNU
      var union REGS in;
      var union REGS out;
      in.h.ah = 0x57; in.h.al = 0; # DOS Function 57H
      in.x.bx = handle;
      intdos(&in,&out);
      file_datetime.time = out.x.cx;
      file_datetime.date = out.x.dx;
     #else # dasselbe, nur effizienter
      var uintW time;
      var uintW date;
      __asm__ (# DOS Function 57H
               " movw $0x5700,%%ax ; int $0x21 "
               : "=c" /* %cx */ (time), "=d" /* %dx */ (date)     # OUT
               :                                                  # IN
               : "ax","bx","si","di" /* %eax, %ebx, %esi, %edi */ # CLOBBER
              );
      file_datetime.time = time;
      file_datetime.date = date;
     #endif
    }
  #endif
  #ifdef EMUNIX_OLD_8d
  extern int __filetime ( /* int handle, int flag, struct _ftd * */ );
  #define get_file_write_datetime(handle)  __filetime(handle,0,&file_datetime)
  #endif
#endif
#ifdef AMIGAOS
  local var struct DateStamp file_datetime; # Buffer für Datum/Uhrzeit einer Datei
#endif
#if defined(UNIX) || defined(EMUNIX_NEW_8e) || defined(VMS)
  local var time_t file_datetime; # Buffer für Datum/Uhrzeit einer Datei
#endif

LISPFUNN(file_write_date,1)
# (FILE-WRITE-DATE file), CLTL S. 424
  { var reg1 object pathname = popSTACK(); # pathname-Argument
    if (streamp(pathname))
      # Stream -> extra behandeln:
      { # muß File-Stream sein:
        if_strm_file_p(pathname, ; , fehler_thing(pathname); );
        # Streamtyp File-Stream
       #if !defined(AMIGAOS)
        if ((TheStream(pathname)->strmflags & strmflags_open_B)
            && (!nullp(TheStream(pathname)->strm_file_handle))
           )
          # offener File-Stream
          { # direkt mit dem Handle arbeiten:
            #if defined(ATARI) || defined(DJUNIX) || defined(EMUNIX_OLD_8d)
            get_file_write_datetime(TheHandle(TheStream(pathname)->strm_file_handle));
            #endif
            #if defined(UNIX) || defined(EMUNIX_NEW_8e) || defined(VMS)
            var struct stat status;
            begin_system_call();
            if (!( fstat(TheHandle(TheStream(pathname)->strm_file_handle),&status) ==0))
              { OS_error(); }
            end_system_call();
            file_datetime = status.st_mtime;
            #endif
          }
          else
       #endif
          # geschlossener File-Stream -> Truename als Pathname verwenden
          { pathname = TheStream(pathname)->strm_file_truename;
            goto is_pathname;
          }
      }
      else
      { pathname = coerce_pathname(pathname); # zu einem Pathname machen
        is_pathname: # pathname ist jetzt wirklich ein Pathname
        check_no_wildcards(pathname); # mit Wildcards -> Fehler
        pathname = use_default_dir(pathname); # Default-Directory einfügen
        if (namenullp(pathname)) { fehler_noname(pathname); } # Kein Name angegeben -> Fehler
        # Name angegeben.
        pushSTACK(pathname);
       {# Directory muß existieren:
        var reg3 object namestring = assure_dir_exists(); # Filename als ASCIZ-String
        #ifdef ATARI
        # Datei öffnen:
        var reg2 sintW errorcode;
        errorcode = # Datei zu öffnen versuchen, Modus 0 (Read)
          GEMDOS_open(TheAsciz(namestring),0);
        if (errorcode < 0) { OS_error(errorcode); } # Error melden
        # Nun enthält errorcode das Handle des geöffneten Files.
        get_file_write_datetime(errorcode); # Datum/Uhrzeit holen
        errorcode = # Datei gleich wieder schließen
          GEMDOS_close(errorcode);
        if (errorcode < 0) { OS_error(errorcode); } # Error melden
        #endif
        #ifdef MSDOS
         #if defined(DJUNIX) || defined(EMUNIX_OLD_8d)
          # Datei öffnen:
          var reg2 sintW ergebnis = # Datei zu öffnen versuchen
            open(TheAsciz(namestring),O_RDONLY);
          if (ergebnis < 0) { OS_error(); } # Error melden
          # Nun enthält ergebnis das Handle des geöffneten Files.
          get_file_write_datetime(ergebnis); # Datum/Uhrzeit holen
          if (close(ergebnis) < 0) { OS_error(); } # Datei gleich wieder schließen
         #else # defined(EMUNIX_NEW_8e)
          { var struct stat statbuf;
            if (stat(TheAsciz(namestring),&statbuf) < 0) { OS_error(); }
            if (!S_ISREG(statbuf.st_mode)) { fehler_file_not_exists(); } # Datei muß existieren
            file_datetime = statbuf.st_mtime;
          }
         #endif
        #endif
        #ifdef AMIGAOS
        if (!file_exists(_EMA_)) { fehler_file_not_exists(); } # Datei muß existieren
        file_datetime = filestatus->fib_Date;
        #endif
        #if defined(UNIX) || defined(VMS)
        if (!file_exists(_EMA_)) { fehler_file_not_exists(); } # Datei muß existieren
        file_datetime = filestatus->st_mtime;
        #endif
        skipSTACK(1);
      }}
    # Datum/Uhrzeit steht nun im Buffer file_datetime.
    # In Decoded-Time-Format umwandeln:
    { var decoded_time timepoint;
      #if defined(ATARI) || defined(DJUNIX) || defined(EMUNIX_OLD_8d)
      convert_timedate(file_datetime.time,file_datetime.date,&timepoint);
      #endif
      #if defined(UNIX) || defined(EMUNIX_NEW_8e) || defined(AMIGAOS) || defined(VMS)
      convert_time(&file_datetime,&timepoint);
      #endif
      pushSTACK(timepoint.Sekunden);
      pushSTACK(timepoint.Minuten);
      pushSTACK(timepoint.Stunden);
      pushSTACK(timepoint.Tag);
      pushSTACK(timepoint.Monat);
      pushSTACK(timepoint.Jahr);
      funcall(S(encode_universal_time),6);
      # (ENCODE-UNIVERSAL-TIME Sekunden Minuten Stunden Tag Monat Jahr)
      # als Ergebnis
  } }

LISPFUNN(file_author,1)
# (FILE-AUTHOR file), CLTL S. 424
  { var reg1 object pathname = popSTACK(); # pathname-Argument
    if (streamp(pathname))
      # Stream -> extra behandeln:
      { # muß File-Stream sein:
        if_strm_file_p(pathname, ; , fehler_thing(pathname); );
        # Streamtyp File-Stream
        if (TheStream(pathname)->strmflags & strmflags_open_B)
          # offener File-Stream -> OK
          {}
          else
          # geschlossener File-Stream -> Truename als Pathname verwenden
          { pathname = TheStream(pathname)->strm_file_truename;
            goto is_pathname;
          }
      }
      else
      { pathname = coerce_pathname(pathname); # zu einem Pathname machen
        is_pathname: # pathname ist jetzt wirklich ein Pathname
        # pathname ist jetzt ein Pathname.
        check_no_wildcards(pathname); # mit Wildcards -> Fehler
        pathname = use_default_dir(pathname); # Default-Directory einfügen
        if (namenullp(pathname)) { fehler_noname(pathname); } # Kein Name angegeben -> Fehler
        # Name angegeben.
        pushSTACK(pathname);
       {# Directory muß existieren:
        var reg3 object namestring = assure_dir_exists(); # Filename als ASCIZ-String
        #ifdef ATARI
        # Überprüfe, ob die Datei existiert:
        var reg2 sintW errorcode;
        errorcode = # Datei zu öffnen versuchen, Modus 0 (Read)
          GEMDOS_open(TheAsciz(namestring),0);
        if (errorcode < 0) { OS_error(errorcode); } # Error melden
        # Nun enthält errorcode das Handle des geöffneten Files.
        errorcode = # Datei gleich wieder schließen
          GEMDOS_close(errorcode);
        if (errorcode < 0) { OS_error(errorcode); } # Error melden
        #endif
        #ifdef MSDOS
         #if 1
          # Datei öffnen:
          var reg2 sintW ergebnis = # Datei zu öffnen versuchen
            open(TheAsciz(namestring),O_RDONLY);
          if (ergebnis < 0) { OS_error(); } # Error melden
          # Nun enthält ergebnis das Handle des geöffneten Files.
          if (close(ergebnis) < 0) { OS_error(); } # Datei gleich wieder schließen
         #else
          { var struct stat statbuf;
            if (stat(TheAsciz(namestring),&statbuf) < 0) { OS_error(); }
            if (!S_ISREG(statbuf.st_mode)) { fehler_file_not_exists(); } # Datei muß existieren
          }
         #endif
        #endif
        #if defined(UNIX) || defined(AMIGAOS) || defined(VMS)
        if (!file_exists(_EMA_)) { fehler_file_not_exists(); } # Datei muß existieren
        #endif
        skipSTACK(1);
      }}
    # Datei existiert -> NIL als Wert
    value1 = NIL; mv_count=1;
  }

#ifdef ATARI

LISPFUN(execute,1,2,norest,nokey,0,NIL)
# (EXECUTE file [command-tail [space]]) ruft file auf, mit command-tail als
# Argumentstring. Es werden space Bytes fürs Programm zur Verfügung gestellt.
# Default für command-tail ist "".
# Default für space ist ein wenig mehr als (file-length file).
  { # file überprüfen:
   {var reg1 object pathname = STACK_2;
    pathname = coerce_pathname(pathname); # zu einem Pathname machen
    # pathname ist jetzt ein Pathname.
    check_no_wildcards(pathname); # mit Wildcards -> Fehler
    pathname = use_default_dir(pathname); # Default-Directory einfügen
    if (namenullp(pathname)) { fehler_noname(pathname); } # Kein Name angegeben -> Fehler
    # Name angegeben.
    pushSTACK(pathname);
   }# Directory muß existieren:
    STACK_(2+1) = assure_dir_exists(); # Filename als ASCIZ-String
    # Stackaufbau: filename, command-tail, space, pathname.
    # Command-Tail überprüfen:
   {var reg1 object command_tail = STACK_(1+1);
    if (eq(command_tail,unbound))
      { command_tail = O(leer_string); } # "" als Default
      else
      { if (!stringp(command_tail))
          { pushSTACK(command_tail);
            pushSTACK(TheSubr(subr_self)->name);
            fehler(
                   DEUTSCH ? "~: Command-Tail muß ein String sein, nicht ~" :
                   ENGLISH ? "~: command tail should be a string, not ~" :
                   FRANCAIS ? "~ : le paramètre de commande doit être une chaîne et non ~" :
                   ""
                  );
          }
        command_tail = coerce_ss(command_tail); # in Simple-String umwandeln
      }
    STACK_(1+1) = command_tail;
   }# space überprüfen:
   {var reg5 uintL space; # Fürs aufzurufende Programm benötigter Platz
    if (!eq(STACK_(0+1),unbound))
      # space angegeben
      { skipSTACK(1); # Pathname vergessen
        if (!mposfixnump(STACK_0))
          { # STACK_0 = space
            pushSTACK(TheSubr(subr_self)->name);
            fehler(
                   DEUTSCH ? "~: Platz-Angabe muß ein Fixnum >=0 sein, nicht ~" :
                   ENGLISH ? "~: space need should be a nonnegative fixnum, not ~" :
                   FRANCAIS ? "~ : Le besoin en place doit être exprimé par un Fixnum positif et non ~" :
                   ""
                  );
          }
        space = posfixnum_to_L(popSTACK()); # space-Argument verwenden
      }
      else
      # space bestimmt sich aus dem Header des File. STACK_0 = pathname.
      { # (OPEN pathname :ELEMENT-TYPE 'UNSIGNED-BYTE) ausführen:
        pushSTACK(S(Kelement_type)); pushSTACK(S(unsigned_byte));
        funcall(L(open),3);
       {var reg4 object stream = value1; # offener Byte-Stream auf pathname
        # Stackaufbau: filename, command-tail, dummy.
        # Überprüfe, ob das File ein ausführbares Programm ist:
        # Erste 14 Bytes lesen (testet auch, ob File-Länge >=14):
        var uintB buffer[14]; # Word-aligned!
        {var reg1 uintB* buffptr = &buffer[0];
         var reg3 uintC count;
         dotimesC(count,14,
           { var reg2 object b = read_byte(stream); # nächstes Byte lesen (löst keine GC aus)
             if (eq(b,eof_value)) { goto bad; } # File zu klein -> war nix
             *buffptr++ = (uintB)posfixnum_to_L(b); # Byte ablegen
           });
        }
        # File muß mit $601A anfangen:
        if (!(*(uintW*)(&buffer[0]) == 0x601A))
          { bad: # File zu kurz oder fängt nicht mit $601A an
            STACK_0 = stream; stream_close(&STACK_0); # File wieder schließen
            pushSTACK(STACK_2); # filename
            pushSTACK(TheSubr(subr_self)->name);
            fehler(
                   DEUTSCH ? "~: File ~ ist kein ausführbares Programm." :
                   ENGLISH ? "~: file ~ is not an executable program" :
                   FRANCAIS ? "~ : Le fichier ~ n'est pas exécutable." :
                   ""
                  );
          }
        space = *(uintL*)(&buffer[2]) # Länge TEXT-Segment
                + *(uintL*)(&buffer[6]) # Länge DATA-Segment
                + *(uintL*)(&buffer[10]) # Länge BSS-Segment
                + sizeof(BASEPAGE) # Länge Basepage
                + 1000 # genehmigte Stacklänge
                ;
        STACK_0 = stream; stream_close(&STACK_0); # File wieder schließen
        skipSTACK(1);
      }}
    # Stackaufbau: filename (Simple-ASCIZ-String), Command-tail (Simple-String)
    # Programm aufrufen:
    {var reg1 sintL ergebnis = execute(floor(space,2)*2);
     if (ergebnis < 0) { OS_error(ergebnis); } # Error melden
     # Rückgabewert verwerten: =0 (OK) -> T, >0 (nicht OK) -> NIL :
     value1 = (ergebnis==0 ? T : NIL); mv_count=1;
  }}}

#endif

#if defined(UNIX) || defined(MSDOS) || defined(VMS)

LISPFUN(execute,1,0,rest,nokey,0,NIL)
# (EXECUTE file arg1 arg2 ...) ruft ein File mit gegebenen Argumenten auf.
  {var reg6 object* args_pointer = rest_args_pointer STACKop 1;
   {var reg1 object* argptr = args_pointer; # Pointer über die Argumente
    # File überprüfen:
    { var reg2 object* file_ = &NEXT(argptr);
      var reg3 object pathname = *file_;
      pathname = coerce_pathname(pathname); # zu einem Pathname machen
      check_no_wildcards(pathname); # mit Wildcards -> Fehler
      pathname = use_default_dir(pathname); # Default-Directory einfügen
      if (namenullp(pathname)) { fehler_noname(pathname); } # Kein Name angegeben -> Fehler
      # Name angegeben.
      pushSTACK(pathname);
     {# Directory muß existieren:
      var reg4 object namestring = assure_dir_exists(); # Filename als ASCIZ-String
      # Überprüfe, ob die Datei existiert:
      if_file_exists(namestring, ; , { fehler_file_not_exists(); } );
      *file_ = namestring; # retten
      skipSTACK(1);
    }}
    # restliche Argumente überprüfen:
    { var reg3 uintC count;
      dotimesC(count,argcount,
        { var reg2 object* arg_ = &NEXT(argptr);
          pushSTACK(*arg_); funcall(L(string),1); # nächstes Argument in String umwandeln
          *arg_ = string_to_asciz(value1); # und ASCIZ-String umwandeln
        });
   }}
   #ifdef DJUNIX
   # Alle Argumente (nun ASCIZ-Strings) zusammenhängen, mit Spaces dazwischen:
   {var reg1 object* argptr = args_pointer; # Pointer über die Argumente
    var reg3 uintC count;
    dotimesC(count,argcount, # alle Argumente außer dem letzten durchlaufen
      { var reg2 object string = NEXT(argptr); # nächster Argumentstring
        TheSstring(string)->data[TheSstring(string)->length - 1] = ' ';
      });
   }
   { var reg2 object command = string_concat(1+argcount);
     # Programm aufrufen:
     begin_system_call();
    {var reg1 int ergebnis = system(TheAsciz(command));
     end_system_call();
     # Rückgabewert verwerten: =0 (OK) -> T, >0 (nicht OK) -> NIL :
     value1 = (ergebnis==0 ? T : NIL); mv_count=1;
   }}
   #endif
   #ifdef EMUNIX
   {# argv-Array aufbauen:
    var reg5 DYNAMIC_ARRAY(argv,char*,1+(uintL)argcount+1);
    { var reg1 object* argptr = args_pointer;
      var reg2 char** argvptr = &argv[0];
      var reg4 uintC count;
      dotimespC(count,argcount+1,
        { var reg3 object arg = NEXT(argptr); # nächstes Argument, ASCIZ-String
          *argvptr++ = TheAsciz(arg); # in argv einfüllen
        });
      *argvptr = NULL; # und mit Nullpointer abschließen
    }
    # Programm aufrufen:
    begin_system_call();
    {var reg1 int ergebnis = spawnv(P_WAIT,argv[0],argv);
     end_system_call();
     if (ergebnis < 0) { OS_error(); } # Error melden
     # Fertig.
     set_args_end_pointer(args_pointer); # STACK aufräumen
     # Rückgabewert verwerten: =0 (OK) -> T, >0 (nicht OK) -> NIL :
     value1 = (ergebnis==0 ? T : NIL); mv_count=1;
    }
    FREE_DYNAMIC_ARRAY(argv);
   }
   #endif
   #if defined(UNIX) || defined(VMS)
   {# argv-Array aufbauen:
    var reg5 DYNAMIC_ARRAY(argv,char*,1+(uintL)argcount+1);
    { var reg1 object* argptr = args_pointer;
      var reg2 char** argvptr = &argv[0];
      var reg4 uintC count;
      dotimespC(count,argcount+1,
        { var reg3 object arg = NEXT(argptr); # nächstes Argument, ASCIZ-String
          *argvptr++ = TheAsciz(arg); # in argv einfüllen
        });
      *argvptr = NULL; # und mit Nullpointer abschließen
    }
    # einen neuen Prozeß starten:
    { var reg2 int child;
      begin_system_call();
      if ((child = vfork()) ==0)
        # Dieses Programmstück wird vom Child-Prozeß ausgeführt:
        { execv(argv[0],argv); # Programm aufrufen
          _exit(-1); # sollte dies mißlingen, Child-Prozeß beenden
        }
      # Dieses Programmstück wird wieder vom Aufrufer ausgeführt:
      if (child==-1)
        # Etwas ist mißlungen, entweder beim vfork oder beim execv.
        # In beiden Fällen wurde errno gesetzt.
        { OS_error(); }
      # Warten, bis der Child-Prozeß beendet wird:
     {var int status = 0;
      # vgl. WAIT(2V) und #include <sys/wait.h> :
      #   WIFSTOPPED(status)  ==  ((status & 0xFF) == 0177)
      #   WEXITSTATUS(status)  == ((status & 0xFF00) >> 8)
      #ifdef HAVE_WAITPID
      loop
        { if (!(waitpid(child,&status,0) == child)) { OS_error(); }
          if (!((status & 0xFF) == 0177)) break; # Child-Prozeß beendet?
        }
      #else
      loop
        { var reg1 int ergebnis = wait(&status);
          if (ergebnis < 0) { OS_error(); }
          if ((ergebnis == child) && !((status & 0xFF) == 0177)) break; # Child-Prozeß beendet?
        }
      #endif
      end_system_call();
      # Fertig.
      set_args_end_pointer(args_pointer); # STACK aufräumen
      value1 = (((status & 0xFF) == 0000) # Prozeß normal beendet (ohne Signal, ohne Core-Dump) ?
                ? # ja -> Exit-Status als Wert:
                  fixnum( (status & 0xFF00) >> 8)
                : NIL # nein -> NIL als Wert
               );
      mv_count=1;
    }}
    FREE_DYNAMIC_ARRAY(argv);
   }
   #endif
  }

#endif

#ifdef AMIGAOS

LISPFUN(execute,1,0,norest,nokey,0,NIL)
# (EXECUTE command-string) schickt einen String an das Betriebssystem.
# Das ist in diesem Fall mit (SHELL command-string) synonym.
  { C_shell(); } # SHELL aufrufen, selber Stackaufbau

#endif

#ifdef HAVE_SHELL

# (SHELL) ruft eine Shell auf.
# (SHELL command) ruft eine Shell auf und läßt sie ein Kommando ausführen.

#if defined(ATARI)

local void (*online_shell)(); # Pointer auf eine online verfügbare Shell, oder NULL

# UP: Holt den Pointer auf die Online-Shell, falls vorhanden.
  local void get_online_shell (void);
  local void get_online_shell()
    { online_shell = *(void* *)0x04F6; } # nur im Supervisor-Modus aufzurufen!

# UP: führt ein Shell-Kommando aus.
# > command: Kommando, ein String
# kann GC auslösen
  local void do_shell (object command);
  local void do_shell(command)
    var reg1 object command; # Kommando, ein String
    { if (!(online_shell==NULL))
        # Online-Shell verfügbar
        { (*online_shell)(TheAsciz(string_to_asciz(command))); }
        else
        # Unsere eigene kleine "Shell" aufrufen:
        { pushSTACK(command); funcall(S(myshell),1); } # (SYS::MYSHELL command)
    }

LISPFUN(shell,0,1,norest,nokey,0,NIL)
  { var reg1 object command = popSTACK();
    Supervisor_Exec(get_online_shell);
    if (eq(command,unbound))
      { # Wir lesen Zeile für Zeile ein und übergeben diese der Shell.
        # (loop
        #   (write-string "$ ")
        #   (let ((line (read-line)))
        #     (cond ((string-equal line "exit") (return))
        #           ((string-equal line "help") (write-line "Mit EXIT zurück zum Lisp."))
        #           (t (shell line))
        # ) ) )
        loop
          { pushSTACK(O(shell_prompt)); funcall(L(write_string),1); # Prompt ausgeben
            funcall(L(read_line),0); # Zeile lesen
            if (!stringp(value1)) break; # EOF -> fertig
            if (string_equal(value1,O(shell_exit))) break; # EXIT -> fertig
            if (string_equal(value1,O(shell_help))) # HELP-Kommando
              { pushSTACK(O(shell_helpstring)); funcall(L(write_line),1); }
            else
              { do_shell(value1); } # sonstiges Shell-Kommando ausführen
          }
      }
      else
      { pushSTACK(command); funcall(L(string),1); # Argument in String umwandeln
        do_shell(value1);
      }
    value1 = T; mv_count=1; # T als Wert
  }

#elif defined(AMIGAOS)

LISPFUN(shell,0,1,norest,nokey,0,NIL)
  { var reg1 object command = popSTACK();
    if (eq(command,unbound))
      # Kommandointerpreter aufrufen:
      { run_time_stop();
        begin_system_call();
       {var reg2 BOOL ergebnis = FALSE;
        #if 0 # so einfach geht's wohl nicht
        ergebnis = Execute("",Input_handle,Output_handle);
        #else
        var reg3 Handle terminal = Open("*",MODE_READWRITE);
        if (!(terminal==Handle_NULL))
          { ergebnis = Execute("",terminal,Handle_NULL);
            Close(terminal);
            Write(Output_handle,CRLFstring,1);
          }
        #endif
        end_system_call();
        run_time_restart();
        # Rückgabewert verwerten: ausgeführt -> T, nicht gefunden -> NIL :
        value1 = (ergebnis ? T : NIL); mv_count=1;
      }}
      else
      # einzelnes Kommando ausführen:
      { if (!stringp(command))
          { pushSTACK(command);
            pushSTACK(TheSubr(subr_self)->name);
            fehler(
                   DEUTSCH ? "~: Befehl muß ein String sein, nicht ~." :
                   ENGLISH ? "~: the command should be a string, not ~" :
                   FRANCAIS ? "~ : La commande doit être de type STRING et non ~." :
                   ""
                  );
          }
        command = string_to_asciz(command); # in Simple-String umwandeln
        # Kommando ausführen:
        run_time_stop();
        begin_system_call();
       {var reg2 BOOL ergebnis = Execute(TheAsciz(command),Handle_NULL,Output_handle);
        end_system_call();
        run_time_restart();
        # Rückgabewert verwerten: ausgeführt -> T, nicht gefunden -> NIL :
        value1 = (ergebnis ? T : NIL); mv_count=1;
      }}
  }

#else # UNIX | MSDOS | VMS | ...

LISPFUN(shell,0,1,norest,nokey,0,NIL)
  { var reg1 object command = popSTACK();
    if (eq(command,unbound))
      #ifdef VMS
      { command = O(leer_string); } # system("") tut das Gewünschte
      #else
      { # (EXECUTE shell) ausführen:
        pushSTACK(O(user_shell)); # Shell-Name
        funcall(L(execute),1);
      }
      else
      #endif
      #if defined(MSDOS) || defined(VMS)
      # Dem DOS-Kommandointerpreter muß man das Kommando bereits entlang
      # der Leerstellen in einzelne Teile zerlegt übergeben. Die Funktion
      # system() erledigt uns das zum Glück.
      { command = string_to_asciz(command);
        begin_system_call();
        # Programm aufrufen:
       {var reg1 int ergebnis = system(TheAsciz(command));
        end_system_call();
        # Rückgabewert verwerten: =0 (OK) -> T, >0 (nicht OK) -> NIL :
        value1 = (ergebnis==0 ? T : NIL); mv_count=1;
      }}
      #else
      { # (EXECUTE shell "-c" command) ausführen:
        pushSTACK(O(user_shell)); # Shell-Name
        pushSTACK(O(user_shell_option)); # Shell-Option "-c"
        #if defined(MSDOS) && defined(EMUNIX)
        # Unter DOS 2.x, 3.x kann das Optionen-Zeichen ein anderes sein!
        if ((_osmode == DOS_MODE) && (_osmajor < 4))
          { var reg2 uintB swchar = _swchar();
            if (swchar) # evtl. "/C" durch etwas anderes ersetzen
              { TheSstring(STACK_0)->data[0] = swchar; } # (destruktiv)
          }
        #endif
        pushSTACK(command);
        funcall(L(execute),3);
      }
      #endif
  }

#endif

#endif

LISPFUNN(savemem,1)
# (SAVEMEM pathname) speichert ein Speicherabbild unter pathname ab.
  { # (OPEN pathname :direction :output) ausführen:
    # pathname als 1. Argument
    pushSTACK(S(Kdirection)); # :DIRECTION als 2. Argument
    pushSTACK(S(Koutput)); # :OUTPUT als 3. Argument
    funcall(L(open),3);
    # Speicherabbild in die Datei schreiben:
    # (Den Stream muß die Funktion savemem() schließen, auch im Fehlerfalle.)
    savemem(value1);
    value1 = T; mv_count=1; # 1 Wert T
  }

# ==============================================================================

#ifdef EMUNIX_PORTABEL

# Umgehen eines lästigen ENAMETOOLONG Errors bei Benutzung von langen
# Filenamen auf FAT-Drives unter OS/2:

#undef chdir
#undef access
#undef stat
#undef unlink
#undef rename
#undef __findfirst
#undef mkdir
#undef open
#undef creat
#undef spawnv

# path2 := verkürzte Kopie von path1
local void shorten_path (const char* path1, char* path2)
  { var reg1 const uintB* p1 = path1;
    var reg2 uintB* p2 = path2;
    var reg3 uintB c;
    var reg4 uintC wordlength = 0; # bisherige Länge in Name oder Typ
    var reg5 uintC maxwordlength = 8; # = 8 im Namen, = 3 im Typ
    loop
      { c = *p1++;
        if (c=='\0') { *p2++ = c; break; }
        if ((c=='\\') || (c=='/') || (c==':'))
          { *p2++ = c; wordlength = 0; maxwordlength = 8; }
        elif (c=='.')
          { *p2++ = c; wordlength = 0; maxwordlength = 3; }
        else
          { if (++wordlength <= maxwordlength) { *p2++ = c; } }
  }   }

global int my_chdir(path)
  var reg2 CONST char* path;
  { var reg1 int erg = chdir(path);
    if ((erg<0) && (errno==ENAMETOOLONG))
      { var reg3 char* shorter_path = alloca(asciz_length(path)+1);
        shorten_path(path,shorter_path);
        erg = chdir(shorter_path);
      }
    return erg;
  }

global int my_access(path,amode)
  var reg3 CONST char* path;
  var reg2 int amode;
  { var reg1 int erg = access(path,amode);
    if ((erg<0) && (errno==ENAMETOOLONG))
      { var reg4 char* shorter_path = alloca(asciz_length(path)+1);
        shorten_path(path,shorter_path);
        erg = access(shorter_path,amode);
      }
    return erg;
  }

global int my_stat(path,buf)
  var reg3 CONST char* path;
  var reg2 struct stat * buf;
  { var reg1 int erg = stat(path,buf);
    if ((erg<0) && (errno==ENAMETOOLONG))
      { var reg4 char* shorter_path = alloca(asciz_length(path)+1);
        shorten_path(path,shorter_path);
        erg = stat(shorter_path,buf);
      }
    return erg;
  }

global int my_unlink(path)
  var reg2 CONST char* path;
  { var reg1 int erg = unlink(path);
    if ((erg<0) && (errno==ENAMETOOLONG))
      { var reg3 char* shorter_path = alloca(asciz_length(path)+1);
        shorten_path(path,shorter_path);
        erg = unlink(shorter_path);
      }
    return erg;
  }

global int my_rename(oldpath,newpath)
  var reg3 CONST char* oldpath;
  var reg2 CONST char* newpath;
  { var reg1 int erg = rename(oldpath,newpath);
    if ((erg<0) && (errno==ENAMETOOLONG))
      { var reg4 char* shorter_oldpath = alloca(asciz_length(oldpath)+1);
        shorten_path(oldpath,shorter_oldpath);
        erg = rename(shorter_oldpath,newpath);
        if ((erg<0) && (errno==ENAMETOOLONG))
          { var reg5 char* shorter_newpath = alloca(asciz_length(newpath)+1);
            shorten_path(newpath,shorter_newpath);
            erg = rename(shorter_oldpath,shorter_newpath);
      }   }
    return erg;
  }

global int my___findfirst(path,attrib,ffblk)
  var reg4 const char* path;
  var reg2 int attrib;
  var reg3 struct ffblk * ffblk;
  { var reg1 int erg = __findfirst(path,attrib,ffblk);
    if ((erg<0) && (errno==ENAMETOOLONG))
      { var reg5 char* shorter_path = alloca(asciz_length(path)+1);
        shorten_path(path,shorter_path);
        erg = __findfirst(shorter_path,attrib,ffblk);
      }
    return erg;
  }

#ifdef EMUNIX_OLD_8e
  #define mkdir(path,attrib) (mkdir)(path)
#endif
global int my_mkdir(path,attrib)
  var reg2 CONST char* path;
  var reg3 long attrib;
  { var reg1 int erg = mkdir(path,attrib);
    if ((erg<0) && (errno==ENAMETOOLONG))
      { var reg4 char* shorter_path = alloca(asciz_length(path)+1);
        shorten_path(path,shorter_path);
        erg = mkdir(shorter_path,attrib);
      }
    return erg;
  }

global int my_open(path,flags)
  var reg3 CONST char* path;
  var reg2 int flags;
  { var reg1 int erg = open(path,flags);
    if ((erg<0) && (errno==ENAMETOOLONG))
      { var reg4 char* shorter_path = alloca(asciz_length(path)+1);
        shorten_path(path,shorter_path);
        erg = open(shorter_path,flags);
      }
    return erg;
  }

#define creat(path,mode)  open(path,O_RDWR|O_TRUNC|O_CREAT,mode)
global int my_creat(path,pmode)
  var reg3 CONST char* path;
  var reg2 int pmode;
  { var reg1 int erg = creat(path,pmode);
    if ((erg<0) && (errno==ENAMETOOLONG))
      { var reg4 char* shorter_path = alloca(asciz_length(path)+1);
        shorten_path(path,shorter_path);
        erg = creat(shorter_path,pmode);
      }
    return erg;
  }

global int my_spawnv(pmode,path,argv)
  var reg2 int pmode;
  var reg4 CONST char* path;
  var reg3 CONST char* CONST * argv;
  { var reg1 int erg = spawnv(pmode,path,argv);
    if ((erg<0) && (errno==ENAMETOOLONG))
      { var reg5 char* shorter_path = alloca(asciz_length(path)+1);
        shorten_path(path,shorter_path);
        erg = spawnv(pmode,shorter_path,argv);
      }
    return erg;
  }

#endif

# ==============================================================================

