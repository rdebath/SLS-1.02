# Include-File für ATARI-Version von CLISP
# Bruno Haible 4.3.1993


# Konstanten für Steuerzeichen:

#define BEL  7              # Ton ausgeben
#define RUBOUT BS           # Rubout = Backspace
#define CRLFstring  "\r\n"  # C-String, der CR/LF enthält

# Datenstrukturen für Betriebssystem-Aufrufe, aus ATARI_TURBO's TOS.H :
# zu GEMDOS:
  typedef struct          # used by Fsetdta, Fgetdta
    {
      BYTE  d_reserved[21];
      UBYTE d_attrib;
      UWORD d_time;
      UWORD d_date;
      ULONG d_length;
      BYTE  d_fname[14];
    } DTA;
  typedef struct { UWORD time; UWORD date; } DOSTIME;
  typedef struct          # used by Pexec
    {
      UBYTE length;
      BYTE  command_tail[127];
    } COMMAND;
  typedef struct baspag   # used by Pexec and Startup-Code
    {
      void* TpaStart;
      void* TpaEnd;
      void* TextSegStart;
      ULONG TextSegSize;
      void* DataSegStart;
      ULONG DataSegSize;
      void* BssSegStart;
      ULONG BssSegSize;
      DTA*  DtaPtr;
      struct baspag *ParentPrcPtr;
      LONG  Reserved0;
      BYTE* EnvStrPtr;
      BYTE  Reserved1[7];
      BYTE  CurDrv;
      LONG  Reserved2[18];
      COMMAND CmdLine;
    } BASEPAGE;
# zu BIOS:
  typedef struct          # used by Getbpb
    {
      WORD recsiz;
      WORD clsiz;
      WORD clsizb;
      WORD rdlen;
      WORD fsiz;
      WORD fatrec;
      WORD datrec;
      WORD numcl;
      WORD bflags;
    } BPB;
# zu XBIOS:
  typedef struct          # used by Keytbl
    {
      uintB* unshift;
      uintB* shift;
      uintB* capslock;
    } KEYTAB;
# zu LINEA/VDI/BIOS:
  typedef struct # ab Vdiesc = Linea - 0x38e
    { LONG  reserved6;         # reserviert
      void* cur_font;          # Zeiger auf Header des aktuellen Fonts
      WORD  reserved5[23];     # reserviert
      WORD  m_pos_hx;          # X-Koordinate des Maus-'Hot spot'
      WORD  m_pos_hy;          # Y-Koordinate des Maus-'Hot spot'
      WORD  m_planes;          # Zeichenmodus der Maus (Transparent oder XOR)
      WORD  m_cdb_bg;          # Maus-Hintergrundfarbe
      WORD  m_cdb_fg;          # Maus-Vordergrundfarbe
      WORD  mask_form[32];     # je 16 words Vordergrund und Maske
      WORD  inq_tab[45];       # wie vq_extnd()
      WORD  dev_tab[45];       # wie v_opnwk()
      WORD  gcurx;             # Aktuelle X-Position der Maus
      WORD  gcury;             # Aktuelle Y-Position der Maus
      WORD  m_hid_ct;          # Anzahl der erfolgten hide_mouse-Aufrufe
      WORD  mouse_bt;          # Aktueller Status der Mausknöpfe (Bit 0 links, Bit 1 rechts)
      WORD  req_col[3][16];    # Interne Daten für vq_color()
      WORD  siz_tab[15];       # wie v_opnwk()
      WORD  reserved4[2];      # reserviert
      void* cur_work;          # Zeiger auf Attributdaten der aktuellen virtuellen Workstation
      void* def_font;          # Zeiger auf den Standard-Systemzeichensatz
      void* font_ring[4];      # 3 Zeiger auf Zeichensatzlisten, verkettete FONT_HEADER-Strukturen
      WORD  font_count;        # Anzahl der Zeichensätze in der font_ring-Liste
      WORD  reserved3[45];     # reserviert
      BYTE  cur_ms_stat;       # Mausstatus (Bit 0 links, Bit 1 rechts, Bit 5 Flag ob Maus bewegt wurde)
      BYTE  reserved2;         # reserviert
      WORD  v_hid_cnt;         # Anzahl der erfolgten hide_cursor-Aufrufe
      WORD  cur_x;             # X-Position der Maus
      WORD  cur_y;             # Y-Position der Maus
      BYTE  cur_flag;          # != 0: Maus neu zeichnen
      BYTE  mouse_flag;        # != 0: Maus-Interrupt eingeschaltet
      LONG  reserved1;         # reserviert
      WORD  v_sav_xy[2];       # gerettete X-,Y-Koordinaten des Cursors
      WORD  save_len;          # Anzahl der gebufferten Bildschirmzeilen
      void* save_addr;         # Adresse des ersten gebufferten Bytes im Bildspeicher
      WORD  save_stat;         # Bit 0: Buffer gültig?, Bit 1: LONGs oder WORDs gebuffert
      LONG  save_area[4][16];  # Buffer für Bild unter Mauszeiger
      void(*user_tim)(void);   # aktueller Timer-Interrupt-Vektor
      void(*next_tim)(void);   # alter Timer-Interrupt-Vektor
      void(*user_but)(void);   # Maustasten-Vektor
      void(*user_cur)(void);   # Maus-Vektor
      void(*user_mot)(void);   # Mausbewegungs-Vektor
      WORD  v_cel_ht;          # Zeichenhöhe
      WORD  v_cel_mx;          # maximale Cursor-Spaltenposition
      WORD  v_cel_my;          # maximale Cursor-Zeilenposition
      WORD  v_cel_wr;          # Characterzeilenbreite in Bytes
      WORD  v_col_bg;          # Hintergrundfarbe
      WORD  v_col_fg;          # Vordergrundfarbe
      void* v_cur_ad;          # Adresse der aktuellen Cursorposition auf dem Bildschirm
      WORD  v_cur_off;         # Vertikaler Offset vom physikalischen Bildschirmanfang
      WORD  v_cur_xy[2];       # X-,Y-Position des Cursors
            #define v_cur_x  v_cur_xy[0]
            #define v_cur_y  v_cur_xy[1]
      BYTE  v_period;          # Blinkgeschwindigkeit des Cursors
      BYTE  v_cur_ct;          # Zähler fürs Blinken des Cursors
      void* v_fnt_ad;          # Zeiger auf Zeichensatzdaten des Systemzeichensatzes
      WORD  v_fnt_nd;          # größter ASCII-Wert im Zeichensatz
      WORD  v_fnt_st;          # kleinster ASCII-Wert im Zeichensatz
      WORD  v_fnt_wd;          # Breite des Fontimage in Bytes
      WORD  v_rez_hz;          # Bildschirmbreite in Pixel
      WORD* v_off_ad;          # Zeiger auf Font-Offset-Tabelle
      WORD  reserved;          # Cursorflag (nur beim alten TOS!), reserviert
      WORD  v_rez_vt;          # Bildschirmhöhe in Pixel
      WORD  bytes_lin;         # Bytes pro Pixelzeile
    } VDIESC;
  #define vdiesc  (((VDIESC*)linea)[-1])
#ifdef ATARI_TURBO
  # Allgemeine Betriebssystem-Aufrufe:
  extern LONG gemdos( void, ... );
  # GEMDOS-Funktionen, siehe TOS.H:
  extern void    Pterm0( void );
  extern LONG    Cconin( void );
  extern void    Cconout( WORD c );
  extern void    Cauxout( WORD c );
  extern WORD    Cprnout( WORD c );
  extern LONG    Crawcin( void );
  extern WORD    Cconws( BYTE *buf );
  extern WORD    Cconis( void );
  extern WORD    Dsetdrv( WORD drv );
  extern WORD    Cprnos( void );
  extern WORD    Cauxos( void );
  extern WORD    Dgetdrv( void );
  extern WORD    Dcreate( const char *path );
  extern WORD    Ddelete( const char *path );
  extern WORD    Dsetpath( const char *path );
  extern WORD    Fcreate( const char *filename, WORD attr );
  extern WORD    Fopen( const char *filename, WORD mode );
  extern WORD    Fclose( WORD handle );
  extern LONG    Fread( WORD handle, LONG count, void *buf );
  extern LONG    Fwrite( WORD handle, LONG count, void *buf );
  extern WORD    Frename( WORD zero, const char *oldname, const char *newname );
  extern WORD    Fdelete( const char *filename );
  extern LONG    Fseek( LONG offset, WORD handle, WORD seekmode );
  extern WORD    Fattrib( const char *filename, WORD wflag, WORD attrib);
  extern WORD    Dgetpath( char *path, WORD driveno );
  extern void    *Malloc( LONG number );
  extern WORD    Mfree( void *block );
  extern WORD    Mshrink( WORD zero, void *block, LONG newsiz );
  extern LONG    Pexec( WORD mode, char *ptr1, COMMAND *ptr2, void *ptr3 );
  extern UWORD   Tgettime( void );
  extern UWORD   Tgetdate( void );
  extern WORD    Fdatime( DOSTIME *timeptr, WORD handle, WORD wflag );
  extern void    Fsetdta( DTA *buf );
  extern WORD    Fsfirst( const char *filename, WORD attr );
  extern WORD    Fsnext( void );
  # BIOS-Funktionen, siehe TOS.H:
  extern void    Bconout( WORD dev, WORD c );
  extern LONG    Rwabs( WORD rwflag, void *buf, WORD cnt, WORD recnr, WORD dev );
  extern void    (*Setexc( WORD number, void (*exchdlr)() )) ();
  extern BPB     *Getbpb( WORD dev );
  extern LONG    Mediach( WORD dev );
  extern LONG    Drvmap( void );
  extern LONG    Kbshift( WORD mode );
  # XBIOS-Funktionen, siehe TOS.H:
  extern KEYTAB* Keytbl( uintB* unshift, uintB* shift, uintB* capslock );
  extern LONG    Random( void );
  extern ULONG   Gettime( void );
  extern WORD    Setprt( WORD config );
  extern void    Supexec( void (*addr)() );
  # Definition der LINEA-Funktionen, siehe LINEA.H:
  extern void    linea_init( void );
  extern void    show_mouse( int flag );
  extern void    hide_mouse( void );
  #define linea  Linea
  extern void* linea;
#endif
#ifdef GNU
  #include <osbind.h>
  #include <linea.h>
  # Hier haben die LineA-Funktionen andere Namen:
  #define linea_init()  linea0()
  #define show_mouse(flag)  (INTIN[0] = (flag), linea9())
  #define hide_mouse()  lineaa()
  #define linea  __aline
#endif


# Typ von argc in der Deklaration von main():
  #ifdef GNU               # nur bei Benutzung von Bammis Runtime-System
    #define argc_t  sintL  # da crt0.o ohne '-mshort' compiliert ist
  #endif

# Environment-Variablen abfragen:
  extern char* getenv (char* name); # siehe GETENV(3V)
# wird verwendet von Runtime-Library (crt0.o, main.o)
  #define getenv  my_getenv
  extern const char * getenv (const char* name); # siehe GETENV(3V)
# wird verwendet von MISC

# Programm verlassen und beenden:
# GEMDOS_exit()
  #define GEMDOS_exit()  Pterm0()
# wird verwendet von SPVW

# Auf Tastendruck warten.
# GEMDOS_ConIn()
# < ULONG ergebnis
# < (UBYTE)ergebnis = Ascii-Code der gedrückten Taste
# < (UBYTE)(ergebnis>>16) = Scan-Code der gedrückten Taste
#             (wichtig z.B. für Funktionstasten, die den Ascii-Code 0 geben)
# Der Ascii-Code wird auch auf den Bildschirm ausgegeben.
  #define GEMDOS_ConIn()  Cconin()
# wird verwendet von SPVW

# Zeichen auf Bildschirm ausgeben.
#   void GEMDOS_ConOut(code)
# > UWORD code : Ascii-Code (>=0,<256) des auszugebenden Zeichens
  #define GEMDOS_ConOut(code)  Cconout(code)
# wird verwendet von SPVW
# Control-Zeichen werden so interpretiert wie bei einer VT52-Emulation:
# BEL           kurzen Ton ausgeben
# TAB           ?
# 10=LF,11,12   Zeilenvorschub ?
# CR            Cursor an den Zeilenanfang
# ESC A         Cursor 1 Zeile hoch (nichts, falls er ganz oben war)
# ESC B         Cursor 1 Zeile runter (nichts, falls er ganz unten war)
# ESC C         Cursor 1 Zeichen nach rechts
# BS oder
# ESC D         Cursor 1 Zeichen nach links (nichts, falls er ganz links war)
# ESC E         (Clear Home) Bildschirm löschen und Cursor an die linke obere
#                Ecke
# ESC H         (Home) Cursor an die linke obere Ecke
# ESC I         Cursor 1 Zeichen nach oben (falls er ganz oben war,
#               alles um 1 Zeile nach unten scrollen und Leerzeile einfügen)
# ESC J         Bildschirminhalt ab Cursorposition löschen
# ESC K         Zeileninhalt ab Cursorposition löschen
# ESC L         Leerzeile an der momentanen Cursorposition einfügen, Rest
#               des Bildschirms um 1 Zeile nach unten scrollen, Cursor an
#               den Anfang der neuen Leerzeile
# ESC M         Zeile löschen: Cursor ganz nach links, diese Zeile löschen
#               und Rest des Bildschirms um 1 Zeile nach oben schieben
# ESC Y chr(32+l) chr(32+c)
#               Cursor positionieren an Zeile l (0<=l<25), Spalte c (0<=c<80)
# ESC b chr(f)  Schriftfarbe wählen, Farbe (f mod 16) (0=weiß, 1=schwarz)
# ESC c chr(f)  Hintergrundfarbe wählen, Farbe (f mod 16) (0=weiß, 1=schwarz)
# ESC d         Bildschirm bis zur Cursorposition (einschließlich!) löschen
# ESC e         Cursor einschalten (sichtbar machen)
# ESC f         Cursor ausschalten (unsichtbar machen)
# ESC j         Cursorposition speichern
# ESC k         Cursor auf gespeicherte Position setzen
# ESC l         Zeile löschen: Cursor ganz nach links, diese Zeile löschen
# ESC o         Zeile bis zur Cursorposition (einschließlich!) löschen
# ESC p         Reverse on: ab jetzt schreibe mit Hintergrundfarbe auf einem
#               Hintergrund, der die Schriftfarbe hat
# ESC q         Reverse off
# ESC v         Ab jetzt bei Zeilenüberlauf automatisch in die nächste Zeile
# ESC w         Ab jetzt bei Zeilenüberlauf jedes Zeichen in die Spalte ganz
#               rechts schreiben.

# Gibt einen Piepser aus.
# GEMDOS_Bell()
  #define GEMDOS_Bell()  GEMDOS_ConOut(BEL)
# wird verwendet von

# Auf Tastendruck warten.
# GEMDOS_DirConIn()
# < ULONG ergebnis
# < (UBYTE)ergebnis = Ascii-Code der gedrückten Taste
# < (UBYTE)(ergebnis>>16) = Scan-Code der gedrückten Taste
# < (UBYTE)(ergebnis>>24) = normalerweise: 0,
#              Bit 3,0x484 gesetzt: Sondertasten-Status (wie bei BIOS_KBSHIFT)
# Auf den Bildschirm wird nichts ausgegeben.
  #define GEMDOS_DirConIn()  Crawcin()
# wird verwendet von STREAM
# Ascii-Codes (dezimal):
#   großer Tastaturblock: bekannt
#   Pfeiltastenblock:
#     ohne Shift: stets 0
#     mit Shift: Insert='0',='8',='4',='2',='6', sonst 0
#   Ziffernblock:
#     ohne Ctrl: bekannt
#     mit Ctrl:
#       obere Zeile     (=8,)=9,/=15,*=10
#       nächste Zeile   7=23,8=24,9=25,-=31
#       nächste Zeile   4=20,5=21,6=30,+=11
#       nächste Zeile   1=17,2=0,3=19,Enter=10
#       untere Zeile    0=16,.=14,Enter=10
#   Funktionstasten: stets 0
# Scan-Codes (dezimal):
#   großer Tastaturblock:
#     obere Zeile    1,2,3,4,5,6,7,8,9,10,11,12,13,41,Backspace=14
#     nächste Zeile  15,16,17,18,19,20,21,22,23,24,25,26,27,Return=28,Delete=83
#     nächste Zeile  Control,30,31,32,33,34,35,36,37,38,39,40,Return=28,~=43
#     untere Zeile   Shift,96,44,45,46,47,48,49,50,51,52,53,Shift
#     Space-Zeile    Alternate,Space=57,CapsLock
#     obere Zeile mit Alt  1,120,121,122,123,124,125,126,127,128,129,130,131,41,...
#   Pfeiltastenblock (liefert meist Ascii-Code 0):
#     obere Zeile     Help=98, Undo=97
#     mittlere Zeile  Insert=82, =72, ClrHome=71, Ctrl-ClrHome=119
#     untere Zeile    =75, Ctrl-=115, =80, =77, Ctrl-=116
#   Ziffernblock:
#     obere Zeile     (=99,)=100,/=101,*=102
#     nächste Zeile   7=103,8=104,9=105,-=74
#     nächste Zeile   4=106,5=107,6=108,+=78
#     nächste Zeile   1=109,2=110,3=111,Enter=114
#     untere Zeile    0=112,.=113,Enter=114
#   Funktionstasten (liefern Ascii-Code 0):
#     normal: F1=59, ..., F10=68
#     mit Shift: F1=84, ..., F10=93

# String auf Bildschirm ausgeben.
# GEMDOS_PrintLine(string)
# > char* string : Anfangsadresse des auszugebenden ASCIZ-Strings
 #ifndef ATARI_TURBO
  #define GEMDOS_PrintLine(string)  Cconws(string)
 #else # Damit das Argument auch ein Präprozessor-abhängiger String sein kann:
       # (TURBO-C auf dem ATARI verkraftet keine Präprozessor-Anweisungen
       #  innerhalb von Macro-Argumenten!)
  #define GEMDOS_PrintLine  Cconws
 #endif
# wird verwendet von SPVW, DEBUG

# Abfragen, ob GEMDOS-Tastaturbuffer (max. 64 Tastendrücke) leer ist.
# GEMDOS_ConStat()
# < UWORD ergebnis : falls =0, ist der Tastaturbuffer leer.
  #define GEMDOS_ConStat()  Cconis()
# wird verwendet von STREAM

# Druckerstatus abfragen.
# GEMDOS_PrtStat()  bei paralleler Schnittstelle
# GEMDOS_AuxStat()  bei serieller Schnittstelle
# < WORD ergebnis : 0x0000 falls nicht bereit, 0xFFFF falls bereit
  #define GEMDOS_PrtStat()  Cprnos()
  #define GEMDOS_AuxStat()  Cauxos()
# wird verwendet von STREAM

# Zeichen auf Drucker ausgeben.
# GEMDOS_PrtOut(ch)  bei paralleler Schnittstelle
# GEMDOS_AuxOut(ch)  bei serieller Schnittstelle
# > UWORD ch : auszugebendes Zeichen
  #define GEMDOS_PrtOut(ch)  Cprnout(ch)
  #define GEMDOS_AuxOut(ch)  Cauxout(ch)
# wird verwendet von STREAM

# Aktuelles Laufwerk festlegen.
# GEMDOS_SetDrv(drive)
# > UWORD drive : Nummer des Laufwerks (0=A, 1=B, ...), das aktuell werden soll
  #define GEMDOS_SetDrv(drive)  Dsetdrv(drive)
# wird verwendet von

# Aktuelles Laufwerk abfragen.
# GEMDOS_CurrentDisk()
# < UWORD ergebnis : Nummer des aktuellen Laufwerks (0=A, 1=B, ...)
  #define GEMDOS_CurrentDisk()  Dgetdrv()
# wird verwendet von PATHNAME

# Neues Subdirectory anlegen.
# GEMDOS_mkdir(name)
# > char* name : Adresse eines ASCIZ-Strings, der den Pfadnamen des neu
#                anzulegenden Subdirectories (ohne '\' am Schluß) enthält
# < WORD ergebnis : Falls negativ, Fehlernummer.
  #define GEMDOS_mkdir(name)  Dcreate(name)
# wird verwendet von PATHNAME

# Subdirectory löschen
# GEMDOS_rmdir(name)
# > char* name : Adresse eines ASCIZ-Strings, der den Pfadnamen des zu löschenden
#                Subdirectories (ohne '\' am Schluß) enthält
# < WORD ergebnis : Falls negativ, Fehlernummer.
  #define GEMDOS_rmdir(name)  Ddelete(name)
# wird verwendet von PATHNAME

# Aktuellen Pfad ändern.
# GEMDOS_chdir(name)
# > char* name : Adresse eines ASCIZ-Strings, der den Pfadnamen des angewählten
#                Subdirectories (ohne '\' am Schluß) enthält
# < WORD ergebnis : Falls negativ, Fehlernummer.
  #define GEMDOS_chdir(name)  Dsetpath(name)
# wird verwendet von

# Attribute einer Datei abfragen.
# GEMDOS_access(name)
# > char* name : Adresse eines ASCIZ-Strings mit dem Filenamen
# < WORD ergebnis : Falls negativ, Fehlernummer. Sonst Attribute des Files.
  #define GEMDOS_access(name)  Fattrib(path,0,0)
# wird verwendet von

# Typ einer Handle-Nummer:
  #define Handle  uintW
# wird verwendet von PATHNAME, SPVW

# Datei anlegen.
# GEMDOS_create(name,attribs)
# > char* name : Adresse eines ASCIZ-Strings mit dem Filenamen
# > UWORD attribs : Attribute (0 für normale Datei)
# < WORD ergebnis : Falls negativ, Fehlernummer. Sonst eine Handle-Nummer.
  #define GEMDOS_create(attribs,name)  Fcreate(attribs,name)
# wird verwendet von PATHNAME

# Datei öffnen.
# GEMDOS_open(name,mode)
# > char* name : Adresse eines ASCIZ-Strings mit dem Filenamen
# > UWORD mode : Zugriffs-Modus (0 für Read, 1 für Write, 2 für Read/Write)
# < WORD ergebnis : Falls negativ, Fehlernummer. Sonst eine Handle-Nummer.
  #define GEMDOS_open(name,mode)  Fopen(name,mode)
# wird verwendet von SPVW, STREAM, PATHNAME
  #define GEMDOS_open_NotFound -33  # Fehlernummer, wenn Datei nicht gefunden

# Datei schließen.
# GEMDOS_close(handle)
# > UWORD handle : Handle-Nummer eines (offenen) Files.
# < WORD ergebnis : Falls negativ, Fehlernummer. Sonst 0.
  #define GEMDOS_close(handle)  Fclose(handle)
# wird verwendet von SPVW, STREAM, PATHNAME
  #define GEMDOS_close_DiskChange -65  # Fehlernummer, wenn Diskette gewechselt wurde
  #define GEMDOS_close_BadHandle  -37  # Fehlernummer, wenn ungültiges Handle

# Lesen.
# GEMDOS_read(handle,bufsize,buf)
# > UWORD handle : Handle (eines offenen Files oder 0 für Console, 2 für RS232)
# > ULONG bufsize : Anzahl der zu lesenden Bytes (= Puffergröße)
# > char* buf : Adresse (24 Bit) eines Puffers, in den die gelesenen Bytes
#               kommen sollen
# < LONG ergebnis : Falls negativ, Fehlernummer. Sonst Anzahl der gelesenen Bytes.
# < Puffer buf : gefüllt mit den gelesenen Bytes
# < (ergebnis==bufsize), falls alle angeforderten Bytes korrekt gelesen
  #define GEMDOS_read(handle,bufsize,buf)  Fread(handle,bufsize,buf)
# wird verwendet von SPVW, STREAM

# Schreiben.
# GEMDOS_write(handle,bufsize,buf)
# > UWORD handle : Handle (eines offenen Files oder 1 für Console,
#                  2 für serielle Schnittstelle, 3 für parallele Schnittstelle)
# > ULONG bufsize : Anzahl der zu schreibenden Bytes (= Puffergröße)
# > char* buf : Adresse (24 Bit) eines Puffers, in dem die zu schreibenden
#               Bytes stehen
# < LONG ergebnis : Falls negativ, Fehlernummer. Sonst Anzahl der geschriebenen Bytes.
# < (ergebnis==bufsize), falls alle Bytes korrekt geschrieben
  #define GEMDOS_write(handle,bufsize,buf)  Fwrite(handle,bufsize,buf)
# wird verwendet von SPVW, STREAM

# Datei umbenennen.
# GEMDOS_rename(oldname,newname)
# > char* oldname : Adresse eines ASCIZ-Strings mit dem alten Filenamen
# > char* newname : Adresse eines ASCIZ-Strings mit dem neuen Filenamen
# < WORD ergebnis : Falls negativ, Fehlernummer.
  #define GEMDOS_rename(oldname,newname)  Frename(0,oldname,newname)
# wird verwendet von PATHNAME
#define GEMDOS_rename_exists -36 # Fehlernummer, wenn Datei mit neuem Namen existiert

# Datei löschen.
# GEMDOS_unlink(name)
# > char* name : Adresse eines ASCIZ-Strings mit dem Filenamen
# < WORD ergebnis : Falls negativ, Fehlernummer. Sonst 0.
  #define GEMDOS_unlink(name)  Fdelete(name)
# wird verwendet von PATHNAME

# Dateizeiger positionieren.
# GEMDOS_Lseek(pos,handle,mode)
# > LONG pos : Position in Bytes (muß >=0 bei Modus 0 bzw. <=0 bei Modus 2 sein)
# > UWORD handle : Handle-Nummer eines (offenen) Files.
# > UWORD mode : Modus (0 = ab Fileanfang, 1 = ab momentan, 2 = ab Fileende)
# < LONG ergebnis : Falls negativ, Fehlernummer. Sonst neue Position ab Fileanfang.
  #define GEMDOS_Lseek(pos,handle,mode)  Fseek(pos,handle,mode)
# wird verwendet von STREAM

# Aktuellen Pfad ermitteln.
# GEMDOS_GetDir(buf,drive)
# > char buf[64] : Adresse eines 64 Bytes großen Puffers
# > UWORD drive : Nummer des Laufwerks (0=aktuelles, 1=A, 2=B, ... !)
# < WORD ergebnis : Falls negativ, Fehlernummer.
# < Puffer buf : enthält (als ASCIZ-String) das aktuelle Subdirectory im
#                angegebenen Laufwerk; Leerstring bedeutet Hauptdirectory.
  #define GEMDOS_GetDir(buf,drive)  Dgetpath(buf,drive)
# wird verwendet von PATHNAME

# Speicherplatz reservieren.
# GEMDOS_Malloc(size)
# > ULONG size : Anzahl der gewünschten Bytes (eine gerade Zahl)
# < LONG ergebnis : Falls negativ, Fehlernummer.
#                   Sonst Anfangsadresse des zugeteilten Speicherbereichs.
  #define GEMDOS_Malloc(size)  Malloc(size)
# wird verwendet von SPVW

# freien Speicherplatz erfragen.
# GEMDOS_FreeMem()
# < ULONG ergebnis : Anzahl der freien Bytes Speicher
  #define GEMDOS_FreeMem()  GEMDOS_Malloc(-1)
# wird verwendet von SPVW

# Reservierten Speicherplatz freigeben.
# GEMDOS_Mfree(block)
# > ULONG block : Anfangsadresse des freizugebenden Speicherbereichs
# < WORD ergebnis : Falls negativ, Fehlernummer.
  #define GEMDOS_Mfree(block)  Mfree(block)
# wird verwendet von SPVW

# Speicherplatz reservieren und zurückgeben.
# GEMDOS_SetBlock(block,size)
# Beansprucht einen vorgegebenen Speicherbereich und gibt alle
# darüberliegenden Speicherbereiche ans System zurück.
# > ULONG block : Anfangsadresse des zu reservierenden Speicherbereiches
# > ULONG size : Größe des zu reservierenden Speicherbereiches (in Bytes)
# < LONG ergebnis : Falls negativ, Fehlernummer.
  #ifdef ATARI_TURBO
    #define GEMDOS_SetBlock(block,size)  Mshrink(0,block,size)
    # NB: der Ergebnistyp ist in TURBO-Cs TOS.H als 'int' deklariert,
    #     ist aber ein LONG.
  #endif
  #ifdef GNU
    #define GEMDOS_SetBlock(block,size)  Mshrink(block,size)
  #endif
# wird verwendet von SPVW

# Lädt ein File.
# GEMDOS_exec_3(name,tail,env)
# > char* name : Adresse eines ASCIZ-Strings mit dem Filenamen
# > struct {BYTE len; char str[];} * tail :
#                     Aufrufparameter (String mit <=127 Zeichen, Länge voran)
# > char* env : Environment (Folge von ASCIZ-Strings, durch '\0' abgeschlossen)
# < WORD ergebnis : Falls negativ, Fehlernummer.
#                   Sonst Adresse der Base-Page des geladenen Programms.
  #define GEMDOS_exec_3(name,tail,env)  Pexec(3,name,tail,env)
# wird verwendet von

# Führt ein bereits geladenes Programm aus.
# GEMDOS_exec_4(dummy1,prog,dummy2)
# > ULONG dummy1 : Dummy (z.B. ASCIZ-Leerstring)                (?)
# > ULONG prog : Adresse der Base-Page des geladenen Programms  (?)
# > ULONG dummy2 : Dummy (z.B. ASCIZ-Leerstring)
# < UWORD ergebnis : Exitcode nach Abschluß des Programms
  #define GEMDOS_exec_4(dummy1,prog,dummy2)  Pexec(4,dummy1,prog,dummy2)
# wird verwendet von

# Lädt und startet ein File.
# GEMDOS_exec_0(name,tail,env)
# > char* name : Adresse eines ASCIZ-Strings mit dem Filenamen
# > struct {BYTE len; char str[];} * tail :
#                     Aufrufparameter (String mit <=127 Zeichen, Länge voran)
# > char* env : Environment (Folge von ASCIZ-Strings, durch '\0' abgeschlossen)
# < LONG ergebnis : Falls negativ, Fehlernummer.
#                   Sonst Returncode des aufgerufenen Programms.
  #define GEMDOS_exec_0(name,tail,env)  Pexec(0,name,tail,env)
# wird verwendet von SPVW

# Liefert die aktuelle Uhrzeit.
# GEMDOS_GetTime()
# < UWORD ergebnis : Bits 15..11: Stunde in {0,...,23},
#                    Bits 10..5:  Minute in {0,...,59},
#                    Bits 4..0:   Sekunde/2 in {0,...,29}.
  #define GEMDOS_GetTime()  (UWORD)Tgettime()
# wird verwendet von SPVW, LISPARIT

# Liefert das aktuelle Datum.
# GEMDOS_GetDate()
# < UWORD ergebnis : Bits 15..9: Jahr-1980 in {0,...,119},
#                    Bits 8..5:  Monat in {1,...,12},
#                    Bits 4..0:  Tag in {1,...,31}.
  #define GEMDOS_GetDate()  (UWORD)Tgetdate()
# wird verwendet von SPVW, LISPARIT

# Liefert Datum und Uhrzeit einer offenen Datei.
# GEMDOS_GSDTOF(buf,handle)
# > struct {UWORD time; UWORD date;} * buf
#   Adresse eines 4-Byte-Buffers, wo Uhrzeit und Datum abgelegt werden sollen.
# > UWORD handle : Handle-Nummer eines (offenen) Files.
# < WORD ergebnis : Falls negativ, Fehlernummer.
  #define GEMDOS_GSDTOF(buf,handle)  Fdatime(buf,handle,0)
# wird verwendet von PATHNAME


# Durchsuchen von Directories nach Dateien:

# Dies geschieht etwa so:
#     GEMDOS_SET_DTA
#     GEMDOS_SFIRST
#     while found do {DTA verwenden, GEMDOS_SNEXT}.
# Dabei muß dem GEMDOS mit GEMDOS_SET_DTA die Adresse eines 44-Byte-Buffers
# übergeben werden, in den die Daten der einzelnen Files gelesen werden.
# Sein Aufbau:
# 0-20 : reserviert
# 21 : File-Attribut
#      (Bit 0: Write-Protected, Bit 1: Hidden file, Bit 2: System file,
#       Bit 3: Volume-label, Bit 4: Subdirectory, Bit 5: korrekt geschlossen)
# 22-23 : Uhrzeit der Dateierstellung
#          Als Word: Bits 15..11: Stunde in {0,...,23},
#                    Bits 10..5:  Minute in {0,...,59},
#                    Bits 4..0:   Sekunde/2 in {0,...,29}.
# 24-25 : Datum der Dateierstellung
#          Als Word: Bits 15..9: Jahr-1980 in {0,...,119},
#                    Bits 8..5:  Monat in {1,...,12},
#                    Bits 4..0:  Tag in {1,...,31}.
# 26-29 als Longword: Größe der Datei (in Bytes)
# 30-43 (max. 13 Bytes)  : Filename der Datei als ASCIZ-String

# Disk Transfer Address setzen.
# GEMDOS_SetDTA(address)
# > UBYTE address[44] : gerade Adresse eines 44-Byte-Buffers, den GEMDOS als DTA
#                       verwenden soll
  #define GEMDOS_SetDTA(address)  Fsetdta(address)
# wird verwendet von PATHNAME

# Dateinamen im Directory suchen.
# GEMDOS_Sfirst(name,mask)
# > char* name : Adresse eines ASCIZ-Strings, der den Filename (Pfadname und
#                Wildcards wie '*.*' zugelassen) enthält.
# > UWORD mask : Attributmaske, welche Dateien gewünscht sind
#                (0 um nur normale Dateien zu durchsuchen)
# < WORD ergebnis : Falls negativ, Fehlermeldung. Falls =-33, keine Datei gefunden.
#                   Sonst Daten der gefundenen Datei in DTA.
  #define GEMDOS_Sfirst(name,mask)  Fsfirst(name,mask)
  #define GEMDOS_Sfirst_notfound -33 # Fehlernummer, wenn Datei nicht gefunden
# wird verwendet von PATHNAME

# Dateien weitersuchen.
# GEMDOS_Snext()
# < WORD ergebnis : Falls negativ, Fehlermeldung.
#                   Falls =-49, keine weitere Datei gefunden.
#                   Sonst Daten der gefundenen Datei in DTA.
  #define GEMDOS_Snext()  Fsnext()
# wird verwendet von SPVW, PATHNAME
#define GEMDOS_Snext_notfound -49 # Fehlernummer, wenn keine weitere Datei gefunden


# Zeichen auf Bildschirm ausgeben.
# BIOS_ConOut(ch)
# > UWORD ch : Ascii-Code (>=0,<256) des auszugebenden Zeichens
  #define BIOS_ConOut(ch)  Bconout(2,ch)
                           # 2 = Handle für Bildschirm mit Steuerzeichen
# wird verwendet von SPVW, STREAM
# Control-Zeichen werden interpretiert wie bei GEMDOS_ConOut().
# Im Unterschied zu diesem werden Control-S/Q und Control-C nicht abgefangen.

# Zeichen auf Bildschirm ausgeben.
# BIOS_GrConOut(ch)
# > UWORD ch : Ascii-Code (>=0,<256) des auszugebenden Zeichens
  #define BIOS_GrConOut(ch)  Bconout(5,ch)
                             # 5 = Handle für Bildschirm ohne Steuerzeichen
# wird verwendet von STREAM
# Control-Zeichen werden nicht interpretiert!

# Gibt einen Piepser aus.
# BIOS_Bell()
  #define BIOS_Bell()  BIOS_ConOut(BEL)
# wird verwendet von SPVW, STREAM

# Sectoren lesen.
# BIOS_ReadAbs(buf,nsectors,sector,drive)
# > BYTE* buf : Adresse (24 Bit) eines Puffers, in den die Daten kommen sollen
# > UWORD nsectors : Anzahl der zu lesenden Sectoren
# > UWORD sector : logische Sectornummer des ersten zu lesenden Sectors
# > UWORD drive : Laufwerksnummer (0=A, 1=B, ...)
# < LONG ergebnis : Falls negativ, Fehlernummer.
  #define BIOS_ReadAbs(buf,nsectors,sector,drive) \
          Rwabs(0,buf,nsectors,sector,drive)
# wird verwendet von PATHNAME

# Exception-Vektor holen.
# BIOS_GetException(nummer)
# > UWORD nummer : Exception-Vektor-Nummer
# < ULONG ergebnis : Wert des Exception-Vektor nummer (bei Adresse 4*nummer)
  #define BIOS_GetException(nummer)  Setexc(nummer,-1)
# wird verwendet von SPVW

# Exception-Vektor setzen.
# BIOS_SetException(nummer,wert)
# > UWORD nummer : Exception-Vektor-Nummer
# > ULONG wert : Adresse als neuer Wert des Exception-Vektors nummer
# < ULONG ergebnis : alter Wert des Exception-Vektors nummer (bei Adresse 4*nummer)
  #define BIOS_SetException(nummer,wert)  Setexc(nummer,wert)
# wird verwendet von SPVW

# Diskettenparameter abfragen.
# BIOS_GetBPB(drive)
# > UWORD drive : Laufwerksnummer (0=A, 1=B, ...)
# < ULONG ergebnis : falls =0, Fehler.
#                    falls /=0, Adresse des Bios-Parameter-Blocks der Diskette:
#                       .W Sectorgröße in Bytes
#                       .W Clustergröße in Sectoren
#                       .W Clustergröße in Bytes
#                       .W Directorylänge in Sectoren
#                       .W FAT-Größe in Sectoren
#                       .W Sectornummer der zweiten FAT
#                       .W Sectornummer des ersten Datenclusters
#                       .W Anzahl der Datencluster auf der Diskette
#                       .W Diverse Flags
  #define BIOS_GetBPB(drive)  Getbpb(drive)
# wird verwendet von PATHNAME

# Diskettenwechsel abfragen.
# BIOS_Mediach(drive)
# > UWORD drive : Laufwerksnummer (0=A, 1=B)
# < UWORD ergebnis : Diskettenwechsel? 0=nein, 1=vielleicht, 2=ja.
  #define BIOS_Mediach(drive)  Mediach(drive)
# wird verwendet von

# Liefert eine Tabelle aller existenten Laufwerke.
# BIOS_DriveMap()
# < uint16 ergebnis : Bitvektor.
#          Darin ist Bit n gesetzt, wenn Laufwerk mit Nummer n
#          (0=A, 1=B, ...) existiert.
  #define BIOS_DriveMap()  Drvmap()
# wird verwendet von STREAM

# Sondertasten-Status abfragen.
# BIOS_KbShift()
# < uint8 ergebnis:
#         Bit 0: rechte Shift-Taste gedrückt
#         Bit 1: linke Shift-Taste gedrückt
#         Bit 2: Ctrl-Taste gedrückt
#         Bit 3: Alt-Taste gedrückt
#         Bit 4: Caps-Lock eingeschaltet
#         Bit 5: rechte Maustaste, Clr/Home
#         Bit 6: linke Maustaste, Insert
  #define BIOS_KbShift()  Kbshift(-1)
# wird verwendet von SPVW
  #define RShiftKey_mask   bit(0)
  #define LShiftKey_mask   bit(1)
  #define CtrlKey_mask     bit(2)
  #define AltKey_mask      bit(3)
  #define CapsLockKey_mask bit(4)
  #define RMouseKey_mask   bit(5)
  #define LMouseKey_mask   bit(6)
  #define BothShiftKey_mask  (LShiftKey_mask|RShiftKey_mask)
  #define BothMouseKey_mask  (LMouseKey_mask|RMouseKey_mask)

# Sondertasten-Status setzen.
# BIOS_SetKbShift(status)
# > uint16 status :
#         Bit 0: rechte Shift-Taste gedrückt
#         Bit 1: linke Shift-Taste gedrückt
#         Bit 2: Ctrl-Taste gedrückt
#         Bit 3: Alt-Taste gedrückt
#         Bit 4: Caps-Lock eingeschaltet
#         Bit 5: rechte Maustaste, Clr/Home
#         Bit 6: linke Maustaste, Insert
  #define BIOS_SetKbShift(status)  Kbshift(status)
# wird verwendet von

# Liefert die Adresse der Tastaturtabellenzeigertabelle.
# XBIOS_GetKeyTbl()
# < KEYTAB* ergebnis:
#           ergebnis->unshift, ergebnis->shift, ergebnis->capslock
#           sind Zeiger auf die drei Tastaturtabellen (Länge 128,
#           Index: Scancode, Wert: Ascii-Code)
  #define XBIOS_GetKeyTbl()  Keytbl((uintB*)(-1),(uintB*)(-1),(uintB*)(-1))
# wird verwendet von STREAM

# Liefert eine Zufallszahl.
# XBIOS_Random()
# < uint32 ergebnis : Bits 31..24 =0, Bits 23..0 zufällig
  #define XBIOS_Random()  Random()
# wird verwendet von LISPARIT

# Liefert das aktuelle Datum und die aktuelle Uhrzeit.
# XBIOS_GetDateTime()
# < uint32 ergebnis :
#          Bits 31..25: Jahr-1980 in {0,...,119},
#          Bits 24..21: Monat in {1,...,12},
#          Bits 20..16: Tag in {1,...,31},
#          Bits 15..11: Stunde in {0,...,23},
#          Bits 10..5:  Minute in {0,...,59},
#          Bits 4..0:   Sekunde/2 in {0,...,29}.
  #define XBIOS_GetDateTime()  Gettime()
# wird verwendet von

# Liefert die Druckerkonfiguration
# XBIOS_GetPrtConfig()
# < uint16 ergebnis :
#          Bit 0: 0=Matrixdrucker, 1=Typenraddrucker
#          Bit 1: 0=Farbdrucker, 1=Monochromdrucker
#          Bit 2: 0=Atari-Drucker, 1=Epson-Drucker
#          Bit 3: 0=Test-Modus, 1=Qualitätsmodus
#          Bit 4: 0=Centronics-Port, 1=RS232-Port
#          Bit 5: 0=Endlospapier, 1=Einzelblatt
  #define XBIOS_GetPrtConfig()  Setprt(-1)
# wird verwendet von STREAM

# Führt ein Programmstück im Supervisor-Modus aus.
# Supervisor_Exec(fun)
# void fun(void) : Adresse des auszuführenden Programmstücks
  #define Supervisor_Exec(fun)  Supexec(&fun)
# wird verwendet von SPVW, PATHNAME

# Initialisiert die Line-A Routinen.
# LineA_Init();
  #define LineA_Init()  linea_init()
# wird verwendet von SPVW

# Schaltet die Maus ein. (Show Mouse)
# LineA_MouseOn();
  #define LineA_MouseOn()  show_mouse(0)
# wird verwendet von

# Schaltet die Maus aus. (Hide Mouse)
# LineA_MouseHide();
  #define LineA_MouseHide()  hide_mouse()
# wird verwendet von SPVW

# Schaltet die Maus wieder ein, falls sie vor dem letzten 'Hide Mouse'
# eingeschaltet war. (Unhide Mouse)
# LineA_MouseUnhide();
  #define LineA_MouseUnhide()  show_mouse(1)
# wird verwendet von SPVW

# Liefert den Status der Maustasten.
# LineA_MouseButtons()
# < ergebnis: Bit 0 gesetzt, wenn die linke Maustaste gedrückt ist,
#             Bit 1 gesetzt, wenn die rechte Maustaste gedrückt ist.
  #define LineA_MouseButtons()  (*(volatile WORD *)(&vdiesc.mouse_bt))
# wird verwendet von STREAM, Macro interruptp

# Liefert den Status der Maus.
# LineA_MouseStatus()
# < ergebnis: Bit 0 gesetzt, wenn die linke Maustaste gedrückt ist,
#             Bit 1 gesetzt, wenn die rechte Maustaste gedrückt ist,
#             Bit 5 gesetzt, wenn die Maus bewegt wurde.
  #define LineA_MouseStatus()  (*(volatile BYTE *)(&vdiesc.cur_ms_stat))
# wird verwendet von STREAM

# Sofortiger Programmabbruch, Sprung in den Debugger
  #ifdef GNU
    #define abort()  __asm__ __volatile__ (" .word 0x4AFC ") # illegaler Befehl
  #else
    extern void abort (void); # siehe STDLIB.H
  #endif
# wird verwendet von DEBUG, EVAL, IO

