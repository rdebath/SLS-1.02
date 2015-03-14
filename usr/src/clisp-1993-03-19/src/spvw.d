# Speicherverwaltung für CLISP
# Bruno Haible 19.3.1993

# Inhalt:
# Zeitmessungsfunktionen
# Speichergröße
# Speicherlängenbestimmung
# Garbage Collection
# Speicherbereitstellungsfunktionen
# Zirkularitätenfeststellung
# elementare Stringfunktionen
# Initialisierung
# Speichern und Laden von MEM-Files
# Fremdprogrammaufruf

#include "lispbibl.c"

# In diesem File haben die Tabellenmacros eine andere Verwendung:
  #undef LISPSPECFORM
  #undef LISPFUN
  #undef LISPSYM
  #undef LISPOBJ

# Tabelle aller FSUBRs: ausgelagert nach SPVWTABF
# Größe dieser Tabelle:
  #define fsubr_anz  (sizeof(fsubr_tab)/sizeof(fsubr_))

# Tabelle aller SUBRs: ausgelagert nach SPVWTABF
# Größe dieser Tabelle:
  #define subr_anz  (sizeof(subr_tab)/sizeof(subr_))

# Tabelle aller Pseudofunktionen: ausgelagert nach STREAM
# Größe dieser Tabelle:
  #define pseudofun_anz  (sizeof(pseudofun_tab)/sizeof(Pseudofun))

# Tabelle aller festen Symbole: ausgelagert nach SPVWTABS
# Größe dieser Tabelle:
  #define symbol_anz  (sizeof(symbol_tab)/sizeof(symbol_))

# Tabelle aller sonstigen festen Objekte: ausgelagert nach SPVWTABO
# Größe dieser Tabelle:
  #define object_anz  (sizeof(object_tab)/sizeof(object))

# Beim Durchlaufen durch fsubr_tab bzw. subr_tab bzw. symbol_tab:
# Wandelt einen durchlaufenden Pointer in ein echtes Lisp-Objekt um.
  #ifdef MAP_MEMORY
    #define fsubr_tab_ptr_as_fsubr(ptr)  ((object)(ptr))
    #define subr_tab_ptr_as_subr(ptr)  ((object)(ptr))
    #define symbol_tab_ptr_as_symbol(ptr)  ((object)(ptr))
  #else
    #define fsubr_tab_ptr_as_fsubr(ptr)  type_pointer_object(fsubr_type,ptr)
    #define subr_tab_ptr_as_subr(ptr)  type_pointer_object(subr_type,ptr)
    #define symbol_tab_ptr_as_symbol(ptr)  type_pointer_object(symbol_type,ptr)
  #endif

# Semaphoren: entscheiden, ob eine Unterbrechung (Atari: mit Shift/Ctrl/Alt)
# unwirksam (/=0) oder wirksam (alle = 0) ist.
# Werden mit set_break_sem_x gesetzt und mit clr_break_sem_x wieder gelöscht.
  global break_sems_ break_sems;
  # break_sem_1 == break_sems.einzeln[0]
  #   gesetzt, solange die Speicherverwaltung eine Unterbrechung verbietet
  #   (damit leerer Speicher nicht von der GC durchlaufen werden kann)
  # break_sem_2 == break_sems.einzeln[1]
  #   für Package-Verwaltung auf unterem Niveau und Hashtable-Verwaltung
  # break_sem_3 == break_sems.einzeln[2]
  #   für Package-Verwaltung auf höherem Niveau
  # break_sem_4 == break_sems.einzeln[3]
  #   gesetzt, solange (ATARI) eine GEMDOS-SFIRST/SNEXT-Suche läuft
  #   bzw. (AMIGAOS) DOS oder externe Funktionen aufgerufen werden.

# GC-Statistik:
  local uintL  gc_count = 0;      # Zähler für GC-Aufrufe
  local uintL2 gc_space = {0,0};  # Größe des von der GC insgesamt bisher
                                  # wiederbeschafften Platzes (64-Bit-Akku)

# ------------------------------------------------------------------------------
#                          Zeitmessung

# Variablen für Zeitmessung:
#ifdef TIME_ATARI
  # (Grundeinheit ist 1/200 sec, ein 32-Bit-Zähler reicht also
  # für 248d 13h 13m 56.48s, und keine LISP-Session dauert 248 Tage.)
#endif
#ifdef TIME_AMIGAOS
  # (Grundeinheit ist 1/50 sec, ein 32-Bit-Zähler reicht also
  # für 994d 4h 55m 45.92s, und keine LISP-Session dauert 2.7 Jahre.)
#endif
#if defined(TIME_MSDOS) || defined(TIME_VMS)
  # (Grundeinheit ist 1/100 sec, ein 32-Bit-Zähler reicht also
  # für 497d 2h 27m 52.96s, und keine LISP-Session dauert 1.3 Jahre.)
#endif
#ifdef TIME_UNIX
  # Grundeinheit ist 1 µsec.
  # (Egal, ob der Systemtakt nun - abhängig vom lokalen Stromnetz - 60 Hz
  # oder 50 Hz beträgt oder eine genauere Uhr eingebaut ist.)
#endif
  # Zeit, die abläuft:
    local internal_time realstart_time;  # Real-Time beim LISP-Start
  # Zeit, die die GC verbraucht:
    local internal_time gc_time =        # GC-Zeitverbrauch bisher insgesamt
      #ifdef TIME_1
      0
      #endif
      #ifdef TIME_2
      {0,0}
      #endif
      ;
#ifndef HAVE_RUN_TIME
  # Zeit, die das LISP insgesamt verbraucht:
    local uintL run_time = 0;       # Runtime bisher insgesamt
    local uintL runstop_time;       # bei laufender Run-Time-Stoppuhr:
                                    # Zeitpunkt des letzten Run/Stop-Wechsels
    local boolean run_flag = FALSE; # /= 0 wenn die Run-Time-Stoppuhr läuft
#endif

#ifdef TIME_RELATIVE

# UP: greift die aktuelle Zeit ab
# get_time()
 #ifdef TIME_ATARI
# < uintL ergebnis : aktueller Stand des 200Hz-Zählers
  local uintL time_now;
  local void get_time_200 (void);
  local void get_time_200() # aktuellen Stand des 200-Hz-Zählers merken
    { time_now = *(uintL*)0x04BA; } # nur im Supervisor-Modus aufzurufen!
  local uintL get_time (void);
  local uintL get_time()
    { Supervisor_Exec(get_time_200); return time_now; }
 #endif
 #ifdef TIME_AMIGAOS
# < uintL ergebnis : aktueller Stand des 50Hz-Zählers
  local uintL get_time(void);
  local uintL get_time()
    { var struct DateStamp datestamp;
      begin_system_call();
      DateStamp(&datestamp); # aktuelle Uhrzeit holen
      end_system_call();
      # und in Ticks ab 1.1.1978 00:00:00 umrechnen:
      return ((uintL)(datestamp.ds_Days)*24*60 + (uintL)(datestamp.ds_Minute))
             *60*ticks_per_second + (uintL)(datestamp.ds_Tick);
    }
 #endif
 #ifdef TIME_MSDOS
# < uintL ergebnis : aktueller Stand des 100Hz-Zählers
  local uintL get_time(void);
  #if defined(DJUNIX) && 0 # Vorsicht: das geht eine Stunde nach!!
    local uintL get_time()
      { var struct timeval real_time;
        gettimeofday(&real_time,NULL);
        return (uintL)(real_time.tv_sec) * 100
               + (uintL)((uintW)((uintL)(real_time.tv_usec) / 16) / 625); # tv_usec/10000
      }
  #endif
  #if defined(DJUNIX) || defined(EMUNIX_OLD_8d)
    typedef struct { uintW year;  # Jahr (1980..2099)
                     uintB month; # Monat (1..12)
                     uintB day;   # Tag (1..31)
                     uintB hour;  # Stunde (0..23)
                     uintB min;   # Minute (0..59)
                     uintB sec;   # Sekunde (0..59)
                     uintB hsec;  # Hundertstel Sekunde (0..59)
                   }
            internal_decoded_time;
    local void get_decoded_time (internal_decoded_time* timepoint);
    local void get_decoded_time(timepoint)
      var reg1 internal_decoded_time* timepoint;
      #ifdef DJUNIX
      { var union REGS in;
        var union REGS out;
        loop
          { # Datum-Teil holen:
            in.h.ah = 0x2A; # DOS Get Date
            intdos(&in,&out);
            timepoint->year = out.x.cx;
            timepoint->month = out.h.dh;
            timepoint->day = out.h.dl;
            # Uhrzeit-Teil holen:
            in.h.ah = 0x2C; # DOS Get Time
            intdos(&in,&out);
            timepoint->hour = out.h.ch;
            timepoint->min = out.h.cl;
            timepoint->sec = out.h.dh;
            timepoint->hsec = out.h.dl;
            # und auf Tageswechsel überprüfen:
            if (!(timepoint->sec == 0)) break;
            if (!(timepoint->min == 0)) break;
            if (!(timepoint->hour == 0)) break;
            in.h.ah = 0x2A; # DOS Get Date
            intdos(&in,&out);
            if (timepoint->day == out.h.dl) break;
            # Datum hat sich zwischenzeitlich verändert -> wiederholen
          }
      }
      #endif
      #ifdef EMUNIX
      # [ältere Version für EMX 0.8c, noch ohne ftime(): siehe emx08c-1.d]
      { var struct _dtd datetime;
        # Uhrzeit holen:
        __ftime(&datetime);
        # und nach *timepoint umfüllen:
        timepoint->year  = datetime.year;
        timepoint->month = datetime.month;
        timepoint->day   = datetime.day;
        timepoint->hour  = datetime.hour;
        timepoint->min   = datetime.min;
        timepoint->sec   = datetime.sec;
        timepoint->hsec  = datetime.hsec;
      }
      #endif
    local uintL get_time()
      { var internal_decoded_time timepoint;
        get_decoded_time(&timepoint);
       {local var uintW monthoffsets[12] = { # Jahrtag ab dem letzten 1. März
          # Monat  1   2   3  4  5  6  7   8   9   10  11  12
                  306,337, 0,31,61,92,122,153,184,214,245,275,
          };
        var reg1 uintL UTTag;
        timepoint.year -= 1980;
        if (timepoint.month >= 3) { timepoint.year += 1; }
        UTTag = (uintL)timepoint.year * 365 + (uintL)ceiling(timepoint.year,4)
                + (uintL)monthoffsets[timepoint.month-1] + (uintL)timepoint.day + 3345;
        # Zeitzone mitberücksichtigen??
        return (((UTTag * 24 + (uintL)timepoint.hour)
                        * 60 + (uintL)timepoint.min)
                        * 60 + (uintL)timepoint.sec)
                        * 100 + (uintL)timepoint.hsec;
      }}
  #endif
  #ifdef EMUNIX_NEW_8e
    local uintL get_time()
      { var struct timeb real_time;
        __ftime(&real_time);
        return (uintL)(real_time.time) * ticks_per_second
               + (uintL)((uintW)(real_time.millitm) / (1000/ticks_per_second));
      }
  #endif
 #endif
 #ifdef TIME_VMS
# < uintL ergebnis : aktueller Stand des 100Hz-Zählers
  local uintL get_time(void);
  local uintL get_time()
    { var struct timeb real_time;
      ftime(&real_time);
      return (uintL)(real_time.time) * ticks_per_second
             + (uintL)((uintW)(real_time.millitm) / (1000/ticks_per_second));
    }
 #endif

#ifndef HAVE_RUN_TIME

# UP: Hält die Run-Time-Stoppuhr an
# run_time_stop();
  global void run_time_stop (void);
  global void run_time_stop()
    { if (!run_flag) return; # Run-Time-Stoppuhr ist schon angehalten -> OK
      # zuletzt verbrauchte Run-Time zur bisherigen Run-Time addieren:
      run_time += get_time()-runstop_time;
      run_flag = FALSE; # Run-Time-Stoppuhr steht
    }

# UP: Läßt die Run-Time-Stoppuhr weiterlaufen
# run_time_restart();
  global void run_time_restart (void);
  global void run_time_restart()
    { if (run_flag) return; # Run-Time-Stoppuhr läuft schon -> OK
      runstop_time = get_time(); # aktuelle Zeit abspeichern
      run_flag = TRUE; # Run-Time-Stoppuhr läuft
    }

#endif

# UP: Liefert die Real-Time
# get_real_time()
# < uintL ergebnis: Zeit seit LISP-System-Start (in 1/200 sec bzw. in 1/50 sec bzw. in 1/100 sec)
  global uintL get_real_time (void);
  global uintL get_real_time()
    { return get_time()-realstart_time; }

#endif

#ifdef TIME_UNIX

# UP: Liefert die Real-Time
# get_real_time()
# < internal_time* ergebnis: absolute Zeit
  global internal_time* get_real_time (void);
  global internal_time* get_real_time()
    { static union { struct timeval tv; internal_time it; } real_time;
      begin_system_call();
      if (!( gettimeofday(&real_time.tv,NULL) ==0)) { OS_error(); }
      end_system_call();
      return &real_time.it;
    }

# UP: Liefert die Run-Time
# get_run_time(&runtime);
# < internal_time runtime: Run-Time seit LISP-System-Start (in Ticks)
  local void get_run_time (internal_time* runtime);
  local void get_run_time(runtime)
    var reg1 internal_time* runtime;
    {
      #if defined(HAVE_GETRUSAGE)
      var struct rusage rusage;
      begin_system_call();
      if (!( getrusage(RUSAGE_SELF,&rusage) ==0)) { OS_error(); }
      end_system_call();
      # runtime = rusage.ru_utime + rusage.ru_stime; # User time + System time
      add_internal_time(rusage.ru_utime,rusage.ru_stime, *runtime);
      #elif defined(HAVE_SYS_TIMES_H)
      var reg2 uintL used_time; # verbrauchte Zeit, gemessen in 1/HZ Sekunden
      var struct tms tms;
      begin_system_call();
      if ((sintL) times(&tms) <0)
        { used_time = 0; } # times scheitert -> used_time unbekannt
        else
        { used_time = tms.tms_utime + tms.tms_stime; } # User time + System time
      end_system_call();
      # in Sekunden und Mikrosekunden umwandeln:
      runtime->tv_sec = floor(used_time,HZ);
      runtime->tv_usec = (used_time % HZ) * floor(2*1000000+HZ,2*HZ);
      #endif
    }

#endif

# UP: Liefert die Run-Time
# get_running_times(&timescore);
# < timescore.runtime:  Run-Time seit LISP-System-Start (in Ticks)
# < timescore.realtime: Real-Time seit LISP-System-Start (in Ticks)
# < timescore.gctime:   GC-Time seit LISP-System-Start (in Ticks)
# < timescore.gccount:  Anzahl der GC's seit LISP-System-Start
# < timescore.gcfreed:  Größe des von den GC's bisher wiederbeschafften Platzes
  global void get_running_times (timescore*);
  global void get_running_times (tm)
    var reg1 timescore* tm;
    {
     #ifndef HAVE_RUN_TIME
      var reg2 uintL time = get_time();
      tm->realtime = time - realstart_time;
      tm->runtime = (run_flag ?
                      time - runstop_time + run_time : # Run-Time-Stoppuhr läuft noch
                      run_time # Run-Time-Stoppuhr steht
                    );
     #endif
     #ifdef TIME_UNIX
      # Real-Time holen:
      var reg2 internal_time* real_time = get_real_time();
      tm->realtime.tv_sec = real_time->tv_sec - realstart_time.tv_sec;
      tm->realtime.tv_usec = real_time->tv_usec;
      # Run-Time holen:
      get_run_time(&tm->runtime);
     #endif
     #ifdef TIME_VMS
      # Real-Time holen:
      tm->realtime = get_real_time();
      # Run-Time holen:
      tm->runtime = clock();
     #endif
      tm->gctime = gc_time;
      tm->gccount = gc_count;
      tm->gcfreed = gc_space;
    }

#if defined(ATARI) || defined(MSDOS)
# UP: Wandelt das Atari-Zeitformat in Decoded-Time um.
# convert_timedate(time,date,&timepoint)
# > uintW time: Uhrzeit
#         Als Word: Bits 15..11: Stunde in {0,...,23},
#                   Bits 10..5:  Minute in {0,...,59},
#                   Bits 4..0:   Sekunde/2 in {0,...,29}.
# > uintW date: Datum
#         Als Word: Bits 15..9: Jahr-1980 in {0,...,119},
#                   Bits 8..5:  Monat in {1,...,12},
#                   Bits 4..0:  Tag in {1,...,31}.
# < timepoint.Sekunden, timepoint.Minuten, timepoint.Stunden,
#   timepoint.Tag, timepoint.Monat, timepoint.Jahr, jeweils als Fixnums
  global void convert_timedate (uintW time, uintW date, decoded_time* timepoint);
  global void convert_timedate(time,date, timepoint)
    var reg2 uintW time;
    var reg2 uintW date;
    var reg1 decoded_time* timepoint;
    { timepoint->Sekunden = fixnum( (time & (bit(5) - 1)) << 1 );
      time = time>>5;
      timepoint->Minuten = fixnum( time & (bit(6) - 1));
      time = time>>6;
      timepoint->Stunden = fixnum( time);
      timepoint->Tag = fixnum( date & (bit(5) - 1));
      date = date>>5;
      timepoint->Monat = fixnum( date & (bit(4) - 1));
      date = date>>4;
      timepoint->Jahr = fixnum( date+1980);
    }
#endif
#ifdef AMIGAOS
# UP: Wandelt das Amiga-Zeitformat in Decoded-Time um.
# convert_time(&datestamp,&timepoint);
# > struct DateStamp datestamp: Uhrzeit
#          datestamp.ds_Days   : Anzahl Tage seit 1.1.1978
#          datestamp.ds_Minute : Anzahl Minuten seit 00:00 des Tages
#          datestamp.ds_Tick   : Anzahl Ticks seit Beginn der Minute
# < timepoint.Sekunden, timepoint.Minuten, timepoint.Stunden,
#   timepoint.Tag, timepoint.Monat, timepoint.Jahr, jeweils als Fixnums
  #include "arilev0.c"  # für Division
  global void convert_time (struct DateStamp * datestamp, decoded_time* timepoint);
  global void convert_time(datestamp,timepoint)
    var reg2 struct DateStamp * datestamp;
    var reg1 decoded_time* timepoint;
    { # Methode:
      # ds_Tick durch ticks_per_second dividieren, liefert Sekunden.
      # ds_Minute durch 60 dividierem liefert Stunden und (als Rest) Minuten.
      # ds_Days in Tag, Monat, Jahr umrechnen:
      #   d := ds_Days - 790; # Tage seit 1.3.1980 (Schaltjahr)
      #   y := floor((4*d+3)/1461); # März-Jahre ab 1.3.1980
      #   d := d - floor(y*1461/4); # Tage ab letztem März-Jahres-Anfang
      #   (Diese Rechnung geht gut, solange jedes vierte Jahr ein Schaltjahr
      #    ist, d.h. bis zum Jahr 2099.)
      #   m := floor((5*d+2)/153); # Monat ab letztem März
      #   d := d - floor((153*m+2)/5); # Tag ab letztem Monatsanfang
      #   m := m+2; if (m>=12) then { m:=m-12; y:=y+1; } # auf Jahre umrechnen
      #   Tag d+1, Monat m+1, Jahr 1980+y.
      {var reg3 uintL sec;
       divu_3216_1616(datestamp->ds_Tick,ticks_per_second,sec=,);
       timepoint->Sekunden = fixnum(sec);
      }
      {var reg3 uintL std;
       var reg4 uintL min;
       divu_3216_1616(datestamp->ds_Minute,60,std=,min=);
       timepoint->Minuten = fixnum(min);
       timepoint->Stunden = fixnum(std);
      }
      {var reg5 uintL y;
       var reg4 uintW m;
       var reg3 uintW d;
       divu_3216_1616(4*(datestamp->ds_Days - 424),1461,y=,d=); # y = März-Jahre ab 1.1.1979
       d = floor(d,4); # Tage ab dem letzten März-Jahres-Anfang
       divu_1616_1616(5*d+2,153,m=,d=); # m = Monat ab letztem März
       d = floor(d,5); # Tag ab letztem Monatsanfang
       # m=0..9 -> Monat März..Dezember des Jahres 1979+y,
       # m=10..11 -> Monat Januar..Februar des Jahres 1980+y.
       if (m<10) { m += 12; y -= 1; } # auf Jahre umrechnen
       timepoint->Tag = fixnum(1+(uintL)d);
       timepoint->Monat = fixnum(-9+(uintL)m);
       timepoint->Jahr = fixnum(1980+y);
    } }
#endif
#if defined(UNIX) || defined(MSDOS) || defined(VMS)
# UP: Wandelt das System-Zeitformat in Decoded-Time um.
# convert_time(&time,&timepoint);
# > time_t time: Zeit im System-Zeitformat
# < timepoint.Sekunden, timepoint.Minuten, timepoint.Stunden,
#   timepoint.Tag, timepoint.Monat, timepoint.Jahr, jeweils als Fixnums
  global void convert_time (time_t* time, decoded_time* timepoint);
  global void convert_time(time,timepoint)
    var reg3 time_t* time;
    var reg1 decoded_time* timepoint;
    { begin_system_call();
     {var reg2 struct tm * tm = localtime(time); # decodieren
      # (Das Zeitformat des Systems muß auch das System auseinandernehmen.)
      end_system_call();
      if (!(tm==NULL))
        # localtime war erfolgreich
        { timepoint->Sekunden = fixnum(tm->tm_sec);
          timepoint->Minuten  = fixnum(tm->tm_min);
          timepoint->Stunden  = fixnum(tm->tm_hour);
          timepoint->Tag      = fixnum(tm->tm_mday);
          timepoint->Monat    = fixnum(1+tm->tm_mon);
          timepoint->Jahr     = fixnum(1900+tm->tm_year);
        }
        else
        # gescheitert -> verwende 1.1.1900, 00:00:00 als Default
        { timepoint->Sekunden = Fixnum_0;
          timepoint->Minuten  = Fixnum_0;
          timepoint->Stunden  = Fixnum_0;
          timepoint->Tag      = Fixnum_1;
          timepoint->Monat    = Fixnum_1;
          timepoint->Jahr     = fixnum(1900);
        }
    }}
#endif

# ------------------------------------------------------------------------------
#                            Debug-Hilfen

# uintL in Dezimalnotation direkt übers Betriebssystem ausgeben:
# dez_out(zahl)
  global void dez_out_ (uintL zahl);
  global void dez_out_(zahl)
    var reg1 uintL zahl;
    { var struct { uintB contents[10+1]; } buffer;
      # 10-Byte-Buffer reicht, da zahl < 2^32 <= 10^10 .
      var reg2 uintB* bufptr = &buffer.contents[10]; # Pointer in den Buffer
      *bufptr = 0; # ASCIZ-String-Ende
      do { *--bufptr = '0'+(zahl%10); zahl=floor(zahl,10); }
         until (zahl==0);
      asciz_out((char*)bufptr);
    }

# uintL in Hexadezimalnotation direkt übers Betriebssystem ausgeben:
# hex_out(zahl)
  global void hex_out_ (uintL zahl);
  global void hex_out_(zahl)
    var reg1 uintL zahl;
    { var struct { uintB contents[8+1]; } buffer;
      # 8-Byte-Buffer reicht, da zahl < 2^32 <= 16^8 .
      var reg2 uintB* bufptr = &buffer.contents[8]; # Pointer in den Buffer
      *bufptr = 0; # ASCIZ-String-Ende
      do { *--bufptr = "0123456789ABCDEF"[zahl%16]; zahl=floor(zahl,16); }
         until (zahl==0);
      asciz_out((char*)bufptr);
    }

# ------------------------------------------------------------------------------
#                         Speicherverwaltung allgemein

# Methode der Speicherverwaltung:
#if defined(SPVW_BLOCKS) && defined(SPVW_MIXED) # z.B. ATARI
  #define SPVW_MIXED_BLOCKS
#endif
#if defined(SPVW_BLOCKS) && defined(SPVW_PURE) # z.B. UNIX_LINUX ab Linux 0.99.7
  #define SPVW_PURE_BLOCKS
#endif
#if defined(SPVW_PAGES) && defined(SPVW_MIXED) # z.B. SUN3, AMIGA, HP9000_800
  #define SPVW_MIXED_PAGES
#endif
#if defined(SPVW_PAGES) && defined(SPVW_PURE) # z.B. SUN4, SUN386
  #define SPVW_PURE_PAGES
#endif

# Gesamtspeicheraufteilung:
# 1. C-Programm. Speicher wird vom Betriebssystem zugeteilt.
#    Nach Programmstart unverschieblich.
#    Auf dem ATARI:
#                |Base|Text|Data|BSS|
#                |Page|Hauptprogramm|
# 2. C-Stack. Speicher wird vom C-Programm geholt.
#    Unverschieblich.
#    Auf dem ATARI:
#               |     SP-Stack     |
#               |                  |
#               SP_BOUND           |
# 3. C-Heap. Hier unbenutzt.
#ifdef SPVW_MIXED_BLOCKS
# 4. LISP-Stack und LISP-Daten.
#    4a. LISP-Stack. Unverschieblich.
#    4b. Objekte variabler Länge. (Unverschieblich).
#    4c. Conses u.ä. Verschieblich mit move_conses.
#    Speicher hierfür wird vom Betriebssystem angefordert (hat den Vorteil,
#    daß bei EXECUTE dem auszuführenden Fremdprogramm der ganze Speicher
#    zur Verfügung gestellt werden kann, den LISP gerade nicht braucht).
#    Auf eine Unterteilung in einzelne Pages wird hier verzichtet.
#          || LISP-      |Objekte         |    leer  |Conses| Reserve |
#          || Stack      |variabler Länge              u.ä. |         |
#          |STACK_BOUND  |         objects.end   conses.start |         |
#        MEMBOT   objects.start                         conses.end    MEMTOP
#endif
#ifdef SPVW_PURE_BLOCKS
# 4. LISP-Stack. Unverschieblich.
# 5. LISP-Daten. Für jeden Typ ein großer Block von Objekten.
#endif
#ifdef SPVW_MIXED_PAGES
# 4. LISP-Stack. Unverschieblich.
# 5. LISP-Daten.
#    Unterteilt in Pages für Objekte variabler Länge und Pages für Conses u.ä.
#endif
#ifdef SPVW_PURE_PAGES
# 4. LISP-Stack. Unverschieblich.
# 5. LISP-Daten. Unterteilt in Pages, die nur Objekte desselben Typs enthalten.
#endif

# ------------------------------------------------------------------------------
#                          Eigenes malloc(), free()

#ifdef AMIGAOS

# Eigenes malloc(), free() nötig wegen Resource Tracking.

  # Doppelt verkettete Liste aller bisher belegten Speicherblöcke führen:
  typedef struct MemBlockHeader { struct MemBlockHeader * next;
                                  #ifdef SPVW_PAGES
                                  struct MemBlockHeader * * prev;
                                  #endif
                                  uintL size;
                                  oint usable_memory[unspecified]; # "oint" erzwingt Alignment
                                }
          MemBlockHeader;
  local MemBlockHeader* allocmemblocks = NULL;
  #ifdef SPVW_PAGES
  # Für alle p = allocmemblocks{->next}^n (n=0,1,...) mit !(p==NULL) gilt
  # *(p->prev) = p.
  #endif

  # Speicher vom Betriebssystem holen:
  local void* allocmem (uintL amount);
  local void* allocmem(amount)
    var reg2 uintL amount;
    { amount = round_up(amount+offsetof(MemBlockHeader,usable_memory),4);
      #ifdef WIDE
        #define allocmemflag  MEMF_ANY
      #else
        # Noch können wir Speicher außerhalb des 26-Bit-Adreßraums nicht nutzen.
        #define allocmemflag  MEMF_24BITDMA
      #endif
     {var reg1 void* address = AllocMem(amount,allocmemflag);
      if (!(address==NULL))
        { ((MemBlockHeader*)address)->size = amount;
          ((MemBlockHeader*)address)->next = allocmemblocks;
          #ifdef SPVW_PAGES
          ((MemBlockHeader*)address)->prev = &allocmemblocks;
          if (!(allocmemblocks == NULL))
            { if (allocmemblocks->prev == &allocmemblocks) # Sicherheits-Check
                { allocmemblocks->prev = &((MemBlockHeader*)address)->next; }
                else
                { abort(); }
            }
          #endif
          allocmemblocks = (MemBlockHeader*)address;
          address = &((MemBlockHeader*)address)->usable_memory[0];
        }
      return address;
    }}

  # Speicher dem Betriebssystem zurückgeben:
  local void freemem (void* address);
  local void freemem(address)
    var reg2 void* address;
    { var reg1 MemBlockHeader* ptr = (MemBlockHeader*)((aint)address - offsetof(MemBlockHeader,usable_memory));
      #ifdef SPVW_PAGES
      if (*(ptr->prev) == ptr) # Sicherheits-Check
        { var reg2 MemBlockHeader* ptrnext = ptr->next;
          *(ptr->prev) = ptrnext; # ptr durch ptr->next ersetzen
          if (!(ptrnext == NULL)) { ptrnext->prev = ptr->prev; }
          FreeMem(ptr,ptr->size);
          return;
        }
      #else
      # Spar-Implementation, die nur in der Lage ist, den letzten allozierten
      # Block zurückzugeben:
      if (allocmem == ptr) # Sicherheits-Check
        { allocmem = ptr->next; # ptr durch ptr->next ersetzen
          FreeMem(ptr,ptr->size);
          return;
        }
      #endif
        else
        { abort(); }
    }

  #define malloc  allocmem
  #define free    freemem

#endif

# ------------------------------------------------------------------------------
#                          Page-Allozierung

#ifdef MULTIMAP_MEMORY

# Das Betriebssystem erlaubt es, denselben (virtuellen) Speicher unter
# verschiedenen Adressen anzusprechen.
# Dabei gibt es allerdings Restriktionen:
# - Die Adressenabbildung kann nur für ganze Speicherseiten auf einmal
#   erstellt werden.
# - Wir brauchen zwar nur diesen Adreßraum und nicht seinen Inhalt, müssen
#   ihn aber mallozieren und dürfen ihn nicht freigeben, da er in unserer
#   Kontrolle bleiben soll.

# Länge einer Speicherseite des Betriebssystems:
  local uintL map_pagesize; # wird eine Zweierpotenz sein, meist 4096.

# Initialisierung:
# initmap() bzw. initmap(tmpdir)

# In einen Speicherbereich [map_addr,map_addr+map_len-1] leere Seiten legen:
# (map_addr und map_len durch pagesize teilbar.)
# zeromap(map_addr,map_len)

# Auf einen Speicherbereich [map_addr,map_addr+map_len-1] Seiten legen,
# die unter den Typcodes, die in typecases angegeben sind, ansprechbar
# sein sollen:
# multimap(typecases,map_addr,map_len);

#ifdef MULTIMAP_MEMORY_VIA_FILE

  local char tempfilename[MAXPATHLEN]; # Name eines temporären Files
  local int zero_fd; # Handle von /dev/zero

  local int initmap (char* tmpdir);
  local int initmap(tmpdir)
    var reg3 char* tmpdir;
    # Virtual Memory Mapping aufbauen:
    { # Wir brauchen ein temporäres File.
      # tempfilename := (string-concat tmpdir "/" "lisptemp.mem")
      {var reg1 char* ptr1 = tmpdir;
       var reg2 char* ptr2 = &tempfilename[0];
       while (!(*ptr1 == '\0')) { *ptr2++ = *ptr1++; }
       if (!((ptr2 > &tempfilename[0]) && (ptr2[-1] == '/')))
         { *ptr2++ = '/'; }
       ptr1 = "lisptemp.mem";
       while (!(*ptr1 == '\0')) { *ptr2++ = *ptr1++; }
       *ptr2 = '\0';
      }
      { var reg1 int fd = open("/dev/zero",O_RDWR,my_open_mask);
        if (fd<0)
          { asciz_out(DEUTSCH ? "Kann /dev/zero nicht öffnen." :
                      ENGLISH ? "Cannot open /dev/zero ." :
                      FRANCAIS ? "Ne peux pas ouvrir /dev/zero ." :
                      ""
                     );
            return -1; # error
          }
        zero_fd = fd;
      }
      return 0;
    }

  local int fdmap (int fd, void* map_addr, uintL map_len);
  local int fdmap(fd,map_addr,map_len)
    var reg3 int fd;
    var reg1 void* map_addr;
    var reg2 uintL map_len;
    { if ( (sintL) mmap(map_addr, # gewünschte Adresse
                        map_len, # Länge
                        PROT_READ|PROT_WRITE, # Zugriffsrechte
                        MAP_SHARED | MAP_FIXED, # genau an diese Adresse!
                        fd, 0 # File ab Position 0 legen
                       )
           <0
         )
        { asciz_out(DEUTSCH ? "Kann keinen Speicher an Adresse " :
                    ENGLISH ? "Cannot map memory to address " :
                    FRANCAIS ? "Ne peux pas placer de la mémoire à l'adresse " :
                    ""
                   );
          dez_out(map_addr);
          asciz_out(DEUTSCH ? " legen." :
                    ENGLISH ? " ." :
                    FRANCAIS ? " ." :
                    ""
                   );
          asciz_out(" errno = "); dez_out(errno); asciz_out(CRLFstring);
          return -1; # error
        }
      return 0;
    }

  local int zeromap (void* map_addr, uintL map_len);
  local int zeromap(map_addr,map_len)
    var reg1 void* map_addr;
    var reg2 uintL map_len;
    { return fdmap(zero_fd,map_addr,map_len); }

  local int open_temp_fd (uintL map_len);
  local int open_temp_fd(map_len)
    var reg2 uintL map_len;
    { var reg1 int fd = open(tempfilename,O_RDWR|O_CREAT|O_TRUNC|O_EXCL,my_open_mask);
      if (fd<0)
        { asciz_out(DEUTSCH ? "Kann " :
                    ENGLISH ? "Cannot open " :
                    FRANCAIS ? "Ne peux pas ouvrir " :
                    ""
                   );
          asciz_out(tempfilename);
          asciz_out(DEUTSCH ? " nicht öffnen." :
                    ENGLISH ? " ." :
                    FRANCAIS ? " ." :
                    ""
                   );
          asciz_out(" errno = "); dez_out(errno); asciz_out(CRLFstring);
          return -1; # error
        }
      # und öffentlich unzugänglich machen, indem wir es löschen:
      # (Das Betriebssystem löscht das File erst dann, wenn am Ende dieses
      # Prozesses in _exit() ein close(fd) durchgeführt wird.)
      if ( unlink(tempfilename) <0)
        { asciz_out(DEUTSCH ? "Kann " :
                    ENGLISH ? "Cannot delete " :
                    FRANCAIS ? "Ne peux pas effacer " :
                    ""
                   );
          asciz_out(tempfilename);
          asciz_out(DEUTSCH ? " nicht löschen." :
                    ENGLISH ? " ." :
                    FRANCAIS ? " ." :
                    ""
                   );
          asciz_out(" errno = "); dez_out(errno); asciz_out(CRLFstring);
          return -1; # error
        }
      # überprüfen, ob genug Plattenplatz da ist:
      { var struct statfs statbuf;
        if (!( fstatfs(fd,&statbuf) <0))
          if (!(statbuf.f_bsize == (long)(-1)) && !(statbuf.f_bavail == (long)(-1)))
            { var reg2 uintL available = (uintL)(statbuf.f_bsize) * (uintL)(statbuf.f_bavail);
              if (available < map_len)
                # auf der Platte ist voraussichtlich zu wenig Platz
                { asciz_out(DEUTSCH ? "** WARNUNG: ** Zu wenig freier Plattenplatz für " :
                            ENGLISH ? "** WARNING: ** Too few free disk space for " :
                            FRANCAIS ? "** AVERTISSEMENT : ** Trop peu de place disque restante sur " :
                            ""
                           );
                  asciz_out(tempfilename);
                  asciz_out(DEUTSCH ? " ." CRLFstring :
                            ENGLISH ? " ." CRLFstring :
                            FRANCAIS ? " ." CRLFstring :
                            ""
                           );
                  asciz_out(DEUTSCH ? "Bitte LISP mit weniger Speicher (Option -m) neu starten." CRLFstring :
                            ENGLISH ? "Please restart LISP with fewer memory (option -m)." CRLFstring :
                            FRANCAIS ? "Prière de relancer LISP avec moins de mémoire (option -m)." CRLFstring :
                            ""
                           );
      }     }   }
      # Auf Größe map_len aufblähen:
      { var uintB dummy = 0;
        if (( lseek(fd,map_len-1,SEEK_SET) <0) || (!( write(fd,&dummy,1) ==1)))
          { asciz_out(DEUTSCH ? "Kann " :
                      ENGLISH ? "Cannot make " :
                      FRANCAIS ? "Ne peux pas agrandir " :
                      ""
                     );
            asciz_out(tempfilename);
            asciz_out(DEUTSCH ? " nicht aufblähen." :
                      ENGLISH ? " long enough." :
                      FRANCAIS ? " ." :
                      ""
                     );
            asciz_out(" errno = "); dez_out(errno); asciz_out(CRLFstring);
            return -1; # error
      }   }
      return fd;
    }

  local int close_temp_fd (int fd);
  local int close_temp_fd(fd)
    var reg1 int fd;
    { if ( close(fd) <0)
        { asciz_out(DEUTSCH ? "Kann " :
                    ENGLISH ? "Cannot close " :
                    FRANCAIS ? "Ne peux pas fermer " :
                    ""
                   );
          asciz_out(tempfilename);
          asciz_out(DEUTSCH ? " nicht schließen." :
                    ENGLISH ? " ." :
                    FRANCAIS ? " ." :
                    ""
                   );
          asciz_out(" errno = "); dez_out(errno); asciz_out(CRLFstring);
          return -1; # error
        }
      return 0;
    }

  # Vorgehen bei multimap:
  # 1. Temporäres File aufmachen
    #define open_mapid(map_len)  open_temp_fd(map_len) # -> fd
  # 2. File mehrfach überlagert in den Speicher legen
    #define map_mapid(fd,map_addr,map_len)  fdmap(fd,map_addr,map_len)
  # 3. File schließen
  # (Das Betriebssystem schließt und löscht das File erst dann, wenn am
  # Ende dieses Prozesses in _exit() ein munmap() durchgeführt wird.)
    #define close_mapid(fd)  close_temp_fd(fd)

  #define multimap(typecases,map_addr,map_len)  \
    { # Temporäres File aufmachen:                            \
      var reg2 int mapid = open_mapid(map_len);               \
      if (mapid<0) goto no_mem;                               \
      # und mehrfach überlagert in den Speicher legen:        \
      { var reg1 oint type;                                   \
        for (type=0; type < bit(oint_type_len<=8 ? oint_type_len : 8); type++) \
          { switch (type)                                     \
              { typecases                                     \
                  if ( map_mapid(mapid,ThePointer(type_pointer_object(type,map_addr)),map_len) <0) \
                    goto no_mem;                              \
                  break;                                      \
                default: break;                               \
      }   }   }                                               \
      # und öffentlich unzugänglich machen:                   \
      if ( close_mapid(mapid) <0)                             \
        goto no_mem;                                          \
    }

#endif # MULTIMAP_MEMORY_VIA_FILE

#ifdef MULTIMAP_MEMORY_VIA_SHM

# Virtual Memory Mapping über Shared Memory aufbauen:

  local int initmap (void);
  local int initmap()
    {
     #ifdef SHM_NEEDS_INIT
     {var struct shminfo shminfo;
      shminfo.shmmax = bit(oint_addr_len);        /* max shared memory segment size (bytes) */
      shminfo.shmmin = SHMMIN;                    /* min shared memory segment size (bytes) */
      shminfo.shmmni = 85; /* floor(4095,44+4) */ /* max num of shared segments system wide */
      shminfo.shmall = bit(oint_addr_len)/SHMLBA; /* max shared mem system wide (in pages) */
      if ( ipc_init(SHM_INIT,&shminfo) <0)
        if (!(errno==EEXIST))
          { asciz_out(DEUTSCH ? "Kann Shared-Memory-Benutzung nicht initialisieren." :
                      ENGLISH ? "Cannot initialize shared memory facility." :
                      FRANCAIS ? "Ne peux pas préparer l'utilisage de mémoire partagée." :
                      ""
                     );
            asciz_out(" errno = "); dez_out(errno); asciz_out(CRLFstring);
            if (errno==ENOSYS)
              asciz_out(DEUTSCH ? "Compilieren Sie Ihr Betriebssystem neu mit Unterstützung von SYSV IPC." CRLFstring :
                        ENGLISH ? "Recompile your operating system with SYSV IPC support." CRLFstring :
                        FRANCAIS ? "Recompilez votre système opérationnel tel qu'il comprenne IPC SYSV." CRLFstring :
                        ""
                       );
            return -1; # error
     }    }
     #endif
     return 0;
    }

  local int open_shmid (uintL map_len);
  local int open_shmid(map_len)
    var reg2 uintL map_len;
    { var reg1 int shmid = shmget(IPC_PRIVATE,map_len,0600|IPC_CREAT); # 0600 = 'Read/Write nur für mich'
      if (shmid<0)
        { asciz_out(DEUTSCH ? "Kann kein privates Shared-Memory-Segment aufmachen." :
                    ENGLISH ? "Cannot allocate private shared memory segment." :
                    FRANCAIS ? "Ne peux pas allouer de segment privé de mémoire partagée." :
                    ""
                   );
          asciz_out(" errno = "); dez_out(errno); asciz_out(CRLFstring);
          return -1; # error
        }
      return shmid;
    }

  local int idmap (int shmid, void* map_addr);
  local int idmap(shmid,map_addr)
    var reg2 int shmid;
    var reg1 void* map_addr;
    { if ( shmat(shmid,
                 map_addr, # Adresse
                 0 # Default: Read/Write
                )
           == (void*)(-1)
         )
        { asciz_out(DEUTSCH ? "Kann kein Shared-Memory an Adresse " :
                    ENGLISH ? "Cannot map shared memory to address " :
                    FRANCAIS ? "Ne peux pas placer de la mémoire partagée à l'adresse " :
                    ""
                   );
          dez_out(map_addr);
          asciz_out(DEUTSCH ? " legen." :
                    ENGLISH ? "." :
                    FRANCAIS ? "." :
                    ""
                   );
          asciz_out(" errno = "); dez_out(errno); asciz_out(CRLFstring);
          return -1; # error
        }
      return 0;
    }

  local int close_shmid (int shmid);
  local int close_shmid(shmid)
    var reg1 int shmid;
    { if ( shmctl(shmid,IPC_RMID,NULL) <0)
        { asciz_out(DEUTSCH ? "Kann Shared-Memory-Segment nicht entfernen." :
                    ENGLISH ? "Cannot remove shared memory segment." :
                    FRANCAIS ? "Ne peux pas retirer un segment de mémoire partagée." :
                    ""
                   );
          asciz_out(" errno = "); dez_out(errno); asciz_out(CRLFstring);
          return -1; # error
        }
      return 0;
    }

  local int zeromap (void* map_addr, uintL map_len);
  local int zeromap(map_addr,map_len)
    var reg3 void* map_addr;
    var reg2 uintL map_len;
    { var reg1 int shmid = open_shmid(map_len);
      if (shmid<0)
        { return -1; } # error
      if (idmap(shmid,map_addr) < 0)
        { return -1; } # error
      return close_shmid(shmid);
    }

  # Vorgehen bei multimap:
  # 1. Shared-Memory-Bereich zur Verfügung stellen
    #define open_mapid(map_len)  open_shmid(map_len) # -> shmid
  # 2. Shared-Memory mehrfach überlagert in den Speicher legen
    #define map_mapid(shmid,map_addr,map_len)  idmap(shmid,map_addr)
  # 3. öffentlich unzugänglich machen, indem wir ihn löschen:
  # (Das Betriebssystem löscht den Shared Memory erst dann, wenn am
  # Ende dieses Prozesses in _exit() ein munmap() durchgeführt wird.)
    #define close_mapid(shmid)  close_shmid(shmid)

  #define multimap(typecases,total_map_addr,total_map_len)  \
    { var reg4 uintL remaining_len = total_map_len;                                    \
      var reg5 aint map_addr = total_map_addr;                                         \
      do { var reg3 uintL map_len = (remaining_len > SHMMAX ? SHMMAX : remaining_len); \
           # Shared-Memory-Bereich aufmachen:                                          \
           var reg2 int mapid = open_mapid(map_len);                                   \
           if (mapid<0) goto no_mem;                                                   \
           # und mehrfach überlagert in den Speicher legen:                            \
           { var reg1 oint type;                                                       \
             for (type=0; type < bit(oint_type_len<=8 ? oint_type_len : 8); type++)    \
               { switch (type)                                                         \
                   { typecases                                                         \
                       if ( map_mapid(mapid,ThePointer(type_pointer_object(type,map_addr)),map_len) <0) \
                         goto no_mem;                                                  \
                       break;                                                          \
                     default: break;                                                   \
           }   }   }                                                                   \
           # und öffentlich unzugänglich machen:                                       \
           if ( close_mapid(mapid) <0)                                                 \
             goto no_mem;                                                              \
           map_addr += map_len; remaining_len -= map_len;                              \
         }                                                                             \
         until (remaining_len==0);                                                     \
    }

#endif # MULTIMAP_MEMORY_VIA_SHM

#ifdef SPVW_PAGES

# Hantiere mit einer kleinen Anzahl von Mapids (temporären Files), die
# jeweils ein ganzes Bündel Pages zum Multimap zur Verfügung stellen.
  #define max_mapid_count  16  # Maximalzahl dieser Mapids
  local uintC mapid_count; # Anzahl der offenen Mapids
  local int mapids[max_mapid_count]; # Tabelle der offenen Mapids
  local uintL pages_per_mapid; # Anzahl der Pages per mapid,
                               # je der Länge map_pagesize
  local uintL page_used_table_size; # Anzahl der möglichen Pages
  local uintB* page_used_table; # Tabelle von Bits, ob eine Page benutzt ist
  #define page_used(pagenr)  (page_used_table[floor(pagenr,intBsize)] & bit((pagenr)%intBsize))
  #define set_page_used(pagenr)  page_used_table[floor(pagenr,intBsize)] |= bit((pagenr)%intBsize)
  #define set_page_unused(pagenr)  page_used_table[floor(pagenr,intBsize)] &= ~bit((pagenr)%intBsize)

# Initialisierung:
  { mapid_count = 0;
    pages_per_mapid = floor(floor(wbit(oint_addr_len),max_mapid_count),map_pagesize);
    page_used_table_size = max_mapid_count * pages_per_mapid;
    page_used_table = malloc(ceiling(page_used_table_size,intBsize));
    {var reg1 uintL i;
     for (i=0; i<page_used_table_size; i++) { set_page_unused(i); }
  } }

#endif

#endif # MULTIMAP_MEMORY

#ifdef SINGLEMAP_MEMORY

# Das Betriebssystem erlaubt es, an willkürlichen Adressen Speicher hinzulegen,
# der sich genauso benimmt wie malloc()-allozierter Speicher.

# Länge einer Speicherseite des Betriebssystems:
  local uintL map_pagesize; # wird eine Zweierpotenz sein, meist 4096.

# Initialisierung:
# initmap()

# In einen Speicherbereich [map_addr,map_addr+map_len-1] leere Seiten legen:
# (map_addr und map_len durch pagesize teilbar.)
# zeromap(map_addr,map_len)

# Beide mmap()-Methoden gleichzeitig anzuwenden, ist unnötig:
#ifdef HAVE_MMAP_ANON
  #undef HAVE_MMAP_DEVZERO
#endif

#ifdef HAVE_MMAP_DEVZERO
  local int zero_fd; # Handle von /dev/zero
  # Zugriff auf /dev/zero: /dev/zero hat manchmal Permissions 0644. Daher
  # open() mit nur O_RDONLY statt O_RDWR. Daher MAP_PRIVATE statt MAP_SHARED.
  #ifdef MAP_FILE
    #define map_flags  MAP_FILE | MAP_PRIVATE
  #else
    #define map_flags  MAP_PRIVATE
  #endif
#endif
#ifdef HAVE_MMAP_ANON
  #define zero_fd  -1 # irgendein ungültiges Handle geht!
  #define map_flags  MAP_ANON | MAP_PRIVATE
#endif

  local int initmap (void);
  local int initmap()
    {
      #ifdef HAVE_MMAP_DEVZERO
      { var reg1 int fd = open("/dev/zero",O_RDONLY,my_open_mask);
        if (fd<0)
          { asciz_out(DEUTSCH ? "Kann /dev/zero nicht öffnen." :
                      ENGLISH ? "Cannot open /dev/zero ." :
                      FRANCAIS ? "Ne peux pas ouvrir /dev/zero ." :
                      ""
                     );
            return -1; # error
          }
        zero_fd = fd;
      }
      #endif
      return 0;
    }

  local int zeromap (void* map_addr, uintL map_len);
  local int zeromap(map_addr,map_len)
    var reg1 void* map_addr;
    var reg2 uintL map_len;
    { if ( (sintL) mmap(map_addr, # gewünschte Adresse
                        map_len, # Länge
                        PROT_READ | PROT_WRITE, # Zugriffsrechte
                        map_flags | MAP_FIXED, # genau an diese Adresse!
                        zero_fd, 0 # leere Seiten legen
                       )
           <0
         )
        { asciz_out(DEUTSCH ? "Kann keinen Speicher an Adresse " :
                    ENGLISH ? "Cannot map memory to address " :
                    FRANCAIS ? "Ne peux pas placer de la mémoire à l'adresse " :
                    ""
                   );
          dez_out(map_addr);
          asciz_out(DEUTSCH ? " legen." :
                    ENGLISH ? " ." :
                    FRANCAIS ? " ." :
                    ""
                   );
          asciz_out(" errno = "); dez_out(errno); asciz_out(CRLFstring);
          return -1; # error
        }
      return 0;
    }

#endif # SINGLEMAP_MEMORY

# ------------------------------------------------------------------------------
#                           Page-Verwaltung

# Page-Deskriptor:
typedef struct { aint start;  # Pointer auf den belegten Platz (aligned)
                 aint end;    # Pointer hinter den belegten Platz (aligned)
                 union { object firstmarked; uintL l; aint d; void* next; }
                      gcpriv; # private Variable während GC
               }
        _Page;

# Page-Deskriptor samt dazugehöriger Verwaltungsinformation:
# typedef ... Page;
# Hat die Komponenten page_start, page_end, page_gcpriv.

# Eine Ansammlung von Pages:
# typedef ... Pages;

# Eine Ansammlung von Pages und die für sie nötige Verwaltungsinformation:
# typedef ... Heap;

#ifdef SPVW_PAGES

# Jede Page enthält einen Header für die AVL-Baum-Verwaltung.
# Das erlaubt es, daß die AVL-Baum-Verwaltung selbst keine malloc-Aufrufe
# tätigen muß.

#define AVLID  spvw
#define AVL_ELEMENT  uintL
#define AVL_EQUAL(element1,element2)  ((element1)==(element2))
#define AVL_KEY  AVL_ELEMENT
#define AVL_KEYOF(element)  (element)
#define AVL_COMPARE(key1,key2)  (sintL)((key1)-(key2))
#define NO_AVL_MEMBER
#define NO_AVL_INSERT
#define NO_AVL_DELETE

#include "avl.c"

typedef struct NODE
               { NODEDATA nodedata;        # NODE für AVL-Baum-Verwaltung
                 #define page_room  nodedata.value # freier Platz in dieser Page (in Bytes)
                 _Page page;       # Page-Deskriptor, bestehend aus:
                 #define page_start  page.start  # Pointer auf den belegten Platz (aligned)
                 #define page_end    page.end    # Pointer auf den freien Platz (aligned)
                 #define page_gcpriv page.gcpriv # private Variable während GC
                 aint m_start;     # von malloc gelieferte Startadresse (unaligned)
                 aint m_length;    # bei malloc angegebene Page-Länge (in Bytes)
               }
        NODE;
#define HAVE_NODE

#include "avl.c"

typedef NODE Page;

typedef Page* Pages;

typedef struct { Pages inuse;     # Die gerade benutzten Pages
                 # _Page reserve; # Eine Reserve-Page ??
                 # Bei Heap für Objekte fester Länge:
                 Pages lastused; # Ein Cache für die letzte benutzte Page
               }
        Heap;

# Größe einer normalen Page = minimale Pagegröße. Durch sizeof(cons_) teilbar.
  #define min_page_size_brutto  bit(oint_addr_len/2)  # um offset_pages_len (s.u.) nicht zu groß werden zu lassen
  #define std_page_size  round_down(min_page_size_brutto-sizeof(NODE)-(Varobject_alignment-1),sizeof(cons_))

# Eine Dummy-Page für lastused:
  local NODE dummy_NODE;
  #define dummy_lastused  (&dummy_NODE)
  #define set_lastused_dummy(heap)  heap.lastused = dummy_lastused;

#endif

#ifdef SPVW_BLOCKS

typedef _Page Page;
#define page_start   start
#define page_end     end
#define page_gcpriv  gcpriv

typedef Page Pages;

#ifdef SPVW_MIXED_BLOCKS

typedef Pages Heap;

#endif

#ifdef SPVW_PURE_BLOCKS

typedef struct { Pages pages; aint limit; } Heap;
#define heap_start  pages.page_start
#define heap_end    pages.page_end
#define heap_limit  limit
# Stets heap_start <= heap_end <= heap_limit.
# Der Speicher zwischen heap_start und heap_end ist belegt,
# der Speicher zwischen heap_end und heap_limit ist frei.
# heap_limit wird, wenn nötig, vergrößert.

#endif

#endif

#ifdef SPVW_MIXED

# Zwei Heaps: einer für Objekte variabler Länge, einer für Conses u.ä.
#define heapcount  2

#endif

#ifdef SPVW_PURE

# Ein Heap für jeden möglichen Typcode
#define heapcount  bit(oint_type_len<=8 ? oint_type_len : 8)

#endif

# Durchlaufen aller CONS-Pages:
# for_each_cons_page(page, [statement, das 'var Page* page' benutzt] );

# Durchlaufen aller Pages von Objekten variabler Länge:
# for_each_varobject_page(page, [statement, das 'var Page* page' benutzt] );

# Durchlaufen aller Pages:
# for_each_page(page, [statement, das 'var Page* page' benutzt] );

#ifdef SPVW_MIXED_BLOCKS
  #define map_heap(heap,pagevar,statement)  \
    { var reg1 Page* pagevar = &(heap); statement; }
#endif
#ifdef SPVW_PURE_BLOCKS
  #define map_heap(heap,pagevar,statement)  \
    { var reg1 Page* pagevar = &(heap).pages; statement; }
#endif
#ifdef SPVW_PAGES
  #define map_heap(heap,pagevar,statement)  \
    { AVL_map((heap).inuse,pagevar,statement); }
#endif

#ifdef SPVW_MIXED

#define for_each_cons_heap(heapmacro)  \
  heapmacro(mem.conses)
#define for_each_varobject_heap(heapmacro)  \
  heapmacro(mem.objects)
#define for_each_heap(heapmacro)  \
  { var reg4 uintL heapnr;                     \
    for (heapnr=0; heapnr<heapcount; heapnr++) \
      heapmacro(mem.heaps[heapnr]);            \
  }

#define for_each_cons_page(pagevar,statement)  \
  map_heap(mem.conses,pagevar,statement)
#define for_each_varobject_page(pagevar,statement)  \
  map_heap(mem.objects,pagevar,statement)
#define for_each_page(pagevar,statement)  \
  { var reg4 uintL heapnr;                           \
    for (heapnr=0; heapnr<heapcount; heapnr++)       \
      map_heap(mem.heaps[heapnr],pagevar,statement); \
  }

#endif

#ifdef SPVW_PURE

# Innerhalb der Schleife ist heapnr die Nummer des Heaps.

#define for_each_cons_heap(heapmacro)  \
  { var reg4 uintL heapnr;                     \
    for (heapnr=0; heapnr<heapcount; heapnr++) \
      if (mem.heaptype[heapnr] > 0)            \
        heapmacro(mem.heaps[heapnr]);          \
  }
#define for_each_varobject_heap(heapmacro)  \
  { var reg4 uintL heapnr;                     \
    for (heapnr=0; heapnr<heapcount; heapnr++) \
      if (mem.heaptype[heapnr] == 0)           \
        heapmacro(mem.heaps[heapnr]);          \
  }
#define for_each_heap(heapmacro)  \
  { var reg4 uintL heapnr;                     \
    for (heapnr=0; heapnr<heapcount; heapnr++) \
      if (mem.heaptype[heapnr] >= 0)           \
        heapmacro(mem.heaps[heapnr]);          \
  }

#define for_each_cons_page(pagevar,statement)  \
  { var reg4 uintL heapnr;                             \
    for (heapnr=0; heapnr<heapcount; heapnr++)         \
      if (mem.heaptype[heapnr] > 0)                    \
        map_heap(mem.heaps[heapnr],pagevar,statement); \
  }
#define for_each_varobject_page(pagevar,statement)  \
  { var reg4 uintL heapnr;                             \
    for (heapnr=0; heapnr<heapcount; heapnr++)         \
      if (mem.heaptype[heapnr] == 0)                   \
        map_heap(mem.heaps[heapnr],pagevar,statement); \
  }
#define for_each_page(pagevar,statement)  \
  { var reg4 uintL heapnr;                             \
    for (heapnr=0; heapnr<heapcount; heapnr++)         \
      if (mem.heaptype[heapnr] >= 0)                   \
        map_heap(mem.heaps[heapnr],pagevar,statement); \
  }

#endif

# ------------------------------------------------------------------------------

# Speichergrenzen der LISP-Daten:
  local struct { aint MEMBOT;
                 # dazwischen der LISP-Stack
                 Heap heaps[heapcount];
                 #ifdef SPVW_PURE
                 sintB heaptype[heapcount];
                   # zu jedem Typcode: 0 falls Objekte variabler Länge,
                   #                   1 falls Conses u.ä.
                   #                  -1 falls unbenutzter Typcode
                 #endif
                 #ifdef SPVW_MIXED
                  #define objects  heaps[0] # Objekte variabler Länge
                  #define conses   heaps[1] # Conses u.ä.
                 #endif
                 #ifdef SPVW_MIXED_BLOCKS
                  # dazwischen leer, frei für LISP-Objekte
                 #define MEMRES    conses.end
                 # dazwischen Reserve
                 aint MEMTOP;
                 #endif
                 #ifdef SPVW_PURE_BLOCKS
                 uintL total_room; # wieviel Platz belegt werden darf, ohne daß GC nötig wird
                 #endif
                 #ifdef SPVW_PAGES
                 Pages free_pages; # eine Liste freier normalgroßer Pages
                 uintL last_gcend_space; # wieviel Platz am Ende der letzten GC belegt war
                 boolean last_gc_compacted; # ob die letzte GC schon kompaktiert hat
                 #endif
               }
        mem;
  #define RESERVE       0x00400L  # 1 KByte Speicherplatz als Reserve
  #define MINIMUM_SPACE 0x10000L  # 64 KByte als minimaler Speicherplatz
                                  #  für LISP-Daten

#ifdef ATARI
  local aint MEMBLOCK;
  # MEMBLOCK = Startadresse des vom Betriebssystem allozierten Speicherblocks
  # Der SP-Stack liegt zwischen MEMBLOCK und mem.MEMBOT.
#endif

# Stack-Grenzen:
  global void* SP_bound;    # SP-Wachstumsgrenze
  global void* STACK_bound; # STACK-Wachstumsgrenze

# Bei Überlauf eines der Stacks:
  global nonreturning void SP_ueber (void);
  global nonreturning void SP_ueber()
    { asciz_out( DEUTSCH ? CRLFstring "*** - " "Programmstack-Überlauf: RESET" :
                 ENGLISH ? CRLFstring "*** - " "Program stack overflow. RESET" :
                 FRANCAIS ? CRLFstring "*** - " "Débordement de pile de programme : RAZ" :
                 ""
               );
      reset();
    }
  global nonreturning void STACK_ueber (void);
  global nonreturning void STACK_ueber()
    { asciz_out( DEUTSCH ? CRLFstring "*** - " "LISP-Stack-Überlauf: RESET" :
                 ENGLISH ? CRLFstring "*** - " "Lisp stack overflow. RESET" :
                 FRANCAIS ? CRLFstring "*** - " "Débordement de pile Lisp : RAZ" :
                 ""
               );
      reset();
    }

# Überprüfung des Speicherinhalts auf GC-Festigkeit:
  #if defined(SPVW_PAGES) && defined(DEBUG_SPVW)
    # Überprüfen, ob die Verwaltung der Pages in Ordnung ist:
      #define CHECK_AVL_CONSISTENCY()  check_avl_consistency()
      local void check_avl_consistency (void);
      local void check_avl_consistency()
        { var reg4 uintL heapnr;
          for (heapnr=0; heapnr<heapcount; heapnr++)
            { AVL(AVLID,check) (mem.heaps[heapnr].inuse); }
        }
    # Überprüfen, ob die Grenzen der Pages in Ordnung sind:
      #define CHECK_GC_CONSISTENCY()  check_gc_consistency()
      local void check_gc_consistency (void);
      local void check_gc_consistency()
        { for_each_page(page,
            if ((sintL)page->page_room < 0)
              { asciz_out("\nPage bei Adresse 0x"); hex_out(page); asciz_out(" übergelaufen!!\n"); abort(); }
            if (!(page->page_start == round_up((aint)page+sizeof(NODE),Varobject_alignment)))
              { asciz_out("\nPage bei Adresse 0x"); hex_out(page); asciz_out(" inkonsistent!!\n"); abort(); }
            if (!(page->page_end + page->page_room
                  == round_down(page->m_start + page->m_length,Varobject_alignment)
               ) )
              { asciz_out("\nPage bei Adresse 0x"); hex_out(page); asciz_out(" inkonsistent!!\n"); abort(); }
            );
        }
    # Überprüfen, ob während der kompaktierenden GC
    # die Grenzen der Pages in Ordnung sind:
      #define CHECK_GC_CONSISTENCY_2()  check_gc_consistency_2()
      local void check_gc_consistency_2 (void);
      local void check_gc_consistency_2()
        { for_each_page(page,
            if ((sintL)page->page_room < 0)
              { asciz_out("\nPage bei Adresse 0x"); hex_out(page); asciz_out(" übergelaufen!!\n"); abort(); }
            if (!(page->page_end + page->page_room
                  - (page->page_start - round_up((aint)page+sizeof(NODE),Varobject_alignment))
                  == round_down(page->m_start + page->m_length,Varobject_alignment)
               ) )
              { asciz_out("\nPage bei Adresse 0x"); hex_out(page); asciz_out(" inkonsistent!!\n"); abort(); }
            );
        }
  #else
    #define CHECK_AVL_CONSISTENCY()
    #define CHECK_GC_CONSISTENCY()
    #define CHECK_GC_CONSISTENCY_2()
  #endif
  #ifdef DEBUG_SPVW
    # Überprüfen, ob die Tabellen der Packages halbwegs in Ordnung sind:
      #define CHECK_PACK_CONSISTENCY()  check_pack_consistency()
      local void check_pack_consistency (void);
      local void check_pack_consistency()
        { var reg5 object plist = O(all_packages);
          while (consp(plist))
            { var reg7 object pack = Car(plist);
              var reg6 object symtab = ThePackage(pack)->pack_external_symbols;
              var reg4 object table = TheSvector(symtab)->data[1];
              var reg3 uintL index = TheSvector(table)->length;
              until (index==0)
                {var reg1 object entry = TheSvector(table)->data[--index];
                 var reg2 uintC count = 0;
                 while (consp(entry))
                   { entry = Cdr(entry); count++; if (count>=1000) abort(); }
                }
              plist = Cdr(plist);
        }   }
  #else
      #define CHECK_PACK_CONSISTENCY()
  #endif

# ------------------------------------------------------------------------------
#                       Speichergröße

# Liefert die Größe des von den LISP-Objekten belegten Platzes.
  global uintL used_space (void);
  #ifdef SPVW_MIXED_BLOCKS
    #define Heap_used_space(h)  ((uintL)((h).end - (h).start))
    global uintL used_space()
      { return Heap_used_space(mem.objects) # Platz für Objekte variabler Länge
               + Heap_used_space(mem.conses); # Platz für Conses
      }
  #else
    global uintL used_space()
      { var reg4 uintL sum = 0;
        for_each_page(page, { sum += page->page_end - page->page_start; } );
        return sum;
      }
  #endif

# Liefert die Größe des für LISP-Objekte noch verfügbaren Platzes.
  global uintL free_space (void);
  #ifdef SPVW_MIXED_BLOCKS
    global uintL free_space()
      { return (mem.conses.start-mem.objects.end); } # Platz in der großen Lücke
  #endif
  #ifdef SPVW_PURE_BLOCKS
    global uintL free_space()
      { return mem.total_room; } # Platz, der bis zur nächsten GC verbraucht werden darf
  #endif
  #ifdef SPVW_PAGES
    global uintL free_space()
      { var reg4 uintL sum = 0;
        for_each_page(page, { sum += page->page_room; } );
        return sum;
      }
  #endif

# ------------------------------------------------------------------------------
#                   Speicherlängenbestimmung

# Bei allen Objekten variabler Länge (die von links nach rechts wachsen)
# steht (außer während der GC) in den ersten 4 Bytes ein Pointer auf sich
# selbst, bei Symbolen auch noch die Flags.

# Liefert den Typcode eines Objekts variabler Länge an einer gegebenen Adresse:
  #define typecode_at(addr)  mtypecode(((Varobject)(addr))->GCself)
  # oder (äquivalent):
  # define typecode_at(addr)  (((Varobject)(addr))->header_flags)
# Fallunterscheidungen nach diesem müssen statt 'case_symbol:' ein
# 'case_symbolwithflags:' enthalten.
  #define case_symbolwithflags  \
    case symbol_type:                                        \
    case symbol_type|bit(constant_bit_t):                    \
    case symbol_type|bit(keyword_bit_t)|bit(constant_bit_t): \
    case symbol_type|bit(special_bit_t):                     \
    case symbol_type|bit(special_bit_t)|bit(constant_bit_t): \
    case symbol_type|bit(special_bit_t)|bit(keyword_bit_t)|bit(constant_bit_t)

# UP, bestimmt die Länge eines LISP-Objektes variabler Länge (in Bytes).
# (Sie ist durch Varobject_alignment teilbar.)
  local uintL speicher_laenge (void* addr);
  # Varobject_aligned_size(HS,ES,C) liefert die Länge eines Objekts variabler
  # Länge mit HS=Header-Size, ES=Element-Size, C=Element-Count.
  # Varobject_aligned_size(HS,ES,C) = round_up(HS+ES*C,Varobject_alignment) .
    #define Varobject_aligned_size(HS,ES,C)  \
      ((ES % Varobject_alignment) == 0               \
       ? # ES ist durch Varobject_alignment teilbar  \
         round_up(HS,Varobject_alignment) + (ES)*(C) \
       : round_up((HS)+(ES)*(C),Varobject_alignment) \
      )
  # Länge eines Objekts, je nach Typ:
    #define size_symbol()  # Symbol \
      round_up( sizeof(symbol_), Varobject_alignment)
    #define size_sbvector(length)  # simple-bit-vector \
      ( ceiling( (uintL)(length) + 8*offsetof(sbvector_,data), 8*Varobject_alignment ) \
        * Varobject_alignment                                                          \
      )
    #define size_sstring(length)  # simple-string \
      round_up( (uintL)(length) + offsetof(sstring_,data), Varobject_alignment)
    #define size_svector(length)  # simple-vector \
      Varobject_aligned_size(offsetof(svector_,data),sizeof(object),(uintL)(length))
    #define size_array(size)  # Nicht-simpler Array, mit \
      # size = Dimensionszahl + (1 falls Fill-Pointer) + (1 falls Displaced-Offset) \
      Varobject_aligned_size(offsetof(array_,dims),sizeof(uintL),(uintL)(size))
    #define size_record(length)  # Record \
      Varobject_aligned_size(offsetof(record_,recdata),sizeof(object),(uintL)(length))
    #define size_bignum(length)  # Bignum \
      Varobject_aligned_size(offsetof(bignum_,data),sizeof(uintD),(uintL)(length))
    #ifndef WIDE
    #define size_ffloat()  # Single-Float \
      round_up( sizeof(ffloat_), Varobject_alignment)
    #endif
    #define size_dfloat()  # Double-Float \
      round_up( sizeof(dfloat_), Varobject_alignment)
    #define size_lfloat(length)  # Long-Float \
      Varobject_aligned_size(offsetof(lfloat_,data),sizeof(uintD),(uintL)(length))

#ifdef SPVW_MIXED

  local uintL speicher_laenge (addr)
    var reg2 void* addr;
    { switch (typecode_at(addr) & ~bit(garcol_bit_t)) # Typ des Objekts
        { case_symbolwithflags: # Symbol
            return size_symbol();
          case_sbvector: # simple-bit-vector
            return size_sbvector(((Sbvector)addr)->length);
          case_sstring: # simple-string
            return size_sstring(((Sstring)addr)->length);
          case_svector: # simple-vector
            return size_svector(((Svector)addr)->length);
          case_array1: case_obvector: case_ostring: case_ovector:
            # Nicht-simpler Array:
            { var reg2 uintL size;
              size = (uintL)(((Array)addr)->rank);
              if (((Array)addr)->flags & bit(arrayflags_fillp_bit)) { size += 1; }
              if (((Array)addr)->flags & bit(arrayflags_dispoffset_bit)) { size += 1; }
              # size = Dimensionszahl + (1 falls Fill-Pointer) + (1 falls Displaced-Offset)
              return size_array(size);
            }
          case_record: # Record
            return size_record(((Record)addr)->reclength);
          case_bignum: # Bignum
            return size_bignum(((Bignum)addr)->length);
          #ifndef WIDE
          case_ffloat: # Single-Float
            return size_ffloat();
          #endif
          case_dfloat: # Double-Float
            return size_dfloat();
          case_lfloat: # Long-Float
            return size_lfloat(((Lfloat)addr)->len);
          case_machine:
          case_char:
          case_subr:
          case_fsubr:
          case_system:
          case_fixnum:
          case_sfloat:
          #ifdef WIDE
          case_ffloat:
          #endif
            # Das sind direkte Objekte, keine Pointer.
          /* case_ratio: */
          /* case_complex: */
          default:
            # Das sind keine Objekte variabler Länge.
            /*NOTREACHED*/ abort();
    }   }

  #define var_speicher_laenge_
  #define calc_speicher_laenge(addr)  speicher_laenge((void*)(addr))

#endif # SPVW_MIXED

#ifdef SPVW_PURE

  # spezielle Funktionen für jeden Typ:
  inline local uintL speicher_laenge_symbol (addr) # Symbol
    var reg1 void* addr;
    { return size_symbol(); }
  inline local uintL speicher_laenge_sbvector (addr) # simple-bit-vector
    var reg1 void* addr;
    { return size_sbvector(((Sbvector)addr)->length); }
  inline local uintL speicher_laenge_sstring (addr) # simple-string
    var reg1 void* addr;
    { return size_sstring(((Sstring)addr)->length); }
  inline local uintL speicher_laenge_svector (addr) # simple-vector
    var reg1 void* addr;
    { return size_svector(((Svector)addr)->length); }
  inline local uintL speicher_laenge_array (addr) # nicht-simpler Array
    var reg1 void* addr;
    { var reg2 uintL size;
      size = (uintL)(((Array)addr)->rank);
      if (((Array)addr)->flags & bit(arrayflags_fillp_bit)) { size += 1; }
      if (((Array)addr)->flags & bit(arrayflags_dispoffset_bit)) { size += 1; }
      # size = Dimensionszahl + (1 falls Fill-Pointer) + (1 falls Displaced-Offset)
      return size_array(size);
    }
  inline local uintL speicher_laenge_record (addr) # Record
    var reg1 void* addr;
    { return size_record(((Record)addr)->reclength); }
  inline local uintL speicher_laenge_bignum (addr) # Bignum
    var reg1 void* addr;
    { return size_bignum(((Bignum)addr)->length); }
  #ifndef WIDE
  inline local uintL speicher_laenge_ffloat (addr) # Single-Float
    var reg1 void* addr;
    { return size_ffloat(); }
  #endif
  inline local uintL speicher_laenge_dfloat (addr) # Double-Float
    var reg1 void* addr;
    { return size_dfloat(); }
  inline local uintL speicher_laenge_lfloat (addr) # Long-Float
    var reg1 void* addr;
    { return size_lfloat(((Lfloat)addr)->len); }

  # Tabelle von Funktionen:
  typedef uintL (*speicher_laengen_fun) (void* addr);
  local speicher_laengen_fun speicher_laengen[heapcount];

  local void init_speicher_laengen (void)
    { var reg1 uintL heapnr;
      for (heapnr=0; heapnr<heapcount; heapnr++)
        { switch (heapnr)
            { case_symbol:
                speicher_laengen[heapnr] = &speicher_laenge_symbol; break;
              case_sbvector:
                speicher_laengen[heapnr] = &speicher_laenge_sbvector; break;
              case_sstring:
                speicher_laengen[heapnr] = &speicher_laenge_sstring; break;
              case_svector:
                speicher_laengen[heapnr] = &speicher_laenge_svector; break;
              case_array1: case_obvector: case_ostring: case_ovector:
                speicher_laengen[heapnr] = &speicher_laenge_array; break;
              case_record:
                speicher_laengen[heapnr] = &speicher_laenge_record; break;
              case_bignum:
                speicher_laengen[heapnr] = &speicher_laenge_bignum; break;
              #ifndef WIDE
              case_ffloat:
                speicher_laengen[heapnr] = &speicher_laenge_ffloat; break;
              #endif
              case_dfloat:
                speicher_laengen[heapnr] = &speicher_laenge_dfloat; break;
              case_lfloat:
                speicher_laengen[heapnr] = &speicher_laenge_lfloat; break;
              case_machine:
              case_char:
              case_subr:
              case_fsubr:
              case_system:
              case_fixnum:
              case_sfloat:
              #ifdef WIDE
              case_ffloat:
              #endif
                # Das sind direkte Objekte, keine Pointer.
              /* case_ratio: */
              /* case_complex: */
              default:
                # Das sind keine Objekte variabler Länge.
                speicher_laengen[heapnr] = &abort; break;
    }   }   }

  #define var_speicher_laenge_  \
    var reg5 speicher_laengen_fun speicher_laenge_ = speicher_laengen[heapnr];
  #define calc_speicher_laenge(addr)  (*speicher_laenge_)((void*)(addr))

#endif # SPVW_PURE

# ------------------------------------------------------------------------------
#                       Garbage-Collector

# Gesamtstrategie:
# 1. Pseudorekursives Markieren durch Setzen von garcol_bit.
# 2. Verschieben der Objekte fester Länge (Conses u.ä.),
#    Durchrechnen der Verschiebungen der Objekte variabler Länge.
# 3. Aktualisieren der Pointer.
# 4. Durchführen der Verschiebungen der Objekte variabler Länge.

# Markierungs-Unterprogramm
  # Verfahren: Markierungsroutine ohne Stackbenutzung (d.h.
  #  nicht "rekursiv") durch Abstieg in die zu markierende
  #  Struktur mit Pointermodifikation (Pointer werden umgedreht,
  #  damit sie als "Ariadnefaden" zurück dienen können)
  # Konvention: ein Objekt X gilt als markiert, wenn
  #  - ein Objekt variabler Länge: Bit garcol_bit,(X) gesetzt
  #  - ein Zwei-Pointer-Objekt: Bit garcol_bit,(X) gesetzt
  #  - ein SUBR/FSUBR: Bit garcol_bit,(X+const_offset) gesetzt
  #  - Character, Short-Float, Fixnum etc.: stets.
  local void gc_mark (object obj);
  # Markierungsbit an einer Adresse setzen: mark(addr);
    #define mark(addr)  *(oint*)(addr) |= wbit(garcol_bit_o)
  # Markierungsbit an einer Adresse setzen: unmark(addr);
    #define unmark(addr)  *(oint*)(addr) &= ~wbit(garcol_bit_o)
  # Markierungsbit an einer Adresse abfragen: if (marked(addr)) ...
    #ifdef fast_mtypecode
      #define marked(addr)  (mtypecode(*(object*)(addr)) & bit(garcol_bit_t))
    #else
      #if !(garcol_bit_o == 32-1) || defined(WIDE)
        #define marked(addr)  (*(oint*)(addr) & wbit(garcol_bit_o))
      #else # garcol_bit_o = 32-1 = Vorzeichenbit
        #define marked(addr)  (*(sintL*)(addr) < 0)
      #endif
    #endif
  # Markierungsbit in einem Objekt setzen:
    #define with_mark_bit(obj)  ((object)((oint)(obj) | wbit(garcol_bit_o)))
  # Markierungsbit in einem Objekt löschen:
    #define without_mark_bit(obj)  ((object)((oint)(obj) & ~wbit(garcol_bit_o)))
  local void gc_mark (obj)
    var reg4 object obj;
    { var reg2 object dies = obj; # aktuelles Objekt
      var reg3 object vorg = nullobj; # Vorgänger-Objekt
      down: # Einsprung für Abstieg.
            # dies = zu markierendes Objekt, vorg = sein Vorgänger
            switch (typecode(dies))
              { case_cons:
                case_ratio:
                case_complex:
                  # Objekt mit genau 2 Pointern (Cons u.ä.)
                  { var reg1 oint* dies_ = (oint*)ThePointer(dies);
                    if (marked(dies_)) goto up; # markiert -> hoch
                    mark(dies_); # markieren
                  }
                  { var reg1 object dies_ = objectplus(dies,(soint)(sizeof(cons_)-sizeof(object))<<(oint_addr_shift-addr_shift));
                                          # mit dem letzten Pointer anfangen
                    var reg1 object nachf = *(object*)ThePointer(dies_); # Nachfolger
                    *(object*)ThePointer(dies_) = vorg; # Vorgänger eintragen
                    vorg = dies_; # aktuelles Objekt wird neuer Vorgänger
                    dies = nachf; # Nachfolger wird aktuelles Objekt
                    goto down; # und absteigen
                  }
                case_symbol: # Symbol
                  { var reg1 oint* dies_ = (oint*)(TheSymbol(dies));
                    if (marked(dies_)) goto up; # markiert -> hoch
                    mark(dies_); # markieren
                    mark(pointerplus(dies_,symbol_objects_offset)); # ersten Pointer markieren
                  }
                  { var reg1 object dies_ = objectplus(dies,(soint)(sizeof(symbol_)-sizeof(object))<<(oint_addr_shift-addr_shift));
                                          # mit dem letzten Pointer anfangen
                    var reg1 object nachf = *(object*)(TheSymbol(dies_)); # Nachfolger
                    *(object*)(TheSymbol(dies_)) = vorg; # Vorgänger eintragen
                    vorg = dies_; # aktuelles Objekt wird neuer Vorgänger
                    dies = nachf; # Nachfolger wird aktuelles Objekt
                    goto down; # und absteigen
                  }
                case_sbvector: # simple-bit-vector
                case_sstring: # simple-string
                case_bignum: # Bignum
                #ifndef WIDE
                case_ffloat: # Single-Float
                #endif
                case_dfloat: # Double-Float
                case_lfloat: # Long-Float
                  # Objekte variabler Länge, die keine Pointer enthalten:
                  mark(TheVarobject(dies)); # markieren
                  goto up; # und hoch
                case_array1: case_obvector: case_ostring: case_ovector:
                  # Arrays, die nicht simple sind:
                  { var reg1 oint* dies_ = (oint*)TheVarobject(dies);
                    if (marked(dies_)) goto up; # markiert -> hoch
                    mark(dies_); # markieren
                  }
                  { var reg1 object dies_ = objectplus(dies,(soint)(array_data_offset)<<(oint_addr_shift-addr_shift));
                                          # Datenvektor ist der erste und einzige Pointer
                    var reg1 object nachf = *(object*)TheVarobject(dies_); # Nachfolger
                    *(object*)TheVarobject(dies_) = vorg; # Vorgänger eintragen
                    mark(TheVarobject(dies_)); # ersten und einzigen Pointer markieren
                    vorg = dies_; # aktuelles Objekt wird neuer Vorgänger
                    dies = nachf; # Nachfolger wird aktuelles Objekt
                    goto down; # und absteigen
                  }
                case_svector: # simple-vector
                  { var reg1 oint* dies_ = (oint*)TheSvector(dies);
                    if (marked(dies_)) goto up; # markiert -> hoch
                    mark(dies_); # markieren
                  }
                  { var reg1 uintL len = TheSvector(dies)->length;
                    if (len==0) goto up; # Länge 0: wieder hoch
                   {var reg1 object dies_ = objectplus(dies,
                                              ((soint)offsetof(svector_,data) << (oint_addr_shift-addr_shift))
                                              + (len * (soint)sizeof(object) << (oint_addr_shift-addr_shift))
                                              - ((soint)sizeof(object) << (oint_addr_shift-addr_shift)) );
                                              # mit dem letzten Pointer anfangen
                    var reg1 object nachf = *(object*)TheSvector(dies_); # Nachfolger
                    *(object*)TheSvector(dies_) = vorg; # Vorgänger eintragen
                    mark(&TheSvector(dies)->data[0]); # ersten Pointer markieren
                    vorg = dies_; # aktuelles Objekt wird neuer Vorgänger
                    dies = nachf; # Nachfolger wird aktuelles Objekt
                    goto down; # und absteigen
                  }}
                case_record:
                  # Record:
                  { var reg1 oint* dies_ = (oint*)TheRecord(dies);
                    if (marked(dies_)) goto up; # markiert -> hoch
                    mark(dies_); # markieren
                  }
                  { var reg1 uintL len = TheRecord(dies)->reclength;
                    # Länge stets >0
                    var reg1 object dies_ = objectplus(dies,
                                              ((soint)offsetof(record_,recdata) << (oint_addr_shift-addr_shift))
                                            + (len * (soint)sizeof(object) << (oint_addr_shift-addr_shift))
                                            - ((soint)sizeof(object) << (oint_addr_shift-addr_shift)) );
                                            # mit dem letzten Pointer anfangen
                    var reg1 object nachf = *(object*)TheRecord(dies_); # Nachfolger
                    *(object*)TheRecord(dies_) = vorg; # Vorgänger eintragen
                    mark(&TheRecord(dies)->recdata[0]); # ersten Pointer markieren
                    vorg = dies_; # aktuelles Objekt wird neuer Vorgänger
                    dies = nachf; # Nachfolger wird aktuelles Objekt
                    goto down; # und absteigen
                  }
                case_machine: # Maschinenadresse
                case_char: # Character
                case_system: # Frame-Pointer, Read-Label, System
                case_fixnum: # Fixnum
                case_sfloat: # Short-Float
                #ifdef WIDE
                case_ffloat: # Single-Float
                #endif
                  # Das sind direkte Objekte, keine Pointer.
                  goto up;
                case_subr: # SUBR
                  { var reg1 oint* dies_ = (oint*)pointerplus(TheSubr(dies),subr_const_offset);
                    if (marked(dies_)) goto up; # markiert -> hoch
                    # markieren später
                  }
                  { var reg1 object dies_ = objectplus(dies,
                                              (soint)(subr_const_offset+(subr_const_anz-1)*sizeof(object))<<(oint_addr_shift-addr_shift));
                                              # mit dem letzten Pointer anfangen
                    var reg1 object nachf = *(object*)TheSubr(dies_); # Nachfolger
                    *(object*)TheSubr(dies_) = vorg; # Vorgänger eintragen
                    # ersten Pointer (und damit das SUBR selbst) markieren:
                    mark(pointerplus(TheSubr(dies),subr_const_offset));
                    vorg = dies_; # aktuelles Objekt wird neuer Vorgänger
                    dies = nachf; # Nachfolger wird aktuelles Objekt
                    goto down; # und absteigen
                  }
                case_fsubr: # FSUBR
                  { var reg1 oint* dies_ = (oint*)pointerplus(TheFsubr(dies),fsubr_const_offset);
                    if (marked(dies_)) goto up; # markiert -> hoch
                    # markieren später
                  }
                  { var reg1 object dies_ = objectplus(dies,
                                              (soint)(fsubr_const_offset+(fsubr_const_anz-1)*sizeof(object))<<(oint_addr_shift-addr_shift));
                                              # mit dem letzten Pointer anfangen
                    var reg1 object nachf = *(object*)TheFsubr(dies_); # Nachfolger
                    *(object*)TheFsubr(dies_) = vorg; # Vorgänger eintragen
                    # ersten Pointer (und damit das FSUBR selbst) markieren:
                    mark(pointerplus(TheFsubr(dies),fsubr_const_offset));
                    vorg = dies_; # aktuelles Objekt wird neuer Vorgänger
                    dies = nachf; # Nachfolger wird aktuelles Objekt
                    goto down; # und absteigen
                  }
                default:
                  # Das sind keine Objekte.
                  /*NOTREACHED*/ abort();
              }
      up:   # Einsprung zum Aufstieg.
            # dies = gerade markiertes Objekt, vorg = sein Vorgänger
            if (vorg==nullobj) # Endekennzeichen erreicht?
              return; # ja -> fertig
            if (!marked(ThePointer(vorg))) # schon durch?
              # nein ->
              # nächstes Element weiter links (Komme von up, gehe nach down)
              # dies = gerade markiertes Objekt, in *vorg einzutragen
              { var reg3 object vorvorg = *(object*)ThePointer(vorg); # alter Vorgänger
                *(object*)ThePointer(vorg) = dies; # Komponente zurückschreiben
                vorg = objectplus(vorg,-(soint)(sizeof(object))<<(oint_addr_shift-addr_shift)); # zur nächsten Komponente
                if (marked(ThePointer(vorg))) # dort schon markiert?
                  { dies = # nächste Komponente, ohne Markierung
                           without_mark_bit(*(object*)ThePointer(vorg));
                    *(object*)ThePointer(vorg) = # alten Vorgänger weiterschieben, dabei Markierung erneuern
                           with_mark_bit(vorvorg);
                  }
                  else
                  { dies = *(object*)ThePointer(vorg); # nächste Komponente, ohne Markierung
                    *(object*)ThePointer(vorg) = vorvorg; # alten Vorgänger weiterschieben
                  }
                goto down;
              }
            # schon durch -> wieder aufsteigen
            { var reg3 object vorvorg = # alten Vorgänger holen, ohne Markierungsbit
                                        without_mark_bit(*(object*)ThePointer(vorg));
              *(object*)ThePointer(vorg) = dies; # erste Komponente zurückschreiben
              switch (typecode(vorg))
                { case_cons:
                  case_ratio:
                  case_complex:
                    # Objekt mit genau 2 Pointern (Cons u.ä.)
                    { mark(ThePointer(vorg)); # wieder markieren
                      dies = vorg; # Cons wird aktuelles Objekt
                      vorg = vorvorg; goto up; # weiter aufsteigen
                    }
                  case_symbol:
                    # Symbol
                    { dies = objectplus(vorg,-(soint)symbol_objects_offset<<(oint_addr_shift-addr_shift)); # Symbol wird aktuelles Objekt
                      vorg = vorvorg; goto up; # weiter aufsteigen
                    }
                  case_svector:
                    # simple-vector mit mindestens 1 Komponente
                    { dies = objectplus(vorg,-(soint)offsetof(svector_,data)<<(oint_addr_shift-addr_shift)); # Svector wird aktuelles Objekt
                      vorg = vorvorg; goto up; # weiter aufsteigen
                    }
                  case_array1: case_obvector: case_ostring: case_ovector:
                    # Nicht-simple Arrays:
                    { dies = objectplus(vorg,-(soint)array_data_offset<<(oint_addr_shift-addr_shift)); # Array wird aktuelles Objekt
                      vorg = vorvorg; goto up; # weiter aufsteigen
                    }
                  case_record:
                    # Record:
                    { dies = objectplus(vorg,-(soint)offsetof(record_,recdata)<<(oint_addr_shift-addr_shift)); # Record wird aktuelles Objekt
                      vorg = vorvorg; goto up; # weiter aufsteigen
                    }
                  case_subr: # SUBR
                    { mark(TheSubr(vorg)); # wieder markieren
                      dies = objectplus(vorg,-(soint)subr_const_offset<<(oint_addr_shift-addr_shift)); # SUBR wird aktuelles Objekt
                      vorg = vorvorg; goto up; # weiter aufsteigen
                    }
                  case_fsubr: # FSUBR
                    { mark(TheFsubr(vorg)); # wieder markieren
                      dies = objectplus(vorg,-(soint)fsubr_const_offset<<(oint_addr_shift-addr_shift)); # FSUBR wird aktuelles Objekt
                      vorg = vorvorg; goto up; # weiter aufsteigen
                    }
                  case_machine: # Maschinenadresse
                  case_char: # Character
                  case_system: # Frame-Pointer, Read-Label, System
                  case_fixnum: # Fixnum
                  case_sfloat: # Short-Float
                  #ifdef WIDE
                  case_ffloat: # Single-Float
                  #endif
                    # Das sind direkte Objekte, keine Pointer.
                  case_sbvector: # simple-bit-vector
                  case_sstring: # simple-string
                  case_bignum: # Bignum
                  #ifndef WIDE
                  case_ffloat: # Single-Float
                  #endif
                  case_dfloat: # Double-Float
                  case_lfloat: # Long-Float
                    # Objekte variabler Länge, die keine Pointer enthalten.
                  default:
                    # Das sind keine Objekte.
                    /*NOTREACHED*/ abort();
    }       }   }

# Markierungsphase:
  # Es werden alle "aktiven" Strukturen markiert.
  # Aktiv ist alles, was erreichbar ist
  # - vom LISP-Stack aus  oder
  # - als Programmkonstanten (dazu gehört auch die Liste aller Packages).
  local void gc_markphase (void);
  local void gc_markphase()
    { { var reg1 object* objptr = &STACK_0; # Pointer, der durch den STACK läuft
        until (*objptr==nullobj) # bis STACK zu Ende ist:
          { if ( *((oint*)objptr) & wbit(frame_bit_o) ) # Beginnt hier ein Frame?
             { if (( *((oint*)objptr) & wbit(skip2_bit_o) ) == 0) # Ohne skip2-Bit?
                objptr skipSTACKop 2; # ja -> um 2 weiterrücken
                else
                objptr skipSTACKop 1; # nein -> um 1 weiterrücken
             }
             else
             { # normales Objekt, markieren:
               var reg2 object obj = *objptr;
               switch (typecode(obj)) # evtl. Symbol-Flags entfernen
                 { case_symbolflagged: obj = symbol_without_flags(obj);
                   default: break;
                 }
               gc_mark(obj);
               objptr skipSTACKop 1; # weiterrücken
      }   }  }
      # alle Programmkonstanten markieren:
      { var reg1 fsubr_* ptr = (fsubr_*)&fsubr_tab; # fsubr_tab durchgehen
        var reg2 uintC count;
        dotimesC(count,fsubr_anz,
          { gc_mark(fsubr_tab_ptr_as_fsubr(ptr)); ptr++; });
      }
      { var reg1 subr_* ptr = (subr_*)&subr_tab; # subr_tab durchgehen
        var reg2 uintC count;
        dotimesC(count,subr_anz,
          { gc_mark(subr_tab_ptr_as_subr(ptr)); ptr++; });
      }
      { var reg1 symbol_* ptr = (symbol_*)&symbol_tab; # symbol_tab durchgehen
        var reg2 uintC count;
        dotimesC(count,symbol_anz,
          { gc_mark(symbol_tab_ptr_as_symbol(ptr)); ptr++; });
      }
      { var reg1 object* objptr = (object*)&object_tab; # object_tab durchgehen
        var reg2 uintC count;
        dotimesC(count,object_anz, { gc_mark(*objptr++); });
      }
    }

#ifdef SPVW_MIXED_BLOCKS

  # CONS-Zellen zwischen page->page_start und page->page_end oben
  # konzentrieren:
  local void gc_compact_cons_page (Page* page);
  local void gc_compact_cons_page(page)
    var reg3 Page* page;
    # Dabei wandert der Pointer p1 von unten und der Pointer p2 von
    # oben durch den Speicherbereich, bis sie kollidieren. Es
    # werden dabei markierte Strukturen über unmarkierte geschoben.
    { var reg1 aint p1 = page->page_start; # untere Grenze
      var reg2 aint p2 = page->page_end; # obere Grenze
      sweeploop:
        # Suche nächstobere unmarkierte Zelle <p2 und demarkiere dabei alle:
        sweeploop1:
          if (p1==p2) goto sweepok2; # Grenzen gleich geworden -> fertig
          p2 -= sizeof(cons_); # nächste Zelle von oben erfassen
          if (marked(p2)) # markiert?
            { unmark(p2); # demarkieren
              goto sweeploop1;
            }
        # p1 <= p2, p2 zeigt auf eine unmarkierte Zelle.
        # Suche nächstuntere markierte Zelle >=p1:
        sweeploop2:
          if (p1==p2) goto sweepok1; # Grenzen gleich geworden -> fertig
          if (!marked(p1)) # unmarkiert?
            { p1 += sizeof(cons_); # bei der nächstunteren Zelle
              goto sweeploop2; # weitersuchen
            }
        # p1 < p2, p1 zeigt auf eine markierte Zelle.
        unmark(p1); # demarkieren
        # Zelleninhalt in die unmarkierte Zelle kopieren:
        ((object*)p2)[0] = ((object*)p1)[0];
        ((object*)p2)[1] = ((object*)p1)[1];
        *(object*)p1 = type_pointer_object(0,p2); # neue Adresse hinterlassen
        mark(p1); # und markieren (als Erkennung fürs Aktualisieren)
        p1 += sizeof(cons_); # Diese Zelle ist fertig.
        goto sweeploop; # weiter
      sweepok1: p1 += sizeof(cons_); # letztes unmarkiertes Cons übergehen
      sweepok2:
      # p1 = neue untere Grenze des Cons-Bereiches
      page->page_start = p1;
    }

#else

  # CONS-Zellen zwischen page->page_start und page->page_end unten
  # konzentrieren:
  local void gc_compact_cons_page (Page* page);
  local void gc_compact_cons_page(page)
    var reg3 Page* page;
    # Dabei wandert der Pointer p1 von unten und der Pointer p2 von
    # oben durch den Speicherbereich, bis sie kollidieren. Es
    # werden dabei markierte Strukturen über unmarkierte geschoben.
    { var reg1 aint p1 = page->page_start; # untere Grenze
      var reg2 aint p2 = page->page_end; # obere Grenze
      sweeploop:
        # Suche nächstobere markierte Zelle <p2:
        sweeploop1:
          if (p1==p2) goto sweepok2; # Grenzen gleich geworden -> fertig
          p2 -= sizeof(cons_); # nächste Zelle von oben erfassen
          if (!marked(p2)) goto sweeploop1; # unmarkiert?
        # p1 <= p2, p2 zeigt auf eine markierte Zelle.
        unmark(p2); # demarkieren
        # Suche nächstuntere unmarkierte Zelle >=p1 und demarkiere dabei alle:
        sweeploop2:
          if (p1==p2) goto sweepok1; # Grenzen gleich geworden -> fertig
          if (marked(p1)) # markiert?
            { unmark(p1); # demarkieren
              p1 += sizeof(cons_); # bei der nächstoberen Zelle
              goto sweeploop2; # weitersuchen
            }
        # p1 < p2, p1 zeigt auf eine unmarkierte Zelle.
        # Zelleninhalt von der markierten in die unmarkierte Zelle kopieren:
        ((object*)p1)[0] = ((object*)p2)[0];
        ((object*)p1)[1] = ((object*)p2)[1];
        *(object*)p2 = type_pointer_object(0,p1); # neue Adresse hinterlassen
        mark(p2); # und markieren (als Erkennung fürs Aktualisieren)
        p1 += sizeof(cons_); # Diese Zelle ist fertig.
        goto sweeploop; # weiter
      sweepok1: p1 += sizeof(cons_); # letztes markiertes Cons übergehen
      sweepok2:
      # p1 = neue obere Grenze des Cons-Bereiches
      page->page_end = p1;
    }

#endif

# SUBRs/FSUBRs und feste Symbole demarkieren:
  local void unmark_fixed_varobjects (void);
  local void unmark_fixed_varobjects()
    { { var reg1 fsubr_* ptr = (fsubr_*)&fsubr_tab; # fsubr_tab durchgehen
        var reg2 uintC count;
        dotimesC(count,fsubr_anz,
          { unmark((aint)ptr+fsubr_const_offset); # Fsubr demarkieren
            ptr++;
          });
      }
      { var reg1 subr_* ptr = (subr_*)&subr_tab; # subr_tab durchgehen
        var reg2 uintC count;
        dotimesC(count,subr_anz,
          { unmark((aint)ptr+subr_const_offset); # Subr demarkieren
            ptr++;
          });
      }
      { var reg1 symbol_* ptr = (symbol_*)&symbol_tab; # symbol_tab durchgehen
        var reg2 uintC count;
        dotimesC(count,symbol_anz,
          { unmark(&((Symbol)ptr)->GCself); # Symbol demarkieren
            ptr++;
          });
      }
    }

# Den Selbstpointer eines Objekts variabler Länge modifizieren:
# set_GCself(p,type,addr);
# setzt p->GCself auf type_pointer_object(type,addr).
  #if !(exact_uint_size_p(oint_type_len) && (tint_type_mask == bit(oint_type_len)-1))
    #define set_GCself(p,type,addr)  \
      ((Varobject)(p))->GCself = type_pointer_object((type)&tint_type_mask,addr)
  #else # besser: zwar zwei Speicherzugriffe, jedoch weniger Arithmetik
    #define set_GCself(p,type,addr)  \
      ((Varobject)(p))->GCself = type_pointer_object(0,addr), \
      ((Varobject)(p))->header_flags = (type)
  #endif

# Objekte variabler Länge zwischen page->page_start und page->page_end zur
# Zusammenschiebung nach unten vorbereiten. Dabei wird in jedes markierte
# Objekt vorne der Pointer auf die Stelle eingetragen, wo das
# Objekt später stehen wird (samt Typinfo). Ist das darauffolgende
# Objekt unmarkiert, so wird in dessen erstem Pointer die Adresse
# des nächsten markierten Objekts eingetragen.
  #ifdef SPVW_PURE
  local void gc_sweep1_varobject_page (Page* page, uintL heapnr);
  local void gc_sweep1_varobject_page(page,heapnr)
    var reg6 Page* page;
    var reg7 uintL heapnr;
  #else
  local void gc_sweep1_varobject_page (Page* page);
  local void gc_sweep1_varobject_page(page)
    var reg6 Page* page;
  #endif
    { var reg4 object* last_open_ptr = &page->page_gcpriv.firstmarked;
        # In *last_open_ptr ist stets die Adresse des nächsten markierten
        # Objekts (als oint) einzutragen.
        # Durch verkettete-Liste-Mechanismus: Am Schluß enthält
        # page->gcpriv.firstmarked die Adresse des 1. markierten Objekts
      var reg2 aint p2 = page->page_start; # Source-Pointer
      var reg5 aint p2end = page->page_end; # obere Grenze des Source-Bereiches
      var reg3 aint p1 = p2; # Ziel-Pointer
      # start <= p1 <= p2 <= end, p1 und p2 wachsen, p2 schneller als p1.
      var_speicher_laenge_;
      sweeploop1:
        # Nächstes markiertes Objekt suchen.
        # Adresse des nächsten markierten Objekts in *last_open_ptr eintragen.
        if (p2==p2end) goto sweepok1; # obere Grenze erreicht -> fertig
        { var reg2 tint flags = ((Varobject)p2)->header_flags;
          # Typinfo (und Flags bei Symbolen) retten
          var reg1 uintL laenge = calc_speicher_laenge(p2); # Byte-Länge bestimmen
          if (!marked(p2)) # Objekt unmarkiert?
            { p2 += laenge; goto sweeploop1; } # ja -> zum nächsten Objekt
          # Objekt markiert
          *last_open_ptr = type_pointer_object(0,p2); # Adresse ablegen
          set_GCself(p2, flags,p1); # neue Adresse eintragen, mit alter
                         # Typinfo (darin ist auch das Markierungsbit enthalten)
          p2 += laenge; # Sourceadresse für nächstes Objekt
          p1 += laenge; # Zieladresse für nächstes Objekt
        }
      sweeploop2:
        # Nächstes unmarkiertes Objekt suchen.
        if (p2==p2end) goto sweepok2; # obere Grenze erreicht -> fertig
        { var reg2 tint flags = ((Varobject)p2)->header_flags;
          # Typinfo (und Flags bei Symbolen) retten
          var reg1 uintL laenge = calc_speicher_laenge(p2); # Byte-Länge bestimmen
          if (!marked(p2)) # Objekt unmarkiert?
            { last_open_ptr = (object*)p2; # ja -> Hier den nächsten Pointer ablegen
              p2 += laenge; goto sweeploop1; # und zum nächsten Objekt
            }
          # Objekt markiert
          set_GCself(p2, flags,p1); # neue Adresse eintragen, mit alter
                         # Typinfo (darin ist auch das Markierungsbit enthalten)
          p2 += laenge; # Sourceadresse für nächstes Objekt
          p1 += laenge; # Zieladresse für nächstes Objekt
          goto sweeploop2;
        }
      sweepok1: *last_open_ptr = type_pointer_object(0,p2);
      sweepok2: ;
    }

# Aktualisierungsphase:
  # Der gesamte LISP-Speicher wird durchgegangen und dabei alte durch
  # neue Adressen ersetzt.
  # Aktualisierung eines Objekts *objptr :
    #define aktualisiere(objptr)  \
      { var reg2 tint type = mtypecode(*(oint*)objptr);                   \
        if (!pointerless_type_p(type)) # unverschieblich -> nichts tun    \
          { var reg1 object obj = *(object*)objptr; # fragliches Objekt   \
            if (marked(ThePointer(obj))) # markiert?                      \
              # nein -> nichts zu tun (Objekt blieb stehen)               \
              # ja -> neue Adresse eintragen und Typinfobyte (incl.       \
              #       evtl. Symbol-Bindungsflags) zurückschreiben         \
              *(object*)objptr =                                          \
                type_untype_object(type,untype(*(oint*)ThePointer(obj))); \
      }   }
  # Durchlaufen durch alle LISP-Objekte und aktualisieren:
    # Pointer im LISP-Stack aktualisieren:
      local void aktualisiere_STACK (void);
      local void aktualisiere_STACK()
        { var reg3 object* objptr = &STACK_0; # Pointer, der durch den STACK läuft
          until (*objptr==nullobj) # bis STACK zu Ende ist:
            { if ( *((oint*)objptr) & wbit(frame_bit_o) ) # Beginnt hier ein Frame?
               { if (( *((oint*)objptr) & wbit(skip2_bit_o) ) == 0) # Ohne skip2-Bit?
                  objptr skipSTACKop 2; # ja -> um 2 weiterrücken
                  else
                  objptr skipSTACKop 1; # nein -> um 1 weiterrücken
               }
               else
               { # normales Objekt, aktualisieren:
                 switch (mtypecode(*objptr))
                   { case_symbolflagged: # Symbol mit evtl. Flags
                       { var reg6 object obj1 = *objptr;
                         var reg4 object obj2 = symbol_without_flags(obj1);
                         var reg5 oint flags = (oint)obj1 ^ (oint)obj2;
                         *objptr = obj2; # vorerst Flags löschen
                         aktualisiere(objptr); # dann aktualisieren
                         *(oint*)objptr |= flags; # dann Flags wieder rein
                         break;
                       }
                     default: aktualisiere(objptr); break;
                   }
                 objptr skipSTACKop 1; # weiterrücken
        }   }  }
    # Die folgenden Macros rufen den Macro aktualisiere() auf.
    # Programmkonstanten aktualisieren:
      #define aktualisiere_fsubr_tab()  \
        { var reg5 fsubr_* ptr = (fsubr_*)&fsubr_tab; # fsubr_tab durchgehen  \
          var reg6 uintC count;                                               \
          dotimesC(count,fsubr_anz,                                           \
            { { var reg3 object* p = (object*)((aint)ptr+fsubr_const_offset); \
                var reg4 uintC c;                                             \
                dotimespC(c,fsubr_const_anz, { aktualisiere(p); p++; } );     \
              }                                                               \
              ptr++;                                                          \
            });                                                               \
        }
      #define aktualisiere_subr_tab()  \
        { var reg5 subr_* ptr = (subr_*)&subr_tab; # subr_tab durchgehen     \
          var reg6 uintC count;                                              \
          dotimesC(count,subr_anz,                                           \
            { { var reg3 object* p = (object*)((aint)ptr+subr_const_offset); \
                var reg4 uintC c;                                            \
                dotimespC(c,subr_const_anz, { aktualisiere(p); p++; } );     \
              }                                                              \
              ptr++;                                                         \
            });                                                              \
        }
      #define aktualisiere_symbol_tab()  \
        { var reg4 symbol_* ptr = (symbol_*)&symbol_tab; # symbol_tab durchgehen \
          var reg5 uintC count;                         \
          dotimesC(count,symbol_anz,                    \
            { { var reg3 object* p;                     \
                p = &ptr->symvalue; aktualisiere(p);    \
                p = &ptr->symfunction; aktualisiere(p); \
                p = &ptr->proplist; aktualisiere(p);    \
                p = &ptr->pname; aktualisiere(p);       \
                p = &ptr->homepackage; aktualisiere(p); \
              }                                         \
              ptr++;                                    \
            });                                         \
        }
      #define aktualisiere_object_tab()  \
        { var reg3 object* objptr = (object*)&object_tab; # object_tab durchgehen    \
          var reg4 uintC count;                                             \
          dotimesC(count,object_anz, { aktualisiere(objptr); objptr++; } ); \
        }
      #define aktualisiere_tab()  \
        { aktualisiere_fsubr_tab();  \
          aktualisiere_subr_tab();   \
          aktualisiere_symbol_tab(); \
          aktualisiere_object_tab(); \
        }
    # Pointer in den Cons-Zellen aktualisieren:
      #define aktualisiere_conses()  \
        for_each_cons_page(page,                      \
          { var reg3 aint objptr = page->page_start;  \
            var reg4 aint objptrend = page->page_end; \
            # alle Pointer im (neuen) CONS-Bereich start <= Adresse < end aktualisieren: \
            until (objptr==objptrend)                 \
              { aktualisiere((object*)objptr);        \
                objptr += sizeof(object);             \
                aktualisiere((object*)objptr);        \
                objptr += sizeof(object);             \
          }   }                                       \
          );
    # Pointer in den Objekten variabler Länge aktualisieren:
    #   #define aktualisiere_page ...
    #   aktualisiere_varobjects();
    #   #undef aktualisiere_page
      #define aktualisiere_page_normal(page,aktualisierer)  \
        { var reg2 aint ptr = page->page_start;                        \
          var reg6 aint ptrend = page->page_end;                       \
          # alle Objekte mit Adresse >=ptr, <ptrend durchgehen:        \
          until (ptr==ptrend) # solange bis ptr am Ende angekommen ist \
            { # nächstes Objekt mit Adresse ptr (< ptrend) durchgehen: \
              aktualisierer(typecode_at(ptr)); # und weiterrücken      \
        }   }
      # aktualisiert das Objekt bei 'ptr', dessen Typcode durch 'type_expr'
      # gegeben wird, und rückt ptr weiter:
      #ifdef SPVW_MIXED
      #define aktualisiere_varobject(type_expr)  \
        { var reg5 tint type = (type_expr); # Typinfo                                           \
          var reg7 uintL laenge = calc_speicher_laenge(ptr); # Länge bestimmen                  \
          var reg8 aint newptr = ptr+laenge; # Zeiger auf nächstes Objekt                       \
          # Fallunterscheidung nach:                                                            \
            # Symbol; Simple-Vector; Nicht-simpler Array;                                       \
            # Record (insbes. Hash-Table); Rest.                                                \
          switch (type)                                                                         \
            { case_symbolwithflags:                                                             \
                # Symbol: alle Pointer innerhalb eines Symbols aktualisieren                    \
                { var reg3 object* p = (object*)pointerplus(ptr,symbol_objects_offset);         \
                  var reg4 uintC count;                                                         \
                  dotimespC(count,((sizeof(symbol_)-symbol_objects_offset)/sizeof(object)),     \
                    { aktualisiere(p); p++; } );                                                \
                }                                                                               \
                break;                                                                          \
              case_svector:                                                                     \
                # Simple-vector: alle Pointer innerhalb eines Simple-vector aktualisieren       \
                { var reg3 uintL count = ((Svector)ptr)->length;                                \
                  if (!(count==0))                                                              \
                    {var reg4 object* p = &((Svector)ptr)->data[0];                             \
                     dotimespL(count,count, { aktualisiere(p); p++; } );                        \
                }   }                                                                           \
                break;                                                                          \
              case_array1: case_obvector: case_ostring: case_ovector:                           \
                # nicht-simpler Array: Datenvektor aktualisieren                                \
                { var reg3 object* p = &((Array)ptr)->data;                                     \
                  aktualisiere(p);                                                              \
                }                                                                               \
                break;                                                                          \
              case_record:                                                                      \
                # Record: alle Pointer innerhalb eines Record aktualisieren                     \
                { # Beim Aktualisieren von Pointern verliert der Aufbau von                     \
                  # Hash-Tables seine Gültigkeit (denn die Hashfunktion eines                   \
                  # Objekts hängt von seiner Adresse ab, die sich ja jetzt                      \
                  # verändert).                                                                 \
                  if ((sintB)(((Record)ptr)->rectype) < 0) # eine Hash-Table ?                  \
                    { ((Record)ptr)->recflags |= bit(7); } # ja -> für Reorganisation vormerken \
                 {var reg3 uintC count;                                                         \
                  var reg4 object* p = &((Record)ptr)->recdata[0];                              \
                  dotimespC(count,((Record)ptr)->reclength, { aktualisiere(p); p++; } );        \
                }}                                                                              \
                break;                                                                          \
              default:                                                                          \
                break; # alle anderen enthalten keine zu aktualisierenden Pointer               \
                       # -> nichts tun                                                          \
            }                                                                                   \
          # zum nächsten Objekt weiterrücken                                                    \
          ptr=newptr;                                                                           \
        }
      #define aktualisiere_varobjects()  \
        for_each_varobject_page(page,                    \
          aktualisiere_page(page,aktualisiere_varobject) \
          );
      #endif
      #ifdef SPVW_PURE
      #define aktualisiere_symbol(type_expr)  # ignoriert type_expr \
        { var reg7 uintL laenge = speicher_laenge_symbol((void*)ptr); # Länge bestimmen \
          var reg8 aint newptr = ptr+laenge; # Zeiger auf nächstes Objekt               \
          # Symbol: alle Pointer innerhalb eines Symbols aktualisieren                  \
          { var reg3 object* p = (object*)pointerplus(ptr,symbol_objects_offset);       \
            var reg4 uintC count;                                                       \
            dotimespC(count,((sizeof(symbol_)-symbol_objects_offset)/sizeof(object)),   \
              { aktualisiere(p); p++; } );                                              \
          }                                                                             \
          ptr=newptr; # zum nächsten Objekt weiterrücken                                \
        }
      #define aktualisiere_svector(type_expr)  # ignoriert type_expr \
        { var reg7 uintL laenge = speicher_laenge_svector((void*)ptr); # Länge bestimmen \
          var reg8 aint newptr = ptr+laenge; # Zeiger auf nächstes Objekt                \
          # Simple-vector: alle Pointer innerhalb eines Simple-vector aktualisieren      \
          { var reg3 uintL count = ((Svector)ptr)->length;                               \
            if (!(count==0))                                                             \
              {var reg4 object* p = &((Svector)ptr)->data[0];                            \
               dotimespL(count,count, { aktualisiere(p); p++; } );                       \
          }   }                                                                          \
          ptr=newptr; # zum nächsten Objekt weiterrücken                                 \
        }
      #define aktualisiere_array(type_expr)  # ignoriert type_expr \
        { var reg7 uintL laenge = speicher_laenge_array((void*)ptr); # Länge bestimmen \
          var reg8 aint newptr = ptr+laenge; # Zeiger auf nächstes Objekt              \
          # nicht-simpler Array: Datenvektor aktualisieren                             \
          { var reg3 object* p = &((Array)ptr)->data;                                  \
            aktualisiere(p);                                                           \
          }                                                                            \
          ptr=newptr; # zum nächsten Objekt weiterrücken                               \
        }
      #define aktualisiere_record(type_expr)  # ignoriert type_expr \
        { var reg7 uintL laenge = speicher_laenge_record((void*)ptr); # Länge bestimmen   \
          var reg8 aint newptr = ptr+laenge; # Zeiger auf nächstes Objekt                 \
          # Record: alle Pointer innerhalb eines Record aktualisieren                     \
          { # Beim Aktualisieren von Pointern verliert der Aufbau von                     \
            # Hash-Tables seine Gültigkeit (denn die Hashfunktion eines                   \
            # Objekts hängt von seiner Adresse ab, die sich ja jetzt                      \
            # verändert).                                                                 \
            if ((sintB)(((Record)ptr)->rectype) < 0) # eine Hash-Table ?                  \
              { ((Record)ptr)->recflags |= bit(7); } # ja -> für Reorganisation vormerken \
           {var reg3 uintC count;                                                         \
            var reg4 object* p = &((Record)ptr)->recdata[0];                              \
            dotimespC(count,((Record)ptr)->reclength, { aktualisiere(p); p++; } );        \
          }}                                                                              \
          ptr=newptr; # zum nächsten Objekt weiterrücken                                  \
        }
      #define aktualisiere_varobjects()  \
        for_each_varobject_page(page,                                               \
          { # Fallunterscheidung nach:                                              \
              # Symbol; Simple-Vector; Nicht-simpler Array;                         \
              # Record (insbes. Hash-Table); Rest.                                  \
            switch (heapnr)                                                         \
              { case_symbol:                                                        \
                  aktualisiere_page(page,aktualisiere_symbol); break;               \
                case_svector:                                                       \
                  aktualisiere_page(page,aktualisiere_svector); break;              \
                case_array1: case_obvector: case_ostring: case_ovector:             \
                  aktualisiere_page(page,aktualisiere_array); break;                \
                case_record:                                                        \
                  aktualisiere_page(page,aktualisiere_record); break;               \
                default:                                                            \
                  break; # alle anderen enthalten keine zu aktualisierenden Pointer \
                         # -> nichts tun                                            \
          }   }                                                                     \
          );
      #endif

# Zweite SWEEP-Phase:
  # Verschiebung eines Objekts variabler Länge, p1 und p2 weiterrücken:
  # move_aligned_p1_p2(count);
  #if (Varobject_alignment==1)
    #define uintV  uintB
  #elif (Varobject_alignment==2)
    #define uintV  uintW
  #elif (Varobject_alignment==4)
    #define uintV  uintL
  #elif (Varobject_alignment==8)
    #define uintV  uintL2
  #else
    #error "Unbekannter Wert von 'Varobject_alignment'!"
  #endif
  #ifdef GNU # so läßt sich's besser optimieren
    #ifdef fast_dotimesL
      #define move_aligned_p1_p2(count)  \
        dotimespL(count,count/Varobject_alignment, *((uintV*)p2)++ = *((uintV*)p1)++; )
    #else
      #define move_aligned_p1_p2(count)  \
        do { *((uintV*)p2)++ = *((uintV*)p1)++; count -= Varobject_alignment; } until (count==0)
    #endif
  #else # andere Compiler akzeptieren ((type*)p)++ nicht.
    # Wie effizient ist das hier ??
    #define move_aligned_p1_p2(count)  \
      do { *(uintV*)p2 = *(uintV*)p1;                            \
           p1 += Varobject_alignment; p2 += Varobject_alignment; \
           count -= Varobject_alignment;                         \
         }                                                                              \
         until (count==0)
  #endif
  # Die Objekte variabler Länge werden an die vorher berechneten
  # neuen Plätze geschoben.
  #ifdef SPVW_PURE
  local void gc_sweep2_varobject_page (Page* page, uintL heapnr);
  local void gc_sweep2_varobject_page(page,heapnr)
    var reg5 Page* page;
    var reg6 uintL heapnr;
  #else
  local void gc_sweep2_varobject_page (Page* page);
  local void gc_sweep2_varobject_page(page)
    var reg5 Page* page;
  #endif
    # Von unten nach oben durchgehen und dabei runterschieben:
    { var reg1 aint p1 = (aint)type_pointable(0,page->page_gcpriv.firstmarked); # Source-Pointer, erstes markiertes Objekt
      var reg4 aint p1end = page->page_end;
      var reg2 aint p2 = page->page_start; # Ziel-Pointer
      var_speicher_laenge_;
      until (p1==p1end) # obere Grenze erreicht -> fertig
        { # nächstes Objekt hat Adresse p1
          if (marked(p1)) # markiert?
            { unmark(p1); # Markierung löschen
              # Objekt behalten und verschieben:
             {var reg3 uintL count = calc_speicher_laenge(p1); # Länge (durch Varobject_alignment teilbar, >0)
              if (!(p1==p2)) # falls Verschiebung nötig
                { move_aligned_p1_p2(count); } # verschieben und weiterrücken
                else # sonst nur weiterrücken:
                { p1 += count; p2 += count; }
            }}
            else
            { p1 = (aint)type_pointable(0,*(object*)p1); } # mit Pointer (Typinfo=0) zum nächsten markierten Objekt
        }
      page->page_end = p2; # obere Grenze der Objekte variabler Länge neu setzen
    }

#ifdef DEBUG_SPVW
  # Kontrolle, ob auch alles unmarkiert ist:
  #define CHECK_GC_UNMARKED()  gc_unmarkcheck()
  local void gc_unmarkcheck (void);
  local void gc_unmarkcheck()
    { for_each_varobject_page(page,
        # Von unten nach oben durchgehen:
        { var reg1 aint p1 = page->page_start;
          var reg4 aint p1end = page->page_end;
          var_speicher_laenge_;
          until (p1==p1end) # obere Grenze erreicht -> fertig
            { # nächstes Objekt hat Adresse p1
              if (marked(p1)) # markiert?
                { asciz_out("\nObjekt 0x"); hex_out(p1); asciz_out(" markiert!!\n");
                  abort();
                }
              p1 += calc_speicher_laenge(p1);
        }   }
        );
      for_each_cons_page(page,
        # Von unten nach oben durchgehen:
        { var reg1 aint p1 = page->page_start;
          var reg4 aint p1end = page->page_end;
          until (p1==p1end) # obere Grenze erreicht -> fertig
            { # nächstes Objekt hat Adresse p1
              if (marked(p1)) # markiert?
                { asciz_out("\nObjekt 0x"); hex_out(p1); asciz_out(" markiert!!\n");
                  abort();
                }
              p1 += sizeof(cons_);
        }   }
        );
    }
#else
  #define CHECK_GC_UNMARKED()
#endif

#if defined(UNIX) || defined(AMIGAOS)
  #define GC_CLOSES_FILES # Flag, ob die GC nicht mehr referenzierte Files schließt
#endif

#ifdef SPVW_PAGES
  # Überflüssige Pages freigeben:
  # Falls nach einer GC der Platz, der uns in mem.free_pages zur Verfügung
  # steht, mehr als 25% dessen ausmacht, was wir momentan brauchen, wird der
  # Rest ans Betriebssystem zurückgegeben.
  local void free_some_unused_pages (void);
  local void free_some_unused_pages()
    { var reg5 uintL needed_space = floor(mem.last_gcend_space,4); # 25%
      var reg4 uintL accu_space = 0;
      var reg2 Pages* pageptr = &mem.free_pages;
      var reg1 Pages page = *pageptr;
      until (page==NULL)
        { var reg3 Pages nextpage = page->page_gcpriv.next;
          if (accu_space < needed_space)
            # page behalten
            { accu_space += page->page_room;
              pageptr = (Pages*)&page->page_gcpriv.next; page = nextpage;
            }
            else
            # page freigeben
            { free((void*)page->m_start); page = *pageptr = nextpage; }
    }   }
#endif

# GC-Timer ein- und ausschalten: gc_timer_on(); ... gc_timer_off();
# Die dazwischen verstrichene Zeit wird auf gc_time addiert.
  #ifndef HAVE_RUN_TIME
    #define gc_timer_on()  \
      { var internal_time gcstart_time = get_time(); # aktuelle Zeit abgreifen und retten
    #define gc_timer_off()  \
        gc_time += get_time()-gcstart_time; \
      }
  #endif
  #ifdef TIME_UNIX
    #define gc_timer_on()  \
      { var internal_time gcstart_time; \
        get_run_time(&gcstart_time); # aktuelle verbrauchte Zeit abfragen und retten
    #define gc_timer_off()  \
       {var internal_time gcend_time;                           \
        get_run_time(&gcend_time);                              \
        # Differenz von gcend_time und gcstart_time bilden:     \
        sub_internal_time(gcend_time,gcstart_time, gcend_time); \
        # diese Differenz zu gc_time addieren:                  \
        add_internal_time(gc_time,gcend_time, gc_time);         \
      }}
  #endif
  #ifdef TIME_VMS
    #define gc_timer_on()  \
      { var internal_time gcstart_time = clock();
    #define gc_timer_off()  \
       {var internal_time gcend_time = clock(); \
        gc_time += gcend_time-gcstart_time;     \
      }}
  #endif

# GC-bedingt Signale disablen: gc_signalblock_on(); ... gc_signalblock_off();
  #if defined(HAVE_SIGNALS) && defined(SIGWINCH) && !defined(NO_ASYNC_INTERRUPTS)
    # Signal SIGWINCH blockieren, denn eine Veränderung des Wertes von
    # SYS::*PRIN-LINELENGTH* können wir während der GC nicht brauchen.
    # Dann Signal SIGWINCH wieder freigeben.
    #define gc_signalblock_on()  signalblock_on(SIGWINCH)
    #define gc_signalblock_off()  signalblock_off(SIGWINCH)
  #else
    #define gc_signalblock_on()
    #define gc_signalblock_off()
  #endif

# Normale Garbage Collection durchführen:
  local void gar_col_normal(void);
  local void gar_col_normal()
    { var uintL gcstart_space; # belegter Speicher bei GC-Start
      var uintL gcend_space; # belegter Speicher bei GC-Ende
      #ifdef GC_CLOSES_FILES
      var object files_to_close; # Liste der zu schließenden Files
      #endif
      set_break_sem_1(); # BREAK während Garbage Collection sperren
      gc_signalblock_on(); # Signale während Garbage Collection sperren
      gc_timer_on();
      gcstart_space = used_space(); # belegten Speicherplatz ermitteln
      #ifdef ATARI
      BIOS_Bell(); # Ton ausgeben
      #endif
      #ifdef HAVE_VADVISE
        begin_system_call();
        vadvise(VA_ANOM); # Paging-Verhalten wird jetzt etwas ungewöhnlich
        end_system_call();
      #endif
      CHECK_GC_UNMARKED();
      #ifdef SPVW_PAGES
        { var reg4 uintL heapnr;
          for (heapnr=0; heapnr<heapcount; heapnr++)
            { AVL_map(mem.heaps[heapnr].inuse,page,
                      page->page_room += page->page_end;
                     );
              # In page_room steht jetzt jeweils das Ende des benutzbaren Speichers.
        }   }
      #endif
      # Markierungsphase:
        #ifdef GC_CLOSES_FILES
        files_to_close = O(open_files); O(open_files) = NIL;
        #endif
        gc_markphase();
        #ifdef GC_CLOSES_FILES
        # (noch unmarkierte) Liste files_to_close aufspalten in zwei Listen:
        { var reg1 object Lu = files_to_close;
          var reg2 object* L1 = &O(open_files);
          var reg3 object* L2 = &files_to_close;
          while (consp(Lu))
            { if (marked(TheStream(Car(Lu)))) # (car Lu) markiert?
                # ja -> in O(open_files) übernehmen:
                { *L1 = Lu; L1 = &Cdr(Lu); Lu = *L1; }
                else
                # nein -> in files_to_close übernehmen:
                { *L2 = Lu; L2 = &Cdr(Lu); Lu = *L2; }
            }
          *L1 = NIL; *L2 = NIL;
        }
        gc_mark(O(open_files)); gc_mark(files_to_close); # Beide Listen jetzt markieren
        #endif
      # Jetzt sind alle aktiven Objekte markiert:
      # Aktive Objekte variabler Länge wie auch aktive Zwei-Pointer-Objekte tragen
      # in ihrem ersten Byte ein gesetztes Markierungsbit, aktive (F)SUBRs tragen
      # in ihrem ersten Konstantenpointer ein gesetztes Markierungsbit, sonst sind
      # alle Markierungsbits gelöscht.
      # "Sweep"-Phase:
        # Die CONSes u.ä. (Objekte mit 2 Pointern) werden kompaktiert.
        # Von den Objekten variabler Länge werden die Zielplätze für die
        # Phase 4 errechnet und abgespeichert.
        # CONS-Zellen kompaktieren:
          for_each_cons_page(page, { gc_compact_cons_page(page); } );
        # Objekte variabler Länge zur Zusammenschiebung nach unten vorbereiten:
          #ifdef SPVW_PURE
          for_each_varobject_page(page, { gc_sweep1_varobject_page(page,heapnr); } );
          #else
          for_each_varobject_page(page, { gc_sweep1_varobject_page(page); } );
          #endif
      # Jetzt sind alle aktiven Objekte für die Aktualisierung vorbereitet:
      # Bei aktiven Objekten variabler Länge A2 ist (A2).L die Adresse, wo das
      # Objekt nach der GC stehen wird (incl. Typinfo und Markierungsbit und evtl.
      # Symbol-Flags). Bei aktiven Zwei-Pointer-Objekten A2 bleibt entweder A2
      # stehen (dann ist das Markierungsbit in (A2) gelöscht), oder A2 wird
      # verschoben (dann ist (A2).L die neue Adresse, ohne Typinfo, aber incl.
      # Markierungsbit).
      # Jedoch: SUBRs/FSUBRs und feste Symbole (sie sind alle aktiv) sind immer
      # noch markiert. Sie werden als erstes demarkiert:
        unmark_fixed_varobjects();
      # Aktualisierungsphase:
        # Der gesamte LISP-Speicher wird durchgegangen und dabei alte durch
        # neue Adressen ersetzt.
        # Durchlaufen durch alle LISP-Objekte und aktualisieren:
          # Pointer im LISP-Stack aktualisieren:
            aktualisiere_STACK();
          # Programmkonstanten aktualisieren:
            aktualisiere_tab();
            #ifdef GC_CLOSES_FILES
            aktualisiere(&files_to_close);
            #endif
          # Pointer in den Cons-Zellen aktualisieren:
            aktualisiere_conses();
          # Pointer in den Objekten variabler Länge aktualisieren:
            #define aktualisiere_page(page,aktualisierer)  \
              { var reg2 aint ptr = (aint)type_pointable(0,page->page_gcpriv.firstmarked); \
                var reg6 aint ptrend = page->page_end;                                     \
                # alle Objekte mit Adresse >=ptr, <ptrend durchgehen:                      \
                until (ptr==ptrend) # solange bis ptr am Ende angekommen ist               \
                  { # nächstes Objekt mit Adresse ptr (< ptrend) durchgehen:               \
                    if (marked(ptr)) # markiert?                                           \
                      # Typinfo ohne Markierungsbit nehmen!                                \
                      { aktualisierer(typecode_at(ptr) & ~bit(garcol_bit_t)); }            \
                      else                                                                 \
                      # mit Pointer (Typinfo=0) zum nächsten markierten Objekt             \
                      { ptr = (aint)type_pointable(0,*(object*)ptr); }                     \
              }   }
            aktualisiere_varobjects();
            #undef aktualisiere_page
      # Jetzt sind alle aktiven Objekte mit korrektem Inhalt versehen (alle darin
      # vorkommenden Pointer zeigen auf die nach der GC korrekten Adressen).
      # Die aktiven Zwei-Pointer-Objekte sind bereits am richtigen Ort und
      # unmarkiert; die Objekte variabler Länge sind noch am alten Ort und
      # markiert, falls aktiv.
      # Zweite SWEEP-Phase:
        # Die Objekte variabler Länge werden an die vorher berechneten
        # neuen Plätze geschoben.
        #ifdef SPVW_PURE
        for_each_varobject_page(page, { gc_sweep2_varobject_page(page,heapnr); } );
        #else
        for_each_varobject_page(page, { gc_sweep2_varobject_page(page); } );
        #endif
      # Jetzt sind alle aktiven Objekte mit korrektem Inhalt versehen, am richtigen
      # Ort und wieder unmarkiert.
      #ifdef SPVW_PAGES
        { var reg5 uintL heapnr;
          for (heapnr=0; heapnr<heapcount; heapnr++)
            { var reg4 Pages* heapptr = &mem.heaps[heapnr].inuse;
              AVL_map(*heapptr,page,
                      page->page_room -= page->page_end;
                     );
              # In page_room steht jetzt jeweils wieder der verfügbare Platz.
              # Pages wieder nach dem verfügbaren Platz sortieren:
              *heapptr = AVL(AVLID,sort)(*heapptr);
        }   }
        for_each_cons_heap(set_lastused_dummy);
        # .reserve behandeln??
      #endif
      CHECK_AVL_CONSISTENCY();
      CHECK_GC_CONSISTENCY();
      CHECK_GC_UNMARKED();
      CHECK_PACK_CONSISTENCY();
      # Ende der Garbage Collection.
      #ifdef HAVE_VADVISE
        begin_system_call();
        vadvise(VA_NORM); # Paging-Verhalten wird ab jetzt wieder normal
        end_system_call();
      #endif
      #ifdef ATARI
      BIOS_Bell(); # Ton ausgeben
      #endif
      gc_count += 1; # GCs mitzählen
      #ifdef SPVW_PAGES
      mem.last_gcend_space =
      #endif
      gcend_space = used_space(); # belegten Speicherplatz ermitteln
      #ifdef SPVW_PURE_BLOCKS
      # Um bis zu 50% lassen wir den benutzten Platz anwachsen, dann erst
      # kommt die nächste GC:
      #define set_total_room(space_used_now)  \
        { mem.total_room = floor(space_used_now,2); # 50% des jetzt benutzten Platzes       \
          if (mem.total_room < 512*1024) { mem.total_room = 512*1024; } # mindestens 512 KB \
        }
      set_total_room(gcend_space);
      #endif
      { var reg1 uintL freed = gcstart_space - gcend_space; # von dieser GC
                                       # wiederbeschaffter Speicherplatz
        # dies zum 64-Bit-Akku gc_space addieren:
        gc_space.lo += freed;
        if (gc_space.lo < freed) # Übertrag?
          gc_space.hi += 1;
      }
      #ifdef SPVW_PAGES
      free_some_unused_pages();
      #endif
      # von dieser GC benötigte Zeit zur GC-Gesamtzeit addieren:
      gc_timer_off();
      #ifdef GC_CLOSES_FILES
      close_some_files(files_to_close); # vorher unmarkierte Files schließen
      #endif
      gc_signalblock_off(); # Signale wieder freigeben
      clr_break_sem_1(); # BREAK wieder ermöglichen
    }

#ifdef SPVW_PAGES

# Eine kleine Sortier-Routine:
#define SORTID  spvw
#define SORT_ELEMENT  Pages
#define SORT_KEY  uintL
#define SORT_KEYOF(page)  (page)->page_gcpriv.l
#define SORT_COMPARE(key1,key2)  (sintL)((key1)-(key2))
#define SORT_LESS(key1,key2)  ((key1) < (key2))
#include "sort.c"

# Kompaktierung einer Page durch Umfüllen in andere Pages derselben Art:
  #ifdef SPVW_PURE
  local void gc_compact_from_varobject_page (Heap* heapptr, Page* page, uintL heapnr);
  local void gc_compact_from_varobject_page(heapptr,page,heapnr)
    var reg9 Heap* heapptr;
    var reg8 Page* page;
    var reg10 uintL heapnr;
  #else
  local void gc_compact_from_varobject_page (Heap* heapptr, Page* page);
  local void gc_compact_from_varobject_page(heapptr,page)
    var reg9 Heap* heapptr;
    var reg8 Page* page;
  #endif
    { var reg1 aint p1 = page->page_start;
      var reg7 aint p1end = page->page_end;
      var_speicher_laenge_;
     {var reg4 Pages new_page = EMPTY; # Page, in die gefüllt wird
      var AVL(AVLID,stack) stack; # Weg von der Wurzel bis zu ihr
      var reg2 aint p2; # Cache von new_page->page_end
      var reg5 uintL l2; # Cache von new_page->page_room
      # Versuche alle Objekte zwischen p1 und p1end zu kopieren:
      loop
        { if (p1==p1end) break; # obere Grenze erreicht -> fertig
         {var reg3 uintL laenge = calc_speicher_laenge(p1); # Byte-Länge bestimmen
          # Suche eine Page, die noch mindestens laenge Bytes frei hat:
          if ((new_page == EMPTY) || (l2 < laenge))
            { if (!(new_page == EMPTY)) # Cache leeren?
                { new_page->page_end = p2;
                  new_page->page_room = l2;
                  AVL(AVLID,move)(&stack);
                }
              new_page = AVL(AVLID,least)(laenge,&heapptr->inuse,&stack);
              if (new_page==EMPTY) break;
              new_page->page_gcpriv.d = -1L; # new_page als "zu füllend" kennzeichnen
              p2 = new_page->page_end;
              l2 = new_page->page_room;
            }
          {var reg6 aint old_p1 = p1;
           var reg6 aint old_p2 = p2;
           # Kopiere das Objekt:
           l2 -= laenge; move_aligned_p1_p2(laenge);
           # Hinterlasse einen Pointer auf die neue Position:
           *(object*)old_p1 = with_mark_bit(type_pointer_object(0,old_p2));
           # p1 = Sourceadresse für nächstes Objekt
        }}}
      if (!(new_page == EMPTY)) # Cache leeren?
        { new_page->page_end = p2;
          new_page->page_room = l2;
          AVL(AVLID,move)(&stack);
        }
     }
     # Die nicht kopierten Objekte erfahren eine konstante Verschiebung nach unten:
     {var reg4 aint p2 = page->page_start;
      page->page_gcpriv.d = p1 - p2; # Verschiebung
      page->page_start = p1; # jetziger Anfang der Page
      if (!(p1==p2)) # falls Verschiebung nötig
        until (p1==p1end) # obere Grenze erreicht -> fertig
          { var reg3 uintL laenge = calc_speicher_laenge(p1); # Byte-Länge bestimmen
            var reg2 tint flags = ((Varobject)p1)->header_flags; # Typinfo (und Flags bei Symbolen) retten
            set_GCself(p1, flags,p2); # neue Adresse eintragen, mit alter Typinfo
            mark(p1); # mit Markierungsbit
            p1 += laenge; p2 += laenge;
          }
    }}
  local void gc_compact_from_cons_page (Heap* heapptr, Page* page);
  local void gc_compact_from_cons_page(heapptr,page)
    var reg7 Heap* heapptr;
    var reg6 Page* page;
    { var reg1 aint p1 = page->page_end;
      var reg5 aint p1start = page->page_start;
     {var reg3 Pages new_page = EMPTY; # Page, in die gefüllt wird
      var AVL(AVLID,stack) stack; # Weg von der Wurzel bis zu ihr
      var reg2 aint p2; # Cache von new_page->page_end
      var reg4 uintL l2; # Cache von new_page->page_room
      # Versuche alle Objekte zwischen p1start und p1 zu kopieren:
      loop
        { if (p1==p1start) break; # untere Grenze erreicht -> fertig
          # Suche eine Page, die noch mindestens sizeof(cons_) Bytes frei hat:
          if ((new_page == EMPTY) || (l2 == 0)) # l2 < sizeof(cons_) bedeutet l2 = 0
            { if (!(new_page == EMPTY)) # Cache leeren?
                { new_page->page_end = p2;
                  new_page->page_room = l2;
                  AVL(AVLID,move)(&stack);
                }
              new_page = AVL(AVLID,least)(sizeof(cons_),&heapptr->inuse,&stack);
              if (new_page==EMPTY) break;
              new_page->page_gcpriv.d = -1L; # new_page als "zu füllend" kennzeichnen
              p2 = new_page->page_end;
              l2 = new_page->page_room;
            }
          p1 -= sizeof(cons_); # p1 = Sourceadresse für nächstes Objekt
          # Kopiere das Objekt:
          ((object*)p2)[0] = ((object*)p1)[0];
          ((object*)p2)[1] = ((object*)p1)[1];
          # Hinterlasse einen Pointer auf die neue Position:
          *(object*)p1 = with_mark_bit(type_pointer_object(0,p2));
          p2 += sizeof(cons_); l2 -= sizeof(cons_);
        }
      if (!(new_page == EMPTY)) # Cache leeren?
        { new_page->page_end = p2;
          new_page->page_room = l2;
          AVL(AVLID,move)(&stack);
        }
     }
     # Die nicht kopierten Objekte bleiben an Ort und Stelle.
     page->page_gcpriv.d = page->page_end - p1; # Zugewinn
     page->page_end = p1; # jetziges Ende der Page
    }

# Kompaktierung aller Pages einer bestimmten Art:
  #ifdef SPVW_PURE
  local void gc_compact_heap (Heap* heapptr, boolean heaptype, uintL heapnr);
  local void gc_compact_heap(heapptr,heaptype,heapnr)
    var reg4 Heap* heapptr;
    var reg5 boolean heaptype;
    var reg5 uintL heapnr;
  #else
  local void gc_compact_heap (Heap* heapptr, boolean heaptype);
  local void gc_compact_heap(heapptr,heaptype)
    var reg4 Heap* heapptr;
    var reg5 boolean heaptype;
  #endif
    { # Erst eine Liste aller Pages erstellen, aufsteigend sortiert
      # nach der Anzahl der belegten Bytes:
      var reg10 uintL pagecount = 0;
      map_heap(*heapptr,page,
               { page->page_gcpriv.l = page->page_end - page->page_start; # Anzahl der belegten Bytes
                 pagecount++;
               }
              );
      # pagecount = Anzahl der Pages.
     {var reg6 DYNAMIC_ARRAY(pages_sorted,Pages,pagecount);
      {var reg4 uintL index = 0;
       map_heap(*heapptr,page, { pages_sorted[index++] = page; } );
      }
      # pages_sorted = Array der Pages.
      SORT(SORTID,sort)(pages_sorted,pagecount);
      # pages_sorted = Array der Pages, sortiert nach der Anzahl der belegten Bytes.
      # In jeder Page bedeutet page_gcpriv.d die Verschiebung nach unten,
      # die der Page in Phase 3 zuteil werden muß (>=0).
      # page_gcpriv.d = -1L für die zu füllenden Pages.
      # page_gcpriv.d = -2L für die noch unbehandelten Pages.
      map_heap(*heapptr,page, { page->page_gcpriv.d = -2L; } ); # alle Pages noch unbehandelt
      {var reg3 uintL index;
       for (index=0; index<pagecount; index++) # Durch alle Pages durchlaufen
         { var reg2 Pages page = pages_sorted[index]; # nächste Page
           if (page->page_gcpriv.d == -2L) # noch unbehandelt und
                                           # noch nicht als "zu füllend" markiert?
             { # page wird geleert.
               heapptr->inuse = AVL(AVLID,delete1)(page,heapptr->inuse); # page herausnehmen
               # page leeren:
               if (heaptype)
                 { gc_compact_from_cons_page(heapptr,page); }
                 else
                 #ifdef SPVW_PURE
                 { gc_compact_from_varobject_page(heapptr,page,heapnr); }
                 #else
                 { gc_compact_from_varobject_page(heapptr,page); }
                 #endif
      }  }   }
      CHECK_AVL_CONSISTENCY();
      CHECK_GC_CONSISTENCY_2();
      {var reg2 uintL index;
       for (index=0; index<pagecount; index++) # Durch alle Pages durchlaufen
         { var reg1 Pages page = pages_sorted[index]; # nächste Page
           if (!(page->page_gcpriv.d == -1L)) # eine zu leerende Page
             { page->page_room += page->page_gcpriv.d; # So viel Platz haben wir nun gemacht
               if (page->page_start == page->page_end)
                 # Page ganz geleert
                 { # Page freigeben:
                   if (page->m_length > min_page_size_brutto)
                     # Übergroße Page
                     { free((void*)page->m_start); } # ans Betriebssystem zurückgeben
                     else
                     # Normalgroße Page
                     { # wieder initialisieren (page->page_room bleibt gleich!):
                       page->page_start = page->page_end =
                         round_up((aint)page+sizeof(NODE),Varobject_alignment);
                       # in den Pool mem.free_pages einhängen:
                       page->page_gcpriv.next = mem.free_pages;
                       mem.free_pages = page;
                 }   }
                 else
                 # Page konnte nicht ganz geleert werden
                 { heapptr->inuse = AVL(AVLID,insert1)(page,heapptr->inuse); } # Page wieder rein
      }  }   }
      FREE_DYNAMIC_ARRAY(pages_sorted);
      CHECK_AVL_CONSISTENCY();
      CHECK_GC_CONSISTENCY_2();
    }}

# Kompaktierende Garbage Collection durchführen.
# Wird aufgerufen, nachdem gar_col() nicht genügend Platz am Stück besorgen
# konnte.
  local void gar_col_compact (void);
  local void gar_col_compact()
    { # Es werden Lisp-Objekte von fast leeren Pages in andere Pages
      # umgefüllt, um die ganz leer machen und zurückgeben zu können.
      # 1. Für jede Page-Art:
      #    Pages unterteilen in zu leerende und zu füllende Pages und dabei
      #    soviel Daten wie möglich von den zu leerenden in die zu füllenden
      #    Pages umkopieren. Kann eine Page nicht ganz geleert werden, so
      #    wird sie so gelassen, wie sie ist, und in ihr werden dann nachher
      #    die übrigen Daten nur nach unten geschoben.
      #    Rückgabe der ganz geleerten Pages.
      # 2. Aktualisierung der Pointer.
      # 3. Durchführung der Verschiebungen in den nicht ganz geleerten Pages.
      set_break_sem_1(); # BREAK während Garbage Collection sperren
      gc_signalblock_on(); # Signale während Garbage Collection sperren
      gc_timer_on();
      CHECK_GC_UNMARKED();
      { var reg1 uintL heapnr;
        for (heapnr=0; heapnr<heapcount; heapnr++)
          #ifdef SPVW_PURE
          if (mem.heaptype[heapnr] >= 0)
            { gc_compact_heap(&mem.heaps[heapnr],mem.heaptype[heapnr],heapnr); }
          #endif
          #ifdef SPVW_MIXED
          { gc_compact_heap(&mem.heaps[heapnr],heapnr); }
          #endif
      }
      # Aktualisierungsphase:
        # Der gesamte LISP-Speicher wird durchgegangen und dabei alte durch
        # neue Adressen ersetzt.
        # Durchlaufen durch alle LISP-Objekte und aktualisieren:
          # Pointer im LISP-Stack aktualisieren:
            aktualisiere_STACK();
          # Programmkonstanten aktualisieren:
            aktualisiere_tab();
          # Pointer in den Cons-Zellen aktualisieren:
            aktualisiere_conses();
          # Pointer in den Objekten variabler Länge aktualisieren:
            #define aktualisiere_page(page,aktualisierer)  \
              { var reg2 aint ptr = page->page_start;                        \
                var reg6 aint ptrend = page->page_end;                       \
                # alle Objekte mit Adresse >=ptr, <ptrend durchgehen:        \
                until (ptr==ptrend) # solange bis ptr am Ende angekommen ist \
                  { # nächstes Objekt mit Adresse ptr (< ptrend) durchgehen: \
                    aktualisierer(typecode_at(ptr) & ~bit(garcol_bit_t)); # und weiterrücken \
              }   }
            aktualisiere_varobjects();
            #undef aktualisiere_page
      # Durchführung der Verschiebungen in den nicht ganz geleerten Pages:
        for_each_varobject_page(page,
          { if (!(page->page_gcpriv.d == -1L))
              { var reg2 aint p1 = page->page_start;
                var reg4 aint p1end = page->page_end;
                var reg1 aint p2 = p1 - page->page_gcpriv.d;
                if (!(p1==p2)) # falls Verschiebung nötig
                  { var_speicher_laenge_;
                    page->page_start = p2;
                    until (p1==p1end) # obere Grenze erreicht -> fertig
                      { # nächstes Objekt hat Adresse p1, ist markiert
                        unmark(p1); # Markierung löschen
                        # Objekt behalten und verschieben:
                       {var reg3 uintL count = calc_speicher_laenge(p1); # Länge (durch Varobject_alignment teilbar, >0)
                        move_aligned_p1_p2(count); # verschieben und weiterrücken
                      }}
                    page->page_end = p2;
          }   }   }
          );
      for_each_cons_heap(set_lastused_dummy);
      free_some_unused_pages();
      CHECK_AVL_CONSISTENCY();
      CHECK_GC_CONSISTENCY();
      CHECK_GC_UNMARKED();
      CHECK_PACK_CONSISTENCY();
      gc_timer_off();
      gc_signalblock_off(); # Signale wieder freigeben
      clr_break_sem_1(); # BREAK wieder ermöglichen
    }

#endif

# Garbage Collection durchführen:
  global void gar_col(void);
  global void gar_col()
    { gar_col_normal();
      #ifdef SPVW_PAGES
      #if defined(UNIX) || defined(VMS) || defined(AMIGAOS)
      # Wenn der in Pages allozierte, aber unbelegte Speicherplatz
      # mehr als 25% dessen ausmacht, was belegt ist, lohnt sich wohl eine
      # Kompaktierung, denn fürs Betriebssystem kostet eine halbleere Page
      # genausoviel wie eine volle Page:
      if (free_space() > floor(mem.last_gcend_space,4))
        { gar_col_compact(); mem.last_gc_compacted = TRUE; }
        else
      #endif
        { mem.last_gc_compacted = FALSE; }
      #endif
    }

# Macro aktualisiere jetzt unnötig:
  #undef aktualisiere

#ifdef SPVW_MIXED_BLOCKS

# Zur Reorganisation des Objektspeichers nach GC oder vor und nach EXECUTE:
  # Unterprogramm zum Verschieben der Conses.
  # move_conses(delta);
  # Der Reservespeicher wird um delta Bytes (durch Varobject_alignment
  # teilbar) verkleinert, dabei die Conses um delta Bytes nach oben geschoben.
  local void move_conses (sintL delta);
  local void move_conses (delta)
    var reg4 sintL delta;
    { if (delta==0) return; # keine Verschiebung nötig?
      set_break_sem_1(); # BREAK währenddessen sperren
      gc_signalblock_on(); # Signale währenddessen sperren
      gc_timer_on();
      if (delta>0)
        # aufwärts schieben, von oben nach unten
        { var reg1 object* source = (object*) mem.conses.end;
          var reg3 object* source_end = (object*) mem.conses.start;
          #if !(defined(MIPS) && !defined(GNU))
          var reg2 object* dest = (object*) (mem.conses.end += delta);
          #else # IRIX 4 "cc -ansi" Compiler-Bug umgehen ??
          var reg2 object* dest = (mem.conses.end += delta, (object*)mem.conses.end);
          #endif
          mem.conses.start += delta;
          until (source==source_end)
            { *--dest = *--source; # ein ganzes Cons nach oben kopieren
              *--dest = *--source;
        }   }
        else # delta<0
        # abwärts schieben, von unten nach oben
        { var reg1 object* source = (object*) mem.conses.start;
          var reg3 object* source_end = (object*) mem.conses.end;
          #if !(defined(MIPS) && !defined(GNU))
          var reg2 object* dest = (object*) (mem.conses.start += delta);
          #else # IRIX 4 "cc -ansi" Compiler-Bug umgehen ??
          var reg2 object* dest = (mem.conses.start += delta, (object*)mem.conses.start);
          #endif
          mem.conses.end += delta;
          until (source==source_end)
            { *dest++ = *source++; # ein ganzes Cons nach oben kopieren
              *dest++ = *source++;
        }   }
      # Pointer auf Conses u.ä. aktualisieren:
      { var reg4 soint odelta = (soint)delta<<(oint_addr_shift-addr_shift); # Offset im oint
        # Der gesamte LISP-Speicher wird durchgegangen und dabei alte durch
        # neue Adressen ersetzt.
        # Aktualisierung eines Objekts *objptr :
          #define aktualisiere(objptr)  \
            { switch (mtypecode(*(object*)(objptr)))                          \
                { case_cons: case_ratio: case_complex: # Zwei-Pointer-Objekt? \
                    *(oint*)(objptr) += odelta; break;                        \
                  default: break;                                             \
            }   }
        # Durchlaufen durch alle LISP-Objekte und aktualisieren:
          # Pointer im LISP-Stack aktualisieren:
            { var reg2 object* objptr = &STACK_0; # Pointer, der durch den STACK läuft
              until (*objptr==nullobj) # bis STACK zu Ende ist:
                { if ( *((oint*)objptr) & wbit(frame_bit_o) ) # Beginnt hier ein Frame?
                   { if (( *((oint*)objptr) & wbit(skip2_bit_o) ) == 0) # Ohne skip2-Bit?
                      objptr skipSTACKop 2; # ja -> um 2 weiterrücken
                      else
                      objptr skipSTACKop 1; # nein -> um 1 weiterrücken
                   }
                   else
                   { aktualisiere(objptr); # normales Objekt, aktualisieren
                     objptr skipSTACKop 1; # weiterrücken
            }   }  }
          # Programmkonstanten aktualisieren:
            aktualisiere_tab();
          # Pointer in den Cons-Zellen aktualisieren:
            aktualisiere_conses();
          # Pointer in den Objekten variabler Länge aktualisieren:
            #define aktualisiere_page  aktualisiere_page_normal
            aktualisiere_varobjects();
            #undef aktualisiere_page
        # Macro aktualisiere jetzt unnötig:
          #undef aktualisiere
      }
      # Ende des Verschiebens und Aktualisierens.
      # benötigte Zeit zur GC-Gesamtzeit addieren:
      gc_timer_off();
      gc_signalblock_off(); # Signale wieder freigeben
      clr_break_sem_1(); # BREAK wieder ermöglichen
    }

#endif

# ------------------------------------------------------------------------------
#                 Speicherbereitstellungsfunktionen

# Fehlermeldung wegen vollen Speichers
  local nonreturning void fehler_speicher_voll (void);
  local nonreturning void fehler_speicher_voll()
    { fehler(
             DEUTSCH ? "Speicherplatz für LISP-Objekte ist voll." :
             ENGLISH ? "No more room for LISP objects" :
             FRANCAIS ? "Il n'y a plus de place pour des objets LISP." :
             ""
            );
    }

# Stellt fest, ob eine Adresse im Intervall [0..2^oint_addr_len-1] liegt:
  #define pointable_usable_test(a)  \
    (pointable(type_pointer_object(0,a)) == (void*)(a))

# Holt Speicher vom Betriebssystem
  local void* mymalloc (uintL need);
  local void* mymalloc(need)
    var reg3 uintL need;
    {
      #ifdef ATARI
        var reg1 sintL erg = GEMDOS_Malloc(need);
        if (erg<0) return NULL;
        return (void*)erg;
      #else
        var reg1 void* addr;
        addr = malloc(need);
        if (addr==NULL) return NULL;
        # Intervall [addr,addr+need-1] muß in [0..2^oint_addr_len-1] liegen:
        { var reg2 aint a = (aint)addr; # a = untere Intervallgrenze
          if (pointable_usable_test(a))
            { a = round_down(a + need-1,bit(addr_shift)); # a = obere Intervallgrenze
              if (pointable_usable_test(a))
                { return addr; }
        }   }
        # Mit diesem Stück Speicher können wir nichts anfangen, wieder zurückgeben:
        free(addr);
        return NULL;
      #endif
    }

#ifdef SPVW_MIXED_BLOCKS

# Schafft Platz für ein neues Objekt.
# Falls keiner vorhanden -> Fehlermeldung.
# make_space(need);
# > uintL need: angeforderter Platz in Bytes (eine Variable oder Konstante)
  # Der Test, ob Platz vorhanden ist, als Macro, der Rest als Funktion:
  #define make_space(need)  \
    { if (mem.conses.start-mem.objects.end < (uintL)(need)) make_space_gc(need); }
  local void make_space_gc (uintL need);
  local void make_space_gc(need)
    var reg1 uintL need;
    { # (mem.conses.start-mem.objects.end < need)  ist schon abgeprüft, also
        # Nicht genügend Platz
        not_enough_room:
        { gar_col(); # Garbage Collector aufrufen
          # Teste auf Tastatur-Unterbrechung
          interruptp(
            { pushSTACK(S(gc)); tast_break();
              if (mem.conses.start-mem.objects.end < need) goto not_enough_room;
                else
                return;
            });
          if (mem.conses.start-mem.objects.end < need) # und wieder testen
            # Wirklich nicht genügend Platz da.
            # [Unter UNIX mit 'realloc' arbeiten??]
            # Abhilfe: Reservespeicher wird halbiert.
            { var reg1 uintL reserve = mem.MEMTOP - mem.MEMRES; # noch freie Reserve
              if (reserve>=8) # Reservespeicher auch voll?
                # nein -> Reservespeicher anzapfen und Fehlermeldung ausgeben
                # halbe Reserve
                { move_conses(round_down(floor(reserve,2),Varobject_alignment));
                  # halbierte Reserve, aligned: um soviel die Conses nach oben schieben
                  fehler_speicher_voll();
                }
                else
                # ja -> harte Fehlermeldung
                { asciz_out(DEUTSCH ? CRLFstring "*** - " "Speicherplatz für LISP-Objekte ist voll: RESET" :
                            ENGLISH ? CRLFstring "*** - " "No more room for LISP objects: RESET" :
                            FRANCAIS ? CRLFstring "*** - " "Il n'y a plus de place pour des objets LISP : RAZ" :
                            ""
                           );
                  reset(); # und zum letzten Driver-Frame zurück
                }
            }
            else
            # Jetzt ist genügend Platz da. Vielleicht sogar genug, den
            # Reservespeicher auf normale Größe zu bringen?
            { var reg2 uintL free = (mem.conses.start-mem.objects.end) - need;
                                # soviel Bytes noch frei
              var reg2 uintL free_reserve = mem.MEMTOP-mem.MEMRES;
                                # soviel Bytes noch in der Reserve frei, <=RESERVE
              var reg2 uintL free_total = free + free_reserve;
                                # freier Objektspeicher + freie Reserve
              if (free_total >= RESERVE) # mindestens Normalwert RESERVE ?
                # ja -> Reservespeicher auf normale Größe bringen, indem
                # die Conses um (RESERVE - free_reserve) nach unten geschoben
                # werden:
                move_conses(free_reserve-RESERVE);
                # Dadurch bleibt genügend für need frei.
            }
    }   }

#endif

#ifdef SPVW_PURE_BLOCKS # <==> SINGLEMAP_MEMORY

# Schafft Platz für ein neues Objekt.
# Falls keiner vorhanden -> Fehlermeldung.
# make_space(need,heapptr);
# > uintL need: angeforderter Platz in Bytes (eine Variable oder Konstante)
# > Heap* heapptr: Pointer auf den Heap, dem der Platz entnommen werden soll
  # Der Test, ob Platz vorhanden ist, als Macro, der Rest als Funktion:
  #define make_space(need,heapptr)  \
    { if ((mem.total_room < (uintL)(need))                             \
          || (heapptr->heap_limit - heapptr->heap_end < (uintL)(need)) \
         )                                                             \
        make_space_gc(need,heapptr);                                   \
    }
  local void make_space_gc (uintL need, Heap* heapptr);
  local void make_space_gc(need,heapptr)
    var reg2 uintL need;
    var reg1 Heap* heapptr;
    { # (mem.total_room < need) || (heapptr->heap_limit - heapptr->heap_end < need)
      # ist schon abgeprüft, also nicht genügend Platz.
      not_enough_room:
      if (mem.total_room < need)
        { gar_col(); # Garbage Collector aufrufen
          # Teste auf Tastatur-Unterbrechung
          interruptp(
            { pushSTACK(S(gc)); tast_break();
              if ((mem.total_room < need) || (heapptr->heap_limit - heapptr->heap_end < need))
                goto not_enough_room;
                else
                return;
            });
        }
      # Entweder ist jetzt (mem.total_room >= need), oder aber wir haben gerade
      # eine GC durchgeführt. In beiden Fällen konzentrieren wir uns nun
      # darauf, heapptr->heap_limit zu vergrößern.
      { var reg3 aint needed_limit = heapptr->heap_end + need;
        if (needed_limit <= heapptr->heap_limit) # hat die GC ihre Arbeit getan?
          return; # ja -> fertig
        # Aufrunden bis zur nächsten Seitengrenze:
        needed_limit = round_up(needed_limit,map_pagesize); # sicher > heapptr->heap_limit
        # neuen Speicher allozieren:
        if (zeromap((void*)(heapptr->heap_limit),needed_limit - heapptr->heap_limit) <0)
          fehler_speicher_voll();
        heapptr->heap_limit = needed_limit;
      }
      # Jetzt ist sicher (heapptr->heap_limit - heapptr->heap_end >= need).
      # Falls (mem.total_room < need), ignorieren wir das:
      if (mem.total_room < need) { mem.total_room = need; }
    }

#endif

#ifdef SPVW_PAGES

# Schafft Platz für ein neues Objekt.
# Falls keiner vorhanden -> Fehlermeldung.
# make_space(need,heap_ptr,stack_ptr, page);
# > uintL need: angeforderter Platz in Bytes (eine Variable oder Konstante)
# > Heap* heap_ptr: Adresse des Heaps, aus dem der Platz genommen werden soll
# > AVL(AVLID,stack) * stack_ptr: Adressen eines lokalen Stacks,
#   für ein späteres AVL(AVLID,move)
# < Pages page: gefundene Page, wo der Platz ist
  # Der Test, ob Platz vorhanden ist, als Macro, der Rest als Funktion:
  #define make_space(need,heap_ptr,stack_ptr,pagevar)  \
    { pagevar = AVL(AVLID,least)(need,&(heap_ptr)->inuse,stack_ptr);    \
      if (pagevar==EMPTY)                                               \
        { pagevar = make_space_gc(need,&(heap_ptr)->inuse,stack_ptr); } \
    }
  local Pages make_space_gc (uintL need, Pages* pages_ptr, AVL(AVLID,stack) * stack_ptr);
  local Pages make_space_gc(need,pages_ptr,stack_ptr)
    var reg2 uintL need;
    var reg3 Pages* pages_ptr;
    var reg4 AVL(AVLID,stack) * stack_ptr;
    { # AVL(AVLID,least)(need,pages_ptr,stack_ptr) == EMPTY
      # ist schon abgeprüft, also
        # Nicht genügend Platz
        not_enough_room:
        #define handle_interrupt_after_gc()  \
          { # Teste auf Tastatur-Unterbrechung                                    \
            interruptp(                                                           \
              { pushSTACK(S(gc)); tast_break();                                   \
               {var reg1 Pages page = AVL(AVLID,least)(need,pages_ptr,stack_ptr); \
                if (page==EMPTY) goto not_enough_room;                            \
                  else                                                            \
                  return page;                                                    \
              }});                                                                \
          }
        #define make_space_using_malloc()  \
          # versuche, beim Betriebssystem Platz zu bekommen:                        \
          { var reg5 uintL size1 = round_up(need,sizeof(cons_));                    \
            if (size1 < std_page_size) { size1 = std_page_size; }                   \
           {var reg7 uintL size2 = size1 + sizeof(NODE) + (Varobject_alignment-1);  \
            var reg6 aint addr = (aint)mymalloc(size2);                             \
            if (!((void*)addr == NULL))                                             \
              { # Page vom Betriebssystem bekommen.                                 \
                var reg1 Pages page = (Pages)addr;                                  \
                page->m_start = addr; page->m_length = size2;                       \
                # Initialisieren:                                                   \
                page->page_start = page->page_end =                                 \
                  round_up((aint)page+sizeof(NODE),Varobject_alignment);            \
                page->page_room = size1;                                            \
                # Diesem Heap zuschlagen:                                           \
                *pages_ptr = AVL(AVLID,insert1)(page,*pages_ptr);                   \
                if (!(AVL(AVLID,least)(need,pages_ptr,stack_ptr) == page)) abort(); \
                return page;                                                        \
          }}  }
        if ((need <= std_page_size) && !(mem.free_pages == NULL))
          { # Eine normalgroße Page aus dem allgemeinen Pool entnehmen:
            var reg1 Pages page = mem.free_pages;
            mem.free_pages = page->page_gcpriv.next;
            # page ist bereits korrekt initialisiert:
            # page->page_start = page->page_end =
            #   round_up((aint)page+sizeof(NODE),Varobject_alignment);
            # page->page_room =
            #   round_down(page->m_start + page->m_length,Varobject_alignment)
            # und diesem Heap zuschlagen:
            *pages_ptr = AVL(AVLID,insert1)(page,*pages_ptr);
            if (!(AVL(AVLID,least)(need,pages_ptr,stack_ptr) == page)) abort();
            return page;
          }
        if (used_space()+need < 5*floor(mem.last_gcend_space,4))
          # Benutzter Platz ist seit der letzten GC noch nicht einmal um 25%
          # angewachsen -> versuche es erstmal beim Betriebssystem;
          # die GC machen wir, wenn die 25%-Grenze erreicht ist.
          { make_space_using_malloc(); }
        { gar_col(); # Garbage Collector aufrufen
          handle_interrupt_after_gc();
          # und wieder testen:
         {var reg1 Pages page = AVL(AVLID,least)(need,pages_ptr,stack_ptr);
          if (page==EMPTY)
            { if (!mem.last_gc_compacted)
                { gar_col_compact(); # kompaktierenden Garbage Collector aufrufen
                  handle_interrupt_after_gc();
                  page = AVL(AVLID,least)(need,pages_ptr,stack_ptr);
                }
              if (page==EMPTY)
                # versuche es nun doch beim Betriebssystem:
                { make_space_using_malloc();
                  fehler_speicher_voll();
            }   }
          # .reserve behandeln??
          return page;
        }}
        #undef make_space_using_malloc
        #undef handle_interrupt_after_gc
    }

#endif

# Macro zur Speicher-Allozierung eines Lisp-Objekts:
# allocate(type,flag,size,ptrtype,ptr,statement)
# > type: Expression, die den Typcode liefert
# > flag: ob Objekt variabler Länge oder nicht
# > size: Expression (constant oder var), die die Größe des benötigten
#         Speicherstücks angibt
# ptrtype: C-Typ von ptr
# ptr: C-Variable
# Ein Speicherstück der Länge size, passend zu einem Lisp-Objekt vom Typ type,
# wird geholt und ptr auf seine Anfangsadresse gesetzt. Dann wird statement
# ausgeführt (Initialisierung des Speicherstücks) und schließlich ptr,
# mit der korrekten Typinfo versehen, als Ergebnis geliefert.
  #ifdef SPVW_BLOCKS
   #ifdef SPVW_MIXED
    #define allocate(type_expr,flag,size_expr,ptrtype,ptrvar,statement)  \
      allocate_##flag (type_expr,size_expr,ptrtype,ptrvar,statement)
    # Objekt variabler Länge:
    #define allocate_TRUE(type_expr,size_expr,ptrtype,ptrvar,statement)  \
      { make_space(size_expr);                                                        \
        set_break_sem_1(); # Break sperren                                            \
       {var reg1 ptrtype ptrvar;                                                      \
        var reg4 object obj;                                                          \
        ptrvar = (ptrtype) mem.objects.end; # Pointer auf Speicherstück               \
        mem.objects.end += (size_expr); # Speicheraufteilung berichtigen              \
        ptrvar->GCself = obj = type_pointer_object(type_expr,ptrvar); # Selbstpointer \
        statement; # Speicherstück initialisieren                                     \
        clr_break_sem_1(); # Break ermöglichen                                        \
        CHECK_GC_CONSISTENCY();                                                       \
        return obj;                                                                   \
      }}
    # Cons o.ä.:
    #define allocate_FALSE(type_expr,size_expr,ptrtype,ptrvar,statement)  \
      { make_space(size_expr);                                                         \
        set_break_sem_1(); # Break sperren                                             \
       {var reg1 ptrtype ptrvar;                                                       \
        ptrvar = (ptrtype)(mem.conses.start -= size_expr); # Pointer auf Speicherstück \
        statement; # Speicherstück initialisieren                                      \
        clr_break_sem_1(); # Break ermöglichen                                         \
        CHECK_GC_CONSISTENCY();                                                        \
        return type_pointer_object(type_expr,ptrvar);                                  \
      }}
   #endif
   #ifdef SPVW_PURE
    #define allocate(type_expr,flag,size_expr,ptrtype,ptrvar,statement)  \
      { var reg4 tint _type = (type_expr);                                 \
        var reg3 Heap* heapptr = &mem.heaps[_type];                        \
        make_space(size_expr,heapptr);                                     \
        set_break_sem_1(); # Break sperren                                 \
       {var reg1 ptrtype ptrvar = (ptrtype)(heapptr->heap_end); # Pointer auf Speicherstück \
        heapptr->heap_end += (size_expr); # Speicheraufteilung berichtigen \
        mem.total_room -= (size_expr);                                     \
        allocate_##flag (ptrvar);                                          \
        statement; # Speicherstück initialisieren                          \
        clr_break_sem_1(); # Break ermöglichen                             \
        CHECK_GC_CONSISTENCY();                                            \
        return (object)ptrvar;                                             \
      }}
    # Objekt variabler Länge:
    #define allocate_TRUE(ptrvar)  \
      ptrvar->GCself = (object)ptrvar; # Selbstpointer eintragen
    # Cons o.ä.:
    #define allocate_FALSE(ptrvar)
   #endif
  #endif
  #ifdef SPVW_PAGES
    #define allocate(type_expr,flag,size_expr,ptrtype,ptrvar,statement)  \
      allocate_##flag (type_expr,size_expr,ptrtype,ptrvar,statement)
   #ifdef SPVW_MIXED
    # Objekt variabler Länge:
    #define allocate_TRUE(type_expr,size_expr,ptrtype,ptrvar,statement)  \
      { # Suche nach der Page mit dem kleinsten page_room >= size_expr:               \
        var AVL(AVLID,stack) stack;                                                   \
        var reg2 Pages page;                                                          \
        make_space(size_expr,&mem.objects,&stack, page);                              \
        set_break_sem_1(); # Break sperren                                            \
       {var reg1 ptrtype ptrvar =                                                     \
          (ptrtype)(page->page_end); # Pointer auf Speicherstück                      \
        var reg4 object obj;                                                          \
        ptrvar->GCself = obj = type_pointer_object(type_expr,ptrvar); # Selbstpointer \
        statement; # Speicherstück initialisieren                                     \
        page->page_room -= (size_expr); # Speicheraufteilung berichtigen              \
        page->page_end += (size_expr);                                                \
        AVL(AVLID,move)(&stack); # Page wieder an die richtige Position hängen        \
        clr_break_sem_1(); # Break ermöglichen                                        \
        CHECK_AVL_CONSISTENCY();                                                      \
        CHECK_GC_CONSISTENCY();                                                       \
        return obj;                                                                   \
      }}
    # Cons o.ä.:
    #define allocate_FALSE(type_expr,size_expr,ptrtype,ptrvar,statement)  \
      { # Suche nach der Page mit dem kleinsten page_room >= size_expr = 8: \
        var reg2 Pages page;                                                \
        # 1. Versuch: letzte benutzte Page                                  \
        page = mem.conses.lastused;                                         \
        if (page->page_room == 0) # Test auf page->page_room < size_expr = sizeof(cons_) \
          { var AVL(AVLID,stack) stack;                                     \
            # 2. Versuch:                                                   \
            make_space(size_expr,&mem.conses,&stack, page);                 \
            mem.conses.lastused = page;                                     \
          }                                                                 \
        set_break_sem_1(); # Break sperren                                  \
       {var reg1 ptrtype ptrvar =                                           \
          (ptrtype)(page->page_end); # Pointer auf Speicherstück            \
        statement; # Speicherstück initialisieren                           \
        page->page_room -= (size_expr); # Speicheraufteilung berichtigen    \
        page->page_end += (size_expr);                                      \
        # Da page_room nun =0 geworden oder >=sizeof(cons_) geblieben ist,  \
        # ist die Sortierreihenfolge der Pages unverändert geblieben.       \
        clr_break_sem_1(); # Break ermöglichen                              \
        CHECK_AVL_CONSISTENCY();                                            \
        CHECK_GC_CONSISTENCY();                                             \
        return type_pointer_object(type_expr,ptrvar);                       \
      }}
   #endif
   #ifdef SPVW_PURE
    # Objekt variabler Länge:
    #define allocate_TRUE(type_expr,size_expr,ptrtype,ptrvar,statement)  \
      { # Suche nach der Page mit dem kleinsten page_room >= size_expr:           \
        var AVL(AVLID,stack) stack;                                               \
        var reg2 Pages page;                                                      \
        var reg4 tint _type = (type_expr);                                        \
        make_space(size_expr,&mem.heaps[_type],&stack, page);                     \
        set_break_sem_1(); # Break sperren                                        \
       {var reg1 ptrtype ptrvar =                                                 \
          (ptrtype)(page->page_end); # Pointer auf Speicherstück                  \
        var reg5 object obj;                                                      \
        ptrvar->GCself = obj = type_pointer_object(_type,ptrvar); # Selbstpointer \
        statement; # Speicherstück initialisieren                                 \
        page->page_room -= (size_expr); # Speicheraufteilung berichtigen          \
        page->page_end += (size_expr);                                            \
        AVL(AVLID,move)(&stack); # Page wieder an die richtige Position hängen    \
        clr_break_sem_1(); # Break ermöglichen                                    \
        CHECK_AVL_CONSISTENCY();                                                  \
        CHECK_GC_CONSISTENCY();                                                   \
        return obj;                                                               \
      }}
    # Cons o.ä.:
    #define allocate_FALSE(type_expr,size_expr,ptrtype,ptrvar,statement)  \
      { # Suche nach der Page mit dem kleinsten page_room >= size_expr = 8: \
        var reg2 Pages page;                                                \
        var reg4 tint _type = (type_expr);                                  \
        var reg3 Heap* heapptr = &mem.heaps[_type];                         \
        # 1. Versuch: letzte benutzte Page                                  \
        page = heapptr->lastused;                                           \
        if (page->page_room == 0) # Test auf page->page_room < size_expr = sizeof(cons_) \
          { var AVL(AVLID,stack) stack;                                     \
            # 2. Versuch:                                                   \
            make_space(size_expr,heapptr,&stack, page);                     \
            heapptr->lastused = page;                                       \
          }                                                                 \
        set_break_sem_1(); # Break sperren                                  \
       {var reg1 ptrtype ptrvar =                                           \
          (ptrtype)(page->page_end); # Pointer auf Speicherstück            \
        statement; # Speicherstück initialisieren                           \
        page->page_room -= (size_expr); # Speicheraufteilung berichtigen    \
        page->page_end += (size_expr);                                      \
        # Da page_room nun =0 geworden oder >=sizeof(cons_) geblieben ist,  \
        # ist die Sortierreihenfolge der Pages unverändert geblieben.       \
        clr_break_sem_1(); # Break ermöglichen                              \
        CHECK_AVL_CONSISTENCY();                                            \
        CHECK_GC_CONSISTENCY();                                             \
        return type_pointer_object(_type,ptrvar);                           \
      }}
   #endif
  #endif

# UP, beschafft ein Cons
# allocate_cons()
# < ergebnis: Pointer auf neues CONS, mit CAR und CDR =NIL
# kann GC auslösen
  global object allocate_cons (void);
  global object allocate_cons()
    { allocate(cons_type,FALSE,sizeof(cons_),Cons,ptr,
               { ptr->cdr = NIL; ptr->car = NIL; }
              )
    }

# UP: Liefert ein neu erzeugtes uninterniertes Symbol mit gegebenem Printnamen.
# make_symbol(string)
# > string: Simple-String
# < ergebnis: neues Symbol mit diesem Namen, mit Home-Package=NIL.
# kann GC auslösen
  global object make_symbol (object string);
  global object make_symbol(string)
    var reg3 object string;
    { pushSTACK(string); # String retten
      allocate(symbol_type,TRUE,size_symbol(),Symbol,ptr,
               { ptr->symvalue = unbound; # leere Wertzelle
                 ptr->symfunction = unbound; # leere Funktionszelle
                 ptr->proplist = NIL; # leere Propertyliste
                 ptr->pname = popSTACK(); # Namen eintragen
                 ptr->homepackage = NIL; # keine Home-Package
               }
              )
    }

# UP, beschafft Vektor
# allocate_vector(len)
# > len: Länge des Vektors
# < ergebnis: neuer Vektor (Elemente werden mit NIL initialisiert)
# kann GC auslösen
  global object allocate_vector (uintL len);
  global object allocate_vector (len)
    var reg2 uintL len;
    { var reg3 uintL need = size_svector(len); # benötigter Speicherplatz
      allocate(svector_type,TRUE,need,Svector,ptr,
               { ptr->length = len;
                {var reg1 object* p = &ptr->data[0];
                 dotimesL(len,len, { *p++ = NIL; } ); # Elemente mit NIL vollschreiben
               }}
              )
    }

# UP, beschafft Bit-Vektor
# allocate_bit_vector(len)
# > len: Länge des Bitvektors (in Bits)
# < ergebnis: neuer Bitvektor (LISP-Objekt)
# kann GC auslösen
  global object allocate_bit_vector (uintL len);
  global object allocate_bit_vector (len)
    var reg2 uintL len;
    { var reg3 uintL need = size_sbvector(len); # benötigter Speicherplatz in Bytes
      allocate(sbvector_type,TRUE,need,Sbvector,ptr,
               { ptr->length = len; } # Keine weitere Initialisierung
              )
    }

# UP, beschafft String
# allocate_string(len)
# > len: Länge des Strings (in Bytes)
# < ergebnis: neuer Simple-String (LISP-Objekt)
# kann GC auslösen
  global object allocate_string (uintL len);
  global object allocate_string (len)
    var reg2 uintL len;
    { var reg4 uintL need = size_sstring(len); # benötigter Speicherplatz in Bytes
      allocate(sstring_type,TRUE,need,Sstring,ptr,
               { ptr->length = len; } # Keine weitere Initialisierung
              )
    }

# UP, beschafft Array
# allocate_array(flags,rank,type)
# > uintB flags: Flags
# > uintC rank: Rang
# > tint type: Typinfo
# < ergebnis: LISP-Objekt Array
# kann GC auslösen
  global object allocate_array (uintB flags, uintC rank, tint type);
  global object allocate_array(flags,rank,type)
    var reg3 uintB flags;
    var reg5 uintC rank;
    var reg6 tint type;
    { var reg2 uintL need = rank;
      if (flags & bit(arrayflags_fillp_bit)) { need += 1; }
      if (flags & bit(arrayflags_dispoffset_bit)) { need += 1; }
      need = size_array(need);
      allocate(type,TRUE,need,Array,ptr,
               { ptr->flags = flags; ptr->rank = rank; # Flags und Rang eintragen
                 ptr->data = NIL; # Datenvektor mit NIL initialisieren
               }
              )
    }

# UP, beschafft Record
# allocate_record_(flags_rectype,reclen,type)
# > uintW flags_rectype: Flags, nähere Typinfo
# > uintC reclen: Länge
# > tint type: Typinfo
# < ergebnis: LISP-Objekt Record (Elemente werden mit NIL initialisiert)
# kann GC auslösen
  global object allocate_record_ (uintW flags_rectype, uintC reclen, tint type);
  global object allocate_record_(flags_rectype,reclen,type)
    var reg3 uintW flags_rectype;
    var reg2 uintC reclen;
    var reg5 tint type;
    { var reg2 uintL need = size_record(reclen);
      allocate(type,TRUE,need,Record,ptr,
               { *(uintW*)pointerplus(ptr,offsetof(record_,recflags)) = flags_rectype; # Flags, Typ eintragen
                 ptr->reclength = reclen; # Länge eintragen
                {var reg1 object* p = &ptr->recdata[0];
                 dotimespC(reclen,reclen, { *p++ = NIL; } ); # Elemente mit NIL vollschreiben
               }}
              )
    }

#ifndef case_stream

# UP, beschafft Stream
# allocate_stream(flags,rectype,reclen)
# > uintB strmflags: Flags
# > uintB strmtype: nähere Typinfo
# > uintC reclen: Länge
# < ergebnis: LISP-Objekt Stream (Elemente werden mit NIL initialisiert)
# kann GC auslösen
  global object allocate_stream (uintB strmflags, uintB strmtype, uintC reclen);
  global object allocate_stream(strmflags,strmtype,reclen)
    var reg3 uintB strmflags;
    var reg4 uintB strmtype;
    var reg2 uintC reclen;
    { var reg1 object obj = allocate_record(0,Rectype_Stream,reclen,orecord_type);
      TheRecord(obj)->recdata[0] = Fixnum_0; # Fixnum als Platz für strmflags und strmtype
      TheStream(obj)->strmflags = strmflags; TheStream(obj)->strmtype = strmtype;
      return obj;
    }

#endif

#ifdef FOREIGN

# UP, beschafft Foreign-Verpackung
# allocate_foreign(foreign)
# > foreign: vom Typ FOREIGN
# < ergebnis: LISP-Objekt, das foreign enthält
# kann GC auslösen
  global object allocate_foreign (FOREIGN foreign);
  global object allocate_foreign(foreign)
    var reg2 FOREIGN foreign;
    { var reg1 object result = allocate_bit_vector(sizeof(FOREIGN)*8);
      TheForeign(result) = foreign;
      return result;
    }

#endif

#ifdef FOREIGN_HANDLE

# UP, beschafft Handle-Verpackung
# allocate_handle(handle)
# < ergebnis: LISP-Objekt, das handle enthält
  global object allocate_handle (Handle handle);
  global object allocate_handle(handle)
    var reg2 Handle handle;
    { var reg1 object result = allocate_bit_vector(sizeof(Handle)*8);
      TheHandle(result) = handle;
      return result;
    }

#endif

# UP, beschafft Bignum
# allocate_bignum(len,sign)
# > uintC len: Länge der Zahl (in Worten)
# > sintB sign: Flag für Vorzeichen (0 = +, -1 = -)
# < ergebnis: neues Bignum (LISP-Objekt)
# kann GC auslösen
  global object allocate_bignum (uintC len, sintB sign);
  global object allocate_bignum(len,sign)
    var reg3 uintC len;
    var reg5 sintB sign;
    { var reg4 uintL need = size_bignum(len); # benötigter Speicherplatz in Bytes
      allocate(bignum_type | (sign & bit(sign_bit_t)),TRUE,need,Bignum,ptr,
               { ptr->length = len; } # Keine weitere Initialisierung
              )
    }

# UP, beschafft Single-Float
# allocate_ffloat(value)
# > ffloat value: Zahlwert (Bit 31 = Vorzeichen)
# < ergebnis: neues Single-Float (LISP-Objekt)
# kann GC auslösen
  global object allocate_ffloat (ffloat value);
  #ifndef WIDE
  global object allocate_ffloat(value)
    var reg3 ffloat value;
    { allocate(ffloat_type | ((sint32)value<0 ? bit(sign_bit_t) : 0) # Vorzeichenbit aus value
               ,TRUE,size_ffloat(),Ffloat,ptr,
               { ptr->float_value = value; }
              )
    }
  #else
  global object allocate_ffloat(value)
    var reg3 ffloat value;
    { return
        type_data_object(ffloat_type | ((sint32)value<0 ? bit(sign_bit_t) : 0), # Vorzeichenbit aus value
                         value
                        );
    }
  #endif

# UP, beschafft Double-Float
# allocate_dfloat(semhi,mlo)
# > semhi,mlo: Zahlwert (Bit 31 von semhi = Vorzeichen)
# < ergebnis: neues Double-Float (LISP-Objekt)
# kann GC auslösen
  global object allocate_dfloat (uint32 semhi, uint32 mlo);
  global object allocate_dfloat(semhi,mlo)
    var reg3 uint32 semhi;
    var reg5 uint32 mlo;
    { allocate(dfloat_type | ((sint32)semhi<0 ? bit(sign_bit_t) : 0) # Vorzeichenbit aus value
               ,TRUE,size_dfloat(),Dfloat,ptr,
               { ptr->float_value.semhi = semhi; ptr->float_value.mlo = mlo; }
              )
    }

# UP, beschafft Long-Float
# allocate_lfloat(len,expo,sign)
# > uintC len: Länge der Mantisse (in Worten)
# > uintL expo: Exponent
# > signean sign: Vorzeichen (0 = +, -1 = -)
# < ergebnis: neues Long-Float, noch ohne Mantisse
# Ein LISP-Objekt liegt erst dann vor, wenn die Mantisse eingetragen ist!
# kann GC auslösen
  global object allocate_lfloat (uintC len, uintL expo, signean sign);
  global object allocate_lfloat(len,expo,sign)
    var reg3 uintC len;
    var reg6 uintL expo;
    var reg5 signean sign;
    { var reg4 uintL need = size_lfloat(len); # benötigter Speicherplatz in Bytes
      allocate(lfloat_type | ((tint)sign & bit(sign_bit_t))
               ,TRUE,need,Lfloat,ptr,
               { ptr->len = len; ptr->expo = expo; } # Keine weitere Initialisierung
              )
    }

# UP, erzeugt Bruch
# make_ratio(num,den)
# > object num: Zähler (muß Integer /= 0 sein, relativ prim zu den)
# > object den: Nenner (muß Integer > 1 sein)
# < ergebnis: Bruch
# kann GC auslösen
  global object make_ratio (object num, object den);
  global object make_ratio(num,den)
    var reg4 object num;
    var reg5 object den;
    { pushSTACK(den); pushSTACK(num); # Argumente sichern
     {var reg3 tint type = # Vorzeichen von num übernehmen
        #ifdef fast_mtypecode
        ratio_type | (mtypecode(STACK_0) & bit(sign_bit_t))
        #else
        ratio_type | (typecode(num) & bit(sign_bit_t))
        #endif
        ;
      allocate(type,FALSE,sizeof(ratio_),Ratio,ptr,
               { ptr->rt_num = popSTACK(); # Zähler eintragen
                 ptr->rt_den = popSTACK(); # Nenner eintragen
               }
              )
    }}

# UP, erzeugt komplexe Zahl
# make_complex(real,imag)
# > real: Realteil (muß reelle Zahl sein)
# > imag: Imaginärteil (muß reelle Zahl /= Fixnum 0 sein)
# < ergebnis: komplexe Zahl
# kann GC auslösen
  global object make_complex (object real, object imag);
  global object make_complex(real,imag)
    var reg4 object real;
    var reg5 object imag;
    { pushSTACK(imag); pushSTACK(real);
      allocate(complex_type,FALSE,sizeof(complex_),Complex,ptr,
               { ptr->c_real = popSTACK(); # Realteil eintragen
                 ptr->c_imag = popSTACK(); # Imaginärteil eintragen
               }
              )
    }

# ------------------------------------------------------------------------------
#                   Zirkularitätenfeststellung

# UP: Liefert eine Tabelle aller Zirkularitäten innerhalb eines Objekts.
# (Eine Zirkularität ist ein in diesem Objekt enthaltenes Teil-Objekt,
# auf den es mehr als einen Zugriffsweg gibt.)
# get_circularities(obj,pr_array,pr_closure)
# > object obj: Objekt
# > boolean pr_array: Flag, ob Arrayelemente rekursiv als Teilobjekte gelten
# > boolean pr_closure: Flag, ob Closurekomponenten rekursiv als Teilobjekte gelten
# < ergebnis: T falls Stacküberlauf eintrat,
#             NIL falls keine Zirkularitäten vorhanden,
#             #(0 ...) ein (n+1)-elementiger Vektor, der die Zahl 0 und die n
#                      Zirkularitäten als Elemente enthält, n>0.
# kann GC auslösen
# Methode:
# Markiere rekursiv das Objekt, lege dabei die Zirkularitäten auf den STACK,
# demarkiere rekursiv das Objekt,
# alloziere Vektor für die Zirkularitäten (kann GC auslösen!),
# fülle die Zirkularitäten vom STACK in den Vektor um.
  global object get_circularities (object obj, boolean pr_array, boolean pr_closure);
  typedef struct { boolean pr_array;
                   boolean pr_closure;
                   uintL counter;
                   jmp_buf abbruch_context;
                   object* abbruch_STACK;
                 }
          get_circ_global;
  # Darauf muß man aus den zwei lokalen Routinen heraus zugreifen.
  local void get_circ_mark (object obj, get_circ_global* env);
  local void get_circ_unmark (object obj, get_circ_global* env);
  global object get_circularities(obj,pr_array,pr_closure)
    var object obj;
    var boolean pr_array;
    var boolean pr_closure;
    { var get_circ_global my_global; # Zähler und Kontext (incl. STACK-Wert)
                                     # für den Fall eines Abbruchs
      set_break_sem_1(); # Break unmöglich machen
      if (!setjmp(my_global.abbruch_context)) # Kontext abspeichern
        { my_global.pr_array = pr_array;
          my_global.pr_closure = pr_closure;
          my_global.counter = 0; # Zähler := 0
          my_global.abbruch_STACK = STACK;
          # Die Kontext-Konserve my_global ist jetzt fertig.
          get_circ_mark(obj,&my_global); # Objekt markieren, mehrfache
                                         # Strukturen auf dem STACK ablegen
                                         # in my_global.counter zählen
          get_circ_unmark(obj,&my_global); # Markierungen wieder löschen
          clr_break_sem_1(); # Break wieder möglich
          { var reg2 uintL n = my_global.counter; # Anzahl der Objekte auf dem STACK
            if (n==0)
              return(NIL); # keine da -> NIL zurück und fertig
              else
              { var reg3 object vector = allocate_vector(n+1); # Vektor mit n+1 Elementen
                # füllen:
                var reg1 object* ptr = &TheSvector(vector)->data[0];
                *ptr++ = Fixnum_0; # erstes Element = Fixnum 0
                # restliche Elemente eintragen (mindestens eins):
                dotimespL(n,n, { *ptr++ = popSTACK(); } );
                return(vector); # Vektor als Ergebnis
        } }   }
        else
        # nach Abbruch wegen SP- oder STACK-Überlauf
        { setSTACK(STACK = my_global.abbruch_STACK); # STACK wieder zurücksetzen
          # Der Kontext ist jetzt wiederhergestellt.
          get_circ_unmark(obj,&my_global); # Markierungen wieder löschen
          clr_break_sem_1(); # Break wieder möglich
          return(T); # T als Ergebnis
        }
    }
# UP: markiert das Objekt obj, legt auftretende Zirkularitäten auf den STACK
# und zählt sie in env->counter mit.
  local void get_circ_mark(obj,env)
    var reg3 object obj;
    var reg4 get_circ_global* env;
    { entry:
      switch (typecode(obj)) # je nach Typ
        { case_cons:
            if (marked(TheCons(obj))) goto m_schon_da; # markiert?
            { var reg2 object obj_cdr = Cdr(obj); # Komponenten (ohne Markierungsbit)
              var reg1 object obj_car = Car(obj);
              mark(TheCons(obj)); # markieren
              if (SP_overflow()) # SP-Tiefe überprüfen
                longjmp(env->abbruch_context,TRUE); # Abbruch
              get_circ_mark(obj_car,env); # CAR markieren (rekursiv)
              obj = obj_cdr; goto entry; # CDR markieren (tail-end-rekursiv)
            }
          case_symbol:
            if (marked(TheSymbol(obj))) # markiert?
              if (eq(Symbol_package(obj),NIL)) # uninterniertes Symbol?
                goto m_schon_da; # ja -> war schon da, merken
                else
                goto m_end; # nein -> war zwar schon da, aber unberücksichtigt lassen
            # bisher unmarkiertes Symbol
            mark(TheSymbol(obj)); # markieren
            goto m_end;
          case_bvector: # Bit-Vector
          case_string: # String
          case_bignum: # Bignum
          #ifndef WIDE
          case_ffloat: # Single-Float
          #endif
          case_dfloat: # Double-Float
          case_lfloat: # Long-Float
          case_ratio: # Ratio
          case_complex: # Complex
            # Objekt ohne Komponenten, die ausgegeben werden:
            if (marked(ThePointer(obj))) goto m_schon_da; # markiert?
            # bisher unmarkiert
            mark(ThePointer(obj)); # markieren
            goto m_end;
          case_svector: # Simple-Vector
            if (marked(TheSvector(obj))) goto m_schon_da; # markiert?
            # bisher unmarkiert
            mark(TheSvector(obj)); # markieren
            if (env->pr_array) # Komponenten weiterzuverfolgen?
              { var reg2 uintL count = TheSvector(obj)->length;
                if (!(count==0))
                  # markiere count>0 Komponenten
                  { var reg1 object* ptr = &TheSvector(obj)->data[0];
                    if (SP_overflow()) # SP-Tiefe überprüfen
                      longjmp(env->abbruch_context,TRUE); # Abbruch
                    dotimespL(count,count, { get_circ_mark(*ptr++,env); } ); # markiere Komponenten (rekursiv)
              }   }
            goto m_end;
          case_array1: case_ovector:
            # Nicht-simpler Array mit Komponenten, die Objekte sind:
            if (marked(TheVarobject(obj))) goto m_schon_da; # markiert?
            # bisher unmarkiert
            mark(TheVarobject(obj)); # markieren
            if (env->pr_array) # Komponenten weiterzuverfolgen?
              { obj=TheArray(obj)->data; goto entry; } # Datenvektor (tail-end-rekursiv) markieren
              else
              goto m_end;
          case_closure: # Closure
            if (marked(TheClosure(obj))) goto m_schon_da; # markiert?
            # bisher unmarkiert
            mark(TheClosure(obj)); # markieren
            if (env->pr_closure) # Komponenten weiterzuverfolgen?
              goto m_record_components; # alle Komponenten werden ausgeben (s. unten)
              else # nur den Namen (tail-end-rekursiv) markieren
              { obj=TheClosure(obj)->clos_name; goto entry; }
          case_structure: # Structure
            if (marked(TheStructure(obj))) goto m_schon_da; # markiert?
            # bisher unmarkiert
            mark(TheStructure(obj)); # markieren
            goto m_record_components;
          case_stream: # Stream
            if (marked(TheStream(obj))) goto m_schon_da; # markiert?
            # bisher unmarkiert
            mark(TheStream(obj));
            switch (TheStream(obj)->strmtype)
              { case strmtype_broad:
                case strmtype_concat:
                  goto m_record_components;
                default:
                  goto m_end;
              }
          case_orecord: # sonstigen Record markieren:
            if (marked(TheRecord(obj))) goto m_schon_da; # markiert?
            # bisher unmarkiert
            mark(TheRecord(obj)); # markieren
            switch (TheRecord(obj)->rectype)
              { case Rectype_Hashtable:
                  # Hash-Table: je nach Array-Ausgabe-Flag
                  if (env->pr_array) break; else goto m_end;
                case Rectype_Package:
                  # Packages werden nicht komponentenweise ausgegeben
                  goto m_end;
                case Rectype_Readtable:
                  # Readtables werden nicht komponentenweise ausgegeben
                  goto m_end;
                #ifndef case_structure
                case Rectype_Structure: goto case_structure;
                #endif
                #ifndef case_stream
                case Rectype_Stream: goto case_stream;
                #endif
                default: break;
              }
            # Pathnames, Random-States, Bytes, Loadtimeevals, Aliens und
            # evtl. Hash-Tables werden evtl. komponentenweise ausgegeben.
            m_record_components: # Komponenten eines Records markieren:
              { var reg2 uintC count = TheRecord(obj)->reclength;
                if (!(count==0))
                  # markiere count>0 Komponenten
                  { var reg1 object* ptr = &TheRecord(obj)->recdata[0];
                    if (SP_overflow()) # SP-Tiefe überprüfen
                      longjmp(env->abbruch_context,TRUE); # Abbruch
                    dotimespC(count,count, { get_circ_mark(*ptr++,env); } ); # markiere Komponenten (rekursiv)
              }   }
            goto m_end;
          m_schon_da:
            # Objekt wurde markiert, war aber schon markiert.
            # Es ist eine Zirkularität.
            if (STACK_overflow()) # STACK-Tiefe überprüfen
              longjmp(env->abbruch_context,TRUE); # Abbruch
            # Objekt mit gelöschtem garcol_bit im STACK ablegen:
            pushSTACK(without_mark_bit(obj));
            env->counter++; # und mitzählen
            goto m_end;
          case_machine: # Maschinenpointer
          case_char: # Character
          case_subr: # Subr
          case_fsubr: # Fsubr
          case_system: # Frame-pointer, Read-label, system
          case_fixnum: # Fixnum
          case_sfloat: # Short-Float
          #ifdef WIDE
          case_ffloat: # Single-Float
          #endif
          default:
            # Objekt kann nicht markiert werden -> fertig
            goto m_end;
          m_end: ; # fertig
    }   }
# UP: Demarkiert Objekt obj.
  local void get_circ_unmark(obj,env)
    var reg2 object obj;
    var reg3 get_circ_global* env;
    { entry:
      switch (typecode(obj) & ~bit(garcol_bit_t)) # je nach Typinfo ohne garcol_bit
        { case_cons:
            if (!marked(TheCons(obj))) goto u_end; # schon demarkiert?
            unmark(TheCons(obj)); # demarkieren
            get_circ_unmark(Car(obj),env); # CAR demarkieren (rekursiv)
            obj=Cdr(obj); goto entry; # CDR demarkieren (tail-end-rekursiv)
          case_symbol:
            # Symbol demarkieren. Wertzelle etc. für PRINT unwesentlich.
          case_bvector: # Bit-Vector
          case_string: # String
          case_bignum: # Bignum
          #ifndef WIDE
          case_ffloat: # Single-Float
          #endif
          case_dfloat: # Double-Float
          case_lfloat: # Long-Float
          case_ratio: # Ratio
          case_complex: # Complex
            # Objekt demarkieren, das keine markierten Komponenten hat:
            unmark(ThePointer(obj)); # demarkieren
            goto u_end;
          case_svector:
            # Simple-Vector demarkieren, seine Komponenten ebenfalls:
            if (!marked(TheSvector(obj))) goto u_end; # schon demarkiert?
            unmark(TheSvector(obj)); # demarkieren
            if (env->pr_array) # wurden die Komponenten weiterverfolgt?
              { var reg2 uintL count = TheSvector(obj)->length;
                if (!(count==0))
                  # demarkiere count>0 Komponenten
                  { var reg1 object* ptr = &TheSvector(obj)->data[0];
                    dotimespL(count,count, { get_circ_unmark(*ptr++,env); } ); # demarkiere Komponenten (rekursiv)
              }   }
            goto u_end;
          case_array1: case_ovector:
            # Nicht-simpler Array mit Komponenten, die Objekte sind:
            if (!marked(TheVarobject(obj))) goto u_end; # schon demarkiert?
            unmark(TheVarobject(obj)); # demarkieren
            if (env->pr_array) # wurden die Komponenten weiterverfolgt?
              { obj=TheArray(obj)->data; goto entry; } # Datenvektor (tail-end-rekursiv) demarkieren
              else
              goto u_end;
          case_closure: # Closure demarkieren
            if (!marked(TheClosure(obj))) goto u_end; # schon demarkiert?
            unmark(TheClosure(obj)); # demarkieren
            if (env->pr_closure) # wurden Komponenten weiterverfolgt?
              goto u_record_components; # alle Komponenten werden ausgeben (s. unten)
              else # nur den Namen (tail-end-rekursiv) demarkieren
              { obj=TheClosure(obj)->clos_name; goto entry; }
          case_structure: # Structure demarkieren:
            if (!marked(TheStructure(obj))) goto u_end; # schon demarkiert?
            unmark(TheStructure(obj)); # demarkieren
            goto u_record_components;
          case_stream: # Stream demarkieren:
            if (!marked(TheStream(obj))) goto u_end; # schon demarkiert?
            unmark(TheStream(obj)); # demarkieren
            switch (TheStream(obj)->strmtype)
              { case strmtype_broad:
                case strmtype_concat:
                  goto u_record_components;
                default:
                  goto u_end;
              }
          case_orecord: # sonstigen Record demarkieren:
            if (!marked(TheRecord(obj))) goto u_end; # schon demarkiert?
            unmark(TheRecord(obj)); # demarkieren
            switch (TheRecord(obj)->rectype)
              { case Rectype_Hashtable:
                  # Hash-Table: je nach Array-Ausgabe-Flag
                  if (env->pr_array) break; else goto u_end;
                case Rectype_Package:
                  # Packages werden nicht komponentenweise ausgegeben
                  goto u_end;
                case Rectype_Readtable:
                  # Readtables werden nicht komponentenweise ausgegeben
                  goto u_end;
                #ifndef case_structure
                case Rectype_Structure: goto case_structure;
                #endif
                #ifndef case_stream
                case Rectype_Stream: goto case_stream;
                #endif
                default: break;
              }
            # Pathnames, Random-States, Bytes, Loadtimeevals, Aliens und
            # evtl. Hash-Tables werden evtl. komponentenweise ausgegeben.
            u_record_components: # Komponenten eines Records demarkieren:
              { var reg2 uintC count = TheRecord(obj)->reclength;
                if (!(count==0))
                  # demarkiere count>0 Komponenten
                  { var reg1 object* ptr = &TheRecord(obj)->recdata[0];
                    dotimespC(count,count, { get_circ_unmark(*ptr++,env); } ); # demarkiere Komponenten (rekursiv)
              }   }
            goto u_end;
          case_machine: # Maschinenpointer
          case_char: # Character
          case_subr: # Subr
          case_fsubr: # Fsubr
          case_system: # Frame-pointer, Read-label, system
          case_fixnum: # Fixnum
          case_sfloat: # Short-Float
          #ifdef WIDE
          case_ffloat: # Single-Float
          #endif
          default:
            # Objekt demarkieren, das gar keine Markierung haben kann:
            goto u_end;
          u_end: ; # fertig
    }   }

# ------------------------------------------------------------------------------
#                  Elementare Stringfunktionen

#ifdef ATARI

# Ausgabe eines konstanten ASCIZ-Strings, direkt übers Betriebssystem:
# asciz_out(asciz);
# > char* asciz: ASCIZ-String
  global void asciz_out (char* asciz);
  global void asciz_out(asciz)
    var reg3 char* asciz;
    { var reg1 const uintB* ptr = asciz;
      # Nullbyte suchen und dabei Zeichen ausgeben:
      loop
        { var reg2 uintB c = *ptr++; # nächstes Zeichen
          if (c==0) break;
          BIOS_ConOut(c); # ausgeben
        }
    }

#endif

# UP: Liefert einen LISP-String mit vorgegebenem Inhalt.
# make_string(charptr,len)
# > uintB* charptr: Adresse einer Zeichenfolge
# > uintL len: Länge der Zeichenfolge
# < ergebnis: Simple-String mit den len Zeichen ab charptr als Inhalt
# kann GC auslösen
  global object make_string (const uintB* charptr, uintL len);
  global object make_string(charptr,len)
    var reg2 const uintB* charptr;
    var reg3 uintL len;
    { var reg4 object obj = allocate_string(len); # String allozieren
      var reg1 uintB* ptr = &TheSstring(obj)->data[0];
      # Zeichenfolge von charptr nach ptr kopieren:
      dotimesL(len,len, { *ptr++ = *charptr++; } );
      return(obj);
    }

#ifndef asciz_length
# UP: Liefert die Länge eines ASCIZ-Strings.
# asciz_length(asciz)
# > char* asciz: ASCIZ-String
#       (Adresse einer durch ein Nullbyte abgeschlossenen Zeichenfolge)
# < ergebnis: Länge der Zeichenfolge (ohne Nullbyte)
  global uintL asciz_length (const char * asciz);
  global uintL asciz_length(asciz)
    var reg3 const char* asciz;
    { var reg1 const char* ptr = asciz;
      var reg2 uintL len = 0;
      # Nullbyte suchen und dabei Länge hochzählen:
      while (!( *ptr++ == 0 )) { len++; }
      return len;
    }
#endif

#ifndef asciz_equal
# UP: Vergleicht zwei ASCIZ-Strings.
# asciz_equal(asciz1,asciz2)
# > char* asciz1: erster ASCIZ-String
# > char* asciz2: zweiter ASCIZ-String
# < ergebnis: TRUE falls die Zeichenfolgen gleich sind
  global boolean asciz_equal (const char * asciz1, const char * asciz2);
  global boolean asciz_equal(asciz1,asciz2)
    var reg2 const char* asciz1;
    var reg3 const char* asciz2;
    { # Bytes vergleichen, solange bis das erste Nullbyte kommt:
      loop
        { var reg1 char ch1 = *asciz1++;
          if (!(ch1 == *asciz2++)) goto no;
          if (ch1 == '\0') goto yes;
        }
      yes: return TRUE;
      no: return FALSE;
    }
#endif

# UP: Wandelt einen ASCIZ-String in einen LISP-String um.
# asciz_to_string(asciz)
# > char* asciz: ASCIZ-String
#       (Adresse einer durch ein Nullbyte abgeschlossenen Zeichenfolge)
# < ergebnis: String mit der Zeichenfolge (ohne Nullbyte) als Inhalt
# kann GC auslösen
  global object asciz_to_string (const char * asciz);
  global object asciz_to_string(asciz)
    var reg1 const char* asciz;
    { return make_string((const uintB*)asciz,asciz_length(asciz)); }

# UP: Wandelt einen String in einen ASCIZ-String um.
# string_to_asciz(obj)
# > object obj: String
# < ergebnis: Simple-String mit denselben Zeichen und einem Nullbyte mehr am Schluß
# kann GC auslösen
  global object string_to_asciz (object obj);
  global object string_to_asciz (obj)
    var reg5 object obj;
    { # (vgl. copy_string in CHARSTRG)
      var reg4 object new = allocate_string(vector_length(obj)+1);
          # neuer Simple-String mit einem Byte mehr Länge
      var uintL len;
      var reg1 uintB* sourceptr = unpack_string(obj,&len);
      # Source-String: Länge in len, Bytes ab sourceptr
      var reg2 uintB* destptr = &TheSstring(new)->data[0];
      # Destination-String: Bytes ab destptr
      { # Kopierschleife:
        var reg3 uintL count;
        dotimesL(count,len, { *destptr++ = *sourceptr++; } );
        *destptr++ = 0; # Nullbyte anfügen
      }
      return(new);
    }

#ifndef SP
# Bestimmung (einer Approximation) des SP-Stackpointers.
  global void* SP (void);
  global void* SP()
    { var long dummy;
      return &dummy;
    }
#endif

# Fehlermeldung wegen Erreichen einer unerreichbaren Programmstelle.
# Kehrt nicht zurück.
# fehler_notreached(file,line);
# > file: Filename (mit Anführungszeichen) als konstanter ASCIZ-String
# > line: Zeilennummer
  global nonreturning void fehler_notreached (const char * file, uintL line);
  global nonreturning void fehler_notreached(file,line)
    var reg2 const char * file;
    var reg1 uintL line;
    { pushSTACK(fixnum(line));
      pushSTACK(asciz_to_string(file));
      fehler(
             DEUTSCH ? "Interner Fehler: Anweisung in File ~, Zeile ~ wurde ausgeführt!!" NLstring
                       "Bitte schicken Sie eine Mitteilung an die Programm-Autoren, "
                       "mit der Beschreibung, wie Sie diesen Fehler erzeugt haben!" :
             ENGLISH ? "internal error: statement in file ~, line ~ has been reached!!" NLstring
                       "Please send the authors of the program "
                       "a description how you produced this error!" :
             FRANCAIS ? "Erreur interne : Dans le fichier ~, la ligne ~ fut exécutée!" NLstring
                        "Veuillez signaler aux auteurs du programme comment "
                        "vous avez pu faire apparaître cette erreur, s.v.p.!" :
             ""
            );
    }

# ------------------------------------------------------------------------------
#                       Tastatur-Unterbrechung

#ifdef ATARI

# Typ einer Interruptfunktion:
# interruptfun = Pointer auf eine Funktion ohne Argumente und ohne Ergebnis
  typedef void (*interruptfun)();

# meine eigene kleine VBL-Assembler-Routine, vgl. VBL.Q, VBL.LST:
  local uintW my_VBL_asm[52] = {
      0x5842,0x5241,        # 000000 :58425241                   DC.L    'XBRA'
      0x4C49,0x5350,        # 000004 :4C495350                   DC.L    'LISP'
      0,0,                  # 000008 : ^     4        OLD_VBL:   DS.L    1
                            # 00000C :                NEW_VBL:           ; EIGENE INTERRUPT-ROUTINE
      0x0817,0x0005,        # 00000C :08170005                   BTST    #5,(SP)        ; INTERRUPT AUS USER-MODE?
      0x663C,               # 000010 :663C                       BNE.S   \1             ; NEIN, DANN MAUS/TASTATUR NICHT ABFRAGEN
      0x4AB9,0,0,           # 000012 :4AB900000068               TST.L   BREAK_SEM      ; BREAK MÖGLICH?
      0x6634,               # 000018 :6634                       BNE.S   \1             ; NEIN
      0x48E7,0x8080,        # 00001A :48E78080                   MOVEM.L D0/A0,-(SP)
      0x2079,0,0,           # 00001E :20790000006C               MOVE.L  LINEA,A0       ; ADRESSE DER LINE-A VARIABLEN
      0x3028,0xFDAC,        # 000024 :3028FDAC                   MOVE.W -596(A0),D0     ; MOUSE_BT = AKTUELLER STATUS DER MAUSTASTEN
      0x4640,               # 000028 :4640                       NOT.W D0
      0xC07C,0x0003,        # 00002A :C07C0003                   AND.W #%11,D0          ; BEIDE BITS 0 (LINKS) UND 1 (RECHTS) GESETZT?
      0x661A,               # 00002E :661A                       BNE.S \2               ; NEIN -> WEITER
      0x48E7,0x60E0,        # 000030 :48E760E0                   MOVEM.L D1-D2/A0-A2,-(SP)
                            # 000034 :                           BIOS_KBSHIFT           ; SHIFT-STATUS LESEN
      0x3F3C,0xFFFF,        # 000034 :3F3CFFFF                       MOVE.W #-1,-(SP) ; MODUS=-1
      0x3F3C,0x000B,        # 000038 :3F3C000B                       MOVE.W #11,-(SP) ; KBSHIFT
      0x4E4D,               # 00003C :4E4D                           TRAP #13 ; BIOS
      0x588F,               # 00003E :588F                           ADDQ.L #4,SP
                            # 000040 :                               ENDM
      0xC03C,0x0003,        # 000040 :C03C0003                   AND.B #%00000011,D0    ; SHIFT LINKS, SHIFT RECHTS ISOLIEREN
      0x4CDF,0x0706,        # 000044 :4CDF0706                   MOVEM.L (SP)+,D1-D2/A0-A2
      0x660A,               # 000048 :660A                       BNE.S \ABBRUCH         ; SHIFT GEDRÜCKT -> ABBRUCH
      0x4CDF,0x0101,        # 00004A :4CDF0101        \2:        MOVEM.L (SP)+,D0/A0
      0x2F3A,0xFFB8,        # 00004E :2F3AFFB8        \1:        MOVE.L OLD_VBL(PC),-(SP) ; SONST ALTE ROUTINE ANSPRINGEN
      0x4E75,               # 000052 :4E75                       RTS
                            # 000054 :                \ABBRUCH:  ; BEIDE MAUSTASTEN UND EINE SHIFT-TASTE GEDRÜCKT -> ABBRECHEN
      0x0268,0xFFFC,0xFDAC, # 000054 :0268FFFCFDAC               AND.W   #-4,-596(A0)   ; BEIDE MAUSTASTEN FÜR NICHT GEDRÜCKT ERKLÄREN
      0x504F,               # 00005A :504F                       ADDQ.W  #2*4,SP        ; MOVEM: GERETTETE REGISTER VERGESSEN
      0x321F,               # 00005C :321F                       MOVE.W  (SP)+,D1       ; SR VOR INTERRUPT
      0x588F,               # 00005E :588F                       ADDQ.L  #4,SP          ; PC VERGESSEN
      0x46C1,               # 000060 :46C1                       MOVE    D1,SR          ; UND ZURÜCK IN USER-MODE
      0x4EF9,0,0,           # 000062 :4EF900000070               JMP     TAST_FEHLER
                            # 000068 :
                            # 000068 :                ; EXTERN:
                            # 000068 : ^     4        BREAK_SEM: DS.L    1
                            # 00006C : ^     4        LINEA:     DS.L    1
                            # 000070 :                TAST_FEHLER:
      };
  local nonreturning void tastatur_interrupt (void);
  local nonreturning void tastatur_interrupt()
    { fehler(
             DEUTSCH ? "Abbruch durch Tastatur-Interrupt" :
             ENGLISH ? "User break" :
             FRANCAIS ? "Interruption clavier" :
             ""
            );
    }
  # geretteter VBL-Vektor:
    #define old_VBL  *((interruptfun*) &my_VBL_asm[4])
  # neuer VBL-Vektor:
    #define new_VBL  ((interruptfun) &my_VBL_asm[6])
  # drei Import-Stellen:
    #define new_VBL_fixup_break  *((void**) &my_VBL_asm[10])
    #define new_VBL_fixup_linea  *((void**) &my_VBL_asm[16])
    #define new_VBL_fixup_tast_fehler  *((void**) &my_VBL_asm[50])

#endif

# ------------------------------------------------------------------------------
#                        Initialisierung

# Name des Programms (für Fehlermeldungszwecke)
  local char* program_name;

# Flag, ob System vollständig geladen (für Fehlermeldungsbehandlung)
  local boolean everything_ready = FALSE;

# jmp_buf zur Rückkehr zum Original-Wert des SP beim Programmstart:
  local jmp_buf original_context;

# LISP sofort verlassen:
# quit_sofort(exitcode);
# > exitcode: 0 bei normalem, 1 bei abnormalem Programmende
  # Wir müssen den SP auf den ursprünglichen Wert setzen.
  # (Bei manchen Betriebssystemen wird erst der vom Programm belegte
  # Speicher mit free() zurückgegeben, bevor ihm die Kontrolle entzogen
  # wird. Für diese kurze Zeit muß man den SP vernünftig setzen.)
  local int exitcode;
  #define quit_sofort(xcode)  exitcode = xcode, longjmp(&!original_context,1)

#ifdef ATARI

# Für eigene Tastatur-Abfrage-Routine:
# Keyboard-Input-Stream funktionsfähig machen:
  local void new_keyboard (void);
  # UP: Bit im Betriebssystem setzen, das dafür sorgt, daß Tastendrücke im
  # BIOS-Buffer nicht nur mit Scancode, sondern auch mit Sondertastenstatus
  # abgelegt werden:
    local void with_KbShift (void);
    local void with_KbShift()
      { *(uintB*)0x484 |= bit(3); } # BSET #3,$484
  local void new_keyboard()
    { Supervisor_Exec(with_KbShift); }
# Tastaturabfrage wieder in den ursprünglichen Zustand versetzen:
  local void old_keyboard (void);
  # Bit im Betriebssystem wieder löschen:
    local void without_KbShift (void);
    local void without_KbShift()
      { *(uintB*)0x484 &= ~bit(3); } # BCLR #3,$484
  local void old_keyboard()
    { Supervisor_Exec(without_KbShift); }

#endif

#ifdef HAVE_SIGNALS

# Paßt den Wert von SYS::*PRIN-LINELENGTH* an die aktuelle Breite des
# Terminal-Fensters an.
# update_linelength();
  local void update_linelength (void);
  local void update_linelength()
    { # SYS::*PRIN-LINELENGTH* := Breite des Terminal-Fensters - 1
      # [vgl. 'term.c' in 'calc' von Hans-J. Böhm, Vernon Lee, Alan J. Demers]
      if (isatty(stdout_handle)) # Standard-Output ein Terminal?
        { /* var reg2 int lines = 0; */
          var reg1 int columns = 0;
          #ifdef TIOCGWINSZ
          # Probiere erst ioctl:
          { var struct winsize stdout_window_size;
            if (!( ioctl(stdout_handle,TIOCGWINSZ,&stdout_window_size) <0))
              { /* lines = stdout_window_size.ws_row; */
                columns = stdout_window_size.ws_col;
          }   }
          # Das kann - entgegen der Dokumentation - scheitern!
          if (/* (lines > 0) && */ (columns > 0)) goto OK;
          #endif
          #ifdef VMS
          { var int scrsize[2];
            vms_scrsize(&scrsize);
            /* lines = scrsize[1]; */ columns = scrsize[0];
          }
          if (/* (lines > 0) && */ (columns > 0)) goto OK;
          #endif
          # Nun probieren wir's über termcap:
          { var reg3 char* term_name = getenv("TERM");
            if (term_name==NULL) { term_name = "unknown"; }
           {var char termcap_entry_buf[10000];
            if ( tgetent(&!termcap_entry_buf,term_name) ==1)
              { /* lines = tgetnum("li"); if (lines<0) { lines = 0; } */
                columns = tgetnum("co"); if (columns<0) { columns = 0; }
              }
          }}
          # Hoffentlich enthält columns jetzt einen vernünftigen Wert.
          if (/* (lines > 0) && */ (columns > 0)) goto OK;
          if (FALSE)
            { OK:
              # Wert von SYS::*PRIN-LINELENGTH* verändern:
              Symbol_value(S(prin_linelength)) =
                fixnum(columns-1);
            }
    }   }
#if defined(SIGWINCH) && !defined(NO_ASYNC_INTERRUPTS)
# Signal-Handler für Signal SIGWINCH:
  local void sigwinch_handler (void);
  local void sigwinch_handler()
    { signal_acknowledge(SIGWINCH,&sigwinch_handler);
      update_linelength();
    }
#endif

#ifdef NEED_OWN_UALARM
# Ein Ersatz für die ualarm-Funktion.
  global unsigned int ualarm (unsigned int value, unsigned int interval);
  global unsigned int ualarm(value,interval)
    var reg1 unsigned int value;
    var reg2 unsigned int interval;
    { var struct itimerval itimer;
      itimer.it_value.tv_sec = floor(value,1000000);
      itimer.it_value.tv_usec = value % 1000000;
      itimer.it_interval.tv_sec = floor(interval,1000000);
      itimer.it_interval.tv_usec = interval % 1000000;
      setitimer(ITIMER_REAL,&itimer,NULL);
      return 0; # den Rückgabewert ignorieren wir immer.
    }
#endif

# Eine Tastatur-Unterbrechung (Signal SIGINT, erzeugt durch Ctrl-C)
# wird eine Sekunde lang aufgehoben. In dieser Zeit kann sie mittels
# 'interruptp' auf fortsetzbare Art behandelt werden. Nach Ablauf dieser
# Zeit wird das Programm nichtfortsetzbar unterbrochen.
  global uintB interrupt_pending = FALSE;
# Signal-Handler für Signal SIGINT:
  local void interrupt_handler (void);
  local void interrupt_handler()
    { signal_acknowledge(SIGINT,&interrupt_handler);
  #ifdef PENDING_INTERRUPTS
      if (!interrupt_pending) # Liegt schon ein Interrupt an -> nichts zu tun
        { interrupt_pending = TRUE; # Flag für 'interruptp' setzen
          #ifdef HAVE_UALARM
          # eine halbe Sekunde warten, dann jede 1/20 sec probieren
          ualarm(ticks_per_second/2,ticks_per_second/20);
          #else
          alarm(1); # eine Sekunde warten, weiter geht's dann bei alarm_handler
          #endif
        }
    }
  local void alarm_handler (void);
  local void alarm_handler()
    { # Die Zeit ist nun abgelaufen.
      #ifdef EMUNIX # Verhindere Programm-Beendigung durch SIGALRM
      #ifndef HAVE_UALARM
      alarm(0); # SIGALRM-Timer abbrechen
      #endif
      #endif
      signal_acknowledge(SIGALRM,&alarm_handler);
  #endif # PENDING_INTERRUPTS (!)
    #ifndef NO_ASYNC_INTERRUPTS
      # Warten, bis Unterbrechung erlaubt:
      if (!(break_sems.gesamt == 0))
    #endif
        {
          #ifndef HAVE_UALARM
          alarm(1); # Probieren wir's in einer Sekunde nochmal
          #endif
          return; # Nach kurzer Zeit wird wieder ein SIGALRM ausgelöst.
        }
    #ifndef NO_ASYNC_INTERRUPTS
      # Wir springen jetzt aus dem signal-Handler heraus, weder mit 'return'
      # noch mit 'longjmp'.
      #if !defined(SIGNALBLOCK_POSIX) && !defined(SIGNALBLOCK_SYSV) && defined(SIGNALBLOCK_BSD) # wann??
      # gerade blockiertes Signal entblockieren:
      sigsetmask(sigblock(0) & ~sigmask(SIGALRM));
      #endif
      #ifdef HAVE_SAVED_STACK
      # STACK auf einen sinnvollen Wert setzen:
      if (!(saved_STACK==NULL)) { setSTACK(STACK = saved_STACK); }
      #endif
      # Über 'fehler' in eine Break-Schleife springen:
      fehler(
             DEUTSCH ? "Ctrl-C: Tastatur-Interrupt" :
             ENGLISH ? "Ctrl-C: User break" :
             FRANCAIS ? "Ctrl-C : Interruption clavier" :
             ""
            );
    #endif
    }

#endif

#ifdef ATARI

# GEMDOS-Fehler während Initialisierung behandeln:
  local nonreturning void gemerror (sintW errorcode);
  local nonreturning void gemerror (errorcode)
    var reg1 sintW errorcode;
    { if (everything_ready) # LISP vollständig initialisiert?
        { OS_error(errorcode); } # ja -> übers Lisp ausgeben
        else
        { quit_sofort(1); } # nein -> Lisp sofort abbrechen
    }

#endif

# Umwandlung der Argumenttypen eines FSUBR in einen Code:
  local fsubr_argtype_ fsubr_argtype (uintW req_anz, uintW opt_anz, fsubr_body_ body_flag);
  local fsubr_argtype_ fsubr_argtype(req_anz,opt_anz,body_flag)
    var reg1 uintW req_anz;
    var reg2 uintW opt_anz;
    var reg3 fsubr_body_ body_flag;
    { switch (body_flag)
        { case fsubr_nobody:
            switch (opt_anz)
              { case 0:
                  switch (req_anz)
                    { case 1: return(fsubr_argtype_1_0_nobody);
                      case 2: return(fsubr_argtype_2_0_nobody);
                      default: goto illegal;
                    }
                case 1:
                  switch (req_anz)
                    { case 1: return(fsubr_argtype_1_1_nobody);
                      case 2: return(fsubr_argtype_2_1_nobody);
                      default: goto illegal;
                    }
                default: goto illegal;
              }
          case fsubr_body:
            switch (opt_anz)
              { case 0:
                  switch (req_anz)
                    { case 0: return(fsubr_argtype_0_body);
                      case 1: return(fsubr_argtype_1_body);
                      case 2: return(fsubr_argtype_2_body);
                      default: goto illegal;
                    }
                default: goto illegal;
              }
          default: goto illegal;
        }
      illegal:
        asciz_out(
                  DEUTSCH ? "Unbekannter FSUBR-Argumenttyp" :
                  ENGLISH ? "Unknown signature of an FSUBR" :
                  FRANCAIS ? "Type d'argument inconnu pour FSUBR" :
                  ""
                 );
        quit();
    }

# Umwandlung der Argumenttypen eines SUBR in einen Code:
  local subr_argtype_ subr_argtype (uintW req_anz, uintW opt_anz, subr_rest_ rest_flag, subr_key_ key_flag);
  local subr_argtype_ subr_argtype(req_anz,opt_anz,rest_flag,key_flag)
    var reg1 uintW req_anz;
    var reg2 uintW opt_anz;
    var reg3 subr_rest_ rest_flag;
    var reg4 subr_key_ key_flag;
    { switch (key_flag)
        { case subr_nokey:
            switch (rest_flag)
              { case subr_norest:
                  switch (opt_anz)
                    { case 0:
                        switch (req_anz)
                          { case 0: return(subr_argtype_0_0);
                            case 1: return(subr_argtype_1_0);
                            case 2: return(subr_argtype_2_0);
                            case 3: return(subr_argtype_3_0);
                            case 4: return(subr_argtype_4_0);
                            default: goto illegal;
                          }
                      case 1:
                        switch (req_anz)
                          { case 0: return(subr_argtype_0_1);
                            case 1: return(subr_argtype_1_1);
                            case 2: return(subr_argtype_2_1);
                            case 3: return(subr_argtype_3_1);
                            case 4: return(subr_argtype_4_1);
                            default: goto illegal;
                          }
                      case 2:
                        switch (req_anz)
                          { case 0: return(subr_argtype_0_2);
                            case 1: return(subr_argtype_1_2);
                            case 2: return(subr_argtype_2_2);
                            default: goto illegal;
                          }
                      case 3:
                        switch (req_anz)
                          { case 0: return(subr_argtype_0_3);
                            default: goto illegal;
                          }
                      case 4:
                        switch (req_anz)
                          { case 0: return(subr_argtype_0_4);
                            default: goto illegal;
                          }
                      case 5:
                        switch (req_anz)
                          { case 0: return(subr_argtype_0_5);
                            default: goto illegal;
                          }
                      default: goto illegal;
                    }
                case subr_rest:
                  switch (opt_anz)
                    { case 0:
                        switch (req_anz)
                          { case 0: return(subr_argtype_0_0_rest);
                            case 1: return(subr_argtype_1_0_rest);
                            case 2: return(subr_argtype_2_0_rest);
                            case 3: return(subr_argtype_3_0_rest);
                            default: goto illegal;
                          }
                      default: goto illegal;
                    }
                default: goto illegal;
              }
          case subr_key:
            switch (rest_flag)
              { case subr_norest:
                  switch (opt_anz)
                    { case 0:
                        switch (req_anz)
                          { case 0: return(subr_argtype_0_0_key);
                            case 1: return(subr_argtype_1_0_key);
                            case 2: return(subr_argtype_2_0_key);
                            case 3: return(subr_argtype_3_0_key);
                            case 4: return(subr_argtype_4_0_key);
                            default: goto illegal;
                          }
                      case 1:
                        switch (req_anz)
                          { case 0: return(subr_argtype_0_1_key);
                            case 1: return(subr_argtype_1_1_key);
                            default: goto illegal;
                          }
                      case 2:
                        switch (req_anz)
                          { case 1: return(subr_argtype_1_2_key);
                            default: goto illegal;
                          }
                      default: goto illegal;
                    }
                case subr_rest:
                default: goto illegal;
              }
          case subr_key_allow: goto illegal;
          default: goto illegal;
        }
      illegal:
        asciz_out(
                  DEUTSCH ? "Unbekannter SUBR-Argumenttyp" :
                  ENGLISH ? "Unknown signature of a SUBR" :
                  FRANCAIS ? "Type d'argument inconnu pour SUBR" :
                  ""
                 );
        quit();
    }

# Initialisierungs-Routinen für die Tabellen
# während des 1. Teils der Initialisierungsphase:
  # fsubr_tab initialisieren:
    local void init_fsubr_tab_1 (void);
    local void init_fsubr_tab_1()
      {
        #if defined(INIT_SUBR_TAB)
          #ifdef MAP_MEMORY
            # Tabelle in den vorgesehenen Bereich kopieren:
            fsubr_tab = fsubr_tab_data;
          #endif
          #if !NIL_IS_CONSTANT
          # Erst noch den name-Slot initialisieren:
          { var reg1 fsubr_* ptr = (fsubr_*)&fsubr_tab; # fsubr_tab durchgehen
            #define LISPSPECFORM  LISPSPECFORM_E
            #include "fsubr.c"
            #undef LISPSPECFORM
          }
          #endif
          # Durch SPVWTABF sind schon alle Slots außer argtype initialisiert.
          # Nur noch den argtype-Slot initialisieren:
          { var reg1 fsubr_* ptr = (fsubr_*)&fsubr_tab; # fsubr_tab durchgehen
            var reg2 uintC count;
            dotimesC(count,fsubr_anz,
              { ptr->argtype = (uintW)fsubr_argtype(ptr->req_anz,ptr->opt_anz,ptr->body_flag);
                ptr++;
              });
          }
        #else
          # Alle Slots initialisieren:
          { var reg1 fsubr_* ptr = (fsubr_*)&fsubr_tab; # fsubr_tab durchgehen
            #define LISPSPECFORM  LISPSPECFORM_D
            #include "fsubr.c"
            #undef LISPSPECFORM
          }
        #endif
      }
  # subr_tab initialisieren:
    local void init_subr_tab_1 (void);
    local void init_subr_tab_1()
      {
        #if defined(INIT_SUBR_TAB)
          #ifdef MAP_MEMORY
            # Tabelle in den vorgesehenen Bereich kopieren:
            subr_tab = subr_tab_data;
          #endif
          #if !NIL_IS_CONSTANT
          # Erst noch den name-Slot initialisieren:
          { var reg1 subr_* ptr = (subr_*)&subr_tab; # subr_tab durchgehen
            #define LISPFUN  LISPFUN_E
            #include "subr.c"
            #undef LISPFUN
          }
          # und den keywords-Slot vorläufig initialisieren:
          { var reg1 subr_* ptr = (subr_*)&subr_tab; # subr_tab durchgehen
            var reg2 uintC count = subr_anz;
            dotimesC(count,subr_anz, { ptr->keywords = NIL; ptr++; });
          }
          #endif
          # Durch SPVWTABF sind schon alle Slots außer keywords und argtype
          # initialisiert.
          # Nun den argtype-Slot initialisieren:
          { var reg1 subr_* ptr = (subr_*)&subr_tab; # subr_tab durchgehen
            var reg2 uintC count = subr_anz;
            dotimesC(count,subr_anz,
              { ptr->argtype =
                 (uintW)subr_argtype(ptr->req_anz,ptr->opt_anz,ptr->rest_flag,ptr->key_flag);
                ptr++;
              });
          }
        #else
          # Alle Slots außer keywords initialisieren:
          { var reg1 subr_* ptr = (subr_*)&subr_tab; # subr_tab durchgehen
            #define LISPFUN  LISPFUN_D
            #include "subr.c"
            #undef LISPFUN
          }
        #endif
      }
  # symbol_tab initialisieren:
    local void init_symbol_tab_1 (void);
    local void init_symbol_tab_1()
      {
        #if defined(INIT_SYMBOL_TAB) && NIL_IS_CONSTANT
          #ifdef MAP_MEMORY
            # Tabelle in den vorgesehenen Bereich kopieren:
            symbol_tab = symbol_tab_data;
          #endif
        #else
          #if 0 # wozu so viel Code produzieren?
            { var reg1 symbol_* ptr = (symbol_*)&symbol_tab; # symbol_tab durchgehen
              #define LISPSYM  LISPSYM_B
              #include "constsym.c"
              #undef LISPSYM
            }
          #else
            { var reg1 symbol_* ptr = (symbol_*)&symbol_tab; # symbol_tab durchgehen
              var reg2 uintC count;
              dotimesC(count,symbol_anz,
                { ptr->GCself = symbol_tab_ptr_as_symbol(ptr);
                  ptr->symvalue = unbound;
                  ptr->symfunction = unbound;
                  ptr->proplist = NIL;
                  ptr->pname = NIL;
                  ptr->homepackage = NIL;
                  ptr++;
                });
              #undef ptr_as_symbol
            }
          #endif
        #endif
      }
  # object_tab initialisieren:
    local void init_object_tab_1 (void);
    local void init_object_tab_1()
      {
        #if !(defined(INIT_OBJECT_TAB) && NIL_IS_CONSTANT)
          #if 0 # wozu so viel Code produzieren?
            { var reg1 object* ptr = (object*)&object_tab; # object_tab durchgehen
              #define LISPOBJ  LISPOBJ_B
              #include "constobj.c"
              #undef LISPOBJ
            }
          #else
            { var reg1 object* ptr = (object*)&object_tab; # object_tab durchgehen
              var reg2 uintC count;
              dotimesC(count,object_anz, { *ptr++ = NIL; });
            }
          #endif
        #endif
      }

# Initialisierungs-Routinen für die Tabellen
# während des 2. Teils der Initialisierungsphase:
  # subr_tab fertig initialisieren: Keyword-Vektoren eintragen.
    local void init_subr_tab_2 (void);
    local void init_subr_tab_2()
      #if 0
        # Ich hätt's gern so einfach, aber
        # bei TURBO-C reicht der Speicher zum Compilieren nicht!
        { # subr_tab durchgehen
          var reg2 object vec;
          var reg1 object* vecptr;
          #define LISPFUN  LISPFUN_H
          #define kw(name)  *vecptr++ = S(K##name)
          #include "subr.c"
          #undef LISPFUN
          #undef kw
        }
      #else
        { # Keyword-Vektoren einzeln erzeugen:
          var reg2 object vec;
          var reg1 object* vecptr;
          # füllt ein einzelnes Keyword mehr in den Vektor ein:
            #define kw(name)  *vecptr++ = S(K##name)
          # bildet Vektor mit gegebenen Keywords:
            #define v(key_anz,keywords)  \
              vec = allocate_vector(key_anz), \
              vecptr = &TheSvector(vec)->data[0], \
              keywords;
          # setzt den Vektor als Keyword-Vektor zum SUBR name fest:
            #define s(name)  subr_tab.D_##name.keywords = vec;
          v(7, (kw(adjustable),kw(element_type),kw(initial_element),
                kw(initial_contents),kw(fill_pointer),
                kw(displaced_to),kw(displaced_index_offset)) )
          s(make_array)
          v(6, (kw(element_type),kw(initial_element),
                kw(initial_contents),kw(fill_pointer),
                kw(displaced_to),kw(displaced_index_offset)) )
          s(adjust_array)
          v(4, (kw(start1),kw(end1),kw(start2),kw(end2)) )
          s(string_gleich)
          s(string_ungleich)
          s(string_kleiner)
          s(string_groesser)
          s(string_klgleich)
          s(string_grgleich)
          s(string_equal)
          s(string_not_equal)
          s(string_lessp)
          s(string_greaterp)
          s(string_not_greaterp)
          s(string_not_lessp)
          s(search_string_gleich)
          s(search_string_equal)
          s(replace)
          v(1, (kw(initial_element)) )
          s(make_string)
          s(make_list)
          v(2, (kw(start),kw(end)) )
          s(nstring_upcase)
          s(string_upcase)
          s(nstring_downcase)
          s(string_downcase)
          s(nstring_capitalize)
          s(string_capitalize)
          s(write_string)
          s(write_line)
          s(fill)
          v(5, (kw(initial_contents),
                kw(test),kw(size),kw(rehash_size),kw(rehash_threshold)) )
          s(make_hash_table)
          v(3, (kw(preserve_whitespace),kw(start),kw(end)) )
          s(read_from_string)
          v(4, (kw(start),kw(end),kw(radix),kw(junk_allowed)) )
          s(parse_integer)
          v(12, (kw(case),kw(level),kw(length),kw(gensym),kw(escape),kw(radix),
                 kw(base),kw(array),kw(circle),kw(pretty),kw(closure),kw(stream)) )
          s(write)
          v(11, (kw(case),kw(level),kw(length),kw(gensym),kw(escape),kw(radix),
                 kw(base),kw(array),kw(circle),kw(pretty),kw(closure)) )
          s(write_to_string)
          v(2, (kw(test),kw(test_not)) )
          s(tree_equal)
          v(3, (kw(test),kw(test_not),kw(key)) )
          s(subst)
          s(nsubst)
          s(sublis)
          s(nsublis)
          s(member)
          s(adjoin)
          s(assoc)
          s(rassoc)
          v(1, (kw(key)) )
          s(subst_if)
          s(subst_if_not)
          s(nsubst_if)
          s(nsubst_if_not)
          s(member_if)
          s(member_if_not)
          s(assoc_if)
          s(assoc_if_not)
          s(rassoc_if)
          s(rassoc_if_not)
          s(merge)
          v(2, (kw(nicknames),kw(use)) )
          s(make_package)
          s(in_package)
          v(2, (kw(initial_element),kw(update)) )
          s(make_sequence)
          v(4, (kw(from_end),kw(start),kw(end),kw(initial_value)) )
          s(reduce)
          v(7, (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not),kw(count)) )
          s(remove)
          s(delete)
          s(substitute)
          s(nsubstitute)
          v(5, (kw(from_end),kw(start),kw(end),kw(key),kw(count)) )
          s(remove_if)
          s(remove_if_not)
          s(delete_if)
          s(delete_if_not)
          s(substitute_if)
          s(substitute_if_not)
          s(nsubstitute_if)
          s(nsubstitute_if_not)
          v(6, (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not)) )
          s(remove_duplicates)
          s(delete_duplicates)
          s(find)
          s(position)
          s(count)
          v(4, (kw(from_end),kw(start),kw(end),kw(key)) )
          s(find_if)
          s(find_if_not)
          s(position_if)
          s(position_if_not)
          s(count_if)
          s(count_if_not)
          v(8, (kw(start1),kw(end1),kw(start2),kw(end2),kw(from_end),
                kw(key),kw(test),kw(test_not)) )
          s(mismatch)
          s(search)
          v(3, (kw(key),kw(start),kw(end)) )
          s(sort)
          s(stable_sort)
          v(3, (kw(start),kw(end),kw(junk_allowed)) )
          s(parse_namestring)
          v(7, (kw(defaults),kw(host),kw(device),kw(directory),kw(name),kw(type),kw(version)) )
          s(make_pathname)
          v(4, (kw(direction),kw(element_type),kw(if_exists),kw(if_does_not_exist)) )
          s(open)
          v(1, (kw(full)) )
          s(directory)
          v(1, (kw(abort)) )
          s(close)
          #ifdef REXX
          v(6, (kw(result),kw(string),kw(token),kw(async),kw(io),kw(return)) )
          s(rexx_put)
          #endif
          #undef s
          #undef v
          #undef kw
        }
      #endif
  # symbol_tab zu Ende initialisieren: Printnamen und Home-Package eintragen.
    local void init_symbol_tab_2 (void);
    local void init_symbol_tab_2()
      { # Tabelle der Printnamen:
        local char* pname_table[symbol_anz] =
          {
            #define LISPSYM  LISPSYM_D
            #include "constsym.c"
            #undef LISPSYM
          };
        # Tabelle der Packages:
        #define lisp_index     0
        #define user_index     1
        #define system_index   2
        #define keyword_index  3
        local uintB package_index_table[symbol_anz] =
          {
            #define LISPSYM  LISPSYM_E
            #include "constsym.c"
            #undef LISPSYM
          };
        {var reg1 object list = O(all_packages); # Liste der Packages
         # kurz nach der Initialisierung:
         # (#<PACKAGE LISP> #<PACKAGE USER> #<PACKAGE SYSTEM> #<PACKAGE KEYWORD>)
         pushSTACK(Car(list)); list = Cdr(list); # #<PACKAGE LISP>
         pushSTACK(Car(list)); list = Cdr(list); # #<PACKAGE USER>
         pushSTACK(Car(list)); list = Cdr(list); # #<PACKAGE SYSTEM>
         pushSTACK(Car(list)); list = Cdr(list); # #<PACKAGE KEYWORD>
        }
       {var reg3 symbol_* ptr = (symbol_*)&symbol_tab; # symbol_tab durchgehen
        var reg4 char** pname_ptr = &pname_table[0]; # pname_table durchgehen
        var reg5 uintB* index_ptr = &package_index_table[0]; # package_index_table durchgehen
        var reg6 uintC count;
        dotimesC(count,symbol_anz,
          { ptr->pname = asciz_to_string(*pname_ptr++); # Printnamen eintragen
           {var reg2 uintB index = *index_ptr++;
            var reg1 object* package_ = &STACK_(4-1) STACKop -index; # Pointer auf Package
            pushSTACK(symbol_tab_ptr_as_symbol(ptr)); # Symbol
            import(&STACK_0,package_); # erst normal importieren
            if (index == lisp_index) # in #<PACKAGE LISP> ?
              { export(&STACK_0,package_); } # ja -> auch exportieren
            Symbol_package(popSTACK()) = *package_; # und die Home-Package setzen
            ptr++;
          }});
        skipSTACK(4);
        #undef keyword_index
        #undef system_index
        #undef user_index
        #undef lisp_index
      }}
  # FSUBRs/SUBRs in ihre Symbole eintragen:
    local void init_symbol_functions (void);
    local void init_symbol_functions()
      {# FSUBRs eintragen:
       {var reg1 fsubr_* ptr = (fsubr_*)&fsubr_tab; # fsubr_tab durchgehen
        var reg2 uintC count;
        dotimesC(count,fsubr_anz,
          { Symbol_function(ptr->name) = fsubr_tab_ptr_as_fsubr(ptr);
            ptr++;
          });
       }
       # SUBRs eintragen:
       {var reg1 subr_* ptr = (subr_*)&subr_tab; # subr_tab durchgehen
        var reg2 uintC count;
        dotimesC(count,subr_anz,
          { Symbol_function(ptr->name) = subr_tab_ptr_as_subr(ptr);
            ptr++;
          });
      }}
  # Konstanten/Variablen ihre Werte zuweisen:
    local void init_symbol_values (void);
    local void init_symbol_values()
      { # Hilfsmacro: Konstante := wert+1
        #define define_constant_UL1(symbol,wert)  \
          { var reg1 object x = # wert+1 als Integer             \
              ( ((uintL)(wert) < (uintL)(bitm(oint_addr_len)-1)) \
                ? fixnum(wert+1)                                 \
                : I_1_plus_I(UL_to_I(wert))                      \
              );                                                 \
            define_constant(symbol,x);                           \
          }
        # allgemein:
        define_constant(S(nil),S(nil));                 # NIL := NIL
        define_constant(S(t),S(t));                     # T := T
        # zu EVAL/CONTROL:
        define_constant_UL1(S(lambda_parameters_limit),lp_limit_1); # LAMBDA-PARAMETERS-LIMIT := lp_limit_1 + 1
        define_constant_UL1(S(call_arguments_limit),ca_limit_1); # CALL-ARGUMENTS-LIMIT := ca_limit_1 + 1
        define_constant(S(multiple_values_limit),       # MULTIPLE-VALUES-LIMIT
          fixnum(mv_limit));      # := mv_limit
        define_constant(S(jmpbuf_size),                 # SYS::*JMPBUF-SIZE* := Größe eines jmp_buf
          fixnum(jmpbufsize));
        define_constant(S(big_endian),(BIG_ENDIAN_P ? T : NIL)); # SYS::*BIG-ENDIAN* := NIL bzw. T
        define_variable(S(macroexpand_hook),L(pfuncall)); # *MACROEXPAND-HOOK* := #'SYS::%FUNCALL
        define_variable(S(evalhookstern),NIL);          # *EVALHOOK*
        define_variable(S(applyhookstern),NIL);         # *APPLYHOOK*
        # zu PACKAGE:
        define_variable(S(packagestern),Car(O(all_packages))); # *PACKAGE* := '#<PACKAGE LISP>
        # zu LISPARIT:
        init_arith(); # definiert folgende:
        # define_variable(S(pi),);                      # PI
        # define_constant(S(most_positive_fixnum),);    # MOST-POSITIVE-FIXNUM
        # define_constant(S(most_negative_fixnum),);    # MOST-NEGATIVE-FIXNUM
        # define_constant(S(most_positive_short_float),); # MOST-POSITIVE-SHORT-FLOAT
        # define_constant(S(least_positive_short_float),); # LEAST-POSITIVE-SHORT-FLOAT
        # define_constant(S(least_negative_short_float),); # LEAST-NEGATIVE-SHORT-FLOAT
        # define_constant(S(most_negative_short_float),); # MOST-NEGATIVE-SHORT-FLOAT
        # define_constant(S(most_positive_single_float),); # MOST-POSITIVE-SINGLE-FLOAT
        # define_constant(S(least_positive_single_float),); # LEAST-POSITIVE-SINGLE-FLOAT
        # define_constant(S(least_negative_single_float),); # LEAST-NEGATIVE-SINGLE-FLOAT
        # define_constant(S(most_negative_single_float),); # MOST-NEGATIVE-SINGLE-FLOAT
        # define_constant(S(most_positive_double_float),); # MOST-POSITIVE-DOUBLE-FLOAT
        # define_constant(S(least_positive_double_float),); # LEAST-POSITIVE-DOUBLE-FLOAT
        # define_constant(S(least_negative_double_float),); # LEAST-NEGATIVE-DOUBLE-FLOAT
        # define_constant(S(most_negative_double_float),); # MOST-NEGATIVE-DOUBLE-FLOAT
        # define_variable(S(most_positive_long_float),); # MOST-POSITIVE-LONG-FLOAT
        # define_variable(S(least_positive_long_float),); # LEAST-POSITIVE-LONG-FLOAT
        # define_variable(S(least_negative_long_float),); # LEAST-NEGATIVE-LONG-FLOAT
        # define_variable(S(most_negative_long_float),); # MOST-NEGATIVE-LONG-FLOAT
        # define_constant(S(short_float_epsilon),);     # SHORT-FLOAT-EPSILON
        # define_constant(S(single_float_epsilon),);    # SINGLE-FLOAT-EPSILON
        # define_constant(S(double_float_epsilon),);    # DOUBLE-FLOAT-EPSILON
        # define_variable(S(long_float_epsilon),);      # LONG-FLOAT-EPSILON
        # define_constant(S(short_float_negative_epsilon),); # SHORT-FLOAT-NEGATIVE-EPSILON
        # define_constant(S(single_float_negative_epsilon),); # SINGLE-FLOAT-NEGATIVE-EPSILON
        # define_constant(S(double_float_negative_epsilon),); # DOUBLE-FLOAT-NEGATIVE-EPSILON
        # define_variable(S(long_float_negative_epsilon),); # LONG-FLOAT-NEGATIVE-EPSILON
        # define_variable(S(read_default_float_format),); # *READ-DEFAULT-FLOAT-FORMAT*
        # define_variable(S(random_state),);            # *RANDOM-STATE*
        # zu ARRAY:
        define_constant_UL1(S(array_total_size_limit),arraysize_limit_1); # ARRAY-TOTAL-SIZE-LIMIT := arraysize_limit_1 + 1
        define_constant_UL1(S(array_dimension_limit),arraysize_limit_1); # ARRAY-DIMENSION-LIMIT := arraysize_limit_1 + 1
        define_constant_UL1(S(array_rank_limit),arrayrank_limit_1); # ARRAY-RANK-LIMIT := arrayrank_limit_1 + 1
        # zu DEBUG:
        define_variable(S(plus),NIL);                   # +
        define_variable(S(plus2),NIL);                  # ++
        define_variable(S(plus3),NIL);                  # +++
        define_variable(S(minus),NIL);                  # -
        define_variable(S(mal),NIL);                    # *
        define_variable(S(mal2),NIL);                   # **
        define_variable(S(mal3),NIL);                   # ***
        define_variable(S(durch),NIL);                  # /
        define_variable(S(durch2),NIL);                 # //
        define_variable(S(durch3),NIL);                 # ///
        define_variable(S(driverstern),NIL);            # *DRIVER* := NIL
        define_variable(S(break_driver),NIL);           # *BREAK-DRIVER* := NIL
        define_variable(S(break_count),Fixnum_0);       # SYS::*BREAK-COUNT* := 0
        # zu STREAM:
        # später: init_streamvars(); # definiert folgende:
        # define_variable(S(standard_input),);          # *STANDARD-INPUT*
        # define_variable(S(standard_output),);         # *STANDARD-OUTPUT*
        # define_variable(S(error_output),);            # *ERROR-OUTPUT*
        # define_variable(S(query_io),);                # *QUERY-IO*
        # define_variable(S(debug_io),);                # *DEBUG-IO*
        # define_variable(S(terminal_io),);             # *TERMINAL-IO*
        # define_variable(S(trace_output),);            # *TRACE-OUTPUT*
        # define_variable(S(keyboard_input),);          # *KEYBOARD-INPUT*
        define_variable(S(default_pathname_defaults),unbound); # *DEFAULT-PATHNAME-DEFAULTS*
        #ifdef PRINTER_ATARI
          define_variable(S(printer_timeout),fixnum(1000)); # *PRINTER-TIMEOUT* := 1000 (= 5 Sekunden)
        #endif
        # zu IO:
        init_reader(); # definiert folgende:
        # define_variable(S(read_base),);               # *READ-BASE* := 10
        # define_variable(S(read_suppress),);           # *READ-SUPPRESS* := NIL
        # define_variable(S(readtablestern),);          # *READTABLE*
        define_variable(S(read_preserve_whitespace),unbound); # SYS::*READ-PRESERVE-WHITESPACE*
        define_variable(S(read_recursive_p),unbound);   # SYS::*READ-RECURSIVE-P*
        define_variable(S(read_reference_table),unbound); # SYS::*READ-REFERENCE-TABLE*
        define_variable(S(backquote_level),unbound);    # SYS::*BACKQUOTE-LEVEL*
        define_variable(S(compiling),NIL);              # SYS::*COMPILING* ;= NIL
        define_variable(S(print_case),S(Kupcase));      # *PRINT-CASE* := :UPCASE
        define_variable(S(print_level),NIL);            # *PRINT-LEVEL* := NIL
        define_variable(S(print_length),NIL);           # *PRINT-LENGTH* := NIL
        define_variable(S(print_gensym),T);             # *PRINT-GENSYM* := T
        define_variable(S(print_escape),T);             # *PRINT-ESCAPE* := T
        define_variable(S(print_radix),NIL);            # *PRINT-RADIX* := NIL
        define_variable(S(print_base),fixnum(10));  # *PRINT-BASE* := 10
        define_variable(S(print_array),T);              # *PRINT-ARRAY* := T
        define_variable(S(print_circle),NIL);           # *PRINT-CIRCLE* := NIL
        define_variable(S(print_pretty),NIL);           # *PRINT-PRETTY* := NIL
        define_variable(S(print_closure),NIL);          # *PRINT-CLOSURE* := NIL
        define_variable(S(print_rpars),T);              # *PRINT-RPARS* := T
        define_variable(S(print_circle_table),unbound); # SYS::*PRINT-CIRCLE-TABLE*
        define_variable(S(prin_level),unbound);         # SYS::*PRIN-LEVEL*
        define_variable(S(prin_stream),unbound);        # SYS::*PRIN-STREAM*
        define_variable(S(prin_linelength),fixnum(79));  # SYS::*PRIN-LINELENGTH* := 79 (vorläufig)
        define_variable(S(prin_l1),unbound);            # SYS::*PRIN-L1*
        define_variable(S(prin_lm),unbound);            # SYS::*PRIN-LM*
        define_variable(S(prin_rpar),unbound);          # SYS::*PRIN-RPAR*
        define_variable(S(prin_jblocks),unbound);       # SYS::*PRIN-JBLOCKS*
        define_variable(S(prin_jbstrings),unbound);     # SYS::*PRIN-JBSTRINGS*
        define_variable(S(prin_jbmodus),unbound);       # SYS::*PRIN-JBMODUS*
        define_variable(S(prin_jblpos),unbound);        # SYS::*PRIN-JBLPOS*
        # zu EVAL:
        define_variable(S(evalhookstern),NIL);          # *EVALHOOK* := NIL
        define_variable(S(applyhookstern),NIL);         # *APPLYHOOK* := NIL
        # zu MISC:
        define_constant(S(internal_time_units_per_second),  # INTERNAL-TIME-UNITS-PER-SECOND
          fixnum(ticks_per_second) ); # := 200 bzw. 1000000
        define_variable(S(error_count),Fixnum_0);       # SYS::*ERROR-COUNT* := 0
        define_variable(S(error_handler),NIL);          # *ERROR-HANDLER* := NIL
        #undef define_constant_UL1
      }
  # sonstige Objekte kreieren und Objekttabelle füllen:
    local void init_object_tab (void);
    local void init_object_tab()
      { # Tabelle mit Initialisierungsstrings:
        local var char* object_initstring_tab []
          = {
             #define LISPOBJ LISPOBJ_D
             #include "constobj.c"
             #undef LISPOBJ
            };
        # *FEATURES* initialisieren:
        { var reg2 char* features_initstring =
            "(CLISP CLTL1 COMMON-LISP INTERPRETER"
            #ifdef FAST_SP
              " SYSTEM::CLISP2"
            #else
              " SYSTEM::CLISP3"
            #endif
            #ifdef ATARI
              " ATARI"
            #endif
            #ifdef AMIGA
              " AMIGA"
            #endif
            #ifdef SUN3
              " SUN3"
            #endif
            #ifdef SUN386
              " SUN386"
            #endif
            #ifdef SUN4
              " SUN4"
            #endif
            #ifdef PC386
              " PC386"
            #endif
            #ifdef MSDOS
             #ifdef OS2
              " OS/2"
             #else
              " DOS"
             #endif
            #endif
            #ifdef UNIX
              " UNIX"
            #endif
            #ifdef VMS
              " VMS"
            #endif
            #if DEUTSCH
              " DEUTSCH"
            #endif
            #if ENGLISH
              " ENGLISH"
            #endif
            #if FRANCAIS
              " FRANCAIS"
            #endif
            ")"
            ;
          pushSTACK(asciz_to_string(features_initstring));
         {var reg1 object list = (funcall(L(read_from_string),1), value1);
          define_variable(S(features),list);             # *FEATURES*
        }}
        # Objekte aus den Strings lesen:
        { var reg1 object* objptr = (object*)&object_tab; # object_tab durchgehen
          var reg2 char** stringptr = &object_initstring_tab[0]; # Stringtabelle durchgehen
          var reg3 uintC count;
          dotimesC(count,object_anz,
            { pushSTACK(asciz_to_string(*stringptr++)); # String
              funcall(L(make_string_input_stream),1); # in Stream verpacken
              pushSTACK(value1);
             {var reg4 object obj = read(&STACK_0,NIL,NIL); # Objekt lesen
              skipSTACK(1);
              if (!eq(obj,dot_value)) { *objptr = obj; } # und eintragen (außer ".")
              objptr++;
            }});
        }
        TheSstring(O(null_string))->data[0] = 0; # Nullbyte in den Null-String einfügen
        Car(O(top_decl_env)) = O(declaration_types); # Toplevel-Deklarations-Environment bauen
        define_constant(S(language),O(language_string)); # *LANGUAGE* := "DEUTSCH" bzw. "ENGLISH" bzw. "FRANCAIS"
      }
  # Zu-Fuß-Initialisierung aller LISP-Daten:
    local void initmem (void);
    local void initmem()
      { init_symbol_tab_1(); # symbol_tab initialisieren
        init_object_tab_1(); # object_tab initialisieren
        # Jetzt sind die Tabellen erst einmal grob initialisiert, bei GC
        # kann nichts passieren.
        # fsubr_tab ist bereits fertig initialisiert.
        # subr_tab fertig initialisieren:
        init_subr_tab_2();
        # Packages initialisieren:
        init_packages();
        # symbol_tab fertig initialisieren:
        init_symbol_tab_2();
        # SUBRs/FSUBRs in ihre Symbole eintragen:
        init_symbol_functions();
        # Konstanten/Variablen: Wert in die Symbole eintragen:
        init_symbol_values();
        # sonstige Objekte kreieren:
        init_object_tab();
      }
  # Laden vom MEM-File:
    local void loadmem (char* filename); # siehe unten

#ifdef ATARI
  # Am Anfang nur den wirklich gebrauchten Speicher behalten.
  #ifdef GNU
    # GNU-C auf dem Atari: siehe libsrc/lib/crt0.c
    long _stksize = 0;
    #define basepage  _base
  #endif
  #ifdef ATARI_TURBO
    # TURBO-C auf dem Atari: siehe lib/tcstart.s
    # _StkSize sollte = 4KB sein; das muß beim Compilieren eingestellt werden!
    #define basepage  _BasPag
  #endif
  extern BASEPAGE* basepage; # Zeiger auf die Base-Page
#endif

#ifdef AMIGAOS

  global Handle Input_handle;     # low-level stdin Eingabekanal
  global Handle Output_handle;    # low-level stdout Ausgabekanal

  global BPTR orig_dir_lock = BPTR_NONE; # das Current Directory beim Programmstart
  # wird verwendet von PATHNAME

  # Initialisierung, ganz zuerst in main() durchzuführen:
    local void init_amiga (void);
    local void init_amiga()
      { Input_handle = Input();
        Output_handle = Output();
        # Abfrage, ob Workbench-Aufruf ohne besonderen Startup:
        if ((Input_handle==Handle_NULL) || (Output_handle==Handle_NULL))
          { exit(RETURN_FAIL); }
        # Benutzter Speicher muß in [0..2^oint_addr_len-1] liegen:
        if (!(pointable_usable_test((aint)&init_amiga) # Code-Segment überprüfen
              && pointable_usable_test((aint)&symbol_tab) # Daten-Segment überprüfen
           ) )
          { asciz_out(DEUTSCH ? "Diese CLISP-Version muß in Speicher mit niedrigen Adressen ablaufen." CRLFstring :
                      ENGLISH ? "This version of CLISP runs only in low address memory." CRLFstring :
                      FRANCAIS ? "Cette version de CLISP ne marche qu'en mémoire à adresse basse." CRLFstring :
                      ""
                     );
            exit(RETURN_FAIL);
          }
      }

  # Rückgabe aller Ressourcen und Programmende:
  local nonreturning void exit_amiga (sintL code);
  local nonreturning void exit_amiga(code)
    var reg3 sintL code;
    { begin_system_call();
      # Zurück ins Verzeichnis, in das wir beim Programmstart waren:
      if (!(orig_dir_lock == BPTR_NONE)) # haben wir das Verzeichnis je gewechselt?
        { var reg1 BPTR lock = CurrentDir(orig_dir_lock); # zurück ins alte
          UnLock(lock); # dieses nun freigeben
        }
      # Speicher freigeben:
      { var reg1 MemBlockHeader* memblocks = allocmemblocks;
        until (memblocks==NULL)
          { var reg2 MemBlockHeader* next = memblocks->next;
            FreeMem(memblocks,memblocks->size);
            memblocks = next;
      }   }
      # Programmende:
      exit(code);
    }

#endif

# Hauptprogramm trägt den Namen 'main'.
  #ifndef argc_t
    #define argc_t int  # Typ von argc ist meist 'int'.
  #endif
  global int main (argc_t argc, char* argv[]);
  local boolean argv_quiet = FALSE; # ob beim Start Quiet-Option angegeben
  global int main(argc,argv)
    var reg1 argc_t argc;
    var reg1 char* * argv;
    { # Initialisierung der Speicherverwaltung.
      # Gesamtvorgehen:
      # Command-Line-Argumente verarbeiten.
      # Speicheraufteilung bestimmen.
      # Commandstring anschauen und entweder LISP-Daten vom .MEM-File
      #   laden oder zu Fuß erzeugen und statische LISP-Daten initialisieren.
      # Interrupt-Handler aufbauen.
      # Begrüßung ausgeben.
      # In den Driver springen.
      #
      #ifdef AMIGAOS
      init_amiga();
      #endif
      #ifdef EMUNIX
      # Wildcards und Response-Files in der Kommandozeile expandieren:
      _response(&argc,&argv);
      _wildcard(&argc,&argv);
      #endif
      #if defined(MSDOS) && 0 # normalerweise unnötig
      # Auf stdin und stdout im Text-Modus zugreifen:
      setmode(stdin_handle,O_TEXT);
      setmode(stdout_handle,O_TEXT);
      #endif
     {var uintL argv_memneed = 0;
      #ifndef NO_SP_MALLOC
      var uintL argv_stackneed = 0;
      #endif
      #ifdef MULTIMAP_MEMORY_VIA_FILE
      var local char* argv_tmpdir = NULL;
      #endif
      var local char* argv_memfile = NULL;
      var local uintL argv_init_filecount = 0;
      var local char** argv_init_files;
      var local boolean argv_compile = FALSE;
      var local boolean argv_compile_listing = FALSE;
      var local uintL argv_compile_filecount = 0;
      var local char** argv_compile_files;
      var local char* argv_expr = NULL;
      {var DYNAMIC_ARRAY(argv_init_files_array,char*,(uintL)argc); # maximal argc Init-Files
       argv_init_files = argv_init_files_array;
      {var DYNAMIC_ARRAY(argv_compile_files_array,char*,(uintL)argc); # maximal argc File-Argumente
       argv_compile_files = argv_compile_files_array;
      if (!(setjmp(&!original_context) == 0)) goto end_of_main;
      # Argumente argv[0..argc-1] abarbeiten:
      #   -h              Help
      #   -m size         Memory size (size = xxxxxxxB oder xxxxKB oder xMB)
      #   -s size         Stack size (size = xxxxxxxB oder xxxxKB oder xMB)
      #   -t directory    temporäres Directory
      #   -M file         MEM-File laden
      #   -q              quiet: keine Copyright-Meldung
      #   -i file ...     LISP-File zur Initialisierung laden
      #   -c file ...     LISP-Files compilieren, dann LISP verlassen
      #   -l              Beim Compilieren: Listings anlegen
      #   -x expr         LISP-Expressions ausführen, dann LISP verlassen
      program_name = argv[0]; # argv[0] ist der Programmname
      if (FALSE)
        { usage:
          asciz_out("Usage:  ");
          asciz_out(program_name);
          asciz_out(" [-h] [-m memsize]");
          #ifndef NO_SP_MALLOC
          asciz_out(" [-s stacksize]");
          #endif
          #ifdef MULTIMAP_MEMORY_VIA_FILE
          asciz_out(" [-t tmpdir]");
          #endif
          asciz_out(" [-M memfile] [-q] [-i initfile ...]"
                    " [-c [-l] lispfile ...] [-x expression]" CRLFstring);
          quit_sofort(1); # anormales Programmende
        }
     {var reg2 char** argptr = &argv[1];
      var reg3 char** argptr_limit = &argv[argc];
      var reg5 enum { illegal, for_init, for_compile } argv_for = illegal;
      # Durchlaufen und Optionen abarbeiten, alles Abgearbeitete durch NULL
      # ersetzen:
      while (argptr < argptr_limit)
        { var reg1 char* arg = *argptr++; # nächstes Argument
          if (arg[0] == '-')
            { switch (arg[1])
                { case 'h': # Help
                    goto usage;
                  # Liefert nach einem einbuchstabigen Kürzel den Rest der
                  # Option in arg. Evtl. Space wird übergangen.
                  #define OPTION_ARG  \
                    if (arg[2] == '\0') \
                      { if (argptr < argptr_limit) arg = *argptr++; else goto usage; } \
                      else { arg = &arg[2]; }
                  # Parst den Rest einer Option, die eine Byte-Größe angibt.
                  # Überprüft auch, ob gewisse Grenzen eingehalten werden.
                  #define SIZE_ARG(docstring,sizevar,limit_low,limit_high)  \
                    # arg sollte aus einigen Dezimalstellen, dann  \
                    # evtl. K oder M, dann evtl. B bestehen.       \
                    {var reg4 uintL val = 0;                       \
                     while ((*arg >= '0') && (*arg <= '9'))        \
                       { val = 10*val + (uintL)(*arg++ - '0'); }   \
                     switch (*arg)                                 \
                       { case 'k': case 'K': # Angabe in Kilobytes \
                           val = val * 1024; arg++; break;         \
                         case 'm': case 'M': # Angabe in Megabytes \
                           val = val * 1024*1024; arg++; break;    \
                       }                                           \
                     switch (*arg)                                 \
                       { case 'b': case 'B': arg++; break; }       \
                     if (!(*arg == '\0')) # Argument zu Ende?      \
                       { asciz_out("Syntax for " docstring ": nnnnnnn or nnnnKB or nMB" CRLFstring); \
                         goto usage;                               \
                       }                                           \
                     if (!((val >= limit_low) && (val <= limit_high))) \
                       { asciz_out(docstring " out of range" CRLFstring); \
                         goto usage;                               \
                       }                                           \
                     if (!(sizevar == 0)) goto usage;              \
                     sizevar = val;                                \
                    }
                  case 'm': # Memory size
                    OPTION_ARG
                    SIZE_ARG("memory size",argv_memneed,100000,
                             (oint_addr_len+addr_shift < intLsize-1 # memory size begrenzt durch
                              ? bitm(oint_addr_len+addr_shift)      # Adreßraum in oint_addr_len+addr_shift Bits
                              : (uintL)(bit(intLsize-1)-1)          # (bzw. große Dummy-Grenze)
                            ))
                    break;
                  #ifndef NO_SP_MALLOC
                  case 's': # Stack size
                    OPTION_ARG
                    SIZE_ARG("stack size",argv_stackneed,40000,8*1024*1024)
                    break;
                  #endif
                  #ifdef MULTIMAP_MEMORY_VIA_FILE
                  case 't': # temporäres Directory
                    OPTION_ARG
                    if (!(argv_tmpdir == NULL)) goto usage;
                    argv_tmpdir = arg;
                    break;
                  #endif
                  case 'M': # MEM-File
                    OPTION_ARG
                    # Bei mehreren -M Argumenten zählt nur das letzte.
                    argv_memfile = arg;
                    break;
                  case 'q': # keine Copyright-Meldung
                    argv_quiet = TRUE;
                    if (!(arg[2] == '\0')) goto usage;
                    break;
                  case 'i': # Initialisierungs-Files
                    argv_for = for_init;
                    if (!(arg[2] == '\0')) goto usage;
                    break;
                  case 'c': # Zu compilierende Files
                    argv_compile = TRUE;
                    argv_for = for_compile;
                    if (arg[2] == 'l')
                      { argv_compile_listing = TRUE;
                        if (!(arg[3] == '\0')) goto usage;
                      }
                      else
                      { if (!(arg[2] == '\0')) goto usage; }
                    break;
                  case 'l': # Compilate und Listings
                    argv_compile_listing = TRUE;
                    if (!(arg[2] == '\0')) goto usage;
                    break;
                  case 'x': # LISP-Expression ausführen
                    OPTION_ARG
                    if (!(argv_expr == NULL)) goto usage;
                    argv_expr = arg;
                    break;
                  default: # Unbekannte Option
                    goto usage;
            }   }
            else
            # keine Option,
            # wird als zu ladendes / zu compilerendes File interpretiert
            { switch (argv_for)
                { case for_init:
                    argv_init_files[argv_init_filecount++] = arg; break;
                  case for_compile:
                    argv_compile_files[argv_compile_filecount++] = arg; break;
                  case illegal:
                  default:
                    goto usage;
            }   }
        }
      # Optionen semantisch überprüfen und Defaults eintragen:
      if (argv_memneed == 0)
        #if defined(ATARI)
        { argv_memneed = GEMDOS_FreeMem(); } # freien Platz erfragen
        #else
        { argv_memneed = 512*1024*sizeof(object); } # 2MB Default
        #endif
      #ifdef MULTIMAP_MEMORY_VIA_FILE
      if (argv_tmpdir == NULL)
        { argv_tmpdir = getenv("TMPDIR"); # Environment-Variable probieren
          if (argv_tmpdir == NULL)
            { argv_tmpdir = "/tmp"; }
        }
      #endif
      if (!argv_compile)
        # Manche Optionen sind nur zusammen mit '-c' sinnvoll:
        { if (argv_compile_listing) goto usage; }
        else
        # Andere Optionen sind nur ohne '-c' sinnvoll:
        { if (!(argv_expr == NULL)) goto usage; }
     }
     # Speicher holen:
     #ifdef SPVW_PURE
     { var reg1 uintL heapnr;
       for (heapnr=0; heapnr<heapcount; heapnr++)
         { switch (heapnr)
             { case_array:
               case_record:
               case_bignum:
               #ifndef WIDE
               case_ffloat:
               #endif
               case_dfloat:
               case_lfloat:
               case_symbol:
                 mem.heaptype[heapnr] = 0; break;
               case_cons:
               case_ratio:
               case_complex:
                 mem.heaptype[heapnr] = 1; break;
               default:
                 mem.heaptype[heapnr] = -1; break;
         }   }
     }
     init_speicher_laengen();
     #endif
     {# Aufteilung des Gesamtspeichers in Teile:
      #define teile             16  # 16/16
        #ifdef NO_SP_MALLOC # wird SP vom Betriebssystem bereitgestellt?
        #define teile_SP         0
        #else
        #define teile_SP         2  # 2/16 (1/16 reicht oft nicht)
        #endif
        #define teile_STACK      2  # 2/16
        #ifdef HAVE_NUM_STACK
        #define teile_NUM_STACK  1  # 1/16
        #else
        #define teile_NUM_STACK  0
        #endif
        #define teile_stacks     (teile_SP + teile_STACK + teile_NUM_STACK)
        #ifdef SPVW_MIXED_BLOCKS
        #define teile_objects    (teile - teile_stacks)  # Rest
        #else
        #define teile_objects    0
        #endif
      var reg4 uintL pagesize = # Länge einer Speicherseite
        #if defined(MULTIMAP_MEMORY_VIA_FILE)
        getpagesize()
        #elif defined(MULTIMAP_MEMORY_VIA_SHM)
        SHMLBA
        #else # wenn sie System-Speicherseiten-Länge keine Rolle spielt
        teile*Varobject_alignment
        #endif
        ;
      var reg5 uintL memneed = argv_memneed; # benötigter Speicher
      var reg6 aint memblock; # untere Adresse des bereitgestellten Speicherblocks
      #ifndef SPVW_MIXED_BLOCKS
      memneed = teile_stacks*floor(memneed,teile); # noch keinen Speicher für objects berechnen
      #undef teile
      #define teile  teile_stacks
      #endif
      #ifndef NO_SP_MALLOC
      if (!(argv_stackneed==0))
        { memneed = memneed*(teile-teile_SP)/teile;
          # Die mit Option -s angegebene SP-Größe ist noch nicht in memneed inbegriffen.
          memneed = memneed + argv_stackneed;
        }
      #endif
      #if defined(MULTIMAP_MEMORY_VIA_SHM) && defined(UNIX_SUNOS4)
      # SunOS 4 weigert sich, ein shmat() in einen vorher mallozierten Bereich
      # hinein zu machen, selbst wenn dawischen ein munmap() liegt:
      # errno = EINVAL. Auch das Umgekehrte, erst shmat() zu machen und dann
      # mit sbrk() oder brk() den belegten Bereich dem Datensegment einzu-
      # verleiben, scheitert mit errno = ENOMEM.
      # Der einzige Ausweg ist, sich den benötigten Speicher von weit weg,
      # möglichst außer Reichweite von malloc(), zu holen.
      { var reg1 uintL memhave = round_down(bit(oint_addr_len) - (aint)sbrk(0),SHMLBA);
        if (memhave < memneed) { memneed = memhave; }
        memblock = round_down(bit(oint_addr_len) - memneed,SHMLBA);
      }
      #else
      loop
        { memblock = (aint)mymalloc(memneed); # Speicher allozieren versuchen
          if (!((void*)memblock == NULL)) break; # gelungen -> OK
          memneed = floor(memneed,8)*7; # sonst mit 7/8 davon nochmals versuchen
          if (memneed < MINIMUM_SPACE+RESERVE) # aber mit weniger als MINIMUM_SPACE
            # geben wir uns nicht zufrieden:
            { asciz_out(DEUTSCH ? "Nur " :
                        ENGLISH ? "Only " :
                        FRANCAIS ? "Seuls " :
                        ""
                       );
              dez_out(memneed);
              asciz_out(DEUTSCH ? " Bytes verfügbar." :
                        ENGLISH ? " bytes available." :
                        FRANCAIS ? " octets libres." :
                        ""
                       );
              asciz_out(CRLFstring);
              goto no_mem;
        }   }
      #endif
      #ifdef ATARI
      MEMBLOCK = memblock;
      #endif
      #ifdef MULTIMAP_MEMORY
      # Wir brauchen zwar nur diesen Adreßraum und nicht seinen Inhalt, dürfen
      # ihn aber nicht freigeben, da er in unserer Kontrolle bleiben soll.
      #endif
      # Aufrunden zur nächsten Speicherseitengrenze:
      {var reg1 uintL unaligned = (uintL)(-memblock) % pagesize;
       memblock += unaligned; memneed -= unaligned;
      }
      # Abrunden zur letzen Speicherseitengrenze:
      {var reg1 uintL unaligned = memneed % pagesize;
       memneed -= unaligned;
      }
      # Der Speicherbereich [memblock,memblock+memneed-1] ist nun frei,
      # und seine Grenzen liegen auf Speicherseitengrenzen.
      #ifdef MULTIMAP_MEMORY
        map_pagesize = pagesize;
        #ifdef MULTIMAP_MEMORY_VIA_FILE
        if ( initmap(argv_tmpdir) <0) goto no_mem;
        #else
        if ( initmap() <0) goto no_mem;
        #endif
        multimap(case_machine: case_array: case_record: case_system:
                 case_bignum: case_ratio: case_ffloat: case_dfloat: case_lfloat: case_complex:
                 case_symbolflagged: case_cons:
                 , memblock, memneed);
        # Dazu noch symbol_tab an die Adresse 0 legen:
        {var reg3 uintL memneed = round_up(sizeof(symbol_tab),pagesize); # Länge aufrunden
         multimap(case_symbolflagged: , 0, memneed);
        }
        # Dazu noch fsubr_tab, subr_tab an die Adresse 0 legen:
        #define map_tab(tab)  \
          if ( zeromap(&tab,round_up(sizeof(tab),pagesize)) <0) goto no_mem;
        map_tab(fsubr_tab);
        map_tab(subr_tab);
        #ifdef MULTIMAP_MEMORY_VIA_FILE
        if ( close(zero_fd) <0)
          { asciz_out(DEUTSCH ? "Kann /dev/zero nicht schließen." :
                      ENGLISH ? "Cannot close /dev/zero ." :
                      FRANCAIS ? "Ne peux pas fermer /dev/zero ." :
                      ""
                     );
            goto no_mem;
          }
        #endif
        #ifdef MULTIMAP_MEMORY_VIA_SHM
        #ifdef SHM_NEEDS_INIT
        # lisp.run ist ein suid-root-Executable, damit ipc_init() geht.
        # Ab jetzt brauchen wir jedoch keine root-Privilegien mehr.
        setuid(getuid()); # effective user id := real/saved user id
        setgid(getgid()); # effective group id := real/saved group id
        #endif
        #endif
      #endif
      #ifdef SINGLEMAP_MEMORY # <==> SPVW_PURE_BLOCKS
        map_pagesize = getpagesize();
        if ( initmap() <0) goto no_mem;
        # Alle Heaps vor-initialisieren:
        { var reg2 uintL heapnr;
          for (heapnr=0; heapnr<heapcount; heapnr++)
            { var reg1 Heap* heapptr = &mem.heaps[heapnr];
              heapptr->heap_limit = (aint)type_pointer_object(heapnr,0);
        }   }
        # Dazu noch symbol_tab, fsubr_tab, subr_tab an die Adresse 0 legen:
        # (Hierzu muß case_symbolflagged mit case_symbol äquivalent sein!)
        #define map_tab(tab)  \
          { var reg1 uintL map_len = round_up(sizeof(tab),map_pagesize); \
            if ( zeromap(&tab,map_len) <0) goto no_mem;                  \
            mem.heaps[typecode(&tab)].limit += map_len;                  \
          }
        map_tab(symbol_tab);
        map_tab(fsubr_tab);
        map_tab(subr_tab);
        # Alle Heaps als leer initialisieren:
        { var reg2 uintL heapnr;
          for (heapnr=0; heapnr<heapcount; heapnr++)
            { var reg1 Heap* heapptr = &mem.heaps[heapnr];
              heapptr->heap_start = heapptr->heap_end = heapptr->heap_limit;
        }   }
        # STACK initialisieren:
        { var reg1 uintL map_len = round_up(memneed * teile_STACK/teile, map_pagesize);
          # Der Stack belegt das Intervall von 0 bis map_len bei Typcode = system_type:
          var reg2 aint low = (aint)type_pointer_object(system_type,0);
          var reg3 aint high = low + map_len;
          if ( zeromap((void*)low,map_len) <0) goto no_mem;
          #ifdef STACK_DOWN
            STACK_bound = (object*)(low + 0x100); # 64 Pointer Sicherheitsmarge
            setSTACK(STACK = (object*)high); # STACK initialisieren
          #endif
          #ifdef STACK_UP
            setSTACK(STACK = (object*)low); # STACK initialisieren
            STACK_bound = (object*)(high - 0x100); # 64 Pointer Sicherheitsmarge
          #endif
        }
        #undef teile_STACK
        #define teile_STACK 0  # brauche keinen Platz mehr für den STACK
        #if (teile==0)
          #undef teile
          #define teile 1  # Division durch 0 vermeiden
        #endif
      #endif
      # Speicherblock aufteilen:
      { var reg3 uintL free_reserved; # Anzahl reservierter Bytes
        #ifndef NO_SP_MALLOC
        var reg10 aint initial_SP; # Initialwert für SP-Stackpointer
        var reg9 uintL for_SP = 0; # Anzahl Bytes für SP-Stack
        #define min_for_SP  40000 # minimale SP-Stack-Größe
        #endif
        var reg7 uintL for_STACK; # Anzahl Bytes für Lisp-STACK
        var reg9 uintL for_NUM_STACK; # Anzahl Bytes für Zahlen-STACK
        var reg8 uintL for_objects; # Anzahl Bytes für Lisp-Objekte
        free_reserved = memneed;
        #ifndef NO_SP_MALLOC
        if (!(argv_stackneed==0))
          if (2*argv_stackneed <= free_reserved) # nicht zu viel für den SP-Stack reservieren
            { for_SP = round_down(argv_stackneed,Varobject_alignment);
              free_reserved -= argv_stackneed;
            }
        #endif
        # Durch teile*Varobject_alignment teilbar machen, damit jedes Sechzehntel aligned ist:
        free_reserved = round_down(free_reserved,teile*Varobject_alignment);
        free_reserved = free_reserved - RESERVE;
       {var reg2 uintL teil = free_reserved/teile; # ein Teilblock, ein Sechzehntel des Platzes
        var reg1 aint ptr = memblock;
        mem.MEMBOT = ptr;
        #ifndef NO_SP_MALLOC
        # SP allozieren:
        if (for_SP==0)
          { for_SP = teile_SP*teil; } # 2/16 für Programmstack
          else
          # Platz für SP ist schon abgezwackt.
          { # teile := teile-teile_SP; # geht nicht mehr, stattdessen:
            teil = round_down(free_reserved/(teile-teile_SP),Varobject_alignment);
          }
        if (for_SP < min_for_SP) { for_SP = round_up(min_for_SP,Varobject_alignment); } # aber nicht zu wenig
        #ifdef SP_DOWN
          SP_bound = ptr + 0x800; # 512 Pointer Sicherheitsmarge
          ptr += for_SP;
          initial_SP = ptr;
          #if defined(I80Z86) && 0 # unnötig?
            # Stack einmal kurz durchgehen, um Page Faults zu vermeiden:
            { var reg2 uintB sum = 0;
              var reg1 aint p = initial_SP;
              do { p -= 0x400; sum += *(volatile uintB *)p; }
                 while (p >= SP_bound);
            }
          #endif
        #endif
        #ifdef SP_UP
          initial_SP = ptr;
          ptr += for_SP;
          SP_bound = ptr - 0x800; # 512 Pointer Sicherheitsmarge
        #endif
        #endif
        # STACK allozieren:
        #ifdef SINGLEMAP_MEMORY
        for_STACK = 0; # STACK ist schon woanders alloziert.
        #else
        #ifdef STACK_DOWN
          STACK_bound = (object*)(ptr + 0x100); # 64 Pointer Sicherheitsmarge
          ptr += for_STACK = teile_STACK*teil; # 2/16 für Lisp-STACK
          setSTACK(STACK = (object*)ptr); # STACK initialisieren
        #endif
        #ifdef STACK_UP
          setSTACK(STACK = (object*)ptr); # STACK initialisieren
          ptr += for_STACK = teile_STACK*teil; # 2/16 für Lisp-STACK
          STACK_bound = (object*)(ptr - 0x100); # 64 Pointer Sicherheitsmarge
        #endif
        #endif
        #ifdef HAVE_NUM_STACK
        # NUM_STACK allozieren:
        #ifdef NUM_STACK_DOWN
          NUM_STACK_bound = ptr;
          ptr += for_NUM_STACK = teile_NUM_STACK*teil; # 1/16 für Zahlen-STACK
          NUM_STACK = NUM_STACK_normal = ptr; # NUM_STACK initialisieren
        #endif
        #ifdef NUM_STACK_UP
          NUM_STACK = NUM_STACK_normal = ptr; # NUM_STACK initialisieren
          ptr += for_NUM_STACK = teile_NUM_STACK*teil; # 1/16 für Zahlen-STACK
          NUM_STACK_bound = ptr;
        #endif
        #else
        for_NUM_STACK = 0; # kein Zahlen-Stack vorhanden
        #endif
        #ifdef SPVW_MIXED_BLOCKS
        # Nun fangen die Lisp-Objekte an:
        mem.objects.start = ptr;
        mem.objects.end = ptr; # Noch gibt es keine Objekte variabler Länge
        # Rest (14/16 oder etwas weniger) für Lisp-Objekte:
        for_objects = memblock+free_reserved - ptr; # etwa = teile_objects*teil
        ptr += for_objects;
        mem.conses.start = ptr; # Noch gibt es keine Conses
        mem.conses.end = ptr;
        # ptr = memblock+free_reserved, da 2/16 + 14/16 = 1
        # Reservespeicher allozieren:
        ptr += RESERVE;
        # oberes Speicherende erreicht.
        mem.MEMTOP = ptr;
        # Darüber (weit weg) der Maschinenstack.
        #endif
        #ifdef SPVW_PURE_BLOCKS
        mem.total_room = 0;
        #endif
        #ifdef SPVW_PAGES
        #define set_inuse_EMPTY(heap)  heap.inuse = EMPTY;
        for_each_heap(set_inuse_EMPTY);
        for_each_cons_heap(set_lastused_dummy);
        dummy_lastused->page_room = 0;
        mem.free_pages = NULL;
        mem.last_gcend_space = 0;
        #endif
        # Stacks initialisieren:
        #ifdef NO_SP_MALLOC
          #ifdef AMIGAOS
          { var struct Process * myprocess = (struct Process *)FindTask(NULL);
            var aint original_SP = process->pr_ReturnAddr; # SP beim Programmstart
            # Die Shell legt die Stackgröße vor dem Start auf den SP.
            ptr = original_SP - *(ULONG*)original_SP;
            SP_bound = ptr + 0x1000; # 1024 Pointer Sicherheitsmarge
          }
          #endif
        #else
          #ifdef GNU
            # eine kleine Dummy-Aktion, die ein hinausgezögertes Aufräumen des SP
            # zu einem späteren Zeitpunkt verhindert:
            if (mem.MEMBOT) { asciz_out(""); }
          #endif
          setSP(initial_SP); # SP setzen! Dabei gehen alle lokalen Variablen verloren!
        #endif
        pushSTACK(nullobj); pushSTACK(nullobj); # Zwei Nullpointer als STACKende-Kennung
     }}}
      #ifdef ATARI
        # Line-A-Routinen initialisieren:
        LineA_Init();
        # Maus abschalten:
        LineA_MouseHide();
        # Bildschirmausgabe initialisieren:
        asciz_out(
          ESCstring "E"  # CLEAR HOME, Bildschirm löschen
          ESCstring "v"  # Ab jetzt bei Zeilenüberlauf immer in neue Zeile
          ESCstring "q"  # Reverse off
          ESCstring "f"  # Cursor ausschalten
          );
      #endif
      init_fsubr_tab_1(); # fsubr_tab initialisieren
      init_subr_tab_1(); # subr_tab initialisieren
      if (argv_memfile==NULL)
       #ifdef ATARI
        # Auf dem Atari muß man meist ohne Kommandozeilen-Option auskommen.
        { argv_memfile = "lispinit.mem"; } # Daher ein sinnvoller Default.
       #else
        # Zu-Fuß-Initialisierung:
        { initmem(); }
        else
       #endif
        # Speicherfile laden:
        { loadmem(argv_memfile); }
      # aktuelle Evaluator-Environments auf den Toplevel-Wert setzen:
      aktenv.var_env   = NIL;
      aktenv.fun_env   = NIL;
      aktenv.block_env = NIL;
      aktenv.go_env    = NIL;
      aktenv.decl_env  = O(top_decl_env);
      # Alles fertig initialisiert.
      set_break_sem_1(); clr_break_sem_2(); clr_break_sem_3(); clr_break_sem_4();
      everything_ready = TRUE;
      # Interrupt-Handler einrichten:
      #ifdef ATARI
        # VBL-Routine modifizieren:
        set_break_sem_1(); # neue Routine erstmal sperren
        new_VBL_fixup_break = &break_sems.gesamt;
        new_VBL_fixup_linea = &linea;
        new_VBL_fixup_tast_fehler = &tastatur_interrupt;
        old_VBL = BIOS_GetException(28);
        BIOS_SetException(28,new_VBL);
      #endif
      #if defined(HAVE_SIGNALS)
        #if defined(SIGWINCH) && !defined(NO_ASYNC_INTERRUPTS)
        # Eine veränderte Größe des Terminal-Fensters soll sich auch sofort
        # in SYS::*PRIN-LINELENGTH* bemerkbar machen:
        signal(SIGWINCH,&sigwinch_handler);
        #endif
        # Die Größe des Terminal-Fensters auch jetzt beim Programmstart erfragen:
        update_linelength();
      #endif
      #if defined(MSDOS)
        # Die Breite des Bildschirms im aktuellen Bildschirm-Modus
        # jetzt beim Programmstart erfragen:
        if (isatty(stdout_handle)) # Standard-Output ein Terminal?
          { extern uintW v_cols(); # siehe STREAM.D
            #ifdef EMUNIX_PORTABEL
            var int scrsize[2];
            var reg1 uintL columns;
            #ifdef EMUNIX_OLD_8d
            if (_osmode == DOS_MODE)
              /* unter DOS */ { columns = v_cols(); }
              else
              /* unter OS/2 */
            #endif
            columns = (_scrsize(&!scrsize), scrsize[0]);
            #else
            var reg1 uintL columns = v_cols();
            #endif
            if (columns > 0)
              { # Wert von SYS::*PRIN-LINELENGTH* verändern:
                Symbol_value(S(prin_linelength)) =
                  fixnum(columns-1);
          }   }
      #endif
      #if defined(AMIGAOS) && 0
        # frage beim console.driver nach??
        if (IsInteractive(Input_handle) && IsInteractive(Output_handle)) # ??
          { var reg1 uintL len;
            var uintB question[4] = { CSI, '0', ' ', 'q' };
            var uintB response[30+1];
            Write(Output_handle,question,4);
            len = Read(Input_handle,response,30);
            response[len] = `\0`; sscanf(&response[5],"%d;%d", &lines, &columns); # ??
          }
      #endif
      #if defined(HAVE_SIGNALS)
      #if defined(UNIX) || defined(EMUNIX) || defined(VMS)
        # Ctrl-C-Handler einsetzen:
        signal(SIGINT,&interrupt_handler);
        #ifdef PENDING_INTERRUPTS
        signal(SIGALRM,&alarm_handler);
        #endif
      #endif
      #if defined(SIGCLD)
        # Wir wollen es ignorieren, wenn ein von uns erzeugter Prozeß endet:
        signal(SIGCLD,SIG_IGN);
        # (Das ist im wesentlichen äquivalent zur Installation eines Signal-
        # Handlers, der ein  while (waitpid(-1,NULL,WNOHANG) > 0);  ausführt.)
      #endif
      #endif
      # Zeitvariablen initialisieren:
      # Es ist noch keine GC dagewesen -> hat auch noch keine Zeit verbraucht.
      # gc_count=0;
      # gc_time=0;
      # gc_space.hi=0, gc_space.lo=0;
      #ifdef TIME_RELATIVE
      realstart_time = get_time(); # Zeitzähler jetzt, beim Systemstart
      #endif
      #ifndef HAVE_RUN_TIME
      # run_time = 0; # Noch keine Run-Time verbraucht,
      # run_flag = FALSE; # denn System läuft noch nicht.
      run_time_restart(); # Run-Time-Stoppuhr loslaufen lassen
      #endif
      #ifdef TIME_UNIX
      realstart_time = *(get_real_time()); # Zeitzähler jetzt, beim Systemstart
      #endif
      #ifdef TIME_RELATIVE
      # Start-Zeit holen und merken:
      { var decoded_time timepoint;
        #ifdef ATARI
        { var reg1 uintW date;
          var reg2 uintW time;
          do { date = GEMDOS_GetDate(); # externes Datum holen
               time = GEMDOS_GetTime(); # externe Uhrzeit holen
             } # und wiederholen, falls sich das Datum zwischenzeitlich
               # geändert hat:
             until (date==GEMDOS_GetDate());
          convert_timedate(time,date,&timepoint); # in Decoded-Time umwandeln
        }
        # Sekunden-Wert (gerades Fixnum >=0, <60) um 1 erhöhen,
        # verringert die Ungenauigkeit:
        timepoint.Sekunden = fixnum_inc(timepoint.Sekunden,1);
        #endif
        #ifdef AMIGAOS
        { var struct DateStamp datestamp; # aktuelle Uhrzeit
          DateStamp(&datestamp);
          convert_time(&datestamp,&timepoint); # in Decoded-Time umwandeln
        }
        #endif
        #if defined(DJUNIX) && 0 # das geht eine Stunde nach!!
        { var struct timeval real_time;
          gettimeofday(&real_time,NULL); # aktuelle Uhrzeit
          convert_time(&real_time.tv_sec,&timepoint); # in Decoded-Time umwandeln
        }
        #endif
        #if defined(DJUNIX) || defined(EMUNIX_OLD_8d)
        { var internal_decoded_time idt;
          get_decoded_time(&idt);
          timepoint.Sekunden = fixnum(idt.sec);
          timepoint.Minuten  = fixnum(idt.min);
          timepoint.Stunden  = fixnum(idt.hour);
          timepoint.Tag      = fixnum(idt.day);
          timepoint.Monat    = fixnum(idt.month);
          timepoint.Jahr     = fixnum(idt.year);
        }
        #endif
        #if defined(EMUNIX_NEW_8e)
        { var struct timeb real_time;
          __ftime(&real_time); # aktuelle Uhrzeit
          convert_time(&real_time.time,&timepoint); # in Decoded-Time umwandeln
        }
        #endif
        #ifdef VMS
        { var struct timeb real_time;
          ftime(&real_time); # aktuelle Uhrzeit
          convert_time(&real_time.time,&timepoint); # in Decoded-Time umwandeln
        }
        #endif
        set_start_time(&timepoint); # Start-Zeit merken
      }
      #endif
      # Stream-Variablen initialisieren:
      init_streamvars();
      #ifdef ATARI
      # Keyboard-Input-Stream funktionsfähig machen:
      new_keyboard();
      #endif
      # Break ermöglichen:
      end_system_call();
      clr_break_sem_1();
      # Pathnames initialisieren:
      init_pathnames();
      #ifdef REXX
      # Rexx-Interface initialisieren:
      init_rexx();
      # Auf eine Fehlermeldung im Falle des Scheiterns verzichten wir.
      # Deswegen wollen wir das CLISP doch nicht unbrauchbar machen!
      #endif
      # Begrüßung ausgeben:
      if (!argv_quiet)
        { local char* banner[] = { # einige Zeilen à 66 Zeichen
          #  |Spalte 0           |Spalte 20                                    |Spalte 66
            #if 0
            "        *  *                                                      " NLstring,
            "     *        *                                                   " NLstring,
            "    *          *                                                  " NLstring,
            "    *          *                                                  " NLstring,
            "     *        *                                                   " NLstring,
            "        *  *                                                      " NLstring,
            "                                                                  " NLstring,
            #else
            "  i i i i i i i       ooooo    o        ooooooo   ooooo   ooooo   " NLstring,
            "  I I I I I I I      8     8   8           8     8     o  8    8  " NLstring,
            "  I I I I I I I      8         8           8     8        8    8  " NLstring,
            "  I I I I I I I      8         8           8      ooooo   8oooo   " NLstring,
           "  I  \\ `+' /  I      8         8           8           8  8       " NLstring,
           "   \\  `-+-'  /       8     o   8           8     o     8  8       " NLstring,
            "    `-__|__-'         ooooo    8oooooo  ooo8ooo   ooooo   8       " NLstring,
            "        |                                                         " NLstring,
            "  ------+------     Copyright (c) Bruno Haible, Michael Stoll 1992, 1993" NLstring,
            #ifdef AMIGA
            #if DEUTSCH
            "                    Amiga-Version: Jörg Höhle                     " NLstring,
            #elif ENGLISH
            "                    Amiga version: Jörg Höhle                     " NLstring,
            #elif FRANCAIS
            "                    version Amiga: Jörg Höhle                     " NLstring,
            #endif
            #endif
            #ifdef DJUNIX
            #if DEUTSCH
            "                    DOS-Portierung: Jürgen Weber, Bruno Haible    " NLstring,
            #elif ENGLISH
            "                    DOS port: Jürgen Weber, Bruno Haible          " NLstring,
            #elif FRANCAIS
            "                    adapté à DOS par Jürgen Weber et Bruno Haible " NLstring,
            #endif
            #endif
            "                                                                  " NLstring,
            #endif
            };
          var reg3 uintL offset = (posfixnum_to_L(Symbol_value(S(prin_linelength))) >= 72 ? 0 : 20);
          var reg1 char** ptr = &banner[0];
          var reg2 uintC count;
          pushSTACK(var_stream(S(standard_output))); # auf *STANDARD-OUTPUT*
          dotimesC(count,sizeof(banner)/sizeof(banner[0]),
            { write_sstring(&STACK_0,asciz_to_string(&(*ptr++)[offset])); }
            );
          skipSTACK(1);
        }
      if (argv_compile || !(argv_expr == NULL))
        # '-c' oder '-x' angegeben -> LISP läuft im Batch-Modus:
        { # (setq *debug-io*
          #   (make-two-way-stream (make-string-input-stream "") *query-io*)
          # )
          funcall(L(make_concatenated_stream),0); # (MAKE-CONCATENATED-STREAM)
          pushSTACK(value1); # leerer Input-Stream
         {var reg1 object stream = var_stream(S(query_io));
          Symbol_value(S(debug_io)) = make_twoway_stream(popSTACK(),stream);
        }}
      # für jedes initfile (LOAD initfile) ausführen:
      { var reg1 char** fileptr = &argv_init_files[0];
        var reg2 uintL count;
        dotimesL(count,argv_init_filecount,
          { var reg3 object filename = asciz_to_string(*fileptr++);
            pushSTACK(filename); funcall(S(load),1);
          });
      }
      if (argv_compile)
        # für jedes File (COMPILE-FILE file :LISTING listing) durchführen:
        { var reg1 char** fileptr = &argv_compile_files[0];
          var reg2 uintL count;
          dotimesL(count,argv_compile_filecount,
            { var reg3 object filename = asciz_to_string(*fileptr++);
              pushSTACK(filename);
              pushSTACK(S(Klisting));
              pushSTACK(argv_compile_listing ? T : NIL);
              funcall(S(compile_file),3);
            });
          quit();
        }
      if (!(argv_expr == NULL))
        # *STANDARD-INPUT* auf einen Stream setzen, der argv_expr produziert:
        { pushSTACK(asciz_to_string(argv_expr));
          funcall(L(make_string_input_stream),1);
          Symbol_value(S(standard_input)) = value1;
          # Dann den Driver aufrufen. Stringende -> EOF -> Programmende.
        }
      # Read-Eval-Print-Schleife aufrufen:
      driver();
      quit();
      /*NOTREACHED*/
      # Falls der Speicher nicht ausreichte:
      no_mem:
      asciz_out(program_name); asciz_out(": ");
      asciz_out(
        DEUTSCH ? "Nicht genug Speicher für LISP" CRLFstring :
        ENGLISH ? "Not enough memory for Lisp." CRLFstring :
        FRANCAIS ? "Il n'y a pas assez de mémoire pour LISP." CRLFstring :
        ""
        );
      #ifdef ATARI
      GEMDOS_ConIn(); # auf Tastendruck warten, bevor der Bildschirm gelöscht wird
      #endif
      quit_sofort(1);
      /*NOTREACHED*/
     # Beendigung des Programms durch quit_sofort():
      end_of_main:
      FREE_DYNAMIC_ARRAY(argv_compile_files); }
      FREE_DYNAMIC_ARRAY(argv_init_files); }
      #if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX)
        _exit(exitcode);
      #endif
      #ifdef ATARI
        GEMDOS_exit();
      #endif
      #ifdef AMIGAOS
        exit_amiga(exitcode ? RETURN_FAIL : RETURN_OK);
      #endif
      #ifdef VMS
        return (exitcode ? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE);
      #endif
      # Wenn das nichts geholfen haben sollte:
      return exitcode;
    }}

# LISP-Interpreter verlassen
  global nonreturning void quit (void);
  global nonreturning void quit()
    { # Erst den STACK bis STACK-Ende "unwinden":
      value1 = NIL; mv_count=0; # Bei UNWIND-PROTECT-Frames keine Werte retten
      unwind_protect_to_save.fun = (restart)&quit;
      loop
        { # Hört der STACK hier auf?
          if ((STACK_0 == nullobj) && (STACK_1 == nullobj)) break;
          if (mtypecode(STACK_0) & bit(frame_bit_t))
            # Bei STACK_0 beginnt ein Frame
            { unwind(); } # Frame auflösen
            else
            # STACK_0 enthält ein normales LISP-Objekt
            { skipSTACK(1); }
        }
      # Dann eine Abschiedsmeldung:
      { funcall(L(fresh_line),0); # (FRESH-LINE [*standard-output*])
        if (!argv_quiet)
          { # (WRITE-LINE "Bye." [*standard-output*]) :
            pushSTACK(O(bye_string)); funcall(L(write_line),1);
      }   }
      close_all_files(); # alle Files schließen
      #ifdef REXX
      close_rexx(); # Rexx-Kommunikation herunterfahren
      #endif
      #ifdef ATARI
      old_keyboard(); # Tastaturabfrage wieder in Urzustand bringen
      BIOS_SetException(28,old_VBL); # alten Exception-Vektor zurückschreiben
      LineA_MouseUnhide(); # Maus wieder in Urzustand bringen
      #endif
      quit_sofort(0); # Programm verlassen
    }

# ------------------------------------------------------------------------------
#                  Speichern und Laden von MEM-Files

#if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX)
  # Betriebssystem-Funktion read sichtbar machen:
    #undef read
#endif

# Format:
# ein Header:
  typedef struct { uintL _magic; # Erkennung
                     #define memdump_magic  0x70768BD2UL
                   uintC _fsubr_anz;
                   uintC _subr_anz;
                   uintC _pseudofun_anz;
                   uintC _symbol_anz;
                   uintC _object_anz;
                   aint _fsubr_tab_addr;
                   aint _subr_tab_addr;
                   aint _symbol_tab_addr;
                   #ifdef SPVW_MIXED_BLOCKS
                   aint _mem_objects_start;
                   aint _mem_objects_end;
                   aint _mem_conses_start;
                   aint _mem_conses_end;
                   #endif
                   #ifndef SPVW_MIXED_BLOCKS
                   uintC _heapcount;
                   #endif
                 }
          memdump_header;
#ifdef SPVW_MIXED_BLOCKS
  # dann fsubr_tab, subr_tab, pseudofun_tab, symbol_tab, object_tab, dann die
  # Objekte variabler Länge (zwischen mem.objects.start und mem.objects.end),
  # dann die Conses (zwischen mem.conses.start und mem.conses.end).
#else
  #ifdef SPVW_PURE_BLOCKS
    # dann zu jedem Heap (Block) die Start- und Endadresse,
  #endif
  #ifdef SPVW_PAGES
    # SPVW_PAGES: dann zu jedem Heap die Anzahl der Pages,
    # dann zu jedem Heap und zu jeder Page des Heaps die Start- und Endadresse,
  #endif
  typedef struct { aint _page_start; aint _page_end; } memdump_page;
  # dann fsubr_tab, subr_tab, pseudofun_tab, symbol_tab, object_tab,
  # dann der Inhalt der Pages in derselben Reihenfolge.
#endif

# UP, speichert Speicherabbild auf Diskette
# savemem(stream);
# > object stream: offener File-Output-Stream, wird geschlossen
# kann GC auslösen
  global void savemem (object stream);
  global void savemem(stream)
    var reg4 object stream;
    { # Wir brauchen den Stream nur wegen des für ihn bereitgestellten Handles.
      # Wir müssen ihn aber im Fehlerfalle schließen (der Aufrufer macht kein
      # WITH-OPEN-FILE, sondern nur OPEN). Daher bekommen wir den ganzen
      # Stream übergeben, um ihn schließen zu können.
      var reg3 Handle handle = TheHandle(TheStream(stream)->strm_file_handle);
      pushSTACK(stream); # Stream retten
      # Erst eine GC ausführen:
      gar_col();
      #ifdef SPVW_PAGES
      if (!mem.last_gc_compacted) { gar_col_compact(); }
      #endif
      #ifdef ATARI
        #define WRITE(buf,len)  \
          { begin_system_call();                                        \
           {var reg1 sintL ergebnis = GEMDOS_write(handle,len,buf);     \
            if (!(ergebnis==(len)))                                     \
              { stream_close(&STACK_0);                                 \
                if (ergebnis<0) { OS_error(ergebnis); } # Fehler aufgetreten? \
                fehler(                                                 \
                       DEUTSCH ? "Diskette/Platte voll." :              \
                       ENGLISH ? "disk full" :                          \
                       FRANCAIS ? "Disque plein." :                     \
                       ""                                               \
                      );                                                \
              }                                                         \
            end_system_call();                                          \
          }}
      #endif
      #ifdef AMIGAOS
        #define WRITE(buf,len)  \
          { begin_system_call();                                      \
           {var reg1 sintL ergebnis = Write(handle,(void*)buf,len);   \
            if (!(ergebnis==(len)))                                   \
              { stream_close(&STACK_0);                               \
                if (ergebnis<0) { OS_error(); } # Fehler aufgetreten? \
                fehler(                                               \
                       DEUTSCH ? "Datenträger vermutlich voll." :     \
                       ENGLISH ? "device possibly full" :             \
                       FRANCAIS ? "Disque peut-être plein." :         \
                       ""                                             \
                      );                                              \
              }                                                       \
            end_system_call();                                        \
          }}
      #endif
      #if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX)
        #define WRITE(buf,len)  \
          { begin_system_call();                                       \
           {var reg1 sintL ergebnis = write(handle,(RW_BUF_T)buf,len); \
            if (!(ergebnis==(len)))                                    \
              { stream_close(&STACK_0);                                \
                if (ergebnis<0) { OS_error(); } # Fehler aufgetreten?  \
                fehler(                                                \
                       DEUTSCH ? "Diskette/Platte voll." :             \
                       ENGLISH ? "disk full" :                         \
                       FRANCAIS ? "Disque plein." :                    \
                       ""                                              \
                      );                                               \
              }                                                        \
            end_system_call();                                         \
          }}
      #endif
      # Grundinformation rausschreiben:
     {var memdump_header header;
      header._magic = memdump_magic;
      header._fsubr_anz     = fsubr_anz;
      header._subr_anz      = subr_anz;
      header._pseudofun_anz = pseudofun_anz;
      header._symbol_anz    = symbol_anz;
      header._object_anz    = object_anz;
      header._fsubr_tab_addr  = (aint)(&fsubr_tab);
      header._subr_tab_addr   = (aint)(&subr_tab);
      header._symbol_tab_addr = (aint)(&symbol_tab);
      #ifdef SPVW_MIXED_BLOCKS
      header._mem_objects_start = mem.objects.start;
      header._mem_objects_end   = mem.objects.end;
      header._mem_conses_start  = mem.conses.start;
      header._mem_conses_end    = mem.conses.end;
      #endif
      #ifndef SPVW_MIXED_BLOCKS
      header._heapcount = heapcount;
      #endif
      WRITE(&header,sizeof(header));
      #ifndef SPVW_MIXED_BLOCKS
      #ifdef SPVW_PAGES
      {var reg6 uintL heapnr;
       for (heapnr=0; heapnr<heapcount; heapnr++)
         { var uintC pagecount = 0;
           map_heap(mem.heaps[heapnr],page, { pagecount++; } );
           WRITE(&pagecount,sizeof(pagecount));
      }  }
      #endif
      {var reg6 uintL heapnr;
       for (heapnr=0; heapnr<heapcount; heapnr++)
         { map_heap(mem.heaps[heapnr],page,
             { var memdump_page _page;
               _page._page_start = page->page_start;
               _page._page_end = page->page_end;
               WRITE(&_page,sizeof(_page));
             });
      }  }
      #endif
      # fsubr_tab, subr_tab, symbol_tab, object_tab raussschreiben:
      WRITE(&fsubr_tab,sizeof(fsubr_tab));
      WRITE(&subr_tab,sizeof(subr_tab));
      WRITE(&pseudofun_tab,sizeof(pseudofun_tab));
      WRITE(&symbol_tab,sizeof(symbol_tab));
      WRITE(&object_tab,sizeof(object_tab));
      #ifdef SPVW_MIXED_BLOCKS
      # Objekte variabler Länge rausschreiben:
      {var reg2 uintL len = header._mem_objects_end - header._mem_objects_start;
       WRITE(header._mem_objects_start,len);
      }
      # Conses rausschreiben:
      {var reg2 uintL len = header._mem_conses_end - header._mem_conses_start;
       WRITE(header._mem_conses_start,len);
      }
      #endif
      #ifndef SPVW_MIXED_BLOCKS
      {var reg6 uintL heapnr;
       for (heapnr=0; heapnr<heapcount; heapnr++)
         { map_heap(mem.heaps[heapnr],page,
             { var reg2 uintL len = page->page_end - page->page_start;
               WRITE(page->page_start,len);
             });
      }  }
      #endif
      #undef WRITE
      # Stream schließen (Stream-Buffer ist unverändert, aber dadurch wird
      # auch das Handle beim Betriebssystem geschlossen):
      stream_close(&STACK_0);
      skipSTACK(1);
    }}

# UP, lädt Speicherabbild von Diskette
# loadmem(filename);
# Zerstört alle LISP-Daten.
  # Aktualisierung eines Objektes im Speicher:
  #ifdef SPVW_MIXED_BLOCKS
  local var oint offset_objects_o;
  local var oint offset_conses_o;
  #endif
  #ifdef SPVW_PAGES
  local var struct { aint old_page_start; oint offset_page_o; } *offset_pages;
  #define addr_mask  ~(((oint_addr_mask>>oint_addr_shift) & ~ (wbit(oint_addr_len)-1)) << addr_shift) # meist = ~0
  #define pagenr_of(addr)  floor(addr,min_page_size_brutto)
  #define offset_pages_len  (pagenr_of((wbit(oint_addr_len)-1)<<addr_shift)+1)
  #endif
  #if !defined(SINGLEMAP_MEMORY)
  local var oint offset_fsubrs_o;
  local var oint offset_subrs_o;
  local var oint offset_symbols_o;
  #if !defined(MULTIMAP_MEMORY)
  local var oint old_symbol_tab_o;
  #endif
  #endif
  local var struct pseudofun_tab_ old_pseudofun_tab;
  local void loadmem_aktualisiere (object* objptr);
  local void loadmem_aktualisiere(objptr)
    var reg3 object* objptr;
    { switch (mtypecode(*objptr))
        { case_symbol: # Symbol
            #ifndef SPVW_PURE_BLOCKS
            #if !defined(MULTIMAP_MEMORY)
            if ((oint)(*objptr) - old_symbol_tab_o
                < ((oint)sizeof(symbol_tab)<<(oint_addr_shift-addr_shift))
               )
              # Symbol aus symbol_tab
              { *(oint*)objptr += offset_symbols_o; break; }
            #else
            if ((oint)(*objptr) - (oint)(&symbol_tab)
                < (sizeof(symbol_tab)<<(oint_addr_shift-addr_shift))
               )
              # Symbol aus symbol_tab erfährt keine Verschiebung
              { break; }
            #endif
            # sonstige Symbole sind Objekte variabler Länge.
            #endif
          case_array:
          case_record:
          case_bignum:
          #ifndef WIDE
          case_ffloat:
          #endif
          case_dfloat:
          case_lfloat:
            # Objekt variabler Länge
            #ifdef SPVW_MIXED_BLOCKS
            *(oint*)objptr += offset_objects_o; break;
            #endif
          case_cons: case_ratio: case_complex:
            # Zwei-Pointer-Objekt
            #ifdef SPVW_MIXED_BLOCKS
            *(oint*)objptr += offset_conses_o; break;
            #endif
            #ifdef SPVW_PAGES
            {var reg2 aint addr = upointer(*(oint*)objptr); # Adresse
             # Da Pages eine minimale Länge haben, also die Anfangsadressen
             # unterschiedlicher Pages sich um mindestens min_page_size_brutto
             # unterscheiden, ist es ganz einfach, aus der Adresse auf die
             # Page zurückzuschließen:
             var reg1 uintL pagenr = pagenr_of(addr & addr_mask);
             if (addr < offset_pages[pagenr].old_page_start) { pagenr--; }
             *(oint*)objptr += offset_pages[pagenr].offset_page_o;
            }
            break;
            #endif
            #ifdef SPVW_PURE_BLOCKS # SINGLEMAP_MEMORY
            break; # Alles Bisherige erfährt keine Verschiebung
            #endif
          case_subr: # SUBR
            #ifndef SPVW_PURE_BLOCKS
            *(oint*)objptr += offset_subrs_o;
            #endif
            break;
          case_fsubr: # FSUBR
            #ifndef SPVW_PURE_BLOCKS
            *(oint*)objptr += offset_fsubrs_o;
            #endif
            break;
          case_system: # Frame-Pointer oder Read-Label oder System-Konstante
            if ((*(oint*)objptr & wbit(oint_addr_len-1+oint_addr_shift)) ==0)
              # Frame-Pointer -> #<DISABLED>
              { *objptr = disabled; }
            break;
          case_machine: # Pseudo-Funktion oder sonstiger Maschinenpointer
            # Umsetzung old_pseudofun_tab -> pseudofun_tab :
            {
              #if (machine_type==0)
              var reg4 void* addr = ThePseudofun(*objptr);
              #else # muß zum Vergleichen die Typinfo wegnehmen
              var reg4 void* addr = (void*)upointer(*objptr);
              #endif
              var reg2 uintC i = pseudofun_anz;
              var reg1 Pseudofun* ptr = &((Pseudofun*)(&old_pseudofun_tab))[pseudofun_anz];
              until (i==0) { i--; if (*--ptr == addr) goto pseudofun_found; }
              # sonstiger Maschinenpointer
              break;
              pseudofun_found: # Pseudo-Funktion
              *objptr = type_pointer_object(machine_type,((Pseudofun*)(&pseudofun_tab))[i]);
              break;
            }
          case_char:
          case_fixnum:
          case_sfloat:
          #ifdef WIDE
          case_ffloat:
          #endif
            break;
          default: /*NOTREACHED*/ abort();
    }   }
  local void loadmem(filename)
    char* filename;
    { # File zum Lesen öffnen:
      begin_system_call();
     {
      #ifdef ATARI
      var reg4 WORD handle = GEMDOS_open(filename,0);
      if (handle<0)
        { if (!(handle==GEMDOS_open_NotFound)) goto abbruch1;
          # sanftere Behandlung des Fehlers, daß das lispinit.mem nicht da ist:
          end_system_call();
          asciz_out(DEUTSCH ? "** WARNUNG: ** Initialisierungsfile " :
                    ENGLISH ? "** WARNING: ** initialization file " :
                    FRANCAIS ? "** AVERTISSEMENT : ** Le fichier d'initialisation " :
                    ""
                   );
          asciz_out(filename);
          asciz_out(DEUTSCH ? " nicht gefunden." CRLFstring :
                    ENGLISH ? " not found" CRLFstring :
                    FRANCAIS ? " n'a pas été trouvé." CRLFstring :
                    ""
                   );
          initmem();
          return;
        }
      #endif
      #ifdef AMIGAOS
      var reg4 Handle handle = Open(filename,MODE_OLDFILE);
      if (handle==Handle_NULL) goto abbruch1;
      #endif
      #if defined(DJUNIX) || defined(EMUNIX)
      var reg4 int handle = open(filename,O_RDONLY);
      if (handle<0) goto abbruch1;
      setmode(handle,O_BINARY);
      #endif
      #ifdef UNIX
      var reg4 int handle = open(filename,O_RDONLY,my_open_mask);
      if (handle<0) goto abbruch1;
      #endif
      end_system_call();
      #ifdef ATARI
        #define READ(buf,len)  \
          { begin_system_call();                                   \
           {var reg1 sintL ergebnis = GEMDOS_read(handle,len,buf); \
            end_system_call();                                     \
            if (ergebnis<0) goto abbruch1;                         \
            if (!(ergebnis==(len))) goto abbruch2;                 \
          }}
      #endif
      #ifdef AMIGAOS
        #define READ(buf,len)  \
          { begin_system_call();                                   \
           {var reg1 sintL ergebnis = Read(handle,(void*)buf,len); \
            end_system_call();                                     \
            if (ergebnis<0) goto abbruch1;                         \
            if (!(ergebnis==(len))) goto abbruch2;                 \
          }}
      #endif
      #if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX)
        #define READ(buf,len)  \
          { begin_system_call();                                      \
           {var reg1 sintL ergebnis = read(handle,(RW_BUF_T)buf,len); \
            end_system_call();                                        \
            if (ergebnis<0) goto abbruch1;                            \
            if (!(ergebnis==(len))) goto abbruch2;                    \
          }}
      #endif
      # Grundinformation lesen:
      {var memdump_header header;
       READ(&header,sizeof(header));
       if (!(header._magic == memdump_magic)) goto abbruch2;
       if (!(header._fsubr_anz == fsubr_anz)) goto abbruch2;
       if (!(header._subr_anz == subr_anz)) goto abbruch2;
       if (!(header._pseudofun_anz == pseudofun_anz)) goto abbruch2;
       if (!(header._symbol_anz == symbol_anz)) goto abbruch2;
       if (!(header._object_anz == object_anz)) goto abbruch2;
       #ifndef SPVW_MIXED_BLOCKS
       if (!(header._heapcount == heapcount)) goto abbruch2;
       #endif
       #ifdef SPVW_MIXED_BLOCKS
       # Offsets berechnen (Offset = neue Adresse - alte Adresse):
       {var reg5 sintL offset_objects = # Offset für Objekte variabler Länge
          mem.objects.start - header._mem_objects_start;
        var reg5 sintL offset_conses = # Offset für Zwei-Pointer-Objekte
          mem.conses.end - header._mem_conses_end;
        # neue Speicheraufteilung berechnen:
        mem.objects.end = header._mem_objects_end + offset_objects;
        mem.conses.start = header._mem_conses_start + offset_conses;
        # Feststellen, ob der Speicherplatz reicht:
        # Er reicht genau dann, wenn
        # geforderter Platz <= vorhandener Platz  <==>
        # header._mem_conses_end-header._mem_conses_start + header._mem_objects_end-header._mem_objects_start
        #   <= mem.conses.end - mem.objects.start  <==>
        # header._mem_objects_end + mem.objects.start-header._mem_objects_start
        #   <= header._mem_conses_start + mem.conses.end-header._mem_conses_end  <==>
        # mem.objects.end <= mem.conses.start
        if (!( (sintL)(mem.objects.end) <= (sintL)(mem.conses.start) )) goto abbruch3;
        # Aktualisierung vorbereiten:
        offset_objects_o = (oint)offset_objects << (oint_addr_shift-addr_shift);
        offset_conses_o = (oint)offset_conses << (oint_addr_shift-addr_shift);
       }
       #endif
       #ifdef SPVW_PURE_BLOCKS # SINGLEMAP_MEMORY
       if (!((aint)(&fsubr_tab) == header._fsubr_tab_addr)) goto abbruch2;
       if (!((aint)(&subr_tab) == header._subr_tab_addr)) goto abbruch2;
       if (!((aint)(&symbol_tab) == header._symbol_tab_addr)) goto abbruch2;
       #else
       offset_fsubrs_o = (oint)((aint)(&fsubr_tab) - header._fsubr_tab_addr) << (oint_addr_shift-addr_shift);
       offset_subrs_o = (oint)((aint)(&subr_tab) - header._subr_tab_addr) << (oint_addr_shift-addr_shift);
       offset_symbols_o = (oint)((aint)(&symbol_tab) - header._symbol_tab_addr) << (oint_addr_shift-addr_shift);
       #ifdef MULTIMAP_MEMORY
       if (!(offset_symbols_o == 0)) goto abbruch2;
       #else
       old_symbol_tab_o = (oint)type_pointer_object(symbol_type,header._symbol_tab_addr);
       #endif
       #endif
       #ifdef SPVW_PURE_BLOCKS
       # Start- und Endadressen jedes Heaps gleich in mem.heaps[] übernehmen:
       {var reg6 uintL heapnr;
        for (heapnr=0; heapnr<heapcount; heapnr++)
          { map_heap(mem.heaps[heapnr],page,
              { var memdump_page _page;
                READ(&_page,sizeof(_page));
                page->page_start = _page._page_start;
                page->page_end = _page._page_end;
              });
       }  }
       #endif
       #ifdef SPVW_PAGES
       {var reg8 uintC total_pagecount;
        #ifdef SPVW_BLOCKS
        total_pagecount = heapcount;
        #endif
        #ifdef SPVW_PAGES
        var uintC pagecounts[heapcount];
        # Pages-per-Heap-Tabelle initialisieren:
        READ(&pagecounts,sizeof(pagecounts));
        # total_pagecount berechnen:
        {var reg1 uintL heapnr;
         total_pagecount = 0;
         for (heapnr=0; heapnr<heapcount; heapnr++)
           { total_pagecount += pagecounts[heapnr]; }
        }
        #endif
        # Offset-per-Page-Tabelle initialisieren:
        offset_pages = malloc(offset_pages_len*sizeof(*offset_pages));
        if (offset_pages==NULL) goto abbruch3;
        {var reg1 uintL pagenr;
         for (pagenr=0; pagenr<offset_pages_len; pagenr++)
           { offset_pages[pagenr].old_page_start = ~0L;
             offset_pages[pagenr].offset_page_o = 0;
        }  }
        # Adressen und Größen der Pages lesen und Pages allozieren:
        { var reg10 DYNAMIC_ARRAY(old_pages,memdump_page,total_pagecount);
          READ(old_pages,total_pagecount*sizeof(memdump_page));
         {var reg10 DYNAMIC_ARRAY(new_pages,aint,total_pagecount);
          {var reg6 memdump_page* old_page_ptr = &old_pages[0];
           var reg6 aint* new_page_ptr = &new_pages[0];
           var reg6 uintL heapnr;
           for (heapnr=0; heapnr<heapcount; heapnr++)
             {var reg6 Pages* pages_ptr = &mem.heaps[heapnr].inuse;
              #ifdef SPVW_PAGES
              var reg5 uintC pagecount = pagecounts[heapnr];
              until (pagecount==0)
                {
              #endif
                  var reg5 uintL need = old_page_ptr->_page_end - old_page_ptr->_page_start;
                  var reg5 uintL size1 = round_up(need,sizeof(cons_));
                  if (size1 < std_page_size) { size1 = std_page_size; }
                  {var reg7 uintL size2 = size1 + sizeof(NODE) + (Varobject_alignment-1);
                   var reg6 aint addr = (aint)mymalloc(size2);
                   if ((void*)addr == NULL) goto abbruch3;
                   # Page vom Betriebssystem bekommen.
                   {var reg1 Pages page = (Pages)addr;
                    page->m_start = addr; page->m_length = size2;
                    # Initialisieren:
                    page->page_start = round_up((aint)page+sizeof(NODE),Varobject_alignment);
                    page->page_end = page->page_start + need;
                    page->page_room = size1 - need;
                    # Diesem Heap zuschlagen:
                    *pages_ptr = AVL(AVLID,insert1)(page,*pages_ptr);
                    *new_page_ptr = page->page_start;
                    {var reg4 aint old_page_start = old_page_ptr->_page_start;
                     var reg4 aint old_page_end = old_page_ptr->_page_end;
                     var reg4 oint offset_page_o = (page->page_start - old_page_start) << (oint_addr_shift-addr_shift);
                     var reg1 uintL pagenr = pagenr_of(old_page_start & addr_mask);
                     do { if (!(offset_pages[pagenr].old_page_start == ~0L)) { abort(); }
                          offset_pages[pagenr].old_page_start = old_page_start;
                          offset_pages[pagenr].offset_page_o = offset_page_o;
                          pagenr++;
                        }
                        while (pagenr < pagenr_of(old_page_end & addr_mask));
                  }}}
                  old_page_ptr++; new_page_ptr++;
              #ifdef SPVW_PAGES
                  pagecount--;
                }
              #endif
          }  }
       #endif
       # fsubr_tab, subr_tab, pseudofun_tab, symbol_tab, object_tab lesen:
       {var struct fsubr_tab_ old_fsubr_tab;
        READ(&old_fsubr_tab,sizeof(fsubr_tab));
        {var reg2 fsubr_* ptr1 = (fsubr_*)&old_fsubr_tab;
         var reg1 fsubr_* ptr2 = (fsubr_*)&fsubr_tab;
         var reg3 uintC count;
         dotimesC(count,fsubr_anz,
           { if (!(   (ptr1->req_anz == ptr2->req_anz)
                   && (ptr1->opt_anz == ptr2->opt_anz)
                   && (ptr1->body_flag == ptr2->body_flag)
                ) )
               goto abbruch2;
             ptr2->name = ptr1->name;
             ptr2->argtype = ptr1->argtype;
             ptr1++; ptr2++;
           });
       }}
       {var struct subr_tab_ old_subr_tab;
        READ(&old_subr_tab,sizeof(subr_tab));
        {var reg2 subr_* ptr1 = (subr_*)&old_subr_tab;
         var reg1 subr_* ptr2 = (subr_*)&subr_tab;
         var reg3 uintC count;
         dotimesC(count,subr_anz,
           { if (!(   (ptr1->req_anz == ptr2->req_anz)
                   && (ptr1->opt_anz == ptr2->opt_anz)
                   && (ptr1->rest_flag == ptr2->rest_flag)
                   && (ptr1->key_flag == ptr2->key_flag)
                   && (ptr1->key_anz == ptr2->key_anz)
                ) )
               goto abbruch2;
             ptr2->name = ptr1->name; ptr2->keywords = ptr1->keywords;
             ptr2->argtype = ptr1->argtype;
             ptr1++; ptr2++;
           });
       }}
       READ(&old_pseudofun_tab,sizeof(pseudofun_tab));
       READ(&symbol_tab,sizeof(symbol_tab));
       READ(&object_tab,sizeof(object_tab));
       #ifdef SPVW_PAGES
          # Inhalt der Pages lesen:
          {var reg6 memdump_page* old_page_ptr = &old_pages[0];
           var reg6 aint* new_page_ptr = &new_pages[0];
           until (total_pagecount == 0)
             { var reg2 uintL len = old_page_ptr->_page_end - old_page_ptr->_page_start;
               READ(*new_page_ptr,len);
               old_page_ptr++; new_page_ptr++;
               total_pagecount--;
          }  }
          FREE_DYNAMIC_ARRAY(new_pages);
         }
         FREE_DYNAMIC_ARRAY(old_pages);
        }
       }
       #endif
       #ifdef SPVW_PURE_BLOCKS # SINGLEMAP_MEMORY
       # Inhalt der Blöcke lesen:
       {var reg6 uintL heapnr;
        for (heapnr=0; heapnr<heapcount; heapnr++)
          { var reg2 Heap* heapptr = &mem.heaps[heapnr];
            var reg3 uintL len = heapptr->heap_end - heapptr->heap_start;
            var reg4 uintL map_len = round_up(len,map_pagesize);
            heapptr->heap_limit = heapptr->heap_start + map_len;
            if (map_len > 0)
              { if (zeromap((void*)(heapptr->heap_start),map_len) <0) goto abbruch3;
                READ(heapptr->heap_start,len);
       }  }   }
       set_total_room(used_space()); # bis zur nächsten GC haben wir viel Zeit
       #endif
       #ifdef SPVW_MIXED_BLOCKS
       # Objekte variabler Länge lesen:
       {var reg2 uintL len = header._mem_objects_end - header._mem_objects_start;
        READ(mem.objects.start,len);
       }
       # Conses lesen:
       {var reg2 uintL len = header._mem_conses_end - header._mem_conses_start;
        READ(mem.conses.start,len);
       }
       #endif
       # File schließen:
       #undef READ
       begin_system_call();
       #ifdef ATARI
       {var reg1 WORD ergebnis = GEMDOS_close(handle);
        if (ergebnis<0) goto abbruch1;
       }
       #endif
       #ifdef AMIGAOS
       Close(handle);
       #endif
       #if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX)
       if ( close(handle) <0) goto abbruch1;
       #endif
       end_system_call();
       # Durchlaufen durch alle LISP-Objekte und aktualisieren:
         #define aktualisiere  loadmem_aktualisiere
         # Programmkonstanten aktualisieren:
           aktualisiere_tab();
         # Pointer in den Cons-Zellen aktualisieren:
           aktualisiere_conses();
         # Pointer in den Objekten variabler Länge aktualisieren:
           #define aktualisiere_page  aktualisiere_page_normal
           aktualisiere_varobjects();
           #undef aktualisiere_page
         #undef aktualisiere
       #ifdef SPVW_PAGES
       free(offset_pages);
       #endif
      }
      # offene Files für geschlossen erklären:
      closed_all_files();
      #ifdef MACHINE_KNOWN
        # (MACHINE-TYPE), (MACHINE-VERSION), (MACHINE-INSTANCE)
        # wieder für unbekannt erklären:
        O(machine_type_string) = NIL;
        O(machine_version_string) = NIL;
        O(machine_instance_string) = NIL;
      #endif
      CHECK_AVL_CONSISTENCY();
      CHECK_GC_CONSISTENCY();
      CHECK_GC_UNMARKED();
      CHECK_PACK_CONSISTENCY();
      return;
      abbruch1:
        asciz_out(program_name); asciz_out(": ");
        asciz_out(
          DEUTSCH ? "Betriebssystem-Fehler beim Versuch, das Initialisierungsfile zu laden." CRLFstring :
          ENGLISH ? "operating system error during load of initialisation file" CRLFstring :
          FRANCAIS ? "Erreur système pendant le chargement du fichier d'initialisation." CRLFstring :
          ""
          );
        goto abbruch_quit;
      abbruch2:
        asciz_out(program_name); asciz_out(": ");
        asciz_out(
          DEUTSCH ? "Initialisierungsfile wurde nicht von dieser LISP-Version erzeugt." CRLFstring :
          ENGLISH ? "initialisation file was not created by this version of LISP" CRLFstring :
          FRANCAIS ? "Le fichier d'initialisation ne provient pas de cette version de LISP." CRLFstring :
          ""
          );
        goto abbruch_quit;
      abbruch3:
        asciz_out(program_name); asciz_out(": ");
        asciz_out(
          DEUTSCH ? "Speicherplatz reicht für Initialisierung nicht aus." CRLFstring :
          ENGLISH ? "not enough memory for initialisation" CRLFstring :
          FRANCAIS ? "Il n'y a pas assez de mémoire pour l'initialisation." CRLFstring :
          ""
          );
        goto abbruch_quit;
      abbruch_quit:
        # Abbruch.
        # Zuvor die Datei schließen, falls sie erfolgreich geöffnet worden war.
        # (Hierbei werden Fehler nun aber wirklich ignoriert!)
        #ifdef ATARI
        if (!(handle<0))
          { begin_system_call(); GEMDOS_close(handle); end_system_call(); }
        #endif
        #ifdef AMIGAOS
        if (!(handle==Handle_NULL))
          { begin_system_call(); Close(handle); end_system_call(); }
        #endif
        #if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX)
        if (!(handle<0))
          { begin_system_call(); close(handle); end_system_call(); }
        #endif
        quit_sofort(1);
    }}

# ------------------------------------------------------------------------------
#ifdef ATARI
#                       Fremdprogramm-Aufruf

# UP: Ruft ein Fremdprogramm auf.
# execute(memneed)
# > -(STACK): Filename des Fremdprogramms, ein Simple-ASCIZ-String
# > -(STACK): Argumente (Command Tail), ein Simple-String
# > uintL memneed: Fürs Fremdprogramm zu reservierende Byte-Zahl (gerade)
# < sintL ergebnis : Falls negativ, Fehlernummer.
#                    Sonst Returncode des aufgerufenen Programms.
# STACK wird aufgeräumt
# kann GC auslösen
  global sintL execute (uintL memneed);
  local void move_MEMTOP (sintL delta);
  local nonreturning void fehler_Malloc_failed (void);
  global sintL execute(memneed)
    var reg4 uintL memneed;
    { var reg5 sintL ergebnis; # Returncode des aufgerufenen Programms
      make_space(memneed); # memneed Bytes Platz machen
      move_MEMTOP(-(sintL)memneed); # MEMTOP um memneed heruntersetzen
      # Programm aufrufen:
      { var reg2 object tail = popSTACK(); # Command-Tail
        if (TheSstring(tail)->length > 127) # Länge soll <=127 sein
          { pushSTACK(tail);
            fehler(
                   DEUTSCH ? "Zu langer Command-Tail: ~" :
                   ENGLISH ? "Command tail too long: ~" :
                   FRANCAIS ? "Paramètres de commande trop longs : ~" :
                   ""
                  );
          }
        set_break_sem_1(); # Break verbieten
        run_time_stop(); # Run-Time-Stoppuhr anhalten
        old_keyboard(); # Tastaturabfrage in Urzustand bringen
        LineA_MouseUnhide(); # Maus in Urzustand bringen
        ergebnis =
          GEMDOS_exec_0( &TheSstring(STACK_0)->data[0], # Filename: ab hier die Zeichen
                         &TheSstring(tail)->data[-1], # Tail: ab hier 1 Byte Länge und die Zeichen
                         basepage->EnvStrPtr # Environment: Parent-Environment
                       );
        LineA_MouseHide(); # Maus abschalten
        new_keyboard(); # Keyboard-Input-Stream wieder funktionsfähig machen
        run_time_restart(); # Run-Time-Stoppuhr weiterlaufen lassen
        clr_break_sem_1(); # Break wieder ermöglichen
      }
      # Überprüfen, ob das Programm seinen Speicher freigegeben hat:
      { var reg1 LONG erg = GEMDOS_Malloc(memneed); # alten Platz wieder allozieren
        if ((erg<0) || !(erg==mem.MEMTOP)) # er muß bei mem.MEMTOP anfangen
          fehler( # Filename im STACK
                  DEUTSCH ? "Programm ~ hat seinen Speicher nicht zurückgegeben." :
                  ENGLISH ? "Program ~ did not return its memory." :
                  FRANCAIS ? "Le programme ~ n'a pas rendu la mémoire allouée." :
                  ""
                );
        # alten Platz wieder freigeben (um Blöcke zu verschmelzen):
        if (GEMDOS_Mfree(erg)<0) fehler_Malloc_failed();
      }
      # Speicher wieder verlangen:
      move_MEMTOP((sintL)memneed);
      skipSTACK(1); # Filename vergessen
      return(ergebnis);
    }
# UP: vergrößert den freien Speicherplatz für LISP-Objekte um delta (gerade)
# Bytes, indem die Conses und MEMTOP um delta Bytes nach oben geschoben werden.
  local void move_MEMTOP(delta)
    var reg3 sintL delta;
    { var reg2 aint new_MEMTOP = mem.MEMTOP+delta; # neue obere Speichergrenze
      # gesamten Speicherblock freigeben:
      { var reg1 WORD erg = GEMDOS_Mfree(MEMBLOCK);
        if (erg<0) fehler_Malloc_failed();
      }
      # verkleinerten Speicherblock wieder allozieren:
      { var reg1 LONG erg = GEMDOS_Malloc(new_MEMTOP-MEMBLOCK);
        # Speicherblock sollte bei MEMBLOCK anfangen:
        if ((erg<0) || !(erg==MEMBLOCK)) fehler_Malloc_failed();
      }
      # Neuer Speicherblock liegt an derselben Stelle, ist jedoch
      # um delta Bytes länger.
      move_conses(delta); # Conses um delta Bytes nach oben schieben
      mem.MEMTOP = new_MEMTOP; # und MEMTOP vergrößern
    }
# UP: Fehlermeldung, wenn Malloc oder Mfree Unvorhersehbares produzierte.
# fehler_Malloc_failed();
  local nonreturning void fehler_Malloc_failed()
    { fehler(
             #if DEUTSCH
             "Ärger mit der Speicherverwaltung des Betriebssystems." NLstring
             "Sie sollten das LISP verlassen und neu starten."
             #elif ENGLISH
             "We have problems with the memory allocation practice "
             "of the operation system." NLstring
             "Please leave LISP and restart again."
             #elif FRANCAIS
             "Difficultés avec le système d'allocation mémoire de TOS." NLstring
             "Vous devriez quitter LISP puis le relancer."
             #endif
            );
    }

# Environment-Variablen abfragen:
  global const char * getenv (const char * name); # siehe GETENV(3V)
  global const char * getenv(name)
    var reg4 const char * name;
    {
     #ifdef GNU
      # Wegen verschiedener Parameter-Übergabekonventionen ist es besser,
      # auf basepage->EnvStrPtr nicht selber zuzugreifen, sondern die von
      # crt0.o bereitgestellte Variable environ zu benutzen.
      extern char** environ;
      var reg3 const char * * env = environ;
      loop # env durchlaufen
        { var reg1 const char * next = *env++;
          if (next == NULL) break;
          # vergleiche, ob next mit name und einem '='-Zeichen beginnt:
          { var reg2 const char * nameptr = name;
            while (*next == *nameptr) { next++; nameptr++; }
            if ((*next == '=') && (*nameptr == '\0')) # gefunden?
              { return next+1; }
        } }
      return NULL;
     #endif
     #ifdef ATARI_TURBO
      var reg1 const char * next = basepage->EnvStrPtr; # Environment: Parent-Environment
      loop # env durchlaufen
        { if (*next == '\0') break;
          # vergleiche, ob next mit name und einem '='-Zeichen beginnt:
          { var reg2 const char * nameptr = name;
            while (*next == *nameptr) { next++; nameptr++; }
            if ((*next == '=') && (*nameptr == '\0')) # gefunden?
              { return next+1; }
          }
          # weiterrücken:
          while (!(*next++ == '\0')) { ; }
        }
      return NULL;
     #endif
    }

#endif

# ------------------------------------------------------------------------------

