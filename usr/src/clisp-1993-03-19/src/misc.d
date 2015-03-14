# Diverse Funktionen für CLISP
# Bruno Haible 16.3.1993

#include "lispbibl.c"
#include "arilev0.c"  # für high16, low16 in %%TIME,
                      # für divu in GET-UNIVERSAL-TIME,
                      # für mulu32 in GET-INTERNAL-RUN-TIME, GET-INTERNAL-REAL-TIME


# Eigenwissen:

LISPFUNN(lisp_implementation_type,0)
# (LISP-IMPLEMENTATION-TYPE), CLTL S. 447
  { value1 = O(lisp_implementation_type_string); mv_count=1; }

LISPFUNN(lisp_implementation_version,0)
# (LISP-IMPLEMENTATION-VERSION), CLTL S. 447
  { value1 = O(lisp_implementation_version_string); mv_count=1; }

LISPFUN(version,0,1,norest,nokey,0,NIL)
# (SYSTEM::VERSION) liefert die Version des Runtime-Systems,
# (SYSTEM::VERSION version) überprüft (am Anfang eines FAS-Files),
# ob die Versionen des Runtime-Systems übereinstimmen.
  { var reg1 object arg = popSTACK();
    if (eq(arg,unbound))
      { value1 = O(version); mv_count=1; }
      else
      { if (equal(arg,O(oldversion)) || equal(arg,O(version)))
          { value1 = NIL; mv_count=0; }
          else
          { fehler(
                   DEUTSCH ? "Dieses File stammt von einer anderen Lisp-Version, muß neu compiliert werden." :
                   ENGLISH ? "This file was produced by another lisp version, must be recompiled." :
                   FRANCAIS ? "Ce fichier provient d'une autre version de LISP et doit être recompilé." :
                   ""
                  );
  }   }   }

#ifdef MACHINE_KNOWN

#if !defined(VMS)

LISPFUNN(machinetype,0)
# (MACHINE-TYPE), CLTL S. 447
  { var reg1 object erg = O(machine_type_string);
    if (nullp(erg)) # noch unbekannt?
      { # ja -> holen
        #ifdef HAVE_SYS_UTSNAME_H
        var struct utsname utsname;
        begin_system_call();
        if ( uname(&utsname) <0) { OS_error(); }
        end_system_call();
        pushSTACK(asciz_to_string(&!utsname.machine));
        funcall(L(nstring_upcase),1); # in Großbuchstaben umwandeln
        erg = value1;
        #else
        # Betriebssystem-Kommando 'arch' ausführen und dessen Output
        # in einen String umleiten:
        # (string-upcase
        #   (with-open-stream (stream (make-pipe-input-stream "/bin/arch"))
        #     (read-line stream nil nil)
        # ) )
        pushSTACK(asciz_to_string("/bin/arch"));
        funcall(L(make_pipe_input_stream),1); # (MAKE-PIPE-INPUT-STREAM "/bin/arch")
        pushSTACK(value1); # Stream retten
        pushSTACK(value1); pushSTACK(NIL); pushSTACK(NIL);
        funcall(L(read_line),3); # (READ-LINE stream NIL NIL)
        pushSTACK(value1); # Ergebnis (kann auch NIL sein) retten
        stream_close(&STACK_1); # Stream schließen
        if (!nullp(STACK_0))
          { funcall(L(string_upcase),1); skipSTACK(1); # in Großbuchstaben umwandeln
            erg = value1;
          }
          else
          { skipSTACK(2); erg = NIL; }
        #endif
        # Das Ergebnis merken wir uns für's nächste Mal:
        O(machine_type_string) = erg;
      }
    value1 = erg; mv_count=1;
  }

LISPFUNN(machine_version,0)
# (MACHINE-VERSION), CLTL S. 447
  { var reg1 object erg = O(machine_version_string);
    if (nullp(erg)) # noch unbekannt?
      { # ja -> holen
        #ifdef HAVE_SYS_UTSNAME_H
        var struct utsname utsname;
        begin_system_call();
        if ( uname(&utsname) <0) { OS_error(); }
        end_system_call();
        pushSTACK(asciz_to_string(&!utsname.machine));
        funcall(L(nstring_upcase),1); # in Großbuchstaben umwandeln
        erg = value1;
        #else
        # Betriebssystem-Kommando 'arch -k' ausführen und dessen Output
        # in einen String umleiten:
        # (string-upcase
        #   (with-open-stream (stream (make-pipe-input-stream "/bin/arch -k"))
        #     (read-line stream nil nil)
        # ) )
        pushSTACK(asciz_to_string("/bin/arch -k"));
        funcall(L(make_pipe_input_stream),1); # (MAKE-PIPE-INPUT-STREAM "/bin/arch -k")
        pushSTACK(value1); # Stream retten
        pushSTACK(value1); pushSTACK(NIL); pushSTACK(NIL);
        funcall(L(read_line),3); # (READ-LINE stream NIL NIL)
        pushSTACK(value1); # Ergebnis (kann auch NIL sein) retten
        stream_close(&STACK_1); # Stream schließen
        funcall(L(string_upcase),1); skipSTACK(1); # in Großbuchstaben umwandeln
        #endif
        # Das Ergebnis merken wir uns für's nächste Mal:
        O(machine_version_string) = erg = value1;
      }
    value1 = erg; mv_count=1;
  }

LISPFUNN(machine_instance,0)
# (MACHINE-INSTANCE), CLTL S. 447
  { var reg1 object erg = O(machine_instance_string);
    if (nullp(erg)) # noch unbekannt?
      { # ja -> Hostname abfragen und dessen Internet-Adresse holen:
        # (let* ((hostname (unix:gethostname))
        #        (address (unix:gethostbyname hostname)))
        #   (if (or (null address) (zerop (length address)))
        #     hostname
        #     (apply #'sys::string-concat hostname " ["
        #       (let ((l nil))
        #         (dotimes (i (length address))
        #           (push (sys::decimal-string (aref address i)) l)
        #           (push "." l)
        #         )
        #         (setf (car l) "]") ; statt (pop l) (push "]" l)
        #         (nreverse l)
        # ) ) ) )
        #if defined(HAVE_GETHOSTNAME)
        var char hostname[MAXHOSTNAMELEN+1];
        # Hostname holen:
        begin_system_call();
        if ( gethostname(&!hostname,MAXHOSTNAMELEN) <0) { OS_error(); }
        end_system_call();
        hostname[MAXHOSTNAMELEN] = '\0'; # und durch ein Nullbyte abschließen
        #elif defined(HAVE_SYS_UTSNAME_H)
        # Hostname u.a. holen:
        var struct utsname utsname;
        begin_system_call();
        if ( uname(&utsname) <0) { OS_error(); }
        end_system_call();
        #define hostname utsname.nodename
        #else
        ??
        #endif
        erg = asciz_to_string(&!hostname); # Hostname als Ergebnis
        #ifdef HAVE_GETHOSTBYNAME
        pushSTACK(erg); # Hostname als 1. String
        { var reg5 uintC stringcount = 1;
          # Internet-Information holen:
          var reg4 struct hostent * h = gethostbyname(&!hostname);
          if ((!(h == (struct hostent *)NULL)) && (!(h->h_addr == (char*)NULL))
              && (h->h_length > 0)
             )
            { pushSTACK(asciz_to_string(" ["));
             {var reg2 uintB* ptr = (uintB*)h->h_addr;
              var reg3 uintC count;
              dotimesC(count,h->h_length,
                pushSTACK(fixnum(*ptr++));
                funcall(L(decimal_string),1); # nächstes Byte in dezimal
                pushSTACK(value1);
                pushSTACK(asciz_to_string(".")); # und ein Punkt als Trennung
                );
              STACK_0 = asciz_to_string("]"); # kein Punkt am Schluß
              stringcount += (2*h->h_length + 1);
            }}
          # Strings zusammenhängen:
          erg = string_concat(stringcount);
        }
        #endif
        #undef hostname
        # Das Ergebnis merken wir uns für's nächste Mal:
        O(machine_instance_string) = erg;
      }
    value1 = erg; mv_count=1;
  }

#else # VMS

LISPFUNN(machinetype,0)
# (MACHINE-TYPE), CLTL S. 447
  { value1 = O(machine_type_string); mv_count=1; }

LISPFUNN(machine_version,0)
# (MACHINE-VERSION), CLTL S. 447
  { value1 = O(machine_version_string); mv_count=1; }

LISPFUNN(machine_instance,0)
# (MACHINE-INSTANCE), CLTL S. 447
  { var reg1 object erg = O(machine_instance_string);
    if (nullp(erg)) # noch unbekannt?
      { # ja -> Environment-Variable SYS$NODE abfragen:
        var reg3 const char* hostname = getenv("SYS$NODE");
        if (!(hostname==NULL))
          { # Suche bis Stringende oder ':'
            var reg2 const char* ptr = hostname;
            until ((*ptr=='\0') || (*ptr==':')) { ptr++; }
            erg = make_string((const uintB*)hostname,ptr-hostname);
          }
          else
          { erg = O(machine_type_string); } # unspezifischer Default
        # Das Ergebnis merken wir uns für's nächste Mal:
        O(machine_instance_string) = erg;
      }
    value1 = erg; mv_count=1;
  }

#endif # VMS

#endif # MACHINE_KNOWN

#ifdef HAVE_ENVIRONMENT

LISPFUNN(get_env,1)
# (SYSTEM::GETENV string) liefert den zu string im Betriebssystem-Environment
# assoziierten String oder NIL.
  { var reg2 object arg = popSTACK();
    if (stringp(arg))
      { var reg1 const char* found;
        begin_system_call();
        found = getenv(TheAsciz(string_to_asciz(arg)));
        end_system_call();
        if (!(found==NULL))
          { value1 = asciz_to_string(found); } # gefunden -> String als Wert
          else
          { value1 = NIL; } # nicht gefunden -> Wert NIL
      }
      else
      { value1 = NIL; } # Kein String -> Wert NIL
    mv_count=1;
  }

#endif

LISPFUNN(software_type,0)
# (SOFTWARE-TYPE), CLTL S. 448
  { value1 = O(software_type_string); mv_count=1; }

LISPFUNN(software_version,0)
# (SOFTWARE-VERSION), CLTL S. 448
  { value1 = O(software_version_string); mv_count=1; }

LISPFUNN(identity,1)
# (IDENTITY object), CLTL S. 448
  { value1 = popSTACK(); mv_count=1; }

LISPFUNN(address_of,1)
# (SYS::ADDRESS-OF object) liefert die Adresse von object
  { value1 = UL_to_I((oint)popSTACK()); mv_count=1; }


# Zeitfunktionen:

#ifdef TIME_ATARI
  # Zwei kleinere Bugs:
  # - Wrap-Around der Uhrzeit nach 248 Tagen,
  # - LISP-Uhr geht um +/- 1 Sekunde falsch gegenüber der Atari-Uhr
  #   (weil die beim LISP-System-Start abgefragte Atari-Uhr 0 bis 2 Sekunden
  #    nachgeht).
  # Decoded Time =
  #   Sekunde, Minute, Stunde, Tag, Monat, Jahr, Wochentag, Sommerzeit, Zeitzone
  # Universal Time =
  #   Sekunden seit 1.1.1900
  # Internal Time =
  #   200stel Sekunden seit LISP-System-Start
#endif
#ifdef TIME_AMIGAOS
  # Ein kleineres Bug:
  # - Wrap-Around der Uhrzeit nach 2.7 Jahren.
  # Decoded Time =
  #   Sekunde, Minute, Stunde, Tag, Monat, Jahr, Wochentag, Sommerzeit, Zeitzone
  # Universal Time =
  #   Sekunden seit 1.1.1900
  # Internal Time =
  #   50stel Sekunden seit LISP-System-Start
#endif
#if defined(TIME_MSDOS) || defined(TIME_VMS)
  # Ein kleineres Bug:
  # - Wrap-Around der Uhrzeit nach 1.36 Jahren.
  # Decoded Time =
  #   Sekunde, Minute, Stunde, Tag, Monat, Jahr, Wochentag, Sommerzeit, Zeitzone
  # Universal Time =
  #   Sekunden seit 1.1.1900
  # Internal Time =
  #   100stel Sekunden seit LISP-System-Start
#endif
#ifdef TIME_UNIX
  # Ein kleineres Bug:
  # - %%TIME funktioniert nur für Zeitdifferenzen <= 194 Tagen.
  # Decoded Time =
  #   Sekunde, Minute, Stunde, Tag, Monat, Jahr, Wochentag, Sommerzeit, Zeitzone
  # Universal Time =
  #   Sekunden seit 1.1.1900
  # Internal Time =
  #   Mikrosekunden seit LISP-System-Start
#endif

#ifdef TIME_RELATIVE

# Uhrzeit und Datum beim LISP-Start:
  local decoded_time realstart_datetime;

# UP: Berechnet die Uhrzeit beim LISP-System-Start als Universal Time.
# calc_start_UT(&timepoint)
# > decoded_time timepoint: Zeit beim LISP-System-Start
# < ergebnis: Universal Time
# kann GC auslösen
  local object calc_start_UT (decoded_time* timepoint);
  local object calc_start_UT(timepoint)
    var reg1 decoded_time* timepoint;
    { # (ENCODE-UNIVERSAL-TIME Sekunden Minuten Stunden Tag Monat Jahr) ausführen:
      pushSTACK(timepoint->Sekunden);
      pushSTACK(timepoint->Minuten);
      pushSTACK(timepoint->Stunden);
      pushSTACK(timepoint->Tag);
      pushSTACK(timepoint->Monat);
      pushSTACK(timepoint->Jahr);
      funcall(S(encode_universal_time),6);
      # als Start-Universal-Time abspeichern:
      return O(start_UT) = value1;
    }

# UP: Merkt sich die Uhrzeit beim LISP-System-Start.
# set_start_time(&timepoint);
# > timepoint: Zeit beim LISP-System-Start
# >   timepoint.Sekunden in {0,...,59},
# >   timepoint.Minuten in {0,...,59},
# >   timepoint.Stunden in {0,...,23},
# >   timepoint.Tag in {1,...,31},
# >   timepoint.Monat in {1,...,12},
# >   timepoint.Jahr in {1980,...,2999},
# >   jeweils als Fixnums.
# kann GC auslösen
  global void set_start_time (decoded_time* timepoint);
  global void set_start_time(timepoint)
    var reg1 decoded_time* timepoint;
    { # Start-Zeit merken:
      realstart_datetime = *timepoint;
      # und, wenn möglich, gleich in Universal Time umwandeln:
      if (!eq(Symbol_function(S(encode_universal_time)),unbound))
        # Ist ENCODE-UNIVERSAL-TIME definiert -> sofort in UT umwandeln:
        { calc_start_UT(timepoint); }
    }

#endif

# Liefert die Uhrzeit in Sekunden (seit Systemstart bzw. 1.1.1900) als uintL.
  local uintL real_time_sec (void);
  local uintL real_time_sec()
    {
     #ifdef TIME_1
      var reg2 uintL real_time = get_real_time();
      # real_time := floor(real_time,ticks_per_second) :
      #if (ticks_per_second == 1000000UL)
        divu_3216_3216(real_time>>6,ticks_per_second>>6,real_time=,);
      #elif (ticks_per_second < bit(16))
        divu_3216_3216(real_time,ticks_per_second,real_time=,);
      #else
        divu_3232_3232(real_time,ticks_per_second,real_time=,);
      #endif
     #endif
     #ifdef TIME_2
      var reg2 uintL real_time = (get_real_time())->tv_sec; # Sekunden
      #ifdef TIME_UNIX
      # real_time sind Sekunden seit 1.1.1970
      real_time = 2208988800UL+real_time; # 25567*24*60*60 Sekunden zwischen 1.1.1900 und 1.1.1970
      #endif
     #endif
     return real_time;
    }

LISPFUNN(get_universal_time,0)
# (get-universal-time), CLTL S. 445
#ifdef TIME_RELATIVE
  # (defun get-universal-time ()
  #   (+ (sys::get-start-time)
  #      (floor (get-internal-real-time) internal-time-units-per-second)
  # ) )
  { var reg1 object start_time = O(start_UT);
    if (nullp(start_time)) # Start-Universal-Time noch NIL ?
      # nein -> schon berechnet.
      # ja -> jetzt erst berechnen:
      { start_time = calc_start_UT(&realstart_datetime); }
    # start_time = die Uhrzeit des LISP-System-Starts in Universal Time.
    pushSTACK(start_time);
    pushSTACK(UL_to_I(real_time_sec())); # Sekunden seit Systemstart
    funcall(L(plus),2); # addieren
  }
#endif
#ifdef TIME_ABSOLUTE
  { value1 = UL_to_I(real_time_sec()); mv_count=1; }
#endif

#ifdef TIME_UNIX
LISPFUNN(default_time_zone,0)
# (sys::default-time-zone) liefert die aktuelle Zeitzone
  { var struct timezone tz;
    #ifndef UNIX_HPUX
    begin_system_call();
    if (!( gettimeofday(NULL,&tz) ==0)) { OS_error(); }
    end_system_call();
    #else # HP-UX mag hier keinen Nullpointer!
    var struct timeval tv;
    begin_system_call();
    if (!( gettimeofday(&tv,&tz) ==0)) { OS_error(); }
    end_system_call();
    #endif
    # Zeitzone in Stunden = (Zeitzone in Minuten / 60) :
    pushSTACK(L_to_I(tz.tz_minuteswest));
    pushSTACK(fixnum(60));
    funcall(L(durch),2);
    #ifdef UNIX_HPUX
    # Normalisieren, so daß der Wert zwischen -12 und 12 liegt:
    # (NTH-VALUE 1 (ROUND timezone 24))
    pushSTACK(value1); pushSTACK(fixnum(24)); funcall(L(round),2);
    value1 = value2; mv_count=1;
    #endif
    # Auch tz.tz_dsttime = DST_XXX durchreichen und dann in DEFS1.LSP eine
    # passende Funktion XXX-Sommerzeit-p aufrufen??
  }
#endif

LISPFUNN(get_internal_run_time,0)
# (GET-INTERNAL-RUN-TIME), CLTL S. 446
  { var timescore tm;
    get_running_times(&tm); # Run-Time seit LISP-System-Start abfragen
   #ifdef TIME_1
    value1 = UL_to_I(tm.runtime); mv_count=1; # in Integer umwandeln
   #endif
   #ifdef TIME_2
    { var reg1 internal_time* tp = &tm.runtime; # Run-Time
      # in Mikrosekunden umwandeln: tp->tv_sec * ticks_per_second + tp->tv_usec
     {var reg3 uintL run_time_hi;
      var reg2 uintL run_time_lo;
      mulu32(tp->tv_sec,ticks_per_second, run_time_hi=,run_time_lo=);
      if ((run_time_lo += tp->tv_usec) < tp->tv_usec) { run_time_hi += 1; }
      value1 = L2_to_I(run_time_hi,run_time_lo); mv_count=1;
    }}
   #endif
  }

LISPFUNN(get_internal_real_time,0)
# (GET-INTERNAL-REAL-TIME), CLTL S. 446
#ifdef TIME_1
  { value1 = UL_to_I(get_real_time()); # Real-Time seit LISP-System-Start, als Integer
    mv_count=1;
  }
#endif
#ifdef TIME_2
  { var reg1 internal_time* tp = get_real_time(); # Real-Time absolut
    # in Mikrosekunden umwandeln: tp->tv_sec * ticks_per_second + tp->tv_usec
   {var reg3 uintL real_time_hi;
    var reg2 uintL real_time_lo;
    mulu32(tp->tv_sec,ticks_per_second, real_time_hi=,real_time_lo=);
    if ((real_time_lo += tp->tv_usec) < tp->tv_usec) { real_time_hi += 1; }
    value1 = L2_to_I(real_time_hi,real_time_lo); mv_count=1;
  }}
#endif

#ifdef SLEEP_1
LISPFUNN(sleep,1)
#if defined(TIME_ATARI) || defined(TIME_MSDOS)
# (SYSTEM::%SLEEP delay) wartet delay/200 bzw. delay/100 Sekunden.
# Argument delay muß ein Integer >=0, <2^32 (TIME_MSDOS: sogar <2^31) sein.
  { var reg2 uintL delay = I_to_UL(popSTACK()); # Pausenlänge
    #ifdef EMUNIX_PORTABEL
    #ifdef EMUNIX_OLD_8e
    if (!(_osmode == DOS_MODE))
    #else
    if (TRUE)
    #endif
      # Unter OS/2 (Multitasking!) nicht CPU-Zeit verbraten!
      # select erlaubt eine wunderschöne Implementation von usleep():
      { var struct timeval timeout; # Zeitintervall
        divu_3216_3216(delay,ticks_per_second, timeout.tv_sec =, timeout.tv_usec = 1000000/ticks_per_second * (uintL) );
        begin_system_call();
       {var reg1 int ergebnis = select(FD_SETSIZE,NULL,NULL,NULL,&timeout);
        end_system_call();
        if (ergebnis<0) { OS_error(); }
      }}
      else
    #endif
    { var reg1 uintL endtime = get_real_time() + delay; # zur momentanen Real-Time addieren,
      # ergibt Zeit, bis zu der zu warten ist.
      # warten, bis die Real-Time bei endtime angelangt ist:
      #ifdef TIME_ATARI
      do {} until (get_real_time() == endtime);
      #else # MSDOS rückt die Uhr jedesmal um 5 oder 6 Ticks auf einmal weiter.
      do {} until ((sintL)(get_real_time()-endtime) >= 0);
      #endif
    }
    value1 = NIL; mv_count=1; # 1 Wert NIL
  }
#endif
#ifdef TIME_AMIGAOS
# (SYSTEM::%SLEEP delay) wartet delay/50 Sekunden.
# Argument delay muß ein Integer >=0, <2^32 sein.
  { var reg2 uintL delay = I_to_UL(popSTACK()); # Pausenlänge
    if (delay>0) { begin_system_call(); Delay(delay); end_system_call(); }
    value1 = NIL; mv_count=1; # 1 Wert NIL
  }
#endif
#ifdef TIME_VMS
# (SYSTEM::%SLEEP delay) wartet delay/100 Sekunden.
# Argument delay muß ein Integer >=0, <2^31 sein.
  { var reg2 uintL delay = I_to_UL(popSTACK()); # Pausenlänge
    if (delay>0) { begin_system_call(); vms_sleep(delay); end_system_call(); }
    value1 = NIL; mv_count=1; # 1 Wert NIL
  }
#endif
#endif
#ifdef SLEEP_2
LISPFUNN(sleep,2)
#ifdef TIME_UNIX
# (SYSTEM::%SLEEP delay-seconds delay-useconds) wartet
# delay-seconds Sekunden und delay-useconds Mikrosekunden.
# Argument delay-seconds muß ein Fixnum >=0, <=16700000 sein,
# Argument delay-useconds muß ein Fixnum >=0, <=1000000 sein.
  { var reg3 uintL useconds = posfixnum_to_L(popSTACK());
    var reg2 uintL seconds = posfixnum_to_L(popSTACK());
    begin_system_call();
    #ifdef HAVE_SELECT
      # select erlaubt eine wunderschöne Implementation von usleep():
      { var struct timeval timeout; # Zeitintervall
        timeout.tv_sec = seconds; timeout.tv_usec = useconds;
       {var reg1 int ergebnis;
        signalblock_on(SIGCLD);
        ergebnis = select(FD_SETSIZE,NULL,NULL,NULL,&timeout);
        signalblock_off(SIGCLD);
        if (ergebnis<0) { OS_error(); }
      }}
    #else
      signalblock_on(SIGCLD);
      if (seconds>0) { sleep(seconds); }
      #ifdef HAVE_USLEEP
      if (useconds>0) { usleep(useconds); }
      #endif
      signalblock_off(SIGCLD);
    #endif
    end_system_call();
    value1 = NIL; mv_count=1; # 1 Wert NIL
  }
#endif
#endif

LISPFUNN(time,0)
# (SYSTEM::%%TIME) liefert den bisherigen Time/Space-Verbrauch, ohne selbst
# Platz anzufordern (und damit eventuell selbst eine GC zu verursachen).
# 9 Werte:
#   Real-Time (Zeit seit Systemstart) in 2 Werten,
#   Run-Time (verbrauchte Zeit seit Systemstart) in 2 Werten,
#   GC-Time (durch GC verbrauchte Zeit seit Systemstart) in 2 Werten,
#   #ifdef TIME_ATARI
#     jeweils in 200stel Sekunden,
#     jeweils (ldb (byte 16 16) time) und (ldb (byte 16 0) time).
#   #endif
#   #ifdef TIME_AMIGAOS
#     jeweils in 50stel Sekunden,
#     jeweils (ldb (byte 16 16) time) und (ldb (byte 16 0) time).
#   #endif
#   #if defined(TIME_MSDOS) || defined(TIME_VMS)
#     jeweils in 100stel Sekunden,
#     jeweils (ldb (byte 16 16) time) und (ldb (byte 16 0) time).
#   #endif
#   #ifdef TIME_UNIX
#     jeweils in Mikrosekunden, jeweils ganze Sekunden und Mikrosekunden.
#   #endif
#   Space (seit Systemstart verbrauchter Platz, in Bytes)
#     in 2 Werten: (ldb (byte 24 24) Space), (ldb (byte 24 0) Space).
#   GC-Count (Anzahl der durchgeführten Garbage Collections).
  { var timescore tm;
    get_running_times(&tm); # Run-Time abfragen
    #ifdef TIME_1
      #define as_2_values(time)  \
        pushSTACK(fixnum(high16(time))); \
        pushSTACK(fixnum(low16(time)));
    #endif
    #ifdef TIME_2
      #define as_2_values(time)  \
        pushSTACK(fixnum(time.tv_sec)); \
        pushSTACK(fixnum(time.tv_usec));
    #endif
    as_2_values(tm.realtime); # erste zwei Werte: Real-Time
    as_2_values(tm.runtime); # nächste zwei Werte: Run-Time
    as_2_values(tm.gctime); # nächste zwei Werte: GC-Time
    # nächste zwei Werte: Space
    # tm.gcfreed = von der GC bisher wieder verfügbar gemachter Platz
    {var reg1 uintL used = used_space(); # momentan belegter Platz
     # beides addieren:
     if ((tm.gcfreed.lo += used) < used) { tm.gcfreed.hi += 1; }
    }
    # Jetzt ist tm.gcfreed = bisher insgesamt verbrauchter Platz
    #if (oint_addr_len<24)
      #error "Funktion SYS::%%TIME anpassen!"
    #endif
    # In 24-Bit-Stücke zerhacken:
    pushSTACK(fixnum( ((tm.gcfreed.hi << 8) + (tm.gcfreed.lo >> 24)) & (bit(24)-1) ));
    pushSTACK(fixnum( tm.gcfreed.lo & (bit(24)-1) ));
    # letzter Wert: GC-Count
    pushSTACK(fixnum(tm.gccount));
    funcall(L(values),9); # 9 Werte produzieren
  }


# Errors:

# SYS::*ERROR-COUNT* = Rekursionstiefe der Ausgabe von Errormeldungen

# UP: Beginnt die Ausgabe einer Errormeldung.
# begin_error()
# < STACK_0: Stream (i.a. *ERROR-OUTPUT*)
# < STACK_1: Wert von *error-handler*
# < STACK_2: Argumentliste für *error-handler*
# erniedrigt STACK um 6
  local void begin_error (void);
  local void begin_error()
    { end_system_call(); # keine Betriebssystem-Operation läuft mehr
      #ifdef PENDING_INTERRUPTS
      interrupt_pending = FALSE; # Ctrl-C-Wartezeit ist gleich beendet
      begin_system_call();
      #ifdef HAVE_UALARM
      ualarm(0,0); # SIGALRM-Timer abbrechen
      #else
      alarm(0); # SIGALRM-Timer abbrechen
      #endif
      end_system_call();
      #endif
      # Error-Count erhöhen, bei >3 Ausgabe-Abbruch:
      dynamic_bind(S(error_count),fixnum_inc(Symbol_value(S(error_count)),1));
      if (posfixnum_to_L(Symbol_value(S(error_count))) > 3)
        { # Mehrfach verschachtelte Fehlermeldung.
          Symbol_value(S(error_count)) = Fixnum_0; # Error-Count löschen
          # *PRINT-PRETTY* an NIL binden (um Speicher zu sparen):
          dynamic_bind(S(print_pretty),NIL);
          fehler(
                 DEUTSCH ? "Unausgebbare Fehlermeldung" :
                 ENGLISH ? "Unprintable error message" :
                 FRANCAIS ? "Message inimprimable" :
                 ""
                );
        }
     {var reg1 object error_handler = Symbol_value(S(error_handler)); # *ERROR-HANDLER*
      if (nullp(error_handler))
        { pushSTACK(NIL); pushSTACK(NIL);
          pushSTACK(var_stream(S(error_output))); # Stream *ERROR-OUTPUT*
          terpri(&STACK_0); # neue Zeile
          write_sstring(&STACK_0,O(error_string1)); # "*** - " ausgeben
        }
        else
        { pushSTACK(NIL); pushSTACK(error_handler);
          pushSTACK(make_string_output_stream()); # String-Output-Stream
        }
    }}

# UP: Gibt ein Error-Objekt aus.
  local void write_errorobject (object obj);
  local void write_errorobject(obj)
    var reg1 object obj;
    { if (nullp(STACK_1))
        { dynamic_bind(S(print_escape),T); # *PRINT-ESCAPE* an T binden
          prin1(&STACK_(0+3),obj); # direkt ausgeben
          dynamic_unbind();
        }
        else
        { # obj auf die Argumentliste schieben:
          pushSTACK(obj);
          obj = allocate_cons();
          Car(obj) = popSTACK();
          Cdr(obj) = STACK_2; STACK_2 = obj;
          # und "~S" in den Format-String schreiben:
          write_schar(&STACK_0,'~'); write_schar(&STACK_0,'S');
    }   }

# UP: Gibt ein Error-Character aus.
  local void write_errorchar (object obj);
  local void write_errorchar(obj)
    var reg1 object obj;
    { if (nullp(STACK_1))
        { write_char(&STACK_0,obj); } # direkt ausgeben
        else
        { # obj auf die Argumentliste schieben:
          pushSTACK(obj);
          obj = allocate_cons();
          Car(obj) = popSTACK();
          Cdr(obj) = STACK_2; STACK_2 = obj;
          # und "~A" in den Format-String schreiben:
          write_schar(&STACK_0,'~'); write_schar(&STACK_0,'A');
    }   }

# UP: Gibt einen Errorstring aus. Bei jeder Tilde '~' wird ein Objekt aus dem
# Stack ausgegeben, bei jedem '$' wird ein Character aus dem Stack ausgegeben.
# write_errorstring(errorstring)
# > STACK_0: Stream usw.
# > errorstring: Errorstring (ein unverschieblicher ASCIZ-String)
# > STACK_3, STACK_4, ...: Argumente (für jedes '~' bzw. '$' eines),
#   in umgekehrter Reihenfolge wie bei FUNCALL !
# < ergebnis: STACK-Wert oberhalb des Stream und der Argumente
  local object* write_errorstring (const char* errorstring);
  local object* write_errorstring(errorstring)
    var reg1 const char* errorstring;
    { var reg2 object* argptr = args_end_pointer STACKop 6; # Pointer übern Stream und Frame
      loop
        { var reg3 uintB ch = *errorstring++; # nächstes Zeichen
          if (ch==0) break; # String zu Ende?
          if (ch=='~') # Tilde?
            # ja -> ein Objekt vom Stack ausgeben:
            { write_errorobject(BEFORE(argptr)); }
          elif (ch=='$') # '$' ?
            # ja -> ein Character vom Stack ausgeben:
            { write_errorchar(BEFORE(argptr)); }
          else
            # nein -> Zeichen normal ausgeben:
            { write_char(&STACK_0,code_char(ch)); }
        }
      return argptr;
    }

# Beendet die Ausgabe einer Fehlermeldung und startet neuen Driver.
# end_error();
  local nonreturning void end_error (object* stackptr);
  local nonreturning void end_error(stackptr)
    var reg2 object* stackptr;
    { if (nullp(STACK_1))
        { skipSTACK(3); }
        else
        { # (apply *error-handler* nil errorstring args) ausführen:
          STACK_0 = get_output_stream_string(&STACK_0);
          # Stackaufbau: args, handler, errorstring.
         {var reg1 object error_handler = STACK_1; STACK_1 = NIL;
          apply(error_handler,2,nreverse(STACK_2));
          skipSTACK(1);
        }}
      dynamic_unbind(); # Bindungsframe für sys::*error-count* auflösen,
                        # da keine Fehlermeldungs-Ausgabe mehr aktiv
      set_args_end_pointer(stackptr);
      break_driver(NIL); # Break-Driver aufrufen (kehrt nicht zurück)
      NOTREACHED
    }

# Fehlermeldung mit Errorstring. Kehrt nicht zurück.
# fehler(errorstring);
# > errorstring: Konstanter ASCIZ-String.
#   Bei jeder Tilde wird ein LISP-Objekt vom STACK genommen und statt der
#   Tilde ausgegeben.
  global nonreturning void fehler (const char * errorstring);
  global nonreturning void fehler(errorstring)
    var reg1 const char * errorstring;
    { begin_error(); # Fehlermeldung anfangen
      end_error(write_errorstring(errorstring)); # Fehlermeldung ausgeben, beenden
    }

#ifdef ATARI
  # Behandlung von BIOS- und GEMDOS-Fehlern
  # OS_error(errorcode);
  # > sintW errorcode: negativer Fehlercode
    global nonreturning void OS_error (sintW errorcode);
    global nonreturning void OS_error(errorcode)
      var reg2 sintW errorcode;
      { clr_break_sem_4(); # keine GEMDOS-Operation mehr aktiv
        begin_error(); # Fehlermeldung anfangen
       {var reg1 uintW errcode = -errorcode; # positive Fehlernummer
        # Meldungbeginn ausgeben:
        write_errorstring(errcode < 32
                           ? # Fehlernummern <32 kommen vom BIOS
                             (DEUTSCH ? "BIOS-Fehler " :
                              ENGLISH ? "BIOS error " :
                              FRANCAIS ? "Erreur BIOS " :
                              ""
                             )
                           : # Fehlernummern >=32 kommen vom GEMDOS
                             (DEUTSCH ? "GEMDOS-Fehler " :
                              ENGLISH ? "GEMDOS error " :
                              FRANCAIS ? "Erreur GEMDOS " :
                              ""
                             )
                         );
        # Fehlernummer ausgeben:
        write_errorobject(fixnum(errcode));
        # nach Möglichkeit noch ausführlicher:
        if (errcode < 68)
          {# Zu Fehlernummern <68 ist ein Text da.
           local char* errormsg_table[68] = {
             /*  0 */ DEUTSCH ? "OK, kein Fehler" :
                      ENGLISH ? "Ok, No error" :
                      FRANCAIS ? "Ok, pas d'erreur" :
                      "",
             /*  1 */ DEUTSCH ? "Allgemeiner Fehler" :
                      ENGLISH ? "General error" :
                      FRANCAIS ? "Erreur générale" :
                      "",
             /*  2 */ DEUTSCH ? "Laufwerk nicht da oder nicht bereit" :
                      ENGLISH ? "Drive not ready" :
                      "",
             /*  3 */ DEUTSCH ? "Unbekannter Befehl" :
                      ENGLISH ? "Unknown command" :
                      FRANCAIS ? "Commande inconnue" :
                      "",
             /*  4 */ DEUTSCH ? "Prüfsumme stimmt nicht" :
                      ENGLISH ? "CRC error" :
                      FRANCAIS ? "Mauvais CRC" :
                      "",
             /*  5 */ DEUTSCH ? "Illegale Anforderung, ungültiger Befehl" :
                      ENGLISH ? "Bad request (invalid command)" :
                      FRANCAIS ? "Requête illégale (commande invalide)" :
                      "",
             /*  6 */ DEUTSCH ? "Track nicht gefunden" :
                      ENGLISH ? "Seek error (track not found)" :
                      FRANCAIS ? "Piste non trouvée" :
                      "",
             /*  7 */ DEUTSCH ? "Unknown media (ungültiger Bootsektor)" :
                      ENGLISH ? "Unknown media" :
                      FRANCAIS ? "Médium inconnu (secteur de boot invalide)" :
                      "",
             /*  8 */ DEUTSCH ? "Sektor nicht gefunden" :
                      ENGLISH ? "Sector not found" :
                      FRANCAIS ? "Secteur non trouvé" :
                      "",
             /*  9 */ DEUTSCH ? "Kein Papier" :
                      ENGLISH ? "Printer error (no paper?)" :
                      FRANCAIS ? "Plus de papier" :
                      "",
             /* 10 */ DEUTSCH ? "Fehler beim Schreibzugriff" :
                      ENGLISH ? "Write fault" :
                      FRANCAIS ? "Erreur en écriture" :
                      "",
             /* 11 */ DEUTSCH ? "Fehler beim Lesezugriff" :
                      ENGLISH ? "Read fault" :
                      FRANCAIS ? "Erreur en lecture" :
                      "",
             /* 12 */ DEUTSCH ? "Allgemeiner Fehler" :
                      ENGLISH ? "General error" :
                      FRANCAIS ? "Erreur générale" :
                      "",
             /* 13 */ DEUTSCH ? "Diskette schreibgeschützt" :
                      ENGLISH ? "Disk write-protected" :
                      FRANCAIS ? "Disquette protégée contre l'écriture" :
                      "",
             /* 14 */ DEUTSCH ? "Diskette wurde gewechselt" :
                      ENGLISH ? "Disk was changed" :
                      FRANCAIS ? "La disquette fut changée" :
                      "",
             /* 15 */ DEUTSCH ? "Unbekanntes Gerät" :
                      ENGLISH ? "Unknown device" :
                      FRANCAIS ? "Device inconnu" :
                      "",
             /* 16 */ DEUTSCH ? "Fehlerhafter Sektor, nicht verifizierbar" :
                      ENGLISH ? "Bad sector encountered during verify" :
                      FRANCAIS ? "Trouvé mauvais secteur pendant validation" :
                      "",
             /* 17 */ DEUTSCH ? "Diskette einlegen" :
                      ENGLISH ? "No disk in drive" :
                      FRANCAIS ? "Pas de disquette dans le lecteur" :
                      "",
             /* 18 ... 31 */ "", "", "", "", "", "", "", "", "", "", "", "", "", "",
             /* 32 */ DEUTSCH ? "Ungültige Funktionsnummer" :
                      ENGLISH ? "Invalid function number" :
                      FRANCAIS ? "Numéro de fonction incorrect" :
                      "",
             /* 33 */ DEUTSCH ? "Datei nicht gefunden" :
                      ENGLISH ? "File not found" :
                      FRANCAIS ? "Fichier non trouvé" :
                      "",
             /* 34 */ DEUTSCH ? "Pfadname nicht gefunden" :
                      ENGLISH ? "Path not found" :
                      FRANCAIS ? "Chemin non trouvé" :
                      "",
             /* 35 */ DEUTSCH ? "Zuviele offene Dateien" :
                      ENGLISH ? "Too many files open" :
                      FRANCAIS ? "Trop de fichiers ouverts" :
                      "",
             /* 36 */ DEUTSCH ? "Zugriff verweigert" :
                      ENGLISH ? "Access not possible" :
                      FRANCAIS ? "Accès non possible" :
                      "",
             /* 37 */ DEUTSCH ? "Ungültige Handle-Nummer" :
                      ENGLISH ? "Invalid handle number" :
                      FRANCAIS ? "numéro de descripteur de fichier invalide" :
                      "",
             /* 38 */ "",
             /* 39 */ DEUTSCH ? "Nicht genügend Speicher" :
                      ENGLISH ? "Not enough memory" :
                      FRANCAIS ? "Pas assez de mémoire" :
                      "",
             /* 40 */ DEUTSCH ? "Ungültige Speicherblockadresse" :
                      ENGLISH ? "Invalid memory block address" :
                      FRANCAIS ? "Adresse de bloc mémoire invalide" :
                      "",
             /* 41 ... 45 */ "", "", "", "", "",
             /* 46 */ DEUTSCH ? "Ungültige Laufwerksbezeichnung" :
                      ENGLISH ? "Invalid drive spec" :
                      FRANCAIS ? "Mauvais descripteur de lecteur" :
                      "",
             /* 47 */ "",
             /* 48 */ DEUTSCH ? "Datei müßte kopiert werden" :
                      ENGLISH ? "Rename across disks impossible" :
                      FRANCAIS ? "Le fichier devrait être copié" :
                      "",
             /* 49 */ DEUTSCH ? "Keine weiteren Dateien" :
                      ENGLISH ? "No more files" :
                      FRANCAIS ? "Pas plus de fichiers" :
                      "",
             /* 50 ... 63 */ "", "", "", "", "", "", "", "", "", "", "", "", "", "",
             /* 64 */ DEUTSCH ? "Bereichsüberschreitung" :
                      ENGLISH ? "Range error, context unknown" :
                      FRANCAIS ? "Valeur hors d'intervalle de validité" :
                      "",
             /* 65 */ DEUTSCH ? "Interner Fehler (Diskettenwechsel?)" :
                      ENGLISH ? "Internal error" :
                      FRANCAIS ? "Erreur interne (changement de disquette?)" :
                      "",
             /* 66 */ DEUTSCH ? "Kein ladbares Programm" :
                      ENGLISH ? "Invalid program load format" :
                      FRANCAIS ? "Fichier non exécutable" :
                      "",
             /* 67 */ DEUTSCH ? "SETBLOCK darf nicht mehr Speicher belegen" :
                      ENGLISH ? "SETBLOCK failed, growth restraints" :
                      FRANCAIS ? "SETBLOCK ne peut pas occuper plus de mémoire" :
                      "",
             };
           var reg2 char* errormsg = errormsg_table[errcode];
           if (!(errormsg[0] == 0)) # nichtleere Meldung?
             { write_errorstring(": ");
               write_errorstring(errormsg);
             }
          }
        end_error(args_end_pointer STACKop 6); # Fehlermeldung beenden
      }}
#endif # ATARI

#ifdef AMIGAOS
  # Behandlung von AMIGAOS-Fehlern
  # OS_error();
  # > IoErr(): Fehlercode
    global nonreturning void OS_error (void);
    global nonreturning void OS_error ()
      { var reg1 uintC errcode = IoErr(); # Fehlernummer
        end_system_call();
        clr_break_sem_4(); # keine AMIGAOS-Operation mehr aktiv
        begin_error(); # Fehlermeldung anfangen
        # Meldungbeginn ausgeben:
        write_errorstring(DEUTSCH ? "AmigaOS-Fehler " :
                          ENGLISH ? "Amiga OS error " :
                          FRANCAIS ? "Erreur AmigaDOS " :
                          ""
                         );
        # Fehlernummer ausgeben:
        write_errorobject(fixnum(errcode));
        { local char* error100_msg_table[2*23] = {
            /* 100 */ "", "",
            /* 101 */ "", "",
            /* 102 */ "", "",
            /* 103 */ "ERROR_NO_FREE_STORE",
                      ENGLISH ? "not enough memory available" :
                      DEUTSCH ? "nicht genügend Speicher vorhanden" :
                      FRANCAIS ? "Pas assez de mémoire" :
                      "",
            /* 104 */ "", "",
            /* 105 */ "ERROR_TASK_TABLE_FULL",
                      ENGLISH ? "process table full" :
                      DEUTSCH ? "keine weiteren CLI Prozesse mehr" :
                      FRANCAIS ? "La table des processus est pleine" :
                      "",
            /* 106 */ "", "",
            /* 107 */ "", "",
            /* 108 */ "", "",
            /* 109 */ "", "",
            /* 110 */ "", "",
            /* 111 */ "", "",
            /* 112 */ "", "",
            /* 113 */ "", "",
            /* 114 */ "ERROR_BAD_TEMPLATE",
                      ENGLISH ? "bad template" :
                      DEUTSCH ? "ungültiges Muster" :
                      "",
            /* 115 */ "ERROR_BAD_NUMBER",
                      ENGLISH ? "bad number" :
                      DEUTSCH ? "ungültige Zahl" :
                      "",
            /* 116 */ "ERROR_REQUIRED_ARG_MISSING",
                      ENGLISH ? "required argument missing" :
                      DEUTSCH ? "benötigtes Schlüsselwort nicht vorhanden" :
                      "",
            /* 117 */ "ERROR_KEY_NEEDS_ARG",
                      ENGLISH ? "value after keyword missing" :
                      DEUTSCH ? "kein Wert nach Schlüsselwort vorhanden" :
                      "",
            /* 118 */ "ERROR_TOO_MANY_ARGS",
                      ENGLISH ? "wrong number of arguments" :
                      DEUTSCH ? "falsche Anzahl Argumente" :
                      "",
            /* 119 */ "ERROR_UNMATCHED_QUOTES",
                      ENGLISH ? "unmatched quotes" :
                      DEUTSCH ? "ausstehende Anführungszeichen" :
                      "",
            /* 120 */ "ERROR_LINE_TOO_LONG",
                      ENGLISH ? "argument line invalid or too long" :
                      DEUTSCH ? "ungültige Zeile oder Zeile zu lang" :
                      "",
            /* 121 */ "ERROR_FILE_NOT_OBJECT",
                      ENGLISH ? "file is not executable" :
                      DEUTSCH ? "Datei ist nicht ausführbar" :
                      FRANCAIS ? "fichier non exécutable" :
                      "",
            /* 122 */ "ERROR_INVALID_RESIDENT_LIBRARY",
                      ENGLISH ? "invalid resident library" :
                      DEUTSCH ? "ungültige residente Library" :
                      FRANCAIS ? "Librarie résidente non valide" :
                      "",
            };
          local char* error200_msg_table[2*44] = {
            /* 200 */ "", "",
            /* 201 */ "ERROR_NO_DEFAULT_DIR",
                      ENGLISH ? "" :
                      DEUTSCH ? "" :
                      "",
            /* 202 */ "ERROR_OBJECT_IN_USE",
                      ENGLISH ? "object is in use" :
                      DEUTSCH ? "Objekt wird schon benutzt" :
                      FRANCAIS ? "l'objet est utilisé" :
                      "",
            /* 203 */ "ERROR_OBJECT_EXISTS",
                      ENGLISH ? "object already exists" :
                      DEUTSCH ? "Objekt existiert bereits" :
                      FRANCAIS ? "l'objet existe déjà" :
                      "",
            /* 204 */ "ERROR_DIR_NOT_FOUND",
                      ENGLISH ? "directory not found" :
                      DEUTSCH ? "Verzeichnis nicht gefunden" :
                      FRANCAIS ? "répertoire non trouvé" :
                      "",
            /* 205 */ "ERROR_OBJECT_NOT_FOUND",
                      ENGLISH ? "object not found" :
                      DEUTSCH ? "Objekt nicht gefunden" :
                      FRANCAIS ? "objet non trouvé" :
                      "",
            /* 206 */ "ERROR_BAD_STREAM_NAME",
                      ENGLISH ? "invalid window description" :
                      DEUTSCH ? "ungültige Fensterbeschreibung" :
                      FRANCAIS ? "mauvais descripteur de fenêtre" :
                      "",
            /* 207 */ "ERROR_OBJECT_TOO_LARGE",
                      ENGLISH ? "object too large" :
                      DEUTSCH ? "Objekt zu groß" :
                      FRANCAIS ? "objet trop grand" :
                      "",
            /* 208 */ "", "",
            /* 209 */ "ERROR_ACTION_NOT_KNOWN",
                      ENGLISH ? "packet request type unknown" :
                      DEUTSCH ? "unbekannter Pakettyp" : # ??
                      FRANCAIS ? "Type de paquet inconnu" :
                      "",
            /* 210 */ "ERROR_INVALID_COMPONENT_NAME",
                      ENGLISH ? "object name invalid" :
                      DEUTSCH ? "ungültiger Objektname" :
                      FRANCAIS ? "nom d'objet incorrect" :
                      "",
            /* 211 */ "ERROR_INVALID_LOCK",
                      ENGLISH ? "invalid object lock" :
                      DEUTSCH ? "ungültiger Objektlock" :
                      FRANCAIS ? "«lock» invalide d'un objet" :
                      "",
            /* 212 */ "ERROR_OBJECT_WRONG_TYPE",
                      ENGLISH ? "object is not of required type" :
                      DEUTSCH ? "Objekt ist nicht von benötigten Typ" :
                      FRANCAIS ? "objet de mauvais type" :
                      "",
            /* 213 */ "ERROR_DISK_NOT_VALIDATED",
                      ENGLISH ? "disk not validated" :
                      DEUTSCH ? "Datenträger ist nicht validiert" :
                      FRANCAIS ? "volume non validé" :
                      "",
            /* 214 */ "ERROR_DISK_WRITE_PROTECTED",
                      ENGLISH ? "disk is write-protected" :
                      DEUTSCH ? "Datenträger ist schreibgeschützt" :
                      FRANCAIS ? "disquette protégée contre l'écriture" :
                      "",
            /* 215 */ "ERROR_RENAME_ACROSS_DEVICES",
                      ENGLISH ? "rename across devices attempted" :
                      DEUTSCH ? "rename über Laufwerke versucht" :
                      FRANCAIS ? "«rename» à travers des unités distinctes" :
                      "",
            /* 216 */ "ERROR_DIRECTORY_NOT_EMPTY",
                      ENGLISH ? "directory not empty" :
                      DEUTSCH ? "Verzeichnis ist nicht leer" :
                      FRANCAIS ? "répertoire non vide" :
                      "",
            /* 217 */ "ERROR_TOO_MANY_LEVELS",
                      ENGLISH ? "too many levels" :
                      DEUTSCH ? "" :
                      "",
            /* 218 */ "ERROR_DEVICE_NOT_MOUNTED",
                      ENGLISH ? "device (or volume) is not mounted" :
                      DEUTSCH ? "Datenträger ist in keinem Laufwerk" :
                      FRANCAIS ? "l'unité n'est dans aucun lecteur" :
                      "",
            /* 219 */ "ERROR_SEEK_ERROR",
                      ENGLISH ? "seek failure" :
                      DEUTSCH ? "seek schlug fehl" :
                      FRANCAIS ? "erreur pendant un déplacement (seek)" :
                      "",
            /* 220 */ "ERROR_COMMENT_TOO_BIG",
                      ENGLISH ? "comment is too long" :
                      DEUTSCH ? "Kommentar ist zu lang" :
                      "",
            /* 221 */ "ERROR_DISK_FULL",
                      ENGLISH ? "disk is full" :
                      DEUTSCH ? "Datenträger ist voll" :
                      FRANCAIS ? "support plein" :
                      "",
            /* 222 */ "ERROR_DELETE_PROTECTED",
                      ENGLISH ? "object is protected from deletion" :
                      DEUTSCH ? "Datei ist gegen Löschen geschützt" :
                      FRANCAIS ? "objet est protégé contre l'effacement" :
                      "",
            /* 223 */ "ERROR_WRITE_PROTECTED",
                      ENGLISH ? "file is write protected" :
                      DEUTSCH ? "Datei ist schreibgeschützt" :
                      FRANCAIS ? "fichier protégé contre l'écriture" :
                      "",
            /* 224 */ "ERROR_READ_PROTECTED",
                      ENGLISH ? "file is read protected" :
                      DEUTSCH ? "Datei ist lesegeschützt" :
                      FRANCAIS ? "fichier protégé contre la lecture" :
                      "",
            /* 225 */ "ERROR_NOT_A_DOS_DISK",
                      ENGLISH ? "not a valid DOS disk" :
                      DEUTSCH ? "kein gültiger DOS-Datenträger" :
                      FRANCAIS ? "disque non DOS" :
                      "",
            /* 226 */ "ERROR_NO_DISK",
                      ENGLISH ? "no disk in drive" :
                      DEUTSCH ? "kein Datenträger im Laufwerk" :
                      FRANCAIS ? "pas de disquette dans le lecteur" :
                      "",
            /* 227 */ "", "",
            /* 228 */ "", "",
            /* 229 */ "", "",
            /* 230 */ "", "",
            /* 231 */ "", "",
            /* 232 */ "ERROR_NO_MORE_ENTRIES",
                      ENGLISH ? "no more entries in directory" :
                      DEUTSCH ? "keine weiteren Verzeichniseinträge mehr" :
                      FRANCAIS ? "pas plus d'entrées dans le répertoire" :
                      "",
            /* 233 */ "ERROR_IS_SOFT_LINK",
                      ENGLISH ? "object is soft link" :
                      DEUTSCH ? "Objekt ist ein Softlink" :
                      FRANCAIS ? "l'objet est un «soft link»" :
                      "",
            /* 234 */ "ERROR_OBJECT_LINKED",
                      ENGLISH ? "object is linked" :
                      DEUTSCH ? "Objekt ist ein Link" : # ??
                      FRANCAIS ? "l'objet est lié" :
                      "",
            /* 235 */ "ERROR_BAD_HUNK",
                      ENGLISH ? "bad loadfile hunk" :
                      DEUTSCH ? "Datei teilweise nicht ladbar" : # ??
                      FRANCAIS ? "fichier pas entièrement chargeable" : # ??
                      "",
            /* 236 */ "ERROR_NOT_IMPLEMENTED",
                      ENGLISH ? "function not implemented" :
                      DEUTSCH ? "unimplementierte Funktion" :
                      FRANCAIS ? "fonction non implémentée" :
                      "",
            /* 237 */ "", "",
            /* 238 */ "", "",
            /* 239 */ "", "",
            /* 240 */ "ERROR_RECORD_NOT_LOCKED",
                      ENGLISH ? "record not locked" :
                      DEUTSCH ? "" :
                      "",
            /* 241 */ "ERROR_LOCK_COLLISION",
                      ENGLISH ? "record lock collision" :
                      DEUTSCH ? "" :
                      "",
            /* 242 */ "ERROR_LOCK_TIMEOUT",
                      ENGLISH ? "record lock timeout" :
                      DEUTSCH ? "" :
                      "",
            /* 243 */ "ERROR_UNLOCK_ERROR",
                      ENGLISH ? "record unlock error" :
                      DEUTSCH ? "" :
                      "",
            };
          local char* error300_msg_table[2*6] = {
            /* 300 */ "", "",
            /* 301 */ "", "",
            /* 302 */ "", "",
            /* 303 */ "ERROR_BUFFER_OVERFLOW",
                      ENGLISH ? "buffer overflow" :
                      DEUTSCH ? "Puffer-Überlauf" :
                      FRANCAIS ? "débordement de tampon" :
                      "",
            /* 304 */ "ERROR_BREAK",
                      ENGLISH ? "break" :
                      DEUTSCH ? "Unterbrechung" :
                      FRANCAIS ? "interruption" :
                      "",
            /* 305 */ "ERROR_NOT_EXECUTABLE",
                      ENGLISH ? "file not executable" :
                      DEUTSCH ? "Datei ist nicht ausführbar" :
                      FRANCAIS ? "fichier non exécutable" :
                      "",
            };
          var reg3 char* errorname = "";
          var reg3 char* errormsg = "";
          var reg2 uintC index;
          if (errcode == 0)
            { errorname = "";
              errormsg =
                /*  0 */ DEUTSCH ? "OK, kein Fehler" :
                         ENGLISH ? "Ok, No error" :
                         FRANCAIS ? "Ok, pas d'erreur" :
                         "";
            }
          elif ((index = errcode-100) < 23)
            { errorname = error100_msg_table[2*index];
              errormsg = error100_msg_table[2*index+1];
            }
          elif ((index = errcode-200) < 44)
            { errorname = error200_msg_table[2*index];
              errormsg = error200_msg_table[2*index+1];
            }
          elif ((index = errcode-300) < 6)
            { errorname = error300_msg_table[2*index];
              errormsg = error300_msg_table[2*index+1];
            }
          if (!(errorname[0] == 0)) # bekannter Name?
            { write_errorstring(" (");
              write_errorstring(errorname);
              write_errorstring(")");
            }
          if (!(errormsg[0] == 0)) # nichtleere Meldung?
            { write_errorstring(": ");
              write_errorstring(errormsg);
            }
        }
        # Fehlercode löschen (fürs nächste Mal):
        ((struct Process *)FindTask(NULL))->pr_Result2 = 0L;
        end_error(args_end_pointer STACKop 6); # Fehlermeldung beenden
      }
#endif

#ifdef DJUNIX
  # Behandlung von DJUNIX-(DOS-)Fehlern
  # OS_error();
  # > int errno: Fehlercode
    global nonreturning void OS_error (void);
    global nonreturning void OS_error ()
      { var reg1 uintC errcode = errno; # positive Fehlernummer
        end_system_call();
        clr_break_sem_4(); # keine DOS-Operation mehr aktiv
        begin_error(); # Fehlermeldung anfangen
        # Meldungbeginn ausgeben:
        write_errorstring(DEUTSCH ? "DJDOS-Fehler " :
                          ENGLISH ? "DJDOS error " :
                          FRANCAIS ? "Erreur DJDOS " :
                          ""
                         );
        # Fehlernummer ausgeben:
        write_errorobject(fixnum(errcode));
        # nach Möglichkeit noch ausführlicher:
        if (errcode < 36)
          {# Zu Fehlernummern <36 ist ein Text da.
           local char* errormsg_table[2*36] = {
             /*  0 */ "", "",
             /*  1 */ "ENOSYS",
                      ENGLISH ? "Function not implemented" :
                      DEUTSCH ? "Funktion ist nicht implementiert" :
                      FRANCAIS ? "fonction non implémentée" :
                      "",
             /*  2 */ "ENOENT",
                      ENGLISH ? "No such file or directory" :
                      DEUTSCH ? "File oder Directory existiert nicht" :
                      FRANCAIS ? "fichier ou répertoire non existant" :
                      "",
             /*  3 */ "ENOTDIR",
                      ENGLISH ? "Not a directory" :
                      DEUTSCH ? "Das ist kein Directory" :
                      FRANCAIS ? "n'est pas un répertoire" :
                      "",
             /*  4 */ "EMFILE",
                      ENGLISH ? "Too many open files" :
                      DEUTSCH ? "Zu viele offene Files" :
                      FRANCAIS ? "Trop de fichiers ouverts" :
                      "",
             /*  5 */ "EACCES",
                      ENGLISH ? "Permission denied" :
                      DEUTSCH ? "Keine Berechtigung" :
                      FRANCAIS ? "Accès dénié" :
                      "",
             /*  6 */ "EBADF",
                      ENGLISH ? "Bad file number" :
                      DEUTSCH ? "File-Descriptor wurde nicht für diese Operation geöffnet" :
                      FRANCAIS ? "déscripteur de fichier non alloué"
                      "",
             /*  7 */ "EARENA",
                      ENGLISH ? "Memory control blocks destroyed" :
                      DEUTSCH ? "Speicherverwaltung ist durcheinander" :
                      FRANCAIS ? "gestionnaire de mémoire perdu" :
                      "",
             /*  8 */ "ENOMEM",
                      ENGLISH ? "Not enough memory" :
                      DEUTSCH ? "Hauptspeicher oder Swapspace reicht nicht" :
                      FRANCAIS ? "Pas assez de mémoire" :
                      "",
             /*  9 */ "ESEGV",
                      ENGLISH ? "Invalid memory address" :
                      DEUTSCH ? "Ungültige Speicher-Adresse" :
                      FRANCAIS ? "adresse mémoire illicite" :
                      "",
             /* 10 */ "EBADENV",
                      ENGLISH ? "Invalid environment" :
                      DEUTSCH ? "Ungültiges Environment" :
                      FRANCAIS ? "environnement incorrect" :
                      "",
             /* 11 */ "", "",
             /* 12 */ "EACCODE",
                      ENGLISH ? "Invalid access code" :
                      DEUTSCH ? "Ungültiger Zugriffsmodus" :
                      FRANCAIS ? "mode d'accès illégal" :
                      "",
             /* 13...14 */ "", "", "", "",
             /* 15 */ "ENODEV",
                      ENGLISH ? "No such device" :
                      DEUTSCH ? "Gerät nicht da oder unpassend" :
                      FRANCAIS ? "il n'y a pas de telle unité" :
                      "",
             /* 16 */ "ECURDIR",
                      ENGLISH ? "Attempt to remove the current directory" :
                      DEUTSCH ? "Das aktuelle Verzeichnis kann nicht entfernt werden" :
                      FRANCAIS ? "Le répertoire courant ne peut pas être effacé" :
                      "",
             /* 17 */ "ENOTSAME",
                      ENGLISH ? "Can't move to other than the same device" :
                      DEUTSCH ? "Verschieben geht nicht über Laufwerksgrenzen hinweg" :
                      FRANCAIS ? "ne peux pas déplacer au-delà de l'unité" :
                      "",
             /* 18 */ "ENOMORE",
                      ENGLISH ? "No more files" :
                      DEUTSCH ? "Keine weiteren Dateien" :
                      FRANCAIS ? "pas plus de fichier" :
                      "",
             /* 19 */ "EINVAL",
                      ENGLISH ? "Invalid argument" :
                      DEUTSCH ? "Ungültiger Parameter" :
                      FRANCAIS ? "Paramètre illicite" :
                      "",
             /* 20 */ "E2BIG",
                      ENGLISH ? "Arg list too long" :
                      DEUTSCH ? "Zu lange Argumentliste" :
                      FRANCAIS ? "liste d'arguments trop longue" :
                      "",
             /* 21 */ "ENOEXEC",
                      ENGLISH ? "Exec format error" :
                      DEUTSCH ? "Kein ausführbares Programm" :
                      FRANCAIS ? "Programme non exécutable" :
                      "",
             /* 22 */ "EXDEV",
                      ENGLISH ? "Cross-device link" :
                      DEUTSCH ? "Links können nur aufs selbe Gerät gehen" :
                      FRANCAIS ? "liens uniquement sur la même unité" :
                      "",
             /* 23...27 */ "", "", "", "", "", "", "", "", "", "",
             /* 28...32 */ "", "", "", "", "", "", "", "", "", "",
             /* 33 */ "EDOM",
                      ENGLISH ? "Argument out of domain" :
                      DEUTSCH ? "Argument zu mathematischer Funktion außerhalb des Definitionsbereichs" :
                      FRANCAIS ? "argument hors du domaine de définition d'une fonction mathématique" :
                      "",
             /* 34 */ "ERANGE",
                      ENGLISH ? "Result too large" :
                      DEUTSCH ? "Ergebnis mathematischer Funktion zu groß" :
                      FRANCAIS ? "débordement de valeur" :
                      "",
             /* 35 */ "EEXIST",
                      ENGLISH ? "File exists" :
                      DEUTSCH ? "File existiert schon" :
                      FRANCAIS ? "Le fichier existe déjà" :
                      "",
             };
           var reg2 char* errorname = errormsg_table[2*errcode];
           var reg2 char* errormsg = errormsg_table[2*errcode+1];
           if (!(errorname[0] == 0)) # bekannter Name?
             { write_errorstring(" (");
               write_errorstring(errorname);
               write_errorstring(")");
             }
           if (!(errormsg[0] == 0)) # nichtleere Meldung?
             { write_errorstring(": ");
               write_errorstring(errormsg);
             }
          }
        end_error(args_end_pointer STACKop 6); # Fehlermeldung beenden
      }
#endif

#if defined(UNIX) || defined(EMUNIX) || defined(VMS)
  # Behandlung von UNIX-Fehlern
  # OS_error();
  # > int errno: Fehlercode
    global nonreturning void OS_error (void);
    global nonreturning void OS_error ()
      { var reg1 uintC errcode = errno; # positive Fehlernummer
        #ifdef VMS
        var reg3 uintL vaxerrcode = vaxc$errno;
        #endif
        end_system_call();
        clr_break_sem_4(); # keine UNIX-Operation mehr aktiv
        begin_error(); # Fehlermeldung anfangen
        #ifdef VMS
        if (errcode==EVMSERR)
          { # anderen Meldungbeginn ausgeben:
            write_errorstring(DEUTSCH ? "VMS-Fehler " :
                              ENGLISH ? "VMS error " :
                              FRANCAIS ? "Erreur VMS " :
                              ""
                             );
            # Fehlernummer ausgeben:
            write_errorobject(fixnum(vaxerrcode));
            # Fehlermeldung des Betriebssystems ausgeben:
            #define MAXERRLEN  256
           {var char buf[MAXERRLEN+1];
            var struct { uintL size; char* addr; } buf_dsc;
            var uintW len;
            buf_dsc.size = MAXERRLEN; buf_dsc.addr = &!buf;
            if (SYS$GETMSG(vaxerrcode,&len,&buf_dsc,1,0) & 1)
              { buf[len] = 0;
                write_errorstring(": ");
                write_errorstring(&!buf);
          }}  }
          else
        #endif
       {# Meldungbeginn ausgeben:
        write_errorstring(DEUTSCH ? "UNIX-Fehler " :
                          ENGLISH ? "UNIX error " :
                          FRANCAIS ? "Erreur UNIX " :
                          ""
                         );
        # Fehlernummer ausgeben:
        write_errorobject(fixnum(errcode));
        #if 0
        { # Fehlermeldung des Betriebssystems ausgeben:
          if (errcode < sys_nerr)
            { var reg2 char* errormsg = sys_errlist[errcode];
              write_errorstring(": ");
              write_errorstring(errormsg);
        }   }
        #else # nach Möglichkeit noch ausführlicher:
        { # Tabelle der Fehlermeldungen wird von GENERRORS.C generiert:
          #include "errors.c"
          if (errcode < errcode_limit)
            # Zu dieser Fehlernummer ist ein Text da.
            { var reg2 char* errorname = errormsg_table[2*errcode];
              var reg2 char* errormsg = errormsg_table[2*errcode+1];
              if (!(errorname[0] == 0)) # bekannter Name?
                { write_errorstring(" (");
                  write_errorstring(errorname);
                  write_errorstring(")");
                }
              if (!(errormsg[0] == 0)) # nichtleere Meldung?
                { write_errorstring(": ");
                  write_errorstring(errormsg);
                }
        }   }
        #endif
       }
        errno = 0; # Fehlercode löschen (fürs nächste Mal)
        end_error(args_end_pointer STACKop 6); # Fehlermeldung beenden
      }
#endif # UNIX || EMUNIX || VMS

LISPFUN(error,1,0,rest,nokey,0,NIL)
# (ERROR errorstring {expr})
# (defun error (errorstring &rest args)
#   (if *error-handler*
#     (apply *error-handler* nil errorstring args)
#     (progn
#       (terpri *error-output*)
#       (write-string "*** - " *error-output*)
#       (apply #'format *error-output* errorstring args)
#   ) )
#   (funcall *break-driver* nil)
# )
  { begin_error(); # Fehlermeldung anfangen
    rest_args_pointer skipSTACKop 1; # Pointer über die Argumente
    {var reg5 object fun;
     var reg4 object arg1;
     if (nullp(STACK_1))
       { fun = S(format); arg1 = STACK_0; } # (FORMAT *error-output* ...)
       else
       { fun = STACK_1; arg1 = NIL; } # (FUNCALL *error-handler* NIL ...)
     skipSTACK(3);
     # Errormeldung ausgeben:
     #   (FORMAT *ERROR-OUTPUT* errorstring {expr})
     # bzw. ({handler} nil errorstring {expr})
     pushSTACK(arg1);
     { var reg1 object* ptr = rest_args_pointer;
       var reg3 uintC count;
       dotimespC(count,1+argcount, { pushSTACK(NEXT(ptr)); } );
     }
     funcall(fun,2+argcount); # fun (= FORMAT bzw. handler) aufrufen
    }
    # Fehlermeldung beenden, vgl. end_error():
    dynamic_unbind(); # Keine Fehlermeldungs-Ausgabe mehr aktiv
    set_args_end_pointer(rest_args_pointer); # STACK aufräumen
    break_driver(NIL); # Break-Driver aufrufen (kehrt nicht zurück)
    NOTREACHED
  }

# UP: Führt eine Break-Schleife wegen Tastaturunterbrechung aus.
# > STACK_0 : aufrufende Funktion
# verändert STACK, kann GC auslösen
  global void tast_break (void);
  global void tast_break()
    {
      #ifdef PENDING_INTERRUPTS
      interrupt_pending = FALSE; # Ctrl-C-Wartezeit ist gleich beendet
      begin_system_call();
      #ifdef HAVE_UALARM
      ualarm(0,0); # SIGALRM-Timer abbrechen
      #else
      alarm(0); # SIGALRM-Timer abbrechen
      #endif
      end_system_call();
      #endif
      pushSTACK(NIL); pushSTACK(NIL); pushSTACK(NIL);
      pushSTACK(NIL); pushSTACK(NIL);
      pushSTACK(var_stream(S(debug_io))); # Stream *DEBUG-IO*
      terpri(&STACK_0); # neue Zeile
      write_sstring(&STACK_0,O(error_string1)); # "*** - " ausgeben
      # String ausgeben, Aufrufernamen verbrauchen, STACK aufräumen:
      set_args_end_pointer(
        write_errorstring(DEUTSCH ? "~: Tastatur-Interrupt" :
                          ENGLISH ? "~: User break" :
                          FRANCAIS ? "~ : Interruption clavier" :
                          "~"
                         ));
      break_driver(T); # Break-Driver aufrufen
    }

