# Streams für CLISP
# Bruno Haible 19.3.1993

#include "lispbibl.c"
#include "arilev0.c" # für R_sign

#ifdef GNU_READLINE
  #define CLISP # Hinweis an chardefs.h; TAB, ESC, RUBOUT definieren wir selbst
  #include <stdio.h> # readline.h benutzt den Typ FILE
  #include "readline.h"
  #include "history.h"
  #undef CLISP
#endif


#if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX) || defined(VMS)
  # Betriebssystem-Funktion read sichtbar machen:
    #undef read
#endif


# Nochmals zum Aufbau von Streams:
# strmflags = Flags
  # Bits in den Flags:
  #define strmflags_ia_bit_B     0  # gesetzt, falls Integer-Stream der Art a
  #define strmflags_ib_bit_B     1  # gesetzt, falls Integer-Stream der Art b
  #define strmflags_ic_bit_B     2  # gesetzt, falls Integer-Stream der Art c
  #define strmflags_rd_by_bit_B  4  # gesetzt, falls READ-BYTE möglich ist
  #define strmflags_wr_by_bit_B  5  # gesetzt, falls WRITE-BYTE möglich ist
  #define strmflags_rd_ch_bit_B  6  # gesetzt, falls READ-CHAR möglich ist
  #define strmflags_wr_ch_bit_B  7  # gesetzt, falls WRITE-CHAR möglich ist
  # Bitmasken in den Flags:
  #define strmflags_ia_B     bit(strmflags_ia_bit_B)
  #define strmflags_ib_B     bit(strmflags_ib_bit_B)
  #define strmflags_ic_B     bit(strmflags_ic_bit_B)
  #define strmflags_rd_by_B  bit(strmflags_rd_by_bit_B)
  #define strmflags_wr_by_B  bit(strmflags_wr_by_bit_B)
  #define strmflags_rd_ch_B  bit(strmflags_rd_ch_bit_B)
  #define strmflags_wr_ch_B  bit(strmflags_wr_ch_bit_B)
  #define strmflags_i_B   (strmflags_ia_B | strmflags_ib_B | strmflags_ic_B)
  #define strmflags_rd_B  (strmflags_rd_by_B | strmflags_rd_ch_B)
  #define strmflags_wr_B  (strmflags_wr_by_B | strmflags_wr_ch_B)
  #define strmflags_by_B  (strmflags_rd_by_B | strmflags_wr_by_B)
  #define strmflags_ch_B  (strmflags_rd_ch_B | strmflags_wr_ch_B)
  # strmflags_open_B: die 4 oberen Bits
# strmtype = Nähere Typinfo. Siehe LISPBIBL.D.
	
# einzelne Komponenten:
  # strm_rd_by       Pseudofunktion für READ-BYTE
  # strm_wr_by       Pseudofunktion für WRITE-BYTE
  # strm_rd_ch       Pseudofunktion für READ-CHAR
  # strm_rd_ch_last  letztes von READ-CHAR gelesenes Zeichen
  #                  (bzw. als Fixnum nach UNREAD-CHAR bzw. NIL sonst)
  # strm_wr_ch       Pseudofunktion für WRITE-CHAR
  # strm_wr_ch_lpos  Line-Position in der Zeile nach dem letzten WRITE-CHAR
  #ifdef STRM_WR_SS
  # strm_wr_ss       Pseudofunktion für WRITE-SIMPLE-STRING
  #endif
# weitere (typspezifische) Komponenten:
  # siehe in LISPBIBL.D und bei den einzelnen Stream-Typen.


# ==============================================================================
#                           S T R E A M S

# Da MAKE-TWO-WAY-STREAM eventuell einen Stream liefern kann, der z.B.
# Character-Input und Byte-Output ist, und damit insbesondere alle
# READ-/WRITE-Operationen effizient laufen, werden Streams folgendermaßen
# aufgebaut:
#    - Typ des Streams,
#    - Komponenten für READ-BYTE,
#    - Komponenten für WRITE-BYTE,
#    - Komponenten für READ-CHAR,
#    - Komponenten für WRITE-CHAR,
#    - vom Typ des Streams abhängige Komponenten.

# Feste Komponenten
# -----------------
# für READ-BYTE:
#     RD_BY          Pseudofunktion zum Lesen eines Bytes
# für WRITE-BYTE:
#     WR_BY          Pseudofunktion zum Schreiben eines Bytes
# für READ-CHAR:
#     RD_CH          Pseudofunktion zum Lesen eines Characters
#     RD_CH_LAST     letztes gelesenes Zeichen und Flag
#                    (NIL zu Beginn, eof_value nach End-of-File,
#                    sonst: letztes gelesenes Zeichen, als Fixnum,
#                    falls es mit UNREAD zurückgeschoben wurde)
# für WRITE-CHAR:
#     WR_CH          Pseudofunktion zum Schreiben eines Characters
#     WR_CH_LPOS     Position in der Zeile (Fixnum >=0) (für FORMAT ~T)

# Pseudofunktionen sind Adressen von C-Funktionen, die direkt angesprungen
# werden können, mit dem Stream als erstem Argument, und ein Objekt
# als Ergebnis liefern.
  #define P(fun)  (type_constpointer_object(machine_type,&(fun)))

# Spezifikation der vier Typen von Pseudofunktionen:
  #
  # Spezifikation für READ-BYTE - Pseudofunktion:
  # fun(stream)
  # > stream: Stream
  # < ergebnis: gelesener Integer (eof_value bei EOF)
  # kann GC auslösen
    typedef object (* rd_by_Pseudofun) (object stream);
  #
  # Spezifikation für WRITE-BYTE - Pseudofunktion:
  # fun(stream,obj)
  # > stream: Stream
  # > obj: auszugebender Integer
  # kann GC auslösen
    typedef void (* wr_by_Pseudofun) (object stream, object obj);
  #
  # Spezifikation für READ-CHAR - Pseudofunktion:
  # fun(&stream)
  # > stream: Stream
  # < stream: Stream
  # < ergebnis: gelesenes Character (eof_value bei EOF)
  # kann GC auslösen
    typedef object (* rd_ch_Pseudofun) (object* stream_);
  #
  # Spezifikation für WRITE-CHAR - Pseudofunktion:
  # fun(&stream,obj)
  # > stream: Stream
  # < stream: Stream
  # > obj: auszugebendes Character
  # kann GC auslösen
    typedef void (* wr_ch_Pseudofun) (object* stream_, object obj);
  #
  #ifdef STRM_WR_SS
  # Spezifikation für WRITE-SIMPLE-STRING - Pseudofunktion:
  # fun(&stream,string,start,len)
  # > string: Simple-String
  # > start: Startindex
  # > len: Anzahl der auszugebenden Zeichen
  # > stream: Stream
  # < stream: Stream
  # kann GC auslösen
    typedef void (* wr_ss_Pseudofun) (object* stream_, object string, uintL start, uintL len);
  #endif

# Pseudofunktionen aus einem Stream herausgreifen:
  #define rd_by(strm)  (*(rd_by_Pseudofun)(ThePseudofun(TheStream(strm)->strm_rd_by)))
  #define wr_by(strm)  (*(wr_by_Pseudofun)(ThePseudofun(TheStream(strm)->strm_wr_by)))
  #define rd_ch(strm)  (*(rd_ch_Pseudofun)(ThePseudofun(TheStream(strm)->strm_rd_ch)))
  #define wr_ch(strm)  (*(wr_ch_Pseudofun)(ThePseudofun(TheStream(strm)->strm_wr_ch)))
  #ifdef STRM_WR_SS
  #define wr_ss(strm)  (*(wr_ss_Pseudofun)(ThePseudofun(TheStream(strm)->strm_wr_ss)))
  #endif

#  Mögliche Typen von Streams                 Zusatzkomponenten
#  --------------------------                 -----------------
#  0. Keyboard-Stream
#  1. Interaktiver Terminalstream             Eingabebuffer, Zeichenzähler
#  2. File-Stream für String-Chars            Handle, Pathname, File-Position,
#     (Input, Output, I/O, Closed=Probe)      Buffer
#  3. File-Stream für Characters              Handle, Pathname, File-Position,
#     (Input, Output, I/O, Closed=Probe)      Buffer
#  4. File-Stream für Unsigned-Bytes          Handle, Pathname, File-Position,
#     (Input, Output, I/O, Closed=Probe)      Buffer, Bit-Buffer
#  5. File-Stream für Signed-Bytes            Handle, Pathname, File-Position,
#     (Input, Output, I/O, Closed=Probe)      Buffer, Bit-Buffer
#  6. Synonym-Stream                          Symbol
#  7. Broadcast-(Output-)Stream               Liste von Streams
#  8. Concatenated-(Input-)Stream             Liste von Streams
#  9. Two-Way-Stream                          Stream für Input, Stream für Output
# 10. Echo-Stream                             Stream für Input, Stream für Output
# 11. String-Input-Stream                     Gesamtstring, Zeichenzähler
# 12. String-Output-Stream                    Buffer (Semi-Simple-String)
# 13. String-Push-Stream                      String mit Fill-Pointer
# 14. Pretty-Printer-Hilfs-Stream             Liste von Buffers, Modus
# 15. Buffered-Input-Stream                   fun, mode, String, Zeichenzähler
# 16. Buffered-Output-Stream                  fun, Buffer (Semi-Simple-String)
# 17. Window-Stream                           ---
#ifdef PRINTER
# 18. Printer-Stream                          Atari: 2 Gemdos-Funktionsnummern
#endif
#ifdef HANDLES
# 19. File-Handle-Stream                      Handle, Pathname
#endif
#ifdef PIPES
# 20. Pipe-Input-Stream                       Pid, Handle
# 21. Pipe-Output-Stream                      Pid, Handle
#endif
#ifdef SOCKETS
# 22. Socket-Stream                           Info, Handle
#endif

# Zusätzlich wird (sicherheitshalber) eine Liste aller offenen File-Streams
# geführt.

# Dummy-Pseudo-Funktionen, die Errors liefern:
  local nonreturning void fehler_illegal (object caller, object stream);
  local nonreturning void fehler_illegal(caller,stream)
    var reg1 object caller;
    var reg2 object stream;
    { pushSTACK(stream);
      pushSTACK(caller);
      fehler(
             DEUTSCH ? "~ auf ~ ist unzulässig." :
             ENGLISH ? "~ on ~ is illegal" :
             FRANCAIS ? "~ de/sur ~ est impossible." :
             ""
            );
    }
  local object rd_by_dummy (object stream);
  local object rd_by_dummy(stream)
    var reg1 object stream;
    { fehler_illegal(S(read_byte),stream); }
  local void wr_by_dummy (object stream, object obj);
  local void wr_by_dummy(stream,obj)
    var reg1 object stream;
    var reg2 object obj;
    { fehler_illegal(S(write_byte),stream); }
  local object rd_ch_dummy (object* stream_);
  local object rd_ch_dummy(stream_)
    var reg1 object* stream_;
    { fehler_illegal(S(read_char),*stream_); }
  local void wr_ch_dummy (object* stream_, object obj);
  local void wr_ch_dummy(stream_,obj)
    var reg1 object* stream_;
    var reg2 object obj;
    { fehler_illegal(S(write_char),*stream_); }
  #ifdef STRM_WR_SS
  local void wr_ss_dummy (object* stream_, object string, uintL start, uintL len);
  local void wr_ss_dummy(stream_,string,start,len)
    var reg3 object* stream_;
    var reg5 object string;
    var reg4 uintL start;
    var reg2 uintL len;
    { if (len==0) return;
     {var reg1 uintL index = start;
      pushSTACK(string); # Simple-String retten
      dotimespL(len,len,
        { write_schar(stream_,TheSstring(STACK_0)->data[index]);
          index++;
        });
      skipSTACK(1);
    }}
  # Dasselbe, wenn write_char auf diesem Stream keine GC auslösen kann:
  local void wr_ss_dummy_nogc (object* stream_, object string, uintL start, uintL len);
  local void wr_ss_dummy_nogc(stream_,string,start,len)
    var reg3 object* stream_;
    var reg5 object string;
    var reg4 uintL start;
    var reg2 uintL len;
    { if (len==0) return;
     {var reg1 uintB* ptr = &TheSstring(string)->data[start];
      dotimespL(len,len, { write_schar(stream_,*ptr++); } );
    }}
  # Am Ende eines wr_ss die Line-Position aktualisieren:
  # wr_ss_lpos(stream,ptr,len);
  # > stream: Stream, nicht der Terminal-Stream
  # > ptr: Pointer ans Ende(!) der bereits auf den Stream ausgegebenen Zeichen
  # > len: Anzahl der Zeichen, >0
  # < ergebnis: TRUE, falls ein NL unter den Zeichen ist, FALSE sonst
  local boolean wr_ss_lpos (object stream, uintB* ptr, uintL len);
  local boolean wr_ss_lpos(stream,ptr,len)
    var reg4 object stream;
    var reg1 uintB* ptr;
    var reg5 uintL len;
    { var reg3 uintL pos = 0; # zähle die Zahl der Zeichen seit dem letzten NL
      var reg2 uintL count;
      dotimespL(count,len, { if (*--ptr == NL) goto found_NL; pos++; } );
      if (FALSE)
        found_NL: # pos Zeichen seit dem letzten NL
        { TheStream(stream)->strm_wr_ch_lpos = fixnum(pos);
          return TRUE;
        }
        else
        # kein NL
        { TheStream(stream)->strm_wr_ch_lpos = fixnum_inc(TheStream(stream)->strm_wr_ch_lpos,len);
          return FALSE;
    }   }
  #endif

# Liest ein Byte von einem Stream.
# read_byte(stream)
# > stream: Stream
# < ergebnis: gelesener Integer (eof_value bei EOF)
# kann GC auslösen
  global object read_byte (object stream);
  global object read_byte(stream)
    var reg1 object stream;
    { return rd_by(stream)(stream); }

# Schreibt ein Byte auf einen Stream.
# write_byte(stream,byte);
# > stream: Stream
# > byte: auszugebender Integer
# kann GC auslösen
  global void write_byte(object stream, object byte);
  global void write_byte(stream,byte)
    var reg1 object stream;
    var reg2 object byte;
    { wr_by(stream)(stream,byte); }

# Liest ein Character von einem Stream.
# read_char(&stream)
# > stream: Stream
# < stream: Stream
# < ergebnis: gelesenes Character (eof_value bei EOF)
# kann GC auslösen
  global object read_char (object* stream_);
  global object read_char(stream_)
    var reg1 object* stream_;
    { var reg2 object stream = *stream_;
      if (!mposfixnump(TheStream(stream)->strm_rd_ch_last)) # Char nach UNREAD ?
        # nein -> neues Zeichen holen:
        { var reg3 object newch = rd_ch(stream)(stream_);
          TheStream(*stream_)->strm_rd_ch_last = newch; # und abspeichern
          return newch;
        }
        else
        # ja -> Flagbit löschen und letztes Zeichen holen:
        { return TheStream(stream)->strm_rd_ch_last =
                   fixnum_to_char(TheStream(stream)->strm_rd_ch_last);
    }   }

# Schiebt das letzte gelesene Character auf einen Stream zurück.
# unread_char(&stream,ch);
# > ch: letztes gelesenes Character
# > stream: Stream
# < stream: Stream
  global void unread_char (object* stream_, object ch);
  global void unread_char(stream_,ch)
    var reg1 object* stream_;
    var reg3 object ch;
    { var reg2 object stream = *stream_;
      if (eq(TheStream(stream)->strm_rd_ch_last,ch))
        { TheStream(stream)->strm_rd_ch_last =
            char_to_fixnum(TheStream(stream)->strm_rd_ch_last); # Flagbit setzen
        }
        else
        { if (mcharp(TheStream(stream)->strm_rd_ch_last))
            { pushSTACK(ch);
              pushSTACK(stream);
              pushSTACK(S(unread_char));
              fehler(
                     DEUTSCH ? "~: Das letzte von ~ gelesene Zeichen war nicht ~." :
                     ENGLISH ? "~: the last character read from ~ was not ~" :
                     FRANCAIS ? "~ : Le dernier caractère lu dans ~ n'était pas ~." :
                     ""
                    );
            }
            else
            { pushSTACK(S(read_char));
              pushSTACK(stream);
              pushSTACK(S(unread_char));
              fehler(
                     DEUTSCH ? "~ von ~ ohne vorheriges ~." :
                     ENGLISH ? "~ from ~ without ~ before it" :
                     FRANCAIS ? "~ de ~ sans précédent ~." :
                     ""
                    );
            }
    }   }

# Liest ein Character von einem Stream, ohne es zu verbrauchen.
# peek_char(&stream)
# > stream: Stream
# < stream: Stream
# < ergebnis: gelesenes Character (eof_value bei EOF)
# kann GC auslösen
  global object peek_char (object* stream_);
  global object peek_char(stream_)
    var reg1 object* stream_;
    { var reg2 object stream = *stream_;
      if (!mposfixnump(TheStream(stream)->strm_rd_ch_last)) # Char nach UNREAD ?
        # nein -> neues Zeichen holen:
        { var reg3 object newch = rd_ch(stream)(stream_);
          # und abspeichern:
          TheStream(*stream_)->strm_rd_ch_last =
            (eq(newch,eof_value) ? newch : char_to_fixnum(newch));
          return newch;
        }
        else
        # ja -> letztes Zeichen holen:
        { return fixnum_to_char(TheStream(stream)->strm_rd_ch_last); }
    }

# Schreibt ein Character auf einen Stream.
# write_char(&stream,ch);
# > ch: auszugebendes Character
# > stream: Stream
# < stream: Stream
# kann GC auslösen
  global void write_char (object* stream_, object ch);
  global void write_char(stream_,ch)
    var reg1 object* stream_;
    var reg4 object ch;
    { var reg3 cint c = char_int(ch);
      # Char schreiben:
      wr_ch(*stream_)(stream_,ch);
      # Line Position aktualisieren:
     {var reg2 object stream = *stream_;
      if (!(TheStream(stream)->strmtype == strmtype_terminal))
        # nicht der Terminal-Stream
        { if (c == NL)
            # Nach Newline: Line Position := 0
            { TheStream(stream)->strm_wr_ch_lpos = Fixnum_0; }
            else
            # Line Position incrementieren:
            { TheStream(stream)->strm_wr_ch_lpos =
                fixnum_inc(TheStream(stream)->strm_wr_ch_lpos,1);
        }   }
        else
        # es ist der Terminal-Stream
        #ifdef TERMINAL_USES_KEYBOARD
        { ; } # Auf dem Atari macht dies wr_ch_terminal() selbst.
        #else
        # Wie wirken sich die Steuerzeichen in der Position aus?
        { if (graphic_char_p(c))
            # normales druckendes Zeichen -> Line Position incrementieren:
            { TheStream(stream)->strm_wr_ch_lpos =
                fixnum_inc(TheStream(stream)->strm_wr_ch_lpos,1);
            }
          elif (c == NL)
            # Newline -> Line Position := 0
            { TheStream(stream)->strm_wr_ch_lpos = Fixnum_0; }
          elif (c == BS)
            # Backspace -> Line Position, wenn möglich, decrementieren:
            { if (!eq(TheStream(stream)->strm_wr_ch_lpos,Fixnum_0))
                { TheStream(stream)->strm_wr_ch_lpos =
                    fixnum_inc(TheStream(stream)->strm_wr_ch_lpos,-1);
            }   }
        }
        #endif
    }}

# UP: Füllt beim Schließen eines Streams die Dummy-Pseudofunktionen ein.
# close_dummys(stream);
# > stream: Stream
  local void close_dummys (object stream);
  local void close_dummys(stream)
    var reg1 object stream;
    { TheStream(stream)->strm_rd_by = P(rd_by_dummy);
      TheStream(stream)->strm_wr_by = P(wr_by_dummy);
      TheStream(stream)->strm_rd_ch = P(rd_ch_dummy);
      TheStream(stream)->strm_rd_ch_last = NIL; # Lastchar := NIL
      TheStream(stream)->strm_wr_ch = P(wr_ch_dummy);
      #ifdef STRM_WR_SS
      TheStream(stream)->strm_wr_ss = P(wr_ss_dummy);
      #endif
      TheStream(stream)->strmflags &= ~strmflags_open_B; # Fähigkeiten-Flags löschen
    }

# Liefert Fehlermeldung, wenn der Wert des Symbols sym kein Stream ist.
  local nonreturning void fehler_value_stream (object sym);
  # siehe unten

# UP: Liefert den Stream, der der Wert einer Variablen ist.
# var_stream(sym)
# > sym: Variable (Symbol)
# < ergebnis: Stream
  global object var_stream (object sym);
  global object var_stream(sym)
    var reg1 object sym;
    { if (!mstreamp(Symbol_value(sym))) { fehler_value_stream(sym); }
      return Symbol_value(sym);
    }

#if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX) || defined(AMIGAOS) || defined(VMS)
# Fehler, wenn aus einem obskuren Grunde ein WRITE nicht gehen sollte:
  local nonreturning void fehler_unwritable (object caller, object stream);
  local nonreturning void fehler_unwritable(caller,stream)
    var reg1 object caller;
    var reg2 object stream;
    { pushSTACK(stream);
      pushSTACK(caller);
      fehler(
             DEUTSCH ? "~: Kann nichts auf ~ ausgeben." :
             ENGLISH ? "~: cannot output to ~" :
             FRANCAIS ? "~ : Ne peux rien écrire sur ~." :
             ""
            );
    }
#endif

# Fehler, wenn ein Objekt kein Character ist:
# fehler_char(stream,obj);
  local nonreturning void fehler_char (object stream, object obj);
  local nonreturning void fehler_char(stream,obj)
    var reg1 object stream;
    var reg2 object obj;
    { pushSTACK(stream);
      pushSTACK(obj);
      fehler(
             DEUTSCH ? "~ ist kein Character und kann daher nicht auf ~ ausgegeben werden." :
             ENGLISH ? "~ is not a character, cannot be output onto ~" :
             FRANCAIS ? "~, n'étant pas de type CHARACTER, ne peut pas être écrit dans ~." :
             ""
            );
    }

# Fehler, wenn ein Character kein String-Char ist:
# fehler_string_char(stream,ch);
  local nonreturning void fehler_string_char (object stream, object ch);
  local nonreturning void fehler_string_char(stream,ch)
    var reg1 object stream;
    var reg2 object ch;
    { pushSTACK(stream);
      pushSTACK(ch);
      fehler(
             DEUTSCH ? "Character ~ ist kein String-Char und kann daher nicht auf ~ ausgegeben werden." :
             ENGLISH ? "character ~ is not a string-char, cannot be output onto ~" :
             FRANCAIS ? "Le caractère ~, n'étant pas de type STRING-CHAR, ne peut pas être écrit dans ~." :
             ""
            );
    }

# Fehler, wenn ein Objekt kein Integer ist:
# fehler_integer(stream,obj);
  local nonreturning void fehler_integer (object stream, object obj);
  local nonreturning void fehler_integer(stream,obj)
    var reg1 object stream;
    var reg2 object obj;
    { pushSTACK(stream);
      pushSTACK(obj);
      fehler(
             DEUTSCH ? "~ ist kein Integer und kann daher nicht auf ~ ausgegeben werden." :
             ENGLISH ? "~ is not an integer, cannot be output onto ~" :
             FRANCAIS ? "~, n'étant pas un entier, ne peut pas être écrit dans ~." :
             ""
            );
    }

# Fehler, wenn ein Integer nicht im passenden Bereich ist:
# fehler_bad_integer(stream,obj);
  local nonreturning void fehler_bad_integer (object stream, object obj);
  local nonreturning void fehler_bad_integer(stream,obj)
    var reg1 object stream;
    var reg2 object obj;
    { pushSTACK(stream);
      pushSTACK(obj);
      fehler(
             DEUTSCH ? "Integer ~ ist zu groß oder zu klein und kann daher nicht auf ~ ausgegeben werden." :
             ENGLISH ? "integer ~ is out of range, cannot be output onto ~" :
             FRANCAIS ? "L'entier ~, n'étant pas dans l'intervalle souhaité, ne peut pas être écrit dans ~." :
             ""
            );
    }

# Fehler, wenn ein Argument kein Fixnum >=0 ist:
# fehler_bad_lpos();
# > STACK_0: lpos
  local nonreturning void fehler_bad_lpos (void);
  local nonreturning void fehler_bad_lpos()
    { # line-position in STACK_0
      pushSTACK(TheSubr(subr_self)->name);
      fehler(
             DEUTSCH ? "~: Argument muß ein Fixnum >=0 sein, nicht ~" :
             ENGLISH ? "~: argument ~ should be a nonnegative fixnum" :
             FRANCAIS ? "~ : L'argument doit être de type FIXNUM positif ou zéro et non pas ~.":
             ""
            );
    }

# Fehler, wenn Argument kein Stream ist:
# fehler_stream(arg);
# > subr_self: Aufrufer (ein SUBR)
  local nonreturning void fehler_stream (object arg);
  local nonreturning void fehler_stream(arg)
    var reg1 object arg;
    { pushSTACK(arg);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(
             DEUTSCH ? "~: Argument muß ein Stream sein, nicht ~" :
             ENGLISH ? "~: argument ~ should be a stream" :
             FRANCAIS ? "~ : L'argument doit être de type STREAM et non pas ~." :
             ""
            );
    }

# UP: Überprüft Argumente, ob sie Streams sind.
# test_stream_args(args_pointer,argcount);
# > args_pointer: Pointer über die Argumente
# > argcount: Anzahl der Argumente
# > subr_self: Aufrufer (ein SUBR)
  local void test_stream_args (object* args_pointer, uintC argcount);
  local void test_stream_args(args_pointer, argcount)
    var reg1 object* args_pointer;
    var reg2 uintC argcount;
    { dotimesC(argcount,argcount,
        { var reg3 object next_arg = NEXT(args_pointer);
          if (!streamp(next_arg)) { fehler_stream(next_arg); }
        });
    }


#if defined(UNIX) || defined(EMUNIX) || defined(DJUNIX) || defined(VMS)

# UP: Löscht bereits eingegebenen interaktiven Input von einem Handle.
  local void clear_tty_input (Handle handle);
  #if !(defined(DJUNIX) || defined(VMS))
  local void clear_tty_input(handle)
    var reg1 Handle handle;
    { # Methode 1: tcflush TCIFLUSH, siehe TERMIOS(3V)
      # Methode 2: ioctl TCFLSH TCIFLUSH, siehe TERMIO(4)
      # Methode 3: ioctl TIOCFLUSH FREAD, siehe TTCOMPAT(4)
      begin_system_call();
      #ifdef UNIX_TERM_TERMIOS
      if (!( tcflush(handle,TCIFLUSH) ==0))
        { if (!((errno==ENOTTY)||(errno==EINVAL))) { OS_error(); } } # kein TTY: OK, sonstigen Error melden
      #endif
      #ifdef UNIX_TERM_TERMIO
      if (!( ioctl(handle,TCFLSH,(caddr_t)TCIFLUSH) ==0))
        { if (!(errno==ENOTTY)) { OS_error(); } } # kein TTY: OK, sonstigen Error melden
      #endif
      #ifdef UNIX_TERM_SGTTY
      {var int arg = FREAD;
       if (!( ioctl(handle,TIOCFLUSH,&arg) ==0))
         { if (!(errno==ENOTTY)) { OS_error(); } } # kein TTY: OK, sonstigen Error melden
      }
      #endif
      #ifdef EMUNIX
      # Eberhard Mattes sagt, das funktioniert nur, wenn IDEFAULT nicht
      # gesetzt ist. ??
      if (!( ioctl(handle,TCFLSH,0) ==0))
        { if (!(errno==ENOTTY)) { OS_error(); } } # kein TTY: OK, sonstigen Erro
      #endif
      end_system_call();
    }
  #else
    #define clear_tty_input(handle)
  #endif

# UP: Bringt den wartenden Output eines Handles ans Ziel.
  local void finish_tty_output (Handle handle);
  #if !(defined(DJUNIX) || defined(VMS))
  local void finish_tty_output(handle)
    var reg1 Handle handle;
    { # Methode 1: fsync, siehe FSYNC(2)
      # Methode 2: tcdrain, siehe TERMIOS(3V)
      # Methode 3: ioctl TCSBRK 1, siehe TERMIO(4)
      # evtl. Methode 3: ioctl TCGETS/TCSETSW, siehe TERMIO(4)
      # oder (fast äquivalent) ioctl TIOCGETP/TIOCSETP, siehe TTCOMPAT(4)
      begin_system_call();
      #if !(defined(UNIX) && !defined(HAVE_FSYNC))
      if (!( fsync(handle) ==0))
        #ifdef EMUNIX_NEW_8e
        if (!(errno==EMSDOS))
        #endif
        if (!(errno==EINVAL)) { OS_error(); }
      #endif
      #ifdef UNIX_TERM_TERMIOS
      if (!( tcdrain(handle) ==0))
        { if (!((errno==ENOTTY)||(errno==EINVAL))) { OS_error(); } } # kein TTY: OK, sonstigen Error melden
      #endif
      #ifdef UNIX_TERM_TERMIO
      if (!( ioctl(handle,TCSBRK,(caddr_t)1) ==0))
        { if (!(errno==ENOTTY)) { OS_error(); } }
      #endif
      #if defined(UNIX_TERM_TERMIOS) && defined(TCGETS) && defined(TCSETSW)
      {var struct termios term_parameters;
       if (!(   ( ioctl(handle,TCGETS,&term_parameters) ==0)
             && ( ioctl(handle,TCSETSW,&term_parameters) ==0)
          ) )
         { if (!(errno==ENOTTY)) { OS_error(); } }
      }
      #endif
      #ifdef EMUNIX
      {var struct termio term_parameters;
       if (!(   ( ioctl(handle,TCGETA,&term_parameters) ==0)
             && ( ioctl(handle,TCSETAW,&term_parameters) ==0)
          ) )
         { if (!(errno==ENOTTY)) { OS_error(); } }
      }
      #endif
      #if 0 # Vorsicht: das müßte FINISH-OUTPUT und CLEAR-INPUT bewirken!
      {var struct sgttyb tty_parameters;
       if (!(   ( ioctl(handle,TIOCGETP,&tty_parameters) ==0)
             && ( ioctl(handle,TIOCSETP,&tty_parameters) ==0)
          ) )
         { if (!(errno==ENOTTY)) { OS_error(); } }
      }
      #endif
      end_system_call();
    }
  #else
    #define finish_tty_output(handle)
  #endif

# UP: Bringt den wartenden Output eines Handles ans Ziel.
  local void force_tty_output (Handle handle);
  #if !(defined(DJUNIX) || defined(VMS) || (defined(UNIX) && !defined(HAVE_FSYNC)))
  local void force_tty_output(handle)
    var reg1 Handle handle;
    { # Methode: fsync, siehe FSYNC(2)
      begin_system_call();
      if (!( fsync(handle) ==0))
        #ifdef EMUNIX_NEW_8e
        if (!(errno==EMSDOS))
        #endif
        if (!(errno==EINVAL)) { OS_error(); }
      end_system_call();
    }
  #else
    #define force_tty_output(handle)
  #endif

# UP: Löscht den wartenden Output eines Handles.
  local void clear_tty_output (Handle handle);
  #if !(defined(DJUNIX) || defined(EMUNIX) || defined(VMS))
  local void clear_tty_output(handle)
    var reg1 Handle handle;
    { # Methode 1: tcflush TCOFLUSH, siehe TERMIOS(3V)
      # Methode 2: ioctl TCFLSH TCOFLUSH, siehe TERMIO(4)
      # Methode 3: ioctl TIOCFLUSH FWRITE, siehe TTCOMPAT(4)
      begin_system_call();
      #ifdef UNIX_TERM_TERMIOS
      if (!( tcflush(handle,TCOFLUSH) ==0))
        { if (!((errno==ENOTTY)||(errno==EINVAL))) { OS_error(); } } # kein TTY: OK, sonstigen Error melden
      #endif
      #ifdef UNIX_TERM_TERMIO
      if (!( ioctl(handle,TCFLSH,(caddr_t)TCOFLUSH) ==0))
        { if (!(errno==ENOTTY)) { OS_error(); } } # kein TTY: OK, sonstigen Error melden
      #endif
      #ifdef UNIX_TERM_SGTTY
      {var int arg = FWRITE;
       if (!( ioctl(handle,TIOCFLUSH,&arg) ==0))
         { if (!(errno==ENOTTY)) { OS_error(); } } # kein TTY: OK, sonstigen Error melden
      }
      #endif
      end_system_call();
    }
  #else
    #define clear_tty_output(handle)
  #endif

#endif

#if defined(AMIGAOS)

# UP: Bringt den wartenden Output eines Handles ans Ziel.
  local void finish_tty_output (Handle handle);
  # Wir können nichts tun, da wir handle nicht schließen dürfen und
  # kein fsync() haben.
  #define finish_tty_output(handle)

# UP: Bringt den wartenden Output eines Handles ans Ziel.
  local void force_tty_output (Handle handle);
  #define force_tty_output(handle)  finish_tty_output(handle)

# UP: Löscht den wartenden Output eines Handles.
  local void clear_tty_output (Handle handle);
  # Nichts zu tun.
  #define clear_tty_output(handle)

#endif


#if (defined(UNIX) || defined(DJUNIX) || defined(EMUNIX) || defined(AMIGAOS) || defined(VMS)) && (!defined(TERMINAL_USES_KEYBOARD) || defined(HANDLES) || defined(PIPES) || defined(SOCKETS))

# Handle-Streams
# ==============

# sind ein gemeinsamer Rahmen für Streams, deren Input/Output ungebuffert
# über ein Handle des Betriebssystems abgewickelt wird. Umfaßt:
# Input: Terminal-Stream, File-Handle-Stream, Pipe-Input-Stream, Socket-Stream.
# Output: File-Handle-Stream, Pipe-Output-Stream, Socket-Stream.

#define strm_isatty   strm_other[0]  # Flag, ob das Input-Handle ein TTY ist
#define strm_ihandle  strm_other[1]  # Input-Handle immer als zweite Komponente
#define strm_ohandle  strm_other[2]  # Output-Handle immer als dritte Komponente

# Daß beim Input EOF erreicht ist, erkennt man an
# TheStream(stream)->strm_rd_ch_last = eof_value.

# READ-CHAR - Pseudofunktion für Handle-Streams:
  local object rd_ch_handle (object* stream_);
  local object rd_ch_handle(stream_)
    var reg4 object* stream_;
    {   restart_it:
     {  var reg2 object stream = *stream_;
        if (eq(TheStream(stream)->strm_rd_ch_last,eof_value)) # schon EOF?
          { return eof_value; }
      { var reg3 Handle handle = TheHandle(TheStream(stream)->strm_ihandle);
        var uintB c;
        run_time_stop(); # Run-Time-Stoppuhr anhalten
        begin_system_call();
       {
        #if !defined(AMIGAOS)
        var reg1 int ergebnis = read(handle,&c,1); # Zeichen lesen versuchen
        #else # defined(AMIGAOS)
        var reg1 long ergebnis = Read(handle,&c,1L); # Zeichen lesen versuchen
        #endif
        end_system_call();
        run_time_restart(); # Run-Time-Stoppuhr weiterlaufen lassen
        if (ergebnis<0)
          {
            #if !defined(AMIGAOS)
            if (errno==EINTR) # Unterbrechung (durch Ctrl-C) ?
              { pushSTACK(S(read_char)); tast_break(); # Break-Schleife aufrufen
                goto restart_it;
              }
            #endif
            OS_error();
          }
        if (ergebnis==0)
          # kein Zeichen verfügbar -> muß EOF sein
          { return eof_value; }
          else
          { return code_char(c); }
    }}}}

# Stellt fest, ob ein Handle-Stream ein Zeichen verfügbar hat.
# listen_handle(stream)
# > stream: Handle-Stream
# < ergebnis:  0 falls Zeichen verfügbar,
#             -1 falls bei EOF angelangt,
#             +1 falls kein Zeichen verfügbar, aber nicht wegen EOF
  local signean listen_handle (object stream);
  local signean listen_handle(stream)
    var reg2 object stream;
    { if (eq(TheStream(stream)->strm_rd_ch_last,eof_value)) # schon EOF ?
        { return signean_minus; }
      # Methode 1: select, siehe SELECT(2)
      # Methode 2: ioctl FIONREAD, siehe FILIO(4)
      # Methode 3: kurzzeitig auf non-blocking I/O schalten und read versuchen,
      #            siehe READ(2V) und FILIO(4), bzw.
      #            siehe READ(2V), FCNTL(2V) und FCNTL(5)
     {var reg3 Handle handle = TheHandle(TheStream(stream)->strm_ihandle);
      #if defined(VMS)
      return signean_plus; # ??
      #elif defined(MSDOS) && !defined(EMUNIX_PORTABEL)
      { var reg2 uintB status;
        begin_system_call();
        get_handle_input_status(handle,status);
        end_system_call();
        if (status) { return signean_null; } # Zeichen verfügbar
      }
      if (!nullp(TheStream(stream)->strm_isatty))
        # Terminal
        { return signean_plus; } # kein Zeichen verfügbar
        else
        # File
        # kein Zeichen verfügbar -> EOF erkennen
        { TheStream(stream)->strm_rd_ch_last = eof_value;
          return signean_minus;
        }
      #elif defined(EMUNIX_PORTABEL)
      { var struct termio oldtermio;
        var struct termio newtermio;
        begin_system_call();
        if (!( ioctl(handle,TCGETA,&oldtermio) ==0))
          { if (!((errno==ENOTTY)||(errno==EINVAL))) { OS_error(); } }
        newtermio = oldtermio;
        newtermio.c_lflag &= ~IDEFAULT & ~ICANON;
        if (!( ioctl(handle,TCSETA,&newtermio) ==0))
          { if (!((errno==ENOTTY)||(errno==EINVAL))) { OS_error(); } }
       {var uintL chars_ready = 0;
        var int result = ioctl(handle,FIONREAD,&chars_ready); # abfragen
        # (Bei EMUNIX_NEW_8f könnte man das auch mit select() machen.)
        if (!( ioctl(handle,TCSETA,&oldtermio) ==0))
          { if (!((errno==ENOTTY)||(errno==EINVAL))) { OS_error(); } }
        end_system_call();
        if (result == 0)
          # Abfrage gelungen
          { if (chars_ready > 0) { return signean_null; } } # welche verfügbar?
        #ifdef EMUNIX_NEW_8e
        begin_system_call();
        if (!isatty(handle))
          { result = eof(handle);
            if (result<0)
              { if (!(errno==ESPIPE)) { OS_error(); } } # "Illegal seek error" ist OK
              else
              { end_system_call();
                if (result>0) # EOF erreicht?
                  { return signean_minus; }
                  else
                  { return signean_null; }
          }   }
        end_system_call();
        #endif
        return signean_plus; # offenbar kein Zeichen verfügbar
      }}
      #elif !defined(AMIGAOS)
      #ifdef HAVE_SELECT
      { # Verwende select mit readfds = einelementige Menge {handle}
        # und timeout = Null-Zeitintervall.
        var fd_set handle_menge; # Menge von Handles := {handle}
        var struct timeval zero_time; # Zeitintervall := 0
        FD_ZERO(&handle_menge); FD_SET(handle,&handle_menge);
        zero_time.tv_sec = 0; zero_time.tv_usec = 0;
        begin_system_call();
       {var reg1 int ergebnis;
        signalblock_on(SIGCLD);
        ergebnis = select(FD_SETSIZE,&handle_menge,NULL,NULL,&zero_time);
        signalblock_off(SIGCLD);
        end_system_call();
        if (ergebnis<0)
          { if (!(errno==EBADF)) { OS_error(); } } # UNIX_LINUX liefert bei Files EBADF !
          else
          { # ergebnis = Anzahl der Handles in handle_menge, bei denen read
            # sofort ein Ergebnis liefern würde.
            if (ergebnis==0)
              { return signean_plus; } # kein Zeichen verfügbar
              else # ergebnis=1
              { return signean_null; } # Zeichen verfügbar
      }}  }
      #endif
      #ifdef HAVE_FIONREAD
      # versuche die Zahl der verfügbaren Zeichen abzufragen:
      begin_system_call();
      {var uintL chars_ready;
       if ( ioctl(handle,FIONREAD,&chars_ready) ==0) # abfragen
         # Abfrage gelungen, also war's ein File
         { end_system_call();
           if (chars_ready > 0) { return signean_null; } # welche verfügbar?
           # sonst EOF des File erkennen:
           TheStream(stream)->strm_rd_ch_last = eof_value;
           return signean_minus;
         }
       if (!((errno == ENOTTY)||(errno == EINVAL))) { OS_error(); }
       end_system_call();
      }# Abfrage mißlungen, war wohl kein File
      #endif
      #ifndef HAVE_SELECT
      if (!nullp(TheStream(stream)->strm_isatty))
        # Terminal
        { # in Non-blocking-Modus umschalten, dann read() versuchen:
          var uintB c;
          var int ergebnis;
          begin_system_call();
          #if 1
          { var int non_blocking_io;
            non_blocking_io = 1;
            if (!( ioctl(handle,FIONBIO,&non_blocking_io) ==0))
              { OS_error(); }
            ergebnis = read(handle,&c,1); # Zeichen lesen versuchen
            non_blocking_io = 0;
            if (!( ioctl(handle,FIONBIO,&non_blocking_io) ==0))
              { OS_error(); }
          }
          #else # ist das portabler??
          { var reg2 int fcntl_flags;
            if (( fcntl_flags = fcntl(handle,F_GETFL,0) )<0) { OS_error(); }
            if ( fcntl(handle,F_SETFL,fcntl_flags|O_NDELAY) <0) { OS_error(); }
            ergebnis = read(handle,&c,1); # Zeichen lesen versuchen
            if ( fcntl(handle,F_SETFL,fcntl_flags) <0) { OS_error(); }
          }
          #endif
          if (ergebnis < 0) { OS_error(); }
          end_system_call();
          if (ergebnis==0)
            # kein Zeichen verfügbar
            { return signean_plus; }
            else
            # Zeichen verfügbar
            { TheStream(stream)->strm_rd_ch_last = fixnum(c);
              return signean_null;
            }
          # Sollte das nicht gehen, einen Timer von 1/10 sec verwenden??
        }
        else
      #endif
        # File (oder Pipe)
        { # ein Zeichen lesen versuchen (wie bei peek_char):
          begin_system_call();
         {var uintB c;
          var reg1 int ergebnis = read(handle,&c,1); # Zeichen lesen versuchen
          if (ergebnis<0) { OS_error(); }
          end_system_call();
          if (ergebnis==0)
            # kein Zeichen verfügbar -> EOF erkennen
            { TheStream(stream)->strm_rd_ch_last = eof_value;
              return signean_minus;
            }
            else # Zeichen verfügbar
            { TheStream(stream)->strm_rd_ch_last = fixnum(c);
              return signean_null;
            }
        }}
      #else # defined(AMIGAOS)
      begin_system_call();
      if (!nullp(TheStream(stream)->strm_isatty))
        # interaktiv
        { if (WaitForChar(handle,1000L)) # 1/1000 sec auf ein Zeichen warten
            { end_system_call(); return signean_null; } # eins da
            else
            { end_system_call(); return signean_plus; } # keins da
        }
        else
        # nicht interaktiv
        { # ein Zeichen lesen versuchen (wie bei peek_char):
          var uintB c;
          var reg1 long ergebnis = Read(handle,&c,1L); # Zeichen lesen versuchen
          end_system_call();
          if (ergebnis<0) { OS_error(); }
          if (ergebnis==0)
            # kein Zeichen verfügbar -> EOF erkennen
            { TheStream(stream)->strm_rd_ch_last = eof_value;
              return signean_minus;
            }
            else # Zeichen verfügbar
            { TheStream(stream)->strm_rd_ch_last = fixnum(c);
              return signean_null;
            }
        }
      #endif
    }}

# UP: Löscht bereits eingegebenen interaktiven Input von einem Handle-Stream.
# clear_input_handle(stream);
# > stream: Handle-Stream
# < ergebnis: TRUE falls Input gelöscht wurde, FALSE sonst
  local boolean clear_input_handle (object stream);
  local boolean clear_input_handle(stream)
    var reg1 object stream;
    { var reg1 Handle handle = TheHandle(TheStream(stream)->strm_ihandle);
      if (nullp(TheStream(stream)->strm_isatty))
        # File -> nichts tun
        { return FALSE; }
      #if !defined(AMIGAOS)
      # Terminal
      TheStream(stream)->strm_rd_ch_last = NIL; # gewesenes EOF vergessen
      clear_tty_input(handle);
      # Für den Fall, das das nicht funktionierte:
      # Zeichen lesen, solange listen_handle() 0 liefert.
      pushSTACK(stream);
      while (listen_handle(STACK_0) == 0) { read_char(&STACK_0); }
      skipSTACK(1);
      return TRUE;
      #else # defined(AMIGAOS)
      # interaktiv
      { begin_system_call();
        loop
          { if (!WaitForChar(handle,1000L)) # 1/1000 sec auf ein Zeichen warten
              break; # keins mehr da -> fertig
           {var uintB c;
            var reg1 long ergebnis = Read(handle,&c,1L); # Zeichen lesen versuchen
            if (ergebnis<0) { OS_error(); }
          }}
        end_system_call();
        return TRUE;
      }
      #endif
    }

# WRITE-CHAR - Pseudofunktion für Handle-Streams:
  local void wr_ch_handle (object* stream_, object ch);
  local void wr_ch_handle(stream_,ch)
    var reg3 object* stream_;
    var reg1 object ch;
    { var reg2 Handle handle = TheHandle(TheStream(*stream_)->strm_ohandle);
      # ch sollte String-Char sein:
      if (!string_char_p(ch)) { fehler_string_char(*stream_,ch); }
     {var uintB c = char_code(ch); # Code des Zeichens
      restart_it:
      begin_system_call();
      {
       #if !defined(AMIGAOS)
       var reg4 int ergebnis = write(handle,&c,1); # Zeichen auszugeben versuchen
       end_system_call();
       if (ergebnis<0)
         { if (errno==EINTR) # Unterbrechung (durch Ctrl-C) ?
             { pushSTACK(S(write_char)); tast_break(); # Break-Schleife aufrufen
               goto restart_it;
             }
           OS_error(); # Error melden
         }
       #else # defined(AMIGAOS)
       var reg4 long ergebnis = Write(handle,&c,1L); # Zeichen auszugeben versuchen
       end_system_call();
       if (ergebnis<0) { OS_error(); } # Error melden
       #endif
       if (ergebnis==0) # nicht erfolgreich?
         { fehler_unwritable(S(write_char),*stream_); }
      }
    }}

#ifdef STRM_WR_SS
# WRITE-SIMPLE-STRING - Pseudofunktion für Handle-Streams:
  local void wr_ss_handle (object* stream_, object string, uintL start, uintL len);
  local void wr_ss_handle(stream_,string,start,len)
    var reg8 object* stream_;
    var reg5 object string;
    var reg6 uintL start;
    var reg9 uintL len;
    { if (len==0) return;
     {var reg7 object stream = *stream_;
      var reg4 Handle handle = TheHandle(TheStream(stream)->strm_ohandle);
      var reg1 uintB* ptr = &TheSstring(string)->data[start];
      var reg2 uintL remaining = len;
      restart_it:
      begin_system_call();
      loop
        {
         #if !defined(AMIGAOS)
         var reg3 int ergebnis = write(handle,ptr,remaining); # Zeichen auszugeben versuchen
         if (ergebnis<0)
           { if (errno==EINTR) # Unterbrechung (durch Ctrl-C) ?
               { end_system_call();
                 pushSTACK(S(write_string)); tast_break(); # Break-Schleife aufrufen
                 goto restart_it;
               }
             OS_error(); # Error melden
           }
         #else # defined(AMIGAOS)
         var reg3 long ergebnis = Write(handle,ptr,remaining); # Zeichen auszugeben versuchen
         if (ergebnis<0) { OS_error(); } # Error melden
         #endif
         if (ergebnis==0) # nicht erfolgreich?
           { fehler_unwritable(S(write_string),*stream_); }
         ptr += ergebnis; remaining -= ergebnis;
         if (remaining==0) break; # fertig?
        }
      end_system_call();
      wr_ss_lpos(stream,ptr,len); # Line-Position aktualisieren
    }}
#endif

# UP: Bringt den wartenden Output eines Handle-Stream ans Ziel.
# finish_output_handle(stream);
# > stream: Handle-Stream
# kann GC auslösen
  local void finish_output_handle (object stream);
  local void finish_output_handle(stream)
    var reg1 object stream;
    { finish_tty_output(TheHandle(TheStream(stream)->strm_ohandle)); }

# UP: Bringt den wartenden Output eines Handle-Stream ans Ziel.
# force_output_handle(stream);
# > stream: Handle-Stream
# kann GC auslösen
  local void force_output_handle (object stream);
  local void force_output_handle(stream)
    var reg1 object stream;
    { force_tty_output(TheHandle(TheStream(stream)->strm_ohandle)); }

# UP: Löscht den wartenden Output eines Handle-Stream.
# clear_output_handle(stream);
# > stream: Handle-Stream
# kann GC auslösen
  local void clear_output_handle (object stream);
  local void clear_output_handle(stream)
    var reg1 object stream;
    { clear_tty_output(TheHandle(TheStream(stream)->strm_ohandle)); }

#if defined(HANDLES) || defined(SOCKETS)

# READ-BYTE - Pseudofunktion für Handle-Streams:
  local object rd_by_handle (object stream);
  local object rd_by_handle(stream)
    var reg1 object stream;
    { pushSTACK(stream);
     {var reg1 object obj = read_char(&STACK_0);
      skipSTACK(1);
      if (!eq(obj,eof_value)) { obj = char_to_fixnum(obj); }
      return obj;
    }}

# WRITE-BYTE - Pseudofunktion für Handle-Streams:
  local void wr_by_handle (object stream, object obj);
  local void wr_by_handle(stream,obj)
    var reg1 object stream;
    var reg2 object obj;
    { # obj überprüfen:
      if (!integerp(obj)) { fehler_integer(stream,obj); }
      if (!(posfixnump(obj) && (posfixnum_to_L(obj) < char_code_limit)))
        { fehler_bad_integer(stream,obj); }
      pushSTACK(stream);
      wr_ch_handle(&STACK_0,fixnum_to_char(obj));
      skipSTACK(1);
    }

#endif

#if defined(HANDLES) || (defined(PIPES) && defined(UNIX)) || defined(SOCKETS)

# Schließt einen Handle-Stream.
# close_ihandle(stream);
# close_ohandle(stream);
# > stream : Handle-Stream
  local void close_ihandle (object stream);
  local void close_ohandle (object stream);
  local void close_ihandle(stream)
    var reg1 object stream;
    { var reg2 Handle handle = TheHandle(TheStream(stream)->strm_ihandle);
      begin_system_call();
      #if !defined(AMIGAOS)
      if (!( close(handle) ==0)) { OS_error(); }
      #else
      Close(handle);
      #endif
      end_system_call();
    }
  local void close_ohandle(stream)
    var reg1 object stream;
    { var reg2 Handle handle = TheHandle(TheStream(stream)->strm_ohandle);
      begin_system_call();
      #if !defined(AMIGAOS)
      if (!( close(handle) ==0)) { OS_error(); }
      #else
      Close(handle);
      #endif
      end_system_call();
    }

#endif

#if defined(HANDLES)

#define close_handle  close_ihandle

# UP: erzeugt ein File-Handle-Stream
# make_handle_stream(handle,direction)
# > handle: Handle des geöffneten Files
# > STACK_1: Filename, ein Pathname
# > STACK_0: Truename, ein Pathname
# > direction: Modus (0 = :PROBE, 1 = :INPUT, 2 = :OUTPUT, 3 = :IO)
# < ergebnis: File-Handle-Stream
# < STACK: aufgeräumt
# kann GC auslösen
  local object make_handle_stream (object handle, uintB direction);
  local object make_handle_stream(handle,direction)
    var reg4 object handle;
    var reg2 uintB direction;
    { # Flags:
      var reg3 uintB flags =
          ((direction & bit(0)) ? strmflags_rd_B : 0) # evtl. READ-CHAR, READ-BYTE erlaubt
        | ((direction & bit(1)) ? strmflags_wr_B : 0); # evtl. WRITE-CHAR, WRITE-BYTE erlaubt
      #if defined(FOREIGN_HANDLE) || !NIL_IS_CONSTANT
      pushSTACK(handle); # Handle retten
      #endif
     {# Stream allozieren:
      var reg1 object stream = allocate_stream(flags,strmtype_handle,strm_len+5);
      # und füllen:
      if (direction & bit(0))
        { TheStream(stream)->strm_rd_by = P(rd_by_handle);
          TheStream(stream)->strm_rd_ch = P(rd_ch_handle);
        }
        else
        { TheStream(stream)->strm_rd_by = P(rd_by_dummy);
          TheStream(stream)->strm_rd_ch = P(rd_ch_dummy);
        }
      if (direction & bit(1))
        { TheStream(stream)->strm_wr_by = P(wr_by_handle);
          TheStream(stream)->strm_wr_ch = P(wr_ch_handle);
          #ifdef STRM_WR_SS
          TheStream(stream)->strm_wr_ss = P(wr_ss_handle);
          #endif
        }
        else
        { TheStream(stream)->strm_wr_by = P(wr_by_dummy);
          TheStream(stream)->strm_wr_ch = P(wr_ch_dummy);
          #ifdef STRM_WR_SS
          TheStream(stream)->strm_wr_ss = P(wr_ss_dummy);
          #endif
        }
      TheStream(stream)->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
      #if defined(FOREIGN_HANDLE) || !NIL_IS_CONSTANT
      handle = popSTACK(); # Handle zurück
      #endif
      TheStream(stream)->strm_ihandle =
      TheStream(stream)->strm_ohandle = handle; # Handle eintragen
      # Flag isatty = (handle_tty ? T : NIL) bestimmen:
      begin_system_call();
      #if !defined(AMIGAOS)
      TheStream(stream)->strm_isatty = (isatty(TheHandle(handle)) ? T : NIL);
      #else # defined(AMIGAOS)
      TheStream(stream)->strm_isatty = (IsInteractive(TheHandle(handle)) ? T : NIL);
      #endif
      end_system_call();
      # File-Handle-Streams werden für Pathname-Zwecke wie File-Streams behandelt.
      # Daher ist (vgl. file_write_date) strm_file_handle == strm_ohandle,
      # und wir tragen nun die Pathnames ein:
      TheStream(stream)->strm_file_truename = popSTACK(); # Truename eintragen
      TheStream(stream)->strm_file_name = popSTACK(); # Filename eintragen
      # Liste der offenen Streams um stream erweitern:
      pushSTACK(stream);
      {var reg2 object new_cons = allocate_cons();
       Car(new_cons) = stream = popSTACK();
       Cdr(new_cons) = O(open_files);
       O(open_files) = new_cons;
      }
      return stream;
    }}

#endif

#endif # (UNIX || DJUNIX || EMUNIX || AMIGAOS || VMS) && (brauche Handle-Streams)


#ifdef KEYBOARD

# Keyboard-Stream
# ===============

# Funktionsweise:
# Liest ein Zeichen von Tastatur.
# Liefert ein Character mit Font=0 und folgenden Bits:
#   HYPER      falls Sondertaste.
#              Zu den Sondertasten zählen die Non-Standard-Tasten.
#              ATARI:
#                Funktionstasten, Cursorblock, Ziffernblock, Delete-Taste.
#              MSDOS:
#                Funktionstasten, Cursorblöcke, Ziffernblock.
#   CHAR-CODE  Bei normalen Tasten der Ascii-Code,
#              bei Sondertasten:
#              ATARI:
#                F1 -> #\F1, ..., F9 -> #\F9, F10 -> #\F10,
#                Help -> #\Help, Undo -> #\Undo,
#                Insert -> #\Insert, Delete -> #\Delete,
#                ClrHome -> #\Home,
#                 -> #\Up,  -> #\Down,  -> #\Left,  -> #\Right.
#              MSDOS:
#                F1 -> #\F1, ..., F10 -> #\F10, F11 -> #\F11, F12 -> #\F12,
#                Insert -> #\Insert, Delete -> #\Delete,
#                Home -> #\Home, End -> #\End, PgUp -> #\PgUp, PgDn -> #\PgDn,
#                Pfeiltasten -> #\Up, #\Down, #\Left, #\Right.
#   SUPER      falls mit Shift-Taste(n) gedrückt und sich ohne Shift
#              ein anderer Code ergeben hätte,
#   CONTROL    falls mit Control-Taste gedrückt,
#   META       falls mit Alternate-Taste gedrückt.

#ifdef UNIX
  # Zusätzliche Komponenten:
  #define strm_keyboard_isatty  strm_isatty   # Flag, ob stdin ein Terminal ist
  #define strm_keyboard_handle  strm_ihandle  # Handle für listen_handle()
  #define strm_keyboard_buffer  strm_other[2] # Liste der noch zu liefernden Zeichen
  #define strm_keyboard_keytab  strm_other[3] # Liste aller Tastenzuordnungen
                                              # jeweils (char1 ... charn . result)
  #define strm_keyboard_len  4
#else
  # Keine zusätzlichen Komponenten.
  #define strm_keyboard_len  0
#endif

#ifdef MSDOS

# Für Tastaturabfrage unter DOS:
#
# INT 16 documentation:
#   INT 16,00 - Wait for keystroke and read
#   INT 16,01 - Get keystroke status
#   INT 16,02 - Get shift status
#   INT 16,03 - Set keyboard typematic rate (AT+)
#   INT 16,04 - Keyboard click adjustment (AT+)
#   INT 16,05 - Keyboard buffer write  (AT,PS/2 enhanced keyboards)
#   INT 16,10 - Wait for keystroke and read  (AT,PS/2 enhanced keyboards)
#   INT 16,11 - Get keystroke status  (AT,PS/2 enhanced keyboards)
#   INT 16,12 - Get shift status  (AT,PS/2 enhanced keyboards)
#
# INT 16,00 - Wait for Keypress and Read Character
#     AH = 00
#     on return:
#     AH = keyboard scan code
#     AL = ASCII character or zero if special function key
#     - halts program until key with a scancode is pressed
#     - see  SCAN CODES
#
# INT 16,01 - Get Keyboard Status
#     AH = 01
#     on return:
#     ZF = 0 if a key pressed (even Ctrl-Break)
#     AX = 0 if no scan code is available
#     AH = scan code
#     AL = ASCII character or zero if special function key
#     - data code is not removed from buffer
#     - Ctrl-Break places a zero word in the keyboard buffer but does
#       register a keypress.
#
# INT 16,10 - Extended Wait for Keypress and Read Character  (AT+)
#     AH = 10h
#     on return:
#     AH = scan code
#     AL = ASCII character or zero if special function key
#     - available on AT and PS/2 machines with extended keyboard support
#     - similar to INT 16,00
#
# INT 16,11 - Extended Get Keyboard Status  (AT+)
#       AH = 11h
#       on return:
#       ZF = 0 if key pressed (data waiting)
#       AX = 0 if no scan code is available
#       AH = scan code
#       AL = ASCII character or zero if special function key
#       - available on AT and PS/2 machines with extended keyboard support
#       - data is not removed from buffer
#       - similar to INT 16,01
#

#ifdef DJUNIX

  # Liefert den nächsten Tastendruck incl. Scan-Code:
  # high byte = Scan-Code oder 0, low byte = Ascii-Code oder 0 oder 0xE0.
  local boolean kbhit()
    { var union REGS in;
      var union REGS out;
      in.h.ah = 0x11;
      int86(0x16,&in,&out);
      return ((out.x.flags & 0x40) == 0); # Zero-Flag abfragen
    }
  local uintW getch()
    { var union REGS in;
      var union REGS out;
      in.h.ah = 0x10;
      int86(0x16,&in,&out);
      return out.x.ax;
    }

#endif

#ifdef EMUNIX

  # Unter DOS:
  #   Bis emx 0.8e ist uns der INT 16,10 offenbar versperrt.
  #   Wir bekommen keine Extended-Keystrokes, können aber immerhin die Return-
  #   von der Enter-Taste unterscheiden.
  # Unter OS/2:
  #   INT 16 funktioniert nicht, dafür geht _read_kbd() präziser als unter DOS.

  # Liefert unter DOS den nächsten Tastendruck incl. Scan-Code:
  # high byte = Scan-Code oder 0, low byte = Ascii-Code oder 0 oder 0xE0.
  #ifdef EMUNIX_OLD_8e
    #define int16_wait "0x00"
    #define int16_stat "0x01"
  #else # EMUNIX_NEW_8f
    #define int16_wait "0x10"
    #define int16_stat "0x11"
  #endif
  local boolean kbhit()
    { var reg1 boolean result;
      __asm__ __volatile__ ("movb $"int16_stat",%%ah ; .byte 0xcd ; .byte 0x16 ; "
                            "movl $0,%%eax ; jz 1f ; incl %%eax ; 1: "
                            : "=a" /* %eax */ (result) /* OUT */
                            :                          /* IN */
                            : "bx","cx","dx","si","di" /* %ebx,%ecx,%edx,%esi,%edi */ /* CLOBBER */
                           );
      return result;
    }
  local uintW getch()
    { var reg1 uintW ch;
      __asm__ __volatile__ ("movb $"int16_wait",%%ah ; .byte 0xcd ; .byte 0x16"
                            : "=a" /* %ax */ (ch)      /* OUT */
                            :                          /* IN */
                            : "bx","cx","dx","si","di" /* %ebx,%ecx,%edx,%esi,%edi */ /* CLOBBER */
                           );
      return ch;
    }

#endif

  # Tabelle der Characters, die den Scan-Codes 0..166 (als Sondertasten)
  # entsprechen:
  local cint scancode_table [167] =
    { 0,
      ESC | char_meta_c, # 1 -> Alt-Escape
      '1' | char_control_c, # [2 = Ctrl-1 -> #\CONTROL-1]
      '2' | char_control_c, # 3 = Ctrl-2 -> #\CONTROL-2
      '3' | char_control_c, # [4 = Ctrl-3 -> #\CONTROL-3]
      '4' | char_control_c, # [5 = Ctrl-4 -> #\CONTROL-4]
      '5' | char_control_c, # [6 = Ctrl-5 -> #\CONTROL-5]
      '6' | char_control_c, # 7 = Ctrl-6 -> #\CONTROL-6
      '7' | char_control_c, # [8 = Ctrl-7 -> #\CONTROL-7]
      '8' | char_control_c, # [9 = Ctrl-8 -> #\CONTROL-8]
      '9' | char_control_c, # [10 = Ctrl-9 -> #\CONTROL-9]
      '0' | char_control_c, # [11 = Ctrl-0 -> #\CONTROL-0]
      '-' | char_meta_c, # [12 = Ctrl-- -> #\CONTROL-- # nicht international portabel]
      '=' | char_meta_c, # [13 = Ctrl-= -> #\CONTROL-= # nicht international portabel]
       BS | char_meta_c, # 14 -> Alt-Backspace
        9 | char_super_c, # 15 -> Shift-Tab
      'Q' | char_meta_c, # 16 -> Alt-Q
      'W' | char_meta_c, # 17 -> Alt-W
      'E' | char_meta_c, # 18 -> Alt-E
      'R' | char_meta_c, # 19 -> Alt-R
      'T' | char_meta_c, # 20 -> Alt-T
      'Y' | char_meta_c, # 21 -> Alt-Y
      'U' | char_meta_c, # 22 -> Alt-U
      'I' | char_meta_c, # 23 -> Alt-I
      'O' | char_meta_c, # 24 -> Alt-O
      'P' | char_meta_c, # 25 -> Alt-P
      '[' | char_meta_c, # 26 -> Alt-[ # nicht international portabel
      ']' | char_meta_c, # 27 -> Alt-] # nicht international portabel
       CR | char_meta_c, # 28 = Alt-Return -> #\META-Return
      0,
      'A' | char_meta_c, # 30 -> Alt-A
      'S' | char_meta_c, # 31 -> Alt-S
      'D' | char_meta_c, # 32 -> Alt-D
      'F' | char_meta_c, # 33 -> Alt-F
      'G' | char_meta_c, # 34 -> Alt-G
      'H' | char_meta_c, # 35 -> Alt-H
      'J' | char_meta_c, # 36 -> Alt-J
      'K' | char_meta_c, # 37 -> Alt-K
      'L' | char_meta_c, # 38 -> Alt-L oder Alt-\ ??
      ';' | char_meta_c, # 39 -> Alt-; # nicht international portabel
      '\''| char_meta_c, # 40 -> Alt-' # nicht international portabel
      '`' | char_meta_c, # 41 -> Alt-` # nicht international portabel
      0,
      '\\'| char_meta_c, # 43 -> Alt-\ # nicht international portabel
      'Z' | char_meta_c, # 44 -> Alt-Z
      'X' | char_meta_c, # 45 -> Alt-X
      'C' | char_meta_c, # 46 -> Alt-C
      'V' | char_meta_c, # 47 -> Alt-V
      'B' | char_meta_c, # 48 -> Alt-B
      'N' | char_meta_c, # 49 -> Alt-N
      'M' | char_meta_c, # 50 -> Alt-M
      ',' | char_meta_c, # 51 = Alt-, -> #\META-',' # nicht international portabel
      '.' | char_meta_c, # 52 = Alt-. -> #\META-'.' # nicht international portabel
      '/' | char_meta_c, # 53 = Alt-/ -> #\META-'/' # nicht international portabel
      0,
      '*' | char_meta_c | char_hyper_c, # 55 = Alt-* -> #\META-HYPER-'*'
      0,
      ' ' | char_meta_c, # 57 = Alt-Space -> #\META-Space
      0,
      'A' | char_hyper_c, #  59 = F1 -> #\F1 = #\HYPER-A
      'B' | char_hyper_c, #  60 = F2 -> #\F2 = #\HYPER-B
      'C' | char_hyper_c, #  61 = F3 -> #\F3 = #\HYPER-C
      'D' | char_hyper_c, #  62 = F4 -> #\F4 = #\HYPER-D
      'E' | char_hyper_c, #  63 = F5 -> #\F5 = #\HYPER-E
      'F' | char_hyper_c, #  64 = F6 -> #\F6 = #\HYPER-F
      'G' | char_hyper_c, #  65 = F7 -> #\F7 = #\HYPER-G
      'H' | char_hyper_c, #  66 = F8 -> #\F8 = #\HYPER-H
      'I' | char_hyper_c, #  67 = F9 -> #\F9 = #\HYPER-I
      'J' | char_hyper_c, #  68 = F10 -> #\F10 = #\HYPER-J
      'K' | char_hyper_c, # [69 = F11 -> #\F11 = #\HYPER-K]
      'L' | char_hyper_c, # [70 = F12 -> #\F12 = #\HYPER-L]
       23 | char_hyper_c, #  71 = Home -> #\Home = #\HYPER-Code23
       24 | char_hyper_c, #  72 = Up -> #\Up = #\HYPER-Code24
       25 | char_hyper_c, #  73 = PgUp -> #\PgUp = #\HYPER-Code25
      '-' | char_meta_c | char_hyper_c, #  74 = Alt-- -> #\META-HYPER--
       20 | char_hyper_c, #  75 = Left -> #\Left = #\HYPER-Code20
       21 | char_hyper_c, # [76 -> #\HYPER-Code21]
       22 | char_hyper_c, #  77 = Right -> #\Right = #\HYPER-Code22
      '+' | char_meta_c | char_hyper_c, #  78 = Alt-+ -> #\META-HYPER-+
       17 | char_hyper_c, #  79 = End -> #\End = #\HYPER-Code17
       18 | char_hyper_c, #  80 = Down -> #\Down = #\HYPER-Code18
       19 | char_hyper_c, #  81 = PgDn -> #\PgDn = #\HYPER-Code19
       16 | char_hyper_c, #  82 = Insert -> #\Insert = #\HYPER-Code16
      127 | char_hyper_c, #  83 = Delete -> #\Delete = #\HYPER-Code127
      'A' | char_super_c | char_hyper_c, #  84 = Shift-F1 -> #\S-F1 = #\SUPER-HYPER-A
      'B' | char_super_c | char_hyper_c, #  85 = Shift-F2 -> #\S-F2 = #\SUPER-HYPER-B
      'C' | char_super_c | char_hyper_c, #  86 = Shift-F3 -> #\S-F3 = #\SUPER-HYPER-C
      'D' | char_super_c | char_hyper_c, #  87 = Shift-F4 -> #\S-F4 = #\SUPER-HYPER-D
      'E' | char_super_c | char_hyper_c, #  88 = Shift-F5 -> #\S-F5 = #\SUPER-HYPER-E
      'F' | char_super_c | char_hyper_c, #  89 = Shift-F6 -> #\S-F6 = #\SUPER-HYPER-F
      'G' | char_super_c | char_hyper_c, #  90 = Shift-F7 -> #\S-F7 = #\SUPER-HYPER-G
      'H' | char_super_c | char_hyper_c, #  91 = Shift-F8 -> #\S-F8 = #\SUPER-HYPER-H
      'I' | char_super_c | char_hyper_c, #  92 = Shift-F9 -> #\S-F9 = #\SUPER-HYPER-I
      'J' | char_super_c | char_hyper_c, #  93 = Shift-F10 -> #\S-F10 = #\SUPER-HYPER-J
      'A' | char_control_c | char_hyper_c, #  94 = Control-F1 -> #\C-F1 = #\CONTROL-HYPER-A
      'B' | char_control_c | char_hyper_c, #  95 = Control-F2 -> #\C-F2 = #\CONTROL-HYPER-B
      'C' | char_control_c | char_hyper_c, #  96 = Control-F3 -> #\C-F3 = #\CONTROL-HYPER-C
      'D' | char_control_c | char_hyper_c, #  97 = Control-F4 -> #\C-F4 = #\CONTROL-HYPER-D
      'E' | char_control_c | char_hyper_c, #  98 = Control-F5 -> #\C-F5 = #\CONTROL-HYPER-E
      'F' | char_control_c | char_hyper_c, #  99 = Control-F6 -> #\C-F6 = #\CONTROL-HYPER-F
      'G' | char_control_c | char_hyper_c, #  100 = Control-F7 -> #\C-F7 = #\CONTROL-HYPER-G
      'H' | char_control_c | char_hyper_c, #  101 = Control-F8 -> #\C-F8 = #\CONTROL-HYPER-H
      'I' | char_control_c | char_hyper_c, #  102 = Control-F9 -> #\C-F9 = #\CONTROL-HYPER-I
      'J' | char_control_c | char_hyper_c, #  103 = Control-F10 -> #\C-F10 = #\CONTROL-HYPER-J
      'A' | char_meta_c | char_hyper_c, #  104 = Alt-F1 -> #\M-F1 = #\META-HYPER-A
      'B' | char_meta_c | char_hyper_c, #  105 = Alt-F2 -> #\M-F2 = #\META-HYPER-B
      'C' | char_meta_c | char_hyper_c, #  106 = Alt-F3 -> #\M-F3 = #\META-HYPER-C
      'D' | char_meta_c | char_hyper_c, #  107 = Alt-F4 -> #\M-F4 = #\META-HYPER-D
      'E' | char_meta_c | char_hyper_c, #  108 = Alt-F5 -> #\M-F5 = #\META-HYPER-E
      'F' | char_meta_c | char_hyper_c, #  109 = Alt-F6 -> #\M-F6 = #\META-HYPER-F
      'G' | char_meta_c | char_hyper_c, #  110 = Alt-F7 -> #\M-F7 = #\META-HYPER-G
      'H' | char_meta_c | char_hyper_c, #  111 = Alt-F8 -> #\M-F8 = #\META-HYPER-H
      'I' | char_meta_c | char_hyper_c, #  112 = Alt-F9 -> #\M-F9 = #\META-HYPER-I
      'J' | char_meta_c | char_hyper_c, #  113 = Alt-F10 -> #\M-F10 = #\META-HYPER-J
       29 | char_control_c | char_hyper_c, # 114 = Control-PrtScr -> #\CONTROL-HYPER-Code29
       20 | char_control_c | char_hyper_c, # 115 = Control-Left -> #\C-Left = #\CONTROL-HYPER-Code20
       22 | char_control_c | char_hyper_c, # 116 = Control-Right -> #\C-Right = #\CONTROL-HYPER-Code22
       17 | char_control_c | char_hyper_c, # 117 = Control-End -> #\C-End = #\CONTROL-HYPER-Code17
       19 | char_control_c | char_hyper_c, # 118 = Control-PgDn -> #\C-PgDn = #\CONTROL-HYPER-Code19
       23 | char_control_c | char_hyper_c, # 119 = Control-Home -> #\C-Home = #\CONTROL-HYPER-Code23
      '1' | char_meta_c, #  120 = Alt-1 -> #\META-1
      '2' | char_meta_c, #  121 = Alt-2 -> #\META-2
      '3' | char_meta_c, #  122 = Alt-3 -> #\META-3
      '4' | char_meta_c, #  123 = Alt-4 -> #\META-4
      '5' | char_meta_c, #  124 = Alt-5 -> #\META-5
      '6' | char_meta_c, #  125 = Alt-6 -> #\META-6
      '7' | char_meta_c, #  126 = Alt-7 -> #\META-7
      '8' | char_meta_c, #  127 = Alt-8 -> #\META-8
      '9' | char_meta_c, #  128 = Alt-9 -> #\META-9
      '0' | char_meta_c, #  129 = Alt-0 -> #\META-0
      '-' | char_meta_c, #  130 = Alt-- -> #\META-- # nicht international portabel
      '=' | char_meta_c, #  131 = Alt-= -> #\META-= # nicht international portabel
       25 | char_control_c | char_hyper_c, # 132 = Control-PgUp -> #\C-PgUp = #\CONTROL-HYPER-Code25
      'K' | char_hyper_c, #  133 = F11 -> #\F11 = #\HYPER-K
      'L' | char_hyper_c, #  134 = F12 -> #\F12 = #\HYPER-L
      'K' | char_super_c | char_hyper_c, #  135 = Shift-F11 -> #\S-F11 = #\SUPER-HYPER-K
      'L' | char_super_c | char_hyper_c, #  136 = Shift-F12 -> #\S-F12 = #\SUPER-HYPER-L
      'K' | char_control_c | char_hyper_c, #  137 = Control-F11 -> #\C-F11 = #\CONTROL-HYPER-K
      'L' | char_control_c | char_hyper_c, #  138 = Control-F12 -> #\C-F12 = #\CONTROL-HYPER-L
      'K' | char_meta_c | char_hyper_c, #  139 = Alt-F1 -> #\M-F11 = #\META-HYPER-K
      'L' | char_meta_c | char_hyper_c, #  140 = Alt-F2 -> #\M-F12 = #\META-HYPER-L
       24 | char_control_c | char_hyper_c, # 141 = Control-Up -> #\C-Up = #\CONTROL-HYPER-Code24
      '-' | char_control_c | char_hyper_c, # 142 = Control-- -> #\CONTROL-HYPER--
       21 | char_control_c | char_hyper_c, # 143 = Control-Keypad5 -> #\CONTROL-HYPER-Code21
      '+' | char_control_c | char_hyper_c, # 142 = Control-+ -> #\CONTROL-HYPER-+
       18 | char_control_c | char_hyper_c, # 145 = Control-Down -> #\C-Down = #\CONTROL-HYPER-Code18
       16 | char_control_c | char_hyper_c, # 146 = Control-Insert -> #\C-Insert = #\CONTROL-HYPER-Code16
      127 | char_control_c | char_hyper_c, # 147 = Control-Delete -> #\CONTROL-HYPER-Delete
        9 | char_control_c, # 148 = Control-Tab -> #\CONTROL-Tab
      '/' | char_control_c | char_hyper_c, # 149 = Control-/ -> #\CONTROL-HYPER-'/'
      '*' | char_control_c | char_hyper_c, # 150 = Control-* -> #\CONTROL-HYPER-'*'
       23 | char_meta_c | char_hyper_c, # 151 = Alt-Home -> #\M-Home = #\META-HYPER-Code23
       24 | char_meta_c | char_hyper_c, # 152 = Alt-Up -> #\M-Up = #\META-HYPER-Code24
       25 | char_meta_c | char_hyper_c, # 153 = Alt-PgUp -> #\M-PgUp = #\META-HYPER-Code25
      0,
       20 | char_meta_c | char_hyper_c, # 155 = Alt-Left -> #\M-Left = #\META-HYPER-Code20
       21 | char_meta_c | char_hyper_c, # [156 -> #\META-HYPER-Code21]
       22 | char_meta_c | char_hyper_c, # 157 = Alt-Right -> #\M-Right = #\META-HYPER-Code22
      0,
       17 | char_meta_c | char_hyper_c, # 159 = Alt-End -> #\M-End = #\META-HYPER-Code17
       18 | char_meta_c | char_hyper_c, # 160 = Alt-Down -> #\M-Down = #\META-HYPER-Code18
       19 | char_meta_c | char_hyper_c, # 161 = Alt-PgDn -> #\M-PgDn = #\META-HYPER-Code19
       16 | char_meta_c | char_hyper_c, # 162 = Alt-Insert -> #\M-Insert = #\META-HYPER-Code16
      127 | char_meta_c | char_hyper_c, # 163 = Alt-Delete -> #\META-HYPER-Delete
      '/' | char_meta_c | char_hyper_c, # 164 = Alt-/ -> #\META-HYPER-'/'
        9 | char_meta_c, # 165 = Alt-Tab -> #\META-Tab
       CR | char_meta_c | char_hyper_c, # 166 = Alt-Enter -> #\META-HYPER-Return
    };

#ifdef EMUNIX_PORTABEL

# Wir haben, um portabel zu bleiben, nur die Funktion _read_kbd zur Verfügung.
# Diese erkennt unter DOS aber nur recht wenige Sondertasten: nur die mit
# Scan-Codes 3, 7, 15-25, 30-38, 44-50, 59-131 (ungefähr).
# Insbesondere fehlen F11, F12, Ctrl-Up, Ctrl-Down, und man kann
# Enter von Return, Tab von Ctrl-I, Backspace von Ctrl-H nicht unterscheiden.
# Trotzdem!
#ifdef EMUNIX_NEW_8f
# Da INT 16,10 unter DOS nun endlich befriedigend funktioniert, verwenden wir
# dieses. Zur Laufzeit wird _osmode abgefragt.
#endif

#endif

#endif

# Stellt fest, ob der Keyboard-Stream ein Zeichen verfügbar hat.
# listen_keyboard(stream)
# > stream: Stream
# < ergebnis:  0 falls Zeichen verfügbar,
#             -1 falls bei EOF angelangt,
#             +1 falls kein Zeichen verfügbar, aber nicht wegen EOF
  local signean listen_keyboard (object stream);
  #if defined(ATARI) || defined(MSDOS)
  local signean listen_keyboard(stream)
    var reg1 object stream;
    {
      #ifdef ATARI
        if (GEMDOS_ConStat()==0) # inzwischen wieder Tasten gedrückt?
          { return signean_plus; } # nein
          else
          { return signean_null; } # ja
      #endif
      #ifdef MSDOS
        #ifdef EMUNIX_PORTABEL
        if (!(_osmode == DOS_MODE))
          # OS/2
          { var reg2 int ch = _read_kbd(FALSE,FALSE,FALSE);
            if (ch < 0) { return signean_plus; } # nein
           {var reg2 cint c =
              (ch==0 ? scancode_table[(uintB)_read_kbd(FALSE,TRUE,FALSE)]
                     : (ch <= 26) && !(ch == BS) && !(ch == CR) && !(ch == TAB)
                       ? # Ctrl-A bis Ctrl-Z -> Buchstabe mit CONTROL-Bit draus machen:
                         ((cint)(ch==LF ? CR : (ch | bit(6))) << char_code_shift_c) | char_control_c
                       : (cint)(uintB)ch << char_code_shift_c
              );
            /* asciz_out("{"); hex_out(ch); asciz_out("}"); _sleep2(500); */ # Test
            TheStream(stream)->strm_rd_ch_last = char_to_fixnum(int_char(c));
            return signean_null;
          }}
          else
        #endif
        # DOS
        if (kbhit()) # inzwischen wieder Tasten gedrückt?
          { return signean_null; } # ja
          else
          { return signean_plus; } # nein
      #endif
    }
  #endif
  #ifdef UNIX
    #define listen_keyboard  listen_handle
  #endif

# UP: Löscht bereits eingegebenen interaktiven Input vom Keyboard-Stream.
# clear_input_keyboard(stream);
# > stream: Stream
# < ergebnis: TRUE falls Input gelöscht wurde, FALSE sonst
  local boolean clear_input_keyboard (object stream);
  local boolean clear_input_keyboard(stream)
    var reg1 object stream;
    {
      #ifdef ATARI
        # GEMDOS-Tastaturbuffer leeren:
        loop
          { if (GEMDOS_ConStat()==0) break; # Buffer leer -> fertig
            GEMDOS_DirConIn(); # sonst: Zeichen verbrauchen
          }
      #endif
      #ifdef MSDOS
        #ifdef EMUNIX_PORTABEL
        if (!(_osmode == DOS_MODE))
          # OS/2
           { while (listen_keyboard(stream)) { /* das Zeichen wurde schon geholt! */ } }
          else
        #endif
        # DOS
        while (kbhit()) { getch(); }
      #endif
      #ifdef UNIX
        if (nullp(TheStream(stream)->strm_keyboard_isatty))
          # File -> nichts tun
          { return FALSE; }
        # Terminal
        TheStream(stream)->strm_rd_ch_last = NIL; # gewesenes EOF vergessen
        clear_tty_input(stdin_handle);
        pushSTACK(stream);
        while (listen_keyboard(STACK_0) == 0) { read_char(&STACK_0); }
        skipSTACK(1);
      #endif
      return TRUE;
    }

# Lesen eines Zeichens vom Keyboard:
  local object rd_ch_keyboard (object* stream_);
  #ifdef ATARI
  #
  # Um einerseits bei normalen Tasten feststellen zu können, ob sie mit
  # und ohne Shift dasselbe ergeben hätte, und um andererseits von nationalen
  # Spezialitäten wie 'ä', 'ö', 'ü', Shift-ü = @ usw. unabhängig zu sein,
  # lassen wir uns vom BIOS den Ascii-Code geben, entscheiden aber hinterher
  # aufgrund der BIOS-Tastaturtabellen, ob wir ihn verwenden wollen oder nicht.
  #
  # Zeiger auf die drei Tastaturtabellen des BIOS (Länge 128, Index: Scancode,
  # Wert: Ascii-Code), werden beim Programmstart initialisiert:
    local KEYTAB keytables;
  #
  local object rd_ch_keyboard(stream_)
    var reg7 object* stream_;
    { var reg6 object ch;
      run_time_stop(); # Run-Time-Stoppuhr anhalten
      restart_it:
     {# Tastendruck abwarten, nichts ausgeben:
      var reg5 uintL erg = GEMDOS_DirConIn();
      # Wegen der Initialisierung in SPVW.Q ist
      # in (erg>>24) der Sondertasten-Status enthalten.
      { var reg4 uintB code = (uintB)erg; # Ascii-Code
        var reg3 uintB scancode = (uintB)(erg>>16); # Scan-Code
        var reg2 uintB kbshift = (uintB)(erg>>24); # Sondertasten-Status
        var reg1 cint c = 0; # neues Character
        # Eine kleine Merkwürdigkeit beheben:
        #   Beim Drücken von [Shift-]Ctrl-Alt-Insert bzw. -ClrHome liefert
        #   das BIOS zwei Tastenanschläge (und doppelten Tastaturclick):
        #   Ctrl-Alt-Insert:  0x0C520000, 0x0CD20000,
        #   Ctrl-Alt-ClrHome: 0x0C770000, 0x0CC70000,
        #   und zwischen diesen beiden Anschlägen wird die linke bzw.
        #   rechte Maustaste als gedrückt betrachtet (z.B. BIOS_KBSHIFT,
        #   liefert gesetztes Bit 6 bzw. Bit 5).
        #   0xD2 und 0xC7 sind natürlich irrsinnige Scan-Codes. Sie ent-
        #   stehen offenbar als 82+128 bzw. 71+128.
        # Wir behandeln das Problem so, daß wir den zweiten Tastenanschlag
        # dann ignorieren, wenn er kommt.
        if (scancode >= 59+128) # Scancode einer Sondertaste, um 128 erhöht?
          goto restart_it; # ja -> diesen Tastenanschlag übergehen
          # (Eigentlich müßte deswegen auch listen_keyboard geändert werden.)
       {var reg8 uintB* keytables_normal = keytables.unshift;
        # Scan-Code aufbereiten und auf Sondertaste entscheiden:
        if ((scancode >= 120) && (scancode <= 131))
          { scancode = scancode-118; } # 120->2,121->3,... 'Alt i' -> 'i'
        if ((scancode >= 59) && !(scancode == 96)) # Scancode <59 oder =96 -> normale Taste
          # Sondertaste
          { if ((scancode < 96) && (scancode >= 84))
              { scancode = scancode-25; } # 84->59,85->60,... 'Shift Fi' -> 'Fi'
            switch (scancode)
              { case 115: scancode = 75; break; # 115->75 'Ctrl-' -> ''
                case 116: scancode = 77; break; # 116->77 'Ctrl-' -> ''
                case 117: scancode = 79; break; # 117->79 'Ctrl-End' -> 'End'
                case 118: scancode = 81; break; # 118->81 'Ctrl-PgDn' -> 'PgDn'
                case 119: scancode = 71; break; # 119->71 'Ctrl-Home' -> 'Home'
                case 132: scancode = 73; break; # 132->73 'Ctrl-PgUp' -> 'PgUp'
                default: ;
              }
            c |= char_hyper_c; # HYPER-Bit setzen
          }
        # Ascii-Code übernehmen bzw. neu berechnen:
        if (kbshift & (CapsLockKey_mask | CtrlKey_mask | AltKey_mask))
          # Caps-Lock eingeschaltet oder Control- oder Alternate-Taste gedrückt
          { keytables_normal = keytables.capslock; }
        if ((code >= 32) && ((scancode < 71) || (scancode >= 84)))
          # Ascii-Code übernehmen, evtl. Shift- oder Alt-Bit killen:
          { if (kbshift & BothShiftKey_mask)
              # Shift gedrückt
              { if (keytables.shift[scancode] == code)
                  # Ascii-Code kam aus der Tabelle 'shift'
                  { if (keytables_normal[scancode] == code)
                      # Ascii-Code kam auch aus der Tabelle 'unshift'/'capslock'
                      { c |= char_super_c; } # 'Shift' war unnötig -> SUPER-Bit setzen
                      else
                      {} # 'Shift' war nötig, 'Shift' killen
                  }
                  else
                  # Ascii-Code kam wohl durch 'Alt' zustande, 'Alt' und 'Shift' killen
                  { kbshift &= ~AltKey_mask; }
              }
              else
              # Shift nicht gedrückt
              { if (keytables_normal[scancode] == code)
                  {} # Ascii-Code kam aus der Tabelle 'unshift'/'capslock'
                  else
                  # Ascii-Code kam wohl durch 'Alt' zustande, 'Alt' killen
                  { kbshift &= ~AltKey_mask; }
              }
          }
          else
          # Ascii-Code <32 (oder >=32, aber vom Cursorblock),
          # das erkennen wir nicht an.
          { if (kbshift & BothShiftKey_mask)
              # Shift gedrückt
              { # Ascii-Code nicht (immer) aus der Tabelle 'shift' nehmen, denn
                # die enthält Ziffern 024678 bei Tasten aus dem Cursorblock!
                code = keytables.capslock[scancode]; # Ascii-Code aus der Tabelle 'capslock'
                c |= char_super_c; # SUPER-Bit setzen
              }
              else
              # Shift nicht gedrückt
              { code = keytables_normal[scancode]; } # Ascii-Code aus der Tabelle 'unshift'/'capslock'
          }
        if (code==0)
          # setze den Scan-Code in eine Sondertaste um:
          { if ((scancode >= 59) && (scancode < 99))
              { # Tabelle der Characters, die den Scan-Codes 59..98 entsprechen:
                local uintB table [99-59] =
                  {
                    'A', #  59 = F1 -> #\F1 = #\HYPER-A
                    'B', #  60 = F2 -> #\F2 = #\HYPER-B
                    'C', #  61 = F3 -> #\F3 = #\HYPER-C
                    'D', #  62 = F4 -> #\F4 = #\HYPER-D
                    'E', #  63 = F5 -> #\F5 = #\HYPER-E
                    'F', #  64 = F6 -> #\F6 = #\HYPER-F
                    'G', #  65 = F7 -> #\F7 = #\HYPER-G
                    'H', #  66 = F8 -> #\F8 = #\HYPER-H
                    'I', #  67 = F9 -> #\F9 = #\HYPER-I
                    'J', #  68 = F10 -> #\F10 = #\HYPER-J
                    'K', # [69 = F11 -> #\F11 = #\HYPER-K]
                    'L', # [70 = F12 -> #\F12 = #\HYPER-L]
                     23, #  71 = ClrHome -> #\Home = #\HYPER-Code23
                     24, #  72 =  -> #\Up = #\HYPER-Code24
                     25, # [73 = PgUp -> #\PgUp = #\HYPER-Code25]
                    '-', # [74 = - -> #\HYPER--]
                     20, #  75 =  -> #\Left = #\HYPER-Code20
                      0,
                     22, #  77 =  -> #\Right = #\HYPER-Code22
                    '+', # [78 = + -> #\HYPER-+]
                     17, # [79 = End -> #\End = #\HYPER-Code17]
                     18, #  80 =  -> #\Down = #\HYPER-Code18
                     19, # [81 = PgDn -> #\PgDn = #\HYPER-Code19]
                     16, #  82 = Insert -> #\Insert = #\HYPER-Code16
                    127, # [83 = Delete -> #\Delete = #\HYPER-Code127]
                    'A', # [84 = Shift F1 -> #\SUPER-F1 = #\SUPER-HYPER-A]
                    'B', # [85 = Shift F2 -> #\SUPER-F2 = #\SUPER-HYPER-B]
                    'C', # [86 = Shift F3 -> #\SUPER-F3 = #\SUPER-HYPER-C]
                    'D', # [87 = Shift F4 -> #\SUPER-F4 = #\SUPER-HYPER-D]
                    'E', # [88 = Shift F5 -> #\SUPER-F5 = #\SUPER-HYPER-E]
                    'F', # [89 = Shift F6 -> #\SUPER-F6 = #\SUPER-HYPER-F]
                    'G', # [90 = Shift F7 -> #\SUPER-F7 = #\SUPER-HYPER-G]
                    'H', # [91 = Shift F8 -> #\SUPER-F8 = #\SUPER-HYPER-H]
                    'I', # [92 = Shift F9 -> #\SUPER-F9 = #\SUPER-HYPER-I]
                    'J', # [93 = Shift F10 -> #\SUPER-F10 = #\SUPER-HYPER-J]
                    'K', # [94 = Shift F11 -> #\SUPER-F11 = #\SUPER-HYPER-K]
                    'L', # [95 = Shift F12 -> #\SUPER-F12 = #\SUPER-HYPER-L]
                      0,
                     29, #  97 = Undo -> #\Undo = #\HYPER-Code29
                     28, #  98 = Help -> #\Help = #\HYPER-Code28
                  };
                code = table[scancode-59];
          }   }
        # Ascii-Code fertig
        if (kbshift & CtrlKey_mask) # Control-Taste gedrückt?
          { c |= char_control_c; } # ja -> CONTROL-Bit setzen
        if (kbshift & AltKey_mask) # Alternate-Taste gedrückt?
          { c |= char_meta_c; } # ja -> META-Bit setzen
        c |= ((cint)code << char_code_shift_c);
        ch = int_char(c);
     }}}
      run_time_restart(); # Run-Time-Stoppuhr weiterlaufen lassen
      return ch;
    }
  #endif

  #ifdef MSDOS
  local object rd_ch_keyboard(stream_)
    var reg7 object* stream_;
    {
      #ifdef EMUNIX_PORTABEL
      if (!(_osmode == DOS_MODE))
        # OS/2
        { run_time_stop(); # Run-Time-Stoppuhr anhalten
         {var reg1 int ch = _read_kbd(FALSE,TRUE,FALSE);
          var reg2 cint c =
            (ch==0 ? scancode_table[(uintB)_read_kbd(FALSE,TRUE,FALSE)]
                   : (ch <= 26) && !(ch == BS) && !(ch == CR) && !(ch == TAB)
                     ? # Ctrl-A bis Ctrl-Z -> Buchstabe mit CONTROL-Bit draus machen:
                       ((cint)(ch==LF ? CR : (ch | bit(6))) << char_code_shift_c) | char_control_c
                     : (cint)(uintB)ch << char_code_shift_c
            );
          # noch zu behandeln: ??
          # Ctrl-2 -> #\Control-2, Ctrl-6 -> #\Code30, Ctrl-ß -> #\Code28,
          # Ctrl-+ -> #\Code29, Ctrl-ü -> #\Code27 = #\Escape
          /* asciz_out("{"); hex_out(ch); asciz_out("}"); _sleep2(500); */ # Test
          run_time_restart(); # Run-Time-Stoppuhr weiterlaufen lassen
          return int_char(c);
        }}
        else
      #endif
      # DOS
     {var reg6 object ch;
      run_time_stop(); # Run-Time-Stoppuhr anhalten
      { # Tastendruck abwarten, nichts ausgeben:
        var reg5 uintW erg = getch();
        var reg4 uintB code = (uintB)erg; # Ascii-Code
        var reg3 uintB scancode = (uintB)(erg>>8); # Scan-Code
        var reg1 cint c = 0; # neues Character
        if (scancode == 0)
          # Multikey-Event, z.B. accent+space oder Alt xyz
          { c = (cint)code << char_code_shift_c; }
        else
          { if ((code == 0) || (code == 0xE0))
              # Sondertaste
              { c = (scancode < 167 ? scancode_table[scancode] : 0); }
              else
              { if (((scancode >= 71) && (scancode < 84)) || (scancode == 55)
                    || ((scancode == 0xE0) && (code >= 32))
                   )
                  # Ziffernblocktaste außer Enter (auch nicht F1 bis F12 !)
                  { c = ((cint)code << char_code_shift_c) | char_hyper_c; }
                elif ((scancode == 14) || (scancode == 28)
                      || ((scancode == 0xE0) && (code < 32))
                     )
                  # Backspace-Taste, Return-Taste, Enter-Taste
                  { var reg5 uintB defaultcode = (scancode==14 ? BS : CR);
                    c = (cint)defaultcode << char_code_shift_c;
                    if (scancode == 0xE0) { c |= char_hyper_c; }
                    if (!(code == defaultcode)) { c |= char_control_c; }
                  }
                else
                  { if ((code < 32) && ((scancode >= 16) && (scancode <= 53)))
                      # Ctrl-A bis Ctrl-Z -> Buchstabe mit CONTROL-Bit draus machen:
                      { c = ((cint)(code | bit(6)) << char_code_shift_c) | char_control_c; }
                    else
                      # normales Zeichen
                      { c = (cint)code << char_code_shift_c; }
          }   }   }
        # noch zu behandeln: ??
        # Ctrl-2          0300
        # Ctrl-6          071E
        # Ctrl-ß          0C1C
        # Ctrl--          0C1F
        ch = int_char(c);
      }
      run_time_restart(); # Run-Time-Stoppuhr weiterlaufen lassen
      return ch;
    }}
  #endif

  #ifdef UNIX

  # vgl. rd_ch_handle() :
  local object rd_ch_keyboard(stream_)
    var reg3 object* stream_;
    { restart_it:
     {var reg2 object stream = *stream_;
      if (eq(TheStream(stream)->strm_rd_ch_last,eof_value)) # schon EOF?
        { return eof_value; }
      # Noch etwas im Buffer?
      if (mconsp(TheStream(stream)->strm_keyboard_buffer))
        goto empty_buffer;
      # Ein Zeichen lesen:
      read_next_char:
      {var uintB c;
       run_time_stop(); # Run-Time-Stoppuhr anhalten
       begin_system_call();
       {var reg1 int ergebnis = read(stdin_handle,&c,1); # Zeichen lesen versuchen
        end_system_call();
        run_time_restart(); # Run-Time-Stoppuhr weiterlaufen lassen
        if (ergebnis<0)
          { if (errno==EINTR) # Unterbrechung (durch Ctrl-C) ?
              { pushSTACK(S(read_char)); tast_break(); # Break-Schleife aufrufen
                goto restart_it;
              }
            OS_error();
          }
        if (ergebnis==0)
          # kein Zeichen verfügbar -> EOF erkennen
          { TheStream(stream)->strm_rd_ch_last = eof_value; return eof_value; }
       }
       # Es verlängert den Buffer:
       {var reg4 object new_cons = allocate_cons();
        Car(new_cons) = code_char(c);
        stream = *stream_;
        {var reg1 object* last_ = &TheStream(stream)->strm_keyboard_buffer;
         while (mconsp(*last_)) { last_ = &Cdr(*last_); }
         *last_ = new_cons;
      }}}
      # Ist der Buffer eine vollständige Zeichenfolge zu einer Taste,
      # so liefern wir diese Taste. Ist der Buffer ein echtes Anfangsstück
      # einer Zeichenfolge zu einer Taste, so warten wir noch ein wenig.
      # Ansonsten fangen wir an, den Buffer Zeichen für Zeichen zu leeren.
      { var reg4 object keytab = TheStream(stream)->strm_keyboard_keytab;
        while (consp(keytab))
          { var reg1 object L1 = Car(keytab);
            keytab = Cdr(keytab);
           {var reg1 object L2 = TheStream(stream)->strm_keyboard_buffer;
            while (consp(L1) && consp(L2) && eq(Car(L1),Car(L2)))
              { L1 = Cdr(L1); L2 = Cdr(L2); }
            if (atomp(L2))
              { if (atomp(L1))
                  # vollständige Zeichenfolge
                  { TheStream(stream)->strm_keyboard_buffer = NIL;
                    return L1;
      }   }}  }   }
      { var reg4 object keytab = TheStream(stream)->strm_keyboard_keytab;
        while (consp(keytab))
          { var reg1 object L1 = Car(keytab);
            keytab = Cdr(keytab);
           {var reg1 object L2 = TheStream(stream)->strm_keyboard_buffer;
            while (consp(L1) && consp(L2) && eq(Car(L1),Car(L2)))
              { L1 = Cdr(L1); L2 = Cdr(L2); }
            if (atomp(L2))
              # Da consp(L1), liegt ein Anfangsstück einer Zeichenfolge vor.
              goto wait_for_another;
      }   }}
      goto empty_buffer;
      wait_for_another:
      #ifdef HAVE_SELECT
      { # Verwende select mit readfds = einelementige Menge {stdin_handle}
        # und timeout = kleines Zeitintervall.
        var fd_set handle_menge; # Menge von Handles := {stdin_handle}
        var struct timeval small_time; # Zeitintervall := 0
        FD_ZERO(&handle_menge); FD_SET(stdin_handle,&handle_menge);
        small_time.tv_sec = 0; small_time.tv_usec = 1000000/10; # 1/10 sec
        run_time_stop(); # Run-Time-Stoppuhr anhalten
        begin_system_call();
       {var reg1 int ergebnis;
        signalblock_on(SIGCLD);
        ergebnis = select(FD_SETSIZE,&handle_menge,NULL,NULL,&small_time);
        signalblock_off(SIGCLD);
        end_system_call();
        run_time_restart(); # Run-Time-Stoppuhr weiterlaufen lassen
        if (ergebnis<0)
          { if (!(errno == EBADF)) { OS_error(); } }
          else
          { # ergebnis = Anzahl der Handles in handle_menge, bei denen read
            # sofort ein Ergebnis liefern würde.
            if (ergebnis==0)
              goto empty_buffer; # kein Zeichen verfügbar
            # ergebnis=1 -> Zeichen verfügbar
      }}  }
      #else
      ??
      #endif
      goto read_next_char;
      # Buffer Zeichen für Zeichen liefern:
      empty_buffer:
      { var reg1 object l = TheStream(stream)->strm_keyboard_buffer;
        TheStream(stream)->strm_keyboard_buffer = Cdr(l);
       {var reg1 uintB c = char_code(Car(l));
        if ((c >= ' ') || (c == ESC) || (c == TAB) || (c == CR) || (c == BS))
          { return code_char(c); }
          else
          # Taste vermutlich mit Ctrl getippt
          { return int_char(((64 | c) << char_code_shift_c) | char_control_c); }
      }}
    }}

  # UP: Erweitert die Liste STACK_0 um eine Tastenzuordnung.
  # kann GC auslösen
    local void keybinding (char* cap, cint key);
    local void keybinding(cap,key)
      var reg4 char* cap;
      var reg3 cint key;
      { var reg1 uintB* ptr = (uintB*)cap;
        if (*ptr=='\0') return; # leere Tastenfolge vermeiden
        pushSTACK(allocate_cons());
        # Liste (char1 ... charn . key) bilden:
        {var reg2 uintC count = 0;
         do { pushSTACK(code_char(*ptr)); ptr++; count++; } until (*ptr=='\0');
         pushSTACK(int_char(key)); count++;
         funcall(L(liststern),count);
        }
        # und auf STACK_0 pushen:
        {var reg2 object l = popSTACK();
         Car(l) = value1; Cdr(l) = STACK_0; STACK_0 = l;
      } }

  #endif

# Liefert einen Keyboard-Stream.
# make_keyboard_stream()
# kann GC auslösen
  local object make_keyboard_stream (void);
  local object make_keyboard_stream()
    {
     #ifdef UNIX
      # Tabelle aller Zuordnungen Zeichenfolge -> Taste bilden:
      pushSTACK(NIL);
      # Terminal-Typ abfragen:
      begin_system_call();
     {var reg3 char* s = getenv("TERM");
      if (s==NULL)
        { end_system_call(); }
        else
        { var char tbuf[4096]; # interner Buffer für die Termcap-Routinen
          if (!(tgetent(tbuf,s)==1))
            { end_system_call(); }
            else
            { var char tentry[4096]; # Buffer für von mir benötigte Capabilities und Pointer da hinein
              var char* tp = &tentry[0];
              var reg1 char* cap;
              end_system_call();
              # Backspace:
              begin_system_call(); cap = tgetstr("kb",&tp); end_system_call();
              if (cap) { keybinding(cap,BS); } # #\Backspace
              # Insert, Delete:
              begin_system_call(); cap = tgetstr("kI",&tp); end_system_call();
              if (cap) { keybinding(cap,16 | char_hyper_c); } # #\Insert
              begin_system_call(); cap = tgetstr("kD",&tp); end_system_call();
              if (cap) { keybinding(cap,127); } # #\Delete
              # Pfeiltasten:
              begin_system_call(); cap = tgetstr("ku",&tp); end_system_call();
              if (cap) { keybinding(cap,24 | char_hyper_c); } # #\Up
              if (cap && (cap[0] == ESC) && (cap[1] == 'O') && (cap[2] == 'A') && (cap[3] == '\0'))
                { keybinding(ESCstring"[A",24 | char_hyper_c); } # #\Up
              begin_system_call(); cap = tgetstr("kd",&tp); end_system_call();
              if (cap) { keybinding(cap,18 | char_hyper_c); } # #\Down
              if (cap && (cap[0] == ESC) && (cap[1] == 'O') && (cap[2] == 'B') && (cap[3] == '\0'))
                { keybinding(ESCstring"[B",18 | char_hyper_c); } # #\Down
              begin_system_call(); cap = tgetstr("kr",&tp); end_system_call();
              if (cap) { keybinding(cap,22 | char_hyper_c); } # #\Right
              if (cap && (cap[0] == ESC) && (cap[1] == 'O') && (cap[2] == 'C') && (cap[3] == '\0'))
                { keybinding(ESCstring"[C",22 | char_hyper_c); } # #\Right
              begin_system_call(); cap = tgetstr("kl",&tp); end_system_call();
              if (cap) { keybinding(cap,20 | char_hyper_c); } # #\Left
              if (cap && (cap[0] == ESC) && (cap[1] == 'O') && (cap[2] == 'D') && (cap[3] == '\0'))
                { keybinding(ESCstring"[D",20 | char_hyper_c); } # #\Left
              # sonstige Cursorblock-Tasten:
              begin_system_call(); cap = tgetstr("kh",&tp); end_system_call();
              if (cap) { keybinding(cap,23 | char_hyper_c); } # #\Home
              begin_system_call(); cap = tgetstr("K1",&tp); end_system_call();
              if (cap) { keybinding(cap,23 | char_hyper_c); } # #\Home
              begin_system_call(); cap = tgetstr("KH",&tp); end_system_call();
              if (cap) { keybinding(cap,17 | char_hyper_c); } # #\End
              begin_system_call(); cap = tgetstr("K4",&tp); end_system_call();
              if (cap) { keybinding(cap,17 | char_hyper_c); } # #\End
              begin_system_call(); cap = tgetstr("kP",&tp); end_system_call();
              if (cap) { keybinding(cap,25 | char_hyper_c); } # #\PgUp
              begin_system_call(); cap = tgetstr("K3",&tp); end_system_call();
              if (cap) { keybinding(cap,25 | char_hyper_c); } # #\PgUp
              begin_system_call(); cap = tgetstr("kN",&tp); end_system_call();
              if (cap) { keybinding(cap,19 | char_hyper_c); } # #\PgDn
              begin_system_call(); cap = tgetstr("K5",&tp); end_system_call();
              if (cap) { keybinding(cap,19 | char_hyper_c); } # #\PgDn
              begin_system_call(); cap = tgetstr("K2",&tp); end_system_call();
              if (cap) { keybinding(cap,21 | char_hyper_c); } # #\Center
              # Funktionstasten:
              begin_system_call(); cap = tgetstr("k1",&tp); end_system_call();
              if (cap) { keybinding(cap,'A' | char_hyper_c); } # #\F1
              begin_system_call(); cap = tgetstr("k2",&tp); end_system_call();
              if (cap) { keybinding(cap,'B' | char_hyper_c); } # #\F2
              begin_system_call(); cap = tgetstr("k3",&tp); end_system_call();
              if (cap) { keybinding(cap,'C' | char_hyper_c); } # #\F3
              begin_system_call(); cap = tgetstr("k4",&tp); end_system_call();
              if (cap) { keybinding(cap,'D' | char_hyper_c); } # #\F4
              begin_system_call(); cap = tgetstr("k5",&tp); end_system_call();
              if (cap) { keybinding(cap,'E' | char_hyper_c); } # #\F5
              begin_system_call(); cap = tgetstr("k6",&tp); end_system_call();
              if (cap) { keybinding(cap,'F' | char_hyper_c); } # #\F6
              begin_system_call(); cap = tgetstr("k7",&tp); end_system_call();
              if (cap) { keybinding(cap,'G' | char_hyper_c); } # #\F7
              begin_system_call(); cap = tgetstr("k8",&tp); end_system_call();
              if (cap) { keybinding(cap,'H' | char_hyper_c); } # #\F8
              begin_system_call(); cap = tgetstr("k9",&tp); end_system_call();
              if (cap) { keybinding(cap,'I' | char_hyper_c); } # #\F9
              begin_system_call(); cap = tgetstr("k0",&tp); end_system_call();
              if (cap) { keybinding(cap,'J' | char_hyper_c); } # #\F10
              begin_system_call(); cap = tgetstr("k;",&tp); end_system_call();
              if (cap) { keybinding(cap,'J' | char_hyper_c); } # #\F10
              begin_system_call(); cap = tgetstr("F1",&tp); end_system_call();
              if (cap) { keybinding(cap,'K' | char_hyper_c); } # #\F11
              begin_system_call(); cap = tgetstr("F2",&tp); end_system_call();
              if (cap) { keybinding(cap,'L' | char_hyper_c); } # #\F12
              # Spezielle xterm-Behandlung:
              begin_system_call();
              cap = tgetstr("ku",&tp);
              if (!(cap && (cap[0] == ESC) && (cap[1] == 'O') && (cap[2] == 'A') && (cap[3] == '\0')))
                goto not_xterm;
              cap = tgetstr("kd",&tp);
              if (!(cap && (cap[0] == ESC) && (cap[1] == 'O') && (cap[2] == 'B') && (cap[3] == '\0')))
                goto not_xterm;
              cap = tgetstr("kr",&tp);
              if (!(cap && (cap[0] == ESC) && (cap[1] == 'O') && (cap[2] == 'C') && (cap[3] == '\0')))
                goto not_xterm;
              cap = tgetstr("kl",&tp);
              if (!(cap && (cap[0] == ESC) && (cap[1] == 'O') && (cap[2] == 'D') && (cap[3] == '\0')))
                goto not_xterm;
              if (!tgetflag("km"))
                goto not_xterm;
              end_system_call();
              { # Insert, Delete:
                keybinding(ESCstring"[2~",16 | char_hyper_c); # #\Insert
                keybinding(ESCstring"[3~",127); # #\Delete
              }
              { # Application Keypad: ESC O M -> Return,
                # ESC O k -> +, ESC O m -> -, ESC O j -> *, ESC O o -> /
                # (ohne Hyper-Bit, da das zu Terminal-abhängig würde)
                var char cap[4];
                cap[0] = ESC; cap[1] = 'O'; cap[3] = '\0';
               {var reg1 uintB c;
                for (c='E'; c<='z'; c++) { cap[2] = c; keybinding(&!cap,c-64); }
              }}
              begin_system_call();
              if (!(tgetnum("kn")==4))
                goto not_xterm;
              end_system_call();
              xterm:
              { # Pfeiltasten s.o.
                # sonstige Cursorblock-Tasten:
                keybinding(ESCstring"[5~",25 | char_hyper_c); # #\PgUp
                keybinding(ESCstring"[6~",19 | char_hyper_c); # #\PgDn
                # Funktionstasten:
                keybinding(ESCstring"[11~",'A' | char_hyper_c); # #\F1
                keybinding(ESCstring"[12~",'B' | char_hyper_c); # #\F2
                keybinding(ESCstring"[13~",'C' | char_hyper_c); # #\F3
                keybinding(ESCstring"[14~",'D' | char_hyper_c); # #\F4
                keybinding(ESCstring"[15~",'E' | char_hyper_c); # #\F5
                keybinding(ESCstring"[17~",'F' | char_hyper_c); # #\F6
                keybinding(ESCstring"[18~",'G' | char_hyper_c); # #\F7
                keybinding(ESCstring"[19~",'H' | char_hyper_c); # #\F8
                keybinding(ESCstring"[20~",'I' | char_hyper_c); # #\F9
                keybinding(ESCstring"[21~",'J' | char_hyper_c); # #\F10
                keybinding(ESCstring"[23~",'K' | char_hyper_c); # #\F11
                keybinding(ESCstring"[24~",'L' | char_hyper_c); # #\F12
              }
              not_xterm:
              end_system_call();
     }  }   }
     #endif
     {# neuen Stream allozieren:
      var reg2 object stream =
        allocate_stream(strmflags_rd_ch_B,strmtype_keyboard,strm_len+strm_keyboard_len);
        # Flags: nur READ-CHAR erlaubt
      # und füllen:
      var reg1 Stream s = TheStream(stream);
        s->strm_rd_by = P(rd_by_dummy); # READ-BYTE unmöglich
        s->strm_wr_by = P(wr_by_dummy); # WRITE-BYTE unmöglich
        s->strm_rd_ch = P(rd_ch_keyboard); # READ-CHAR-Pseudofunktion
        s->strm_rd_ch_last = NIL; # Lastchar := NIL
        s->strm_wr_ch = P(wr_ch_dummy); # WRITE-CHAR unmöglich
        s->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
        #ifdef STRM_WR_SS
        s->strm_wr_ss = P(wr_ss_dummy);
        #endif
        #ifdef UNIX
        # Flag isatty = (stdin_tty ? T : NIL) bestimmen:
        begin_system_call();
        s->strm_keyboard_isatty = (isatty(stdin_handle) ? T : NIL);
        end_system_call();
        s->strm_keyboard_handle = allocate_handle(stdin_handle);
        s->strm_keyboard_buffer = NIL;
        s->strm_keyboard_keytab = popSTACK();
        #endif
      return stream;
    }}

#endif # KEYBOARD


# Interaktiver Terminalstream
# ===========================

#ifdef ATARI

# Funktionsweise:
# Bei Bedarf an Zeichen wird eine ganze Zeile eingelesen. Sie wird durch NL
# abgeschlossen. Die eingegebenen Zeichen bleiben am Bildschirm stehen.
# Das abschließende NL wird erst dann ausgegeben (als CR/LF), wenn nötig,
# d.h. bei der nächsten Bildschirmaktion (Lesen einer weiteren Eingabezeile
# oder Ausgeben eines Zeichens). Ist diese nächste Bildschirmaktion eine
# NL-Ausgabe, so wird NL nur einfach und nicht doppelt ausgegeben.
# Ausgabe von Control-Zeichen:  als Grafik-Zeichen,
# alle anderen als Steuerzeichen.
# Bug: Wird eine Zeile eingelesen, die mehr als einen ganzen Bildschirm
# umfaßt, so kann der Bildschirm bei Backspace usw. durcheinanderkommen.

# Zusätzliche Komponenten:
  # INBUFF : Eingabebuffer, ein Simple-String
  #define strm_terminal_inbuff  strm_other[0]
  # COUNT  : Anzahl der Zeichen im Eingabebuffer
  #define strm_terminal_count   strm_other[1]
  # INDEX  : Anzahl der bereits verbrauchten Zeichen
  #define strm_terminal_index   strm_other[2]
  # NLflag : 1 falls NL wartet, 0 sonst
  #define strm_terminal_NLflag  strm_other[3]

# Zusätzliche Information: Größe des Ausgabe-Fensters in x- bzw. y-Richtung
  local struct { uintC x; uintC y; } window_size;
  #define window_width  window_size.x

# Stellt fest, ob ein Terminal-Stream ein Zeichen verfügbar hat.
# listen_terminal(stream)
# > stream: Terminal-Stream
# < ergebnis:  0 falls Zeichen verfügbar,
#             -1 falls bei EOF angelangt,
#             +1 falls kein Zeichen verfügbar, aber nicht wegen EOF
  local signean listen_terminal (object stream);
  local signean listen_terminal(stream)
    var reg1 object stream;
    { # Buffer der gelesenen Zeichen leer ?
      if (TheStream(stream)->strm_terminal_index >= TheStream(stream)->strm_terminal_count)
        { return listen_keyboard(stream); } # ja -> Tastatur abfragen
        else
        { return signean_null; } # nein -> Es sind noch Zeichen im Buffer
    }

# UP: Löscht bereits eingegebenen interaktiven Input von einem Terminal-Stream.
# clear_input_terminal(stream);
# > stream: Terminal-Stream
# < ergebnis: TRUE falls Input gelöscht wurde, FALSE sonst
  local boolean clear_input_terminal (object stream);
  local boolean clear_input_terminal(stream)
    var reg1 object stream;
    { # Buffer der gelesenen Zeichen leeren durch INDEX := COUNT :
      TheStream(stream)->strm_terminal_index = TheStream(stream)->strm_terminal_count;
      # GEMDOS-Tastaturbuffer leeren:
      return clear_input_keyboard(stream);
    }

# UP: Newline auf einen Terminal-Stream ausgeben und Flag löschen.
# wr_NL_terminal(stream);
# > stream: Terminal-Stream
  local void wr_NL_terminal (object stream);
  local void wr_NL_terminal(stream)
    var reg1 object stream;
    { asciz_out(CRLFstring); # Newline als CR/LF ausgeben
      TheStream(stream)->strm_terminal_NLflag = Fixnum_0; # nun wartet kein NL mehr
    }

# UP: Ein Zeichen auf einen Terminal-Stream ausgeben.
# wr_ch_terminal(&stream,ch);
# > stream: Terminal-Stream
# > ch: auszugebendes Zeichen
  local void wr_ch_terminal (object* stream_, object ch);
  local void wr_ch_terminal(stream_,ch)
    var reg4 object* stream_;
    var reg2 object ch;
    { var reg1 object stream = *stream_;
      if (!string_char_p(ch)) { fehler_string_char(stream,ch); } # ch sollte String-Char sein
     {var reg2 uintB c = char_code(ch); # Code des Zeichens
      if (c == NL) # Newline ?
        { wr_NL_terminal(stream); }
        else
        { # kein Newline
          # Wartet noch ein NL, wird es jetzt ausgegeben:
          if (eq(TheStream(stream)->strm_terminal_NLflag,Fixnum_1))
            { wr_NL_terminal(stream); }
          # Code c übers BIOS auf den Bildschirm ausgeben:
          # c >= ' ' -> übers GEMDOS
          # 0 <= c < 32 -> genau c in {1,..,4}u{14,..,25}u{28,..,31} übers BIOS
          #                sonst als Steuerzeichen übers BIOS
          if ((c < ' ') &&
              # Bit c aus der 32-Bit-Zahl %11110011111111111100000000011110 holen:
              (0xF3FFC01EUL & bit(c))
             )
            { BIOS_GrConOut(c); }
            else
            { BIOS_ConOut(c); }
        }
      # Line Position aktualisieren (es wartet kein NL mehr!):
      TheStream(stream)->strm_wr_ch_lpos =
        fixnum((UWORD)(vdiesc.v_cur_x));
      # Bei gedrückter rechter Maustaste warten:
      if (LineA_MouseButtons() & bit(1)) # rechte Maustaste gedrückt?
        { run_time_stop(); # Run-Time-Stoppuhr anhalten
          if (!(c==NL))
            # Maus wird bewegt -> Zeitschleife von 0.005 Sekunden.
            # Maus wird nicht bewegt -> warten, bis die rechte Maustaste los-
            # gelassen oder die Maus bewegt wurde, mind. jedoch 0.005 Sekunden.
            { interruptp( {goto interrupt;} ); # erst auf Tastatur-Interrupt testen
              # Zeitschleife von 0.005 Sekunden:
              { var reg1 uintC count; dotimespC(count,4096, ; ); }
            }
            else
            # Maus wird bewegt -> Zeitschleife von 0.4 Sekunden.
            # Maus wird nicht bewegt -> warten, bis die rechte Maustaste los-
            # gelassen oder die Maus bewegt wurde, mind. jedoch 0.4 Sekunden.
            { # Zeitschleife von 0.4 Sekunden mit Tastatur-Interrupt-Abfrage:
              var reg1 uintL endtime = get_real_time() + ticks_per_second*2/5; # zur momentanen Real-Time 0.4 sec. addieren,
              # ergibt Zeit, bis zu der zu warten ist.
              # warten, bis die Real-Time bei endtime angelangt ist:
              do { interruptp( {goto interrupt;} ); }
                 until (get_real_time() == endtime);
            }
          # Ende der Warteschleife oder nicht, je nach Mausstatus:
          loop
            { if (LineA_MouseStatus() & bit(5)) # Maus wurde bewegt?
                { LineA_MouseStatus() &= ~bit(5); break; } # ja -> nicht weiter warten
              interruptp( {goto interrupt;} );
              if ((LineA_MouseButtons() & bit(1)) ==0) # rechte Maustaste gedrückt?
                break; # nein -> nicht mehr warten
            }
          if (TRUE)
            { run_time_restart(); } # Run-Time-Stoppuhr weiterlaufen lassen
            else
            { interrupt: # Tastatur-Interrupt
              run_time_restart(); # Run-Time-Stoppuhr weiterlaufen lassen
              pushSTACK(S(write_char)); tast_break(); # Break-Schleife aufrufen
        }   }
    }}

# UP: Bringt den wartenden Output eines Terminal-Stream ans Ziel.
# finish_output_terminal(stream);
# > stream: Terminal-Stream
# kann GC auslösen
  local void finish_output_terminal (object stream);
  local void finish_output_terminal(stream)
    var reg1 object stream;
    { # Wartet noch ein NL, wird es jetzt ausgegeben:
      if (eq(TheStream(stream)->strm_terminal_NLflag,Fixnum_1))
        { wr_NL_terminal(stream); }
    }

# UP: Bringt den wartenden Output eines Terminal-Stream ans Ziel.
# force_output_terminal(stream);
# > stream: Terminal-Stream
# kann GC auslösen
  #define force_output_terminal  finish_output_terminal

# Lesen eines Zeichens vom Terminal-Stream
  local object rd_ch_terminal (object* stream_);
  #
  # Zustand während der Eingabe:
  typedef struct { uintL linelen; # Länge der Eingabezeile (<= length(inbuff))
                   uintL linepos; # Position in der Eingabezeile
                   uintC screenpos; # Line Position am Bildschirm (>=0, <window_width)
                 }
          rd_ch_vars;
  # > v : aktueller Zustand
  # > STACK_1 : Terminal-Stream
  # > STACK_0 : Eingabebuffer INBUFF
  #
  # Unterprogramme während der Eingabe einer Zeile:
  #
  # UP: macht String so lang, daß noch ein Zeichen hineinpaßt,
  # und erhöht die Zeilenlänge um 1.
  # add1char(&v);
    local void add1char (rd_ch_vars* v);
    local void add1char(v)
      var reg4 rd_ch_vars* v;
      { if (v->linelen >= TheSstring(STACK_0)->length)
          # Bei linelen < Stringlänge ist im String noch Platz.
          # Bei linelen = Stringlänge muß man den String verlängern:
          { var reg3 uintL count = TheSstring(STACK_0)->length; # alte Stringlänge (>0)
            var reg5 object new_string = allocate_string(2*count); # doppelt so langer String
            # Stringinhalt von String STACK_0 nach String new_string kopieren:
            var reg2 uintB* ptr1 = &TheSstring(STACK_0)->data[0];
            var reg1 uintB* ptr2 = &TheSstring(new_string)->data[0];
            dotimespL(count,count, { *ptr2++ = *ptr1++; } );
            # new_string ablegen:
            STACK_0 = new_string;
            TheStream(STACK_1)->strm_terminal_inbuff = new_string;
          }
        v->linelen++; # Zeilenlänge erhöhen
      }
  #
  # UP: akzeptiert ein normales Zeichen c.
  # normalchar(&v,c);
    local void normalchar (rd_ch_vars* v, uintB c);
    local void normalchar(v,c)
      var reg1 rd_ch_vars* v;
      var reg2 uintB c;
      { # Eventuell die Eingabezeile verlängern:
        if (v->linepos >= v->linelen) { add1char(v); }
        # Jetzt ist sicher linepos < linelen.
        TheSstring(STACK_0)->data[v->linepos] = c; # Zeichen in den String
        v->linepos++; # und Index erhöhen
        BIOS_ConOut(c); # Zeichen ausgeben
        # Bildschirmposition adjustieren:
        v->screenpos++; if (v->screenpos == window_width) { v->screenpos = 0; }
      }
  #
  # UP: Cursor in derselben Zeile an eine neue Position setzen.
  # cursor_gotox(&v,x);
  # > x: neue Position (>=0, <window_width)
    local void cursor_gotox (rd_ch_vars* v, uintC x);
    local void cursor_gotox(v,x)
      var reg2 rd_ch_vars* v;
      var reg3 uintC x;
      { if (x < floor(v->screenpos,2))
          { BIOS_ConOut(CR); v->screenpos = 0; } # nach links geht's mit CR schneller
       {var reg4 sintC rel_pos = x - v->screenpos; # relative Positionierung
        if (rel_pos >= 0)
          { var reg1 uintC count;
            dotimesC(count,rel_pos, { asciz_out(ESCstring "C"); } ); # Cursor nach rechts
          }
        else
          { var reg1 uintC count;
            dotimespC(count,-rel_pos, { asciz_out(ESCstring "D"); } ); # Cursor nach links
          }
        v->screenpos = x; # Cursor steht jetzt in Spalte x
      }}
  #
  # UP: Cursor ein Zeichen weiter nach links, linepos mitführen.
  # cursor_1left(&v);
    local void cursor_1left (rd_ch_vars* v);
    local void cursor_1left(v)
      var reg2 rd_ch_vars* v;
      { v->linepos--;
        if (!(v->screenpos == 0))
          # Cursor war nicht ganz links -> Cursor eins nach links
          { v->screenpos--; asciz_out(ESCstring "D"); }
          else
          # Cursor war ganz links -> zeilenübergreifend:
          { v->screenpos = window_width-1;
            # bewege den Cursor vom linken Rand einer Zeile
            # an den rechten Rand der nächsthöheren Zeile:
            asciz_out(ESCstring "f"); # Cursor ausschalten
            asciz_out(ESCstring "I"); # Cursor eine Zeile hoch
            { var reg1 uintC count;
              dotimesC(count,window_width-1, # 79 mal:
                { asciz_out(ESCstring "C"); } # Cursor ein Zeichen rechts
                );
            }
            asciz_out(ESCstring "e"); # Cursor wieder einschalten
          }
      }
  #
  # UP: Cursor einige Zeichen nach links.
  # cursor_left(&v,count);
  # > count: Zeichenzahl (>=0)
    local void cursor_left (rd_ch_vars* v, sintL count);
    local void cursor_left(v, count)
      var reg3 rd_ch_vars* v;
      var reg2 sintL count;
      { asciz_out(ESCstring "f"); # Cursor ausschalten
        # Cursor einige Zeilen hoch:
        {var reg1 sintL countlimit = v->screenpos;
         while (count > countlimit)
           { asciz_out(ESCstring "I"); # Cursor eine Zeile hoch
             count -= window_width;
        }  }
        # Nun ist screenpos-window_width < count <= screenpos.
        # Der Cursor befindet sich also in der richtigen Zeile.
        cursor_gotox(v,v->screenpos-count);
        # Der Cursor steht jetzt richtig.
        asciz_out(ESCstring "e"); # Cursor wieder einschalten
      }
  #
  # UP: Cursor ein Zeichen weiter nach rechts, linepos mitführen.
  # cursor_1right(&v);
    local void cursor_1right (rd_ch_vars* v);
    local void cursor_1right(v)
      var reg2 rd_ch_vars* v;
      { if (!(v->screenpos == window_width-1))
          # Cursor war nicht ganz rechts -> Cursor eins nach rechts
          { v->screenpos++; asciz_out(ESCstring "C"); }
          else
          # Cursor war ganz rechts -> zeilenübergreifend:
          { v->screenpos = 0;
            BIOS_ConOut(TheSstring(STACK_0)->data[v->linepos]);
          }
        v->linepos++;
      }
  #
  # UP: Cursor einige Zeichen nach rechts.
  # cursor_right(&v,count);
  # > count: Zeichenzahl (>=0)
    local void cursor_right (rd_ch_vars* v, sintL count);
    local void cursor_right(v, count)
      var reg3 rd_ch_vars* v;
      var reg2 sintL count;
      { asciz_out(ESCstring "f"); # Cursor ausschalten
        # Cursor einige Zeilen runter:
        {var reg1 sintL countlimit = window_width - v->screenpos;
         while (count >= countlimit)
           { asciz_out(ESCstring "B"); # Cursor eine Zeile runter
             count -= window_width;
        }  }
        # Nun ist -screenpos <= count < window_width-screenpos.
        # Der Cursor befindet sich also in der richtigen Zeile.
        cursor_gotox(v,v->screenpos+count);
        # Der Cursor steht jetzt richtig.
        asciz_out(ESCstring "e"); # Cursor wieder einschalten
      }
  #
  # UP: An der Cursor-Position Platz für ein Zeichen machen.
  # insert1char(&v,c);
    local void insert1char (rd_ch_vars* v, uintB c);
    local void insert1char(v,c)
      var reg5 rd_ch_vars* v;
      var reg2 uintB c;
      { add1char(v); # Eingabezeile wird 1 Zeichen länger
       {var reg5 uintL charcount = v->linelen - v->linepos;
        # = Anzahl der zu verschiebenden Zeichen + 1
        asciz_out(ESCstring "f"); # Cursor ausschalten
        {var reg1 uintB* ptr = &TheSstring(STACK_0)->data[v->linepos];
         var reg3 uintL count;
         # Schleife charcount mal durchlaufen, dabei beim ersten
         # Durchlauf das übergebene c einfügen:
         dotimespL(count,charcount,
           { var reg4 uintB nextc = *ptr; # nächstes Zeichen
             *ptr++ = c; # durch c ersetzen
             BIOS_ConOut(c); # und c ausgeben
             c = nextc;
           });
        }
        # neue Spalte am Bildschirm berechnen:
        {var reg1 uintL new_screenpos = v->screenpos + charcount;
         while (new_screenpos >= window_width) { new_screenpos -= window_width; }
         v->screenpos = new_screenpos;
        }
        # Cursor an die alte Position und Cursor wieder einschalten:
        cursor_left(v,charcount);
      }}
  #
  # UP: An der Cursor-Position ein Zeichen löschen.
  # > 0 <= linepos < linelen.
  # delete1(&v);
    local void delete1 (rd_ch_vars* v);
    local void delete1(v)
      var reg5 rd_ch_vars* v;
      { v->linelen--; # Gesamtzeichenzahl decrementieren
       {var reg4 uintL charcount = v->linelen - v->linepos;
        # = Anzahl der zu verschiebenden Zeichen
        asciz_out(ESCstring "f"); # Cursor ausschalten
        {var reg1 uintB* ptr = &TheSstring(STACK_0)->data[v->linepos];
         var reg3 uintL count;
         dotimesL(count,charcount,
           { var reg2 uintB nextch = *(ptr+1); # nächstes Zeichen
             *ptr++ = nextch; # um eine Position nach vorne schieben
             BIOS_ConOut(nextch); # und ausgeben
           });
        }
        BIOS_ConOut(' '); # letztes Zeichen mit ' ' überschreiben
        charcount++;
        # neue Spalte am Bildschirm berechnen:
        {var reg1 uintL new_screenpos = v->screenpos + charcount;
         while (new_screenpos >= window_width) { new_screenpos -= window_width; }
         v->screenpos = new_screenpos;
        }
        # Cursor an die alte Position und Cursor wieder einschalten:
        cursor_left(v,charcount);
      }}
  #
  local object rd_ch_terminal(stream_)
    var reg1 object* stream_;
    { var reg1 object stream = *stream_;
      var reg1 object inbuff = TheStream(stream)->strm_terminal_inbuff; # Eingabebuffer
      if (posfixnum_to_L(TheStream(stream)->strm_terminal_index)
          >= posfixnum_to_L(TheStream(stream)->strm_terminal_count)
         )
        # index<count -> Es sind noch Zeichen im Buffer
        # index=count -> muß eine ganze Zeile von Tastatur lesen:
        { # Wartet noch ein NL, wird es jetzt ausgegeben:
          if (eq(TheStream(stream)->strm_terminal_NLflag,Fixnum_1))
            { wr_NL_terminal(stream); }
          TheStream(stream)->strm_terminal_index = Fixnum_0; # index := 0
          TheStream(stream)->strm_terminal_count = Fixnum_0; # count := 0
         {var rd_ch_vars v; # Zustandsvariablen
          v.linelen = 0; v.linepos = 0; # Zeile noch leer
          v.screenpos = posfixnum_to_L(TheStream(stream)->strm_wr_ch_lpos); # aktuelle Screen-Spalte
          pushSTACK(stream);
          pushSTACK(inbuff);
          asciz_out(ESCstring "e"); # Cursor einschalten
          loop
            { # Eingabeschleife.
              # STACK_1 : Terminal-Stream,
              # STACK_0 : Eingabezeile inbuff (ein Simple-String),
              # v.linelen : Länge der Eingabezeile (<= length(inbuff))
              # Eingabezeile = TheSstring(STACK_0)->data[0..(v.linelen-1)]
              # v.linepos : Position in der Eingabezeile (>=0, <=linelen)
              # v.screenpos : Line Position am Bildschirm (>=0, <window_width)
              {var object kbstream = var_stream(S(keyboard_input)); # Stream *KEYBOARD-INPUT*
               var reg2 object ch = read_char(&kbstream); # Zeichen holen
               if (eq(ch,eof_value)) # EOF (sollte nicht auftreten) ?
                 { funcall(L(exit),0); } # ja -> LISP beenden
               if (v.linelen == 0)
                 # Eine Taste aus SYS::*KEY-BINDINGS* ?
                 { var reg1 object alist = Symbol_value(S(key_bindings));
                   while (consp(alist))
                     { if (mconsp(Car(alist)) && eq(ch,Car(Car(alist))))
                         { asciz_out(ESCstring "f"); # Cursor ausschalten
                           funcall(Cdr(Car(alist)),0); # Funktion dazu aufrufen
                           # und wenn diese Funktion zurückkehren sollte:
                           asciz_out(ESCstring "e"); # Cursor einschalten
                           break;
                         }
                       alist = Cdr(alist);
                 }   }
               {var reg1 cint c = char_int(ch);
                if (!(c & char_hyper_c)) # HYPER-Bit gesetzt ?
                  # nein -> normales Zeichen
                  { c &= char_code_mask_c; # SUPER-Bit usw. wegmaskieren
                    switch (c)
                      { case BS: # Backspace
                          # Backspace-Taste entfernt das Zeichen links vom Cursor
                          if (v.linepos == 0) goto nix; # war der Cursor schon ganz links -> Fehleingabe
                          cursor_1left(&v); # Cursor ein Zeichen nach links
                          delete1(&v); # Zeichen unterm Cursor löschen
                          break;
                        case TAB: # Tab
                          # Tab fügt Spaces ein, bis der Cursor auf einer
                          # durch 8 teilbaren Spalte steht.
                          do { normalchar(&v,' '); }
                             until ((v.screenpos % 8) == 0);
                          break;
                        case CR: # Return/Enter
                          do_CR:
                          # Return/Enter beendet die Zeile mit einem NL.
                          cursor_right(&v,v.linelen - v.linepos);
                          add1char(&v);
                          # String mit NL abschließen:
                          TheSstring(STACK_0)->data[v.linelen-1] = NL;
                          goto loopend;
                        default:
                          if (c < ' ') # sonstiges Control-Zeichen < ' ' ?
                            goto nix; # ja -> war nix
                          # normales Zeichen
                          normalchar(&v,c);
                  }   }
                  else
                  # ja -> Sondertaste
                  { switch (c)
                      { case char_hyper_c|CR : # Taste 'Enter'
                          goto do_CR;
                        case char_hyper_c|127: # Taste 'Delete'
                          # Delete-Taste entfernt das Zeichen unterm Cursor
                          if (v.linepos >= v.linelen) goto nix; # war der Cursor schon ganz rechts -> Fehleingabe
                          delete1(&v); # Zeichen unterm Cursor löschen
                          break;
                        case char_hyper_c|16 : # Taste 'Insert'
                          # Insert-Taste macht am Cursor Platz für ein Zeichen
                          insert1char(&v,' '); break;
                        case char_hyper_c|20 : # Taste '<-'
                          # Pfeil links -> Cursor eine Position nach links
                          if (v.linepos==0) goto nix; # war der Cursor schon ganz links -> Fehleingabe
                          cursor_1left(&v); break;
                        case char_hyper_c|char_super_c|20 : # Taste Shift '<-'
                          # Shift Pfeil links -> Cursor ganz nach links
                          cursor_left(&v,v.linepos);
                          v.linepos = 0;
                          break;
                        case char_hyper_c|22 : # Taste '->'
                          # Pfeil rechts -> Cursor eine Position nach rechts
                          if (v.linepos < v.linelen)
                            { cursor_1right(&v); }
                            else
                            { normalchar(&v,' '); } # am Zeilenende: Leerstelle anfügen
                          break;
                        case char_hyper_c|char_super_c|22 : # Taste Shift '->'
                          # Shift Pfeil rechts -> Cursor ganz nach rechts
                          cursor_right(&v,v.linelen-v.linepos);
                          v.linepos = v.linelen;
                          break;
                        default:
                          { var reg1 uintB code = c & char_code_mask_c;
                            if ((code >= ' ') && (code < '@'))
                              # Sondertaste >=' ' und <'@' kommt vom Ziffernblock
                              # -> als normales Zeichen behandeln
                              { normalchar(&v,code); break; }
                          }
                        nix: # Zeichen war nix (Fehleingabe) -> Glocke läuten
                          BIOS_Bell();
                  }   }
            } }}
          loopend: # Eingabezeile fertig.
          asciz_out(ESCstring "f"); # Cursor ausschalten
          inbuff = popSTACK(); stream = popSTACK(); # STACK aufräumen
          TheStream(stream)->strm_terminal_index = Fixnum_0; # Index := 0
          TheStream(stream)->strm_terminal_count = fixnum(v.linelen);
          TheStream(stream)->strm_terminal_NLflag = Fixnum_1; # wartendes NL vormerken
          TheStream(stream)->strm_wr_ch_lpos = Fixnum_0; # und deswegen Line Position := 0
        }}
      # Jetzt sind genug Zeichen da, d.h. INDEX < COUNT .
     {var reg1 uintL index =
        posfixnum_to_L(TheStream(stream)->strm_terminal_index); # Index
      TheStream(stream)->strm_terminal_index =
        fixnum_inc(TheStream(stream)->strm_terminal_index,1); # Index erhöhen
      return code_char(TheSstring(inbuff)->data[index]); # nächstes Character
    }}

# Liefert einen interaktiven Terminal-Stream.
# kann GC auslösen
  local object make_terminal_stream (void);
  local object make_terminal_stream()
    { # Bildschirmgröße bestimmen:
      window_size.x = (UWORD)(vdiesc.v_cel_mx+1);
      window_size.y = (UWORD)(vdiesc.v_cel_my+1);
     {# neuen Stream allozieren:
      var reg2 object stream =
        allocate_stream(strmflags_ch_B,strmtype_terminal,strm_len+4);
        # Flags: nur READ-CHAR und WRITE-CHAR erlaubt
      # und füllen:
      var reg1 Stream s = TheStream(stream);
        s->strm_rd_by = P(rd_by_dummy); # READ-BYTE unmöglich
        s->strm_wr_by = P(wr_by_dummy); # WRITE-BYTE unmöglich
        s->strm_rd_ch = P(rd_ch_terminal); # READ-CHAR-Pseudofunktion
        s->strm_rd_ch_last = NIL; # Lastchar := NIL
        s->strm_wr_ch = P(wr_ch_terminal); # WRITE-CHAR-Pseudofunktion
        s->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
        #ifdef STRM_WR_SS
        s->strm_wr_ss = P(wr_ss_dummy);
        #endif
        {pushSTACK(stream);
         {var reg1 object inbuff = allocate_string(window_width+1); # neuer String
          # (reicht für alle einzeiligen Eingaben)
          stream = popSTACK(); s = TheStream(stream);
          s->strm_terminal_inbuff = inbuff; # als Eingabebuffer
        }}
        s->strm_terminal_count = Fixnum_0; # Buffer leer, Count:=0
        s->strm_terminal_index = Fixnum_0; # Index:=0
        s->strm_terminal_NLflag = Fixnum_0; # NL-Flag := false
      return stream;
    }}

#endif # ATARI

#if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX) || defined(AMIGAOS) || defined(VMS)

# Funktionsweise:
# Es wird auf Standard-Input und Standard-Output zugegriffen.
# Wegen Umleite-Möglichkeit müssen manche Funktionen unterscheiden, ob es
# sich bei Standard-Input um ein Terminal handelt oder nicht.
# Ob Standard-Output ein Terminal ist oder nicht, ist hier irrelevant.
# Relevant ist nur, ob Standard-Input und Standard-Output dasselbe Terminal
# sind; in diesem Falle nehmen wir an, daß nach Beendigung einer Eingabezeile
# (durch NL) von Standard-Input der Cursor von Standard-Output in Spalte 0
# steht, und in diesem Falle können wir auch die GNU readline()-Library
# benutzen.

# Es gibt drei mögliche Varianten des Terminal-Streams:
# Wenn Standard-Input und Standard-Output nicht dasselbe Terminal sind:
#   * terminal1 normalerweise,
#   * terminal2 mit zeilenweiser Bufferung der Eingabe,
# Wenn Standard-Input und Standard-Output dasselbe Terminal sind:
#   * terminal3 benutzt readline()-Library, mit zeilenweiser Bufferung der
#     Eingabe und der Ausgabe.

#define HAVE_TERMINAL1
  # define TERMINAL_LINEBUFFERED  0
  # define TERMINAL_OUTBUFFERED   0

#ifdef MSDOS
  # Bei Eingabe einer Zeile von Tastatur wird das <Enter> am Ende der Zeile als
  # CR/LF ausgegeben. Jedoch: Das CR sofort, das LF jedoch erst dann, wenn das
  # <Enter> mit read() gelesen wird - das ist bei uns manchmal erst viel später.
  # [Wer diesen Schwachsinn programmiert hat - im DOS vermutlich -
  # gehört an die Wand gestellt und erschossen! :-(]
  # Aus diesem Grund müssen wir den Terminal-Stream auf der Input-Seite
  # zeilengepuffert machen.
#define HAVE_TERMINAL2
  # define TERMINAL_LINEBUFFERED  1
  # define TERMINAL_OUTBUFFERED   0
#endif

#ifdef GNU_READLINE
  # Wir benutzen die GNU Readline-Library. Sie liefert den Input zeilenweise,
  # mit Editiermöglichkeit, Vervollständigung und History. Leider müssen wir
  # den Output zeilenweise zwischenspeichern, um die letzte angefangene Zeile
  # als "Prompt" verwenden zu können.
#define HAVE_TERMINAL3
  # define TERMINAL_LINEBUFFERED  1
  # define TERMINAL_OUTBUFFERED   1
#endif

# Zusätzliche Komponenten:
  # ISATTY : Flag, ob stdin ein TTY ist und ob stdin und stdout dasselbe sind:
  #          NIL: stdin ein File o.ä.
  #          T, EQUAL: stdin ein Terminal
  #          EQUAL: stdin und stdout dasselbe Terminal
  #define strm_terminal_isatty   strm_isatty
  #define strm_terminal_ihandle  strm_ihandle
  #define strm_terminal_ohandle  strm_ohandle
#if defined(HAVE_TERMINAL2) || defined(HAVE_TERMINAL3)
  # Komponenten wegen TERMINAL_LINEBUFFERED:
  # INBUFF : Eingabebuffer, ein Semi-Simple-String
  #define strm_terminal_inbuff  strm_other[3]
  # COUNT = sein Fill-Pointer : Anzahl der Zeichen im Eingabebuffer
  # INDEX : Anzahl der bereits verbrauchten Zeichen
  #define strm_terminal_index   strm_other[4]
#endif
#ifdef HAVE_TERMINAL3
  # Komponenten wegen TERMINAL_OUTBUFFERED:
  # OUTBUFF : Ausgabebuffer, ein Semi-Simple-String
  #define strm_terminal_outbuff strm_other[5]
#endif

# Längen der unterschiedlichen Terminal-Streams:
  #define strm_terminal1_len  (strm_len+3)
  #define strm_terminal2_len  (strm_len+5)
  #define strm_terminal3_len  (strm_len+6)

# Unterscheidung nach Art des Terminal-Streams:
# terminalcase(stream, statement1,statement2,statement3);
  #if defined(HAVE_TERMINAL2) && defined(HAVE_TERMINAL3)
    #define terminalcase(stream,statement1,statement2,statement3)  \
      switch (TheStream(stream)->reclength)          \
        { case strm_terminal1_len: statement1 break; \
          case strm_terminal2_len: statement2 break; \
          case strm_terminal3_len: statement3 break; \
          default: NOTREACHED                        \
        }
  #elif defined(HAVE_TERMINAL2)
    #define terminalcase(stream,statement1,statement2,statement3)  \
      if (TheStream(stream)->reclength == strm_terminal2_len) { statement2 } else { statement1 }
  #elif defined(HAVE_TERMINAL3)
    #define terminalcase(stream,statement1,statement2,statement3)  \
      if (TheStream(stream)->reclength == strm_terminal3_len) { statement3 } else { statement1 }
  #else
    #define terminalcase(stream,statement1,statement2,statement3)  \
      statement1
  #endif

#ifdef HAVE_TERMINAL1

# Lesen eines Zeichens von einem Terminal-Stream.
  local object rd_ch_terminal1 (object* stream_);
  local object rd_ch_terminal1(stream_)
    var reg3 object* stream_;
    { var reg1 object ch = rd_ch_handle(stream_);
      # Wenn stdin und stdout beide dasselbe Terminal sind,
      # und wir lesen ein NL, so können wir davon ausgehen,
      # daß der Cursor danach in Spalte 0 steht.
      if (eq(ch,code_char(NL)))
        { var reg2 object stream = *stream_;
          if (eq(TheStream(stream)->strm_terminal_isatty,S(equal)))
            { TheStream(stream)->strm_wr_ch_lpos = Fixnum_0; }
        }
      return ch;
    }

# Stellt fest, ob ein Terminal-Stream ein Zeichen verfügbar hat.
# listen_terminal1(stream)
# > stream: Terminal-Stream
# < ergebnis:  0 falls Zeichen verfügbar,
#             -1 falls bei EOF angelangt,
#             +1 falls kein Zeichen verfügbar, aber nicht wegen EOF
  #define listen_terminal1  listen_handle

# UP: Löscht bereits eingegebenen interaktiven Input von einem Terminal-Stream.
# clear_input_terminal1(stream);
# > stream: Terminal-Stream
# < ergebnis: TRUE falls Input gelöscht wurde, FALSE sonst
  #define clear_input_terminal1  clear_input_handle

# UP: Ein Zeichen auf einen Terminal-Stream ausgeben.
# wr_ch_terminal1(&stream,ch);
# > stream: Terminal-Stream
# > ch: auszugebendes Zeichen
 #if !defined(AMIGAOS)
  #define wr_ch_terminal1  wr_ch_handle
 #else # defined(AMIGAOS)
  local void wr_ch_terminal1 (object* stream_, object ch);
  local void wr_ch_terminal1(stream_,ch)
    var reg6 object* stream_;
    var reg5 object ch;
    { # ch sollte ein Character mit höchstens Font, aber ohne Bits sein:
      if (!(((oint)ch & ~(((oint)char_code_mask_c|(oint)char_font_mask_c)<<oint_addr_shift)) == (oint)type_data_object(char_type,0)))
        { pushSTACK(*stream_);
          pushSTACK(ch);
          fehler(
                 DEUTSCH ? "Character ~ enthält Bits und kann daher nicht auf ~ ausgegeben werden." :
                 ENGLISH ? "character ~ contains bits, cannot be output onto ~" :
                 FRANCAIS ? "Le caractère ~ contient des «bits» et ne peut pas être écrit dans ~." :
                 ""
                );
        }
     #if (!(char_font_len_c == 4))
       #error "char_font_len_c neu einstellen oder wr_ch_terminal neu schreiben!"
     #endif
     {var uintB outbuffer[14];
      var reg1 uintB* ptr = &outbuffer[0];
      var reg3 uintL count = 1;
      var reg2 uintB f = (char_int(ch) & char_font_mask_c) >> char_font_shift_c; # Font des Zeichens
      var reg4 uintB c = char_code(ch); # Code des Zeichens
      if (f==0)
        { *ptr++ = c; }
        else
        { *ptr++ = CSI; # Kontroll-Sequenz zum Umschalten auf den richtigen Font:
          if (f & bit(0)) { *ptr++ = '1'; *ptr++ = ';'; count += 2; } # Fettschrift ein
          if (f & bit(1)) { *ptr++ = '3'; *ptr++ = ';'; count += 2; } # Kursiv ein
          if (f & bit(2)) { *ptr++ = '4'; *ptr++ = ';'; count += 2; } # Unterstreichung ein
          if (f & bit(3)) { *ptr++ = '7'; *ptr++ = ';'; count += 2; } # Reverse ein
          *ptr++ = 0x6D;
          *ptr++ = c; # dann das Zeichen ausgeben
          *ptr++ = CSI; *ptr++ = '0'; *ptr++ = 0x6D; # Wieder Normalschrift
          count += 5;
        }
      begin_system_call();
      {var reg1 long ergebnis = Write(Output_handle,&outbuffer[0],count); # Zeichen auszugeben versuchen
       end_system_call();
       if (ergebnis<0) { OS_error(); } # Error melden
       if (ergebnis<count) # nicht erfolgreich?
         { fehler_unwritable(S(write_char),*stream_); }
    }}}
 #endif

#ifdef STRM_WR_SS
# UP: Mehrere Zeichen auf einen Terminal-Stream ausgeben.
# wr_ss_terminal1(&stream,string,start,len);
# > stream: Terminal-Stream
# > string: Simple-String
# > start: Startindex
# > len: Anzahl der auszugebenden Zeichen
  #define wr_ss_terminal1  wr_ss_handle
#endif

# UP: Löscht den wartenden Output eines Terminal-Stream.
# clear_output_terminal1(stream);
# > stream: Terminal-Stream
# kann GC auslösen
  #define clear_output_terminal1  clear_output_handle

#endif # HAVE_TERMINAL1

#ifdef HAVE_TERMINAL2

#define TERMINAL_LINEBUFFERED  TRUE

# Lesen eines Zeichens von einem Terminal-Stream.
  local object rd_ch_terminal2 (object* stream_);
  # vgl. rd_ch_handle() :
  local object rd_ch_terminal2(stream_)
    var reg3 object* stream_;
    { restart_it:
     {var reg2 object stream = *stream_;
      if (eq(TheStream(stream)->strm_rd_ch_last,eof_value)) # schon EOF ?
        { return eof_value; }
      #if TERMINAL_LINEBUFFERED
      { var reg4 object inbuff = TheStream(stream)->strm_terminal_inbuff; # Eingabebuffer
        if (posfixnum_to_L(TheStream(stream)->strm_terminal_index)
            < TheArray(inbuff)->dims[1]
           )
          # index<count -> Es sind noch Zeichen im Buffer
          { var reg1 uintL index =
              posfixnum_to_L(TheStream(stream)->strm_terminal_index); # Index
            TheStream(stream)->strm_terminal_index =
              fixnum_inc(TheStream(stream)->strm_terminal_index,1); # Index erhöhen
            return code_char(TheSstring(TheArray(inbuff)->data)->data[index]); # nächstes Character
          }
        # index=count -> muß eine ganze Zeile von Tastatur lesen:
        TheStream(stream)->strm_terminal_index = Fixnum_0; # index := 0
        TheArray(inbuff)->dims[1] = 0; # count := 0
      }
      continue_line:
      #endif
      {var uintB c;
       run_time_stop(); # Run-Time-Stoppuhr anhalten
       begin_system_call();
       {var reg1 int ergebnis = read(stdin_handle,&c,1); # Zeichen lesen versuchen
        end_system_call();
        run_time_restart(); # Run-Time-Stoppuhr weiterlaufen lassen
        if (ergebnis<0)
          { if (errno==EINTR) # Unterbrechung (durch Ctrl-C) ?
              { pushSTACK(S(read_char)); tast_break(); # Break-Schleife aufrufen
                goto restart_it;
              }
            OS_error();
          }
        if (ergebnis==0)
          # kein Zeichen verfügbar -> EOF erkennen
          #if TERMINAL_LINEBUFFERED
          if (TheArray(TheStream(stream)->strm_terminal_inbuff)->dims[1] > 0)
            goto restart_it; # Zeichen des Buffers liefern, dann erst eof_value liefern
            else
          #endif
            { TheStream(stream)->strm_rd_ch_last = eof_value; return eof_value; }
       }
       #if TERMINAL_LINEBUFFERED
       # Zeichen c zur Eingabezeile dazunehmen, evtl. die Zeile vergrößern:
       ssstring_push_extend(TheStream(stream)->strm_terminal_inbuff,c);
       stream = *stream_;
       #endif
       # Wenn stdin und stdout beide dasselbe Terminal sind,
       # und wir lesen ein NL, so können wir davon ausgehen,
       # daß der Cursor danach in Spalte 0 steht.
       if (c==NL)
         { if (eq(TheStream(stream)->strm_terminal_isatty,S(equal)))
             { TheStream(stream)->strm_wr_ch_lpos = Fixnum_0; }
         }
       #if TERMINAL_LINEBUFFERED
         else
         goto continue_line; # so lang weiterlesen, bis ein NL kommt...
       # Kam ein NL, so fangen wir an, die Zeichen des Buffers zu liefern:
       goto restart_it;
       #endif
      }
    }}

# Stellt fest, ob ein Terminal-Stream ein Zeichen verfügbar hat.
# listen_terminal2(stream)
# > stream: Terminal-Stream
# < ergebnis:  0 falls Zeichen verfügbar,
#             -1 falls bei EOF angelangt,
#             +1 falls kein Zeichen verfügbar, aber nicht wegen EOF
  local signean listen_terminal2 (object stream);
  local signean listen_terminal2(stream)
    var reg1 object stream;
    { if (eq(TheStream(stream)->strm_rd_ch_last,eof_value)) # schon EOF ?
        { return signean_minus; }
      if (posfixnum_to_L(TheStream(stream)->strm_terminal_index)
          < TheArray(TheStream(stream)->strm_terminal_inbuff)->dims[1]
         )
        # index<count -> Es sind noch Zeichen im Buffer
        { return signean_null; }
      return listen_handle(stream);
    }

# UP: Löscht bereits eingegebenen interaktiven Input von einem Terminal-Stream.
# clear_input_terminal2(stream);
# > stream: Terminal-Stream
# < ergebnis: TRUE falls Input gelöscht wurde, FALSE sonst
  local boolean clear_input_terminal2 (object stream);
  local boolean clear_input_terminal2(stream)
    var reg1 object stream;
    { if (nullp(TheStream(stream)->strm_terminal_isatty))
        # File -> nichts tun
        { return FALSE; }
      # Terminal
      TheStream(stream)->strm_rd_ch_last = NIL; # gewesenes EOF vergessen
      #if TERMINAL_LINEBUFFERED
      TheStream(stream)->strm_terminal_index = Fixnum_0; # index := 0
      TheArray(TheStream(stream)->strm_terminal_inbuff)->dims[1] = 0; # count := 0
      #endif
      clear_tty_input(stdin_handle);
      pushSTACK(stream);
      while (listen_terminal2(STACK_0) == 0) { read_char(&STACK_0); }
      skipSTACK(1);
      return TRUE;
    }

# UP: Ein Zeichen auf einen Terminal-Stream ausgeben.
# wr_ch_terminal2(&stream,ch);
# > stream: Terminal-Stream
# > ch: auszugebendes Zeichen
  #define wr_ch_terminal2  wr_ch_handle

#ifdef STRM_WR_SS
# UP: Mehrere Zeichen auf einen Terminal-Stream ausgeben.
# wr_ss_terminal2(&stream,string,start,len);
# > stream: Terminal-Stream
# > string: Simple-String
# > start: Startindex
# > len: Anzahl der auszugebenden Zeichen
  #define wr_ss_terminal2  wr_ss_handle
#endif

# UP: Löscht den wartenden Output eines Terminal-Stream.
# clear_output_terminal2(stream);
# > stream: Terminal-Stream
# kann GC auslösen
  #define clear_output_terminal2  clear_output_handle

#endif # HAVE_TERMINAL2

#ifdef HAVE_TERMINAL3

#define TERMINAL_LINEBUFFERED  TRUE
#define TERMINAL_OUTBUFFERED   TRUE

# Unsere eigene Vervollständigungs-Funktion, imitiert completion_matches()
# aus readline.c.
  local uintB** lisp_completion (uintB* text, int start, int end);
  local boolean want_filename_completion;
  extern char* filename_completion_function (char* text, int state); # siehe readline.c
  local uintB** lisp_completion(text,start,end)
    var reg7 uintB* text; # text[0..end-start-1] = the_line[start..end-1]
    var reg8 int start;
    var reg9 int end;
    { # Dies ist eine Callback-Funktion, wir müssen den Stack wieder korrekt
      # setzen:
      #ifdef HAVE_SAVED_STACK
      setSTACK(STACK = saved_STACK);
      #endif
      end_system_call();
      if ((start>=2) && (rl_line_buffer[start-2]=='#') && (rl_line_buffer[start-1]== '\"'))
        # Vervollständigung nach #" beziegt sich auf Filenamen:
        { want_filename_completion = TRUE; return NULL; }
      # (SYS::COMPLETION text start end) aufrufen:
      pushSTACK(asciz_to_string((char*)rl_line_buffer));
      pushSTACK(fixnum((uintL)start));
      pushSTACK(fixnum((uintL)end));
      funcall(S(completion),3);
      want_filename_completion = FALSE;
      begin_system_call();
     {var reg4 object mlist = value1; # Liste der Möglichkeiten
      # Liste von Simple-Strings in mallozierten Array von mallozierten
      # Asciz-Strings umbauen:
      if (nullp(mlist)) { return NULL; }
      {var reg6 uintB** array = malloc((llength(mlist)+1)*sizeof(uintB*));
       if (array==NULL) { return NULL; }
       {var reg5 uintB** ptr = array;
        while (consp(mlist))
          { var reg3 uintC count = TheSstring(Car(mlist))->length;
            var reg2 uintB* ptr1 = &TheSstring(Car(mlist))->data[0];
            var reg1 uintB* ptr2 = malloc((count+1)*sizeof(uintB));
            if (ptr2==NULL) # malloc scheitert -> alles zurückgeben
              { until (ptr==array) { free(*--ptr); }
                free(array);
                return NULL;
              }
            *ptr++ = ptr2;
            dotimesC(count,count, { *ptr2++ = *ptr1++; });
            *ptr2 = '\0';
            mlist = Cdr(mlist);
          }
        *ptr = NULL;
       }
       return array;
    }}}

# Falls obige Funktion NULL (keine Matches) lieferte, wird die folgende
# Funktion so lange aufgerufen, bis sie ihrerseits NULL liefert.
  local char* lisp_completion_more (char* text, int state);
  local char* lisp_completion_more(text,state)
    var reg2 char* text;
    var reg1 int state;
    { if (want_filename_completion)
        { return filename_completion_function(text,state); }
        else
        { return NULL; }
    }

# Lesen eines Zeichens von einem Terminal-Stream.
  local object rd_ch_terminal3 (object* stream_);
  # vgl. rd_ch_handle() :
  local object rd_ch_terminal3(stream_)
    var reg3 object* stream_;
    { restart_it:
     {var reg2 object stream = *stream_;
      if (eq(TheStream(stream)->strm_rd_ch_last,eof_value)) # schon EOF ?
        { return eof_value; }
      #if TERMINAL_LINEBUFFERED
      { var reg4 object inbuff = TheStream(stream)->strm_terminal_inbuff; # Eingabebuffer
        if (posfixnum_to_L(TheStream(stream)->strm_terminal_index)
            < TheArray(inbuff)->dims[1]
           )
          # index<count -> Es sind noch Zeichen im Buffer
          { var reg1 uintL index =
              posfixnum_to_L(TheStream(stream)->strm_terminal_index); # Index
            TheStream(stream)->strm_terminal_index =
              fixnum_inc(TheStream(stream)->strm_terminal_index,1); # Index erhöhen
            return code_char(TheSstring(TheArray(inbuff)->data)->data[index]); # nächstes Character
          }
        # index=count -> muß eine ganze Zeile von Tastatur lesen:
        TheStream(stream)->strm_terminal_index = Fixnum_0; # index := 0
        TheArray(inbuff)->dims[1] = 0; # count := 0
      }
      #endif
      { var reg5 uintB* prompt; # Prompt: letzte bisher ausgegebene Zeile
       {var reg6 object lastline = string_to_asciz(TheStream(*stream_)->strm_terminal_outbuff);
        prompt = malloc(TheSstring(lastline)->length);
        if (!(prompt==NULL)) { strcpy(prompt,TheAsciz(lastline)); }
       }
       # Lexem-trennende Characters: die mit Syntaxcode whsp,tmac,nmac
       # (siehe IO.D, eigentlich von der Readtable abhängig):
       rl_basic_word_break_characters = "\t" NLstring " \"#'(),;`";
       rl_basic_quote_characters = "\"|";
       rl_completer_quote_characters = "\\|";
       run_time_stop(); # Run-Time-Stoppuhr anhalten
       begin_system_call();
       rl_already_prompted = TRUE;
       {var reg4 uintB* line = (uintB*)readline(prompt==NULL ? "" : (char*)prompt); # Zeile lesen
        end_system_call();
        run_time_restart(); # Run-Time-Stoppuhr weiterlaufen lassen
        if (!(prompt==NULL)) { free(prompt); }
        if (line==NULL)
          # EOF (am Zeilenanfang) erkennen
          { TheStream(stream)->strm_rd_ch_last = eof_value; return eof_value; }
        # gelesene Zeile zur Eingabezeile dazunehmen:
        {var reg1 uintB* ptr = line;
         until (*ptr == '\0')
           { ssstring_push_extend(TheStream(*stream_)->strm_terminal_inbuff,*ptr++); }
         ssstring_push_extend(TheStream(*stream_)->strm_terminal_inbuff,NL);
        }
        # und in die History übernehmen, falls nicht leer:
        if (!(line[0]=='\0')) { add_history(line); }
        # Freigeben müssen wir die Zeile!
        free(line);
      }}
      # Wenn stdin und stdout beide dasselbe Terminal sind, können
      # wir davon ausgehen, daß der Cursor in Spalte 0 steht.
      if (eq(TheStream(stream)->strm_terminal_isatty,S(equal)))
        { TheStream(stream)->strm_wr_ch_lpos = Fixnum_0; }
      # Nun fangen wir an, die Zeichen des Buffers zu liefern:
      goto restart_it;
    }}

# Stellt fest, ob ein Terminal-Stream ein Zeichen verfügbar hat.
# listen_terminal3(stream)
# > stream: Terminal-Stream
# < ergebnis:  0 falls Zeichen verfügbar,
#             -1 falls bei EOF angelangt,
#             +1 falls kein Zeichen verfügbar, aber nicht wegen EOF
  local signean listen_terminal3 (object stream);
  local signean listen_terminal3(stream)
    var reg1 object stream;
    { if (eq(TheStream(stream)->strm_rd_ch_last,eof_value)) # schon EOF ?
        { return signean_minus; }
      if (posfixnum_to_L(TheStream(stream)->strm_terminal_index)
          < TheArray(TheStream(stream)->strm_terminal_inbuff)->dims[1]
         )
        # index<count -> Es sind noch Zeichen im Buffer
        { return signean_null; }
      return listen_handle(stream);
    }

# UP: Löscht bereits eingegebenen interaktiven Input von einem Terminal-Stream.
# clear_input_terminal3(stream);
# > stream: Terminal-Stream
# < ergebnis: TRUE falls Input gelöscht wurde, FALSE sonst
  local boolean clear_input_terminal3 (object stream);
  local boolean clear_input_terminal3(stream)
    var reg1 object stream;
    { if (nullp(TheStream(stream)->strm_terminal_isatty))
        # File -> nichts tun
        { return FALSE; }
      # Terminal
      TheStream(stream)->strm_rd_ch_last = NIL; # gewesenes EOF vergessen
      #if TERMINAL_LINEBUFFERED
      TheStream(stream)->strm_terminal_index = Fixnum_0; # index := 0
      TheArray(TheStream(stream)->strm_terminal_inbuff)->dims[1] = 0; # count := 0
      #endif
      clear_tty_input(stdin_handle);
      pushSTACK(stream);
      while (listen_terminal3(STACK_0) == 0) { read_char(&STACK_0); }
      skipSTACK(1);
      return TRUE;
    }

# UP: Ein Zeichen auf einen Terminal-Stream ausgeben.
# wr_ch_terminal3(&stream,ch);
# > stream: Terminal-Stream
# > ch: auszugebendes Zeichen
  local void wr_ch_terminal3 (object* stream_, object ch);
  local void wr_ch_terminal3(stream_,ch)
    var reg3 object* stream_;
    var reg1 object ch;
    { if (!string_char_p(ch)) { fehler_string_char(*stream_,ch); } # ch sollte String-Char sein
     {var uintB c = char_code(ch); # Code des Zeichens
      #if TERMINAL_OUTBUFFERED
      if (c==NL)
        TheArray(TheStream(*stream_)->strm_terminal_outbuff)->dims[1] = 0; # Fill-Pointer := 0
        else
        ssstring_push_extend(TheStream(*stream_)->strm_terminal_outbuff,c);
      #endif
      restart_it:
      begin_system_call();
      {var reg2 int ergebnis = write(stdout_handle,&c,1); # Zeichen auszugeben versuchen
       end_system_call();
       if (ergebnis<0)
         { if (errno==EINTR) # Unterbrechung (durch Ctrl-C) ?
             { pushSTACK(S(write_char)); tast_break(); # Break-Schleife aufrufen
               goto restart_it;
             }
           OS_error(); # Error melden
         }
       if (ergebnis==0) # nicht erfolgreich?
         { fehler_unwritable(S(write_char),*stream_); }
      }
    }}

#ifdef STRM_WR_SS
# UP: Mehrere Zeichen auf einen Terminal-Stream ausgeben.
# wr_ss_terminal3(&stream,string,start,len);
# > stream: Terminal-Stream
# > string: Simple-String
# > start: Startindex
# > len: Anzahl der auszugebenden Zeichen
  local void wr_ss_terminal3 (object* stream_, object string, uintL start, uintL len);
  local void wr_ss_terminal3(stream_,string,start,len)
    var reg6 object* stream_;
    var reg4 object string;
    var reg5 uintL start;
    var reg7 uintL len;
    { if (len==0) return;
     {var reg1 uintB* ptr = &TheSstring(string)->data[start];
      var reg2 uintL remaining = len;
      loop
        { restart_it:
          begin_system_call();
         {var reg3 int ergebnis = write(stdout_handle,ptr,remaining); # Zeichen auszugeben versuchen
          end_system_call();
          if (ergebnis<0)
            { if (errno==EINTR) # Unterbrechung (durch Ctrl-C) ?
                { pushSTACK(S(write_string)); tast_break(); # Break-Schleife aufrufen
                  goto restart_it;
                }
              OS_error(); # Error melden
            }
          if (ergebnis==0) # nicht erfolgreich?
            { fehler_unwritable(S(write_string),*stream_); }
          ptr += ergebnis; remaining -= ergebnis;
          if (remaining==0) break; # fertig?
        }}
      #if TERMINAL_OUTBUFFERED
      # Zeichen seit dem letzten NL in den Buffer:
      { var reg3 uintL pos = 0; # zähle die Zahl der Zeichen seit dem letzten NL
        var reg2 uintL count;
        dotimespL(count,len, { if (*--ptr == NL) goto found_NL; pos++; } );
        if (FALSE)
          found_NL: # pos Zeichen seit dem letzten NL
          { ptr++;
            TheArray(TheStream(*stream_)->strm_terminal_outbuff)->dims[1] = 0; # Fill-Pointer := 0
          }
        dotimesL(count,pos,
          ssstring_push_extend(TheStream(*stream_)->strm_terminal_outbuff,*ptr++);
          );
      }
      #endif
      wr_ss_lpos(*stream_,ptr,len); # Line-Position aktualisieren
    }}
#endif

# UP: Löscht den wartenden Output eines Terminal-Stream.
# clear_output_terminal3(stream);
# > stream: Terminal-Stream
# kann GC auslösen
  local void clear_output_terminal3 (object stream);
  local void clear_output_terminal3(stream)
    var reg1 object stream;
    { clear_output_handle(stream);
      #if TERMINAL_OUTBUFFERED
      TheArray(TheStream(stream)->strm_terminal_outbuff)->dims[1] = 0; # Fill-Pointer := 0
      #endif
    }

#endif # HAVE_TERMINAL3

# UP: Bringt den wartenden Output eines Terminal-Stream ans Ziel.
# finish_output_terminal(stream);
# > stream: Terminal-Stream
# kann GC auslösen
  #define finish_output_terminal  finish_output_handle

# UP: Bringt den wartenden Output eines Terminal-Stream ans Ziel.
# force_output_terminal(stream);
# > stream: Terminal-Stream
# kann GC auslösen
  #define force_output_terminal  force_output_handle

# Liefert einen interaktiven Terminal-Stream.
# kann GC auslösen
  local object make_terminal_stream (void);
  local object make_terminal_stream()
    {
     #ifdef AMIGAOS
      # nur HAVE_TERMINAL1
      { pushSTACK(allocate_handle(Output_handle));
        pushSTACK(allocate_handle(Input_handle));
       {var reg2 object stream =
          allocate_stream(strmflags_ch_B,strmtype_terminal,strm_terminal1_len);
        # Flags: nur READ-CHAR und WRITE-CHAR erlaubt
        # und füllen:
        var reg1 Stream s = TheStream(stream);
          s->strm_rd_by = P(rd_by_dummy); # READ-BYTE unmöglich
          s->strm_wr_by = P(wr_by_dummy); # WRITE-BYTE unmöglich
          s->strm_rd_ch = P(rd_ch_terminal1); # READ-CHAR-Pseudofunktion
          s->strm_rd_ch_last = NIL; # Lastchar := NIL
          s->strm_wr_ch = P(wr_ch_terminal1); # WRITE-CHAR-Pseudofunktion
          s->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
          #ifdef STRM_WR_SS
          s->strm_wr_ss = P(wr_ss_terminal1);
          #endif
          begin_system_call();
          s->strm_terminal_isatty =
            (IsInteractive(Input_handle)
              ? (IsInteractive(Output_handle)
                  ? S(equal) # Input und Output Terminals -> vermutlich dasselbe
                  : T
                )
              : NIL
            );
          end_system_call();
          s->strm_terminal_ihandle = popSTACK();
          s->strm_terminal_ohandle = popSTACK();
        return stream;
      }}
     #else
      { var reg7 int stdin_tty;
        var reg8 int stdout_tty;
        var reg6 int same_tty;
        begin_system_call();
        stdin_tty = isatty(stdin_handle); # stdin ein Terminal?
        stdout_tty = isatty(stdout_handle); # stdout ein Terminal?
        same_tty = FALSE; # vorläufig
        if (stdin_tty && stdout_tty)
          # stdin und stdout Terminals.
          {
           #if defined(UNIX) || defined(VMS)
            var reg1 char* ergebnis;
            var reg2 object filename;
            ergebnis = ttyname(stdin_handle); # Filename von stdin holen
            if (!(ergebnis==NULL))
              { end_system_call();
                filename = asciz_to_string(ergebnis);
                begin_system_call();
                ergebnis = ttyname(stdout_handle); # Filename von stdout holen
                if (!(ergebnis==NULL))
                  { end_system_call();
                    pushSTACK(filename);
                    filename = asciz_to_string(ergebnis);
                    if (string_gleich(popSTACK(),filename)) # gleiche Filenamen?
                      { same_tty = TRUE; }
              }   }
           #endif
           #ifdef MSDOS
            if (   ((get_handle_info(stdin_handle) & (bit(7)|bit(0))) == (bit(7)|bit(0))) # stdin == console_input ?
                && ((get_handle_info(stdout_handle) & (bit(7)|bit(1))) == (bit(7)|bit(1))) # stdout == console_output ?
               )
              { same_tty = TRUE; }
           #endif
          }
        end_system_call();
        #ifdef HAVE_TERMINAL3
        if (rl_present_p && same_tty)
          # Baue einen TERMINAL3-Stream:
          { pushSTACK(make_ssstring(80)); # Zeilenbuffer allozieren
            pushSTACK(make_ssstring(80)); # Zeilenbuffer allozieren
            # neuen Stream allozieren:
           {var reg2 object stream =
              allocate_stream(strmflags_ch_B,strmtype_terminal,strm_terminal3_len);
              # Flags: nur READ-CHAR und WRITE-CHAR erlaubt
            # und füllen:
            var reg1 Stream s = TheStream(stream);
              s->strm_rd_by = P(rd_by_dummy); # READ-BYTE unmöglich
              s->strm_wr_by = P(wr_by_dummy); # WRITE-BYTE unmöglich
              s->strm_rd_ch = P(rd_ch_terminal3); # READ-CHAR-Pseudofunktion
              s->strm_rd_ch_last = NIL; # Lastchar := NIL
              s->strm_wr_ch = P(wr_ch_terminal3); # WRITE-CHAR-Pseudofunktion
              s->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
              #ifdef STRM_WR_SS
              s->strm_wr_ss = P(wr_ss_terminal3);
              #endif
              s->strm_terminal_isatty = S(equal); # stdout=stdin
              s->strm_terminal_ihandle = allocate_handle(stdin_handle); # Handle für listen_handle()
              s->strm_terminal_ohandle = allocate_handle(stdout_handle); # Handle für Output
              #if 1 # TERMINAL_LINEBUFFERED
              s->strm_terminal_inbuff = popSTACK(); # Zeilenbuffer eintragen, count := 0
              s->strm_terminal_index = Fixnum_0; # index := 0
              #endif
              #if 1 # TERMINAL_OUTBUFFERED
              s->strm_terminal_outbuff = popSTACK(); # Zeilenbuffer eintragen
              #endif
            return stream;
          }}
        #endif
        #ifdef HAVE_TERMINAL2
        if (stdin_tty)
          # Baue einen TERMINAL2-Stream:
          { pushSTACK(make_ssstring(80)); # Zeilenbuffer allozieren
            # neuen Stream allozieren:
           {var reg2 object stream =
              allocate_stream(strmflags_ch_B,strmtype_terminal,strm_terminal2_len);
              # Flags: nur READ-CHAR und WRITE-CHAR erlaubt
            # und füllen:
            var reg1 Stream s = TheStream(stream);
              s->strm_rd_by = P(rd_by_dummy); # READ-BYTE unmöglich
              s->strm_wr_by = P(wr_by_dummy); # WRITE-BYTE unmöglich
              s->strm_rd_ch = P(rd_ch_terminal2); # READ-CHAR-Pseudofunktion
              s->strm_rd_ch_last = NIL; # Lastchar := NIL
              s->strm_wr_ch = P(wr_ch_terminal2); # WRITE-CHAR-Pseudofunktion
              s->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
              #ifdef STRM_WR_SS
              s->strm_wr_ss = P(wr_ss_terminal2);
              #endif
              s->strm_terminal_isatty = (stdin_tty ? (same_tty ? S(equal) : T) : NIL);
              s->strm_terminal_ihandle = allocate_handle(stdin_handle); # Handle für listen_handle()
              s->strm_terminal_ohandle = allocate_handle(stdout_handle); # Handle für Output
              #if 1 # TERMINAL_LINEBUFFERED
              s->strm_terminal_inbuff = popSTACK(); # Zeilenbuffer eintragen, count := 0
              s->strm_terminal_index = Fixnum_0; # index := 0
              #endif
            return stream;
          }}
        #endif
        # Baue einen TERMINAL1-Stream:
        { # neuen Stream allozieren:
          var reg2 object stream =
            allocate_stream(strmflags_ch_B,strmtype_terminal,strm_terminal1_len);
            # Flags: nur READ-CHAR und WRITE-CHAR erlaubt
          # und füllen:
          var reg1 Stream s = TheStream(stream);
            s->strm_rd_by = P(rd_by_dummy); # READ-BYTE unmöglich
            s->strm_wr_by = P(wr_by_dummy); # WRITE-BYTE unmöglich
            s->strm_rd_ch = P(rd_ch_terminal1); # READ-CHAR-Pseudofunktion
            s->strm_rd_ch_last = NIL; # Lastchar := NIL
            s->strm_wr_ch = P(wr_ch_terminal1); # WRITE-CHAR-Pseudofunktion
            s->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
            #ifdef STRM_WR_SS
            s->strm_wr_ss = P(wr_ss_terminal1);
            #endif
            s->strm_terminal_isatty = (stdin_tty ? (same_tty ? S(equal) : T) : NIL);
            s->strm_terminal_ihandle = allocate_handle(stdin_handle); # Handle für listen_handle()
            s->strm_terminal_ohandle = allocate_handle(stdout_handle); # Handle für Output
          return stream;
        }
      }
     #endif
    }

#if defined(UNIX) || defined(VMS)

# (SYS::TERMINAL-RAW *terminal-io* flag)
# flag /= NIL: versetzt das Terminal in cbreak/noecho-Modus,
# flag = NIL: versetzt das Terminal in nocbreak/echo-Modus zurück.

# (SYS::TERMINAL-RAW *terminal-io* t) entspricht im wesentlichen
# (progn
#   ; keine Editiermöglichkeiten, kein Echo, keine CR<-->NL-Umwandlungen:
#   (shell "stty -icanon -echo -icrnl -inlcr")
#   ; nichts abfangen:
#   ;              C-S   C-Q      Del     C-U       C-W      C-R      C-O      C-V     C-Y     C-C     C-\      C-Q     C-S    C-D
#   (shell "stty -ixon -ixoff erase ^- kill ^- werase ^- rprnt ^- flush ^- lnext ^- susp ^- intr ^- quit ^- start ^- stop ^- eof ^-")
#   ; 1 Zeichen auf einmal verlangen (nicht 4!):
#   (shell "stty min 1") ; das muß seltsamerweise zuletzt kommen...
# )
# (SYS::TERMINAL-RAW *terminal-io* nil) entspricht im wesentlichen
# (shell "stty sane")

local void term_raw (void);
local void term_unraw (void);

local boolean oldterm_initialized = FALSE;

#if defined(UNIX_TERM_TERMIOS)
  local struct termios oldtermio; # ursprünglicher TTY-Modus
  local void term_raw()
    { if (!oldterm_initialized)
        { if (!( tcgetattr(stdout_handle,&oldtermio) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
          oldterm_initialized = TRUE;
        }
     {var struct termios newtermio;
      newtermio = oldtermio;
      newtermio.c_iflag &= ( /* IXON|IXOFF|IXANY| */ ISTRIP|IGNBRK);
      /* newtermio.c_oflag &= ~OPOST; */ # Curses stört sich dran!
      newtermio.c_lflag &= ISIG;
      { var reg1 uintC i;
        for (i=0; i<NCCS; i++) { newtermio.c_cc[i] = 0; }
      }
      newtermio.c_cc[VMIN] = 1;
      newtermio.c_cc[VTIME] = 0;
      if (!( tcsetattr(stdout_handle,TCSAFLUSH,&newtermio) ==0))
        { if (!(errno==ENOTTY)) { OS_error(); } }
    }}
  local void term_unraw()
    { if (oldterm_initialized)
        { if (!( tcsetattr(stdout_handle,TCSAFLUSH,&oldtermio) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
    }   }
  # Manche machen's so:
    # define crmode()    (_tty.c_lflag &=~ICANON,_tty.c_cc[VMIN]=1,tcsetattr(_tty_ch, TCSAFLUSH, &_tty))
    # define nocrmode()  (_tty.c_lflag |= ICANON,_tty.c_cc[VEOF] = CEOF,tcsetattr(_tty_ch, TCSAFLUSH,&_tty))
    # define echo()      (_tty.c_lflag |= ECHO, tcsetattr(_tty_ch, TCSAFLUSH, &_tty))
    # define noecho()    (_tty.c_lflag &=~ECHO, tcsetattr(_tty_ch, TCSAFLUSH, &_tty))
    # define nl()        (_tty.c_iflag |= ICRNL,_tty.c_oflag |= ONLCR,tcsetattr(_tty_ch, TCSAFLUSH, &_tty))
    # define nonl()      (_tty.c_iflag &=~ICRNL,_tty.c_oflag &=~ONLCR,tcsetattr(_tty_ch, TCSAFLUSH, &_tty))
    # define savetty()   (tcgetattr(_tty_ch, &_oldtty),tcgetattr(_tty_ch, &_tty))
    # define resetty()   (tcsetattr(_tty_ch, TCSAFLUSH, &_oldtty))
#elif defined(UNIX_TERM_TERMIO) || defined(EMUNIX)
  local struct termio oldtermio; # ursprünglicher TTY-Modus
  local void term_raw()
    { if (!oldterm_initialized)
        { if (!( ioctl(stdout_handle,TCGETA,&oldtermio) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
          oldterm_initialized = TRUE;
        }
     {var struct termio newtermio;
      newtermio = oldtermio;
      newtermio.c_iflag &= ( /* IXON|IXOFF|IXANY| */ ISTRIP|IGNBRK);
      /* newtermio.c_oflag &= ~OPOST; */ # Curses stört sich dran!
      newtermio.c_lflag &= ISIG;
      { var reg1 uintC i;
        for (i=0; i<NCCS; i++) { newtermio.c_cc[i] = 0; }
      }
      newtermio.c_cc[VMIN] = 1;
      newtermio.c_cc[VTIME] = 0;
      if (!( ioctl(stdout_handle,TCSETAF,&newtermio) ==0))
        { if (!(errno==ENOTTY)) { OS_error(); } }
    }}
  local void term_unraw()
    { if (oldterm_initialized)
        { if (!( ioctl(stdout_handle,TCSETAF,&oldtermio) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
    }   }
  # Manche machen's so:
    # define crmode()    (_tty.c_lflag &=~ICANON,_tty.c_cc[VMIN] = 1,ioctl(_tty_ch,TCSETAF,&_tty))
    # define nocrmode()  (_tty.c_lflag |= ICANON,_tty.c_cc[VEOF] = CEOF,stty(_tty_ch,&_tty))
    # define echo()      (_tty.c_lflag |= ECHO, ioctl(_tty_ch, TCSETA, &_tty))
    # define noecho()    (_tty.c_lflag &=~ECHO, ioctl(_tty_ch, TCSETA, &_tty))
    # define nl()        (_tty.c_iflag |= ICRNL,_tty.c_oflag |= ONLCR,ioctl(_tty_ch, TCSETAW, &_tty))
    # define nonl()      (_tty.c_iflag &=~ICRNL,_tty.c_oflag &=~ONLCR,ioctl(_tty_ch, TCSETAW, &_tty))
#elif defined(UNIX_TERM_SGTTY)
  local struct sgttyb oldsgttyb; # ursprünglicher TTY-Modus
  local struct tchars oldtchars; # ursprüngliche Steuerzeichen
  #ifdef TIOCSLTC
  local struct ltchars oldltchars; # ursprüngliche Editierzeichen
  #endif
  local void term_raw()
    { if (!oldterm_initialized)
        { if (!( ioctl(stdout_handle,TIOCGETP,&oldsgttyb) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
          if (!( ioctl(stdout_handle,TIOCGETC,&oldtchars) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
          #ifdef TIOCSLTC
          if (!( ioctl(stdout_handle,TIOCGLTC,&oldltchars) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
          #endif
          oldterm_initialized = TRUE;
        }
     {var struct sgttyb newsgttyb;
      newsgttyb = oldsgttyb;
      newsgttyb.sg_flags |= CBREAK;
      newsgttyb.sg_flags &= ~(CRMOD|ECHO|XTABS);
      if (!( ioctl(stdout_handle,TIOCSETP,&newsgttyb) ==0))
        { if (!(errno==ENOTTY)) { OS_error(); } }
     }
     {var struct tchars newtchars;
      local var union { char a [sizeof(struct tchars)];
                        struct tchars b;
                      }
                zero_tchars = {{0,}};
      newtchars = zero_tchars.b;
      if (!( ioctl(stdout_handle,TIOCSETC,&newtchars) ==0))
        { if (!(errno==ENOTTY)) { OS_error(); } }
     }
     #ifdef TIOCSLTC
     {var struct ltchars newltchars;
      local var union { char a [sizeof(struct ltchars)];
                        struct ltchars b;
                      }
                zero_ltchars = {{0,}};
      newltchars = zero_ltchars.b;
      if (!( ioctl(stdout_handle,TIOCSLTC,&newltchars) ==0))
        { if (!(errno==ENOTTY)) { OS_error(); } }
     }
     #endif
    }
  local void term_unraw()
    { if (oldterm_initialized)
        { if (!( ioctl(stdout_handle,TIOCSETP,&oldsgttyb) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
          if (!( ioctl(stdout_handle,TIOCSETC,&oldtchars) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
          #ifdef TIOCSLTC
          if (!( ioctl(stdout_handle,TIOCSLTC,&oldltchars) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
          #endif
    }   }
  # Manche machen's so:
    # define raw()       (_tty.sg_flags|=RAW, stty(_tty_ch,&_tty))
    # define noraw()     (_tty.sg_flags&=~RAW,stty(_tty_ch,&_tty))
    # define crmode()    (_tty.sg_flags |= CBREAK, stty(_tty_ch,&_tty))
    # define nocrmode()  (_tty.sg_flags &= ~CBREAK,stty(_tty_ch,&_tty))
    # define echo()      (_tty.sg_flags |= ECHO, stty(_tty_ch, &_tty))
    # define noecho()    (_tty.sg_flags &= ~ECHO, stty(_tty_ch, &_tty))
    # define nl()        (_tty.sg_flags |= CRMOD,stty(_tty_ch, &_tty))
    # define nonl()      (_tty.sg_flags &= ~CRMOD, stty(_tty_ch, &_tty))
    # define savetty()   (gtty(_tty_ch, &_tty), _res_flg = _tty.sg_flags)
    # define resetty()   (_tty.sg_flags = _res_flg, stty(_tty_ch, &_tty))
#elif defined(VMS)
  #include <curses.h>
  local void term_raw()
    { raw(); noecho(); }
  local void term_unraw()
    { noraw(); echo(); }
  # Funktioniert das, oder muß man dazu getch() statt read() verwenden??
#endif

LISPFUNN(terminal_raw,2)
  { var reg2 object flag = popSTACK();
    var reg1 object stream = popSTACK();
    if (!streamp(stream)) { fehler_stream(stream); }
    if (TheStream(stream)->strmtype == strmtype_terminal)
      # der Terminal-Stream
      { if (!nullp(TheStream(stream)->strm_terminal_isatty))
          # Terminal
          { if (!nullp(flag))
              # Umschalten in cbreak/noecho-Modus:
              { term_raw(); }
              else
              # Umschalten in nocbreak/echo-Modus:
              { term_unraw(); }
      }   }
    value1 = NIL; mv_count=1;
  }

#endif # UNIX || VMS

#endif # UNIX || DJUNIX || EMUNIX || AMIGAOS || VMS


# Window-Stream
# =============

#ifdef WINDOWS

# Editor-Unterstützung:
# ATARI: Direkt mit dem BIOS-VT52-Emulator.
# MSDOS: Übers BIOS.
# OS/2: Mit der Video-Library von Eberhard Mattes.
# CURSES: Ein Window-Stream ist im wesentlichen ein Curses-WINDOW.
#
# (SYSTEM::MAKE-WINDOW)
#   liefert einen Window-Stream. Solange bis dieser wieder geschlossen wird,
#   ist das Terminal im cbreak-noecho-Modus; weitere Ein-/Ausgabe über
#   *terminal-io* sollte in dieser Zeit nicht erfolgen.
#
# (SYSTEM::WINDOW-SIZE window-stream)
#   liefert die Größe des Windows,
#   als 2 Werte: Höhe (= Ymax+1), Breite (= Xmax+1).
#
# (SYSTEM::WINDOW-CURSOR-POSITION window-stream)
#   liefert die Position des Cursors im Window
#   als 2 Werte: Zeile (>=0, <=Ymax, 0=oben), Spalte (>=0, <=Xmax, 0=links).
#
# (SYSTEM::SET-WINDOW-CURSOR-POSITION window-stream line column)
#   setzt die Position des Cursors im Window.
#
# (SYSTEM::CLEAR-WINDOW window-stream)
#   löscht den Inhalt des Window und setzt den Cursor an die linke obere Ecke
#
# (SYSTEM::CLEAR-WINDOW-TO-EOT window-stream)
#   löscht den Inhalt des Window ab Cursor-Position bis Bildschirmende
#
# (SYSTEM::CLEAR-WINDOW-TO-EOL window-stream)
#   löscht den Inhalt des Window ab Cursor-Position bis Zeilenende
#
# (SYSTEM::DELETE-WINDOW-LINE window-stream)
#   löscht die Cursorzeile, schiebt die Zeilen drunter um 1 nach oben
#   und löscht die letzte Bildschirmzeile.
#
# (SYSTEM::INSERT-WINDOW-LINE window-stream)
#   fügt in der Zeile des Cursors eine neue Zeile ein und schiebt dabei alle
#   Zeilen ab der Cursorzeile um 1 nach unten.
#
# (SYSTEM::HIGHLIGHT-ON window-stream)
#   schaltet "hervorgehobene" Ausgabe ein.
#
# (SYSTEM::HIGHLIGHT-OFF window-stream)
#   schaltet "hervorgehobene" Ausgabe wieder aus.
#
# (SYSTEM::WINDOW-CURSOR-ON window-stream)
#   macht den Cursor(block) sichtbar.
#
# (SYSTEM::WINDOW-CURSOR-OFF window-stream)
#   macht den Cursor(block) wieder unsichtbar.

# Überprüft, ob das Argument ein Window-Stream ist.
  local void check_window_stream (object stream);
  local void check_window_stream(stream)
    var reg1 object stream;
    { if (streamp(stream)
          && (TheStream(stream)->strmtype == strmtype_window)
         )
        return;
      pushSTACK(stream);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(
             DEUTSCH ? "~: Argument ~ sollte ein Window-Stream sein." :
             ENGLISH ? "~: argument ~ should be a window stream" :
             FRANCAIS ? "~ : L'argument ~ devrait être un WINDOW-STREAM." :
             ""
            );
    }

#ifdef ATARI

# Terminal-Emulation:
# VT52 mit einigen Erweiterungen,
# vgl. TERMCAP-Einträge "vt52", "atari st", "heathkit 19"

# UP: Ein Zeichen auf einen Window-Stream ausgeben.
# wr_ch_window(&stream,ch);
# > stream: Window-Stream
# > ch: auszugebendes Zeichen
  local void wr_ch_window (object* stream_, object ch);
  local void wr_ch_window(stream_,ch)
    var reg2 object* stream_;
    var reg3 object ch;
    { if (!string_char_p(ch)) { fehler_string_char(*stream_,ch); } # ch sollte String-Char sein
     {var reg1 uintB c = char_code(ch); # Code des Zeichens
      # Code c übers BIOS auf den Bildschirm ausgeben:
      # c >= ' ' -> übers GEMDOS
      # 0 <= c < 32 -> genau c in {1,..,4}u{14,..,25}u{28,..,31} übers BIOS
      #                sonst als Steuerzeichen übers BIOS
      if ((c < ' ') &&
          # Bit c aus der 32-Bit-Zahl %11110011111111111100000000011110 holen:
          (0xF3FFC01EUL & bit(c))
         )
        { BIOS_GrConOut(c); }
        else
        { BIOS_ConOut(c); }
    }}

LISPFUNN(make_window,0)
  { finish_output_terminal(var_stream(S(terminal_io))); # evtl. wartendes NL jetzt ausgeben
   {var reg2 object stream =
      allocate_stream(strmflags_wr_ch_B,strmtype_window,strm_len+0);
      # Flags: nur WRITE-CHAR erlaubt
    # und füllen:
    var reg1 Stream s = TheStream(stream);
      s->strm_rd_by = P(rd_by_dummy); # READ-BYTE unmöglich
      s->strm_wr_by = P(wr_by_dummy); # WRITE-BYTE unmöglich
      s->strm_rd_ch = P(rd_ch_dummy); # READ-CHAR unmöglich
      s->strm_rd_ch_last = NIL; # Lastchar := NIL
      s->strm_wr_ch = P(wr_ch_window); # WRITE-CHAR-Pseudofunktion
      s->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
      #ifdef STRM_WR_SS
      s->strm_wr_ss = P(wr_ss_dummy_nogc);
      #endif
    set_break_sem_1();
    BIOS_ConOut(ESC); BIOS_ConOut('w'); # Wrap off
    BIOS_ConOut(ESC); BIOS_ConOut('q'); # Reverse off
    clr_break_sem_1();
    value1 = stream; mv_count=1;
  }}

# Schließt einen Window-Stream.
  local void close_window (object stream);
  local void close_window(stream)
    var reg1 object stream;
    { set_break_sem_1();
      BIOS_ConOut(ESC); BIOS_ConOut('v'); # Wrap on
      BIOS_ConOut(ESC); BIOS_ConOut('q'); # Reverse off
      clr_break_sem_1();
    }

LISPFUNN(window_size,1)
  { check_window_stream(popSTACK());
    value1 = fixnum(window_size.y);
    value2 = fixnum(window_size.x);
    mv_count=2;
  }

LISPFUNN(window_cursor_position,1)
  { check_window_stream(popSTACK());
    value1 = fixnum((UWORD)(vdiesc.v_cur_y));
    value2 = fixnum((UWORD)(vdiesc.v_cur_x));
    mv_count=2;
  }

LISPFUNN(set_window_cursor_position,3)
  { check_window_stream(STACK_2);
   {var reg2 uintL line = posfixnum_to_L(STACK_1);
    var reg3 uintL column = posfixnum_to_L(STACK_0);
    if ((line < (uintL)window_size.y) && (column < (uintL)window_size.x))
      { set_break_sem_1();
        BIOS_ConOut(ESC); BIOS_ConOut('Y'); BIOS_ConOut(32+line); BIOS_ConOut(32+column);
        clr_break_sem_1();
      }
    value1 = STACK_1; value2 = STACK_0; mv_count=2; skipSTACK(3);
  }}

LISPFUNN(clear_window,1)
  { check_window_stream(popSTACK());
    set_break_sem_1();
    BIOS_ConOut(ESC); BIOS_ConOut('E');
    clr_break_sem_1();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(clear_window_to_eot,1)
  { check_window_stream(popSTACK());
    set_break_sem_1();
    BIOS_ConOut(ESC); BIOS_ConOut('J');
    clr_break_sem_1();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(clear_window_to_eol,1)
  { check_window_stream(popSTACK());
    set_break_sem_1();
    BIOS_ConOut(ESC); BIOS_ConOut('K');
    clr_break_sem_1();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(delete_window_line,1)
  { check_window_stream(popSTACK());
    set_break_sem_1();
    BIOS_ConOut(ESC); BIOS_ConOut('M');
    clr_break_sem_1();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(insert_window_line,1)
  { check_window_stream(popSTACK());
    set_break_sem_1();
    BIOS_ConOut(ESC); BIOS_ConOut('L');
    clr_break_sem_1();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(highlight_on,1)
  { check_window_stream(popSTACK());
    set_break_sem_1();
    BIOS_ConOut(ESC); BIOS_ConOut('p'); # Reverse on
    clr_break_sem_1();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(highlight_off,1)
  { check_window_stream(popSTACK());
    set_break_sem_1();
    BIOS_ConOut(ESC); BIOS_ConOut('q'); # Reverse off
    clr_break_sem_1();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(window_cursor_on,1)
  { check_window_stream(popSTACK());
    set_break_sem_1();
    BIOS_ConOut(ESC); BIOS_ConOut('e'); # Cursor on
    clr_break_sem_1();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(window_cursor_off,1)
  { check_window_stream(popSTACK());
    set_break_sem_1();
    BIOS_ConOut(ESC); BIOS_ConOut('f'); # Cursor off
    clr_break_sem_1();
    value1 = NIL; mv_count=0;
  }

#endif # ATARI

#if defined(MSDOS) && !defined(EMUNIX_PORTABEL)

# Aus der Distribution von ELVIS 1.4, File PC.C :

# Author:
#      Guntram Blohm
#      Buchenstraße 19
#      W 7904 Erbach
#      Germany
#      Tel. ++49-7305-6997
#      sorry - no regular network connection

# This file implements the ibm pc bios interface. See IBM documentation
# for details.
# If TERM is set upon invocation of CLISP, this code is ignored completely,
# and the standard termcap functions are used, thus, even not-so-close
# compatibles can run CLISP. For close compatibles however, bios output
# is much faster (and permits reverse scrolling, adding and deleting lines,
# and much more ansi.sys isn't capable of). GB.

local uintL screentype; # 0 = monochrome, 1 = color

local uintW screenattr; # screen attribute index

# Documentation of attributes:
# bit 7    : foreground character blinking,
# bit 6..4 : background color,
# bit 3    : foreground intensity,
# bit 2..0 : foreground color,
# color table:
#   0 black, 1 blue, 2 green, 3 cyan, 4 red, 5 magenta, 6 brown, 7 lightgray,
# and as foreground color with intensity bit set, it is light:
#   8 darkgray, ..., E yelloe, F white.
  #define col_black    0  # schwarz
  #define col_blue     1  # blau
  #define col_green    2  # grün
  #define col_cyan     3  # blaugrün
  #define col_red      4  # rot
  #define col_magenta  5  # lila
  #define col_brown    6  # braun 
  #define col_white    7  # weiß
  #define col_light(x)  (8 | x)  # hell
  #define FG(x)  x         # foreground color
  #define BG(x)  (x << 4)  # background color
local uintB attr_table[2][5] =
  { # monochrome:
    { /* no standout   */  BG(col_black) | FG(col_white),
      /* standout      */  BG(col_white) | FG(col_black),
      /* visible bell  */  BG(col_black) | FG(col_light(col_white)),
      /* underline     */  BG(col_black) | FG(1), # underline
      /* alt. char set */  BG(col_black) | FG(col_light(col_white)),
    },
    # color:
    { /* no standout   */  BG(col_blue) | FG(col_light(col_white)),
      /* standout      */  BG(col_blue) | FG(col_light(col_magenta)),
      /* visible bell  */  BG(col_blue) | FG(col_light(col_brown)),
      /* underline     */  BG(col_blue) | FG(col_light(col_green)),
      /* alt. char set */  BG(col_blue) | FG(col_light(col_red)),
    },
  };
local uintB attr; # = attr_table[screentype][screenattr];

# INT 10 documentation:
#   INT 10,01 - Set cursor type
#   INT 10,02 - Set cursor position
#   INT 10,03 - Read cursor position
#   INT 10,06 - Scroll active page up
#   INT 10,07 - Scroll active page down
#   INT 10,09 - Write character and attribute at cursor
#   INT 10,0E - Write text in teletype mode
#   INT 10,0F - Get current video state
#
# INT 10,01 - Set Cursor Type
#     AH = 01
#     CH = cursor starting scan line (cursor top) (low order 5 bits)
#     CL = cursor ending scan line (cursor bottom) (low order 5 bits)
#     returns nothing
#     - cursor scan lines are zero based
#     - the following is a list of the cursor scan lines associated with
#       most common adapters;  screen sizes over 40 lines may differ
#       depending on adapters.
#               Line     Starting     Ending      Character
#       Video   Count    Scan Line    Scan Line   Point Size
#       CGA      25         06           07           08
#       MDA      25         0B           0C           0E
#       EGA      25         06           07           0E
#       EGA      43       04/06          07           08
#       VGA      25         0D           0E           10
#       VGA      40         08           09           0A
#       VGA      50         06           07           08
#     - use CX = 2000h to disable cursor
#
# INT 10,02 - Set Cursor Position
#     AH = 02
#     BH = page number (0 for graphics modes)
#     DH = row
#     DL = column
#     returns nothing
#     - positions relative to 0,0 origin
#     - 80x25 uses coordinates 0,0 to 24,79;  40x25 uses 0,0 to 24,39
#
# INT 10,03 - Read Cursor Position and Size
#     AH = 03
#     BH = video page
#     on return:
#     CH = cursor starting scan line (low order 5 bits)
#     CL = cursor ending scan line (low order 5 bits)
#     DH = row
#     DL = column
#
# INT 10,06 - Scroll Window Up
#     AH = 06
#     AL = number of lines to scroll, previous lines are
#          blanked, if 0 or AL > screen size, window is blanked
#     BH = attribute to be used on blank line
#     CH = row of upper left corner of scroll window
#     CL = column of upper left corner of scroll window
#     DH = row of lower right corner of scroll window
#     DL = column of lower right corner of scroll window
#     returns nothing
#     - in video mode 4 (300x200 4 color) on the EGA, MCGA and VGA
#       this function scrolls page 0 regardless of the current page
#
# INT 10,07 - Scroll Window Down
#     AH = 07
#     AL = number of lines to scroll, previous lines are
#          blanked, if 0 or AL > screen size, window is blanked
#     BH = attribute to be used on blank line
#     CH = row of upper left corner of scroll window
#     CL = column of upper left corner of scroll window
#     DH = row of lower right corner of scroll window
#     DL = column of lower right corner of scroll window
#     returns nothing
#     - in video mode 4 (300x200 4 color) on the EGA, MCGA and VGA
#       this function scrolls page 0 regardless of the current page
#
# INT 10,09 - Write Character and Attribute at Cursor Position
#     AH = 09
#     AL = ASCII character to write
#     BH = display page  (or mode 13h, background pixel value)
#     BL = character attribute (text) foreground color (graphics)
#     CX = count of characters to write (CX >= 1)
#     returns nothing
#     - does not move the cursor
#     - in graphics mode (except mode 13h), if BL bit 7=1 then
#       value of BL is XOR'ed with the background color
#
# INT 10,0E - Write Text in Teletype Mode
#     AH = 0E
#     AL = ASCII character to write
#     BH = page number (text modes)
#     BL = foreground pixel color (graphics modes)
#     returns nothing
#     - cursor advances after write
#     - characters BEL (7), BS (8), LF (A), and CR (D) are
#       treated as control codes
#     - for some older BIOS (10/19/81), the BH register must point
#       to the currently displayed page
#     - on CGA adapters this function can disable the video signal while
#       performing the output which causes flitter.
#
# INT 10,0F - Get Video State
#     AH = 0F
#     on return:
#     AH = number of screen columns
#     AL = mode currently set (see ~VIDEO MODES~)
#     BH = current display page
#     - video modes greater than 13h on EGA, MCGA and VGA indicate
#       ~INT 10,0~ was called with the high bit of the mode (AL) set
#       to 1, meaning the display does not need cleared

# low-level BIOS interface

#ifdef DJUNIX
  #define intvideo(in_ptr,out_ptr)  int86(0x10,in_ptr,out_ptr)
#endif
#ifdef EMUNIX
  local void intvideo (union REGS * in_regs, union REGS * out_regs);
  local void intvideo(in_regs,out_regs)
    var register union REGS * in_regs;
    var register union REGS * out_regs;
    { __asm__ __volatile__ ( "movl 0(%%esi),%%eax ; "
                             "movl 4(%%esi),%%ebx ; "
                             "movl 8(%%esi),%%ecx ; "
                             "movl 12(%%esi),%%edx ; "
                             "pushl %%edi ; "
                             ".byte 0xcd ; .byte 0x10 ; "
                             "popl %%edi ; "
                             "movl %%eax,0(%%edi) ; "
                             "movl %%ebx,4(%%edi) ; "
                             "movl %%ecx,8(%%edi) ; "
                             "movl %%edx,12(%%edi)"
                             :                                                         # OUT
                             : "S" /* %esi */ (in_regs), "D" /* %edi */ (out_regs)     # IN
                             : "ax","bx","cx","si","di" /* %eax,%ebx,%ecx,%esi,%edi */ # CLOBBER
                           );
    }
#endif

local void video (uintW ax, uintW* cx, uintW* dx);
local void video(ax,cx,dx)
  var reg1 uintW ax;
  var reg1 uintW* cx;
  var reg1 uintW* dx;
  { var union REGS in;
    var union REGS out;
    in.x.ax = ax;
    { var uintB ah = in.h.ah;
      if (ah==0x06 || ah==0x07)
        { in.h.bh = attr; }
        else
        { in.h.bh = 0; # "active page"
          if (ah==0x09 || ah==0x0e) { in.h.bl = attr; }
    }   }
    if (cx) { in.x.cx = *cx; }
    if (dx) { in.x.dx = *dx; }
    begin_system_call();
    intvideo(&in,&out);
    end_system_call();
    if (dx) { *dx = out.x.dx; }
    if (cx) { *cx = out.x.cx; }
  }

global uintW v_cols()
  { # determine number of screen columns. Also set screentype according
    # to monochrome/color screen.
    var union REGS in;
    var union REGS out;
    in.h.ah=0x0f;
    intvideo(&in,&out); # INT 10,0F : get current video state
   {var uintB reg1 videomode = out.h.al & 0x7f;
    # Text modes are 0,1,2,3,7, and others (depending on the graphics card).
    # Only modes 0 and 7 are mono. (Well, mode 2 is gray shaded.)
    screentype = (((videomode==0) || (videomode==7))
                  ? 0 # monochrome
                  : 1 # color
                 );
    return out.h.ah;
  }}

local uintW v_rows()
  { # Getting the number of rows is hard. Most screens support 25 only,
    # EGA/VGA also support 43/50 lines, and some OEM's even more.
    # Unfortunately, there is no standard bios variable for the number
    # of lines, and the bios screen memory size is always rounded up
    # to 0x1000. So, we'll really have to cheat.
    # When using the screen memory size, keep in mind that each character
    # byte has an associated attribute byte.
    # uses:        word at 40:4c contains  memory size
    #              byte at 40:84           # of rows-1 (sometimes)
    #              byte at 40:4a           # of columns
    #ifndef GNU
    # screen size less then 4K? then we have 25 lines only
    if (*(uintW far *)(0x0040004CL)<=4096)
      return 25;
    # VEGA vga uses the bios byte at 0x40:0x84 for # of rows.
    # Use that byte, if it makes sense together with memory size.
    if ((((*(uintB far *)(0x0040004AL)*2*(*(uintB far *)(0x00400084L)+1))
          +0xfff
         )
         &(~0xfff)
        )
        == *(uintW far *)(0x0040004CL)
       )
      return *(uintB far *)(0x00400084L)+1;
    #endif
    # uh oh. Emit LFs until screen starts scrolling.
    { var uintW line;
      var uintW oldline = 0;
      video(0x0200,NULL,&oldline); # INT 10,02 : set cursor position to (0,0)
      loop
        { video(0x0e0a,NULL,NULL); # INT 10,0E : write LF in teletype mode
          video(0x0300,NULL,&line); # INT 10,03 : read cursor position
          line>>=8;
          if (oldline==line) { return line+1; }
          oldline = line;
  } }   }

# High-level BIOS interface

local uintW LINES;
local uintW COLS;

void v_up()
  { # cursor up: determine current position, decrement row, set position
    var uintW dx;
    video(0x0300,NULL,&dx); # INT 10,03 : read cursor position
    dx -= 0x100;
    video(0x0200,NULL,&dx); # INT 10,02 : set cursor position
  }

#if 1

void v_cb()
  { # cursor big: set begin scan to end scan - 4
    var uintW cx;
    video(0x0300,&cx,NULL); # INT 10,03 : read cursor position
    cx=((cx&0xff)|(((cx&0xff)-4)<<8));
    video(0x0100,&cx,NULL); # INT 10,01 : set cursor type
  }

void v_cs()
  { # cursor small: set begin scan to end scan - 1
    var uintW cx;
    video(0x0300,&cx,NULL); # INT 10,03 : read cursor position
    cx=((cx&0xff)|(((cx&0xff)-1)<<8));
    video(0x0100,&cx,NULL); # INT 10,01 : set cursor type
  }

#endif

void v_ce()
  { # clear to end: get cursor position and emit the aproppriate number
    # of spaces, without moving cursor.
    var uintW cx;
    var uintW dx;
    video(0x0300,NULL,&dx); # INT 10,03 : read cursor position
    cx = COLS - (dx&0xff);
    video(0x0920,&cx,NULL); # INT 10,09 : write character at cursor, cx times 0x20
  }

void v_cl()
  { # clear screen: clear all and set cursor home
    var uintW cx = 0;
    var uintW dx = ((LINES-1)<<8)+(COLS-1);
    video(0x0600,&cx,&dx); # INT 10,06 : scroll active page up
    dx = 0;
    video(0x0200,&cx,&dx); # INT 10,02 : set cursor position
  }

void v_cd()
  { # clear to bottom: get position, clear to eol, clear next line to end
    var uintW cx;
    var uintW dx;
    var uintW dxtmp;
    video(0x0300,NULL,&dx); # INT 10,03 : read cursor position
    dxtmp = (dx&0xff00)|(COLS-1);
    cx = dx;
    video(0x0600,&cx,&dxtmp); # INT 10,06 : scroll active page up
    cx = (dx&0xff00)+0x100;
    dx = ((LINES-1)<<8)+(COLS-1);
    video(0x0600,&cx,&dx); # INT 10,06 : scroll active page up
  }

void v_al()
  { # add line: scroll rest of screen down
    var uintW cx;
    var uintW dx;
    video(0x0300,NULL,&dx); # INT 10,03 : read cursor position
    cx = (dx&0xff00);
    dx = ((LINES-1)<<8)+(COLS-1);
    video(0x0701,&cx,&dx); # INT 10,06 : scroll active page down
  }

void v_dl()
  { # delete line: scroll rest up
    var uintW cx;
    var uintW dx;
    video(0x0300,NULL,&dx); # INT 10,03 : read cursor position
    cx = (dx&0xff00) /* + 0x100 */ ;
    dx = ((LINES-1)<<8)+(COLS-1);
    video(0x0601,&cx,&dx); # INT 10,06 : scroll active page up
  }

void v_sr()
  { # scroll reverse: scroll whole screen
    var uintW cx = 0;
    var uintW dx = ((LINES-1)<<8)+(COLS-1);
    video(0x0701,&cx,&dx); # INT 10,06 : scroll active page down
  }

void v_move(y,x)
  var uintW y;
  var uintW x;
  { # set cursor
    var uintW dx = (y<<8)+x;
    video(0x0200,NULL,&dx); # INT 10,02 : set cursor position
  }

uintW v_put(ch)
  var uintW ch;
  { # put character:
    # put attribute and char (no scroll!), then update cursor position.
    var uintW cx=1;
    ch &= 0xff;
    if (ch==NL)
      { video(0x0e00|CR,NULL,NULL); # INT 10,0E : write in teletype mode
        video(0x0e00|LF,NULL,NULL); # INT 10,0E : write in teletype mode
      }
      else
      { video(0x0900|ch,&cx,NULL); # INT 10,09 : write character at cursor
       {# cursor right: determine current position, increment column, set position
        var uintW dx;
        video(0x0300,NULL,&dx); # INT 10,03 : read cursor position
        dx += 0x1; # increment column
        if ((dx & 0xff) == COLS) # at right margin?
          { dx &= 0xff00; # set column to 0
            dx += 0x100; # increment row
            if ((dx >> 8) == LINES) # at bottom margin?
              goto no_scroll; # do not scroll at right bottom corner!!
          }
        video(0x0200,NULL,&dx); # INT 10,02 : set cursor position
        no_scroll: ;
      }}
    return ch;
  }

# Lisp-Funktionen:

# UP: Ein Zeichen auf einen Window-Stream ausgeben.
# wr_ch_window(&stream,ch);
# > stream: Window-Stream
# > ch: auszugebendes Zeichen
  local void wr_ch_window (object* stream_, object ch);
  local void wr_ch_window(stream_,ch)
    var reg2 object* stream_;
    var reg3 object ch;
    { if (!string_char_p(ch)) { fehler_string_char(*stream_,ch); } # ch sollte String-Char sein
     {var reg1 uintB c = char_code(ch); # Code des Zeichens
      # Code c übers BIOS auf den Bildschirm ausgeben:
      v_put(c);
    }}

LISPFUNN(make_window,0)
  { var reg2 object stream =
      allocate_stream(strmflags_wr_ch_B,strmtype_window,strm_len+0);
      # Flags: nur WRITE-CHAR erlaubt
    # und füllen:
    var reg1 Stream s = TheStream(stream);
      s->strm_rd_by = P(rd_by_dummy); # READ-BYTE unmöglich
      s->strm_wr_by = P(wr_by_dummy); # WRITE-BYTE unmöglich
      s->strm_rd_ch = P(rd_ch_dummy); # READ-CHAR unmöglich
      s->strm_rd_ch_last = NIL; # Lastchar := NIL
      s->strm_wr_ch = P(wr_ch_window); # WRITE-CHAR-Pseudofunktion
      s->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
      #ifdef STRM_WR_SS
      s->strm_wr_ss = P(wr_ss_dummy_nogc);
      #endif
    LINES = v_rows(); COLS = v_cols();
    screenattr = 0; attr = attr_table[screentype][screenattr];
    v_cs();
    value1 = stream; mv_count=1;
  }

# Schließt einen Window-Stream.
  local void close_window (object stream);
  local void close_window(stream)
    var reg1 object stream;
    { v_cs();
      attr = BG(col_black) | FG(col_white); v_cl(); # clear screen black
    }

LISPFUNN(window_size,1)
  { check_window_stream(popSTACK());
    value1 = fixnum(LINES);
    value2 = fixnum(COLS);
    mv_count=2;
  }

LISPFUNN(window_cursor_position,1)
  { check_window_stream(popSTACK());
   {var uintW dx;
    video(0x0300,NULL,&dx); # INT 10,03 : read cursor position
    value1 = fixnum(dx>>8);
    value2 = fixnum(dx&0xff);
    mv_count=2;
  }}

LISPFUNN(set_window_cursor_position,3)
  { check_window_stream(STACK_2);
   {var reg2 uintL line = posfixnum_to_L(STACK_1);
    var reg3 uintL column = posfixnum_to_L(STACK_0);
    if ((line < (uintL)LINES) && (column < (uintL)COLS))
      { v_move(line,column); }
    value1 = STACK_1; value2 = STACK_0; mv_count=2; skipSTACK(3);
  }}

LISPFUNN(clear_window,1)
  { check_window_stream(popSTACK());
    v_cl();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(clear_window_to_eot,1)
  { check_window_stream(popSTACK());
    v_cd();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(clear_window_to_eol,1)
  { check_window_stream(popSTACK());
    v_ce();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(delete_window_line,1)
  { check_window_stream(popSTACK());
    v_dl();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(insert_window_line,1)
  { check_window_stream(popSTACK());
    v_al();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(highlight_on,1)
  { check_window_stream(popSTACK());
    screenattr = 1; attr = attr_table[screentype][screenattr];
    value1 = NIL; mv_count=0;
  }

LISPFUNN(highlight_off,1)
  { check_window_stream(popSTACK());
    screenattr = 0; attr = attr_table[screentype][screenattr];
    value1 = NIL; mv_count=0;
  }

LISPFUNN(window_cursor_on,1)
  { check_window_stream(popSTACK());
    v_cb();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(window_cursor_off,1)
  { check_window_stream(popSTACK());
    v_cs();
    value1 = NIL; mv_count=0;
  }

#endif # MSDOS && !EMUNIX_PORTABEL

#if defined(MSDOS) && (defined(EMUNIX_PORTABEL) && defined(EMUNIX_NEW_8f))

# Benutze die Video-Library von Eberhard Mattes.
# Vorzüge:
# - einfaches Interface,
# - ruft unter OS/2 die Vio-Funktionen auf, unter DOS wird der Bildschirm-
#   speicher direkt angesprochen (schnell!), falls einer der Standard-Textmodi
#   vorliegt, sonst wird das BIOS bemüht (portabel!).

local uintL screentype; # 0 = monochrome, 1 = color

local uintB attr_table[2][5] =
  { # monochrome:
    { /* no standout   */  BW_NORMAL,
      /* standout      */  BW_REVERSE,
      /* visible bell  */  BW_NORMAL | INTENSITY,
      /* underline     */  BW_UNDERLINE,
      /* alt. char set */  BW_NORMAL | INTENSITY,
    },
    # color:
    { /* no standout   */  B_BLUE | F_WHITE | INTENSITY,
      /* standout      */  B_BLUE | F_MAGENTA | INTENSITY,
      /* visible bell  */  B_BLUE | F_BROWN | INTENSITY,
      /* underline     */  B_BLUE | F_GREEN | INTENSITY,
      /* alt. char set */  B_BLUE | F_RED | INTENSITY,
    },
  };

local int cursor_scanlines_start;
local int cursor_scanlines_end;

local int LINES; # Anzahl Zeilen
local int COLS;  # Anzahl Spalten, Anzahl Zeichen pro Zeile

# UP: Ein Zeichen auf einen Window-Stream ausgeben.
# wr_ch_window(&stream,ch);
# > stream: Window-Stream
# > ch: auszugebendes Zeichen
  local void wr_ch_window (object* stream_, object ch);
  local void wr_ch_window(stream_,ch)
    var reg2 object* stream_;
    var reg3 object ch;
    { if (!string_char_p(ch)) { fehler_string_char(*stream_,ch); } # ch sollte String-Char sein
     {var reg1 uintB c = char_code(ch); # Code des Zeichens
      # Code c über die Video-Library auf den Bildschirm ausgeben:
      if (c==NL)
        { v_putc(c); }
        else
        { var int current_x;
          var int current_y;
          v_getxy(&current_x,&current_y); # get current cursor position
          if ((current_x==COLS-1) && (current_y==LINES-1))
            { v_putn(c,1); } # do not scroll at right bottom corner!!
            else
            { v_putc(c); }
        }
    }}

LISPFUNN(make_window,0)
  { var reg2 object stream =
      allocate_stream(strmflags_wr_ch_B,strmtype_window,strm_len+0);
      # Flags: nur WRITE-CHAR erlaubt
    # und füllen:
    var reg1 Stream s = TheStream(stream);
      s->strm_rd_by = P(rd_by_dummy); # READ-BYTE unmöglich
      s->strm_wr_by = P(wr_by_dummy); # WRITE-BYTE unmöglich
      s->strm_rd_ch = P(rd_ch_dummy); # READ-CHAR unmöglich
      s->strm_rd_ch_last = NIL; # Lastchar := NIL
      s->strm_wr_ch = P(wr_ch_window); # WRITE-CHAR-Pseudofunktion
      s->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
      #ifdef STRM_WR_SS
      s->strm_wr_ss = P(wr_ss_dummy_nogc);
      #endif
    v_init(); # Initialisieren
    #if 1
    screentype = (v_hardware()==V_MONOCHROME ? 0 : 1); # Bildschirmtyp abfragen
    #else
    videomode abfragen wie in vinit.c, dann
    screentype = (((videomode==0) || (videomode==7))
                  ? 0 # monochrome
                  : 1 # color
                 );
    #endif
    v_dimen(&COLS,&LINES); # Bildschirmgröße abfragen
    v_getctype(&cursor_scanlines_start,&cursor_scanlines_end); # Cursorform abfragen
    v_attrib(attr_table[screentype][0]); # Highlight off
    v_ctype(cursor_scanlines_end-1,cursor_scanlines_end); # cursor small
    value1 = stream; mv_count=1;
  }

# Schließt einen Window-Stream.
  local void close_window (object stream);
  local void close_window(stream)
    var reg1 object stream;
    { v_gotoxy(0,0); # Cursor home
      v_attrib(screentype==0 ? BW_NORMAL : (B_BLACK | F_WHITE));
      v_putn(' ',LINES*COLS); # Bildschirm löschen
      v_ctype(cursor_scanlines_start,cursor_scanlines_end); # Cursorform zurücksetzen
    }

LISPFUNN(window_size,1)
  { check_window_stream(popSTACK());
    value1 = fixnum((uintW)LINES);
    value2 = fixnum((uintW)COLS);
    mv_count=2;
  }

LISPFUNN(window_cursor_position,1)
  { check_window_stream(popSTACK());
   {var int current_x;
    var int current_y;
    v_getxy(&current_x,&current_y); # get current cursor position
    value1 = fixnum((uintW)current_y);
    value2 = fixnum((uintW)current_x);
    mv_count=2;
  }}

LISPFUNN(set_window_cursor_position,3)
  { check_window_stream(STACK_2);
   {var reg2 uintL line = posfixnum_to_L(STACK_1);
    var reg3 uintL column = posfixnum_to_L(STACK_0);
    if ((line < (uintL)LINES) && (column < (uintL)COLS))
      { v_gotoxy((int)column,(int)line); }
    value1 = STACK_1; value2 = STACK_0; mv_count=2; skipSTACK(3);
  }}

LISPFUNN(clear_window,1)
  { check_window_stream(popSTACK());
    v_gotoxy(0,0);
    #if 0 # Das funktioniert nicht
    v_clear();
    #else
    v_putn(' ',LINES*COLS);
    #endif
    value1 = NIL; mv_count=0;
  }

LISPFUNN(clear_window_to_eot,1)
  { check_window_stream(popSTACK());
   {var int current_x;
    var int current_y;
    v_getxy(&current_x,&current_y); # get current cursor position
    v_putn(' ',COLS*(LINES-current_y)-current_x);
    value1 = NIL; mv_count=0;
  }}

LISPFUNN(clear_window_to_eol,1)
  { check_window_stream(popSTACK());
    v_clreol();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(delete_window_line,1)
  { check_window_stream(popSTACK());
    #if 0 # Bug in EMX 0.8f
    v_delline(1);
    #else
    {var int current_x;
     var int current_y;
     v_getxy(&current_x,&current_y); # get current cursor position
     v_scroll(0,current_y,COLS-1,LINES-1,1,V_SCROLL_UP);
    }
    #endif
    value1 = NIL; mv_count=0;
  }

LISPFUNN(insert_window_line,1)
  { check_window_stream(popSTACK());
    v_insline(1);
    value1 = NIL; mv_count=0;
  }

LISPFUNN(highlight_on,1)
  { check_window_stream(popSTACK());
    v_attrib(attr_table[screentype][1]);
    value1 = NIL; mv_count=0;
  }

LISPFUNN(highlight_off,1)
  { check_window_stream(popSTACK());
    v_attrib(attr_table[screentype][0]);
    value1 = NIL; mv_count=0;
  }

LISPFUNN(window_cursor_on,1)
  { check_window_stream(popSTACK());
    # cursor big: set begin scan to end scan - 4
    v_ctype(cursor_scanlines_end-4,cursor_scanlines_end);
    value1 = NIL; mv_count=0;
  }

LISPFUNN(window_cursor_off,1)
  { check_window_stream(popSTACK());
    # cursor small: set begin scan to end scan - 1
    v_ctype(cursor_scanlines_end-1,cursor_scanlines_end);
    value1 = NIL; mv_count=0;
  }

#endif # MSDOS && (EMUNIX_PORTABEL && EMUNIX_NEW_8f)

#if defined(UNIX) || (defined(EMUNIX_PORTABEL) && defined(EMUNIX_OLD_8e))

# ------------------------------------------------------------------------------

# Routinen zur Emulation aller VT100-Features auf normalen Terminals.
# Idee: Oliver Laumann 1987

# Benutzt die TERMCAP-Library:
  # Besorgt die Capability-Informationen zu Terminal-Type name.
  # Ergebnis: 1 falls OK, 0 falls name unbekannt, -1 bei sonstigem Fehler.
    extern int tgetent (char* bp, char* name);
  # Besorgt den Wert einer numerischen Capability (-1 falls nicht vorhanden).
    extern int tgetnum (char* id);
  # Besorgt den Wert einer booleschen Capability (1 falls vorhanden, 0 sonst).
    extern int tgetflag (char* id);
  # Besorgt den Wert einer String-wertigen Capability und (falls area/=NULL)
  # kopiert es nach *area und rückt dabei *area weiter.
    extern char* tgetstr (char* id, char** area);
  # Besorgt den String, der eine Cursor-Positionierung an Stelle (destcol,destline)
  # bewirkt. (Nötig, da tgetstr("cm") ein spezielles Format hat!)
    extern char* tgoto (char* cm, int destcol, int destline);
  # Führt eine String-Capability aus. Dazu wird für jedes Character die
  # Ausgabefunktion *outcharfun aufgerufen. (Nötig, da String-Capabilities
  # Padding-Befehle enthalten können!)
    extern char* tputs (char* cp, int affcnt, void (*outcharfun)());

# Einstellbare Wünsche:
  #define WANT_INSERT  FALSE  # Insert-Modus
  #define WANT_SAVE    FALSE  # Save/Restore für die Cursor-Position
  #define WANT_ATTR    TRUE   # Attribute (fett, reverse etc.)
  #define WANT_CHARSET FALSE  # Fonts = Charsets
  # zu definierende Funktionen:
  #define WANT_CURSOR_MOVE         FALSE
  #define WANT_CURSOR_BACKSPACE    FALSE
  #define WANT_CURSOR_RETURN       TRUE
  #define WANT_CURSOR_LINEFEED     TRUE
  #define WANT_CURSOR_REVLINEFEED  FALSE
  #define WANT_CLEAR_SCREEN        TRUE
  #define WANT_CLEAR_FROM_BOS      FALSE
  #define WANT_CLEAR_TO_EOS        TRUE
  #define WANT_CLEAR_LINE          FALSE
  #define WANT_CLEAR_FROM_BOL      FALSE
  #define WANT_CLEAR_TO_EOL        TRUE
  #define WANT_INSERT_1CHAR        FALSE
  #define WANT_INSERT_CHAR         FALSE
  #define WANT_INSERT_LINE         TRUE
  #define WANT_DELETE_CHAR         FALSE
  #define WANT_DELETE_LINE         TRUE
  #define WANT_OUTPUT_1CHAR        TRUE
  # kleine Korrekturen:
  #define WANT_CLEAR_SCREEN        TRUE
  #if WANT_OUTPUT_1CHAR && WANT_INSERT
  #define WANT_INSERT_1CHAR        TRUE
  #endif

# Ausgabe eines Zeichens, direkt.
  local void out_char (uintB c);
  local void out_char(c)
    var uintB c;
    { begin_system_call();
     {var reg1 int ergebnis = write(stdout_handle,&c,1); # Zeichen auszugeben versuchen
      end_system_call();
      if (ergebnis<0) { OS_error(); } # Error melden
      if (ergebnis==0) # nicht erfolgreich?
        { fehler(
                 DEUTSCH ? "Kann nichts auf Standard-Output ausgeben." :
                 ENGLISH ? "cannot output to standard output" :
                 ""
                );
        }
    }}

# Ausgabe eines Capability-Strings.
  local void out_capstring (char* s);
  local void out_capstring(s)
    var reg1 char* s;
    { if (!(s==NULL)) # Absichern gegen nicht vorhandene Capability
        { tputs(s,1,(void (*)()) &out_char); }
    }

# Ausgabe eines Capability-Strings mit einem Argument.
  local void out_cap1string (char* s, int arg);
  local void out_cap1string(s,arg)
    var reg1 char* s;
    var reg2 int arg;
    { if (!(s==NULL)) # Absichern gegen nicht vorhandene Capability
        { tputs(tgoto(s,0,arg),1,(void (*)()) &out_char); }
    }

# Kosten der Ausführung einer Capability:
  #define EXPENSIVE 1000
  local uintC cost_counter; # Zähler
  # Funktion, die nicht ausgibt, sondern nur zählt:
  local void count_char (char c);
  local void count_char(c)
    var reg1 char c;
    { cost_counter++; }
  # Berechnet die Kosten der Ausgabe einer Capability:
  local uintC cap_cost (char* s);
  local uintC cap_cost(s)
    var reg1 char* s;
    { if (s==NULL)
        { return EXPENSIVE; } # Capability nicht vorhanden
        else
        { cost_counter = 0;
          tputs(s,1,(void (*)()) &count_char);
          return cost_counter;
        }
    }

# Buffer für von mir benötigte Capabilities und Pointer da hinein:
  local char tentry[4096];
  local char* tp = &tentry[0];
# Einige ausgewählte Capabilities (NULL oder Pointer in tentry hinein):
  # Insert-Modus:
  local char* IMcap; # Enter Insert Mode
  local uintC IMcost;
  local char* EIcap; # End Insert Mode
  local uintC EIcost;
  #if WANT_ATTR
  # Attribute:
  local char* SOcap; # Enter standout mode
  local char* SEcap; # End standout mode
  local char* UScap; # Enter underline mode
  local char* UEcap; # End underline mode
  local char* MBcap; # Turn on blinking
  local char* MDcap; # Turn on bold (extra-bright) mode
  local char* MHcap; # Turn on half-bright mode
  local char* MRcap; # Turn on reverse mode
  local char* MEcap; # Turn off all attributes
  #endif
  #if WANT_CHARSET
  # Zeichensätze:
  local boolean ISO2022; # ob Zeichensatzwechsel nach ISO2022 unterstützt wird
  #endif
  # Cursor-Bewegung:
  local char* CMcap; # Cursor motion, allgemeine Cursor-Positionierung
  local char* TIcap; # Initialize mode where CM is usable
  local char* TEcap; # Exit mode where CM is usable
  local char* BCcap; # Backspace Cursor
  local uintC BCcost;
  local char* NDcap; # cursor right
  local uintC NDcost;
  local char* DOcap; # cursor down
  local uintC DOcost;
  local char* UPcap; # cursor up
  local uintC UPcost;
  local char* NLcap; # Newline
  local char* CRcap; # Carriage Return
  local uintC CRcost;
  # Scrolling:
  local char* CScap; # change scroll region
  #if WANT_DELETE_LINE
  local char* SFcap; # Scroll (text up)
  #endif
  #if WANT_CURSOR_REVLINEFEED || WANT_INSERT_LINE
  local char* SRcap; # Scroll reverse (text down)
  #endif
  # Sonstige:
  local char* IScap; # Terminal Initialization 2
#  local char* BLcap; # Bell
#  local char* VBcap; # Visible Bell (Flash)
  local char* CLcap; # clear screen, cursor home
  #if WANT_CLEAR_FROM_BOS || WANT_CLEAR_TO_EOS || WANT_CLEAR_LINE || WANT_CLEAR_FROM_BOL || WANT_CLEAR_TO_EOL
  local char* CEcap; # clear to end of line
  #endif
  #if WANT_CLEAR_TO_EOS
  local char* CDcap; # clear to end of screen
  #endif
  #if WANT_CURSOR_REVLINEFEED || WANT_INSERT_LINE
  local char* ALcap; # add new blank line
  #endif
  #if WANT_DELETE_LINE
  local char* DLcap; # delete line
  #endif
  #if WANT_DELETE_CHAR
  local char* DCcap; # delete character
  #endif
  #if WANT_INSERT_1CHAR || WANT_INSERT_CHAR
  local char* ICcap; # insert character
  #endif
  #if WANT_INSERT_CHAR
  local char* CICcap; # insert count characters
  #endif
  #if WANT_INSERT_LINE
  local char* CALcap; # add count blank lines
  #endif
  #if WANT_DELETE_CHAR
  local char* CDCcap; # delete count chars
  #endif
  #if WANT_DELETE_LINE
  local char* CDLcap; # delete count lines
  #endif
  local boolean AM; # automatic margins, ob an rechterer unterer Ecke scrollt
  local int rows; # Anzahl der Zeilen des Bildschirms, >0
  local int cols; # Anzahl der Spalten des Bildschirms, >0
  # Obere Zeile ist Zeile 0, untere Zeile ist Zeile rows-1.
  # Linke Spalte ist Spalte 0, rechte Spalte ist Spalte cols-1.
  #if WANT_ATTR || WANT_CHARSET
  local uintB* null; # Pointer auf cols Nullen
  #endif
  local uintB* blank; # Pointer auf cols Blanks

# Beschreibung einer Terminal-Ausgabe-Einheit:
typedef struct { uintB** image; # image[y][x] ist das Zeichen an Position (x,y)
                 #if WANT_ATTR
                 uintB** attr;  # attr[y][x] ist sein Attribut
                 uintB curr_attr; # welches Attribut gerade aktuell ist
                 #endif
                 #if WANT_CHARSET
                 uintB** font;  # font[y][x] ist sein Font (Charset)
                 #define charset_count 4
                 uintB charsets[charset_count]; # Tabelle von Zeichensätzen
                 uintC curr_charset; # welcher der Zeichensätze gerade aktuell ist
                                     # (>=0, <charset_count)
                 #endif
                 int x; # Cursorposition (>=0, <=cols)
                 int y; # Cursorposition (>=0, <rows)
                        # (Bei x=cols wird der Cursor in Spalte cols-1 dargestellt.)
                 int top, bot; # Scroll-Region = Zeilen y mit top <= y <= bot,
                               # es ist 0 <= top <= bot <= rows-1.
                 #if WANT_INSERT
                 boolean insert; # ob die Ausgabe-Einheit im Insert-Modus arbeitet
                                 # (dann ist das Terminal meist im Insert-Modus)
                 #endif
                 #if WANT_SAVE
                 boolean saved;
                 #if WANT_ATTR
                 uintB saved_curr_attr;
                 #endif
                 #if WANT_CHARSET
                 uintB saved_charsets[charset_count];
                 uintC saved_curr_charset;
                 #endif
                 int saved_x, saved_y;
                 #endif
               }
        win;

# aktuelle Ausgabe-Einheit:
  local win currwin; # es gibt nur eine!
  #define curr (&currwin)

#if WANT_INSERT

# Insert-Modus ein- bzw. ausschalten:
  # Flag, ob das Terminal im Insert-Modus ist (falls es einen solchen gibt):
  local boolean insert;
  local void set_insert_mode (boolean flag);
  local void set_insert_mode(flag)
    var reg1 boolean flag;
    { if (flag)
        # Einschalten
        { if (!insert) { out_capstring(IMcap); } }
        else
        # Ausschalten
        { if (insert) { out_capstring(EIcap); } }
      insert = flag;
    }

#endif

#if WANT_ATTR

# Ausgabe-Attribute des Terminals umschalten:
  local uintB term_attr; # aktuelle Attribute des Terminals
  # mögliche Attribute sind ein ODER aus:
    #define A_SO    bit(0)  # Standout mode
    #define A_US    bit(1)  # Underscore mode
    #define A_BL    bit(2)  # Blinking
    #define A_BD    bit(3)  # Bold mode
    #define A_DI    bit(4)  # Dim mode
    #define A_RV    bit(5)  # Reverse mode
  local void change_attr (uintB new_attr);
  local void change_attr(new_attr)
    var reg1 uintB new_attr;
    { var reg2 uintB old_attr = term_attr;
      if (old_attr == new_attr) { return; }
      if (   ((old_attr & A_SO) && !(new_attr & A_SO))
          || ((old_attr & A_US) && !(new_attr & A_US))
          || ((old_attr & A_BL) && !(new_attr & A_BL))
          || ((old_attr & A_BD) && !(new_attr & A_BD))
          || ((old_attr & A_DI) && !(new_attr & A_DI))
          || ((old_attr & A_RV) && !(new_attr & A_RV))
         )
        # Muß Attribute ausschalten.
        { out_capstring(UEcap); # alle aus
          out_capstring(SEcap);
          out_capstring(MEcap);
          if (new_attr & A_SO) out_capstring(SOcap); # und selektiv wieder an
          if (new_attr & A_US) out_capstring(UScap);
          if (new_attr & A_BL) out_capstring(MBcap);
          if (new_attr & A_BD) out_capstring(MDcap);
          if (new_attr & A_DI) out_capstring(MHcap);
          if (new_attr & A_RV) out_capstring(MRcap);
        }
        else
        { # selektiv einschalten:
          if ((new_attr & A_SO) && !(old_attr & A_SO)) out_capstring(SOcap);
          if ((new_attr & A_US) && !(old_attr & A_US)) out_capstring(UScap);
          if ((new_attr & A_BL) && !(old_attr & A_BL)) out_capstring(MBcap);
          if ((new_attr & A_BD) && !(old_attr & A_BD)) out_capstring(MDcap);
          if ((new_attr & A_DI) && !(old_attr & A_DI)) out_capstring(MHcap);
          if ((new_attr & A_RV) && !(old_attr & A_RV)) out_capstring(MRcap);
        }
      term_attr = new_attr;
    }

#endif

#if WANT_CHARSET

# Ausgabe-Zeichensatz des Terminals umschalten:
  local uintB term_charset; # aktueller Zeichensatz des Terminals
                            # = curr->charsets[curr->curr_charset]
  #define ASCII 0  # Abkürzung für den Zeichensatz 'B'
  local void change_charset (uintB new);
  local void change_charset(new)
    var reg1 uintB new;
    { if (term_charset==new) { return; }
      if (ISO2022)
        { out_char(ESC); out_char('('); out_char(new==ASCII ? 'B' : new); } /*)*/
      term_charset = new;
    }
  # Charset Nr. n auf c schalten:
  local void choose_charset (uintB c, uintC n);
  local void choose_charset(c,n)
    var reg1 uintB c;
    var reg2 uintC n;
    { if (c=='B') { c = ASCII; }
      if (curr->charsets[n] == c) return;
      curr->charsets[n] = c;
      if (curr->curr_charset == n) # der aktuelle?
        { change_charset(c); }
    }
  # Charset Nr. n aktuell machen:
  local void set_curr_charset (uintC n);
  local void set_curr_charset(n)
    var reg1 uintC n;
    { if (curr->curr_charset == n) return;
      curr->curr_charset = n;
      change_charset(curr->charsets[n]);
    }

#endif

# Kosten des Neu-Anzeigens von Zeile y, Zeichen x1..x2-1 berechnen:
# (0 <= y < rows, 0 <= x1 <= x2 <= cols)
  local uintC rewrite_cost (int y, int x1, int x2);
  local uintC rewrite_cost(y,x1,x2)
    var reg4 int y;
    var reg6 int x1;
    var reg5 int x2;
    { if (AM && (y==rows-1) && (x2==cols)) # rechte untere Ecke kann scrollen?
        { return EXPENSIVE; }
     {var reg1 int dx = x2-x1;
      if (dx==0) { return 0; }
      #if WANT_ATTR
      {var reg2 uintB* p = &curr->attr[y][x1];
       var reg3 uintC count;
       dotimespC(count,dx,
         { if (!(*p++ == term_attr)) # Attribut-Wechsel nötig?
             { return EXPENSIVE; }
         });
      }
      #endif
      #if WANT_CHARSET
      {var reg2 uintB* p = &curr->font[y][x1];
       var reg3 uintC count;
       dotimespC(count,dx,
         { if (!(*p++ == term_charset)) # Font-Wechsel nötig?
             { return EXPENSIVE; }
         });
      }
      #endif
      {var reg2 uintC cost = dx;
       #if WANT_INSERT
       if (curr->insert) { cost += EIcost + IMcost; }
       #endif
       return cost;
    }}}

# Bewegt den Cursor von Position (y1,x1) an Position (y2,x2).
# (x1,y1) = (-1,-1) falls aktuelle Position unbekannt.
  local void gofromto (int y1, int x1, int y2, int x2);
  local void gofromto(y1,x1,y2,x2)
    var reg10 int y1;
    var reg10 int x1;
    var reg10 int y2;
    var reg9 int x2;
    { if (x2==cols) # Cursor an den rechten Rand?
        { x2--; out_capstring(tgoto(CMcap,x2,y2)); return; } # Bleibt in der letzten Spalte
      if (x1==cols) # Cursor ist am rechten Rand?
        { out_capstring(tgoto(CMcap,x2,y2)); return; } # absolut adressieren
     {var reg4 int dy = y2-y1;
      var reg5 int dx = x2-x1;
      if ((dy==0) && (dx==0)) { return; }
      if ((y1==-1) || (x1==-1) || (y2 > curr->bot) || (y2 < curr->top))
        { out_capstring(tgoto(CMcap,x2,y2)); return; }
      { var reg7 enum { MX_NONE, MX_LE, MX_RI, MX_RW, MX_CR } mx = MX_NONE;
        var reg8 enum { MY_NONE, MY_UP, MY_DO } my = MY_NONE;
        # Möglichkeit 1: mit CMcap
        var reg6 uintC CMcost = cap_cost(tgoto(CMcap,x2,y2));
        # Möglichkeit 2: mit getrennten x- und y-Bewegungen:
        var reg1 uintC xycost = 0;
        if (dx > 0)
          { var reg2 uintC cost1 = rewrite_cost(y1,x1,x2);
            var reg3 uintC cost2 = dx * NDcost;
            if (cost1 < cost2)
              { mx = MX_RW; xycost += cost1; }
              else
              { mx = MX_RI; xycost += cost2; }
          }
        elif (dx < 0)
          { mx = MX_LE; xycost += (-dx) * BCcost; }
        if (!(dx==0))
          { var reg2 uintC cost1 = CRcost + rewrite_cost(y1,0,x2);
            if (cost1 < xycost) { mx = MX_CR; xycost = cost1; }
          }
        if (dy > 0)
          { my = MY_DO; xycost += dy * DOcost; }
        elif (dy < 0)
          { my = MY_UP; xycost += (-dy) * UPcost; }
        if (xycost >= CMcost)
          { out_capstring(tgoto(CMcap,x2,y2)); return; }
        if (!(mx==MX_NONE))
          { if ((mx==MX_LE) || (mx==MX_RI))
              { var reg2 char* s;
                if (mx==MX_LE) { dx = -dx; s = BCcap; } else { s = NDcap; }
                do { out_capstring(s); } until (--dx == 0);
              }
              else
              { if (mx==MX_CR) { out_capstring(CRcap); x1=0; }
                # Hiervon wurden die Kosten mit rewrite_cost berechnet:
                if (x1<x2)
                  {
                    #if WANT_INSERT
                    if (curr->insert) { set_insert_mode(FALSE); }
                    #endif
                    {var reg2 uintB* ptr = &curr->image[y1][x1];
                     var reg3 uintC count;
                     dotimespC(count,x2-x1, { out_char(*ptr++); });
                    }
                    #if WANT_INSERT
                    if (curr->insert) { set_insert_mode(TRUE); }
                    #endif
              }   }
          }
        if (!(my==MY_NONE))
          { var reg2 char* s;
            if (my==MY_UP) { dy = -dy; s = UPcap; } else { s = DOcap; }
            do { out_capstring(s); } until (--dy == 0);
          }
    }}}

# Redisplay
  # lokale Variablen:
  local int last_x;
  local int last_y;
  # Eine Zeile neu anzeigen, die sich verändert haben kann:
    # nur benötigte Parameter wirklich übergeben:
    #if WANT_ATTR && WANT_CHARSET
      #define RHargs(osp,oap,ofp,nsp,nap,nfp,y,x1,x2) (osp,oap,ofp,nsp,nap,nfp,y,x1,x2)
      #define RHparms(osp,oap,ofp,nsp,nap,nfp,y,x1,x2) (osp,oap,ofp,nsp,nap,nfp,y,x1,x2)
    #endif
    #if !WANT_ATTR && WANT_CHARSET
      #define RHargs(osp,oap,ofp,nsp,nap,nfp,y,x1,x2) (osp,ofp,nsp,nfp,y,x1,x2)
      #define RHparms(osp,oap,ofp,nsp,nap,nfp,y,x1,x2) (osp,ofp,nsp,nfp,y,x1,x2,oap,nap)
    #endif
    #if WANT_ATTR && !WANT_CHARSET
      #define RHargs(osp,oap,ofp,nsp,nap,nfp,y,x1,x2) (osp,oap,nsp,nap,y,x1,x2)
      #define RHparms(osp,oap,ofp,nsp,nap,nfp,y,x1,x2) (osp,oap,nsp,nap,y,x1,x2,ofp,nfp)
    #endif
    #if !WANT_ATTR && !WANT_CHARSET
      #define RHargs(osp,oap,ofp,nsp,nap,nfp,y,x1,x2) (osp,nsp,y,x1,x2)
      #define RHparms(osp,oap,ofp,nsp,nap,nfp,y,x1,x2) (osp,nsp,y,x1,x2,oap,ofp,nap,nfp)
    #endif
    #ifdef ANSI
    #undef RHparms
    #define RHparms  RHargs  # korrekt deklarieren
    local void redisplay_help RHparms (uintB* osp, uintB* oap, uintB* ofp, # old
                                       uintB* nsp, uintB* nap, uintB* nfp, # new
                                       int y, int x1, int x2); # Zeile y, von x1 bis x2-1
    #endif
    local void redisplay_help RHparms(osp,oap,ofp,nsp,nap,nfp,y,x1,x2)
      var reg6 uintB* osp;
      var reg4 uintB* oap;
      var reg5 uintB* ofp;
      var reg3 uintB* nsp;
      var reg1 uintB* nap;
      var reg2 uintB* nfp;
      var reg9 int y;
      var reg10 int x1;
      var reg10 int x2;
      { if (AM && (y == rows-1) && (x2 == cols)) { x2--; }
       {
        #if WANT_ATTR
        var reg8 uintB a = term_attr; # letztes Attribut
        #endif
        #if WANT_CHARSET
        var reg8 uintB f = term_charset; # letzter Font
        #endif
        var reg7 int x = x1;
        osp = &osp[x1]; nsp = &nsp[x1];
        #if WANT_ATTR
        oap = &oap[x1]; nap = &nap[x1];
        #endif
        #if WANT_CHARSET
        ofp = &ofp[x1]; nfp = &nfp[x1];
        #endif
        while (x < x2)
          { if (!((*nsp==*osp)
                  #if WANT_ATTR
                  && (*nap==*oap) && (*nap==a)
                  #endif
                  #if WANT_CHARSET
                  && (*nfp==*nap) && (*nfp==f)
                  #endif
               ) )
              { gofromto(last_y,last_x,y,x);
                #if WANT_ATTR
                a = *nap; if (!(a==term_attr)) { change_attr(a); }
                #endif
                #if WANT_CHARSET
                f = *nfp; if (!(f==term_charset)) { change_charset(f); }
                #endif
                out_char(*nsp);
                last_y = y; last_x = x+1;
              }
            x++;
            osp++; nsp++;
            #if WANT_ATTR
            oap++; nap++;
            #endif
            #if WANT_CHARSET
            ofp++; nfp++;
            #endif
          }
      }}
  #if WANT_INSERT_1CHAR || WANT_INSERT_CHAR || WANT_DELETE_CHAR
  # Eine Zeile neu anzeigen:
    # nur benötigte Parameter wirklich übergeben:
    #if WANT_ATTR && WANT_CHARSET
      #define RLargs(osp,oap,ofp,y,x1,x2) (osp,oap,ofp,y,x1,x2)
      #define RLparms(osp,oap,ofp,y,x1,x2) (osp,oap,ofp,y,x1,x2)
    #endif
    #if !WANT_ATTR && WANT_CHARSET
      #define RLargs(osp,oap,ofp,y,x1,x2) (osp,ofp,y,x1,x2)
      #define RLparms(osp,oap,ofp,y,x1,x2) (osp,ofp,y,x1,x2,oap)
    #endif
    #if WANT_ATTR && !WANT_CHARSET
      #define RLargs(osp,oap,ofp,y,x1,x2) (osp,oap,y,x1,x2)
      #define RLparms(osp,oap,ofp,y,x1,x2) (osp,oap,y,x1,x2,ofp)
    #endif
    #if !WANT_ATTR && !WANT_CHARSET
      #define RLargs(osp,oap,ofp,y,x1,x2) (osp,y,x1,x2)
      #define RLparms(osp,oap,ofp,y,x1,x2) (osp,y,x1,x2,oap,ofp)
    #endif
    #ifdef ANSI
    #undef RHparms
    #define RHparms  RHargs  # korrekt deklarieren
    local void redisplay_line RLparms (uintB* osp, uintB* oap, uintB* ofp, # old
                                       int y, int x1, int x2); # Zeile y, von x1 bis x2-1
    #endif
    local void redisplay_line RLparms(osp,oap,ofp,y,x1,x2)
      var reg4 uintB* osp;
      var reg5 uintB* oap;
      var reg6 uintB* ofp;
      var reg1 int y;
      var reg2 int x1;
      var reg3 int x2;
      {
        #if WANT_INSERT
        if (curr->insert) { set_insert_mode(FALSE); }
        #endif
        #if WANT_ATTR
        {var reg4 uintB saved_attr = term_attr; change_attr(0);
        #endif
        #if WANT_CHARSET
        {var reg4 uintB saved_charset = term_charset; change_charset(ASCII);
        #endif
        last_y = y; last_x = x1;
        redisplay_help RHargs(osp,           oap,          ofp,
                              curr->image[y],curr->attr[y],curr->font[y],
                              y, x1,x2
                             );
        #if WANT_CHARSET
        change_charset(saved_charset); }
        #endif
        #if WANT_ATTR
        change_attr(saved_attr); }
        #endif
        #if WANT_INSERT
        if (curr->insert) { set_insert_mode(TRUE); }
        #endif
      }
  #endif
  # Den ganzen Schirm neu anzeigen:
    local void redisplay (void);
    local void redisplay()
      {
        #if WANT_INSERT
        set_insert_mode(FALSE);
        #endif
        #if WANT_ATTR
        {var reg2 uintB saved_attr = term_attr; change_attr(0);
        #endif
        #if WANT_CHARSET
        {var reg2 uintB saved_charset = term_charset; change_charset(ASCII);
        #endif
        out_capstring(CLcap); last_x = 0; last_y = 0;
        {var reg1 uintC y = 0;
         while (y<rows)
           { redisplay_help RHargs(blank,         null,         null,          # old
                                   curr->image[y],curr->attr[y],curr->font[y], # new
                                   y,                                          # Zeile y
                                   0,cols                                      # alle Spalten
                                  );
             y++;
        }  }
        #if WANT_CHARSET
        change_charset(saved_charset); }
        #endif
        #if WANT_ATTR
        change_attr(saved_attr); }
        #endif
        #if WANT_INSERT
        if (curr->insert) { set_insert_mode(TRUE); }
        #endif
        gofromto(last_y,last_x,curr->y,curr->x);
      }

# Weitere Cursor-Bewegungen:
#if WANT_CURSOR_MOVE

  local void cursor_right (int n);
  local void cursor_right(n)
    var reg3 int n;
    { var reg2 int x = curr->x;
      if (x==cols) { return; }
     {var reg1 int new_x = x + n;
      if (new_x > cols) { new_x = cols; }
      gofromto(curr->y,x,curr->y,curr->x = new_x);
    }}

  local void cursor_left (int n);
  local void cursor_left(n)
    var reg3 int n;
    { var reg2 int x = curr->x;
      var reg1 int new_x = x - n;
      if (new_x < 0) { new_x = 0; }
      gofromto(curr->y,x,curr->y,curr->x = new_x);
    }

  local void cursor_up (int n);
  local void cursor_up(n)
    var reg3 int n;
    { var reg2 int y = curr->y;
      var reg1 int new_y = y - n;
      if (new_y < 0) { new_y = 0; }
      gofromto(y,curr->x,curr->y = new_y,curr->x);
    }

  local void cursor_down (int n);
  local void cursor_down(n)
    var reg3 int n;
    { var reg2 int y = curr->y;
      var reg1 int new_y = y + n;
      if (new_y >= rows) { new_y = rows-1; }
      gofromto(y,curr->x,curr->y = new_y,curr->x);
    }

#endif

# Backspace (Cursor um 1 nach links, innerhalb einer Zeile)
#if WANT_CURSOR_BACKSPACE
  local void cursor_backspace (void);
  local void cursor_backspace()
    { if (curr->x > 0)
        { if (curr->x < cols)
            { if (BCcap)
                { out_capstring(BCcap); }
                else
                { gofromto(curr->y,curr->x,curr->y,curr->x - 1); }
            }
          curr->x = curr->x - 1;
    }   }
#endif

# Return (Cursor an den Anfang der Zeile)
#if WANT_CURSOR_RETURN
  local void cursor_return (void);
  local void cursor_return()
    { if (curr->x > 0) { out_capstring(CRcap); curr->x = 0; } }
#endif

# Hilfroutinen zum Scrollen:
#if WANT_CURSOR_LINEFEED || WANT_DELETE_LINE
  local void scroll_up_help (uintB** pp, uintB filler);
  local void scroll_up_help(pp,filler)
    var reg1 uintB** pp;
    var reg3 uintB filler;
    { # pp[top..bot] um eins nach links verschieben,
      # pp[top] herausnehmen, löschen und als pp[bot] wieder einhängen:
      pp = &pp[curr->top];
     {var reg2 uintC count;
      var reg4 uintB* tmp = *pp;
      dotimesC(count,curr->bot - curr->top, { pp[0] = pp[1]; pp++; } );
      {var reg1 uintB* p = tmp;
       dotimesC(count,cols, { *p++ = filler; } );
      }
      *pp = tmp;
    }}
  local void scroll_up (void);
  local void scroll_up()
    { scroll_up_help(curr->image,' ');
      #if WANT_ATTR
      scroll_up_help(curr->attr,0);
      #endif
      #if WANT_CHARSET
      scroll_up_help(curr->font,0);
      #endif
    }
#endif
#if WANT_CURSOR_REVLINEFEED || WANT_INSERT_LINE
  local void scroll_down_help (uintB** pp, uintB filler);
  local void scroll_down_help(pp,filler)
    var reg1 uintB** pp;
    var reg3 uintB filler;
    { # pp[top..bot] um eins nach rechts verschieben,
      # pp[top] herausnehmen, löschen und als pp[bot] wieder einhängen:
      pp = &pp[curr->bot];
     {var reg2 uintC count;
      var reg4 uintB* tmp = *pp;
      dotimesC(count,curr->bot - curr->top, { pp[0] = pp[-1]; pp--; } );
      {var reg1 uintB* p = tmp;
       dotimesC(count,cols, { *p++ = filler; } );
      }
      *pp = tmp;
    }}
  local void scroll_down (void);
  local void scroll_down()
    { scroll_down_help(curr->image,' ');
      #if WANT_ATTR
      scroll_down_help(curr->attr,0);
      #endif
      #if WANT_CHARSET
      scroll_down_help(curr->font,0);
      #endif
    }
#endif

# Linefeed (Cursor um 1 nach unten):
#if WANT_CURSOR_LINEFEED
  local void cursor_linefeed (void);
  local void cursor_linefeed()
    { if (curr->y == curr->bot) { scroll_up(); }
      elif (curr->y < rows-1) { curr->y++; }
      out_capstring(NLcap);
    }
#endif

# Reverse Linefeed (Cursor um 1 nach oben):
#if WANT_CURSOR_REVLINEFEED
  local void cursor_revlinefeed (void);
  local void cursor_revlinefeed()
    { if (curr->y == curr->top)
        { scroll_down();
          if (SRcap)
            { out_capstring(SRcap); }
          elif (ALcap)
            { gofromto(curr->top,curr->x,curr->top,0); # Cursor nach links
              out_capstring(ALcap);
              gofromto(curr->top,0,curr->top,curr->x); # Cursor wieder zurück
            }
          else
            { redisplay(); }
        }
      elif (curr->y > 0)
        { cursor_up(1); }
    }
#endif

# Lösch-Operationen:

# Stück einer Zeile löschen:
#if WANT_CLEAR_SCREEN || WANT_CLEAR_FROM_BOS
  local void cleared_linepart (int y, int x1, int x2);
  local void cleared_linepart(y,x1,x2)
    var reg5 int y;
    var reg4 int x1;
    var reg6 int x2;
    { var reg3 int n = x2-x1;
      if (n>0)
        { {var reg1 uintB* sp = &curr->image[y][x1];
           var reg2 uintC count;
           dotimespC(count,n, { *sp++ = ' '; } );
          }
          #if WANT_ATTR
          {var reg1 uintB* ap = &curr->attr[y][x1];
           var reg2 uintC count;
           dotimespC(count,n, { *ap++ = 0; } );
          }
          #endif
          #if WANT_CHARSET
          {var reg1 uintB* fp = &curr->font[y][x1];
           var reg2 uintC count;
           dotimespC(count,n, { *fp++ = 0; } );
          }
          #endif
    }   }
#endif

# Bildschirm löschen:
#if WANT_CLEAR_SCREEN
  local void clear_screen (void);
  local void clear_screen()
    { out_capstring(CLcap);
     {var reg3 uintC y = 0;
      while (y<rows) { cleared_linepart(y,0,cols); y++; }
    }}
#endif

# Stück einer Zeile löschen:
#if WANT_CLEAR_FROM_BOS || WANT_CLEAR_TO_EOS || WANT_CLEAR_LINE || WANT_CLEAR_FROM_BOL || WANT_CLEAR_TO_EOL
  local void clear_linepart (int y, int x1, int x2);
  local void clear_linepart(y,x1,x2)
    var reg5 int y;
    var reg4 int x1;
    var reg6 int x2;
    { var reg3 int n = x2-x1;
      if (n>0)
        { {var reg1 uintB* sp = &curr->image[y][x1];
           var reg2 uintC count;
           dotimesC(count,n, { *sp++ = ' '; } );
          }
          #if WANT_ATTR
          {var reg1 uintB* ap = &curr->attr[y][x1];
           var reg2 uintC count;
           dotimesC(count,n, { *ap++ = 0; } );
          }
          #endif
          #if WANT_CHARSET
          {var reg1 uintB* fp = &curr->font[y][x1];
           var reg2 uintC count;
           dotimesC(count,n, { *fp++ = 0; } );
          }
          #endif
          if ((x2==cols) && CEcap)
            { gofromto(curr->y,curr->x,y,x1); curr->y = y; curr->x = x1;
              out_capstring(CEcap);
            }
            else
            { if ((x2==cols) && (y==rows-1) && AM) { n--; }
              if (n>0)
                {
                  #if WANT_ATTR
                  {var reg7 uintB saved_attr = term_attr; change_attr(0);
                  #endif
                  #if WANT_CHARSET
                  {var reg7 uintB saved_charset = term_charset; change_charset(ASCII);
                  #endif
                  #if WANT_INSERT
                  if (curr->insert) { set_insert_mode(FALSE); }
                  #endif
                  gofromto(curr->y,curr->x,y,x1);
                  {var reg1 uintC count;
                   dotimespC(count,n, { out_char(' '); } );
                  }
                  curr->y = y; curr->x = x1+n;
                  #if WANT_CHARSET
                  change_charset(saved_charset); }
                  #endif
                  #if WANT_ATTR
                  change_attr(saved_attr); }
                  #endif
                  #if WANT_INSERT
                  if (curr->insert) { set_insert_mode(TRUE); }
                  #endif
    }   }   }   }
#endif

# Bildschirm bis zum Cursor (ausschließlich) löschen:
#if WANT_CLEAR_FROM_BOS
  local void clear_from_BOS (void);
  local void clear_from_BOS()
    { var reg2 int y0 = curr->y;
      var reg3 int x0 = curr->x;
      var reg1 int y = 0;
      while (y<y0) { clear_linepart(y,0,cols); y++; }
      clear_linepart(y0,0,x0);
      gofromto(curr->y,curr->x,y0,x0); curr->y = y0; curr->x = x0;
    }
#endif

# Bildschirm ab Cursor (einschließlich) löschen:
#if WANT_CLEAR_TO_EOS
  local void clear_to_EOS (void);
  local void clear_to_EOS()
    { var reg2 int y0 = curr->y;
      var reg3 int x0 = curr->x;
      if (CDcap)
        { out_capstring(CDcap);
          cleared_linepart(y0,x0,cols);
         {var reg1 int y = y0;
          while (++y < rows) { cleared_linepart(y,0,cols); }
        }}
        else
        { clear_linepart(y0,x0,cols);
         {var reg1 int y = y0;
          while (++y < rows) { clear_linepart(y,0,cols); }
        }}
      gofromto(curr->y,curr->x,y0,x0); curr->y = y0; curr->x = x0;
    }
#endif

# Cursorzeile löschen:
#if WANT_CLEAR_LINE
  local void clear_line (void);
  local void clear_line()
    { var reg1 int y0 = curr->y;
      var reg2 int x0 = curr->x;
      clear_linepart(y0,0,cols);
      gofromto(curr->y,curr->x,y0,x0); curr->y = y0; curr->x = x0;
    }
#endif

# Cursorzeile bis Cursor (ausschließlich) löschen:
#if WANT_CLEAR_FROM_BOL
  local void clear_from_BOL (void);
  local void clear_from_BOL()
    { var reg1 int y0 = curr->y;
      var reg2 int x0 = curr->x;
      clear_linepart(y0,0,x0);
      gofromto(curr->y,curr->x,y0,x0); curr->y = y0; curr->x = x0;
    }
#endif

# Cursorzeile ab Cursor (einschließlich) löschen:
#if WANT_CLEAR_TO_EOL
  local void clear_to_EOL (void);
  local void clear_to_EOL()
    { var reg1 int y0 = curr->y;
      var reg2 int x0 = curr->x;
      clear_linepart(y0,x0,cols);
      gofromto(curr->y,curr->x,y0,x0); curr->y = y0; curr->x = x0;
    }
#endif

# Einfüge-Operationen:

# alter Zeileninhalt:
#if WANT_INSERT_1CHAR || WANT_INSERT_CHAR || WANT_DELETE_CHAR
  local uintB* old_image_y;
  #if WANT_ATTR
  local uintB* old_attr_y;
  #endif
  #if WANT_CHARSET
  local uintB* old_font_y;
  #endif
  local void save_line_old (int y);
  local void save_line_old(y)
    var reg4 int y;
    { {var reg1 uintB* p1 = &curr->image[y][0];
       var reg2 uintB* p2 = &old_image_y[0];
       var reg3 uintC count;
       dotimesC(count,cols, { *p2++ = *p1++; } );
      }
      #if WANT_ATTR
      {var reg1 uintB* p1 = &curr->attr[y][0];
       var reg2 uintB* p2 = &old_attr_y[0];
       var reg3 uintC count;
       dotimesC(count,cols, { *p2++ = *p1++; } );
      }
      #endif
      #if WANT_CHARSET
      {var reg1 uintB* p1 = &curr->font[y][0];
       var reg2 uintB* p2 = &old_font_y[0];
       var reg3 uintC count;
       dotimesC(count,cols, { *p2++ = *p1++; } );
      }
      #endif
    }
#endif

# Ein Zeichen einfügen:
#if WANT_INSERT_1CHAR
  local void insert_1char (uintB c);
  local void insert_1char(c)
    var reg6 uintB c;
    { var reg4 int y = curr->y;
      var reg5 int x = curr->x;
      if (x==cols) { x--; } # nicht über den rechten Rand schreiben!
      if (ICcap || IMcap)
        { curr->image[y][x] = c;
          #if WANT_ATTR
          curr->attr[y][x] = curr->curr_attr;
          #endif
          #if WANT_CHARSET
          curr->font[y][x] = curr->charsets[curr->curr_charset]; # = term_charset
          #endif
          #if WANT_INSERT
          if (!curr->insert)
          #endif
            { set_insert_mode(TRUE); }
          out_capstring(ICcap); out_char(c);
          #if WANT_INSERT
          if (!curr->insert)
          #endif
            { set_insert_mode(FALSE); }
          curr->x = x+1;
        }
        else
        { # alten Zeileninhalt retten:
          save_line_old(y);
          # neuen Zeileninhalt bilden:
          {var reg1 uintB* p1 = &curr->image[y][x];
           var reg2 uintB* p2 = &old_image[x];
           var reg3 uintC count;
           *p1++ = c;
           dotimesC(count,cols-1-x, { *p1++ = *p2++; } );
          }
          #if WANT_ATTR
          {var reg1 uintB* p1 = &curr->attr[y][x];
           var reg2 uintB* p2 = &old_attr[x];
           var reg3 uintC count;
           *p1++ = curr->curr_attr;
           dotimesC(count,cols-1-x, { *p1++ = *p2++; } );
          }
          #endif
          #if WANT_CHARSET
          {var reg1 uintB* p1 = &curr->font[y][x];
           var reg2 uintB* p2 = &old_font[x];
           var reg3 uintC count;
           *p1++ = term_charset; # = curr->charsets[curr->curr_charset]
           dotimesC(count,cols-1-x, { *p1++ = *p2++; } );
          }
          #endif
          # Zeile anzeigen:
          redisplay_line RLargs(old_image,old_attr,old_font,y,x,cols);
          x++;
          gofromto(last_y,last_x,y,x); curr->x = x;
    }   }
#endif

# Platz für n Zeichen machen:
#if WANT_INSERT_CHAR
  local void insert_char (uintC n);
  local void insert_char(n)
    var reg6 uintC n;
    { var reg5 int y = curr->y;
      var reg4 int x = curr->x;
      if (n > cols-x) { n = cols-x; }
      if (n==0) return;
      # alten Zeileninhalt retten:
      save_line_old(y);
      # neuen Zeileninhalt bilden:
      {var reg1 uintB* p1 = &curr->image[y][x];
       var reg2 uintB* p2 = &old_image[x];
       var reg3 uintC count;
       dotimespC(count,n, { *p1++ = ' '; } );
       dotimesC(count,cols-x-n, { *p1++ = *p2++; } );
      }
      #if WANT_ATTR
      {var reg1 uintB* p1 = &curr->attr[y][x];
       var reg2 uintB* p2 = &old_attr[x];
       var reg3 uintC count;
       dotimespC(count,n, { *p1++ = 0; } );
       dotimesC(count,cols-x-n, { *p1++ = *p2++; } );
      }
      #endif
      #if WANT_CHARSET
      {var reg1 uintB* p1 = &curr->font[y][x];
       var reg2 uintB* p2 = &old_font[x];
       var reg3 uintC count;
       dotimespC(count,n, { *p1++ = 0; } );
       dotimesC(count,cols-x-n, { *p1++ = *p2++; } );
      }
      #endif
      if (CICcap && (n > 1))
        {
          #if WANT_INSERT
          if (curr->insert) { set_insert_mode(FALSE); }
          #endif
          out_cap1string(CICcap,n);
          {var reg1 uintC count;
           dotimespC(count,n, { out_char(' '); } );
          }
          #if WANT_INSERT
          if (curr->insert) { set_insert_mode(TRUE); }
          #endif
          gofromto(y,x+n,y,x);
        }
      elif (ICcap || IMcap)
        {
          #if WANT_INSERT
          if (!curr->insert)
          #endif
            { set_insert_mode(TRUE); }
          {var reg1 uintC count;
           dotimespC(count,n, { out_capstring(ICcap); out_char(' '); } );
          }
          #if WANT_INSERT
          if (!curr->insert)
          #endif
            { set_insert_mode(FALSE); }
          gofromto(y,x+n,y,x);
        }
      else
        { redisplay_line RLargs(old_image,old_attr,old_font,y,x,cols);
          gofromto(last_y,last_x,y,x);
        }
    }
#endif

# Zeilen einfügen:
#if WANT_INSERT_LINE
  local void insert_line (uintC n);
  local void insert_line(n)
    var reg2 uintC n;
    { if (n > curr->bot - curr->y + 1) { n = curr->bot - curr->y + 1; }
      if (n==0) return;
     {var reg3 int oldtop = curr->top;
      curr->top = curr->y;
      {var reg1 uintC count;
       dotimespC(count,n, { scroll_down(); } );
      }
      if (ALcap || CALcap)
        { gofromto(curr->y,curr->x,curr->y,0); # an den Zeilenanfang
          if ((CALcap && (n>1)) || !ALcap)
            { out_cap1string(CALcap,n); }
            else
            { var reg1 uintC count;
              dotimespC(count,n, { out_capstring(ALcap); } );
            }
          gofromto(curr->y,0,curr->y,curr->x);
        }
      elif (CScap && SRcap)
        { out_capstring(tgoto(CScap,curr->bot,curr->top));
          gofromto(-1,-1,curr->top,0);
          {var reg1 uintC count;
           dotimespC(count,n, { out_capstring(SRcap); } );
          }
          out_capstring(tgoto(CScap,curr->bot,oldtop));
          gofromto(-1,-1,curr->y,curr->x);
        }
      else
        { redisplay(); }
      curr->top = oldtop;
    }}
#endif

# Lösch-Operationen:

# Characters löschen:
#if WANT_DELETE_CHAR
  local void delete_char (uintC n);
  local void delete_char(n)
    var reg6 uintC n;
    { var reg5 int y = curr->y;
      var reg4 int x = curr->x;
      if (n > cols-x) { n = cols-x; }
      if (n==0) return;
      # alten Zeileninhalt retten:
      save_line_old(y);
      # neuen Zeileninhalt bilden:
      {var reg1 uintB* p1 = &curr->image[y][x];
       var reg2 uintB* p2 = &old_image[x];
       var reg3 uintC count;
       dotimesC(count,cols-x-n, { *p1++ = *p2++; } );
       dotimespC(count,n, { *p1++ = ' '; } );
      }
      #if WANT_ATTR
      {var reg1 uintB* p1 = &curr->attr[y][x];
       var reg2 uintB* p2 = &old_attr[x];
       var reg3 uintC count;
       dotimesC(count,cols-x-n, { *p1++ = *p2++; } );
       dotimespC(count,n, { *p1++ = 0; } );
      }
      #endif
      #if WANT_CHARSET
      {var reg1 uintB* p1 = &curr->font[y][x];
       var reg2 uintB* p2 = &old_font[x];
       var reg3 uintC count;
       dotimesC(count,cols-x-n, { *p1++ = *p2++; } );
       dotimespC(count,n, { *p1++ = 0; } );
      }
      #endif
      if (CDCcap && ((n>1) || !DCcap))
        { out_cap1string(CDCcap,n); }
      elif (DCcap)
        { var reg1 uintC count;
          dotimespC(count,n, { out_capstring(DCcap); } );
        }
      else
        { redisplay_line RLargs(old_image,old_attr,old_font,y,x,cols);
          gofromto(last_y,last_x,y,x);
        }
    }
#endif

# Zeilen löschen:
#if WANT_DELETE_LINE
  local void delete_line (uintC n);
  local void delete_line(n)
    var reg2 uintC n;
    { if (n > curr->bot - curr->y + 1) { n = curr->bot - curr->y + 1; }
      if (n==0) return;
     {var reg3 int oldtop = curr->top;
      curr->top = curr->y;
      {var reg1 uintC count;
       dotimespC(count,n, { scroll_up(); } );
      }
      if (DLcap || CDLcap)
        { gofromto(curr->y,curr->x,curr->y,0); # an den Zeilenanfang
          if ((CDLcap && (n>1)) || !DLcap)
            { out_cap1string(CDLcap,n); }
            else
            { var reg1 uintC count;
              dotimespC(count,n, { out_capstring(DLcap); } );
            }
          gofromto(curr->y,0,curr->y,curr->x);
        }
      elif (CScap)
        { out_capstring(tgoto(CScap,curr->bot,curr->top));
          gofromto(-1,-1,curr->bot,0);
          {var reg1 uintC count;
           dotimespC(count,n, { out_capstring(SFcap); } );
          }
          out_capstring(tgoto(CScap,curr->bot,oldtop));
          gofromto(-1,-1,curr->y,curr->x);
        }
      else
        { redisplay(); }
      curr->top = oldtop;
    }}
#endif

# Ein Zeichen ausgeben:
#if WANT_OUTPUT_1CHAR
  local void output_1char (uintB c);
  local void output_1char(c)
    var reg3 uintB c;
    {
      #if WANT_INSERT
      if (curr->insert)
        { insert_1char(c); }
        else
      #endif
        { var reg1 int y = curr->y;
          var reg2 int x = curr->x;
          if (x==cols) { x--; } # nicht über den rechten Rand schreiben!
          curr->image[y][x] = c;
          #if WANT_ATTR
          curr->attr[y][x] = curr->curr_attr;
          #endif
          #if WANT_CHARSET
          curr->font[y][x] = curr->charsets[curr->curr_charset]; # = term_charset
          #endif
          x++;
          if (!(AM && (x==cols) && (curr->y==curr->bot))) # rechte untere Ecke evtl. freilassen
            { out_char(c); } # Zeichen ausgeben
          curr->x = x; # Cursor rückt um eins weiter
          if (x==cols) # außer wenn er schon ganz rechts war
            { gofromto(-1,-1,curr->y,curr->x); }
    }   }
#endif

#if WANT_SAVE

# gespeicherte Cursor-Position:
  local void save_cursor (void);
  local void save_cursor()
    { curr->saved_x = curr->x;
      curr->saved_y = curr->y;
      #if WANT_ATTR
      curr->saved_curr_attr = curr->curr_attr;
      #endif
      #if WANT_CHARSET
      curr->saved_curr_charset = curr->curr_charset;
      {var reg1 uintC i = 0;
       while (i<charset_count) { curr->saved_charsets[i] = curr->charsets[i]; i++; }
      }
      #endif
      curr->saved = TRUE;
    }
  local void restore_cursor (void);
  local void restore_cursor()
    { if (curr->saved)
        { gofromto(curr->y,curr->x,curr->saved_y,curr->saved_x);
          curr->y = curr->saved_y; curr->x = curr->saved_x;
          #if WANT_ATTR
          curr->curr_attr = curr->saved_curr_attr;
          change_attr(curr->curr_attr);
          #endif
          #if WANT_CHARSET
          curr->curr_charset = curr->saved_curr_charset;
          {var reg1 uintC i = 0;
           while (i<charset_count) { curr->charsets[i] = curr->saved_charsets[i]; i++; }
          }
          change_charset(curr->charsets[curr->curr_charset]);
          #endif
    }   }

#endif

# Initialisiert das Terminal.
# Liefert NULL falls OK, einen Fehlerstring sonst.
  local boolean term_initialized = FALSE;
  local char* init_term (void);
  local char* init_term()
    { var char tbuf[4096]; # interner Buffer für die Termcap-Routinen
      if (term_initialized) { return NULL; } # schon initialisiert -> OK
      # Terminal-Typ abfragen:
      begin_system_call();
      { var reg1 char* s = getenv("TERM");
        if (s==NULL)
          { end_system_call();
            return (DEUTSCH ? "Environment enthält keine TERM-Variable." :
                    ENGLISH ? "environment has no TERM variable" :
                    FRANCAIS ? "L'environnment ne contient pas de variable TERM." :
                    ""
                   );
          }
        if (!(tgetent(tbuf,s)==1))
          { end_system_call();
            pushSTACK(asciz_to_string(s));
            return (DEUTSCH ? "TERMCAP kennt Terminal-Typ ~ nicht." :
                    ENGLISH ? "terminal type ~ unknown to termcap" :
                    FRANCAIS ? "TERMCAP ne connait pas le type d'écran ~." :
                    ""
                   );
          }
      }
      { var reg1 int i = tgetnum("co");
        cols = (i>0 ? i : 80);
      }
      { var reg1 int i = tgetnum("li");
        rows = (i>0 ? i : 24);
      }
      #ifdef EMUNIX
      # Obwohl das eigentlich unsauber ist, holen wir uns die aktuelle Bildschirm-
      # größe mit _scrsize().
      #ifdef EMUNIX_OLD_8d
      if (!(_osmode == DOS_MODE))
      #endif
      { var int scrsize[2];
        _scrsize(&!scrsize);
        if (scrsize[0] > 0) { cols = scrsize[0]; }
        if (scrsize[1] > 0) { rows = scrsize[1]; }
      }
      #endif
      if (tgetflag("hc"))
        { end_system_call();
          return (DEUTSCH ? "Unzureichendes Terminal: Hardcopy-Terminal." :
                  ENGLISH ? "insufficient terminal: hardcopy terminal" :
                  FRANCAIS ? "Terminal insuffisant : imprimante au lieu d'écran." :
                  ""
                 );
        }
      if (tgetflag("os"))
        { end_system_call();
          return (DEUTSCH ? "Unzureichendes Terminal: Kann Ausgegebenes nicht mehr löschen." :
                  ENGLISH ? "insufficient terminal: overstrikes, cannot clear output" :
                  FRANCAIS ? "Terminal insuffisant : ne peut rien effacer." :
                  ""
                 );
        }
      if (tgetflag("ns"))
        { end_system_call();
          return (DEUTSCH ? "Unzureichendes Terminal: Kann nicht scrollen." :
                  ENGLISH ? "insufficient terminal: cannot scroll" :
                  FRANCAIS ? "Terminal insuffisant : pas de défilement." :
                  ""
                 );
        }
      if (!(CLcap = tgetstr("cl",&tp)))
        { # Könnte CLcap = "\n\n\n\n"; als Default nehmen ('weird HPs')
          end_system_call();
          return (DEUTSCH ? "Unzureichendes Terminal: Kann Bildschirm nicht löschen." :
                  ENGLISH ? "insufficient terminal: cannot clear screen" :
                  FRANCAIS ? "Terminal insuffisant : ne peut pas effacer l'écran." :
                  ""
                 );
        }
      if (!(CMcap = tgetstr("cm",&tp)))
        { end_system_call();
          return (DEUTSCH ? "Unzureichendes Terminal: Kann Cursor nicht willkürlich positionieren." :
                  ENGLISH ? "insufficient terminal: cannot position cursor randomly" :
                  FRANCAIS ? "Terminal insuffisant : ne peut pas placer le curseur n'importe où." :
                  ""
                 );
        }
      # Capabilities initialisieren:
      AM = tgetflag("am"); if (tgetflag("LP")) { AM = FALSE; }
      TIcap = tgetstr("ti",&tp);
      TEcap = tgetstr("te",&tp);
      # BLcap = tgetstr("bl",&tp); if (!BLcap) BLcap = "\007";
      # VBcap = tgetstr("vb",&tp);
      BCcap = tgetstr("bc",&tp); if (!BCcap) BCcap = (tgetflag("bs") ? "\b" : tgetstr("le",&tp));
      CRcap = tgetstr("cr",&tp); if (!CRcap) CRcap = "\r";
      NLcap = tgetstr("nl",&tp); if (!NLcap) NLcap = "\n";
      DOcap = tgetstr("do",&tp); if (!DOcap) DOcap = NLcap;
      UPcap = tgetstr("up",&tp);
      NDcap = tgetstr("nd",&tp);
      IScap = tgetstr("is",&tp);
      #if WANT_ATTR
      if ((tgetnum("sg") > 0) || (tgetnum("ug") > 0))
        # Beim Umschalten in Standout-Mode oder beim Umschalten in den
        # Underline-Mode gibt's Leerstellen -> unbrauchbar!
        { SOcap = NULL; SEcap = NULL; UScap = NULL; UEcap = NULL;
          MBcap = NULL; MDcap = NULL; MHcap = NULL; MRcap = NULL; MEcap = NULL;
        }
        else
        { SOcap = tgetstr("so",&tp);
          SEcap = tgetstr("se",&tp);
          UScap = tgetstr("us",&tp);
          UEcap = tgetstr("ue",&tp);
          if (!UScap && !UEcap) # kein Underline?
            { UScap = SOcap; UEcap = SEcap; } # nimm Standout als Ersatz
          MBcap = tgetstr("mb",&tp);
          MDcap = tgetstr("md",&tp);
          MHcap = tgetstr("mh",&tp);
          MRcap = tgetstr("mr",&tp);
          MEcap = tgetstr("me",&tp);
          # Does ME also reverse the effect of SO and/or US?  This is not
          # clearly specified by the termcap manual.
          # Anyway, we should at least look whether ME/SE/UE are equal:
          if (UEcap && SEcap && asciz_equal(UEcap,SEcap)) { UEcap = NULL; }
          if (UEcap && MEcap && asciz_equal(UEcap,MEcap)) { UEcap = NULL; }
          if (SEcap && MEcap && asciz_equal(SEcap,MEcap)) { SEcap = NULL; }
          # tgetstr("uc",&tp) liefert ein underline-character. Dann jeweils
          # in redisplay_help() und output_1char() nach dem out_char() noch
          # backspace() und out_capstring(UCcap) durchführen.
          # Für welche Terminals lohnt sich das??
        }
      #endif
      #if WANT_CHARSET
      ISO2022 = tgetflag("G0");
      #endif
      CScap = tgetstr("cs",&tp);
      #if WANT_DELETE_LINE
      SFcap = tgetstr("sf",&tp); if (!SFcap) SFcap = NLcap;
      #endif
      #if WANT_CURSOR_REVLINEFEED || WANT_INSERT_LINE
      SRcap = tgetstr("sr",&tp);
      #endif
      #if WANT_CLEAR_FROM_BOS || WANT_CLEAR_TO_EOS || WANT_CLEAR_LINE || WANT_CLEAR_FROM_BOL || WANT_CLEAR_TO_EOL
      CEcap = tgetstr("ce",&tp);
      #endif
      #if WANT_CLEAR_TO_EOS
      CDcap = tgetstr("cd",&tp);
      #endif
      #if WANT_CURSOR_REVLINEFEED || WANT_INSERT_LINE
      ALcap = tgetstr("al",&tp);
      #endif
      #if WANT_DELETE_LINE
      DLcap = tgetstr("dl",&tp);
      #endif
      #if WANT_DELETE_CHAR
      DCcap = tgetstr("dc",&tp);
      #endif
      #if WANT_INSERT_1CHAR || WANT_INSERT_CHAR
      ICcap = tgetstr("ic",&tp);
      #endif
      #if WANT_INSERT_CHAR
      CICcap = tgetstr("IC",&tp);
      #endif
      #if WANT_INSERT_LINE
      CALcap = tgetstr("AL",&tp);
      #endif
      #if WANT_DELETE_CHAR
      CDCcap = tgetstr("DC",&tp);
      #endif
      #if WANT_DELETE_LINE
      CDLcap = tgetstr("DL",&tp);
      #endif
      IMcap = tgetstr("im",&tp);
      EIcap = tgetstr("ei",&tp);
      if (tgetflag ("in")) # Insert-Modus unbrauchbar?
        { IMcap = NULL; EIcap = NULL;
          #if WANT_INSERT_1CHAR || WANT_INSERT_CHAR
          ICcap = NULL;
          #endif
          #if WANT_INSERT_CHAR
          CICcap = NULL;
          #endif
        }
      if (IMcap && (IMcap[0]==0)) { IMcap = NULL; } # IMcap leer?
      if (EIcap && (EIcap[0]==0)) { EIcap = NULL; } # EIcap leer?
      #if WANT_INSERT_1CHAR || WANT_INSERT_CHAR
      if (ICcap && (ICcap[0]==0)) { ICcap = NULL; } # ICcap leer?
      #endif
      # Kosten der Capabilities berechnen:
      IMcost = cap_cost(IMcap);
      EIcost = cap_cost(EIcap);
      BCcost = cap_cost(BCcap);
      NDcost = cap_cost(NDcap);
      DOcost = cap_cost(DOcap);
      #ifndef NL_HACK
      # Falls DOcap ein LF ausgibt, ist nicht sicher, ob dies auch als solches
      # (und nicht als CR/LF) beim Terminal ankommt. In diesem Fall erklären
      # wir DOcap für unbrauchbar. Das erspart uns den NL_HACK.
      if (DOcap[0]=='\n') { DOcost = EXPENSIVE; }
      #endif
      UPcost = cap_cost(UPcap);
      CRcost = cap_cost(CRcap);
      # Hilfs-Datenstrukturen bereitstellen:
      {var reg1 uintB* ptr = malloc(cols*sizeof(uintB));
       var reg2 uintC count;
       blank = ptr;
       dotimespC(count,cols, { *ptr++ = ' '; } );
      }
      #if WANT_ATTR || WANT_CHARSET
      {var reg1 uintB* ptr = malloc(cols*sizeof(uintB));
       var reg2 uintC count;
       null = ptr;
       dotimespC(count,cols, { *ptr++ = 0; } );
      }
      #endif
      #if WANT_INSERT_1CHAR || WANT_INSERT_CHAR || WANT_DELETE_CHAR
      old_image_y = malloc(cols*sizeof(uintB));
      #if WANT_ATTR
      old_attr_y = malloc(cols*sizeof(uintB));
      #endif
      #if WANT_CHARSET
      old_font_y = malloc(cols*sizeof(uintB));
      #endif
      #endif
      end_system_call();
      term_initialized = TRUE;
      return NULL;
    }

#ifdef NL_HACK

# Wenn NLcap = "\n" ist, müssen wir ein "stty -onlcr" durchführen, weil sonst
# das NL vom Terminal-Driver in ein CR umgewandelt wird, bevor es beim
# Terminal ankommt.
  local void term_nlraw (void);
  local void term_nlunraw (void);
#if defined(UNIX_TERM_TERMIOS)
  static unsigned long old_c_oflag = 0;
  local void term_nlraw()
    { var struct termios oldtermio;
      if (!( tcgetattr(stdout_handle,&oldtermio) ==0))
        { if (!(errno==ENOTTY)) { OS_error(); } }
      old_c_oflag = oldtermio.c_oflag;
      oldtermio.c_oflag &= ~ONLCR;
      if (!( tcsetattr(stdout_handle,TCSAFLUSH,&oldtermio) ==0))
        { if (!(errno==ENOTTY)) { OS_error(); } }
    }
  local void term_nlunraw()
    { if (old_c_oflag & ONLCR)
        { var struct termios oldtermio;
          if (!( tcgetattr(stdout_handle,&oldtermio) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
          oldtermio.c_oflag |= ONLCR;
          if (!( tcsetattr(stdout_handle,TCSAFLUSH,&oldtermio) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
    }   }
#elif defined(UNIX_TERM_TERMIO) || defined(EMUNIX)
  static unsigned long old_c_oflag = 0;
  local void term_nlraw()
    { var struct termio oldtermio;
      if (!( ioctl(stdout_handle,TCGETA,&oldtermio) ==0))
        { if (!(errno==ENOTTY)) { OS_error(); } }
      old_c_oflag = oldtermio.c_oflag;
      oldtermio.c_oflag &= ~ONLCR;
      if (!( ioctl(stdout_handle,TCSETAF,&oldtermio) ==0))
        { if (!(errno==ENOTTY)) { OS_error(); } }
    }
  local void term_nlunraw()
    { if (old_c_oflag & ONLCR)
        { var struct termio oldtermio;
          if (!( ioctl(stdout_handle,TCGETA,&oldtermio) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
          oldtermio.c_oflag |= ONLCR;
          if (!( ioctl(stdout_handle,TCSETAF,&oldtermio) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
    }   }
#elif defined(UNIX_TERM_SGTTY)
  static unsigned long old_sg_flags = 0;
  local void term_nlraw()
    { var struct sgttyb oldsgttyb;
      if (!( ioctl(stdout_handle,TIOCGETP,&oldsgttyb) ==0))
        { if (!(errno==ENOTTY)) { OS_error(); } }
      old_sg_flags = oldsgttyb.sg_flags;
      oldsgttyb.sg_flags &= ~CRMOD;
      if (!( ioctl(stdout_handle,TIOCSETP,&oldsgttyb) ==0))
        { if (!(errno==ENOTTY)) { OS_error(); } }
    }
  local void term_nlunraw()
    { if (old_sg_flags & CRMOD)
        { var struct sgttyb oldsgttyb;
          if (!( ioctl(stdout_handle,TIOCGETP,&oldsgttyb) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
          oldsgttyb.sg_flags |= CRMOD;
          if (!( ioctl(stdout_handle,TIOCSETP,&oldsgttyb) ==0))
            { if (!(errno==ENOTTY)) { OS_error(); } }
    }   }
#endif

#endif # NL_HACK

# Beginn des Arbeitens mit diesem Paket:
  local void start_term (void);
  local void start_term()
    {
      #ifdef NL_HACK
      if (NLcap[0] == '\n') { term_nlraw(); }
      #endif
      out_capstring (IScap);
      out_capstring (TIcap);
    }

# Ende des Arbeitens mit diesem Paket:
  local void end_term (void);
  local void end_term()
    { out_capstring (TEcap);
      out_capstring (IScap);
      #ifdef MSDOS # wie testet man auf Farb-ANSI-Terminal??
      # Auf ANSI-Terminals mit mehreren Farben: TEcap setzt die Farben zurück.
      out_capstring(CLcap); # Bildschirm löschen, diesmal in der normalen Farbe
      #endif
      #ifdef NL_HACK
      if (NLcap[0] == '\n') { term_nlunraw(); }
      #endif
    }

# Initialisiert das Window curr.
  local void init_curr (void);
  local void init_curr()
    { {var reg1 uintB** ptr = malloc(rows*sizeof(uintB*));
       var reg2 uintC count;
       curr->image = ptr;
       dotimespC(count,rows, { *ptr++ = malloc(cols*sizeof(uintB)); } );
      }
      #if WANT_ATTR
      {var reg1 uintB** ptr = malloc(rows*sizeof(uintB*));
       var reg2 uintC count;
       curr->attr = ptr;
       dotimespC(count,rows, { *ptr++ = malloc(cols*sizeof(uintB)); } );
      }
      # Attribute ausschalten:
      out_capstring(UEcap); # alle aus
      out_capstring(SEcap);
      out_capstring(MEcap);
      term_attr = curr->curr_attr = 0;
      #endif
      #if WANT_CHARSET
      {var reg1 uintB** ptr = malloc(rows*sizeof(uintB*));
       var reg2 uintC count;
       curr->font = ptr;
       dotimespC(count,rows, { *ptr++ = malloc(cols*sizeof(uintB)); } );
      }
      {var reg1 uintC i = 0;
       while (i<charset_count) { curr->charsets[i] = ASCII; i++; }
      }
      curr->curr_charset = 0;
      if (ISO2022) { out_char(ESC); out_char('('); out_char('B'); } /*)*/
      term_charset = ASCII;
      #endif
      curr->x = 0; curr->y = 0;
      curr->top = 0; curr->bot = rows-1;
      #if WANT_INSERT
      curr->insert = FALSE;
      #endif
      #if WANT_SAVE
      curr->saved = FALSE;
      #endif
      if (CScap) { out_capstring(tgoto(CScap,curr->bot,curr->top)); }
      clear_screen();
    }

# ------------------------------------------------------------------------------

# UP: Ein Zeichen auf einen Window-Stream ausgeben.
# wr_ch_window(&stream,ch);
# > stream: Window-Stream
# > ch: auszugebendes Zeichen
  local void wr_ch_window (object* stream_, object ch);
  local void wr_ch_window(stream_,ch)
    var reg2 object* stream_;
    var reg3 object ch;
    { if (!string_char_p(ch)) { fehler_string_char(*stream_,ch); } # ch sollte String-Char sein
     {var reg1 uintB c = char_code(ch); # Code des Zeichens
      if (graphic_char_p(c))
        { if (curr->x == cols) { cursor_return(); cursor_linefeed(); } # Wrap!
          output_1char(c);
        }
      elif (c == NL)
        { cursor_return(); cursor_linefeed(); }
      elif (c == BS)
        { var reg4 int x0 = curr->x;
          if (x0>0)
            { var reg5 int y0 = curr->y;
              clear_linepart(y0,x0-1,x0);
              gofromto(curr->y,curr->x,y0,x0-1); curr->y = y0; curr->x = x0-1;
        }   }
    }}

LISPFUNN(make_window,0)
  { var reg2 object stream =
      allocate_stream(strmflags_wr_ch_B,strmtype_window,strm_len+1);
      # Flags: nur WRITE-CHAR erlaubt
    # und füllen:
    var reg1 Stream s = TheStream(stream);
      s->strm_rd_by = P(rd_by_dummy); # READ-BYTE unmöglich
      s->strm_wr_by = P(wr_by_dummy); # WRITE-BYTE unmöglich
      s->strm_rd_ch = P(rd_ch_dummy); # READ-CHAR unmöglich
      s->strm_rd_ch_last = NIL; # Lastchar := NIL
      s->strm_wr_ch = P(wr_ch_window); # WRITE-CHAR-Pseudofunktion
      s->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
      #ifdef STRM_WR_SS
      s->strm_wr_ss = P(wr_ss_dummy_nogc);
      #endif
    # Initialisieren:
    {var reg3 char* ergebnis = init_term();
     if (!(ergebnis==NULL)) { fehler(ergebnis); }
    }
    start_term();
    init_curr();
    value1 = stream; mv_count=1;
  }

# Schließt einen Window-Stream.
  local void close_window (object stream);
  local void close_window(stream)
    var reg1 object stream;
    { end_term(); }

LISPFUNN(window_size,1)
  { check_window_stream(popSTACK());
    value1 = fixnum(rows); # Variablen rows,cols abfragen
    value2 = fixnum(cols);
    mv_count=2;
  }

LISPFUNN(window_cursor_position,1)
  { check_window_stream(popSTACK());
    value1 = fixnum(curr->y);
    value2 = fixnum(curr->x);
    mv_count=2;
  }

LISPFUNN(set_window_cursor_position,3)
  { check_window_stream(STACK_2);
   {var reg1 uintL line = posfixnum_to_L(STACK_1);
    var reg2 uintL column = posfixnum_to_L(STACK_0);
    if ((line < rows) && (column < cols))
      { gofromto(curr->y,curr->x,line,column); # Cursor positionieren
        curr->y = line; curr->x = column;
      }
    value1 = STACK_1; value2 = STACK_0; mv_count=2; skipSTACK(3);
  }}

LISPFUNN(clear_window,1)
  { check_window_stream(popSTACK());
    clear_screen();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(clear_window_to_eot,1)
  { check_window_stream(popSTACK());
    clear_to_EOS();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(clear_window_to_eol,1)
  { check_window_stream(popSTACK());
    clear_to_EOL();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(delete_window_line,1)
  { check_window_stream(popSTACK());
    delete_line(1);
    value1 = NIL; mv_count=0;
  }

LISPFUNN(insert_window_line,1)
  { check_window_stream(popSTACK());
    insert_line(1);
    value1 = NIL; mv_count=0;
  }

LISPFUNN(highlight_on,1)
  { check_window_stream(popSTACK());
    change_attr(curr->curr_attr |= A_US);
    value1 = NIL; mv_count=0;
  }

LISPFUNN(highlight_off,1)
  { check_window_stream(popSTACK());
    change_attr(curr->curr_attr &= ~A_US);
    value1 = NIL; mv_count=0;
  }

LISPFUNN(window_cursor_on,1)
  { check_window_stream(popSTACK());
    # Cursor ist permanent an!
    value1 = NIL; mv_count=0;
  }

LISPFUNN(window_cursor_off,1)
  { check_window_stream(popSTACK());
    # geht nicht, da Cursor permanent an!
    value1 = NIL; mv_count=0;
  }

#endif # UNIX || EMUNIX_PORTABEL

#if (defined(UNIX) && 0) || defined(VMS)

# Normales CURSES-Paket, wir benutzen nur stdscr.

#undef BS
#undef CR
#undef NL
#include <curses.h>
#undef OK
#define CR  13
#define NL  10
#ifdef VMS
  #define cbreak()  crmode()
  #define nocbreak()  nocrmode()
#endif

# UP: Ein Zeichen auf einen Window-Stream ausgeben.
# wr_ch_window(&stream,ch);
# > stream: Window-Stream
# > ch: auszugebendes Zeichen
  local void wr_ch_window (object* stream_, object ch);
  local void wr_ch_window(stream_,ch)
    var reg2 object* stream_;
    var reg1 object ch;
    { if (!string_char_p(ch)) { fehler_string_char(*stream_,ch); } # ch sollte String-Char sein
     {var reg1 uintB c = char_code(ch); # Code des Zeichens
      begin_system_call();
      if (graphic_char_p(c)) # nur druckbare Zeichen auf den Bildschirm lassen
        { addch(c); }
      elif (c == NL) # NL in CR/LF umwandeln
        { addch(CR); addch(LF); }
      else # etwas ausgeben, damit die Cursorposition stimmt
        { addch('?'); }
      end_system_call();
    }}

LISPFUNN(make_window,0)
  { var reg2 object stream =
      allocate_stream(strmflags_wr_ch_B,strmtype_window,strm_len+1);
      # Flags: nur WRITE-CHAR erlaubt
    # und füllen:
    var reg1 Stream s = TheStream(stream);
      s->strm_rd_by = P(rd_by_dummy); # READ-BYTE unmöglich
      s->strm_wr_by = P(wr_by_dummy); # WRITE-BYTE unmöglich
      s->strm_rd_ch = P(rd_ch_dummy); # READ-CHAR unmöglich
      s->strm_rd_ch_last = NIL; # Lastchar := NIL
      s->strm_wr_ch = P(wr_ch_window); # WRITE-CHAR-Pseudofunktion
      s->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
      #ifdef STRM_WR_SS
      s->strm_wr_ss = P(wr_ss_dummy_nogc);
      #endif
    begin_system_call();
    initscr(); # Curses initialisieren # Was ist, wenn das abstürzt?? newterm() benutzen??
    cbreak(); noecho(); # Input nicht zeilengebuffert, ohne Echo
    #if defined(SUN3) || defined(SUN4)
    keypad(stdscr,TRUE); # Funktionstasten-Erkennung einschalten
    #endif
    end_system_call();
    value1 = stream; mv_count=1;
  }

# Schließt einen Window-Stream.
  local void close_window (object stream);
  local void close_window(stream)
    var reg1 object stream;
    { begin_system_call();
      nocbreak(); echo(); # Input wieder zeilengebuffert, mit Echo
      #if defined(SUN3) || defined(SUN4)
      keypad(stdscr,FALSE); # Funktionstasten-Erkennung wieder ausschalten
      #endif
      endwin(); # Curses abschalten
      end_system_call();
    }

LISPFUNN(window_size,1)
  { check_window_stream(popSTACK());
    value1 = fixnum(LINES); # Curses-Variablen LINES, COLS abfragen
    value2 = fixnum(COLS);
    mv_count=2;
  }

LISPFUNN(window_cursor_position,1)
  { check_window_stream(popSTACK());
   {var reg1 int y;
    var reg2 int x;
    begin_system_call();
    getyx(stdscr,y,x); # (y,x) := cursor position
    end_system_call();
    value1 = fixnum(y);
    value2 = fixnum(x);
    mv_count=2;
  }}

LISPFUNN(set_window_cursor_position,3)
  { check_window_stream(STACK_2);
   {var reg1 uintL line = posfixnum_to_L(STACK_1);
    var reg2 uintL column = posfixnum_to_L(STACK_0);
    if ((line < LINES) && (column < COLS))
      { begin_system_call();
        move(line,column); refresh(); # Cursor positionieren
        end_system_call();
      }
    value1 = STACK_1; value2 = STACK_0; mv_count=2; skipSTACK(3);
  }}

LISPFUNN(clear_window,1)
  { check_window_stream(popSTACK());
    begin_system_call();
    clear(); refresh();
    end_system_call();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(clear_window_to_eot,1)
  { check_window_stream(popSTACK());
    begin_system_call();
    clrtobot(); refresh();
    end_system_call();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(clear_window_to_eol,1)
  { check_window_stream(popSTACK());
    begin_system_call();
    clrtoeol(); refresh();
    end_system_call();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(delete_window_line,1)
  { check_window_stream(popSTACK());
    begin_system_call();
    deleteln(); refresh();
    end_system_call();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(insert_window_line,1)
  { check_window_stream(popSTACK());
    begin_system_call();
    insertln(); refresh();
    end_system_call();
    value1 = NIL; mv_count=0;
  }

LISPFUNN(highlight_on,1)
  { check_window_stream(popSTACK());
    #ifdef A_STANDOUT # geht nur, wenn Curses Attribute verwaltet
    begin_system_call();
    attron(A_STANDOUT); # Attribut A_STANDOUT bei addch() hineinoderieren
    end_system_call();
    #endif
    #ifdef VMS
    begin_system_call();
    standout();
    end_system_call();
    #endif
    value1 = NIL; mv_count=0;
  }

LISPFUNN(highlight_off,1)
  { check_window_stream(popSTACK());
    #ifdef A_STANDOUT # geht nur, wenn Curses Attribute verwaltet
    begin_system_call();
    attroff(A_STANDOUT); # kein Attribut mehr bei addch() hineinoderieren
    end_system_call();
    #endif
    #ifdef VMS
    begin_system_call();
    standend();
    end_system_call();
    #endif
    value1 = NIL; mv_count=0;
  }

LISPFUNN(window_cursor_on,1)
  { check_window_stream(popSTACK());
    # Cursor ist permanent an!
    value1 = NIL; mv_count=0;
  }

LISPFUNN(window_cursor_off,1)
  { check_window_stream(popSTACK());
    # geht nicht, da Cursor permanent an!
    value1 = NIL; mv_count=0;
  }

#endif # UNIX || VMS

#endif # WINDOWS


# File-Stream
# ===========

# Um nicht für jedes Character das GEMDOS/UNIX/AMIGADOS bemühen zu müssen,
# wird ein eigener Buffer geführt.
# (Dies bewirkt z.B. beim Einlesen eines 408 KByte- Files auf dem Atari
# eine Beschleunigung um einen Faktor 2.7 von 500 sec auf 180 sec.)

# Zusätzliche Komponenten:
  # define strm_file_name       strm_other[3] # Filename, ein Pathname
  # define strm_file_truename   strm_other[4] # Truename, ein Pathname
  # define strm_file_handle     strm_other[2] # Handle, ein Fixnum >=0, <2^16
  #define strm_file_buffstart   strm_other[0] # Buffer-Startposition, ein Fixnum >=0
  #define strm_file_bufflen     512           # Bufferlänge (Zweierpotenz <2^16)
  #define strm_file_buffer      strm_other[1] # eigener Buffer,
                                # ein Simple-String der Länge strm_file_bufflen
  #define strm_file_eofindex    strm_other[5] # Index darin, bis wo die
                                # Daten gültig sind (für EOF-Erkennung)
  #define strm_file_index       strm_other[6] # Fixnum mit Index im Buffer
                                              # (>=0, <=STRM_FILE_BUFFLEN)
                                              # und Modified-Flag in Bit 16.
  # eofindex = NIL: Bufferdaten ganz ungültig, index=0.
  # eofindex Fixnum: 0 <= index <= eofindex <= strm_file_bufflen.
  # eofindex = T: Bufferdaten ganz gültig, 0 <= index <= strm_file_bufflen.
  # buffstart = (Nummer des Sectors) * strm_file_bufflen.
  # Beim Betriebssystem ist das File 'handle' ans Ende des aktuellen Buffers
  #   positioniert:
  #   bei eofindex = T: buffstart + strm_file_bufflen,
  #   bei eofindex Fixnum: buffstart + eofindex,
  #   bei eofindex = NIL: buffstart.
  # Modified-Flag abfragen und verändern:
  #define modified_flag(stream)  \
    ((oint)(TheStream(stream)->strm_file_index) & wbit(16+oint_addr_shift))
  #define set_modified_flag(stream)  \
    TheStream(stream)->strm_file_index = \
      (object)((oint)(TheStream(stream)->strm_file_index) | wbit(16+oint_addr_shift))
  #define reset_modified_flag(stream)  \
    TheStream(stream)->strm_file_index = \
      (object)((oint)(TheStream(stream)->strm_file_index) & ~wbit(16+oint_addr_shift))
# Bis hierher wird ein File aus Bytes à 8 Bits aufgebaut gedacht.
# Logisch ist es jedoch aus anderen Einheiten aufgebaut:
  #define strm_file_position    strm_other[7] # Position, ein Fixnum >=0
  # Bei File-Streams mit element-type = STRING-CHAR (sch_file)
  #   belegt jedes Character 1 Byte.
  # Bei File-Streams mit element-type = CHARACTER (ch_file)
  #   belegt jedes Character 2 Bytes.
  # Bei File-Streams mit element-type = INTEGER ("Byte-Files")
  #   belegt jeder Integer immer dieselbe Anzahl Bits.
  #define strm_file_bitsize     strm_other[8] # Anzahl der Bits, ein Fixnum >0 und <intDsize*uintC_max
  #define strm_file_bitbuffer   strm_other[9] # Buffer, ein Simple-Bit-Vector
                                              # mit ceiling(bitsize/8)*8 Bits
  #   Ist diese Anzahl nicht durch 8 teilbar, so ist bitindex der Index
  #   im aktuellen Byte:
  #define strm_file_bitindex    strm_other[10] # Index im Byte, ein Fixnum >=0 und <=8
  #   8*index+bitindex ist die Anzahl der gelesenen Bits des Buffers.
  #   Die Bits sind in der Reihenfolge Bit0,...,Bit7 angeordnet.
  #   Ist Bitsize<8, so wird beim Schließen des Files die Länge des Files
  #   (gemessen in Bits) als .L am Anfang des Files abgelegt, die Daten
  #   fangen also erst beim 5. Byte an.
  #define strm_file_eofposition  strm_other[11] # Position des logischen EOF, ein Fixnum >=0
# Bei geschlossenen File-Streams sind nur die Komponenten name und truename
# relevant.

# File-Stream allgemein
# =====================

#if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX)
# Annahme: Alle von OPEN(2) gelieferten File-Descriptoren (hier Handles
# genannt) passen in ein uintW.
# Begründung: Bekanntlich ist 0 <= fd < getdtablesize() .
#endif

# Handle positionieren:
# file_lseek(stream,offset,mode,ergebnis_zuweisung);
# > mode: Positionierungsmodus:
#         SEEK_SET  "absolut"
#         SEEK_CUR  "relativ"
#         SEEK_END  "ab Ende"
# < ergebnis: neue Position
  #ifdef ATARI
    #define file_lseek(stream,offset,mode,ergebnis_zuweisung)  \
      { var reg2 sintL ergebnis =    \
          GEMDOS_Lseek(offset,       \
                       TheHandle(TheStream(stream)->strm_file_handle), # Handle \
                       mode          \
                      );             \
        if (ergebnis<0) { OS_error(ergebnis); } # Fehler aufgetreten? \
        ergebnis_zuweisung ergebnis; \
      }
    #define SEEK_SET  0
    #define SEEK_CUR  1
    #define SEEK_END  2
  #endif
  #if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX)
    #define file_lseek(stream,offset,mode,ergebnis_zuweisung)  \
      { var reg2 sintL ergebnis =    \
          lseek(TheHandle(TheStream(stream)->strm_file_handle), # Handle \
                offset,              \
                mode                 \
               );                    \
        if (ergebnis<0) { OS_error(); } # Fehler aufgetreten? \
        ergebnis_zuweisung ergebnis; \
      }
  #endif
  #ifdef AMIGAOS
    #define file_lseek(stream,offset,mode,ergebnis_zuweisung)  \
      { var reg3 uintL _offset = (offset);                     \
        var reg2 sintL ergebnis =                              \
          Seek(TheHandle(TheStream(stream)->strm_file_handle), \
               _offset,                                        \
               mode                                            \
              );                                               \
        if (ergebnis<0) { OS_error(); } # Fehler aufgetreten?  \
        if (mode==SEEK_SET) { ergebnis_zuweisung _offset; }    \
        elif (mode==SEEK_CUR) { ergebnis_zuweisung ergebnis+_offset; } \
        else /* mode==SEEK_END */                                      \
          { ergebnis = Seek(TheHandle(TheStream(stream)->strm_file_handle),0,SEEK_CUR); \
            if (ergebnis<0) { OS_error(); } # Fehler aufgetreten?      \
            ergebnis_zuweisung ergebnis;                               \
      }   }
    #define SEEK_SET  OFFSET_BEGINNING
    #define SEEK_CUR  OFFSET_CURRENT
    #define SEEK_END  OFFSET_END
  #endif

# UP: Beendet das Zurückschreiben des Buffers.
# b_file_finish_flush(stream,bufflen);
# > stream : (offener) Byte-basierter File-Stream.
# > bufflen : Anzahl der zu schreibenden Bytes
# < modified_flag von stream : gelöscht
# verändert in stream: index
  local void b_file_finish_flush (object stream, uintL bufflen);
  local void b_file_finish_flush(stream,bufflen)
    var reg1 object stream;
    var reg2 uintL bufflen;
    { begin_system_call();
     {var reg3 sintL ergebnis = # Buffer hinausschreiben
        #ifdef ATARI
          GEMDOS_write(TheHandle(TheStream(stream)->strm_file_handle), # Handle
                       bufflen,
                       untype(&TheSstring(TheStream(stream)->strm_file_buffer)->data[0]) # Bufferadresse ohne Typinfo
                      )
        #endif
        #if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX)
          write(TheHandle(TheStream(stream)->strm_file_handle), # Handle
                &TheSstring(TheStream(stream)->strm_file_buffer)->data[0], # Bufferadresse
                bufflen
               )
        #endif
        #ifdef AMIGAOS
          Write(TheHandle(TheStream(stream)->strm_file_handle),
                &TheSstring(TheStream(stream)->strm_file_buffer)->data[0], # Bufferadresse
                bufflen
               )
        #endif
        ;
      end_system_call();
      if (ergebnis==bufflen)
        # alles korrekt geschrieben
        { reset_modified_flag(stream); }
        else
        # Nicht alles geschrieben
        {
          #ifdef ATARI
          if (ergebnis<0) { OS_error(ergebnis); } # Fehler aufgetreten?
          #endif
          #if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX)
          if (ergebnis<0) # Fehler aufgetreten?
            #ifdef ENOSPC
            #ifdef UNIX_LINUX # nötig??
            if (!(ergebnis == -ENOSPC))
            #endif
            if (!(errno == ENOSPC))
            #endif
              { OS_error(); }
          #endif
          #ifdef AMIGAOS
          if (ergebnis<0) { OS_error(); } # Fehler aufgetreten?
          #endif
          # Nicht alles geschrieben, wohl wegen voller Diskette.
          # Um Inkonsistenzen zu vermeiden, muß man das File schließen.
          reset_modified_flag(stream); # Hierbei gehen Daten verloren!
          pushSTACK(stream);
          stream_close(&STACK_0); # File schließen
          clr_break_sem_4(); # keine GEMDOS/UNIX-Operation mehr aktiv
          # Fehler melden. STACK_0 = stream
          fehler(
                 DEUTSCH ? "Diskette/Platte voll. Deswegen wurde ~ geschlossen." :
                 ENGLISH ? "Closed ~ because disk is full." :
                 FRANCAIS ? "Ai fermé ~, parce que le disque est sans doute plein." :
                 ""
                );
    }}  }

# UP: Schreibt den vollen, modifizierten Buffer zurück.
# b_file_full_flush(stream);
# > stream : (offener) Byte-basierter File-Stream.
# < modified_flag von stream : gelöscht
# verändert in stream: index
  local void b_file_full_flush (object stream);
  local void b_file_full_flush(stream)
    var reg1 object stream;
    { # erst zurückpositionieren, dann schreiben.
      begin_system_call();
      file_lseek(stream,-strm_file_bufflen,SEEK_CUR,); # Zurückpositionieren
      end_system_call();
      b_file_finish_flush(stream,strm_file_bufflen);
    }

# UP: Schreibt den halbvollen, modifizierten Buffer zurück.
# b_file_full_flush(stream);
# > stream : (offener) Byte-basierter File-Stream.
# < modified_flag von stream : gelöscht
# verändert in stream: index
  local void b_file_half_flush (object stream);
  local void b_file_half_flush(stream)
    var reg1 object stream;
    { begin_system_call();
      file_lseek(stream,posfixnum_to_L(TheStream(stream)->strm_file_buffstart),SEEK_SET,); # Zurückpositionieren
      end_system_call();
      # eofindex Bytes schreiben:
      b_file_finish_flush(stream,
                          posfixnum_to_L(TheStream(stream)->strm_file_eofindex)
                         );
    }

# UP: Schreibt den modifizierten Buffer zurück.
# b_file_flush(stream);
# > stream : (offener) Byte-basierter File-Stream.
# < modified_flag von stream : gelöscht
# verändert in stream: index
  local void b_file_flush (object stream);
  local void b_file_flush(stream)
    var reg1 object stream;
    { if (eq(TheStream(stream)->strm_file_eofindex,T)) # Buffer ganz gültig ?
        { b_file_full_flush(stream); }
        else
        { b_file_half_flush(stream); }
    }

# UP: Positioniert einen Byte-basierten File-Stream so, daß das nächste Byte
# gelesen oder überschrieben werden kann.
# b_file_nextbyte(stream)
# > stream : (offener) Byte-basierter File-Stream.
# < ergebnis : NULL falls EOF (und dann ist index=eofindex),
#              sonst: Pointer auf nächstes Byte
# verändert in stream: index, eofindex, buffstart
  local uintB* b_file_nextbyte (object stream);
  local uintB* b_file_nextbyte(stream)
    var reg1 object stream;
    { var reg2 object eofindex = TheStream(stream)->strm_file_eofindex;
      var reg3 object index = TheStream(stream)->strm_file_index;
      if (!eq(eofindex,T))
        # Bufferdaten nur halb gültig
        { if (eq(eofindex,NIL))
            # Bufferdaten ganz ungültig
            { goto reread; }
            else
            # EOF tritt in diesem Sector auf
            { goto eofsector; }
        }
      # Bufferdaten ganz gültig
      if (!((uintW)posfixnum_to_L(index) == strm_file_bufflen)) # index = bufflen ?
        # nein, also 0 <= index < strm_file_bufflen -> OK
        { return &TheSstring(TheStream(stream)->strm_file_buffer)->data[(uintW)posfixnum_to_L(index)]; }
      # Buffer muß neu gefüllt werden.
      if (modified_flag(stream))
        # Zuvor muß der Buffer hinausgeschrieben werden:
        { b_file_full_flush(stream); }
      TheStream(stream)->strm_file_buffstart =
        fixnum_inc(TheStream(stream)->strm_file_buffstart,strm_file_bufflen);
      reread: # Ab hier den Buffer neu lesen:
      { begin_system_call();
       {var reg4 sintL ergebnis = # Buffer füllen
          #ifdef ATARI
            GEMDOS_read(TheHandle(TheStream(stream)->strm_file_handle), # Handle
                        strm_file_bufflen,
                        untype(&TheSstring(TheStream(stream)->strm_file_buffer)->data[0]) # Bufferadresse ohne Typinfo
                       )
          #endif
          #if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX)
            read(TheHandle(TheStream(stream)->strm_file_handle), # Handle
                 &TheSstring(TheStream(stream)->strm_file_buffer)->data[0], # Bufferadresse
                 strm_file_bufflen
                )
          #endif
          #ifdef AMIGAOS
            Read(TheHandle(TheStream(stream)->strm_file_handle),
                 &TheSstring(TheStream(stream)->strm_file_buffer)->data[0], # Bufferadresse
                 strm_file_bufflen
                )
          #endif
          ;
        end_system_call();
        if (ergebnis==strm_file_bufflen)
          # der ganze Buffer wurde gefüllt
          { TheStream(stream)->strm_file_index = Fixnum_0; # Index := 0, Buffer unmodifiziert
            TheStream(stream)->strm_file_eofindex = T; # eofindex := T
            return &TheSstring(TheStream(stream)->strm_file_buffer)->data[0];
          }
        #ifdef ATARI
        if (ergebnis<0) { OS_error(ergebnis); } # Fehler aufgetreten?
        #endif
        #if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX)
        if (ergebnis<0) { OS_error(); } # Fehler aufgetreten?
        #endif
        #ifdef AMIGAOS
        if (ergebnis<0) { OS_error(); } # Fehler aufgetreten?
        #endif
        # Es wurden ergebnis (< strm_file_bufflen) Bytes gelesen.
        # Nicht der ganze Buffer wurde gefüllt -> EOF ist erreicht.
        TheStream(stream)->strm_file_index = index = Fixnum_0; # Index := 0, Buffer unmodifiziert
        TheStream(stream)->strm_file_eofindex = eofindex = fixnum(ergebnis); # eofindex := ergebnis
      }}
      eofsector: # eofindex ist ein Fixnum, d.h. EOF tritt in diesem Sector auf.
      if ((uintW)posfixnum_to_L(index) == (uintW)posfixnum_to_L(eofindex))
        { return (uintB*)NULL; } # EOF erreicht
        else
        { return &TheSstring(TheStream(stream)->strm_file_buffer)->data[(uintW)posfixnum_to_L(index)]; }
    }

# UP: Bereitet das Schreiben eines Bytes am EOF vor.
# b_file_eofbyte(stream);
# > stream : (offener) Byte-basierter File-Stream,
#            bei dem gerade b_file_nextbyte(stream)==NULL ist.
# < ergebnis : Pointer auf nächstes (freies) Byte
# verändert in stream: index, eofindex, buffstart
  local uintB* b_file_eofbyte (object stream);
  local uintB* b_file_eofbyte(stream)
    var reg1 object stream;
    { # EOF. Es ist eofindex=index.
      if (eq(TheStream(stream)->strm_file_eofindex,
             fixnum(strm_file_bufflen)
         )  )
        # eofindex = strm_file_bufflen
        { # Buffer muß neu gefüllt werden. Da nach ihm sowieso EOF kommt,
          # genügt es, ihn hinauszuschreiben:
          if (modified_flag(stream)) { b_file_half_flush(stream); }
          TheStream(stream)->strm_file_buffstart =
            fixnum_inc(TheStream(stream)->strm_file_buffstart,strm_file_bufflen);
          TheStream(stream)->strm_file_eofindex = Fixnum_0; # eofindex := 0
          TheStream(stream)->strm_file_index = Fixnum_0; # index := 0, unmodifiziert
        }
      # eofindex erhöhen:
      TheStream(stream)->strm_file_eofindex = fixnum_inc(TheStream(stream)->strm_file_eofindex,1);
      return &TheSstring(TheStream(stream)->strm_file_buffer)->data[(uintW)posfixnum_to_L(TheStream(stream)->strm_file_index)];
    }

# UP: Schreibt ein Byte auf einen Byte-basierten File-Stream.
# b_file_writebyte(stream,b);
# > stream : (offener) Byteblock-basierter File-Stream.
# > b : zu schreibendes Byte
# verändert in stream: index, eofindex, buffstart
  local void b_file_writebyte (object stream, uintB b);
  local void b_file_writebyte(stream,b)
    var reg1 object stream;
    var reg3 uintB b;
    { var reg2 uintB* ptr = b_file_nextbyte(stream);
      if (!(ptr == (uintB*)NULL))
        { if (*ptr == b) goto no_modification; } # keine wirkliche Modifikation?
        else
        { ptr = b_file_eofbyte(stream); } # EOF -> 1 Byte Platz machen
      # nächstes Byte in den Buffer schreiben:
      *ptr = b; set_modified_flag(stream);
      no_modification:
      # index incrementieren:
      TheStream(stream)->strm_file_index = fixnum_inc(TheStream(stream)->strm_file_index,1);
    }

# File-Stream, Byte-basiert (b_file)
# ===========  ============

# Fehler wegen Positionierung hinter EOF.
# fehler_position_beyond_EOF(stream);
  local nonreturning void fehler_position_beyond_EOF (object stream);
  local nonreturning void fehler_position_beyond_EOF(stream)
    var reg1 object stream;
    { pushSTACK(stream);
      fehler(
             DEUTSCH ? "Positionierung von ~ hinter EOF unmöglich." :
             ENGLISH ? "cannot position ~ beyond EOF" :
             FRANCAIS ? "Ne peux pas positionner ~ au-delà de la fin du fichier." :
             ""
            );
    }

# UP: Positioniert einen (offenen) Byte-basierten File-Stream an eine
# gegebene Position.
# position_b_file(stream,position);
# > stream : (offener) Byte-basierter File-Stream.
# > position : neue Position
# verändert in stream: index, eofindex, buffstart
  local void position_b_file (object stream, uintL position);
  local void position_b_file(stream,position)
    var reg1 object stream;
    var reg2 uintL position;
    { # Liegt die neue Position im selben Sector?
      { var reg3 object eofindex = TheStream(stream)->strm_file_eofindex;
        var reg4 uintL newindex = position - posfixnum_to_L(TheStream(stream)->strm_file_buffstart);
        if (newindex
            <= (eq(eofindex,T) ? strm_file_bufflen :
                (!eq(eofindex,NIL)) ? posfixnum_to_L(eofindex) :
                0
           )   )
          { # ja -> brauche nur index zu verändern:
            # (Dabei aber das modified_flag erhalten!)
            TheStream(stream)->strm_file_index =
              (modified_flag(stream)
               ? fixnum(bit(16)+newindex)
               : fixnum(newindex)
              );
            return;
      }   }
      # evtl. Buffer hinausschreiben:
      if (modified_flag(stream)) { b_file_flush(stream); }
      # Nun ist modified_flag gelöscht.
     {var reg5 uintL oldposition = posfixnum_to_L(TheStream(stream)->strm_file_buffstart)
                                   + posfixnum_to_L(TheStream(stream)->strm_file_index);
      # Positionieren:
      { var reg4 uintL newposition;
        begin_system_call();
        file_lseek(stream,floor(position,strm_file_bufflen)*strm_file_bufflen,SEEK_SET,newposition=);
        end_system_call();
        TheStream(stream)->strm_file_buffstart = fixnum(newposition);
      }
      # Sector lesen:
      TheStream(stream)->strm_file_eofindex = NIL; # eofindex := NIL
      TheStream(stream)->strm_file_index = Fixnum_0; # index := 0, unmodifiziert
      { var reg3 uintL newindex = position % strm_file_bufflen; # gewünschter Index im Sector
        if (!(newindex==0)) # Position zwischen Sectoren -> brauche nichts zu lesen
          { b_file_nextbyte(stream);
            # Jetzt ist index=0.
            # index auf (position mod bufflen) setzen, vorher überprüfen:
           {var reg4 object eofindex = TheStream(stream)->strm_file_eofindex;
            # Es muß entweder eofindex=T oder 0<=newindex<=eofindex sein:
            if (!(eq(eofindex,T) || (newindex <= posfixnum_to_L(eofindex))))
              # Fehler. Aber erst an die alte Position zurückpositionieren:
              { check_SP();
                position_b_file(stream,oldposition); # zurückpositionieren
                fehler_position_beyond_EOF(stream);
              }
            TheStream(stream)->strm_file_index = fixnum(newindex);
      }   }}
    }}

# File-Stream für String-Chars
# ============================

# Funktionsweise:
# Beim Schreiben: Characters werden unverändert durchgereicht, nur NL wird auf
# ATARI/MSDOS in CR/LF umgewandelt. Beim Lesen: CR/LF wird in NL umgewandelt.

# READ-CHAR - Pseudofunktion für File-Streams für String-Chars
  local object rd_ch_sch_file (object* stream_);
  local object rd_ch_sch_file(stream_)
    var reg3 object* stream_;
    { var reg1 object stream = *stream_;
      var reg2 uintB* charptr = b_file_nextbyte(stream);
      if (charptr == (uintB*)NULL) { return eof_value; } # EOF ?
      # nächstes Zeichen holen:
     {var reg3 object ch = code_char(*charptr); # Character aus dem Buffer holen
      # index und position incrementieren:
      TheStream(stream)->strm_file_index = fixnum_inc(TheStream(stream)->strm_file_index,1);
      TheStream(stream)->strm_file_position = fixnum_inc(TheStream(stream)->strm_file_position,1);
      # ch = nächstes Zeichen
      if (!eq(ch,code_char(CR))) # Ist es CR ?
        { return ch; } # nein -> OK
      # ja -> nächstes Zeichen auf LF untersuchen
      charptr = b_file_nextbyte(stream);
      if (charptr == (uintB*)NULL) { return ch; } # EOF -> bleibt CR
      if (!(*charptr == LF)) { return ch; } # kein LF -> bleibt CR
      # LF übergehen, index und position incrementieren:
      TheStream(stream)->strm_file_index = fixnum_inc(TheStream(stream)->strm_file_index,1);
      TheStream(stream)->strm_file_position = fixnum_inc(TheStream(stream)->strm_file_position,1);
      # NL als Ergebnis:
      return code_char(NL);
    }}

# Stellt fest, ob ein File-Stream ein Zeichen verfügbar hat.
# listen_sch_file(stream)
# > stream: File-Stream für String-Chars
# < ergebnis:  0 falls Zeichen verfügbar,
#             -1 falls bei EOF angelangt,
#             +1 falls kein Zeichen verfügbar, aber nicht wegen EOF
  local signean listen_sch_file (object stream);
  local signean listen_sch_file(stream)
    var reg1 object stream;
    { if (b_file_nextbyte(stream) == (uintB*)NULL)
        { return signean_minus; } # EOF
        else
        { return signean_null; }
    }

# UP: Schreibt ein Byte auf einen Byte-basierten File-Stream.
# write_b_file(stream,b);
# > stream : (offener) Byte-basierter File-Stream.
# > b : zu schreibendes Byte
# verändert in stream: index, eofindex, buffstart, position
  local void write_b_file (object stream, uintB b);
  local void write_b_file(stream,b)
    var reg1 object stream;
    var reg2 uintB b;
    { b_file_writebyte(stream,b);
      # position incrementieren:
      TheStream(stream)->strm_file_position = fixnum_inc(TheStream(stream)->strm_file_position,1);
    }

# WRITE-CHAR - Pseudofunktion für File-Streams für String-Chars
  local void wr_ch_sch_file (object* stream_, object obj);
  local void wr_ch_sch_file(stream_,obj)
    var reg4 object* stream_;
    var reg2 object obj;
    { var reg1 object stream = *stream_;
      # obj muß ein String-Char sein:
      if (!string_char_p(obj)) { fehler_string_char(stream,obj); }
     {var reg3 uintB ch = char_code(obj);
      #if defined(ATARI) || defined(MSDOS) || defined(VMS)
      if (ch==NL)
        # Newline als CR/LF ausgeben
        { write_b_file(stream,CR); write_b_file(stream,LF); }
        else
        # alle anderen Zeichen unverändert ausgeben
        { write_b_file(stream,ch); }
      #else
      write_b_file(stream,ch); # unverändert ausgeben
      #endif
    }}

#ifdef STRM_WR_SS
# WRITE-SIMPLE-STRING - Pseudofunktion für File-Streams für String-Chars
 #if defined(ATARI) || defined(MSDOS) || defined(VMS)
  # Wegen NL->CR/LF-Umwandlung keine Optimierung möglich.
  #define wr_ss_sch_file  wr_ss_dummy_nogc
 #else
  local void wr_ss_sch_file (object* stream_, object string, uintL start, uintL len);
  local void wr_ss_sch_file(stream_,string,start,len)
    var reg9 object* stream_;
    var reg10 object string;
    var reg10 uintL start;
    var reg10 uintL len;
    { if (len==0) return;
     {var reg5 object stream = *stream_;
      var reg2 uintB* strptr = &TheSstring(string)->data[start];
      var reg6 uintL remaining = len;
      var reg1 uintB* ptr;
      do # Noch remaining>0 Bytes abzulegen.
        { ptr = b_file_nextbyte(stream);
          if (ptr == (uintB*)NULL) goto eof_reached;
         {var reg8 object eofindex = TheStream(stream)->strm_file_eofindex;
          var reg7 uintL next = # so viel wie noch in den Buffer oder bis EOF paßt
            (eq(eofindex,T) ? strm_file_bufflen : posfixnum_to_L(eofindex))
            - (uintW)posfixnum_to_L(TheStream(stream)->strm_file_index); # > 0 !
          if (next > remaining) { next = remaining; }
          # next Bytes in den Buffer kopieren:
          {var reg4 uintL count;
           dotimespL(count,next,
             { var reg3 uintB b = *strptr++; # nächstes Byte
               if (!(*ptr == b)) { *ptr = b; set_modified_flag(stream); } # in den Buffer
               ptr++;
             });
          }
          remaining = remaining - next;
          # index incrementieren:
          TheStream(stream)->strm_file_index =
            fixnum_inc(TheStream(stream)->strm_file_index,next);
        }}
        until (remaining == 0);
      if (FALSE)
        eof_reached: # Schreiben am EOF, eofindex = index
        do # Noch remaining>0 Bytes abzulegen.
          { var reg7 uintL next = # so viel wie noch Platz im Buffer ist
              strm_file_bufflen
              - (uintW)posfixnum_to_L(TheStream(stream)->strm_file_index);
            if (next==0)
              { # Buffer muß neu gefüllt werden. Da nach ihm sowieso EOF kommt,
                # genügt es, ihn hinauszuschreiben:
                if (modified_flag(stream)) { b_file_half_flush(stream); }
                TheStream(stream)->strm_file_buffstart =
                  fixnum_inc(TheStream(stream)->strm_file_buffstart,strm_file_bufflen);
                TheStream(stream)->strm_file_eofindex = Fixnum_0; # eofindex := 0
                TheStream(stream)->strm_file_index = Fixnum_0; # index := 0, unmodifiziert
                # Dann nochmals versuchen:
                next = strm_file_bufflen;
              }
            if (next > remaining) { next = remaining; }
            # next Bytes in den Buffer kopieren:
            {var reg3 uintL count;
             ptr = &TheSstring(TheStream(stream)->strm_file_buffer)->data[(uintW)posfixnum_to_L(TheStream(stream)->strm_file_index)];
             dotimespL(count,next, { *ptr++ = *strptr++; } );
             set_modified_flag(stream);
            }
            remaining = remaining - next;
            # index und eofindex incrementieren:
            TheStream(stream)->strm_file_index =
              fixnum_inc(TheStream(stream)->strm_file_index,next);
            TheStream(stream)->strm_file_eofindex =
              fixnum_inc(TheStream(stream)->strm_file_eofindex,next);
          }
          until (remaining == 0);
      # position incrementieren:
      TheStream(stream)->strm_file_position = fixnum_inc(TheStream(stream)->strm_file_position,len);
      wr_ss_lpos(stream,strptr,len); # Line-Position aktualisieren
    }}
 #endif
#endif

# File-Stream für Characters
# ==========================

# Funktionsweise:
# Characters werden incl. Fonts und Bits durchgereicht.
  #if (!((char_int_len % 8) == 0)) # char_int_len muß durch 8 teilbar sein
    #error "Charactergröße neu einstellen!"
  #endif
  #define char_size  (char_int_len / 8)  # Größe eines Characters in Bytes

# READ-CHAR - Pseudofunktion für File-Streams für Characters
  local object rd_ch_ch_file (object* stream_);
  local object rd_ch_ch_file(stream_)
    var reg4 object* stream_;
    { var reg1 object stream = *stream_;
      var reg3 cint c;
      var reg2 uintB* ptr = b_file_nextbyte(stream);
      if (ptr == (uintB*)NULL) goto eof; # EOF ?
      c = *ptr;
      # index incrementieren:
      TheStream(stream)->strm_file_index = fixnum_inc(TheStream(stream)->strm_file_index,1);
      doconsttimes(char_size-1,
        ptr = b_file_nextbyte(stream);
        if (ptr == (uintB*)NULL) goto eof1; # EOF ?
        c = (c<<8) | *ptr;
        # index incrementieren:
        TheStream(stream)->strm_file_index = fixnum_inc(TheStream(stream)->strm_file_index,1);
        );
      # position incrementieren:
      TheStream(stream)->strm_file_position = fixnum_inc(TheStream(stream)->strm_file_position,1);
      return int_char(c);
      eof1:
        # Wieder zurückpositionieren:
        position_b_file(stream,posfixnum_to_L(TheStream(stream)->strm_file_position) * char_size);
      eof: # EOF erreicht gewesen
        return eof_value;
    }

# Stellt fest, ob ein File-Stream ein Zeichen verfügbar hat.
# listen_ch_file(stream)
# > stream: File-Stream für Characters
# < ergebnis:  0 falls Zeichen verfügbar,
#             -1 falls bei EOF angelangt,
#             +1 falls kein Zeichen verfügbar, aber nicht wegen EOF
# kann GC auslösen
  local signean listen_ch_file (object stream);
  local signean listen_ch_file(stream)
    var reg1 object stream;
    { var reg2 uintB* ptr = b_file_nextbyte(stream); # erstes Byte da ?
      if (ptr == (uintB*)NULL) goto eof; # EOF ?
      doconsttimes(char_size-1,
        # index incrementieren:
        TheStream(stream)->strm_file_index = fixnum_inc(TheStream(stream)->strm_file_index,1);
        ptr = b_file_nextbyte(stream); # nächstes Byte da ?
        if (ptr == (uintB*)NULL) goto eof1; # EOF ?
        );
      # Wieder zurückpositionieren:
      position_b_file(stream,posfixnum_to_L(TheStream(stream)->strm_file_position) * char_size);
      return signean_null;
      eof1:
        # Wieder zurückpositionieren:
        position_b_file(stream,posfixnum_to_L(TheStream(stream)->strm_file_position) * char_size);
      eof: # EOF erreicht gewesen
        return signean_minus;
    }

# WRITE-CHAR - Pseudofunktion für File-Streams für Characters
  local void wr_ch_ch_file (object* stream_, object obj);
  local void wr_ch_ch_file(stream_,obj)
    var reg4 object* stream_;
    var reg2 object obj;
    { var reg1 object stream = *stream_;
      # obj muß ein Character sein:
      if (!charp(obj)) { fehler_char(stream,obj); }
     {var reg3 cint c = char_int(obj);
      #define WRITEBYTE(i)  b_file_writebyte(stream,(uintB)(c>>(char_size-1-i)));
      DOCONSTTIMES(char_size,WRITEBYTE) # WRITEBYTE(0..char_size-1)
      #undef WRITEBYTE
      # position incrementieren:
      TheStream(stream)->strm_file_position = fixnum_inc(TheStream(stream)->strm_file_position,1);
    }}

# File-Stream, Bit-basiert
# ========================

# Davon gibt es insgesamt 6 Arten:
# Drei Fälle
#   a - bitsize durch 8 teilbar,
#   b - bitsize < 8,
#   c - bitsize nicht durch 8 teilbar und >= 8,
# jeweils unterschieden durch
#   s - Elementtyp (signed-byte bitsize),
#       dazu zählt auch signed-byte = (signed-byte 8)
#   u - Elementtyp (unsigned-byte bitsize),
#       dazu zählen auch unsigned-byte = (unsigned-byte 8)
#       und bit = (unsigned-byte 1)
#       und (mod n) = (unsigned-byte (integer-length n))

# UP: Positioniert einen (offenen) Bit-basierten File-Stream an eine
# gegebene Position.
# position_i_file(stream,position);
# > stream : (offener) Byte-basierter File-Stream.
# > position : neue (logische) Position
# verändert in stream: index, eofindex, buffstart, bitindex
  local void position_i_file (object stream, uintL position);
  local void position_i_file(stream,position)
    var reg1 object stream;
    var reg2 uintL position;
    { var reg5 uintB flags = TheStream(stream)->strmflags;
      var reg4 uintL bitsize = posfixnum_to_L(TheStream(stream)->strm_file_bitsize);
      var reg3 uintL position_bits = position * bitsize;
      if (flags & strmflags_ib_B)
        { position_bits += sizeof(uintL)*8; } # Header berücksichtigen
      # An Bit Nummer position_bits positionieren.
      position_b_file(stream,floor(position_bits,8)); # Aufs Byte positionieren
      if (flags & strmflags_ia_B) return; # Bei Art a war's das.
      if (# Liegt die angesprochene Position im ersten Byte nach EOF ?
          ((!((position_bits%8)==0))
           && (b_file_nextbyte(stream) == (uintB*)NULL)
          )
          ||
          # Liegt die angesprochene Position im letzten Byte, aber zu weit?
          ((flags & strmflags_ib_B)
           && (position > posfixnum_to_L(TheStream(stream)->strm_file_eofposition))
         ))
        # Fehler. Aber erst an die alte Position zurückpositionieren:
        { var reg6 uintL oldposition = posfixnum_to_L(TheStream(stream)->strm_file_position);
          check_SP();
          position_i_file(stream,oldposition); # zurückpositionieren
          fehler_position_beyond_EOF(stream);
        }
      TheStream(stream)->strm_file_bitindex = fixnum(position_bits%8);
    }

# UP für READ-BYTE auf File-Streams für Integers, Art u :
# Liefert die im Bitbuffer enthaltenen bytesize Bytes als Integer >=0.
# kann GC auslösen
  local object rd_by_iu_I (object stream, uintL bitsize, uintL bytesize);
  local object rd_by_iu_I(stream,bitsize,bytesize)
    var reg6 object stream;
    var reg7 uintL bitsize;
    var reg5 uintL bytesize;
    { var reg4 object bitbuffer = TheStream(stream)->strm_file_bitbuffer;
      # Zahl im bitbuffer normalisieren:
      var reg1 uintB* bitbufferptr = &TheSbvector(bitbuffer)->data[0];
      *bitbufferptr &= (bit(((bitsize-1)%8)+1)-1); # High byte maskieren
     {var reg2 uintL count = bytesize;
      while ((!(count==0)) && (*bitbufferptr==0)) { count--; bitbufferptr++; }
      # Zahl bilden:
      if # höchstens oint_addr_len Bits ?
         ((count <= floor(oint_addr_len,8))
          || ((count == floor(oint_addr_len,8)+1)
              && (*bitbufferptr < bit(oint_addr_len%8))
         )   )
        # ja -> Fixnum >=0 bilden:
        { var reg3 uintL wert = 0;
          until (count==0) { wert = (wert<<8) | *bitbufferptr++; count--; }
          return fixnum(wert);
        }
        else
        # nein -> Bignum >0 bilden:
        { pushSTACK(bitbuffer);
         {var reg5 uintL digitcount = floor(count,(intDsize/8));
          if (((count%(intDsize/8)) > 0) || (*bitbufferptr & bit(7)))
            { digitcount++; }
          # Da bitsize < intDsize*uintC_max, ist
          # digitcount <= ceiling((bitsize+1)/intDsize) <= uintC_max .
          { var reg4 object big = allocate_bignum(digitcount,0); # neues Bignum >0
            TheBignum(big)->data[0] = 0; # höchstes Digit auf 0 setzen
            # restliche Digits von rechts füllen, dabei Folge von Bytes in
            # Folge von uintD übersetzen:
            bitbuffer = popSTACK();
            bitbufferptr = &TheSbvector(bitbuffer)->data[bytesize];
            #if BIG_ENDIAN_P
            {var reg1 uintB* bigptr = (uintB*)(&TheBignum(big)->data[digitcount]);
             dotimespL(count,count, { *--bigptr = *--bitbufferptr; } );
            }
            #else
            {var reg1 uintD* bigptr = &TheBignum(big)->data[digitcount];
             var reg6 uintL count2;
             #define GET_NEXT_BYTE(i)  \
               digit |= ((uintD)(*--bitbufferptr) << (8*i));
             dotimespL(count2,floor(count,intDsize/8),
               { var reg3 uintD digit = 0;
                 DOCONSTTIMES(intDsize/8,GET_NEXT_BYTE); # GET_NEXT_BYTE(0..intDsize/8-1)
                 *--bigptr = digit;
               });
             #undef GET_NEXT_BYTE
             count2 = count % (intDsize/8);
             if (count2>0)
               { var reg7 uintL shiftcount = 0;
                 var reg3 uintD digit = (uintD)(*--bitbufferptr);
                 dotimesL(count2,count2-1,
                   { shiftcount += 8;
                     digit |= ((uintD)(*--bitbufferptr) << shiftcount);
                   });
                 *--bigptr = digit;
               }
            }
            #endif
            # Wegen (intDsize/8)*(digitcount-1) <= count <= (intDsize/8)*digitcount
            # ist alles gefüllt.
            return big;
        }}}
    }}

# UP für READ-BYTE auf File-Streams für Integers, Art s :
# Liefert die im Bitbuffer enthaltenen bytesize Bytes als Integer.
# kann GC auslösen
  local object rd_by_is_I (object stream, uintL bitsize, uintL bytesize);
  local object rd_by_is_I(stream,bitsize,bytesize)
    var reg6 object stream;
    var reg7 uintL bitsize;
    var reg5 uintL bytesize;
    { var reg4 object bitbuffer = TheStream(stream)->strm_file_bitbuffer;
      # Zahl im bitbuffer normalisieren:
      var reg1 uintB* bitbufferptr = &TheSbvector(bitbuffer)->data[0];
      var reg8 sintD sign;
      var reg3 uintL signbitnr = (bitsize-1)%8;
      var reg2 uintL count = bytesize;
      if (!(*bitbufferptr & bit(signbitnr)))
        { sign = 0;
          *bitbufferptr &= (bitm(signbitnr+1)-1); # High byte sign-extenden
          # normalisieren, höchstes Bit muß 0 bleiben:
          while ((count>=2) && (*bitbufferptr==0) && !(*(bitbufferptr+1) & bit(7)))
            { count--; bitbufferptr++; }
          # Zahl bilden:
          if # höchstens oint_addr_len+1 Bits, Zahl <2^oint_addr_len ?
             ((count <= floor(oint_addr_len,8))
              || ((count == floor(oint_addr_len,8)+1)
                  && (*bitbufferptr < bit(oint_addr_len%8))
             )   )
            # ja -> Fixnum >=0 bilden:
            { var reg3 uintL wert = 0;
              until (count==0) { wert = (wert<<8) | *bitbufferptr++; count--; }
              return fixnum_inc(Fixnum_0,wert);
            }
        }
        else
        { sign = -1;
          *bitbufferptr |= minus_bitm(signbitnr+1); # High byte sign-extenden
          # normalisieren, höchstes Bit muß 1 bleiben:
          while ((count>=2) && (*bitbufferptr==(uintB)(-1)) && (*(bitbufferptr+1) & bit(7)))
            { count--; bitbufferptr++; }
          # Zahl bilden:
          if # höchstens oint_addr_len+1 Bits, Zahl >=-2^oint_addr_len ?
             ((count <= floor(oint_addr_len,8))
              || ((count == floor(oint_addr_len,8)+1)
                  && (*bitbufferptr >= (uintB)(-bit(oint_addr_len%8)))
             )   )
            # ja -> Fixnum <0 bilden:
            { var reg3 uintL wert = -1;
              until (count==0) { wert = (wert<<8) | *bitbufferptr++; count--; }
              return fixnum_inc(fixnum_inc(Fixnum_minus1,1),wert);
            }
        }
      # Bignum bilden:
      pushSTACK(bitbuffer);
      { var reg5 uintL digitcount = ceiling(count,(intDsize/8));
        # Da bitsize < intDsize*uintC_max, ist
        # digitcount <= ceiling(bitsize/intDsize) <= uintC_max .
        var reg4 object big = allocate_bignum(digitcount,sign); # neues Bignum
        TheBignum(big)->data[0] = sign; # höchstes Word auf sign setzen
        # restliche Digits von rechts füllen, dabei Folge von Bytes in
        # Folge von uintD übersetzen:
        bitbuffer = popSTACK();
        bitbufferptr = &TheSbvector(bitbuffer)->data[bytesize];
        #if BIG_ENDIAN_P
        {var reg1 uintB* bigptr = (uintB*)(&TheBignum(big)->data[digitcount]);
         dotimespL(count,count, { *--bigptr = *--bitbufferptr; } );
        }
        #else
        {var reg1 uintD* bigptr = &TheBignum(big)->data[digitcount];
         var reg6 uintL count2;
         #define GET_NEXT_BYTE(i)  \
           digit |= ((uintD)(*--bitbufferptr) << (8*i));
         dotimespL(count2,floor(count,intDsize/8),
           { var reg3 uintD digit = 0;
             DOCONSTTIMES(intDsize/8,GET_NEXT_BYTE); # GET_NEXT_BYTE(0..intDsize/8-1)
             *--bigptr = digit;
           });
         #undef GET_NEXT_BYTE
         count2 = count % (intDsize/8);
         if (count2>0)
           { var reg7 uintL shiftcount = 0;
             var reg3 uintD digit = (uintD)(*--bitbufferptr);
             dotimesL(count2,count2-1,
               { shiftcount += 8;
                 digit |= ((uintD)(*--bitbufferptr) << shiftcount);
               });
             *--bigptr = digit;
           }
        }
        #endif
        # Wegen (intDsize/8)*(digitcount-1) < count <= (intDsize/8)*digitcount
        # ist alles gefüllt.
        return big;
      }
    }

# Typ rd_by_ix_I: eines dieser beiden Unterprogramme:
  typedef object rd_by_ix_I (object stream, uintL bitsize, uintL bytesize);

# UP für READ-BYTE auf File-Streams für Integers, Art a :
# Füllt den Bitbuffer mit den nächsten bitsize Bits.
# > stream : File-Stream für Integers, Art a
# > finisher : Beendigungsroutine
# < ergebnis : gelesener Integer oder eof_value
  local object rd_by_iax_file (object stream, rd_by_ix_I* finisher);
  local object rd_by_iax_file(stream,finisher)
    var reg1 object stream;
    var reg7 rd_by_ix_I* finisher;
    { var reg6 uintL bitsize = posfixnum_to_L(TheStream(stream)->strm_file_bitsize);
      var reg5 uintL bytesize = bitsize/8;
      # genügend viele Bytes in den Bitbuffer übertragen:
     {var reg2 uintB* bitbufferptr = &TheSbvector(TheStream(stream)->strm_file_bitbuffer)->data[bytesize];
      var reg4 uintL count;
      dotimespL(count,bytesize,
        { var reg3 uintB* ptr = b_file_nextbyte(stream);
          if (ptr == (uintB*)NULL) goto eof;
          # nächstes Byte holen:
          *--bitbufferptr = *ptr;
          # index incrementieren:
          TheStream(stream)->strm_file_index = fixnum_inc(TheStream(stream)->strm_file_index,1);
        });
      # position incrementieren:
      TheStream(stream)->strm_file_position = fixnum_inc(TheStream(stream)->strm_file_position,1);
      # in Zahl umwandeln:
      return (*finisher)(stream,bitsize,bytesize);
      eof: # EOF erreicht
      position_b_file(stream,posfixnum_to_L(TheStream(stream)->strm_file_position)*bytesize);
      return eof_value;
    }}

# UP für READ-BYTE auf File-Streams für Integers, Art b :
# Füllt den Bitbuffer mit den nächsten bitsize Bits.
# > stream : File-Stream für Integers, Art b
# > finisher : Beendigungsroutine
# < ergebnis : gelesener Integer oder eof_value
  local object rd_by_ibx_file (object stream, rd_by_ix_I* finisher);
  local object rd_by_ibx_file(stream,finisher)
    var reg1 object stream;
    var reg8 rd_by_ix_I* finisher;
    { # Nur bei position < eofposition gibt's was zu lesen:
      if (eq(TheStream(stream)->strm_file_position,TheStream(stream)->strm_file_eofposition))
        goto eof;
     {var reg6 uintL bitsize = posfixnum_to_L(TheStream(stream)->strm_file_bitsize); # bitsize (>0, <8)
      # genügend viele Bits in den Bitbuffer übertragen:
      var reg4 uintL bitindex = posfixnum_to_L(TheStream(stream)->strm_file_bitindex);
      var reg5 uintL count = bitindex + bitsize;
      var reg1 uint8 bit_akku;
      var reg3 uintB* ptr = b_file_nextbyte(stream);
      if (ptr == (uintB*)NULL) goto eof;
      # angefangenes Byte holen:
      bit_akku = (*ptr)>>bitindex;
      # bitshift := 8-bitindex
      # Von bit_akku sind die Bits (bitshift-1)..0 gültig.
      if (count > 8)
        { # index incrementieren, da gerade *ptr verarbeitet:
          TheStream(stream)->strm_file_index = fixnum_inc(TheStream(stream)->strm_file_index,1);
          count -= 8; # Noch count (>0) Bits zu holen.
         {var reg3 uintB* ptr = b_file_nextbyte(stream);
          if (ptr == (uintB*)NULL) goto eof1;
          # nächstes Byte holen:
          # (8-bitindex < 8, da sonst count = 0+bitsize < 8 gewesen wäre!)
          bit_akku |= (*ptr)<<(8-bitindex);
        }}# Von bit_akku sind alle 8 Bits gültig.
      # 8 Bit abspeichern:
      TheSbvector(TheStream(stream)->strm_file_bitbuffer)->data[0] = bit_akku;
      TheStream(stream)->strm_file_bitindex = fixnum(count);
      # position incrementieren:
      TheStream(stream)->strm_file_position = fixnum_inc(TheStream(stream)->strm_file_position,1);
      # in Zahl umwandeln:
      return (*finisher)(stream,bitsize,1);
      eof1:
        # Wieder zurückpositionieren:
        position_i_file(stream,posfixnum_to_L(TheStream(stream)->strm_file_position));
      eof: # EOF erreicht gewesen
        return eof_value;
    }}

# UP für READ-BYTE auf File-Streams für Integers, Art c :
# Füllt den Bitbuffer mit den nächsten bitsize Bits.
# > stream : File-Stream für Integers, Art c
# > finisher : Beendigungsroutine
# < ergebnis : gelesener Integer oder eof_value
  local object rd_by_icx_file (object stream, rd_by_ix_I* finisher);
  local object rd_by_icx_file(stream,finisher)
    var reg1 object stream;
    var reg8 rd_by_ix_I* finisher;
    { var reg6 uintL bitsize = posfixnum_to_L(TheStream(stream)->strm_file_bitsize);
      var reg7 uintL bytesize = ceiling(bitsize,8);
      # genügend viele Bits in den Bitbuffer übertragen:
      var reg2 uintB* bitbufferptr = &TheSbvector(TheStream(stream)->strm_file_bitbuffer)->data[bytesize];
      var reg4 uintL count = bitsize;
      var reg5 uintL bitshift = posfixnum_to_L(TheStream(stream)->strm_file_bitindex);
      var reg3 uintB* ptr = b_file_nextbyte(stream);
      if (ptr == (uintB*)NULL) goto eof;
      if (bitshift==0)
        { loop
            { *--bitbufferptr = *ptr; # 8 Bits holen und abspeichern
              # index incrementieren, da gerade *ptr verarbeitet:
              TheStream(stream)->strm_file_index = fixnum_inc(TheStream(stream)->strm_file_index,1);
              count -= 8;
              # Noch count (>0) Bits zu holen.
              ptr = b_file_nextbyte(stream);
              if (ptr == (uintB*)NULL) goto eof;
              if (count<=8) break; # Sind damit count Bits fertig?
            }
          # Noch count = bitsize mod 8 (>0,<8) Bits zu holen.
          *--bitbufferptr = *ptr; # count Bits holen und abspeichern
        }
        else # 0<bitindex<8
        { var reg1 uint16 bit_akku;
          # angefangenes Byte holen:
          bit_akku = (*ptr)>>bitshift;
          bitshift = 8-bitshift; # bitshift := 8-bitindex
          count -= bitshift;
          loop
            { # index incrementieren, da gerade *ptr verarbeitet:
              TheStream(stream)->strm_file_index = fixnum_inc(TheStream(stream)->strm_file_index,1);
              # Von bit_akku sind die Bits (bitshift-1)..0 gültig.
              # Noch count (>0) Bits zu holen.
             {var reg3 uintB* ptr = b_file_nextbyte(stream);
              if (ptr == (uintB*)NULL) goto eof;
              # nächstes Byte holen:
              bit_akku |= (uint16)(*ptr)<<bitshift;
             }# Von bit_akku sind die Bits (7+bitshift)..0 gültig.
              *--bitbufferptr = (uint8)bit_akku; # 8 Bit abspeichern
              if (count<=8) break; # Sind damit count Bits fertig?
              count -= 8;
              bit_akku = bit_akku>>8;
            }
        }
      TheStream(stream)->strm_file_bitindex = fixnum(count);
      # position incrementieren:
      TheStream(stream)->strm_file_position = fixnum_inc(TheStream(stream)->strm_file_position,1);
      # in Zahl umwandeln:
      return (*finisher)(stream,bitsize,bytesize);
      eof: # EOF erreicht
      position_i_file(stream,posfixnum_to_L(TheStream(stream)->strm_file_position));
      return eof_value;
    }

# UP für WRITE-BYTE auf File-Streams für Integers, Art a :
# Schreibt den Bitbuffer-Inhalt aufs File.
  local void wr_by_ia (object stream, uintL bitsize, uintL bytesize);
  local void wr_by_ia(stream,bitsize,bytesize)
    var reg3 object stream;
    var reg5 uintL bitsize;
    var reg4 uintL bytesize;
    { var reg1 uintB* bitbufferptr = &TheSbvector(TheStream(stream)->strm_file_bitbuffer)->data[bytesize];
      var reg2 uintL count;
      dotimespL(count,bytesize, { b_file_writebyte(stream,*--bitbufferptr); } );
      # position incrementieren:
      TheStream(stream)->strm_file_position = fixnum_inc(TheStream(stream)->strm_file_position,1);
    }

# UP für WRITE-BYTE auf File-Streams für Integers, Art b :
# Schreibt den Bitbuffer-Inhalt aufs File.
  local void wr_by_ib (object stream, uintL bitsize, uintL bytesize);
  local void wr_by_ib(stream,bitsize,bytesize)
    var reg4 object stream;
    var reg6 uintL bitsize;
    var reg7 uintL bytesize;
    { var reg1 uintL bitshift = posfixnum_to_L(TheStream(stream)->strm_file_bitindex);
      var reg3 uint16 bit_akku = (uint16)(TheSbvector(TheStream(stream)->strm_file_bitbuffer)->data[0])<<bitshift;
      var reg2 uintL count = bitsize;
      var reg5 uintB* ptr = b_file_nextbyte(stream);
      # angefangenes Byte holen:
      if (!(ptr == (uintB*)NULL)) { bit_akku |= (*ptr)&(bit(bitshift)-1); }
      count += bitshift;
      # evtl. einzelnes Byte schreiben:
      if (count>=8)
        { b_file_writebyte(stream,(uint8)bit_akku);
          bit_akku = bit_akku>>8;
          count -= 8;
        }
      # letztes Byte (count Bits) schreiben:
      if (!(count==0))
        { ptr = b_file_nextbyte(stream);
          if (ptr == (uintB*)NULL) # EOF ?
            { ptr = b_file_eofbyte(stream); # 1 Byte Platz machen
              *ptr = (uint8)bit_akku; # Byte schreiben
            }
            else
            # nächstes Byte nur teilweise überschreiben:
            { var reg3 uint8 diff = (*ptr ^ (uint8)bit_akku) & (uint8)(bit(count)-1);
              if (diff == 0) goto no_modification;
              *ptr ^= diff;
            }
          set_modified_flag(stream);
          no_modification: ;
        }
      TheStream(stream)->strm_file_bitindex = fixnum(count);
      # position und evtl. eofposition incrementieren:
      if (eq(TheStream(stream)->strm_file_eofposition,TheStream(stream)->strm_file_position))
        { TheStream(stream)->strm_file_eofposition = fixnum_inc(TheStream(stream)->strm_file_eofposition,1); }
      TheStream(stream)->strm_file_position = fixnum_inc(TheStream(stream)->strm_file_position,1);
    }

# UP für WRITE-BYTE auf File-Streams für Integers, Art c :
# Schreibt den Bitbuffer-Inhalt aufs File.
  local void wr_by_ic (object stream, uintL bitsize, uintL bytesize);
  local void wr_by_ic(stream,bitsize,bytesize)
    var reg5 object stream;
    var reg7 uintL bitsize;
    var reg8 uintL bytesize;
    { var reg1 uintB* bitbufferptr = &TheSbvector(TheStream(stream)->strm_file_bitbuffer)->data[bytesize];
      var reg2 uintL bitshift = posfixnum_to_L(TheStream(stream)->strm_file_bitindex);
      var reg3 uintL count = bitsize;
      var reg4 uint16 bit_akku;
      var reg6 uintB* ptr = b_file_nextbyte(stream);
      # angefangenes Byte holen:
      bit_akku = (ptr==(uintB*)NULL ? 0 : (*ptr)&(bit(bitshift)-1) );
      count += bitshift;
      # einzelne Bytes schreiben:
      loop
        { bit_akku |= (uint16)(*--bitbufferptr)<<bitshift;
          if (count<8) break;
          b_file_writebyte(stream,(uint8)bit_akku);
          bit_akku = bit_akku>>8;
          count -= 8;
          if (count<=bitshift) break;
        }
      # letztes Byte (count Bits) schreiben:
      if (!(count==0))
        { ptr = b_file_nextbyte(stream);
          if (ptr == (uintB*)NULL) # EOF ?
            { ptr = b_file_eofbyte(stream); # 1 Byte Platz machen
              *ptr = (uint8)bit_akku; # Byte schreiben
            }
            else
            # nächstes Byte nur teilweise überschreiben:
            { var reg3 uint8 diff = (*ptr ^ (uint8)bit_akku) & (uint8)(bit(count)-1);
              if (diff == 0) goto no_modification;
              *ptr ^= diff;
            }
          set_modified_flag(stream);
          no_modification: ;
        }
      TheStream(stream)->strm_file_bitindex = fixnum(count);
      # position incrementieren:
      TheStream(stream)->strm_file_position = fixnum_inc(TheStream(stream)->strm_file_position,1);
    }

# Typ wr_by_ix: eines dieser drei Unterprogramme:
  typedef void wr_by_ix (object stream, uintL bitsize, uintL bytesize);

# UP für WRITE-BYTE auf File-Streams für Integers, Art u :
# Legt das Objekt (ein Integer >=0) als bytesize Bytes im Bitbuffer ab.
# > stream : File-Stream für Integers, Art u
# > obj : auszugebendes Objekt
# > finisher : Beendigungsroutine
  local void wr_by_ixu_file (object stream, object obj, wr_by_ix* finisher);
  local void wr_by_ixu_file(stream,obj,finisher)
    var reg1 object stream;
    var reg5 object obj;
    var reg8 wr_by_ix* finisher;
    { # obj überprüfen:
      if (!integerp(obj)) { fehler_integer(stream,obj); }
      if (!positivep(obj)) { fehler_bad_integer(stream,obj); }
      # obj ist jetzt ein Integer >=0
     {var reg6 uintL bitsize = posfixnum_to_L(TheStream(stream)->strm_file_bitsize);
      var reg6 uintL bytesize = ceiling(bitsize,8);
      # obj in den Bitbuffer übertragen:
      { var reg2 uintB* bitbufferptr = &TheSbvector(TheStream(stream)->strm_file_bitbuffer)->data[bytesize];
        var reg4 uintL count = bytesize;
        if (posfixnump(obj))
          # obj ist ein Fixnum >=0
          { var reg3 uintL wert = posfixnum_to_L(obj);
            # wert < 2^bitsize überprüfen:
            if (!((bitsize>=oint_addr_len) || (wert < bit(bitsize))))
              { fehler_bad_integer(stream,obj); }
            # wert im Bitbuffer ablegen:
            until (wert==0)
              { *--bitbufferptr = (uint8)wert; wert = wert>>8; count--; }
          }
          else
          # obj ist ein Bignum >0
          { var reg5 uintL len = (uintL)(TheBignum(obj)->length);
            # obj < 2^bitsize überprüfen:
            if (!((floor(bitsize,intDsize) >= len)
                  || ((floor(bitsize,intDsize) == len-1)
                      && (TheBignum(obj)->data[0] < bit(bitsize%intDsize))
               ) )   )
              { fehler_bad_integer(stream,obj); }
            #if BIG_ENDIAN_P
            {var reg3 uintB* ptr = (uintB*)&TheBignum(obj)->data[len];
             # Digit-Länge in Byte-Länge umrechnen:
             len = (intDsize/8)*len;
             #define CHECK_NEXT_BYTE(i)  \
               if (!( ((uintB*)(&TheBignum(obj)->data[0]))[i] ==0)) goto len_ok; \
               len--;
             DOCONSTTIMES(intDsize/8,CHECK_NEXT_BYTE); # CHECK_NEXT_BYTE(0..intDsize/8-1)
             #undef CHECK_NEXT_BYTE
             len_ok:
             # obj im Bitbuffer ablegen:
             count = count - len;
             dotimespL(len,len, { *--bitbufferptr = *--ptr; } );
            }
            #else
            {var reg3 uintD* ptr = &TheBignum(obj)->data[len];
             len--;
             count -= (intDsize/8)*len;
             dotimesL(len,len,
               { var reg2 uintD digit = *--ptr;
                 doconsttimes(intDsize/8,
                   { *--bitbufferptr = (uintB)digit; digit = digit >> 8; }
                   );
               });
             {var reg2 uintD digit = *--ptr;
              doconsttimes(intDsize/8,
                { if (digit==0) goto ok;
                  *--bitbufferptr = (uintB)digit; digit = digit >> 8;
                  count--;
                });
              ok: ;
            }}
            #endif
          }
        dotimesL(count,count, { *--bitbufferptr = 0; } );
      }
      (*finisher)(stream,bitsize,bytesize);
    }}

# UP für WRITE-BYTE auf File-Streams für Integers, Art s :
# Legt das Objekt (ein Integer) als bytesize Bytes im Bitbuffer ab.
# > stream : File-Stream für Integers, Art s
# > obj : auszugebendes Objekt
# > finisher : Beendigungsroutine
  local void wr_by_ixs_file (object stream, object obj, wr_by_ix* finisher);
  local void wr_by_ixs_file(stream,obj,finisher)
    var reg1 object stream;
    var reg5 object obj;
    var reg8 wr_by_ix* finisher;
    { # obj überprüfen:
      if (!integerp(obj)) { fehler_integer(stream,obj); }
      # obj ist jetzt ein Integer
     {var reg6 uintL bitsize = posfixnum_to_L(TheStream(stream)->strm_file_bitsize);
      var reg6 uintL bytesize = ceiling(bitsize,8);
      # obj in den Bitbuffer übertragen:
      { var reg2 uintB* bitbufferptr = &TheSbvector(TheStream(stream)->strm_file_bitbuffer)->data[bytesize];
        var reg4 uintL count = bytesize;
        var reg6 uintL sign = (sintL)R_sign(obj);
        if (fixnump(obj))
          # obj ist ein Fixnum
          { var reg3 uintL wert = fixnum_to_L(obj); # >=0 oder <0, je nach sign
            # 0 <= wert < 2^(bitsize-1) bzw. -2^(bitsize-1) <= wert < 0 überprüfen:
            wert = wert^sign;
            if (!((bitsize>oint_addr_len) || (wert < bit(bitsize-1))))
              { fehler_bad_integer(stream,obj); }
            # wert^sign im Bitbuffer ablegen:
            until (wert == 0)
              { *--bitbufferptr = (uint8)(wert^sign); wert = wert>>8; count--; }
            dotimesL(count,count, { *--bitbufferptr = (uint8)sign; } );
          }
          else
          # obj ist ein Bignum
          { var reg5 uintL len = (uintL)(TheBignum(obj)->length);
            # -2^(bitsize-1) <= obj < 2^(bitsize-1) überprüfen:
            if (!((floor(bitsize,intDsize) >= len)
                  || ((bitsize > intDsize*(len-1))
                      && ((TheBignum(obj)->data[0] ^ (uintD)sign) < bit((bitsize%intDsize)-1))
               ) )   )
              { fehler_bad_integer(stream,obj); }
            #if BIG_ENDIAN_P
            {var reg3 uintB* ptr = (uintB*)&TheBignum(obj)->data[len];
             # Digit-Länge in Byte-Länge umrechnen:
             len = (intDsize/8)*len;
             #define CHECK_NEXT_BYTE(i)  \
               if (!( ((uintB*)(&TheBignum(obj)->data[0]))[i] == (uintB)sign)) goto len_ok; \
               len--;
             DOCONSTTIMES(intDsize/8,CHECK_NEXT_BYTE); # CHECK_NEXT_BYTE(0..intDsize/8-1)
             #undef CHECK_NEXT_BYTE
             len_ok:
             # obj im Bitbuffer ablegen:
             count = count - len;
             dotimespL(len,len, { *--bitbufferptr = *--ptr; } );
            }
            #else
            {var reg3 uintD* ptr = &TheBignum(obj)->data[len];
             len--;
             count -= (intDsize/8)*len;
             dotimesL(len,len,
               { var reg2 uintD digit = *--ptr;
                 doconsttimes(intDsize/8,
                   { *--bitbufferptr = (uintB)digit; digit = digit >> 8; }
                   );
               });
             {var reg2 sintD digit = *--ptr;
              doconsttimes(intDsize/8,
                { if (digit == (uintD)sign) goto ok;
                  *--bitbufferptr = (uintB)digit; digit = digit >> 8;
                  count--;
                });
              ok: ;
            }}
            #endif
            dotimesL(count,count, { *--bitbufferptr = (uintB)sign; } );
          }
      }
      (*finisher)(stream,bitsize,bytesize);
    }}

# READ-BYTE - Pseudofunktion für File-Streams für Integers, Art au :
  local object rd_by_iau_file (object stream);
  local object rd_by_iau_file(stream)
    var reg1 object stream;
    { return rd_by_iax_file(stream,&rd_by_iu_I); }

# WRITE-BYTE - Pseudofunktion für File-Streams für Integers, Art au :
  local void wr_by_iau_file (object stream, object obj);
  local void wr_by_iau_file(stream,obj)
    var reg1 object stream;
    var reg2 object obj;
    { wr_by_ixu_file(stream,obj,&wr_by_ia); }

# READ-BYTE - Pseudofunktion für File-Streams für Integers, Art as :
  local object rd_by_ias_file (object stream);
  local object rd_by_ias_file(stream)
    var reg1 object stream;
    { return rd_by_iax_file(stream,&rd_by_is_I); }

# WRITE-BYTE - Pseudofunktion für File-Streams für Integers, Art as :
  local void wr_by_ias_file (object stream, object obj);
  local void wr_by_ias_file(stream,obj)
    var reg1 object stream;
    var reg2 object obj;
    { wr_by_ixs_file(stream,obj,&wr_by_ia); }

# READ-BYTE - Pseudofunktion für File-Streams für Integers, Art bu :
  local object rd_by_ibu_file (object stream);
  local object rd_by_ibu_file(stream)
    var reg1 object stream;
    { return rd_by_ibx_file(stream,&rd_by_iu_I); }

# WRITE-BYTE - Pseudofunktion für File-Streams für Integers, Art bu :
  local void wr_by_ibu_file (object stream, object obj);
  local void wr_by_ibu_file(stream,obj)
    var reg1 object stream;
    var reg2 object obj;
    { wr_by_ixu_file(stream,obj,&wr_by_ib); }

# READ-BYTE - Pseudofunktion für File-Streams für Integers, Art bs :
  local object rd_by_ibs_file (object stream);
  local object rd_by_ibs_file(stream)
    var reg1 object stream;
    { return rd_by_ibx_file(stream,&rd_by_is_I); }

# WRITE-BYTE - Pseudofunktion für File-Streams für Integers, Art bs :
  local void wr_by_ibs_file (object stream, object obj);
  local void wr_by_ibs_file(stream,obj)
    var reg1 object stream;
    var reg2 object obj;
    { wr_by_ixs_file(stream,obj,&wr_by_ib); }

# READ-BYTE - Pseudofunktion für File-Streams für Integers, Art cu :
  local object rd_by_icu_file (object stream);
  local object rd_by_icu_file(stream)
    var reg1 object stream;
    { return rd_by_icx_file(stream,&rd_by_iu_I); }

# WRITE-BYTE - Pseudofunktion für File-Streams für Integers, Art cu :
  local void wr_by_icu_file (object stream, object obj);
  local void wr_by_icu_file(stream,obj)
    var reg1 object stream;
    var reg2 object obj;
    { wr_by_ixu_file(stream,obj,&wr_by_ic); }

# READ-BYTE - Pseudofunktion für File-Streams für Integers, Art cs :
  local object rd_by_ics_file (object stream);
  local object rd_by_ics_file(stream)
    var reg1 object stream;
    { return rd_by_icx_file(stream,&rd_by_is_I); }

# WRITE-BYTE - Pseudofunktion für File-Streams für Integers, Art cs :
  local void wr_by_ics_file (object stream, object obj);
  local void wr_by_ics_file(stream,obj)
    var reg1 object stream;
    var reg2 object obj;
    { wr_by_ixs_file(stream,obj,&wr_by_ic); }

# File-Stream allgemein
# =====================

# UP: Positioniert einen (offenen) File-Stream an den Anfang.
# position_file_start(stream);
# > stream : (offener) File-Stream.
# verändert in stream: index, eofindex, buffstart, ..., position, rd_ch_last
  local void position_file_start (object stream);
  local void position_file_start(stream)
    var reg1 object stream;
    { position_b_file(stream,
                      TheStream(stream)->strmflags & strmflags_ib_B # Integer-Stream vom Typ b ?
                      ? sizeof(uintL) : 0 # ja -> Position 4, sonst Position 0
                     );
      if (TheStream(stream)->strmflags & (strmflags_ib_B | strmflags_ic_B))
        # Integer-Stream der Art b,c
        { TheStream(stream)->strm_file_bitindex = Fixnum_0; } # bitindex := 0
      TheStream(stream)->strm_file_position = Fixnum_0; # position := 0
      TheStream(stream)->strm_rd_ch_last = NIL; # Lastchar := NIL
    }

# UP: Positioniert einen (offenen) File-Stream an eine gegebene Position.
# position_file(stream,position);
# > stream : (offener) File-Stream.
# > position : neue (logische) Position
# verändert in stream: index, eofindex, buffstart, ..., position, rd_ch_last
  local void position_file (object stream, uintL position);
  local void position_file(stream,position)
    var reg1 object stream;
    var reg2 uintL position;
    { var reg3 uintB flags = TheStream(stream)->strmflags;
      if (flags & strmflags_i_B) # Integer-Stream ?
        { if (flags & strmflags_ia_B)
            # Art a
            { var reg4 uintL bitsize = posfixnum_to_L(TheStream(stream)->strm_file_bitsize);
              position_b_file(stream,position*(bitsize/8));
            }
            else
            # Art b,c
            { position_i_file(stream,position); }
        }
        else
        { if (TheStream(stream)->strmtype == strmtype_ch_file) # Character-Stream ?
            { position_b_file(stream,position*char_size); }
          else # String-Char-Stream
            { position_b_file(stream,position); }
          TheStream(stream)->strm_rd_ch_last = NIL; # Lastchar := NIL
        }
      TheStream(stream)->strm_file_position = fixnum(position);
    }

# UP: Positioniert einen (offenen) File-Stream ans Ende.
# position_file_end(stream);
# > stream : (offener) File-Stream.
# verändert in stream: index, eofindex, buffstart, ..., position, rd_ch_last
  local void position_file_end (object stream);
  local void position_file_end(stream)
    var reg1 object stream;
    { # evtl. Buffer hinausschreiben:
      if (modified_flag(stream)) { b_file_flush(stream); }
     {var reg2 uintL eofbytes; # EOF-Position, gemessen in Bytes
      # ans Ende positionieren:
      begin_system_call();
      file_lseek(stream,0,SEEK_END,eofbytes=);
      end_system_call();
      # logische Position berechnen und eofbytes korrigieren:
      { var reg5 uintL position; # logische Position
        var reg6 uintL eofbits = 0; # Bit-Ergänzung zu eofbytes
        var reg3 uintB flags = TheStream(stream)->strmflags;
        if (flags & strmflags_i_B) # Integer-Stream ?
          { var reg4 uintL bitsize = posfixnum_to_L(TheStream(stream)->strm_file_bitsize);
            if (flags & strmflags_ia_B)
              # Art a
              { var reg4 uintL bytesize = bitsize/8;
                position = floor(eofbytes,bytesize);
                eofbytes = position*bytesize;
              }
            elif (flags & strmflags_ib_B)
              # Art b
              { eofbytes -= sizeof(uintL); # Header berücksichtigen
                # Ist die gemerkte EOF-Position plausibel?
                position = posfixnum_to_L(TheStream(stream)->strm_file_eofposition);
                if (!(ceiling(position*bitsize,8)==eofbytes)) # ja -> verwende sie
                  { position = floor(eofbytes*8,bitsize); } # nein -> rechne sie neu aus
                # Rechne eofbytes und eofbits neu aus:
                eofbytes = floor(position*bitsize,8);
                eofbits = (position*bitsize)%8;
                eofbytes += sizeof(uintL); # Header berücksichtigen
              }
            else
              # Art c
              { position = floor(eofbytes*8,bitsize);
                eofbytes = floor(position*bitsize,8);
                eofbits = (position*bitsize)%8;
              }
          }
          else
          { if (TheStream(stream)->strmtype == strmtype_ch_file) # Character-Stream ?
              { position = floor(eofbytes,char_size); eofbytes = position*char_size; }
            else # String-Char-Stream
              { position = eofbytes; }
          }
        # auf den Anfang des letzten Sectors positionieren:
        { var reg4 uintL buffstart;
          begin_system_call();
          file_lseek(stream,floor(eofbytes,strm_file_bufflen)*strm_file_bufflen,SEEK_SET,buffstart=);
          end_system_call();
          TheStream(stream)->strm_file_buffstart = fixnum(buffstart);
        }
        # Sector lesen:
        TheStream(stream)->strm_file_eofindex = NIL; # eofindex := NIL
        TheStream(stream)->strm_file_index = Fixnum_0; # index := 0, unmodifiziert
        { var reg4 uintL eofindex = eofbytes % strm_file_bufflen;
          if (!((eofindex==0) && (eofbits==0))) # EOF am Sectorende -> brauche nichts zu lesen
            { b_file_nextbyte(stream);
              # Jetzt ist index=0. index und eofindex setzen:
              TheStream(stream)->strm_file_index = fixnum(eofindex);
              if (!(eofbits==0)) { eofindex += 1; }
              TheStream(stream)->strm_file_eofindex = fixnum(eofindex);
        }   }
        if (flags & (strmflags_ib_B | strmflags_ic_B))
          # Integer-Stream der Art b,c
          { TheStream(stream)->strm_file_bitindex = fixnum(eofbits); }
        # position setzen:
        TheStream(stream)->strm_file_position = fixnum(position);
        TheStream(stream)->strm_rd_ch_last = NIL; # Lastchar := NIL
    }}}

# UP: erzeugt ein File-Stream
# make_file_stream(handle,direction,type,eltype_size,append_flag)
# > handle: Handle des geöffneten Files
# > STACK_1: Filename, ein Pathname
# > STACK_0: Truename, ein Pathname
# > direction: Modus (0 = :PROBE, 1 = :INPUT, 2 = :OUTPUT, 3 = :IO)
# > type: nähere Typinfo
#         (STRMTYPE_SCH_FILE oder STRMTYPE_CH_FILE oder
#          STRMTYPE_IU_FILE oder STRMTYPE_IS_FILE)
# > eltype_size: (bei Integer-Streams) Größe der Elemente in Bits,
#         ein Fixnum >0 und <intDsize*uintC_max
# > append_flag: TRUE falls der Stream gleich ans Ende positioniert werden
#         soll, FALSE sonst
# < ergebnis: File-Stream (oder evtl. File-Handle-Stream)
# < STACK: aufgeräumt
# kann GC auslösen
  global object make_file_stream (object handle, uintB direction, uintB type, object eltype_size, boolean append_flag);
  global object make_file_stream(handle,direction,type,eltype_size,append_flag)
    var reg9 object handle;
    var reg8 uintB direction;
    var reg4 uintB type;
    var reg9 object eltype_size;
    var reg10 boolean append_flag;
    {
      #if defined(HANDLES)
      # Nur reguläre Files zu gebufferten File-Streams machen.
      # Alles andere gibt File-Handle-Streams, weil vermutlich lseek() nicht geht.
      {
        #ifdef UNIX
        var struct stat statbuf;
        begin_system_call();
        if (!( fstat(TheHandle(handle),&statbuf) ==0)) { OS_error(); }
        end_system_call();
        if (!S_ISREG(statbuf.st_mode))
        #endif
        #ifdef AMIGAOS
        var reg1 LONG not_regular_p;
        begin_system_call();
        not_regular_p = IsInteractive(TheHandle(handle)); # Behandlung nicht interaktiver, nicht regulärer Files??
        end_system_call();
        if (not_regular_p)
        #endif
          { if (((type == strmtype_sch_file)
                 || ((type == strmtype_iu_file) && eq(eltype_size,fixnum(8)))
                )
                && !append_flag
               )
              { return make_handle_stream(handle,direction); }
              else
              { # Truename noch in STACK_0
                pushSTACK(S(open));
                fehler(
                       DEUTSCH ? "~: ~ ist kein reguläres File." :
                       ENGLISH ? "~: ~ is not a regular file." :
                       FRANCAIS ? "~: ~ n'est pas un fichier régulier." :
                       ""
                      );
      }   }   }
      #endif
     { # Flags:
       var reg6 uintB flags =
         (direction==0 ? 0 : # bei Modus :PROBE sind alle Flags =0
           # sonst:
           (direction==1 ? strmflags_rd_B : strmflags_open_B) # Modus :INPUT -> nur Read, sonst Read/Write
           &
           (type>=strmtype_iu_file ? strmflags_by_B : strmflags_ch_B) # auf Integers oder Characters
         );
       # Art von Integer-Streams:
       var reg5 uintB art;
       # Länge:
       var reg7 uintC len = strm_len; # Das hat jeder Stream
       len += 8; # Das haben alle File-Streams
       if (type>=strmtype_iu_file)
         { len += 2; # Das haben die File-Streams für Integers
           {var reg1 uintL bitsize = posfixnum_to_L(eltype_size);
            if ((bitsize%8)==0)
              { art = strmflags_ia_bit_B; } # Art a
              else
              { len += 1; # Arten b,c
                if (bitsize<8)
                  { art = strmflags_ib_bit_B; len += 1; } # Art b
                  else
                  { art = strmflags_ic_bit_B; } # Art c
           }  }
           flags |= bit(art); # Art in die Flags mit aufnehmen
         }
       #if defined(FOREIGN_HANDLE) || !NIL_IS_CONSTANT
       pushSTACK(handle); # Handle retten
       #endif
      {# Stream allozieren:
       var reg1 object stream = allocate_stream(flags,type,len);
       # und füllen:
       # Komponenten aller Streams:
       switch (type)
         { case strmtype_sch_file:
             TheStream(stream)->strm_rd_ch = P(rd_ch_sch_file);
             TheStream(stream)->strm_wr_ch = P(wr_ch_sch_file);
             #ifdef STRM_WR_SS
             TheStream(stream)->strm_wr_ss = P(wr_ss_sch_file);
             #endif
             break;
           case strmtype_ch_file:
             TheStream(stream)->strm_rd_ch = P(rd_ch_ch_file);
             TheStream(stream)->strm_wr_ch = P(wr_ch_ch_file);
             #ifdef STRM_WR_SS
             TheStream(stream)->strm_wr_ss = P(wr_ss_dummy_nogc);
             #endif
             break;
           case strmtype_iu_file:
             TheStream(stream)->strm_rd_by =
               (art==strmflags_ia_bit_B ? P(rd_by_iau_file) :
                art==strmflags_ib_bit_B ? P(rd_by_ibu_file) :
                                          P(rd_by_icu_file)
               );
             TheStream(stream)->strm_wr_by =
               (art==strmflags_ia_bit_B ? P(wr_by_iau_file) :
                art==strmflags_ib_bit_B ? P(wr_by_ibu_file) :
                                          P(wr_by_icu_file)
               );
             break;
           case strmtype_is_file:
             TheStream(stream)->strm_rd_by =
               (art==strmflags_ia_bit_B ? P(rd_by_ias_file) :
                art==strmflags_ib_bit_B ? P(rd_by_ibs_file) :
                                          P(rd_by_ics_file)
               );
             TheStream(stream)->strm_wr_by =
               (art==strmflags_ia_bit_B ? P(wr_by_ias_file) :
                art==strmflags_ib_bit_B ? P(wr_by_ibs_file) :
                                          P(wr_by_ics_file)
               );
             break;
           default: NOTREACHED
         }
       # Default für READ-BYTE-Pseudofunktion:
       if ((flags & strmflags_rd_by_B)==0)
         { TheStream(stream)->strm_rd_by = P(rd_by_dummy); }
       # Default für WRITE-BYTE-Pseudofunktion:
       if ((flags & strmflags_wr_by_B)==0)
         { TheStream(stream)->strm_wr_by = P(wr_by_dummy); }
       # Default für READ-CHAR-Pseudofunktion:
       if ((flags & strmflags_rd_ch_B)==0)
         { TheStream(stream)->strm_rd_ch = P(rd_ch_dummy); }
       TheStream(stream)->strm_rd_ch_last = NIL; # Lastchar := NIL
       # Default für WRITE-CHAR-Pseudofunktion:
       if ((flags & strmflags_wr_ch_B)==0)
         { TheStream(stream)->strm_wr_ch = P(wr_ch_dummy);
           #ifdef STRM_WR_SS
           TheStream(stream)->strm_wr_ss = P(wr_ss_dummy);
           #endif
         }
       TheStream(stream)->strm_wr_ch_lpos = Fixnum_0; # Line Position := 0
       # Komponenten von File-Streams:
       #if defined(FOREIGN_HANDLE) || !NIL_IS_CONSTANT
       handle = popSTACK(); # Handle zurück
       #endif
       TheStream(stream)->strm_file_truename = popSTACK(); # Truename eintragen
       TheStream(stream)->strm_file_name = popSTACK(); # Filename eintragen
       if (!nullp(handle)) # Handle=NIL -> Rest bereits mit NIL initialisiert, fertig
         { TheStream(stream)->strm_file_handle = handle; # Handle eintragen
           TheStream(stream)->strm_file_buffstart = Fixnum_0; # buffstart := 0
           # Buffer allozieren:
           pushSTACK(stream);
          {var reg2 object buffer = allocate_string(strm_file_bufflen); # neuer String
           stream = popSTACK();
           TheStream(stream)->strm_file_buffer = buffer;
          }
           TheStream(stream)->strm_file_eofindex = NIL; # eofindex := NIL
           TheStream(stream)->strm_file_index = Fixnum_0; # index := 0, Buffer unmodifiziert
           TheStream(stream)->strm_file_position = Fixnum_0; # position := 0
           if (type>=strmtype_iu_file)
             # File-Stream für Integers
             { TheStream(stream)->strm_file_bitsize = eltype_size;
               # Bitbuffer allozieren:
               pushSTACK(stream);
              {var reg2 object bitbuffer = allocate_bit_vector(ceiling(posfixnum_to_L(eltype_size),8)*8);
               stream = popSTACK();
               TheStream(stream)->strm_file_bitbuffer = bitbuffer;
              }
               if (!(art==strmflags_ia_bit_B))
                 # Arten b,c
                 { TheStream(stream)->strm_file_bitindex = Fixnum_0; # bitindex := 0
                   if (art==strmflags_ib_bit_B)
                     # Art b
                     { # eofposition lesen:
                       var reg3 uintL eofposition = 0;
                       var reg2 uintC count;
                       for (count=0; count < 8*sizeof(uintL); count += 8 )
                         { var reg1 uintB* ptr = b_file_nextbyte(stream);
                           if (ptr == (uintB*)NULL) goto too_short;
                           eofposition |= ((*ptr) << count);
                           # index incrementieren, da gerade *ptr verarbeitet:
                           TheStream(stream)->strm_file_index = fixnum_inc(TheStream(stream)->strm_file_index,1);
                         }
                       if (FALSE)
                         { too_short:
                           # File zu kurz (< sizeof(uintL) Bytes)
                           if ((TheStream(stream)->strmflags & strmflags_wr_by_B) == 0) # Read-Only-Stream?
                             goto bad_eofposition;
                           # File Read/Write -> setze eofposition := 0
                           eofposition = 0;
                           position_b_file(stream,0); # an Position 0 positionieren
                          {var reg2 uintC count; # und eofposition = 0 herausschreiben
                           dotimespC(count,sizeof(uintL), { b_file_writebyte(stream,0); } );
                         }}
                       elif (eofposition > (uintL)(bitm(oint_addr_len)-1))
                         { bad_eofposition:
                           # Keine gültige EOF-Position.
                           # File schließen und Error melden:
                           TheStream(stream)->strmflags &= ~strmflags_wr_by_B; # Stream Read-Only machen
                           pushSTACK(stream);
                           stream_close(&STACK_0);
                           STACK_0 = TheStream(STACK_0)->strm_file_truename;
                           fehler(
                                  DEUTSCH ? "File ~ hat nicht das Format eines Integer-Files." :
                                  ENGLISH ? "file ~ is not an integer file" :
                                  FRANCAIS ? "Le fichier ~ n'a pas le format d'un fichier d'entiers." :
                                  ""
                                 );
                         }
                       # Auf die gelesene EOF-Position verlassen wir uns jetzt!
                       TheStream(stream)->strm_file_eofposition =
                         fixnum(eofposition);
             }   }   }
           # Liste der offenen File-Streams um stream erweitern:
           pushSTACK(stream);
          {var reg1 object new_cons = allocate_cons();
           Car(new_cons) = stream = popSTACK();
           Cdr(new_cons) = O(open_files);
           O(open_files) = new_cons;
          }# Modus :APPEND behandeln:
           if (append_flag) { position_file_end(stream); }
         }
       return stream;
    }}}

# UP: Bereitet das Schließen eines File-Streams vor.
# Dabei wird der Buffer und evtl. eofposition hinausgeschrieben.
# file_flush(stream);
# > stream : (offener) File-Stream.
# verändert in stream: index, eofindex, buffstart, ...
  local void file_flush (object stream);
  local void file_flush(stream)
    var reg1 object stream;
    { # Bei Integer-Streams (Art b) eofposition abspeichern:
      if (TheStream(stream)->strmflags & strmflags_ib_B)
        if (TheStream(stream)->strmflags & strmflags_wr_by_B) # nur falls nicht Read-Only
          { position_b_file(stream,0); # an Position 0 positionieren
           {var reg2 uintL eofposition = posfixnum_to_L(TheStream(stream)->strm_file_eofposition);
            var reg3 uintC count;
            dotimespC(count,sizeof(uintL),
              { b_file_writebyte(stream,(uintB)eofposition);
                eofposition = eofposition>>8;
              });
          }}
      # evtl. Buffer hinausschreiben:
      if (modified_flag(stream)) { b_file_flush(stream); }
      # Nun ist das modified_flag gelöscht.
    }

# UP: Bringt den wartenden Output eines File-Stream ans Ziel.
# Schreibt dazu den Buffer des File-Streams (auch physikalisch) aufs File.
# finish_output_file(stream);
# > stream : File-Stream.
# verändert in stream: handle, index, eofindex, buffstart, ..., rd_ch_last
# kann GC auslösen
  local void finish_output_file (object stream);
  local void finish_output_file(stream)
    var reg1 object stream;
    { # Handle=NIL (Stream bereits geschlossen) -> fertig:
      if (nullp(TheStream(stream)->strm_file_handle)) { return; }
      # kein File mit Schreibzugriff -> gar nichts zu tun:
      if (!(TheStream(stream)->strmflags & strmflags_wr_B)) { return; }
      # evtl. Buffer und evtl. eofposition hinausschreiben:
      file_flush(stream);
      # Nun ist das modified_flag gelöscht.
     #ifdef ATARI
        # File schließen (GEMDOS schreibt physikalisch):
       {var reg2 sintW ergebnis =
          GEMDOS_close(TheHandle(TheStream(stream)->strm_file_handle));
        if (ergebnis<0) { OS_error(ergebnis); } # Fehler aufgetreten?
       }
        # File neu öffnen:
        pushSTACK(stream); # stream retten
        pushSTACK(TheStream(stream)->strm_file_truename); # Filename
       {# Directory existiert schon:
        var reg3 object namestring = assume_dir_exists(); # Filename als ASCIZ-String
        var reg2 sintW errorcode;
        errorcode = # Datei neu öffnen, Modus 2 (Read/Write)
          GEMDOS_open(TheAsciz(namestring),2);
        if (errorcode < 0) { OS_error(errorcode); } # Error melden
        # Nun enthält errorcode das Handle des geöffneten Files.
        skipSTACK(1);
        stream = popSTACK(); # stream zurück
        # neues Handle eintragen:
        TheStream(stream)->strm_file_handle = allocate_handle(errorcode);
       }
     #endif
     #ifdef MSDOS
      #if 1
        # File-Handle duplizieren und schließen:
        {var reg3 uintW handle = TheHandle(TheStream(stream)->strm_file_handle);
         var reg2 sintW handle2 = dup(handle);
         if (handle2 < 0) { OS_error(); } # Error melden
         if ( close(handle2) <0) { OS_error(); }
        }
      #else
        # File schließen (DOS schreibt physikalisch):
        if ( close(TheHandle(TheStream(stream)->strm_file_handle)) <0) { OS_error(); }
        # File neu öffnen:
        pushSTACK(stream); # stream retten
        pushSTACK(TheStream(stream)->strm_file_truename); # Filename
       {# Directory existiert schon:
        var reg3 object namestring = assume_dir_exists(); # Filename als ASCIZ-String
        var reg2 sintW handle;
        begin_system_call();
        handle = open(TheAsciz(namestring),O_RDWR); # Datei neu öffnen
        if (handle < 0) { OS_error(); } # Error melden
        setmode(handle,O_BINARY);
        end_system_call();
        # Nun enthält handle das Handle des geöffneten Files.
        skipSTACK(1);
        stream = popSTACK(); # stream zurück
        # neues Handle eintragen:
        TheStream(stream)->strm_file_handle = allocate_handle(handle);
       }
      #endif
     #endif
     #ifdef UNIX
      #ifdef HAVE_FSYNC
      begin_system_call();
      if (!( fsync(TheHandle(TheStream(stream)->strm_file_handle)) ==0))
        { OS_error(); }
      end_system_call();
      #endif
     #endif
     #ifdef AMIGAOS
      #if 0 # Manche Devices vertragen es nicht, wenn man geöffnete Dateien
            # zu- und wieder aufmacht.
      begin_system_call();
      {var reg1 Handle handle = TheHandle(TheStream(stream)->strm_file_handle);
       if (!IsInteractive(handle))
         { # File schließen (OS schreibt physikalisch):
           Close(handle);
           # File neu öffnen:
           pushSTACK(stream); # stream retten
           pushSTACK(TheStream(stream)->strm_file_truename); # Filename
          {# Directory existiert schon, Datei neu öffnen:
           var reg2 object namestring = assume_dir_exists(); # Filename als ASCIZ-String
           handle = Open(TheAsciz(namestring),MODE_OLDFILE);
           if (handle==NULL) { OS_error(); } # Error melden
           skipSTACK(1);
           stream = popSTACK(); # stream zurück
           # neues Handle eintragen:
           TheHandle(TheStream(stream)->strm_file_handle) = handle;
      }  }}
      end_system_call();
      #endif
     #endif
      # und neu positionieren:
     {var reg2 uintL position = posfixnum_to_L(TheStream(stream)->strm_file_buffstart)
                                + posfixnum_to_L(TheStream(stream)->strm_file_index);
      TheStream(stream)->strm_file_buffstart = Fixnum_0; # buffstart := 0
      TheStream(stream)->strm_file_index = Fixnum_0; # index := 0
      TheStream(stream)->strm_file_eofindex = NIL; # eofindex := NIL
      position_b_file(stream,position);
     }# Komponenten position, ..., lastchar bleiben unverändert
    }

# UP: Bringt den wartenden Output eines File-Stream ans Ziel.
# Schreibt dazu den Buffer des File-Streams (auch physikalisch) aufs File.
# force_output_file(stream);
# > stream : File-Stream.
# verändert in stream: handle, index, eofindex, buffstart, ..., rd_ch_last
# kann GC auslösen
  #define force_output_file  finish_output_file

# UP: Erklärt einen File-Stream für geschlossen.
# closed_file(stream);
# > stream : (offener) File-Stream.
# verändert in stream: alle Komponenten außer name und truename
  local void closed_file (object stream);
  local void closed_file(stream)
    var reg1 object stream;
    { TheStream(stream)->strm_file_handle = NIL; # Handle wird ungültig
      TheStream(stream)->strm_file_buffer = NIL; # Buffer freimachen
      TheStream(stream)->strm_file_buffstart = NIL; # buffstart löschen (unnötig)
      TheStream(stream)->strm_file_eofindex = NIL; # eofindex löschen (unnötig)
      TheStream(stream)->strm_file_index = NIL; # index löschen (unnötig)
      TheStream(stream)->strm_file_position = NIL; # position löschen (unnötig)
      if (TheStream(stream)->strmflags & strmflags_i_B)
        { TheStream(stream)->strm_file_bitsize = NIL; # bitsize löschen (unnötig)
          TheStream(stream)->strm_file_bitbuffer = NIL; # Bitbuffer freimachen
        }
    }

# UP: Schließt einen File-Stream.
# close_file(stream);
# > stream : File-Stream.
# verändert in stream: alle Komponenten außer name und truename
  local void close_file (object stream);
  local void close_file(stream)
    var reg1 object stream;
    { # Handle=NIL (Stream bereits geschlossen) -> fertig:
      if (nullp(TheStream(stream)->strm_file_handle)) { return; }
      # evtl. Buffer und evtl. eofposition hinausschreiben:
      file_flush(stream);
      # Nun ist das modified_flag gelöscht.
      # File schließen:
      #ifdef ATARI
      { var reg2 sintW ergebnis =
          GEMDOS_close(TheHandle(TheStream(stream)->strm_file_handle));
        if (ergebnis<0) # Fehler aufgetreten?
          # Fehler beim Schließen abfragen, dabei die "weniger schlimmen"
          # GEMDOS_close_DiskChange (Diskettenwechsel) und
          # GEMDOS_close_BadHandle (ungültige Handle-Nummer)
          # vorübergehend ignorieren und verzögert ausgeben:
          { if ((ergebnis==GEMDOS_close_DiskChange)||(ergebnis==GEMDOS_close_BadHandle)) # harmloser Fehler?
              { closed_file(stream); close_dummys(stream); }
            OS_error(ergebnis);
      }   }
      #endif
      #if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX)
      begin_system_call();
      if (!( close(TheHandle(TheStream(stream)->strm_file_handle)) ==0))
        { OS_error(); }
      end_system_call();
      #endif
      #ifdef AMIGAOS
      begin_system_call();
      Close(TheHandle(TheStream(stream)->strm_file_handle));
      end_system_call();
      #endif
      # Komponenten ungültig machen (close_dummys kommt später):
      closed_file(stream);
      # stream aus der Liste aller offenen File-Streams streichen:
      O(open_files) = deleteq(O(open_files),stream);
    }


# Synonym-Stream
# ==============

# Zusätzliche Komponenten:
  # define strm_synonym_symbol  strm_other[0]  # Symbol, auf dessen Wert verwiesen wird

# Macro: Liefert den Wert eines Symbols, ein Stream.
# get_synonym_stream(sym)
# > sym: Symbol
# < ergebnis: sein Wert, ein Stream
  #define get_synonym_stream(sym)  \
    ((!mstreamp(Symbol_value(sym))) ?         \
       (fehler_value_stream(sym), unbound) :  \
       Symbol_value(sym)                      \
    )

# READ-BYTE - Pseudofunktion für Synonym-Streams:
  local object rd_by_synonym (object stream);
  local object rd_by_synonym(stream)
    var reg2 object stream;
    { check_SP();
     {var reg1 object symbol = TheStream(stream)->strm_synonym_symbol;
      return read_byte(get_synonym_stream(symbol));
    }}

# WRITE-BYTE - Pseudofunktion für Synonym-Streams:
  local void wr_by_synonym (object stream, object obj);
  local void wr_by_synonym(stream,obj)
    var reg2 object stream;
    var reg3 object obj;
    { check_SP();
     {var reg1 object symbol = TheStream(stream)->strm_synonym_symbol;
      write_byte(get_synonym_stream(symbol),obj);
    }}

# READ-CHAR - Pseudofunktion für Synonym-Streams:
  local object rd_ch_synonym (object* stream_);
  local object rd_ch_synonym(stream_)
    var reg3 object* stream_;
    {  check_SP(); check_STACK();
     { var reg2 object stream = *stream_;
       var reg1 object symbol = TheStream(stream)->strm_synonym_symbol;
       pushSTACK(get_synonym_stream(symbol));
      {var reg1 object ergebnis = read_char(&STACK_0);
       skipSTACK(1);
       return ergebnis;
    }}}

# WRITE-CHAR - Pseudofunktion für Synonym-Streams:
  local void wr_ch_synonym (object* stream_, object obj);
  local void wr_ch_synonym(stream_,obj)
    var reg3 object* stream_;
    var reg4 object obj;
    { check_SP(); check_STACK();
     {var reg2 object stream = *stream_;
      var reg1 object symbol = TheStream(stream)->strm_synonym_symbol;
      pushSTACK(get_synonym_stream(symbol));
      write_char(&STACK_0,obj);
      skipSTACK(1);
    }}

#ifdef STRM_WR_SS
# WRITE-SIMPLE-STRING - Pseudofunktion für Synonym-Streams:
  local void wr_ss_synonym (object* stream_, object string, uintL start, uintL len);
  local void wr_ss_synonym(stream_,string,start,len)
    var reg1 object* stream_;
    var reg3 object string;
    var reg4 uintL start;
    var reg5 uintL len;
    { check_SP(); check_STACK();
     {var reg2 object symbol = TheStream(*stream_)->strm_synonym_symbol;
      pushSTACK(get_synonym_stream(symbol));
      wr_ss(STACK_0)(&STACK_0,string,start,len);
      skipSTACK(1);
      # Line-Position aktualisieren kann hier entfallen.
    }}
#endif

# Schließt einen Synonym-Stream.
# close_synonym(stream);
# > stream : Synonym-Stream
  local void close_synonym (object stream);
  local void close_synonym(stream)
    var reg2 object stream;
    { check_SP(); check_STACK();
     {var reg1 object symbol = TheStream(stream)->strm_synonym_symbol;
      pushSTACK(get_synonym_stream(symbol));
      stream_close(&STACK_0);
      skipSTACK(1);
    }}

# Stellt fest, ob ein Synonym-Stream ein Zeichen verfügbar hat.
# listen_synonym(stream)
# > stream : Synonym-Stream
# < ergebnis:  0 falls Zeichen verfügbar,
#             -1 falls bei EOF angelangt,
#             +1 falls kein Zeichen verfügbar, aber nicht wegen EOF
# kann GC auslösen
  local signean listen_synonym (object stream);
  local signean listen_synonym(stream)
    var reg2 object stream;
    { check_SP();
     {var reg1 object symbol = TheStream(stream)->strm_synonym_symbol;
      return stream_listen(get_synonym_stream(symbol));
    }}

# UP: Löscht bereits eingegebenen interaktiven Input von einem Synonym-Stream.
# clear_input_synonym(stream)
# > stream: Synonym-Stream
# < ergebnis: TRUE falls Input gelöscht wurde
# kann GC auslösen
  local boolean clear_input_synonym (object stream);
  local boolean clear_input_synonym(stream)
    var reg2 object stream;
    { check_SP();
     {var reg1 object symbol = TheStream(stream)->strm_synonym_symbol;
      return clear_input(get_synonym_stream(symbol));
    }}

# UP: Wartenden Output eines Synonym-Stream ans Ziel bringen.
# finish_output_synonym(stream);
# > stream: Synonym-Stream
# kann GC auslösen
  local void finish_output_synonym (object stream);
  local void finish_output_synonym(stream)
    var reg2 object stream;
    { check_SP();
     {var reg1 object symbol = TheStream(stream)->strm_synonym_symbol;
      finish_output(get_synonym_stream(symbol));
    }}

# UP: Wartenden Output eines Synonym-Stream ans Ziel bringen.
# force_output_synonym(stream);
# > stream: Synonym-Stream
# kann GC auslösen
  local void force_output_synonym (object stream);
  local void force_output_synonym(stream)
    var reg2 object stream;
    { check_SP();
     {var reg1 object symbol = TheStream(stream)->strm_synonym_symbol;
      force_output(get_synonym_stream(symbol));
    }}

# UP: Löscht den wartenden Output eines Synonym-Stream.
# clear_output_synonym(stream);
# > stream: Synonym-Stream
# kann GC auslösen
  local void clear_output_synonym (object stream);
  local void clear_output_synonym(stream)
    var reg2 object stream;
    { check_SP();
     {var reg1 object symbol = TheStream(stream)->strm_synonym_symbol;
      clear_output(get_synonym_stream(symbol));
    }}

# Liefert einen Synonym-Stream zu einem Symbol.
# make_synonym_stream(symbol)
# > symbol : Symbol
# < ergebnis : neuer Synonym-Stream
# kann GC auslösen
  local object make_synonym_stream (object symbol);
  local object make_synonym_stream(symbol)
    var reg2 object symbol;
    { pushSTACK(symbol); # Symbol retten
     {var reg1 object stream = # neuer Stream, alle Operationen erlaubt
        allocate_stream(strmflags_open_B,strmtype_synonym,strm_len+1);
      TheStream(stream)->strm_rd_by = P(rd_by_synonym);
      TheStream(stream)->strm_wr_by = P(wr_by_synonym);
      TheStream(stream)->strm_rd_ch = P(rd_ch_synonym);
      TheStream(stream)->strm_rd_ch_last = NIL;
      TheStream(stream)->strm_wr_ch = P(wr_ch_synonym);
      TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
      #ifdef STRM_WR_SS
      TheStream(stream)->strm_wr_ss = P(wr_ss_synonym);
      #endif
      TheStream(stream)->strm_synonym_symbol = popSTACK();
      return stream;
    }}

LISPFUNN(make_synonym_stream,1)
# (MAKE-SYNONYM-STREAM symbol), CLTL S. 329
  { var reg1 object arg = popSTACK();
    if (!symbolp(arg))
      { pushSTACK(arg); pushSTACK(TheSubr(subr_self)->name);
        fehler(
               DEUTSCH ? "~: Argument muß ein Symbol sein, nicht ~" :
               ENGLISH ? "~: argument should be a symbol, not ~" :
               FRANCAIS ? "~ : L'argument doit être un symbole et non ~":
               ""
              );
      }
    value1 = make_synonym_stream(arg); mv_count=1;
  }


# Broadcast-Stream
# ================

# Zusätzliche Komponenten:
  # define strm_broad_list  strm_other[0] # Liste von Streams

# WRITE-BYTE - Pseudofunktion für Broadcast-Streams:
  local void wr_by_broad (object stream, object obj);
  local void wr_by_broad(stream,obj)
    var reg2 object stream;
    var reg3 object obj;
    { check_SP(); check_STACK();
      pushSTACK(obj);
      { var reg1 object streamlist = TheStream(stream)->strm_broad_list; # Liste von Streams
        # obj auf jeden Stream aus der Liste ausgeben:
        while (consp(streamlist))
          { pushSTACK(Cdr(streamlist)); # restliche Streams
            write_byte(Car(streamlist),STACK_1); # obj ausgeben
            streamlist = popSTACK();
      }   }
      skipSTACK(1);
    }

# WRITE-CHAR - Pseudofunktion für Broadcast-Streams:
  local void wr_ch_broad (object* stream_, object obj);
  local void wr_ch_broad(stream_,obj)
    var reg3 object* stream_;
    var reg4 object obj;
    { check_SP(); check_STACK();
      pushSTACK(obj);
      pushSTACK(NIL); # dummy
      pushSTACK(TheStream(*stream_)->strm_broad_list); # Liste von Streams
      # obj auf jeden Stream aus der Liste ausgeben:
      while (mconsp(STACK_0))
        { # Stackaufbau: obj, dummy, streamlistr.
          STACK_1 = Car(STACK_0); # ein Stream aus der Liste
          write_char(&STACK_1,STACK_2); # obj ausgeben
          STACK_0 = Cdr(STACK_0);
        }
      skipSTACK(3);
    }

#ifdef STRM_WR_SS
# WRITE-CHAR - Pseudofunktion für Broadcast-Streams:
  local void wr_ss_broad (object* stream_, object string, uintL start, uintL len);
  local void wr_ss_broad(stream_,string,start,len)
    var reg1 object* stream_;
    var reg4 object string;
    var reg2 uintL start;
    var reg3 uintL len;
    { check_SP(); check_STACK();
      pushSTACK(string);
      pushSTACK(NIL); # dummy
      pushSTACK(TheStream(*stream_)->strm_broad_list); # Liste von Streams
      # string auf jeden Stream aus der Liste ausgeben:
      while (mconsp(STACK_0))
        { # Stackaufbau: string, dummy, streamlistr.
          STACK_1 = Car(STACK_0); # ein Stream aus der Liste
          wr_ss(STACK_1)(&STACK_1,STACK_2,start,len); # string-Stück ausgeben
          STACK_0 = Cdr(STACK_0);
        }
      skipSTACK(3);
      # Line-Position aktualisieren kann hier entfallen.
    }
#endif

# UP: Bringt den wartenden Output eines Broadcast-Stream ans Ziel.
# finish_output_broad(stream);
# > stream: Broadcast-Stream
# kann GC auslösen
  local void finish_output_broad (object stream);
  local void finish_output_broad(stream)
    var reg2 object stream;
    { check_SP(); check_STACK();
      { var reg1 object streamlist = TheStream(stream)->strm_broad_list; # Liste von Streams
        # Jeden Stream aus der Liste einzeln behandeln:
        while (consp(streamlist))
          { pushSTACK(Cdr(streamlist)); # restliche Streams
            finish_output(Car(streamlist));
            streamlist = popSTACK();
      }   }
    }

# UP: Bringt den wartenden Output eines Broadcast-Stream ans Ziel.
# force_output_broad(stream);
# > stream: Broadcast-Stream
# kann GC auslösen
  local void force_output_broad (object stream);
  local void force_output_broad(stream)
    var reg2 object stream;
    { check_SP(); check_STACK();
      { var reg1 object streamlist = TheStream(stream)->strm_broad_list; # Liste von Streams
        # Jeden Stream aus der Liste einzeln behandeln:
        while (consp(streamlist))
          { pushSTACK(Cdr(streamlist)); # restliche Streams
            force_output(Car(streamlist));
            streamlist = popSTACK();
      }   }
    }

# UP: Löscht den wartenden Output eines Broadcast-Stream.
# clear_output_broad(stream);
# > stream: Broadcast-Stream
# kann GC auslösen
  local void clear_output_broad (object stream);
  local void clear_output_broad(stream)
    var reg2 object stream;
    { check_SP(); check_STACK();
      { var reg1 object streamlist = TheStream(stream)->strm_broad_list; # Liste von Streams
        # Jeden Stream aus der Liste einzeln behandeln:
        while (consp(streamlist))
          { pushSTACK(Cdr(streamlist)); # restliche Streams
            clear_output(Car(streamlist));
            streamlist = popSTACK();
      }   }
    }

# Liefert einen Broadcast-Stream zu einer Streamliste.
# make_broadcast_stream(list)
# > list : Liste von Streams
# < ergebnis : Broadcast-Stream
# Die Liste list wird dabei zerstört.
# kann GC auslösen
  local object make_broadcast_stream (object list);
  local object make_broadcast_stream(list)
    var reg2 object list;
    { pushSTACK(list); # list retten
     {var reg1 object stream = # neuer Stream, nur WRITEs erlaubt
        allocate_stream(strmflags_wr_B,strmtype_broad,strm_len+1);
      TheStream(stream)->strm_rd_by = P(rd_by_dummy);
      TheStream(stream)->strm_wr_by = P(wr_by_broad);
      TheStream(stream)->strm_rd_ch = P(rd_ch_dummy);
      TheStream(stream)->strm_rd_ch_last = NIL;
      TheStream(stream)->strm_wr_ch = P(wr_ch_broad);
      TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
      #ifdef STRM_WR_SS
      TheStream(stream)->strm_wr_ss = P(wr_ss_broad);
      #endif
      TheStream(stream)->strm_broad_list = popSTACK();
      return stream;
    }}

# Liefert einen Broadcast-Stream zum Stream stream.
# make_broadcast1_stream(stream)
# > stream : Stream
# < ergebnis : Broadcast-Stream
# kann GC auslösen
  global object make_broadcast1_stream (object stream);
  global object make_broadcast1_stream(oldstream)
    var reg3 object oldstream;
    {  pushSTACK(oldstream);
       # oldstream in eine einelementige Liste packen:
     { var reg2 object new_cons = allocate_cons();
       Car(new_cons) = STACK_0;
      {var reg1 object stream = make_broadcast_stream(new_cons); # neuer Stream
       oldstream = popSTACK();
       # Line-Position übernehmen:
       TheStream(stream)->strm_wr_ch_lpos = TheStream(oldstream)->strm_wr_ch_lpos;
       return stream;
    }}}

LISPFUN(make_broadcast_stream,0,0,rest,nokey,0,NIL)
# (MAKE-BROADCAST-STREAM {stream}), CLTL S. 329
  { # Überprüfen, ob alle Argumente Streams sind:
    test_stream_args(rest_args_pointer,argcount);
    # zu einer Liste zusammenfassen:
   {var reg1 object list = listof(argcount);
    # Stream bauen:
    value1 = make_broadcast_stream(list); mv_count=1;
  }}


# Concatenated-Stream
# ===================

# Zusätzliche Komponenten:
  # define strm_concat_list   strm_other[0]  # Liste von Streams
  #define strm_concat_list2  strm_other[1]  # Liste der verbrauchten Streams

# READ-BYTE - Pseudofunktion für Concatenated-Streams:
  local object rd_by_concat (object stream);
  local object rd_by_concat(stream)
    var reg3 object stream;
    { check_SP(); check_STACK();
      pushSTACK(stream);
     {var reg1 object streamlist = TheStream(stream)->strm_concat_list; # Liste von Streams
      var reg2 object ergebnis;
      while (consp(streamlist))
        { ergebnis = read_byte(Car(streamlist)); # Integer lesen
          if (!eq(ergebnis,eof_value)) { goto OK; } # nicht EOF ?
          # EOF erreicht -> verbrauchten Stream aus der Liste nehmen
          # und in die zweite Liste stecken:
          stream = STACK_0;
         {var reg4 object first_cons = TheStream(stream)->strm_concat_list;
          streamlist = Cdr(first_cons);
          Cdr(first_cons) = TheStream(stream)->strm_concat_list2;
          TheStream(stream)->strm_concat_list2 = first_cons;
          TheStream(stream)->strm_concat_list = streamlist;
        }}
      # alle Streams verbraucht -> liefere EOF:
      ergebnis = eof_value;
      OK: # ergebnis fertig
      skipSTACK(1);
      return ergebnis;
    }}

# READ-CHAR - Pseudofunktion für Concatenated-Streams:
  local object rd_ch_concat (object* stream_);
  local object rd_ch_concat(stream_)
    var reg3 object* stream_;
    { check_SP(); check_STACK();
     {var reg1 object streamlist = TheStream(*stream_)->strm_concat_list; # Liste von Streams
      while (consp(streamlist))
        { pushSTACK(Car(streamlist));
         {var reg2 object ergebnis = read_char(&STACK_0); # Character lesen
          skipSTACK(1);
          if (!eq(ergebnis,eof_value)) { return ergebnis; }
          # EOF erreicht -> verbrauchten Stream aus der Liste nehmen
          # und in die zweite Liste stecken:
          {var reg2 object stream = *stream_;
           var reg4 object first_cons = TheStream(stream)->strm_concat_list;
           streamlist = Cdr(first_cons);
           Cdr(first_cons) = TheStream(stream)->strm_concat_list2;
           TheStream(stream)->strm_concat_list2 = first_cons;
           TheStream(stream)->strm_concat_list = streamlist;
        }}}
      # alle Streams verbraucht -> liefere EOF:
      return eof_value;
    }}

# Stellt fest, ob ein Concatenated-Stream ein Zeichen verfügbar hat.
# listen_concat(stream)
# > stream : Concatenated-Stream
# < ergebnis:  0 falls Zeichen verfügbar,
#             -1 falls bei EOF angelangt,
#             +1 falls kein Zeichen verfügbar, aber nicht wegen EOF
# kann GC auslösen
  local signean listen_concat (object stream);
  local signean listen_concat(stream)
    var reg3 object stream;
    { pushSTACK(stream);
     {var reg1 object streamlist = TheStream(stream)->strm_concat_list; # Liste von Streams
      var reg2 signean ergebnis;
      while (consp(streamlist))
        { ergebnis = stream_listen(Car(streamlist));
          if (ergebnis>=0) { goto OK; } # nicht EOF ?
          # EOF erreicht -> verbrauchten Stream aus der Liste nehmen
          # und in die zweite Liste stecken:
          stream = STACK_0;
         {var reg4 object first_cons = TheStream(stream)->strm_concat_list;
          streamlist = Cdr(first_cons);
          Cdr(first_cons) = TheStream(stream)->strm_concat_list2;
          TheStream(stream)->strm_concat_list2 = first_cons;
          TheStream(stream)->strm_concat_list = streamlist;
        }}
      # alle Streams verbraucht -> liefere EOF:
      ergebnis = signean_minus;
      OK: # ergebnis fertig
      skipSTACK(1);
      return ergebnis;
    }}

# UP: Löscht bereits eingegebenen interaktiven Input von einem
# Concatenated-Stream.
# clear_input_concat(stream)
# > stream: Concatenated-Stream
# < ergebnis: TRUE falls Input gelöscht wurde
# kann GC auslösen
  local boolean clear_input_concat (object stream);
  local boolean clear_input_concat(stream)
    var reg3 object stream;
    { var reg2 boolean ergebnis = FALSE; # noch kein Input gelöscht
      # alle Streams einzeln behandeln:
      var reg1 object streamlist = TheStream(stream)->strm_concat_list; # Liste von Streams
      while (consp(streamlist))
        { pushSTACK(Cdr(streamlist)); # restliche Streamliste
          ergebnis |= clear_input(Car(streamlist)); # allen Input des Teilstreams löschen
          streamlist = popSTACK();
        }
      return ergebnis;
    }

# Liefert einen Concatenated-Stream zu einer Streamliste.
# make_concatenated_stream(list)
# > list : Liste von Streams
# < ergebnis : Concatenated-Stream
# Die Liste list wird dabei zerstört.
# kann GC auslösen
  local object make_concatenated_stream (object list);
  local object make_concatenated_stream(list)
    var reg2 object list;
    { pushSTACK(list); # list retten
     {var reg1 object stream = # neuer Stream, nur READs erlaubt
        allocate_stream(strmflags_rd_B,strmtype_concat,strm_len+2);
      TheStream(stream)->strm_rd_by = P(rd_by_concat);
      TheStream(stream)->strm_wr_by = P(wr_by_dummy);
      TheStream(stream)->strm_rd_ch = P(rd_ch_concat);
      TheStream(stream)->strm_rd_ch_last = NIL;
      TheStream(stream)->strm_wr_ch = P(wr_ch_dummy);
      TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
      #ifdef STRM_WR_SS
      TheStream(stream)->strm_wr_ss = P(wr_ss_dummy);
      #endif
      TheStream(stream)->strm_concat_list = popSTACK();
      TheStream(stream)->strm_concat_list2 = NIL;
      return stream;
    }}

LISPFUN(make_concatenated_stream,0,0,rest,nokey,0,NIL)
# (MAKE-CONCATENATED-STREAM {stream}), CLTL S. 329
  { # Überprüfen, ob alle Argumente Streams sind:
    test_stream_args(rest_args_pointer,argcount);
    # zu einer Liste zusammenfassen:
   {var reg1 object list = listof(argcount);
    # Stream bauen:
    value1 = make_concatenated_stream(list); mv_count=1;
  }}


# Two-Way-Stream, Echo-Stream
# ===========================

# Zusätzliche Komponenten:
  #define strm_twoway_input   strm_other[0]  # Stream für Input
  #define strm_twoway_output  strm_other[1]  # Stream für Output

# WRITE-BYTE - Pseudofunktion für Two-Way- und Echo-Streams:
  local void wr_by_twoway (object stream, object obj);
  local void wr_by_twoway(stream,obj)
    var reg1 object stream;
    var reg2 object obj;
    { check_SP();
      write_byte(TheStream(stream)->strm_twoway_output,obj);
    }

# WRITE-CHAR - Pseudofunktion für Two-Way- und Echo-Streams:
  local void wr_ch_twoway (object* stream_, object obj);
  local void wr_ch_twoway(stream_,obj)
    var reg1 object* stream_;
    var reg2 object obj;
    { check_SP(); check_STACK();
      pushSTACK(TheStream(*stream_)->strm_twoway_output);
      write_char(&STACK_0,obj);
      skipSTACK(1);
    }

#ifdef STRM_WR_SS
# WRITE-SIMPLE-STRING - Pseudofunktion für Two-Way- und Echo-Streams:
  local void wr_ss_twoway (object* stream_, object string, uintL start, uintL len);
  local void wr_ss_twoway(stream_,string,start,len)
    var reg1 object* stream_;
    var reg2 object string;
    var reg3 uintL start;
    var reg4 uintL len;
    { check_SP(); check_STACK();
      pushSTACK(TheStream(*stream_)->strm_twoway_output);
      wr_ss(STACK_0)(&STACK_0,string,start,len);
      skipSTACK(1);
      # Line-Position aktualisieren kann hier entfallen.
    }
#endif

# Stellt fest, ob ein Two-Way- oder Echo-Stream ein Zeichen verfügbar hat.
# listen_twoway(stream)
# > stream : Two-Way- oder Echo-Stream
# < ergebnis:  0 falls Zeichen verfügbar,
#             -1 falls bei EOF angelangt,
#             +1 falls kein Zeichen verfügbar, aber nicht wegen EOF
# kann GC auslösen
  local signean listen_twoway (object stream);
  local signean listen_twoway(stream)
    var reg1 object stream;
    { check_SP();
      return stream_listen(TheStream(stream)->strm_twoway_input);
    }

# UP: Löscht bereits eingegebenen interaktiven Input von einem Two-Way-
# oder Echo-Stream.
# clear_input_twoway(stream)
# > stream: Two-Way- oder Echo-Stream
# < ergebnis: TRUE falls Input gelöscht wurde
# kann GC auslösen
  local boolean clear_input_twoway (object stream);
  local boolean clear_input_twoway(stream)
    var reg1 object stream;
    { check_SP();
      return clear_input(TheStream(stream)->strm_twoway_input);
    }

# UP: Bringt den wartenden Output eines Two-Way- oder Echo-Stream ans Ziel.
# finish_output_twoway(stream);
# > stream: Two-Way- oder Echo-Stream
# kann GC auslösen
  local void finish_output_twoway (object stream);
  local void finish_output_twoway(stream)
    var reg1 object stream;
    { check_SP();
      finish_output(TheStream(stream)->strm_twoway_output);
    }

# UP: Bringt den wartenden Output eines Two-Way- oder Echo-Stream ans Ziel.
# force_output_twoway(stream);
# > stream: Two-Way- oder Echo-Stream
# kann GC auslösen
  local void force_output_twoway (object stream);
  local void force_output_twoway(stream)
    var reg1 object stream;
    { check_SP();
      force_output(TheStream(stream)->strm_twoway_output);
    }

# UP: Löscht den wartenden Output eines Two-Way- oder Echo-Stream.
# clear_output_twoway(stream);
# > stream: Two-Way- oder Echo-Stream
# kann GC auslösen
  local void clear_output_twoway (object stream);
  local void clear_output_twoway(stream)
    var reg1 object stream;
    { check_SP();
      clear_output(TheStream(stream)->strm_twoway_output);
    }


# Two-Way-Stream
# ==============

# READ-BYTE - Pseudofunktion für Two-Way-Streams:
  local object rd_by_twoway (object stream);
  local object rd_by_twoway(stream)
    var reg1 object stream;
    { check_SP();
      return read_byte(TheStream(stream)->strm_twoway_input);
    }

# READ-CHAR - Pseudofunktion für Two-Way-Streams:
  local object rd_ch_twoway (object* stream_);
  local object rd_ch_twoway(stream_)
    var reg1 object* stream_;
    { check_SP(); check_STACK();
      pushSTACK(TheStream(*stream_)->strm_twoway_input);
     {var reg2 object ergebnis = read_char(&STACK_0);
      skipSTACK(1);
      return ergebnis;
    }}

# Liefert einen Two-Way-Stream zu einem Input-Stream und einem Output-Stream.
# make_twoway_stream(input_stream,output_stream)
# > input_stream : Input-Stream
# > output_stream : Output-Stream
# < ergebnis : Two-Way-Stream
# kann GC auslösen
  global object make_twoway_stream (object input_stream, object output_stream);
  global object make_twoway_stream(input_stream,output_stream)
    var reg2 object input_stream;
    var reg2 object output_stream;
    { pushSTACK(input_stream); pushSTACK(output_stream); # Streams retten
     {var reg1 object stream = # neuer Stream, alle Operationen erlaubt
        allocate_stream(strmflags_open_B,strmtype_twoway,strm_len+2);
      TheStream(stream)->strm_rd_by = P(rd_by_twoway);
      TheStream(stream)->strm_wr_by = P(wr_by_twoway);
      TheStream(stream)->strm_rd_ch = P(rd_ch_twoway);
      TheStream(stream)->strm_rd_ch_last = NIL;
      TheStream(stream)->strm_wr_ch = P(wr_ch_twoway);
      output_stream = popSTACK(); input_stream = popSTACK(); # Streams zurück
      TheStream(stream)->strm_wr_ch_lpos = TheStream(output_stream)->strm_wr_ch_lpos;
      #ifdef STRM_WR_SS
      TheStream(stream)->strm_wr_ss = P(wr_ss_twoway);
      #endif
      TheStream(stream)->strm_twoway_input = input_stream;
      TheStream(stream)->strm_twoway_output = output_stream;
      return stream;
    }}

LISPFUNN(make_two_way_stream,2)
# (MAKE-TWO-WAY-STREAM input-stream output-stream), CLTL S. 329
  { # Überprüfen, ob beides Streams sind:
    test_stream_args(args_end_pointer STACKop 2, 2);
   {var reg2 object output_stream = popSTACK();
    var reg1 object input_stream = popSTACK();
    # Stream bauen:
    value1 = make_twoway_stream(input_stream,output_stream); mv_count=1;
  }}


# Echo-Stream
# ===========

# READ-BYTE - Pseudofunktion für Echo-Streams:
  local object rd_by_echo (object stream);
  local object rd_by_echo(stream)
    var reg1 object stream;
    { check_SP(); check_STACK();
      pushSTACK(stream);
     {var reg1 object obj = read_byte(TheStream(stream)->strm_twoway_input);
      stream = popSTACK();
      if (!eq(obj,eof_value))
        { pushSTACK(obj);
          write_byte(TheStream(stream)->strm_twoway_output,obj);
          obj = popSTACK();
        }
      return obj;
    }}

# READ-CHAR - Pseudofunktion für Echo-Streams:
  local object rd_ch_echo (object* stream_);
  local object rd_ch_echo(stream_)
    var reg1 object* stream_;
    { check_SP(); check_STACK();
      pushSTACK(TheStream(*stream_)->strm_twoway_input);
     {var reg2 object obj = read_char(&STACK_0);
      if (!eq(obj,eof_value))
        { STACK_0 = TheStream(*stream_)->strm_twoway_output;
          pushSTACK(obj);
          write_char(&STACK_1,obj);
          obj = popSTACK();
        }
      skipSTACK(1);
      return obj;
    }}

# Liefert einen Echo-Stream zu einem Input-Stream und einem Output-Stream.
# make_echo_stream(input_stream,output_stream)
# > input_stream : Input-Stream
# > output_stream : Output-Stream
# < ergebnis : Echo-Stream
# kann GC auslösen
  local object make_echo_stream (object input_stream, object output_stream);
  local object make_echo_stream(input_stream,output_stream)
    var reg2 object input_stream;
    var reg2 object output_stream;
    { pushSTACK(input_stream); pushSTACK(output_stream); # Streams retten
     {var reg1 object stream = # neuer Stream, alle Operationen erlaubt
        allocate_stream(strmflags_open_B,strmtype_echo,strm_len+2);
      TheStream(stream)->strm_rd_by = P(rd_by_echo);
      TheStream(stream)->strm_wr_by = P(wr_by_twoway);
      TheStream(stream)->strm_rd_ch = P(rd_ch_echo);
      TheStream(stream)->strm_rd_ch_last = NIL;
      TheStream(stream)->strm_wr_ch = P(wr_ch_twoway);
      output_stream = popSTACK(); input_stream = popSTACK(); # Streams zurück
      TheStream(stream)->strm_wr_ch_lpos = TheStream(output_stream)->strm_wr_ch_lpos;
      #ifdef STRM_WR_SS
      TheStream(stream)->strm_wr_ss = P(wr_ss_twoway);
      #endif
      TheStream(stream)->strm_twoway_input = input_stream;
      TheStream(stream)->strm_twoway_output = output_stream;
      return stream;
    }}

LISPFUNN(make_echo_stream,2)
# (MAKE-ECHO-STREAM input-stream output-stream), CLTL S. 330
  { # Überprüfen, ob beides Streams sind:
    test_stream_args(args_end_pointer STACKop 2, 2);
   {var reg2 object output_stream = popSTACK();
    var reg1 object input_stream = popSTACK();
    # Stream bauen:
    value1 = make_echo_stream(input_stream,output_stream); mv_count=1;
  }}


# String-Input-Stream
# ===================

# Zusätzliche Komponenten:
  #define strm_str_in_string    strm_other[0]  # String für Input
  #define strm_str_in_index     strm_other[1]  # Index in den String (Fixnum >=0)
  #define strm_str_in_endindex  strm_other[2]  # Endindex (Fixnum >= index >=0)

# READ-CHAR - Pseudofunktion für String-Input-Streams:
  local object rd_ch_str_in (object* stream_);
  local object rd_ch_str_in(stream_)
    var reg4 object* stream_;
    { var reg1 object stream = *stream_;
      var reg2 uintL index = posfixnum_to_L(TheStream(stream)->strm_str_in_index); # Index
      var reg4 uintL endindex = posfixnum_to_L(TheStream(stream)->strm_str_in_endindex);
      if (index >= endindex)
        { return eof_value; } # EOF erreicht
        else
        # index < eofindex
        { var uintL len;
          var reg3 uintB* charptr = unpack_string(TheStream(stream)->strm_str_in_string,&len);
          # Ab charptr kommen len Zeichen.
          if (index >= len) # Index zu groß ?
            { pushSTACK(TheStream(stream)->strm_str_in_string);
              pushSTACK(stream);
              fehler(
                     DEUTSCH ? "~ hinterm Stringende angelangt, weil String ~ adjustiert wurde." :
                     ENGLISH ? "~ is beyond the end because the string ~ has been adjusted" :
                     FRANCAIS ? "~ est arrivé après la fin de la chaîne, parce que la chaîne ~ a été ajustée." :
                     ""
                    );
            }
         {var reg1 object ch = code_char(charptr[index]); # Zeichen aus dem String holen
          # Index erhöhen:
          TheStream(stream)->strm_str_in_index = fixnum_inc(TheStream(stream)->strm_str_in_index,1);
          return ch;
        }}
    }

# Schließt einen String-Input-Stream.
# close_str_in(stream);
# > stream : String-Input-Stream
  local void close_str_in (object stream);
  local void close_str_in(stream)
    var reg1 object stream;
    { TheStream(stream)->strm_str_in_string = NIL; } # String := NIL

# Stellt fest, ob ein String-Input-Stream ein Zeichen verfügbar hat.
# listen_str_in(stream)
# > stream : String-Input-Stream
# < ergebnis:  0 falls Zeichen verfügbar,
#             -1 falls bei EOF angelangt,
#             +1 falls kein Zeichen verfügbar, aber nicht wegen EOF
# kann GC auslösen
  local signean listen_str_in (object stream);
  local signean listen_str_in(stream)
    var reg1 object stream;
    { var reg2 uintL index = posfixnum_to_L(TheStream(stream)->strm_str_in_index); # Index
      var reg3 uintL endindex = posfixnum_to_L(TheStream(stream)->strm_str_in_endindex);
      if (index >= endindex)
        { return signean_minus; } # EOF erreicht
        else
        { return signean_null; }
    }

LISPFUN(make_string_input_stream,1,2,norest,nokey,0,NIL)
# (MAKE-STRING-INPUT-STREAM string [start [end]]), CLTL S. 330
  { # String holen und Grenzen überprüfen:
    var object string;
    var uintL start;
    var uintL len;
    test_string_limits(&string,&start,&len);
   {var reg2 object start_arg = fixnum(start); # start-Argument (Fixnum >=0)
    var reg3 object end_arg = fixnum_inc(start_arg,len); # end-Argument (Fixnum >=0)
    pushSTACK(string); # String retten
    { var reg1 object stream = # neuer Stream, nur READ-CHAR erlaubt
        allocate_stream(strmflags_rd_ch_B,strmtype_str_in,strm_len+3);
      TheStream(stream)->strm_rd_by = P(rd_by_dummy);
      TheStream(stream)->strm_wr_by = P(wr_by_dummy);
      TheStream(stream)->strm_rd_ch = P(rd_ch_str_in);
      TheStream(stream)->strm_rd_ch_last = NIL;
      TheStream(stream)->strm_wr_ch = P(wr_ch_dummy);
      TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
      #ifdef STRM_WR_SS
      TheStream(stream)->strm_wr_ss = P(wr_ss_dummy);
      #endif
      TheStream(stream)->strm_str_in_string = popSTACK();
      TheStream(stream)->strm_str_in_index = start_arg; # Index := start-Argument
      TheStream(stream)->strm_str_in_endindex = end_arg; # Endindex := end-Argument
      value1 = stream; mv_count=1; # stream als Wert
  }}}

LISPFUNN(string_input_stream_index,1)
# (SYSTEM::STRING-INPUT-STREAM-INDEX string-input-stream) liefert den Index
  { var reg1 object stream = popSTACK(); # Argument
    # muß ein String-Input-Stream sein:
    if (!(streamp(stream) && (TheStream(stream)->strmtype == strmtype_str_in)))
      { pushSTACK(stream);
        pushSTACK(TheSubr(subr_self)->name);
        fehler(
               DEUTSCH ? "~: ~ ist kein String-Input-Stream." :
               ENGLISH ? "~: ~ is not a string input stream" :
               FRANCAIS ? "~ : ~ n'est pas un «stream» lisant d'une chaîne." :
               ""
              );
      }
   {var reg2 object index = TheStream(stream)->strm_str_in_index;
    # Falls ein Character mit UNREAD-CHAR zurückgeschoben wurde,
    # verwende (1- index), ein Fixnum >=0, als Wert:
    if (mposfixnump(TheStream(stream)->strm_rd_ch_last))
      { index = fixnum_inc(index,-1); }
    value1 = index; mv_count=1;
  }}


# String-Output-Stream
# ====================

# Zusätzliche Komponenten:
  #define strm_str_out_string  strm_other[0]  # Semi-Simple-String für Output

# WRITE-CHAR - Pseudofunktion für String-Output-Streams:
  local void wr_ch_str_out (object* stream_, object ch);
  local void wr_ch_str_out(stream_,ch)
    var reg3 object* stream_;
    var reg1 object ch;
    { var reg2 object stream = *stream_;
      # obj sollte String-Char sein:
      if (!string_char_p(ch)) { fehler_string_char(stream,ch); }
      # Character in den String schieben:
      ssstring_push_extend(TheStream(stream)->strm_str_out_string,char_code(ch));
    }

#ifdef STRM_WR_SS
# WRITE-SIMPLE-STRING - Pseudofunktion für String-Output-Streams:
  local void wr_ss_str_out (object* stream_, object string, uintL start, uintL len);
  local void wr_ss_str_out(stream_,srcstring,start,len)
    var reg8 object* stream_;
    var reg9 object srcstring;
    var reg10 uintL start;
    var reg6 uintL len;
    { if (len==0) return;
     {var reg4 object ssstring = TheStream(*stream_)->strm_str_out_string; # Semi-Simple-String
      var reg5 uintL old_len = TheArray(ssstring)->dims[1]; # jetzige Länge = Fill-Pointer
      if (old_len + len > TheArray(ssstring)->dims[0]) # passen keine len Bytes mehr hinein
        { pushSTACK(srcstring);
          ssstring = ssstring_extend(ssstring,old_len+len); # dann länger machen
          srcstring = popSTACK();
        }
      # Zeichen hineinschieben:
      {var reg1 uintB* srcptr = &TheSstring(srcstring)->data[start];
       var reg3 uintL count;
       {var reg2 uintB* ptr = &TheSstring(TheArray(ssstring)->data)->data[old_len];
        dotimespL(count,len, { *ptr++ = *srcptr++; } );
       }
       # und Fill-Pointer erhöhen:
       TheArray(ssstring)->dims[1] = old_len + len;
       wr_ss_lpos(*stream_,srcptr,len); # Line-Position aktualisieren
    }}}
#endif

# Liefert einen String-Output-Stream.
# make_string_output_stream()
# kann GC auslösen
  global object make_string_output_stream (void);
  global object make_string_output_stream()
    { # kleinen Semi-Simple-String der Länge 50 allozieren:
      pushSTACK(make_ssstring(50));
     {var reg1 object stream = # neuer Stream, nur WRITE-CHAR erlaubt
        allocate_stream(strmflags_wr_ch_B,strmtype_str_out,strm_len+1);
      TheStream(stream)->strm_rd_by = P(rd_by_dummy);
      TheStream(stream)->strm_wr_by = P(wr_by_dummy);
      TheStream(stream)->strm_rd_ch = P(rd_ch_dummy);
      TheStream(stream)->strm_rd_ch_last = NIL;
      TheStream(stream)->strm_wr_ch = P(wr_ch_str_out);
      TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
      #ifdef STRM_WR_SS
      TheStream(stream)->strm_wr_ss = P(wr_ss_str_out);
      #endif
      TheStream(stream)->strm_str_out_string = popSTACK(); # String eintragen
      return stream;
    }}

LISPFUN(make_string_output_stream,0,1,norest,nokey,0,NIL)
# (MAKE-STRING-OUTPUT-STREAM [line-position]), CLTL S. 330
  { # line-position überprüfen:
    if (eq(STACK_0,unbound))
      { STACK_0 = Fixnum_0; } # Defaultwert 0
      else
      # line-position angegeben, sollte ein Fixnum >=0 sein:
      { if (!mposfixnump(STACK_0)) { fehler_bad_lpos(); } }
   {var reg1 object stream = make_string_output_stream(); # String-Output-Stream
    TheStream(stream)->strm_wr_ch_lpos = popSTACK(); # Line Position eintragen
    value1 = stream; mv_count=1; # stream als Wert
  }}

# UP: Liefert das von einem String-Output-Stream Angesammelte.
# get_output_stream_string(&stream)
# > stream: String-Output-Stream
# < stream: geleerter Stream
# < ergebnis: Angesammeltes, ein Simple-String
# kann GC auslösen
  global object get_output_stream_string (object* stream_);
  global object get_output_stream_string(stream_)
    var reg1 object* stream_;
    { var reg2 object string = TheStream(*stream_)->strm_str_out_string; # alter String
      string = coerce_ss(string); # in Simple-String umwandeln (erzwingt ein Kopieren)
      # alten String durch Fill-Pointer:=0 leeren:
      TheArray(TheStream(*stream_)->strm_str_out_string)->dims[1] = 0;
      return string;
    }

LISPFUNN(get_output_stream_string,1)
# (GET-OUTPUT-STREAM-STRING string-output-stream), CLTL S. 330
  { var reg1 object stream = STACK_0; # Argument
    # muß ein String-Output-Stream sein:
    if (!(streamp(stream) && (TheStream(stream)->strmtype == strmtype_str_out)))
      { # stream in STACK_0
        pushSTACK(TheSubr(subr_self)->name);
        fehler(
               DEUTSCH ? "~: ~ ist kein String-Output-Stream." :
               ENGLISH ? "~: ~ is not a string output stream" :
               FRANCAIS ? "~ : ~ n'est pas un «stream» écrivant dans une chaîne." :
               ""
              );
      }
   {value1 = get_output_stream_string(&STACK_0); mv_count=1; # Angesammeltes als Wert
    skipSTACK(1);
  }}


# String-Push-Stream
# ==================

# Zusätzliche Komponenten:
  #define strm_str_push_string  strm_other[0]  # String mit Fill-Pointer für Output

# WRITE-CHAR - Pseudofunktion für String-Push-Streams:
  local void wr_ch_str_push (object* stream_, object ch);
  local void wr_ch_str_push(stream_,ch)
    var reg3 object* stream_;
    var reg1 object ch;
    { var reg2 object stream = *stream_;
      # ch sollte String-Char sein:
      if (!string_char_p(ch)) { fehler_string_char(stream,ch); }
      # Character in den String schieben:
      pushSTACK(ch); pushSTACK(TheStream(stream)->strm_str_push_string);
      funcall(L(vector_push_extend),2); # (VECTOR-PUSH-EXTEND ch string)
    }

# (SYSTEM::MAKE-STRING-PUSH-STREAM string) liefert einen Stream, dessen
# WRITE-CHAR-Operation mit einem VECTOR-PUSH-EXTEND auf den gegebenen String
# äquivalent ist.
LISPFUNN(make_string_push_stream,1)
  { {var reg1 object arg = STACK_0; # Argument
     # muß ein String mit Fill-Pointer sein:
     if (!(stringp(arg) && array_has_fill_pointer_p(arg)))
       { # arg in STACK_0
         pushSTACK(S(with_output_to_string));
         fehler(
                DEUTSCH ? "~: Argument muß ein String mit Fill-Pointer sein, nicht ~" :
                ENGLISH ? "~: argument ~ should be a string with fill pointer" :
                FRANCAIS ? "~ : L'argument ~ doit être une chaîne munie d'un pointeur de remplissage." :
                ""
               );
    }  }
    {var reg1 object stream = # neuer Stream, nur WRITE-CHAR erlaubt
       allocate_stream(strmflags_wr_ch_B,strmtype_str_push,strm_len+1);
     TheStream(stream)->strm_rd_by = P(rd_by_dummy);
     TheStream(stream)->strm_wr_by = P(wr_by_dummy);
     TheStream(stream)->strm_rd_ch = P(rd_ch_dummy);
     TheStream(stream)->strm_rd_ch_last = NIL;
     TheStream(stream)->strm_wr_ch = P(wr_ch_str_push);
     TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
     #ifdef STRM_WR_SS
     TheStream(stream)->strm_wr_ss = P(wr_ss_dummy);
     #endif
     TheStream(stream)->strm_str_push_string = popSTACK(); # String eintragen
     value1 = stream; mv_count=1; # stream als Wert
  } }


# Pretty-Printer-Hilfs-Stream
# ===========================

# Zusätzliche Komponenten:
  # define strm_pphelp_strings  strm_other[0]   # Semi-Simple-Strings für Output
  # define strm_pphelp_modus    strm_other[1]   # Modus (NIL=Einzeiler, T=Mehrzeiler)

# WRITE-CHAR - Pseudofunktion für Pretty-Printer-Hilfs-Streams:
  local void wr_ch_pphelp (object* stream_, object ch);
  local void wr_ch_pphelp(stream_,ch)
    var reg4 object* stream_;
    var reg2 object ch;
    { var reg1 object stream = *stream_;
      # ch sollte String-Char sein:
      if (!string_char_p(ch)) { fehler_string_char(stream,ch); }
     {var reg3 uintB c = char_code(ch); # Character
      # Bei NL: Ab jetzt  Modus := Mehrzeiler
      if (c == NL) { TheStream(stream)->strm_pphelp_modus = T; }
      # Character in den ersten String schieben:
      ssstring_push_extend(Car(TheStream(stream)->strm_pphelp_strings),c);
    }}

#ifdef STRM_WR_SS
# WRITE-SIMPLE-STRING - Pseudofunktion für Pretty-Printer-Hilfs-Streams:
  local void wr_ss_pphelp (object* stream_, object string, uintL start, uintL len);
  local void wr_ss_pphelp(stream_,srcstring,start,len)
    var reg8 object* stream_;
    var reg9 object srcstring;
    var reg10 uintL start;
    var reg6 uintL len;
    { if (len==0) return;
     {var reg4 object ssstring = Car(TheStream(*stream_)->strm_pphelp_strings); # Semi-Simple-String
      var reg5 uintL old_len = TheArray(ssstring)->dims[1]; # jetzige Länge = Fill-Pointer
      if (old_len + len > TheArray(ssstring)->dims[0]) # passen keine len Bytes mehr hinein
        { pushSTACK(srcstring);
          ssstring = ssstring_extend(ssstring,old_len+len); # dann länger machen
          srcstring = popSTACK();
        }
      # Zeichen hineinschieben:
      {var reg1 uintB* srcptr = &TheSstring(srcstring)->data[start];
       var reg3 uintL count;
       {var reg2 uintB* ptr = &TheSstring(TheArray(ssstring)->data)->data[old_len];
        dotimespL(count,len, { *ptr++ = *srcptr++; } );
       }
       # und Fill-Pointer erhöhen:
       TheArray(ssstring)->dims[1] = old_len + len;
       if (wr_ss_lpos(*stream_,srcptr,len)) # Line-Position aktualisieren
         { TheStream(*stream_)->strm_pphelp_modus = T; } # Nach NL: Modus := Mehrzeiler
    }}}
#endif

# UP: Liefert einen Pretty-Printer-Hilfs-Stream.
# make_pphelp_stream()
# kann GC auslösen
  global object make_pphelp_stream (void);
  global object make_pphelp_stream()
    { # kleinen Semi-Simple-String der Länge 50 allozieren:
      pushSTACK(make_ssstring(50));
      # einelementige Stringliste bauen:
     {var reg1 object new_cons = allocate_cons();
      Car(new_cons) = popSTACK();
      pushSTACK(new_cons);
     }
     {var reg1 object stream = # neuer Stream, nur WRITE-CHAR erlaubt
        allocate_stream(strmflags_wr_ch_B,strmtype_pphelp,strm_len+2);
      TheStream(stream)->strm_rd_by = P(rd_by_dummy);
      TheStream(stream)->strm_wr_by = P(wr_by_dummy);
      TheStream(stream)->strm_rd_ch = P(rd_ch_dummy);
      TheStream(stream)->strm_rd_ch_last = NIL;
      TheStream(stream)->strm_wr_ch = P(wr_ch_pphelp);
      TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
      #ifdef STRM_WR_SS
      TheStream(stream)->strm_wr_ss = P(wr_ss_pphelp);
      #endif
      TheStream(stream)->strm_pphelp_strings = popSTACK(); # String-Liste eintragen
      TheStream(stream)->strm_pphelp_modus = NIL; # Modus := Einzeiler
      return stream;
    }}

# Buffered-Input-Stream
# =====================

# Elementtyp: string-char
# Richtungen: nur input
# (make-buffered-input-stream fun mode) liefert einen solchen.
#   Dabei ist fun eine Funktion von 0 Argumenten, die bei Aufruf
#   entweder NIL (steht für EOF) oder bis zu drei Werte string, start, end
#   zurückliefert.
#   Funktionsweise: (read-char ...) liefert nacheinander die Zeichen des
#   aktuellen Strings; ist der zu Ende, wird fun aufgerufen, und ist der
#   Wert davon ein String, so wird der neue aktuelle String gegeben durch
#     (multiple-value-bind (str start end) (funcall fun)
#       (subseq str (or start 0) (or end 'NIL))
#     )
#   Der von fun zurückgegebene String sollte nicht verändert werden.
#   (Ansonsten sollte fun vorher den String mit COPY-SEQ kopieren.)
#   mode bestimmt, wie sich der Stream bezüglich LISTEN verhält.
#   mode = NIL: Stream verhält sich wie ein File-Stream, d.h. bei LISTEN
#               und leerem aktuellen String wird fun aufgerufen.
#   mode = T: Stream verhält sich wie ein interaktiver Stream ohne EOF,
#             d.h. man kann davon ausgehen, das stets noch weitere Zeichen
#             kommen, auch ohne fun aufzurufen.
#   mode eine Funktion: Diese Funktion teilt, wenn aufgerufen, mit, ob
#             noch weitere nichtleere Strings zu erwarten sind.
#   (clear-input ...) beendet die Bearbeitung des aktuellen Strings.

# Zusätzliche Komponenten:
  # define strm_buff_in_fun      strm_other[0]  # Lesefunktion
  #define strm_buff_in_mode      strm_other[1]  # Modus oder Listen-Funktion
  #define strm_buff_in_string    strm_other[2]  # aktueller String für Input
  #define strm_buff_in_index     strm_other[3]  # Index in den String (Fixnum >=0)
  #define strm_buff_in_endindex  strm_other[4]  # Endindex (Fixnum >= index >=0)

# READ-CHAR - Pseudofunktion für Buffered-Input-Streams:
  local object rd_ch_buff_in (object* stream_);
  local object rd_ch_buff_in(stream_)
    var reg5 object* stream_;
    { var reg1 object stream = *stream_;
      var reg2 uintL index = posfixnum_to_L(TheStream(stream)->strm_buff_in_index); # Index
      var reg4 uintL endindex = posfixnum_to_L(TheStream(stream)->strm_buff_in_endindex);
      loop
        { if (index < endindex) break; # noch was im aktuellen String?
          # String-Ende erreicht
          # fun aufrufen:
          funcall(TheStream(stream)->strm_buff_in_fun,0);
          if (!stringp(value1))
            { return eof_value; } # EOF erreicht
          # neuen String holen und Grenzen überprüfen:
          pushSTACK(value1); # String
          pushSTACK(mv_count >= 2 ? value2 : unbound); # start
          pushSTACK(mv_count >= 3 ? value3 : unbound); # end
         {var object string;
          var uintL start;
          var uintL len;
          subr_self = L(read_char);
          test_string_limits(&string,&start,&len);
          stream = *stream_;
          index = start;
          endindex = index+len;
          TheStream(stream)->strm_buff_in_string = string;
          TheStream(stream)->strm_buff_in_index = fixnum(index);
          TheStream(stream)->strm_buff_in_endindex = fixnum(endindex);
        }}
      # index < eofindex
      { var uintL len;
        var reg3 uintB* charptr = unpack_string(TheStream(stream)->strm_buff_in_string,&len);
        # Ab charptr kommen len Zeichen.
        if (index >= len) # Index zu groß ?
          { pushSTACK(TheStream(stream)->strm_buff_in_string);
            pushSTACK(stream);
            fehler(
                   DEUTSCH ? "~ hinterm Stringende angelangt, weil String ~ adjustiert wurde." :
                   ENGLISH ? "~ is beyond the end because the string ~ has been adjusted" :
                   FRANCAIS ? "~ est arrivé après la fin de la chaîne, parce que la chaîne ~ a été ajustée." :
                   ""
                  );
          }
       {var reg1 object ch = code_char(charptr[index]); # Zeichen aus dem String holen
        # Index erhöhen:
        TheStream(stream)->strm_buff_in_index = fixnum_inc(TheStream(stream)->strm_buff_in_index,1);
        return ch;
      }}
    }

# Schließt einen Buffered-Input-Stream.
# close_buff_in(stream);
# > stream : Buffered-Input-Stream
  local void close_buff_in (object stream);
  local void close_buff_in(stream)
    var reg1 object stream;
    { TheStream(stream)->strm_buff_in_fun = NIL; # Funktion := NIL
      TheStream(stream)->strm_buff_in_mode = NIL; # Mode := NIL
      TheStream(stream)->strm_buff_in_string = NIL; # String := NIL
    }

# Stellt fest, ob ein Buffered-Input-Stream ein Zeichen verfügbar hat.
# listen_buff_in(stream)
# > stream : Buffered-Input-Stream
# < ergebnis:  0 falls Zeichen verfügbar,
#             -1 falls bei EOF angelangt,
#             +1 falls kein Zeichen verfügbar, aber nicht wegen EOF
# kann GC auslösen
  local signean listen_buff_in (object stream);
  local signean listen_buff_in(stream)
    var reg1 object stream;
    { var reg3 uintL index = posfixnum_to_L(TheStream(stream)->strm_buff_in_index); # Index
      var reg4 uintL endindex = posfixnum_to_L(TheStream(stream)->strm_buff_in_endindex);
      if (index < endindex) { return signean_null; }
     {var reg2 object mode = TheStream(stream)->strm_buff_in_mode;
      if (eq(mode,S(nil)))
        { pushSTACK(stream);
          mode = peek_char(&STACK_0); # peek_char macht read_char, ruft fun auf
          skipSTACK(1);
          if (eq(mode,eof_value))
            { return signean_minus; } # EOF erreicht
            else
            { return signean_null; }
        }
      elif (eq(mode,S(t)))
        { return signean_null; }
      else
        { funcall(mode,0); # mode aufrufen
          if (nullp(value1)) # keine Strings mehr zu erwarten?
            { return signean_minus; } # ja -> EOF erreicht
            else
            { return signean_null; }
        }
    }}

# UP: Löscht bereits eingegebenen interaktiven Input von einem Buffered-Input-Stream.
# clear_input_buff_in(stream)
# > stream: Buffered-Input-Stream
# < ergebnis: TRUE falls Input gelöscht wurde
# kann GC auslösen
  local boolean clear_input_buff_in (object stream);
  local boolean clear_input_buff_in(stream)
    var reg1 object stream;
    { # Bearbeitung des aktuellen Strings beenden:
      var reg3 object index = TheStream(stream)->strm_buff_in_index; # Index
      var reg2 object endindex = TheStream(stream)->strm_buff_in_endindex;
      TheStream(stream)->strm_buff_in_index = endindex; # index := endindex
      if (eq(index,endindex)) { return FALSE; } else { return TRUE; }
    }

LISPFUNN(make_buffered_input_stream,2)
# (MAKE-BUFFERED-INPUT-STREAM fun mode)
  { var reg1 object stream = # neuer Stream, nur READ-CHAR erlaubt
      allocate_stream(strmflags_rd_ch_B,strmtype_buff_in,strm_len+5);
    TheStream(stream)->strm_rd_by = P(rd_by_dummy);
    TheStream(stream)->strm_wr_by = P(wr_by_dummy);
    TheStream(stream)->strm_rd_ch = P(rd_ch_buff_in);
    TheStream(stream)->strm_rd_ch_last = NIL;
    TheStream(stream)->strm_wr_ch = P(wr_ch_dummy);
    TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
    #ifdef STRM_WR_SS
    TheStream(stream)->strm_wr_ss = P(wr_ss_dummy);
    #endif
    TheStream(stream)->strm_buff_in_mode = popSTACK();
    TheStream(stream)->strm_buff_in_fun = popSTACK();
    TheStream(stream)->strm_buff_in_string = O(leer_string); # String := ""
    TheStream(stream)->strm_buff_in_index = Fixnum_0; # Index := 0
    TheStream(stream)->strm_buff_in_endindex = Fixnum_0; # Endindex := 0
    value1 = stream; mv_count=1; # stream als Wert
  }

LISPFUNN(buffered_input_stream_index,1)
# (SYSTEM::BUFFERED-INPUT-STREAM-INDEX buffered-input-stream) liefert den Index
  { var reg1 object stream = popSTACK(); # Argument
    # muß ein Buffered-Input-Stream sein:
    if (!(streamp(stream) && (TheStream(stream)->strmtype == strmtype_buff_in)))
      { pushSTACK(stream);
        pushSTACK(TheSubr(subr_self)->name);
        fehler(
               DEUTSCH ? "~: ~ ist kein Buffered-Input-Stream." :
               ENGLISH ? "~: ~ is not a buffered input stream" :
               FRANCAIS ? "~ : ~ n'est pas un «stream» d'entrée bufferisé." :
               ""
              );
      }
   {var reg2 object index = TheStream(stream)->strm_buff_in_index;
    # Falls ein Character mit UNREAD-CHAR zurückgeschoben wurde,
    # verwende (1- index), ein Fixnum >=0, als Wert:
    if (mposfixnump(TheStream(stream)->strm_rd_ch_last))
      { index = fixnum_inc(index,-1); }
    value1 = index; mv_count=1;
  }}


# Buffered-Output-Stream
# ======================

# Elementtyp: string-char
# Richtungen: nur output
# (make-buffered-output-stream fun) liefert einen solchen.
#   Dabei ist fun eine Funktion von einem Argument, die, mit einem
#   Simple-String als Argument aufgerufen, dessen Inhalt in Empfang nimmt.
#   Funktionsweise: (write-char ...) sammelt die geschriebenen Zeichen in
#   einem String, bis ein #\Newline oder eine FORCE-/FINISH-OUTPUT-
#   Anforderung kommt, und ruft dann fun mit einem Simple-String, der das
#   bisher Angesammelte enthält, als Argument auf.
#   (clear-output ...) wirft die bisher angesammelten Zeichen weg.

# Zusätzliche Komponenten:
  # define strm_buff_out_fun    strm_other[0]  # Ausgabefunktion
  #define strm_buff_out_string  strm_other[1]  # Semi-Simple-String für Output

# UP: Bringt den wartenden Output eines Buffered-Output-Stream ans Ziel.
# finish_output_buff_out(stream);
# > stream: Buffered-Output-Stream
# kann GC auslösen
  local void finish_output_buff_out (object stream);
  local void finish_output_buff_out(stream)
    var reg1 object stream;
    { pushSTACK(stream);
     {var reg2 object string = TheStream(stream)->strm_buff_out_string; # String
      string = coerce_ss(string); # in Simple-String umwandeln (erzwingt ein Kopieren)
      stream = STACK_0; STACK_0 = string;
      # String durch Fill-Pointer:=0 leeren:
      TheArray(TheStream(stream)->strm_buff_out_string)->dims[1] = 0;
      funcall(TheStream(stream)->strm_buff_out_fun,1); # Funktion aufrufen
    }}

# UP: Bringt den wartenden Output eines Buffered-Output-Stream ans Ziel.
# force_output_buff_out(stream);
# > stream: Buffered-Output-Stream
# kann GC auslösen
  #define force_output_buff_out  finish_output_buff_out

# UP: Löscht den wartenden Output eines Buffered-Output-Stream.
# clear_output_buff_out(stream);
# > stream: Buffered-Output-Stream
# kann GC auslösen
  local void clear_output_buff_out (object stream);
  local void clear_output_buff_out(stream)
    var reg1 object stream;
    { # String durch Fill-Pointer:=0 leeren:
      TheArray(TheStream(stream)->strm_buff_out_string)->dims[1] = 0;
      # Line-Position unverändert lassen??
    }

# WRITE-CHAR - Pseudofunktion für Buffered-Output-Streams:
  local void wr_ch_buff_out (object* stream_, object ch);
  local void wr_ch_buff_out(stream_,ch)
    var reg3 object* stream_;
    var reg1 object ch;
    { var reg2 object stream = *stream_;
      # obj sollte String-Char sein:
      if (!string_char_p(ch)) { fehler_string_char(stream,ch); }
      # Character in den String schieben:
      ssstring_push_extend(TheStream(stream)->strm_buff_out_string,char_code(ch));
      # Nach #\Newline den Buffer durchreichen:
      if (char_code(ch) == NL) { force_output_buff_out(*stream_); }
    }

# Schließt einen Buffered-Output-Stream.
# close_buff_out(stream);
# > stream : Buffered-Output-Stream
# kann GC auslösen
  local void close_buff_out (object stream);
  local void close_buff_out(stream)
    var reg1 object stream;
    { pushSTACK(stream); # stream retten
      finish_output_buff_out(stream);
      stream = popSTACK(); # stream zurück
      TheStream(stream)->strm_buff_out_fun = NIL; # Funktion := NIL
      TheStream(stream)->strm_buff_out_string = NIL; # String := NIL
    }

LISPFUN(make_buffered_output_stream,1,1,norest,nokey,0,NIL)
# (MAKE-BUFFERED-OUTPUT-STREAM fun [line-position])
  { # line-position überprüfen:
    if (eq(STACK_0,unbound))
      { STACK_0 = Fixnum_0; } # Defaultwert 0
      else
      # line-position angegeben, sollte ein Fixnum >=0 sein:
      { if (!mposfixnump(STACK_0)) { fehler_bad_lpos(); } }
    # kleinen Semi-Simple-String der Länge 50 allozieren:
    pushSTACK(make_ssstring(50));
   {var reg1 object stream = # neuer Stream, nur WRITE-CHAR erlaubt
      allocate_stream(strmflags_wr_ch_B,strmtype_buff_out,strm_len+2);
    TheStream(stream)->strm_rd_by = P(rd_by_dummy);
    TheStream(stream)->strm_wr_by = P(wr_by_dummy);
    TheStream(stream)->strm_rd_ch = P(rd_ch_dummy);
    TheStream(stream)->strm_rd_ch_last = NIL;
    TheStream(stream)->strm_wr_ch = P(wr_ch_buff_out);
    #ifdef STRM_WR_SS
    TheStream(stream)->strm_wr_ss = P(wr_ss_dummy);
    #endif
    TheStream(stream)->strm_buff_out_string = popSTACK(); # String eintragen
    TheStream(stream)->strm_wr_ch_lpos = popSTACK(); # Line Position eintragen
    TheStream(stream)->strm_buff_out_fun = popSTACK(); # Funktion eintragen
    value1 = stream; mv_count=1; # stream als Wert
  }}


#ifdef PRINTER_ATARI

# Printer-Stream
# ==============

# Zusätzliche Komponenten:
  #define strm_printer_port  strm_other[0]  # Fixnum >=0:
  # 0 bei paralleler, 1 bei serieller Schnittstelle

# Gibt ein Zeichen auf den Printer aus.
# wr_printer(port,c)
# > port: Printer-Port
# > c: Auszugebendes Zeichen
# kann GC auslösen
  local void printer_out (object port, uintB c);
  local void printer_out(port,c)
    var reg1 object port;
    var reg2 uintB c;
    { # Stellt fest, ob der Drucker bereit ist:
      #define printer_ready()  (eq(port,Fixnum_0) ? GEMDOS_PrtStat() : GEMDOS_AuxStat())
      until (printer_ready()) # Drucker bereit?
        { # Drucker noch nicht bereit -> eine Warteschleife einlegen:
          var reg5 object timeout = Symbol_value(S(printer_timeout)) ; # Wert von *PRINTER-TIMEOUT*
          var reg4 uintL wait_time;
          # sollte ein Fixnum >=0, <= 60*ticks_per_second = 12000 sein:
          if (posfixnump(timeout)
              && ((wait_time = posfixnum_to_L(timeout)) <= 12000)
             )
            { var reg3 uintL wait_start_time = get_real_time();
              loop # Warteschleife
                { if (printer_ready()) goto OK; # Drucker bereit -> Warteschleife beenden
                  if ((get_real_time() - wait_start_time) >= wait_time) break;
                }
              # Habe wait_time gewartet, jetzt wird's Zeit für eine Fehlermeldung:
              # Continuable Error melden:
              pushSTACK(O(druckerstreik_string1)); # "Neuer Versuch."
              pushSTACK(O(druckerstreik_string2)); # "Drucker nicht angeschlossen oder off-line."
              funcall(S(cerror),2); # (CERROR "..." "...")
            }
            else
            { # Illegaler Wert in *PRINTER-TIMEOUT* vorgefunden.
              # Wert zurücksetzen:
              Symbol_value(S(printer_timeout)) = fixnum(1000); # *PRINTER-TIMEOUT* := 1000
              # Warnung ausgeben:
              pushSTACK(O(printer_timeout_warnung_string)); # "Der Wert ..."
              pushSTACK(S(printer_timeout));
              funcall(S(warn),2); # (WARN "..." '*PRINTER-TIMEOUT*)
            }
        }
      OK: # Drucker bereit -> Zeichen endlich ausgeben:
      if (eq(port,Fixnum_0)) { GEMDOS_PrtOut(c); } else { GEMDOS_AuxOut(c); }
      #undef printer_ready
    }

# WRITE-CHAR - Pseudofunktion für Printer-Streams:
  local void wr_ch_printer (object* stream_, object ch);
  local void wr_ch_printer(stream_,ch)
    var reg4 object* stream_;
    var reg2 object ch;
    { var reg1 object stream = *stream_;
      # ch sollte String-Char sein:
      if (!string_char_p(ch)) { fehler_string_char(stream,ch); }
     {var reg4 object port = TheStream(stream)->strm_printer_port;
      var reg3 uintB c = char_code(ch); # Character
      if (c==NL)
        # NL in CR/LF umwandeln:
        { printer_out(port,CR); printer_out(port,LF); }
        else
        { printer_out(port,c); }
    }}

# UP: Liefert einen Printer-Stream.
# kann GC auslösen
  local object make_printer_stream (void);
  local object make_printer_stream()
    { var reg1 object stream = # neuer Stream, nur WRITE-CHAR erlaubt
        allocate_stream(strmflags_wr_ch_B,strmtype_printer,strm_len+1);
      TheStream(stream)->strm_rd_by = P(rd_by_dummy);
      TheStream(stream)->strm_wr_by = P(wr_by_dummy);
      TheStream(stream)->strm_rd_ch = P(rd_ch_dummy);
      TheStream(stream)->strm_rd_ch_last = NIL;
      TheStream(stream)->strm_wr_ch = P(wr_ch_printer);
      TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
      #ifdef STRM_WR_SS
      TheStream(stream)->strm_wr_ss = P(wr_ss_dummy);
      #endif
      TheStream(stream)->strm_printer_port = # Printer-Port
        # Druckerkonfiguration abfragen:
        (XBIOS_GetPrtConfig() & bit(4) ? Fixnum_1 # serielle Schnittstelle
                                       : Fixnum_0 # parallele Schnittstelle
        );
      return stream;
    }

#endif

#ifdef PRINTER_AMIGAOS

# Printer-Stream
# ==============

# Zusätzliche Komponenten:
  #define strm_printer_handle  strm_other[0]  # Handle von "PRT:"

# WRITE-CHAR - Pseudofunktion für Printer-Streams:
  local void wr_ch_printer (object* stream_, object ch);
  local void wr_ch_printer(stream_,ch)
    var reg4 object* stream_;
    var reg2 object ch;
    { var reg1 object stream = *stream_;
      # ch sollte String-Char sein:
      if (!string_char_p(ch)) { fehler_string_char(stream,ch); }
      begin_system_call();
     {var uintB c = char_code(ch);
      var reg3 long ergebnis = # Zeichen auszugeben versuchen
        Write(TheHandle(TheStream(stream)->strm_printer_handle),&c,1L);
      end_system_call();
      if (ergebnis<0) { OS_error(); } # Error melden
      # ergebnis = Anzahl der ausgegebenen Zeichen (0 oder 1)
      if (ergebnis==0) # nicht erfolgreich?
        { fehler_unwritable(S(write_char),stream); }
    }}

# Schließt einen Printer-Stream.
  local void close_printer (object stream);
  local void close_printer(stream)
    var reg1 object stream;
    { begin_system_call();
      Close(TheHandle(TheStream(stream)->strm_printer_handle));
      end_system_call();
    }

# UP: Liefert einen Printer-Stream.
# kann GC auslösen
  local object make_printer_stream (void);
  local object make_printer_stream()
    { pushSTACK(allocate_cons()); # Cons für Liste
      pushSTACK(allocate_handle(Handle_NULL)); # Handle-Verpackung
     {var reg1 object stream = # neuer Stream, nur WRITE-CHAR erlaubt
        allocate_stream(strmflags_wr_ch_B,strmtype_printer,strm_len+1);
      set_break_sem_4();
      begin_system_call();
      {var reg2 Handle handle = Open("PRT:",MODE_NEWFILE);
       if (handle==Handle_NULL) { OS_error(); } # Error melden
       end_system_call();
       TheHandle(STACK_0) = handle; # Handle verpacken
      }
      TheStream(stream)->strm_rd_by = P(rd_by_dummy);
      TheStream(stream)->strm_wr_by = P(wr_by_dummy);
      TheStream(stream)->strm_rd_ch = P(rd_ch_dummy);
      TheStream(stream)->strm_rd_ch_last = NIL;
      TheStream(stream)->strm_wr_ch = P(wr_ch_printer);
      TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
      #ifdef STRM_WR_SS
      TheStream(stream)->strm_wr_ss = P(wr_ss_dummy_nogc);
      #endif
      TheStream(stream)->strm_printer_handle = popSTACK();
      # Liste der offenen Streams um stream erweitern:
      {var reg1 object new_cons = popSTACK();
       Car(new_cons) = stream;
       Cdr(new_cons) = O(open_files);
       O(open_files) = new_cons;
      }
      clr_break_sem_4();
      return stream;
    }}

LISPFUNN(make_printer_stream,0)
# (SYSTEM::MAKE-PRINTER-STREAM) liefert einen Printer-Stream.
# Für die verstandenen Escape-Sequenzen siehe in PRINTER.DOC.
  { value1 = make_printer_stream(); mv_count=1; return; }

#endif


#ifdef PIPES

# Pipe-Input-Stream
# =================

# Zusätzliche Komponenten:
  # define strm_pipe_pid       strm_other[3] # Prozeß-Id, ein Fixnum >=0
  #define strm_pipe_in_handle  strm_ihandle  # Handle für Input

# READ-CHAR - Pseudofunktion für Pipe-Input-Streams:
  #define rd_ch_pipe_in  rd_ch_handle

# Schließt einen Pipe-Input-Stream.
# close_pipe_in(stream);
# > stream : Pipe-Input-Stream
  #ifdef EMUNIX
    local void close_pipe_in (object stream);
    local void close_pipe_in(stream)
      var reg1 object stream;
      { var reg2 Handle handle = TheHandle(TheStream(stream)->strm_pipe_in_handle);
        begin_system_call();
        if ( pclose(&_streamv[handle]) == -1) { OS_error(); }
        end_system_call();
      }
  #endif
  #ifdef UNIX
    #define close_pipe_in  close_ihandle
  #endif

# Stellt fest, ob ein Pipe-Input-Stream ein Zeichen verfügbar hat.
# listen_pipe_in(stream)
# > stream : Pipe-Input-Stream
# < ergebnis:  0 falls Zeichen verfügbar,
#             -1 falls bei EOF angelangt,
#             +1 falls kein Zeichen verfügbar, aber nicht wegen EOF
# kann GC auslösen
  #define listen_pipe_in  listen_handle

LISPFUNN(make_pipe_input_stream,1)
# (MAKE-PIPE-INPUT-STREAM command)
# ruft eine Shell auf, die command ausführt, wobei deren Standard-Output
# in unsere Pipe hinein geht.
  { # command überprüfen:
    funcall(L(string),1); # (STRING command)
    pushSTACK(string_to_asciz(value1)); # als ASCIZ-String
   {var int handles[2]; # zwei Handles für die Pipe
    var reg2 int child;
    #ifdef EMUNIX
    { # Stackaufbau: command.
      begin_system_call();
     {var reg1 FILE* f = popen(TheAsciz(STACK_0),"r");
      if (f==NULL) { OS_error(); }
      child = f->pid;
      handles[0] = fileno(f);
      end_system_call();
    }}
    #endif
    #ifdef UNIX
    { # Shell bestimmen:
      var reg1 object shell = string_to_asciz(O(user_shell)); # User-Shell als ASCIZ-String
      # (Im Gegensatz zu EXECUTE gehen wir hier nicht über den LISP-Pathname.)
      # Stackaufbau: command.
      begin_system_call();
      if (!( pipe(handles) ==0)) { OS_error(); } # Pipe aufbauen.
      # Alles, was in handles[1] reingeschoben wird, kommt bei handles[0]
      # wieder raus. Wir werden dies so benutzen:
      #
      #       write            system            read
      # child  ->   handles[1]   ->   handles[0]  ->  parent
      #
      # einen neuen Prozeß starten:
      if ((child = vfork()) ==0)
        # Dieses Programmstück wird vom Child-Prozeß ausgeführt:
        { if ( dup2(handles[1],stdout_handle) >=0) # Standard-Output umleiten
            if ( close(handles[1]) ==0) # Wir wollen nur über stdout_handle schreiben
              if ( close(handles[0]) ==0) # Wir wollen von der Pipe nicht lesen
                # (Muß das dem Betriebssystem sagen, damit - wenn der Child
                # die Pipe gefüllt hat - der Parent-Prozeß und nicht etwa der
                # Child-Prozeß aufgerufen wird, um die Pipe zu leeren.)
                { execl(TheAsciz(shell), # Shell aufrufen
                        TheAsciz(shell),   # =: argv[0]
                        "-c",              # =: argv[1]
                        TheAsciz(STACK_0), # =: argv[2]
                        NULL
                       );
                }
          _exit(-1); # sollte dies mißlingen, Child-Prozeß beenden
        }
      # Dieses Programmstück wird wieder vom Aufrufer ausgeführt:
      if (child==-1)
        # Etwas ist mißlungen, entweder beim vfork oder beim execl.
        # In beiden Fällen wurde errno gesetzt.
        { close(handles[1]); close(handles[0]); OS_error(); }
      # Wir wollen von der Pipe nur lesen, nicht schreiben:
      if (!( close(handles[1]) ==0)) { OS_error(); }
      # (Muß das dem Betriebssystem sagen, damit - wenn der Parent-Prozeß
      # die Pipe geleert hat - der Child-Prozeß und nicht etwa der
      # Parent-Prozeß aufgerufen wird, um die Pipe wieder zu füllen.)
      end_system_call();
    }
    #endif
    # Stream allozieren:
    { var reg1 object stream = # neuer Stream, nur READ-CHAR erlaubt
        allocate_stream(strmflags_rd_ch_B,strmtype_pipe_in,strm_len+4);
      TheStream(stream)->strm_rd_by = P(rd_by_dummy);
      TheStream(stream)->strm_wr_by = P(wr_by_dummy);
      TheStream(stream)->strm_rd_ch = P(rd_ch_pipe_in);
      TheStream(stream)->strm_rd_ch_last = NIL;
      TheStream(stream)->strm_wr_ch = P(wr_ch_dummy);
      TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
      #ifdef STRM_WR_SS
      TheStream(stream)->strm_wr_ss = P(wr_ss_dummy);
      #endif
      TheStream(stream)->strm_pipe_pid = fixnum(child); # Child-Pid
      TheStream(stream)->strm_pipe_in_handle = allocate_handle(handles[0]); # Read-Handle
      TheStream(stream)->strm_isatty = NIL;
      value1 = stream; mv_count=1; # stream als Wert
      skipSTACK(1);
  }}}


# Pipe-Output-Stream
# ==================

# Zusätzliche Komponenten:
  # define strm_pipe_pid          strm_other[3] # Prozeß-Id, ein Fixnum >=0
  #define strm_pipe_out_handle    strm_ohandle  # Handle für Output

# WRITE-CHAR - Pseudofunktion für Pipe-Output-Streams:
  #define wr_ch_pipe_out  wr_ch_handle

#ifdef STRM_WR_SS
# WRITE-SIMPLE-STRING - Pseudofunktion für Pipe-Output-Streams:
  #define wr_ss_pipe_out  wr_ss_handle
#endif

# Schließt einen Pipe-Output-Stream.
# close_pipe_out(stream);
# > stream : Pipe-Output-Stream
  #ifdef EMUNIX
    local void close_pipe_out (object stream);
    local void close_pipe_out(stream)
      var reg1 object stream;
      { var reg2 Handle handle = TheHandle(TheStream(stream)->strm_pipe_out_handle);
        begin_system_call();
        if ( pclose(&_streamv[handle]) == -1) { OS_error(); }
        end_system_call();
      }
  #endif
  #ifdef UNIX
    #define close_pipe_out  close_ohandle
  #endif

LISPFUNN(make_pipe_output_stream,1)
# (MAKE-PIPE-OUTPUT-STREAM command)
# ruft eine Shell auf, die command ausführt, wobei unsere Pipe in deren
# Standard-Input hinein geht.
  { # command überprüfen:
    funcall(L(string),1); # (STRING command)
    pushSTACK(string_to_asciz(value1)); # als ASCIZ-String
   {var int handles[2]; # zwei Handles für die Pipe
    var reg2 int child;
    #ifdef EMUNIX
    { # Stackaufbau: command.
      begin_system_call();
     {var reg1 FILE* f = popen(TheAsciz(STACK_0),"w");
      if (f==NULL) { OS_error(); }
      child = f->pid;
      handles[1] = fileno(f);
      end_system_call();
    }}
    #endif
    #ifdef UNIX
    { # Shell bestimmen:
      var reg1 object shell = string_to_asciz(O(user_shell)); # User-Shell als ASCIZ-String
      # (Im Gegensatz zu EXECUTE gehen wir hier nicht über den LISP-Pathname.)
      # Stackaufbau: command.
      begin_system_call();
      if (!( pipe(handles) ==0)) { OS_error(); } # Pipe aufbauen.
      # Alles, was in handles[1] reingeschoben wird, kommt bei handles[0]
      # wieder raus. Wir werden dies so benutzen:
      #
      #        write            system            read
      # parent  ->   handles[1]   ->   handles[0]  ->  child
      #
      # einen neuen Prozeß starten:
      if ((child = vfork()) ==0)
        # Dieses Programmstück wird vom Child-Prozeß ausgeführt:
        { if ( dup2(handles[0],stdin_handle) >=0) # Standard-Input umleiten
            if ( close(handles[0]) ==0) # Wir wollen nur über stdin_handle lesen
              if ( close(handles[1]) ==0) # Wir wollen auf die Pipe nicht schreiben
                # (Muß das dem Betriebssystem sagen, damit - wenn der Child
                # die Pipe geleert hat - der Parent-Prozeß und nicht etwa der
                # Child-Prozeß aufgerufen wird, um die Pipe zu wieder zu füllen.)
                { execl(TheAsciz(shell), # Shell aufrufen
                        TheAsciz(shell),   # =: argv[0]
                        "-c",              # =: argv[1]
                        TheAsciz(STACK_0), # =: argv[2]
                        NULL
                       );
                }
          _exit(-1); # sollte dies mißlingen, Child-Prozeß beenden
        }
      # Dieses Programmstück wird wieder vom Aufrufer ausgeführt:
      if (child==-1)
        # Etwas ist mißlungen, entweder beim vfork oder beim execl.
        # In beiden Fällen wurde errno gesetzt.
        { close(handles[1]); close(handles[0]); OS_error(); }
      # Wir wollen auf die Pipe nur schreiben, nicht lesen:
      if (!( close(handles[0]) ==0)) { OS_error(); }
      # (Muß das dem Betriebssystem sagen, damit - wenn der Parent-Prozeß
      # die Pipe gefüllt hat - der Child-Prozeß und nicht etwa der
      # Parent-Prozeß aufgerufen wird, um die Pipe wieder zu leeren.)
      end_system_call();
    }
    #endif
    # Stream allozieren:
    { var reg1 object stream = # neuer Stream, nur WRITE-CHAR erlaubt
        allocate_stream(strmflags_wr_ch_B,strmtype_pipe_out,strm_len+4);
      TheStream(stream)->strm_rd_by = P(rd_by_dummy);
      TheStream(stream)->strm_wr_by = P(wr_by_dummy);
      TheStream(stream)->strm_rd_ch = P(rd_ch_dummy);
      TheStream(stream)->strm_rd_ch_last = NIL;
      TheStream(stream)->strm_wr_ch = P(wr_ch_pipe_out);
      TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
      #ifdef STRM_WR_SS
      TheStream(stream)->strm_wr_ss = P(wr_ss_pipe_out);
      #endif
      TheStream(stream)->strm_pipe_pid = fixnum(child); # Child-Pid
      TheStream(stream)->strm_pipe_out_handle = allocate_handle(handles[1]); # Write-Handle
      value1 = stream; mv_count=1; # stream als Wert
      skipSTACK(1);
  }}}

#endif


#ifdef SOCKETS

# Socket-Stream
# =============

# Verwendung: für X-Windows.

# Zusätzliche Komponenten:
  # define strm_socket_connect strm_other[3] # Liste (host display)

# READ-CHAR - Pseudofunktion für Socket-Streams:
  #define rd_ch_socket  rd_ch_handle

# Stellt fest, ob ein Socket-Stream ein Zeichen verfügbar hat.
# listen_socket(stream)
# > stream : Socket-Stream
# < ergebnis:  0 falls Zeichen verfügbar,
#             -1 falls bei EOF angelangt,
#             +1 falls kein Zeichen verfügbar, aber nicht wegen EOF
# kann GC auslösen
  #define listen_socket  listen_handle

# WRITE-CHAR - Pseudofunktion für Socket-Streams:
  #define wr_ch_socket  wr_ch_handle

#ifdef STRM_WR_SS
# WRITE-SIMPLE-STRING - Pseudofunktion für Socket-Streams:
  #define wr_ss_socket  wr_ss_handle
#endif

# READ-BYTE - Pseudofunktion für Socket-Streams:
  #define rd_by_socket  rd_by_handle

# WRITE-BYTE - Pseudofunktion für Socket-Streams:
  #define wr_by_socket  wr_by_handle

# Schließt einen Socket-Stream.
# close_socket(stream);
# > stream : Socket-Stream
  #define close_socket  close_ihandle

extern int connect_to_server (char* host, int display); # ein Stück X-Source...

LISPFUNN(make_socket_stream,2)
# (SYS::MAKE-SOCKET-STREAM host display)
# liefert einen Socket-Stream für X-Windows oder NIL.
  { if (!mstringp(STACK_1))
      { pushSTACK(STACK_1);
        fehler(
               DEUTSCH ? "Host muß ein String sein, nicht ~" :
               ENGLISH ? "host should be string, not ~" :
               FRANCAIS ? "L'hôte devrait être un chaîne et non ~" :
               ""
              );
      }
    if (!mposfixnump(STACK_0))
      { fehler(
               DEUTSCH ? "Display sollte ein Fixnum >=0 sein, nicht ~" :
               ENGLISH ? "display should be a nonnegative fixnum, not ~" :
               FRANCAIS ? "Le «display» doit être de type FIXNUM >= 0 et non ~" :
               ""
              );
     }
  begin_system_call();
  {var reg2 int handle = connect_to_server(TheAsciz(string_to_asciz(STACK_1)),posfixnum_to_L(STACK_0));
   end_system_call();
   if (handle < 0) { OS_error(); }
   # Liste bilden:
   {var reg1 object list = listof(2); pushSTACK(list); }
   # Stream allozieren:
   {var reg1 object stream = # neuer Stream, nur READ-CHAR und WRITE-CHAR erlaubt
      allocate_stream(strmflags_ch_B,strmtype_socket,strm_len+4);
    TheStream(stream)->strm_rd_by = P(rd_by_socket);
    TheStream(stream)->strm_wr_by = P(wr_by_socket);
    TheStream(stream)->strm_rd_ch = P(rd_ch_socket);
    TheStream(stream)->strm_rd_ch_last = NIL;
    TheStream(stream)->strm_wr_ch = P(wr_ch_socket);
    TheStream(stream)->strm_wr_ch_lpos = Fixnum_0;
    #ifdef STRM_WR_SS
    TheStream(stream)->strm_wr_ss = P(wr_ss_socket);
    #endif
    TheStream(stream)->strm_socket_connect = popSTACK(); # zweielementige Liste
    TheStream(stream)->strm_ihandle =
    TheStream(stream)->strm_ohandle = allocate_handle(handle); # Handle eintragen
    TheStream(stream)->strm_isatty = NIL;
    value1 = stream; mv_count=1; # stream als Wert
  }}}

# Die beiden folgenden Funktionen sollten
# 1. nicht nur auf Handle- und Socket-Streams, sondern auch auf Synonym-
#    und Concatenated-Streams funktionieren, idealerweise auch auf File-Streams.
# 2. das rd_ch_lastchar ebenso verändern wie READ-BYTE.
# 3. auch nicht-simple Byte-Vektoren akzeptieren.
# Für CLX reicht aber die vorliegende Implementation.

# (SYS::READ-N-BYTES stream vector start count)
# liest n Bytes auf einmal.
# Quelle:
#   stream: Handle- oder Socket-Stream
# Ziel: (aref vector start), ..., (aref vector (+ start (- count 1))), wobei
#   vector: semi-simpler 8Bit-Byte-Vektor
#   start: Start-Index in den Vektor
#   count: Anzahl der Bytes

# (SYS::WRITE-N-BYTES stream vector start count)
# schreibt n Bytes auf einmal.
# Quelle: (aref vector start), ..., (aref vector (+ start (- count 1))), wobei
#   vector: semi-simpler 8Bit-Byte-Vektor
#   start: Start-Index in den Vektor
#   count: Anzahl der Bytes
# Ziel:
#   stream: Handle- oder Socket-Stream

# Argumentüberprüfungen:
# Liefert den Index in *index_, den count in *count_, den Datenvektor im
# Stack statt des Vektors, und räumt den Stack um 2 auf.
  local void test_n_bytes_args (uintL* index_, uintL* count_);
  local void test_n_bytes_args(index_,count_)
    var reg3 uintL* index_;
    var reg2 uintL* count_;
    { if (!mstreamp(STACK_3)) { fehler_stream(STACK_3); }
      {var reg1 object stream = STACK_3;
       if (!(   (TheStream(stream)->strm_rd_by == P(rd_by_handle))
             && (TheStream(stream)->strm_wr_by == P(wr_by_handle))
          ) )
         { pushSTACK(stream);
           pushSTACK(TheSubr(subr_self)->name);
           fehler(
                  DEUTSCH ? "~: Stream muß ein Handle-Stream sein, nicht ~" :
                  ENGLISH ? "~: stream must be a handle-stream, not ~" :
                  FRANCAIS ? "~ : Le stream doit être un «handle-stream» et non ~" :
                  ""
                 );
      }  }
      {var reg1 object vector = STACK_2;
       if (!((typecode(vector) == bvector_type) # Bit/Byte-Vektor?
             && ((TheArray(vector)->flags & arrayflags_atype_mask) == Atype_8Bit) # 8Bit
          ) )
         { pushSTACK(vector);
           pushSTACK(TheSubr(subr_self)->name);
           fehler(
                  DEUTSCH ? "~: Argument ~ sollte ein Vektor vom Typ (ARRAY (UNSIGNED-BYTE 8) (*)) sein." :
                  ENGLISH ? "~: argument ~ should be a vector of type (ARRAY (UNSIGNED-BYTE 8) (*))" :
                  FRANCAIS ? "~ : l'argument ~ doit être un vecteur de type (ARRAY (UNSIGNED-BYTTE 8) (*))." :
                  ""
                 );
         }
       if (!mposfixnump(STACK_0)) { fehler_bad_lpos(); }
       *count_ = posfixnum_to_L(popSTACK());
       if (!mposfixnump(STACK_0)) { fehler_bad_lpos(); }
       *index_ = posfixnum_to_L(popSTACK());
       STACK_0 = array1_displace_check(vector,*count_,index_);
    } }

LISPFUNN(read_n_bytes,4)
  { var uintL startindex;
    var uintL totalcount;
    test_n_bytes_args(&startindex,&totalcount);
    if (!(totalcount==0))
      { var reg4 Handle handle = TheHandle(TheStream(STACK_1)->strm_ihandle);
        var reg2 uintL remaining = totalcount;
        restart_it:
       {var reg1 uintB* ptr = &TheSbvector(TheArray(STACK_0)->data)->data[startindex];
        begin_system_call();
        loop
          { var reg3 int ergebnis = read(handle,ptr,remaining);
            if (ergebnis<0)
              { if (errno==EINTR) # Unterbrechung (durch Ctrl-C) ?
                  { end_system_call();
                    pushSTACK(S(read_n_bytes)); tast_break(); # Break-Schleife aufru
                    goto restart_it;
                  }
                OS_error(); # Error melden
              }
            ptr += ergebnis; startindex += ergebnis; remaining -= ergebnis;
            if (remaining==0) break; # fertig?
          }
        end_system_call();
      }}
    skipSTACK(2);
    value1 = T; mv_count=1; # Wert T
  }

LISPFUNN(write_n_bytes,4)
  { var uintL startindex;
    var uintL totalcount;
    test_n_bytes_args(&startindex,&totalcount);
    if (!(totalcount==0))
      { var reg4 Handle handle = TheHandle(TheStream(STACK_1)->strm_ihandle);
        var reg2 uintL remaining = totalcount;
        restart_it:
       {var reg1 uintB* ptr = &TheSbvector(TheArray(STACK_0)->data)->data[startindex];
        begin_system_call();
        loop
          { var reg3 int ergebnis = write(handle,ptr,remaining);
            if (ergebnis<0)
              { if (errno==EINTR) # Unterbrechung (durch Ctrl-C) ?
                  { end_system_call();
                    pushSTACK(S(write_n_bytes)); tast_break(); # Break-Schleife aufru
                    goto restart_it;
                  }
                OS_error(); # Error melden
              }
            if (ergebnis==0) # nicht erfolgreich?
              { fehler_unwritable(S(write_n_bytes),STACK_1); }
            ptr += ergebnis; startindex += ergebnis; remaining -= ergebnis;
            if (remaining==0) break; # fertig?
          }
        end_system_call();
      }}
    skipSTACK(2);
    value1 = T; mv_count=1; # Wert T
  }

#endif


# Streams allgemein
# =================

# UP: Initialisiert die Stream-Variablen.
# init_streamvars();
# kann GC auslösen
  global void init_streamvars (void);
  global void init_streamvars()
    {
     #ifdef KEYBOARD
     #ifdef ATARI
     keytables = *(KEYTAB*)XBIOS_GetKeyTbl(); # Keyboard-Stream initialisieren
     #endif
     {var reg1 object stream = make_keyboard_stream();
      define_variable(S(keyboard_input),stream);   # *KEYBOARD-INPUT*
     }
     #endif
     #ifdef GNU_READLINE
     rl_readline_name = "Clisp";
     rl_attempted_completion_function = (Function*)&lisp_completion;
     rl_completion_entry_function = (Function*)&lisp_completion_more;
     #endif
     {var reg1 object stream = make_terminal_stream();
      define_variable(S(terminal_io),stream);      # *TERMINAL-IO*
     }
     {var reg1 object stream = make_synonym_stream(S(terminal_io)); # Synonym-Stream auf *TERMINAL-IO*
      define_variable(S(query_io),stream);         # *QUERY-IO*
      define_variable(S(debug_io),stream);         # *DEBUG-IO*
      define_variable(S(standard_input),stream);   # *STANDARD-INPUT*
      define_variable(S(standard_output),stream);  # *STANDARD-OUTPUT*
      define_variable(S(error_output),stream);     # *ERROR-OUTPUT*
      define_variable(S(trace_output),stream);     # *TRACE-OUTPUT*
     }
     #ifdef PRINTER_ATARI
     # *PRINTER-OUTPUT* mit Printer-Stream initialisieren:
     define_variable(S(printer_output),make_printer_stream());
     #endif
    }

# Liefert Fehlermeldung, wenn der Wert des Symbols sym kein Stream ist.
  local nonreturning void fehler_value_stream(sym)
    var reg1 object sym;
    { # Vor der Fehlermeldung eventuell noch reparieren
      # (so wie bei init_streamvars bzw. init_pathnames initialisiert):
      var reg2 object stream;
      pushSTACK(sym); # sym retten
      #ifdef KEYBOARD
      if (eq(sym,S(keyboard_input)))
        # Keyboard-Stream als Default
        { stream = make_keyboard_stream(); }
      else
      #endif
      if (eq(sym,S(terminal_io)))
        # Terminal-Stream als Default
        { stream = make_terminal_stream(); }
      elif (eq(sym,S(query_io)) || eq(sym,S(debug_io)) ||
            eq(sym,S(standard_input)) || eq(sym,S(standard_output)) ||
            eq(sym,S(error_output)) || eq(sym,S(trace_output))
           )
        # Synonym-Stream auf *TERMINAL-IO* als Default
        { stream = make_synonym_stream(S(terminal_io)); }
      #ifdef PRINTER_ATARI
      elif (eq(sym,S(printer_output)))
        # Printer-Stream als Default
        { stream = make_printer_stream(); }
      #endif
      else
        # sonstiges Symbol, nicht reparierbar -> sofort Fehlermeldung:
        { pushSTACK(Symbol_value(sym)); # Variablenwert
          pushSTACK(sym); # Variable
          fehler(
                 DEUTSCH ? "Der Wert von ~ ist kein Stream: ~" :
                 ENGLISH ? "The value of ~ is not a stream: ~" :
                 FRANCAIS ? "La valeur de ~ n'est pas de type STREAM : ~" :
                 ""
                );
        }
      sym = popSTACK();
      # Reparatur beendet: stream ist der neue Wert von sym.
     {var reg3 object oldvalue = Symbol_value(sym);
      Symbol_value(sym) = stream;
      pushSTACK(stream); # neuer Variablenwert
      pushSTACK(oldvalue); # alter Variablenwert
      pushSTACK(sym); # Variable
      fehler(
             DEUTSCH ? "Der Wert von ~ war kein Stream: ~. Wurde zurückgesetzt auf ~." :
             ENGLISH ? "The value of ~ was not a stream: ~. It has been changed to ~." :
             FRANCAIS ? "La valeur de ~ n'était pas de type STREAM : ~. Changé en ~." :
             ""
            );
    }}

#ifdef GNU_READLINE

# Hilfsfunktionen für die GNU readline Library:

local nonreturning void rl_memory_abort (void);
local nonreturning void rl_memory_abort()
  { # Wenn für die Readline-Library der Speicher nicht mehr reicht,
    # schmeißen wir sie raus und ersetzen den Terminal-Stream durch einen,
    # der ohne sie auskommt.
    rl_deprep_terminal(); # alle komischen ioctl()s rückgängig machen
    rl_present_p = FALSE;
    Symbol_value(S(terminal_io)) = make_terminal_stream();
    fehler(
           DEUTSCH ? "Readline-Library: kein freier Speicher mehr da." :
           ENGLISH ? "readline library: out of memory." :
           FRANCAIS ? "Bibliothèque readline: mémoire épuisée." :
           ""
          );
  }

global char* xmalloc (int count);
global char* xmalloc(count)
  var reg2 int count;
  { var reg1 char* tmp = (char*)malloc(count);
    if (tmp) return tmp; else rl_memory_abort();
  }

global char* xrealloc (char* ptr, int count);
global char* xrealloc(ptr,count)
  var reg2 char* ptr;
  var reg2 int count;
  { var reg1 char* tmp = (ptr==NULL ? (char*)malloc(count) : (char*)realloc(ptr,count));
    if (tmp) return tmp; else rl_memory_abort();
  }

#endif

LISPFUNN(input_stream_p,1)
# (INPUT-STREAM-P stream), CLTL S. 332
  { var reg1 object stream = popSTACK();
    if (!streamp(stream)) { fehler_stream(stream); }
    if (TheStream(stream)->strmflags & strmflags_rd_B) # READ-BYTE oder READ-CHAR erlaubt ?
      { value1 = T; mv_count=1; } # Wert T
      else
      { value1 = NIL; mv_count=1; } # Wert NIL
  }

LISPFUNN(output_stream_p,1)
# (OUTPUT-STREAM-P stream), CLTL S. 332
  { var reg1 object stream = popSTACK();
    if (!streamp(stream)) { fehler_stream(stream); }
    if (TheStream(stream)->strmflags & strmflags_wr_B) # WRITE-BYTE oder WRITE-CHAR erlaubt ?
      { value1 = T; mv_count=1; } # Wert T
      else
      { value1 = NIL; mv_count=1; } # Wert NIL
  }

LISPFUNN(stream_element_type,1)
# (STREAM-ELEMENT-TYPE stream), CLTL S. 332
# liefert NIL (für geschlossene Streams) oder CHARACTER oder INTEGER oder T
# oder (spezieller) STRING-CHAR oder (UNSIGNED-BYTE n) oder (SIGNED-BYTE n).
  { var reg1 object stream = popSTACK();
    if (!streamp(stream)) { fehler_stream(stream); }
   {var reg2 object eltype;
    if ((TheStream(stream)->strmflags & strmflags_open_B) == 0)
      # Stream geschlossen
      { eltype = NIL; }
      else
      # Stream offen
      { switch (TheStream(stream)->strmtype)
          { # erst die Streamtypen mit eingeschränkten Element-Typen:
            #ifdef KEYBOARD
            case strmtype_keyboard:
            #endif
            case strmtype_ch_file:
              # CHARACTER
              eltype = S(character); break;
            case strmtype_terminal:
            case strmtype_sch_file:
            case strmtype_str_in:
            case strmtype_str_out:
            case strmtype_str_push:
            case strmtype_pphelp:
            case strmtype_buff_in:
            case strmtype_buff_out:
            #ifdef WINDOWS
            case strmtype_window:
            #endif
            #ifdef PRINTER
            case strmtype_printer:
            #endif
            #ifdef PIPES
            case strmtype_pipe_in:
            case strmtype_pipe_out:
            #endif
              # STRING-CHAR
              eltype = S(string_char); break;
            case strmtype_iu_file:
              # (UNSIGNED-BYTE bitsize)
              pushSTACK(S(unsigned_byte));
              pushSTACK(TheStream(stream)->strm_file_bitsize);
              eltype = listof(2);
              break;
            case strmtype_is_file:
              # (SIGNED-BYTE bitsize)
              pushSTACK(S(signed_byte));
              pushSTACK(TheStream(stream)->strm_file_bitsize);
              eltype = listof(2);
              break;
            # dann die allgemeinen Streams:
            #ifdef HANDLES
            case strmtype_handle:
            #endif
            default:
              { var reg1 uintB flags = TheStream(stream)->strmflags;
                if (flags & strmflags_by_B)
                  { if (flags & strmflags_ch_B)
                      # (OR CHARACTER INTEGER)
                      { pushSTACK(S(or)); pushSTACK(S(character)); pushSTACK(S(integer));
                        eltype = listof(3);
                      }
                      else
                      # INTEGER
                      { eltype = S(integer); }
                  }
                  else
                  { if (flags & strmflags_ch_B)
                      # CHARACTER
                      { eltype = S(character); }
                      else
                      # NIL
                      { eltype = NIL; }
                  }
                break;
              }
      }   }
    value1 = eltype; mv_count=1;
  }}

# UP: Stellt fest, ob ein Stream "interaktiv" ist, d.h. ob Input vom Stream
# vermutlich von einem vorher ausgegebenen Prompt abhängen wird.
# interactive_stream_p(stream)
# > stream: Stream
  local boolean interactive_stream_p (object stream);
  local boolean interactive_stream_p(stream)
    var reg1 object stream;
    { start:
      if ((TheStream(stream)->strmflags & strmflags_rd_B) == 0)
        # Stream für Input geschlossen
        { return FALSE; }
      # Stream offen
      switch (TheStream(stream)->strmtype)
        {
          #ifdef KEYBOARD
          case strmtype_keyboard:
          #endif
          case strmtype_terminal:
          case strmtype_buff_in:
          #ifdef HANDLES
          case strmtype_handle:
          #endif
          #ifdef PIPES
          case strmtype_pipe_in:
          #endif
            return TRUE;
          case strmtype_synonym:
            # Synonym-Stream: weiterverfolgen
            { var reg2 object symbol = TheStream(stream)->strm_synonym_symbol;
              stream = get_synonym_stream(symbol);
              /* return interactive_stream_p(stream); */ # entrekursiviert:
              goto start;
            }
          case strmtype_concat:
            # den ersten der Streams abfragen:
            { var reg2 object streamlist = TheStream(stream)->strm_concat_list; # Liste von Streams
              if (consp(streamlist))
                { stream = Car(streamlist);
                  /* return interactive_stream_p(stream); */ # entrekursiviert:
                  goto start;
                }
                else
                { return FALSE; }
            }
          case strmtype_twoway:
          case strmtype_echo:
            { # Two-Way-Stream oder Echo-Stream: Input-Stream anschauen
              stream = TheStream(stream)->strm_twoway_input;
              /* return interactive_stream_p(stream); */ # entrekursiviert:
              goto start;
            }
          case strmtype_sch_file:
          case strmtype_ch_file:
          case strmtype_iu_file:
          case strmtype_is_file:
          case strmtype_str_in:
          default:
            return FALSE;
    }   }

LISPFUNN(interactive_stream_p,1)
# (INTERACTIVE-STREAM-P stream), CLTL2 S. 507/508
# stellt fest, ob stream interaktiv ist.
  { value1 = (interactive_stream_p(popSTACK()) ? T : NIL); mv_count=1; }

# UP: Schließt einen Stream.
# stream_close(&stream);
# > stream: Stream
# < stream: Stream
# kann GC auslösen
  global void stream_close (object* stream_);
  global void stream_close(stream_)
    var reg2 object* stream_;
    { var reg1 object stream = *stream_;
      if ((TheStream(stream)->strmflags & strmflags_open_B) == 0) # Stream schon geschlossen?
        return;
      # Typspezifische Routine aufrufen (darf GC auslösen):
      switch (TheStream(stream)->strmtype)
        {
          #ifdef KEYBOARD
          case strmtype_keyboard:
          #endif
          case strmtype_terminal:
            pushSTACK(stream);
            fehler(
                   DEUTSCH ? "~ darf nicht geschlossen werden." :
                   ENGLISH ? "~ may not be closed" :
                   FRANCAIS ? "Il est interdit de fermer ~." :
                   ""
                  );
            break;
          case strmtype_sch_file:
          case strmtype_ch_file:
          case strmtype_iu_file:
          case strmtype_is_file:
            close_file(stream); break;
          case strmtype_synonym:
            close_synonym(stream); break;
          case strmtype_broad: break; # nichtrekursiv
          case strmtype_concat: break; # nichtrekursiv
          case strmtype_twoway: break; # nichtrekursiv
          case strmtype_echo: break; # nichtrekursiv
          case strmtype_str_in:
            close_str_in(stream); break;
          case strmtype_str_out: break;
          case strmtype_str_push: break;
          case strmtype_pphelp: break;
          case strmtype_buff_in:
            close_buff_in(stream); break;
          case strmtype_buff_out:
            close_buff_out(stream); break;
          #ifdef WINDOWS
          case strmtype_window:
            close_window(stream); break;
          #endif
          #ifdef PRINTER_ATARI
          case strmtype_printer: break;
          #endif
          #ifdef PRINTER_AMIGAOS
          case strmtype_printer:
            close_printer(stream); break;
          #endif
          #ifdef HANDLES
          case strmtype_handle:
            close_handle(stream); break;
          #endif
          #ifdef PIPES
          case strmtype_pipe_in:
            close_pipe_in(stream); break;
          case strmtype_pipe_out:
            close_pipe_out(stream); break;
          #endif
          #ifdef SOCKETS
          case strmtype_socket:
            close_socket(stream); break;
          #endif
          default: NOTREACHED
        }
      # Dummys eintragen:
      close_dummys(*stream_);
    }

# UP: Schließt eine Liste offener Files.
# close_some_files(list);
# > list: Liste von offenen Streams
# kann GC auslösen
  global void close_some_files (object list);
  global void close_some_files(list)
    var reg2 object list;
    { pushSTACK(NIL); # dummy
      pushSTACK(list); # list
      while (mconsp(STACK_0))
        { var reg1 object streamlist = STACK_0;
          STACK_0 = Cdr(streamlist); # restliche Streams
          STACK_1 = Car(streamlist); # ein Stream aus der Liste
          stream_close(&STACK_1); # schließen
        }
      skipSTACK(2);
    }

# UP: Schließt alle offenen Files.
# close_all_files();
# kann GC auslösen
  global void close_all_files (void);
  global void close_all_files()
    { close_some_files(O(open_files)); } # Liste aller offenen File-Streams

# UP: Erklärt alle offenen File-Streams für geschlossen.
# closed_all_files();
  global void closed_all_files (void);
  global void closed_all_files()
    { var reg2 object streamlist = O(open_files); # Liste aller offenen File-Streams
      while (consp(streamlist))
        { var reg1 object stream = Car(streamlist); # ein Stream aus der Liste
          if_strm_bfile_p(stream, # File-Stream ?
            { if (!nullp(TheStream(stream)->strm_file_handle)) # mit Handle /= NIL ?
                # ja: Stream noch offen
                { closed_file(stream); }
            },
            ; );
          close_dummys(stream);
          streamlist = Cdr(streamlist); # restliche Streams
        }
      O(open_files) = NIL; # keine offenen Files mehr
    }

LISPFUN(close,1,0,norest,key,1, (kw(abort)) )
# (CLOSE stream :abort), CLTL S. 332
  { skipSTACK(1); # :ABORT-Argument ignorieren
   {var reg1 object stream = STACK_0; # Argument
    if (!streamp(stream)) { fehler_stream(stream); } # muß ein Stream sein
    stream_close(&STACK_0); # schließen
    skipSTACK(1);
    value1 = T; mv_count=1; # T als Ergebnis
  }}

# UP: Stellt fest, ob im Stream stream ein Zeichen sofort verfügbar ist.
# stream_listen(stream)
# > stream: Stream
# < ergebnis:  0 falls Zeichen verfügbar,
#             -1 falls bei EOF angelangt,
#             +1 falls kein Zeichen verfügbar, aber nicht wegen EOF
# kann GC auslösen
  global signean stream_listen (object stream);
  global signean stream_listen(stream)
    var reg1 object stream;
    { check_SP(); check_STACK();
      if (mposfixnump(TheStream(stream)->strm_rd_ch_last)) # Char nach UNREAD ?
        { return signean_null; } # ja -> verfügbar
        else
        # sonst nach Streamtyp verzweigen.
        # Jede Einzelroutine darf GC auslösen. Außer beim Keyboard-Stream
        # oder Terminal-Stream handelt es sich um einen reinen EOF-Test.
        { switch (TheStream(stream)->strmtype)
            {
              #ifdef KEYBOARD
              case strmtype_keyboard: return listen_keyboard(stream);
              #endif
              case strmtype_terminal:
                #ifdef ATARI
                return listen_terminal(stream);
                #endif
                #if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX) || defined(AMIGAOS)
                terminalcase(stream,
                             { return listen_terminal1(stream); },
                             { return listen_terminal2(stream); },
                             { return listen_terminal3(stream); }
                            );
                #endif
                NOTREACHED
              case strmtype_sch_file:
                if (TheStream(stream)->strmflags & strmflags_rd_ch_B)
                  { return listen_sch_file(stream); }
                  else
                  { return signean_minus; } # kein READ-CHAR
              case strmtype_ch_file:
                if (TheStream(stream)->strmflags & strmflags_rd_ch_B)
                  { return listen_ch_file(stream); }
                  else
                  { return signean_minus; } # kein READ-CHAR
              case strmtype_iu_file:  return signean_minus; # kein READ-CHAR
              case strmtype_is_file:  return signean_minus; # kein READ-CHAR
              case strmtype_synonym:  return listen_synonym(stream);
              case strmtype_broad:    return signean_minus; # kein READ-CHAR
              case strmtype_concat:   return listen_concat(stream);
              case strmtype_twoway:   return listen_twoway(stream);
              case strmtype_echo:     return listen_twoway(stream);
              case strmtype_str_in:   return listen_str_in(stream);
              case strmtype_str_out:  return signean_minus; # kein READ-CHAR
              case strmtype_str_push: return signean_minus; # kein READ-CHAR
              case strmtype_pphelp:   return signean_minus; # kein READ-CHAR
              case strmtype_buff_in:  return listen_buff_in(stream);
              case strmtype_buff_out: return signean_minus; # kein READ-CHAR
              #ifdef WINDOWS
              case strmtype_window:   return signean_minus; # kein READ-CHAR
              #endif
              #ifdef PRINTER
              case strmtype_printer:  return signean_minus; # kein READ-CHAR
              #endif
              #ifdef HANDLES
              case strmtype_handle:
                if (TheStream(stream)->strmflags & strmflags_rd_ch_B)
                  { return listen_handle(stream); }
                  else
                  { return signean_minus; } # kein READ-CHAR
              #endif
              #ifdef PIPES
              case strmtype_pipe_in:  return listen_pipe_in(stream);
              case strmtype_pipe_out: return signean_minus; # kein READ-CHAR
              #endif
              #ifdef SOCKETS
              case strmtype_socket:   return listen_socket(stream);
              #endif
              default: # Allgemein: nur EOF abfragen
                if (TheStream(stream)->strmflags & strmflags_rd_ch_B)
                  { pushSTACK(stream);
                   {var reg2 object nextchar = peek_char(&STACK_0);
                    skipSTACK(1);
                    if (eq(nextchar,eof_value))
                      { return signean_minus; } # EOF erreicht
                      else
                      { return signean_null; }
                  }}
                  else
                  { return signean_minus; } # kein READ-CHAR
    }   }   }

# UP: Löscht bereits eingegebenen interaktiven Input von einem Stream stream.
# clear_input(stream)
# > stream: Stream
# < ergebnis: TRUE falls Input gelöscht wurde
# kann GC auslösen
  global boolean clear_input (object stream);
  global boolean clear_input(stream)
    var reg1 object stream;
    { check_SP(); check_STACK();
      pushSTACK(stream); # Stream retten
      # Typspezifische Routine aufrufen (darf GC auslösen).
      # Nur beim Keyboard-Stream und Terminal-Stream wird etwas getan.
     {var reg2 boolean ergebnis;
      switch (TheStream(stream)->strmtype)
        {
          #ifdef KEYBOARD
          case strmtype_keyboard:
            ergebnis = clear_input_keyboard(stream); break;
          #endif
          case strmtype_terminal:
            #ifdef ATARI
            ergebnis = clear_input_terminal(stream);
            #endif
            #if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX) || defined(AMIGAOS)
            terminalcase(stream,
                         { ergebnis = clear_input_terminal1(stream); },
                         { ergebnis = clear_input_terminal2(stream); },
                         { ergebnis = clear_input_terminal3(stream); }
                        );
            #endif
            break;
          case strmtype_synonym:
            ergebnis = clear_input_synonym(stream); break;
          case strmtype_concat:
            ergebnis = clear_input_concat(stream); break;
          case strmtype_twoway:
          case strmtype_echo:
            ergebnis = clear_input_twoway(stream); break;
          case strmtype_buff_in:
            ergebnis = clear_input_buff_in(stream); break;
          #ifdef HANDLES
          case strmtype_handle:
            if (TheStream(stream)->strmflags & strmflags_rd_ch_B)
              { ergebnis = clear_input_handle(stream); }
              else
              { ergebnis = FALSE; }
            break;
          #endif
          #ifdef PIPES
          case strmtype_pipe_in: # Pipe: nichts löschen??
          #endif
          #ifdef SOCKETS
          case strmtype_socket: # Socket: nichts löschen??
          #endif
          default:
            ergebnis = FALSE; break;
        }
      stream = popSTACK();
      if (ergebnis)
        # Input wurde gelöscht -> auch das Lastchar muß gelöscht werden:
        { TheStream(stream)->strm_rd_ch_last = NIL; }
      return ergebnis;
    }}

# UP: Wartenden Output eines Stream stream ans Ziel bringen.
# finish_output(stream);
# > stream: Stream
# kann GC auslösen
  global void finish_output (object stream);
  global void finish_output(stream)
    var reg1 object stream;
    { if (TheStream(stream)->strmflags & strmflags_wr_B) # Output-Stream?
        # nein -> fertig, ja -> nach Streamtyp verzweigen:
        { switch (TheStream(stream)->strmtype)
            { case strmtype_terminal:
                finish_output_terminal(stream); break;
              case strmtype_sch_file:
              case strmtype_ch_file:
              case strmtype_iu_file:
              case strmtype_is_file:
                finish_output_file(stream); break;
              case strmtype_synonym:
                finish_output_synonym(stream); break;
              case strmtype_broad:
                finish_output_broad(stream); break;
              case strmtype_twoway:
              case strmtype_echo:
                finish_output_twoway(stream); break;
              case strmtype_buff_out:
                finish_output_buff_out(stream); break;
              #ifdef PRINTER_AMIGAOS
              case strmtype_printer: # Printer:
                # Schließen und neu aufmachen würde vermutlich einen
                # Seitenvorschub ausgeben, und das ist ja wohl nicht erwünscht.
                break; # Daher nichts tun.
              #endif
              #ifdef HANDLES
              case strmtype_handle:
                finish_output_handle(stream); break;
              #endif
              #ifdef PIPES
              case strmtype_pipe_out: # Pipe: kann nichts tun
              #endif
              #ifdef SOCKETS
              case strmtype_socket: # Socket: kann nichts tun
              #endif
              default: # nichts tun
                break;
        }   }
    }

# UP: Wartenden Output eines Stream stream ans Ziel bringen.
# force_output(stream);
# > stream: Stream
# kann GC auslösen
  global void force_output (object stream);
  global void force_output(stream)
    var reg1 object stream;
    { if (TheStream(stream)->strmflags & strmflags_wr_B) # Output-Stream?
        # nein -> fertig, ja -> nach Streamtyp verzweigen:
        { switch (TheStream(stream)->strmtype)
            { case strmtype_terminal:
                force_output_terminal(stream); break;
              case strmtype_sch_file:
              case strmtype_ch_file:
              case strmtype_iu_file:
              case strmtype_is_file:
                force_output_file(stream); break;
              case strmtype_synonym:
                force_output_synonym(stream); break;
              case strmtype_broad:
                force_output_broad(stream); break;
              case strmtype_twoway:
              case strmtype_echo:
                force_output_twoway(stream); break;
              case strmtype_buff_out:
                force_output_buff_out(stream); break;
              #ifdef PRINTER_AMIGAOS
              case strmtype_printer: # Printer:
                # Schließen und neu aufmachen würde vermutlich einen
                # Seitenvorschub ausgeben, und das ist ja wohl nicht erwünscht.
                break; # Daher nichts tun.
              #endif
              #ifdef HANDLES
              case strmtype_handle:
                force_output_handle(stream); break;
              #endif
              #ifdef PIPES
              case strmtype_pipe_out: # Pipe: kann nichts tun
              #endif
              #ifdef SOCKETS
              case strmtype_socket: # Socket: kann nichts tun
              #endif
              default: # nichts tun
                break;
        }   }
    }

# UP: Wartenden Output eines Stream stream löschen.
# clear_output(stream);
# > stream: Stream
# kann GC auslösen
  global void clear_output (object stream);
  global void clear_output(stream)
    var reg1 object stream;
    { # Auf dem ATARI oder unter DOS ist zwar bei keinem File- oder
      # Terminal-Stream etwas zu tun, aber das kann man nicht ausnutzen, denn
      # clear_output auf Buffered-Output-Streams geht immer.
      if (TheStream(stream)->strmflags & strmflags_wr_B) # Output-Stream?
        # nein -> fertig, ja -> nach Streamtyp verzweigen:
        { switch (TheStream(stream)->strmtype)
            { case strmtype_terminal:
                #if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX) || defined(AMIGAOS)
                terminalcase(stream,
                             { clear_output_terminal1(stream); },
                             { clear_output_terminal2(stream); },
                             { clear_output_terminal3(stream); }
                            );
                #endif
                break;
              case strmtype_sch_file:
              case strmtype_ch_file:
              case strmtype_iu_file:
              case strmtype_is_file:
                # File: nichts tun (würde die File-Verwaltung durcheinanderbringen)
                break;
              case strmtype_synonym:
                clear_output_synonym(stream); break;
              case strmtype_broad:
                clear_output_broad(stream); break;
              case strmtype_twoway:
              case strmtype_echo:
                clear_output_twoway(stream); break;
              case strmtype_buff_out:
                clear_output_buff_out(stream); break;
              #ifdef PRINTER_AMIGAOS
              case strmtype_printer: # Printer: ungebuffert, also nichts zu tun
                break;
              #endif
              #ifdef HANDLES
              case strmtype_handle:
                clear_output_handle(stream); break;
              #endif
              #ifdef PIPES
              case strmtype_pipe_out: # Pipe: geht nicht
                break;
              #endif
              #ifdef SOCKETS
              case strmtype_socket: # Socket: geht nicht
                break;
              #endif
              default: # nichts tun
                break;
        }   }
    }

# UP: Liefert die Line-Position eines Streams.
# get_line_position(stream)
# > stream: Stream
# < ergebnis: Line-Position (Fixnum >=0)
  global object get_line_position (object stream);
  global object get_line_position(stream)
    var reg1 object stream;
    { check_SP();
      start:
      switch (TheStream(stream)->strmtype)
        { case strmtype_synonym:
            # Synonym-Stream: weiterverfolgen
            { var reg2 object symbol = TheStream(stream)->strm_synonym_symbol;
              stream = get_synonym_stream(symbol);
              /* return get_line_position(stream); */ # entrekursiviert:
              goto start;
            }
          case strmtype_broad:
            # Broadcast-Stream:
            # Maximum der Line-Positions der einzelnen Streams
            { var reg2 object streamlist = TheStream(stream)->strm_broad_list;
              var reg3 uintL maximum = 0; # bisheriges Maximum := 0
              while (consp(streamlist))
                { var reg4 uintL next = # Line-Position des nächsten Teilstreams
                    posfixnum_to_L(get_line_position(Car(streamlist)));
                  if (next > maximum) { maximum = next; } # Maximum nehmen
                  streamlist = Cdr(streamlist);
                }
              return fixnum(maximum); # Maximum als Ergebnis
            }
          case strmtype_twoway:
          case strmtype_echo:
            { # Two-Way-Stream oder Echo-Stream: Output-Stream anschauen
              stream = TheStream(stream)->strm_twoway_output;
              /* return get_line_position(stream); */ # entrekursiviert:
              goto start;
            }
          default: # normaler Stream
            return TheStream(stream)->strm_wr_ch_lpos;
    }   }

LISPFUN(read_byte,1,2,norest,nokey,0,NIL)
# (READ-BYTE stream [eof-error-p [eof-value]]), CLTL S. 382
  { # Stream überprüfen:
    var reg1 object stream = STACK_2;
    if (!streamp(stream)) { fehler_stream(stream); }
    # Integer lesen:
   {var reg2 object obj = read_byte(stream);
    if (eq(obj,eof_value))
      # EOF-Behandlung
      { if (!nullp(STACK_1)) # eof-error-p /= NIL (z.B. = #<UNBOUND>) ?
          # Error melden:
          { pushSTACK(STACK_2); # Stream
            pushSTACK(S(read_byte));
            fehler(
                   DEUTSCH ? "~: Eingabestream ~ ist zu Ende." :
                   ENGLISH ? "~: input stream ~ has reached its end" :
                   FRANCAIS ? "~ : Arrivée en fin du «stream» d'entrée ~." :
                   ""
                  );
          }
          else
          # EOF verarzten:
          { var reg2 object eofval = STACK_0;
            if (eq(eofval,unbound)) { eofval = eof_value; } # Default ist #<EOF>
            value1 = eofval; mv_count=1; skipSTACK(3); # eofval als Wert
          }
      }
      else
      { value1 = obj; mv_count=1; skipSTACK(3); } # obj als Wert
  }}

LISPFUNN(write_byte,2)
# (WRITE-BYTE integer stream), CLTL S. 385
  { # Stream überprüfen:
    var reg1 object stream = STACK_0;
    if (!streamp(stream)) { fehler_stream(stream); }
   {# Integer überprüfen:
    var reg2 object obj = STACK_1;
    if (!integerp(obj)) { fehler_integer(stream,obj); }
    # Integer schreiben:
    write_byte(stream,obj);
    value1 = STACK_1; mv_count=1; skipSTACK(2); # obj als Wert
  }}

# UP: Überprüft, ob ein Argument ein offener File-Stream ist.
# check_open_file_stream(obj);
# > obj: Argument
# > subr_self: Aufrufer (ein SUBR)
  local void check_open_file_stream (object obj);
  local void check_open_file_stream(obj)
    var reg1 object obj;
    { if (!streamp(obj)) goto fehler_bad_obj; # Stream ?
      if_strm_bfile_p(obj, ; , goto fehler_bad_obj; ); # Streamtyp File-Stream ?
      if ((TheStream(obj)->strmflags & strmflags_open_B) == 0) goto fehler_bad_obj; # Stream offen ?
      if (nullp(TheStream(obj)->strm_file_handle)) goto fehler_bad_obj; # und Handle /= NIL ?
      return; # ja -> OK
      fehler_bad_obj:
        pushSTACK(obj);
        pushSTACK(TheSubr(subr_self)->name);
        fehler(
               DEUTSCH ? "~: Argument muß ein offener File-Stream sein, nicht ~" :
               ENGLISH ? "~: argument ~ is not an open file stream" :
               FRANCAIS ? "~ : L'argument ~ doit être un «stream» ouvert de fichier et non ~." :
               ""
              );
    }

LISPFUN(file_position,1,1,norest,nokey,0,NIL)
# (FILE-POSITION file-stream [position]), CLTL S. 425
  { var reg1 object position = popSTACK();
    var reg2 object stream = popSTACK();
    check_open_file_stream(stream); # stream überprüfen
    if (eq(position,unbound))
      # position nicht angegeben -> Position als Wert:
      { value1 = TheStream(stream)->strm_file_position; mv_count=1; }
      else
      { if (eq(position,S(Kstart)))
          # :START -> an den Anfang positionieren:
          { position_file_start(stream); }
        elif (eq(position,S(Kend)))
          # :END -> ans Ende positionieren:
          { position_file_end(stream); }
        elif (posfixnump(position))
          # an die angegebene Position positionieren:
          { position_file(stream,posfixnum_to_L(position)); }
        else
          # Unzulässiges Position-Argument
          { pushSTACK(position); pushSTACK(S(Kend)); pushSTACK(S(Kstart));
            pushSTACK(TheSubr(subr_self)->name);
            fehler(
                   DEUTSCH ? "~: Position-Argument muß ~ oder ~ oder ein Fixnum >=0 sein, nicht ~" :
                   ENGLISH ? "~: position argument should be ~ or ~ or a nonnegative fixnum, not ~" :
                   FRANCAIS ? "~ : L'argument position doit être ~, ~ ou de type FIXNUM positif ou zéro, mais non ~." :
                   ""
                  );
          }
        value1 = T; mv_count=1; # Wert T
      }
  }

LISPFUNN(file_length,1)
# (FILE-LENGTH file-stream), CLTL S. 425
  { var reg1 object stream = popSTACK();
    check_open_file_stream(stream); # stream überprüfen
    # Position merken:
   {var reg2 object position = TheStream(stream)->strm_file_position;
    # ans Ende positionieren:
    position_file_end(stream);
    # Ende-Position merken:
    {var reg3 object endposition = TheStream(stream)->strm_file_position;
     # an die alte Position zurückpositionieren:
     position_file(stream,posfixnum_to_L(position));
     value1 = endposition; mv_count=1; # Ende-Position als Wert
  }}}

# Tabelle aller Pseudofunktionen
  global struct pseudofun_tab_ pseudofun_tab =
    {
      #define PSEUDOFUN  PSEUDOFUN_B
      #include "pseudofun.c"
      #undef PSEUDOFUN
    };

# ==============================================================================

# filestatus/if_file_exists, file_datetime durch break_sem_4 schützen??
# Signalbehandlung bei EXECUTE, SHELL, MAKE-PIPE-INPUT-STREAM, MAKE-PIPE-OUTPUT-STREAM ??

