# Rexx-Interface für CLISP
# Jörg Höhle 13.3.1993


#include "lispbibl.c"

#ifdef AMIGAOS

#include "amiga2.c"


#ifdef DEBUG_REXX
  #define debug_asciz_out  asciz_out
  #define debug_dez_out  dez_out
  #define debug_hex_out  hex_out
#else
  #define debug_asciz_out(x)
  #define debug_dez_out(x)
  #define debug_hex_out(x)
#endif


# Fehlermeldung wenn kein Rexx möglich
# fehler_norexx();
# > subr_self: Aufrufer (ein SUBR)
  local nonreturning void fehler_norexx (void);
  local nonreturning void fehler_norexx()
    { fehler(
             DEUTSCH ? "Keine Kommunikation mit ARexx möglich." :
             ENGLISH ? "Communication with ARexx isn't possible." :
             FRANCAIS ? "La communication avec ARexx n'est pas possible." :
             ""
            );
    }

# Speicher freigeben, der wegen Fehler nicht freigegeben wurde.
  local UBYTE* rexxLostArgstr = NULL;
  local void handle_lost_resources (void);
  local void handle_lost_resources()
    { if (rexxLostArgstr)
        { begin_system_call();
          DeleteArgstring(rexxLostArgstr);
          end_system_call();
          rexxLostArgstr = NULL;
    }   }

# Die Anzahl der auf Antwort (durch andere Prozesse) wartenden Messages:
  local uintC rexxNeededReplies = 0;

# O(rexx_inmsg_list) ist eine Liste von Foreigns, die jeweils die ein-
# gegangenen und auf Antwort (durch CLISP) wartenden Messages repräsentieren.

# Sucht eine gegebene Message in O(rexx_inmsg_list):
  local object find_inmsg (FOREIGN pointer);
  local object find_inmsg(pointer)
    var reg2 FOREIGN pointer;
    { var reg1 object current;
      for (current = O(rexx_inmsg_list); consp(current); current = Cdr(current))
        { if (TheForeign(Car(current)) == pointer) { return Car(current); } }
      return NIL;
    }

# Der Message Port, auf dem wir arbeiten:
  local struct MsgPort * rexxPort = NULL;
# Sein Name:
  local UBYTE rexxPortName[] = {'C','L','I','S','P','1','\0','\0'};
# Position der Ziffer darin:
  #define NRPOSITION 5
# Default-Extension für ARexx-Kommandofiles:
  local UBYTE rexxExtension[] = "cl";
# Signalnummer, mit der wir auf Ereignisse an diesem Port warten können:
  local ULONG rexxPortBit = 0UL;

LISPFUN(rexx_put,1,0,norest,key,6,\
        (kw(result),kw(string),kw(token),kw(async),kw(io),kw(return)) )
  { # Stackaufbau: string/array, resultp, stringp, tokenp, asyncp, iop, returnp.
    # > string/array: String für Kommando inklusive Argumente oder
    #                 Array von Strings für Funktion und Argumente
    # > resultp: Flag: Antwort merken?
    # > stringp: Flag: ARexx Argument als Befehle oder
    #                  erstes Token als Dateiname verstehen?
    # > tokenp: Flag: Soll ARexx Tokens erzeugen?
    # > asyncp: Flag: Nachricht asynchron bearbeiten?
    # > iop: Flag: E/A Kanäle übernehmen?
    # > returnp: Flag: Auf Antwort warten?!?
    # Es sind nicht alle Kombinationen sinvoll.
    var reg4 uintL vargs; # 1 + Zahl Funktionsargumente
    var reg5 boolean functionp; # Funktions- statt Kommandoaufruf
    if (rexxPort == NULL) { fehler_norexx(); }
    # vorsorglich ein Foreign allozieren:
    pushSTACK(allocate_foreign(NULL));
   {var reg6 object* args_pointer = args_end_pointer;
    # Erstes Argument verarbeiten:
    if (mstringp(STACK_(6+1)))
      { # String
        functionp = FALSE;
        STACK_(6+1) = coerce_ss(STACK_(6+1));
      }
      else
      { functionp = TRUE;
        # sollte (Simple-)Vector sein:
        # evtl.: STACK_(6+1) = coerce_sequence(STACK_(6+1),S(simple_vector));
        if (!m_simple_vector_p(STACK_(6+1)))
          { fehler(
                   DEUTSCH ? "~ muß ein String für Kommandos oder ein Vektor von Strings für eine Funktion sein." :
                   ENGLISH ? "~ must be a string for commands or a vector of strings for a function" :
                   FRANCAIS ? "~ doit être une chaîne pour une commande ou un vecteur de chaînes pour une fonction." :
                   ""
                  );
          }
        vargs = TheSvector(STACK_(6+1))->length;
        if (!(vargs-1 <= MAXRMARG))
          { pushSTACK(STACK_(6+1));
            pushSTACK(fixnum(MAXRMARG));
            pushSTACK(S(rexx_put));
            fehler(
                   DEUTSCH ? "~: ARexx Funktion muß 0 bis ~ Argumente haben: ~" :
                   ENGLISH ? "~: an ARexx function must have 0 to ~ arguments: ~" :
                   FRANCAIS ? "~ : Une fonction ARexx a de 0 à ~ arguments : ~" :
                   ""
                  );
          }
        # Alle Argumentstrings aus dem Vektor auf dem Stack ablegen:
       {var reg3 object* vptr = &STACK_(6+1);
        var reg2 uintL index;
        for (index = 0; index < vargs; index++)
          { var reg1 object arg = TheSvector(*vptr)->data[index];
            if (!stringp(arg))
              { pushSTACK(arg);
                pushSTACK(S(rexx_put));
                fehler(
                       DEUTSCH ? "~: Muß für ARexx ein String sein: ~" :
                       ENGLISH ? "~: must be a string for ARexx: ~" :
                       FRANCAIS ? "~ : Doit être une chaîne pour ARexx : ~" :
                       ""
                      );
              }
            # Argument in Simple-String umwandeln:
            pushSTACK(coerce_ss(arg));
      }}  }
    # Stackaufbau: ... string/vector ..(6).. foreign ..(vargs).. .
    # Ab hier für eine Weile keine GC mehr
    { var reg3 struct RexxMsg * rexxmsg;
      debug_asciz_out("%REXX-PUT: ");
      begin_system_call();
      rexxmsg = CreateRexxMsg(rexxPort,rexxExtension,rexxPortName);
      end_system_call();
      if (!(rexxmsg == NULL))
        # vorerst erfolgreich
        { var reg4 boolean success;
          if (functionp)
            { # ARexx Funktionsaufruf
              debug_asciz_out("function ");
              # Argumente einfüllen:
              { var reg2 uintL i;
                var reg2 object* argptr = args_pointer;
                success = TRUE;
                begin_system_call();
                for (i=0; i<vargs; i++)
                  { var reg1 object s = NEXT(argptr);
                    if ((rexxmsg->rm_Args[i] = CreateArgstring(&TheSstring(s)->data[0],TheSstring(s)->length)) == NULL)
                      { if (i>0) ClearRexxMsg(rexxmsg,i);
                        success = FALSE;
                        break;
                  }   }
                end_system_call();
              }
              set_args_end_pointer(args_pointer); # Stack aufräumen
            }
            else
            { # ARexx Kommando
              debug_asciz_out("command ");
              begin_system_call();
              if (rexxmsg->rm_Args[0] = CreateArgstring(&TheSstring(STACK_(6+1))->data[0],TheSstring(STACK_(6+1))->length))
                { success = TRUE; }
                else
                { success = FALSE; }
              end_system_call();
            }
          # Stackaufbau: ... string/vector ..(6).. foreign.
          if (success)
            # vorerst immer noch erfolgreich
            { rexxmsg->rm_Action = (functionp ? (RXFUNC | (vargs-1)) : RXCOMM);
              # Keyword-Argumente verarbeiten:
              #define is_set(obj)  (!(eq(obj,unbound) || nullp(obj)))
              # :RESULT-Argument:
              if (is_set(STACK_(5+1))) { rexxmsg->rm_Action |= RXFF_RESULT; }
              # :STRING-Argument:
              if (is_set(STACK_(4+1))) { rexxmsg->rm_Action |= RXFF_STRING; }
              # :TOKEN-Argument:
              if (is_set(STACK_(3+1))) { rexxmsg->rm_Action |= RXFF_TOKEN; }
              # :IO-Argument:
              if (!is_set(STACK_(1+1))) { rexxmsg->rm_Action |= RXFF_NOIO; }
              # :RETURN-Argument:
              if (!is_set(STACK_(0+1))) { rexxmsg->rm_Action |= RXFF_NONRET; }
              rexxmsg->rm_Node.mn_Node.ln_Name = RXSDIR;
              { var reg2 boolean asyncp = is_set(STACK_(2+1)); # :ASYNCP-Argument
                var reg1 struct MsgPort* arexxport;
                begin_system_call();
                Forbid();
                arexxport = FindPort(asyncp ? RXADIR : RXSDIR);
                if (!(arexxport==NULL))
                  # Message abschicken:
                  { PutMsg(arexxport,(struct Message *)rexxmsg); }
                  else
                  { success = FALSE; }
                Permit();
                end_system_call();
              }
              #undef is_set
              if (success)
                # erfolgreich -> mitzählen:
                { rexxNeededReplies++;
                  TheForeign(STACK_0) = rexxmsg;
                  debug_hex_out(rexxmsg);
                }
                else
                # nicht erfolgreich -> aufräumen:
                { begin_system_call();
                  if (functionp)
                    { ClearRexxMsg(rexxmsg,vargs); }
                  else
                    { DeleteArgstring(rexxmsg->rm_Args[0]); }
                  end_system_call();
            }   }
          if (success)
            { value1 = STACK_0; } # Wert ist das Foreign zu rexxmsg
            else
            # Nachricht konnte nicht erfolgreich abgeschickt werden, also löschen
            { begin_system_call();
              DeleteRexxMsg(rexxmsg);
              end_system_call();
              value1 = NIL;
        }   }
        else
        { set_args_end_pointer(args_pointer); # Stack aufräumen
          value1 = NIL;
        }
      debug_asciz_out(CRLFstring);
    }
    mv_count=1; skipSTACK(7+1);
  }}

# Warten, bis am Port eine Message eintrifft oder Ctrl-C.
# Ergebnis ist ein Flag, das angibt, ob eine Message eintraf.
  local boolean rexx_wait (void);
  local boolean rexx_wait()
    { start:
      begin_system_call();
     {var LONG wait_erg = Wait(rexxPortBit | SIGBREAKF_CTRL_C);
      end_system_call();
      #if 0 # Packet-Handling (spätere asynchrone DOS-Packet-Bearbeitung) ??
      if (wait_erg & ioPortBit)
        { flush_io_queue(); }
      #endif
      if (wait_erg & SIGBREAKF_CTRL_C)
        { # Bearbeitung einer evtl. Message verschieben:
          if (wait_erg & rexxPortBit)
            { begin_system_call(); SetSignal(wait_erg,rexxPortBit); end_system_call(); }
          # Ctrl-C behandeln:
          pushSTACK(S(rexx_wait_input)); tast_break();
          goto start;
        }
        else
        { if (wait_erg & rexxPortBit)
            return TRUE;
            else
            return FALSE; # eigentlich nicht möglich
        }
    }}

# (SYSTEM::REXX-WAIT-INPUT) wartet bis am AREXX-Port etwas anliegt,
# und liefert dann T.
LISPFUNN(rexx_wait_input,0)
  { if (!(rexxPort == NULL))
      { if (rexx_wait())
          { value1 = T; mv_count=1; return; }
      }
    value1 = NIL; mv_count=1;
  }

# Flag, ob sich das ARexx-Interface gerade in der Endphase befindet und
# deswegen keine neuen Nachrichten entgegennimmt:
  local boolean rexxShutdown = TRUE;

# Empfängt ARexx Nachrichten.
# Liefert eine Liste (MsgId ...) oder T, wenn eine Nachricht empfangen wurde.
# Falls rexxShutdown gesetzt ist, werden keine neuen Nachrichten, nur noch
# Antworten, angenommen.
# Kann GC auslösen, falls nicht im rexxShutdown Modus.
  local object rexx_getmsg(void);
  local object rexx_getmsg()
    { if (rexxPort == NULL)
        { return NIL; }
        else
        { var reg1 struct RexxMsg * rexxmsg;
          handle_lost_resources();
          # Resource-tracking für einkommende Nachrichten
          # (Benutzt eine globale Variable O(rexx_prefetch_inmsg),
          # um nicht jedes Mal ein neues Cons erzeugen zu müssen.)
          if (matomp(O(rexx_prefetch_inmsg)))
            { pushSTACK(allocate_foreign(NULL));
             {var reg1 object new_cons = allocate_cons();
              Car(new_cons) = popSTACK();
              O(rexx_prefetch_inmsg) = new_cons;
            }}
          # O(rexx_prefetch_inmsg) ist nun garantiert ein brauchbares Cons.
          # Bereich gegen GC geschützt.
          begin_system_call();
          rexxmsg = (struct RexxMsg *)GetMsg(rexxPort);
          end_system_call();
          if (rexxmsg == NULL) # keine Nachricht vorhanden?
            { return NIL; }
            else
            { debug_asciz_out("rexx_getmsg: ");
              debug_hex_out(rexxmsg->rm_Action);
              if (rexxmsg->rm_Node.mn_Node.ln_Type == NT_REPLYMSG)
                # Antwort auf eine von uns geschickte Message
                { var reg3 LONG result1 = rexxmsg->rm_Result1;
                  begin_system_call();
                  if (rexxmsg->rm_Action & RXCOMM)
                    { DeleteArgstring(rexxmsg->rm_Args[0]); }
                    else
                    { ClearRexxMsg(rexxmsg,1+(rexxmsg->rm_Action & RXARGMASK)); }
                  if ((rexxmsg->rm_Action & RXFF_RESULT)
                      && (rexxmsg->rm_Result1 == 0)
                      && rexxmsg->rm_Result2
                      && !rexxShutdown
                     )
                    { # DeleteArgstring(rexxmsg->rm_Result2); kommt später
                      rexxLostArgstr = (UBYTE*)(rexxmsg->rm_Result2);
                    }
                  DeleteRexxMsg(rexxmsg);
                  end_system_call();
                  rexxNeededReplies--;
                  debug_asciz_out(" a reply ");
                  # ab hier GC wieder möglich
                  if (rexxShutdown)
                    { handle_lost_resources(); return T; }
                    else
                    # Ergebnis ist eine 2- oder 3-elementige Liste (Msg-ID RC [RESULT]),
                    { pushSTACK(allocate_foreign(rexxmsg));
                      pushSTACK(L_to_I(result1));
                      if (rexxLostArgstr)
                        { pushSTACK(make_string(rexxLostArgstr,LengthArgstring(rexxLostArgstr)));
                          handle_lost_resources();
                          return listof(3);
                        }
                        else
                        { /* handle_lost_resources(); */ # hier unnötig
                          return listof(2);
                    }   }
                }
                else
                { rexxmsg->rm_Result2 = 0;
                  debug_asciz_out(" incoming is " CRLFstring);
                  debug_asciz_out(rexxmsg->rm_Args[0]);
                  # Eingehender Befehl
                  if (rexxShutdown)
                    { # Schluß, nichts läuft mehr
                      rexxmsg->rm_Result1 = RXERRORIMGONE;
                      begin_system_call();
                      ReplyMsg((struct Message *)rexxmsg);
                      end_system_call();
                      return T;
                    }
                    else
                    { var reg2 object new_cons = O(rexx_prefetch_inmsg);
                      # Resource-tracking, bis dahin keine GC.
                      TheForeign(Car(new_cons)) = rexxmsg;
                      Cdr(new_cons) = O(rexx_inmsg_list);
                      O(rexx_inmsg_list) = new_cons;
                      O(rexx_prefetch_inmsg) = NIL;
                      # Resource-tracking beendet, ab hier wieder GC möglich
                      # Ergebnis ist zweielementige Liste (Msg-ID "Msg-string")
                      pushSTACK(Car(new_cons));
                      pushSTACK(make_string(rexxmsg->rm_Args[0],LengthArgstring(rexxmsg->rm_Args[0])));
                      return listof(2);
                    }
                }
            }
        }
    }

# (SYSTEM::%REXX-GET) empfängt eine Nachricht und liefert sie im
# Format (MsgId ...). Ergebnis NIL falls keine Nachricht vorliegt.
LISPFUNN(rexx_get,0)
  { if (rexxPort == NULL) { fehler_norexx(); }
    value1 = rexx_getmsg(); mv_count=1;
  }

# Antwortet auf eine eingegangene Nachricht.
# > foreign: Foreign mit der Message-Adresse
# > rc, result,result_length: Return-Code und Ergebnis-String
  local void rexx_replymsg (object foreign, LONG rc, UBYTE* result, ULONG result_length);
  local void rexx_replymsg(foreign,rc,result,result_length)
    var reg4 object foreign;
    var reg2 LONG rc;
    var reg3 UBYTE* result;
    var reg5 ULONG result_length;
    { var reg1 struct RexxMsg* rexxmsg = TheForeign(foreign);
      debug_asciz_out("rexx_replymsg: ");
      debug_hex_out(rexxmsg);
      rexxmsg->rm_Result1 = rc;
      begin_system_call();
      rexxmsg->rm_Result2 = (ULONG)
        (((rc == 0) && (rexxmsg->rm_Action & RXFF_RESULT))
         ? CreateArgstring(result,result_length)
         : NULL
        );
      ReplyMsg((struct Message *)rexxmsg);
      end_system_call();
      # Die Message foreign ist nun beantwortet.
      O(rexx_inmsg_list) = deleteq(O(rexx_inmsg_list),foreign);
      debug_asciz_out(CRLFstring);
    }

# (SYS::%REXX-REPLY message-id return-code return-string)
# antwortet auf eine Message.
LISPFUNN(rexx_reply,3)
  { # Stackaufbau: ..., message-id, return-code, return-string.
    if (rexxPort == NULL) { fehler_norexx(); }
    # Argumente überprüfen:
    # return-code sollte ein Fixnum sein:
    if (!mfixnump(STACK_1))
      { pushSTACK(STACK_1);
        pushSTACK(S(rexx_reply));
        fehler(
               DEUTSCH ? "~: Kein Fixnum: ~" :
               ENGLISH ? "~: Not a Fixnum: ~" :
               FRANCAIS ? "~ : ~ n'est pas de type FIXNUM" :
               ""
              );
      }
    # return-string sollte ein String oder NIL sein:
    if (mstringp(STACK_0))
      { STACK_0 = coerce_ss(STACK_0); } # in Simple-String umwandeln
    # message-id sollte ein Foreign sein:
   {var reg3 object foreign;
    if (!(foreignp(STACK_2) && !nullp(foreign = find_inmsg(TheForeign(STACK_2)))))
      { pushSTACK(STACK_2);
        pushSTACK(S(rexx_reply));
        fehler(
               DEUTSCH ? "~: Keine eingehende Rexx Nachricht: ~" :
               ENGLISH ? "~: Not an incoming Rexx message: ~" :
               FRANCAIS ? "~ : ~ n'est pas un message Rexx entrant" :
               ""
              );
      }
    # Beantworten:
    { var reg2 object retcode = STACK_1;
      var reg4 LONG result1 = fixnum_to_L(retcode);
      var reg1 object retstring = STACK_0;
      if (simple_string_p(retstring))
        { rexx_replymsg(foreign,result1,&TheSstring(retstring)->data[0],TheSstring(retstring)->length); }
        else
        { rexx_replymsg(foreign,result1,NULL,0); }
    }
    skipSTACK(3);
    value1 = NIL; mv_count=0;
  }}

# ARexx 'library base' pointer:
# (Muß global sichtbar sein und diesen Namen tragen, damit's der Linker findet!)
  global struct Library /* struct RxsLib */ * RexxSysBase = NULL;

# Initialisiert das REXX-Interface.
# < ergebnis: Flag, ob erfolgreich initialisiert.
# Kann mehrfach aufgerufen werden.
  global boolean init_rexx(void)
    { if (rexxPort == NULL) # noch was zu tun?
        { if (RexxSysBase == NULL)
            { begin_system_call();
              RexxSysBase = OpenLibrary(RXSNAME,0L);
              end_system_call();
              if (RexxSysBase == NULL) { return FALSE; }
            }
         {var reg1 uintC nr = 1; # wir probieren verschiedene Ports
          loop
            { if (!(rexxPort == NULL)) break;
             {var reg2 boolean existent;
              rexxPortName[NRPOSITION] = '0' + nr;
              { begin_system_call();
                Forbid();
                if (FindPort(rexxPortName) == NULL)
                  # Port existiert noch nicht, wir machen einen (öffentlichen):
                  { rexxPort = CreatePort(rexxPortName,0L);
                    existent = FALSE;
                  }
                  else
                  { existent = TRUE; }
                Permit();
                end_system_call();
              }
              if (!existent)
                # Wir haben's wenigstens probiert...
                { if (rexxPort == NULL) { return FALSE; }
                  rexxPortBit = bit(rexxPort->mp_SigBit);
                  rexxNeededReplies = 0;
                  rexxShutdown = FALSE;
                  break;
                }
              # Wir versuchen es mit einem anderem Namen erneut.
              nr++; if (nr==10) { return FALSE; }
            }}
        }}
      return TRUE;
    }

# Schließt das REXX-Interface.
# Kann nur einmal aufgerufen werden.
  global void close_rexx(void)
    { rexxShutdown = TRUE;
      debug_asciz_out("close_rexx: ");
      debug_dez_out(rexxNeededReplies); debug_asciz_out(" messages waiting." CRLFstring);
      if (!(rexxPort == NULL))
        { # Port unbekannt machen (abmelden):
          begin_system_call();
          RemPort(rexxPort);
          end_system_call();
          handle_lost_resources();
          # Ausstehende Nachrichten mit Fehler zurückschicken:
          while (mconsp(O(rexx_inmsg_list)))
            { rexx_replymsg(Car(O(rexx_inmsg_list)),RXERRORIMGONE,NULL,0L); }
          # Eingegangene Nachrichten mit Fehler zurückschicken:
          loop
            { until (nullp(rexx_getmsg())) { /* loop until empty */ }
              if (rexxNeededReplies == 0) break;
              begin_system_call();
              Wait(rexxPortBit);
              end_system_call();
              debug_asciz_out("Looping" CRLFstring);
            }
          begin_system_call();
          rexxPort->mp_Node.ln_Name = NULL;
          DeletePort(rexxPort);
          end_system_call();
          rexxPort = NULL;
          rexxPortBit = 0;
        }
      if (!(RexxSysBase == NULL))
        { begin_system_call();
          CloseLibrary(RexxSysBase);
          end_system_call();
          RexxSysBase = NULL;
        }
    }

#endif # AMIGAOS

