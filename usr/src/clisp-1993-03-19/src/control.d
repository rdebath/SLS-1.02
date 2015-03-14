# Special-Forms, Kontrollstrukturen, Evaluator-Nahes für CLISP
# Bruno Haible 17.3.1993

#include "lispbibl.c"


LISPFUNN(exit,0)
# (SYSTEM::%EXIT) verläßt das System
  { quit(); }

# Fehlermeldung, wenn ein Objekt kein Symbol ist.
# fehler_symbol(obj);
  local nonreturning void fehler_symbol (object obj);
  local nonreturning void fehler_symbol(obj)
    var reg2 object obj;
    { var reg1 object aufrufer = subr_self;
      aufrufer = (subrp(aufrufer) ? TheSubr(aufrufer)->name : TheFsubr(aufrufer)->name);
      fehler_kein_symbol(aufrufer,obj);
    }

LISPSPECFORM(eval_when, 1,0,body)
# (EVAL-WHEN ({situation}) {form}), CLTL S. 69
  { var reg1 object situations = STACK_1; # Liste der Situationen
    # Symbol EVAL darin suchen:
    while (consp(situations))
      { if (eq(Car(situations),S(eval))) # Symbol EVAL gefundn?
          goto found;
        situations = Cdr(situations);
      }
    # Symbol EVAL nicht gefunden
    value1 = NIL; mv_count=1; # Wert NIL
    skipSTACK(2);
    return;
    found: # Symbol EVAL gefunden
   {var reg2 object body = popSTACK();
    skipSTACK(1);
    implicit_progn(body,NIL); # body auswerten
    return;
  }}

LISPSPECFORM(quote, 1,0,nobody)
# (QUOTE object) == 'object, CLTL S. 86
  { value1 = popSTACK(); mv_count=1; } # Argument als Wert

LISPSPECFORM(function, 1,1,nobody)
# (FUNCTION funname), CLTL. S. 87
# entweder (FUNCTION symbol)
#     oder (FUNCTION (LAMBDA . lambdabody))
#     oder (FUNCTION name (LAMBDA . lambdabody))
  { var reg1 object funname; # Funktionsname (Symbol oder Lambdabody)
    var reg2 object name; # Name (Symbol)
    if (eq(STACK_0,unbound))
      # 1 Argument
      { funname = STACK_1;
        if (symbolp(funname))
          # (FUNCTION symbol) - Syntax
          { # Symbol im aktuellen Funktions-Environment suchen:
            var reg3 object fun = sym_function(funname,aktenv.fun_env);
            # SUBR oder Closure zurückgeben, sonst Fehler:
            if (!(subrp(fun) || closurep(fun)))
              { pushSTACK(funname);
                pushSTACK(S(function));
                fehler(
                       DEUTSCH ? "~: Die Funktion ~ ist nicht definiert." :
                       ENGLISH ? "~: undefined function ~" :
                       FRANCAIS ? "~: La fonction ~ n'est pas définie." :
                       ""
                      );
              }
            value1 = fun; mv_count=1; skipSTACK(2); return;
          }
        name = S(Klambda); # :LAMBDA als Default-Name
      }
      else
      # 2 Argumente
      { name = STACK_1; # 1. Argument
        if (!symbolp(name)) # sollte ein Symbol sein
          { # name in STACK_1
            STACK_0 = S(function);
            fehler(
                   DEUTSCH ? "~: Funktionsname ~ ist kein Symbol." :
                   ENGLISH ? "~: function name ~ is not a symbol" :
                   FRANCAIS ? "~: Le nom de fonction ~ n'est pas un symbôle." :
                   ""
                  );
          }
        funname = STACK_0; # 2. Argument, hoffentlich Lambdaausdruck
      }
    if (!(consp(funname) && eq(Car(funname),S(lambda)))) # Cons (LAMBDA . ...) ?
      { pushSTACK(funname);
        pushSTACK(S(function));
        fehler(
               DEUTSCH ? "~: ~ ist keine Funktionsbezeichnung." :
               ENGLISH ? "~: ~ is not a function name" :
               FRANCAIS ? "~: ~ n'est pas un nom de fonction." :
               ""
              );
      }
    # Lambdaausdruck
    # im aktuellen Environment in eine Closure umwandeln:
    value1 = get_closure(Cdr(funname),name,&aktenv); mv_count=1;
    skipSTACK(2); return;
  }

LISPFUNN(symbol_value,1)
# (SYMBOL-VALUE symbol), CLTL S. 90
  { var reg1 object symbol = popSTACK();
    if (!symbolp(symbol)) { fehler_symbol(symbol); }
   {var reg2 object val = Symbol_value(symbol);
    if (eq(val,unbound))
      { pushSTACK(symbol);
        pushSTACK(S(symbol_value));
        fehler(
               DEUTSCH ? "~: ~ hat keinen dynamischen Wert." :
               ENGLISH ? "~: ~ has no dynamic value" :
               FRANCAIS ? "~: ~ n'a pas de valeur dynamique." :
               ""
              );
      }
    value1 = val; mv_count=1;
  }}

LISPFUNN(symbol_function,1)
# (SYMBOL-FUNCTION symbol), CLTL S. 90
  { var reg1 object symbol = popSTACK();
    if (!symbolp(symbol)) { fehler_symbol(symbol); }
   {var reg2 object val = Symbol_function(symbol);
    if (eq(val,unbound))
      { pushSTACK(symbol);
        pushSTACK(S(symbol_value));
        fehler(
               DEUTSCH ? "~: ~ hat keine globale Funktionsdefinition." :
               ENGLISH ? "~: ~ has no global function definition" :
               FRANCAIS ? "~: ~ n'as pas de définition de fonction globale." :
               ""
              );
      }
    value1 = val; mv_count=1;
  }}

LISPFUNN(boundp,1)
# (BOUNDP symbol), CLTL S. 90
  { var reg1 object symbol = popSTACK();
    if (!symbolp(symbol)) { fehler_symbol(symbol); }
    value1 = (eq(Symbol_value(symbol),unbound) ? NIL : T); mv_count=1;
  }

LISPFUNN(fboundp,1)
# (FBOUNDP symbol), CLTL S. 90
  { var reg1 object symbol = popSTACK();
    if (!symbolp(symbol)) { fehler_symbol(symbol); }
    value1 = (eq(Symbol_function(symbol),unbound) ? NIL : T); mv_count=1;
  }

LISPFUNN(special_form_p,1)
# (SPECIAL-FORM-P symbol), CLTL S. 91
  { var reg1 object symbol = popSTACK();
    if (!symbolp(symbol)) { fehler_symbol(symbol); }
    value1 = (mfsubrp(Symbol_function(symbol)) ? T : NIL); mv_count=1;
  }

# Fehlermeldung bei Zuweisung, wenn ein Symbol eine Konstante ist.
# (Einer Konstante kann nicht zugewiesen werden.)
# fehler_symbol_constant(caller,symbol);
# > caller: Aufrufer (ein Symbol)
# > symbol: konstantes Symbol
  local nonreturning void fehler_symbol_constant (object caller, object symbol);
  local nonreturning void fehler_symbol_constant(caller,symbol)
    var reg2 object caller;
    var reg1 object symbol;
    { pushSTACK(symbol);
      pushSTACK(caller);
      fehler(
             DEUTSCH ? "~: Der Konstanten ~ kann kein Wert zugewiesen werden." :
             ENGLISH ? "~: the value of the constant ~ may not be altered" :
             FRANCAIS ? "~: Aucune valeur ne peut être assignée à la constante ~." :
             ""
            );
    }

LISPSPECFORM(setq, 0,0,body)
# (SETQ {var form}), CLTL S. 91
  { var reg1 object body = STACK_0;
    if (consp(body))
      { do { var reg2 object symbol = Car(body); # Variable
             if (!symbolp(symbol)) { fehler_kein_symbol(S(setq),symbol); }
             if (constantp(TheSymbol(symbol)))
               { fehler_symbol_constant(S(setq),symbol); }
             body = Cdr(body);
             if (atomp(body))
               { if (!nullp(body)) goto fehler_dotted;
                 # Der ganze Body noch in STACK_0.
                 pushSTACK(S(setq));
                 fehler(
                        DEUTSCH ? "~ mit ungerader Anzahl von Argumenten: ~" :
                        ENGLISH ? "~ called with odd number of arguments: ~" :
                        FRANCAIS ? "~ appelé avec un nombre impair d'arguments : ~" :
                        ""
                       );
               }
             pushSTACK(Cdr(body)); # Restliste retten
             pushSTACK(symbol); # Symbol retten
             eval(Car(body)); # nächste Form auswerten
             symbol = popSTACK();
             setq(symbol,value1); # Zuweisung durchführen
             body = popSTACK();
           }
           while (consp(body));
        # value1 ist noch das letzte Auswertungs-Ergebnis.
      }
      else
      { value1 = NIL; } # Defaultwert bei (SETQ)
    mv_count=1;
    # body ist zu Ende.
    if (!nullp(body))
      { fehler_dotted: # Der ganze Body noch in STACK_0.
        pushSTACK(S(setq));
        fehler(
               DEUTSCH ? "Dotted List als Argumentliste an ~ : ~" :
               ENGLISH ? "dotted list given to ~ : ~" :
               FRANCAIS ? "Liste pointée d'arguments fournie à ~ : ~" :
               ""
              );
      }
    skipSTACK(1);
  }

LISPSPECFORM(psetq, 0,0,body)
# (PSETQ {var form}), CLTL S. 92
  { var reg1 object body = popSTACK();
    var reg4 uintL body_length = llength(body);
    if (!((body_length%2)==0))
      { pushSTACK(body);
        pushSTACK(S(psetq));
        fehler(
               DEUTSCH ? "~: Anzahl der Parameter ist ungerade: ~" :
               ENGLISH ? "~: odd number of arguments: ~" :
               FRANCAIS ? "~: Nombre impair d'arguments : ~" :
               ""
              );
      }
    body_length = body_length/2; # Anzahl der Paare (var form)
    get_space_on_STACK(body_length*2*sizeof(object)); # Platz im STACK belegen
    { var reg3 uintL count;
      dotimesL(count,body_length,
        { var reg2 object symbol = Car(body);
          if (!symbolp(symbol)) { fehler_kein_symbol(S(psetq),symbol); }
          if (constantp(TheSymbol(symbol)))
            { fehler_symbol_constant(S(psetq),symbol); }
          pushSTACK(symbol); # Variable auf den Stack
          body = Cdr(body);
          pushSTACK(Cdr(body)); # Restliche Liste auf den Stack
          eval(Car(body)); # nächste Form auswerten
          body = STACK_0;
          STACK_0 = value1; # ihr Ergebnis in den Stack
        });
    }
    { var reg3 uintL count;
      dotimesL(count,body_length,
        { var reg2 object val = popSTACK(); # Wert
          var reg1 object sym = popSTACK(); # Symbol
          setq(sym,val); # Zuweisung durchführen
        });
    }
    value1 = NIL; mv_count=1; # Wert NIL
  }

LISPFUNN(set,2)
# (SET symbol value), CLTL S. 92
  { var reg1 object symbol = STACK_1;
    if (!symbolp(symbol)) { fehler_symbol(symbol); }
    if (constantp(TheSymbol(symbol)))
      { fehler_symbol_constant(S(set),symbol); }
    value1 = Symbol_value(symbol) = STACK_0; mv_count=1;
    skipSTACK(2);
  }

LISPFUNN(makunbound,1)
# (MAKUNBOUND symbol), CLTL S. 92
  { var reg1 object symbol = popSTACK();
    if (!symbolp(symbol)) { fehler_symbol(symbol); }
    if (constantp(TheSymbol(symbol)))
      { pushSTACK(symbol);
        pushSTACK(S(makunbound));
        fehler(
               DEUTSCH ? "~: Der Wert der Konstanten ~ muß erhalten bleiben." :
               ENGLISH ? "~: the value of the constant ~ must not be removed" :
               FRANCAIS ? "~: La valeur de la constante ~ doit être conservée." :
               ""
              );
      }
    Symbol_value(symbol) = unbound;
    value1 = symbol; mv_count=1;
  }

LISPFUNN(fmakunbound,1)
# (FMAKUNBOUND symbol), CLTL S. 92
  { var reg1 object symbol = popSTACK();
    if (!symbolp(symbol)) { fehler_symbol(symbol); }
    if (mfsubrp(Symbol_function(symbol)))
      { pushSTACK(symbol);
        pushSTACK(S(makunbound));
        fehler(
               DEUTSCH ? "~: Definition der Spezialform ~ darf nicht gelöscht werden." :
               ENGLISH ? "~: the special form definition of ~ must not be removed" :
               FRANCAIS ? "~: La définition de la forme spéciale ~ doit être conservée." :
               ""
              );
      }
    Symbol_function(symbol) = unbound;
    value1 = symbol; mv_count=1;
  }

LISPFUN(apply,2,0,rest,nokey,0,NIL)
# (APPLY function {arg} arglist), CLTL S. 107
  { BEFORE(rest_args_pointer);
    apply(Before(rest_args_pointer), # function
          argcount, # Anzahl der {arg} auf dem Stack
          popSTACK() # arglist
         );
    skipSTACK(1); # function aus dem Stack entfernen
  }

LISPFUN(pfuncall,1,0,rest,nokey,0,NIL)
# (SYS::%FUNCALL function {arg})
  { funcall(Before(rest_args_pointer),argcount); skipSTACK(1); }

LISPFUN(funcall,1,0,rest,nokey,0,NIL)
# (FUNCALL function {arg}), CLTL S. 108
  { funcall(Before(rest_args_pointer),argcount); skipSTACK(1); }

LISPSPECFORM(progn, 0,0,body)
# (PROGN {form}), CLTL S. 109
  { implicit_progn(popSTACK(),NIL); }

# Macro: Wertet die Formen einer Formenliste aus.
# implicit_prog();
# > -(STACK): Formenliste
# erhöht STACK um 1
# kann GC auslösen
  #define implicit_prog()  \
    { while (mconsp(STACK_0))                         \
        { var reg1 object forms = STACK_0;            \
          STACK_0 = Cdr(forms);                       \
          eval(Car(forms)); # nächste Form evaluieren \
        }                                             \
      skipSTACK(1);                                   \
    }

LISPSPECFORM(prog1, 1,0,body)
# (PROG1 form1 {form}), CLTL S. 109
  { STACK_1 = (eval(STACK_1),value1); # form1 evaluieren, Wert retten
    implicit_prog();
    value1 = popSTACK(); mv_count=1; # geretteten Wert zurückgeben
  }

LISPSPECFORM(prog2, 2,0,body)
# (PROG2 form1 form2 {form}), CLTL S. 109
  { eval(STACK_2); # form1 evaluieren
    eval(STACK_1); STACK_2 = value1; # form2 evaluieren, Wert retten
    STACK_1 = STACK_0; skipSTACK(1);
    implicit_prog();
    value1 = popSTACK(); mv_count=1; # geretteten Wert zurückgeben
  }

# Fehlermeldung wegen nicht erlaubter Docstrings
# fehler_docstring(caller,body);
# > caller: Aufrufer, ein Symbol
# > body: gesamter Body
  local nonreturning void fehler_docstring (object caller, object body);
  local nonreturning void fehler_docstring(caller,body)
    var reg1 object caller;
    var reg2 object body;
    { pushSTACK(body);
      pushSTACK(caller);
      fehler(
             DEUTSCH ? "~: Doc-Strings sind nicht hier erlaubt: ~" :
             ENGLISH ? "~: doc-strings are not allowed here: ~" :
             FRANCAIS ? "~: Une chaîne de documentation n'est pas permise ici : ~" :
             ""
            );
    }

# UP für LET, LET*, MULTIPLE-VALUE-BIND:
# Kompiliert die aktuelle Form und führt sie in kompiliertem Zustand aus.
# compile_form()
# > im STACK: EVAL-Frame mit der Form
# < mv_count/mv_space: Werte
# kann GC auslösen
  local Values compile_eval_form (void);
  local Values compile_eval_form()
    { # (SYS::COMPILE-FORM form venv fenv benv genv denv) ausführen:
      # Die ganze Form aus dem EVAL-Frame im Stack holen:
      pushSTACK(STACK_(frame_form)); # als 1. Argument
     {var reg1 environment* stack_env = nest_aktenv(); # aktuelles Environment nesten, auf den STACK legen
      #if !defined(STACK_UP)
      var environment my_env;
      my_env = *stack_env; # und hierher übertragen
      skipSTACK(5); # und wieder vom STACK nehmen
      pushSTACK(my_env.var_env); # 2. Argument
      pushSTACK(my_env.fun_env); # 3. Argument
      pushSTACK(my_env.block_env); # 4. Argument
      pushSTACK(my_env.go_env); # 5. Argument
      pushSTACK(my_env.decl_env); # 6. Argument
      #endif
      funcall(S(compile_form),6);
     }# Die sich ergebende compilierte Closure mit 0 Argumenten aufrufen:
      funcall(value1,0);
    }

# UP für LET, LET*, MULTIPLE-VALUE-BIND:
# Analysiert die Variablen und Deklarationen, baut einen Variablenbindungs-
# Frame auf und erweitert VENV und evtl. auch DENV durch einen Frame.
# make_variable_frame(caller,varspecs,&bind_ptr,&bind_count)
# > object caller: Aufrufer, ein Symbol
# > object varspecs: Liste von Variablen-Specifiern
# > object value2: Liste von Declaration-Specifiern
# > object value1: Liste ({form}) von Formen
# < Stackaufbau: Variablenbindungsframe, Env-Bindungs-Frame, ({form}).
# < object* bind_ptr: Pointer über die erste "richtige" Bindung.
# < uintC bind_count: Anzahl der "richtigen" Bindungen.
# verändert STACK
# kann GC auslösen
  local void make_variable_frame (object caller, object varspecs, object** bind_ptr_, uintC* bind_count_);
  local void make_variable_frame(caller,varspecs,bind_ptr_,bind_count_)
    var reg10 object caller;
    var reg10 object varspecs;
    var reg10 object** bind_ptr_;
    var reg10 uintC* bind_count_;
    { var reg10 object declarations = value2;
      # Variablenbindungs-Frame aufbauen:
      { var reg9 object* top_of_frame = STACK; # Pointer übern Frame
        # zuerst die Special-deklarierten Variablen aus declarations
        # im Stack ablegen:
        var reg9 object* spec_pointer = args_end_pointer;
        var reg8 uintL spec_anz = 0; # Anzahl der SPECIAL-Referenzen
        { var reg3 object declspecs = declarations;
          while (consp(declspecs))
            { var reg1 object declspec = Car(declspecs); # nächse Deklaration
              if (consp(declspec) && eq(Car(declspec),S(special))) # (SPECIAL ...) ?
                { while (consp( declspec = Cdr(declspec) ))
                    { var reg2 object declsym = Car(declspec); # nächstes Special-deklariertes Item
                      if (!symbolp(declsym)) # sollte ein Symbol sein
                        { pushSTACK(declsym);
                          pushSTACK(caller);
                          fehler(
                                 DEUTSCH ? "~: ~ ist kein Symbol, wurde aber als SPECIAL deklariert." :
                                 ENGLISH ? "~: ~ is not a symbol, but was declared SPECIAL" :
                                 FRANCAIS ? "~: ~ n'est pas un symbôle mais fut déclaré SPECIAL." :
                                 ""
                                );
                        }
                      # Special-deklariertes Symbol im Stack ablegen:
                      pushSTACK(specdecl); # SPECDECL als "Wert"
                      pushSTACK_symbolwithflags(declsym,wbit(active_bit_o)); # Symbol aktiv
                      check_STACK();
                      spec_anz++;
                }   }
              declspecs = Cdr(declspecs);
        }   }
        *bind_ptr_ = args_end_pointer; # Pointer über erste "richtige" Bindung
        # Dann die "richtigen" Variablenbindungen (jeweils die Variable
        # und ihren unausgewerteten Init) im Stack ablegen:
       {var reg7 uintL var_anz = 0; # Anzahl der Variablenbindungen
        { while (consp(varspecs))
            { var reg4 object varspec = Car(varspecs); # nächstes varspec
              # in Symbol und Init aufspalten:
              var reg5 object symbol;
              var reg6 object init;
              if (symbolp(varspec)) # Symbol ?
                { symbol = varspec; init = unbound; }
              elif # zweielementige Liste, mit Symbol als CAR ?
                   (consp(varspec)
                    && (symbol = Car(varspec), varspec = Cdr(varspec),
                        symbolp(symbol) && consp(varspec) && nullp(Cdr(varspec))
                   )   )
                { init = Car(varspec); }
              else
                { pushSTACK(Car(varspecs));
                  pushSTACK(caller);
                  fehler(
                         DEUTSCH ? "~: ~ ist keine korrekte Variablenspezifikation." :
                         ENGLISH ? "~: illegal variable specification ~" :
                         FRANCAIS ? "~: ~ n'est pas une spécification de variable licite." :
                         ""
                        );
                }
              pushSTACK(init); # Init und
              pushSTACK_symbolwithflags(symbol,0); # Variable ablegen
              check_STACK();
              # feststellen, ob statische oder dynamische Bindung:
              if (!special_var_p(TheSymbol(symbol)))
                { # Variable unter den Special-deklarierten?
                  #ifdef NO_symbolflags
                  var reg1 object* ptr = spec_pointer;
                  var reg2 uintL count;
                  dotimesL(count,spec_anz,
                    { NEXT(ptr);
                      if (eq(NEXT(ptr),symbol))
                        { if (eq(NEXT(ptr),fixnum(bit(active_bit)))) goto dynamic; }
                        else
                        { NEXT(ptr); }
                    });
                  #else
                  var reg3 object to_compare = (object)((oint)symbol | wbit(active_bit_o));
                  var reg1 object* ptr = spec_pointer;
                  var reg2 uintL count;
                  dotimesL(count,spec_anz,
                    { NEXT(ptr);
                      if (eq(NEXT(ptr),to_compare))
                        goto dynamic;
                    });
                  #endif
                  # Nein -> statische Bindung
                }
                else
                { dynamic: # dynamisch binden
                  *(oint*)(&STACK_0) |= wbit(dynam_bit_o);
                }
              varspecs = Cdr(varspecs);
              var_anz++;
        }   }
        *bind_count_ = var_anz;
        var_anz += spec_anz; # Gesamtzahl Symbol/Wert-Paare
        if (var_anz > (uintC)(~(uintC)0)) # paßt es in ein uintC ?
          { pushSTACK(caller);
            fehler(
                   DEUTSCH ? "~: Zuviele Variablen und/oder Deklarationen." :
                   ENGLISH ? "~: too many variables and/or declarations" :
                   FRANCAIS ? "~: Trop de déclarations et/ou de variables." :
                   ""
                  );
          }
        pushSTACK(aktenv.var_env); # aktuelles VAR_ENV als NEXT_ENV
        pushSTACK((object)var_anz); # Anzahl Bindungen
        finish_frame(VAR);
      }}
      # Der Variablenbindungsframe ist jetzt fertig.
     {var reg5 object* var_frame_ptr = STACK; # Pointer auf Variablenbindungs-Frame
      # VENV-Bindungsframe aufbauen:
      { var reg4 object* top_of_frame = STACK; # Pointer übern Frame
        # Zuerst DENV um die nötigen declspecs erweitern:
        var reg3 object denv = aktenv.decl_env;
        pushSTACK(value1); # ({form}) retten
        pushSTACK(declarations);
        while (mconsp(STACK_0))
          { var reg2 object declspecs = STACK_0;
            STACK_0 = Cdr(declspecs);
           {var reg1 object declspec = Car(declspecs); # nächstes Declspec
            if (consp(declspec)) # sollte ein Cons sein
              { if (!eq(Car(declspec),S(special))) # (SPECIAL ...) haben wir schon behandelt
                  { denv = augment_decl_env(declspec,denv); } # alles andere behandeln
          }}  }
        skipSTACK(1);
       {var reg1 object forms = popSTACK();
        # Nun den Frame bauen:
        if (eq(denv,aktenv.decl_env))
          { pushSTACK(aktenv.var_env);
            finish_frame(ENV1V);
          }
          else
          { pushSTACK(aktenv.decl_env);
            pushSTACK(aktenv.var_env);
            finish_frame(ENV2VD);
            aktenv.decl_env = denv;
          }
        # VENV-Bindungsframe ist fertig.
        aktenv.var_env = make_framepointer(var_frame_ptr); # Pointer auf Variablenbindungsframe
        pushSTACK(forms);
    }}}}

LISPSPECFORM(let, 1,0,body)
# (LET ({varspec}) {decl} {form}), CLTL S. 110
  { # {decl} {form} trennen:
    var reg6 boolean to_compile = parse_dd(STACK_0,aktenv.fun_env);
    # bitte kein Docstring:
    if (!nullp(value3)) { fehler_docstring(S(let),STACK_0); }
    if (to_compile) # Deklaration (COMPILE) ?
      # ja -> Form kompilieren:
      { skipSTACK(2); return_Values compile_eval_form(); }
      else
      { skipSTACK(1);
        # Variablenbindungsframe aufbauen, VAR_ENV erweitern:
       {var object* bind_ptr;
        var uintC bind_count;
        make_variable_frame(S(let),popSTACK(),&bind_ptr,&bind_count);
        # Dann die Initialisierungsformen auswerten:
        { var reg3 object* frame_pointer = bind_ptr;
          var reg4 uintC count;
          dotimesC(count,bind_count,
            { var reg1 object* initptr = &NEXT(frame_pointer);
              var reg2 object init = *initptr; # nächstes Init
              *initptr = (eq(init,unbound) ? NIL : (eval(init),value1)); # auswerten, NIL als Default
              frame_pointer skipSTACKop -(varframe_binding_size-1);
            });
        }
        # Dann die Bindungen aktivieren:
        { var reg4 object* frame_pointer = bind_ptr;
          var reg5 uintC count;
          dotimesC(count,bind_count,
            { frame_pointer skipSTACKop -varframe_binding_size;
             {var reg1 object* markptr = &Before(frame_pointer);
              if (*(oint*)(markptr) & wbit(dynam_bit_o)) # Bindung dynamisch?
                { var reg2 object symbol = *(markptr STACKop varframe_binding_sym); # Variable
                  var reg3 object newval = *(markptr STACKop varframe_binding_value); # neuer Wert
                  *(markptr STACKop varframe_binding_value) = TheSymbolflagged(symbol)->symvalue; # alten Wert im Frame sichern
                  *(oint*)(markptr) |= wbit(active_bit_o); # Bindung aktivieren
                  TheSymbolflagged(symbol)->symvalue = newval; # neuer Wert
                }
                else
                { *(oint*)(markptr) |= wbit(active_bit_o); } # Bindung aktivieren
            }});
        }
        # Body abinterpretieren:
        implicit_progn(popSTACK(),NIL);
        # Frames auflösen:
        unwind(); # VENV-Bindungsframe auflösen
        unwind(); # Variablenbindungs-Frame auflösen
  }   }}

LISPSPECFORM(letstern, 1,0,body)
# (LET* ({varspec}) {decl} {form}), CLTL S. 111
  { # {decl} {form} trennen:
    var reg7 boolean to_compile = parse_dd(STACK_0,aktenv.fun_env);
    # bitte kein Docstring:
    if (!nullp(value3)) { fehler_docstring(S(letstern),STACK_0); }
    if (to_compile) # Deklaration (COMPILE) ?
      # ja -> Form kompilieren:
      { skipSTACK(2); return_Values compile_eval_form(); }
      else
      { skipSTACK(1);
        # Variablenbindungsframe aufbauen, VAR_ENV erweitern:
       {var object* bind_ptr;
        var uintC bind_count;
        make_variable_frame(S(letstern),popSTACK(),&bind_ptr,&bind_count);
        # Dann die Initialisierungsformen auswerten und die Bindungen aktivieren:
        { var reg5 object* frame_pointer = bind_ptr;
          var reg6 uintC count;
          dotimesC(count,bind_count,
            { var reg2 object* initptr = &Next(frame_pointer);
              frame_pointer skipSTACKop -varframe_binding_size;
             {var reg1 object* markptr = &Before(frame_pointer);
              var reg4 object init = *initptr; # nächstes Init
              var reg4 object newval = (eq(init,unbound) ? NIL : (eval(init),value1)); # auswerten, NIL als Default
              if (*(oint*)(markptr) & wbit(dynam_bit_o)) # Bindung dynamisch?
                { var reg3 object symbol = *(markptr STACKop varframe_binding_sym); # Variable
                  *initptr = TheSymbolflagged(symbol)->symvalue; # alten Wert im Frame sichern
                  *(oint*)(markptr) |= wbit(active_bit_o); # Bindung aktivieren
                  TheSymbolflagged(symbol)->symvalue = newval; # neuer Wert
                }
                else
                { *initptr = newval; # neuen Wert in den Frame
                  *(oint*)(markptr) |= wbit(active_bit_o); # Bindung aktivieren
                }
            }});
        }
        # Body abinterpretieren:
        implicit_progn(popSTACK(),NIL);
        # Frames auflösen:
        unwind(); # VENV-Bindungsframe auflösen
        unwind(); # Variablenbindungs-Frame auflösen
  }   }}

LISPSPECFORM(compiler_let, 1,0,body)
# (COMPILER-LET ({varspec}) {form}), CLTL S. 112
  { var reg5 object* varspecs_ = &STACK_1;
    var reg3 object varspecs = *varspecs_; # Liste der Variablen
    var reg7 uintL varcount = llength(varspecs); # Anzahl der Variablen
    get_space_on_STACK(varcount*3*sizeof(object)); # Platz auf dem STACK verlangen
    # varspecs evaluieren:
   {var reg6 object* val_pointer = args_end_pointer; # Pointer über die Werte
    while (consp(varspecs))
      { var reg1 object varspec = Car(varspecs);
        var reg2 object symbol;
        if (consp(varspec))
          # varspec ist ein Cons
          { symbol = Car(varspec);
            varspec = Cdr(varspec);
            if (!(consp(varspec) && nullp(Cdr(varspec))))
              { pushSTACK(Car(varspecs));
                pushSTACK(S(compiler_let));
                fehler(
                       DEUTSCH ? "~: ~ ist keine korrekte Variablenspezifikation." :
                       ENGLISH ? "~: illegal variable specification ~" :
                       FRANCAIS ? "~: ~ n'est pas une spécification de variable licite." :
                       ""
                      );
              }
            # symbol sollte ein nichtkonstantes Symbol sein:
            if (!symbolp(symbol))
              { fehler_symbol:
                fehler_kein_symbol(S(compiler_let),symbol);
              }
            if (constantp(TheSymbol(symbol)))
              { fehler_constant:
                pushSTACK(symbol);
                pushSTACK(S(compiler_let));
                fehler(
                       DEUTSCH ? "~: ~ ist eine Konstante und kann nicht dynamisch gebunden werden." :
                       ENGLISH ? "~: ~ is a constant, cannot be bound" :
                       FRANCAIS ? "~: ~ est une constante et ne peut pas être liée." :
                       ""
                      );
              }
            pushSTACK(Cdr(varspecs));
            eval_noenv(Car(varspec)); # zweites Listenelement auswerten
            varspecs = STACK_0;
            STACK_0 = value1; # und in den Stack
          }
          else
          { symbol = varspec;
            if (!symbolp(symbol)) goto fehler_symbol;
            if (constantp(TheSymbol(symbol))) goto fehler_constant;
            pushSTACK(NIL); # NIL als Wert in den Stack
            varspecs = Cdr(varspecs);
      }   }
    varspecs = *varspecs_;
    # Frame aufbauen:
    { var reg4 object* top_of_frame = STACK; # Pointer übern Frame
      while (consp(varspecs))
        { var reg1 object varspec = Car(varspecs);
          if (consp(varspec)) { varspec = Car(varspec); }
          pushSTACK(Symbol_value(varspec)); # alter Wert der Variablen
          pushSTACK(varspec); # Variable
          varspecs = Cdr(varspecs);
        }
      finish_frame(DYNBIND);
    }
    # Frame fertig aufgebaut, nun die Werte der Variablen verändern:
    varspecs = *varspecs_;
    { var reg2 object* valptr = val_pointer;
      while (consp(varspecs))
        { var reg1 object varspec = Car(varspecs);
          if (consp(varspec)) { varspec = Car(varspec); }
          Symbol_value(varspec) = NEXT(valptr); # neuen Wert der Variablen zuweisen
          varspecs = Cdr(varspecs);
    }   }
    # Nun die Formen evaluieren:
    implicit_progn(*(varspecs_ STACKop -1),NIL);
    # Bindungsframe auflösen:
    unwind();
    # Stack aufräumen:
    set_args_end_pointer(val_pointer);
    skipSTACK(2);
  }}

LISPSPECFORM(progv, 2,0,body)
# (PROGV symbollist valuelist {form}), CLTL S. 112
  { STACK_2 = (eval(STACK_2),value1); # Symbolliste auswerten
   {var reg2 object valuelist = (eval(STACK_1),value1); # Wertliste auswerten
    var reg1 object body = popSTACK();
    skipSTACK(1);
    progv(popSTACK(),valuelist); # Frame aufbauen
    implicit_progn(body,NIL); # body auswerten
    unwind(); # Frame auflösen
  }}

# Fehlermeldung bei FLET/LABELS, wenn keine Funktionsspezifikation vorliegt.
# > caller: Aufrufer, ein Symbol
# > obj: fehlerhafte Funktionsspezifikation
  local nonreturning void fehler_funspec (object caller, object obj);
  local nonreturning void fehler_funspec(caller,obj)
    var reg2 object caller;
    var reg1 object obj;
    { pushSTACK(obj);
      pushSTACK(caller);
      fehler(
             DEUTSCH ? "~: ~ ist keine Funktionsspezifikation." :
             ENGLISH ? "~: ~ is not a function specification" :
             FRANCAIS ? "~: ~ n'est pas une spécification de fonction." :
             ""
            );
    }

# Fehlermeldung bei FLET/LABELS, wenn keine Funktionssymbol vorliegt.
# > caller: Aufrufer, ein Symbol
# > obj: fehlerhaftes Funktionssymbol
  local nonreturning void fehler_funsymbol (object caller, object obj);
  local nonreturning void fehler_funsymbol(caller,obj)
    var reg2 object caller;
    var reg1 object obj;
    { pushSTACK(obj);
      pushSTACK(caller);
      fehler(
             DEUTSCH ? "~: Funktionsname ~ ist kein Symbol." :
             ENGLISH ? "~: function name ~ should be a symbol" :
             FRANCAIS ? "~: Le nom de fonction ~ n'est pas un symbôle." :
             ""
            );
    }

# UP: Beendet ein FLET/MACROLET.
# finish_flet(top_of_frame,body);
# > Stackaufbau: [top_of_frame] def1 name1 ... defn namen [STACK]
# > top_of_frame: Pointer übern Frame
# > body: Formenliste
# < mv_count/mv_space: Werte
# kann GC auslösen
  local Values finish_flet (object* top_of_frame, object body);
  local Values finish_flet(top_of_frame,body)
    var reg2 object* top_of_frame;
    var reg3 object body;
    {{var reg1 uintL bindcount = # Anzahl der Bindungen
        STACK_item_count(STACK,top_of_frame) / 2;
      pushSTACK(aktenv.fun_env); # aktuelles FUN_ENV als NEXT_ENV
      pushSTACK((object)bindcount);
      finish_frame(FUN);
     }# Funktionsbindungsframe ist fertig.
      # FENV-Bindungsframe bauen:
     {var reg1 object* top_of_frame = STACK; # Pointer übern Frame
      pushSTACK(aktenv.fun_env);
      finish_frame(ENV1F);
      # FENV-Bindungsframe ist fertig.
      # FUN_ENV erweitern:
      # top_of_frame = Pointer auf den Funktionsbindungsframe
      aktenv.fun_env = make_framepointer(top_of_frame);
     }# Formen ausführen:
      implicit_progn(body,NIL);
      unwind(); # FENV-Bindungsframe auflösen
      unwind(); # Funktionsbindungsframe auflösen
    }

LISPSPECFORM(flet, 1,0,body)
# (FLET ({funspec}) {form}), CLTL S. 113
  { var reg5 object body = popSTACK(); # ({form})
    var reg1 object funspecs = popSTACK(); # ({funspec})
    # Funktionsbindungs-Frame aufbauen:
    var reg6 object* top_of_frame = STACK; # Pointer übern Frame
    while (consp(funspecs))
      { pushSTACK(body); # Formenliste retten
        pushSTACK(Cdr(funspecs)); # restliche funspecs
        funspecs = Car(funspecs); # nächstes funspec = (name . lambdabody)
        # sollte ein Cons sein, dessen CAR ein Symbol und dessen CDR ein Cons ist:
        if (!consp(funspecs)) { fehler_spec: fehler_funspec(S(flet),funspecs); }
       {var reg2 object name = Car(funspecs);
        var reg3 object lambdabody = Cdr(funspecs);
        if (!symbolp(name)) { fehler_funsymbol(S(flet),name); }
        if (!consp(lambdabody)) { goto fehler_spec; }
        pushSTACK(name); # name retten
        # lambdabody zu einer Closure machen:
        {var reg4 object fun = get_closure(lambdabody,name,&aktenv);
         name = popSTACK();
         funspecs = popSTACK(); # restliche funspecs
         body = popSTACK();
         # in den Frame:
         pushSTACK(fun); # als "Wert" die Closure
         pushSTACK(name); # Name, Bindung ist automatisch aktiv
      }}}
    return_Values finish_flet(top_of_frame,body);
  }

LISPSPECFORM(labels, 1,0,body)
# (LABELS ({funspec}) {form}), CLTL S. 113
  { # Auf den Aufbau eines Funktionsbindungs-Frames kann hier verzichtet werden,
    # weil bei der Bildung der ersten Closure sowieso das Environment genestet
    # und dabei dieser Funktionsbindungs-Frame in einen Vektor geschrieben würde.
    # aktuelles FUN_ENV nesten:
    pushSTACK(nest_fun(aktenv.fun_env));
    # Anzahl der funspecs bestimmen und Syntax abtesten:
   {var reg6 uintL veclength = 1; # = 2 * (Anzahl der funspecs) + 1
    { var reg2 object funspecs = STACK_(1+1);
      while (consp(funspecs))
        { var reg1 object funspec = Car(funspecs);
          # sollte ein Cons sein, dessen CAR ein Symbol und dessen CDR ein Cons ist:
          if (!consp(funspec)) { fehler_spec: fehler_funspec(S(labels),funspec); }
          {var reg3 object name = Car(funspec);
           var reg4 object lambdabody = Cdr(funspec);
           if (!symbolp(name)) { fehler_funsymbol(S(labels),name); }
           if (!consp(lambdabody)) { goto fehler_spec; }
          }
          funspecs = Cdr(funspecs);
          veclength += 2;
    }   }
    # Vektor passender Länge allozieren und darin die Namen eintragen:
    {var reg7 object vec = allocate_vector(veclength);
     {var reg2 object* ptr = &TheSvector(vec)->data[0];
      var reg1 object funspecs = STACK_(1+1);
      while (consp(funspecs))
        { *ptr++ = Car(Car(funspecs)); # nächster name
          ptr++; # Funktion bleibt vorerst NIL
          funspecs = Cdr(funspecs);
        }
      *ptr++ = popSTACK(); # genestetes FUN_ENV als letztes Vektor-Element
     }
     {var reg5 object body = popSTACK(); # Formenliste
      var reg2 object funspecs = popSTACK();
      # FENV-Bindungsframe aufbauen:
      { var reg1 object* top_of_frame = STACK; # Pointer übern Frame
        pushSTACK(aktenv.fun_env);
        finish_frame(ENV1F);
      }
      # FUN_ENV erweitern:
      aktenv.fun_env = vec;
      # Closures erzeugen und in den Vektor stecken:
      pushSTACK(body);
      pushSTACK(vec);
      {var reg4 uintL index = 1; # Index in den Vektor
       while (consp(funspecs))
         { pushSTACK(Cdr(funspecs)); # restliche funspecs
          {var reg1 object funspec = Car(funspecs);
           # Closure erzeugen:
           var reg3 object fun = get_closure(Cdr(funspec),Car(funspec),&aktenv);
           funspecs = popSTACK();
           TheSvector(STACK_0)->data[index] = fun; # in den Vektor stecken
           index += 2;
      }  }}
      skipSTACK(1); # Vektor vergessen
      body = popSTACK();
      # Formen ausführen:
      implicit_progn(body,NIL);
      unwind(); # FENV-Bindungsframe auflösen
  }}}}

LISPSPECFORM(macrolet, 1,0,body)
# (MACROLET ({macrodef}) {form}), CLTL S. 113
  { var reg3 object body = popSTACK(); # ({form})
    var reg1 object macrodefs = popSTACK(); # ({macrodef})
    # Macrobindungs-Frame aufbauen:
    var reg5 object* top_of_frame = STACK; # Pointer übern Frame
    while (consp(macrodefs))
      { pushSTACK(body); # Formenliste retten
        pushSTACK(Cdr(macrodefs)); # restliche macrodefs
        macrodefs = Car(macrodefs); # nächstes macrodef = (name . lambdabody)
        # sollte ein Cons sein, dessen CAR ein Symbol und dessen CDR ein Cons ist:
        if (!consp(macrodefs))
          { fehler_spec:
            pushSTACK(macrodefs);
            pushSTACK(S(macrolet));
            fehler(
                   DEUTSCH ? "~: ~ ist keine Macro-Spezifikation." :
                   ENGLISH ? "~: ~ is not a macro specification" :
                   FRANCAIS ? "~: ~ n'est pas une spécification de macro." :
                   ""
                  );
          }
       {var reg4 object name = Car(macrodefs);
        if (!symbolp(name))
          { pushSTACK(name);
            pushSTACK(S(macrolet));
            fehler(
                   DEUTSCH ? "~: Macro-Name ~ ist kein Symbol." :
                   ENGLISH ? "~: macro name ~ should be a symbol" :
                   FRANCAIS ? "~: Le nom de macro ~ n'est pas un symbôle." :
                   ""
                  );
          }
        if (!mconsp(Cdr(macrodefs))) { goto fehler_spec; }
        pushSTACK(name); # name retten
        # Macro-Expander bauen: (EVAL (SYSTEM::MAKE-MACRO-EXPANSION macrodef))
        pushSTACK(macrodefs); funcall(S(make_macro_expansion),1);
        eval_noenv(value1);
        pushSTACK(value1);
        {var reg2 object fun = allocate_cons();
         Car(fun) = S(macro); Cdr(fun) = popSTACK(); # fun = (SYS::MACRO . expander)
         name = popSTACK();
         macrodefs = popSTACK(); # restliche macrodefs
         body = popSTACK();
         # in den Frame:
         pushSTACK(fun); # als "Wert" das Cons mit dem Expander
         pushSTACK(name); # Name, Bindung ist automatisch aktiv
      }}}
    return_Values finish_flet(top_of_frame,body);
  }

LISPSPECFORM(if, 2,1,nobody)
# (IF test form1 [form2]), CLTL S. 115
  { eval(STACK_2); # Bedingung auswerten
   {var reg1 object form;
    if (!nullp(value1))
      { form = STACK_1; skipSTACK(3); } # form1 auswerten
      else
      { form = STACK_0; skipSTACK(3); # form2 auswerten
        if (eq(form,unbound))
          { value1 = NIL; mv_count=1; return; } # keine angegeben -> NIL
      }
    eval(form);
  }}
        
LISPSPECFORM(when, 1,0,body)
# (WHEN test {form}), CLTL S. 115
  { eval(STACK_1); # Bedingung auswerten
    if (!nullp(value1))
      { var reg1 object body = STACK_0;
        skipSTACK(2);
        implicit_progn(body,NIL);
      }
      else
      { skipSTACK(2);
        value1 = NIL; mv_count=1;
      }
  }

LISPSPECFORM(unless, 1,0,body)
# (UNLESS test {form}), CLTL S. 115
  { eval(STACK_1); # Bedingung auswerten
    if (nullp(value1))
      { var reg1 object body = STACK_0;
        skipSTACK(2);
        implicit_progn(body,NIL);
      }
      else
      { skipSTACK(2);
        value1 = NIL; mv_count=1;
      }
  }

LISPSPECFORM(cond, 0,0,body)
# (COND {(bed {form})}), CLTL S. 116
  { while (mconsp(STACK_0))
      { var reg1 object clause = STACK_0; # Klausel-Liste
        STACK_0 = Cdr(clause); # restliche Klauseln retten
        clause = Car(clause); # nächste Klausel
        if (!consp(clause)) # sollte ein Cons sein
          { pushSTACK(clause);
            pushSTACK(S(cond));
            fehler(
                   DEUTSCH ? "~: Klausel ~ muß Liste sein." :
                   ENGLISH ? "~: clause ~ should be a list" :
                   FRANCAIS ? "~: La clause ~ doit être une liste." :
                   ""
                  );
          }
        pushSTACK(Cdr(clause)); # Klausel-Rest retten
        eval(Car(clause)); # Bedingung auswerten
        if (!nullp(value1)) goto eval_clause;
        skipSTACK(1); # nächste probieren
      }
    # keine Bedingung war erfüllt.
    skipSTACK(1); value1 = NIL; mv_count=1; return;
    # erfüllte Bedingung gefunden:
    eval_clause:
   {var reg1 object clause_rest = popSTACK(); # Klausel-Rest
    skipSTACK(1);
    implicit_progn(clause_rest,value1); # auswerten
  }}

LISPSPECFORM(block, 1,0,body)
# (BLOCK name {form}), CLTL S. 119
  { var reg9 object body = popSTACK();
    var reg9 object name = popSTACK();
    if (!symbolp(name)) { fehler_symbol(name); }
   {var jmp_buf returner; # Rücksprungpunkt
    # Block-Frame aufbauen:
    { var reg1 object* top_of_frame = STACK; # Pointer übern Frame
      pushSTACK(name); # Block-Name
      pushSTACK(aktenv.block_env); # aktuelles BLOCK_ENV als NEXT_ENV
      finish_entry_frame(IBLOCK,&!returner,, goto block_return; );
    }
    # BENV-Frame aufbauen:
    {var reg1 object* top_of_frame = STACK;
     pushSTACK(aktenv.block_env);
     finish_frame(ENV1B);
    # BLOCK_ENV erweitern (top_of_frame = Pointer auf den Block-Frame)
     aktenv.block_env = make_framepointer(top_of_frame);
    }
    # Body ausführen:
    implicit_progn(body,NIL);
    unwind(); # BENV-Bindungsframe auflösen
    block_return: # Hierher wird gesprungen, wenn der BLOCK-Frame einen
                  # RETURN-FROM gefangen hat.
    unwind(); # BLOCK-Frame auflösen
  }}

# Fehler, wenn ein Block bereits verlassen wurde.
# fehler_block_left(name);
# > name: Block-Name
  global nonreturning void fehler_block_left (object name);
  global nonreturning void fehler_block_left(name)
    var reg1 object name;
    { pushSTACK(name);
      pushSTACK(S(return_from));
      fehler(
             DEUTSCH ? "~: Der Block mit Namen ~ wurde bereits verlassen." :
             ENGLISH ? "~: the block named ~ has already been left" :
             FRANCAIS ? "~: Le bloc de nom ~ a déjà été quitté." :
             ""
            );
    }

LISPSPECFORM(return_from, 1,1,nobody)
# (RETURN-FROM name [result]), CLTL S. 120
  { var reg4 object name = STACK_1;
    if (!symbolp(name)) { fehler_symbol(name); } # sollte ein Symbol sein
    # BLOCK_ENV durchgehen:
   {var reg1 object env = aktenv.block_env; # aktuelles BLOCK_ENV
    var reg2 object* FRAME;
    while (stack_env_p(env))
      { # env ist ein Frame-Pointer auf einen IBLOCK-Frame im Stack.
        FRAME = TheFramepointer(env);
        if (mtypecode(FRAME_(0)) & bit(nested_bit_t))
          # Frame schon genestet
          { env = FRAME_(frame_next_env); break; }
        if (eq(FRAME_(frame_name),name)) goto found;
        env = FRAME_(frame_next_env);
      }
    # env ist eine Aliste.
    while (consp(env))
      { var reg3 object block_cons = Car(env);
        if (eq(Car(block_cons),name))
          { env = Cdr(block_cons);
            if (eq(env,disabled)) # Block noch aktiv?
              { fehler_block_left(name); }
            goto found;
          }
        env = Cdr(env);
      }
    # env ist zu Ende.
    pushSTACK(name);
    pushSTACK(S(return_from));
    fehler(
           DEUTSCH ? "~: Es ist kein Block namens ~ sichtbar." :
           ENGLISH ? "~: no block named ~ is currently visible" :
           FRANCAIS ? "~: Aucun bloc de nom ~ n'est visible." :
           ""
          );
    # Block-Frame gefunden: env
    found:
    FRAME = uTheFramepointer(env); # Pointer auf ihn
    # Werte produzieren, mit denen der Block verlassen werden soll:
    {var reg5 object result = popSTACK();
     skipSTACK(1);
     if (!eq(result,unbound)) # result angegeben?
       { eval(result); }
       else
       { value1 = NIL; mv_count=1; }
     # Zum gefundenen Block-Frame springen und ihn auflösen:
     unwind_upto(FRAME);
  }}}

# Die Funktionen MAPCAR, MAPLIST, MAPCAN, MAPCON bauen wir in zwei Versionen:
# Die erste baut die Liste im umgekehrter Reihenfolge, muß sie dann umdrehen.
# Die zweite arbeitet vorwärtsherum, braucht dafür aber ein Cons zuviel.
  #define MAP_REVERSES

#ifdef MAP_REVERSES

# Macro für MAPCAR und MAPLIST
  #define MAPCAR_MAPLIST_BODY(listaccess)  \
    { var reg7 object* args_pointer = rest_args_pointer STACKop 2;              \
      argcount++; # argcount := Anzahl der Listen auf dem Stack                 \
      # Platz für die Funktionsaufruf-Argumente reservieren:                    \
      get_space_on_STACK(sizeof(object)*(uintL)argcount);                       \
      pushSTACK(NIL); # Anfang der Ergebnisliste                                \
     {var reg6 object* ergptr = &STACK_0; # Pointer darauf                      \
      # alle Listen parallel durchlaufen:                                       \
      loop                                                                      \
        { var reg3 object* argptr = args_pointer;                               \
          var reg5 object fun = NEXT(argptr);                                   \
          var reg4 uintC count;                                                 \
          dotimespC(count,argcount,                                             \
            { var reg2 object* next_list_ = &NEXT(argptr);                      \
              var reg1 object next_list = *next_list_;                          \
              if (atomp(next_list)) goto fertig; # eine Liste zu Ende -> fertig \
              pushSTACK(listaccess(next_list)); # als Argument auf den Stack    \
              *next_list_ = Cdr(next_list); # Liste verkürzen                   \
            });                                                                 \
          funcall(fun,argcount); # Funktion aufrufen                            \
          pushSTACK(value1);                                                    \
         {var reg1 object new_cons = allocate_cons(); # neues Cons              \
          Car(new_cons) = popSTACK(); Cdr(new_cons) = STACK_0;                  \
          STACK_0 = new_cons; # verlängert die Ergebnisliste                    \
        }}                                                                      \
      fertig:                                                                   \
      value1 = nreverse(*ergptr); mv_count=1; # Ergebnisliste umdrehen          \
      set_args_end_pointer(args_pointer); # STACK aufräumen                     \
    }}

#else

# Macro für MAPCAR und MAPLIST
  #define MAPCAR_MAPLIST_BODY(listaccess)  \
    { var reg7 object* args_pointer = rest_args_pointer STACKop 2;              \
      argcount++; # argcount := Anzahl der Listen auf dem Stack                 \
      # Platz für die Funktionsaufruf-Argumente reservieren:                    \
      get_space_on_STACK(sizeof(object)*(uintL)argcount);                       \
      # Gesamtliste anfangen:                                                   \
      {var reg1 object new_cons = allocate_cons(); # (CONS NIL NIL)             \
       pushSTACK(new_cons); # Gesamtliste                                       \
       pushSTACK(new_cons); # (last Gesamtliste)                                \
      }                                                                         \
     {var reg6 object* ergptr = &STACK_1; # Pointer darauf                      \
      # alle Listen parallel durchlaufen:                                       \
      loop                                                                      \
        { var reg3 object* argptr = args_pointer;                               \
          var reg5 object fun = NEXT(argptr);                                   \
          var reg4 uintC count;                                                 \
          dotimespC(count,argcount,                                             \
            { var reg2 object* next_list_ = &NEXT(argptr);                      \
              var reg1 object next_list = *next_list_;                          \
              if (atomp(next_list)) goto fertig; # eine Liste zu Ende -> fertig \
              pushSTACK(listaccess(next_list)); # als Argument auf den Stack    \
              *next_list_ = Cdr(next_list); # Liste verkürzen                   \
            });                                                                 \
          funcall(fun,argcount); # Funktion aufrufen                            \
          pushSTACK(value1);                                                    \
         {var reg1 object new_cons = allocate_cons(); # neues Cons              \
          Car(new_cons) = popSTACK(); # new_cons = (LIST (FUNCALL ...))         \
          Cdr(STACK_0) = new_cons; STACK_0 = new_cons; # verlängert Gesamtliste \
        }}                                                                      \
      fertig:                                                                   \
      value1 = Cdr(*ergptr); mv_count=1; # Ergebnisliste ohne Header-Cons       \
      set_args_end_pointer(args_pointer); # STACK aufräumen                     \
    }}

#endif

# Macro für MAPC und MAPL
  #define MAPC_MAPL_BODY(listaccess)  \
    { var reg7 object* args_pointer = rest_args_pointer STACKop 2;              \
      argcount++; # argcount := Anzahl der Listen auf dem Stack                 \
      # Platz für die Funktionsaufruf-Argumente reservieren:                    \
      get_space_on_STACK(sizeof(object)*(uintL)argcount);                       \
      pushSTACK(BEFORE(rest_args_pointer)); # erstes Listenargument retten      \
     {var reg6 object* ergptr = &STACK_0; # Pointer darauf                      \
      # alle Listen parallel durchlaufen:                                       \
      loop                                                                      \
        { var reg3 object* argptr = args_pointer;                               \
          var reg5 object fun = NEXT(argptr);                                   \
          var reg4 uintC count;                                                 \
          dotimespC(count,argcount,                                             \
            { var reg2 object* next_list_ = &NEXT(argptr);                      \
              var reg1 object next_list = *next_list_;                          \
              if (atomp(next_list)) goto fertig; # eine Liste zu Ende -> fertig \
              pushSTACK(listaccess(next_list)); # als Argument auf den Stack    \
              *next_list_ = Cdr(next_list); # Liste verkürzen                   \
            });                                                                 \
          funcall(fun,argcount); # Funktion aufrufen                            \
        }                                                                       \
      fertig:                                                                   \
      value1 = *ergptr; mv_count=1; # 1. Liste als Wert                         \
      set_args_end_pointer(args_pointer); # STACK aufräumen                     \
    }}

#ifdef MAP_REVERSES

# Macro für MAPCAN und MAPCON
  #define MAPCAN_MAPCON_BODY(listaccess)  \
    { var reg7 object* args_pointer = rest_args_pointer STACKop 2;              \
      argcount++; # argcount := Anzahl der Listen auf dem Stack                 \
      # Platz für die Funktionsaufruf-Argumente reservieren:                    \
      get_space_on_STACK(sizeof(object)*(uintL)argcount);                       \
      pushSTACK(NIL); # Anfang der Ergebnisliste                                \
     {var reg6 object* ergptr = &STACK_0; # Pointer darauf                      \
      # alle Listen parallel durchlaufen:                                       \
      loop                                                                      \
        { var reg3 object* argptr = args_pointer;                               \
          var reg5 object fun = NEXT(argptr);                                   \
          var reg4 uintC count;                                                 \
          dotimespC(count,argcount,                                             \
            { var reg2 object* next_list_ = &NEXT(argptr);                      \
              var reg1 object next_list = *next_list_;                          \
              if (atomp(next_list)) goto fertig; # eine Liste zu Ende -> fertig \
              pushSTACK(listaccess(next_list)); # als Argument auf den Stack    \
              *next_list_ = Cdr(next_list); # Liste verkürzen                   \
            });                                                                 \
          funcall(fun,argcount); # Funktion aufrufen                            \
          STACK_0 = nreconc(value1,STACK_0); # Ergebnis anhängen                \
        }                                                                       \
      fertig:                                                                   \
      value1 = nreconc(*ergptr,NIL); mv_count=1; # Ergebnisliste umdrehen       \
      set_args_end_pointer(args_pointer); # STACK aufräumen                     \
    }}

#else

# Macro für MAPCAN und MAPCON
  #define MAPCAN_MAPCON_BODY(listaccess)  \
    { var reg7 object* args_pointer = rest_args_pointer STACKop 2;              \
      argcount++; # argcount := Anzahl der Listen auf dem Stack                 \
      # Platz für die Funktionsaufruf-Argumente reservieren:                    \
      get_space_on_STACK(sizeof(object)*(uintL)argcount);                       \
      # Gesamtliste anfangen:                                                   \
      {var reg1 object new_cons = allocate_cons(); # (CONS NIL NIL)             \
       pushSTACK(new_cons); # Gesamtliste                                       \
       pushSTACK(new_cons); # (last Gesamtliste)                                \
      }                                                                         \
     {var reg6 object* ergptr = &STACK_1; # Pointer darauf                      \
      # alle Listen parallel durchlaufen:                                       \
      loop                                                                      \
        { var reg3 object* argptr = args_pointer;                               \
          var reg5 object fun = NEXT(argptr);                                   \
          var reg4 uintC count;                                                 \
          dotimespC(count,argcount,                                             \
            { var reg2 object* next_list_ = &NEXT(argptr);                      \
              var reg1 object next_list = *next_list_;                          \
              if (atomp(next_list)) goto fertig; # eine Liste zu Ende -> fertig \
              pushSTACK(listaccess(next_list)); # als Argument auf den Stack    \
              *next_list_ = Cdr(next_list); # Liste verkürzen                   \
            });                                                                 \
          funcall(fun,argcount); # Funktion aufrufen                            \
         {var reg1 object list = value1; # anzuhängende Liste                   \
          if (consp(list))                                                      \
            { Cdr(STACK_0) = list; # als (cdr (last Gesamtliste)) einhängen     \
              while (mconsp(Cdr(list))) { list = Cdr(list); }                   \
              STACK_0 = list; # und (last Gesamtliste) := (last list)           \
        }}  }                                                                   \
      fertig:                                                                   \
      value1 = Cdr(*ergptr); mv_count=1; # Ergebnisliste ohne Header-Cons       \
      set_args_end_pointer(args_pointer); # STACK aufräumen                     \
    }}

#endif

#define Identity

LISPFUN(mapcar,2,0,rest,nokey,0,NIL)
# (MAPCAR fun list {list}), CLTL S. 128
  MAPCAR_MAPLIST_BODY(Car)

LISPFUN(maplist,2,0,rest,nokey,0,NIL)
# (MAPLIST fun list {list}), CLTL S. 128
  MAPCAR_MAPLIST_BODY(Identity)

LISPFUN(mapc,2,0,rest,nokey,0,NIL)
# (MAPC fun list {list}), CLTL S. 128
  MAPC_MAPL_BODY(Car)

LISPFUN(mapl,2,0,rest,nokey,0,NIL)
# (MAPL fun list {list}), CLTL S. 128
  MAPC_MAPL_BODY(Identity)

LISPFUN(mapcan,2,0,rest,nokey,0,NIL)
# (MAPCAN fun list {list}), CLTL S. 128
  MAPCAN_MAPCON_BODY(Car)

LISPFUN(mapcon,2,0,rest,nokey,0,NIL)
# (MAPCON fun list {list}), CLTL S. 128
  MAPCAN_MAPCON_BODY(Identity)

LISPSPECFORM(tagbody, 0,0,body)
# (TAGBODY {tag | statement}), CLTL S. 130
  { var reg5 object body = popSTACK();
    # GENV-Frame aufbauen:
    { var reg1 object* top_of_frame = STACK; # Pointer übern Frame
      pushSTACK(aktenv.go_env);
      finish_frame(ENV1G);
    }
    # TAGBODY-Frame aufbauen:
   {var reg6 object* top_of_frame = STACK; # Pointer übern Frame
    # Body durchparsen und Tags im Stack ablegen:
    var reg4 uintL tagcount = 0;
    { var reg3 object body_rest = body;
      while (consp(body_rest))
        { var reg2 object item = Car(body_rest);
          body_rest = Cdr(body_rest);
          # Als Tags werden Symbole /=NIL sowie Zahlen angesehen
          # (wie im Compiler), Conses sind Statements.
          if (atomp(item))
            { if (numberp(item) || (symbolp(item) && (!nullp(item))))
                # Marke im Stack ablegen:
                { check_STACK();
                  pushSTACK(body_rest); # Body-Rest nach der Marke
                  pushSTACK(item);
                  tagcount++;
                }
                else
                { pushSTACK(item);
                  pushSTACK(S(tagbody));
                  fehler(
                         DEUTSCH ? "~: ~ ist weder Marke noch Statement." :
                         ENGLISH ? "~: ~ is neither tag nor form" :
                         FRANCAIS ?"~: ~ n'est ni un marqueur ni une forme à evaluer." :
                         ""
                        );
                }
    }   }   }
    if (tagcount>0)
      { var jmp_buf returner; # Rücksprungpunkt
        pushSTACK(aktenv.go_env); # aktuelles GO_ENV als NEXT_ENV
        finish_entry_frame(ITAGBODY,&!returner,, goto go_entry; );
        # GO_ENV erweitern:
        aktenv.go_env = make_framepointer(STACK);
        if (FALSE)
          { go_entry: # Hierher wird gesprungen, wenn dieser Frame ein GO
                      # gefangen hat.
            body = value1; # Die Formenliste wird als value1 übergeben.
          }
        # Statements abarbeiten:
        pushSTACK(body);
        while (mconsp(STACK_0))
          { var reg1 object body_rest = STACK_0;
            STACK_0 = Cdr(body_rest); # restlicher Body
            body_rest = Car(body_rest); # nächstes Item
            if (consp(body_rest)) { eval(body_rest); } # Form -> auswerten
          }
        skipSTACK(1); # Body vergessen
        unwind(); # TAGBODY-Frame auflösen
        unwind(); # GENV-Frame auflösen
      }
      else
      # Body ohne Tags -> nur PROGN mit Wert NIL
      { skipSTACK(2); # GENV-Frame wieder auflösen, GENV ist unverändert
        pushSTACK(body); implicit_prog();
      }
    value1 = NIL; mv_count=1; # Wert NIL
  }}

LISPSPECFORM(go, 1,0,nobody)
# (GO tag), CLTL S. 133
  { var reg3 object tag = popSTACK();
    if (!(numberp(tag) || (symbolp(tag) && (!nullp(tag)))))
      { pushSTACK(tag);
        pushSTACK(S(go));
        fehler(
               DEUTSCH ? "~: ~ ist keine zulässige Marke." :
               ENGLISH ? "~: illegal tag ~" :
               FRANCAIS ? "~: ~ n'est pas un marqueur permis." :
               ""
              );
      }
    # GO_ENV durchgehen:
   {var reg7 object env = aktenv.go_env; # aktuelles GO_ENV
    var reg8 object* FRAME;
    while (stack_env_p(env))
      { # env ist ein Frame-Pointer auf einen ITAGBODY-Frame im Stack.
        FRAME = uTheFramepointer(env);
        if (mtypecode(FRAME_(0)) & bit(nested_bit_t))
          # Frame schon genestet
          { env = FRAME_(frame_next_env); break; }
        # Tags im ungenesteten ITAGBODY-Frame absuchen:
        { var reg1 object* bind_ptr = &FRAME_(frame_bindings); # Pointer unter die Tagbindungen
          var reg2 object* bindend_ptr = STACKpointable(topofframe(FRAME_(0))); # Pointer über die Tagbindungen
          do { if (eql(*bind_ptr,tag)) # Tag gefunden?
                 { value1 = *(bind_ptr STACKop 1); # Formenliste aus dem Frame holen
                   goto found;
                 }
               bind_ptr skipSTACKop 2;
             }
             until (bind_ptr==bindend_ptr);
        }
        env = FRAME_(frame_next_env);
      }
    # env ist eine Aliste.
    while (consp(env))
      { var reg6 object tagbody_cons = Car(env);
        var reg5 object tagbody_vec = Car(tagbody_cons); # Tag-Vektor
        var reg1 object* tagptr = &TheSvector(tagbody_vec)->data[0];
        var reg4 uintL index = 0;
        var reg2 uintL count;
        dotimespL(count,TheSvector(tagbody_vec)->length,
          { if (eql(*tagptr++,tag)) # Tag gefunden?
              { env = Cdr(tagbody_cons);
                if (eq(env,disabled)) # Tagbody noch aktiv?
                  { pushSTACK(tag);
                    pushSTACK(S(go));
                    fehler(
                           DEUTSCH ? "~: Tagbody zur Marke ~ wurde bereits verlassen." :
                           ENGLISH ? "~: tagbody for tag ~ has already been left" :
                           FRANCAIS ? "~: Le TAGBODY du marqueur ~ a déjà été quitté." :
                           ""
                          );
                  }
                FRAME = uTheFramepointer(env); # Pointer auf den (noch aktiven!) Frame
                value1 = FRAME_(frame_bindings+2*index+1); # Formenliste
                goto found;
              }
            index++;
          });
        env = Cdr(env);
      }
    # env ist zu Ende.
    pushSTACK(tag);
    pushSTACK(S(go));
    fehler(
           DEUTSCH ? "~: Es ist keine Marke namens ~ sichtbar." :
           ENGLISH ? "~: no tag named ~ is currently visible" :
           FRANCAIS ? "~: Aucun marqueur de nom ~ n'est visible." :
           ""
          );
    # Tagbody-Frame gefunden. FRAME ist ein Pointer auf ihn (ohne Typinfo),
    # value1 die Liste der auszuführenden Formen.
    found:
    mv_count=1; # Formenliste value1 wird übergeben
    # Zum gefundenen Tagbody-Frame springen und dort weitermachen:
    unwind_upto(FRAME);
  }}

# Fehlermeldung bei zu vielen Werten
# fehler_mv_zuviel(caller);
# > caller: Aufrufer, ein Symbol
  global nonreturning void fehler_mv_zuviel (object caller);
  global nonreturning void fehler_mv_zuviel(caller)
    var reg1 object caller;
    { pushSTACK(caller);
      fehler(
             DEUTSCH ? "~: Zu viele Werte." :
             ENGLISH ? "~: too many values" :
             FRANCAIS ? "~: Trop de valeurs." :
             ""
            );
    }

LISPFUN(values,0,0,rest,nokey,0,NIL)
# (VALUES {arg}), CLTL S. 134
  { if (argcount >= mv_limit) { fehler_mv_zuviel(S(values)); }
    STACK_to_mv(argcount);
  }

LISPFUNN(values_list,1)
# (VALUES-LIST list), CLTL S. 135
  { list_to_mv(popSTACK(), fehler_mv_zuviel(S(values_list)); ); }

LISPSPECFORM(multiple_value_list, 1,0,nobody)
# (MULTIPLE-VALUE-LIST form), CLTL S. 135
  { eval(popSTACK()); # form auswerten
    mv_to_list(); # Werte in Liste packen
    value1 = popSTACK(); mv_count=1; # Liste als Wert
  }

LISPSPECFORM(multiple_value_call, 1,0,body)
# (MULTIPLE-VALUE-CALL fun {form}), CLTL S. 135
  { var reg3 object* fun_ = &STACK_1;
    *fun_ = (eval(*fun_),value1); # Funktion auswerten
   {var reg1 object forms = popSTACK(); # Formenliste
    var reg2 uintL argcount = 0; # Anzahl der bisherigen Argumente
    while (consp(forms))
      { pushSTACK(Cdr(forms)); # restliche Formen
        eval(Car(forms)); # nächste Form auswerten
        forms = popSTACK();
        # Deren Werte in den Stack:
        argcount += (uintL)mv_count;
        mv_to_STACK();
      }
    if (((uintL)~(uintL)0 > ca_limit_1) && (argcount > ca_limit_1))
      { pushSTACK(*fun_);
        pushSTACK(S(multiple_value_call));
        fehler(
               DEUTSCH ? "~: Zu viele Argumente für ~" :
               ENGLISH ? "~: too many arguments to ~" :
               FRANCAIS ? "~: Trop d'arguments pour ~." :
               ""
              );
      }
    funcall(*fun_,argcount); # Funktion aufrufen
    skipSTACK(1);
  }}

LISPSPECFORM(multiple_value_prog1, 1,0,body)
# (MULTIPLE-VALUE-PROG1 form {form}), CLTL S. 136
  {  eval(STACK_1); # erste Form auswerten
   { var reg3 object body = popSTACK();
     skipSTACK(1);
    {var reg2 uintC mvcount = mv_count; # Wertezahl
     mv_to_STACK(); # alle Werte in den Stack
     pushSTACK(body); implicit_prog();
     STACK_to_mv(mvcount); # alle Werte wieder aus dem Stack zurückholen
  }}}

LISPSPECFORM(multiple_value_bind, 2,0,body)
# (MULTIPLE-VALUE-BIND ({var}) values-form {decl} {form}), CLTL S. 136
  { # {decl} {form} trennen:
    var reg10 boolean to_compile = parse_dd(STACK_0,aktenv.fun_env);
    # bitte kein Docstring:
    if (!nullp(value3)) { fehler_docstring(S(multiple_value_bind),STACK_0); }
    if (to_compile) # Deklaration (COMPILE) ?
      # ja -> Form kompilieren:
      { skipSTACK(3); return_Values compile_eval_form(); }
      else
      { var reg10 object varlist = STACK_2;
        STACK_2 = STACK_1;
        skipSTACK(2);
        # Variablenbindungsframe aufbauen, VAR_ENV erweitern:
       {var reg9 object* form_ = &STACK_0;
        var object* bind_ptr;
        var uintC bind_count;
        make_variable_frame(S(multiple_value_bind),varlist,&bind_ptr,&bind_count);
        # Stackaufbau: values-form, Variablenbindungsframe, Env-Bindungs-Frame, ({form}).
        # Dann values-form auswerten:
        eval(*form_);
        # Macro zum Binden von Variablen im Variablenframe:
        # Bindet die nächste Variable an value, erniedrigt frame_pointer um 2 bzw. 3.
        #define bind_next_var(value)  \
          { var reg3 object* valptr = &Next(frame_pointer);                   \
            frame_pointer skipSTACKop -varframe_binding_size;                 \
           {var reg2 object* markptr = &Before(frame_pointer);                \
            if (*(oint*)(markptr) & wbit(dynam_bit_o))                        \
              # dynamische Bindung aktivieren:                                \
              { var reg4 object sym = *(markptr STACKop varframe_binding_sym); # Variable \
                *valptr = TheSymbolflagged(sym)->symvalue; # alten Wert in den Frame      \
                *(oint*)(markptr) |= wbit(active_bit_o); # Bindung aktivieren             \
                TheSymbolflagged(sym)->symvalue = (value); # neuen Wert in die Wertzelle  \
              }                                                               \
              else                                                            \
              # statische Bindung aktivieren:                                 \
              { *valptr = (value); # neuen Wert in den Frame                  \
                *(oint*)(markptr) |= wbit(active_bit_o); # Bindung aktivieren \
              }                                                               \
          }}
        # Die r:=bind_count Variablen an die s:=mv_count Werte binden:
        # (Falls die Variablen ausgehen: restliche Werte wegwerfen;
        #  falls die Werte ausgehen: mit NIL auffüllen.)
        # Hier r>=0 und s>=0.
        { var reg5 object* frame_pointer = bind_ptr;
          var reg7 uintC r = bind_count;
          var reg6 object* mv_pointer;
          var reg8 uintC s = mv_count;
          if (r==0) goto ok; # keine Variablen?
          if (s==0) goto fill; # keine Werte?
          # noch min(r,s)>0 Werte binden:
          #if !defined(VALUE1_EXTRA)
          mv_pointer = &mv_space[0];
          #else
          bind_next_var(value1);
          if (--r == 0) goto ok; # keine Variablen mehr?
          if (--s == 0) goto fill; # keine Werte mehr?
          mv_pointer = &mv_space[1];
          #endif
          # noch min(r,s)>0 Werte binden:
          loop
            { bind_next_var(*mv_pointer++);
              if (--r == 0) goto ok; # keine Variablen mehr?
              if (--s == 0) goto fill; # keine Werte mehr?
            }
          fill: # Noch r>0 Variablen an NIL binden
          dotimespC(r,r, { bind_next_var(NIL); } );
          ok: ;
        }
        # Body abinterpretieren:
        implicit_progn(popSTACK(),NIL);
        # Frames auflösen:
        unwind(); # VENV-Bindungsframe auflösen
        unwind(); # Variablenbindungs-Frame auflösen
        skipSTACK(1);
  }   }}

LISPSPECFORM(multiple_value_setq, 2,0,nobody)
# (MULTIPLE-VALUE-SETQ ({var}) form), CLTL S. 136
  {  eval(popSTACK()); # form auswerten
   { var reg5 object varlist = popSTACK();
     var reg6 object* args_end = args_end_pointer;
     mv_to_STACK(); # Werte in den Stack schreiben (erleichtert den Zugriff)
     # Variablenliste durchgehen:
    {var reg2 object* mvptr = args_end;
     var reg4 uintC count = mv_count; # Anzahl noch verfügbarer Werte
     while (consp(varlist))
       { var reg1 object symbol = Car(varlist); # nächste Variable
         if (!symbolp(symbol)) # sollte ein Symbol
           { fehler_kein_symbol(S(multiple_value_setq),symbol); }
         if (constantp(TheSymbol(symbol))) # und keine Konstante sein
           { fehler_symbol_constant(S(multiple_value_setq),symbol); }
        {var reg3 object value;
         if (count>0)
           { value = NEXT(mvptr); count--; } # nächster Wert
           else
           { value = NIL; } # NIL, wenn alle Werte verbraucht
         setq(symbol,value);
         varlist = Cdr(varlist);
       }}
     set_args_end_pointer(args_end); # STACK aufräumen
     mv_count=1; # letzter value1 als einziger Wert
  }}}

LISPSPECFORM(catch, 1,0,body)
# (CATCH tag {form}), CLTL S. 139
  { STACK_1 = (eval(STACK_1),value1); # tag auswerten
    # CATCH-Frame zu Ende aufbauen:
   {var reg1 object body = popSTACK(); # ({form})
    var reg2 object* top_of_frame = STACK STACKop 1; # Pointer übern Frame
    var jmp_buf returner; # Rücksprungpunkt merken
    finish_entry_frame(CATCH,&!returner,, goto catch_return; );
    # Body ausführen:
    implicit_progn(body,NIL);
    catch_return: # Hierher wird gesprungen, wenn der oben aufgebaute
                  # Catch-Frame einen Throw gefangen hat.
    skipSTACK(3); # CATCH-Frame auflösen
  }}

LISPSPECFORM(unwind_protect, 1,0,body)
# (UNWIND-PROTECT form {cleanup}), CLTL S. 140
  { var reg2 object cleanup = popSTACK();
    var reg3 object form = popSTACK();
    # UNWIND-PROTECT-Frame aufbauen:
    pushSTACK(cleanup);
   {var reg4 object* top_of_frame = STACK;
    var jmp_buf returner; # Rücksprungpunkt
    finish_entry_frame(UNWIND_PROTECT,&!returner,, goto throw_save; );
    # Protected form auswerten:
    eval(form);
    # Cleanup nach normaler Beendigung der Protected form:
      # UNWIND-PROTECT-Frame auflösen:
      skipSTACK(2);
      cleanup = popSTACK();
      # Werte retten:
     {var reg1 uintC mvcount = mv_count;
      mv_to_STACK();
      # Cleanup-Formen abarbeiten:
      pushSTACK(cleanup); implicit_prog();
      # Werte zurückschreiben:
      STACK_to_mv(mvcount);
     }
    return;
    throw_save: # Hierher wird gesprungen, wenn der oben aufgebaute
                # Unwind-Protect-Frame einen Throw aufgehalten hat.
                # unwind_protect_to_save ist zu retten und am Schluß anzuspringen.
    { var reg5 restart fun = unwind_protect_to_save.fun;
      var reg6 object* arg = unwind_protect_to_save.upto_frame;
    # Cleanup:
      # UNWIND-PROTECT-Frame auflösen:
      skipSTACK(2);
      cleanup = popSTACK();
      # Werte retten:
     {var reg1 uintC mvcount = mv_count;
      mv_to_STACK();
      # Cleanup-Formen abarbeiten:
      pushSTACK(cleanup); implicit_prog();
      # Werte zurückschreiben:
      STACK_to_mv(mvcount);
     }# und weiterspringen:
      fun(arg);
    }
  }}

LISPSPECFORM(throw, 2,0,nobody)
# (THROW tag result), CLTL S. 142
  { STACK_1 = (eval(STACK_1),value1); # tag auswerten
    eval(popSTACK()); # result auswerten
   {var reg1 object tag = popSTACK(); # ausgewertetes Tag
    throw(tag); # versuche auf dieses zu THROWen
    # Nicht gelungen.
    pushSTACK(tag);
    pushSTACK(S(throw));
    fehler(
           DEUTSCH ? "~: Es gibt kein CATCH zur Marke ~." :
           ENGLISH ? "~: there is no CATCHer for tag ~" :
           FRANCAIS ? "~: Il n'y a pas de CATCH correspondant au marqueur ~." :
           ""
          );
  }}

LISPFUNN(driver,1)
# (SYS::DRIVER fun) baut einen Driver-Frame auf, der jedesmal die Funktion
# fun (mit 0 Argumenten) aufruft. fun wird in einer Endlosschleife ausgeführt,
# die mit GO oder THROW abgebrochen werden kann.
  { var reg1 object* top_of_frame = STACK; # Pointer übern Frame
    var DRIVER_frame_data returner_and_data; # Einsprungpunkt merken
    #ifdef HAVE_NUM_STACK
    returner_and_data.old_NUM_STACK_normal = NUM_STACK_normal;
    #endif
    finish_entry_frame(DRIVER,&!returner_and_data.returner,,;);
    # Hier ist der Einsprungpunkt.
    loop { funcall(STACK_(0+2),0); } # fun aufrufen, Endlosschleife
  }

LISPFUNN(unwind_to_driver,0)
# (SYS::UNWIND-TO-DRIVER) macht ein UNWIND bis zum nächsthöheren Driver-Frame.
  { reset(); }

LISPFUNN(macro_function,1)
# (MACRO-FUNCTION symbol), CLTL S. 144
  { var reg3 object symbol = popSTACK();
    if (!symbolp(symbol)) { fehler_symbol(symbol); }
   {var reg2 object fundef = Symbol_function(symbol); # globale Funktionsdefinition
    if (fsubrp(fundef))
      # ein FSUBR -> Propertyliste absuchen: (GET symbol 'SYS::MACRO)
      { var reg1 object got = get(symbol,S(macro)); # suchen
        if (eq(got,unbound)) goto nil; # nichts gefunden?
        value1 = got;
      }
    elif (consp(fundef) && eq(Car(fundef),S(macro))) # (SYS::MACRO . expander) ?
      { value1 = Cdr(fundef); }
    else # SUBR/Closure/#<UNBOUND> -> keine Macrodefinition
      { nil: value1 = NIL; }
    mv_count=1;
  }}

LISPFUN(macroexpand,1,1,norest,nokey,0,NIL)
# (MACROEXPAND form [env]), CLTL S. 151
  { var reg1 object env = popSTACK();
    if (eq(env,unbound)) { env = NIL; } # NIL (leeres FUN_ENV) als Default
   {var reg2 object form = STACK_0;
    STACK_0 = env; # env retten
    macroexp0(form,env); # expandieren
    if (!nullp(value2)) # was getan?
      # ja -> zu Tode expandieren:
      { do { macroexp0(value1,STACK_0); } until (nullp(value2));
        value2 = T;
      }
    mv_count=2; skipSTACK(1);
  }}

LISPFUN(macroexpand_1,1,1,norest,nokey,0,NIL)
# (MACROEXPAND-1 form [env]), CLTL S. 151
  { var reg1 object env = popSTACK();
    if (eq(env,unbound)) { env = NIL; } # NIL (leeres FUN_ENV) als Default
   {var reg2 object form = popSTACK();
    macroexp0(form,env); # 1 mal expandieren
    mv_count=2;
  }}

LISPSPECFORM(declare, 0,0,body)
# (DECLARE {decl-spec}), CLTL S. 153
  { # ({decl-spec}) bereits in STACK_0
    fehler(
           DEUTSCH ? "Deklarationen ~ an dieser Stelle nicht erlaubt." :
           ENGLISH ? "declarations ~ are not allowed here" :
           FRANCAIS ? "Les déclarations ~ ne sont pas permises à cet endroit." :
           ""
          );
  }

LISPSPECFORM(the, 2,0,nobody)
# (THE value-type form), CLTL S. 161
  { eval(STACK_0); # form auswerten
    mv_to_list(); # Werteliste bilden und retten
    # Stackaufbau: value-type, form, values.
    # zum Typ-Check (SYS::%THE values value-type) aufrufen:
    pushSTACK(STACK_0); pushSTACK(STACK_(2+1)); funcall(S(pthe),2);
    if (nullp(value1))
      # Typ-Check mißlang
      { pushSTACK(STACK_(2+0)); # value-type
        pushSTACK(STACK_(0+1)); # values
        pushSTACK(STACK_(1+2)); # form
        pushSTACK(S(the));
        fehler(
               DEUTSCH ? "~: Die Form ~ produzierte die Werte ~, nicht vom Typ ~" :
               ENGLISH ? "~: ~ evaluated to the values ~, not of type ~" :
               FRANCAIS ? "~: La forme ~ a produit les valeurs ~ qui ne sont pas de type ~." :
               ""
              );
      }
    # Typ-Check OK -> Werte zurückgeben:
    list_to_mv(popSTACK(), { fehler_mv_zuviel(S(the)); } );
    skipSTACK(2);
  }

LISPFUNN(proclaim,1)
# (PROCLAIM decl-spec)
  { var reg3 object declspec = popSTACK();
    if (!consp(declspec))
      { pushSTACK(declspec);
        pushSTACK(S(proclaim));
        fehler(
               DEUTSCH ? "~: Falsche Deklaration: ~" :
               ENGLISH ? "~: bad declaration ~" :
               FRANCAIS ? "~: Mauvaise déclaration : ~" :
               ""
              );
      }
   {var reg4 object decltype = Car(declspec); # Deklarationstyp
    if (eq(decltype,S(special))) # SPECIAL
      { while (consp( declspec = Cdr(declspec) ))
          { var reg1 object symbol = Car(declspec);
            if (!symbolp(symbol)) { fehler_symbol(symbol); }
            if (!keywordp(symbol)) { clear_const_flag(TheSymbol(symbol)); }
            set_special_flag(TheSymbol(symbol));
      }   }
    elif (eq(decltype,S(declaration))) # DECLARATION
      { while (consp( declspec = Cdr(declspec) ))
          { var reg2 object symbol = Car(declspec);
            if (!symbolp(symbol)) { fehler_symbol(symbol); }
            # (PUSHNEW symbol (cdr declaration-types)) :
            { var reg1 object list = Cdr(O(declaration_types));
              while (consp(list))
                { if (eq(Car(list),symbol)) goto not_adjoin;
                  list = Cdr(list);
            }   }
            pushSTACK(declspec); pushSTACK(symbol);
           {var reg1 object new_cons = allocate_cons();
            var reg2 object list = O(declaration_types);
            Car(new_cons) = popSTACK(); Cdr(new_cons) = Cdr(list);
            Cdr(list) = new_cons;
            declspec = popSTACK();
           }
            not_adjoin: ;
      }   }
    elif (eq(decltype,S(inline)) || eq(decltype,S(notinline))) # INLINE, NOTINLINE
      { pushSTACK(decltype);
        while (consp( declspec = Cdr(declspec) ))
          { var reg2 object symbol = Car(declspec);
            if (!symbolp(symbol)) { fehler_kein_symbol(S(proclaim),symbol); }
            # (SYS::%PUT symbol 'SYS::INLINABLE decltype) :
            pushSTACK(declspec);
            pushSTACK(symbol); pushSTACK(S(inlinable)); pushSTACK(STACK_(1+2));
            funcall(L(put),3);
            declspec = popSTACK();
          }
        skipSTACK(1);
      }
    # Alles restliche wird ignoriert.
    value1 = NIL; mv_count=1;
  }}

LISPFUNN(eval,1)
# (EVAL form), CLTL S. 321
  { eval_noenv(popSTACK()); } # form im leeren Environment auswerten

# UP: Überprüft ein optionales Environment-Argument für EVALHOOK und APPLYHOOK.
# test_optional_env_arg(&env5);
# > subr_self: Aufrufer (ein SUBR)
# < env5: 5 Komponenten des Environments
# erhöht STACK um 1
  local void test_optional_env_arg (environment* env5);
  local void test_optional_env_arg(env5)
    var reg2 environment* env5;
    { var reg1 object env = popSTACK(); # env-Argument
      if (eq(env,unbound)) # nicht angegeben -> leeres Environment
        { env5->var_env   = NIL;
          env5->fun_env   = NIL;
          env5->block_env = NIL;
          env5->go_env    = NIL;
          env5->decl_env  = O(top_decl_env);
        }
      elif (simple_vector_p(env) && (TheSvector(env)->length == 5))
        # ein Simple-Vector der Länge 5
        { env5->var_env   = TheSvector(env)->data[0];
          env5->fun_env   = TheSvector(env)->data[1];
          env5->block_env = TheSvector(env)->data[2];
          env5->go_env    = TheSvector(env)->data[3];
          env5->decl_env  = TheSvector(env)->data[4];
        }
      else
        { pushSTACK(env);
          pushSTACK(TheSubr(subr_self)->name);
          fehler(
                 DEUTSCH ? "~: ~ ist nicht als Environment geeignet." :
                 ENGLISH ? "~: ~ may not be used as an environment" :
                 FRANCAIS ? "~: ~ ne peut pas être utilisé comme environnement." :
                 ""
                );
    }   }

LISPFUN(evalhook,3,1,norest,nokey,0,NIL)
# (EVALHOOK form evalhookfn applyhookfn [env]), CLTL S. 323
  { var environment env5;
    test_optional_env_arg(&env5); # env-Argument nach env5
   {var reg4 object applyhookfn = popSTACK();
    var reg3 object evalhookfn = popSTACK();
    var reg2 object form = popSTACK();
    bindhooks(evalhookfn,applyhookfn); # *EVALHOOK* und *APPLYHOOK* binden
    # Environment-Frame aufbauen:
    make_ENV5_frame();
    # aktuelle Environments setzen:
    aktenv = env5;
    # form unter Umgehung von *EVALHOOK* und *APPLYHOOK* auswerten:
    eval_no_hooks(form);
    unwind(); # Environment-Frame auflösen
    unwind(); # Bindungsframe für *EVALHOOK* / *APPLYHOOK* auflösen
  }}

LISPFUN(applyhook,4,1,norest,nokey,0,NIL)
# (APPLYHOOK function args evalhookfn applyhookfn [env]), CLTL S. 323
  { var environment env5;
    test_optional_env_arg(&env5); # env-Argument nach env5
   {var reg6 object applyhookfn = popSTACK();
    var reg5 object evalhookfn = popSTACK();
    var reg1 object args = popSTACK();
    var reg7 object fun = popSTACK();
    bindhooks(evalhookfn,applyhookfn); # *EVALHOOK* und *APPLYHOOK* binden
    # Environment-Frame aufbauen:
    make_ENV5_frame();
    # aktuelle Environments setzen:
    aktenv = env5;
    # fun retten:
    { pushSTACK(fun);
     {var reg3 object* fun_ = &STACK_0;
      # Argumente einzeln auswerten und auf dem Stack ablegen:
      var reg2 uintC argcount = 0;
      while (consp(args))
        { pushSTACK(Cdr(args)); # restliche Argumentliste
          eval_no_hooks(Car(args)); # nächstes arg auswerten
          args = STACK_0; STACK_0 = value1; # Wert im Stack ablegen
          argcount++;
          if (argcount==0) # Überlauf?
            { pushSTACK(*fun_);
              pushSTACK(S(applyhook));
              fehler(
                     DEUTSCH ? "~: Zu viele Argumente für ~" :
                     ENGLISH ? "~: too many arguments given to ~" :
                     FRANCAIS ? "~: Trop d'arguments fournis à ~." :
                     ""
                    );
        }   }
      funcall(*fun_,argcount); # Funktion anwenden
      skipSTACK(1);
    }}
    unwind(); # Environment-Frame auflösen
    unwind(); # Bindungsframe für *EVALHOOK* / *APPLYHOOK* auflösen
  }}

LISPFUNN(constantp,1)
# (CONSTANTP expr), CLTL S. 324
  { var reg1 object arg = popSTACK();
    switch (typecode(arg))
      { case_cons: # Cons
          if (eq(Car(arg),S(quote))) goto ja; else goto nein;
        case_symbol: # Symbol
          if (constantp(TheSymbol(arg))) goto ja; else goto nein;
        case_number: # Zahl
        case_char: # Character
        case_string: # String
        case_bvector: # Bit-Vektor
          goto ja;
        default:
          goto nein;
      }
    ja: value1 = T; mv_count=1; return;
    nein: value1 = NIL; mv_count=1; return;
  }

LISPFUN(parse_body,1,2,norest,nokey,0,NIL)
# (SYS::PARSE-BODY body [docstring-allowed [env]])
# parst body, erkennt Deklarationen, liefert 3 Werte:
# 1. body-rest, alle Formen nach den Deklarationen,
# 2. Liste der aufgetretenen declspecs
# 3. docstring (nur falls docstring-allowed=T war) oder NIL.
# (docstring-allowed sollte = NIL oder T sein,
#  env sollte ein Function-Environment sein.)
  { if (eq(STACK_0,unbound)) { STACK_0 = NIL; } # NIL als Default-Environment
   {var reg5 boolean docstring_allowed = (!eq(STACK_1,unbound) && !nullp(STACK_1)); # Docstrings erlaubt?
    var reg2 object body = STACK_2; # body = ({decl|doc} {form})
    STACK_1 = NIL; # Noch war kein Doc-String da
    pushSTACK(NIL); # Anfang decl-spec-Liste
    # Stackaufbau: body, docstring, env, declspecs.
    while (consp(body))
      {  pushSTACK(body); # body retten
       { var reg1 object form = Car(body); # nächste Form
         # evtl. macroexpandieren (ohne FSUBRs zu expandieren):
         do { macroexp(form,STACK_(1+1)); form = value1; }
            until (nullp(value2));
         body = popSTACK();
        {var reg4 object body_rest = Cdr(body); # body verkürzen
         if (stringp(form)) # Doc-String gefunden?
           { if (atomp(body_rest)) # an letzter Stelle der Formenliste?
               goto fertig; # ja -> letzte Form kann kein Doc-String sein!
             if (!docstring_allowed) # kein Doc-String erlaubt?
               { pushSTACK(STACK_3); # ganzer body
                 fehler(
                        DEUTSCH ? "Hier sind keine Doc-Strings erlaubt: ~" :
                        ENGLISH ? "no doc-strings allowed here: ~" :
                        FRANCAIS ? "Les chaînes de documentation ne sont pas permises ici : ~" :
                        ""
                       );
               }
             if (!nullp(STACK_2)) # schon ein Doc-String dagewesen?
               # ja -> mehr als ein Doc-String ist zuviel:
               { pushSTACK(STACK_3); # ganzer body
                 fehler(
                        DEUTSCH ? "In ~ kommen zu viele Doc-Strings vor." :
                        ENGLISH ? "Too many documentation strings in ~" :
                        FRANCAIS ? "Trop de chaînes de documentation apparaîssent dans ~." :
                        ""
                       );
               }
             STACK_2 = form; # neuer Doc-String
             body = body_rest;
           }
         elif (consp(form) && eq(Car(form),S(declare))) # Deklaration (DECLARE ...) ?
           { # neue decl-specs einzeln auf STACK_0 consen:
             pushSTACK(body_rest); # body_rest retten
             pushSTACK(Cdr(form)); # Liste der neuen decl-specs
             while (mconsp(STACK_0))
               { # Diese Deklaration auf STACK_(0+2) consen:
                 var reg3 object new_cons = allocate_cons();
                 Car(new_cons) = Car(STACK_0);
                 Cdr(new_cons) = STACK_(0+2);
                 STACK_(0+2) = new_cons;
                 # zum nächsten decl-spec:
                 STACK_0 = Cdr(STACK_0);
               }
             skipSTACK(1);
             body = popSTACK(); # body := alter body_rest
           }
         else
           { fertig: # fertig mit Durchlaufen der Formenliste
             if (!docstring_allowed # (bei DEFUN o.ä. möchte man das nicht)
                 && !eq(form,Car(body)) # Sofern die Form expandiert wurde,
                )
               # ersetze body durch (cons form (cdr body)) :
               { pushSTACK(body_rest); pushSTACK(form);
                 body = allocate_cons();
                 Car(body) = popSTACK(); # form
                 Cdr(body) = popSTACK(); # body_rest
               }
             break;
           }
      }}}
    value1 = body;
    value2 = nreverse(popSTACK()); # decl-spec-Liste
    skipSTACK(1);
    value3 = popSTACK(); # Doc-String
    skipSTACK(1);
    mv_count=3; # 3 Werte: ({form}), declspecs, doc
  }}

LISPFUNN(keyword_test,2)
# (SYSTEM::KEYWORD-TEST arglist kwlist)
# stellt fest, ob in der Argumentliste arglist (eine paarige Keyword/Value -
# Liste) alle Keywords in der Liste kwlist  vorkommen oder aber
# ein Keyword/Value-Paar :ALLOW-OTHER-KEYS mit value /= NIL vorkommt.
# Wenn nein, Error.
  { var reg4 object arglist = STACK_1;
    # Argumente-Zahl überprüfen:
    { var reg1 uintL argcount = llength(arglist);
      if (!((argcount%2) == 0))
        { pushSTACK(arglist);
          fehler(
                 DEUTSCH ? "Keyword-Argumentliste ~ hat ungerade Länge." :
                 ENGLISH ? "keyword argument list ~ has an odd length" :
                 FRANCAIS ? "La liste de mots clé ~ est de longueur impaire." :
                 ""
                );
    }   }
    # Suche, ob :ALLOW-OTHER-KEYS kommt:
    { var reg1 object arglistr = arglist;
      while (consp(arglistr))
        { if (eq(Car(arglistr),S(Kallow_other_keys)) && !nullp(Car(Cdr(arglistr))))
            goto fertig;
          arglistr = Cdr(Cdr(arglistr));
    }   }
    # Suche, ob alle angegebenen Keywords in kwlist vorkommen:
    { var reg3 object arglistr = arglist;
      while (consp(arglistr))
        { var reg2 object key = Car(arglistr);
          var reg1 object kwlistr = STACK_0;
          while (consp(kwlistr))
            { if (eq(Car(kwlistr),key)) goto found;
              kwlistr = Cdr(kwlistr);
            }
          # nicht gefunden
          pushSTACK(Car(Cdr(arglistr)));
          pushSTACK(key);
          fehler(
                 DEUTSCH ? "Unzulässiges Keyword/Wert-Paar ~, ~ in einer Argumentliste. Die erlaubten Keywords sind ~" :
                 ENGLISH ? "illegal keyword/value pair ~, ~ in argument list. The allowed keywords are ~" :
                 FRANCAIS ? "Paire mot-clé - valeur ~, ~ illicite dans une liste d'arguments. Les mots-clé permis sont ~" :
                 ""
                );
          found: # gefunden. Weiter:
          arglistr = Cdr(Cdr(arglistr));
    }   }
    fertig:
    skipSTACK(2);
    value1 = NIL; mv_count=0; # keine Werte
  }

LISPSPECFORM(and, 0,0,body)
# (AND {form}), CLTL S. 82
  { var reg1 object body = popSTACK();
    if (atomp(body))
      { value1 = T; mv_count=1; } # (AND) -> T
      else
      loop
        { pushSTACK(Cdr(body));
          eval(Car(body)); # form auswerten
          body = popSTACK();
          if (atomp(body)) break; # am Schluß: Werte der letzten Form zurück
          if (nullp(value1)) { mv_count=1; break; } # vorzeitig: 1 Wert NIL
        }
  }

LISPSPECFORM(or, 0,0,body)
# (OR {form}), CLTL S. 83
  { var reg1 object body = popSTACK();
    if (atomp(body))
      { value1 = NIL; mv_count=1; } # (OR) -> NIL
      else
      loop
        { pushSTACK(Cdr(body));
          eval(Car(body)); # form auswerten
          body = popSTACK();
          if (atomp(body)) break; # am Schluß: Werte der letzten Form zurück
          if (!nullp(value1)) { mv_count=1; break; } # vorzeitig: 1 Wert /=NIL
        }
  }

