# Funktionen betr. Symbole für CLISP
# Bruno Haible 12.12.1992

#include "lispbibl.c"


# Fehlermeldung, wenn ein Objekt kein Symbol ist.
# fehler_kein_symbol(caller,obj);
# > caller: Aufrufer (ein Symbol)
# > obj: Nicht-Symbol
  global nonreturning void fehler_kein_symbol (object caller, object obj);
  global nonreturning void fehler_kein_symbol(caller,obj)
    var reg2 object caller;
    var reg1 object obj;
    { pushSTACK(obj);
      pushSTACK(caller);
      fehler(
             DEUTSCH ? "~: ~ ist kein Symbol." :
             ENGLISH ? "~: ~ is not a symbol" :
             FRANCAIS ? "~ : ~ n'est pas un symbole." :
             ""
            );
    }

# UP: Liefert die globale Funktionsdefinition eines Symbols,
# mit Test, ob das Symbol eine globale Funktion darstellt.
# Symbol_function_checked(symbol)
# > symbol: Symbol
# < ergebnis: seine globale Funktionsdefinition
  global object Symbol_function_checked (object symbol);
  global object Symbol_function_checked(symbol)
    var reg1 object symbol;
    { var reg2 object fun = Symbol_function(symbol);
      if (eq(fun,unbound))
        { pushSTACK(symbol);
          pushSTACK(S(symbol_function));
          fehler(
                 DEUTSCH ? "~: ~ hat keine globale Funktionsdefinition." :
                 ENGLISH ? "~: ~ has no global function definition" :
                 FRANCAIS ? "~ : ~ n'a pas de définition globale de fonction." :
                 ""
                );
        }
      if (consp(fun))
        { pushSTACK(symbol);
          pushSTACK(S(function));
          fehler(
                 DEUTSCH ? "~: ~ ist ein Macro und keine Funktion." :
                 ENGLISH ? "~: ~ is a macro, not a function" :
                 FRANCAIS ? "~ : ~ est une macro et non une fonction." :
                 ""
                );
        }
      return fun;
    }

# Fehlermeldung, wenn ein Symbol eine Property-Liste ungerader Länge hat.
# fehler_plist_odd(symbol);
# > symbol: Symbol
  local nonreturning void fehler_plist_odd (object symbol);
  local nonreturning void fehler_plist_odd(symbol)
    var reg1 object symbol;
    { pushSTACK(symbol);
      pushSTACK(S(get));
      fehler(
             DEUTSCH ? "~: Die Property-Liste von ~ hat ungerade Länge." :
             ENGLISH ? "~: the property list of ~ has an odd length" :
             FRANCAIS ? "~ : La liste de propriétés attachée à ~ est de longueur impaire." :
             ""
            );
    }

# UP: Holt eine Property aus der Property-Liste eines Symbols.
# get(symbol,key)
# > symbol: ein Symbol
# > key: ein mit EQ zu vergleichender Key
# < value: dazugehöriger Wert aus der Property-Liste von symbol, oder unbound.
  global object get (object symbol, object key);
  global object get(symbol,key)
    var reg3 object symbol;
    var reg2 object key;
    { var reg1 object plistr = Symbol_plist(symbol);
      loop
        { if (atomp(plistr)) goto notfound;
          if (eq(Car(plistr),key)) goto found;
          plistr = Cdr(plistr);
          if (atomp(plistr)) goto odd;
          plistr = Cdr(plistr);
        }
      found: # key gefunden
        plistr = Cdr(plistr);
        if (atomp(plistr)) goto odd;
        return Car(plistr);
      odd: # Property-Liste hat ungerade Länge
        fehler_plist_odd(symbol);
      notfound: # key nicht gefunden
        return unbound;
    }

# Fehlermeldung, wenn ein Objekt kein Symbol ist.
# fehler_symbol(obj);
# > subr_self: Aufrufer (ein SUBR)
# > obj: Nicht-Symbol
  local nonreturning void fehler_symbol (object obj);
  local nonreturning void fehler_symbol(obj)
    var reg1 object obj;
    { fehler_kein_symbol(TheSubr(subr_self)->name,obj); }

LISPFUNN(putd,2)
# (SYS::%PUTD symbol function)
  { var reg2 object symbol = STACK_1;
    if (!symbolp(symbol)) { fehler_symbol(symbol); }
   {var reg1 object fun = STACK_0;
    # fun muß SUBR, FSUBR, Closure oder (SYS::MACRO . Closure) sein,
    # Lambda-Ausdruck wird sofort in eine Closure umgewandelt:
    if (subrp(fun) || fsubrp(fun) || closurep(fun)) goto ok;
    if (consp(fun)) # ein Cons?
      { if (eq(Car(fun),S(macro)))
          { if (mclosurep(Cdr(fun))) goto ok; } # (SYS::MACRO . Closure) ist ok
        elif (eq(Car(fun),S(lambda)))
          { var reg3 object lambdabody = Cdr(fun); # (lambda-list {decl|doc} . body)
            # leeres Environment für get_closure:
            pushSTACK(NIL); pushSTACK(NIL); pushSTACK(NIL); pushSTACK(NIL); pushSTACK(NIL);
           {var reg4 environment* env = &STACKblock_(environment,0);
            fun = get_closure(lambdabody,symbol,env); # Closure erzeugen
            skipSTACK(5);
            goto ok;
      }   }}
    pushSTACK(fun);
    fehler(
           DEUTSCH ? "SETF SYMBOL-FUNCTION: ~ ist keine Funktion." :
           ENGLISH ? "SETF SYMBOL-FUNCTION: ~ is not a function" :
           FRANCAIS ? "SETF SYMBOL-FUNCTION : ~ n'est pas une fonction." :
           ""
          );
    ok: # fun korrekt, in die Funktionszelle stecken:
    value1 = popSTACK(); # function-Argument als Wert
    Symbol_function(popSTACK()) = fun;
    mv_count=1;
  }}

LISPFUNN(proclaim_constant,2)
# (SYS::%PROCLAIM-CONSTANT symbol value) erklärt ein Symbol zu einer Konstanten
# und ihm einen Wert zu.
  { var reg2 object val = popSTACK();
    var reg1 object symbol = popSTACK();
    if (!symbolp(symbol)) { fehler_symbol(symbol); }
    set_const_flag(TheSymbol(symbol)); # symbol zu einer Konstanten machen
    Symbol_value(symbol) = val; # ihren Wert setzen
    value1 = symbol; mv_count=1; # symbol als Wert
  }

LISPFUN(get,2,1,norest,nokey,0,NIL)
# (GET symbol key [not-found]), CLTL S. 164
  { var reg2 object symbol = STACK_2;
    if (!symbolp(symbol)) { fehler_symbol(symbol); }
   {var reg1 object result = get(symbol,STACK_1); # suchen
    if (eq(result,unbound)) # nicht gefunden?
      { result = STACK_0; # Defaultwert ist not-found
        if (eq(result,unbound)) # Ist der nicht angegeben,
          { result = NIL; } # dann NIL.
      }
    value1 = result; mv_count=1;
    skipSTACK(3);
  }}

LISPFUN(getf,2,1,norest,nokey,0,NIL)
# (GETF place key [not-found]), CLTL S. 166
  { var reg1 object plistr = STACK_2;
    var reg2 object key = STACK_1;
    loop
      { if (atomp(plistr)) goto notfound;
        if (eq(Car(plistr),key)) goto found;
        plistr = Cdr(plistr);
        if (atomp(plistr)) goto odd;
        plistr = Cdr(plistr);
      }
    found: # key gefunden
      plistr = Cdr(plistr);
      if (atomp(plistr)) goto odd;
      value1 = Car(plistr); mv_count=1; skipSTACK(3); return;
    odd: # Property-Liste hat ungerade Länge
    { pushSTACK(STACK_2);
      pushSTACK(S(getf));
      fehler(
             DEUTSCH ? "~: Die Property-Liste ~ hat ungerade Länge." :
             ENGLISH ? "~: the property list ~ has an odd length" :
             FRANCAIS ? "~ : La liste de propriétés ~ est de longueur impaire." :
             ""
            );
    }
    notfound: # key nicht gefunden
      if (eq( value1 = STACK_0, unbound)) # Defaultwert ist not-found
        { value1 = NIL; } # Ist der nicht angegeben, dann NIL.
      mv_count=1; skipSTACK(3); return;
  }

LISPFUNN(get_properties,2)
# (GET-PROPERTIES place keylist), CLTL S. 167
  { var reg4 object keylist = popSTACK();
    var reg5 object plist = popSTACK();
    var reg3 object plistr = plist;
    loop
      { if (atomp(plistr)) goto notfound;
       {var reg2 object item = Car(plistr);
        var reg1 object keylistr = keylist;
        while (consp(keylistr))
          { if (eq(item,Car(keylistr))) goto found;
            keylistr = Cdr(keylistr);
          }
        plistr = Cdr(plistr);
        if (atomp(plistr)) goto odd;
        plistr = Cdr(plistr);
      }}
    found: # key gefunden
      value3 = plistr; # Dritter Wert = Listenrest
      value1 = Car(plistr); # Erster Wert = gefundener Key
      plistr = Cdr(plistr);
      if (atomp(plistr)) goto odd;
      value2 = Car(plistr); # Zweiter Wert = Wert zum Key
      mv_count=3; return; # Drei Werte
    odd: # Property-Liste hat ungerade Länge
    { pushSTACK(plist);
      pushSTACK(S(get_properties));
      fehler(
             DEUTSCH ? "~: Die Property-Liste ~ hat ungerade Länge." :
             ENGLISH ? "~: the property list ~ has an odd length" :
             FRANCAIS ? "~ : La liste de propriétés ~ est de longueur impaire." :
             ""
            );
    }
    notfound: # key nicht gefunden
      value1 = value2 = value3 = NIL; mv_count=3; return; # alle 3 Werte NIL
  }

LISPFUNN(putplist,2)
# (SYS::%PUTPLIST symbol list) == (SETF (SYMBOL-PLIST symbol) list)
  { var reg2 object list = popSTACK();
    var reg1 object symbol = popSTACK();
    if (!symbolp(symbol)) { fehler_symbol(symbol); }
    value1 = Symbol_plist(symbol) = list; mv_count=1;
  }

LISPFUNN(put,3)
# (SYS::%PUT symbol key value) == (SETF (GET symbol key) value)
  { { var reg3 object symbol = STACK_2;
      if (!symbolp(symbol)) { fehler_symbol(symbol); }
     {var reg2 object key = STACK_1;
      var reg1 object plistr = Symbol_plist(symbol);
      loop
        { if (atomp(plistr)) goto notfound;
          if (eq(Car(plistr),key)) goto found;
          plistr = Cdr(plistr);
          if (atomp(plistr)) goto odd;
          plistr = Cdr(plistr);
        }
      found: # key gefunden
        plistr = Cdr(plistr);
        if (atomp(plistr)) goto odd;
        value1 = Car(plistr) = STACK_0; mv_count=1; # neues value eintragen
        skipSTACK(3); return;
      odd: # Property-Liste hat ungerade Länge
        fehler_plist_odd(symbol);
    }}
    notfound: # key nicht gefunden
    # Property-Liste um 2 Conses erweitern:
    pushSTACK(allocate_cons());
    { var reg2 object cons1 = allocate_cons();
      var reg1 object cons2 = popSTACK();
      value1 = Car(cons2) = popSTACK(); # value
      Car(cons1) = popSTACK(); # key
     {var reg3 object symbol = popSTACK();
      Cdr(cons2) = Symbol_plist(symbol);
      Cdr(cons1) = cons2;
      Symbol_plist(symbol) = cons1;
      mv_count=1; return;
    }}
  }

LISPFUNN(remprop,2)
# (REMPROP symbol indicator), CLTL S. 166
  { var reg3 object key = popSTACK();
    var reg4 object symbol = popSTACK();
    if (!symbolp(symbol)) { fehler_symbol(symbol); }
   {var reg2 object* plistr_ = &Symbol_plist(symbol);
    var reg1 object plistr;
    loop
      { plistr = *plistr_;
        if (atomp(plistr)) goto notfound;
        if (eq(Car(plistr),key)) goto found;
        plistr = Cdr(plistr);
        if (atomp(plistr)) goto odd;
        plistr_ = &Cdr(plistr);
      }
    found: # key gefunden
      plistr = Cdr(plistr);
      if (atomp(plistr)) goto odd;
      *plistr_ = Cdr(plistr); # Property-Liste um 2 Elemente verkürzen
      value1 = T; mv_count=1; return; # Wert T
    odd: # Property-Liste hat ungerade Länge
      fehler_plist_odd(symbol);
    notfound: # key nicht gefunden
      value1 = NIL; mv_count=1; return; # Wert NIL
  }}

LISPFUNN(symbol_package,1)
# (SYMBOL-PACKAGE symbol), CLTL S. 170
  { var reg1 object symbol = popSTACK();
    if (!symbolp(symbol)) { fehler_symbol(symbol); }
    value1 = Symbol_package(symbol); mv_count=1;
  }

LISPFUNN(symbol_plist,1)
# (SYMBOL-PLIST symbol), CLTL S. 166
  { var reg1 object symbol = popSTACK();
    if (!symbolp(symbol)) { fehler_symbol(symbol); }
    value1 = Symbol_plist(symbol); mv_count=1;
  }

LISPFUNN(symbol_name,1)
# (SYMBOL-NAME symbol), CLTL S. 168
  { var reg1 object symbol = popSTACK();
    if (!symbolp(symbol)) { fehler_symbol(symbol); }
    value1 = Symbol_name(symbol); mv_count=1;
  }

LISPFUNN(keywordp,1)
# (KEYWORDP object), CLTL S. 170
  { var reg1 object obj = popSTACK();
    if (symbolp(obj) && keywordp(obj))
      { value1 = T; }
      else
      { value1 = NIL; }
    mv_count=1;
  }

LISPFUNN(special_variable_p,1)
# (SYS::SPECIAL-VARIABLE-P symbol) stellt fest, ob das Symbol eine
# Special-Variable (oder eine Konstante) darstellt.
# (Bei Konstanten ist ja das Special-Bit bedeutungslos.)
  { var reg1 object symbol = popSTACK();
    if (!symbolp(symbol)) { fehler_symbol(symbol); }
    value1 = (constantp(TheSymbol(symbol)) || special_var_p(TheSymbol(symbol))
              ? T : NIL
             );
    mv_count=1;
  }

LISPFUN(gensym,0,1,norest,nokey,0,NIL)
# (GENSYM x), CLTL S. 169
# (let ((gensym-prefix "G") ; ein String
#       (gensym-count 1)) ; ein Integer >=0
#   (defun gensym (&optional (x nil s))
#     (when s
#       (cond ((stringp x) (setq gensym-prefix x))
#             ((integerp x)
#              (if (minusp x)
#                (error #+DEUTSCH "~S: Index ~S ist negativ."
#                       #+ENGLISH "~S: index ~S is negative"
#                       #+FRANCAIS "~S: L'index ~S est négatif."
#                       'gensym x
#                )
#                (setq gensym-count x)
#             ))
#             (t (error #+DEUTSCH "~S: Argument ~S hat falschen Typ"
#                       #+ENGLISH "~S: invalid argument ~S"
#                       #+FRANCAIS "~S: L'argument ~S n'est pas du bon type."
#                       'gensym x
#             )  )
#     ) )
#     (prog1
#       (make-symbol
#         (string-concat
#           gensym-prefix
#           #-CLISP (write-to-string gensym-count :base 10 :radix nil)
#           #+CLISP (sys::decimal-string gensym-count)
#       ) )
#       (setq gensym-count (1+ gensym-count))
# ) ) )
  { var reg1 object x = popSTACK(); # Argument
    if (!eq(x,unbound))
      # x angegeben
      { if (stringp(x))
          { O(gensym_prefix) = x; } # gensym-prefix setzen
        elif (integerp(x))
          { if (R_minusp(x))
              { pushSTACK(x);
                pushSTACK(S(gensym));
                fehler(
                       DEUTSCH ? "~: Index ~ ist negativ." :
                       ENGLISH ? "~: index ~ is negative" :
                       FRANCAIS ? "~ : L'index ~ est négatif." :
                       ""
                      );
              }
            # x ist ein Integer >=0
            O(gensym_count) = x; # gensym-count setzen
          }
        else
          { pushSTACK(x);
            pushSTACK(S(gensym));
            fehler(
                   DEUTSCH ? "~: Argument ~ hat falschen Typ." :
                   ENGLISH ? "~: invalid argument ~" :
                   FRANCAIS ? "~ : L'argument ~ n'est pas du bon type." :
                   ""
                  );
      }   }
    # String zusammenbauen:
    pushSTACK(O(gensym_prefix)); # 1. Teilstring
    pushSTACK(O(gensym_count)); # altes gensym-count
    O(gensym_count) = I_1_plus_I(O(gensym_count)); # (incf gensym-count)
    funcall(L(decimal_string),1); # (sys::decimal-string gensym-count)
    pushSTACK(value1); # 2. String
    value1 = make_symbol(string_concat(2)); # zusammenhängen, Symbol bilden
    mv_count=1; # als Wert
  }

