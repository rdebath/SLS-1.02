# Liste aller FSUBRs
# Bruno Haible 1.2.1993

# Eine Special-Form wird definiert durch eine Deklaration
#   LISPSPECFORM(name,req_anz,opt_anz,body_flag)
# in diesem File.
# Zus‰tzlich muﬂ in CONTROL.C dieselbe Deklaration samt C-Body stehen.

# name ist der Funktionsname (ein C-Identifier), req_anz die Anzahl der
# required-Parameter (eine Zahl), opt_anz die Anzahl der optional-Parameter
# (eine Zahl), body_flag entweder nobody oder body.


# Expander f¸r die Konstruktion der extern-Deklarationen:
  #define LISPSPECFORM_A(name,req_anz,opt_anz,body_flag)  \
    extern fsubr_function C_##name;

# Expander f¸r die Konstruktion der Deklaration der C-Funktion:
  #ifdef TYPENAME_FOR_FUNDEFS_ALLOWED
    #define LISPSPECFORM_B(name,req_anz,opt_anz,body_flag)  \
      global fsubr_function C_##name
  #else
    #ifdef ANSI
      #define LISPSPECFORM_B(name,req_anz,opt_anz,body_flag)  \
        global Values C_##name (void)
    #else
      #define LISPSPECFORM_B(name,req_anz,opt_anz,body_flag)  \
        global Values C_##name ()
    #endif
  #endif

# Expander f¸r die Deklaration der FSUBR-Tabelle:
  #define LISPSPECFORM_C(name,req_anz,opt_anz,body_flag)  \
    fsubr_ D_##name;

# Expander f¸r die Initialisierung der FSUBR-Tabelle:
  #define LISPSPECFORM_D(name_,req_anz_,opt_anz_,body_flag_)  \
    ptr->function = &C_##name_;                 \
    ptr->name = S(name_);                       \
    ptr->argtype = (uintW)fsubr_argtype(req_anz_,opt_anz_,fsubr_##body_flag_); \
    ptr->req_anz = req_anz_;                    \
    ptr->opt_anz = opt_anz_;                    \
    ptr->body_flag = (uintW)fsubr_##body_flag_; \
    ptr++;
  #define LISPSPECFORM_E(name_,req_anz,opt_anz,body_flag)  \
    ptr->name = S(name_); \
    ptr++;
  #define LISPSPECFORM_F(name,req_anz,opt_anz,body_flag)  \
    { &C_##name,                \
      0, # vorl‰ufig            \
      0, # vorl‰ufig            \
      req_anz,                  \
      opt_anz,                  \
      (uintW)fsubr_##body_flag, \
    },
  #define LISPSPECFORM_G(name,req_anz,opt_anz,body_flag)  \
    { &C_##name,                \
      S(name),                  \
      0, # vorl‰ufig            \
      req_anz,                  \
      opt_anz,                  \
      (uintW)fsubr_##body_flag, \
    },

# Welcher Expander benutzt wird, muﬂ vom Hauptfile aus eingestellt werden.
# Default ist   #define LISPSPECFORM LISPSPECFORM_B

# ---------- CONTROL ----------
LISPSPECFORM(eval_when, 1,0,body)
LISPSPECFORM(quote, 1,0,nobody)
LISPSPECFORM(function, 1,1,nobody)
LISPSPECFORM(setq, 0,0,body)
LISPSPECFORM(psetq, 0,0,body)
LISPSPECFORM(progn, 0,0,body)
LISPSPECFORM(prog1, 1,0,body)
LISPSPECFORM(prog2, 2,0,body)
LISPSPECFORM(let, 1,0,body)
LISPSPECFORM(letstern, 1,0,body)
LISPSPECFORM(compiler_let, 1,0,body)
LISPSPECFORM(progv, 2,0,body)
LISPSPECFORM(flet, 1,0,body)
LISPSPECFORM(labels, 1,0,body)
LISPSPECFORM(macrolet, 1,0,body)
LISPSPECFORM(if, 2,1,nobody)
LISPSPECFORM(when, 1,0,body)
LISPSPECFORM(unless, 1,0,body)
LISPSPECFORM(cond, 0,0,body)
LISPSPECFORM(block, 1,0,body)
LISPSPECFORM(return_from, 1,1,nobody)
LISPSPECFORM(tagbody, 0,0,body)
LISPSPECFORM(go, 1,0,nobody)
LISPSPECFORM(multiple_value_list, 1,0,nobody)
LISPSPECFORM(multiple_value_call, 1,0,body)
LISPSPECFORM(multiple_value_prog1, 1,0,body)
LISPSPECFORM(multiple_value_bind, 2,0,body)
LISPSPECFORM(multiple_value_setq, 2,0,nobody)
LISPSPECFORM(catch, 1,0,body)
LISPSPECFORM(unwind_protect, 1,0,body)
LISPSPECFORM(throw, 2,0,nobody)
LISPSPECFORM(declare, 0,0,body)
LISPSPECFORM(the, 2,0,nobody)
LISPSPECFORM(and, 0,0,body)
LISPSPECFORM(or, 0,0,body)
# Weitere FSUBRs auch in INIT.LSP (%EXPAND-...) und im Compiler (c-form)
# vermerken!

