# Funktionen für Records und Structures von CLISP
# Bruno Haible 6.12.1992

#include "lispbibl.c"


# ==============================================================================
# Records allgemein:

# (SYS::%RECORD-REF record index) liefert den Eintrag index in einem record.
# (SYS::%RECORD-STORE record index value) speichert value als Eintrag index
#   in record ab und liefert value.
# (SYS::%RECORD-LENGTH record) liefert die Länge eines record.

# Fehlermeldung
# > STACK_1: Record
# > STACK_0: (fehlerhafter) Index
# > subr_self: Aufrufer (ein SUBR)
  local nonreturning void fehler_index (void);
  local nonreturning void fehler_index()
    { pushSTACK(TheSubr(subr_self)->name); # Funktionsname
      fehler(
             DEUTSCH ? "~: ~ ist kein erlaubter Index für ~." :
             ENGLISH ? "~: ~ is not a valid index into ~" :
             FRANCAIS ? "~ : ~ n'est pas un index valide pour ~." :
             ""
            );
    }

# Fehlermeldung
# > STACK_0: (fehlerhafter) Record
# > subr_self: Aufrufer (ein SUBR)
  local nonreturning void fehler_record (void);
  local nonreturning void fehler_record()
    { pushSTACK(TheSubr(subr_self)->name); # Funktionsname
      fehler(
             DEUTSCH ? "~: ~ ist kein Record." :
             ENGLISH ? "~: ~ is not a record" :
             FRANCAIS ? "~ : ~ n'est pas un «record»." :
             ""
            );
    }

# Überprüfung eines Index auf Typ `(INTEGER 0 (,ARRAY-SIZE-LIMIT))
# > STACK_0: Index
# > STACK_1: Record o.ä. (für Fehlermeldung)
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: Index
  local uintL test_index (void);
  local uintL test_index()
    { if (!mposfixnump(STACK_0)) { fehler_index(); }
      return posfixnum_to_L(STACK_0);
    }

# Unterprogramm für Record-Zugriffsfunktionen:
# > STACK_1: record-Argument
# > STACK_0: index-Argument
# > subr_self: Aufrufer (ein SUBR)
# < STACK: aufgeräumt
# < ergebnis: Adresse des angesprochenen Record-Elements
  local object* record_up (void);
  local object* record_up ()
    { # record muß vom Typ Closure/Structure/Stream/OtherRecord sein:
      if (!mrecordp(STACK_1)) { skipSTACK(1); fehler_record(); }
     {var reg2 uintL index = test_index(); # Index holen
      var reg1 object record = STACK_1;
      if (!(index < (uintL)(TheRecord(record)->reclength))) { fehler_index(); } # und prüfen
      skipSTACK(2); # Stack aufräumen
      return &TheRecord(record)->recdata[index]; # Record-Element adressieren
    }}

LISPFUNN(record_ref,2)
# (SYS::%RECORD-REF record index) liefert den Eintrag index in einem record.
  { value1 = *(record_up()); mv_count=1; } # Record-Element als Wert

LISPFUNN(record_store,3)
# (SYS::%RECORD-STORE record index value) speichert value als Eintrag index
#   in record ab und liefert value.
  { var reg3 object value = popSTACK();
    value1 = *(record_up()) = value; mv_count=1; # Record-Element eintragen
  }

LISPFUNN(record_length,1)
# (SYS::%RECORD-LENGTH record) liefert die Länge eines record.
  { # record muß vom Typ Closure/Structure/Stream/OtherRecord sein:
    if (!mrecordp(STACK_0)) { fehler_record(); }
   {var reg1 object record = popSTACK();
    value1 = fixnum((uintL)(TheRecord(record)->reclength)); # Länge als Fixnum
    mv_count=1;
  }}

# ==============================================================================
# Structures:

# (SYS::%STRUCTURE-REF type structure index) liefert zu einer Structure vom
#   gegebenen Typ type (ein Symbol) den Eintrag index>=1.
# (SYS::%STRUCTURE-STORE type structure index object) speichert object als
#   Eintrag index in einer Structure vom gegebenen Typ type und liefert object.
# (SYS::%MAKE-STRUCTURE type length) erzeugt eine Structure mit length>=1
#   Elementen, vom Typ type.
# (SYS::%COPY-STRUCTURE structure) liefert eine Kopie der Structure structure,
#   vom selben Typ.
# (SYS::%STRUCTURE-TYPE-P type object) überprüft, ob object eine
#   Structure ist, die vom Typ type ist, was daran erkennbar ist, daß in
#   der Komponente 0 ein Objekt (name_1 ... name_i-1 . name_i) steht, wobei
#   einer der Namen EQ zu type ist.

# Unterprogramm für Structure-Zugriffsfunktionen:
# > STACK_2: type-Argument
# > STACK_1: structure-Argument
# > STACK_0: index-Argument
# > subr_self: Aufrufer (ein SUBR)
# < STACK: aufgeräumt
# < ergebnis: Adresse des angesprochenen Structure-Elements
  local object* structure_up (void);
  local object* structure_up ()
    { # structure muß vom Typ Structure sein:
      if (!mstructurep(STACK_1))
        { fehler_bad_structure: # STACK_2 = type, STACK_1 = structure
          STACK_0 = TheSubr(subr_self)->name; # Funktionsname
          fehler(
                 DEUTSCH ? "~: ~ ist keine Structure vom Typ ~." :
                 ENGLISH ? "~: ~ is not a structure of type ~" :
                 FRANCAIS ? "~ : ~ n'est pas une structure de type ~." :
                 ""
                );
        }
     {var reg4 uintL index = test_index(); # Index holen
      var reg3 object structure = STACK_1;
      var reg1 object namelist = TheStructure(structure)->structure_types; # erste Komponente
      var reg2 object type = STACK_2; # type-Argument
      # Teste, ob in namelist = (name_1 ... name_i-1 . name_i) type vorkommt:
      while (consp(namelist))
        { if (eq(Car(namelist),type)) goto yes;
          namelist = Cdr(namelist);
        }
      if (eq(namelist,type)) goto yes;
      # type kam nicht vor -> Error:
      goto fehler_bad_structure;
      # type kam vor:
      yes:
      if (!(index < (uintL)(TheStructure(structure)->reclength))) { fehler_index(); } # und prüfen
      skipSTACK(3); # Stack aufräumen
      return &TheStructure(structure)->recdata[index]; # Structure-Komponente adressieren
    }}

LISPFUNN(structure_ref,3)
# (SYS::%STRUCTURE-REF type structure index) liefert zu einer Structure vom
#   gegebenen Typ type (ein Symbol) den Eintrag index>=1.
  { value1 = *(structure_up()); mv_count=1; } # Structure-Element als Wert

LISPFUNN(structure_store,4)
# (SYS::%STRUCTURE-STORE type structure index object) speichert object als
#   Eintrag index in einer Structure vom gegebenen Typ type und liefert object.
  { var reg3 object value = popSTACK();
    value1 = *(structure_up()) = value; mv_count=1; # Structure-Element eintragen
  }

LISPFUNN(make_structure,2)
# (SYS::%MAKE-STRUCTURE type length) erzeugt eine Structure mit length>=1
#   Elementen, vom Typ type.
  { # Länge überprüfen, sollte ein Fixnum /=0 sein, das in ein uintC paßt:
    var reg1 uintL length;
    if (!(mposfixnump(STACK_0)
          && ((length = posfixnum_to_L(STACK_0)) <= (uintL)(bitm(intCsize)-1))
          && (length>0)
       ) )
      { pushSTACK(TheSubr(subr_self)->name); # Funktionsname
        fehler(
               DEUTSCH ? "~: ~ ist nicht als Länge zugelassen, da nicht vom Typ (INTEGER (0) (65536))." :
               ENGLISH ? "~: length ~ is illegal, should be of type (INTEGER (0) (65536))" :
               FRANCAIS ? "~ : ~ n'est pas permis comme longueur parce qu'il faut le type (INTEGER (0) (65536))." :
               ""
              );
      }
    skipSTACK(1);
   {var reg2 object structure = allocate_structure(length);
    # neue Structure, mit NILs gefüllt
    TheStructure(structure)->structure_types = popSTACK(); # Typ-Komponente eintragen
    value1 = structure; mv_count=1; # structure als Wert
  }}

LISPFUNN(copy_structure,1)
# (SYS::%COPY-STRUCTURE structure) liefert eine Kopie der Structure structure,
#   vom selben Typ.
  { if (!(mstructurep(STACK_0)))
      { pushSTACK(TheSubr(subr_self)->name); # Funktionsname
        fehler(
               DEUTSCH ? "~: ~ ist keine Structure." :
               ENGLISH ? "~: ~ is not a structure" :
               FRANCAIS ? "~ : ~ n'est pas une structure." :
               ""
              );
      }
   {var reg3 uintC length = TheStructure(STACK_0)->reclength;
    var reg4 object new_structure = allocate_structure(length); # neue Structure
    # und füllen:
    {var reg1 object* old_ptr = &TheStructure(popSTACK())->structure_types;
     var reg2 object* new_ptr = &TheStructure(new_structure)->structure_types;
     dotimespC(length,length, { *new_ptr++ = *old_ptr++; });
    }
    # und als Wert zurück:
    value1 = new_structure; mv_count=1;
  }}

LISPFUNN(structure_type_p,2)
# (SYS::%STRUCTURE-TYPE-P type object) überprüft, ob object eine
#   Structure ist, die vom Typ type ist, was daran erkennbar ist, daß in
#   der Komponente 0 ein Objekt (name_1 ... name_i-1 . name_i) steht, wobei
#   einer der Namen EQ zu type ist.
  { # object auf Structure testen:
    if (!(mstructurep(STACK_0))) { skipSTACK(2); goto no; }
   {var reg1 object namelist = TheStructure(popSTACK())->structure_types;
    var reg2 object type = popSTACK();
    # Teste, ob in namelist = (name_1 ... name_i-1 . name_i) type vorkommt:
    while (consp(namelist))
      { if (eq(Car(namelist),type)) goto yes;
        namelist = Cdr(namelist);
      }
    if (eq(namelist,type)) goto yes;
    # type kam nicht vor:
    no: value1 = NIL; mv_count=1; return; # 1 Wert NIL
    # type kam vor:
    yes: value1 = T; mv_count=1; return; # 1 Wert T
  }}

# ==============================================================================
# Closures:

# (SYS::CLOSURE-NAME closure) liefert den Namen einer Closure.
# (SYS::CLOSURE-CODEVEC closure) liefert den Code-Vektor einer compilierten
#   Closure, als Liste von Fixnums >=0, <256.
# (SYS::CLOSURE-CONSTS closure) liefert eine Liste aller Konstanten einer
#   compilierten Closure.
# (SYS::MAKE-CODE-VECTOR list) liefert zu einer Liste von Fixnums >=0, <256
#   einen Simple-Bit-Vector der 8-fachen Länge, der diese Zahlen als Bytes
#   enthält.
# (SYS::%MAKE-CLOSURE name codevec consts) liefert eine Closure mit gegebenem
#   Namen (einem Symbol), gegebenem Code-Vektor (einem Simple-Bit-Vector) und
#   gegebenen weiteren Konstanten.

LISPFUNN(closure_name,1)
# (SYS::CLOSURE-NAME closure) liefert den Namen einer Closure.
  { var reg1 object closure = popSTACK();
    if (!(closurep(closure)))
      { pushSTACK(TheSubr(subr_self)->name); # Funktionsname
        fehler(
               DEUTSCH ? "~: ~ ist keine Closure." :
               ENGLISH ? "~: ~ is not a closure" :
               FRANCAIS ? "~ : ~ n'est pas une fermeture." :
               ""
              );
      }
    value1 = TheClosure(closure)->clos_name; mv_count=1;
  }

# Fehler, wenn Argument keine compilierte Closure
  local nonreturning void fehler_cclosure (object obj);
  local nonreturning void fehler_cclosure(obj)
    var reg1 object obj;
    { pushSTACK(obj);
      pushSTACK(TheSubr(subr_self)->name); # Funktionsname
      fehler(
             DEUTSCH ? "~: Das ist keine compilierte Closure: ~" :
             ENGLISH ? "~: This is not a compiled closure: ~" :
             FRANCAIS ? "~ : Ceci n'est pas un fermeture compilée : ~" :
             ""
            );
    }

LISPFUNN(closure_codevec,1)
# (SYS::CLOSURE-CODEVEC closure) liefert den Code-Vektor einer compilierten
#   Closure, als Liste von Fixnums >=0, <256.
  { var reg3 object closure = popSTACK();
    if (!(cclosurep(closure))) fehler_cclosure(closure);
   {var reg2 object codevec = TheCclosure(closure)->clos_codevec;
    var reg1 uintL index = (TheSbvector(codevec)->length)/8; # index := Länge in Bytes
    # Codevektor codevec von hinten durchgehen und Bytes auf eine Liste pushen:
    pushSTACK(codevec); # Codevektor
    pushSTACK(NIL); # Liste := ()
    until (index==0)
      { index--; # Index decrementieren
        # neues Cons vor die Liste setzen:
       {var reg1 object new_cons = allocate_cons();
        Cdr(new_cons) = popSTACK();
        Car(new_cons) = fixnum((uintL)(TheSbvector(STACK_0)->data[index])); # Byte herausholen
        pushSTACK(new_cons);
      }}
    value1 = STACK_0; mv_count=1; skipSTACK(2); # Liste als Wert
  }}

LISPFUNN(closure_consts,1)
# (SYS::CLOSURE-CONSTS closure) liefert eine Liste aller Konstanten einer
#   compilierten Closure.
  { var reg2 object closure = popSTACK();
    if (!(cclosurep(closure))) fehler_cclosure(closure);
    # Elemente 2,3,... zu einer Liste zusammenfassen:
   {var reg1 uintC index = (TheCclosure(closure)->reclength)-2; # index := Länge
    # Closure von hinten durchgehen und Konstanten auf eine Liste pushen:
    pushSTACK(closure); # Closure
    pushSTACK(NIL); # Liste := ()
    until (index==0)
      { index--; # Index decrementieren
        # neues Cons vor die Liste setzen:
       {var reg1 object new_cons = allocate_cons();
        Cdr(new_cons) = popSTACK();
        Car(new_cons) = TheCclosure(STACK_0)->clos_consts[(uintL)index]; # Konstante herausholen
        pushSTACK(new_cons);
      }}
    value1 = STACK_0; mv_count=1; skipSTACK(2); # Liste als Wert
  }}

LISPFUNN(make_code_vector,1)
# (SYS::MAKE-CODE-VECTOR list) liefert zu einer Liste von Fixnums >=0, <256
#   einen Simple-Bit-Vector der 8-fachen Länge, der diese Zahlen als Bytes
#   enthält.
  { var reg4 object bv = allocate_bit_vector(8*llength(STACK_0)); # Simple-Bit-Vektor
    # füllen:
    var reg1 object listr = popSTACK(); # Liste
    var reg3 uintB* ptr = &TheSbvector(bv)->data[0]; # läuft durch den Bit-Vektor
    while (consp(listr))
      { var reg2 uintL byte;
        # Listenelement muß ein Fixnum >=0, <256 sein:
        if (!(mposfixnump(Car(listr))
              && ((byte = posfixnum_to_L(Car(listr))) < (1<<intBsize))
           ) )
          goto bad_byte;
        # in den Bit-Vektor stecken:
        *ptr++ = (uintB)byte;
        listr = Cdr(listr);
      }
    value1 = bv; mv_count=1; return; # bv als Wert
    bad_byte:
      pushSTACK(Car(listr));
      fehler(
             DEUTSCH ? "~ ist als Byte in einem Code-Vektor ungeeignet." :
             ENGLISH ? "~ is not a valid code-vector byte" :
             FRANCAIS ? "~ est inutilisable comme octet dans un «code-vector»." :
             ""
            );
  }

LISPFUNN(make_closure,3)
# (SYS::%MAKE-CLOSURE name codevec consts) liefert eine Closure mit gegebenem
#   Namen (einem Symbol), gegebenem Code-Vektor (einem Simple-Bit-Vector) und
#   gegebenen weiteren Konstanten.
  { # codevec muß ein Simple-Bit-Vector sein:
    if (!(m_simple_bit_vector_p(STACK_1)))
      { # STACK_1 = codevec
        STACK_0 = TheSubr(subr_self)->name;
        fehler(
               DEUTSCH ? "~: Als Code-Vektor einer Funktion ist ~ ungeeignet." :
               ENGLISH ? "~: invalid code-vector ~" :
               FRANCAIS ? "~ : ~ n'est pas utilisable comme «code-vector» d'une fonction." :
               ""
              );
      }
   {# neue Closure der Länge (+ 2 (length consts)) erzeugen:
    var reg3 object closure = allocate_record(0,0,2+llength(STACK_0),closure_type);
    TheCclosure(closure)->clos_name = STACK_2; # Namen einfüllen
    TheCclosure(closure)->clos_codevec = STACK_1; # Codevektor einfüllen
    # Konstanten einfüllen:
    {var reg1 object constsr = popSTACK();
     var reg2 object* ptr = &TheCclosure(closure)->clos_consts[0];
     while (consp(constsr))
       { *ptr++ = Car(constsr); constsr = Cdr(constsr); }
    }
    value1 = closure; mv_count=1; skipSTACK(2);
  }}

# ==============================================================================
# Load-Time-Eval:

# (SYS::MAKE-LOAD-TIME-EVAL form) liefert ein Load-Time-Eval-Objekt, das
#   - wenn ausgegeben und wieder eingelesen - form auswertet.

LISPFUNN(make_load_time_eval,1)
# (SYS::MAKE-LOAD-TIME-EVAL form) liefert ein Load-Time-Eval-Objekt, das
#   - wenn ausgegeben und wieder eingelesen - form auswertet.
  { var reg1 object lte = allocate_loadtimeeval();
    TheLoadtimeeval(lte)->loadtimeeval_form = popSTACK();
    value1 = lte; mv_count=1;
  }

# ==============================================================================

