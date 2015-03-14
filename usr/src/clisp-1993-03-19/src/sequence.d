# Sequences für CLISP
# Bruno Haible 4.1.1993

#include "lispbibl.c"


# O(seq_types) enthält eine Liste von Typdescriptoren für Sequences.
# Das sind Simple-Vektoren der Länge 16, mit folgendem Inhalt:
#  SEQ-TYPE        ; der Typ der Sequence, meist ein Symbol
#  Zugriffsfunktionen:
#  SEQ-INIT
#  SEQ-UPD
#  SEQ-ENDTEST
#  SEQ-FE-INIT
#  SEQ-FE-UPD
#  SEQ-FE-ENDTEST
#  SEQ-ACCESS
#  SEQ-ACCESS-SET
#  SEQ-COPY
#  SEQ-LENGTH
#  SEQ-MAKE
#  SEQ-ELT
#  SEQ-SET-ELT
#  SEQ-INIT-START
#  SEQ-FE-INIT-END

/*

 Erklärung der Einzelfunktionen SEQ-XXX:

Ein "Pointer" ist etwas, was durch die Sequence durchlaufen kann.
Es gibt Pointer, die von links nach rechts laufen;
  sie werden mit INIT oder INIT-START kreiert, mit COPY kopiert,
             mit UPD um eine Stelle weitergerückt,
             mit ENDTEST getestet, ob sie am Ende der Sequence angelangt sind,
             mit ACCESS wird das Element, worauf der Pointer zeigt, geholt,
             mit ACCESS-SET wird das Element, worauf der Pointer zeigt, gesetzt.
Es gibt auch Pointer, die von links nach rechts laufen;
  sie werden mit FE-INIT oder FE-INIT-END kreiert, mit COPY kopiert,
             mit FE-UPD um eine Stelle nach links weitergerückt,
             mit FE-ENDTEST getestet, ob sie am Ende der Sequence angelangt sind,
             mit ACCESS wird das Element, worauf der Pointer zeigt, geholt.
  Für sie funktioniert ACCESS-SET nicht.

Durchlaufe-Operationen:
INIT          (lambda (seq) ...) -> pointer
              liefert den Pointer zu SEQ, der ganz links steht.
UPD           (lambda (seq pointer) ...) -> pointer
              liefert zu einem Pointer den Pointer eins weiter rechts.
              SEQ-UPD kann voraussetzen, daß dabei der rechte Rand von
              SEQ nicht überschritten wird.
ENDTEST       (lambda (seq pointer) ...) -> boolean
              testet, ob dieser Pointer am rechten Rand von SEQ steht.
Dasselbe "FROM END" :
FE-INIT       (lambda (seq) ...) -> pointer
              liefert den Pointer zu SEQ, der ganz rechts steht.
FE-UPD        (lambda (seq pointer) ...) -> pointer
              liefert zu einem Pointer den Pointer eins weiter links.
              SEQ-FE-UPD kann voraussetzen, daß dabei der linke Rand von
              SEQ nicht überschritten wird.
FE-ENDTEST    (lambda (seq pointer) ...) -> boolean
              testet, ob dieser Pointer am linken Rand von SEQ steht.
Zugriff mit Pointer:
ACCESS        (lambda (seq pointer) ...) -> value
              liefert zu einem Pointer in SEQ das entsprechende Element an
              dieser Stelle.
ACCESS-SET    (lambda (seq pointer value) ...) ->
              setzt das Element in SEQ, auf das der Pointer zeigt, auf den
              gegebenen Wert. Nur bei von links nach rechts laufenden Pointern!
COPY          (lambda (pointer) ...) -> pointer
              liefert eine Kopie des Pointers zu SEQ (denn UPD und FE-UPD
              können destruktiv auf den Pointern arbeiten)
Gesamtlänge:
LENGTH        (lambda (seq) ...) -> size
              liefert die (aktive) Länge der Sequence SEQ.
MAKE          (lambda (size) ...) -> sequence
              liefert eine neu allozierte, leere Sequence, die vom Typ
              SEQ-TYPE ist und die angegebene Länge hat.
Zugriff über Index (meist ineffizienter als über Pointer):
ELT           (lambda (seq index) ...) -> value
              liefert (ELT SEQ index)
SET-ELT       (lambda (seq index value) ...) ->
              setzt (ELT SEQ index) auf value.
INIT-START    (lambda (seq index) ...) -> pointer
              liefert einen nach rechts laufenden Pointer in SEQ
              ab Position index. Muß den Range-test selbst durchführen.
FE-INIT-END   (lambda (seq index) ...) -> pointer
              liefert einen nach links laufenden Pointer in SEQ
              an Position index. Muß den Range-test selbst durchführen.

*/

#define seq_type(seqdesc)         (TheSvector(seqdesc)->data[0])
#define seq_init(seqdesc)         (TheSvector(seqdesc)->data[1])
#define seq_upd(seqdesc)          (TheSvector(seqdesc)->data[2])
#define seq_endtest(seqdesc)      (TheSvector(seqdesc)->data[3])
#define seq_fe_init(seqdesc)      (TheSvector(seqdesc)->data[4])
#define seq_fe_upd(seqdesc)       (TheSvector(seqdesc)->data[5])
#define seq_fe_endtest(seqdesc)   (TheSvector(seqdesc)->data[6])
#define seq_access(seqdesc)       (TheSvector(seqdesc)->data[7])
#define seq_access_set(seqdesc)   (TheSvector(seqdesc)->data[8])
#define seq_copy(seqdesc)         (TheSvector(seqdesc)->data[9])
#define seq_length(seqdesc)       (TheSvector(seqdesc)->data[10])
#define seq_make(seqdesc)         (TheSvector(seqdesc)->data[11])
#define seq_elt(seqdesc)          (TheSvector(seqdesc)->data[12])
#define seq_set_elt(seqdesc)      (TheSvector(seqdesc)->data[13])
#define seq_init_start(seqdesc)   (TheSvector(seqdesc)->data[14])
#define seq_fe_init_end(seqdesc)  (TheSvector(seqdesc)->data[15])

# UP: überprüft, ob name ein gültiger Sequence-Typ-Bezeichner ist
# (sonst Error) und liefert den dazugehörigen Typdescriptor.
# valid_type(name)
# > name: Sequence-Typ-Bezeichner
# < ergebnis: dazugehöriger Typdescriptor
# kann GC auslösen
  local object valid_type (object name);
  local object valid_type(name)
    var reg2 object name;
    { # Unsere elementaren Sequence-Typen sind LIST, VECTOR, STRING, BIT-VECTOR.
      # Wir erkennen aber auch gewisse Alias-Namen:
      # - DEFTYPE-defininierte Typen werden expandiert.
      # - ([SIMPLE-]ARRAY [eltype [(dim)]]), (VECTOR [eltype [size]]) ergeben
      #   STRING falls eltype = STRING-CHAR,
      #   BIT-VECTOR falls eltype = BIT,
      #   n [steht für (VECTOR (UNSIGNED-BYTE n))] falls eltype = n BIT,
      #   VECTOR sonst.
      # - (SIMPLE-VECTOR [size]), VECTOR, SIMPLE-VECTOR ergeben VECTOR.
      # - ([SIMPLE-]STRING [size]), [SIMPLE-]STRING ergeben STRING.
      # - ([SIMPLE-]BIT-VECTOR [size]), [SIMPLE-]BIT-VECTOR ergeben BIT-VECTOR.
      # - Zusätzlich (nicht sehr schön): [SIMPLE-]ARRAY ergibt VECTOR.
      reexpand:
      if (symbolp(name))
        { if (eq(name,S(list))) { goto expanded; }
          if (eq(name,S(vector))) { goto expanded; }
          if (eq(name,S(simple_vector))) { name = S(vector); goto expanded; }
          if (eq(name,S(string))) { goto expanded; }
          if (eq(name,S(simple_string))) { name = S(string); goto expanded; }
          if (eq(name,S(bit_vector))) { goto expanded; }
          if (eq(name,S(simple_bit_vector))) { name = S(bit_vector); goto expanded; }
          if (eq(name,S(array)) || eq(name,S(simple_array))) { name = S(vector); goto expanded; }
          # evtl. (get name 'DEFTYPE-EXPANDER) mit Argument (list name) aufrufen:
          {var reg1 object expander = get(name,S(deftype_expander));
           if (!eq(expander,unbound))
             { pushSTACK(expander);
               pushSTACK(name); name = allocate_cons(); Car(name) = popSTACK(); # (list name)
               expander = STACK_0; STACK_0 = name;
               funcall(expander,1); # Expander aufrufen
               name = value1; goto reexpand; # Ergebnis weiterverwenden
          } }
          goto expanded; # sonstige Symbole können DEFSTRUCT-Typen sein
        }
      elif (consp(name))
        { var reg1 object name1 = Car(name);
          if (symbolp(name1))
            { if (nullp(Cdr(name)) || (mconsp(Cdr(name)) && nullp(Cdr(Cdr(name)))))
                { if (eq(name1,S(simple_vector))) { name = S(vector); goto expanded; }
                  if (eq(name1,S(string)) || eq(name1,S(simple_string))) { name = S(string); goto expanded; }
                  if (eq(name1,S(bit_vector)) || eq(name1,S(simple_bit_vector))) { name = S(bit_vector); goto expanded; }
                }
             {var reg3 object name2;
              var reg4 object name3;
              if (nullp(name2=Cdr(name))) { name2 = S(mal); name3 = S(mal); goto try_vector; }
              if (consp(name2))
                { name3=Cdr(name2); name2 = Car(name2);
                  if (nullp(name3)) { name3 = S(mal); goto try_vector; }
                  if (consp(name3) && nullp(Cdr(name3)))
                    { name3 = Car(name3); goto try_vector; }
                }
              if (FALSE)
                { try_vector: # Hier ist name2 = (second name), name3 = (third name), Defaults: *
                  if (eq(name1,S(vector))
                      || (   (eq(name1,S(array)) || eq(name1,S(simple_array)))
                          && (eq(name3,S(mal)) || (consp(name3) && nullp(Cdr(name3))))
                     )   )
                    { var reg3 uintB atype = eltype_code(name2);
                      if (atype==Atype_T) { name = S(vector); goto expanded; } # (VECTOR T)
                      elif (atype==Atype_String_Char) { name = S(string); goto expanded; } # (VECTOR STRING-CHAR)
                      elif (atype==Atype_Bit) { name = S(bit_vector); goto expanded; } # (VECTOR BIT)
                      else { name = fixnum(bit(atype)); goto expanded; } # (VECTOR (UNSIGNED-BYTE n))
             }  }   }
              # evtl. (get name1 'DEFTYPE-EXPANDER) mit Argument name aufrufen:
             {var reg3 object expander = get(name1,S(deftype_expander));
              if (!eq(expander,unbound))
                { pushSTACK(name); funcall(expander,1); # Expander aufrufen
                  name = value1; goto reexpand; # Ergebnis weiterverwenden
        }   }} }
      goto bad_name;
      expanded:
      # SEQ-TYPES-Liste durchgehen:
      { var reg1 object list = O(seq_types);
        while (consp(list))
          { var reg2 object typdescr = Car(list);
            if (eq(name,seq_type(typdescr))) { return typdescr; }
            list = Cdr(list);
      }   }
      bad_name:
      pushSTACK(name);
      fehler(
             DEUTSCH ? "Es gibt keine Sequences vom Typ ~." :
             ENGLISH ? "There are no sequences of type ~" :
             FRANCAIS ? "Il n'existe pas de séquences de type ~." :
             ""
            );
    }

# UP: liefert den Typdescriptor einer Sequence
# get_seq_type(seq)
# > seq: eine Sequence
# < ergebnis: Typdescriptor oder NIL
  local object get_seq_type (object seq);
  local object get_seq_type(seq)
    var reg2 object seq;
    { var reg3 object name;
      if (listp(seq)) { name = S(list); } # Typ LIST
      elif (stringp(seq)) { name = S(string); } # Typ STRING
      elif (bit_vector_p(seq)) { name = S(bit_vector); } # Typ BIT-VECTOR
      elif (typecode(seq)==bvector_type) # Typ n, bedeutet (VECTOR (UNSIGNED-BYTE n))
        { name = fixnum(bit(TheArray(seq)->flags & arrayflags_atype_mask)); }
      elif (vectorp(seq)) { name = S(vector); } # Typ [GENERAL-]VECTOR
      elif (structurep(seq))
        { name = TheStructure(seq)->structure_types; # Structure-Typen-List*e
          while (consp(name)) { name = Cdr(name); } # davon den letzten Typ nehmen
        }
      else return NIL;
      # SEQ-TYPES-Liste durchgehen:
      { var reg1 object list = O(seq_types);
        while (consp(list))
          { var reg2 object typdescr = Car(list);
            if (eq(name,seq_type(typdescr))) { return typdescr; }
            list = Cdr(list);
          }
        return NIL;
    } }

# UP: liefert den Typdescriptor einer Sequence, evtl. Fehlermeldung
# get_valid_seq_type(seq)
# > seq: eine Sequence
# < ergebnis: Typdescriptor
  local object get_valid_seq_type (object seq);
  local object get_valid_seq_type(seq)
    var reg2 object seq;
    { var reg1 object typdescr = get_seq_type(seq); # Typdescriptor bestimmen
      if (!(nullp(typdescr))) { return typdescr; } # gefunden -> OK
      # sonst Fehler melden:
      pushSTACK(seq);
      fehler(
             DEUTSCH ? "Das ist keine Sequence: ~" :
             ENGLISH ? "~ is not a sequence" :
             FRANCAIS ? "~ n'est pas une séquence." :
             ""
            );
    }

# Fehler, wenn Argument kein Integer >=0
  local nonreturning void fehler_posint (object fun, object kw, object obj);
  local nonreturning void fehler_posint(fun,kw,obj)
    var reg3 object fun;
    var reg2 object kw;
    var reg1 object obj;
    { pushSTACK(obj);
      pushSTACK(kw);
      pushSTACK(fun);
      fehler(
             DEUTSCH ? "~: ~ muß ein Integer >=0 sein, nicht ~" :
             ENGLISH ? "~: ~ should be an integer >=0, not ~" :
             FRANCAIS ? "~ : ~ doit être un entier positif ou zéro et non ~" :
             ""
            );
    }

# Macro: Trägt NIL als Defaultwert eines Parameters in den Stack ein:
# default_NIL(par);
  #define default_NIL(par)  \
    if (eq(par,unbound)) { par = NIL; }

# Macro: Trägt 0 als Defaultwert von START in den Stack ein:
# start_default_0(start);
  #define start_default_0(start)  \
    if (eq(start,unbound)) { start = Fixnum_0; }

# Macro: Trägt (SEQ-LENGTH sequence) als Defaultwert von END in den Stack ein:
# end_default_len(end,seq,typdescr);
# kann GC auslösen
  #define end_default_len(end,seq,typdescr)  \
    if (eq(end,unbound) || eq(end,NIL))                   \
      { var object old_subr_self = subr_self; # aktuelles SUBR, nicht GC-gefährdet! \
        var reg1 object lengthfun = seq_length(typdescr); \
        pushSTACK(seq); funcall(lengthfun,1);             \
        end = value1;                                     \
        subr_self = old_subr_self;                        \
      }

# UP: Überprüft START- und END- Argumente
# > subr_self: Aufrufer (ein SUBR)
# > kwptr: kwptr[0] = START-Keyword,
#          kwptr[1] = END-Keyword
# > argptr: *(argptr STACKop 1) = START-Argument,
#           *(argptr STACKop 0) = END-Argument
  local void test_start_end (object* kwptr, object* argptr);
  local void test_start_end(kwptr,argptr)
    var reg4 object* kwptr;
    var reg3 object* argptr;
    { # START-Argument muß ein Integer >= 0 sein:
      var reg2 object start = *(argptr STACKop 1);
      if (!(integerp(start) && positivep(start)))
        { fehler_posint(TheSubr(subr_self)->name,kwptr[0],start); }
      # END-Argument muß ein Integer >= 0 sein:
     {var reg1 object end = *(argptr STACKop 0);
      if (!(integerp(end) && positivep(end)))
        { fehler_posint(TheSubr(subr_self)->name,kwptr[1],end); }
      # Argumente vergleichen:
      if (!(I_I_comp(end,start)>=0)) # end >= start ?
        { # nein -> Fehler melden:
          pushSTACK(end); pushSTACK(kwptr[1]);
          pushSTACK(start); pushSTACK(kwptr[0]);
          pushSTACK(TheSubr(subr_self)->name);
          fehler(
                 DEUTSCH ? "~: ~ = ~ darf ~ = ~ nicht übersteigen." :
                 ENGLISH ? "~: ~ = ~ should not be greater than ~ = ~" :
                 FRANCAIS ? "~ : ~ = ~ ne doit pas excéder ~ = ~." :
                 ""
                );
        }
    }}

# UP: Überprüft START- und END- Argumente (END-Argument evtl. NIL)
# > subr_self: Aufrufer (ein SUBR)
# > kwptr: kwptr[0] = START-Keyword,
#          kwptr[1] = END-Keyword
# > argptr: *(argptr STACKop 1) = START-Argument,
#           *(argptr STACKop 0) = END-Argument
  local void test_start_end_1 (object* kwptr, object* argptr);
  local void test_start_end_1(kwptr,argptr)
    var reg4 object* kwptr;
    var reg3 object* argptr;
    { # START-Argument muß ein Integer >= 0 sein:
      var reg2 object start = *(argptr STACKop 1);
      if (!(integerp(start) && positivep(start)))
        { fehler_posint(TheSubr(subr_self)->name,kwptr[0],start); }
      # END-Argument muß NIL oder ein Integer >= 0 sein:
     {var reg1 object end = *(argptr STACKop 0);
      if (nullp(end)) { return; } # end=NIL -> OK, fertig
      if (!(integerp(end) && positivep(end)))
        { fehler_posint(TheSubr(subr_self)->name,kwptr[1],end); }
      # Argumente vergleichen:
      if (!(I_I_comp(end,start)>=0)) # end >= start ?
        { # nein -> Fehler melden:
          pushSTACK(end); pushSTACK(kwptr[1]);
          pushSTACK(start); pushSTACK(kwptr[0]);
          pushSTACK(TheSubr(subr_self)->name);
          fehler(
                 DEUTSCH ? "~: ~ = ~ darf ~ = ~ nicht übersteigen." :
                 ENGLISH ? "~: ~ = ~ should not be greater than ~ = ~" :
                 FRANCAIS ? "~ : ~ = ~ ne doit pas excéder ~ = ~." :
                 ""
                );
        }
    }}

# Macro: Incrementiert eine Integer-Variable (im Stack).
# increment(var)
# > var: alter Wert
# < var: neuer Wert
# < ergebnis: neuer Wert
# kann GC auslösen
  #define increment(var)  (var = I_1_plus_I(var)) # var := (1+ var)

# Macro: Decrementiert eine Integer-Variable (im Stack).
# decrement(var)
# > var: alter Wert
# < var: neuer Wert
# < ergebnis: neuer Wert
# kann GC auslösen
  #define decrement(var)  (var = I_minus1_plus_I(var)) # var := (1- var)

# Macro: Rückt einen Vorwärts-Pointer (im Stack) weiter.
# pointer_update(pointer,sequence,typdescr);
# pointer muß von der Form STACK_i sein!
# kann GC auslösen
  #define pointer_update(pointer,sequence,typdescr)  \
    { var reg1 object updatefun = seq_upd(typdescr);     \
      pushSTACK(sequence); # sequence                    \
      pushSTACK(*(&(pointer) STACKop 1)); # pointer      \
      funcall(updatefun,2); # (SEQ-UPD sequence pointer) \
      pointer = value1; # =: pointer                     \
    }

# Macro: Rückt einen Rückwärts-Pointer (im Stack) weiter.
# pointer_fe_update(pointer,sequence,typdescr);
# pointer muß von der Form STACK_i sein!
# kann GC auslösen
  #define pointer_fe_update(pointer,sequence,typdescr)  \
    { var reg1 object updatefun = seq_fe_upd(typdescr);     \
      pushSTACK(sequence); # sequence                       \
      pushSTACK(*(&(pointer) STACKop 1)); # pointer         \
      funcall(updatefun,2); # (SEQ-FE-UPD sequence pointer) \
      pointer = value1; # =: pointer                        \
    }

# UP: kopiert einen Teil einer Sequence in eine andere Sequence.
# > STACK_6: sequence1
# > STACK_5: typdescr1
# > STACK_4: sequence2
# > STACK_3: typdescr2
# > STACK_2: count (ein Integer >=0)
# > STACK_1: pointer1
# > STACK_0: pointer2
# kopiert count Elemente von sequence1 nach sequence2 und rückt dabei
# pointer1 und pointer2 um count Stellen weiter (mit SEQ-UPD), setzt count:=0.
# kann GC auslösen
  local void copy_seqpart_into (void);
  local void copy_seqpart_into()
    { # Methode etwa so:
      # (loop
      #   (when (zerop count) (return))
      #   (SEQ2-ACCESS-SET sequence2 pointer2 (SEQ1-ACCESS sequence1 pointer1))
      #   (setq pointer1 (SEQ1-UPD pointer1))
      #   (setq pointer2 (SEQ2-UPD pointer2))
      #   (decf count)
      # )
      until (eq(STACK_2,Fixnum_0)) # count (ein Integer) = 0 -> Ende
        { # (SEQ1-ACCESS seq1 pointer1) bilden:
          pushSTACK(STACK_(6+0)); # seq1
          pushSTACK(STACK_(1+1)); # pointer1
          funcall(seq_access(STACK_(5+2)),2);
          # (SEQ2-ACCESS-SET seq2 pointer2 ...) ausführen:
          pushSTACK(STACK_(4+0)); # seq2
          pushSTACK(STACK_(0+1)); # pointer2
          pushSTACK(value1);
          funcall(seq_access_set(STACK_(3+3)),3);
          # pointer1 := (SEQ1-UPD seq1 pointer1) :
          pointer_update(STACK_1,STACK_6,STACK_5);
          # pointer2 := (SEQ2-UPD seq2 pointer2) :
          pointer_update(STACK_0,STACK_4,STACK_3);
          # count := (1- count) :
          decrement(STACK_2);
        }
    }

LISPFUNN(sequencep,1)
# (SYS::SEQUENCEP object) testet, ob object eine Sequence ist.
  { var reg1 object typdescr = get_seq_type(popSTACK()); # Typdescriptor oder NIL
    value1 = (!(nullp(typdescr)) ? T : NIL); mv_count=1;
  }

LISPFUNN(defseq,1)
# (SYSTEM::%DEFSEQ typdescr) erweitert die Liste der Sequencetypen um
# typdescr (muß ein Simple-Vector der Länge 16 sein).
  { # (list typdescr) bilden:
    var reg1 object new_cons = allocate_cons();
    Car(new_cons) = STACK_0;
    # (nconc SEQ_TYPES (list typdescr)) bilden:
    Cdr(new_cons) = nreverse(O(seq_types)); # (nreverse SEQ_TYPES)
    O(seq_types) = nreverse(new_cons);
    # Typ (als Symbol) zurück:
    value1 = seq_type(popSTACK()); mv_count=1;
  }

LISPFUNN(elt,2) # (ELT sequence index), CLTL S. 248
  { # sequence überprüfen:
    var reg1 object typdescr = get_valid_seq_type(STACK_1);
    # index überprüfen:
    if (!(mposfixnump(STACK_0)))
      { pushSTACK(STACK_0); pushSTACK(S(elt));
        fehler(
               DEUTSCH ? "~: Der Index muß ein Fixnum >=0 sein, nicht ~" :
               ENGLISH ? "~: the index should be a fixnum >=0, not ~" :
               FRANCAIS ? "~ : L'index doit être de type FIXNUM positif ou zéro et non ~" :
               ""
              );
      }
    # SEQ-ELT aufrufen:
    funcall(seq_elt(typdescr),2); # (SEQ-ELT sequence index)
    # value1 als Wert
  }

LISPFUNN(setelt,3) # (SYSTEM::%SETELT sequence index value), vgl. CLTL S. 248
  { # sequence überprüfen:
    var reg1 object typdescr = get_valid_seq_type(STACK_2);
    # index überprüfen:
    if (!(mposfixnump(STACK_1)))
      { pushSTACK(STACK_1); pushSTACK(S(elt)); pushSTACK(S(setf));
        fehler(
               DEUTSCH ? "~ ~: Der Index muß ein Fixnum >=0 sein, nicht ~" :
               ENGLISH ? "~ ~: the index should be a fixnum >=0, not ~" :
               FRANCAIS ? "~ ~ : L'index doit être de type FIXNUM positif ou zéro et non ~" :
               ""
              );
      }
    # SEQ-SET-ELT aufrufen:
    pushSTACK(STACK_(2+0)); # sequence
    pushSTACK(STACK_(1+1)); # index
    pushSTACK(STACK_(0+2)); # value
    funcall(seq_set_elt(typdescr),3); # (SEQ-SET-ELT sequence index value)
    value1 = popSTACK(); mv_count=1; # value als Wert
    skipSTACK(2);
  }

# UP: Kopiert ein sequence1 - Teilstück in sequence2 hinein
# und liefert sequence2 als Wert.
# copy_seqpart_onto()
# > Stackaufbau: seq1, typdescr1, seq2, typdescr2, count, pointer1
# < STACK: aufgeräumt
# < Wert: gefüllte seq2
  local Values copy_seqpart_onto (void);
  local Values copy_seqpart_onto()
    { # Stackaufbau: seq1, typdescr1, seq2, typdescr2, count, pointer1.
      pushSTACK(STACK_3); funcall(seq_init(STACK_(2+1)),1); # (SEQ2-INIT seq2)
      pushSTACK(value1);
      # Stackaufbau: seq1, typdescr1, seq2, typdescr2, count, pointer1, pointer2.
      copy_seqpart_into(); # Teilstück von seq1 nach seq2 kopieren
      value1 = STACK_4; mv_count=1; # seq2 als Wert
      skipSTACK(7);
    }

# UP: Liefert ein neu alloziertes sequence-Teilstück als Wert.
# subseq()
# > Stackaufbau: sequence, start, end, typdescr,
#   mit überprüften Argumenten (start,end Integers >=0, start<=end)
# < STACK: aufgeräumt
# < Wert: Kopie des angegebenen Teilstücks von sequence
  local Values subseq (void);
  local Values subseq()
    { STACK_1 = I_I_minus_I(STACK_1,STACK_2); # count := (- end start)
      # Stackaufbau: sequence, start, count, typdescr.
      pushSTACK(STACK_1); funcall(seq_make(STACK_(0+1)),1); # (SEQ-MAKE count)
     {var reg3 object start = STACK_2;
      var reg2 object typdescr = STACK_0;
      STACK_2 = typdescr;
      pushSTACK(STACK_1);
      STACK_2 = value1;
      # Stackaufbau: sequence, typdescr, seq2, typdescr, count.
      pushSTACK(STACK_4); pushSTACK(start); funcall(seq_init_start(typdescr),2);
      pushSTACK(value1); # (SEQ-INIT-START sequence start)
      # Stackaufbau; seq1, typdescr, seq2, typdescr, count, pointer1.
      return_Values copy_seqpart_onto(); # kopieren, seq2 als Wert
    }}

LISPFUN(subseq,2,1,norest,nokey,0,NIL)
# (SUBSEQ sequence start &optional end), CLTL S. 248
  { # Stackaufbau: sequence, start, end.
    # sequence überprüfen:
    var reg1 object typdescr = get_valid_seq_type(STACK_2);
    pushSTACK(typdescr);
    # Stackaufbau: sequence, start, end, typdescr.
    # Defaultwert für end ist (length sequence):
    if (eq(STACK_1,unbound))
      { var reg3 object old_subr_self = subr_self; # aktuelles SUBR, nicht GC-gefährdet!
        # end nicht angegeben -> muß end:=(length sequence) setzen:
        pushSTACK(STACK_3); funcall(seq_length(typdescr),1); # (SEQ-LENGTH sequence)
        STACK_1 = value1;
        subr_self = old_subr_self;
      }
    # Stackaufbau: sequence, start, end, typdescr.
    # Start- und End-Argumente überprüfen:
    test_start_end(&O(kwpair_start),&STACK_1);
    # Teilstück bilden:
    return_Values subseq();
  }

# UP: Kopiert sequence1 in sequence2 hinein und liefert sequence2 als Wert.
# copy_seq_onto()
# > Stackaufbau: seq1, typdescr1, seq2, typdescr2, len
# < STACK: aufgeräumt
# < Wert: gefüllte seq2
  local Values copy_seq_onto (void);
  local Values copy_seq_onto()
    { # Stackaufbau: seq1, typdescr1, seq2, typdescr2, len.
      pushSTACK(STACK_4); funcall(seq_init(STACK_(3+1)),1); # (SEQ1-INIT seq1)
      pushSTACK(value1);
      # Stackaufbau: seq1, typdescr1, seq2, typdescr2, len, pointer1.
      return_Values copy_seqpart_onto();
    }

LISPFUNN(copy_seq,1) # (COPY-SEQ sequence), CLTL S. 248
  { # Stackaufbau: sequence.
    # sequence überprüfen:
    var reg1 object typdescr = get_valid_seq_type(STACK_0);
    pushSTACK(typdescr);
    # Stackaufbau: sequence, typdescr.
    pushSTACK(STACK_1); funcall(seq_length(typdescr),1);
    pushSTACK(value1); # (SEQ-LENGTH sequence)
    # Stackaufbau: sequence, typdescr, len.
    pushSTACK(STACK_0); funcall(seq_make(STACK_(1+1)),1); # (SEQ-MAKE len)
    pushSTACK(STACK_1); pushSTACK(STACK_(0+1)); STACK_2 = value1;
    # Stackaufbau: seq1, typdescr, seq2, typdescr, len.
    return_Values copy_seq_onto();
  }

LISPFUNN(length,1) # (LENGTH sequence), CLTL S. 248
  { var reg1 object arg = popSTACK();
    if (consp(arg))
      { # arg ist ein Cons
        value1 = fixnum(llength(arg)); # Listenlänge als Fixnum
        mv_count=1;
        return;
      }
    elif (symbolp(arg))
      { # arg ist ein Symbol
        if (nullp(arg))
          { value1 = Fixnum_0; mv_count=1; # NIL hat als Liste die Länge 0
            return;
      }   } # sonstige Symbole sind keine Sequences
    elif (vectorp(arg))
      { # arg ist ein Vektor
        value1 = fixnum(vector_length(arg)); # Vektorlänge als Fixnum
        mv_count=1;
        return;
      }
    else
      { # arg ist weder eine Liste noch ein Vektor
        var reg2 object typdescr = get_valid_seq_type(arg); # hier evtl. Fehlermeldung
        # sonstige Sequences:
        pushSTACK(arg); funcall(seq_length(typdescr),1); # (SEQ-LENGTH arg) aufrufen
        return;
      }
    # arg ist keine Sequence
    pushSTACK(arg); pushSTACK(S(length));
    fehler(
           DEUTSCH ? "~: ~ ist keine Sequence." :
           ENGLISH ? "~: ~ is not a sequence" :
           FRANCAIS ? "~ : ~ n'est pas une séquence." :
           ""
          );
  }

LISPFUNN(reverse,1) # (REVERSE sequence), CLTL S. 248
  { var reg1 object arg = STACK_0;
    if (listp(arg))
      { # arg ist eine Liste
        value1 = reverse(arg); mv_count=1; skipSTACK(1);
      }
      else
      { var reg2 object typdescr = get_valid_seq_type(arg);
        # arg ist eine sonstige Sequence
        pushSTACK(typdescr);
        # Stackaufbau: seq1, typdescr.
        pushSTACK(arg); funcall(seq_length(typdescr),1); # (SEQ-LENGTH seq1)
        pushSTACK(value1);
        # Stackaufbau: seq1, typdescr, len.
        pushSTACK(STACK_0); funcall(seq_make(STACK_(1+1)),1); # (SEQ-MAKE len)
        pushSTACK(value1);
        # Stackaufbau: seq1, typdescr, count, seq2.
        pushSTACK(STACK_3); funcall(seq_fe_init(STACK_(2+1)),1); # (SEQ-FE-INIT seq1)
        pushSTACK(value1);
        # Stackaufbau: seq1, typdescr, count, seq2, pointer1.
        pushSTACK(STACK_1); funcall(seq_init(STACK_(3+1)),1); # (SEQ-INIT seq2)
        pushSTACK(value1);
        # Stackaufbau: seq1, typdescr, count, seq2, pointer1, pointer2.
        until (eq(STACK_3,Fixnum_0)) # count (ein Integer) = 0 -> Ende
          { # (SEQ-ACCESS seq1 pointer1) bilden:
            pushSTACK(STACK_5); pushSTACK(STACK_(1+1));
            funcall(seq_access(STACK_(4+2)),2); # (SEQ-ACCESS seq1 pointer1)
            # (SEQ-ACCESS-SET seq2 pointer2 ...) ausführen:
            pushSTACK(STACK_2); pushSTACK(STACK_(0+1)); pushSTACK(value1);
            funcall(seq_access_set(STACK_(4+3)),3); # (SEQ-ACCESS-SET seq2 pointer2 ...)
            # pointer1 := (SEQ-FE-UPD seq1 pointer1) :
            pointer_fe_update(STACK_1,STACK_5,STACK_4);
            # pointer2 := (SEQ-UPD seq2 pointer2) :
            pointer_update(STACK_0,STACK_2,STACK_4);
            # count := (1- count) :
            decrement(STACK_3);
          }
        value1 = STACK_2; mv_count=1; # seq2 als Wert
        skipSTACK(6);
  }   }

LISPFUNN(nreverse,1) # (NREVERSE sequence), CLTL S. 248
  { var reg1 object seq = STACK_0;
    if (listp(seq))
      { # seq ist eine Liste
        value1 = nreverse(seq); mv_count=1;
        skipSTACK(1);
      }
    elif (vectorp(seq))
      { # seq ist ein Vektor
        var reg2 object typdescr = get_valid_seq_type(seq);
        pushSTACK(typdescr);
        # Stackaufbau: seq, typdescr.
        pushSTACK(seq); funcall(seq_length(typdescr),1); # (SEQ-LENGTH seq)
        { var reg3 object len = value1;
          var reg4 object len2 = I_I_ash_I(len,Fixnum_minus1);
          pushSTACK(len2); # (ASH len -1) = (FLOOR len 2)
        }
        # Stackaufbau: seq, typdescr, count.
        pushSTACK(STACK_2); funcall(seq_init(STACK_(1+1)),1); # (SEQ-INIT seq)
        pushSTACK(value1);
        # Stackaufbau: seq, typdescr, count, pointer1.
        pushSTACK(STACK_3); funcall(seq_fe_init(STACK_(2+1)),1); # (SEQ-FE-INIT seq)
        pushSTACK(value1);
        # Stackaufbau: seq, typdescr, count, pointer1, pointer2.
        until (eq(STACK_2,Fixnum_0)) # count (ein Integer) = 0 -> Ende
          { # (SEQ-ACCESS seq pointer1) bilden:
            pushSTACK(STACK_4); pushSTACK(STACK_(1+1));
            funcall(seq_access(STACK_(3+2)),2); # (SEQ-ACCESS seq pointer1)
            pushSTACK(value1); # und retten
            # (SEQ-ACCESS seq pointer2) bilden:
            pushSTACK(STACK_(4+1)); pushSTACK(STACK_(0+1+1));
            funcall(seq_access(STACK_(3+1+2)),2); # (SEQ-ACCESS seq pointer2)
            # (SEQ-ACCESS-SET seq pointer1 ...) ausführen:
            pushSTACK(STACK_(4+1)); pushSTACK(STACK_(1+1+1)); pushSTACK(value1);
            funcall(seq_access_set(STACK_(3+1+3)),3); # (SEQ-ACCESS-SET seq pointer1 ...)
            # (SEQ-ACCESS-SET seq pointer2 ...) ausführen:
           {var reg1 object element1 = popSTACK(); # gerettetes ELement
            pushSTACK(STACK_4); pushSTACK(STACK_(0+1)); pushSTACK(element1); }
            funcall(seq_access_set(STACK_(3+3)),3); # (SEQ-ACCESS-SET seq pointer2 ...)
            # pointer1 := (SEQ-UPD seq pointer1) :
            pointer_update(STACK_1,STACK_4,STACK_3);
            # pointer2 := (SEQ-FE-UPD seq pointer2) :
            pointer_fe_update(STACK_0,STACK_4,STACK_3);
            # count := (1- count) :
            decrement(STACK_2);
          }
        skipSTACK(4);
        value1 = popSTACK(); mv_count=1; # modifizierte seq als Wert
      }
    else
      { var reg2 object typdescr = get_valid_seq_type(seq);
        # seq ist eine allgemeine Sequence
        pushSTACK(typdescr);
        # Stackaufbau: seq, typdescr.
        pushSTACK(seq); funcall(seq_length(typdescr),1); # (SEQ-LENGTH seq)
        if (!(posfixnump(value1))) # sollte ein Fixnum >=0 sein
          { pushSTACK(value1); pushSTACK(S(nreverse));
            fehler(
                   DEUTSCH ? "~: Fehlerhafte Länge aufgetreten: ~" :
                   ENGLISH ? "~: bad length ~" :
                   FRANCAIS ? "~ : occurence d'une mauvaise longueur: ~." :
                   ""
                  );
          }
        {var reg9 uintL len = posfixnum_to_L(value1); # len
         # Grundidee: Um eine Sequence mit len Elementen umzudrehen, müssen
         # der linke und der rechte Block mit je floor(len/2) Elementen
         # vertauscht und dann einzeln umgedreht werden (rekursiv!); das
         # mittlere Element (bei ungeradem len) bleibt unverändert.
         # Entrekursivierter Algorithmus:
         # Für j=0,1,2,... sind 2^j mal zwei (fast) adjazente Blöcke
         # der Länge k2=floor(len/2^(j+1)) zu vertauschen.
         var reg8 uintL j = 0; # j := 0
         var reg7 uintL k = len; # k = floor(len/2^j) := len
         var reg6 uintL k2; # k2 = floor(k/2)
         var reg5 uintL k1; # k1 = ceiling(k/2)
         until ((k2 = floor(k,2)) == 0) # k halbiert =0 -> Schleifenende
           { k1 = k - k2; # k1 = (altes k) - (neues k) = ceiling((altes k)/2)
            {var reg4 uintL pstack = 0; # ein Pseudo-Stack
             # Stackaufbau: seq, typdescr.
             pushSTACK(STACK_1); funcall(seq_init(STACK_(0+1)),1); # (SEQ-INIT seq)
             pushSTACK(value1);
             # Stackaufbau: seq, typdescr, pointer1.
             pushSTACK(STACK_2); pushSTACK(fixnum(k1));
             funcall(seq_init_start(STACK_(1+2)),2); # (SEQ-INIT-START seq k1)
             pushSTACK(value1);
             # Stackaufbau: seq, typdescr, pointer1, pointer2.
             # pointer1 und pointer2 laufen gemeinsam durch seq, dabei hat
             # pointer2 einen Vorsprung von k1.
             loop
               { # Zwei Blöcke der Länge k2 = floor(len/2^(j+1)) vertauschen:
                 {var reg2 uintL i = k2; # i:=k2 >0
                  do { # (SEQ-ACCESS seq pointer1) bilden:
                       pushSTACK(STACK_3); pushSTACK(STACK_(1+1));
                       funcall(seq_access(STACK_(2+2)),2); # (SEQ-ACCESS seq pointer1)
                       pushSTACK(value1); # und retten
                       # (SEQ-ACCESS seq pointer2) bilden:
                       pushSTACK(STACK_(3+1)); pushSTACK(STACK_(0+1+1));
                       funcall(seq_access(STACK_(2+1+2)),2); # (SEQ-ACCESS seq pointer2)
                       # (SEQ-ACCESS-SET seq pointer1 ...) ausführen:
                       pushSTACK(STACK_(3+1)); pushSTACK(STACK_(1+1+1)); pushSTACK(value1);
                       funcall(seq_access_set(STACK_(2+1+3)),3); # (SEQ-ACCESS-SET seq pointer1 ...)
                       # (SEQ-ACCESS-SET seq pointer2 ...) ausführen:
                      {var reg1 object element1 = popSTACK(); # gerettetes ELement
                       pushSTACK(STACK_3); pushSTACK(STACK_(0+1)); pushSTACK(element1); }
                       funcall(seq_access_set(STACK_(2+3)),3); # (SEQ-ACCESS-SET seq pointer2 ...)
                       # pointer1 := (SEQ-UPD seq pointer1) :
                       pointer_update(STACK_1,STACK_3,STACK_2);
                       # pointer2 := (SEQ-FE-UPD seq pointer2) :
                       pointer_fe_update(STACK_0,STACK_3,STACK_2);
                       --i; # i:=i-1
                     }
                     until (i==0); # bei i=0 Schleifenende
                 }
                 pstack = pstack+1; # stack:=stack+1
                 if (pstack == (1UL<<j)) break; # stack=2^j geworden -> Schleifenabbruch
                 # pointer1 und pointer2 um k1+(0 oder 1) Stellen weiterrücken:
                 { var reg3 uintL skipcount = k1;
                   { var reg2 uintL r1 = 1;
                     # r := Anzahl der Nullbits am Ende der Dualdarstellung von stack:
                     { var reg1 uintL pstackr = pstack;
                       while ((pstackr & bit(0))==0) { pstackr = pstackr>>1; r1=r1+1; }
                     }
                     # r1 = r+1
                     if (len & bit(j-r1)) # Bit j-r-1 in len gesetzt?
                       { skipcount++; } # falls ja: skipcount=k1+1, sonst skipcount=k1
                   }
                   # skipcount >= k1 >= k2 > 0
                   do { # pointer1 := (SEQ-UPD seq pointer1) :
                        pointer_update(STACK_1,STACK_3,STACK_2);
                        # pointer2 := (SEQ-FE-UPD seq pointer2) :
                        pointer_fe_update(STACK_0,STACK_3,STACK_2);
                        --skipcount;
                      }
                      until (skipcount==0);
               } }
             skipSTACK(2); # pointer1 und pointer2 vergessen
            }
            j=j+1; k=k2; # j:=j+1, k halbieren
        }  }
        skipSTACK(1); # typdescr vergessen
        value1 = popSTACK(); mv_count=1; # modifizierte seq als Wert
      }
  }

LISPFUN(make_sequence,2,0,norest,key,2,\
        (kw(initial_element),kw(update)) )
# (MAKE-SEQUENCE type size [:initial-element] [:update]), CLTL S. 249
# mit zusätzlichem Argument :update, z.B.
# (make-sequence 'vector 5 :initial-element 3 :update #'1+) ==> #(3 4 5 6 7)
  { # Stackaufbau: type, size, initial-element, updatefun.
    # type überprüfen:
    var reg4 object typdescr = valid_type(STACK_3);
    STACK_3 = typdescr;
    # size überprüfen, muß Integer >=0 sein:
   {var reg3 object size = STACK_2;
    if (!(integerp(size) && positivep(size)))
      { pushSTACK(size); pushSTACK(S(make_sequence));
        fehler(
               DEUTSCH ? "~: SIZE muß ein Integer >=0 sein, nicht ~" :
               ENGLISH ? "~: size should be an integer >=0, not ~" :
               FRANCAIS ? "~ : SIZE doit être un entier positif ou zéro et non ~" :
               ""
              );
      }
    # initial-element bei Strings defaultmäßig ergänzen:
    if (eq(STACK_1,unbound)) # :initial-element nicht angegeben?
      { if (!eq(STACK_0,unbound)) # :update ohne :initial-element -> Error
          { pushSTACK(S(make_sequence));
            fehler(
                   DEUTSCH ? "~: :UPDATE darf nur mit :INITIAL-ELEMENT angegeben werden." :
                   ENGLISH ? "~: :update must not be specified without :initial-element" :
                   FRANCAIS ? "~ : :UPDATE ne peut être spécifié qu'avec :INITIAL-ELEMENT." :
                   ""
                  );
          }
        if (eq(seq_type(typdescr),S(string))) # Typname = STRING ?
          { STACK_1 = code_char(' '); } # initial-element := ' '
        elif (mposfixnump(seq_type(typdescr))) # Typname Integer? (bedeutet Byte-Vektoren)
          { STACK_1 = Fixnum_0; } # initial-element := 0
      }
    # Stackaufbau: typdescr, size, initial-element, updatefun.
    pushSTACK(size); funcall(seq_make(typdescr),1); # (SEQ-MAKE size)
   }
    if (!(eq(STACK_1,unbound))) # :initial-element angegeben?
      if (!(eq(STACK_2,Fixnum_0))) # size (ein Integer) = 0 -> nichts zu tun
        { pushSTACK(value1);
          # Stackaufbau: typdescr, count, element, updatefun, seq.
          pushSTACK(STACK_0); funcall(seq_init(STACK_(4+1)),1); # (SEQ-INIT seq)
          pushSTACK(value1);
          # Stackaufbau: typdescr, count, element, updatefun, seq, pointer.
          loop
            { pushSTACK(STACK_(1+0)); pushSTACK(STACK_(0+1)); pushSTACK(STACK_(3+2));
              funcall(seq_access_set(STACK_(5+3)),3); # (SEQ-ACCESS-SET seq pointer element)
              # pointer := (SEQ-UPD seq pointer) :
              pointer_update(STACK_0,STACK_1,STACK_5);
              # count := (1- count) :
              decrement(STACK_4);
              if (eq(STACK_4,Fixnum_0)) break; # count (ein Integer) = 0 -> Schleifenende
              {var reg1 object updatefun = STACK_2;
               if (!(eq(updatefun,unbound))) # falls angegeben,
                 { pushSTACK(STACK_3); funcall(updatefun,1); # (FUNCALL updatefun element)
                   STACK_3 = value1; # =: element
            } }  }
          skipSTACK(1); # pointer vergessen
          value1 = popSTACK(); # seq
        }
    mv_count=1; # seq als Wert
    skipSTACK(4);
  }

# UP: Wandelt ein Objekt in eine Sequence gegebenen Typs um.
# coerce_sequence(obj,result_type)
# > obj: Objekt, sollte eine Sequence sein
# > result_type: Bezeichner (Symbol) des Sequence-Typs
# < Wert: Sequence vom Typ result_type
# kann GC auslösen
  global Values coerce_sequence (object sequence, object result_type);
  global Values coerce_sequence(sequence,result_type)
    var reg4 object sequence;
    var reg3 object result_type;
    { pushSTACK(sequence);
      pushSTACK(result_type);
      { # result-type überprüfen:
        var reg2 object typdescr2 = valid_type(result_type);
        pushSTACK(seq_type(typdescr2)); # neuer type2
        pushSTACK(typdescr2);
        # Stackaufbau: seq1, result-type, type2, typdescr2.
       {var reg1 object typdescr1 = get_valid_seq_type(STACK_3); # Typ von seq1
        if (eq(seq_type(typdescr1),STACK_1))
          { # beide Typen dieselben -> nichts zu tun
            skipSTACK(3); value1 = popSTACK(); mv_count=1; # seq1 als Wert
          }
          else
          { STACK_2 = typdescr1;
            # Stackaufbau: seq1, typdescr1, type2, typdescr2.
            pushSTACK(STACK_3); funcall(seq_length(typdescr1),1); # (SEQ1-LENGTH seq1)
            pushSTACK(value1);
            # Stackaufbau: seq1, typdescr1, type2, typdescr2, len.
            pushSTACK(STACK_0); funcall(seq_make(STACK_(1+1)),1); # (SEQ2-MAKE len)
            STACK_2 = value1;
            # Stackaufbau: seq1, typdescr1, seq2, typdescr2, len.
            return_Values copy_seq_onto();
          }
    } }}

LISPFUN(concatenate,1,0,rest,nokey,0,NIL)
# (CONCATENATE result-type {sequence}), CLTL S. 249
  { var reg5 object* args_pointer = rest_args_pointer;
    # result-type in Typdescriptor umwandeln:
    { var reg1 object type = Before(args_pointer);
      type = valid_type(type);
      BEFORE(args_pointer) = type;
    }
    # args_pointer = Pointer über die Argumente,
    # rest_args_pointer = Pointer über die argcount Sequence-Argumente.
    # Stackaufbau: [args_pointer] typdescr2, [rest_args_pointer] {sequence}, [STACK].
    # Brauche 2*argcount STACK-Einträge:
    get_space_on_STACK(sizeof(object) * 2*(uintL)argcount);
   {var reg3 object* behind_args_pointer = args_end_pointer; # Pointer unter die Argumente
    # Stackaufbau: [args_pointer] typdescr2,
    #              [rest_args_pointer] {sequence}, [behind_args_pointer].
    # Typdescriptoren und Längen bestimmen und im STACK ablegen:
    { var reg3 object* ptr = rest_args_pointer;
      var reg4 uintC count;
      dotimesC(count,argcount,
        { var reg2 object seq = NEXT(ptr); # nächste Sequence
          var reg1 object typdescr = get_valid_seq_type(seq);
          pushSTACK(typdescr); # Typdescriptor in den Stack
          pushSTACK(seq); funcall(seq_length(typdescr),1); # (SEQ-LENGTH seq)
          pushSTACK(value1); # Länge in den Stack
        });
    }
    # Stackaufbau: [args_pointer] typdescr2,
    #              [rest_args_pointer] {sequence},
    #              [behind_args_pointer] {typdescr, len}, [STACK].
    # Längen addieren:
    { var reg2 object total_length = Fixnum_0;
      {var reg3 object* ptr = behind_args_pointer;
       var reg4 uintC count;
       dotimesC(count,argcount,
         { NEXT(ptr); # typdescr überspringen
          {var reg1 object len = NEXT(ptr); # nächste Länge
           if (!(posfixnump(len)))
             { pushSTACK(len); pushSTACK(S(concatenate));
               fehler(
                      DEUTSCH ? "~: Fehlerhafte Länge aufgetreten: ~" :
                      ENGLISH ? "~: bad length ~" :
                      FRANCAIS ? "~ : occurence d'une mauvaise longueur: ~" :
                      ""
                     );
             }
           total_length = I_I_plus_I(total_length,len); # total_length = total_length + len
         }});
      }
      pushSTACK(NIL); pushSTACK(NIL); pushSTACK(NIL); # Dummies
      # neue Sequence allozieren:
      {var reg2 object* ptr = args_pointer;
       var reg1 object typdescr2 = NEXT(ptr);
       pushSTACK(typdescr2);
       pushSTACK(total_length); funcall(seq_make(typdescr2),1); # (SEQ2-MAKE total_length)
       STACK_1 = value1; # =: seq2
    } }
    # Stackaufbau: [args_pointer] typdescr2,
    #              [rest_args_pointer] {sequence},
    #              [behind_args_pointer] {typdescr, len},
    #              NIL, NIL, seq2, typdescr2, [STACK].
    pushSTACK(NIL); pushSTACK(NIL); # Dummies
    # Stackaufbau: [args_pointer] typdescr2,
    #              [rest_args_pointer] {sequence},
    #              [behind_args_pointer] {typdescr, len},
    #              NIL, NIL, seq2, typdescr2, NIL, NIL, [STACK].
    pushSTACK(STACK_(3)); funcall(seq_init(STACK_(2+1)),1); # (SEQ-INIT seq2)
    pushSTACK(value1);
    # Stackaufbau: [args_pointer] typdescr2,
    #              [rest_args_pointer] {sequence},
    #              [behind_args_pointer] {typdescr, len},
    #              NIL, NIL, seq2, typdescr2, NIL, NIL, pointer2, [STACK].
    # Schleife über die argcount Sequences: in seq2 hineinkopieren
    dotimesC(argcount,argcount,
      { STACK_6 = NEXT(rest_args_pointer); # seq1 = nächste Sequence
        STACK_5 = NEXT(behind_args_pointer); # deren typdescr1
        STACK_2 = NEXT(behind_args_pointer); # deren Länge
        pushSTACK(STACK_6); funcall(seq_init(STACK_(5+1)),1); # (SEQ1-INIT seq1)
        STACK_1 = value1; # =: pointer1
        # Stackaufbau: [args_pointer] typdescr2,
        #              [rest_args_pointer] {sequence},
        #              [behind_args_pointer] {typdescr, len},
        #              seq1, typdescr1, seq2, typdescr2, count,
        #              pointer1, pointer2, [STACK].
        copy_seqpart_into(); # ganze seq1 in die seq2 hineinkopieren
      });
    value1 = STACK_4; mv_count=1; # seq2 als Wert
    set_args_end_pointer(args_pointer); # STACK aufräumen
  }}

# UP: führt eine Boolesche Operation mit Prädikat wie SOME oder EVERY aus.
# > Stackaufbau: [args_pointer] ... predicate sequence,
#                [rest_args_pointer] {sequence} [STACK].
# > fun: Routine, die das predicate-Ergebnis abtestet und
#        TRUE liefert (und in value1 ihr Ergebnis hinterläßt),
#        falls vorzeitig herausgesprungen werden soll.
# > argcount: Anzahl der Sequence-Argumente - 1
# > default: Defaultwert am Schluß
# < 1 Wert: wie von fun beim Hinausspringen vorgegeben, oder default.
# < STACK: aufgeräumt (= args_pointer beim Einsprung)
# kann GC auslösen
  typedef boolean seq_boolop_fun (object pred_ergebnis);
  local Values seq_boolop (seq_boolop_fun* boolop_fun,
                           object* args_pointer,
                           object* rest_args_pointer,
                           uintC argcount,
                           object defolt);
  local Values seq_boolop(boolop_fun,args_pointer,rest_args_pointer,argcount,defolt)
    var seq_boolop_fun* boolop_fun;
    var reg10 object* args_pointer;
    var reg8 object* rest_args_pointer;
    var reg9 uintC argcount;
    var object defolt;
    { BEFORE(rest_args_pointer);
      # rest_args_pointer zeigt jetzt über alle argcount+1 Sequence-Argumente
      pushSTACK(defolt); # Defaultwert retten
      # 3*(argcount+1) Plätze auf dem STACK beanspruchen:
      # (2mal für Typdescriptoren und Pointer, 1mal für Funktionsaufruf)
      get_space_on_STACK(sizeof(object)*3*(uintL)(argcount+1));
     {var reg7 object* typdescr_pointer = args_end_pointer; # Pointer über die Typdescriptoren
      # Typdescriptoren und je einen Pointer zu jeder der argcount+1
      # Sequences bestimmen und im STACK ablegen:
      { var reg4 object* ptr = rest_args_pointer;
        var reg3 uintC count;
        dotimespC(count,argcount+1,
          { var reg1 object seq = NEXT(ptr); # nächste Sequence
            var reg2 object typdescr = get_valid_seq_type(seq);
            pushSTACK(typdescr); # Typdescriptor im STACK ablegen
            pushSTACK(seq); funcall(seq_init(typdescr),1); # (SEQ-INIT sequence)
            pushSTACK(value1); # Pointer im STACK ablegen
          });
      }
      # Stackaufbau:
      #         [args_pointer] ... predicate,
      #         [rest_args_pointer] {sequence}, default,
      #         [typdescr_pointer] {typdescr, pointer}, [STACK].
      # Schleife: die Funktion aufrufen:
      loop
        { var reg5 object* ptr1 = rest_args_pointer;
          var reg4 object* ptr2 = typdescr_pointer;
          # ptr1 läuft von oben durch die Sequences durch,
          # ptr2 läuft von oben durch die Typdescr/Pointer durch.
          var reg6 uintC count;
          dotimespC(count,argcount+1,
            { var reg3 object* sequence_ = &NEXT(ptr1);
              var reg2 object* typdescr_ = &NEXT(ptr2);
              var reg1 object* pointer_ = &NEXT(ptr2);
              # (SEQ-ENDTEST sequence pointer) :
              pushSTACK(*sequence_); pushSTACK(*pointer_); funcall(seq_endtest(*typdescr_),2);
              # eine der Sequences zu Ende -> große Schleife beenden:
              if (!(nullp(value1))) goto end_with_default;
              # (SEQ-ACCESS sequence pointer) :
              pushSTACK(*sequence_); pushSTACK(*pointer_); funcall(seq_access(*typdescr_),2);
              # als Argument auf den STACK legen:
              pushSTACK(value1);
              # pointer := (SEQ-UPD sequence pointer) :
              pushSTACK(*sequence_); pushSTACK(*pointer_); funcall(seq_upd(*typdescr_),2);
              *pointer_ = value1;
            });
          # Alle Sequences abgearbeitet.
          # (FUNCALL predicate (SEQ-ACCESS sequence pointer) ...) aufrufen:
          { var reg1 object* ptr = rest_args_pointer;
            var reg2 object predicate = BEFORE(ptr);
            funcall(predicate,argcount+1);
          }
          # Abtestroutine drauf anwenden:
          if ((*boolop_fun)(value1)) goto end_with_value1;
        }
      end_with_default:
        { var reg1 object* ptr = typdescr_pointer;
          value1 = BEFORE(ptr); # default als Wert
        }
      end_with_value1:
        mv_count=1; # 1 Wert
        set_args_end_pointer(args_pointer); # STACK aufräumen
    }}

# Hilfsfunktion für MAP:
  local boolean boolop_nothing (object pred_ergebnis);
  local boolean boolop_nothing(pred_ergebnis)
    var object pred_ergebnis;
    { return FALSE; } # nie vorzeitig zurückkehren

LISPFUN(map,3,0,rest,nokey,0,NIL)
# (MAP result-type function sequence {sequence}), CLTL S. 249
  { var reg10 object* args_pointer = rest_args_pointer STACKop 3;
    # args_pointer = Pointer über die Argumente,
    # rest_args_pointer = Pointer über die argcount weiteren Sequence-Argumente.
    var reg7 object* result_type_ = &Next(args_pointer);
    # result_type_ zeigt in den STACK, auf result-type.
    if (!(nullp(*result_type_)))
      # allgemeines result-type
      { BEFORE(rest_args_pointer);
        # rest_args_pointer zeigt jetzt über alle argcount+1 Sequence-Argumente
        # 4*(argcount+1) Plätze auf dem STACK beanspruchen:
        # (3mal für Typdescriptoren und Pointer, 1mal für Funktionsaufruf)
        get_space_on_STACK(sizeof(object)*4*(uintL)(argcount+1));
        # result-type überprüfen:
        *result_type_ = valid_type(*result_type_);
       {var reg8 object* typdescr_pointer = args_end_pointer; # Pointer über die Typdescriptoren
        # Typdescriptoren und je zwei Pointer zu jeder der argcount+1
        # Sequences bestimmen und im STACK ablegen:
        { var reg4 object* ptr = rest_args_pointer;
          var reg5 uintC count;
          dotimespC(count,argcount+1,
            { var reg1 object* sequence_ = &NEXT(ptr);
              var reg2 object seq = *sequence_; # nächste Sequence
              var reg3 object typdescr = get_valid_seq_type(seq);
              pushSTACK(typdescr); # Typdescriptor im STACK ablegen
              pushSTACK(seq); funcall(seq_init(typdescr),1); # (SEQ-INIT sequence)
              pushSTACK(value1); # Pointer im STACK ablegen
              pushSTACK(*sequence_); funcall(seq_init(STACK_(1+1)),1); # (SEQ-INIT sequence)
              pushSTACK(value1); # Pointer im STACK ablegen
            });
        }
        # Stackaufbau:
        #         [args_pointer] *result_type_ = typdescr2, function,
        #         [rest_args_pointer] {sequence},
        #         [typdescr_pointer] {typdescr, pointer, pointer}, [STACK].
        # Minimale Länge aller Sequences bestimmen, indem jeweils mit dem
        # zweiten Pointer durchgelaufen wird:
        pushSTACK(Fixnum_0); # minlength:=0
        loop
          { var reg5 object* ptr1 = rest_args_pointer;
            var reg4 object* ptr2 = typdescr_pointer;
            # ptr1 läuft von oben durch die Sequences durch,
            # ptr2 läuft von oben durch die Typdescr/Pointer durch.
            var reg6 uintC count;
            dotimespC(count,argcount+1,
              { var reg3 object* sequence_ = &NEXT(ptr1);
                var reg2 object* typdescr_ = &NEXT(ptr2);
                NEXT(ptr2);
               {var reg1 object* pointer_ = &NEXT(ptr2);
                # (SEQ-ENDTEST sequence pointer) :
                pushSTACK(*sequence_); pushSTACK(*pointer_); funcall(seq_endtest(*typdescr_),2);
                # eine der Sequences zu Ende -> große Schleife beenden:
                if (!(nullp(value1))) goto end_found;
                # pointer := (SEQ-UPD sequence pointer) :
                pushSTACK(*sequence_); pushSTACK(*pointer_); funcall(seq_upd(*typdescr_),2);
                *pointer_ = value1;
              }});
            # Keine der Sequences war zu Ende.
            STACK_0 = fixnum_inc(STACK_0,1); # minlength := minlength+1
          }
        end_found:
        # STACK_0 = minimale Länge der Sequences
        # Stackaufbau:
        #         [args_pointer] *result_type_ = typdescr2, function,
        #         [rest_args_pointer] {sequence},
        #         [typdescr_pointer] {typdescr, pointer, pointer},
        #         size [STACK].
        # Neue Sequence der Länge size allozieren:
        pushSTACK(STACK_0); funcall(seq_make(*result_type_),1); # (SEQ2-MAKE size)
        pushSTACK(value1); # seq2 im STACK ablegen
        pushSTACK(STACK_0); funcall(seq_init(*result_type_),1); # (SEQ2-INIT seq2)
        pushSTACK(value1); # pointer2 im STACK ablegen
        # Stackaufbau:
        #         [args_pointer] *result_type_ = typdescr2, function,
        #         [rest_args_pointer] {sequence},
        #         [typdescr_pointer] {typdescr, pointer, pointer},
        #         size, seq2, pointer2 [STACK].
        # size mal die Funktion aufrufen, Ergebnis in seq2 eintragen:
        until (eq(STACK_2,Fixnum_0)) # count (ein Integer) = 0 -> fertig
          { var reg5 object* ptr1 = rest_args_pointer;
            var reg4 object* ptr2 = typdescr_pointer;
            # ptr1 läuft von oben durch die Sequences durch,
            # ptr2 läuft von oben durch die Typdescr/Pointer durch.
            var reg6 uintC count;
            dotimespC(count,argcount+1,
              { var reg3 object* sequence_ = &NEXT(ptr1);
                var reg2 object* typdescr_ = &NEXT(ptr2);
                var reg1 object* pointer_ = &NEXT(ptr2);
                NEXT(ptr2);
                # (SEQ-ACCESS sequence pointer) :
                pushSTACK(*sequence_); pushSTACK(*pointer_); funcall(seq_access(*typdescr_),2);
                # als Argument auf den STACK legen:
                pushSTACK(value1);
                # pointer := (SEQ-UPD sequence pointer) :
                pushSTACK(*sequence_); pushSTACK(*pointer_); funcall(seq_upd(*typdescr_),2);
                *pointer_ = value1;
              });
            # Alle Sequences abgearbeitet.
            # (FUNCALL function (SEQ-ACCESS sequence pointer) ...) aufrufen:
            funcall(*(result_type_ STACKop -1),argcount+1);
            # (SEQ2-ACCESS-SET seq2 pointer2 ...) ausführen:
            pushSTACK(STACK_(1+0)); pushSTACK(STACK_(0+1)); pushSTACK(value1);
            funcall(seq_access_set(*result_type_),3);
            # pointer2 := (SEQ2-UPD seq2 pointer2) :
            pointer_update(STACK_0,STACK_1,*result_type_);
            # size := (1- size) :
            STACK_2 = fixnum_inc(STACK_2,-1);
          }
        value1 = STACK_1; mv_count=1; # seq2 als Wert
        set_args_end_pointer(args_pointer); # STACK aufräumen
      }}
      else
      # result-type = NIL -> viel einfacher:
      # seq_boolop mit boolop_nothing als Funktion und NIL als (Default-)Wert.
      # Dadurch wird function auf alle Elemente der Sequences angewandt.
      return_Values seq_boolop(&boolop_nothing,args_pointer,rest_args_pointer,argcount,NIL);
  }

# Hilfsfunktion für SOME:
  local boolean boolop_some (object pred_ergebnis);
  local boolean boolop_some(pred_ergebnis)
    var reg1 object pred_ergebnis;
    { if (nullp(pred_ergebnis)) # Funktionsergebnis abtesten
        { return FALSE; } # =NIL -> weitersuchen
        else
        { value1 = pred_ergebnis; # /=NIL -> dies als Wert
          return TRUE;
    }   }

LISPFUN(some,2,0,rest,nokey,0,NIL)
# (SOME predicate sequence {sequence}), CLTL S. 250
  { return_Values seq_boolop(&boolop_some,rest_args_pointer STACKop 2,rest_args_pointer,argcount,NIL); }

# Hilfsfunktion für EVERY:
  local boolean boolop_every (object pred_ergebnis);
  local boolean boolop_every(pred_ergebnis)
    var reg1 object pred_ergebnis;
    { if (!(nullp(pred_ergebnis))) # Funktionsergebnis abtesten
        { return FALSE; } # /=NIL -> weitersuchen
        else
        { value1 = pred_ergebnis; # =NIL -> dies (= NIL) als Wert
          return TRUE;
    }   }

LISPFUN(every,2,0,rest,nokey,0,NIL)
# (EVERY predicate sequence {sequence}), CLTL S. 250
  { return_Values seq_boolop(&boolop_every,rest_args_pointer STACKop 2,rest_args_pointer,argcount,T); }

# Hilfsfunktion für NOTANY:
  local boolean boolop_notany (object pred_ergebnis);
  local boolean boolop_notany(pred_ergebnis)
    var reg1 object pred_ergebnis;
    { if (nullp(pred_ergebnis)) # Funktionsergebnis abtesten
        { return FALSE; } # =NIL -> weitersuchen
        else
        { value1 = NIL; # /=NIL -> NIL als Wert
          return TRUE;
    }   }

LISPFUN(notany,2,0,rest,nokey,0,NIL)
# (NOTANY predicate sequence {sequence}), CLTL S. 250
  { return_Values seq_boolop(&boolop_notany,rest_args_pointer STACKop 2,rest_args_pointer,argcount,T); }

# Hilfsfunktion für NOTEVERY:
  local boolean boolop_notevery (object pred_ergebnis);
  local boolean boolop_notevery(pred_ergebnis)
    var reg1 object pred_ergebnis;
    { if (!(nullp(pred_ergebnis))) # Funktionsergebnis abtesten
        { return FALSE; } # /=NIL -> weitersuchen
        else
        { value1 = T; # =NIL -> T als Wert
          return TRUE;
    }   }

LISPFUN(notevery,2,0,rest,nokey,0,NIL)
# (NOTEVERY predicate sequence {sequence}), CLTL S. 250
  { return_Values seq_boolop(&boolop_notevery,rest_args_pointer STACKop 2,rest_args_pointer,argcount,NIL); }

LISPFUN(reduce,2,0,norest,key,4,\
        (kw(from_end),kw(start),kw(end),kw(initial_value)) )
# (REDUCE function sequence [:from-end] [:start] [:end] [:initial-value]),
# CLTL S. 251
  { # Stackaufbau: function, sequence, from-end, start, end, initial-value.
    # sequence überprüfen:
    pushSTACK(get_valid_seq_type(STACK_4));
    # Stackaufbau: function, sequence, from-end, start, end, initial-value,
    #              typdescr.
    # Defaultwert für start ist 0:
    start_default_0(STACK_(2+1));
    # Defaultwert für end ist die Länge der Sequence:
    end_default_len(STACK_(1+1),STACK_(4+1),STACK_0);
    # start- und end-Argumente überprüfen:
    test_start_end(&O(kwpair_start),&STACK_(1+1));
    # start- und end-Argumente subtrahieren und vergleichen:
    { var reg1 object count = I_I_minus_I(STACK_(1+1),STACK_(2+1));
      # count = (- end start), ein Integer >=0.
      if (eq(count,Fixnum_0)) # count = 0 ?
        # start und end sind gleich
        { if (eq(STACK_(0+1),unbound)) # initial-value angegeben?
            { # nein -> function mit 0 Argumenten aufrufen:
              funcall(STACK_(5+1),0);
            }
            else
            { # ja -> initial-value als Wert:
              value1 = STACK_(0+1); mv_count=1;
            }
          skipSTACK(6+1);
          return;
        }
      # allgemeiner Fall: start < end, count > 0
      pushSTACK(count);
    }
    # Stackaufbau: function, sequence, from-end, start, end, initial-value,
    #              typdescr, count.
    # from-end abfragen:
    if (!(eq(STACK_(3+2),unbound)) && !(nullp(STACK_(3+2))))
      # from-end ist angegeben und /=NIL
      { # Durchlauf-Pointer bestimmen:
        pushSTACK(STACK_(4+2)); pushSTACK(STACK_(1+2+1));
        funcall(seq_fe_init_end(STACK_(1+2)),2); # (SEQ-FE-INIT-END seq end)
        pushSTACK(value1); # =: pointer
        # Stackaufbau: function, sequence, from-end, start, end, initial-value,
        #              typdescr, count, pointer.
        # Startwert bestimmen:
        if (eq(STACK_(0+3),unbound))
          # initial-value ist nicht angegeben
          { pushSTACK(STACK_(4+3)); pushSTACK(STACK_(0+1));
            funcall(seq_access(STACK_(2+2)),2); # (SEQ-ACCESS seq pointer)
            pushSTACK(value1); # =: value
            goto into_fromend_loop;
          }
          else
          # initial-value ist angegeben
          { pushSTACK(STACK_(0+3)); } # value := initial-value
        # Stackaufbau: function, seq, from-end, start, end, initial-value,
        #              typdescr, count, pointer, value.
        do { # nächstes value berechnen:
             pushSTACK(STACK_(4+4)); pushSTACK(STACK_(1+1));
             funcall(seq_access(STACK_(3+2)),2); # (SEQ-ACCESS seq pointer)
             pushSTACK(value1); pushSTACK(STACK_(0+1));
             funcall(STACK_(5+4+2),2); # (FUNCALL fun (SEQ-ACCESS seq pointer) value)
             STACK_0 = value1; # =: value
             into_fromend_loop:
             # pointer weiterrücken:
             pointer_fe_update(STACK_1,STACK_(4+4),STACK_3);
             # count := (1- count) :
             decrement(STACK_2);
           }
           until (eq(STACK_2,Fixnum_0)); # count (ein Integer) = 0 ?
        value1 = popSTACK(); mv_count=1; # value als Wert
        skipSTACK(6+3);
      }
      else
      # from-end ist nicht angegeben
      { # Durchlauf-Pointer bestimmen:
        pushSTACK(STACK_(4+2)); pushSTACK(STACK_(2+2+1));
        funcall(seq_init_start(STACK_(1+2)),2); # (SEQ-INIT-START seq start)
        pushSTACK(value1); # =: pointer
        # Stackaufbau: function, sequence, from-end, start, end, initial-value,
        #              typdescr, count, pointer.
        # Startwert bestimmen:
        if (eq(STACK_(0+3),unbound))
          # initial-value ist nicht angegeben
          { pushSTACK(STACK_(4+3)); pushSTACK(STACK_(0+1));
            funcall(seq_access(STACK_(2+2)),2); # (SEQ-ACCESS seq pointer)
            pushSTACK(value1); # =: value
            goto into_fromstart_loop;
          }
          else
          # initial-value ist angegeben
          { pushSTACK(STACK_(0+3)); } # value := initial-value
        # Stackaufbau: function, seq, from-end, start, end, initial-value,
        #              typdescr, count, pointer, value.
        do { # nächstes value berechnen:
             pushSTACK(STACK_(4+4)); pushSTACK(STACK_(1+1));
             funcall(seq_access(STACK_(3+2)),2); # (SEQ-ACCESS seq pointer)
             pushSTACK(STACK_0); pushSTACK(value1);
             funcall(STACK_(5+4+2),2); # (FUNCALL fun value (SEQ-ACCESS seq pointer))
             STACK_0 = value1; # =: value
             into_fromstart_loop:
             # pointer weiterrücken:
             pointer_update(STACK_1,STACK_(4+4),STACK_3);
             # count := (1- count) :
             decrement(STACK_2);
           }
           until (eq(STACK_2,Fixnum_0)); # count (ein Integer) = 0 ?
        value1 = popSTACK(); mv_count=1; # value als Wert
        skipSTACK(6+3);
      }
  }

LISPFUN(fill,2,0,norest,key,2, (kw(start),kw(end)) )
# (FILL sequence item [:start] [:end]), CLTL S. 252
  { # Stackaufbau: sequence, item, start, end.
    # sequence überprüfen:
    pushSTACK(get_valid_seq_type(STACK_3));
    # Stackaufbau: sequence, item, start, end, typdescr.
    # Defaultwert für start ist 0:
    start_default_0(STACK_2);
    # Defaultwert für end ist die Länge der Sequence:
    end_default_len(STACK_1,STACK_4,STACK_0);
    # start- und end-Argumente überprüfen:
    test_start_end(&O(kwpair_start),&STACK_1);
    # start- und end-Argumente subtrahieren:
    STACK_1 = I_I_minus_I(STACK_1,STACK_2); # (- end start), ein Integer >=0
    # Stackaufbau: sequence, item, start, count, typdescr.
    # Durchlauf-Pointer bestimmen:
    pushSTACK(STACK_4); pushSTACK(STACK_(2+1));
    funcall(seq_init_start(STACK_(0+2)),2); # (SEQ-INIT-START sequence start)
    STACK_2 = value1; # =: pointer
    # Stackaufbau: sequence, item, pointer, count, typdescr.
    until (eq(STACK_1,Fixnum_0)) # count (ein Integer) = 0 -> fertig
      { pushSTACK(STACK_4); pushSTACK(STACK_(2+1)); pushSTACK(STACK_(3+2));
        funcall(seq_access_set(STACK_(0+3)),3); # (SEQ-ACCESS-SET sequence pointer item)
        # pointer := (SEQ-UPD sequence pointer) :
        pointer_update(STACK_2,STACK_4,STACK_0);
        # count := (1- count) :
        decrement(STACK_1);
      }
    skipSTACK(4);
    value1 = popSTACK(); mv_count=1; # sequence als Wert
  }

LISPFUN(replace,2,0,norest,key,4,\
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
# (REPLACE sequence1 sequence2 [:start1] [:end1] [:start2] [:end2]),
# CLTL S. 252
  { # Methode (schematisch):
    # Argumente überprüfen.
    # Anzahl der zu kopierenden Elemente bestimmen:
    #   count1 := (- end1 start1), count2 := (- end2 start2).
    #   count1 < count2  ->  count := count1, end2 := (+ start2 count).
    #   count1 > count2  ->  count := count2, #| end1 := (+ start1 count) |# .
    # Nun ist (= count #|(- end1 start1)|# (- end2 start2)).
    # Falls sequence1 und sequence2 EQ sind, die Indexbereiche sich
    # überschneiden (also nicht (or (>= start2 end1) (>= start1 end2)) gilt)
    # und nach oben kopiert werden soll (also (< start2 start1) gilt):
    #   Das Source-Stück aus sequence2 herauskopieren:
    #   (unless (or #|(>= start2 end1)|# (>= start1 end2) (>= start2 start1))
    #     (psetq sequence2 (subseq sequence2 start2 end2)
    #            start2    0
    #         #| end2      count |#
    #   ) )
    # Dann elementweise kopieren: für i=0,1,...
    #   (setf (elt sequence1 (+ start1 i)) (elt sequence2 (+ start2 i))).
    # Stackaufbau: sequence1, sequence2, start1, end1, start2, end2.
    # sequence1 überprüfen:
    pushSTACK(get_valid_seq_type(STACK_5));
    # sequence1 überprüfen:
    pushSTACK(get_valid_seq_type(STACK_(4+1)));
    # Stackaufbau: sequence1, sequence2, start1, end1, start2, end2,
    #              typdescr1, typdescr2.
    # Defaultwert für start1 ist 0:
    start_default_0(STACK_(3+2));
    # Defaultwert für end1 ist die Länge von sequence1:
    end_default_len(STACK_(2+2),STACK_(5+2),STACK_1);
    # Defaultwert für start2 ist 0:
    start_default_0(STACK_(1+2));
    # Defaultwert für end2 ist die Länge von sequence2:
    end_default_len(STACK_(0+2),STACK_(4+2),STACK_0);
    # start- und end-Argumente überprüfen:
    test_start_end(&O(kwpair_start1),&STACK_(2+2));
    test_start_end(&O(kwpair_start2),&STACK_(0+2));
    # count1 bestimmen:
    STACK_(2+2) = I_I_minus_I(STACK_(2+2),STACK_(3+2)); # (- end1 start1) = count1
    # Stackaufbau: sequence1, sequence2, start1, count1, start2, end2,
    #              typdescr1, typdescr2.
    # count2 bestimmen:
   {var reg1 object count2 = I_I_minus_I(STACK_(0+2),STACK_(1+2)); # (- end2 start2)
    # count bestimmen und evtl. end2 herabsetzen:
    if (I_I_comp(STACK_(2+2),count2)<0) # count1 < count2 ?
      { # ja -> count1 ist das Minimum
        STACK_(0+2) = I_I_plus_I(STACK_(1+2),STACK_(2+2)); # end2 := (+ start2 count1)
      }
      else
      { # nein -> count2 ist das Minimum
        STACK_(2+2) = count2; # count := count2
   }  }
    # Stackaufbau: sequence1, sequence2, start1, count, start2, end2,
    #              typdescr1, typdescr2.
    # Falls beide Sequences dieselben sind und die Bereiche sich
    # überschneiden, muß die Source erst herauskopiert werden:
    if (eq(STACK_(5+2),STACK_(4+2)) # (eq sequence1 sequence2)
        && (I_I_comp(STACK_(1+2),STACK_(3+2))<0) # (< start2 start1)
        && (I_I_comp(STACK_(3+2),STACK_(0+2))<0) # (< start1 end2)
       )
      { # Stück aus sequence2 herauskopieren:
        pushSTACK(STACK_(4+2)); pushSTACK(STACK_(1+2+1)); pushSTACK(STACK_(0+2+2));
        pushSTACK(STACK_(0+3)); subseq(); # (SUBSEQ sequence2 start2 end2)
        STACK_(4+2) = value1; # =: sequence2
        # Indizes adjustieren:
        STACK_(1+2) = Fixnum_0; # start2 := 0
      }
    # Stackaufbau: sequence1, sequence2, start1, count, start2, dummy,
    #              typdescr1, typdescr2.
    # Argumente für copy_seqpart_into auf den Stack legen:
    pushSTACK(STACK_(4+2+0)); pushSTACK(STACK_(0+1));
    pushSTACK(STACK_(5+2+2)); pushSTACK(STACK_(1+3));
    pushSTACK(STACK_(2+2+4));
    # Stackaufbau: sequence1, sequence2, start1, count, start2, dummy,
    #              typdescr1, typdescr2,
    #              sequence2, typdescr2, sequence1, typdescr1, count.
    pushSTACK(STACK_4); pushSTACK(STACK_(1+2+5+1));
    funcall(seq_init_start(STACK_(3+2)),2); # (SEQ-INIT-START sequence2 start2)
    pushSTACK(value1); # =: pointer2
    pushSTACK(STACK_(2+1)); pushSTACK(STACK_(3+2+5+1+1));
    funcall(seq_init_start(STACK_(1+1+2)),2); # (SEQ-INIT-START sequence1 start1)
    pushSTACK(value1); # =: pointer1
    # Stackaufbau: sequence1, sequence2, start1, count, start2, dummy,
    #              typdescr1, typdescr2,
    #              sequence2, typdescr2, sequence1, typdescr1, count,
    #              pointer2, pointer1.
    copy_seqpart_into(); # kopiere von sequence2 nach sequence1
    skipSTACK(5+2+5+2);
    value1 = popSTACK(); mv_count=1; # sequence1 als Wert
  }

# Unterprogramm zum Ausführen des Tests :TEST
# up_test(stackptr,x)
# > *(stackptr-5): die Testfunktion
# > *(stackptr+1): das zu vergleichende Item
# > x: Argument
# < ergebnis: TRUE falls der Test erfüllt ist, FALSE sonst
# kann GC auslösen
  local boolean up_test (object* stackptr, object x);
  local boolean up_test(stackptr,x)
    var object* stackptr;
    var object x;
    { # nach CLTL S. 247 ein (funcall testfun item x) ausführen:
      pushSTACK(*(stackptr STACKop 1)); # item
      pushSTACK(x); # x
      funcall(*(stackptr STACKop -5),2);
      if (nullp(value1)) return FALSE; else return TRUE;
    }

# Unterprogramm zum Ausführen des Tests :TEST-NOT
# up_test_not(stackptr,x)
# > *(stackptr-6): die Testfunktion
# > *(stackptr+1): das zu vergleichende Item
# > x: Argument
# < ergebnis: TRUE falls der Test erfüllt ist, FALSE sonst
# kann GC auslösen
  local boolean up_test_not (object* stackptr, object x);
  local boolean up_test_not(stackptr,x)
    var object* stackptr;
    var object x;
    { # nach CLTL S. 247 ein (not (funcall testfun item x)) ausführen:
      pushSTACK(*(stackptr STACKop 1)); # item
      pushSTACK(x); # x
      funcall(*(stackptr STACKop -6),2);
      if (nullp(value1)) return TRUE; else return FALSE;
    }

# Unterprogramm zum Ausführen des Tests -IF
# up_if(stackptr,x)
# > *(stackptr+1): das Testprädikat
# > x: Argument
# < ergebnis: TRUE falls der Test erfüllt ist, FALSE sonst
# kann GC auslösen
  local boolean up_if (object* stackptr, object x);
  local boolean up_if(stackptr,x)
    var object* stackptr;
    var object x;
    { # nach CLTL S. 247 ein (funcall predicate x) ausführen:
      pushSTACK(x); funcall(*(stackptr STACKop 1),1);
      if (nullp(value1)) return FALSE; else return TRUE;
    }

# Unterprogramm zum Ausführen des Tests -IF-NOT
# up_if_not(stackptr,x)
# > *(stackptr+1): das Testprädikat
# > x: Argument
# < ergebnis: TRUE falls der Test erfüllt ist, FALSE sonst
# kann GC auslösen
  local boolean up_if_not (object* stackptr, object x);
  local boolean up_if_not(stackptr,x)
    var object* stackptr;
    var object x;
    { # nach CLTL S. 247 ein (not (funcall predicate x)) ausführen:
      pushSTACK(x); funcall(*(stackptr STACKop 1),1);
      if (nullp(value1)) return TRUE; else return FALSE;
    }

# UP: Überprüft das :KEY-Argument
# test_key_arg(stackptr)
# > *(stackptr-4): optionales Argument
# < *(stackptr-4): korrekte KEY-Funktion
  local void test_key_arg (object* stackptr);
  local void test_key_arg(stackptr)
    var reg2 object* stackptr;
    { var reg1 object key_arg = *(stackptr STACKop -4);
      if (eq(key_arg,unbound) || nullp(key_arg))
        *(stackptr STACKop -4) = L(identity); # #'IDENTITY als Default für :KEY
    }

# UP: Überprüft das :COUNT-Argument
# > STACK_1: optionales Argument
# > subr_self: Aufrufer (ein SUBR)
# < STACK_1: korrekter COUNT-Wert: NIL oder ein Integer >=0
  local void test_count_arg (void);
  local void test_count_arg()
    { var reg1 object count = STACK_1;
      if (eq(count,unbound))
        { STACK_1 = NIL; } # Defaultwert NIL
        else
        # COUNT-Argument muß NIL oder ein Integer >= 0 sein:
        if (!(nullp(count) || (integerp(count) && positivep(count))))
          { fehler_posint(TheSubr(subr_self)->name,S(Kcount),count); }
    }

# Fehler, wenn beide :TEST, :TEST-NOT - Argumente angegeben wurden.
# fehler_both_tests();
# > subr_self: Aufrufer (ein SUBR)
  global nonreturning void fehler_both_tests (void);
  global nonreturning void fehler_both_tests()
    { pushSTACK(TheSubr(subr_self)->name);
      fehler(
             DEUTSCH ? "~: Argumente zu :TEST und :TEST-NOT dürfen nicht beide angegeben werden." :
             ENGLISH ? "~: Must not specify both arguments to :TEST and :TEST-NOT" :
             FRANCAIS ? "~ : Les arguments pour :TEST et :TEST-NOT ne peuvent être spécifiés en même temps." :
             ""
            );
    }

# UP: Überprüft die :TEST, :TEST-NOT - Argumente
# test_test_args(stackptr)
# > stackptr: Pointer in den STACK
# > *(stackptr-5): :TEST-Argument
# > *(stackptr-6): :TEST-NOT-Argument
# > subr_self: Aufrufer (ein SUBR)
# < *(stackptr-5): verarbeitetes :TEST-Argument
# < *(stackptr-6): verarbeitetes :TEST-NOT-Argument
# < up_fun: Adresse einer Testfunktion, die wie folgt spezifiziert ist:
#       > stackptr: derselbe Pointer in den Stack, *(stackptr+1) = item,
#         *(stackptr-5) = :test-Argument, *(stackptr-6) = :test-not-Argument,
#       > x: Argument
#       < TRUE, falls der Test erfüllt ist, FALSE sonst.
  # up_function sei der Typ der Adresse einer solchen Testfunktion:
  typedef boolean (*up_function) (object* stackptr, object x);
  local up_function test_test_args (object* stackptr);
  local up_function test_test_args(stackptr)
    var reg1 object* stackptr;
    { var reg3 object test_arg = *(stackptr STACKop -5);
      if (eq(test_arg,unbound)) { test_arg=NIL; }
      # test_arg ist das :TEST-Argument
     {var reg2 object test_not_arg = *(stackptr STACKop -6);
      if (eq(test_not_arg,unbound)) { test_not_arg=NIL; }
      # test_not_arg ist das :TEST-NOT-Argument
      if (nullp(test_not_arg))
        # :TEST-NOT wurde nicht angegeben
        { if (nullp(test_arg))
            *(stackptr STACKop -5) = L(eql); # #'EQL als Default für :TEST
          return(&up_test);
        }
        # :TEST-NOT wurde angegeben
        { if (nullp(test_arg))
            return(&up_test_not);
          else
            fehler_both_tests();
    }}  }

# UP: bereitet eine Sequence-Operation mit Test vor.
# > Stackaufbau:
#     ... sequence [stackptr] from-end start end key ... [STACK]
#   genauer:
#     ... item sequence [stackptr] from-end start end key test test-not ... [STACK]
#     oder
#     ... test sequence [stackptr] from-end start end key ... [STACK]
#     oder
#     ... test-not sequence [stackptr] from-end start end key ... [STACK]
# > stackptr: Pointer in den Stack
# > subr_self: Aufrufer (ein SUBR)
# < STACK: wird um 1 erniedrigt
# < STACK_0: typdescr zu sequence
  local void seq_prepare_testop (object* stackptr);
  local void seq_prepare_testop(stackptr)
    var reg1 object* stackptr;
    { # sequence überprüfen, typdescr auf den Stack:
      pushSTACK(get_valid_seq_type(*(stackptr STACKop 0)));
      # key überprüfen:
      test_key_arg(stackptr);
      # Defaultwert für from-end ist NIL:
      default_NIL(*(stackptr STACKop -1));
      # Defaultwert für start ist 0:
      start_default_0(*(stackptr STACKop -2));
      # Defaultwert für end ist NIL:
      default_NIL(*(stackptr STACKop -3));
      # start und end überprüfen:
      test_start_end_1(&O(kwpair_start),&*(stackptr STACKop -3));
    }

# UP: führt eine Sequence-Filter-Operation aus.
# Eine Sequence wird durchlaufen und dabei in einem Bit-Vektor abgespeichert,
# welche Elemente dem Test genügen. Dann wird eine Routine aufgerufen, die
# den Rest erledigt.
# > Stackaufbau:
#     ... sequence [stackptr] from-end start end key ... count typdescr [STACK]
# > stackptr: Pointer in den Stack
# > up_fun: Adresse einer Testfunktion, die wie folgt spezifiziert ist:
#           > stackptr: derselbe Pointer in den Stack,
#           > x: Argument
#           < TRUE, falls der Test erfüllt ist, FALSE sonst.
# > help_fun: Adresse einer Hilfsroutine, die den Rest erledigt.
#   Spezifiziert durch:
#       > stackptr: Pointer in den Stack,
#         *(stackptr+0)=sequence, *(stackptr-2)=start, *(stackptr-3)=end,
#       > STACK_2: typdescr,
#       > STACK_1: Länge l der Sequence,
#       > STACK_0: Bit-Vektor bv,
#       > bvl: Länge des Bit-Vektors (= end - start),
#       > dl: Anzahl der im Bit-Vektor gesetzten Bits,
#       < ergebnis: Ergebnis
# > subr_self: Aufrufer (ein SUBR)
# < mv_space/mv_count: Werte
# kann GC auslösen
  # help_function sei der Typ der Adresse einer solchen Hilfsfunktion:
  typedef object (*help_function) (object* stackptr, uintL bvl, uintL dl);
  local Values seq_filterop (object* stackptr, up_function up_fun, help_function help_fun);
  local Values seq_filterop(stackptr,up_fun,help_fun)
    var reg1 object* stackptr;
    var reg5 up_function up_fun;
    var reg6 help_function help_fun;
    { # COUNT-Argument muß NIL oder ein Integer >= 0 sein:
      test_count_arg();
     {var reg7 object old_subr_self = subr_self; # aktuelles SUBR, nicht GC-gefährdet!
      # l = (SEQ-LENGTH sequence) bestimmen:
      pushSTACK(*(stackptr STACKop 0)); # sequence
      funcall(seq_length(STACK_(0+1)),1); # (SEQ-LENGTH sequence)
      pushSTACK(value1); # l in den Stack
      subr_self = old_subr_self;
     }
      # Defaultwert für END ist l:
      if (nullp(*(stackptr STACKop -3))) # end=NIL ?
        { *(stackptr STACKop -3) = STACK_0; # ja -> end:=l
          # Dann nochmals start und end überprüfen:
          test_start_end(&O(kwpair_start),&*(stackptr STACKop -3));
        }
      # Nun sind alle Argumente überprüft.
      pushSTACK(*(stackptr STACKop 0)); # sequence
      pushSTACK(*(stackptr STACKop -4)); # key
      # (- end start) bestimmen und neuen Bitvektor allozieren:
     {var reg3 uintL bvl; # Bitvektor-Länge
      var reg4 uintL dl = 0; # Anzahl der im Bitvektor gesetzten Bits
      { var reg2 object bvsize = I_I_minus_I(*(stackptr STACKop -3),*(stackptr STACKop -2));
        # bvsize = (- end start), ein Integer >=0
        if (!(posfixnump(bvsize))) # Fixnum?
          { pushSTACK(*(stackptr STACKop 0)); # sequence
            pushSTACK(TheSubr(subr_self)->name);
            fehler(
                   DEUTSCH ? "~: Zu lange Sequence: ~" :
                   ENGLISH ? "~: sequence ~ is too long" :
                   FRANCAIS ? "~ : Séquence trop longue: ~" :
                   ""
                  );
          }
        bvl = posfixnum_to_L(bvsize); # Länge des Bitvektors als Longword
      }
      pushSTACK(allocate_bit_vector_0(bvl)); # neuer Bitvektor bv
      # Stackaufbau: ... count, typdescr,
      #              l, sequence, key, bv [STACK].
      if (!(nullp(*(stackptr STACKop -1)))) # from-end abfragen
        # from-end ist angegeben
        { pushSTACK(STACK_2); # sequence
          pushSTACK(*(stackptr STACKop -3)); # end
          funcall(seq_fe_init_end(STACK_(0+4+2)),2); # (SEQ-FE-INIT-END sequence end)
          pushSTACK(value1); # =: pointer
          pushSTACK(STACK_(1+4+1)); # countdown := count
          # Stackaufbau: ... count, typdescr,
          #              l, sequence, key, bv,
          #              pointer, countdown [STACK].
         {var reg2 uintL bvi = bvl; # Schleife bvl mal durchlaufen
          until (bvi==0)
            { bvi--;
              if (!(nullp(STACK_(1+4+2))) && eq(STACK_0,Fixnum_0))
                # count/=NIL und countdown=0 -> Schleife kann abgebrochen werden
                break;
              # nächstes Element abtesten:
              pushSTACK(STACK_(2+2)); # sequence
              pushSTACK(STACK_(1+1)); # pointer
              funcall(seq_access(STACK_(0+4+2+2)),2); # (SEQ-ACCESS sequence pointer)
              pushSTACK(value1); funcall(STACK_(1+2+1),1); # (FUNCALL key ...)
              if ((*up_fun)(stackptr,value1)) # Testroutine aufrufen
                # Test erfüllt
                { sbvector_bset(STACK_(0+2),bvi); # (setf (sbit bv bvi) 1)
                  dl++; # dl := dl+1, ein gesetztes Bit mehr
                  if (!(nullp(STACK_(1+4+2)))) # falls count/=NIL:
                    { decrement(STACK_0); } # (decf countdown)
                }
              # pointer weiterrücken:
              pointer_fe_update(STACK_1,STACK_(2+2),STACK_(0+4+2));
        }}  }
        else
        # from-end ist nicht angegeben
        { pushSTACK(STACK_2); # sequence
          pushSTACK(*(stackptr STACKop -2)); # start
          funcall(seq_init_start(STACK_(0+4+2)),2); # (SEQ-INIT-START sequence start)
          pushSTACK(value1); # =: pointer
          pushSTACK(STACK_(1+4+1)); # countdown := count
          # Stackaufbau: ... count, typdescr,
          #              l, sequence, key, bv,
          #              pointer, countdown [STACK].
         {var reg2 uintL bvi = 0; # Schleife bvl mal durchlaufen
          until (bvi==bvl)
            { if (!(nullp(STACK_(1+4+2))) && eq(STACK_0,Fixnum_0))
                # count/=NIL und countdown=0 -> Schleife kann abgebrochen werden
                break;
              # nächstes Element abtesten:
              pushSTACK(STACK_(2+2)); # sequence
              pushSTACK(STACK_(1+1)); # pointer
              funcall(seq_access(STACK_(0+4+2+2)),2); # (SEQ-ACCESS sequence pointer)
              pushSTACK(value1); funcall(STACK_(1+2+1),1); # (FUNCALL key ...)
              if ((*up_fun)(stackptr,value1)) # Testroutine aufrufen
                # Test erfüllt
                { sbvector_bset(STACK_(0+2),bvi); # (setf (sbit bv bvi) 1)
                  dl++; # dl := dl+1, ein gesetztes Bit mehr
                  if (!(nullp(STACK_(1+4+2)))) # falls count/=NIL:
                    { decrement(STACK_0); } # (decf countdown)
                }
              # pointer weiterrücken:
              pointer_update(STACK_1,STACK_(2+2),STACK_(0+4+2));
              bvi++;
        }}  }
      skipSTACK(2); # pointer und countdown vergessen
      # Stackaufbau: ... count, typdescr,
      #              l, sequence, key, bv [STACK].
      STACK_2 = STACK_0; skipSTACK(2); # bv hochschieben
      # Stackaufbau: ... count, typdescr, l, bv [STACK].
      value1 = (*help_fun)(stackptr,bvl,dl); # Rest durchführen
      mv_count=1; # Ergebnis als Wert
      skipSTACK(2); # l und bv vergessen
    }}

# UP: Hilfsroutine für REMOVE-Funktionen.
# Bildet zu einer Sequence eine neue Sequence, in der genau die Elemente
# fehlen, die in einem Bitvektor markiert sind.
# > stackptr: Pointer in den Stack,
#   *(stackptr+0)=sequence, *(stackptr-2)=start, *(stackptr-3)=end,
# > STACK_2: typdescr,
# > STACK_1: Länge l der Sequence,
# > STACK_0: Bit-Vektor bv,
# > bvl: Länge des Bit-Vektors (= end - start),
# > dl: Anzahl der im Bit-Vektor gesetzten Bits,
# < ergebnis: Ergebnis
# kann GC auslösen
  local object remove_help (object* stackptr, uintL bvl, uintL dl);
  local object remove_help(stackptr,bvl,dl)
    var reg1 object* stackptr;
    var reg3 uintL bvl;
    var reg4 uintL dl;
    { # dl=0 -> sequence unverändert zurückgeben:
      if (dl==0) { return *(stackptr STACKop 0); }
      # neue Sequence allozieren:
      pushSTACK(I_I_minus_I(STACK_1,fixnum(dl))); # (- l dl)
      funcall(seq_make(STACK_(2+1)),1); # (SEQ-MAKE (- l dl))
      pushSTACK(value1);
      # Stackaufbau: typdescr, l, bv, sequence2.
      pushSTACK(*(stackptr STACKop 0)); # sequence
      pushSTACK(STACK_(3+1)); # typdescr
      pushSTACK(STACK_(0+2)); # sequence2
      pushSTACK(STACK_(3+3)); # typdescr
      pushSTACK(*(stackptr STACKop -2)); # start
      # Stackaufbau: typdescr, l, bv, sequence2,
      #              seq1, typdescr1, seq2, typdescr2, start.
      pushSTACK(STACK_4); funcall(seq_init(STACK_(3+1)),1); # (SEQ-INIT sequence)
      pushSTACK(value1); # =: pointer1
      pushSTACK(STACK_(2+1)); funcall(seq_init(STACK_(1+1+1)),1); # (SEQ-INIT sequence2)
      pushSTACK(value1); # =: pointer2
      # Stackaufbau: typdescr, l, bv, sequence2,
      #              seq1, typdescr1, seq2, typdescr2, start,
      #              pointer1, pointer2.
      { # Vorderes Teilstück:
        # Elemente mit Index <start von sequence1 nach sequence2
        # unverändert übertragen:
        copy_seqpart_into();
      }
      { # Mittleres Teilstück: sieben.
        var reg2 uintL bvi = 0;
        until (bvi==bvl)
          { if (!(sbvector_btst(STACK_(1+5+2),bvi))) # (sbit bv bvi) abfragen
              # Bit ist nicht gesetzt, also Element übernehmen
              { pushSTACK(STACK_(4+2)); pushSTACK(STACK_(1+1));
                funcall(seq_access(STACK_(3+2+2)),2); # (SEQ-ACCESS seq1 pointer1)
                pushSTACK(STACK_(2+2)); pushSTACK(STACK_(0+1)); pushSTACK(value1);
                funcall(seq_access_set(STACK_(1+2+3)),3); # (SEQ-ACCESS-SET seq2 pointer2 ...)
                # pointer2 := (SEQ-UPD seq2 pointer2) :
                pointer_update(STACK_0,STACK_(2+2),STACK_(1+2));
              }
            # pointer1 := (SEQ-UPD seq1 pointer1) :
            pointer_update(STACK_1,STACK_(4+2),STACK_(3+2));
            bvi++;
      }   }
      { # Hinteres Teilstück:
        # Elemente mit Index >=end von sequence1 nach sequence2
        # unverändert übertragen:
        STACK_(0+2) = I_I_minus_I(STACK_(2+5+2),*(stackptr STACKop -3)); # (- l end)
        copy_seqpart_into();
      }
      skipSTACK(5+2);
      return popSTACK(); # sequence2 als Ergebnis
    }

# UP: Hilfsroutine für DELETE-Funktionen.
# Entfernt aus einer Sequence genau die Elemente, die in einem Bitvektor
# markiert sind.
# > stackptr: Pointer in den Stack,
#   *(stackptr+0)=sequence, *(stackptr-2)=start, *(stackptr-3)=end,
# > STACK_2: typdescr,
# > STACK_1: Länge l der Sequence,
# > STACK_0: Bit-Vektor bv,
# > bvl: Länge des Bit-Vektors (= end - start),
# > dl: Anzahl der im Bit-Vektor gesetzten Bits,
# < ergebnis: Ergebnis
# kann GC auslösen
  local object delete_help (object* stackptr, uintL bvl, uintL dl);
  local object delete_help(stackptr,bvl,dl)
    var reg4 object* stackptr;
    var reg5 uintL bvl;
    var reg6 uintL dl;
    { # dl=0 -> sequence unverändert zurückgeben:
      if (dl==0) { return *(stackptr STACKop 0); }
     {var reg7 object type = seq_type(STACK_2);
      if (eq(type,S(list))) # Typ LIST ?
        { # Noch überprüfen, ob sequence wirklich eine Liste ist.
          # Wegen l >= dl > 0 ist zu testen, ob sequence ein Cons ist.
          if (mconsp(*(stackptr STACKop 0)))
            { # Listen speziell behandeln:
              var object whole_list = *(stackptr STACKop 0); # ganze Liste
              var reg1 object* list_ = &whole_list;
              var reg2 object list = *list_;
              # Stets list = *list_.
              # Vorderes Teilstück:
              # start mal mit list:=Cdr(list) weiterrücken:
              { var reg3 uintL count;
                dotimesL(count,posfixnum_to_L(*(stackptr STACKop -2)),
                  { list_ = &Cdr(list); list = *list_; });
              }
              # Mittleres Teilstück:
              # bvl mal ein Bit abfragen und evtl. ein Cons streichen:
              { var reg3 uintL bvi = 0;
                until (bvi==bvl)
                  { if (sbvector_btst(STACK_0,bvi)) # (sbit bv bvi) abfragen
                      # Bit ist =1 -> Cons bei list herausnehmen:
                      { *list_ = list = Cdr(list); }
                      else
                      # Bit ist =0 -> nur weiterrücken:
                      { list_ = &Cdr(list); list = *list_; }
                    bvi++;
              }   }
              return whole_list; # modifizierte Liste als Ergebnis
            }
            else
            goto other;
        }
      elif (eq(type,S(vector)) || eq(type,S(string)) || eq(type,S(bit_vector)) || posfixnump(type))
        # Typ [GENERAL-]VECTOR, STRING, BIT-VECTOR, Byte-VECTOR
        { # Noch überprüfen, ob sequence wirklich ein Vektor ist.
          var reg3 object sequence = *(stackptr STACKop 0);
          if (!(vectorp(sequence))) { goto other; }
          # Bei Arrays ohne Fill-Pointer kann man nichts Spezielles machen:
          if (!(array_has_fill_pointer_p(sequence))) { goto other; }
          # sequence ist ein Vektor mit Fill-Pointer.
          # Elemente zusammenschieben und dann Fill-Pointer herabsetzen:
          pushSTACK(sequence); # sequence
          pushSTACK(*(stackptr STACKop -2)); # i := start
          pushSTACK(STACK_0); # j := i
          # Stackaufbau: typdescr, l, bv, sequence, i, j.
          # j = Source-Index, i = Destination-Index, start <= i <= j .
          # Mittleres Teilstück:
          { var reg1 uintL bvi = 0;
            until (bvi==bvl)
              { if (!(sbvector_btst(STACK_3,bvi))) # (sbit bv bvi) abfragen
                  # Bit gelöscht -> Element übertragen:
                  { # (setf (aref sequence i) (aref sequence j)) :
                    pushSTACK(STACK_2); pushSTACK(STACK_(0+1));
                    funcall(L(aref),2); # (AREF sequence j)
                    pushSTACK(STACK_2); pushSTACK(STACK_(1+1)); pushSTACK(value1);
                    funcall(L(store),3); # (SYS::STORE sequence i ...)
                    # i:=i+1 :
                    STACK_1 = fixnum_inc(STACK_1,1);
                  }
                # j:=j+1 :
                STACK_0 = fixnum_inc(STACK_0,1);
                bvi++;
          }   }
          # Hinteres Teilstück:
          { until (eq(STACK_0,STACK_4)) # solange bis j = l (beides Fixnums)
              # Element übertragen:
              { # (setf (aref sequence i) (aref sequence j)) :
                pushSTACK(STACK_2); pushSTACK(STACK_(0+1));
                funcall(L(aref),2); # (AREF sequence j)
                pushSTACK(STACK_2); pushSTACK(STACK_(1+1)); pushSTACK(value1);
                funcall(L(store),3); # (SYS::STORE sequence i ...)
                # i:=i+1 :
                STACK_1 = fixnum_inc(STACK_1,1);
                # j:=j+1 :
                STACK_0 = fixnum_inc(STACK_0,1);
          }   }
          skipSTACK(1);
          # Stackaufbau: typdescr, l, bv, sequence, i.
          # (setf (fill-pointer sequence) i) :
          funcall(L(set_fill_pointer),2); # (SYS::SET-FILL-POINTER sequence i)
          # Stackaufbau: typdescr, l, bv.
          return *(stackptr STACKop 0); # sequence mit modifiziertem Fill-Pointer
        }
      other: # sonstige Sequences
        return remove_help(stackptr,bvl,dl); # DELETE wie REMOVE behandeln
    }}

LISPFUN(remove,2,0,norest,key,7,\
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not),kw(count)) )
# (REMOVE item sequence [:from-end] [:start] [:end] [:key] [:test] [:test-not] [:count]),
# CLTL S. 253
  { var reg1 object* stackptr = &STACK_7;
    var reg2 up_function up_fun = test_test_args(stackptr); # Testfunktion
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    seq_filterop(stackptr,up_fun,&remove_help); # Filtern
    skipSTACK(2+7+1);
  }

LISPFUN(remove_if,2,0,norest,key,5,\
        (kw(from_end),kw(start),kw(end),kw(key),kw(count)) )
# (REMOVE-IF test sequence [:from-end] [:start] [:end] [:key] [:count]),
# CLTL S. 253
  { var reg1 object* stackptr = &STACK_5;
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    seq_filterop(stackptr,&up_if,&remove_help); # Filtern
    skipSTACK(2+5+1);
  }

LISPFUN(remove_if_not,2,0,norest,key,5,\
        (kw(from_end),kw(start),kw(end),kw(key),kw(count)) )
# (REMOVE-IF-NOT test sequence [:from-end] [:start] [:end] [:key] [:count]),
# CLTL S. 253
  { var reg1 object* stackptr = &STACK_5;
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    seq_filterop(stackptr,&up_if_not,&remove_help); # Filtern
    skipSTACK(2+5+1);
  }

LISPFUN(delete,2,0,norest,key,7,\
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not),kw(count)) )
# (DELETE item sequence [:from-end] [:start] [:end] [:key] [:test] [:test-not] [:count]),
# CLTL S. 254
  { var reg1 object* stackptr = &STACK_7;
    var reg2 up_function up_fun = test_test_args(stackptr); # Testfunktion
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    seq_filterop(stackptr,up_fun,&delete_help); # Filtern
    skipSTACK(2+7+1);
  }

LISPFUN(delete_if,2,0,norest,key,5,\
        (kw(from_end),kw(start),kw(end),kw(key),kw(count)) )
# (DELETE-IF test sequence [:from-end] [:start] [:end] [:key] [:count]),
# CLTL S. 254
  { var reg1 object* stackptr = &STACK_5;
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    seq_filterop(stackptr,&up_if,&delete_help); # Filtern
    skipSTACK(2+5+1);
  }

LISPFUN(delete_if_not,2,0,norest,key,5,\
        (kw(from_end),kw(start),kw(end),kw(key),kw(count)) )
# (DELETE-IF-NOT test sequence [:from-end] [:start] [:end] [:key] [:count]),
# CLTL S. 254
  { var reg1 object* stackptr = &STACK_5;
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    seq_filterop(stackptr,&up_if_not,&delete_help); # Filtern
    skipSTACK(2+5+1);
  }

# Unterprogramm zum Ausführen des Tests :TEST
# up2_test(stackptr,x,y)
# > *(stackptr-5): die Testfunktion
# > x,y: Argumente
# < ergebnis: TRUE falls der Test erfüllt ist, FALSE sonst
# kann GC auslösen
  local boolean up2_test (object* stackptr, object x, object y);
  local boolean up2_test(stackptr,x,y)
    var object* stackptr;
    var object x;
    var object y;
    { # ein (funcall testfun x y) ausführen:
      pushSTACK(x); # x
      pushSTACK(y); # y
      funcall(*(stackptr STACKop -5),2);
      if (nullp(value1)) return FALSE; else return TRUE;
    }

# Unterprogramm zum Ausführen des Tests :TEST-NOT
# up2_test_not(stackptr,x,y)
# > *(stackptr-6): die Testfunktion
# > x,y: Argumente
# < ergebnis: TRUE falls der Test erfüllt ist, FALSE sonst
# kann GC auslösen
  local boolean up2_test_not (object* stackptr, object x, object y);
  local boolean up2_test_not(stackptr,x,y)
    var object* stackptr;
    var object x;
    var object y;
    { # ein (not (funcall testfun x y)) ausführen:
      pushSTACK(x); # x
      pushSTACK(y); # y
      funcall(*(stackptr STACKop -6),2);
      if (nullp(value1)) return TRUE; else return FALSE;
    }

# UP: Überprüft die :TEST, :TEST-NOT - Argumente
# test_test2_args(stackptr)
# > stackptr: Pointer in den STACK
# > *(stackptr-5): :TEST-Argument
# > *(stackptr-6): :TEST-NOT-Argument
# > subr_self: Aufrufer (ein SUBR)
# < *(stackptr-5): verarbeitetes :TEST-Argument
# < *(stackptr-6): verarbeitetes :TEST-NOT-Argument
# < up2_fun: Adresse einer Testfunktion, die wie folgt spezifiziert ist:
#       > stackptr: derselbe Pointer in den Stack,
#         *(stackptr-5) = :test-Argument, *(stackptr-6) = :test-not-Argument,
#       > x,y: Argumente
#       < TRUE, falls der Test erfüllt ist, FALSE sonst.
  # up2_function sei der Typ der Adresse einer solchen Testfunktion:
  typedef boolean (*up2_function) (object* stackptr, object x, object y);
  local up2_function test_test2_args (object* stackptr);
  local up2_function test_test2_args(stackptr)
    var reg1 object* stackptr;
    { var reg3 object test_arg = *(stackptr STACKop -5);
      if (eq(test_arg,unbound)) { test_arg=NIL; }
      # test_arg ist das :TEST-Argument
     {var reg2 object test_not_arg = *(stackptr STACKop -6);
      if (eq(test_not_arg,unbound)) { test_not_arg=NIL; }
      # test_not_arg ist das :TEST-NOT-Argument
      if (nullp(test_not_arg))
        # :TEST-NOT wurde nicht angegeben
        { if (nullp(test_arg))
            *(stackptr STACKop -5) = L(eql); # #'EQL als Default für :TEST
          return(&up2_test);
        }
        # :TEST-NOT wurde angegeben
        { if (nullp(test_arg))
            return(&up2_test_not);
          else
            fehler_both_tests();
    }}  }

# UP: führt eine Sequence-Duplicates-Operation aus.
# seq_duplicates(help_fun)
# Eine Sequence wird durchlaufen und dabei in einem Bit-Vektor abgespeichert,
# welche Elemente doppelt vorkommen. Dann wird eine Routine aufgerufen, die
# den Rest erledigt.
# > Stackaufbau:
#     sequence from-end start end key test test-not [STACK]
# > help_fun: Adresse einer Hilfsroutine, die den Rest erledigt.
#     Spezifiziert durch:
#       > stackptr: Pointer in den Stack,
#         *(stackptr+0)=sequence, *(stackptr-2)=start, *(stackptr-3)=end,
#       > STACK_2: typdescr,
#       > STACK_1: Länge der Sequence,
#       > STACK_0: Bit-Vektor bv,
#       > bvl: Länge des Bit-Vektors (= end - start),
#       > dl: Anzahl der im Bit-Vektor gesetzten Bits,
#       < ergebnis: Ergebnis
#       kann GC auslösen
# > subr_self: Aufrufer (ein SUBR)
# < mv_space/mv_count: Werte
# kann GC auslösen
  local Values seq_duplicates (help_function help_fun);
  local Values seq_duplicates(help_fun)
    var reg8 help_function help_fun;
    { var reg2 object* stackptr = &STACK_6;
      # Stackaufbau:
      #   sequence [stackptr], from-end, start, end, key, test, test-not.
      # sequence überprüfen:
      { var reg1 object sequence = *(stackptr STACKop 0);
        pushSTACK(get_valid_seq_type(sequence)); # typdescr auf den Stack
      }
      # Stackaufbau:
      #   sequence [stackptr], from-end, start, end, key, test, test-not,
      #   typdescr.
      # :test und :test-not überprüfen:
     {var reg3 up2_function up2_fun = test_test2_args(stackptr);
      # key überprüfen:
      test_key_arg(stackptr);
      # Defaultwert für from-end ist NIL:
      default_NIL(*(stackptr STACKop -1));
      # Defaultwert für start ist 0:
      start_default_0(*(stackptr STACKop -2));
      # Defaultwert für end ist nil:
      default_NIL(*(stackptr STACKop -3));
      # start und end überprüfen:
      test_start_end_1(&O(kwpair_start),&*(stackptr STACKop -3));
      # Länge der Sequence bestimmen:
      { var reg9 object old_subr_self = subr_self; # aktuelles SUBR, nicht GC-gefährdet!
        pushSTACK(STACK_(6+1)); # sequence
        funcall(seq_length(STACK_(0+1)),1); # (SEQ-LENGTH sequence)
        pushSTACK(value1); # l
        subr_self = old_subr_self;
      }
      # Stackaufbau:
      #   sequence [stackptr], from-end, start, end, key, test, test-not,
      #   typdescr, l.
      # Defaultwert für end ist l = (length sequence):
      if (nullp(*(stackptr STACKop -3)))
        { *(stackptr STACKop -3) = STACK_0; # end := l
          # Dann nochmals start und end überprüfen:
          test_start_end(&O(kwpair_start),&*(stackptr STACKop -3));
        }
      # Nun sind alle Argumente überprüft.
      { var reg5 uintL bvl; # Bitvektor-Länge
        var reg6 uintL dl; # Anzahl der im Bitvektor gesetzten Bits
        # (- end start) bestimmen und neuen Bitvektor allozieren:
        { var reg1 object size = I_I_minus_I(STACK_(3+2),STACK_(4+2));
          # size = (- end start), ein Integer >=0
          if (!(posfixnump(size)))
            { pushSTACK(*(stackptr STACKop 0)); # sequence
              fehler(
                     DEUTSCH ? "Zu lange Sequence: ~" :
                     ENGLISH ? "too long sequence ~" :
                     FRANCAIS ? "Séquence trop longue : ~" :
                     ""
                    );
            }
          bvl = posfixnum_to_L(size);
        }
        pushSTACK(allocate_bit_vector_0(bvl));
        # Stackaufbau:
        #   sequence [stackptr], from-end, start, end, key, test, test-not,
        #   typdescr, l, bv.
        dl = 0; # dl := 0
        # Bei :test #'eq/eql/equal und großer Länge verwende Hashtabelle:
        if (bvl < 10) goto standard;
        if (!(up2_fun == &up2_test)) goto standard;
        { var reg7 object test = STACK_(1+3);
          if (!(eq(test,L(eq)) || eq(test,L(eql)) || eq(test,L(equal))
                || eq(test,S(eq)) || eq(test,S(eql)) || eq(test,S(equal))
             ) )
            goto standard;
        }
        if (FALSE)
          standard: # Standardmethode
          { if (!(nullp(STACK_(5+3)))) # from-end abfragen
              # from-end ist angegeben
              {{pushSTACK(STACK_(6+3)); # sequence
                pushSTACK(STACK_(4+3+1)); # start
                funcall(seq_init_start(STACK_(2+2)),2); # (SEQ-INIT-START sequence start)
                pushSTACK(value1); # =: pointer1
               }
               # Stackaufbau:
               #   sequence [stackptr], from-end, start, end, key, test, test-not,
               #   typdescr, l, bv,
               #   pointer1.
               # pointer1 läuft von links nach rechts (von start bis end).
               {var reg4 uintL bvi1 = 0; # Schleife bvl mal durchlaufen
                until (bvi1==bvl)
                  { if (!(sbvector_btst(STACK_(0+1),bvi1))) # (sbit bv bvi1) abfragen
                      # falls Bit=0: dieses Element ist noch nicht gestrichen ->
                      # teste, ob es weiter rechts vorkommt:
                      {{pushSTACK(STACK_(6+3+1)); # sequence
                        pushSTACK(STACK_(0+1)); # pointer1
                        funcall(seq_access(STACK_(2+1+2)),2); # (SEQ-ACCESS sequence pointer1)
                        pushSTACK(value1);
                        funcall(STACK_(2+3+1+1),1); # (FUNCALL key (SEQ-ACCESS sequence pointer1))
                        pushSTACK(value1); # =: item1
                       }
                        # Stackaufbau:
                        #   sequence [stackptr], from-end, start, end, key, test, test-not,
                        #   typdescr, l, bv,
                        #   pointer1, item1.
                        # pointer1 := (SEQ-UPD sequence pointer1) :
                        pointer_update(STACK_1,STACK_(6+3+2),STACK_(2+2));
                        # pointer2 := (SEQ-COPY pointer1) :
                       {pushSTACK(STACK_1); funcall(seq_copy(STACK_(2+2+1)),1); # (SEQ-COPY pointer1)
                        pushSTACK(value1); # =: pointer2
                       }
                        # Stackaufbau:
                        #   sequence [stackptr], from-end, start, end, key, test, test-not,
                        #   typdescr, l, bv,
                        #   pointer1, item1, pointer2.
                        # pointer2 läuft von pointer1 nach rechts.
                       {var reg1 uintL bvi2 = bvi1; # bvi2 := bvi1
                        until (bvi2==bvl)
                          { if (!(sbvector_btst(STACK_(0+3),bvi2))) # (sbit bv bvi2) abfragen
                              # falls Bit=0: dieses Element ist auch noch nicht gestrichen.
                              # vergleiche beide Elemente:
                              { pushSTACK(STACK_(6+3+3)); # sequence
                                pushSTACK(STACK_(0+1)); # pointer2
                                funcall(seq_access(STACK_(2+3+2)),2); # (SEQ-ACCESS sequence pointer2)
                                pushSTACK(value1);
                                funcall(STACK_(2+3+3+1),1); # (FUNCALL key (SEQ-ACCESS sequence pointer2))
                                # value1 =: item2
                                # item1 und item2 vergleichen:
                                if ((*up2_fun)(stackptr,STACK_1,value1)) # Testroutine aufrufen
                                  # Test erfüllt -> vermerke, daß item2 zu streichen ist:
                                  { sbvector_bset(STACK_(0+3),bvi2); # (setf (sbit bv bvi2) 1)
                                    dl = dl+1; # dl:=dl+1
                                  }
                              }
                            # pointer2 := (SEQ-UPD sequence pointer2) :
                            pointer_update(STACK_0,STACK_(6+3+3),STACK_(2+3));
                            bvi2++; # bvi2 := bvi2+1
                       }  }
                        skipSTACK(2); # item1 und pointer2 vergessen
                      }
                      else
                      # falls Bit=1: dieses Element einfach übergehen
                      { # pointer1 := (SEQ-UPD sequence pointer1) :
                        pointer_update(STACK_0,STACK_(6+3+1),STACK_(2+1));
                      }
                    bvi1++;
               }  }
                skipSTACK(1); # pointer1 vergessen
              }
              else
              # from-end ist nicht angegeben
              {{pushSTACK(STACK_(6+3)); # sequence
                pushSTACK(STACK_(4+3+1)); # start
                funcall(seq_init_start(STACK_(2+2)),2); # (SEQ-INIT-START sequence start)
                pushSTACK(value1); # =: pointer0
               }
               # Stackaufbau:
               #   sequence [stackptr], from-end, start, end, key, test, test-not,
               #   typdescr, l, bv,
               #   pointer0.
               # pointer0 steht links.
               {pushSTACK(STACK_0); funcall(seq_copy(STACK_(2+1+1)),1); # (SEQ-COPY pointer0)
                pushSTACK(value1); # =: pointer2
               }
               # Stackaufbau:
               #   sequence [stackptr], from-end, start, end, key, test, test-not,
               #   typdescr, l, bv,
               #   pointer0, pointer2.
               # pointer2 läuft von links nach rechts (von start bis end).
               {var reg4 uintL bvi2 = 0; # Schleife bvl mal durchlaufen
                until (bvi2==bvl)
                  { if (!(sbvector_btst(STACK_(0+2),bvi2))) # (sbit bv bvi2) abfragen
                      # falls Bit=0: dieses Element ist noch nicht gestrichen ->
                      # teste, ob es weiter links vorkommt:
                      {{pushSTACK(STACK_(6+3+2)); # sequence
                        pushSTACK(STACK_(0+1)); # pointer2
                        funcall(seq_access(STACK_(2+2+2)),2); # (SEQ-ACCESS sequence pointer2)
                        pushSTACK(value1);
                        funcall(STACK_(2+3+2+1),1); # (FUNCALL key (SEQ-ACCESS sequence pointer1))
                        pushSTACK(value1); # =: item2
                       }
                        # Stackaufbau:
                        #   sequence [stackptr], from-end, start, end, key, test, test-not,
                        #   typdescr, l, bv,
                        #   pointer0, pointer2, item2.
                        # pointer1 := (SEQ-COPY pointer0) :
                       {pushSTACK(STACK_2); funcall(seq_copy(STACK_(2+3+1)),1); # (SEQ-COPY pointer0)
                        pushSTACK(value1); # =: pointer1
                       }
                        # Stackaufbau:
                        #   sequence [stackptr], from-end, start, end, key, test, test-not,
                        #   typdescr, l, bv,
                        #   pointer0, pointer2, item2, pointer1.
                        # pointer1 läuft von links bis pointer2.
                       {var reg1 uintL bvi1 = 0; # bvi1 := 0
                        until (bvi1==bvi2)
                          { if (!(sbvector_btst(STACK_(0+4),bvi1))) # (sbit bv bvi1) abfragen
                              # falls Bit=0: dieses Element ist auch noch nicht gestrichen.
                              # vergleiche beide Elemente:
                              { pushSTACK(STACK_(6+3+4)); # sequence
                                pushSTACK(STACK_(0+1)); # pointer1
                                funcall(seq_access(STACK_(2+4+2)),2); # (SEQ-ACCESS sequence pointer1)
                                pushSTACK(value1);
                                funcall(STACK_(2+3+4+1),1); # (FUNCALL key (SEQ-ACCESS sequence pointer1))
                                # value1 =: item1
                                # item1 und item2 vergleichen:
                                if ((*up2_fun)(stackptr,value1,STACK_1)) # Testroutine aufrufen
                                  # Test erfüllt -> vermerke, daß item1 zu streichen ist:
                                  { sbvector_bset(STACK_(0+4),bvi1); # (setf (sbit bv bvi1) 1)
                                    dl = dl+1; # dl:=dl+1
                                  }
                              }
                            # pointer1 := (SEQ-UPD sequence pointer1) :
                            pointer_update(STACK_0,STACK_(6+3+4),STACK_(2+4));
                            bvi1++; # bvi1 := bvi1+1
                       }  }
                        skipSTACK(2); # item2 und pointer1 vergessen
                      }
                    # falls Bit=1: dieses Element einfach übergehen
                    # pointer2 := (SEQ-UPD sequence pointer2) :
                    pointer_update(STACK_0,STACK_(6+3+2),STACK_(2+2));
                    bvi2++; # bvi2 := bvi2+1
               }  }
                skipSTACK(2); # pointer0 und pointer2 vergessen
              }
          }
          else
          # Methode mit Hash-Tabelle
          { # mit (MAKE-HASH-TABLE :test test) eine leere Hash-Tabelle bauen:
            pushSTACK(S(Ktest)); pushSTACK(STACK_(1+3+1)); funcall(L(make_hash_table),2);
            pushSTACK(value1); # ht retten
            {pushSTACK(STACK_(6+3+1)); # sequence
             pushSTACK(STACK_(4+3+2)); # start
             funcall(seq_init_start(STACK_(2+3)),2); # (SEQ-INIT-START sequence start)
             pushSTACK(value1); # =: pointer
            }
            # Stackaufbau:
            #   sequence [stackptr], from-end, start, end, key, test, test-not,
            #   typdescr, l, bv,
            #   ht, pointer.
            if (!(nullp(STACK_(5+3+2)))) # from-end abfragen
              # from-end ist angegeben
              { # pointer läuft von links nach rechts (von start bis end).
                var reg2 uintL bvi = 0; # Schleife bvl mal durchlaufen
                until (bvi==bvl)
                  {{pushSTACK(STACK_(6+3+2)); # sequence
                    pushSTACK(STACK_(0+1)); # pointer
                    funcall(seq_access(STACK_(2+2+2)),2); # (SEQ-ACCESS sequence pointer)
                    pushSTACK(value1);
                    funcall(STACK_(2+3+2+1),1); # (FUNCALL key (SEQ-ACCESS sequence pointer))
                   }# item wird in die Tabelle gesteckt; war es schon
                    # drin, wird bei pointer gestrichen.
                   {var reg1 object old_value = shifthash(STACK_1,value1,T);
                    if (!nullp(old_value))
                      # item war schon in ht -> wird jetzt gestrichen
                      { sbvector_bset(STACK_(0+2),bvi); # (setf (sbit bv bvi) 1)
                        dl = dl+1; # dl:=dl+1
                   }  }
                    # pointer := (SEQ-UPD sequence pointer) :
                    pointer_update(STACK_0,STACK_(6+3+2),STACK_(2+2));
                    bvi++; # bvi := bvi+1
                  }
              }
              else
              # from-end ist nicht angegeben
              { # pointer läuft von links nach rechts (von start bis end).
                var reg2 uintL bvi = 0; # Schleife bvl mal durchlaufen
                until (bvi==bvl)
                  {{pushSTACK(STACK_(6+3+2)); # sequence
                    pushSTACK(STACK_(0+1)); # pointer
                    funcall(seq_access(STACK_(2+2+2)),2); # (SEQ-ACCESS sequence pointer)
                    pushSTACK(value1);
                    funcall(STACK_(2+3+2+1),1); # (FUNCALL key (SEQ-ACCESS sequence pointer))
                   }# item wird in die Tabelle gesteckt; war es schon
                    # drin, wird an der vorigen Position gestrichen.
                   {var reg1 object old_value =
                      shifthash(STACK_1,value1,fixnum(bvi));
                    if (!nullp(old_value))
                      # item war schon in ht -> wird an der vorigen Position gestrichen
                      { var reg1 uintL i = posfixnum_to_L(old_value);
                        sbvector_bset(STACK_(0+2),i); # (setf (sbit bv i) 1)
                        dl = dl+1; # dl:=dl+1
                   }  }
                    # pointer := (SEQ-UPD sequence pointer) :
                    pointer_update(STACK_0,STACK_(6+3+2),STACK_(2+2));
                    bvi++; # bvi := bvi+1
                  }
              }
            skipSTACK(2); # ht und pointer vergessen
          }
        # Stackaufbau:
        #   sequence [stackptr], from-end, start, end, key, test, test-not,
        #   typdescr, l, bv.
        value1 = (*help_fun)(stackptr,bvl,dl); # Rest durchführen
        mv_count=1; # Ergebnis als Wert
        skipSTACK(7+3); # STACK aufräumen
    }}}

LISPFUN(remove_duplicates,1,0,norest,key,6,\
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not)) )
# (REMOVE-DUPLICATES sequence [:from-end] [:start] [:end] [:key] [:test] [:test-not]),
# CLTL S. 254
  { return_Values seq_duplicates(&remove_help); }

LISPFUN(delete_duplicates,1,0,norest,key,6,\
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not)) )
# (DELETE-DUPLICATES sequence [:from-end] [:start] [:end] [:key] [:test] [:test-not]),
# CLTL S. 254
  { return_Values seq_duplicates(&delete_help); }

# UP: Hilfsroutine für SUBSTITUTE-Funktionen.
# Bildet zu einer Sequence eine neue Sequence, in der genau die Elemente
# ersetzt sind, die in einem Bitvektor markiert sind.
# > stackptr: Pointer in den Stack, *(stackptr+2)=newitem,
#   *(stackptr+0)=sequence, *(stackptr-2)=start, *(stackptr-3)=end,
# > STACK_2: typdescr,
# > STACK_1: Länge l der Sequence,
# > STACK_0: Bit-Vektor bv,
# > bvl: Länge des Bit-Vektors (= end - start),
# > dl: Anzahl der im Bit-Vektor gesetzten Bits,
# < ergebnis: Ergebnis
# kann GC auslösen
  local object substitute_help (object* stackptr, uintL bvl, uintL dl);
  local object substitute_help(stackptr,bvl,dl)
    var reg4 object* stackptr;
    var reg5 uintL bvl;
    var reg6 uintL dl;
    { # dl=0 -> sequence unverändert zurückgeben:
      if (dl==0) { return *(stackptr STACKop 0); }
      if (eq(seq_type(STACK_2),S(list))) # Typ LIST ?
        # Noch überprüfen, ob sequence wirklich eine Liste ist.
        # Wegen l >= dl > 0 ist zu testen, ob sequence ein Cons ist.
        if (mconsp(*(stackptr STACKop 0)))
          { # Listen speziell behandeln:
            pushSTACK(NIL); # L1 := nil
            pushSTACK(*(stackptr STACKop 0)); # L2 := sequence
            # Stackaufbau: ..., typdescr, l, bv,
            #              L1, L2.
            # Erste start Conses kopieren:
            { var reg3 uintL count = posfixnum_to_L(*(stackptr STACKop -2)); # 0 <= start <= l ==> start ist Fixnum
              dotimesL(count,count,
                { # Hier gilt (revappend L1 L2) = sequence
                  var reg1 object new_cons = allocate_cons();
                  var reg2 object L2 = STACK_0;
                  Car(new_cons) = Car(L2);
                  STACK_0 = Cdr(L2); # L2 := (cdr L2)
                  Cdr(new_cons) = STACK_1; STACK_1 = new_cons; # L1 := (cons ... L1)
                });
            }
            # bvl bis über die letzte Eins im Bit-Vector erniedrigen:
            # (Es gibt Einsen, da dl>0.)
            { var reg2 object bv = STACK_(0+2);
              loop { var reg1 uintL bvl_1 = bvl-1;
                     if (sbvector_btst(bv,bvl_1)) break; #  Bit bvl-1 abfragen
                     bvl = bvl_1; # Bit =0 -> bvl erniedrigen und weitersuchen
            }      }
            # Teilabschnitt kopieren bzw. mit newitem füllen:
            { var reg2 uintL bvi = 0; # bvi := 0
              until (bvi==bvl) # Schleife bvl mal durchlaufen
                { if (sbvector_btst(STACK_(0+2),bvi)) # (sbit bv bvi) abfragen
                    { # Bit =1 -> newitem nehmen
                      pushSTACK(*(stackptr STACKop 2)); # newitem
                    }
                    else
                    { # Bit =0 -> (car L2) nehmen
                      pushSTACK(Car(STACK_0));
                    }
                  {var reg1 object new_cons = allocate_cons();
                   Car(new_cons) = popSTACK(); # mit Obigem als CAR
                   Cdr(new_cons) = STACK_1; STACK_1 = new_cons; # L1 := (cons ... L1)
                  }
                  STACK_0 = Cdr(STACK_0); # L2 := (cdr L2)
                  bvi++; # bvi:=bvi+1
            }   }
            # letzten Teilabschnitt unverändert dazunehmen:
            { var reg1 object L2 = popSTACK();
              var reg2 object L1 = popSTACK();
              return nreconc(L1,L2); # (nreconc L1 L2) als Ergebnis
          } }
      # neue Sequence allozieren:
      pushSTACK(STACK_1); # l
      funcall(seq_make(STACK_(2+1)),1); # (SEQ-MAKE l)
      pushSTACK(value1); # =: sequence2
      # Stackaufbau: ..., typdescr, l, bv, sequence2.
      pushSTACK(*(stackptr STACKop 0)); # sequence
      pushSTACK(STACK_(3+1)); # typdescr
      pushSTACK(STACK_(0+2)); # sequence2
      pushSTACK(STACK_(3+3)); # typdescr
      pushSTACK(*(stackptr STACKop -2)); # start
      # Stackaufbau: ..., typdescr, l, bv, sequence2,
      #              seq1, typdescr1, seq2, typdescr2, start.
      pushSTACK(STACK_4); funcall(seq_init(STACK_(3+1)),1); # (SEQ-INIT sequence)
      pushSTACK(value1); # =: pointer1
      pushSTACK(STACK_(2+1)); funcall(seq_init(STACK_(1+1+1)),1); # (SEQ-INIT sequence2)
      pushSTACK(value1); # =: pointer2
      # Stackaufbau: ..., typdescr, l, bv, sequence2,
      #              seq1, typdescr1, seq2, typdescr2, start,
      #              pointer1, pointer2.
      { # Vorderes Teilstück:
        # Elemente mit Index <start von sequence1 nach sequence2
        # unverändert übertragen:
        copy_seqpart_into();
      }
      { # Mittleres Teilstück:
        var reg2 uintL bvi = 0;
        until (bvi==bvl)
          { var reg1 object item; # zu übernehmendes Element
            if (sbvector_btst(STACK_(1+5+2),bvi)) # (sbit bv bvi) abfragen
              # Bit =1 -> newitem nehmen:
              { item = *(stackptr STACKop 2); }
              else
              # Bit =0 -> Element aus sequence übernehmen:
              { pushSTACK(STACK_(4+2)); pushSTACK(STACK_(1+1));
                funcall(seq_access(STACK_(3+2+2)),2); # (SEQ-ACCESS seq1 pointer1)
                item = value1;
              }
            pushSTACK(STACK_(2+2)); pushSTACK(STACK_(0+1)); pushSTACK(item);
            funcall(seq_access_set(STACK_(1+2+3)),3); # (SEQ-ACCESS-SET seq2 pointer2 ...)
            # pointer1, pointer2, bvi weiterrücken:
            # pointer1 := (SEQ-UPD seq1 pointer1) :
            pointer_update(STACK_1,STACK_(4+2),STACK_(3+2));
            # pointer2 := (SEQ-UPD seq2 pointer2) :
            pointer_update(STACK_0,STACK_(2+2),STACK_(1+2));
            bvi++;
      }   }
      { # Hinteres Teilstück:
        # Elemente mit Index >=end von sequence1 nach sequence2
        # unverändert übertragen:
        STACK_(0+2) = I_I_minus_I(STACK_(2+5+2),*(stackptr STACKop -3)); # (- l end)
        copy_seqpart_into();
      }
      skipSTACK(5+2);
      return popSTACK(); # sequence2 als Ergebnis
    }

LISPFUN(substitute,3,0,norest,key,7,\
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not),kw(count)) )
# (SUBSTITUTE newitem item sequence [:from-end] [:start] [:end] [:key] [:test] [:test-not] [:count]),
# CLTL S. 255
  { var reg1 object* stackptr = &STACK_7;
    var reg2 up_function up_fun = test_test_args(stackptr); # Testfunktion
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    seq_filterop(stackptr,up_fun,&substitute_help); # Filtern
    skipSTACK(3+7+1);
  }

LISPFUN(substitute_if,3,0,norest,key,5,\
        (kw(from_end),kw(start),kw(end),kw(key),kw(count)) )
# (SUBSTITUTE-IF newitem test sequence [:from-end] [:start] [:end] [:key] [:count]),
# CLTL S. 255
  { var reg1 object* stackptr = &STACK_5;
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    seq_filterop(stackptr,&up_if,&substitute_help); # Filtern
    skipSTACK(3+5+1);
  }

LISPFUN(substitute_if_not,3,0,norest,key,5,\
        (kw(from_end),kw(start),kw(end),kw(key),kw(count)) )
# (SUBSTITUTE-IF-NOT newitem test sequence [:from-end] [:start] [:end] [:key] [:count]),
# CLTL S. 255
  { var reg1 object* stackptr = &STACK_5;
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    seq_filterop(stackptr,&up_if_not,&substitute_help); # Filtern
    skipSTACK(3+5+1);
  }

# UP: Hilfsroutine für NSUBSTITUTE-Funktionen im Fall FROM-END.
# Ersetzt in einer Sequence genau die Elemente, die in einem Bitvektor
# markiert sind.
# > stackptr: Pointer in den Stack, *(stackptr+2)=newitem,
#   *(stackptr+0)=sequence, *(stackptr-2)=start, *(stackptr-3)=end,
# > STACK_2: typdescr,
# > STACK_1: Länge l der Sequence,
# > STACK_0: Bit-Vektor bv,
# > bvl: Länge des Bit-Vektors (= end - start),
# > dl: Anzahl der im Bit-Vektor gesetzten Bits,
# < ergebnis: Ergebnis
# kann GC auslösen
  local object nsubstitute_fe_help (object* stackptr, uintL bvl, uintL dl);
  local object nsubstitute_fe_help(stackptr,bvl,dl)
    var reg1 object* stackptr;
    var reg3 uintL bvl;
    var uintL dl;
    { {pushSTACK(*(stackptr STACKop 0)); # sequence
       pushSTACK(*(stackptr STACKop -2)); # start
       funcall(seq_init_start(STACK_(2+2)),2); # (SEQ-INIT-START sequence start)
       pushSTACK(value1); # =: pointer
      }
      # Stackaufbau: ..., typdescr, l, bv,
      #                   pointer.
      {var reg2 uintL bvi = 0; # bvi := 0
       until (bvi==bvl) # Schleife bvl mal durchlaufen
         { if (sbvector_btst(STACK_(0+1),bvi)) # (sbit bv bvi) abfragen
             # Bit =1 -> ersetze Element durch newitem:
             { pushSTACK(*(stackptr STACKop 0)); # sequence
               pushSTACK(STACK_(0+1)); # pointer
               pushSTACK(*(stackptr STACKop 2)); # newitem
               funcall(seq_access_set(STACK_(2+1+3)),3); # (SEQ-ACCESS-SET sequence pointer newitem)
             }
           # pointer := (SEQ-UPD sequence pointer) :
           pointer_update(STACK_0,*(stackptr STACKop 0),STACK_(2+1));
           bvi++; # bvi:=bvi+1
      }  }
      skipSTACK(1); # pointer vergessen
      return *(stackptr STACKop 0); # sequence als Ergebnis
    }

# Macro: endvar := (and end (- end start)) auf den STACK legen
# init_endvar(stackptr);
# > stackptr: Pointer in den Stack, *(stackptr+1)=start, *(stackptr+0)=end
  #define init_endvar(stackptr)  \
    {var reg1 object end = *(stackptr STACKop 0); # end                                   \
     if (!(nullp(end)))                                                                   \
       { end = I_I_minus_I(end,*(stackptr STACKop 1)); } # (- end start), ein Integer >=0 \
     pushSTACK(end);                                                                      \
    }

# Macro: endvar decrementieren falls endvar/=NIL
# decrement_endvar(endvar);
# > object endvar: entweder NIL oder ein Fixnum >0
# < object endvar: entweder immer noch NIL oder (decrementiert) ein Fixnum >=0
  #define decrement_endvar(endvar)  \
    { if (!(nullp(endvar))) # end angegeben ?                \
        { decrement(endvar); } # ja -> endvar := (1- endvar) \
    }

# UP: Führt eine NSUBSTITUTE-Operation durch.
# > Stackaufbau:
#     ... sequence [stackptr] from-end start end key ... count typdescr [STACK]
# > stackptr: Pointer in den Stack, *(stackptr+2)=newitem
# > up_fun: Adresse einer Testfunktion, die wie folgt spezifiziert ist:
#           > stackptr: derselbe Pointer in den Stack,
#           > x: Argument
#           < TRUE, falls der Test erfüllt ist, FALSE sonst.
# > subr_self: Aufrufer (ein SUBR)
# < mv_space/mv_count: Werte
# kann GC auslösen
  local Values nsubstitute_op (object* stackptr, up_function up_fun);
  local Values nsubstitute_op(stackptr,up_fun)
    var reg1 object* stackptr;
    var reg2 up_function up_fun;
    { if (!(nullp(*(stackptr STACKop -1)))) # from-end abfragen
        # from-end ist angegeben -> Bit-Vector erzeugen und dann ersetzen:
        { return_Values seq_filterop(stackptr,up_fun,&nsubstitute_fe_help); }
        else
        # from-end ist nicht angegeben
        { # COUNT-Argument muß NIL oder ein Integer >= 0 sein:
          test_count_arg();
          # Nun sind alle Argumente überprüft.
          pushSTACK(*(stackptr STACKop 0)); # sequence
          pushSTACK(*(stackptr STACKop -4)); # key
          init_endvar(&*(stackptr STACKop -3)); # endvar := (and end (- end start)) auf den Stack
          pushSTACK(STACK_(1+3)); # countdown := count
          # Stackaufbau: ..., count, typdescr,
          #              sequence, key, endvar, countdown.
          {pushSTACK(STACK_3); # sequence
           pushSTACK(*(stackptr STACKop -2)); # start
           funcall(seq_init_start(STACK_(0+4+2)),2); # (SEQ-INIT-START sequence start)
           pushSTACK(value1); # =: pointer
          }
          # Stackaufbau: ..., count, typdescr,
          #              sequence, key, endvar, countdown, pointer.
          # endvar und countdown sind jeweils entweder =NIL oder ein Integer >=0.
          { until (eq(STACK_2,Fixnum_0)) # endvar = 0 ?
                # (also end angegeben und (- end start) Elemente durchlaufen ?)
                # ja -> fertig
              { pushSTACK(STACK_4); pushSTACK(STACK_(0+1));
                funcall(seq_endtest(STACK_(0+5+2)),2); # (SEQ-ENDTEST sequence pointer)
                if (!(nullp(value1))) break; # Pointer am Ende -> fertig
                if (eq(STACK_1,Fixnum_0)) # countdown=0 ?
                  # (also count angegeben und erschöpft?)
                  break; # ja -> Schleife kann abgebrochen werden
                # item herausgreifen:
                pushSTACK(STACK_4); pushSTACK(STACK_(0+1));
                funcall(seq_access(STACK_(0+5+2)),2); # (SEQ-ACCESS sequence pointer)
                pushSTACK(value1);
                funcall(STACK_(3+1),1); # (FUNCALL key (SEQ-ACCESS sequence pointer))
                # value1 =: item
                if ((*up_fun)(stackptr,value1)) # Testroutine aufrufen
                  # Test ist erfüllt
                  { pushSTACK(STACK_4); pushSTACK(STACK_(0+1));
                    pushSTACK(*(stackptr STACKop 2)); # newitem
                    funcall(seq_access_set(STACK_(0+5+3)),3); # (SEQ-ACCESS-SET sequence pointer newitem)
                    if (!(nullp(STACK_(1+5)))) # falls count/=NIL:
                      { decrement(STACK_1); } # (decf countdown)
                  }
                # pointer := (SEQ-UPD sequence pointer) :
                pointer_update(STACK_0,STACK_4,STACK_(0+5));
                # endvar eventuell decrementieren:
                decrement_endvar(STACK_2);
          }   }
          skipSTACK(4);
          value1 = popSTACK(); mv_count=1; # modifizierte Sequence als Wert
        }
    }

LISPFUN(nsubstitute,3,0,norest,key,7,\
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not),kw(count)) )
# (NSUBSTITUTE newitem item sequence [:from-end] [:start] [:end] [:key] [:test] [:test-not] [:count]),
# CLTL S. 256
  { var reg1 object* stackptr = &STACK_7;
    var reg2 up_function up_fun = test_test_args(stackptr); # Testfunktion
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    nsubstitute_op(stackptr,up_fun); # gefiltert ersetzen
    skipSTACK(3+7+1);
  }

LISPFUN(nsubstitute_if,3,0,norest,key,5,\
        (kw(from_end),kw(start),kw(end),kw(key),kw(count)) )
# (NSUBSTITUTE-IF newitem test sequence [:from-end] [:start] [:end] [:key] [:count]),
# CLTL S. 256
  { var reg1 object* stackptr = &STACK_5;
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    nsubstitute_op(stackptr,&up_if); # gefiltert ersetzen
    skipSTACK(3+5+1);
  }

LISPFUN(nsubstitute_if_not,3,0,norest,key,5,\
        (kw(from_end),kw(start),kw(end),kw(key),kw(count)) )
# (NSUBSTITUTE-IF-NOT newitem test sequence [:from-end] [:start] [:end] [:key] [:count]),
# CLTL S. 256
  { var reg1 object* stackptr = &STACK_5;
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    nsubstitute_op(stackptr,&up_if_not); # gefiltert ersetzen
    skipSTACK(3+5+1);
  }

# UP: Führt eine FIND-Operation durch.
# > Stackaufbau:
#     ... sequence [stackptr] from-end start end key ... typdescr [STACK]
# > stackptr: Pointer in den Stack
# > up_fun: Adresse einer Testfunktion, die wie folgt spezifiziert ist:
#           > stackptr: derselbe Pointer in den Stack,
#           > x: Argument
#           < TRUE, falls der Test erfüllt ist, FALSE sonst.
# > subr_self: Aufrufer (ein SUBR)
# < mv_space/mv_count: Werte
# kann GC auslösen
  local Values find_op (object* stackptr, up_function up_fun);
  local Values find_op(stackptr,up_fun)
    var reg1 object* stackptr;
    var reg2 up_function up_fun;
    { pushSTACK(*(stackptr STACKop 0)); # sequence
      # Stackaufbau: ..., typdescr, sequence.
      if (!(nullp(*(stackptr STACKop -1)))) # from-end abfragen
        # from-end ist angegeben
        { # Defaultwert für end ist die Länge der Sequence:
          if (nullp(*(stackptr STACKop -3)))
            { { var reg4 object old_subr_self = subr_self; # aktuelles SUBR, nicht GC-gefährdet!
                pushSTACK(STACK_0); funcall(seq_length(STACK_(1+1)),1); # (SEQ-LENGTH sequence)
                *(stackptr STACKop -3) = value1; # =: end
                subr_self = old_subr_self;
              }
              # Dann nochmals start und end überprüfen:
              test_start_end(&O(kwpair_start),&*(stackptr STACKop -3));
            }
          {pushSTACK(STACK_0); pushSTACK(*(stackptr STACKop -3));
           funcall(seq_fe_init_end(STACK_(1+2)),2); # (SEQ-FE-INIT-END sequence end)
           pushSTACK(value1); # =: pointer
          }
          { # count := (- end start), ein Integer >=0 :
            pushSTACK(I_I_minus_I(*(stackptr STACKop -3),*(stackptr STACKop -2)));
          }
          # Stackaufbau: ..., typdescr, sequence, pointer, count.
          { until (eq(STACK_0,Fixnum_0)) # count (ein Integer) = 0 -> fertig
              { # item herausgreifen:
                pushSTACK(STACK_2); pushSTACK(STACK_(1+1));
                funcall(seq_access(STACK_(3+2)),2); # (SEQ-ACCESS sequence pointer)
                pushSTACK(value1); # =: item
                pushSTACK(STACK_0); funcall(*(stackptr STACKop -4),1); # (FUNCALL key item)
                if ((*up_fun)(stackptr,value1)) # Testroutine aufrufen
                  goto found; # Test erfüllt -> gefunden
                # Test ist nicht erfüllt
                skipSTACK(1); # item vergessen
                # pointer weiterrücken und count decrementieren:
                # pointer := (SEQ-FE-UPD sequence pointer) :
                pointer_fe_update(STACK_1,STACK_2,STACK_3);
                decrement(STACK_0); # count := (1- count)
        } }   }
        else
        # from-end ist nicht angegeben
        { init_endvar(&*(stackptr STACKop -3)); # endvar := (and end (- end start)) auf den Stack
          # Stackaufbau: ..., typdescr, sequence, endvar.
          {pushSTACK(STACK_1); pushSTACK(*(stackptr STACKop -2));
           funcall(seq_init_start(STACK_(2+2)),2); # (SEQ-INIT-START sequence start)
           pushSTACK(value1); # =: pointer
          }
          # Stackaufbau: ... typdescr, sequence, endvar, pointer
          { until (eq(STACK_1,Fixnum_0)) # endvar = 0 ?
                # (also end angegeben und (- end start) Elemente durchlaufen ?)
                # ja -> fertig
              { pushSTACK(STACK_2); pushSTACK(STACK_(0+1));
                funcall(seq_endtest(STACK_(3+2)),2); # (SEQ-ENDTEST sequence pointer)
                if (!(nullp(value1))) break; # Pointer am Ende -> fertig
                # item herausgreifen:
                pushSTACK(STACK_2); pushSTACK(STACK_(0+1));
                funcall(seq_access(STACK_(3+2)),2); # (SEQ-ACCESS sequence pointer)
                pushSTACK(value1); # =: item
                pushSTACK(STACK_0); funcall(*(stackptr STACKop -4),1); # (FUNCALL key item)
                if ((*up_fun)(stackptr,value1)) # Testroutine aufrufen
                  goto found; # Test erfüllt -> gefunden
                # Test ist nicht erfüllt
                skipSTACK(1); # item vergessen
                # pointer := (SEQ-UPD sequence pointer) :
                pointer_update(STACK_0,STACK_2,STACK_3);
                # endvar eventuell decrementieren:
                decrement_endvar(STACK_1);
        } }   }
      skipSTACK(3); # STACK aufräumen
      value1 = NIL; mv_count=1; return; # NIL als Wert
      found: # item gefunden, das den Test erfüllt. STACK_0 = item.
      value1 = popSTACK(); mv_count=1; # item als Wert
      skipSTACK(3); # STACK aufräumen
    }

LISPFUN(find,2,0,norest,key,6,\
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not)) )
# (FIND item sequence [:from-end] [:start] [:end] [:key] [:test] [:test-not]),
# CLTL S. 257
  { var reg1 object* stackptr = &STACK_6;
    var reg2 up_function up_fun = test_test_args(stackptr); # Testfunktion
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    find_op(stackptr,up_fun); # suchen
    skipSTACK(2+6+1);
  }

LISPFUN(find_if,2,0,norest,key,4,\
        (kw(from_end),kw(start),kw(end),kw(key)) )
# (FIND-IF test sequence [:from-end] [:start] [:end] [:key]),
# CLTL S. 257
  { var reg1 object* stackptr = &STACK_4;
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    find_op(stackptr,&up_if); # suchen
    skipSTACK(2+4+1);
  }

LISPFUN(find_if_not,2,0,norest,key,4,\
        (kw(from_end),kw(start),kw(end),kw(key)) )
# (FIND-IF-NOT test sequence [:from-end] [:start] [:end] [:key]),
# CLTL S. 257
  { var reg1 object* stackptr = &STACK_4;
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    find_op(stackptr,&up_if_not); # suchen
    skipSTACK(2+4+1);
  }

# UP: Führt eine POSITION-Operation durch.
# > Stackaufbau:
#     ... sequence [stackptr] from-end start end key ... typdescr [STACK]
# > stackptr: Pointer in den Stack
# > up_fun: Adresse einer Testfunktion, die wie folgt spezifiziert ist:
#           > stackptr: derselbe Pointer in den Stack,
#           > x: Argument
#           < TRUE, falls der Test erfüllt ist, FALSE sonst.
# > subr_self: Aufrufer (ein SUBR)
# < mv_space/mv_count: Werte
# kann GC auslösen
  local Values position_op (object* stackptr, up_function up_fun);
  local Values position_op(stackptr,up_fun)
    var reg1 object* stackptr;
    var reg2 up_function up_fun;
    { pushSTACK(*(stackptr STACKop 0)); # sequence
      # Stackaufbau: ..., typdescr, sequence.
      if (!(nullp(*(stackptr STACKop -1)))) # from-end abfragen
        # from-end ist angegeben
        { # Defaultwert für end ist die Länge der Sequence:
          if (nullp(*(stackptr STACKop -3)))
            { { var reg4 object old_subr_self = subr_self; # aktuelles SUBR, nicht GC-gefährdet!
                pushSTACK(STACK_0); funcall(seq_length(STACK_(1+1)),1); # (SEQ-LENGTH sequence)
                *(stackptr STACKop -3) = value1; # =: end
                subr_self = old_subr_self;
              }
              # Dann nochmals start und end überprüfen:
              test_start_end(&O(kwpair_start),&*(stackptr STACKop -3));
            }
          pushSTACK(*(stackptr STACKop -3)); # index := end
          {pushSTACK(STACK_(0+1)); pushSTACK(*(stackptr STACKop -3));
           funcall(seq_fe_init_end(STACK_(1+1+2)),2); # (SEQ-FE-INIT-END sequence end)
           pushSTACK(value1); # =: pointer
          }
          { # count := (- end start), ein Integer >=0 :
            pushSTACK(I_I_minus_I(*(stackptr STACKop -3),*(stackptr STACKop -2)));
          }
          # Stackaufbau: ..., typdescr, sequence, index, pointer, count.
          { until (eq(STACK_0,Fixnum_0)) # count (ein Integer) = 0 -> fertig
              { # index decrementieren:
                decrement(STACK_2);
                # item herausgreifen:
                pushSTACK(STACK_3); pushSTACK(STACK_(1+1));
                funcall(seq_access(STACK_(4+2)),2); # (SEQ-ACCESS sequence pointer)
                pushSTACK(value1); funcall(*(stackptr STACKop -4),1); # (FUNCALL key (SEQ-ACCESS sequence pointer))
                if ((*up_fun)(stackptr,value1)) # Testroutine aufrufen
                  goto found; # Test erfüllt -> gefunden
                # Test ist nicht erfüllt
                # pointer weiterrücken und count decrementieren:
                # pointer := (SEQ-FE-UPD sequence pointer) :
                pointer_fe_update(STACK_1,STACK_3,STACK_4);
                decrement(STACK_0); # count := (1- count)
        } }   }
        else
        # from-end ist nicht angegeben
        { pushSTACK(*(stackptr STACKop -2)); # index := start
          init_endvar(&*(stackptr STACKop -3)); # endvar := (and end (- end start)) auf den Stack
          # Stackaufbau: ..., typdescr, sequence, index, endvar.
          {pushSTACK(STACK_2); pushSTACK(*(stackptr STACKop -2));
           funcall(seq_init_start(STACK_(3+2)),2); # (SEQ-INIT-START sequence start)
           pushSTACK(value1); # =: pointer
          }
          # Stackaufbau: ... typdescr, sequence, index, endvar, pointer
          { until (eq(STACK_1,Fixnum_0)) # endvar = 0 ?
                # (also end angegeben und (- end start) Elemente durchlaufen ?)
                # ja -> fertig
              { pushSTACK(STACK_3); pushSTACK(STACK_(0+1));
                funcall(seq_endtest(STACK_(4+2)),2); # (SEQ-ENDTEST sequence pointer)
                if (!(nullp(value1))) break; # Pointer am Ende -> fertig
                # item herausgreifen:
                pushSTACK(STACK_3); pushSTACK(STACK_(0+1));
                funcall(seq_access(STACK_(4+2)),2); # (SEQ-ACCESS sequence pointer)
                pushSTACK(value1); funcall(*(stackptr STACKop -4),1); # (FUNCALL key (SEQ-ACCESS sequence pointer))
                if ((*up_fun)(stackptr,value1)) # Testroutine aufrufen
                  goto found; # Test erfüllt -> gefunden
                # Test ist nicht erfüllt
                # pointer := (SEQ-UPD sequence pointer) :
                pointer_update(STACK_0,STACK_3,STACK_4);
                # endvar eventuell decrementieren:
                decrement_endvar(STACK_1);
                # index incrementieren:
                increment(STACK_2);
        } }   }
      skipSTACK(4); # STACK aufräumen
      value1 = NIL; mv_count=1; return; # NIL als Wert
      found: # item gefunden, das den Test erfüllt. STACK_2 = index.
      value1 = STACK_2; mv_count=1; # index als Wert
      skipSTACK(4); # STACK aufräumen
    }

LISPFUN(position,2,0,norest,key,6,\
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not)) )
# (POSITION item sequence [:from-end] [:start] [:end] [:key] [:test] [:test-not]),
# CLTL S. 257
  { var reg1 object* stackptr = &STACK_6;
    var reg2 up_function up_fun = test_test_args(stackptr); # Testfunktion
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    position_op(stackptr,up_fun); # suchen
    skipSTACK(2+6+1);
  }

LISPFUN(position_if,2,0,norest,key,4,\
        (kw(from_end),kw(start),kw(end),kw(key)) )
# (POSITION-IF test sequence [:from-end] [:start] [:end] [:key]),
# CLTL S. 257
  { var reg1 object* stackptr = &STACK_4;
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    position_op(stackptr,&up_if); # suchen
    skipSTACK(2+4+1);
  }

LISPFUN(position_if_not,2,0,norest,key,4,\
        (kw(from_end),kw(start),kw(end),kw(key)) )
# (POSITION-IF-NOT test sequence [:from-end] [:start] [:end] [:key]),
# CLTL S. 257
  { var reg1 object* stackptr = &STACK_4;
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    position_op(stackptr,&up_if_not); # suchen
    skipSTACK(2+4+1);
  }

# UP: Führt eine COUNT-Operation durch.
# > Stackaufbau:
#     ... sequence [stackptr] from-end start end key ... typdescr [STACK]
# > stackptr: Pointer in den Stack
# > up_fun: Adresse einer Testfunktion, die wie folgt spezifiziert ist:
#           > stackptr: derselbe Pointer in den Stack,
#           > x: Argument
#           < TRUE, falls der Test erfüllt ist, FALSE sonst.
# > subr_self: Aufrufer (ein SUBR)
# < mv_space/mv_count: Werte
# kann GC auslösen
  local Values count_op (object* stackptr, up_function up_fun);
  local Values count_op(stackptr,up_fun)
    var reg1 object* stackptr;
    var reg2 up_function up_fun;
    { pushSTACK(*(stackptr STACKop 0)); # sequence
      pushSTACK(Fixnum_0); # total := 0
      # Stackaufbau: ..., typdescr, sequence, total.
      if (!(nullp(*(stackptr STACKop -1)))) # from-end abfragen
        # from-end ist angegeben
        { # Defaultwert für end ist die Länge der Sequence:
          if (nullp(*(stackptr STACKop -3)))
            { { var reg4 object old_subr_self = subr_self; # aktuelles SUBR, nicht GC-gefährdet!
                pushSTACK(STACK_1); funcall(seq_length(STACK_(2+1)),1); # (SEQ-LENGTH sequence)
                *(stackptr STACKop -3) = value1; # =: end
                subr_self = old_subr_self;
              }
              # Dann nochmals start und end überprüfen:
              test_start_end(&O(kwpair_start),&*(stackptr STACKop -3));
            }
          {pushSTACK(STACK_1); pushSTACK(*(stackptr STACKop -3));
           funcall(seq_fe_init_end(STACK_(2+2)),2); # (SEQ-FE-INIT-END sequence end)
           pushSTACK(value1); # =: pointer
          }
          { # count := (- end start), ein Integer >=0 :
            pushSTACK(I_I_minus_I(*(stackptr STACKop -3),*(stackptr STACKop -2)));
          }
          # Stackaufbau: ..., typdescr, sequence, total, pointer, count.
          { until (eq(STACK_0,Fixnum_0)) # count (ein Integer) = 0 -> fertig
              { # item herausgreifen:
                pushSTACK(STACK_3); pushSTACK(STACK_(1+1));
                funcall(seq_access(STACK_(4+2)),2); # (SEQ-ACCESS sequence pointer)
                pushSTACK(value1); funcall(*(stackptr STACKop -4),1); # (FUNCALL key (SEQ-ACCESS sequence pointer))
                if ((*up_fun)(stackptr,value1)) # Testroutine aufrufen
                  { # Test ist erfüllt -> total := total + 1 :
                    STACK_2 = fixnum_inc(STACK_2,1);
                  }
                # pointer weiterrücken und count decrementieren:
                # pointer := (SEQ-FE-UPD sequence pointer) :
                pointer_fe_update(STACK_1,STACK_3,STACK_4);
                decrement(STACK_0); # count := (1- count)
        } }   }
        else
        # from-end ist nicht angegeben
        { init_endvar(&*(stackptr STACKop -3)); # endvar := (and end (- end start)) auf den Stack
          # Stackaufbau: ..., typdescr, sequence, total, endvar.
          {pushSTACK(STACK_2); pushSTACK(*(stackptr STACKop -2));
           funcall(seq_init_start(STACK_(3+2)),2); # (SEQ-INIT-START sequence start)
           pushSTACK(value1); # =: pointer
          }
          # Stackaufbau: ... typdescr, sequence, total, endvar, pointer
          { until (eq(STACK_1,Fixnum_0)) # endvar = 0 ?
                # (also end angegeben und (- end start) Elemente durchlaufen ?)
                # ja -> fertig
              { pushSTACK(STACK_3); pushSTACK(STACK_(0+1));
                funcall(seq_endtest(STACK_(4+2)),2); # (SEQ-ENDTEST sequence pointer)
                if (!(nullp(value1))) break; # Pointer am Ende -> fertig
                # item herausgreifen:
                pushSTACK(STACK_3); pushSTACK(STACK_(0+1));
                funcall(seq_access(STACK_(4+2)),2); # (SEQ-ACCESS sequence pointer)
                pushSTACK(value1); funcall(*(stackptr STACKop -4),1); # (FUNCALL key (SEQ-ACCESS sequence pointer))
                if ((*up_fun)(stackptr,value1)) # Testroutine aufrufen
                  { # Test ist erfüllt -> total := total + 1 :
                    STACK_2 = fixnum_inc(STACK_2,1);
                  }
                # pointer := (SEQ-UPD sequence pointer) :
                pointer_update(STACK_0,STACK_3,STACK_4);
                # endvar eventuell decrementieren:
                decrement_endvar(STACK_1);
        } }   }
      value1 = STACK_2; mv_count=1; skipSTACK(4); # total als Wert
    }

LISPFUN(count,2,0,norest,key,6,\
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not)) )
# (COUNT item sequence [:from-end] [:start] [:end] [:key] [:test] [:test-not]),
# CLTL S. 257
  { var reg1 object* stackptr = &STACK_6;
    var reg2 up_function up_fun = test_test_args(stackptr); # Testfunktion
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    count_op(stackptr,up_fun); # suchen
    skipSTACK(2+6+1);
  }

LISPFUN(count_if,2,0,norest,key,4,\
        (kw(from_end),kw(start),kw(end),kw(key)) )
# (COUNT-IF test sequence [:from-end] [:start] [:end] [:key]),
# CLTL S. 257
  { var reg1 object* stackptr = &STACK_4;
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    count_op(stackptr,&up_if); # suchen
    skipSTACK(2+4+1);
  }

LISPFUN(count_if_not,2,0,norest,key,4,\
        (kw(from_end),kw(start),kw(end),kw(key)) )
# (COUNT-IF-NOT test sequence [:from-end] [:start] [:end] [:key]),
# CLTL S. 257
  { var reg1 object* stackptr = &STACK_4;
    seq_prepare_testop(stackptr); # Argumente aufbereiten, typdescr
    count_op(stackptr,&up_if_not); # suchen
    skipSTACK(2+4+1);
  }

LISPFUN(mismatch,2,0,norest,key,8,\
        (kw(start1),kw(end1),kw(start2),kw(end2),kw(from_end),\
         kw(key),kw(test),kw(test_not)) )
# (MISMATCH sequence1 sequence2
#           [:start1] [:end1] [:start2] [:end2] [:from-end] [:key] [:test] [:test-not]),
# CLTL S. 257
  { # Stackaufbau: seq1, seq2, start1, end1, start2, end2, from-end,
    #              key, test, test-not.
    var reg2 object* stackptr = &STACK_6;
    # key überprüfen:
    test_key_arg(stackptr);
    # test, test-not überprüfen:
   {var reg1 up2_function up2_fun = test_test2_args(stackptr);
    # sequence1 überprüfen:
    pushSTACK(get_valid_seq_type(STACK_(6+3)));
    # sequence2 überprüfen:
    pushSTACK(get_valid_seq_type(STACK_(5+3+1)));
    # Stackaufbau: seq1, seq2, start1, end1, start2, end2, from-end,
    #              key, test, test-not, typdescr1, typdescr2.
    default_NIL(STACK_(0+5)); # Defaultwert für from-end ist NIL
    start_default_0(STACK_(4+5)); # Defaultwert für start1 ist 0
    default_NIL(STACK_(3+5)); # Defaultwert für end1 ist NIL
    start_default_0(STACK_(2+5)); # Defaultwert für start2 ist 0
    default_NIL(STACK_(1+5)); # Defaultwert für end2 ist NIL
    # from-end abfragen:
    if (!(nullp(STACK_(0+5))))
      # from-end ist angegeben
      { # Defaultwert von end1 ist (SEQ-LENGTH seq1):
        end_default_len(STACK_(3+5),STACK_(6+5),STACK_1);
        # Defaultwert von end2 ist (SEQ-LENGTH seq2):
        end_default_len(STACK_(1+5),STACK_(5+5),STACK_0);
        # start- und end-Argumente überprüfen:
        subr_self = L(mismatch);
        test_start_end(&O(kwpair_start1),&STACK_(3+5));
        test_start_end(&O(kwpair_start2),&STACK_(1+5));
        # pointer1 und pointer2 ans Ende der Sequences setzen:
        { pushSTACK(STACK_(6+5)); pushSTACK(STACK_(3+5+1));
          funcall(seq_fe_init_end(STACK_(1+2)),2); # (SEQ-FE-INIT-END seq1 end1)
          pushSTACK(value1); # =: pointer1
        }
        { pushSTACK(STACK_(5+5+1)); pushSTACK(STACK_(1+5+1+1));
          funcall(seq_fe_init_end(STACK_(0+1+2)),2); # (SEQ-FE-INIT-END seq2 end2)
          pushSTACK(value1); # =: pointer2
        }
        { pushSTACK(STACK_(3+5+2)); } # index := end1
        { var reg1 object len1 = I_I_minus_I(STACK_(3+5+3),STACK_(4+5+3)); # (- end1 start1)
          pushSTACK(len1); # =: len1, ein Integer >=0
        }
        { var reg1 object len2 = I_I_minus_I(STACK_(1+5+4),STACK_(2+5+4)); # (- end2 start2)
          pushSTACK(len2); # =: len2, ein Integer >=0
        }
        { var reg1 object count = (I_I_comp(STACK_1,STACK_0)<0 ? STACK_1 : STACK_0); # (min len1 len2)
          pushSTACK(count); # =: count, ein Integer >=0
        }
        # Stackaufbau: seq1, seq2, start1, end1, start2, end2, from-end,
        #              key, test, test-not, typdescr1, typdescr2,
        #              pointer1, pointer2, index, len1, len2, count.
        until (eq(STACK_0,Fixnum_0)) # count (ein Integer) = 0 ?
          { pushSTACK(STACK_(6+5+6)); pushSTACK(STACK_(5+1));
            funcall(seq_access(STACK_(1+6+2)),2); # (SEQ-ACCESS seq1 pointer1)
            pushSTACK(value1); funcall(STACK_(4+6+1),1); # (FUNCALL key (SEQ-ACCESS seq1 pointer1))
            pushSTACK(value1); # =: item1, retten
            pushSTACK(STACK_(5+5+6+1)); pushSTACK(STACK_(4+1+1));
            funcall(seq_access(STACK_(0+6+1+2)),2); # (SEQ-ACCESS seq2 pointer2)
            pushSTACK(value1); funcall(STACK_(4+6+1+1),1); # (FUNCALL key (SEQ-ACCESS seq2 pointer2))
            {var reg2 object item2 = value1;
             var reg3 object item1 = popSTACK();
             # beide vergleichen:
             if (!((*up2_fun)(&STACK_(8+6),item1,item2))) # Testroutine anwenden
               goto fe_found;
            }
            # Test erfüllt -> weitersuchen:
            # pointer1 := (SEQ-FE-UPD seq1 pointer1) :
            pointer_fe_update(STACK_5,STACK_(6+5+6),STACK_(1+6));
            # pointer2 := (SEQ-FE-UPD seq2 pointer2) :
            pointer_fe_update(STACK_4,STACK_(5+5+6),STACK_(0+6));
            # index decrementieren:
            decrement(STACK_3);
            # count decrementieren:
            decrement(STACK_0);
          }
        # Schleife erfolgreich.
        # Bei len1=len2 Ergebnis NIL, sonst index:
        if (I_I_comp(STACK_2,STACK_1)==0) # len1=len2 (Integers) ?
          # Beide Sequence-Stücke sind gleich -> NIL als Wert
          { value1 = NIL; mv_count=1; skipSTACK(7+5+6); return; }
        fe_found: # Es ist ein Unterschied gefunden -> index als Wert
        { value1 = STACK_3; mv_count=1; skipSTACK(7+5+6); return; }
      }
      else
      # from-end ist nicht angegeben
      { # start- und end-Argumente überprüfen:
        test_start_end_1(&O(kwpair_start1),&STACK_(3+5));
        test_start_end_1(&O(kwpair_start2),&STACK_(1+5));
        # pointer1 und pointer2 an den Anfang der Sequences setzen:
        { pushSTACK(STACK_(6+5)); pushSTACK(STACK_(4+5+1));
          funcall(seq_init_start(STACK_(1+2)),2); # (SEQ-INIT-START seq1 start1)
          pushSTACK(value1); # =: pointer1
        }
        { pushSTACK(STACK_(5+5+1)); pushSTACK(STACK_(2+5+1+1));
          funcall(seq_init_start(STACK_(0+1+2)),2); # (SEQ-INIT-START seq2 start2)
          pushSTACK(value1); # =: pointer2
        }
        { pushSTACK(STACK_(4+5+2)); } # index := start1
        init_endvar(&STACK_(3+5+3)); # endvar1 := (and end1 (- end1 start1))
        init_endvar(&STACK_(1+5+4)); # endvar2 := (and end2 (- end2 start2))
        # Stackaufbau: seq1, seq2, start1, end1, start2, end2, from-end,
        #              key, test, test-not, typdescr1, typdescr2,
        #              pointer1, pointer2, index, endvar1, endvar2.
        { var reg3 boolean seq1_ended; # Flag, ob seq1-Teilstück zu Ende
          var reg4 boolean seq2_ended; # Flag, ob seq2-Teilstück zu Ende
          loop
            { # Teste, ob seq1-Teilstück zu Ende:
              if (eq(STACK_1,Fixnum_0)) # endvar1 = 0 (und damit end1 /= nil) ?
                { seq1_ended = TRUE; }
                else
                { pushSTACK(STACK_(6+5+5)); pushSTACK(STACK_(4+1));
                  funcall(seq_endtest(STACK_(1+5+2)),2); # (SEQ-ENDTEST seq1 pointer1)
                  seq1_ended = !nullp(value1);
                }
              # Teste, ob seq2-Teilstück zu Ende:
              if (eq(STACK_0,Fixnum_0)) # endvar2 = 0 (und damit end2 /= nil) ?
                { seq2_ended = TRUE; }
                else
                { pushSTACK(STACK_(5+5+5)); pushSTACK(STACK_(3+1));
                  funcall(seq_endtest(STACK_(0+5+2)),2); # (SEQ-ENDTEST seq2 pointer2)
                  seq2_ended = !nullp(value1);
                }
              # Flags abtesten:
              if (seq1_ended || seq2_ended) break;
              # keines der beiden Flags ist gesetzt
              pushSTACK(STACK_(6+5+5)); pushSTACK(STACK_(4+1));
              funcall(seq_access(STACK_(1+5+2)),2); # (SEQ-ACCESS seq1 pointer1)
              pushSTACK(value1); funcall(STACK_(4+5+1),1); # (FUNCALL key (SEQ-ACCESS seq1 pointer1))
              pushSTACK(value1); # =: item1, retten
              pushSTACK(STACK_(5+5+5+1)); pushSTACK(STACK_(3+1+1));
              funcall(seq_access(STACK_(0+5+1+2)),2); # (SEQ-ACCESS seq2 pointer2)
              pushSTACK(value1); funcall(STACK_(4+5+1+1),1); # (FUNCALL key (SEQ-ACCESS seq2 pointer2))
              {var reg2 object item2 = value1;
               var reg3 object item1 = popSTACK();
               # beide vergleichen:
               if (!((*up2_fun)(&STACK_(8+5),item1,item2))) # Testroutine anwenden
                 goto fs_found;
              }
              # Test erfüllt -> weitersuchen:
              # pointer1 := (SEQ-UPD seq1 pointer1) :
              pointer_update(STACK_4,STACK_(6+5+5),STACK_(1+5));
              # pointer2 := (SEQ-UPD seq2 pointer2) :
              pointer_update(STACK_3,STACK_(5+5+5),STACK_(0+5));
              # index incrementieren:
              increment(STACK_2);
              # endvar1 eventuell decrementieren:
              decrement_endvar(STACK_1);
              # endvar2 eventuell decrementieren:
              decrement_endvar(STACK_0);
            }
          # Falls beide Flags gesetzt sind, Ergebnis NIL, sonst index:
          if (seq1_ended && seq2_ended)
            # Beide Sequence-Stücke sind gleich -> NIL als Wert
            { value1 = NIL; mv_count=1; skipSTACK(7+5+5); return; }
          fs_found: # Es ist ein Unterschied gefunden -> index als Wert
          { value1 = STACK_2; mv_count=1; skipSTACK(7+5+5); return; }
      } }
  }}

LISPFUN(search,2,0,norest,key,8,\
        (kw(start1),kw(end1),kw(start2),kw(end2),kw(from_end),\
         kw(key),kw(test),kw(test_not)) )
# (SEARCH sequence1 sequence2
#         [:start1] [:end1] [:start2] [:end2] [:from-end] [:key] [:test] [:test-not]),
# CLTL S. 258
  # Primitiv-Algorithmus:
  #   Rücke immer in sequence2 um 1 weiter und teste, ob dann sequence1 kommt.
  # Knuth-Algorithmus:
  #   [Donald Ervin Knuth, James H. Morris, Vaughan R. Pratt:
  #    Fast pattern matching in string.
  #    SIAM J. Comput. 6(1977), 323-350.]
  #   Kann hier nicht verwendet werden, weil er die Kommutativität der
  #   Testfunktion erfordert, die nach CLTL S. 247 nicht notwendig gegeben ist.
  { # Stackaufbau: seq1, seq2, start1, end1, start2, end2, from-end,
    #              key, test, test-not.
    var reg2 object* stackptr = &STACK_6;
    # key überprüfen:
    test_key_arg(stackptr);
    # test, test-not überprüfen:
   {var reg1 up2_function up2_fun = test_test2_args(stackptr);
    # sequence1 überprüfen:
    pushSTACK(get_valid_seq_type(STACK_(6+3)));
    # sequence2 überprüfen:
    pushSTACK(get_valid_seq_type(STACK_(5+3+1)));
    # Stackaufbau: seq1, seq2, start1, end1, start2, end2, from-end,
    #              key, test, test-not, typdescr1, typdescr2.
    default_NIL(STACK_(0+5)); # Defaultwert für from-end ist NIL
    # Sonderfall für Strings: schnellere Routine aufrufen
    if (eq(seq_type(STACK_1),S(string)) && eq(seq_type(STACK_0),S(string)) # beides STRINGs ?
        && nullp(STACK_(0+5)) # und kein from-end ?
        && eq(STACK_4,L(identity)) # und key = #'identity ?
        && (up2_fun == &up2_test) # und test-not nicht angegeben ?
       )
      { var reg3 object test = STACK_3;
        if (eq(test,L(eq)) || eq(test,L(eql)) || eq(test,L(equal)) || eq(test,L(char_gleich)))
          { skipSTACK(6);
            C_search_string_gleich(); # SUBR sys::search-string= mit denselben Argumenten
            return;
          }
        if (eq(test,L(equalp)) || eq(test,L(char_equal)))
          { skipSTACK(6);
            C_search_string_equal(); # SUBR sys::search-string-equal mit denselben Argumenten
            return;
      }   }
    start_default_0(STACK_(4+5)); # Defaultwert für start1 ist 0
    default_NIL(STACK_(3+5)); # Defaultwert für end1 ist NIL
    start_default_0(STACK_(2+5)); # Defaultwert für start2 ist 0
    default_NIL(STACK_(1+5)); # Defaultwert für end2 ist NIL
    # from-end abfragen:
    if (!(nullp(STACK_(0+5))))
      # from-end ist angegeben
      { # Defaultwert von end1 ist (SEQ-LENGTH seq1):
        end_default_len(STACK_(3+5),STACK_(6+5),STACK_1);
        # Defaultwert von end2 ist (SEQ-LENGTH seq2):
        end_default_len(STACK_(1+5),STACK_(5+5),STACK_0);
        # start- und end-Argumente überprüfen:
        subr_self = L(search);
        test_start_end(&O(kwpair_start1),&STACK_(3+5));
        test_start_end(&O(kwpair_start2),&STACK_(1+5));
        # pointer10 und pointer20 ans Ende der Sequences setzen:
        { pushSTACK(STACK_(6+5)); pushSTACK(STACK_(3+5+1));
          funcall(seq_fe_init_end(STACK_(1+2)),2); # (SEQ-FE-INIT-END seq1 end1)
          pushSTACK(value1); # =: pointer10
        }
        { pushSTACK(STACK_(5+5+1)); pushSTACK(STACK_(1+5+1+1));
          funcall(seq_fe_init_end(STACK_(0+1+2)),2); # (SEQ-FE-INIT-END seq2 end2)
          pushSTACK(value1); # =: pointer20
        }
        { var reg1 object len1 = I_I_minus_I(STACK_(3+5+2),STACK_(4+5+2)); # (- end1 start1)
          pushSTACK(len1); # =: len1, ein Integer >=0
        }
        { var reg1 object len2 = I_I_minus_I(STACK_(1+5+3),STACK_(2+5+3)); # (- end2 start2)
          pushSTACK(len2); # =: len2, ein Integer >=0
        }
        { var reg1 object index = I_I_minus_I(STACK_(1+5+4),STACK_1); # (- end2 len1)
          pushSTACK(index); # =: index, ein Integer
        }
        # Stackaufbau: seq1, seq2, start1, end1, start2, end2, from-end,
        #              key, test, test-not, typdescr1, typdescr2,
        #              pointer10, pointer20, len1, len2, index.
        loop
          { # pointer1 und pointer2 ab pointer10 bzw. pointer20 laufen lassen:
            { pushSTACK(STACK_4); funcall(seq_copy(STACK_(1+5+1)),1); # (SEQ-COPY pointer10)
              pushSTACK(value1); # =: pointer1
            }
            { pushSTACK(STACK_(3+1)); funcall(seq_copy(STACK_(0+5+1+1)),1); # (SEQ-COPY pointer20)
              pushSTACK(value1); # =: pointer2
            }
            pushSTACK(STACK_(2+2)); # count1 := len1
            pushSTACK(STACK_(1+3)); # count2 := len2
            # Stackaufbau: seq1, seq2, start1, end1, start2, end2, from-end,
            #              key, test, test-not, typdescr1, typdescr2,
            #              pointer10, pointer20, len1, len2, index,
            #              pointer1, pointer2, count1, count2.
            loop
              { if (eq(STACK_1,Fixnum_0)) # count1 (ein Integer) = 0 ?
                  goto found; # ja -> seq1 zu Ende, gefunden
                if (eq(STACK_0,Fixnum_0)) # count2 (ein Integer) = 0 ?
                  goto notfound; # ja -> seq2 zu Ende, nicht gefunden
                pushSTACK(STACK_(6+5+5+4)); pushSTACK(STACK_(3+1));
                funcall(seq_access(STACK_(1+5+4+2)),2); # (SEQ-ACCESS seq1 pointer1)
                pushSTACK(value1); funcall(STACK_(4+5+4+1),1); # (FUNCALL key (SEQ-ACCESS seq1 pointer1))
                pushSTACK(value1); # =: item1, retten
                pushSTACK(STACK_(5+5+5+4+1)); pushSTACK(STACK_(2+1+1));
                funcall(seq_access(STACK_(0+5+4+1+2)),2); # (SEQ-ACCESS seq2 pointer2)
                pushSTACK(value1); funcall(STACK_(4+5+4+1+1),1); # (FUNCALL key (SEQ-ACCESS seq2 pointer2))
                {var reg2 object item2 = value1;
                 var reg3 object item1 = popSTACK();
                 # beide vergleichen:
                 if (!((*up2_fun)(&STACK_(8+5+4),item1,item2))) # Testroutine anwenden
                   break;
                }
                # Test erfüllt -> weitervergleichen:
                # pointer1 := (SEQ-FE-UPD seq1 pointer1) :
                pointer_fe_update(STACK_3,STACK_(6+5+5+4),STACK_(1+5+4));
                # pointer2 := (SEQ-FE-UPD seq2 pointer2) :
                pointer_fe_update(STACK_2,STACK_(5+5+5+4),STACK_(0+5+4));
                # count1 decrementieren:
                decrement(STACK_1);
                # count2 decrementieren:
                decrement(STACK_0);
              }
            # Test nicht erfüllt -> weitersuchen
            skipSTACK(4); # pointer1, pointer2, count1, count2 vergessen
            # pointer20 weiterrücken, len2 und index decrementieren:
            pointer_fe_update(STACK_3,STACK_(6+5+5),STACK_(0+5));
            decrement(STACK_1); # len2 := (1- len2)
            decrement(STACK_0); # index := (1- index)
      }   }
      else
      # from-end ist nicht angegeben
      { # start- und end-Argumente überprüfen:
        test_start_end_1(&O(kwpair_start1),&STACK_(3+5));
        test_start_end_1(&O(kwpair_start2),&STACK_(1+5));
        # pointer10 und pointer20 an den Anfang der Sequences setzen:
        { pushSTACK(STACK_(6+5)); pushSTACK(STACK_(4+5+1));
          funcall(seq_init_start(STACK_(1+2)),2); # (SEQ-INIT-START seq1 start1)
          pushSTACK(value1); # =: pointer10
        }
        { pushSTACK(STACK_(5+5+1)); pushSTACK(STACK_(2+5+1+1));
          funcall(seq_init_start(STACK_(0+1+2)),2); # (SEQ-INIT-START seq2 start2)
          pushSTACK(value1); # =: pointer20
        }
        init_endvar(&STACK_(3+5+2)); # endvar10 := (and end1 (- end1 start1))
        init_endvar(&STACK_(1+5+3)); # endvar20 := (and end2 (- end2 start2))
        pushSTACK(STACK_(2+5+4)); # index := start2
        # Stackaufbau: seq1, seq2, start1, end1, start2, end2, from-end,
        #              key, test, test-not, typdescr1, typdescr2,
        #              pointer10, pointer20, endvar10, endvar20, index.
        loop
          { # pointer1 und pointer2 ab pointer10 bzw. pointer20 laufen lassen:
            { pushSTACK(STACK_4); funcall(seq_copy(STACK_(1+5+1)),1); # (SEQ-COPY pointer10)
              pushSTACK(value1); # =: pointer1
            }
            { pushSTACK(STACK_(3+1)); funcall(seq_copy(STACK_(0+5+1+1)),1); # (SEQ-COPY pointer20)
              pushSTACK(value1); # =: pointer2
            }
            pushSTACK(STACK_(2+2)); # endvar1 := endvar10
            pushSTACK(STACK_(1+3)); # endvar2 := endvar20
            # Stackaufbau: seq1, seq2, from-end, start1, end1, start2, end2,
            #              key, test, test-not, typdescr1, typdescr2,
            #              pointer10, pointer20, endvar10, endvar20, index,
            #              pointer1, pointer2, endvar1, endvar2.
            loop
              { # Teste, ob seq1-Teilstück zu Ende. Wenn ja: gefunden.
                if (eq(STACK_1,Fixnum_0)) # endvar1 = 0 (und damit end1 /= nil) ?
                  { goto found; }
                  else
                  { pushSTACK(STACK_(6+5+5+4)); pushSTACK(STACK_(3+1));
                    funcall(seq_endtest(STACK_(1+5+4+2)),2); # (SEQ-ENDTEST seq1 pointer1)
                    if (!nullp(value1)) goto found;
                  }
                # seq1 ist noch nicht am Ende.
                # Teste, ob seq2-Teilstück zu Ende. Wenn ja: nicht gefunden.
                if (eq(STACK_0,Fixnum_0)) # endvar2 = 0 (und damit end2 /= nil) ?
                  { goto notfound; }
                  else
                  { pushSTACK(STACK_(5+5+5+4)); pushSTACK(STACK_(2+1));
                    funcall(seq_endtest(STACK_(0+5+4+2)),2); # (SEQ-ENDTEST seq2 pointer2)
                    if (!nullp(value1)) goto notfound;
                  }
                # seq2 ist noch nicht am Ende.
                pushSTACK(STACK_(6+5+5+4)); pushSTACK(STACK_(3+1));
                funcall(seq_access(STACK_(1+5+4+2)),2); # (SEQ-ACCESS seq1 pointer1)
                pushSTACK(value1); funcall(STACK_(4+5+4+1),1); # (FUNCALL key (SEQ-ACCESS seq1 pointer1))
                pushSTACK(value1); # =: item1, retten
                pushSTACK(STACK_(5+5+5+4+1)); pushSTACK(STACK_(2+1+1));
                funcall(seq_access(STACK_(0+5+4+1+2)),2); # (SEQ-ACCESS seq2 pointer2)
                pushSTACK(value1); funcall(STACK_(4+5+4+1+1),1); # (FUNCALL key (SEQ-ACCESS seq2 pointer2))
                {var reg2 object item2 = value1;
                 var reg3 object item1 = popSTACK();
                 # beide vergleichen:
                 if (!((*up2_fun)(&STACK_(8+5+4),item1,item2))) # Testroutine anwenden
                   break;
                }
                # Test erfüllt -> weitervergleichen:
                # pointer1 := (SEQ-UPD seq1 pointer1) :
                pointer_update(STACK_3,STACK_(6+5+5+4),STACK_(1+5+4));
                # pointer2 := (SEQ-UPD seq2 pointer2) :
                pointer_update(STACK_2,STACK_(5+5+5+4),STACK_(0+5+4));
                # endvar1 eventuell decrementieren:
                decrement_endvar(STACK_1);
                # endvar2 eventuell decrementieren:
                decrement_endvar(STACK_0);
              }
            # Test nicht erfüllt -> weitersuchen
            skipSTACK(4); # pointer1, pointer2, endvar1, endvar2 vergessen
            # pointer20 weiterrücken:
            pointer_update(STACK_3,STACK_(6+5+5),STACK_(0+5));
            # endvar20 eventuell decrementieren:
            decrement_endvar(STACK_1);
            # index incrementieren:
            increment(STACK_0);
      }   }
    /*NOTREACHED*/
    found: # index als Wert
      { value1 = STACK_4; mv_count=1; skipSTACK(7+5+5+4); return; }
    notfound: # NIL als Wert
      { value1 = NIL; mv_count=1; skipSTACK(7+5+5+4); return; }
  }}

# UP für SORT, STABLE-SORT und MERGE:
# merge(stackptr);
# sortiert zwei sortierte Sequence-Teile in eine dritte Sequence zusammen.
# > STACK_10: sequence1
# > STACK_9: typdescr1
# > STACK_8: sequence2
# > STACK_7: typdescr2
# > STACK_6: sequence3
# > STACK_5: typdescr3
# > STACK_4: count1 (ein Integer >=0)
# > STACK_3: count2 (ein Integer >=0)
# > STACK_2: pointer1
# > STACK_1: pointer2
# > STACK_0: pointer3
# > stackptr: Pointer in den Stack,
#     *(stackptr+0) = predicate, *(stackptr-1) = key
# count1+count2 Elemente aus sequence1 oder sequence2 werden nach sequence3
# übertragen (im Zweifelsfall die aus sequence1 zuerst).
# Dabei wird pointer1 genau  count1  mal weitergerückt (mit SEQ-UPD),
#            pointer2 genau  count2  mal weitergerückt (mit SEQ-UPD),
#            pointer3 genau  count1+count2  mal weitergerückt (mit SEQ-UPD).
# count1 und count2 werden auf 0 gesetzt.
# kann GC auslösen
  local void merge (object* stackptr);
  local void merge(stackptr)
    var reg1 object* stackptr;
    { loop
        { if (eq(STACK_4,Fixnum_0)) goto seq1_end; # count1 = 0 -> seq1 zu Ende
          if (eq(STACK_3,Fixnum_0)) goto seq2_end; # count1 = 0 -> seq2 zu Ende
          # item2 holen:
          { pushSTACK(STACK_8); pushSTACK(STACK_(1+1));
            funcall(seq_access(STACK_(7+2)),2); # (SEQ-ACCESS sequence2 pointer2)
            pushSTACK(value1); funcall(*(stackptr STACKop -1),1); # (FUNCALL key (SEQ-ACCESS sequence2 pointer2))
            pushSTACK(value1); # =: item2
          }
          # item1 holen:
          { pushSTACK(STACK_(10+1)); pushSTACK(STACK_(2+1+1));
            funcall(seq_access(STACK_(9+1+2)),2); # (SEQ-ACCESS sequence1 pointer1)
            pushSTACK(value1); funcall(*(stackptr STACKop -1),1); # (FUNCALL key (SEQ-ACCESS sequence1 pointer1))
            pushSTACK(value1); # =: item1
          }
          funcall(*(stackptr STACKop 0),2); # (FUNCALL predicate item2 item1)
          if (nullp(value1))
            # predicate lieferte NIL, item aus sequence1 übernehmen:
            { pushSTACK(STACK_(10)); pushSTACK(STACK_(2+1));
              funcall(seq_access(STACK_(9+2)),2); # (SEQ-ACCESS sequence1 pointer1)
              pushSTACK(value1); # auf den Stack
              # pointer1 := (SEQ-UPD sequence1 pointer1) :
              pointer_update(STACK_(2+1),STACK_(10+1),STACK_(9+1));
              # count1 := (1- count1) :
              decrement(STACK_(4+1));
            }
            else
            # predicate war erfüllt, item aus sequence2 übernehmen:
            { pushSTACK(STACK_(8)); pushSTACK(STACK_(1+1));
              funcall(seq_access(STACK_(7+2)),2); # (SEQ-ACCESS sequence2 pointer2)
              pushSTACK(value1); # auf den Stack
              # pointer2 := (SEQ-UPD sequence2 pointer2) :
              pointer_update(STACK_(1+1),STACK_(8+1),STACK_(7+1));
              # count2 := (1- count2) :
              decrement(STACK_(3+1));
            }
          {var reg2 object item = popSTACK(); # zu übernehmendes item
           pushSTACK(STACK_6); pushSTACK(STACK_(0+1)); pushSTACK(item);
           funcall(seq_access_set(STACK_(5+3)),3); # (SEQ-ACCESS-SET sequence3 pointer3 item)
          }
          # pointer3 := (SEQ-UPD sequence3 pointer3) :
          pointer_update(STACK_0,STACK_6,STACK_5);
        }
      /*NOTREACHED*/
      seq1_end:
        # sequence1 zu Ende. Rest aus sequence2 übernehmen:
        # Falls sequence2 und sequence3 EQ sind, liegt ein Aufruf
        # von SORT oder STABLE-SORT aus vor. Dort sind dann auch die
        # Pointer pointer2 und pointer3 gleich, also braucht gar nicht
        # mehr kopiert zu werden:
        if (eq(STACK_8,STACK_6)) # sequence2 = sequence3 ?
          { return; }
        until (eq(STACK_3,Fixnum_0)) # count2 = 0 ?
          { pushSTACK(STACK_(8)); pushSTACK(STACK_(1+1));
            funcall(seq_access(STACK_(7+2)),2); # (SEQ-ACCESS sequence2 pointer2)
            pushSTACK(STACK_6); pushSTACK(STACK_(0+1)); pushSTACK(value1);
            funcall(seq_access_set(STACK_(5+3)),3); # (SEQ-ACCESS-SET sequence3 pointer3 ...)
            # pointer2 := (SEQ-UPD sequence2 pointer2) :
            pointer_update(STACK_1,STACK_8,STACK_7);
            # count2 := (1- count2) :
            decrement(STACK_3);
            # pointer3 := (SEQ-UPD sequence3 pointer3) :
            pointer_update(STACK_0,STACK_6,STACK_5);
          }
        return;
      seq2_end:
        # sequence2 zu Ende, sequence1 nicht. Rest aus sequence1 nehmen:
        do { pushSTACK(STACK_(10)); pushSTACK(STACK_(2+1));
             funcall(seq_access(STACK_(9+2)),2); # (SEQ-ACCESS sequence1 pointer1)
             pushSTACK(STACK_6); pushSTACK(STACK_(0+1)); pushSTACK(value1);
             funcall(seq_access_set(STACK_(5+3)),3); # (SEQ-ACCESS-SET sequence3 pointer3 ...)
             # pointer1 := (SEQ-UPD sequence1 pointer1) :
             pointer_update(STACK_2,STACK_10,STACK_9);
             # count1 := (1- count1) :
             decrement(STACK_4);
             # pointer3 := (SEQ-UPD sequence3 pointer3) :
             pointer_update(STACK_0,STACK_6,STACK_5);
           }
           until (eq(STACK_4,Fixnum_0)); # count1 = 0 ?
        return;
    }

# UP: Sortiert in sequence ab pointer_left genau k Elemente (k >= 1)
# und liefert einen Pointer nach diesen k Elementen.
# sort_part(pointer_left,k,stackptr)
# pointer_left wird destruktiv verändert.
# > pointer_left
# > k
# > stackptr: Pointer in den Stack:
#       sequence, predicate [stackptr], key, start, end, typdescr, seq2
# < ergebnis: Pointer nach den k Elementen
# kann GC auslösen
  local object sort_part (object pointer_left, object k, object* stackptr);
  local object sort_part(pointer_left,k,stackptr)
    var reg4 object pointer_left;
    var reg3 object k;
    var reg1 object* stackptr;
    { if (eq(k,Fixnum_1))
        { # k=1. Fast nichts zu tun
          pushSTACK(*(stackptr STACKop 1)); pushSTACK(pointer_left);
          funcall(seq_upd(*(stackptr STACKop -4)),2); # (SEQ-UPD sequence pointer_left)
          return value1; # als Ergebnis
        }
        else
        { # k>1.
          pushSTACK(pointer_left);
          pushSTACK(k);
          pushSTACK(I_I_ash_I(k,Fixnum_minus1)); # (ASH k -1) = (FLOOR k 2) =: kl
          STACK_1 = I_I_minus_I(STACK_1,STACK_0); # (- k (FLOOR k 2)) = (CEILING k 2) =: kr
          # Stackaufbau: pointer_left, kr, kl.
          # mit kl = (floor k 2) und kr = (ceiling k 2), also k = (+ kl kr).
          # rekursiv die linke Hälfte sortieren:
          { pushSTACK(STACK_2); # pointer_left
            funcall(seq_copy(*(stackptr STACKop -4)),1); # (SEQ-COPY pointer_left)
           {var reg2 object pointer_mid = sort_part(value1,STACK_0,stackptr);
            pushSTACK(pointer_mid);
          }}
          # Stackaufbau: pointer_left, kr, kl, pointer_mid.
          # rekursiv die rechte Hälfte sortieren:
          { pushSTACK(STACK_0); # pointer_mid
            funcall(seq_copy(*(stackptr STACKop -4)),1); # (SEQ-COPY pointer_mid)
           {var reg2 object pointer_right = sort_part(value1,STACK_2,stackptr);
            pushSTACK(pointer_right);
          }}
          # Stackaufbau: pointer_left, kr, kl, pointer_mid, pointer_right.
          # Linke Hälfte (sortiert) nach seq2 kopieren:
          { var reg2 object typdescr = *(stackptr STACKop -4);
            pushSTACK(*(stackptr STACKop 1)); # sequence
            pushSTACK(typdescr); # typdescr
            pushSTACK(*(stackptr STACKop -5)); # seq2
            pushSTACK(typdescr); # typdescr
            pushSTACK(STACK_(2+4)); # kl
            { pushSTACK(STACK_(4+5)); # pointer_left
              funcall(seq_copy(typdescr),1); # (SEQ-COPY pointer_left)
              pushSTACK(value1); # =: pointer1
            }
            typdescr = STACK_2;
            { pushSTACK(STACK_3); # seq2
              funcall(seq_init(typdescr),1); # (SEQ-INIT seq2)
              pushSTACK(value1); # =: pointer2
            }
            # Stackaufbau: pointer_left, kr, kl, pointer_mid, pointer_right,
            #              sequence, typdescr, seq2, typdescr, kl, pointer1, pointer2.
            copy_seqpart_into(); # kopieren
            skipSTACK(3);
          }
          # Stackaufbau: pointer_left, kr, kl, pointer_mid, pointer_right,
          #              sequence, typdescr, seq2, typdescr.
          { pushSTACK(STACK_3); # sequence
            pushSTACK(STACK_(2+1)); # typdescr
            pushSTACK(STACK_(3+2)); # sequence
            pushSTACK(STACK_(2+3)); # typdescr
            pushSTACK(STACK_(2+4+4)); # kl
            pushSTACK(STACK_(3+4+5)); # kr
            { pushSTACK(STACK_(1+6)); # seq2
              funcall(seq_init(STACK_(0+6+1)),1); # (SEQ-INIT seq2)
              pushSTACK(value1); # als Source-Pointer in seq2
            }
            pushSTACK(STACK_(1+4+7)); # pointer_mid als Source in sequence
            pushSTACK(STACK_(4+4+8)); # pointer_left als Destination in sequence
            merge(stackptr); # von seq2 nach sequence hineinmergen
            { var reg2 object pointer_right = STACK_(0+4+9); # pointer_right
              skipSTACK(5+4+9);
              return pointer_right; # als Ergebnis
        } } }
    }

# UP für SORT und STABLE-SORT: Sortiert einen Teil einer Sequence.
# stable_sort();
# > Stackaufbau: sequence, predicate, key, start, end
# < mv_space/mv_count: Werte
# kann GC auslösen
  local Values stable_sort (void);
  local Values stable_sort()
    { # Stackaufbau: sequence, predicate, key, start, end.
      # sequence überprüfen:
      pushSTACK(get_valid_seq_type(STACK_4)); # typdescr
      # Stackaufbau: sequence, predicate, key, start, end, typdescr.
      # Defaultwert für start ist 0 :
      start_default_0(STACK_2);
      # Defaultwert für end:
      end_default_len(STACK_1,STACK_5,STACK_0);
      # Argumente start und end überprüfen:
      test_start_end(&O(kwpair_start),&STACK_1);
      # key überprüfen:
      test_key_arg(&STACK_7);
      # l := (- end start), ein Integer >=0
     {var reg2 object l = I_I_minus_I(STACK_1,STACK_2);
      pushSTACK(l);
      # Stackaufbau: sequence, predicate, key, start, end, typdescr, l.
      if (!(eq(l,Fixnum_0))) # Bei l=0 ist nichts zu tun
        { # Hilfssequence der Länge (floor l 2) erzeugen:
          { pushSTACK(I_I_ash_I(l,Fixnum_minus1)); # (ASH l -1) = (FLOOR l 2)
            funcall(seq_make(STACK_(1+1)),1); # (SEQ-MAKE (FLOOR l 2))
            pushSTACK(value1); # =: seq2
          }
          # Stackaufbau: sequence, predicate, key, start, end, typdescr, l,
          #              seq2.
          pushSTACK(STACK_(6+1)); pushSTACK(STACK_(3+1+1));
          funcall(seq_init_start(STACK_(1+1+2)),2); # (SEQ-INIT-START sequence start)
          l = STACK_(0+1); STACK_(0+1) = STACK_0; skipSTACK(1); # seq2 ersetzt l im Stack
          sort_part(value1,l,&STACK_5); # Stück der Länge l ab start sortieren
        }
      skipSTACK(6); value1 = popSTACK(); mv_count=1; # sortierte sequence als Wert
    }}

LISPFUN(sort,2,0,norest,key,3, (kw(key),kw(start),kw(end)) )
# (SORT sequence predicate [:key] [:start] [:end]), CLTL S. 258
  { return_Values stable_sort(); }

LISPFUN(stable_sort,2,0,norest,key,3, (kw(key),kw(start),kw(end)) )
# (STABLE-SORT sequence predicate [:key] [:start] [:end]), CLTL S. 258
  { return_Values stable_sort(); }

LISPFUN(merge,4,0,norest,key,1, (kw(key)) )
# (MERGE result-type sequence1 sequence2 predicate [:key]), CLTL S. 260
  { # Stackaufbau: result-type, sequence1, sequence2, predicate, key.
    # key-Argument überprüfen:
    test_key_arg(&STACK_4);
    # sequence1 überprüfen:
    {var reg1 object seq1 = STACK_3;
     pushSTACK(seq1);
     pushSTACK(get_valid_seq_type(seq1));
    }
    # sequence2 überprüfen:
    {var reg1 object seq2 = STACK_(2+2);
     pushSTACK(seq2);
     pushSTACK(get_valid_seq_type(seq2));
    }
    # result-type überprüfen:
    {var reg1 object typdescr3 = valid_type(STACK_(4+4));
     pushSTACK(NIL); # Dummy
     pushSTACK(typdescr3);
    }
    # Stackaufbau: result-type, sequence1, sequence2, predicate, key,
    #              sequence1, typdescr1, sequence2, typdescr2, dummy, typdescr3.
    # Längen von sequence1 und sequence2 bestimmen:
    { pushSTACK(STACK_5); funcall(seq_length(STACK_(4+1)),1); # (SEQ-LENGTH sequence1)
      pushSTACK(value1); # =: len1
    }
    { pushSTACK(STACK_(3+1)); funcall(seq_length(STACK_(2+1+1)),1); # (SEQ-LENGTH sequence2)
      pushSTACK(value1); # =: len2
    }
    # beide Längen addieren und neue Sequence der Gesamtlänge bilden:
    { pushSTACK(I_I_plus_I(STACK_1,STACK_0)); # (+ len1 len2)
      funcall(seq_make(STACK_(0+2+1)),1); # (SEQ-MAKE (+ len1 len2))
      STACK_(1+2) = value1; # ersetzt Dummy im Stack
    }
    # Stackaufbau: result-type, sequence1, sequence2, predicate, key,
    #              sequence1, typdescr1, sequence2, typdescr2, sequence3, typdescr3,
    #              len1, len2.
    # Pointer an den Anfang der Sequences bestimmen:
    { pushSTACK(STACK_(5+2)); funcall(seq_init(STACK_(4+2+1)),1); # (SEQ-INIT sequence1)
      pushSTACK(value1); # =: pointer1
    }
    { pushSTACK(STACK_(3+2+1)); funcall(seq_init(STACK_(2+2+1+1)),1); # (SEQ-INIT sequence2)
      pushSTACK(value1); # =: pointer2
    }
    { pushSTACK(STACK_(1+2+2)); funcall(seq_init(STACK_(0+2+2+1)),1); # (SEQ-INIT sequence3)
      pushSTACK(value1); # =: pointer3
    }
    # Stackaufbau: result-type, sequence1, sequence2, predicate, key,
    #              sequence1, typdescr1, sequence2, typdescr2, sequence3, typdescr3,
    #              len1, len2, pointer1, pointer2, pointer3.
    # Merge-Operation durchführen:
    merge(&STACK_(1+6+5));
    value1 = STACK_(1+5); mv_count=1; # sequence3 als Wert
    skipSTACK(5+6+5);
  }

