# Arrayfunktionen von CLISP
# Bruno Haible 28.1.1993

#include "lispbibl.c"
#include "arilev0.c" # für bit_op, definiert auch mulu24 und mulu32_unchecked

# UP: Kopiert einen Simple-Vector
# copy_svector(vector)
# > vector : Simple-Vector
# < ergebnis : neuer Simple-Vector desselben Inhalts
# kann GC auslösen
  global object copy_svector (object vector);
  global object copy_svector(vector)
    var reg4 object vector;
    { var reg3 uintL length = TheSvector(vector)->length;
      pushSTACK(vector);
     {var reg5 object newvector = allocate_vector(length); # gleichlanger Vektor
      vector = popSTACK();
      # Inhalt von vector in newvector kopieren:
      if (!(length==0))
        { var reg1 object* ptr1 = &TheSvector(vector)->data[0];
          var reg2 object* ptr2 = &TheSvector(newvector)->data[0];
          dotimespL(length,length, { *ptr2++ = *ptr1++; } );
        }
      return newvector;
    }}

# UP: Bestimmt die aktive Länge eines Vektors (wie in LENGTH)
# vector_length(vector)
# > vector: ein Vektor
# < ergebnis: seine Länge als uintL
  global uintL vector_length (object vector);
  global uintL vector_length(vector)
    var reg1 object vector;
    { if (array_simplep(vector))
        return TheSarray(vector)->length;
      # Nicht-simpler Array
      { var reg2 Array addr = TheArray(vector);
        var reg3 uintL offset = offsetof(array_,dims);
        if (addr->flags & bit(arrayflags_dispoffset_bit))
          offset += sizeof(uintL);
        # Bei addr+offset fangen die Dimensionen an.
        if (addr->flags & bit(arrayflags_fillp_bit)) # evtl. Fillpointer
          offset += sizeof(uintL);
        return *(uintL*)pointerplus(addr,offset);
    } }

# Wandelt element-type in einen der Standard-Typen um
# und liefert seinen Elementtyp-Code.
# eltype_code(element_type)
# > element_type: Type-Specifier
# < ergebnis: Elementtyp-Code Atype_xxx
# Standard-Typen sind die möglichen Ergebnisse von ARRAY-ELEMENT-TYPE
# (Symbole T, BIT, STRING-CHAR und Listen (UNSIGNED-BYTE n)).
# Das Ergebnis ist ein Obertyp von element-type.
# kann GC auslösen
  global uintB eltype_code (object element_type);
  global uintB eltype_code(obj)
    var reg1 object obj;
    # Bei jeder Modifikation auch upgraded-array-element-type in type.lsp anpassen!
    {
      #if 0
      # Vorläufige Methode:
      # obj mit den Symbolen BIT, STRING-CHAR vergleichen.
      # Default ist T (garantiert ein Obertyp von allem).
      # Besser wäre:
      # (subtypep obj 'BIT) und (subtypep obj 'STRING-CHAR) abfragen.
      if (eq(obj,S(bit))) { return Atype_Bit; } # Symbol BIT ?
      elif (eq(obj,S(string_char))) { return Atype_String_Char; } # Symbol STRING-CHAR ?
      else # alles andere wird als T interpretiert
        { STACK_5 = S(t); return Atype_T; }
      #else
      # (cond ((eq obj 'BIT) Atype_Bit)
      #       ((eq obj 'STRING-CHAR) Atype_String_Char)
      #       ((eq obj 'T) Atype_T)
      #       (t (multiple-value-bind (low high) (sys::subtype-integer obj))
      #            ; Es gilt (or (null low) (subtypep obj `(INTEGER ,low ,high)))
      #            (if (and (integerp low) (not (minusp low)) (integerp high))
      #              (let ((l (integer-length high)))
      #                ; Es gilt (subtypep obj `(UNSIGNED-BYTE ,l))
      #                (cond ((<= l 1) Atype_Bit)
      #                      ((<= l 2) Atype_2Bit)
      #                      ((<= l 4) Atype_4Bit)
      #                      ((<= l 8) Atype_8Bit)
      #                      ((<= l 16) Atype_16Bit)
      #                      ((<= l 32) Atype_32Bit)
      #                      (t Atype_T)
      #              ) )
      #              Atype_T
      # )     )  ) )
      if (eq(obj,S(bit))) { return Atype_Bit; } # Symbol BIT ?
      elif (eq(obj,S(string_char))) { return Atype_String_Char; } # Symbol STRING-CHAR ?
      elif (eq(obj,S(t))) { return Atype_T; } # Symbol T ?
      pushSTACK(subr_self); # subr_self retten
      pushSTACK(obj); funcall(S(subtype_integer),1); # (SYS::SUBTYPE-INTEGER obj)
      subr_self = popSTACK(); # subr_self zurück
      if ((mv_count>1) && integerp(value1) && positivep(value1) && mintegerp(value2))
        { var reg2 uintL l = I_integer_length(value2); # (INTEGER-LENGTH high)
          if (l<=1) return Atype_Bit;
          if (l<=2) return Atype_2Bit;
          if (l<=4) return Atype_4Bit;
          if (l<=8) return Atype_8Bit;
          if (l<=16) return Atype_16Bit;
          if (l<=32) return Atype_32Bit;
        }
      return Atype_T;
      #endif
    }

# UP: erzeugt einen Bytevektor
# allocate_byte_vector(atype,len)
# > uintB atype: Atype_nBit
# > uintL len: Länge (in n-Bit-Blöcken)
# < ergebnis: neuer Semi-Simple-Bytevektor dieser Länge
# kann GC auslösen
  local object allocate_byte_vector (uintB atype, uintL len);
  local object allocate_byte_vector(atype,len)
    var reg2 uintB atype;
    var reg3 uintL len;
    { {var reg1 object new_sbvector = allocate_bit_vector(len<<atype);
       # neuer Simple-Bit-Vektor passender Länge
       pushSTACK(new_sbvector); # retten
      }
      {var reg1 object new_array = allocate_array(atype,1,bvector_type);
                                   # Flags: keine, Elementtyp Atype_nBit, Rang=1
       TheArray(new_array)->totalsize =
         TheArray(new_array)->dims[0] = len; # Länge und Total-Size eintragen
       TheArray(new_array)->data = popSTACK(); # Datenvektor eintragen
       return new_array;
    } }

LISPFUN(vector,0,0,rest,nokey,0,NIL) # (VECTOR {object}), CLTL S. 290
  { var reg4 object new_vector = allocate_vector((uintL)argcount);
    var reg1 uintC count;
    var reg3 object* argptr = rest_args_pointer;
    var reg2 object* ptr = &TheSvector(new_vector)->data[0];
    dotimesC(count,argcount, { *ptr++ = NEXT(argptr); } );
    value1 = new_vector; mv_count=1;
    set_args_end_pointer(rest_args_pointer);
  }

# Vom Standpunkt der Speicherstruktur her ist "der Datenvektor" eines
# nicht-simplen Arrays  TheArray(array)->data.
# Vom Standpunkt der Arrayfunktionen her bekommt man "den Datenvektor" eines
# Arrays, indem man so lange  TheArray(array)->data  nimmt, bis
# (bei Elementtypen T, BIT, STRING-CHAR) array ein simpler Vektor oder
# (bei Byte-Arrays) array ein nicht-simpler Vektor ohne arrayflags_..._bits,
# aber TheArray(array)->data ein Simple-Bit-Vektor ist.

# UP: verfolgt Kette von displaced-Arrays und addiert displaced-Offsets
#     für Zugriff auf ein einzelnes Array-Element
# notsimple_displace(array,&index);
# > array: Nicht-simpler Array
# > index: Row-major-index
# < ergebnis: Datenvektor
# < index: absoluter Index in den Datenvektor
# Es wird überprüft, ob das adressierte Array-Element in jedem der Arrays liegt.
# Es wird nicht überprüft, ob die Kette in einen Zyklus läuft.
  local object notsimple_displace (object array, uintL* index);
  local object notsimple_displace(array,index)
    var reg1 object array;
    var reg2 uintL* index;
    { loop
        { if (*index >= TheArray(array)->totalsize) goto fehler_bad_index;
          if (!(TheArray(array)->flags & bit(arrayflags_displaced_bit)))
            goto notdisplaced;
          # Array ist displaced
          *index += TheArray(array)->dims[0]; # displaced-Offset addieren
          array = TheArray(array)->data; # nächster Array
          if (array_simplep(array)) goto simple; # nächster Array simple?
        }
      notdisplaced:
        # Array ist nicht displaced, aber auch nicht simple
        if (TheArray(array)->flags & bit(arrayflags_notbytep_bit))
          { array = TheArray(array)->data; # Datenvektor ist garantiert simple
            simple:
            # Array ist simple
            if (*index >= TheSarray(array)->length) goto fehler_bad_index;
            return array;
          }
          else
          # Byte-Array
          { if (!m_simple_bit_vector_p(TheArray(array)->data))
              array = TheArray(array)->data;
            # letzter Datenvektor erreicht
            if (*index >= TheArray(array)->totalsize) goto fehler_bad_index;
            return array;
          }
      fehler_bad_index:
        fehler( # ausführlicher??
               DEUTSCH ? "Index in Array zu groß." :
               ENGLISH ? "index too large" :
               FRANCAIS ? "Index dans matrice trop grand." :
               ""
              );
    }

# Fehler, wenn ein displaced Array nicht mehr in seinen Ziel-Array paßt
  local nonreturning void fehler_displaced_inconsistent (void);
  local nonreturning void fehler_displaced_inconsistent()
    { fehler(
             DEUTSCH ? "Der Ziel-Array eines Displaced-Array wurde durch Adjustieren verkleinert." :
             ENGLISH ? "An array has been shortened by adjusting it while another array was displaced to it." :
             FRANCAIS ? "La matrice cible d'un «displaced-array» a été rapetissée par ajustement." :
             ""
            );
    }

# UP: Liefert zu einem Array gegebener Größe den Datenvektor und den Offset.
# Überprüft auch, ob alle Elemente des Arrays physikalisch vorhanden sind.
# array1_displace_check(array,size,&index)
# > object array: (echter) Array
# > uintL size: Größe
# < ergebnis: Datenvektor
# < index: wird um den Offset in den Datenvektor erhöht.
  global object array1_displace_check (object array, uintL size, uintL* index);
  global object array1_displace_check(array,size,index)
    var reg1 object array;
    var reg2 uintL size;
    var reg2 uintL* index;
    { loop
        { if (*index+size > TheArray(array)->totalsize) goto fehler_bad_index;
          if (!(TheArray(array)->flags & bit(arrayflags_displaced_bit)))
            goto notdisplaced;
          # Array ist displaced
          *index += TheArray(array)->dims[0]; # displaced-Offset addieren
          array = TheArray(array)->data; # nächster Array
          if (array_simplep(array)) goto simple; # nächster Array simple?
        }
      notdisplaced:
        # Array ist nicht displaced, aber auch nicht simple
        if (TheArray(array)->flags & bit(arrayflags_notbytep_bit))
          { array = TheArray(array)->data; # Datenvektor ist garantiert simple
            simple:
            # Array ist simple
            if (*index+size > TheSarray(array)->length) goto fehler_bad_index;
            return array;
          }
          else
          # Byte-Array
          { if (!m_simple_bit_vector_p(TheArray(array)->data))
              array = TheArray(array)->data;
            # letzter Datenvektor erreicht
            if (*index+size > TheArray(array)->totalsize) goto fehler_bad_index;
            return array;
          }
      fehler_bad_index:
        fehler_displaced_inconsistent();
    }

# UP: Liefert zu einem Array gegebener Größe den Datenvektor und den Offset.
# Überprüft auch, ob alle Elemente des Arrays physikalisch vorhanden sind.
# array_displace_check(array,size,&index)
# > object array: Array
# > uintL size: Größe
# < ergebnis: Datenvektor
# < index: wird um den Offset in den Datenvektor erhöht.
  global object array_displace_check (object array, uintL size, uintL* index);
  global object array_displace_check(array,size,index)
    var reg1 object array;
    var reg2 uintL size;
    var reg2 uintL* index;
    { if (array_simplep(array)) goto simple; # Array simple?
      loop
        { if (*index+size > TheArray(array)->totalsize) goto fehler_bad_index;
          if (!(TheArray(array)->flags & bit(arrayflags_displaced_bit)))
            goto notdisplaced;
          # Array ist displaced
          *index += TheArray(array)->dims[0]; # displaced-Offset addieren
          array = TheArray(array)->data; # nächster Array
          if (array_simplep(array)) goto simple; # nächster Array simple?
        }
      notdisplaced:
        # Array ist nicht displaced, aber auch nicht simple
        if (TheArray(array)->flags & bit(arrayflags_notbytep_bit))
          { array = TheArray(array)->data; # Datenvektor ist garantiert simple
            simple:
            # Array ist simple
            if (*index+size > TheSarray(array)->length) goto fehler_bad_index;
            return array;
          }
          else
          # Byte-Array
          { if (!m_simple_bit_vector_p(TheArray(array)->data))
              array = TheArray(array)->data;
            # letzter Datenvektor erreicht
            if (*index+size > TheArray(array)->totalsize) goto fehler_bad_index;
            return array;
          }
      fehler_bad_index:
        fehler_displaced_inconsistent();
    }

# Fehlermeldung
# > obj: Nicht-Array
# > subr_self: Aufrufer (ein SUBR)
  local nonreturning void fehler_array (object obj);
  local nonreturning void fehler_array(obj)
    var reg1 object obj;
    { pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(
             DEUTSCH ? "~: ~ ist kein Array." :
             ENGLISH ? "~: ~ is not an array" :
             FRANCAIS ? "~: n'est pas une matrice." :
             ""
            );
    }

# Überprüft Array-Argument.
# > object: Argument
# > subr_self: Aufrufer (ein SUBR)
# test_array(object)
  #define test_array(object_from_test_array)  \
    if_arrayp (object_from_test_array, ; , { fehler_array(object_from_test_array); } )

# Fehlermeldung
# > array : Array
# > argcount : (fehlerhafte) Anzahl Subscripts
# > subr_self: Aufrufer (ein SUBR)
  local nonreturning void fehler_subscript_anz (object array, uintC argcount);
  local nonreturning void fehler_subscript_anz(array,argcount)
    var reg1 object array;
    var reg2 uintC argcount;
    { pushSTACK(fixnum(TheArray(array)->rank));
      pushSTACK(array);
      pushSTACK(fixnum(argcount));
      pushSTACK(TheSubr(subr_self)->name);
      fehler(
             DEUTSCH ? "~: Es wurden ~ Subscripts angegeben, ~ hat aber den Rang ~." :
             ENGLISH ? "~: got ~ subscripts, but ~ has rank ~" :
             FRANCAIS ? "~: ~ indices donnés mais ~ est de rang ~." :
             ""
            );
    }

# Fehlermeldung
# > argcount : Anzahl der Subscripts, über ihnen der Array
# > subr_self: Aufrufer (ein SUBR)
  local nonreturning void fehler_subscript_type (uintC argcount);
  local nonreturning void fehler_subscript_type(argcount)
    var reg2 uintC argcount;
    { var reg1 object list = listof(argcount); # Subscript-Liste
      # Nun ist STACK_0 der Array.
      pushSTACK(list);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(
             DEUTSCH ? "~: Subscripts ~ für ~ sind nicht vom Typ `(INTEGER 0 (,ARRAY-DIMENSION-LIMIT))." :
             ENGLISH ? "~: subscripts ~ for ~ are not of type `(INTEGER 0 (,ARRAY-DIMENSION-LIMIT))" :
             FRANCAIS ? "~: Les indices ~ pour ~ ne sont pas de type `(INTEGER 0 (,ARRAY-DIMENSION-LIMIT))." :
             ""
            );
    }

# Fehlermeldung
# > argcount : Anzahl der Subscripts, über ihnen der Array
# > subr_self: Aufrufer (ein SUBR)
  local nonreturning void fehler_subscript_range (uintC argcount);
  local nonreturning void fehler_subscript_range(argcount)
    var reg2 uintC argcount;
    { var reg1 object list = listof(argcount); # Subscript-Liste
      # Nun ist STACK_0 der Array.
      pushSTACK(list);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(
             DEUTSCH ? "~: Subscripts ~ für ~ liegen nicht im erlaubten Bereich." :
             ENGLISH ? "~: subscripts ~ for ~ are out of range" :
             FRANCAIS ? "~: Les indices ~ pour ~ ne sont pas dans l'intervalle permis." :
             ""
            );
    }

# Überprüft Subscripts für einen AREF/STORE-Zugriff, entfernt sie vom STACK
# und liefert den Row-Major-Index (>=0, <arraysize_limit).
# test_subscripts(array,argptr,argcount)
# > array : nicht-simpler Array
# > argptr : Pointer über die Subscripts
# > argcount : Anzahl der Subscripts
# < ergebnis : row-major-index
  local uintL test_subscripts (object array, object* argptr, uintC argcount);
  local uintL test_subscripts(array,argptr,argcount)
    var reg1 object array;
    var reg4 object* argptr;
    var reg6 uintC argcount;
    { var reg5 object* args_pointer = argptr; # argptr retten für später
      # Anzahl der Subscripts überprüfen:
      if (!(argcount == TheArray(array)->rank)) # sollte = Rang sein
        fehler_subscript_anz(array,argcount);
      # Subscripts selbst überprüfen:
     {var reg5 uintL row_major_index = 0;
      var reg2 uintL* dimptr = &TheArray(array)->dims[0]; # Zeiger auf Dimensionen
      if (TheArray(array)->flags & bit(arrayflags_dispoffset_bit))
        dimptr++; # evtl. Displaced-Offset überspringen
      { var reg3 uintC count;
        dotimesC(count,argcount,
          { var reg1 object subscriptobj = NEXT(argptr); # Subscript als Objekt
            if (!(posfixnump(subscriptobj))) # Subscript muß Fixnum>=0 sein.
              fehler_subscript_type(argcount);
           {var reg1 uintL subscript = posfixnum_to_L(subscriptobj); # als uintL
            var reg2 uintL dim = *dimptr++; # entsprechende Dimension
            if (!(subscript<dim)) # Subscript muß kleiner als Dimension sein
              fehler_subscript_range(argcount);
            # Bilde row_major_index := row_major_index*dim+subscript:
            row_major_index =
              mulu32_unchecked(row_major_index,dim)+subscript;
            # Das gibt keinen Überlauf, weil dies
            # < Produkt der bisherigen Dimensionen
            # <= Produkt aller Dimensionen < arraysize_limit <= 2^32
            # ist. (Ausnahme: Falls eine spätere Dimension =0 ist.
            # Aber dann gibt's nachher sowieso eine Fehlermeldung.)
          }});
      }
      set_args_end_pointer(args_pointer);
      return row_major_index;
    }}

# Fehlermeldung
# > STACK_1: Vektor
# > STACK_0: (fehlerhafter) Index
# > subr_self: Aufrufer (ein SUBR)
  local nonreturning void fehler_index_type (void);
  local nonreturning void fehler_index_type()
    { pushSTACK(TheSubr(subr_self)->name);
      fehler(
             DEUTSCH ? "~: Index ~ für ~ ist nicht vom Typ `(INTEGER 0 (,ARRAY-DIMENSION-LIMIT))." :
             ENGLISH ? "~: index ~ for ~ is not of type `(INTEGER 0 (,ARRAY-DIMENSION-LIMIT))" :
             FRANCAIS ? "~: L'indice ~ pour ~ n'est pas de type `(INTEGER 0 (,ARRAY-DIMENSION-LIMIT))." :
             ""
            );
    }

# Fehlermeldung
# > STACK_1: Vektor
# > STACK_0: (fehlerhafter) Index
# > subr_self: Aufrufer (ein SUBR)
  local nonreturning void fehler_index_range (void);
  local nonreturning void fehler_index_range()
    { pushSTACK(TheSubr(subr_self)->name);
      fehler(
             DEUTSCH ? "~: Index ~ für ~ ist zu groß." :
             ENGLISH ? "~: index ~ for ~ is out of range" :
             FRANCAIS ? "~: L'index ~ pour ~ est trop grand." :
             ""
            );
    }

# Überprüft einen Index für einen AREF/STORE-Zugriff in einen simplen Vektor.
# test_index()
# > STACK_1: simpler Vektor
# > STACK_0: Index
# < ergebnis: Index als uintL
  local uintL test_index (void);
  local uintL test_index()
    { if (!mposfixnump(STACK_0)) # Index muß Fixnum>=0 sein.
        fehler_index_type();
     {var reg1 uintL index = posfixnum_to_L(STACK_0); # Index als uintL
      if (!(index < TheSarray(STACK_1)->length)) # Index muß kleiner als Länge sein
        fehler_index_range();
      return index;
    }}

# Überprüft Subscripts für einen AREF/STORE-Zugriff, entfernt sie vom STACK
# und liefert den Row-Major-Index (>=0, <arraysize_limit) und den Datenvektor.
# subscripts_to_index(array,argptr,argcount, &index)
# > array : nicht-simpler Array
# > argptr : Pointer über die Subscripts
# > argcount : Anzahl der Subscripts
# < index : Index in den Datenvektor
# < ergebnis : der Datenvektor
  local object subscripts_to_index (object array, object* argptr, uintC argcount, uintL* index_);
  local object subscripts_to_index(array,argptr,argcount,index_)
    var reg1 object array;
    var reg4 object* argptr;
    var reg2 uintC argcount;
    var uintL* index_;
    { test_array(array); # Array überprüfen
      if (array_simplep(array))
        # simpler Vektor, wird getrennt behandelt:
        { # Anzahl der Subscripts überprüfen:
          if (!(argcount == 1)) # sollte = 1 sein
            fehler_subscript_anz(array,argcount);
          # Subscript selbst überprüfen:
          *index_ = test_index(); # Index = Row-Major-Index = Subscript
          skipSTACK(1); return array;
        }
        else
        # nicht-simpler Array
        { # Subscripts überprüfen, Row-Major-Index errechnen, STACK aufräumen:
          *index_ = test_subscripts(array,argptr,argcount);
          # Datenvektor und absoluten Index holen:
          return notsimple_displace(array,&(*index_));
        }
    }

# Führt einen AREF-Zugriff aus.
# datenvektor_aref(datenvektor,index)
# > datenvektor : ein Datenvektor (simpler Vektor oder semi-simpler Byte-Vektor)
# > index : (geprüfter) Index in den Datenvektor
# < ergebnis : (AREF datenvektor index)
# kann GC auslösen (nur bei 32Bit-Byte-Vektoren)
  global object datenvektor_aref (object datenvektor, uintL index);
  global object datenvektor_aref(datenvektor,index)
    var reg3 object datenvektor;
    var reg1 uintL index;
    { switch (typecode(datenvektor))
        { case_svector: # Simple-Vector
            return TheSvector(datenvektor)->data[index];
          case_sbvector: # Simple-Bit-Vector
            return ( sbvector_btst(datenvektor,index) ? Fixnum_1 : Fixnum_0 );
          case_sstring: # Simple-String
            return code_char(TheSstring(datenvektor)->data[index]);
          case_obvector: # Byte-Vector
            { var reg2 uintB* ptr = &TheSbvector(TheArray(datenvektor)->data)->data[0];
              switch (TheArray(datenvektor)->flags /* & arrayflags_atype_mask */ )
                { case Atype_2Bit:
                    return fixnum((ptr[index/4]>>(2*((~index)%4)))&(bit(2)-1));
                  case Atype_4Bit:
                    return fixnum((ptr[index/2]>>(4*((~index)%2)))&(bit(4)-1));
                  case Atype_8Bit:
                    return fixnum(ptr[index]);
                  case Atype_16Bit:
                    return fixnum(((uint16*)ptr)[index]);
                  case Atype_32Bit:
                    return UL_to_I(((uint32*)ptr)[index]);
                  default: NOTREACHED
            }   }
          default: NOTREACHED
    }   }

# Führt einen STORE-Zugriff aus.
# datenvektor_store(datenvektor,index,element)
# > datenvektor : ein Datenvektor (simpler Vektor oder semi-simpler Byte-Vektor)
# > index : (geprüfter) Index in den Datenvektor
# > element : (ungeprüftes) einzutragendes Objekt
# > STACK_0 : array (für Fehlermeldung)
# > subr_self: Aufrufer (ein SUBR)
  local void datenvektor_store (object datenvektor, uintL index, object element);
  local void datenvektor_store(datenvektor,index,element)
    var reg4 object datenvektor;
    var reg1 uintL index;
    var reg3 object element;
    { switch (typecode(datenvektor))
        { case_svector: # Simple-Vector
            { TheSvector(datenvektor)->data[index] = element; return; }
          case_sbvector: # Simple-Bit-Vector
            { var reg2 uintB* addr = &TheSbvector(datenvektor)->data[index/8];
              var reg1 uintL bitnummer = (~index)%8; # 7 - (index mod 8)
              if (eq(element,Fixnum_0)) { *addr &= ~bit(bitnummer); return; }
              elif (eq(element,Fixnum_1)) { *addr |= bit(bitnummer); return; }
              else break;
            }
          case_sstring: # Simple-String
            if (string_char_p(element))
              { TheSstring(datenvektor)->data[index] = char_code(element);
                return;
              }
            else break;
          case_obvector: # Byte-Vector
            { var reg2 uintB* ptr = &TheSbvector(TheArray(datenvektor)->data)->data[0];
              var reg5 uintL wert;
              switch (TheArray(datenvektor)->flags /* & arrayflags_atype_mask */ )
                { case Atype_2Bit:
                    if (posfixnump(element) && ((wert = posfixnum_to_L(element)) < bit(2)))
                      { ptr[index/4] ^= (ptr[index/4] ^ (wert<<(2*((~index)%4)))) & ((bit(2)-1)<<(2*((~index)%4)));
                        return;
                      }
                      else break;
                  case Atype_4Bit:
                    if (posfixnump(element) && ((wert = posfixnum_to_L(element)) < bit(4)))
                      { ptr[index/2] ^= (ptr[index/2] ^ (wert<<(4*((~index)%2)))) & ((bit(4)-1)<<(4*((~index)%2)));
                        return;
                      }
                      else break;
                  case Atype_8Bit:
                    if (posfixnump(element) && ((wert = posfixnum_to_L(element)) < bit(8)))
                      { ptr[index] = wert; return; }
                      else break;
                  case Atype_16Bit:
                    if (posfixnump(element) && ((wert = posfixnum_to_L(element)) < bit(16)))
                      { ((uint16*)ptr)[index] = wert; return; }
                      else break;
                  case Atype_32Bit:
                    ((uint32*)ptr)[index] = I_to_UL(element); # evtl. Fehlermeldung macht I_to_UL
                    return;
                  default: NOTREACHED
                }
              break;
            }
          default: NOTREACHED
        }
      # Objekt war vom falschen Typ.
      { # array bereits in STACK_0
        pushSTACK(element);
        pushSTACK(TheSubr(subr_self)->name);
        fehler(
               DEUTSCH ? "~: ~ hat nicht den richtigen Typ für ~" :
               ENGLISH ? "~: ~ does not fit into ~, bad type" :
               FRANCAIS ? "~: ~ n'est pas de type correct pour ~." :
               ""
              );
    } }

LISPFUN(aref,1,0,rest,nokey,0,NIL) # (AREF array {subscript}), CLTL S. 290
  { var reg1 object array = Before(rest_args_pointer); # Array holen
    # Subscripts verarbeiten und Datenvektor und Index holen:
    var uintL index;
    var reg2 object datenvektor = subscripts_to_index(array,rest_args_pointer,argcount, &index);
    # Element des Datenvektors holen:
    value1 = datenvektor_aref(datenvektor,index); mv_count=1;
    skipSTACK(1);
  }

LISPFUN(store,2,0,rest,nokey,0,NIL) # (SYS::STORE array {subscript} object)
                     # = (SETF (AREF array {subscript}) object), CLTL S. 291
  { rest_args_pointer skipSTACKop 1; # Pointer über ersten Subscript
   {var reg1 object element = popSTACK();
    var reg2 object array = Before(rest_args_pointer); # Array holen
    # Subscripts verarbeiten und Datenvektor und Index holen:
    var uintL index;
    var reg3 object datenvektor = subscripts_to_index(array,rest_args_pointer,argcount, &index);
    # Element in den Datenvektor eintragen:
    datenvektor_store(datenvektor,index,element);
    value1 = element; mv_count=1;
    skipSTACK(1);
  }}

# Fehlermeldung
# > STACK_1: Nicht-Simple-Vector
# > subr_self: Aufrufer (ein SUBR)
  local nonreturning void fehler_svector (void);
  local nonreturning void fehler_svector()
    { STACK_0 = TheSubr(subr_self)->name;
      fehler(
             DEUTSCH ? "~: ~ ist kein Simple-Vector." :
             ENGLISH ? "~: ~ is not a simple-vector" :
             FRANCAIS ? "~: ~ n'est pas de type SIMPLE-VECTOR." :
             ""
            );
    }

LISPFUNN(svref,2) # (SVREF simple-vector index), CLTL S. 291
  { # simple-vector überprüfen:
    if (!m_simple_vector_p(STACK_1)) fehler_svector();
   {# index überprüfen:
    var reg1 uintL index = test_index();
    # Element holen:
    value1 = TheSvector(STACK_1)->data[index]; mv_count=1;
    skipSTACK(2);
  }}

LISPFUNN(svstore,3) # (SYS::SVSTORE simple-vector index element)
                    # = (SETF (SVREF simple-vector index) element), CLTL S. 291
  { var reg3 object element = popSTACK();
    # simple-vector überprüfen:
    if (!m_simple_vector_p(STACK_1)) fehler_svector();
   {# index überprüfen:
    var reg1 uintL index = test_index();
    # Element ablegen:
    TheSvector(STACK_1)->data[index] = element;
    value1 = element; mv_count=1;
    skipSTACK(2);
  }}

LISPFUNN(psvstore,3) # (SYS::%SVSTORE element simple-vector index)
                     # = (SETF (SVREF simple-vector index) element)
  { # simple-vector überprüfen:
    if (!m_simple_vector_p(STACK_1)) fehler_svector();
   {# index überprüfen:
    var reg1 uintL index = test_index();
    # Element ablegen:
    value1 = TheSvector(STACK_1)->data[index] = STACK_2; mv_count=1;
    skipSTACK(3);
  }}

# UP, liefert den Element-Typ eines Arrays
# array_element_type(array)
# > array : ein Array (simple oder nicht)
# < ergebnis : Element-Typ, eines der Symbole T, BIT, STRING-CHAR, oder eine Liste
# kann GC auslösen
  global object array_element_type (object array);
  global object array_element_type(array)
    var reg2 object array;
    { switch (typecode(array))
        { case_string: # String -> STRING-CHAR
            return S(string_char);
          case_sbvector: # Simple-Bit-Vector -> BIT
            return S(bit);
          case_vector: # allg. Vector -> T
            return S(t);
          case_obvector: # Byte-Vector
          case_array1: # allgemeiner Array
            { var reg1 uintBWL atype = TheArray(array)->flags & arrayflags_atype_mask;
              switch (atype)
                { case Atype_T:           return S(t);           # T
                  case Atype_Bit:         return S(bit);         # BIT
                  case Atype_String_Char: return S(string_char); # STRING-CHAR
                  case Atype_2Bit:        # (UNSIGNED-BYTE 2)
                  case Atype_4Bit:        # (UNSIGNED-BYTE 4)
                  case Atype_8Bit:        # (UNSIGNED-BYTE 8)
                  case Atype_16Bit:       # (UNSIGNED-BYTE 16)
                  case Atype_32Bit:       # (UNSIGNED-BYTE 32)
                    pushSTACK(S(unsigned_byte));
                    pushSTACK(fixnum(bit(atype)));
                    return listof(2);
                  default: NOTREACHED
            }   }
          default: NOTREACHED
    }   }

LISPFUNN(array_element_type,1) # (ARRAY-ELEMENT-TYPE array), CLTL S. 291
  { var reg1 object array = popSTACK();
    test_array(array);
    value1 = array_element_type(array); mv_count=1;
  }

LISPFUNN(array_rank,1) # (ARRAY-RANK array), CLTL S. 292
  { var reg1 object array = popSTACK();
    test_array(array);
    value1 = (array1p(array)
              ? fixnum((uintL)(TheArray(array)->rank)) # allgemeiner Array
              : Fixnum_1 # Vektor hat Rang 1
             );
    mv_count=1;
  }

LISPFUNN(array_dimension,2) # (ARRAY-DIMENSION array axis-number), CLTL S. 292
  { var reg1 object axis_number = popSTACK();
    var reg2 object array = popSTACK();
    test_array(array);
    if (array_simplep(array))
      # simpler Vektor: axis-number muß =0 sein, Wert ist dann die Länge.
      { if (eq(axis_number,Fixnum_0))
          { value1 = fixnum(TheSarray(array)->length);
            mv_count=1; return;
          }
          else goto fehler_axis;
      }
      else
      # nicht-simpler Array
      { if (posfixnump(axis_number)) # axis-number muß ein Fixnum >=0,
          { var reg1 uintL axis = posfixnum_to_L(axis_number);
            if (axis < (uintL)(TheArray(array)->rank)) # und <rank sein
              { var reg2 uintL* dimptr = &TheArray(array)->dims[0]; # Zeiger auf Dimensionen
                if (TheArray(array)->flags & bit(arrayflags_dispoffset_bit))
                  dimptr++; # evtl. Displaced-Offset überspringen
                value1 = fixnum(dimptr[axis]);
                mv_count=1; return;
              }
              else goto fehler_axis;
          }
          else goto fehler_axis;
      }
    fehler_axis:
      pushSTACK(array);
      pushSTACK(axis_number);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(
             DEUTSCH ? "~: ~ ist nicht >= 0 und < dem Rang von ~" :
             ENGLISH ? "~: ~ is not an nonnegative integer less than the rank of ~" :
             FRANCAIS ? "~: ~ n'est pas un entier >= 0 et strictement inférieur au rang de ~." :
             ""
            );
  }

# UP, bildet Liste der Dimensionen eines Arrays
# array_dimensions(array)
# > array: ein Array (simple oder nicht)
# < ergebnis: Liste seiner Dimensionen
# kann GC auslösen
  global object array_dimensions (object array);
  global object array_dimensions(array)
    var reg1 object array;
    { if (array_simplep(array))
        # simpler Vektor, bilde (LIST length)
        { var reg3 object len # Länge als Fixnum (nicht GC-gefährdet)
            = fixnum(TheSarray(array)->length);
          var reg2 object new_cons = allocate_cons();
          Car(new_cons) = len; Cdr(new_cons) = NIL;
          return new_cons;
        }
        else
        # nicht-simpler Array: alle Dimensionen als Fixnums auf den STACK,
        # dann eine Liste daraus machen.
        { var reg3 uintC rank = TheArray(array)->rank;
          var reg2 uintL* dimptr = &TheArray(array)->dims[0]; # Zeiger auf Dimensionen
          if (TheArray(array)->flags & bit(arrayflags_dispoffset_bit))
            dimptr++; # evtl. Displaced-Offset überspringen
          get_space_on_STACK(sizeof(object)*(uintL)rank); # STACK überprüfen
          { var reg1 uintC count;
            dotimesC(count,rank,
              { # nächste Dimension als Fixnum in den Stack:
                pushSTACK(fixnum(*dimptr++));
              });
          }
          return listof(rank); # Liste bilden
        }
    }

LISPFUNN(array_dimensions,1) # (ARRAY-DIMENSIONS array), CLTL S. 292
  { var reg1 object array = popSTACK();
    test_array(array);
    value1 = array_dimensions(array); mv_count=1;
  }

# UP, liefert Dimensionen eines Arrays und ihre Teilprodukte
# array_dims_sizes(array,&dims_sizes);
# > array: (echter) Array vom Rang r
# > struct { uintL dim; uintL dimprod; } dims_sizes[r]: Platz fürs Ergebnis
# < für i=1,...r:  dims_sizes[r-i] = { Dim_i, Dim_i * ... * Dim_r }
  global void array_dims_sizes (object array, array_dim_size* dims_sizes);
  global void array_dims_sizes(array,dims_sizes)
    var reg5 object array;
    var reg1 array_dim_size* dims_sizes;
    { var reg4 uintC r = TheArray(array)->rank; # Rang
      var reg2 uintL* dimptr = &TheArray(array)->dims[0]; # Zeiger auf Dimensionen
      if (TheArray(array)->flags & bit(arrayflags_dispoffset_bit))
        dimptr++; # evtl. Displaced-Offset überspringen
      dimptr = &dimptr[(uintL)r]; # Zeiger hinter die Dimensionen
     {var reg3 uintL produkt = 1;
      dotimesC(r,r, # Schleife über die r Dimensionen von hinten
        { var reg2 uintL dim = *--dimptr; # nächste Dimension
          produkt = mulu32_unchecked(produkt,dim); # aufs Produkt multiplizieren
          # Das gibt keinen Überlauf, weil dies
          # < Produkt der bisherigen Dimensionen
          # <= Produkt aller Dimensionen < arraysize_limit <= 2^32 ist.
          # (Ausnahme: Falls eine Dimension kleinerer Nummer =0 ist.
          # Aber dann ist das jetzige Produkt sowieso irrelevant, da
          # jede Schleife über diese Dimension eine Leerschleife ist.)
          dims_sizes->dim = dim; dims_sizes->dimprod = produkt;
          dims_sizes++;
        });
    }}

LISPFUNN(array_total_size,1) # (ARRAY-TOTAL-SIZE array), CLTL S. 292
  { var reg1 object array = popSTACK();
    test_array(array);
    value1 = fixnum(array_total_size(array));
    mv_count=1;
  }

LISPFUN(array_in_bounds_p,1,0,rest,nokey,0,NIL)
# (ARRAY-IN-BOUNDS-P array {subscript}), CLTL S. 292
  { var reg4 object* argptr = rest_args_pointer;
    var reg1 object array = BEFORE(rest_args_pointer); # Array holen
    test_array(array); # Array überprüfen
    if (array_simplep(array))
      # simpler Vektor, wird getrennt behandelt:
      { # Anzahl der Subscripts überprüfen:
        if (!(argcount == 1)) # sollte = 1 sein
          fehler_subscript_anz(array,argcount);
        # Subscript selbst überprüfen:
        { var reg2 object subscriptobj = STACK_0; # Subscript als Objekt
          if (!integerp(subscriptobj)) { fehler_index_type(); } # muß Integer sein
          # Subscript muß Fixnum>=0 sein,
          # Subscript als uintL muß kleiner als Länge sein:
          if (!( (posfixnump(subscriptobj))
                 && (posfixnum_to_L(subscriptobj) < TheSarray(array)->length) ))
            goto no;
          goto yes;
      } }
      else
      # nicht-simpler Array
      { # Anzahl der Subscripts überprüfen:
        if (!(argcount == TheArray(array)->rank)) # sollte = Rang sein
          fehler_subscript_anz(array,argcount);
        # Subscripts selbst überprüfen:
        {var reg2 uintL* dimptr = &TheArray(array)->dims[0]; # Zeiger auf Dimensionen
         if (TheArray(array)->flags & bit(arrayflags_dispoffset_bit))
           dimptr++; # evtl. Displaced-Offset überspringen
         { var reg3 uintC count;
           dotimesC(count,argcount,
             { var reg1 object subscriptobj = NEXT(argptr); # Subscript als Objekt
               if (!integerp(subscriptobj)) { fehler_subscript_type(argcount); } # muß Integer sein
               # Subscript muß Fixnum>=0 sein,
               # Subscript als uintL muß kleiner als die entsprechende Dimension sein:
               if (!( (posfixnump(subscriptobj))
                      && (posfixnum_to_L(subscriptobj) < *dimptr++) ))
                 goto no;
             });
        }}
        goto yes;
      }
    yes: value1 = T; mv_count=1; set_args_end_pointer(rest_args_pointer); return;
    no: value1 = NIL; mv_count=1; set_args_end_pointer(rest_args_pointer); return;
  }

LISPFUN(array_row_major_index,1,0,rest,nokey,0,NIL)
# (ARRAY-ROW-MAJOR-INDEX array {subscript}), CLTL S. 293
  { var reg1 object array = Before(rest_args_pointer); # Array holen
    var reg2 uintL index;
    test_array(array); # Array überprüfen
    if (array_simplep(array))
      # simpler Vektor, wird getrennt behandelt:
      { # Anzahl der Subscripts überprüfen:
        if (!(argcount == 1)) # sollte = 1 sein
          fehler_subscript_anz(array,argcount);
        # Subscript selbst überprüfen:
        test_index();
        value1 = popSTACK(); mv_count=1; # Index = Row-Major-Index = Subscript
        skipSTACK(1);
      }
      else
      # nicht-simpler Array
      { # Subscripts überprüfen, Row-Major-Index errechnen, STACK aufräumen:
        index = test_subscripts(array,rest_args_pointer,argcount);
        # Index als Fixnum zurück:
        value1 = fixnum(index); mv_count=1;
        skipSTACK(1);
      }
  }

LISPFUNN(adjustable_array_p,1) # (ADJUSTABLE-ARRAY-P array), CLTL S. 293
  { var reg1 object array = popSTACK(); # Argument holen
    test_array(array); # Array überprüfen
    if (array_simplep(array))
      goto no; # simpler Vektor, ist nicht adjustable
      else
      if (TheArray(array)->flags & bit(arrayflags_adjustable_bit))
        goto yes;
        else
        goto no;
    yes: value1 = T; mv_count=1; return;
    no:  value1 = NIL; mv_count=1; return;
  }

# Fehlermeldung
# fehler_bit_array()
# > STACK_0: Array, der kein Bit-Array ist
  local nonreturning void fehler_bit_array (void);
  local nonreturning void fehler_bit_array()
    { pushSTACK(TheSubr(subr_self)->name);
      fehler(
             DEUTSCH ? "~: ~ ist kein Bit-Array." :
             ENGLISH ? "~: ~ is not an array of bits" :
             FRANCAIS ? "~: ~ n'est pas une matrice de bits." :
             ""
            );
    }

LISPFUN(bit,1,0,rest,nokey,0,NIL) # (BIT bit-array {subscript}), CLTL S. 293
  { var reg1 object array = Before(rest_args_pointer); # Array holen
    # Subscripts verarbeiten und Datenvektor und Index holen:
    var uintL index;
    var reg2 object datenvektor = subscripts_to_index(array,rest_args_pointer,argcount, &index);
    if (!(simple_bit_vector_p(datenvektor)))
      fehler_bit_array();
    # Datenvektor ist ein Simple-Bit-Vector. Element des Datenvektors holen:
    value1 = ( sbvector_btst(datenvektor,index) ? Fixnum_1 : Fixnum_0 ); mv_count=1;
    skipSTACK(1);
  }

LISPFUN(sbit,1,0,rest,nokey,0,NIL) # (SBIT bit-array {subscript}), CLTL S. 293
  { var reg1 object array = Before(rest_args_pointer); # Array holen
    # Subscripts verarbeiten und Datenvektor und Index holen:
    var uintL index;
    var reg2 object datenvektor = subscripts_to_index(array,rest_args_pointer,argcount, &index);
    if (!(simple_bit_vector_p(datenvektor)))
      fehler_bit_array();
    # Datenvektor ist ein Simple-Bit-Vector. Element des Datenvektors holen:
    value1 = ( sbvector_btst(datenvektor,index) ? Fixnum_1 : Fixnum_0 ); mv_count=1;
    skipSTACK(1);
  }

# Für Unterprogramme für Bitvektoren:
  # Man arbeitet mit Bit-Blöcken à bitpack Bits.
  # uint_bitpack ist ein unsigned Integer mit bitpack Bits.
  # uint_2bitpack ist ein unsigned Integer mit 2*bitpack Bits.
  # R_bitpack(x) liefert die rechte (untere) Hälfte eines uint_2bitpack.
  # L_bitpack(x) liefert die linke (obere) Hälfte eines uint_2bitpack.
  # LR_2bitpack(x,y) liefert zu x,y das aus der linken Hälfte x und der
  #                  rechten Hälfte y zusammengesetzte uint_2bitpack.
  # Verwende LR_0_bitpack(y) falls x=0, LR_bitpack_0(x) falls y=0.
  #if BIG_ENDIAN_P && (Varobject_alignment%2 == 0)
    # Bei Big-Endian-Maschinen kann man gleich mit 16 Bit auf einmal arbeiten
    # (sofern Varobject_alignment durch 2 Byte teilbar ist):
    #define bitpack  16
    #define uint_bitpack  uint16
    #define uint_2bitpack  uint32
    #define R_bitpack(x)  low16(x)
    #define L_bitpack(x)  high16(x)
    #define LR_2bitpack(x,y)  highlow32(x,y)
    #define LR_0_bitpack(y)  ((uint32)(uint16)(y))
    #define LR_bitpack_0(x)  highlow32_0(x)
  #else
    # Sonst kann man nur 8 Bit auf einmal nehmen:
    #define bitpack  8
    #define uint_bitpack  uint8
    #define uint_2bitpack  uint16
    #define R_bitpack(x)  ((uint_bitpack)(uint_2bitpack)(x))
    #define L_bitpack(x)  ((uint_bitpack)((uint_2bitpack)(x) >> bitpack))
    #define LR_2bitpack(x,y)  \
      (((uint_2bitpack)(uint_bitpack)(x) << bitpack)  \
       | (uint_2bitpack)(uint_bitpack)(y)             \
      )
    #define LR_0_bitpack(y)  LR_2bitpack(0,y)
    #define LR_bitpack_0(x)  LR_2bitpack(x,0)
  #endif

# Unterprogramm für Bitvektor-Vergleich:
# bit_compare(array1,index1,array2,index2,count)
# > array1: erster Bit-Array,
# > index1: absoluter Index in array1
# > array2: zweiter Bit-Array,
# > index2: absoluter Index in array2
# > count: Anzahl der zu vergleichenden Bits
# < ergebnis: TRUE, wenn die Ausschnitte bitweise gleich sind, FALSE sonst.
  global boolean bit_compare (object array1, uintL index1,
                              object array2, uintL index2,
                              uintL bitcount);
  global boolean bit_compare(array1,index1,array2,index2,bitcount)
    var reg9 object array1;
    var reg5 uintL index1;
    var reg9 object array2;
    var reg6 uintL index2;
    var reg8 uintL bitcount;
    { var reg3 uint_bitpack* ptr1 = &((uint_bitpack*)(&TheSbvector(array1)->data[0]))[index1/bitpack];
      var reg4 uint_bitpack* ptr2 = &((uint_bitpack*)(&TheSbvector(array2)->data[0]))[index2/bitpack];
      # ptr1 zeigt auf das erste teilnehmende Word des 1. Bit-Arrays.
      # ptr2 zeigt auf das erste teilnehmende Word des 2. Bit-Arrays.
      var reg7 uintL bitpackcount = bitcount / bitpack;
      # bitpackcount = Anzahl der ganzen Words
      var uintL bitcount_rest = bitcount % bitpack;
      # bitcount_rest = Anzahl der übrigbleibenden Bits
      index1 = index1 % bitpack; # Bit-Offset im 1. Bit-Array
      index2 = index2 % bitpack; # Bit-Offset im 2. Bit-Array
      if ((index1==0) && (index2==0))
        # einfache Schleife, da alle Bit-Offsets im Word =0 sind:
        { dotimesL(bitpackcount,bitpackcount,
            { if (!(*ptr1++ == *ptr2++)) { return FALSE; } }
            );
          # bitcount_rest = Anzahl der noch vergleichenden Bits
          if (!(bitcount_rest==0))
            # letztes Word vergleichen:
            { if (!(( (*ptr1 ^ *ptr2)
                      & # Bitmaske mit Bits bitpack-1..bitpack-bitcount_rest gesetzt
                        ~( (uint_bitpack)(bitm(bitpack)-1) >> bitcount_rest)
                    ) ==0
                 ) )
                { return FALSE; }
            }
          return TRUE;
        }
        else
        # kompliziertere Schleife:
        { var reg1 uint_2bitpack carry1 = LR_bitpack_0((*ptr1++) << index1);
          # carry1 hat in seinen oberen bitpack-index1 Bits (Bits 2*bitpack-1..bitpack+index1)
          # die betroffenen Bits des 1. Words des 1. Arrays, sonst Nullen.
          var reg2 uint_2bitpack carry2 = LR_bitpack_0((*ptr2++) << index2);
          # carry2 hat in seinen oberen bitpack-index2 Bits (Bits 2*bitpack-1..bitpack+index2)
          # die betroffenen Bits des 1. Words des 2. Arrays, sonst Nullen.
          dotimesL(bitpackcount,bitpackcount,
            { # Vergleichsschleife (jeweils wortweise):
              # Nach n>=0 Schleifendurchläufen ist
              # ptr1 und ptr2 um n+1 Words weitergerückt, also Pointer aufs
              # nächste zu lesende Word des 1. bzw. 2. Arrays,
              # bitpackcount = Zahl zu verknüpfender ganzer Words - n,
              # carry1 = Übertrag vom 1. Array
              #          (in den bitpack-index1 oberen Bits, sonst Null),
              # carry2 = Übertrag vom 2. Array
              #          (in den bitpack-index2 oberen Bits, sonst Null).
              if (!(
                    ( carry1 |=
                        LR_0_bitpack(*ptr1++) # nächstes Word des 1. Arrays lesen
                        << index1, # zum carry1 dazunehmen
                      L_bitpack(carry1) # und davon das linke Word verwenden
                    )
                    ==
                    ( carry2 |=
                        LR_0_bitpack(*ptr2++) # nächstes Word des 2. Arrays lesen
                        << index2, # zum carry2 dazunehmen
                      L_bitpack(carry2) # und davon das linke Word verwenden
                    )
                 ) )
                { return FALSE; }
              carry1 = LR_bitpack_0(R_bitpack(carry1)); # carry1 := rechtes Word von carry1
              carry2 = LR_bitpack_0(R_bitpack(carry2)); # carry2 := rechtes Word von carry2
            });
          # Noch bitcount_rest Bits zu vergleichen:
          if (!(bitcount_rest==0))
            # letztes Word vergleichen:
            { if (!(( (
                       ( carry1 |=
                           LR_0_bitpack(*ptr1++) # nächstes Word des 1. Arrays lesen
                           << index1, # zum carry1 dazunehmen
                         L_bitpack(carry1) # und davon das linke Word verwenden
                       )
                       ^
                       ( carry2 |=
                           LR_0_bitpack(*ptr2++) # nächstes Word des 2. Arrays lesen
                           << index2, # zum carry2 dazunehmen
                         L_bitpack(carry2) # und davon das linke Word verwenden
                       )
                      )
                      & # Bitmaske mit Bits bitpack-1..bitpack-bitcount_rest gesetzt
                        ~( (uint_bitpack)(bitm(bitpack)-1) >> bitcount_rest)
                    ) ==0
                 ) )
                { return FALSE; }
            }
          return TRUE;
        }
    }

# Unterprogramm für Bitvektor-Operationen:
# bit_op(array1,index1,array2,index2,array3,index3,op,count);
# > array1: erster Bit-Array,
# > index1: absoluter Index in array1
# > array2: zweiter Bit-Array,
# > index2: absoluter Index in array2
# > array3: dritter Bit-Array,
# > index3: absoluter Index in array3
# > op: Adresse der Operationsroutine
# > count: Anzahl der zu verknüpfenden Bits
  # bit_op_fun ist eine Funktion, die zwei bitpack-Bit-Wörter verknüpft:
  typedef uint_bitpack bit_op_fun (uint_bitpack x, uint_bitpack y);
  local void bit_op (object array1, uintL index1,
                     object array2, uintL index2,
                     object array3, uintL index3,
                     bit_op_fun* op, uintL bitcount);
  local void bit_op(array1,index1,array2,index2,array3,index3,op,bitcount)
    var object array1;
    var reg2 uintL index1;
    var object array2;
    var reg2 uintL index2;
    var object array3;
    var reg2 uintL index3;
    var reg3 bit_op_fun* op;
    var uintL bitcount;
    { var reg1 uint_bitpack* ptr1 = &((uint_bitpack*)(&TheSbvector(array1)->data[0]))[index1/bitpack];
      var reg1 uint_bitpack* ptr2 = &((uint_bitpack*)(&TheSbvector(array2)->data[0]))[index2/bitpack];
      var reg1 uint_bitpack* ptr3 = &((uint_bitpack*)(&TheSbvector(array3)->data[0]))[index3/bitpack];
      # ptr1 zeigt auf das erste teilnehmende Word des 1. Bit-Arrays.
      # ptr2 zeigt auf das erste teilnehmende Word des 2. Bit-Arrays.
      # ptr3 zeigt auf das erste teilnehmende Word des 3. Bit-Arrays.
      var reg3 uintL bitpackcount = bitcount / bitpack;
      # bitpackcount = Anzahl der ganzen Words
      var uintL bitcount_rest = bitcount % bitpack;
      # bitcount_rest = Anzahl der übrigbleibenden Bits
      index1 = index1 % bitpack; # Bit-Offset im 1. Bit-Array
      index2 = index2 % bitpack; # Bit-Offset im 2. Bit-Array
      index3 = index3 % bitpack; # Bit-Offset im 3. Bit-Array
      if ((index1==0) && (index2==0) && (index3==0))
        # einfache Schleife, da alle Bit-Offsets im Word =0 sind:
        { dotimesL(bitpackcount,bitpackcount,
            { *ptr3++ = (*op)(*ptr1++,*ptr2++); }
            );
          # bitcount_rest = Anzahl der noch abzulegenden Bits
          if (!(bitcount_rest==0))
            # letztes Word ablegen:
            { var reg1 uint_bitpack temp = (*op)(*ptr1,*ptr2);
              *ptr3 =
                ( ~
                    ( (uint_bitpack)(bitm(bitpack)-1) >> bitcount_rest)
                    # Bitmaske mit Bits bitpack-bitcount_rest-1..0 gesetzt
                  # Bitmaske mit Bits bitpack-1..bitpack-bitcount_rest gesetzt
                 &
                 (*ptr3 ^ temp)
                ) # zu ändernde Bits
                ^ *ptr3
                ;
        }   }
        else
        # kompliziertere Schleife:
        { var reg1 uint_2bitpack carry1 = LR_bitpack_0((*ptr1++) << index1);
          # carry1 hat in seinen oberen bitpack-index1 Bits (Bits 2*bitpack-1..bitpack+index1)
          # die betroffenen Bits des 1. Words des 1. Arrays, sonst Nullen.
          var reg1 uint_2bitpack carry2 = LR_bitpack_0((*ptr2++) << index2);
          # carry2 hat in seinen oberen bitpack-index2 Bits (Bits 2*bitpack-1..bitpack+index2)
          # die betroffenen Bits des 1. Words des 2. Arrays, sonst Nullen.
          var reg1 uint_2bitpack carry3 =
            LR_bitpack_0(
                         (~
                            ( (uint_bitpack)(bitm(bitpack)-1) >> index3)
                            # Bitmaske mit Bits bitpack-index3-1..0 gesetzt
                         ) # Bitmaske mit Bits bitpack-1..bitpack-index3 gesetzt
                         & (*ptr3)
                        );
          # carry3 hat in seinen obersten index3 Bits (Bits 2*bitpack-1..2*bitpack-index3)
          # genau die Bits von *ptr3, die nicht verändert werden dürfen.
          loop
            { # Verknüpfungsschleife (jeweils wortweise):
              # Nach n>=0 Schleifendurchläufen ist
              # ptr1 und ptr2 um n+1 Words weitergerückt, also Pointer aufs
              # nächste zu lesende Word des 1. bzw. 2. Arrays,
              # ptr3 um n Words weitergerückt, also Pointer aufs
              # nächste zu schreibende Word des 3. Arrays,
              # bitpackcount = Zahl zu verknüpfender ganzer Words - n,
              # carry1 = Übertrag vom 1. Array
              #          (in den bitpack-index1 oberen Bits, sonst Null),
              # carry2 = Übertrag vom 2. Array
              #          (in den bitpack-index2 oberen Bits, sonst Null),
              # carry3 = Übertrag noch abzuspeichernder Bits
              #          (in den index3 oberen Bits, sonst Null).
              var reg1 uint_bitpack temp =
                (*op)(
                      ( carry1 |=
                          LR_0_bitpack(*ptr1++) # nächstes Word des 1. Arrays lesen
                          << index1, # zum carry1 dazunehmen
                        L_bitpack(carry1) # und davon das linke Word verwenden
                      ),
                      ( carry2 |=
                          LR_0_bitpack(*ptr2++) # nächstes Word des 2. Arrays lesen
                          << index2, # zum carry2 dazunehmen
                        L_bitpack(carry2) # und davon das linke Word verwenden
                      )
                     ) ; # beide durch *op verknüpfen
              carry1 = LR_bitpack_0(R_bitpack(carry1)); # carry1 := rechtes Word von carry1
              carry2 = LR_bitpack_0(R_bitpack(carry2)); # carry2 := rechtes Word von carry2
              carry3 |= LR_bitpack_0(temp) >> index3;
              # Die oberen bitpack+index3 Bits von carry3 sind abzulegen.
              if (bitpackcount==0) break;
              *ptr3++ = L_bitpack(carry3); # bitpack Bits davon ablegen
              carry3 = LR_bitpack_0(R_bitpack(carry3)); # und index3 Bits für später behalten.
              bitpackcount--;
            }
          # letztes (halbes) Datenword speziell behandeln:
          # Vom letzten Word (nun in den Bits
          # 2*bitpack-index3-1..bitpack-index3 von carry3)
          # dürfen nur bitcount_rest Bits im 3. Array abgelegt werden.
          { var reg4 uint_bitpack last_carry;
            bitcount_rest = index3+bitcount_rest;
            # Die oberen bitcount_rest Bits ablegen:
            if (bitcount_rest>=bitpack)
              { *ptr3++ = L_bitpack(carry3);
                last_carry = R_bitpack(carry3);
                bitcount_rest -= bitpack;
              }
              else
              { last_carry = L_bitpack(carry3); }
            # Die noch übrigen bitcount_rest Bits von last_carry ablegen:
            if (!(bitcount_rest==0))
              *ptr3 ^=
                (*ptr3 ^ last_carry)
                & (~( (uint_bitpack)(bitm(bitpack)-1) >> bitcount_rest ));
                  # Bitmaske, in der die oberen bitcount_rest Bits gesetzt sind
        } }
    }

# Unterprogramm für Bit-Verknüpfung mit 2 Operanden
# bit_up(op)
# > STACK_2: bit-array1
# > STACK_1: bit-array2
# > STACK_0: result-bit-array oder #<UNBOUND>
# > op: Adresse der Verknüpfungsroutine
# < value1/mv_count: Funktionswert
# Testet Argumente, räumt STACK auf.
  local Values bit_up (bit_op_fun* op);
  local Values bit_up(op)
    var reg4 bit_op_fun* op;
    { # Hauptunterscheidung: Vektor / mehrdimensionaler Array
      var reg2 uintL len; # Länge (des 1. Arrays), falls Vektoren
      var reg2 uintC rank; # Rang und
      var reg2 uintL* dimptr; # Pointer auf Dimensionen, falls mehrdimensional
      # Typ von bit-array1 untersuchen und danach verzweigen:
      switch (mtypecode(STACK_2))
        { case_sbvector:
            len = TheSbvector(STACK_2)->length; goto vector;
          case_obvector:
            { var reg1 Array array1 = TheArray(STACK_2);
              # bit-array1 muß den Elementtyp BIT haben:
              if (!((array1->flags & arrayflags_atype_mask) == Atype_Bit))
                goto fehler2;
              len = array1->totalsize;
              goto vector;
            }
          case_array1:
            { var reg1 Array array1 = TheArray(STACK_2);
              # bit-array1 muß den Elementtyp BIT haben:
              if (!((array1->flags & arrayflags_atype_mask) == Atype_Bit))
                goto fehler2;
              # Rang merken:
              rank = array1->rank;
              # Dimensionen merken:
              dimptr = &array1->dims[0];
              if (array1->flags & bit(arrayflags_dispoffset_bit))
                dimptr++;
              # die Anzahl der zu verknüpfenden Bits ist die Totalsize:
              len = array1->totalsize;
              goto array;
            }
          default:
            goto fehler2;
        }
      vector: # Das erste Argument ist ein Bit-Vektor, mit Länge len.
        # Teste, ob dies auch auf den/die anderen zutrifft:
        # bit-array2 überprüfen:
        switch (mtypecode(STACK_1))
          { case_sbvector:
              if (!(len == TheSbvector(STACK_1)->length)) goto fehler2;
              break;
            case_obvector:
              { var reg1 Array array2 = TheArray(STACK_1);
                # bit-array2 muß den Elementtyp BIT haben:
                if (!((array2->flags & arrayflags_atype_mask) == Atype_Bit))
                  goto fehler2;
                if (!(len == array2->totalsize)) goto fehler2;
              }
              break;
            default:
              goto fehler2;
          }
        # bit-array3 überprüfen:
        {var reg1 object array3 = STACK_0;
         if (eq(array3,unbound) || eq(array3,NIL)) # nicht angegeben oder NIL?
           # ja -> neuen Vektor erzeugen:
           { STACK_0 = allocate_bit_vector(len); }
           else
           if (eq(array3,T))
             { STACK_0 = STACK_2; } # statt T verwende bit-array1
             else
             switch (mtypecode(STACK_0))
               { case_sbvector:
                   if (!(len == TheSbvector(array3)->length)) goto fehler3;
                   break;
                 case_obvector:
                   # bit-array3 muß den Elementtyp BIT haben:
                   if (!((TheArray(array3)->flags & arrayflags_atype_mask) == Atype_Bit))
                     goto fehler3;
                   if (!(len == TheArray(array3)->totalsize)) goto fehler3;
                   break;
                 default:
                   goto fehler3;
               }
        }
        goto weiter;
      array: # erstes Argument war ein mehrdimensionaler Bit-Array
        # mit Rang rank, Dimensionen ab dimptr und Totalsize len.
        # bit-array2 überprüfen:
        switch (mtypecode(STACK_1))
          { case_array1:
              { var reg1 Array array2 = TheArray(STACK_1);
                # bit-array2 muß den Elementtyp BIT haben:
                if (!((array2->flags & arrayflags_atype_mask) == Atype_Bit))
                  goto fehler2;
                # Rang vergleichen:
                if (!(rank == array2->rank)) goto fehler2;
                # Dimensionen vergleichen:
                { var reg3 uintC count;
                  var reg1 uintL* dimptr1 = dimptr;
                  var reg2 uintL* dimptr2;
                  dimptr2 = &array2->dims[0];
                  if (array2->flags & bit(arrayflags_dispoffset_bit))
                    dimptr2++;
                  dotimesC(count,rank, { if (!(*dimptr1++==*dimptr2++)) goto fehler2; });
                }
                break;
              }
            default:
              goto fehler2;
          }
        # bit-array3 überprüfen:
        {var reg1 object array3 = STACK_0;
         if (eq(array3,unbound) || eq(array3,NIL)) # nicht angegeben oder NIL?
           # ja -> neuen Array erzeugen:
           { STACK_0 = allocate_bit_vector(len); # Bitvektor erzeugen
             array3 = allocate_array(bit(arrayflags_notbytep_bit)|Atype_Bit,rank,array_type); # Array erzeugen
             TheArray(array3)->data = STACK_0; # Datenvektor eintragen
             # Dimensionen eintragen:
             { var reg3 uintC count;
               var reg1 uintL* dimptr1 = dimptr;
               var reg2 uintL* dimptr2 = &TheArray(array3)->dims[0];
               dotimesC(count,rank, { *dimptr1++ = *dimptr2++; });
             }
             STACK_0 = array3; # neuen Array ablegen
           }
           else
           if (eq(array3,T))
             { STACK_0 = STACK_2; } # statt T verwende bit-array1
             else
             switch (mtypecode(STACK_0))
               { case_array1:
                   { var reg1 Array array3 = TheArray(STACK_0);
                     # bit-array3 muß den Elementtyp BIT haben:
                     if (!((array3->flags & arrayflags_atype_mask) == Atype_Bit))
                       goto fehler3;
                     # Rang vergleichen:
                     if (!(rank == array3->rank)) goto fehler3;
                     # Dimensionen vergleichen:
                     { var reg3 uintC count;
                       var reg1 uintL* dimptr1 = dimptr;
                       var reg2 uintL* dimptr2;
                       dimptr2 = &array3->dims[0];
                       if (array3->flags & bit(arrayflags_dispoffset_bit))
                         dimptr2++;
                       dotimesC(count,rank, { if (!(*dimptr1++==*dimptr2++)) goto fehler3; });
                     }
                     break;
                   }
                 default:
                   goto fehler3;
               }
        }
      weiter: # Vorbereitungen sind abgeschlossen:
        # STACK_2 = bit-array1, STACK_1 = bit-array2, STACK_0 = bit-array3,
        # alle von denselben Dimensionen, mit je len Bits.
        { var uintL index1 = 0; # Index in Datenvektor von bit-array1
          var object array1 = # Datenvektor von bit-array1
                              (m_simple_bit_vector_p(STACK_2)
                                ? STACK_2
                                : array1_displace_check(STACK_2,len,&index1)
                              );
          var uintL index2 = 0; # Index in Datenvektor von bit-array2
          var object array2 = # Datenvektor von bit-array2
                              (m_simple_bit_vector_p(STACK_1)
                                ? STACK_1
                                : array1_displace_check(STACK_1,len,&index2)
                              );
          var uintL index3 = 0; # Index in Datenvektor von bit-array3
          var object array3 = # Datenvektor von bit-array3
                              (m_simple_bit_vector_p(STACK_0)
                                ? STACK_0
                                : array1_displace_check(STACK_0,len,&index3)
                              );
          # Los geht's:
          bit_op(array1,index1,array2,index2,array3,index3,op,len);
        }
        # Fertig:
        value1 = popSTACK(); mv_count=1; # bit-array3 ist das Ergebnis
        skipSTACK(2);
        return;
      fehler2: # Fehlermeldung bei (mindestens) 2 Argumenten
        { var reg1 object array1 = STACK_2;
          var reg2 object array2 = STACK_1;
          pushSTACK(array2); pushSTACK(array1);
          pushSTACK(TheSubr(subr_self)->name);
          fehler(
                 DEUTSCH ? "~: Die Argumente ~ und ~ müssen Bit-Arrays gleicher Dimensionierung sein." :
                 ENGLISH ? "~: The arguments ~ and ~ should be arrays of bits with the same dimensions" :
                 FRANCAIS ? "~: Les arguments ~ et ~ doivent être des matrices de mêmes dimensions." :
                 ""
                );
        }
      fehler3: # Fehlermeldung bei 3 Argumenten
        { var reg1 object array1 = STACK_2;
          var reg2 object array2 = STACK_1;
          # array3 bereits in STACK_0
          pushSTACK(array2); pushSTACK(array1);
          pushSTACK(TheSubr(subr_self)->name);
          fehler(
                 DEUTSCH ? "~: Die Argumente ~, ~ und ~ müssen Bit-Arrays gleicher Dimensionierung sein." :
                 ENGLISH ? "~: The arguments ~, ~ and ~ should be arrays of bits with the same dimensions" :
                 FRANCAIS ? "~: Les arguments ~, ~ et ~ doivent être des matrices de mêmes dimensions." :
                 ""
                );
        }
    }

# Die einzelnen Operatoren für BIT-AND usw.:
  local uint_bitpack bitpack_and (uint_bitpack x, uint_bitpack y);
  local uint_bitpack bitpack_and(x,y) var reg1 uint_bitpack x; var reg2 uint_bitpack y;
    { return x&y; }
  local uint_bitpack bitpack_ior (uint_bitpack x, uint_bitpack y);
  local uint_bitpack bitpack_ior(x,y) var reg1 uint_bitpack x; var reg2 uint_bitpack y;
    { return x|y; }
  local uint_bitpack bitpack_xor (uint_bitpack x, uint_bitpack y);
  local uint_bitpack bitpack_xor(x,y) var reg1 uint_bitpack x; var reg2 uint_bitpack y;
    { return x^y; }
  local uint_bitpack bitpack_eqv (uint_bitpack x, uint_bitpack y);
  local uint_bitpack bitpack_eqv(x,y) var reg1 uint_bitpack x; var reg2 uint_bitpack y;
    { return ~(x^y); }
  local uint_bitpack bitpack_nand (uint_bitpack x, uint_bitpack y);
  local uint_bitpack bitpack_nand(x,y) var reg1 uint_bitpack x; var reg2 uint_bitpack y;
    { return ~(x&y); }
  local uint_bitpack bitpack_nor (uint_bitpack x, uint_bitpack y);
  local uint_bitpack bitpack_nor(x,y) var reg1 uint_bitpack x; var reg2 uint_bitpack y;
    { return ~(x|y); }
  local uint_bitpack bitpack_andc1 (uint_bitpack x, uint_bitpack y);
  local uint_bitpack bitpack_andc1(x,y) var reg2 uint_bitpack x; var reg1 uint_bitpack y;
    { return (~x)&y; }
  local uint_bitpack bitpack_andc2 (uint_bitpack x, uint_bitpack y);
  local uint_bitpack bitpack_andc2(x,y) var reg1 uint_bitpack x; var reg2 uint_bitpack y;
    { return x&(~y); }
  local uint_bitpack bitpack_orc1 (uint_bitpack x, uint_bitpack y);
  local uint_bitpack bitpack_orc1(x,y) var reg2 uint_bitpack x; var reg1 uint_bitpack y;
    { return (~x)|y; }
  local uint_bitpack bitpack_orc2 (uint_bitpack x, uint_bitpack y);
  local uint_bitpack bitpack_orc2(x,y) var reg1 uint_bitpack x; var reg2 uint_bitpack y;
    { return x|(~y); }
  local uint_bitpack bitpack_not (uint_bitpack x, uint_bitpack y);
  local uint_bitpack bitpack_not(x,y) var reg1 uint_bitpack x; var uint_bitpack y;
    { return ~x; }

LISPFUN(bit_and,2,1,norest,nokey,0,NIL)
# (BIT-AND bit-array1 bit-array2 [result-bit-array]), CLTL S. 294
  { return_Values bit_up(&bitpack_and); }

LISPFUN(bit_ior,2,1,norest,nokey,0,NIL)
# (BIT-IOR bit-array1 bit-array2 [result-bit-array]), CLTL S. 294
  { return_Values bit_up(&bitpack_ior); }

LISPFUN(bit_xor,2,1,norest,nokey,0,NIL)
# (BIT-XOR bit-array1 bit-array2 [result-bit-array]), CLTL S. 294
  { return_Values bit_up(&bitpack_xor); }

LISPFUN(bit_eqv,2,1,norest,nokey,0,NIL)
# (BIT-EQV bit-array1 bit-array2 [result-bit-array]), CLTL S. 294
  { return_Values bit_up(&bitpack_eqv); }

LISPFUN(bit_nand,2,1,norest,nokey,0,NIL)
# (BIT-NAND bit-array1 bit-array2 [result-bit-array]), CLTL S. 294
  { return_Values bit_up(&bitpack_nand); }

LISPFUN(bit_nor,2,1,norest,nokey,0,NIL)
# (BIT-NOR bit-array1 bit-array2 [result-bit-array]), CLTL S. 294
  { return_Values bit_up(&bitpack_nor); }

LISPFUN(bit_andc1,2,1,norest,nokey,0,NIL)
# (BIT-ANDC1 bit-array1 bit-array2 [result-bit-array]), CLTL S. 294
  { return_Values bit_up(&bitpack_andc1); }

LISPFUN(bit_andc2,2,1,norest,nokey,0,NIL)
# (BIT-ANDC2 bit-array1 bit-array2 [result-bit-array]), CLTL S. 294
  { return_Values bit_up(&bitpack_andc2); }

LISPFUN(bit_orc1,2,1,norest,nokey,0,NIL)
# (BIT-ORC1 bit-array1 bit-array2 [result-bit-array]), CLTL S. 294
  { return_Values bit_up(&bitpack_orc1); }

LISPFUN(bit_orc2,2,1,norest,nokey,0,NIL)
# (BIT-ORC2 bit-array1 bit-array2 [result-bit-array]), CLTL S. 294
  { return_Values bit_up(&bitpack_orc2); }

LISPFUN(bit_not,1,1,norest,nokey,0,NIL)
# (BIT-NOT bit-array [result-bit-array]), CLTL S. 295
  { # erstes Argument verdoppeln (wird bei der Operation ignoriert):
    {var reg1 object array3 = STACK_0; pushSTACK(array3); }
    STACK_1 = STACK_2;
    return_Values bit_up(&bitpack_not);
  }

# UP: Testet, ob ein Array einen Fill-Pointer hat.
# array_has_fill_pointer_p(array)
# > array: ein Array
# < TRUE, falls ja; FALSE falls nein.
  global boolean array_has_fill_pointer_p (object array);
  global boolean array_has_fill_pointer_p(array)
    var reg1 object array;
    { if_simplep(array,
        { return FALSE; },
        { if (TheArray(array)->flags & bit(arrayflags_fillp_bit))
            return TRUE;
            else
            return FALSE;
        });
    }

LISPFUNN(array_has_fill_pointer_p,1) # (ARRAY-HAS-FILL-POINTER-P array), CLTL S. 296
  { var reg1 object array = popSTACK();
    test_array(array);
    value1 = (array_has_fill_pointer_p(array) ? T : NIL); mv_count=1;
  }

# Fehlermeldung
# fehler_vector(object)
# > object: Nicht-Vektor
# > subr_self: Aufrufer (ein SUBR)
  local nonreturning void fehler_vector (object obj);
  local nonreturning void fehler_vector(obj)
    var reg1 object obj;
    { pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(
             DEUTSCH ? "~: ~ ist kein Vektor." :
             ENGLISH ? "~: ~ is not a vector" :
             FRANCAIS ? "~: ~ n'est pas un vecteur." :
             ""
            );
    }

# Überprüft, ob ein Objekt ein Vektor mit Fill-Pointer ist, und liefert
# die Adresse des Fill-Pointers.
# *get_fill_pointer(obj) ist dann der Fill-Pointer selbst.
# get_fill_pointer(obj)[-1] ist dann die Länge (Dimension 0) des Vektors.
# > subr_self: Aufrufer (ein SUBR)
  local uintL* get_fill_pointer (object obj);
  local uintL* get_fill_pointer(obj)
    var reg2 object obj;
    { # obj muß ein Vektor sein:
      if_vectorp(obj, ; , { fehler_vector(obj); });
      # darf nicht simple sein:
      if_simplep(obj, { goto fehler_fillp; } , ; );
      # muß einen Fill-Pointer enthalten:
      if (!(TheArray(obj)->flags & bit(arrayflags_fillp_bit))) { goto fehler_fillp; }
      # Wo steht der Fill-Pointer?
      return ((TheArray(obj)->flags & bit(arrayflags_dispoffset_bit))
              ? &TheArray(obj)->dims[2] # nach Displaced-Offset und Dimension 0
              : &TheArray(obj)->dims[1] # nach der Dimension 0
             );
      # Fehlermeldung:
      fehler_fillp:
        pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
        fehler(
               DEUTSCH ? "~: Vektor ~ hat keinen Fill-Pointer." :
               ENGLISH ? "~: vector ~ has no fill pointer" :
               FRANCAIS ? "~: Le vecteur ~ n'a pas de pointeur de remplissage." :
               ""
              );
    }

LISPFUNN(fill_pointer,1) # (FILL-POINTER vector), CLTL S. 296
  { var reg1 object obj = popSTACK();
    value1 = fixnum(* get_fill_pointer(obj)); # Fill-Pointer holen, als Fixnum
    mv_count=1;
  }

LISPFUNN(set_fill_pointer,2) # (SYS::SET-FILL-POINTER vector index)
                             # = (SETF (FILL-POINTER vector) index), CLTL S. 296
  { var reg1 uintL* fillp = get_fill_pointer(STACK_1); # Fillpointer-Adresse
    if (!mposfixnump(STACK_0)) # neuer Fill-Pointer muß Fixnum>=0 sein.
      fehler_index_type();
   {var reg1 uintL newfillp = posfixnum_to_L(STACK_0); # als uintL
    if (!(newfillp <= fillp[-1])) # muß kleinergleich der Länge sein
      fehler_index_range();
    *fillp = newfillp; # neuen Fill-Pointer eintragen
    value1 = STACK_0; mv_count=1; # neuen Fillpointer zurück
    skipSTACK(2);
  }}

LISPFUNN(vector_push,2) # (VECTOR-PUSH new-element vector), CLTL S. 296
  { var reg1 uintL* fillp = get_fill_pointer(STACK_0); # Fillpointer-Adresse
    var reg2 uintL oldfillp = *fillp; # alter Wert des Fillpointers
    if (oldfillp >= fillp[-1]) # Fill-Pointer am Ende?
      { value1 = NIL; mv_count=1; } # NIL zurück
      else
      { var uintL index = oldfillp;
        var reg4 object datenvektor = notsimple_displace(STACK_0,&index);
        datenvektor_store(datenvektor,index,STACK_1); # new-element eintragen
        (*fillp)++; # Fill-Pointer erhöhen
        value1 = fixnum(oldfillp); mv_count=1;
        # alter Fill-Pointer als Wert
      }
    skipSTACK(2);
  }

LISPFUNN(vector_pop,1) # (VECTOR-POP vector), CLTL S. 296
  { var reg2 object array = popSTACK();
    var reg1 uintL* fillp = get_fill_pointer(array);
    if (*fillp==0)
      { # Fill-Pointer war =0 -> Fehlermeldung
        pushSTACK(array); pushSTACK(TheSubr(subr_self)->name);
        fehler(
               DEUTSCH ? "~: ~ hat keine aktiven Elemente." :
               ENGLISH ? "~: ~ has length zero" :
               FRANCAIS ? "~: ~ ne contient pas d'éléments actifs (la longueur est nulle)." :
               ""
              );
      }
      else
      { var uintL index = --(*fillp); # Fill-Pointer erniedrigen
        var reg4 object datenvektor = notsimple_displace(array,&index);
        value1 = datenvektor_aref(datenvektor,index); mv_count=1; # Element zurück
      }
  }

LISPFUN(vector_push_extend,2,1,norest,nokey,0,NIL)
# (VECTOR-PUSH-EXTEND new-element vector [extension]), CLTL S. 296
  { var reg3 object extension = popSTACK(); # Extension (ungeprüft)
    var reg1 uintL* fillp = get_fill_pointer(STACK_0); # Fillpointer-Adresse
    var reg2 uintL oldfillp = *fillp; # alter Wert des Fillpointers
    if (oldfillp < fillp[-1]) # Fill-Pointer noch nicht am Ende?
      { var uintL index = oldfillp;
        var reg4 object datenvektor = notsimple_displace(STACK_0,&index);
        datenvektor_store(datenvektor,index,STACK_1); # new-element eintragen
        (*fillp)++; # Fill-Pointer erhöhen
      }
      else
      { # Fill-Pointer am Ende -> Versuche, den Vektor zu verlängern:
        var reg3 object array = STACK_0;
        if (!(TheArray(array)->flags & bit(arrayflags_adjustable_bit)))
          { # Vektor nicht adjustable -> Fehlermeldung:
            # array noch in STACK_0
            pushSTACK(TheSubr(subr_self)->name);
            fehler(
                   DEUTSCH ? "~ funktioniert nur auf adjustierbaren Arrays, nicht auf ~" :
                   ENGLISH ? "~ works only on adjustable arrays, not on ~" :
                   FRANCAIS ? "~ ne fonctionne qu'avec des matrices ajustables et non avec ~." :
                   ""
                  );
          }
        { var reg3 uintB atype = TheArray(array)->flags & arrayflags_atype_mask;
          var uintL len = fillp[-1]; # bisherige Länge (Dimension 0)
          var reg5 uintL inc; # gewünschter Increment der Länge
          if (!eq(extension,unbound))
            { # extension sollte ein Fixnum >0, <arraysize_limit sein:
              if ( (!(posfixnump(extension)))
                   || ((inc = posfixnum_to_L(extension)) == 0)
                   || (inc > arraysize_limit_1)
                 )
                { pushSTACK(extension); pushSTACK(TheSubr(subr_self)->name);
                  fehler(
                         DEUTSCH ? "~: Extension ~ sollte ein Fixnum > 0 sein." :
                         ENGLISH ? "~: extension ~ should be a positive fixnum" :
                         FRANCAIS ? "~: L'extension ~ doit être de type FIXNUM strictement positif." :
                         ""
                        );
            }   }
            else
            { # Default-Verlängerung:
              switch (atype)
                { case Atype_T:           inc =  16; break; # bei general-Vektoren: 16 Objekte
                  case Atype_String_Char: inc =  64; break; # bei Strings: 64 Zeichen
                  case Atype_Bit:         inc = 128; break; # bei Bit-Vektoren: 128 Bits
                  case Atype_2Bit: case Atype_4Bit: case Atype_8Bit:
                  case Atype_16Bit: case Atype_32Bit: # bei Byte-Vektoren: entsprechend
                                          inc = bit(floor(14-atype,2)); break;
                  default: NOTREACHED
                }
              # mindestens jedoch die bisherige Länge:
              if (inc<len) { inc = len; }
            }
          { var reg4 uintL newlen = len + inc; # neue Länge
            if (newlen > arraysize_limit_1)
              { # Vektor würde zu lang -> Fehlermeldung
                pushSTACK(extension); pushSTACK(TheSubr(subr_self)->name);
                fehler(
                       DEUTSCH ? "~: Durch die angegebene Extension von ~ wird der Vektor zu lang." :
                       ENGLISH ? "~: extending the vector by ~ elements makes it too long" :
                       FRANCAIS ? "~: Étendre le vecteur de ~ le rend trop long." :
                       ""
                      );
              }
            { # Neuen Datenvektor holen. Dazu Fallunterscheidung je nach Typ:
              var reg2 object neuer_datenvektor;
              switch (atype)
                { case Atype_T: # array ist ein General-Vector
                    neuer_datenvektor = allocate_vector(newlen);
                    array = STACK_0; # array wieder holen
                    { var uintL index = 0;
                      var reg4 object datenvektor = notsimple_displace(array,&index);
                      # alten in neuen Datenvektor kopieren:
                      var reg1 object* ptr1 = &TheSvector(datenvektor)->data[index];
                      var reg2 object* ptr2 = &TheSvector(neuer_datenvektor)->data[0];
                      var reg3 uintL count;
                      dotimesL(count,len, { *ptr2++ = *ptr1++; } );
                      # dann new_element anfügen:
                      *ptr2 = STACK_1;
                    }
                    break;
                  case Atype_String_Char: # array ist ein String
                    neuer_datenvektor = allocate_string(newlen);
                    array = STACK_0; # array wieder holen
                    { var uintL index = 0;
                      var reg4 object datenvektor = notsimple_displace(array,&index);
                      # alten in neuen Datenvektor kopieren:
                      var reg1 uintB* ptr1 = &TheSstring(datenvektor)->data[index];
                      var reg2 uintB* ptr2 = &TheSstring(neuer_datenvektor)->data[0];
                      var reg3 uintL count;
                      dotimesL(count,len, { *ptr2++ = *ptr1++; } );
                      # dann new_element anfügen:
                      if (!(string_char_p(STACK_1))) goto fehler_type;
                      *ptr2 = char_code(STACK_1);
                    }
                    break;
                  case Atype_Bit: # array ist ein Bit-Vektor
                  case Atype_2Bit: case Atype_4Bit: case Atype_8Bit: 
                  case Atype_16Bit: case Atype_32Bit: # array ist ein Byte-Vektor
                    neuer_datenvektor = (atype==Atype_Bit
                                         ? allocate_bit_vector(newlen)
                                         : allocate_byte_vector(atype,newlen)
                                        );
                    array = STACK_0; # array wieder holen
                    { var uintL index = 0;
                      var reg4 object datenvektor = notsimple_displace(array,&index);
                      index = index << atype;
                      { # alten in neuen Datenvektor kopieren:
                        var reg1 uint_bitpack* ptr1 = &((uint_bitpack*)(&TheSbvector(atype==Atype_Bit ? datenvektor : TheArray(datenvektor)->data)->data[0]))[index/bitpack];
                        var reg2 uint_bitpack* ptr2 = (uint_bitpack*)(&TheSbvector(atype==Atype_Bit ? neuer_datenvektor : TheArray(neuer_datenvektor)->data)->data[0]);
                        var reg5 uintL bitpackcount = ceiling(len<<atype,bitpack); # Anzahl der zu schreibenden Worte
                        # kopiere bitpackcount Words, von ptr1 ab (dabei um
                        # (index mod bitpack) Bits nach links schieben), mit
                        # Ziel ab ptr2. (Eventuell schießt man über den Source-
                        # Datenvektor hinweg, aber das macht nichts.)
                        var reg3 uintL shift = index % bitpack;
                        if (shift==0)
                          { # keine Verschiebung nötig
                            var reg3 uintL count;
                            dotimesL(count,bitpackcount, { *ptr2++ = *ptr1++; } );
                          }
                          else
                          { # beim Kopieren um shift Bits links schieben.
                            ptr1 += bitpackcount; ptr2 += bitpackcount; # von hinten anfangen
                           {var reg1 uint_2bitpack carry = L_bitpack(LR_0_bitpack(*ptr1)<<shift);
                            var reg3 uintL count;
                            dotimesL(count,bitpackcount,
                                      # Hier enthalten die rechten shift Bits von carry
                                      # den Übertrag von rechts, sonst Null.
                                      { carry |= LR_0_bitpack(*--ptr1)<<shift;
                                        *--ptr2 = R_bitpack(carry);
                                        carry = L_bitpack(carry);
                                      });
                      }   }}
                      index = index >> atype;
                      # new-element eintragen:
                      datenvektor_store(datenvektor,index,STACK_1);
                    }
                    break;
                  default: NOTREACHED
                  fehler_type:
                    { var reg1 object new_element = STACK_1;
                      # Vektor bereits in STACK_0.
                      pushSTACK(new_element); pushSTACK(TheSubr(subr_self)->name);
                      fehler(
                             DEUTSCH ? "~: Das Objekt ~ kann nicht in den Array ~ geschoben werden, weil vom falschen Typ." :
                             ENGLISH ? "~: cannot push ~ into array ~ (bad type)" :
                             FRANCAIS ? "~: L'objet ~ ne peut pas être poussé dans la matrice ~ car il est de mauvais type." :
                             ""
                            );
                    }
                }
              set_break_sem_1(); # Unterbrechungen verbieten
              TheArray(array)->data = neuer_datenvektor; # neuen Vektor als Datenvektor eintragen
              TheArray(array)->flags &= ~bit(arrayflags_displaced_bit); # Displaced-Bit löschen
              TheArray(array)->dims[2] += 1; # Fillpointer um 1 erhöhen
              TheArray(array)->dims[1] = newlen; # neue Länge eintragen
              TheArray(array)->totalsize = newlen; # ist auch neue totalsize
              clr_break_sem_1(); # Unterbrechungen wieder zulassen
          } }
      } }
    value1 = fixnum(oldfillp); mv_count=1;
    # alter Fill-Pointer als Wert
    skipSTACK(2);
  }

# UP: erzeugt einen mit Nullen gefüllten Bitvektor
# allocate_bit_vector_0(len)
# > uintL len: Länge des Bitvektors (in Bits)
# < ergebnis: neuer Bitvektor, mit Nullen gefüllt
# kann GC auslösen
  global object allocate_bit_vector_0 (uintL len);
  global object allocate_bit_vector_0(len)
    var reg4 uintL len;
    { var reg3 object new = allocate_bit_vector(len); # neuer Bit-Vektor
      var reg2 uintL count = ceiling(len,bitpack); # ceiling(len/bitpack) Worte mit Nullen füllen
      if (!(count==0))
        { var reg1 uint_bitpack* ptr = (uint_bitpack*)(&TheSbvector(new)->data[0]);
          dotimespL(count,count, { *ptr++ = 0; } );
        }
      return new;
    }

#if 0 # nur als Reserve, für den Fall, daß wir wieder auf ein GCC-Bug stoßen

# UP: löscht ein Bit in einem Simple-Bit-Vector
# sbvector_bclr(sbvector,index);
# > sbvector: ein Simple-Bit-Vector
# > index: Index (Variable, sollte < (length sbvector) sein)
  global void sbvector_bclr (object sbvector, uintL index);
  global void sbvector_bclr(sbvector,index)
    var reg1 object sbvector;
    var reg2 uintL index;
    { # im Byte (index div 8) das Bit 7 - (index mod 8) löschen:
      TheSbvector(sbvector)->data[index/8] &= ~bit((~index) % 8);
    }

# UP: setzt ein Bit in einem Simple-Bit-Vector
# sbvector_bset(sbvector,index);
# > sbvector: ein Simple-Bit-Vector
# > index: Index (Variable, sollte < (length sbvector) sein)
  global void sbvector_bset (object sbvector, uintL index);
  global void sbvector_bset(sbvector,index)
    var reg1 object sbvector;
    var reg2 uintL index;
    { # im Byte (index div 8) das Bit 7 - (index mod 8) setzen:
      TheSbvector(sbvector)->data[index/8] |= bit((~index) % 8);
    }

#endif

# Folgende beide Funktionen arbeiten auf "Semi-Simple String"s.
# Das sind STRING-CHAR-Arrays mit FILL-POINTER, die aber nicht adjustierbar
# und nicht displaced sind und deren Datenvektor ein Simple-String ist.
# Beim Überschreiten der Länge wird ihre Länge verdoppelt
# (so daß der Aufwand fürs Erweitern nicht sehr ins Gewicht fällt).

# UP: Liefert einen Semi-Simple String gegebener Länge, Fill-Pointer =0.
# make_ssstring(len)
# > uintL len: Länge >0
# < ergebnis: neuer Semi-Simple String dieser Länge
# kann GC auslösen
  global object make_ssstring (uintL len);
  global object make_ssstring(len)
    var reg2 uintL len;
    { {var reg1 object new_string = allocate_string(len);
       # neuer Simple-String dieser Länge
       pushSTACK(new_string); # retten
      }
      {var reg1 object new_array =
         allocate_array(bit(arrayflags_fillp_bit)|bit(arrayflags_notbytep_bit)|Atype_String_Char,1,string_type);
         # Flags: nur FILL_POINTER_BIT, Elementtyp STRING-CHAR, Rang=1
       TheArray(new_array)->dims[1] = 0; # Fill-Pointer := 0
       TheArray(new_array)->totalsize =
         TheArray(new_array)->dims[0] = len; # Länge und Total-Size eintragen
       TheArray(new_array)->data = popSTACK(); # Datenvektor eintragen
       return new_array;
    } }

# UP: Schiebt ein String-Char in einen Semi-Simple String und erweitert ihn
# dabei eventuell.
# ssstring_push_extend(ssstring,ch)
# > ssstring: Semi-Simple String
# > ch: Character
# < ergebnis: derselbe Semi-Simple String
# kann GC auslösen
  global object ssstring_push_extend (object ssstring, uintB ch);
  global object ssstring_push_extend(ssstring,ch)
    var reg2 object ssstring;
    var reg3 uintB ch;
    { var reg1 object sstring = TheArray(ssstring)->data; # Datenvektor (ein Simple-String)
      if (TheArray(ssstring)->dims[1] # Fill-Pointer
          >= TheSstring(sstring)->length ) # >= Länge ?
        { # ja -> String wird um den Faktor 2 länger gemacht
          pushSTACK(ssstring); # ssstring retten
          pushSTACK(sstring); # Datenvektor ebenfalls retten
         {var reg4 object neuer_sstring = allocate_string(2 * TheSstring(sstring)->length);
          # neuer Simple-String der doppelten Länge
          sstring = popSTACK(); # sstring zurück
          # Stringinhalt von String sstring nach String neuer_sstring kopieren:
          { var reg1 uintB* ptr1 = &TheSstring(sstring)->data[0];
            var reg2 uintB* ptr2 = &TheSstring(neuer_sstring)->data[0];
            var reg3 uintL count;
            dotimespL(count,TheSstring(sstring)->length, { *ptr2++ = *ptr1++; } );
          }
          ssstring = popSTACK(); # ssstring zurück
          set_break_sem_1(); # Unterbrechungen verbieten
          TheArray(ssstring)->data = neuer_sstring; # neuen String als Datenvektor abspeichern
          TheArray(ssstring)->totalsize =
            TheArray(ssstring)->dims[0] = TheSstring(neuer_sstring)->length; # neue Länge eintragen
          clr_break_sem_1(); # Unterbrechungen wieder zulassen
          sstring = neuer_sstring;
        }}
      # Nun ist wieder sstring der Datenvektor, und es gilt
      # Fill-Pointer < Länge(Datenvektor).
      # Character hineinschieben und Fill-Pointer erhöhen:
      TheSstring(sstring)->data[ TheArray(ssstring)->dims[1]++ ] = ch;
      return ssstring;
    }

#ifdef STRM_WR_SS
# UP: Stellt sicher, daß ein Semi-Simple String eine bestimmte Länge hat
# und erweitert ihn dazu eventuell.
# ssstring_extend(ssstring,size)
# > ssstring: Semi-Simple String
# > size: gewünschte Mindestgröße
# < ergebnis: derselbe Semi-Simple String
# kann GC auslösen
  global object ssstring_extend (object ssstring, uintL needed_len);
  global object ssstring_extend(ssstring,needed_len)
    var reg4 object ssstring;
    var reg8 uintL needed_len;
    { var reg5 object sstring = TheArray(ssstring)->data; # Datenvektor (ein Simple-String)
      var reg7 uintL now_len = TheSstring(sstring)->length; # jetzige Maximal-Länge
      if (needed_len > now_len)
        { # ja -> String wird länger gemacht, mindestens um den Faktor 2:
          pushSTACK(ssstring); # ssstring retten
          pushSTACK(sstring); # Datenvektor ebenfalls retten
          now_len = now_len * 2;
          if (needed_len > now_len) { now_len = needed_len; } # now_len vergrößern
         {var reg6 object neuer_sstring = allocate_string(now_len);
          # neuer Simple-String mindestens der gewünschten und der doppelten Länge
          sstring = popSTACK(); # sstring zurück
          # Stringinhalt von String sstring nach String neuer_sstring kopieren:
          { var reg1 uintB* ptr1 = &TheSstring(sstring)->data[0];
            var reg2 uintB* ptr2 = &TheSstring(neuer_sstring)->data[0];
            var reg3 uintL count;
            dotimespL(count,TheSstring(sstring)->length, { *ptr2++ = *ptr1++; } );
          }
          ssstring = popSTACK(); # ssstring zurück
          set_break_sem_1(); # Unterbrechungen verbieten
          TheArray(ssstring)->data = neuer_sstring; # neuen String als Datenvektor abspeichern
          TheArray(ssstring)->totalsize =
            TheArray(ssstring)->dims[0] = now_len; # neue Länge eintragen
          clr_break_sem_1(); # Unterbrechungen wieder zulassen
        }}
      return ssstring;
    }
#endif


# Stackaufbau bei MAKE-ARRAY :
#   dims, adjustable, element-type, initial-element, initial-contents,
#   fill-pointer, displaced-to, displaced-index-offset.
# Stackaufbau bei ADJUST-ARRAY :
#   dims, array, element-type, initial-element, initial-contents,
#   fill-pointer, displaced-to, displaced-index-offset.

# Fehlermeldung
# > dim: fehlerhafte Dimension
# > subr_self: Aufrufer (ein SUBR)
  local nonreturning void fehler_dim_type (object dim);
  local nonreturning void fehler_dim_type(dim)
    var reg1 object dim;
    { pushSTACK(dim);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(
             DEUTSCH ? "~: Dimension ~ ist nicht vom Typ `(INTEGER 0 (,ARRAY-DIMENSION-LIMIT))." :
             ENGLISH ? "~: dimension ~ is not of type `(INTEGER 0 (,ARRAY-DIMENSION-LIMIT))" :
             FRANCAIS ? "~: La dimension ~ n'est pas de type  `(INTEGER 0 (,ARRAY-DIMENSION-LIMIT))." :
             ""
            );
    }

# Hilfsroutine für MAKE-ARRAY und ADJUST-ARRAY:
# Überprüft die Dimensionen und liefert den Rang und die Gesamtgröße.
# test_dims(&totalsize)
# > STACK_7: Dimension oder Dimensionenliste
# > subr_self: Aufrufer (ein SUBR)
# < totalsize: Gesamtgröße = Produkt der Dimensionen
# < ergebnis: Rang = Anzahl der Dimensionen
  local uintL test_dims (uintL* totalsize_);
  local uintL test_dims(totalsize_)
    var reg5 uintL* totalsize_;
    { var reg2 object dims = STACK_7;
      if (listp(dims))
        { var reg4 uintL rank = 0; # bisherige Anzahl der Dimensionen
          var reg3 uintL totalsize = 1; # bisheriges Produkt der Dimensionen,
                                        # bleibt < arraysize_limit
          while (consp(dims))
            { var reg1 object dim = Car(dims); # nächste Dimension
              # if (!integerp(dim)) { fehler_dim_type(dim); } # muß Integer sein
              if (!posfixnump(dim)) { fehler_dim_type(dim); } # muß Fixnum >=0 sein
              # totalsize * dim bilden:
             {var reg7 uintL produkt_hi;
              var reg6 uintL produkt_lo;
              #if (oint_addr_len<=24)
              mulu24(totalsize,posfixnum_to_L(dim), produkt_hi=,produkt_lo=);
              #else
              mulu32(totalsize,posfixnum_to_L(dim), produkt_hi=,produkt_lo=);
              #endif
              if (!((produkt_hi==0) && (produkt_lo<=arraysize_limit_1))) # Produkt < 2^24 ?
                { # nein -> (sofern nicht noch eine Dimension=0 kommt)
                  # Total-Size zu groß
                  pushSTACK(STACK_7); # dims
                  pushSTACK(TheSubr(subr_self)->name);
                  fehler(
                         DEUTSCH ? "~: Dimensionen ~ ergeben zu große Gesamtgröße." :
                         ENGLISH ? "~: dimensions ~ produce too large total-size" :
                         FRANCAIS ? "~: Les dimensions ~ donnent une taille totale trop grande." :
                         ""
                        );
                }
              totalsize = produkt_lo;
              rank++;
              dims = Cdr(dims);
            }}
          *totalsize_ = totalsize;
          return rank;
        }
      # dims ist keine Liste. Sollte eine einzelne Dimension sein:
      # if (!integerp(dims)) { fehler_dim_type(dims); } # muß Integer sein
      if (!posfixnump(dims)) { fehler_dim_type(dims); } # muß Fixnum >=0 sein
      *totalsize_ = posfixnum_to_L(dims); # Totalsize = einzige Dimension
      return 1; # Rang = 1
    }

# Hilfsroutine für MAKE-ARRAY und ADJUST-ARRAY:
# Überprüft einige der Keywords.
  local void test_otherkeys (void);
  local void test_otherkeys()
    { # fill-pointer hat Defaultwert NIL:
      if (eq(STACK_2,unbound)) { STACK_2 = NIL; }
      # displaced-to hat Defaultwert NIL:
      if (eq(STACK_1,unbound)) { STACK_1 = NIL; }
      # Testen, ob mehr als eine Initialisierung
      # (:initial-element, :initial-contents, :displaced-to) angegeben wurde:
      { var reg1 uintB initcount = 0; # Zähler
        if (!(eq(STACK_4,unbound))) { initcount++; } # initial-element angegeben?
        if (!(eq(STACK_3,unbound))) { initcount++; } # initial-contents angegeben?
        if (!nullp(STACK_1)) { initcount++; } # displaced-to angegeben?
        if (initcount > 1) # Mehr als eine Initialisierung?
          { pushSTACK(TheSubr(subr_self)->name);
            fehler(
                   DEUTSCH ? "~: Mehr als eine Initialisierung angegeben." :
                   ENGLISH ? "~: ambiguous, more than one initialisation specified" :
                   FRANCAIS ? "~: Il fut indiqué plus d'une initialisation, c'est ambigu." :
                   ""
                  );
      }   }
      # Testen, ob :displaced-index-offset ohne :displaced-to verwendet wurde:
      if ((!eq(STACK_0,unbound)) # displaced-index-offset angegeben?
          && (nullp(STACK_1)) # und displaced-to nicht angegeben?
         )
        { pushSTACK(S(Kdisplaced_to));
          pushSTACK(S(Kdisplaced_index_offset));
          pushSTACK(TheSubr(subr_self)->name);
            fehler(
                   DEUTSCH ? "~: ~ darf nur zusammen mit ~ verwendet werden." :
                   ENGLISH ? "~: ~ must not be specified without ~" :
                   FRANCAIS ? "~: ~ ne peut être utilisé qu'avec ~." :
                   ""
                  );
        }
    }

# Hilfsroutine für MAKE-ARRAY und ADJUST-ARRAY:
# erzeugt einen Datenvektor gegebener Länge
# und füllt ihn mit initial-element, falls angegeben.
# make_datenvektor(len,eltype)
# > len: Länge
# > eltype: Elementtyp-Code
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: einfacher Vektor des gegebenen Typs, evtl. gefüllt.
# kann GC auslösen
  local object make_datenvektor (uintL len, uintB eltype);
  local object make_datenvektor(len,eltype)
    var reg2 uintL len;
    var reg4 uintB eltype;
    { switch (eltype)
        { case Atype_T: # Simple-Vector erzeugen
            { var reg5 object vektor = allocate_vector(len);
              if (!(eq(STACK_4,unbound))) # initial-element angegeben?
                if (!(len==0)) # und Länge > 0 ?
                  { # ja -> Vektor mit initial-element füllen:
                    var reg1 object* ptr = &TheSvector(vektor)->data[0];
                    var reg3 object initial_element = STACK_4;
                    dotimespL(len,len, { *ptr++ = initial_element; });
                  }
              return vektor;
            }
          case Atype_Bit: # Simple-Bit-Vector erzeugen
            { var reg5 object vektor = allocate_bit_vector(len);
              if (!(eq(STACK_4,unbound))) # initial-element angegeben?
                { # ja -> überprüfen:
                  var reg3 uint_bitpack initial_bitpack;
                  if (eq(STACK_4,Fixnum_0)) { initial_bitpack = 0UL; } # 0 -> mit Nullword füllen
                  elif (eq(STACK_4,Fixnum_1)) { initial_bitpack = ~0UL; } # 1 -> mit Einsenword füllen
                  else goto fehler_init;
                  if (!(len==0)) # und Länge > 0 ?
                    { # ja -> Vektor mit initial-element füllen:
                      var reg1 uint_bitpack* ptr = (uint_bitpack*)(&TheSbvector(vektor)->data[0]);
                      dotimespL(len,ceiling(len,bitpack), { *ptr++ = initial_bitpack; });
                }   }
              return vektor;
            }
          case Atype_String_Char: # Simple-String erzeugen
            { var reg5 object vektor = allocate_string(len);
              if (!(eq(STACK_4,unbound))) # initial-element angegeben?
                { # ja -> überprüfen, muß String-Char sein:
                  if (!(string_char_p(STACK_4))) goto fehler_init;
                 {var reg3 uintB initial_char = char_code(STACK_4);
                  if (!(len==0)) # und Länge > 0 ?
                    { # ja -> Vektor mit initial-element füllen:
                      var reg1 uintB* ptr = &TheSstring(vektor)->data[0];
                      dotimespL(len,len, { *ptr++ = initial_char; });
                }}  }
              return vektor;
            }
          case Atype_2Bit:
          case Atype_4Bit:
          case Atype_8Bit:
          case Atype_16Bit:
          case Atype_32Bit: # semi-simplen Byte-Vektor erzeugen
            { var reg5 object vektor = allocate_byte_vector(eltype,len);
              if (!(eq(STACK_4,unbound))) # initial-element angegeben?
                { # ja -> überprüfen, muß passender Integer sein:
                  var reg6 uintL wert;
                  if (eltype==Atype_32Bit)
                    { wert = I_to_UL(STACK_4); }
                    else
                    { if (!(mposfixnump(STACK_4) && ((wert = posfixnum_to_L(STACK_4)) < bit(bit(eltype)))))
                        goto fehler_init;
                    }
                  if (!(len==0))
                    switch (eltype)
                      { case Atype_2Bit:
                          len = ceiling(len,2); wert |= wert<<2;
                        case Atype_4Bit:
                          len = ceiling(len,2); wert |= wert<<4;
                        case Atype_8Bit:
                          #if !(Varobject_alignment%2 == 0)
                          { var reg1 uintB* ptr = &TheSbvector(TheArray(vektor)->data)->data[0];
                            dotimespL(len,len, { *ptr++ = wert; });
                          }
                          break;
                          #else
                          # Kann mit 16-Bit-Blöcken arbeiten
                          len = ceiling(len,2); wert |= wert<<8;
                          #endif
                        case Atype_16Bit:
                          #if !(Varobject_alignment%4 == 0)
                          { var reg1 uint16* ptr = (uint16*)(&TheSbvector(TheArray(vektor)->data)->data[0]);
                            dotimespL(len,len, { *ptr++ = wert; });
                          }
                          break;
                          #else
                          # Kann mit 32-Bit-Blöcken arbeiten
                          len = ceiling(len,2); wert |= wert<<16;
                          #endif
                        case Atype_32Bit:
                          { var reg1 uint32* ptr = (uint32*)(&TheSbvector(TheArray(vektor)->data)->data[0]);
                            dotimespL(len,len, { *ptr++ = wert; });
                          }
                          break;
                }     }
              return vektor;
            }
          default: NOTREACHED
          fehler_init:
            pushSTACK(STACK_5); # element-type
            pushSTACK(STACK_(4+1)); # initial-element
            pushSTACK(TheSubr(subr_self)->name);
            fehler(
                   DEUTSCH ? "~: Das Initialisierungselement ~ ist nicht vom Typ ~." :
                   ENGLISH ? "~: the initial-element ~ is not of type ~" :
                   FRANCAIS ? "~: L'élément initial ~ n'est pas de type ~." :
                   ""
                  );
    }   }

# Hilfsroutine für MAKE-ARRAY und ADJUST-ARRAY:
# Füllt einen Vektor lexikographisch mit dem Inhalt einer verschachtelten
# Sequence-Struktur, wie sie als Argument zum Keyword :initial-contents
# bei MAKE-ARRAY und ADJUST-ARRAY anzugeben ist.
# initial_contents(datenvektor,dims,rank,contents)
# > datenvektor: ein simpler Vektor
# > dims: Dimension oder Dimensionenliste, alle Dimensionen Fixnums,
#         Länge(datenvektor) = Produkt der Dimensionen
# > rank: Anzahl der Dimensionen
# > contents: verschachtelte Sequence-Struktur
# < ergebnis: derselbe Datenvektor
# Nicht reentrant!
# kann GC auslösen
  local object initial_contents (object datenvektor, object dims, uintL rank, object contents);
  local object* initial_contents_local; # Pointer auf Datenvektor und Dimensionen
  local uintL initial_contents_index; # Index in den Datenvektor
  local uintL initial_contents_depth; # Rekursionstiefe
  local object initial_contents(datenvektor,dims,rank,contents)
    var reg3 object datenvektor;
    var reg1 object dims;
    var reg2 uintL rank;
    var reg4 object contents;
    { # alle Dimensionen auf den Stack:
      get_space_on_STACK(rank*sizeof(object));
      if (listp(dims))
        { while (consp(dims)) { pushSTACK(Car(dims)); dims = Cdr(dims); } }
        else
        { pushSTACK(dims); }
      initial_contents_local = &STACK_0; # aktuellen STACK-Wert merken
      initial_contents_index = 0; # Index := 0
      initial_contents_depth = rank; # depth := rank
      pushSTACK(datenvektor); # Datenvektor in den Stack
      pushSTACK(subr_self); # aktuelles SUBR retten
      # initial_contents_aux aufrufen:
      pushSTACK(contents); funcall(L(initial_contents_aux),1);
      subr_self = popSTACK(); # aktuelles SUBR zurück
      datenvektor = popSTACK(); # Datenvektor zurück
      skipSTACK(rank); # STACK aufräumen
      return datenvektor;
    }

# Hilfsfunktion für initial_contents:
# Arbeitet die Sequence-Struktur rekursiv ab.
LISPFUNN(initial_contents_aux,1)
  { # Übergeben wird:
    # initial_contents_depth = Rekursionstiefe,
    # initial_contents_index = Index in den Datenvektor,
    # initial_contents_local = Pointer auf die Dimensionen,
    #   bei Tiefe depth>0 ist maßgeblich
    #   Dimension (rank-depth) = *(local+depth-1),
    #   Datenvektor = *(local-1), Aufrufer = *(local-2).
    var reg1 object* localptr = initial_contents_local;
    if (initial_contents_depth==0)
      # Tiefe 0 -> Element STACK_0 in den Datenvektor eintragen:
      { var reg2 object datenvektor = *(localptr STACKop -1);
        subr_self = *(localptr STACKop -2);
        pushSTACK(datenvektor);
        datenvektor_store(datenvektor,initial_contents_index,STACK_(0+1));
        initial_contents_index++;
        skipSTACK(2); # Stack aufräumen
      }
      else
      # Tiefe >0 -> rekursiv aufrufen:
      { initial_contents_depth--;
        # seq = STACK_0 muß eine Sequence korrekter Länge sein:
        pushSTACK(STACK_0); funcall(L(length),1); # Länge bestimmen
        # muß EQL (also EQ) zur Dimension *(local+depth) sein:
        if (!(eq(value1,*(localptr STACKop initial_contents_depth))))
          { # fehlerhafte Sequence seq noch in STACK_0.
            pushSTACK(TheSubr(*(localptr STACKop -2))->name);
            fehler(
                   DEUTSCH ? "~: ~ hat nicht die richtige Länge." :
                   ENGLISH ? "~: ~ has not the correct length" :
                   FRANCAIS ? "~: ~ n'est pas de longueur convenable." :
                   ""
                  );
          }
        # Länge stimmt, nun (MAP NIL #'INITIAL-CONTENTS-AUX seq) ausführen:
        pushSTACK(NIL); pushSTACK(L(initial_contents_aux)); pushSTACK(STACK_(0+2));
        funcall(L(map),3);
        initial_contents_depth++;
        skipSTACK(1); # Stack aufräumen
      }
    value1=NIL; mv_count=0; # keine Werte
  }

# Hilfsroutine für MAKE-ARRAY und ADJUST-ARRAY:
# Überprüfe ein displaced-to-Argument und den dazugehörigen Offset.
# test_displaced(eltype,totalsize)
# > eltype: Elementtyp-Code des zu erzeugenden Arrays
# > totalsize: Gesamtgröße des zu erzeugenden Arrays
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: Wert des displaced-index-offset
  local uintL test_displaced (uintB eltype, uintL totalsize);
  local uintL test_displaced(eltype,totalsize)
    var reg4 uintB eltype;
    var reg5 uintL totalsize;
    { # displaced-to überprüfen, muß ein Array sein:
      var reg1 object displaced_to = STACK_1;
      if_arrayp(displaced_to, ; ,
        { pushSTACK(displaced_to);
          pushSTACK(S(Kdisplaced_to));
          pushSTACK(TheSubr(subr_self)->name);
          fehler(
                 DEUTSCH ? "~: ~-Argument ~ ist kein Array." :
                 ENGLISH ? "~: ~-argument ~ is not an array" :
                 FRANCAIS ? "~: Le ~ argument ~ n'est pas une matrice." :
                 ""
                );
        });
      {# Elementtyp von displaced_to bestimmen:
       var reg2 uintB displaced_eltype;
       switch (mtypecode(STACK_1))
         { case_array1: case_obvector: # allgemeiner Array -> Arrayflags anschauen
             displaced_eltype = TheArray(displaced_to)->flags & arrayflags_atype_mask;
             break;
           # Zuordnung  Vektor-Typinfo -> ATYPE-Byte :
           case_sbvector: displaced_eltype = Atype_Bit; break;
           case_string: displaced_eltype = Atype_String_Char; break;
           case_vector: displaced_eltype = Atype_T; break;
           default: NOTREACHED
         }
       # displaced_eltype ist der ATYPE des :displaced-to-Arguments.
       # Gegebenen Elementtyp damit vergleichen:
       if (!(eltype == displaced_eltype))
         { pushSTACK(STACK_5); # element-type
           pushSTACK(displaced_to);
           pushSTACK(S(Kdisplaced_to));
           pushSTACK(TheSubr(subr_self)->name);
           fehler(
                  DEUTSCH ? "~: ~-Argument ~ hat nicht den Elementtyp ~." :
                  ENGLISH ? "~: ~-argument ~ has not element type ~" :
                  FRANCAIS ? "~: Le ~ argument ~ n'a pas ~ comme type d'élément." :
                  ""
                 );
         }
      }
      {# Displaced-Index-Offset überprüfen:
       var reg2 uintL displaced_index_offset;
       if (eq(STACK_0,unbound)) { displaced_index_offset = 0; } # Default ist 0
       elif (mposfixnump(STACK_0)) { displaced_index_offset = posfixnum_to_L(STACK_0); }
       else
         { pushSTACK(STACK_0);
           pushSTACK(S(Kdisplaced_index_offset));
           pushSTACK(TheSubr(subr_self)->name);
           fehler(
                  DEUTSCH ? "~: ~-Argument ~ ist nicht vom Typ `(INTEGER 0 (,ARRAY-TOTAL-SIZE-LIMIT))." :
                  ENGLISH ? "~: ~-argument ~ is not of type `(INTEGER 0 (,ARRAY-TOTAL-SIZE-LIMIT))" :
                  FRANCAIS ? "~: Le ~ argument ~ n'est pas de type `(INTEGER 0 (,ARRAY-TOTAL-SIZE-LIMIT))." :
                  ""
                 );
         }
       {# Überprüfen, ob angesprochenes Teilstück ganz in displaced-to paßt:
        var reg3 uintL displaced_totalsize = array_total_size(displaced_to);
        if (!(displaced_index_offset+totalsize <= displaced_totalsize))
          { pushSTACK(S(Kdisplaced_to));
            pushSTACK(fixnum(displaced_totalsize));
            pushSTACK(fixnum(displaced_index_offset));
            pushSTACK(TheSubr(subr_self)->name);
            fehler(
                   DEUTSCH ? "~: Array-Gesamtgröße mit Displaced-Offset (~) > Gesamtgröße ~ des ~-Arguments" :
                   ENGLISH ? "~: array-total-size + displaced-offset (= ~) exceeds total size ~ of ~-argument" :
                   FRANCAIS ? "~: La taille totale de la matrice avec «displaced-offset» (~) est supérieure à la taille totale ~ du ~ argument." :
                   ""
                  );
       }  }
       return displaced_index_offset;
    } }

# Hilfsroutine für MAKE-ARRAY und ADJUST-ARRAY:
# Überprüfe ein fill-pointer-Argument /=NIL.
# test_fillpointer(len)
# > totalsize: Maximalwert von fill-pointer
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: Wert des fill-pointer
  local uintL test_fillpointer (uintL totalsize);
  local uintL test_fillpointer(totalsize)
    var reg1 uintL totalsize;
    { # fill-pointer war angegeben und /=NIL
      if (eq(STACK_2,S(t))) # T angegeben ->
        { return totalsize; } # Fill-Pointer := Länge = Gesamtgröße
      elif (!mposfixnump(STACK_2)) # kein Fixnum >=0 -> Fehler
        { pushSTACK(STACK_2);
          pushSTACK(TheSubr(subr_self)->name);
          fehler(
                 DEUTSCH ? "~: Gewünschter Fill-Pointer ~ sollte ein Fixnum >=0 sein." :
                 ENGLISH ? "~: fill-pointer ~ should be a nonnegative fixnum" :
                 FRANCAIS ? "~: Le pointeur de remplissage ~ devrait être de type FIXNUM positif ou zéro." :
                 ""
                );
        }
      else
        { var reg2 uintL fillpointer = posfixnum_to_L(STACK_2);
          if (!(fillpointer <= totalsize)) # mit Länge vergleichen
            { pushSTACK(fixnum(totalsize));
              pushSTACK(STACK_(2+1));
              pushSTACK(TheSubr(subr_self)->name);
              fehler(
                     DEUTSCH ? "~: Gewünschter Fill-Pointer ~ ist größer als die Länge ~" :
                     ENGLISH ? "~: fill-pointer argument ~ is larger than the length ~" :
                     FRANCAIS ? "~: L'argument ~ pour le pointeur de remplissage est plus grand que la longueur ~." :
                     ""
                    );
            }
          return fillpointer;
    }   }

LISPFUN(make_array,1,0,norest,key,7,\
        (kw(adjustable),kw(element_type),kw(initial_element),\
         kw(initial_contents),kw(fill_pointer),\
         kw(displaced_to),kw(displaced_index_offset)) )
# (MAKE-ARRAY dimensions :adjustable :element-type :initial-element
#   :initial-contents :fill-pointer :displaced-to :displaced-index-offset),
#   CLTL S. 286
  # Stackaufbau:
  #   dims, adjustable, element-type, initial-element, initial-contents,
  #   fill-pointer, displaced-to, displaced-index-offset.
  { # Dimensionen überprüfen und Rang und Total-Size berechnen:
    var uintL totalsize;
    var reg4 uintL rank = test_dims(&totalsize);
    # adjustable hat Defaultwert NIL:
    if (eq(STACK_6,unbound)) { STACK_6 = NIL; }
   {# element-type in einen Code umwandeln:
    var reg6 uintB eltype;
    if (!(eq(STACK_5,unbound)))
      { eltype = eltype_code(STACK_5); }
      else
      { # Defaultwert ist T.
        STACK_5 = S(t); eltype = Atype_T;
      }
    test_otherkeys(); # einiges überprüfen
    { var reg5 uintB flags = eltype;
      var reg7 uintL displaced_index_offset;
      var reg9 uintL fillpointer;
      if (!((eltype<=Atype_32Bit) && !(eltype==Atype_Bit))) # außer bei Byte-Vektoren
        flags |= bit(arrayflags_notbytep_bit); # notbytep-Bit setzen
      # Falls nicht displaced, Datenvektor bilden und evtl. füllen:
      if (nullp(STACK_1)) # displaced-to nicht angegeben?
        { # Datenvektor bilden:
          var reg1 object datenvektor = make_datenvektor(totalsize,eltype);
          if (!eq(STACK_3,unbound)) # und falls initial-contents angegeben:
            { datenvektor = initial_contents(datenvektor,STACK_7,rank,STACK_3); } # füllen
          # Falls displaced-to nicht angegeben ist
          # und fill-pointer nicht angegeben ist
          # und adjustable nicht angegeben ist
          # und rank=1 ist,
          # ist ein (semi-)simpler Vektor zu liefern:
          if ((rank==1) && (nullp(STACK_6)) && (nullp(STACK_2)))
            { value1 = datenvektor; mv_count=1; # Datenvektor als Ergebnis
              skipSTACK(8); return;
            }
          # Es ist ein allgemeiner Array zu liefern.
          STACK_1 = datenvektor; # datenvektor als "displaced-to" ablegen
          displaced_index_offset = 0; # mit Displacement 0
          # und ohne Displacement-Bit in den Flags
        }
        else
        { # displaced-to angegeben -> Es ist ein allgemeiner Array zu liefern.
          displaced_index_offset = test_displaced(eltype,totalsize);
          # Flags enthalten das Displacement-Bit:
          flags |= bit(arrayflags_displaced_bit)|bit(arrayflags_dispoffset_bit);
        }
      # Erzeuge einen allgemeinen Array.
      # Rang überprüfen:
      if (rank > arrayrank_limit_1)
        { pushSTACK(fixnum(rank));
          pushSTACK(TheSubr(subr_self)->name);
          fehler(
                 DEUTSCH ? "~: Der gewünschte Rang ~ ist zu groß." :
                 ENGLISH ? "~: attempted rank ~ is too large" :
                 FRANCAIS ? "~: Le rang souhaité est trop grand." :
                 ""
                );
        }
      # Flags für allocate_array zusammensetzen:
      # flags enthält schon eltype und evtl. Displacement-Bit.
      if (!nullp(STACK_6)) # adjustable angegeben?
        { flags |= bit(arrayflags_adjustable_bit)|bit(arrayflags_dispoffset_bit); }
      if (!nullp(STACK_2)) # fill-pointer angegeben?
        { if (!(rank==1)) # Rang muß 1 sein
            { pushSTACK(fixnum(rank));
              pushSTACK(S(Kfill_pointer));
              pushSTACK(TheSubr(subr_self)->name);
              fehler(
                     DEUTSCH ? "~: ~ darf bei einem Array vom Rang ~ nicht angegeben werden." :
                     ENGLISH ? "~: ~ may not be specified for an array of rank ~" :
                     FRANCAIS ? "~: ~ ne peut pas être spécifié avec une matrice de rang ~." :
                     ""
                    );
            }
          flags |= bit(arrayflags_fillp_bit);
          fillpointer = test_fillpointer(totalsize); # Fill-Pointer-Wert
        }
      # Typinfo für das zu erzeugende Objekt bestimmen:
     {var reg8 tint type;
      if (rank==1)
        { # Vektor: Typinfo aus Tabelle bestimmen
          local tint type_table[8] =
               # Tabelle für Zuordnung  ATYPE-Byte -> Vektor-Typinfo
               {      bvector_type,  # Atype_Bit         -> bvector_type
                      bvector_type,  # Atype_2Bit        -> bvector_type
                      bvector_type,  # Atype_4Bit        -> bvector_type
                      bvector_type,  # Atype_8Bit        -> bvector_type
                      bvector_type,  # Atype_16Bit       -> bvector_type
                      bvector_type,  # Atype_32Bit       -> bvector_type
                      vector_type,   # Atype_T           -> vector_type
                      string_type,   # Atype_String_Char -> string_type
                                     # restliche ATYPEs unbenutzt
               };
          type = type_table[eltype];
        }
        else
        { # allgemeiner Array
          type = array_type;
        }
      # Array allozieren:
      { var reg3 object array = allocate_array(flags,rank,type);
        TheArray(array)->totalsize = totalsize; # Total-Size eintragen
        {var reg1 uintL* dimptr = &TheArray(array)->dims[0];
         if (flags & bit(arrayflags_dispoffset_bit))
           { *dimptr++ = displaced_index_offset; } # Displaced-Index-Offset eintragen
         # Dimensionen eintragen:
         { var reg2 object dims = STACK_7;
           if (listp(dims))
             { while (consp(dims))
                 { *dimptr++ = posfixnum_to_L(Car(dims)); dims = Cdr(dims); }
             }
             else
             { *dimptr++ = posfixnum_to_L(dims); }
         }
         # evtl. Fill-Pointer eintragen:
         if (flags & bit(arrayflags_fillp_bit))
           { # fill-pointer war angegeben und /=NIL
             *dimptr++ = fillpointer;
           }
        }
        # Datenvektor eintragen:
        TheArray(array)->data = STACK_1; # displaced-to-Argument oder neuer Datenvektor
        # array als Wert:
        value1 = array; mv_count=1; skipSTACK(8);
  }}}}}

# Hilfsfunktion für die Umfüllaufgabe bei ADJUST-ARRAY:
# Füllt den Datenvektor eines Arrays teilweise mit dem Inhalt eines anderen
# Datenvektors, und zwar so, daß die Elemente zu Indextupeln, die für beide
# Arrays gültig sind, übereinstimmen.
# reshape(newvec,newdims,oldvec,olddims,offset,rank,eltype);
# > newvec: (semi-)simpler Vektor, in den zu füllen ist.
# > newdims: Dimension(en) des Arrays,
#            in dem newvec Datenvektor ist (mit Offset 0).
# > oldvec: (semi-)simpler Vektor, aus dem zu füllen ist.
# > olddims: Pointer auf die Dimensionen des Arrays,
#            in dem oldvec Datenvektor ist (mit Offset offset).
# > rank: Dimensionszahl von newdims = Dimensionenzahl von olddims.
# > eltype: Elementtyp von newvec = Elementtyp von oldvec.
  local void reshape (object newvec, object newdims, object oldvec, uintL* olddims, uintL offset, uintL rank, uintB eltype);
  # Methode: pseudo-rekursiv, mit Pseudo-Stack, der unterhalb von STACK liegt.
  typedef struct { uintL olddim; # Dimension aus olddims
                   uintL newdim; # Dimension aus newdims
                   uintL mindim; # minimale dieser Dimensionen
                   uintL subscript; # Subscript, läuft von 0 bis mindim-1
                   uintL oldindex; # Row-Major-Index in oldvec
                   uintL newindex; # Row-Major-Index in newvec
                   uintL olddelta; # Increment von oldindex bei subscript++
                   uintL newdelta; # Increment von newindex bei subscript++
                 }
          reshape_data;
  local void reshape(newvec,newdims,oldvec,olddims,offset,rank,eltype)
    var reg6 object newvec;
    var reg9 object newdims;
    var reg7 object oldvec;
    var reg8 uintL* olddims;
    var uintL offset;
    var reg5 uintL rank;
    var reg10 uintB eltype;
    { # Platz für den Pseudo-Stack reservieren:
      get_space_on_STACK(rank*sizeof(reshape_data));
      # Startpunkt:
     {var reg9 reshape_data* reshape_stack = &STACKblock_(reshape_data,-1);
      # Pseudo-Stack füllen:
      if (!(rank==0))
        { var reg1 reshape_data* ptr;
          var reg4 uintC count;
          # jeweils newdim einfüllen:
          ptr = reshape_stack;
          if (consp(newdims))
            { dotimespC(count,rank,
                { ptr->newdim = posfixnum_to_L(Car(newdims)); newdims = Cdr(newdims);
                  ptr = ptr STACKop -1;
                });
            }
            else
            { ptr->newdim = posfixnum_to_L(newdims); }
          # jeweils olddim und mindim einfüllen:
          ptr = reshape_stack;
          dotimespC(count,rank,
            { var reg2 uintL olddim;
              var reg3 uintL newdim;
              olddim = ptr->olddim = *olddims++;
              newdim = ptr->newdim;
              ptr->mindim = (olddim<newdim ? olddim : newdim);
              ptr = ptr STACKop -1;
            });
          # jeweils olddelta und newdelta einfüllen:
          { var reg2 uintL olddelta = 1;
            var reg3 uintL newdelta = 1;
            dotimespC(count,rank,
              { ptr = ptr STACKop 1;
                ptr->olddelta = olddelta;
                olddelta = mulu32_unchecked(olddelta,ptr->olddim);
                ptr->newdelta = newdelta;
                newdelta = mulu32_unchecked(newdelta,ptr->newdim);
              });
          }
        }
      # Los geht's mit der Pseudo-Rekursion:
      { var reg1 reshape_data* ptr = reshape_stack;
        var reg2 uintL oldindex = offset; # Row-Major-Index in oldvec
        var reg3 uintL newindex = 0; # Row-Major-Index in newvec
        var reg4 uintL depth = rank;
        entry: # Rekursionseinstieg
          if (depth==0)
            { # Element kopieren:
              # (setf (aref newvec newindex) (aref oldvec oldindex))
              # so kopieren, daß keine GC ausgelöst werden kann:
              if (eltype == Atype_32Bit)
                { ((uint32*)&TheSbvector(TheArray(newvec)->data)->data[0])[newindex]
                    = ((uint32*)&TheSbvector(TheArray(oldvec)->data)->data[0])[oldindex];
                }
                else
                { datenvektor_store(newvec,newindex,datenvektor_aref(oldvec,oldindex)); }
            }
            else
            { # Schleife über alle gemeinsamen Indizes:
              ptr->oldindex = oldindex; ptr->newindex = newindex;
              depth--;
              dotimesL(ptr->subscript,ptr->mindim,
                { oldindex = ptr->oldindex; newindex = ptr->newindex;
                  ptr = ptr STACKop -1;
                  goto entry;
                  reentry:
                  ptr = ptr STACKop 1;
                  ptr->oldindex += ptr->olddelta;
                  ptr->newindex += ptr->newdelta;
                });
              depth++;
            }
          # Rekursionsaustritt:
          if (depth<rank) goto reentry;
    }}}

LISPFUN(adjust_array,2,0,norest,key,6,\
        (kw(element_type),kw(initial_element),\
         kw(initial_contents),kw(fill_pointer),\
         kw(displaced_to),kw(displaced_index_offset)) )
# (ADJUST-ARRAY array dimensions :element-type :initial-element
#   :initial-contents :fill-pointer :displaced-to :displaced-index-offset),
#   CLTL S. 297
  { # array überprüfen:
    { var reg1 object array = STACK_7;
      switch (typecode(array))
        { case_array1:
          case_ostring: case_obvector: case_ovector:
            if (TheArray(array)->flags & bit(arrayflags_adjustable_bit))
              break; # adjustierbar -> OK
          case_sstring: case_sbvector: case_svector:
            # nicht adjustierbarer Array
            pushSTACK(array);
            pushSTACK(TheSubr(subr_self)->name);
            fehler(
                   DEUTSCH ? "~: Array ~ ist nicht adjustierbar." :
                   ENGLISH ? "~: array ~ is not adjustable" :
                   FRANCAIS ? "~: La matrice ~ n'est pas ajustable." :
                   ""
                  );
          default:
            # kein Array
            fehler_array(array);
        }
      STACK_7 = STACK_6; STACK_6 = array; # Stack etwas umordnen
    }
   # Stackaufbau:
   #   dims, array, element-type, initial-element, initial-contents,
   #   fill-pointer, displaced-to, displaced-index-offset.
   {# Dimensionen überprüfen und Rang und Total-Size berechnen:
    var uintL totalsize;
    var reg4 uintL rank = test_dims(&totalsize);
    # Rang überprüfen, muß = (array-rank array) sein:
    {var reg1 uintL oldrank = (uintL)(TheArray(STACK_6)->rank);
     if (!(rank==oldrank))
       { pushSTACK(STACK_7); # dims
         pushSTACK(STACK_(6+1)); # array
         pushSTACK(fixnum(oldrank));
         pushSTACK(TheSubr(subr_self)->name);
         fehler(
                DEUTSCH ? "~: Dimensionszahl ~ des Arrays ~ kann nicht geändert werden: ~" :
                ENGLISH ? "~: rank ~ of array ~ cannot be altered: ~" :
                FRANCAIS ? "~: Le rang ~ de la matrice ~ ne peut pas être modifié." :
                ""
               );
    }  }
    {# element-type in einen Code umwandeln und überprüfen:
     var reg6 uintB eltype;
     if (!(eq(STACK_5,unbound)))
       { eltype = eltype_code(STACK_5);
         # mit dem Elementtyp des Array-Arguments vergleichen:
         if (!(eltype == (TheArray(STACK_6)->flags & arrayflags_atype_mask)))
           { pushSTACK(STACK_5); # element-type
             pushSTACK(STACK_(6+1)); # array
             pushSTACK(TheSubr(subr_self)->name);
             fehler(
                    DEUTSCH ? "~: Array ~ hat nicht Elementtyp ~" :
                    ENGLISH ? "~: array ~ has not element-type ~" :
                    FRANCAIS ? "~: La matrice ~ n'as pas ~ comme type d'élément." :
                    ""
                   );
       }   }
       else
       { # Defaultwert ist der Elementtyp des Array-Arguments.
         eltype = (TheArray(STACK_6)->flags & arrayflags_atype_mask);
         STACK_5 = array_element_type(STACK_6);
       }
     test_otherkeys(); # einiges überprüfen
     { var reg5 uintB flags = TheArray(STACK_6)->flags;
       # Die Flags enthalten genau eltype als Atype (mit evtl.
       # arrayflags_notbytep_bit) und arrayflags_adjustable_bit und daher auch
       # arrayflags_dispoffset_bit und vielleicht auch arrayflags_fillp_bit
       # (diese werden nicht verändert) und vielleicht auch
       # arrayflags_displaced_bit (dieses kann geändert werden).
       var reg7 uintL displaced_index_offset;
       var reg9 uintL fillpointer;
       # Falls nicht displaced, Datenvektor bilden und evtl. füllen:
       if (nullp(STACK_1)) # displaced-to nicht angegeben?
         { # Datenvektor bilden:
           var reg3 object datenvektor = make_datenvektor(totalsize,eltype);
           if (!eq(STACK_3,unbound)) # und falls initial-contents angegeben:
             { # mit dem initial-contents-Argument füllen:
               datenvektor = initial_contents(datenvektor,STACK_7,rank,STACK_3);
             }
             else
             { # mit dem ursprünglichen Inhalt von array füllen:
               var reg1 object oldarray = STACK_6; # array
               var uintL oldoffset = 0;
               var reg2 object oldvec = array1_displace_check(oldarray,TheArray(oldarray)->totalsize,&oldoffset);
               # oldvec ist der Datenvektor, mit Displaced-Offset oldoffset.
               var reg2 uintL* olddimptr = &TheArray(oldarray)->dims[1];
               # Ab olddimptr kommen die alten Dimensionen von array
               # (beachte: Da arrayflags_adjustable_bit gesetzt ist, ist auch
               # arrayflags_dispoffset_bit gesetzt, also ist
               # TheArray(array)->data[0] für den Displaced-Offset reserviert.)
               reshape(datenvektor,STACK_7,oldvec,olddimptr,oldoffset,rank,eltype);
             }
           STACK_1 = datenvektor; # datenvektor als "displaced-to" ablegen
           displaced_index_offset = 0; # mit Displacement 0
           flags &= ~bit(arrayflags_displaced_bit); # und ohne Displacement-Bit in den Flags
         }
         else
         { # displaced-to angegeben.
           displaced_index_offset = test_displaced(eltype,totalsize);
           # Test auf entstehenden Zyklus:
           { var reg2 object array = STACK_6; # Array, der displaced werden soll
             var reg1 object to_array = STACK_1; # Array, auf den displaced werden soll
             # Teste, ob array in der Datenvektorenkette von to_array vorkommt:
             loop
               { # Falls array = to_array, ist ein Zyklus da.
                 if (eq(array,to_array))
                   { pushSTACK(array);
                     pushSTACK(TheSubr(subr_self)->name);
                     fehler(
                            DEUTSCH ? "~: Array ~ kann nicht auf sich selbst displaced werden." :
                            ENGLISH ? "~: cannot displace array ~ to itself" :
                            FRANCAIS ? "~: La matrice ~ ne peut pas être déplacée («displaced») vers elle-même." :
                            ""
                           );
                   }
                 # Falls to_array simple ist (also nicht displaced),
                 # liegt kein Zyklus vor.
                 if_simplep(to_array, break; , ; );
                 # Displaced-Kette von to_array weiterverfolgen:
                 to_array = TheArray(to_array)->data;
           }   }
           # Flags enthalten das Displacement-Bit:
           flags |= bit(arrayflags_displaced_bit);
         }
       # Flags sind nun korrekt.
       # Modifiziere den gegebenen Array.
       if (!nullp(STACK_2)) # fill-pointer angegeben?
         { # array muß Fill-Pointer haben:
           if (!(TheArray(STACK_6)->flags & bit(arrayflags_fillp_bit)))
             { pushSTACK(STACK_6);
               pushSTACK(TheSubr(subr_self)->name);
               fehler(
                      DEUTSCH ? "~: Array ~ hat keinen Fill-Pointer." :
                      ENGLISH ? "~: array ~ has no fill-pointer" :
                      FRANCAIS ? "~: La matrice ~ n'a pas de pointeur de remplissage." :
                      ""
                     );
             }
           fillpointer = test_fillpointer(totalsize); # Fill-Pointer-Wert
         }
         else
         { # Hat array einen Fill-Pointer, so muß er <= neue Total-Size sein:
           var reg1 object array = STACK_6;
           if (TheArray(array)->flags & bit(arrayflags_fillp_bit))
             if (!(TheArray(array)->dims[2] <= totalsize))
               # dims[0] = displaced-offset, dims[1] = Länge, dims[2] = Fill-Pointer
               { pushSTACK(fixnum(totalsize));
                 pushSTACK(fixnum(TheArray(array)->dims[2]));
                 pushSTACK(array);
                 pushSTACK(TheSubr(subr_self)->name);
                 fehler(
                        DEUTSCH ? "~: Array ~ hat einen Fill-Pointer ~ > gewünschte Länge ~." :
                        ENGLISH ? "~: the fill-pointer of array ~ is ~, greater than ~" :
                        FRANCAIS ? "~: La matrice ~ possède un pointeur de remplissage ~ supérieur à la longueur souhaitée ~." :
                        ""
                       );
         }     }
       # Array modifizieren:
       { var reg3 object array = STACK_6;
         set_break_sem_1(); # Unterbrechungen verbieten
         TheArray(array)->flags = flags; # neue Flags eintragen
         TheArray(array)->totalsize = totalsize; # neue Total-Size eintragen
         {var reg1 uintL* dimptr = &TheArray(array)->dims[0];
          *dimptr++ = displaced_index_offset; # Displaced-Index-Offset eintragen
          # neue Dimensionen eintragen:
          { var reg2 object dims = STACK_7;
            if (listp(dims))
              { while (consp(dims))
                  { *dimptr++ = posfixnum_to_L(Car(dims)); dims = Cdr(dims); }
              }
              else
              { *dimptr++ = posfixnum_to_L(dims); }
          }
          # evtl. Fill-Pointer eintragen bzw. korrigieren:
          if (flags & bit(arrayflags_fillp_bit)) # Array mit Fill-Pointer?
            if (!nullp(STACK_2)) # und fill-pointer angegeben?
              { # fill-pointer war angegeben und /=NIL
                *dimptr = fillpointer;
              }
         }
         # Datenvektor eintragen:
         TheArray(array)->data = STACK_1; # displaced-to-Argument oder neuer Datenvektor
         clr_break_sem_1(); # Unterbrechungen wieder zulassen
         # array als Wert:
         value1 = array; mv_count=1; skipSTACK(8);
  }}}} }


# Funktionen, die Vektoren zu Sequences machen:

LISPFUNN(vector_init,1)
# #'(lambda (seq) 0)
  { skipSTACK(1);
    value1 = Fixnum_0; mv_count=1; # 0 als Wert
  }

LISPFUNN(vector_upd,2)
# #'(lambda (seq pointer) (1+ pointer))
  { if (mposfixnump(STACK_0))
      { var reg1 object newpointer = fixnum_inc(STACK_0,1); # Fixnum >=0 um 1 erhöhen
        if (posfixnump(newpointer))
          { # ist ein Fixnum >=0 geblieben
            skipSTACK(2);
            value1 = newpointer; mv_count=1; # newpointer als Wert
            return;
      }   }
    # Pointer ist vor oder nach dem Erhöhen kein Fixnum >=0
    funcall(L(einsplus),1); # (1+ pointer) als Wert
    skipSTACK(1);
  }

LISPFUNN(vector_endtest,2)
# #'(lambda (seq pointer) (= pointer (vector-length seq)))
  { var reg1 object seq = STACK_1;
    if (!vectorp(seq)) { fehler_vector(seq); }
    if (eq(fixnum(vector_length(seq)),STACK_0))
      { value1 = T; mv_count=1; skipSTACK(2); } # 1 Wert T
      else
      { value1 = NIL; mv_count=1; skipSTACK(2); } # 1 Wert NIL
  }

LISPFUNN(vector_fe_init,1)
# #'(lambda (seq) (1- (vector-length seq)))
  { var reg1 object seq = popSTACK();
    if (!vectorp(seq)) { fehler_vector(seq); }
   {var reg2 uintL len = vector_length(seq);
    # len = (vector-length seq).
    # Als Fixnum, und um 1 erniedrigen:
    value1 = (len==0 ? Fixnum_minus1 : fixnum(len-1));
    mv_count=1;
  }}

LISPFUNN(vector_fe_upd,2)
# #'(lambda (seq pointer) (1- pointer))
  { if (mposfixnump(STACK_0))
      { var reg1 object pointer = popSTACK();
        value1 = (eq(pointer,Fixnum_0)
                  ? Fixnum_minus1
                  : fixnum_inc(pointer,-1) # Fixnum >0 um 1 erniedrigen
                 );
        mv_count=1;
      }
      else
      { # Pointer ist vor oder nach dem Erniedrigen kein Fixnum >=0
        funcall(L(einsminus),1); # (1- pointer) als Wert
      }
    skipSTACK(1);
  }

LISPFUNN(vector_fe_endtest,2)
# #'(lambda (seq pointer) (minusp pointer))
  { value1 = (mpositivep(STACK_0) ? NIL : T); # Vorzeichen von pointer abfragen
    mv_count=1;
    skipSTACK(2);
  }

LISPFUNN(vector_length,1)
  { var reg1 object seq = popSTACK();
    if (!vectorp(seq)) { fehler_vector(seq); }
    value1 = fixnum(vector_length(seq)); mv_count=1;
  }

LISPFUNN(vector_init_start,2)
# #'(lambda (seq index)
#     (if (<= 0 index (vector-length seq))
#       index
#       (error "Unzulässiger :START - Index : ~S" index)
#   ) )
  { var reg1 object seq = STACK_1;
    if (!vectorp(seq)) { fehler_vector(seq); }
   {var reg2 uintL len = vector_length(seq);
    # index sollte ein Fixnum zwischen 0 und len (inclusive) sein:
    if (mposfixnump(STACK_0) && (posfixnum_to_L(STACK_0)<=len))
      { value1 = STACK_0; mv_count=1; skipSTACK(2); } # index als Wert
      else
      { # Stackaufbau: seq, index.
        fehler(
               DEUTSCH ? "Unzulässiger START - Index ~ für ~" :
               ENGLISH ? "Illegal START index ~ for ~" :
               FRANCAIS ? "Index START ~ invalide pour ~." :
               ""
              );
      }
  }}

LISPFUNN(vector_fe_init_end,2)
# #'(lambda (seq index)
#     (if (<= 0 index (vector-length seq))
#       (1- index)
#       (error "Unzulässiger :END - Index : ~S" index)
#   ) )
  { var reg1 object seq = STACK_1;
    if (!vectorp(seq)) { fehler_vector(seq); }
   {var reg2 uintL len = vector_length(seq);
    # index sollte ein Fixnum zwischen 0 und len (inclusive) sein:
    if (mposfixnump(STACK_0) && (posfixnum_to_L(STACK_0)<=len))
      { var reg2 object index = STACK_0;
        skipSTACK(2);
        value1 = (eq(index,Fixnum_0)
                  ? Fixnum_minus1
                  : fixnum_inc(index,-1) # Fixnum >0 um 1 erniedrigen
                 );
        mv_count=1;
      }
      else
      { # Stackaufbau: seq, index.
        fehler(
               DEUTSCH ? "Unzulässiger END - Index ~ für ~" :
               ENGLISH ? "Illegal END index ~ for ~" :
               FRANCAIS ? "Index END ~ invalide pour ~." :
               ""
              );
      }
  }}

LISPFUNN(make_bit_vector,1)
# (SYS::MAKE-BIT-VECTOR size) liefert einen Bit-Vector mit size Bits.
  { if (!mposfixnump(STACK_0))
      { # STACK_0 = size
        pushSTACK(TheSubr(subr_self)->name);
        fehler(
               DEUTSCH ? "~: Als Bit-Vektoren-Länge ist ~ ungeeignet." :
               ENGLISH ? "~: invalid bit-vector length ~" :
               FRANCAIS ? "~: ~ n'est pas convenable comme longeur de vecteur bit." :
               ""
              );
      }
   {var reg1 uintL size = posfixnum_to_L(popSTACK()); # Länge
    value1 = allocate_bit_vector(size); # euen Bit-Vektor beschaffen
    mv_count=1;
  }}

