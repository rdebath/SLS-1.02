# Prädikate für Gleichheit und Typtests in CLISP
# Bruno Haible 13.12.1992

#include "lispbibl.c"

# UP: testet auf Atomgleichheit EQL
# eql(obj1,obj2)
# > obj1,obj2: Lisp-Objekte
# < ergebnis: TRUE, falls Objekte gleich
  global boolean eql (object obj1, object obj2);
  global boolean eql(obj1,obj2)
    var reg1 object obj1;
    var reg2 object obj2;
    { start:
      if (eq(obj1,obj2)) { return TRUE; } # (EQ x y) ==> (EQL x y)
      # sonst ist EQL-Gleichheit nur möglich, wenn beides Zahlen sind:
      if (!(numberp(obj1) && numberp(obj2))) { return FALSE; }
      # und der Typ von beiden muß übereinstimmen:
     {var reg3 tint type1 = typecode(obj1);
      if (!(type1 == typecode(obj2))) { return FALSE; }
      switch (type1)
        { case_fixnum: # Fixnums
            goto no; # hätten schon EQ sein müssen
          case_bignum: # Bignums
            # Längen vergleichen:
            { var reg6 uintC length1 = TheBignum(obj1)->length;
              if (!(length1 == TheBignum(obj2)->length)) goto no;
            # Ziffern vergleichen:
             {var reg4 uintD* ptr1 = &TheBignum(obj1)->data[0];
              var reg5 uintD* ptr2 = &TheBignum(obj2)->data[0];
              dotimespC(length1,length1, { if (!(*ptr1++ == *ptr2++)) goto no; });
            }}
            return TRUE;
          case_ratio: # Ratio
            # Zähler und Nenner müssen übereinstimmen:
            # (and (eql (numerator obj1) (numerator obj2))
            #      (eql (denominator obj1) (denominator obj2))
            # )
            if (!eql(TheRatio(obj1)->rt_num,TheRatio(obj2)->rt_num)) goto no;
            # return eql(TheRatio(obj1)->rt_den,TheRatio(obj2)->rt_den);
            obj1 = TheRatio(obj1)->rt_den; obj2 = TheRatio(obj2)->rt_den;
            goto start;
          case_complex: # Complex
            # Real- und Imaginärteil müssen übereinstimmen:
            # (and (eql (realpart obj1) (realpart obj2))
            #      (eql (imagpart obj1) (imagpart obj2))
            # )
            if (!eql(TheComplex(obj1)->c_real,TheComplex(obj2)->c_real)) goto no;
            # return eql(TheComplex(obj1)->c_imag,TheComplex(obj2)->c_imag);
            obj1 = TheComplex(obj1)->c_imag; obj2 = TheComplex(obj2)->c_imag;
            goto start;
          case_sfloat: # Short-Floats
            goto no; # hätten schon EQ sein müssen
          case_ffloat: # Single-Floats
            #ifndef WIDE
            if (TheFfloat(obj1)->float_value == TheFfloat(obj2)->float_value)
              return TRUE;
              else
            #endif
              goto no;
          case_dfloat: # Double-Floats
            if ((TheDfloat(obj1)->float_value.semhi == TheDfloat(obj2)->float_value.semhi)
                && (TheDfloat(obj1)->float_value.mlo == TheDfloat(obj2)->float_value.mlo)
               )
              return TRUE;
              else
              goto no;
          case_lfloat: # Long-Floats
            # Längen vergleichen:
            { var reg6 uintC len1 = TheLfloat(obj1)->len;
              if (!(len1 == TheLfloat(obj2)->len)) goto no;
            # Exponenten vergleichen:
              if (!(TheLfloat(obj1)->expo == TheLfloat(obj2)->expo)) goto no;
            # Ziffern vergleichen:
             {var reg4 uintD* ptr1 = &TheLfloat(obj1)->data[0];
              var reg5 uintD* ptr2 = &TheLfloat(obj2)->data[0];
              dotimespC(len1,len1, { if (!(*ptr1++ == *ptr2++)) goto no; });
            }}
            return TRUE;
          default:
          no: return FALSE;
        }
    }}

# UP: testet auf Gleichheit EQUAL
# equal(obj1,obj2)
# > obj1,obj2: Lisp-Objekte
# < ergebnis: TRUE, falls Objekte gleich
  global boolean equal (object obj1, object obj2);
  global boolean equal(obj1,obj2)
    var reg2 object obj1;
    var reg3 object obj2;
    { start:
      if (eql(obj1,obj2)) { return TRUE; } # (EQL x y) ==> (EQUAL x y)
      # sonst ist EQUAL-Gleichheit nur möglich, wenn beides strukturierte
      # Typen sind. Typen müssen (bis auf notsimple_bit) übereinstimmen:
      switch (typecode(obj1))
        { case_cons: # Conses rekursiv vergleichen:
            if (!consp(obj2)) { return FALSE; }
            # CAR und CDR müssen übereinstimmen:
            # (and (equal (car obj1) (car obj2)) (equal (cdr obj1) (cdr obj2)))
            check_SP();
            if (!equal(Car(obj1),Car(obj2))) goto no;
            # return equal(Cdr(obj1),Cdr(obj2));
            obj1 = Cdr(obj1); obj2 = Cdr(obj2);
            goto start;
          case_obvector: # Byte-Vektoren
            if (!((TheArray(obj1)->flags & arrayflags_atype_mask) == Atype_Bit))
              { return FALSE; } # hätten schon EQL sein müssen
          case_sbvector: # Bit-Vektoren elementweise vergleichen:
            if (!bit_vector_p(obj2)) { return FALSE; }
            { # Längen vergleichen:
              var reg4 uintL len1 = vector_length(obj1);
              if (!(len1 == vector_length(obj2))) goto no;
              # Inhalt vergleichen:
             {var uintL index1 = 0;
              var uintL index2 = 0;
              var reg5 object sbv1 = array_displace_check(obj1,len1,&index1);
              var reg6 object sbv2 = array_displace_check(obj2,len1,&index2);
              # sbvi ist der Datenvektor, indexi der Index in den Datenvektor
              # zu obji (i=1,2).
              return bit_compare(sbv1,index1,sbv2,index2,len1);
            }}
          case_string: # Strings elementweise vergleichen:
            if (!stringp(obj2)) { return FALSE; }
            { # Längen vergleichen:
              var reg6 uintL len1 = vector_length(obj1);
              if (!(len1 == vector_length(obj2))) goto no;
              # Inhalt vergleichen:
              if (!(len1==0))
                { var uintL index1 = 0;
                  var uintL index2 = 0;
                  var reg7 object ss1 = array_displace_check(obj1,len1,&index1);
                  var reg8 object ss2 = array_displace_check(obj2,len1,&index2);
                  # ssi ist der Datenvektor, indexi der Index in den Datenvektor
                  # zu obji (i=1,2).
                  var reg4 uintB* ptr1 = &TheSstring(ss1)->data[0];
                  var reg5 uintB* ptr2 = &TheSstring(ss2)->data[0];
                  dotimespL(len1,len1,
                    { if (!(*ptr1++ == *ptr2++)) goto no; }
                    );
                }
              return TRUE;
            }
          default:
            # Pathnames komponentenweise vergleichen:
            if (pathnamep(obj1) && pathnamep(obj2))
              { var reg4 object* ptr1 = &TheRecord(obj1)->recdata[0];
                var reg5 object* ptr2 = &TheRecord(obj2)->recdata[0];
                var reg6 uintC count;
                check_SP();
               #if !(defined(PATHNAME_AMIGAOS) || defined(PATHNAME_OS2))
                dotimespC(count,pathname_length,
                  { if (!equal(*ptr1++,*ptr2++)) goto no; }
                  );
               #else # defined(PATHNAME_AMIGAOS) || defined(PATHNAME_OS2)
                # Pathname-Komponenten bestehen aus Conses, Simple-Strings
                # und Symbolen. Simple-Strings case-insensitive vergleichen:
                dotimespC(count,pathname_length,
                  { if (!equalp(*ptr1++,*ptr2++)) goto no; } # (löst keine GC aus!)
                  );
               #endif
                return TRUE;
              }
            # Sonst gelten obj1 und obj2 als verschieden.
            else
              { no: return FALSE; }
    }   }

# UP: testet auf laschere Gleichheit EQUALP
# equalp(obj1,obj2)
# > obj1,obj2: Lisp-Objekte
# < ergebnis: TRUE, falls Objekte gleich
# kann GC auslösen
  global boolean equalp (object obj1, object obj2);
  global boolean equalp(obj1,obj2)
    var reg1 object obj1;
    var reg2 object obj2;
    { start:
      if (eq(obj1,obj2)) { return TRUE; } # (EQ x y) ==> (EQUALP x y)
      # Fallunterscheidung nach dem Typ von obj1:
      if (consp(obj1))
        { if (!consp(obj2)) goto no;
          # Conses rekursiv vergleichen:
          # CAR und CDR müssen übereinstimmen:
          # (and (equalp (car obj1) (car obj2)) (equalp (cdr obj1) (cdr obj2)))
          check_SP(); check_STACK();
          pushSTACK(Cdr(obj2)); pushSTACK(Cdr(obj1));
          if (!equalp(Car(obj1),Car(obj2))) goto no2;
          # return equalp(Cdr(obj1),Cdr(obj2));
          obj1 = popSTACK(); obj2 = popSTACK();
          goto start;
        }
      elif (symbolp(obj1)) # Symbol ?
        { goto no; } # ja -> hätte schon EQ zu obj2 sein müssen
      elif (numberp(obj1))
        { if (!numberp(obj2)) goto no;
          # Zahlen mit = vergleichen
          return number_gleich(obj1,obj2);
        }
      else
        { # Hilfvariablen beim Vergleich von Vektoren:
          var reg5 uintL len1; # Länge des zu vergleichenden Abschnitts (>0)
          var reg7 object dv1; # Datenvektor von obj1
          var reg8 object dv2; # Datenvektor von obj2
          var uintL index1; # Start-Index in den Datenvektor von obj1
          var uintL index2; # Start-Index in den Datenvektor von obj1
          # Weiter mit der Fallunterscheidung nach dem Typ von obj1:
          switch (typecode(obj1))
            { case_machine: # Maschinenpointer
                goto no; # hätte schon EQ sein müssen
              case_bvector: # Bit/Byte-Vektor
                if (!((typecode(obj2) & ~bit(notsimple_bit_t)) == sbvector_type))
                  goto no;
                # Byte-Vektoren wie die Bit-Vektoren bei EQUAL vergleichen:
                # Längen vergleichen:
                len1 = vector_length(obj1);
                if (!(len1 == vector_length(obj2))) goto no;
                # Inhalt vergleichen:
                index1 = 0; index2 = 0;
                dv1 = array_displace_check(obj1,len1,&index1);
                dv2 = array_displace_check(obj2,len1,&index2);
                # dvi ist der Datenvektor, indexi der Index in den Datenvektor
                # zu obji (i=1,2).
                # Beide Datenvektor-Typcodes vergleichen:
                { var reg3 tint dvtype1 = typecode(dv1);
                  if (!(dvtype1 == typecode(dv2))) goto no;
                  if (dvtype1 == bvector_type)
                    { compare_sbyvector: # Byte-Vektoren vergleichen
                     {var reg1 uintB atype1 = (TheArray(dv1)->flags /* & arrayflags_atype_mask */ );
                      if (!(atype1 == (TheArray(dv2)->flags /* & arrayflags_atype_mask */ ))) goto no;
                      index1 = index1 << atype1; index2 = index2 << atype1;
                      len1 = len1 << atype1;
                      dv1 = TheArray(dv1)->data; dv2 = TheArray(dv2)->data;
                }   }}
                compare_sbvector:
                return bit_compare(dv1,index1,dv2,index2,len1);
              case_string: # String
                if (!stringp(obj2)) goto no;
                # Strings wie bei EQUAL, jedoch case-insensitive, vergleichen:
                # Längen vergleichen:
                len1 = vector_length(obj1);
                if (!(len1 == vector_length(obj2))) goto no;
                # Inhalt vergleichen:
                if (!(len1==0))
                  { index1 = 0; index2 = 0;
                    dv1 = array_displace_check(obj1,len1,&index1);
                    dv2 = array_displace_check(obj2,len1,&index2);
                    # dvi ist der Datenvektor, indexi der Index in den Datenvektor
                    # zu obji (i=1,2).
                    compare_sstring:
                    # Vergleich wie  in CHARSTRG.D bei string_eqcomp_ci:
                    {var reg3 uintB* ptr1 = &TheSstring(dv1)->data[0];
                     var reg4 uintB* ptr2 = &TheSstring(dv2)->data[0];
                     dotimespL(len1,len1,
                       { if (!(up_case(*ptr1++) == up_case(*ptr2++))) goto no; }
                       );
                  } }
                return TRUE;
              case_vector: # (VECTOR T)
                if (!general_vector_p(obj2)) goto no;
                # obj1, obj2 beide vom Typ (VECTOR T).
                # Sie sind genau dann EQUALP, wenn ihre Längen gleich
                # und ihre Komponenten jeweils EQUALP sind.
                # Längen vergleichen:
                len1 = vector_length(obj1);
                if (!(len1 == vector_length(obj2))) goto no;
                # Inhalt vergleichen:
                if (!(len1==0))
                  { index1 = 0; index2 = 0;
                    dv1 = array_displace_check(obj1,len1,&index1);
                    dv2 = array_displace_check(obj2,len1,&index2);
                    # svi ist der Datenvektor, indexi der Index in den Datenvektor
                    # zu obji (i=1,2).
                    compare_svector:
                    check_SP(); check_STACK();
                    pushSTACK(dv1); pushSTACK(dv2);
                    dotimespL(len1,len1,
                      { if (!equalp(TheSvector(STACK_1)->data[index1], # Element von dv1
                                    TheSvector(STACK_0)->data[index2]  # Element von dv2
                           )       )
                          goto no2;
                        index1++; index2++;
                      });
                    skipSTACK(2);
                  }
                return TRUE;
              case_array1: # Array vom Rang /=1
                if (!array1p(obj2)) goto no;
                # obj1 und obj2 sind Arrays vom Rang /=1.
                # Ihr Rang und ihre Dimensionen müssen übereinstimmen, und
                # die Elemente werden dann wie bei Vektoren verglichen.
                { # Ränge vergleichen:
                  var reg6 uintC rank1 = TheArray(obj1)->rank;
                  if (!(rank1 == TheArray(obj2)->rank)) goto no;
                  # Dimensionen vergleichen:
                  { var reg3 uintL* dimptr1 = &TheArray(obj1)->dims[0];
                    if (TheArray(obj1)->flags & bit(arrayflags_dispoffset_bit))
                      dimptr1++;
                   {var reg4 uintL* dimptr2 = &TheArray(obj2)->dims[0];
                    if (TheArray(obj2)->flags & bit(arrayflags_dispoffset_bit))
                      dimptr2++;
                    dotimesC(rank1,rank1,
                      { if (!(*dimptr1++ == *dimptr2++)) goto no; }
                      );
                } }}
                # Inhalt vergleichen:
                len1 = TheArray(obj1)->totalsize;
                # muß als Produkt der Dimensionen auch = TheArray(obj2)->totalsize sein.
                index1 = 0; index2 = 0;
                dv1 = array1_displace_check(obj1,len1,&index1);
                dv2 = array1_displace_check(obj2,len1,&index2);
                # dvi ist der Datenvektor, indexi der Index in den Datenvektor
                # zu obji (i=1,2).
                # Beide Datenvektor-Typcodes vergleichen:
                { var reg3 tint dvtype1 = typecode(dv1);
                  if (!(dvtype1 == typecode(dv2))) goto no;
                  # Einzelelemente vergleichen:
                  if (len1==0) return TRUE;
                  switch (dvtype1)
                    { case_svector: goto compare_svector;
                      case_sbvector: goto compare_sbvector;
                      case_obvector: goto compare_sbyvector;
                      case_sstring: goto compare_sstring;
                      default: NOTREACHED
                }   }
              case_record: # Record
                # obj2 muß vom selben Typ wie obj1, also ein Record, sein
                # und in rectype und recflags und reclength mit obj1 überein-
                # stimmen, und alle Komponenten müssen EQUALP sein.
                if (!(typecode(obj1) == typecode(obj2))) goto no;
                # obj1 und obj2 beide Records.
                { var reg4 uintC len;
                  if (!(TheRecord(obj1)->recflags == TheRecord(obj2)->recflags)) goto no;
                  if (!(TheRecord(obj1)->rectype == TheRecord(obj2)->rectype)) goto no;
                  if (!((len = TheRecord(obj1)->reclength) == TheRecord(obj2)->reclength)) goto no;
                  # rekursiv die Elemente vergleichen (auch bei PATHNAMEs):
                  check_SP(); check_STACK();
                  pushSTACK(obj1); pushSTACK(obj2);
                 {var reg3 uintL index = 0;
                  dotimespC(len,len,
                    { if (!equalp(TheRecord(STACK_1)->recdata[index], # Element von obj1
                                  TheRecord(STACK_0)->recdata[index]  # Element von obj2
                         )       )
                        goto no2;
                      index++;
                    });
                  skipSTACK(2);
                }}
                return TRUE;
              case_char:
                # Character
                if (!charp(obj2)) goto no;
                # obj1, obj2 beide Characters.
                # Wie mit CHAR-EQUAL vergleichen: Bits und Font ignorieren,
                # in Großbuchstaben umwandeln und dann vergleichen.
                if (up_case(char_code(obj1)) == up_case(char_code(obj2)))
                  return TRUE;
                  else
                  goto no;
              case_subr: # SUBR
                goto no; # hätte schon EQ sein müssen
              case_fsubr: # FSUBR
                goto no; # hätte schon EQ sein müssen
              case_system: # SYSTEM, Read-Label, FRAME-Pointer
                goto no; # hätte schon EQ sein müssen
              default: NOTREACHED
        }   }
      no2: skipSTACK(2);
      no: return FALSE;
    }

LISPFUNN(eq,2)
# (EQ obj1 obj2), CLTL S. 77
  { var reg2 object obj2 = popSTACK();
    var reg1 object obj1 = popSTACK();
    value1 = (eq(obj1,obj2) ? T : NIL); mv_count=1;
  }

LISPFUNN(eql,2)
# (EQL obj1 obj2), CLTL S. 78
  { var reg2 object obj2 = popSTACK();
    var reg1 object obj1 = popSTACK();
    value1 = (eql(obj1,obj2) ? T : NIL); mv_count=1;
  }

LISPFUNN(equal,2)
# (EQUAL obj1 obj2), CLTL S. 80
  { var reg2 object obj2 = popSTACK();
    var reg1 object obj1 = popSTACK();
    value1 = (equal(obj1,obj2) ? T : NIL); mv_count=1;
  }

LISPFUNN(equalp,2)
# (EQUALP obj1 obj2), CLTL S. 81
  { var reg2 object obj2 = popSTACK();
    var reg1 object obj1 = popSTACK();
    value1 = (equalp(obj1,obj2) ? T : NIL); mv_count=1;
  }

LISPFUNN(consp,1)
# (CONSP object), CLTL S. 74
  { value1 = (mconsp(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(atom,1)
# (ATOM object), CLTL S. 73
  { value1 = (matomp(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(symbolp,1)
# (SYMBOLP object), CLTL S. 73
  { value1 = (msymbolp(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(stringp,1)
# (STRINGP object), CLTL S. 75
  { value1 = (mstringp(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(numberp,1)
# (NUMBERP object), CLTL S. 74
  { value1 = (mnumberp(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(compiled_function_p,1)
# (COMPILED-FUNCTION-P object), CLTL S. 76
  { var reg1 object arg = popSTACK();
    # Test auf SUBR oder FSUBR oder compilierte Closure:
    value1 = (subrp(arg) || fsubrp(arg) || cclosurep(arg) ? T : NIL); mv_count=1;
  }

LISPFUNN(null,1)
# (NULL object), CLTL S. 73
  { value1 = (nullp(popSTACK()) ? T : NIL); mv_count=1; }

LISPFUNN(not,1)
# (NOT object), CLTL S. 82
  { value1 = (nullp(popSTACK()) ? T : NIL); mv_count=1; }

LISPFUNN(closurep,1)
# (SYS::CLOSUREP object)
  { value1 = (mclosurep(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(listp,1)
# (LISTP object), CLTL S. 74
  { var reg1 object arg = popSTACK();
    value1 = (listp(arg) ? T : NIL); mv_count=1;
  }

LISPFUNN(integerp,1)
# (INTEGERP object), CLTL S. 74
  { value1 = (mintegerp(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(fixnump,1)
# (SYS::FIXNUMP object)
  { value1 = (mfixnump(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(rationalp,1)
# (RATIONALP object), CLTL S. 74
  { if_rationalp(popSTACK(), value1 = T; , value1 = NIL; ); mv_count=1; }

LISPFUNN(floatp,1)
# (FLOATP object), CLTL S. 75
  { value1 = (mfloatp(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(realp,1)
# (REALP object), CLTL2 S. 101
  { if_realp(popSTACK(), value1 = T; , value1 = NIL; ); mv_count=1; }

LISPFUNN(short_float_p,1)
# (SYS::SHORT-FLOAT-P object)
  { value1 = (m_short_float_p(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(single_float_p,1)
# (SYS::SINGLE-FLOAT-P object)
  { value1 = (m_single_float_p(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(double_float_p,1)
# (SYS::DOUBLE-FLOAT-P object)
  { value1 = (m_double_float_p(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(long_float_p,1)
# (SYS::LONG-FLOAT-P object)
  { value1 = (m_long_float_p(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(complexp,1)
# (COMPLEXP object), CLTL S. 75
  { value1 = (mcomplexp(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(streamp,1)
# (STREAMP object), CLTL S. 332
  { value1 = (mstreamp(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(random_state_p,1)
# (RANDOM-STATE-P object), CLTL S. 231
  { var reg1 object arg = popSTACK();
    value1 = (random_state_p(arg) ? T : NIL); mv_count=1;
  }

LISPFUNN(readtablep,1)
# (READTABLEP object), CLTL S. 361
  { var reg1 object arg = popSTACK();
    value1 = (readtablep(arg) ? T : NIL); mv_count=1;
  }

LISPFUNN(hash_table_p,1)
# (HASH-TABLE-P object), CLTL S. 284
  { var reg1 object arg = popSTACK();
    value1 = (hash_table_p(arg) ? T : NIL); mv_count=1;
  }

LISPFUNN(pathnamep,1)
# (PATHNAMEP object), CLTL S. 416
  { var reg1 object arg = popSTACK();
    value1 = (pathnamep(arg) ? T : NIL); mv_count=1;
  }

LISPFUNN(characterp,1)
# (CHARACTERP object), CLTL S. 75
  { value1 = (mcharp(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(functionp,1)
# (FUNCTIONP object), CLTL S. 76
  { var reg1 object arg = popSTACK();
    # Test auf SUBR, Closure, Symbol, Cons (LAMBDA . ...):
    value1 = (subrp(arg) || closurep(arg) || symbolp(arg)
              || (consp(arg) && eq(Car(arg),S(lambda)))
              ? T
              : NIL
             );
    mv_count=1;
  }

LISPFUNN(packagep,1)
# (PACKAGEP object), CLTL S. 76
  { var reg1 object arg = popSTACK();
    value1 = (packagep(arg) ? T : NIL); mv_count=1;
  }

LISPFUNN(arrayp,1)
# (ARRAYP object), CLTL S. 76
  { if_arrayp(popSTACK(), value1 = T; , value1 = NIL; ); mv_count=1; }

LISPFUNN(simple_array_p,1)
# (SYSTEM::SIMPLE-ARRAY-P object)
  { var reg1 object arg = popSTACK();
    if_simplep(arg, goto yes; , # Simple eindimensionale Arrays -> ja
      { if_arrayp(arg, # sonstige Arrays, nur falls alle Flagbits =0 sind
          { if ((TheArray(arg)->flags
                 & (  bit(arrayflags_adjustable_bit)
                    | bit(arrayflags_fillp_bit)
                    | bit(arrayflags_displaced_bit)
                    | bit(arrayflags_dispoffset_bit)
                )  )
                == 0
               )
              { yes: value1 = T; }
              else
              goto no; # nicht-simple Arrays -> nein
          },
          { no: value1 = NIL; } # sonstige Objekte -> nein
          );
      });
    mv_count=1;
  }

LISPFUNN(bit_vector_p,1)
# (BIT-VECTOR-P object), CLTL S. 75
  { value1 = (m_bit_vector_p(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(vectorp,1)
# (VECTORP object), CLTL S. 75
  { value1 = (mvectorp(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(simple_vector_p,1)
# (SIMPLE-VECTOR-P object), CLTL S. 75
  { value1 = (m_simple_vector_p(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(simple_string_p,1)
# (SIMPLE-STRING-P object), CLTL S. 75
  { value1 = (m_simple_string_p(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(simple_bit_vector_p,1)
# (SIMPLE-BIT-VECTOR-P object), CLTL S. 76
  { value1 = (m_simple_bit_vector_p(STACK_0) ? T : NIL); mv_count=1; skipSTACK(1); }

LISPFUNN(commonp,1)
# (COMMONP object), CLTL S. 76
  { var reg1 object arg = popSTACK();
    # Fallunterscheidung nach Typ:
    switch (typecode(arg))
      { case_cons: goto yes; # Conses ja
        case_symbol: goto yes; # Symbole ja
        case_number: goto yes; # Zahlen ja
        case_array: goto yes; # Arrays ja
        case_structure: goto yes; # Structures ja
        case_stream: goto yes; # Streams ja
        case_char: # Character: nur Standard-Char
          # (STANDARD-CHAR-P object) als Wert:
          pushSTACK(arg); funcall(L(standard_char_p),1); return;
        case_orecord: # sonstige Records:
          # nur Package, Hash-Table, Readtable, Pathname, Random-State [,Structure, Stream]
          # Das sind die ersten 7 der orecord-Typen, Codes -1..5.
          if ((sintB)TheRecord(arg)->rectype < 6) goto yes; else goto no;
        default: goto no;
      }
     no: value1 = NIL; mv_count=1; return;
    yes: value1 = T; mv_count=1; return;
  }

LISPFUNN(type_of,1)
# (TYPE-OF object), CLTL S. 52
  { var reg2 object arg = popSTACK();
    switch (typecode(arg))
      { case_cons: # Cons -> CONS
          value1 = S(cons); break;
        case_symbol: # Symbol -> SYMBOL oder NULL
          value1 = (nullp(arg) ? S(null) : S(symbol)); break;
        case_machine: # Maschinenpointer -> ADDRESS
          value1 = S(address); break;
        case_sbvector: # Simple-Bit-Vector -> (SIMPLE-BIT-VECTOR dim0)
          pushSTACK(S(simple_bit_vector)); goto vectors;
        case_sstring: # Simple-String -> (SIMPLE-STRING dim0)
          pushSTACK(S(simple_string)); goto vectors;
        case_svector: # Simple-Vector -> (SIMPLE-VECTOR dim0)
          pushSTACK(S(simple_vector)); goto vectors;
        case_ostring: # sonstiger String -> (STRING dim0)
          pushSTACK(S(string)); goto vectors;
        vectors: # Typ des Vektors in STACK_0
          pushSTACK(array_dimensions(arg)); # Dimensionsliste
          {var reg1 object new_cons = allocate_cons();
           Cdr(new_cons) = popSTACK(); Car(new_cons) = popSTACK();
           value1 = new_cons;
          }
          break;
        case_ovector: # sonstiger general-vector -> (VECTOR T dim0)
          pushSTACK(array_dimensions(arg)); # Dimensionenliste
          {var reg1 object new_cons = allocate_cons();
           Cdr(new_cons) = popSTACK(); Car(new_cons) = T;
           pushSTACK(new_cons);
          }
          {var reg1 object new_cons = allocate_cons();
           Cdr(new_cons) = popSTACK(); Car(new_cons) = S(vector);
           value1 = new_cons;
          }
          break;
        case_obvector: # sonstiger Bit-Vector -> (BIT-VECTOR dim0)
                       # sonstiger Byte-Vector -> ([SIMPLE-]ARRAY (UNSIGNED-BYTE n) (dim0))
          if ((TheArray(arg)->flags & arrayflags_atype_mask) == Atype_Bit)
            { pushSTACK(S(bit_vector)); goto vectors; }
        case_array1: # sonstiger Array -> ([SIMPLE-]ARRAY eltype dims)
          pushSTACK( ((TheArray(arg)->flags
                       & (  bit(arrayflags_adjustable_bit)
                          | bit(arrayflags_fillp_bit)
                          | bit(arrayflags_displaced_bit)
                          | bit(arrayflags_dispoffset_bit)
                      )  )
                      == 0
                     )
                     ? S(simple_array)
                     : S(array)
                   );
          pushSTACK(arg);
          pushSTACK(array_dimensions(arg)); # Dimensionenliste
          STACK_1 = array_element_type(STACK_1); # eltype
          value1 = listof(3);
          break;
        case_closure: # Closure -> FUNCTION
          value1 = S(function); break;
        case_structure: # Structure -> Typ der Structure
          {var reg1 object type = TheStructure(arg)->structure_types;
           # (name_1 ... name_i-1 . name_i). Typ ist name_1.
           if (consp(type)) { type = Car(type); }
           value1 = type;
          }
          break;
        case_stream: # Stream -> STREAM
          value1 = S(stream); break;
        case_orecord: # OtherRecord -> PACKAGE, ...
          switch (TheRecord(arg)->rectype)
            { case Rectype_Hashtable: # Hash-Table
                value1 = S(hash_table); break;
              case Rectype_Package: # Package
                value1 = S(package); break;
              case Rectype_Readtable: # Readtable
                value1 = S(readtable); break;
              case Rectype_Pathname: # Pathname
                value1 = S(pathname); break;
              case Rectype_Random_State: # Random-State
                value1 = S(random_state); break;
              case Rectype_Byte: # Byte
                value1 = S(byte); break;
              case Rectype_Loadtimeeval: # Load-Time-Eval
                value1 = S(load_time_eval); break;
              #ifndef case_structure
              case Rectype_Structure: goto case_structure;
              #endif
              #ifndef case_stream
              case Rectype_Stream: goto case_stream;
              #endif
              #ifdef ALIEN
              case Rectype_Alienfun: # Alienfun -> COMPILED-FUNCTION
                value1 = S(compiled_function); break;
              case Rectype_Alien: # Alien -> (ALIEN type)
                pushSTACK(S(alien));
                pushSTACK(TheAlien(arg)->alien_type);
                value1 = listof(2);
                break;
              #endif
              default: goto unknown;
            }
          break;
        case_char: # Character -> CHARACTER
          value1 = S(character); break;
        case_subr: # SUBR -> COMPILED-FUNCTION
        case_fsubr: # FSUBR -> COMPILED-FUNCTION
          value1 = S(compiled_function); break;
        case_system: # -> FRAME-POINTER, READ-LABEL, SYSTEM-INTERNAL
          if (!wbit_test((oint)arg,oint_addr_len+oint_addr_shift-1))
            { value1 = S(frame_pointer); }
            else
            { if (!wbit_test((oint)arg,oint_addr_len+oint_addr_shift-2))
                { value1 = S(read_label); }
                else
                { value1 = S(system_internal); }
            }
          break;
        case_fixnum: # Fixnum -> FIXNUM
          value1 = S(fixnum); break;
        case_bignum: # Bignum -> BIGNUM
          value1 = S(bignum); break;
        case_ratio: # Ratio -> RATIO
          value1 = S(ratio); break;
        case_sfloat: # Short-Float -> SHORT-FLOAT
          value1 = S(short_float); break;
        case_ffloat: # Single-Float -> SINGLE-FLOAT
          value1 = S(single_float); break;
        case_dfloat: # Double-Float -> DOUBLE-FLOAT
          value1 = S(double_float); break;
        case_lfloat: # Long-Float -> LONG-FLOAT
          value1 = S(long_float); break;
        case_complex: # Complex -> COMPLEX
          value1 = S(complex); break;
        default:
        unknown: # unbekannter Typ
          pushSTACK(S(type_of));
          fehler(
                 DEUTSCH ? "~: Typ nicht identifizierbar!!!" :
                 ENGLISH ? "~: unidentifiable type!!!" :
                 FRANCAIS ? "~ : Type non identifiable!!!" :
                 ""
                );
      }
    mv_count=1;
  }

LISPFUNN(coerce,2)
# (COERCE object result-type), CLTL S. 51
# Methode:
# (TYPEP object result-type) -> object zurück
# result-type ein Symbol type:
#   (get type 'DEFTYPE-EXPANDER) /= NIL ->
#          mit (list result-type) als Argument aufrufen, zum Anfang
#   type = T -> object zurück
#   type = CHARACTER -> COERCE_CHAR anwenden
#   type = STRING-CHAR -> COERCE_CHAR anwenden und überprüfen
#   type = FLOAT, SHORT-FLOAT, SINGLE-FLOAT, DOUBLE-FLOAT, LONG-FLOAT ->
#          mit der Arithmetik umwandeln
#   type = COMPLEX -> auf Zahl überprüfen
#   type = ARRAY, SIMPLE-ARRAY, VECTOR, SIMPLE-VECTOR, STRING, SIMPLE-STRING,
#          BIT-VECTOR, SIMPLE-BIT-VECTOR ->
#          [hier auch result-type an object anpassen wie unten??]
#          mit COERCE-SEQUENCE umwandeln, mit TYPEP überprüfen
#          und evtl. mit COPY-SEQ kopieren.
#   sonst mit COERCE-SEQUENCE umwandeln
# result-type ein Cons mit Symbol type als CAR:
#   type = AND -> (coerce object (second result-type)), mit TYPEP überprüfen
#   (get type 'DEFTYPE-EXPANDER) /= NIL ->
#          mit result-type als Argument aufrufen, zum Anfang
#   type = FLOAT, SHORT-FLOAT, SINGLE-FLOAT, DOUBLE-FLOAT, LONG-FLOAT ->
#          mit der Arithmetik umwandeln, mit TYPEP überprüfen
#   type = COMPLEX -> auf Zahl überprüfen,
#          Realteil zum Typ (second result-type) coercen,
#          Imaginärteil zum Typ (third result-type) bzw. (second result-type)
#          coercen, COMPLEX anwenden.
#   type = ARRAY, SIMPLE-ARRAY, VECTOR, SIMPLE-VECTOR, STRING, SIMPLE-STRING,
#          BIT-VECTOR, SIMPLE-BIT-VECTOR -> result-type an object anpassen,
#          mit COERCE-SEQUENCE umwandeln (das verarbeitet auch den in
#          result-type angegebenen element-type), auf type überprüfen und
#          evtl. mit COPY-SEQ kopieren. Dann auf result-type überprüfen.
# Sonst Error.
  { # (TYPEP object result-type) abfragen:
    pushSTACK(STACK_1); pushSTACK(STACK_(0+1)); funcall(S(typep),2);
    if (!nullp(value1))
      # object als Wert
      return_object:
      { value1 = STACK_1; mv_count=1; skipSTACK(2); return; }
    anfang: # Los geht's mit der Umwandlung.
    # Stackaufbau: object, result-type.
    if (matomp(STACK_0))
      { if (!msymbolp(STACK_0)) goto fehler_type;
        # result-type ist ein Symbol
       {var reg1 object result_type = STACK_0;
        {var reg3 object expander = get(result_type,S(deftype_expander)); # (GET result-type 'DEFTYPE-EXPANDER)
         if (!eq(expander,unbound))
           { pushSTACK(expander);
            {var reg2 object new_cons = allocate_cons();
             expander = popSTACK();
             Car(new_cons) = STACK_0; # new_cons = (list result-type)
             pushSTACK(new_cons); funcall(expander,1); # Expander aufrufen
             STACK_0 = value1; # Ergebnis als neues result-type verwenden
             goto anfang;
        }  }}
        if (eq(result_type,T)) # result-type = T ?
          goto return_object; # ja -> object als Wert
        if (eq(result_type,S(character))) # result-type = CHARACTER ?
          { var reg2 object as_char = coerce_char(STACK_1); # object in Character umzuwandeln versuchen
            if (nullp(as_char)) goto fehler_object;
            value1 = as_char; mv_count=1; skipSTACK(2); return;
          }
        if (eq(result_type,S(string_char))) # result-type = STRING-CHAR ?
          { var reg2 object as_char = coerce_char(STACK_1); # object in Character umzuwandeln versuchen
            if (!string_char_p(as_char)) goto fehler_object;
            value1 = as_char; mv_count=1; skipSTACK(2); return;
          }
        if (   eq(result_type,S(float)) # FLOAT ?
            || eq(result_type,S(short_float)) # SHORT-FLOAT ?
            || eq(result_type,S(single_float)) # SINGLE-FLOAT ?
            || eq(result_type,S(double_float)) # DOUBLE-FLOAT ?
            || eq(result_type,S(long_float)) # LONG-FLOAT ?
           )
          { # object in Float umwandeln:
            subr_self = L(coerce);
            value1 = coerce_float(STACK_1,result_type); mv_count=1;
            skipSTACK(2); return;
          }
        if (eq(result_type,S(complex))) # COMPLEX ?
          { if (!mnumberp(STACK_1)) goto fehler_object; # object muß eine Zahl sein
            goto return_object;
          }
        if (   eq(result_type,S(array)) # ARRAY ?
            || eq(result_type,S(simple_array)) # SIMPLE-ARRAY ?
            || eq(result_type,S(vector)) # VECTOR ?
            || eq(result_type,S(simple_vector)) # SIMPLE-VECTOR ?
            || eq(result_type,S(string)) # STRING ?
            || eq(result_type,S(simple_string)) # SIMPLE-STRING ?
            || eq(result_type,S(bit_vector)) # BIT-VECTOR ?
            || eq(result_type,S(simple_bit_vector)) # SIMPLE-BIT-VECTOR ?
           )
          { # result-type an den Typ von object anpassen:
            if (eq(result_type,S(array)) || eq(result_type,S(vector))) # ARRAY oder VECTOR ?
              { if (mstringp(STACK_1)) # object ein String
                  goto return_object; # -> ist ein Vektor und Array
                elif (m_bit_vector_p(STACK_1)) # object ein Bitvektor
                  goto return_object; # -> ist ein Vektor und Array
                # Hier auch Byte-Vektoren behandeln!??
              }
            elif (eq(result_type,S(simple_array))) # SIMPLE-ARRAY ?
              { if (mstringp(STACK_1)) # object ein String
                  { result_type = S(simple_string); } # -> result-type := SIMPLE-STRING
                elif (m_bit_vector_p(STACK_1)) # object ein Bitvektor
                  { result_type = S(simple_bit_vector); } # -> result-type := SIMPLE-BIT-VECTOR
                # Hier auch Byte-Vektoren behandeln!??
              }
            pushSTACK(result_type);
            # neue Sequence bauen:
           {var reg2 object new_seq = (coerce_sequence(STACK_2,result_type),value1);
            # und nochmals mit TYPEP überprüfen:
            pushSTACK(new_seq); pushSTACK(STACK_(0+1)); STACK_(0+2) = new_seq;
            funcall(S(typep),2); # (TYPEP new_seq result-type)
            if (!nullp(value1))
              # ja -> new_seq als Wert
              { value1 = STACK_0; mv_count=1; skipSTACK(2+1); return; }
              else
              # Trifft wegen SIMPLE-... nicht zu -> new_seq kopieren:
              { funcall(L(copy_seq),1); # (COPY-SEQ new_seq)
                skipSTACK(2); return;
              }
          }}
        # result-type ist ein sonstiges Symbol
        coerce_sequence(STACK_1,result_type); # (coerce-sequence object result-type)
        skipSTACK(2); return;
      }}
      else
      # result-type ist ein Cons.
      { var reg2 object result_type = STACK_0;
        var reg1 object type = Car(result_type);
        if (!symbolp(type)) goto fehler_type; # muß ein Symbol sein
        if (eq(type,S(and))) # (AND ...) ?
          { if (matomp(Cdr(result_type))) # (AND)
              goto return_object; # wie T behandeln
            # (COERCE object (second result-type)) ausführen:
            pushSTACK(STACK_1); pushSTACK(Car(Cdr(result_type)));
            funcall(L(coerce),2);
            check_return: # new-object in value1 überprüfen und dann als Wert liefern:
            pushSTACK(value1); # new-object retten
            # (TYPEP new-object result-type) abfragen:
            pushSTACK(value1); pushSTACK(STACK_(0+1+1)); funcall(S(typep),2);
            if (nullp(value1))
              { skipSTACK(1); goto fehler_object; }
              else
              { value1 = STACK_0; mv_count=1; skipSTACK(3); return; } # new-object
          }
        {var reg3 object expander = get(type,S(deftype_expander)); # (GET type 'DEFTYPE-EXPANDER)
         if (!eq(expander,unbound))
           { pushSTACK(result_type); funcall(expander,1); # Expander aufrufen
             STACK_0 = value1; # Ergebnis als neues result-type verwenden
             goto anfang;
        }  }
        if (   eq(type,S(float)) # FLOAT ?
            || eq(type,S(short_float)) # SHORT-FLOAT ?
            || eq(type,S(single_float)) # SINGLE-FLOAT ?
            || eq(type,S(double_float)) # DOUBLE-FLOAT ?
            || eq(type,S(long_float)) # LONG-FLOAT ?
           )
          { # object in Float umwandeln:
            subr_self = L(coerce);
            value1 = coerce_float(STACK_1,result_type);
            goto check_return; # und auf result-type überprüfen
          }
        if (eq(type,S(complex))) # COMPLEX ?
          { if (!mnumberp(STACK_1)) goto fehler_object; # object muß eine Zahl sein
            if (!mconsp(Cdr(result_type))) goto fehler_type; # (rest result-type) muß ein Cons sein
            result_type = Cdr(result_type);
           {var reg3 object rtype = Car(result_type); # Typ für den Realteil
            var reg4 object itype = # Typ für den Imaginärteil, Default ist rtype
              (mconsp(Cdr(result_type)) ? Car(Cdr(result_type)) : rtype);
            pushSTACK(rtype); pushSTACK(itype);
            # Realteil holen und zum Typ rtype coercen:
            pushSTACK(STACK_(1+2)); funcall(L(realpart),1);
            pushSTACK(value1); pushSTACK(STACK_(1+1)); funcall(L(coerce),2);
            STACK_1 = value1;
            # Imaginärteil holen und zum Typ itype coercen:
            pushSTACK(STACK_(1+2)); funcall(L(imagpart),1);
            pushSTACK(value1); pushSTACK(STACK_(0+1)); funcall(L(coerce),2);
            STACK_0 = value1;
            # COMPLEX darauf anwenden:
            funcall(L(complex),2);
            skipSTACK(2); return;
          }}
        if (   eq(type,S(array)) # ARRAY ?
            || eq(type,S(simple_array)) # SIMPLE-ARRAY ?
            || eq(type,S(vector)) # VECTOR ?
            || eq(type,S(simple_vector)) # SIMPLE-VECTOR ?
            || eq(type,S(string)) # STRING ?
            || eq(type,S(simple_string)) # SIMPLE-STRING ?
            || eq(type,S(bit_vector)) # BIT-VECTOR ?
            || eq(type,S(simple_bit_vector)) # SIMPLE-BIT-VECTOR ?
           )
          { # result-type an den Typ von object anpassen:
            if (eq(type,S(array)) || eq(type,S(simple_array)) || eq(type,S(vector))) # [SIMPLE-]ARRAY oder VECTOR ?
              { var reg4 object type2 = Cdr(result_type);
                if (nullp(type2)) { goto adjust_eltype; }
                if (!consp(type2)) goto fehler_type;
                if (eq(Car(type2),S(mal))) # element-type = * (unspecified) ?
                  { type2 = Cdr(type2);
                    adjust_eltype: # Hier ist type2 = (cddr result-type)
                    # wird ersetzt durch geeigneten Elementtyp:
                    pushSTACK(type);
                    pushSTACK(type2);
                    if_arrayp(STACK_(1+2),
                      { pushSTACK(array_element_type(STACK_(1+2))); },
                      { pushSTACK(T); });
                   {var reg3 object new_cons = allocate_cons();
                    Car(new_cons) = popSTACK(); Cdr(new_cons) = popSTACK();
                    pushSTACK(new_cons);
                   }
                   {var reg3 object new_cons = allocate_cons();
                    Cdr(new_cons) = popSTACK();
                    Car(new_cons) = type = popSTACK();
                    result_type = new_cons;
              }   }}
            pushSTACK(type);
            # neue Sequence bauen:
           {var reg3 object new_seq = (coerce_sequence(STACK_2,result_type),value1);
            # und nochmals mit TYPEP überprüfen:
            pushSTACK(new_seq); pushSTACK(STACK_(0+1)); STACK_(0+2) = new_seq;
            funcall(S(typep),2); # (TYPEP new_seq type)
            if (!nullp(value1))
              { value1 = popSTACK(); }
              else
              # Trifft wegen SIMPLE-... nicht zu -> new_seq kopieren:
              { funcall(L(copy_seq),1); } # (COPY-SEQ new_seq)
            goto check_return;
          }}
        # type ist ein sonstiges Symbol
      }
    fehler_type:
      # result-type in STACK_0
      pushSTACK(S(coerce));
      fehler(
             DEUTSCH ? "~: ~ ist keine zugelassene Typspezifikation." :
             ENGLISH ? "~: bad type specification ~" :
             FRANCAIS ? "~ : ~ est une mauvaise spécification de type." :
             ""
            );
    fehler_object:
      # result-type in STACK_0
      pushSTACK(STACK_1); # object
      pushSTACK(S(coerce));
      fehler(
             DEUTSCH ? "~: ~ kann nicht in Typ ~ umgewandelt werden." :
             ENGLISH ? "~: ~ cannot be coerced to type ~" :
             FRANCAIS ? "~ : ~ ne peut pas être transformé en objet de type ~." :
             ""
            );
  }

