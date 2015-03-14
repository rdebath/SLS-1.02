# restliche Float-Funktionen


# Macro: verteilt je nach Float-Typ eines Floats x auf 4 Statements.
# floatcase(x, SF_statement,FF_statement,DF_statement,LF_statement);
# x sollte eine Variable sein.
  #define floatcase(obj, SF_statement,FF_statement,DF_statement,LF_statement) \
    { if (!wbit_test((oint)(obj),float1_bit_o))   \
        if (!wbit_test((oint)(obj),float2_bit_o)) \
          { SF_statement }                        \
          else                                    \
          { FF_statement }                        \
        else                                      \
        if (!wbit_test((oint)(obj),float2_bit_o)) \
          { DF_statement }                        \
          else                                    \
          { LF_statement }                        \
    }


# Generiert eine Float-Operation F_op_F wie F_minus_F oder F_durch_F
  #define GEN_F_op1(op)  \
    local object CONCAT3(F_,op,_F) (x)                 \
      var reg1 object x;                               \
      { floatcase(x,                                   \
                  { return CONCAT3(SF_,op,_SF) (x); }, \
                  { return CONCAT3(FF_,op,_FF) (x); }, \
                  { return CONCAT3(DF_,op,_DF) (x); }, \
                  { return CONCAT3(LF_,op,_LF) (x); }  \
                 );                                    \
      }

# F_minus_F(x) liefert (- x), wo x ein Float ist.
# kann GC auslösen
  local object F_minus_F (object x);
  GEN_F_op1(minus)

# F_abs_F(x) liefert (abs x), wo x ein Float ist.
# kann GC auslösen
  local object F_abs_F (object x);
  local object F_abs_F(x)
    var reg1 object x;
    { return (R_minusp(x) ? F_minus_F(x) : x); } # x<0 -> (- x), x>=0 -> x

# SF_durch_SF(x) liefert (/ x), wo x ein SF ist.
  #define SF_durch_SF(x)  SF_SF_durch_SF(SF_1,x)

# FF_durch_FF(x) liefert (/ x), wo x ein FF ist.
# kann GC auslösen
  #define FF_durch_FF(x)  FF_FF_durch_FF(FF_1,x)

# DF_durch_DF(x) liefert (/ x), wo x ein DF ist.
# kann GC auslösen
  #define DF_durch_DF(x)  DF_DF_durch_DF(DF_1,x)

# LF_durch_LF(x) liefert (/ x), wo x ein LF ist.
# kann GC auslösen
  local object LF_durch_LF (object x);
  local object LF_durch_LF(x)
    var reg1 object x;
    { pushSTACK(x);
      encode_LF1(TheLfloat(x)->len, x=);
      return LF_LF_durch_LF(x,popSTACK());
    }

# F_durch_F(x) liefert (/ x), wo x ein Float ist.
# kann GC auslösen
  local object F_durch_F (object x);
  GEN_F_op1(durch)

# F_sqrt_F(x) liefert (sqrt x), wo x ein Float >=0 ist.
# kann GC auslösen
  local object F_sqrt_F (object x);
  GEN_F_op1(sqrt)


# Generiert eine Float-Funktion mit zwei Argumenten.
# Die Funktion wird erst ausgeführt, nachdem beide Argumente auf dasselbe
# Float-Format (das längere von beiden) gebracht wurden; danach werden die
# r (=0,1 oder 2) Ergebnisse auf das kürzere der beiden Float-Formate
# gebracht.
# s (=0 oder 1): Da LF_LF_comp Long-Floats verschiedener Längen verarbeitet,
# braucht bei s=1 ein SF, FF oder DF nur zu einem LF der Länge LF_minlen
# gemacht zu werden.
  #define GEN_F_op2(arg1,arg2,SF_op,FF_op,DF_op,LF_op,r,s,RETURN)  \
    { floatcase(arg1,                                                                                                  \
      /* arg1 SF */ { floatcase(arg2,                                                                                  \
                      /* arg2 SF */ { RETURN SF_op(arg1,arg2); },                                                      \
                      /* arg2 FF */ { pushSTACK(arg2); arg1 = SF_to_FF(arg1); arg2 = popSTACK();                       \
                                      RETURN CONCAT(TO_F_,r) (FF_op(arg1,arg2),FF_to_SF);                              \
                                    },                                                                                 \
                      /* arg2 DF */ { pushSTACK(arg2); arg1 = SF_to_DF(arg1); arg2 = popSTACK();                       \
                                      RETURN CONCAT(TO_F_,r) (DF_op(arg1,arg2),DF_to_SF);                              \
                                    },                                                                                 \
                      /* arg2 LF */ { pushSTACK(arg2); arg1 = SF_to_LF(arg1,CONCAT(LFlen,s)(arg2)); arg2 = popSTACK(); \
                                      RETURN CONCAT(TO_F_,r) (LF_op(arg1,arg2),LF_to_SF);                              \
                                    }                                                                                  \
                               );                                                                                      \
                    },                                                                                                 \
      /* arg1 FF */ { floatcase(arg2,                                                                                  \
                      /* arg2 SF */ { pushSTACK(arg1); arg2 = SF_to_FF(arg2); arg1 = popSTACK();                       \
                                      RETURN CONCAT(TO_F_,r) (FF_op(arg1,arg2),FF_to_SF);                              \
                                    },                                                                                 \
                      /* arg2 FF */ { RETURN FF_op(arg1,arg2); },                                                      \
                      /* arg2 DF */ { pushSTACK(arg2); arg1 = FF_to_DF(arg1); arg2 = popSTACK();                       \
                                      RETURN CONCAT(TO_F_,r) (DF_op(arg1,arg2),DF_to_FF);                              \
                                    },                                                                                 \
                      /* arg2 LF */ { pushSTACK(arg2); arg1 = FF_to_LF(arg1,CONCAT(LFlen,s)(arg2)); arg2 = popSTACK(); \
                                      RETURN CONCAT(TO_F_,r) (LF_op(arg1,arg2),LF_to_FF);                              \
                                    }                                                                                  \
                               );                                                                                      \
                    },                                                                                                 \
      /* arg1 DF */ { floatcase(arg2,                                                                                  \
                      /* arg2 SF */ { pushSTACK(arg1); arg2 = SF_to_DF(arg2); arg1 = popSTACK();                       \
                                      RETURN CONCAT(TO_F_,r) (DF_op(arg1,arg2),DF_to_SF);                              \
                                    },                                                                                 \
                      /* arg2 FF */ { pushSTACK(arg1); arg2 = FF_to_DF(arg2); arg1 = popSTACK();                       \
                                      RETURN CONCAT(TO_F_,r) (DF_op(arg1,arg2),DF_to_FF);                              \
                                    },                                                                                 \
                      /* arg2 DF */ { RETURN DF_op(arg1,arg2); },                                                      \
                      /* arg2 LF */ { pushSTACK(arg2); arg1 = DF_to_LF(arg1,CONCAT(LFlen,s)(arg2)); arg2 = popSTACK(); \
                                      RETURN CONCAT(TO_F_,r) (LF_op(arg1,arg2),LF_to_DF);                              \
                                    }                                                                                  \
                               );                                                                                      \
                    },                                                                                                 \
      /* arg1 LF */ { floatcase(arg2,                                                                                  \
                      /* arg2 SF */ { pushSTACK(arg1); arg2 = SF_to_LF(arg2,CONCAT(LFlen,s)(arg1)); arg1 = popSTACK(); \
                                      RETURN CONCAT(TO_F_,r) (LF_op(arg1,arg2),LF_to_SF);                              \
                                    },                                                                                 \
                      /* arg2 FF */ { pushSTACK(arg1); arg2 = FF_to_LF(arg2,CONCAT(LFlen,s)(arg1)); arg1 = popSTACK(); \
                                      RETURN CONCAT(TO_F_,r) (LF_op(arg1,arg2),LF_to_FF);                              \
                                    },                                                                                 \
                      /* arg2 DF */ { pushSTACK(arg1); arg2 = DF_to_LF(arg2,CONCAT(LFlen,s)(arg1)); arg1 = popSTACK(); \
                                      RETURN CONCAT(TO_F_,r) (LF_op(arg1,arg2),LF_to_DF);                              \
                                    },                                                                                 \
                      /* arg2 LF */ { CONCAT(GEN_LF_op2_,s)(arg1,arg2,LF_op,r,_EMA_ RETURN); }                         \
                               );                                                                                      \
                    }                                                                                                  \
               );                                                                                                      \
    }
  # Hilfmacro, wenn arg1 und arg2 beide LF sind:
  #define GEN_LF_op2_0(arg1,arg2,LF_op,r,ergebnis_zuweisung)  \
    { var reg3 uintC len1 = TheLfloat(arg1)->len;                                \
      var reg4 uintC len2 = TheLfloat(arg2)->len;                                \
      if (len1==len2) # gleich -> direkt ausführen                               \
        { ergebnis_zuweisung LF_op(arg1,arg2); }                                 \
      elif (len1>len2) # -> arg2 auf die Länge von arg1 bringen                  \
        { pushSTACK(arg1); arg2 = LF_extend_LF(arg2,len1); arg1 = popSTACK();    \
          ergebnis_zuweisung CONCAT(TO_F_,r) (LF_op(arg1,arg2),LF_shorten_LF_2); \
        }                                                                        \
      else # (len1<len2) -> arg1 auf die Länge von arg2 bringen                  \
        { pushSTACK(arg2); arg1 = LF_extend_LF(arg1,len2); arg2 = popSTACK();    \
          ergebnis_zuweisung CONCAT(TO_F_,r) (LF_op(arg1,arg2),LF_shorten_LF_1); \
        }                                                                        \
    }
  #define GEN_LF_op2_1(arg1,arg2,LF_op,r,ergebnis_zuweisung)  \
    ergebnis_zuweisung LF_op(arg1,arg2);
  #define LF_shorten_LF_1(arg)  LF_shorten_LF(arg,len1)
  #define LF_shorten_LF_2(arg)  LF_shorten_LF(arg,len2)
  # Hilfsmacro zum Besorgen der Ziel-Länge für Konversion SF,FF,DF -> LF :
  #define LFlen0(arg)  TheLfloat(arg)->len
  #define LFlen1(arg)  LF_minlen
  # Hilfsmacro zur Konversion des Ergebnisses zurück zum kürzeren Format:
  #define TO_F_0(erg,to)  erg
  #define TO_F_1(erg,to)  to(erg)
  #define TO_F_2(erg,to)  \
    erg; # Operation durchführen                 \
    { STACK_1 = to(STACK_1); # 1. Wert umwandeln \
      STACK_0 = to(STACK_0); # 2. Wert umwandeln \
    }

# F_F_plus_F(x,y) liefert (+ x y), wo x und y Floats sind.
# kann GC auslösen
  local object F_F_plus_F (object x, object y);
  local object F_F_plus_F(x,y)
    var reg1 object x;
    var reg2 object y;
    { GEN_F_op2(x,y,SF_SF_plus_SF,FF_FF_plus_FF,DF_DF_plus_DF,LF_LF_plus_LF,1,0,return) }

# F_F_minus_F(x,y) liefert (- x y), wo x und y Floats sind.
# kann GC auslösen
  local object F_F_minus_F (object x, object y);
  local object F_F_minus_F(x,y)
    var reg1 object x;
    var reg2 object y;
    { GEN_F_op2(x,y,SF_SF_minus_SF,FF_FF_minus_FF,DF_DF_minus_DF,LF_LF_minus_LF,1,0,return) }

# F_F_mal_F(x,y) liefert (* x y), wo x und y Floats sind.
# kann GC auslösen
  local object F_F_mal_F (object x, object y);
  local object F_F_mal_F(x,y)
    var reg1 object x;
    var reg2 object y;
    { GEN_F_op2(x,y,SF_SF_mal_SF,FF_FF_mal_FF,DF_DF_mal_DF,LF_LF_mal_LF,1,0,return) }

# F_F_durch_F(x,y) liefert (/ x y), wo x und y Floats sind.
# kann GC auslösen
  local object F_F_durch_F (object x, object y);
  local object F_F_durch_F(x,y)
    var reg1 object x;
    var reg2 object y;
    { GEN_F_op2(x,y,SF_SF_durch_SF,FF_FF_durch_FF,DF_DF_durch_DF,LF_LF_durch_LF,1,0,return) }

# F_F_comp(x,y) vergleicht zwei Floats x und y.
# Ergebnis: 0 falls x=y, +1 falls x>y, -1 falls x<y.
# kann GC auslösen
  local signean F_F_comp (object x, object y);
  local signean F_F_comp(x,y)
    var reg1 object x;
    var reg2 object y;
    { GEN_F_op2(x,y,SF_SF_comp,FF_FF_comp,DF_DF_comp,LF_LF_comp,0,1,return) }


# Generiert eine Funktion wie SF_ffloor_SF
# Methode: x<0 -> von der 0 wegrunden, sonst zur 0 hinrunden.
  #define GEN_ffloor(F)  \
    local object CONCAT3(F,_ffloor_,F) (x)      \
      var reg1 object x;                        \
      { return (R_minusp(x)                     \
                ? CONCAT3(F,_futruncate_,F) (x) \
                : CONCAT3(F,_ftruncate_,F) (x)  \
               );                               \
      }

# SF_ffloor_SF(x) liefert (ffloor x), wo x ein SF ist.
  local object SF_ffloor_SF (object x);
  GEN_ffloor(SF)

# FF_ffloor_FF(x) liefert (ffloor x), wo x ein FF ist.
# kann GC auslösen
  local object FF_ffloor_FF (object x);
  GEN_ffloor(FF)

# DF_ffloor_DF(x) liefert (ffloor x), wo x ein DF ist.
# kann GC auslösen
  local object DF_ffloor_DF (object x);
  GEN_ffloor(DF)

# LF_ffloor_LF(x) liefert (ffloor x), wo x ein LF ist.
# kann GC auslösen
  local object LF_ffloor_LF (object x);
  GEN_ffloor(LF)

# Generiert eine Funktion wie SF_fceiling_SF
# Methode: x<0 -> zur 0 hinrunden, sonst von der 0 wegrunden.
  #define GEN_fceiling(F)  \
    local object CONCAT3(F,_fceiling_,F) (x)    \
      var reg1 object x;                        \
      { return (R_minusp(x)                     \
                ? CONCAT3(F,_ftruncate_,F) (x)  \
                : CONCAT3(F,_futruncate_,F) (x) \
               );                               \
      }

# SF_fceiling_SF(x) liefert (fceiling x), wo x ein SF ist.
  local object SF_fceiling_SF (object x);
  GEN_fceiling(SF)

# FF_fceiling_FF(x) liefert (fceiling x), wo x ein FF ist.
# kann GC auslösen
  local object FF_fceiling_FF (object x);
  GEN_fceiling(FF)

# DF_fceiling_DF(x) liefert (fceiling x), wo x ein DF ist.
# kann GC auslösen
  local object DF_fceiling_DF (object x);
  GEN_fceiling(DF)

# LF_fceiling_LF(x) liefert (fceiling x), wo x ein LF ist.
# kann GC auslösen
  local object LF_fceiling_LF (object x);
  GEN_fceiling(LF)


# Generiert eine Funktion wie SF_fround_SF_SF
  #define GEN_fround(F,rounding)  \
    local void CONCAT7(F,_f,rounding,_,F,_,F) (x)  \
      var reg1 object x;                                                              \
      { pushSTACK(x);                                                                 \
       {var reg2 object y = CONCAT5(F,_f,rounding,_,F) (x); # ganzer Anteil von x     \
        x = STACK_0; STACK_0 = y;                                                     \
        pushSTACK( CONCAT5(F,_,F,_minus_,F) (x,y) ); # x-y = gebrochener Anteil von x \
      }}

# SF_ffloor_SF_SF(x) liefert (ffloor x), wo x ein SF ist.
# Beide Werte in den Stack.
  local void SF_ffloor_SF_SF (object x);
  GEN_fround(SF,floor)

# FF_ffloor_FF_FF(x) liefert (ffloor x), wo x ein FF ist.
# Beide Werte in den Stack.
# kann GC auslösen
  local void FF_ffloor_FF_FF (object x);
  GEN_fround(FF,floor)

# DF_ffloor_DF_DF(x) liefert (ffloor x), wo x ein DF ist.
# Beide Werte in den Stack.
# kann GC auslösen
  local void DF_ffloor_DF_DF (object x);
  GEN_fround(DF,floor)

# LF_ffloor_LF_LF(x) liefert (ffloor x), wo x ein LF ist.
# Beide Werte in den Stack.
# kann GC auslösen
  local void LF_ffloor_LF_LF (object x);
  GEN_fround(LF,floor)

# SF_fceiling_SF_SF(x) liefert (fceiling x), wo x ein SF ist.
# Beide Werte in den Stack.
  local void SF_fceiling_SF_SF (object x);
  GEN_fround(SF,ceiling)

# FF_fceiling_FF_FF(x) liefert (fceiling x), wo x ein FF ist.
# Beide Werte in den Stack.
# kann GC auslösen
  local void FF_fceiling_FF_FF (object x);
  GEN_fround(FF,ceiling)

# DF_fceiling_DF_DF(x) liefert (fceiling x), wo x ein DF ist.
# Beide Werte in den Stack.
# kann GC auslösen
  local void DF_fceiling_DF_DF (object x);
  GEN_fround(DF,ceiling)

# LF_fceiling_LF_LF(x) liefert (fceiling x), wo x ein LF ist.
# Beide Werte in den Stack.
# kann GC auslösen
  local void LF_fceiling_LF_LF (object x);
  GEN_fround(LF,ceiling)

# SF_ftruncate_SF_SF(x) liefert (ftruncate x), wo x ein SF ist.
# Beide Werte in den Stack.
  local void SF_ftruncate_SF_SF (object x);
  GEN_fround(SF,truncate)

# FF_ftruncate_FF_FF(x) liefert (ftruncate x), wo x ein FF ist.
# Beide Werte in den Stack.
# kann GC auslösen
  local void FF_ftruncate_FF_FF (object x);
  GEN_fround(FF,truncate)

# DF_ftruncate_DF_DF(x) liefert (ftruncate x), wo x ein DF ist.
# Beide Werte in den Stack.
# kann GC auslösen
  local void DF_ftruncate_DF_DF (object x);
  GEN_fround(DF,truncate)

# LF_ftruncate_LF_LF(x) liefert (ftruncate x), wo x ein LF ist.
# Beide Werte in den Stack.
# kann GC auslösen
  local void LF_ftruncate_LF_LF (object x);
  GEN_fround(LF,truncate)

# SF_fround_SF_SF(x) liefert (fround x), wo x ein SF ist.
# Beide Werte in den Stack.
  local void SF_fround_SF_SF (object x);
  GEN_fround(SF,round)

# FF_fround_FF_FF(x) liefert (fround x), wo x ein FF ist.
# Beide Werte in den Stack.
# kann GC auslösen
  local void FF_fround_FF_FF (object x);
  GEN_fround(FF,round)

# DF_fround_DF_DF(x) liefert (fround x), wo x ein DF ist.
# Beide Werte in den Stack.
# kann GC auslösen
  local void DF_fround_DF_DF (object x);
  GEN_fround(DF,round)

# LF_fround_LF_LF(x) liefert (fround x), wo x ein LF ist.
# Beide Werte in den Stack.
# kann GC auslösen
  local void LF_fround_LF_LF (object x);
  GEN_fround(LF,round)


# Generiert eine Funktion wie SF_round_I_SF
  #define GEN_round(F,rounding)  \
    local void CONCAT7(F,_,rounding,_,I,_,F) (x)  \
      var reg1 object x;                                                   \
      { CONCAT7(F,_f,rounding,_,F,_,F) (x);                                \
        STACK_1 = CONCAT3(F,_to_,I) (STACK_1); # ganzer Anteil als Integer \
      }

# SF_floor_I_SF(x) liefert (floor x), wo x ein SF ist.
# Beide Werte in den Stack.
  local void SF_floor_I_SF (object x);
  GEN_round(SF,floor)

# FF_floor_I_FF(x) liefert (floor x), wo x ein FF ist.
# Beide Werte in den Stack.
# kann GC auslösen
  local void FF_floor_I_FF (object x);
  GEN_round(FF,floor)

# DF_floor_I_DF(x) liefert (floor x), wo x ein DF ist.
# Beide Werte in den Stack.
# kann GC auslösen
  local void DF_floor_I_DF (object x);
  GEN_round(DF,floor)

# LF_floor_I_LF(x) liefert (floor x), wo x ein LF ist.
# Beide Werte in den Stack.
# kann GC auslösen
  local void LF_floor_I_LF (object x);
  GEN_round(LF,floor)

# SF_ceiling_I_SF(x) liefert (ceiling x), wo x ein SF ist.
# Beide Werte in den Stack.
  local void SF_ceiling_I_SF (object x);
  GEN_round(SF,ceiling)

# FF_ceiling_I_FF(x) liefert (ceiling x), wo x ein FF ist.
# Beide Werte in den Stack.
# kann GC auslösen
  local void FF_ceiling_I_FF (object x);
  GEN_round(FF,ceiling)

# DF_ceiling_I_DF(x) liefert (ceiling x), wo x ein DF ist.
# Beide Werte in den Stack.
# kann GC auslösen
  local void DF_ceiling_I_DF (object x);
  GEN_round(DF,ceiling)

# LF_ceiling_I_LF(x) liefert (ceiling x), wo x ein LF ist.
# Beide Werte in den Stack.
# kann GC auslösen
  local void LF_ceiling_I_LF (object x);
  GEN_round(LF,ceiling)

# SF_truncate_I_SF(x) liefert (truncate x), wo x ein SF ist.
# Beide Werte in den Stack.
  local void SF_truncate_I_SF (object x);
  GEN_round(SF,truncate)

# FF_truncate_I_FF(x) liefert (truncate x), wo x ein FF ist.
# Beide Werte in den Stack.
# kann GC auslösen
  local void FF_truncate_I_FF (object x);
  GEN_round(FF,truncate)

# DF_truncate_I_DF(x) liefert (truncate x), wo x ein DF ist.
# Beide Werte in den Stack.
# kann GC auslösen
  local void DF_truncate_I_DF (object x);
  GEN_round(DF,truncate)

# LF_truncate_I_LF(x) liefert (truncate x), wo x ein LF ist.
# Beide Werte in den Stack.
# kann GC auslösen
  local void LF_truncate_I_LF (object x);
  GEN_round(LF,truncate)

# SF_round_I_SF(x) liefert (round x), wo x ein SF ist.
# Beide Werte in den Stack.
  local void SF_round_I_SF (object x);
  GEN_round(SF,round)

# FF_round_I_FF(x) liefert (round x), wo x ein FF ist.
# Beide Werte in den Stack.
# kann GC auslösen
  local void FF_round_I_FF (object x);
  GEN_round(FF,round)

# DF_round_I_DF(x) liefert (round x), wo x ein DF ist.
# Beide Werte in den Stack.
# kann GC auslösen
  local void DF_round_I_DF (object x);
  GEN_round(DF,round)

# LF_round_I_LF(x) liefert (round x), wo x ein LF ist.
# Beide Werte in den Stack.
# kann GC auslösen
  local void LF_round_I_LF (object x);
  GEN_round(LF,round)


# Generiert eine Funktion wie F_fround_F_F
  #define GEN_F_fround(rounding)  \
    local void CONCAT3(F_f,rounding,_F_F) (x)                     \
      var reg1 object x;                                          \
      { floatcase(x,                                              \
                  { CONCAT3(SF_f,rounding,_SF_SF) (x); return; }, \
                  { CONCAT3(FF_f,rounding,_FF_FF) (x); return; }, \
                  { CONCAT3(DF_f,rounding,_DF_DF) (x); return; }, \
                  { CONCAT3(LF_f,rounding,_LF_LF) (x); return; }  \
                 );                                               \
      }

# F_ffloor_F_F(x) liefert (ffloor x), wo x ein Float ist.
# Beide Werte in den Stack.
# kann GC auslösen
  local void F_ffloor_F_F (object x);
  GEN_F_fround(floor)

# F_fceiling_F_F(x) liefert (fceiling x), wo x ein Float ist.
# Beide Werte in den Stack.
# kann GC auslösen
  local void F_fceiling_F_F (object x);
  GEN_F_fround(ceiling)

# F_ftruncate_F_F(x) liefert (ftruncate x), wo x ein Float ist.
# Beide Werte in den Stack.
# kann GC auslösen
  local void F_ftruncate_F_F (object x);
  GEN_F_fround(truncate)

# F_fround_F_F(x) liefert (fround x), wo x ein Float ist.
# Beide Werte in den Stack.
# kann GC auslösen
  local void F_fround_F_F (object x);
  GEN_F_fround(round)


# Generiert eine Funktion wie F_round_I_F
  #define GEN_F_round(rounding)  \
    local void CONCAT3(F_,rounding,_I_F) (x)                    \
      var reg1 object x;                                        \
      { floatcase(x,                                            \
                  { CONCAT3(SF_,rounding,_I_SF) (x); return; }, \
                  { CONCAT3(FF_,rounding,_I_FF) (x); return; }, \
                  { CONCAT3(DF_,rounding,_I_DF) (x); return; }, \
                  { CONCAT3(LF_,rounding,_I_LF) (x); return; }  \
                 );                                             \
      }

# F_floor_I_F(x) liefert (floor x), wo x ein Float ist.
# Beide Werte in den Stack.
# kann GC auslösen
  local void F_floor_I_F (object x);
  GEN_F_round(floor)

# F_ceiling_I_F(x) liefert (ceiling x), wo x ein Float ist.
# Beide Werte in den Stack.
# kann GC auslösen
  local void F_ceiling_I_F (object x);
  GEN_F_round(ceiling)

# F_truncate_I_F(x) liefert (truncate x), wo x ein Float ist.
# Beide Werte in den Stack.
# kann GC auslösen
  local void F_truncate_I_F (object x);
  GEN_F_round(truncate)

# F_round_I_F(x) liefert (round x), wo x ein Float ist.
# Beide Werte in den Stack.
# kann GC auslösen
  local void F_round_I_F (object x);
  GEN_F_round(round)


# Generiert eine Funktion wie F_F_floor_I_F
  #define GEN_F_F_round(rounding)  \
    # Liefert ganzzahligen Quotienten und Rest \
    # einer Division reeller Zahlen.           \
    # (q,r) := (rounding x y)                  \
    # F_F_rounding_I_F(x,y);                   \
    # > x,y: reelle Zahlen                     \
    # < STACK_1: Quotient q, ein Integer       \
    # < STACK_0: Rest r, eine reelle Zahl      \
    # Erniedrigt STACK um 2                    \
    # kann GC auslösen                         \
    # Methode:                                               \
    # F_rounding_I_F(x/y) -> (q,r). Liefere q und x-y*q=y*r. \
    local void CONCAT3(F_F_,rounding,_I_F) (x,y) \
      var reg2 object x;                         \
      var reg1 object y;                         \
      { pushSTACK(y);                            \
        CONCAT3(F_,rounding,_I_F) (F_F_durch_F(x,y)); # ganzzahligen Anteil des Quotienten bilden \
        y = STACK_2; STACK_2 = STACK_1;          \
        STACK_1 = F_F_mal_F(y,STACK_0); # Nachkommateil mit y multiplizieren \
        skipSTACK(1);                            \
      }

# F_F_floor_I_F(x,y) liefert (floor x y), wo x und y Floats sind.
# Beide Werte in den Stack.
# kann GC auslösen
  local void F_F_floor_I_F (object x, object y);
  GEN_F_F_round(floor)

#if 0 # unbenutzt

# F_F_ceiling_I_F(x,y) liefert (ceiling x y), wo x und y Floats sind.
# Beide Werte in den Stack.
# kann GC auslösen
  local void F_F_ceiling_I_F (object x, object y);
  GEN_F_F_round(ceiling)

# F_F_truncate_I_F(x,y) liefert (truncate x y), wo x und y Floats sind.
# Beide Werte in den Stack.
# kann GC auslösen
  local void F_F_truncate_I_F (object x, object y);
  GEN_F_F_round(truncate)

# F_F_round_I_F(x,y) liefert (round x y), wo x und y Floats sind.
# Beide Werte in den Stack.
# kann GC auslösen
  local void F_F_round_I_F (object x, object y);
  GEN_F_F_round(round)

#endif


# F_to_SF(x) wandelt ein Float x in ein Short-Float um und rundet dabei.
  local object F_to_SF (object x);
  local object F_to_SF(x)
    var reg1 object x;
    { floatcase(x,
                { return x; },
                { return FF_to_SF(x); },
                { return DF_to_SF(x); },
                { return LF_to_SF(x); }
               );
    }

# F_to_FF(x) wandelt ein Float x in ein Single-Float um und rundet dabei.
# kann GC auslösen
  local object F_to_FF (object x);
  local object F_to_FF(x)
    var reg1 object x;
    { floatcase(x,
                { return SF_to_FF(x); },
                { return x; },
                { return DF_to_FF(x); },
                { return LF_to_FF(x); }
               );
    }

# F_to_DF(x) wandelt ein Float x in ein Double-Float um und rundet dabei.
# kann GC auslösen
  local object F_to_DF (object x);
  local object F_to_DF(x)
    var reg1 object x;
    { floatcase(x,
                { return SF_to_DF(x); },
                { return FF_to_DF(x); },
                { return x; },
                { return LF_to_DF(x); }
               );
    }

# F_to_LF(x,len) wandelt ein Float x in ein Long-Float mit len Digits um
# und rundet dabei.
# > uintC len: gewünschte Anzahl Digits, >=LF_minlen
# kann GC auslösen
  local object F_to_LF (object x, uintC len);
  local object F_to_LF(x,len)
    var reg1 object x;
    var reg2 uintC len;
    { floatcase(x,
                { return SF_to_LF(x,len); },
                { return FF_to_LF(x,len); },
                { return DF_to_LF(x,len); },
                { return LF_to_LF(x,len); }
               );
    }

# F_F_float_F(x,y) wandelt ein Float x in das Float-Format des Floats y um
# und rundet dabei nötigenfalls.
# > x,y: Floats
# < ergebnis: (float x y)
# kann GC auslösen
  local object F_F_float_F (object x, object y);
  local object F_F_float_F(x,y)
    var reg2 object x;
    var reg1 object y;
    { floatcase(y,
                { return F_to_SF(x); },
                { return F_to_FF(x); },
                { return F_to_DF(x); },
                { return F_to_LF(x,TheLfloat(y)->len); }
               );
    }


# Vergrößert eine Long-Float-Länge n, so daß aus d = intDsize*n
# mindestens d+sqrt(d)+2 wird.
# Methode bei intDsize=16:
# n -> n+1 für n<=12 wegen 16n+sqrt(16n)+2 < 16(n+1)
# n -> n+2 für n<=56 wegen 16n+sqrt(16n)+2 < 16(n+2)
# n -> n+4 für n<=240
# n -> n+8 für n<=992
# n -> n+16 für n<=4032
# n -> n+32 für n<=16256
# n -> n+65 für n<=65535
# Allgemein: intDsize*n + sqrt(intDsize*n) + 2 < intDsize*(n+inc)
# <==>       sqrt(intDsize*n) + 2 < intDsize*inc
# <==>       sqrt(intDsize*n) < intDsize*inc - 2
# <==>       intDsize*n < intDsize^2*inc^2 - 4*intDsize*inc + 4
# <==>       n <= intDsize*inc^2 - 4*inc
  local uintC lf_len_extend (uintC n);
  local uintC lf_len_extend(n)
    var reg1 uintC n;
    { var reg2 uintC inc =
        #define FITS(n,k)  ((n) <= (uintL)((intDsize*(k)-4)*(k)))
        #define n_max  (uintL)(bitm(intCsize)-1)
        #define TEST(i)  FITS(n_max,1UL<<i) || FITS(n,1UL<<i) ? 1UL<<i :
        TEST(0) TEST(1) TEST(2) TEST(3) TEST(4) TEST(5) TEST(6) TEST(7)
        TEST(8) TEST(9) TEST(10) TEST(11) TEST(12) TEST(13)
        (fehler_LF_toolong(),0);
        #undef TEST
        #undef n_max
        #undef FITS
      if ((n = n+inc) < inc) { fehler_LF_toolong(); }
      return n;
    }

# F_extend_F(x) erweitert die Genauigkeit eines Floats x um eine Stufe
# SF -> FF -> DF -> LF(4) -> LF(5) -> LF(6) -> ...
# Ein Float mit d Mantissenbits wird so zu einem Float mit
# mindestens d+sqrt(d)+2 Mantissenbits.
# SF -> FF wegen 17+sqrt(17)+2 = 23.2 < 24
# FF -> DF wegen 24+sqrt(24)+2 = 30.9 < 53
# DF -> LF(4) wegen 53+sqrt(53)+2 = 62.3 < 64
# LF(n) -> LF(n+1) für n<=12 wegen 16n+sqrt(16n)+2 < 16(n+1)
# LF(n) -> LF(n+2) für n<=56 wegen 16n+sqrt(16n)+2 < 16(n+2)
# LF(n) -> LF(n+4) für n<=240
# LF(n) -> LF(n+8) für n<=992
# LF(n) -> LF(n+16) für n<=4032
# LF(n) -> LF(n+32) für n<=16256
# LF(n) -> LF(n+65) für n<=65535
# kann GC auslösen
  local object F_extend_F (object x);
  local object F_extend_F(x)
    var reg1 object x;
    { floatcase(x,
                { return (SF_mant_len+1<=17 ? SF_to_FF(x) # 17+sqrt(17)+2 = 23.2 < 24
                                            : SF_to_DF(x) # 24+sqrt(24)+2 = 30.9 < 53
                         );
                },
                { return FF_to_DF(x); }, # 24+sqrt(24)+2 = 30.9 < 53
                { return DF_to_LF(x,ceiling(63,intDsize)); }, # 53+sqrt(53)+2 = 62.3 < 63
                { return LF_extend_LF(x,lf_len_extend(TheLfloat(x)->len)); }
               );
    }


# F_decode_float_F_I_F(x) liefert zu einem Float x:
# (decode-float x), alle drei Werte in den Stack.
# x = 0.0 liefert (0.0, 0, 1.0).
# x = (-1)^s * 2^e * m liefert ((-1)^0 * 2^0 * m, e als Integer, (-1)^s).
# kann GC auslösen
  local void F_decode_float_F_I_F (object x);
  local void F_decode_float_F_I_F(x)
    var reg1 object x;
    { floatcase(x,
      /* x SF */ { # x entpacken:
                   var reg4 signean sign;
                   var reg3 sintWL exp;
                   var reg2 uint32 mant;
                   SF_decode(x, { pushSTACK(SF_0); pushSTACK(Fixnum_0); pushSTACK(SF_1); return; },
                                sign=,exp=,mant=
                            );
                   encode_SF(0,0,mant, x=); pushSTACK(x); # (-1)^0 * 2^0 * m erzeugen
                   pushSTACK(L_to_FN((sintL)exp)); # e als Fixnum
                   encode_SF(sign,1,bit(SF_mant_len), x=); pushSTACK(x); # (-1)^s erzeugen
                   return;
                 },
      /* x FF */ { # x entpacken:
                   var reg4 signean sign;
                   var reg3 sintWL exp;
                   var reg2 uint32 mant;
                   FF_decode(x, { pushSTACK(FF_0); pushSTACK(Fixnum_0); pushSTACK(FF_1); return; },
                                sign=,exp=,mant=
                            );
                   encode_FF(0,0,mant, x=); pushSTACK(x); # (-1)^0 * 2^0 * m erzeugen
                   pushSTACK(L_to_FN((sintL)exp)); # e als Fixnum
                   encode_FF(sign,1,bit(FF_mant_len), x=); pushSTACK(x); # (-1)^s erzeugen
                   return;
                 },
      /* x DF */ { # x entpacken:
                   var reg4 signean sign;
                   var reg3 sintWL exp;
                   var reg2 uint32 manthi;
                   var reg2 uint32 mantlo;
                   DF_decode(x, { pushSTACK(DF_0); pushSTACK(Fixnum_0); pushSTACK(DF_1); return; },
                                sign=,exp=,manthi=,mantlo=
                            );
                   encode_DF(0,0,manthi,mantlo, x=); pushSTACK(x); # (-1)^0 * 2^0 * m erzeugen
                   pushSTACK(L_to_FN((sintL)exp)); # e als Fixnum
                   encode_DF(sign,1,bit(DF_mant_len-32),0, x=); pushSTACK(x); # (-1)^s erzeugen
                   return;
                 },
      /* x LF */ { # x entpacken:
                   var reg4 signean sign;
                   var reg3 sintL exp;
                   var reg2 uintC mantlen;
                   LF_decode(x, { pushSTACK(x); # 0.0
                                  pushSTACK(Fixnum_0); # 0
                                  encode_LF1(mantlen, x=); pushSTACK(x); # 1.0
                                  return;
                                },
                             sign=,exp=,,mantlen=,);
                   pushSTACK(x); # x retten
                   x = allocate_lfloat(mantlen,0+LF_exp_mid,0); # (-1)^0 * 2^0 * m erzeugen
                   copy_loop_up(&TheLfloat(STACK_0)->data[0],&TheLfloat(x)->data[0],mantlen); # m hineinkopieren
                   STACK_0 = x; # 1. Wert fertig
                   pushSTACK(L_to_I(exp)); # e als Fixnum
                   encode_LF1s(sign,mantlen, x=); pushSTACK(x); # (-1)^s erzeugen
                   return;
                 }
               );
    }

# F_exponent_L(x) liefert zu einem Float x:
# den Exponenten von (decode-float x).
# x = 0.0 liefert 0.
# x = (-1)^s * 2^e * m liefert e.
  local sintL F_exponent_L (object x);
  local sintL F_exponent_L(x)
    var reg1 object x;
    { floatcase(x,
      /* x SF */ { var reg2 uintBWL uexp = SF_uexp(x);
                   if (uexp==0) { return 0; }
                   return (sintL)(sintWL)((uintWL)uexp - SF_exp_mid);
                 },
      /* x FF */ { var reg2 uintBWL uexp = FF_uexp(ffloat_value(x));
                   if (uexp==0) { return 0; }
                   return (sintL)(sintWL)((uintWL)uexp - FF_exp_mid);
                 },
      /* x DF */ { var reg2 uintWL uexp = DF_uexp(TheDfloat(x)->float_value.semhi);
                   if (uexp==0) { return 0; }
                   return (sintL)(sintWL)(uexp - DF_exp_mid);
                 },
      /* x LF */ { var reg2 uintL uexp = TheLfloat(x)->expo;
                   if (uexp==0) { return 0; }
                   return (sintL)(uexp - LF_exp_mid);
                 }
               );
    }

# SF_I_scale_float_SF(x,delta) liefert x*2^delta, wo x ein SF ist.
  local object SF_I_scale_float_SF (object x, object delta);
  # Methode:
  # x=0.0 -> x als Ergebnis
  # delta muß ein Fixnum betragsmäßig <= SF_exp_high-SF_exp_low sein.
  # Neues SF mit um delta vergrößertem Exponenten bilden.
  local object SF_I_scale_float_SF(x,delta)
    var reg1 object x;
    var reg2 object delta;
    { # x entpacken:
      var reg5 signean sign;
      var reg4 sintWL exp;
      var reg6 uint32 mant;
      SF_decode(x, { return x; }, sign=,exp=,mant=);
      if (!R_minusp(delta))
        # delta>=0
        { var reg3 uintL udelta;
          if (I_fixnump(delta)
              && ((udelta = posfixnum_to_L(delta)) <= (uintL)(SF_exp_high-SF_exp_low))
             )
            { exp = exp+udelta;
              encode_SF(sign,exp,mant, return);
            }
            else
            { fehler_overflow(); }
        }
        else
        # delta<0
        { var reg3 uintL udelta;
          if (I_fixnump(delta)
              && ((udelta = negfixnum_abs_L(delta)) <= (uintL)(SF_exp_high-SF_exp_low))
              && ((oint_addr_len<intLsize) || !(udelta==0))
             )
            { exp = exp-udelta;
              encode_SF(sign,exp,mant, return);
            }
            else
            { fehler_underflow(); }
        }
    }

# FF_I_scale_float_FF(x,delta) liefert x*2^delta, wo x ein FF ist.
# kann GC auslösen
  local object FF_I_scale_float_FF (object x, object delta);
  # Methode:
  # x=0.0 -> x als Ergebnis
  # delta muß ein Fixnum betragsmäßig <= FF_exp_high-FF_exp_low sein.
  # Neues FF mit um delta vergrößertem Exponenten bilden.
  local object FF_I_scale_float_FF(x,delta)
    var reg1 object x;
    var reg2 object delta;
    { # x entpacken:
      var reg5 signean sign;
      var reg4 sintWL exp;
      var reg6 uint32 mant;
      FF_decode(x, { return x; }, sign=,exp=,mant=);
      if (!R_minusp(delta))
        # delta>=0
        { var reg3 uintL udelta;
          if (I_fixnump(delta)
              && ((udelta = posfixnum_to_L(delta)) <= (uintL)(FF_exp_high-FF_exp_low))
             )
            { exp = exp+udelta;
              encode_FF(sign,exp,mant, return);
            }
            else
            { fehler_overflow(); }
        }
        else
        # delta<0
        { var reg3 uintL udelta;
          if (I_fixnump(delta)
              && ((udelta = negfixnum_abs_L(delta)) <= (uintL)(FF_exp_high-FF_exp_low))
              && ((oint_addr_len<intLsize) || !(udelta==0))
             )
            { exp = exp-udelta;
              encode_FF(sign,exp,mant, return);
            }
            else
            { fehler_underflow(); }
        }
    }

# DF_I_scale_float_DF(x,delta) liefert x*2^delta, wo x ein DF ist.
# kann GC auslösen
  local object DF_I_scale_float_DF (object x, object delta);
  # Methode:
  # x=0.0 -> x als Ergebnis
  # delta muß ein Fixnum betragsmäßig <= DF_exp_high-DF_exp_low sein.
  # Neues DF mit um delta vergrößertem Exponenten bilden.
  local object DF_I_scale_float_DF(x,delta)
    var reg1 object x;
    var reg2 object delta;
    { # x entpacken:
      var reg5 signean sign;
      var reg4 sintWL exp;
      var reg6 uint32 manthi;
      var reg7 uint32 mantlo;
      DF_decode(x, { return x; }, sign=,exp=,manthi=,mantlo=);
      if (!R_minusp(delta))
        # delta>=0
        { var reg3 uintL udelta;
          if (I_fixnump(delta)
              && ((udelta = posfixnum_to_L(delta)) <= (uintL)(DF_exp_high-DF_exp_low))
             )
            { exp = exp+udelta;
              encode_DF(sign,exp,manthi,mantlo, return);
            }
            else
            { fehler_overflow(); }
        }
        else
        # delta<0
        { var reg3 uintL udelta;
          if (I_fixnump(delta)
              && ((udelta = negfixnum_abs_L(delta)) <= (uintL)(DF_exp_high-DF_exp_low))
              && ((oint_addr_len<intLsize) || !(udelta==0))
             )
            { exp = exp-udelta;
              encode_DF(sign,exp,manthi,mantlo, return);
            }
            else
            { fehler_underflow(); }
        }
    }

# LF_I_scale_float_LF(x,delta) liefert x*2^delta, wo x ein LF ist.
# kann GC auslösen
  local object LF_I_scale_float_LF (object x, object delta);
  # Methode:
  # delta=0 -> x als Ergebnis
  # x=0.0 -> x als Ergebnis
  # delta muß ein Fixnum betragsmäßig <= LF_exp_high-LF_exp_low sein.
  # Neues LF mit um delta vergrößertem Exponenten bilden.
  local object LF_I_scale_float_LF(x,delta)
    var reg3 object x;
    var reg4 object delta;
    { if (eq(delta,Fixnum_0)) { return x; } # delta=0 -> x als Ergebnis
     {var reg5 uintL uexp = TheLfloat(x)->expo;
      if (uexp==0) { return x; }
      pushSTACK(x); # x retten
      { var reg2 uintL udelta;
        # |delta| muß <= LF_exp_high-LF_exp_low < 2^32 sein. Wie bei I_to_UL:
        switch (typecode(delta))
          { case_posfixnum: # Fixnum >=0
              udelta = posfixnum_to_L(delta); goto pos;
            case_posbignum: # Bignum >0
              { var reg1 Bignum bn = TheBignum(delta);
                #define IF_LENGTH(i)  \
                  if (bn_minlength <= i) # genau i Digits überhaupt möglich?       \
                    if (bn->length == i) # genau i Digits?                         \
                      # 2^((i-1)*intDsize-1) <= obj < 2^(i*intDsize-1)             \
                      if ( (i*intDsize-1 > 32)                                     \
                           && ( ((i-1)*intDsize-1 >= 32)                           \
                                || (bn->data[0] >= (uintD)bitc(32-(i-1)*intDsize)) \
                         )    )                                                    \
                        goto overflow;                                             \
                        else
                IF_LENGTH(1)
                  { udelta = get_uint1D_Dptr(bn->data); goto pos; }
                IF_LENGTH(2)
                  { udelta = get_uint2D_Dptr(bn->data); goto pos; }
                IF_LENGTH(3)
                  { udelta = get_uint3D_Dptr(bn->data); goto pos; }
                IF_LENGTH(4)
                  { udelta = get_uint4D_Dptr(bn->data); goto pos; }
                IF_LENGTH(5)
                  { udelta = get_uint4D_Dptr(bn->data); goto pos; }
                #undef IF_LENGTH
              }
              goto overflow; # delta zu groß
            case_negfixnum: # Fixnum <0
              udelta = negfixnum_to_L(delta); goto neg;
            case_negbignum: # Bignum <0
              { var reg1 Bignum bn = TheBignum(delta);
                #define IF_LENGTH(i)  \
                  if (bn_minlength <= i) # genau i Digits überhaupt möglich?         \
                    if (bn->length == i) # genau i Digits?                           \
                      # - 2^((i-1)*intDsize-1) > obj >= - 2^(i*intDsize-1)           \
                      if ( (i*intDsize-1 > 32)                                       \
                           && ( ((i-1)*intDsize-1 >= 32)                             \
                                || (bn->data[0] < (uintD)(-bitc(32-(i-1)*intDsize))) \
                         )    )                                                      \
                        goto underflow;                                              \
                        else
                IF_LENGTH(1)
                  { udelta = get_sint1D_Dptr(bn->data); goto neg; }
                IF_LENGTH(2)
                  { udelta = get_sint2D_Dptr(bn->data); goto neg; }
                IF_LENGTH(3)
                  { udelta = get_sint3D_Dptr(bn->data); goto neg; }
                IF_LENGTH(4)
                  { udelta = get_sint4D_Dptr(bn->data); goto neg; }
                IF_LENGTH(5)
                  { udelta = get_sint4D_Dptr(bn->data); goto neg; }
                #undef IF_LENGTH
              }
              goto underflow; # delta zu klein
            pos: # udelta = delta >=0
              if (   ((uexp = uexp+udelta) < udelta) # Exponent-Überlauf?
                  || (uexp > LF_exp_high) # oder Exponent zu groß?
                 )
                { fehler_overflow(); } # ja -> Überlauf
              break; # sonst OK
            neg: # delta <0, udelta = 2^32+delta
              if (   ((uexp = uexp+udelta) >= udelta) # oder Exponent-Unterlauf?
                  || (uexp < LF_exp_low) # oder Exponent zu klein?
                 )
                { fehler_underflow(); } # ja -> Unterlauf
              break; # sonst OK
            default: # unpassender Integer
              if (!R_minusp(delta))
                { overflow: fehler_overflow(); } # delta zu groß
                else
                { underflow: fehler_underflow(); } # delta zu klein
          }
       {var reg1 uintC mantlen = TheLfloat(x)->len;
        x = allocate_lfloat(mantlen,uexp,R_sign(x)); # neues Long-Float
        copy_loop_up(&TheLfloat(popSTACK())->data[0],&TheLfloat(x)->data[0],mantlen); # füllen
        return x;
    }}}}

# F_I_scale_float_F(x,delta) liefert x*2^delta, wo x ein Float ist.
# kann GC auslösen
  local object F_I_scale_float_F (object x, object delta);
  local object F_I_scale_float_F(x,delta)
    var reg1 object x;
    var reg2 object delta;
    { floatcase(x,
                { return SF_I_scale_float_SF(x,delta); },
                { return FF_I_scale_float_FF(x,delta); },
                { return DF_I_scale_float_DF(x,delta); },
                { return LF_I_scale_float_LF(x,delta); }
               );
    }

# F_float_radix_I(x) liefert (float-radix x), wo x ein Float ist.
  local object F_float_radix_I (object x);
#if 0
  local object F_float_radix_I(x)
    var reg1 object x;
    { return fixnum(2); } # stets 2 als Ergebnis
#else # Macro spart Code
  #define F_float_radix_I(obj)  ((obj), fixnum(2)) # stets 2 als Ergebnis
#endif

# F_float_sign_F(x) liefert (float-sign x), wo x ein Float ist.
# kann GC auslösen
  local object F_float_sign_F (object x);
  # Methode: x>=0 -> Ergebnis 1.0; x<0 -> Ergebnis -1.0
  local object F_float_sign_F(x)
    var reg1 object x;
    { floatcase(x,
      /* x SF */ { encode_SF(R_sign(x),1,bit(SF_mant_len), return); },
      /* x FF */ # { encode_FF(R_sign(x),1,bit(FF_mant_len), return); }, # besser:
                 { return (!R_minusp(x) ? FF_1 : FF_minus1); },
      /* x DF */ # { encode_DF(R_sign(x),1,bit(DF_mant_len-32),0, return); }, # besser:
                 { return (!R_minusp(x) ? DF_1 : DF_minus1); },
      /* x LF */ { encode_LF1s(R_sign(x),TheLfloat(x)->len, return); }
               );
    }

# F_F_float_sign_F(x) liefert (float-sign x y), wo x und y Floats sind.
# kann GC auslösen
  local object F_F_float_sign_F (object x, object y);
  # Methode:
  # Falls x<0 xor y<0, Ergebnis (- y), sonst Ergebnis y.
  local object F_F_float_sign_F(x,y)
    var reg2 object x;
    var reg1 object y;
    { return (wbit_test((oint)x ^ (oint)y, vorz_bit_o) ? F_minus_F(y) : y); }

# F_float_digits(x) liefert (float-digits x), wo x ein Float ist.
# < ergebnis: ein uintL >0
  local uintL F_float_digits (object x);
  local uintL F_float_digits(x)
    var reg1 object x;
    { floatcase(x,
                { return SF_mant_len+1; }, # 17
                { return FF_mant_len+1; }, # 24
                { return DF_mant_len+1; }, # 53
                { return intDsize*(uintL)(TheLfloat(x)->len); } # 16n
               );
    }

# F_float_digits_I(x) liefert (float-digits x), wo x ein Float ist.
# < ergebnis: ein Integer >0
# kann GC auslösen
  local object F_float_digits_I (object x);
  local object F_float_digits_I(x)
    var reg1 object x;
    { floatcase(x,
                { return fixnum(SF_mant_len+1); }, # Fixnum 17
                { return fixnum(FF_mant_len+1); }, # Fixnum 24
                { return fixnum(DF_mant_len+1); }, # Fixnum 53
                { var reg2 uintL bitcount = intDsize*(uintL)(TheLfloat(x)->len); # 16n
                  return (log2_intDsize+intCsize<=oint_addr_len # intDsize*2^intCsize <= 2^oint_addr_len ?
                          ? fixnum(bitcount)
                          : UL_to_I(bitcount)
                         );
                }
               );
    }

# F_float_precision_I(x) liefert (float-precision x), wo x ein Float ist.
# < ergebnis: ein Integer >=0
# kann GC auslösen
  local object F_float_precision_I (object x);
  # Methode: Falls x=0.0, Ergebnis 0, sonst (float-digits x).
  local object F_float_precision_I(x)
    var reg1 object x;
    { floatcase(x,
                { if (SF_zerop(x)) { return Fixnum_0; }
                  return fixnum(SF_mant_len+1); # Fixnum 17
                },
                { if (FF_zerop(x)) { return Fixnum_0; }
                  return fixnum(FF_mant_len+1); # Fixnum 24
                },
                { if (DF_zerop(x)) { return Fixnum_0; }
                  return fixnum(DF_mant_len+1); # Fixnum 53
                },
                { if (LF_zerop(x)) { return Fixnum_0; }
                 {var reg2 uintL bitcount = intDsize*(uintL)(TheLfloat(x)->len); # 16n
                  return (log2_intDsize+intCsize<=oint_addr_len # intDsize*2^intCsize <= 2^oint_addr_len ?
                          ? fixnum(bitcount)
                          : UL_to_I(bitcount)
                         );
                }}
               );
    }

# F_integer_decode_float_I_I_I(x) liefert zu einem Float x:
# (integer-decode-float x), alle drei Werte in den Stack.
# x = 0.0 liefert (0, 0, 1).
# x = (-1)^s * 2^e * m bei Float-Precision p liefert
#   (Mantisse 2^p * m als Integer, e-p als Integer, (-1)^s als Fixnum).
# kann GC auslösen
  local void F_integer_decode_float_I_I_I (object x);
  local void F_integer_decode_float_I_I_I(x)
    var reg1 object x;
    { floatcase(x,
      /* x SF */ { # x entpacken:
                   var reg3 sintWL exp;
                   var reg2 uint32 mant;
                   SF_decode(x, { goto zero; }, ,exp=,mant=);
                   pushSTACK(fixnum(mant)); # Mantisse als Fixnum (>0, <2^17)
                   pushSTACK(L_to_FN((sintL)(exp-(SF_mant_len+1)))); # e-17 als Fixnum
                 },
      /* x FF */ { # x entpacken:
                   var reg3 sintWL exp;
                   var reg2 uint32 mant;
                   FF_decode(x, { goto zero; }, ,exp=,mant=);
                   pushSTACK( # Mantisse (>0, <2^24) als Integer
                              (FF_mant_len+1 <= oint_addr_len
                               ? fixnum(mant) # Mantisse als Fixnum
                               : UL_to_I(mant) # oder evtl. als Bignum
                            ) );
                   pushSTACK(L_to_FN((sintL)(exp-(FF_mant_len+1)))); # e-24 als Fixnum
                 },
      /* x DF */ { # x entpacken:
                   var reg3 sintWL exp;
                   var reg2 uint32 manthi;
                   var reg2 uint32 mantlo;
                   DF_decode(x, { goto zero; }, ,exp=,manthi=,mantlo=);
                   pushSTACK(L2_to_I(manthi,mantlo)); # Mantisse (>0, <2^53) als Bignum
                   pushSTACK(L_to_FN((sintL)(exp-(DF_mant_len+1)))); # e-53 als Fixnum
                 },
      /* x LF */ { var reg6 uintL uexp = TheLfloat(x)->expo;
                   if (uexp == 0) goto zero;
                   pushSTACK(x); # x retten
                  {var reg3 uintC len = TheLfloat(x)->len; # Anzahl Mantissendigits
                   var reg4 uintC len1 = len+1; # brauche 1 Digit mehr
                   if (uintCoverflow(len1)) { fehler_LF_toolong(); }
                   # intDsize*len >= 53 >= 33 >= oint_addr_len+1, also len >= bn_minlength.
                   {var reg5 object mant = allocate_bignum(len1,0); # Integer für Mantisse
                    var reg2 uintD* mantptr = &TheBignum(mant)->data[0];
                    *mantptr++ = 0; # vorne 1 Nulldigit, damit es eine NDS wird
                    copy_loop_up(&TheLfloat(STACK_0)->data[0],mantptr,len); # NUDS kopieren
                    STACK_0 = mant; # 1. Wert fertig
                   }
                   # e-16n = uexp-LF_exp_mid-16n als Integer bilden:
                   {var reg2 uintL sub = LF_exp_mid + intDsize*(uintL)len;
                    pushSTACK(L2_to_I( (uexp<sub ? -1L : 0), uexp-sub));
                 }}}
               );
      pushSTACK(!R_minusp(x) ? Fixnum_1 : Fixnum_minus1); # Vorzeichen von x (nicht GC-gefährdet!)
      return;
      zero: pushSTACK(Fixnum_0); pushSTACK(Fixnum_0); pushSTACK(Fixnum_1); return;
    }

