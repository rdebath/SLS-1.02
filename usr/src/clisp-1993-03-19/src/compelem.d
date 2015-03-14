# Elementare Funktionen für komplexe Zahlen

# Liefert zu reellen Zahlen a und b /= Fixnum 0 die komplexe Zahl a+bi.
# R_R_complex_C(a,b)
# kann GC auslösen
  #define R_R_complex_C  make_complex

# Liefert zu reellen Zahlen a und b die komplexe Zahl a+bi.
# R_R_complex_N(a,b)
# kann GC auslösen
  local object R_R_complex_N (object a, object b);
# Methode:
# Falls b=0, nur a. sonst komplexe Zahl erzeugen.
  local object R_R_complex_N(a,b)
    var reg2 object a;
    var reg1 object b;
    { return (eq(b,Fixnum_0) ? a : R_R_complex_C(a,b)); }

# N_realpart_R(x) liefert den Realteil der Zahl x.
  local object N_realpart_R (object x);
#if 0
  local object N_realpart_R(x)
    var reg1 object x;
    { return (N_realp(x) ? x : TheComplex(x)->c_real); }
#else # Macro spart Code
  #define N_realpart_R(x)  (N_realp(x) ? x : TheComplex(x)->c_real)
#endif

# N_imagpart_R(x) liefert den Imaginärteil der Zahl x.
  local object N_imagpart_R (object x);
#if 0
  local object N_imagpart_R(x)
    var reg1 object x;
    { return (N_realp(x) ? Fixnum_0 : TheComplex(x)->c_imag); }
#else # Macro spart Code
  #define N_imagpart_R(x)  (N_realp(x) ? Fixnum_0 : TheComplex(x)->c_imag)
#endif

# N_conjugate_N(x) liefert die konjugiert komplexe Zahl zur Zahl x.
# kann GC auslösen
  local object N_conjugate_N (object x);
  local object N_conjugate_N(x)
    var reg1 object x;
    { if (N_realp(x))
        { return x; }
        else
        { pushSTACK(TheComplex(x)->c_real);
         {var reg2 object b = TheComplex(x)->c_imag;
          b = R_minus_R(b); # Vorzeichenwechsel beim Imaginärteil
          return R_R_complex_N(popSTACK(),b);
    }   }}

# N_minus_N(x) liefert (- x), wo x eine Zahl ist.
# kann GC auslösen
  local object N_minus_N (object x);
# Methode:
# x reell -> klar.
# x=a+bi -> (-a) + (-b) i
  local object N_minus_N(x)
    var reg1 object x;
    { if (N_realp(x))
        { return R_minus_R(x); }
        else
        { pushSTACK(TheComplex(x)->c_real);
         {var reg2 object b = R_minus_R(TheComplex(x)->c_imag);
          var reg3 object a = STACK_0; STACK_0 = b;
          a = R_minus_R(a);
          return R_R_complex_C(a,popSTACK());
    }   }}

# N_N_plus_N(x) liefert (+ x y), wo x und y Zahlen sind.
# kann GC auslösen
  local object N_N_plus_N (object x, object y);
# Methode:
# x,y beide reell -> klar.
# x=a, y=b+ci -> (a+b)+ci
# x=a+bi, y=c -> (a+c)+bi
# x=a+bi, y=c+di -> (a+c)+(b+d)i
  local object N_N_plus_N(x,y)
    var reg2 object x;
    var reg1 object y;
    { if (N_realp(x))
        { if (N_realp(y))
            { return R_R_plus_R(x,y); }
            else
            # x=a, y=b+ci
            { pushSTACK(TheComplex(y)->c_imag); # c
             {var reg3 object re = R_R_plus_R(x,TheComplex(y)->c_real); # a+b
              return R_R_complex_C(re,popSTACK());
            }}
        }
        else
        { if (N_realp(y))
            # x=a+bi, y=c
            { pushSTACK(TheComplex(x)->c_imag); # b
             {var reg3 object re = R_R_plus_R(TheComplex(x)->c_real,y); # a+c
              return R_R_complex_C(re,popSTACK());
            }}
            else
            # x=a+bi, y=c+di
            { pushSTACK(TheComplex(x)->c_real); # a
              pushSTACK(TheComplex(y)->c_real); # c
             {var reg3 object temp = # b+d
                R_R_plus_R(TheComplex(x)->c_imag,TheComplex(y)->c_imag);
              y = STACK_0; STACK_0 = temp;
              temp = R_R_plus_R(STACK_1,y); # a+c
              temp = R_R_complex_N(temp,STACK_0);
              skipSTACK(2); return temp;
            }}
    }   }

# N_N_minus_N(x) liefert (- x y), wo x und y Zahlen sind.
# kann GC auslösen
  local object N_N_minus_N (object x, object y);
# Methode:
# x,y beide reell -> klar.
# x=a, y=b+ci -> (a-b)+(-c)i
# x=a+bi, y=c -> (a-c)+bi
# x=a+bi, y=c+di -> (a-c)+(b-d)i
  local object N_N_minus_N(x,y)
    var reg2 object x;
    var reg1 object y;
    { if (N_realp(x))
        { if (N_realp(y))
            { return R_R_minus_R(x,y); }
            else
            # x=a, y=b+ci
            { pushSTACK(TheComplex(y)->c_imag); # c
             {var reg3 object temp = R_R_minus_R(x,TheComplex(y)->c_real); # a-b
              y = STACK_0; STACK_0 = temp;
              y = R_minus_R(y); # -c
              return R_R_complex_C(popSTACK(),y);
            }}
        }
        else
        { if (N_realp(y))
            # x=a+bi, y=c
            { pushSTACK(TheComplex(x)->c_imag); # b
             {var reg3 object re = R_R_minus_R(TheComplex(x)->c_real,y); # a-c
              return R_R_complex_C(re,popSTACK());
            }}
            else
            # x=a+bi, y=c+di
            { pushSTACK(TheComplex(x)->c_real); # a
              pushSTACK(TheComplex(y)->c_real); # c
             {var reg3 object temp = # b-d
                R_R_minus_R(TheComplex(x)->c_imag,TheComplex(y)->c_imag);
              y = STACK_0; STACK_0 = temp;
              temp = R_R_minus_R(STACK_1,y); # a-c
              temp = R_R_complex_N(temp,STACK_0);
              skipSTACK(2); return temp;
            }}
    }   }

# N_1_plus_N(x) liefert (1+ x), wo x eine Zahl ist.
# kann GC auslösen
  local object N_1_plus_N (object x);
# Methode:
# x reell -> klar.
# x=a+bi -> (a+1)+bi
  local object N_1_plus_N(x)
    var reg1 object x;
    { if (N_realp(x))
        { return R_1_plus_R(x); }
        else
        { pushSTACK(TheComplex(x)->c_imag);
         {var reg2 object re = R_1_plus_R(TheComplex(x)->c_real); # a+1
          return R_R_complex_C(re,popSTACK());
    }   }}

# N_minus1_plus_N(x) liefert (1- x), wo x eine Zahl ist.
# kann GC auslösen
  local object N_minus1_plus_N (object x);
# Methode:
# x reell -> klar.
# x=a+bi -> (a-1)+bi
  local object N_minus1_plus_N(x)
    var reg1 object x;
    { if (N_realp(x))
        { return R_minus1_plus_R(x); }
        else
        { pushSTACK(TheComplex(x)->c_imag);
         {var reg2 object re = R_minus1_plus_R(TheComplex(x)->c_real); # a-1
          return R_R_complex_C(re,popSTACK());
    }   }}

# N_N_mal_N(x) liefert (* x y), wo x und y Zahlen sind.
# kann GC auslösen
  local object N_N_mal_N (object x, object y);
# Methode:
# x,y beide reell -> klar.
# x=a, y=b+ci -> (a*b)+(a*c)i
# x=a+bi, y=c -> (a*c)+(b*c)i
# x=a+bi, y=c+di -> (a*c-b*d)+(a*d+b*c)i
  local object N_N_mal_N(x,y)
    var reg2 object x;
    var reg1 object y;
    { if (N_realp(x))
        { if (N_realp(y))
            { return R_R_mal_R(x,y); }
            else
            # x=a, y=b+ci
            { pushSTACK(x); # a
              pushSTACK(TheComplex(y)->c_real); # b
             {var reg3 object temp = R_R_mal_R(x,TheComplex(y)->c_imag); # a*c
              y = popSTACK(); x = STACK_0; STACK_0 = temp;
              y = R_R_mal_R(x,y); # a*b
              return R_R_complex_N(y,popSTACK());
            }}
        }
        else
        { if (N_realp(y))
            # x=a+bi, y=c
            { pushSTACK(y); # c
              pushSTACK(TheComplex(x)->c_real); # a
             {var reg3 object temp = R_R_mal_R(TheComplex(x)->c_imag,y); # b*c
              x = popSTACK(); y = STACK_0; STACK_0 = temp;
              x = R_R_mal_R(x,y); # a*c
              return R_R_complex_N(x,popSTACK());
            }}
            else
            # x=a+bi, y=c+di -> (a*c-b*d)+(a*d+b*c)i
            { pushSTACK(TheComplex(x)->c_real); # a
              pushSTACK(TheComplex(y)->c_real); # c
              pushSTACK(TheComplex(x)->c_imag); # b
              pushSTACK(y = TheComplex(y)->c_imag); # d
              # Stackaufbau: a, c, b, d.
              y = R_R_mal_R(STACK_3,y); # a*d
              x = STACK_3; STACK_3 = y;
              # Stackaufbau: a*d, c, b, d.
              x = R_R_mal_R(x,STACK_2); # a*c
              y = STACK_2; STACK_2 = x;
              # Stackaufbau: a*d, a*c, b, d.
              y = R_R_mal_R(STACK_1,y); # b*c
              STACK_3 = R_R_plus_R(STACK_3,y); # a*d+b*c
              # Stackaufbau: a*d+b*c, a*c, b, d.
              y = R_R_mal_R(STACK_1,STACK_0); # b*d
              y = R_R_minus_R(STACK_2,y); # a*c-b*d
              x = STACK_3; # a*d+b*c
              skipSTACK(4);
              return R_R_complex_N(y,x);
            }
    }   }

# N_zerop(x) stellt fest, ob (= x 0), wo x eine Zahl ist.
  local boolean N_zerop (object x);
  local boolean N_zerop(x)
    var reg1 object x;
    { if (N_realp(x))
        { return R_zerop(x); }
        else
        # x komplex, teste ob Real- und Imaginärteil beide = 0 sind.
        { return (R_zerop(TheComplex(x)->c_real) && R_zerop(TheComplex(x)->c_imag)); }
    }

# N_durch_N(x) liefert (/ x), wo x eine Zahl ist.
# kann GC auslösen
  local object N_durch_N (object x);
# Methode:
# Falls x reell, klar.
# Falls x=a+bi:
#    Falls a=0: 0+(-1/b)i.
#    Falls a und b beide rational sind:
#      c:=a*a+b*b, c:=1/c, liefere a*c+(-b*c)i.
#    Falls a oder b Floats sind:
#      Falls einer von beiden rational ist, runde ihn zum selben Float-Typ
#        wie der andere und führe das UP durch.
#      Falls beide Floats sind, erweitere auf den genaueren, führe das UP
#        durch und runde wieder auf den ungenaueren.
#      Das Ergebnis ist eine komplexe Zahl, da beide Komponenten Floats sind.
# UP: [a,b Floats vom selben Typ]
#  a=0.0 -> liefere die Komponenten a=0.0 und -1/b.
#  b=0.0 -> liefere die Komponenten 1/a und b=0.0.
#  e:=max(exponent(a),exponent(b)).
#  a':=a/2^e bzw. 0.0 bei Underflowmöglichkeit (beim Skalieren a':=a/2^e
#      oder beim Quadrieren a'*a':  2*(e-exponent(a))>exp_mid-exp_low-1
#      d.h. exponent(b)-exponent(a)>floor((exp_mid-exp_low-1)/2) ).
#  b':=b/2^e bzw. 0.0 bei Underflowmöglichkeit (beim Skalieren b':=b/2^e
#      oder beim Quadrieren b'*b':  2*(e-exponent(b))>exp_mid-exp_low-1
#      d.h. exponent(a)-exponent(b)>floor((exp_mid-exp_low-1)/2) ).
#  c':=a'*a'+b'*b',
#  liefere die beiden Komponenten 2^(-e)*a'/c' und -2^(-e)*b'/c'.
  local void SFC_durch_SFC (object a, object b);
  local void FFC_durch_FFC (object a, object b);
  local void DFC_durch_DFC (object a, object b);
  local void LFC_durch_LFC (object a, object b);
  local void SFC_durch_SFC(a,b)
    var reg1 object a;
    var reg2 object b;
    { var reg5 sintWL a_exp;
      var reg6 sintWL b_exp;
      {# Exponenten von a holen:
       var reg3 uintBWL uexp = SF_uexp(a);
       if (uexp==0) # a=0.0 -> liefere (complex a (- (/ b))) :
         { pushSTACK(a); pushSTACK(SF_minus_SF(SF_durch_SF(b))); return; }
       a_exp = (sintWL)((uintWL)uexp - SF_exp_mid);
      }
      {# Exponenten von b holen:
       var reg3 uintBWL uexp = SF_uexp(b);
       if (uexp==0) # b=0.0 -> liefere (complex (/ a) b) :
         { pushSTACK(SF_durch_SF(a)); pushSTACK(b); return; }
       b_exp = (sintWL)((uintWL)uexp - SF_exp_mid);
      }
       # Nun a_exp = exponent(a), b_exp = exponent(b).
      {var reg4 sintWL e = (a_exp > b_exp ? a_exp : b_exp); # Maximum der Exponenten
       var reg3 object delta = L_to_FN(-(sintL)e); # -e als Fixnum
       # a und b durch 2^e dividieren:
       a = (b_exp-a_exp > floor(SF_exp_mid-SF_exp_low-1,2) ? SF_0 : SF_I_scale_float_SF(a,delta));
       b = (a_exp-b_exp > floor(SF_exp_mid-SF_exp_low-1,2) ? SF_0 : SF_I_scale_float_SF(b,delta));
       # c' := a'*a'+b'*b' berechnen:
       {var reg4 object c = SF_SF_plus_SF(SF_SF_mal_SF(a,a),SF_SF_mal_SF(b,b));
        a = SF_I_scale_float_SF(SF_SF_durch_SF(a,c),delta); # 2^(-e)*a'/c'
        b = SF_I_scale_float_SF(SF_minus_SF(SF_SF_durch_SF(b,c)),delta); # -2^(-e)*b'/c'
        pushSTACK(a); pushSTACK(b); return;
    } }}
  local void FFC_durch_FFC(a,b)
    var reg1 object a;
    var reg2 object b;
    { var reg5 sintWL a_exp;
      var reg6 sintWL b_exp;
      {# Exponenten von a holen:
       var reg3 uintBWL uexp = FF_uexp(ffloat_value(a));
       if (uexp==0) # a=0.0 -> liefere (complex a (- (/ b))) :
         { pushSTACK(a); pushSTACK(FF_minus_FF(FF_durch_FF(b))); return; }
       a_exp = (sintWL)((uintWL)uexp - FF_exp_mid);
      }
      {# Exponenten von b holen:
       var reg3 uintBWL uexp = FF_uexp(ffloat_value(b));
       if (uexp==0) # b=0.0 -> liefere (complex (/ a) b) :
         { pushSTACK(FF_durch_FF(a)); pushSTACK(FF_0); return; }
       b_exp = (sintWL)((uintWL)uexp - FF_exp_mid);
      }
       pushSTACK(a); pushSTACK(b);
       # Stackaufbau: a, b.
       # Nun a_exp = exponent(a), b_exp = exponent(b).
      {var reg4 sintWL e = (a_exp > b_exp ? a_exp : b_exp); # Maximum der Exponenten
       var reg3 object delta = L_to_FN(-(sintL)e); # -e als Fixnum
       # a und b durch 2^e dividieren:
       STACK_1 = (b_exp-a_exp > floor(FF_exp_mid-FF_exp_low-1,2) ? FF_0 : FF_I_scale_float_FF(STACK_1,delta));
       STACK_0 = (a_exp-b_exp > floor(FF_exp_mid-FF_exp_low-1,2) ? FF_0 : FF_I_scale_float_FF(STACK_0,delta));
       # Stackaufbau: a', b'.
       # c' := a'*a'+b'*b' berechnen:
       {var reg4 object temp;
        temp = STACK_1; pushSTACK(FF_FF_mal_FF(temp,temp)); # a'*a'
        temp = STACK_1; temp = FF_FF_mal_FF(temp,temp); # b'*b'
        STACK_0 = temp = FF_FF_plus_FF(STACK_0,temp); # c' = a'*a'+b'*b'
        STACK_2 = FF_I_scale_float_FF(FF_FF_durch_FF(STACK_2,temp),delta); # 2^(-e)*a'/c'
        STACK_1 = FF_I_scale_float_FF(FF_minus_FF(FF_FF_durch_FF(STACK_1,STACK_0)),delta); # -2^(-e)*b'/c'
        skipSTACK(1); return;
    } }}
  local void DFC_durch_DFC(a,b)
    var reg1 object a;
    var reg2 object b;
    { var reg5 sintWL a_exp;
      var reg6 sintWL b_exp;
      {# Exponenten von a holen:
       var reg3 uintWL uexp = DF_uexp(TheDfloat(a)->float_value.semhi);
       if (uexp==0) # a=0.0 -> liefere (complex a (- (/ b))) :
         { pushSTACK(a); pushSTACK(DF_minus_DF(DF_durch_DF(b))); return; }
       a_exp = (sintWL)(uexp - DF_exp_mid);
      }
      {# Exponenten von b holen:
       var reg3 uintWL uexp = DF_uexp(TheDfloat(b)->float_value.semhi);
       if (uexp==0) # b=0.0 -> liefere (complex (/ a) b) :
         { pushSTACK(DF_durch_DF(a)); pushSTACK(DF_0); return; }
       b_exp = (sintWL)(uexp - DF_exp_mid);
      }
       pushSTACK(a); pushSTACK(b);
       # Stackaufbau: a, b.
       # Nun a_exp = exponent(a), b_exp = exponent(b).
      {var reg4 sintWL e = (a_exp > b_exp ? a_exp : b_exp); # Maximum der Exponenten
       var reg3 object delta = L_to_FN(-(sintL)e); # -e als Fixnum
       # a und b durch 2^e dividieren:
       STACK_1 = (b_exp-a_exp > floor(DF_exp_mid-DF_exp_low-1,2) ? DF_0 : DF_I_scale_float_DF(STACK_1,delta));
       STACK_0 = (a_exp-b_exp > floor(DF_exp_mid-DF_exp_low-1,2) ? DF_0 : DF_I_scale_float_DF(STACK_0,delta));
       # Stackaufbau: a', b'.
       # c' := a'*a'+b'*b' berechnen:
       {var reg4 object temp;
        temp = STACK_1; pushSTACK(DF_DF_mal_DF(temp,temp)); # a'*a'
        temp = STACK_1; temp = DF_DF_mal_DF(temp,temp); # b'*b'
        STACK_0 = temp = DF_DF_plus_DF(STACK_0,temp); # c' = a'*a'+b'*b'
        STACK_2 = DF_I_scale_float_DF(DF_DF_durch_DF(STACK_2,temp),delta); # 2^(-e)*a'/c'
        STACK_1 = DF_I_scale_float_DF(DF_minus_DF(DF_DF_durch_DF(STACK_1,STACK_0)),delta); # -2^(-e)*b'/c'
        skipSTACK(1); return;
    } }}
  local void LFC_durch_LFC(a,b)
    var reg1 object a;
    var reg2 object b;
    { var reg4 uintL a_exp;
      var reg5 uintL b_exp;
      {# Exponenten von a holen:
       a_exp = TheLfloat(a)->expo;
       if (a_exp==0) # a=0.0 -> liefere (complex a (- (/ b))) :
         { pushSTACK(a); pushSTACK(LF_minus_LF(LF_durch_LF(b))); return; }
      }
      {# Exponenten von b holen:
       b_exp = TheLfloat(b)->expo;
       if (b_exp==0) # b=0.0 -> liefere (complex (/ a) b) :
         { pushSTACK(b); pushSTACK(b); STACK_1 = LF_durch_LF(a); return; }
      }
      pushSTACK(a); pushSTACK(b);
      # Stackaufbau: a, b.
      # Nun a_exp = exponent(a)+LF_exp_mid, b_exp = exponent(b)+LF_exp_mid.
      {var reg3 uintL e = (a_exp > b_exp ? a_exp : b_exp); # Maximum der Exponenten
       pushSTACK(L_to_I(-(sintL)(e-LF_exp_mid))); # -e als Integer
      }
      # a und b durch 2^e dividieren:
      if ((b_exp>a_exp) && (b_exp-a_exp > (uintL)floor((uintL)(LF_exp_mid-LF_exp_low-1),2)))
        { encode_LF0(TheLfloat(STACK_2)->len, STACK_2=); }
        else
        { STACK_2 = LF_I_scale_float_LF(STACK_2,STACK_0); }
      if ((a_exp>b_exp) && (a_exp-b_exp > (uintL)floor((uintL)(LF_exp_mid-LF_exp_low-1),2)))
        { encode_LF0(TheLfloat(STACK_1)->len, STACK_1=); }
        else
        { STACK_1 = LF_I_scale_float_LF(STACK_1,STACK_0); }
      # Stackaufbau: a', b', -e.
      # c' := a'*a'+b'*b' berechnen:
      {var reg3 object temp;
       temp = STACK_2; pushSTACK(LF_LF_mal_LF(temp,temp)); # a'*a'
       temp = STACK_2; temp = LF_LF_mal_LF(temp,temp); # b'*b'
       STACK_0 = temp = LF_LF_plus_LF(STACK_0,temp); # c' = a'*a'+b'*b'
       temp = LF_LF_durch_LF(STACK_3,temp); STACK_3 = LF_I_scale_float_LF(temp,STACK_1); # 2^(-e)*a'/c'
       temp = LF_minus_LF(LF_LF_durch_LF(STACK_2,STACK_0)); STACK_2 = LF_I_scale_float_LF(temp,STACK_1); # -2^(-e)*b'/c'
       skipSTACK(2); return;
    } }
  local object N_durch_N(x)
    var reg2 object x;
    { if (N_realp(x))
        { return R_durch_R(x); }
        else
        { var reg3 object a = TheComplex(x)->c_real;
          var reg4 object b = TheComplex(x)->c_imag;
          # x = a+bi
          if (R_rationalp(a))
            { if (eq(a,Fixnum_0))
                { return R_R_complex_C(Fixnum_0,R_minus_R(R_durch_R(b))); } # (complex 0 (- (/ b)))
              if (R_rationalp(b))
                # a,b beide rational
                { var reg1 object temp;
                  pushSTACK(a); pushSTACK(b);
                  temp = RA_RA_mal_RA(a,a); pushSTACK(temp); # a*a
                  temp = STACK_1; temp = RA_RA_mal_RA(temp,temp); # b*b
                  temp = RA_RA_plus_RA(STACK_0,temp); # a*a+b*b = c
                  STACK_0 = temp = RA_durch_RA(temp); # c:=1/c
                  STACK_2 = RA_RA_mal_RA(STACK_2,temp); # a*c
                  STACK_1 = RA_minus_RA(RA_RA_mal_RA(STACK_1,STACK_0)); # -b*c
                  temp = R_R_complex_C(STACK_2,STACK_1); # (a*c) + (-b*c) i
                  skipSTACK(3);
                  return temp;
                }
                else
                # a rational, b Float
                { pushSTACK(b);
                  floatcase(b,
                            { a = RA_to_SF(a); SFC_durch_SFC(a,popSTACK()); },
                            { a = RA_to_FF(a); FFC_durch_FFC(a,popSTACK()); },
                            { a = RA_to_DF(a); DFC_durch_DFC(a,popSTACK()); },
                            { a = RA_to_LF(a,TheLfloat(b)->len); LFC_durch_LFC(a,popSTACK()); }
                           );
                }
            }
            else
            { if (R_rationalp(b))
                # a Float, b rational
                { pushSTACK(a);
                  floatcase(a,
                            { b = RA_to_SF(b); SFC_durch_SFC(popSTACK(),b); },
                            { b = RA_to_FF(b); FFC_durch_FFC(popSTACK(),b); },
                            { b = RA_to_DF(b); DFC_durch_DFC(popSTACK(),b); },
                            { b = RA_to_LF(b,TheLfloat(a)->len); LFC_durch_LFC(popSTACK(),b); }
                           );
                }
                else
                # a,b Floats
                GEN_F_op2(a,b,SFC_durch_SFC,FFC_durch_FFC,DFC_durch_DFC,LFC_durch_LFC,2,0,)
            }
          # beide Komponenten zu einer komplexen Zahl zusammenfügen:
         {var reg2 object a = STACK_1;
          var reg1 object b = STACK_0;
          skipSTACK(2); return R_R_complex_C(a,b);
        }}
    }
  #define C_durch_C  N_durch_N

# N_N_durch_N(x) liefert (/ x y), wo x und y Zahlen sind.
# kann GC auslösen
  local object N_N_mal_N (object x, object y);
# Methode:
# x,y beide reell -> klar.
# x=a+bi, y=c reell -> (a/c)+(b/c)i
# y komplex -> (* x (/ y))
  local object N_N_durch_N(x,y)
    var reg2 object x;
    var reg1 object y;
    { if (N_realp(y))
        # y reell
        { if (N_realp(x))
            # x,y beide reell
            { return R_R_durch_R(x,y); }
            else
            # x komplex: x=a+bi, y=c
            { pushSTACK(y); # c
              pushSTACK(TheComplex(x)->c_real); # a
             {var reg3 object temp = R_R_durch_R(TheComplex(x)->c_imag,y); # b/c
              x = popSTACK(); y = STACK_0; STACK_0 = temp;
              x = R_R_durch_R(x,y); # a/c
              return R_R_complex_N(x,popSTACK());
            }}
        }
        else
        # y komplex
        { pushSTACK(x); y = C_durch_C(y); # mit dem Kehrwert von y
          return N_N_mal_N(popSTACK(),y); # multiplizieren
        }
    }

# R_R_hypot_R(a,b) liefert sqrt(a^2+b^2), wo a und b reelle Zahlen sind.
# kann GC auslösen
  local object R_R_hypot_R (object a, object b);
# Methode:
# Falls a=0: (abs b).
# Falls b=0: (abs a).
# Falls a und b beide rational sind:
#   c:=a*a+b*b, liefere (sqrt c).
# Falls a oder b Floats sind:
#   Falls einer von beiden rational ist, runde ihn zum selben Float-Typ
#     wie der andere und führe das UP durch.
#   Falls beide Floats sind, erweitere auf den genaueren, führe das UP
#     durch und runde wieder auf den ungenaueren.
#   Das Ergebnis ist ein Float >=0.
# UP: [a,b Floats vom selben Typ]
#  a=0.0 -> liefere abs(b).
#  b=0.0 -> liefere abs(a).
#  e:=max(exponent(a),exponent(b)).
#  a':=a/2^e bzw. 0.0 bei Underflowmöglichkeit (beim Skalieren a':=a/2^e
#      oder beim Quadrieren a'*a':  2*(e-exponent(a))>exp_mid-exp_low-1
#      d.h. exponent(b)-exponent(a)>floor((exp_mid-exp_low-1)/2) ).
#  b':=b/2^e bzw. 0.0 bei Underflowmöglichkeit (beim Skalieren b':=b/2^e
#      oder beim Quadrieren b'*b':  2*(e-exponent(b))>exp_mid-exp_low-1
#      d.h. exponent(a)-exponent(b)>floor((exp_mid-exp_low-1)/2) ).
#  c':=a'*a'+b'*b', c':=sqrt(c'), liefere 2^e*c'.
  local object SF_SF_hypot_SF (object a, object b);
  local object FF_FF_hypot_FF (object a, object b);
  local object DF_DF_hypot_DF (object a, object b);
  local object LF_LF_hypot_LF (object a, object b);
  local object SF_SF_hypot_SF(a,b)
    var reg1 object a;
    var reg2 object b;
    { var reg5 sintWL a_exp;
      var reg6 sintWL b_exp;
      {# Exponenten von a holen:
       var reg3 uintBWL uexp = SF_uexp(a);
       if (uexp==0) # a=0.0 -> liefere (abs b) :
         { return (R_minusp(b) ? SF_minus_SF(b) : b); }
       a_exp = (sintWL)((uintWL)uexp - SF_exp_mid);
      }
      {# Exponenten von b holen:
       var reg3 uintBWL uexp = SF_uexp(b);
       if (uexp==0) # b=0.0 -> liefere (abs a) :
         { return (R_minusp(a) ? SF_minus_SF(a) : a); }
       b_exp = (sintWL)((uintWL)uexp - SF_exp_mid);
      }
       # Nun a_exp = exponent(a), b_exp = exponent(b).
      {var reg4 sintWL e = (a_exp > b_exp ? a_exp : b_exp); # Maximum der Exponenten
       var reg3 object delta = L_to_FN(-(sintL)e); # -e als Fixnum
       # a und b durch 2^e dividieren:
       a = (b_exp-a_exp > floor(SF_exp_mid-SF_exp_low-1,2) ? SF_0 : SF_I_scale_float_SF(a,delta));
       b = (a_exp-b_exp > floor(SF_exp_mid-SF_exp_low-1,2) ? SF_0 : SF_I_scale_float_SF(b,delta));
       # c' := a'*a'+b'*b' berechnen:
       {var reg4 object c = SF_SF_plus_SF(SF_SF_mal_SF(a,a),SF_SF_mal_SF(b,b));
        c = SF_sqrt_SF(c); # c':=2^e*c'
        return SF_I_scale_float_SF(c,L_to_FN((sintL)e)); # 2^e*c'
    } }}
  local object FF_FF_hypot_FF(a,b)
    var reg1 object a;
    var reg2 object b;
    { var reg5 sintWL a_exp;
      var reg6 sintWL b_exp;
      {# Exponenten von a holen:
       var reg3 uintBWL uexp = FF_uexp(ffloat_value(a));
       if (uexp==0) # a=0.0 -> liefere (abs b) :
         { return (R_minusp(b) ? FF_minus_FF(b) : b); }
       a_exp = (sintWL)((uintWL)uexp - FF_exp_mid);
      }
      {# Exponenten von b holen:
       var reg3 uintBWL uexp = FF_uexp(ffloat_value(b));
       if (uexp==0) # b=0.0 -> liefere (abs a) :
         { return (R_minusp(a) ? FF_minus_FF(a) : a); }
       b_exp = (sintWL)((uintWL)uexp - FF_exp_mid);
      }
       pushSTACK(a); pushSTACK(b);
       # Stackaufbau: a, b.
       # Nun a_exp = exponent(a), b_exp = exponent(b).
      {var reg4 sintWL e = (a_exp > b_exp ? a_exp : b_exp); # Maximum der Exponenten
       var reg3 object delta = L_to_FN(-(sintL)e); # -e als Fixnum
       # a und b durch 2^e dividieren:
       STACK_1 = (b_exp-a_exp > floor(FF_exp_mid-FF_exp_low-1,2) ? FF_0 : FF_I_scale_float_FF(STACK_1,delta));
       STACK_0 = (a_exp-b_exp > floor(FF_exp_mid-FF_exp_low-1,2) ? FF_0 : FF_I_scale_float_FF(STACK_0,delta));
       # Stackaufbau: a', b'.
       # c' := a'*a'+b'*b' berechnen:
       {var reg4 object temp;
        temp = STACK_1; pushSTACK(FF_FF_mal_FF(temp,temp)); # a'*a'
        temp = STACK_1; temp = FF_FF_mal_FF(temp,temp); # b'*b'
        temp = FF_FF_plus_FF(STACK_0,temp); # c' = a'*a'+b'*b'
        skipSTACK(3);
        temp = FF_sqrt_FF(temp); # c':=2^e*c'
        return FF_I_scale_float_FF(temp,L_to_FN((sintL)e)); # 2^e*c'
    } }}
  local object DF_DF_hypot_DF(a,b)
    var reg1 object a;
    var reg2 object b;
    { var reg5 sintWL a_exp;
      var reg6 sintWL b_exp;
      {# Exponenten von a holen:
       var reg3 uintWL uexp = DF_uexp(TheDfloat(a)->float_value.semhi);
       if (uexp==0) # a=0.0 -> liefere (abs b) :
         { return (R_minusp(b) ? DF_minus_DF(b) : b); }
       a_exp = (sintWL)(uexp - DF_exp_mid);
      }
      {# Exponenten von b holen:
       var reg3 uintWL uexp = DF_uexp(TheDfloat(b)->float_value.semhi);
       if (uexp==0) # b=0.0 -> liefere (abs a) :
         { return (R_minusp(a) ? DF_minus_DF(a) : a); }
       b_exp = (sintWL)(uexp - DF_exp_mid);
      }
       pushSTACK(a); pushSTACK(b);
       # Stackaufbau: a, b.
       # Nun a_exp = exponent(a), b_exp = exponent(b).
      {var reg4 sintWL e = (a_exp > b_exp ? a_exp : b_exp); # Maximum der Exponenten
       var reg3 object delta = L_to_FN(-(sintL)e); # -e als Fixnum
       # a und b durch 2^e dividieren:
       STACK_1 = (b_exp-a_exp > floor(DF_exp_mid-DF_exp_low-1,2) ? DF_0 : DF_I_scale_float_DF(STACK_1,delta));
       STACK_0 = (a_exp-b_exp > floor(DF_exp_mid-DF_exp_low-1,2) ? DF_0 : DF_I_scale_float_DF(STACK_0,delta));
       # Stackaufbau: a', b'.
       # c' := a'*a'+b'*b' berechnen:
       {var reg4 object temp;
        temp = STACK_1; pushSTACK(DF_DF_mal_DF(temp,temp)); # a'*a'
        temp = STACK_1; temp = DF_DF_mal_DF(temp,temp); # b'*b'
        temp = DF_DF_plus_DF(STACK_0,temp); # c' = a'*a'+b'*b'
        skipSTACK(3);
        temp = DF_sqrt_DF(temp); # c':=2^e*c'
        return DF_I_scale_float_DF(temp,L_to_FN((sintL)e)); # 2^e*c'
    } }}
  local object LF_LF_hypot_LF(a,b)
    var reg1 object a;
    var reg2 object b;
    { var reg4 uintL a_exp;
      var reg5 uintL b_exp;
      {# Exponenten von a holen:
       a_exp = TheLfloat(a)->expo;
       if (a_exp==0) # a=0.0 -> liefere (abs b) :
         { return (R_minusp(b) ? LF_minus_LF(b) : b); }
      }
      {# Exponenten von b holen:
       b_exp = TheLfloat(b)->expo;
       if (b_exp==0) # b=0.0 -> liefere (abs a) :
         { return (R_minusp(a) ? LF_minus_LF(a) : a); }
      }
      pushSTACK(a); pushSTACK(b);
      # Stackaufbau: a, b.
      # Nun a_exp = exponent(a)+LF_exp_mid, b_exp = exponent(b)+LF_exp_mid.
      {var reg6 uintL exp = (a_exp > b_exp ? a_exp : b_exp); # Maximum der Exponenten
       var reg3 sintL e = (sintL)(exp-LF_exp_mid);
       pushSTACK(L_to_I(e)); # e als Integer
       pushSTACK(L_to_I(-e)); # -e als Integer
      }
      # a und b durch 2^e dividieren:
      if ((b_exp>a_exp) && (b_exp-a_exp > (uintL)floor((uintL)(LF_exp_mid-LF_exp_low-1),2)))
        { encode_LF0(TheLfloat(STACK_3)->len, STACK_3=); }
        else
        { STACK_3 = LF_I_scale_float_LF(STACK_3,STACK_0); }
      if ((a_exp>b_exp) && (a_exp-b_exp > (uintL)floor((uintL)(LF_exp_mid-LF_exp_low-1),2)))
        { encode_LF0(TheLfloat(STACK_2)->len, STACK_2=); }
        else
        { STACK_2 = LF_I_scale_float_LF(STACK_2,STACK_0); }
      # Stackaufbau: a', b', e, -e.
      # c' := a'*a'+b'*b' berechnen:
      {var reg3 object temp;
       temp = STACK_3; pushSTACK(LF_LF_mal_LF(temp,temp)); # a'*a'
       temp = STACK_3; temp = LF_LF_mal_LF(temp,temp); # b'*b'
       temp = LF_LF_plus_LF(STACK_0,temp); # c' = a'*a'+b'*b'
       temp = LF_sqrt_LF(temp); # c':=2^e*c'
       temp = LF_I_scale_float_LF(temp,STACK_2); # 2^e*c'
       skipSTACK(5); return temp;
    } }
  local object R_R_hypot_R(a,b)
    var reg3 object a;
    var reg2 object b;
    { if (R_rationalp(a))
        { if (eq(a,Fixnum_0)) { return R_abs_R(b); } # a=0 -> (abs b)
          if (R_rationalp(b))
            # a,b beide rational
            { var reg1 object temp;
              if (eq(b,Fixnum_0)) { return R_abs_R(a); } # b=0 -> (abs a)
              pushSTACK(a); pushSTACK(b);
              temp = RA_RA_mal_RA(a,a); pushSTACK(temp); # a*a
              temp = STACK_1; temp = RA_RA_mal_RA(temp,temp); # b*b
              temp = RA_RA_plus_RA(STACK_0,temp); # a*a+b*b = c
              skipSTACK(3);
              return RA_sqrt_R(temp); # (sqrt c)
            }
            else
            # a rational, b Float
            { pushSTACK(b);
              floatcase(b,
                        { a = RA_to_SF(a); return SF_SF_hypot_SF(a,popSTACK()); },
                        { a = RA_to_FF(a); return FF_FF_hypot_FF(a,popSTACK()); },
                        { a = RA_to_DF(a); return DF_DF_hypot_DF(a,popSTACK()); },
                        { a = RA_to_LF(a,TheLfloat(b)->len); return LF_LF_hypot_LF(a,popSTACK()); }
                       );
            }
        }
        else
        { if (R_rationalp(b))
            # a Float, b rational
            { if (eq(b,Fixnum_0)) { return R_abs_R(a); } # b=0 -> (abs a)
              pushSTACK(a);
              floatcase(a,
                        { b = RA_to_SF(b); return SF_SF_hypot_SF(popSTACK(),b); },
                        { b = RA_to_FF(b); return FF_FF_hypot_FF(popSTACK(),b); },
                        { b = RA_to_DF(b); return DF_DF_hypot_DF(popSTACK(),b); },
                        { b = RA_to_LF(b,TheLfloat(a)->len); return LF_LF_hypot_LF(popSTACK(),b); }
                       );
            }
            else
            # a,b Floats
            GEN_F_op2(a,b,SF_SF_hypot_SF,FF_FF_hypot_FF,DF_DF_hypot_DF,LF_LF_hypot_LF,1,0,return)
        }
    }

# N_abs_R(x) liefert (abs x), wo x eine Zahl ist.
# kann GC auslösen
  local object N_abs_R (object x);
# Methode:
# Falls x reell: klar
# Falls x=a+bi: sqrt(a^2+b^2)
  local object N_abs_R(x)
    var reg1 object x;
    { if (N_realp(x))
        { return R_abs_R(x); }
        else
        { return R_R_hypot_R(TheComplex(x)->c_real,TheComplex(x)->c_imag); }
    }
  #define C_abs_R(x)  R_R_hypot_R(TheComplex(x)->c_real,TheComplex(x)->c_imag)

# N_signum_N(x) liefert (signum x), wo x eine Zahl ist.
# kann GC auslösen
  local object N_signum_N (object x);
# Methode:
# x reell -> klar.
# x komplex -> falls (zerop x), x als Ergebnis, sonst (/ x (abs x)).
  local object N_signum_N(x)
    var reg1 object x;
    { if (N_realp(x))
        { return R_signum_R(x); }
        else
        { if (N_zerop(x)) return x;
          pushSTACK(x); x = C_abs_R(x); # (abs x) errechnen
          return N_N_durch_N(popSTACK(),x); # (/ x (abs x))
    }   }

# N_sqrt_N(x) liefert (sqrt x), wo x eine beliebige Zahl ist.
# kann GC auslösen
  local object N_sqrt_N (object x);
# Methode:
# x reell -> Für x>=0 klar, für x<0: sqrt(-x)*i.
# x=a+bi ->
#   Bestimme r=abs(x)=sqrt(a*a+b*b).
#   Falls a>=0: Setze c:=sqrt((r+a)/2), d:=(b/(2*c) falls c>0, c falls c=0).
#   Falls a<0: Setze d:=sqrt((r-a)/2)*(1 falls b>=0, -1 falls b<0), c:=b/(2*d).
#   Damit ist c>=0, 2*c*d=b, c*c=(r+a)/2, d*d=(r-a)/2, c*c-d*d=a, c*c+d*d=r,
#   also c+di die gesuchte Wurzel.
  local object N_sqrt_N(x)
    var reg2 object x;
    { if (N_realp(x))
        # x reell
        { if (!R_minusp(x))
            { return R_sqrt_R(x); }
            else
            { return R_R_complex_C(Fixnum_0,R_sqrt_R(R_minus_R(x))); }
        }
        else
        # x komplex
        { var reg5 object a = TheComplex(x)->c_real;
          var reg6 object b = TheComplex(x)->c_imag;
          pushSTACK(b); pushSTACK(a);
         {var reg4 object r = R_R_hypot_R(a,b); # r = (abs x)
          var reg3 object a = STACK_0;
          if (!R_minusp(a))
            # a>=0
            { var reg1 object c = # sqrt((r+a)/2)
                R_sqrt_R(R_R_durch_R(R_R_plus_R(r,a),fixnum(2)));
              STACK_0 = c;
              if (!R_zerop(c))
                { c = R_R_mal_R(fixnum(2),c); # 2*c
                  c = R_R_durch_R(STACK_1,c); # d:=b/(2*c)
                }
              c = R_R_complex_C(STACK_0,c); # c+di
              skipSTACK(2); return c; # als Ergebnis
            }
            else
            # a<0
            { var reg1 object d = # sqrt((r-a)/2)
                R_sqrt_R(R_R_durch_R(R_R_minus_R(r,a),fixnum(2)));
              if (R_mminusp(STACK_1)) { d = R_minus_R(d); } # bei b<0 Vorzeichenwechsel
              STACK_0 = d;
              d = R_R_mal_R(fixnum(2),d); # 2*d
              d = R_R_durch_R(STACK_1,d); # c:=b/(2*d)
              d = R_R_complex_C(d,STACK_0); # c+di
              skipSTACK(2); return d; # als Ergebnis
            }
        }}
    }

# N_N_gleich(x,y) vergleicht zwei reelle Zahlen x und y.
# Ergebnis: TRUE falls x=y, FALSE sonst.
# kann GC auslösen
  global /* local */ boolean N_N_gleich (object x, object y);
# Methode:
# Falls beide reell, klar.
# Falls x reell, y komplex: (= x (realpart y)) und (zerop (imagpart y)).
# Falls x komplex, y reell: analog
# Falls beide komplex: Realteile und Imaginärteile jeweils gleich?
  global /* local */ boolean N_N_gleich(x,y)
    var reg1 object x;
    var reg2 object y;
    { if (N_realp(x))
        # x reell
        { if (N_realp(y))
            # x,y beide reell
            { return (R_R_comp(x,y)==0 ? TRUE : FALSE); }
            else
            # x reell, y komplex
            { if (!R_zerop(TheComplex(y)->c_imag)) return FALSE;
              return (R_R_comp(x,TheComplex(y)->c_real)==0 ? TRUE : FALSE);
            }
        }
        else
        # x komplex
        { if (N_realp(y))
            # x komplex, y reell
            { if (!R_zerop(TheComplex(x)->c_imag)) return FALSE;
              return (R_R_comp(TheComplex(x)->c_real,y)==0 ? TRUE : FALSE);
            }
            else
            # x,y beide komplex
            { pushSTACK(TheComplex(x)->c_imag); pushSTACK(TheComplex(y)->c_imag);
              x = TheComplex(x)->c_real; y = TheComplex(y)->c_real;
              if (!(R_R_comp(x,y)==0)) { skipSTACK(2); return FALSE; } # Realteile vergleichen
              y = popSTACK(); x = popSTACK();
              if (!(R_R_comp(x,y)==0)) return FALSE; # Imaginärteile vergleichen
              return TRUE;
            }
        }
    }

