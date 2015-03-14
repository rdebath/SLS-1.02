# Elementare Grundfunktionen beim Arbeiten mit Integers

# Umwandlungsroutinen Digit-Sequence-Teil <--> Longword:

# get_32_Dptr(ptr)
#   holt die nächsten 32 Bits aus den 32/intDsize Digits ab ptr.
# set_32_Dptr(ptr,wert);
#   speichert den Wert wert (32 Bits) in die 32/intDsize Digits ab ptr.
# get_max32_Dptr(count,ptr)
#   holt die nächsten count Bits aus den ceiling(count/intDsize) Digits ab ptr.
# set_max32_Dptr(count,ptr,wert)
#   speichert wert (count Bits) in die ceiling(count/intDsize) Digits ab ptr.
# Jeweils ptr eine Variable vom Typ uintD*,
#         wert eine Variable vom Typ uint32,
#         count eine Variable oder constant-expression mit Wert >=0, <=32.
  #if (intDsize==32)
    #define get_32_Dptr(ptr)  ((uint32)((ptr)[0]))
    #define set_32_Dptr(ptr,wert)  ((ptr)[0] = (uintD)(wert))
    #define get_max32_Dptr(count,ptr)  \
      ((count)==0 ? 0 :                \
                    (uint32)((ptr)[0]) \
      )
    #define set_max32_Dptr(count,ptr,wert)  \
      ((count)==0 ? 0 :                        \
                    ((ptr)[0] = (uintD)(wert)) \
      )
  #endif
  #if (intDsize==16)
    # define get_32_Dptr(ptr)  (((uint32)((ptr)[0])<<16) | ((uint32)((ptr)[1])))
    #define get_32_Dptr(ptr)  highlow32_at(ptr)
    # define set_32_Dptr(ptr,wert)  ((ptr)[0] = (uintD)((wert)>>16), (ptr)[1] = (uintD)(wert))
    #define set_32_Dptr(ptr,wert)  set_highlow32_at(ptr,wert)
    #define get_max32_Dptr(count,ptr)  \
      ((count)==0 ? 0 :                   \
       (count)<=16 ? (uint32)((ptr)[0]) : \
                     highlow32_at(ptr)    \
      )
    #define set_max32_Dptr(count,ptr,wert)  \
      ((count)==0 ? 0 :                           \
       (count)<=16 ? ((ptr)[0] = (uintD)(wert)) : \
                     set_highlow32_at(ptr,wert)   \
      )
  #endif
  #if (intDsize==8)
    #define get_32_Dptr(ptr)  (((((( (uint32)((ptr)[0]) <<8) | (uint32)((ptr)[1])) <<8) | (uint32)((ptr)[2])) <<8) | (uint32)((ptr)[3]))
    #define set_32_Dptr(ptr,wert)  ((ptr)[0] = (uintD)((wert)>>24), (ptr)[1] = (uintD)((wert)>>16), (ptr)[2] = (uintD)((wert)>>8), (ptr)[3] = (uintD)(wert))
    #define get_max32_Dptr(count,ptr)  \
      ((count)==0 ? 0 : \
       (count)<=8 ? (uint32)((ptr)[0]) : \
       (count)<=16 ? (( (uint32)((ptr)[0]) <<8) | (uint32)((ptr)[1])) : \
       (count)<=24 ? (((( (uint32)((ptr)[0]) <<8) | (uint32)((ptr)[1])) <<8) | (uint32)((ptr)[2])) : \
                     (((((( (uint32)((ptr)[0]) <<8) | (uint32)((ptr)[1])) <<8) | (uint32)((ptr)[2])) <<8) | (uint32)((ptr)[3])) \
      )
    #define set_max32_Dptr(count,ptr,wert)  \
      ((count)==0 ? 0 : \
       (count)<=8 ? ((ptr)[0] = (uintD)(wert)) : \
       (count)<=16 ? ((ptr)[0] = (uintD)((wert)>>8), (ptr)[1] = (uintD)(wert)) : \
       (count)<=24 ? ((ptr)[0] = (uintD)((wert)>>16), (ptr)[1] = (uintD)((wert)>>8), (ptr)[2] = (uintD)(wert)) : \
                     ((ptr)[0] = (uintD)((wert)>>24), (ptr)[1] = (uintD)((wert)>>16), (ptr)[2] = (uintD)((wert)>>8), (ptr)[3] = (uintD)(wert)) \
      )
  #endif

# get_uint1D_Dptr(ptr)  holt 1 Digit (unsigned) ab ptr
# get_uint2D_Dptr(ptr)  holt 2 Digits (unsigned) ab ptr
# get_uint3D_Dptr(ptr)  holt 3 Digits (unsigned) ab ptr
# get_uint4D_Dptr(ptr)  holt 4 Digits (unsigned) ab ptr
# get_sint1D_Dptr(ptr)  holt 1 Digit (signed) ab ptr
# get_sint2D_Dptr(ptr)  holt 2 Digits (signed) ab ptr
# get_sint3D_Dptr(ptr)  holt 3 Digits (signed) ab ptr
# get_sint4D_Dptr(ptr)  holt 4 Digits (signed) ab ptr
# Jeweils ptr eine Variable vom Typ uintD*.
  #define get_uint1D_Dptr(ptr)  ((uint32)((ptr)[0]))
  #define get_uint2D_Dptr(ptr)  (((uint32)((ptr)[0]) << intDsize) | (uint32)((ptr)[1]))
  #define get_uint3D_Dptr(ptr)  (((((uint32)((ptr)[0]) << intDsize) | (uint32)((ptr)[1])) << intDsize) | (uint32)((ptr)[2]))
  #define get_uint4D_Dptr(ptr)  (((((((uint32)((ptr)[0]) << intDsize) | (uint32)((ptr)[1])) << intDsize) | (uint32)((ptr)[2])) << intDsize) | (uint32)((ptr)[3]))
  #define get_sint1D_Dptr(ptr)  ((sint32)(sintD)((ptr)[0]))
  #define get_sint2D_Dptr(ptr)  (((sint32)(sintD)((ptr)[0]) << intDsize) | (uint32)((ptr)[1]))
  #define get_sint3D_Dptr(ptr)  (((((sint32)(sintD)((ptr)[0]) << intDsize) | (uint32)((ptr)[1])) << intDsize) | (uint32)((ptr)[2]))
  #define get_sint4D_Dptr(ptr)  (((((((sint32)(sintD)((ptr)[0]) << intDsize) | (uint32)((ptr)[1])) << intDsize) | (uint32)((ptr)[2])) << intDsize) | (uint32)((ptr)[3]))
  #if (intDsize==16) && (defined(MC680X0) && !defined(MC680Y0)) # Verbesserung:
    #undef get_uint2D_Dptr
    #undef get_sint2D_Dptr
    #define get_uint2D_Dptr(ptr)  highlow32_at(ptr)
    #define get_sint2D_Dptr(ptr)  (sint32)highlow32_at(ptr)
  #endif
  #if (intDsize==16)
    #undef get_uint3D_Dptr
    #undef get_uint4D_Dptr
    #undef get_sint3D_Dptr
    #undef get_sint4D_Dptr
    #define get_uint3D_Dptr(ptr)  get_uint2D_Dptr(&(ptr)[1])
    #define get_uint4D_Dptr(ptr)  get_uint2D_Dptr(&(ptr)[2])
    #define get_sint3D_Dptr  get_uint3D_Dptr
    #define get_sint4D_Dptr  get_uint4D_Dptr
  #endif
  #if (intDsize==32)
    #undef get_uint2D_Dptr
    #undef get_uint3D_Dptr
    #undef get_uint4D_Dptr
    #undef get_sint2D_Dptr
    #undef get_sint3D_Dptr
    #undef get_sint4D_Dptr
    #define get_uint2D_Dptr(ptr)  get_uint1D_Dptr(&(ptr)[1])
    #define get_uint3D_Dptr(ptr)  get_uint1D_Dptr(&(ptr)[2])
    #define get_uint4D_Dptr(ptr)  get_uint1D_Dptr(&(ptr)[3])
    #define get_sint2D_Dptr  get_uint2D_Dptr
    #define get_sint3D_Dptr  get_uint3D_Dptr
    #define get_sint4D_Dptr  get_uint4D_Dptr
  #endif

# Umwandlungsroutinen Integer <--> Longword:

# Wandelt Fixnum in Longword um.
# FN_to_L(obj)
# > obj: ein Fixnum
# < ergebnis: der Wert des Fixnum als 32-Bit-Zahl.
  local sint32 FN_to_L (object obj);
  #if 1
    #define FN_to_L(obj)  fixnum_to_L(obj)
  #else
    local sint32 FN_to_L(obj)
      var reg1 object obj;
      { if (R_minusp(obj))
          # negativ: mit 1-Bits füllen
          return ((oint)obj >> oint_addr_shift) | ~ (FN_value_mask >> oint_addr_shift);
          else
          # >=0: mit 0-Bits füllen
          return ((oint)obj >> oint_addr_shift) & (FN_value_mask >> oint_addr_shift);
      }
  #endif

# FN_L_zerop(x,x_) stellt fest, ob x = 0 ist.
# Dabei ist x ein Fixnum und x_ = FN_to_L(x).
  #if (oint_addr_len<intLsize)
    #define FN_L_zerop(x,x_)  (x_==0)
  #else
    #define FN_L_zerop(x,x_)  (eq(x,Fixnum_0))
  #endif

# FN_L_minusp(x,x_) stellt fest, ob x < 0 ist.
# Dabei ist x ein Fixnum und x_ = FN_to_L(x).
  #if (oint_addr_len<intLsize)
    #define FN_L_minusp(x,x_)  (x_<0)
  #else
    #define FN_L_minusp(x,x_)  (R_minusp(x))
  #endif

# Wandelt Integer >=0 in Unsigned Longword um.
# I_to_UL(obj)
# > obj: ein Objekt, sollte ein Integer >=0, <2^32 sein
# < ergebnis: der Wert des Integer als 32-Bit-Zahl.
  global uint32 I_to_UL (object obj);
  global uint32 I_to_UL(obj)
    var reg2 object obj;
    { switch (typecode(obj))
        { case_posfixnum: # Fixnum >=0
            return posfixnum_to_L(obj);
          case_posbignum: # Bignum >0
            { var reg1 Bignum bn = TheBignum(obj);
              var reg3 uintC len = bn->length;
              #define IF_LENGTH(i)  \
                if (bn_minlength <= i) # genau i Digits überhaupt möglich? \
                  if (len == i) # genau i Digits?                          \
                    # 2^((i-1)*intDsize-1) <= obj < 2^(i*intDsize-1)       \
                    if ( (i*intDsize-1 > 32)                               \
                         && ( ((i-1)*intDsize-1 >= 32)                     \
                              || (bn->data[0] >= bitc(32-(i-1)*intDsize))  \
                       )    )                                              \
                      goto bad;                                            \
                      else
              IF_LENGTH(1)
                return get_uint1D_Dptr(bn->data);
              IF_LENGTH(2)
                return get_uint2D_Dptr(bn->data);
              IF_LENGTH(3)
                return get_uint3D_Dptr(bn->data);
              IF_LENGTH(4)
                return get_uint4D_Dptr(bn->data);
              #undef IF_LENGTH
            }
          default:
          bad: # unpassendes Objekt
            fehler(
                   DEUTSCH ? "Das ist keine 32-Bit-Zahl: ~" :
                   ENGLISH ? "not a 32-bit integer: ~" :
                   FRANCAIS ? "Ceci n'est pas un nombre à 32 bits : ~" :
                   ""
                  );
    }   }

# Wandelt Longword in Fixnum um.
# L_to_FN(wert)
# > wert: Wert des Fixnums, ein signed 32-Bit-Integer
#         >= -2^oint_addr_len, < 2^oint_addr_len
# < ergebnis: Fixnum mit diesem Wert.
# wert sollte eine Variable sein.
  #if (oint_addr_shift <= vorz_bit_o)
    #define L_to_FN(wert)  \
      ((object)((( (soint)(sint32)(wert)                                          \
                   & (FN_value_vz_mask>>oint_addr_shift) # Unnötiges wegmaskieren \
                 ) << oint_addr_shift                                             \
                )                                                                 \
                | ((oint)fixnum_type<<oint_type_shift) # dafür Typinfo rein       \
      )        )
  #else # (oint_addr_shift > vorz_bit_o)
    #define L_to_FN(wert)  \
      ((object)((( (soint)(sint32)(wert) << oint_addr_shift )                       \
                 & FN_value_mask # Unnötiges wegmaskieren                           \
                )                                                                   \
                | ((soint)(sint32)sign_of_sint32((sint32)(wert)) & bit(vorz_bit_o)) \
                | ((oint)fixnum_type<<oint_type_shift) # dafür Typinfo rein         \
      )        )
  #endif

# Wandelt Longword in Integer um.
# L_to_I(wert)
# > wert: Wert des Integers, ein signed 32-Bit-Integer.
# < ergebnis: Integer mit diesem Wert.
# kann GC auslösen
  global object L_to_I (sint32 wert);
  #if (oint_addr_len+1 >= intLsize)
  inline global object L_to_I(wert)
    var reg2 sint32 wert;
    { return L_to_FN(wert); }
  #else
  global object L_to_I(wert)
    var reg2 sint32 wert;
    {{var reg1 uint32 test = wert & (uint32)(~(FN_value_mask >> oint_addr_shift));
      # test enthält die Bits, die nicht in den Fixnum-Wert reinpassen.
      if (test == (uint32)0) # alle =0 ?
        return (object)(((oint)fixnum_type<<oint_type_shift) | ((oint)wert<<oint_addr_shift));
      if (test == (uint32)(~(FN_value_mask >> oint_addr_shift))) # alle =1 ?
        return (object)(((((oint)fixnum_vz_type<<oint_type_shift)+FN_value_mask) & ((oint)wert<<oint_addr_shift))
                        |(((oint)fixnum_vz_type<<oint_type_shift) & (wbit(oint_addr_shift)-1))
                       );
     }
      # Bignum erzeugen:
      # (dessen Länge  bn_minlength <= n <= ceiling(32/intDsize)  erfüllt)
      if (bn_minlength == ceiling(32,intDsize))
        #if (intDsize==8)
        { if (wert >= 0) goto pos4; else goto neg4; } # Bignum mit 32/intDsize = 4 Digits
        #endif
        #if (intDsize==16)
        { if (wert >= 0) goto pos2; else goto neg2; } # Bignum mit 32/intDsize = 2 Digits
        #endif
        #if (intDsize==32)
        { if (wert >= 0) goto pos1; else goto neg1; } # Bignum mit 32/intDsize = 1 Digits
        #endif
      else
        {
          #define FILL_1_DIGIT(from)  \
            *ptr-- = (uintD)from;
          #define FILL_2_DIGITS(from)  \
            *ptr-- = (uintD)from; from = from >> intDsize; \
            *ptr-- = (uintD)from;
          #define FILL_3_DIGITS(from)  \
            *ptr-- = (uintD)from; from = from >> intDsize; \
            *ptr-- = (uintD)from; from = from >> intDsize; \
            *ptr-- = (uintD)from;
          #define FILL_4_DIGITS(from)  \
            *ptr-- = (uintD)from; from = from >> intDsize; \
            *ptr-- = (uintD)from; from = from >> intDsize; \
            *ptr-- = (uintD)from; from = from >> intDsize; \
            *ptr-- = (uintD)from;
          #define FILL_1  FILL_1_DIGIT(wert);
          #define FILL_2  FILL_2_DIGITS(wert);
          #define FILL_3  FILL_3_DIGITS(wert);
          #define FILL_4  FILL_4_DIGITS(wert);
          #define OK  return new;
          if (wert >= 0)
            {
              #define ALLOC(i)  \
                var reg2 object new = allocate_bignum(i,0); \
                var reg1 uintD* ptr = &TheBignum(new)->data[i-1];
              #define IF_LENGTH(i)  \
                if ((bn_minlength <= i) && (i*intDsize <= 32)) \
                  if (!((i+1)*intDsize <= 32)                  \
                      || ((uint32)wert < bitc(i*intDsize-1))   \
                     )
              #if (intDsize <= 32)
              IF_LENGTH(1)
                pos1:
                { ALLOC(1); FILL_1; OK; } # Bignum mit 1 Digit
              #if (intDsize <= 16)
              IF_LENGTH(2)
                pos2:
                { ALLOC(2); FILL_2; OK; } # Bignum mit 2 Digits
              #if (intDsize <= 8)
              IF_LENGTH(3)
                { ALLOC(3); FILL_3; OK; } # Bignum mit 3 Digits
              IF_LENGTH(4)
                pos4:
                { ALLOC(4); FILL_4; OK; } # Bignum mit 4 Digits
              #endif
              #endif
              #endif
              #undef IF_LENGTH
              #undef ALLOC
            }
            else
            {
              #define ALLOC(i)  \
                var reg2 object new = allocate_bignum(i,-1); \
                var reg1 uintD* ptr = &TheBignum(new)->data[i-1];
              #define IF_LENGTH(i)  \
                if ((bn_minlength <= i) && (i*intDsize <= 32))           \
                  if (!((i+1)*intDsize <= 32)                            \
                      || ((uint32)wert >= (uint32)(-bitc(i*intDsize-1))) \
                     )
              #if (intDsize <= 32)
              IF_LENGTH(1)
                neg1:
                { ALLOC(1); FILL_1; OK; } # Bignum mit 1 Digit
              #if (intDsize <= 16)
              IF_LENGTH(2)
                neg2:
                { ALLOC(2); FILL_2; OK; } # Bignum mit 2 Digits
              #if (intDsize <= 8)
              IF_LENGTH(3)
                { ALLOC(3); FILL_3; OK; } # Bignum mit 3 Digits
              IF_LENGTH(4)
                neg4:
                { ALLOC(4); FILL_4; OK; } # Bignum mit 4 Digits
              #endif
              #endif
              #endif
              #undef IF_LENGTH
              #undef ALLOC
            }
          #undef OK
          #undef FILL_4
          #undef FILL_3
          #undef FILL_2
          #undef FILL_1
          #undef FILL_4_DIGITS
          #undef FILL_3_DIGITS
          #undef FILL_2_DIGITS
          #undef FILL_1_DIGIT
        }
    }
  #endif

# Wandelt Doppel-Longword in Integer um.
# L2_to_I(wert_hi,wert_lo)
# > wert_hi|wert_lo: Wert des Integers, ein signed 64-Bit-Integer.
# < ergebnis: Integer mit diesem Wert.
# kann GC auslösen
  global object L2_to_I (sint32 wert_hi, uint32 wert_lo);
  global object L2_to_I(wert_hi,wert_lo)
    var reg2 sint32 wert_hi;
    var reg2 uint32 wert_lo;
    { if (wert_hi == 0)
        { if ((wert_lo & (uint32)(~(FN_value_mask >> oint_addr_shift))) # Bits von wert_lo, die nicht in den Fixnum-Wert passen
              == (uint32)0                                              # alle =0 ?
             )
            return (object)(((oint)fixnum_type<<oint_type_shift) | ((oint)wert_lo<<oint_addr_shift));
        }
      elif (wert_hi == ~(uintL)0)
        { if ((wert_lo & (uint32)(~(FN_value_mask >> oint_addr_shift))) # Bits von wert_lo, die nicht in den Fixnum-Wert passen
              == (uint32)(~(FN_value_mask >> oint_addr_shift))          # alle =1 ?
             )
            #ifndef WIDE
            return (object)(((((oint)fixnum_vz_type<<oint_type_shift)+FN_value_mask) & (wert_lo<<oint_addr_shift))
                            |(((oint)fixnum_vz_type<<oint_type_shift) & (wbit(oint_addr_shift)-1))
                           );
            #else
            return (object)(((oint)fixnum_vz_type<<oint_type_shift) | ((oint)(wert_lo & (uint32)(FN_value_mask >> oint_addr_shift)) << oint_addr_shift));
            #endif
        }
      # Bignum erzeugen:
      # (dessen Länge  bn_minlength <= n <= ceiling(64/intDsize)  erfüllt)
      #define FILL_1_DIGIT(from)  \
        *ptr-- = (uintD)from;
      #define FILL_2_DIGITS(from)  \
        *ptr-- = (uintD)from; from = from >> intDsize; \
        *ptr-- = (uintD)from;
      #define FILL_3_DIGITS(from)  \
        *ptr-- = (uintD)from; from = from >> intDsize; \
        *ptr-- = (uintD)from; from = from >> intDsize; \
        *ptr-- = (uintD)from;
      #define FILL_4_DIGITS(from)  \
        *ptr-- = (uintD)from; from = from >> intDsize; \
        *ptr-- = (uintD)from; from = from >> intDsize; \
        *ptr-- = (uintD)from; from = from >> intDsize; \
        *ptr-- = (uintD)from;
      #if (32/intDsize==1)
      #define FILL_1  FILL_1_DIGIT(wert_lo);
      #define FILL_2  FILL_1_DIGIT(wert_lo); FILL_1_DIGIT(wert_hi);
      #define FILL_3
      #define FILL_4
      #define FILL_5
      #define FILL_6
      #define FILL_7
      #define FILL_8
      #endif
      #if (32/intDsize==2)
      #define FILL_1  FILL_1_DIGIT(wert_lo);
      #define FILL_2  FILL_2_DIGITS(wert_lo);
      #define FILL_3  FILL_2_DIGITS(wert_lo); FILL_1_DIGIT(wert_hi);
      #define FILL_4  FILL_2_DIGITS(wert_lo); FILL_2_DIGITS(wert_hi);
      #define FILL_5
      #define FILL_6
      #define FILL_7
      #define FILL_8
      #endif
      #if (32/intDsize==4)
      #define FILL_1  FILL_1_DIGIT(wert_lo);
      #define FILL_2  FILL_2_DIGITS(wert_lo);
      #define FILL_3  FILL_3_DIGITS(wert_lo);
      #define FILL_4  FILL_4_DIGITS(wert_lo);
      #define FILL_5  FILL_4_DIGITS(wert_lo); FILL_1_DIGIT(wert_hi);
      #define FILL_6  FILL_4_DIGITS(wert_lo); FILL_2_DIGITS(wert_hi);
      #define FILL_7  FILL_4_DIGITS(wert_lo); FILL_3_DIGITS(wert_hi);
      #define FILL_8  FILL_4_DIGITS(wert_lo); FILL_4_DIGITS(wert_hi);
      #endif
      #define OK  return new;
      if (wert_hi >= 0)
        {
          #define ALLOC(i)  \
            var reg2 object new = allocate_bignum(i,0); \
            var reg1 uintD* ptr = &TheBignum(new)->data[i-1];
          #define IF_LENGTH(i)  \
            if ((bn_minlength <= i) && (i*intDsize <= 64))                 \
              if (!((i+1)*intDsize <= 64)                                  \
                  || (i*intDsize-1 < 32                                    \
                      ? ((wert_hi == 0) && (wert_lo < bitc(i*intDsize-1))) \
                      : ((uint32)wert_hi < bitc(i*intDsize-1-32))          \
                 )   )
          IF_LENGTH(1)
            { ALLOC(1); FILL_1; OK; } # Bignum mit 1 Digit
          IF_LENGTH(2)
            { ALLOC(2); FILL_2; OK; } # Bignum mit 2 Digits
          IF_LENGTH(3)
            { ALLOC(3); FILL_3; OK; } # Bignum mit 3 Digits
          IF_LENGTH(4)
            { ALLOC(4); FILL_4; OK; } # Bignum mit 4 Digits
          IF_LENGTH(5)
            { ALLOC(5); FILL_5; OK; } # Bignum mit 5 Digits
          IF_LENGTH(6)
            { ALLOC(6); FILL_6; OK; } # Bignum mit 6 Digits
          IF_LENGTH(7)
            { ALLOC(7); FILL_7; OK; } # Bignum mit 7 Digits
          IF_LENGTH(8)
            { ALLOC(8); FILL_8; OK; } # Bignum mit 8 Digits
          #undef IF_LENGTH
          #undef ALLOC
        }
        else
        {
          #define ALLOC(i)  \
            var reg2 object new = allocate_bignum(i,-1); \
            var reg1 uintD* ptr = &TheBignum(new)->data[i-1];
          #define IF_LENGTH(i)  \
            if ((bn_minlength <= i) && (i*intDsize <= 64))                    \
              if (!((i+1)*intDsize <= 64)                                     \
                  || (i*intDsize-1 < 32                                       \
                      ? ((wert_hi == ~(uint32)0) && (wert_lo >= (uint32)(-bitc(i*intDsize-1)))) \
                      : ((uint32)wert_hi >= (uint32)(-bitc(i*intDsize-1-32))) \
                 )   )
          IF_LENGTH(1)
            { ALLOC(1); FILL_1; OK; } # Bignum mit 1 Digit
          IF_LENGTH(2)
            { ALLOC(2); FILL_2; OK; } # Bignum mit 2 Digits
          IF_LENGTH(3)
            { ALLOC(3); FILL_3; OK; } # Bignum mit 3 Digits
          IF_LENGTH(4)
            { ALLOC(4); FILL_4; OK; } # Bignum mit 4 Digits
          IF_LENGTH(5)
            { ALLOC(5); FILL_5; OK; } # Bignum mit 5 Digits
          IF_LENGTH(6)
            { ALLOC(6); FILL_6; OK; } # Bignum mit 6 Digits
          IF_LENGTH(7)
            { ALLOC(7); FILL_7; OK; } # Bignum mit 7 Digits
          IF_LENGTH(8)
            { ALLOC(8); FILL_8; OK; } # Bignum mit 8 Digits
          #undef IF_LENGTH
          #undef ALLOC
        }
      #undef OK
      #undef FILL_8
      #undef FILL_7
      #undef FILL_6
      #undef FILL_5
      #undef FILL_4
      #undef FILL_3
      #undef FILL_2
      #undef FILL_1
      #undef FILL_4_DIGITS
      #undef FILL_3_DIGITS
      #undef FILL_2_DIGITS
      #undef FILL_1_DIGIT
    }

# Wandelt Unsigned Longword in Integer >=0 um.
# UL_to_I(wert)
# > wert: Wert des Integers, ein unsigned 32-Bit-Integer.
# < ergebnis: Integer mit diesem Wert.
# kann GC auslösen
#ifndef UL_to_I # wenn nicht schon als Macro definiert
  global object UL_to_I (uint32 wert);
  global object UL_to_I(wert)
    var reg2 uint32 wert;
    { if ((wert & ~ (FN_value_mask >> oint_addr_shift)) == 0)
        # alle Bits, die nicht in den Fixnum-Wert reinpassen, =0 ?
        return (object)(((oint)fixnum_type<<oint_type_shift) | (wert<<oint_addr_shift));
      # Bignum erzeugen:
      # (dessen Länge  bn_minlength <= n <= ceiling((32+1)/intDsize)  erfüllt)
      #define UL_maxlength  ceiling(32+1,intDsize)
      #if (bn_minlength <= 1) && (UL_maxlength >= 1)
      if ((1*intDsize-1 < 32)
          ? ((uint32)wert <= (uint32)(bitc(1*intDsize-1)-1))
          : TRUE
         )
        # Bignum mit 1 Digit
        { var reg1 object new = allocate_bignum(1,0);
          TheBignum(new)->data[0] = (uintD)wert;
          return new;
        }
      #endif
      #if (bn_minlength <= 2) && (UL_maxlength >= 2)
      if ((2*intDsize-1 < 32)
          ? ((uint32)wert <= (uint32)(bitc(2*intDsize-1)-1))
          : TRUE
         )
        # Bignum mit 2 Digits
        { var reg2 object new = allocate_bignum(2,0);
          var reg1 uintD* ptr = &TheBignum(new)->data[1];
          *ptr-- = (uintD)wert;
          #if (intDsize>=32)
            *ptr = 0;
          #else
            wert = wert >> intDsize; *ptr = (uintD)wert;
          #endif
          return new;
        }
      #endif
      #if (bn_minlength <= 3) && (UL_maxlength >= 3)
      if ((3*intDsize-1 < 32)
          ? ((uint32)wert <= (uint32)(bitc(3*intDsize-1)-1))
          : TRUE
         )
        # Bignum mit 3 Digits
        { var reg2 object new = allocate_bignum(3,0);
          var reg1 uintD* ptr = &TheBignum(new)->data[2];
          *ptr-- = (uintD)wert; wert = wert >> intDsize;
          *ptr-- = (uintD)wert;
          #if (2*intDsize>=32)
            *ptr = 0;
          #else
            wert = wert >> intDsize; *ptr = (uintD)wert;
          #endif
          return new;
        }
      #endif
      #if (bn_minlength <= 4) && (UL_maxlength >= 4)
      if ((4*intDsize-1 < 32)
          ? ((uint32)wert <= (uint32)(bitc(4*intDsize-1)-1))
          : TRUE
         )
        # Bignum mit 4 Digits
        { var reg2 object new = allocate_bignum(4,0);
          var reg1 uintD* ptr = &TheBignum(new)->data[3];
          *ptr-- = (uintD)wert; wert = wert >> intDsize;
          *ptr-- = (uintD)wert; wert = wert >> intDsize;
          *ptr-- = (uintD)wert;
          #if (3*intDsize>=32)
            *ptr = 0;
          #else
            wert = wert >> intDsize; *ptr = (uintD)wert;
          #endif
          return new;
        }
      #endif
      #if (bn_minlength <= 5) && (UL_maxlength >= 5)
      if (TRUE)
        # Bignum mit 5 Digits
        { var reg2 object new = allocate_bignum(5,0);
          var reg1 uintD* ptr = &TheBignum(new)->data[4];
          *ptr-- = (uintD)wert; wert = wert >> intDsize;
          *ptr-- = (uintD)wert; wert = wert >> intDsize;
          *ptr-- = (uintD)wert; wert = wert >> intDsize;
          *ptr-- = (uintD)wert;
          #if (4*intDsize>=32)
            *ptr = 0;
          #else
            wert = wert >> intDsize; *ptr = (uintD)wert;
          #endif
          return new;
        }
      #endif
    }
#endif

# Umwandlungsroutinen Digit sequence --> Integer:

# Normalized Digit sequence to Integer
# NDS_to_I(MSDptr,len)
# Digit Sequence MSDptr/len/.. in Integer umwandeln.
# kann GC auslösen
  local object NDS_to_I (uintD* MSDptr, uintC len);
  local object NDS_to_I(MSDptr,len)
    var reg2 uintD* MSDptr;
    var reg3 uintC len;
    { # Mehr als bn_minlength Digits -> Bignum.
      # Weniger als bn_minlength Digits -> Fixnum.
      # Genau bn_minlength Digits -> Bignum oder Fixnum.
      if (len < bn_minlength)
        { # 0..bn_minlength-1 Digits, paßt in ein Fixnum:
          if (bn_minlength>1 ? (len==0) : TRUE)
            # 0 Digits
            { return Fixnum_0; }
         {var reg1 sint32 wert;
          if (bn_minlength>2 ? (len==1) : TRUE)
            # 1 Digit
            len_1:
            { wert = get_sint1D_Dptr(MSDptr); }
          elif (bn_minlength>3 ? (len==2) : TRUE)
            # 2 Digits
            len_2:
            { wert = get_sint2D_Dptr(MSDptr); }
          elif (TRUE)
            # 3 Digits
            len_3:
            { wert = get_sint3D_Dptr(MSDptr); }
          elif (FALSE)
            # 4 Digits
            len_4:
            { wert = get_sint4D_Dptr(MSDptr); }
          return
            #if (oint_addr_shift <= vorz_bit_o)
            (object)((( (soint)wert
                        & (FN_value_vz_mask>>oint_addr_shift) # Unnötiges wegmaskieren
                      ) << oint_addr_shift
                     )
                     | ((oint)fixnum_type<<oint_type_shift) # dafür Typinfo rein
                    )
            #else # (oint_addr_shift > vorz_bit_o)
            (object)((( (soint)wert << oint_addr_shift )
                      & FN_value_mask # Unnötiges wegmaskieren
                     )
                     | ((soint)(sint32)sign_of_sintD(MSDptr[0]) & bit(vorz_bit_o))
                     | ((oint)fixnum_type<<oint_type_shift) # dafür Typinfo rein
                    )
            #endif
            ;
        }}
      if (len == bn_minlength)
        # bn_minlength Digits, also (incl. Vorzeichen) zwischen
        # (bn_minlength-1)*intDsize+1 und bn_minlength*intDsize Bits.
        # Höchstens oint_addr_len+1 Bits -> paßt in ein Fixnum:
        { if (  (MSDptr[0] <= (uintD)(bit(oint_addr_len-(bn_minlength-1)*intDsize)-1)) # Fixnum >=0 ?
              ||(MSDptr[0] >= (uintD)(-bit(oint_addr_len-(bn_minlength-1)*intDsize))) # Fixnum <0 ?
             )
            #if (bn_minlength==1)
            goto len_1;
            #endif
            #if (bn_minlength==2)
            goto len_2;
            #endif
            #if (bn_minlength==3)
            goto len_3;
            #endif
            #if (bn_minlength==4)
            goto len_4;
            #endif
        }
      # mindestens bn_minlength Digits, mache ein Bignum
      { var reg4 object new = allocate_bignum(len,sign_of_sintD(MSDptr[0]));
        # neues Bignum mit dem Inhalt der NDS füllen:
        copy_loop_up(MSDptr,&TheBignum(new)->data[0],len);
        return new;
    } }

# Bignum-Überlauf melden:
  local nonreturning void BN_ueberlauf (void);
  local nonreturning void BN_ueberlauf()
    { fehler(
             DEUTSCH ? "Überlauf von Bignums" :
             ENGLISH ? "bignum overflow" :
             FRANCAIS ? "Dépassage de capacité des entiers BIGNUM" :
             ""
            );
    }

# Normalized Unsigned Digit Sequence to Integer
# NUDS_to_I(MSDptr,len)
# Normalized UDS MSDptr/len/.. in Integer >=0 umwandeln.
# Unterhalb von MSDptr muß 1 Digit Platz sein.
# kann GC auslösen
  local object NUDS_to_I (uintD* MSDptr, uintC len);
  local object NUDS_to_I(MSDptr,len)
    var reg1 uintD* MSDptr;
    var reg2 uintC len;
    { if ((!(len==0)) && ((sintD)MSDptr[0] < 0))
        # Falls die Länge >0 und das Most significant Bit = 1 sind,
        # die Digit Sequence um ein Nulldigit erweitern:
        { *--MSDptr = 0;
          len++;
          if (uintCoverflow(len)) { BN_ueberlauf(); } # Überlauf der Länge?
        }
      return NDS_to_I(MSDptr,len);
    }

# Unsigned Digit Sequence to Integer
# UDS_to_I(MSDptr,len)
# UDS MSDptr/len/.. in Integer >=0 umwandeln.
# Unterhalb von MSDptr muß 1 Digit Platz sein.
# kann GC auslösen
  local object UDS_to_I (uintD* MSDptr, uintC len);
  local object UDS_to_I(MSDptr,len)
    var reg1 uintD* MSDptr;
    var reg2 uintC len;
    { while ( (!(len==0)) && (MSDptr[0]==0) ) # solange len>0 und MSD = 0,
        { MSDptr++; len--; } # Nulldigit streichen
      # Dann wie bei NUDS_to_I :
      if ((!(len==0)) && ((sintD)MSDptr[0] < 0))
        # Falls die Länge >0 und das Most significant Bit = 1 sind,
        # die Digit Sequence um ein Nulldigit erweitern:
        { *--MSDptr = 0;
          len++;
          if (uintCoverflow(len)) { BN_ueberlauf(); } # Überlauf der Länge?
        }
      return NDS_to_I(MSDptr,len);
    }

# Digit Sequence to Integer
# DS_to_I(MSDptr,len)
# DS MSDptr/len/.. in Integer umwandeln.
# kann GC auslösen
  local object DS_to_I (uintD* MSDptr, uintC len);
  local object DS_to_I(MSDptr,len)
    var reg1 uintD* MSDptr;
    var reg3 uintC len;
    { # erst normalisieren.
      # Dabei evtl. MSDptr erhöhen und len erniedrigen:
      if (!(len==0)) # leere DS ist normalisiert
        { var reg2 uintC count = len-1;
          if ((sintD)MSDptr[0] >= 0)
            # Zahl >= 0
            { # versuche maximal len-1 führende Nullen-Digits zu streichen:
              while (!(count==0) && (MSDptr[0]==0) && ((sintD)MSDptr[1]>=0))
                { MSDptr++; len--; count--; } # Nulldigit streichen
            }
            else
            # Zahl < 0
            # versuche maximal len-1 führende Einsen-Digits zu streichen:
            { while (!(count==0) && ((sintD)MSDptr[0]==-1) && ((sintD)MSDptr[1]<0))
                { MSDptr++; len--; count--; } # Einsen-digit streichen
        }   }
      # Eventuell ist jetzt noch bei der DS 0 ausnahmsweise len=1,
      # aber NDS_to_I wird auch damit fertig.
      return NDS_to_I(MSDptr,len);
    }

# Umwandlungsroutinen Integer --> Digit sequence:

# Unterteilung eines Fixnums in Digits:
# intDsize=8 -> MSD=LSD3,LSD2,LSD1,LSD0, sollte FN_maxlength=4 sein.
# intDsize=16 -> MSD=LSD1,LSD0, sollte FN_maxlength=2 sein.
# intDsize=32 -> MSD=LSD0, sollte FN_maxlength=1 sein.
# WIDE -> ebenso, nur ist FN_maxlength noch eins größer.

#if FN_maxlength>1
  #define FN_LSD0(obj)  ((uintD)((oint)(obj)>>oint_addr_shift))
#elif FN_maxlength==1
  #define FN_LSD0  FN_MSD
#endif
#if FN_maxlength>2
  #define FN_LSD1(obj)  ((uintD)((oint)(obj)>>(oint_addr_shift+intDsize)))
#elif FN_maxlength==2
  #define FN_LSD1  FN_MSD
#else # FN_maxlength<2
  #define FN_LSD1(obj)  0; NOTREACHED  # sollte nicht aufgerufen werden!
#endif
#if FN_maxlength>3
  #define FN_LSD2(obj)  ((uintD)((oint)(obj)>>(oint_addr_shift+2*intDsize)))
#elif FN_maxlength==3
  #define FN_LSD2  FN_MSD
#else # FN_maxlength<3
  #define FN_LSD2(obj)  0; NOTREACHED  # sollte nicht aufgerufen werden!
#endif
#if FN_maxlength>4
  #define FN_LSD3(obj)  ((uintD)((oint)(obj)>>(oint_addr_shift+3*intDsize)))
#elif FN_maxlength==4
  #define FN_LSD3  FN_MSD
#else # FN_maxlength<4
  #define FN_LSD3(obj)  0; NOTREACHED  # sollte nicht aufgerufen werden!
#endif
#if FN_maxlength==5
  #define FN_LSD4  FN_MSD
#else # FN_maxlength<5
  #define FN_LSD4(obj)  0; NOTREACHED  # sollte nicht aufgerufen werden!
#endif
# FN_MSD: insgesamt muß um (FN_maxlength-1)*intDsize+oint_addr_shift Bits
# nach rechts geshiftet werden.
#if defined(WIDE)
  #define FN_MSD(obj)  \
    ((uintD)( (sintD)(typecode(obj) << (intDsize-1-vorz_bit_t)) >> (intDsize-1)))
#elif (vorz_bit_o == oint_addr_len+oint_addr_shift) || ((oint_addr_len==(FN_maxlength-1)*intDsize) && (vorz_bit_o >= intDsize-1))
  #if HAVE_DD
    #define FN_MSD(obj)  \
      ((sintD)((sintDD)(sintD)((oint)(obj)>>(vorz_bit_o-(intDsize-1))) \
               >>(oint_addr_shift-vorz_bit_o+FN_maxlength*intDsize-1)  \
      )       )
  #elif (vorz_bit_o < intDsize)
    #define FN_MSD(obj)  \
      (((sintD)(oint)(obj) << (intDsize-1-vorz_bit_o)) >> (FN_maxlength*intDsize-1-vorz_bit_o+oint_addr_shift))
  #endif
#else
  # signD_of_sintD(x,k) liefert das Vorzeichen von x als sintD; die hinteren
  # k Bit sind irrelevant.
  #if HAVE_DD
    #define signD_of_sintD(x,k)  ((sintDD)(sintD)(x)>>intDsize)
  #else
    #define signD_of_sintD(x,k)  ((sintD)(x)>>(intDsize-1-(k)))
  #endif
  #if (vorz_bit_o >= intDsize)
    #define FN_MSD(obj)  \
      ( ((sintD)((oint)(obj)>>(oint_addr_shift+(FN_maxlength-1)*intDsize))&(bit(oint_addr_len-(FN_maxlength-1)*intDsize)-1)) \
       |((sintD)signD_of_sintD((oint)(obj)>>(vorz_bit_o-(intDsize-1)),oint_addr_len-(FN_maxlength-1)*intDsize)&(-bit(oint_addr_len-(FN_maxlength-1)*intDsize))) \
      )
  #else # (vorz_bit_o < intDsize)
    #define FN_MSD(obj)  \
      ( ((sintD)((oint)(obj)>>(oint_addr_shift+(FN_maxlength-1)*intDsize))&(bit(oint_addr_len-(FN_maxlength-1)*intDsize)-1)) \
       |((sintD)signD_of_sintD((oint)(obj)<<((intDsize-1)-vorz_bit_o),oint_addr_len-(FN_maxlength-1)*intDsize)&(-bit(oint_addr_len-(FN_maxlength-1)*intDsize))) \
      )
  #endif
#endif

# Fixnum to Normalized Digit sequence
# FN_to_NDS_nocopy(obj, MSDptr=,len=,LSDptr=);
# > obj: ein Fixnum
# < MSDptr/len/LSDptr: Normalized Digit sequence, im Maschinenstack
  #define FN_to_NDS_nocopy(obj,MSDptr_zuweisung,len_zuweisung,LSDptr_zuweisung)  \
    FN_to_NDS_(nocopy,obj,MSDptr_zuweisung,len_zuweisung,_EMA_ LSDptr_zuweisung)
  #define alloc_FNDS_nocopy(len,MSDptr_zuweisung,LSDptr_zuweisung)  \
     {var uintD UDS_from_FN_to_NDS[FN_maxlength]; \
      MSDptr_zuweisung &UDS_from_FN_to_NDS[0];    \
      LSDptr_zuweisung &UDS_from_FN_to_NDS[len];  \
     }

# Fixnum to Normalized Digit sequence
# FN_to_NDS(obj, MSDptr=,len=,LSDptr=);
# > obj: ein Fixnum
# < MSDptr/len/LSDptr: Normalized Digit sequence, darf modifiziert werden.
# Dabei wird num_stack erniedrigt.
  #define FN_to_NDS(obj,MSDptr_zuweisung,len_zuweisung,LSDptr_zuweisung)  \
    FN_to_NDS_(copy,obj,MSDptr_zuweisung,len_zuweisung,_EMA_ LSDptr_zuweisung)
  #define alloc_FNDS_copy  num_stack_need

# Fixnum to Normalized Digit sequence
# FN_to_NDS_1(obj, MSDptr=,len=,LSDptr=);
# > obj: ein Fixnum
# < MSDptr/len/LSDptr: Normalized Digit sequence, darf modifiziert werden.
# Unterhalb von MSDptr ist noch 1 Digit Platz.
# Dabei wird num_stack erniedrigt.
  #define FN_to_NDS_1(obj,MSDptr_zuweisung,len_zuweisung,LSDptr_zuweisung)  \
    FN_to_NDS_(copy_1,obj,MSDptr_zuweisung,len_zuweisung,_EMA_ LSDptr_zuweisung)
  #define alloc_FNDS_copy_1  num_stack_need_1

  #define FN_MSD1_mask  # wird nur bei FN_maxlength >= 2 gebraucht, d.h. intDsize-1 < oint_addr_len \
    (FN_value_vz_mask & ~((oint)(bitc(intDsize-1)-1)<<oint_addr_shift))
  #define FN_MSD2_mask  # wird nur bei FN_maxlength >= 3 gebraucht, d.h. 2*intDsize-1 < oint_addr_len \
    (FN_value_vz_mask & ~((oint)(bitc(2*intDsize-1)-1)<<oint_addr_shift))
  #define FN_MSD3_mask  # wird nur bei FN_maxlength >= 4 gebraucht, d.h. 3*intDsize-1 < oint_addr_len \
    (FN_value_vz_mask & ~((oint)(bitc(3*intDsize-1)-1)<<oint_addr_shift))
  #define FN_MSD4_mask  # wird nur bei FN_maxlength >= 5 gebraucht, d.h. 4*intDsize-1 < oint_addr_len \
    (FN_value_vz_mask & ~((oint)(bitc(4*intDsize-1)-1)<<oint_addr_shift))
  #define FN_to_NDS_(option, obj, MSDptr_zuweisung,len_zuweisung,LSDptr_zuweisung)  \
    { var reg1 oint fix_from_FN_to_NDS = (oint)(obj);                                               \
      var reg3 uintC len_from_FN_to_NDS;                                                            \
      var reg2 uintD* ptr_from_FN_to_NDS;                                                           \
      # Länge der NDS bestimmen:                                                                    \
      if (eq((object)fix_from_FN_to_NDS,Fixnum_0)) # mindestens 1 Digit nötig?                      \
        { len_from_FN_to_NDS=0; }                                                                   \
        else                                                                                        \
        { var reg3 uint32 testMSD; # vordere Bits von fix_from_FN_to_NDS                            \
          if ((FN_maxlength<=1) ||                                                                  \
              (((testMSD = fix_from_FN_to_NDS & FN_MSD1_mask) == 0) || (testMSD == FN_MSD1_mask))   \
             )                                                                                      \
            { len_from_FN_to_NDS=1; } # nur ein Digit abzulegen                                     \
          elif ((FN_maxlength<=2) ||                                                                \
                (((testMSD = fix_from_FN_to_NDS & FN_MSD2_mask) == 0) || (testMSD == FN_MSD2_mask)) \
               )                                                                                    \
            { len_from_FN_to_NDS=2; } # zwei Digits abzulegen                                       \
          elif ((FN_maxlength<=3) ||                                                                \
                (((testMSD = fix_from_FN_to_NDS & FN_MSD3_mask) == 0) || (testMSD == FN_MSD3_mask)) \
               )                                                                                    \
            { len_from_FN_to_NDS=3; } # drei Digits abzulegen                                       \
          elif ((FN_maxlength<=4) ||                                                                \
                (((testMSD = fix_from_FN_to_NDS & FN_MSD4_mask) == 0) || (testMSD == FN_MSD4_mask)) \
               )                                                                                    \
            { len_from_FN_to_NDS=4; } # vier Digits abzulegen                                       \
          else                                                                                      \
            { len_from_FN_to_NDS=5; } # fünf Digits abzulegen                                       \
        }                                                                                           \
      len_zuweisung len_from_FN_to_NDS;                                                             \
      # Platz belegen:                                                                              \
      CONCAT(alloc_FNDS_,option)                                                                    \
        (len_from_FN_to_NDS, MSDptr_zuweisung ptr_from_FN_to_NDS =,_EMA_ LSDptr_zuweisung);         \
      # Platz füllen:                                                                               \
      if (len_from_FN_to_NDS > 0)                                                                   \
        { if ((FN_maxlength>1) && (len_from_FN_to_NDS > 1))                                         \
            { if ((FN_maxlength>2) && (len_from_FN_to_NDS > 2))                                     \
                { if ((FN_maxlength>3) && (len_from_FN_to_NDS > 3))                                 \
                    { if ((FN_maxlength>4) && (len_from_FN_to_NDS > 4))                             \
                         # fünf Digits abzulegen:                                                   \
                         { *ptr_from_FN_to_NDS++ = FN_LSD4((object)fix_from_FN_to_NDS); }           \
                      # noch vier Digits abzulegen:                                                 \
                      *ptr_from_FN_to_NDS++ = FN_LSD3((object)fix_from_FN_to_NDS);                  \
                    }                                                                               \
                  # noch drei Digits abzulegen:                                                     \
                  *ptr_from_FN_to_NDS++ = FN_LSD2((object)fix_from_FN_to_NDS);                      \
                }                                                                                   \
              # noch zwei Digits abzulegen:                                                         \
              *ptr_from_FN_to_NDS++ = FN_LSD1((object)fix_from_FN_to_NDS);                          \
            }                                                                                       \
          # noch ein Digit abzulegen:                                                               \
          *ptr_from_FN_to_NDS = FN_LSD0((object)fix_from_FN_to_NDS);                                \
        }                                                                                           \
    }

# Bignum to Normalized Digit sequence, Kopieren unnötig
# BN_to_NDS_nocopy(obj, MSDptr=,len=,LSDptr=);
# > obj: ein Bignum
# < MSDptr/len/LSDptr: Normalized Digit sequence
  #define BN_to_NDS_nocopy(obj, MSDptr_zuweisung,len_zuweisung,LSDptr_zuweisung)  \
    { var reg1 Bignum bn_from_BN_to_NDS_nocopy = TheBignum(obj); \
      MSDptr_zuweisung &bn_from_BN_to_NDS_nocopy->data[0];       \
      LSDptr_zuweisung &bn_from_BN_to_NDS_nocopy->data[(uintL)(  \
        len_zuweisung bn_from_BN_to_NDS_nocopy->length )];       \
    }

# Bignum to Normalized Digit sequence
# BN_to_NDS(obj, MSDptr=,len=,LSDptr=);
# > obj: ein Bignum
# < MSDptr/len/LSDptr: Normalized Digit sequence, darf modifiziert werden.
# Dabei wird num_stack erniedrigt.
  #define BN_to_NDS(obj, MSDptr_zuweisung,len_zuweisung,LSDptr_zuweisung)  \
    { var reg3 object obj_from_BN_to_NDS = (obj);                          \
      var reg1 uintD* MSDptr_from_BN_to_NDS;                               \
      var reg2 uintC len_from_BN_to_NDS;                                   \
      len_zuweisung len_from_BN_to_NDS = TheBignum(obj_from_BN_to_NDS)->length; \
      num_stack_need(len_from_BN_to_NDS, MSDptr_zuweisung MSDptr_from_BN_to_NDS = ,_EMA_ LSDptr_zuweisung); \
      copy_loop_up(&TheBignum(obj_from_BN_to_NDS)->data[0],MSDptr_from_BN_to_NDS,len_from_BN_to_NDS); \
    }

# Bignum to Normalized Digit sequence
# BN_to_NDS_1(obj, MSDptr=,len=,LSDptr=);
# > obj: ein Bignum
# < MSDptr/len/LSDptr: Normalized Digit sequence, darf modifiziert werden.
# Unterhalb von MSDptr ist noch 1 Digit Platz.
# Dabei wird num_stack erniedrigt.
  #define BN_to_NDS_1(obj, MSDptr_zuweisung,len_zuweisung,LSDptr_zuweisung)  \
    { var reg3 object obj_from_BN_to_NDS = (obj);                          \
      var reg1 uintD* MSDptr_from_BN_to_NDS;                               \
      var reg2 uintC len_from_BN_to_NDS;                                   \
      len_zuweisung len_from_BN_to_NDS = TheBignum(obj_from_BN_to_NDS)->length; \
      num_stack_need_1(len_from_BN_to_NDS, MSDptr_zuweisung MSDptr_from_BN_to_NDS = ,_EMA_ LSDptr_zuweisung); \
      copy_loop_up(&TheBignum(obj_from_BN_to_NDS)->data[0],MSDptr_from_BN_to_NDS,len_from_BN_to_NDS); \
    }

# Integer to Normalized Digit sequence, Kopieren unnötig.
# I_to_NDS_nocopy(obj, MSDptr=,len=,LSDptr=);
# > obj: ein Integer
# < MSDptr/len/LSDptr: Normalized Digit sequence
  #define I_to_NDS_nocopy(obj, MSDptr_zuweisung,len_zuweisung,LSDptr_zuweisung)  \
    { var reg3 object obj_from_I_to_NDS_nocopy = (obj);                          \
      if (I_fixnump(obj_from_I_to_NDS_nocopy))                                   \
        { FN_to_NDS_nocopy(obj_from_I_to_NDS_nocopy,MSDptr_zuweisung,len_zuweisung,_EMA_ LSDptr_zuweisung); } \
        else                                                                     \
        { BN_to_NDS_nocopy(obj_from_I_to_NDS_nocopy,MSDptr_zuweisung,len_zuweisung,_EMA_ LSDptr_zuweisung); } \
    }

# Integer to Normalized Digit sequence
# I_to_NDS(obj, MSDptr=,len=,LSDptr=);
# > obj: ein Integer
# < MSDptr/len/LSDptr: Normalized Digit sequence, darf modifiziert werden.
# Dabei wird num_stack erniedrigt.
  #define I_to_NDS(obj, MSDptr_zuweisung,len_zuweisung,LSDptr_zuweisung)  \
    { var reg3 object obj_from_I_to_NDS = (obj);                          \
      if (I_fixnump(obj_from_I_to_NDS))                                   \
        { FN_to_NDS(obj_from_I_to_NDS,MSDptr_zuweisung,len_zuweisung,_EMA_ LSDptr_zuweisung); } \
        else                                                              \
        { BN_to_NDS(obj_from_I_to_NDS,MSDptr_zuweisung,len_zuweisung,_EMA_ LSDptr_zuweisung); } \
    }

# Integer to Normalized Digit sequence
# I_to_NDS_1(obj, MSDptr=,len=,LSDptr=);
# > obj: ein Integer
# < MSDptr/len/LSDptr: Normalized Digit sequence, darf modifiziert werden.
# Unterhalb von MSDptr ist noch 1 Digit Platz.
# Dabei wird num_stack erniedrigt.
  #define I_to_NDS_1(obj, MSDptr_zuweisung,len_zuweisung,LSDptr_zuweisung)  \
    { var reg3 object obj_from_I_to_NDS = (obj);                            \
      if (I_fixnump(obj_from_I_to_NDS))                                     \
        { FN_to_NDS_1(obj_from_I_to_NDS,MSDptr_zuweisung,len_zuweisung,_EMA_ LSDptr_zuweisung); } \
        else                                                                \
        { BN_to_NDS_1(obj_from_I_to_NDS,MSDptr_zuweisung,len_zuweisung,_EMA_ LSDptr_zuweisung); } \
    }

# Holt die nächsten pFN_maxlength Digits in ein uint32:
# _ptr ist vom Typ uintD*.
  #if (pFN_maxlength==1)
    #define pFN_maxlength_digits_at(_ptr)  \
      (uint32)(_ptr[0])
  #elif (pFN_maxlength==2) && (intDsize==16)
    #define pFN_maxlength_digits_at(_ptr)  \
      highlow32_at(_ptr)
  #elif (pFN_maxlength==2)
    #define pFN_maxlength_digits_at(_ptr)  \
      (((uint32)(_ptr[0])<<intDsize)|       \
        (uint32)(_ptr[1]))
  #elif (pFN_maxlength==3)
    #define pFN_maxlength_digits_at(_ptr)  \
      (((((uint32)(_ptr[0])<<intDsize)|     \
          (uint32)(_ptr[1]))<<intDsize)|    \
          (uint32)(_ptr[2]))
  #elif (pFN_maxlength==4)
    #define pFN_maxlength_digits_at(_ptr)  \
      (((((((uint32)(_ptr[0])<<intDsize)|   \
            (uint32)(_ptr[1]))<<intDsize)|  \
            (uint32)(_ptr[2]))<<intDsize)|  \
            (uint32)(_ptr[3]))
  #endif

# Schreibt ein uint32 in die nächsten pFN_maxlength Digits:
# _ptr ist vom Typ uintD*, _wert vom Typ uint32.
  #if (pFN_maxlength==1)
    #define set_pFN_maxlength_digits_at(_ptr,_wert)  \
      (_ptr[0] = (uintD)_wert)
  #elif (pFN_maxlength==2) && (intDsize==16)
    #define set_pFN_maxlength_digits_at(_ptr,_wert)  \
      set_highlow32_at(_ptr,_wert)
  #elif (pFN_maxlength==2)
    #define set_pFN_maxlength_digits_at(_ptr,_wert)  \
      (_ptr[0] = (uintD)(_wert>>intDsize), \
       _ptr[1] = (uintD)(_wert)            \
      )
  #elif (pFN_maxlength==3)
    #define set_pFN_maxlength_digits_at(_ptr,_wert)  \
      (_ptr[0] = (uintD)(_wert>>(2*intDsize)), \
       _ptr[1] = (uintD)(_wert>>intDsize),     \
       _ptr[2] = (uintD)(_wert)                \
      )
  #elif (pFN_maxlength==4)
    #define set_pFN_maxlength_digits_at(_ptr,_wert)  \
      (_ptr[0] = (uintD)(_wert>>(3*intDsize)), \
       _ptr[1] = (uintD)(_wert>>(2*intDsize)), \
       _ptr[2] = (uintD)(_wert>>intDsize),     \
       _ptr[3] = (uintD)(_wert)                \
      )
  #endif

