# Vergleich von Integers

# I_I_comp(x,y) vergleicht zwei Integers x und y.
# Ergebnis: 0 falls x=y, +1 falls x>y, -1 falls x<y.
  global signean I_I_comp (object x, object y);
  global signean I_I_comp(x,y)
    var reg3 object x;
    var reg4 object y;
    { # Methode:
      # x und y haben verschiedenes Vorzeichen ->
      #    x < 0 -> x < y
      #    x >= 0 -> x > y
      # x und y haben gleiches Vorzeichen ->
      # x Fixnum ->
      #    y Fixnum -> direkt vergleichen.
      #    y Bignum ->
      #       y > 0 -> x < y
      #       y < 0 -> x > y
      # x Bignum ->
      #    y Fixnum ->
      #       x < 0 -> x < y
      #       x > 0 -> x > y
      #    y Bignum ->
      #       falls beide gleich lang -> wortweise vergleichen
      #       x kürzer als y -> bei x,y > 0 : x < y, bei x,y < 0 : x > y
      #       y kürzer als x -> bei x,y > 0 : x > y, bei x,y > 0 : x < y
      var reg2 uintC xlen;
      var reg2 uintC ylen;
      if (!(R_minusp(x)))
        # x>=0
        if (!(R_minusp(y)))
          # x>=0, y>=0
          if (I_fixnump(x))
            # x Fixnum >=0, y>=0
            if (I_fixnump(y))
              { # x Fixnum >=0, y Fixnum >=0
                if ((oint)x == (oint)y) return signean_null;
                else if ((oint)x > (oint)y) return signean_plus;
                else return signean_minus;
              }
              else
              # x Fixnum >=0, y Bignum >0
              return signean_minus; # x<y
            else
            # x Bignum >0, y>=0
            if (I_fixnump(y))
              # x Bignum >0, y Fixnum >=0
              return signean_plus; # x>y
              else
              # x und y Bignums >0
              if (eq(x,y))
                return signean_null; # gleiche Pointer -> selbe Zahl
                else
                { xlen = TheBignum(x)->length;
                  ylen = TheBignum(y)->length;
                  if (xlen==ylen)
                    samelength:
                    # gleiche Länge -> digitweise vergleichen
                    return compare_loop_up(&TheBignum(x)->data[0],&TheBignum(y)->data[0],xlen);
                    else
                    return (xlen > ylen ? signean_plus : signean_minus);
                }
          else
          # x>=0, y<0
          return signean_plus; # x>y
        else
        # x<0
        if (!(R_minusp(y)))
          # x<0, y>=0
          return signean_minus; # x<y
          else
          # x<0, y<0
          if (I_fixnump(x))
            # x Fixnum <0, y<0
            if (I_fixnump(y))
              { # x Fixnum <0, y Fixnum <0
                if ((oint)x == (oint)y) return signean_null;
                else if ((oint)x > (oint)y) return signean_plus;
                else return signean_minus;
              }
              else
              # x Fixnum <0, y Bignum <0
              return signean_plus; # x>y
            else
            # x Bignum <0, y<0
            if (I_fixnump(y))
              # x Bignum <0, y Fixnum <0
              return signean_minus; # x<y
              else
              # x und y Bignums <0
              if (eq(x,y))
                return signean_null; # gleiche Pointer -> selbe Zahl
                else
                { xlen = TheBignum(x)->length;
                  ylen = TheBignum(y)->length;
                  if (xlen==ylen)
                    # gleiche Länge -> wortweise vergleichen
                    goto samelength; # wie oben
                    else
                    return (xlen > ylen ? signean_minus : signean_plus);
                }
    }

