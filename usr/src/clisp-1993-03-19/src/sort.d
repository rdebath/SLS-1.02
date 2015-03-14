# n log(n) - Sortierroutine für CLISP
# Bruno Haible 9.6.1992

# Ziel: Eine feste Anzahl n von Elementen zu sortieren,
# mit maximalem Zeitaufwand von O(n log(n)),
# und dies, ohne allzu aufwendige Datenstrukturen aufzubauen.

# Von außen ist einzustellen:
# Identifier SORTID :
#   Identifier, der die Inkarnation dieser Package identifiziert
# Typ SORT_ELEMENT :
#   Typ der Elemente, die zu sortieren sind.
# Typ SORT_KEY :
#   Typ des Key, nach dem sortiert wird.
# Funktion SORT_KEYOF, mit
#   local SORT_KEY SORT_KEYOF (SORT_ELEMENT element);
#   liefert den Sortier-Key eines Elements.
# Funktion SORT_COMPARE, mit
#   local signean SORT_COMPARE (SORT_KEY key1, SORT_KEY key2);
#   liefert >0 falls key1>key2, <0 falls key1<key2, 0 falls key1=key2.
# Funktion SORT_LESS, mit
#   local boolean SORT_LESS (SORT_KEY key1, SORT_KEY key2);
#   liefert TRUE falls key1<key2, FALSE falls key1>=key2.

#ifndef SORT
  # Eine Art "SORT-Package"
  #define SORT(incarnation,identifier)  CONCAT4(sort_,incarnation,_,identifier)
#endif

# Quelle: Samuel P. Harbison, Guy L. Steele: C - A Reference Manual, S. 61

# Feststellen, ob element1 < element2 gilt:
  #define less(element1,element2)  \
    SORT_LESS(SORT_KEYOF(element1),SORT_KEYOF(element2))

# sort(v,n); sortiert den Array v[0]..v[n-1] in aufsteigende Reihenfolge.
  local void SORT(SORTID,sort) (SORT_ELEMENT* v, uintL n);
  local void SORT(SORTID,sort) (v,n)
    var reg7 SORT_ELEMENT* v;
    var reg6 uintL n;
    { var reg3 SORT_ELEMENT* w = &v[-1];
      # w[1]..w[n] ist dasselbe wie v[0]..v[n-1] .
      # Man faßt die Zahlen 1,...,n so zu einem balancierten Binärbaum
      # zusammen, daß k die Söhne 2*k und 2*k+1 habe.
      # Ein Teilstück w[r]..w[s] heißt sortiert, wenn für alle
      # k mit r <= k <= s gilt:
      #   Falls 2*k <= s, gilt w[k] >= w[2*k], und
      #   falls 2*k+1 <= s, gilt w[k] >= w[2*k+1],
      # d.h. wenn jedes Element einen Wert >= dem Wert seiner Söhne hat.
      # Teilaufgabe:
      #   Sei 0<r<=s und w[r+1]..w[s] bereits sortiert.
      #   Sortiere w[r]..w[s].
      #   Zeitaufwand: max. O(log(s)).
        #define adjust(r,s)  \
          { var reg2 uintL i = r;                                                       \
            loop # w[i] ist im Teilbaum unterhalb von i unterzubringen                  \
              { var reg1 uintL j = 2*i; # ein Sohn von i                                \
                if (j > s) break; # 2*i und 2*i+1 nicht mehr vorhanden -> fertig        \
                if ((j < s) && less(w[j],w[j+1])) { j++; } # evtl. j = 2*i+1, der andere Sohn von i \
                # j ist der Sohn von i mit dem größeren Wert.                           \
                if (less(w[i],w[j])) # Falls w[i] < w[j],                               \
                  { swap(reg4 SORT_ELEMENT, w[i], w[j]); } # w[i] und w[j] vertauschen  \
                # w[i] ist nun der größere der drei Werte w[i],w[2*i],w[2*i+1].         \
                # Jetzt haben wir aber w[j] verkleinert, so daß ein                     \
                # tail-rekursives adjust(j,s) nötig wird:                               \
                i = j;                                                                  \
          }   }
      if (n<=1) return; # nichts zu tun?
      # Wegen 2*(floor(n/2)+1) > n ist w[floor(n/2)+1]..w[n] bereits sortiert.
      { var reg5 uintL r;
        for (r = floor(n,2); r>0; r--)
          { # Hier ist w[r+1]..w[n] sortiert.
            adjust(r,n);
            # Hier ist w[r]..w[n] sortiert.
      }   }
      # Nun ist w[1]..w[n] ein sortierter Baum.
      # Jeweils das höchste Element w[1] entnehmen und ans Ende setzen:
      { var reg5 uintL s;
        for (s = n-1; s>0; s--)
          { # Hier ist w[1]..w[s+1] ein sortierter Baum, und
            # w[s+2]..w[n] die höchsten Elemente, aufsteigend sortiert.
            swap(reg1 SORT_ELEMENT, v[0], v[s]); # w[1] und w[s+1] vertauschen
            # Hier ist w[2]..w[s] ein sortierter Baum, und
            # w[s+1]..w[n] die höchsten Elemente, aufsteigend sortiert.
            adjust(1,s); # w[1] in den Baum hineinsortieren
            # Hier ist w[1]..w[s] ein sortierter Baum, und
            # w[s+1]..w[n] die höchsten Elemente, aufsteigend sortiert.
      }   }
    }

#undef adjust
#undef less

