# Package-Verwaltung für CLISP
# Bruno Haible 23.3.1992

#include "lispbibl.c"
#include "arilev0.c" # für Hashcode-Berechnung

# Datenstruktur des Symbols: siehe LISPBIBL.D
# Datenstruktur der Symboltabelle:
# ein Vektor mit 3 Slots:
#   size    Fixnum >0, <2^16, = Länge der table
#   table   Vektor der Länge size,
#             enthält einzelne Symbole (/= NIL) und Symbollisten
#   count   Anzahl der Symbole in der Table, Fixnum >=0
  #define Symtab_size(symtab)  (TheSvector(symtab)->data[0])
  #define Symtab_table(symtab)  (TheSvector(symtab)->data[1])
  #define Symtab_count(symtab)  (TheSvector(symtab)->data[2])
# Konsistenzregel:
# Zu jedem String gibt es in der Tabelle höchstens ein Symbol mit diesem
# Printnamen.

# UP: Kreiert eine neue leere Symboltabelle.
# make_symtab(size)
# > size: gewünschte Größe der Tabelle (ungerade, >0, <2^16)
# < ergebnis: neue Symboltabelle dieser Größe
# kann GC auslösen
  local object make_symtab (uintL size);
  local object make_symtab(size)
    var reg2 uintL size;
    { var reg3 object table = allocate_vector(size); # Vektor mit size NIL-Einträgen
      pushSTACK(table);
     {var reg1 object symtab = allocate_vector(3); # Vektor der Länge 3
      Symtab_table(symtab) = popSTACK(); # table einfüllen
      Symtab_size(symtab) = fixnum(size); # size einfüllen
      Symtab_count(symtab) = Fixnum_0; # count := 0 einfüllen
      return symtab;
    }}

# UP: berechnet den Hashcode eines Strings. Dies ist eine 16-Bit-Zahl.
# string_hashcode(string)
# > string: ein String.
# < ergebnis: der Hashcode des Strings
  local uint16 string_hashcode (object string);
  local uint16 string_hashcode(string)
    var reg5 object string;
    { var uintL len;
      var reg2 uintB* charptr = unpack_string(string,&len);
      # ab charptr kommen len Zeichen
      var reg1 uint32 hashcode = 0; # Hashcode, nur die unteren 16 Bit sind wesentlich
      var reg3 uintC count;
      dotimesC(count, (len>16 ? 16 : len), # min(len,16) mal:
        { # hashcode um 5 Bit nach links rotieren:
          hashcode = hashcode << 5; hashcode = hashcode | high16(hashcode);
          # und nächstes Byte dazuXORen:
          hashcode = hashcode ^ (uint32)(*charptr++);
        });
      return (uint16)hashcode;
    }

# UP: Reorganisiert eine Symboltabelle, nachdem sie gewachsen ist, und
# versucht dabei Conses zu sparen.
# rehash_symtab(symtab)
# > symtab: Symboltabelle
# < ergebnis: reorganisierte Symboltabelle (EQ zur ersten).
# nur bei gesetzter BREAK_SEM_2 aufzurufen
# kann GC auslösen
  local object rehash_symtab (object symtab);
  #
  # Hilfsfunktionen:
  #
  # Entnimmt ein Cons aus free-conses oder liefert ein frisches.
  # new_cons()
  # < ergebnis: neues Cons.
  # Stackaufbau: free-conses, newtable, listr, symbol, entry.
  # kann GC auslösen
    local object new_cons (void);
    local object new_cons()
      { var reg1 object free = STACK_4; # free-conses
        if (!nullp(free))
          { STACK_4 = Cdr(free); # free-conses verkürzen
            return free;
          }
          else
          { return allocate_cons(); } # neues Cons aus der Speicherverwaltung anfordern
      }
  #
  # Fügt ein Symbol zusätzlich in die neue Tabelle ein.
  # newinsert(sym,size);
  # > sym: Symbol
  # Stackaufbau: tab, oldtable, free-conses, newtable, listr.
  # kann GC auslösen
    local void newinsert (object sym, uintWL size);
    local void newinsert(sym,size)
      var reg4 object sym;
      var reg5 uintWL size;
      { var reg2 uintL index = # Index = Hashcode mod size
                       (uintL)(string_hashcode(Symbol_name(sym)) % size);
        var reg3 object entry = TheSvector(STACK_1)->data[index]; # entry in der newtable
        if ((!nullp(entry)) || nullp(sym))
          # Ist entry=NIL und sym/=NIL, so ist einfach sym einzutragen.
          # Sonst muß entry durch Consen erweitert werden:
          { pushSTACK(sym); # Symbol retten
            pushSTACK(entry); # entry retten
            if (!listp(entry))
              # Falls entry keine Liste ist, durch (new-cons entry NIL) ersetzen:
              { var reg1 object new_entry = new_cons();
                Cdr(new_entry) = NIL; Car(new_entry) = STACK_0;
                STACK_0 = new_entry;
              }
            # und Symbol davorconsen:
            { var reg1 object new_entry = new_cons();
              Cdr(new_entry) = popSTACK(); # entry bzw. Liste als CDR eintragen
              Car(new_entry) = popSTACK(); # Symbol als CAR eintragen
              sym = new_entry; # und dann new_entry eintragen
          } }
        TheSvector(STACK_1)->data[index] = sym; # neue Entry in newtable eintragen
      }
  #
  local object rehash_symtab(symtab)
    var reg6 object symtab;
    { pushSTACK(symtab); # Symboltabelle retten
     {var reg5 uintL oldsize = posfixnum_to_L(Symtab_size(symtab)); # alte Größe
      var reg6 uintL newsize; # neue Größe
      var reg4 object size; # neue Größe (als Fixnum)
      pushSTACK(Symtab_table(symtab)); # oldtable = alter Tabellenvektor
      pushSTACK(NIL); # free-conses := NIL
      # neue Größe = min(floor(oldsize*1.6),65535)
      { # multipliziere oldsize (>0, <2^16) mit 1.6*2^15, dann durch 2^15 :
        var reg1 uint32 prod = mulu16(oldsize,52429UL);
        newsize = (prod < (1UL<<31) ? prod>>15 : (1UL<<16)-1 );
      } # newsize ist jetzt >= oldsize > 0 und < 2^16
      # newsize durch Abrunden ungerade machen:
      newsize = (newsize - 1) | 1 ;
      # size berechnen:
      size = fixnum(newsize);
      # Bei newsize <= oldsize braucht die Tabelle nicht vergrößert zu werden:
      if (newsize <= oldsize) { skipSTACK(3); return symtab; }
      { var reg1 object newtable = allocate_vector(newsize); # neuer Vektor mit size NILs
        pushSTACK(newtable); # retten
      }
      # Hier könnte man gegen Unterbrechungen schützen.
      # Stackaufbau: tab, oldtable, free-conses, newtable.
      # Symbole von oldtable nach newtable übertragen:
        # Erst die Symbole verarbeiten, die auf Listen sitzen
        # (dabei werden evtl. Conses frei):
        { var reg3 object* offset = 0; # offset = sizeof(object)*index
          var reg2 uintC count;
          dotimespC(count,oldsize,
            { var reg1 object oldentry = # Eintrag mit Nummer index in oldtable
                  *(object*)(pointerplus(&TheSvector(STACK_2)->data[0],(aint)offset));
              if (consp(oldentry)) # diesmal nur nichtleere Symbollisten verarbeiten
                do { pushSTACK(Cdr(oldentry)); # Restliste retten
                     Cdr(oldentry) = STACK_2; STACK_2 = oldentry; # oldentry vor free-conses consen
                     newinsert(Car(oldentry),newsize); # Symbol in die neue Tabelle eintragen
                     oldentry = popSTACK(); # Restliste
                   }
                   while (consp(oldentry));
              offset++;
            });
        }
        # Dann die Symbole verarbeiten, die kollisionsfrei dasitzen:
        { var reg3 object* offset = 0; # offset = sizeof(object)*index
          var reg2 uintC count;
          dotimespC(count,oldsize,
            { var reg1 object oldentry = # Eintrag mit Nummer index in oldtable
                  *(object*)(pointerplus(&TheSvector(STACK_2)->data[0],(aint)offset));
              if (!(listp(oldentry))) # diesmal nur Symbole /= NIL verarbeiten
                { pushSTACK(oldentry); # Dummy, damit der Stack stimmt
                  newinsert(oldentry,newsize); # in die neue Tabelle eintragen
                  skipSTACK(1);
                }
              offset++;
            });
        }
        # Stackaufbau: tab, oldtable, free-conses, newtable.
      # tab aktualisieren:
      { var reg1 object newtable = popSTACK(); # newtable
        skipSTACK(2);
        symtab = popSTACK(); # tab
        Symtab_size(symtab) = size;
        Symtab_table(symtab) = newtable;
      }
      # Hier könnte man Unterbrechungen wieder zulassen.
      return symtab;
    }}

# UP: Sucht ein Symbol gegebenen Printnamens in einer Symboltabelle.
# symtab_lookup(string,symtab,&sym)
# > string: String
# > symtab: Symboltabelle
# < ergebnis: TRUE falls gefunden, FALSE falls nicht gefunden.
# falls gefunden:
#   < sym: das Symbol aus der Symboltabelle, das den gegebenen Printnamen hat
  local boolean symtab_lookup (object string, object symtab, object* sym_);
  local boolean symtab_lookup(string,symtab,sym_)
    var reg3 object string;
    var reg4 object symtab;
    var reg5 object* sym_;
    { var reg2 uintL index = # Index = Hashcode mod size
          (uintL)(string_hashcode(string) % (uintW)(posfixnum_to_L(Symtab_size(symtab))));
      var reg1 object entry = TheSvector(Symtab_table(symtab))->data[index]; # entry in der table
      if (!(listp(entry)))
        # entry ist ein einzelnes Symbol
        { # erster String und Printname des gefundenen Symbols gleich ?
          if (string_gleich(string,Symbol_name(entry)))
            { *sym_ = entry; return TRUE; }
            else
            { return FALSE; }
        }
        else
        # entry ist eine Symbolliste
        { while (consp(entry))
            { # erster String und Printname des Symbols gleich ?
              if (string_gleich(string,Symbol_name(Car(entry)))) { goto found; }
              entry = Cdr(entry);
            }
          { return FALSE; } # nicht gefunden
          found: # gefunden als CAR von entry
          { *sym_ = Car(entry); return TRUE; }
        }
    }

# UP: Sucht ein gegebenes Symbol in einer Symboltabelle.
# symtab_find(sym,symtab)
# > sym: Symbol
# > symtab: Symboltabelle
# < ergebnis: TRUE wenn gefunden
  local boolean symtab_find (object sym, object symtab);
  local boolean symtab_find(sym,symtab)
    var reg3 object sym;
    var reg4 object symtab;
    { var reg2 uintL index = # Index = Hashcode mod size
          (uintL)(string_hashcode(Symbol_name(sym)) % (uintW)(posfixnum_to_L(Symtab_size(symtab))));
      var reg1 object entry = TheSvector(Symtab_table(symtab))->data[index]; # entry in der table
      if (!(listp(entry)))
        # entry ist ein einzelnes Symbol
        { # sym und gefundenes Symbol gleich ?
          if (eq(sym,entry)) { return TRUE; } else { return FALSE; }
        }
        else
        # entry ist eine Symbolliste
        { while (consp(entry))
            { # sym und Symbol aus entry gleich ?
              if (eq(sym,Car(entry))) { goto found; }
              entry = Cdr(entry);
            }
          { return FALSE; } # nicht gefunden
          found: # gefunden als CAR von entry
          { return TRUE; }
        }
    }

# UP: Fügt ein gegebenes Symbol in eine Symboltabelle ein (destruktiv).
# symtab_insert(sym,symtab)
# > sym: Symbol
# > symtab: Symboltabelle
# < ergebnis: neue Symboltabelle, EQ zur alten
# nur bei gesetzter BREAK_SEM_2 aufzurufen
# kann GC auslösen
  local object symtab_insert (object sym, object symtab);
  local object symtab_insert(sym,symtab)
    var reg4 object sym;
    var reg3 object symtab;
    { # erst der Test, ob Reorganisieren nötig ist:
      { var reg1 uintL size = posfixnum_to_L(Symtab_size(symtab));
        var reg2 uintL count = posfixnum_to_L(Symtab_count(symtab));
        # Bei count>=2*size muß die Tabelle reorganisiert werden:
        if (count >= 2*size)
          { pushSTACK(sym); # Symbol retten
            symtab = rehash_symtab(symtab);
            sym = popSTACK();
          }
      }
      # Dann das Symbol einfügen:
     {var reg2 uintL index = # Index = Hashcode mod size
          (uintL)(string_hashcode(Symbol_name(sym)) % (uintW)(posfixnum_to_L(Symtab_size(symtab))));
      var reg1 object entry = TheSvector(Symtab_table(symtab))->data[index]; # entry in der table
      if ((!(nullp(entry))) || (nullp(sym)))
        # Ist entry=NIL und sym/=NIL, so ist einfach sym einzutragen.
        # Sonst muß entry durch Consen erweitert werden:
        { pushSTACK(symtab); # symtab retten
          pushSTACK(sym); # Symbol retten
          pushSTACK(entry); # entry retten
          if (!(listp(entry)))
            # Falls entry keine Liste ist, durch (cons entry NIL) ersetzen:
            { var reg1 object new_entry = allocate_cons();
              Car(new_entry) = STACK_0;
              STACK_0 = new_entry;
            }
          # und Symbol davorconsen:
          { var reg1 object new_entry = allocate_cons();
            Cdr(new_entry) = popSTACK(); # entry bzw. Liste als CDR eintragen
            Car(new_entry) = popSTACK(); # Symbol als CAR eintragen
            sym = new_entry; # und dann new_entry eintragen
          }
          symtab = popSTACK();
        }
      TheSvector(Symtab_table(symtab))->data[index] = sym; # neue Entry eintragen
      Symtab_count(symtab) = fixnum_inc(Symtab_count(symtab),1); # (incf count)
      return symtab;
    }}

# UP: Entfernt aus einer Symboltabelle ein darin vorkommendes Symbol.
# symtab_delete(sym,symtab)
# > sym: Symbol
# > symtab: Symboltabelle
  local void symtab_delete (object sym, object symtab);
  local void symtab_delete(sym,symtab)
    var reg3 object sym;
    var reg4 object symtab;
    { var reg2 uintL index = # Index = Hashcode mod size
          (uintL)(string_hashcode(Symbol_name(sym)) % (uintW)(posfixnum_to_L(Symtab_size(symtab))));
      var reg2 object* entryptr = &TheSvector(Symtab_table(symtab))->data[index];
      var reg1 object entry = *entryptr; # entry in der table
      if (!(listp(entry)))
        # entry ist ein einzelnes Symbol
        { # sym und gefundenes Symbol gleich ?
          if (!eq(sym,entry)) { goto notfound; }
          # entry durch NIL ersetzen:
          *entryptr = NIL;
        }
        else
        # entry ist eine Symbolliste
        { while (consp(entry))
            { # sym und Symbol aus entry gleich ?
              if (eq(sym,Car(entry))) { goto found; }
              entryptr = &Cdr(entry); entry = *entryptr;
            }
          goto notfound; # nicht gefunden
          found: # gefunden als CAR von *entryptr = entry
                 # -> ein Listenelement streichen:
          { *entryptr = Cdr(entry); } # entry durch Cdr(entry) ersetzen
        }
      # schließlich noch den Symbolzähler um 1 erniedrigen:
      Symtab_count(symtab) = fixnum_inc(Symtab_count(symtab),-1); # (decf count)
      return;
      # nicht gefunden
      notfound:
        pushSTACK(sym);
        fehler(
               DEUTSCH ? "Symbol ~ kann nicht aus der Symboltabelle entfernt werden." :
               ENGLISH ? "symbol ~ cannot be deleted from symbol table" :
               FRANCAIS ? "Le symbole ~ ne peux pas être retiré de la table des symboles." :
               ""
              );
    }

# Datenstruktur der Package siehe LISPBIBL.D.
# Komponenten:
# pack_external_symbols   Symboltabelle der extern präsenten Symbole
# pack_internal_symbols   Symboltabelle der intern präsenten Symbole
# pack_shadowing_symbols  Liste der Shadowing-Symbole
# pack_use_list           Use-List, eine Liste von Packages
# pack_used_by_list       Used-by-List, eine Liste von Packages
# pack_name               der Name, ein Simple-String
# pack_nicknames          die Nicknames, eine Liste von Simple-Strings

# Konsistenzregeln:
# 1. Alle Packages sind genau einmal in ALL_PACKAGES aufgeführt.
# 2. Die Vereinigung über ALL_PACKAGES von {Name} U Nicknames ist disjunkt.
# 3. Für je zwei Packages p,q gilt:
#    p in use_list(q) <==> q in used_by_list(q)
# 4. p sei eine Package.
#    accessible(p) = ISymbols(p) U ESymbols(p) U
#                    U {ESymbols(q) | q in use_list(p)}
# 5. Für jede Package p ist
#    shadowing_symbols(p)  eine Teilmenge von  ISymbols(p) U ESymbols(p)
#    und damit auch eine Teilmenge von  accessible(p).
# 6. s sei ein String, p eine Package.
#    Ist die Menge der Symbole in accessible(p) mit dem Printnamen = s
#    mehr als einelementig,
#    so liegt genau eines dieser Symbole in shadowing_symbols(p).
# 7. s sei ein String, p eine Package.
#    Es gibt höchstens ein Symbol mit dem Printnamen = s
#    in  ISymbols(p) U ESymbols(p)  .
# 8. Ist s ein Symbol mit der Home-Package p /= NIL,
#    so ist s in  ISymbols(p) U ESymbols(p)  enthalten.

# UP: Erzeugt eine neue Package, ohne auf Namenskonflikte zu testen.
# make_package(name,nicknames)
# > name: Name (ein Simple-String)
# > nicknames: Nicknames (eine Liste von Simple-Strings)
# < ergebnis: neue Package
# kann GC auslösen
  local object make_package (object name, object nicknames);
  local object make_package(name,nicknames)
    var reg3 object name;
    var reg4 object nicknames;
    { set_break_sem_2();
      pushSTACK(nicknames); pushSTACK(name); # Nicknames und Namen retten
      # Tabelle für externe Symbole erzeugen:
      { var reg1 object symtab = make_symtab(11); pushSTACK(symtab); }
      # Tabelle für interne Symbole erzeugen:
      { var reg1 object symtab = make_symtab(63); pushSTACK(symtab); }
      # neue Package erzeugen:
      { var reg1 object pack = allocate_package();
        # und füllen:
        ThePackage(pack)->pack_internal_symbols = popSTACK();
        ThePackage(pack)->pack_external_symbols = popSTACK();
        ThePackage(pack)->pack_shadowing_symbols = NIL;
        ThePackage(pack)->pack_use_list = NIL;
        ThePackage(pack)->pack_used_by_list = NIL;
        ThePackage(pack)->pack_name = popSTACK();
        ThePackage(pack)->pack_nicknames = popSTACK();
        # und in ALL_PACKAGES einhängen:
        pushSTACK(pack);
       {var reg2 object new_cons = allocate_cons();
        pack = popSTACK();
        Car(new_cons) = pack; Cdr(new_cons) = O(all_packages);
        O(all_packages) = new_cons;
        # fertig:
        clr_break_sem_2();
        return pack;
    } }}

# UP: Sucht ein Symbol gegebenen Printnamens in der Shadowing-Liste einer
# Package.
# shadowing_lookup(string,pack,&sym)
# > string: String
# > pack: Package
# < ergebnis: TRUE, falls gefunden.
# < sym: das Symbol aus der Shadowing-Liste, das den gegebenen Printnamen hat
#        (falls gefunden)
  local boolean shadowing_lookup (object string, object pack, object* sym_);
  local boolean shadowing_lookup(string,pack,sym_)
    var reg2 object string;
    var reg4 object pack;
    var reg3 object* sym_;
    { var reg1 object list = ThePackage(pack)->pack_shadowing_symbols;
      # Shadowing-Liste durchlaufen:
      while (consp(list))
        { if (string_gleich(string,Symbol_name(Car(list)))) { goto found; }
          list = Cdr(list);
        }
      return FALSE; # nicht gefunden
      found: # gefunden
        *sym_ = Car(list); return TRUE;
    }

# UP: Sucht ein gegebenes Symbol in der Shadowing-Liste einer Package.
# shadowing_find(sym,pack)
# > sym: Symbol
# > pack: Package
# < ergebnis: TRUE falls gefunden.
  local boolean shadowing_find (object sym, object pack);
  local boolean shadowing_find(sym,pack)
    var reg2 object sym;
    var reg3 object pack;
    { var reg1 object list = ThePackage(pack)->pack_shadowing_symbols;
      # Shadowing-Liste durchlaufen:
      while (consp(list))
        { if (eq(sym,Car(list))) { goto found; }
          list = Cdr(list);
        }
      return FALSE; # nicht gefunden
      found: # gefunden
        return TRUE;
    }

# UP: Fügt ein Symbol zur Shadowing-Liste einer Package, die noch kein
# Symbol desselben Namens enthält, hinzu.
# shadowing_insert(&sym,&pack)
# > sym: Symbol (im STACK)
# > pack: Package (im STACK)
# < sym: Symbol, EQ zum alten
# < pack: Package, EQ zur alten
# kann GC auslösen
  local void shadowing_insert (object* sym_, object* pack_);
  local void shadowing_insert(sym_,pack_)
    var reg2 object* sym_;
    var reg3 object* pack_;
    { # neues Cons mit Symbol als CAR vor die Shadowing-Symbols einhängen:
      var reg1 object new_cons = allocate_cons();
      var reg2 object pack = *pack_;
      Car(new_cons) = *sym_;
      Cdr(new_cons) = ThePackage(pack)->pack_shadowing_symbols;
      ThePackage(pack)->pack_shadowing_symbols = new_cons;
    }

# UP: Entfernt ein Symbol gegebenen Namens aus der Shadowing-Liste
# einer Package.
# shadowing_delete(string,pack)
# > string: String
# > pack: Package
  local void shadowing_delete (object string, object pack);
  local void shadowing_delete(string,pack)
    var reg3 object string;
    var reg4 object pack;
    { var reg2 object* listptr = &ThePackage(pack)->pack_shadowing_symbols;
      var reg1 object list = *listptr;
      # list = *listptr durchläuft die Shadowing-Liste
      while (consp(list))
        { if (string_gleich(string,Symbol_name(Car(list)))) { goto found; }
          listptr = &Cdr(list); list = *listptr;
        }
      # kein Symbol dieses Namens gefunden, fertig.
      return;
      found:
        # Gleichheit: entfernen. Danach ist man fertig, da es in der
        # Shadowing-Liste nur ein Symbol desselben Printnamens geben kann.
        *listptr = Cdr(list); # list durch Cdr(list) ersetzen
        return;
    }

# UP: testet, ob ein Symbol in einer Package accessible ist und dabei nicht
# von einem anderen Symbol desselben Namens verdeckt wird.
# accessiblep(sym,pack)
# > sym: Symbol
# > pack: Package
# < ergebnis: TRUE falls sym in pack accessible und nicht verdeckt ist,
#             FALSE sonst
  global boolean accessiblep (object sym, object pack);
  global boolean accessiblep(sym,pack)
    var reg2 object sym;
    var reg3 object pack;
    { # Methode:
      # Suche erst ein Symbol gleichen Namens in der Shadowing-Liste;
      # falls nicht gefunden, suche das Symbol unter den präsenten und dann
      # unter den vererbten Symbolen.
      # Andere mögliche Methode (hier nicht realisiert):
      # Ist die Home-Package von sym gleich pack, so ist sym in pack präsent,
      # fertig. Sonst suche ein präsentes Symbol gleichen Namens.
      # sym gefunden -> fertig.
      # Ein anderes gefunden -> sym ist nicht auf der Shadowing-Liste und
      # daher nicht sichtbar.
      # Keins gefunden -> Suche sym unter den vererbten Symbolen.
      var object shadowingsym;
      # Suche erst in der Shadowing-Liste von pack:
      if (shadowing_lookup(Symbol_name(sym),pack,&shadowingsym))
        { # shadowingsym = in der Shadowing-Liste gefundenes Symbol
          return (eq(shadowingsym,sym)); # mit sym vergleichen
        }
        else
        # Kein Symbol gleichen Namens in der Shadowing-Liste
        { # Suche unter den internen Symbolen:
          if (symtab_find(sym,ThePackage(pack)->pack_internal_symbols))
            goto found;
          # Suche unter den externen Symbolen:
          if (symtab_find(sym,ThePackage(pack)->pack_external_symbols))
            goto found;
          # Suche unter den externen Symbolen der Packages aus der Use-List:
          { var reg1 object list = ThePackage(pack)->pack_use_list;
            while (consp(list))
              { if (symtab_find(sym,ThePackage(Car(list))->pack_external_symbols))
                  goto found;
                list = Cdr(list);
              }
            return FALSE; # nicht gefunden
          }
          found: # gefunden
            return TRUE;
    }   }

# UP: testet, ob ein Symbol in einer Package als externes Symbol accessible
# ist.
# externalp(sym,pack)
# > sym: Symbol
# > pack: Package
# < ergebnis:
#     TRUE falls sym in pack als externes Symbol accessible ist,
#     (in diesem Falle ist sym nicht verdeckt, denn ein eventuell sym
#      vedeckendes Symbol müßte in shadowing-symbols(pack) aufgeführt sein,
#      nach den Konsistenzregeln 5 und 7 also mit sym identisch sein),
#     FALSE sonst
  global boolean externalp (object sym, object pack);
  global boolean externalp(sym,pack)
    var reg2 object sym;
    var reg1 object pack;
    { return symtab_find(sym,ThePackage(pack)->pack_external_symbols); }

# UP: sucht ein externes Symbol gegebenen Printnamens in einer Package.
# find_external_symbol(string,pack,&sym)
# > string: String
# > pack: Package
# < ergebnis: TRUE, falls ein externes Symbol dieses Printnamens in pack gefunden.
# < sym: dieses Symbol, falls gefunden.
  global boolean find_external_symbol (object string, object pack, object* sym_);
  global boolean find_external_symbol(string,pack,sym_)
    var reg2 object string;
    var reg3 object pack;
    var reg1 object* sym_;
    { return symtab_lookup(string,ThePackage(pack)->pack_external_symbols,&(*sym_)); }

# UP: Nachfragefunktion an den Benutzer.
# query_user(ml)
# > ml: nichtleere Liste von Möglichkeiten. Jede Möglichkeit ist dabei eine
#       Liste aus einem Kurz-String (den der Benutzer eintippen soll), einem
#       Langstring (der der Erläuterung dient) und weiteren Informationen.
# < ergebnis: Die vom Benutzer angewählte Möglichkeit.
# kann GC auslösen
  local object query_user (object ml);
  local object query_user(ml)
    var reg5 object ml;
    { pushSTACK(ml);
     {var object stream = var_stream(S(query_io)); # Stream *QUERY-IO*
      terpri(&stream); # Neue Zeile
      write_sstring(&stream,O(query_string1)); # "Wählen Sie bitte aus:"
      # Möglichkeiten ausgeben:
      { var reg2 object mlistr = STACK_0; # restliche Möglichkeiten
        while (consp(mlistr))
          { pushSTACK(mlistr);
            terpri(&stream);
            write_sstring(&stream,O(query_string2)); # "          "
            { var reg1 object moeglichkeit = Car(STACK_0); # nächste Möglichkeit
              pushSTACK(Car(Cdr(moeglichkeit))); # Langstring retten
              write_string(&stream,Car(moeglichkeit)); # Kurzstring ausgeben
              write_sstring(&stream,O(query_string3)); # "  --  "
              write_string(&stream,popSTACK()); # Langstring ausgeben
            }
            mlistr = popSTACK();
            mlistr = Cdr(mlistr);
      }   }
      terpri(&stream);
      terpri(&stream);
      # Benutzer-Antwort einlesen:
      loop
        { write_sstring(&stream,O(query_string7)); # "> "
          pushSTACK(stream); # Stream retten
          pushSTACK(stream); funcall(L(read_line),1); # (READ-LINE stream) aufrufen
          pushSTACK(value1); # Antwort retten
          # Stackaufbau: Möglichkeiten, Stream, Antwort
            # Antwort mit den Kurzstrings vergleichen:
            pushSTACK(STACK_2); # Möglichkeiten durchgehen
            while (mconsp(STACK_0))
              { pushSTACK(Car(Car(STACK_0))); # nächsten Kurzstring
                pushSTACK(STACK_2); # mit Antwort vergleichen:
                funcall(L(string_gleich),2); # (STRING= Kurzstring Antwort)
                if (!nullp(value1)) goto antwort_ok;
                STACK_0 = Cdr(STACK_0); # Möglichkeitenliste verkürzen
              }
            skipSTACK(1);
            # Antwort mit den Kurzstrings vergleichen, diesmal lascher:
            pushSTACK(STACK_2); # Möglichkeiten durchgehen
            while (mconsp(STACK_0))
              { pushSTACK(Car(Car(STACK_0))); # nächsten Kurzstring
                pushSTACK(STACK_2); # mit Antwort vergleichen:
                funcall(L(string_equal),2); # (STRING-EQUAL Kurzstring Antwort)
                if (!nullp(value1)) goto antwort_ok;
                STACK_0 = Cdr(STACK_0); # Möglichkeitenliste verkürzen
              }
            skipSTACK(1);
          skipSTACK(1); # Antwort vergessen
          stream = popSTACK(); # Stream zurück
          # bis jetzt immer noch keine korrekte Antwort
          write_sstring(&stream,O(query_string4)); # "Wählen Sie bitte eines von "
          # Möglichkeiten ausgeben:
          { var reg2 object mlistr = STACK_0; # restliche Möglichkeiten
            while (consp(mlistr))
              { pushSTACK(mlistr);
                write_string(&stream,Car(Car(mlistr))); # Kurzstring ausgeben
                mlistr = popSTACK();
                mlistr = Cdr(mlistr);
                if (atomp(mlistr)) break;
                pushSTACK(mlistr);
                write_sstring(&stream,O(query_string5)); # ", "
                mlistr = popSTACK();
          }   }
          write_sstring(&stream,O(query_string6)); # " aus."
          terpri(&stream);
        }
      antwort_ok:
      { var reg1 object mlistr = popSTACK(); # letzte Möglichkeitenliste
        skipSTACK(3); # Antwort, Stream und Möglichkeitenliste vergessen
        return Car(mlistr); # angewählte Möglichkeit
    }}}

# UP: sucht eine Package mit gegebenem Namen oder Nickname
# find_package(string)
# > string: String
# < ergebnis: Package mit diesem Namen oder NIL
  global object find_package (object string);
  global object find_package(string)
    var reg2 object string;
    { var reg4 object packlistr = O(all_packages); # Package-Liste durchgehen
      var reg3 object pack;
      while (consp(packlistr))
        { pack = Car(packlistr); # zu testende Package
          # Teste Namen:
          if (string_gleich(string,ThePackage(pack)->pack_name)) goto found;
          # Teste Nicknamen:
          { var reg1 object nicknamelistr = ThePackage(pack)->pack_nicknames; # Nickname-Liste durchgehen
            while (consp(nicknamelistr))
              { if (string_gleich(string,Car(nicknamelistr))) goto found;
                nicknamelistr = Cdr(nicknamelistr);
          }   }
          packlistr = Cdr(packlistr); # nächste Package
        }
      # nicht gefunden
      return NIL;
      found: # gefunden
        return pack;
    }

# UP: Sucht ein Symbol gegebenen Printnamens in einer Package.
# find_symbol(string,pack,&sym)
# > string: String
# > pack: Package
# < sym: Symbol, falls gefunden; sonst NIL
# < ergebnis: 0, wenn nicht gefunden
#             1, wenn als externes Symbol vorhanden
#             2, wenn vererbt über use-list
#             3, wenn als internes Symbol vorhanden
  local uintBWL find_symbol (object string, object pack, object* sym_);
  local uintBWL find_symbol(string,pack,sym_)
    var reg4 object string;
    var reg5 object pack;
    var reg3 object* sym_;
    { # Suche erst in der Shadowing-Liste von pack:
      if (shadowing_lookup(string,pack,&(*sym_)))
        # *sym_ = in der Shadowing-Liste gefundenes Symbol
        { # Suche es unter den internen Symbolen:
          if (symtab_find(*sym_,ThePackage(pack)->pack_internal_symbols))
            { return 3; } # unter den internen Symbolen gefunden
          # Suche es unter den externen Symbolen:
          if (symtab_find(*sym_,ThePackage(pack)->pack_external_symbols))
            { return 1; } # unter den externen Symbolen gefunden
          # Widerspruch zur Konsistenzregel 5.
          pushSTACK(*sym_); pushSTACK(pack);
          fehler(
                 DEUTSCH ? "Inkonsistenz in ~ : Symbol ~ ist zwar unter SHADOWING-SYMBOLS vorhanden, aber nicht präsent." :
                 ENGLISH ? "~ inconsistent: symbol ~ is a shadowing symbol but not present" :
                 FRANCAIS ? "Inconsistence dans ~ : Le symbole ~ est énuméré parmi les SHADOWING-SYMBOLS mais n'est pas présent." :
                 ""
                );
        }
        else
        # Symbol noch nicht gefunden
        { # Suche unter den internen Symbolen:
          if (symtab_lookup(string,ThePackage(pack)->pack_internal_symbols,&(*sym_)))
            { return 3; } # unter den internen Symbolen gefunden
          # Suche unter den externen Symbolen:
          if (symtab_lookup(string,ThePackage(pack)->pack_external_symbols,&(*sym_)))
            { return 1; } # unter den externen Symbolen gefunden
          # Suche unter den externen Packages aus der Use-List:
          { var reg2 object packlistr = ThePackage(pack)->pack_use_list;
            while (consp(packlistr))
              { var reg1 object usedpack = Car(packlistr);
                if (symtab_lookup(string,ThePackage(usedpack)->pack_external_symbols,&(*sym_)))
                  { return 2; } # unter den vererbten Symbolen gefunden
                  # (nur einmal vererbt, sonst wäre was in der
                  #  Shadowing-Liste gewesen)
                packlistr = Cdr(packlistr);
          }   }
          # nicht gefunden
          *sym_ = NIL; return 0;
    }   }
    # Eigentlich bräuchte man in der Shadowing-Liste erst zu suchen, nachdem
    # man die präsenten Symbole abgesucht hat, denn das Symbol in der
    # Shadowing-Liste ist ja präsent (Konsistenzregel 5).

# UP: Fügt ein Symbol in eine Package ein, in der noch kein Symbol desselben
# Namens existiert. Achtet nicht auf Konflikte.
# make_present(sym,pack);
# > sym: Symbol
# > pack: Package
# nur bei gesetzter BREAK_SEM_2 aufzurufen
# kann GC auslösen
  local void make_present (object sym, object pack);
  local void make_present(sym,pack)
    var reg1 object sym;
    var reg2 object pack;
    { if (!eq(pack,O(keyword_package)))
        # Symbol in die internen Symbole einfügen:
        { symtab_insert(sym,ThePackage(pack)->pack_internal_symbols); }
        else
        # Symbol modifizieren und in die externen Symbole einfügen:
        { Symbol_value(sym) = sym; # sym erhält sich selbst als Wert
          # als konstant und als Keyword markieren:
          TheSymbol(sym)->header_flags |= (bit(constant_bit_t) | bit(keyword_bit_t));
          symtab_insert(sym,ThePackage(pack)->pack_external_symbols);
    }   }

# UP: Interniert ein Symbol gegebenen Printnamens in einer Package.
# intern(string,pack,&sym)
# > string: String
# > pack: Package
# < sym: Symbol
# < ergebnis: 0, wenn nicht gefunden, sondern neu erzeugt
#             1, wenn als externes Symbol vorhanden
#             2, wenn vererbt über use-list
#             3, wenn als internes Symbol vorhanden
# kann GC auslösen
  global uintBWL intern (object string, object pack, object* sym_);
  global uintBWL intern(string,pack,sym_)
    var reg3 object string;
    var reg4 object pack;
    var reg2 object* sym_;
    { { var reg1 uintBWL ergebnis = find_symbol(string,pack,&(*sym_)); # suchen
        if (!(ergebnis==0)) { return ergebnis; } # gefunden -> fertig
      }
      pushSTACK(pack); # Package retten
      string = coerce_ss(string); # String in Simple-String umwandeln
     {var reg1 object sym = make_symbol(string); # (make-symbol string) ausführen
      pack = popSTACK();
      # dieses neue Symbol in die Package eintragen:
      set_break_sem_2(); # Vor Unterbrechungen schützen
      Symbol_package(sym) = pack; # Home-Package eintragen
      pushSTACK(sym); # Symbol retten
      make_present(sym,pack); # und in diese internieren
      *sym_ = popSTACK();
      clr_break_sem_2(); # Unterbrechungen wieder zulassen
      return 0;
    }}

# UP: Interniert ein Symbol gegebenen Printnamens in der Keyword-Package.
# intern_keyword(string)
# > string: String
# < ergebnis: Symbol, ein Keyword
# kann GC auslösen
  global object intern_keyword (object string);
  global object intern_keyword(string)
    var reg1 object string;
    { var object sym;
      intern(string,O(keyword_package),&sym);
      return sym;
    }

# UP: Importiert ein Symbol in eine Package und macht es zum Shadowing-Symbol.
# Eventuell wird dazu ein anderes in dieser Package präsentes Symbol
# desselben Namens uninterniert.
# shadowing_import(&sym,&pack);
# > sym: Symbol (im STACK)
# > pack: Package (im STACK)
# < sym: Symbol, EQ zum alten
# < pack: Package, EQ zur alten
# kann GC auslösen
  local void shadowing_import (object* sym_, object* pack_);
  local void shadowing_import(sym_,pack_)
    var reg3 object* sym_;
    var reg4 object* pack_;
    { set_break_sem_2(); # Vor Unterbrechungen schützen
     {var reg2 object sym = *sym_;
      var reg1 object pack = *pack_;
      # Suche ein internes oder ein externes Symbol gleichen Namens:
      var object othersym;
      var reg5 boolean i_found;
      var reg6 object string = Symbol_name(sym);
      pushSTACK(string); # String retten
      if ( (i_found = symtab_lookup(string,ThePackage(pack)->pack_internal_symbols,&othersym))
           || (symtab_lookup(string,ThePackage(pack)->pack_external_symbols,&othersym))
         )
        # ein Symbol othersym desselben Namens war schon präsent in der Package
        { if (!eq(othersym,sym)) # war es das zu importierende Symbol selbst?
            { # Nein -> muß othersym aus den internen bzw. aus den externen
              # Symbolen herausnehmen:
              symtab_delete(othersym,
                            i_found ? ThePackage(pack)->pack_internal_symbols
                                    : ThePackage(pack)->pack_external_symbols
                           );
              # Wurde dieses Symbol aus seiner Home-Package herausgenommen,
              # so muß seine Home-Package auf NIL gesetzt werden:
              if (eq(Symbol_package(othersym),pack))
                { Symbol_package(othersym) = NIL; }
              # Symbol sym muß in die Package pack neu aufgenommen werden.
              make_present(sym,pack);
        }   }
        else
        # Symbol sym muß in die Package pack neu aufgenommen werden.
        make_present(sym,pack);
     }
      # Symbol muß in die Shadowing-Liste der Package aufgenommen werden.
      shadowing_delete(popSTACK(),*pack_); # String aus der Shadowing-Liste herausnehmen
      shadowing_insert(&(*sym_),&(*pack_)); # Symbol dafür in die Shadowing-Liste aufnehmen
      clr_break_sem_2(); # Unterbrechungen wieder zulassen
    }

# UP: Überdeckt in einer Package alle aus anderen Packages accessiblen
# Symbole gegebenen Namens durch ein in dieser Package präsentes Symbol
# desselben Namens.
# shadow(&sym,&pack)
# > sym: Symbol (im STACK)
# > pack: Package (im STACK)
# < pack: Package, EQ zur alten
# kann GC auslösen
  local void shadow (object* sym_, object* pack_);
  local void shadow(sym_,pack_)
    var reg2 object* sym_;
    var reg3 object* pack_;
    { set_break_sem_2(); # Vor Unterbrechungen schützen
     {var reg1 object pack = *pack_;
      # Suche ein internes oder ein externes Symbol gleichen Namens:
      var reg4 object string = Symbol_name(*sym_); # Nur der Name des Symbols interessiert.
      pushSTACK(NIL); # Platz für othersym machen
      pushSTACK(string); # String retten
      if (!(symtab_lookup(string,ThePackage(pack)->pack_internal_symbols,&STACK_1)
            || symtab_lookup(string,ThePackage(pack)->pack_external_symbols,&STACK_1)
         ) )
        # nicht gefunden -> neues Symbol desselben Namens erzeugen:
        { var reg1 object othersym = make_symbol(STACK_0); # neues Symbol
          STACK_1 = othersym;
          make_present(othersym,*pack_); # in die Package eintragen
          Symbol_package(STACK_1) = *pack_; # Home-Package des neuen Symbols sei pack
     }  }
      # Stackaufbau: othersym, string
      # In der Package ist nun das Symbol othersym desselben Namens präsent.
      shadowing_delete(popSTACK(),*pack_); # String aus der Shadowing-Liste herausnehmen
      shadowing_insert(&STACK_0,&(*pack_)); # othersym dafür in die Shadowing-Liste aufnehmen
      skipSTACK(1); # othersym vergessen
      clr_break_sem_2(); # Unterbrechungen wieder zulassen
    }

# UP: Entfernt ein Symbol aus der Menge der präsenten Symbole einer Package
# und sorgt für Konfliktauflösung für den Fall, daß es in der Shadowing-List
# dieser Package war und deswegen ein Namenskonflikt entsteht.
# unintern(&sym,&pack)
# > sym: Symbol (im STACK)
# > pack: Package (im STACK)
# < sym: Symbol, EQ zum alten
# < pack: Package, EQ zur alten
# < ergebnis: T wenn gefunden und gelöscht, NIL falls nichts getan.
# kann GC auslösen
  local object unintern (object* sym_, object* pack_);
  local object unintern(sym_,pack_)
    var reg3 object* sym_;
    var reg4 object* pack_;
    { var reg2 object sym = *sym_;
      var reg1 object pack = *pack_;
      var reg5 object symtab;
      # sym unter den internen und den externen Symbolen suchen:
      if (symtab_find(sym,symtab=ThePackage(pack)->pack_internal_symbols)
          || symtab_find(sym,symtab=ThePackage(pack)->pack_external_symbols)
         )
        { # Symbol sym in der Tabelle symtab gefunden
          if (shadowing_find(sym,pack)) # in der Shadowing-Liste suchen
            # möglicher Konflikt -> Auswahlliste aufbauen:
            { pushSTACK(symtab); # Symboltabelle retten
              pushSTACK(NIL); # Möglichkeitenliste anfangen
              pushSTACK(ThePackage(pack)->pack_use_list); # Use-List durchgehen
              # Stackaufbau: Symboltabelle, ML, Use-List-Rest
              while (mconsp(STACK_0))
                { var object othersym;
                  pack = Car(STACK_0); # Package aus der Use-List
                  STACK_0 = Cdr(STACK_0);
                  # vererbtes Symbol gleichen Namens suchen:
                  if (symtab_lookup(Symbol_name(*sym_),ThePackage(pack)->pack_external_symbols,&othersym))
                    # othersym ist ein Symbol gleichen Namens, aus pack vererbt
                    { var reg1 object temp;
                      pushSTACK(temp=ThePackage(pack)->pack_name); # Name von pack
                      pushSTACK(othersym); # Symbol
                       pushSTACK(O(unint_string1)); # "Symbol "
                       pushSTACK(Symbol_name(othersym)); # Symbolname
                       pushSTACK(O(unint_string2)); # " aus #<PACKAGE "
                       pushSTACK(temp); # Packagename
                       pushSTACK(O(unint_string3)); # "> wird als Shadowing deklariert"
                       temp = string_concat(5); # (STRING-CONCAT "..." Symbolname "..." Packagename "...")
                      pushSTACK(temp); # Gesamtstring
                      temp = allocate_cons(); Car(temp) = STACK_1;
                      STACK_1 = temp; # (list othersym)
                      temp = allocate_cons(); Car(temp) = popSTACK(); Cdr(temp) = popSTACK();
                      pushSTACK(temp); # (list Gesamtstring othersym)
                      temp = allocate_cons(); Cdr(temp) = popSTACK(); Car(temp) = popSTACK();
                      # temp = (list Packagename Gesamtstring othersym)
                      # STACK stimmt wieder
                      # auf die Möglichkeitenliste pushen:
                      pushSTACK(temp);
                      temp = allocate_cons();
                      Car(temp) = popSTACK(); Cdr(temp) = STACK_1;
                      STACK_1 = temp;
                }   }
              skipSTACK(1);
              # Möglichkeitenliste fertig aufgebaut.
              # Stackaufbau: Symboltabelle, ML
              # Falls (length ML) >= 2, liegt ein Konflikt vor:
              if (mconsp(STACK_0) && mconsp(Cdr(STACK_0)))
                # Continuable Error auslösen:
                { pushSTACK(O(unint_string4)); # "Sie dürfen auswählen..."
                  pushSTACK(O(unint_string5)); # "Durch Uninternieren von ~S aus ~S ..."
                  pushSTACK(*sym_); # Symbol
                  pushSTACK(*pack_); # Package
                  funcall(S(cerror),4); # (CERROR "..." "..." Symbol Package)
                  STACK_0 = query_user(STACK_0); # Auswahl erfragen
                }
                else
                { STACK_0 = NIL; }
              # STACK_0 ist die Auswahl (NIL falls kein Konflikt entsteht)
              # Stackaufbau: Symboltabelle, Auswahl
              set_break_sem_3();
              { var reg1 object sym = *sym_;
                var reg2 object pack = *pack_;
                # Symbol aus der Symboltabelle entfernen:
                symtab_delete(sym,STACK_1);
                # Falls es aus seiner Home-Package entfernt wurde,
                # setze die Home-Package auf NIL:
                if (eq(Symbol_package(sym),pack))
                  { Symbol_package(sym) = NIL; }
                # Symbol aus Shadowing-Liste streichen:
                shadowing_delete(Symbol_name(sym),pack);
              }
              { var reg1 object auswahl = popSTACK(); # Auswahl
                if (!nullp(auswahl))
                  # im Konfliktfalle: angewähltes Symbol importieren:
                  { pushSTACK(Car(Cdr(Cdr(auswahl))));
                    shadowing_import(&STACK_0,&(*pack_));
                    skipSTACK(1);
              }   }
              skipSTACK(1); # Symboltabelle vergessen
              clr_break_sem_3();
              return T; # Das war's
            }
            else
            # kein Konflikt
            { set_break_sem_2();
              symtab_delete(sym,symtab); # Symbol löschen
              if (eq(Symbol_package(sym),pack))
                { Symbol_package(sym) = NIL; } # evtl. Home-Package auf NIL setzen
              clr_break_sem_2();
              return T;
            }
        }
        else
        # nicht gefunden
        { return NIL; }
    }

# UP: Importiert ein Symbol in eine Package und sorgt für Konfliktauflösung
# für den Fall, daß ein Namenskonflikt entweder mit einem aus einer anderen
# Package vererbten Symbol oder mit einem bereits in dieser Package präsenten
# Symbol desselben Namens entsteht.
# import(&sym,&pack);
# > sym: Symbol (im STACK)
# > pack: Package (im STACK)
# < pack: Package, EQ zur alten
# kann GC auslösen
  global void import (object* sym_, object* pack_);
  global void import(sym_,pack_)
    var reg3 object* sym_;
    var reg4 object* pack_;
    { var reg2 object sym = *sym_;
      var reg1 object pack = *pack_;
      var reg3 object string = Symbol_name(sym);
      var object othersym;
      var reg5 object othersymtab;
      # Symbol gleichen Namens unter den internen und den externen Symbolen suchen:
      if (symtab_lookup(string,othersymtab=ThePackage(pack)->pack_internal_symbols,&othersym)
          || symtab_lookup(string,othersymtab=ThePackage(pack)->pack_external_symbols,&othersym)
         )
        # othersym = Symbol desselben Namens, gefunden in othersymtab
        { if (eq(othersym,sym))
            # dasselbe Symbol -> nichts tun
            { return; }
          # nicht dasselbe Symbol war präsent -> muß othersym rauswerfen und
          # dafür das gegebene Symbol sym reinsetzen.
          # Zuvor feststellen, ob zusätzlich noch vererbte Symbole da sind,
          # und dann Continuable Error melden.
          pushSTACK(string);
          pushSTACK(othersym);
          pushSTACK(othersymtab);
          # erst Inherited-Flag berechnen:
          pushSTACK(ThePackage(pack)->pack_use_list); # Use-List wird abgesucht
          while (mconsp(STACK_0))
            { var object otherusedsym;
              var reg1 object usedpack = Car(STACK_0);
              STACK_0 = Cdr(STACK_0);
              # Symbol gleichen Namens in usedpack suchen:
              if (symtab_lookup(string,ThePackage(usedpack)->pack_external_symbols,&otherusedsym))
                { STACK_0 = T; break; } # gefunden -> inherited-Flag := T
            } # sonst ist am Schluß inherited-Flag = STACK_0 = NIL
          # Stackaufbau: Symbol-Name, othersym, othersymtab, inherited-Flag.
          # Continuable Error melden:
          { pushSTACK(O(import_string1)); # "Sie dürfen über das weitere Vorgehen entscheiden."
            pushSTACK(nullp(STACK_1) # bei inherited=NIL die kurze Meldung
                      ? O(import_string2) # "Durch Importieren von ~S in ~S entsteht ein Namenskonflikt mit ~S."
                      : O(import_string3) # "Durch Importieren von ~S in ~S ... Namenskonflikt mit ~S und weiteren Symbolen."
                     );
            pushSTACK(sym); # Symbol
            pushSTACK(pack); # Package
            pushSTACK(STACK_6); # othersym
            funcall(S(cerror),5); # (CERROR String1 String2/3 sym pack othersym)
          }
          # Antwort vom Benutzer erfragen:
          { var reg2 object ml = # Möglichkeitenliste (("I" ... T) ("N" ... NIL))
                            (nullp(STACK_0) ? O(import_list1) : O(import_list2));
            var reg1 object antwort = query_user(ml);
            if (nullp(Car(Cdr(Cdr(antwort))))) # NIL-Möglichkeit angewählt?
              { skipSTACK(4); return; } # ja -> nicht importieren, fertig
          }
          # Importieren:
          set_break_sem_2();
          pack = *pack_;
          # othersym aus pack entfernen:
          { var reg1 object othersym = STACK_2;
            symtab_delete(othersym,STACK_1); # othersym aus othersymtab entfernen
            if (eq(Symbol_package(othersym),pack))
              { Symbol_package(othersym) = NIL; } # evtl. Home-Package := NIL
          }
          # sym in pack einfügen:
          make_present(*sym_,pack);
          # Symbole gleichen Namens aus der Shadowing-List von pack entfernen:
          shadowing_delete(STACK_3,*pack_);
          # Falls inherited-Flag, sym in pack zum Shadowing-Symbol machen:
          if (!nullp(STACK_0))
            { shadowing_insert(&(*sym_),&(*pack_)); }
          clr_break_sem_2();
          skipSTACK(4); return;
        }
        else
        # Kein Symbol desselben Namens war präsent.
        # Suche ein Symbol desselben Namens, das vererbt ist (es gibt
        # nach den Konsistenzregeln 6 und 5 höchstens ein solches):
        { var object otherusedsym;
          { pushSTACK(ThePackage(pack)->pack_use_list); # Use-List wird abgesucht
            while (mconsp(STACK_0))
              { var reg1 object usedpack = Car(STACK_0);
                STACK_0 = Cdr(STACK_0);
                # Symbol gleichen Namens in usedpack suchen:
                if (symtab_lookup(string,ThePackage(usedpack)->pack_external_symbols,&otherusedsym))
                  goto inherited_found;
              }
            skipSTACK(1);
            # Kein Symbol desselben Namens war accessible.
            # sym kann daher gefahrlos importiert werden.
            goto import_sym;
          }
          inherited_found: # gefunden.
            skipSTACK(1);
            # Wurde genau das gegebene Symbol gefunden?
            if (eq(otherusedsym,sym))
              goto import_sym; # ja -> importieren
            # nein -> Continuable Error melden und Benutzer fragen:
            { pushSTACK(O(import_string1)); # "Sie dürfen über das weitere Vorgehen entscheiden."
              pushSTACK(O(import_string2)); # "Durch Importieren von ~S in ~S entsteht ein Namenskonflikt mit ~S."
              pushSTACK(sym); # Symbol
              pushSTACK(pack); # Package
              pushSTACK(otherusedsym); # otherusedsym
              funcall(S(cerror),5); # (CERROR String1 String2 sym pack otherusedsym)
            }
            { var reg1 object antwort = query_user(O(import_list3));
              if (nullp(Car(Cdr(Cdr(antwort))))) # NIL-Möglichkeit angewählt?
                { return; } # ja -> nicht importieren, fertig
            }
            # Importieren:
            set_break_sem_2();
            # sym in pack einfügen:
            make_present(*sym_,*pack_);
            # sym in pack zum Shadowing-Symbol machen:
            shadowing_insert(&(*sym_),&(*pack_));
            clr_break_sem_2(); return;
          import_sym:
            # sym einfach in pack einfügen:
            set_break_sem_2();
            make_present(sym,pack);
            clr_break_sem_2(); return;
        }
    }

# UP: Setzt ein Symbol vom externen auf den internen Status in einer Package
# zurück.
# unexport(&sym,&pack);
# > sym: Symbol (im STACK)
# > pack: Package (im STACK)
# < pack: Package, EQ zur alten
# kann GC auslösen
  local void unexport (object* sym_, object* pack_);
  local void unexport(sym_,pack_)
    var reg3 object* sym_;
    var reg4 object* pack_;
    { var reg2 object sym = *sym_;
      var reg1 object pack = *pack_;
      var reg3 object symtab;
      if (symtab_find(sym,symtab=ThePackage(pack)->pack_external_symbols))
        # sym ist in pack extern
        { if (eq(pack,O(keyword_package))) # auf Keyword-Package testen
            { pushSTACK(pack);
              fehler(
                     DEUTSCH ? "UNEXPORT ist in ~ nicht zulässig." :
                     ENGLISH ? "UNEXPORT in ~ is illegal" :
                     FRANCAIS ? "UNEXPORT n'est pas permis dans ~." :
                     ""
                    );
            }
          set_break_sem_2();
          symtab_delete(sym,symtab); # sym aus den externen Symbolen entfernen
          symtab_insert(sym,ThePackage(pack)->pack_internal_symbols); # dafür in die internen Symbole einfügen
          clr_break_sem_2();
        }
        else
        # Suchen, ob das Symbol überhaupt accessible ist.
        { # Suche unter den internen Symbolen:
          if (symtab_find(sym,ThePackage(pack)->pack_internal_symbols))
            goto found;
          # Suche unter den externen Symbolen der Packages aus der Use-List:
          { var reg1 object list = ThePackage(pack)->pack_use_list;
            while (consp(list))
              { if (symtab_find(sym,ThePackage(Car(list))->pack_external_symbols))
                  goto found;
                list = Cdr(list);
          }   }
          # nicht gefunden unter den accessiblen Symbolen
          { pushSTACK(pack); pushSTACK(sym);
            fehler(
                   DEUTSCH ? "UNEXPORT ist nur auf accessiblen Symbolen möglich, nicht auf Symbol ~ in ~." :
                   ENGLISH ? "UNEXPORT works only on accessible symbols, not on ~ in ~" :
                   FRANCAIS ? "UNEXPORT n'est possible que pour des symboles accessibles mais pas pour le symbole ~ dans ~." :
                   ""
                  );
          }
          found: # gefunden unter den nicht-externen accessiblen Symbolen
            return; # nichts zu tun
    }   }

# UP: Setzt ein präsentes Symbol auf externen Status.
# make_external(sym,pack);
# > sym: Symbol
# > pack: Package, in der das Symbol präsent ist
# kann GC auslösen
  local void make_external (object sym, object pack);
  local void make_external(sym,pack)
    var reg2 object sym;
    var reg1 object pack;
    { if (symtab_find(sym,ThePackage(pack)->pack_external_symbols))
        { return; } # Symbol bereits extern -> nichts zu tun
      set_break_sem_2();
      symtab_delete(sym,ThePackage(pack)->pack_internal_symbols); # sym aus den internen Symbolen entfernen
      symtab_insert(sym,ThePackage(pack)->pack_external_symbols); # dafür in die externen Symbole einfügen
      clr_break_sem_2();
    }

# UP: Exportiert ein Symbol aus einer Package
# export(&sym,&pack);
# > sym: Symbol (im STACK)
# > pack: Package (im STACK)
# < sym: Symbol, EQ zum alten
# < pack: Package, EQ zur alten
# kann GC auslösen
  global void export (object* sym_, object* pack_);
  global void export(sym_,pack_)
    var reg3 object* sym_;
    var reg4 object* pack_;
    { var reg2 object sym = *sym_;
      var reg1 object pack = *pack_;
      # sym unter den externen Symbolen von pack suchen:
      if (symtab_find(sym,ThePackage(pack)->pack_external_symbols))
        { return; } # gefunden -> fertig
      { var reg6 boolean import_it = FALSE;
        # import_it = Flag, ob Symbol erst noch importiert werden muß.
        # sym unter den internen Symbolen von pack suchen:
        if (!(symtab_find(sym,ThePackage(pack)->pack_internal_symbols)))
          # Symbol sym ist nicht präsent in Package pack
          { import_it = TRUE;
            # Suche, ob es wenigstens accessible ist:
            { var reg1 object list = ThePackage(pack)->pack_use_list;
              while (consp(list))
                { if (symtab_find(sym,ThePackage(Car(list))->pack_external_symbols))
                    goto found;
                  list = Cdr(list);
            }   }
            # Symbol sym ist nicht einmal accessible in der Package pack
            # Continuable Error melden:
            { pushSTACK(O(export_string1)); # "Sie dürfen über das weitere Vorgehen entscheiden."
              pushSTACK(O(export_string2)); # "Symbol ~S müßte erst in ~S importiert werden, bevor es exportiert werden kann."
              pushSTACK(sym); # Symbol
              pushSTACK(pack); # Package
              funcall(S(cerror),4); # (CERROR "Sie dürfen aussuchen, ..." "..." Symbol Package)
            }
            # beim Benutzer nachfragen:
            { var reg1 object antwort = query_user(O(export_list1));
              if (nullp(Car(Cdr(Cdr(antwort))))) # NIL-Möglichkeit angewählt?
                { return; } # ja -> nicht exportieren, fertig
            }
            found: ;
          }
        # Nun auf Namensfonflikte testen:
        pushSTACK(NIL); # Conflict-Resolver:=NIL
        # Stackaufbau: Conflict-Resolver (eine Liste von Paaren (sym . pack),
        #              auf die shadowing_import angewandt werden muß).
        pushSTACK(ThePackage(*pack_)->pack_used_by_list); # Used-By-List wird abgesucht
        while (mconsp(STACK_0))
          { var reg1 object usingpack = Car(STACK_0); # USEnde Package
            STACK_0 = Cdr(STACK_0);
           {var object othersym;
            if (!(find_symbol(Symbol_name(*sym_),usingpack,&othersym)==0))
              # othersym ist ein Symbol desselben Namens in usingpack
              if (!eq(othersym,*sym_))
                # es ist nicht sym selbst -> es liegt ein Konflikt vor
                { pushSTACK(othersym); pushSTACK(usingpack);
                  # Stackaufbau: Conflict-Resolver, Used-by-list-Rest,
                  #              anderes Symbol, USEnde Package.
                  # Continuable Error melden:
                  { pushSTACK(O(export_string3)); # "Sie dürfen aussuchen, welches Symbol Vorrang hat."
                    pushSTACK(O(export_string4)); # "Durch Exportieren von ~S aus ~S ... Namenskonflikt mit ~S in ~S."
                    pushSTACK(*sym_); # Symbol
                    pushSTACK(*pack_); # Package
                    pushSTACK(othersym); # anderes Symbol
                    pushSTACK(usingpack); # USEnde Package
                    funcall(S(cerror),6); # (CERROR "..." "..." sym pack othersym usingpack)
                  }
                  # Einleitung ausgeben:
                  { var object stream = var_stream(S(query_io)); # Stream *QUERY-IO*
                    terpri(&stream); # Neue Zeile
                    write_sstring(&stream,O(export_string5)); # "Welches Symbol soll in "
                    prin1(&stream,STACK_0); # usingpack ausgeben
                    write_sstring(&stream,O(export_string6)); # " Vorrang haben?"
                  }
                  # Möglichkeitenliste konstruieren:
                  { var reg1 object temp;
                     pushSTACK(O(export_string7)); # "1"
                      pushSTACK(O(export_string9)); # "Das zu exportierende Symbol "
                       pushSTACK(*sym_); # Symbol
                       funcall(L(prin1_to_string),1); # (prin1-to-string Symbol)
                      pushSTACK(value1);
                      temp = string_concat(2); # (string-concat "Das zu exportierende Symbol " (prin1-to-string Symbol))
                     pushSTACK(temp);
                     pushSTACK(T);
                     temp = listof(3); # (list "1" (string-concat ...) 'T)
                    pushSTACK(temp);
                     pushSTACK(O(export_string8)); # "2"
                      pushSTACK(O(export_string10)); # "Das alte Symbol "
                       pushSTACK(STACK_4); # anderes Symbol
                       funcall(L(prin1_to_string),1); # (prin1-to-string anderesSymbol)
                      pushSTACK(value1);
                      temp = string_concat(2); # (string-concat "Das alte Symbol " (prin1-to-string anderesSymbol))
                     pushSTACK(temp);
                     pushSTACK(NIL);
                     temp = listof(3); # (list "2" (string-concat ...) 'NIL)
                    pushSTACK(temp);
                    temp = listof(2); # (list (list "1" ... 'T) (list "2" ... 'NIL))
                  # Beim Benutzer nachfragen:
                    { var reg2 object antwort = query_user(temp);
                      var reg3 object solvingsym =
                          (!(nullp(Car(Cdr(Cdr(antwort))))) # NIL-Möglichkeit angewählt?
                           ? *sym_ # nein -> sym
                           : STACK_1 # ja -> othersym
                          );
                      pushSTACK(solvingsym); # ausgewähltes Symbol
                    }
                  # Conflict-Resolver um (solvingsym . usingpack) erweitern:
                    temp = allocate_cons();
                    Car(temp) = popSTACK(); # solvingsym
                    Cdr(temp) = popSTACK(); # usingpack
                    # temp = (cons solvingsym usingpack)
                    # vor Conflict-Resolver davorconsen:
                    STACK_0 = temp;
                    temp = allocate_cons();
                    Car(temp) = popSTACK(); # (solvingsym . usingpack)
                    Cdr(temp) = STACK_1;
                    STACK_1 = temp;
                  }
                  # Stackaufbau: Conflict-Resolver, Used-by-list-Rest.
          }}    }
        skipSTACK(1);
        # Stackaufbau: Conflict-Resolver.
        # Nun evtl. Symbol sym importieren:
        if (import_it)
          { # sym in pack importieren:
            import(&(*sym_),&(*pack_));
            # Dieses Importieren kann durch einen CERROR abgebrochen werden.
            # Ein Abbruch an dieser Stelle ist ungefährlich, denn bis jetzt
            # ist das Symbol nur intern in der Package (außer falls es sich
            # um das KEYWORD-Package handelt, das nicht geUSEd werden kann).
          }
        set_break_sem_3(); # gegen Unterbrechungen schützen
        # Nun die Konflikte auflösen:
        while (mconsp(STACK_0))
          { var reg1 object cons_sym_pack = Car(STACK_0);
            STACK_0 = Cdr(STACK_0);
            pushSTACK(Car(cons_sym_pack)); # solvingsym
            pushSTACK(Cdr(cons_sym_pack)); # usingpack
            shadowing_import(&STACK_1,&STACK_0); # importieren und shadowen
            skipSTACK(2);
          }
        skipSTACK(1);
        make_external(*sym_,*pack_); # sym in pack extern machen
        clr_break_sem_3(); # Unterbrechungen wieder freigeben
    } }

# UP: Wendet eine Funktion auf alle Symbole einer Symboltabelle an.
# (Diese Funktion darf im Extremfall das Symbol mittels symtab_delete
# aus der Tabelle herausnehmen.)
# map_symtab(fun,symtab);
# > fun: Funktion mit einem Argument
# > symtab: Symboltabelle
# kann GC auslösen
  local void map_symtab (object fun, object symtab);
  local void map_symtab(fun,symtab)
    var reg5 object fun;
    var reg4 object symtab;
    { pushSTACK(fun); # Funktion
      pushSTACK(Symtab_table(symtab)); # Tabellenvektor
     {var uintL size = posfixnum_to_L(Symtab_size(symtab)); # Anzahl der Einträge
      var reg3 object* offset = 0; # offset = sizeof(object)*index
      var reg2 uintC count;
      dotimespC(count,size,
        { var reg1 object entry = # Eintrag mit Nummer index in table
              *(object*)(pointerplus(&TheSvector(STACK_0)->data[0],(aint)offset));
          if (atomp(entry))
            { if (!(nullp(entry)))
                # entry ist ein Symbol /= NIL
                { pushSTACK(entry); funcall(STACK_2,1); } # Funktion anwenden
            }
            else
            # nichtleere Symbolliste abarbeiten
            { pushSTACK(entry);
              do { var reg1 object listr = STACK_0;
                   STACK_0 = Cdr(listr);
                   pushSTACK(Car(listr)); funcall(STACK_3,1); # Funktion auf Symbol anwenden
                 }
                 until (matomp(STACK_0));
              skipSTACK(1);
            }
          offset++;
        });
      skipSTACK(2);
    }}

# UP: Bewirkt, daß alle externen Symbole einer gegebenen Liste von Packages
# implizit accessible in einer gegebenen Package werden.
# use_package(packlist,pack);
# > packlist: Liste von Packages, die zu USEn sind
# > pack: Package
# Die Liste packlist wird dabei zerstört!
# kann GC auslösen
  local void use_package (object packlist, object pack);
  local object* use_package_local; # Pointer auf drei lokale Variablen
  local void use_package(packlist,pack)
    var object packlist;
    var reg5 object pack;
    { # packlist := (delete-duplicates packlist :test #'eq) :
      { var reg4 object packlist1 = packlist;
        while (consp(packlist1))
          { var reg3 object to_delete = Car(packlist1);
            # Entferne to_delete destruktiv aus (cdr packlist1) :
            var reg2 object packlist2 = packlist1; # läuft ab packlist1
            var reg1 object packlist3; # stets = (cdr packlist2)
            while (consp(packlist3=Cdr(packlist2)))
              { if (eq(Car(packlist3),to_delete))
                  # streiche (car packlist3) destruktiv aus der Liste:
                  { Cdr(packlist2) = Cdr(packlist3); }
                  else
                  # weiterrücken:
                  { packlist2 = packlist3; }
              }
            packlist1 = Cdr(packlist1);
      }   }
      # Entferne aus packlist alle die Packages, die gleich pack sind
      # oder bereits in der Use-List von pack vorkommen:
      { var reg4 object* packlistr_ = &packlist;
        var reg3 object packlistr = *packlistr_;
        # packlistr läuft durch packlist, packlistr = *packlistr_
        while (consp(packlistr))
          { # Teste, ob (car packlistr) gestrichen werden muß:
            var reg2 object pack_to_test = Car(packlistr);
            if (eq(pack_to_test,pack))
              goto delete_pack_to_test;
            { var reg1 object usedpacklistr = ThePackage(pack)->pack_use_list;
              while (consp(usedpacklistr))
                { if (eq(pack_to_test,Car(usedpacklistr)))
                    goto delete_pack_to_test;
                  usedpacklistr = Cdr(usedpacklistr);
            }   }
            if (TRUE)
              # nichts streichen, weiterrücken:
              { packlistr_ = &Cdr(packlistr); packlistr = *packlistr_; }
              else
              # streiche (car packlistr) :
              { delete_pack_to_test:
                packlistr = *packlistr_ = Cdr(packlistr);
              }
      }   }
      # Konfliktliste aufbauen.
      # Dabei ist ein Konflikt eine mindestens zweielementige Liste
      # von Symbolen gleichen Printnamens, zusammen mit der Package,
      # aus der dieses Symbol genommen wird:
      # ((pack1 . sym1) ...) bedeutet, daß bei Ausführung des USE-PACKAGE
      # die Symbole sym1,... (aus pack1 etc.) sich um die Sichtbarkeit in
      # Package pack streiten würden.
      # Die Konfliktliste ist die Liste aller auftretenden Konflikte.
      { pushSTACK(pack); # Package pack retten
        pushSTACK(packlist); # Liste zu USEnder Packages retten
        pushSTACK(NIL); # (bisher leere) Konfliktliste
        # Stackaufbau: pack, packlist, conflicts.
        use_package_local = &STACK_0; # zeigt auf die drei lokalen Variablen
        # Packageliste durchgehen:
        { pushSTACK(packlist);
          while (mconsp(STACK_0))
            { var reg1 object pack_to_use = Car(STACK_0);
              STACK_0 = Cdr(STACK_0);
              # #'use_package_aux auf alle externen Symbole von pack_to_use anwenden:
              map_symtab(L(use_package_aux),ThePackage(pack_to_use)->pack_external_symbols);
            }
          skipSTACK(1);
        }
        # Konfliktliste umbauen: Jeder Konflikt ((pack1 . sym1) ...) wird
        # umgeformt zu (("1" packname1 . sym1) ...).
        { pushSTACK(STACK_0); # Konfliktliste durchgehen
          while (mconsp(STACK_0))
            { var reg4 object conflict = Car(STACK_0);
              STACK_0 = Cdr(STACK_0);
              pushSTACK(conflict); # Konflikt durchgehen
              { var reg5 object counter = Fixnum_0; # Zähler := 0
                while (mconsp(STACK_0))
                  {  counter = fixnum_inc(counter,1); # Zähler um 1 erhöhen
                     pushSTACK(counter); funcall(L(prin1_to_string),1); # (prin1-to-string Zähler)
                     pushSTACK(value1); # Zählerstring retten
                   { var reg2 object new_cons = allocate_cons(); # neues Cons
                     Car(new_cons) = popSTACK(); # Zählerstring als CAR
                    {var reg1 object old_cons = Car(STACK_0); # Cons der Form (pack . sym)
                     Car(old_cons) = ThePackage(Car(old_cons))->pack_name; # pack durch seinen Namen ersetzen
                     Cdr(new_cons) = old_cons; Car(STACK_0) = new_cons; # Zählerstring-Cons einfügen
                   }}
                     STACK_0 = Cdr(STACK_0);
              }   }
              skipSTACK(1);
            }
          skipSTACK(1);
        }
        # Konflikt-Liste fertig.
        pushSTACK(NIL); # Conflict-Resolver := NIL
        # Stackaufbau: pack, packlist, conflicts, conflict-resolver.
        # Konflikte durch Benutzerfragen behandeln:
        if (!(nullp(STACK_1))) # nur bei conflicts/=NIL nötig
          { # Continuable Error melden:
            { pushSTACK(O(usepack_string1)); # "Sie dürfen bei jedem Konflikt ..."
              pushSTACK(O(usepack_string2)); # "~S Namenskonflikte bei USE-PACKAGE von ~S in die Package ~S."
              pushSTACK(fixnum(llength(STACK_3))); # (length conflicts)
              pushSTACK(STACK_5); # packlist
              pushSTACK(STACK_7); # pack
              funcall(S(cerror),5); # (CERROR "..." "..." (length conflicts) usedpacks pack)
            }
            { pushSTACK(STACK_1); # conflicts durchgehen
              while (mconsp(STACK_0))
                { pushSTACK(Car(STACK_0)); # conflict
                 {var object stream = var_stream(S(query_io)); # Stream *QUERY-IO*
                  terpri(&stream); # Neue Zeile
                  write_sstring(&stream,O(usepack_string3)); # "Welches Symbol mit dem Namen "
                  # (cdr (cdr (car conflict))) = (cdr (cdr '("1" packname1 . sym1))) = sym1
                  prin1(&stream,Symbol_name(Cdr(Cdr(Car(STACK_0))))); # Name davon ausgeben
                  write_sstring(&stream,O(usepack_string4)); # " soll in "
                  prin1(&stream,STACK_5); # pack ausgeben
                  write_sstring(&stream,O(usepack_string5)); # " Vorrang haben?"
                 }
                  # Beim Benutzer nachfragen,
                  # mit conflict als Möglichkeitenliste:
                 {var reg2 object antwort = query_user(popSTACK());
                  # Davon das Symbol nehmen und in den conflict-resolver stecken:
                  pushSTACK(Cdr(Cdr(antwort))); # sym
                 }
                 {var reg1 object new_cons = allocate_cons();
                  Car(new_cons) = popSTACK(); # sym
                  Cdr(new_cons) = STACK_1; # conflict-resolver
                  STACK_1 = new_cons; # conflict-resolver := (cons sym conflict-resolver)
                 }
                  STACK_0 = Cdr(STACK_0); # restliche Konfliktliste verkürzen
                }
              skipSTACK(1);
          } }
        # Stackaufbau: pack, packlist, conflicts, conflict-resolver.
        # Konflikte auflösen:
        { set_break_sem_3();
          # conflict-resolver durchgehen:
          while (mconsp(STACK_0))
            { pushSTACK(Car(STACK_0)); # Symbol aus conflict-resolver
              shadowing_import(&STACK_0,&STACK_4); # in pack zum Shadowing-Symbol machen
              skipSTACK(1);
              STACK_0 = Cdr(STACK_0);
            }
          skipSTACK(2); # conflicts und conflict-resolver vergessen
          # Stackaufbau: pack, packlist.
          # packlist durchgehen:
          while (mconsp(STACK_0))
            { pushSTACK(Car(STACK_0)); # pack_to_use
              # pack_to_use auf die Use-List von pack setzen:
              # (push pack_to_use (package-use-list pack))
              { var reg1 object new_cons = allocate_cons();
                var reg2 object pack = STACK_2;
                Car(new_cons) = STACK_0; # pack_to_use
                Cdr(new_cons) = ThePackage(pack)->pack_use_list;
                ThePackage(pack)->pack_use_list = new_cons;
              }
              # pack auf die Used-By-List von pack_to_use setzen:
              # (push pack (package-used-by-list pack_to_use))
              { var reg1 object new_cons = allocate_cons();
                var reg2 object pack_to_use = popSTACK();
                Car(new_cons) = STACK_1; # pack
                Cdr(new_cons) = ThePackage(pack_to_use)->pack_used_by_list;
                ThePackage(pack_to_use)->pack_used_by_list = new_cons;
              }
              STACK_0 = Cdr(STACK_0);
            }
          skipSTACK(2); # pack und packlist vergessen
          clr_break_sem_3();
    } } }

# Hilfsfunktion für use_package:
# Teste das Argument (ein externes Symbol aus einer der Packages aus
# packlist), ob es einen Konflikt erzeugt. Wenn ja, erweitere conflicts.
LISPFUNN(use_package_aux,1)
  { var reg6 object* localptr = use_package_local;
    # Pointer auf lokale Variablen von use_package:
    #   *(localptr STACKop 2) = pack,
    #   *(localptr STACKop 1) = packlist,
    #   *(localptr STACKop 0) = conflicts.
    var reg7 object string = Symbol_name(popSTACK()); # Printname des übergebenen Symbols
    # Gibt es einen Konflikt zwischen den Symbolen mit Printname = string ?
    # Bisherige Konfliktliste (((pack1 . sym1) ...) ...) durchgehen:
    { var reg1 object conflictsr = *(localptr STACKop 0);
      while (consp(conflictsr))
        { # Konflikt schon behandelt?
          # (car conflictsr) = nächster Konflikt,
          # (car (car conflictsr)) = dessen erstes Cons,
          # (cdr (car (car conflictsr))) = darin das Symbol,
          # ist dessen Printname = string ?
          if (string_gleich(Symbol_name(Cdr(Car(Car(conflictsr)))),string))
            goto ok;
          conflictsr = Cdr(conflictsr);
    }   }
    pushSTACK(string); # string retten
    # neuen Konflikt aufbauen:
    pushSTACK(NIL); # neuer Konflikt (noch leer)
    # Testen, ob ein gleichnamiges Symbol bereits in pack accessible ist:
    { var object othersym;
      if (!(find_symbol(STACK_1,*(localptr STACKop 2),&othersym)==0))
        # ja -> Konflikt um (pack . othersym) erweitern:
        { pushSTACK(othersym);
         {var reg1 object temp = allocate_cons();
          Cdr(temp) = popSTACK(); # othersym
          Car(temp) = *(localptr STACKop 2); # pack
          pushSTACK(temp); # (pack . othersym)
         }
         {var reg1 object new_cons = allocate_cons();
          Car(new_cons) = popSTACK(); Cdr(new_cons) = STACK_0;
          STACK_0 = new_cons;
    }   }}
    # Testen, in welchen Packages aus packlist ein gleichnamiges Symbol
    # extern ist:
    { var reg3 object packlistr = *(localptr STACKop 1); # packlist durchgehen
      while (consp(packlistr))
        { var reg2 object pack_to_use = Car(packlistr);
          packlistr = Cdr(packlistr);
          { var object othersym;
            if (symtab_lookup(STACK_1,ThePackage(pack_to_use)->pack_external_symbols,&othersym))
              # othersym hat den Printnamen = string und ist extern in pack_to_use.
              # (pack_to_use . othersym) auf conflict pushen:
              { pushSTACK(packlistr); # packlistr retten
                pushSTACK(pack_to_use);
                pushSTACK(othersym);
               {var reg1 object new_cons = allocate_cons();
                Cdr(new_cons) = popSTACK(); Car(new_cons) = popSTACK();
                pushSTACK(new_cons); # (cons pack_to_use othersym)
               }
               {var reg1 object new_cons = allocate_cons();
                Car(new_cons) = popSTACK();
                packlistr = popSTACK();
                Cdr(new_cons) = STACK_0;
                STACK_0 = new_cons; # conflict := (cons (cons pack_to_use othersym) conflict)
    }   } }   }}
    { var reg5 object conflict = popSTACK(); # der fertige Konflikt
      # conflict := (delete-duplicates conflict :key #'cdr :test #'eq):
      { var reg4 object conflict1 = conflict;
        while (consp(conflict1))
          { var reg3 object to_delete = Cdr(Car(conflict1));
            # Entferne alle Elemente mit CDR=to_delete
            # destruktiv aus (cdr conflict1) :
            var reg2 object conflict2 = conflict1; # läuft ab conflict1
            var reg1 object conflict3; # stets = (cdr conflict2)
            while (consp(conflict3=Cdr(conflict2)))
              { if (eq(Cdr(Car(conflict3)),to_delete))
                  # streiche (car conflict3) destruktiv aus der Liste:
                  { Cdr(conflict2) = Cdr(conflict3); }
                  else
                  # weiterrücken:
                  { conflict2 = conflict3; }
              }
            conflict1 = Cdr(conflict1);
      }   }
      # Falls conflict eine Länge >=2 hat, wird es zu conflicts geconst:
      if (consp(conflict) && mconsp(Cdr(conflict)))
        { pushSTACK(conflict);
         {var reg1 object new_cons = allocate_cons();
          Car(new_cons) = popSTACK(); # conflict
          Cdr(new_cons) = *(localptr STACKop 0); # conflicts
          *(localptr STACKop 0) = new_cons; # conflicts := (cons conflict conflicts)
        }}
    }
    skipSTACK(1); # string vergessen
    ok: value1 = NIL; mv_count=0; # keine Werte
  }

# UP: Bewirkt, daß eine gegebene Package nicht mehr von einer (anderen)
# gegebenen Package geUSEt wird.
# unuse_1package(pack,qpack);
# > pack: Package
# > qpack: Package
# Entfernt qpack von der Use-List von pack
# und pack von der Used-By-List von qpack.
  local void unuse_1package (object pack, object qpack);
  local void unuse_1package(pack,qpack)
    var reg1 object pack;
    var reg2 object qpack;
    { set_break_sem_2();
      # qpack aus der Use-List von pack entfernen:
      ThePackage(pack)->pack_use_list =
        deleteq(ThePackage(pack)->pack_use_list,qpack);
      # pack aus der Used-By-List von qpack entfernen:
      ThePackage(qpack)->pack_used_by_list =
        deleteq(ThePackage(qpack)->pack_used_by_list,pack);
      clr_break_sem_2();
    }

# UP: Bewirkt, daß eine Liste gegebener Packages nicht mehr von einer
# gegebenen Package geUSEt wird.
# unuse_package(packlist,pack);
# > packlist: Liste von Packages
# > pack: Package
# Entfernt alle Packages aus packlist von der Use-List von pack
# und pack von den Used-By-Listen aller Packages aus packlist.
  local void unuse_package (object packlist, object pack);
  local void unuse_package(packlist,pack)
    var reg1 object packlist;
    var reg2 object pack;
    { set_break_sem_3();
      # packlist durchgehen:
      while (consp(packlist))
        { unuse_1package(pack,Car(packlist));
          packlist = Cdr(packlist);
        }
      clr_break_sem_3();
    }

# UP: liefert die aktuelle Package
# get_current_package()
# < ergebnis: aktuelle Package
  global object get_current_package (void);
  global object get_current_package()
    { var reg1 object pack = Symbol_value(S(packagestern)); # Wert von *PACKAGE*
      if (packagep(pack))
        { return pack; }
        else
        { var reg1 object newpack =
              Symbol_value(S(packagestern)) = O(default_package); # *PACKAGE* zurücksetzen
          pushSTACK(newpack); pushSTACK(pack);
          fehler(
                 DEUTSCH ? "Der Wert von *PACKAGE* war keine Package. Alter Wert: ~. Neuer Wert: ~." :
                 ENGLISH ? "The value of *PACKAGE* was not a package. Old value ~. New value ~." :
                 FRANCAIS ? "La valeur de *PACKAGE* n'était pas un paquetage. Ancienne valeur : ~. Nouvelle valeur : ~." :
                 ""
                );
    }   }

# UP: Überprüft ein Package-Argument.
# Testet, ob es eine Package oder ein Packagename ist, und liefert es als
# Package zurück. Sonst Fehlermeldung.
# test_package_arg(obj)
# > obj: Argument
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: in eine Package umgewandeltes Argument
  local object test_package_arg (object obj);
  local object test_package_arg(obj)
    var reg1 object obj;
    { if (packagep(obj)) { return obj; } # Package: OK
      if (stringp(obj))
        string: # String -> Package mit Namen obj suchen:
        { var reg2 object pack = find_package(obj);
          if (!(nullp(pack))) { return pack; }
          pushSTACK(obj);
          fehler(
                 DEUTSCH ? "Eine Package mit Namen ~ gibt es nicht." :
                 ENGLISH ? "There is no package with name ~" :
                 FRANCAIS ? "Il n'y a pas de paquetage de nom ~." :
                 ""
                );
        }
      if (symbolp(obj)) # Symbol ->
        { obj = Symbol_name(obj); goto string; } # Printnamen verwenden
      pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(
             DEUTSCH ? "Argument zu ~ muß eine Package oder ein Packagename sein, nicht ~" :
             ENGLISH ? "~: argument should be a package or a package name, not ~" :
             FRANCAIS ? "L'argument de ~ doit être un paquetage ou un nom de paquetage et non ~." :
             ""
            );
    }

LISPFUNN(make_symbol,1) # (MAKE-SYMBOL printname), CLTL S. 168
  { var reg1 object arg = popSTACK();
    if (!(stringp(arg)))
      { pushSTACK(arg); pushSTACK(TheSubr(subr_self)->name);
        fehler(
               DEUTSCH ? "~: Argument muß ein String sein, nicht ~." :
               ENGLISH ? "~: argument should be a string, not ~" :
               FRANCAIS ? "~ : L'argument doit être une chaîne et non ~." :
               ""
              );
      }
    # Simple-String draus machen und Symbol bauen:
    value1 = make_symbol(coerce_ss(arg)); mv_count=1;
  }

# UP: Überprüft ein String/Symbol-Argument
# > obj: Argument
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: Argument als String
  local object test_stringsym_arg (object obj);
  local object test_stringsym_arg(obj)
    var reg1 object obj;
    { if (stringp(obj)) return obj; # String: unverändert zurück
      if (symbolp(obj)) return TheSymbol(obj)->pname; # Symbol: Printnamen verwenden
      pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      fehler(
             DEUTSCH ? "~: Argument muß ein String oder Symbol sein, nicht ~." :
             ENGLISH ? "~: argument ~ should be a string or a symbol" :
             FRANCAIS ? "~ : L'argument doit être un symbole ou une chaîne et non ~." :
             ""
            );
    }

LISPFUNN(find_package,1) # (FIND-PACKAGE name), CLTL S. 183
  { var reg1 object name = test_stringsym_arg(popSTACK()); # Argument als String
    value1 = find_package(name); # Package suchen
    mv_count=1;
  }

LISPFUNN(package_name,1) # (PACKAGE-NAME package), CLTL S. 184
  { var reg1 object pack = test_package_arg(popSTACK()); # Argument als Package
    value1 = ThePackage(pack)->pack_name; # der Name
    mv_count=1;
  }

LISPFUNN(package_nicknames,1) # (PACKAGE-NICKNAMES package), CLTL S. 184
  { var reg1 object pack = test_package_arg(popSTACK()); # Argument als Package
    value1 = copy_list(ThePackage(pack)->pack_nicknames); # Nicknameliste sicherheitshalber kopieren
    mv_count=1;
  }

# UP: Überprüft name und nicknames - Argumente von RENAME-PACKAGE und MAKE-PACKAGE.
# Testet, ob STACK_2 ein Name ist, und macht daraus einen Simple-String.
# Testet, ob STACK_1 ein Name oder eine Liste von Namen ist, und macht
# daraus eine neue Liste von Simple-Strings.
# > subr-self: Aufrufer (ein SUBR)
# kann GC auslösen
  local void test_names_args (void);
  local void test_names_args()
    { # name auf String prüfen und zu einem Simple-String machen:
      STACK_2 = coerce_ss(test_stringsym_arg(STACK_2));
      # Nickname-Argument in eine Liste umwandeln:
      { var reg2 object nicknames = STACK_1;
        if (eq(nicknames,unbound))
          { STACK_1 = NIL; } # keine Nicknames angegeben -> Default NIL
          else
          if (!(listp(nicknames)))
            { # nicknames keine Liste -> eine einelementige Liste daraus machen:
              nicknames = allocate_cons();
              Car(nicknames) = STACK_1;
              STACK_1 = nicknames;
      }     }
      # Nickname(s) auf String prüfen, zu Simple-Strings machen
      # und daraus eine neue Nicknameliste machen:
      { pushSTACK(NIL); # neue Nicknameliste := NIL
        while (mconsp(STACK_2))
          {{var reg1 object nickname = Car(STACK_2); # nächster Nickname
            STACK_2 = Cdr(STACK_2);
            nickname = coerce_ss(test_stringsym_arg(nickname)); # als Simple-String
            # vor die neue Nicknameliste consen:
            pushSTACK(nickname);
           }
           {var reg1 object new_cons = allocate_cons();
            Car(new_cons) = popSTACK();
            Cdr(new_cons) = STACK_0;
            STACK_0 = new_cons;
          }}
       {var reg1 object nicknames = popSTACK();
        STACK_1 = nicknames; # neue Nicknameliste ersetzt die alte
      }}
    }

LISPFUN(rename_package,2,1,norest,nokey,0,NIL)
# (RENAME-PACKAGE pack name [nicknames]), CLTL S. 184
  { # Testen, ob pack eine Package ist:
    STACK_2 = test_package_arg(STACK_2);
    # name und nicknames überprüfen:
    pushSTACK(NIL); # Dummy auf den Stack
    test_names_args();
    skipSTACK(1);
   {var reg1 object pack = STACK_2;
    # Teste, ob ein Packagenamenkonflikt entsteht:
    { var reg3 object name = STACK_1;
      var reg2 object nicknamelistr = STACK_0;
      # name durchläuft den Namen und alle Nicknames
      loop
        { var reg1 object found = find_package(name); # Suche Package mit diesem Namen
          if (!(nullp(found) || eq(found,pack)))
            { # gefunden, aber eine andere als die gegebene Package:
              pushSTACK(name); pushSTACK(TheSubr(subr_self)->name);
              fehler(
                     DEUTSCH ? "~: Eine Package mit dem Namen ~ gibt es schon." :
                     ENGLISH ? "~: there is already a package named ~" :
                     FRANCAIS ? "~ : Il y a déjà un paquetage de nom ~." :
                     ""
                    );
            }
          # Keine oder nur die gegebene Package hat den Namen name ->
          # kein Konflikt mit diesem (Nick)name, weiter:
          if (atomp(nicknamelistr)) break;
          name = Car(nicknamelistr); # nächster Nickname
          nicknamelistr = Cdr(nicknamelistr); # restliche Nicknameliste verkürzen
    }   }
    # Es gibt keine Konflikte.
    set_break_sem_2();
    ThePackage(pack)->pack_name = STACK_1;
    ThePackage(pack)->pack_nicknames = STACK_0;
    clr_break_sem_2();
    skipSTACK(3);
    value1 = pack; mv_count=1; # pack als Wert
  }}

LISPFUNN(package_use_list,1) # (PACKAGE-USE-LIST package), CLTL S. 184
  { var reg1 object pack = test_package_arg(popSTACK()); # Argument als Package
    value1 = copy_list(ThePackage(pack)->pack_use_list); # Use-List sicherheitshalber kopieren
    mv_count=1;
  }

LISPFUNN(package_used_by_list,1) # (PACKAGE-USED-BY-LIST package), CLTL S. 184
  { var reg1 object pack = test_package_arg(popSTACK()); # Argument als Package
    value1 = copy_list(ThePackage(pack)->pack_used_by_list); # Used-By-List sicherheitshalber kopieren
    mv_count=1;
  }

LISPFUNN(package_shadowing_symbols,1) # (PACKAGE-SHADOWING-SYMBOLS package), CLTL S. 184
  { var reg1 object pack = test_package_arg(popSTACK()); # Argument als Package
    value1 = copy_list(ThePackage(pack)->pack_shadowing_symbols); # Shadowing-Liste sicherheitshalber kopieren
    mv_count=1;
  }

LISPFUNN(list_all_packages,0)
# (LIST-ALL-PACKAGES) liefert eine Liste aller Packages, CLTL S. 184
  { value1 = reverse(O(all_packages)); # (Kopie der Liste, sicherheitshalber)
    mv_count=1;
  }

# UP: Überprüft das letzte Argument &optional (pack *package*) einer
# LISP-Funktion.
# test_optional_package_arg()
# > STACK_0: letztes Argument
# > subr_self: Aufrufer (ein SUBR)
# < STACK_0: in eine Package umgewandeltes Argument
  local void test_optional_package_arg (void);
  local void test_optional_package_arg()
    { var reg1 object pack = STACK_0;
      if (eq(pack,unbound))
        { STACK_0 = get_current_package(); } # Default ist Wert von *PACKAGE*
        else
        { STACK_0 = test_package_arg(pack); }
    }

# UP: Überprüfung der Argumente von INTERN und FIND-SYMBOL.
# test_intern_args()
# > subr_self: Aufrufer (ein SUBR)
  local void test_intern_args (void);
  local void test_intern_args()
    { # String überprüfen:
      if (!(stringp(STACK_1)))
        { pushSTACK(STACK_1); pushSTACK(TheSubr(subr_self)->name);
          fehler(
                 DEUTSCH ? "~: Argument ~ ist kein String." :
                 ENGLISH ? "~: argument ~ is not a string" :
                 FRANCAIS ? "~ : L'argument ~ n'est pas une chaîne." :
                 ""
                );
        }
      # Package überprüfen:
      test_optional_package_arg();
    }

# UP: Wandelt ein INTERN/FIND-SYMBOL - Ergebnis in ein Keyword um.
# intern_result(code)
# > code : Flag wie bei intern und find_symbol
# < ergebnis : entsprechendes Keyword
  local object intern_result (uintBWL code);
  local object intern_result(code)
    var reg1 uintBWL code;
    { switch (code)
        { case 0: return NIL;           # 0 -> NIL
          case 1: return S(Kexternal);  # 1 -> :EXTERNAL
          case 2: return S(Kinherited); # 2 -> :INHERITED
          case 3: return S(Kinternal);  # 3 -> :INTERNAL
          default: NOTREACHED
    }   }

LISPFUN(intern,1,1,norest,nokey,0,NIL)
# (INTERN string [package]), CLTL S. 184
  { test_intern_args(); # Argumente überprüfen
   {var reg2 object pack = popSTACK();
    var reg1 object string = popSTACK();
    #if !defined(VALUE1_EXTRA)
    var reg3 uintBWL code = intern(string,pack,&value1); # Symbol nach value1
    #else
    var object value;
    var reg3 uintBWL code = intern(string,pack,&value); # Symbol nach value
    value1 = value;
    #endif
    value2 = intern_result(code); mv_count=2; # 2 Werte
  }}

LISPFUN(find_symbol,1,1,norest,nokey,0,NIL)
# (FIND-SYMBOL string [package]), CLTL S. 185
  { test_intern_args(); # Argumente überprüfen
   {var reg2 object pack = popSTACK();
    var reg1 object string = popSTACK();
    #if !defined(VALUE1_EXTRA)
    var reg3 uintBWL code = find_symbol(string,pack,&value1); # Symbol nach value1
    #else
    var object value;
    var reg3 uintBWL code = find_symbol(string,pack,&value); # Symbol nach value
    value1 = value;
    #endif
    value2 = intern_result(code); mv_count=2; # 2 Werte
  }}

LISPFUN(unintern,1,1,norest,nokey,0,NIL)
# (UNINTERN symbol [package]), CLTL S. 185
  { # Symbol überprüfen:
    if (!(msymbolp(STACK_1)))
      { pushSTACK(STACK_1); pushSTACK(TheSubr(subr_self)->name);
        fehler(
               DEUTSCH ? "~: Argument ~ ist kein Symbol." :
               ENGLISH ? "~: argument ~ is not a symbol" :
               FRANCAIS ? "~ : L'argument ~ n'est pas un symbole." :
               ""
              );
      }
    # Package überprüfen:
    test_optional_package_arg();
    # uninternieren:
    value1 = unintern(&STACK_1,&STACK_0); mv_count=1;
    skipSTACK(2);
  }

# Ausführer einer Funktion wie EXPORT, UNEXPORT, IMPORT, SHADOWING-IMPORT
# oder SHADOW. Überprüft, ob das erste Argument eine Symbolliste ist, ob
# das zweite Argument (Default: *PACKAGE*) eine Package ist, und wendet das
# Unterprogramm auf jedes der Symbole an. Rücksprung mit 1 Wert T.
# apply_symbols(&fun);
# Spezifikation des Unterprogrammes fun:
#   fun(&sym,&pack);
#   > sym: Symbol (im STACK)
#   > pack: Package (im STACK)
#   < pack: Package, EQ zur alten
#   kann GC auslösen
# < STACK: aufgeräumt
# kann GC auslösen
  typedef void sym_pack_function (object* sym_, object* pack_);
  local Values apply_symbols (sym_pack_function* fun);
  local Values apply_symbols(fun)
    var reg2 sym_pack_function* fun;
    { # Überprüfe, ob das erste Argument eine Symbolliste oder ein Symbol ist:
      { var reg1 object symarg = STACK_1;
        # auf Symbol testen:
        if (symbolp(symarg)) goto ok;
        # auf Symbolliste testen:
        while (consp(symarg)) # symarg durchläuft STACK_1
          { if (!(msymbolp(Car(symarg)))) goto not_ok;
            symarg = Cdr(symarg);
          }
        if (!(nullp(symarg))) goto not_ok; # Liste korrekt beendet?
        goto ok; # korrekte Symbolliste
        not_ok:
          pushSTACK(STACK_1); pushSTACK(TheSubr(subr_self)->name);
          fehler(
                 DEUTSCH ? "Argument zu ~ muß ein Symbol oder eine Symbolliste sein, nicht ~" :
                 ENGLISH ? "~: argument should be a symbol or a list of symbols, not ~" :
                 FRANCAIS ? "~ : L'argument de ~ doit être un symbole ou une liste de symboles et non ~." :
                 ""
                );
        ok: ;
      }
      # Package überprüfen:
      test_optional_package_arg();
      # Stackaufbau: symarg, pack.
      # fun auf alle Symbole anwenden:
      if (matomp(STACK_1))
        # einzelnes Symbol
        { # Stackaufbau: sym, pack.
          (*fun)(&STACK_1,&STACK_0);
          skipSTACK(2);
        }
        else
        # nichtleere Symbolliste
        { pushSTACK(NIL);
          do { var reg1 object symlistr = STACK_2;
               STACK_2 = Cdr(symlistr);
               STACK_0 = Car(symlistr); # Symbol
               # Stackaufbau: symlistr, pack, sym.
               (*fun)(&STACK_0,&STACK_1);
             }
             until (matomp(STACK_2));
          skipSTACK(3);
        }
      # beenden:
      value1 = T; mv_count=1;
    }

LISPFUN(export,1,1,norest,nokey,0,NIL)
# (EXPORT symbols [package]), CLTL S. 186
  { return_Values apply_symbols(&export); }

LISPFUN(unexport,1,1,norest,nokey,0,NIL)
# (UNEXPORT symbols [package]), CLTL S. 186
  { return_Values apply_symbols(&unexport); }

LISPFUN(import,1,1,norest,nokey,0,NIL)
# (IMPORT symbols [package]), CLTL S. 186
  { return_Values apply_symbols(&import); }

LISPFUN(shadowing_import,1,1,norest,nokey,0,NIL)
# (SHADOWING-IMPORT symbols [package]), CLTL S. 186
  { return_Values apply_symbols(&shadowing_import); }

LISPFUN(shadow,1,1,norest,nokey,0,NIL)
# (SHADOW symbols [package]), CLTL S. 186
  { return_Values apply_symbols(&shadow); }

# UP: Vorbereitung der Argumente bei USE-PACKAGE und UNUSE-PACKAGE.
# Das 1. Argument STACK_1 wird zu einer (neu erzeugten) Liste von Packages
# gemacht, das 2. Argument STACK_0 wird überprüft.
# > subr_self: Aufrufer (ein SUBR)
# kann GC auslösen
  local void prepare_use_package (void);
  local void prepare_use_package()
    { # 2. Argument (Package) überprüfen:
      test_optional_package_arg();
      # 1. Argument (Package oder Packageliste) überprüfen:
      { var reg2 object packs_to_use = STACK_1;
        if (!(listp(packs_to_use)))
          # packs_to_use keine Liste -> einelementige Liste draus machen:
          { pushSTACK(test_package_arg(packs_to_use)); # einzelne Package
           {var reg1 object new_cons = allocate_cons();
            Car(new_cons) = popSTACK();
            STACK_1 = new_cons;
          }}
          else
          # packs_to_use eine Liste -> neue Packageliste aufbauen:
          { pushSTACK(NIL); # mit NIL anfangen
            while (mconsp(STACK_2))
              { var reg1 object packlistr = STACK_2;
                STACK_2 = Cdr(packlistr);
                pushSTACK(test_package_arg(Car(packlistr))); # nächste Package
               {var reg1 object new_cons = allocate_cons();
                Car(new_cons) = popSTACK();
                Cdr(new_cons) = STACK_0;
                STACK_0 = new_cons;
              }}
           {var reg1 object packlist = popSTACK(); # neue Packageliste
            STACK_1 = packlist;
          }}
    } }

LISPFUN(use_package,1,1,norest,nokey,0,NIL)
# (USE-PACKAGE packs-to-use [package]), CLTL S. 187
  { prepare_use_package();
   {var reg2 object pack = popSTACK();
    var reg1 object packlist = popSTACK();
    use_package(packlist,pack);
    value1 = T; mv_count=1;
  }}

LISPFUN(unuse_package,1,1,norest,nokey,0,NIL)
# (UNUSE-PACKAGE packs-to-use [package]), CLTL S. 187
  { prepare_use_package();
   {var reg2 object pack = popSTACK();
    var reg1 object packlist = popSTACK();
    unuse_package(packlist,pack);
    value1 = T; mv_count=1;
  }}

# UP: Korrigiert einen Package(nick)name.
# > name: Gewünschter Packagename (Simple-String)
# > STACK_1: "Sie dürfen einen neuen (Nick)Name eingeben."
# > STACK_0: "Bitte neuen Package(nick)name eingeben:"
# < ergebnis: Noch nicht vorkommender Packagename
# kann GC auslösen
  local object correct_packname (object name);
  local object correct_packname(name)
    var reg2 object name;
    { while (!(nullp(find_package(name))))
        { # Package mit diesem Namen existiert schon
          { pushSTACK(STACK_1); # "Sie dürfen ... eingeben."
            pushSTACK(O(makepack_string3)); # "Eine Package mit dem Namen ~S gibt es schon."
            pushSTACK(name);
            funcall(S(cerror),3); # (CERROR "Sie dürfen ..." "Package ~S existiert" name)
          }
          { var object stream = var_stream(S(query_io)); # Stream *QUERY-IO*
            terpri(&stream); # neue Zeile
            write_sstring(&stream,STACK_0); # "Bitte ... eingeben:"
            pushSTACK(stream); funcall(L(read_line),1); # (READ-LINE stream)
            name = value1;
        } }
      return name;
    }

# UP für MAKE-PACKAGE und IN-PACKAGE:
# Bildet eine neue Package und liefert sie als Wert.
# > STACK_2: name-Argument
# > STACK_1: nicknames-Argument
# > STACK_0: uselist-Argument
# > subr_self: Aufrufer (ein SUBR)
# erhöht STACK um 3
# kann GC auslösen
  local void in_make_package (void);
  local void in_make_package()
    { # name in Simple-String und nicknames in neue Simple-String-Liste umwandeln:
      test_names_args();
      # name überprüfen und evtl. korrigieren:
      { pushSTACK(O(makepack_string1)); # "Sie dürfen einen neuen Namen eingeben."
        pushSTACK(O(makepack_string4)); # "Bitte neuen Packagenamen eingeben:"
        STACK_4 = correct_packname(STACK_4);
        skipSTACK(2);
      }
      # nicknames überprüfen und evtl. korrigieren:
      { pushSTACK(STACK_1); # nicknames durchgehen
        while (mconsp(STACK_0))
          { var reg1 object nickname = Car(STACK_0); # nickname herausgreifen
            pushSTACK(O(makepack_string2)); # "Sie dürfen einen neuen Nickname eingeben."
            pushSTACK(O(makepack_string5)); # "Bitte neuen Packagenickname eingeben:"
            nickname = correct_packname(nickname); # korrigieren
            skipSTACK(2);
            Car(STACK_0) = nickname; # und wieder einsetzen
            STACK_0 = Cdr(STACK_0);
          }
        skipSTACK(1);
      }
      # Package erzeugen:
      {var reg1 object pack = make_package(STACK_2,STACK_1);
       STACK_2 = pack; # und retten
       # Stackaufbau: pack, nicknames, uselist-Argument.
       # Defaultwert für Use-Argument verwenden:
       if (eq(STACK_0,unbound)) { STACK_0 = O(use_default); }
       # (USE-PACKAGE UseList neuePackage) ausführen:
       { pushSTACK(STACK_0); # UseList
         pushSTACK(pack); # neue Package
         funcall(L(use_package),2);
      }}
      skipSTACK(2);
      value1 = popSTACK(); mv_count=1; # Package als Wert
    }

LISPFUN(make_package,1,0,norest,key,2, (kw(nicknames),kw(use)) )
# (MAKE-PACKAGE name [:NICKNAMES nicknames] [:USE uselist]), CLTL S. 183
  { in_make_package(); }

LISPFUN(in_package,1,0,norest,key,2, (kw(nicknames),kw(use)) )
# (IN-PACKAGE name [:NICKNAMES nicknames] [:USE uselist]), CLTL S. 183
  { # name überprüfen und in String umwandeln:
    var reg3 object name = test_stringsym_arg(STACK_2);
    STACK_2 = name;
    # Package mit diesem Namen suchen:
   {var reg2 object pack = find_package(name);
    if (nullp(pack))
      # Package nicht gefunden, muß eine neue erzeugen
      { in_make_package(); }
      else
      # Package gefunden
      { STACK_2 = pack; # pack retten
        # Stackaufbau: pack, nicknames, uselist.
        # Die Nicknames anpassen:
        if (!eq(STACK_1,unbound))
          # Nicknames installieren mit RENAME-PACKAGE:
          { pushSTACK(pack); # pack
            pushSTACK(ThePackage(pack)->pack_name); # (package-name pack)
            pushSTACK(STACK_3); # nicknames
            funcall(L(rename_package),3); # (RENAME-PACKAGE pack (package-name pack) nicknames)
          }
        # Die Use-List anpassen:
        if (!eq(STACK_0,unbound))
          { # Use-List erweitern mit USE-PACKAGE
            # und verkürzen mit UNUSE-PACKAGE:
            pushSTACK(STACK_2); # pack als 2. Argument für USE-PACKAGE
            prepare_use_package(); # Argumente STACK_1, STACK_0 überprüfen
            skipSTACK(1);
            # Stackaufbau: pack, nicknames, neue Use-List.
            # USE-PACKAGE ausführen (mit kopierter Use-List):
            use_package(reverse(STACK_0),STACK_2);
            # Alle Packages, die jetzt noch in der Use-List von pack
            # aufgeführt sind, aber nicht in der in STACK_0 befindlichen
            # uselist vorkommen, werden mit unuse_1package entfernt:
            pack = STACK_2;
            { var reg3 object used_packs = ThePackage(pack)->pack_use_list; # Use-List von pack durchgehen
              while (consp(used_packs))
                { var reg2 object qpack = Car(used_packs);
                  # in uselist suchen:
                  var reg1 object listr = STACK_0;
                  while (consp(listr))
                    { if (eq(qpack,Car(listr))) goto unuse_ok; # in uselist gefunden -> OK
                      listr = Cdr(listr);
                    }
                  # nicht in uselist gefunden
                  unuse_1package(pack,qpack);
                  unuse_ok: ;
                  used_packs = Cdr(used_packs);
          } }   }
        # Die Use-List ist korrekt angepaßt.
        skipSTACK(2); # uselist und nicknames vergessen
        value1 = popSTACK(); mv_count=1; # pack als Wert
      }
    # Ergebnis an *PACKAGE* zuweisen:
    Symbol_value(S(packagestern)) = value1;
  }}

LISPFUNN(find_all_symbols,1)
# (FIND-ALL-SYMBOLS name), CLTL S. 187
  { STACK_0 = test_stringsym_arg(STACK_0); # name als String
    pushSTACK(NIL); # (bisher leere) Symbolliste
    pushSTACK(O(all_packages)); # Liste aller Packages durchgehen
    while (mconsp(STACK_0))
      { var reg2 object pack = Car(STACK_0); # nächste Package
        # in deren internen und externen Symbolen suchen:
        var object sym;
        if (symtab_lookup(STACK_2,ThePackage(pack)->pack_internal_symbols,&sym)
            || symtab_lookup(STACK_2,ThePackage(pack)->pack_external_symbols,&sym)
           )
          # gefunden: Symbol sym ist in Package pack präsent,
          # mit (pushnew sym STACK_1 :test #'eq) auf die Symbolliste consen:
          { # Suche, ob das gefundene Symbol sym in STACK_1 vorkommt:
            {var reg1 object symlistr = STACK_1;
             while (consp(symlistr))
               { if (eq(sym,Car(symlistr))) goto already_found; # gefunden -> nichts weiter zu tun
                 symlistr = Cdr(symlistr);
            }  }
            # nicht gefunden, muß consen:
            pushSTACK(sym);
            {var reg1 object new_cons = allocate_cons();
             Car(new_cons) = popSTACK();
             Cdr(new_cons) = STACK_1;
             STACK_1 = new_cons;
            }
            already_found: ;
          }
        STACK_0 = Cdr(STACK_0);
      }
    skipSTACK(1);
    value1 = popSTACK(); mv_count=1; # Symbolliste als Wert
    skipSTACK(1);
  }

LISPFUNN(map_symbols,2)
# (SYSTEM::MAP-SYMBOLS fun pack)
# wendet die Funktion fun auf alle in pack accessiblen Symbole an. Wert NIL.
  { # 2. Argument überprüfen:
    STACK_0 = test_package_arg(STACK_0);
    # fun auf alle internen Symbole loslassen:
    map_symtab(STACK_1,ThePackage(STACK_0)->pack_internal_symbols);
    # fun auf alle externen Symbole loslassen:
    map_symtab(STACK_1,ThePackage(STACK_0)->pack_external_symbols);
    # fun auf alle vererbten Symbole loslassen:
    STACK_0 = ThePackage(STACK_0)->pack_use_list; # Use-List durchgehen
    while (mconsp(STACK_0))
      { var reg1 object usedpack = Car(STACK_0); # nächste Package aus der Use-List
        STACK_0 = Cdr(STACK_0);
        map_symtab(STACK_1,ThePackage(usedpack)->pack_external_symbols);
      }
    skipSTACK(2);
    value1 = NIL; mv_count=1; # NIL als Wert
  }

LISPFUNN(map_external_symbols,2)
# (SYSTEM::MAP-EXTERNAL-SYMBOLS fun pack)
# wendet die Funktion fun auf alle in pack externen Symbole an. Wert NIL.
  { # 2. Argument überprüfen:
    var reg1 object pack = test_package_arg(popSTACK());
    # fun auf alle externen Symbole loslassen:
    map_symtab(popSTACK(),ThePackage(pack)->pack_external_symbols);
    value1 = NIL; mv_count=1; # NIL als Wert
  }

LISPFUNN(map_all_symbols,1)
# (SYSTEM::MAP-ALL-SYMBOLS fun)
# wendet die Funktion fun auf alle in irgendeiner Package präsenten Symbole an.
  { pushSTACK(O(all_packages)); # Package-Liste durchgehen
    while (mconsp(STACK_0))
      { var reg1 object pack = Car(STACK_0); # nächste Package
        STACK_0 = Cdr(STACK_0);
        pushSTACK(pack); # retten
        # fun auf alle internen Symbole loslassen:
        map_symtab(STACK_2,ThePackage(pack)->pack_internal_symbols);
        pack = popSTACK();
        # fun auf alle externen Symbole loslassen:
        map_symtab(STACK_1,ThePackage(pack)->pack_external_symbols);
      }
    skipSTACK(2);
    value1 = NIL; mv_count=1; # NIL als Wert
  }

# UP: Initialisiert die Packageverwaltung
# init_packages();
  global void init_packages (void);
  global void init_packages()
    { pushSTACK(asciz_to_string("LISP"));
      pushSTACK(asciz_to_string("USER"));
      pushSTACK(asciz_to_string("SYSTEM"));
      pushSTACK(asciz_to_string("SYS"));
      pushSTACK(asciz_to_string("KEYWORD"));
      pushSTACK(asciz_to_string(""));
      # Stackaufbau: "LISP", "USER", "SYSTEM", "SYS", "KEYWORD", "".
      O(all_packages) = NIL; # ALL_PACKAGES := NIL
      # #<PACKAGE KEYWORD> einrichten:
      {var reg1 object new_cons = allocate_cons();
       Car(new_cons) = popSTACK(); # ""
       O(keyword_package) = make_package(popSTACK(),new_cons); # "KEYWORD",("")
      }
      # #<PACKAGE SYSTEM> einrichten:
      {var reg1 object new_cons = allocate_cons();
       Car(new_cons) = STACK_0; # "SYS"
       STACK_0 = make_package(STACK_1,new_cons); # "SYSTEM",("SYS")
      }
      # #<PACKAGE USER> einrichten:
      O(default_package) = # und zur Default-Package machen
      STACK_2 = make_package(STACK_2,NIL); # "USER",()
      # #<PACKAGE LISP> einrichten:
      STACK_3 = STACK_1 = make_package(STACK_3,NIL); # "LISP",()
      # Stackaufbau: #<PACKAGE LISP>, #<PACKAGE USER>, #<PACKAGE LISP>, #<PACKAGE SYSTEM>.
      # (USE-PACKAGE '#<PACKAGE LISP> '#<PACKAGE SYSTEM>) :
      funcall(L(use_package),2);
      # (USE-PACKAGE '#<PACKAGE LISP> '#<PACKAGE USER>) :
      funcall(L(use_package),2);
    }

