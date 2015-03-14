# Liste aller dem C-Programm bekannten Objekte ("Programmkonstanten")
# Bruno Haible 3.3.1993

# Die Symbole sind bereits speziell abgehandelt.
# Es wird eine Tabelle aller sonstigen dem C-Programm bekannten Objekte
# gehalten.

# Der Macro LISPOBJ deklariert ein sonstiges LISP-Objekt.
# LISPOBJ(name,initstring)
# > name: Objekt ist als object_tab.name oder als O(name) ansprechbar
# > initstring: Initialisierungsstring in LISP-Syntax

# Expander für die Deklaration der Objekt-Tabelle:
  #define LISPOBJ_A(name,initstring)  \
    object name;

# Expander für die Initialisierung der Objekt-Tabelle:
  #define LISPOBJ_B(name,initstring)  \
    *ptr++ = NIL;
  #define LISPOBJ_C(name,initstring)  \
    NIL,
  #define LISPOBJ_D(name,initstring)  \
    initstring,

# Welcher Expander benutzt wird, muß vom Hauptfile aus eingestellt werden.

# zu EVAL.D:
  # Die 5 aktuellen Environments:
  LISPOBJ(akt_var_env,"NIL")    # --+
  LISPOBJ(akt_fun_env,"NIL")    #   | Reihenfolge
  LISPOBJ(akt_block_env,"NIL")  #   | mit LISPBIBL.D
  LISPOBJ(akt_go_env,"NIL")     #   | abgestimmt!
  LISPOBJ(akt_decl_env,"NIL")   # --+
# zu CHARSTRG.D:
  # Bei Änderung der Character-Namen außer CONSTOBJ.D auch
  # CHARSTRG.D, FORMAT.LSP, IMPNOTES.TXT anpassen!
  #ifdef ATARI_CHARNAMES
    # Namen von Characters mit Codes 0,5,...,13,26,27,32,8,10:
    LISPOBJ(charname_0,"\"Null\"")
    LISPOBJ(charname_5,"\"Code5\"")
    LISPOBJ(charname_6,"\"Code6\"")
    LISPOBJ(charname_7,"\"Bell\"")
    LISPOBJ(charname_8,"\"Backspace\"")
    LISPOBJ(charname_9,"\"Tab\"")
    LISPOBJ(charname_10,"\"Newline\"")
    LISPOBJ(charname_11,"\"Code11\"")
    LISPOBJ(charname_12,"\"Page\"")
    LISPOBJ(charname_13,"\"Return\"")
    LISPOBJ(charname_26,"\"Code26\"")
    LISPOBJ(charname_27,"\"Escape\"")
    LISPOBJ(charname_32,"\"Space\"")
    LISPOBJ(charname_8bis,"\"Rubout\"")
    LISPOBJ(charname_10bis,"\"Linefeed\"")
    # Namen von Characters mit Hyper-Bit:
    LISPOBJ(charname_hyper_13,"\"Enter\"") # #\Hyper-Return
    LISPOBJ(charname_hyper_16,"\"Insert\"") # #\Hyper-Code16
    LISPOBJ(charname_hyper_17,"\"End\"") # #\Hyper-Code17
    LISPOBJ(charname_hyper_18,"\"Down\"") # #\Hyper-Code18
    LISPOBJ(charname_hyper_19,"\"PgDn\"") # #\Hyper-Code19
    LISPOBJ(charname_hyper_20,"\"Left\"") # #\Hyper-Code20
    LISPOBJ(charname_hyper_22,"\"Right\"") # #\Hyper-Code22
    LISPOBJ(charname_hyper_23,"\"Home\"") # #\Hyper-Code23
    LISPOBJ(charname_hyper_24,"\"Up\"") # #\Hyper-Code24
    LISPOBJ(charname_hyper_25,"\"PgUp\"") # #\Hyper-Code25
    LISPOBJ(charname_hyper_28,"\"Help\"") # #\Hyper-Code28
    LISPOBJ(charname_hyper_29,"\"Undo\"") # #\Hyper-Code29
    LISPOBJ(charname_hyper_127,"\"Delete\"") # #\Hyper-Code127
    LISPOBJ(charname_hyper_a,"\"F1\"") # #\Hyper-A
    LISPOBJ(charname_hyper_b,"\"F2\"") # #\Hyper-B
    LISPOBJ(charname_hyper_c,"\"F3\"") # #\Hyper-C
    LISPOBJ(charname_hyper_d,"\"F4\"") # #\Hyper-D
    LISPOBJ(charname_hyper_e,"\"F5\"") # #\Hyper-E
    LISPOBJ(charname_hyper_f,"\"F6\"") # #\Hyper-F
    LISPOBJ(charname_hyper_g,"\"F7\"") # #\Hyper-G
    LISPOBJ(charname_hyper_h,"\"F8\"") # #\Hyper-H
    LISPOBJ(charname_hyper_i,"\"F9\"") # #\Hyper-I
    LISPOBJ(charname_hyper_j,"\"F10\"") # #\Hyper-J
    LISPOBJ(charname_hyper_k,"\"F11\"") # #\Hyper-K
    LISPOBJ(charname_hyper_l,"\"F12\"") # #\Hyper-L
  #endif
  #ifdef AMIGA_CHARNAMES
    LISPOBJ(charname_0,"\"Null\"")
    LISPOBJ(charname_1,"\"Code1\"")
    LISPOBJ(charname_2,"\"Code2\"")
    LISPOBJ(charname_3,"\"Code3\"")
    LISPOBJ(charname_4,"\"Code4\"")
    LISPOBJ(charname_5,"\"Code5\"")
    LISPOBJ(charname_6,"\"Code6\"")
    LISPOBJ(charname_7,"\"Bell\"")
    LISPOBJ(charname_8,"\"Backspace\"")
    LISPOBJ(charname_9,"\"Tab\"")
    LISPOBJ(charname_10,"\"Newline\"")
    LISPOBJ(charname_11,"\"Vt\"")
    LISPOBJ(charname_12,"\"Page\"")
    LISPOBJ(charname_13,"\"Return\"")
    LISPOBJ(charname_14,"\"So\"")
    LISPOBJ(charname_15,"\"Si\"")
    LISPOBJ(charname_16,"\"Code16\"")
    LISPOBJ(charname_17,"\"Code17\"")
    LISPOBJ(charname_18,"\"Code18\"")
    LISPOBJ(charname_19,"\"Code19\"")
    LISPOBJ(charname_20,"\"Code20\"")
    LISPOBJ(charname_21,"\"Code21\"")
    LISPOBJ(charname_22,"\"Code22\"")
    LISPOBJ(charname_23,"\"Code23\"")
    LISPOBJ(charname_24,"\"Code24\"")
    LISPOBJ(charname_25,"\"Code25\"")
    LISPOBJ(charname_26,"\"Code26\"")
    LISPOBJ(charname_27,"\"Escape\"")
    LISPOBJ(charname_28,"\"Code28\"")
    LISPOBJ(charname_29,"\"Code29\"")
    LISPOBJ(charname_30,"\"Code30\"")
    LISPOBJ(charname_31,"\"Code31\"")
    LISPOBJ(charname_32,"\"Space\"")
    LISPOBJ(charname_7bis,"\"Bel\"")
    LISPOBJ(charname_8bis,"\"Bs\"")
    LISPOBJ(charname_8tris,"\"Rubout\"")
    LISPOBJ(charname_9bis,"\"Ht\"")
    LISPOBJ(charname_10bis,"\"Linefeed\"")
    LISPOBJ(charname_10tris,"\"Lf\"")
    LISPOBJ(charname_12bis,"\"Ff\"")
    LISPOBJ(charname_13bis,"\"Cr\"")
    LISPOBJ(charname_27bis,"\"Esc\"")
    LISPOBJ(charname_155,"\"Csi\"")
  #endif
  #ifdef MSDOS_CHARNAMES
    # Namen von Characters mit Codes 0,7,...,13,26,27,32,8,10:
    LISPOBJ(charname_0,"\"Null\"")
    LISPOBJ(charname_7,"\"Bell\"")
    LISPOBJ(charname_8,"\"Backspace\"")
    LISPOBJ(charname_9,"\"Tab\"")
    LISPOBJ(charname_10,"\"Newline\"")
    LISPOBJ(charname_11,"\"Code11\"")
    LISPOBJ(charname_12,"\"Page\"")
    LISPOBJ(charname_13,"\"Return\"")
    LISPOBJ(charname_26,"\"Code26\"")
    LISPOBJ(charname_27,"\"Escape\"")
    LISPOBJ(charname_32,"\"Space\"")
    LISPOBJ(charname_8bis,"\"Rubout\"")
    LISPOBJ(charname_10bis,"\"Linefeed\"")
    # Namen von Characters mit Hyper-Bit:
    LISPOBJ(charname_hyper_13,"\"Enter\"") # #\Hyper-Return
    LISPOBJ(charname_hyper_16,"\"Insert\"") # #\Hyper-Code16
    LISPOBJ(charname_hyper_17,"\"End\"") # #\Hyper-Code17
    LISPOBJ(charname_hyper_18,"\"Down\"") # #\Hyper-Code18
    LISPOBJ(charname_hyper_19,"\"PgDn\"") # #\Hyper-Code19
    LISPOBJ(charname_hyper_20,"\"Left\"") # #\Hyper-Code20
    LISPOBJ(charname_hyper_22,"\"Right\"") # #\Hyper-Code22
    LISPOBJ(charname_hyper_23,"\"Home\"") # #\Hyper-Code23
    LISPOBJ(charname_hyper_24,"\"Up\"") # #\Hyper-Code24
    LISPOBJ(charname_hyper_25,"\"PgUp\"") # #\Hyper-Code25
    LISPOBJ(charname_hyper_29,"\"Prtscr\"") # #\Hyper-Code29
    LISPOBJ(charname_hyper_127,"\"Delete\"") # #\Hyper-Code127
    LISPOBJ(charname_hyper_a,"\"F1\"") # #\Hyper-A
    LISPOBJ(charname_hyper_b,"\"F2\"") # #\Hyper-B
    LISPOBJ(charname_hyper_c,"\"F3\"") # #\Hyper-C
    LISPOBJ(charname_hyper_d,"\"F4\"") # #\Hyper-D
    LISPOBJ(charname_hyper_e,"\"F5\"") # #\Hyper-E
    LISPOBJ(charname_hyper_f,"\"F6\"") # #\Hyper-F
    LISPOBJ(charname_hyper_g,"\"F7\"") # #\Hyper-G
    LISPOBJ(charname_hyper_h,"\"F8\"") # #\Hyper-H
    LISPOBJ(charname_hyper_i,"\"F9\"") # #\Hyper-I
    LISPOBJ(charname_hyper_j,"\"F10\"") # #\Hyper-J
    LISPOBJ(charname_hyper_k,"\"F11\"") # #\Hyper-K
    LISPOBJ(charname_hyper_l,"\"F12\"") # #\Hyper-L
  #endif
  #ifdef UNIX_CHARNAMES
    LISPOBJ(charname_0bis,"\"Null\"")
    LISPOBJ(charname_7bis,"\"Bell\"")
    LISPOBJ(charname_8bis,"\"Backspace\"")
    LISPOBJ(charname_9bis,"\"Tab\"")
    LISPOBJ(charname_10bis,"\"Newline\"")
    LISPOBJ(charname_10tris,"\"Linefeed\"")
    LISPOBJ(charname_12bis,"\"Page\"")
    LISPOBJ(charname_13bis,"\"Return\"")
    LISPOBJ(charname_27bis,"\"Escape\"")
    LISPOBJ(charname_32bis,"\"Space\"")
    LISPOBJ(charname_127bis,"\"Rubout\"")
    LISPOBJ(charname_127tris,"\"Delete\"")
    LISPOBJ(charname_0,"\"Nul\"")
    LISPOBJ(charname_1,"\"Soh\"")
    LISPOBJ(charname_2,"\"Stx\"")
    LISPOBJ(charname_3,"\"Etx\"")
    LISPOBJ(charname_4,"\"Eot\"")
    LISPOBJ(charname_5,"\"Enq\"")
    LISPOBJ(charname_6,"\"Ack\"")
    LISPOBJ(charname_7,"\"Bel\"")
    LISPOBJ(charname_8,"\"Bs\"")
    LISPOBJ(charname_9,"\"Ht\"")
    LISPOBJ(charname_10,"\"Nl\"")
    LISPOBJ(charname_11,"\"Vt\"")
    LISPOBJ(charname_12,"\"Np\"")
    LISPOBJ(charname_13,"\"Cr\"")
    LISPOBJ(charname_14,"\"So\"")
    LISPOBJ(charname_15,"\"Si\"")
    LISPOBJ(charname_16,"\"Dle\"")
    LISPOBJ(charname_17,"\"Dc1\"")
    LISPOBJ(charname_18,"\"Dc2\"")
    LISPOBJ(charname_19,"\"Dc3\"")
    LISPOBJ(charname_20,"\"Dc4\"")
    LISPOBJ(charname_21,"\"Nak\"")
    LISPOBJ(charname_22,"\"Syn\"")
    LISPOBJ(charname_23,"\"Etb\"")
    LISPOBJ(charname_24,"\"Can\"")
    LISPOBJ(charname_25,"\"Em\"")
    LISPOBJ(charname_26,"\"Sub\"")
    LISPOBJ(charname_27,"\"Esc\"")
    LISPOBJ(charname_28,"\"Fs\"")
    LISPOBJ(charname_29,"\"Gs\"")
    LISPOBJ(charname_30,"\"Rs\"")
    LISPOBJ(charname_31,"\"Us\"")
    LISPOBJ(charname_32,"\"Sp\"")
    LISPOBJ(charname_127,"\"Del\"")
    # Namen von Characters mit Hyper-Bit:
    LISPOBJ(charname_hyper_16,"\"Insert\"") # #\Hyper-Code16
    LISPOBJ(charname_hyper_17,"\"End\"") # #\Hyper-Code17
    LISPOBJ(charname_hyper_18,"\"Down\"") # #\Hyper-Code18
    LISPOBJ(charname_hyper_19,"\"PgDn\"") # #\Hyper-Code19
    LISPOBJ(charname_hyper_20,"\"Left\"") # #\Hyper-Code20
    LISPOBJ(charname_hyper_21,"\"Center\"") # #\Hyper-Code21
    LISPOBJ(charname_hyper_22,"\"Right\"") # #\Hyper-Code22
    LISPOBJ(charname_hyper_23,"\"Home\"") # #\Hyper-Code23
    LISPOBJ(charname_hyper_24,"\"Up\"") # #\Hyper-Code24
    LISPOBJ(charname_hyper_25,"\"PgUp\"") # #\Hyper-Code25
    LISPOBJ(charname_hyper_a,"\"F1\"") # #\Hyper-A
    LISPOBJ(charname_hyper_b,"\"F2\"") # #\Hyper-B
    LISPOBJ(charname_hyper_c,"\"F3\"") # #\Hyper-C
    LISPOBJ(charname_hyper_d,"\"F4\"") # #\Hyper-D
    LISPOBJ(charname_hyper_e,"\"F5\"") # #\Hyper-E
    LISPOBJ(charname_hyper_f,"\"F6\"") # #\Hyper-F
    LISPOBJ(charname_hyper_g,"\"F7\"") # #\Hyper-G
    LISPOBJ(charname_hyper_h,"\"F8\"") # #\Hyper-H
    LISPOBJ(charname_hyper_i,"\"F9\"") # #\Hyper-I
    LISPOBJ(charname_hyper_j,"\"F10\"") # #\Hyper-J
    LISPOBJ(charname_hyper_k,"\"F11\"") # #\Hyper-K
    LISPOBJ(charname_hyper_l,"\"F12\"") # #\Hyper-L
  #endif
  # Tabelle der Bitnamen:
  LISPOBJ(bitnamekw_0,":CONTROL")
  LISPOBJ(bitnamekw_1,":META")
  LISPOBJ(bitnamekw_2,":SUPER")
  LISPOBJ(bitnamekw_3,":HYPER")
# zu SEQUENCE.D:
  # interne Liste aller definierten Sequence-Typen:
  LISPOBJ(seq_types,"NIL")
  # Keywordpaare für test_start_end (Paare nicht trennen!):
  LISPOBJ(kwpair_start,":START")
  LISPOBJ(kwpair_end,":END")
  LISPOBJ(kwpair_start1,":START1")
  LISPOBJ(kwpair_end1,":END1")
  LISPOBJ(kwpair_start2,":START2")
  LISPOBJ(kwpair_end2,":END2")
# zu PACKAGE.D:
  # interne Liste aller Packages:
  LISPOBJ(all_packages,".")
  # die Keyword-Package:
  LISPOBJ(keyword_package,".")
  # die Default-Package für *PACKAGE*:
  LISPOBJ(default_package,".")
  # verschiedene Strings und Listen für interaktive Konfliktbehebung:
  LISPOBJ(query_string1,
    DEUTSCH ? "\"Wählen Sie bitte aus:\"" :
    ENGLISH ? "\"Please choose:\"" :
    FRANCAIS ? "\"Choisissez :\"" :
    "")
  LISPOBJ(query_string2,"\"          \"")
  LISPOBJ(query_string3,"\"  --  \"")
  LISPOBJ(query_string4,
    DEUTSCH ? "\"Wählen Sie bitte eines von \"" :
    ENGLISH ? "\"Please choose one of \"" :
    FRANCAIS ? "\"Choisissez parmi \"" :
    "")
  LISPOBJ(query_string5,"\", \"")
  LISPOBJ(query_string6,
    DEUTSCH ? "\" aus.\"" :
    ENGLISH ? "\" .\"" :
    FRANCAIS ? "\", s.v.p.\"" :
    "")
  LISPOBJ(query_string7,"\"> \"")
  LISPOBJ(unint_string1,
    DEUTSCH ? "\"Symbol \"" :
    ENGLISH ? "\"symbol \"" :
    FRANCAIS ? "\"Le symbole \"" :
    "")
  LISPOBJ(unint_string2,
    DEUTSCH ? "\" aus #<PACKAGE \"" :
    ENGLISH ? "\" from #<PACKAGE \"" :
    FRANCAIS ? "\" du paquetage #<PACKAGE \"" :
    "")
  LISPOBJ(unint_string3,
    DEUTSCH ? "\"> wird als Shadowing deklariert\"" :
    ENGLISH ? "\"> will become a shadowing symbol\"" :
    FRANCAIS ? "\"> sera déclaré «shadowing».\"" :
    "")
  LISPOBJ(unint_string4,
    DEUTSCH ? "\"Sie dürfen auswählen, welches der gleichnamigen Symbole Vorrang bekommt, um den Konflikt aufzulösen.\"" :
    ENGLISH ? "\"You may choose the symbol in favour of which to resolve the conflict.\"" :
    FRANCAIS ? "\"Vous pouvez choisir, parmi les symboles homonymes, auquel donner priorité pour éviter le conflit de noms.\"" :
    "")
  LISPOBJ(unint_string5,
    DEUTSCH ? "\"Durch Uninternieren von ~S aus ~S entsteht ein Namenskonflikt.\"" :
    ENGLISH ? "\"uninterning ~S from ~S uncovers a name conflict.\"" :
    FRANCAIS ? "\"Un conflit de noms apparaît dès que ~S est retiré de ~S.\"" :
    "")
  LISPOBJ(import_string1,
    DEUTSCH ? "\"Sie dürfen über das weitere Vorgehen entscheiden.\"" :
    ENGLISH ? "\"You may choose how to proceed.\"" :
    FRANCAIS ? "\"Vous pouvez décider de la démarche à suivre.\"" :
    "")
  LISPOBJ(import_string2,
    DEUTSCH ? "\"Durch Importieren von ~S in ~S entsteht ein Namenskonflikt mit ~S.\"" :
    ENGLISH ? "\"importing ~S into ~S produces a name conflict with ~S.\"" :
    FRANCAIS ? "\"Un conflit de noms apparaît par l'importation de ~S dans ~S avec ~S.\"" :
    "")
  LISPOBJ(import_string3,
    DEUTSCH ? "\"Durch Importieren von ~S in ~S entsteht ein Namenskonflikt mit ~S und weiteren Symbolen.\"" :
    ENGLISH ? "\"importing ~S into ~S produces a name conflict with ~S and other symbols.\"" :
    FRANCAIS ? "\"Un conflit de noms apparaît par l'importation de ~S dans ~S avec ~S et d'autres symboles.\"" :
    "")
  LISPOBJ(import_list1,
    DEUTSCH ? "((\"I\" \"Importieren und dabei das eine andere Symbol uninternieren\" T)"
              " (\"N\" \"Nicht importieren, alles beim alten lassen\" NIL))" :
    ENGLISH ? "((\"I\" \"import it and unintern the other symbol\" T)"
              " (\"N\" \"do not import it, leave undone\" NIL))" :
    FRANCAIS ? "((\"I\" \"Importer en retirant l'autre symbole\" T)"
               " (\"N\" \"Ne pas importer, ne rien faire\" NIL))" :
    "")
  LISPOBJ(import_list2,
    DEUTSCH ? "((\"I\" \"Importieren, dabei das eine andere Symbol uninternieren und die anderen Symbole verdecken\" T)"
              " (\"N\" \"Nicht importieren, alles beim alten lassen\" NIL))" :
    ENGLISH ? "((\"I\" \"import it, unintern one other symbol and shadow the other symbols\" T)"
              " (\"N\" \"do not import it, leave undone\" NIL))" :
    FRANCAIS ? "((\"I\" \"Importer en retirant l'autre symbole et en cachant les autres\" T)"
               " (\"N\" \"Ne pas importer, ne rien faire\" NIL))" :
    "")
  LISPOBJ(import_list3,
    DEUTSCH ? "((\"I\" \"Importieren und das andere Symbol shadowen\" T) (\"N\" \"Nichts tun\" NIL))" :
    ENGLISH ? "((\"I\" \"import it and shadow the other symbol\" T) (\"N\" \"do nothing\" NIL))" :
    FRANCAIS ? "((\"I\" \"Importer et cacher l'autre symbole\" T) (\"N\" \"Ne rien faire\"NIL))" :
    "")
  LISPOBJ(export_string1,
    DEUTSCH ? "\"Sie dürfen über das weitere Vorgehen entscheiden.\"" :
    ENGLISH ? "\"You may choose how to proceed.\"" :
    FRANCAIS ? "\"Vous pouvez décider de la démarche à suivre.\"" :
    "")
  LISPOBJ(export_string2,
    DEUTSCH ? "\"Symbol ~S müßte erst in ~S importiert werden, bevor es exportiert werden kann.\"" :
    ENGLISH ? "\"symbol ~S should be imported into ~S before being exported.\"" :
    FRANCAIS ? "\"Le symbole ~S devrait d'abord être importé avant de pouvoir être exporté.\"" :
    "")
  LISPOBJ(export_list1,
    DEUTSCH ? "((\"I\" \"Symbol erst importieren\" T)"
              " (\"N\" \"Nichts tun, Symbol nicht exportieren\" NIL))" :
    ENGLISH ? "((\"I\" \"import the symbol first\" T)"
              " (\"N\" \"do nothing, don't export the symbol\" NIL))" :
    FRANCAIS ? "((\"I\" \"Tout d'abord importer le symbole\" NIL)"
               " (\"N\" \"Ne rien faire, ne pas exporter le symbole\" T))" :
    "")
  LISPOBJ(export_string3,
    DEUTSCH ? "\"Sie dürfen aussuchen, welches Symbol Vorrang hat.\"" :
    ENGLISH ? "\"You may choose in favour of which symbol to resolve the conflict.\"" :
    FRANCAIS ? "\"Vous pouvez choisir à quel symbole donner priorité.\"" :
    "")
  LISPOBJ(export_string4,
    DEUTSCH ? "\"Durch Exportieren von ~S aus ~S ergibt sich ein Namenskonflikt mit ~S in ~S.\"" :
    ENGLISH ? "\"exporting ~S from ~S produces a name conflict with ~S from ~S.\"" :
    FRANCAIS ? "\"Un conflit de noms apparaît par l'exportation de ~S depuis ~S avec ~S de ~S.\"" :
    "")
  LISPOBJ(export_string5,
    DEUTSCH ? "\"Welches Symbol soll in \"" :
    ENGLISH ? "\"Which symbol should be accessible in \"" :
    FRANCAIS ? "\"Quel symbole devrait obtenir la priorité dans \"" :
    "")
  LISPOBJ(export_string6,
    DEUTSCH ? "\" Vorrang haben?\"" :
    ENGLISH ? "\" ?\"" :
    FRANCAIS ? "\" ?\"" :
    "")
  LISPOBJ(export_string7,"\"1\"")
  LISPOBJ(export_string8,"\"2\"")
  LISPOBJ(export_string9,
    DEUTSCH ? "\"Das zu exportierende Symbol \"" :
    ENGLISH ? "\"the symbol to export, \"" :
    FRANCAIS ? "\"Le symbole à exporter \"" :
    "")
  LISPOBJ(export_string10,
    DEUTSCH ? "\"Das alte Symbol \"" :
    ENGLISH ? "\"the old symbol, \"" :
    FRANCAIS ? "\"Le symbole original \"" :
    "")
  LISPOBJ(usepack_string1,
    DEUTSCH ? "\"Sie dürfen bei jedem Konflikt angeben, welches Symbol Vorrang haben soll.\"" :
    ENGLISH ? "\"You may choose for every conflict in favour of which symbol to resolve it.\"" :
    FRANCAIS ? "\"Pour chaque conflit, vous pouvez choisir à quel symbole donner priorité.\"" :
    "")
  LISPOBJ(usepack_string2,
    DEUTSCH ? "\"~S Namenskonflikte bei USE-PACKAGE von ~S in die Package ~S.\"" :
    ENGLISH ? "\"~S name conflicts while executing USE-PACKAGE of ~S into package ~S.\"" :
    FRANCAIS ? "\"~S conflits de nom par USE-PACKAGE de ~S dans le paquetage ~S.\"" :
    "")
  LISPOBJ(usepack_string3,
    DEUTSCH ? "\"Welches Symbol mit dem Namen \"" :
    ENGLISH ? "\"which symbol with name \"" :
    FRANCAIS ? "\"À quel symbole de nom \"" :
    "")
  LISPOBJ(usepack_string4,
    DEUTSCH ? "\" soll in \"" :
    ENGLISH ? "\" should be accessible in \"" :
    FRANCAIS ? "\" donner priorité dans \"" :
    "")
  LISPOBJ(usepack_string5,
    DEUTSCH ? "\" Vorrang haben?\"" :
    ENGLISH ? "\" ?\"" :
    FRANCAIS ? "\" ?\"" :
    "")
  LISPOBJ(makepack_string1,
    DEUTSCH ? "\"Sie dürfen einen neuen Namen eingeben.\"" :
    ENGLISH ? "\"You can input another name.\"" :
    FRANCAIS ? "\"Vous pouvez entrer un nouveau nom.\"" :
    "")
  LISPOBJ(makepack_string2,
    DEUTSCH ? "\"Sie dürfen einen neuen Nickname eingeben.\"" :
    ENGLISH ? "\"You can input another nickname.\"" :
    FRANCAIS ? "\"Vous pouvez entrer un nouveau nom supplémentaire.\"" :
    "")
  LISPOBJ(makepack_string3,
    DEUTSCH ? "\"Eine Package mit dem Namen ~S gibt es schon.\"" :
    ENGLISH ? "\"a package with name ~S already exists.\"" :
    FRANCAIS ? "\"Il existe déjà un paquetage de nom ~S.\"" :
    "")
  LISPOBJ(makepack_string4,
    DEUTSCH ? "\"Bitte neuen Packagenamen eingeben:\"" :
    ENGLISH ? "\"Please input new package name:\"" :
    FRANCAIS ? "\"Prière d'entrer un nouveau nom de paquetage :\"" :
    "")
  LISPOBJ(makepack_string5,
    DEUTSCH ? "\"Bitte neuen Packagenickname eingeben:\"" :
    ENGLISH ? "\"Please input new package nickname:\"" :
    FRANCAIS ? "\"Prière d'entrer un nouveau nom supplémentaire du paquetage :\"" :
    "")
  # Default-Use-List:
  LISPOBJ(use_default,"(\"LISP\")")
# zu SYMBOL.D:
  # Status von gensym:
  LISPOBJ(gensym_prefix,"\"G\"") # Präfix für gensym, ein String
  LISPOBJ(gensym_count,"0") # Zähler für gensym, ein Integer >=0
# zu MISC.D:
  # Eigenwissen:
  LISPOBJ(lisp_implementation_type_string,"\"CLISP\"")
  LISPOBJ(lisp_implementation_version_string,
    DEUTSCH ? "\"März 1993\"" :
    ENGLISH ? "\"March 1993\"" :
    FRANCAIS ? "\"Mars 1993\"" :
    "")
  LISPOBJ(oldversion, # Version im alten Format, für Kompatibilität. Bald wieder herausnehmen!
                  "( #.(fifth *features*)" # Symbol SYS::CLISP2 bzw. SYS::CLISP3
                   " #.sys::*jmpbuf-size*" # Zahl *jmpbuf-size*
                   " 210292" # Datum der letzten Änderung des Bytecode-Interpreters
                  ")"
         )
  LISPOBJ(version,"( #.(fifth *features*)" # Symbol SYS::CLISP2 bzw. SYS::CLISP3
                   " #.sys::*jmpbuf-size*" # Zahl *jmpbuf-size*
                   " #.sys::*big-endian*"  # Flag *big-endian*
                   " 210292" # Datum der letzten Änderung des Bytecode-Interpreters
                  ")"
         )
  #ifdef MACHINE_KNOWN
   #ifndef VMS
    LISPOBJ(machine_type_string,"NIL")
    LISPOBJ(machine_version_string,"NIL")
   #else # VMS
    LISPOBJ(machine_type_string,"VAX-VMS")
    LISPOBJ(machine_version_string,"VAX-VMS")
   #endif
    LISPOBJ(machine_instance_string,"NIL")
  #endif
  LISPOBJ(software_type_string,
    DEUTSCH ? "\"ANSI-C-Programm\"" :
    ENGLISH ? "\"ANSI C program\"" :
    FRANCAIS ? "\"Programme en ANSI C\"" :
    "")
  LISPOBJ(software_version_string,"\"" C_compiler_name "\"")
  LISPOBJ(language_string,
    DEUTSCH ? "\"DEUTSCH\"" :
    ENGLISH ? "\"ENGLISH\"" :
    FRANCAIS ? "\"FRANCAIS\"" :
    "")
 #ifdef TIME_RELATIVE
  # Start-Universal-Time:
  LISPOBJ(start_UT,"NIL")
 #endif
  # Errormeldungs-Startstring:
  LISPOBJ(error_string1,"\"*** - \"")
# zu PATHNAME.D:
  LISPOBJ(leer_string,"\"\"")
  LISPOBJ(wild_string,"\"*\"")
  LISPOBJ(doppelpunkt_string,"\":\"")
 #if defined(PATHNAME_ATARI) || defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2)
  LISPOBJ(backslash_string,"\"\\\\\"")
 #endif
 #if defined(PATHNAME_UNIX) || defined(PATHNAME_AMIGAOS)
  LISPOBJ(slash_string,"\"/\"")
 #endif
 #if defined(PATHNAME_VMS)
  LISPOBJ(parent_string,"\"-\"")
  LISPOBJ(zweidoppelpunkt_string,"\"::\"")
  LISPOBJ(bracket1_string,"\"[\"")
  LISPOBJ(bracket2_string,"\"]\"")
  LISPOBJ(strichpunkt_string,"\";\"")
  LISPOBJ(zero_string,"\"0\"")
  LISPOBJ(parentdir_string,"\"[.-]\"")
 #else
  LISPOBJ(punkt_string,"\".\"")
  LISPOBJ(punktpunkt_string,"\"..\"")
  LISPOBJ(punktpunktpunkt_string,"\"...\"")
 #endif
 #if defined(PATHNAME_ATARI) || defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2)
  LISPOBJ(wild_wild_string,"\"*.*\"")
 #endif
 #ifdef PATHNAME_VMS
  LISPOBJ(wild_wild_string,"\"*.*;*\"")
 #endif
  LISPOBJ(null_string,"\"0\"") # String aus einem Nullbyte
 #if defined(PATHNAME_ATARI) || defined(PATHNAME_MSDOS) || defined(PATHNAME_VMS)
  LISPOBJ(backuptype_string,"\"BAK\"") # Filetyp von Backupfiles
 #endif
 #ifdef PATHNAME_OS2
  LISPOBJ(backuptype_string,"\"bak\"") # Filetyp von Backupfiles
 #endif
 #ifdef PATHNAME_AMIGAOS
  LISPOBJ(backupextend_string,"\".bak\"") # Namenserweiterung von Backupfiles
 #endif
 #ifdef PATHNAME_UNIX
  LISPOBJ(backupextend_string,"\"%\"") # Namenserweiterung von Backupfiles
 #endif
 #if defined(PATHNAME_ATARI) || defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2)
  # Default-Drive (als String der Länge 1):
  LISPOBJ(default_drive,"NIL")
 #endif
 #ifdef PATHNAME_ATARI
  # Aliste Drive -> Aliste der Default-Directories
  LISPOBJ(drive_alist,"NIL")
 #endif
 #if defined(PATHNAME_UNIX) || defined(PATHNAME_AMIGAOS) || defined(PATHNAME_OS2)
  LISPOBJ(wildwild_string,"\"**\"")
  LISPOBJ(directory_absolute,"(:ABSOLUTE)") # Directory des leeren absoluten Pathname
 #endif
 #ifdef USER_HOMEDIR
  LISPOBJ(user_homedir,"#\".\"") # User-Homedir-Pathname
 #endif
 #ifdef HAVE_SHELL
 #ifdef ATARI
  LISPOBJ(shell_prompt,"\"$ \"") # Prompt der "Shell"
  LISPOBJ(shell_exit,"\"exit\"") # Exit-Kommando der "Shell"
  LISPOBJ(shell_help,"\"help\"") # Help-Kommando der "Shell"
  LISPOBJ(shell_helpstring, # Hilfestring in der "Shell"
    DEUTSCH ? "\"Mit EXIT zurück zum Lisp.\"" :
    ENGLISH ? "\"Type EXIT to return to Lisp.\"" :
    FRANCAIS ? "\"Tapez EXIT pour retourner au Lisp.\"" :
    "")
 #endif
 #ifdef UNIX
  LISPOBJ(user_shell,"\"/bin/csh\"") # User-Shell als String
  LISPOBJ(user_shell_option,"\"-c\"") # User-Shell-Option für Kommando
 #endif
 #ifdef MSDOS
  LISPOBJ(user_shell,"\"\\\\COMMAND.COM\"") # Kommandointerpreter als String
  LISPOBJ(user_shell_option,"\"/C\"") # Kommandointerpreter-Option für Kommando
 #endif
 #endif
  # Liste aller offenen File-Streams:
  LISPOBJ(open_files,"NIL")
  # Argumentliste für WRITE-TO-STRING :
  LISPOBJ(base10_radixnil,"(:BASE 10 :RADIX NIL)")
  # Defaults-Warnungs-String:
  LISPOBJ(defaults_warn_string,
    DEUTSCH ? "\"Der Wert von ~S war kein Pathname. ~:*~S wird zurückgesetzt.\"" :
    ENGLISH ? "\"The value of ~S was not a pathname. ~:*~S is being reset.\"" :
    FRANCAIS ? "\"La valeur de ~S n'était pas de type PATHNAME. ~:*~S est réinitialisé.\"" :
    "")
 #if HAS_SERNR
  # Cerror-String bei falscher Diskette:
  LISPOBJ(otherdisk_string1,
    DEUTSCH ? "\"Es geht weiter.\"" :
    ENGLISH ? "\"let's continue\"" :
    FRANCAIS ? "\"Continuons.\"" :
    "")
  LISPOBJ(otherdisk_string2,
    DEUTSCH ? "\"Legen Sie bitte die Diskette mit der Nummer ~D in Laufwerk ~A.\"" :
    ENGLISH ? "\"Put disk with number ~D in Drive ~A\"" :
    FRANCAIS ? "\"Prière d'insérer la disquette numéro ~D dans le «drive» ~A.\"" :
    "")
 #endif
  # Defaultwert für :DIRECTORY-Argument:
  #if HAS_SERNR
  LISPOBJ(directory_default,"(NIL :RELATIVE)")
  #else
  LISPOBJ(directory_default,"(:RELATIVE)")
  #endif
# zu STREAM.D:
 #ifdef PRINTER_ATARI
  # Warnungs-String:
  LISPOBJ(printer_timeout_warnung_string,
    DEUTSCH ? "\"Der Wert von ~S war keine ganze Zahl zwischen 0 und 12000. Er wird auf 1000 gesetzt.\"" :
    ENGLISH ? "\"The value of ~S was not an integer between 0 and 12000. It is being set to 1000.\"" :
    FRANCAIS ? "\"La valeur de ~S n'était pas un nombre entier compris entre 0 et 12000. Elle fut fixée à 1000.\"" :
    "")
  # Cerror-String bei streikendem Drucker:
  LISPOBJ(druckerstreik_string1,
    DEUTSCH ? "\"Neuer Versuch.\"" :
    ENGLISH ? "\"Retry\"" :
    FRANCAIS ? "\"Nouvel essai.\"" :
    "")
  LISPOBJ(druckerstreik_string2,
    DEUTSCH ? "\"Drucker nicht angeschlossen oder off-line.\"" :
    ENGLISH ? "\"Printer not connected or off-line\"" :
    FRANCAIS ? "\"Imprimante non connectée ou bien «off-line».\"" :
    "")
 #endif
# zu IO.D:
  # 4 Bitnamen:
  LISPOBJ(bitname_0,"\"CONTROL\"")
  LISPOBJ(bitname_1,"\"META\"")
  LISPOBJ(bitname_2,"\"SUPER\"")
  LISPOBJ(bitname_3,"\"HYPER\"")
 # zum Reader:
  # Standard-Readtable von Common Lisp
  LISPOBJ(standard_readtable,".")
  # Präfix für Character-Namen:
  LISPOBJ(charname_prefix,"\"Code\"")
  # interne Variablen des Readers:
  LISPOBJ(token_buff_1,".")
  LISPOBJ(token_buff_2,".")
  LISPOBJ(displaced_string,".")
 # zum Printer:
  # beim Ausgeben von Objekten verwendete Teilstrings:
  LISPOBJ(printstring_array,"\"ARRAY\"")
  LISPOBJ(printstring_fill_pointer,"\"FILL-POINTER=\"")
  LISPOBJ(printstring_address,"\"ADDRESS\"")
  LISPOBJ(printstring_system,"\"SYSTEM-POINTER\"")
  LISPOBJ(printstring_frame_pointer,"\"FRAME-POINTER\"")
  LISPOBJ(printstring_read_label,"\"READ-LABEL\"")
  LISPOBJ(printstring_unbound,"\"#<UNBOUND>\"")
  LISPOBJ(printstring_special_reference,"\"#<SPECIAL REFERENCE>\"")
  LISPOBJ(printstring_disabled_pointer,"\"#<DISABLED POINTER>\"")
  LISPOBJ(printstring_dot,"\"#<DOT>\"")
  LISPOBJ(printstring_eof,"\"#<END OF FILE>\"")
  LISPOBJ(printstring_hash_table,"\"HASH-TABLE\"")
  LISPOBJ(printstring_package,"\"PACKAGE\"")
  LISPOBJ(printstring_readtable,"\"READTABLE\"")
  LISPOBJ(pathname_slotlist,"#.(list (cons :HOST #'pathname-host) (cons :DEVICE #'pathname-device) (cons :DIRECTORY #'pathname-directory) (cons :NAME #'pathname-name) (cons :TYPE #'pathname-type) (cons :VERSION #'pathname-version))")
  LISPOBJ(byte_slotlist,"#.(list (cons :SIZE #'byte-size) (cons :POSITION #'byte-position))")
  #ifdef ALIEN
  LISPOBJ(printstring_alienfun,"\"ALIEN-FUNCTION\"")
  LISPOBJ(printstring_alien,"\"ALIEN\"")
  #endif
  LISPOBJ(printstring_closure,"\"CLOSURE\"")
  LISPOBJ(printstring_compiled_closure,"\"COMPILED-CLOSURE\"")
  LISPOBJ(printstring_subr,"\"SYSTEM-FUNCTION\"")
  LISPOBJ(printstring_fsubr,"\"SPECIAL-FORM\"")
  LISPOBJ(printstring_closed,"\"CLOSED \"")
    # Namensstring zu jedem Streamtyp, adressiert durch Streamtyp:
    LISPOBJ(printstring_strmtype_sch_file,"\"STRING-CHAR-FILE\"")
    LISPOBJ(printstring_strmtype_ch_file,"\"CHAR-FILE\"")
    LISPOBJ(printstring_strmtype_iu_file,"\"UNSIGNED-BYTE-FILE\"")
    LISPOBJ(printstring_strmtype_is_file,"\"SIGNED-BYTE-FILE\"")
    #ifdef HANDLES
    LISPOBJ(printstring_strmtype_handle,"\"FILE-HANDLE\"")
    #endif
    #ifdef KEYBOARD
    LISPOBJ(printstring_strmtype_keyboard,"\"KEYBOARD\"")
    #endif
    LISPOBJ(printstring_strmtype_terminal,"\"TERMINAL\"")
    LISPOBJ(printstring_strmtype_synonym,"\"SYNONYM\"")
    LISPOBJ(printstring_strmtype_broad,"\"BROADCAST\"")
    LISPOBJ(printstring_strmtype_concat,"\"CONCATENATED\"")
    LISPOBJ(printstring_strmtype_twoway,"\"TWO-WAY\"")
    LISPOBJ(printstring_strmtype_echo,"\"ECHO\"")
    LISPOBJ(printstring_strmtype_str_in,"\"STRING-INPUT\"")
    LISPOBJ(printstring_strmtype_str_out,"\"STRING-OUTPUT\"")
    LISPOBJ(printstring_strmtype_str_push,"\"STRING-PUSH\"")
    LISPOBJ(printstring_strmtype_pphelp,"\"PRETTY-PRINTER-HELP\"")
    LISPOBJ(printstring_strmtype_buff_in,"\"BUFFERED-INPUT\"")
    LISPOBJ(printstring_strmtype_buff_out,"\"BUFFERED-OUTPUT\"")
    #ifdef WINDOWS
    LISPOBJ(printstring_strmtype_window,"\"WINDOW\"")
    #endif
    #ifdef PRINTER
    LISPOBJ(printstring_strmtype_printer,"\"PRINTER\"")
    #endif
    #ifdef PIPES
    LISPOBJ(printstring_strmtype_pipe_in,"\"PIPE-INPUT\"")
    LISPOBJ(printstring_strmtype_pipe_out,"\"PIPE-OUTPUT\"")
    #endif
    #ifdef SOCKETS
    LISPOBJ(printstring_strmtype_socket,"\"SOCKET\"")
    #endif
  LISPOBJ(printstring_stream,"\"-STREAM\"")
# zu LISPARIT.D:
  # verschiedene konstante Zahlen:
  #ifndef WIDE
  LISPOBJ(FF_zero,"0.0F0")
  LISPOBJ(FF_one,"1.0F0")
  LISPOBJ(FF_minusone,"-1.0F0")
  #endif
  LISPOBJ(DF_zero,"0.0D0")
  LISPOBJ(DF_one,"1.0D0")
  LISPOBJ(DF_minusone,"-1.0D0")
  # Defaultlänge beim Einlesen von Long-Floats (Integer >=LF_minlen, <2^intCsize):
  LISPOBJ(LF_digits,".") # (schon initialisiert)
  # variable Long-Floats: (schon initialisiert)
  LISPOBJ(SF_pi,".")   # Wert von pi als Short-Float
  LISPOBJ(FF_pi,".")   # Wert von pi als Single-Float
  LISPOBJ(DF_pi,".")   # Wert von pi als Double-Float
  LISPOBJ(pi,".")      # Wert von pi, Long-Float der Defaultlänge
  LISPOBJ(LF_pi,".")   # Wert von pi, so genau wie bekannt
  LISPOBJ(LF_ln2,".")  # Wert von ln 2, so genau wie bekannt
  LISPOBJ(LF_ln10,".") # Wert von ln 10, so genau wie bekannt
  # Warnungs-String:
  LISPOBJ(default_float_format_warnung_string,
    DEUTSCH ? "\"In ~S wurde ein illegaler Wert vorgefunden," NLstring "~S wird auf ~S zurückgesetzt.\"" :
    ENGLISH ? "\"The variable ~S had an illegal value." NLstring "~S has been reset to ~S.\"" :
    FRANCAIS ? "\"Une valeur invalide fut trouvée dans la variable ~S," NLstring "~S fut réinitialisé à ~S.\"" :
    "")
# zu EVAL.D:
  # Toplevel-Deklarations-Environment:
  LISPOBJ(top_decl_env,"(NIL)") # Liste aus O(declaration_types) (wird nachinitialisiert)
  # Decl-Spec mit Liste der zu erkennenden Deklarations-Typen:
  LISPOBJ(declaration_types,"(DECLARATION OPTIMIZE DECLARATION)")
# zu DEBUG.D:
  LISPOBJ(newline_string,"\"" NLstring "\"")
  # Prompts:
  LISPOBJ(prompt_string,"\"> \"")
  LISPOBJ(breakprompt_string,"\". Break> \"")
  # Abschieds-String:
  LISPOBJ(bye_string,
    DEUTSCH ? "\"Bis bald!\"" :
    ENGLISH ? "\"Bye.\"" :
    FRANCAIS ? "\"À bientôt!\"" :
    "")
  # verschiedene Strings zur Beschreibung des Stacks:
  LISPOBJ(showstack_string_lisp_obj,"\"" NLstring "- \"")
  LISPOBJ(showstack_string_bindung,"\"" NLstring "  | \"")
  LISPOBJ(showstack_string_next_env,
    DEUTSCH ? "\"" NLstring "  Weiteres Environment: \"" :
    ENGLISH ? "\"" NLstring "  Next environment: \"" :
    FRANCAIS ? "\"" NLstring "  prochain environnement : \"" :
    "")
  LISPOBJ(showstack_string_APPLY_frame,
    DEUTSCH ? "\"" NLstring "APPLY-Frame für Aufruf \"" :
    ENGLISH ? "\"" NLstring "APPLY frame for call \"" :
    FRANCAIS ? "\"" NLstring "«frame» APPLY pour l'application \"" :
    "")
  LISPOBJ(showstack_string_EVAL_frame,
    DEUTSCH ? "\"" NLstring "EVAL-Frame für Form \"" :
    ENGLISH ? "\"" NLstring "EVAL frame for form \"" :
    FRANCAIS ? "\"" NLstring "«frame» EVAL pour la forme \"" :
    "")
  LISPOBJ(showstack_string_DYNBIND_frame,
    DEUTSCH ? "\"" NLstring "Variablenbindungs-Frame bindet (~ = dynamisch):\"" :
    ENGLISH ? "\"" NLstring "frame binding variables (~ = dynamically):\"" :
    FRANCAIS ? "\"" NLstring "Le «frame» de liaison de variables (~ signifiant dynamique) lie :\"" :
    "")
  LISPOBJ(showstack_string_VAR_frame,
    DEUTSCH ? "\"" NLstring "Variablenbindungs-Frame \"" :
    ENGLISH ? "\"" NLstring "frame binding variables \"" :
    FRANCAIS ? "\"" NLstring "«frame» de liaison de variables \"" :
    "")
  LISPOBJ(showstack_string_FUN_frame,
    DEUTSCH ? "\"" NLstring "Funktionsbindungs-Frame \"" :
    ENGLISH ? "\"" NLstring "frame binding functions \"" :
    FRANCAIS ? "\"" NLstring "«frame» de liaison de fonctions \"" :
    "")
  LISPOBJ(showstack_string_binds,
    DEUTSCH ? "\" bindet (~ = dynamisch):\"" :
    ENGLISH ? "\" binds (~ = dynamically):\"" :
    FRANCAIS ? "\" lie (~ signifiant dynamiquement) :\"" :
    "")
  LISPOBJ(showstack_string_zuord,"\" <--> \"")
  LISPOBJ(showstack_string_IBLOCK_frame,
    DEUTSCH ? "\"" NLstring "Block-Frame \"" :
    ENGLISH ? "\"" NLstring "block frame \"" :
    FRANCAIS ? "\"" NLstring "«frame» BLOCK \"" :
    "")
  LISPOBJ(showstack_string_NESTED_IBLOCK_frame,
    DEUTSCH ? "\"" NLstring "Block-Frame (genestet) \"" :
    ENGLISH ? "\"" NLstring "nested block frame \"" :
    FRANCAIS ? "\"" NLstring "«frame» BLOCK dépilé \"" :
    "")
  LISPOBJ(showstack_string_for1,
    DEUTSCH ? "\" für \"" :
    ENGLISH ? "\" for \"" :
    FRANCAIS ? "\" pour \"" :
    "")
  LISPOBJ(showstack_string_CBLOCK_frame,
    DEUTSCH ? "\"" NLstring "Block-Frame (compiliert) für \"" :
    ENGLISH ? "\"" NLstring "compiled block frame for \"" :
    FRANCAIS ? "\"" NLstring "«frame» BLOCK compilé pour \"" :
    "")
  LISPOBJ(showstack_string_ITAGBODY_frame,
    DEUTSCH ? "\"" NLstring "Tagbody-Frame \"" :
    ENGLISH ? "\"" NLstring "tagbody frame \"" :
    FRANCAIS ? "\"" NLstring "«frame» TAGBODY \"" :
    "")
  LISPOBJ(showstack_string_NESTED_ITAGBODY_frame,
    DEUTSCH ? "\"" NLstring "Tagbody-Frame (genestet) \"" :
    ENGLISH ? "\"" NLstring "nested tagbody frame \"" :
    FRANCAIS ? "\"" NLstring "«frame» TAGBODY dépilé \"" :
    "")
  LISPOBJ(showstack_string_for2,
    DEUTSCH ? "\" für\"" :
    ENGLISH ? "\" for\"" :
    FRANCAIS ? "\" pour\"" :
    "")
  LISPOBJ(showstack_string_zuordtag,"\" --> \"")
  LISPOBJ(showstack_string_CTAGBODY_frame,
    DEUTSCH ? "\"" NLstring "Tagbody-Frame (compiliert) für \"" :
    ENGLISH ? "\"" NLstring "compiled tagbody frame for \"" :
    FRANCAIS ? "\"" NLstring "«frame» TAGBODY compilé pour \"" :
    "")
  LISPOBJ(showstack_string_CATCH_frame,
    DEUTSCH ? "\"" NLstring "Catch-Frame für Tag \"" :
    ENGLISH ? "\"" NLstring "catch frame for tag \"" :
    FRANCAIS ? "\"" NLstring "«frame» CATCH pour l'étiquette \"" :
    "")
  LISPOBJ(showstack_string_UNWIND_PROTECT_frame,
    DEUTSCH ? "\"" NLstring "Unwind-Protect-Frame\"" :
    ENGLISH ? "\"" NLstring "unwind-protect frame\"" :
    FRANCAIS ? "\"" NLstring "«frame» UNWIND-PROTECT\"" :
    "")
  LISPOBJ(showstack_string_DRIVER_frame,
    DEUTSCH ? "\"" NLstring NLstring "Driver-Frame\"" :
    ENGLISH ? "\"" NLstring NLstring "driver frame\"" :
    FRANCAIS ? "\"" NLstring NLstring "«driver frame»\"" :
    "")
  LISPOBJ(showstack_string_ENV_frame,
    DEUTSCH ? "\"" NLstring "Environment-Bindungs-Frame\"" :
    ENGLISH ? "\"" NLstring "frame binding environments\"" :
    FRANCAIS ? "\"" NLstring "«frame» de liaison d'environnements\"" :
    "")
  LISPOBJ(showstack_string_VENV_frame,"\"" NLstring "  VAR_ENV <--> \"")
  LISPOBJ(showstack_string_FENV_frame,"\"" NLstring "  FUN_ENV <--> \"")
  LISPOBJ(showstack_string_BENV_frame,"\"" NLstring "  BLOCK_ENV <--> \"")
  LISPOBJ(showstack_string_GENV_frame,"\"" NLstring "  GO_ENV <--> \"")
  LISPOBJ(showstack_string_DENV_frame,"\"" NLstring "  DECL_ENV <--> \"")
# zu REXX.D:
 #ifdef REXX
  LISPOBJ(rexx_inmsg_list,"NIL")
  LISPOBJ(rexx_prefetch_inmsg,"NIL")
 #endif

