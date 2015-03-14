# Liste aller dem C-Programm bekannten Symbole ("Programmkonstanten")
# Bruno Haible 4.3.1993

# Der Macro LISPSYM deklariert ein LISP-Symbol.
# LISPSYM(name,printname,package)
# > name: C-Name des Symbols.
# > printname: Printname des Symbols (ein C-String).
# > package: Home-Package des Symbols, entweder lisp oder system oder keyword.
# >          Aus der Package lisp wird es automatisch exportiert.

# Expander für die Deklaration der Symbol-Tabelle:
  #define LISPSYM_A(name,printname,package)  \
    symbol_ S_##name;

# Expander für die Initialisierung der Symbol-Tabelle:
  #define LISPSYM_B(name,printname,package_)  \
    ptr->header.GCself = S(name); \
    ptr->symvalue = unbound;      \
    ptr->symfunction = unbound;   \
    ptr->proplist = NIL;          \
    ptr->pname = NIL;             \
    ptr->homepackage = NIL;       \
    ptr++;
  #define LISPSYM_C(name,printname,package)  \
    { {S(name)}, unbound, unbound, NIL, NIL, NIL, },
  #define LISPSYM_D(name,printname,package)  printname,
  #define LISPSYM_E(name,printname,package)  package##_index,

# Welcher Expander benutzt wird, muß vom Hauptfile aus eingestellt werden.


LISPSYM(nil,"NIL",lisp)
LISPSYM(t,"T",lisp)

# FSUBRs in CONTROL:
LISPSYM(eval_when,"EVAL-WHEN",lisp)
LISPSYM(quote,"QUOTE",lisp)
LISPSYM(function,"FUNCTION",lisp)
LISPSYM(setq,"SETQ",lisp)
LISPSYM(psetq,"PSETQ",lisp)
LISPSYM(progn,"PROGN",lisp)
LISPSYM(prog1,"PROG1",lisp)
LISPSYM(prog2,"PROG2",lisp)
LISPSYM(let,"LET",lisp)
LISPSYM(letstern,"LET*",lisp)
LISPSYM(compiler_let,"COMPILER-LET",lisp)
LISPSYM(progv,"PROGV",lisp)
LISPSYM(flet,"FLET",lisp)
LISPSYM(labels,"LABELS",lisp)
LISPSYM(macrolet,"MACROLET",lisp)
LISPSYM(if,"IF",lisp)
LISPSYM(when,"WHEN",lisp)
LISPSYM(unless,"UNLESS",lisp)
LISPSYM(cond,"COND",lisp)
LISPSYM(block,"BLOCK",lisp)
LISPSYM(return_from,"RETURN-FROM",lisp)
LISPSYM(tagbody,"TAGBODY",lisp)
LISPSYM(go,"GO",lisp)
LISPSYM(multiple_value_list,"MULTIPLE-VALUE-LIST",lisp)
LISPSYM(multiple_value_call,"MULTIPLE-VALUE-CALL",lisp)
LISPSYM(multiple_value_prog1,"MULTIPLE-VALUE-PROG1",lisp)
LISPSYM(multiple_value_bind,"MULTIPLE-VALUE-BIND",lisp)
LISPSYM(multiple_value_setq,"MULTIPLE-VALUE-SETQ",lisp)
LISPSYM(catch,"CATCH",lisp)
LISPSYM(unwind_protect,"UNWIND-PROTECT",lisp)
LISPSYM(throw,"THROW",lisp)
LISPSYM(declare,"DECLARE",lisp)
LISPSYM(the,"THE",lisp)
LISPSYM(and,"AND",lisp)
LISPSYM(or,"OR",lisp)

# SUBRs:
# ---------- SPVW ----------
# keine SUBRs
# ---------- EVAL ----------
LISPSYM(funtabref,"%FUNTABREF",system)
LISPSYM(subr_info,"SUBR-INFO",system)
# ---------- ARRAY ----------
LISPSYM(vector,"VECTOR",lisp)
LISPSYM(aref,"AREF",lisp)
LISPSYM(store,"STORE",system)
LISPSYM(svref,"SVREF",lisp)
LISPSYM(svstore,"SVSTORE",system)
LISPSYM(psvstore,"%SVSTORE",system)
LISPSYM(array_element_type,"ARRAY-ELEMENT-TYPE",lisp)
LISPSYM(array_rank,"ARRAY-RANK",lisp)
LISPSYM(array_dimension,"ARRAY-DIMENSION",lisp)
LISPSYM(array_dimensions,"ARRAY-DIMENSIONS",lisp)
LISPSYM(array_total_size,"ARRAY-TOTAL-SIZE",lisp)
LISPSYM(array_in_bounds_p,"ARRAY-IN-BOUNDS-P",lisp)
LISPSYM(array_row_major_index,"ARRAY-ROW-MAJOR-INDEX",lisp)
LISPSYM(adjustable_array_p,"ADJUSTABLE-ARRAY-P",lisp)
LISPSYM(bit,"BIT",lisp)
LISPSYM(sbit,"SBIT",lisp)
LISPSYM(bit_and,"BIT-AND",lisp)
LISPSYM(bit_ior,"BIT-IOR",lisp)
LISPSYM(bit_xor,"BIT-XOR",lisp)
LISPSYM(bit_eqv,"BIT-EQV",lisp)
LISPSYM(bit_nand,"BIT-NAND",lisp)
LISPSYM(bit_nor,"BIT-NOR",lisp)
LISPSYM(bit_andc1,"BIT-ANDC1",lisp)
LISPSYM(bit_andc2,"BIT-ANDC2",lisp)
LISPSYM(bit_orc1,"BIT-ORC1",lisp)
LISPSYM(bit_orc2,"BIT-ORC2",lisp)
LISPSYM(bit_not,"BIT-NOT",lisp)
LISPSYM(array_has_fill_pointer_p,"ARRAY-HAS-FILL-POINTER-P",lisp)
LISPSYM(fill_pointer,"FILL-POINTER",lisp)
LISPSYM(set_fill_pointer,"SET-FILL-POINTER",system)
LISPSYM(vector_push,"VECTOR-PUSH",lisp)
LISPSYM(vector_pop,"VECTOR-POP",lisp)
LISPSYM(vector_push_extend,"VECTOR-PUSH-EXTEND",lisp)
LISPSYM(initial_contents_aux,"INITIAL-CONTENTS-AUX",system)
LISPSYM(make_array,"MAKE-ARRAY",lisp)
LISPSYM(adjust_array,"ADJUST-ARRAY",lisp)
LISPSYM(vector_init,"VECTOR-INIT",system)
LISPSYM(vector_upd,"VECTOR-UPD",system)
LISPSYM(vector_endtest,"VECTOR-ENDTEST",system)
LISPSYM(vector_fe_init,"VECTOR-FE-INIT",system)
LISPSYM(vector_fe_upd,"VECTOR-FE-UPD",system)
LISPSYM(vector_fe_endtest,"VECTOR-FE-ENDTEST",system)
LISPSYM(vector_length,"VECTOR-LENGTH",system)
LISPSYM(vector_init_start,"VECTOR-INIT-START",system)
LISPSYM(vector_fe_init_end,"VECTOR-FE-INIT-END",system)
LISPSYM(make_bit_vector,"MAKE-BIT-VECTOR",system)
# ---------- CHARSTRG ----------
LISPSYM(standard_char_p,"STANDARD-CHAR-P",lisp)
LISPSYM(graphic_char_p,"GRAPHIC-CHAR-P",lisp)
LISPSYM(string_char_p,"STRING-CHAR-P",lisp)
LISPSYM(alpha_char_p,"ALPHA-CHAR-P",lisp)
LISPSYM(upper_case_p,"UPPER-CASE-P",lisp)
LISPSYM(lower_case_p,"LOWER-CASE-P",lisp)
LISPSYM(both_case_p,"BOTH-CASE-P",lisp)
LISPSYM(digit_char_p,"DIGIT-CHAR-P",lisp)
LISPSYM(alphanumericp,"ALPHANUMERICP",lisp)
LISPSYM(char_gleich,"CHAR=",lisp)
LISPSYM(char_ungleich,"CHAR/=",lisp)
LISPSYM(char_kleiner,"CHAR<",lisp)
LISPSYM(char_groesser,"CHAR>",lisp)
LISPSYM(char_klgleich,"CHAR<=",lisp)
LISPSYM(char_grgleich,"CHAR>=",lisp)
LISPSYM(char_equal,"CHAR-EQUAL",lisp)
LISPSYM(char_not_equal,"CHAR-NOT-EQUAL",lisp)
LISPSYM(char_lessp,"CHAR-LESSP",lisp)
LISPSYM(char_greaterp,"CHAR-GREATERP",lisp)
LISPSYM(char_not_greaterp,"CHAR-NOT-GREATERP",lisp)
LISPSYM(char_not_lessp,"CHAR-NOT-LESSP",lisp)
LISPSYM(char_code,"CHAR-CODE",lisp)
LISPSYM(char_bits,"CHAR-BITS",lisp)
LISPSYM(char_font,"CHAR-FONT",lisp)
LISPSYM(code_char,"CODE-CHAR",lisp)
LISPSYM(make_char,"MAKE-CHAR",lisp)
LISPSYM(character,"CHARACTER",lisp)
LISPSYM(char_upcase,"CHAR-UPCASE",lisp)
LISPSYM(char_downcase,"CHAR-DOWNCASE",lisp)
LISPSYM(digit_char,"DIGIT-CHAR",lisp)
LISPSYM(char_int,"CHAR-INT",lisp)
LISPSYM(int_char,"INT-CHAR",lisp)
LISPSYM(char_name,"CHAR-NAME",lisp)
LISPSYM(char_bit,"CHAR-BIT",lisp)
LISPSYM(set_char_bit,"SET-CHAR-BIT",lisp)
LISPSYM(char,"CHAR",lisp)
LISPSYM(schar,"SCHAR",lisp)
LISPSYM(store_char,"STORE-CHAR",system)
LISPSYM(store_schar,"STORE-SCHAR",system)
LISPSYM(string_gleich,"STRING=",lisp)
LISPSYM(string_ungleich,"STRING/=",lisp)
LISPSYM(string_kleiner,"STRING<",lisp)
LISPSYM(string_groesser,"STRING>",lisp)
LISPSYM(string_klgleich,"STRING<=",lisp)
LISPSYM(string_grgleich,"STRING>=",lisp)
LISPSYM(string_equal,"STRING-EQUAL",lisp)
LISPSYM(string_not_equal,"STRING-NOT-EQUAL",lisp)
LISPSYM(string_lessp,"STRING-LESSP",lisp)
LISPSYM(string_greaterp,"STRING-GREATERP",lisp)
LISPSYM(string_not_greaterp,"STRING-NOT-GREATERP",lisp)
LISPSYM(string_not_lessp,"STRING-NOT-LESSP",lisp)
LISPSYM(search_string_gleich,"SEARCH-STRING=",system)
LISPSYM(search_string_equal,"SEARCH-STRING-EQUAL",system)
LISPSYM(make_string,"MAKE-STRING",lisp)
LISPSYM(string_both_trim,"STRING-BOTH-TRIM",system)
LISPSYM(nstring_upcase,"NSTRING-UPCASE",lisp)
LISPSYM(string_upcase,"STRING-UPCASE",lisp)
LISPSYM(nstring_downcase,"NSTRING-DOWNCASE",lisp)
LISPSYM(string_downcase,"STRING-DOWNCASE",lisp)
LISPSYM(nstring_capitalize,"NSTRING-CAPITALIZE",lisp)
LISPSYM(string_capitalize,"STRING-CAPITALIZE",lisp)
LISPSYM(string,"STRING",lisp)
LISPSYM(name_char,"NAME-CHAR",lisp)
LISPSYM(substring,"SUBSTRING",lisp)
LISPSYM(string_concat,"STRING-CONCAT",lisp)
# ---------- CONTROL ----------
LISPSYM(exit,"%EXIT",system)
LISPSYM(symbol_value,"SYMBOL-VALUE",lisp)
LISPSYM(symbol_function,"SYMBOL-FUNCTION",lisp)
LISPSYM(boundp,"BOUNDP",lisp)
LISPSYM(fboundp,"FBOUNDP",lisp)
LISPSYM(special_form_p,"SPECIAL-FORM-P",lisp)
LISPSYM(set,"SET",lisp)
LISPSYM(makunbound,"MAKUNBOUND",lisp)
LISPSYM(fmakunbound,"FMAKUNBOUND",lisp)
LISPSYM(apply,"APPLY",lisp)
LISPSYM(pfuncall,"%FUNCALL",system)
LISPSYM(funcall,"FUNCALL",lisp)
LISPSYM(mapcar,"MAPCAR",lisp)
LISPSYM(maplist,"MAPLIST",lisp)
LISPSYM(mapc,"MAPC",lisp)
LISPSYM(mapl,"MAPL",lisp)
LISPSYM(mapcan,"MAPCAN",lisp)
LISPSYM(mapcon,"MAPCON",lisp)
LISPSYM(values,"VALUES",lisp)
LISPSYM(values_list,"VALUES-LIST",lisp)
LISPSYM(driver,"DRIVER",system)
LISPSYM(unwind_to_driver,"UNWIND-TO-DRIVER",system)
LISPSYM(macro_function,"MACRO-FUNCTION",lisp)
LISPSYM(macroexpand,"MACROEXPAND",lisp)
LISPSYM(macroexpand_1,"MACROEXPAND-1",lisp)
LISPSYM(proclaim,"PROCLAIM",lisp)
LISPSYM(eval,"EVAL",lisp)
LISPSYM(evalhook,"EVALHOOK",lisp)
LISPSYM(applyhook,"APPLYHOOK",lisp)
LISPSYM(constantp,"CONSTANTP",lisp)
LISPSYM(parse_body,"PARSE-BODY",system)
LISPSYM(keyword_test,"KEYWORD-TEST",system)
# ---------- DEBUG ----------
LISPSYM(read_form,"READ-FORM",system)
LISPSYM(read_eval_print,"READ-EVAL-PRINT",system)
LISPSYM(load,"LOAD",lisp)
LISPSYM(frame_up_1,"FRAME-UP-1",system)
LISPSYM(frame_up,"FRAME-UP",system)
LISPSYM(frame_down_1,"FRAME-DOWN-1",system)
LISPSYM(frame_down,"FRAME-DOWN",system)
LISPSYM(the_frame,"THE-FRAME",system)
LISPSYM(same_env_as,"SAME-ENV-AS",system)
LISPSYM(eval_at,"EVAL-AT",system)
LISPSYM(eval_frame_p,"EVAL-FRAME-P",system)
LISPSYM(driver_frame_p,"DRIVER-FRAME-P",system)
LISPSYM(redo_eval_frame,"REDO-EVAL-FRAME",system)
LISPSYM(return_from_eval_frame,"RETURN-FROM-EVAL-FRAME",system)
LISPSYM(describe_frame,"DESCRIBE-FRAME",system)
LISPSYM(show_stack,"SHOW-STACK",lisp)
LISPSYM(debug,"DEBUG",system)
LISPSYM(room,"ROOM",lisp)
LISPSYM(gc,"GC",lisp)
# ---------- HASHTABL ----------
LISPSYM(make_hash_table,"MAKE-HASH-TABLE",lisp)
LISPSYM(gethash,"GETHASH",lisp)
LISPSYM(puthash,"PUTHASH",system)
LISPSYM(remhash,"REMHASH",lisp)
LISPSYM(maphash,"MAPHASH",lisp)
LISPSYM(clrhash,"CLRHASH",lisp)
LISPSYM(hash_table_count,"HASH-TABLE-COUNT",lisp)
LISPSYM(hash_table_iterator,"HASH-TABLE-ITERATOR",system)
LISPSYM(hash_table_iterate,"HASH-TABLE-ITERATE",system)
LISPSYM(sxhash,"SXHASH",lisp)
# ---------- IO ----------
LISPSYM(copy_readtable,"COPY-READTABLE",lisp)
LISPSYM(set_syntax_from_char,"SET-SYNTAX-FROM-CHAR",lisp)
LISPSYM(set_macro_character,"SET-MACRO-CHARACTER",lisp)
LISPSYM(get_macro_character,"GET-MACRO-CHARACTER",lisp)
LISPSYM(make_dispatch_macro_character,"MAKE-DISPATCH-MACRO-CHARACTER",lisp)
LISPSYM(set_dispatch_macro_character,"SET-DISPATCH-MACRO-CHARACTER",lisp)
LISPSYM(get_dispatch_macro_character,"GET-DISPATCH-MACRO-CHARACTER",lisp)
LISPSYM(lpar_reader,"LPAR-READER",system)
LISPSYM(rpar_reader,"RPAR-READER",system)
LISPSYM(quote_reader,"QUOTE-READER",system)
LISPSYM(function_reader,"FUNCTION-READER",system)
LISPSYM(string_reader,"STRING-READER",system)
LISPSYM(line_comment_reader,"LINE-COMMENT-READER",system)
LISPSYM(comment_reader,"COMMENT-READER",system)
LISPSYM(char_reader,"CHAR-READER",system)
LISPSYM(binary_reader,"BINARY-READER",system)
LISPSYM(octal_reader,"OCTAL-READER",system)
LISPSYM(hexadecimal_reader,"HEXADECIMAL-READER",system)
LISPSYM(radix_reader,"RADIX-READER",system)
LISPSYM(complex_reader,"COMPLEX-READER",system)
LISPSYM(uninterned_reader,"UNINTERNED-READER",system)
LISPSYM(bit_vector_reader,"BIT-VECTOR-READER",system)
LISPSYM(vector_reader,"VECTOR-READER",system)
LISPSYM(array_reader,"ARRAY-READER",system)
LISPSYM(read_eval_reader,"READ-EVAL-READER",system)
LISPSYM(load_eval_reader,"LOAD-EVAL-READER",system)
LISPSYM(label_definition_reader,"LABEL-DEFINIION-READER",system)
LISPSYM(label_reference_reader,"LABEL-REFERENCE-READER",system)
LISPSYM(not_readable_reader,"NOT-READABLE-READER",system)
LISPSYM(syntax_error_reader,"SYNTAX-ERROR-READER",system)
LISPSYM(feature_reader,"FEATURE-READER",system)
LISPSYM(not_feature_reader,"NOT-FEATURE-READER",system)
LISPSYM(structure_reader,"STRUCTURE-READER",system)
LISPSYM(closure_reader,"CLOSURE-READER",system)
LISPSYM(pathname_reader,"PATHNAME-READER",system)
LISPSYM(read,"READ",lisp)
LISPSYM(read_preserving_whitespace,"READ-PRESERVING-WHITESPACE",lisp)
LISPSYM(read_delimited_list,"READ-DELIMITED-LIST",lisp)
LISPSYM(read_line,"READ-LINE",lisp)
LISPSYM(read_char,"READ-CHAR",lisp)
LISPSYM(unread_char,"UNREAD-CHAR",lisp)
LISPSYM(peek_char,"PEEK-CHAR",lisp)
LISPSYM(listen,"LISTEN",lisp)
LISPSYM(read_char_no_hang,"READ-CHAR-NO-HANG",lisp)
LISPSYM(clear_input,"CLEAR-INPUT",lisp)
LISPSYM(read_from_string,"READ-FROM-STRING",lisp)
LISPSYM(parse_integer,"PARSE-INTEGER",lisp)
LISPSYM(write,"WRITE",lisp)
LISPSYM(prin1,"PRIN1",lisp)
LISPSYM(print,"PRINT",lisp)
LISPSYM(pprint,"PPRINT",lisp)
LISPSYM(princ,"PRINC",lisp)
LISPSYM(write_to_string,"WRITE-TO-STRING",lisp)
LISPSYM(prin1_to_string,"PRIN1-TO-STRING",lisp)
LISPSYM(princ_to_string,"PRINC-TO-STRING",lisp)
LISPSYM(write_char,"WRITE-CHAR",lisp)
LISPSYM(write_string,"WRITE-STRING",lisp)
LISPSYM(write_line,"WRITE-LINE",lisp)
LISPSYM(terpri,"TERPRI",lisp)
LISPSYM(fresh_line,"FRESH-LINE",lisp)
LISPSYM(finish_output,"FINISH-OUTPUT",lisp)
LISPSYM(force_output,"FORCE-OUTPUT",lisp)
LISPSYM(clear_output,"CLEAR-OUTPUT",lisp)
LISPSYM(line_position,"LINE-POSITION",system)
# ---------- LIST ----------
LISPSYM(car,"CAR",lisp)
LISPSYM(cdr,"CDR",lisp)
LISPSYM(caar,"CAAR",lisp)
LISPSYM(cadr,"CADR",lisp)
LISPSYM(cdar,"CDAR",lisp)
LISPSYM(cddr,"CDDR",lisp)
LISPSYM(caaar,"CAAAR",lisp)
LISPSYM(caadr,"CAADR",lisp)
LISPSYM(cadar,"CADAR",lisp)
LISPSYM(caddr,"CADDR",lisp)
LISPSYM(cdaar,"CDAAR",lisp)
LISPSYM(cdadr,"CDADR",lisp)
LISPSYM(cddar,"CDDAR",lisp)
LISPSYM(cdddr,"CDDDR",lisp)
LISPSYM(caaaar,"CAAAAR",lisp)
LISPSYM(caaadr,"CAAADR",lisp)
LISPSYM(caadar,"CAADAR",lisp)
LISPSYM(caaddr,"CAADDR",lisp)
LISPSYM(cadaar,"CADAAR",lisp)
LISPSYM(cadadr,"CADADR",lisp)
LISPSYM(caddar,"CADDAR",lisp)
LISPSYM(cadddr,"CADDDR",lisp)
LISPSYM(cdaaar,"CDAAAR",lisp)
LISPSYM(cdaadr,"CDAADR",lisp)
LISPSYM(cdadar,"CDADAR",lisp)
LISPSYM(cdaddr,"CDADDR",lisp)
LISPSYM(cddaar,"CDDAAR",lisp)
LISPSYM(cddadr,"CDDADR",lisp)
LISPSYM(cdddar,"CDDDAR",lisp)
LISPSYM(cddddr,"CDDDDR",lisp)
LISPSYM(cons,"CONS",lisp)
LISPSYM(tree_equal,"TREE-EQUAL",lisp)
LISPSYM(endp,"ENDP",lisp)
LISPSYM(list_length,"LIST-LENGTH",lisp)
LISPSYM(nth,"NTH",lisp)
LISPSYM(first,"FIRST",lisp)
LISPSYM(second,"SECOND",lisp)
LISPSYM(third,"THIRD",lisp)
LISPSYM(fourth,"FOURTH",lisp)
LISPSYM(fifth,"FIFTH",lisp)
LISPSYM(sixth,"SIXTH",lisp)
LISPSYM(seventh,"SEVENTH",lisp)
LISPSYM(eighth,"EIGHTH",lisp)
LISPSYM(ninth,"NINTH",lisp)
LISPSYM(tenth,"TENTH",lisp)
LISPSYM(rest,"REST",lisp)
LISPSYM(nthcdr,"NTHCDR",lisp)
LISPSYM(last,"LAST",lisp)
LISPSYM(list,"LIST",lisp)
LISPSYM(liststern,"LIST*",lisp)
LISPSYM(make_list,"MAKE-LIST",lisp)
LISPSYM(append,"APPEND",lisp)
LISPSYM(copy_list,"COPY-LIST",lisp)
LISPSYM(copy_alist,"COPY-ALIST",lisp)
LISPSYM(copy_tree,"COPY-TREE",lisp)
LISPSYM(revappend,"REVAPPEND",lisp)
LISPSYM(nconc,"NCONC",lisp)
LISPSYM(nreconc,"NRECONC",lisp)
LISPSYM(list_nreverse,"LIST-NREVERSE",system)
LISPSYM(butlast,"BUTLAST",lisp)
LISPSYM(nbutlast,"NBUTLAST",lisp)
LISPSYM(ldiff,"LDIFF",lisp)
LISPSYM(rplaca,"RPLACA",lisp)
LISPSYM(prplaca,"%RPLACA",system)
LISPSYM(rplacd,"RPLACD",lisp)
LISPSYM(prplacd,"%RPLACD",system)
LISPSYM(subst,"SUBST",lisp)
LISPSYM(subst_if,"SUBST-IF",lisp)
LISPSYM(subst_if_not,"SUBST-IF-NOT",lisp)
LISPSYM(nsubst,"NSUBST",lisp)
LISPSYM(nsubst_if,"NSUBST-IF",lisp)
LISPSYM(nsubst_if_not,"NSUBST-IF-NOT",lisp)
LISPSYM(sublis,"SUBLIS",lisp)
LISPSYM(nsublis,"NSUBLIS",lisp)
LISPSYM(member,"MEMBER",lisp)
LISPSYM(member_if,"MEMBER-IF",lisp)
LISPSYM(member_if_not,"MEMBER-IF-NOT",lisp)
LISPSYM(tailp,"TAILP",lisp)
LISPSYM(adjoin,"ADJOIN",lisp)
LISPSYM(acons,"ACONS",lisp)
LISPSYM(pairlis,"PAIRLIS",lisp)
LISPSYM(assoc,"ASSOC",lisp)
LISPSYM(assoc_if,"ASSOC-IF",lisp)
LISPSYM(assoc_if_not,"ASSOC-IF-NOT",lisp)
LISPSYM(rassoc,"RASSOC",lisp)
LISPSYM(rassoc_if,"RASSOC-IF",lisp)
LISPSYM(rassoc_if_not,"RASSOC-IF-NOT",lisp)
LISPSYM(list_upd,"LIST-UPD",system)
LISPSYM(list_endtest,"LIST-ENDTEST",system)
LISPSYM(list_fe_init,"LIST-FE-INIT",system)
LISPSYM(list_access,"LIST-ACCESS",system)
LISPSYM(list_access_set,"LIST-ACCESS-SET",system)
LISPSYM(list_llength,"LIST-LLENGTH",system)
LISPSYM(list_elt,"LIST-ELT",system)
LISPSYM(list_set_elt,"LIST-SET-ELT",system)
LISPSYM(list_init_start,"LIST-INIT-START",system)
LISPSYM(list_fe_init_end,"LIST-FE-INIT-END",system)
# ---------- MISC ----------
LISPSYM(lisp_implementation_type,"LISP-IMPLEMENTATION-TYPE",lisp)
LISPSYM(lisp_implementation_version,"LISP-IMPLEMENTATION-VERSION",lisp)
LISPSYM(version,"VERSION",system)
#ifdef MACHINE_KNOWN
LISPSYM(machinetype,"MACHINE-TYPE",lisp)
LISPSYM(machine_version,"MACHINE-VERSION",lisp)
LISPSYM(machine_instance,"MACHINE-INSTANCE",lisp)
#endif
#ifdef HAVE_ENVIRONMENT
LISPSYM(get_env,"GETENV",system)
#endif
LISPSYM(software_type,"SOFTWARE-TYPE",lisp)
LISPSYM(software_version,"SOFTWARE-VERSION",lisp)
LISPSYM(language,"*LANGUAGE*",lisp)
LISPSYM(identity,"IDENTITY",lisp)
LISPSYM(address_of,"ADDRESS-OF",system)
LISPSYM(get_universal_time,"GET-UNIVERSAL-TIME",lisp)
#ifdef TIME_UNIX
LISPSYM(default_time_zone,"DEFAULT-TIME-ZONE",system)
#endif
LISPSYM(get_internal_run_time,"GET-INTERNAL-RUN-TIME",lisp)
LISPSYM(get_internal_real_time,"GET-INTERNAL-REAL-TIME",lisp)
LISPSYM(sleep,"%SLEEP",system)
LISPSYM(time,"%%TIME",system)
LISPSYM(error,"ERROR",lisp)
# ---------- PACKAGE ----------
LISPSYM(use_package_aux,"USE-PACKAGE-AUX",system)
LISPSYM(make_symbol,"MAKE-SYMBOL",lisp)
LISPSYM(find_package,"FIND-PACKAGE",lisp)
LISPSYM(package_name,"PACKAGE-NAME",lisp)
LISPSYM(package_nicknames,"PACKAGE-NICKNAMES",lisp)
LISPSYM(rename_package,"RENAME-PACKAGE",lisp)
LISPSYM(package_use_list,"PACKAGE-USE-LIST",lisp)
LISPSYM(package_used_by_list,"PACKAGE-USED-BY-LIST",lisp)
LISPSYM(package_shadowing_symbols,"PACKAGE-SHADOWING-SYMBOLS",lisp)
LISPSYM(list_all_packages,"LIST-ALL-PACKAGES",lisp)
LISPSYM(intern,"INTERN",lisp)
LISPSYM(find_symbol,"FIND-SYMBOL",lisp)
LISPSYM(unintern,"UNINTERN",lisp)
LISPSYM(export,"EXPORT",lisp)
LISPSYM(unexport,"UNEXPORT",lisp)
LISPSYM(import,"IMPORT",lisp)
LISPSYM(shadowing_import,"SHADOWING-IMPORT",lisp)
LISPSYM(shadow,"SHADOW",lisp)
LISPSYM(use_package,"USE-PACKAGE",lisp)
LISPSYM(unuse_package,"UNUSE-PACKAGE",lisp)
LISPSYM(make_package,"MAKE-PACKAGE",lisp)
LISPSYM(in_package,"IN-PACKAGE",lisp)
LISPSYM(find_all_symbols,"FIND-ALL-SYMBOLS",lisp)
LISPSYM(map_symbols,"MAP-SYMBOLS",system)
LISPSYM(map_external_symbols,"MAP-EXTERNAL-SYMBOLS",system)
LISPSYM(map_all_symbols,"MAP-ALL-SYMBOLS",system)
# ---------- PATHNAME ----------
LISPSYM(parse_namestring,"PARSE-NAMESTRING",lisp)
LISPSYM(pathname,"PATHNAME",lisp)
LISPSYM(pathnamehost,"PATHNAME-HOST",lisp)
LISPSYM(pathnamedevice,"PATHNAME-DEVICE",lisp)
LISPSYM(pathnamedirectory,"PATHNAME-DIRECTORY",lisp)
LISPSYM(pathnamename,"PATHNAME-NAME",lisp)
LISPSYM(pathnametype,"PATHNAME-TYPE",lisp)
LISPSYM(pathnameversion,"PATHNAME-VERSION",lisp)
LISPSYM(file_namestring,"FILE-NAMESTRING",lisp)
LISPSYM(directory_namestring,"DIRECTORY-NAMESTRING",lisp)
LISPSYM(host_namestring,"HOST-NAMESTRING",lisp)
LISPSYM(merge_pathnames,"MERGE-PATHNAMES",lisp)
LISPSYM(enough_namestring,"ENOUGH-NAMESTRING",lisp)
LISPSYM(make_pathname,"MAKE-PATHNAME",lisp)
#ifdef USER_HOMEDIR
LISPSYM(user_homedir_pathname,"USER-HOMEDIR-PATHNAME",lisp)
#endif
LISPSYM(namestring,"NAMESTRING",lisp)
LISPSYM(truename,"TRUENAME",lisp)
LISPSYM(probe_file,"PROBE-FILE",lisp)
LISPSYM(delete_file,"DELETE-FILE",lisp)
LISPSYM(rename_file,"RENAME-FILE",lisp)
LISPSYM(open,"OPEN",lisp)
LISPSYM(directory,"DIRECTORY",lisp)
LISPSYM(cd,"CD",lisp)
LISPSYM(make_dir,"MAKE-DIR",lisp)
LISPSYM(delete_dir,"DELETE-DIR",lisp)
LISPSYM(file_write_date,"FILE-WRITE-DATE",lisp)
LISPSYM(file_author,"FILE-AUTHOR",lisp)
#if defined(ATARI) || defined(UNIX) || defined(MSDOS) || defined(AMIGAOS)
LISPSYM(execute,"EXECUTE",lisp)
#endif
#ifdef HAVE_SHELL
LISPSYM(shell,"SHELL",lisp)
#endif
LISPSYM(savemem,"SAVEMEM",lisp)
# ---------- PREDTYPE ----------
LISPSYM(eq,"EQ",lisp)
LISPSYM(eql,"EQL",lisp)
LISPSYM(equal,"EQUAL",lisp)
LISPSYM(equalp,"EQUALP",lisp)
LISPSYM(consp,"CONSP",lisp)
LISPSYM(atom,"ATOM",lisp)
LISPSYM(symbolp,"SYMBOLP",lisp)
LISPSYM(stringp,"STRINGP",lisp)
LISPSYM(numberp,"NUMBERP",lisp)
LISPSYM(compiled_function_p,"COMPILED-FUNCTION-P",lisp)
LISPSYM(null,"NULL",lisp)
LISPSYM(not,"NOT",lisp)
LISPSYM(closurep,"CLOSUREP",system)
LISPSYM(listp,"LISTP",lisp)
LISPSYM(integerp,"INTEGERP",lisp)
LISPSYM(fixnump,"FIXNUMP",system)
LISPSYM(rationalp,"RATIONALP",lisp)
LISPSYM(floatp,"FLOATP",lisp)
LISPSYM(realp,"REALP",lisp)
LISPSYM(short_float_p,"SHORT-FLOAT-P",system)
LISPSYM(single_float_p,"SINGLE-FLOAT-P",system)
LISPSYM(double_float_p,"DOUBLE-FLOAT-P",system)
LISPSYM(long_float_p,"LONG-FLOAT-P",system)
LISPSYM(complexp,"COMPLEXP",lisp)
LISPSYM(streamp,"STREAMP",lisp)
LISPSYM(random_state_p,"RANDOM-STATE-P",lisp)
LISPSYM(readtablep,"READTABLEP",lisp)
LISPSYM(hash_table_p,"HASH-TABLE-P",lisp)
LISPSYM(pathnamep,"PATHNAMEP",lisp)
LISPSYM(characterp,"CHARACTERP",lisp)
LISPSYM(functionp,"FUNCTIONP",lisp)
LISPSYM(packagep,"PACKAGEP",lisp)
LISPSYM(arrayp,"ARRAYP",lisp)
LISPSYM(simple_array_p,"SIMPLE-ARRAY-P",system)
LISPSYM(bit_vector_p,"BIT-VECTOR-P",lisp)
LISPSYM(vectorp,"VECTORP",lisp)
LISPSYM(simple_vector_p,"SIMPLE-VECTOR-P",lisp)
LISPSYM(simple_string_p,"SIMPLE-STRING-P",lisp)
LISPSYM(simple_bit_vector_p,"SIMPLE-BIT-VECTOR-P",lisp)
LISPSYM(commonp,"COMMONP",lisp)
LISPSYM(type_of,"TYPE-OF",lisp)
LISPSYM(coerce,"COERCE",lisp)
# ---------- RECORD ----------
LISPSYM(record_ref,"%RECORD-REF",system)
LISPSYM(record_store,"%RECORD-STORE",system)
LISPSYM(record_length,"%RECORD-LENGTH",system)
LISPSYM(structure_ref,"%STRUCTURE-REF",system)
LISPSYM(structure_store,"%STRUCTURE-STORE",system)
LISPSYM(make_structure,"%MAKE-STRUCTURE",system)
LISPSYM(copy_structure,"%COPY-STRUCTURE",system)
LISPSYM(structure_type_p,"%STRUCTURE-TYPE-P",system)
LISPSYM(closure_name,"CLOSURE-NAME",system)
LISPSYM(closure_codevec,"CLOSURE-CODEVEC",system)
LISPSYM(closure_consts,"CLOSURE-CONSTS",system)
LISPSYM(make_code_vector,"MAKE-CODE-VECTOR",system)
LISPSYM(make_closure,"%MAKE-CLOSURE",system)
LISPSYM(make_load_time_eval,"MAKE-LOAD-TIME-EVAL",system)
# ---------- SEQUENCE ----------
LISPSYM(sequencep,"SEQUENCEP",system)
LISPSYM(defseq,"%DEFSEQ",system)
LISPSYM(elt,"ELT",lisp)
LISPSYM(setelt,"%SETELT",system)
LISPSYM(subseq,"SUBSEQ",lisp)
LISPSYM(copy_seq,"COPY-SEQ",lisp)
LISPSYM(length,"LENGTH",lisp)
LISPSYM(reverse,"REVERSE",lisp)
LISPSYM(nreverse,"NREVERSE",lisp)
LISPSYM(make_sequence,"MAKE-SEQUENCE",lisp)
LISPSYM(concatenate,"CONCATENATE",lisp)
LISPSYM(map,"MAP",lisp)
LISPSYM(some,"SOME",lisp)
LISPSYM(every,"EVERY",lisp)
LISPSYM(notany,"NOTANY",lisp)
LISPSYM(notevery,"NOTEVERY",lisp)
LISPSYM(reduce,"REDUCE",lisp)
LISPSYM(fill,"FILL",lisp)
LISPSYM(replace,"REPLACE",lisp)
LISPSYM(remove,"REMOVE",lisp)
LISPSYM(remove_if,"REMOVE-IF",lisp)
LISPSYM(remove_if_not,"REMOVE-IF-NOT",lisp)
LISPSYM(delete,"DELETE",lisp)
LISPSYM(delete_if,"DELETE-IF",lisp)
LISPSYM(delete_if_not,"DELETE-IF-NOT",lisp)
LISPSYM(remove_duplicates,"REMOVE-DUPLICATES",lisp)
LISPSYM(delete_duplicates,"DELETE-DUPLICATES",lisp)
LISPSYM(substitute,"SUBSTITUTE",lisp)
LISPSYM(substitute_if,"SUBSTITUTE-IF",lisp)
LISPSYM(substitute_if_not,"SUBSTITUTE-IF-NOT",lisp)
LISPSYM(nsubstitute,"NSUBSTITUTE",lisp)
LISPSYM(nsubstitute_if,"NSUBSTITUTE-IF",lisp)
LISPSYM(nsubstitute_if_not,"NSUBSTITUTE-IF-NOT",lisp)
LISPSYM(find,"FIND",lisp)
LISPSYM(find_if,"FIND-IF",lisp)
LISPSYM(find_if_not,"FIND-IF-NOT",lisp)
LISPSYM(position,"POSITION",lisp)
LISPSYM(position_if,"POSITION-IF",lisp)
LISPSYM(position_if_not,"POSITION-IF-NOT",lisp)
LISPSYM(count,"COUNT",lisp)
LISPSYM(count_if,"COUNT-IF",lisp)
LISPSYM(count_if_not,"COUNT-IF-NOT",lisp)
LISPSYM(mismatch,"MISMATCH",lisp)
LISPSYM(search,"SEARCH",lisp)
LISPSYM(sort,"SORT",lisp)
LISPSYM(stable_sort,"STABLE-SORT",lisp)
LISPSYM(merge,"MERGE",lisp)
# ---------- STREAM ----------
#ifdef UNIX
LISPSYM(terminal_raw,"TERMINAL-RAW",system)
#endif
#ifdef WINDOWS
LISPSYM(make_window,"MAKE-WINDOW",system)
LISPSYM(window_size,"WINDOW-SIZE",system)
LISPSYM(window_cursor_position,"WINDOW-CURSOR-POSITION",system)
LISPSYM(set_window_cursor_position,"SET-WINDOW-CURSOR-POSITION",system)
LISPSYM(clear_window,"CLEAR-WINDOW",system)
LISPSYM(clear_window_to_eot,"CLEAR-WINDOW-TO-EOT",system)
LISPSYM(clear_window_to_eol,"CLEAR-WINDOW-TO-EOL",system)
LISPSYM(delete_window_line,"DELETE-WINDOW-LINE",system)
LISPSYM(insert_window_line,"INSERT-WINDOW-LINE",system)
LISPSYM(highlight_on,"HIGHLIGHT-ON",system)
LISPSYM(highlight_off,"HIGHLIGHT-OFF",system)
LISPSYM(window_cursor_on,"WINDOW-CURSOR-ON",system)
LISPSYM(window_cursor_off,"WINDOW-CURSOR-OFF",system)
#endif
LISPSYM(make_synonym_stream,"MAKE-SYNONYM-STREAM",lisp)
LISPSYM(make_broadcast_stream,"MAKE-BROADCAST-STREAM",lisp)
LISPSYM(make_concatenated_stream,"MAKE-CONCATENATED-STREAM",lisp)
LISPSYM(make_two_way_stream,"MAKE-TWO-WAY-STREAM",lisp)
LISPSYM(make_echo_stream,"MAKE-ECHO-STREAM",lisp)
LISPSYM(make_string_input_stream,"MAKE-STRING-INPUT-STREAM",lisp)
LISPSYM(string_input_stream_index,"STRING-INPUT-STREAM-INDEX",system)
LISPSYM(make_string_output_stream,"MAKE-STRING-OUTPUT-STREAM",lisp)
LISPSYM(get_output_stream_string,"GET-OUTPUT-STREAM-STRING",lisp)
LISPSYM(make_string_push_stream,"MAKE-STRING-PUSH-STREAM",system)
LISPSYM(make_buffered_input_stream,"MAKE-BUFFERED-INPUT-STREAM",lisp)
LISPSYM(buffered_input_stream_index,"BUFFERED-INPUT-STREAM-INDEX",system)
LISPSYM(make_buffered_output_stream,"MAKE-BUFFERED-OUTPUT-STREAM",lisp)
#ifdef PRINTER_AMIGAOS
LISPSYM(make_printer_stream,"MAKE-PRINTER-STREAM",system)
#endif
#ifdef PIPES
LISPSYM(make_pipe_input_stream,"MAKE-PIPE-INPUT-STREAM",lisp)
LISPSYM(make_pipe_output_stream,"MAKE-PIPE-OUTPUT-STREAM",lisp)
#endif
#ifdef SOCKETS
LISPSYM(make_socket_stream,"MAKE-SOCKET-STREAM",system)
LISPSYM(read_n_bytes,"READ-N-BYTES",system)
LISPSYM(write_n_bytes,"WRITE-N-BYTES",system)
#endif
LISPSYM(input_stream_p,"INPUT-STREAM-P",lisp)
LISPSYM(output_stream_p,"OUTPUT-STREAM-P",lisp)
LISPSYM(stream_element_type,"STREAM-ELEMENT-TYPE",lisp)
LISPSYM(interactive_stream_p,"INTERACTIVE-STREAM-P",lisp)
LISPSYM(close,"CLOSE",lisp)
LISPSYM(read_byte,"READ-BYTE",lisp)
LISPSYM(write_byte,"WRITE-BYTE",lisp)
LISPSYM(file_position,"FILE-POSITION",lisp)
LISPSYM(file_length,"FILE-LENGTH",lisp)
# ---------- SYMBOL ----------
LISPSYM(putd,"%PUTD",system)
LISPSYM(proclaim_constant,"%PROCLAIM-CONSTANT",system)
LISPSYM(get,"GET",lisp)
LISPSYM(getf,"GETF",lisp)
LISPSYM(get_properties,"GET-PROPERTIES",lisp)
LISPSYM(putplist,"%PUTPLIST",system)
LISPSYM(put,"%PUT",system)
LISPSYM(remprop,"REMPROP",lisp)
LISPSYM(symbol_package,"SYMBOL-PACKAGE",lisp)
LISPSYM(symbol_plist,"SYMBOL-PLIST",lisp)
LISPSYM(symbol_name,"SYMBOL-NAME",lisp)
LISPSYM(keywordp,"KEYWORDP",lisp)
LISPSYM(special_variable_p,"SPECIAL-VARIABLE-P",system)
LISPSYM(gensym,"GENSYM",lisp)
# ---------- LISPARIT ----------
LISPSYM(decimal_string,"DECIMAL-STRING",system)
LISPSYM(zerop,"ZEROP",lisp)
LISPSYM(plusp,"PLUSP",lisp)
LISPSYM(minusp,"MINUSP",lisp)
LISPSYM(oddp,"ODDP",lisp)
LISPSYM(evenp,"EVENP",lisp)
LISPSYM(gleich,"=",lisp)
LISPSYM(ungleich,"/=",lisp)
LISPSYM(kleiner,"<",lisp)
LISPSYM(groesser,">",lisp)
LISPSYM(klgleich,"<=",lisp)
LISPSYM(grgleich,">=",lisp)
LISPSYM(max,"MAX",lisp)
LISPSYM(min,"MIN",lisp)
LISPSYM(plus,"+",lisp)
LISPSYM(minus,"-",lisp)
LISPSYM(mal,"*",lisp)
LISPSYM(durch,"/",lisp)
LISPSYM(einsplus,"1+",lisp)
LISPSYM(einsminus,"1-",lisp)
LISPSYM(conjugate,"CONJUGATE",lisp)
LISPSYM(gcd,"GCD",lisp)
LISPSYM(xgcd,"XGCD",lisp)
LISPSYM(lcm,"LCM",lisp)
LISPSYM(exp,"EXP",lisp)
LISPSYM(expt,"EXPT",lisp)
LISPSYM(log,"LOG",lisp)
LISPSYM(sqrt,"SQRT",lisp)
LISPSYM(isqrt,"ISQRT",lisp)
LISPSYM(abs,"ABS",lisp)
LISPSYM(phase,"PHASE",lisp)
LISPSYM(signum,"SIGNUM",lisp)
LISPSYM(sin,"SIN",lisp)
LISPSYM(cos,"COS",lisp)
LISPSYM(tan,"TAN",lisp)
LISPSYM(cis,"CIS",lisp)
LISPSYM(asin,"ASIN",lisp)
LISPSYM(acos,"ACOS",lisp)
LISPSYM(atan,"ATAN",lisp)
LISPSYM(sinh,"SINH",lisp)
LISPSYM(cosh,"COSH",lisp)
LISPSYM(tanh,"TANH",lisp)
LISPSYM(asinh,"ASINH",lisp)
LISPSYM(acosh,"ACOSH",lisp)
LISPSYM(atanh,"ATANH",lisp)
LISPSYM(float,"FLOAT",lisp)
LISPSYM(rational,"RATIONAL",lisp)
LISPSYM(rationalize,"RATIONALIZE",lisp)
LISPSYM(numerator,"NUMERATOR",lisp)
LISPSYM(denominator,"DENOMINATOR",lisp)
LISPSYM(floor,"FLOOR",lisp)
LISPSYM(ceiling,"CEILING",lisp)
LISPSYM(truncate,"TRUNCATE",lisp)
LISPSYM(round,"ROUND",lisp)
LISPSYM(mod,"MOD",lisp)
LISPSYM(rem,"REM",lisp)
LISPSYM(ffloor,"FFLOOR",lisp)
LISPSYM(fceiling,"FCEILING",lisp)
LISPSYM(ftruncate,"FTRUNCATE",lisp)
LISPSYM(fround,"FROUND",lisp)
LISPSYM(decode_float,"DECODE-FLOAT",lisp)
LISPSYM(scale_float,"SCALE-FLOAT",lisp)
LISPSYM(float_radix,"FLOAT-RADIX",lisp)
LISPSYM(float_sign,"FLOAT-SIGN",lisp)
LISPSYM(float_digits,"FLOAT-DIGITS",lisp)
LISPSYM(float_precision,"FLOAT-PRECISION",lisp)
LISPSYM(integer_decode_float,"INTEGER-DECODE-FLOAT",lisp)
LISPSYM(complex,"COMPLEX",lisp)
LISPSYM(realpart,"REALPART",lisp)
LISPSYM(imagpart,"IMAGPART",lisp)
LISPSYM(logior,"LOGIOR",lisp)
LISPSYM(logxor,"LOGXOR",lisp)
LISPSYM(logand,"LOGAND",lisp)
LISPSYM(logeqv,"LOGEQV",lisp)
LISPSYM(lognand,"LOGNAND",lisp)
LISPSYM(lognor,"LOGNOR",lisp)
LISPSYM(logandc1,"LOGANDC1",lisp)
LISPSYM(logandc2,"LOGANDC2",lisp)
LISPSYM(logorc1,"LOGORC1",lisp)
LISPSYM(logorc2,"LOGORC2",lisp)
LISPSYM(boole,"BOOLE",lisp)
LISPSYM(lognot,"LOGNOT",lisp)
LISPSYM(logtest,"LOGTEST",lisp)
LISPSYM(logbitp,"LOGBITP",lisp)
LISPSYM(ash,"ASH",lisp)
LISPSYM(logcount,"LOGCOUNT",lisp)
LISPSYM(integer_length,"INTEGER-LENGTH",lisp)
LISPSYM(byte,"BYTE",lisp)
LISPSYM(bytesize,"BYTE-SIZE",lisp)
LISPSYM(byteposition,"BYTE-POSITION",lisp)
LISPSYM(ldb,"LDB",lisp)
LISPSYM(ldb_test,"LDB-TEST",lisp)
LISPSYM(mask_field,"MASK-FIELD",lisp)
LISPSYM(dpb,"DPB",lisp)
LISPSYM(deposit_field,"DEPOSIT-FIELD",lisp)
LISPSYM(random,"RANDOM",lisp)
LISPSYM(make_random_state,"MAKE-RANDOM-STATE",lisp)
LISPSYM(fakultaet,"!",lisp)
LISPSYM(exquo,"EXQUO",lisp)
LISPSYM(long_float_digits,"LONG-FLOAT-DIGITS",lisp)
LISPSYM(set_long_float_digits,"%SET-LONG-FLOAT-DIGITS",system)
LISPSYM(log2,"LOG2",system)
LISPSYM(log10,"LOG10",system)
# ---------- REXX ----------
#ifdef REXX
LISPSYM(rexx_put,"%REXX-PUT",system)
LISPSYM(rexx_wait_input,"%REXX-WAIT-INPUT",system)
LISPSYM(rexx_get,"%REXX-GET",system)
LISPSYM(rexx_reply,"%REXX-REPLY",system)
#endif

# Keywords:
LISPSYM(Kallow_other_keys,"ALLOW-OTHER-KEYS",keyword)
LISPSYM(Kadjustable,"ADJUSTABLE",keyword)
LISPSYM(Kelement_type,"ELEMENT-TYPE",keyword)
LISPSYM(Kinitial_element,"INITIAL-ELEMENT",keyword)
LISPSYM(Kinitial_contents,"INITIAL-CONTENTS",keyword)
LISPSYM(Kfill_pointer,"FILL-POINTER",keyword)
LISPSYM(Kdisplaced_to,"DISPLACED-TO",keyword)
LISPSYM(Kdisplaced_index_offset,"DISPLACED-INDEX-OFFSET",keyword)
LISPSYM(Kstart1,"START1",keyword)
LISPSYM(Kend1,"END1",keyword)
LISPSYM(Kstart2,"START2",keyword)
LISPSYM(Kend2,"END2",keyword)
LISPSYM(Kstart,"START",keyword)
LISPSYM(Kend,"END",keyword)
LISPSYM(Kpreserve_whitespace,"PRESERVE-WHITESPACE",keyword)
LISPSYM(Kradix,"RADIX",keyword)
LISPSYM(Kjunk_allowed,"JUNK-ALLOWED",keyword)
LISPSYM(Kcase,"CASE",keyword)
LISPSYM(Klevel,"LEVEL",keyword)
LISPSYM(Klength,"LENGTH",keyword)
LISPSYM(Kgensym,"GENSYM",keyword)
LISPSYM(Kescape,"ESCAPE",keyword)
LISPSYM(Kbase,"BASE",keyword)
LISPSYM(Karray,"ARRAY",keyword)
LISPSYM(Kcircle,"CIRCLE",keyword)
LISPSYM(Kpretty,"PRETTY",keyword)
LISPSYM(Kclosure,"CLOSURE",keyword)
LISPSYM(Kstream,"STREAM",keyword)
LISPSYM(Ktest,"TEST",keyword)
LISPSYM(Ktest_not,"TEST-NOT",keyword)
LISPSYM(Kkey,"KEY",keyword)
LISPSYM(Knicknames,"NICKNAMES",keyword)
LISPSYM(Kuse,"USE",keyword)
LISPSYM(Kupdate,"UPDATE",keyword)
LISPSYM(Kfrom_end,"FROM-END",keyword)
LISPSYM(Kinitial_value,"INITIAL-VALUE",keyword)
LISPSYM(Kcount,"COUNT",keyword)
LISPSYM(Ksize,"SIZE",keyword)
LISPSYM(Krehash_size,"REHASH-SIZE",keyword)
LISPSYM(Krehash_threshold,"REHASH-THRESHOLD",keyword)
LISPSYM(Kdefaults,"DEFAULTS",keyword)
LISPSYM(Kdevice,"DEVICE",keyword)
LISPSYM(Kdirectory,"DIRECTORY",keyword)
LISPSYM(Kname,"NAME",keyword)
LISPSYM(Ktype,"TYPE",keyword)
LISPSYM(Kversion,"VERSION",keyword)
LISPSYM(Khost,"HOST",keyword)
LISPSYM(Kdirection,"DIRECTION",keyword)
LISPSYM(Kif_exists,"IF-EXISTS",keyword)
LISPSYM(Kif_does_not_exist,"IF-DOES-NOT-EXIST",keyword)
LISPSYM(Kfull,"FULL",keyword)
LISPSYM(Kabort,"ABORT",keyword)
#ifdef REXX
LISPSYM(Kresult,"RESULT",keyword)
LISPSYM(Kstring,"STRING",keyword)
LISPSYM(Ktoken,"TOKEN",keyword)
LISPSYM(Kasync,"ASYNC",keyword)
LISPSYM(Kreturn,"RETURN",keyword)
#endif

# sonstige Symbole:
LISPSYM(string_char,"STRING-CHAR",lisp) # als Typ in ARRAY
LISPSYM(array_rank_limit,"ARRAY-RANK-LIMIT",lisp) # als Konstante in ARRAY
LISPSYM(array_dimension_limit,"ARRAY-DIMENSION-LIMIT",lisp) # als Konstante in ARRAY
LISPSYM(array_total_size_limit,"ARRAY-TOTAL-SIZE-LIMIT",lisp) # als Konstante in ARRAY
LISPSYM(subtype_integer,"SUBTYPE-INTEGER",system) # als Funktion für ARRAY
LISPSYM(simple_vector,"SIMPLE-VECTOR",lisp) # als Typ in SEQUENCE, PREDTYPE
LISPSYM(simple_string,"SIMPLE-STRING",lisp) # als Typ in SEQUENCE, PREDTYPE
LISPSYM(bit_vector,"BIT-VECTOR",lisp) # als Typ in SEQUENCE, PREDTYPE
LISPSYM(simple_bit_vector,"SIMPLE-BIT-VECTOR",lisp) # als Typ in SEQUENCE, PREDTYPE
LISPSYM(array,"ARRAY",lisp) # als Typ in SEQUENCE, PREDTYPE
LISPSYM(simple_array,"SIMPLE-ARRAY",lisp) # als Typ in SEQUENCE, PREDTYPE
LISPSYM(setf,"SETF",lisp) # als Fehlermelder in SEQUENCE
LISPSYM(cerror,"CERROR",lisp) # als Funktion für PACKAGE
LISPSYM(Kinternal,"INTERNAL",keyword) # als INTERN-Ergebnis in PACKAGE
LISPSYM(Kexternal,"EXTERNAL",keyword) # als INTERN-Ergebnis in PACKAGE
LISPSYM(Kinherited,"INHERITED",keyword) # als INTERN-Ergebnis in PACKAGE
LISPSYM(do_symbols,"DO-SYMBOLS",lisp) # als Fehlermelder in PACKAGE
LISPSYM(do_external_symbols,"DO-EXTERNAL-SYMBOLS",lisp) # als Fehlermelder in PACKAGE
LISPSYM(packagestern,"*PACKAGE*",lisp) # als Variable in PACKAGE
LISPSYM(internal_time_units_per_second,"INTERNAL-TIME-UNITS-PER-SECOND",lisp) # als Konstante in MISC
LISPSYM(encode_universal_time,"ENCODE-UNIVERSAL-TIME",lisp) # als Funktion in MISC
LISPSYM(error_count,"*ERROR-COUNT*",system) # als Variable in MISC
LISPSYM(error_handler,"*ERROR-HANDLER*",lisp) # als Variable in MISC
LISPSYM(format,"FORMAT",lisp) # als Funktion in MISC
LISPSYM(completion,"COMPLETION",system) # als Funktion in STREAM, für den Fall, daß GNU_READLINE benutzt wird
#ifdef KEYBOARD
LISPSYM(keyboard_input,"*KEYBOARD-INPUT*",lisp) # als Variable in STREAM
#endif
LISPSYM(terminal_io,"*TERMINAL-IO*",lisp) # als Variable in STREAM
LISPSYM(key_bindings,"*KEY-BINDINGS*",system) # als Variable in STREAM
LISPSYM(query_io,"*QUERY-IO*",lisp) # als Variable in STREAM
LISPSYM(debug_io,"*DEBUG-IO*",lisp) # als Variable in STREAM
LISPSYM(standard_input,"*STANDARD-INPUT*",lisp) # als Variable in STREAM
LISPSYM(standard_output,"*STANDARD-OUTPUT*",lisp) # als Variable in STREAM
LISPSYM(error_output,"*ERROR-OUTPUT*",lisp) # als Variable in STREAM
LISPSYM(trace_output,"*TRACE-OUTPUT*",lisp) # als Variable in STREAM
#ifdef PRINTER_ATARI
LISPSYM(printer_output,"*PRINTER-OUTPUT*",lisp) # als Variable in STREAM
LISPSYM(printer_timeout,"*PRINTER-TIMEOUT*",lisp) # als Variable in STREAM
#endif
LISPSYM(default_pathname_defaults,"*DEFAULT-PATHNAME-DEFAULTS*",lisp) # als Variable in PATHNAME
LISPSYM(Kwild,"WILD",keyword) # als Pathname-Komponente in PATHNAME
LISPSYM(Krelative,"RELATIVE",keyword) # als Pathname-Komponente in PATHNAME
LISPSYM(Kabsolute,"ABSOLUTE",keyword) # als Pathname-Komponente in PATHNAME
#if defined(PATHNAME_ATARI) || defined(PATHNAME_MSDOS)
LISPSYM(Kcurrent,"CURRENT",keyword) # als Pathname-Komponente in PATHNAME
#endif
#if defined(PATHNAME_ATARI) || defined(PATHNAME_MSDOS) || defined(PATHNAME_AMIGAOS)
LISPSYM(Kparent,"PARENT",keyword) # als Pathname-Komponente in PATHNAME
#endif
LISPSYM(Knewest,"NEWEST",keyword) # als Pathname-Komponente in PATHNAME
LISPSYM(Kinput,"INPUT",keyword) # als Argument in PATHNAME
LISPSYM(Koutput,"OUTPUT",keyword) # als Argument in PATHNAME
LISPSYM(Kio,"IO",keyword) # als Argument in PATHNAME
LISPSYM(Kprobe,"PROBE",keyword) # als Argument in PATHNAME
LISPSYM(unsigned_byte,"UNSIGNED-BYTE",lisp) # als Argument in PATHNAME
LISPSYM(signed_byte,"SIGNED-BYTE",lisp) # als Argument in PATHNAME
LISPSYM(Kdefault,"DEFAULT",keyword) # als Argument in PATHNAME
LISPSYM(Kerror,"ERROR",keyword) # als Argument in PATHNAME
LISPSYM(Knew_version,"NEW-VERSION",keyword) # als Argument in PATHNAME
LISPSYM(Krename,"RENAME",keyword) # als Argument in PATHNAME
LISPSYM(Krename_and_delete,"RENAME-AND-DELETE",keyword) # als Argument in PATHNAME
LISPSYM(Koverwrite,"OVERWRITE",keyword) # als Argument in PATHNAME
LISPSYM(Kappend,"APPEND",keyword) # als Argument in PATHNAME
LISPSYM(Ksupersede,"SUPERSEDE",keyword) # als Argument in PATHNAME
LISPSYM(Kcreate,"CREATE",keyword) # als Argument in PATHNAME
#if defined(HAVE_SHELL) && defined(ATARI)
LISPSYM(myshell,"MYSHELL",system) # als Funktion in PATHNAME
#endif
LISPSYM(warn,"WARN",lisp) # als Funktion in STREAM, PATHNAME
LISPSYM(with_output_to_string,"WITH-OUTPUT-TO-STRING",lisp) # als Fehlermelder in STREAM
LISPSYM(integer,"INTEGER",lisp) # als Typ in STREAM
LISPSYM(hash_table,"HASH-TABLE",lisp) # als Typ in IO, PREDTYPE
LISPSYM(random_state,"RANDOM-STATE",lisp) # als Typ in IO, PREDTYPE
LISPSYM(read_base,"*READ-BASE*",lisp) # als Variable in IO
LISPSYM(read_suppress,"*READ-SUPPRESS*",lisp) # als Variable in IO
LISPSYM(readtablestern,"*READTABLE*",lisp) # als Variable in IO
LISPSYM(features,"*FEATURES*",lisp) # als Variable in IO
LISPSYM(read_preserve_whitespace,"*READ-PRESERVE-WHITESPACE*",system) # als Variable in IO
LISPSYM(read_recursive_p,"*READ-RECURSIVE-P*",system) # als Variable in IO
LISPSYM(read_reference_table,"*READ-REFERENCE-TABLE*",system) # als Variable in IO
LISPSYM(backquote_level,"*BACKQUOTE-LEVEL*",system) # als Variable in IO
LISPSYM(backquote_reader,"`-READER",system) # als Funktion für IO
LISPSYM(comma_reader,",-READER",system) # als Funktion für IO
LISPSYM(compiling,"*COMPILING*",system) # als Variable in IO
LISPSYM(make_byte,"MAKE-BYTE",system) # als Funktion für IO
LISPSYM(Kupcase,"UPCASE",keyword) # als *PRINT-CASE* - Wert in IO
LISPSYM(Kdowncase,"DOWNCASE",keyword) # als *PRINT-CASE* - Wert in IO
LISPSYM(Kcapitalize,"CAPITALIZE",keyword) # als *PRINT-CASE* - Wert in IO
LISPSYM(print_case,"*PRINT-CASE*",lisp) # als Variable in IO       --+
LISPSYM(print_level,"*PRINT-LEVEL*",lisp) # als Variable in IO       |
LISPSYM(print_length,"*PRINT-LENGTH*",lisp) # als Variable in IO     |
LISPSYM(print_gensym,"*PRINT-GENSYM*",lisp) # als Variable in IO     |
LISPSYM(print_escape,"*PRINT-ESCAPE*",lisp) # als Variable in IO     | Reihenfolge
LISPSYM(print_radix,"*PRINT-RADIX*",lisp) # als Variable in IO       | mit IO.D abgestimmt!
LISPSYM(print_base,"*PRINT-BASE*",lisp) # als Variable in IO         |
LISPSYM(print_array,"*PRINT-ARRAY*",lisp) # als Variable in IO       |
LISPSYM(print_circle,"*PRINT-CIRCLE*",lisp) # als Variable in IO     |
LISPSYM(print_pretty,"*PRINT-PRETTY*",lisp) # als Variable in IO     |
LISPSYM(print_closure,"*PRINT-CLOSURE*",lisp) # als Variable in IO --+
LISPSYM(print_rpars,"*PRINT-RPARS*",lisp) # als Variable in IO
LISPSYM(print_circle_table,"*PRINT-CIRCLE-TABLE*",system) # als Variable in IO
LISPSYM(prin_level,"*PRIN-LEVEL*",system) # als Variable in IO
LISPSYM(prin_bqlevel,"*PRIN-BQLEVEL*",system) # als Variable in IO
LISPSYM(prin_stream,"*PRIN-STREAM*",system) # als Variable in IO
LISPSYM(prin_linelength,"*PRIN-LINELENGTH*",system) # als Variable in IO
LISPSYM(prin_l1,"*PRIN-L1*",system) # als Variable in IO
LISPSYM(prin_lm,"*PRIN-LM*",system) # als Variable in IO
LISPSYM(prin_rpar,"*PRIN-RPAR*",system) # als Variable in IO
LISPSYM(prin_jblocks,"*PRIN-JBLOCKS*",system) # als Variable in IO
LISPSYM(prin_jbstrings,"*PRIN-JBSTRINGS*",system) # als Variable in IO
LISPSYM(prin_jbmodus,"*PRIN-JBMODUS*",system) # als Variable in IO
LISPSYM(prin_jblpos,"*PRIN-JBLPOS*",system) # als Variable in IO
LISPSYM(backquote,"BACKQUOTE",system) # als Marker in IO
LISPSYM(splice,"SPLICE",system) # als Marker in IO
LISPSYM(nsplice,"NSPLICE",system) # als Marker in IO
LISPSYM(unquote,"UNQUOTE",system) # als Marker in IO
LISPSYM(structure_print,"STRUCTURE-PRINT",system) # als Funktion in IO
LISPSYM(defstruct_description,"DEFSTRUCT-DESCRIPTION",system) # als Property in IO
LISPSYM(trace_values,"*TRACE-VALUES*",lisp) # als Variable in EVAL
LISPSYM(lambda,"LAMBDA",lisp) # als Marker in EVAL
LISPSYM(LLoptional,"&OPTIONAL",lisp) # als Lambdalisten-Marker in EVAL
LISPSYM(LLkey,"&KEY",lisp) # als Lambdalisten-Marker in EVAL
LISPSYM(LLallow_other_keys,"&ALLOW-OTHER-KEYS",lisp) # als Lambdalisten-Marker in EVAL
LISPSYM(LLrest,"&REST",lisp) # als Lambdalisten-Marker in EVAL
LISPSYM(LLaux,"&AUX",lisp) # als Lambdalisten-Marker in EVAL
LISPSYM(macro,"MACRO",system) # als Marker in EVAL
LISPSYM(special,"SPECIAL",lisp) # als Declaration-Specifier in EVAL
LISPSYM(source,"SOURCE",system) # als Declaration-Specifier in EVAL
LISPSYM(optimize,"OPTIMIZE",lisp) # als Declaration-Specifier in EVAL
LISPSYM(declaration,"DECLARATION",lisp) # als Declaration-Specifier in EVAL
LISPSYM(compile_lambda,"COMPILE-LAMBDA",system) # als Funktion für EVAL
LISPSYM(expand_lambdabody_main,"%EXPAND-LAMBDABODY-MAIN",system) # als Funktion für EVAL
LISPSYM(compile,"COMPILE",lisp) # als Declaration-Specifier und Funktion für EVAL
LISPSYM(evalhookstern,"*EVALHOOK*",lisp) # als Variable in EVAL
LISPSYM(applyhookstern,"*APPLYHOOK*",lisp) # als Variable in EVAL
LISPSYM(macroexpand_hook,"*MACROEXPAND-HOOK*",lisp) # als Variable in EVAL
LISPSYM(lambda_parameters_limit,"LAMBDA-PARAMETERS-LIMIT",lisp) # als Konstante in EVAL
LISPSYM(call_arguments_limit,"CALL-ARGUMENTS-LIMIT",lisp) # als Konstante in EVAL
LISPSYM(multiple_values_limit,"MULTIPLE-VALUES-LIMIT",lisp) # als Konstante in EVAL
LISPSYM(jmpbuf_size,"*JMPBUF-SIZE*",system) # als Konstante in EVAL für COMPILER
LISPSYM(big_endian,"*BIG-ENDIAN*",system) # als Konstante in EVAL für COMPILER
LISPSYM(Klambda,"LAMBDA",keyword) # als Marker in EVAL
LISPSYM(plus2,"++",lisp) # als Variable in DEBUG
LISPSYM(plus3,"+++",lisp) # als Variable in DEBUG
LISPSYM(mal2,"**",lisp) # als Variable in DEBUG
LISPSYM(mal3,"***",lisp) # als Variable in DEBUG
LISPSYM(durch2,"//",lisp) # als Variable in DEBUG
LISPSYM(durch3,"///",lisp) # als Variable in DEBUG
LISPSYM(driverstern,"*DRIVER*",lisp) # als Variable in DEBUG
LISPSYM(break_driver,"*BREAK-DRIVER*",lisp) # als Variable in DEBUG
LISPSYM(break_count,"*BREAK-COUNT*",system) # als Variable in DEBUG
LISPSYM(frame_limit1,"*FRAME-LIMIT1*",system) # als Variable in DEBUG
LISPSYM(frame_limit2,"*FRAME-LIMIT2*",system) # als Variable in DEBUG
LISPSYM(make_macro_expansion,"MAKE-MACRO-EXPANSION",system) # als Funktion für CONTROL
LISPSYM(pthe,"%THE",system) # als Funktion für CONTROL
LISPSYM(compile_form,"COMPILE-FORM",system) # als Funktion für CONTROL
LISPSYM(inline,"INLINE",lisp) # als Declaration-Specifier in CONTROL
LISPSYM(notinline,"NOTINLINE",lisp) # als Declaration-Specifier in CONTROL
LISPSYM(inlinable,"INLINABLE",system) # als Property in CONTROL
LISPSYM(symbol,"SYMBOL",lisp) # als Typ in PREDTYPE
LISPSYM(address,"ADDRESS",lisp) # als Typ in PREDTYPE
LISPSYM(stream,"STREAM",lisp) # als Typ in PREDTYPE
LISPSYM(package,"PACKAGE",lisp) # als Typ in PREDTYPE
LISPSYM(readtable,"READTABLE",lisp) # als Typ in PREDTYPE
LISPSYM(load_time_eval,"LOAD-TIME-EVAL",lisp) # als Typ in PREDTYPE
#ifdef ALIEN
LISPSYM(alien,"ALIEN",lisp) # als Typ in PREDTYPE
#endif
LISPSYM(compiled_function,"COMPILED-FUNCTION",lisp) # als Typ in PREDTYPE
LISPSYM(frame_pointer,"FRAME-POINTER",lisp) # als Typ in PREDTYPE
LISPSYM(read_label,"READ-LABEL",lisp) # als Typ in PREDTYPE
LISPSYM(system_internal,"SYSTEM-INTERNAL",lisp) # als Typ in PREDTYPE
LISPSYM(fixnum,"FIXNUM",lisp) # als Typ in PREDTYPE
LISPSYM(bignum,"BIGNUM",lisp) # als Typ in PREDTYPE
LISPSYM(ratio,"RATIO",lisp) # als Typ in PREDTYPE
LISPSYM(short_float,"SHORT-FLOAT",lisp) # als Typ in PREDTYPE
LISPSYM(single_float,"SINGLE-FLOAT",lisp) # als Typ in PREDTYPE
LISPSYM(double_float,"DOUBLE-FLOAT",lisp) # als Typ in PREDTYPE
LISPSYM(long_float,"LONG-FLOAT",lisp) # als Typ in PREDTYPE
LISPSYM(typep,"TYPEP",lisp) # als Funktion für PREDTYPE
LISPSYM(deftype_expander,"DEFTYPE-EXPANDER",system) # als Property in PREDTYPE
LISPSYM(pi,"PI",lisp) # als Variable in LISPARIT
LISPSYM(most_positive_fixnum,"MOST-POSITIVE-FIXNUM",lisp) # als Konstante in LISPARIT
LISPSYM(most_negative_fixnum,"MOST-NEGATIVE-FIXNUM",lisp) # als Konstante in LISPARIT
LISPSYM(most_positive_short_float,"MOST-POSITIVE-SHORT-FLOAT",lisp) # als Konstante in LISPARIT
LISPSYM(least_positive_short_float,"LEAST-POSITIVE-SHORT-FLOAT",lisp) # als Konstante in LISPARIT
LISPSYM(least_negative_short_float,"LEAST-NEGATIVE-SHORT-FLOAT",lisp) # als Konstante in LISPARIT
LISPSYM(most_negative_short_float,"MOST-NEGATIVE-SHORT-FLOAT",lisp) # als Konstante in LISPARIT
LISPSYM(most_positive_single_float,"MOST-POSITIVE-SINGLE-FLOAT",lisp) # als Konstante in LISPARIT
LISPSYM(least_positive_single_float,"LEAST-POSITIVE-SINGLE-FLOAT",lisp) # als Konstante in LISPARIT
LISPSYM(least_negative_single_float,"LEAST-NEGATIVE-SINGLE-FLOAT",lisp) # als Konstante in LISPARIT
LISPSYM(most_negative_single_float,"MOST-NEGATIVE-SINGLE-FLOAT",lisp) # als Konstante in LISPARIT
LISPSYM(most_positive_double_float,"MOST-POSITIVE-DOUBLE-FLOAT",lisp) # als Konstante in LISPARIT
LISPSYM(least_positive_double_float,"LEAST-POSITIVE-DOUBLE-FLOAT",lisp) # als Konstante in LISPARIT
LISPSYM(least_negative_double_float,"LEAST-NEGATIVE-DOUBLE-FLOAT",lisp) # als Konstante in LISPARIT
LISPSYM(most_negative_double_float,"MOST-NEGATIVE-DOUBLE-FLOAT",lisp) # als Konstante in LISPARIT
LISPSYM(most_positive_long_float,"MOST-POSITIVE-LONG-FLOAT",lisp) # als Variable in LISPARIT
LISPSYM(least_positive_long_float,"LEAST-POSITIVE-LONG-FLOAT",lisp) # als Variable in LISPARIT
LISPSYM(least_negative_long_float,"LEAST-NEGATIVE-LONG-FLOAT",lisp) # als Variable in LISPARIT
LISPSYM(most_negative_long_float,"MOST-NEGATIVE-LONG-FLOAT",lisp) # als Variable in LISPARIT
LISPSYM(short_float_epsilon,"SHORT-FLOAT-EPSILON",lisp) # als Konstante in LISPARIT
LISPSYM(single_float_epsilon,"SINGLE-FLOAT-EPSILON",lisp) # als Konstante in LISPARIT
LISPSYM(double_float_epsilon,"DOUBLE-FLOAT-EPSILON",lisp) # als Konstante in LISPARIT
LISPSYM(long_float_epsilon,"LONG-FLOAT-EPSILON",lisp) # als Variable in LISPARIT
LISPSYM(short_float_negative_epsilon,"SHORT-FLOAT-NEGATIVE-EPSILON",lisp) # als Konstante in LISPARIT
LISPSYM(single_float_negative_epsilon,"SINGLE-FLOAT-NEGATIVE-EPSILON",lisp) # als Konstante in LISPARIT
LISPSYM(double_float_negative_epsilon,"DOUBLE-FLOAT-NEGATIVE-EPSILON",lisp) # als Konstante in LISPARIT
LISPSYM(long_float_negative_epsilon,"LONG-FLOAT-NEGATIVE-EPSILON",lisp) # als Variable in LISPARIT
LISPSYM(default_float_format,"*DEFAULT-FLOAT-FORMAT*",lisp) # als Variable in LISPARIT
LISPSYM(read_default_float_format,"*READ-DEFAULT-FLOAT-FORMAT*",lisp) # als Variable in LISPARIT
LISPSYM(write_float,"WRITE-FLOAT",system) # als Funktion für LISPARIT
LISPSYM(random_state_stern,"*RANDOM-STATE*",lisp) # als Variable in LISPARIT
LISPSYM(Klisting,"LISTING",keyword) # als Argument für SPVW
LISPSYM(compile_file,"COMPILE-FILE",lisp) # als Funktion für SPVW

