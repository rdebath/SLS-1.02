# Evaluator, Applyer und Bytecode-Interpreter für CLISP
# Bruno Haible 18.3.1993

#include "lispbibl.c"


# Der STACK:
  #if !defined(STACK_register)
    global object* STACK;
  #endif
  #ifdef HAVE_SAVED_STACK
    global object* saved_STACK;
  #endif

# MULTIPLE-VALUE-SPACE:
  #if !defined(mv_count_register)
    global uintC mv_count;
  #endif
  global object mv_space [mv_limit-1];

# Während der Ausführung eines SUBR, FSUBR: das aktuelle SUBR bzw. FSUBR
  #if !defined(subr_self_register)
    global object subr_self;
  #endif

# Funktionen-Tabelle:
# Darin stehen nur SUBRs, die der Compiler "inline" machen darf.
# In FUNTAB1 und FUNTAB2 stehen SUBRs ohne Rest-Parameter (also
# mit zur Compile-Zeit bekannter fester Argumentezahl).
# In FUNTABR stehen SUBRs mit Rest-Parameter.
  #define _(name)  &subr_tab.D_##name  # Adresse von SUBR name, wie L(name)
  # erst FUNTAB1 und FUNTAB2 :
  local Subr FUNTAB[] = {
    # SPVW : 0 SUBRs
    # EVAL : 2 SUBRs
    _(funtabref), _(subr_info),
    # ARRAY : 27 SUBRs
    _(svref), _(svstore), _(array_element_type), _(array_rank),
    _(array_dimension), _(array_dimensions), _(array_total_size),
    _(adjustable_array_p), _(bit_and), _(bit_ior), _(bit_xor), _(bit_eqv),
    _(bit_nand), _(bit_nor), _(bit_andc1), _(bit_andc2), _(bit_orc1),
    _(bit_orc2), _(bit_not), _(array_has_fill_pointer_p), _(fill_pointer),
    _(set_fill_pointer), _(vector_push), _(vector_pop), _(vector_push_extend),
    _(make_array), _(adjust_array),
    # CHARSTRG : 52 SUBRs
    _(standard_char_p), _(graphic_char_p), _(string_char_p), _(alpha_char_p),
    _(upper_case_p), _(lower_case_p), _(both_case_p), _(digit_char_p),
    _(alphanumericp), _(char_code), _(char_bits), _(char_font), _(code_char),
    _(make_char), _(character), _(char_upcase), _(char_downcase),
    _(digit_char), _(char_int), _(int_char), _(char_name), _(char_bit),
    _(set_char_bit), _(char), _(schar), _(store_char), _(store_schar),
    _(string_gleich), _(string_ungleich), _(string_kleiner),
    _(string_groesser), _(string_klgleich), _(string_grgleich),
    _(string_equal), _(string_not_equal), _(string_lessp), _(string_greaterp),
    _(string_not_greaterp), _(string_not_lessp), _(search_string_gleich),
    _(search_string_equal), _(make_string), _(string_both_trim),
    _(nstring_upcase), _(string_upcase), _(nstring_downcase),
    _(string_downcase), _(nstring_capitalize), _(string_capitalize),
    _(string), _(name_char), _(substring),
    # CONTROL : 21 SUBRs
    _(symbol_value), _(symbol_function), _(boundp), _(fboundp),
    _(special_form_p), _(set), _(makunbound), _(fmakunbound), _(values_list),
    _(driver), _(unwind_to_driver), _(macro_function), _(macroexpand),
    _(macroexpand_1), _(proclaim), _(eval), _(evalhook), _(applyhook),
    _(constantp), _(parse_body), _(keyword_test),
    # DEBUG : 2 SUBRs
    _(room), _(gc),
    # HASHTABL : 10 SUBRs
    _(make_hash_table), _(gethash), _(puthash), _(remhash), _(maphash),
    _(clrhash), _(hash_table_count), _(hash_table_iterator),
    _(hash_table_iterate), _(sxhash),
    # IO : 36 SUBRs
    _(copy_readtable), _(set_syntax_from_char), _(set_macro_character),
    _(get_macro_character), _(make_dispatch_macro_character),
    _(set_dispatch_macro_character), _(get_dispatch_macro_character),
    _(read), _(read_preserving_whitespace), _(read_delimited_list),
    _(read_line), _(read_char), _(unread_char), _(peek_char), _(listen),
    _(read_char_no_hang), _(clear_input), _(read_from_string),
    _(parse_integer), _(write), _(prin1), _(print), _(pprint), _(princ),
    _(write_to_string), _(prin1_to_string), _(princ_to_string), _(write_char),
    _(write_string), _(write_line), _(terpri), _(fresh_line),
    _(finish_output), _(force_output), _(clear_output), _(line_position),
    # LIST : 83 SUBRs
    _(car), _(cdr), _(caar), _(cadr), _(cdar), _(cddr), _(caaar), _(caadr),
    _(cadar), _(caddr), _(cdaar), _(cdadr), _(cddar), _(cdddr), _(caaaar),
    _(caaadr), _(caadar), _(caaddr), _(cadaar), _(cadadr), _(caddar),
    _(cadddr), _(cdaaar), _(cdaadr), _(cdadar), _(cdaddr), _(cddaar),
    _(cddadr), _(cdddar), _(cddddr), _(cons), _(tree_equal), _(endp),
    _(list_length), _(nth), _(first), _(second), _(third), _(fourth),
    _(fifth), _(sixth), _(seventh), _(eighth), _(ninth), _(tenth), _(rest),
    _(nthcdr), _(last), _(make_list), _(copy_list), _(copy_alist),
    _(copy_tree), _(revappend), _(nreconc), _(list_nreverse), _(butlast),
    _(nbutlast), _(ldiff), _(rplaca), _(prplaca), _(rplacd), _(prplacd),
    _(subst), _(subst_if), _(subst_if_not), _(nsubst), _(nsubst_if),
    _(nsubst_if_not), _(sublis), _(nsublis), _(member), _(member_if),
    _(member_if_not), _(tailp), _(adjoin), _(acons), _(pairlis), _(assoc),
    _(assoc_if), _(assoc_if_not), _(rassoc), _(rassoc_if), _(rassoc_if_not),
    # MISC : 10 SUBRs
    _(lisp_implementation_type), _(lisp_implementation_version),
    _(software_type), _(software_version), _(identity), _(get_universal_time),
    _(get_internal_run_time), _(get_internal_real_time), _(sleep), _(time),
    # PACKAGE : 25 SUBRs
    _(make_symbol), _(find_package), _(package_name), _(package_nicknames),
    _(rename_package), _(package_use_list), _(package_used_by_list),
    _(package_shadowing_symbols), _(list_all_packages), _(intern),
    _(find_symbol), _(unintern), _(export), _(unexport), _(import),
    _(shadowing_import), _(shadow), _(use_package), _(unuse_package),
    _(make_package), _(in_package), _(find_all_symbols), _(map_symbols),
    _(map_external_symbols), _(map_all_symbols),
    # PATHNAME : 27 SUBRs
    _(parse_namestring), _(pathname), _(pathnamehost), _(pathnamedevice),
    _(pathnamedirectory), _(pathnamename), _(pathnametype),
    _(pathnameversion), _(file_namestring), _(directory_namestring),
    _(host_namestring), _(merge_pathnames), _(enough_namestring),
    _(make_pathname), _(namestring), _(truename), _(probe_file),
    _(delete_file), _(rename_file), _(open), _(directory), _(cd),
    _(make_dir), _(delete_dir), _(file_write_date), _(file_author),
    _(savemem),
    # PREDTYPE : 41 SUBRs
    _(eq), _(eql), _(equal), _(equalp), _(consp), _(atom), _(symbolp),
    _(stringp), _(numberp), _(compiled_function_p), _(null), _(not),
    _(closurep), _(listp), _(integerp), _(fixnump), _(rationalp), _(floatp),
    _(short_float_p), _(single_float_p), _(double_float_p), _(long_float_p),
    _(complexp), _(streamp), _(random_state_p), _(readtablep),
    _(hash_table_p), _(pathnamep), _(characterp), _(functionp), _(packagep),
    _(arrayp), _(simple_array_p), _(bit_vector_p), _(vectorp),
    _(simple_vector_p), _(simple_string_p), _(simple_bit_vector_p),
    _(commonp), _(type_of), _(coerce),
    # RECORD : 14 SUBRs
    _(record_ref), _(record_store), _(record_length), _(structure_ref),
    _(structure_store), _(make_structure), _(copy_structure),
    _(structure_type_p), _(closure_name), _(closure_codevec),
    _(closure_consts), _(make_code_vector), _(make_closure),
    _(make_load_time_eval),
    # SEQUENCE : 40 SUBRs
    _(sequencep), _(elt), _(setelt), _(subseq), _(copy_seq), _(length),
    _(reverse), _(nreverse), _(make_sequence), _(reduce), _(fill),
    _(replace), _(remove), _(remove_if), _(remove_if_not), _(delete),
    _(delete_if), _(delete_if_not), _(remove_duplicates),
    _(delete_duplicates), _(substitute), _(substitute_if),
    _(substitute_if_not), _(nsubstitute), _(nsubstitute_if),
    _(nsubstitute_if_not), _(find), _(find_if), _(find_if_not), _(position),
    _(position_if), _(position_if_not), _(count), _(count_if),
    _(count_if_not), _(mismatch), _(search), _(sort), _(stable_sort),
    _(merge),
    # STREAM : 16 SUBRs
    _(make_synonym_stream), _(make_two_way_stream), _(make_echo_stream),
    _(make_string_input_stream), _(string_input_stream_index),
    _(make_string_output_stream), _(get_output_stream_string),
    _(make_string_push_stream), _(input_stream_p), _(output_stream_p),
    _(stream_element_type), _(close), _(read_byte), _(write_byte),
    _(file_position), _(file_length),
    # SYMBOL : 15 SUBRs
    _(putd), _(proclaim_constant), _(get), _(getf), _(get_properties),
    _(putplist), _(put), _(remprop), _(symbol_package), _(symbol_plist),
    _(symbol_name), _(keywordp), _(gensym), _(special_variable_p), _(gensym),
    # LISPARIT : 84 SUBRs
    _(decimal_string), _(zerop), _(plusp), _(minusp), _(oddp), _(evenp),
    _(einsplus), _(einsminus), _(conjugate), _(exp), _(expt), _(log),
    _(sqrt), _(isqrt), _(abs), _(phase), _(signum), _(sin), _(cos), _(tan),
    _(cis), _(asin), _(acos), _(atan), _(sinh), _(cosh), _(tanh), _(asinh),
    _(acosh), _(atanh), _(float), _(rational), _(rationalize), _(numerator),
    _(denominator), _(floor), _(ceiling), _(truncate), _(round), _(mod),
    _(rem), _(ffloor), _(fceiling), _(ftruncate), _(fround), _(decode_float),
    _(scale_float), _(float_radix), _(float_sign), _(float_digits),
    _(float_precision), _(integer_decode_float), _(complex), _(realpart),
    _(imagpart), _(lognand), _(lognor), _(logandc1), _(logandc2), _(logorc1),
    _(logorc2), _(boole), _(lognot), _(logtest), _(logbitp), _(ash),
    _(logcount), _(integer_length), _(byte), _(bytesize), _(byteposition),
    _(ldb), _(ldb_test), _(mask_field), _(dpb), _(deposit_field), _(random),
    _(make_random_state), _(fakultaet), _(exquo), _(long_float_digits),
    _(set_long_float_digits), _(log2), _(log10),
    };
  # Das waren 505 SUBRs.
  # Nun FUNTABR :
  local Subr FUNTABR[] = {
    # SPVW : 0 SUBRs
    # EVAL : 0 SUBRs
    # ARRAY : 7 SUBRs
    _(vector), _(aref), _(store), _(array_in_bounds_p),
    _(array_row_major_index), _(bit), _(sbit),
    # CHARSTRG : 13 SUBRs
    _(char_gleich), _(char_ungleich), _(char_kleiner), _(char_groesser),
    _(char_klgleich), _(char_grgleich), _(char_equal), _(char_not_equal),
    _(char_lessp), _(char_greaterp), _(char_not_greaterp), _(char_not_lessp),
    _(string_concat),
    # CONTROL : 10 SUBRs
    _(apply), _(pfuncall), _(funcall), _(mapcar), _(maplist), _(mapc),
    _(mapl), _(mapcan), _(mapcon), _(values),
    # DEBUG : 0 SUBRs
    # HASHTABL : 0 SUBRs
    # IO : 0 SUBRs
    # LIST : 4 SUBRs
    _(list), _(liststern), _(append), _(nconc),
    # MISC : 1 SUBR
    _(error),
    # PACKAGE : 0 SUBRs
    # PATHNAME : 0 SUBRs
    # PREDTYPE : 0 SUBRs
    # RECORD : 0 SUBRs
    # SEQUENCE : 6 SUBRs
    _(concatenate), _(map), _(some), _(every), _(notany), _(notevery),
    # STREAM : 2 SUBRs
    _(make_broadcast_stream), _(make_concatenated_stream),
    # SYMBOL : 0 SUBRs
    # LISPARIT : 18 SUBRs
    _(gleich), _(ungleich), _(kleiner), _(groesser), _(klgleich),
    _(grgleich), _(max), _(min), _(plus), _(minus), _(mal), _(durch), _(gcd),
    _(lcm), _(logior), _(logxor), _(logand), _(logeqv),
    };
  # Das waren 61 SUBRs.
  #undef _
  #define FUNTAB1  (&FUNTAB[0])
  #define FUNTAB2  (&FUNTAB[256])
  #define FUNTAB_length  (sizeof(FUNTAB)/sizeof(Subr))
  #define FUNTABR_length  (sizeof(FUNTABR)/sizeof(Subr))

# Argumenttyp-Kürzel bei compilierten Closures:
  typedef enum {cclos_argtype_default,
                cclos_argtype_0_0,
                cclos_argtype_1_0,
                cclos_argtype_2_0,
                cclos_argtype_3_0,
                cclos_argtype_4_0,
                cclos_argtype_5_0,
                cclos_argtype_0_1,
                cclos_argtype_1_1,
                cclos_argtype_2_1,
                cclos_argtype_3_1,
                cclos_argtype_4_1,
                cclos_argtype_0_2,
                cclos_argtype_1_2,
                cclos_argtype_2_2,
                cclos_argtype_3_2,
                cclos_argtype_0_3,
                cclos_argtype_1_3,
                cclos_argtype_2_3,
                cclos_argtype_0_4,
                cclos_argtype_1_4,
                cclos_argtype_0_5,
                cclos_argtype_0_0_rest,
                cclos_argtype_1_0_rest,
                cclos_argtype_2_0_rest,
                cclos_argtype_3_0_rest,
                cclos_argtype_4_0_rest,
                cclos_argtype_0_0_key,
                cclos_argtype_1_0_key,
                cclos_argtype_2_0_key,
                cclos_argtype_3_0_key,
                cclos_argtype_4_0_key,
                cclos_argtype_0_1_key,
                cclos_argtype_1_1_key,
                cclos_argtype_2_1_key,
                cclos_argtype_3_1_key,
                cclos_argtype_0_2_key,
                cclos_argtype_1_2_key,
                cclos_argtype_2_2_key,
                cclos_argtype_0_3_key,
                cclos_argtype_1_3_key,
                cclos_argtype_0_4_key,
               }
          cclos_argtype_;

# Header im Bytecode einer compilierten Closure:
  #ifndef FAST_SP
    #define CCHD 2
  #else
    #define CCHD 0
  #endif

# Aufruf des Bytecode-Interpreters:
# Interpretiert den Bytecode einer compilierten Closure.
# interpret_bytecode(closure,codevec,index);
# > closure: compilierte Closure
# > codevec: ihr Codevektor, ein Simple-Bit-Vector
# > index: Start-Index
# < mv_count/mv_space: Werte
# verändert STACK, kann GC auslösen
  # local Values interpret_bytecode (object closure, object codevec, uintL index);
  local Values interpret_bytecode_ (object closure, Sbvector codeptr, uintB* byteptr);
  #define interpret_bytecode(closure,codevec,index)  \
    interpret_bytecode_(closure,TheSbvector(codevec),&TheSbvector(codevec)->data[index])

# Werte der Bytecodes (256 Stück):
  typedef enum {
               # (1) Konstanten
               cod_nil,
               cod_push_nil,
               cod_t,
               cod_const,
               # (2) statische Variablen
               cod_load,
               cod_loadi,
               cod_loadc,
               cod_loadv,
               cod_loadic,
               cod_store,
               cod_storei,
               cod_storec,
               cod_storev,
               cod_storeic,
               # (3) dynamische Variablen
               cod_getvalue,
               cod_setvalue,
               cod_bind,
               cod_unbind1,
               cod_unbind,
               cod_progv,
               # (4) Stackoperationen
               cod_push,
               cod_pop,
               cod_skip,
               cod_skipi,
               cod_skipsp,
               # (5) Programmfluß und Sprünge
               cod_skip_ret,
               cod_jmp,
               cod_jmpif,
               cod_jmpifnot,
               cod_jmpif1,
               cod_jmpifnot1,
               cod_jmpifatom,
               cod_jmpifconsp,
               cod_jmpifeq,
               cod_jmpifnoteq,
               cod_jmpifeqto,
               cod_jmpifnoteqto,
               cod_jmphash,
               cod_jsr,
               cod_jmptail,
               # (6) Environments und Closures
               cod_venv,
               cod_make_vector1_push,
               cod_copy_closure,
               # (7) Funktionsaufrufe
               cod_call,
               cod_call0,
               cod_call1,
               cod_call2,
               cod_calls1,
               cod_calls2,
               cod_callsr,
               cod_callc,
               cod_callckey,
               cod_funcall,
               cod_apply,
               # (8) optionale und Keyword-Argumente
               cod_push_unbound,
               cod_jmpifboundp,
               cod_boundp,
               cod_unbound_nil,
               # (9) Behandlung mehrerer Werte
               cod_values0,
               cod_values1,
               cod_stack_to_mv,
               cod_mv_to_stack,
               cod_nv_to_stack,
               cod_mv_to_list,
               cod_list_to_mv,
               cod_mvcallp,
               cod_mvcall,
               # (10) BLOCK
               cod_block_open,
               cod_block_close,
               cod_return_from,
               # (11) TAGBODY
               cod_tagbody_open,
               cod_tagbody_close_nil,
               cod_tagbody_close,
               cod_go,
               # (12) CATCH und THROW
               cod_catch_open,
               cod_catch_close,
               cod_throw,
               # (13) UNWIND-PROTECT
               cod_uwp_open,
               cod_uwp_normal_exit,
               cod_uwp_close,
               cod_uwp_cleanup,
               # (14) einige Funktionen
               cod_not,
               cod_eq,
               cod_car,
               cod_cdr,
               cod_cons,
               cod_symbol_function,
               cod_svref,
               cod_svset,
               cod_list,
               cod_error,
               # (15) kombinierte Operationen
               cod_nil_push,
               cod_t_push,
               cod_const_push,
               cod_load_push,
               cod_loadi_push,
               cod_loadc_push,
               cod_loadv_push,
               cod_pop_store,
               cod_getvalue_push,
               cod_jsr_push,
               cod_copy_closure_push,
               cod_call_push,
               cod_call1_push,
               cod_call2_push,
               cod_calls1_push,
               cod_calls2_push,
               cod_callsr_push,
               cod_callc_push,
               cod_callckey_push,
               cod_funcall_push,
               cod_apply_push,
               cod_car_push,
               cod_cdr_push,
               cod_cons_push,
               cod_list_push,
               cod_nil_store,
               cod_t_store,
               cod_load_storec,
               cod_calls1_store,
               cod_calls2_store,
               cod_callsr_store,
               cod_load_cdr_store,
               cod_load_cons_store,
               cod_load_inc_store,
               cod_load_dec_store,
               cod_load_car_store,
               cod_call1_jmpif,
               cod_call1_jmpifnot,
               cod_call2_jmpif,
               cod_call2_jmpifnot,
               cod_calls1_jmpif,
               cod_calls1_jmpifnot,
               cod_calls2_jmpif,
               cod_calls2_jmpifnot,
               cod_callsr_jmpif,
               cod_callsr_jmpifnot,
               cod_load_jmpif,
               cod_load_jmpifnot,
               cod_load_car_push,
               cod_load_cdr_push,
               cod_load_inc_push,
               cod_load_dec_push,
               cod_const_symbol_function,
               cod_const_symbol_function_push,
               cod_const_symbol_function_store,
               # (16) Kurzcodes
               cod_load0,
               cod_load1,
               cod_load2,
               cod_load3,
               cod_load4,
               cod_load5,
               cod_load6,
               cod_load7,
               cod_load8,
               cod_load9,
               cod_load10,
               cod_load11,
               cod_load12,
               cod_load13,
               cod_load14,
               cod_load15,
               cod_load16,
               cod_load17,
               cod_load18,
               cod_load19,
               cod_load20,
               cod_load21,
               cod_load_push0,
               cod_load_push1,
               cod_load_push2,
               cod_load_push3,
               cod_load_push4,
               cod_load_push5,
               cod_load_push6,
               cod_load_push7,
               cod_load_push8,
               cod_load_push9,
               cod_load_push10,
               cod_load_push11,
               cod_load_push12,
               cod_load_push13,
               cod_load_push14,
               cod_load_push15,
               cod_load_push16,
               cod_load_push17,
               cod_load_push18,
               cod_load_push19,
               cod_load_push20,
               cod_load_push21,
               cod_const0,
               cod_const1,
               cod_const2,
               cod_const3,
               cod_const4,
               cod_const5,
               cod_const6,
               cod_const7,
               cod_const8,
               cod_const9,
               cod_const10,
               cod_const11,
               cod_const12,
               cod_const13,
               cod_const14,
               cod_const15,
               cod_const16,
               cod_const17,
               cod_const18,
               cod_const19,
               cod_const20,
               cod_const21,
               cod_const_push0,
               cod_const_push1,
               cod_const_push2,
               cod_const_push3,
               cod_const_push4,
               cod_const_push5,
               cod_const_push6,
               cod_const_push7,
               cod_const_push8,
               cod_const_push9,
               cod_const_push10,
               cod_const_push11,
               cod_const_push12,
               cod_const_push13,
               cod_const_push14,
               cod_const_push15,
               cod_const_push16,
               cod_const_push17,
               cod_const_push18,
               cod_const_push19,
               cod_const_push20,
               cod_const_push21,
               cod_store0,
               cod_store1,
               cod_store2,
               cod_store3,
               cod_store4,
               cod_store5,
               cod_store6,
               cod_store7,
               cod_store8,
               cod_store9,
               cod_store10,
               cod_store11,
               cod_store12,
               cod_store13,
               cod_store14,
               cod_store15,
               cod_store16,
               cod_store17,
               cod_store18,
               cod_store19,
               cod_store20,
               cod_store21,
               }
          bytecode_enum;


#        ---------------------- LISP-FUNKTIONEN -----------------------

# (SYS::%FUNTABREF i) liefert den Namen der Funktion Nr. i aus der Funktionen-
# tabelle (ein Symbol), bzw. NIL falls i nicht im richtigen Bereich liegt.
LISPFUNN(funtabref,1)
  { var reg2 object arg = popSTACK(); # Argument
    var reg1 uintL i;
    if (posfixnump(arg) # sollte ein Fixnum >=0
        && (i = posfixnum_to_L(arg),
            i < FUNTAB_length+FUNTABR_length # und < Tabellenlänge sein
       )   )
      # Name des indizierten Elements der Tabelle:
      { value1 = (i < FUNTAB_length
                  ? FUNTAB[i]                # aus FUNTAB1/2
                  : FUNTABR[i-FUNTAB_length] # bzw. aus FUNTABR
                 )->name;
      }
      else
      { value1 = NIL; } # oder NIL
    mv_count=1; # als Wert
  }

# (SYS::SUBR-INFO obj) liefert, wenn obj ein SUBR (oder ein Symbol mit einem
# SUBR als globaler Funktionsdefinition) ist, Information zu diesem SUBR,
# 6 Werte:
#   name              Name,
#   req-anz           Anzahl der required-Parameter,
#   opt-anz           Anzahl der optionalen Parameter,
#   rest-p            Flag, ob &rest angegeben,
#   keywords          Liste der zulässigen Keywords (leer: kein &key angegeben),
#   allow-other-keys  Flag, ob zusätzliche Keywords erlaubt sind,
# und sonst NIL.
LISPFUNN(subr_info,1)
  { var reg1 object obj = popSTACK();
    if (!subrp(obj))
      { if (!(symbolp(obj) && msubrp(Symbol_function(obj))))
          { value1 = NIL; mv_count=0; return; } # kein SUBR -> kein Wert
        obj = Symbol_function(obj);
      }
    # obj ist ein SUBR
    pushSTACK(TheSubr(obj)->name); # Name
    pushSTACK(fixnum(TheSubr(obj)->req_anz)); # req-anz
    pushSTACK(fixnum(TheSubr(obj)->opt_anz)); # opt-anz
    pushSTACK(TheSubr(obj)->rest_flag == subr_norest ? NIL : T); # rest-p
    coerce_sequence(TheSubr(obj)->keywords,S(list));
    pushSTACK(value1); # Keyword-Vektor als Liste
    pushSTACK(TheSubr(obj)->key_flag == subr_key_allow ? T : NIL); # allow-other-keys
    funcall(L(values),6); # 6 Werte
  }


#        ----------------------- UNTERPROGRAMME -----------------------

# UP: Löst einen Frame auf, auf den STACK zeigt.
# unwind();
# Die Werte mv_count/mv_space bleiben dieselben.
# Falls es kein Unwind-Protect-Frame ist: kehrt normal zurück.
# Falls es ein Unwind-Protect-Frame ist:
#   rettet die Werte, klettert STACK und SP hoch
#   und springt dann unwind_protect_to_save.fun an.
# verändert STACK
# kann GC auslösen
  global unwind_protect_caller unwind_protect_to_save;
  global void unwind (void);
  global void unwind()
    { var reg3 tint frame_info = mtypecode(STACK_0);
      #ifdef unwind_bit_t
      if (frame_info & bit(unwind_bit_t)) # überhaupt etwas zu tun?
      #else
      if (frame_info >= unwind_limit_t) # überhaupt etwas zu tun?
      #endif
        # (Nein bei APPLY, EVAL ungetrapped, CATCH,
        #  IBLOCK und ITAGBODY ungenestet)
        { if ((frame_info & bit(skip2_bit_t)) == 0) # ENV-Frame oder DYNBIND-Frame?
            #ifdef entrypoint_bit_t
            if (frame_info & bit(entrypoint_bit_t)) # BLOCK, TAGBODY, CATCH etc. ?
            #else
            if (frame_info < entrypoint_limit_t) # BLOCK, TAGBODY, CATCH etc. ?
            #endif
              # Frame mit Exitpoint liegt vor
              if (frame_info & bit(blockgo_bit_t)) # BLOCK oder TAGBODY?
                # BLOCK_FRAME oder TAGBODY_FRAME liegt vor
                if (frame_info & bit(cframe_bit_t)) # compilierter?
                  # CBLOCK_FRAME oder CTAGBODY_FRAME liegt vor
                  { # Im Cons (NAME/Tags . <Framepointer>)
                    Cdr(STACK_(frame_ctag)) = disabled; # Exit/Tags disablen
                  }
                  else
                  # IBLOCK_FRAME oder ITAGBODY_FRAME liegt vor, genestet
                  { # Im Cons (NAME/Tags . <Framepointer>)
                    # (erstes Paar der Aliste next_env)
                    Cdr(Car(STACK_(frame_next_env))) = disabled; # Exit/Tags disablen
                  }
                else
                # UNWIND_PROTECT_FRAME, DRIVER_FRAME oder getrappter APPLY/EVAL_FRAME liegt vor
                if (frame_info & bit(dynjump_bit_t))
                  # UNWIND_PROTECT_FRAME oder DRIVER_FRAME liegt vor
                  if (frame_info & bit(driver_bit_t))
                    # DRIVER_FRAME liegt vor
                    {
                      #ifdef HAVE_NUM_STACK
                      # NUM_STACK_normal muß wieder den Wert bekommen, den es vor
                      # Aufbau des Driver-Frames hatte:
                      NUM_STACK =
                      NUM_STACK_normal =
                        ((DRIVER_frame_data*)(STACK_(frame_SP)))->old_NUM_STACK_normal;
                      #endif
                    }
                    else
                    # UNWIND_PROTECT_FRAME liegt vor
                    { enter_frame_at_STACK(); }
                  else
                  # getrappter APPLY/EVAL_FRAME liegt vor
                  { # Wie im Tracer:
                    var reg1 object values;
                    mv_to_list(); values = popSTACK(); # Werte in Liste packen
                    dynamic_bind(S(trace_values),values); # *TRACE-VALUES* binden
                    break_driver(T); # Break-Driver aufrufen
                    list_to_mv(Symbol_value(S(trace_values)), # wieder Werte bilden
                               fehler_mv_zuviel(mtypecode(STACK_(0+3))==TRAPPED_EVAL_frame_info
                                                ? S(eval)
                                                : S(apply)
                                               );
                              );
                    dynamic_unbind(); # Bindung auflösen
                  }
              else
              # VAR_FRAME oder FUN_FRAME liegt vor
              { var reg4 object* new_STACK = topofframe(STACK_0); # Pointer übern Frame
                if (frame_info & bit(fun_bit_t))
                  {} # bei Funktionen nichts weiter zu tun
                  else
                  # VAR_FRAME liegt vor, bindingptr läuft durch die Bindungen hoch
                  { var reg2 object* frame_end = STACKpointable(new_STACK);
                    var reg1 object* bindingptr = &STACK_(frame_bindings); # Beginn der Variablen-/Funktionsbindungen
                    until (bindingptr == frame_end)
                      { if ((oint)(*(bindingptr STACKop 0)) & wbit(dynam_bit_o))
                          if ((oint)(*(bindingptr STACKop 0)) & wbit(active_bit_o))
                            # Bindung statisch oder inaktiv -> nichts zu tun
                            # Bindung dynamisch und aktiv -> Wert zurückschreiben:
                            { TheSymbolflagged(*(bindingptr STACKop varframe_binding_sym))->symvalue =
                                *(bindingptr STACKop varframe_binding_value);
                            }
                        bindingptr skipSTACKop varframe_binding_size; # nächste Bindung
                  }   }
                # STACK neu setzen, dadurch Frame auflösen:
                setSTACK(STACK = new_STACK);
                goto fertig;
              }
            else
            # DYNBIND_FRAME oder ENV_FRAME liegt vor
            if (frame_info & bit(envbind_bit_t))
              # ENV_FRAME liegt vor
              { var reg1 object* ptr = &STACK_1;
                switch (frame_info & envbind_case_mask_t)
                  { case (ENV1V_frame_info & envbind_case_mask_t): # 1 VAR_ENV
                      aktenv.var_env = *ptr; ptr skipSTACKop 1; break;
                    case (ENV1F_frame_info & envbind_case_mask_t): # 1 FUN_ENV
                      aktenv.fun_env = *ptr; ptr skipSTACKop 1; break;
                    case (ENV1B_frame_info & envbind_case_mask_t): # 1 BLOCK_ENV
                      aktenv.block_env = *ptr; ptr skipSTACKop 1; break;
                    case (ENV1G_frame_info & envbind_case_mask_t): # 1 GO_ENV
                      aktenv.go_env = *ptr; ptr skipSTACKop 1; break;
                    case (ENV1D_frame_info & envbind_case_mask_t): # 1 DECL_ENV
                      aktenv.decl_env = *ptr; ptr skipSTACKop 1; break;
                    case (ENV2VD_frame_info & envbind_case_mask_t): # 1 VAR_ENV und 1 DECL_ENV
                      aktenv.var_env = *ptr; ptr skipSTACKop 1;
                      aktenv.decl_env = *ptr; ptr skipSTACKop 1;
                      break;
                    case (ENV5_frame_info & envbind_case_mask_t): # alle 5 Environments
                      aktenv.var_env = *ptr; ptr skipSTACKop 1;
                      aktenv.fun_env = *ptr; ptr skipSTACKop 1;
                      aktenv.block_env = *ptr; ptr skipSTACKop 1;
                      aktenv.go_env = *ptr; ptr skipSTACKop 1;
                      aktenv.decl_env = *ptr; ptr skipSTACKop 1;
                      break;
                    default: NOTREACHED
              }   }
              else
              # DYNBIND_FRAME liegt vor
              { var reg4 object* new_STACK = topofframe(STACK_0); # Pointer übern Frame
                var reg2 object* frame_end = STACKpointable(new_STACK);
                var reg1 object* bindingptr = &STACK_1; # Beginn der Bindungen
                # bindingptr läuft durch die Bindungen hoch
                until (bindingptr == frame_end)
                  { Symbol_value(*(bindingptr STACKop 0)) = *(bindingptr STACKop 1);
                    bindingptr skipSTACKop 2; # nächste Bindung
                  }
                # STACK neu setzen, dadurch Frame auflösen:
                setSTACK(STACK = new_STACK);
                goto fertig;
              }
        }
      # STACK neu setzen, dadurch Frame auflösen:
      setSTACK(STACK = topofframe(STACK_0));
      fertig: ;
    }

# UP: "unwindet" den STACK bis zum nächsten DRIVER_FRAME und
# springt in die entsprechende Top-Level-Schleife.
# reset();
  global nonreturning void reset (void);
  global nonreturning void reset()
    { # Beim Auflösen von UNWIND-PROTECT-Frames keine Werte retten:
      value1 = NIL; mv_count=0;
      unwind_protect_to_save.fun = (restart)&reset;
      loop
        { # Hört der STACK hier auf?
          if ((STACK_0 == nullobj) && (STACK_1 == nullobj))
            { driver(); } # STACK völlig weg -> Neustart
          if (mtypecode(STACK_0) & bit(frame_bit_t))
            # Bei STACK_0 beginnt ein Frame
            { if (mtypecode(STACK_0) == DRIVER_frame_info) # DRIVER_FRAME ?
                break; # ja -> gefunden
              unwind(); # Frame auflösen
            }
            else
            # STACK_0 enthält ein normales LISP-Objekt
            { skipSTACK(1); }
        }
      # Bei STACK_0 beginnt ein Driver-Frame.
      enter_frame_at_STACK();
    }

# UP: bindet dynamisch die Symbole der Liste symlist
# an die Werte aus der Liste vallist.
# progv(symlist,vallist);
# > symlist, vallist: zwei Listen
# Es wird genau ein Variablenbindungsframe aufgebaut.
# verändert STACK
  global void progv (object symlist, object vallist);
  global void progv(symlist,vallist)
    var reg2 object symlist;
    var reg4 object vallist;
    { # Platz auf dem STACK verlangen:
      get_space_on_STACK(llength(symlist)*2*sizeof(object));
      # Frame aufbauen:
      { var reg5 object* top_of_frame = STACK; # Pointer übern Frame
        var reg3 object symlistr = symlist;
        while (consp(symlistr)) # Symbolliste durchgehen
          { var reg1 object sym = Car(symlistr);
            if (!symbolp(sym)) { fehler_kein_symbol(S(progv),sym); }
            if (constantp(TheSymbol(sym)))
              { pushSTACK(sym);
                pushSTACK(S(progv));
                fehler(
                       DEUTSCH ? "~: ~ ist eine Konstante und kann nicht dynamisch gebunden werden." :
                       ENGLISH ? "~: ~ is a constant, cannot be bound dynamically" :
                       FRANCAIS ? "~: ~ est une constante et ne peut pas être liée dynamiquement." :
                       ""
                      );
              }
            pushSTACK(Symbol_value(sym)); # alter Wert der Variablen
            pushSTACK(sym); # Variable
            symlistr = Cdr(symlistr);
          }
        finish_frame(DYNBIND);
        # Frame fertig aufgebaut, nun die Werte der Variablen verändern:
        while (consp(symlist))
          { if (atomp(vallist))
              # Wertliste kürzer als Symbolliste
              # -> alle weiteren "Werte" sind #<UNBOUND>
              { do { Symbol_value(Car(symlist)) = unbound;
                     symlist = Cdr(symlist);
                   }
                   while (consp(symlist));
                break;
              }
            # Symbol bekommt neuen Wert:
            Symbol_value(Car(symlist)) = Car(vallist);
            symlist = Cdr(symlist); vallist = Cdr(vallist);
          }
    } }

# UP: Löst die dynamische Schachtelung im STACK auf bis zu dem Frame
# (ausschließlich), auf den upto zeigt, und springt diesen dann an.
# unwind_upto(upto);
# > upto: Pointer auf einen Frame (in den Stack, ohne Typinfo).
# Rettet die Werte mv_count/mv_space.
# verändert STACK,SP
# kann GC auslösen
# Springt dann den gefundenen Frame an.
  global nonreturning void unwind_upto (object* upto_frame);
  global nonreturning void unwind_upto(upto_frame)
    var reg1 object* upto_frame;
    { unwind_protect_to_save.fun        = &unwind_upto;
      unwind_protect_to_save.upto_frame = upto_frame;
      until (STACK == upto_frame) # am Ziel-Frame angelangt?
        { if (mtypecode(STACK_0) & bit(frame_bit_t)) # liegt ein Frame vor?
            { unwind(); } # ja -> auflösen
            # (Sollte dies ein Unwind-Protect-Frame sein, so wird danach wieder
            # unwind_upto(upto_frame) aufgerufen, und wir sind wieder hier.)
            else
            { skipSTACK(1); } # nein -> einfach weiter
        }
      # Nun zeigt STACK auf den gefundenen FRAME.
      enter_frame_at_STACK();
    }

# UP: throwt zum Tag tag und übergibt dabei die Werte mv_count/mv_space.
# Kommt nur dann zurück, wenn es keinen CATCH-Frame dieses Tags gibt.
# throw(tag);
  global void throw (object tag);
  global void throw(tag)
    var reg1 object tag;
    { # Suche nach Catch-Frame mit Tag =tag:
      var reg1 object* FRAME = STACK;
      loop # Suche im Stack ab ptr nach einem CATCH-Frame mit demselben Tag:
        { if (FRAME_(0) == nullobj) # Stackende?
            { return; } # ja -> kein passendes Catch vorhanden -> Rücksprung
          if (mtypecode(FRAME_(0)) & bit(frame_bit_t))
            # Frame gefunden
            { if ((mtypecode(FRAME_(0)) == CATCH_frame_info) # Catch-Frame?
                  && eq(FRAME_(frame_tag),tag) # mit demselben Tag?
                 )
                break; # ja -> Suchschleife fertig
              # Frame übergehen:
              FRAME = topofframe(FRAME_(0));
            }
            else
            { FRAME skipSTACKop 1; }
        }
      # ptr zeigt auf den untersten CATCH-Frame mit demselben Tag
      unwind_upto(FRAME); # bis dorthin auflösen, dann anspringen
    }

# UP: Liefert den Wert eines Symbols im aktuellen Environment.
# sym_value(symbol)
# > symbol: Symbol
# < ergebnis: Wert des Symbols im aktuellen Environment
  local object sym_value (object sym);
  local object sym_value(sym)
    var reg6 object sym;
    { if (constantp(TheSymbol(sym))) goto globalvalue; # Konstanten haben nur globale Werte
      if (special_var_p(TheSymbol(sym))) goto globalvalue; # special deklarierte ebenso
     {var reg5 object env = aktenv.var_env; # aktuelles VAR_ENV
      #ifdef NO_symbolflags
        #define binds_sym_p(bindptr) # Bindet die Bindung bei bindptr das Symbol sym? \
          (eq(*(bindptr STACKop 1),sym) # richtiges Symbol?                                  \
           && eq(*(bindingsptr STACKop 0),fixnum(bit(active_bit))) # und aktiv und statisch? \
          )
      #else
      var reg4 object cmp = (object)((oint)sym | wbit(active_bit_o)); # zum Vergleich: Bindung muß aktiv sein
        #define binds_sym_p(bindptr) # Bindet die Bindung bei bindptr das Symbol sym? \
          (eq(*(bindingsptr STACKop 0),cmp)) # richtiges Symbol und aktiv und statisch?
      #endif
      next_env:
        switch (typecode(env))
          { case_system: # Environment ist ein Pointer auf einen Variablenbindungs-Frame
              { var reg2 object* FRAME = TheFramepointer(env);
               {var reg3 uintL count = (oint)(FRAME_(frame_anz)); # Anzahl der Bindungen
                var reg1 object* bindingsptr = &FRAME_(frame_bindings); # Pointer auf die erste Bindung
                dotimesL(count,count,
                  { if (binds_sym_p(bindingsptr)) # richtiges Symbol und aktiv und statisch?
                      { var reg1 object value = *(bindingsptr STACKop varframe_binding_value);
                        if (eq(value,specdecl))
                          { goto globalvalue; }
                          else
                          { return value; }
                      }
                    bindingsptr skipSTACKop varframe_binding_size; # nein: nächste Bindung
                  });
                env = FRAME_(frame_next_env);
                goto next_env;
              }}
            case_svector: # Environment ist ein Simple-Vector
              goto next_vector;
            default: # Environment ist NIL
              goto globalvalue;
          }
      next_vector:
        # Environment ist ein Simple-Vector
        { var reg2 uintL count = floor(TheSvector(env)->length,2); # Anzahl der Bindungen
          var reg1 object* ptr = &TheSvector(env)->data[0];
          dotimesL(count,count,
            { if (eq(*ptr,sym)) # richtiges Symbol?
                { var reg1 object value = *(ptr+1);
                  if (eq(value,specdecl))
                    { goto globalvalue; }
                    else
                    { return value; }
                }
              ptr += 2; # nächste Bindung
            });
          env = *ptr; # nächstes Environment
          if (simple_vector_p(env)) goto next_vector; # ein Simple-Vector?
          # sonst: Environment ist NIL
        }
      #undef binds_sym_p
     }
      globalvalue: # Es gilt der globale (dynamische) Wert des Symbols
        return Symbol_value(sym);
    }

# UP: Setzt den Wert eines Symbols im aktuellen Environment.
# setq(symbol,value);
# > symbol: Symbol, keine Konstante
# > value: gewünschter Wert des Symbols im aktuellen Environment
  global void setq (object sym, object value);
  global void setq(sym,value)
    var reg6 object sym;
    var reg7 object value;
    { if (special_var_p(TheSymbol(sym))) goto globalvalue; # special deklarierte ebenso
     {var reg5 object env = aktenv.var_env; # aktuelles VAR_ENV
      #ifdef NO_symbolflags
        #define binds_sym_p(bindptr) # Bindet die Bindung bei bindptr das Symbol sym? \
          (eq(*(bindptr STACKop 1),sym) # richtiges Symbol?                                  \
           && eq(*(bindingsptr STACKop 0),fixnum(bit(active_bit))) # und aktiv und statisch? \
          )
      #else
      var reg4 object cmp = (object)((oint)sym | wbit(active_bit_o)); # zum Vergleich: Bindung muß aktiv sein
        #define binds_sym_p(bindptr) # Bindet die Bindung bei bindptr das Symbol sym? \
          (eq(*(bindingsptr STACKop 0),cmp)) # richtiges Symbol und aktiv und statisch?
      #endif
      next_env:
        switch (typecode(env))
          { case_system: # Environment ist ein Pointer auf einen Variablenbindungs-Frame
              { var reg2 object* FRAME = TheFramepointer(env);
               {var reg3 uintL count = (oint)(FRAME_(frame_anz)); # Anzahl der Bindungen
                var reg1 object* bindingsptr = &FRAME_(frame_bindings); # Pointer auf die erste Bindung
                dotimesL(count,count,
                  { if (binds_sym_p(bindingsptr)) # richtiges Symbol und aktiv und statisch?
                      { if (eq(*(bindingsptr STACKop varframe_binding_value),specdecl))
                          { goto globalvalue; }
                          else
                          { *(bindingsptr STACKop varframe_binding_value) = value; return; }
                      }
                    bindingsptr skipSTACKop varframe_binding_size; # nein: nächste Bindung
                  });
                env = FRAME_(frame_next_env);
                goto next_env;
              }}
            case_svector: # Environment ist ein Simple-Vector
              goto next_vector;
            default: # Environment ist NIL
              goto globalvalue;
          }
      next_vector:
        # Environment ist ein Simple-Vector
        { var reg2 uintL count = floor(TheSvector(env)->length,2); # Anzahl der Bindungen
          var reg1 object* ptr = &TheSvector(env)->data[0];
          dotimesL(count,count,
            { if (eq(*ptr,sym)) # richtiges Symbol?
                { if (eq(*(ptr+1),specdecl))
                    { goto globalvalue; }
                    else
                    { *(ptr+1) = value; return; }
                }
              ptr += 2; # nächste Bindung
            });
          env = *ptr; # nächstes Environment
          if (simple_vector_p(env)) goto next_vector; # ein Simple-Vector?
          # sonst: Environment ist NIL
        }
      #undef binds_sym_p
     }
      globalvalue: # Es gilt der globale (dynamische) Wert des Symbols
        Symbol_value(sym) = value; return;
    }

# UP: Liefert zu einem Symbol seine Funktionsdefinition in einem Environment
# sym_function(sym,fenv)
# > sym: Symbol
# > fenv: ein Funktions- und Macrobindungs-Environment
# < ergebnis: Funktionsdefinition, entweder unbound (falls undefinierte Funktion)
#             oder Closure/SUBR/FSUBR oder ein Cons (SYS::MACRO . expander).
  global object sym_function (object sym, object fenv);
  global object sym_function(sym,env)
    var reg6 object sym;
    var reg4 object env;
    { var reg5 object value;
     {next_env:
        switch (typecode(env))
          { case_system: # Environment ist ein Pointer auf einen Funktionsbindungs-Frame
              { var reg2 object* FRAME = TheFramepointer(env);
               {var reg3 uintL count = (oint)(FRAME_(frame_anz)); # Anzahl der Bindungen
                var reg1 object* bindingsptr = &FRAME_(frame_bindings); # Pointer auf die erste Bindung
                dotimesL(count,count,
                  { if (eq(*(bindingsptr STACKop 0),sym)) # richtiges Symbol?
                      { value = *(bindingsptr STACKop 1); goto fertig; }
                    bindingsptr skipSTACKop 2; # nein: nächste Bindung
                  });
                env = FRAME_(frame_next_env);
                goto next_env;
              }}
            case_svector: # Environment ist ein Simple-Vector
              goto next_vector;
            default: # Environment ist NIL
              goto globalvalue;
          }
      next_vector:
        # Environment ist ein Simple-Vector
        { var reg2 uintL count = floor(TheSvector(env)->length,2); # Anzahl der Bindungen
          var reg1 object* ptr = &TheSvector(env)->data[0];
          dotimesL(count,count,
            { if (eq(*ptr,sym)) # richtiges Symbol?
                { value = *(ptr+1); goto fertig; }
              ptr += 2; # nächste Bindung
            });
          env = *ptr; # nächstes Environment
          if (simple_vector_p(env)) goto next_vector; # ein Simple-Vector?
          # sonst: Environment ist NIL
        }
     }
      globalvalue: # Es gilt die globale Funktionsdefinition
        return Symbol_function(sym);
      fertig: # Symbol aktiv im Environment gefunden, "Wert" value
        # (eine Closure oder NIL oder ein Cons (SYS::MACRO . expander) )
        # Falls Definition = NIL (während LABELS), gilt die Funktion als
        # undefiniert:
        if (nullp(value)) { value = unbound; }
        return value;
    }

# UP: Wertet eine Form in einem gegebenen Environment aus.
# eval_5env(form,var,fun,block,go,decl);
# > var_env: Wert für VAR_ENV
# > fun_env: Wert für FUN_ENV
# > block_env: Wert für BLOCK_ENV
# > go_env: Wert für GO_ENV
# > decl_env: Wert für DECL_ENV
# > form: Form
# < mv_count/mv_space: Werte
# kann GC auslösen
  global Values eval_5env (object form, object var_env, object fun_env, object block_env, object go_env, object decl_env);
  global Values eval_5env(form,var_env,fun_env,block_env,go_env,decl_env)
    var reg2 object form;
    var reg3 object var_env;
    var reg4 object fun_env;
    var reg5 object block_env;
    var reg6 object go_env;
    var reg7 object decl_env;
    { # Environments binden:
      make_ENV5_frame();
      # aktuelle Environments setzen:
      aktenv.var_env = var_env;
      aktenv.fun_env = fun_env;
      aktenv.block_env = block_env;
      aktenv.go_env = go_env;
      aktenv.decl_env = decl_env;
      # Form auswerten:
      eval(form);
      # Environment-Frame auflösen:
      unwind();
      return; # fertig
    }

# UP: Wertet eine Form in einem leeren Environment aus.
# eval_noenv(form);
# > form: Form
# < mv_count/mv_space: Werte
# kann GC auslösen
  global Values eval_noenv (object form);
  global Values eval_noenv(form)
    var reg1 object form;
    { return_Values eval_5env(form,NIL,NIL,NIL,NIL,O(top_decl_env)); }

# UP: "nestet" ein FUN-Environment, d.h. schreibt alle aktiven Bindungen
# aus dem Stack in neu allozierte Vektoren.
# nest_fun(env)
# > env: FUN-Env
# < ergebnis: selbes Environment, kein Pointer in den Stack
# kann GC auslösen
  global object nest_fun (object env);
  global object nest_fun(env)
    var reg5 object env;
    { var reg6 uintL depth = 0; # Rekursionszähler:=0
      # Pseudorekursion mit Input env, Output env.
      nest_start: # Rekursionsbeginn
      if (typecode(env) == system_type)
        # env ist ein Pointer auf einen STACK-Frame.
        { check_STACK();
          pushSTACK(env); # env retten
          # entrekursiviert nest_fun(NEXT_ENV(env)) durchführen:
          {var reg1 object* FRAME = TheFramepointer(env);
           env = FRAME_(frame_next_env); depth++; goto nest_start;
          }
          nest_reentry: depth--;
          # NEXT_ENV ist nun genestet.
          {var reg4 object* FRAME = TheFramepointer(STACK_0); # nächster zu nestender STACK-Frame
           STACK_0 = env; # bisher genestetes Environment
           {var reg3 uintL anzahl = (oint)(FRAME_(frame_anz)); # Anzahl der noch nicht genesteten Bindungen
            if (anzahl == 0)
              # keine Bindungen -> unnötig, einen Vektor zu erzeugen.
              { env = popSTACK(); }
              else
              # Vektor für anzahl Bindungen erzeugen:
              { env = allocate_vector(2*anzahl+1);
                # und füllen:
                { var reg1 object* ptr = &TheSvector(env)->data[0];
                  var reg2 object* bindingsptr = &FRAME_(frame_bindings); # Pointer auf die erste Bindung
                  # anzahl Bindungen ab bindingsptr in den Vektor ab ptr eintragen:
                  dotimespL(anzahl,anzahl,
                    { *ptr++ = *(bindingsptr STACKop 0); # Bindung in den Vektor kopieren
                      *ptr++ = *(bindingsptr STACKop 1);
                      bindingsptr skipSTACKop 2;
                    });
                  *ptr++ = popSTACK(); # genestetes NEXT_ENV in Vektor eintragen
                }
                FRAME_(frame_next_env) = env; # Vektor als NEXT_ENV in den Frame
                FRAME_(frame_anz) = (object)0; # neue Zahl noch nicht genesteter Bindungen
              }
        } }}
      # mit diesem Nest-Teilschritt fertig.
      if (depth>0) goto nest_reentry; # Ende der Rekursion
      return env;
    }

# UP: "nestet" ein VAR-Environment, d.h. schreibt alle aktiven Bindungen
# aus dem Stack in neu allozierte Vektoren.
# nest_var(env)
# > env: VAR-Env
# < ergebnis: selbes Environment, kein Pointer in den Stack
# kann GC auslösen
  local object nest_var (object env);
  local object nest_var(env)
    var reg6 object env;
    { var reg7 uintL depth = 0; # Rekursionszähler:=0
      # Pseudorekursion mit Input env, Output env.
      nest_start: # Rekursionsbeginn
      if (typecode(env) == system_type)
        # env ist ein Pointer auf einen STACK-Frame.
        { check_STACK();
          pushSTACK(env); # env retten
          # entrekursiviert nest_var(NEXT_ENV(env)) durchführen:
          {var reg1 object* FRAME = TheFramepointer(env);
           env = FRAME_(frame_next_env); depth++; goto nest_start;
          }
          nest_reentry: depth--;
          # NEXT_ENV ist nun genestet.
          {var reg5 object* FRAME = TheFramepointer(STACK_0); # nächster zu nestender STACK-Frame
           STACK_0 = env; # bisher genestetes Environment
           # Suche (von unten) die erste aktive unter den noch nicht
           # genesteten Bindungen:
           {var reg3 uintL anzahl = (oint)(FRAME_(frame_anz)); # Anzahl der noch nicht genesteten Bindungen
            var reg4 uintL count = 0;
            var reg1 object* bindingsptr = &FRAME_(frame_bindings); # Pointer auf die erste Bindung
            until ((count>=anzahl) # alle ungenesteten Bindungen durch?
                   || ((oint)(*(bindingsptr STACKop 0)) & wbit(active_bit_o)) # aktive Bindung entdeckt?
                  )
              { # nein -> weitersuchen:
                bindingsptr skipSTACKop varframe_binding_size;
                count++;
              }
            # Unterhalb von bindingsptr sind count inaktive Bindungen.
            # Ab bindingsptr kommen anzahl-count aktive, zu nestende Bindungen.
            anzahl = anzahl-count; # Anzahl zu nestender Bindungen
            if (anzahl == 0)
              # keine Bindungen -> unnötig, einen Vektor zu erzeugen.
              { env = popSTACK(); }
              else
              # Vektor für anzahl Bindungen erzeugen:
              { env = allocate_vector(2*anzahl+1);
                # und füllen:
                { var reg2 object* ptr = &TheSvector(env)->data[0];
                  # Bindungen ab bindingsptr in den Vektor ab ptr eintragen:
                  dotimespL(anzahl,anzahl,
                    { if ((oint)(*(bindingsptr STACKop varframe_binding_mark)) & wbit(dynam_bit_o)) # Bindung dynamisch?
                        # dynamische Bindung, lexikalische Sichtbarkeit
                        { *ptr++ = symbol_without_flags(*(bindingsptr STACKop varframe_binding_sym)); # Symbol ohne Flag-Bits in den Vektor
                          *ptr++ = specdecl; # als special reference kennzeichnen
                          # Bindung bleibt im Frame aktiv
                        }
                        else
                        # statische Bindung, lexikalische Sichtbarkeit
                        { *(oint*)(bindingsptr STACKop varframe_binding_mark) &= ~wbit(active_bit_o); # Bindung inaktivieren
                          *ptr++ = *(bindingsptr STACKop varframe_binding_sym); # Bindung in den Vektor kopieren
                          *ptr++ = *(bindingsptr STACKop varframe_binding_value);
                        }
                      bindingsptr skipSTACKop varframe_binding_size;
                    });
                  *ptr++ = popSTACK(); # genestetes NEXT_ENV in Vektor eintragen
                }
                FRAME_(frame_next_env) = env; # Vektor als NEXT_ENV in den Frame
                FRAME_(frame_anz) = (object)count; # neue Zahl noch nicht genesteter Bindungen
              }
        } }}
      # mit diesem Nest-Teilschritt fertig.
      if (depth>0) goto nest_reentry; # Ende der Rekursion
      return env;
    }

# Macro: Legt fünf einzelne Environment auf den STACK
# und bildet daraus ein einzelnes Environment.
# make_STACK_env(venv,fenv,benv,genv,denv, env5 = );
# > object venv,fenv,benv,genv,denv: 5 einzelne Environments
# < environment* env5: Pointer auf im Stack liegendes Environment
  #ifdef STACK_UP
    #define make_STACK_env(venv,fenv,benv,genv,denv,env5_zuweisung)  \
      { pushSTACK(venv); pushSTACK(fenv); pushSTACK(benv); pushSTACK(genv); pushSTACK(denv); \
        env5_zuweisung &STACKblock_(environment,0);                                           \
      }
  #endif
  #ifdef STACK_DOWN
    #define make_STACK_env(venv,fenv,benv,genv,denv,env5_zuweisung)  \
      { pushSTACK(denv); pushSTACK(genv); pushSTACK(benv); pushSTACK(fenv); pushSTACK(venv); \
        env5_zuweisung &STACKblock_(environment,0);                                           \
      }
  #endif

# UP: Nestet die Environments in *env (d.h. schreibt alle Informationen in
# Stack-unabhängige Strukturen) und schiebt sie auf den STACK.
# (Die Werte VAR_ENV, FUN_ENV, BLOCK_ENV, GO_ENV, DECL_ENV werden nicht
# verändert, da evtl. noch inaktive Bindungen in Frames sitzen, die ohne
# Veränderung von VAR_ENV aktiviert werden können müssen.)
# nest_env(env)
# > environment* env: Pointer auf fünf einzelne Environments
# < environment* ergebnis: Pointer auf die Environments im STACK
# verändert STACK, kann GC auslösen
  global environment* nest_env (environment* env);
  global environment* nest_env(env5)
    var reg6 environment* env5;
    { # Erst alle Environments in den STACK kopieren:
      make_STACK_env(env5->var_env,env5->fun_env,env5->block_env,env5->go_env,env5->decl_env,
                     env5 = );
      # DECL_ENV: Nicht zu verändern.
      # GO_ENV:
      { var reg5 object env = env5->go_env;
        var reg9 uintL depth = 0; # Rekursionstiefe := 0
        # Pseudo-Rekursion: nestet ein GO_ENV.
        # Input: env, ein GO_ENV. Output: env, die Aliste dazu.
        nest_go_start: # Rekursionsbeginn
        if (typecode(env) == system_type)
          # env ist ein Pointer in den STACK auf einen ITAGBODY-Frame.
          { check_STACK();
           {var reg4 object* FRAME = TheFramepointer(env);
            if (mtypecode(FRAME_(0)) & bit(nested_bit_t)) # Frame schon genestet?
              { env = FRAME_(frame_next_env); } # ja -> bisherige Aliste holen
              else
              {  pushSTACK(env); # env retten
                 # entrekursiviert nest_go(NEXT_ENV(env)) durchführen:
                 env = FRAME_(frame_next_env); depth++; goto nest_go_start;
                 nest_go_reentry: depth--;
                 # NEXT_ENV ist nun genestet.
               { var reg8 object frame = STACK_0; # nächster zu nestender STACK-Frame
                 FRAME = uTheFramepointer(frame);
                 STACK_0 = env; # bisher genestetes Environment
                {var reg1 object* tagsptr = &FRAME_(frame_bindings); # Pointer aufs unterste Tag
                 var reg7 object* frame_end = STACKpointable(topofframe(FRAME_(0))); # Pointer übern Frame
                 var reg3 uintL count = # Anzahl der Tags
                   # Dazu die Pointer tagsptr und frame_end (beide ohne Typinfo!) abziehen:
                   STACK_item_count(tagsptr,frame_end) / 2;
                 # Vektor für count Tags erzeugen:
                 { var reg6 object tagvec = allocate_vector(count);
                   # und füllen:
                   { var reg2 object* ptr = &TheSvector(tagvec)->data[0];
                     # Tags ab tagsptr in den Vektor ab ptr eintragen:
                     dotimesL(count,count,
                       { *ptr++ = *(tagsptr STACKop 0);
                         tagsptr skipSTACKop 2;
                       });
                   }
                   pushSTACK(tagvec); # und retten
                 }
                 # Nächstes Alistencons (cons Tag-Vektor Frame-Pointer) erzeugen:
                 { var reg2 object new_cons = allocate_cons();
                   Car(new_cons) = STACK_0; # tagvec
                   Cdr(new_cons) = frame;
                   STACK_0 = new_cons;
                 }
                 # und vor die Aliste hängen:
                 env = allocate_cons();
                 Car(env) = popSTACK(); # new_cons
                 Cdr(env) = popSTACK(); # bisherige Aliste
                 FRAME_(frame_next_env) = env; # neues NEXT_ENV eintragen
                 *(oint*)(&FRAME_(0)) |= wbit(nested_bit_o); # Dieser Frame ist nun genestet.
              }}}
          }}
        # mit diesem Nest-Teilschritt fertig.
        if (depth>0) goto nest_go_reentry; # Ende der Rekursion
        env5->go_env = env; # genestetes GO_ENV ablegen
      }
      # BLOCK_ENV:
      { var reg2 object env = env5->block_env;
        var reg5 uintL depth = 0; # Rekursionstiefe := 0
        # Pseudo-Rekursion: nestet ein BLOCK_ENV.
        # Input: env, ein BLOCK_ENV. Output: env, die Aliste dazu.
        nest_block_start: # Rekursionsbeginn
        if (typecode(env) == system_type)
          # env ist ein Pointer in den STACK auf einen IBLOCK-Frame.
          { check_STACK();
           {var reg1 object* FRAME = TheFramepointer(env);
            if (mtypecode(FRAME_(0)) & bit(nested_bit_t)) # Frame schon genestet?
              { env = FRAME_(frame_next_env); } # ja -> bisherige Aliste holen
              else
              { pushSTACK(env); # env retten
                # entrekursiviert nest_block(NEXT_ENV(env)) durchführen:
                env = FRAME_(frame_next_env); depth++; goto nest_block_start;
                nest_block_reentry: depth--;
                # NEXT_ENV ist nun genestet.
               {var reg4 object frame = STACK_0; # nächster zu nestender STACK-Frame
                FRAME = TheFramepointer(frame);
                STACK_0 = env; # bisher genestetes Environment
                # Nächstes Alistencons (cons Block-Name Frame-Pointer) erzeugen:
                { var reg3 object new_cons = allocate_cons();
                  Car(new_cons) = FRAME_(frame_name);
                  Cdr(new_cons) = frame;
                  pushSTACK(new_cons);
                }
                # und vor die Aliste hängen:
                env = allocate_cons();
                Car(env) = popSTACK(); # new_cons
                Cdr(env) = popSTACK(); # bisherige Aliste
                FRAME_(frame_next_env) = env; # neues NEXT_ENV eintragen
                *(oint*)(&FRAME_(0)) |= wbit(nested_bit_o); # Dieser Frame ist nun genestet.
              }}
          }}
        # mit diesem Nest-Teilschritt fertig.
        if (depth>0) goto nest_block_reentry; # Ende der Rekursion
        env5->block_env = env; # genestetes BLOCK_ENV ablegen
      }
      # FUN_ENV:
      env5->fun_env = nest_fun(env5->fun_env);
      # VAR_ENV:
      env5->var_env = nest_var(env5->var_env);
      # fertig.
      return env5;
    }

# UP: Nestet die aktuellen Environments (d.h. schreibt alle Informationen in
# Stack-unabhängige Strukturen) und schiebt sie auf den STACK.
# (Die Werte VAR_ENV, FUN_ENV, BLOCK_ENV, GO_ENV, DECL_ENV werden nicht
# verändert, da evtl. noch inaktive Bindungen in Frames sitzen, die ohne
# Veränderung von VAR_ENV aktiviert werden können müssen.)
# nest_aktenv()
# < environment* ergebnis: Pointer auf die Environments im STACK
# verändert STACK, kann GC auslösen
  #define nest_aktenv()  nest_env(&aktenv)

# UP: Ergänzt ein Deklarations-Environment um ein decl-spec.
# augment_decl_env(declspec,env)
# > declspec: Deklarations-Specifier, ein Cons
# > env: Deklarations-Environment
# < ergebnis: neues (evtl. augmentiertes) Deklarations-Environment
# kann GC auslösen
  global object augment_decl_env (object new_declspec, object env);
  global object augment_decl_env(new_declspec,env)
    var reg6 object new_declspec;
    var reg5 object env;
    { var reg2 object decltype = Car(new_declspec); # Deklarations-Typ
      # Ist dies ein zu beachtender Deklarationstyp?
      # Gibt es in env ein Decl-Spec der Form (DECLARATION ... decltype ...) ?
      # NB: Die Liste O(declaration_types) ist das letzte Decl-Spec in env.
      if (symbolp(decltype))
        { # Alle lokal zu beachtenden Deklarations-Typen durchgehen:
          { var reg4 object declspecs = env;
            while (consp(declspecs)) # Alle declspecs aus env durchgehen
              { var reg3 object declspec = Car(declspecs);
                if (eq(Car(declspec),S(declaration))) # Deklaration (DECLARATION ...) ?
                  { var reg1 object list = Cdr(declspec); # ja -> restliche Liste durchgehen
                    while (consp(list))
                      { if (eq(Car(list),decltype)) # Listenelement = decltype ?
                          goto beachten;
                        list = Cdr(list);
                  }   }
                declspecs = Cdr(declspecs);
          }   }
        }
      # nicht zu beachtende Deklaration.
      return env; # env unverändert lassen
      beachten:
      # eine zu beachtende Deklaration -> env := (cons new_declspec env)
      pushSTACK(env); pushSTACK(new_declspec);
      env = allocate_cons();
      Car(env) = popSTACK(); Cdr(env) = popSTACK();
      return env;
    }

# UP: expandiert eine Form in einem Environment
# expand(form,expander,env);
# > form: Form
# > expander: Expanderfunktion
# > env: ein Funktions- und Macrobindungs-Environment
# < value1: die Expansion
# < value2: T, da expandiert wurde
# erhöht STACK um 1
# kann GC auslösen
  local void expand (object form, object expander, object env);
  local void expand(form,expander,env)
    var reg3 object form;
    var reg2 object expander;
    var reg1 object env;
    { # (FUNCALL *MACROEXPAND-HOOK* expander form env) ausführen:
      pushSTACK(expander); # Expander als erstes Argument
      pushSTACK(form); # Form als zweites Argument
      pushSTACK(nest_fun(env)); # genestetes Funktions- und Macrobindungs-Environment als drittes Argument
      funcall(Symbol_value(S(macroexpand_hook)),3);
      value2 = T; # expandierte Form als 1. Wert, T als 2. Wert
    }

# UP: expandiert eine Form, falls möglich, (nicht jedoch, wenn FSUBR-Aufruf)
# in einem Environment
# macroexp(form,env);
# > form: Form
# > env: ein Funktions- und Macrobindungs-Environment
# < value1: die Expansion
# < value2: NIL, wenn nicht expandiert,
#           T, wenn expandiert wurde
# kann GC auslösen
  global void macroexp (object form, object env);
  global void macroexp(form,env)
    var reg2 object form;
    var reg4 object env;
    { if (consp(form)) # nur Listen können Macro-call sein
        { var reg3 object funname = Car(form); # Funktionsname
          if (symbolp(funname))
            { var reg1 object fdef = sym_function(funname,env); # Funktionsdefinition holen
              # Ist sie (SYS::MACRO . Expander) ?
              if (consp(fdef) && eq(Car(fdef),S(macro)))
                # ja -> expandieren:
                { expand(form,Cdr(fdef),env); return; }
        }   }
      # sonst nicht expandieren:
      value1 = form; value2 = NIL;
    }

# UP: expandiert eine Form, falls möglich, (auch, wenn FSUBR-Aufruf)
# in einem Environment
# macroexp0(form,env);
# > form: Form
# > env: ein Funktions- und Macrobindungs-Environment
# < value1: die Expansion
# < value2: NIL, wenn nicht expandiert,
#           T, wenn expandiert wurde
# kann GC auslösen
  global void macroexp0 (object form, object env);
  global void macroexp0(form,env)
    var reg4 object form;
    var reg6 object env;
    { if (consp(form)) # nur Listen können Macro-call sein
        { var reg5 object funname = Car(form); # Funktionsname
          if (symbolp(funname))
            { var reg3 object fdef = sym_function(funname,env); # Funktionsdefinition holen
              if (fsubrp(fdef))
                # fdef ist ein FSUBR, also war die globale Funktionsdefinition gültig.
                # Schaue nach, ob die Propertyliste eine Macrodefinition enthält:
                { var reg1 object expander = get(funname,S(macro)); # nach Property SYS::MACRO suchen
                  if (!eq(expander,unbound))
                    # gefunden. Mit dem Expander aus der Propertyliste expandieren:
                    { expand(form,expander,env); return; }
                }
                else
                # 3 Möglichkeiten:
                # #UNBOUND/SUBR/Closure (globale oder lexikalische Funktionsdef.)
                #   -> nicht expandieren
                # (SYS::MACRO . Expander) (lexikalische Macrodefinition)
                #   -> expandieren (Expander aufrufen)
                # Symbol (lexikalische Funktionsdefinition während SYS::%EXPAND)
                #   expandieren: (list* 'SYS::%FUNCALL Symbol (cdr form))
                if (consp(fdef))
                  { # Ist es (SYS::MACRO . Expander) ?
                    if (eq(Car(fdef),S(macro)))
                      # ja -> expandieren:
                      { expand(form,Cdr(fdef),env); return; }
                  }
                elif (symbolp(fdef))
                  # fdef ein Symbol
                  { # Muß zu (SYS::%FUNCALL fdef ...) expandieren:
                    pushSTACK(Cdr(form)); # (cdr form)
                    pushSTACK(fdef); # Symbol
                   {var reg1 object new_cons = allocate_cons();
                    Car(new_cons) = popSTACK(); Cdr(new_cons) = STACK_0;
                    STACK_0 = new_cons; # (cons Symbol (cdr form))
                   }
                   {var reg1 object new_cons = allocate_cons();
                    Car(new_cons) = S(pfuncall); Cdr(new_cons) = popSTACK();
                    value1 = new_cons; # (cons 'SYS::%FUNCALL (cons Symbol (cdr form)))
                   }
                    value2 = T; return; # es wurde expandiert.
                  }
        }   }
      # sonst nicht expandieren:
      value1 = form; value2 = NIL;
    }

# UP: Parse-Declarations-Docstring. Trennt von einer Formenliste diejenigen
# ab, die als Deklarationen bzw. Dokumentationsstring angesehen werden
# müssen.
# parse_dd(formlist,env)
# > formlist: ( {decl|doc-string} . body )
# > env: Funktions- und Macrobindungs-Environment (für die Macroexpansionen)
# < value1: body
# < value2: Liste der decl-specs
# < value3: Doc-String oder NIL
# < ergebnis: TRUE falls eine (COMPILE)-Deklaration vorkam, FALSE sonst
# kann GC auslösen
  global boolean parse_dd (object formlist, object env);
  global boolean parse_dd(formlist,env)
    var reg7 object formlist;
    var reg5 object env;
    { pushSTACK(formlist); # formlist aufheben für Fehlermeldung
      pushSTACK(env); # Macrobindungs-Environment
      pushSTACK(NIL); # vorläufiger Doc-String
      pushSTACK(NIL); # Anfang decl-spec-Liste
      # Stackaufbau: formlist, env, docstring, declspecs.
     {var reg6 boolean compile_decl = FALSE; # Flag, ob eine (COMPILE)-Deklaration vorkam
      var reg2 object body = formlist; # Rest der Formenliste
      while (consp(body))
        {  pushSTACK(body); # body retten
         { var reg1 object form = Car(body); # nächste Form
           # evtl. macroexpandieren (ohne FSUBRs zu expandieren):
           do { macroexp(form,STACK_(2+1)); form = value1; }
              until (nullp(value2));
           body = popSTACK();
          {var reg4 object body_rest = Cdr(body); # body verkürzen
           if (stringp(form)) # Doc-String gefunden?
             { if (atomp(body_rest)) # an letzter Stelle der Formenliste?
                 goto fertig; # ja -> letzte Form kann kein Doc-String sein!
               if (!nullp(STACK_1)) # schon ein Doc-String dagewesen?
                 # ja -> mehr als ein Doc-String ist zuviel:
                 { pushSTACK(STACK_3); # formlist
                   fehler(
                          DEUTSCH ? "In ~ kommen zu viele Doc-Strings vor." :
                          ENGLISH ? "Too many documentation strings in ~" :
                          FRANCAIS ? "Trop de chaînes de documentation dans ~." :
                          ""
                         );
                 }
               STACK_1 = form; # neuer Doc-String
               body = body_rest;
             }
           elif (consp(form) && eq(Car(form),S(declare))) # Deklaration (DECLARE ...) ?
             { # neue decl-specs einzeln auf STACK_0 consen:
               pushSTACK(body_rest); # body_rest retten
               pushSTACK(Cdr(form)); # Liste der neuen decl-specs
               while (mconsp(STACK_0))
                 {{var reg3 object declspec = Car(STACK_0); # nächstes decl-spec
                   # Teste, ob (EQUAL d '(COMPILE)) =
                   #   (and (consp d) (eq (car d) 'COMPILE) (null (cdr d)))
                   if (consp(declspec)
                       && eq(Car(declspec),S(compile))
                       && nullp(Cdr(declspec))
                      )
                     { compile_decl = TRUE; }
                  }# Diese Deklaration auf STACK_(0+2) consen:
                  {var reg3 object new_cons = allocate_cons();
                   Car(new_cons) = Car(STACK_0);
                   Cdr(new_cons) = STACK_(0+2);
                   STACK_(0+2) = new_cons;
                  }# zum nächsten decl-spec:
                   STACK_0 = Cdr(STACK_0);
                 }
               skipSTACK(1);
               body = popSTACK(); # body := alter body_rest
             }
           else
             { fertig: # fertig mit Durchlaufen der Formenliste
               if (!eq(form,Car(body))) # sofern die Form expandiert wurde,
                 # ersetze body durch (cons form (cdr body)) :
                 { pushSTACK(body_rest); pushSTACK(form);
                   body = allocate_cons();
                   Car(body) = popSTACK(); # form
                   Cdr(body) = popSTACK(); # body_rest
                 }
               break;
             }
        }}}
      value1 = body;
      value2 = nreverse(popSTACK()); # decl-spec-Liste
      value3 = popSTACK(); # Doc-String
      skipSTACK(2);
      return compile_decl;
    }}

# UP: bindet *EVALHOOK* und *APPLYHOOK* dynamisch an die gegebenen Werte.
# bindhooks(evalhook_value,applyhook_value);
# > evalhook_value: Wert für *EVALHOOK*
# > applyhook_value: Wert für *APPLYHOOK*
# verändert STACK
  global void bindhooks (object evalhook_value, object applyhook_value);
  global void bindhooks(evalhook_value,applyhook_value)
    var reg2 object evalhook_value;
    var reg3 object applyhook_value;
    { # Frame aufbauen:
      { var reg1 object* top_of_frame = STACK; # Pointer übern Frame
        pushSTACK(Symbol_value(S(evalhookstern)));  # alter Wert von *EVALHOOK*
        pushSTACK(S(evalhookstern));                # *EVALHOOK*
        pushSTACK(Symbol_value(S(applyhookstern))); # alter Wert von *APPLYHOOK*
        pushSTACK(S(applyhookstern));               # *APPLYHOOK*
        finish_frame(DYNBIND);
      }
      # Frame fertig aufgebaut, nun die Werte der Variablen verändern:
      Symbol_value(S(evalhookstern)) = evalhook_value; # (SETQ *EVALHOOK* evalhook_value)
      Symbol_value(S(applyhookstern)) = applyhook_value; # (SETQ *APPLYHOOK* applyhook_value)
    }

# UP: bindet *EVALHOOK* und *APPLYHOOK* dynamisch an NIL.
# bindhooks_NIL();
# verändert STACK
  #define bindhooks_NIL()  bindhooks(NIL,NIL)

# UP: Bestimmt den Source-Lambdabody eines Lambdabody.
# lambdabody_source(lambdabody)
# > lambdabody: Lambdabody (ein Cons)
# < ergebnis: Source-Lambdabody (unbound falls keine Source angegeben)
  local object lambdabody_source (object lambdabody);
  local object lambdabody_source(lambdabody)
    var reg3 object lambdabody;
    { var reg2 object body = Cdr(lambdabody);
      # body = ((DECLARE (SOURCE ...) ...) ...) ?
      if (consp(body))
        { var reg1 object form = Car(body); # erste Form
          # form = (DECLARE (SOURCE ...) ...) ?
          if (consp(form) && eq(Car(form),S(declare)))
            { var reg6 object declspecs = Cdr(form);
              # declspecs = ((SOURCE ...) ...) ?
              if (consp(declspecs))
                { var reg5 object declspec = Car(declspecs);
                  # declspec = (SOURCE ...) ?
                  if (consp(declspec) && eq(Car(declspec),S(source)))
                    { var reg4 object declspecr = Cdr(declspec);
                      if (consp(declspecr))
                        # Source gefunden
                        { return Car(declspecr); }
        }   }   }   }
      return unbound;
    }

# UP: Erzeugt zu einem Lambdabody die entsprechende Closure durch Zerlegen
# der Lambdaliste und eventuelles Macroexpandieren aller Formen.
# get_closure(lambdabody,name,env)
# > lambdabody: (lambda-list {decl|doc} {form})
# > name: Name (ein Symbol)
# > env: Pointer auf die fünf einzelnen Environments:
#        env->var_env = VENV, env->fun_env = FENV,
#        env->block_env = BENV, env->go_env = GENV,
#        end->decl_env = DENV.
# < ergebnis: Closure
# kann GC auslösen
  global object get_closure (object lambdabody, object name, environment* env);
  global object get_closure(lambdabody,name,env)
    var reg10 object lambdabody;
    var reg10 object name;
    var reg10 environment* env;
    { # Lambdabody muß ein Cons sein:
      if (atomp(lambdabody))
        { pushSTACK(name);
          fehler(
                 DEUTSCH ? "FUNCTION: Lambda-Liste für ~ fehlt." :
                 ENGLISH ? "FUNCTION: lambda-list for ~ is missing" :
                 FRANCAIS ? "FUNCTION: La liste lambda pour ~ manque." :
                 ""
                );
        }
      # und der CAR muß eine Liste sein:
      {var reg1 object lambdalist = Car(lambdabody);
       if (!listp(lambdalist))
         { pushSTACK(lambdalist);
           pushSTACK(name);
           fehler(
                  DEUTSCH ? "FUNCTION: Lambda-Liste für ~ muß eine Liste sein, nicht ~" :
                  ENGLISH ? "FUNCTION: lambda-list for ~ should be a list, not ~" :
                  FRANCAIS ? "FUNCTION: La liste lambda pour ~ doit être une liste et non ~" :
                  ""
                 );
      }  }
      pushSTACK(name);
      pushSTACK(lambdabody);
      # Stackaufbau: name, lambdabody.
      if (parse_dd(Cdr(lambdabody),env->fun_env)) # ({decl|doc} {form}) zerlegen
        # Es trat eine (COMPILE)-Deklaration auf.
        { # Lambdabody durch seine Source ersetzen (denn manche Macros
          # können effizienter compiliert werden als ihre Macro-Expansion):
          { var reg1 object source = lambdabody_source(STACK_0);
            if (!eq(source,unbound)) { STACK_0 = source; }
          }
          # Environments nesten:
          { var reg1 environment* stack_env = nest_env(env); # nesten, auf den STACK legen
            #if !defined(STACK_UP)
            var environment my_env;
            my_env = *stack_env; # und hierher übertragen
            skipSTACK(5); # und wieder vom STACK nehmen
            pushSTACK(my_env.var_env);
            pushSTACK(my_env.fun_env);
            pushSTACK(my_env.block_env);
            pushSTACK(my_env.go_env);
            pushSTACK(my_env.decl_env);
            #endif
            # Stackaufbau: name, lambdabody, venv, fenv, benv, genv, denv.
          }
          # (SYS::COMPILE-LAMBDA name lambdabody venv fenv benv genv denv) ausführen:
          funcall(S(compile_lambda),7);
          return value1; # compilierte Closure als Wert
        }
      # Interpretierte Closure bauen:
      { var reg1 object source = lambdabody_source(STACK_0);
        if (eq(source,unbound))
          # keine Source angegeben -> Lambdabody expandieren:
          { # (SYS::%EXPAND-LAMBDABODY-MAIN lambdabody fenv) aufrufen:
            pushSTACK(STACK_0); # Lambdabody als 1. Argument
            pushSTACK(nest_fun(env->fun_env)); # Funktions-Environment genestet als 2. Argument
            funcall(S(expand_lambdabody_main),2);
            lambdabody = value1; # expandierter Lambdabody
          }
          else
          # Source angegeben -> sie ersetzt den alten Lambdabody:
          { lambdabody = STACK_0; # Lambdabody
            STACK_0 = source; # Source-Lambdabody
          }
      }
      # Nun ist  STACK_0     der Source-Lambdabody,
      #          lambdabody  der zu verwendende Lambdabody.
      pushSTACK(Car(lambdabody)); # Lambdaliste
      parse_dd(Cdr(lambdabody),env->fun_env); # ({decl|doc} {form}) zerlegen
      pushSTACK(value1); # Body
      pushSTACK(value2); # Deklarationen
      pushSTACK(value3); # Doc-String oder NIL
     {var reg3 object* closure_; # Pointer auf die Closure im STACK
      # Closure erzeugen (mit NIL gefüllt):
      {  var reg1 object closure = allocate_record(0,0,iclos_length,closure_type);
         # und teilweise füllen:
         TheIclosure(closure)->clos_docstring = popSTACK(); # Doc-String
       { var reg5 object declarations         = popSTACK(); # Deklarationen
         TheIclosure(closure)->clos_body      = popSTACK(); # Body
        {var reg4 object lambdalist           = popSTACK(); # Lambda-Liste
         TheIclosure(closure)->clos_form      = popSTACK(); # Source-Lambdabody
         TheIclosure(closure)->clos_name      = STACK_0;    # Name
         # und retten:
         STACK_0 = closure;
         # Stackaufbau: closure.
         closure_ = &STACK_0; # Pointer auf die Closure im STACK
         pushSTACK(lambdalist); pushSTACK(declarations);
      }}}
      # Environments nesten und genestet in die Closure stecken:
      {var reg1 environment* stack_env = nest_env(env);
       var reg2 object closure = *closure_;
       TheIclosure(closure)->clos_var_env   = stack_env->var_env  ;
       TheIclosure(closure)->clos_fun_env   = stack_env->fun_env  ;
       TheIclosure(closure)->clos_block_env = stack_env->block_env;
       TheIclosure(closure)->clos_go_env    = stack_env->go_env   ;
       TheIclosure(closure)->clos_decl_env  = stack_env->decl_env ;
       skipSTACK(5);
       TheIclosure(closure)->clos_keywords = Fixnum_0; # keywords:=0, solange &KEY fehlt
      }
      # Stackaufbau: closure, lambdalist, declarations.
      {var reg10 uintL spec_count = 0; # Anzahl der dynamischen Referenzen
       var reg10 uintL req_count  = 0; # Anzahl der required-Parameter
       var reg10 uintL opt_count  = 0; # Anzahl der optional-Parameter
       var reg10 uintL key_count  = 0; # Anzahl der Keyword-Parameter
       var reg10 uintL aux_count  = 0; # Anzahl der &AUX-Variablen
       var reg9  uintL var_count  = 0; # Gesamtzahl der auf dem STACK liegenden Variablen
       {var reg4 object declarations = popSTACK();
        # Deklarationen verarbeiten:
        # Dynamisch referenzierte Variablen aus der decl-spec-Liste declarations
        # herauslesen und auf dem STACK ablegen. Sonstige zu beachtende
        # Deklarationen verändern das Deklarations-Environment der Closure.
        while (consp(declarations)) # alle decl-specs abgearbeitet?
          { var reg1 object declspec = Car(declarations);
            # declspec muß Liste sein:
            if (atomp(declspec))
              { pushSTACK(declspec);
                fehler(
                       DEUTSCH ? "FUNCTION: ~ ist keine erlaubte Deklaration." :
                       ENGLISH ? "FUNCTION: illegal declaration ~" :
                       FRANCAIS ? "FUNCTION: ~ n'est pas une déclaration licite." :
                       ""
                      );
              }
            # SPECIAL-Deklaration verarbeiten:
            if (eq(Car(declspec),S(special))) # SPECIAL-Deklaration ?
              { declspec = Cdr(declspec);
                while (consp(declspec))
                  { var reg2 object sym = Car(declspec);
                    if (!symbolp(sym))
                      { pushSTACK(sym);
                        fehler(
                               DEUTSCH ? "FUNCTION: ~ ist kein Symbol, wurde aber als SPECIAL deklariert." :
                               ENGLISH ? "FUNCTION: ~ is not a symbol, cannot be declared SPECIAL" :
                               FRANCAIS ? "FUNCTION: ~ n'est pas un symbôle mais fut déclaré SPECIAL." :
                               ""
                              );
                      }
                    # Symbol im STACK ablegen:
                    check_STACK(); pushSTACK(sym); spec_count++; var_count++;
                    declspec = Cdr(declspec);
              }   }
            # sonstige Deklaration verarbeiten:
            pushSTACK(Cdr(declarations)); # declarations verkürzen und retten
            {var reg2 object denv = TheIclosure(*closure_)->clos_decl_env;
             denv = augment_decl_env(declspec,denv);
             TheIclosure(*closure_)->clos_decl_env = denv;
            }
            declarations = popSTACK();
       }  }
       {var reg2 object lambdalist = *(closure_ STACKop -1); # restliche Lambdaliste
        var reg1 object item; # Element der Lambdaliste
        # Macro:
        # NEXT_ITEM(&OPTIONAL_label,&REST_label,&KEY_label,
        #           &ALLOW-OTHER-KEYS_label,&AUX_label,Ende_label)
        # verkürzt den Lambdalistenrest, bringt das nächste Element nach item
        # und springt im Falle eines der 6 angegebenen Lambdalistenmarker an
        # die entsprechenden Stellen.
          #define NEXT_ITEM(opt_label,rest_label,key_label,allow_label,aux_label,end_label)  \
            { if (atomp(lambdalist)) goto end_label; # Lambda-Liste zu Ende?              \
              item = Car(lambdalist); # nächstes Element                                  \
              lambdalist = Cdr(lambdalist); # Liste verkürzen                             \
              if (eq(item,S(LLoptional)))         goto opt_label;   # &OPTIONAL ?         \
              if (eq(item,S(LLrest)))             goto rest_label;  # &REST ?             \
              if (eq(item,S(LLkey)))              goto key_label;   # &KEY ?              \
              if (eq(item,S(LLallow_other_keys))) goto allow_label; # &ALLOW-OTHER-KEYS ? \
              if (eq(item,S(LLaux)))              goto aux_label;   # &AUX ?              \
            }
        req: # required-Parameter abarbeiten und auf dem STACK ablegen:
        loop
          { NEXT_ITEM(opt,rest,key,badLLkey,aux,ende);
            if (!symbolp(item)) goto fehler_symbol;
            if (constantp(TheSymbol(item))) goto fehler_constant;
            # Variable im STACK ablegen:
            check_STACK();
            pushSTACK(item); pushSTACK(Fixnum_0); req_count++; var_count++;
          }
        opt: # &OPTIONAL-Parameter abarbeiten, auf dem STACK ablegen und
             # Init-Formen in die Closure stecken:
        loop
          { NEXT_ITEM(badLLkey,rest,key,badLLkey,aux,ende);
           {var reg7 object init_form;
            # Parse Variablenspezifikation in item:
            #   var  oder  (var [init [svar]])
            # Lege var und evtl. svar auf den STACK, setze in var evtl.
            # das svar_bit. Liefert auch init (oder NIL) in init_form.
            check_STACK();
            if (atomp(item))
              { if (!symbolp(item)) goto fehler_symbol;
                if (constantp(TheSymbol(item))) goto fehler_constant;
                # Variable im STACK ablegen:
                pushSTACK(item); pushSTACK(Fixnum_0); opt_count++; var_count++;
                init_form = NIL; # Default-Init
              }
              else
              { var reg4 object item_rest = Cdr(item);
                item = Car(item); # erstes Listenelement: var
                if (!symbolp(item)) goto fehler_symbol;
                if (constantp(TheSymbol(item))) goto fehler_constant;
                # Variable im STACK ablegen:
                pushSTACK(item); pushSTACK(Fixnum_0); opt_count++; var_count++;
                if (consp(item_rest))
                  { init_form = Car(item_rest); # zweites Listenelement: init
                    item_rest = Cdr(item_rest);
                    if (consp(item_rest))
                      { if (mconsp(Cdr(item_rest)))
                          # varspec ist zu lang
                          { pushSTACK(*(closure_ STACKop -1)); # ganze Lambda-Liste
                            fehler(
                                   DEUTSCH ? "FUNCTION: Zu lange Variablenspezifikation nach &OPTIONAL: ~" :
                                   ENGLISH ? "FUNCTION: too long variable specification after &OPTIONAL: ~" :
                                   FRANCAIS ? "FUNCTION: Spécification de variable trop longue après &OPTIONAL : ~" :
                                   ""
                                  );
                          }
                        item = Car(item_rest); # drittes Listenelement: svar
                        if (!symbolp(item)) goto fehler_symbol;
                        if (constantp(TheSymbol(item))) goto fehler_constant;
                        # svar-Bit für var setzen:
                        STACK_0 = fixnum_inc(STACK_0,bit(svar_bit));
                        # Variable im STACK ablegen:
                        pushSTACK(item); pushSTACK(Fixnum_0); var_count++;
                  }   }
                  else
                  { init_form = NIL; } # Default-Init
              }
            # init_form vor (clos_opt_inits closure) pushen:
            pushSTACK(lambdalist); pushSTACK(init_form);
            { var reg5 object new_cons = allocate_cons();
              Car(new_cons) = popSTACK();
             {var reg6 object closure = *closure_;
              Cdr(new_cons) = TheIclosure(closure)->clos_opt_inits;
              TheIclosure(closure)->clos_opt_inits = new_cons;
            }}
            lambdalist = popSTACK();
          }}
        rest: # &REST-Parameter abarbeiten und auf dem Stack ablegen:
        { NEXT_ITEM(badrest,badrest,badrest,badrest,badrest,badrest);
          if (!symbolp(item)) goto fehler_symbol;
          if (constantp(TheSymbol(item))) goto fehler_constant;
          # Variable im STACK ablegen:
          pushSTACK(item); pushSTACK(Fixnum_0); var_count++;
          # Rest-Flag auf T setzen:
          TheIclosure(*closure_)->clos_rest_flag = T;
        }
        { NEXT_ITEM(badLLkey,badLLkey,key,badLLkey,aux,ende);
          pushSTACK(*(closure_ STACKop -1)); # ganze Lambda-Liste
          fehler(
                 DEUTSCH ? "FUNCTION: Nach &REST var muß &KEY oder &AUX oder Listenende folgen: ~" :
                 ENGLISH ? "FUNCTION: &REST var must be followed by &KEY or &AUX or end of list: ~" :
                 FRANCAIS ? "FUNCTION: &KEY, &AUX ou fin de liste doit suivre une variable &REST : ~." :
                 ""
                );
        }
        badrest:
          pushSTACK(*(closure_ STACKop -1)); # ganze Lambda-Liste
          fehler(
                 DEUTSCH ? "FUNCTION: Nach &REST muß Variable folgen: ~" :
                 ENGLISH ? "FUNCTION: &REST must be followed by a variable: ~" :
                 FRANCAIS ? "FUNCTION: Une variable doit suivre &REST : ~" :
                 ""
                );
        key: # &KEY-Parameter abarbeiten, auf dem STACK ablegen
             # und Init-Formen in die Closure stecken:
        TheIclosure(*closure_)->clos_keywords = NIL; # keywords:=NIL
        loop
          { NEXT_ITEM(badLLkey,badLLkey,badLLkey,allow,aux,ende);
           {var reg8 object keyword;
            var reg7 object init_form;
            # Parse Variablenspezifikation in item:
            #   var  oder  (var [init [svar]])  oder ((key var) [init [svar]])
            # Lege var und evtl. svar auf den STACK, setze in var evtl.
            # das svar_bit. Liefert auch das Keyword in keyword und
            # init (oder NIL) in init_form.
            check_STACK();
            if (atomp(item))
              { if (!symbolp(item)) goto fehler_symbol;
                if (constantp(TheSymbol(item))) goto fehler_constant;
                # Variable im STACK ablegen:
                pushSTACK(item); pushSTACK(Fixnum_0); key_count++; var_count++;
                # Keyword holen:
                pushSTACK(lambdalist);
                keyword = intern_keyword(Symbol_name(item));
                lambdalist = popSTACK();
                # Default-Init:
                init_form = NIL;
              }
              else
              { var reg4 object item_rest = Cdr(item); # ([init [svar]])
                item = Car(item); # erstes Listenelement: var oder (key var)
                if (atomp(item))
                  # item = var
                  { if (!symbolp(item)) goto fehler_symbol;
                    if (constantp(TheSymbol(item))) goto fehler_constant;
                    # Variable im STACK ablegen:
                    pushSTACK(item); pushSTACK(Fixnum_0); key_count++; var_count++;
                    # Keyword holen:
                    pushSTACK(item_rest); pushSTACK(lambdalist);
                    keyword = intern_keyword(Symbol_name(item));
                    lambdalist = popSTACK(); item_rest = popSTACK();
                  }
                  else
                  # item = (key var)
                  { keyword = Car(item); # key
                    # sollte ein Keyword sein:
                    if (!(symbolp(keyword) && keywordp(keyword)))
                      { pushSTACK(*(closure_ STACKop -1)); # ganze Lambda-Liste
                        pushSTACK(keyword);
                        fehler(
                               DEUTSCH ? "FUNCTION: ~ in ~ ist kein Keyword." :
                               ENGLISH ? "FUNCTION: ~ in ~ is not a keyword" :
                               FRANCAIS ? "FUNCTION: ~ dans ~ n'est pas un mot-clé." :
                               ""
                              );
                      }
                    item = Cdr(item); # (var)
                    if (!(consp(item) && matomp(Cdr(item))))
                      goto fehler_keyspec;
                    item = Car(item); # var
                    if (!symbolp(item)) goto fehler_symbol;
                    if (constantp(TheSymbol(item))) goto fehler_constant;
                    # Variable im STACK ablegen:
                    pushSTACK(item); pushSTACK(Fixnum_0); key_count++; var_count++;
                  }
                if (consp(item_rest))
                  { init_form = Car(item_rest); # zweites Listenelement: init
                    item_rest = Cdr(item_rest); # ([svar])
                    if (consp(item_rest))
                      { if (mconsp(Cdr(item_rest))) goto fehler_keyspec;
                        item = Car(item_rest); # drittes Listenelement: svar
                        if (!symbolp(item)) goto fehler_symbol;
                        if (constantp(TheSymbol(item))) goto fehler_constant;
                        # svar-Bit in var setzen:
                        STACK_0 = fixnum_inc(STACK_0,bit(svar_bit));
                        # Variable im STACK ablegen:
                        pushSTACK(item); pushSTACK(Fixnum_0); var_count++;
                  }   }
                  else
                  { init_form = NIL; } # Default-Init
              }
            # keyword vor (clos_keywords closure) pushen und
            # init_form vor (clos_key_inits closure) pushen:
            pushSTACK(lambdalist); pushSTACK(init_form); pushSTACK(keyword);
            { var reg5 object new_cons = allocate_cons();
              Car(new_cons) = popSTACK();
             {var reg6 object closure = *closure_;
              Cdr(new_cons) = TheIclosure(closure)->clos_keywords;
              TheIclosure(closure)->clos_keywords = new_cons;
            }}
            { var reg5 object new_cons = allocate_cons();
              Car(new_cons) = popSTACK();
             {var reg6 object closure = *closure_;
              Cdr(new_cons) = TheIclosure(closure)->clos_key_inits;
              TheIclosure(closure)->clos_key_inits = new_cons;
            }}
            lambdalist = popSTACK();
          }}
        fehler_keyspec:
          pushSTACK(*(closure_ STACKop -1)); # ganze Lambda-Liste
          fehler(
                 DEUTSCH ? "FUNCTION: Variablenspezifikation nach &KEY ist nicht korrekt: ~" :
                 ENGLISH ? "FUNCTION: incorrect variable specification after &KEY: ~" :
                 FRANCAIS ? "FUNCTION: Spécification de variable incorrecte après &KEY : ~" :
                 ""
                );
        allow: # &ALLOW-OTHER-KEYS abarbeiten:
        { TheIclosure(*closure_)->clos_allow_flag = T; # Flag auf T setzen
          NEXT_ITEM(badLLkey,badLLkey,badLLkey,badLLkey,aux,ende);
          pushSTACK(*(closure_ STACKop -1)); # ganze Lambda-Liste
          fehler(
                 DEUTSCH ? "FUNCTION: Auf &ALLOW-OTHER-KEYS muß &AUX oder Listenende folgen: ~" :
                 ENGLISH ? "FUNCTION: &ALLOW-OTHER-KEYS must be followed by &AUX or end of list: ~" :
                 FRANCAIS ? "FUNCTION: &AUX ou fin de liste doit suivre &ALLOW-OTHER-KEYS : ~" :
                 ""
                );
        }
        aux: # &AUX-Parameter abarbeiten, auf dem STACK ablegen und
             # Init-Formen in die Closure stecken:
        loop
          { NEXT_ITEM(badLLkey,badLLkey,badLLkey,badLLkey,badLLkey,ende);
           {var reg7 object init_form;
            # Parse Variablenspezifikation in item:
            #   var  oder  (var [init])
            # Lege var auf den STACK.
            # Liefert auch init (oder NIL) in init_form.
            check_STACK();
            if (atomp(item))
              { if (!symbolp(item)) goto fehler_symbol;
                if (constantp(TheSymbol(item))) goto fehler_constant;
                # Variable im STACK ablegen:
                pushSTACK(item); pushSTACK(Fixnum_0); aux_count++; var_count++;
                init_form = NIL; # Default-Init
              }
              else
              { var reg4 object item_rest = Cdr(item);
                item = Car(item); # erstes Listenelement: var
                if (!symbolp(item)) goto fehler_symbol;
                if (constantp(TheSymbol(item))) goto fehler_constant;
                # Variable im STACK ablegen:
                pushSTACK(item); pushSTACK(Fixnum_0); aux_count++; var_count++;
                if (consp(item_rest))
                  { init_form = Car(item_rest); # zweites Listenelement: init
                    if (mconsp(Cdr(item_rest)))
                      # varspec ist zu lang
                      { pushSTACK(*(closure_ STACKop -1)); # ganze Lambda-Liste
                        fehler(
                               DEUTSCH ? "FUNCTION: Zu lange Variablenspezifikation nach &AUX: ~" :
                               ENGLISH ? "FUNCTION: too long variable specification after &AUX: ~" :
                               FRANCAIS ? "FUNCTION: Spécification de variable trop longue après &AUX : ~" :
                               ""
                              );
                  }   }
                  else
                  { init_form = NIL; } # Default-Init
              }
            # init_form vor (clos_aux_inits closure) pushen:
            pushSTACK(lambdalist); pushSTACK(init_form);
            { var reg5 object new_cons = allocate_cons();
              Car(new_cons) = popSTACK();
             {var reg6 object closure = *closure_;
              Cdr(new_cons) = TheIclosure(closure)->clos_aux_inits;
              TheIclosure(closure)->clos_aux_inits = new_cons;
            }}
            lambdalist = popSTACK();
          }}
        # Gesammelte Fehlermeldungen:
        badLLkey:
          pushSTACK(*(closure_ STACKop -1)); # ganze Lambda-Liste
          pushSTACK(item);
          fehler(
                 DEUTSCH ? "FUNCTION: Lambda-Listen-Keyword ~ an der falschen Stelle: ~" :
                 ENGLISH ? "FUNCTION: badly placed lambda-list keyword ~: ~" :
                 FRANCAIS ? "FUNCTION: Mot clé de liste lambda ~ mal placé : ~" :
                 ""
                );
        fehler_symbol:
          pushSTACK(item);
          fehler(
                 DEUTSCH ? "FUNCTION: ~ ist kein Symbol und kann daher nicht als Variable verwendet werden." :
                 ENGLISH ? "FUNCTION: ~ is not a symbol, may not be used as a variable" :
                 FRANCAIS ? "FUNCTION: ~ n'est pas un symbole et ne peut donc pas être utilisé comme variable." :
                 ""
                );
        fehler_constant:
          pushSTACK(item);
          fehler(
                 DEUTSCH ? "FUNCTION: ~ ist eine Konstante und kann daher nicht als Variable verwendet werden." :
                 ENGLISH ? "FUNCTION: ~ is a constant, may not be used as a variable" :
                 FRANCAIS ? "FUNCTION: ~ est une constante et ne peut donc pas être utilisée comme variable." :
                 ""
                );
        ende: # Listenende erreicht
        #undef NEXT_ITEM
        if (((uintL)~(uintL)0 > lp_limit_1) && (var_count > lp_limit_1)) # Zu viele Parameter?
          { pushSTACK(*(closure_ STACKop -1)); # ganze Lambda-Liste
            fehler(
                   DEUTSCH ? "FUNCTION: Zu viele Parameter in der Lambda-Liste ~" :
                   ENGLISH ? "FUNCTION: too many parameters in the lambda-list ~" :
                   FRANCAIS ? "FUNCTION: Trop de paramètres dans la liste lambda ~" :
                   ""
                  );
          }
        # Da nun var_count <= lp_limit_1, passen alle counts in ein uintC.
        if (!nullp(lambdalist)) # Lambda-Liste eine Dotted List?
          { pushSTACK(*(closure_ STACKop -1)); # ganze Lambda-Liste
            fehler(
                   DEUTSCH ? "FUNCTION: Ein Punkt in der Lambda-Liste ist nur bei Macros erlaubt, nicht hier: ~" :
                   ENGLISH ? "FUNCTION: a dot in a lambda-list is allowed only for macros, not here: ~" :
                   FRANCAIS ? "FUNCTION: Un point dans une liste lambda n'est permis que pour des macros, pas ici : ~" :
                   ""
                  );
          }
        # Variablen zu einem Vektor zusammenfassen und in die Closure,
        # Variablenflags zu einem Byte-Vektor zusammenfassen und in die Closure:
        pushSTACK(allocate_bit_vector(intBsize*(var_count-spec_count))); # Byte-Vektor erzeugen
        { var reg8 object vars = allocate_vector(var_count); # Vektor erzeugen
          var reg8 object varflags = popSTACK();
          # Variablen in den Vektor schreiben (letzte hinten, erste vorne):
          { var reg4 object* ptr = &TheSvector(vars)->data[var_count];
            var reg5 uintB* ptrflags = &TheSbvector(varflags)->data[var_count-spec_count];
            var reg6 uintC count;
            dotimesC(count,var_count-spec_count,
              { *--ptrflags = (uintB)posfixnum_to_L(popSTACK());
                *--ptr = popSTACK();
              });
            dotimesC(count,spec_count, { *--ptr = popSTACK(); } );
          }
         {var reg4 object closure = *closure_;
          TheIclosure(closure)->clos_vars     = vars;
          TheIclosure(closure)->clos_varflags = varflags;
        # Anzahlen in die Closure eintragen:
          TheIclosure(closure)->clos_spec_anz = fixnum(spec_count);
          TheIclosure(closure)->clos_req_anz  = fixnum(req_count);
          TheIclosure(closure)->clos_opt_anz  = fixnum(opt_count);
          TheIclosure(closure)->clos_key_anz  = fixnum(key_count);
          TheIclosure(closure)->clos_aux_anz  = fixnum(aux_count);
        # Im Variablen-Vektor sind die ersten spec_count Variablen die
        # SPECIAL-Deklarierten. In jeder übrigen Variablen wird das DYNAM_BIT
        # gesetzt, falls sie unter den SPECIAL-deklarierten vorkommt.
          if (!(spec_count==0))
            { # Schleife über die übrigen Variablen:
              var reg9 object* othervarptr = &TheSvector(vars)->data[spec_count];
              var reg9 uintB* othervarflagsptr = &TheSbvector(varflags)->data[0];
              var reg9 uintC count1;
              dotimesC(count1,var_count-spec_count,
                { var reg7 object othervar = *othervarptr++; # nächste Variable
                  # Suche sie in den SPECIAL-deklarierten Variablen:
                  {var reg5 object* specvarptr = &TheSvector(vars)->data[0];
                   var reg6 uintC count2;
                   dotimespC(count2,spec_count,
                     { if (eq(*specvarptr++,othervar)) # gefunden?
                         # ja -> also ist die Variable othervar dynamisch zu binden.
                         { *othervarflagsptr |= bit(dynam_bit); break; }
                     });
                  }
                  othervarflagsptr++;
                });
            }
        # Schließlich noch die akkumulierten Listen in der Closure umdrehen:
          nreverse(TheIclosure(closure)->clos_opt_inits);
          nreverse(TheIclosure(closure)->clos_keywords);
          nreverse(TheIclosure(closure)->clos_key_inits);
          nreverse(TheIclosure(closure)->clos_aux_inits);
        # Fertig.
        # Stackaufbau: closure, lambdalist.
          skipSTACK(2);
          return closure;
        }}
    }}}}

# UP: Wandelt ein Argument in eine Funktion um.
# coerce_function(obj)
# > obj: Objekt
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: Objekt als Funktion (SUBR oder Closure)
# kann GC auslösen
  global object coerce_function (object obj);
  global object coerce_function(obj)
    var reg1 object obj;
    { # obj sollte ein SUBR, eine Closure oder ein Lambdaausdruck sein.
      if (subrp(obj)) { return obj; } # SUBR ist OK
      elif (closurep(obj)) { return obj; } # Closure ist OK
      elif (consp(obj) && eq(Car(obj),S(lambda))) # Cons (LAMBDA . ...) ?
        # Lambda-Ausdruck wird sofort in eine Closure umgewandelt:
        { # leeres Environment für get_closure:
          var reg2 environment* env5;
          make_STACK_env(NIL,NIL,NIL,NIL,O(top_decl_env), env5 = );
          # Closure bilden aus lambdabody = (cdr obj), name = :LAMBDA :
         {var reg3 object closure = get_closure(Cdr(obj),S(Klambda),env5);
          skipSTACK(5);
          return closure;
        }}
      else
        { pushSTACK(obj);
          pushSTACK(TheSubr(subr_self)->name);
          fehler(
                 DEUTSCH ? "~: ~ ist keine Funktion." :
                 ENGLISH ? "~: ~ is not a function" :
                 FRANCAIS ? "~: ~ n'est pas une fonction." :
                 ""
                );
        }
    }

# Fehlermeldung bei unpaarigen Keyword-Argumenten
# fehler_key_unpaarig(fun);
# > fun: Funktion
  local nonreturning void fehler_key_unpaarig (object fun);
  local nonreturning void fehler_key_unpaarig(fun)
    var reg1 object fun;
    { pushSTACK(fun);
      fehler(
             DEUTSCH ? "EVAL/APPLY: Keyword-Argumente für ~ sind nicht paarig." :
             ENGLISH ? "EVAL/APPLY: keyword arguments for ~ should occur pairwise" :
             FRANCAIS ? "EVAL/APPLY: Les arguments mot-clé de ~ ne sont pas par paires." :
             ""
            );
    }

# Fehlermeldung bei zu vielen Keyword-Argumenten
# fehler_key_zuviel(fun);
# > fun: Funktion
  local nonreturning void fehler_key_zuviel (object fun);
  local nonreturning void fehler_key_zuviel(fun)
    var reg1 object fun;
    { pushSTACK(fun);
      fehler(
             DEUTSCH ? "EVAL/APPLY: Zu viele Argumente für ~" :
             ENGLISH ? "EVAL/APPLY: too many arguments given to ~" :
             FRANCAIS ? "EVAL/APPLY: Trop d'arguments pour ~" :
             ""
            );
    }

# Fehlermeldung bei fehlerhaftem Keyword
# fehler_key_notkw(kw);
# > kw: Nicht-Keyword
  local nonreturning void fehler_key_notkw (object kw);
  local nonreturning void fehler_key_notkw(kw)
    var reg1 object kw;
    { pushSTACK(kw);
      fehler(
             DEUTSCH ? "EVAL/APPLY: ~ ist kein Keyword." :
             ENGLISH ? "EVAL/APPLY: ~ is not a keyword" :
             FRANCAIS ? "EVAL/APPLY: ~ n'est pas un mot-clé." :
             ""
            );
    }

# Fehlermeldung bei fehlerhaftem Keyword
# fehler_key_badkw(fun,kw,kwlist);
# > fun: Funktion
# > kw: unzulässiges Keyword
# > kwlist: Liste der zugelassenen Keywords
  local nonreturning void fehler_key_badkw (object fun, object kw, object kwlist);
  local nonreturning void fehler_key_badkw(fun,kw,kwlist)
    var reg2 object fun;
    var reg3 object kw;
    var reg1 object kwlist;
    { pushSTACK(kwlist);
      pushSTACK(fun);
      pushSTACK(kw);
      fehler(
             DEUTSCH ? "EVAL/APPLY: Das Keyword ~ ist bei ~ nicht erlaubt. Die möglichen Keywords sind ~" :
             ENGLISH ? "EVAL/APPLY: keyword ~ is illegal for ~. The possible keywords are ~" :
             FRANCAIS ? "EVAL/APPLY: Le mot-clé ~ n'est pas permis pour ~. Possibles sont ~" :
             ""
            );
    }

# Test auf unerlaubte Keywords
# check_for_illegal_keywords(allow_flag,fehler_statement);
# > uintC argcount: Anzahl der Keyword/Value-Paare
# > object* rest_args_pointer: Pointer über die 2*argcount restlichen Argumente
# > boolean allow_flag: Flag, ob &ALLOW-OTHER-KEYS angegeben war
# > for_every_keyword: Macro, der alle Keywords durchläuft und an 'keyword'
#                      zuweist.
# > fehler_statement: Statement, das meldet, daß bad_keyword illegal ist.
  #define check_for_illegal_keywords(allow_flag_expr,fehler_statement)  \
    { var reg6 object* argptr = rest_args_pointer; # Pointer in die Argumente \
      var reg8 object bad_keyword = nullobj; # erstes unerlaubtes Keyword oder nullobj \
      var reg4 boolean allow_flag = # Flag für allow-other-keys (ob           \
        # &ALLOW-OTHER-KEYS angegeben war oder ':ALLOW-OTHER-KEY T' vorkam)   \
        (allow_flag_expr);                                                    \
      var reg9 uintC check_count;                                             \
      dotimesC(check_count,argcount,                                          \
        { var reg3 object kw = NEXT(argptr); # nächstes Argument              \
          var reg7 object val = NEXT(argptr); # und Wert dazu                 \
          # sollte ein Keyword sein:                                          \
          if (!(symbolp(kw) && keywordp(kw)))                                 \
            { fehler_key_notkw(kw); }                                         \
          if (!allow_flag) # andere Keywords erlaubt? ja -> ok                \
            { if (eq(kw,S(Kallow_other_keys))) #  Kommt :ALLOW-OTHER-KEYS ?   \
                { if (!nullp(val)) { allow_flag = TRUE; } }                   \
                else                                                          \
                # bis hierher war nicht :ALLOW-OTHER-KEYS da, und NOALLOW     \
                { if (bad_keyword==nullobj) # bisher alle Keywords ok?        \
                    # muß testen, ob das Keyword kw erlaubt ist.              \
                    { for_every_keyword(                                      \
                        { if (eq(keyword,kw)) goto kw_ok; }                   \
                        );                                                    \
                      # Keyword kw war nicht erlaubt.                         \
                      bad_keyword = kw;                                       \
                      kw_ok: ;                                                \
            }   }   }                                                         \
        });                                                                   \
      if (!allow_flag)                                                        \
        if (!(bad_keyword==nullobj))                                          \
          # falsches Keyword aufgetreten                                      \
          { fehler_statement }                                                \
    }

# Zu einem Keyword 'keyword' das Paar Key.Wert suchen:
# find_keyword_value( notfound_statement, found_statement );
# > keyword: Keyword
# > uintC argcount: Anzahl der Keyword/Value-Paare
# > object* rest_args_pointer: Pointer über die 2*argcount restlichen Argumente
# > notfound_statement: Was zu tun ist, wenn nicht gefunden
# > found_statement: Was zu tun ist, wenn Wert value gefunden
  #define find_keyword_value(notfound_statement,found_statement)  \
    { var reg1 object* argptr = rest_args_pointer;                          \
      var reg2 uintC find_count;                                            \
      dotimesC(find_count,argcount,                                         \
        { if (eq(NEXT(argptr),keyword)) goto kw_found; # richtiges Keyword? \
          NEXT(argptr);                                                     \
        });                                                                 \
      if (TRUE)                                                             \
        # nicht gefunden                                                    \
        { notfound_statement }                                              \
        else                                                                \
        kw_found: # gefunden                                                \
        { var reg1 object value = NEXT(argptr);                             \
          found_statement                                                   \
        }                                                                   \
    }

# UP: Wendet eine interpretierte Closure auf Argumente an.
# funcall_iclosure(closure,args_pointer,argcount);
# > closure: Closure
# > args_pointer: Pointer über die Argumente (im Stack)
# > argcount: Anzahl der Argumente
# < mv_count/mv_space: Werte
# < STACK: aufgeräumt, = args_pointer
# kann GC auslösen
  local Values funcall_iclosure (object closure, object* args_pointer, uintC argcount);
  local Values funcall_iclosure(closure,args_pointer,argcount)
    var reg5 object closure;
    var reg8 object* args_pointer;
    var reg9 uintC argcount;
    { # 1. Schritt: APPLY-Frame zu Ende aufbauen:
      var jmp_buf my_jmp_buf;
      { var reg1 object* top_of_frame = args_pointer; # Pointer übern Frame
        pushSTACK(closure);
        finish_entry_frame(APPLY,&!my_jmp_buf,,
          { if (mv_count==0) # nach Wiedereintritt: Form übergeben?
              { closure = STACK_(frame_closure); # selben APPLY nochmals versuchen
                args_pointer = topofframe(STACK_0);
                argcount = STACK_item_count(STACK STACKop frame_args,args_pointer);
              }
              else
              { setSTACK(STACK = topofframe(STACK_0)); # STACK aufräumen # oder unwind() ??
                eval_noenv(value1); return; # übergebene Form evaluieren
          }   }
          );
      }
     {var reg10 object* closure_ = &STACK_(frame_closure); # Pointer auf die Closure
      var reg1 object* frame_pointer; # Pointer in den Frame
      # 2. Schritt: Variablenbindungsframe aufbauen:
      { var reg8 object* top_of_frame = STACK; # Pointer übern Frame
        var reg6 object vars = TheIclosure(closure)->clos_vars; # Vektor mit Variablennamen
        var reg7 uintL var_count = TheSvector(vars)->length; # Anzahl der Variablen
        get_space_on_STACK(var_count * 2 * sizeof(object)); # Platz reservieren
        { var reg3 object* varptr = &TheSvector(vars)->data[0]; # Pointer auf Variablen im Vektor
          var reg9 uintC spec_count = posfixnum_to_L(TheIclosure(closure)->clos_spec_anz);
          var reg4 uintC count;
          # erst die Special-Referenzen:
          dotimesC(count,spec_count,
            { # Bindung mit "Wert" specdecl:
              pushSTACK(specdecl);
              pushSTACK_symbolwithflags(*varptr++,wbit(active_bit_o)); # Bindung schon mal als aktiv vormerken
            });
          frame_pointer = args_end_pointer;
         {var reg3 uintB* varflagsptr = &TheSbvector(TheIclosure(closure)->clos_varflags)->data[0];
          dotimesC(count,var_count-spec_count,
            { pushSTACK(NIL); # NIL als vorläufiger Wert
             {var reg2 object next_var = *varptr++; # nächste Variable
              var reg1 oint next_varflags = (oint)(*varflagsptr++)<<oint_symbolflags_shift; # mit evtl. dynam_bit, svar_bit
              if (special_var_p(TheSymbol(next_var))) # SPECIAL-proklamiert?
                { next_varflags |= wbit(dynam_bit_o); } # -> dynamisch binden
              pushSTACK_symbolwithflags(next_var,next_varflags);
            }});
        }}
        # VAR_ENV der Closure wird NEXT_ENV im Frame:
        pushSTACK(TheIclosure(closure)->clos_var_env);
        pushSTACK((object)var_count); # var_count Bindungen, alle noch ungenestet
        finish_frame(VAR);
      }
      # STACK zeigt nun unter den Variablenbindungs-Frame.
      # frame_pointer = Pointer in den Variablenbindungsframe, über die erste
      # noch inaktive Bindung, unter die bereits aktiven SPECIAL-Referenzen.
      {var reg10 object new_var_env = make_framepointer(STACK);
       # Dieser Frame wird nachher zum neuen VAR_ENV.
      # 3. Schritt: aktuelle Environments binden:
       make_ENV5_frame();
      # Das Closure-Environment aktivieren:
       aktenv.var_env   = new_var_env; # Variablenbindungsframe
       aktenv.fun_env   = TheIclosure(closure)->clos_fun_env;
       aktenv.block_env = TheIclosure(closure)->clos_block_env;
       aktenv.go_env    = TheIclosure(closure)->clos_go_env;
       aktenv.decl_env  = TheIclosure(closure)->clos_decl_env;
      }
      # Stackaufbau:
      #   APPLY-Frame
      #   Variablenbindungsframe
      #   ENV-Frame
      # 4. Schritt: Parameter abarbeiten:
      { check_SP();
        # Macro zum Binden von Variablen im Variablenframe:
        # Bindet die nächste Variable an value, erniedrigt frame_pointer um 2 bzw. 3.
        # (Benutzt, daß varframe_binding_mark = 0 !)
        #define bind_next_var(value,markptr_zuweisung)  \
          { frame_pointer skipSTACKop -varframe_binding_size;                                  \
           {var reg2 object* markptr = markptr_zuweisung &Before(frame_pointer);               \
            if (*(oint*)(markptr) & wbit(dynam_bit_o))                                         \
              # dynamische Bindung aktivieren:                                                 \
              { var reg3 object sym = *(markptr STACKop varframe_binding_sym); # Variable      \
                *(markptr STACKop varframe_binding_value) = TheSymbolflagged(sym)->symvalue; # alten Wert in den Frame \
                *(oint*)(markptr) |= wbit(active_bit_o); # Bindung aktivieren                  \
                TheSymbolflagged(sym)->symvalue = (value); # neuen Wert in die Wertzelle       \
              }                                                                                \
              else                                                                             \
              # statische Bindung aktivieren:                                                  \
              { *(markptr STACKop varframe_binding_value) = (value); # neuen Wert in den Frame \
                *(oint*)(markptr) |= wbit(active_bit_o); # Bindung aktivieren                  \
              }                                                                                \
          }}
        # required-Parameter abarbeiten:
        # Es ist das jeweils nächste Argument zu holen und im Stack zu binden.
        { var reg7 uintC count = posfixnum_to_L(TheIclosure(closure)->clos_req_anz);
          if (count>0)
            { if (argcount < count)
                { pushSTACK(TheIclosure(closure)->clos_name);
                  fehler(
                         DEUTSCH ? "EVAL/APPLY: Zu wenig Argumente für ~" :
                         ENGLISH ? "EVAL/APPLY: too few arguments arguments given to ~" :
                         FRANCAIS ? "EVAL/APPLY: Trop peu d'arguments pour ~" :
                         ""
                        );
                }
              argcount -= count;
              dotimespC(count,count,
                { var reg6 object next_arg = NEXT(args_pointer); # nächstes Argument
                  bind_next_var(next_arg,); # nächste Variable binden
                });
        }   }
        # optionale Parameter abarbeiten:
        # Es ist jeweils das nächste Argument zu holen; falls keines vorliegt,
        # eine Init-Form auszuführen; dann im Stack zu binden.
        { var reg8 uintC count = posfixnum_to_L(TheIclosure(closure)->clos_opt_anz);
          if (count==0) goto optional_ende;
         {var reg7 object inits = TheIclosure(closure)->clos_opt_inits; # Init-Formen
          do { if (argcount==0) goto optional_aus;
               argcount--;
              {var reg7 object next_arg = NEXT(args_pointer); # nächstes Argument
               {var reg6 object* optmarkptr;
                bind_next_var(next_arg,optmarkptr=); # nächste Variable binden
                if (*(oint*)optmarkptr & wbit(svar_bit_o)) # supplied-p-Parameter folgt?
                  { *(oint*)optmarkptr &= ~wbit(svar_bit_o);
                    bind_next_var(T,); # ja -> an T binden
               }  }
               inits = Cdr(inits); # Init-Formen-Liste verkürzen
               count--;
             }}
             until (count==0);
          goto optional_ende;
          optional_aus: # Hier sind die optionalen Argumente ausgegangen.
          pushSTACK(inits);
         }# Ab hier alle Init-Formen der optionalen Parameter ausführen:
          dotimespC(count,count,
            { var reg7 object inits = STACK_0; # restliche Initformen
              STACK_0 = Cdr(inits);
              inits = (eval(Car(inits)),value1); # nächste Initform, ausgewertet
             {var reg6 object* optmarkptr;
              bind_next_var(inits,optmarkptr=); # nächste Variable binden
              if (*(oint*)optmarkptr & wbit(svar_bit_o)) # supplied-p-Parameter folgt?
                { *(oint*)optmarkptr &= ~wbit(svar_bit_o);
                  bind_next_var(NIL,); # ja -> an NIL binden
             }  }
            });
          closure = *closure_;
          # &REST-Parameter ohne Argumente initialisieren:
          if (!nullp(TheIclosure(closure)->clos_rest_flag)) # Rest-Flag?
            { bind_next_var(NIL,); } # ja -> an NIL binden
          # &KEY-Parameter ohne Argumente initialisieren:
          count = posfixnum_to_L(TheIclosure(closure)->clos_key_anz); # Anzahl Keyword-Parameter
          if (count>0)
            { STACK_0 = TheIclosure(closure)->clos_key_inits; # zugehörige Init-Formen
              dotimespC(count,count,
                { var reg7 object inits = STACK_0; # restliche Initformen
                  STACK_0 = Cdr(inits);
                  inits = (eval(Car(inits)),value1); # nächste Initform, ausgewertet
                 {var reg6 object* keymarkptr;
                  bind_next_var(inits,keymarkptr=); # nächste Variable binden
                  if (*(oint*)keymarkptr & wbit(svar_bit_o)) # supplied-p-Parameter folgt?
                    { *(oint*)keymarkptr &= ~wbit(svar_bit_o);
                      bind_next_var(NIL,); # ja -> an NIL binden
                 }  }
                });
              closure = *closure_;
            }
          skipSTACK(1); # restliche Init-Formen vergessen
          goto aux; # weiter zu den AUX-Variablen
        }
        optional_ende:
        # &KEY-Parameter und &REST-Parameter vorbereiten:
        if (mnumberp(TheIclosure(closure)->clos_keywords) # keyword eine Zahl?
            && nullp(TheIclosure(closure)->clos_rest_flag) # und kein Rest-Parameter?
           )
          # ja -> weder &KEY noch &REST angegeben
          { if (argcount>0) # noch Argumente da -> Fehler
              { pushSTACK(TheIclosure(closure)->clos_name);
                fehler(
                       DEUTSCH ? "EVAL/APPLY: Zu viele Argumente für ~" :
                       ENGLISH ? "EVAL/APPLY: too many arguments given to ~" :
                       FRANCAIS ? "EVAL/APPLY: Trop d'arguments pour ~." :
                       ""
                      );
              }
          }
          else
          # &KEY oder &REST vorhanden.
          { # &REST-Parameter abarbeiten:
            if (!nullp(TheIclosure(closure)->clos_rest_flag)) # Rest-Parameter vorhanden?
              # ja -> übrige Argumente zu einer Liste zusammenfassen:
              { pushSTACK(NIL); # Listenanfang
                if (argcount>0)
                  {var reg3 object* ptr = args_pointer STACKop -(uintL)argcount;
                   var reg4 uintC count;
                   dotimespC(count,argcount,
                     { var reg2 object new_cons = allocate_cons();
                       Car(new_cons) = BEFORE(ptr);
                       Cdr(new_cons) = STACK_0;
                       STACK_0 = new_cons;
                     });
                    closure = *closure_;
                  }
               {var reg6 object list = popSTACK(); # Gesamtliste
                bind_next_var(list,); # &REST-Parameter an diese Liste binden
              }}
            # &KEY-Parameter abarbeiten:
            if (!mnumberp(TheIclosure(closure)->clos_keywords))
              # Keyword-Parameter vorhanden
              { var reg10 object* rest_args_pointer = args_pointer;
                # argcount = Anzahl restlicher Argumente
                # argcount halbieren, gibt die Anzahl der Paare Key.Wert:
                if (!((argcount%2)==0))
                  # Anzahl war ungerade -> nicht paarig:
                  { fehler_key_unpaarig(TheIclosure(closure)->clos_name); }
                argcount = argcount/2;
                # Test auf unerlaubte Keywords:
                { var reg10 object keywords = TheIclosure(closure)->clos_keywords;
                  #define for_every_keyword(statement)  \
                    { var reg2 object keywordsr = keywords;         \
                      while (consp(keywordsr))                      \
                        { var reg1 object keyword = Car(keywordsr); \
                          statement;                                \
                          keywordsr = Cdr(keywordsr);               \
                    }   }
                  check_for_illegal_keywords(
                    !nullp(TheIclosure(closure)->clos_allow_flag),
                    { fehler_key_badkw(TheIclosure(closure)->clos_name,
                                       bad_keyword,
                                       TheIclosure(closure)->clos_keywords);
                    }
                    );
                  #undef for_every_keyword
                # Jetzt die Key-Werte zuordnen und die Key-Inits auswerten:
                 {var reg9 object key_inits = TheIclosure(closure)->clos_key_inits;
                  var reg9 uintC count;
                  dotimesC(count,posfixnum_to_L(TheIclosure(closure)->clos_key_anz),
                    { var reg8 object keyword = Car(keywords); # Keyword
                      var reg7 object var_value;
                      var reg8 object svar_value;
                      # Zu diesem Keyword das Paar Key.Wert suchen:
                      find_keyword_value(
                        # nicht gefunden, muß den Init auswerten:
                        { pushSTACK(keywords); pushSTACK(key_inits);
                          var_value = (eval(Car(key_inits)),value1);
                          key_inits = popSTACK(); keywords = popSTACK();
                          svar_value = NIL; # NIL für evtl. supplied-p-Parameter
                        },
                        # gefunden -> Wert nehmen:
                        { var_value = value;
                          svar_value = T; # T für evtl. supplied-p-Parameter
                        }
                        );
                      {var reg6 object* keymarkptr;
                       bind_next_var(var_value,keymarkptr=); # Keyword-Variable binden
                       if (*(oint*)keymarkptr & wbit(svar_bit_o)) # supplied-p-Parameter folgt?
                         { *(oint*)keymarkptr &= ~wbit(svar_bit_o);
                           bind_next_var(svar_value,); # ja -> an NIL bzw. T binden
                      }  }
                      keywords = Cdr(keywords);
                      key_inits = Cdr(key_inits);
                    });
                }}
                closure = *closure_;
          }   }
        aux: # &AUX-Parameter behandeln:
        { var reg7 uintC count = posfixnum_to_L(TheIclosure(closure)->clos_aux_anz);
          if (count>0)
            { pushSTACK(TheIclosure(closure)->clos_aux_inits); # Init-Formen für &AUX-Variablen
              dotimespC(count,count,
                { var reg6 object inits = STACK_0;
                  STACK_0 = Cdr(inits);
                  inits = (eval(Car(inits)),value1); # nächstes Init auswerten
                  bind_next_var(inits,); # und Variable daran binden
                });
              skipSTACK(1); # restliche Init-Formen vergessen
              closure = *closure_;
        }   }
        #undef bind_next_var
      }
      # 5. Schritt: Body auswerten:
      implicit_progn(TheIclosure(closure)->clos_body,NIL);
      unwind(); # ENV-Frame auflösen
      unwind(); # Variablenbindungsframe auflösen
      unwind(); # APPLY-Frame auflösen
      # fertig
    }}

# UP: Besorgt die Zuordnung der Key-Argumente bei SUBRs.
# Nur aufzurufen, falls key_flag /= subr_nokey.
# > fun: Funktion, ein SUBR
# > argcount: Argumentezahl nach den optionalen
# > STACK_(argcount-1),...,STACK_0: die argcount Argumente nach den optionalen
# > key_args_pointer: Pointer über die Key-Parameter im STACK
# > rest_args_pointer: Pointer über die restlichen Argumente im STACK
# < STACK: korrekt gesetzt
# verändert STACK
  local void match_subr_key (object fun, uintL argcount, object* key_args_pointer, object* rest_args_pointer);
  local void match_subr_key(fun,argcount,key_args_pointer,rest_args_pointer)
    var reg6 object fun;
    var reg9 uintL argcount;
    var reg10 object* key_args_pointer;
    var reg10 object* rest_args_pointer;
    { # argcount halbieren, gibt die Anzahl der Paare Key.Wert:
      if (!((argcount%2)==0))
        # Anzahl war ungerade -> nicht paarig:
        { fehler_key_unpaarig(fun); }
      if (((uintL)~(uintL)0 > ca_limit_1) && (argcount > ca_limit_1))
        { fehler_key_zuviel(fun); }
      # Da nun argcount <= ca_limit_1, passen alle count's in ein uintC.
      argcount = argcount/2;
      # Test auf unerlaubte Keywords:
      { var reg10 object* keywords_pointer = &TheSvector(TheSubr(fun)->keywords)->data[0];
        var reg10 uintC key_anz = TheSubr(fun)->key_anz;
        #define for_every_keyword(statement)  \
          { var reg1 object* keywordptr = keywords_pointer; \
            var reg2 uintC count;                           \
            dotimesC(count,key_anz,                         \
              { var reg1 object keyword = *keywordptr++;    \
                statement;                                  \
              });                                           \
          }
        check_for_illegal_keywords(
          TheSubr(fun)->key_flag == subr_key_allow,
          { pushSTACK(bad_keyword); # fehlerhaftes Keyword retten
            # Keyword-Vektor in eine Liste umwandeln:
            # (SYS::COERCE-SEQUENCE kwvec 'LIST)
            coerce_sequence(TheSubr(fun)->keywords,S(list));
           {var reg1 object kwlist = value1;
            fehler_key_badkw(fun,popSTACK(),kwlist);
          }}
          );
        #undef for_every_keyword
      # Jetzt Argumente und Parameter zuordnen:
       {var reg4 object* keywordptr = keywords_pointer;
        var reg5 object* key_args_ptr = key_args_pointer;
        var reg7 uintC count;
        dotimesC(count,key_anz,
          { var reg3 object keyword = *keywordptr++; # Keyword
            # Zu diesem Keyword das Paar Key.Wert suchen:
            find_keyword_value(
              # nicht gefunden -> Wert bleibt #<UNBOUND> :
              { NEXT(key_args_ptr); },
              # gefunden -> Wert eintragen:
              { NEXT(key_args_ptr) = value; }
              );
          });
      }}
      # evtl. Rest-Parameter versorgen:
      if (TheSubr(fun)->rest_flag == subr_norest)
        # SUBR ohne &REST-Flag: restliche Argumente vergessen:
        { set_args_end_pointer(rest_args_pointer); }
        # SUBR mit &REST-Flag: restliche Argumente im Stack belassen
    }

# UP: Besorgt die Zuordnung zwischen Argumentliste und Keyword-Parametern
# und eventuellem Rest-Parameter einer compilierten Closure.
# > closure: compilierte Closure mit &KEY-Parametern
# > argcount: Argumentezahl nach den optionalen
# > STACK_(argcount-1),...,STACK_0: die argcount Argumente nach den optionalen
# > key_args_pointer: Pointer über die Key-Parameter im STACK
#                     (evtl. auch Pointer unter den Rest-Parameter im STACK,
#                      der = #<UNBOUND> ist, falls er noch zu versorgen ist)
# > rest_args_pointer: Pointer über die restlichen Argumente im STACK
# < STACK: korrekt gesetzt
# < ergebnis: closure
# verändert STACK
# kann GC auslösen
  local object match_cclosure_key (object closure, uintL argcount, object* key_args_pointer, object* rest_args_pointer);
  local object match_cclosure_key(closure,argcount,key_args_pointer,rest_args_pointer)
    var reg10 object closure;
    var reg9 uintL argcount;
    var reg10 object* key_args_pointer;
    var reg9 object* rest_args_pointer;
    { # argcount halbieren, gibt die Anzahl der Paare Key.Wert:
      if (!((argcount%2)==0))
        # Anzahl war ungerade -> nicht paarig:
        { fehler_key_unpaarig(closure); }
      if (((uintL)~(uintL)0 > ca_limit_1) && (argcount > ca_limit_1))
        { fehler_key_zuviel(closure); }
      # Da nun argcount <= ca_limit_1, passen alle count's in ein uintC.
      argcount = argcount/2;
     {var reg10 object codevec = TheCclosure(closure)->clos_codevec; # Code-Vektor
      {var reg9 uintC key_anz = *(uintW*)(&TheSbvector(codevec)->data[CCHD+6]); # Anzahl Keywords
       var reg10 uintL keywords_offset = *(uintW*)(&TheSbvector(codevec)->data[CCHD+8]); # Offset der Keywords in FUNC
       var reg6 object* keywords_pointer = &TheCclosure(closure)->clos_consts[keywords_offset]; # zeigt aufs erste Keyword
      # Test auf unerlaubte Keywords:
        #define for_every_keyword(statement)  \
          { var reg1 object* keywordptr = keywords_pointer; \
            var reg2 uintC count;                        \
            dotimesC(count,key_anz,                      \
              { var reg1 object keyword = *keywordptr++; \
                statement;                               \
              });                                        \
          }
        check_for_illegal_keywords(
          !((TheSbvector(codevec)->data[CCHD+4] & bit(6)) == 0),
          { pushSTACK(bad_keyword); # retten
            # Liste der erlaubten Keywords bilden:
            for_every_keyword( { pushSTACK(keyword); } );
           {var reg7 object kwlist = listof(key_anz);
            bad_keyword = popSTACK();
            # und Fehler melden:
            fehler_key_badkw(closure,bad_keyword,kwlist);
          }}
          );
        #undef for_every_keyword
      # Jetzt Argumente und Parameter zuordnen:
       {var reg4 object* keywordptr = keywords_pointer;
        var reg5 object* key_args_ptr = key_args_pointer;
        var reg6 uintC count;
        dotimesC(count,key_anz,
          { var reg3 object keyword = *keywordptr++; # Keyword
            # Zu diesem Keyword das Paar Key.Wert suchen:
            find_keyword_value(
              # nicht gefunden -> Wert bleibt #<UNBOUND> :
              { NEXT(key_args_ptr); },
              # gefunden -> Wert eintragen:
              { NEXT(key_args_ptr) = value; }
              );
          });
      }}
      # evtl. Rest-Parameter versorgen:
      if (TheSbvector(codevec)->data[CCHD+4] & bit(0)) # Rest-Flag?
        # Closure mit Keywords und &REST-Flag:
        { var reg2 object* rest_arg_ = &BEFORE(key_args_pointer); # Pointer auf den REST-Parameter
          if (eq(*rest_arg_,unbound))
            # muß noch gefüllt werden: Liste basteln
            { *rest_arg_ = closure; # Closure retten
             {var reg1 object rest_arg = NIL;
              until (args_end_pointer == rest_args_pointer)
                { pushSTACK(rest_arg);
                  rest_arg = allocate_cons();
                  Cdr(rest_arg) = popSTACK();
                  Car(rest_arg) = popSTACK();
                }
              closure = *rest_arg_; # Closure zurück
              *rest_arg_ = rest_arg;
            }}
            else
            # restliche Argumente vergessen:
            { set_args_end_pointer(rest_args_pointer); }
        }
        else
        # Closure ohne &REST-Flag: restliche Argumente vergessen:
        { set_args_end_pointer(rest_args_pointer); }
      return closure;
    }}


#           ----------------------- E V A L -----------------------

# später:
  local Values eval1 (object form);
  local Values eval_fsubr (object fun, object args);
  local Values eval_applyhook (object fun);
  local Values eval_subr (object fun);
  local Values eval_closure (object fun);

# UP: Wertet eine Form im aktuellen Environment aus.
# eval(form);
# > form: Form
# < mv_count/mv_space: Werte
# kann GC auslösen
  global Values eval (object form);
  global Values eval(form)
    var reg2 object form;
    { start:
      # Test auf Tastatur-Interrupt:
      interruptp(
        { pushSTACK(form); # form retten
          pushSTACK(S(eval)); tast_break(); # Break-Schleife aufrufen
          form = popSTACK();
          goto start;
        });
     {var jmp_buf my_jmp_buf;
      # EVAL-Frame aufbauen:
      { var reg1 object* top_of_frame = STACK; # Pointer übern Frame
        pushSTACK(form); # Form
        finish_entry_frame(EVAL,&!my_jmp_buf,,
          { if (mv_count==0) # nach Wiedereintritt: Form übergeben?
              { form = STACK_(frame_form); } # selbe Form nochmal evaluieren
              else
              { form = STACK_(frame_form) = value1; } # übergebene Form evaluieren
          });
      }
      # Test auf *EVALHOOK*:
      { var reg1 object evalhook_value = Symbol_value(S(evalhookstern)); # *EVALHOOK*
        if (nullp(evalhook_value)) # *EVALHOOK* = NIL ?
          # ja -> normal weiter-evaluieren
          { pushSTACK(Symbol_value(S(applyhookstern))); eval1(form); }
          else
          { # *EVALHOOK*, *APPLYHOOK* an NIL binden:
            bindhooks_NIL();
            # (FUNCALL *EVALHOOK* form env) ausführen:
            pushSTACK(form); # Form als 1. Argument
            pushSTACK(evalhook_value); # Funktion retten
           {var reg4 environment* stack_env = nest_aktenv(); # Environments in den Stack,
            var reg3 object env = allocate_vector(5); # in neu allozierten Vektor
            *(environment*)(&TheSvector(env)->data[0]) = *stack_env; # hineinschieben
            skipSTACK(5);
            evalhook_value = popSTACK(); # Funktion zurück
            pushSTACK(env); # gesamtes Environment als 2. Argument
            funcall(evalhook_value,2);
            # alte Werte von *EVALHOOK*, *APPLYHOOK* zurück:
            unwind();
            # EVAL-Frame auflösen:
            unwind();
      }   }}
    }}

# UP: Wertet eine Form im aktuellen Environment aus. Nimmt dabei auf
# *EVALHOOK* und *APPLYHOOK* keine Rücksicht.
# eval_no_hooks(form);
# > form: Form
# < mv_count/mv_space: Werte
# kann GC auslösen
  global Values eval_no_hooks (object form);
  global Values eval_no_hooks(form)
    var reg2 object form;
    { var jmp_buf my_jmp_buf;
      # EVAL-Frame aufbauen:
      { var reg1 object* top_of_frame = STACK; # Pointer übern Frame
        pushSTACK(form); # Form
        finish_entry_frame(EVAL,&!my_jmp_buf,,
          { if (mv_count==0) # nach Wiedereintritt: Form übergeben?
              { form = STACK_(frame_form); } # selbe Form nochmal evaluieren
              else
              { form = STACK_(frame_form) = value1; } # übergebene Form evaluieren
          });
      }
      # weiterevaluieren, *APPLYHOOK* als NIL betrachten:
      { pushSTACK(NIL); eval1(form); }
    }

# UP: Wertet eine Form im aktuellen Environment aus.
# Nimmt dabei auf *EVALHOOK* keine Rücksicht, und erwartet den Wert von
# *APPLYHOOK*.
# Der EVAL-Frame muß bereits aufgebaut sein; er wird dann abgebaut.
# eval1(form);
# > form: Form
# > STACK_3..STACK_1: EVAL-Frame, mit Form in STACK_3
# > STACK_0: Wert von *APPLYHOOK*
# < mv_count/mv_space: Werte
# verändert STACK
# kann GC auslösen
  local Values eval1(form)
    var reg1 object form;
    { if (atomp(form))
        { if (symbolp(form))
            { # Form ist Symbol
              if (eq( value1 = sym_value(form) # Wert im aktuellen Environment
                     ,unbound
                 )  )
                { pushSTACK(form);
                  fehler(
                         DEUTSCH ? "EVAL: Die Variable ~ hat keinen Wert." :
                         ENGLISH ? "EVAL: variable ~ has no value" :
                         FRANCAIS ? "EVAL: La variable ~ n'a pas de valeur." :
                         ""
                        );
                }
                else
                { mv_count=1; # value1 als Wert
                  skipSTACK(1);
                  unwind(); # EVAL-Frame auflösen
                }
            }
          elif (   numberp(form) # Zahl ?
                || charp(form) # Character ?
                || stringp(form) # String ?
                || bit_vector_p(form) # Bitvektor ?
               )
            # self-evaluating form
            { value1 = form; mv_count=1; # form als Wert
              skipSTACK(1);
              unwind(); # EVAL-Frame auflösen
            }
          else
            { pushSTACK(form);
              fehler(
                     DEUTSCH ? "EVAL: ~ ist keine korrekte Form." :
                     ENGLISH ? "EVAL: illegal form ~" :
                     FRANCAIS ? "EVAL: ~ n'est pas une forme correcte." :
                     ""
                    );
            }
        }
        else
        # Form ist ein Cons
        { # Feststellen, ob Macro-call, evtl. expandieren:
          macroexp(form,aktenv.fun_env); form = value1;
          if (!nullp(value2)) # expandiert ?
            # jetzt erst richtig evaluieren:
            { skipSTACK(1); # Wert von *APPLYHOOK* vergessen
              check_SP(); check_STACK();
              eval(form); # expandierte Form evaluieren
              unwind(); # EVAL-Frame auflösen
            }
            else
            { var reg2 object fun = Car(form); # Funktionsbezeichnung
              if (symbolp(fun))
                { # Funktionsdefinition im Environment holen:
                  fun = sym_function(fun,aktenv.fun_env);
                  # je nach Typ der Funktion verzweigen:
                  # unbound / SUBR/FSUBR/Closure / Macro-Cons
                  switch (typecode(fun))
                    { case_subr: # SUBR
                        pushSTACK(Cdr(form)); # Argumentliste
                        if (!nullp(STACK_1)) goto applyhook;
                        eval_subr(fun);
                        break;
                      case_fsubr: # FSUBR
                        eval_fsubr(fun,Cdr(form));
                        break;
                      case_closure: # Closure
                        pushSTACK(Cdr(form)); # Argumentliste
                        closure: # fun ist eine Closure
                        if (!nullp(STACK_1)) goto applyhook;
                        eval_closure(fun);
                        break;
                      applyhook: # Wert von *APPLYHOOK* ist /= NIL.
                        eval_applyhook(fun);
                        break;
                      #ifdef ALIEN
                      case_orecord:
                        if (TheRecord(fun)->rectype == Rectype_Alienfun)
                          # Alienfun
                          { eval_alienfun(fun); break; }
                      #endif
                      default:
                        pushSTACK(Car(form));
                        fehler(
                               DEUTSCH ? "EVAL: Die Funktion ~ ist undefiniert." :
                               ENGLISH ? "EVAL: undefined function ~" :
                               FRANCAIS ? "EVAL: La fonction ~ n'est pas définie." :
                               ""
                              );
                }   }
              elif (consp(fun) && eq(Car(fun),S(lambda))) # Lambda-Ausdruck?
                { pushSTACK(Cdr(form)); # Argumentliste
                  fun = get_closure(Cdr(fun),S(Klambda),&aktenv); # Closure im aktuellen Environment erzeugen
                  goto closure; # und diese auf die Argumente anwenden, wie oben
                }
              else
                { pushSTACK(fun);
                  fehler(
                         DEUTSCH ? "EVAL: ~ ist keine Funktionsbezeichnung." :
                         ENGLISH ? "EVAL: ~ is not a function name" :
                         FRANCAIS ? "EVAL: ~ n'est pas un nom de fonction." :
                         ""
                        );
                }
            }
        }
    }

# In EVAL: Wendet ein FSUBR auf eine Argumentliste an, räumt den STACK auf
# und liefert die Werte.
# eval_fsubr(fun,args);
# > fun: ein FSUBR
# > args: Argumentliste
# > STACK-Aufbau: EVAL-Frame, *APPLYHOOK*.
# < STACK: aufgeräumt
# < mv_count/mv_space: Werte
# verändert STACK
# kann GC auslösen
  local Values eval_fsubr(fun,args)
    var reg2 object fun;
    var reg1 object args;
    { skipSTACK(1); # Wert von *APPLYHOOK* vergessen
      check_SP(); check_STACK();
     #if STACKCHECKS
     {var reg3 object* STACKbefore = STACK;
     #endif
      # Argumente in den STACK legen:
      switch (TheFsubr(fun)->argtype)
        { # Macro für 1 required-Parameter:
          #define REQ_PAR()  \
            { if (atomp(args)) goto fehler_zuwenig;                   \
              pushSTACK(Car(args)); # nächster Parameter in den STACK \
              args = Cdr(args);                                       \
            }
          case (uintW)fsubr_argtype_2_0_nobody:
            # FSUBR mit 2 required-Parametern
            REQ_PAR();
          case (uintW)fsubr_argtype_1_0_nobody:
            # FSUBR mit 1 required-Parameter
            REQ_PAR();
            if (!nullp(args)) goto fehler_zuviel;
            break;
          case (uintW)fsubr_argtype_2_1_nobody:
            # FSUBR mit 2 required-Parametern und 1 optional-Parameter
            REQ_PAR();
          case (uintW)fsubr_argtype_1_1_nobody:
            # FSUBR mit 1 required-Parameter und 1 optional-Parameter
            REQ_PAR();
            if (consp(args))
              { pushSTACK(Car(args)); # optionalen Parameter in den STACK
                args = Cdr(args);
                if (!nullp(args)) goto fehler_zuviel;
              }
              else
              { pushSTACK(unbound); # unbound stattdessen in den STACK
                if (!nullp(args)) goto fehler_dotted;
              }
            break;
          case (uintW)fsubr_argtype_2_body:
            # FSUBR mit 2 required-Parametern und Body-Parameter
            REQ_PAR();
          case (uintW)fsubr_argtype_1_body:
            # FSUBR mit 1 required-Parameter und Body-Parameter
            REQ_PAR();
          case (uintW)fsubr_argtype_0_body:
            # FSUBR mit 0 required-Parametern und Body-Parameter
            pushSTACK(args); # restlichen Body in den STACK
            break;
          default: NOTREACHED
          fehler_zuwenig: # Argumentliste args ist vorzeitig ein Atom
            if (!nullp(args)) goto fehler_dotted;
            # STACK bis zum aufrufenden EVAL-Frame aufräumen:
            until (mtypecode(STACK_0) & bit(frame_bit_t)) { skipSTACK(1); }
            { var reg4 object form = STACK_(frame_form); # Form aus dem EVAL-Frame
              pushSTACK(form);
              pushSTACK(Car(form));
              fehler(
                     DEUTSCH ? "EVAL: Zu wenig Parameter für Spezialform ~: ~" :
                     ENGLISH ? "EVAL: too few parameters for special-form ~: ~" :
                     FRANCAIS ? "EVAL: Trop peu de paramètres pour la forme spéciale ~ : ~" :
                     ""
                    );
            }
          fehler_zuviel: # Argumentliste args ist am Schluß nicht NIL
            if (atomp(args)) goto fehler_dotted;
            # STACK bis zum aufrufenden EVAL-Frame aufräumen:
            until (mtypecode(STACK_0) & bit(frame_bit_t)) { skipSTACK(1); }
            { var reg4 object form = STACK_(frame_form); # Form aus dem EVAL-Frame
              pushSTACK(form);
              pushSTACK(Car(form));
              fehler(
                     DEUTSCH ? "EVAL: Zu viele Parameter für Spezialform ~: ~" :
                     ENGLISH ? "EVAL: too many parameters for special-form ~: ~" :
                     FRANCAIS ? "EVAL: Trop de paramètres pour la forme spéciale ~ : ~" :
                     ""
                    );
            }
          fehler_dotted: # Argumentliste args endet mit Atom /= NIL
            # STACK bis zum aufrufenden EVAL-Frame aufräumen:
            until (mtypecode(STACK_0) & bit(frame_bit_t)) { skipSTACK(1); }
            { var reg4 object form = STACK_(frame_form); # Form aus dem EVAL-Frame
              pushSTACK(form);
              pushSTACK(Car(form));
              fehler(
                     DEUTSCH ? "EVAL: Parameterliste für Spezialform ~ ist dotted: ~" :
                     ENGLISH ? "EVAL: dotted parameter list for special form ~: ~" :
                     FRANCAIS ? "EVAL: La liste de paramètres pour la forme spéciale ~ est pointée." :
                     ""
                    );
            }
          #undef REQ_PAR
        }
      # FSUBR selbst aufrufen:
      subr_self = fun;
      (*(fsubr_function*)(TheFsubr(fun)->function))();
     #if STACKCHECKS
      if (!(STACK == STACKbefore)) # STACK so wie vorher?
        { abort(); } # nein -> ab in den Debugger
     }
     #endif
      unwind(); # EVAL-Frame auflösen
    }

# In EVAL: Wendet *APPLYHOOK* auf eine Funktion (SUBR oder Closure) und
# eine Argumentliste an, räumt den STACK auf und liefert die Werte.
# eval_applyhook(fun);
# > fun: Funktion, ein SUBR oder eine Closure
# > STACK-Aufbau: EVAL-Frame, *APPLYHOOK* (/= NIL), Argumentliste.
# < STACK: aufgeräumt
# < mv_count/mv_space: Werte
# verändert STACK
# kann GC auslösen
  local Values eval_applyhook(fun)
    var reg5 object fun;
    { var reg4 object args = popSTACK(); # Argumentliste
      var reg3 object applyhook_value = popSTACK(); # Wert von *APPLYHOOK*
      check_SP();
      # *EVALHOOK*, *APPLYHOOK* an NIL binden:
      bindhooks_NIL();
      # (FUNCALL *APPLYHOOK* fun args env) ausführen:
      pushSTACK(fun); # Funktion als 1. Argument
      pushSTACK(args); # Argumentliste als 2. Argument
      pushSTACK(applyhook_value); # Funktion retten
     {var reg2 environment* stack_env = nest_aktenv(); # Environments in den Stack,
      var reg1 object env = allocate_vector(5); # in neu allozierten Vektor
      *(environment*)(&TheSvector(env)->data[0]) = *stack_env; # hineinschieben
      skipSTACK(5);
      applyhook_value = popSTACK(); # Funktion zurück
      pushSTACK(env); # gesamtes Environment als 3. Argument
      funcall(applyhook_value,3);
      # alte Werte von *EVALHOOK*, *APPLYHOOK* zurück:
      unwind();
      # EVAL-Frame auflösen:
      unwind();
    }}

# In EVAL: Fehler bei zu wenig Argumenten
  local nonreturning void fehler_eval_zuwenig (object fun);
  local nonreturning void fehler_eval_zuwenig(fun)
    var reg2 object fun;
    { var reg1 object form = STACK_(frame_form); # Form
      pushSTACK(form);
      pushSTACK(fun);
      fehler(
             DEUTSCH ? "EVAL: Zu wenig Argumente für ~: ~" :
             ENGLISH ? "EVAL: too few arguments given to ~: ~" :
             FRANCAIS ? "EVAL: Trop peu d'arguments pour ~ : ~" :
             ""
            );
    }

# In EVAL: Fehler bei zu vielen Argumenten
  local nonreturning void fehler_eval_zuviel (object fun);
  local nonreturning void fehler_eval_zuviel(fun)
    var reg2 object fun;
    { var reg1 object form = STACK_(frame_form); # Form
      pushSTACK(form);
      pushSTACK(fun);
      fehler(
             DEUTSCH ? "EVAL: Zu viele Argumente für ~: ~" :
             ENGLISH ? "EVAL: too many arguments given to ~: ~" :
             FRANCAIS ? "EVAL: Trop d'arguments pour ~ : ~" :
             ""
            );
    }

# In EVAL: Fehler bei punktierter Argumentliste
  local nonreturning void fehler_eval_dotted (object fun);
  local nonreturning void fehler_eval_dotted(fun)
    var reg2 object fun;
    { var reg1 object form = STACK_(frame_form); # Form
      pushSTACK(form);
      pushSTACK(fun);
      fehler(
             DEUTSCH ? "EVAL: Argumentliste für ~ ist dotted: ~" :
             ENGLISH ? "EVAL: argument list given to ~ is dotted: ~" :
             FRANCAIS ? "EVAL: La liste d'arguments passée à ~ est pointée." :
             ""
            );
    }

# In EVAL: Wendet ein SUBR auf eine Argumentliste an, räumt den STACK auf
# und liefert die Werte.
# eval_subr(fun);
# > fun: Funktion, ein SUBR
# > STACK-Aufbau: EVAL-Frame, *APPLYHOOK*, Argumentliste.
# < STACK: aufgeräumt
# < mv_count/mv_space: Werte
# verändert STACK
# kann GC auslösen
  local Values eval_subr(fun)
    var reg2 object fun;
    { var reg1 object args = popSTACK(); # Argumentliste
      skipSTACK(1); # Wert von *APPLYHOOK* vergessen
      check_SP(); check_STACK();
     {var reg1 object* args_pointer = args_end_pointer; # Pointer über die Argumente
      var reg1 object* rest_args_pointer; # Pointer über die restlichen Argumente
      var reg1 uintL argcount; # Anzahl der restlichen Argumente
      # Argumente ausgewertet in den STACK legen:
      # erst ein Dispatch für die wichtigsten Fälle:
      switch (TheSubr(fun)->argtype)
        { # Macro für ein required-Argument:
          #define REQ_ARG()  \
            { if (atomp(args)) goto fehler_zuwenig;                \
              pushSTACK(Cdr(args)); # restliche Argumente          \
              eval(Car(args)); # nächstes Argument auswerten       \
              args = STACK_0; STACK_0 = value1; # und in den STACK \
            }
          # Macro für das n-letzte optional-Argument:
          #define OPT_ARG(n)  \
            { if (atomp(args)) goto unbound_optional_##n ;         \
              pushSTACK(Cdr(args)); # restliche Argumente          \
              eval(Car(args)); # nächstes Argument auswerten       \
              args = STACK_0; STACK_0 = value1; # und in den STACK \
            }
          case (uintW)subr_argtype_4_0:
            # SUBR mit 4 required-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_3_0:
            # SUBR mit 3 required-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_2_0:
            # SUBR mit 2 required-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_1_0:
            # SUBR mit 1 required-Argument
            REQ_ARG();
          case (uintW)subr_argtype_0_0:
            # SUBR ohne Argumente
            if (!nullp(args)) goto fehler_zuviel;
            goto apply_subr_norest;
          case (uintW)subr_argtype_4_1:
            # SUBR mit 4 required-Argumenten und 1 optional-Argument
            REQ_ARG();
          case (uintW)subr_argtype_3_1:
            # SUBR mit 3 required-Argumenten und 1 optional-Argument
            REQ_ARG();
          case (uintW)subr_argtype_2_1:
            # SUBR mit 2 required-Argumenten und 1 optional-Argument
            REQ_ARG();
          case (uintW)subr_argtype_1_1:
            # SUBR mit 1 required-Argument und 1 optional-Argument
            REQ_ARG();
          case (uintW)subr_argtype_0_1:
            # SUBR mit 1 optional-Argument
            OPT_ARG(1);
            if (!nullp(args)) goto fehler_zuviel;
            goto apply_subr_norest;
          case (uintW)subr_argtype_2_2:
            # SUBR mit 2 required-Argumenten und 2 optional-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_1_2:
            # SUBR mit 1 required-Argument und 2 optional-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_0_2:
            # SUBR mit 2 optional-Argumenten
            OPT_ARG(2);
            OPT_ARG(1);
            if (!nullp(args)) goto fehler_zuviel;
            goto apply_subr_norest;
          case (uintW)subr_argtype_0_5:
            # SUBR mit 5 optional-Argumenten
            OPT_ARG(5);
          case (uintW)subr_argtype_0_4:
            # SUBR mit 4 optional-Argumenten
            OPT_ARG(4);
          case (uintW)subr_argtype_0_3:
            # SUBR mit 3 optional-Argumenten
            OPT_ARG(3);
            OPT_ARG(2);
            OPT_ARG(1);
            if (!nullp(args)) goto fehler_zuviel;
            goto apply_subr_norest;
          unbound_optional_5: # Noch 5 optionale Argumente, aber atomp(args)
            pushSTACK(unbound);
          unbound_optional_4: # Noch 4 optionale Argumente, aber atomp(args)
            pushSTACK(unbound);
          unbound_optional_3: # Noch 3 optionale Argumente, aber atomp(args)
            pushSTACK(unbound);
          unbound_optional_2: # Noch 2 optionale Argumente, aber atomp(args)
            pushSTACK(unbound);
          unbound_optional_1: # Noch 1 optionales Argument, aber atomp(args)
            pushSTACK(unbound);
            if (!nullp(args)) goto fehler_dotted;
            goto apply_subr_norest;
          case (uintW)subr_argtype_3_0_rest:
            # SUBR mit 3 required-Argumenten und weiteren Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_2_0_rest:
            # SUBR mit 2 required-Argumenten und weiteren Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_1_0_rest:
            # SUBR mit 1 required-Argument und weiteren Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_0_0_rest:
            # SUBR mit weiteren Argumenten
            rest_args_pointer = args_end_pointer; # Pointer über die restlichen Argumente
            # alle weiteren Argumente auswerten und in den Stack:
            argcount = 0; # Zähler für die restlichen Argumente
            while (consp(args))
              { check_STACK();
                pushSTACK(Cdr(args)); # restliche Argumente
                eval(Car(args)); # nächstes Argument auswerten
                args = STACK_0; STACK_0 = value1; # und in den STACK
                argcount++;
              }
            goto apply_subr_rest;
          case (uintW)subr_argtype_4_0_key:
            # SUBR mit 4 required-Argumenten und Keyword-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_3_0_key:
            # SUBR mit 3 required-Argumenten und Keyword-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_2_0_key:
            # SUBR mit 2 required-Argumenten und Keyword-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_1_0_key:
            # SUBR mit 1 required-Argument und Keyword-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_0_0_key:
            # SUBR mit Keyword-Argumenten
            if (atomp(args)) goto unbound_optional_key_0;
            goto apply_subr_key;
          case (uintW)subr_argtype_1_1_key:
            # SUBR mit 1 required-Argument, 1 optional-Argument und Keyword-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_0_1_key:
            # SUBR mit 1 optional-Argument und Keyword-Argumenten
            OPT_ARG(key_1);
            if (atomp(args)) goto unbound_optional_key_0;
            goto apply_subr_key;
          case (uintW)subr_argtype_1_2_key:
            # SUBR mit 1 required-Argument, 2 optional-Argumenten und Keyword-Argumenten
            REQ_ARG();
            OPT_ARG(key_2);
            OPT_ARG(key_1);
            if (atomp(args)) goto unbound_optional_key_0;
            goto apply_subr_key;
          unbound_optional_key_2: # Noch 2 optionale Argumente, aber atomp(args)
            pushSTACK(unbound);
          unbound_optional_key_1: # Noch 1 optionales Argument, aber atomp(args)
            pushSTACK(unbound);
          unbound_optional_key_0: # Vor den Keywords ist atomp(args)
            { var reg1 uintC count;
              dotimesC(count,TheSubr(fun)->key_anz, { pushSTACK(unbound); } );
            }
            if (!nullp(args)) goto fehler_dotted;
            goto apply_subr_norest;
          default: NOTREACHED
          #undef OPT_ARG
          #undef REQ_ARG
        }
      # Nun die allgemeine Version:
      # Platz auf dem STACK reservieren:
      get_space_on_STACK(sizeof(object) *
                         (uintL)(TheSubr(fun)->req_anz +
                                 TheSubr(fun)->opt_anz +
                                 TheSubr(fun)->key_anz));
      # required Parameter auswerten und in den Stack ablegen:
      { var reg1 uintC count;
        dotimesC(count,TheSubr(fun)->req_anz,
          { if (atomp(args)) goto fehler_zuwenig; # Argumentliste zu Ende?
            pushSTACK(Cdr(args)); # restliche Argumentliste
            eval(Car(args)); # nächstes Argument auswerten
            args = STACK_0; STACK_0 = value1; # und in den Stack
          });
      }
      # optionale Parameter auswerten und in den Stack ablegen:
      { var reg1 uintC count = TheSubr(fun)->opt_anz;
        loop
          { if (atomp(args)) break; # Argumentliste zu Ende?
            if (count==0) goto optionals_ok; # alle optionalen Parameter versorgt?
            count--;
            pushSTACK(Cdr(args)); # restliche Argumentliste
            eval(Car(args)); # nächstes Argument auswerten
            args = STACK_0; STACK_0 = value1; # und in den Stack
          }
        # Argumentliste beendet.
        # Alle weiteren count optionalen Parameter bekommen den "Wert"
        # #<UNBOUND>, auch die Keyword-Parameter:
        dotimesC(count,count + TheSubr(fun)->key_anz, { pushSTACK(unbound); } );
        if (TheSubr(fun)->rest_flag == subr_rest) # &REST-Flag?
          # ja -> 0 zusätzliche Argumente:
          { argcount = 0; rest_args_pointer = args_end_pointer; }
          # nein -> nichts zu tun
        goto los;
      }
      optionals_ok:
      # Rest- und Keyword-Parameter behandeln.
      # args = restliche Argumentliste (noch nicht zu Ende)
      if (TheSubr(fun)->key_flag == subr_nokey)
        # SUBR ohne KEY
        { if (TheSubr(fun)->rest_flag == subr_norest)
            # SUBR ohne REST oder KEY -> Argumentliste müßte zu Ende sein
            { goto fehler_zuviel; }
            else
            # SUBR mit nur REST, ohne KEY: Behandlung der restlichen Argumente
            { rest_args_pointer = args_end_pointer;
              argcount = 0; # Zähler für die restlichen Argumente
              do { check_STACK();
                   pushSTACK(Cdr(args)); # restliche Argumentliste
                   eval(Car(args)); # nächstes Argument auswerten
                   args = STACK_0; STACK_0 = value1; # und in den Stack
                   argcount++;
                 }
                 while (consp(args));
              if (((uintL)~(uintL)0 > ca_limit_1) && (argcount > ca_limit_1))
                { goto fehler_zuviel; }
        }   }
        else
        # SUBR mit Keywords.
        apply_subr_key:
        # args = restliche Argumentliste (noch nicht zu Ende)
        # Erst die Keyword-Parameter mit #<UNBOUND> vorbesetzen, dann
        # die restlichen Argumente auswerten und im Stack ablegen, dann
        # die Keywords zuordnen:
        { var reg1 object* key_args_pointer = args_end_pointer; # Pointer über Keyword-Parameter
          # alle Keyword-Parameter mit #<UNBOUND> vorbesetzen:
          { var reg1 uintC count;
            dotimesC(count,TheSubr(fun)->key_anz, { pushSTACK(unbound); } );
          }
          rest_args_pointer = args_end_pointer; # Pointer über die restlichen Argumente
          # alle weiteren Argumente auswerten und in den Stack:
          argcount = 0; # Zähler für die restlichen Argumente
          do { check_STACK();
               pushSTACK(Cdr(args)); # restliche Argumentliste
               eval(Car(args)); # nächstes Argument auswerten
               args = STACK_0; STACK_0 = value1; # und in den Stack
               argcount++;
             }
             while (consp(args));
          if (((uintL)~(uintL)0 > ca_limit_1) && (argcount > ca_limit_1))
            { goto fehler_zuviel; }
          # Keywords zuordnen und evtl. restliche Argumente wegwerfen:
          match_subr_key(fun,argcount,key_args_pointer,rest_args_pointer);
        }
      los: # Funktion anspringen
      # restliche Argumentliste muß =NIL sein:
      if (!nullp(args)) goto fehler_dotted;
      if (TheSubr(fun)->rest_flag == subr_norest)
        # SUBR ohne &REST-Flag:
        apply_subr_norest:
        { subr_self = fun;
          (*(subr_norest_function*)(TheSubr(fun)->function))();
        }
        else
        # SUBR mit &REST-Flag:
        apply_subr_rest:
        { subr_self = fun;
          (*(subr_rest_function*)(TheSubr(fun)->function))
           (argcount,rest_args_pointer);
        }
      #if STACKCHECKS
      if (!(args_pointer == args_end_pointer)) # Stack aufgeräumt?
        { abort(); } # nein -> ab in den Debugger
      #endif
      unwind(); # EVAL-Frame auflösen
      return; # fertig
      # Gesammelte Fehlermeldungen:
      fehler_zuwenig: # Argumentliste args ist vorzeitig ein Atom
        if (!nullp(args)) goto fehler_dotted;
        set_args_end_pointer(args_pointer); # STACK aufräumen
        fehler_eval_zuwenig(TheSubr(fun)->name);
      fehler_zuviel: # Argumentliste args ist am Schluß nicht NIL
        if (atomp(args)) goto fehler_dotted;
        set_args_end_pointer(args_pointer); # STACK aufräumen
        fehler_eval_zuviel(TheSubr(fun)->name);
      fehler_dotted: # Argumentliste args endet mit Atom /= NIL
        set_args_end_pointer(args_pointer); # STACK aufräumen
        fehler_eval_dotted(TheSubr(fun)->name);
    }}

# In EVAL: Wendet eine Closure auf eine Argumentliste an, räumt den STACK auf
# und liefert die Werte.
# eval_closure(fun);
# > fun: Funktion, eine Closure
# > STACK-Aufbau: EVAL-Frame, *APPLYHOOK*, Argumentliste.
# < STACK: aufgeräumt
# < mv_count/mv_space: Werte
# verändert STACK
# kann GC auslösen
  local Values eval_closure(closure)
    var reg2 object closure;
    { var reg1 object args = popSTACK(); # Argumentliste
      skipSTACK(1); # Wert von *APPLYHOOK* vergessen
      # STACK-Aufbau: EVAL-Frame.
      check_SP(); check_STACK();
      pushSTACK(closure); # Closure retten
     {var reg1 object* closure_ = &STACK_0; # und merken, wo sie sitzt
      if (m_simple_bit_vector_p(TheClosure(closure)->clos_codevec))
        # closure ist eine compilierte Closure
        { var reg10 object* STACKbefore = STACK;
          var reg1 object codevec = TheCclosure(closure)->clos_codevec; # Code-Vektor
          # Argumente ausgewertet in den STACK legen:
          # erst ein Dispatch für die wichtigsten Fälle:
          switch (TheSbvector(codevec)->data[CCHD+5])
            { # Macro für ein required-Argument:
              #define REQ_ARG()  \
                { if (atomp(args)) goto fehler_zuwenig;                \
                  pushSTACK(Cdr(args)); # restliche Argumente          \
                  eval(Car(args)); # nächstes Argument auswerten       \
                  args = STACK_0; STACK_0 = value1; # und in den STACK \
                }
              # Macro für das n-letzte optional-Argument:
              #define OPT_ARG(n)  \
                { if (atomp(args)) goto unbound_optional_##n ;         \
                  pushSTACK(Cdr(args)); # restliche Argumente          \
                  eval(Car(args)); # nächstes Argument auswerten       \
                  args = STACK_0; STACK_0 = value1; # und in den STACK \
                }
              case (uintB)cclos_argtype_5_0:
                # 5 required-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_4_0:
                # 4 required-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_3_0:
                # 3 required-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_2_0:
                # 2 required-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_1_0:
                # 1 required-Argument
                REQ_ARG();
              case (uintB)cclos_argtype_0_0:
                # keine Argumente
                noch_0_opt_args:
                if (!nullp(args)) goto fehler_zuviel;
                goto apply_cclosure_nokey;
              case (uintB)cclos_argtype_4_1:
                # 4 required-Argumente und 1 optional-Argument
                REQ_ARG();
              case (uintB)cclos_argtype_3_1:
                # 3 required-Argumente und 1 optional-Argument
                REQ_ARG();
              case (uintB)cclos_argtype_2_1:
                # 2 required-Argumente und 1 optional-Argument
                REQ_ARG();
              case (uintB)cclos_argtype_1_1:
                # 1 required-Argument und 1 optional-Argument
                REQ_ARG();
              case (uintB)cclos_argtype_0_1:
                # 1 optional-Argument
                noch_1_opt_args:
                OPT_ARG(1);
                goto noch_0_opt_args;
              case (uintB)cclos_argtype_3_2:
                # 3 required-Argumente und 2 optional-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_2_2:
                # 2 required-Argumente und 2 optional-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_1_2:
                # 1 required-Argument und 2 optional-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_0_2:
                # 2 optional-Argumente
                noch_2_opt_args:
                OPT_ARG(2);
                goto noch_1_opt_args;
              case (uintB)cclos_argtype_2_3:
                # 2 required-Argumente und 3 optional-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_1_3:
                # 1 required-Argument und 3 optional-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_0_3:
                # 3 optional-Argumente
                noch_3_opt_args:
                OPT_ARG(3);
                goto noch_2_opt_args;
              case (uintB)cclos_argtype_1_4:
                # 1 required-Argument und 4 optional-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_0_4:
                # 4 optional-Argumente
                noch_4_opt_args:
                OPT_ARG(4);
                goto noch_3_opt_args;
              case (uintB)cclos_argtype_0_5:
                # 5 optional-Argumente
                OPT_ARG(5);
                goto noch_4_opt_args;
              unbound_optional_5: # Noch 5 optionale Argumente, aber atomp(args)
                pushSTACK(unbound);
              unbound_optional_4: # Noch 4 optionale Argumente, aber atomp(args)
                pushSTACK(unbound);
              unbound_optional_3: # Noch 3 optionale Argumente, aber atomp(args)
                pushSTACK(unbound);
              unbound_optional_2: # Noch 2 optionale Argumente, aber atomp(args)
                pushSTACK(unbound);
              unbound_optional_1: # Noch 1 optionales Argument, aber atomp(args)
                pushSTACK(unbound);
                if (!nullp(args)) goto fehler_dotted;
                goto apply_cclosure_nokey;
              case (uintB)cclos_argtype_4_0_rest:
                # 4 required-Argumente, Rest-Parameter
                REQ_ARG();
              case (uintB)cclos_argtype_3_0_rest:
                # 3 required-Argumente, Rest-Parameter
                REQ_ARG();
              case (uintB)cclos_argtype_2_0_rest:
                # 2 required-Argumente, Rest-Parameter
                REQ_ARG();
              case (uintB)cclos_argtype_1_0_rest:
                # 1 required-Argument, Rest-Parameter
                REQ_ARG();
              case (uintB)cclos_argtype_0_0_rest:
                # keine Argumente, Rest-Parameter
                if (consp(args)) goto apply_cclosure_rest_nokey;
                if (!nullp(args)) goto fehler_dotted;
                pushSTACK(NIL); # Rest-Parameter := NIL
                goto apply_cclosure_nokey;
              case (uintB)cclos_argtype_4_0_key:
                # 4 required-Argumente, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_3_0_key:
                # 3 required-Argumente, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_2_0_key:
                # 2 required-Argumente, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_1_0_key:
                # 1 required-Argument, Keyword-Argumente
                REQ_ARG();
                noch_0_opt_args_key:
                closure = *closure_; codevec = TheCclosure(closure)->clos_codevec;
              case (uintB)cclos_argtype_0_0_key:
                # nur Keyword-Argumente
                if (atomp(args)) goto unbound_optional_key_0;
                goto apply_cclosure_key;
              case (uintB)cclos_argtype_3_1_key:
                # 3 required-Argumente und 1 optional-Argument, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_2_1_key:
                # 2 required-Argumente und 1 optional-Argument, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_1_1_key:
                # 1 required-Argument und 1 optional-Argument, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_0_1_key:
                # 1 optional-Argument, Keyword-Argumente
                noch_1_opt_args_key:
                OPT_ARG(key_1);
                goto noch_0_opt_args_key;
              case (uintB)cclos_argtype_2_2_key:
                # 2 required-Argumente und 2 optional-Argumente, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_1_2_key:
                # 1 required-Argument und 2 optional-Argumente, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_0_2_key:
                # 2 optional-Argumente, Keyword-Argumente
                noch_2_opt_args_key:
                OPT_ARG(key_2);
                goto noch_1_opt_args_key;
              case (uintB)cclos_argtype_1_3_key:
                # 1 required-Argument und 3 optional-Argumente, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_0_3_key:
                # 3 optional-Argumente, Keyword-Argumente
                noch_3_opt_args_key:
                OPT_ARG(key_3);
                goto noch_2_opt_args_key;
              case (uintB)cclos_argtype_0_4_key:
                # 4 optional-Argumente, Keyword-Argumente
                OPT_ARG(key_4);
                goto noch_3_opt_args_key;
              unbound_optional_key_4: # Noch 4 optionale Argumente, aber atomp(args)
                pushSTACK(unbound);
              unbound_optional_key_3: # Noch 3 optionale Argumente, aber atomp(args)
                pushSTACK(unbound);
              unbound_optional_key_2: # Noch 2 optionale Argumente, aber atomp(args)
                pushSTACK(unbound);
              unbound_optional_key_1: # Noch 1 optionales Argument, aber atomp(args)
                pushSTACK(unbound);
              unbound_optional_key_0: # Vor den Keywords ist atomp(args)
                if (!nullp(args)) goto fehler_dotted;
                goto apply_cclosure_key_noargs;
              case (uintB)cclos_argtype_default:
                # Allgemeine Version
                break;
              default: NOTREACHED
              #undef OPT_ARG
              #undef REQ_ARG
            }
          # Nun die allgemeine Version:
          { var reg1 uintL req_anz = *(uintW*)(&TheSbvector(codevec)->data[CCHD+0]); # Anzahl required Parameter
            var reg1 uintL opt_anz = *(uintW*)(&TheSbvector(codevec)->data[CCHD+2]); # Anzahl optionale Parameter
            var reg1 uintB flags = TheSbvector(codevec)->data[CCHD+4]; # Flags
            # Platz auf dem STACK reservieren:
            get_space_on_STACK(sizeof(object) * (req_anz+opt_anz));
            # required Parameter auswerten und in den Stack ablegen:
            { var reg1 uintC count;
              dotimesC(count,req_anz,
                { if (atomp(args)) goto fehler_zuwenig; # Argumentliste zu Ende?
                  pushSTACK(Cdr(args)); # restliche Argumentliste
                  eval(Car(args)); # nächstes Argument auswerten
                  args = STACK_0; STACK_0 = value1; # und in den Stack
                });
            }
            # optionale Parameter auswerten und in den Stack ablegen:
            { var reg1 uintC count = opt_anz;
              loop
                { if (atomp(args)) break; # Argumentliste zu Ende?
                  if (count==0) goto optionals_ok; # alle optionalen Parameter versorgt?
                  count--;
                  pushSTACK(Cdr(args)); # restliche Argumentliste
                  eval(Car(args)); # nächstes Argument auswerten
                  args = STACK_0; STACK_0 = value1; # und in den Stack
                }
              # Argumentliste beendet.
              if (!nullp(args)) goto fehler_dotted;
              # Alle weiteren count optionalen Parameter bekommen den "Wert"
              # #<UNBOUND>, der &REST-Parameter den Wert NIL,
              # die Keyword-Parameter den Wert #<UNBOUND> :
              dotimesC(count,count, { pushSTACK(unbound); } );
              closure = *closure_; codevec = TheCclosure(closure)->clos_codevec;
              if (flags & bit(0)) # &REST-Flag?
                { pushSTACK(NIL); } # ja -> mit NIL initialisieren
              if (flags & bit(7)) # &KEY-Flag?
                apply_cclosure_key_noargs:
                { var reg1 uintC count = *(uintW*)(&TheSbvector(codevec)->data[CCHD+6]); # Anzahl Keyword-Parameter
                  dotimesC(count,count, { pushSTACK(unbound); } ); # mit #<UNBOUND> initialisieren
                  interpret_bytecode(closure,codevec,CCHD+10); # Bytecode ab Byte 10 abinterpretieren
                }
                else
                { interpret_bytecode(closure,codevec,CCHD+6); } # Bytecode ab Byte 6 abinterpretieren
              goto fertig;
            }
            optionals_ok:
            # Rest- und Keyword-Parameter behandeln.
            # args = restliche Argumentliste (noch nicht zu Ende)
            closure = *closure_; codevec = TheCclosure(closure)->clos_codevec;
            if (flags == 0)
              # Closure ohne REST oder KEY -> Argumentliste müßte zu Ende sein
              { goto fehler_zuviel; }
            elif (flags & bit(7)) # Key-Flag?
              # Closure mit Keywords.
              # args = restliche Argumentliste (noch nicht zu Ende)
              # Erst die Keyword-Parameter mit #<UNBOUND> vorbesetzen, dann
              # die restlichen Argumente auswerten und im Stack ablegen, dann
              # die Keywords zuordnen:
              { # evtl. den Rest-Parameter vorbesetzen:
                if (flags & bit(0)) { pushSTACK(unbound); }
                apply_cclosure_key: # Closure mit nur &KEY anspringen:
               {var reg1 object* key_args_pointer = args_end_pointer; # Pointer über Keyword-Parameter
                # alle Keyword-Parameter mit #<UNBOUND> vorbesetzen:
                { var reg1 uintC count = *(uintW*)(&TheSbvector(codevec)->data[CCHD+6]);
                  dotimesC(count,count, { pushSTACK(unbound); } );
                }
                {var reg1 object* rest_args_pointer = args_end_pointer; # Pointer über die restlichen Argumente
                 # alle weiteren Argumente auswerten und in den Stack:
                 var reg1 uintL argcount = 0; # Zähler für die restlichen Argumente
                 do { check_STACK();
                      pushSTACK(Cdr(args)); # restliche Argumentliste
                      eval(Car(args)); # nächstes Argument auswerten
                      args = STACK_0; STACK_0 = value1; # und in den Stack
                      argcount++;
                    }
                    while (consp(args));
                 # Argumentliste beendet.
                 if (!nullp(args)) goto fehler_dotted;
                 # Keywords zuordnen, Rest-Parameter bauen
                 # und evtl. restliche Argumente wegwerfen:
                 closure = match_cclosure_key(*closure_,argcount,key_args_pointer,rest_args_pointer);
                 codevec = TheCclosure(closure)->clos_codevec;
                 interpret_bytecode(closure,codevec,CCHD+10); # Bytecode ab Byte 10 abinterpretieren
              }}}
            else
              apply_cclosure_rest_nokey:
              # Closure mit nur REST, ohne KEY:
              # restlichen Argumente einzeln auswerten, zu einer Liste machen
              # args = restliche Argumentliste (noch nicht zu Ende)
              { pushSTACK(NIL); # bisher ausgewertete restliche Argumente
                pushSTACK(args); # restliche Argumente, unausgewertet
                do { args = STACK_0; STACK_0 = Cdr(args);
                     eval(Car(args)); # nächstes Argument auswerten
                     pushSTACK(value1);
                     # und auf die Liste consen:
                    {var reg1 object new_cons = allocate_cons();
                     Car(new_cons) = popSTACK();
                     Cdr(new_cons) = STACK_1;
                     STACK_1 = new_cons;
                   }}
                   while (mconsp(STACK_0));
                args = popSTACK();
                # Liste STACK_0 umdrehen und als REST-Parameter verwenden:
                nreverse(STACK_0);
                # Argumentliste beendet.
                if (!nullp(args)) goto fehler_dotted;
                apply_cclosure_nokey: # Closure ohne &KEY anspringen:
                closure = *closure_; codevec = TheCclosure(closure)->clos_codevec;
                interpret_bytecode(closure,codevec,CCHD+6); # Bytecode ab Byte 6 abinterpretieren
              }
            fertig: ;
          }
          #if STACKCHECKC
          if (!(STACK == STACKbefore)) # STACK so wie vorher?
            { abort(); } # nein -> ab in den Debugger
          #endif
          skipSTACK(1); # Closure wegwerfen
          unwind(); # EVAL-Frame auflösen
          return; # fertig
          # Gesammelte Fehlermeldungen:
          fehler_zuwenig: # Argumentliste args ist vorzeitig ein Atom
            if (!nullp(args)) goto fehler_dotted;
            setSTACK(STACK = STACKbefore); # STACK aufräumen
            closure = popSTACK();
            fehler_eval_zuwenig(TheCclosure(closure)->clos_name);
          fehler_zuviel: # Argumentliste args ist am Schluß nicht NIL
            if (atomp(args)) goto fehler_dotted;
            setSTACK(STACK = STACKbefore); # STACK aufräumen
            closure = popSTACK();
            fehler_eval_zuviel(TheCclosure(closure)->clos_name);
          fehler_dotted: # Argumentliste args endet mit Atom /= NIL
            setSTACK(STACK = STACKbefore); # STACK aufräumen
            closure = popSTACK();
            fehler_eval_dotted(TheCclosure(closure)->clos_name);
        }
        else
        # closure ist eine interpretierte Closure
        { var reg7 object* args_pointer = args_end_pointer; # Pointer über die Argumente
          var reg6 uintC args_on_stack = 0; # Anzahl der Argumente
          while (consp(args))
            { pushSTACK(Cdr(args)); # Listenrest retten
              eval(Car(args)); # nächstes Element auswerten
              args = STACK_0; STACK_0 = value1; # Auswertungsergebnis in den STACK
              args_on_stack += 1;
              if (((uintL)~(uintL)0 > ca_limit_1) && (args_on_stack > ca_limit_1))
                goto fehler_zuviel;
            }
          funcall_iclosure(*closure_,args_pointer,args_on_stack);
          skipSTACK(1); # Closure wegwerfen
          unwind(); # EVAL-Frame auflösen
          return; # fertig
        }
    }}


#          ----------------------- A P P L Y -----------------------

# später:
  local Values apply_subr (object fun, uintC args_on_stack, object other_args);
  local Values apply_closure (object fun, uintC args_on_stack, object other_args);

# UP: Wendet eine Funktion auf ihre Argumente an.
# apply(function,args_on_stack,other_args);
# > function: Funktion
# > Argumente: args_on_stack Argumente auf dem STACK,
#              restliche Argumentliste in other_args
# < STACK: aufgeräumt (d.h. STACK wird um args_on_stack erhöht)
# < mv_count/mv_space: Werte
# verändert STACK, kann GC auslösen
  global Values apply (object fun, uintC args_on_stack, object other_args);
  global Values apply(fun,args_on_stack,other_args)
    var reg2 object fun;
    var reg4 uintC args_on_stack;
    var reg3 object other_args;
    { # fun muß ein SUBR oder eine Closure oder ein Cons (LAMBDA ...) sein:
      var reg1 tint type = typecode(fun); # Typinfo
      if (type == subr_type) # SUBR ?
        { return_Values apply_subr(fun,args_on_stack,other_args); }
      elif (type == closure_type) # Closure ?
        { return_Values apply_closure(fun,args_on_stack,other_args); }
      elif (symbolp(fun)) # Symbol ?
        # Symbol anwenden: globale Definition Symbol_function(fun) gilt.
        { type = mtypecode(Symbol_function(fun)); # Typinfo davon
          if (type == subr_type) # SUBR -> anwenden
            { return_Values apply_subr(Symbol_function(fun),args_on_stack,other_args); }
          elif (type == closure_type) # Closure -> anwenden
            { return_Values apply_closure(Symbol_function(fun),args_on_stack,other_args); }
          elif (type == fsubr_type) # FSUBR -> Fehler
            { pushSTACK(fun);
              fehler(
                     DEUTSCH ? "APPLY: ~ ist eine Spezialform, keine Funktion." :
                     ENGLISH ? "APPLY: ~ is a special form, not a function" :
                     FRANCAIS ? "APPLY: ~ est une forme spéciale et non une fonction." :
                     ""
                    );
            }
          elif (mconsp(Symbol_function(fun))) # Macro-Cons -> Fehler
            { pushSTACK(fun);
              fehler(
                     DEUTSCH ? "APPLY: ~ ist ein Macro, keine Funktion." :
                     ENGLISH ? "APPLY: ~ is a macro, not a function" :
                     FRANCAIS ? "APPLY: ~ est un macro et non une fonction." :
                     ""
                    );
            }
          else
            # wenn kein SUBR, keine Closure, kein FSUBR, kein Cons:
            # Symbol_function(fun) muß #<UNBOUND> sein.
            { pushSTACK(fun);
              fehler(
                     DEUTSCH ? "APPLY: Die Funktion ~ ist undefiniert." :
                     ENGLISH ? "APPLY: the function ~ is undefined" :
                     FRANCAIS ? "APPLY: La fonction ~ n'est pas définie." :
                     ""
                    );
            }
        }
      elif (consp(fun) && eq(Car(fun),S(lambda))) # Cons (LAMBDA ...) ?
        # Lambda-Ausdruck: zu einer Funktion mit leerem Environment machen
        { pushSTACK(other_args); # Argumentliste retten
          # leeres Environment bauen:
         {var reg5 environment* env5;
          make_STACK_env(NIL,NIL,NIL,NIL,O(top_decl_env), env5 = );
          fun = get_closure(Cdr(fun), # Lambdabody (lambda-list {decl|doc} . body)
                            S(Klambda), # :LAMBDA als Name
                            env5); # im leeren Environment
          skipSTACK(5); # Environment wieder vergessen
          other_args = popSTACK();
          # und neu erzeugte Closure anwenden:
          return_Values apply_closure(fun,args_on_stack,other_args);
        }}
      else
        { pushSTACK(fun);
          fehler(
                 DEUTSCH ? "APPLY: ~ ist keine Funktionsbezeichnung." :
                 ENGLISH ? "APPLY: ~ is not a function name" :
                 FRANCAIS ? "APPLY: ~ n'est pas un nom de fonction." :
                 ""
                );
        }
    }

# Fehler wegen punktierter Argumentliste
# > name: Name der Funktion
  local nonreturning void fehler_apply_dotted (object name);
  local nonreturning void fehler_apply_dotted(name)
    var reg1 object name;
    { pushSTACK(name);
      fehler(
             DEUTSCH ? "APPLY: Argumentliste für ~ ist dotted." :
             ENGLISH ? "APPLY: argument list given to ~ is dotted" :
             FRANCAIS ? "APPLY: La liste d'arguments pour ~ est pointée." :
             ""
            );
    }

# Fehler wegen zu vielen Argumenten
# > name: Name der Funktion
  local nonreturning void fehler_apply_zuviel (object name);
  local nonreturning void fehler_apply_zuviel(name)
    var reg1 object name;
    { pushSTACK(name);
      fehler(
             DEUTSCH ? "APPLY: Zu viele Argumente für ~" :
             ENGLISH ? "APPLY: too many arguments given to ~" :
             FRANCAIS ? "APPLY: Trop d'arguments pour ~" :
             ""
            );
    }

# Fehler wegen zu wenig Argumenten
# > name: Name der Funktion
  local nonreturning void fehler_apply_zuwenig (object name);
  local nonreturning void fehler_apply_zuwenig(name)
    var reg1 object name;
    { pushSTACK(name);
      fehler(
             DEUTSCH ? "APPLY: Zu wenig Argumente für ~" :
             ENGLISH ? "APPLY: too few arguments given to ~" :
             FRANCAIS ? "APPLY: Trop peu d'arguments pour ~" :
             ""
            );
    }

# Fehler wegen zu vielen Argumenten für ein SUBR
# > fun: Funktion, ein SUBR
  local nonreturning void fehler_subr_zuviel (object fun);
  #define fehler_subr_zuviel(fun)  fehler_apply_zuviel(TheSubr(fun)->name)

# Fehler wegen zu wenig Argumenten für ein SUBR
# > fun: Funktion, ein SUBR
  local nonreturning void fehler_subr_zuwenig (object fun);
  #define fehler_subr_zuwenig(fun)  fehler_apply_zuwenig(TheSubr(fun)->name)

# In APPLY: Wendet ein SUBR auf eine Argumentliste an, räumt den STACK auf
# und liefert die Werte.
# apply_subr(fun,args_on_stack,other_args);
# > fun: Funktion, ein SUBR
# > Argumente: args_on_stack Argumente auf dem STACK,
#              restliche Argumentliste in other_args
# < STACK: aufgeräumt (d.h. STACK wird um args_on_stack erhöht)
# < mv_count/mv_space: Werte
# verändert STACK, kann GC auslösen
  local Values apply_subr(fun,args_on_stack,args)
    var reg4 object fun;
    var reg3 uintC args_on_stack;
    var reg2 object args;
    {
      #if STACKCHECKS
      var reg9 object* args_pointer = args_end_pointer STACKop (uintL)args_on_stack; # Pointer über die Argumente
      #endif
      var reg9 object* key_args_pointer; # Pointer über die Keyword-Argumente
      var reg9 object* rest_args_pointer; # Pointer über die restlichen Argumente
      var reg8 uintL argcount; # Anzahl der restlichen Argumente
      # Argumente in den STACK legen:
      # erst ein Dispatch für die wichtigsten Fälle:
      switch (TheSubr(fun)->argtype)
        { # Macro für ein required-Argument:
          #define REQ_ARG()  \
            { if (args_on_stack>0) { args_on_stack--; }                      \
              elif (consp(args)) { pushSTACK(Car(args)); args = Cdr(args); } \
              else goto fehler_zuwenig;                                      \
            }
          # Macro für das n-letzte optional-Argument:
          #define OPT_ARG(n)  \
            { if (args_on_stack>0) { args_on_stack--; }                      \
              elif (consp(args)) { pushSTACK(Car(args)); args = Cdr(args); } \
              else goto unbound_optional_##n;                                \
            }
          case (uintW)subr_argtype_4_0:
            # SUBR mit 4 required-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_3_0:
            # SUBR mit 3 required-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_2_0:
            # SUBR mit 2 required-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_1_0:
            # SUBR mit 1 required-Argument
            REQ_ARG();
          case (uintW)subr_argtype_0_0:
            # SUBR ohne Argumente
            if ((args_on_stack>0) || consp(args)) goto fehler_zuviel;
            goto apply_subr_norest;
          case (uintW)subr_argtype_4_1:
            # SUBR mit 4 required-Argumenten und 1 optional-Argument
            REQ_ARG();
          case (uintW)subr_argtype_3_1:
            # SUBR mit 3 required-Argumenten und 1 optional-Argument
            REQ_ARG();
          case (uintW)subr_argtype_2_1:
            # SUBR mit 2 required-Argumenten und 1 optional-Argument
            REQ_ARG();
          case (uintW)subr_argtype_1_1:
            # SUBR mit 1 required-Argument und 1 optional-Argument
            REQ_ARG();
          case (uintW)subr_argtype_0_1:
            # SUBR mit 1 optional-Argument
            OPT_ARG(1);
            if ((args_on_stack>0) || consp(args)) goto fehler_zuviel;
            goto apply_subr_norest;
          case (uintW)subr_argtype_2_2:
            # SUBR mit 2 required-Argumenten und 2 optional-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_1_2:
            # SUBR mit 1 required-Argument und 2 optional-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_0_2:
            # SUBR mit 2 optional-Argumenten
            OPT_ARG(2);
            OPT_ARG(1);
            if ((args_on_stack>0) || consp(args)) goto fehler_zuviel;
            goto apply_subr_norest;
          case (uintW)subr_argtype_0_5:
            # SUBR mit 5 optional-Argumenten
            OPT_ARG(5);
          case (uintW)subr_argtype_0_4:
            # SUBR mit 4 optional-Argumenten
            OPT_ARG(4);
          case (uintW)subr_argtype_0_3:
            # SUBR mit 3 optional-Argumenten
            OPT_ARG(3);
            OPT_ARG(2);
            OPT_ARG(1);
            if ((args_on_stack>0) || consp(args)) goto fehler_zuviel;
            goto apply_subr_norest;
          unbound_optional_5: # Noch 5 optionale Argumente, aber args_on_stack=0 und atomp(args)
            pushSTACK(unbound);
          unbound_optional_4: # Noch 4 optionale Argumente, aber args_on_stack=0 und atomp(args)
            pushSTACK(unbound);
          unbound_optional_3: # Noch 3 optionale Argumente, aber args_on_stack=0 und atomp(args)
            pushSTACK(unbound);
          unbound_optional_2: # Noch 2 optionale Argumente, aber args_on_stack=0 und atomp(args)
            pushSTACK(unbound);
          unbound_optional_1: # Noch 1 optionales Argument, aber args_on_stack=0 und atomp(args)
            pushSTACK(unbound);
            goto apply_subr_norest;
          case (uintW)subr_argtype_3_0_rest:
            # SUBR mit 3 required-Argumenten und weiteren Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_2_0_rest:
            # SUBR mit 2 required-Argumenten und weiteren Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_1_0_rest:
            # SUBR mit 1 required-Argument und weiteren Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_0_0_rest:
            # SUBR mit weiteren Argumenten
            if (args_on_stack==0)
              goto apply_subr_rest_onlylist;
              else
              goto apply_subr_rest_withlist;
          case (uintW)subr_argtype_4_0_key:
            # SUBR mit 4 required-Argumenten und Keyword-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_3_0_key:
            # SUBR mit 3 required-Argumenten und Keyword-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_2_0_key:
            # SUBR mit 2 required-Argumenten und Keyword-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_1_0_key:
            # SUBR mit 1 required-Argument und Keyword-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_0_0_key:
            # SUBR mit Keyword-Argumenten
            if ((args_on_stack==0) && atomp(args)) goto unbound_optional_key_0;
            goto apply_subr_key;
          case (uintW)subr_argtype_1_1_key:
            # SUBR mit 1 required-Argument, 1 optional-Argument und Keyword-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_0_1_key:
            # SUBR mit 1 optional-Argument und Keyword-Argumenten
            OPT_ARG(key_1);
            if ((args_on_stack==0) && atomp(args)) goto unbound_optional_key_0;
            goto apply_subr_key;
          case (uintW)subr_argtype_1_2_key:
            # SUBR mit 1 required-Argument, 2 optional-Argumenten und Keyword-Argumenten
            REQ_ARG();
            OPT_ARG(key_2);
            OPT_ARG(key_1);
            if ((args_on_stack==0) && atomp(args)) goto unbound_optional_key_0;
            goto apply_subr_key;
          unbound_optional_key_2: # Noch 2 optionale Argumente, aber args_on_stack=0 und atomp(args)
            pushSTACK(unbound);
          unbound_optional_key_1: # Noch 1 optionales Argument, aber args_on_stack=0 und atomp(args)
            pushSTACK(unbound);
          unbound_optional_key_0: # Vor den Keywords ist args_on_stack=0 und atomp(args)
            { var reg1 uintC count;
              dotimesC(count,TheSubr(fun)->key_anz, { pushSTACK(unbound); } );
            }
            goto apply_subr_norest;
          default: NOTREACHED
          #undef OPT_ARG
          #undef REQ_ARG
        }
      # Nun die allgemeine Version:
     {var reg5 uintC req_anz = TheSubr(fun)->req_anz;
      var reg6 uintC opt_anz = TheSubr(fun)->opt_anz;
      var reg7 uintC key_anz = TheSubr(fun)->key_anz;
      if (args_on_stack < req_anz)
        # weniger Argumente da als verlangt
        { req_anz = req_anz - args_on_stack; # soviele müssen noch auf den STACK
          # Platz auf dem STACK reservieren:
          get_space_on_STACK(sizeof(object) * (uintL)(req_anz + opt_anz + key_anz));
          # required Parameter in den Stack ablegen:
          { var reg1 uintC count;
            dotimespC(count,req_anz,
              { if (atomp(args)) { goto fehler_zuwenig; }
                pushSTACK(Car(args)); # nächstes Argument ablegen
                args = Cdr(args);
              });
          }
          goto optionals_from_list;
        }
      args_on_stack -= req_anz; # verbleibende Anzahl
      if (args_on_stack < opt_anz)
        # Argumente im Stack reichen nicht für die optionalen
        { opt_anz = opt_anz - args_on_stack; # soviele müssen noch auf den STACK
          # Platz auf dem STACK reservieren:
          get_space_on_STACK(sizeof(object) * (uintL)(opt_anz + key_anz));
          optionals_from_list:
          # optionale Parameter in den Stack ablegen:
          { var reg1 uintC count = opt_anz;
            loop
              { if (atomp(args)) break; # Argumentliste zu Ende?
                if (count==0) goto optionals_ok; # alle optionalen Parameter versorgt?
                count--;
                pushSTACK(Car(args)); # nächstes Argument ablegen
                args = Cdr(args);
              }
            # Argumentliste beendet.
            # Alle weiteren count optionalen Parameter bekommen den "Wert"
            # #<UNBOUND>, auch die Keyword-Parameter:
            dotimesC(count,count + key_anz, { pushSTACK(unbound); } );
            if (TheSubr(fun)->rest_flag == subr_rest) # &REST-Flag?
              # ja -> 0 zusätzliche Argumente:
              { argcount = 0; rest_args_pointer = args_end_pointer;
                goto apply_subr_rest;
              }
              else
              # nein -> nichts zu tun
              { goto apply_subr_norest; }
          }
          optionals_ok: # optionale Argumente OK, (nichtleere) Liste weiter abarbeiten
          if (TheSubr(fun)->key_flag == subr_nokey)
            # SUBR ohne KEY
            { if (TheSubr(fun)->rest_flag == subr_norest)
                # SUBR ohne REST oder KEY
                { fehler_subr_zuviel(fun); } # zuviele Argumente
                else
                # SUBR mit nur REST, ohne KEY
                apply_subr_rest_onlylist:
                { argcount = 0; rest_args_pointer = args_end_pointer;
                  goto rest_from_list;
                }
            }
            else
            # SUBR mit KEY
            { key_args_pointer = args_end_pointer;
              { var reg1 uintC count;
                dotimesC(count,key_anz, { pushSTACK(unbound); } );
              }
              rest_args_pointer = args_end_pointer;
              argcount = 0;
              goto key_from_list;
            }
        }
      args_on_stack -= opt_anz; # verbleibende Anzahl
      if (TheSubr(fun)->key_flag == subr_nokey)
        # SUBR ohne KEY
        { if (TheSubr(fun)->rest_flag == subr_norest)
            # SUBR ohne REST oder KEY
            { if ((args_on_stack>0) || consp(args)) # noch Argumente?
                { fehler_subr_zuviel(fun); }
              goto apply_subr_norest;
            }
            else
            # SUBR mit nur REST, ohne KEY
            apply_subr_rest_withlist:
            { argcount = args_on_stack;
              rest_args_pointer = args_end_pointer STACKop argcount;
              rest_from_list: # restliche Argumente aus der Liste nehmen
              while (consp(args))
                { check_STACK(); pushSTACK(Car(args)); # nächstes Argument in den Stack
                  args = Cdr(args);
                  argcount++;
                }
              if (((uintL)~(uintL)0 > ca_limit_1) && (argcount > ca_limit_1)) # zu viele Argumente?
                { goto fehler_zuviel; }
              goto apply_subr_rest;
            }
        }
        else
        # SUBR mit Keywords.
        { if (FALSE)
            apply_subr_key: { key_anz = TheSubr(fun)->key_anz; }
          # restliche Argumente im STACK nach unten schieben und dadurch
          # Platz für die Keyword-Parameter schaffen:
          argcount = args_on_stack;
          get_space_on_STACK(sizeof(object) * (uintL)key_anz);
          {var reg9 object* new_args_end_pointer = args_end_pointer STACKop -(uintL)key_anz;
           var reg1 object* ptr1 = args_end_pointer;
           var reg1 object* ptr2 = new_args_end_pointer;
           var reg1 uintC count;
           dotimesC(count,args_on_stack, { BEFORE(ptr2) = BEFORE(ptr1); } );
           key_args_pointer = ptr1;
           rest_args_pointer = ptr2;
           dotimesC(count,key_anz, { NEXT(ptr1) = unbound; } );
           set_args_end_pointer(new_args_end_pointer);
          }
          key_from_list: # restliche Argumente für Keywords aus der Liste nehmen
          while (consp(args))
            { check_STACK(); pushSTACK(Car(args)); # nächstes Argument in den Stack
              args = Cdr(args);
              argcount++;
            }
          # Keywords zuordnen und evtl. restliche Argumente wegwerfen:
          match_subr_key(fun,argcount,key_args_pointer,rest_args_pointer);
          if (TheSubr(fun)->rest_flag == subr_norest)
            # SUBR ohne &REST-Flag:
            apply_subr_norest:
            { if (!nullp(args)) goto fehler_dotted;
              subr_self = fun;
              (*(subr_norest_function*)(TheSubr(fun)->function))();
            }
            else
            # SUBR mit &REST-Flag:
            apply_subr_rest:
            { if (!nullp(args)) goto fehler_dotted;
              subr_self = fun;
              (*(subr_rest_function*)(TheSubr(fun)->function))
               (argcount,rest_args_pointer);
            }
     }  }
      #if STACKCHECKS
      if (!(args_pointer == args_end_pointer)) # Stack aufgeräumt?
        { abort(); } # nein -> ab in den Debugger
      #endif
      return; # fertig
      # Gesammelte Fehlermeldungen:
      fehler_zuwenig: fehler_subr_zuwenig(fun);
      fehler_zuviel: fehler_subr_zuviel(fun);
      fehler_dotted: fehler_apply_dotted(TheSubr(fun)->name);
    }

# Fehler wegen zu vielen Argumenten für eine Closure
# > closure: Funktion, eine Closure
  local nonreturning void fehler_closure_zuviel (object closure);
  #define fehler_closure_zuviel(closure)  fehler_apply_zuviel(closure)

# Fehler wegen zu wenig Argumenten für eine Closure
# > closure: Funktion, eine Closure
  local nonreturning void fehler_closure_zuwenig (object closure);
  #define fehler_closure_zuwenig(closure)  fehler_apply_zuwenig(closure)

# In APPLY: Wendet eine Closure auf eine Argumentliste an, räumt den STACK auf
# und liefert die Werte.
# apply_closure(fun,args_on_stack,other_args);
# > fun: Funktion, eine Closure
# > Argumente: args_on_stack Argumente auf dem STACK,
#              restliche Argumentliste in other_args
# < STACK: aufgeräumt (d.h. STACK wird um args_on_stack erhöht)
# < mv_count/mv_space: Werte
# verändert STACK, kann GC auslösen
  local Values apply_closure(closure,args_on_stack,args)
    var reg5 object closure;
    var reg3 uintC args_on_stack;
    var reg2 object args;
    { if (m_simple_bit_vector_p(TheClosure(closure)->clos_codevec))
        # closure ist eine compilierte Closure
        {
          #if STACKCHECKC
          var reg9 object* args_pointer = args_end_pointer STACKop (uintL)args_on_stack; # Pointer über die Argumente
          #endif
          var reg4 object codevec = TheCclosure(closure)->clos_codevec; # Code-Vektor
          var reg9 object* key_args_pointer; # Pointer über die Keyword-Argumente
          var reg9 object* rest_args_pointer; # Pointer über die restlichen Argumente
          var reg8 uintL argcount; # Anzahl der restlichen Argumente
          check_SP(); check_STACK();
          # Argumente in den STACK legen:
          # erst ein Dispatch für die wichtigsten Fälle:
          switch (TheSbvector(codevec)->data[CCHD+5])
            { # Macro für ein required-Argument:
              #define REQ_ARG()  \
                { if (args_on_stack>0) { args_on_stack--; }                      \
                  elif (consp(args)) { pushSTACK(Car(args)); args = Cdr(args); } \
                  else goto fehler_zuwenig;                                      \
                }
              # Macro für das n-letzte optional-Argument:
              #define OPT_ARG(n)  \
                { if (args_on_stack>0) { args_on_stack--; }                      \
                  elif (consp(args)) { pushSTACK(Car(args)); args = Cdr(args); } \
                  else goto unbound_optional_##n;                                \
                }
              case (uintB)cclos_argtype_5_0:
                # 5 required-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_4_0:
                # 4 required-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_3_0:
                # 3 required-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_2_0:
                # 2 required-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_1_0:
                # 1 required-Argument
                REQ_ARG();
              case (uintB)cclos_argtype_0_0:
                # keine Argumente
                noch_0_opt_args:
                if (args_on_stack>0) goto fehler_zuviel;
                if (!nullp(args))
                  { if (consp(args))
                      goto fehler_zuviel;
                      else
                      goto fehler_dotted;
                  }
                goto apply_cclosure_nokey;
              case (uintB)cclos_argtype_4_1:
                # 4 required-Argumente und 1 optional-Argument
                REQ_ARG();
              case (uintB)cclos_argtype_3_1:
                # 3 required-Argumente und 1 optional-Argument
                REQ_ARG();
              case (uintB)cclos_argtype_2_1:
                # 2 required-Argumente und 1 optional-Argument
                REQ_ARG();
              case (uintB)cclos_argtype_1_1:
                # 1 required-Argument und 1 optional-Argument
                REQ_ARG();
              case (uintB)cclos_argtype_0_1:
                # 1 optional-Argument
                noch_1_opt_args:
                OPT_ARG(1);
                goto noch_0_opt_args;
              case (uintB)cclos_argtype_3_2:
                # 3 required-Argumente und 2 optional-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_2_2:
                # 2 required-Argumente und 2 optional-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_1_2:
                # 1 required-Argument und 2 optional-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_0_2:
                # 2 optional-Argumente
                noch_2_opt_args:
                OPT_ARG(2);
                goto noch_1_opt_args;
              case (uintB)cclos_argtype_2_3:
                # 2 required-Argumente und 3 optional-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_1_3:
                # 1 required-Argument und 3 optional-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_0_3:
                # 3 optional-Argumente
                noch_3_opt_args:
                OPT_ARG(3);
                goto noch_2_opt_args;
              case (uintB)cclos_argtype_1_4:
                # 1 required-Argument und 4 optional-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_0_4:
                # 4 optional-Argumente
                noch_4_opt_args:
                OPT_ARG(4);
                goto noch_3_opt_args;
              case (uintB)cclos_argtype_0_5:
                # 5 optional-Argumente
                OPT_ARG(5);
                goto noch_4_opt_args;
              unbound_optional_5: # Noch 5 optionale Argumente, aber args_on_stack=0 und atomp(args)
                pushSTACK(unbound);
              unbound_optional_4: # Noch 4 optionale Argumente, aber args_on_stack=0 und atomp(args)
                pushSTACK(unbound);
              unbound_optional_3: # Noch 3 optionale Argumente, aber args_on_stack=0 und atomp(args)
                pushSTACK(unbound);
              unbound_optional_2: # Noch 2 optionale Argumente, aber args_on_stack=0 und atomp(args)
                pushSTACK(unbound);
              unbound_optional_1: # Noch 1 optionales Argument, aber args_on_stack=0 und atomp(args)
                pushSTACK(unbound);
                if (!nullp(args)) goto fehler_dotted;
                goto apply_cclosure_nokey;
              case (uintB)cclos_argtype_4_0_rest:
                # 4 required-Argumente, Rest-Parameter
                REQ_ARG();
              case (uintB)cclos_argtype_3_0_rest:
                # 3 required-Argumente, Rest-Parameter
                REQ_ARG();
              case (uintB)cclos_argtype_2_0_rest:
                # 2 required-Argumente, Rest-Parameter
                REQ_ARG();
              case (uintB)cclos_argtype_1_0_rest:
                # 1 required-Argument, Rest-Parameter
                REQ_ARG();
              case (uintB)cclos_argtype_0_0_rest:
                # keine Argumente, Rest-Parameter
                goto apply_cclosure_rest_nokey;
              case (uintB)cclos_argtype_4_0_key:
                # 4 required-Argumente, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_3_0_key:
                # 3 required-Argumente, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_2_0_key:
                # 2 required-Argumente, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_1_0_key:
                # 1 required-Argument, Keyword-Argumente
                REQ_ARG();
                noch_0_opt_args_key:
              case (uintB)cclos_argtype_0_0_key:
                # nur Keyword-Argumente
                if ((args_on_stack==0) && atomp(args)) goto unbound_optional_key_0;
                goto apply_cclosure_key_withlist;
              case (uintB)cclos_argtype_3_1_key:
                # 3 required-Argumente und 1 optional-Argument, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_2_1_key:
                # 2 required-Argumente und 1 optional-Argument, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_1_1_key:
                # 1 required-Argument und 1 optional-Argument, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_0_1_key:
                # 1 optional-Argument, Keyword-Argumente
                noch_1_opt_args_key:
                OPT_ARG(key_1);
                goto noch_0_opt_args_key;
              case (uintB)cclos_argtype_2_2_key:
                # 2 required-Argumente und 2 optional-Argumente, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_1_2_key:
                # 1 required-Argument und 2 optional-Argumente, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_0_2_key:
                # 2 optional-Argumente, Keyword-Argumente
                noch_2_opt_args_key:
                OPT_ARG(key_2);
                goto noch_1_opt_args_key;
              case (uintB)cclos_argtype_1_3_key:
                # 1 required-Argument und 3 optional-Argumente, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_0_3_key:
                # 3 optional-Argumente, Keyword-Argumente
                noch_3_opt_args_key:
                OPT_ARG(key_3);
                goto noch_2_opt_args_key;
              case (uintB)cclos_argtype_0_4_key:
                # 4 optional-Argumente, Keyword-Argumente
                OPT_ARG(key_4);
                goto noch_3_opt_args_key;
              unbound_optional_key_4: # Noch 4 optionale Argumente, aber args_on_stack=0 und atomp(args)
                pushSTACK(unbound);
              unbound_optional_key_3: # Noch 3 optionale Argumente, aber args_on_stack=0 und atomp(args)
                pushSTACK(unbound);
              unbound_optional_key_2: # Noch 2 optionale Argumente, aber args_on_stack=0 und atomp(args)
                pushSTACK(unbound);
              unbound_optional_key_1: # Noch 1 optionales Argument, aber args_on_stack=0 und atomp(args)
                pushSTACK(unbound);
              unbound_optional_key_0: # Vor den Keywords ist args_on_stack=0 und atomp(args)
                if (!nullp(args)) goto fehler_dotted;
                goto apply_cclosure_key_noargs;
              case (uintB)cclos_argtype_default:
                # Allgemeine Version
                break;
              default: NOTREACHED
              #undef OPT_ARG
              #undef REQ_ARG
            }
          # Nun die allgemeine Version:
         {var reg5 uintC req_anz = *(uintW*)(&TheSbvector(codevec)->data[CCHD+0]); # Anzahl required Parameter
          var reg6 uintC opt_anz = *(uintW*)(&TheSbvector(codevec)->data[CCHD+2]); # Anzahl optionale Parameter
          var reg7 uintB flags = TheSbvector(codevec)->data[CCHD+4]; # Flags
          if (args_on_stack < req_anz)
            # weniger Argumente da als verlangt
            { req_anz = req_anz - args_on_stack; # soviele müssen noch auf den STACK
              # Platz auf dem STACK reservieren:
              get_space_on_STACK(sizeof(object) * (uintL)(req_anz + opt_anz));
              # required Parameter in den Stack ablegen:
              { var reg1 uintC count;
                dotimespC(count,req_anz,
                  { if (atomp(args)) { goto fehler_zuwenig; }
                    pushSTACK(Car(args)); # nächstes Argument ablegen
                    args = Cdr(args);
                  });
              }
              goto optionals_from_list;
            }
          args_on_stack -= req_anz; # verbleibende Anzahl
          if (args_on_stack < opt_anz)
            # Argumente im Stack reichen nicht für die optionalen
            { opt_anz = opt_anz - args_on_stack; # soviele müssen noch auf den STACK
              # Platz auf dem STACK reservieren:
              get_space_on_STACK(sizeof(object) * (uintL)opt_anz);
              optionals_from_list:
              # optionale Parameter in den Stack ablegen:
              { var reg1 uintC count = opt_anz;
                loop
                  { if (atomp(args)) break; # Argumentliste zu Ende?
                    if (count==0) goto optionals_ok; # alle optionalen Parameter versorgt?
                    count--;
                    pushSTACK(Car(args)); # nächstes Argument ablegen
                    args = Cdr(args);
                  }
                # Argumentliste beendet.
                if (!nullp(args)) goto fehler_dotted;
                # Alle weiteren count optionalen Parameter bekommen den "Wert"
                # #<UNBOUND>, der &REST-Parameter den Wert NIL,
                # die Keyword-Parameter den Wert #<UNBOUND> :
                dotimesC(count,count, { pushSTACK(unbound); } );
                if (flags & bit(0)) # &REST-Flag?
                  { pushSTACK(NIL); } # ja -> mit NIL initialisieren
                if (flags & bit(7)) # &KEY-Flag?
                  apply_cclosure_key_noargs:
                  { var reg1 uintC key_anz = *(uintW*)(&TheSbvector(codevec)->data[CCHD+6]); # Anzahl Keyword-Parameter
                    get_space_on_STACK(sizeof(object) * (uintL)key_anz);
                    {var reg1 uintC count;
                     dotimesC(count,key_anz, { pushSTACK(unbound); } ); # mit #<UNBOUND> initialisieren
                    }
                    goto apply_cclosure_key;
                  }
                  else
                  goto apply_cclosure_nokey;
              }
              optionals_ok:
              # Rest- und Keyword-Parameter behandeln.
              # args = restliche Argumentliste (noch nicht zu Ende)
              if (flags == 0)
                # Closure ohne REST oder KEY -> Argumentliste müßte zu Ende sein
                { goto fehler_zuviel; }
              # evtl. den Rest-Parameter füllen:
              if (flags & bit(0))
                { pushSTACK(args); }
              if (flags & bit(7)) # Key-Flag?
                # Closure mit Keywords.
                # args = restliche Argumentliste (noch nicht zu Ende)
                # Erst die Keyword-Parameter mit #<UNBOUND> vorbesetzen,
                # dann die restlichen Argumente im Stack ablegen,
                # dann die Keywords zuordnen:
                { key_args_pointer = args_end_pointer; # Pointer über Keyword-Parameter
                  # alle Keyword-Parameter mit #<UNBOUND> vorbesetzen:
                  { var reg1 uintC count = *(uintW*)(&TheSbvector(codevec)->data[CCHD+6]);
                    dotimesC(count,count, { pushSTACK(unbound); } );
                  }
                  rest_args_pointer = args_end_pointer; # Pointer über die restlichen Argumente
                  argcount = 0; # Zähler für die restlichen Argumente
                  goto key_from_list;
                }
                else
                # Closure mit nur REST, ohne KEY:
                goto apply_cclosure_nokey;
            }
          args_on_stack -= opt_anz; # verbleibende Anzahl
          if (flags & bit(7)) # Key-Flag?
            { if (FALSE)
                apply_cclosure_key_withlist:
                { flags = TheSbvector(codevec)->data[CCHD+4]; } # Flags initialisieren!
            # Closure mit Keywords
             {var reg1 uintC key_anz = *(uintW*)(&TheSbvector(codevec)->data[CCHD+6]); # Anzahl Keyword-Parameter
              # restliche Argumente im STACK nach unten schieben und dadurch
              # Platz für die Keyword-Parameter (und evtl. Rest-Parameter)
              # schaffen:
              var reg1 uintL shift = key_anz;
              if (flags & bit(0)) { shift++; } # evtl. 1 mehr für Rest-Parameter
              argcount = args_on_stack;
              get_space_on_STACK(sizeof(object) * shift);
              {var reg9 object* new_args_end_pointer = args_end_pointer STACKop -shift;
               var reg1 object* ptr1 = args_end_pointer;
               var reg1 object* ptr2 = new_args_end_pointer;
               var reg1 uintC count;
               dotimesC(count,args_on_stack, { BEFORE(ptr2) = BEFORE(ptr1); } );
               if (flags & bit(0)) { NEXT(ptr1) = unbound; } # Rest-Parameter
               key_args_pointer = ptr1;
               rest_args_pointer = ptr2;
               dotimesC(count,key_anz, { NEXT(ptr1) = unbound; } );
               set_args_end_pointer(new_args_end_pointer);
              }
              key_from_list: # restliche Argumente für Keywords aus der Liste nehmen
              while (consp(args))
                { check_STACK(); pushSTACK(Car(args)); # nächstes Argument in den Stack
                  args = Cdr(args);
                  argcount++;
                }
              # Argumentliste beendet.
              if (!nullp(args)) goto fehler_dotted;
              # Keywords zuordnen, Rest-Parameter bauen
              # und evtl. restliche Argumente wegwerfen:
              closure = match_cclosure_key(closure,argcount,key_args_pointer,rest_args_pointer);
              codevec = TheCclosure(closure)->clos_codevec;
              apply_cclosure_key:
              interpret_bytecode(closure,codevec,CCHD+10); # Bytecode ab Byte 10 abinterpretieren
            }}
          elif (flags & bit(0))
            apply_cclosure_rest_nokey:
            # Closure mit nur REST, ohne KEY:
            { # muß noch args_on_stack Argumente aus dem Stack auf args consen:
              pushSTACK(args);
              pushSTACK(closure); # Closure muß gerettet werden
              dotimesC(args_on_stack,args_on_stack,
                { var reg1 object new_cons = allocate_cons();
                  Cdr(new_cons) = STACK_1;
                  Car(new_cons) = STACK_2; # nächstes Argument draufconsen
                  STACK_2 = new_cons;
                  STACK_1 = STACK_0; skipSTACK(1);
                });
              closure = popSTACK(); codevec = TheCclosure(closure)->clos_codevec;
              goto apply_cclosure_nokey;
            }
          else
            # Closure ohne REST oder KEY
            { if ((args_on_stack>0) || consp(args)) # noch Argumente?
                goto fehler_zuviel;
              apply_cclosure_nokey: # Closure ohne &KEY anspringen:
              interpret_bytecode(closure,codevec,CCHD+6); # Bytecode ab Byte 6 abinterpretieren
            }
         }
          #if STACKCHECKC
          if (!(args_pointer == args_end_pointer)) # Stack aufgeräumt?
            { abort(); } # nein -> ab in den Debugger
          #endif
          return; # fertig
          # Gesammelte Fehlermeldungen:
          fehler_zuwenig: fehler_closure_zuwenig(closure);
          fehler_zuviel: fehler_closure_zuviel(closure);
          fehler_dotted: fehler_apply_dotted(closure);
        }
        else
        # closure ist eine interpretierte Closure
        { while (consp(args)) # Noch Argumente in der Liste?
            { pushSTACK(Car(args)); # nächstes Element in den STACK
              args = Cdr(args);
              args_on_stack += 1;
              if (((uintL)~(uintL)0 > ca_limit_1) && (args_on_stack > ca_limit_1))
                goto fehler_zuviel;
            }
          funcall_iclosure(closure,args_end_pointer STACKop (uintL)args_on_stack,args_on_stack);
        }
    }


#        ----------------------- F U N C A L L -----------------------

# später:
  local Values funcall_subr (object fun, uintC args_on_stack);
  local Values funcall_closure (object fun, uintC args_on_stack);

# UP: Wendet eine Funktion auf ihre Argumente an.
# funcall(function,argcount);
# > function: Funktion
# > Argumente: argcount Argumente auf dem STACK
# < STACK: aufgeräumt (d.h. STACK wird um argcount erhöht)
# < mv_count/mv_space: Werte
# verändert STACK, kann GC auslösen
  global Values funcall (object fun, uintC argcount);
  global Values funcall(fun,args_on_stack)
    var reg2 object fun;
    var reg3 uintC args_on_stack;
    { # fun muß ein SUBR oder eine Closure oder ein Cons (LAMBDA ...) sein:
      var reg1 tint type = typecode(fun); # Typinfo
      if (type == subr_type) # SUBR ?
        { return_Values funcall_subr(fun,args_on_stack); }
      elif (type == closure_type) # Closure ?
        { return_Values funcall_closure(fun,args_on_stack); }
      elif (symbolp(fun)) # Symbol ?
        # Symbol anwenden: globale Definition Symbol_function(fun) gilt.
        { type = mtypecode(Symbol_function(fun)); # Typinfo davon
          if (type == subr_type) # SUBR -> anwenden
            { return_Values funcall_subr(Symbol_function(fun),args_on_stack); }
          elif (type == closure_type) # Closure -> anwenden
            { return_Values funcall_closure(Symbol_function(fun),args_on_stack); }
          elif (type == fsubr_type) # FSUBR -> Fehler
            { pushSTACK(fun);
              fehler(
                     DEUTSCH ? "FUNCALL: ~ ist eine Spezialform, keine Funktion." :
                     ENGLISH ? "FUNCALL: ~ is a special form, not a function" :
                     FRANCAIS ? "FUNCALL: ~ est une forme spéciale et non une fonction." :
                     ""
                    );
            }
          elif (mconsp(Symbol_function(fun))) # Macro-Cons -> Fehler
            { pushSTACK(fun);
              fehler(
                     DEUTSCH ? "FUNCALL: ~ ist ein Macro, keine Funktion." :
                     ENGLISH ? "FUNCALL: ~ is a macro, not a function" :
                     FRANCAIS ? "FUNCALL: ~ est un macro et non une fonction." :
                     ""
                    );
            }
          else
            # wenn kein SUBR, keine Closure, kein FSUBR, kein Cons:
            # Symbol_function(fun) muß #<UNBOUND> sein.
            { pushSTACK(fun);
              fehler(
                     DEUTSCH ? "FUNCALL: Die Funktion ~ ist undefiniert." :
                     ENGLISH ? "FUNCALL: the function ~ is undefined" :
                     FRANCAIS ? "FUNCALL: La fonction ~ n'est pas définie." :
                     ""
                    );
            }
        }
      elif (consp(fun) && eq(Car(fun),S(lambda))) # Cons (LAMBDA ...) ?
        # Lambda-Ausdruck: zu einer Funktion mit leerem Environment machen
        { # leeres Environment bauen:
         {var reg5 environment* env5;
          make_STACK_env(NIL,NIL,NIL,NIL,O(top_decl_env), env5 = );
          fun = get_closure(Cdr(fun), # Lambdabody (lambda-list {decl|doc} . body)
                            S(Klambda), # :LAMBDA als Name
                            env5); # im leeren Environment
          skipSTACK(5); # Environment wieder vergessen
          # und neu erzeugte Closure anwenden:
          return_Values funcall_closure(fun,args_on_stack);
        }}
      else
        { pushSTACK(fun);
          fehler(
                 DEUTSCH ? "FUNCALL: ~ ist keine Funktionsbezeichnung." :
                 ENGLISH ? "FUNCALL: ~ is not a function name" :
                 FRANCAIS? " FUNCALL: ~ n'est pas un nom de fonction." :
                 ""
                );
        }
    }

# In FUNCALL: Wendet ein SUBR auf Argumente an, räumt den STACK auf
# und liefert die Werte.
# funcall_subr(fun,args_on_stack);
# > fun: Funktion, ein SUBR
# > Argumente: args_on_stack Argumente auf dem STACK
# < STACK: aufgeräumt (d.h. STACK wird um args_on_stack erhöht)
# < mv_count/mv_space: Werte
# verändert STACK, kann GC auslösen
  local Values funcall_subr(fun,args_on_stack)
    var reg4 object fun;
    var reg3 uintC args_on_stack;
    {
      #if STACKCHECKS
      var reg9 object* args_pointer = args_end_pointer STACKop (uintL)args_on_stack; # Pointer über die Argumente
      #endif
      var reg9 object* key_args_pointer; # Pointer über die Keyword-Argumente
      var reg9 object* rest_args_pointer; # Pointer über die restlichen Argumente
      var reg8 uintL argcount; # Anzahl der restlichen Argumente
      # Argumente in den STACK legen:
      # erst ein Dispatch für die wichtigsten Fälle:
      switch (TheSubr(fun)->argtype)
        { case (uintW)subr_argtype_0_0:
            # SUBR ohne Argumente
            if (!(args_on_stack==0)) goto fehler_zuviel;
            goto apply_subr_norest;
          case (uintW)subr_argtype_1_0:
            # SUBR mit 1 required-Argument
            if (!(args_on_stack==1)) goto fehler_anzahl;
            goto apply_subr_norest;
          case (uintW)subr_argtype_2_0:
            # SUBR mit 2 required-Argumenten
            if (!(args_on_stack==2)) goto fehler_anzahl;
            goto apply_subr_norest;
          case (uintW)subr_argtype_3_0:
            # SUBR mit 3 required-Argumenten
            if (!(args_on_stack==3)) goto fehler_anzahl;
            goto apply_subr_norest;
          case (uintW)subr_argtype_4_0:
            # SUBR mit 4 required-Argumenten
            if (!(args_on_stack==4)) goto fehler_anzahl;
            goto apply_subr_norest;
          case (uintW)subr_argtype_0_1:
            # SUBR mit 1 optional-Argument
            if (args_on_stack==1) goto apply_subr_norest;
            elif (args_on_stack>1) goto fehler_zuviel;
            else { pushSTACK(unbound); goto apply_subr_norest; }
          case (uintW)subr_argtype_1_1:
            # SUBR mit 1 required-Argument und 1 optional-Argument
            if (args_on_stack==2) goto apply_subr_norest;
            elif (args_on_stack>2) goto fehler_zuviel;
            elif (args_on_stack==0) goto fehler_zuwenig;
            else { pushSTACK(unbound); goto apply_subr_norest; }
          case (uintW)subr_argtype_2_1:
            # SUBR mit 2 required-Argumenten und 1 optional-Argument
            if (args_on_stack==3) goto apply_subr_norest;
            elif (args_on_stack>3) goto fehler_zuviel;
            elif (args_on_stack<2) goto fehler_zuwenig;
            else { pushSTACK(unbound); goto apply_subr_norest; }
          case (uintW)subr_argtype_3_1:
            # SUBR mit 3 required-Argumenten und 1 optional-Argument
            if (args_on_stack==4) goto apply_subr_norest;
            elif (args_on_stack>4) goto fehler_zuviel;
            elif (args_on_stack<3) goto fehler_zuwenig;
            else { pushSTACK(unbound); goto apply_subr_norest; }
          case (uintW)subr_argtype_4_1:
            # SUBR mit 4 required-Argumenten und 1 optional-Argument
            if (args_on_stack==5) goto apply_subr_norest;
            elif (args_on_stack>5) goto fehler_zuviel;
            elif (args_on_stack<4) goto fehler_zuwenig;
            else { pushSTACK(unbound); goto apply_subr_norest; }
          case (uintW)subr_argtype_0_2:
            # SUBR mit 2 optional-Argumenten
            switch (args_on_stack)
              { case 0: pushSTACK(unbound);
                case 1: pushSTACK(unbound);
                case 2: goto apply_subr_norest;
                default: goto fehler_zuviel;
              }
          case (uintW)subr_argtype_1_2:
            # SUBR mit 1 required-Argument und 2 optional-Argumenten
            switch (args_on_stack)
              { case 0: goto fehler_zuwenig;
                case 1: pushSTACK(unbound);
                case 2: pushSTACK(unbound);
                case 3: goto apply_subr_norest;
                default: goto fehler_zuviel;
              }
          case (uintW)subr_argtype_2_2:
            # SUBR mit 2 required-Argumenten und 2 optional-Argumenten
            switch (args_on_stack)
              { case 0: goto fehler_zuwenig;
                case 1: goto fehler_zuwenig;
                case 2: pushSTACK(unbound);
                case 3: pushSTACK(unbound);
                case 4: goto apply_subr_norest;
                default: goto fehler_zuviel;
              }
          case (uintW)subr_argtype_0_3:
            # SUBR mit 3 optional-Argumenten
            switch (args_on_stack)
              { case 0: pushSTACK(unbound);
                case 1: pushSTACK(unbound);
                case 2: pushSTACK(unbound);
                case 3: goto apply_subr_norest;
                default: goto fehler_zuviel;
              }
          case (uintW)subr_argtype_0_4:
            # SUBR mit 4 optional-Argumenten
            switch (args_on_stack)
              { case 0: pushSTACK(unbound);
                case 1: pushSTACK(unbound);
                case 2: pushSTACK(unbound);
                case 3: pushSTACK(unbound);
                case 4: goto apply_subr_norest;
                default: goto fehler_zuviel;
              }
          case (uintW)subr_argtype_0_5:
            # SUBR mit 5 optional-Argumenten
            switch (args_on_stack)
              { case 0: pushSTACK(unbound);
                case 1: pushSTACK(unbound);
                case 2: pushSTACK(unbound);
                case 3: pushSTACK(unbound);
                case 4: pushSTACK(unbound);
                case 5: goto apply_subr_norest;
                default: goto fehler_zuviel;
              }
          case (uintW)subr_argtype_0_0_rest:
            # SUBR mit weiteren Argumenten
            goto apply_subr_rest_ok;
          case (uintW)subr_argtype_1_0_rest:
            # SUBR mit 1 required-Argument und weiteren Argumenten
            if (args_on_stack==0) goto fehler_zuwenig;
            args_on_stack -= 1;
            goto apply_subr_rest_ok;
          case (uintW)subr_argtype_2_0_rest:
            # SUBR mit 2 required-Argumenten und weiteren Argumenten
            if (args_on_stack<2) goto fehler_zuwenig;
            args_on_stack -= 2;
            goto apply_subr_rest_ok;
          case (uintW)subr_argtype_3_0_rest:
            # SUBR mit 3 required-Argumenten und weiteren Argumenten
            if (args_on_stack<3) goto fehler_zuwenig;
            args_on_stack -= 3;
            goto apply_subr_rest_ok;
          case (uintW)subr_argtype_0_0_key:
            # SUBR mit Keyword-Argumenten
            if (args_on_stack==0) goto unbound_optional_key_0;
            else goto apply_subr_key;
          case (uintW)subr_argtype_1_0_key:
            # SUBR mit 1 required-Argument und Keyword-Argumenten
            if (args_on_stack==1) goto unbound_optional_key_0;
            elif (args_on_stack<1) goto fehler_zuwenig;
            else { args_on_stack -= 1; goto apply_subr_key; }
          case (uintW)subr_argtype_2_0_key:
            # SUBR mit 2 required-Argumenten und Keyword-Argumenten
            if (args_on_stack==2) goto unbound_optional_key_0;
            elif (args_on_stack<2) goto fehler_zuwenig;
            else { args_on_stack -= 2; goto apply_subr_key; }
          case (uintW)subr_argtype_3_0_key:
            # SUBR mit 3 required-Argumenten und Keyword-Argumenten
            if (args_on_stack==3) goto unbound_optional_key_0;
            elif (args_on_stack<3) goto fehler_zuwenig;
            else { args_on_stack -= 3; goto apply_subr_key; }
          case (uintW)subr_argtype_4_0_key:
            # SUBR mit 4 required-Argumenten und Keyword-Argumenten
            if (args_on_stack==4) goto unbound_optional_key_0;
            elif (args_on_stack<4) goto fehler_zuwenig;
            else { args_on_stack -= 4; goto apply_subr_key; }
          case (uintW)subr_argtype_0_1_key:
            # SUBR mit 1 optional-Argument und Keyword-Argumenten
            switch (args_on_stack)
              { case 0: goto unbound_optional_key_1;
                case 1: goto unbound_optional_key_0;
                default: args_on_stack -= 1; goto apply_subr_key;
              }
          case (uintW)subr_argtype_1_1_key:
            # SUBR mit 1 required-Argument, 1 optional-Argument und Keyword-Argumenten
            switch (args_on_stack)
              { case 0: goto fehler_zuwenig;
                case 1: goto unbound_optional_key_1;
                case 2: goto unbound_optional_key_0;
                default: args_on_stack -= 2; goto apply_subr_key;
              }
          case (uintW)subr_argtype_1_2_key:
            # SUBR mit 1 required-Argument, 2 optional-Argumenten und Keyword-Argumenten
            switch (args_on_stack)
              { case 0: goto fehler_zuwenig;
                case 1: goto unbound_optional_key_2;
                case 2: goto unbound_optional_key_1;
                case 3: goto unbound_optional_key_0;
                default: args_on_stack -= 3; goto apply_subr_key;
              }
          unbound_optional_key_2: # Noch 2 optionale Argumente, aber args_on_stack=0
            pushSTACK(unbound);
          unbound_optional_key_1: # Noch 1 optionales Argument, aber args_on_stack=0
            pushSTACK(unbound);
          unbound_optional_key_0: # Vor den Keywords ist args_on_stack=0
            { var reg1 uintC count;
              dotimesC(count,TheSubr(fun)->key_anz, { pushSTACK(unbound); } );
            }
            goto apply_subr_norest;
          default: NOTREACHED
          #undef OPT_ARG
          #undef REQ_ARG
        }
      # Nun die allgemeine Version:
     {var reg5 uintC req_anz = TheSubr(fun)->req_anz;
      var reg6 uintC opt_anz = TheSubr(fun)->opt_anz;
      var reg7 uintC key_anz = TheSubr(fun)->key_anz;
      if (args_on_stack < req_anz)
        # weniger Argumente da als verlangt
        goto fehler_zuwenig;
      args_on_stack -= req_anz; # verbleibende Anzahl
      if (args_on_stack <= opt_anz)
        # Argumente im Stack reichen nicht für die optionalen
        { opt_anz = opt_anz - args_on_stack; # soviele müssen noch auf den STACK
          # Platz auf dem STACK reservieren:
          get_space_on_STACK(sizeof(object) * (uintL)(opt_anz + key_anz));
          # Alle weiteren count optionalen Parameter bekommen den "Wert"
          # #<UNBOUND>, auch die Keyword-Parameter:
          { var reg1 uintC count;
            dotimesC(count,opt_anz + key_anz, { pushSTACK(unbound); } );
            if (TheSubr(fun)->rest_flag == subr_rest) # &REST-Flag?
              # ja -> 0 zusätzliche Argumente:
              { argcount = 0; rest_args_pointer = args_end_pointer;
                goto apply_subr_rest;
              }
              else
              # nein -> nichts zu tun
              { goto apply_subr_norest; }
        } }
      args_on_stack -= opt_anz; # verbleibende Anzahl (> 0)
      if (TheSubr(fun)->key_flag == subr_nokey)
        # SUBR ohne KEY
        { if (TheSubr(fun)->rest_flag == subr_norest)
            # SUBR ohne REST oder KEY
            { goto fehler_zuviel; } # noch Argumente!
            else
            # SUBR mit nur REST, ohne KEY
            apply_subr_rest_ok:
            { argcount = args_on_stack;
              rest_args_pointer = args_end_pointer STACKop argcount;
              goto apply_subr_rest;
            }
        }
        else
        # SUBR mit Keywords.
        { if (FALSE)
            apply_subr_key: { key_anz = TheSubr(fun)->key_anz; }
          # restliche Argumente im STACK nach unten schieben und dadurch
          # Platz für die Keyword-Parameter schaffen:
          argcount = args_on_stack; # (> 0)
          get_space_on_STACK(sizeof(object) * (uintL)key_anz);
          {var reg9 object* new_args_end_pointer = args_end_pointer STACKop -(uintL)key_anz;
           var reg1 object* ptr1 = args_end_pointer;
           var reg1 object* ptr2 = new_args_end_pointer;
           var reg1 uintC count;
           dotimespC(count,args_on_stack, { BEFORE(ptr2) = BEFORE(ptr1); } );
           key_args_pointer = ptr1;
           rest_args_pointer = ptr2;
           dotimesC(count,key_anz, { NEXT(ptr1) = unbound; } );
           set_args_end_pointer(new_args_end_pointer);
          }
          # Keywords zuordnen und evtl. restliche Argumente wegwerfen:
          match_subr_key(fun,argcount,key_args_pointer,rest_args_pointer);
          if (TheSubr(fun)->rest_flag == subr_norest)
            # SUBR ohne &REST-Flag:
            apply_subr_norest:
            { subr_self = fun;
              (*(subr_norest_function*)(TheSubr(fun)->function))();
            }
            else
            # SUBR mit &REST-Flag:
            apply_subr_rest:
            { subr_self = fun;
              (*(subr_rest_function*)(TheSubr(fun)->function))
               (argcount,rest_args_pointer);
            }
     }  }
      #if STACKCHECKS
      if (!(args_pointer == args_end_pointer)) # Stack aufgeräumt?
        { abort(); } # nein -> ab in den Debugger
      #endif
      return; # fertig
      # Gesammelte Fehlermeldungen:
      fehler_anzahl:
        if (args_on_stack < TheSubr(fun)->req_anz)
          { goto fehler_zuwenig; } # zu wenig Argumente
          else
          { goto fehler_zuviel; } # zu viele Argumente
      fehler_zuwenig: fehler_subr_zuwenig(fun);
      fehler_zuviel: fehler_subr_zuviel(fun);
    }

# In FUNCALL: Wendet eine Closure auf Argumente an, räumt den STACK auf
# und liefert die Werte.
# funcall_closure(fun,args_on_stack);
# > fun: Funktion, eine Closure
# > Argumente: args_on_stack Argumente auf dem STACK
# < STACK: aufgeräumt (d.h. STACK wird um args_on_stack erhöht)
# < mv_count/mv_space: Werte
# verändert STACK, kann GC auslösen
  local Values funcall_closure(closure,args_on_stack)
    var reg5 object closure;
    var reg3 uintC args_on_stack;
    { if (m_simple_bit_vector_p(TheClosure(closure)->clos_codevec))
        # closure ist eine compilierte Closure
        {
          #if STACKCHECKC
          var reg9 object* args_pointer = args_end_pointer STACKop (uintL)args_on_stack; # Pointer über die Argumente
          #endif
          var reg4 object codevec = TheCclosure(closure)->clos_codevec; # Code-Vektor
          var reg9 object* key_args_pointer; # Pointer über die Keyword-Argumente
          var reg9 object* rest_args_pointer; # Pointer über die restlichen Argumente
          var reg8 uintL argcount; # Anzahl der restlichen Argumente
          check_SP(); check_STACK();
          # Argumente in den STACK legen:
          # erst ein Dispatch für die wichtigsten Fälle:
          switch (TheSbvector(codevec)->data[CCHD+5])
            { case (uintB)cclos_argtype_0_0:
                # keine Argumente
                if (!(args_on_stack==0)) goto fehler_zuviel;
                goto apply_cclosure_nokey;
              case (uintB)cclos_argtype_1_0:
                # 1 required-Argument
                if (!(args_on_stack==1)) goto fehler_anzahl;
                goto apply_cclosure_nokey;
              case (uintB)cclos_argtype_2_0:
                # 2 required-Argumente
                if (!(args_on_stack==2)) goto fehler_anzahl;
                goto apply_cclosure_nokey;
              case (uintB)cclos_argtype_3_0:
                # 3 required-Argumente
                if (!(args_on_stack==3)) goto fehler_anzahl;
                goto apply_cclosure_nokey;
              case (uintB)cclos_argtype_4_0:
                # 4 required-Argumente
                if (!(args_on_stack==4)) goto fehler_anzahl;
                goto apply_cclosure_nokey;
              case (uintB)cclos_argtype_5_0:
                # 5 required-Argumente
                if (!(args_on_stack==5)) goto fehler_anzahl;
                goto apply_cclosure_nokey;
              case (uintB)cclos_argtype_0_1:
                # 1 optional-Argument
                if (args_on_stack==1) goto apply_cclosure_nokey;
                elif (args_on_stack>1) goto fehler_zuviel;
                else { pushSTACK(unbound); goto apply_cclosure_nokey; }
              case (uintB)cclos_argtype_1_1:
                # 1 required-Argument und 1 optional-Argument
                if (args_on_stack==2) goto apply_cclosure_nokey;
                elif (args_on_stack>2) goto fehler_zuviel;
                elif (args_on_stack==0) goto fehler_zuwenig;
                else { pushSTACK(unbound); goto apply_cclosure_nokey; }
              case (uintB)cclos_argtype_2_1:
                # 2 required-Argumente und 1 optional-Argument
                if (args_on_stack==3) goto apply_cclosure_nokey;
                elif (args_on_stack>3) goto fehler_zuviel;
                elif (args_on_stack<2) goto fehler_zuwenig;
                else { pushSTACK(unbound); goto apply_cclosure_nokey; }
              case (uintB)cclos_argtype_3_1:
                # 3 required-Argumente und 1 optional-Argument
                if (args_on_stack==4) goto apply_cclosure_nokey;
                elif (args_on_stack>4) goto fehler_zuviel;
                elif (args_on_stack<3) goto fehler_zuwenig;
                else { pushSTACK(unbound); goto apply_cclosure_nokey; }
              case (uintB)cclos_argtype_4_1:
                # 4 required-Argumente und 1 optional-Argument
                if (args_on_stack==5) goto apply_cclosure_nokey;
                elif (args_on_stack>5) goto fehler_zuviel;
                elif (args_on_stack<4) goto fehler_zuwenig;
                else { pushSTACK(unbound); goto apply_cclosure_nokey; }
              case (uintB)cclos_argtype_0_2:
                # 2 optional-Argumente
                switch (args_on_stack)
                  { case 0: pushSTACK(unbound);
                    case 1: pushSTACK(unbound);
                    case 2: goto apply_cclosure_nokey;
                    default: goto fehler_zuviel;
                  }
              case (uintB)cclos_argtype_1_2:
                # 1 required-Argument und 2 optional-Argumente
                switch (args_on_stack)
                  { case 0: goto fehler_zuwenig;
                    case 1: pushSTACK(unbound);
                    case 2: pushSTACK(unbound);
                    case 3: goto apply_cclosure_nokey;
                    default: goto fehler_zuviel;
                  }
              case (uintB)cclos_argtype_2_2:
                # 2 required-Argumente und 2 optional-Argumente
                switch (args_on_stack)
                  { case 0: case 1: goto fehler_zuwenig;
                    case 2: pushSTACK(unbound);
                    case 3: pushSTACK(unbound);
                    case 4: goto apply_cclosure_nokey;
                    default: goto fehler_zuviel;
                  }
              case (uintB)cclos_argtype_3_2:
                # 3 required-Argumente und 2 optional-Argumente
                switch (args_on_stack)
                  { case 0: case 1: case 2: goto fehler_zuwenig;
                    case 3: pushSTACK(unbound);
                    case 4: pushSTACK(unbound);
                    case 5: goto apply_cclosure_nokey;
                    default: goto fehler_zuviel;
                  }
              case (uintB)cclos_argtype_0_3:
                # 3 optional-Argumente
                switch (args_on_stack)
                  { case 0: pushSTACK(unbound);
                    case 1: pushSTACK(unbound);
                    case 2: pushSTACK(unbound);
                    case 3: goto apply_cclosure_nokey;
                    default: goto fehler_zuviel;
                  }
              case (uintB)cclos_argtype_1_3:
                # 1 required-Argument und 3 optional-Argumente
                switch (args_on_stack)
                  { case 0: goto fehler_zuwenig;
                    case 1: pushSTACK(unbound);
                    case 2: pushSTACK(unbound);
                    case 3: pushSTACK(unbound);
                    case 4: goto apply_cclosure_nokey;
                    default: goto fehler_zuviel;
                  }
              case (uintB)cclos_argtype_2_3:
                # 2 required-Argumente und 3 optional-Argumente
                switch (args_on_stack)
                  { case 0: case 1: goto fehler_zuwenig;
                    case 2: pushSTACK(unbound);
                    case 3: pushSTACK(unbound);
                    case 4: pushSTACK(unbound);
                    case 5: goto apply_cclosure_nokey;
                    default: goto fehler_zuviel;
                  }
              case (uintB)cclos_argtype_0_4:
                # 4 optional-Argumente
                switch (args_on_stack)
                  { case 0: pushSTACK(unbound);
                    case 1: pushSTACK(unbound);
                    case 2: pushSTACK(unbound);
                    case 3: pushSTACK(unbound);
                    case 4: goto apply_cclosure_nokey;
                    default: goto fehler_zuviel;
                  }
              case (uintB)cclos_argtype_1_4:
                # 1 required-Argument und 4 optional-Argumente
                switch (args_on_stack)
                  { case 0: goto fehler_zuwenig;
                    case 1: pushSTACK(unbound);
                    case 2: pushSTACK(unbound);
                    case 3: pushSTACK(unbound);
                    case 4: pushSTACK(unbound);
                    case 5: goto apply_cclosure_nokey;
                    default: goto fehler_zuviel;
                  }
              case (uintB)cclos_argtype_0_5:
                # 5 optional-Argumente
                switch (args_on_stack)
                  { case 0: pushSTACK(unbound);
                    case 1: pushSTACK(unbound);
                    case 2: pushSTACK(unbound);
                    case 3: pushSTACK(unbound);
                    case 4: pushSTACK(unbound);
                    case 5: goto apply_cclosure_nokey;
                    default: goto fehler_zuviel;
                  }
              case (uintB)cclos_argtype_0_0_rest:
                # keine Argumente, Rest-Parameter
                goto apply_cclosure_rest_nokey;
              case (uintB)cclos_argtype_1_0_rest:
                # 1 required-Argument, Rest-Parameter
                if (args_on_stack==0) goto fehler_zuwenig;
                args_on_stack -= 1;
                goto apply_cclosure_rest_nokey;
              case (uintB)cclos_argtype_2_0_rest:
                # 2 required-Argumente, Rest-Parameter
                if (args_on_stack<2) goto fehler_zuwenig;
                args_on_stack -= 2;
                goto apply_cclosure_rest_nokey;
              case (uintB)cclos_argtype_3_0_rest:
                # 3 required-Argumente, Rest-Parameter
                if (args_on_stack<3) goto fehler_zuwenig;
                args_on_stack -= 3;
                goto apply_cclosure_rest_nokey;
              case (uintB)cclos_argtype_4_0_rest:
                # 4 required-Argumente, Rest-Parameter
                if (args_on_stack<4) goto fehler_zuwenig;
                args_on_stack -= 4;
                goto apply_cclosure_rest_nokey;
              case (uintB)cclos_argtype_0_0_key:
                # nur Keyword-Argumente
                if (args_on_stack==0) goto unbound_optional_key_0;
                else goto apply_cclosure_key_withargs;
              case (uintB)cclos_argtype_1_0_key:
                # 1 required-Argument, Keyword-Argumente
                if (args_on_stack==1) goto unbound_optional_key_0;
                elif (args_on_stack<1) goto fehler_zuwenig;
                else { args_on_stack -= 1; goto apply_cclosure_key_withargs; }
              case (uintB)cclos_argtype_2_0_key:
                # 2 required-Argumente, Keyword-Argumente
                if (args_on_stack==2) goto unbound_optional_key_0;
                elif (args_on_stack<2) goto fehler_zuwenig;
                else { args_on_stack -= 2; goto apply_cclosure_key_withargs; }
              case (uintB)cclos_argtype_3_0_key:
                # 3 required-Argumente, Keyword-Argumente
                if (args_on_stack==3) goto unbound_optional_key_0;
                elif (args_on_stack<3) goto fehler_zuwenig;
                else { args_on_stack -= 3; goto apply_cclosure_key_withargs; }
              case (uintB)cclos_argtype_4_0_key:
                # 4 required-Argumente, Keyword-Argumente
                if (args_on_stack==4) goto unbound_optional_key_0;
                elif (args_on_stack<4) goto fehler_zuwenig;
                else { args_on_stack -= 4; goto apply_cclosure_key_withargs; }
              case (uintB)cclos_argtype_0_1_key:
                # 1 optional-Argument, Keyword-Argumente
                switch (args_on_stack)
                  { case 0: goto unbound_optional_key_1;
                    case 1: goto unbound_optional_key_0;
                    default: args_on_stack -= 1; goto apply_cclosure_key_withargs;
                  }
              case (uintB)cclos_argtype_1_1_key:
                # 1 required-Argument und 1 optional-Argument, Keyword-Argumente
                switch (args_on_stack)
                  { case 0: goto fehler_zuwenig;
                    case 1: goto unbound_optional_key_1;
                    case 2: goto unbound_optional_key_0;
                    default: args_on_stack -= 2; goto apply_cclosure_key_withargs;
                  }
              case (uintB)cclos_argtype_2_1_key:
                # 2 required-Argumente und 1 optional-Argument, Keyword-Argumente
                switch (args_on_stack)
                  { case 0: case 1: goto fehler_zuwenig;
                    case 2: goto unbound_optional_key_1;
                    case 3: goto unbound_optional_key_0;
                    default: args_on_stack -= 3; goto apply_cclosure_key_withargs;
                  }
              case (uintB)cclos_argtype_3_1_key:
                # 3 required-Argumente und 1 optional-Argument, Keyword-Argumente
                switch (args_on_stack)
                  { case 0: case 1: case 2: goto fehler_zuwenig;
                    case 3: goto unbound_optional_key_1;
                    case 4: goto unbound_optional_key_0;
                    default: args_on_stack -= 4; goto apply_cclosure_key_withargs;
                  }
              case (uintB)cclos_argtype_0_2_key:
                # 2 optional-Argumente, Keyword-Argumente
                switch (args_on_stack)
                  { case 0: goto unbound_optional_key_2;
                    case 1: goto unbound_optional_key_1;
                    case 2: goto unbound_optional_key_0;
                    default: args_on_stack -= 2; goto apply_cclosure_key_withargs;
                  }
              case (uintB)cclos_argtype_1_2_key:
                # 1 required-Argument und 2 optional-Argumente, Keyword-Argumente
                switch (args_on_stack)
                  { case 0: goto fehler_zuwenig;
                    case 1: goto unbound_optional_key_2;
                    case 2: goto unbound_optional_key_1;
                    case 3: goto unbound_optional_key_0;
                    default: args_on_stack -= 3; goto apply_cclosure_key_withargs;
                  }
              case (uintB)cclos_argtype_2_2_key:
                # 2 required-Argumente und 2 optional-Argumente, Keyword-Argumente
                switch (args_on_stack)
                  { case 0: case 1: goto fehler_zuwenig;
                    case 2: goto unbound_optional_key_2;
                    case 3: goto unbound_optional_key_1;
                    case 4: goto unbound_optional_key_0;
                    default: args_on_stack -= 4; goto apply_cclosure_key_withargs;
                  }
              case (uintB)cclos_argtype_0_3_key:
                # 3 optional-Argumente, Keyword-Argumente
                switch (args_on_stack)
                  { case 0: goto unbound_optional_key_3;
                    case 1: goto unbound_optional_key_2;
                    case 2: goto unbound_optional_key_1;
                    case 3: goto unbound_optional_key_0;
                    default: args_on_stack -= 3; goto apply_cclosure_key_withargs;
                  }
              case (uintB)cclos_argtype_1_3_key:
                # 1 required-Argument und 3 optional-Argumente, Keyword-Argumente
                switch (args_on_stack)
                  { case 0: goto fehler_zuwenig;
                    case 1: goto unbound_optional_key_3;
                    case 2: goto unbound_optional_key_2;
                    case 3: goto unbound_optional_key_1;
                    case 4: goto unbound_optional_key_0;
                    default: args_on_stack -= 4; goto apply_cclosure_key_withargs;
                  }
              case (uintB)cclos_argtype_0_4_key:
                # 4 optional-Argumente, Keyword-Argumente
                switch (args_on_stack)
                  { case 0: goto unbound_optional_key_4;
                    case 1: goto unbound_optional_key_3;
                    case 2: goto unbound_optional_key_2;
                    case 3: goto unbound_optional_key_1;
                    case 4: goto unbound_optional_key_0;
                    default: args_on_stack -= 4; goto apply_cclosure_key_withargs;
                  }
              unbound_optional_key_4: # Noch 4 optionale Argumente, aber args_on_stack=0
                pushSTACK(unbound);
              unbound_optional_key_3: # Noch 3 optionale Argumente, aber args_on_stack=0
                pushSTACK(unbound);
              unbound_optional_key_2: # Noch 2 optionale Argumente, aber args_on_stack=0
                pushSTACK(unbound);
              unbound_optional_key_1: # Noch 1 optionales Argument, aber args_on_stack=0
                pushSTACK(unbound);
              unbound_optional_key_0: # Vor den Keywords ist args_on_stack=0
                goto apply_cclosure_key_noargs;
              case (uintB)cclos_argtype_default:
                # Allgemeine Version
                break;
              default: NOTREACHED
            }
          # Nun die allgemeine Version:
         {var reg5 uintC req_anz = *(uintW*)(&TheSbvector(codevec)->data[CCHD+0]); # Anzahl required Parameter
          var reg6 uintC opt_anz = *(uintW*)(&TheSbvector(codevec)->data[CCHD+2]); # Anzahl optionale Parameter
          var reg7 uintB flags = TheSbvector(codevec)->data[CCHD+4]; # Flags
          if (args_on_stack < req_anz)
            # weniger Argumente da als verlangt
            { goto fehler_zuwenig; }
          args_on_stack -= req_anz; # verbleibende Anzahl
          if (args_on_stack <= opt_anz)
            # Argumente im Stack reichen nicht für die optionalen
            { opt_anz = opt_anz - args_on_stack; # soviele müssen noch auf den STACK
              # Platz auf dem STACK reservieren:
              get_space_on_STACK(sizeof(object) * (uintL)opt_anz);
              # Alle weiteren count optionalen Parameter bekommen den "Wert"
              # #<UNBOUND>, der &REST-Parameter den Wert NIL,
              # die Keyword-Parameter den Wert #<UNBOUND> :
              { var reg1 uintC count;
                dotimesC(count,opt_anz, { pushSTACK(unbound); } );
              }
              if (flags & bit(0)) # &REST-Flag?
                { pushSTACK(NIL); } # ja -> mit NIL initialisieren
              if (flags & bit(7)) # &KEY-Flag?
                apply_cclosure_key_noargs:
                { var reg1 uintC key_anz = *(uintW*)(&TheSbvector(codevec)->data[CCHD+6]); # Anzahl Keyword-Parameter
                  get_space_on_STACK(sizeof(object) * (uintL)key_anz);
                  {var reg1 uintC count;
                   dotimesC(count,key_anz, { pushSTACK(unbound); } ); # mit #<UNBOUND> initialisieren
                  }
                  goto apply_cclosure_key;
                }
                else
                goto apply_cclosure_nokey;
            }
          args_on_stack -= opt_anz; # verbleibende Anzahl
          if (flags & bit(7)) # Key-Flag?
            { if (FALSE)
                apply_cclosure_key_withargs:
                { flags = TheSbvector(codevec)->data[CCHD+4]; } # Flags initialisieren!
            # Closure mit Keywords
             {var reg1 uintC key_anz = *(uintW*)(&TheSbvector(codevec)->data[CCHD+6]); # Anzahl Keyword-Parameter
              # restliche Argumente im STACK nach unten schieben und dadurch
              # Platz für die Keyword-Parameter (und evtl. Rest-Parameter)
              # schaffen:
              var reg1 uintL shift = key_anz;
              if (flags & bit(0)) { shift++; } # evtl. 1 mehr für Rest-Parameter
              argcount = args_on_stack;
              get_space_on_STACK(sizeof(object) * shift);
              {var reg9 object* new_args_end_pointer = args_end_pointer STACKop -shift;
               var reg1 object* ptr1 = args_end_pointer;
               var reg1 object* ptr2 = new_args_end_pointer;
               var reg1 uintC count;
               dotimesC(count,args_on_stack, { BEFORE(ptr2) = BEFORE(ptr1); } );
               if (flags & bit(0)) { NEXT(ptr1) = unbound; } # Rest-Parameter
               key_args_pointer = ptr1;
               rest_args_pointer = ptr2;
               dotimesC(count,key_anz, { NEXT(ptr1) = unbound; } );
               set_args_end_pointer(new_args_end_pointer);
              }
              # Keywords zuordnen, Rest-Parameter bauen
              # und evtl. restliche Argumente wegwerfen:
              closure = match_cclosure_key(closure,argcount,key_args_pointer,rest_args_pointer);
              codevec = TheCclosure(closure)->clos_codevec;
              apply_cclosure_key:
              interpret_bytecode(closure,codevec,CCHD+10); # Bytecode ab Byte 10 abinterpretieren
            }}
          elif (flags & bit(0))
            apply_cclosure_rest_nokey:
            # Closure mit nur REST, ohne KEY:
            { # muß noch args_on_stack Argumente aus dem Stack zusammenconsen:
              pushSTACK(NIL);
              pushSTACK(closure); # Closure muß gerettet werden
              dotimesC(args_on_stack,args_on_stack,
                { var reg1 object new_cons = allocate_cons();
                  Cdr(new_cons) = STACK_1;
                  Car(new_cons) = STACK_2; # nächstes Argument draufconsen
                  STACK_2 = new_cons;
                  STACK_1 = STACK_0; skipSTACK(1);
                });
              closure = popSTACK(); codevec = TheCclosure(closure)->clos_codevec;
              goto apply_cclosure_nokey;
            }
          else
            # Closure ohne REST oder KEY
            { if (args_on_stack>0) # noch Argumente?
                goto fehler_zuviel;
              apply_cclosure_nokey: # Closure ohne &KEY anspringen:
              interpret_bytecode(closure,codevec,CCHD+6); # Bytecode ab Byte 6 abinterpretieren
            }
         }
          #if STACKCHECKC
          if (!(args_pointer == args_end_pointer)) # Stack aufgeräumt?
            { abort(); } # nein -> ab in den Debugger
          #endif
          return; # fertig
          # Gesammelte Fehlermeldungen:
          fehler_anzahl:
            if (args_on_stack < *(uintW*)(&TheSbvector(codevec)->data[CCHD+0]))
              { goto fehler_zuwenig; } # zu wenig Argumente
              else
              { goto fehler_zuviel; } # zu viele Argumente
          fehler_zuwenig: fehler_closure_zuwenig(closure);
          fehler_zuviel: fehler_closure_zuviel(closure);
        }
        else
        # closure ist eine interpretierte Closure
        { funcall_iclosure(closure,args_end_pointer STACKop (uintL)args_on_stack,args_on_stack); }
    }


#      ---------------------- BYTECODE-INTERPRETER ----------------------

# Interpretiert den Bytecode einer compilierten Closure.
# interpret_bytecode_(closure,codeptr,byteptr);
# > closure: compilierte Closure
# > codeptr: ihr Codevektor, ein Simple-Bit-Vector, pointable
# > byteptr: Start-Bytecodepointer
# < mv_count/mv_space: Werte
# verändert STACK, kann GC auslösen
  # Den GNU-C dazu überreden, closure und byteptr in Registern zu halten:
  #ifdef GNU
    #ifdef MC680X0
      #define closure_register  "a2"
      #define byteptr_register  "a3"
    #endif
    #ifdef SPARC
      #define closure_register  "%l0"
      #define byteptr_register  "%l1"
    #endif
    #ifdef I80Z86
      #define byteptr_register  "%edi"
    #endif
  #endif
  #ifndef closure_register
    #define closure_in  closure
  #endif
  #ifndef byteptr_register
    #define byteptr_in  byteptr
  #endif
  local Values interpret_bytecode_(closure_in,codeptr,byteptr_in)
    var reg3 object closure_in;
    var reg8 Sbvector codeptr;
    var reg1 uintB* byteptr_in;
    { # Argument closure im Register unterbringen:
      #ifdef closure_register
      var reg3 object closure __asm__(closure_register);
      closure = closure_in;
      #endif
     {# Argument byteptr im Register unterbringen:
      #ifdef byteptr_register
      var reg1 uintB* byteptr __asm__(byteptr_register);
      byteptr = byteptr_in;
      #endif
      {# Closure im STACK unterbringen, unter die Argumente:
       var reg5 object* closureptr = (pushSTACK(closure), &STACK_0);
       #ifndef FAST_SP
         # Hat man keinen schnellen SP-Zugriff, muß man einen extra Pointer
         # einführen:
         var reg10 uintL private_SP_length = (uintL)(*(uintW*)(&codeptr->data[0]));
         var DYNAMIC_ARRAY(private_SP_space,uintL,private_SP_length);
         var reg6 uintL* private_SP = &private_SP_space[private_SP_length];
         #undef SP_
         #undef skipSP
         #undef pushSP
         #undef popSP
         #define SP_(n)  (private_SP[n])
         #define skipSP(n)  (private_SP += (n))
         #define pushSP(item)  (*--private_SP = (item))
         #define popSP(item_zuweisung)  (item_zuweisung *private_SP++)
       #endif
       # var JMPBUF_on_SP(name);  alloziert einen jmp_buf im SP.
       # FREE_JMPBUF_on_SP();  dealloziert ihn wieder.
       # finish_entry_frame_1(frametype,returner,reentry_statement);  ist wie
       # finish_entry_frame(frametype,returner,,reentry_statement);  nur daß
       # auch private_SP gerettet wird.
       #ifndef FAST_SP
         #define JMPBUF_on_SP(name)  \
           jmp_buf* name = (jmp_buf*)(private_SP -= jmpbufsize);
         #define FREE_JMPBUF_on_SP()  \
           private_SP += jmpbufsize;
         #define finish_entry_frame_1(frametype,returner,reentry_statement)  \
           finish_entry_frame(frametype,&!*returner, # Beim Eintritt: returner = private_SP      \
             returner = (jmp_buf*) , # returner wird beim Rücksprung wieder gesetzt              \
             { private_SP = (uintL*)returner; reentry_statement } # und private_SP rekonstruiert \
             )
       #else
         #ifdef SP_DOWN
           #define JMPBUF_on_SP(name)  \
             jmp_buf* name;                      \
             {var reg1 uintL* sp = (uintL*)SP(); \
              sp -= jmpbufsize;                  \
              setSP(sp);                         \
              name = (jmp_buf*)&sp[SPoffset];    \
             }
         #endif
         #ifdef SP_UP
           #define JMPBUF_on_SP(name)  \
             jmp_buf* name;                      \
             {var reg1 uintL* sp = (uintL*)SP(); \
              name = (jmp_buf*)&sp[SPoffset+1];  \
              sp += jmpbufsize;                  \
              setSP(sp);                         \
             }
         #endif
         #define FREE_JMPBUF_on_SP()  \
           skipSP(jmpbufsize);
         #define finish_entry_frame_1(frametype,returner,reentry_statement)  \
           finish_entry_frame(frametype,&!*returner,,reentry_statement)
       #endif
       #
       # nächstes Byte abzuinterpretieren
       # > mv_count/mv_space: aktuelle Werte
       # > closureptr: Pointer auf die compilierte Closure im Stack
       # > closure: compilierte Closure
       # > codeptr: ihr Codevektor, ein Simple-Bit-Vektor, pointable
       #            (kein LISP-Objekt, aber dennoch GC-gefährdet!)
       # > byteptr: Pointer auf das nächste Byte im Code
       #            (kein LISP-Objekt, aber dennoch GC-gefährdet!)
       next_byte:
        switch (*byteptr++) # Fallunterscheidung nach abzuinterpretierendem Byte
          { # Holen der Operanden:
            #   nächstes Byte:
            #     Bit 7 = 0 --> Bits 6..0 sind der Operand (7 Bits).
            #     Bit 7 = 1 --> Bits 6..0 und nächstes Byte bilden den
            #                   Operanden (15 Bits).
            #                   Bei Sprungdistanzen: Sollte dieser =0 sein, so
            #                   bilden die nächsten 4 Bytes den Operanden
            #                   (32 Bits).
            #
            # Macro B_operand(where);
            # bringt den nächsten Operanden (ein Byte als Unsigned Integer)
            # nach (uintL)where und rückt dabei den Bytecodepointer weiter.
              #define B_operand(where)  \
                { where = *byteptr++; }
            #
            # Macro U_operand(where);
            # bringt den nächsten Operanden (ein Unsigned Integer)
            # nach (uintL)where oder (uintC)where
            # und rückt dabei den Bytecodepointer weiter.
              #define U_operand(where)  \
                { where = *byteptr++; # erstes Byte lesen            \
                  if ((uintB)where & bit(7)) # Bit 7 gesetzt?        \
                    { where &= ~bit(7); # ja -> löschen              \
                      where = where << 8;                            \
                      where |= *byteptr++; # und nächstes Byte lesen \
                }   }
            #if defined(GNU) && defined(MC680X0)
              #undef U_operand
              #define U_operand(where)  \
                __asm__("\
                  moveq #0,%0   ; \
                  moveb %1@+,%0 ; \
                  bpl 1f        ; \
                  addb %0,%0    ; \
                  lslw #7,%0    ; \
                  moveb %1@+,%0 ; \
                  1:              \
                  " : "=d" (where), "=a" (byteptr) : "1" (byteptr) )
            #endif
            #if defined(GNU) && defined(SPARC)
              #undef U_operand
              #define U_operand(where)  \
                { var reg1 uintL dummy;  \
                  __asm__("\
                    ldub [%1],%0       ; \
                    andcc %0,0x80,%%g0 ; \
                    be 1f              ; \
                     add %1,1,%1       ; \
                    sll %0,25,%2       ; \
                    ldub [%1],%0       ; \
                    srl %2,17,%2       ; \
                    add %1,1,%1        ; \
                    or %0,%2,%0        ; \
                    1:                   \
                    " : "=r" (where), "=r" (byteptr), "=r" (dummy) : "1" (byteptr) : "ccr" ); \
                }
            #endif
            #if defined(GNU) && defined(I80Z86) && !defined(SUN386) && !defined(UHC)
              #undef U_operand
              #define U_operand(where)  \
                __asm__("\
                  movzbl (%1),%0             ; \
                  incl %1                    ; \
                  testb %0,%0                ; \
                  jge LASM%c3"STRING(where)" ; \
                  andb $127,%0               ; \
                  sall $8,%0                 ; \
                  movb (%1),%0               ; \
                  incl %1                    ; \
                  LASM%c3"STRING(where)":      \
                  " : "=q" (where), "=r" (byteptr) : "1" (byteptr), "i" (__LINE__) );
              # Vorsicht: 1. testb %eax,%eax wird als testb %al,%al assembliert!
              #              (Der GNU-Assembler macht das, der Sun-Assembler weigert sich.)
              #           2. ccr wird verändert. Wie deklariert man das??
              #           3. Der Sun-Assembler kennt diese Syntax für lokale Labels nicht.
              #              Daher generieren wir unsere lokalen Labels selbst.
            #endif
            #
            # Macro S_operand(where);
            # bringt den nächsten Operanden (ein Signed Integer)
            # nach (uintL)where und rückt dabei den Bytecodepointer weiter.
              #define S_operand(where)  \
                { where = *byteptr++; # erstes Byte lesen              \
                  if ((uintB)where & bit(7))                           \
                    # Bit 7 war gesetzt                                \
                    { where = where << 8;                              \
                      where |= *byteptr++; # nächstes Byte dazunehmen  \
                      # Sign-Extend von 15 auf 32 Bits:                \
                      where = (sintL)((sintL)((sintWL)where << (intWLsize-15)) >> (intWLsize-15)); \
                      if (where == 0)                                  \
                        # Sonderfall: 2-Byte-Operand = 0 -> 6-Byte-Operand \
                        { where = (uintL)( ((uintWL)(byteptr[0]) << 8) \
                                          | (uintWL)(byteptr[1])       \
                                         ) << 16                       \
                                | (uintL)( ((uintWL)(byteptr[2]) << 8) \
                                          | (uintWL)(byteptr[3])       \
                                         );                            \
                          byteptr += 4;                                \
                    }   }                                              \
                    else                                               \
                    # Bit 7 war gelöscht                               \
                    { # Sign-Extend von 7 auf 32 Bits:                 \
                      where = (sintL)((sintL)((sintBWL)where << (intBWLsize-7)) >> (intBWLsize-7)); \
                    }                                                  \
                }
            #if defined(GNU) && defined(MC680X0)
              #undef S_operand
              #define S_operand(where)  \
                __asm__("\
                  moveb %1@+,%0   ; \
                  bpl 1f          ; \
                  lslw #8,%0      ; \
                  moveb %1@+,%0   ; \
                  addw %0,%0      ; \
                  asrw #1,%0      ; \
                  bne 2f          ; \
                  moveb %1@(2),%0 ; \
                  swap %0         ; \
                  moveb %1@+,%0   ; \
                  lsll #8,%0      ; \
                  moveb %1@,%0    ; \
                  swap %0         ; \
                  addql #2,%0     ; \
                  moveb %1@+,%0   ; \
                  jra 3f          ; \
                  1:                \
                  addb %0,%0      ; \
                  asrb #1,%0      ; \
                  extw %0         ; \
                  2:                \
                  extl %0         ; \
                  3:                \
                  " : "=d" (where), "=a" (byteptr) : "1" (byteptr) )
            #endif
            #if defined(GNU) && defined(SPARC)
              #undef S_operand
              #define S_operand(where)  \
                { var reg1 uintL dummy;  \
                  __asm__("\
                    ldub [%1],%0       ; \
                    andcc %0,0x80,%%g0 ; \
                    be 2f              ; \
                     add %1,1,%1       ; \
                    sll %0,25,%2       ; \
                    ldub [%1],%0       ; \
                    sra %2,17,%2       ; \
                    orcc %2,%0,%0      ; \
                    bne 3f             ; \
                     add %1,1,%1       ; \
                    ldub [%1],%0       ; \
                    sll %0,24,%2       ; \
                    ldub [%1+1],%0     ; \
                    sll %0,16,%0       ; \
                    or %2,%0,%2        ; \
                    ldub [%1+2],%0     ; \
                    sll %0,8,%0        ; \
                    or %2,%0,%2        ; \
                    ldub [%1+3],%0     ; \
                    or %2,%0,%0        ; \
                    b 3f               ; \
                     add %1,4,%1       ; \
                    2:                   \
                    sll %0,25,%0       ; \
                    sra %0,25,%0       ; \
                    3:                   \
                    " : "=r" (where), "=r" (byteptr), "=r" (dummy) : "1" (byteptr) : "ccr" ); \
                }
            #endif
            #if defined(GNU) && defined(I80Z86) && !defined(SUN386) && !defined(UHC)
              #undef S_operand
              #define S_operand(where)  \
                __asm__("\
                  movzbl (%1),%0 ; \
                  incl %1        ; \
                  testb %0,%0    ; \
                  jge LASM%c3X1  ; \
                  sall $8,%0     ; \
                  movb (%1),%0   ; \
                  incl %1        ; \
                  sall $17,%0    ; \
                  sarl $17,%0    ; \
                  jne LASM%c3X2  ; \
                  movb (%1),%0   ; \
                  sall $8,%0     ; \
                  movb 1(%1),%0  ; \
                  sall $8,%0     ; \
                  movb 2(%1),%0  ; \
                  sall $8,%0     ; \
                  movb 3(%1),%0  ; \
                  addl $4,%0     ; \
                  jmp LASM%c3X2  ; \
                  LASM%c3X1:       \
                  sall $25,%0    ; \
                  sarl $25,%0    ; \
                  LASM%c3X2:       \
                  " : "=q" (where), "=r" (byteptr) : "1" (byteptr), "i" (__LINE__) );
            #endif
            #
            # Macro S_operand_ignore();
            # übergeht den nächsten Operanden (ein Signed Integer)
            # und rückt dabei den Bytecodepointer weiter.
              #define S_operand_ignore()  \
                { var reg1 uintB where = *byteptr++; # erstes Byte lesen \
                  if ((uintB)where & bit(7))                             \
                    # Bit 7 war gesetzt                                  \
                    { if ((uintB)((where<<1) | *byteptr++) == 0) # nächstes Byte dazu \
                        # Sonderfall: 2-Byte-Operand = 0 -> 6-Byte-Operand \
                        { byteptr += 4; }                                \
                }   }
            #if defined(GNU) && defined(MC680X0)
              #undef S_operand_ignore
              #define S_operand_ignore()  \
                { var reg1 uintB where; \
                  __asm__("\
                    moveb %1@+,%0   ; \
                    bpl 1f          ; \
                    addb %0,%0      ; \
                    orb %1@+,%0     ; \
                    bne 1f          ; \
                    addql #4,%1     ; \
                    1:                \
                    " : "=d" (where), "=a" (byteptr) : "1" (byteptr) ); \
                }
            #endif
            #if defined(GNU) && defined(SPARC)
              #undef S_operand_ignore
              #define S_operand_ignore()  \
                { var reg1 uintL where;  \
                  var reg2 uintL dummy;  \
                  __asm__("\
                    ldub [%1],%0       ; \
                    andcc %0,0x80,%%g0 ; \
                    be 1f              ; \
                     add %1,1,%1       ; \
                    sll %0,1,%2        ; \
                    ldub [%1],%0       ; \
                    orcc %2,%0,%0      ; \
                    bne 1f             ; \
                     add %1,1,%1       ; \
                    add %1,4,%1        ; \
                    1:                   \
                    " : "=r" (where), "=r" (byteptr), "=r" (dummy) : "1" (byteptr) : "ccr" ); \
                }
            #endif
            #
            # Macro L_operand(where);
            # bringt den nächsten Operanden (ein Label)
            # nach (uintB*)where und rückt dabei den Bytecodepointer weiter.
              #define L_operand(Lwhere)  \
                { var reg1 uintL where; # Variable fürs Displacement \
                  S_operand(where); # Displacement                   \
                  Lwhere = byteptr + (sintL)where; # addieren        \
                }
            #
            # Macro L_operand_ignore();
            # übergeht den nächsten Operanden (ein Label)
            # und rückt dabei den Bytecodepointer weiter.
              #define L_operand_ignore()  S_operand_ignore()
            #
            # Die einzelnen Bytecodes werden interpretiert:
            # Dabei ist meist mv_count/mv_space = Werte,
            # closureptr = Pointer auf die compilierte Closure im Stack,
            # closure = compilierte Closure,
            # codeptr = Pointer auf ihren Codevektor,
            # byteptr = Pointer auf das nächste Byte im Code.
            # (byteptr ist kein LISP-Objekt, aber dennoch GC-gefährdet! Um es
            #  GC-invariant zu machen, muß man CODEPTR
            #  davon subtrahieren. Addiert man dann Fixnum_0 dazu,
            #  so hat man die Bytenummer als Fixnum.)
            #if 0
              #define CODEPTR  (&codeptr->data[0])
            #else # liefert effizienteren Code
              #define CODEPTR  (uintB*)(codeptr)
            #endif
            #
            # Kontextinformation aufbewahren:
            # Wird etwas aufgerufen, das eine GC auslösen kann, so muß dies in ein
            # with_saved_context( ... ) eingebaut werden.
              #define with_saved_context(statement)  \
                { var reg9 uintL index = byteptr - CODEPTR;                  \
                  statement;                                                 \
                  closure = *closureptr; # Closure aus dem Stack holen       \
                  codeptr = TheSbvector(TheCclosure(closure)->clos_codevec); \
                  byteptr = CODEPTR + index;                                 \
                }
            #
            # ------------------- (1) Konstanten -----------------------
            case (uintB)cod_nil:             # (NIL)
              code_nil:
              value1 = NIL; mv_count = 1;
              goto next_byte;
            case (uintB)cod_nil_push:        # (NIL&PUSH)
              pushSTACK(NIL);
              goto next_byte;
            case (uintB)cod_push_nil:        # (PUSH-NIL n)
              { var reg2 uintC n;
                U_operand(n);
                dotimesC(n,n, { pushSTACK(NIL); } );
              }
              goto next_byte;
            case (uintB)cod_t:               # (T)
              code_t:
              value1 = T; mv_count = 1;
              goto next_byte;
            case (uintB)cod_t_push:          # (T&PUSH)
              pushSTACK(T);
              goto next_byte;
            case (uintB)cod_const:           # (CONST n)
              { var reg2 uintL n;
                U_operand(n);
                value1 = TheCclosure(closure)->clos_consts[n]; mv_count=1;
              }
              goto next_byte;
            case (uintB)cod_const_push:      # (CONST&PUSH n)
              { var reg2 uintL n;
                U_operand(n);
                pushSTACK(TheCclosure(closure)->clos_consts[n]);
              }
              goto next_byte;
            # ------------------- (2) statische Variablen -----------------------
            case (uintB)cod_load:            # (LOAD n)
              { var reg2 uintL n;
                U_operand(n);
                value1 = STACK_(n); mv_count=1;
              }
              goto next_byte;
            case (uintB)cod_load_push:       # (LOAD&PUSH n)
              { var reg2 uintL n;
                U_operand(n);
                pushSTACK(STACK_(n));
              }
              goto next_byte;
            case (uintB)cod_loadi:           # (LOADI k n)
              { var reg4 uintL k;
                var reg4 uintL n;
                U_operand(k);
                U_operand(n);
               {var reg2 object* FRAME = (object*) SP_(k);
                value1 = FRAME_(n); mv_count=1;
              }}
              goto next_byte;
            case (uintB)cod_loadi_push:      # (LOADI&PUSH k n)
              { var reg4 uintL k;
                var reg4 uintL n;
                U_operand(k);
                U_operand(n);
               {var reg2 object* FRAME = (object*) SP_(k);
                pushSTACK(FRAME_(n));
              }}
              goto next_byte;
            case (uintB)cod_loadc:           # (LOADC n m)
              { var reg4 uintL n;
                var reg2 uintL m;
                U_operand(n);
                U_operand(m);
                value1 = TheSvector(STACK_(n))->data[1+m]; mv_count=1;
              }
              goto next_byte;
            case (uintB)cod_loadc_push:      # (LOADC&PUSH n m)
              { var reg4 uintL n;
                var reg2 uintL m;
                U_operand(n);
                U_operand(m);
                pushSTACK(TheSvector(STACK_(n))->data[1+m]);
              }
              goto next_byte;
            case (uintB)cod_loadv:           # (LOADV k m)
              { var reg4 uintC k;
                var reg7 uintL m;
                U_operand(k);
                U_operand(m);
               {var reg2 object venv = TheCclosure(closure)->clos_venv; # VenvConst
                # k mal (svref ... 0) nehmen:
                dotimesC(k,k, { venv = TheSvector(venv)->data[0]; } );
                # (svref ... 1+m) holen:
                value1 = TheSvector(venv)->data[1+m]; mv_count=1;
              }}
              goto next_byte;
            case (uintB)cod_loadv_push:      # (LOADV&PUSH k m)
              { var reg4 uintC k;
                var reg7 uintL m;
                U_operand(k);
                U_operand(m);
               {var reg2 object venv = TheCclosure(closure)->clos_venv; # VenvConst
                # k mal (svref ... 0) nehmen:
                dotimesC(k,k, { venv = TheSvector(venv)->data[0]; } );
                # (svref ... 1+m) holen:
                pushSTACK(TheSvector(venv)->data[1+m]);
              }}
              goto next_byte;
            case (uintB)cod_loadic:          # (LOADIC k n m)
              { var reg8 uintL k;
                var reg7 uintL n;
                var reg4 uintL m;
                U_operand(k);
                U_operand(n);
                U_operand(m);
               {var reg2 object* FRAME = (object*) SP_(k);
                value1 = TheSvector(FRAME_(n))->data[1+m]; mv_count=1;
              }}
              goto next_byte;
            case (uintB)cod_store: store:    # (STORE n)
              { var reg2 uintL n;
                U_operand(n);
                STACK_(n) = value1; mv_count=1;
              }
              goto next_byte;
            case (uintB)cod_pop_store:       # (POP&STORE n)
              { var reg2 uintL n;
                U_operand(n);
               {var reg4 object obj = popSTACK();
                STACK_(n) = value1 = obj; mv_count=1;
              }}
              goto next_byte;
            case (uintB)cod_storei:          # (STOREI k n)
              { var reg4 uintL k;
                var reg4 uintL n;
                U_operand(k);
                U_operand(n);
               {var reg2 object* FRAME = (object*) SP_(k);
                FRAME_(n) = value1; mv_count=1;
              }}
              goto next_byte;
            case (uintB)cod_load_storec:     # (LOAD&STOREC k m n)
              { var reg2 uintL k;
                U_operand(k);
                value1 = STACK_(k);
              }
            case (uintB)cod_storec:          # (STOREC n m)
              { var reg4 uintL n;
                var reg2 uintL m;
                U_operand(n);
                U_operand(m);
                TheSvector(STACK_(n))->data[1+m] = value1; mv_count=1;
              }
              goto next_byte;
            case (uintB)cod_storev:          # (STOREV k m)
              { var reg4 uintC k;
                var reg7 uintL m;
                U_operand(k);
                U_operand(m);
               {var reg2 object venv = TheCclosure(closure)->clos_venv; # VenvConst
                # k mal (svref ... 0) nehmen:
                dotimesC(k,k, { venv = TheSvector(venv)->data[0]; } );
                # (svref ... 1+m) abspeichern:
                TheSvector(venv)->data[1+m] = value1; mv_count=1;
              }}
              goto next_byte;
            case (uintB)cod_storeic:         # (STOREIC k n m)
              { var reg8 uintL k;
                var reg7 uintL n;
                var reg4 uintL m;
                U_operand(k);
                U_operand(n);
                U_operand(m);
               {var reg2 object* FRAME = (object*) SP_(k);
                TheSvector(FRAME_(n))->data[1+m] = value1; mv_count=1;
              }}
              goto next_byte;
            # ------------------- (3) dynamische Variablen -----------------------
            case (uintB)cod_getvalue:        # (GETVALUE n)
              { var reg4 uintL n;
                U_operand(n);
               {var reg2 object symbol = TheCclosure(closure)->clos_consts[n];
                # Der Compiler hat schon überprüft, daß es ein Symbol ist.
                if (eq(Symbol_value(symbol),unbound))
                  { pushSTACK(symbol);
                    fehler(
                           DEUTSCH ? "Symbol ~ hat keinen Wert." :
                           ENGLISH ? "symbol ~ has no value" :
                           FRANCAIS ? "Le symbôle ~ n'a pas de valeur." :
                           ""
                          );
                  }
                value1 = Symbol_value(symbol); mv_count=1;
              }}
              goto next_byte;
            case (uintB)cod_getvalue_push:   # (GETVALUE&PUSH n)
              { var reg4 uintL n;
                U_operand(n);
               {var reg2 object symbol = TheCclosure(closure)->clos_consts[n];
                # Der Compiler hat schon überprüft, daß es ein Symbol ist.
                if (eq(Symbol_value(symbol),unbound))
                  { pushSTACK(symbol);
                    fehler(
                           DEUTSCH ? "Symbol ~ hat keinen Wert." :
                           ENGLISH ? "symbol ~ has no value" :
                           FRANCAIS ? "Le symbôle ~ n'a pas de valeur." :
                           ""
                          );
                  }
                pushSTACK(Symbol_value(symbol));
              }}
              goto next_byte;
            case (uintB)cod_setvalue:        # (SETVALUE n)
              { var reg4 uintL n;
                U_operand(n);
               {var reg2 object symbol = TheCclosure(closure)->clos_consts[n];
                # Der Compiler hat schon überprüft, daß es ein Symbol ist.
                if (constantp(TheSymbol(symbol)))
                  { pushSTACK(symbol);
                    fehler(
                           DEUTSCH ? "Zuweisung nicht möglich auf das konstante Symbol ~" :
                           ENGLISH ? "assignment to constant symbol ~ is impossible" :
                           FRANCAIS ? "Une assignation du symbôle constant ~ n'est pas possible." :
                           ""
                          );
                  }
                Symbol_value(symbol) = value1; mv_count=1;
              }}
              goto next_byte;
            case (uintB)cod_bind:            # (BIND n)
              { var reg2 uintL n;
                U_operand(n);
                dynamic_bind(TheCclosure(closure)->clos_consts[n],value1);
              }
              goto next_byte;
            case (uintB)cod_unbind1:         # (UNBIND1)
              #if STACKCHECKC
              if (!(mtypecode(STACK_0) == DYNBIND_frame_info))
                goto fehler_STACK_putt;
              #endif
              # Variablenbindungsframe auflösen:
              { var reg7 object* new_STACK = topofframe(STACK_0); # Pointer übern Frame
                var reg4 object* frame_end = STACKpointable(new_STACK);
                var reg2 object* bindingptr = &STACK_1; # Beginn der Bindungen
                # bindingptr läuft durch die Bindungen hoch
                until (bindingptr == frame_end)
                  { # alten Wert zurückschreiben:
                    Symbol_value(*(bindingptr STACKop 0)) = *(bindingptr STACKop 1);
                    bindingptr skipSTACKop 2; # nächste Bindung
                  }
                # STACK neu setzen, dadurch Frame auflösen:
                setSTACK(STACK = new_STACK);
              }
              goto next_byte;
            case (uintB)cod_unbind:          # (UNBIND n)
              { var reg8 uintC n;
                U_operand(n); # n>0
               {var reg2 object* FRAME = STACK;
                do {
                    #if STACKCHECKC
                    if (!(mtypecode(FRAME_(0)) == DYNBIND_frame_info))
                      goto fehler_STACK_putt;
                    #endif
                    # Variablenbindungsframe auflösen:
                    { var reg7 object* new_FRAME = topofframe(FRAME_(0)); # Pointer übern Frame
                      var reg4 object* frame_end = STACKpointable(new_FRAME);
                      var reg2 object* bindingptr = &FRAME_(1); # Beginn der Bindungen
                      # bindingptr läuft durch die Bindungen hoch
                      until (bindingptr == frame_end)
                        { # alten Wert zurückschreiben:
                          Symbol_value(*(bindingptr STACKop 0)) = *(bindingptr STACKop 1);
                          bindingptr skipSTACKop 2; # nächste Bindung
                        }
                      FRAME = new_FRAME;
                   }}
                   until (--n == 0);
                setSTACK(STACK = FRAME); # STACK neu setzen
              }}
              goto next_byte;
            case (uintB)cod_progv:           # (PROGV)
              { var reg2 object vallist = value1; # Wertliste
                var reg4 object symlist = popSTACK(); # Symbolliste
                pushSP((uintL)STACK); # STACK in den SP legen
                progv(symlist,vallist); # Frame aufbauen
              }
              goto next_byte;
            # ------------------- (4) Stackoperationen -----------------------
            case (uintB)cod_push:            # (PUSH)
              pushSTACK(value1);
              goto next_byte;
            case (uintB)cod_pop:             # (POP)
              value1 = popSTACK(); mv_count=1;
              goto next_byte;
            case (uintB)cod_skip:            # (SKIP n)
              { var reg2 uintL n;
                U_operand(n);
                skipSTACK(n);
              }
              goto next_byte;
            case (uintB)cod_skipi:           # (SKIPI k n)
              { var reg2 uintL k;
                var reg4 uintL n;
                U_operand(k);
                U_operand(n);
                skipSP(k);
               {var reg2 object* newSTACK;
                popSP( newSTACK = (object*) );
                setSTACK(STACK = newSTACK STACKop n);
              }}
              goto next_byte;
            case (uintB)cod_skipsp:          # (SKIPSP k)
              { var reg2 uintL k;
                U_operand(k);
                skipSP(k);
              }
              goto next_byte;
            # ------------------- (5) Programmfluß und Sprünge -----------------------
            case (uintB)cod_skip_ret:        # (SKIP&RET n)
              { var reg2 uintL n;
                U_operand(n);
                skipSTACK(n);
                goto finished; # Rücksprung zum Aufrufer
              }
            #define JMP()  \
              { var reg2 uintB* label_byteptr; \
                L_operand(label_byteptr);      \
                byteptr = label_byteptr;       \
                goto next_byte;                \
              }
            #define NOTJMP()  \
              { L_operand_ignore(); goto next_byte; }
            jmp1: mv_count=1;
            case (uintB)cod_jmp: jmp:        # (JMP label)
              JMP();
            case (uintB)cod_jmpif:           # (JMPIF label)
              if (!nullp(value1)) goto jmp;
              notjmp:
              NOTJMP();
            case (uintB)cod_jmpifnot:        # (JMPIFNOT label)
              if (nullp(value1)) goto jmp;
              NOTJMP();
            case (uintB)cod_jmpif1:          # (JMPIF1 label)
              if (!nullp(value1)) goto jmp1;
              NOTJMP();
            case (uintB)cod_jmpifnot1:       # (JMPIFNOT1 label)
              if (nullp(value1)) goto jmp1;
              NOTJMP();
            case (uintB)cod_jmpifatom:       # (JMPIFATOM label)
              if (atomp(value1)) goto jmp;
              NOTJMP();
            case (uintB)cod_jmpifconsp:      # (JMPIFCONSP label)
              if (consp(value1)) goto jmp;
              NOTJMP();
            case (uintB)cod_jmpifeq:         # (JMPIFEQ label)
              if (eq(popSTACK(),value1)) goto jmp;
              NOTJMP();
            case (uintB)cod_jmpifnoteq:      # (JMPIFNOTEQ label)
              if (!eq(popSTACK(),value1)) goto jmp;
              NOTJMP();
            case (uintB)cod_jmpifeqto:       # (JMPIFEQTO n label)
              { var reg2 uintL n;
                U_operand(n);
                if (eq(popSTACK(),TheCclosure(closure)->clos_consts[n])) goto jmp;
              }
              NOTJMP();
            case (uintB)cod_jmpifnoteqto:    # (JMPIFNOTEQTO n label)
              { var reg2 uintL n;
                U_operand(n);
                if (!eq(popSTACK(),TheCclosure(closure)->clos_consts[n])) goto jmp;
              }
              NOTJMP();
            case (uintB)cod_jmphash:         # (JMPHASH n label)
              { var reg7 uintL n;
                U_operand(n);
               {var reg4 object hashvalue = # value1 in der Hash-Tabelle suchen
                  gethash(value1,TheCclosure(closure)->clos_consts[n]);
                if (hashvalue == nullobj)
                  goto jmp; # nicht gefunden -> zu label springen
                  else # gefundenes Fixnum als Label interpretieren:
                  { byteptr += fixnum_to_L(hashvalue); }
              }}
              goto next_byte;
            # Führt einen (JSR label)-Befehl aus.
            #define JSR()  \
              check_STACK(); check_SP();                              \
              { var reg2 uintB* label_byteptr;                        \
                L_operand(label_byteptr);                             \
                with_saved_context(                                   \
                  interpret_bytecode_(closure,codeptr,label_byteptr); \
                  );                                                  \
              }
            case (uintB)cod_jsr:             # (JSR label)
              JSR();
              goto next_byte;
            case (uintB)cod_jsr_push:        # (JSR&PUSH label)
              JSR(); pushSTACK(value1);
              goto next_byte;
            case (uintB)cod_jmptail:         # (JMPTAIL m n label)
              { var reg7 uintL m;
                var reg8 uintL n;
                U_operand(m);
                U_operand(n);
                # Es gilt n>=m. m Stackeinträge um n-m nach oben kopieren:
               {var reg4 object* ptr1 = STACK STACKop m;
                var reg2 object* ptr2 = STACK STACKop n;
                var reg6 uintC count;
                dotimesC(count,m, { NEXT(ptr2) = NEXT(ptr1); } );
                # Nun ist ptr1 = STACK und ptr2 = STACK STACKop (n-m).
                *(closureptr = &NEXT(ptr2)) = closure; # Closure im Stack ablegen
                setSTACK(STACK = ptr2); # STACK verkürzen
              }}
              JMP(); # an label springen
            # ------------------- (6) Environments und Closures -----------------------
            case (uintB)cod_venv:            # (VENV)
              # VenvConst aus der Closure holen:
              value1 = TheCclosure(closure)->clos_venv; mv_count=1;
              goto next_byte;
            case (uintB)cod_make_vector1_push: # (MAKE-VECTOR1&PUSH n)
              { var reg4 uintL n;
                U_operand(n);
                pushSTACK(value1);
                # Vektor erzeugen:
               {var reg2 object vec;
                with_saved_context( { vec = allocate_vector(n+1); } );
                # Erstes Element eintragen:
                TheSvector(vec)->data[0] = STACK_0;
                STACK_0 = vec;
              }}
              goto next_byte;
            case (uintB)cod_copy_closure:    # (COPY-CLOSURE m n)
              { var reg9 object old;
                # zu kopierende Closure holen:
               {var reg2 uintL m;
                U_operand(m);
                old = TheCclosure(closure)->clos_consts[m];
               }
                # Closure gleicher Länge allozieren:
               {var reg8 object new;
                pushSTACK(old);
                with_saved_context(
                  new = allocate_record(0,0,TheCclosure(old)->reclength,closure_type);
                  );
                old = popSTACK();
                # Inhalt der alten in die neue Closure kopieren:
                { var reg2 object* newptr = &((Record)TheCclosure(new))->recdata[0];
                  var reg4 object* oldptr = &((Record)TheCclosure(old))->recdata[0];
                  var reg6 uintC count;
                  dotimespC(count,((Record)TheCclosure(old))->reclength,
                    { *newptr++ = *oldptr++; }
                    );
                }
                # Stackinhalt in die neue Closure kopieren:
                { var reg7 uintL n;
                  U_operand(n);
                 {var reg2 object* newptr = &TheCclosure(new)->clos_consts[n];
                  dotimespL(n,n, { *--newptr = popSTACK(); } );
                }}
                value1 = new; mv_count=1;
              }}
              goto next_byte;
            case (uintB)cod_copy_closure_push: # (COPY-CLOSURE&PUSH m n)
              { var reg9 object old;
                # zu kopierende Closure holen:
               {var reg2 uintL m;
                U_operand(m);
                old = TheCclosure(closure)->clos_consts[m];
               }
                # Closure gleicher Länge allozieren:
               {var reg8 object new;
                pushSTACK(old);
                with_saved_context(
                  new = allocate_record(0,0,TheCclosure(old)->reclength,closure_type);
                  );
                old = popSTACK();
                # Inhalt der alten in die neue Closure kopieren:
                { var reg2 object* newptr = &((Record)TheCclosure(new))->recdata[0];
                  var reg4 object* oldptr = &((Record)TheCclosure(old))->recdata[0];
                  var reg6 uintC count;
                  dotimespC(count,((Record)TheCclosure(old))->reclength,
                    { *newptr++ = *oldptr++; }
                    );
                }
                # Stackinhalt in die neue Closure kopieren:
                { var reg7 uintL n;
                  U_operand(n);
                 {var reg2 object* newptr = &TheCclosure(new)->clos_consts[n];
                  dotimespL(n,n, { *--newptr = popSTACK(); } );
                }}
                pushSTACK(new);
              }}
              goto next_byte;
            # ------------------- (7) Funktionsaufrufe -----------------------
            # Führt (CALL k n)-Befehl aus.
            #define CALL()  \
              { var reg4 uintC k; # Argumentezahl                  \
                var reg2 uintL n;                                  \
                U_operand(k);                                      \
                U_operand(n);                                      \
                with_saved_context(                                \
                  funcall(TheCclosure(closure)->clos_consts[n],k); \
                  );                                               \
              }
            # Führt (CALL0 n)-Befehl aus.
            #define CALL0()  \
              { var reg2 uintL n;                                  \
                U_operand(n);                                      \
                with_saved_context(                                \
                  funcall(TheCclosure(closure)->clos_consts[n],0); \
                  );                                               \
              }
            # Führt (CALL1 n)-Befehl aus.
            #define CALL1()  \
              { var reg2 uintL n;                                  \
                U_operand(n);                                      \
                with_saved_context(                                \
                  funcall(TheCclosure(closure)->clos_consts[n],1); \
                  );                                               \
              }
            # Führt (CALL2 n)-Befehl aus.
            #define CALL2()  \
              { var reg2 uintL n;                                  \
                U_operand(n);                                      \
                with_saved_context(                                \
                  funcall(TheCclosure(closure)->clos_consts[n],2); \
                  );                                               \
              }
            # Führt (CALLS1 n)-Befehl aus.
            #define CALLS1()  \
              { var reg2 uintL n;                                         \
                B_operand(n);                                             \
                # Der Compiler hat die Argumentüberprüfung schon gemacht. \
               {var reg2 Subr fun = FUNTAB1[n];                           \
                subr_self = Subr_to_object(fun);                          \
                with_saved_context(                                       \
                  (*(subr_norest_function*)(fun->function))();            \
                  );                                                      \
              }}
            # Führt (CALLS2 n)-Befehl aus.
            #define CALLS2()  \
              { var reg2 uintL n;                                         \
                B_operand(n);                                             \
                # Der Compiler hat die Argumentüberprüfung schon gemacht. \
               {var reg2 Subr fun = FUNTAB2[n];                           \
                subr_self = Subr_to_object(fun);                          \
                with_saved_context(                                       \
                  (*(subr_norest_function*)(fun->function))();            \
                  );                                                      \
              }}
            # Führt (CALLSR m n)-Befehl aus.
            #define CALLSR()  \
              { var reg4 uintL m;                                         \
                var reg2 uintL n;                                         \
                U_operand(m);                                             \
                B_operand(n);                                             \
                # Der Compiler hat die Argumentüberprüfung schon gemacht. \
               {var reg2 Subr fun = FUNTABR[n];                           \
                subr_self = Subr_to_object(fun);                          \
                with_saved_context(                                       \
                  (*(subr_rest_function*)(fun->function))(m,args_end_pointer STACKop m); \
                  );                                                      \
              }}
            case (uintB)cod_call:            # (CALL k n)
              CALL();
              goto next_byte;
            case (uintB)cod_call_push:       # (CALL&PUSH k n)
              CALL(); pushSTACK(value1);
              goto next_byte;
            case (uintB)cod_call0:           # (CALL0 n)
              CALL0();
              goto next_byte;
            case (uintB)cod_call1:           # (CALL1 n)
              CALL1();
              goto next_byte;
            case (uintB)cod_call1_push:      # (CALL1&PUSH n)
              CALL1(); pushSTACK(value1);
              goto next_byte;
            case (uintB)cod_call2:           # (CALL2 n)
              CALL2();
              goto next_byte;
            case (uintB)cod_call2_push:      # (CALL2&PUSH n)
              CALL2(); pushSTACK(value1);
              goto next_byte;
            case (uintB)cod_calls1:          # (CALLS1 n)
              CALLS1();
              goto next_byte;
            case (uintB)cod_calls1_push:     # (CALLS1&PUSH n)
              CALLS1(); pushSTACK(value1);
              goto next_byte;
            case (uintB)cod_calls2:          # (CALLS2 n)
              CALLS2();
              goto next_byte;
            case (uintB)cod_calls2_push:     # (CALLS2&PUSH n)
              CALLS2(); pushSTACK(value1);
              goto next_byte;
            case (uintB)cod_callsr:          # (CALLSR m n)
              CALLSR();
              goto next_byte;
            case (uintB)cod_callsr_push:     # (CALLSR&PUSH m n)
              CALLSR(); pushSTACK(value1);
              goto next_byte;
            # Führt einen (CALLC)-Befehl aus.
            #define CALLC()  \
              { check_STACK(); check_SP(); # STACK und SP überprüfen \
                with_saved_context(                                  \
                  # compilierte Closure ab Byte 6 interpretieren:    \
                  interpret_bytecode(value1,TheCclosure(value1)->clos_codevec,CCHD+6); \
                  );                                                 \
              }
            # Führt einen (CALLCKEY)-Befehl aus.
            #define CALLCKEY()  \
              { check_STACK(); check_SP(); # STACK und SP überprüfen \
                with_saved_context(                                  \
                  # compilierte Closure ab Byte 10 interpretieren:   \
                  interpret_bytecode(value1,TheCclosure(value1)->clos_codevec,CCHD+10); \
                  );                                                 \
              }
            case (uintB)cod_callc:           # (CALLC)
              CALLC();
              goto next_byte;
            case (uintB)cod_callc_push:      # (CALLC&PUSH)
              CALLC(); pushSTACK(value1);
              goto next_byte;
            case (uintB)cod_callckey:        # (CALLCKEY)
              CALLCKEY();
              goto next_byte;
            case (uintB)cod_callckey_push:   # (CALLCKEY&PUSH)
              CALLCKEY(); pushSTACK(value1);
              goto next_byte;
            case (uintB)cod_funcall:         # (FUNCALL n)
              { var reg2 uintL n;
                U_operand(n);
               {var reg4 object fun = STACK_(n); # Funktion
                with_saved_context( funcall(fun,n); ); # Funktion aufrufen
                skipSTACK(1); # Funktion aus dem Stack streichen
              }}
              goto next_byte;
            case (uintB)cod_funcall_push:    # (FUNCALL&PUSH n)
              { var reg2 uintL n;
                U_operand(n);
               {var reg4 object fun = STACK_(n); # Funktion
                with_saved_context( funcall(fun,n); ); # Funktion aufrufen
                STACK_0 = value1; # Funktion im Stack durch den Wert ersetzen
              }}
              goto next_byte;
            case (uintB)cod_apply:           # (APPLY n)
              { var reg2 uintL n;
                U_operand(n);
               {var reg4 object fun = STACK_(n); # Funktion
                with_saved_context( apply(fun,n,value1); ); # Funktion aufrufen
                skipSTACK(1); # Funktion aus dem Stack streichen
              }}
              goto next_byte;
            case (uintB)cod_apply_push:      # (APPLY&PUSH n)
              { var reg2 uintL n;
                U_operand(n);
               {var reg4 object fun = STACK_(n); # Funktion
                with_saved_context( apply(fun,n,value1); ); # Funktion aufrufen
                STACK_0 = value1; # Funktion im Stack durch den Wert ersetzen
              }}
              goto next_byte;
            # ------------------- (8) optionale und Keyword-Argumente -----------------------
            case (uintB)cod_push_unbound:    # (PUSH-UNBOUND n)
              { var reg2 uintC n;
                U_operand(n);
                dotimesC(n,n, { pushSTACK(unbound); } );
              }
              goto next_byte;
            case (uintB)cod_jmpifboundp:
              { var reg4 uintL n;
                U_operand(n);
               {var reg2 object obj = STACK_(n);
                if (eq(obj,unbound)) goto notjmp;
                value1 = obj; mv_count=1; JMP();
              }}
            case (uintB)cod_boundp:          # (BOUNDP n)
              { var reg4 uintL n;
                U_operand(n);
               {var reg2 object obj = STACK_(n);
                if (eq(obj,unbound)) goto code_nil; else goto code_t;
              }}
            case (uintB)cod_unbound_nil:     # (UNBOUND->NIL n)
              { var reg2 uintL n;
                U_operand(n);
                if (eq(STACK_(n),unbound)) { STACK_(n) = NIL; }
              }
              goto next_byte;
            # ------------------- (9) Behandlung mehrerer Werte -----------------------
            case (uintB)cod_values0:         # (VALUES0)
              value1 = NIL; mv_count = 0;
              goto next_byte;
            case (uintB)cod_values1:         # (VALUES1)
              mv_count = 1;
              goto next_byte;
            case (uintB)cod_stack_to_mv:     # (STACK-TO-MV n)
              { var reg2 uintL n;
                U_operand(n);
                if (n >= mv_limit) goto fehler_zuviele_werte;
                STACK_to_mv(n);
              }
              goto next_byte;
            case (uintB)cod_mv_to_stack:     # (MV-TO-STACK)
              mv_to_STACK(); # Werte auf den Stack schieben
              goto next_byte;
            case (uintB)cod_nv_to_stack:     # (NV-TO-STACK n)
              { var reg4 uintL n;
                U_operand(n);
                # Test auf Stacküberlauf:
                get_space_on_STACK(n*sizeof(object));
                # n Werte in den Stack schieben:
               {var reg7 uintC count = mv_count;
                if (n==0) goto nv_to_stack_end; # kein Wert gewünscht -> fertig
                # mindestens 1 Wert gewünscht
                pushSTACK(value1);
                n--; if (n==0) goto nv_to_stack_end; # nur 1 Wert gewünscht -> fertig
                if (count<=1) goto nv_to_stack_fill; # nur 1 Wert vorhanden -> mit NILs auffüllen
                count--;
                # mindestens 2 Werte gewünscht und vorhanden
                { var reg2 object* mvp = &mv_space[1];
                  loop
                    { pushSTACK(*mvp++);
                      n--; if (n==0) goto nv_to_stack_end; # kein Wert mehr gewünscht -> fertig
                      count--; if (count==0) goto nv_to_stack_fill; # kein Wert mehr vorhanden -> mit NILs auffüllen
                }   }
                nv_to_stack_fill: # Auffüllen mit n>0 NILs als zusätzlichen Werten:
                dotimespL(n,n, { pushSTACK(NIL); } );
                nv_to_stack_end: ;
              }}
              goto next_byte;
            case (uintB)cod_mv_to_list:      # (MV-TO-LIST)
              with_saved_context(
                # Werte auf den Stack schieben und daraus Liste basteln:
                mv_to_list();
                );
              value1 = popSTACK(); mv_count=1;
              goto next_byte;
            case (uintB)cod_list_to_mv:      # (LIST-TO-MV)
              list_to_mv(value1, { goto fehler_zuviele_werte; } );
              goto next_byte;
            case (uintB)cod_mvcallp:         # (MVCALLP)
              pushSP((uintL)STACK); # STACK retten
              pushSTACK(value1); # auszuführende Funktion retten
              goto next_byte;
            case (uintB)cod_mvcall:          # (MVCALL)
              { var reg2 object* FRAME; popSP( FRAME = (object*) ); # Pointer über Argumente und Funktion
               {var reg7 object fun = NEXT(FRAME); # Funktion
                with_saved_context(
                  {var reg4 uintL argcount = # Anzahl der Argumente auf dem Stack
                     STACK_item_count(STACK,FRAME);
                   if (((uintL)~(uintL)0 > ca_limit_1) && (argcount > ca_limit_1))
                     { pushSTACK(fun);
                       pushSTACK(S(multiple_value_call));
                       fehler(
                              DEUTSCH ? "~: Zu viele Argumente für ~" :
                              ENGLISH ? "~: too many arguments given to ~" :
                              FRANCAIS ? "~: Trop d'arguments pour ~" :
                              ""
                             );
                     }
                   # Funktion anwenden, Stack anheben bis unter die Funktion:
                   funcall(fun,argcount);
                   skipSTACK(1); # Funktion aus dem STACK streichen
                  });
              }}
              goto next_byte;
            # ------------------- (10) BLOCK -----------------------
            case (uintB)cod_block_open:      # (BLOCK-OPEN n label)
              # belegt 3 STACK-Einträge und 1 SP-jmp_buf-Eintrag und 2 SP-Einträge
              { var reg4 uintL n;
                var reg7 sintL label_dist;
                U_operand(n);
                S_operand(label_dist);
                # Block_Cons erzeugen:
               {var reg2 object block_cons;
                with_saved_context(
                  block_cons = allocate_cons();
                  label_dist += index; # CODEPTR+label_dist ist das Sprungziel
                  );
                # Block-Cons füllen: (CONST n) als CAR
                Car(block_cons) = TheCclosure(closure)->clos_consts[n];
                # Sprungziel in den SP:
                pushSP(label_dist); pushSP((aint)closureptr);
                # CBLOCK-Frame aufbauen:
                { var reg7 object* top_of_frame = STACK; # Pointer übern Frame
                  pushSTACK(block_cons); # Cons ( (CONST n) . ...)
                 {var reg4 JMPBUF_on_SP(returner); # Rücksprungpunkt merken
                  finish_entry_frame_1(CBLOCK,returner, goto block_return; );
                }}
                # Framepointer im Block-Cons ablegen:
                Cdr(block_cons) = make_framepointer(STACK);
              }}
              goto next_byte;
              block_return: # Hierher wird gesprungen, wenn der oben aufgebaute
                            # CBLOCK-Frame ein RETURN-FROM gefangen hat.
              { FREE_JMPBUF_on_SP();
                skipSTACK(2); # CBLOCK-Frame auflösen, dabei
                Cdr(popSTACK()) = disabled; # Block-Cons als Disabled markieren
               {var reg2 uintL index;
                # closure zurück, byteptr:=label_byteptr :
                popSP(closureptr = (object*) ); popSP(index = );
                closure = *closureptr; # Closure aus dem Stack holen
                codeptr = TheSbvector(TheCclosure(closure)->clos_codevec);
                byteptr = CODEPTR + index;
              }}
              goto next_byte; # am Label weiterinterpretieren
            case (uintB)cod_block_close:     # (BLOCK-CLOSE)
              # CBLOCK-Frame auflösen:
              #if STACKCHECKC
              if (!(mtypecode(STACK_0) == CBLOCK_frame_info))
                goto fehler_STACK_putt;
              #endif
              { FREE_JMPBUF_on_SP();
                skipSTACK(2); # CBLOCK-Frame auflösen, dabei
                Cdr(popSTACK()) = disabled; # Block-Cons als Disabled markieren
                skipSP(2); # Ziel-Closureptr und Ziel-Label kennen wir
              }
              goto next_byte; # am Label gleich weiterinterpretieren
            case (uintB)cod_return_from:     # (RETURN-FROM n)
              { var reg4 uintL n;
                U_operand(n);
               {var reg2 object block_cons = TheCclosure(closure)->clos_consts[n];
                if (eq(Cdr(block_cons),disabled))
                  { fehler_block_left(Car(block_cons)); }
                # Bis zum Block-Frame unwinden, dann seine Routine zum Auflösen anspringen:
                unwind_upto(uTheFramepointer(Cdr(block_cons)));
              }}
            # ------------------- (11) TAGBODY -----------------------
            case (uintB)cod_tagbody_open:    # (TAGBODY-OPEN m label1 ... labelm)
              # belegt 3+m STACK-Einträge und 1 SP-jmp_buf-Eintrag und 1 SP-Eintrag
              { var reg7 uintL m;
                U_operand(m);
                get_space_on_STACK(m*sizeof(object)); # Platz reservieren
                # alle labeli als Fixnums auf den STACK legen:
                {var reg4 uintL count;
                 dotimesL(count,m,
                   { var reg2 uintB* label_byteptr;
                     L_operand(label_byteptr);
                     pushSTACK(fixnum(label_byteptr - CODEPTR));
                   });
                }
                # Tagbody-Cons erzeugen:
               {var reg2 object tagbody_cons;
                with_saved_context(
                  tagbody_cons = allocate_cons();
                  );
                # Tagbody-Cons füllen: m als Fixnum als CAR
                Car(tagbody_cons) = fixnum(m);
                # Sprungziel in den SP:
                pushSP((aint)closureptr);
                # CTAGBODY-Frame aufbauen:
                { var reg9 object* top_of_frame = STACK; # Pointer übern Frame
                  pushSTACK(tagbody_cons); # Cons ( (CONST n) . ...)
                 {var reg4 JMPBUF_on_SP(returner); # Rücksprungpunkt merken
                  finish_entry_frame_1(CTAGBODY,returner, goto tagbody_go; );
                }}
                # Framepointer im Tagbody-Cons ablegen:
                Cdr(tagbody_cons) = make_framepointer(STACK);
              }}
              goto next_byte;
              tagbody_go: # Hierher wird gesprungen, wenn der oben aufgebaute
                          # CTAGBODY-Frame ein GO zum Label Nummer i gefangen hat.
              { var reg7 uintL m = posfixnum_to_L(Car(STACK_2)); # Anzahl der Labels
                # (Könnte auch das obige m als 'auto' deklarieren und hier benutzen.)
                var reg4 uintL i = posfixnum_to_L(value1); # Nummer des Labels
                var reg2 uintL index = posfixnum_to_L(STACK_((m-i)+3)); # labeli
                # closure zurück, byteptr:=labeli_byteptr :
                closureptr = (object*) SP_(jmpbufsize+0);
                closure = *closureptr; # Closure aus dem Stack holen
                codeptr = TheSbvector(TheCclosure(closure)->clos_codevec);
                byteptr = CODEPTR + index;
              }
              goto next_byte; # am Label i weiterinterpretieren
            case (uintB)cod_tagbody_close_nil: # (TAGBODY-CLOSE-NIL)
              value1 = NIL; mv_count=1; # Wert des Tagbody ist NIL
            case (uintB)cod_tagbody_close:   # (TAGBODY-CLOSE)
              # CTAGBODY-Frame auflösen:
              #if STACKCHECKC
              if (!(mtypecode(STACK_0) == CTAGBODY_frame_info))
                goto fehler_STACK_putt;
              #endif
              { FREE_JMPBUF_on_SP();
               {var reg2 object tagbody_cons = STACK_2; # Tagbody-Cons
                Cdr(tagbody_cons) = disabled; # als Disabled markieren
                skipSTACK(3+posfixnum_to_L(Car(tagbody_cons)));
                skipSP(1);
              }}
              goto next_byte;
            case (uintB)cod_go:              # (GO n k)
              { var reg7 uintL n;
                var reg7 uintL k;
                U_operand(n);
                U_operand(k);
               {var reg2 object tagbody_cons = # (CONST n)
                  TheCclosure(closure)->clos_consts[n];
                if (eq(Cdr(tagbody_cons),disabled))
                  { var reg8 object tag_vector = Car(tagbody_cons);
                    pushSTACK(tag_vector);
                    pushSTACK(TheSvector(tag_vector)->data[k]); # Marke k
                    pushSTACK(S(go));
                    fehler(
                           DEUTSCH ? "(~ ~): Der Tagbody mit den Marken ~ wurde bereits verlassen." :
                           ENGLISH ? "(~ ~): the tagbody of the tags ~ has already been left" :
                           FRANCAIS ? "(~ ~): Le «tagbody» avec les marqueurs ~ a déjà été quitté." :
                           ""
                          );
                  }
                # Übergabewert an den Tagbody:
                # Bei CTAGBODY-Frames 1+k als Fixnum,
                # bei ITAGBODY-Frames die Formenliste zu Tag Nummer k.
                {var reg4 object* FRAME = uTheFramepointer(Cdr(tagbody_cons));
                 value1 = (mtypecode(FRAME_(0)) == CTAGBODY_frame_info
                           ? fixnum(1+k)
                           : FRAME_(frame_bindings+2*k+1)
                          );
                 mv_count=1;
                 # Bis zum Tagbody-Frame unwinden, dann seine Routine anspringen,
                 # die zum Label k springt:
                 unwind_upto(FRAME);
              }}}
            # ------------------- (12) CATCH und THROW -----------------------
            case (uintB)cod_catch_open:      # (CATCH-OPEN label)
              # belegt 3 STACK-Einträge und 1 SP-jmp_buf-Eintrag und 2 SP-Einträge
              { var reg2 uintB* label_byteptr;
                L_operand(label_byteptr);
                # closureptr, label_byteptr retten:
                pushSP(label_byteptr - CODEPTR); pushSP((aint)closureptr);
              } # Frame aufbauen:
              { var reg4 object* top_of_frame = STACK;
                pushSTACK(value1); # Tag
               {var reg2 JMPBUF_on_SP(returner); # Rücksprungpunkt merken
                finish_entry_frame_1(CATCH,returner, goto catch_return; );
              }}
              goto next_byte;
              catch_return: # Hierher wird gesprungen, wenn der oben aufgebaute
                            # Catch-Frame einen Throw gefangen hat.
              { FREE_JMPBUF_on_SP();
                skipSTACK(3); # CATCH-Frame auflösen
               {var reg2 uintL index;
                # closure zurück, byteptr:=label_byteptr :
                popSP(closureptr = (object*) ); popSP(index = );
                closure = *closureptr; # Closure aus dem Stack holen
                codeptr = TheSbvector(TheCclosure(closure)->clos_codevec);
                byteptr = CODEPTR + index;
              }}
              goto next_byte; # am Label weiterinterpretieren
            case (uintB)cod_catch_close:     # (CATCH-CLOSE)
              # Es muß ein CATCH-Frame kommen:
              #if STACKCHECKC
              if (!(mtypecode(STACK_0) == CATCH_frame_info))
                goto fehler_STACK_putt;
              #endif
              FREE_JMPBUF_on_SP();
              #if STACKCHECKC
              if (!(closureptr == SP_(0))) # dort stehender Closureptr muß der jetzige sein
                goto fehler_STACK_putt;
              #endif
              skipSP(2); skipSTACK(3); # CATCH-Frame auflösen
              goto next_byte;
            case (uintB)cod_throw:           # (THROW)
              { var reg2 object tag = popSTACK();
                throw(tag);
                pushSTACK(tag);
                pushSTACK(S(throw));
                fehler(
                       DEUTSCH ? "~: Es gibt kein CATCH zur Marke ~." :
                       ENGLISH ? "~: There is no CATCHer for tag ~" :
                       FRANCAIS ? "~: Il n'y a pas de CATCH pour le marqueur ~." :
                       ""
                      );
              }
            # ------------------- (13) UNWIND-PROTECT -----------------------
            case (uintB)cod_uwp_open:        # (UNWIND-PROTECT-OPEN label)
              # belegt 2 STACK-Einträge und 1 SP-jmp_buf-Eintrag und 2 SP-Einträge
              { var reg2 uintB* label_byteptr;
                L_operand(label_byteptr);
                # closureptr, label_byteptr retten:
                pushSP(label_byteptr - CODEPTR); pushSP((aint)closureptr);
              } # Frame aufbauen:
              { var reg4 object* top_of_frame = STACK;
                var reg2 JMPBUF_on_SP(returner); # Rücksprungpunkt merken
                finish_entry_frame_1(UNWIND_PROTECT,returner, goto throw_save; );
              }
              goto next_byte;
              throw_save: # Hierher wird gesprungen, wenn der oben aufgebaute
                          # Unwind-Protect-Frame einen Throw aufgehalten hat.
              # unwind_protect_to_save ist zu retten und am Schluß anzuspringen.
              #if STACKCHECKC
              if (!(mtypecode(STACK_0) == UNWIND_PROTECT_frame_info))
                { fehler(
                         DEUTSCH ? "STACK kaputt." :
                         ENGLISH ? "STACK corrupted" :
                         FRANCAIS ? "Pile STACK est corrompue." :
                         ""
                        );
                }
              #endif
              # Frame auflösen:
              FREE_JMPBUF_on_SP();
              skipSTACK(2);
              { var reg2 uintL index;
                # closure zurück, byteptr:=label_byteptr :
                popSP(closureptr = (object*) );
                popSP(index = );
                # unwind_protect_to_save retten:
                pushSP((uintL)unwind_protect_to_save.fun);
                pushSP((uintL)unwind_protect_to_save.upto_frame);
                pushSP((uintL)STACK); # Pointer übern Frame zusätzlich auf den SP
                # alle Werte auf den Stack:
                mv_to_STACK();
                # Cleanup-Formen ausführen:
                closure = *closureptr; # Closure aus dem Stack holen
                codeptr = TheSbvector(TheCclosure(closure)->clos_codevec);
                byteptr = CODEPTR + index;
              }
              goto next_byte;
            case (uintB)cod_uwp_normal_exit: # (UNWIND-PROTECT-NORMAL-EXIT)
              #if STACKCHECKC
              if (!(mtypecode(STACK_0) == UNWIND_PROTECT_frame_info))
                goto fehler_STACK_putt;
              if (!(closureptr == SP_(jmpbufsize+0))) # dort stehender Closureptr muß der jetzige sein
                goto fehler_STACK_putt;
              #endif
              # Frame auflösen:
              # Nichts zu tun, da closure und byteptr unverändert bleiben.
              FREE_JMPBUF_on_SP(); skipSP(2);
              skipSTACK(2);
              # Dummy-Werte für 'unwind_protect_to_save':
              pushSP((uintL)NULL); pushSP((uintL)NULL); # NULL,NULL -> uwp_continue
              pushSP((uintL)STACK); # Pointer übern Frame zusätzlich auf den SP
              # alle Werte auf den Stack:
              mv_to_STACK();
              # Cleanup-Formen ausführen:
              goto next_byte;
            case (uintB)cod_uwp_close:       # (UNWIND-PROTECT-CLOSE)
              # Hierher wird am Ende der Cleanup-Formen gesprungen.
              { var reg4 object* oldSTACK; # Wert von STACK vor dem Retten der Werte
                popSP( oldSTACK = (object*) );
               {var reg2 uintL mvcount = # Anzahl der geretteten Werte auf dem Stack
                  STACK_item_count(STACK,oldSTACK);
                if (mvcount >= mv_limit) goto fehler_zuviele_werte;
                STACK_to_mv(mvcount);
              }}
              # Rücksprung zum geretteten unwind_protect_to_save.fun :
              { var reg4 restart fun;
                var reg2 object* arg;
                popSP( arg = (object*) ); popSP( fun = (restart) );
                # Rücksprung zu uwp_continue oder uwp_jmpback oder unwind_upto:
                if (!(fun == (restart)NULL))
                  { (*fun)(arg); } # Rücksprung zu unwind_upto o.ä.
                if (arg == (object*)NULL)
                  { # uwp_continue:
                    # Hierher wird gesprungen, wenn nach dem Ausführen der
                    # Cleanup-Formen einfach weiterinterpretiert werden soll.
                    goto next_byte;
                  }
                  else
                  { # uwp_jmpback:
                    # Hierher wird gesprungen, wenn nach dem Ausführen der
                    # Cleanup-Formen an der alten Stelle in derselben Closure
                    # weiterinterpretiert werden soll.
                    byteptr = CODEPTR + (uintL)arg;
                    goto next_byte;
              }   }
            case (uintB)cod_uwp_cleanup:     # (UNWIND-PROTECT-CLEANUP)
              # Dies wird ausgeführt, wenn innerhalb derselben Closure ein
              # Ausführen des Cleanup-Codes nötig ist.
              #if STACKCHECKC
              if (!(mtypecode(STACK_0) == UNWIND_PROTECT_frame_info))
                goto fehler_STACK_putt;
              if (!(closureptr == SP_(jmpbufsize+0))) # dort stehender Closureptr muß der jetzige sein
                goto fehler_STACK_putt;
              #endif
              # closure bleibt, byteptr:=label_byteptr :
              { var reg2 uintL index = SP_(jmpbufsize+1);
                # Frame auflösen:
                FREE_JMPBUF_on_SP(); skipSP(2);
                skipSTACK(2);
                # Dummy-Werte für 'unwind_protect_to_save':
                pushSP((uintL)NULL); # NULL -> uwp_jmpback
                pushSP(byteptr - CODEPTR);
                pushSP((uintL)STACK); # Pointer übern Frame zusätzlich auf den SP
                # alle Werte auf den Stack:
                mv_to_STACK();
                # Cleanup-Formen ausführen:
                byteptr = CODEPTR + index;
              }
              goto next_byte;
            # ------------------- (14) einige Funktionen -----------------------
            case (uintB)cod_not:             # (NOT)
              if (nullp(value1)) goto code_t; else goto code_nil;
            case (uintB)cod_eq:              # (EQ)
              if (!eq(value1,popSTACK())) goto code_nil; else goto code_t;
            case (uintB)cod_car:             # (CAR)
              { var reg2 object arg = value1;
                if (consp(arg)) { value1 = Car(arg); } # CAR eines Cons
                elif (nullp(arg)) {} # (CAR NIL) = NIL: value1 bleibt NIL
                else { subr_self = L(car); fehler_list(arg); }
                mv_count=1;
              }
              goto next_byte;
            case (uintB)cod_car_push:        # (CAR&PUSH)
              { var reg2 object arg = value1;
                if (consp(arg)) { pushSTACK(Car(arg)); } # CAR eines Cons
                elif (nullp(arg)) { pushSTACK(arg); } # (CAR NIL) = NIL
                else { subr_self = L(car); fehler_list(arg); }
              }
              goto next_byte;
            case (uintB)cod_load_car_push:   # (LOAD&CAR&PUSH n)
              { var reg4 uintL n;
                U_operand(n);
               {var reg2 object arg = STACK_(n);
                if (consp(arg)) { pushSTACK(Car(arg)); } # CAR eines Cons
                elif (nullp(arg)) { pushSTACK(arg); } # (CAR NIL) = NIL
                else { subr_self = L(car); fehler_list(arg); }
              }}
              goto next_byte;
            case (uintB)cod_load_car_store:  # (LOAD&CAR&STORE m n)
              { var reg7 uintL m;
                var reg4 uintL n;
                U_operand(m);
                U_operand(n);
               {var reg2 object arg = STACK_(m);
                if (consp(arg)) { STACK_(n) = value1 = Car(arg); } # CAR eines Cons
                elif (nullp(arg)) { STACK_(n) = value1 = arg; } # (CAR NIL) = NIL
                else { subr_self = L(car); fehler_list(arg); }
                mv_count=1;
              }}
              goto next_byte;
            case (uintB)cod_cdr:             # (CDR)
              { var reg2 object arg = value1;
                if (consp(arg)) { value1 = Cdr(arg); } # CDR eines Cons
                elif (nullp(arg)) {} # (CDR NIL) = NIL: value1 bleibt NIL
                else { subr_self = L(cdr); fehler_list(arg); }
                mv_count=1;
              }
              goto next_byte;
            case (uintB)cod_cdr_push:        # (CDR&PUSH)
              { var reg2 object arg = value1;
                if (consp(arg)) { pushSTACK(Cdr(arg)); } # CDR eines Cons
                elif (nullp(arg)) { pushSTACK(arg); } # (CDR NIL) = NIL
                else { subr_self = L(cdr); fehler_list(arg); }
              }
              goto next_byte;
            case (uintB)cod_load_cdr_push:   # (LOAD&CDR&PUSH n)
              { var reg4 uintL n;
                U_operand(n);
               {var reg2 object arg = STACK_(n);
                if (consp(arg)) { pushSTACK(Cdr(arg)); } # CDR eines Cons
                elif (nullp(arg)) { pushSTACK(arg); } # (CDR NIL) = NIL
                else { subr_self = L(cdr); fehler_list(arg); }
              }}
              goto next_byte;
            case (uintB)cod_load_cdr_store:  # (LOAD&CDR&STORE n)
              { var reg4 uintL n;
                U_operand(n);
               {var reg4 object* arg_ = &STACK_(n);
                var reg2 object arg = *arg_;
                if (consp(arg)) { *arg_ = value1 = Cdr(arg); } # CDR eines Cons
                elif (nullp(arg)) { value1 = arg; } # (CDR NIL) = NIL
                else { subr_self = L(cdr); fehler_list(arg); }
                mv_count=1;
              }}
              goto next_byte;
            case (uintB)cod_cons:            # (CONS)
              pushSTACK(value1);
              # Cons anfordern:
              {var reg2 object new_cons;
               with_saved_context( { new_cons = allocate_cons(); } );
               # Cons füllen:
               Cdr(new_cons) = popSTACK();
               Car(new_cons) = popSTACK();
               value1 = new_cons; mv_count=1;
              }
              goto next_byte;
            case (uintB)cod_cons_push:       # (CONS&PUSH)
              pushSTACK(value1);
              # Cons anfordern:
              {var reg2 object new_cons;
               with_saved_context( { new_cons = allocate_cons(); } );
               # Cons füllen:
               Cdr(new_cons) = popSTACK();
               Car(new_cons) = STACK_0;
               STACK_0 = new_cons;
              }
              goto next_byte;
            case (uintB)cod_load_cons_store: # (LOAD&CONS&STORE n)
              { var reg4 uintL n;
                U_operand(n);
                # Cons anfordern:
               {var reg2 object new_cons;
                with_saved_context( { new_cons = allocate_cons(); } );
                # Cons füllen:
                Car(new_cons) = popSTACK();
                {var reg4 object* arg_ = &STACK_(n);
                 Cdr(new_cons) = *arg_;
                 value1 = *arg_ = new_cons; mv_count=1;
              }}}
              goto next_byte;
            {var reg2 object symbol;
            case (uintB)cod_symbol_function: # (SYMBOL-FUNCTION)
              symbol = value1;
              if (!symbolp(symbol)) goto csf_kein_symbol;
              if (eq(Symbol_function(symbol),unbound)) goto csf_unbound;
              value1 = Symbol_function(symbol); mv_count=1;
              goto next_byte;
            case (uintB)cod_const_symbol_function: # (CONST&SYMBOL-FUNCTION n)
              {var reg4 uintL n;
               U_operand(n);
               symbol = TheCclosure(closure)->clos_consts[n];
              }
              if (!symbolp(symbol)) goto csf_kein_symbol;
              if (eq(Symbol_function(symbol),unbound)) goto csf_unbound;
              value1 = Symbol_function(symbol); mv_count=1;
              goto next_byte;
            case (uintB)cod_const_symbol_function_push: # (CONST&SYMBOL-FUNCTION&PUSH n)
              {var reg4 uintL n;
               U_operand(n);
               symbol = TheCclosure(closure)->clos_consts[n];
              }
              if (!symbolp(symbol)) goto csf_kein_symbol;
              if (eq(Symbol_function(symbol),unbound)) goto csf_unbound;
              pushSTACK(Symbol_function(symbol));
              goto next_byte;
            case (uintB)cod_const_symbol_function_store: # (CONST&SYMBOL-FUNCTION&STORE n k)
              {var reg4 uintL n;
               U_operand(n);
               symbol = TheCclosure(closure)->clos_consts[n];
              }
              if (!symbolp(symbol)) goto csf_kein_symbol;
              if (eq(Symbol_function(symbol),unbound)) goto csf_unbound;
              {var reg4 uintL k;
               U_operand(k);
               STACK_(k) = value1 = Symbol_function(symbol); mv_count=1;
              }
              goto next_byte;
            csf_kein_symbol:
              fehler_kein_symbol(S(symbol_function),symbol);
            csf_unbound:
              pushSTACK(symbol);
              pushSTACK(S(symbol_function));
              fehler(
                     DEUTSCH ? "~: ~ hat keine Funktionsdefinition." :
                     ENGLISH ? "~: the function ~ is undefined" :
                     FRANCAIS ? "~: la fonction ~ n'est pas définie." :
                     ""
                    );
            }
            {var reg2 object vec; var reg4 object index;
            case (uintB)cod_svref:           # (SVREF)
              # STACK_0 muß ein Simple-Vector sein:
              if (!m_simple_vector_p(STACK_0)) goto svref_kein_svector;
              vec = popSTACK(); # Simple-Vector
              index = value1;
              # und der Index muß ein Fixnum >=0, <Länge(vec) sein:
              {var reg7 uintL i;
               if (!(posfixnump(index) &&
                     ((i = posfixnum_to_L(index)) < TheSvector(vec)->length)
                  ) )
                 goto svref_kein_index;
               value1 = TheSvector(vec)->data[i]; # indiziertes Element als Wert
               mv_count = 1;
              }
              goto next_byte;
            case (uintB)cod_svset:           # (SVSET)
              # STACK_0 muß ein Simple-Vector sein:
              if (!m_simple_vector_p(STACK_0)) goto svref_kein_svector;
              vec = popSTACK(); # Simple-Vector
              index = value1;
              # und der Index muß ein Fixnum >=0, <Länge(vec) sein:
              {var reg7 uintL i;
               if (!(posfixnump(index) &&
                     ((i = posfixnum_to_L(index)) < TheSvector(vec)->length)
                  ) )
                 goto svref_kein_index;
               value1 = TheSvector(vec)->data[i] = popSTACK(); # neues Element hineinstecken
               mv_count = 1;
              }
              goto next_byte;
            svref_kein_svector: # Nicht-Simple-Vector in STACK_0
              pushSTACK(S(svref));
              fehler(
                     DEUTSCH ? "~: ~ ist kein Simple-Vector." :
                     ENGLISH ? "~: ~ is not a simple-vector" :
                     FRANCAIS ? "~: ~ n'est pas de type SIMPLE-VECTOR." :
                     ""
                    );
            svref_kein_index: # unpassender Index in index, zum Vektor vec
              pushSTACK(vec);
              pushSTACK(index);
              pushSTACK(S(svref));
              fehler(
                     DEUTSCH ? "~: ~ ist kein passender Index für ~" :
                     ENGLISH ? "~: ~ is not a correct index into ~" :
                     FRANCAIS ? "~: ~ n'est pas un index convenable dans ~." :
                     ""
                    );
            }
            case (uintB)cod_list:            # (LIST n)
              { var reg2 uintC n;
                U_operand(n);
                with_saved_context( { value1 = listof(n); mv_count=1; } );
              }
              goto next_byte;
            case (uintB)cod_list_push:       # (LIST&PUSH n)
              { var reg2 uintC n;
                U_operand(n);
                with_saved_context( { pushSTACK(listof(n)); } );
              }
              goto next_byte;
            case (uintB)cod_error:           # (ERROR n)
              { var reg2 uintL n;
                U_operand(n);
                C_error(n,args_end_pointer STACKop n); # funcall(L(error),n);
              }
              NOTREACHED
            # ------------------- (15) kombinierte Operationen -----------------------
            case (uintB)cod_nil_store:       # (NIL&STORE n)
              {var reg1 uintL n;
               U_operand(n);
               STACK_(n) = value1 = NIL; mv_count=1;
              }
              goto next_byte;
            case (uintB)cod_t_store:         # (T&STORE n)
              {var reg1 uintL n;
               U_operand(n);
               STACK_(n) = value1 = T; mv_count=1;
              }
              goto next_byte;
            case (uintB)cod_calls1_store:    # (CALLS1&STORE n k)
              CALLS1();
              goto store;
            case (uintB)cod_calls2_store:    # (CALLS2&STORE n k)
              CALLS2();
              goto store;
            case (uintB)cod_callsr_store:    # (CALLSR&STORE m n k)
              CALLSR();
              goto store;
            # Incrementieren. Speziell optimiert für Fixnums >=0.
            #define INC(arg,statement)  \
              { if (posfixnump(arg) # Fixnum >= 0 und < most-positive-fixnum ? \
                    && !eq(arg,fixnum(bitm(oint_addr_len)-1))                  \
                   )                                                           \
                  { arg = fixnum_inc(arg,1); statement; }                      \
                  else                                                         \
                  { with_saved_context(                                        \
                      { pushSTACK(arg); subr_self = L(einsplus); C_einsplus(); } # funcall(L(einsplus),1); \
                      );                                                       \
                    arg = value1;                                              \
              }   }
            # Decrementieren. Speziell optimiert für Fixnums >=0.
            #define DEC(arg,statement)  \
              { if (posfixnump(arg) && !eq(arg,Fixnum_0)) # Fixnum > 0 ? \
                  { arg = fixnum_inc(arg,-1); statement; }               \
                  else                                                   \
                  { with_saved_context(                                  \
                      { pushSTACK(arg); subr_self = L(einsminus); C_einsminus(); } # funcall(L(einsminus),1); \
                      );                                                 \
                    arg = value1;                                        \
              }   }
            case (uintB)cod_load_inc_push:   # (LOAD&INC&PUSH n)
              { var reg4 uintL n;
                U_operand(n);
               {var reg2 object arg = STACK_(n);
                INC(arg,); # incrementieren
                pushSTACK(arg);
              }}
              goto next_byte;
            case (uintB)cod_load_inc_store:  # (LOAD&INC&STORE n)
              { var reg4 uintL n;
                U_operand(n);
               {var reg4 object* arg_ = &STACK_(n);
                var reg2 object arg = *arg_;
                INC(arg,mv_count=1); # incrementieren, 1 Wert
                value1 = *arg_ = arg;
              }}
              goto next_byte;
            case (uintB)cod_load_dec_push:   # (LOAD&DEC&PUSH n)
              { var reg4 uintL n;
                U_operand(n);
               {var reg2 object arg = STACK_(n);
                DEC(arg,); # decrementieren
                pushSTACK(arg);
              }}
              goto next_byte;
            case (uintB)cod_load_dec_store:  # (LOAD&DEC&STORE n)
              { var reg4 uintL n;
                U_operand(n);
               {var reg4 object* arg_ = &STACK_(n);
                var reg2 object arg = *arg_;
                DEC(arg,mv_count=1); # decrementieren, 1 Wert
                value1 = *arg_ = arg;
              }}
              goto next_byte;
            case (uintB)cod_call1_jmpif:     # (CALL1&JMPIF n label)
              CALL1();
              if (!nullp(value1)) goto jmp; else goto notjmp;
            case (uintB)cod_call1_jmpifnot:  # (CALL1&JMPIFNOT n label)
              CALL1();
              if (nullp(value1)) goto jmp; else goto notjmp;
            case (uintB)cod_call2_jmpif:     # (CALL2&JMPIF n label)
              CALL2();
              if (!nullp(value1)) goto jmp; else goto notjmp;
            case (uintB)cod_call2_jmpifnot:  # (CALL2&JMPIFNOT n label)
              CALL2();
              if (nullp(value1)) goto jmp; else goto notjmp;
            case (uintB)cod_calls1_jmpif:    # (CALLS1&JMPIF n label)
              CALLS1();
              if (!nullp(value1)) goto jmp; else goto notjmp;
            case (uintB)cod_calls1_jmpifnot: # (CALLS1&JMPIFNOT n label)
              CALLS1();
              if (nullp(value1)) goto jmp; else goto notjmp;
            case (uintB)cod_calls2_jmpif:    # (CALLS2&JMPIF n label)
              CALLS2();
              if (!nullp(value1)) goto jmp; else goto notjmp;
            case (uintB)cod_calls2_jmpifnot: # (CALLS2&JMPIFNOT n label)
              CALLS2();
              if (nullp(value1)) goto jmp; else goto notjmp;
            case (uintB)cod_callsr_jmpif:    # (CALLSR&JMPIF m n label)
              CALLSR();
              if (!nullp(value1)) goto jmp; else goto notjmp;
            case (uintB)cod_callsr_jmpifnot: # (CALLSR&JMPIFNOT m n label)
              CALLSR();
              if (nullp(value1)) goto jmp; else goto notjmp;
            case (uintB)cod_load_jmpif:      # (LOAD&JMPIF n label)
              {var reg2 uintL n;
               U_operand(n);
               mv_count=1;
               if (!nullp(value1 = STACK_(n))) goto jmp; else goto notjmp;
              }
            case (uintB)cod_load_jmpifnot:   # (LOAD&JMPIFNOT n label)
              {var reg2 uintL n;
               U_operand(n);
               mv_count=1;
               if (nullp(value1 = STACK_(n))) goto jmp; else goto notjmp;
              }
            # ------------------- (16) Kurzcodes -----------------------
            case (uintB)cod_load0:           # (LOAD.S 0)
              value1 = STACK_(0); mv_count=1;
              goto next_byte;
            case (uintB)cod_load1:           # (LOAD.S 1)
              value1 = STACK_(1); mv_count=1;
              goto next_byte;
            case (uintB)cod_load2:           # (LOAD.S 2)
              value1 = STACK_(2); mv_count=1;
              goto next_byte;
            case (uintB)cod_load3:           # (LOAD.S 3)
              value1 = STACK_(3); mv_count=1;
              goto next_byte;
            case (uintB)cod_load4:           # (LOAD.S 4)
              value1 = STACK_(4); mv_count=1;
              goto next_byte;
            case (uintB)cod_load5:           # (LOAD.S 5)
              value1 = STACK_(5); mv_count=1;
              goto next_byte;
            case (uintB)cod_load6:           # (LOAD.S 6)
              value1 = STACK_(6); mv_count=1;
              goto next_byte;
            case (uintB)cod_load7:           # (LOAD.S 7)
              value1 = STACK_(7); mv_count=1;
              goto next_byte;
            case (uintB)cod_load8:           # (LOAD.S 8)
              value1 = STACK_(8); mv_count=1;
              goto next_byte;
            case (uintB)cod_load9:           # (LOAD.S 9)
              value1 = STACK_(9); mv_count=1;
              goto next_byte;
            case (uintB)cod_load10:          # (LOAD.S 10)
              value1 = STACK_(10); mv_count=1;
              goto next_byte;
            case (uintB)cod_load11:          # (LOAD.S 11)
              value1 = STACK_(11); mv_count=1;
              goto next_byte;
            case (uintB)cod_load12:          # (LOAD.S 12)
              value1 = STACK_(12); mv_count=1;
              goto next_byte;
            case (uintB)cod_load13:          # (LOAD.S 13)
              value1 = STACK_(13); mv_count=1;
              goto next_byte;
            case (uintB)cod_load14:          # (LOAD.S 14)
              value1 = STACK_(14); mv_count=1;
              goto next_byte;
            case (uintB)cod_load15:          # (LOAD.S 15)
              value1 = STACK_(15); mv_count=1;
              goto next_byte;
            case (uintB)cod_load16:          # (LOAD.S 16)
              value1 = STACK_(16); mv_count=1;
              goto next_byte;
            case (uintB)cod_load17:          # (LOAD.S 17)
              value1 = STACK_(17); mv_count=1;
              goto next_byte;
            case (uintB)cod_load18:          # (LOAD.S 18)
              value1 = STACK_(18); mv_count=1;
              goto next_byte;
            case (uintB)cod_load19:          # (LOAD.S 19)
              value1 = STACK_(19); mv_count=1;
              goto next_byte;
            case (uintB)cod_load20:          # (LOAD.S 20)
              value1 = STACK_(20); mv_count=1;
              goto next_byte;
            case (uintB)cod_load21:          # (LOAD.S 21)
              value1 = STACK_(21); mv_count=1;
              goto next_byte;
            case (uintB)cod_load_push0:      # (LOAD&PUSH.S 0)
              pushSTACK(STACK_(0));
              goto next_byte;
            case (uintB)cod_load_push1:      # (LOAD&PUSH.S 1)
              pushSTACK(STACK_(1));
              goto next_byte;
            case (uintB)cod_load_push2:      # (LOAD&PUSH.S 2)
              pushSTACK(STACK_(2));
              goto next_byte;
            case (uintB)cod_load_push3:      # (LOAD&PUSH.S 3)
              pushSTACK(STACK_(3));
              goto next_byte;
            case (uintB)cod_load_push4:      # (LOAD&PUSH.S 4)
              pushSTACK(STACK_(4));
              goto next_byte;
            case (uintB)cod_load_push5:      # (LOAD&PUSH.S 5)
              pushSTACK(STACK_(5));
              goto next_byte;
            case (uintB)cod_load_push6:      # (LOAD&PUSH.S 6)
              pushSTACK(STACK_(6));
              goto next_byte;
            case (uintB)cod_load_push7:      # (LOAD&PUSH.S 7)
              pushSTACK(STACK_(7));
              goto next_byte;
            case (uintB)cod_load_push8:      # (LOAD&PUSH.S 8)
              pushSTACK(STACK_(8));
              goto next_byte;
            case (uintB)cod_load_push9:      # (LOAD&PUSH.S 9)
              pushSTACK(STACK_(9));
              goto next_byte;
            case (uintB)cod_load_push10:     # (LOAD&PUSH.S 10)
              pushSTACK(STACK_(10));
              goto next_byte;
            case (uintB)cod_load_push11:     # (LOAD&PUSH.S 11)
              pushSTACK(STACK_(11));
              goto next_byte;
            case (uintB)cod_load_push12:     # (LOAD&PUSH.S 12)
              pushSTACK(STACK_(12));
              goto next_byte;
            case (uintB)cod_load_push13:     # (LOAD&PUSH.S 13)
              pushSTACK(STACK_(13));
              goto next_byte;
            case (uintB)cod_load_push14:     # (LOAD&PUSH.S 14)
              pushSTACK(STACK_(14));
              goto next_byte;
            case (uintB)cod_load_push15:     # (LOAD&PUSH.S 15)
              pushSTACK(STACK_(15));
              goto next_byte;
            case (uintB)cod_load_push16:     # (LOAD&PUSH.S 16)
              pushSTACK(STACK_(16));
              goto next_byte;
            case (uintB)cod_load_push17:     # (LOAD&PUSH.S 17)
              pushSTACK(STACK_(17));
              goto next_byte;
            case (uintB)cod_load_push18:     # (LOAD&PUSH.S 18)
              pushSTACK(STACK_(18));
              goto next_byte;
            case (uintB)cod_load_push19:     # (LOAD&PUSH.S 19)
              pushSTACK(STACK_(19));
              goto next_byte;
            case (uintB)cod_load_push20:     # (LOAD&PUSH.S 20)
              pushSTACK(STACK_(20));
              goto next_byte;
            case (uintB)cod_load_push21:     # (LOAD&PUSH.S 21)
              pushSTACK(STACK_(21));
              goto next_byte;
            case (uintB)cod_const0:          # (CONST.S 0)
              value1 = TheCclosure(closure)->clos_consts[0]; mv_count=1;
              goto next_byte;
            case (uintB)cod_const1:          # (CONST.S 1)
              value1 = TheCclosure(closure)->clos_consts[1]; mv_count=1;
              goto next_byte;
            case (uintB)cod_const2:          # (CONST.S 2)
              value1 = TheCclosure(closure)->clos_consts[2]; mv_count=1;
              goto next_byte;
            case (uintB)cod_const3:          # (CONST.S 3)
              value1 = TheCclosure(closure)->clos_consts[3]; mv_count=1;
              goto next_byte;
            case (uintB)cod_const4:          # (CONST.S 4)
              value1 = TheCclosure(closure)->clos_consts[4]; mv_count=1;
              goto next_byte;
            case (uintB)cod_const5:          # (CONST.S 5)
              value1 = TheCclosure(closure)->clos_consts[5]; mv_count=1;
              goto next_byte;
            case (uintB)cod_const6:          # (CONST.S 6)
              value1 = TheCclosure(closure)->clos_consts[6]; mv_count=1;
              goto next_byte;
            case (uintB)cod_const7:          # (CONST.S 7)
              value1 = TheCclosure(closure)->clos_consts[7]; mv_count=1;
              goto next_byte;
            case (uintB)cod_const8:          # (CONST.S 8)
              value1 = TheCclosure(closure)->clos_consts[8]; mv_count=1;
              goto next_byte;
            case (uintB)cod_const9:          # (CONST.S 9)
              value1 = TheCclosure(closure)->clos_consts[9]; mv_count=1;
              goto next_byte;
            case (uintB)cod_const10:         # (CONST.S 10)
              value1 = TheCclosure(closure)->clos_consts[10]; mv_count=1;
              goto next_byte;
            case (uintB)cod_const11:         # (CONST.S 11)
              value1 = TheCclosure(closure)->clos_consts[11]; mv_count=1;
              goto next_byte;
            case (uintB)cod_const12:         # (CONST.S 12)
              value1 = TheCclosure(closure)->clos_consts[12]; mv_count=1;
              goto next_byte;
            case (uintB)cod_const13:         # (CONST.S 13)
              value1 = TheCclosure(closure)->clos_consts[13]; mv_count=1;
              goto next_byte;
            case (uintB)cod_const14:         # (CONST.S 14)
              value1 = TheCclosure(closure)->clos_consts[14]; mv_count=1;
              goto next_byte;
            case (uintB)cod_const15:         # (CONST.S 15)
              value1 = TheCclosure(closure)->clos_consts[15]; mv_count=1;
              goto next_byte;
            case (uintB)cod_const16:         # (CONST.S 16)
              value1 = TheCclosure(closure)->clos_consts[16]; mv_count=1;
              goto next_byte;
            case (uintB)cod_const17:         # (CONST.S 17)
              value1 = TheCclosure(closure)->clos_consts[17]; mv_count=1;
              goto next_byte;
            case (uintB)cod_const18:         # (CONST.S 18)
              value1 = TheCclosure(closure)->clos_consts[18]; mv_count=1;
              goto next_byte;
            case (uintB)cod_const19:         # (CONST.S 19)
              value1 = TheCclosure(closure)->clos_consts[19]; mv_count=1;
              goto next_byte;
            case (uintB)cod_const20:         # (CONST.S 20)
              value1 = TheCclosure(closure)->clos_consts[20]; mv_count=1;
              goto next_byte;
            case (uintB)cod_const21:         # (CONST.S 21)
              value1 = TheCclosure(closure)->clos_consts[21]; mv_count=1;
              goto next_byte;
            case (uintB)cod_const_push0:     # (CONST&PUSH.S 0)
              pushSTACK(TheCclosure(closure)->clos_consts[0]);
              goto next_byte;
            case (uintB)cod_const_push1:     # (CONST&PUSH.S 1)
              pushSTACK(TheCclosure(closure)->clos_consts[1]);
              goto next_byte;
            case (uintB)cod_const_push2:     # (CONST&PUSH.S 2)
              pushSTACK(TheCclosure(closure)->clos_consts[2]);
              goto next_byte;
            case (uintB)cod_const_push3:     # (CONST&PUSH.S 3)
              pushSTACK(TheCclosure(closure)->clos_consts[3]);
              goto next_byte;
            case (uintB)cod_const_push4:     # (CONST&PUSH.S 4)
              pushSTACK(TheCclosure(closure)->clos_consts[4]);
              goto next_byte;
            case (uintB)cod_const_push5:     # (CONST&PUSH.S 5)
              pushSTACK(TheCclosure(closure)->clos_consts[5]);
              goto next_byte;
            case (uintB)cod_const_push6:     # (CONST&PUSH.S 6)
              pushSTACK(TheCclosure(closure)->clos_consts[6]);
              goto next_byte;
            case (uintB)cod_const_push7:     # (CONST&PUSH.S 7)
              pushSTACK(TheCclosure(closure)->clos_consts[7]);
              goto next_byte;
            case (uintB)cod_const_push8:     # (CONST&PUSH.S 8)
              pushSTACK(TheCclosure(closure)->clos_consts[8]);
              goto next_byte;
            case (uintB)cod_const_push9:     # (CONST&PUSH.S 9)
              pushSTACK(TheCclosure(closure)->clos_consts[9]);
              goto next_byte;
            case (uintB)cod_const_push10:    # (CONST&PUSH.S 10)
              pushSTACK(TheCclosure(closure)->clos_consts[10]);
              goto next_byte;
            case (uintB)cod_const_push11:    # (CONST&PUSH.S 11)
              pushSTACK(TheCclosure(closure)->clos_consts[11]);
              goto next_byte;
            case (uintB)cod_const_push12:    # (CONST&PUSH.S 12)
              pushSTACK(TheCclosure(closure)->clos_consts[12]);
              goto next_byte;
            case (uintB)cod_const_push13:    # (CONST&PUSH.S 13)
              pushSTACK(TheCclosure(closure)->clos_consts[13]);
              goto next_byte;
            case (uintB)cod_const_push14:    # (CONST&PUSH.S 14)
              pushSTACK(TheCclosure(closure)->clos_consts[14]);
              goto next_byte;
            case (uintB)cod_const_push15:    # (CONST&PUSH.S 15)
              pushSTACK(TheCclosure(closure)->clos_consts[15]);
              goto next_byte;
            case (uintB)cod_const_push16:    # (CONST&PUSH.S 16)
              pushSTACK(TheCclosure(closure)->clos_consts[16]);
              goto next_byte;
            case (uintB)cod_const_push17:    # (CONST&PUSH.S 17)
              pushSTACK(TheCclosure(closure)->clos_consts[17]);
              goto next_byte;
            case (uintB)cod_const_push18:    # (CONST&PUSH.S 18)
              pushSTACK(TheCclosure(closure)->clos_consts[18]);
              goto next_byte;
            case (uintB)cod_const_push19:    # (CONST&PUSH.S 19)
              pushSTACK(TheCclosure(closure)->clos_consts[19]);
              goto next_byte;
            case (uintB)cod_const_push20:    # (CONST&PUSH.S 20)
              pushSTACK(TheCclosure(closure)->clos_consts[20]);
              goto next_byte;
            case (uintB)cod_const_push21:    # (CONST&PUSH.S 21)
              pushSTACK(TheCclosure(closure)->clos_consts[21]);
              goto next_byte;
            case (uintB)cod_store0:          # (STORE.S 0)
              STACK_(0) = value1; mv_count=1;
              goto next_byte;
            case (uintB)cod_store1:          # (STORE.S 1)
              STACK_(1) = value1; mv_count=1;
              goto next_byte;
            case (uintB)cod_store2:          # (STORE.S 2)
              STACK_(2) = value1; mv_count=1;
              goto next_byte;
            case (uintB)cod_store3:          # (STORE.S 3)
              STACK_(3) = value1; mv_count=1;
              goto next_byte;
            case (uintB)cod_store4:          # (STORE.S 4)
              STACK_(4) = value1; mv_count=1;
              goto next_byte;
            case (uintB)cod_store5:          # (STORE.S 5)
              STACK_(5) = value1; mv_count=1;
              goto next_byte;
            case (uintB)cod_store6:          # (STORE.S 6)
              STACK_(6) = value1; mv_count=1;
              goto next_byte;
            case (uintB)cod_store7:          # (STORE.S 7)
              STACK_(7) = value1; mv_count=1;
              goto next_byte;
            case (uintB)cod_store8:          # (STORE.S 8)
              STACK_(8) = value1; mv_count=1;
              goto next_byte;
            case (uintB)cod_store9:          # (STORE.S 9)
              STACK_(9) = value1; mv_count=1;
              goto next_byte;
            case (uintB)cod_store10:         # (STORE.S 10)
              STACK_(10) = value1; mv_count=1;
              goto next_byte;
            case (uintB)cod_store11:         # (STORE.S 11)
              STACK_(11) = value1; mv_count=1;
              goto next_byte;
            case (uintB)cod_store12:         # (STORE.S 12)
              STACK_(12) = value1; mv_count=1;
              goto next_byte;
            case (uintB)cod_store13:         # (STORE.S 13)
              STACK_(13) = value1; mv_count=1;
              goto next_byte;
            case (uintB)cod_store14:         # (STORE.S 14)
              STACK_(14) = value1; mv_count=1;
              goto next_byte;
            case (uintB)cod_store15:         # (STORE.S 15)
              STACK_(15) = value1; mv_count=1;
              goto next_byte;
            case (uintB)cod_store16:         # (STORE.S 16)
              STACK_(16) = value1; mv_count=1;
              goto next_byte;
            case (uintB)cod_store17:         # (STORE.S 17)
              STACK_(17) = value1; mv_count=1;
              goto next_byte;
            case (uintB)cod_store18:         # (STORE.S 18)
              STACK_(18) = value1; mv_count=1;
              goto next_byte;
            case (uintB)cod_store19:         # (STORE.S 19)
              STACK_(19) = value1; mv_count=1;
              goto next_byte;
            case (uintB)cod_store20:         # (STORE.S 20)
              STACK_(20) = value1; mv_count=1;
              goto next_byte;
            case (uintB)cod_store21:         # (STORE.S 21)
              STACK_(21) = value1; mv_count=1;
              goto next_byte;
            # ------------------- sonstiges -----------------------
            default:
              # undefinierter Code
              #if defined(GNU) && defined(FAST_SP)
                # -fomit-frame-pointer zunichte machen, damit
                # %sp bzw. %esp als private_SP verwendbar ist:
                alloca(0);
              #endif
              pushSTACK(fixnum(byteptr-&codeptr->data[0]-1)); # fehlerhafte Bytenummer
              pushSTACK(closure); # Closure
              fehler(
                     DEUTSCH ? "Undefinierter Byte-Code in ~ bei Byte ~" :
                     ENGLISH ? "undefined bytecode in ~ at byte ~" :
                     FRANCAIS ? "Code octet indéfinissable ~ à l'octet ~" :
                     ""
                    );
            #undef L_operand
            #undef S_operand
            #undef U_operand
            #undef B_operand
          }
        fehler_zuviele_werte:
          fehler(
                 DEUTSCH ? "Zu viele Werte erzeugt." :
                 ENGLISH ? "too many return values" :
                 FRANCAIS ? "Trop de valeurs VALUES." :
                 ""
                );
        #if STACKCHECKC
        fehler_STACK_putt:
          pushSTACK(fixnum(byteptr-&codeptr->data[0])); # PC
          pushSTACK(closure); # FUNC
          fehler(
                 DEUTSCH ? "Stack kaputt in ~ bei Byte ~" :
                 ENGLISH ? "Corrupted STACK in ~ at byte ~" :
                 FRANCAIS ? "Pile STACK corrompue dans ~ à l'octet ~" :
                 ""
                );
        #endif
        finished:
        #undef FREE_JMPBUF_on_SP
        #undef JMPBUF_on_SP
        #ifndef FAST_SP
        FREE_DYNAMIC_ARRAY(private_SP_space);
        #endif
        return;
    }}}


# wo ist check_SP() oder check_STACK() einzufügen??
# soll nest_env sein Ziel-Environment übergeben bekommen??
# Register-Allozierung in eval_subr und eval_cclosure usw.??
# subr_self eliminieren??

