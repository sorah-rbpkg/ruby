/* DO NOT EDIT THIS FILE DIRECTLY */
/**********************************************************************

  id.c -

  $Author$
  created at: Wed Dec  5 02:36:10 2012

  Copyright (C) 2004-2007 Koichi Sasada

**********************************************************************/

#define tDOT2 RUBY_TOKEN(DOT2)
#define tDOT3 RUBY_TOKEN(DOT3)
#define tUPLUS RUBY_TOKEN(UPLUS)
#define tUMINUS RUBY_TOKEN(UMINUS)
#define tPOW RUBY_TOKEN(POW)
#define tCMP RUBY_TOKEN(CMP)
#define tLSHFT RUBY_TOKEN(LSHFT)
#define tRSHFT RUBY_TOKEN(RSHFT)
#define tLEQ RUBY_TOKEN(LEQ)
#define tGEQ RUBY_TOKEN(GEQ)
#define tEQ RUBY_TOKEN(EQ)
#define tEQQ RUBY_TOKEN(EQQ)
#define tNEQ RUBY_TOKEN(NEQ)
#define tMATCH RUBY_TOKEN(MATCH)
#define tNMATCH RUBY_TOKEN(NMATCH)
#define tAREF RUBY_TOKEN(AREF)
#define tASET RUBY_TOKEN(ASET)
#define tCOLON2 RUBY_TOKEN(COLON2)
#define tANDOP RUBY_TOKEN(ANDOP)
#define tOROP RUBY_TOKEN(OROP)
#define tANDDOT RUBY_TOKEN(ANDDOT)

static const struct {
    unsigned short token;
    RBIMPL_ATTR_NONSTRING() const char name[3];
    const char term;
} op_tbl[] = {
    {tDOT2, ".."},
    {tDOT3, "..."},
    {tUPLUS, "+@"},
    {tUMINUS, "-@"},
    {tPOW, "**"},
    {tCMP, "<=>"},
    {tLSHFT, "<<"},
    {tRSHFT, ">>"},
    {tLEQ, "<="},
    {tGEQ, ">="},
    {tEQ, "=="},
    {tEQQ, "==="},
    {tNEQ, "!="},
    {tMATCH, "=~"},
    {tNMATCH, "!~"},
    {tAREF, "[]"},
    {tASET, "[]="},
    {tCOLON2, "::"},
    {tANDOP, "&&"},
    {tOROP, "||"},
    {tANDDOT, "&."},
};

static void
Init_id(void)
{
    rb_encoding *enc = rb_usascii_encoding();

    REGISTER_SYMID(idMax, "max");
    REGISTER_SYMID(idMin, "min");
    REGISTER_SYMID(idHash, "hash");
    REGISTER_SYMID(idFreeze, "freeze");
    REGISTER_SYMID(idNilP, "nil?");
    REGISTER_SYMID(idInspect, "inspect");
    REGISTER_SYMID(idIntern, "intern");
    REGISTER_SYMID(idObject_id, "object_id");
    REGISTER_SYMID(id__id__, "__id__");
    REGISTER_SYMID(idConst_added, "const_added");
    REGISTER_SYMID(idConst_missing, "const_missing");
    REGISTER_SYMID(idMethodMissing, "method_missing");
    REGISTER_SYMID(idMethod_added, "method_added");
    REGISTER_SYMID(idSingleton_method_added, "singleton_method_added");
    REGISTER_SYMID(idMethod_removed, "method_removed");
    REGISTER_SYMID(idSingleton_method_removed, "singleton_method_removed");
    REGISTER_SYMID(idMethod_undefined, "method_undefined");
    REGISTER_SYMID(idSingleton_method_undefined, "singleton_method_undefined");
    REGISTER_SYMID(idLength, "length");
    REGISTER_SYMID(idSize, "size");
    REGISTER_SYMID(idGets, "gets");
    REGISTER_SYMID(idSucc, "succ");
    REGISTER_SYMID(idEach, "each");
    REGISTER_SYMID(idProc, "proc");
    REGISTER_SYMID(idLambda, "lambda");
    REGISTER_SYMID(idSend, "send");
    REGISTER_SYMID(id__send__, "__send__");
    REGISTER_SYMID(id__recursive_key__, "__recursive_key__");
    REGISTER_SYMID(idInitialize, "initialize");
    REGISTER_SYMID(idInitialize_copy, "initialize_copy");
    REGISTER_SYMID(idInitialize_clone, "initialize_clone");
    REGISTER_SYMID(idInitialize_dup, "initialize_dup");
    REGISTER_SYMID(idTo_int, "to_int");
    REGISTER_SYMID(idTo_ary, "to_ary");
    REGISTER_SYMID(idTo_str, "to_str");
    REGISTER_SYMID(idTo_sym, "to_sym");
    REGISTER_SYMID(idTo_hash, "to_hash");
    REGISTER_SYMID(idTo_proc, "to_proc");
    REGISTER_SYMID(idTo_io, "to_io");
    REGISTER_SYMID(idTo_a, "to_a");
    REGISTER_SYMID(idTo_s, "to_s");
    REGISTER_SYMID(idTo_i, "to_i");
    REGISTER_SYMID(idTo_f, "to_f");
    REGISTER_SYMID(idTo_r, "to_r");
    REGISTER_SYMID(idBt, "bt");
    REGISTER_SYMID(idBt_locations, "bt_locations");
    REGISTER_SYMID(idCall, "call");
    REGISTER_SYMID(idMesg, "mesg");
    REGISTER_SYMID(idException, "exception");
    REGISTER_SYMID(idLocals, "locals");
    REGISTER_SYMID(idNOT, "not");
    REGISTER_SYMID(idAND, "and");
    REGISTER_SYMID(idOR, "or");
    REGISTER_SYMID(idDiv, "div");
    REGISTER_SYMID(idDivmod, "divmod");
    REGISTER_SYMID(idFdiv, "fdiv");
    REGISTER_SYMID(idQuo, "quo");
    REGISTER_SYMID(idName, "name");
    REGISTER_SYMID(idNil, "nil");
    REGISTER_SYMID(idPath, "path");
    REGISTER_SYMID(idPack, "pack");
    REGISTER_SYMID(idBuffer, "buffer");
    REGISTER_SYMID(idIncludeP, "include?");
    REGISTER_SYMID(idUScore, "_");
    REGISTER_SYMID(idNUMPARAM_1, "_1");
    REGISTER_SYMID(idNUMPARAM_2, "_2");
    REGISTER_SYMID(idNUMPARAM_3, "_3");
    REGISTER_SYMID(idNUMPARAM_4, "_4");
    REGISTER_SYMID(idNUMPARAM_5, "_5");
    REGISTER_SYMID(idNUMPARAM_6, "_6");
    REGISTER_SYMID(idNUMPARAM_7, "_7");
    REGISTER_SYMID(idNUMPARAM_8, "_8");
    REGISTER_SYMID(idNUMPARAM_9, "_9");
    REGISTER_SYMID(idNULL, ""/*NULL*/"");
    REGISTER_SYMID(idEmptyP, "empty?");
    REGISTER_SYMID(idEqlP, "eql?");
    REGISTER_SYMID(idDefault, "default");
    REGISTER_SYMID(idRespond_to, "respond_to?");
    REGISTER_SYMID(idRespond_to_missing, "respond_to_missing?");
    REGISTER_SYMID(idIFUNC, "<IFUNC>");
    REGISTER_SYMID(idCFUNC, "<CFUNC>");
    REGISTER_SYMID(id_core_set_method_alias, "core#set_method_alias");
    REGISTER_SYMID(id_core_set_variable_alias, "core#set_variable_alias");
    REGISTER_SYMID(id_core_undef_method, "core#undef_method");
    REGISTER_SYMID(id_core_define_method, "core#define_method");
    REGISTER_SYMID(id_core_define_singleton_method, "core#define_singleton_method");
    REGISTER_SYMID(id_core_set_postexe, "core#set_postexe");
    REGISTER_SYMID(id_core_hash_merge_ptr, "core#hash_merge_ptr");
    REGISTER_SYMID(id_core_hash_merge_kwd, "core#hash_merge_kwd");
    REGISTER_SYMID(id_core_raise, "core#raise");
    REGISTER_SYMID(id_core_sprintf, "core#sprintf");
    REGISTER_SYMID(idLASTLINE, "$_");
    REGISTER_SYMID(idBACKREF, "$~");
    REGISTER_SYMID(idERROR_INFO, "$!");
    REGISTER_SYMID(idRuby, "Ruby");
}
