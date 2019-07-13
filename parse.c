/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 1

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 12 "parse.y" /* yacc.c:339  */


#if !YYPURE
# error needs pure parser
#endif
#ifndef PARSER_DEBUG
#define PARSER_DEBUG 0
#endif
#define YYDEBUG 1
#define YYERROR_VERBOSE 1
#define YYSTACK_USE_ALLOCA 0

#include "ruby/ruby.h"
#include "ruby/st.h"
#include "ruby/encoding.h"
#include "internal.h"
#include "node.h"
#include "parse.h"
#include "symbol.h"
#include "regenc.h"
#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include "probes.h"

#ifndef WARN_PAST_SCOPE
# define WARN_PAST_SCOPE 0
#endif

#define TAB_WIDTH 8

#define YYMALLOC(size)		rb_parser_malloc(parser, (size))
#define YYREALLOC(ptr, size)	rb_parser_realloc(parser, (ptr), (size))
#define YYCALLOC(nelem, size)	rb_parser_calloc(parser, (nelem), (size))
#define YYFREE(ptr)		rb_parser_free(parser, (ptr))
#define YYFPRINTF		rb_parser_printf
#if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
# define YY_LOCATION_PRINT(File, Loc) \
     rb_parser_printf(parser, "%d.%d-%d.%d", \
		      (Loc).first_line, (Loc).first_column, \
		      (Loc).last_line,  (Loc).last_column)
#endif
#undef malloc
#undef realloc
#undef calloc
#undef free
#define malloc	YYMALLOC
#define realloc	YYREALLOC
#define calloc	YYCALLOC
#define free	YYFREE

enum lex_state_bits {
    EXPR_BEG_bit,		/* ignore newline, +/- is a sign. */
    EXPR_END_bit,		/* newline significant, +/- is an operator. */
    EXPR_ENDARG_bit,		/* ditto, and unbound braces. */
    EXPR_ENDFN_bit,		/* ditto, and unbound braces. */
    EXPR_ARG_bit,		/* newline significant, +/- is an operator. */
    EXPR_CMDARG_bit,		/* newline significant, +/- is an operator. */
    EXPR_MID_bit,		/* newline significant, +/- is an operator. */
    EXPR_FNAME_bit,		/* ignore newline, no reserved words. */
    EXPR_DOT_bit,		/* right after `.' or `::', no reserved words. */
    EXPR_CLASS_bit,		/* immediate after `class', no here document. */
    EXPR_LABEL_bit,		/* flag bit, label is allowed. */
    EXPR_LABELED_bit,		/* flag bit, just after a label. */
    EXPR_FITEM_bit,		/* symbol literal as FNAME. */
    EXPR_MAX_STATE
};
/* examine combinations */
enum lex_state_e {
#define DEF_EXPR(n) EXPR_##n = (1 << EXPR_##n##_bit)
    DEF_EXPR(BEG),
    DEF_EXPR(END),
    DEF_EXPR(ENDARG),
    DEF_EXPR(ENDFN),
    DEF_EXPR(ARG),
    DEF_EXPR(CMDARG),
    DEF_EXPR(MID),
    DEF_EXPR(FNAME),
    DEF_EXPR(DOT),
    DEF_EXPR(CLASS),
    DEF_EXPR(LABEL),
    DEF_EXPR(LABELED),
    DEF_EXPR(FITEM),
    EXPR_VALUE = EXPR_BEG,
    EXPR_BEG_ANY  =  (EXPR_BEG | EXPR_MID | EXPR_CLASS),
    EXPR_ARG_ANY  =  (EXPR_ARG | EXPR_CMDARG),
    EXPR_END_ANY  =  (EXPR_END | EXPR_ENDARG | EXPR_ENDFN)
};
#define IS_lex_state_for(x, ls)	((x) & (ls))
#define IS_lex_state_all_for(x, ls) (((x) & (ls)) == (ls))
#define IS_lex_state(ls)	IS_lex_state_for(lex_state, (ls))
#define IS_lex_state_all(ls)	IS_lex_state_all_for(lex_state, (ls))

# define SET_LEX_STATE(ls) \
    (lex_state = (yydebug ? trace_lex_state(lex_state, (ls), __LINE__) : \
		  (enum lex_state_e)(ls)))
static enum lex_state_e trace_lex_state(enum lex_state_e from, enum lex_state_e to, int line);

typedef VALUE stack_type;

static void show_bitstack(stack_type, const char *, int);
# define SHOW_BITSTACK(stack, name) (yydebug ? show_bitstack(stack, name, __LINE__) : (void)0)
# define BITSTACK_PUSH(stack, n) (((stack) = ((stack)<<1)|((n)&1)), SHOW_BITSTACK(stack, #stack"(push)"))
# define BITSTACK_POP(stack)	 (((stack) = (stack) >> 1), SHOW_BITSTACK(stack, #stack"(pop)"))
# define BITSTACK_LEXPOP(stack)	 (((stack) = ((stack) >> 1) | ((stack) & 1)), SHOW_BITSTACK(stack, #stack"(lexpop)"))
# define BITSTACK_SET_P(stack)	 (SHOW_BITSTACK(stack, #stack), (stack)&1)
# define BITSTACK_SET(stack, n)	 ((stack)=(n), SHOW_BITSTACK(stack, #stack"(set)"))

#define COND_PUSH(n)	BITSTACK_PUSH(cond_stack, (n))
#define COND_POP()	BITSTACK_POP(cond_stack)
#define COND_LEXPOP()	BITSTACK_LEXPOP(cond_stack)
#define COND_P()	BITSTACK_SET_P(cond_stack)
#define COND_SET(n)	BITSTACK_SET(cond_stack, (n))

#define CMDARG_PUSH(n)	BITSTACK_PUSH(cmdarg_stack, (n))
#define CMDARG_POP()	BITSTACK_POP(cmdarg_stack)
#define CMDARG_LEXPOP()	BITSTACK_LEXPOP(cmdarg_stack)
#define CMDARG_P()	BITSTACK_SET_P(cmdarg_stack)
#define CMDARG_SET(n)	BITSTACK_SET(cmdarg_stack, (n))

struct vtable {
    ID *tbl;
    int pos;
    int capa;
    struct vtable *prev;
};

struct local_vars {
    struct vtable *args;
    struct vtable *vars;
    struct vtable *used;
# if WARN_PAST_SCOPE
    struct vtable *past;
# endif
    struct local_vars *prev;
    stack_type cmdargs;
};

#define DVARS_INHERIT ((void*)1)
#define DVARS_TOPSCOPE NULL
#define DVARS_SPECIAL_P(tbl) (!POINTER_P(tbl))
#define POINTER_P(val) ((VALUE)(val) & ~(VALUE)3)

static int
vtable_size(const struct vtable *tbl)
{
    if (POINTER_P(tbl)) {
        return tbl->pos;
    }
    else {
        return 0;
    }
}

#define VTBL_DEBUG 0

static struct vtable *
vtable_alloc(struct vtable *prev)
{
    struct vtable *tbl = ALLOC(struct vtable);
    tbl->pos = 0;
    tbl->capa = 8;
    tbl->tbl = ALLOC_N(ID, tbl->capa);
    tbl->prev = prev;
    if (VTBL_DEBUG) printf("vtable_alloc: %p\n", (void *)tbl);
    return tbl;
}

static void
vtable_free(struct vtable *tbl)
{
    if (VTBL_DEBUG)printf("vtable_free: %p\n", (void *)tbl);
    if (POINTER_P(tbl)) {
        if (tbl->tbl) {
            xfree(tbl->tbl);
        }
        xfree(tbl);
    }
}

static void
vtable_add(struct vtable *tbl, ID id)
{
    if (!POINTER_P(tbl)) {
        rb_bug("vtable_add: vtable is not allocated (%p)", (void *)tbl);
    }
    if (VTBL_DEBUG) printf("vtable_add: %p, %"PRIsVALUE"\n", (void *)tbl, rb_id2str(id));

    if (tbl->pos == tbl->capa) {
        tbl->capa = tbl->capa * 2;
        REALLOC_N(tbl->tbl, ID, tbl->capa);
    }
    tbl->tbl[tbl->pos++] = id;
}

#ifndef RIPPER
static void
vtable_pop(struct vtable *tbl, int n)
{
    if (tbl->pos < n) rb_bug("vtable_pop: unreachable");
    tbl->pos -= n;
}
#endif

static int
vtable_included(const struct vtable * tbl, ID id)
{
    int i;

    if (POINTER_P(tbl)) {
        for (i = 0; i < tbl->pos; i++) {
            if (tbl->tbl[i] == id) {
                return i+1;
            }
        }
    }
    return 0;
}

typedef struct token_info {
    const char *token;
    int linenum;
    int column;
    int nonspc;
    struct token_info *next;
} token_info;

/*
    Structure of Lexer Buffer:

 lex_pbeg      tokp         lex_p        lex_pend
    |           |              |            |
    |-----------+--------------+------------|
                |<------------>|
                     token
*/
struct parser_params {
    NODE *heap;

    YYSTYPE *lval;

    struct {
	NODE *strterm;
	VALUE (*gets)(struct parser_params*,VALUE);
	VALUE input;
	VALUE lastline;
	VALUE nextline;
	const char *pbeg;
	const char *pcur;
	const char *pend;
	long gets_ptr;
	enum lex_state_e state;
	int paren_nest;
	int lpar_beg;
	int brace_nest;
    } lex;
    stack_type cond_stack;
    stack_type cmdarg_stack;
    int tokidx;
    int toksiz;
    int tokline;
    int heredoc_end;
    int heredoc_indent;
    int heredoc_line_indent;
    char *tokenbuf;
    struct local_vars *lvtbl;
    int line_count;
    int ruby_sourceline;	/* current line no. */
    char *ruby_sourcefile; /* current source file */
    VALUE ruby_sourcefile_string;
    rb_encoding *enc;
    token_info *token_info;
    VALUE compile_option;

    VALUE debug_buffer;

    ID cur_arg;

    unsigned int command_start:1;
    unsigned int eofp: 1;
    unsigned int ruby__end__seen: 1;
    unsigned int yydebug: 1;
    unsigned int has_shebang: 1;
    unsigned int in_defined: 1;
    unsigned int in_main: 1;
    unsigned int in_kwarg: 1;
    unsigned int in_single: 1;
    unsigned int in_def: 1;
    unsigned int token_seen: 1;
    unsigned int token_info_enabled: 1;
# if WARN_PAST_SCOPE
    unsigned int past_scope_enabled: 1;
# endif
    unsigned int error_p: 1;
    unsigned int cr_seen: 1;

#ifndef RIPPER
    /* Ruby core only */

    NODE *eval_tree_begin;
    NODE *eval_tree;
    VALUE error_buffer;
    VALUE debug_lines;
    VALUE coverage;
    const struct rb_block *base_block;
#else
    /* Ripper only */

    const char *tokp;
    VALUE delayed;
    int delayed_line;
    int delayed_col;

    VALUE value;
    VALUE result;
    VALUE parsing_thread;
#endif
};

#ifdef RIPPER
#define intern_cstr(n,l,en) rb_intern3(n,l,en)
#else
#define intern_cstr(n,l,en) rb_intern3(n,l,en)
#endif

#define STR_NEW(p,n) rb_enc_str_new((p),(n),current_enc)
#define STR_NEW0() rb_enc_str_new(0,0,current_enc)
#define STR_NEW2(p) rb_enc_str_new((p),strlen(p),current_enc)
#define STR_NEW3(p,n,e,func) parser_str_new((p),(n),(e),(func),current_enc)
#define TOK_INTERN() intern_cstr(tok(), toklen(), current_enc)

static int parser_yyerror(struct parser_params*, const char*);
#define yyerror(msg) parser_yyerror(parser, (msg))

#define lex_strterm		(parser->lex.strterm)
#define lex_state		(parser->lex.state)
#define cond_stack		(parser->cond_stack)
#define cmdarg_stack		(parser->cmdarg_stack)
#define paren_nest		(parser->lex.paren_nest)
#define lpar_beg		(parser->lex.lpar_beg)
#define brace_nest		(parser->lex.brace_nest)
#define in_single		(parser->in_single)
#define in_def			(parser->in_def)
#define in_main 		(parser->in_main)
#define in_defined		(parser->in_defined)
#define tokenbuf		(parser->tokenbuf)
#define tokidx			(parser->tokidx)
#define toksiz			(parser->toksiz)
#define tokline 		(parser->tokline)
#define lex_input		(parser->lex.input)
#define lex_lastline		(parser->lex.lastline)
#define lex_nextline		(parser->lex.nextline)
#define lex_pbeg		(parser->lex.pbeg)
#define lex_p			(parser->lex.pcur)
#define lex_pend		(parser->lex.pend)
#define heredoc_end		(parser->heredoc_end)
#define heredoc_indent		(parser->heredoc_indent)
#define heredoc_line_indent	(parser->heredoc_line_indent)
#define command_start		(parser->command_start)
#define lex_gets_ptr		(parser->lex.gets_ptr)
#define lex_gets		(parser->lex.gets)
#define lvtbl			(parser->lvtbl)
#define ruby__end__seen 	(parser->ruby__end__seen)
#define ruby_sourceline 	(parser->ruby_sourceline)
#define ruby_sourcefile 	(parser->ruby_sourcefile)
#define ruby_sourcefile_string	(parser->ruby_sourcefile_string)
#define current_enc		(parser->enc)
#define current_arg		(parser->cur_arg)
#define yydebug 		(parser->yydebug)
#ifdef RIPPER
#define compile_for_eval	(0)
#else
#define compile_for_eval	(parser->base_block != 0 && !in_main)
#define ruby_eval_tree		(parser->eval_tree)
#define ruby_eval_tree_begin	(parser->eval_tree_begin)
#define ruby_debug_lines	(parser->debug_lines)
#define ruby_coverage		(parser->coverage)
#endif

#define CALL_Q_P(q) ((q) == tANDDOT)
#define NODE_CALL_Q(q) (CALL_Q_P(q) ? NODE_QCALL : NODE_CALL)
#define NEW_QCALL(q,r,m,a) NEW_NODE(NODE_CALL_Q(q),r,m,a)

#define lambda_beginning_p() (lpar_beg && lpar_beg == paren_nest)

static int yylex(YYSTYPE*, struct parser_params*);

#ifndef RIPPER
static inline void
set_line_body(NODE *body, int line)
{
    if (!body) return;
    switch (nd_type(body)) {
      case NODE_RESCUE:
      case NODE_ENSURE:
	nd_set_line(body, line);
    }
}

#define yyparse ruby_yyparse

static NODE* node_newnode(struct parser_params *, enum node_type, VALUE, VALUE, VALUE);
#define rb_node_newnode(type, a1, a2, a3) node_newnode(parser, (type), (a1), (a2), (a3))

static NODE *cond_gen(struct parser_params*,NODE*,int);
#define cond(node) cond_gen(parser, (node), FALSE)
#define method_cond(node) cond_gen(parser, (node), TRUE)
static NODE *new_if_gen(struct parser_params*,NODE*,NODE*,NODE*);
#define new_if(cc,left,right) new_if_gen(parser, (cc), (left), (right))
#define new_unless(cc,left,right) new_if_gen(parser, (cc), (right), (left))
static NODE *logop_gen(struct parser_params*,enum node_type,NODE*,NODE*);
#define logop(type,node1,node2) logop_gen(parser, (type), (node1), (node2))

static NODE *newline_node(NODE*);
static void fixpos(NODE*,NODE*);

static int value_expr_gen(struct parser_params*,NODE*);
static void void_expr_gen(struct parser_params*,NODE*);
static NODE *remove_begin(NODE*);
static NODE *remove_begin_all(NODE*);
#define value_expr(node) value_expr_gen(parser, (node) = remove_begin(node))
#define void_expr0(node) void_expr_gen(parser, (node))
#define void_expr(node) void_expr0((node) = remove_begin(node))
static void void_stmts_gen(struct parser_params*,NODE*);
#define void_stmts(node) void_stmts_gen(parser, (node))
static void reduce_nodes_gen(struct parser_params*,NODE**);
#define reduce_nodes(n) reduce_nodes_gen(parser,(n))
static void block_dup_check_gen(struct parser_params*,NODE*,NODE*);
#define block_dup_check(n1,n2) block_dup_check_gen(parser,(n1),(n2))

static NODE *block_append_gen(struct parser_params*,NODE*,NODE*);
#define block_append(h,t) block_append_gen(parser,(h),(t))
static NODE *list_append_gen(struct parser_params*,NODE*,NODE*);
#define list_append(l,i) list_append_gen(parser,(l),(i))
static NODE *list_concat(NODE*,NODE*);
static NODE *arg_append_gen(struct parser_params*,NODE*,NODE*);
#define arg_append(h,t) arg_append_gen(parser,(h),(t))
static NODE *arg_concat_gen(struct parser_params*,NODE*,NODE*);
#define arg_concat(h,t) arg_concat_gen(parser,(h),(t))
static NODE *literal_concat_gen(struct parser_params*,NODE*,NODE*);
#define literal_concat(h,t) literal_concat_gen(parser,(h),(t))
static int literal_concat0(struct parser_params *, VALUE, VALUE);
static NODE *new_evstr_gen(struct parser_params*,NODE*);
#define new_evstr(n) new_evstr_gen(parser,(n))
static NODE *evstr2dstr_gen(struct parser_params*,NODE*);
#define evstr2dstr(n) evstr2dstr_gen(parser,(n))
static NODE *splat_array(NODE*);

static NODE *call_bin_op_gen(struct parser_params*,NODE*,ID,NODE*);
#define call_bin_op(recv,id,arg1) call_bin_op_gen(parser, (recv),(id),(arg1))
static NODE *call_uni_op_gen(struct parser_params*,NODE*,ID);
#define call_uni_op(recv,id) call_uni_op_gen(parser, (recv),(id))

static NODE *new_args_gen(struct parser_params*,NODE*,NODE*,ID,NODE*,NODE*);
#define new_args(f,o,r,p,t) new_args_gen(parser, (f),(o),(r),(p),(t))
static NODE *new_args_tail_gen(struct parser_params*,NODE*,ID,ID);
#define new_args_tail(k,kr,b) new_args_tail_gen(parser, (k),(kr),(b))
#define new_kw_arg(k) ((k) ? NEW_KW_ARG(0, (k)) : 0)

static VALUE negate_lit(VALUE);
static NODE *ret_args_gen(struct parser_params*,NODE*);
#define ret_args(node) ret_args_gen(parser, (node))
static NODE *arg_blk_pass(NODE*,NODE*);
static NODE *new_yield_gen(struct parser_params*,NODE*);
#define new_yield(node) new_yield_gen(parser, (node))
static NODE *dsym_node_gen(struct parser_params*,NODE*);
#define dsym_node(node) dsym_node_gen(parser, (node))

static NODE *gettable_gen(struct parser_params*,ID);
#define gettable(id) gettable_gen(parser,(id))
static NODE *assignable_gen(struct parser_params*,ID,NODE*);
#define assignable(id,node) assignable_gen(parser, (id), (node))

static NODE *aryset_gen(struct parser_params*,NODE*,NODE*);
#define aryset(node1,node2) aryset_gen(parser, (node1), (node2))
static NODE *attrset_gen(struct parser_params*,NODE*,ID,ID);
#define attrset(node,q,id) attrset_gen(parser, (node), (q), (id))

static void rb_backref_error_gen(struct parser_params*,NODE*);
#define rb_backref_error(n) rb_backref_error_gen(parser,(n))
static NODE *node_assign_gen(struct parser_params*,NODE*,NODE*);
#define node_assign(node1, node2) node_assign_gen(parser, (node1), (node2))

static NODE *new_op_assign_gen(struct parser_params *parser, NODE *lhs, ID op, NODE *rhs);
static NODE *new_attr_op_assign_gen(struct parser_params *parser, NODE *lhs, ID atype, ID attr, ID op, NODE *rhs);
#define new_attr_op_assign(lhs, type, attr, op, rhs) new_attr_op_assign_gen(parser, (lhs), (type), (attr), (op), (rhs))
static NODE *new_const_op_assign_gen(struct parser_params *parser, NODE *lhs, ID op, NODE *rhs);
#define new_const_op_assign(lhs, op, rhs) new_const_op_assign_gen(parser, (lhs), (op), (rhs))

#define const_path_field(w, n) NEW_COLON2(w, n)
#define top_const_field(n) NEW_COLON3(n)
static NODE *const_decl_gen(struct parser_params *parser, NODE* path);
#define const_decl(path) const_decl_gen(parser, path)

#define var_field(n) (n)
#define backref_assign_error(n, a) (rb_backref_error(n), NEW_BEGIN(0))

static NODE *kwd_append(NODE*, NODE*);

static NODE *new_hash_gen(struct parser_params *parser, NODE *hash);
#define new_hash(hash) new_hash_gen(parser, (hash))

#define new_defined(expr) NEW_DEFINED(remove_begin_all(expr))

static NODE *new_regexp_gen(struct parser_params *, NODE *, int);
#define new_regexp(node, opt) new_regexp_gen(parser, node, opt)

static NODE *new_xstring_gen(struct parser_params *, NODE *);
#define new_xstring(node) new_xstring_gen(parser, node)
#define new_string1(str) (str)

#define new_brace_body(param, stmt) NEW_ITER(param, stmt)
#define new_do_body(param, stmt) NEW_ITER(param, stmt)

static NODE *match_op_gen(struct parser_params*,NODE*,NODE*);
#define match_op(node1,node2) match_op_gen(parser, (node1), (node2))

static ID  *local_tbl_gen(struct parser_params*);
#define local_tbl() local_tbl_gen(parser)

static VALUE reg_compile_gen(struct parser_params*, VALUE, int);
#define reg_compile(str,options) reg_compile_gen(parser, (str), (options))
static void reg_fragment_setenc_gen(struct parser_params*, VALUE, int);
#define reg_fragment_setenc(str,options) reg_fragment_setenc_gen(parser, (str), (options))
static int reg_fragment_check_gen(struct parser_params*, VALUE, int);
#define reg_fragment_check(str,options) reg_fragment_check_gen(parser, (str), (options))
static NODE *reg_named_capture_assign_gen(struct parser_params* parser, VALUE regexp);
#define reg_named_capture_assign(regexp) reg_named_capture_assign_gen(parser,(regexp))

static NODE *parser_heredoc_dedent(struct parser_params*,NODE*);
# define heredoc_dedent(str) parser_heredoc_dedent(parser, (str))

#define get_id(id) (id)
#define get_value(val) (val)
#else  /* RIPPER */
#define NODE_RIPPER NODE_CDECL

static inline VALUE
ripper_new_yylval(ID a, VALUE b, VALUE c)
{
    return (VALUE)NEW_CDECL(a, b, c);
}

static inline int
ripper_is_node_yylval(VALUE n)
{
    return RB_TYPE_P(n, T_NODE) && nd_type(RNODE(n)) == NODE_RIPPER;
}

#define value_expr(node) ((void)(node))
#define remove_begin(node) (node)
#define rb_dvar_defined(id, base) 0
#define rb_local_defined(id, base) 0
static ID ripper_get_id(VALUE);
#define get_id(id) ripper_get_id(id)
static VALUE ripper_get_value(VALUE);
#define get_value(val) ripper_get_value(val)
static VALUE assignable_gen(struct parser_params*,VALUE);
#define assignable(lhs,node) assignable_gen(parser, (lhs))
static int id_is_var_gen(struct parser_params *parser, ID id);
#define id_is_var(id) id_is_var_gen(parser, (id))

#define node_assign(node1, node2) dispatch2(assign, (node1), (node2))

static VALUE new_op_assign_gen(struct parser_params *parser, VALUE lhs, VALUE op, VALUE rhs);
static VALUE new_attr_op_assign_gen(struct parser_params *parser, VALUE lhs, VALUE type, VALUE attr, VALUE op, VALUE rhs);
#define new_attr_op_assign(lhs, type, attr, op, rhs) new_attr_op_assign_gen(parser, (lhs), (type), (attr), (op), (rhs))
#define new_const_op_assign(lhs, op, rhs) new_op_assign(lhs, op, rhs)

static VALUE new_regexp_gen(struct parser_params *, VALUE, VALUE);
#define new_regexp(node, opt) new_regexp_gen(parser, node, opt)

static VALUE new_xstring_gen(struct parser_params *, VALUE);
#define new_xstring(str) new_xstring_gen(parser, str)
#define new_string1(str) dispatch1(string_literal, str)

#define new_brace_body(param, stmt) dispatch2(brace_block, escape_Qundef(param), stmt)
#define new_do_body(param, stmt) dispatch2(do_block, escape_Qundef(param), stmt)

#define const_path_field(w, n) dispatch2(const_path_field, (w), (n))
#define top_const_field(n) dispatch1(top_const_field, (n))
static VALUE const_decl_gen(struct parser_params *parser, VALUE path);
#define const_decl(path) const_decl_gen(parser, path)

#define var_field(n) dispatch1(var_field, (n))
static VALUE assign_error_gen(struct parser_params *parser, VALUE a);
#define assign_error(a) assign_error_gen(parser, (a))
#define backref_assign_error(n, a) assign_error(a)

static VALUE parser_reg_compile(struct parser_params*, VALUE, int, VALUE *);

#endif /* !RIPPER */

#define new_op_assign(lhs, op, rhs) new_op_assign_gen(parser, (lhs), (op), (rhs))

RUBY_FUNC_EXPORTED VALUE rb_parser_reg_compile(struct parser_params* parser, VALUE str, int options);
RUBY_FUNC_EXPORTED int rb_reg_fragment_setenc(struct parser_params*, VALUE, int);


static ID formal_argument_gen(struct parser_params*, ID);
#define formal_argument(id) formal_argument_gen(parser, (id))
static ID shadowing_lvar_gen(struct parser_params*,ID);
#define shadowing_lvar(name) shadowing_lvar_gen(parser, (name))
static void new_bv_gen(struct parser_params*,ID);
#define new_bv(id) new_bv_gen(parser, (id))

static void local_push_gen(struct parser_params*,int);
#define local_push(top) local_push_gen(parser,(top))
static void local_pop_gen(struct parser_params*);
#define local_pop() local_pop_gen(parser)
static void local_var_gen(struct parser_params*, ID);
#define local_var(id) local_var_gen(parser, (id))
static void arg_var_gen(struct parser_params*, ID);
#define arg_var(id) arg_var_gen(parser, (id))
static int  local_id_gen(struct parser_params*, ID);
#define local_id(id) local_id_gen(parser, (id))
static ID   internal_id_gen(struct parser_params*);
#define internal_id() internal_id_gen(parser)

static const struct vtable *dyna_push_gen(struct parser_params *);
#define dyna_push() dyna_push_gen(parser)
static void dyna_pop_gen(struct parser_params*, const struct vtable *);
#define dyna_pop(node) dyna_pop_gen(parser, (node))
static int dyna_in_block_gen(struct parser_params*);
#define dyna_in_block() dyna_in_block_gen(parser)
#define dyna_var(id) local_var(id)
static int dvar_defined_gen(struct parser_params*,ID,int);
#define dvar_defined(id) dvar_defined_gen(parser, (id), 0)
#define dvar_defined_get(id) dvar_defined_gen(parser, (id), 1)
static int dvar_curr_gen(struct parser_params*,ID);
#define dvar_curr(id) dvar_curr_gen(parser, (id))

static int lvar_defined_gen(struct parser_params*, ID);
#define lvar_defined(id) lvar_defined_gen(parser, (id))

#define RE_OPTION_ONCE (1<<16)
#define RE_OPTION_ENCODING_SHIFT 8
#define RE_OPTION_ENCODING(e) (((e)&0xff)<<RE_OPTION_ENCODING_SHIFT)
#define RE_OPTION_ENCODING_IDX(o) (((o)>>RE_OPTION_ENCODING_SHIFT)&0xff)
#define RE_OPTION_ENCODING_NONE(o) ((o)&RE_OPTION_ARG_ENCODING_NONE)
#define RE_OPTION_MASK  0xff
#define RE_OPTION_ARG_ENCODING_NONE 32

#define NODE_STRTERM NODE_ZARRAY	/* nothing to gc */
#define NODE_HEREDOC NODE_ARRAY 	/* 1, 3 to gc */
#define SIGN_EXTEND(x,n) (((1<<(n)-1)^((x)&~(~0<<(n))))-(1<<(n)-1))
#define nd_func u1.id
#if SIZEOF_SHORT == 2
#define nd_term(node) ((signed short)(node)->u2.id)
#else
#define nd_term(node) SIGN_EXTEND((node)->u2.id, CHAR_BIT*2)
#endif
#define nd_paren(node) (char)((node)->u2.id >> CHAR_BIT*2)
#define nd_nest u3.cnt

/****** Ripper *******/

#ifdef RIPPER
#define RIPPER_VERSION "0.1.0"

static inline VALUE intern_sym(const char *name);

#include "eventids1.c"
#include "eventids2.c"

static VALUE ripper_dispatch0(struct parser_params*,ID);
static VALUE ripper_dispatch1(struct parser_params*,ID,VALUE);
static VALUE ripper_dispatch2(struct parser_params*,ID,VALUE,VALUE);
static VALUE ripper_dispatch3(struct parser_params*,ID,VALUE,VALUE,VALUE);
static VALUE ripper_dispatch4(struct parser_params*,ID,VALUE,VALUE,VALUE,VALUE);
static VALUE ripper_dispatch5(struct parser_params*,ID,VALUE,VALUE,VALUE,VALUE,VALUE);
static VALUE ripper_dispatch7(struct parser_params*,ID,VALUE,VALUE,VALUE,VALUE,VALUE,VALUE,VALUE);
static void ripper_error_gen(struct parser_params *parser);
#define ripper_error() ripper_error_gen(parser)

#define dispatch0(n)            ripper_dispatch0(parser, TOKEN_PASTE(ripper_id_, n))
#define dispatch1(n,a)          ripper_dispatch1(parser, TOKEN_PASTE(ripper_id_, n), (a))
#define dispatch2(n,a,b)        ripper_dispatch2(parser, TOKEN_PASTE(ripper_id_, n), (a), (b))
#define dispatch3(n,a,b,c)      ripper_dispatch3(parser, TOKEN_PASTE(ripper_id_, n), (a), (b), (c))
#define dispatch4(n,a,b,c,d)    ripper_dispatch4(parser, TOKEN_PASTE(ripper_id_, n), (a), (b), (c), (d))
#define dispatch5(n,a,b,c,d,e)  ripper_dispatch5(parser, TOKEN_PASTE(ripper_id_, n), (a), (b), (c), (d), (e))
#define dispatch7(n,a,b,c,d,e,f,g) ripper_dispatch7(parser, TOKEN_PASTE(ripper_id_, n), (a), (b), (c), (d), (e), (f), (g))

#define yyparse ripper_yyparse

#define ripper_intern(s) ID2SYM(rb_intern(s))
static VALUE ripper_id2sym(ID);
#ifdef __GNUC__
#define ripper_id2sym(id) (rb_ispunct((int)(id)) ? \
			   ID2SYM(id) : ripper_id2sym(id))
#endif

#define arg_new() dispatch0(args_new)
#define arg_add(l,a) dispatch2(args_add, (l), (a))
#define arg_add_star(l,a) dispatch2(args_add_star, (l), (a))
#define arg_add_block(l,b) dispatch2(args_add_block, (l), (b))
#define arg_add_optblock(l,b) ((b)==Qundef? (l) : dispatch2(args_add_block, (l), (b)))
#define bare_assoc(v) dispatch1(bare_assoc_hash, (v))
#define arg_add_assocs(l,b) arg_add((l), bare_assoc(b))

#define args2mrhs(a) dispatch1(mrhs_new_from_args, (a))
#define mrhs_new() dispatch0(mrhs_new)
#define mrhs_add(l,a) dispatch2(mrhs_add, (l), (a))
#define mrhs_add_star(l,a) dispatch2(mrhs_add_star, (l), (a))

#define mlhs_new() dispatch0(mlhs_new)
#define mlhs_add(l,a) dispatch2(mlhs_add, (l), (a))
#define mlhs_add_star(l,a) dispatch2(mlhs_add_star, (l), (a))

#define params_new(pars, opts, rest, pars2, kws, kwrest, blk) \
        dispatch7(params, (pars), (opts), (rest), (pars2), (kws), (kwrest), (blk))

#define blockvar_new(p,v) dispatch2(block_var, (p), (v))
#define blockvar_add_star(l,a) dispatch2(block_var_add_star, (l), (a))
#define blockvar_add_block(l,a) dispatch2(block_var_add_block, (l), (a))

#define method_optarg(m,a) ((a)==Qundef ? (m) : dispatch2(method_add_arg,(m),(a)))
#define method_arg(m,a) dispatch2(method_add_arg,(m),(a))
#define method_add_block(m,b) dispatch2(method_add_block, (m), (b))

#define escape_Qundef(x) ((x)==Qundef ? Qnil : (x))

static inline VALUE
new_args_gen(struct parser_params *parser, VALUE f, VALUE o, VALUE r, VALUE p, VALUE tail)
{
    NODE *t = (NODE *)tail;
    VALUE k = t->u1.value, kr = t->u2.value, b = t->u3.value;
    return params_new(f, o, r, p, k, kr, escape_Qundef(b));
}
#define new_args(f,o,r,p,t) new_args_gen(parser, (f),(o),(r),(p),(t))

static inline VALUE
new_args_tail_gen(struct parser_params *parser, VALUE k, VALUE kr, VALUE b)
{
    return (VALUE)MEMO_NEW(k, kr, b);
}
#define new_args_tail(k,kr,b) new_args_tail_gen(parser, (k),(kr),(b))

#define new_defined(expr) dispatch1(defined, (expr))

static VALUE parser_heredoc_dedent(struct parser_params*,VALUE);
# define heredoc_dedent(str) parser_heredoc_dedent(parser, (str))

#define FIXME 0

#else
#define ripper_id2sym(id) id
#endif /* RIPPER */

#ifndef RIPPER
# define Qnone 0
# define ifndef_ripper(x) (x)
#else
# define Qnone Qnil
# define ifndef_ripper(x)
#endif

# define rb_warn0(fmt)         WARN_CALL(WARN_ARGS(fmt, 1))
# define rb_warn1(fmt,a)       WARN_CALL(WARN_ARGS(fmt, 2), (a))
# define rb_warn2(fmt,a,b)     WARN_CALL(WARN_ARGS(fmt, 3), (a), (b))
# define rb_warn3(fmt,a,b,c)   WARN_CALL(WARN_ARGS(fmt, 4), (a), (b), (c))
# define rb_warn4(fmt,a,b,c,d) WARN_CALL(WARN_ARGS(fmt, 5), (a), (b), (c), (d))
# define rb_warning0(fmt)         WARNING_CALL(WARNING_ARGS(fmt, 1))
# define rb_warning1(fmt,a)       WARNING_CALL(WARNING_ARGS(fmt, 2), (a))
# define rb_warning2(fmt,a,b)     WARNING_CALL(WARNING_ARGS(fmt, 3), (a), (b))
# define rb_warning3(fmt,a,b,c)   WARNING_CALL(WARNING_ARGS(fmt, 4), (a), (b), (c))
# define rb_warning4(fmt,a,b,c,d) WARNING_CALL(WARNING_ARGS(fmt, 5), (a), (b), (c), (d))
# define rb_warn0L(l,fmt)         WARN_CALL(WARN_ARGS_L(l, fmt, 1))
# define rb_warn1L(l,fmt,a)       WARN_CALL(WARN_ARGS_L(l, fmt, 2), (a))
# define rb_warn2L(l,fmt,a,b)     WARN_CALL(WARN_ARGS_L(l, fmt, 3), (a), (b))
# define rb_warn3L(l,fmt,a,b,c)   WARN_CALL(WARN_ARGS_L(l, fmt, 4), (a), (b), (c))
# define rb_warn4L(l,fmt,a,b,c,d) WARN_CALL(WARN_ARGS_L(l, fmt, 5), (a), (b), (c), (d))
# define rb_warning0L(l,fmt)         WARNING_CALL(WARNING_ARGS_L(l, fmt, 1))
# define rb_warning1L(l,fmt,a)       WARNING_CALL(WARNING_ARGS_L(l, fmt, 2), (a))
# define rb_warning2L(l,fmt,a,b)     WARNING_CALL(WARNING_ARGS_L(l, fmt, 3), (a), (b))
# define rb_warning3L(l,fmt,a,b,c)   WARNING_CALL(WARNING_ARGS_L(l, fmt, 4), (a), (b), (c))
# define rb_warning4L(l,fmt,a,b,c,d) WARNING_CALL(WARNING_ARGS_L(l, fmt, 5), (a), (b), (c), (d))
#ifdef RIPPER
static ID id_warn, id_warning, id_gets;
# define WARN_S_L(s,l) STR_NEW(s,l)
# define WARN_S(s) STR_NEW2(s)
# define WARN_I(i) INT2NUM(i)
# define PRIsWARN "s"
# define WARN_ARGS(fmt,n) parser->value, id_warn, n, rb_usascii_str_new_lit(fmt)
# define WARN_ARGS_L(l,fmt,n) WARN_ARGS(fmt,n)
# define WARN_CALL rb_funcall
# define WARNING_ARGS(fmt,n) parser->value, id_warning, n, rb_usascii_str_new_lit(fmt)
# define WARNING_ARGS_L(l, fmt,n) WARNING_ARGS(fmt,n)
# define WARNING_CALL rb_funcall
static void ripper_compile_error(struct parser_params*, const char *fmt, ...);
# define compile_error ripper_compile_error
# define PARSER_ARG parser,
#else
# define WARN_S_L(s,l) s
# define WARN_S(s) s
# define WARN_I(i) i
# define PRIsWARN PRIsVALUE
# define WARN_ARGS(fmt,n) WARN_ARGS_L(ruby_sourceline,fmt,n)
# define WARN_ARGS_L(l,fmt,n) ruby_sourcefile, (l), (fmt)
# define WARN_CALL rb_compile_warn
# define WARNING_ARGS(fmt,n) WARN_ARGS(fmt,n)
# define WARNING_ARGS_L(l,fmt,n) WARN_ARGS_L(l,fmt,n)
# define WARNING_CALL rb_compile_warning
static void parser_compile_error(struct parser_params*, const char *fmt, ...);
# define compile_error parser_compile_error
# define PARSER_ARG parser,
#endif

/* Older versions of Yacc set YYMAXDEPTH to a very low value by default (150,
   for instance).  This is too low for Ruby to parse some files, such as
   date/format.rb, therefore bump the value up to at least Bison's default. */
#ifdef OLD_YACC
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif
#endif

static void token_info_push_gen(struct parser_params*, const char *token, size_t len);
static void token_info_pop_gen(struct parser_params*, const char *token, size_t len);
#define token_info_push(token) token_info_push_gen(parser, (token), rb_strlen_lit(token))
#define token_info_pop(token) token_info_pop_gen(parser, (token), rb_strlen_lit(token))

#line 889 "parse.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* In a future release of Bison, this section will be replaced
   by #include "y.tab.h".  */
#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
#ifndef yydebug
extern int yydebug;
#endif
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    END_OF_INPUT = 0,
    keyword_class = 258,
    keyword_module = 259,
    keyword_def = 260,
    keyword_undef = 261,
    keyword_begin = 262,
    keyword_rescue = 263,
    keyword_ensure = 264,
    keyword_end = 265,
    keyword_if = 266,
    keyword_unless = 267,
    keyword_then = 268,
    keyword_elsif = 269,
    keyword_else = 270,
    keyword_case = 271,
    keyword_when = 272,
    keyword_while = 273,
    keyword_until = 274,
    keyword_for = 275,
    keyword_break = 276,
    keyword_next = 277,
    keyword_redo = 278,
    keyword_retry = 279,
    keyword_in = 280,
    keyword_do = 281,
    keyword_do_cond = 282,
    keyword_do_block = 283,
    keyword_do_LAMBDA = 284,
    keyword_return = 285,
    keyword_yield = 286,
    keyword_super = 287,
    keyword_self = 288,
    keyword_nil = 289,
    keyword_true = 290,
    keyword_false = 291,
    keyword_and = 292,
    keyword_or = 293,
    keyword_not = 294,
    modifier_if = 295,
    modifier_unless = 296,
    modifier_while = 297,
    modifier_until = 298,
    modifier_rescue = 299,
    keyword_alias = 300,
    keyword_defined = 301,
    keyword_BEGIN = 302,
    keyword_END = 303,
    keyword__LINE__ = 304,
    keyword__FILE__ = 305,
    keyword__ENCODING__ = 306,
    tIDENTIFIER = 307,
    tFID = 308,
    tGVAR = 309,
    tIVAR = 310,
    tCONSTANT = 311,
    tCVAR = 312,
    tLABEL = 313,
    tINTEGER = 314,
    tFLOAT = 315,
    tRATIONAL = 316,
    tIMAGINARY = 317,
    tSTRING_CONTENT = 318,
    tCHAR = 319,
    tNTH_REF = 320,
    tBACK_REF = 321,
    tREGEXP_END = 322,
    tUPLUS = 130,
    tUMINUS = 131,
    tPOW = 132,
    tCMP = 134,
    tEQ = 139,
    tEQQ = 140,
    tNEQ = 141,
    tGEQ = 138,
    tLEQ = 137,
    tANDOP = 148,
    tOROP = 149,
    tMATCH = 142,
    tNMATCH = 143,
    tDOT2 = 128,
    tDOT3 = 129,
    tAREF = 144,
    tASET = 145,
    tLSHFT = 135,
    tRSHFT = 136,
    tANDDOT = 150,
    tCOLON2 = 323,
    tCOLON3 = 324,
    tOP_ASGN = 325,
    tASSOC = 326,
    tLPAREN = 327,
    tLPAREN_ARG = 328,
    tRPAREN = 329,
    tLBRACK = 330,
    tLBRACE = 331,
    tLBRACE_ARG = 332,
    tSTAR = 333,
    tDSTAR = 334,
    tAMPER = 335,
    tLAMBDA = 336,
    tSYMBEG = 337,
    tSTRING_BEG = 338,
    tXSTRING_BEG = 339,
    tREGEXP_BEG = 340,
    tWORDS_BEG = 341,
    tQWORDS_BEG = 342,
    tSYMBOLS_BEG = 343,
    tQSYMBOLS_BEG = 344,
    tSTRING_DBEG = 345,
    tSTRING_DEND = 346,
    tSTRING_DVAR = 347,
    tSTRING_END = 348,
    tLAMBEG = 349,
    tLABEL_END = 350,
    tLOWEST = 351,
    tUMINUS_NUM = 352,
    tLAST_TOKEN = 353
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 839 "parse.y" /* yacc.c:355  */

    VALUE val;
    NODE *node;
    ID id;
    int num;
    const struct vtable *vars;

#line 1057 "parse.c" /* yacc.c:355  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif



int yyparse (struct parser_params *parser);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 1073 "parse.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   11794

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  146
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  217
/* YYNRULES -- Number of rules.  */
#define YYNRULES  642
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  1085

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   353

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     145,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,   143,   131,     2,     2,     2,   129,   124,     2,
     139,   140,   127,   125,   137,   126,   144,   128,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   119,   142,
     121,   117,   120,   118,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   136,     2,   141,   123,     2,   138,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   134,   122,   135,   132,     2,    81,    82,
      68,    69,    70,     2,    71,    85,    86,    76,    75,    72,
      73,    74,    79,    80,    83,    84,     2,     2,    77,    78,
      87,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   130,   133
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,  1005,  1005,  1005,  1036,  1046,  1055,  1063,  1071,  1077,
    1079,  1078,  1099,  1132,  1142,  1151,  1159,  1167,  1173,  1178,
    1177,  1198,  1198,  1206,  1214,  1225,  1236,  1244,  1253,  1262,
    1275,  1288,  1297,  1309,  1310,  1320,  1325,  1334,  1337,  1342,
    1347,  1368,  1373,  1378,  1383,  1388,  1395,  1403,  1412,  1415,
    1416,  1424,  1432,  1440,  1448,  1451,  1463,  1464,  1467,  1468,
    1480,  1479,  1495,  1505,  1514,  1527,  1536,  1548,  1557,  1569,
    1578,  1587,  1595,  1603,  1613,  1614,  1624,  1625,  1635,  1643,
    1651,  1659,  1668,  1676,  1685,  1693,  1702,  1710,  1721,  1722,
    1732,  1740,  1750,  1758,  1768,  1772,  1776,  1784,  1792,  1800,
    1808,  1812,  1816,  1823,  1832,  1841,  1849,  1857,  1865,  1873,
    1877,  1881,  1888,  1897,  1900,  1908,  1916,  1926,  1927,  1928,
    1929,  1934,  1945,  1946,  1949,  1957,  1960,  1968,  1968,  1978,
    1979,  1980,  1981,  1982,  1983,  1984,  1985,  1986,  1987,  1988,
    1989,  1990,  1991,  1992,  1993,  1994,  1995,  1996,  1997,  1998,
    1999,  2000,  2001,  2002,  2003,  2004,  2005,  2006,  2007,  2010,
    2010,  2010,  2011,  2011,  2012,  2012,  2012,  2013,  2013,  2013,
    2013,  2014,  2014,  2014,  2014,  2015,  2015,  2015,  2016,  2016,
    2016,  2016,  2017,  2017,  2017,  2017,  2018,  2018,  2018,  2018,
    2019,  2019,  2019,  2019,  2020,  2020,  2020,  2020,  2021,  2021,
    2024,  2028,  2032,  2058,  2063,  2068,  2073,  2078,  2083,  2088,
    2098,  2108,  2116,  2124,  2132,  2140,  2148,  2156,  2165,  2173,
    2181,  2189,  2197,  2205,  2213,  2221,  2229,  2237,  2245,  2253,
    2261,  2269,  2283,  2291,  2299,  2307,  2315,  2323,  2331,  2339,
    2339,  2348,  2358,  2364,  2376,  2377,  2381,  2389,  2399,  2407,
    2418,  2428,  2429,  2432,  2433,  2434,  2438,  2446,  2456,  2465,
    2473,  2483,  2492,  2501,  2501,  2513,  2523,  2527,  2533,  2541,
    2549,  2563,  2579,  2580,  2583,  2597,  2612,  2622,  2623,  2624,
    2625,  2626,  2627,  2628,  2629,  2630,  2631,  2632,  2641,  2640,
    2666,  2666,  2675,  2679,  2674,  2688,  2696,  2704,  2712,  2725,
    2733,  2741,  2749,  2757,  2765,  2765,  2774,  2782,  2790,  2800,
    2801,  2811,  2815,  2827,  2839,  2839,  2839,  2850,  2850,  2850,
    2861,  2872,  2881,  2883,  2880,  2929,  2928,  2951,  2950,  2973,
    2972,  2995,  3000,  2994,  3021,  3022,  3021,  3047,  3055,  3063,
    3071,  3081,  3093,  3099,  3105,  3111,  3117,  3123,  3129,  3135,
    3141,  3147,  3157,  3163,  3168,  3169,  3176,  3181,  3184,  3185,
    3198,  3199,  3209,  3210,  3213,  3221,  3231,  3239,  3249,  3257,
    3266,  3275,  3283,  3291,  3300,  3312,  3320,  3331,  3335,  3339,
    3343,  3349,  3354,  3359,  3363,  3367,  3371,  3375,  3379,  3387,
    3391,  3395,  3399,  3403,  3407,  3411,  3415,  3419,  3425,  3426,
    3432,  3442,  3451,  3463,  3467,  3477,  3484,  3493,  3501,  3507,
    3510,  3515,  3518,  3507,  3537,  3545,  3551,  3556,  3563,  3562,
    3577,  3593,  3602,  3614,  3628,  3638,  3637,  3654,  3653,  3669,
    3678,  3677,  3694,  3693,  3710,  3718,  3726,  3741,  3740,  3754,
    3753,  3768,  3769,  3768,  3778,  3779,  3778,  3788,  3800,  3801,
    3804,  3823,  3826,  3834,  3842,  3845,  3849,  3852,  3860,  3863,
    3864,  3872,  3875,  3892,  3893,  3894,  3904,  3910,  3916,  3922,
    3931,  3942,  3949,  3959,  3967,  3977,  3986,  3997,  4004,  4022,
    4031,  4041,  4050,  4061,  4068,  4079,  4086,  4101,  4108,  4119,
    4126,  4137,  4144,  4185,  4187,  4186,  4202,  4208,  4212,  4216,
    4220,  4201,  4242,  4250,  4258,  4266,  4269,  4280,  4281,  4282,
    4283,  4286,  4297,  4298,  4309,  4310,  4311,  4312,  4315,  4316,
    4317,  4318,  4319,  4322,  4323,  4324,  4325,  4326,  4327,  4328,
    4331,  4344,  4354,  4362,  4372,  4373,  4377,  4376,  4386,  4395,
    4405,  4405,  4419,  4423,  4427,  4431,  4437,  4442,  4447,  4451,
    4455,  4459,  4463,  4467,  4471,  4475,  4479,  4483,  4487,  4491,
    4495,  4499,  4504,  4510,  4520,  4530,  4540,  4552,  4553,  4560,
    4569,  4578,  4597,  4604,  4618,  4627,  4637,  4649,  4658,  4669,
    4677,  4688,  4696,  4706,  4707,  4710,  4715,  4722,  4734,  4746,
    4754,  4770,  4778,  4794,  4795,  4798,  4811,  4822,  4823,  4826,
    4843,  4847,  4857,  4867,  4867,  4896,  4897,  4907,  4914,  4938,
    4950,  4958,  4966,  4980,  4981,  4982,  4985,  4986,  4987,  4988,
    4991,  4992,  4993,  4996,  5001,  5008,  5016,  5026,  5027,  5037,
    5038,  5041,  5042,  5045,  5048,  5051,  5052,  5053,  5056,  5057,
    5060,  5061,  5065
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end-of-input\"", "error", "$undefined", "keyword_class",
  "keyword_module", "keyword_def", "keyword_undef", "keyword_begin",
  "keyword_rescue", "keyword_ensure", "keyword_end", "keyword_if",
  "keyword_unless", "keyword_then", "keyword_elsif", "keyword_else",
  "keyword_case", "keyword_when", "keyword_while", "keyword_until",
  "keyword_for", "keyword_break", "keyword_next", "keyword_redo",
  "keyword_retry", "keyword_in", "keyword_do", "keyword_do_cond",
  "keyword_do_block", "keyword_do_LAMBDA", "keyword_return",
  "keyword_yield", "keyword_super", "keyword_self", "keyword_nil",
  "keyword_true", "keyword_false", "keyword_and", "keyword_or",
  "keyword_not", "modifier_if", "modifier_unless", "modifier_while",
  "modifier_until", "modifier_rescue", "keyword_alias", "keyword_defined",
  "keyword_BEGIN", "keyword_END", "keyword__LINE__", "keyword__FILE__",
  "keyword__ENCODING__", "tIDENTIFIER", "tFID", "tGVAR", "tIVAR",
  "tCONSTANT", "tCVAR", "tLABEL", "tINTEGER", "tFLOAT", "tRATIONAL",
  "tIMAGINARY", "tSTRING_CONTENT", "tCHAR", "tNTH_REF", "tBACK_REF",
  "tREGEXP_END", "\"unary+\"", "\"unary-\"", "\"**\"", "\"<=>\"", "\"==\"",
  "\"===\"", "\"!=\"", "\">=\"", "\"<=\"", "\"&&\"", "\"||\"", "\"=~\"",
  "\"!~\"", "\"..\"", "\"...\"", "\"[]\"", "\"[]=\"", "\"<<\"", "\">>\"",
  "\"&.\"", "\"::\"", "\":: at EXPR_BEG\"", "tOP_ASGN", "\"=>\"", "\"(\"",
  "\"( arg\"", "\")\"", "\"[\"", "\"{\"", "\"{ arg\"", "\"*\"",
  "\"**arg\"", "\"&\"", "\"->\"", "tSYMBEG", "tSTRING_BEG", "tXSTRING_BEG",
  "tREGEXP_BEG", "tWORDS_BEG", "tQWORDS_BEG", "tSYMBOLS_BEG",
  "tQSYMBOLS_BEG", "tSTRING_DBEG", "tSTRING_DEND", "tSTRING_DVAR",
  "tSTRING_END", "tLAMBEG", "tLABEL_END", "tLOWEST", "'='", "'?'", "':'",
  "'>'", "'<'", "'|'", "'^'", "'&'", "'+'", "'-'", "'*'", "'/'", "'%'",
  "tUMINUS_NUM", "'!'", "'~'", "tLAST_TOKEN", "'{'", "'}'", "'['", "','",
  "'`'", "'('", "')'", "']'", "';'", "' '", "'.'", "'\\n'", "$accept",
  "program", "$@1", "top_compstmt", "top_stmts", "top_stmt", "$@2",
  "bodystmt", "compstmt", "stmts", "stmt_or_begin", "$@3", "stmt", "$@4",
  "command_asgn", "command_rhs", "expr", "expr_value", "command_call",
  "block_command", "cmd_brace_block", "@5", "fcall", "command", "mlhs",
  "mlhs_inner", "mlhs_basic", "mlhs_item", "mlhs_head", "mlhs_post",
  "mlhs_node", "lhs", "cname", "cpath", "fname", "fsym", "fitem",
  "undef_list", "$@6", "op", "reswords", "arg", "$@7", "arg_value",
  "aref_args", "arg_rhs", "paren_args", "opt_paren_args", "opt_call_args",
  "call_args", "command_args", "@8", "block_arg", "opt_block_arg", "args",
  "mrhs_arg", "mrhs", "primary", "@9", "$@10", "$@11", "$@12", "$@13",
  "$@14", "$@15", "$@16", "$@17", "$@18", "$@19", "@20", "@21", "@22",
  "@23", "@24", "@25", "@26", "primary_value", "k_begin", "k_if",
  "k_unless", "k_while", "k_until", "k_case", "k_for", "k_class",
  "k_module", "k_def", "k_end", "then", "do", "if_tail", "opt_else",
  "for_var", "f_marg", "f_marg_list", "f_margs", "block_args_tail",
  "opt_block_args_tail", "block_param", "opt_block_param",
  "block_param_def", "opt_bv_decl", "bv_decls", "bvar", "lambda", "@27",
  "@28", "@29", "@30", "f_larglist", "lambda_body", "do_block", "@31",
  "block_call", "method_call", "@32", "@33", "@34", "@35", "brace_block",
  "@36", "@37", "brace_body", "@38", "@39", "do_body", "@40", "@41",
  "case_body", "cases", "opt_rescue", "exc_list", "exc_var", "opt_ensure",
  "literal", "strings", "string", "string1", "xstring", "regexp", "words",
  "word_list", "word", "symbols", "symbol_list", "qwords", "qsymbols",
  "qword_list", "qsym_list", "string_contents", "xstring_contents",
  "regexp_contents", "string_content", "@42", "@43", "@44", "@45", "@46",
  "@47", "string_dvar", "symbol", "sym", "dsym", "numeric",
  "simple_numeric", "user_variable", "keyword_variable", "var_ref",
  "var_lhs", "backref", "superclass", "$@48", "f_arglist", "@49",
  "args_tail", "opt_args_tail", "f_args", "f_bad_arg", "f_norm_arg",
  "f_arg_asgn", "f_arg_item", "f_arg", "f_label", "f_kw", "f_block_kw",
  "f_block_kwarg", "f_kwarg", "kwrest_mark", "f_kwrest", "f_opt",
  "f_block_opt", "f_block_optarg", "f_optarg", "restarg_mark",
  "f_rest_arg", "blkarg_mark", "f_block_arg", "opt_f_block_arg",
  "singleton", "$@50", "assoc_list", "assocs", "assoc", "operation",
  "operation2", "operation3", "dot_or_colon", "call_op", "call_op2",
  "opt_terms", "opt_nl", "rparen", "rbracket", "trailer", "term", "terms",
  "none", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   130,   131,
     132,   134,   139,   140,   141,   138,   137,   148,   149,   142,
     143,   128,   129,   144,   145,   135,   136,   150,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,    61,    63,    58,
      62,    60,   124,    94,    38,    43,    45,    42,    47,    37,
     352,    33,   126,   353,   123,   125,    91,    44,    96,    40,
      41,    93,    59,    32,    46,    10
};
# endif

#define YYPACT_NINF -876

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-876)))

#define YYTABLE_NINF -643

#define yytable_value_is_error(Yytable_value) \
  (!!((Yytable_value) == (-643)))

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -876,    96,  3133,  -876,  8152,  -876,  -876,  -876,  7646,  -876,
    -876,  -876,  -876,  -876,  -876,  -876,  8269,  8269,  -876,  -876,
    8269,  4485,  4071,  -876,  -876,  -876,  -876,   235,  7510,    -1,
    -876,    81,  -876,  -876,  -876,  3381,  4209,  -876,  -876,  3519,
    -876,  -876,  -876,  -876,  -876,  -876,  -876,  -876,  9673,  9673,
     172,  5845,    -3,  8620,  8971,  7912,  -876,  7374,  -876,  -876,
    -876,   234,   278,   284,   295,  1381,  9790,  9673,  -876,   333,
    -876,  1276,  -876,   125,  -876,  -876,   141,   406,   329,  -876,
     318, 10024,  -876,   363,  2119,    45,   338,  -876,  9907,  9907,
    -876,  -876,  6746, 10137, 10250, 10363,  7237,    12,    67,  -876,
    -876,   380,  -876,  -876,  -876,  -876,  -876,  -876,  -876,  -876,
    -876,  -876,  -876,   225,   574,  -876,   395,   598,  -876,  -876,
    -876,  -876,  -876,  -876,  -876,  -876,  -876,  -876,  -876,  -876,
    -876,  -876,  -876,  -876,  -876,  -876,  -876,  -876,  -876,  -876,
    -876,  -876,  -876,  -876,  -876,  -876,  -876,  -876,  -876,  -876,
    -876,  -876,  -876,  -876,  -876,  -876,  -876,  -876,  -876,  -876,
    -876,  -876,  -876,  -876,  -876,  -876,  -876,  -876,  -876,  -876,
    -876,  -876,  -876,  -876,  -876,  -876,  -876,  -876,  -876,  -876,
    -876,  -876,  -876,  -876,  -876,  -876,  -876,  -876,  -876,  -876,
    -876,  -876,  -876,  -876,  -876,  -876,  -876,  -876,   387,  -876,
    -876,  -876,  -876,   388,  9673,   473,  5977,  9673,  9673,  9673,
    -876,  9673,  -876,   454,  2119,   451,  -876,  -876,   442,   421,
      51,   210,   503,   268,   472,  -876,  -876,  -876,  6629,  -876,
    8269,  8269,  -876,  -876,  6863,  -876,  9907,   523,  -876,   488,
     507,  6109,  -876,  -876,  -876,   524,   533,   141,  -876,   606,
     572,   629,  8386,  -876,  5845,   504,   333,  -876,  1276,    -1,
     550,  -876,    -1,  8386,   534,   -11,    76,  -876,   451,   548,
      76,  -876,    -1,   621,  1381, 10476,   549,  -876,   702,   883,
     974,   976,  -876,  -876,  -876,  -876,  -876,  -876,   505,  -876,
     674,   712,   492,   582,  1109,   590,    40,   593,  1152,   601,
      47,   626,  -876,  -876,  -876,  -876,  -876,  -876,  -876,  6980,
    9907,  9907,  9907,  9907,  8386,  9907,  9907,  -876,  -876,  -876,
     624,  -876,  -876,  -876,  9088,  -876,  5845,  8032,   581,  9088,
    9673,  9673,  9673,  9673,  9673,  9673,  9673,  9673,  9673,  9673,
    9673,  9673,  9673,  9673,  9673,  9673,  9673,  9673,  9673,  9673,
    9673,  9673,  9673,  9673,  9673,  9673,  -876,  1985,  8269,  -876,
    2365,  5182,   125,   116,   116,  9907,  9907,   333,   726,   587,
     698,  -876,  -876,  1000,   733,    66,   129,   136,    52,   730,
    9907,   583,  -876,   634,  1012,  -876,  -876,  -876,  -876,    25,
      27,    29,    30,    39,    88,    90,   216,   291,  -876,  -876,
    -876,   299,  -876,  -876,  2639,  -876,  -876,  9790,  9790,  -876,
    -876,   322,  -876,  -876,  -876,   425,  9673,  9673,  8503,  -876,
    -876, 10765,  8269, 10846,  9673,  9673,  8737,  -876,    -1,   625,
    -876,  -876,    -1,  -876,   635,   637,  -876,    49,  -876,  -876,
    -876,  -876,  -876,  7646,  -876,  9673,  6252,   641, 10765, 10846,
    9673,  1276,   649,    -1,  -876,  -876,  7097,   648,  -876,  1276,
    -876,  8854,  -876,  -876,  8971,  -876,  -876,  -876,   488,  1014,
    -876,  -876,   654, 10476, 10927,  8269, 11008,  1156,  -876,  -876,
    -876,  -876,  -876,  -876,  -876,  -876,  -876,  -876,  -876,  -876,
     287,  -876,  -876,   657,  -876,  -876,  -876,   390,  -876,   663,
    -876,  9673,  -876,  -876,  -876,  -876,  -876,  -876,  -876,  -876,
    -876,  -876,  -876,  -876,  -876,  -876,  9673,  -876,   660,   676,
    -876,  -876,    -1, 10476,   679,  -876,  -876,  -876,   767,   719,
    1264,  -876,  -876,  -876,  1017,   323,   572,  2443,  2443,  2443,
    2443,   969,   969,  3497,  3359,  2443,  2443,  3221,  3221,   818,
     818,  2710,   969,   969,   834,   834,   765,   542,   542,   572,
     572,   572,  4623,  3657,  4899,  3795,   533,   690,  -876,    -1,
     643,  -876,   740,  -876,   533,  4347,   830,   833,  -876,  5325,
     831,  5611,   256,   256,   726,  9205,   830,   147, 11089,  8269,
   11170,  -876,   125,  -876,  1014,  -876,  -876,  -876, 11251,  8269,
    2639,  5182,  9907,  -876,  -876,  -876,  -876,  -876,  -876,  1288,
    -876,  1391,  -876,  -876,  -876,  7646,  9673,  -876,  9673,   451,
    -876,   472,  3243,  3933,    -1,   339,   382,  -876,  -876,  -876,
    -876,  8503,  8737,  -876,  -876,  9907,  2119,   708,  -876,  -876,
    -876,  -876,  6252,   274,  -876,    -1,    76, 10476,   654,   424,
     306,    -1,   411,   461,  -876,  -876,  -876,  -876,  -876,  -876,
    -876,  1214,  -876,  -876,  -876,  -876,  -876,  1584,  -876,  -876,
    -876,  -876,  -876,   731,  -876,   710,  9673,  -876,   717,   803,
     722,  -876,   724,   810,   728,   815,  -876,  -876,  1224,  -876,
    -876,  -876,  -876,  -876,   572,   860,  -876,   738,  -876,  -876,
     865,   739,  9322,  -876,   654, 10476,  8386,  9790,  9673, 11332,
    8269, 11413,   763,  9790,  9790,  -876,   624,   533,   754,   741,
    9790,  9790,  -876,   624,   533,  -876,  -876,  9439,   882,  -876,
     571,  -876,   882,  -876,  -876,  -876,  -876,   830,    82,  -876,
      72,   103,    -1,   155,   162,  9907,   333,  9907,  5182,   898,
     306,  -876,    -1,   830,    49,   762,  7782,  -876,    67,   406,
    -876,  -876,  -876,  -876,  -876,  9673,  9673,   404,  9673,  9673,
     773,    49,  -876,   780,  -876,  -876,  -876,   528,  1214,   576,
    -876,   779,    -1,  -876,    -1,  -876,  9673,  1584,  -876,  -876,
     699,  -876,  -876,  -876,   377,  -876,  1584,  -876,  -876,  1175,
    -876,  -876,  -876,  -876,  -876,  -876,  -876,  -876,  -876,    33,
    -876,    33,   782,  -876,  9673,   785,   654,  -876,  2119,  4761,
    5037,    -1,   432,   563,  9673,  -876,  -876,  -876,  -876,  -876,
    -876,  -876,  9790,  -876,  -876,  -876,  -876,  -876,  -876,   660,
    -876,   837,  -876,  5728,   921,  -876,  9907,   830,  -876,   830,
    6395,  6395,  -876,  9556,  5468,   165,   256,  5182,   333,   830,
    -876,  -876,  -876,  1584,  5182,  1584,  -876,  -876,  -876,  9673,
    8737,  -876,  -876,    -1,  1230,   799,  1310,  -876,   801,   806,
      65,  -876,  -876,  -876,   812,   813,  -876,   722,  -876,   816,
    -876,   829,  -876,   816,  -876,  -876,  1489,  6395,  -876,  -876,
    6109,  -876,   832,   662,  2119,  -876,  -876, 10589,   116,  -876,
    -876,  6395,  -876,  -876,   116,  -876,  -876,   830,   830,  -876,
     344,  -876,   830,  -876,  -876,    -1,   830,   333,   762,  -876,
     841,  1230,   595,  -876,  -876,  1382,  6395,  6109,  -876,  1584,
    -876,  1175,  -876,  -876,  1175,  -876,  1175,  -876,  -876,  -876,
    -876,    -1,   845,   835,   842, 10702,  -876,   843,   722,  -876,
     846,   847,  -876,  -876,  -876,   931,  -876,  1027,   152,   171,
     184,  5182,  -876,  5325,  -876,  -876,  -876,  -876,  -876,  6395,
    -876,  -876,  -876,  -876,  5182,  1230,   841,  1230,   851,  -876,
     185,  -876,  -876,   830,   854,   816,   868,   816,   816,  6512,
     869,  -876, 10702,  1584,  -876,   951,  1051,   699,  -876,  -876,
    1584,  -876,  1175,  -876,   199, 11494,  8269, 11575,   833,   571,
     830,   830,   841,  1230,  1382,  -876,  -876,  -876,  -876,  1175,
    -876,  -876,  -876,   902,  -876,  1051,  -876,   878,   884,  -876,
   11656,  -876,   722,   888,  -876,   892,   888,    94,   106,    -1,
     265,   321,  -876,  -876,  -876,  -876,   841,  -876,   816,  -876,
    1584,  -876,  1175,  -876,   881,   894,  -876,  1175,  -876,  1175,
    -876,  -876,   342,  -876,   888,   901,   888,   888,  -876,  1175,
    -876,  -876,  -876,   888,  -876
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       2,     0,     0,     1,     0,   349,   350,   351,     0,   342,
     343,   344,   347,   345,   346,   348,   337,   338,   339,   340,
     300,   263,   263,   524,   523,   525,   526,   631,     0,   631,
      10,     0,   528,   527,   529,   613,   615,   520,   519,   614,
     522,   514,   515,   516,   517,   463,   534,   535,     0,     0,
       0,     0,   292,   642,   642,    86,   409,   489,   487,   489,
     491,   471,   483,   477,   485,     0,     0,     0,     3,   629,
       6,     9,    33,    37,    49,    57,   263,    56,     0,    74,
       0,    78,    88,     0,    54,   242,     0,   288,     0,     0,
     314,   317,   629,     0,     0,     0,     0,    58,   309,   277,
     278,   462,   464,   279,   280,   281,   283,   282,   284,   460,
     461,   459,   512,   530,   531,   285,     0,   286,    62,     5,
       8,   169,   180,   170,   193,   166,   186,   176,   175,   196,
     197,   191,   174,   173,   168,   194,   198,   199,   178,   167,
     181,   185,   187,   179,   172,   188,   195,   190,   189,   182,
     192,   177,   165,   184,   183,   164,   171,   162,   163,   159,
     160,   161,   117,   119,   118,   154,   155,   150,   132,   133,
     134,   141,   138,   140,   135,   136,   156,   157,   142,   143,
     147,   151,   137,   139,   129,   130,   131,   144,   145,   146,
     148,   149,   152,   153,   158,   122,   124,   126,    26,   120,
     121,   123,   125,     0,     0,     0,     0,     0,     0,     0,
     487,     0,   258,     0,   243,   268,    72,   262,   642,     0,
     530,   531,     0,   286,   642,   607,    73,    71,   631,    70,
       0,   642,   434,    69,   631,   632,     0,     0,    21,   239,
       0,     0,   337,   338,   300,   303,   435,     0,   218,     0,
     219,   297,     0,    19,     0,     0,   629,    15,    18,   631,
      76,    14,   631,     0,     0,   635,   635,   244,     0,     0,
     635,   605,   631,     0,     0,     0,    84,   341,     0,    94,
      95,   102,   311,   410,   509,   508,   510,   507,     0,   506,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   513,    53,   233,   234,   638,   639,     4,   640,   630,
       0,     0,     0,     0,     0,     0,     0,   439,   437,   424,
      63,   308,   418,   420,     0,    90,     0,    82,    79,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   626,   432,   642,   625,
     430,     0,    55,     0,     0,     0,     0,   629,     0,   630,
       0,   363,   362,     0,     0,   530,   531,   286,   112,   113,
       0,     0,   115,   538,     0,   530,   531,   286,   329,   189,
     182,   192,   177,   159,   160,   161,   117,   118,   603,   331,
     602,     0,   628,   627,     0,   310,   465,     0,     0,   127,
     610,   297,   269,   612,   265,     0,     0,     0,     0,   259,
     267,   432,   642,   430,     0,     0,     0,   260,   631,     0,
     302,   264,   631,   254,   642,   642,   253,   631,   307,    52,
      23,    25,    24,     0,   304,     0,     0,     0,   432,   430,
       0,    17,     0,   631,   295,    13,   630,    75,   291,   293,
     298,   637,   636,   245,   637,   247,   299,   606,     0,   101,
     513,    92,    87,     0,   432,   642,   430,   562,   493,   496,
     494,   511,   490,   466,   488,   467,   468,   492,   469,   470,
       0,   473,   479,     0,   480,   475,   476,     0,   481,     0,
     482,     0,   641,     7,    27,    28,    29,    30,    31,    50,
      51,   444,   441,    60,    64,   444,     0,    34,   273,     0,
      36,   272,   631,     0,    80,    91,    48,    38,    46,     0,
     248,   268,   200,    35,     0,   286,   216,   223,   228,   229,
     230,   225,   227,   237,   238,   231,   232,   209,   210,   235,
     236,   631,   224,   226,   220,   221,   222,   211,   212,   213,
     214,   215,   616,   618,   617,   619,     0,   263,   429,   631,
     616,   618,   617,   619,     0,   263,     0,   642,   354,     0,
     353,     0,     0,     0,     0,     0,     0,   297,   432,   642,
     430,   322,   327,   112,   113,   114,   536,   325,   432,   642,
     430,     0,     0,   332,   624,   623,   334,   616,   617,   263,
      39,   248,   201,    45,   208,     0,     0,   609,     0,   270,
     266,   642,   616,   617,   631,   616,   617,   608,   301,   633,
     250,   255,   257,   306,    22,     0,   240,     0,    32,   427,
     425,   207,     0,    77,    16,   631,   635,     0,    85,   620,
     100,   631,   616,   617,   568,   565,   564,   563,   566,   574,
     583,     0,   594,   584,   598,   597,   593,   562,   411,   561,
     415,   567,   569,   570,   572,   547,   576,   581,   642,   586,
     642,   591,   547,   596,   547,     0,   545,   497,     0,   472,
     474,   484,   478,   486,   217,     0,   445,     0,   442,   441,
       0,   276,     0,    89,    83,     0,     0,     0,     0,   432,
     642,   430,     0,     0,     0,   433,    67,     0,     0,   436,
       0,     0,   431,    65,   642,   352,   289,   642,   642,   451,
     642,   355,   642,   357,   315,   356,   318,     0,     0,   321,
     620,   296,   631,   616,   617,     0,     0,     0,     0,   112,
     113,   116,   631,     0,   631,   540,     0,   252,   421,    59,
     251,   128,   611,   271,   261,     0,     0,   436,     0,     0,
     642,   631,    11,     0,   294,   246,    93,   436,     0,   375,
     366,   368,   631,   364,   631,   412,     0,     0,   554,   575,
       0,   543,   601,   585,     0,   544,     0,   557,   595,     0,
     559,   599,   498,   502,   503,   504,   495,   505,   440,   642,
     438,   642,     0,   419,     0,   274,    81,    47,   249,   616,
     617,   631,   616,   617,     0,    44,   205,    43,   206,    68,
     428,   634,     0,    41,   203,    42,   204,    66,   426,   452,
     453,   642,   454,     0,   642,   360,     0,     0,   358,     0,
       0,     0,   320,     0,     0,   436,     0,     0,     0,     0,
     436,   330,   604,   562,     0,   562,   335,   422,   423,     0,
     256,   305,    20,   631,     0,   373,     0,   571,     0,   403,
       0,   587,   546,   573,   547,   547,   582,   642,   600,   547,
     592,   547,   570,   547,   499,   401,   631,     0,   399,   398,
       0,    61,   275,   436,   241,    40,   202,     0,     0,   456,
     361,     0,    12,   458,     0,   312,   313,     0,     0,   270,
     642,   323,     0,   537,   326,   631,     0,     0,   540,   365,
     376,     0,   371,   367,   414,     0,     0,     0,   413,     0,
     550,     0,   552,   542,     0,   558,     0,   555,   560,   500,
     397,   631,     0,   570,   382,   578,   579,   642,   642,   589,
     382,   382,   380,   446,   443,     0,   455,     0,   530,   531,
     286,     0,   457,     0,   316,   319,   448,   449,   447,     0,
     328,   539,   333,   541,     0,     0,   374,     0,   369,   407,
     631,   405,   408,     0,     0,   547,   547,   547,   547,     0,
       0,   400,     0,   388,   390,     0,   577,     0,   378,   379,
       0,   393,     0,   395,   297,   432,   642,   430,   642,   642,
       0,     0,   372,     0,     0,   404,   417,   416,   551,     0,
     548,   553,   556,     0,   402,   588,   381,   382,   382,   297,
     432,   580,   642,   382,   590,   382,   382,   620,   296,   631,
     616,   617,   450,   359,   324,   336,   370,   406,   547,   501,
       0,   385,     0,   387,   620,   296,   377,     0,   394,     0,
     391,   396,   436,   549,   382,   382,   382,   382,   386,     0,
     383,   389,   392,   382,   384
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -876,  -876,  -876,  -382,  -876,    15,  -876,  -543,     2,  -876,
     575,  -876,    43,  -876,  -209,  -325,   180,   -58,   -59,  -876,
    -592,  -876,   817,    -5,   950,  -162,    20,   -71,  -876,  -443,
      -9,  2093,  -302,   949,   -56,  -876,   -12,  -876,  -876,     5,
    -876,  1150,  -876,   196,  -876,   166,   -27,   326,  -332,    89,
     -13,  -876,  -401,  -159,     7,  -876,  -308,   -26,  -876,  -876,
    -876,  -876,  -876,  -876,  -876,  -876,  -876,  -876,  -876,  -876,
    -876,  -876,  -876,  -876,  -876,  -876,    57,  -876,  -876,  -876,
    -876,  -876,  -876,  -876,  -876,  -876,  -876,   159,  -356,  -546,
      38,  -656,  -876,  -792,  -817,   282,   150,  -336,  -876,   241,
    -876,  -749,  -876,    41,  -876,  -876,  -876,  -876,  -876,  -876,
    -876,   308,  -876,  -876,  -876,  -876,  -876,  -876,  -876,   -92,
    -876,  -876,   369,  -876,  -876,   555,  -876,  -876,  -566,  -876,
      54,  -876,  -876,  -876,  -876,  -876,  -876,   978,  -876,  -876,
    -876,  -876,   777,  -876,  -876,  -876,  -876,  -876,  -876,   872,
    1024,  -876,    53,  -876,  -876,  -876,  -876,  -876,  -876,  -876,
      14,  -876,    28,  -876,   -30,  1683,  1997,   988,  2151,  1319,
    -876,  -876,   158,  -876,  -457,   111,  -586,  -847,  -415,  -329,
    -744,   178,  -259,   300,   104,  -876,  -876,  -876,   182,  -757,
    -875,    86,   316,  -876,  -721,  -876,   -70,  -582,  -876,  -876,
    -876,   105,  -394,  -876,  -319,  -876,  -876,   -83,  -876,   -42,
     -25,    78,  -535,  -222,   -64,   -15,    -2
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,    68,    69,    70,   240,   576,   577,   256,
     257,   452,   258,   443,    72,   527,    73,   363,    74,    75,
     514,   699,   247,    77,    78,   259,    79,    80,    81,   472,
      82,   213,   382,   383,   195,   196,   197,   198,   615,   565,
     200,    84,   445,   215,   264,   532,   232,   758,   432,   433,
     229,   230,   217,   419,   434,   520,   521,    85,   361,   262,
     263,   645,   635,   365,   850,   366,   851,   745,   979,   748,
     746,   601,   603,   755,   756,   928,   249,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,   726,   579,   734,
     847,   848,   374,   780,   781,   782,  1036,  1004,   951,   897,
     898,   878,   990,   991,   282,   283,   477,   785,   880,   668,
     938,   323,   515,    97,    98,   724,   717,   574,   566,   321,
     512,   511,   697,   698,   811,   695,   696,   809,   586,   978,
     728,   841,   908,   912,    99,   100,   101,   102,   103,   104,
     105,   294,   490,   106,   298,   107,   108,   296,   300,   290,
     288,   292,   482,   688,   687,   802,   894,   949,   999,   806,
     109,   289,   110,   111,   112,   220,   221,   115,   222,   223,
     597,   747,   864,   865,   882,   788,   670,   671,   672,   892,
     674,   675,   676,   677,   956,   957,   678,   679,   680,   681,
     959,   960,   682,   683,   684,   685,   686,   791,   401,   602,
     269,   435,   225,   118,   639,   568,   606,   600,   404,   307,
     429,   430,   719,   463,   580,   369,   261
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     119,   287,   236,   360,   239,   308,   405,   302,   581,   233,
     328,   212,   212,   199,   403,   212,   238,   620,   737,   120,
     669,   533,   201,   218,   218,   620,   569,   218,   308,   277,
     648,   364,   627,   199,   367,   301,   202,   736,   567,   890,
     399,   575,   201,   883,   465,    71,   276,    71,   467,   319,
     368,   267,   271,   255,   309,   277,   202,   930,   753,    86,
     265,    86,   199,   320,   637,   427,   885,   277,   277,   277,
     627,   260,   844,   219,   219,   891,   849,   219,  -613,   595,
     704,   784,   610,   613,   933,   609,   315,   316,   992,   767,
     624,  -103,   453,   317,   936,   578,     3,  -107,   795,   356,
     402,   199,   567,   493,   575,   216,   226,  -107,    86,   227,
     499,   895,   278,  -524,   986,  -523,   777,  -525,  -526,  -109,
     526,   224,   224,   219,   829,   224,   461,  -528,  -109,   578,
     640,   837,  -341,  -341,   462,  1044,   423,  -290,   278,  -518,
    -518,  -532,  -290,   651,   235,   219,   219,   952,   673,   219,
     373,   384,   384,   494,  -104,   896,   359,   640,   266,   270,
     500,  -111,   315,   316,   522,  -103,   449,   317,  -103,  -524,
    1022,  -523,  -110,  -525,  -526,   961,  -527,   992,  -529,   937,
    -106,  -341,   890,  -528,  -104,  1044,  -613,  -108,  -518,  -341,
    -105,  -613,   308,   933,   235,   476,  -518,  -111,   526,   526,
     883,   318,  1000,   -94,   471,   859,  1056,   855,   255,   -98,
     669,  -616,  -110,   464,   455,   241,   420,   860,   996,   853,
     319,   462,   420,   212,   305,   212,   212,   306,   251,   436,
     620,   620,  -527,  -616,  -529,   218,  -107,   218,   627,  -107,
    -100,   456,  -617,   447,   470,  -617,   783,   468,  -109,   277,
     268,  -109,   504,   505,   506,   507,   255,   742,   305,   883,
     773,   306,   816,    86,   976,   517,   -95,   752,   362,   362,
     528,   640,   362,  -102,   260,   318,   427,   925,  -106,   927,
     231,   640,  1038,   733,  -101,   219,   903,   219,   219,  1045,
     590,   219,   -97,   219,  -103,   451,   751,  -103,    86,   -99,
    -533,   277,   -96,   308,  -518,   943,   459,   582,   583,    86,
     921,    86,   438,  -104,   922,  -532,  -104,   428,   524,   431,
      86,   926,  1024,   883,   503,   584,  -111,  -104,   255,  -111,
     235,   519,   278,   224,  -108,   224,   519,   457,   673,  1075,
     458,  -110,  -103,   484,  -110,   487,   260,   491,   528,   528,
     478,   491,    71,   212,   977,  -105,   436,   508,   425,   843,
    -518,   585,   -94,   783,   875,   573,    86,   219,   219,   219,
     219,    86,   219,   219,   234,  1008,  1009,   293,   821,  -521,
     235,   219,   854,    86,   278,  -111,   534,   604,   825,   827,
     567,   -75,   575,  -296,  -296,   833,   835,   479,   305,   480,
     410,   306,   471,   412,   413,   414,   669,  -106,   669,   573,
    -106,   -89,   450,   408,   437,   219,   439,   212,    86,   840,
     436,   295,   219,   219,   775,   356,   357,   297,   573,   768,
     689,   634,   420,   420,   322,  -521,   -97,   219,   299,  -110,
    -111,  1021,  -296,   605,   119,  -617,   324,   277,   199,   -98,
    -296,   711,   471,   478,   573,   325,  -106,   201,   673,   783,
    1066,   783,   764,  -108,   534,   534,  -108,   673,   484,   620,
     212,   202,   769,   436,   358,   305,   627,   664,   306,   219,
     329,   573,   359,    58,  -105,   407,   -99,  -105,   478,    71,
     362,   362,   362,   362,   869,   509,   510,   277,   526,  -108,
     479,   665,   480,    86,   526,   526,   628,   905,   356,   421,
     630,   526,   526,    86,  1072,   633,   783,   988,   735,   735,
     518,  -105,   720,   621,   409,   531,   712,   234,   -97,   411,
     278,   643,   219,   692,   673,   479,   673,   480,   483,   715,
     616,   -98,   417,   690,   718,   362,   362,   722,   -97,  -106,
     690,   -97,   971,   -96,   716,   478,   -97,   422,   973,   486,
     592,   -98,   723,  -616,   -98,   359,   646,   953,   478,   -98,
     783,   416,   783,   612,   614,   729,   776,   440,   -99,   418,
     278,   730,   757,   732,   212,   846,   843,   436,   441,   442,
     612,   614,   738,   424,   212,   573,   759,   436,   -99,   718,
     703,   -99,   479,   761,   480,   573,   -99,   760,   783,   426,
     673,   764,   330,   617,   619,   479,   641,   480,   481,   420,
     199,   277,   268,   526,  1011,  1013,   718,   444,   654,   201,
     655,   656,   657,   658,   471,   593,    86,   955,    86,   594,
     119,   446,   330,   202,   454,   -96,   219,   654,   528,   655,
     656,   657,   658,   721,   528,   528,   219,   619,    86,   219,
     268,   528,   528,   228,  -533,   -96,   867,   -74,   -96,   353,
     354,   355,   231,   -96,   953,   460,   792,   469,   792,   277,
    -108,   953,   857,   466,  1049,    71,   473,   856,   408,   858,
     830,  -104,   219,   356,   448,   488,   501,   757,   640,    86,
     866,  1061,  1063,   492,   278,   212,   495,  1068,   436,  1070,
    1071,   -95,   701,   874,   498,  -111,   573,   718,   525,   450,
     888,   513,   760,   774,   888,   842,   845,   718,   845,   502,
     845,   953,   987,   720,   519,  -102,   770,   478,  1078,  1080,
    1081,  1082,   422,   585,   955,   739,  -110,  1084,   955,   817,
     359,   955,   832,   955,   587,   596,  -614,   659,   591,   879,
    -106,   199,   278,    86,   534,   629,  -101,   219,   420,   660,
     534,   534,   631,   528,   632,   478,   638,   534,   534,  -105,
     -97,   531,   754,   642,   479,   -89,   480,   483,   914,   356,
     474,   647,   735,   797,   923,   800,   718,  -268,   663,   664,
     691,   955,   219,   955,   219,    86,   693,   899,   955,   899,
     955,   706,   762,   702,   763,   771,   705,  -521,  -521,    76,
     955,    76,   479,   665,   480,   485,   962,   619,   268,  -427,
     721,   832,   862,    76,    76,   330,   707,    76,   475,   909,
     725,   727,   913,   772,   731,   910,   359,   787,   786,   871,
     343,   344,   917,   918,   790,   793,   920,  -108,  -105,   794,
     877,   796,   798,   983,  -614,   799,  -521,   801,    76,  -614,
     808,   879,   789,   810,  -521,   813,  -269,   -99,   -96,   826,
     828,   277,   824,    76,  1017,   792,   834,   836,   330,   534,
     351,   352,   353,   354,   355,   831,   852,   843,   815,   963,
      86,   863,   964,   219,   330,    76,    76,    86,    86,    76,
     870,    86,   861,   972,    86,   872,   876,   901,   845,   343,
     344,    86,  -270,   839,  -620,   362,   879,   362,   907,   277,
     911,   826,   828,   962,   834,   836,   931,   888,   993,   994,
     962,   934,   962,   351,   352,   353,   354,   355,   935,   939,
     941,   929,  1002,   944,    86,   792,   792,    86,   350,   351,
     352,   353,   354,   355,   967,  1025,   946,  1001,    86,  -271,
    -530,  -530,   887,  1018,   889,  1019,   277,   893,   985,  1003,
    1007,  1020,   881,  1010,  1012,  -620,  -620,  1014,  1023,  1027,
     962,  1034,   962,    86,    86,   940,   942,   962,   906,   962,
     945,  1033,   947,   981,   948,  1029,   915,  1039,   916,   962,
     902,   212,  1006,  1059,   436,  1060,   729,   845,   924,  -530,
    -616,  1062,   573,    76,   718,  1067,   362,  -530,    86,  1069,
      86,   644,  -620,  -617,  -620,   906,    86,  -616,  1079,   330,
     792,    86,  -620,   371,   388,    76,   950,    76,    76,   919,
     838,    76,   900,    76,   343,   344,    86,  1053,    76,  1035,
     873,  -531,  -531,  -286,  -286,  1057,   268,   868,   812,    76,
     700,    76,  1052,   219,   954,   497,   974,   975,   958,   406,
      76,   980,   415,   291,   400,   982,   984,   356,   588,  1037,
     886,   348,   349,   350,   351,   352,   353,   354,   355,   356,
     598,  -297,  -297,   884,   356,   709,  1028,  1030,  1031,  1032,
    -531,  1041,  -286,     0,   356,  1015,     0,   995,  -531,   997,
    -286,     0,     0,     0,   998,     0,    76,    76,    76,    76,
      76,    76,    76,    76,     0,     0,   589,     0,   356,  1040,
       0,    76,     0,    76,   359,     0,    76,     0,   599,     0,
    -297,     0,  1026,   710,     0,     0,   359,     0,  -297,     0,
       0,   359,     0,  1016,     0,     0,   214,   214,     0,  1073,
     214,   359,   478,     0,     0,    76,     0,     0,    76,  1054,
    1055,     0,    76,    76,     0,   958,     0,   599,  1043,  1042,
    1046,     0,   958,     0,   958,   359,     0,    76,   248,   250,
       0,     0,     0,   214,   214,     0,     0,  1058,   654,     0,
     655,   656,   657,   658,   659,   478,   303,   304,     0,   479,
       0,   480,   489,     0,    76,    76,   660,   654,     0,   655,
     656,   657,   658,   659,     0,     0,     0,     0,  1074,    76,
    1076,     0,   958,     0,   958,   660,     0,  1077,   661,   958,
       0,   958,     0,     0,   662,   663,   664,  1083,     0,     0,
       0,   958,   479,    76,   480,   496,   654,   661,   655,   656,
     657,   658,     0,    76,   663,   664,     0,     0,   803,   804,
     665,   805,   654,   666,   655,   656,   657,   658,  -642,    46,
      47,     0,    76,     0,     0,   667,  -642,  -642,  -642,   665,
       0,  -642,  -642,  -642,     0,  -642,   778,     0,   708,     0,
       0,     0,   779,     0,  -642,  -642,   310,   311,   312,   313,
     314,   117,   778,   117,     0,  -642,  -642,     0,  -642,  -642,
    -642,  -642,  -642,     0,   330,   331,   332,   333,   334,   335,
     336,   337,   338,   339,   340,   341,   342,     0,     0,   343,
     344,     0,     0,     0,   214,     0,     0,   214,   214,   214,
       0,   303,   654,     0,   655,   656,   657,   658,     0,     0,
     117,     0,     0,     0,   281,  -642,  -642,     0,   214,     0,
     214,   214,   345,     0,   346,   347,   348,   349,   350,   351,
     352,   353,   354,   355,     0,     0,    76,     0,    76,  -642,
     281,  -243,   778,     0,     0,     0,    76,     0,   932,     0,
       0,     0,   377,   387,   387,     0,    76,     0,    76,    76,
       0,     0,  -642,  -642,     0,     0,     0,   231,  -642,     0,
    -642,     0,  -642,  -642,   989,   708,   655,   656,   657,   658,
      41,    42,    43,    44,     0,     0,     0,     0,     0,     0,
       0,     0,    76,     0,     0,     0,     0,     0,     0,    76,
       0,   330,   331,   332,   333,   334,   335,   336,   337,   338,
     339,   340,   341,   342,   214,     0,   343,   344,     0,   530,
     536,   537,   538,   539,   540,   541,   542,   543,   544,   545,
     546,   547,   548,   549,   550,   551,   552,   553,   554,   555,
     556,   557,   558,   559,   560,   561,     0,     0,   214,   345,
       0,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,     0,     0,    76,    76,   117,     0,    76,     0,     0,
      76,    76,     0,     0,     0,     0,     0,    76,    76,     0,
       0,   654,     0,   655,   656,   657,   658,   659,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   611,   611,   660,
     117,     0,    76,     0,    76,    76,   611,   214,   214,     0,
       0,   117,   214,   117,   611,   611,   214,     0,     0,     0,
       0,   661,   117,     0,     0,     0,     0,   662,   663,   664,
       0,     0,     0,     0,   281,   636,     0,     0,     0,     0,
     611,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   214,     0,   665,   214,     0,   666,     0,     0,     0,
       0,     0,     0,     0,     0,   214,     0,     0,   117,     0,
       0,     0,     0,   117,   235,     0,   654,     0,   655,   656,
     657,   658,   659,     0,     0,   117,   281,     0,   535,    76,
       0,   694,     0,     0,   660,     0,     0,     0,     0,     0,
      76,     0,     0,    76,     0,     0,   214,    76,    76,     0,
       0,    76,     0,     0,    76,     0,   661,     0,     0,     0,
     117,    76,   662,   663,   664,   113,     0,   113,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   665,     0,
       0,   666,     0,     0,    76,     0,     0,    76,     0,     0,
       0,     0,     0,     0,     0,     0,   535,   535,    76,     0,
       0,     0,     0,     0,   113,   214,     0,     0,   279,   214,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   214,
       0,     0,     0,    76,    76,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   279,   117,   214,     0,   214,     0,
       0,     0,     0,     0,     0,   117,   375,   385,   385,   385,
       0,   214,   214,     0,     0,     0,     0,     0,    76,     0,
      76,     0,   281,     0,     0,     0,    76,     0,     0,     0,
       0,    76,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   214,     0,     0,     0,
       0,     0,     0,    76,     0,     0,     0,     0,     0,     0,
       0,     0,   281,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   214,     0,     0,     0,     0,   611,   818,     0,
     214,     0,     0,   611,   611,     0,     0,     0,     0,     0,
     611,   611,     0,     0,     0,     0,     0,   214,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   113,
       0,     0,     0,     0,     0,     0,     0,     0,   117,     0,
     117,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   611,   611,     0,   611,   611,
     117,     0,     0,     0,   113,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   113,   214,   113,     0,     0,
       0,     0,     0,     0,     0,     0,   113,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   279,     0,
       0,   117,     0,     0,   214,     0,   281,     0,     0,     0,
       0,     0,     0,     0,   904,     0,     0,     0,     0,     0,
       0,     0,   611,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   113,     0,     0,     0,     0,   113,     0,   114,
       0,   114,     0,   214,     0,     0,     0,   807,     0,   113,
     279,     0,     0,     0,     0,     0,     0,     0,     0,   611,
     214,     0,     0,     0,   281,   117,   535,     0,     0,     0,
       0,     0,   535,   535,     0,     0,     0,   562,   563,   535,
     535,   564,     0,     0,   113,     0,     0,     0,   114,     0,
       0,     0,   280,   165,   166,   167,   168,   169,   170,   171,
     172,   173,     0,     0,   174,   175,     0,   117,   176,   177,
     178,   179,     0,     0,     0,     0,     0,     0,   280,     0,
       0,     0,     0,   180,   181,     0,     0,     0,     0,     0,
     376,   386,   386,   386,     0,    83,     0,    83,     0,     0,
       0,     0,     0,     0,     0,   182,   183,   184,   185,   186,
     187,   188,   189,   190,   191,     0,   192,   193,     0,     0,
       0,     0,     0,   194,     0,     0,     0,     0,     0,   113,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   113,
       0,     0,     0,     0,    83,     0,     0,     0,     0,     0,
       0,   535,     0,   116,     0,   116,   279,     0,     0,     0,
       0,     0,   117,     0,     0,     0,   214,     0,     0,   117,
     117,     0,     0,   117,     0,     0,   117,     0,     0,     0,
       0,     0,     0,   117,     0,     0,   372,     0,     0,   330,
     331,   332,   333,   334,   335,   336,   337,   338,   339,   340,
     341,   342,   116,   114,   343,   344,   279,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   117,     0,     0,   117,
       0,     0,     0,     0,     0,     0,   970,     0,     0,     0,
     117,     0,     0,     0,     0,     0,     0,   345,   114,   346,
     347,   348,   349,   350,   351,   352,   353,   354,   355,   114,
       0,   114,     0,     0,     0,   117,   117,     0,     0,     0,
     114,     0,   113,     0,   113,     0,     0,     0,     0,     0,
       0,     0,   280,     0,   387,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   113,     0,     0,     0,     0,     0,
     117,     0,   117,     0,     0,     0,     0,     0,   117,    83,
       0,     0,     0,   117,     0,     0,   114,     0,     0,     0,
       0,   114,     0,     0,     0,     0,     0,     0,   117,     0,
       0,   387,     0,   114,   280,   113,     0,     0,     0,     0,
     279,     0,     0,     0,    83,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    83,     0,    83,     0,     0,
       0,     0,     0,     0,     0,     0,    83,   116,   114,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   279,   113,
       0,     0,   116,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    83,   116,     0,   116,     0,    83,     0,     0,
       0,     0,     0,     0,   116,     0,     0,   570,   571,    83,
       0,   572,   529,     0,     0,     0,     0,     0,     0,     0,
       0,   113,     0,   165,   166,   167,   168,   169,   170,   171,
     172,   173,     0,   114,   174,   175,     0,     0,   176,   177,
     178,   179,     0,   114,    83,     0,     0,     0,     0,     0,
     116,     0,     0,   180,   181,   116,     0,     0,     0,     0,
     280,     0,     0,     0,     0,     0,     0,   116,     0,     0,
     116,     0,     0,     0,     0,   182,   183,   184,   185,   186,
     187,   188,   189,   190,   191,     0,   192,   193,     0,     0,
     529,   529,     0,   194,     0,     0,     0,     0,     0,     0,
       0,     0,   116,   330,  -643,  -643,  -643,  -643,   335,   336,
     280,     0,  -643,  -643,     0,     0,   113,     0,   343,   344,
       0,     0,     0,   113,   113,     0,     0,   113,     0,    83,
     113,     0,     0,     0,     0,     0,     0,   113,     0,    83,
       0,     0,     0,     0,     0,     0,     0,     0,   116,   116,
       0,     0,     0,   346,   347,   348,   349,   350,   351,   352,
     353,   354,   355,     0,     0,     0,   114,     0,   114,     0,
     113,     0,     0,   113,     0,     0,     0,     0,     0,     0,
     968,     0,     0,     0,   113,     0,     0,   116,   114,     0,
       0,     0,     0,     0,     0,     0,     0,   116,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   113,
     113,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   385,   114,
       0,     0,     0,     0,   280,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   113,     0,   113,     0,     0,     0,
       0,     0,   113,     0,     0,     0,     0,   113,     0,     0,
       0,     0,    83,     0,    83,     0,     0,     0,     0,     0,
       0,     0,   113,     0,     0,   385,     0,     0,     0,     0,
       0,   607,   571,     0,    83,   608,     0,     0,     0,     0,
       0,     0,   280,   114,     0,     0,     0,   165,   166,   167,
     168,   169,   170,   171,   172,   173,     0,     0,   174,   175,
       0,     0,   176,   177,   178,   179,     0,     0,     0,     0,
     116,     0,   116,     0,     0,    83,     0,   180,   181,     0,
       0,     0,     0,     0,     0,   114,     0,     0,     0,     0,
       0,     0,   116,     0,     0,     0,     0,     0,     0,   182,
     183,   184,   185,   186,   187,   188,   189,   190,   191,     0,
     192,   193,     0,     0,     0,     0,     0,   194,     0,     0,
     330,   331,   332,   333,   334,   335,   336,   337,   338,   339,
     340,   341,   342,   116,     0,   343,   344,     0,     0,    83,
     529,     0,     0,     0,     0,     0,   529,   529,     0,     0,
       0,     0,     0,   529,   529,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   345,     0,
     346,   347,   348,   349,   350,   351,   352,   353,   354,   355,
     114,    83,     0,     0,     0,     0,     0,   114,   114,     0,
       0,   114,     0,     0,   114,   235,     0,   116,   116,     0,
       0,   114,     0,     0,   116,   116,     0,     0,     0,     0,
       0,   116,   116,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   114,     0,     0,   114,     0,   116,
       0,     0,     0,     0,   969,     0,     0,     0,   114,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   529,     0,     0,     0,     0,
       0,     0,     0,   114,   114,     0,    83,     0,     0,     0,
       0,     0,     0,    83,    83,     0,     0,    83,     0,     0,
      83,     0,   386,     0,     0,     0,     0,    83,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   114,     0,
     114,     0,     0,     0,     0,     0,   114,     0,     0,     0,
       0,   114,     0,   116,     0,     0,     0,     0,     0,     0,
      83,     0,     0,    83,   116,     0,   114,     0,     0,   386,
     966,   116,   116,     0,    83,   116,     0,     0,   116,     0,
       0,     0,     0,     0,     0,   116,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    83,
      83,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   116,     0,
       0,   116,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   116,     0,    83,     0,    83,     0,     0,     0,
       0,     0,    83,     0,     0,     0,     0,    83,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   116,   116,     0,
       0,     0,    83,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   116,     0,   116,     0,     0,     0,     0,     0,
     116,     0,     0,  -642,     4,   116,     5,     6,     7,     8,
       9,     0,     0,     0,    10,    11,     0,     0,     0,    12,
     116,    13,    14,    15,    16,    17,    18,    19,     0,     0,
       0,     0,     0,    20,    21,    22,    23,    24,    25,    26,
       0,     0,    27,     0,     0,     0,     0,     0,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,     0,    41,    42,    43,    44,     0,    45,    46,    47,
       0,    48,    49,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    50,     0,     0,    51,    52,     0,    53,    54,
       0,    55,     0,     0,    56,    57,    58,    59,    60,    61,
      62,    63,    64,  -620,     0,     0,     0,     0,     0,     0,
       0,  -620,  -620,  -620,     0,     0,  -620,  -620,  -620,     0,
    -620,     0,     0,    65,    66,    67,     0,     0,     0,  -620,
    -620,  -620,  -620,     0,     0,  -642,     0,     0,  -642,     0,
    -620,  -620,     0,  -620,  -620,  -620,  -620,  -620,     0,     0,
       0,   330,   331,   332,   333,   334,   335,   336,   337,   338,
     339,   340,  -643,  -643,     0,     0,   343,   344,     0,     0,
       0,     0,     0,  -620,  -620,  -620,  -620,  -620,  -620,  -620,
    -620,  -620,  -620,  -620,  -620,  -620,     0,     0,  -620,  -620,
    -620,  -620,     0,   765,  -620,     0,     0,     0,     0,     0,
    -620,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,     0,     0,     0,  -620,     0,     0,  -620,     0,     0,
    -107,  -620,  -620,  -620,  -620,  -620,  -620,  -620,  -620,  -620,
    -620,  -620,  -620,     0,     0,     0,     0,  -620,  -620,  -620,
    -620,  -518,     0,  -620,  -620,  -620,     0,  -620,  -620,  -518,
    -518,  -518,     0,     0,  -518,  -518,  -518,     0,  -518,     0,
       0,     0,     0,     0,     0,     0,  -518,     0,  -518,  -518,
    -518,     0,     0,     0,     0,     0,     0,     0,  -518,  -518,
       0,  -518,  -518,  -518,  -518,  -518,     0,     0,     0,   330,
     331,   332,   333,   334,   335,   336,   337,     0,   339,   340,
       0,     0,     0,     0,   343,   344,     0,     0,     0,     0,
       0,  -518,  -518,  -518,  -518,  -518,  -518,  -518,  -518,  -518,
    -518,  -518,  -518,  -518,     0,     0,  -518,  -518,  -518,  -518,
       0,  -518,  -518,     0,     0,     0,     0,     0,  -518,   346,
     347,   348,   349,   350,   351,   352,   353,   354,   355,     0,
       0,     0,  -518,     0,     0,  -518,     0,     0,  -518,  -518,
    -518,  -518,  -518,  -518,  -518,  -518,  -518,  -518,  -518,  -518,
    -518,     0,     0,     0,     0,     0,  -518,  -518,  -518,  -521,
       0,  -518,  -518,  -518,     0,  -518,  -518,  -521,  -521,  -521,
       0,     0,  -521,  -521,  -521,     0,  -521,     0,     0,     0,
       0,     0,     0,     0,  -521,     0,  -521,  -521,  -521,     0,
       0,     0,     0,     0,     0,     0,  -521,  -521,     0,  -521,
    -521,  -521,  -521,  -521,     0,     0,     0,   330,   331,   332,
     333,   334,   335,   336,     0,     0,   339,   340,     0,     0,
       0,     0,   343,   344,     0,     0,     0,     0,     0,  -521,
    -521,  -521,  -521,  -521,  -521,  -521,  -521,  -521,  -521,  -521,
    -521,  -521,     0,     0,  -521,  -521,  -521,  -521,     0,  -521,
    -521,     0,     0,     0,     0,     0,  -521,   346,   347,   348,
     349,   350,   351,   352,   353,   354,   355,     0,     0,     0,
    -521,     0,     0,  -521,     0,     0,  -521,  -521,  -521,  -521,
    -521,  -521,  -521,  -521,  -521,  -521,  -521,  -521,  -521,     0,
       0,     0,     0,     0,  -521,  -521,  -521,  -621,     0,  -521,
    -521,  -521,     0,  -521,  -521,  -621,  -621,  -621,     0,     0,
    -621,  -621,  -621,     0,  -621,     0,     0,     0,     0,     0,
       0,     0,     0,  -621,  -621,  -621,  -621,     0,     0,     0,
       0,     0,     0,     0,  -621,  -621,     0,  -621,  -621,  -621,
    -621,  -621,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -621,  -621,  -621,
    -621,  -621,  -621,  -621,  -621,  -621,  -621,  -621,  -621,  -621,
       0,     0,  -621,  -621,  -621,  -621,     0,     0,  -621,     0,
       0,     0,     0,     0,  -621,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -621,     0,
       0,  -621,     0,     0,     0,  -621,  -621,  -621,  -621,  -621,
    -621,  -621,  -621,  -621,  -621,  -621,  -621,     0,     0,     0,
       0,  -621,  -621,  -621,  -621,  -622,     0,  -621,  -621,  -621,
       0,  -621,  -621,  -622,  -622,  -622,     0,     0,  -622,  -622,
    -622,     0,  -622,     0,     0,     0,     0,     0,     0,     0,
       0,  -622,  -622,  -622,  -622,     0,     0,     0,     0,     0,
       0,     0,  -622,  -622,     0,  -622,  -622,  -622,  -622,  -622,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -622,  -622,  -622,  -622,  -622,
    -622,  -622,  -622,  -622,  -622,  -622,  -622,  -622,     0,     0,
    -622,  -622,  -622,  -622,     0,     0,  -622,     0,     0,     0,
       0,     0,  -622,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -622,     0,     0,  -622,
       0,     0,     0,  -622,  -622,  -622,  -622,  -622,  -622,  -622,
    -622,  -622,  -622,  -622,  -622,     0,     0,     0,     0,  -622,
    -622,  -622,  -622,  -296,     0,  -622,  -622,  -622,     0,  -622,
    -622,  -296,  -296,  -296,     0,     0,  -296,  -296,  -296,     0,
    -296,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -296,  -296,  -296,     0,     0,     0,     0,     0,     0,     0,
    -296,  -296,     0,  -296,  -296,  -296,  -296,  -296,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -296,  -296,  -296,  -296,  -296,  -296,  -296,
    -296,  -296,  -296,  -296,  -296,  -296,     0,     0,  -296,  -296,
    -296,  -296,     0,   766,  -296,     0,     0,     0,     0,     0,
    -296,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -296,     0,     0,  -296,     0,     0,
    -109,  -296,  -296,  -296,  -296,  -296,  -296,  -296,  -296,  -296,
    -296,  -296,  -296,     0,     0,     0,     0,     0,  -296,  -296,
    -296,  -435,     0,  -296,  -296,  -296,     0,  -296,  -296,  -435,
    -435,  -435,     0,     0,  -435,  -435,  -435,     0,  -435,     0,
       0,     0,     0,     0,     0,     0,     0,  -435,  -435,  -435,
       0,     0,     0,     0,     0,     0,     0,     0,  -435,  -435,
       0,  -435,  -435,  -435,  -435,  -435,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -435,  -435,  -435,  -435,  -435,  -435,  -435,  -435,  -435,
    -435,  -435,  -435,  -435,     0,     0,  -435,  -435,  -435,  -435,
       0,     0,  -435,     0,     0,     0,     0,     0,  -435,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -435,     0,     0,     0,     0,     0,     0,  -435,
       0,  -435,  -435,  -435,  -435,  -435,  -435,  -435,  -435,  -435,
    -435,     0,     0,     0,     0,  -435,  -435,  -435,  -435,  -287,
     231,  -435,  -435,  -435,     0,  -435,  -435,  -287,  -287,  -287,
       0,     0,  -287,  -287,  -287,     0,  -287,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -287,  -287,  -287,     0,
       0,     0,     0,     0,     0,     0,  -287,  -287,     0,  -287,
    -287,  -287,  -287,  -287,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -287,
    -287,  -287,  -287,  -287,  -287,  -287,  -287,  -287,  -287,  -287,
    -287,  -287,     0,     0,  -287,  -287,  -287,  -287,     0,     0,
    -287,     0,     0,     0,     0,     0,  -287,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -287,     0,     0,  -287,     0,     0,     0,  -287,  -287,  -287,
    -287,  -287,  -287,  -287,  -287,  -287,  -287,  -287,  -287,     0,
       0,     0,     0,     0,  -287,  -287,  -287,  -425,     0,  -287,
    -287,  -287,     0,  -287,  -287,  -425,  -425,  -425,     0,     0,
    -425,  -425,  -425,     0,  -425,     0,     0,     0,     0,     0,
       0,     0,     0,  -425,  -425,  -425,     0,     0,     0,     0,
       0,     0,     0,     0,  -425,  -425,     0,  -425,  -425,  -425,
    -425,  -425,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -425,  -425,  -425,
    -425,  -425,  -425,  -425,  -425,  -425,  -425,  -425,  -425,  -425,
       0,     0,  -425,  -425,  -425,  -425,     0,     0,  -425,     0,
       0,     0,     0,     0,  -425,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -425,     0,
       0,     0,     0,     0,     0,  -425,     0,  -425,  -425,  -425,
    -425,  -425,  -425,  -425,  -425,  -425,  -425,     0,     0,     0,
       0,  -425,  -425,  -425,  -425,  -303,  -425,  -425,  -425,  -425,
       0,  -425,  -425,  -303,  -303,  -303,     0,     0,  -303,  -303,
    -303,     0,  -303,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -303,  -303,     0,     0,     0,     0,     0,     0,
       0,     0,  -303,  -303,     0,  -303,  -303,  -303,  -303,  -303,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -303,  -303,  -303,  -303,  -303,
    -303,  -303,  -303,  -303,  -303,  -303,  -303,  -303,     0,     0,
    -303,  -303,  -303,  -303,     0,     0,  -303,     0,     0,     0,
       0,     0,  -303,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -303,     0,     0,     0,
       0,     0,     0,  -303,     0,  -303,  -303,  -303,  -303,  -303,
    -303,  -303,  -303,  -303,  -303,     0,     0,     0,     0,     0,
    -303,  -303,  -303,  -620,   228,  -303,  -303,  -303,     0,  -303,
    -303,  -620,  -620,  -620,     0,     0,     0,  -620,  -620,     0,
    -620,     0,     0,     0,     0,     0,     0,     0,     0,  -620,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -620,  -620,     0,  -620,  -620,  -620,  -620,  -620,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -620,  -620,  -620,  -620,  -620,  -620,  -620,
    -620,  -620,  -620,  -620,  -620,  -620,     0,     0,  -620,  -620,
    -620,  -620,     0,   713,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -620,     0,     0,     0,     0,     0,
    -107,  -620,     0,  -620,  -620,  -620,  -620,  -620,  -620,  -620,
    -620,  -620,  -620,     0,     0,     0,     0,  -620,  -620,  -620,
     -98,  -620,     0,  -620,     0,  -620,     0,  -620,  -620,  -620,
    -620,  -620,     0,     0,     0,  -620,  -620,     0,  -620,     0,
       0,     0,     0,     0,     0,     0,     0,  -620,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -620,  -620,
       0,  -620,  -620,  -620,  -620,  -620,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -620,  -620,  -620,  -620,  -620,  -620,  -620,  -620,  -620,
    -620,  -620,  -620,  -620,     0,     0,  -620,  -620,  -620,  -620,
       0,   713,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -620,     0,     0,     0,     0,     0,  -107,  -620,
       0,  -620,  -620,  -620,  -620,  -620,  -620,  -620,  -620,  -620,
    -620,     0,     0,     0,     0,  -620,  -620,  -620,  -620,  -296,
       0,  -620,     0,  -620,     0,  -620,  -620,  -296,  -296,  -296,
       0,     0,     0,  -296,  -296,     0,  -296,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -296,  -296,     0,  -296,
    -296,  -296,  -296,  -296,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -296,
    -296,  -296,  -296,  -296,  -296,  -296,  -296,  -296,  -296,  -296,
    -296,  -296,     0,     0,  -296,  -296,  -296,  -296,     0,   714,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -296,     0,     0,     0,     0,     0,  -109,  -296,     0,  -296,
    -296,  -296,  -296,  -296,  -296,  -296,  -296,  -296,  -296,     0,
       0,     0,     0,     0,  -296,  -296,  -100,  -296,     0,  -296,
       0,  -296,     0,  -296,  -296,  -296,  -296,  -296,     0,     0,
       0,  -296,  -296,     0,  -296,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -296,  -296,     0,  -296,  -296,  -296,
    -296,  -296,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -296,  -296,  -296,
    -296,  -296,  -296,  -296,  -296,  -296,  -296,  -296,  -296,  -296,
       0,     0,  -296,  -296,  -296,  -296,     0,   714,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -296,     0,
       0,     0,     0,     0,  -109,  -296,     0,  -296,  -296,  -296,
    -296,  -296,  -296,  -296,  -296,  -296,  -296,     0,     0,     0,
       0,     0,  -296,  -296,  -296,     0,     0,  -296,     0,  -296,
       0,  -296,  -296,   252,     0,     5,     6,     7,     8,     9,
    -642,  -642,  -642,    10,    11,     0,     0,  -642,    12,     0,
      13,    14,    15,    16,    17,    18,    19,     0,     0,     0,
       0,     0,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,    28,    29,   253,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
       0,    41,    42,    43,    44,     0,    45,    46,    47,     0,
      48,    49,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    50,     0,     0,    51,    52,     0,    53,    54,     0,
      55,     0,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    65,    66,    67,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -642,     0,   252,  -642,     5,     6,
       7,     8,     9,     0,     0,  -642,    10,    11,     0,  -642,
    -642,    12,     0,    13,    14,    15,    16,    17,    18,    19,
       0,     0,     0,     0,     0,    20,    21,    22,    23,    24,
      25,    26,     0,     0,    27,     0,     0,     0,     0,     0,
      28,    29,   253,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,     0,    41,    42,    43,    44,     0,    45,
      46,    47,     0,    48,    49,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    50,     0,     0,    51,    52,     0,
      53,    54,     0,    55,     0,     0,    56,    57,    58,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    65,    66,    67,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -642,     0,   252,
    -642,     5,     6,     7,     8,     9,     0,     0,  -642,    10,
      11,     0,     0,  -642,    12,  -642,    13,    14,    15,    16,
      17,    18,    19,     0,     0,     0,     0,     0,    20,    21,
      22,    23,    24,    25,    26,     0,     0,    27,     0,     0,
       0,     0,     0,    28,    29,   253,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,     0,    41,    42,    43,
      44,     0,    45,    46,    47,     0,    48,    49,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    50,     0,     0,
      51,    52,     0,    53,    54,     0,    55,     0,     0,    56,
      57,    58,    59,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    65,    66,
      67,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -642,     0,   252,  -642,     5,     6,     7,     8,     9,     0,
       0,  -642,    10,    11,     0,     0,  -642,    12,     0,    13,
      14,    15,    16,    17,    18,    19,     0,     0,     0,     0,
       0,    20,    21,    22,    23,    24,    25,    26,     0,     0,
      27,     0,     0,     0,     0,     0,    28,    29,   253,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,     0,
      41,    42,    43,    44,     0,    45,    46,    47,     0,    48,
      49,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      50,     0,     0,    51,    52,     0,    53,    54,     0,    55,
       0,     0,    56,    57,    58,    59,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,   252,
       0,     5,     6,     7,     8,     9,     0,  -642,  -642,    10,
      11,    65,    66,    67,    12,     0,    13,    14,    15,    16,
      17,    18,    19,  -642,     0,     0,  -642,     0,    20,    21,
      22,    23,    24,    25,    26,     0,     0,    27,     0,     0,
       0,     0,     0,    28,    29,   253,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,     0,    41,    42,    43,
      44,     0,    45,    46,    47,     0,    48,    49,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    50,     0,     0,
      51,    52,     0,    53,    54,     0,    55,     0,     0,    56,
      57,    58,    59,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,   252,     0,     5,     6,
       7,     8,     9,     0,     0,     0,    10,    11,    65,    66,
      67,    12,     0,    13,    14,    15,    16,    17,    18,    19,
    -642,     0,     0,  -642,     0,    20,    21,    22,    23,    24,
      25,    26,     0,     0,    27,     0,     0,     0,     0,     0,
      28,    29,   253,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,     0,    41,    42,    43,    44,     0,    45,
      46,    47,     0,    48,    49,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    50,     0,     0,   254,    52,     0,
      53,    54,     0,    55,     0,     0,    56,    57,    58,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    65,    66,    67,   252,     0,
       5,     6,     7,     8,     9,  -642,     0,  -642,    10,    11,
    -642,     0,     0,    12,     0,    13,    14,    15,    16,    17,
      18,    19,     0,     0,     0,     0,     0,    20,    21,    22,
      23,    24,    25,    26,     0,     0,    27,     0,     0,     0,
       0,     0,    28,    29,   253,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,     0,    41,    42,    43,    44,
       0,    45,    46,    47,     0,    48,    49,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    50,     0,     0,    51,
      52,     0,    53,    54,     0,    55,     0,     0,    56,    57,
      58,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    65,    66,    67,
     252,     0,     5,     6,     7,     8,     9,  -642,     0,  -642,
      10,    11,  -642,     0,     0,    12,     0,    13,    14,    15,
      16,    17,    18,    19,     0,     0,     0,     0,     0,    20,
      21,    22,    23,    24,    25,    26,     0,     0,    27,     0,
       0,     0,     0,     0,    28,    29,   253,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,     0,    41,    42,
      43,    44,     0,    45,    46,    47,     0,    48,    49,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    50,     0,
       0,    51,    52,     0,    53,    54,     0,    55,     0,     0,
      56,    57,    58,    59,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    65,
      66,    67,     0,     0,  -642,     0,     0,     0,     0,     0,
       0,  -642,     0,     4,  -642,     5,     6,     7,     8,     9,
       0,     0,     0,    10,    11,     0,     0,     0,    12,     0,
      13,    14,    15,    16,    17,    18,    19,     0,     0,     0,
       0,     0,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
       0,    41,    42,    43,    44,     0,    45,    46,    47,     0,
      48,    49,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    50,     0,     0,    51,    52,     0,    53,    54,     0,
      55,     0,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    65,    66,    67,     0,     0,  -642,     0,     0,
       0,     0,     0,     0,  -642,     0,   252,  -642,     5,     6,
       7,     8,     9,     0,     0,  -642,    10,    11,     0,     0,
       0,    12,     0,    13,    14,    15,    16,    17,    18,    19,
       0,     0,     0,     0,     0,    20,    21,    22,    23,    24,
      25,    26,     0,     0,    27,     0,     0,     0,     0,     0,
      28,    29,   253,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,     0,    41,    42,    43,    44,     0,    45,
      46,    47,     0,    48,    49,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    50,     0,     0,    51,    52,     0,
      53,    54,     0,    55,     0,     0,    56,    57,    58,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,   252,     0,     5,     6,     7,     8,     9,
       0,     0,     0,    10,    11,    65,    66,    67,    12,     0,
      13,    14,    15,    16,    17,    18,    19,  -642,     0,     0,
    -642,     0,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,    28,    29,   253,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
       0,    41,    42,    43,    44,     0,    45,    46,    47,     0,
      48,    49,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    50,     0,     0,    51,    52,     0,    53,    54,     0,
      55,     0,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,     0,  -642,     0,     0,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     0,     9,     0,     0,     0,
      10,    11,    65,    66,    67,    12,     0,    13,    14,    15,
      16,    17,    18,    19,  -642,     0,     0,  -642,     0,    20,
      21,    22,    23,    24,    25,    26,     0,     0,   203,     0,
       0,     0,     0,     0,     0,    29,     0,     0,    32,    33,
      34,    35,    36,    37,    38,    39,    40,   204,    41,    42,
      43,    44,     0,    45,    46,    47,     0,    48,    49,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   205,     0,
       0,   206,    52,     0,    53,    54,     0,   207,   208,   209,
      56,    57,   210,    59,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     5,
       6,     7,     0,     9,     0,     0,     0,    10,    11,    65,
     211,    67,    12,     0,    13,    14,    15,    16,    17,    18,
      19,     0,     0,     0,   235,     0,    20,    21,    22,    23,
      24,    25,    26,     0,     0,    27,     0,     0,     0,     0,
       0,     0,    29,     0,     0,    32,    33,    34,    35,    36,
      37,    38,    39,    40,     0,    41,    42,    43,    44,     0,
      45,    46,    47,     0,    48,    49,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   205,     0,     0,   206,    52,
       0,    53,    54,     0,     0,     0,     0,    56,    57,    58,
      59,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     0,
       9,     0,     0,     0,    10,    11,    65,    66,    67,    12,
       0,    13,    14,    15,    16,    17,    18,    19,   305,     0,
       0,   306,     0,    20,    21,    22,    23,    24,    25,    26,
       0,     0,    27,     0,     0,     0,     0,     0,     0,    29,
       0,     0,    32,    33,    34,    35,    36,    37,    38,    39,
      40,     0,    41,    42,    43,    44,     0,    45,    46,    47,
       0,    48,    49,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   205,     0,     0,   206,    52,     0,    53,    54,
       0,     0,     0,     0,    56,    57,    58,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     5,     6,     7,     8,     9,     0,     0,
       0,    10,    11,    65,    66,    67,    12,     0,    13,    14,
      15,    16,    17,    18,    19,     0,     0,     0,   235,     0,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,     0,    41,
      42,    43,    44,     0,    45,    46,    47,     0,    48,    49,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    50,
       0,     0,    51,    52,     0,    53,    54,     0,    55,     0,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       5,     6,     7,     8,     9,     0,     0,     0,    10,    11,
      65,    66,    67,    12,     0,    13,    14,    15,    16,    17,
      18,    19,   502,     0,     0,     0,     0,    20,    21,    22,
      23,    24,    25,    26,     0,     0,    27,     0,     0,     0,
       0,     0,    28,    29,   253,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,     0,    41,    42,    43,    44,
       0,    45,    46,    47,     0,    48,    49,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    50,     0,     0,    51,
      52,     0,    53,    54,     0,    55,     0,     0,    56,    57,
      58,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    65,    66,    67,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   502,
     121,   122,   123,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,   134,   135,   136,   137,   138,   139,   140,
     141,   142,   143,   144,     0,     0,     0,   145,   146,   147,
     389,   390,   391,   392,   152,   153,   154,     0,     0,     0,
       0,     0,   155,   156,   157,   158,   393,   394,   395,   396,
     163,    37,    38,   397,    40,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   165,   166,   167,   168,   169,
     170,   171,   172,   173,     0,     0,   174,   175,     0,     0,
     176,   177,   178,   179,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   180,   181,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   182,   183,   184,
     185,   186,   187,   188,   189,   190,   191,     0,   192,   193,
       0,     0,     0,     0,     0,   194,   398,   121,   122,   123,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
     134,   135,   136,   137,   138,   139,   140,   141,   142,   143,
     144,     0,     0,     0,   145,   146,   147,   148,   149,   150,
     151,   152,   153,   154,     0,     0,     0,     0,     0,   155,
     156,   157,   158,   159,   160,   161,   162,   163,   284,   285,
     164,   286,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   165,   166,   167,   168,   169,   170,   171,   172,
     173,     0,     0,   174,   175,     0,     0,   176,   177,   178,
     179,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   180,   181,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   182,   183,   184,   185,   186,   187,
     188,   189,   190,   191,     0,   192,   193,     0,     0,     0,
       0,     0,   194,   121,   122,   123,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,   134,   135,   136,   137,
     138,   139,   140,   141,   142,   143,   144,     0,     0,     0,
     145,   146,   147,   148,   149,   150,   151,   152,   153,   154,
       0,     0,     0,     0,     0,   155,   156,   157,   158,   159,
     160,   161,   162,   163,   237,     0,   164,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   165,   166,
     167,   168,   169,   170,   171,   172,   173,     0,     0,   174,
     175,     0,     0,   176,   177,   178,   179,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   180,   181,
       0,     0,    57,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     182,   183,   184,   185,   186,   187,   188,   189,   190,   191,
       0,   192,   193,     0,     0,     0,     0,     0,   194,   121,
     122,   123,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,   134,   135,   136,   137,   138,   139,   140,   141,
     142,   143,   144,     0,     0,     0,   145,   146,   147,   148,
     149,   150,   151,   152,   153,   154,     0,     0,     0,     0,
       0,   155,   156,   157,   158,   159,   160,   161,   162,   163,
       0,     0,   164,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   165,   166,   167,   168,   169,   170,
     171,   172,   173,     0,     0,   174,   175,     0,     0,   176,
     177,   178,   179,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   180,   181,     0,     0,    57,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   182,   183,   184,   185,
     186,   187,   188,   189,   190,   191,     0,   192,   193,     0,
       0,     0,     0,     0,   194,   121,   122,   123,   124,   125,
     126,   127,   128,   129,   130,   131,   132,   133,   134,   135,
     136,   137,   138,   139,   140,   141,   142,   143,   144,     0,
       0,     0,   145,   146,   147,   148,   149,   150,   151,   152,
     153,   154,     0,     0,     0,     0,     0,   155,   156,   157,
     158,   159,   160,   161,   162,   163,     0,     0,   164,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     165,   166,   167,   168,   169,   170,   171,   172,   173,     0,
       0,   174,   175,     0,     0,   176,   177,   178,   179,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     180,   181,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   182,   183,   184,   185,   186,   187,   188,   189,
     190,   191,     0,   192,   193,     5,     6,     7,     0,     9,
     194,     0,     0,    10,    11,     0,     0,     0,    12,     0,
      13,    14,    15,   242,   243,    18,    19,     0,     0,     0,
       0,     0,   244,   245,   246,    23,    24,    25,    26,     0,
       0,   203,     0,     0,     0,     0,     0,     0,   272,     0,
       0,    32,    33,    34,    35,    36,    37,    38,    39,    40,
       0,    41,    42,    43,    44,     0,    45,    46,    47,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   273,     0,     0,   206,    52,     0,    53,    54,     0,
       0,     0,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     5,     6,     7,     0,     9,
       0,     0,   274,    10,    11,     0,     0,     0,    12,   275,
      13,    14,    15,   242,   243,    18,    19,     0,     0,     0,
       0,     0,   244,   245,   246,    23,    24,    25,    26,     0,
       0,   203,     0,     0,     0,     0,     0,     0,   272,     0,
       0,    32,    33,    34,    35,    36,    37,    38,    39,    40,
       0,    41,    42,    43,    44,     0,    45,    46,    47,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   273,     0,     0,   206,    52,     0,    53,    54,     0,
       0,     0,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     5,     6,     7,     8,     9,
       0,     0,   274,    10,    11,     0,     0,     0,    12,   523,
      13,    14,    15,    16,    17,    18,    19,     0,     0,     0,
       0,     0,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
       0,    41,    42,    43,    44,     0,    45,    46,    47,     0,
      48,    49,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    50,     0,     0,    51,    52,     0,    53,    54,     0,
      55,     0,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     0,     9,     0,     0,     0,
      10,    11,    65,    66,    67,    12,     0,    13,    14,    15,
      16,    17,    18,    19,     0,     0,     0,     0,     0,    20,
      21,    22,    23,    24,    25,    26,     0,     0,   203,     0,
       0,     0,     0,     0,     0,    29,     0,     0,    32,    33,
      34,    35,    36,    37,    38,    39,    40,   204,    41,    42,
      43,    44,     0,    45,    46,    47,     0,    48,    49,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   205,     0,
       0,   206,    52,     0,    53,    54,     0,   207,   208,   209,
      56,    57,   210,    59,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     5,
       6,     7,     8,     9,     0,     0,     0,    10,    11,    65,
     211,    67,    12,     0,    13,    14,    15,    16,    17,    18,
      19,     0,     0,     0,     0,     0,    20,    21,    22,    23,
      24,    25,    26,     0,     0,    27,     0,     0,     0,     0,
       0,    28,    29,     0,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,     0,    41,    42,    43,    44,     0,
      45,    46,    47,     0,    48,    49,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    50,     0,     0,    51,    52,
       0,    53,    54,     0,    55,     0,     0,    56,    57,    58,
      59,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     0,
       9,     0,     0,     0,    10,    11,    65,    66,    67,    12,
       0,    13,    14,    15,   242,   243,    18,    19,     0,     0,
       0,     0,     0,   244,   245,   246,    23,    24,    25,    26,
       0,     0,   203,     0,     0,     0,     0,     0,     0,    29,
       0,     0,    32,    33,    34,    35,    36,    37,    38,    39,
      40,   204,    41,    42,    43,    44,     0,    45,    46,    47,
       0,    48,    49,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   205,     0,     0,   206,    52,     0,    53,    54,
       0,   618,   208,   209,    56,    57,   210,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     5,     6,     7,     0,     9,     0,     0,
       0,    10,    11,    65,   211,    67,    12,     0,    13,    14,
      15,   242,   243,    18,    19,     0,     0,     0,     0,     0,
     244,   245,   246,    23,    24,    25,    26,     0,     0,   203,
       0,     0,     0,     0,     0,     0,    29,     0,     0,    32,
      33,    34,    35,    36,    37,    38,    39,    40,   204,    41,
      42,    43,    44,     0,    45,    46,    47,     0,    48,    49,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   205,
       0,     0,   206,    52,     0,    53,    54,     0,   207,   208,
       0,    56,    57,   210,    59,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       5,     6,     7,     0,     9,     0,     0,     0,    10,    11,
      65,   211,    67,    12,     0,    13,    14,    15,   242,   243,
      18,    19,     0,     0,     0,     0,     0,   244,   245,   246,
      23,    24,    25,    26,     0,     0,   203,     0,     0,     0,
       0,     0,     0,    29,     0,     0,    32,    33,    34,    35,
      36,    37,    38,    39,    40,   204,    41,    42,    43,    44,
       0,    45,    46,    47,     0,    48,    49,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   205,     0,     0,   206,
      52,     0,    53,    54,     0,     0,   208,   209,    56,    57,
     210,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     5,     6,     7,
       0,     9,     0,     0,     0,    10,    11,    65,   211,    67,
      12,     0,    13,    14,    15,   242,   243,    18,    19,     0,
       0,     0,     0,     0,   244,   245,   246,    23,    24,    25,
      26,     0,     0,   203,     0,     0,     0,     0,     0,     0,
      29,     0,     0,    32,    33,    34,    35,    36,    37,    38,
      39,    40,   204,    41,    42,    43,    44,     0,    45,    46,
      47,     0,    48,    49,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   205,     0,     0,   206,    52,     0,    53,
      54,     0,   618,   208,     0,    56,    57,   210,    59,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     0,     9,     0,
       0,     0,    10,    11,    65,   211,    67,    12,     0,    13,
      14,    15,   242,   243,    18,    19,     0,     0,     0,     0,
       0,   244,   245,   246,    23,    24,    25,    26,     0,     0,
     203,     0,     0,     0,     0,     0,     0,    29,     0,     0,
      32,    33,    34,    35,    36,    37,    38,    39,    40,   204,
      41,    42,    43,    44,     0,    45,    46,    47,     0,    48,
      49,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     205,     0,     0,   206,    52,     0,    53,    54,     0,     0,
     208,     0,    56,    57,   210,    59,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     5,     6,     7,     0,     9,     0,     0,     0,    10,
      11,    65,   211,    67,    12,     0,    13,    14,    15,    16,
      17,    18,    19,     0,     0,     0,     0,     0,    20,    21,
      22,    23,    24,    25,    26,     0,     0,   203,     0,     0,
       0,     0,     0,     0,    29,     0,     0,    32,    33,    34,
      35,    36,    37,    38,    39,    40,     0,    41,    42,    43,
      44,     0,    45,    46,    47,     0,    48,    49,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   205,     0,     0,
     206,    52,     0,    53,    54,     0,   516,     0,     0,    56,
      57,    58,    59,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     0,     9,     0,     0,     0,    10,    11,    65,   211,
      67,    12,     0,    13,    14,    15,   242,   243,    18,    19,
       0,     0,     0,     0,     0,   244,   245,   246,    23,    24,
      25,    26,     0,     0,   203,     0,     0,     0,     0,     0,
       0,    29,     0,     0,    32,    33,    34,    35,    36,    37,
      38,    39,    40,     0,    41,    42,    43,    44,     0,    45,
      46,    47,     0,    48,    49,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   205,     0,     0,   206,    52,     0,
      53,    54,     0,   207,     0,     0,    56,    57,    58,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     5,     6,     7,     0,     9,
       0,     0,     0,    10,    11,    65,   211,    67,    12,     0,
      13,    14,    15,   242,   243,    18,    19,     0,     0,     0,
       0,     0,   244,   245,   246,    23,    24,    25,    26,     0,
       0,   203,     0,     0,     0,     0,     0,     0,    29,     0,
       0,    32,    33,    34,    35,    36,    37,    38,    39,    40,
       0,    41,    42,    43,    44,     0,    45,    46,    47,     0,
      48,    49,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   205,     0,     0,   206,    52,     0,    53,    54,     0,
     814,     0,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     0,     9,     0,     0,     0,
      10,    11,    65,   211,    67,    12,     0,    13,    14,    15,
     242,   243,    18,    19,     0,     0,     0,     0,     0,   244,
     245,   246,    23,    24,    25,    26,     0,     0,   203,     0,
       0,     0,     0,     0,     0,    29,     0,     0,    32,    33,
      34,    35,    36,    37,    38,    39,    40,     0,    41,    42,
      43,    44,     0,    45,    46,    47,     0,    48,    49,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   205,     0,
       0,   206,    52,     0,    53,    54,     0,   516,     0,     0,
      56,    57,    58,    59,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     5,
       6,     7,     0,     9,     0,     0,     0,    10,    11,    65,
     211,    67,    12,     0,    13,    14,    15,   242,   243,    18,
      19,     0,     0,     0,     0,     0,   244,   245,   246,    23,
      24,    25,    26,     0,     0,   203,     0,     0,     0,     0,
       0,     0,    29,     0,     0,    32,    33,    34,    35,    36,
      37,    38,    39,    40,     0,    41,    42,    43,    44,     0,
      45,    46,    47,     0,    48,    49,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   205,     0,     0,   206,    52,
       0,    53,    54,     0,   618,     0,     0,    56,    57,    58,
      59,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     0,
       9,     0,     0,     0,    10,    11,    65,   211,    67,    12,
       0,    13,    14,    15,   242,   243,    18,    19,     0,     0,
       0,     0,     0,   244,   245,   246,    23,    24,    25,    26,
       0,     0,   203,     0,     0,     0,     0,     0,     0,    29,
       0,     0,    32,    33,    34,    35,    36,    37,    38,    39,
      40,     0,    41,    42,    43,    44,     0,    45,    46,    47,
       0,    48,    49,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   205,     0,     0,   206,    52,     0,    53,    54,
       0,     0,     0,     0,    56,    57,    58,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     5,     6,     7,     0,     9,     0,     0,
       0,    10,    11,    65,   211,    67,    12,     0,    13,    14,
      15,    16,    17,    18,    19,     0,     0,     0,     0,     0,
      20,    21,    22,    23,    24,    25,    26,     0,     0,   203,
       0,     0,     0,     0,     0,     0,    29,     0,     0,    32,
      33,    34,    35,    36,    37,    38,    39,    40,     0,    41,
      42,    43,    44,     0,    45,    46,    47,     0,    48,    49,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   205,
       0,     0,   206,    52,     0,    53,    54,     0,     0,     0,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       5,     6,     7,     0,     9,     0,     0,     0,    10,    11,
      65,   211,    67,    12,     0,    13,    14,    15,    16,    17,
      18,    19,     0,     0,     0,     0,     0,    20,    21,    22,
      23,    24,    25,    26,     0,     0,    27,     0,     0,     0,
       0,     0,     0,    29,     0,     0,    32,    33,    34,    35,
      36,    37,    38,    39,    40,     0,    41,    42,    43,    44,
       0,    45,    46,    47,     0,    48,    49,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   205,     0,     0,   206,
      52,     0,    53,    54,     0,     0,     0,     0,    56,    57,
      58,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     5,     6,     7,
       0,     9,     0,     0,     0,    10,    11,    65,    66,    67,
      12,     0,    13,    14,    15,   242,   243,    18,    19,     0,
       0,     0,     0,     0,   244,   245,   246,    23,    24,    25,
      26,     0,     0,   203,     0,     0,     0,     0,     0,     0,
     272,     0,     0,    32,    33,    34,    35,    36,    37,    38,
      39,    40,     0,    41,    42,    43,    44,     0,    45,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   273,     0,     0,   326,    52,     0,    53,
      54,     0,   327,     0,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       5,     6,     7,     0,     9,     0,     0,     0,    10,    11,
       0,     0,     0,    12,   274,    13,    14,    15,   242,   243,
      18,    19,     0,     0,     0,     0,     0,   244,   245,   246,
      23,    24,    25,    26,     0,     0,   203,     0,     0,     0,
       0,     0,     0,   272,     0,     0,    32,    33,    34,    35,
      36,    37,    38,    39,    40,     0,    41,    42,    43,    44,
       0,    45,    46,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   370,     0,     0,    51,
      52,     0,    53,    54,     0,    55,     0,     0,    56,    57,
      58,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     5,     6,     7,     0,     9,     0,     0,
       0,    10,    11,     0,     0,     0,    12,   274,    13,    14,
      15,   242,   243,    18,    19,     0,     0,     0,     0,     0,
     244,   245,   246,    23,    24,    25,    26,     0,     0,   203,
       0,     0,     0,     0,     0,     0,   272,     0,     0,    32,
      33,    34,   378,    36,    37,    38,   379,    40,     0,    41,
      42,    43,    44,     0,    45,    46,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   380,     0,     0,     0,   381,
       0,     0,   206,    52,     0,    53,    54,     0,     0,     0,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     0,
       9,     0,     0,     0,    10,    11,     0,     0,     0,    12,
     274,    13,    14,    15,   242,   243,    18,    19,     0,     0,
       0,     0,     0,   244,   245,   246,    23,    24,    25,    26,
       0,     0,   203,     0,     0,     0,     0,     0,     0,   272,
       0,     0,    32,    33,    34,   378,    36,    37,    38,   379,
      40,     0,    41,    42,    43,    44,     0,    45,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   381,     0,     0,   206,    52,     0,    53,    54,
       0,     0,     0,     0,    56,    57,    58,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     5,
       6,     7,     0,     9,     0,     0,     0,    10,    11,     0,
       0,     0,    12,   274,    13,    14,    15,   242,   243,    18,
      19,     0,     0,     0,     0,     0,   244,   245,   246,    23,
      24,    25,    26,     0,     0,   203,     0,     0,     0,     0,
       0,     0,   272,     0,     0,    32,    33,    34,    35,    36,
      37,    38,    39,    40,     0,    41,    42,    43,    44,     0,
      45,    46,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   273,     0,     0,   326,    52,
       0,    53,    54,     0,     0,     0,     0,    56,    57,    58,
      59,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     0,     9,     0,     0,     0,
      10,    11,     0,     0,     0,    12,   274,    13,    14,    15,
     242,   243,    18,    19,     0,     0,     0,     0,     0,   244,
     245,   246,    23,    24,    25,    26,     0,     0,   203,     0,
       0,     0,     0,     0,     0,   272,     0,     0,    32,    33,
      34,    35,    36,    37,    38,    39,    40,     0,    41,    42,
      43,    44,     0,    45,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   965,     0,
       0,   206,    52,     0,    53,    54,     0,     0,     0,     0,
      56,    57,    58,    59,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     5,     6,     7,     0,     9,
       0,     0,     0,    10,    11,     0,     0,     0,    12,   274,
      13,    14,    15,   242,   243,    18,    19,     0,     0,     0,
       0,     0,   244,   245,   246,    23,    24,    25,    26,     0,
       0,   203,     0,     0,     0,     0,     0,     0,   272,     0,
       0,    32,    33,    34,    35,    36,    37,    38,    39,    40,
       0,    41,    42,    43,    44,     0,    45,    46,    47,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1005,     0,     0,   206,    52,     0,    53,    54,     0,
       0,     0,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,   622,   563,     0,
       0,   623,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   274,   165,   166,   167,   168,   169,   170,   171,
     172,   173,     0,     0,   174,   175,     0,     0,   176,   177,
     178,   179,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   180,   181,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   182,   183,   184,   185,   186,
     187,   188,   189,   190,   191,     0,   192,   193,   625,   571,
       0,     0,   626,   194,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   165,   166,   167,   168,   169,   170,
     171,   172,   173,     0,     0,   174,   175,     0,     0,   176,
     177,   178,   179,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   180,   181,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   182,   183,   184,   185,
     186,   187,   188,   189,   190,   191,     0,   192,   193,   649,
     563,     0,     0,   650,   194,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   165,   166,   167,   168,   169,
     170,   171,   172,   173,     0,     0,   174,   175,     0,     0,
     176,   177,   178,   179,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   180,   181,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   182,   183,   184,
     185,   186,   187,   188,   189,   190,   191,     0,   192,   193,
     652,   571,     0,     0,   653,   194,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   165,   166,   167,   168,
     169,   170,   171,   172,   173,     0,     0,   174,   175,     0,
       0,   176,   177,   178,   179,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   180,   181,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   182,   183,
     184,   185,   186,   187,   188,   189,   190,   191,     0,   192,
     193,   740,   563,     0,     0,   741,   194,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   165,   166,   167,
     168,   169,   170,   171,   172,   173,     0,     0,   174,   175,
       0,     0,   176,   177,   178,   179,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   180,   181,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   182,
     183,   184,   185,   186,   187,   188,   189,   190,   191,     0,
     192,   193,   743,   571,     0,     0,   744,   194,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   165,   166,
     167,   168,   169,   170,   171,   172,   173,     0,     0,   174,
     175,     0,     0,   176,   177,   178,   179,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   180,   181,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     182,   183,   184,   185,   186,   187,   188,   189,   190,   191,
       0,   192,   193,   749,   563,     0,     0,   750,   194,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   165,
     166,   167,   168,   169,   170,   171,   172,   173,     0,     0,
     174,   175,     0,     0,   176,   177,   178,   179,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   180,
     181,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   182,   183,   184,   185,   186,   187,   188,   189,   190,
     191,     0,   192,   193,   819,   563,     0,     0,   820,   194,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     165,   166,   167,   168,   169,   170,   171,   172,   173,     0,
       0,   174,   175,     0,     0,   176,   177,   178,   179,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     180,   181,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   182,   183,   184,   185,   186,   187,   188,   189,
     190,   191,     0,   192,   193,   822,   571,     0,     0,   823,
     194,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   165,   166,   167,   168,   169,   170,   171,   172,   173,
       0,     0,   174,   175,     0,     0,   176,   177,   178,   179,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   180,   181,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   182,   183,   184,   185,   186,   187,   188,
     189,   190,   191,     0,   192,   193,  1047,   563,     0,     0,
    1048,   194,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   165,   166,   167,   168,   169,   170,   171,   172,
     173,     0,     0,   174,   175,     0,     0,   176,   177,   178,
     179,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   180,   181,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   182,   183,   184,   185,   186,   187,
     188,   189,   190,   191,     0,   192,   193,  1050,   571,     0,
       0,  1051,   194,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   165,   166,   167,   168,   169,   170,   171,
     172,   173,     0,     0,   174,   175,     0,     0,   176,   177,
     178,   179,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   180,   181,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   182,   183,   184,   185,   186,
     187,   188,   189,   190,   191,     0,   192,   193,  1064,   563,
       0,     0,  1065,   194,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   165,   166,   167,   168,   169,   170,
     171,   172,   173,     0,     0,   174,   175,     0,     0,   176,
     177,   178,   179,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   180,   181,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   182,   183,   184,   185,
     186,   187,   188,   189,   190,   191,     0,   192,   193,     0,
       0,     0,     0,     0,   194
};

static const yytype_int16 yycheck[] =
{
       2,    57,    27,    86,    29,    69,    98,    66,   364,    22,
      81,    16,    17,     8,    97,    20,    28,   418,   584,     4,
     477,   329,     8,    16,    17,   426,   358,    20,    92,    55,
     473,    89,   426,    28,    92,    65,     8,   583,   357,   796,
      96,   360,    28,   787,   266,     2,    55,     4,   270,    76,
      92,    53,    54,    51,    69,    81,    28,   874,   601,     2,
      53,     4,    57,    76,   446,   224,   787,    93,    94,    95,
     464,    51,   728,    16,    17,   796,   732,    20,    26,   381,
     523,   667,   407,   408,   876,   404,    37,    38,   935,   624,
     422,    25,   254,    26,    29,    13,     0,    25,   680,    87,
      88,    96,   421,    63,   423,    16,    17,    13,    51,    20,
      63,    78,    55,    88,   931,    88,   651,    88,    88,    13,
     329,    16,    17,    66,   716,    20,   137,    88,    25,    13,
     449,   723,    87,    88,   145,  1010,   219,   140,    81,    87,
      88,    90,   145,   475,   145,    88,    89,   896,   477,    92,
      93,    94,    95,   113,    25,   122,   144,   476,    53,    54,
     113,    25,    37,    38,   326,    13,   249,    26,   117,   144,
     987,   144,    25,   144,   144,   896,    88,  1024,    88,   114,
      25,   136,   939,   144,    13,  1060,   134,    25,   136,   144,
      25,   139,   256,   985,   145,   278,   144,    13,   407,   408,
     944,   134,   951,   137,   275,   748,  1023,   742,   206,   137,
     667,   139,    13,   137,   256,   134,   218,   752,   939,   137,
     247,   145,   224,   228,   142,   230,   231,   145,    56,   231,
     631,   632,   144,   139,   144,   228,   142,   230,   632,   145,
     137,   256,   139,   241,   274,   139,   661,   272,   142,   275,
      54,   145,   310,   311,   312,   313,   254,   589,   142,  1003,
     642,   145,   705,   206,   920,   324,   137,   599,    88,    89,
     329,   590,    92,   137,   254,   134,   435,   863,    13,   865,
     139,   600,  1003,    27,   137,   228,   821,   230,   231,  1010,
     373,   234,   137,   236,   142,   252,   598,   145,   241,   137,
      90,   327,   137,   367,    88,   887,   263,   365,   366,   252,
     856,   254,   234,   142,   857,    90,   145,   228,   327,   230,
     263,   864,   137,  1067,   309,   367,   142,   117,   326,   145,
     145,   324,   275,   228,    13,   230,   329,   259,   667,  1060,
     262,   142,   117,   290,   145,   292,   326,   294,   407,   408,
      63,   298,   309,   358,   920,    13,   358,   314,    90,    15,
     144,    17,   137,   778,   779,   360,   309,   310,   311,   312,
     313,   314,   315,   316,   139,   957,   958,   143,   710,    88,
     145,   324,   738,   326,   327,   117,   329,    88,   713,   714,
     709,   117,   711,    87,    88,   720,   721,   110,   142,   112,
     204,   145,   473,   207,   208,   209,   863,   142,   865,   404,
     145,   137,    90,    90,   234,   358,   236,   422,   361,   727,
     422,   143,   365,   366,   646,    87,    88,   143,   423,    90,
     143,   443,   434,   435,    28,   144,    25,   380,   143,   117,
     117,   984,   136,   144,   446,   139,   117,   473,   443,    25,
     144,   534,   523,    63,   449,   137,   117,   443,   787,   874,
    1042,   876,   621,   142,   407,   408,   145,   796,   415,   870,
     475,   443,    90,   475,   136,   142,   870,   100,   145,   422,
     117,   476,   144,   103,   142,    90,    25,   145,    63,   446,
     310,   311,   312,   313,    90,   315,   316,   523,   707,   117,
     110,   124,   112,   446,   713,   714,   428,   832,    87,    88,
     432,   720,   721,   456,  1049,   437,   931,   932,   582,   583,
     324,   117,    90,   418,   137,   329,   551,   139,   117,    56,
     473,   453,   475,   143,   863,   110,   865,   112,   113,   566,
     115,   117,    91,   490,   569,   365,   366,   574,   137,   117,
     497,   140,   908,    25,   567,    63,   145,   136,   914,    67,
     380,   137,   575,   139,   140,   144,   461,   896,    63,   145,
     985,   117,   987,   407,   408,   577,   647,    54,   117,   137,
     523,   579,   609,   581,   589,    14,    15,   589,    65,    66,
     424,   425,   585,    90,   599,   590,   609,   599,   137,   624,
     522,   140,   110,   615,   112,   600,   145,   609,  1023,   137,
     939,   770,    70,   417,   418,   110,   450,   112,   113,   621,
     615,   647,   426,   832,   960,   961,   651,   139,    52,   615,
      54,    55,    56,    57,   705,    52,   579,   896,   581,    56,
     642,   134,    70,   615,   140,   117,   589,    52,   707,    54,
      55,    56,    57,    90,   713,   714,   599,   461,   601,   602,
     464,   720,   721,   139,    90,   137,   758,   117,   140,   127,
     128,   129,   139,   145,  1003,   141,   678,    56,   680,   705,
     117,  1010,   746,   135,  1016,   642,   137,   745,    90,   747,
     717,   117,   635,    87,    88,   113,    70,   724,  1017,   642,
     756,  1037,  1038,   113,   647,   710,   113,  1043,   710,  1045,
    1046,   137,   516,   137,   113,   117,   711,   742,   137,    90,
     790,    97,   724,   645,   794,   727,   728,   752,   730,   142,
     732,  1060,   137,    90,   727,   137,   631,    63,  1074,  1075,
    1076,  1077,   136,    17,  1003,   586,   117,  1083,  1007,   706,
     144,  1010,    90,  1012,    56,   121,    26,    58,    25,   784,
     117,   756,   705,   706,   707,   140,   137,   710,   770,    70,
     713,   714,   137,   832,   137,    63,   135,   720,   721,   117,
     137,   585,   602,   134,   110,   137,   112,   113,   846,    87,
      88,   137,   856,   682,   858,   684,   821,   137,    99,   100,
     143,  1060,   745,  1062,   747,   748,   143,   809,  1067,   811,
    1069,    44,   616,   137,   618,   635,   137,    87,    88,     2,
    1079,     4,   110,   124,   112,   113,   896,   631,   632,   139,
      90,    90,   754,    16,    17,    70,   117,    20,   136,   841,
      10,     8,   844,   135,    13,   843,   144,   137,   117,   771,
      85,    86,   850,   851,   137,    52,   854,   117,   117,   137,
     782,   137,    52,   927,   134,   137,   136,    52,    51,   139,
      10,   896,   676,   135,   144,    10,   137,   137,   137,   713,
     714,   907,   119,    66,   967,   887,   720,   721,    70,   832,
     125,   126,   127,   128,   129,   141,   737,    15,   702,   897,
     843,   139,   900,   846,    70,    88,    89,   850,   851,    92,
     137,   854,   753,   911,   857,   135,   137,   135,   920,    85,
      86,   864,   137,   727,    26,   745,   951,   747,    91,   955,
       9,   765,   766,  1003,   768,   769,   137,  1007,   936,   937,
    1010,   140,  1012,   125,   126,   127,   128,   129,   142,   137,
     137,   873,   117,   137,   897,   957,   958,   900,   124,   125,
     126,   127,   128,   129,   907,   990,   137,   122,   911,   137,
      87,    88,   790,   971,   796,   973,  1002,   799,   137,   137,
     137,   979,   786,   137,   137,    87,    88,    56,   137,   135,
    1060,   122,  1062,   936,   937,   884,   885,  1067,   832,  1069,
     889,   999,   891,   925,   893,   137,   847,    56,   849,  1079,
     814,  1016,   955,   111,  1016,   137,  1018,  1019,   859,   136,
     139,   137,  1017,   206,  1049,   137,   846,   144,   971,   137,
     973,   456,   134,   139,   136,   869,   979,   139,   137,    70,
    1042,   984,   144,    93,    95,   228,   896,   230,   231,   853,
     724,   234,   811,   236,    85,    86,   999,  1019,   241,  1002,
     778,    87,    88,    87,    88,  1024,   870,   759,   699,   252,
     515,   254,  1018,  1016,   896,   298,   917,   918,   896,   101,
     263,   922,   210,    59,    96,   926,   928,    87,    88,  1003,
     790,   122,   123,   124,   125,   126,   127,   128,   129,    87,
      88,    87,    88,   787,    87,    88,   995,   996,   997,   998,
     136,  1007,   136,    -1,    87,    88,    -1,   939,   144,   941,
     144,    -1,    -1,    -1,   946,    -1,   309,   310,   311,   312,
     313,   314,   315,   316,    -1,    -1,   136,    -1,    87,    88,
      -1,   324,    -1,   326,   144,    -1,   329,    -1,   136,    -1,
     136,    -1,   993,   136,    -1,    -1,   144,    -1,   144,    -1,
      -1,   144,    -1,   136,    -1,    -1,    16,    17,    -1,  1058,
      20,   144,    63,    -1,    -1,   358,    -1,    -1,   361,  1020,
    1021,    -1,   365,   366,    -1,  1003,    -1,   136,  1010,  1007,
    1012,    -1,  1010,    -1,  1012,   144,    -1,   380,    48,    49,
      -1,    -1,    -1,    53,    54,    -1,    -1,  1029,    52,    -1,
      54,    55,    56,    57,    58,    63,    66,    67,    -1,   110,
      -1,   112,   113,    -1,   407,   408,    70,    52,    -1,    54,
      55,    56,    57,    58,    -1,    -1,    -1,    -1,  1060,   422,
    1062,    -1,  1060,    -1,  1062,    70,    -1,  1069,    92,  1067,
      -1,  1069,    -1,    -1,    98,    99,   100,  1079,    -1,    -1,
      -1,  1079,   110,   446,   112,   113,    52,    92,    54,    55,
      56,    57,    -1,   456,    99,   100,    -1,    -1,    54,    55,
     124,    57,    52,   127,    54,    55,    56,    57,     0,    65,
      66,    -1,   475,    -1,    -1,   139,     8,     9,    10,   124,
      -1,    13,    14,    15,    -1,    17,    92,    -1,    44,    -1,
      -1,    -1,    98,    -1,    26,    27,    40,    41,    42,    43,
      44,     2,    92,     4,    -1,    37,    38,    -1,    40,    41,
      42,    43,    44,    -1,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    -1,    -1,    85,
      86,    -1,    -1,    -1,   204,    -1,    -1,   207,   208,   209,
      -1,   211,    52,    -1,    54,    55,    56,    57,    -1,    -1,
      51,    -1,    -1,    -1,    55,    87,    88,    -1,   228,    -1,
     230,   231,   118,    -1,   120,   121,   122,   123,   124,   125,
     126,   127,   128,   129,    -1,    -1,   579,    -1,   581,   111,
      81,   137,    92,    -1,    -1,    -1,   589,    -1,    98,    -1,
      -1,    -1,    93,    94,    95,    -1,   599,    -1,   601,   602,
      -1,    -1,   134,   135,    -1,    -1,    -1,   139,   140,    -1,
     142,    -1,   144,   145,    52,    44,    54,    55,    56,    57,
      59,    60,    61,    62,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   635,    -1,    -1,    -1,    -1,    -1,    -1,   642,
      -1,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,   324,    -1,    85,    86,    -1,   329,
     330,   331,   332,   333,   334,   335,   336,   337,   338,   339,
     340,   341,   342,   343,   344,   345,   346,   347,   348,   349,
     350,   351,   352,   353,   354,   355,    -1,    -1,   358,   118,
      -1,   120,   121,   122,   123,   124,   125,   126,   127,   128,
     129,    -1,    -1,   706,   707,   206,    -1,   710,    -1,    -1,
     713,   714,    -1,    -1,    -1,    -1,    -1,   720,   721,    -1,
      -1,    52,    -1,    54,    55,    56,    57,    58,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   407,   408,    70,
     241,    -1,   745,    -1,   747,   748,   416,   417,   418,    -1,
      -1,   252,   422,   254,   424,   425,   426,    -1,    -1,    -1,
      -1,    92,   263,    -1,    -1,    -1,    -1,    98,    99,   100,
      -1,    -1,    -1,    -1,   275,   445,    -1,    -1,    -1,    -1,
     450,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   461,    -1,   124,   464,    -1,   127,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   475,    -1,    -1,   309,    -1,
      -1,    -1,    -1,   314,   145,    -1,    52,    -1,    54,    55,
      56,    57,    58,    -1,    -1,   326,   327,    -1,   329,   832,
      -1,   501,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,
     843,    -1,    -1,   846,    -1,    -1,   516,   850,   851,    -1,
      -1,   854,    -1,    -1,   857,    -1,    92,    -1,    -1,    -1,
     361,   864,    98,    99,   100,     2,    -1,     4,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,    -1,
      -1,   127,    -1,    -1,   897,    -1,    -1,   900,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   407,   408,   911,    -1,
      -1,    -1,    -1,    -1,    51,   585,    -1,    -1,    55,   589,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   599,
      -1,    -1,    -1,   936,   937,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    81,   446,   616,    -1,   618,    -1,
      -1,    -1,    -1,    -1,    -1,   456,    93,    94,    95,    96,
      -1,   631,   632,    -1,    -1,    -1,    -1,    -1,   971,    -1,
     973,    -1,   473,    -1,    -1,    -1,   979,    -1,    -1,    -1,
      -1,   984,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   999,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   676,    -1,    -1,    -1,
      -1,    -1,    -1,  1016,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   523,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   702,    -1,    -1,    -1,    -1,   707,   708,    -1,
     710,    -1,    -1,   713,   714,    -1,    -1,    -1,    -1,    -1,
     720,   721,    -1,    -1,    -1,    -1,    -1,   727,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   206,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   579,    -1,
     581,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   765,   766,    -1,   768,   769,
     601,    -1,    -1,    -1,   241,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   252,   786,   254,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   263,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   275,    -1,
      -1,   642,    -1,    -1,   814,    -1,   647,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   824,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   832,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   309,    -1,    -1,    -1,    -1,   314,    -1,     2,
      -1,     4,    -1,   853,    -1,    -1,    -1,   688,    -1,   326,
     327,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   869,
     870,    -1,    -1,    -1,   705,   706,   707,    -1,    -1,    -1,
      -1,    -1,   713,   714,    -1,    -1,    -1,    52,    53,   720,
     721,    56,    -1,    -1,   361,    -1,    -1,    -1,    51,    -1,
      -1,    -1,    55,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    -1,    -1,    79,    80,    -1,   748,    83,    84,
      85,    86,    -1,    -1,    -1,    -1,    -1,    -1,    81,    -1,
      -1,    -1,    -1,    98,    99,    -1,    -1,    -1,    -1,    -1,
      93,    94,    95,    96,    -1,     2,    -1,     4,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,    -1,   131,   132,    -1,    -1,
      -1,    -1,    -1,   138,    -1,    -1,    -1,    -1,    -1,   446,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   456,
      -1,    -1,    -1,    -1,    51,    -1,    -1,    -1,    -1,    -1,
      -1,   832,    -1,     2,    -1,     4,   473,    -1,    -1,    -1,
      -1,    -1,   843,    -1,    -1,    -1,  1016,    -1,    -1,   850,
     851,    -1,    -1,   854,    -1,    -1,   857,    -1,    -1,    -1,
      -1,    -1,    -1,   864,    -1,    -1,    93,    -1,    -1,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    51,   206,    85,    86,   523,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   897,    -1,    -1,   900,
      -1,    -1,    -1,    -1,    -1,    -1,   907,    -1,    -1,    -1,
     911,    -1,    -1,    -1,    -1,    -1,    -1,   118,   241,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,   252,
      -1,   254,    -1,    -1,    -1,   936,   937,    -1,    -1,    -1,
     263,    -1,   579,    -1,   581,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   275,    -1,   955,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   601,    -1,    -1,    -1,    -1,    -1,
     971,    -1,   973,    -1,    -1,    -1,    -1,    -1,   979,   206,
      -1,    -1,    -1,   984,    -1,    -1,   309,    -1,    -1,    -1,
      -1,   314,    -1,    -1,    -1,    -1,    -1,    -1,   999,    -1,
      -1,  1002,    -1,   326,   327,   642,    -1,    -1,    -1,    -1,
     647,    -1,    -1,    -1,   241,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   252,    -1,   254,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   263,   206,   361,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   705,   706,
      -1,    -1,   241,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   309,   252,    -1,   254,    -1,   314,    -1,    -1,
      -1,    -1,    -1,    -1,   263,    -1,    -1,    52,    53,   326,
      -1,    56,   329,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   748,    -1,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    -1,   446,    79,    80,    -1,    -1,    83,    84,
      85,    86,    -1,   456,   361,    -1,    -1,    -1,    -1,    -1,
     309,    -1,    -1,    98,    99,   314,    -1,    -1,    -1,    -1,
     473,    -1,    -1,    -1,    -1,    -1,    -1,   326,    -1,    -1,
     329,    -1,    -1,    -1,    -1,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,    -1,   131,   132,    -1,    -1,
     407,   408,    -1,   138,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   361,    70,    71,    72,    73,    74,    75,    76,
     523,    -1,    79,    80,    -1,    -1,   843,    -1,    85,    86,
      -1,    -1,    -1,   850,   851,    -1,    -1,   854,    -1,   446,
     857,    -1,    -1,    -1,    -1,    -1,    -1,   864,    -1,   456,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   407,   408,
      -1,    -1,    -1,   120,   121,   122,   123,   124,   125,   126,
     127,   128,   129,    -1,    -1,    -1,   579,    -1,   581,    -1,
     897,    -1,    -1,   900,    -1,    -1,    -1,    -1,    -1,    -1,
     907,    -1,    -1,    -1,   911,    -1,    -1,   446,   601,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   456,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   936,
     937,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   955,   642,
      -1,    -1,    -1,    -1,   647,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   971,    -1,   973,    -1,    -1,    -1,
      -1,    -1,   979,    -1,    -1,    -1,    -1,   984,    -1,    -1,
      -1,    -1,   579,    -1,   581,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   999,    -1,    -1,  1002,    -1,    -1,    -1,    -1,
      -1,    52,    53,    -1,   601,    56,    -1,    -1,    -1,    -1,
      -1,    -1,   705,   706,    -1,    -1,    -1,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    -1,    -1,    79,    80,
      -1,    -1,    83,    84,    85,    86,    -1,    -1,    -1,    -1,
     579,    -1,   581,    -1,    -1,   642,    -1,    98,    99,    -1,
      -1,    -1,    -1,    -1,    -1,   748,    -1,    -1,    -1,    -1,
      -1,    -1,   601,    -1,    -1,    -1,    -1,    -1,    -1,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,    -1,
     131,   132,    -1,    -1,    -1,    -1,    -1,   138,    -1,    -1,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,   642,    -1,    85,    86,    -1,    -1,   706,
     707,    -1,    -1,    -1,    -1,    -1,   713,   714,    -1,    -1,
      -1,    -1,    -1,   720,   721,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   118,    -1,
     120,   121,   122,   123,   124,   125,   126,   127,   128,   129,
     843,   748,    -1,    -1,    -1,    -1,    -1,   850,   851,    -1,
      -1,   854,    -1,    -1,   857,   145,    -1,   706,   707,    -1,
      -1,   864,    -1,    -1,   713,   714,    -1,    -1,    -1,    -1,
      -1,   720,   721,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   897,    -1,    -1,   900,    -1,   748,
      -1,    -1,    -1,    -1,   907,    -1,    -1,    -1,   911,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   832,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   936,   937,    -1,   843,    -1,    -1,    -1,
      -1,    -1,    -1,   850,   851,    -1,    -1,   854,    -1,    -1,
     857,    -1,   955,    -1,    -1,    -1,    -1,   864,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   971,    -1,
     973,    -1,    -1,    -1,    -1,    -1,   979,    -1,    -1,    -1,
      -1,   984,    -1,   832,    -1,    -1,    -1,    -1,    -1,    -1,
     897,    -1,    -1,   900,   843,    -1,   999,    -1,    -1,  1002,
     907,   850,   851,    -1,   911,   854,    -1,    -1,   857,    -1,
      -1,    -1,    -1,    -1,    -1,   864,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   936,
     937,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   897,    -1,
      -1,   900,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   911,    -1,   971,    -1,   973,    -1,    -1,    -1,
      -1,    -1,   979,    -1,    -1,    -1,    -1,   984,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   936,   937,    -1,
      -1,    -1,   999,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   971,    -1,   973,    -1,    -1,    -1,    -1,    -1,
     979,    -1,    -1,     0,     1,   984,     3,     4,     5,     6,
       7,    -1,    -1,    -1,    11,    12,    -1,    -1,    -1,    16,
     999,    18,    19,    20,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    -1,    59,    60,    61,    62,    -1,    64,    65,    66,
      -1,    68,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,
      -1,    98,    -1,    -1,   101,   102,   103,   104,   105,   106,
     107,   108,   109,     0,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     8,     9,    10,    -1,    -1,    13,    14,    15,    -1,
      17,    -1,    -1,   130,   131,   132,    -1,    -1,    -1,    26,
      27,    28,    29,    -1,    -1,   142,    -1,    -1,   145,    -1,
      37,    38,    -1,    40,    41,    42,    43,    44,    -1,    -1,
      -1,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    -1,    -1,    85,    86,    -1,    -1,
      -1,    -1,    -1,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    -1,    -1,    85,    86,
      87,    88,    -1,    90,    91,    -1,    -1,    -1,    -1,    -1,
      97,   120,   121,   122,   123,   124,   125,   126,   127,   128,
     129,    -1,    -1,    -1,   111,    -1,    -1,   114,    -1,    -1,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,   128,   129,    -1,    -1,    -1,    -1,   134,   135,   136,
     137,     0,    -1,   140,   141,   142,    -1,   144,   145,     8,
       9,    10,    -1,    -1,    13,    14,    15,    -1,    17,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    25,    -1,    27,    28,
      29,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,
      -1,    40,    41,    42,    43,    44,    -1,    -1,    -1,    70,
      71,    72,    73,    74,    75,    76,    77,    -1,    79,    80,
      -1,    -1,    -1,    -1,    85,    86,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    -1,    -1,    85,    86,    87,    88,
      -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    97,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,    -1,
      -1,    -1,   111,    -1,    -1,   114,    -1,    -1,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,   128,
     129,    -1,    -1,    -1,    -1,    -1,   135,   136,   137,     0,
      -1,   140,   141,   142,    -1,   144,   145,     8,     9,    10,
      -1,    -1,    13,    14,    15,    -1,    17,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    25,    -1,    27,    28,    29,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,
      41,    42,    43,    44,    -1,    -1,    -1,    70,    71,    72,
      73,    74,    75,    76,    -1,    -1,    79,    80,    -1,    -1,
      -1,    -1,    85,    86,    -1,    -1,    -1,    -1,    -1,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    -1,    -1,    85,    86,    87,    88,    -1,    90,
      91,    -1,    -1,    -1,    -1,    -1,    97,   120,   121,   122,
     123,   124,   125,   126,   127,   128,   129,    -1,    -1,    -1,
     111,    -1,    -1,   114,    -1,    -1,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,    -1,
      -1,    -1,    -1,    -1,   135,   136,   137,     0,    -1,   140,
     141,   142,    -1,   144,   145,     8,     9,    10,    -1,    -1,
      13,    14,    15,    -1,    17,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    26,    27,    28,    29,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,    42,
      43,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      -1,    -1,    85,    86,    87,    88,    -1,    -1,    91,    -1,
      -1,    -1,    -1,    -1,    97,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   111,    -1,
      -1,   114,    -1,    -1,    -1,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,   128,   129,    -1,    -1,    -1,
      -1,   134,   135,   136,   137,     0,    -1,   140,   141,   142,
      -1,   144,   145,     8,     9,    10,    -1,    -1,    13,    14,
      15,    -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    26,    27,    28,    29,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    37,    38,    -1,    40,    41,    42,    43,    44,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    -1,    -1,
      85,    86,    87,    88,    -1,    -1,    91,    -1,    -1,    -1,
      -1,    -1,    97,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   111,    -1,    -1,   114,
      -1,    -1,    -1,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,    -1,    -1,    -1,    -1,   134,
     135,   136,   137,     0,    -1,   140,   141,   142,    -1,   144,
     145,     8,     9,    10,    -1,    -1,    13,    14,    15,    -1,
      17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      27,    28,    29,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      37,    38,    -1,    40,    41,    42,    43,    44,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    -1,    -1,    85,    86,
      87,    88,    -1,    90,    91,    -1,    -1,    -1,    -1,    -1,
      97,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   111,    -1,    -1,   114,    -1,    -1,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,   128,   129,    -1,    -1,    -1,    -1,    -1,   135,   136,
     137,     0,    -1,   140,   141,   142,    -1,   144,   145,     8,
       9,    10,    -1,    -1,    13,    14,    15,    -1,    17,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    26,    27,    28,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,
      -1,    40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    -1,    -1,    85,    86,    87,    88,
      -1,    -1,    91,    -1,    -1,    -1,    -1,    -1,    97,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,   118,
      -1,   120,   121,   122,   123,   124,   125,   126,   127,   128,
     129,    -1,    -1,    -1,    -1,   134,   135,   136,   137,     0,
     139,   140,   141,   142,    -1,   144,   145,     8,     9,    10,
      -1,    -1,    13,    14,    15,    -1,    17,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    27,    28,    29,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,
      41,    42,    43,    44,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    -1,    -1,    85,    86,    87,    88,    -1,    -1,
      91,    -1,    -1,    -1,    -1,    -1,    97,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     111,    -1,    -1,   114,    -1,    -1,    -1,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,    -1,
      -1,    -1,    -1,    -1,   135,   136,   137,     0,    -1,   140,
     141,   142,    -1,   144,   145,     8,     9,    10,    -1,    -1,
      13,    14,    15,    -1,    17,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    26,    27,    28,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,    42,
      43,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      -1,    -1,    85,    86,    87,    88,    -1,    -1,    91,    -1,
      -1,    -1,    -1,    -1,    97,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   111,    -1,
      -1,    -1,    -1,    -1,    -1,   118,    -1,   120,   121,   122,
     123,   124,   125,   126,   127,   128,   129,    -1,    -1,    -1,
      -1,   134,   135,   136,   137,     0,   139,   140,   141,   142,
      -1,   144,   145,     8,     9,    10,    -1,    -1,    13,    14,
      15,    -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    27,    28,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    37,    38,    -1,    40,    41,    42,    43,    44,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    -1,    -1,
      85,    86,    87,    88,    -1,    -1,    91,    -1,    -1,    -1,
      -1,    -1,    97,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   111,    -1,    -1,    -1,
      -1,    -1,    -1,   118,    -1,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,    -1,    -1,    -1,    -1,    -1,
     135,   136,   137,     0,   139,   140,   141,   142,    -1,   144,
     145,     8,     9,    10,    -1,    -1,    -1,    14,    15,    -1,
      17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    26,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      37,    38,    -1,    40,    41,    42,    43,    44,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    -1,    -1,    85,    86,
      87,    88,    -1,    90,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   111,    -1,    -1,    -1,    -1,    -1,
     117,   118,    -1,   120,   121,   122,   123,   124,   125,   126,
     127,   128,   129,    -1,    -1,    -1,    -1,   134,   135,   136,
     137,     0,    -1,   140,    -1,   142,    -1,   144,   145,     8,
       9,    10,    -1,    -1,    -1,    14,    15,    -1,    17,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    26,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,
      -1,    40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    -1,    -1,    85,    86,    87,    88,
      -1,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   111,    -1,    -1,    -1,    -1,    -1,   117,   118,
      -1,   120,   121,   122,   123,   124,   125,   126,   127,   128,
     129,    -1,    -1,    -1,    -1,   134,   135,   136,   137,     0,
      -1,   140,    -1,   142,    -1,   144,   145,     8,     9,    10,
      -1,    -1,    -1,    14,    15,    -1,    17,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,
      41,    42,    43,    44,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    -1,    -1,    85,    86,    87,    88,    -1,    90,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     111,    -1,    -1,    -1,    -1,    -1,   117,   118,    -1,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,    -1,
      -1,    -1,    -1,    -1,   135,   136,   137,     0,    -1,   140,
      -1,   142,    -1,   144,   145,     8,     9,    10,    -1,    -1,
      -1,    14,    15,    -1,    17,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,    42,
      43,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      -1,    -1,    85,    86,    87,    88,    -1,    90,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   111,    -1,
      -1,    -1,    -1,    -1,   117,   118,    -1,   120,   121,   122,
     123,   124,   125,   126,   127,   128,   129,    -1,    -1,    -1,
      -1,    -1,   135,   136,   137,    -1,    -1,   140,    -1,   142,
      -1,   144,   145,     1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    -1,    -1,    15,    16,    -1,
      18,    19,    20,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    59,    60,    61,    62,    -1,    64,    65,    66,    -1,
      68,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,
      98,    -1,    -1,   101,   102,   103,   104,   105,   106,   107,
     108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   130,   131,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    -1,     1,   145,     3,     4,
       5,     6,     7,    -1,    -1,    10,    11,    12,    -1,    14,
      15,    16,    -1,    18,    19,    20,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    59,    60,    61,    62,    -1,    64,
      65,    66,    -1,    68,    69,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,
      95,    96,    -1,    98,    -1,    -1,   101,   102,   103,   104,
     105,   106,   107,   108,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   130,   131,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,     1,
     145,     3,     4,     5,     6,     7,    -1,    -1,    10,    11,
      12,    -1,    -1,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    -1,    59,    60,    61,
      62,    -1,    64,    65,    66,    -1,    68,    69,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,
      92,    93,    -1,    95,    96,    -1,    98,    -1,    -1,   101,
     102,   103,   104,   105,   106,   107,   108,   109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   130,   131,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     142,    -1,     1,   145,     3,     4,     5,     6,     7,    -1,
      -1,    10,    11,    12,    -1,    -1,    15,    16,    -1,    18,
      19,    20,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    -1,
      59,    60,    61,    62,    -1,    64,    65,    66,    -1,    68,
      69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,    98,
      -1,    -1,   101,   102,   103,   104,   105,   106,   107,   108,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,
      -1,     3,     4,     5,     6,     7,    -1,     9,    10,    11,
      12,   130,   131,   132,    16,    -1,    18,    19,    20,    21,
      22,    23,    24,   142,    -1,    -1,   145,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    -1,    59,    60,    61,
      62,    -1,    64,    65,    66,    -1,    68,    69,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,
      92,    93,    -1,    95,    96,    -1,    98,    -1,    -1,   101,
     102,   103,   104,   105,   106,   107,   108,   109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,     3,     4,
       5,     6,     7,    -1,    -1,    -1,    11,    12,   130,   131,
     132,    16,    -1,    18,    19,    20,    21,    22,    23,    24,
     142,    -1,    -1,   145,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    59,    60,    61,    62,    -1,    64,
      65,    66,    -1,    68,    69,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,
      95,    96,    -1,    98,    -1,    -1,   101,   102,   103,   104,
     105,   106,   107,   108,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   130,   131,   132,     1,    -1,
       3,     4,     5,     6,     7,   140,    -1,   142,    11,    12,
     145,    -1,    -1,    16,    -1,    18,    19,    20,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    -1,    59,    60,    61,    62,
      -1,    64,    65,    66,    -1,    68,    69,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      93,    -1,    95,    96,    -1,    98,    -1,    -1,   101,   102,
     103,   104,   105,   106,   107,   108,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   130,   131,   132,
       1,    -1,     3,     4,     5,     6,     7,   140,    -1,   142,
      11,    12,   145,    -1,    -1,    16,    -1,    18,    19,    20,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    -1,    59,    60,
      61,    62,    -1,    64,    65,    66,    -1,    68,    69,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      -1,    92,    93,    -1,    95,    96,    -1,    98,    -1,    -1,
     101,   102,   103,   104,   105,   106,   107,   108,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   130,
     131,   132,    -1,    -1,   135,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,     1,   145,     3,     4,     5,     6,     7,
      -1,    -1,    -1,    11,    12,    -1,    -1,    -1,    16,    -1,
      18,    19,    20,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    59,    60,    61,    62,    -1,    64,    65,    66,    -1,
      68,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,
      98,    -1,    -1,   101,   102,   103,   104,   105,   106,   107,
     108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   130,   131,   132,    -1,    -1,   135,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    -1,     1,   145,     3,     4,
       5,     6,     7,    -1,    -1,    10,    11,    12,    -1,    -1,
      -1,    16,    -1,    18,    19,    20,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    59,    60,    61,    62,    -1,    64,
      65,    66,    -1,    68,    69,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,
      95,    96,    -1,    98,    -1,    -1,   101,   102,   103,   104,
     105,   106,   107,   108,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     1,    -1,     3,     4,     5,     6,     7,
      -1,    -1,    -1,    11,    12,   130,   131,   132,    16,    -1,
      18,    19,    20,    21,    22,    23,    24,   142,    -1,    -1,
     145,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    59,    60,    61,    62,    -1,    64,    65,    66,    -1,
      68,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,
      98,    -1,    -1,   101,   102,   103,   104,   105,   106,   107,
     108,   109,    -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,
      11,    12,   130,   131,   132,    16,    -1,    18,    19,    20,
      21,    22,    23,    24,   142,    -1,    -1,   145,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    58,    59,    60,
      61,    62,    -1,    64,    65,    66,    -1,    68,    69,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      -1,    92,    93,    -1,    95,    96,    -1,    98,    99,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,    -1,     7,    -1,    -1,    -1,    11,    12,   130,
     131,   132,    16,    -1,    18,    19,    20,    21,    22,    23,
      24,    -1,    -1,    -1,   145,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    46,    -1,    -1,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    -1,    59,    60,    61,    62,    -1,
      64,    65,    66,    -1,    68,    69,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,
      -1,    95,    96,    -1,    -1,    -1,    -1,   101,   102,   103,
     104,   105,   106,   107,   108,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,    -1,
       7,    -1,    -1,    -1,    11,    12,   130,   131,   132,    16,
      -1,    18,    19,    20,    21,    22,    23,    24,   142,    -1,
      -1,   145,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    46,
      -1,    -1,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    -1,    59,    60,    61,    62,    -1,    64,    65,    66,
      -1,    68,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,
      -1,    -1,    -1,    -1,   101,   102,   103,   104,   105,   106,
     107,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,     6,     7,    -1,    -1,
      -1,    11,    12,   130,   131,   132,    16,    -1,    18,    19,
      20,    21,    22,    23,    24,    -1,    -1,    -1,   145,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    -1,    59,
      60,    61,    62,    -1,    64,    65,    66,    -1,    68,    69,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    93,    -1,    95,    96,    -1,    98,    -1,
      -1,   101,   102,   103,   104,   105,   106,   107,   108,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,     6,     7,    -1,    -1,    -1,    11,    12,
     130,   131,   132,    16,    -1,    18,    19,    20,    21,    22,
      23,    24,   142,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    -1,    59,    60,    61,    62,
      -1,    64,    65,    66,    -1,    68,    69,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      93,    -1,    95,    96,    -1,    98,    -1,    -1,   101,   102,
     103,   104,   105,   106,   107,   108,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   130,   131,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    -1,    -1,    -1,
      -1,    -1,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    -1,    -1,    79,    80,    -1,    -1,
      83,    84,    85,    86,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    98,    99,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   120,   121,   122,
     123,   124,   125,   126,   127,   128,   129,    -1,   131,   132,
      -1,    -1,    -1,    -1,    -1,   138,   139,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    -1,    -1,    -1,    -1,    -1,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    -1,    -1,    79,    80,    -1,    -1,    83,    84,    85,
      86,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    98,    99,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   120,   121,   122,   123,   124,   125,
     126,   127,   128,   129,    -1,   131,   132,    -1,    -1,    -1,
      -1,    -1,   138,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    -1,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    68,    69,
      70,    71,    72,    73,    74,    75,    76,    -1,    -1,    79,
      80,    -1,    -1,    83,    84,    85,    86,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    99,
      -1,    -1,   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     120,   121,   122,   123,   124,   125,   126,   127,   128,   129,
      -1,   131,   132,    -1,    -1,    -1,    -1,    -1,   138,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    -1,    -1,    -1,    -1,
      -1,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      -1,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    -1,    -1,    79,    80,    -1,    -1,    83,
      84,    85,    86,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    98,    99,    -1,    -1,   102,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   120,   121,   122,   123,
     124,   125,   126,   127,   128,   129,    -1,   131,   132,    -1,
      -1,    -1,    -1,    -1,   138,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    -1,    -1,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      68,    69,    70,    71,    72,    73,    74,    75,    76,    -1,
      -1,    79,    80,    -1,    -1,    83,    84,    85,    86,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      98,    99,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   120,   121,   122,   123,   124,   125,   126,   127,
     128,   129,    -1,   131,   132,     3,     4,     5,    -1,     7,
     138,    -1,    -1,    11,    12,    -1,    -1,    -1,    16,    -1,
      18,    19,    20,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,
      -1,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    59,    60,    61,    62,    -1,    64,    65,    66,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,
      -1,    -1,    -1,   101,   102,   103,   104,   105,   106,   107,
     108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,    -1,     7,
      -1,    -1,   130,    11,    12,    -1,    -1,    -1,    16,   137,
      18,    19,    20,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,
      -1,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    59,    60,    61,    62,    -1,    64,    65,    66,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,
      -1,    -1,    -1,   101,   102,   103,   104,   105,   106,   107,
     108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,
      -1,    -1,   130,    11,    12,    -1,    -1,    -1,    16,   137,
      18,    19,    20,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    59,    60,    61,    62,    -1,    64,    65,    66,    -1,
      68,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,
      98,    -1,    -1,   101,   102,   103,   104,   105,   106,   107,
     108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,
      11,    12,   130,   131,   132,    16,    -1,    18,    19,    20,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    58,    59,    60,
      61,    62,    -1,    64,    65,    66,    -1,    68,    69,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      -1,    92,    93,    -1,    95,    96,    -1,    98,    99,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,     7,    -1,    -1,    -1,    11,    12,   130,
     131,   132,    16,    -1,    18,    19,    20,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    45,    46,    -1,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    -1,    59,    60,    61,    62,    -1,
      64,    65,    66,    -1,    68,    69,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,
      -1,    95,    96,    -1,    98,    -1,    -1,   101,   102,   103,
     104,   105,   106,   107,   108,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,    -1,
       7,    -1,    -1,    -1,    11,    12,   130,   131,   132,    16,
      -1,    18,    19,    20,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    46,
      -1,    -1,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    58,    59,    60,    61,    62,    -1,    64,    65,    66,
      -1,    68,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,
      -1,    98,    99,   100,   101,   102,   103,   104,   105,   106,
     107,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,    -1,     7,    -1,    -1,
      -1,    11,    12,   130,   131,   132,    16,    -1,    18,    19,
      20,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    58,    59,
      60,    61,    62,    -1,    64,    65,    66,    -1,    68,    69,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    93,    -1,    95,    96,    -1,    98,    99,
      -1,   101,   102,   103,   104,   105,   106,   107,   108,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,    -1,     7,    -1,    -1,    -1,    11,    12,
     130,   131,   132,    16,    -1,    18,    19,    20,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    46,    -1,    -1,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    58,    59,    60,    61,    62,
      -1,    64,    65,    66,    -1,    68,    69,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      93,    -1,    95,    96,    -1,    -1,    99,   100,   101,   102,
     103,   104,   105,   106,   107,   108,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,
      -1,     7,    -1,    -1,    -1,    11,    12,   130,   131,   132,
      16,    -1,    18,    19,    20,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      46,    -1,    -1,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    58,    59,    60,    61,    62,    -1,    64,    65,
      66,    -1,    68,    69,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,
      96,    -1,    98,    99,    -1,   101,   102,   103,   104,   105,
     106,   107,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,    -1,     7,    -1,
      -1,    -1,    11,    12,   130,   131,   132,    16,    -1,    18,
      19,    20,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    58,
      59,    60,    61,    62,    -1,    64,    65,    66,    -1,    68,
      69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,    -1,
      99,    -1,   101,   102,   103,   104,   105,   106,   107,   108,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,    11,
      12,   130,   131,   132,    16,    -1,    18,    19,    20,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    -1,    59,    60,    61,
      62,    -1,    64,    65,    66,    -1,    68,    69,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,
      92,    93,    -1,    95,    96,    -1,    98,    -1,    -1,   101,
     102,   103,   104,   105,   106,   107,   108,   109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,    -1,     7,    -1,    -1,    -1,    11,    12,   130,   131,
     132,    16,    -1,    18,    19,    20,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    46,    -1,    -1,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    59,    60,    61,    62,    -1,    64,
      65,    66,    -1,    68,    69,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,
      95,    96,    -1,    98,    -1,    -1,   101,   102,   103,   104,
     105,   106,   107,   108,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,    -1,     7,
      -1,    -1,    -1,    11,    12,   130,   131,   132,    16,    -1,
      18,    19,    20,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,
      -1,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    59,    60,    61,    62,    -1,    64,    65,    66,    -1,
      68,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,
      98,    -1,    -1,   101,   102,   103,   104,   105,   106,   107,
     108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,
      11,    12,   130,   131,   132,    16,    -1,    18,    19,    20,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    -1,    59,    60,
      61,    62,    -1,    64,    65,    66,    -1,    68,    69,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      -1,    92,    93,    -1,    95,    96,    -1,    98,    -1,    -1,
     101,   102,   103,   104,   105,   106,   107,   108,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,    -1,     7,    -1,    -1,    -1,    11,    12,   130,
     131,   132,    16,    -1,    18,    19,    20,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    46,    -1,    -1,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    -1,    59,    60,    61,    62,    -1,
      64,    65,    66,    -1,    68,    69,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,
      -1,    95,    96,    -1,    98,    -1,    -1,   101,   102,   103,
     104,   105,   106,   107,   108,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,    -1,
       7,    -1,    -1,    -1,    11,    12,   130,   131,   132,    16,
      -1,    18,    19,    20,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    46,
      -1,    -1,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    -1,    59,    60,    61,    62,    -1,    64,    65,    66,
      -1,    68,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,
      -1,    -1,    -1,    -1,   101,   102,   103,   104,   105,   106,
     107,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,    -1,     7,    -1,    -1,
      -1,    11,    12,   130,   131,   132,    16,    -1,    18,    19,
      20,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    -1,    59,
      60,    61,    62,    -1,    64,    65,    66,    -1,    68,    69,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    93,    -1,    95,    96,    -1,    -1,    -1,
      -1,   101,   102,   103,   104,   105,   106,   107,   108,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,    -1,     7,    -1,    -1,    -1,    11,    12,
     130,   131,   132,    16,    -1,    18,    19,    20,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    46,    -1,    -1,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    -1,    59,    60,    61,    62,
      -1,    64,    65,    66,    -1,    68,    69,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      93,    -1,    95,    96,    -1,    -1,    -1,    -1,   101,   102,
     103,   104,   105,   106,   107,   108,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,
      -1,     7,    -1,    -1,    -1,    11,    12,   130,   131,   132,
      16,    -1,    18,    19,    20,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      46,    -1,    -1,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    -1,    59,    60,    61,    62,    -1,    64,    65,
      66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,
      96,    -1,    98,    -1,    -1,   101,   102,   103,   104,   105,
     106,   107,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,    -1,     7,    -1,    -1,    -1,    11,    12,
      -1,    -1,    -1,    16,   130,    18,    19,    20,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    46,    -1,    -1,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    -1,    59,    60,    61,    62,
      -1,    64,    65,    66,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      93,    -1,    95,    96,    -1,    98,    -1,    -1,   101,   102,
     103,   104,   105,   106,   107,   108,   109,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,    -1,     7,    -1,    -1,
      -1,    11,    12,    -1,    -1,    -1,    16,   130,    18,    19,
      20,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    -1,    59,
      60,    61,    62,    -1,    64,    65,    66,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    85,    -1,    -1,    -1,    89,
      -1,    -1,    92,    93,    -1,    95,    96,    -1,    -1,    -1,
      -1,   101,   102,   103,   104,   105,   106,   107,   108,   109,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,    -1,
       7,    -1,    -1,    -1,    11,    12,    -1,    -1,    -1,    16,
     130,    18,    19,    20,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    46,
      -1,    -1,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    -1,    59,    60,    61,    62,    -1,    64,    65,    66,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,
      -1,    -1,    -1,    -1,   101,   102,   103,   104,   105,   106,
     107,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,    -1,     7,    -1,    -1,    -1,    11,    12,    -1,
      -1,    -1,    16,   130,    18,    19,    20,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    46,    -1,    -1,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    -1,    59,    60,    61,    62,    -1,
      64,    65,    66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,
      -1,    95,    96,    -1,    -1,    -1,    -1,   101,   102,   103,
     104,   105,   106,   107,   108,   109,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,
      11,    12,    -1,    -1,    -1,    16,   130,    18,    19,    20,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    -1,    59,    60,
      61,    62,    -1,    64,    65,    66,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      -1,    92,    93,    -1,    95,    96,    -1,    -1,    -1,    -1,
     101,   102,   103,   104,   105,   106,   107,   108,   109,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,    -1,     7,
      -1,    -1,    -1,    11,    12,    -1,    -1,    -1,    16,   130,
      18,    19,    20,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,
      -1,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    59,    60,    61,    62,    -1,    64,    65,    66,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,
      -1,    -1,    -1,   101,   102,   103,   104,   105,   106,   107,
     108,   109,    -1,    -1,    -1,    -1,    -1,    52,    53,    -1,
      -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   130,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    -1,    -1,    79,    80,    -1,    -1,    83,    84,
      85,    86,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    98,    99,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,    -1,   131,   132,    52,    53,
      -1,    -1,    56,   138,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    -1,    -1,    79,    80,    -1,    -1,    83,
      84,    85,    86,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    98,    99,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   120,   121,   122,   123,
     124,   125,   126,   127,   128,   129,    -1,   131,   132,    52,
      53,    -1,    -1,    56,   138,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    -1,    -1,    79,    80,    -1,    -1,
      83,    84,    85,    86,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    98,    99,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   120,   121,   122,
     123,   124,   125,   126,   127,   128,   129,    -1,   131,   132,
      52,    53,    -1,    -1,    56,   138,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    -1,    -1,    79,    80,    -1,
      -1,    83,    84,    85,    86,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    98,    99,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   120,   121,
     122,   123,   124,   125,   126,   127,   128,   129,    -1,   131,
     132,    52,    53,    -1,    -1,    56,   138,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    -1,    -1,    79,    80,
      -1,    -1,    83,    84,    85,    86,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    99,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,    -1,
     131,   132,    52,    53,    -1,    -1,    56,   138,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    68,    69,
      70,    71,    72,    73,    74,    75,    76,    -1,    -1,    79,
      80,    -1,    -1,    83,    84,    85,    86,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    99,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     120,   121,   122,   123,   124,   125,   126,   127,   128,   129,
      -1,   131,   132,    52,    53,    -1,    -1,    56,   138,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    -1,    -1,
      79,    80,    -1,    -1,    83,    84,    85,    86,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,
      99,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   120,   121,   122,   123,   124,   125,   126,   127,   128,
     129,    -1,   131,   132,    52,    53,    -1,    -1,    56,   138,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      68,    69,    70,    71,    72,    73,    74,    75,    76,    -1,
      -1,    79,    80,    -1,    -1,    83,    84,    85,    86,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      98,    99,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   120,   121,   122,   123,   124,   125,   126,   127,
     128,   129,    -1,   131,   132,    52,    53,    -1,    -1,    56,
     138,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      -1,    -1,    79,    80,    -1,    -1,    83,    84,    85,    86,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    98,    99,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   120,   121,   122,   123,   124,   125,   126,
     127,   128,   129,    -1,   131,   132,    52,    53,    -1,    -1,
      56,   138,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    -1,    -1,    79,    80,    -1,    -1,    83,    84,    85,
      86,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    98,    99,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   120,   121,   122,   123,   124,   125,
     126,   127,   128,   129,    -1,   131,   132,    52,    53,    -1,
      -1,    56,   138,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    -1,    -1,    79,    80,    -1,    -1,    83,    84,
      85,    86,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    98,    99,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,    -1,   131,   132,    52,    53,
      -1,    -1,    56,   138,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    -1,    -1,    79,    80,    -1,    -1,    83,
      84,    85,    86,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    98,    99,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   120,   121,   122,   123,
     124,   125,   126,   127,   128,   129,    -1,   131,   132,    -1,
      -1,    -1,    -1,    -1,   138
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint16 yystos[] =
{
       0,   147,   148,     0,     1,     3,     4,     5,     6,     7,
      11,    12,    16,    18,    19,    20,    21,    22,    23,    24,
      30,    31,    32,    33,    34,    35,    36,    39,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    59,    60,    61,    62,    64,    65,    66,    68,    69,
      89,    92,    93,    95,    96,    98,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   130,   131,   132,   149,   150,
     151,   158,   160,   162,   164,   165,   168,   169,   170,   172,
     173,   174,   176,   177,   187,   203,   222,   223,   224,   225,
     226,   227,   228,   229,   230,   231,   232,   259,   260,   280,
     281,   282,   283,   284,   285,   286,   289,   291,   292,   306,
     308,   309,   310,   311,   312,   313,   314,   315,   349,   362,
     151,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    56,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    79,    80,    83,    84,    85,    86,
      98,    99,   120,   121,   122,   123,   124,   125,   126,   127,
     128,   129,   131,   132,   138,   180,   181,   182,   183,   185,
     186,   306,   308,    39,    58,    89,    92,    98,    99,   100,
     103,   131,   169,   177,   187,   189,   195,   198,   200,   222,
     311,   312,   314,   315,   347,   348,   195,   195,   139,   196,
     197,   139,   192,   196,   139,   145,   356,    54,   182,   356,
     152,   134,    21,    22,    30,    31,    32,   168,   187,   222,
     187,    56,     1,    47,    92,   154,   155,   156,   158,   171,
     172,   362,   205,   206,   190,   200,   347,   362,   189,   346,
     347,   362,    46,    89,   130,   137,   176,   203,   222,   311,
     312,   315,   250,   251,    54,    55,    57,   180,   296,   307,
     295,   296,   297,   143,   287,   143,   293,   143,   290,   143,
     294,   310,   164,   187,   187,   142,   145,   355,   360,   361,
      40,    41,    42,    43,    44,    37,    38,    26,   134,   192,
     196,   265,    28,   257,   117,   137,    92,    98,   173,   117,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    85,    86,   118,   120,   121,   122,   123,
     124,   125,   126,   127,   128,   129,    87,    88,   136,   144,
     353,   204,   162,   163,   163,   209,   211,   163,   355,   361,
      89,   170,   177,   222,   238,   311,   312,   315,    52,    56,
      85,    89,   178,   179,   222,   311,   312,   315,   179,    33,
      34,    35,    36,    49,    50,    51,    52,    56,   139,   180,
     313,   344,    88,   353,   354,   265,   283,    90,    90,   137,
     189,    56,   189,   189,   189,   295,   117,    91,   137,   199,
     362,    88,   136,   353,    90,    90,   137,   199,   195,   356,
     357,   195,   194,   195,   200,   347,   362,   162,   357,   162,
      54,    65,    66,   159,   139,   188,   134,   154,    88,   353,
      90,   158,   157,   171,   140,   355,   361,   357,   357,   158,
     141,   137,   145,   359,   137,   359,   135,   359,   356,    56,
     310,   173,   175,   137,    88,   136,   353,   252,    63,   110,
     112,   113,   298,   113,   298,   113,    67,   298,   113,   113,
     288,   298,   113,    63,   113,   113,   113,   288,   113,    63,
     113,    70,   142,   151,   163,   163,   163,   163,   158,   162,
     162,   267,   266,    97,   166,   258,    98,   164,   189,   200,
     201,   202,   171,   137,   176,   137,   160,   161,   164,   177,
     187,   189,   191,   202,   222,   315,   187,   187,   187,   187,
     187,   187,   187,   187,   187,   187,   187,   187,   187,   187,
     187,   187,   187,   187,   187,   187,   187,   187,   187,   187,
     187,   187,    52,    53,    56,   185,   264,   350,   351,   194,
      52,    53,    56,   185,   263,   350,   153,   154,    13,   234,
     360,   234,   163,   163,   355,    17,   274,    56,    88,   136,
     353,    25,   162,    52,    56,   178,   121,   316,    88,   136,
     353,   217,   345,   218,    88,   144,   352,    52,    56,   350,
     161,   187,   191,   161,   191,   184,   115,   189,    98,   189,
     198,   347,    52,    56,   194,    52,    56,   348,   357,   140,
     357,   137,   137,   357,   182,   208,   187,   149,   135,   350,
     350,   191,   134,   357,   156,   207,   347,   137,   175,    52,
      56,   194,    52,    56,    52,    54,    55,    56,    57,    58,
      70,    92,    98,    99,   100,   124,   127,   139,   255,   320,
     322,   323,   324,   325,   326,   327,   328,   329,   332,   333,
     334,   335,   338,   339,   340,   341,   342,   300,   299,   143,
     298,   143,   143,   143,   187,   271,   272,   268,   269,   167,
     271,   189,   137,   357,   175,   137,    44,   117,    44,    88,
     136,   353,   356,    90,    90,   192,   196,   262,   356,   358,
      90,    90,   192,   196,   261,    10,   233,     8,   276,   362,
     154,    13,   154,    27,   235,   360,   235,   274,   200,   233,
      52,    56,   194,    52,    56,   213,   216,   317,   215,    52,
      56,   178,   194,   153,   162,   219,   220,   192,   193,   196,
     362,   182,   189,   189,   199,    90,    90,   358,    90,    90,
     347,   162,   135,   149,   357,   359,   173,   358,    92,    98,
     239,   240,   241,   324,   322,   253,   117,   137,   321,   189,
     137,   343,   362,    52,   137,   343,   137,   321,    52,   137,
     321,    52,   301,    54,    55,    57,   305,   315,    10,   273,
     135,   270,   268,    10,    98,   189,   175,   158,   187,    52,
      56,   194,    52,    56,   119,   161,   191,   161,   191,   166,
     192,   141,    90,   161,   191,   161,   191,   166,   193,   189,
     202,   277,   362,    15,   237,   362,    14,   236,   237,   237,
     210,   212,   233,   137,   234,   358,   163,   360,   163,   153,
     358,   233,   357,   139,   318,   319,   180,   265,   257,    90,
     137,   357,   135,   241,   137,   324,   137,   357,   247,   356,
     254,   189,   320,   326,   338,   340,   329,   334,   342,   327,
     335,   340,   325,   327,   302,    78,   122,   245,   246,   362,
     245,   135,   189,   358,   187,   161,   191,    91,   278,   362,
     154,     9,   279,   362,   163,   233,   233,   154,   154,   189,
     154,   235,   153,   360,   233,   322,   153,   322,   221,   357,
     240,   137,    98,   239,   140,   142,    29,   114,   256,   137,
     321,   137,   321,   343,   137,   321,   137,   321,   321,   303,
     242,   244,   247,   325,   327,   328,   330,   331,   334,   336,
     337,   340,   342,   154,   154,    89,   177,   222,   311,   312,
     315,   234,   154,   234,   233,   233,   237,   274,   275,   214,
     233,   357,   233,   360,   318,   137,   240,   137,   324,    52,
     248,   249,   323,   154,   154,   327,   340,   327,   327,   304,
     247,   122,   117,   137,   243,    89,   222,   137,   343,   343,
     137,   243,   137,   243,    56,    88,   136,   353,   154,   154,
     154,   153,   240,   137,   137,   356,   233,   135,   321,   137,
     321,   321,   321,   154,   122,   222,   242,   337,   340,    56,
      88,   330,   334,   327,   336,   340,   327,    52,    56,   194,
      52,    56,   276,   236,   233,   233,   240,   249,   327,   111,
     137,   243,   137,   243,    52,    56,   343,   137,   243,   137,
     243,   243,   358,   321,   327,   340,   327,   327,   243,   137,
     243,   243,   243,   327,   243
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint16 yyr1[] =
{
       0,   146,   148,   147,   149,   150,   150,   150,   150,   151,
     152,   151,   153,   154,   155,   155,   155,   155,   156,   157,
     156,   159,   158,   158,   158,   158,   158,   158,   158,   158,
     158,   158,   158,   158,   158,   158,   158,   158,   160,   160,
     160,   160,   160,   160,   160,   160,   161,   161,   161,   162,
     162,   162,   162,   162,   162,   163,   164,   164,   165,   165,
     167,   166,   168,   169,   169,   169,   169,   169,   169,   169,
     169,   169,   169,   169,   170,   170,   171,   171,   172,   172,
     172,   172,   172,   172,   172,   172,   172,   172,   173,   173,
     174,   174,   175,   175,   176,   176,   176,   176,   176,   176,
     176,   176,   176,   177,   177,   177,   177,   177,   177,   177,
     177,   177,   178,   178,   179,   179,   179,   180,   180,   180,
     180,   180,   181,   181,   182,   182,   183,   184,   183,   185,
     185,   185,   185,   185,   185,   185,   185,   185,   185,   185,
     185,   185,   185,   185,   185,   185,   185,   185,   185,   185,
     185,   185,   185,   185,   185,   185,   185,   185,   185,   186,
     186,   186,   186,   186,   186,   186,   186,   186,   186,   186,
     186,   186,   186,   186,   186,   186,   186,   186,   186,   186,
     186,   186,   186,   186,   186,   186,   186,   186,   186,   186,
     186,   186,   186,   186,   186,   186,   186,   186,   186,   186,
     187,   187,   187,   187,   187,   187,   187,   187,   187,   187,
     187,   187,   187,   187,   187,   187,   187,   187,   187,   187,
     187,   187,   187,   187,   187,   187,   187,   187,   187,   187,
     187,   187,   187,   187,   187,   187,   187,   187,   187,   188,
     187,   187,   187,   189,   190,   190,   190,   190,   191,   191,
     192,   193,   193,   194,   194,   194,   194,   194,   195,   195,
     195,   195,   195,   197,   196,   198,   199,   199,   200,   200,
     200,   200,   201,   201,   202,   202,   202,   203,   203,   203,
     203,   203,   203,   203,   203,   203,   203,   203,   204,   203,
     205,   203,   206,   207,   203,   203,   203,   203,   203,   203,
     203,   203,   203,   203,   208,   203,   203,   203,   203,   203,
     203,   203,   203,   203,   209,   210,   203,   211,   212,   203,
     203,   203,   213,   214,   203,   215,   203,   216,   203,   217,
     203,   218,   219,   203,   220,   221,   203,   203,   203,   203,
     203,   222,   223,   224,   225,   226,   227,   228,   229,   230,
     231,   232,   233,   234,   234,   234,   235,   235,   236,   236,
     237,   237,   238,   238,   239,   239,   240,   240,   241,   241,
     241,   241,   241,   241,   241,   241,   241,   242,   242,   242,
     242,   243,   243,   244,   244,   244,   244,   244,   244,   244,
     244,   244,   244,   244,   244,   244,   244,   244,   245,   245,
     246,   246,   246,   247,   247,   248,   248,   249,   249,   251,
     252,   253,   254,   250,   255,   255,   256,   256,   258,   257,
     259,   259,   259,   259,   260,   261,   260,   262,   260,   260,
     263,   260,   264,   260,   260,   260,   260,   266,   265,   267,
     265,   269,   270,   268,   272,   273,   271,   274,   275,   275,
     276,   276,   277,   277,   277,   278,   278,   279,   279,   280,
     280,   280,   281,   282,   282,   282,   283,   284,   285,   286,
     286,   287,   287,   288,   288,   289,   289,   290,   290,   291,
     291,   292,   292,   293,   293,   294,   294,   295,   295,   296,
     296,   297,   297,   298,   299,   298,   300,   301,   302,   303,
     304,   298,   305,   305,   305,   305,   306,   307,   307,   307,
     307,   308,   309,   309,   310,   310,   310,   310,   311,   311,
     311,   311,   311,   312,   312,   312,   312,   312,   312,   312,
     313,   313,   314,   314,   315,   315,   317,   316,   316,   318,
     319,   318,   320,   320,   320,   320,   321,   321,   322,   322,
     322,   322,   322,   322,   322,   322,   322,   322,   322,   322,
     322,   322,   322,   323,   323,   323,   323,   324,   324,   325,
     326,   326,   327,   327,   328,   329,   329,   330,   330,   331,
     331,   332,   332,   333,   333,   334,   334,   335,   336,   337,
     337,   338,   338,   339,   339,   340,   340,   341,   341,   342,
     343,   343,   344,   345,   344,   346,   346,   347,   347,   348,
     348,   348,   348,   349,   349,   349,   350,   350,   350,   350,
     351,   351,   351,   352,   352,   353,   353,   354,   354,   355,
     355,   356,   356,   357,   358,   359,   359,   359,   360,   360,
     361,   361,   362
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     2,     1,     1,     3,     2,     1,
       0,     5,     4,     2,     1,     1,     3,     2,     1,     0,
       5,     0,     4,     3,     3,     3,     2,     3,     3,     3,
       3,     3,     4,     1,     3,     3,     3,     1,     3,     3,
       6,     5,     5,     5,     5,     3,     1,     3,     1,     1,
       3,     3,     3,     2,     1,     1,     1,     1,     1,     4,
       0,     4,     1,     2,     3,     4,     5,     4,     5,     2,
       2,     2,     2,     2,     1,     3,     1,     3,     1,     2,
       3,     5,     2,     4,     2,     4,     1,     3,     1,     3,
       2,     3,     1,     3,     1,     1,     4,     3,     3,     3,
       3,     2,     1,     1,     1,     4,     3,     3,     3,     3,
       2,     1,     1,     1,     2,     1,     3,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     0,     4,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       3,     3,     6,     5,     5,     5,     5,     4,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     4,     2,     2,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     2,     2,     3,     3,     3,     3,     0,
       4,     6,     1,     1,     1,     2,     4,     2,     1,     3,
       3,     1,     1,     1,     1,     2,     4,     2,     1,     2,
       2,     4,     1,     0,     2,     2,     2,     1,     1,     2,
       3,     4,     1,     1,     3,     4,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     0,     4,
       0,     3,     0,     0,     5,     3,     3,     2,     3,     3,
       1,     4,     3,     1,     0,     6,     4,     3,     2,     1,
       2,     2,     6,     6,     0,     0,     7,     0,     0,     7,
       5,     4,     0,     0,     9,     0,     6,     0,     7,     0,
       5,     0,     0,     7,     0,     0,     9,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     1,     1,     1,     5,
       1,     2,     1,     1,     1,     3,     1,     3,     1,     4,
       6,     3,     5,     2,     4,     1,     3,     4,     2,     2,
       1,     2,     0,     6,     8,     4,     6,     4,     2,     6,
       2,     4,     6,     2,     4,     2,     4,     1,     1,     1,
       3,     1,     4,     1,     4,     1,     3,     1,     1,     0,
       0,     0,     0,     6,     4,     1,     3,     3,     0,     4,
       2,     4,     5,     5,     2,     0,     5,     0,     5,     3,
       0,     4,     0,     4,     2,     1,     4,     0,     4,     0,
       4,     0,     0,     4,     0,     0,     4,     5,     1,     1,
       6,     1,     1,     1,     1,     2,     1,     2,     1,     1,
       1,     1,     1,     1,     1,     2,     3,     3,     3,     3,
       3,     0,     3,     1,     2,     3,     3,     0,     3,     3,
       3,     3,     3,     0,     3,     0,     3,     0,     2,     0,
       2,     0,     2,     1,     0,     3,     0,     0,     0,     0,
       0,     8,     1,     1,     1,     1,     2,     1,     1,     1,
       1,     3,     1,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     0,     4,     0,     3,
       0,     3,     4,     2,     2,     1,     2,     0,     6,     8,
       4,     6,     4,     6,     2,     4,     6,     2,     4,     2,
       4,     1,     0,     1,     1,     1,     1,     1,     1,     1,
       1,     3,     1,     3,     1,     2,     1,     2,     1,     1,
       3,     1,     3,     1,     1,     2,     1,     3,     3,     1,
       3,     1,     3,     1,     1,     2,     1,     1,     1,     2,
       2,     1,     1,     0,     4,     1,     2,     1,     3,     3,
       2,     4,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     0,
       1,     0,     1,     2,     2,     0,     1,     1,     1,     1,
       1,     2,     0
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      parser_yyerror (parser, YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (parser, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value, parser); \
      YYFPRINTF (parser, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, struct parser_params *parser)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  YYUSE (parser);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, struct parser_params *parser)
{
  YYFPRINTF (parser, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep, parser);
  YYFPRINTF (parser, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop, struct parser_params *parser)
{
  YYFPRINTF (parser, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (parser, " %d", yybot);
    }
  YYFPRINTF (parser, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top), parser);                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule, struct parser_params *parser)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (parser, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (parser, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              , parser);
      YYFPRINTF (parser, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule, parser); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
#ifndef yydebug
int yydebug;
#endif
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep, struct parser_params *parser)
{
  YYUSE (yyvaluep);
  YYUSE (parser);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/*----------.
| yyparse.  |
`----------*/

int
yyparse (struct parser_params *parser)
{
/* The lookahead symbol.  */
int yychar;


/* The semantic value of the lookahead symbol.  */
/* Default value used for initialization, for pacifying older GCCs
   or non-GCC compilers.  */
YY_INITIAL_VALUE (static YYSTYPE yyval_default;)
YYSTYPE yylval YY_INITIAL_VALUE (= yyval_default);

    /* Number of syntax errors so far.  */
    int yynerrs;

    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((parser, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((parser, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((parser, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((parser, "Reading a token: "));
      yychar = yylex (&yylval, parser);
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((parser, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 1005 "parse.y" /* yacc.c:1646  */
    {
			SET_LEX_STATE(EXPR_BEG);
		    /*%%%*/
			local_push(compile_for_eval || in_main);
		    /*%
			local_push(0);
		    %*/
		    }
#line 5154 "parse.c" /* yacc.c:1646  */
    break;

  case 3:
#line 1014 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			if ((yyvsp[0].node) && !compile_for_eval) {
			    /* last expression should not be void */
			    if (nd_type((yyvsp[0].node)) != NODE_BLOCK) void_expr((yyvsp[0].node));
			    else {
				NODE *node = (yyvsp[0].node);
				while (node->nd_next) {
				    node = node->nd_next;
				}
				void_expr(node->nd_head);
			    }
			}
			ruby_eval_tree = NEW_SCOPE(0, block_append(ruby_eval_tree, (yyvsp[0].node)));
		    /*%
			$$ = $2;
			parser->result = dispatch1(program, $$);
		    %*/
			local_pop();
		    }
#line 5179 "parse.c" /* yacc.c:1646  */
    break;

  case 4:
#line 1037 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			void_stmts((yyvsp[-1].node));
		    /*%
		    %*/
			(yyval.node) = (yyvsp[-1].node);
		    }
#line 5191 "parse.c" /* yacc.c:1646  */
    break;

  case 5:
#line 1047 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_BEGIN(0);
		    /*%
			$$ = dispatch2(stmts_add, dispatch0(stmts_new),
						  dispatch0(void_stmt));
		    %*/
		    }
#line 5204 "parse.c" /* yacc.c:1646  */
    break;

  case 6:
#line 1056 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = newline_node((yyvsp[0].node));
		    /*%
			$$ = dispatch2(stmts_add, dispatch0(stmts_new), $1);
		    %*/
		    }
#line 5216 "parse.c" /* yacc.c:1646  */
    break;

  case 7:
#line 1064 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = block_append((yyvsp[-2].node), newline_node((yyvsp[0].node)));
		    /*%
			$$ = dispatch2(stmts_add, $1, $3);
		    %*/
		    }
#line 5228 "parse.c" /* yacc.c:1646  */
    break;

  case 8:
#line 1072 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = remove_begin((yyvsp[0].node));
		    }
#line 5236 "parse.c" /* yacc.c:1646  */
    break;

  case 10:
#line 1079 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			/* local_push(0); */
		    /*%
		    %*/
		    }
#line 5247 "parse.c" /* yacc.c:1646  */
    break;

  case 11:
#line 1086 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			ruby_eval_tree_begin = block_append(ruby_eval_tree_begin,
							    (yyvsp[-1].node));
			/* NEW_PREEXE($4)); */
			/* local_pop(); */
			(yyval.node) = NEW_BEGIN(0);
		    /*%
			$$ = dispatch1(BEGIN, $4);
		    %*/
		    }
#line 5263 "parse.c" /* yacc.c:1646  */
    break;

  case 12:
#line 1103 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-3].node);
			if ((yyvsp[-2].node)) {
			    (yyval.node) = NEW_RESCUE((yyvsp[-3].node), (yyvsp[-2].node), (yyvsp[-1].node));
			}
			else if ((yyvsp[-1].node)) {
			    rb_warn0("else without rescue is useless");
			    (yyval.node) = block_append((yyval.node), (yyvsp[-1].node));
			}
			if ((yyvsp[0].node)) {
			    if ((yyval.node)) {
				(yyval.node) = NEW_ENSURE((yyval.node), (yyvsp[0].node));
			    }
			    else {
				(yyval.node) = block_append((yyvsp[0].node), NEW_NIL());
			    }
			}
			fixpos((yyval.node), (yyvsp[-3].node));
		    /*%
			$$ = dispatch4(bodystmt,
				       escape_Qundef($1),
				       escape_Qundef($2),
				       escape_Qundef($3),
				       escape_Qundef($4));
		    %*/
		    }
#line 5295 "parse.c" /* yacc.c:1646  */
    break;

  case 13:
#line 1133 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			void_stmts((yyvsp[-1].node));
		    /*%
		    %*/
			(yyval.node) = (yyvsp[-1].node);
		    }
#line 5307 "parse.c" /* yacc.c:1646  */
    break;

  case 14:
#line 1143 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_BEGIN(0);
		    /*%
			$$ = dispatch2(stmts_add, dispatch0(stmts_new),
						  dispatch0(void_stmt));
		    %*/
		    }
#line 5320 "parse.c" /* yacc.c:1646  */
    break;

  case 15:
#line 1152 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = newline_node((yyvsp[0].node));
		    /*%
			$$ = dispatch2(stmts_add, dispatch0(stmts_new), $1);
		    %*/
		    }
#line 5332 "parse.c" /* yacc.c:1646  */
    break;

  case 16:
#line 1160 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = block_append((yyvsp[-2].node), newline_node((yyvsp[0].node)));
		    /*%
			$$ = dispatch2(stmts_add, $1, $3);
		    %*/
		    }
#line 5344 "parse.c" /* yacc.c:1646  */
    break;

  case 17:
#line 1168 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = remove_begin((yyvsp[0].node));
		    }
#line 5352 "parse.c" /* yacc.c:1646  */
    break;

  case 18:
#line 1174 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = (yyvsp[0].node);
		    }
#line 5360 "parse.c" /* yacc.c:1646  */
    break;

  case 19:
#line 1178 "parse.y" /* yacc.c:1646  */
    {
			yyerror("BEGIN is permitted only at toplevel");
		    /*%%%*/
			/* local_push(0); */
		    /*%
		    %*/
		    }
#line 5372 "parse.c" /* yacc.c:1646  */
    break;

  case 20:
#line 1186 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			ruby_eval_tree_begin = block_append(ruby_eval_tree_begin,
							    (yyvsp[-1].node));
			/* NEW_PREEXE($4)); */
			/* local_pop(); */
			(yyval.node) = NEW_BEGIN(0);
		    /*%
			$$ = dispatch1(BEGIN, $4);
		    %*/
		    }
#line 5388 "parse.c" /* yacc.c:1646  */
    break;

  case 21:
#line 1198 "parse.y" /* yacc.c:1646  */
    {SET_LEX_STATE(EXPR_FNAME|EXPR_FITEM);}
#line 5394 "parse.c" /* yacc.c:1646  */
    break;

  case 22:
#line 1199 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_ALIAS((yyvsp[-2].node), (yyvsp[0].node));
		    /*%
			$$ = dispatch2(alias, $2, $4);
		    %*/
		    }
#line 5406 "parse.c" /* yacc.c:1646  */
    break;

  case 23:
#line 1207 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_VALIAS((yyvsp[-1].id), (yyvsp[0].id));
		    /*%
			$$ = dispatch2(var_alias, $2, $3);
		    %*/
		    }
#line 5418 "parse.c" /* yacc.c:1646  */
    break;

  case 24:
#line 1215 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			char buf[2];
			buf[0] = '$';
			buf[1] = (char)(yyvsp[0].node)->nd_nth;
			(yyval.node) = NEW_VALIAS((yyvsp[-1].id), rb_intern2(buf, 2));
		    /*%
			$$ = dispatch2(var_alias, $2, $3);
		    %*/
		    }
#line 5433 "parse.c" /* yacc.c:1646  */
    break;

  case 25:
#line 1226 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			yyerror("can't make alias for the number variables");
			(yyval.node) = NEW_BEGIN(0);
		    /*%
			$$ = dispatch2(var_alias, $2, $3);
			$$ = dispatch1(alias_error, $$);
			ripper_error();
		    %*/
		    }
#line 5448 "parse.c" /* yacc.c:1646  */
    break;

  case 26:
#line 1237 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[0].node);
		    /*%
			$$ = dispatch1(undef, $2);
		    %*/
		    }
#line 5460 "parse.c" /* yacc.c:1646  */
    break;

  case 27:
#line 1245 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_if((yyvsp[0].node), remove_begin((yyvsp[-2].node)), 0);
			fixpos((yyval.node), (yyvsp[0].node));
		    /*%
			$$ = dispatch2(if_mod, $3, $1);
		    %*/
		    }
#line 5473 "parse.c" /* yacc.c:1646  */
    break;

  case 28:
#line 1254 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_unless((yyvsp[0].node), remove_begin((yyvsp[-2].node)), 0);
			fixpos((yyval.node), (yyvsp[0].node));
		    /*%
			$$ = dispatch2(unless_mod, $3, $1);
		    %*/
		    }
#line 5486 "parse.c" /* yacc.c:1646  */
    break;

  case 29:
#line 1263 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			if ((yyvsp[-2].node) && nd_type((yyvsp[-2].node)) == NODE_BEGIN) {
			    (yyval.node) = NEW_WHILE(cond((yyvsp[0].node)), (yyvsp[-2].node)->nd_body, 0);
			}
			else {
			    (yyval.node) = NEW_WHILE(cond((yyvsp[0].node)), (yyvsp[-2].node), 1);
			}
		    /*%
			$$ = dispatch2(while_mod, $3, $1);
		    %*/
		    }
#line 5503 "parse.c" /* yacc.c:1646  */
    break;

  case 30:
#line 1276 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			if ((yyvsp[-2].node) && nd_type((yyvsp[-2].node)) == NODE_BEGIN) {
			    (yyval.node) = NEW_UNTIL(cond((yyvsp[0].node)), (yyvsp[-2].node)->nd_body, 0);
			}
			else {
			    (yyval.node) = NEW_UNTIL(cond((yyvsp[0].node)), (yyvsp[-2].node), 1);
			}
		    /*%
			$$ = dispatch2(until_mod, $3, $1);
		    %*/
		    }
#line 5520 "parse.c" /* yacc.c:1646  */
    break;

  case 31:
#line 1289 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			NODE *resq = NEW_RESBODY(0, remove_begin((yyvsp[0].node)), 0);
			(yyval.node) = NEW_RESCUE(remove_begin((yyvsp[-2].node)), resq, 0);
		    /*%
			$$ = dispatch2(rescue_mod, $1, $3);
		    %*/
		    }
#line 5533 "parse.c" /* yacc.c:1646  */
    break;

  case 32:
#line 1298 "parse.y" /* yacc.c:1646  */
    {
			if (in_def || in_single) {
			    rb_warn0("END in method; use at_exit");
			}
		    /*%%%*/
			(yyval.node) = NEW_POSTEXE(NEW_NODE(
			    NODE_SCOPE, 0 /* tbl */, (yyvsp[-1].node) /* body */, 0 /* args */));
		    /*%
			$$ = dispatch1(END, $3);
		    %*/
		    }
#line 5549 "parse.c" /* yacc.c:1646  */
    break;

  case 34:
#line 1311 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			value_expr((yyvsp[0].node));
			(yyvsp[-2].node)->nd_value = (yyvsp[0].node);
			(yyval.node) = (yyvsp[-2].node);
		    /*%
			$$ = dispatch2(massign, $1, $3);
		    %*/
		    }
#line 5563 "parse.c" /* yacc.c:1646  */
    break;

  case 35:
#line 1321 "parse.y" /* yacc.c:1646  */
    {
			value_expr((yyvsp[0].node));
			(yyval.node) = node_assign((yyvsp[-2].node), (yyvsp[0].node));
		    }
#line 5572 "parse.c" /* yacc.c:1646  */
    break;

  case 36:
#line 1326 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyvsp[-2].node)->nd_value = (yyvsp[0].node);
			(yyval.node) = (yyvsp[-2].node);
		    /*%
			$$ = dispatch2(massign, $1, $3);
		    %*/
		    }
#line 5585 "parse.c" /* yacc.c:1646  */
    break;

  case 38:
#line 1338 "parse.y" /* yacc.c:1646  */
    {
			value_expr((yyvsp[0].node));
			(yyval.node) = node_assign((yyvsp[-2].node), (yyvsp[0].node));
		    }
#line 5594 "parse.c" /* yacc.c:1646  */
    break;

  case 39:
#line 1343 "parse.y" /* yacc.c:1646  */
    {
			value_expr((yyvsp[0].node));
			(yyval.node) = new_op_assign((yyvsp[-2].node), (yyvsp[-1].id), (yyvsp[0].node));
		    }
#line 5603 "parse.c" /* yacc.c:1646  */
    break;

  case 40:
#line 1348 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			NODE *args;

			value_expr((yyvsp[0].node));
			if (!(yyvsp[-3].node)) (yyvsp[-3].node) = NEW_ZARRAY();
			args = arg_concat((yyvsp[-3].node), (yyvsp[0].node));
			if ((yyvsp[-1].id) == tOROP) {
			    (yyvsp[-1].id) = 0;
			}
			else if ((yyvsp[-1].id) == tANDOP) {
			    (yyvsp[-1].id) = 1;
			}
			(yyval.node) = NEW_OP_ASGN1((yyvsp[-5].node), (yyvsp[-1].id), args);
			fixpos((yyval.node), (yyvsp[-5].node));
		    /*%
			$$ = dispatch2(aref_field, $1, escape_Qundef($3));
			$$ = dispatch3(opassign, $$, $5, $6);
		    %*/
		    }
#line 5628 "parse.c" /* yacc.c:1646  */
    break;

  case 41:
#line 1369 "parse.y" /* yacc.c:1646  */
    {
			value_expr((yyvsp[0].node));
			(yyval.node) = new_attr_op_assign((yyvsp[-4].node), (yyvsp[-3].id), (yyvsp[-2].id), (yyvsp[-1].id), (yyvsp[0].node));
		    }
#line 5637 "parse.c" /* yacc.c:1646  */
    break;

  case 42:
#line 1374 "parse.y" /* yacc.c:1646  */
    {
			value_expr((yyvsp[0].node));
			(yyval.node) = new_attr_op_assign((yyvsp[-4].node), (yyvsp[-3].id), (yyvsp[-2].id), (yyvsp[-1].id), (yyvsp[0].node));
		    }
#line 5646 "parse.c" /* yacc.c:1646  */
    break;

  case 43:
#line 1379 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = const_path_field((yyvsp[-4].node), (yyvsp[-2].id));
			(yyval.node) = new_const_op_assign((yyval.node), (yyvsp[-1].id), (yyvsp[0].node));
		    }
#line 5655 "parse.c" /* yacc.c:1646  */
    break;

  case 44:
#line 1384 "parse.y" /* yacc.c:1646  */
    {
			value_expr((yyvsp[0].node));
			(yyval.node) = new_attr_op_assign((yyvsp[-4].node), ripper_id2sym(idCOLON2), (yyvsp[-2].id), (yyvsp[-1].id), (yyvsp[0].node));
		    }
#line 5664 "parse.c" /* yacc.c:1646  */
    break;

  case 45:
#line 1389 "parse.y" /* yacc.c:1646  */
    {
			(yyvsp[-2].node) = var_field((yyvsp[-2].node));
			(yyval.node) = backref_assign_error((yyvsp[-2].node), node_assign((yyvsp[-2].node), (yyvsp[0].node)));
		    }
#line 5673 "parse.c" /* yacc.c:1646  */
    break;

  case 46:
#line 1396 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			value_expr((yyvsp[0].node));
			(yyval.node) = (yyvsp[0].node);
		    /*%
		    %*/
		    }
#line 5685 "parse.c" /* yacc.c:1646  */
    break;

  case 47:
#line 1404 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			value_expr((yyvsp[-2].node));
			(yyval.node) = NEW_RESCUE((yyvsp[-2].node), NEW_RESBODY(0, remove_begin((yyvsp[0].node)), 0), 0);
		    /*%
			$$ = dispatch2(rescue_mod, $1, $3);
		    %*/
		    }
#line 5698 "parse.c" /* yacc.c:1646  */
    break;

  case 50:
#line 1417 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = logop(NODE_AND, (yyvsp[-2].node), (yyvsp[0].node));
		    /*%
			$$ = dispatch3(binary, $1, ripper_intern("and"), $3);
		    %*/
		    }
#line 5710 "parse.c" /* yacc.c:1646  */
    break;

  case 51:
#line 1425 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = logop(NODE_OR, (yyvsp[-2].node), (yyvsp[0].node));
		    /*%
			$$ = dispatch3(binary, $1, ripper_intern("or"), $3);
		    %*/
		    }
#line 5722 "parse.c" /* yacc.c:1646  */
    break;

  case 52:
#line 1433 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = call_uni_op(method_cond((yyvsp[0].node)), '!');
		    /*%
			$$ = dispatch2(unary, ripper_intern("not"), $3);
		    %*/
		    }
#line 5734 "parse.c" /* yacc.c:1646  */
    break;

  case 53:
#line 1441 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = call_uni_op(method_cond((yyvsp[0].node)), '!');
		    /*%
			$$ = dispatch2(unary, ripper_id2sym('!'), $2);
		    %*/
		    }
#line 5746 "parse.c" /* yacc.c:1646  */
    break;

  case 55:
#line 1452 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			value_expr((yyvsp[0].node));
			(yyval.node) = (yyvsp[0].node);
			if (!(yyval.node)) (yyval.node) = NEW_NIL();
		    /*%
			$$ = $1;
		    %*/
		    }
#line 5760 "parse.c" /* yacc.c:1646  */
    break;

  case 59:
#line 1469 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_QCALL((yyvsp[-2].id), (yyvsp[-3].node), (yyvsp[-1].id), (yyvsp[0].node));
		    /*%
			$$ = dispatch3(call, $1, $2, $3);
			$$ = method_arg($$, $4);
		    %*/
		    }
#line 5773 "parse.c" /* yacc.c:1646  */
    break;

  case 60:
#line 1480 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.num) = ruby_sourceline;
		    /*%
		    %*/
		    }
#line 5784 "parse.c" /* yacc.c:1646  */
    break;

  case 61:
#line 1487 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = (yyvsp[-1].node);
		    /*%%%*/
			nd_set_line((yyval.node), (yyvsp[-2].num));
		    /*% %*/
		    }
#line 5795 "parse.c" /* yacc.c:1646  */
    break;

  case 62:
#line 1496 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_FCALL((yyvsp[0].id), 0);
			nd_set_line((yyval.node), tokline);
		    /*%
		    %*/
		    }
#line 5807 "parse.c" /* yacc.c:1646  */
    break;

  case 63:
#line 1506 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node);
			(yyval.node)->nd_args = (yyvsp[0].node);
		    /*%
			$$ = dispatch2(command, $1, $2);
		    %*/
		    }
#line 5820 "parse.c" /* yacc.c:1646  */
    break;

  case 64:
#line 1515 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			block_dup_check((yyvsp[-1].node),(yyvsp[0].node));
			(yyvsp[-2].node)->nd_args = (yyvsp[-1].node);
			(yyvsp[0].node)->nd_iter = (yyvsp[-2].node);
			(yyval.node) = (yyvsp[0].node);
			fixpos((yyval.node), (yyvsp[-2].node));
		    /*%
			$$ = dispatch2(command, $1, $2);
			$$ = method_add_block($$, $3);
		    %*/
		    }
#line 5837 "parse.c" /* yacc.c:1646  */
    break;

  case 65:
#line 1528 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_QCALL((yyvsp[-2].id), (yyvsp[-3].node), (yyvsp[-1].id), (yyvsp[0].node));
			fixpos((yyval.node), (yyvsp[-3].node));
		    /*%
			$$ = dispatch4(command_call, $1, $2, $3, $4);
		    %*/
		    }
#line 5850 "parse.c" /* yacc.c:1646  */
    break;

  case 66:
#line 1537 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			block_dup_check((yyvsp[-1].node),(yyvsp[0].node));
			(yyvsp[0].node)->nd_iter = NEW_QCALL((yyvsp[-3].id), (yyvsp[-4].node), (yyvsp[-2].id), (yyvsp[-1].node));
			(yyval.node) = (yyvsp[0].node);
			fixpos((yyval.node), (yyvsp[-4].node));
		    /*%
			$$ = dispatch4(command_call, $1, $2, $3, $4);
			$$ = method_add_block($$, $5);
		    %*/
		   }
#line 5866 "parse.c" /* yacc.c:1646  */
    break;

  case 67:
#line 1549 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_CALL((yyvsp[-3].node), (yyvsp[-1].id), (yyvsp[0].node));
			fixpos((yyval.node), (yyvsp[-3].node));
		    /*%
			$$ = dispatch4(command_call, $1, ID2SYM(idCOLON2), $3, $4);
		    %*/
		    }
#line 5879 "parse.c" /* yacc.c:1646  */
    break;

  case 68:
#line 1558 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			block_dup_check((yyvsp[-1].node),(yyvsp[0].node));
			(yyvsp[0].node)->nd_iter = NEW_CALL((yyvsp[-4].node), (yyvsp[-2].id), (yyvsp[-1].node));
			(yyval.node) = (yyvsp[0].node);
			fixpos((yyval.node), (yyvsp[-4].node));
		    /*%
			$$ = dispatch4(command_call, $1, ID2SYM(idCOLON2), $3, $4);
			$$ = method_add_block($$, $5);
		    %*/
		   }
#line 5895 "parse.c" /* yacc.c:1646  */
    break;

  case 69:
#line 1570 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_SUPER((yyvsp[0].node));
			fixpos((yyval.node), (yyvsp[0].node));
		    /*%
			$$ = dispatch1(super, $2);
		    %*/
		    }
#line 5908 "parse.c" /* yacc.c:1646  */
    break;

  case 70:
#line 1579 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_yield((yyvsp[0].node));
			fixpos((yyval.node), (yyvsp[0].node));
		    /*%
			$$ = dispatch1(yield, $2);
		    %*/
		    }
#line 5921 "parse.c" /* yacc.c:1646  */
    break;

  case 71:
#line 1588 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_RETURN(ret_args((yyvsp[0].node)));
		    /*%
			$$ = dispatch1(return, $2);
		    %*/
		    }
#line 5933 "parse.c" /* yacc.c:1646  */
    break;

  case 72:
#line 1596 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_BREAK(ret_args((yyvsp[0].node)));
		    /*%
			$$ = dispatch1(break, $2);
		    %*/
		    }
#line 5945 "parse.c" /* yacc.c:1646  */
    break;

  case 73:
#line 1604 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_NEXT(ret_args((yyvsp[0].node)));
		    /*%
			$$ = dispatch1(next, $2);
		    %*/
		    }
#line 5957 "parse.c" /* yacc.c:1646  */
    break;

  case 75:
#line 1615 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node);
		    /*%
			$$ = dispatch1(mlhs_paren, $2);
		    %*/
		    }
#line 5969 "parse.c" /* yacc.c:1646  */
    break;

  case 77:
#line 1626 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN(NEW_LIST((yyvsp[-1].node)), 0);
		    /*%
			$$ = dispatch1(mlhs_paren, $2);
		    %*/
		    }
#line 5981 "parse.c" /* yacc.c:1646  */
    break;

  case 78:
#line 1636 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN((yyvsp[0].node), 0);
		    /*%
			$$ = $1;
		    %*/
		    }
#line 5993 "parse.c" /* yacc.c:1646  */
    break;

  case 79:
#line 1644 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN(list_append((yyvsp[-1].node),(yyvsp[0].node)), 0);
		    /*%
			$$ = mlhs_add($1, $2);
		    %*/
		    }
#line 6005 "parse.c" /* yacc.c:1646  */
    break;

  case 80:
#line 1652 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN((yyvsp[-2].node), (yyvsp[0].node));
		    /*%
			$$ = mlhs_add_star($1, $3);
		    %*/
		    }
#line 6017 "parse.c" /* yacc.c:1646  */
    break;

  case 81:
#line 1660 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN((yyvsp[-4].node), NEW_POSTARG((yyvsp[-2].node),(yyvsp[0].node)));
		    /*%
			$1 = mlhs_add_star($1, $3);
			$$ = mlhs_add($1, $5);
		    %*/
		    }
#line 6030 "parse.c" /* yacc.c:1646  */
    break;

  case 82:
#line 1669 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN((yyvsp[-1].node), -1);
		    /*%
			$$ = mlhs_add_star($1, Qnil);
		    %*/
		    }
#line 6042 "parse.c" /* yacc.c:1646  */
    break;

  case 83:
#line 1677 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN((yyvsp[-3].node), NEW_POSTARG(-1, (yyvsp[0].node)));
		    /*%
			$1 = mlhs_add_star($1, Qnil);
			$$ = mlhs_add($1, $4);
		    %*/
		    }
#line 6055 "parse.c" /* yacc.c:1646  */
    break;

  case 84:
#line 1686 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN(0, (yyvsp[0].node));
		    /*%
			$$ = mlhs_add_star(mlhs_new(), $2);
		    %*/
		    }
#line 6067 "parse.c" /* yacc.c:1646  */
    break;

  case 85:
#line 1694 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN(0, NEW_POSTARG((yyvsp[-2].node),(yyvsp[0].node)));
		    /*%
			$2 = mlhs_add_star(mlhs_new(), $2);
			$$ = mlhs_add($2, $4);
		    %*/
		    }
#line 6080 "parse.c" /* yacc.c:1646  */
    break;

  case 86:
#line 1703 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN(0, -1);
		    /*%
			$$ = mlhs_add_star(mlhs_new(), Qnil);
		    %*/
		    }
#line 6092 "parse.c" /* yacc.c:1646  */
    break;

  case 87:
#line 1711 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN(0, NEW_POSTARG(-1, (yyvsp[0].node)));
		    /*%
			$$ = mlhs_add_star(mlhs_new(), Qnil);
			$$ = mlhs_add($$, $3);
		    %*/
		    }
#line 6105 "parse.c" /* yacc.c:1646  */
    break;

  case 89:
#line 1723 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node);
		    /*%
			$$ = dispatch1(mlhs_paren, $2);
		    %*/
		    }
#line 6117 "parse.c" /* yacc.c:1646  */
    break;

  case 90:
#line 1733 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_LIST((yyvsp[-1].node));
		    /*%
			$$ = mlhs_add(mlhs_new(), $1);
		    %*/
		    }
#line 6129 "parse.c" /* yacc.c:1646  */
    break;

  case 91:
#line 1741 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = list_append((yyvsp[-2].node), (yyvsp[-1].node));
		    /*%
			$$ = mlhs_add($1, $2);
		    %*/
		    }
#line 6141 "parse.c" /* yacc.c:1646  */
    break;

  case 92:
#line 1751 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_LIST((yyvsp[0].node));
		    /*%
			$$ = mlhs_add(mlhs_new(), $1);
		    %*/
		    }
#line 6153 "parse.c" /* yacc.c:1646  */
    break;

  case 93:
#line 1759 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = list_append((yyvsp[-2].node), (yyvsp[0].node));
		    /*%
			$$ = mlhs_add($1, $3);
		    %*/
		    }
#line 6165 "parse.c" /* yacc.c:1646  */
    break;

  case 94:
#line 1769 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = assignable((yyvsp[0].id), 0);
		    }
#line 6173 "parse.c" /* yacc.c:1646  */
    break;

  case 95:
#line 1773 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = assignable((yyvsp[0].id), 0);
		    }
#line 6181 "parse.c" /* yacc.c:1646  */
    break;

  case 96:
#line 1777 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = aryset((yyvsp[-3].node), (yyvsp[-1].node));
		    /*%
			$$ = dispatch2(aref_field, $1, escape_Qundef($3));
		    %*/
		    }
#line 6193 "parse.c" /* yacc.c:1646  */
    break;

  case 97:
#line 1785 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = attrset((yyvsp[-2].node), (yyvsp[-1].id), (yyvsp[0].id));
		    /*%
			$$ = dispatch3(field, $1, $2, $3);
		    %*/
		    }
#line 6205 "parse.c" /* yacc.c:1646  */
    break;

  case 98:
#line 1793 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = attrset((yyvsp[-2].node), idCOLON2, (yyvsp[0].id));
		    /*%
			$$ = dispatch2(const_path_field, $1, $3);
		    %*/
		    }
#line 6217 "parse.c" /* yacc.c:1646  */
    break;

  case 99:
#line 1801 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = attrset((yyvsp[-2].node), (yyvsp[-1].id), (yyvsp[0].id));
		    /*%
			$$ = dispatch3(field, $1, $2, $3);
		    %*/
		    }
#line 6229 "parse.c" /* yacc.c:1646  */
    break;

  case 100:
#line 1809 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = const_decl(const_path_field((yyvsp[-2].node), (yyvsp[0].id)));
		    }
#line 6237 "parse.c" /* yacc.c:1646  */
    break;

  case 101:
#line 1813 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = const_decl(top_const_field((yyvsp[0].id)));
		    }
#line 6245 "parse.c" /* yacc.c:1646  */
    break;

  case 102:
#line 1817 "parse.y" /* yacc.c:1646  */
    {
			(yyvsp[0].node) = var_field((yyvsp[0].node));
			(yyval.node) = backref_assign_error((yyvsp[0].node), (yyvsp[0].node));
		    }
#line 6254 "parse.c" /* yacc.c:1646  */
    break;

  case 103:
#line 1824 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = assignable((yyvsp[0].id), 0);
		    /*%%%*/
			if (!(yyval.node)) (yyval.node) = NEW_BEGIN(0);
		    /*%
			$$ = dispatch1(var_field, $$);
		    %*/
		    }
#line 6267 "parse.c" /* yacc.c:1646  */
    break;

  case 104:
#line 1833 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = assignable((yyvsp[0].id), 0);
		    /*%%%*/
			if (!(yyval.node)) (yyval.node) = NEW_BEGIN(0);
		    /*%
			$$ = dispatch1(var_field, $$);
		    %*/
		    }
#line 6280 "parse.c" /* yacc.c:1646  */
    break;

  case 105:
#line 1842 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = aryset((yyvsp[-3].node), (yyvsp[-1].node));
		    /*%
			$$ = dispatch2(aref_field, $1, escape_Qundef($3));
		    %*/
		    }
#line 6292 "parse.c" /* yacc.c:1646  */
    break;

  case 106:
#line 1850 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = attrset((yyvsp[-2].node), (yyvsp[-1].id), (yyvsp[0].id));
		    /*%
			$$ = dispatch3(field, $1, $2, $3);
		    %*/
		    }
#line 6304 "parse.c" /* yacc.c:1646  */
    break;

  case 107:
#line 1858 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = attrset((yyvsp[-2].node), idCOLON2, (yyvsp[0].id));
		    /*%
			$$ = dispatch3(field, $1, ID2SYM(idCOLON2), $3);
		    %*/
		    }
#line 6316 "parse.c" /* yacc.c:1646  */
    break;

  case 108:
#line 1866 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = attrset((yyvsp[-2].node), (yyvsp[-1].id), (yyvsp[0].id));
		    /*%
			$$ = dispatch3(field, $1, $2, $3);
		    %*/
		    }
#line 6328 "parse.c" /* yacc.c:1646  */
    break;

  case 109:
#line 1874 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = const_decl(const_path_field((yyvsp[-2].node), (yyvsp[0].id)));
		    }
#line 6336 "parse.c" /* yacc.c:1646  */
    break;

  case 110:
#line 1878 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = const_decl(top_const_field((yyvsp[0].id)));
		    }
#line 6344 "parse.c" /* yacc.c:1646  */
    break;

  case 111:
#line 1882 "parse.y" /* yacc.c:1646  */
    {
			(yyvsp[0].node) = var_field((yyvsp[0].node));
			(yyval.node) = backref_assign_error((yyvsp[0].node), (yyvsp[0].node));
		    }
#line 6353 "parse.c" /* yacc.c:1646  */
    break;

  case 112:
#line 1889 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			yyerror("class/module name must be CONSTANT");
		    /*%
			$$ = dispatch1(class_name_error, $1);
			ripper_error();
		    %*/
		    }
#line 6366 "parse.c" /* yacc.c:1646  */
    break;

  case 114:
#line 1901 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_COLON3((yyvsp[0].id));
		    /*%
			$$ = dispatch1(top_const_ref, $2);
		    %*/
		    }
#line 6378 "parse.c" /* yacc.c:1646  */
    break;

  case 115:
#line 1909 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_COLON2(0, (yyval.node));
		    /*%
			$$ = dispatch1(const_ref, $1);
		    %*/
		    }
#line 6390 "parse.c" /* yacc.c:1646  */
    break;

  case 116:
#line 1917 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_COLON2((yyvsp[-2].node), (yyvsp[0].id));
		    /*%
			$$ = dispatch2(const_path_ref, $1, $3);
		    %*/
		    }
#line 6402 "parse.c" /* yacc.c:1646  */
    break;

  case 120:
#line 1930 "parse.y" /* yacc.c:1646  */
    {
			SET_LEX_STATE(EXPR_ENDFN);
			(yyval.id) = (yyvsp[0].id);
		    }
#line 6411 "parse.c" /* yacc.c:1646  */
    break;

  case 121:
#line 1935 "parse.y" /* yacc.c:1646  */
    {
			SET_LEX_STATE(EXPR_ENDFN);
		    /*%%%*/
			(yyval.id) = (yyvsp[0].id);
		    /*%
			$$ = $1;
		    %*/
		    }
#line 6424 "parse.c" /* yacc.c:1646  */
    break;

  case 124:
#line 1950 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_LIT(ID2SYM((yyvsp[0].id)));
		    /*%
			$$ = dispatch1(symbol_literal, $1);
		    %*/
		    }
#line 6436 "parse.c" /* yacc.c:1646  */
    break;

  case 126:
#line 1961 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_UNDEF((yyvsp[0].node));
		    /*%
			$$ = rb_ary_new3(1, $1);
		    %*/
		    }
#line 6448 "parse.c" /* yacc.c:1646  */
    break;

  case 127:
#line 1968 "parse.y" /* yacc.c:1646  */
    {SET_LEX_STATE(EXPR_FNAME|EXPR_FITEM);}
#line 6454 "parse.c" /* yacc.c:1646  */
    break;

  case 128:
#line 1969 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = block_append((yyvsp[-3].node), NEW_UNDEF((yyvsp[0].node)));
		    /*%
			rb_ary_push($1, $4);
		    %*/
		    }
#line 6466 "parse.c" /* yacc.c:1646  */
    break;

  case 129:
#line 1978 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = '|'); }
#line 6472 "parse.c" /* yacc.c:1646  */
    break;

  case 130:
#line 1979 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = '^'); }
#line 6478 "parse.c" /* yacc.c:1646  */
    break;

  case 131:
#line 1980 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = '&'); }
#line 6484 "parse.c" /* yacc.c:1646  */
    break;

  case 132:
#line 1981 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = tCMP); }
#line 6490 "parse.c" /* yacc.c:1646  */
    break;

  case 133:
#line 1982 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = tEQ); }
#line 6496 "parse.c" /* yacc.c:1646  */
    break;

  case 134:
#line 1983 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = tEQQ); }
#line 6502 "parse.c" /* yacc.c:1646  */
    break;

  case 135:
#line 1984 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = tMATCH); }
#line 6508 "parse.c" /* yacc.c:1646  */
    break;

  case 136:
#line 1985 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = tNMATCH); }
#line 6514 "parse.c" /* yacc.c:1646  */
    break;

  case 137:
#line 1986 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = '>'); }
#line 6520 "parse.c" /* yacc.c:1646  */
    break;

  case 138:
#line 1987 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = tGEQ); }
#line 6526 "parse.c" /* yacc.c:1646  */
    break;

  case 139:
#line 1988 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = '<'); }
#line 6532 "parse.c" /* yacc.c:1646  */
    break;

  case 140:
#line 1989 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = tLEQ); }
#line 6538 "parse.c" /* yacc.c:1646  */
    break;

  case 141:
#line 1990 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = tNEQ); }
#line 6544 "parse.c" /* yacc.c:1646  */
    break;

  case 142:
#line 1991 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = tLSHFT); }
#line 6550 "parse.c" /* yacc.c:1646  */
    break;

  case 143:
#line 1992 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = tRSHFT); }
#line 6556 "parse.c" /* yacc.c:1646  */
    break;

  case 144:
#line 1993 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = '+'); }
#line 6562 "parse.c" /* yacc.c:1646  */
    break;

  case 145:
#line 1994 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = '-'); }
#line 6568 "parse.c" /* yacc.c:1646  */
    break;

  case 146:
#line 1995 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = '*'); }
#line 6574 "parse.c" /* yacc.c:1646  */
    break;

  case 147:
#line 1996 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = '*'); }
#line 6580 "parse.c" /* yacc.c:1646  */
    break;

  case 148:
#line 1997 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = '/'); }
#line 6586 "parse.c" /* yacc.c:1646  */
    break;

  case 149:
#line 1998 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = '%'); }
#line 6592 "parse.c" /* yacc.c:1646  */
    break;

  case 150:
#line 1999 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = tPOW); }
#line 6598 "parse.c" /* yacc.c:1646  */
    break;

  case 151:
#line 2000 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = tDSTAR); }
#line 6604 "parse.c" /* yacc.c:1646  */
    break;

  case 152:
#line 2001 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = '!'); }
#line 6610 "parse.c" /* yacc.c:1646  */
    break;

  case 153:
#line 2002 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = '~'); }
#line 6616 "parse.c" /* yacc.c:1646  */
    break;

  case 154:
#line 2003 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = tUPLUS); }
#line 6622 "parse.c" /* yacc.c:1646  */
    break;

  case 155:
#line 2004 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = tUMINUS); }
#line 6628 "parse.c" /* yacc.c:1646  */
    break;

  case 156:
#line 2005 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = tAREF); }
#line 6634 "parse.c" /* yacc.c:1646  */
    break;

  case 157:
#line 2006 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = tASET); }
#line 6640 "parse.c" /* yacc.c:1646  */
    break;

  case 158:
#line 2007 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = '`'); }
#line 6646 "parse.c" /* yacc.c:1646  */
    break;

  case 200:
#line 2025 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = node_assign((yyvsp[-2].node), (yyvsp[0].node));
		    }
#line 6654 "parse.c" /* yacc.c:1646  */
    break;

  case 201:
#line 2029 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_op_assign((yyvsp[-2].node), (yyvsp[-1].id), (yyvsp[0].node));
		    }
#line 6662 "parse.c" /* yacc.c:1646  */
    break;

  case 202:
#line 2033 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			NODE *args;

			value_expr((yyvsp[0].node));
			if (!(yyvsp[-3].node)) (yyvsp[-3].node) = NEW_ZARRAY();
			if (nd_type((yyvsp[-3].node)) == NODE_BLOCK_PASS) {
			    args = NEW_ARGSCAT((yyvsp[-3].node), (yyvsp[0].node));
			}
			else {
			    args = arg_concat((yyvsp[-3].node), (yyvsp[0].node));
			}
			if ((yyvsp[-1].id) == tOROP) {
			    (yyvsp[-1].id) = 0;
			}
			else if ((yyvsp[-1].id) == tANDOP) {
			    (yyvsp[-1].id) = 1;
			}
			(yyval.node) = NEW_OP_ASGN1((yyvsp[-5].node), (yyvsp[-1].id), args);
			fixpos((yyval.node), (yyvsp[-5].node));
		    /*%
			$1 = dispatch2(aref_field, $1, escape_Qundef($3));
			$$ = dispatch3(opassign, $1, $5, $6);
		    %*/
		    }
#line 6692 "parse.c" /* yacc.c:1646  */
    break;

  case 203:
#line 2059 "parse.y" /* yacc.c:1646  */
    {
			value_expr((yyvsp[0].node));
			(yyval.node) = new_attr_op_assign((yyvsp[-4].node), (yyvsp[-3].id), (yyvsp[-2].id), (yyvsp[-1].id), (yyvsp[0].node));
		    }
#line 6701 "parse.c" /* yacc.c:1646  */
    break;

  case 204:
#line 2064 "parse.y" /* yacc.c:1646  */
    {
			value_expr((yyvsp[0].node));
			(yyval.node) = new_attr_op_assign((yyvsp[-4].node), (yyvsp[-3].id), (yyvsp[-2].id), (yyvsp[-1].id), (yyvsp[0].node));
		    }
#line 6710 "parse.c" /* yacc.c:1646  */
    break;

  case 205:
#line 2069 "parse.y" /* yacc.c:1646  */
    {
			value_expr((yyvsp[0].node));
			(yyval.node) = new_attr_op_assign((yyvsp[-4].node), ripper_id2sym(idCOLON2), (yyvsp[-2].id), (yyvsp[-1].id), (yyvsp[0].node));
		    }
#line 6719 "parse.c" /* yacc.c:1646  */
    break;

  case 206:
#line 2074 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = const_path_field((yyvsp[-4].node), (yyvsp[-2].id));
			(yyval.node) = new_const_op_assign((yyval.node), (yyvsp[-1].id), (yyvsp[0].node));
		    }
#line 6728 "parse.c" /* yacc.c:1646  */
    break;

  case 207:
#line 2079 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = top_const_field((yyvsp[-2].id));
			(yyval.node) = new_const_op_assign((yyval.node), (yyvsp[-1].id), (yyvsp[0].node));
		    }
#line 6737 "parse.c" /* yacc.c:1646  */
    break;

  case 208:
#line 2084 "parse.y" /* yacc.c:1646  */
    {
			(yyvsp[-2].node) = var_field((yyvsp[-2].node));
			(yyval.node) = backref_assign_error((yyvsp[-2].node), new_op_assign((yyvsp[-2].node), (yyvsp[-1].id), (yyvsp[0].node)));
		    }
#line 6746 "parse.c" /* yacc.c:1646  */
    break;

  case 209:
#line 2089 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			value_expr((yyvsp[-2].node));
			value_expr((yyvsp[0].node));
			(yyval.node) = NEW_DOT2((yyvsp[-2].node), (yyvsp[0].node));
		    /*%
			$$ = dispatch2(dot2, $1, $3);
		    %*/
		    }
#line 6760 "parse.c" /* yacc.c:1646  */
    break;

  case 210:
#line 2099 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			value_expr((yyvsp[-2].node));
			value_expr((yyvsp[0].node));
			(yyval.node) = NEW_DOT3((yyvsp[-2].node), (yyvsp[0].node));
		    /*%
			$$ = dispatch2(dot3, $1, $3);
		    %*/
		    }
#line 6774 "parse.c" /* yacc.c:1646  */
    break;

  case 211:
#line 2109 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = call_bin_op((yyvsp[-2].node), '+', (yyvsp[0].node));
		    /*%
			$$ = dispatch3(binary, $1, ID2SYM('+'), $3);
		    %*/
		    }
#line 6786 "parse.c" /* yacc.c:1646  */
    break;

  case 212:
#line 2117 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = call_bin_op((yyvsp[-2].node), '-', (yyvsp[0].node));
		    /*%
			$$ = dispatch3(binary, $1, ID2SYM('-'), $3);
		    %*/
		    }
#line 6798 "parse.c" /* yacc.c:1646  */
    break;

  case 213:
#line 2125 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = call_bin_op((yyvsp[-2].node), '*', (yyvsp[0].node));
		    /*%
			$$ = dispatch3(binary, $1, ID2SYM('*'), $3);
		    %*/
		    }
#line 6810 "parse.c" /* yacc.c:1646  */
    break;

  case 214:
#line 2133 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = call_bin_op((yyvsp[-2].node), '/', (yyvsp[0].node));
		    /*%
			$$ = dispatch3(binary, $1, ID2SYM('/'), $3);
		    %*/
		    }
#line 6822 "parse.c" /* yacc.c:1646  */
    break;

  case 215:
#line 2141 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = call_bin_op((yyvsp[-2].node), '%', (yyvsp[0].node));
		    /*%
			$$ = dispatch3(binary, $1, ID2SYM('%'), $3);
		    %*/
		    }
#line 6834 "parse.c" /* yacc.c:1646  */
    break;

  case 216:
#line 2149 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = call_bin_op((yyvsp[-2].node), tPOW, (yyvsp[0].node));
		    /*%
			$$ = dispatch3(binary, $1, ID2SYM(idPow), $3);
		    %*/
		    }
#line 6846 "parse.c" /* yacc.c:1646  */
    break;

  case 217:
#line 2157 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_CALL(call_bin_op((yyvsp[-2].node), tPOW, (yyvsp[0].node)), tUMINUS, 0);
		    /*%
			$$ = dispatch3(binary, $2, ID2SYM(idPow), $4);
			$$ = dispatch2(unary, ID2SYM(idUMinus), $$);
		    %*/
		    }
#line 6859 "parse.c" /* yacc.c:1646  */
    break;

  case 218:
#line 2166 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = call_uni_op((yyvsp[0].node), tUPLUS);
		    /*%
			$$ = dispatch2(unary, ID2SYM(idUPlus), $2);
		    %*/
		    }
#line 6871 "parse.c" /* yacc.c:1646  */
    break;

  case 219:
#line 2174 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = call_uni_op((yyvsp[0].node), tUMINUS);
		    /*%
			$$ = dispatch2(unary, ID2SYM(idUMinus), $2);
		    %*/
		    }
#line 6883 "parse.c" /* yacc.c:1646  */
    break;

  case 220:
#line 2182 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = call_bin_op((yyvsp[-2].node), '|', (yyvsp[0].node));
		    /*%
			$$ = dispatch3(binary, $1, ID2SYM('|'), $3);
		    %*/
		    }
#line 6895 "parse.c" /* yacc.c:1646  */
    break;

  case 221:
#line 2190 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = call_bin_op((yyvsp[-2].node), '^', (yyvsp[0].node));
		    /*%
			$$ = dispatch3(binary, $1, ID2SYM('^'), $3);
		    %*/
		    }
#line 6907 "parse.c" /* yacc.c:1646  */
    break;

  case 222:
#line 2198 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = call_bin_op((yyvsp[-2].node), '&', (yyvsp[0].node));
		    /*%
			$$ = dispatch3(binary, $1, ID2SYM('&'), $3);
		    %*/
		    }
#line 6919 "parse.c" /* yacc.c:1646  */
    break;

  case 223:
#line 2206 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = call_bin_op((yyvsp[-2].node), tCMP, (yyvsp[0].node));
		    /*%
			$$ = dispatch3(binary, $1, ID2SYM(idCmp), $3);
		    %*/
		    }
#line 6931 "parse.c" /* yacc.c:1646  */
    break;

  case 224:
#line 2214 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = call_bin_op((yyvsp[-2].node), '>', (yyvsp[0].node));
		    /*%
			$$ = dispatch3(binary, $1, ID2SYM('>'), $3);
		    %*/
		    }
#line 6943 "parse.c" /* yacc.c:1646  */
    break;

  case 225:
#line 2222 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = call_bin_op((yyvsp[-2].node), tGEQ, (yyvsp[0].node));
		    /*%
			$$ = dispatch3(binary, $1, ID2SYM(idGE), $3);
		    %*/
		    }
#line 6955 "parse.c" /* yacc.c:1646  */
    break;

  case 226:
#line 2230 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = call_bin_op((yyvsp[-2].node), '<', (yyvsp[0].node));
		    /*%
			$$ = dispatch3(binary, $1, ID2SYM('<'), $3);
		    %*/
		    }
#line 6967 "parse.c" /* yacc.c:1646  */
    break;

  case 227:
#line 2238 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = call_bin_op((yyvsp[-2].node), tLEQ, (yyvsp[0].node));
		    /*%
			$$ = dispatch3(binary, $1, ID2SYM(idLE), $3);
		    %*/
		    }
#line 6979 "parse.c" /* yacc.c:1646  */
    break;

  case 228:
#line 2246 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = call_bin_op((yyvsp[-2].node), tEQ, (yyvsp[0].node));
		    /*%
			$$ = dispatch3(binary, $1, ID2SYM(idEq), $3);
		    %*/
		    }
#line 6991 "parse.c" /* yacc.c:1646  */
    break;

  case 229:
#line 2254 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = call_bin_op((yyvsp[-2].node), tEQQ, (yyvsp[0].node));
		    /*%
			$$ = dispatch3(binary, $1, ID2SYM(idEqq), $3);
		    %*/
		    }
#line 7003 "parse.c" /* yacc.c:1646  */
    break;

  case 230:
#line 2262 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = call_bin_op((yyvsp[-2].node), tNEQ, (yyvsp[0].node));
		    /*%
			$$ = dispatch3(binary, $1, ID2SYM(idNeq), $3);
		    %*/
		    }
#line 7015 "parse.c" /* yacc.c:1646  */
    break;

  case 231:
#line 2270 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = match_op((yyvsp[-2].node), (yyvsp[0].node));
			if (nd_type((yyvsp[-2].node)) == NODE_LIT) {
			    VALUE lit = (yyvsp[-2].node)->nd_lit;
			    if (RB_TYPE_P(lit, T_REGEXP)) {
				(yyval.node)->nd_args = reg_named_capture_assign(lit);
			    }
			}
		    /*%
			$$ = dispatch3(binary, $1, ID2SYM(idEqTilde), $3);
		    %*/
		    }
#line 7033 "parse.c" /* yacc.c:1646  */
    break;

  case 232:
#line 2284 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = call_bin_op((yyvsp[-2].node), tNMATCH, (yyvsp[0].node));
		    /*%
			$$ = dispatch3(binary, $1, ID2SYM(idNeqTilde), $3);
		    %*/
		    }
#line 7045 "parse.c" /* yacc.c:1646  */
    break;

  case 233:
#line 2292 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = call_uni_op(method_cond((yyvsp[0].node)), '!');
		    /*%
			$$ = dispatch2(unary, ID2SYM('!'), $2);
		    %*/
		    }
#line 7057 "parse.c" /* yacc.c:1646  */
    break;

  case 234:
#line 2300 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = call_uni_op((yyvsp[0].node), '~');
		    /*%
			$$ = dispatch2(unary, ID2SYM('~'), $2);
		    %*/
		    }
#line 7069 "parse.c" /* yacc.c:1646  */
    break;

  case 235:
#line 2308 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = call_bin_op((yyvsp[-2].node), tLSHFT, (yyvsp[0].node));
		    /*%
			$$ = dispatch3(binary, $1, ID2SYM(idLTLT), $3);
		    %*/
		    }
#line 7081 "parse.c" /* yacc.c:1646  */
    break;

  case 236:
#line 2316 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = call_bin_op((yyvsp[-2].node), tRSHFT, (yyvsp[0].node));
		    /*%
			$$ = dispatch3(binary, $1, ID2SYM(idGTGT), $3);
		    %*/
		    }
#line 7093 "parse.c" /* yacc.c:1646  */
    break;

  case 237:
#line 2324 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = logop(NODE_AND, (yyvsp[-2].node), (yyvsp[0].node));
		    /*%
			$$ = dispatch3(binary, $1, ID2SYM(idANDOP), $3);
		    %*/
		    }
#line 7105 "parse.c" /* yacc.c:1646  */
    break;

  case 238:
#line 2332 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = logop(NODE_OR, (yyvsp[-2].node), (yyvsp[0].node));
		    /*%
			$$ = dispatch3(binary, $1, ID2SYM(idOROP), $3);
		    %*/
		    }
#line 7117 "parse.c" /* yacc.c:1646  */
    break;

  case 239:
#line 2339 "parse.y" /* yacc.c:1646  */
    {in_defined = 1;}
#line 7123 "parse.c" /* yacc.c:1646  */
    break;

  case 240:
#line 2340 "parse.y" /* yacc.c:1646  */
    {
			in_defined = 0;
		    /*%%%*/
			(yyval.node) = new_defined((yyvsp[0].node));
		    /*%
			$$ = dispatch1(defined, $4);
		    %*/
		    }
#line 7136 "parse.c" /* yacc.c:1646  */
    break;

  case 241:
#line 2349 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			value_expr((yyvsp[-5].node));
			(yyval.node) = new_if((yyvsp[-5].node), (yyvsp[-3].node), (yyvsp[0].node));
			fixpos((yyval.node), (yyvsp[-5].node));
		    /*%
			$$ = dispatch3(ifop, $1, $3, $6);
		    %*/
		    }
#line 7150 "parse.c" /* yacc.c:1646  */
    break;

  case 242:
#line 2359 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = (yyvsp[0].node);
		    }
#line 7158 "parse.c" /* yacc.c:1646  */
    break;

  case 243:
#line 2365 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			value_expr((yyvsp[0].node));
			(yyval.node) = (yyvsp[0].node);
			if (!(yyval.node)) (yyval.node) = NEW_NIL();
		    /*%
			$$ = $1;
		    %*/
		    }
#line 7172 "parse.c" /* yacc.c:1646  */
    break;

  case 245:
#line 2378 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = (yyvsp[-1].node);
		    }
#line 7180 "parse.c" /* yacc.c:1646  */
    break;

  case 246:
#line 2382 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node) ? arg_append((yyvsp[-3].node), new_hash((yyvsp[-1].node))) : (yyvsp[-3].node);
		    /*%
			$$ = arg_add_assocs($1, $3);
		    %*/
		    }
#line 7192 "parse.c" /* yacc.c:1646  */
    break;

  case 247:
#line 2390 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node) ? NEW_LIST(new_hash((yyvsp[-1].node))) : 0;
		    /*%
			$$ = arg_add_assocs(arg_new(), $1);
		    %*/
		    }
#line 7204 "parse.c" /* yacc.c:1646  */
    break;

  case 248:
#line 2400 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			value_expr((yyvsp[0].node));
			(yyval.node) = (yyvsp[0].node);
		    /*%
		    %*/
		    }
#line 7216 "parse.c" /* yacc.c:1646  */
    break;

  case 249:
#line 2408 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			value_expr((yyvsp[-2].node));
			(yyval.node) = NEW_RESCUE((yyvsp[-2].node), NEW_RESBODY(0, remove_begin((yyvsp[0].node)), 0), 0);
		    /*%
			$$ = dispatch2(rescue_mod, $1, $3);
		    %*/
		    }
#line 7229 "parse.c" /* yacc.c:1646  */
    break;

  case 250:
#line 2419 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node);
		    /*%
			$$ = dispatch1(arg_paren, escape_Qundef($2));
		    %*/
		    }
#line 7241 "parse.c" /* yacc.c:1646  */
    break;

  case 255:
#line 2435 "parse.y" /* yacc.c:1646  */
    {
		      (yyval.node) = (yyvsp[-1].node);
		    }
#line 7249 "parse.c" /* yacc.c:1646  */
    break;

  case 256:
#line 2439 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node) ? arg_append((yyvsp[-3].node), new_hash((yyvsp[-1].node))) : (yyvsp[-3].node);
		    /*%
			$$ = arg_add_assocs($1, $3);
		    %*/
		    }
#line 7261 "parse.c" /* yacc.c:1646  */
    break;

  case 257:
#line 2447 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node) ? NEW_LIST(new_hash((yyvsp[-1].node))) : 0;
		    /*%
			$$ = arg_add_assocs(arg_new(), $1);
		    %*/
		    }
#line 7273 "parse.c" /* yacc.c:1646  */
    break;

  case 258:
#line 2457 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			value_expr((yyvsp[0].node));
			(yyval.node) = NEW_LIST((yyvsp[0].node));
		    /*%
			$$ = arg_add(arg_new(), $1);
		    %*/
		    }
#line 7286 "parse.c" /* yacc.c:1646  */
    break;

  case 259:
#line 2466 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = arg_blk_pass((yyvsp[-1].node), (yyvsp[0].node));
		    /*%
			$$ = arg_add_optblock($1, $2);
		    %*/
		    }
#line 7298 "parse.c" /* yacc.c:1646  */
    break;

  case 260:
#line 2474 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node) ? NEW_LIST(new_hash((yyvsp[-1].node))) : 0;
			(yyval.node) = arg_blk_pass((yyval.node), (yyvsp[0].node));
		    /*%
			$$ = arg_add_assocs(arg_new(), $1);
			$$ = arg_add_optblock($$, $2);
		    %*/
		    }
#line 7312 "parse.c" /* yacc.c:1646  */
    break;

  case 261:
#line 2484 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node) ? arg_append((yyvsp[-3].node), new_hash((yyvsp[-1].node))) : (yyvsp[-3].node);
			(yyval.node) = arg_blk_pass((yyval.node), (yyvsp[0].node));
		    /*%
			$$ = arg_add_optblock(arg_add_assocs($1, $3), $4);
		    %*/
		    }
#line 7325 "parse.c" /* yacc.c:1646  */
    break;

  case 263:
#line 2501 "parse.y" /* yacc.c:1646  */
    {
			(yyval.val) = cmdarg_stack;
			CMDARG_PUSH(1);
		    }
#line 7334 "parse.c" /* yacc.c:1646  */
    break;

  case 264:
#line 2506 "parse.y" /* yacc.c:1646  */
    {
			/* CMDARG_POP() */
			CMDARG_SET((yyvsp[-1].val));
			(yyval.node) = (yyvsp[0].node);
		    }
#line 7344 "parse.c" /* yacc.c:1646  */
    break;

  case 265:
#line 2514 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_BLOCK_PASS((yyvsp[0].node));
		    /*%
			$$ = $2;
		    %*/
		    }
#line 7356 "parse.c" /* yacc.c:1646  */
    break;

  case 266:
#line 2524 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = (yyvsp[0].node);
		    }
#line 7364 "parse.c" /* yacc.c:1646  */
    break;

  case 267:
#line 2528 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = 0;
		    }
#line 7372 "parse.c" /* yacc.c:1646  */
    break;

  case 268:
#line 2534 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_LIST((yyvsp[0].node));
		    /*%
			$$ = arg_add(arg_new(), $1);
		    %*/
		    }
#line 7384 "parse.c" /* yacc.c:1646  */
    break;

  case 269:
#line 2542 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_SPLAT((yyvsp[0].node));
		    /*%
			$$ = arg_add_star(arg_new(), $2);
		    %*/
		    }
#line 7396 "parse.c" /* yacc.c:1646  */
    break;

  case 270:
#line 2550 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			NODE *n1;
			if ((n1 = splat_array((yyvsp[-2].node))) != 0) {
			    (yyval.node) = list_append(n1, (yyvsp[0].node));
			}
			else {
			    (yyval.node) = arg_append((yyvsp[-2].node), (yyvsp[0].node));
			}
		    /*%
			$$ = arg_add($1, $3);
		    %*/
		    }
#line 7414 "parse.c" /* yacc.c:1646  */
    break;

  case 271:
#line 2564 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			NODE *n1;
			if ((nd_type((yyvsp[0].node)) == NODE_ARRAY) && (n1 = splat_array((yyvsp[-3].node))) != 0) {
			    (yyval.node) = list_concat(n1, (yyvsp[0].node));
			}
			else {
			    (yyval.node) = arg_concat((yyvsp[-3].node), (yyvsp[0].node));
			}
		    /*%
			$$ = arg_add_star($1, $4);
		    %*/
		    }
#line 7432 "parse.c" /* yacc.c:1646  */
    break;

  case 274:
#line 2584 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			NODE *n1;
			if ((n1 = splat_array((yyvsp[-2].node))) != 0) {
			    (yyval.node) = list_append(n1, (yyvsp[0].node));
			}
			else {
			    (yyval.node) = arg_append((yyvsp[-2].node), (yyvsp[0].node));
			}
		    /*%
			$$ = mrhs_add(args2mrhs($1), $3);
		    %*/
		    }
#line 7450 "parse.c" /* yacc.c:1646  */
    break;

  case 275:
#line 2598 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			NODE *n1;
			if (nd_type((yyvsp[0].node)) == NODE_ARRAY &&
			    (n1 = splat_array((yyvsp[-3].node))) != 0) {
			    (yyval.node) = list_concat(n1, (yyvsp[0].node));
			}
			else {
			    (yyval.node) = arg_concat((yyvsp[-3].node), (yyvsp[0].node));
			}
		    /*%
			$$ = mrhs_add_star(args2mrhs($1), $4);
		    %*/
		    }
#line 7469 "parse.c" /* yacc.c:1646  */
    break;

  case 276:
#line 2613 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_SPLAT((yyvsp[0].node));
		    /*%
			$$ = mrhs_add_star(mrhs_new(), $2);
		    %*/
		    }
#line 7481 "parse.c" /* yacc.c:1646  */
    break;

  case 287:
#line 2633 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_FCALL((yyvsp[0].id), 0);
		    /*%
			$$ = method_arg(dispatch1(fcall, $1), arg_new());
		    %*/
		    }
#line 7493 "parse.c" /* yacc.c:1646  */
    break;

  case 288:
#line 2641 "parse.y" /* yacc.c:1646  */
    {
			(yyvsp[0].val) = cmdarg_stack;
			CMDARG_SET(0);
		    /*%%%*/
			(yyval.num) = ruby_sourceline;
		    /*%
		    %*/
		    }
#line 7506 "parse.c" /* yacc.c:1646  */
    break;

  case 289:
#line 2651 "parse.y" /* yacc.c:1646  */
    {
			CMDARG_SET((yyvsp[-3].val));
		    /*%%%*/
			if ((yyvsp[-1].node) == NULL) {
			    (yyval.node) = NEW_NIL();
			}
			else {
			    set_line_body((yyvsp[-1].node), (yyvsp[-2].num));
			    (yyval.node) = NEW_BEGIN((yyvsp[-1].node));
			}
			nd_set_line((yyval.node), (yyvsp[-2].num));
		    /*%
			$$ = dispatch1(begin, $3);
		    %*/
		    }
#line 7526 "parse.c" /* yacc.c:1646  */
    break;

  case 290:
#line 2666 "parse.y" /* yacc.c:1646  */
    {SET_LEX_STATE(EXPR_ENDARG);}
#line 7532 "parse.c" /* yacc.c:1646  */
    break;

  case 291:
#line 2667 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_BEGIN(0);
		    /*%
			$$ = dispatch1(paren, 0);
		    %*/
		    }
#line 7544 "parse.c" /* yacc.c:1646  */
    break;

  case 292:
#line 2675 "parse.y" /* yacc.c:1646  */
    {
			(yyvsp[0].val) = cmdarg_stack;
			CMDARG_SET(0);
		    }
#line 7553 "parse.c" /* yacc.c:1646  */
    break;

  case 293:
#line 2679 "parse.y" /* yacc.c:1646  */
    {SET_LEX_STATE(EXPR_ENDARG);}
#line 7559 "parse.c" /* yacc.c:1646  */
    break;

  case 294:
#line 2680 "parse.y" /* yacc.c:1646  */
    {
			CMDARG_SET((yyvsp[-4].val));
		    /*%%%*/
			(yyval.node) = (yyvsp[-2].node);
		    /*%
			$$ = dispatch1(paren, $3);
		    %*/
		    }
#line 7572 "parse.c" /* yacc.c:1646  */
    break;

  case 295:
#line 2689 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node);
		    /*%
			$$ = dispatch1(paren, $2);
		    %*/
		    }
#line 7584 "parse.c" /* yacc.c:1646  */
    break;

  case 296:
#line 2697 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_COLON2((yyvsp[-2].node), (yyvsp[0].id));
		    /*%
			$$ = dispatch2(const_path_ref, $1, $3);
		    %*/
		    }
#line 7596 "parse.c" /* yacc.c:1646  */
    break;

  case 297:
#line 2705 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_COLON3((yyvsp[0].id));
		    /*%
			$$ = dispatch1(top_const_ref, $2);
		    %*/
		    }
#line 7608 "parse.c" /* yacc.c:1646  */
    break;

  case 298:
#line 2713 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			if ((yyvsp[-1].node) == 0) {
			    (yyval.node) = NEW_ZARRAY(); /* zero length array*/
			}
			else {
			    (yyval.node) = (yyvsp[-1].node);
			}
		    /*%
			$$ = dispatch1(array, escape_Qundef($2));
		    %*/
		    }
#line 7625 "parse.c" /* yacc.c:1646  */
    break;

  case 299:
#line 2726 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_hash((yyvsp[-1].node));
		    /*%
			$$ = dispatch1(hash, escape_Qundef($2));
		    %*/
		    }
#line 7637 "parse.c" /* yacc.c:1646  */
    break;

  case 300:
#line 2734 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_RETURN(0);
		    /*%
			$$ = dispatch0(return0);
		    %*/
		    }
#line 7649 "parse.c" /* yacc.c:1646  */
    break;

  case 301:
#line 2742 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_yield((yyvsp[-1].node));
		    /*%
			$$ = dispatch1(yield, dispatch1(paren, $3));
		    %*/
		    }
#line 7661 "parse.c" /* yacc.c:1646  */
    break;

  case 302:
#line 2750 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_YIELD(0);
		    /*%
			$$ = dispatch1(yield, dispatch1(paren, arg_new()));
		    %*/
		    }
#line 7673 "parse.c" /* yacc.c:1646  */
    break;

  case 303:
#line 2758 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_YIELD(0);
		    /*%
			$$ = dispatch0(yield0);
		    %*/
		    }
#line 7685 "parse.c" /* yacc.c:1646  */
    break;

  case 304:
#line 2765 "parse.y" /* yacc.c:1646  */
    {in_defined = 1;}
#line 7691 "parse.c" /* yacc.c:1646  */
    break;

  case 305:
#line 2766 "parse.y" /* yacc.c:1646  */
    {
			in_defined = 0;
		    /*%%%*/
			(yyval.node) = new_defined((yyvsp[-1].node));
		    /*%
			$$ = dispatch1(defined, $5);
		    %*/
		    }
#line 7704 "parse.c" /* yacc.c:1646  */
    break;

  case 306:
#line 2775 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = call_uni_op(method_cond((yyvsp[-1].node)), '!');
		    /*%
			$$ = dispatch2(unary, ripper_intern("not"), $3);
		    %*/
		    }
#line 7716 "parse.c" /* yacc.c:1646  */
    break;

  case 307:
#line 2783 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = call_uni_op(method_cond(NEW_NIL()), '!');
		    /*%
			$$ = dispatch2(unary, ripper_intern("not"), Qnil);
		    %*/
		    }
#line 7728 "parse.c" /* yacc.c:1646  */
    break;

  case 308:
#line 2791 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyvsp[0].node)->nd_iter = (yyvsp[-1].node);
			(yyval.node) = (yyvsp[0].node);
		    /*%
			$$ = method_arg(dispatch1(fcall, $1), arg_new());
			$$ = method_add_block($$, $2);
		    %*/
		    }
#line 7742 "parse.c" /* yacc.c:1646  */
    break;

  case 310:
#line 2802 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			block_dup_check((yyvsp[-1].node)->nd_args, (yyvsp[0].node));
			(yyvsp[0].node)->nd_iter = (yyvsp[-1].node);
			(yyval.node) = (yyvsp[0].node);
		    /*%
			$$ = method_add_block($1, $2);
		    %*/
		    }
#line 7756 "parse.c" /* yacc.c:1646  */
    break;

  case 311:
#line 2812 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = (yyvsp[0].node);
		    }
#line 7764 "parse.c" /* yacc.c:1646  */
    break;

  case 312:
#line 2819 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_if((yyvsp[-4].node), (yyvsp[-2].node), (yyvsp[-1].node));
			fixpos((yyval.node), (yyvsp[-4].node));
		    /*%
			$$ = dispatch3(if, $2, $4, escape_Qundef($5));
		    %*/
		    }
#line 7777 "parse.c" /* yacc.c:1646  */
    break;

  case 313:
#line 2831 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_unless((yyvsp[-4].node), (yyvsp[-2].node), (yyvsp[-1].node));
			fixpos((yyval.node), (yyvsp[-4].node));
		    /*%
			$$ = dispatch3(unless, $2, $4, escape_Qundef($5));
		    %*/
		    }
#line 7790 "parse.c" /* yacc.c:1646  */
    break;

  case 314:
#line 2839 "parse.y" /* yacc.c:1646  */
    {COND_PUSH(1);}
#line 7796 "parse.c" /* yacc.c:1646  */
    break;

  case 315:
#line 2839 "parse.y" /* yacc.c:1646  */
    {COND_POP();}
#line 7802 "parse.c" /* yacc.c:1646  */
    break;

  case 316:
#line 2842 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_WHILE(cond((yyvsp[-4].node)), (yyvsp[-1].node), 1);
			fixpos((yyval.node), (yyvsp[-4].node));
		    /*%
			$$ = dispatch2(while, $3, $6);
		    %*/
		    }
#line 7815 "parse.c" /* yacc.c:1646  */
    break;

  case 317:
#line 2850 "parse.y" /* yacc.c:1646  */
    {COND_PUSH(1);}
#line 7821 "parse.c" /* yacc.c:1646  */
    break;

  case 318:
#line 2850 "parse.y" /* yacc.c:1646  */
    {COND_POP();}
#line 7827 "parse.c" /* yacc.c:1646  */
    break;

  case 319:
#line 2853 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_UNTIL(cond((yyvsp[-4].node)), (yyvsp[-1].node), 1);
			fixpos((yyval.node), (yyvsp[-4].node));
		    /*%
			$$ = dispatch2(until, $3, $6);
		    %*/
		    }
#line 7840 "parse.c" /* yacc.c:1646  */
    break;

  case 320:
#line 2864 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_CASE((yyvsp[-3].node), (yyvsp[-1].node));
			fixpos((yyval.node), (yyvsp[-3].node));
		    /*%
			$$ = dispatch2(case, $2, $4);
		    %*/
		    }
#line 7853 "parse.c" /* yacc.c:1646  */
    break;

  case 321:
#line 2873 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_CASE(0, (yyvsp[-1].node));
		    /*%
			$$ = dispatch2(case, Qnil, $3);
		    %*/
		    }
#line 7865 "parse.c" /* yacc.c:1646  */
    break;

  case 322:
#line 2881 "parse.y" /* yacc.c:1646  */
    {COND_PUSH(1);}
#line 7871 "parse.c" /* yacc.c:1646  */
    break;

  case 323:
#line 2883 "parse.y" /* yacc.c:1646  */
    {COND_POP();}
#line 7877 "parse.c" /* yacc.c:1646  */
    break;

  case 324:
#line 2886 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			/*
			 *  for a, b, c in e
			 *  #=>
			 *  e.each{|*x| a, b, c = x}
			 *
			 *  for a in e
			 *  #=>
			 *  e.each{|x| a, = x}
			 */
			ID id = internal_id();
			ID *tbl = ALLOC_N(ID, 2);
			NODE *m = NEW_ARGS_AUX(0, 0);
			NODE *args, *scope;

			switch (nd_type((yyvsp[-7].node))) {
			  case NODE_MASGN:
			    m->nd_next = node_assign((yyvsp[-7].node), NEW_FOR(NEW_DVAR(id), 0, 0));
			    args = new_args(m, 0, id, 0, new_args_tail(0, 0, 0));
			    break;
			  case NODE_LASGN:
			  case NODE_DASGN:
			  case NODE_DASGN_CURR:
			    (yyvsp[-7].node)->nd_value = NEW_DVAR(id);
			    m->nd_plen = 1;
			    m->nd_next = (yyvsp[-7].node);
			    args = new_args(m, 0, 0, 0, new_args_tail(0, 0, 0));
			    break;
			  default:
			    m->nd_next = node_assign(NEW_MASGN(NEW_LIST((yyvsp[-7].node)), 0), NEW_DVAR(id));
			    args = new_args(m, 0, id, 0, new_args_tail(0, 0, 0));
			    break;
			}
			scope = NEW_NODE(NODE_SCOPE, tbl, (yyvsp[-1].node), args);
			tbl[0] = 1; tbl[1] = id;
			(yyval.node) = NEW_FOR(0, (yyvsp[-4].node), scope);
			fixpos((yyval.node), (yyvsp[-7].node));
		    /*%
			$$ = dispatch3(for, $2, $5, $8);
		    %*/
		    }
#line 7924 "parse.c" /* yacc.c:1646  */
    break;

  case 325:
#line 2929 "parse.y" /* yacc.c:1646  */
    {
			if (in_def || in_single)
			    yyerror("class definition in method body");
			local_push(0);
		    /*%%%*/
			(yyval.num) = ruby_sourceline;
		    /*%
		    %*/
		    }
#line 7938 "parse.c" /* yacc.c:1646  */
    break;

  case 326:
#line 2940 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_CLASS((yyvsp[-4].node), (yyvsp[-1].node), (yyvsp[-3].node));
			set_line_body((yyvsp[-1].node), (yyvsp[-2].num));
			nd_set_line((yyval.node), (yyvsp[-2].num));
		    /*%
			$$ = dispatch3(class, $2, $3, $5);
		    %*/
			local_pop();
		    }
#line 7953 "parse.c" /* yacc.c:1646  */
    break;

  case 327:
#line 2951 "parse.y" /* yacc.c:1646  */
    {
			(yyval.num) = (in_def << 1) | in_single;
			in_def = 0;
			in_single = 0;
			local_push(0);
		    }
#line 7964 "parse.c" /* yacc.c:1646  */
    break;

  case 328:
#line 2960 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_SCLASS((yyvsp[-4].node), (yyvsp[-1].node));
			set_line_body((yyvsp[-1].node), nd_line((yyvsp[-4].node)));
			fixpos((yyval.node), (yyvsp[-4].node));
		    /*%
			$$ = dispatch2(sclass, $3, $6);
		    %*/
			local_pop();
			in_def = ((yyvsp[-3].num) >> 1) & 1;
			in_single = (yyvsp[-3].num) & 1;
		    }
#line 7981 "parse.c" /* yacc.c:1646  */
    break;

  case 329:
#line 2973 "parse.y" /* yacc.c:1646  */
    {
			if (in_def || in_single)
			    yyerror("module definition in method body");
			local_push(0);
		    /*%%%*/
			(yyval.num) = ruby_sourceline;
		    /*%
		    %*/
		    }
#line 7995 "parse.c" /* yacc.c:1646  */
    break;

  case 330:
#line 2984 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MODULE((yyvsp[-3].node), (yyvsp[-1].node));
			set_line_body((yyvsp[-1].node), (yyvsp[-2].num));
			nd_set_line((yyval.node), (yyvsp[-2].num));
		    /*%
			$$ = dispatch2(module, $2, $4);
		    %*/
			local_pop();
		    }
#line 8010 "parse.c" /* yacc.c:1646  */
    break;

  case 331:
#line 2995 "parse.y" /* yacc.c:1646  */
    {
			local_push(0);
			(yyval.id) = current_arg;
			current_arg = 0;
		    }
#line 8020 "parse.c" /* yacc.c:1646  */
    break;

  case 332:
#line 3000 "parse.y" /* yacc.c:1646  */
    {
			(yyval.num) = in_def;
			in_def = 1;
		    }
#line 8029 "parse.c" /* yacc.c:1646  */
    break;

  case 333:
#line 3007 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			NODE *body = remove_begin((yyvsp[-1].node));
			reduce_nodes(&body);
			(yyval.node) = NEW_DEFN((yyvsp[-5].id), (yyvsp[-2].node), body, METHOD_VISI_PRIVATE);
			set_line_body(body, (yyvsp[-6].num));
			nd_set_line((yyval.node), (yyvsp[-6].num));
		    /*%
			$$ = dispatch3(def, $2, $5, $6);
		    %*/
			local_pop();
			in_def = (yyvsp[-3].num) & 1;
			current_arg = (yyvsp[-4].id);
		    }
#line 8048 "parse.c" /* yacc.c:1646  */
    break;

  case 334:
#line 3021 "parse.y" /* yacc.c:1646  */
    {SET_LEX_STATE(EXPR_FNAME);}
#line 8054 "parse.c" /* yacc.c:1646  */
    break;

  case 335:
#line 3022 "parse.y" /* yacc.c:1646  */
    {
			(yyvsp[-1].num) = in_single;
			in_single = 1;
			SET_LEX_STATE(EXPR_ENDFN|EXPR_LABEL); /* force for args */
			local_push(0);
			(yyval.id) = current_arg;
			current_arg = 0;
		    }
#line 8067 "parse.c" /* yacc.c:1646  */
    break;

  case 336:
#line 3033 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			NODE *body = remove_begin((yyvsp[-1].node));
			reduce_nodes(&body);
			(yyval.node) = NEW_DEFS((yyvsp[-7].node), (yyvsp[-4].id), (yyvsp[-2].node), body);
			set_line_body(body, (yyvsp[-8].num));
			nd_set_line((yyval.node), (yyvsp[-8].num));
		    /*%
			$$ = dispatch5(defs, $2, $3, $5, $7, $8);
		    %*/
			local_pop();
			in_single = (yyvsp[-5].num) & 1;
			current_arg = (yyvsp[-3].id);
		    }
#line 8086 "parse.c" /* yacc.c:1646  */
    break;

  case 337:
#line 3048 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_BREAK(0);
		    /*%
			$$ = dispatch1(break, arg_new());
		    %*/
		    }
#line 8098 "parse.c" /* yacc.c:1646  */
    break;

  case 338:
#line 3056 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_NEXT(0);
		    /*%
			$$ = dispatch1(next, arg_new());
		    %*/
		    }
#line 8110 "parse.c" /* yacc.c:1646  */
    break;

  case 339:
#line 3064 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_REDO();
		    /*%
			$$ = dispatch0(redo);
		    %*/
		    }
#line 8122 "parse.c" /* yacc.c:1646  */
    break;

  case 340:
#line 3072 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_RETRY();
		    /*%
			$$ = dispatch0(retry);
		    %*/
		    }
#line 8134 "parse.c" /* yacc.c:1646  */
    break;

  case 341:
#line 3082 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			value_expr((yyvsp[0].node));
			(yyval.node) = (yyvsp[0].node);
			if (!(yyval.node)) (yyval.node) = NEW_NIL();
		    /*%
			$$ = $1;
		    %*/
		    }
#line 8148 "parse.c" /* yacc.c:1646  */
    break;

  case 342:
#line 3094 "parse.y" /* yacc.c:1646  */
    {
			token_info_push("begin");
		    }
#line 8156 "parse.c" /* yacc.c:1646  */
    break;

  case 343:
#line 3100 "parse.y" /* yacc.c:1646  */
    {
			token_info_push("if");
		    }
#line 8164 "parse.c" /* yacc.c:1646  */
    break;

  case 344:
#line 3106 "parse.y" /* yacc.c:1646  */
    {
			token_info_push("unless");
		    }
#line 8172 "parse.c" /* yacc.c:1646  */
    break;

  case 345:
#line 3112 "parse.y" /* yacc.c:1646  */
    {
			token_info_push("while");
		    }
#line 8180 "parse.c" /* yacc.c:1646  */
    break;

  case 346:
#line 3118 "parse.y" /* yacc.c:1646  */
    {
			token_info_push("until");
		    }
#line 8188 "parse.c" /* yacc.c:1646  */
    break;

  case 347:
#line 3124 "parse.y" /* yacc.c:1646  */
    {
			token_info_push("case");
		    }
#line 8196 "parse.c" /* yacc.c:1646  */
    break;

  case 348:
#line 3130 "parse.y" /* yacc.c:1646  */
    {
			token_info_push("for");
		    }
#line 8204 "parse.c" /* yacc.c:1646  */
    break;

  case 349:
#line 3136 "parse.y" /* yacc.c:1646  */
    {
			token_info_push("class");
		    }
#line 8212 "parse.c" /* yacc.c:1646  */
    break;

  case 350:
#line 3142 "parse.y" /* yacc.c:1646  */
    {
			token_info_push("module");
		    }
#line 8220 "parse.c" /* yacc.c:1646  */
    break;

  case 351:
#line 3148 "parse.y" /* yacc.c:1646  */
    {
			token_info_push("def");
		    /*%%%*/
			(yyval.num) = ruby_sourceline;
		    /*%
		    %*/
		    }
#line 8232 "parse.c" /* yacc.c:1646  */
    break;

  case 352:
#line 3158 "parse.y" /* yacc.c:1646  */
    {
			token_info_pop("end");
		    }
#line 8240 "parse.c" /* yacc.c:1646  */
    break;

  case 359:
#line 3188 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_if((yyvsp[-3].node), (yyvsp[-1].node), (yyvsp[0].node));
			fixpos((yyval.node), (yyvsp[-3].node));
		    /*%
			$$ = dispatch3(elsif, $2, $4, escape_Qundef($5));
		    %*/
		    }
#line 8253 "parse.c" /* yacc.c:1646  */
    break;

  case 361:
#line 3200 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[0].node);
		    /*%
			$$ = dispatch1(else, $2);
		    %*/
		    }
#line 8265 "parse.c" /* yacc.c:1646  */
    break;

  case 364:
#line 3214 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = assignable((yyvsp[0].id), 0);
		    /*%%%*/
		    /*%
			$$ = dispatch1(mlhs_paren, $$);
		    %*/
		    }
#line 8277 "parse.c" /* yacc.c:1646  */
    break;

  case 365:
#line 3222 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node);
		    /*%
			$$ = dispatch1(mlhs_paren, $2);
		    %*/
		    }
#line 8289 "parse.c" /* yacc.c:1646  */
    break;

  case 366:
#line 3232 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_LIST((yyvsp[0].node));
		    /*%
			$$ = mlhs_add(mlhs_new(), $1);
		    %*/
		    }
#line 8301 "parse.c" /* yacc.c:1646  */
    break;

  case 367:
#line 3240 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = list_append((yyvsp[-2].node), (yyvsp[0].node));
		    /*%
			$$ = mlhs_add($1, $3);
		    %*/
		    }
#line 8313 "parse.c" /* yacc.c:1646  */
    break;

  case 368:
#line 3250 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN((yyvsp[0].node), 0);
		    /*%
			$$ = $1;
		    %*/
		    }
#line 8325 "parse.c" /* yacc.c:1646  */
    break;

  case 369:
#line 3258 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = assignable((yyvsp[0].id), 0);
		    /*%%%*/
			(yyval.node) = NEW_MASGN((yyvsp[-3].node), (yyval.node));
		    /*%
			$$ = mlhs_add_star($1, $$);
		    %*/
		    }
#line 8338 "parse.c" /* yacc.c:1646  */
    break;

  case 370:
#line 3267 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = assignable((yyvsp[-2].id), 0);
		    /*%%%*/
			(yyval.node) = NEW_MASGN((yyvsp[-5].node), NEW_POSTARG((yyval.node), (yyvsp[0].node)));
		    /*%
			$$ = mlhs_add_star($1, $$);
		    %*/
		    }
#line 8351 "parse.c" /* yacc.c:1646  */
    break;

  case 371:
#line 3276 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN((yyvsp[-2].node), -1);
		    /*%
			$$ = mlhs_add_star($1, Qnil);
		    %*/
		    }
#line 8363 "parse.c" /* yacc.c:1646  */
    break;

  case 372:
#line 3284 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN((yyvsp[-4].node), NEW_POSTARG(-1, (yyvsp[0].node)));
		    /*%
			$$ = mlhs_add_star($1, $5);
		    %*/
		    }
#line 8375 "parse.c" /* yacc.c:1646  */
    break;

  case 373:
#line 3292 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = assignable((yyvsp[0].id), 0);
		    /*%%%*/
			(yyval.node) = NEW_MASGN(0, (yyval.node));
		    /*%
			$$ = mlhs_add_star(mlhs_new(), $$);
		    %*/
		    }
#line 8388 "parse.c" /* yacc.c:1646  */
    break;

  case 374:
#line 3301 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = assignable((yyvsp[-2].id), 0);
		    /*%%%*/
			(yyval.node) = NEW_MASGN(0, NEW_POSTARG((yyval.node), (yyvsp[0].node)));
		    /*%
		      #if 0
		      TODO: Check me
		      #endif
			$$ = mlhs_add_star($$, $4);
		    %*/
		    }
#line 8404 "parse.c" /* yacc.c:1646  */
    break;

  case 375:
#line 3313 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN(0, -1);
		    /*%
			$$ = mlhs_add_star(mlhs_new(), Qnil);
		    %*/
		    }
#line 8416 "parse.c" /* yacc.c:1646  */
    break;

  case 376:
#line 3321 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN(0, NEW_POSTARG(-1, (yyvsp[0].node)));
		    /*%
			$$ = mlhs_add_star(mlhs_new(), Qnil);
		    %*/
		    }
#line 8428 "parse.c" /* yacc.c:1646  */
    break;

  case 377:
#line 3332 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args_tail((yyvsp[-3].node), (yyvsp[-1].id), (yyvsp[0].id));
		    }
#line 8436 "parse.c" /* yacc.c:1646  */
    break;

  case 378:
#line 3336 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args_tail((yyvsp[-1].node), Qnone, (yyvsp[0].id));
		    }
#line 8444 "parse.c" /* yacc.c:1646  */
    break;

  case 379:
#line 3340 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args_tail(Qnone, (yyvsp[-1].id), (yyvsp[0].id));
		    }
#line 8452 "parse.c" /* yacc.c:1646  */
    break;

  case 380:
#line 3344 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args_tail(Qnone, Qnone, (yyvsp[0].id));
		    }
#line 8460 "parse.c" /* yacc.c:1646  */
    break;

  case 381:
#line 3350 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = (yyvsp[0].node);
		    }
#line 8468 "parse.c" /* yacc.c:1646  */
    break;

  case 382:
#line 3354 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args_tail(Qnone, Qnone, Qnone);
		    }
#line 8476 "parse.c" /* yacc.c:1646  */
    break;

  case 383:
#line 3360 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args((yyvsp[-5].node), (yyvsp[-3].node), (yyvsp[-1].id), Qnone, (yyvsp[0].node));
		    }
#line 8484 "parse.c" /* yacc.c:1646  */
    break;

  case 384:
#line 3364 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args((yyvsp[-7].node), (yyvsp[-5].node), (yyvsp[-3].id), (yyvsp[-1].node), (yyvsp[0].node));
		    }
#line 8492 "parse.c" /* yacc.c:1646  */
    break;

  case 385:
#line 3368 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args((yyvsp[-3].node), (yyvsp[-1].node), Qnone, Qnone, (yyvsp[0].node));
		    }
#line 8500 "parse.c" /* yacc.c:1646  */
    break;

  case 386:
#line 3372 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args((yyvsp[-5].node), (yyvsp[-3].node), Qnone, (yyvsp[-1].node), (yyvsp[0].node));
		    }
#line 8508 "parse.c" /* yacc.c:1646  */
    break;

  case 387:
#line 3376 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args((yyvsp[-3].node), Qnone, (yyvsp[-1].id), Qnone, (yyvsp[0].node));
		    }
#line 8516 "parse.c" /* yacc.c:1646  */
    break;

  case 388:
#line 3380 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args((yyvsp[-1].node), Qnone, 1, Qnone, new_args_tail(Qnone, Qnone, Qnone));
		    /*%%%*/
		    /*%
                        dispatch1(excessed_comma, $$);
		    %*/
		    }
#line 8528 "parse.c" /* yacc.c:1646  */
    break;

  case 389:
#line 3388 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args((yyvsp[-5].node), Qnone, (yyvsp[-3].id), (yyvsp[-1].node), (yyvsp[0].node));
		    }
#line 8536 "parse.c" /* yacc.c:1646  */
    break;

  case 390:
#line 3392 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args((yyvsp[-1].node), Qnone, Qnone, Qnone, (yyvsp[0].node));
		    }
#line 8544 "parse.c" /* yacc.c:1646  */
    break;

  case 391:
#line 3396 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(Qnone, (yyvsp[-3].node), (yyvsp[-1].id), Qnone, (yyvsp[0].node));
		    }
#line 8552 "parse.c" /* yacc.c:1646  */
    break;

  case 392:
#line 3400 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(Qnone, (yyvsp[-5].node), (yyvsp[-3].id), (yyvsp[-1].node), (yyvsp[0].node));
		    }
#line 8560 "parse.c" /* yacc.c:1646  */
    break;

  case 393:
#line 3404 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(Qnone, (yyvsp[-1].node), Qnone, Qnone, (yyvsp[0].node));
		    }
#line 8568 "parse.c" /* yacc.c:1646  */
    break;

  case 394:
#line 3408 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(Qnone, (yyvsp[-3].node), Qnone, (yyvsp[-1].node), (yyvsp[0].node));
		    }
#line 8576 "parse.c" /* yacc.c:1646  */
    break;

  case 395:
#line 3412 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(Qnone, Qnone, (yyvsp[-1].id), Qnone, (yyvsp[0].node));
		    }
#line 8584 "parse.c" /* yacc.c:1646  */
    break;

  case 396:
#line 3416 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(Qnone, Qnone, (yyvsp[-3].id), (yyvsp[-1].node), (yyvsp[0].node));
		    }
#line 8592 "parse.c" /* yacc.c:1646  */
    break;

  case 397:
#line 3420 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(Qnone, Qnone, Qnone, Qnone, (yyvsp[0].node));
		    }
#line 8600 "parse.c" /* yacc.c:1646  */
    break;

  case 399:
#line 3427 "parse.y" /* yacc.c:1646  */
    {
			command_start = TRUE;
		    }
#line 8608 "parse.c" /* yacc.c:1646  */
    break;

  case 400:
#line 3433 "parse.y" /* yacc.c:1646  */
    {
			current_arg = 0;
		    /*%%%*/
			(yyval.node) = 0;
		    /*%
			$$ = blockvar_new(params_new(Qnil,Qnil,Qnil,Qnil,Qnil,Qnil,Qnil),
                                          escape_Qundef($2));
		    %*/
		    }
#line 8622 "parse.c" /* yacc.c:1646  */
    break;

  case 401:
#line 3443 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = 0;
		    /*%
			$$ = blockvar_new(params_new(Qnil,Qnil,Qnil,Qnil,Qnil,Qnil,Qnil),
                                          Qnil);
		    %*/
		    }
#line 8635 "parse.c" /* yacc.c:1646  */
    break;

  case 402:
#line 3452 "parse.y" /* yacc.c:1646  */
    {
			current_arg = 0;
		    /*%%%*/
			(yyval.node) = (yyvsp[-2].node);
		    /*%
			$$ = blockvar_new(escape_Qundef($2), escape_Qundef($3));
		    %*/
		    }
#line 8648 "parse.c" /* yacc.c:1646  */
    break;

  case 403:
#line 3464 "parse.y" /* yacc.c:1646  */
    {
		      (yyval.node) = 0;
		    }
#line 8656 "parse.c" /* yacc.c:1646  */
    break;

  case 404:
#line 3468 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = 0;
		    /*%
			$$ = $3;
		    %*/
		    }
#line 8668 "parse.c" /* yacc.c:1646  */
    break;

  case 407:
#line 3494 "parse.y" /* yacc.c:1646  */
    {
			new_bv(get_id((yyvsp[0].id)));
		    /*%%%*/
		    /*%
			$$ = get_value($1);
		    %*/
		    }
#line 8680 "parse.c" /* yacc.c:1646  */
    break;

  case 408:
#line 3502 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = 0;
		    }
#line 8688 "parse.c" /* yacc.c:1646  */
    break;

  case 409:
#line 3507 "parse.y" /* yacc.c:1646  */
    {
			(yyval.vars) = dyna_push();
		    }
#line 8696 "parse.c" /* yacc.c:1646  */
    break;

  case 410:
#line 3510 "parse.y" /* yacc.c:1646  */
    {
			(yyval.num) = lpar_beg;
			lpar_beg = ++paren_nest;
		    }
#line 8705 "parse.c" /* yacc.c:1646  */
    break;

  case 411:
#line 3515 "parse.y" /* yacc.c:1646  */
    {
			(yyval.num) = ruby_sourceline;
		    }
#line 8713 "parse.c" /* yacc.c:1646  */
    break;

  case 412:
#line 3518 "parse.y" /* yacc.c:1646  */
    {
			(yyval.val) = cmdarg_stack;
			CMDARG_SET(0);
		    }
#line 8722 "parse.c" /* yacc.c:1646  */
    break;

  case 413:
#line 3523 "parse.y" /* yacc.c:1646  */
    {
			lpar_beg = (yyvsp[-4].num);
			CMDARG_SET((yyvsp[-1].val));
			CMDARG_LEXPOP();
		    /*%%%*/
			(yyval.node) = NEW_LAMBDA((yyvsp[-3].node), (yyvsp[0].node));
			nd_set_line((yyval.node), (yyvsp[-2].num));
		    /*%
			$$ = dispatch2(lambda, $3, $6);
		    %*/
			dyna_pop((yyvsp[-5].vars));
		    }
#line 8739 "parse.c" /* yacc.c:1646  */
    break;

  case 414:
#line 3538 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-2].node);
		    /*%
			$$ = dispatch1(paren, $2);
		    %*/
		    }
#line 8751 "parse.c" /* yacc.c:1646  */
    break;

  case 415:
#line 3546 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = (yyvsp[0].node);
		    }
#line 8759 "parse.c" /* yacc.c:1646  */
    break;

  case 416:
#line 3552 "parse.y" /* yacc.c:1646  */
    {
			token_info_pop("}");
			(yyval.node) = (yyvsp[-1].node);
		    }
#line 8768 "parse.c" /* yacc.c:1646  */
    break;

  case 417:
#line 3557 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = (yyvsp[-1].node);
		    }
#line 8776 "parse.c" /* yacc.c:1646  */
    break;

  case 418:
#line 3563 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.num) = ruby_sourceline;
		    /*% %*/
		    }
#line 8786 "parse.c" /* yacc.c:1646  */
    break;

  case 419:
#line 3569 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = (yyvsp[-1].node);
		    /*%%%*/
			nd_set_line((yyval.node), (yyvsp[-2].num));
		    /*% %*/
		    }
#line 8797 "parse.c" /* yacc.c:1646  */
    break;

  case 420:
#line 3578 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			if (nd_type((yyvsp[-1].node)) == NODE_YIELD) {
			    compile_error(PARSER_ARG "block given to yield");
			}
			else {
			    block_dup_check((yyvsp[-1].node)->nd_args, (yyvsp[0].node));
			}
			(yyvsp[0].node)->nd_iter = (yyvsp[-1].node);
			(yyval.node) = (yyvsp[0].node);
			fixpos((yyval.node), (yyvsp[-1].node));
		    /*%
			$$ = method_add_block($1, $2);
		    %*/
		    }
#line 8817 "parse.c" /* yacc.c:1646  */
    break;

  case 421:
#line 3594 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_QCALL((yyvsp[-2].id), (yyvsp[-3].node), (yyvsp[-1].id), (yyvsp[0].node));
		    /*%
			$$ = dispatch3(call, $1, $2, $3);
			$$ = method_optarg($$, $4);
		    %*/
		    }
#line 8830 "parse.c" /* yacc.c:1646  */
    break;

  case 422:
#line 3603 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			block_dup_check((yyvsp[-1].node), (yyvsp[0].node));
			(yyvsp[0].node)->nd_iter = NEW_QCALL((yyvsp[-3].id), (yyvsp[-4].node), (yyvsp[-2].id), (yyvsp[-1].node));
			(yyval.node) = (yyvsp[0].node);
			fixpos((yyval.node), (yyvsp[-4].node));
		    /*%
			$$ = dispatch4(command_call, $1, $2, $3, $4);
			$$ = method_add_block($$, $5);
		    %*/
		    }
#line 8846 "parse.c" /* yacc.c:1646  */
    break;

  case 423:
#line 3615 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			block_dup_check((yyvsp[-1].node), (yyvsp[0].node));
			(yyvsp[0].node)->nd_iter = NEW_QCALL((yyvsp[-3].id), (yyvsp[-4].node), (yyvsp[-2].id), (yyvsp[-1].node));
			(yyval.node) = (yyvsp[0].node);
			fixpos((yyval.node), (yyvsp[-4].node));
		    /*%
			$$ = dispatch4(command_call, $1, $2, $3, $4);
			$$ = method_add_block($$, $5);
		    %*/
		    }
#line 8862 "parse.c" /* yacc.c:1646  */
    break;

  case 424:
#line 3629 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node);
			(yyval.node)->nd_args = (yyvsp[0].node);
		    /*%
			$$ = method_arg(dispatch1(fcall, $1), $2);
		    %*/
		    }
#line 8875 "parse.c" /* yacc.c:1646  */
    break;

  case 425:
#line 3638 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.num) = ruby_sourceline;
		    /*% %*/
		    }
#line 8885 "parse.c" /* yacc.c:1646  */
    break;

  case 426:
#line 3644 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_QCALL((yyvsp[-3].id), (yyvsp[-4].node), (yyvsp[-2].id), (yyvsp[0].node));
			nd_set_line((yyval.node), (yyvsp[-1].num));
		    /*%
			$$ = dispatch3(call, $1, $2, $3);
			$$ = method_optarg($$, $5);
		    %*/
		    }
#line 8899 "parse.c" /* yacc.c:1646  */
    break;

  case 427:
#line 3654 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.num) = ruby_sourceline;
		    /*% %*/
		    }
#line 8909 "parse.c" /* yacc.c:1646  */
    break;

  case 428:
#line 3660 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_CALL((yyvsp[-4].node), (yyvsp[-2].id), (yyvsp[0].node));
			nd_set_line((yyval.node), (yyvsp[-1].num));
		    /*%
			$$ = dispatch3(call, $1, ripper_id2sym(idCOLON2), $3);
			$$ = method_optarg($$, $5);
		    %*/
		    }
#line 8923 "parse.c" /* yacc.c:1646  */
    break;

  case 429:
#line 3670 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_CALL((yyvsp[-2].node), (yyvsp[0].id), 0);
		    /*%
			$$ = dispatch3(call, $1, ID2SYM(idCOLON2), $3);
		    %*/
		    }
#line 8935 "parse.c" /* yacc.c:1646  */
    break;

  case 430:
#line 3678 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.num) = ruby_sourceline;
		    /*% %*/
		    }
#line 8945 "parse.c" /* yacc.c:1646  */
    break;

  case 431:
#line 3684 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_QCALL((yyvsp[-2].id), (yyvsp[-3].node), idCall, (yyvsp[0].node));
			nd_set_line((yyval.node), (yyvsp[-1].num));
		    /*%
			$$ = dispatch3(call, $1, $2, ID2SYM(idCall));
			$$ = method_optarg($$, $4);
		    %*/
		    }
#line 8959 "parse.c" /* yacc.c:1646  */
    break;

  case 432:
#line 3694 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.num) = ruby_sourceline;
		    /*% %*/
		    }
#line 8969 "parse.c" /* yacc.c:1646  */
    break;

  case 433:
#line 3700 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_CALL((yyvsp[-3].node), idCall, (yyvsp[0].node));
			nd_set_line((yyval.node), (yyvsp[-1].num));
		    /*%
			$$ = dispatch3(call, $1, ID2SYM(idCOLON2),
				       ID2SYM(idCall));
			$$ = method_optarg($$, $4);
		    %*/
		    }
#line 8984 "parse.c" /* yacc.c:1646  */
    break;

  case 434:
#line 3711 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_SUPER((yyvsp[0].node));
		    /*%
			$$ = dispatch1(super, $2);
		    %*/
		    }
#line 8996 "parse.c" /* yacc.c:1646  */
    break;

  case 435:
#line 3719 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_ZSUPER();
		    /*%
			$$ = dispatch0(zsuper);
		    %*/
		    }
#line 9008 "parse.c" /* yacc.c:1646  */
    break;

  case 436:
#line 3727 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			if ((yyvsp[-3].node) && nd_type((yyvsp[-3].node)) == NODE_SELF)
			    (yyval.node) = NEW_FCALL(tAREF, (yyvsp[-1].node));
			else
			    (yyval.node) = NEW_CALL((yyvsp[-3].node), tAREF, (yyvsp[-1].node));
			fixpos((yyval.node), (yyvsp[-3].node));
		    /*%
			$$ = dispatch2(aref, $1, escape_Qundef($3));
		    %*/
		    }
#line 9024 "parse.c" /* yacc.c:1646  */
    break;

  case 437:
#line 3741 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.num) = ruby_sourceline;
		    /*% %*/
		    }
#line 9034 "parse.c" /* yacc.c:1646  */
    break;

  case 438:
#line 3747 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = (yyvsp[-1].node);
		    /*%%%*/
			nd_set_line((yyval.node), (yyvsp[-2].num));
		    /*% %*/
		    }
#line 9045 "parse.c" /* yacc.c:1646  */
    break;

  case 439:
#line 3754 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.num) = ruby_sourceline;
		    /*% %*/
		    }
#line 9055 "parse.c" /* yacc.c:1646  */
    break;

  case 440:
#line 3760 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = (yyvsp[-1].node);
		    /*%%%*/
			nd_set_line((yyval.node), (yyvsp[-2].num));
		    /*% %*/
		    }
#line 9066 "parse.c" /* yacc.c:1646  */
    break;

  case 441:
#line 3768 "parse.y" /* yacc.c:1646  */
    {(yyval.vars) = dyna_push();}
#line 9072 "parse.c" /* yacc.c:1646  */
    break;

  case 442:
#line 3769 "parse.y" /* yacc.c:1646  */
    {(yyval.val) = cmdarg_stack >> 1; CMDARG_SET(0);}
#line 9078 "parse.c" /* yacc.c:1646  */
    break;

  case 443:
#line 3771 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_brace_body((yyvsp[-1].node), (yyvsp[0].node));
			dyna_pop((yyvsp[-3].vars));
			CMDARG_SET((yyvsp[-2].val));
		    }
#line 9088 "parse.c" /* yacc.c:1646  */
    break;

  case 444:
#line 3778 "parse.y" /* yacc.c:1646  */
    {(yyval.vars) = dyna_push();}
#line 9094 "parse.c" /* yacc.c:1646  */
    break;

  case 445:
#line 3779 "parse.y" /* yacc.c:1646  */
    {(yyval.val) = cmdarg_stack; CMDARG_SET(0);}
#line 9100 "parse.c" /* yacc.c:1646  */
    break;

  case 446:
#line 3781 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_do_body((yyvsp[-1].node), (yyvsp[0].node));
			dyna_pop((yyvsp[-3].vars));
			CMDARG_SET((yyvsp[-2].val));
		    }
#line 9110 "parse.c" /* yacc.c:1646  */
    break;

  case 447:
#line 3791 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_WHEN((yyvsp[-3].node), (yyvsp[-1].node), (yyvsp[0].node));
		    /*%
			$$ = dispatch3(when, $2, $4, escape_Qundef($5));
		    %*/
		    }
#line 9122 "parse.c" /* yacc.c:1646  */
    break;

  case 450:
#line 3807 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			if ((yyvsp[-3].node)) {
			    (yyvsp[-3].node) = node_assign((yyvsp[-3].node), NEW_ERRINFO());
			    (yyvsp[-1].node) = block_append((yyvsp[-3].node), (yyvsp[-1].node));
			}
			(yyval.node) = NEW_RESBODY((yyvsp[-4].node), (yyvsp[-1].node), (yyvsp[0].node));
			fixpos((yyval.node), (yyvsp[-4].node)?(yyvsp[-4].node):(yyvsp[-1].node));
		    /*%
			$$ = dispatch4(rescue,
				       escape_Qundef($2),
				       escape_Qundef($3),
				       escape_Qundef($5),
				       escape_Qundef($6));
		    %*/
		    }
#line 9143 "parse.c" /* yacc.c:1646  */
    break;

  case 452:
#line 3827 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_LIST((yyvsp[0].node));
		    /*%
			$$ = rb_ary_new3(1, $1);
		    %*/
		    }
#line 9155 "parse.c" /* yacc.c:1646  */
    break;

  case 453:
#line 3835 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			if (!((yyval.node) = splat_array((yyvsp[0].node)))) (yyval.node) = (yyvsp[0].node);
		    /*%
			$$ = $1;
		    %*/
		    }
#line 9167 "parse.c" /* yacc.c:1646  */
    break;

  case 455:
#line 3846 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = (yyvsp[0].node);
		    }
#line 9175 "parse.c" /* yacc.c:1646  */
    break;

  case 457:
#line 3853 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[0].node);
		    /*%
			$$ = dispatch1(ensure, $2);
		    %*/
		    }
#line 9187 "parse.c" /* yacc.c:1646  */
    break;

  case 460:
#line 3865 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_LIT(ID2SYM((yyvsp[0].id)));
		    /*%
			$$ = dispatch1(symbol_literal, $1);
		    %*/
		    }
#line 9199 "parse.c" /* yacc.c:1646  */
    break;

  case 462:
#line 3876 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			NODE *node = (yyvsp[0].node);
			if (!node) {
			    node = NEW_STR(STR_NEW0());
			}
			else {
			    node = evstr2dstr(node);
			}
			(yyval.node) = node;
		    /*%
			$$ = $1;
		    %*/
		    }
#line 9218 "parse.c" /* yacc.c:1646  */
    break;

  case 465:
#line 3895 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = literal_concat((yyvsp[-1].node), (yyvsp[0].node));
		    /*%
			$$ = dispatch2(string_concat, $1, $2);
		    %*/
		    }
#line 9230 "parse.c" /* yacc.c:1646  */
    break;

  case 466:
#line 3905 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_string1(heredoc_dedent((yyvsp[-1].node)));
		    }
#line 9238 "parse.c" /* yacc.c:1646  */
    break;

  case 467:
#line 3911 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_xstring(heredoc_dedent((yyvsp[-1].node)));
		    }
#line 9246 "parse.c" /* yacc.c:1646  */
    break;

  case 468:
#line 3917 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_regexp((yyvsp[-1].node), (yyvsp[0].num));
		    }
#line 9254 "parse.c" /* yacc.c:1646  */
    break;

  case 469:
#line 3923 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_ZARRAY();
		    /*%
			$$ = dispatch0(words_new);
			$$ = dispatch1(array, $$);
		    %*/
		    }
#line 9267 "parse.c" /* yacc.c:1646  */
    break;

  case 470:
#line 3932 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node);
		    /*%
			$$ = dispatch1(array, $2);
		    %*/
		    }
#line 9279 "parse.c" /* yacc.c:1646  */
    break;

  case 471:
#line 3942 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = 0;
		    /*%
			$$ = dispatch0(words_new);
		    %*/
		    }
#line 9291 "parse.c" /* yacc.c:1646  */
    break;

  case 472:
#line 3950 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = list_append((yyvsp[-2].node), evstr2dstr((yyvsp[-1].node)));
		    /*%
			$$ = dispatch2(words_add, $1, $2);
		    %*/
		    }
#line 9303 "parse.c" /* yacc.c:1646  */
    break;

  case 474:
#line 3968 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = literal_concat((yyvsp[-1].node), (yyvsp[0].node));
		    /*%
			$$ = dispatch2(word_add, $1, $2);
		    %*/
		    }
#line 9315 "parse.c" /* yacc.c:1646  */
    break;

  case 475:
#line 3978 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_ZARRAY();
		    /*%
			$$ = dispatch0(symbols_new);
			$$ = dispatch1(array, $$);
		    %*/
		    }
#line 9328 "parse.c" /* yacc.c:1646  */
    break;

  case 476:
#line 3987 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node);
		    /*%
			$$ = dispatch1(array, $2);
		    %*/
		    }
#line 9340 "parse.c" /* yacc.c:1646  */
    break;

  case 477:
#line 3997 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = 0;
		    /*%
			$$ = dispatch0(symbols_new);
		    %*/
		    }
#line 9352 "parse.c" /* yacc.c:1646  */
    break;

  case 478:
#line 4005 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyvsp[-1].node) = evstr2dstr((yyvsp[-1].node));
			if (nd_type((yyvsp[-1].node)) == NODE_DSTR) {
			    nd_set_type((yyvsp[-1].node), NODE_DSYM);
			}
			else {
			    nd_set_type((yyvsp[-1].node), NODE_LIT);
			    (yyvsp[-1].node)->nd_lit = rb_str_intern((yyvsp[-1].node)->nd_lit);
			}
			(yyval.node) = list_append((yyvsp[-2].node), (yyvsp[-1].node));
		    /*%
			$$ = dispatch2(symbols_add, $1, $2);
		    %*/
		    }
#line 9372 "parse.c" /* yacc.c:1646  */
    break;

  case 479:
#line 4023 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_ZARRAY();
		    /*%
			$$ = dispatch0(qwords_new);
			$$ = dispatch1(array, $$);
		    %*/
		    }
#line 9385 "parse.c" /* yacc.c:1646  */
    break;

  case 480:
#line 4032 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node);
		    /*%
			$$ = dispatch1(array, $2);
		    %*/
		    }
#line 9397 "parse.c" /* yacc.c:1646  */
    break;

  case 481:
#line 4042 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_ZARRAY();
		    /*%
			$$ = dispatch0(qsymbols_new);
			$$ = dispatch1(array, $$);
		    %*/
		    }
#line 9410 "parse.c" /* yacc.c:1646  */
    break;

  case 482:
#line 4051 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node);
		    /*%
			$$ = dispatch1(array, $2);
		    %*/
		    }
#line 9422 "parse.c" /* yacc.c:1646  */
    break;

  case 483:
#line 4061 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = 0;
		    /*%
			$$ = dispatch0(qwords_new);
		    %*/
		    }
#line 9434 "parse.c" /* yacc.c:1646  */
    break;

  case 484:
#line 4069 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = list_append((yyvsp[-2].node), (yyvsp[-1].node));
		    /*%
			$$ = dispatch2(qwords_add, $1, $2);
		    %*/
		    }
#line 9446 "parse.c" /* yacc.c:1646  */
    break;

  case 485:
#line 4079 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = 0;
		    /*%
			$$ = dispatch0(qsymbols_new);
		    %*/
		    }
#line 9458 "parse.c" /* yacc.c:1646  */
    break;

  case 486:
#line 4087 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			VALUE lit;
			lit = (yyvsp[-1].node)->nd_lit;
			(yyvsp[-1].node)->nd_lit = ID2SYM(rb_intern_str(lit));
			nd_set_type((yyvsp[-1].node), NODE_LIT);
			(yyval.node) = list_append((yyvsp[-2].node), (yyvsp[-1].node));
		    /*%
			$$ = dispatch2(qsymbols_add, $1, $2);
		    %*/
		    }
#line 9474 "parse.c" /* yacc.c:1646  */
    break;

  case 487:
#line 4101 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = 0;
		    /*%
			$$ = dispatch0(string_content);
		    %*/
		    }
#line 9486 "parse.c" /* yacc.c:1646  */
    break;

  case 488:
#line 4109 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = literal_concat((yyvsp[-1].node), (yyvsp[0].node));
		    /*%
			$$ = dispatch2(string_add, $1, $2);
		    %*/
		    }
#line 9498 "parse.c" /* yacc.c:1646  */
    break;

  case 489:
#line 4119 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = 0;
		    /*%
			$$ = dispatch0(xstring_new);
		    %*/
		    }
#line 9510 "parse.c" /* yacc.c:1646  */
    break;

  case 490:
#line 4127 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = literal_concat((yyvsp[-1].node), (yyvsp[0].node));
		    /*%
			$$ = dispatch2(xstring_add, $1, $2);
		    %*/
		    }
#line 9522 "parse.c" /* yacc.c:1646  */
    break;

  case 491:
#line 4137 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = 0;
		    /*%
			$$ = ripper_new_yylval(0, dispatch0(regexp_new), 0);
		    %*/
		    }
#line 9534 "parse.c" /* yacc.c:1646  */
    break;

  case 492:
#line 4145 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			NODE *head = (yyvsp[-1].node), *tail = (yyvsp[0].node);
			if (!head) {
			    (yyval.node) = tail;
			}
			else if (!tail) {
			    (yyval.node) = head;
			}
			else {
			    switch (nd_type(head)) {
			      case NODE_STR:
				nd_set_type(head, NODE_DSTR);
				break;
			      case NODE_DSTR:
				break;
			      default:
				head = list_append(NEW_DSTR(Qnil), head);
				break;
			    }
			    (yyval.node) = list_append(head, tail);
			}
		    /*%
			VALUE s1 = 1, s2 = 0, n1 = $1, n2 = $2;
			if (ripper_is_node_yylval(n1)) {
			    s1 = RNODE(n1)->nd_cval;
			    n1 = RNODE(n1)->nd_rval;
			}
			if (ripper_is_node_yylval(n2)) {
			    s2 = RNODE(n2)->nd_cval;
			    n2 = RNODE(n2)->nd_rval;
			}
			$$ = dispatch2(regexp_add, n1, n2);
			if (!s1 && s2) {
			    $$ = ripper_new_yylval(0, $$, s2);
			}
		    %*/
		    }
#line 9577 "parse.c" /* yacc.c:1646  */
    break;

  case 494:
#line 4187 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = lex_strterm;
			lex_strterm = 0;
			SET_LEX_STATE(EXPR_BEG);
		    }
#line 9587 "parse.c" /* yacc.c:1646  */
    break;

  case 495:
#line 4193 "parse.y" /* yacc.c:1646  */
    {
			lex_strterm = (yyvsp[-1].node);
		    /*%%%*/
			(yyval.node) = NEW_EVSTR((yyvsp[0].node));
		    /*%
			$$ = dispatch1(string_dvar, $3);
		    %*/
		    }
#line 9600 "parse.c" /* yacc.c:1646  */
    break;

  case 496:
#line 4202 "parse.y" /* yacc.c:1646  */
    {
			(yyvsp[0].val) = cond_stack;
			(yyval.val) = cmdarg_stack;
			COND_SET(0);
			CMDARG_SET(0);
		    }
#line 9611 "parse.c" /* yacc.c:1646  */
    break;

  case 497:
#line 4208 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = lex_strterm;
			lex_strterm = 0;
		    }
#line 9620 "parse.c" /* yacc.c:1646  */
    break;

  case 498:
#line 4212 "parse.y" /* yacc.c:1646  */
    {
			(yyval.num) = lex_state;
			SET_LEX_STATE(EXPR_BEG);
		    }
#line 9629 "parse.c" /* yacc.c:1646  */
    break;

  case 499:
#line 4216 "parse.y" /* yacc.c:1646  */
    {
			(yyval.num) = brace_nest;
			brace_nest = 0;
		    }
#line 9638 "parse.c" /* yacc.c:1646  */
    break;

  case 500:
#line 4220 "parse.y" /* yacc.c:1646  */
    {
			(yyval.num) = heredoc_indent;
			heredoc_indent = 0;
		    }
#line 9647 "parse.c" /* yacc.c:1646  */
    break;

  case 501:
#line 4225 "parse.y" /* yacc.c:1646  */
    {
			COND_SET((yyvsp[-7].val));
			CMDARG_SET((yyvsp[-6].val));
			lex_strterm = (yyvsp[-5].node);
			SET_LEX_STATE((yyvsp[-4].num));
			brace_nest = (yyvsp[-3].num);
			heredoc_indent = (yyvsp[-2].num);
			heredoc_line_indent = -1;
		    /*%%%*/
			if ((yyvsp[-1].node)) (yyvsp[-1].node)->flags &= ~NODE_FL_NEWLINE;
			(yyval.node) = new_evstr((yyvsp[-1].node));
		    /*%
			$$ = dispatch1(string_embexpr, $7);
		    %*/
		    }
#line 9667 "parse.c" /* yacc.c:1646  */
    break;

  case 502:
#line 4243 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_GVAR((yyvsp[0].id));
		    /*%
			$$ = dispatch1(var_ref, $1);
		    %*/
		    }
#line 9679 "parse.c" /* yacc.c:1646  */
    break;

  case 503:
#line 4251 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_IVAR((yyvsp[0].id));
		    /*%
			$$ = dispatch1(var_ref, $1);
		    %*/
		    }
#line 9691 "parse.c" /* yacc.c:1646  */
    break;

  case 504:
#line 4259 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_CVAR((yyvsp[0].id));
		    /*%
			$$ = dispatch1(var_ref, $1);
		    %*/
		    }
#line 9703 "parse.c" /* yacc.c:1646  */
    break;

  case 506:
#line 4270 "parse.y" /* yacc.c:1646  */
    {
			SET_LEX_STATE(EXPR_END|EXPR_ENDARG);
		    /*%%%*/
			(yyval.id) = (yyvsp[0].id);
		    /*%
			$$ = dispatch1(symbol, $2);
		    %*/
		    }
#line 9716 "parse.c" /* yacc.c:1646  */
    break;

  case 511:
#line 4287 "parse.y" /* yacc.c:1646  */
    {
			SET_LEX_STATE(EXPR_END|EXPR_ENDARG);
		    /*%%%*/
			(yyval.node) = dsym_node((yyvsp[-1].node));
		    /*%
			$$ = dispatch1(dyna_symbol, $2);
		    %*/
		    }
#line 9729 "parse.c" /* yacc.c:1646  */
    break;

  case 513:
#line 4299 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[0].node);
			(yyval.node)->nd_lit = negate_lit((yyval.node)->nd_lit);
		    /*%
			$$ = dispatch2(unary, ID2SYM(idUMinus), $2);
		    %*/
		    }
#line 9742 "parse.c" /* yacc.c:1646  */
    break;

  case 523:
#line 4322 "parse.y" /* yacc.c:1646  */
    {ifndef_ripper((yyval.id) = keyword_nil);}
#line 9748 "parse.c" /* yacc.c:1646  */
    break;

  case 524:
#line 4323 "parse.y" /* yacc.c:1646  */
    {ifndef_ripper((yyval.id) = keyword_self);}
#line 9754 "parse.c" /* yacc.c:1646  */
    break;

  case 525:
#line 4324 "parse.y" /* yacc.c:1646  */
    {ifndef_ripper((yyval.id) = keyword_true);}
#line 9760 "parse.c" /* yacc.c:1646  */
    break;

  case 526:
#line 4325 "parse.y" /* yacc.c:1646  */
    {ifndef_ripper((yyval.id) = keyword_false);}
#line 9766 "parse.c" /* yacc.c:1646  */
    break;

  case 527:
#line 4326 "parse.y" /* yacc.c:1646  */
    {ifndef_ripper((yyval.id) = keyword__FILE__);}
#line 9772 "parse.c" /* yacc.c:1646  */
    break;

  case 528:
#line 4327 "parse.y" /* yacc.c:1646  */
    {ifndef_ripper((yyval.id) = keyword__LINE__);}
#line 9778 "parse.c" /* yacc.c:1646  */
    break;

  case 529:
#line 4328 "parse.y" /* yacc.c:1646  */
    {ifndef_ripper((yyval.id) = keyword__ENCODING__);}
#line 9784 "parse.c" /* yacc.c:1646  */
    break;

  case 530:
#line 4332 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			if (!((yyval.node) = gettable((yyvsp[0].id)))) (yyval.node) = NEW_BEGIN(0);
		    /*%
			if (id_is_var(get_id($1))) {
			    $$ = dispatch1(var_ref, $1);
			}
			else {
			    $$ = dispatch1(vcall, $1);
			}
		    %*/
		    }
#line 9801 "parse.c" /* yacc.c:1646  */
    break;

  case 531:
#line 4345 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			if (!((yyval.node) = gettable((yyvsp[0].id)))) (yyval.node) = NEW_BEGIN(0);
		    /*%
			$$ = dispatch1(var_ref, $1);
		    %*/
		    }
#line 9813 "parse.c" /* yacc.c:1646  */
    break;

  case 532:
#line 4355 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = assignable((yyvsp[0].id), 0);
		    /*%%%*/
		    /*%
			$$ = dispatch1(var_field, $$);
		    %*/
		    }
#line 9825 "parse.c" /* yacc.c:1646  */
    break;

  case 533:
#line 4363 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = assignable((yyvsp[0].id), 0);
		    /*%%%*/
		    /*%
			$$ = dispatch1(var_field, $$);
		    %*/
		    }
#line 9837 "parse.c" /* yacc.c:1646  */
    break;

  case 536:
#line 4377 "parse.y" /* yacc.c:1646  */
    {
			SET_LEX_STATE(EXPR_BEG);
			command_start = TRUE;
		    }
#line 9846 "parse.c" /* yacc.c:1646  */
    break;

  case 537:
#line 4382 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = (yyvsp[-1].node);
		    }
#line 9854 "parse.c" /* yacc.c:1646  */
    break;

  case 538:
#line 4386 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = 0;
		    /*%
			$$ = Qnil;
		    %*/
		    }
#line 9866 "parse.c" /* yacc.c:1646  */
    break;

  case 539:
#line 4396 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node);
		    /*%
			$$ = dispatch1(paren, $2);
		    %*/
			SET_LEX_STATE(EXPR_BEG);
			command_start = TRUE;
		    }
#line 9880 "parse.c" /* yacc.c:1646  */
    break;

  case 540:
#line 4405 "parse.y" /* yacc.c:1646  */
    {
			(yyval.num) = parser->in_kwarg;
			parser->in_kwarg = 1;
			lex_state |= EXPR_LABEL; /* force for args */
		    }
#line 9890 "parse.c" /* yacc.c:1646  */
    break;

  case 541:
#line 4411 "parse.y" /* yacc.c:1646  */
    {
			parser->in_kwarg = !!(yyvsp[-2].num);
			(yyval.node) = (yyvsp[-1].node);
			SET_LEX_STATE(EXPR_BEG);
			command_start = TRUE;
		    }
#line 9901 "parse.c" /* yacc.c:1646  */
    break;

  case 542:
#line 4420 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args_tail((yyvsp[-3].node), (yyvsp[-1].id), (yyvsp[0].id));
		    }
#line 9909 "parse.c" /* yacc.c:1646  */
    break;

  case 543:
#line 4424 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args_tail((yyvsp[-1].node), Qnone, (yyvsp[0].id));
		    }
#line 9917 "parse.c" /* yacc.c:1646  */
    break;

  case 544:
#line 4428 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args_tail(Qnone, (yyvsp[-1].id), (yyvsp[0].id));
		    }
#line 9925 "parse.c" /* yacc.c:1646  */
    break;

  case 545:
#line 4432 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args_tail(Qnone, Qnone, (yyvsp[0].id));
		    }
#line 9933 "parse.c" /* yacc.c:1646  */
    break;

  case 546:
#line 4438 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = (yyvsp[0].node);
		    }
#line 9941 "parse.c" /* yacc.c:1646  */
    break;

  case 547:
#line 4442 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args_tail(Qnone, Qnone, Qnone);
		    }
#line 9949 "parse.c" /* yacc.c:1646  */
    break;

  case 548:
#line 4448 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args((yyvsp[-5].node), (yyvsp[-3].node), (yyvsp[-1].id), Qnone, (yyvsp[0].node));
		    }
#line 9957 "parse.c" /* yacc.c:1646  */
    break;

  case 549:
#line 4452 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args((yyvsp[-7].node), (yyvsp[-5].node), (yyvsp[-3].id), (yyvsp[-1].node), (yyvsp[0].node));
		    }
#line 9965 "parse.c" /* yacc.c:1646  */
    break;

  case 550:
#line 4456 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args((yyvsp[-3].node), (yyvsp[-1].node), Qnone, Qnone, (yyvsp[0].node));
		    }
#line 9973 "parse.c" /* yacc.c:1646  */
    break;

  case 551:
#line 4460 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args((yyvsp[-5].node), (yyvsp[-3].node), Qnone, (yyvsp[-1].node), (yyvsp[0].node));
		    }
#line 9981 "parse.c" /* yacc.c:1646  */
    break;

  case 552:
#line 4464 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args((yyvsp[-3].node), Qnone, (yyvsp[-1].id), Qnone, (yyvsp[0].node));
		    }
#line 9989 "parse.c" /* yacc.c:1646  */
    break;

  case 553:
#line 4468 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args((yyvsp[-5].node), Qnone, (yyvsp[-3].id), (yyvsp[-1].node), (yyvsp[0].node));
		    }
#line 9997 "parse.c" /* yacc.c:1646  */
    break;

  case 554:
#line 4472 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args((yyvsp[-1].node), Qnone, Qnone, Qnone, (yyvsp[0].node));
		    }
#line 10005 "parse.c" /* yacc.c:1646  */
    break;

  case 555:
#line 4476 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(Qnone, (yyvsp[-3].node), (yyvsp[-1].id), Qnone, (yyvsp[0].node));
		    }
#line 10013 "parse.c" /* yacc.c:1646  */
    break;

  case 556:
#line 4480 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(Qnone, (yyvsp[-5].node), (yyvsp[-3].id), (yyvsp[-1].node), (yyvsp[0].node));
		    }
#line 10021 "parse.c" /* yacc.c:1646  */
    break;

  case 557:
#line 4484 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(Qnone, (yyvsp[-1].node), Qnone, Qnone, (yyvsp[0].node));
		    }
#line 10029 "parse.c" /* yacc.c:1646  */
    break;

  case 558:
#line 4488 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(Qnone, (yyvsp[-3].node), Qnone, (yyvsp[-1].node), (yyvsp[0].node));
		    }
#line 10037 "parse.c" /* yacc.c:1646  */
    break;

  case 559:
#line 4492 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(Qnone, Qnone, (yyvsp[-1].id), Qnone, (yyvsp[0].node));
		    }
#line 10045 "parse.c" /* yacc.c:1646  */
    break;

  case 560:
#line 4496 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(Qnone, Qnone, (yyvsp[-3].id), (yyvsp[-1].node), (yyvsp[0].node));
		    }
#line 10053 "parse.c" /* yacc.c:1646  */
    break;

  case 561:
#line 4500 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(Qnone, Qnone, Qnone, Qnone, (yyvsp[0].node));
		    }
#line 10061 "parse.c" /* yacc.c:1646  */
    break;

  case 562:
#line 4504 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args_tail(Qnone, Qnone, Qnone);
			(yyval.node) = new_args(Qnone, Qnone, Qnone, Qnone, (yyval.node));
		    }
#line 10070 "parse.c" /* yacc.c:1646  */
    break;

  case 563:
#line 4511 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			yyerror("formal argument cannot be a constant");
			(yyval.id) = 0;
		    /*%
			$$ = dispatch1(param_error, $1);
			ripper_error();
		    %*/
		    }
#line 10084 "parse.c" /* yacc.c:1646  */
    break;

  case 564:
#line 4521 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			yyerror("formal argument cannot be an instance variable");
			(yyval.id) = 0;
		    /*%
			$$ = dispatch1(param_error, $1);
			ripper_error();
		    %*/
		    }
#line 10098 "parse.c" /* yacc.c:1646  */
    break;

  case 565:
#line 4531 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			yyerror("formal argument cannot be a global variable");
			(yyval.id) = 0;
		    /*%
			$$ = dispatch1(param_error, $1);
			ripper_error();
		    %*/
		    }
#line 10112 "parse.c" /* yacc.c:1646  */
    break;

  case 566:
#line 4541 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			yyerror("formal argument cannot be a class variable");
			(yyval.id) = 0;
		    /*%
			$$ = dispatch1(param_error, $1);
			ripper_error();
		    %*/
		    }
#line 10126 "parse.c" /* yacc.c:1646  */
    break;

  case 568:
#line 4554 "parse.y" /* yacc.c:1646  */
    {
			formal_argument(get_id((yyvsp[0].id)));
			(yyval.id) = (yyvsp[0].id);
		    }
#line 10135 "parse.c" /* yacc.c:1646  */
    break;

  case 569:
#line 4561 "parse.y" /* yacc.c:1646  */
    {
			ID id = get_id((yyvsp[0].id));
			arg_var(id);
			current_arg = id;
			(yyval.id) = (yyvsp[0].id);
		    }
#line 10146 "parse.c" /* yacc.c:1646  */
    break;

  case 570:
#line 4570 "parse.y" /* yacc.c:1646  */
    {
			current_arg = 0;
		    /*%%%*/
			(yyval.node) = NEW_ARGS_AUX((yyvsp[0].id), 1);
		    /*%
			$$ = get_value($1);
		    %*/
		    }
#line 10159 "parse.c" /* yacc.c:1646  */
    break;

  case 571:
#line 4579 "parse.y" /* yacc.c:1646  */
    {
			ID tid = internal_id();
			arg_var(tid);
		    /*%%%*/
			if (dyna_in_block()) {
			    (yyvsp[-1].node)->nd_value = NEW_DVAR(tid);
			}
			else {
			    (yyvsp[-1].node)->nd_value = NEW_LVAR(tid);
			}
			(yyval.node) = NEW_ARGS_AUX(tid, 1);
			(yyval.node)->nd_next = (yyvsp[-1].node);
		    /*%
			$$ = dispatch1(mlhs_paren, $2);
		    %*/
		    }
#line 10180 "parse.c" /* yacc.c:1646  */
    break;

  case 573:
#line 4605 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-2].node);
			(yyval.node)->nd_plen++;
			(yyval.node)->nd_next = block_append((yyval.node)->nd_next, (yyvsp[0].node)->nd_next);
			rb_gc_force_recycle((VALUE)(yyvsp[0].node));
		    /*%
			$$ = rb_ary_push($1, $3);
		    %*/
		    }
#line 10195 "parse.c" /* yacc.c:1646  */
    break;

  case 574:
#line 4619 "parse.y" /* yacc.c:1646  */
    {
			ID id = get_id((yyvsp[0].id));
			arg_var(formal_argument(id));
			current_arg = id;
			(yyval.id) = (yyvsp[0].id);
		    }
#line 10206 "parse.c" /* yacc.c:1646  */
    break;

  case 575:
#line 4628 "parse.y" /* yacc.c:1646  */
    {
			current_arg = 0;
			(yyval.node) = assignable((yyvsp[-1].id), (yyvsp[0].node));
		    /*%%%*/
			(yyval.node) = new_kw_arg((yyval.node));
		    /*%
			$$ = rb_assoc_new($$, $2);
		    %*/
		    }
#line 10220 "parse.c" /* yacc.c:1646  */
    break;

  case 576:
#line 4638 "parse.y" /* yacc.c:1646  */
    {
			current_arg = 0;
			(yyval.node) = assignable((yyvsp[0].id), (NODE *)-1);
		    /*%%%*/
			(yyval.node) = new_kw_arg((yyval.node));
		    /*%
			$$ = rb_assoc_new($$, 0);
		    %*/
		    }
#line 10234 "parse.c" /* yacc.c:1646  */
    break;

  case 577:
#line 4650 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = assignable((yyvsp[-1].id), (yyvsp[0].node));
		    /*%%%*/
			(yyval.node) = new_kw_arg((yyval.node));
		    /*%
			$$ = rb_assoc_new($$, $2);
		    %*/
		    }
#line 10247 "parse.c" /* yacc.c:1646  */
    break;

  case 578:
#line 4659 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = assignable((yyvsp[0].id), (NODE *)-1);
		    /*%%%*/
			(yyval.node) = new_kw_arg((yyval.node));
		    /*%
			$$ = rb_assoc_new($$, 0);
		    %*/
		    }
#line 10260 "parse.c" /* yacc.c:1646  */
    break;

  case 579:
#line 4670 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[0].node);
		    /*%
			$$ = rb_ary_new3(1, $1);
		    %*/
		    }
#line 10272 "parse.c" /* yacc.c:1646  */
    break;

  case 580:
#line 4678 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = kwd_append((yyvsp[-2].node), (yyvsp[0].node));
		    /*%
			$$ = rb_ary_push($1, $3);
		    %*/
		    }
#line 10284 "parse.c" /* yacc.c:1646  */
    break;

  case 581:
#line 4689 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[0].node);
		    /*%
			$$ = rb_ary_new3(1, $1);
		    %*/
		    }
#line 10296 "parse.c" /* yacc.c:1646  */
    break;

  case 582:
#line 4697 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = kwd_append((yyvsp[-2].node), (yyvsp[0].node));
		    /*%
			$$ = rb_ary_push($1, $3);
		    %*/
		    }
#line 10308 "parse.c" /* yacc.c:1646  */
    break;

  case 585:
#line 4711 "parse.y" /* yacc.c:1646  */
    {
			shadowing_lvar(get_id((yyvsp[0].id)));
			(yyval.id) = (yyvsp[0].id);
		    }
#line 10317 "parse.c" /* yacc.c:1646  */
    break;

  case 586:
#line 4716 "parse.y" /* yacc.c:1646  */
    {
			(yyval.id) = internal_id();
			arg_var((yyval.id));
		    }
#line 10326 "parse.c" /* yacc.c:1646  */
    break;

  case 587:
#line 4723 "parse.y" /* yacc.c:1646  */
    {
			current_arg = 0;
			(yyval.node) = assignable((yyvsp[-2].id), (yyvsp[0].node));
		    /*%%%*/
			(yyval.node) = NEW_OPT_ARG(0, (yyval.node));
		    /*%
			$$ = rb_assoc_new($$, $3);
		    %*/
		    }
#line 10340 "parse.c" /* yacc.c:1646  */
    break;

  case 588:
#line 4735 "parse.y" /* yacc.c:1646  */
    {
			current_arg = 0;
			(yyval.node) = assignable((yyvsp[-2].id), (yyvsp[0].node));
		    /*%%%*/
			(yyval.node) = NEW_OPT_ARG(0, (yyval.node));
		    /*%
			$$ = rb_assoc_new($$, $3);
		    %*/
		    }
#line 10354 "parse.c" /* yacc.c:1646  */
    break;

  case 589:
#line 4747 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[0].node);
		    /*%
			$$ = rb_ary_new3(1, $1);
		    %*/
		    }
#line 10366 "parse.c" /* yacc.c:1646  */
    break;

  case 590:
#line 4755 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			NODE *opts = (yyvsp[-2].node);

			while (opts->nd_next) {
			    opts = opts->nd_next;
			}
			opts->nd_next = (yyvsp[0].node);
			(yyval.node) = (yyvsp[-2].node);
		    /*%
			$$ = rb_ary_push($1, $3);
		    %*/
		    }
#line 10384 "parse.c" /* yacc.c:1646  */
    break;

  case 591:
#line 4771 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[0].node);
		    /*%
			$$ = rb_ary_new3(1, $1);
		    %*/
		    }
#line 10396 "parse.c" /* yacc.c:1646  */
    break;

  case 592:
#line 4779 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			NODE *opts = (yyvsp[-2].node);

			while (opts->nd_next) {
			    opts = opts->nd_next;
			}
			opts->nd_next = (yyvsp[0].node);
			(yyval.node) = (yyvsp[-2].node);
		    /*%
			$$ = rb_ary_push($1, $3);
		    %*/
		    }
#line 10414 "parse.c" /* yacc.c:1646  */
    break;

  case 595:
#line 4799 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			if (!is_local_id((yyvsp[0].id)))
			    yyerror("rest argument must be local variable");
		    /*% %*/
			arg_var(shadowing_lvar(get_id((yyvsp[0].id))));
		    /*%%%*/
			(yyval.id) = (yyvsp[0].id);
		    /*%
			$$ = dispatch1(rest_param, $2);
		    %*/
		    }
#line 10431 "parse.c" /* yacc.c:1646  */
    break;

  case 596:
#line 4812 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.id) = internal_id();
			arg_var((yyval.id));
		    /*%
			$$ = dispatch1(rest_param, Qnil);
		    %*/
		    }
#line 10444 "parse.c" /* yacc.c:1646  */
    break;

  case 599:
#line 4827 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			if (!is_local_id((yyvsp[0].id)))
			    yyerror("block argument must be local variable");
			else if (!dyna_in_block() && local_id((yyvsp[0].id)))
			    yyerror("duplicated block argument name");
		    /*% %*/
			arg_var(shadowing_lvar(get_id((yyvsp[0].id))));
		    /*%%%*/
			(yyval.id) = (yyvsp[0].id);
		    /*%
			$$ = dispatch1(blockarg, $2);
		    %*/
		    }
#line 10463 "parse.c" /* yacc.c:1646  */
    break;

  case 600:
#line 4844 "parse.y" /* yacc.c:1646  */
    {
			(yyval.id) = (yyvsp[0].id);
		    }
#line 10471 "parse.c" /* yacc.c:1646  */
    break;

  case 601:
#line 4848 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.id) = 0;
		    /*%
			$$ = Qundef;
		    %*/
		    }
#line 10483 "parse.c" /* yacc.c:1646  */
    break;

  case 602:
#line 4858 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			value_expr((yyvsp[0].node));
			(yyval.node) = (yyvsp[0].node);
			if (!(yyval.node)) (yyval.node) = NEW_NIL();
		    /*%
			$$ = $1;
		    %*/
		    }
#line 10497 "parse.c" /* yacc.c:1646  */
    break;

  case 603:
#line 4867 "parse.y" /* yacc.c:1646  */
    {SET_LEX_STATE(EXPR_BEG);}
#line 10503 "parse.c" /* yacc.c:1646  */
    break;

  case 604:
#line 4868 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			if ((yyvsp[-1].node) == 0) {
			    yyerror("can't define singleton method for ().");
			}
			else {
			    switch (nd_type((yyvsp[-1].node))) {
			      case NODE_STR:
			      case NODE_DSTR:
			      case NODE_XSTR:
			      case NODE_DXSTR:
			      case NODE_DREGX:
			      case NODE_LIT:
			      case NODE_ARRAY:
			      case NODE_ZARRAY:
				yyerror("can't define singleton method for literals");
			      default:
				value_expr((yyvsp[-1].node));
				break;
			    }
			}
			(yyval.node) = (yyvsp[-1].node);
		    /*%
			$$ = dispatch1(paren, $3);
		    %*/
		    }
#line 10534 "parse.c" /* yacc.c:1646  */
    break;

  case 606:
#line 4898 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node);
		    /*%
			$$ = dispatch1(assoclist_from_args, $1);
		    %*/
		    }
#line 10546 "parse.c" /* yacc.c:1646  */
    break;

  case 608:
#line 4915 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			NODE *assocs = (yyvsp[-2].node);
			NODE *tail = (yyvsp[0].node);
			if (!assocs) {
			    assocs = tail;
			}
			else if (tail) {
			    if (assocs->nd_head &&
				!tail->nd_head && nd_type(tail->nd_next) == NODE_ARRAY &&
				nd_type(tail->nd_next->nd_head) == NODE_HASH) {
				/* DSTAR */
				tail = tail->nd_next->nd_head->nd_head;
			    }
			    assocs = list_concat(assocs, tail);
			}
			(yyval.node) = assocs;
		    /*%
			$$ = rb_ary_push($1, $3);
		    %*/
		    }
#line 10572 "parse.c" /* yacc.c:1646  */
    break;

  case 609:
#line 4939 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			if (nd_type((yyvsp[-2].node)) == NODE_STR) {
			    nd_set_type((yyvsp[-2].node), NODE_LIT);
			    (yyvsp[-2].node)->nd_lit = rb_fstring((yyvsp[-2].node)->nd_lit);
			}
			(yyval.node) = list_append(NEW_LIST((yyvsp[-2].node)), (yyvsp[0].node));
		    /*%
			$$ = dispatch2(assoc_new, $1, $3);
		    %*/
		    }
#line 10588 "parse.c" /* yacc.c:1646  */
    break;

  case 610:
#line 4951 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = list_append(NEW_LIST(NEW_LIT(ID2SYM((yyvsp[-1].id)))), (yyvsp[0].node));
		    /*%
			$$ = dispatch2(assoc_new, $1, $2);
		    %*/
		    }
#line 10600 "parse.c" /* yacc.c:1646  */
    break;

  case 611:
#line 4959 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = list_append(NEW_LIST(dsym_node((yyvsp[-2].node))), (yyvsp[0].node));
		    /*%
			$$ = dispatch2(assoc_new, dispatch1(dyna_symbol, $2), $4);
		    %*/
		    }
#line 10612 "parse.c" /* yacc.c:1646  */
    break;

  case 612:
#line 4967 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			if (nd_type((yyvsp[0].node)) == NODE_HASH &&
			    !((yyvsp[0].node)->nd_head && (yyvsp[0].node)->nd_head->nd_alen))
			    (yyval.node) = 0;
			else
			    (yyval.node) = list_append(NEW_LIST(0), (yyvsp[0].node));
		    /*%
			$$ = dispatch1(assoc_splat, $2);
		    %*/
		    }
#line 10628 "parse.c" /* yacc.c:1646  */
    break;

  case 625:
#line 5009 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.id) = '.';
		    /*%
			$$ = ripper_id2sym('.');
		    %*/
		    }
#line 10640 "parse.c" /* yacc.c:1646  */
    break;

  case 626:
#line 5017 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.id) = tANDDOT;
		    /*%
			$$ = ripper_id2sym(idANDDOT);
		    %*/
		    }
#line 10652 "parse.c" /* yacc.c:1646  */
    break;

  case 628:
#line 5028 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.id) = tCOLON2;
		    /*%
			$$ = ripper_id2sym(idCOLON2);
		    %*/
		    }
#line 10664 "parse.c" /* yacc.c:1646  */
    break;

  case 638:
#line 5056 "parse.y" /* yacc.c:1646  */
    {yyerrok;}
#line 10670 "parse.c" /* yacc.c:1646  */
    break;

  case 641:
#line 5061 "parse.y" /* yacc.c:1646  */
    {yyerrok;}
#line 10676 "parse.c" /* yacc.c:1646  */
    break;

  case 642:
#line 5065 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = 0;
		    /*%
			$$ = Qundef;
		    %*/
		    }
#line 10688 "parse.c" /* yacc.c:1646  */
    break;


#line 10692 "parse.c" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      parser_yyerror (parser, YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        parser_yyerror (parser, yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval, parser);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp, parser);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  parser_yyerror (parser, YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, parser);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp, parser);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 5073 "parse.y" /* yacc.c:1906  */

# undef parser
# undef yylex
# undef yylval
# define yylval  (*parser->lval)

static int parser_regx_options(struct parser_params*);
static int parser_tokadd_string(struct parser_params*,int,int,int,long*,rb_encoding**);
static void parser_tokaddmbc(struct parser_params *parser, int c, rb_encoding *enc);
static int parser_parse_string(struct parser_params*,NODE*);
static int parser_here_document(struct parser_params*,NODE*);


# define nextc()                      parser_nextc(parser)
# define pushback(c)                  parser_pushback(parser, (c))
# define newtok()                     parser_newtok(parser)
# define tokspace(n)                  parser_tokspace(parser, (n))
# define tokadd(c)                    parser_tokadd(parser, (c))
# define tok_hex(numlen)              parser_tok_hex(parser, (numlen))
# define read_escape(flags,e)         parser_read_escape(parser, (flags), (e))
# define tokadd_escape(e)             parser_tokadd_escape(parser, (e))
# define regx_options()               parser_regx_options(parser)
# define tokadd_string(f,t,p,n,e)     parser_tokadd_string(parser,(f),(t),(p),(n),(e))
# define parse_string(n)              parser_parse_string(parser,(n))
# define tokaddmbc(c, enc)            parser_tokaddmbc(parser, (c), (enc))
# define here_document(n)             parser_here_document(parser,(n))
# define heredoc_identifier()         parser_heredoc_identifier(parser)
# define heredoc_restore(n)           parser_heredoc_restore(parser,(n))
# define whole_match_p(e,l,i)         parser_whole_match_p(parser,(e),(l),(i))
# define number_literal_suffix(f)     parser_number_literal_suffix(parser, (f))
# define set_number_literal(v, t, f)  parser_set_number_literal(parser, (v), (t), (f))
# define set_integer_literal(v, f)    parser_set_integer_literal(parser, (v), (f))

#ifndef RIPPER
# define set_yylval_str(x) (yylval.node = NEW_STR(x))
# define set_yylval_num(x) (yylval.num = (x))
# define set_yylval_id(x)  (yylval.id = (x))
# define set_yylval_name(x)  (yylval.id = (x))
# define set_yylval_literal(x) (yylval.node = NEW_LIT(x))
# define set_yylval_node(x) (yylval.node = (x))
# define yylval_id() (yylval.id)
#else
static inline VALUE
ripper_yylval_id(ID x)
{
    return ripper_new_yylval(x, ID2SYM(x), 0);
}
# define set_yylval_str(x) (yylval.val = (x))
# define set_yylval_num(x) (yylval.val = ripper_new_yylval((x), 0, 0))
# define set_yylval_id(x)  (void)(x)
# define set_yylval_name(x) (void)(yylval.val = ripper_yylval_id(x))
# define set_yylval_literal(x) (void)(x)
# define set_yylval_node(x) (void)(x)
# define yylval_id() yylval.id
#endif

#ifndef RIPPER
#define ripper_flush(p) (void)(p)
#define dispatch_scan_event(t) ((void)0)
#define dispatch_delayed_token(t) ((void)0)
#define has_delayed_token() (0)
#else
#define ripper_flush(p) ((p)->tokp = (p)->lex.pcur)

#define yylval_rval (*(RB_TYPE_P(yylval.val, T_NODE) ? &yylval.node->nd_rval : &yylval.val))

static inline VALUE
intern_sym(const char *name)
{
    ID id = rb_intern_const(name);
    return ID2SYM(id);
}

static int
ripper_has_scan_event(struct parser_params *parser)
{

    if (lex_p < parser->tokp) rb_raise(rb_eRuntimeError, "lex_p < tokp");
    return lex_p > parser->tokp;
}

static VALUE
ripper_scan_event_val(struct parser_params *parser, int t)
{
    VALUE str = STR_NEW(parser->tokp, lex_p - parser->tokp);
    VALUE rval = ripper_dispatch1(parser, ripper_token2eventid(t), str);
    ripper_flush(parser);
    return rval;
}

static void
ripper_dispatch_scan_event(struct parser_params *parser, int t)
{
    if (!ripper_has_scan_event(parser)) return;
    yylval_rval = ripper_scan_event_val(parser, t);
}
#define dispatch_scan_event(t) ripper_dispatch_scan_event(parser, t)

static void
ripper_dispatch_delayed_token(struct parser_params *parser, int t)
{
    int saved_line = ruby_sourceline;
    const char *saved_tokp = parser->tokp;

    ruby_sourceline = parser->delayed_line;
    parser->tokp = lex_pbeg + parser->delayed_col;
    yylval_rval = ripper_dispatch1(parser, ripper_token2eventid(t), parser->delayed);
    parser->delayed = Qnil;
    ruby_sourceline = saved_line;
    parser->tokp = saved_tokp;
}
#define dispatch_delayed_token(t) ripper_dispatch_delayed_token(parser, t)
#define has_delayed_token() (!NIL_P(parser->delayed))
#endif /* RIPPER */

#include "ruby/regex.h"
#include "ruby/util.h"

#define parser_encoding_name()  (current_enc->name)
#define parser_mbclen()  mbclen((lex_p-1),lex_pend,current_enc)
#define is_identchar(p,e,enc) (rb_enc_isalnum((unsigned char)(*(p)),(enc)) || (*(p)) == '_' || !ISASCII(*(p)))
#define parser_is_identchar() (!parser->eofp && is_identchar((lex_p-1),lex_pend,current_enc))

#define parser_isascii() ISASCII(*(lex_p-1))

static int
token_info_get_column(struct parser_params *parser, const char *pend)
{
    int column = 1;
    const char *p;
    for (p = lex_pbeg; p < pend; p++) {
	if (*p == '\t') {
	    column = (((column - 1) / TAB_WIDTH) + 1) * TAB_WIDTH;
	}
	column++;
    }
    return column;
}

static int
token_info_has_nonspaces(struct parser_params *parser, const char *pend)
{
    const char *p;
    for (p = lex_pbeg; p < pend; p++) {
	if (*p != ' ' && *p != '\t') {
	    return 1;
	}
    }
    return 0;
}

static void
token_info_push_gen(struct parser_params *parser, const char *token, size_t len)
{
    token_info *ptinfo;
    const char *t = lex_p - len;

    if (!parser->token_info_enabled) return;
    ptinfo = ALLOC(token_info);
    ptinfo->token = token;
    ptinfo->linenum = ruby_sourceline;
    ptinfo->column = token_info_get_column(parser, t);
    ptinfo->nonspc = token_info_has_nonspaces(parser, t);
    ptinfo->next = parser->token_info;

    parser->token_info = ptinfo;
}

static void
token_info_pop_gen(struct parser_params *parser, const char *token, size_t len)
{
    int linenum;
    token_info *ptinfo = parser->token_info;
    const char *t = lex_p - len;

    if (!ptinfo) return;
    parser->token_info = ptinfo->next;
    linenum = ruby_sourceline;
    if (parser->token_info_enabled &&
	linenum != ptinfo->linenum && !ptinfo->nonspc &&
	!token_info_has_nonspaces(parser, t) &&
	token_info_get_column(parser, t) != ptinfo->column) {
	rb_warn3L(linenum,
		  "mismatched indentations at '%s' with '%s' at %d",
		  WARN_S(token), WARN_S(ptinfo->token), WARN_I(ptinfo->linenum));
    }

    xfree(ptinfo);
}

static int
parser_precise_mbclen(struct parser_params *parser, const char *p)
{
    int len = rb_enc_precise_mbclen(p, lex_pend, current_enc);
    if (!MBCLEN_CHARFOUND_P(len)) {
	compile_error(PARSER_ARG "invalid multibyte char (%s)", parser_encoding_name());
	return -1;
    }
    return len;
}

static int
parser_yyerror(struct parser_params *parser, const char *msg)
{
#ifndef RIPPER
    const int max_line_margin = 30;
    const char *p, *pe;
    const char *pre = "", *post = "";
    const char *code = "", *caret = "", *newline = "";
    const char *lim;
    char *buf;
    long len;
    int i;

    p = lex_p;
    lim = p - lex_pbeg > max_line_margin ? p - max_line_margin : lex_pbeg;
    while (lim < p) {
	if (*(p-1) == '\n') break;
	p--;
    }

    pe = lex_p;
    lim = lex_pend - pe > max_line_margin ? pe + max_line_margin : lex_pend;
    while (pe < lim) {
	if (*pe == '\n') break;
	pe++;
    }

    len = pe - p;
    if (len > 4) {
	char *p2;

	if (len > max_line_margin * 2 + 10) {
	    if (lex_p - p > max_line_margin) {
		p = rb_enc_prev_char(p, lex_p - max_line_margin, pe, rb_enc_get(lex_lastline));
		pre = "...";
	    }
	    if (pe - lex_p > max_line_margin) {
		pe = rb_enc_prev_char(lex_p, lex_p + max_line_margin, pe, rb_enc_get(lex_lastline));
		post = "...";
	    }
	    len = pe - p;
	}
	i = (int)(lex_p - p);
	buf = ALLOCA_N(char, i+2);
	code = p;
	caret = p2 = buf;
	while (i-- > 0) {
	    *p2++ = *p++ == '\t' ? '\t' : ' ';
	}
	*p2++ = '^';
	*p2 = '\0';
	newline = "\n";
    }
    else {
	len = 0;
    }
    compile_error(PARSER_ARG "%s%s""%s%.*s%s%s""%s%s",
		  msg, newline,
		  pre, (int)len, code, post, newline,
		  pre, caret);
#else
    dispatch1(parse_error, STR_NEW2(msg));
    ripper_error();
#endif /* !RIPPER */
    return 0;
}

static void parser_prepare(struct parser_params *parser);

#ifndef RIPPER
static VALUE
debug_lines(VALUE fname)
{
    ID script_lines;
    CONST_ID(script_lines, "SCRIPT_LINES__");
    if (rb_const_defined_at(rb_cObject, script_lines)) {
	VALUE hash = rb_const_get_at(rb_cObject, script_lines);
	if (RB_TYPE_P(hash, T_HASH)) {
	    VALUE lines = rb_ary_new();
	    rb_hash_aset(hash, fname, lines);
	    return lines;
	}
    }
    return 0;
}

static VALUE
coverage(VALUE fname, int n)
{
    VALUE coverages = rb_get_coverages();
    if (RTEST(coverages) && RBASIC(coverages)->klass == 0) {
	VALUE lines = n > 0 ? rb_ary_tmp_new_fill(n) : rb_ary_tmp_new(0);
	rb_hash_aset(coverages, fname, lines);
	return lines;
    }
    return 0;
}

static int
e_option_supplied(struct parser_params *parser)
{
    return strcmp(ruby_sourcefile, "-e") == 0;
}

static VALUE
yycompile0(VALUE arg)
{
    int n;
    NODE *tree;
    struct parser_params *parser = (struct parser_params *)arg;
    VALUE cov = Qfalse;

    if (!compile_for_eval && rb_safe_level() == 0) {
	ruby_debug_lines = debug_lines(ruby_sourcefile_string);
	if (ruby_debug_lines && ruby_sourceline > 0) {
	    VALUE str = STR_NEW0();
	    n = ruby_sourceline;
	    do {
		rb_ary_push(ruby_debug_lines, str);
	    } while (--n);
	}

	if (!e_option_supplied(parser)) {
	    ruby_coverage = coverage(ruby_sourcefile_string, ruby_sourceline);
	    cov = Qtrue;
	}
    }

    parser_prepare(parser);
#ifndef RIPPER
#define RUBY_DTRACE_PARSE_HOOK(name) \
    if (RUBY_DTRACE_PARSE_##name##_ENABLED()) { \
	RUBY_DTRACE_PARSE_##name(ruby_sourcefile, ruby_sourceline); \
    }
    RUBY_DTRACE_PARSE_HOOK(BEGIN);
#endif
    n = yyparse((void*)parser);
#ifndef RIPPER
    RUBY_DTRACE_PARSE_HOOK(END);
#endif
    ruby_debug_lines = 0;
    ruby_coverage = 0;

    lex_strterm = 0;
    lex_p = lex_pbeg = lex_pend = 0;
    lex_lastline = lex_nextline = 0;
    if (parser->error_p) {
	VALUE mesg = parser->error_buffer;
	if (!mesg) {
	    mesg = rb_class_new_instance(0, 0, rb_eSyntaxError);
	}
	rb_set_errinfo(mesg);
	return 0;
    }
    tree = ruby_eval_tree;
    if (!tree) {
	tree = NEW_NIL();
    }
    else {
	VALUE opt = parser->compile_option;
	if (!opt) opt = rb_obj_hide(rb_ident_hash_new());
	rb_hash_aset(opt, rb_sym_intern_ascii_cstr("coverage_enabled"), cov);
	tree->nd_body = NEW_PRELUDE(ruby_eval_tree_begin, tree->nd_body, opt);
    }
    return (VALUE)tree;
}

static NODE*
yycompile(struct parser_params *parser, VALUE fname, int line)
{
    ruby_sourcefile_string = rb_str_new_frozen(fname);
    ruby_sourcefile = RSTRING_PTR(fname);
    ruby_sourceline = line - 1;
    return (NODE *)rb_suppress_tracing(yycompile0, (VALUE)parser);
}
#endif /* !RIPPER */

static rb_encoding *
must_be_ascii_compatible(VALUE s)
{
    rb_encoding *enc = rb_enc_get(s);
    if (!rb_enc_asciicompat(enc)) {
	rb_raise(rb_eArgError, "invalid source encoding");
    }
    return enc;
}

static VALUE
lex_get_str(struct parser_params *parser, VALUE s)
{
    char *beg, *end, *start;
    long len;

    beg = RSTRING_PTR(s);
    len = RSTRING_LEN(s);
    start = beg;
    if (lex_gets_ptr) {
	if (len == lex_gets_ptr) return Qnil;
	beg += lex_gets_ptr;
	len -= lex_gets_ptr;
    }
    end = memchr(beg, '\n', len);
    if (end) len = ++end - beg;
    lex_gets_ptr += len;
    return rb_str_subseq(s, beg - start, len);
}

static VALUE
lex_getline(struct parser_params *parser)
{
    VALUE line = (*lex_gets)(parser, lex_input);
    if (NIL_P(line)) return line;
    must_be_ascii_compatible(line);
#ifndef RIPPER
    if (ruby_debug_lines) {
	rb_enc_associate(line, current_enc);
	rb_ary_push(ruby_debug_lines, line);
    }
    if (ruby_coverage) {
	rb_ary_push(ruby_coverage, Qnil);
    }
#endif
    return line;
}

static const rb_data_type_t parser_data_type;

#ifndef RIPPER
static NODE*
parser_compile_string(VALUE vparser, VALUE fname, VALUE s, int line)
{
    struct parser_params *parser;
    NODE *node;

    TypedData_Get_Struct(vparser, struct parser_params, &parser_data_type, parser);
    lex_gets = lex_get_str;
    lex_gets_ptr = 0;
    lex_input = rb_str_new_frozen(s);
    lex_pbeg = lex_p = lex_pend = 0;

    node = yycompile(parser, fname, line);
    RB_GC_GUARD(vparser); /* prohibit tail call optimization */

    return node;
}

NODE*
rb_compile_string(const char *f, VALUE s, int line)
{
    must_be_ascii_compatible(s);
    return parser_compile_string(rb_parser_new(), rb_filesystem_str_new_cstr(f), s, line);
}

NODE*
rb_parser_compile_string(VALUE vparser, const char *f, VALUE s, int line)
{
    return rb_parser_compile_string_path(vparser, rb_filesystem_str_new_cstr(f), s, line);
}

NODE*
rb_parser_compile_string_path(VALUE vparser, VALUE f, VALUE s, int line)
{
    must_be_ascii_compatible(s);
    return parser_compile_string(vparser, f, s, line);
}

NODE*
rb_compile_cstr(const char *f, const char *s, int len, int line)
{
    VALUE str = rb_str_new(s, len);
    return parser_compile_string(rb_parser_new(), rb_filesystem_str_new_cstr(f), str, line);
}

NODE*
rb_parser_compile_cstr(VALUE vparser, const char *f, const char *s, int len, int line)
{
    VALUE str = rb_str_new(s, len);
    return parser_compile_string(vparser, rb_filesystem_str_new_cstr(f), str, line);
}

VALUE rb_io_gets_internal(VALUE io);

static VALUE
lex_io_gets(struct parser_params *parser, VALUE io)
{
    return rb_io_gets_internal(io);
}

NODE*
rb_compile_file(const char *f, VALUE file, int start)
{
    VALUE vparser = rb_parser_new();

    return rb_parser_compile_file(vparser, f, file, start);
}

NODE*
rb_parser_compile_file(VALUE vparser, const char *f, VALUE file, int start)
{
    return rb_parser_compile_file_path(vparser, rb_filesystem_str_new_cstr(f), file, start);
}

NODE*
rb_parser_compile_file_path(VALUE vparser, VALUE fname, VALUE file, int start)
{
    struct parser_params *parser;
    NODE *node;

    TypedData_Get_Struct(vparser, struct parser_params, &parser_data_type, parser);
    lex_gets = lex_io_gets;
    lex_input = file;
    lex_pbeg = lex_p = lex_pend = 0;

    node = yycompile(parser, fname, start);
    RB_GC_GUARD(vparser); /* prohibit tail call optimization */

    return node;
}
#endif  /* !RIPPER */

#define STR_FUNC_ESCAPE 0x01
#define STR_FUNC_EXPAND 0x02
#define STR_FUNC_REGEXP 0x04
#define STR_FUNC_QWORDS 0x08
#define STR_FUNC_SYMBOL 0x10
#define STR_FUNC_INDENT 0x20
#define STR_FUNC_LABEL  0x40
#define STR_TERM_END    -1

enum string_type {
    str_label  = STR_FUNC_LABEL,
    str_squote = (0),
    str_dquote = (STR_FUNC_EXPAND),
    str_xquote = (STR_FUNC_EXPAND),
    str_regexp = (STR_FUNC_REGEXP|STR_FUNC_ESCAPE|STR_FUNC_EXPAND),
    str_sword  = (STR_FUNC_QWORDS),
    str_dword  = (STR_FUNC_QWORDS|STR_FUNC_EXPAND),
    str_ssym   = (STR_FUNC_SYMBOL),
    str_dsym   = (STR_FUNC_SYMBOL|STR_FUNC_EXPAND)
};

static VALUE
parser_str_new(const char *p, long n, rb_encoding *enc, int func, rb_encoding *enc0)
{
    VALUE str;

    str = rb_enc_str_new(p, n, enc);
    if (!(func & STR_FUNC_REGEXP) && rb_enc_asciicompat(enc)) {
	if (rb_enc_str_coderange(str) == ENC_CODERANGE_7BIT) {
	}
	else if (enc0 == rb_usascii_encoding() && enc != rb_utf8_encoding()) {
	    rb_enc_associate(str, rb_ascii8bit_encoding());
	}
    }

    return str;
}

#define lex_goto_eol(parser) ((parser)->lex.pcur = (parser)->lex.pend)
#define lex_eol_p() (lex_p >= lex_pend)
#define peek(c) peek_n((c), 0)
#define peek_n(c,n) (lex_p+(n) < lex_pend && (c) == (unsigned char)lex_p[n])
#define peekc() peekc_n(0)
#define peekc_n(n) (lex_p+(n) < lex_pend ? (unsigned char)lex_p[n] : -1)

static int
parser_nextline(struct parser_params *parser)
{
    VALUE v = lex_nextline;
    lex_nextline = 0;
    if (!v) {
	if (parser->eofp)
	    return -1;

	if (!lex_input || NIL_P(v = lex_getline(parser))) {
	    parser->eofp = 1;
	    lex_goto_eol(parser);
	    return -1;
	}
	parser->cr_seen = FALSE;
    }
#ifdef RIPPER
    if (parser->tokp < lex_pend) {
	if (!has_delayed_token()) {
	    parser->delayed = rb_str_buf_new(1024);
	    rb_enc_associate(parser->delayed, current_enc);
	    rb_str_buf_cat(parser->delayed,
			   parser->tokp, lex_pend - parser->tokp);
	    parser->delayed_line = ruby_sourceline;
	    parser->delayed_col = (int)(parser->tokp - lex_pbeg);
	}
	else {
	    rb_str_buf_cat(parser->delayed,
			   parser->tokp, lex_pend - parser->tokp);
	}
    }
#endif
    if (heredoc_end > 0) {
	ruby_sourceline = heredoc_end;
	heredoc_end = 0;
    }
    ruby_sourceline++;
    parser->line_count++;
    lex_pbeg = lex_p = RSTRING_PTR(v);
    lex_pend = lex_p + RSTRING_LEN(v);
    ripper_flush(parser);
    lex_lastline = v;
    return 0;
}

static int
parser_cr(struct parser_params *parser, int c)
{
    if (peek('\n')) {
	lex_p++;
	c = '\n';
    }
    else if (!parser->cr_seen) {
	parser->cr_seen = TRUE;
	/* carried over with lex_nextline for nextc() */
	rb_warn0("encountered \\r in middle of line, treated as a mere space");
    }
    return c;
}

static inline int
parser_nextc(struct parser_params *parser)
{
    int c;

    if (UNLIKELY(lex_p == lex_pend)) {
	if (parser_nextline(parser)) return -1;
    }
    c = (unsigned char)*lex_p++;
    if (UNLIKELY(c == '\r')) {
	c = parser_cr(parser, c);
    }

    return c;
}

static void
parser_pushback(struct parser_params *parser, int c)
{
    if (c == -1) return;
    lex_p--;
    if (lex_p > lex_pbeg && lex_p[0] == '\n' && lex_p[-1] == '\r') {
	lex_p--;
    }
}

#define was_bol() (lex_p == lex_pbeg + 1)

#define tokfix() (tokenbuf[tokidx]='\0')
#define tok() tokenbuf
#define toklen() tokidx
#define toklast() (tokidx>0?tokenbuf[tokidx-1]:0)

static char*
parser_newtok(struct parser_params *parser)
{
    tokidx = 0;
    tokline = ruby_sourceline;
    if (!tokenbuf) {
	toksiz = 60;
	tokenbuf = ALLOC_N(char, 60);
    }
    if (toksiz > 4096) {
	toksiz = 60;
	REALLOC_N(tokenbuf, char, 60);
    }
    return tokenbuf;
}

static char *
parser_tokspace(struct parser_params *parser, int n)
{
    tokidx += n;

    if (tokidx >= toksiz) {
	do {toksiz *= 2;} while (toksiz < tokidx);
	REALLOC_N(tokenbuf, char, toksiz);
    }
    return &tokenbuf[tokidx-n];
}

static void
parser_tokadd(struct parser_params *parser, int c)
{
    tokenbuf[tokidx++] = (char)c;
    if (tokidx >= toksiz) {
	toksiz *= 2;
	REALLOC_N(tokenbuf, char, toksiz);
    }
}

static int
parser_tok_hex(struct parser_params *parser, size_t *numlen)
{
    int c;

    c = scan_hex(lex_p, 2, numlen);
    if (!*numlen) {
	yyerror("invalid hex escape");
	return 0;
    }
    lex_p += *numlen;
    return c;
}

#define tokcopy(n) memcpy(tokspace(n), lex_p - (n), (n))

static int
parser_tokadd_codepoint(struct parser_params *parser, rb_encoding **encp,
			int regexp_literal, int wide)
{
    size_t numlen;
    int codepoint = scan_hex(lex_p, wide ? 6 : 4, &numlen);
    if (wide ? (numlen == 0) : (numlen < 4))  {
	yyerror("invalid Unicode escape");
	return FALSE;
    }
    if (codepoint > 0x10ffff) {
	yyerror("invalid Unicode codepoint (too large)");
	return FALSE;
    }
    if ((codepoint & 0xfffff800) == 0xd800) {
	yyerror("invalid Unicode codepoint");
	return FALSE;
    }
    lex_p += numlen;
    if (regexp_literal) {
	tokcopy((int)numlen);
    }
    else if (codepoint >= 0x80) {
	*encp = rb_utf8_encoding();
	tokaddmbc(codepoint, *encp);
    }
    else {
	tokadd(codepoint);
    }
    return TRUE;
}

/* return value is for ?\u3042 */
static int
parser_tokadd_utf8(struct parser_params *parser, rb_encoding **encp,
		   int string_literal, int symbol_literal, int regexp_literal)
{
    /*
     * If string_literal is true, then we allow multiple codepoints
     * in \u{}, and add the codepoints to the current token.
     * Otherwise we're parsing a character literal and return a single
     * codepoint without adding it
     */

    const int open_brace = '{', close_brace = '}';

    if (regexp_literal) { tokadd('\\'); tokadd('u'); }

    if (peek(open_brace)) {  /* handle \u{...} form */
	int c, last = nextc();
	do c = nextc(); while (ISSPACE(c));
	pushback(c);
	while (!string_literal || c != close_brace) {
	    if (regexp_literal) tokadd(last);
	    if (!parser_tokadd_codepoint(parser, encp, regexp_literal, TRUE)) {
		return 0;
	    }
	    while (ISSPACE(c = nextc())) last = c;
	    pushback(c);
	    if (!string_literal) break;
	}

	if (c != close_brace) {
	    yyerror("unterminated Unicode escape");
	    return 0;
	}

	if (regexp_literal) tokadd(close_brace);
	nextc();
    }
    else {			/* handle \uxxxx form */
	if (!parser_tokadd_codepoint(parser, encp, regexp_literal, FALSE)) {
	    return 0;
	}
    }

    return TRUE;
}

#define ESCAPE_CONTROL 1
#define ESCAPE_META    2

static int
parser_read_escape(struct parser_params *parser, int flags,
		   rb_encoding **encp)
{
    int c;
    size_t numlen;

    switch (c = nextc()) {
      case '\\':	/* Backslash */
	return c;

      case 'n':	/* newline */
	return '\n';

      case 't':	/* horizontal tab */
	return '\t';

      case 'r':	/* carriage-return */
	return '\r';

      case 'f':	/* form-feed */
	return '\f';

      case 'v':	/* vertical tab */
	return '\13';

      case 'a':	/* alarm(bell) */
	return '\007';

      case 'e':	/* escape */
	return 033;

      case '0': case '1': case '2': case '3': /* octal constant */
      case '4': case '5': case '6': case '7':
	pushback(c);
	c = scan_oct(lex_p, 3, &numlen);
	lex_p += numlen;
	return c;

      case 'x':	/* hex constant */
	c = tok_hex(&numlen);
	if (numlen == 0) return 0;
	return c;

      case 'b':	/* backspace */
	return '\010';

      case 's':	/* space */
	return ' ';

      case 'M':
	if (flags & ESCAPE_META) goto eof;
	if ((c = nextc()) != '-') {
	    pushback(c);
	    goto eof;
	}
	if ((c = nextc()) == '\\') {
	    if (peek('u')) goto eof;
	    return read_escape(flags|ESCAPE_META, encp) | 0x80;
	}
	else if (c == -1 || !ISASCII(c)) goto eof;
	else {
	    return ((c & 0xff) | 0x80);
	}

      case 'C':
	if ((c = nextc()) != '-') {
	    pushback(c);
	    goto eof;
	}
      case 'c':
	if (flags & ESCAPE_CONTROL) goto eof;
	if ((c = nextc())== '\\') {
	    if (peek('u')) goto eof;
	    c = read_escape(flags|ESCAPE_CONTROL, encp);
	}
	else if (c == '?')
	    return 0177;
	else if (c == -1 || !ISASCII(c)) goto eof;
	return c & 0x9f;

      eof:
      case -1:
        yyerror("Invalid escape character syntax");
	return '\0';

      default:
	return c;
    }
}

static void
parser_tokaddmbc(struct parser_params *parser, int c, rb_encoding *enc)
{
    int len = rb_enc_codelen(c, enc);
    rb_enc_mbcput(c, tokspace(len), enc);
}

static int
parser_tokadd_escape(struct parser_params *parser, rb_encoding **encp)
{
    int c;
    int flags = 0;
    size_t numlen;

  first:
    switch (c = nextc()) {
      case '\n':
	return 0;		/* just ignore */

      case '0': case '1': case '2': case '3': /* octal constant */
      case '4': case '5': case '6': case '7':
	{
	    ruby_scan_oct(--lex_p, 3, &numlen);
	    if (numlen == 0) goto eof;
	    lex_p += numlen;
	    tokcopy((int)numlen + 1);
	}
	return 0;

      case 'x':	/* hex constant */
	{
	    tok_hex(&numlen);
	    if (numlen == 0) return -1;
	    tokcopy((int)numlen + 2);
	}
	return 0;

      case 'M':
	if (flags & ESCAPE_META) goto eof;
	if ((c = nextc()) != '-') {
	    pushback(c);
	    goto eof;
	}
	tokcopy(3);
	flags |= ESCAPE_META;
	goto escaped;

      case 'C':
	if (flags & ESCAPE_CONTROL) goto eof;
	if ((c = nextc()) != '-') {
	    pushback(c);
	    goto eof;
	}
	tokcopy(3);
	goto escaped;

      case 'c':
	if (flags & ESCAPE_CONTROL) goto eof;
	tokcopy(2);
	flags |= ESCAPE_CONTROL;
      escaped:
	if ((c = nextc()) == '\\') {
	    goto first;
	}
	else if (c == -1) goto eof;
	tokadd(c);
	return 0;

      eof:
      case -1:
        yyerror("Invalid escape character syntax");
	return -1;

      default:
        tokadd('\\');
	tokadd(c);
    }
    return 0;
}

static int
parser_regx_options(struct parser_params *parser)
{
    int kcode = 0;
    int kopt = 0;
    int options = 0;
    int c, opt, kc;

    newtok();
    while (c = nextc(), ISALPHA(c)) {
        if (c == 'o') {
            options |= RE_OPTION_ONCE;
        }
        else if (rb_char_to_option_kcode(c, &opt, &kc)) {
	    if (kc >= 0) {
		if (kc != rb_ascii8bit_encindex()) kcode = c;
		kopt = opt;
	    }
	    else {
		options |= opt;
	    }
        }
        else {
	    tokadd(c);
        }
    }
    options |= kopt;
    pushback(c);
    if (toklen()) {
	tokfix();
	compile_error(PARSER_ARG "unknown regexp option%s - %s",
		      toklen() > 1 ? "s" : "", tok());
    }
    return options | RE_OPTION_ENCODING(kcode);
}

static void
dispose_string(VALUE str)
{
    rb_str_free(str);
    rb_gc_force_recycle(str);
}

static int
parser_tokadd_mbchar(struct parser_params *parser, int c)
{
    int len = parser_precise_mbclen(parser, lex_p-1);
    if (len < 0) return -1;
    tokadd(c);
    lex_p += --len;
    if (len > 0) tokcopy(len);
    return c;
}

#define tokadd_mbchar(c) parser_tokadd_mbchar(parser, (c))

static inline int
simple_re_meta(int c)
{
    switch (c) {
      case '$': case '*': case '+': case '.':
      case '?': case '^': case '|':
      case ')': case ']': case '}': case '>':
	return TRUE;
      default:
	return FALSE;
    }
}

static int
parser_update_heredoc_indent(struct parser_params *parser, int c)
{
    if (heredoc_line_indent == -1) {
	if (c == '\n') heredoc_line_indent = 0;
    }
    else {
	if (c == ' ') {
	    heredoc_line_indent++;
	    return TRUE;
	}
	else if (c == '\t') {
	    int w = (heredoc_line_indent / TAB_WIDTH) + 1;
	    heredoc_line_indent = w * TAB_WIDTH;
	    return TRUE;
	}
	else if (c != '\n') {
	    if (heredoc_indent > heredoc_line_indent) {
		heredoc_indent = heredoc_line_indent;
	    }
	    heredoc_line_indent = -1;
	}
    }
    return FALSE;
}

static int
parser_tokadd_string(struct parser_params *parser,
		     int func, int term, int paren, long *nest,
		     rb_encoding **encp)
{
    int c;
    int has_nonascii = 0;
    rb_encoding *enc = *encp;
    char *errbuf = 0;
    static const char mixed_msg[] = "%s mixed within %s source";

#define mixed_error(enc1, enc2) if (!errbuf) {	\
	size_t len = sizeof(mixed_msg) - 4;	\
	len += strlen(rb_enc_name(enc1));	\
	len += strlen(rb_enc_name(enc2));	\
	errbuf = ALLOCA_N(char, len);		\
	snprintf(errbuf, len, mixed_msg,	\
		 rb_enc_name(enc1),		\
		 rb_enc_name(enc2));		\
	yyerror(errbuf);			\
    }
#define mixed_escape(beg, enc1, enc2) do {	\
	const char *pos = lex_p;		\
	lex_p = (beg);				\
	mixed_error((enc1), (enc2));		\
	lex_p = pos;				\
    } while (0)

    while ((c = nextc()) != -1) {
	if (heredoc_indent > 0) {
	    parser_update_heredoc_indent(parser, c);
	}

	if (paren && c == paren) {
	    ++*nest;
	}
	else if (c == term) {
	    if (!nest || !*nest) {
		pushback(c);
		break;
	    }
	    --*nest;
	}
	else if ((func & STR_FUNC_EXPAND) && c == '#' && lex_p < lex_pend) {
	    int c2 = *lex_p;
	    if (c2 == '$' || c2 == '@' || c2 == '{') {
		pushback(c);
		break;
	    }
	}
	else if (c == '\\') {
	    const char *beg = lex_p - 1;
	    c = nextc();
	    switch (c) {
	      case '\n':
		if (func & STR_FUNC_QWORDS) break;
		if (func & STR_FUNC_EXPAND) {
		    if (!(func & STR_FUNC_INDENT) || (heredoc_indent < 0))
			continue;
		    if (c == term) {
			c = '\\';
			goto terminate;
		    }
		}
		tokadd('\\');
		break;

	      case '\\':
		if (func & STR_FUNC_ESCAPE) tokadd(c);
		break;

	      case 'u':
		if ((func & STR_FUNC_EXPAND) == 0) {
		    tokadd('\\');
		    break;
		}
		parser_tokadd_utf8(parser, &enc, 1,
				   func & STR_FUNC_SYMBOL,
				   func & STR_FUNC_REGEXP);
		if (has_nonascii && enc != *encp) {
		    mixed_escape(beg, enc, *encp);
		}
		continue;

	      default:
		if (c == -1) return -1;
		if (!ISASCII(c)) {
		    if ((func & STR_FUNC_EXPAND) == 0) tokadd('\\');
		    goto non_ascii;
		}
		if (func & STR_FUNC_REGEXP) {
		    if (c == term && !simple_re_meta(c)) {
			tokadd(c);
			continue;
		    }
		    pushback(c);
		    if ((c = tokadd_escape(&enc)) < 0)
			return -1;
		    if (has_nonascii && enc != *encp) {
			mixed_escape(beg, enc, *encp);
		    }
		    continue;
		}
		else if (func & STR_FUNC_EXPAND) {
		    pushback(c);
		    if (func & STR_FUNC_ESCAPE) tokadd('\\');
		    c = read_escape(0, &enc);
		}
		else if ((func & STR_FUNC_QWORDS) && ISSPACE(c)) {
		    /* ignore backslashed spaces in %w */
		}
		else if (c != term && !(paren && c == paren)) {
		    tokadd('\\');
		    pushback(c);
		    continue;
		}
	    }
	}
	else if (!parser_isascii()) {
	  non_ascii:
	    has_nonascii = 1;
	    if (enc != *encp) {
		mixed_error(enc, *encp);
		continue;
	    }
	    if (tokadd_mbchar(c) == -1) return -1;
	    continue;
	}
	else if ((func & STR_FUNC_QWORDS) && ISSPACE(c)) {
	    pushback(c);
	    break;
	}
        if (c & 0x80) {
	    has_nonascii = 1;
	    if (enc != *encp) {
		mixed_error(enc, *encp);
		continue;
	    }
        }
	tokadd(c);
    }
  terminate:
    *encp = enc;
    return c;
}

#define NEW_STRTERM(func, term, paren) \
	rb_node_newnode(NODE_STRTERM, (func), (term) | ((paren) << (CHAR_BIT * 2)), 0)

#ifdef RIPPER
static void
ripper_flush_string_content(struct parser_params *parser, rb_encoding *enc)
{
    VALUE content = yylval.val;
    if (!ripper_is_node_yylval(content))
	content = ripper_new_yylval(0, 0, content);
    if (has_delayed_token()) {
	ptrdiff_t len = lex_p - parser->tokp;
	if (len > 0) {
	    rb_enc_str_buf_cat(parser->delayed, parser->tokp, len, enc);
	}
	dispatch_delayed_token(tSTRING_CONTENT);
	parser->tokp = lex_p;
	RNODE(content)->nd_rval = yylval.val;
    }
    dispatch_scan_event(tSTRING_CONTENT);
    if (yylval.val != content)
	RNODE(content)->nd_rval = yylval.val;
    yylval.val = content;
}

#define flush_string_content(enc) ripper_flush_string_content(parser, (enc))
#else
#define flush_string_content(enc) ((void)(enc))
#endif

RUBY_FUNC_EXPORTED const unsigned int ruby_global_name_punct_bits[(0x7e - 0x20 + 31) / 32];
/* this can be shared with ripper, since it's independent from struct
 * parser_params. */
#ifndef RIPPER
#define BIT(c, idx) (((c) / 32 - 1 == idx) ? (1U << ((c) % 32)) : 0)
#define SPECIAL_PUNCT(idx) ( \
	BIT('~', idx) | BIT('*', idx) | BIT('$', idx) | BIT('?', idx) | \
	BIT('!', idx) | BIT('@', idx) | BIT('/', idx) | BIT('\\', idx) | \
	BIT(';', idx) | BIT(',', idx) | BIT('.', idx) | BIT('=', idx) | \
	BIT(':', idx) | BIT('<', idx) | BIT('>', idx) | BIT('\"', idx) | \
	BIT('&', idx) | BIT('`', idx) | BIT('\'', idx) | BIT('+', idx) | \
	BIT('0', idx))
const unsigned int ruby_global_name_punct_bits[] = {
    SPECIAL_PUNCT(0),
    SPECIAL_PUNCT(1),
    SPECIAL_PUNCT(2),
};
#undef BIT
#undef SPECIAL_PUNCT
#endif

static int
parser_peek_variable_name(struct parser_params *parser)
{
    int c;
    const char *p = lex_p;

    if (p + 1 >= lex_pend) return 0;
    c = *p++;
    switch (c) {
      case '$':
	if ((c = *p) == '-') {
	    if (++p >= lex_pend) return 0;
	    c = *p;
	}
	else if (is_global_name_punct(c) || ISDIGIT(c)) {
	    return tSTRING_DVAR;
	}
	break;
      case '@':
	if ((c = *p) == '@') {
	    if (++p >= lex_pend) return 0;
	    c = *p;
	}
	break;
      case '{':
	lex_p = p;
	command_start = TRUE;
	return tSTRING_DBEG;
      default:
	return 0;
    }
    if (!ISASCII(c) || c == '_' || ISALPHA(c))
	return tSTRING_DVAR;
    return 0;
}

static inline int
parser_string_term(struct parser_params *parser, int func)
{
    if (!(func & STR_FUNC_REGEXP)) return tSTRING_END;
    set_yylval_num(regx_options());
    dispatch_scan_event(tREGEXP_END);
    return tREGEXP_END;
}

static int
parser_parse_string(struct parser_params *parser, NODE *quote)
{
    int func = (int)quote->nd_func;
    int term = nd_term(quote);
    int paren = nd_paren(quote);
    int c, space = 0;
    rb_encoding *enc = current_enc;

    if (term == STR_TERM_END) return tSTRING_END;
    c = nextc();
    if ((func & STR_FUNC_QWORDS) && ISSPACE(c)) {
	do {c = nextc();} while (ISSPACE(c));
	space = 1;
    }
    if (c == term && !quote->nd_nest) {
	if (func & STR_FUNC_QWORDS) {
	    quote->u2.id = STR_TERM_END;
	    return ' ';
	}
	return parser_string_term(parser, func);
    }
    if (space) {
	pushback(c);
	return ' ';
    }
    newtok();
    if ((func & STR_FUNC_EXPAND) && c == '#') {
	int t = parser_peek_variable_name(parser);
	if (t) return t;
	tokadd('#');
	c = nextc();
    }
    pushback(c);
    if (tokadd_string(func, term, paren, &quote->nd_nest,
		      &enc) == -1) {
	ruby_sourceline = nd_line(quote);
	if (func & STR_FUNC_REGEXP) {
	    if (parser->eofp)
		compile_error(PARSER_ARG "unterminated regexp meets end of file");
	    return tREGEXP_END;
	}
	else {
	    if (parser->eofp)
		compile_error(PARSER_ARG "unterminated string meets end of file");
	    return tSTRING_END;
	}
    }

    tokfix();
    set_yylval_str(STR_NEW3(tok(), toklen(), enc, func));
    flush_string_content(enc);

    return tSTRING_CONTENT;
}

static int
parser_heredoc_identifier(struct parser_params *parser)
{
    int c = nextc(), term, func = 0;
    int token = tSTRING_BEG;
    long len;
    int newline = 0;
    int indent = 0;

    if (c == '-') {
	c = nextc();
	func = STR_FUNC_INDENT;
    }
    else if (c == '~') {
	c = nextc();
	func = STR_FUNC_INDENT;
	indent = INT_MAX;
    }
    switch (c) {
      case '\'':
	func |= str_squote; goto quoted;
      case '"':
	func |= str_dquote; goto quoted;
      case '`':
	token = tXSTRING_BEG;
	func |= str_xquote; goto quoted;

      quoted:
	newtok();
	tokadd(func);
	term = c;
	while ((c = nextc()) != -1 && c != term) {
	    if (tokadd_mbchar(c) == -1) return 0;
	    if (!newline && c == '\n') newline = 1;
	    else if (newline) newline = 2;
	}
	if (c == -1) {
	    compile_error(PARSER_ARG "unterminated here document identifier");
	    return 0;
	}
	switch (newline) {
	  case 1:
	    rb_warn0("here document identifier ends with a newline");
	    if (--tokidx > 0 && tokenbuf[tokidx] == '\r') --tokidx;
	    break;
	  case 2:
	    compile_error(PARSER_ARG "here document identifier across newlines, never match");
	    return -1;
	}
	break;

      default:
	if (!parser_is_identchar()) {
	    pushback(c);
	    if (func & STR_FUNC_INDENT) {
		pushback(indent > 0 ? '~' : '-');
	    }
	    return 0;
	}
	newtok();
	tokadd(func |= str_dquote);
	do {
	    if (tokadd_mbchar(c) == -1) return 0;
	} while ((c = nextc()) != -1 && parser_is_identchar());
	pushback(c);
	break;
    }

    tokfix();
    dispatch_scan_event(tHEREDOC_BEG);
    len = lex_p - lex_pbeg;
    lex_goto_eol(parser);
    lex_strterm = rb_node_newnode(NODE_HEREDOC,
				  STR_NEW(tok(), toklen()),	/* nd_lit */
				  len,				/* nd_nth */
				  lex_lastline);		/* nd_orig */
    nd_set_line(lex_strterm, ruby_sourceline);
    ripper_flush(parser);
    heredoc_indent = indent;
    heredoc_line_indent = 0;
    return token;
}

static void
parser_heredoc_restore(struct parser_params *parser, NODE *here)
{
    VALUE line;

    lex_strterm = 0;
    line = here->nd_orig;
    lex_lastline = line;
    lex_pbeg = RSTRING_PTR(line);
    lex_pend = lex_pbeg + RSTRING_LEN(line);
    lex_p = lex_pbeg + here->nd_nth;
    heredoc_end = ruby_sourceline;
    ruby_sourceline = nd_line(here);
    dispose_string(here->nd_lit);
    rb_gc_force_recycle((VALUE)here);
    ripper_flush(parser);
}

static int
dedent_string(VALUE string, int width)
{
    char *str;
    long len;
    int i, col = 0;

    RSTRING_GETMEM(string, str, len);
    for (i = 0; i < len && col < width; i++) {
	if (str[i] == ' ') {
	    col++;
	}
	else if (str[i] == '\t') {
	    int n = TAB_WIDTH * (col / TAB_WIDTH + 1);
	    if (n > width) break;
	    col = n;
	}
	else {
	    break;
	}
    }
    if (!i) return 0;
    rb_str_modify(string);
    str = RSTRING_PTR(string);
    if (RSTRING_LEN(string) != len)
	rb_fatal("literal string changed: %+"PRIsVALUE, string);
    MEMMOVE(str, str + i, char, len - i);
    rb_str_set_len(string, len - i);
    return i;
}

#ifndef RIPPER
static NODE *
parser_heredoc_dedent(struct parser_params *parser, NODE *root)
{
    NODE *node, *str_node;
    int bol = TRUE;
    int indent = heredoc_indent;

    if (indent <= 0) return root;
    heredoc_indent = 0;
    if (!root) return root;

    node = str_node = root;
    if (nd_type(root) == NODE_ARRAY) str_node = root->nd_head;

    while (str_node) {
	VALUE lit = str_node->nd_lit;
	if (bol) dedent_string(lit, indent);
	bol = TRUE;

	str_node = 0;
	while ((node = node->nd_next) != 0 && nd_type(node) == NODE_ARRAY) {
	    if ((str_node = node->nd_head) != 0) {
		enum node_type type = nd_type(str_node);
		if (type == NODE_STR || type == NODE_DSTR) break;
		bol = FALSE;
		str_node = 0;
	    }
	}
    }
    return root;
}
#else /* RIPPER */
static VALUE
parser_heredoc_dedent(struct parser_params *parser, VALUE array)
{
    int indent = heredoc_indent;

    if (indent <= 0) return array;
    heredoc_indent = 0;
    dispatch2(heredoc_dedent, array, INT2NUM(indent));
    return array;
}

static VALUE
parser_dedent_string(VALUE self, VALUE input, VALUE width)
{
    int wid, col;

    StringValue(input);
    wid = NUM2UINT(width);
    col = dedent_string(input, wid);
    return INT2NUM(col);
}
#endif

static int
parser_whole_match_p(struct parser_params *parser,
    const char *eos, long len, int indent)
{
    const char *p = lex_pbeg;
    long n;

    if (indent) {
	while (*p && ISSPACE(*p)) p++;
    }
    n = lex_pend - (p + len);
    if (n < 0) return FALSE;
    if (n > 0 && p[len] != '\n') {
	if (p[len] != '\r') return FALSE;
	if (n <= 1 || p[len+1] != '\n') return FALSE;
    }
    return strncmp(eos, p, len) == 0;
}

#define NUM_SUFFIX_R   (1<<0)
#define NUM_SUFFIX_I   (1<<1)
#define NUM_SUFFIX_ALL 3

static int
parser_number_literal_suffix(struct parser_params *parser, int mask)
{
    int c, result = 0;
    const char *lastp = lex_p;

    while ((c = nextc()) != -1) {
	if ((mask & NUM_SUFFIX_I) && c == 'i') {
	    result |= (mask & NUM_SUFFIX_I);
	    mask &= ~NUM_SUFFIX_I;
	    /* r after i, rational of complex is disallowed */
	    mask &= ~NUM_SUFFIX_R;
	    continue;
	}
	if ((mask & NUM_SUFFIX_R) && c == 'r') {
	    result |= (mask & NUM_SUFFIX_R);
	    mask &= ~NUM_SUFFIX_R;
	    continue;
	}
	if (!ISASCII(c) || ISALPHA(c) || c == '_') {
	    lex_p = lastp;
	    return 0;
	}
	pushback(c);
	if (c == '.') {
	    c = peekc_n(1);
	    if (ISDIGIT(c)) {
		yyerror("unexpected fraction part after numeric literal");
		lex_p += 2;
		while (parser_is_identchar()) nextc();
	    }
	}
	break;
    }
    return result;
}

static int
parser_set_number_literal(struct parser_params *parser, VALUE v, int type, int suffix)
{
    if (suffix & NUM_SUFFIX_I) {
	v = rb_complex_raw(INT2FIX(0), v);
	type = tIMAGINARY;
    }
    set_yylval_literal(v);
    SET_LEX_STATE(EXPR_END|EXPR_ENDARG);
    return type;
}

static int
parser_set_integer_literal(struct parser_params *parser, VALUE v, int suffix)
{
    int type = tINTEGER;
    if (suffix & NUM_SUFFIX_R) {
	v = rb_rational_raw1(v);
	type = tRATIONAL;
    }
    return set_number_literal(v, type, suffix);
}

#ifdef RIPPER
static void
ripper_dispatch_heredoc_end(struct parser_params *parser)
{
    VALUE str;
    if (has_delayed_token())
	dispatch_delayed_token(tSTRING_CONTENT);
    str = STR_NEW(parser->tokp, lex_pend - parser->tokp);
    ripper_dispatch1(parser, ripper_token2eventid(tHEREDOC_END), str);
    lex_goto_eol(parser);
    ripper_flush(parser);
}

#define dispatch_heredoc_end() ripper_dispatch_heredoc_end(parser)
#else
#define dispatch_heredoc_end() ((void)0)
#endif

static int
parser_here_document(struct parser_params *parser, NODE *here)
{
    int c, func, indent = 0;
    const char *eos, *p, *pend;
    long len;
    VALUE str = 0;
    rb_encoding *enc = current_enc;
    int bol;

    eos = RSTRING_PTR(here->nd_lit);
    len = RSTRING_LEN(here->nd_lit) - 1;
    indent = (func = *eos++) & STR_FUNC_INDENT;

    if ((c = nextc()) == -1) {
      error:
	compile_error(PARSER_ARG "can't find string \"%s\" anywhere before EOF", eos);
#ifdef RIPPER
	if (!has_delayed_token()) {
	    dispatch_scan_event(tSTRING_CONTENT);
	}
	else {
	    if (str) {
		rb_str_append(parser->delayed, str);
	    }
	    else if ((len = lex_p - parser->tokp) > 0) {
		if (!(func & STR_FUNC_REGEXP) && rb_enc_asciicompat(enc)) {
		    int cr = ENC_CODERANGE_UNKNOWN;
		    rb_str_coderange_scan_restartable(parser->tokp, lex_p, enc, &cr);
		    if (cr != ENC_CODERANGE_7BIT &&
			current_enc == rb_usascii_encoding() &&
			enc != rb_utf8_encoding()) {
			enc = rb_ascii8bit_encoding();
		    }
		}
		rb_enc_str_buf_cat(parser->delayed, parser->tokp, len, enc);
	    }
	    dispatch_delayed_token(tSTRING_CONTENT);
	}
	lex_goto_eol(parser);
#endif
      restore:
	heredoc_restore(lex_strterm);
	return 0;
    }
    bol = was_bol();
    /* `heredoc_line_indent == -1` means
     * - "after an interpolation in the same line", or
     * - "in a continuing line"
     */
    if (bol &&
	(heredoc_line_indent != -1 || (heredoc_line_indent = 0)) &&
	whole_match_p( eos, len, indent)) {
	dispatch_heredoc_end();
	heredoc_restore(lex_strterm);
	return tSTRING_END;
    }

    if (!(func & STR_FUNC_EXPAND)) {
	do {
	    p = RSTRING_PTR(lex_lastline);
	    pend = lex_pend;
	    if (pend > p) {
		switch (pend[-1]) {
		  case '\n':
		    if (--pend == p || pend[-1] != '\r') {
			pend++;
			break;
		    }
		  case '\r':
		    --pend;
		}
	    }

	    if (heredoc_indent > 0) {
		long i = 0;
		while (p + i < pend && parser_update_heredoc_indent(parser, p[i]))
		    i++;
		heredoc_line_indent = 0;
	    }

	    if (str)
		rb_str_cat(str, p, pend - p);
	    else
		str = STR_NEW(p, pend - p);
	    if (pend < lex_pend) rb_str_cat(str, "\n", 1);
	    lex_goto_eol(parser);
	    if (heredoc_indent > 0) {
		set_yylval_str(str);
		flush_string_content(enc);
		return tSTRING_CONTENT;
	    }
	    if (nextc() == -1) {
		if (str) {
		    dispose_string(str);
		    str = 0;
		}
		goto error;
	    }
	} while (!whole_match_p(eos, len, indent));
    }
    else {
	/*	int mb = ENC_CODERANGE_7BIT, *mbp = &mb;*/
	newtok();
	if (c == '#') {
	    int t = parser_peek_variable_name(parser);
	    if (heredoc_line_indent != -1) {
		if (heredoc_indent > heredoc_line_indent) {
		    heredoc_indent = heredoc_line_indent;
		}
		heredoc_line_indent = -1;
	    }
	    if (t) return t;
	    tokadd('#');
	    c = nextc();
	}
	do {
	    pushback(c);
	    if ((c = tokadd_string(func, '\n', 0, NULL, &enc)) == -1) {
		if (parser->eofp) goto error;
		goto restore;
	    }
	    if (c != '\n') {
		if (c == '\\') heredoc_line_indent = -1;
	      flush:
		set_yylval_str(STR_NEW3(tok(), toklen(), enc, func));
		flush_string_content(enc);
		return tSTRING_CONTENT;
	    }
	    tokadd(nextc());
	    if (heredoc_indent > 0) {
		lex_goto_eol(parser);
		goto flush;
	    }
	    /*	    if (mbp && mb == ENC_CODERANGE_UNKNOWN) mbp = 0;*/
	    if ((c = nextc()) == -1) goto error;
	} while (!whole_match_p(eos, len, indent));
	str = STR_NEW3(tok(), toklen(), enc, func);
    }
    dispatch_heredoc_end();
#ifdef RIPPER
    str = ripper_new_yylval(ripper_token2eventid(tSTRING_CONTENT),
			    yylval.val, str);
#endif
    heredoc_restore(lex_strterm);
    lex_strterm = NEW_STRTERM(func, STR_TERM_END, 0);
    set_yylval_str(str);
    return tSTRING_CONTENT;
}

#include "lex.c"

static void
arg_ambiguous_gen(struct parser_params *parser, char c)
{
#ifndef RIPPER
    rb_warning1("ambiguous first argument; put parentheses or a space even after `%c' operator", WARN_I(c));
#else
    dispatch1(arg_ambiguous, rb_usascii_str_new(&c, 1));
#endif
}
#define arg_ambiguous(c) (arg_ambiguous_gen(parser, (c)), 1)

static ID
formal_argument_gen(struct parser_params *parser, ID lhs)
{
    switch (id_type(lhs)) {
      case ID_LOCAL:
	break;
#ifndef RIPPER
      case ID_CONST:
	yyerror("formal argument cannot be a constant");
	return 0;
      case ID_INSTANCE:
	yyerror("formal argument cannot be an instance variable");
	return 0;
      case ID_GLOBAL:
	yyerror("formal argument cannot be a global variable");
	return 0;
      case ID_CLASS:
	yyerror("formal argument cannot be a class variable");
	return 0;
      default:
	yyerror("formal argument must be local variable");
	return 0;
#else
      default:
	lhs = dispatch1(param_error, lhs);
	ripper_error();
	return 0;
#endif
    }
    shadowing_lvar(lhs);
    return lhs;
}

static int
lvar_defined_gen(struct parser_params *parser, ID id)
{
    return (dyna_in_block() && dvar_defined_get(id)) || local_id(id);
}

/* emacsen -*- hack */
static long
parser_encode_length(struct parser_params *parser, const char *name, long len)
{
    long nlen;

    if (len > 5 && name[nlen = len - 5] == '-') {
	if (rb_memcicmp(name + nlen + 1, "unix", 4) == 0)
	    return nlen;
    }
    if (len > 4 && name[nlen = len - 4] == '-') {
	if (rb_memcicmp(name + nlen + 1, "dos", 3) == 0)
	    return nlen;
	if (rb_memcicmp(name + nlen + 1, "mac", 3) == 0 &&
	    !(len == 8 && rb_memcicmp(name, "utf8-mac", len) == 0))
	    /* exclude UTF8-MAC because the encoding named "UTF8" doesn't exist in Ruby */
	    return nlen;
    }
    return len;
}

static void
parser_set_encode(struct parser_params *parser, const char *name)
{
    int idx = rb_enc_find_index(name);
    rb_encoding *enc;
    VALUE excargs[3];

    if (idx < 0) {
	excargs[1] = rb_sprintf("unknown encoding name: %s", name);
      error:
	excargs[0] = rb_eArgError;
	excargs[2] = rb_make_backtrace();
	rb_ary_unshift(excargs[2], rb_sprintf("%"PRIsVALUE":%d", ruby_sourcefile_string, ruby_sourceline));
	rb_exc_raise(rb_make_exception(3, excargs));
    }
    enc = rb_enc_from_index(idx);
    if (!rb_enc_asciicompat(enc)) {
	excargs[1] = rb_sprintf("%s is not ASCII compatible", rb_enc_name(enc));
	goto error;
    }
    parser->enc = enc;
#ifndef RIPPER
    if (ruby_debug_lines) {
	VALUE lines = ruby_debug_lines;
	long i, n = RARRAY_LEN(lines);
	for (i = 0; i < n; ++i) {
	    rb_enc_associate_index(RARRAY_AREF(lines, i), idx);
	}
    }
#endif
}

static int
comment_at_top(struct parser_params *parser)
{
    const char *p = lex_pbeg, *pend = lex_p - 1;
    if (parser->line_count != (parser->has_shebang ? 2 : 1)) return 0;
    while (p < pend) {
	if (!ISSPACE(*p)) return 0;
	p++;
    }
    return 1;
}

typedef long (*rb_magic_comment_length_t)(struct parser_params *parser, const char *name, long len);
typedef void (*rb_magic_comment_setter_t)(struct parser_params *parser, const char *name, const char *val);

static void
magic_comment_encoding(struct parser_params *parser, const char *name, const char *val)
{
    if (!comment_at_top(parser)) {
	return;
    }
    parser_set_encode(parser, val);
}

static int
parser_get_bool(struct parser_params *parser, const char *name, const char *val)
{
    switch (*val) {
      case 't': case 'T':
	if (strcasecmp(val, "true") == 0) {
	    return TRUE;
	}
	break;
      case 'f': case 'F':
	if (strcasecmp(val, "false") == 0) {
	    return FALSE;
	}
	break;
    }
    rb_compile_warning(ruby_sourcefile, ruby_sourceline, "invalid value for %s: %s", name, val);
    return -1;
}

static void
parser_set_token_info(struct parser_params *parser, const char *name, const char *val)
{
    int b = parser_get_bool(parser, name, val);
    if (b >= 0) parser->token_info_enabled = b;
}

static void
parser_set_compile_option_flag(struct parser_params *parser, const char *name, const char *val)
{
    int b;

    if (parser->token_seen) {
	rb_warning1("`%s' is ignored after any tokens", WARN_S(name));
	return;
    }

    b = parser_get_bool(parser, name, val);
    if (b < 0) return;

    if (!parser->compile_option)
	parser->compile_option = rb_obj_hide(rb_ident_hash_new());
    rb_hash_aset(parser->compile_option, ID2SYM(rb_intern(name)),
		 (b ? Qtrue : Qfalse));
}

# if WARN_PAST_SCOPE
static void
parser_set_past_scope(struct parser_params *parser, const char *name, const char *val)
{
    int b = parser_get_bool(parser, name, val);
    if (b >= 0) parser->past_scope_enabled = b;
}
# endif

struct magic_comment {
    const char *name;
    rb_magic_comment_setter_t func;
    rb_magic_comment_length_t length;
};

static const struct magic_comment magic_comments[] = {
    {"coding", magic_comment_encoding, parser_encode_length},
    {"encoding", magic_comment_encoding, parser_encode_length},
    {"frozen_string_literal", parser_set_compile_option_flag},
    {"warn_indent", parser_set_token_info},
# if WARN_PAST_SCOPE
    {"warn_past_scope", parser_set_past_scope},
# endif
};

static const char *
magic_comment_marker(const char *str, long len)
{
    long i = 2;

    while (i < len) {
	switch (str[i]) {
	  case '-':
	    if (str[i-1] == '*' && str[i-2] == '-') {
		return str + i + 1;
	    }
	    i += 2;
	    break;
	  case '*':
	    if (i + 1 >= len) return 0;
	    if (str[i+1] != '-') {
		i += 4;
	    }
	    else if (str[i-1] != '-') {
		i += 2;
	    }
	    else {
		return str + i + 2;
	    }
	    break;
	  default:
	    i += 3;
	    break;
	}
    }
    return 0;
}

static int
parser_magic_comment(struct parser_params *parser, const char *str, long len)
{
    int indicator = 0;
    VALUE name = 0, val = 0;
    const char *beg, *end, *vbeg, *vend;
#define str_copy(_s, _p, _n) ((_s) \
	? (void)(rb_str_resize((_s), (_n)), \
	   MEMCPY(RSTRING_PTR(_s), (_p), char, (_n)), (_s)) \
	: (void)((_s) = STR_NEW((_p), (_n))))

    if (len <= 7) return FALSE;
    if (!!(beg = magic_comment_marker(str, len))) {
	if (!(end = magic_comment_marker(beg, str + len - beg)))
	    return FALSE;
	indicator = TRUE;
	str = beg;
	len = end - beg - 3;
    }

    /* %r"([^\\s\'\":;]+)\\s*:\\s*(\"(?:\\\\.|[^\"])*\"|[^\"\\s;]+)[\\s;]*" */
    while (len > 0) {
	const struct magic_comment *p = magic_comments;
	char *s;
	int i;
	long n = 0;

	for (; len > 0 && *str; str++, --len) {
	    switch (*str) {
	      case '\'': case '"': case ':': case ';':
		continue;
	    }
	    if (!ISSPACE(*str)) break;
	}
	for (beg = str; len > 0; str++, --len) {
	    switch (*str) {
	      case '\'': case '"': case ':': case ';':
		break;
	      default:
		if (ISSPACE(*str)) break;
		continue;
	    }
	    break;
	}
	for (end = str; len > 0 && ISSPACE(*str); str++, --len);
	if (!len) break;
	if (*str != ':') {
	    if (!indicator) return FALSE;
	    continue;
	}

	do str++; while (--len > 0 && ISSPACE(*str));
	if (!len) break;
	if (*str == '"') {
	    for (vbeg = ++str; --len > 0 && *str != '"'; str++) {
		if (*str == '\\') {
		    --len;
		    ++str;
		}
	    }
	    vend = str;
	    if (len) {
		--len;
		++str;
	    }
	}
	else {
	    for (vbeg = str; len > 0 && *str != '"' && *str != ';' && !ISSPACE(*str); --len, str++);
	    vend = str;
	}
	if (indicator) {
	    while (len > 0 && (*str == ';' || ISSPACE(*str))) --len, str++;
	}
	else {
	    while (len > 0 && (ISSPACE(*str))) --len, str++;
	    if (len) return FALSE;
	}

	n = end - beg;
	str_copy(name, beg, n);
	s = RSTRING_PTR(name);
	for (i = 0; i < n; ++i) {
	    if (s[i] == '-') s[i] = '_';
	}
	do {
	    if (STRNCASECMP(p->name, s, n) == 0 && !p->name[n]) {
		n = vend - vbeg;
		if (p->length) {
		    n = (*p->length)(parser, vbeg, n);
		}
		str_copy(val, vbeg, n);
		(*p->func)(parser, p->name, RSTRING_PTR(val));
		break;
	    }
	} while (++p < magic_comments + numberof(magic_comments));
#ifdef RIPPER
	str_copy(val, vbeg, vend - vbeg);
	dispatch2(magic_comment, name, val);
#endif
    }

    return TRUE;
}

static void
set_file_encoding(struct parser_params *parser, const char *str, const char *send)
{
    int sep = 0;
    const char *beg = str;
    VALUE s;

    for (;;) {
	if (send - str <= 6) return;
	switch (str[6]) {
	  case 'C': case 'c': str += 6; continue;
	  case 'O': case 'o': str += 5; continue;
	  case 'D': case 'd': str += 4; continue;
	  case 'I': case 'i': str += 3; continue;
	  case 'N': case 'n': str += 2; continue;
	  case 'G': case 'g': str += 1; continue;
	  case '=': case ':':
	    sep = 1;
	    str += 6;
	    break;
	  default:
	    str += 6;
	    if (ISSPACE(*str)) break;
	    continue;
	}
	if (STRNCASECMP(str-6, "coding", 6) == 0) break;
    }
    for (;;) {
	do {
	    if (++str >= send) return;
	} while (ISSPACE(*str));
	if (sep) break;
	if (*str != '=' && *str != ':') return;
	sep = 1;
	str++;
    }
    beg = str;
    while ((*str == '-' || *str == '_' || ISALNUM(*str)) && ++str < send);
    s = rb_str_new(beg, parser_encode_length(parser, beg, str - beg));
    parser_set_encode(parser, RSTRING_PTR(s));
    rb_str_resize(s, 0);
}

static void
parser_prepare(struct parser_params *parser)
{
    int c = nextc();
    parser->token_info_enabled = !compile_for_eval && RTEST(ruby_verbose);
    switch (c) {
      case '#':
	if (peek('!')) parser->has_shebang = 1;
	break;
      case 0xef:		/* UTF-8 BOM marker */
	if (lex_pend - lex_p >= 2 &&
	    (unsigned char)lex_p[0] == 0xbb &&
	    (unsigned char)lex_p[1] == 0xbf) {
	    parser->enc = rb_utf8_encoding();
	    lex_p += 2;
	    lex_pbeg = lex_p;
	    return;
	}
	break;
      case EOF:
	return;
    }
    pushback(c);
    parser->enc = rb_enc_get(lex_lastline);
}

#define IS_ARG() IS_lex_state(EXPR_ARG_ANY)
#define IS_END() IS_lex_state(EXPR_END_ANY)
#define IS_BEG() (IS_lex_state(EXPR_BEG_ANY) || IS_lex_state_all(EXPR_ARG|EXPR_LABELED))
#define IS_SPCARG(c) (IS_ARG() && space_seen && !ISSPACE(c))
#define IS_LABEL_POSSIBLE() (\
	(IS_lex_state(EXPR_LABEL|EXPR_ENDFN) && !cmd_state) || \
	IS_ARG())
#define IS_LABEL_SUFFIX(n) (peek_n(':',(n)) && !peek_n(':', (n)+1))
#define IS_AFTER_OPERATOR() IS_lex_state(EXPR_FNAME | EXPR_DOT)

#ifndef RIPPER
#define ambiguous_operator(op, syn) ( \
    rb_warning0("`"op"' after local variable or literal is interpreted as binary operator"), \
    rb_warning0("even though it seems like "syn""))
#else
#define ambiguous_operator(op, syn) dispatch2(operator_ambiguous, ripper_intern(op), rb_str_new_cstr(syn))
#endif
#define warn_balanced(op, syn) ((void) \
    (!IS_lex_state_for(last_state, EXPR_CLASS|EXPR_DOT|EXPR_FNAME|EXPR_ENDFN) && \
     space_seen && !ISSPACE(c) && \
     (ambiguous_operator(op, syn), 0)))

static VALUE
parse_rational(struct parser_params *parser, char *str, int len, int seen_point)
{
    VALUE v;
    char *point = &str[seen_point];
    size_t fraclen = len-seen_point-1;
    memmove(point, point+1, fraclen+1);
    v = rb_cstr_to_inum(str, 10, FALSE);
    return rb_rational_new(v, rb_int_positive_pow(10, fraclen));
}

static int
parse_numeric(struct parser_params *parser, int c)
{
    int is_float, seen_point, seen_e, nondigit;
    int suffix;

    is_float = seen_point = seen_e = nondigit = 0;
    SET_LEX_STATE(EXPR_END);
    newtok();
    if (c == '-' || c == '+') {
	tokadd(c);
	c = nextc();
    }
    if (c == '0') {
#define no_digits() do {yyerror("numeric literal without digits"); return 0;} while (0)
	int start = toklen();
	c = nextc();
	if (c == 'x' || c == 'X') {
	    /* hexadecimal */
	    c = nextc();
	    if (c != -1 && ISXDIGIT(c)) {
		do {
		    if (c == '_') {
			if (nondigit) break;
			nondigit = c;
			continue;
		    }
		    if (!ISXDIGIT(c)) break;
		    nondigit = 0;
		    tokadd(c);
		} while ((c = nextc()) != -1);
	    }
	    pushback(c);
	    tokfix();
	    if (toklen() == start) {
		no_digits();
	    }
	    else if (nondigit) goto trailing_uc;
	    suffix = number_literal_suffix(NUM_SUFFIX_ALL);
	    return set_integer_literal(rb_cstr_to_inum(tok(), 16, FALSE), suffix);
	}
	if (c == 'b' || c == 'B') {
	    /* binary */
	    c = nextc();
	    if (c == '0' || c == '1') {
		do {
		    if (c == '_') {
			if (nondigit) break;
			nondigit = c;
			continue;
		    }
		    if (c != '0' && c != '1') break;
		    nondigit = 0;
		    tokadd(c);
		} while ((c = nextc()) != -1);
	    }
	    pushback(c);
	    tokfix();
	    if (toklen() == start) {
		no_digits();
	    }
	    else if (nondigit) goto trailing_uc;
	    suffix = number_literal_suffix(NUM_SUFFIX_ALL);
	    return set_integer_literal(rb_cstr_to_inum(tok(), 2, FALSE), suffix);
	}
	if (c == 'd' || c == 'D') {
	    /* decimal */
	    c = nextc();
	    if (c != -1 && ISDIGIT(c)) {
		do {
		    if (c == '_') {
			if (nondigit) break;
			nondigit = c;
			continue;
		    }
		    if (!ISDIGIT(c)) break;
		    nondigit = 0;
		    tokadd(c);
		} while ((c = nextc()) != -1);
	    }
	    pushback(c);
	    tokfix();
	    if (toklen() == start) {
		no_digits();
	    }
	    else if (nondigit) goto trailing_uc;
	    suffix = number_literal_suffix(NUM_SUFFIX_ALL);
	    return set_integer_literal(rb_cstr_to_inum(tok(), 10, FALSE), suffix);
	}
	if (c == '_') {
	    /* 0_0 */
	    goto octal_number;
	}
	if (c == 'o' || c == 'O') {
	    /* prefixed octal */
	    c = nextc();
	    if (c == -1 || c == '_' || !ISDIGIT(c)) {
		no_digits();
	    }
	}
	if (c >= '0' && c <= '7') {
	    /* octal */
	  octal_number:
	    do {
		if (c == '_') {
		    if (nondigit) break;
		    nondigit = c;
		    continue;
		}
		if (c < '0' || c > '9') break;
		if (c > '7') goto invalid_octal;
		nondigit = 0;
		tokadd(c);
	    } while ((c = nextc()) != -1);
	    if (toklen() > start) {
		pushback(c);
		tokfix();
		if (nondigit) goto trailing_uc;
		suffix = number_literal_suffix(NUM_SUFFIX_ALL);
		return set_integer_literal(rb_cstr_to_inum(tok(), 8, FALSE), suffix);
	    }
	    if (nondigit) {
		pushback(c);
		goto trailing_uc;
	    }
	}
	if (c > '7' && c <= '9') {
	  invalid_octal:
	    yyerror("Invalid octal digit");
	}
	else if (c == '.' || c == 'e' || c == 'E') {
	    tokadd('0');
	}
	else {
	    pushback(c);
	    suffix = number_literal_suffix(NUM_SUFFIX_ALL);
	    return set_integer_literal(INT2FIX(0), suffix);
	}
    }

    for (;;) {
	switch (c) {
	  case '0': case '1': case '2': case '3': case '4':
	  case '5': case '6': case '7': case '8': case '9':
	    nondigit = 0;
	    tokadd(c);
	    break;

	  case '.':
	    if (nondigit) goto trailing_uc;
	    if (seen_point || seen_e) {
		goto decode_num;
	    }
	    else {
		int c0 = nextc();
		if (c0 == -1 || !ISDIGIT(c0)) {
		    pushback(c0);
		    goto decode_num;
		}
		c = c0;
	    }
	    seen_point = toklen();
	    tokadd('.');
	    tokadd(c);
	    is_float++;
	    nondigit = 0;
	    break;

	  case 'e':
	  case 'E':
	    if (nondigit) {
		pushback(c);
		c = nondigit;
		goto decode_num;
	    }
	    if (seen_e) {
		goto decode_num;
	    }
	    nondigit = c;
	    c = nextc();
	    if (c != '-' && c != '+' && !ISDIGIT(c)) {
		pushback(c);
		nondigit = 0;
		goto decode_num;
	    }
	    tokadd(nondigit);
	    seen_e++;
	    is_float++;
	    tokadd(c);
	    nondigit = (c == '-' || c == '+') ? c : 0;
	    break;

	  case '_':	/* `_' in number just ignored */
	    if (nondigit) goto decode_num;
	    nondigit = c;
	    break;

	  default:
	    goto decode_num;
	}
	c = nextc();
    }

  decode_num:
    pushback(c);
    if (nondigit) {
	char tmp[30];
      trailing_uc:
	snprintf(tmp, sizeof(tmp), "trailing `%c' in number", nondigit);
	yyerror(tmp);
    }
    tokfix();
    if (is_float) {
	int type = tFLOAT;
	VALUE v;

	suffix = number_literal_suffix(seen_e ? NUM_SUFFIX_I : NUM_SUFFIX_ALL);
	if (suffix & NUM_SUFFIX_R) {
	    type = tRATIONAL;
	    v = parse_rational(parser, tok(), toklen(), seen_point);
	}
	else {
	    double d = strtod(tok(), 0);
	    if (errno == ERANGE) {
		rb_warning1("Float %s out of range", WARN_S(tok()));
		errno = 0;
	    }
	    v = DBL2NUM(d);
	}
	return set_number_literal(v, type, suffix);
    }
    suffix = number_literal_suffix(NUM_SUFFIX_ALL);
    return set_integer_literal(rb_cstr_to_inum(tok(), 10, FALSE), suffix);
}

static int
parse_qmark(struct parser_params *parser, int space_seen)
{
    rb_encoding *enc;
    register int c;

    if (IS_END()) {
	SET_LEX_STATE(EXPR_VALUE);
	return '?';
    }
    c = nextc();
    if (c == -1) {
	compile_error(PARSER_ARG "incomplete character syntax");
	return 0;
    }
    if (rb_enc_isspace(c, current_enc)) {
	if (!IS_ARG()) {
	    int c2 = 0;
	    switch (c) {
	      case ' ':
		c2 = 's';
		break;
	      case '\n':
		c2 = 'n';
		break;
	      case '\t':
		c2 = 't';
		break;
	      case '\v':
		c2 = 'v';
		break;
	      case '\r':
		c2 = 'r';
		break;
	      case '\f':
		c2 = 'f';
		break;
	    }
	    if (c2) {
		rb_warn1("invalid character syntax; use ?\\%c", WARN_I(c2));
	    }
	}
      ternary:
	pushback(c);
	SET_LEX_STATE(EXPR_VALUE);
	return '?';
    }
    newtok();
    enc = current_enc;
    if (!parser_isascii()) {
	if (tokadd_mbchar(c) == -1) return 0;
    }
    else if ((rb_enc_isalnum(c, current_enc) || c == '_') &&
	     lex_p < lex_pend && is_identchar(lex_p, lex_pend, current_enc)) {
	if (space_seen) {
	    const char *start = lex_p - 1, *p = start;
	    do {
		int n = parser_precise_mbclen(parser, p);
		if (n < 0) return -1;
		p += n;
	    } while (p < lex_pend && is_identchar(p, lex_pend, current_enc));
	    rb_warn2("`?' just followed by `%.*s' is interpreted as" \
		     " a conditional operator, put a space after `?'",
		     WARN_I((int)(p - start)), WARN_S_L(start, (p - start)));
	}
	goto ternary;
    }
    else if (c == '\\') {
	if (peek('u')) {
	    nextc();
	    if (!parser_tokadd_utf8(parser, &enc, 0, 0, 0))
		return 0;
	}
	else if (!lex_eol_p() && !(c = *lex_p, ISASCII(c))) {
	    nextc();
	    if (tokadd_mbchar(c) == -1) return 0;
	}
	else {
	    c = read_escape(0, &enc);
	    tokadd(c);
	}
    }
    else {
	tokadd(c);
    }
    tokfix();
    set_yylval_str(STR_NEW3(tok(), toklen(), enc, 0));
    SET_LEX_STATE(EXPR_END);
    return tCHAR;
}

static int
parse_percent(struct parser_params *parser, const int space_seen, const enum lex_state_e last_state)
{
    register int c;

    if (IS_BEG()) {
	int term;
	int paren;

	c = nextc();
      quotation:
	if (c == -1 || !ISALNUM(c)) {
	    term = c;
	    c = 'Q';
	}
	else {
	    term = nextc();
	    if (rb_enc_isalnum(term, current_enc) || !parser_isascii()) {
		yyerror("unknown type of %string");
		return 0;
	    }
	}
	if (c == -1 || term == -1) {
	    compile_error(PARSER_ARG "unterminated quoted string meets end of file");
	    return 0;
	}
	paren = term;
	if (term == '(') term = ')';
	else if (term == '[') term = ']';
	else if (term == '{') term = '}';
	else if (term == '<') term = '>';
	else paren = 0;

	switch (c) {
	  case 'Q':
	    lex_strterm = NEW_STRTERM(str_dquote, term, paren);
	    return tSTRING_BEG;

	  case 'q':
	    lex_strterm = NEW_STRTERM(str_squote, term, paren);
	    return tSTRING_BEG;

	  case 'W':
	    lex_strterm = NEW_STRTERM(str_dword, term, paren);
	    do {c = nextc();} while (ISSPACE(c));
	    pushback(c);
	    return tWORDS_BEG;

	  case 'w':
	    lex_strterm = NEW_STRTERM(str_sword, term, paren);
	    do {c = nextc();} while (ISSPACE(c));
	    pushback(c);
	    return tQWORDS_BEG;

	  case 'I':
	    lex_strterm = NEW_STRTERM(str_dword, term, paren);
	    do {c = nextc();} while (ISSPACE(c));
	    pushback(c);
	    return tSYMBOLS_BEG;

	  case 'i':
	    lex_strterm = NEW_STRTERM(str_sword, term, paren);
	    do {c = nextc();} while (ISSPACE(c));
	    pushback(c);
	    return tQSYMBOLS_BEG;

	  case 'x':
	    lex_strterm = NEW_STRTERM(str_xquote, term, paren);
	    return tXSTRING_BEG;

	  case 'r':
	    lex_strterm = NEW_STRTERM(str_regexp, term, paren);
	    return tREGEXP_BEG;

	  case 's':
	    lex_strterm = NEW_STRTERM(str_ssym, term, paren);
	    SET_LEX_STATE(EXPR_FNAME|EXPR_FITEM);
	    return tSYMBEG;

	  default:
	    yyerror("unknown type of %string");
	    return 0;
	}
    }
    if ((c = nextc()) == '=') {
	set_yylval_id('%');
	SET_LEX_STATE(EXPR_BEG);
	return tOP_ASGN;
    }
    if (IS_SPCARG(c) || (IS_lex_state(EXPR_FITEM) && c == 's')) {
	goto quotation;
    }
    SET_LEX_STATE(IS_AFTER_OPERATOR() ? EXPR_ARG : EXPR_BEG);
    pushback(c);
    warn_balanced("%%", "string literal");
    return '%';
}

static int
tokadd_ident(struct parser_params *parser, int c)
{
    do {
	if (tokadd_mbchar(c) == -1) return -1;
	c = nextc();
    } while (parser_is_identchar());
    pushback(c);
    return 0;
}

static ID
tokenize_ident(struct parser_params *parser, const enum lex_state_e last_state)
{
    ID ident = TOK_INTERN();

    set_yylval_name(ident);

    return ident;
}

static int
parse_numvar(struct parser_params *parser)
{
    size_t len;
    int overflow;
    unsigned long n = ruby_scan_digits(tok()+1, toklen()-1, 10, &len, &overflow);
    const unsigned long nth_ref_max =
	((FIXNUM_MAX < INT_MAX) ? FIXNUM_MAX : INT_MAX) >> 1;
    /* NTH_REF is left-shifted to be ORed with back-ref flag and
     * turned into a Fixnum, in compile.c */

    if (overflow || n > nth_ref_max) {
	/* compile_error()? */
	rb_warn1("`%s' is too big for a number variable, always nil", WARN_S(tok()));
	return 0;		/* $0 is $PROGRAM_NAME, not NTH_REF */
    }
    else {
	return (int)n;
    }
}

static int
parse_gvar(struct parser_params *parser, const enum lex_state_e last_state)
{
    register int c;

    SET_LEX_STATE(EXPR_END);
    newtok();
    c = nextc();
    switch (c) {
      case '_':		/* $_: last read line string */
	c = nextc();
	if (parser_is_identchar()) {
	    tokadd('$');
	    tokadd('_');
	    break;
	}
	pushback(c);
	c = '_';
	/* fall through */
      case '~':		/* $~: match-data */
      case '*':		/* $*: argv */
      case '$':		/* $$: pid */
      case '?':		/* $?: last status */
      case '!':		/* $!: error string */
      case '@':		/* $@: error position */
      case '/':		/* $/: input record separator */
      case '\\':		/* $\: output record separator */
      case ';':		/* $;: field separator */
      case ',':		/* $,: output field separator */
      case '.':		/* $.: last read line number */
      case '=':		/* $=: ignorecase */
      case ':':		/* $:: load path */
      case '<':		/* $<: reading filename */
      case '>':		/* $>: default output handle */
      case '\"':		/* $": already loaded files */
	tokadd('$');
	tokadd(c);
	goto gvar;

      case '-':
	tokadd('$');
	tokadd(c);
	c = nextc();
	if (parser_is_identchar()) {
	    if (tokadd_mbchar(c) == -1) return 0;
	}
	else {
	    pushback(c);
	    pushback('-');
	    return '$';
	}
      gvar:
	set_yylval_name(TOK_INTERN());
	return tGVAR;

      case '&':		/* $&: last match */
      case '`':		/* $`: string before last match */
      case '\'':		/* $': string after last match */
      case '+':		/* $+: string matches last paren. */
	if (IS_lex_state_for(last_state, EXPR_FNAME)) {
	    tokadd('$');
	    tokadd(c);
	    goto gvar;
	}
	set_yylval_node(NEW_BACK_REF(c));
	return tBACK_REF;

      case '1': case '2': case '3':
      case '4': case '5': case '6':
      case '7': case '8': case '9':
	tokadd('$');
	do {
	    tokadd(c);
	    c = nextc();
	} while (c != -1 && ISDIGIT(c));
	pushback(c);
	if (IS_lex_state_for(last_state, EXPR_FNAME)) goto gvar;
	tokfix();
	set_yylval_node(NEW_NTH_REF(parse_numvar(parser)));
	return tNTH_REF;

      default:
	if (!parser_is_identchar()) {
	    if (c == -1 || ISSPACE(c)) {
		compile_error(PARSER_ARG "`$' without identifiers is not allowed as a global variable name");
	    }
	    else {
		pushback(c);
		compile_error(PARSER_ARG "`$%c' is not allowed as a global variable name", c);
	    }
	    return 0;
	}
      case '0':
	tokadd('$');
    }

    if (tokadd_ident(parser, c)) return 0;
    SET_LEX_STATE(EXPR_END);
    tokenize_ident(parser, last_state);
    return tGVAR;
}

static int
parse_atmark(struct parser_params *parser, const enum lex_state_e last_state)
{
    int result = tIVAR;
    register int c = nextc();

    newtok();
    tokadd('@');
    if (c == '@') {
	result = tCVAR;
	tokadd('@');
	c = nextc();
    }
    if (c == -1 || ISSPACE(c)) {
	if (result == tIVAR) {
	    compile_error(PARSER_ARG "`@' without identifiers is not allowed as an instance variable name");
	}
	else {
	    compile_error(PARSER_ARG "`@@' without identifiers is not allowed as a class variable name");
	}
	return 0;
    }
    else if (ISDIGIT(c) || !parser_is_identchar()) {
	pushback(c);
	if (result == tIVAR) {
	    compile_error(PARSER_ARG "`@%c' is not allowed as an instance variable name", c);
	}
	else {
	    compile_error(PARSER_ARG "`@@%c' is not allowed as a class variable name", c);
	}
	return 0;
    }

    if (tokadd_ident(parser, c)) return 0;
    SET_LEX_STATE(EXPR_END);
    tokenize_ident(parser, last_state);
    return result;
}

static int
parse_ident(struct parser_params *parser, int c, int cmd_state)
{
    int result = 0;
    int mb = ENC_CODERANGE_7BIT;
    const enum lex_state_e last_state = lex_state;
    ID ident;

    do {
	if (!ISASCII(c)) mb = ENC_CODERANGE_UNKNOWN;
	if (tokadd_mbchar(c) == -1) return 0;
	c = nextc();
    } while (parser_is_identchar());
    if ((c == '!' || c == '?') && !peek('=')) {
	tokadd(c);
    }
    else {
	pushback(c);
    }
    tokfix();

    if (toklast() == '!' || toklast() == '?') {
	result = tFID;
    }
    else {
	if (IS_lex_state(EXPR_FNAME)) {
	    register int c = nextc();
	    if (c == '=' && !peek('~') && !peek('>') &&
		(!peek('=') || (peek_n('>', 1)))) {
		result = tIDENTIFIER;
		tokadd(c);
		tokfix();
	    }
	    else {
		pushback(c);
	    }
	}
	if (result == 0 && ISUPPER(tok()[0])) {
	    result = tCONSTANT;
	}
	else {
	    result = tIDENTIFIER;
	}
    }

    if (IS_LABEL_POSSIBLE()) {
	if (IS_LABEL_SUFFIX(0)) {
	    SET_LEX_STATE(EXPR_ARG|EXPR_LABELED);
	    nextc();
	    set_yylval_name(TOK_INTERN());
	    return tLABEL;
	}
    }
    if (mb == ENC_CODERANGE_7BIT && !IS_lex_state(EXPR_DOT)) {
	const struct kwtable *kw;

	/* See if it is a reserved word.  */
	kw = rb_reserved_word(tok(), toklen());
	if (kw) {
	    enum lex_state_e state = lex_state;
	    SET_LEX_STATE(kw->state);
	    if (IS_lex_state_for(state, EXPR_FNAME)) {
		set_yylval_name(rb_intern2(tok(), toklen()));
		return kw->id[0];
	    }
	    if (IS_lex_state(EXPR_BEG)) {
		command_start = TRUE;
	    }
	    if (kw->id[0] == keyword_do) {
		if (lambda_beginning_p()) {
		    lpar_beg = 0;
		    --paren_nest;
		    return keyword_do_LAMBDA;
		}
		if (COND_P()) return keyword_do_cond;
		if (CMDARG_P() && !IS_lex_state_for(state, EXPR_CMDARG))
		    return keyword_do_block;
		if (IS_lex_state_for(state, (EXPR_BEG | EXPR_ENDARG)))
		    return keyword_do_block;
		return keyword_do;
	    }
	    if (IS_lex_state_for(state, (EXPR_BEG | EXPR_LABELED)))
		return kw->id[0];
	    else {
		if (kw->id[0] != kw->id[1])
		    SET_LEX_STATE(EXPR_BEG | EXPR_LABEL);
		return kw->id[1];
	    }
	}
    }

    if (IS_lex_state(EXPR_BEG_ANY | EXPR_ARG_ANY | EXPR_DOT)) {
	if (cmd_state) {
	    SET_LEX_STATE(EXPR_CMDARG);
	}
	else {
	    SET_LEX_STATE(EXPR_ARG);
	}
    }
    else if (lex_state == EXPR_FNAME) {
	SET_LEX_STATE(EXPR_ENDFN);
    }
    else {
	SET_LEX_STATE(EXPR_END);
    }

    ident = tokenize_ident(parser, last_state);
    if (!IS_lex_state_for(last_state, EXPR_DOT|EXPR_FNAME) &&
	(result == tIDENTIFIER) && /* not EXPR_FNAME, not attrasgn */
	lvar_defined(ident)) {
	SET_LEX_STATE(EXPR_END|EXPR_LABEL);
    }
    return result;
}

static int
parser_yylex(struct parser_params *parser)
{
    register int c;
    int space_seen = 0;
    int cmd_state;
    int label;
    enum lex_state_e last_state;
    int fallthru = FALSE;
    int token_seen = parser->token_seen;

    if (lex_strterm) {
	int token;
	if (nd_type(lex_strterm) == NODE_HEREDOC) {
	    token = here_document(lex_strterm);
	    if (token == tSTRING_END) {
		lex_strterm = 0;
		SET_LEX_STATE(EXPR_END);
	    }
	}
	else {
	    token = parse_string(lex_strterm);
	    if ((token == tSTRING_END) && (lex_strterm->nd_func & STR_FUNC_LABEL)) {
		if (((IS_lex_state(EXPR_BEG | EXPR_ENDFN) && !COND_P()) || IS_ARG()) &&
		    IS_LABEL_SUFFIX(0)) {
		    nextc();
		    token = tLABEL_END;
		}
	    }
	    if (token == tSTRING_END || token == tREGEXP_END || token == tLABEL_END) {
		const enum lex_state_e next_state =
		    token == tLABEL_END ? EXPR_BEG|EXPR_LABEL : EXPR_END|EXPR_ENDARG;
		rb_gc_force_recycle((VALUE)lex_strterm);
		lex_strterm = 0;
		SET_LEX_STATE(next_state);
	    }
	}
	return token;
    }
    cmd_state = command_start;
    command_start = FALSE;
    parser->token_seen = TRUE;
  retry:
    last_state = lex_state;
    switch (c = nextc()) {
      case '\0':		/* NUL */
      case '\004':		/* ^D */
      case '\032':		/* ^Z */
      case -1:			/* end of script. */
	return 0;

	/* white spaces */
      case ' ': case '\t': case '\f': case '\r':
      case '\13': /* '\v' */
	space_seen = 1;
#ifdef RIPPER
	while ((c = nextc())) {
	    switch (c) {
	      case ' ': case '\t': case '\f': case '\r':
	      case '\13': /* '\v' */
		break;
	      default:
		goto outofloop;
	    }
	}
      outofloop:
	pushback(c);
	dispatch_scan_event(tSP);
#endif
	goto retry;

      case '#':		/* it's a comment */
	parser->token_seen = token_seen;
	/* no magic_comment in shebang line */
	if (!parser_magic_comment(parser, lex_p, lex_pend - lex_p)) {
	    if (comment_at_top(parser)) {
		set_file_encoding(parser, lex_p, lex_pend);
	    }
	}
	lex_p = lex_pend;
        dispatch_scan_event(tCOMMENT);
        fallthru = TRUE;
	/* fall through */
      case '\n':
	parser->token_seen = token_seen;
	c = (IS_lex_state(EXPR_BEG|EXPR_CLASS|EXPR_FNAME|EXPR_DOT) &&
	     !IS_lex_state(EXPR_LABELED));
	if (c || IS_lex_state_all(EXPR_ARG|EXPR_LABELED)) {
            if (!fallthru) {
                dispatch_scan_event(tIGNORED_NL);
            }
            fallthru = FALSE;
	    if (!c && parser->in_kwarg) {
		goto normal_newline;
	    }
	    goto retry;
	}
	while (1) {
	    switch (c = nextc()) {
	      case ' ': case '\t': case '\f': case '\r':
	      case '\13': /* '\v' */
		space_seen = 1;
		break;
	      case '&':
	      case '.': {
		dispatch_delayed_token(tIGNORED_NL);
		if (peek('.') == (c == '&')) {
		    pushback(c);
		    dispatch_scan_event(tSP);
		    goto retry;
		}
	      }
	      default:
		--ruby_sourceline;
		lex_nextline = lex_lastline;
	      case -1:		/* EOF no decrement*/
		lex_goto_eol(parser);
#ifdef RIPPER
		if (c != -1) {
		    parser->tokp = lex_p;
		}
#endif
		goto normal_newline;
	    }
	}
      normal_newline:
	command_start = TRUE;
	SET_LEX_STATE(EXPR_BEG);
	return '\n';

      case '*':
	if ((c = nextc()) == '*') {
	    if ((c = nextc()) == '=') {
                set_yylval_id(tPOW);
		SET_LEX_STATE(EXPR_BEG);
		return tOP_ASGN;
	    }
	    pushback(c);
	    if (IS_SPCARG(c)) {
		rb_warning0("`**' interpreted as argument prefix");
		c = tDSTAR;
	    }
	    else if (IS_BEG()) {
		c = tDSTAR;
	    }
	    else {
		warn_balanced("**", "argument prefix");
		c = tPOW;
	    }
	}
	else {
	    if (c == '=') {
                set_yylval_id('*');
		SET_LEX_STATE(EXPR_BEG);
		return tOP_ASGN;
	    }
	    pushback(c);
	    if (IS_SPCARG(c)) {
		rb_warning0("`*' interpreted as argument prefix");
		c = tSTAR;
	    }
	    else if (IS_BEG()) {
		c = tSTAR;
	    }
	    else {
		warn_balanced("*", "argument prefix");
		c = '*';
	    }
	}
	SET_LEX_STATE(IS_AFTER_OPERATOR() ? EXPR_ARG : EXPR_BEG);
	return c;

      case '!':
	c = nextc();
	if (IS_AFTER_OPERATOR()) {
	    SET_LEX_STATE(EXPR_ARG);
	    if (c == '@') {
		return '!';
	    }
	}
	else {
	    SET_LEX_STATE(EXPR_BEG);
	}
	if (c == '=') {
	    return tNEQ;
	}
	if (c == '~') {
	    return tNMATCH;
	}
	pushback(c);
	return '!';

      case '=':
	if (was_bol()) {
	    /* skip embedded rd document */
	    if (strncmp(lex_p, "begin", 5) == 0 && ISSPACE(lex_p[5])) {
		int first_p = TRUE;

		lex_goto_eol(parser);
		dispatch_scan_event(tEMBDOC_BEG);
		for (;;) {
		    lex_goto_eol(parser);
		    if (!first_p) {
			dispatch_scan_event(tEMBDOC);
		    }
		    first_p = FALSE;
		    c = nextc();
		    if (c == -1) {
			compile_error(PARSER_ARG "embedded document meets end of file");
			return 0;
		    }
		    if (c != '=') continue;
		    if (c == '=' && strncmp(lex_p, "end", 3) == 0 &&
			(lex_p + 3 == lex_pend || ISSPACE(lex_p[3]))) {
			break;
		    }
		}
		lex_goto_eol(parser);
		dispatch_scan_event(tEMBDOC_END);
		goto retry;
	    }
	}

	SET_LEX_STATE(IS_AFTER_OPERATOR() ? EXPR_ARG : EXPR_BEG);
	if ((c = nextc()) == '=') {
	    if ((c = nextc()) == '=') {
		return tEQQ;
	    }
	    pushback(c);
	    return tEQ;
	}
	if (c == '~') {
	    return tMATCH;
	}
	else if (c == '>') {
	    return tASSOC;
	}
	pushback(c);
	return '=';

      case '<':
	last_state = lex_state;
	c = nextc();
	if (c == '<' &&
	    !IS_lex_state(EXPR_DOT | EXPR_CLASS) &&
	    !IS_END() &&
	    (!IS_ARG() || IS_lex_state(EXPR_LABELED) || space_seen)) {
	    int token = heredoc_identifier();
	    if (token) return token;
	}
	if (IS_AFTER_OPERATOR()) {
	    SET_LEX_STATE(EXPR_ARG);
	}
	else {
	    if (IS_lex_state(EXPR_CLASS))
		command_start = TRUE;
	    SET_LEX_STATE(EXPR_BEG);
	}
	if (c == '=') {
	    if ((c = nextc()) == '>') {
		return tCMP;
	    }
	    pushback(c);
	    return tLEQ;
	}
	if (c == '<') {
	    if ((c = nextc()) == '=') {
                set_yylval_id(tLSHFT);
		SET_LEX_STATE(EXPR_BEG);
		return tOP_ASGN;
	    }
	    pushback(c);
	    warn_balanced("<<", "here document");
	    return tLSHFT;
	}
	pushback(c);
	return '<';

      case '>':
	SET_LEX_STATE(IS_AFTER_OPERATOR() ? EXPR_ARG : EXPR_BEG);
	if ((c = nextc()) == '=') {
	    return tGEQ;
	}
	if (c == '>') {
	    if ((c = nextc()) == '=') {
                set_yylval_id(tRSHFT);
		SET_LEX_STATE(EXPR_BEG);
		return tOP_ASGN;
	    }
	    pushback(c);
	    return tRSHFT;
	}
	pushback(c);
	return '>';

      case '"':
	label = (IS_LABEL_POSSIBLE() ? str_label : 0);
	lex_strterm = NEW_STRTERM(str_dquote | label, '"', 0);
	return tSTRING_BEG;

      case '`':
	if (IS_lex_state(EXPR_FNAME)) {
	    SET_LEX_STATE(EXPR_ENDFN);
	    return c;
	}
	if (IS_lex_state(EXPR_DOT)) {
	    if (cmd_state)
		SET_LEX_STATE(EXPR_CMDARG);
	    else
		SET_LEX_STATE(EXPR_ARG);
	    return c;
	}
	lex_strterm = NEW_STRTERM(str_xquote, '`', 0);
	return tXSTRING_BEG;

      case '\'':
	label = (IS_LABEL_POSSIBLE() ? str_label : 0);
	lex_strterm = NEW_STRTERM(str_squote | label, '\'', 0);
	return tSTRING_BEG;

      case '?':
	return parse_qmark(parser, space_seen);

      case '&':
	if ((c = nextc()) == '&') {
	    SET_LEX_STATE(EXPR_BEG);
	    if ((c = nextc()) == '=') {
                set_yylval_id(tANDOP);
		SET_LEX_STATE(EXPR_BEG);
		return tOP_ASGN;
	    }
	    pushback(c);
	    return tANDOP;
	}
	else if (c == '=') {
            set_yylval_id('&');
	    SET_LEX_STATE(EXPR_BEG);
	    return tOP_ASGN;
	}
	else if (c == '.') {
	    SET_LEX_STATE(EXPR_DOT);
	    return tANDDOT;
	}
	pushback(c);
	if (IS_SPCARG(c)) {
	    rb_warning0("`&' interpreted as argument prefix");
	    c = tAMPER;
	}
	else if (IS_BEG()) {
	    c = tAMPER;
	}
	else {
	    warn_balanced("&", "argument prefix");
	    c = '&';
	}
	SET_LEX_STATE(IS_AFTER_OPERATOR() ? EXPR_ARG : EXPR_BEG);
	return c;

      case '|':
	if ((c = nextc()) == '|') {
	    SET_LEX_STATE(EXPR_BEG);
	    if ((c = nextc()) == '=') {
                set_yylval_id(tOROP);
		SET_LEX_STATE(EXPR_BEG);
		return tOP_ASGN;
	    }
	    pushback(c);
	    return tOROP;
	}
	if (c == '=') {
            set_yylval_id('|');
	    SET_LEX_STATE(EXPR_BEG);
	    return tOP_ASGN;
	}
	SET_LEX_STATE(IS_AFTER_OPERATOR() ? EXPR_ARG : EXPR_BEG|EXPR_LABEL);
	pushback(c);
	return '|';

      case '+':
	c = nextc();
	if (IS_AFTER_OPERATOR()) {
	    SET_LEX_STATE(EXPR_ARG);
	    if (c == '@') {
		return tUPLUS;
	    }
	    pushback(c);
	    return '+';
	}
	if (c == '=') {
            set_yylval_id('+');
	    SET_LEX_STATE(EXPR_BEG);
	    return tOP_ASGN;
	}
	if (IS_BEG() || (IS_SPCARG(c) && arg_ambiguous('+'))) {
	    SET_LEX_STATE(EXPR_BEG);
	    pushback(c);
	    if (c != -1 && ISDIGIT(c)) {
		return parse_numeric(parser, '+');
	    }
	    return tUPLUS;
	}
	SET_LEX_STATE(EXPR_BEG);
	pushback(c);
	warn_balanced("+", "unary operator");
	return '+';

      case '-':
	c = nextc();
	if (IS_AFTER_OPERATOR()) {
	    SET_LEX_STATE(EXPR_ARG);
	    if (c == '@') {
		return tUMINUS;
	    }
	    pushback(c);
	    return '-';
	}
	if (c == '=') {
            set_yylval_id('-');
	    SET_LEX_STATE(EXPR_BEG);
	    return tOP_ASGN;
	}
	if (c == '>') {
	    SET_LEX_STATE(EXPR_ENDFN);
	    token_info_push("->");
	    return tLAMBDA;
	}
	if (IS_BEG() || (IS_SPCARG(c) && arg_ambiguous('-'))) {
	    SET_LEX_STATE(EXPR_BEG);
	    pushback(c);
	    if (c != -1 && ISDIGIT(c)) {
		return tUMINUS_NUM;
	    }
	    return tUMINUS;
	}
	SET_LEX_STATE(EXPR_BEG);
	pushback(c);
	warn_balanced("-", "unary operator");
	return '-';

      case '.':
	SET_LEX_STATE(EXPR_BEG);
	if ((c = nextc()) == '.') {
	    if ((c = nextc()) == '.') {
		return tDOT3;
	    }
	    pushback(c);
	    return tDOT2;
	}
	pushback(c);
	if (c != -1 && ISDIGIT(c)) {
	    yyerror("no .<digit> floating literal anymore; put 0 before dot");
	}
	SET_LEX_STATE(EXPR_DOT);
	return '.';

      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
	return parse_numeric(parser, c);

      case ')':
      case ']':
	paren_nest--;
      case '}':
	COND_LEXPOP();
	CMDARG_LEXPOP();
	if (c == ')')
	    SET_LEX_STATE(EXPR_ENDFN);
	else
	    SET_LEX_STATE(EXPR_ENDARG);
	if (c == '}') {
	    if (!brace_nest--) c = tSTRING_DEND;
	}
	return c;

      case ':':
	c = nextc();
	if (c == ':') {
	    if (IS_BEG() || IS_lex_state(EXPR_CLASS) || IS_SPCARG(-1)) {
		SET_LEX_STATE(EXPR_BEG);
		return tCOLON3;
	    }
	    SET_LEX_STATE(EXPR_DOT);
	    return tCOLON2;
	}
	if (IS_END() || ISSPACE(c) || c == '#') {
	    pushback(c);
	    warn_balanced(":", "symbol literal");
	    SET_LEX_STATE(EXPR_BEG);
	    return ':';
	}
	switch (c) {
	  case '\'':
	    lex_strterm = NEW_STRTERM(str_ssym, c, 0);
	    break;
	  case '"':
	    lex_strterm = NEW_STRTERM(str_dsym, c, 0);
	    break;
	  default:
	    pushback(c);
	    break;
	}
	SET_LEX_STATE(EXPR_FNAME);
	return tSYMBEG;

      case '/':
	if (IS_BEG()) {
	    lex_strterm = NEW_STRTERM(str_regexp, '/', 0);
	    return tREGEXP_BEG;
	}
	if ((c = nextc()) == '=') {
            set_yylval_id('/');
	    SET_LEX_STATE(EXPR_BEG);
	    return tOP_ASGN;
	}
	pushback(c);
	if (IS_SPCARG(c)) {
	    (void)arg_ambiguous('/');
	    lex_strterm = NEW_STRTERM(str_regexp, '/', 0);
	    return tREGEXP_BEG;
	}
	SET_LEX_STATE(IS_AFTER_OPERATOR() ? EXPR_ARG : EXPR_BEG);
	warn_balanced("/", "regexp literal");
	return '/';

      case '^':
	if ((c = nextc()) == '=') {
            set_yylval_id('^');
	    SET_LEX_STATE(EXPR_BEG);
	    return tOP_ASGN;
	}
	SET_LEX_STATE(IS_AFTER_OPERATOR() ? EXPR_ARG : EXPR_BEG);
	pushback(c);
	return '^';

      case ';':
	SET_LEX_STATE(EXPR_BEG);
	command_start = TRUE;
	return ';';

      case ',':
	SET_LEX_STATE(EXPR_BEG|EXPR_LABEL);
	return ',';

      case '~':
	if (IS_AFTER_OPERATOR()) {
	    if ((c = nextc()) != '@') {
		pushback(c);
	    }
	    SET_LEX_STATE(EXPR_ARG);
	}
	else {
	    SET_LEX_STATE(EXPR_BEG);
	}
	return '~';

      case '(':
	if (IS_BEG()) {
	    c = tLPAREN;
	}
	else if (IS_SPCARG(-1)) {
	    c = tLPAREN_ARG;
	}
	else if (IS_lex_state(EXPR_ENDFN) && space_seen && !lambda_beginning_p()) {
	    rb_warning0("parentheses after method name is interpreted as "
			"an argument list, not a decomposed argument");
	}
	paren_nest++;
	COND_PUSH(0);
	CMDARG_PUSH(0);
	SET_LEX_STATE(EXPR_BEG|EXPR_LABEL);
	return c;

      case '[':
	paren_nest++;
	if (IS_AFTER_OPERATOR()) {
	    SET_LEX_STATE(EXPR_ARG);
	    if ((c = nextc()) == ']') {
		if ((c = nextc()) == '=') {
		    return tASET;
		}
		pushback(c);
		return tAREF;
	    }
	    pushback(c);
	    lex_state |= EXPR_LABEL;
	    return '[';
	}
	else if (IS_BEG()) {
	    c = tLBRACK;
	}
	else if (IS_ARG() && (space_seen || IS_lex_state(EXPR_LABELED))) {
	    c = tLBRACK;
	}
	SET_LEX_STATE(EXPR_BEG|EXPR_LABEL);
	COND_PUSH(0);
	CMDARG_PUSH(0);
	return c;

      case '{':
	++brace_nest;
	if (lambda_beginning_p()) {
	    SET_LEX_STATE(EXPR_BEG);
	    lpar_beg = 0;
	    --paren_nest;
	    COND_PUSH(0);
	    CMDARG_PUSH(0);
	    return tLAMBEG;
	}
	if (IS_lex_state(EXPR_LABELED))
	    c = tLBRACE;      /* hash */
	else if (IS_lex_state(EXPR_ARG_ANY | EXPR_END | EXPR_ENDFN))
	    c = '{';          /* block (primary) */
	else if (IS_lex_state(EXPR_ENDARG))
	    c = tLBRACE_ARG;  /* block (expr) */
	else
	    c = tLBRACE;      /* hash */
	COND_PUSH(0);
	CMDARG_PUSH(0);
	SET_LEX_STATE(EXPR_BEG);
	if (c != tLBRACE_ARG) lex_state |= EXPR_LABEL;
	if (c != tLBRACE) command_start = TRUE;
	return c;

      case '\\':
	c = nextc();
	if (c == '\n') {
	    space_seen = 1;
	    dispatch_scan_event(tSP);
	    goto retry; /* skip \\n */
	}
	pushback(c);
	return '\\';

      case '%':
	return parse_percent(parser, space_seen, last_state);

      case '$':
	return parse_gvar(parser, last_state);

      case '@':
	return parse_atmark(parser, last_state);

      case '_':
	if (was_bol() && whole_match_p("__END__", 7, 0)) {
	    ruby__end__seen = 1;
	    parser->eofp = 1;
#ifndef RIPPER
	    return -1;
#else
            lex_goto_eol(parser);
            dispatch_scan_event(k__END__);
            return 0;
#endif
	}
	newtok();
	break;

      default:
	if (!parser_is_identchar()) {
	    compile_error(PARSER_ARG  "Invalid char `\\x%02X' in expression", c);
	    goto retry;
	}

	newtok();
	break;
    }

    return parse_ident(parser, c, cmd_state);
}

static int
yylex(YYSTYPE *lval, struct parser_params *parser)
{
    int t;

    parser->lval = lval;
    lval->val = Qundef;
    t = parser_yylex(parser);
    if (has_delayed_token())
	dispatch_delayed_token(t);
    else if (t != 0)
	dispatch_scan_event(t);

    return t;
}

#ifndef RIPPER
static NODE*
node_newnode(struct parser_params *parser, enum node_type type, VALUE a0, VALUE a1, VALUE a2)
{
    NODE *n = (rb_node_newnode)(type, a0, a1, a2);
    nd_set_line(n, ruby_sourceline);
    return n;
}

static enum node_type
nodetype(NODE *node)			/* for debug */
{
    return (enum node_type)nd_type(node);
}

static int
nodeline(NODE *node)
{
    return nd_line(node);
}

static NODE*
newline_node(NODE *node)
{
    if (node) {
	node = remove_begin(node);
	node->flags |= NODE_FL_NEWLINE;
    }
    return node;
}

static void
fixpos(NODE *node, NODE *orig)
{
    if (!node) return;
    if (!orig) return;
    if (orig == (NODE*)1) return;
    nd_set_line(node, nd_line(orig));
}

static void
parser_warning(struct parser_params *parser, NODE *node, const char *mesg)
{
    rb_compile_warning(ruby_sourcefile, nd_line(node), "%s", mesg);
}
#define parser_warning(node, mesg) parser_warning(parser, (node), (mesg))

static void
parser_warn(struct parser_params *parser, NODE *node, const char *mesg)
{
    rb_compile_warn(ruby_sourcefile, nd_line(node), "%s", mesg);
}
#define parser_warn(node, mesg) parser_warn(parser, (node), (mesg))

static NODE*
block_append_gen(struct parser_params *parser, NODE *head, NODE *tail)
{
    NODE *end, *h = head, *nd;

    if (tail == 0) return head;

    if (h == 0) return tail;
    switch (nd_type(h)) {
      case NODE_LIT:
      case NODE_STR:
      case NODE_SELF:
      case NODE_TRUE:
      case NODE_FALSE:
      case NODE_NIL:
	parser_warning(h, "unused literal ignored");
	return tail;
      default:
	h = end = NEW_BLOCK(head);
	end->nd_end = end;
	fixpos(end, head);
	head = end;
	break;
      case NODE_BLOCK:
	end = h->nd_end;
	break;
    }

    nd = end->nd_head;
    switch (nd_type(nd)) {
      case NODE_RETURN:
      case NODE_BREAK:
      case NODE_NEXT:
      case NODE_REDO:
      case NODE_RETRY:
	if (RTEST(ruby_verbose)) {
	    parser_warning(tail, "statement not reached");
	}
	break;

      default:
	break;
    }

    if (nd_type(tail) != NODE_BLOCK) {
	tail = NEW_BLOCK(tail);
	tail->nd_end = tail;
    }
    end->nd_next = tail;
    h->nd_end = tail->nd_end;
    return head;
}

/* append item to the list */
static NODE*
list_append_gen(struct parser_params *parser, NODE *list, NODE *item)
{
    NODE *last;

    if (list == 0) return NEW_LIST(item);
    if (list->nd_next) {
	last = list->nd_next->nd_end;
    }
    else {
	last = list;
    }

    list->nd_alen += 1;
    last->nd_next = NEW_LIST(item);
    list->nd_next->nd_end = last->nd_next;
    return list;
}

/* concat two lists */
static NODE*
list_concat(NODE *head, NODE *tail)
{
    NODE *last;

    if (head->nd_next) {
	last = head->nd_next->nd_end;
    }
    else {
	last = head;
    }

    head->nd_alen += tail->nd_alen;
    last->nd_next = tail;
    if (tail->nd_next) {
	head->nd_next->nd_end = tail->nd_next->nd_end;
    }
    else {
	head->nd_next->nd_end = tail;
    }

    return head;
}

static int
literal_concat0(struct parser_params *parser, VALUE head, VALUE tail)
{
    if (NIL_P(tail)) return 1;
    if (!rb_enc_compatible(head, tail)) {
	compile_error(PARSER_ARG "string literal encodings differ (%s / %s)",
		      rb_enc_name(rb_enc_get(head)),
		      rb_enc_name(rb_enc_get(tail)));
	rb_str_resize(head, 0);
	rb_str_resize(tail, 0);
	return 0;
    }
    rb_str_buf_append(head, tail);
    return 1;
}

/* concat two string literals */
static NODE *
literal_concat_gen(struct parser_params *parser, NODE *head, NODE *tail)
{
    enum node_type htype;
    NODE *headlast;
    VALUE lit;

    if (!head) return tail;
    if (!tail) return head;

    htype = nd_type(head);
    if (htype == NODE_EVSTR) {
	NODE *node = NEW_DSTR(STR_NEW0());
	head = list_append(node, head);
	htype = NODE_DSTR;
    }
    if (heredoc_indent > 0) {
	switch (htype) {
	  case NODE_STR:
	    nd_set_type(head, NODE_DSTR);
	  case NODE_DSTR:
	    return list_append(head, tail);
	  default:
	    break;
	}
    }
    switch (nd_type(tail)) {
      case NODE_STR:
	if (htype == NODE_DSTR && (headlast = head->nd_next->nd_end->nd_head) &&
	    nd_type(headlast) == NODE_STR) {
	    htype = NODE_STR;
	    lit = headlast->nd_lit;
	}
	else {
	    lit = head->nd_lit;
	}
	if (htype == NODE_STR) {
	    if (!literal_concat0(parser, lit, tail->nd_lit)) {
	      error:
		rb_gc_force_recycle((VALUE)head);
		rb_gc_force_recycle((VALUE)tail);
		return 0;
	    }
	    rb_gc_force_recycle((VALUE)tail);
	}
	else {
	    list_append(head, tail);
	}
	break;

      case NODE_DSTR:
	if (htype == NODE_STR) {
	    if (!literal_concat0(parser, head->nd_lit, tail->nd_lit))
		goto error;
	    tail->nd_lit = head->nd_lit;
	    rb_gc_force_recycle((VALUE)head);
	    head = tail;
	}
	else if (NIL_P(tail->nd_lit)) {
	  append:
	    head->nd_alen += tail->nd_alen - 1;
	    head->nd_next->nd_end->nd_next = tail->nd_next;
	    head->nd_next->nd_end = tail->nd_next->nd_end;
	    rb_gc_force_recycle((VALUE)tail);
	}
	else if (htype == NODE_DSTR && (headlast = head->nd_next->nd_end->nd_head) &&
		 nd_type(headlast) == NODE_STR) {
	    lit = headlast->nd_lit;
	    if (!literal_concat0(parser, lit, tail->nd_lit))
		goto error;
	    tail->nd_lit = Qnil;
	    goto append;
	}
	else {
	    nd_set_type(tail, NODE_ARRAY);
	    tail->nd_head = NEW_STR(tail->nd_lit);
	    list_concat(head, tail);
	}
	break;

      case NODE_EVSTR:
	if (htype == NODE_STR) {
	    nd_set_type(head, NODE_DSTR);
	    head->nd_alen = 1;
	}
	list_append(head, tail);
	break;
    }
    return head;
}

static NODE *
evstr2dstr_gen(struct parser_params *parser, NODE *node)
{
    if (nd_type(node) == NODE_EVSTR) {
	node = list_append(NEW_DSTR(STR_NEW0()), node);
    }
    return node;
}

static NODE *
new_evstr_gen(struct parser_params *parser, NODE *node)
{
    NODE *head = node;

    if (node) {
	switch (nd_type(node)) {
	  case NODE_STR: case NODE_DSTR: case NODE_EVSTR:
	    return node;
	}
    }
    return NEW_EVSTR(head);
}

static NODE *
call_bin_op_gen(struct parser_params *parser, NODE *recv, ID id, NODE *arg1)
{
    value_expr(recv);
    value_expr(arg1);
    return NEW_CALL(recv, id, NEW_LIST(arg1));
}

static NODE *
call_uni_op_gen(struct parser_params *parser, NODE *recv, ID id)
{
    value_expr(recv);
    return NEW_CALL(recv, id, 0);
}

static NODE*
match_op_gen(struct parser_params *parser, NODE *node1, NODE *node2)
{
    value_expr(node1);
    value_expr(node2);
    if (node1) {
	switch (nd_type(node1)) {
	  case NODE_DREGX:
	  case NODE_DREGX_ONCE:
	    return NEW_MATCH2(node1, node2);

	  case NODE_LIT:
	    if (RB_TYPE_P(node1->nd_lit, T_REGEXP)) {
		return NEW_MATCH2(node1, node2);
	    }
	}
    }

    if (node2) {
	switch (nd_type(node2)) {
	  case NODE_DREGX:
	  case NODE_DREGX_ONCE:
	    return NEW_MATCH3(node2, node1);

	  case NODE_LIT:
	    if (RB_TYPE_P(node2->nd_lit, T_REGEXP)) {
		return NEW_MATCH3(node2, node1);
	    }
	}
    }

    return NEW_CALL(node1, tMATCH, NEW_LIST(node2));
}

# if WARN_PAST_SCOPE
static int
past_dvar_p(struct parser_params *parser, ID id)
{
    struct vtable *past = lvtbl->past;
    while (past) {
	if (vtable_included(past, id)) return 1;
	past = past->prev;
    }
    return 0;
}
# endif

static NODE*
gettable_gen(struct parser_params *parser, ID id)
{
    switch (id) {
      case keyword_self:
	return NEW_SELF();
      case keyword_nil:
	return NEW_NIL();
      case keyword_true:
	return NEW_TRUE();
      case keyword_false:
	return NEW_FALSE();
      case keyword__FILE__:
	return NEW_STR(rb_str_dup(ruby_sourcefile_string));
      case keyword__LINE__:
	return NEW_LIT(INT2FIX(tokline));
      case keyword__ENCODING__:
	return NEW_LIT(rb_enc_from_encoding(current_enc));
    }
    switch (id_type(id)) {
      case ID_LOCAL:
	if (dyna_in_block() && dvar_defined(id)) {
	    if (id == current_arg) {
		rb_warn1("circular argument reference - %"PRIsWARN, rb_id2str(id));
	    }
	    return NEW_DVAR(id);
	}
	if (local_id(id)) {
	    if (id == current_arg) {
		rb_warn1("circular argument reference - %"PRIsWARN, rb_id2str(id));
	    }
	    return NEW_LVAR(id);
	}
# if WARN_PAST_SCOPE
	if (!in_defined && RTEST(ruby_verbose) && past_dvar_p(parser, id)) {
	    rb_warning1("possible reference to past scope - %"PRIsWARN, rb_id2str(id));
	}
# endif
	/* method call without arguments */
	return NEW_VCALL(id);
      case ID_GLOBAL:
	return NEW_GVAR(id);
      case ID_INSTANCE:
	return NEW_IVAR(id);
      case ID_CONST:
	return NEW_CONST(id);
      case ID_CLASS:
	return NEW_CVAR(id);
    }
    compile_error(PARSER_ARG "identifier %"PRIsVALUE" is not valid to get", rb_id2str(id));
    return 0;
}

static NODE *
kwd_append(NODE *kwlist, NODE *kw)
{
    if (kwlist) {
	NODE *kws = kwlist;
	while (kws->nd_next) {
	    kws = kws->nd_next;
	}
	kws->nd_next = kw;
    }
    return kwlist;
}

static NODE *
new_regexp_gen(struct parser_params *parser, NODE *node, int options)
{
    NODE *list, *prev;

    if (!node) {
	return NEW_LIT(reg_compile(STR_NEW0(), options));
    }
    switch (nd_type(node)) {
      case NODE_STR:
	{
	    VALUE src = node->nd_lit;
	    nd_set_type(node, NODE_LIT);
	    node->nd_lit = reg_compile(src, options);
	}
	break;
      default:
	node = NEW_NODE(NODE_DSTR, STR_NEW0(), 1, NEW_LIST(node));
      case NODE_DSTR:
	if (options & RE_OPTION_ONCE) {
	    nd_set_type(node, NODE_DREGX_ONCE);
	}
	else {
	    nd_set_type(node, NODE_DREGX);
	}
	node->nd_cflag = options & RE_OPTION_MASK;
	if (!NIL_P(node->nd_lit)) reg_fragment_check(node->nd_lit, options);
	for (list = (prev = node)->nd_next; list; list = list->nd_next) {
	    if (nd_type(list->nd_head) == NODE_STR) {
		VALUE tail = list->nd_head->nd_lit;
		if (reg_fragment_check(tail, options) && prev && !NIL_P(prev->nd_lit)) {
		    VALUE lit = prev == node ? prev->nd_lit : prev->nd_head->nd_lit;
		    if (!literal_concat0(parser, lit, tail)) {
			return NEW_NIL(); /* dummy node on error */
		    }
		    rb_str_resize(tail, 0);
		    prev->nd_next = list->nd_next;
		    rb_gc_force_recycle((VALUE)list->nd_head);
		    rb_gc_force_recycle((VALUE)list);
		    list = prev;
		}
		else {
		    prev = list;
		}
	    }
	    else {
		prev = 0;
	    }
	}
	if (!node->nd_next) {
	    VALUE src = node->nd_lit;
	    nd_set_type(node, NODE_LIT);
	    node->nd_lit = reg_compile(src, options);
	}
	break;
    }
    return node;
}

static NODE *
new_xstring_gen(struct parser_params *parser, NODE *node)
{
    if (!node) {
	return NEW_XSTR(STR_NEW0());
    }
    switch (nd_type(node)) {
      case NODE_STR:
	nd_set_type(node, NODE_XSTR);
	break;
      case NODE_DSTR:
	nd_set_type(node, NODE_DXSTR);
	break;
      default:
	node = NEW_NODE(NODE_DXSTR, Qnil, 1, NEW_LIST(node));
	break;
    }
    return node;
}
#else  /* !RIPPER */
static int
id_is_var_gen(struct parser_params *parser, ID id)
{
    if (is_notop_id(id)) {
	switch (id & ID_SCOPE_MASK) {
	  case ID_GLOBAL: case ID_INSTANCE: case ID_CONST: case ID_CLASS:
	    return 1;
	  case ID_LOCAL:
	    if (dyna_in_block() && dvar_defined(id)) return 1;
	    if (local_id(id)) return 1;
	    /* method call without arguments */
	    return 0;
	}
    }
    compile_error(PARSER_ARG "identifier %s is not valid to get", rb_id2str(id));
    return 0;
}

static VALUE
new_regexp_gen(struct parser_params *parser, VALUE re, VALUE opt)
{
    VALUE src = 0, err;
    int options = 0;
    if (ripper_is_node_yylval(re)) {
	src = RNODE(re)->nd_cval;
	re = RNODE(re)->nd_rval;
    }
    if (ripper_is_node_yylval(opt)) {
	options = (int)RNODE(opt)->nd_tag;
	opt = RNODE(opt)->nd_rval;
    }
    if (src && NIL_P(parser_reg_compile(parser, src, options, &err))) {
	compile_error(PARSER_ARG "%"PRIsVALUE, err);
    }
    return dispatch2(regexp_literal, re, opt);
}

static VALUE
new_xstring_gen(struct parser_params *parser, VALUE str)
{
    return dispatch1(xstring_literal, str);
}
#endif /* !RIPPER */

static const char lex_state_names[][13] = {
    "EXPR_BEG",    "EXPR_END",    "EXPR_ENDARG", "EXPR_ENDFN",  "EXPR_ARG",
    "EXPR_CMDARG", "EXPR_MID",    "EXPR_FNAME",  "EXPR_DOT",    "EXPR_CLASS",
    "EXPR_LABEL",  "EXPR_LABELED","EXPR_FITEM",
};

static VALUE
append_lex_state_name(enum lex_state_e state, VALUE buf)
{
    int i, sep = 0;
    unsigned int mask = 1;
    static const char none[] = "EXPR_NONE";

    for (i = 0; i < EXPR_MAX_STATE; ++i, mask <<= 1) {
	if ((unsigned)state & mask) {
	    if (sep) {
		rb_str_cat(buf, "|", 1);
	    }
	    sep = 1;
	    rb_str_cat_cstr(buf, lex_state_names[i]);
	}
    }
    if (!sep) {
	rb_str_cat(buf, none, sizeof(none)-1);
    }
    return buf;
}

static enum lex_state_e
trace_lex_state(enum lex_state_e from, enum lex_state_e to, int line)
{
    VALUE mesg;
    mesg = rb_str_new_cstr("lex_state: ");
    append_lex_state_name(from, mesg);
    rb_str_cat_cstr(mesg, " -> ");
    append_lex_state_name(to, mesg);
    rb_str_catf(mesg, " at line %d\n", line);
    rb_io_write(rb_stdout, mesg);
    return to;
}

static void
show_bitstack(stack_type stack, const char *name, int line)
{
    VALUE mesg = rb_sprintf("%s: ", name);
    if (stack == 0) {
	rb_str_cat_cstr(mesg, "0");
    }
    else {
	stack_type mask = (stack_type)1U << (CHAR_BIT * sizeof(stack_type) - 1);
	for (; mask && !(stack & mask); mask >>= 1) continue;
	for (; mask; mask >>= 1) rb_str_cat(mesg, stack & mask ? "1" : "0", 1);
    }
    rb_str_catf(mesg, " at line %d\n", line);
    rb_io_write(rb_stdout, mesg);
}

#ifdef RIPPER
static VALUE
assignable_gen(struct parser_params *parser, VALUE lhs)
#else
static NODE*
assignable_gen(struct parser_params *parser, ID id, NODE *val)
#endif
{
#ifdef RIPPER
    ID id = get_id(lhs);
# define assignable_result(x) get_value(lhs)
# define parser_yyerror(parser, x) assign_error_gen(parser, lhs)
#else
# define assignable_result(x) (x)
#endif
    if (!id) return assignable_result(0);
    switch (id) {
      case keyword_self:
	yyerror("Can't change the value of self");
	goto error;
      case keyword_nil:
	yyerror("Can't assign to nil");
	goto error;
      case keyword_true:
	yyerror("Can't assign to true");
	goto error;
      case keyword_false:
	yyerror("Can't assign to false");
	goto error;
      case keyword__FILE__:
	yyerror("Can't assign to __FILE__");
	goto error;
      case keyword__LINE__:
	yyerror("Can't assign to __LINE__");
	goto error;
      case keyword__ENCODING__:
	yyerror("Can't assign to __ENCODING__");
	goto error;
    }
    switch (id_type(id)) {
      case ID_LOCAL:
	if (dyna_in_block()) {
	    if (dvar_curr(id)) {
		return assignable_result(NEW_DASGN_CURR(id, val));
	    }
	    else if (dvar_defined(id)) {
		return assignable_result(NEW_DASGN(id, val));
	    }
	    else if (local_id(id)) {
		return assignable_result(NEW_LASGN(id, val));
	    }
	    else {
		dyna_var(id);
		return assignable_result(NEW_DASGN_CURR(id, val));
	    }
	}
	else {
	    if (!local_id(id)) {
		local_var(id);
	    }
	    return assignable_result(NEW_LASGN(id, val));
	}
	break;
      case ID_GLOBAL:
	return assignable_result(NEW_GASGN(id, val));
      case ID_INSTANCE:
	return assignable_result(NEW_IASGN(id, val));
      case ID_CONST:
	if (!in_def && !in_single)
	    return assignable_result(NEW_CDECL(id, val, 0));
	yyerror("dynamic constant assignment");
	break;
      case ID_CLASS:
	return assignable_result(NEW_CVASGN(id, val));
      default:
	compile_error(PARSER_ARG "identifier %"PRIsVALUE" is not valid to set", rb_id2str(id));
    }
  error:
    return assignable_result(0);
#undef assignable_result
#undef parser_yyerror
}

static int
is_private_local_id(ID name)
{
    VALUE s;
    if (name == idUScore) return 1;
    if (!is_local_id(name)) return 0;
    s = rb_id2str(name);
    if (!s) return 0;
    return RSTRING_PTR(s)[0] == '_';
}

#define LVAR_USED ((ID)1 << (sizeof(ID) * CHAR_BIT - 1))

static int
shadowing_lvar_0(struct parser_params *parser, ID name)
{
    if (is_private_local_id(name)) return 1;
    if (dyna_in_block()) {
	if (dvar_curr(name)) {
	    yyerror("duplicated argument name");
	}
	else if (dvar_defined_get(name) || local_id(name)) {
	    rb_warning1("shadowing outer local variable - %"PRIsWARN, rb_id2str(name));
	    vtable_add(lvtbl->vars, name);
	    if (lvtbl->used) {
		vtable_add(lvtbl->used, (ID)ruby_sourceline | LVAR_USED);
	    }
	    return 0;
	}
    }
    else {
	if (local_id(name)) {
	    yyerror("duplicated argument name");
	}
    }
    return 1;
}

static ID
shadowing_lvar_gen(struct parser_params *parser, ID name)
{
    shadowing_lvar_0(parser, name);
    return name;
}

static void
new_bv_gen(struct parser_params *parser, ID name)
{
    if (!name) return;
    if (!is_local_id(name)) {
	compile_error(PARSER_ARG "invalid local variable - %"PRIsVALUE,
		      rb_id2str(name));
	return;
    }
    if (!shadowing_lvar_0(parser, name)) return;
    dyna_var(name);
}

#ifndef RIPPER
static NODE *
aryset_gen(struct parser_params *parser, NODE *recv, NODE *idx)
{
    return NEW_ATTRASGN(recv, tASET, idx);
}

static void
block_dup_check_gen(struct parser_params *parser, NODE *node1, NODE *node2)
{
    if (node2 && node1 && nd_type(node1) == NODE_BLOCK_PASS) {
	compile_error(PARSER_ARG "both block arg and actual block given");
    }
}

static NODE *
attrset_gen(struct parser_params *parser, NODE *recv, ID atype, ID id)
{
    if (!CALL_Q_P(atype)) id = rb_id_attrset(id);
    return NEW_ATTRASGN(recv, id, 0);
}

static void
rb_backref_error_gen(struct parser_params *parser, NODE *node)
{
    switch (nd_type(node)) {
      case NODE_NTH_REF:
	compile_error(PARSER_ARG "Can't set variable $%ld", node->nd_nth);
	break;
      case NODE_BACK_REF:
	compile_error(PARSER_ARG "Can't set variable $%c", (int)node->nd_nth);
	break;
    }
}

static NODE *
arg_concat_gen(struct parser_params *parser, NODE *node1, NODE *node2)
{
    if (!node2) return node1;
    switch (nd_type(node1)) {
      case NODE_BLOCK_PASS:
	if (node1->nd_head)
	    node1->nd_head = arg_concat(node1->nd_head, node2);
	else
	    node1->nd_head = NEW_LIST(node2);
	return node1;
      case NODE_ARGSPUSH:
	if (nd_type(node2) != NODE_ARRAY) break;
	node1->nd_body = list_concat(NEW_LIST(node1->nd_body), node2);
	nd_set_type(node1, NODE_ARGSCAT);
	return node1;
      case NODE_ARGSCAT:
	if (nd_type(node2) != NODE_ARRAY ||
	    nd_type(node1->nd_body) != NODE_ARRAY) break;
	node1->nd_body = list_concat(node1->nd_body, node2);
	return node1;
    }
    return NEW_ARGSCAT(node1, node2);
}

static NODE *
arg_append_gen(struct parser_params *parser, NODE *node1, NODE *node2)
{
    if (!node1) return NEW_LIST(node2);
    switch (nd_type(node1))  {
      case NODE_ARRAY:
	return list_append(node1, node2);
      case NODE_BLOCK_PASS:
	node1->nd_head = arg_append(node1->nd_head, node2);
	return node1;
      case NODE_ARGSPUSH:
	node1->nd_body = list_append(NEW_LIST(node1->nd_body), node2);
	nd_set_type(node1, NODE_ARGSCAT);
	return node1;
    }
    return NEW_ARGSPUSH(node1, node2);
}

static NODE *
splat_array(NODE* node)
{
    if (nd_type(node) == NODE_SPLAT) node = node->nd_head;
    if (nd_type(node) == NODE_ARRAY) return node;
    return 0;
}

static NODE *
node_assign_gen(struct parser_params *parser, NODE *lhs, NODE *rhs)
{
    if (!lhs) return 0;

    switch (nd_type(lhs)) {
      case NODE_GASGN:
      case NODE_IASGN:
      case NODE_IASGN2:
      case NODE_LASGN:
      case NODE_DASGN:
      case NODE_DASGN_CURR:
      case NODE_MASGN:
      case NODE_CDECL:
      case NODE_CVASGN:
	lhs->nd_value = rhs;
	break;

      case NODE_ATTRASGN:
      case NODE_CALL:
	lhs->nd_args = arg_append(lhs->nd_args, rhs);
	break;

      default:
	/* should not happen */
	break;
    }

    return lhs;
}

static int
value_expr_gen(struct parser_params *parser, NODE *node)
{
    int cond = 0;

    if (!node) {
	rb_warning0("empty expression");
    }
    while (node) {
	switch (nd_type(node)) {
	  case NODE_RETURN:
	  case NODE_BREAK:
	  case NODE_NEXT:
	  case NODE_REDO:
	  case NODE_RETRY:
	    if (!cond) yyerror("void value expression");
	    /* or "control never reach"? */
	    return FALSE;

	  case NODE_BLOCK:
	    while (node->nd_next) {
		node = node->nd_next;
	    }
	    node = node->nd_head;
	    break;

	  case NODE_BEGIN:
	    node = node->nd_body;
	    break;

	  case NODE_IF:
	    if (!node->nd_body) {
		node = node->nd_else;
		break;
	    }
	    else if (!node->nd_else) {
		node = node->nd_body;
		break;
	    }
	    if (!value_expr(node->nd_body)) return FALSE;
	    node = node->nd_else;
	    break;

	  case NODE_AND:
	  case NODE_OR:
	    cond = 1;
	    node = node->nd_2nd;
	    break;

	  default:
	    return TRUE;
	}
    }

    return TRUE;
}

static void
void_expr_gen(struct parser_params *parser, NODE *node)
{
    const char *useless = 0;

    if (!RTEST(ruby_verbose)) return;

    if (!node) return;
    switch (nd_type(node)) {
      case NODE_CALL:
	switch (node->nd_mid) {
	  case '+':
	  case '-':
	  case '*':
	  case '/':
	  case '%':
	  case tPOW:
	  case tUPLUS:
	  case tUMINUS:
	  case '|':
	  case '^':
	  case '&':
	  case tCMP:
	  case '>':
	  case tGEQ:
	  case '<':
	  case tLEQ:
	  case tEQ:
	  case tNEQ:
	    useless = rb_id2name(node->nd_mid);
	    break;
	}
	break;

      case NODE_LVAR:
      case NODE_DVAR:
      case NODE_GVAR:
      case NODE_IVAR:
      case NODE_CVAR:
      case NODE_NTH_REF:
      case NODE_BACK_REF:
	useless = "a variable";
	break;
      case NODE_CONST:
	useless = "a constant";
	break;
      case NODE_LIT:
      case NODE_STR:
      case NODE_DSTR:
      case NODE_DREGX:
      case NODE_DREGX_ONCE:
	useless = "a literal";
	break;
      case NODE_COLON2:
      case NODE_COLON3:
	useless = "::";
	break;
      case NODE_DOT2:
	useless = "..";
	break;
      case NODE_DOT3:
	useless = "...";
	break;
      case NODE_SELF:
	useless = "self";
	break;
      case NODE_NIL:
	useless = "nil";
	break;
      case NODE_TRUE:
	useless = "true";
	break;
      case NODE_FALSE:
	useless = "false";
	break;
      case NODE_DEFINED:
	useless = "defined?";
	break;
    }

    if (useless) {
	rb_warn1L(nd_line(node), "possibly useless use of %s in void context", WARN_S(useless));
    }
}

static void
void_stmts_gen(struct parser_params *parser, NODE *node)
{
    if (!RTEST(ruby_verbose)) return;
    if (!node) return;
    if (nd_type(node) != NODE_BLOCK) return;

    for (;;) {
	if (!node->nd_next) return;
	void_expr0(node->nd_head);
	node = node->nd_next;
    }
}

static NODE *
remove_begin(NODE *node)
{
    NODE **n = &node, *n1 = node;
    while (n1 && nd_type(n1) == NODE_BEGIN && n1->nd_body) {
	*n = n1 = n1->nd_body;
    }
    return node;
}

static NODE *
remove_begin_all(NODE *node)
{
    NODE **n = &node, *n1 = node;
    while (n1 && nd_type(n1) == NODE_BEGIN) {
	*n = n1 = n1->nd_body;
    }
    return node;
}

static void
reduce_nodes_gen(struct parser_params *parser, NODE **body)
{
    NODE *node = *body;

    if (!node) {
	*body = NEW_NIL();
	return;
    }
#define subnodes(n1, n2) \
    ((!node->n1) ? (node->n2 ? (body = &node->n2, 1) : 0) : \
     (!node->n2) ? (body = &node->n1, 1) : \
     (reduce_nodes(&node->n1), body = &node->n2, 1))

    while (node) {
	int newline = (int)(node->flags & NODE_FL_NEWLINE);
	switch (nd_type(node)) {
	  end:
	  case NODE_NIL:
	    *body = 0;
	    return;
	  case NODE_RETURN:
	    *body = node = node->nd_stts;
	    if (newline && node) node->flags |= NODE_FL_NEWLINE;
	    continue;
	  case NODE_BEGIN:
	    *body = node = node->nd_body;
	    if (newline && node) node->flags |= NODE_FL_NEWLINE;
	    continue;
	  case NODE_BLOCK:
	    body = &node->nd_end->nd_head;
	    break;
	  case NODE_IF:
	    if (subnodes(nd_body, nd_else)) break;
	    return;
	  case NODE_CASE:
	    body = &node->nd_body;
	    break;
	  case NODE_WHEN:
	    if (!subnodes(nd_body, nd_next)) goto end;
	    break;
	  case NODE_ENSURE:
	    if (!subnodes(nd_head, nd_resq)) goto end;
	    break;
	  case NODE_RESCUE:
	    if (node->nd_else) {
		body = &node->nd_resq;
		break;
	    }
	    if (!subnodes(nd_head, nd_resq)) goto end;
	    break;
	  default:
	    return;
	}
	node = *body;
	if (newline && node) node->flags |= NODE_FL_NEWLINE;
    }

#undef subnodes
}

static int
is_static_content(NODE *node)
{
    if (!node) return 1;
    switch (nd_type(node)) {
      case NODE_HASH:
	if (!(node = node->nd_head)) break;
      case NODE_ARRAY:
	do {
	    if (!is_static_content(node->nd_head)) return 0;
	} while ((node = node->nd_next) != 0);
      case NODE_LIT:
      case NODE_STR:
      case NODE_NIL:
      case NODE_TRUE:
      case NODE_FALSE:
      case NODE_ZARRAY:
	break;
      default:
	return 0;
    }
    return 1;
}

static int
assign_in_cond(struct parser_params *parser, NODE *node)
{
    switch (nd_type(node)) {
      case NODE_MASGN:
      case NODE_LASGN:
      case NODE_DASGN:
      case NODE_DASGN_CURR:
      case NODE_GASGN:
      case NODE_IASGN:
	break;

      default:
	return 0;
    }

    if (!node->nd_value) return 1;
    if (is_static_content(node->nd_value)) {
	/* reports always */
	parser_warn(node->nd_value, "found = in conditional, should be ==");
    }
    return 1;
}

static void
warn_unless_e_option(struct parser_params *parser, NODE *node, const char *str)
{
    if (!e_option_supplied(parser)) parser_warn(node, str);
}

static void
warning_unless_e_option(struct parser_params *parser, NODE *node, const char *str)
{
    if (!e_option_supplied(parser)) parser_warning(node, str);
}

static NODE *cond0(struct parser_params*,NODE*,int);

static NODE*
range_op(struct parser_params *parser, NODE *node)
{
    enum node_type type;

    if (node == 0) return 0;

    type = nd_type(node);
    value_expr(node);
    if (type == NODE_LIT && FIXNUM_P(node->nd_lit)) {
	warn_unless_e_option(parser, node, "integer literal in conditional range");
	return NEW_CALL(node, tEQ, NEW_LIST(NEW_GVAR(rb_intern("$."))));
    }
    return cond0(parser, node, FALSE);
}

static int
literal_node(NODE *node)
{
    if (!node) return 1;	/* same as NODE_NIL */
    switch (nd_type(node)) {
      case NODE_LIT:
      case NODE_STR:
      case NODE_DSTR:
      case NODE_EVSTR:
      case NODE_DREGX:
      case NODE_DREGX_ONCE:
      case NODE_DSYM:
	return 2;
      case NODE_TRUE:
      case NODE_FALSE:
      case NODE_NIL:
	return 1;
    }
    return 0;
}

static NODE*
cond0(struct parser_params *parser, NODE *node, int method_op)
{
    if (node == 0) return 0;
    assign_in_cond(parser, node);

    switch (nd_type(node)) {
      case NODE_DSTR:
      case NODE_EVSTR:
      case NODE_STR:
	if (!method_op) rb_warn0("string literal in condition");
	break;

      case NODE_DREGX:
      case NODE_DREGX_ONCE:
	if (!method_op)
	    warning_unless_e_option(parser, node, "regex literal in condition");
	return NEW_MATCH2(node, NEW_GVAR(idLASTLINE));

      case NODE_AND:
      case NODE_OR:
	node->nd_1st = cond0(parser, node->nd_1st, FALSE);
	node->nd_2nd = cond0(parser, node->nd_2nd, FALSE);
	break;

      case NODE_DOT2:
      case NODE_DOT3:
	node->nd_beg = range_op(parser, node->nd_beg);
	node->nd_end = range_op(parser, node->nd_end);
	if (nd_type(node) == NODE_DOT2) nd_set_type(node,NODE_FLIP2);
	else if (nd_type(node) == NODE_DOT3) nd_set_type(node, NODE_FLIP3);
	if (!method_op && !e_option_supplied(parser)) {
	    int b = literal_node(node->nd_beg);
	    int e = literal_node(node->nd_end);
	    if ((b == 1 && e == 1) || (b + e >= 2 && RTEST(ruby_verbose))) {
		parser_warn(node, "range literal in condition");
	    }
	}
	break;

      case NODE_DSYM:
	if (!method_op) parser_warning(node, "literal in condition");
	break;

      case NODE_LIT:
	if (RB_TYPE_P(node->nd_lit, T_REGEXP)) {
	    if (!method_op)
		warn_unless_e_option(parser, node, "regex literal in condition");
	    nd_set_type(node, NODE_MATCH);
	}
	else {
	    if (!method_op)
		parser_warning(node, "literal in condition");
	}
      default:
	break;
    }
    return node;
}

static NODE*
cond_gen(struct parser_params *parser, NODE *node, int method_op)
{
    if (node == 0) return 0;
    return cond0(parser, node, method_op);
}

static NODE*
new_if_gen(struct parser_params *parser, NODE *cc, NODE *left, NODE *right)
{
    if (!cc) return right;
    cc = cond0(parser, cc, FALSE);
    return newline_node(NEW_IF(cc, left, right));
}

static NODE*
logop_gen(struct parser_params *parser, enum node_type type, NODE *left, NODE *right)
{
    value_expr(left);
    if (left && (enum node_type)nd_type(left) == type) {
	NODE *node = left, *second;
	while ((second = node->nd_2nd) != 0 && (enum node_type)nd_type(second) == type) {
	    node = second;
	}
	node->nd_2nd = NEW_NODE(type, second, right, 0);
	return left;
    }
    return NEW_NODE(type, left, right, 0);
}

static void
no_blockarg(struct parser_params *parser, NODE *node)
{
    if (node && nd_type(node) == NODE_BLOCK_PASS) {
	compile_error(PARSER_ARG "block argument should not be given");
    }
}

static NODE *
ret_args_gen(struct parser_params *parser, NODE *node)
{
    if (node) {
	no_blockarg(parser, node);
	if (nd_type(node) == NODE_ARRAY) {
	    if (node->nd_next == 0) {
		node = node->nd_head;
	    }
	    else {
		nd_set_type(node, NODE_VALUES);
	    }
	}
    }
    return node;
}

static NODE *
new_yield_gen(struct parser_params *parser, NODE *node)
{
    if (node) no_blockarg(parser, node);

    return NEW_YIELD(node);
}

static VALUE
negate_lit(VALUE lit)
{
    int type = TYPE(lit);
    switch (type) {
      case T_FIXNUM:
	lit = LONG2FIX(-FIX2LONG(lit));
	break;
      case T_BIGNUM:
	BIGNUM_NEGATE(lit);
	lit = rb_big_norm(lit);
	break;
      case T_RATIONAL:
	RRATIONAL_SET_NUM(lit, negate_lit(RRATIONAL(lit)->num));
	break;
      case T_COMPLEX:
	RCOMPLEX_SET_REAL(lit, negate_lit(RCOMPLEX(lit)->real));
	RCOMPLEX_SET_IMAG(lit, negate_lit(RCOMPLEX(lit)->imag));
	break;
      case T_FLOAT:
#if USE_FLONUM
	if (FLONUM_P(lit)) {
	    lit = DBL2NUM(-RFLOAT_VALUE(lit));
	    break;
	}
#endif
	RFLOAT(lit)->float_value = -RFLOAT_VALUE(lit);
	break;
      default:
	rb_bug("unknown literal type (%d) passed to negate_lit", type);
	break;
    }
    return lit;
}

static NODE *
arg_blk_pass(NODE *node1, NODE *node2)
{
    if (node2) {
	node2->nd_head = node1;
	return node2;
    }
    return node1;
}


static NODE*
new_args_gen(struct parser_params *parser, NODE *m, NODE *o, ID r, NODE *p, NODE *tail)
{
    int saved_line = ruby_sourceline;
    struct rb_args_info *args = tail->nd_ainfo;

    args->pre_args_num   = m ? rb_long2int(m->nd_plen) : 0;
    args->pre_init       = m ? m->nd_next : 0;

    args->post_args_num  = p ? rb_long2int(p->nd_plen) : 0;
    args->post_init      = p ? p->nd_next : 0;
    args->first_post_arg = p ? p->nd_pid : 0;

    args->rest_arg       = r;

    args->opt_args       = o;

    ruby_sourceline = saved_line;

    return tail;
}

static NODE*
new_args_tail_gen(struct parser_params *parser, NODE *k, ID kr, ID b)
{
    int saved_line = ruby_sourceline;
    struct rb_args_info *args;
    NODE *node;

    args = ZALLOC(struct rb_args_info);
    node = NEW_NODE(NODE_ARGS, 0, 0, args);

    args->block_arg      = b;
    args->kw_args        = k;

    if (k) {
	/*
	 * def foo(k1: 1, kr1:, k2: 2, **krest, &b)
	 * variable order: k1, kr1, k2, &b, internal_id, krest
	 * #=> <reorder>
	 * variable order: kr1, k1, k2, internal_id, krest, &b
	 */
	ID kw_bits;
	NODE *kwn = k;
	struct vtable *required_kw_vars = vtable_alloc(NULL);
	struct vtable *kw_vars = vtable_alloc(NULL);
	int i;

	while (kwn) {
	    NODE *val_node = kwn->nd_body->nd_value;
	    ID vid = kwn->nd_body->nd_vid;

	    if (val_node == (NODE *)-1) {
		vtable_add(required_kw_vars, vid);
	    }
	    else {
		vtable_add(kw_vars, vid);
	    }

	    kwn = kwn->nd_next;
	}

	kw_bits = internal_id();
	if (kr && is_junk_id(kr)) vtable_pop(lvtbl->args, 1);
	vtable_pop(lvtbl->args, vtable_size(required_kw_vars) + vtable_size(kw_vars) + (b != 0));

	for (i=0; i<vtable_size(required_kw_vars); i++) arg_var(required_kw_vars->tbl[i]);
	for (i=0; i<vtable_size(kw_vars); i++) arg_var(kw_vars->tbl[i]);
	vtable_free(required_kw_vars);
	vtable_free(kw_vars);

	arg_var(kw_bits);
	if (kr) arg_var(kr);
	if (b) arg_var(b);

	args->kw_rest_arg = NEW_DVAR(kr);
	args->kw_rest_arg->nd_cflag = kw_bits;
    }
    else if (kr) {
	if (b) vtable_pop(lvtbl->args, 1); /* reorder */
	arg_var(kr);
	if (b) arg_var(b);
	args->kw_rest_arg = NEW_DVAR(kr);
    }

    ruby_sourceline = saved_line;
    return node;
}

static NODE*
dsym_node_gen(struct parser_params *parser, NODE *node)
{
    VALUE lit;

    if (!node) {
	return NEW_LIT(ID2SYM(idNULL));
    }

    switch (nd_type(node)) {
      case NODE_DSTR:
	nd_set_type(node, NODE_DSYM);
	break;
      case NODE_STR:
	lit = node->nd_lit;
	node->nd_lit = ID2SYM(rb_intern_str(lit));
	nd_set_type(node, NODE_LIT);
	break;
      default:
	node = NEW_NODE(NODE_DSYM, Qnil, 1, NEW_LIST(node));
	break;
    }
    return node;
}

static int
append_literal_keys(st_data_t k, st_data_t v, st_data_t h)
{
    NODE *node = (NODE *)v;
    NODE **result = (NODE **)h;
    node->nd_alen = 2;
    node->nd_next->nd_end = node->nd_next;
    node->nd_next->nd_next = 0;
    if (*result)
	list_concat(*result, node);
    else
	*result = node;
    return ST_CONTINUE;
}

static NODE *
remove_duplicate_keys(struct parser_params *parser, NODE *hash)
{
    st_table *literal_keys = st_init_numtable_with_size(hash->nd_alen / 2);
    NODE *result = 0;
    while (hash && hash->nd_head && hash->nd_next) {
	NODE *head = hash->nd_head;
	NODE *value = hash->nd_next;
	NODE *next = value->nd_next;
	VALUE key = (VALUE)head;
	st_data_t data;
	if (nd_type(head) == NODE_LIT &&
	    st_lookup(literal_keys, (key = head->nd_lit), &data)) {
	    rb_compile_warn(ruby_sourcefile, nd_line((NODE *)data),
			    "key %+"PRIsVALUE" is duplicated and overwritten on line %d",
			    head->nd_lit, nd_line(head));
	    head = ((NODE *)data)->nd_next;
	    head->nd_head = block_append(head->nd_head, value->nd_head);
	}
	else {
	    st_insert(literal_keys, (st_data_t)key, (st_data_t)hash);
	}
	hash = next;
    }
    st_foreach(literal_keys, append_literal_keys, (st_data_t)&result);
    st_free_table(literal_keys);
    if (hash) {
	if (!result) result = hash;
	else list_concat(result, hash);
    }
    return result;
}

static NODE *
new_hash_gen(struct parser_params *parser, NODE *hash)
{
    if (hash) hash = remove_duplicate_keys(parser, hash);
    return NEW_HASH(hash);
}
#endif /* !RIPPER */

#ifndef RIPPER
static NODE *
new_op_assign_gen(struct parser_params *parser, NODE *lhs, ID op, NODE *rhs)
{
    NODE *asgn;

    if (lhs) {
	ID vid = lhs->nd_vid;
	if (op == tOROP) {
	    lhs->nd_value = rhs;
	    asgn = NEW_OP_ASGN_OR(gettable(vid), lhs);
	    if (is_notop_id(vid)) {
		switch (id_type(vid)) {
		  case ID_GLOBAL:
		  case ID_INSTANCE:
		  case ID_CLASS:
		    asgn->nd_aid = vid;
		}
	    }
	}
	else if (op == tANDOP) {
	    lhs->nd_value = rhs;
	    asgn = NEW_OP_ASGN_AND(gettable(vid), lhs);
	}
	else {
	    asgn = lhs;
	    asgn->nd_value = NEW_CALL(gettable(vid), op, NEW_LIST(rhs));
	}
    }
    else {
	asgn = NEW_BEGIN(0);
    }
    return asgn;
}

static NODE *
new_attr_op_assign_gen(struct parser_params *parser, NODE *lhs,
		       ID atype, ID attr, ID op, NODE *rhs)
{
    NODE *asgn;

    if (op == tOROP) {
	op = 0;
    }
    else if (op == tANDOP) {
	op = 1;
    }
    asgn = NEW_OP_ASGN2(lhs, CALL_Q_P(atype), attr, op, rhs);
    fixpos(asgn, lhs);
    return asgn;
}

static NODE *
new_const_op_assign_gen(struct parser_params *parser, NODE *lhs, ID op, NODE *rhs)
{
    NODE *asgn;

    if (op == tOROP) {
	op = 0;
    }
    else if (op == tANDOP) {
	op = 1;
    }
    if (lhs) {
	asgn = NEW_OP_CDECL(lhs, op, rhs);
    }
    else {
	asgn = NEW_BEGIN(0);
    }
    fixpos(asgn, lhs);
    return asgn;
}

static NODE *
const_decl_gen(struct parser_params *parser, NODE *path)
{
    if (in_def || in_single) {
	yyerror("dynamic constant assignment");
    }
    return NEW_CDECL(0, 0, (path));
}
#else
static VALUE
new_op_assign_gen(struct parser_params *parser, VALUE lhs, VALUE op, VALUE rhs)
{
    return dispatch3(opassign, lhs, op, rhs);
}

static VALUE
new_attr_op_assign_gen(struct parser_params *parser, VALUE lhs, VALUE type, VALUE attr, VALUE op, VALUE rhs)
{
    VALUE recv = dispatch3(field, lhs, type, attr);
    return dispatch3(opassign, recv, op, rhs);
}

static VALUE
const_decl_gen(struct parser_params *parser, VALUE path)
{
    if (in_def || in_single) {
	assign_error(path);
    }
    return path;
}

static VALUE
assign_error_gen(struct parser_params *parser, VALUE a)
{
    a = dispatch1(assign_error, a);
    ripper_error();
    return a;
}
#endif

static void
warn_unused_var(struct parser_params *parser, struct local_vars *local)
{
    int i, cnt;
    ID *v, *u;

    if (!local->used) return;
    v = local->vars->tbl;
    u = local->used->tbl;
    cnt = local->used->pos;
    if (cnt != local->vars->pos) {
	rb_bug("local->used->pos != local->vars->pos");
    }
    for (i = 0; i < cnt; ++i) {
	if (!v[i] || (u[i] & LVAR_USED)) continue;
	if (is_private_local_id(v[i])) continue;
	rb_warn1L((int)u[i], "assigned but unused variable - %"PRIsWARN, rb_id2str(v[i]));
    }
}

static void
local_push_gen(struct parser_params *parser, int inherit_dvars)
{
    struct local_vars *local;

    local = ALLOC(struct local_vars);
    local->prev = lvtbl;
    local->args = vtable_alloc(0);
    local->vars = vtable_alloc(inherit_dvars ? DVARS_INHERIT : DVARS_TOPSCOPE);
    local->used = !(inherit_dvars &&
		    (ifndef_ripper(compile_for_eval || e_option_supplied(parser))+0)) &&
	RTEST(ruby_verbose) ? vtable_alloc(0) : 0;
# if WARN_PAST_SCOPE
    local->past = 0;
# endif
    local->cmdargs = cmdarg_stack;
    CMDARG_SET(0);
    lvtbl = local;
}

static void
local_pop_gen(struct parser_params *parser)
{
    struct local_vars *local = lvtbl->prev;
    if (lvtbl->used) {
	warn_unused_var(parser, lvtbl);
	vtable_free(lvtbl->used);
    }
# if WARN_PAST_SCOPE
    while (lvtbl->past) {
	struct vtable *past = lvtbl->past;
	lvtbl->past = past->prev;
	vtable_free(past);
    }
# endif
    vtable_free(lvtbl->args);
    vtable_free(lvtbl->vars);
    CMDARG_SET(lvtbl->cmdargs);
    xfree(lvtbl);
    lvtbl = local;
}

#ifndef RIPPER
static ID*
local_tbl_gen(struct parser_params *parser)
{
    int cnt_args = vtable_size(lvtbl->args);
    int cnt_vars = vtable_size(lvtbl->vars);
    int cnt = cnt_args + cnt_vars;
    int i, j;
    ID *buf;

    if (cnt <= 0) return 0;
    buf = ALLOC_N(ID, cnt + 1);
    MEMCPY(buf+1, lvtbl->args->tbl, ID, cnt_args);
    /* remove IDs duplicated to warn shadowing */
    for (i = 0, j = cnt_args+1; i < cnt_vars; ++i) {
	ID id = lvtbl->vars->tbl[i];
	if (!vtable_included(lvtbl->args, id)) {
	    buf[j++] = id;
	}
    }
    if (--j < cnt) REALLOC_N(buf, ID, (cnt = j) + 1);
    buf[0] = cnt;
    return buf;
}
#endif

static void
arg_var_gen(struct parser_params *parser, ID id)
{
    vtable_add(lvtbl->args, id);
}

static void
local_var_gen(struct parser_params *parser, ID id)
{
    vtable_add(lvtbl->vars, id);
    if (lvtbl->used) {
	vtable_add(lvtbl->used, (ID)ruby_sourceline);
    }
}

static int
local_id_gen(struct parser_params *parser, ID id)
{
    struct vtable *vars, *args, *used;

    vars = lvtbl->vars;
    args = lvtbl->args;
    used = lvtbl->used;

    while (vars && POINTER_P(vars->prev)) {
	vars = vars->prev;
	args = args->prev;
	if (used) used = used->prev;
    }

    if (vars && vars->prev == DVARS_INHERIT) {
	return rb_local_defined(id, parser->base_block);
    }
    else if (vtable_included(args, id)) {
	return 1;
    }
    else {
	int i = vtable_included(vars, id);
	if (i && used) used->tbl[i-1] |= LVAR_USED;
	return i != 0;
    }
}

static const struct vtable *
dyna_push_gen(struct parser_params *parser)
{
    lvtbl->args = vtable_alloc(lvtbl->args);
    lvtbl->vars = vtable_alloc(lvtbl->vars);
    if (lvtbl->used) {
	lvtbl->used = vtable_alloc(lvtbl->used);
    }
    return lvtbl->args;
}

static void
dyna_pop_vtable(struct parser_params *parser, struct vtable **vtblp)
{
    struct vtable *tmp = *vtblp;
    *vtblp = tmp->prev;
# if WARN_PAST_SCOPE
    if (parser->past_scope_enabled) {
	tmp->prev = lvtbl->past;
	lvtbl->past = tmp;
	return;
    }
# endif
    vtable_free(tmp);
}

static void
dyna_pop_1(struct parser_params *parser)
{
    struct vtable *tmp;

    if ((tmp = lvtbl->used) != 0) {
	warn_unused_var(parser, lvtbl);
	lvtbl->used = lvtbl->used->prev;
	vtable_free(tmp);
    }
    dyna_pop_vtable(parser, &lvtbl->args);
    dyna_pop_vtable(parser, &lvtbl->vars);
}

static void
dyna_pop_gen(struct parser_params *parser, const struct vtable *lvargs)
{
    while (lvtbl->args != lvargs) {
	dyna_pop_1(parser);
	if (!lvtbl->args) {
	    struct local_vars *local = lvtbl->prev;
	    xfree(lvtbl);
	    lvtbl = local;
	}
    }
    dyna_pop_1(parser);
}

static int
dyna_in_block_gen(struct parser_params *parser)
{
    return POINTER_P(lvtbl->vars) && lvtbl->vars->prev != DVARS_TOPSCOPE;
}

static int
dvar_defined_gen(struct parser_params *parser, ID id, int get)
{
    struct vtable *vars, *args, *used;
    int i;

    args = lvtbl->args;
    vars = lvtbl->vars;
    used = lvtbl->used;

    while (POINTER_P(vars)) {
	if (vtable_included(args, id)) {
	    return 1;
	}
	if ((i = vtable_included(vars, id)) != 0) {
	    if (used) used->tbl[i-1] |= LVAR_USED;
	    return 1;
	}
	args = args->prev;
	vars = vars->prev;
	if (get) used = 0;
	if (used) used = used->prev;
    }

    if (vars == DVARS_INHERIT) {
        return rb_dvar_defined(id, parser->base_block);
    }

    return 0;
}

static int
dvar_curr_gen(struct parser_params *parser, ID id)
{
    return (vtable_included(lvtbl->args, id) ||
	    vtable_included(lvtbl->vars, id));
}

static void
reg_fragment_enc_error(struct parser_params* parser, VALUE str, int c)
{
    compile_error(PARSER_ARG
        "regexp encoding option '%c' differs from source encoding '%s'",
        c, rb_enc_name(rb_enc_get(str)));
}

#ifndef RIPPER
int
rb_reg_fragment_setenc(struct parser_params* parser, VALUE str, int options)
{
    int c = RE_OPTION_ENCODING_IDX(options);

    if (c) {
	int opt, idx;
	rb_char_to_option_kcode(c, &opt, &idx);
	if (idx != ENCODING_GET(str) &&
	    rb_enc_str_coderange(str) != ENC_CODERANGE_7BIT) {
            goto error;
	}
	ENCODING_SET(str, idx);
    }
    else if (RE_OPTION_ENCODING_NONE(options)) {
        if (!ENCODING_IS_ASCII8BIT(str) &&
            rb_enc_str_coderange(str) != ENC_CODERANGE_7BIT) {
            c = 'n';
            goto error;
        }
	rb_enc_associate(str, rb_ascii8bit_encoding());
    }
    else if (current_enc == rb_usascii_encoding()) {
	if (rb_enc_str_coderange(str) != ENC_CODERANGE_7BIT) {
	    /* raise in re.c */
	    rb_enc_associate(str, rb_usascii_encoding());
	}
	else {
	    rb_enc_associate(str, rb_ascii8bit_encoding());
	}
    }
    return 0;

  error:
    return c;
}

static void
reg_fragment_setenc_gen(struct parser_params* parser, VALUE str, int options)
{
    int c = rb_reg_fragment_setenc(parser, str, options);
    if (c) reg_fragment_enc_error(parser, str, c);
}

static int
reg_fragment_check_gen(struct parser_params* parser, VALUE str, int options)
{
    VALUE err;
    reg_fragment_setenc(str, options);
    err = rb_reg_check_preprocess(str);
    if (err != Qnil) {
        err = rb_obj_as_string(err);
        compile_error(PARSER_ARG "%"PRIsVALUE, err);
	return 0;
    }
    return 1;
}

typedef struct {
    struct parser_params* parser;
    rb_encoding *enc;
    NODE *succ_block;
} reg_named_capture_assign_t;

static int
reg_named_capture_assign_iter(const OnigUChar *name, const OnigUChar *name_end,
          int back_num, int *back_refs, OnigRegex regex, void *arg0)
{
    reg_named_capture_assign_t *arg = (reg_named_capture_assign_t*)arg0;
    struct parser_params* parser = arg->parser;
    rb_encoding *enc = arg->enc;
    long len = name_end - name;
    const char *s = (const char *)name;
    ID var;
    NODE *node, *succ;

    if (!len || (*name != '_' && ISASCII(*name) && !rb_enc_islower(*name, enc)) ||
	(len < MAX_WORD_LENGTH && rb_reserved_word(s, (int)len)) ||
	!rb_enc_symname2_p(s, len, enc)) {
        return ST_CONTINUE;
    }
    var = intern_cstr(s, len, enc);
    node = node_assign(assignable(var, 0), NEW_LIT(ID2SYM(var)));
    succ = arg->succ_block;
    if (!succ) succ = NEW_BEGIN(0);
    succ = block_append(succ, node);
    arg->succ_block = succ;
    return ST_CONTINUE;
}

static NODE *
reg_named_capture_assign_gen(struct parser_params* parser, VALUE regexp)
{
    reg_named_capture_assign_t arg;

    arg.parser = parser;
    arg.enc = rb_enc_get(regexp);
    arg.succ_block = 0;
    onig_foreach_name(RREGEXP_PTR(regexp), reg_named_capture_assign_iter, &arg);

    if (!arg.succ_block) return 0;
    return arg.succ_block->nd_next;
}

static VALUE
parser_reg_compile(struct parser_params* parser, VALUE str, int options)
{
    reg_fragment_setenc(str, options);
    return rb_parser_reg_compile(parser, str, options);
}

VALUE
rb_parser_reg_compile(struct parser_params* parser, VALUE str, int options)
{
    return rb_reg_compile(str, options & RE_OPTION_MASK, ruby_sourcefile, ruby_sourceline);
}

static VALUE
reg_compile_gen(struct parser_params* parser, VALUE str, int options)
{
    VALUE re;
    VALUE err;

    err = rb_errinfo();
    re = parser_reg_compile(parser, str, options);
    if (NIL_P(re)) {
	VALUE m = rb_attr_get(rb_errinfo(), idMesg);
	rb_set_errinfo(err);
	compile_error(PARSER_ARG "%"PRIsVALUE, m);
	return Qnil;
    }
    return re;
}
#else
static VALUE
parser_reg_compile(struct parser_params* parser, VALUE str, int options, VALUE *errmsg)
{
    VALUE err = rb_errinfo();
    VALUE re;
    int c = rb_reg_fragment_setenc(parser, str, options);
    if (c) reg_fragment_enc_error(parser, str, c);
    re = rb_parser_reg_compile(parser, str, options);
    if (NIL_P(re)) {
	*errmsg = rb_attr_get(rb_errinfo(), idMesg);
	rb_set_errinfo(err);
    }
    return re;
}
#endif

#ifndef RIPPER
NODE*
rb_parser_append_print(VALUE vparser, NODE *node)
{
    NODE *prelude = 0;
    NODE *scope = node;
    struct parser_params *parser;

    if (!node) return node;

    TypedData_Get_Struct(vparser, struct parser_params, &parser_data_type, parser);

    node = node->nd_body;

    if (nd_type(node) == NODE_PRELUDE) {
	prelude = node;
	node = node->nd_body;
    }

    node = block_append(node,
			NEW_FCALL(rb_intern("print"),
				  NEW_ARRAY(NEW_GVAR(idLASTLINE))));
    if (prelude) {
	prelude->nd_body = node;
	scope->nd_body = prelude;
    }
    else {
	scope->nd_body = node;
    }

    return scope;
}

NODE *
rb_parser_while_loop(VALUE vparser, NODE *node, int chop, int split)
{
    NODE *prelude = 0;
    NODE *scope = node;
    struct parser_params *parser;

    if (!node) return node;

    TypedData_Get_Struct(vparser, struct parser_params, &parser_data_type, parser);

    node = node->nd_body;

    if (nd_type(node) == NODE_PRELUDE) {
	prelude = node;
	node = node->nd_body;
    }
    if (split) {
	node = block_append(NEW_GASGN(rb_intern("$F"),
				      NEW_CALL(NEW_GVAR(idLASTLINE),
					       rb_intern("split"), 0)),
			    node);
    }
    if (chop) {
	node = block_append(NEW_CALL(NEW_GVAR(idLASTLINE),
				     rb_intern("chop!"), 0), node);
    }

    node = NEW_OPT_N(node);

    if (prelude) {
	prelude->nd_body = node;
	scope->nd_body = prelude;
    }
    else {
	scope->nd_body = node;
    }

    return scope;
}

void
rb_init_parse(void)
{
    /* just to suppress unused-function warnings */
    (void)nodetype;
    (void)nodeline;
}
#endif /* !RIPPER */

static ID
internal_id_gen(struct parser_params *parser)
{
    ID id = (ID)vtable_size(lvtbl->args) + (ID)vtable_size(lvtbl->vars);
    id += ((tLAST_TOKEN - ID_INTERNAL) >> ID_SCOPE_SHIFT) + 1;
    return ID_STATIC_SYM | ID_INTERNAL | (id << ID_SCOPE_SHIFT);
}

static void
parser_initialize(struct parser_params *parser)
{
    /* note: we rely on TypedData_Make_Struct to set most fields to 0 */
    command_start = TRUE;
    ruby_sourcefile_string = Qnil;
#ifdef RIPPER
    parser->delayed = Qnil;
    parser->result = Qnil;
    parser->parsing_thread = Qnil;
#else
    parser->error_buffer = Qfalse;
#endif
    parser->debug_buffer = Qnil;
    parser->enc = rb_utf8_encoding();
}

#ifdef RIPPER
#define parser_mark ripper_parser_mark
#define parser_free ripper_parser_free
#endif

static void
parser_mark(void *ptr)
{
    struct parser_params *parser = (struct parser_params*)ptr;

    rb_gc_mark((VALUE)lex_strterm);
    rb_gc_mark(lex_input);
    rb_gc_mark(lex_lastline);
    rb_gc_mark(lex_nextline);
    rb_gc_mark(ruby_sourcefile_string);
#ifndef RIPPER
    rb_gc_mark((VALUE)ruby_eval_tree_begin);
    rb_gc_mark((VALUE)ruby_eval_tree);
    rb_gc_mark(ruby_debug_lines);
    rb_gc_mark(parser->compile_option);
    rb_gc_mark(parser->error_buffer);
#else
    rb_gc_mark(parser->delayed);
    rb_gc_mark(parser->value);
    rb_gc_mark(parser->result);
    rb_gc_mark(parser->parsing_thread);
#endif
    rb_gc_mark(parser->debug_buffer);
#ifdef YYMALLOC
    rb_gc_mark((VALUE)parser->heap);
#endif
}

static void
parser_free(void *ptr)
{
    struct parser_params *parser = (struct parser_params*)ptr;
    struct local_vars *local, *prev;

    if (tokenbuf) {
        xfree(tokenbuf);
    }
    for (local = lvtbl; local; local = prev) {
	if (local->vars) xfree(local->vars);
	prev = local->prev;
	xfree(local);
    }
    {
	token_info *ptinfo;
	while ((ptinfo = parser->token_info) != 0) {
	    parser->token_info = ptinfo->next;
	    xfree(ptinfo);
	}
    }
    xfree(ptr);
}

static size_t
parser_memsize(const void *ptr)
{
    struct parser_params *parser = (struct parser_params*)ptr;
    struct local_vars *local;
    size_t size = sizeof(*parser);

    size += toksiz;
    for (local = lvtbl; local; local = local->prev) {
	size += sizeof(*local);
	if (local->vars) size += local->vars->capa * sizeof(ID);
    }
    return size;
}

static const rb_data_type_t parser_data_type = {
#ifndef RIPPER
    "parser",
#else
    "ripper",
#endif
    {
	parser_mark,
	parser_free,
	parser_memsize,
    },
    0, 0, RUBY_TYPED_FREE_IMMEDIATELY
};

#ifndef RIPPER
#undef rb_reserved_word

const struct kwtable *
rb_reserved_word(const char *str, unsigned int len)
{
    return reserved_word(str, len);
}

VALUE
rb_parser_new(void)
{
    struct parser_params *p;
    VALUE parser = TypedData_Make_Struct(0, struct parser_params,
					 &parser_data_type, p);
    parser_initialize(p);
    return parser;
}

VALUE
rb_parser_set_context(VALUE vparser, const struct rb_block *base, int main)
{
    struct parser_params *parser;

    TypedData_Get_Struct(vparser, struct parser_params, &parser_data_type, parser);
    parser->error_buffer = main ? Qfalse : Qnil;
    parser->base_block = base;
    in_main = main;
    return vparser;
}
#endif

#ifdef RIPPER
#define rb_parser_end_seen_p ripper_parser_end_seen_p
#define rb_parser_encoding ripper_parser_encoding
#define rb_parser_get_yydebug ripper_parser_get_yydebug
#define rb_parser_set_yydebug ripper_parser_set_yydebug
static VALUE ripper_parser_end_seen_p(VALUE vparser);
static VALUE ripper_parser_encoding(VALUE vparser);
static VALUE ripper_parser_get_yydebug(VALUE self);
static VALUE ripper_parser_set_yydebug(VALUE self, VALUE flag);

/*
 *  call-seq:
 *    ripper#error?   -> Boolean
 *
 *  Return true if parsed source has errors.
 */
static VALUE
ripper_error_p(VALUE vparser)
{
    struct parser_params *parser;

    TypedData_Get_Struct(vparser, struct parser_params, &parser_data_type, parser);
    return parser->error_p ? Qtrue : Qfalse;
}
#endif

/*
 *  call-seq:
 *    ripper#end_seen?   -> Boolean
 *
 *  Return true if parsed source ended by +\_\_END\_\_+.
 */
VALUE
rb_parser_end_seen_p(VALUE vparser)
{
    struct parser_params *parser;

    TypedData_Get_Struct(vparser, struct parser_params, &parser_data_type, parser);
    return ruby__end__seen ? Qtrue : Qfalse;
}

/*
 *  call-seq:
 *    ripper#encoding   -> encoding
 *
 *  Return encoding of the source.
 */
VALUE
rb_parser_encoding(VALUE vparser)
{
    struct parser_params *parser;

    TypedData_Get_Struct(vparser, struct parser_params, &parser_data_type, parser);
    return rb_enc_from_encoding(current_enc);
}

/*
 *  call-seq:
 *    ripper.yydebug   -> true or false
 *
 *  Get yydebug.
 */
VALUE
rb_parser_get_yydebug(VALUE self)
{
    struct parser_params *parser;

    TypedData_Get_Struct(self, struct parser_params, &parser_data_type, parser);
    return yydebug ? Qtrue : Qfalse;
}

/*
 *  call-seq:
 *    ripper.yydebug = flag
 *
 *  Set yydebug.
 */
VALUE
rb_parser_set_yydebug(VALUE self, VALUE flag)
{
    struct parser_params *parser;

    TypedData_Get_Struct(self, struct parser_params, &parser_data_type, parser);
    yydebug = RTEST(flag);
    return flag;
}

#ifndef RIPPER
#ifdef YYMALLOC
#define HEAPCNT(n, size) ((n) * (size) / sizeof(YYSTYPE))
#define NEWHEAP() rb_node_newnode(NODE_ALLOCA, 0, (VALUE)parser->heap, 0)
#define ADD2HEAP(n, c, p) ((parser->heap = (n))->u1.node = (p), \
			   (n)->u3.cnt = (c), (p))

void *
rb_parser_malloc(struct parser_params *parser, size_t size)
{
    size_t cnt = HEAPCNT(1, size);
    NODE *n = NEWHEAP();
    void *ptr = xmalloc(size);

    return ADD2HEAP(n, cnt, ptr);
}

void *
rb_parser_calloc(struct parser_params *parser, size_t nelem, size_t size)
{
    size_t cnt = HEAPCNT(nelem, size);
    NODE *n = NEWHEAP();
    void *ptr = xcalloc(nelem, size);

    return ADD2HEAP(n, cnt, ptr);
}

void *
rb_parser_realloc(struct parser_params *parser, void *ptr, size_t size)
{
    NODE *n;
    size_t cnt = HEAPCNT(1, size);

    if (ptr && (n = parser->heap) != NULL) {
	do {
	    if (n->u1.node == ptr) {
		n->u1.node = ptr = xrealloc(ptr, size);
		if (n->u3.cnt) n->u3.cnt = cnt;
		return ptr;
	    }
	} while ((n = n->u2.node) != NULL);
    }
    n = NEWHEAP();
    ptr = xrealloc(ptr, size);
    return ADD2HEAP(n, cnt, ptr);
}

void
rb_parser_free(struct parser_params *parser, void *ptr)
{
    NODE **prev = &parser->heap, *n;

    while ((n = *prev) != NULL) {
	if (n->u1.node == ptr) {
	    *prev = n->u2.node;
	    rb_gc_force_recycle((VALUE)n);
	    break;
	}
	prev = &n->u2.node;
    }
    xfree(ptr);
}
#endif

void
rb_parser_printf(struct parser_params *parser, const char *fmt, ...)
{
    va_list ap;
    VALUE mesg = parser->debug_buffer;

    if (NIL_P(mesg)) parser->debug_buffer = mesg = rb_str_new(0, 0);
    va_start(ap, fmt);
    rb_str_vcatf(mesg, fmt, ap);
    va_end(ap);
    if (RSTRING_END(mesg)[-1] == '\n') {
	rb_io_write(rb_stdout, mesg);
	parser->debug_buffer = Qnil;
    }
}

static void
parser_compile_error(struct parser_params *parser, const char *fmt, ...)
{
    va_list ap;

    parser->error_p = 1;
    va_start(ap, fmt);
    parser->error_buffer =
	rb_syntax_error_append(parser->error_buffer,
			       ruby_sourcefile_string,
			       ruby_sourceline,
			       rb_long2int(lex_p - lex_pbeg),
			       current_enc, fmt, ap);
    va_end(ap);
}
#endif

#ifdef RIPPER
#ifdef RIPPER_DEBUG
extern int rb_is_pointer_to_heap(VALUE);

/* :nodoc: */
static VALUE
ripper_validate_object(VALUE self, VALUE x)
{
    if (x == Qfalse) return x;
    if (x == Qtrue) return x;
    if (x == Qnil) return x;
    if (x == Qundef)
        rb_raise(rb_eArgError, "Qundef given");
    if (FIXNUM_P(x)) return x;
    if (SYMBOL_P(x)) return x;
    if (!rb_is_pointer_to_heap(x))
        rb_raise(rb_eArgError, "invalid pointer: %p", x);
    switch (BUILTIN_TYPE(x)) {
      case T_STRING:
      case T_OBJECT:
      case T_ARRAY:
      case T_BIGNUM:
      case T_FLOAT:
      case T_COMPLEX:
      case T_RATIONAL:
        return x;
      case T_NODE:
	if (nd_type(x) != NODE_RIPPER) {
	    rb_raise(rb_eArgError, "NODE given: %p", x);
	}
	return ((NODE *)x)->nd_rval;
      default:
        rb_raise(rb_eArgError, "wrong type of ruby object: %p (%s)",
                 x, rb_obj_classname(x));
    }
    return x;
}
#endif

#define validate(x) ((x) = get_value(x))

static VALUE
ripper_dispatch0(struct parser_params *parser, ID mid)
{
    return rb_funcall(parser->value, mid, 0);
}

static VALUE
ripper_dispatch1(struct parser_params *parser, ID mid, VALUE a)
{
    validate(a);
    return rb_funcall(parser->value, mid, 1, a);
}

static VALUE
ripper_dispatch2(struct parser_params *parser, ID mid, VALUE a, VALUE b)
{
    validate(a);
    validate(b);
    return rb_funcall(parser->value, mid, 2, a, b);
}

static VALUE
ripper_dispatch3(struct parser_params *parser, ID mid, VALUE a, VALUE b, VALUE c)
{
    validate(a);
    validate(b);
    validate(c);
    return rb_funcall(parser->value, mid, 3, a, b, c);
}

static VALUE
ripper_dispatch4(struct parser_params *parser, ID mid, VALUE a, VALUE b, VALUE c, VALUE d)
{
    validate(a);
    validate(b);
    validate(c);
    validate(d);
    return rb_funcall(parser->value, mid, 4, a, b, c, d);
}

static VALUE
ripper_dispatch5(struct parser_params *parser, ID mid, VALUE a, VALUE b, VALUE c, VALUE d, VALUE e)
{
    validate(a);
    validate(b);
    validate(c);
    validate(d);
    validate(e);
    return rb_funcall(parser->value, mid, 5, a, b, c, d, e);
}

static VALUE
ripper_dispatch7(struct parser_params *parser, ID mid, VALUE a, VALUE b, VALUE c, VALUE d, VALUE e, VALUE f, VALUE g)
{
    validate(a);
    validate(b);
    validate(c);
    validate(d);
    validate(e);
    validate(f);
    validate(g);
    return rb_funcall(parser->value, mid, 7, a, b, c, d, e, f, g);
}

static const struct kw_assoc {
    ID id;
    const char *name;
} keyword_to_name[] = {
    {keyword_class,	"class"},
    {keyword_module,	"module"},
    {keyword_def,	"def"},
    {keyword_undef,	"undef"},
    {keyword_begin,	"begin"},
    {keyword_rescue,	"rescue"},
    {keyword_ensure,	"ensure"},
    {keyword_end,	"end"},
    {keyword_if,	"if"},
    {keyword_unless,	"unless"},
    {keyword_then,	"then"},
    {keyword_elsif,	"elsif"},
    {keyword_else,	"else"},
    {keyword_case,	"case"},
    {keyword_when,	"when"},
    {keyword_while,	"while"},
    {keyword_until,	"until"},
    {keyword_for,	"for"},
    {keyword_break,	"break"},
    {keyword_next,	"next"},
    {keyword_redo,	"redo"},
    {keyword_retry,	"retry"},
    {keyword_in,	"in"},
    {keyword_do,	"do"},
    {keyword_do_cond,	"do"},
    {keyword_do_block,	"do"},
    {keyword_return,	"return"},
    {keyword_yield,	"yield"},
    {keyword_super,	"super"},
    {keyword_self,	"self"},
    {keyword_nil,	"nil"},
    {keyword_true,	"true"},
    {keyword_false,	"false"},
    {keyword_and,	"and"},
    {keyword_or,	"or"},
    {keyword_not,	"not"},
    {modifier_if,	"if"},
    {modifier_unless,	"unless"},
    {modifier_while,	"while"},
    {modifier_until,	"until"},
    {modifier_rescue,	"rescue"},
    {keyword_alias,	"alias"},
    {keyword_defined,	"defined?"},
    {keyword_BEGIN,	"BEGIN"},
    {keyword_END,	"END"},
    {keyword__LINE__,	"__LINE__"},
    {keyword__FILE__,	"__FILE__"},
    {keyword__ENCODING__, "__ENCODING__"},
    {0, NULL}
};

static const char*
keyword_id_to_str(ID id)
{
    const struct kw_assoc *a;

    for (a = keyword_to_name; a->id; a++) {
        if (a->id == id)
            return a->name;
    }
    return NULL;
}

#undef ripper_id2sym
static VALUE
ripper_id2sym(ID id)
{
    const char *name;
    char buf[8];

    if (id == (ID)(signed char)id) {
        buf[0] = (char)id;
        buf[1] = '\0';
        return ID2SYM(rb_intern2(buf, 1));
    }
    if ((name = keyword_id_to_str(id))) {
        return ID2SYM(rb_intern(name));
    }
    if (!rb_id2str(id)) {
	rb_bug("cannot convert ID to string: %ld", (unsigned long)id);
    }
    return ID2SYM(id);
}

static ID
ripper_get_id(VALUE v)
{
    NODE *nd;
    if (!RB_TYPE_P(v, T_NODE)) return 0;
    nd = (NODE *)v;
    if (nd_type(nd) != NODE_RIPPER) return 0;
    return nd->nd_vid;
}

static VALUE
ripper_get_value(VALUE v)
{
    NODE *nd;
    if (v == Qundef) return Qnil;
    if (!RB_TYPE_P(v, T_NODE)) return v;
    nd = (NODE *)v;
    if (nd_type(nd) != NODE_RIPPER) return Qnil;
    return nd->nd_rval;
}

static void
ripper_error_gen(struct parser_params *parser)
{
    parser->error_p = TRUE;
}

static void
ripper_compile_error(struct parser_params *parser, const char *fmt, ...)
{
    VALUE str;
    va_list args;

    va_start(args, fmt);
    str = rb_vsprintf(fmt, args);
    va_end(args);
    rb_funcall(parser->value, rb_intern("compile_error"), 1, str);
    ripper_error_gen(parser);
}

static VALUE
ripper_lex_get_generic(struct parser_params *parser, VALUE src)
{
    VALUE line = rb_funcallv_public(src, id_gets, 0, 0);
    if (!NIL_P(line) && !RB_TYPE_P(line, T_STRING)) {
	rb_raise(rb_eTypeError,
		 "gets returned %"PRIsVALUE" (expected String or nil)",
		 rb_obj_class(line));
    }
    return line;
}

static VALUE
ripper_lex_io_get(struct parser_params *parser, VALUE src)
{
    return rb_io_gets(src);
}

static VALUE
ripper_s_allocate(VALUE klass)
{
    struct parser_params *p;
    VALUE self = TypedData_Make_Struct(klass, struct parser_params,
				       &parser_data_type, p);
    p->value = self;
    return self;
}

#define ripper_initialized_p(r) ((r)->lex.input != 0)

/*
 *  call-seq:
 *    Ripper.new(src, filename="(ripper)", lineno=1) -> ripper
 *
 *  Create a new Ripper object.
 *  _src_ must be a String, an IO, or an Object which has #gets method.
 *
 *  This method does not starts parsing.
 *  See also Ripper#parse and Ripper.parse.
 */
static VALUE
ripper_initialize(int argc, VALUE *argv, VALUE self)
{
    struct parser_params *parser;
    VALUE src, fname, lineno;

    TypedData_Get_Struct(self, struct parser_params, &parser_data_type, parser);
    rb_scan_args(argc, argv, "12", &src, &fname, &lineno);
    if (RB_TYPE_P(src, T_FILE)) {
        lex_gets = ripper_lex_io_get;
    }
    else if (rb_respond_to(src, id_gets)) {
        lex_gets = ripper_lex_get_generic;
    }
    else {
        StringValue(src);
        lex_gets = lex_get_str;
    }
    lex_input = src;
    parser->eofp = 0;
    if (NIL_P(fname)) {
        fname = STR_NEW2("(ripper)");
	OBJ_FREEZE(fname);
    }
    else {
	StringValueCStr(fname);
	fname = rb_str_new_frozen(fname);
    }
    parser_initialize(parser);

    ruby_sourcefile_string = fname;
    ruby_sourcefile = RSTRING_PTR(fname);
    ruby_sourceline = NIL_P(lineno) ? 0 : NUM2INT(lineno) - 1;

    return Qnil;
}

struct ripper_args {
    struct parser_params *parser;
    int argc;
    VALUE *argv;
};

static VALUE
ripper_parse0(VALUE parser_v)
{
    struct parser_params *parser;

    TypedData_Get_Struct(parser_v, struct parser_params, &parser_data_type, parser);
    parser_prepare(parser);
    ripper_yyparse((void*)parser);
    return parser->result;
}

static VALUE
ripper_ensure(VALUE parser_v)
{
    struct parser_params *parser;

    TypedData_Get_Struct(parser_v, struct parser_params, &parser_data_type, parser);
    parser->parsing_thread = Qnil;
    return Qnil;
}

/*
 *  call-seq:
 *    ripper#parse
 *
 *  Start parsing and returns the value of the root action.
 */
static VALUE
ripper_parse(VALUE self)
{
    struct parser_params *parser;

    TypedData_Get_Struct(self, struct parser_params, &parser_data_type, parser);
    if (!ripper_initialized_p(parser)) {
        rb_raise(rb_eArgError, "method called for uninitialized object");
    }
    if (!NIL_P(parser->parsing_thread)) {
        if (parser->parsing_thread == rb_thread_current())
            rb_raise(rb_eArgError, "Ripper#parse is not reentrant");
        else
            rb_raise(rb_eArgError, "Ripper#parse is not multithread-safe");
    }
    parser->parsing_thread = rb_thread_current();
    rb_ensure(ripper_parse0, self, ripper_ensure, self);

    return parser->result;
}

/*
 *  call-seq:
 *    ripper#column   -> Integer
 *
 *  Return column number of current parsing line.
 *  This number starts from 0.
 */
static VALUE
ripper_column(VALUE self)
{
    struct parser_params *parser;
    long col;

    TypedData_Get_Struct(self, struct parser_params, &parser_data_type, parser);
    if (!ripper_initialized_p(parser)) {
        rb_raise(rb_eArgError, "method called for uninitialized object");
    }
    if (NIL_P(parser->parsing_thread)) return Qnil;
    col = parser->tokp - lex_pbeg;
    return LONG2NUM(col);
}

/*
 *  call-seq:
 *    ripper#filename   -> String
 *
 *  Return current parsing filename.
 */
static VALUE
ripper_filename(VALUE self)
{
    struct parser_params *parser;

    TypedData_Get_Struct(self, struct parser_params, &parser_data_type, parser);
    if (!ripper_initialized_p(parser)) {
        rb_raise(rb_eArgError, "method called for uninitialized object");
    }
    return ruby_sourcefile_string;
}

/*
 *  call-seq:
 *    ripper#lineno   -> Integer
 *
 *  Return line number of current parsing line.
 *  This number starts from 1.
 */
static VALUE
ripper_lineno(VALUE self)
{
    struct parser_params *parser;

    TypedData_Get_Struct(self, struct parser_params, &parser_data_type, parser);
    if (!ripper_initialized_p(parser)) {
        rb_raise(rb_eArgError, "method called for uninitialized object");
    }
    if (NIL_P(parser->parsing_thread)) return Qnil;
    return INT2NUM(ruby_sourceline);
}

#ifdef RIPPER_DEBUG
/* :nodoc: */
static VALUE
ripper_assert_Qundef(VALUE self, VALUE obj, VALUE msg)
{
    StringValue(msg);
    if (obj == Qundef) {
        rb_raise(rb_eArgError, "%"PRIsVALUE, msg);
    }
    return Qnil;
}

/* :nodoc: */
static VALUE
ripper_value(VALUE self, VALUE obj)
{
    return ULONG2NUM(obj);
}
#endif


void
Init_ripper(void)
{
    ripper_init_eventids1();
    ripper_init_eventids2();
    id_warn = rb_intern_const("warn");
    id_warning = rb_intern_const("warning");
    id_gets = rb_intern_const("gets");

    InitVM(ripper);
}

void
InitVM_ripper(void)
{
    VALUE Ripper;

    Ripper = rb_define_class("Ripper", rb_cObject);
    /* version of Ripper */
    rb_define_const(Ripper, "Version", rb_usascii_str_new2(RIPPER_VERSION));
    rb_define_alloc_func(Ripper, ripper_s_allocate);
    rb_define_method(Ripper, "initialize", ripper_initialize, -1);
    rb_define_method(Ripper, "parse", ripper_parse, 0);
    rb_define_method(Ripper, "column", ripper_column, 0);
    rb_define_method(Ripper, "filename", ripper_filename, 0);
    rb_define_method(Ripper, "lineno", ripper_lineno, 0);
    rb_define_method(Ripper, "end_seen?", rb_parser_end_seen_p, 0);
    rb_define_method(Ripper, "encoding", rb_parser_encoding, 0);
    rb_define_method(Ripper, "yydebug", rb_parser_get_yydebug, 0);
    rb_define_method(Ripper, "yydebug=", rb_parser_set_yydebug, 1);
    rb_define_method(Ripper, "error?", ripper_error_p, 0);
#ifdef RIPPER_DEBUG
    rb_define_method(rb_mKernel, "assert_Qundef", ripper_assert_Qundef, 2);
    rb_define_method(rb_mKernel, "rawVALUE", ripper_value, 1);
    rb_define_method(rb_mKernel, "validate_object", ripper_validate_object, 1);
#endif

    rb_define_singleton_method(Ripper, "dedent_string", parser_dedent_string, 2);
    rb_define_private_method(Ripper, "dedent_string", parser_dedent_string, 2);

    ripper_init_eventids1_table(Ripper);
    ripper_init_eventids2_table(Ripper);

# if 0
    /* Hack to let RDoc document SCRIPT_LINES__ */

    /*
     * When a Hash is assigned to +SCRIPT_LINES__+ the contents of files loaded
     * after the assignment will be added as an Array of lines with the file
     * name as the key.
     */
    rb_define_global_const("SCRIPT_LINES__", Qnil);
#endif

}
#endif /* RIPPER */
