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
#define YYDEBUG 1
#define YYERROR_VERBOSE 1
#define YYSTACK_USE_ALLOCA 0
#define YYLTYPE rb_code_location_t
#define YYLTYPE_IS_DECLARED 1

#include "ruby/internal/config.h"

#include <ctype.h>
#include <errno.h>
#include <stdio.h>

struct lex_context {
    unsigned int in_defined: 1;
    unsigned int in_kwarg: 1;
    unsigned int in_def: 1;
    unsigned int in_class: 1;
};

#include "internal.h"
#include "internal/compile.h"
#include "internal/complex.h"
#include "internal/error.h"
#include "internal/hash.h"
#include "internal/imemo.h"
#include "internal/io.h"
#include "internal/numeric.h"
#include "internal/parse.h"
#include "internal/rational.h"
#include "internal/re.h"
#include "internal/symbol.h"
#include "internal/thread.h"
#include "internal/util.h"
#include "internal/variable.h"
#include "node.h"
#include "parse.h"
#include "probes.h"
#include "regenc.h"
#include "ruby/encoding.h"
#include "ruby/regex.h"
#include "ruby/ruby.h"
#include "ruby/st.h"
#include "ruby/util.h"
#include "ruby/ractor.h"
#include "symbol.h"

#define AREF(ary, i) RARRAY_AREF(ary, i)

#ifndef WARN_PAST_SCOPE
# define WARN_PAST_SCOPE 0
#endif

#define TAB_WIDTH 8

#define yydebug (p->debug)	/* disable the global variable definition */

#define YYMALLOC(size)		rb_parser_malloc(p, (size))
#define YYREALLOC(ptr, size)	rb_parser_realloc(p, (ptr), (size))
#define YYCALLOC(nelem, size)	rb_parser_calloc(p, (nelem), (size))
#define YYFREE(ptr)		rb_parser_free(p, (ptr))
#define YYFPRINTF		rb_parser_printf
#define YYPRINT(out, tok, val)	parser_token_value_print(p, (tok), &(val))
#define YY_LOCATION_PRINT(File, loc) \
     rb_parser_printf(p, "%d.%d-%d.%d", \
		      (loc).beg_pos.lineno, (loc).beg_pos.column,\
		      (loc).end_pos.lineno, (loc).end_pos.column)
#define YYLLOC_DEFAULT(Current, Rhs, N)					\
    do									\
      if (N)								\
	{								\
	  (Current).beg_pos = YYRHSLOC(Rhs, 1).beg_pos;			\
	  (Current).end_pos = YYRHSLOC(Rhs, N).end_pos;			\
	}								\
      else								\
        {                                                               \
          (Current).beg_pos = YYRHSLOC(Rhs, 0).end_pos;                 \
          (Current).end_pos = YYRHSLOC(Rhs, 0).end_pos;                 \
        }                                                               \
    while (0)

#define RUBY_SET_YYLLOC_FROM_STRTERM_HEREDOC(Current)			\
    rb_parser_set_location_from_strterm_heredoc(p, &p->lex.strterm->u.heredoc, &(Current))
#define RUBY_SET_YYLLOC_OF_NONE(Current)					\
    rb_parser_set_location_of_none(p, &(Current))
#define RUBY_SET_YYLLOC(Current)					\
    rb_parser_set_location(p, &(Current))
#define RUBY_INIT_YYLLOC() \
    { \
	{p->ruby_sourceline, (int)(p->lex.ptok - p->lex.pbeg)}, \
	{p->ruby_sourceline, (int)(p->lex.pcur - p->lex.pbeg)}, \
    }

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
    EXPR_END_ANY  =  (EXPR_END | EXPR_ENDARG | EXPR_ENDFN),
    EXPR_NONE = 0
};
#define IS_lex_state_for(x, ls)	((x) & (ls))
#define IS_lex_state_all_for(x, ls) (((x) & (ls)) == (ls))
#define IS_lex_state(ls)	IS_lex_state_for(p->lex.state, (ls))
#define IS_lex_state_all(ls)	IS_lex_state_all_for(p->lex.state, (ls))

# define SET_LEX_STATE(ls) \
    (p->lex.state = \
     (p->debug ? \
      rb_parser_trace_lex_state(p, p->lex.state, (ls), __LINE__) : \
      (enum lex_state_e)(ls)))

typedef VALUE stack_type;

static const rb_code_location_t NULL_LOC = { {0, -1}, {0, -1} };

# define SHOW_BITSTACK(stack, name) (p->debug ? rb_parser_show_bitstack(p, stack, name, __LINE__) : (void)0)
# define BITSTACK_PUSH(stack, n) (((p->stack) = ((p->stack)<<1)|((n)&1)), SHOW_BITSTACK(p->stack, #stack"(push)"))
# define BITSTACK_POP(stack)	 (((p->stack) = (p->stack) >> 1), SHOW_BITSTACK(p->stack, #stack"(pop)"))
# define BITSTACK_SET_P(stack)	 (SHOW_BITSTACK(p->stack, #stack), (p->stack)&1)
# define BITSTACK_SET(stack, n)	 ((p->stack)=(n), SHOW_BITSTACK(p->stack, #stack"(set)"))

/* A flag to identify keyword_do_cond, "do" keyword after condition expression.
   Examples: `while ... do`, `until ... do`, and `for ... in ... do` */
#define COND_PUSH(n)	BITSTACK_PUSH(cond_stack, (n))
#define COND_POP()	BITSTACK_POP(cond_stack)
#define COND_P()	BITSTACK_SET_P(cond_stack)
#define COND_SET(n)	BITSTACK_SET(cond_stack, (n))

/* A flag to identify keyword_do_block; "do" keyword after command_call.
   Example: `foo 1, 2 do`. */
#define CMDARG_PUSH(n)	BITSTACK_PUSH(cmdarg_stack, (n))
#define CMDARG_POP()	BITSTACK_POP(cmdarg_stack)
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
# ifndef RIPPER
    struct {
	NODE *outer, *inner, *current;
    } numparam;
# endif
};

enum {
    ORDINAL_PARAM = -1,
    NO_PARAM = 0,
    NUMPARAM_MAX = 9,
};

#define NUMPARAM_ID_P(id) numparam_id_p(id)
#define NUMPARAM_ID_TO_IDX(id) (unsigned int)(((id) >> ID_SCOPE_SHIFT) - tNUMPARAM_1 + 1)
#define NUMPARAM_IDX_TO_ID(idx) TOKEN2LOCALID((tNUMPARAM_1 + (idx) - 1))
static int
numparam_id_p(ID id)
{
    if (!is_local_id(id)) return 0;
    unsigned int idx = NUMPARAM_ID_TO_IDX(id);
    return idx > 0 && idx <= NUMPARAM_MAX;
}
static void numparam_name(struct parser_params *p, ID id);

#define DVARS_INHERIT ((void*)1)
#define DVARS_TOPSCOPE NULL
#define DVARS_TERMINAL_P(tbl) ((tbl) == DVARS_INHERIT || (tbl) == DVARS_TOPSCOPE)

typedef struct token_info {
    const char *token;
    rb_code_position_t beg;
    int indent;
    int nonspc;
    struct token_info *next;
} token_info;

typedef struct rb_strterm_struct rb_strterm_t;

/*
    Structure of Lexer Buffer:

 lex.pbeg     lex.ptok     lex.pcur     lex.pend
    |            |            |            |
    |------------+------------+------------|
                 |<---------->|
                     token
*/
struct parser_params {
    rb_imemo_tmpbuf_t *heap;

    YYSTYPE *lval;

    struct {
	rb_strterm_t *strterm;
	VALUE (*gets)(struct parser_params*,VALUE);
	VALUE input;
	VALUE prevline;
	VALUE lastline;
	VALUE nextline;
	const char *pbeg;
	const char *pcur;
	const char *pend;
	const char *ptok;
	union {
	    long ptr;
	    VALUE (*call)(VALUE, int);
	} gets_;
	enum lex_state_e state;
	/* track the nest level of any parens "()[]{}" */
	int paren_nest;
	/* keep p->lex.paren_nest at the beginning of lambda "->" to detect tLAMBEG and keyword_do_LAMBDA */
	int lpar_beg;
	/* track the nest level of only braces "{}" */
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
    st_table *pvtbl;
    st_table *pktbl;
    int line_count;
    int ruby_sourceline;	/* current line no. */
    const char *ruby_sourcefile; /* current source file */
    VALUE ruby_sourcefile_string;
    rb_encoding *enc;
    token_info *token_info;
    VALUE case_labels;
    VALUE compile_option;

    VALUE debug_buffer;
    VALUE debug_output;

    ID cur_arg;

    rb_ast_t *ast;
    int node_id;

    int max_numparam;

    struct lex_context ctxt;

    unsigned int command_start:1;
    unsigned int eofp: 1;
    unsigned int ruby__end__seen: 1;
    unsigned int debug: 1;
    unsigned int has_shebang: 1;
    unsigned int token_seen: 1;
    unsigned int token_info_enabled: 1;
# if WARN_PAST_SCOPE
    unsigned int past_scope_enabled: 1;
# endif
    unsigned int error_p: 1;
    unsigned int cr_seen: 1;

#ifndef RIPPER
    /* Ruby core only */

    unsigned int do_print: 1;
    unsigned int do_loop: 1;
    unsigned int do_chomp: 1;
    unsigned int do_split: 1;

    NODE *eval_tree_begin;
    NODE *eval_tree;
    VALUE error_buffer;
    VALUE debug_lines;
    const struct rb_iseq_struct *parent_iseq;
#else
    /* Ripper only */

    struct {
	VALUE token;
	int line;
	int col;
    } delayed;

    VALUE value;
    VALUE result;
    VALUE parsing_thread;
#endif
};

#define intern_cstr(n,l,en) rb_intern3(n,l,en)

#define STR_NEW(ptr,len) rb_enc_str_new((ptr),(len),p->enc)
#define STR_NEW0() rb_enc_str_new(0,0,p->enc)
#define STR_NEW2(ptr) rb_enc_str_new((ptr),strlen(ptr),p->enc)
#define STR_NEW3(ptr,len,e,func) parser_str_new((ptr),(len),(e),(func),p->enc)
#define TOK_INTERN() intern_cstr(tok(p), toklen(p), p->enc)

static st_table *
push_pvtbl(struct parser_params *p)
{
    st_table *tbl = p->pvtbl;
    p->pvtbl = st_init_numtable();
    return tbl;
}

static void
pop_pvtbl(struct parser_params *p, st_table *tbl)
{
    st_free_table(p->pvtbl);
    p->pvtbl = tbl;
}

static st_table *
push_pktbl(struct parser_params *p)
{
    st_table *tbl = p->pktbl;
    p->pktbl = 0;
    return tbl;
}

static void
pop_pktbl(struct parser_params *p, st_table *tbl)
{
    if (p->pktbl) st_free_table(p->pktbl);
    p->pktbl = tbl;
}

static int parser_yyerror(struct parser_params*, const YYLTYPE *yylloc, const char*);
#define yyerror0(msg) parser_yyerror(p, NULL, (msg))
#define yyerror1(loc, msg) parser_yyerror(p, (loc), (msg))
#define yyerror(yylloc, p, msg) parser_yyerror(p, yylloc, msg)
#define token_flush(ptr) ((ptr)->lex.ptok = (ptr)->lex.pcur)

#ifdef RIPPER
#define compile_for_eval	(0)
#else
#define compile_for_eval	(p->parent_iseq != 0)
#endif

#define token_column		((int)(p->lex.ptok - p->lex.pbeg))

#define CALL_Q_P(q) ((q) == TOKEN2VAL(tANDDOT))
#define NODE_CALL_Q(q) (CALL_Q_P(q) ? NODE_QCALL : NODE_CALL)
#define NEW_QCALL(q,r,m,a,loc) NEW_NODE(NODE_CALL_Q(q),r,m,a,loc)

#define lambda_beginning_p() (p->lex.lpar_beg == p->lex.paren_nest)

static enum yytokentype yylex(YYSTYPE*, YYLTYPE*, struct parser_params*);

#ifndef RIPPER
static inline void
rb_discard_node(struct parser_params *p, NODE *n)
{
    rb_ast_delete_node(p->ast, n);
}
#endif

#ifdef RIPPER
static inline VALUE
add_mark_object(struct parser_params *p, VALUE obj)
{
    if (!SPECIAL_CONST_P(obj)
	&& !RB_TYPE_P(obj, T_NODE) /* Ripper jumbles NODE objects and other objects... */
    ) {
	rb_ast_add_mark_object(p->ast, obj);
    }
    return obj;
}
#else
static NODE* node_newnode_with_locals(struct parser_params *, enum node_type, VALUE, VALUE, const rb_code_location_t*);
#endif

static NODE* node_newnode(struct parser_params *, enum node_type, VALUE, VALUE, VALUE, const rb_code_location_t*);
#define rb_node_newnode(type, a1, a2, a3, loc) node_newnode(p, (type), (a1), (a2), (a3), (loc))

static NODE *nd_set_loc(NODE *nd, const YYLTYPE *loc);

static int
parser_get_node_id(struct parser_params *p)
{
    int node_id = p->node_id;
    p->node_id++;
    return node_id;
}

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

static NODE* cond(struct parser_params *p, NODE *node, const YYLTYPE *loc);
static NODE* method_cond(struct parser_params *p, NODE *node, const YYLTYPE *loc);
#define new_nil(loc) NEW_NIL(loc)
static NODE *new_nil_at(struct parser_params *p, const rb_code_position_t *pos);
static NODE *new_if(struct parser_params*,NODE*,NODE*,NODE*,const YYLTYPE*);
static NODE *new_unless(struct parser_params*,NODE*,NODE*,NODE*,const YYLTYPE*);
static NODE *logop(struct parser_params*,ID,NODE*,NODE*,const YYLTYPE*,const YYLTYPE*);

static NODE *newline_node(NODE*);
static void fixpos(NODE*,NODE*);

static int value_expr_gen(struct parser_params*,NODE*);
static void void_expr(struct parser_params*,NODE*);
static NODE *remove_begin(NODE*);
static NODE *remove_begin_all(NODE*);
#define value_expr(node) value_expr_gen(p, (node) = remove_begin(node))
static NODE *void_stmts(struct parser_params*,NODE*);
static void reduce_nodes(struct parser_params*,NODE**);
static void block_dup_check(struct parser_params*,NODE*,NODE*);

static NODE *block_append(struct parser_params*,NODE*,NODE*);
static NODE *list_append(struct parser_params*,NODE*,NODE*);
static NODE *list_concat(NODE*,NODE*);
static NODE *arg_append(struct parser_params*,NODE*,NODE*,const YYLTYPE*);
static NODE *last_arg_append(struct parser_params *p, NODE *args, NODE *last_arg, const YYLTYPE *loc);
static NODE *rest_arg_append(struct parser_params *p, NODE *args, NODE *rest_arg, const YYLTYPE *loc);
static NODE *literal_concat(struct parser_params*,NODE*,NODE*,const YYLTYPE*);
static NODE *new_evstr(struct parser_params*,NODE*,const YYLTYPE*);
static NODE *new_dstr(struct parser_params*,NODE*,const YYLTYPE*);
static NODE *evstr2dstr(struct parser_params*,NODE*);
static NODE *splat_array(NODE*);
static void mark_lvar_used(struct parser_params *p, NODE *rhs);

static NODE *call_bin_op(struct parser_params*,NODE*,ID,NODE*,const YYLTYPE*,const YYLTYPE*);
static NODE *call_uni_op(struct parser_params*,NODE*,ID,const YYLTYPE*,const YYLTYPE*);
static NODE *new_qcall(struct parser_params* p, ID atype, NODE *recv, ID mid, NODE *args, const YYLTYPE *op_loc, const YYLTYPE *loc);
static NODE *new_command_qcall(struct parser_params* p, ID atype, NODE *recv, ID mid, NODE *args, NODE *block, const YYLTYPE *op_loc, const YYLTYPE *loc);
static NODE *method_add_block(struct parser_params*p, NODE *m, NODE *b, const YYLTYPE *loc) {b->nd_iter = m; b->nd_loc = *loc; return b;}

static bool args_info_empty_p(struct rb_args_info *args);
static NODE *new_args(struct parser_params*,NODE*,NODE*,ID,NODE*,NODE*,const YYLTYPE*);
static NODE *new_args_tail(struct parser_params*,NODE*,ID,ID,const YYLTYPE*);
static NODE *new_array_pattern(struct parser_params *p, NODE *constant, NODE *pre_arg, NODE *aryptn, const YYLTYPE *loc);
static NODE *new_array_pattern_tail(struct parser_params *p, NODE *pre_args, int has_rest, ID rest_arg, NODE *post_args, const YYLTYPE *loc);
static NODE *new_find_pattern(struct parser_params *p, NODE *constant, NODE *fndptn, const YYLTYPE *loc);
static NODE *new_find_pattern_tail(struct parser_params *p, ID pre_rest_arg, NODE *args, ID post_rest_arg, const YYLTYPE *loc);
static NODE *new_hash_pattern(struct parser_params *p, NODE *constant, NODE *hshptn, const YYLTYPE *loc);
static NODE *new_hash_pattern_tail(struct parser_params *p, NODE *kw_args, ID kw_rest_arg, const YYLTYPE *loc);

static NODE *new_kw_arg(struct parser_params *p, NODE *k, const YYLTYPE *loc);
static NODE *args_with_numbered(struct parser_params*,NODE*,int);

static VALUE negate_lit(struct parser_params*, VALUE);
static NODE *ret_args(struct parser_params*,NODE*);
static NODE *arg_blk_pass(NODE*,NODE*);
static NODE *new_yield(struct parser_params*,NODE*,const YYLTYPE*);
static NODE *dsym_node(struct parser_params*,NODE*,const YYLTYPE*);

static NODE *gettable(struct parser_params*,ID,const YYLTYPE*);
static NODE *assignable(struct parser_params*,ID,NODE*,const YYLTYPE*);

static NODE *aryset(struct parser_params*,NODE*,NODE*,const YYLTYPE*);
static NODE *attrset(struct parser_params*,NODE*,ID,ID,const YYLTYPE*);

static void rb_backref_error(struct parser_params*,NODE*);
static NODE *node_assign(struct parser_params*,NODE*,NODE*,const YYLTYPE*);

static NODE *new_op_assign(struct parser_params *p, NODE *lhs, ID op, NODE *rhs, const YYLTYPE *loc);
static NODE *new_ary_op_assign(struct parser_params *p, NODE *ary, NODE *args, ID op, NODE *rhs, const YYLTYPE *args_loc, const YYLTYPE *loc);
static NODE *new_attr_op_assign(struct parser_params *p, NODE *lhs, ID atype, ID attr, ID op, NODE *rhs, const YYLTYPE *loc);
static NODE *new_const_op_assign(struct parser_params *p, NODE *lhs, ID op, NODE *rhs, const YYLTYPE *loc);
static NODE *new_bodystmt(struct parser_params *p, NODE *head, NODE *rescue, NODE *rescue_else, NODE *ensure, const YYLTYPE *loc);

static NODE *const_decl(struct parser_params *p, NODE* path, const YYLTYPE *loc);

static NODE *opt_arg_append(NODE*, NODE*);
static NODE *kwd_append(NODE*, NODE*);

static NODE *new_hash(struct parser_params *p, NODE *hash, const YYLTYPE *loc);
static NODE *new_unique_key_hash(struct parser_params *p, NODE *hash, const YYLTYPE *loc);

static NODE *new_defined(struct parser_params *p, NODE *expr, const YYLTYPE *loc);

static NODE *new_regexp(struct parser_params *, NODE *, int, const YYLTYPE *);

#define make_list(list, loc) ((list) ? (nd_set_loc(list, loc), list) : NEW_ZLIST(loc))

static NODE *new_xstring(struct parser_params *, NODE *, const YYLTYPE *loc);

static NODE *symbol_append(struct parser_params *p, NODE *symbols, NODE *symbol);

static NODE *match_op(struct parser_params*,NODE*,NODE*,const YYLTYPE*,const YYLTYPE*);

static ID  *local_tbl(struct parser_params*);

static VALUE reg_compile(struct parser_params*, VALUE, int);
static void reg_fragment_setenc(struct parser_params*, VALUE, int);
static int reg_fragment_check(struct parser_params*, VALUE, int);
static NODE *reg_named_capture_assign(struct parser_params* p, VALUE regexp, const YYLTYPE *loc);

static int literal_concat0(struct parser_params *p, VALUE head, VALUE tail);
static NODE *heredoc_dedent(struct parser_params*,NODE*);

static void check_literal_when(struct parser_params *p, NODE *args, const YYLTYPE *loc);

#define get_id(id) (id)
#define get_value(val) (val)
#define get_num(num) (num)
#else  /* RIPPER */
#define NODE_RIPPER NODE_CDECL
#define NEW_RIPPER(a,b,c,loc) (VALUE)NEW_CDECL(a,b,c,loc)

static inline int ripper_is_node_yylval(VALUE n);

static inline VALUE
ripper_new_yylval(struct parser_params *p, ID a, VALUE b, VALUE c)
{
    if (ripper_is_node_yylval(c)) c = RNODE(c)->nd_cval;
    add_mark_object(p, b);
    add_mark_object(p, c);
    return NEW_RIPPER(a, b, c, &NULL_LOC);
}

static inline int
ripper_is_node_yylval(VALUE n)
{
    return RB_TYPE_P(n, T_NODE) && nd_type(RNODE(n)) == NODE_RIPPER;
}

#define value_expr(node) ((void)(node))
#define remove_begin(node) (node)
#define void_stmts(p,x) (x)
#define rb_dvar_defined(id, base) 0
#define rb_local_defined(id, base) 0
static ID ripper_get_id(VALUE);
#define get_id(id) ripper_get_id(id)
static VALUE ripper_get_value(VALUE);
#define get_value(val) ripper_get_value(val)
#define get_num(num) (int)get_id(num)
static VALUE assignable(struct parser_params*,VALUE);
static int id_is_var(struct parser_params *p, ID id);

#define method_cond(p,node,loc) (node)
#define call_bin_op(p, recv,id,arg1,op_loc,loc) dispatch3(binary, (recv), STATIC_ID2SYM(id), (arg1))
#define match_op(p,node1,node2,op_loc,loc) call_bin_op(0, (node1), idEqTilde, (node2), op_loc, loc)
#define call_uni_op(p, recv,id,op_loc,loc) dispatch2(unary, STATIC_ID2SYM(id), (recv))
#define logop(p,id,node1,node2,op_loc,loc) call_bin_op(0, (node1), (id), (node2), op_loc, loc)

#define new_nil(loc) Qnil

static VALUE new_regexp(struct parser_params *, VALUE, VALUE, const YYLTYPE *);

static VALUE const_decl(struct parser_params *p, VALUE path);

static VALUE var_field(struct parser_params *p, VALUE a);
static VALUE assign_error(struct parser_params *p, VALUE a);

static VALUE parser_reg_compile(struct parser_params*, VALUE, int, VALUE *);

#endif /* !RIPPER */

/* forward declaration */
typedef struct rb_strterm_heredoc_struct rb_strterm_heredoc_t;

RUBY_SYMBOL_EXPORT_BEGIN
VALUE rb_parser_reg_compile(struct parser_params* p, VALUE str, int options);
int rb_reg_fragment_setenc(struct parser_params*, VALUE, int);
enum lex_state_e rb_parser_trace_lex_state(struct parser_params *, enum lex_state_e, enum lex_state_e, int);
VALUE rb_parser_lex_state_name(enum lex_state_e state);
void rb_parser_show_bitstack(struct parser_params *, stack_type, const char *, int);
PRINTF_ARGS(void rb_parser_fatal(struct parser_params *p, const char *fmt, ...), 2, 3);
YYLTYPE *rb_parser_set_location_from_strterm_heredoc(struct parser_params *p, rb_strterm_heredoc_t *here, YYLTYPE *yylloc);
YYLTYPE *rb_parser_set_location_of_none(struct parser_params *p, YYLTYPE *yylloc);
YYLTYPE *rb_parser_set_location(struct parser_params *p, YYLTYPE *yylloc);
RUBY_SYMBOL_EXPORT_END

static void error_duplicate_pattern_variable(struct parser_params *p, ID id, const YYLTYPE *loc);
static void error_duplicate_pattern_key(struct parser_params *p, ID id, const YYLTYPE *loc);
static void parser_token_value_print(struct parser_params *p, enum yytokentype type, const YYSTYPE *valp);
static ID formal_argument(struct parser_params*, ID);
static ID shadowing_lvar(struct parser_params*,ID);
static void new_bv(struct parser_params*,ID);

static void local_push(struct parser_params*,int);
static void local_pop(struct parser_params*);
static void local_var(struct parser_params*, ID);
static void arg_var(struct parser_params*, ID);
static int  local_id(struct parser_params *p, ID id);
static int  local_id_ref(struct parser_params*, ID, ID **);
#ifndef RIPPER
static ID   internal_id(struct parser_params*);
static NODE *new_args_forward_call(struct parser_params*, NODE*, const YYLTYPE*, const YYLTYPE*);
static NODE *new_args_forward_def(struct parser_params*, NODE*, const YYLTYPE*);
#endif
static int check_forwarding_args(struct parser_params*);
static void add_forwarding_args(struct parser_params *p);

static const struct vtable *dyna_push(struct parser_params *);
static void dyna_pop(struct parser_params*, const struct vtable *);
static int dyna_in_block(struct parser_params*);
#define dyna_var(p, id) local_var(p, id)
static int dvar_defined(struct parser_params*, ID);
static int dvar_defined_ref(struct parser_params*, ID, ID**);
static int dvar_curr(struct parser_params*,ID);

static int lvar_defined(struct parser_params*, ID);

static NODE *numparam_push(struct parser_params *p);
static void numparam_pop(struct parser_params *p, NODE *prev_inner);

#ifdef RIPPER
# define METHOD_NOT idNOT
#else
# define METHOD_NOT '!'
#endif

#define idFWD_REST   '*'
#ifdef RUBY3_KEYWORDS
#define idFWD_KWREST idPow /* Use simple "**", as tDSTAR is "**arg" */
#else
#define idFWD_KWREST 0
#endif
#define idFWD_BLOCK  '&'

#define RE_OPTION_ONCE (1<<16)
#define RE_OPTION_ENCODING_SHIFT 8
#define RE_OPTION_ENCODING(e) (((e)&0xff)<<RE_OPTION_ENCODING_SHIFT)
#define RE_OPTION_ENCODING_IDX(o) (((o)>>RE_OPTION_ENCODING_SHIFT)&0xff)
#define RE_OPTION_ENCODING_NONE(o) ((o)&RE_OPTION_ARG_ENCODING_NONE)
#define RE_OPTION_MASK  0xff
#define RE_OPTION_ARG_ENCODING_NONE 32

/* structs for managing terminator of string literal and heredocment */
typedef struct rb_strterm_literal_struct {
    union {
	VALUE dummy;
	long nest;
    } u0;
    union {
	VALUE dummy;
	long func;	    /* STR_FUNC_* (e.g., STR_FUNC_ESCAPE and STR_FUNC_EXPAND) */
    } u1;
    union {
	VALUE dummy;
	long paren;	    /* '(' of `%q(...)` */
    } u2;
    union {
	VALUE dummy;
	long term;	    /* ')' of `%q(...)` */
    } u3;
} rb_strterm_literal_t;

#define HERETERM_LENGTH_BITS ((SIZEOF_VALUE - 1) * CHAR_BIT - 1)

struct rb_strterm_heredoc_struct {
    VALUE lastline;	/* the string of line that contains `<<"END"` */
    long offset;	/* the column of END in `<<"END"` */
    int sourceline;	/* lineno of the line that contains `<<"END"` */
    unsigned length	/* the length of END in `<<"END"` */
#if HERETERM_LENGTH_BITS < SIZEOF_INT * CHAR_BIT
    : HERETERM_LENGTH_BITS
# define HERETERM_LENGTH_MAX ((1U << HERETERM_LENGTH_BITS) - 1)
#else
# define HERETERM_LENGTH_MAX UINT_MAX
#endif
    ;
#if HERETERM_LENGTH_BITS < SIZEOF_INT * CHAR_BIT
    unsigned quote: 1;
    unsigned func: 8;
#else
    uint8_t quote;
    uint8_t func;
#endif
};
STATIC_ASSERT(rb_strterm_heredoc_t, sizeof(rb_strterm_heredoc_t) <= 4 * SIZEOF_VALUE);

#define STRTERM_HEREDOC IMEMO_FL_USER0

struct rb_strterm_struct {
    VALUE flags;
    union {
	rb_strterm_literal_t literal;
	rb_strterm_heredoc_t heredoc;
    } u;
};

#ifndef RIPPER
void
rb_strterm_mark(VALUE obj)
{
    rb_strterm_t *strterm = (rb_strterm_t*)obj;
    if (RBASIC(obj)->flags & STRTERM_HEREDOC) {
	rb_strterm_heredoc_t *heredoc = &strterm->u.heredoc;
	rb_gc_mark(heredoc->lastline);
    }
}
#endif

#define yytnamerr(yyres, yystr) (YYSIZE_T)rb_yytnamerr(p, yyres, yystr)
size_t rb_yytnamerr(struct parser_params *p, char *yyres, const char *yystr);

#define TOKEN2ID(tok) ( \
    tTOKEN_LOCAL_BEGIN<(tok)&&(tok)<tTOKEN_LOCAL_END ? TOKEN2LOCALID(tok) : \
    tTOKEN_INSTANCE_BEGIN<(tok)&&(tok)<tTOKEN_INSTANCE_END ? TOKEN2INSTANCEID(tok) : \
    tTOKEN_GLOBAL_BEGIN<(tok)&&(tok)<tTOKEN_GLOBAL_END ? TOKEN2GLOBALID(tok) : \
    tTOKEN_CONST_BEGIN<(tok)&&(tok)<tTOKEN_CONST_END ? TOKEN2CONSTID(tok) : \
    tTOKEN_CLASS_BEGIN<(tok)&&(tok)<tTOKEN_CLASS_END ? TOKEN2CLASSID(tok) : \
    tTOKEN_ATTRSET_BEGIN<(tok)&&(tok)<tTOKEN_ATTRSET_END ? TOKEN2ATTRSETID(tok) : \
    ((tok) / ((tok)<tPRESERVED_ID_END && ((tok)>=128 || rb_ispunct(tok)))))

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
static void ripper_error(struct parser_params *p);

#define dispatch0(n)            ripper_dispatch0(p, TOKEN_PASTE(ripper_id_, n))
#define dispatch1(n,a)          ripper_dispatch1(p, TOKEN_PASTE(ripper_id_, n), (a))
#define dispatch2(n,a,b)        ripper_dispatch2(p, TOKEN_PASTE(ripper_id_, n), (a), (b))
#define dispatch3(n,a,b,c)      ripper_dispatch3(p, TOKEN_PASTE(ripper_id_, n), (a), (b), (c))
#define dispatch4(n,a,b,c,d)    ripper_dispatch4(p, TOKEN_PASTE(ripper_id_, n), (a), (b), (c), (d))
#define dispatch5(n,a,b,c,d,e)  ripper_dispatch5(p, TOKEN_PASTE(ripper_id_, n), (a), (b), (c), (d), (e))
#define dispatch7(n,a,b,c,d,e,f,g) ripper_dispatch7(p, TOKEN_PASTE(ripper_id_, n), (a), (b), (c), (d), (e), (f), (g))

#define yyparse ripper_yyparse

#define ID2VAL(id) STATIC_ID2SYM(id)
#define TOKEN2VAL(t) ID2VAL(TOKEN2ID(t))
#define KWD2EID(t, v) ripper_new_yylval(p, keyword_##t, get_value(v), 0)

#define params_new(pars, opts, rest, pars2, kws, kwrest, blk) \
        dispatch7(params, (pars), (opts), (rest), (pars2), (kws), (kwrest), (blk))

#define escape_Qundef(x) ((x)==Qundef ? Qnil : (x))

static inline VALUE
new_args(struct parser_params *p, VALUE pre_args, VALUE opt_args, VALUE rest_arg, VALUE post_args, VALUE tail, YYLTYPE *loc)
{
    NODE *t = (NODE *)tail;
    VALUE kw_args = t->u1.value, kw_rest_arg = t->u2.value, block = t->u3.value;
    return params_new(pre_args, opt_args, rest_arg, post_args, kw_args, kw_rest_arg, escape_Qundef(block));
}

static inline VALUE
new_args_tail(struct parser_params *p, VALUE kw_args, VALUE kw_rest_arg, VALUE block, YYLTYPE *loc)
{
    NODE *t = rb_node_newnode(NODE_ARGS_AUX, kw_args, kw_rest_arg, block, &NULL_LOC);
    add_mark_object(p, kw_args);
    add_mark_object(p, kw_rest_arg);
    add_mark_object(p, block);
    return (VALUE)t;
}

static inline VALUE
args_with_numbered(struct parser_params *p, VALUE args, int max_numparam)
{
    return args;
}

static VALUE
new_array_pattern(struct parser_params *p, VALUE constant, VALUE pre_arg, VALUE aryptn, const YYLTYPE *loc)
{
    NODE *t = (NODE *)aryptn;
    VALUE pre_args = t->u1.value, rest_arg = t->u2.value, post_args = t->u3.value;

    if (!NIL_P(pre_arg)) {
	if (!NIL_P(pre_args)) {
	    rb_ary_unshift(pre_args, pre_arg);
	}
	else {
	    pre_args = rb_ary_new_from_args(1, pre_arg);
	}
    }
    return dispatch4(aryptn, constant, pre_args, rest_arg, post_args);
}

static VALUE
new_array_pattern_tail(struct parser_params *p, VALUE pre_args, VALUE has_rest, VALUE rest_arg, VALUE post_args, const YYLTYPE *loc)
{
    NODE *t;

    if (has_rest) {
	rest_arg = dispatch1(var_field, rest_arg ? rest_arg : Qnil);
    }
    else {
	rest_arg = Qnil;
    }

    t = rb_node_newnode(NODE_ARYPTN, pre_args, rest_arg, post_args, &NULL_LOC);
    add_mark_object(p, pre_args);
    add_mark_object(p, rest_arg);
    add_mark_object(p, post_args);
    return (VALUE)t;
}

static VALUE
new_find_pattern(struct parser_params *p, VALUE constant, VALUE fndptn, const YYLTYPE *loc)
{
    NODE *t = (NODE *)fndptn;
    VALUE pre_rest_arg = t->u1.value, args = t->u2.value, post_rest_arg = t->u3.value;

    return dispatch4(fndptn, constant, pre_rest_arg, args, post_rest_arg);
}

static VALUE
new_find_pattern_tail(struct parser_params *p, VALUE pre_rest_arg, VALUE args, VALUE post_rest_arg, const YYLTYPE *loc)
{
    NODE *t;

    pre_rest_arg = dispatch1(var_field, pre_rest_arg ? pre_rest_arg : Qnil);
    post_rest_arg = dispatch1(var_field, post_rest_arg ? post_rest_arg : Qnil);

    t = rb_node_newnode(NODE_FNDPTN, pre_rest_arg, args, post_rest_arg, &NULL_LOC);
    add_mark_object(p, pre_rest_arg);
    add_mark_object(p, args);
    add_mark_object(p, post_rest_arg);
    return (VALUE)t;
}

#define new_hash(p,h,l) rb_ary_new_from_args(0)

static VALUE
new_unique_key_hash(struct parser_params *p, VALUE ary, const YYLTYPE *loc)
{
    return ary;
}

static VALUE
new_hash_pattern(struct parser_params *p, VALUE constant, VALUE hshptn, const YYLTYPE *loc)
{
    NODE *t = (NODE *)hshptn;
    VALUE kw_args = t->u1.value, kw_rest_arg = t->u2.value;
    return dispatch3(hshptn, constant, kw_args, kw_rest_arg);
}

static VALUE
new_hash_pattern_tail(struct parser_params *p, VALUE kw_args, VALUE kw_rest_arg, const YYLTYPE *loc)
{
    NODE *t;
    if (kw_rest_arg) {
	kw_rest_arg = dispatch1(var_field, kw_rest_arg);
    }
    else {
	kw_rest_arg = Qnil;
    }
    t = rb_node_newnode(NODE_HSHPTN, kw_args, kw_rest_arg, 0, &NULL_LOC);

    add_mark_object(p, kw_args);
    add_mark_object(p, kw_rest_arg);
    return (VALUE)t;
}

#define new_defined(p,expr,loc) dispatch1(defined, (expr))

static VALUE heredoc_dedent(struct parser_params*,VALUE);

#else
#define ID2VAL(id) (id)
#define TOKEN2VAL(t) ID2VAL(t)
#define KWD2EID(t, v) keyword_##t

static NODE *
set_defun_body(struct parser_params *p, NODE *n, NODE *args, NODE *body, const YYLTYPE *loc)
{
    body = remove_begin(body);
    reduce_nodes(p, &body);
    n->nd_defn = NEW_SCOPE(args, body, loc);
    n->nd_loc = *loc;
    nd_set_line(n->nd_defn, loc->end_pos.lineno);
    set_line_body(body, loc->beg_pos.lineno);
    return n;
}

static NODE *
rescued_expr(struct parser_params *p, NODE *arg, NODE *rescue,
	     const YYLTYPE *arg_loc, const YYLTYPE *mod_loc, const YYLTYPE *res_loc)
{
    YYLTYPE loc = code_loc_gen(mod_loc, res_loc);
    rescue = NEW_RESBODY(0, remove_begin(rescue), 0, &loc);
    loc.beg_pos = arg_loc->beg_pos;
    return NEW_RESCUE(arg, rescue, 0, &loc);
}

#endif /* RIPPER */

static void
restore_defun(struct parser_params *p, NODE *name)
{
    YYSTYPE c = {.val = name->nd_cval};
    p->cur_arg = name->nd_vid;
    p->ctxt.in_def = c.ctxt.in_def;
}

static void
endless_method_name(struct parser_params *p, NODE *defn, const YYLTYPE *loc)
{
#ifdef RIPPER
    defn = defn->nd_defn;
#endif
    ID mid = defn->nd_mid;
    if (is_attrset_id(mid)) {
	yyerror1(loc, "setter method cannot be defined in an endless method definition");
    }
}

#ifndef RIPPER
# define Qnone 0
# define Qnull 0
# define ifndef_ripper(x) (x)
#else
# define Qnone Qnil
# define Qnull Qundef
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
static ID id_warn, id_warning, id_gets, id_assoc;
# define WARN_S_L(s,l) STR_NEW(s,l)
# define WARN_S(s) STR_NEW2(s)
# define WARN_I(i) INT2NUM(i)
# define WARN_ID(i) rb_id2str(i)
# define WARN_IVAL(i) i
# define PRIsWARN "s"
# define WARN_ARGS(fmt,n) p->value, id_warn, n, rb_usascii_str_new_lit(fmt)
# define WARN_ARGS_L(l,fmt,n) WARN_ARGS(fmt,n)
# ifdef HAVE_VA_ARGS_MACRO
# define WARN_CALL(...) rb_funcall(__VA_ARGS__)
# else
# define WARN_CALL rb_funcall
# endif
# define WARNING_ARGS(fmt,n) p->value, id_warning, n, rb_usascii_str_new_lit(fmt)
# define WARNING_ARGS_L(l, fmt,n) WARNING_ARGS(fmt,n)
# ifdef HAVE_VA_ARGS_MACRO
# define WARNING_CALL(...) rb_funcall(__VA_ARGS__)
# else
# define WARNING_CALL rb_funcall
# endif
PRINTF_ARGS(static void ripper_compile_error(struct parser_params*, const char *fmt, ...), 2, 3);
# define compile_error ripper_compile_error
#else
# define WARN_S_L(s,l) s
# define WARN_S(s) s
# define WARN_I(i) i
# define WARN_ID(i) rb_id2name(i)
# define WARN_IVAL(i) NUM2INT(i)
# define PRIsWARN PRIsVALUE
# define WARN_ARGS(fmt,n) WARN_ARGS_L(p->ruby_sourceline,fmt,n)
# define WARN_ARGS_L(l,fmt,n) p->ruby_sourcefile, (l), (fmt)
# define WARN_CALL rb_compile_warn
# define WARNING_ARGS(fmt,n) WARN_ARGS(fmt,n)
# define WARNING_ARGS_L(l,fmt,n) WARN_ARGS_L(l,fmt,n)
# define WARNING_CALL rb_compile_warning
PRINTF_ARGS(static void parser_compile_error(struct parser_params*, const char *fmt, ...), 2, 3);
# define compile_error parser_compile_error
#endif

static void token_info_setup(token_info *ptinfo, const char *ptr, const rb_code_location_t *loc);
static void token_info_push(struct parser_params*, const char *token, const rb_code_location_t *loc);
static void token_info_pop(struct parser_params*, const char *token, const rb_code_location_t *loc);
static void token_info_warn(struct parser_params *p, const char *token, token_info *ptinfo_beg, int same, const rb_code_location_t *loc);
static void token_info_drop(struct parser_params *p, const char *token, rb_code_position_t beg_pos);

#define WARN_EOL(tok) \
    (looking_at_eol_p(p) ? \
     (void)rb_warning0("`" tok "' at the end of line without an expression") : \
     (void)0)
static int looking_at_eol_p(struct parser_params *p);

#line 1113 "parse.c" /* yacc.c:339  */

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
# define YYERROR_VERBOSE 1
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
    tCHAR = 318,
    tNTH_REF = 319,
    tBACK_REF = 320,
    tSTRING_CONTENT = 321,
    tREGEXP_END = 322,
    tSP = 323,
    tUPLUS = 132,
    tUMINUS = 133,
    tPOW = 134,
    tCMP = 135,
    tEQ = 140,
    tEQQ = 141,
    tNEQ = 142,
    tGEQ = 139,
    tLEQ = 138,
    tANDOP = 148,
    tOROP = 149,
    tMATCH = 143,
    tNMATCH = 144,
    tDOT2 = 128,
    tDOT3 = 129,
    tBDOT2 = 130,
    tBDOT3 = 131,
    tAREF = 145,
    tASET = 146,
    tLSHFT = 136,
    tRSHFT = 137,
    tANDDOT = 150,
    tCOLON2 = 147,
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
    tSTRING_END = 345,
    tSTRING_DEND = 346,
    tSTRING_DBEG = 347,
    tSTRING_DVAR = 348,
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
#line 1069 "parse.y" /* yacc.c:355  */

    VALUE val;
    NODE *node;
    ID id;
    int num;
    st_table *tbl;
    const struct vtable *vars;
    struct rb_strterm_struct *strterm;
    struct lex_context ctxt;

#line 1287 "parse.c" /* yacc.c:355  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif

/* Location type.  */
#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE YYLTYPE;
struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
};
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif



int yyparse (struct parser_params *p);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 1317 "parse.c" /* yacc.c:358  */

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
         || (defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL \
             && defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
  YYLTYPE yyls_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE) + sizeof (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

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
#define YYLAST   14175

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  154
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  266
/* YYNRULES -- Number of rules.  */
#define YYNRULES  767
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  1268

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
       0,     2,     2,     2,     2,     2,     2,     2,     2,    71,
     153,    74,    72,    73,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,   152,   140,     2,     2,     2,   138,   133,     2,
     148,   149,   136,   134,   146,   135,    68,   137,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   128,   151,
     130,   126,   129,   127,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   145,    69,   150,   132,     2,   147,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   143,   131,   144,   141,     2,    88,    89,
      90,    91,    75,    76,    77,    78,    94,    95,    83,    82,
      79,    80,    81,    86,    87,    92,    93,    97,    84,    85,
      96,     2,     2,     2,     2,     2,     2,     2,     2,     2,
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
      65,    66,    67,    70,    98,    99,   100,   101,   102,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   139,   142
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,  1269,  1269,  1269,  1295,  1301,  1308,  1315,  1322,  1328,
    1329,  1335,  1348,  1346,  1357,  1368,  1374,  1381,  1388,  1395,
    1401,  1406,  1405,  1415,  1415,  1422,  1429,  1439,  1447,  1454,
    1462,  1470,  1482,  1494,  1504,  1518,  1519,  1527,  1535,  1544,
    1551,  1554,  1561,  1568,  1576,  1583,  1590,  1598,  1605,  1615,
    1620,  1629,  1632,  1633,  1637,  1641,  1645,  1650,  1657,  1659,
    1649,  1671,  1674,  1691,  1700,  1700,  1714,  1721,  1721,  1721,
    1727,  1728,  1731,  1732,  1741,  1751,  1761,  1770,  1781,  1788,
    1795,  1802,  1809,  1817,  1825,  1832,  1839,  1848,  1849,  1858,
    1859,  1868,  1875,  1882,  1889,  1896,  1903,  1910,  1917,  1924,
    1931,  1940,  1941,  1950,  1957,  1966,  1973,  1982,  1989,  1996,
    2003,  2013,  2020,  2030,  2037,  2044,  2054,  2061,  2068,  2075,
    2082,  2089,  2096,  2103,  2110,  2120,  2127,  2130,  2137,  2144,
    2153,  2154,  2155,  2156,  2161,  2164,  2171,  2174,  2181,  2181,
    2191,  2192,  2193,  2194,  2195,  2196,  2197,  2198,  2199,  2200,
    2201,  2202,  2203,  2204,  2205,  2206,  2207,  2208,  2209,  2210,
    2211,  2212,  2213,  2214,  2215,  2216,  2217,  2218,  2219,  2220,
    2223,  2223,  2223,  2224,  2224,  2225,  2225,  2225,  2226,  2226,
    2226,  2226,  2227,  2227,  2227,  2227,  2228,  2228,  2228,  2229,
    2229,  2229,  2229,  2230,  2230,  2230,  2230,  2231,  2231,  2231,
    2231,  2232,  2232,  2232,  2232,  2233,  2233,  2233,  2233,  2234,
    2234,  2237,  2244,  2251,  2258,  2265,  2272,  2279,  2287,  2294,
    2302,  2311,  2320,  2328,  2336,  2344,  2352,  2356,  2360,  2364,
    2368,  2372,  2376,  2380,  2384,  2388,  2392,  2396,  2400,  2404,
    2405,  2409,  2413,  2417,  2421,  2425,  2429,  2433,  2437,  2441,
    2445,  2449,  2449,  2454,  2463,  2474,  2486,  2498,  2511,  2517,
    2518,  2519,  2520,  2523,  2527,  2534,  2541,  2542,  2546,  2553,
    2562,  2567,  2577,  2584,  2596,  2610,  2611,  2614,  2615,  2616,
    2620,  2627,  2636,  2644,  2651,  2659,  2667,  2671,  2671,  2708,
    2717,  2721,  2727,  2734,  2741,  2748,  2757,  2758,  2761,  2768,
    2775,  2784,  2785,  2786,  2787,  2788,  2789,  2790,  2791,  2792,
    2793,  2794,  2802,  2801,  2816,  2816,  2823,  2823,  2831,  2839,
    2846,  2853,  2860,  2868,  2875,  2882,  2889,  2896,  2896,  2901,
    2905,  2909,  2916,  2917,  2925,  2926,  2937,  2948,  2958,  2969,
    2968,  2985,  2984,  2999,  3008,  3053,  3052,  3075,  3074,  3096,
    3095,  3117,  3129,  3143,  3150,  3157,  3164,  3173,  3180,  3186,
    3203,  3209,  3215,  3221,  3227,  3233,  3240,  3247,  3253,  3259,
    3265,  3271,  3277,  3283,  3298,  3305,  3311,  3318,  3319,  3320,
    3323,  3324,  3327,  3328,  3340,  3341,  3350,  3351,  3354,  3362,
    3371,  3378,  3387,  3394,  3401,  3408,  3415,  3424,  3432,  3441,
    3442,  3445,  3449,  3453,  3457,  3463,  3468,  3473,  3483,  3487,
    3491,  3495,  3499,  3503,  3508,  3512,  3516,  3520,  3524,  3528,
    3532,  3536,  3540,  3546,  3547,  3553,  3562,  3574,  3578,  3587,
    3589,  3593,  3598,  3605,  3611,  3615,  3619,  3604,  3644,  3652,
    3662,  3667,  3673,  3683,  3697,  3704,  3711,  3720,  3729,  3737,
    3745,  3752,  3760,  3768,  3775,  3782,  3795,  3803,  3813,  3814,
    3818,  3813,  3835,  3836,  3840,  3835,  3859,  3867,  3874,  3882,
    3891,  3903,  3904,  3908,  3915,  3919,  3907,  3934,  3935,  3938,
    3939,  3947,  3957,  3958,  3963,  3971,  3975,  3979,  3985,  3988,
    3997,  4000,  4007,  4010,  4011,  4013,  4014,  4023,  4032,  4041,
    4046,  4055,  4064,  4073,  4078,  4082,  4086,  4092,  4091,  4103,
    4108,  4108,  4115,  4124,  4128,  4137,  4141,  4145,  4149,  4153,
    4156,  4160,  4169,  4173,  4179,  4189,  4193,  4199,  4200,  4209,
    4218,  4222,  4226,  4230,  4236,  4238,  4247,  4255,  4269,  4270,
    4293,  4297,  4303,  4309,  4310,  4313,  4314,  4323,  4332,  4340,
    4348,  4349,  4350,  4351,  4359,  4369,  4370,  4371,  4372,  4373,
    4374,  4375,  4376,  4377,  4384,  4387,  4397,  4410,  4417,  4424,
    4433,  4445,  4448,  4455,  4462,  4465,  4469,  4472,  4479,  4482,
    4483,  4486,  4503,  4504,  4505,  4514,  4524,  4533,  4539,  4549,
    4555,  4564,  4566,  4575,  4585,  4591,  4600,  4609,  4619,  4625,
    4635,  4641,  4651,  4661,  4680,  4686,  4696,  4706,  4747,  4750,
    4749,  4766,  4770,  4775,  4779,  4783,  4765,  4804,  4811,  4818,
    4825,  4828,  4829,  4832,  4842,  4843,  4844,  4845,  4848,  4858,
    4859,  4869,  4870,  4871,  4872,  4875,  4876,  4877,  4878,  4879,
    4882,  4883,  4884,  4885,  4886,  4887,  4888,  4891,  4904,  4913,
    4920,  4929,  4930,  4934,  4933,  4943,  4951,  4951,  4953,  4962,
    4972,  4984,  4985,  4985,  4999,  5003,  5007,  5011,  5017,  5022,
    5027,  5031,  5035,  5039,  5043,  5047,  5051,  5055,  5059,  5063,
    5067,  5071,  5075,  5079,  5084,  5090,  5099,  5107,  5115,  5123,
    5133,  5134,  5142,  5151,  5159,  5180,  5182,  5195,  5205,  5213,
    5223,  5230,  5239,  5246,  5256,  5263,  5272,  5273,  5276,  5284,
    5292,  5302,  5312,  5322,  5329,  5338,  5345,  5354,  5355,  5358,
    5366,  5376,  5377,  5380,  5390,  5394,  5400,  5405,  5405,  5429,
    5430,  5439,  5441,  5464,  5475,  5482,  5490,  5509,  5510,  5511,
    5514,  5515,  5516,  5517,  5520,  5521,  5522,  5525,  5526,  5529,
    5530,  5533,  5534,  5537,  5538,  5541,  5542,  5545,  5548,  5551,
    5554,  5555,  5556,  5559,  5560,  5563,  5564,  5568
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 1
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end-of-input\"", "error", "$undefined", "\"`class'\"",
  "\"`module'\"", "\"`def'\"", "\"`undef'\"", "\"`begin'\"",
  "\"`rescue'\"", "\"`ensure'\"", "\"`end'\"", "\"`if'\"", "\"`unless'\"",
  "\"`then'\"", "\"`elsif'\"", "\"`else'\"", "\"`case'\"", "\"`when'\"",
  "\"`while'\"", "\"`until'\"", "\"`for'\"", "\"`break'\"", "\"`next'\"",
  "\"`redo'\"", "\"`retry'\"", "\"`in'\"", "\"`do'\"",
  "\"`do' for condition\"", "\"`do' for block\"", "\"`do' for lambda\"",
  "\"`return'\"", "\"`yield'\"", "\"`super'\"", "\"`self'\"", "\"`nil'\"",
  "\"`true'\"", "\"`false'\"", "\"`and'\"", "\"`or'\"", "\"`not'\"",
  "\"`if' modifier\"", "\"`unless' modifier\"", "\"`while' modifier\"",
  "\"`until' modifier\"", "\"`rescue' modifier\"", "\"`alias'\"",
  "\"`defined?'\"", "\"`BEGIN'\"", "\"`END'\"", "\"`__LINE__'\"",
  "\"`__FILE__'\"", "\"`__ENCODING__'\"", "\"local variable or method\"",
  "\"method\"", "\"global variable\"", "\"instance variable\"",
  "\"constant\"", "\"class variable\"", "\"label\"", "\"integer literal\"",
  "\"float literal\"", "\"rational literal\"", "\"imaginary literal\"",
  "\"char literal\"", "\"numbered reference\"", "\"back reference\"",
  "\"literal content\"", "tREGEXP_END", "'.'", "\"backslash\"",
  "\"escaped space\"", "\"escaped horizontal tab\"",
  "\"escaped form feed\"", "\"escaped carriage return\"",
  "\"escaped vertical tab\"", "\"unary+\"", "\"unary-\"", "\"**\"",
  "\"<=>\"", "\"==\"", "\"===\"", "\"!=\"", "\">=\"", "\"<=\"", "\"&&\"",
  "\"||\"", "\"=~\"", "\"!~\"", "\"..\"", "\"...\"", "\"(..\"", "\"(...\"",
  "\"[]\"", "\"[]=\"", "\"<<\"", "\">>\"", "\"&.\"", "\"::\"",
  "\":: at EXPR_BEG\"", "\"operator-assignment\"", "\"=>\"", "\"(\"",
  "\"( arg\"", "\")\"", "\"[\"", "\"{\"", "\"{ arg\"", "\"*\"",
  "\"**arg\"", "\"&\"", "\"->\"", "\"symbol literal\"",
  "\"string literal\"", "\"backtick literal\"", "\"regexp literal\"",
  "\"word list\"", "\"verbatim word list\"", "\"symbol list\"",
  "\"verbatim symbol list\"", "\"terminator\"", "\"'}'\"", "tSTRING_DBEG",
  "tSTRING_DVAR", "tLAMBEG", "tLABEL_END", "tLOWEST", "'='", "'?'", "':'",
  "'>'", "'<'", "'|'", "'^'", "'&'", "'+'", "'-'", "'*'", "'/'", "'%'",
  "tUMINUS_NUM", "'!'", "'~'", "tLAST_TOKEN", "'{'", "'}'", "'['", "','",
  "'`'", "'('", "')'", "']'", "';'", "' '", "'\\n'", "$accept", "program",
  "$@1", "top_compstmt", "top_stmts", "top_stmt", "begin_block",
  "bodystmt", "$@2", "compstmt", "stmts", "stmt_or_begin", "$@3", "stmt",
  "$@4", "command_asgn", "command_rhs", "expr", "@5", "@6", "$@7",
  "def_name", "defn_head", "defs_head", "$@8", "expr_value",
  "expr_value_do", "$@9", "$@10", "command_call", "block_command",
  "cmd_brace_block", "fcall", "command", "mlhs", "mlhs_inner",
  "mlhs_basic", "mlhs_item", "mlhs_head", "mlhs_post", "mlhs_node", "lhs",
  "cname", "cpath", "fname", "fitem", "undef_list", "$@11", "op",
  "reswords", "arg", "$@12", "relop", "rel_expr", "arg_value", "aref_args",
  "arg_rhs", "paren_args", "opt_paren_args", "opt_call_args", "call_args",
  "command_args", "$@13", "block_arg", "opt_block_arg", "args", "mrhs_arg",
  "mrhs", "primary", "$@14", "$@15", "$@16", "$@17", "@18", "@19", "$@20",
  "$@21", "$@22", "primary_value", "k_begin", "k_if", "k_unless",
  "k_while", "k_until", "k_case", "k_for", "k_class", "k_module", "k_def",
  "k_do", "k_do_block", "k_rescue", "k_ensure", "k_when", "k_else",
  "k_elsif", "k_end", "k_return", "then", "do", "if_tail", "opt_else",
  "for_var", "f_marg", "f_marg_list", "f_margs", "f_rest_marg",
  "f_any_kwrest", "block_args_tail", "opt_block_args_tail",
  "excessed_comma", "block_param", "opt_block_param", "block_param_def",
  "opt_bv_decl", "bv_decls", "bvar", "lambda", "@23", "@24", "@25", "$@26",
  "f_larglist", "lambda_body", "do_block", "block_call", "method_call",
  "brace_block", "brace_body", "@27", "@28", "@29", "do_body", "@30",
  "@31", "@32", "case_args", "case_body", "cases", "p_case_body", "@33",
  "@34", "$@35", "p_cases", "p_top_expr", "p_top_expr_body", "p_expr",
  "p_as", "p_alt", "p_lparen", "p_lbracket", "p_expr_basic", "@36", "@37",
  "p_args", "p_args_head", "p_args_tail", "p_find", "p_rest",
  "p_args_post", "p_arg", "p_kwargs", "p_kwarg", "p_kw", "p_kw_label",
  "p_kwrest", "p_kwnorest", "p_any_kwrest", "p_value", "p_primitive",
  "p_variable", "p_var_ref", "p_const", "opt_rescue", "exc_list",
  "exc_var", "opt_ensure", "literal", "strings", "string", "string1",
  "xstring", "regexp", "words", "word_list", "word", "symbols",
  "symbol_list", "qwords", "qsymbols", "qword_list", "qsym_list",
  "string_contents", "xstring_contents", "regexp_contents",
  "string_content", "@38", "$@39", "@40", "@41", "@42", "@43",
  "string_dvar", "symbol", "ssym", "sym", "dsym", "numeric",
  "simple_numeric", "user_variable", "keyword_variable", "var_ref",
  "var_lhs", "backref", "superclass", "$@44", "f_opt_paren_args",
  "f_paren_args", "f_arglist", "@45", "args_tail", "opt_args_tail",
  "f_args", "args_forward", "f_bad_arg", "f_norm_arg", "f_arg_asgn",
  "f_arg_item", "f_arg", "f_label", "f_kw", "f_block_kw", "f_block_kwarg",
  "f_kwarg", "kwrest_mark", "f_no_kwarg", "f_kwrest", "f_opt",
  "f_block_opt", "f_block_optarg", "f_optarg", "restarg_mark",
  "f_rest_arg", "blkarg_mark", "f_block_arg", "opt_f_block_arg",
  "singleton", "$@46", "assoc_list", "assocs", "assoc", "operation",
  "operation2", "operation3", "dot_or_colon", "call_op", "call_op2",
  "opt_terms", "opt_nl", "rparen", "rbracket", "rbrace", "trailer", "term",
  "terms", "none", YY_NULLPTR
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
     315,   316,   317,   318,   319,   320,   321,   322,    46,    92,
     323,     9,    12,    13,    11,   132,   133,   134,   135,   140,
     141,   142,   139,   138,   148,   149,   143,   144,   128,   129,
     130,   131,   145,   146,   136,   137,   150,   147,   324,   325,
     326,   327,   328,   329,   330,   331,   332,   333,   334,   335,
     336,   337,   338,   339,   340,   341,   342,   343,   344,   345,
     346,   347,   348,   349,   350,   351,    61,    63,    58,    62,
      60,   124,    94,    38,    43,    45,    42,    47,    37,   352,
      33,   126,   353,   123,   125,    91,    44,    96,    40,    41,
      93,    59,    32,    10
};
# endif

#define YYPACT_NINF -1036

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-1036)))

#define YYTABLE_NINF -768

#define yytable_value_is_error(Yytable_value) \
  (!!((Yytable_value) == (-768)))

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
   -1036,   179,  4097, -1036,  9373, -1036, -1036, -1036,  8831, -1036,
   -1036, -1036, -1036, -1036, -1036, -1036,  9499,  9499, -1036, -1036,
   -1036,  5291,  4850, -1036, -1036, -1036, -1036,    45,  8686,    42,
      92,   106, -1036, -1036, -1036,  3802,  4997, -1036, -1036,  4253,
   -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036, 11263, 11263,
   11263, 11263,   151,  6886,  9625, 10129, 10507,  9115, -1036,  8541,
   -1036, -1036, -1036,   138,   185,   198,   276,  1191, 11389, 11263,
   -1036,   626, -1036,  1219, -1036,   556,    57,    57, -1036, -1036,
     156,   359,   397, -1036,   339, 11641, -1036,   403,  5560,   713,
     468,   521, -1036, 11515, 11515, -1036, -1036,  7868, 11763, 11885,
   12007,  8395,  9499, -1036,   322,    75, -1036, -1036,   531, -1036,
   -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036,
   -1036,    66,   301, -1036,   469,   446, -1036, -1036, -1036, -1036,
   -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036,
   -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036,
   -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036,
   -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036,
   -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036,
   -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036,
   -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036,
   -1036, -1036, -1036, -1036, -1036,   502, -1036, -1036, -1036,   506,
   11263,   606,  7037, 11263, 11263, 11263, -1036, 11263, -1036,   545,
    4828,   582, -1036, -1036,   573,   534,    64,    68,   633,    74,
     591, -1036, -1036,  7742, -1036,  9499,  9751, -1036, -1036,  7994,
   -1036, 11515,   278, -1036,   609,  7188, -1036,  7339, -1036, -1036,
     614,   635,   156, -1036,   536, -1036,   708,  4975,  4975,   459,
    9625, -1036,  6886,   638,   626, -1036,  1219,    42,   667, -1036,
    1219,    42,   657,     7,   375, -1036,   582,   661,   375, -1036,
      42,   756,  1191, 12129,   670,   670,   675, -1036,   731,   742,
     764,   777, -1036, -1036, -1036, -1036, -1036,   507, -1036,   771,
     804,   709, -1036, -1036, -1036, -1036,   748, -1036, -1036, -1036,
   -1036, -1036, -1036, -1036,  8120, 11515, 11515, 11515, 11515,  9625,
   11515, 11515,  1987,   697,   715,  6030,  2051, -1036,   721,  6030,
   -1036, -1036, -1036,   745, -1036, -1036, -1036, -1036, -1036, 10633,
   -1036,  6886,  9244,   716, 10633, 11263, 11263, 11263, 11263, 11263,
   -1036, -1036, 11263, 11263, 11263, 11263, 11263, 11263, 11263, 11263,
   -1036, 11263, -1036, -1036, 11263, 11263, 11263, 11263, 11263, 11263,
   11263, 11263, 11263, 11263, -1036, -1036, 12678,  9499, 12768,  6030,
     556,   118,   118,  7490, 11515,  7490,   626, -1036,   704,   824,
   -1036, -1036,   837,   859,    80,    81,    90,   768,   959, 11515,
     511, -1036,   776,   913, -1036, -1036, -1036, -1036,    88,   325,
     355,   356,   406,   412,   420,   421,   425, -1036, -1036, -1036,
   -1036,   518, -1036, -1036, -1036, 14028, -1036, -1036, 11389, 11389,
   -1036, -1036,   207, -1036, -1036, -1036,   428, 11263, 11263,  9877,
   -1036, -1036, 12858,  9499, 12948, 11263, 11263, 10255, -1036,    42,
     761, -1036, -1036, 11263,    42, -1036,   766,    42,   769, -1036,
      98, -1036, -1036, -1036, -1036, -1036,  8831, -1036, 11263,   773,
     785, 12858, 12948, 11263,  1219,    92,    42, -1036, -1036,  8246,
     774,    42, -1036, -1036, 10381, -1036, -1036, 10507, -1036, -1036,
   -1036,   609,   949, -1036, -1036,   775, -1036, 12129, 13038,  9499,
   13128, -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036,
   -1036, -1036,   823,    89,  1018,   120, 11263, -1036, -1036, -1036,
   -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036,
   -1036, -1036, -1036, -1036,  1424, -1036, -1036, -1036, -1036, -1036,
     792, -1036,    42,    42, -1036, -1036,   836, -1036,   817, 11263,
   -1036,   821,   567, -1036, -1036, -1036,   822,   920,   825,   935,
   -1036, 11263,   980,   985,   626,   851, 11263,   980,   855, -1036,
   -1036, -1036,   980, -1036,   980, 11263, -1036,   857,   865,   956,
   -1036,    42, 12129,   870, -1036, -1036, -1036,   979,   902,  4231,
   -1036, -1036, -1036,   984,   212,   708,  2631,  2631,  2631,  2631,
    3306,  2347,  2631,  2631,  4975,  4975,  1036,  1036, -1036,  5413,
    1102,  1102,  1243,   579,   579,   708,   708,   708,  1336,  1336,
    5438,  4409,  5732,  4556, -1036,   635, -1036,    42,   883,   546,
   -1036,   801, -1036, -1036,  5144,   980, -1036,  6181,  1019,  6634,
     980,    99,   980,  1013,  1023,   116, 13218,  9499, 13308, -1036,
     556, -1036,   949, -1036, -1036, -1036, 13398,  9499, 13488,  6030,
   11515, -1036, -1036, -1036, -1036, -1036,  3225, -1036,  4387, -1036,
   -1036, -1036,  8831, 11263, -1036, 11263,   582, -1036,   591,  3463,
    4703,    42,   250,   485, -1036, -1036, -1036, -1036, 10003, -1036,
   10255, -1036, -1036, 11515,  4828, -1036, -1036,   635,   635, -1036,
   -1036,   202, -1036, -1036,   375, 12129,   775,   450,   429,    42,
     602,   621,  1659, -1036,   919, -1036,    67, -1036,   889, -1036,
   -1036,   417,   890, -1036,   708,  1424,  1068, -1036,   898,    42,
     903, -1036,    82, -1036, -1036, -1036, -1036, 11263,  1987, -1036,
   -1036,   681, -1036, -1036, -1036,  2051, -1036, -1036,  1385, -1036,
   -1036,  4534, -1036, -1036, -1036, 10759,   400, -1036, -1036,  2051,
    4681, -1036, -1036, -1036,   910, -1036, -1036, -1036,   914, 10885,
    9625, -1036,   775, 12129,  9625, 11389, 11263, 13578,  9499, 13668,
   12611,   931, 11389, 11389, -1036,   745,   911,   802,  9877, 11389,
   11389, -1036, -1036,   745, -1036, -1036,   735, -1036,  1053, -1036,
   -1036, -1036, -1036, -1036, -1036,  1023,   980, -1036, 11011,   980,
      94,   124,    42,   133,   147,  7490,   626, 11515,  6030,   966,
     429, -1036,    42,   980,    98,  8976,    75,   359, -1036, -1036,
   -1036, -1036, 11263, 11263,   513, 11263, 11263,    42,   929,    98,
   -1036, -1036,   662,  2051, -1036, -1036, -1036, -1036, -1036, -1036,
   -1036, -1036, -1036, -1036, -1036, -1036, -1036,    42, -1036,  1424,
   -1036,  1169, -1036, -1036, -1036,    42, -1036,   930,   937, -1036,
    1035,   792,   943, -1036,   954, -1036,   943, 11263,   857, -1036,
    1006, -1036, -1036, -1036,  7490, -1036, -1036, -1036, 11263,   977,
   -1036,   977, 11263,   964, -1036,   775, -1036,  4828,  5585,  5879,
      42,   568,   597, -1036, -1036,  2250,  2250,   655, -1036, 12406,
     456,  1060, -1036,  1016, -1036,   987, -1036, -1036,   746, -1036,
   -1036,    54, -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036,
   -1036, 11263, -1036, -1036, -1036, -1036, -1036, -1036, 11389, -1036,
   -1036, -1036, -1036, -1036, -1036,  7490, 11515,   980, -1036, -1036,
     980, -1036, -1036,   980, -1036, 11263, -1036,   312, -1036,   152,
     980,  6030,   626,   980, -1036, -1036, -1036, -1036, -1036, -1036,
   11263, -1036, 10255, -1036,    42,   110, -1036, -1036, -1036,   973,
     982, -1036,  2051, -1036,  1385, -1036, -1036,  1385, -1036,  1385,
   -1036, -1036,  4828, 12251,   118, -1036, -1036,  6760,  4828,  1251,
    7339, -1036, -1036,  6030,   988,   643, -1036, -1036, -1036, -1036,
   12611,  1074,    71,    42, 12492, -1036,    42,   992,  1000, -1036,
     298,  1005, -1036, -1036,  1079, -1036, 12611,  2250,  2250,   655,
     648,   559,  3937,  3937,  4828, -1036, -1036, -1036,   118, -1036,
   -1036,  3937, -1036, -1036, 11137,  6332, -1036,   980, -1036, -1036,
    1001,  1003,  6030,  7339, -1036, -1036,  1169,  1169,   943,  1009,
     943,   943,  1096, -1036,   999,   129,   135,   167,  6030,  1148,
     792, -1036,    42,  1027,  1034,  1017, 12373, -1036,  1020, -1036,
    1029,  1030, -1036, -1036, -1036,   163, -1036, -1036,   100,  1016,
    1038, -1036, 12611, -1036, -1036, -1036,    42,  1039, -1036, 12611,
   -1036, -1036, -1036,   679, -1036, -1036, -1036, -1036, -1036, -1036,
   -1036, -1036, -1036,    42,    42,    42,    42,    42,    42,  6181,
     118,   854,   178, -1036, -1036, -1036, 11263, -1036,   682, -1036,
   -1036,  1175,   980,  1021,  7616,   982, -1036,  1385, -1036, -1036,
   -1036,   235, 13758,  9499, 13848,   985, -1036, -1036,  1055, -1036,
   12373,  2051, -1036, -1036,  1132,  1002,   681, -1036,  2051, -1036,
    1385, -1036, -1036,  1045, 12611, -1036,  1048, -1036,   335, -1036,
     298,  1016, -1036, -1036,   603, -1036, -1036, -1036, -1036, -1036,
   -1036,   735, -1036, 11515, 11515, 12578, -1036, -1036, -1036, -1036,
   -1036,   505, -1036, -1036, -1036, -1036,  1081,   943,   108,   328,
      42,   349,   381, -1036, -1036,  1002, -1036,  1056,  1057, -1036,
   13938, -1036,   792,  1061, -1036,  1063,  1061, 12611,  1064, 12578,
   -1036, -1036, -1036, -1036,  6483, -1036, -1036, -1036,  1065,  1175,
   -1036, -1036, -1036,   384,  2051, -1036,  1385, -1036,  1051,  1058,
   -1036,  1385, -1036,  1385, -1036, -1036,  1064, 12611, -1036, -1036,
     172, 12611, -1036,  1061,  1067,  1061,  1061, -1036, -1036, -1036,
    1064, -1036,  1385, -1036, -1036, -1036,  1061, -1036
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       2,     0,     0,     1,     0,   365,   366,   367,     0,   358,
     359,   360,   363,   361,   362,   364,   353,   354,   355,   356,
     376,   287,   287,   641,   640,   642,   643,   755,     0,   755,
       0,     0,   645,   644,   646,   737,   739,   637,   636,   738,
     639,   631,   632,   633,   634,   582,   651,   652,     0,     0,
       0,     0,     0,     0,   314,   767,   767,    99,   433,   602,
     602,   604,   606,     0,     0,     0,     0,     0,     0,     0,
       3,   753,     6,     9,    35,    40,   662,   662,    52,    71,
     287,    70,     0,    87,     0,    91,   101,     0,    61,   239,
     258,     0,   312,     0,     0,    67,    67,   753,     0,     0,
       0,     0,   323,   334,    72,   332,   301,   302,   581,   583,
     303,   304,   305,   307,   306,   308,   580,   621,   622,   579,
     629,   647,   648,   309,     0,   310,    75,     5,     8,   180,
     191,   181,   204,   177,   197,   187,   186,   207,   208,   202,
     185,   184,   179,   205,   209,   210,   189,   178,   192,   196,
     198,   190,   183,   199,   206,   201,   200,   193,   203,   188,
     176,   195,   194,   175,   182,   173,   174,   170,   171,   172,
     130,   132,   131,   165,   166,   161,   143,   144,   145,   152,
     149,   151,   146,   147,   167,   168,   153,   154,   158,   162,
     148,   150,   140,   141,   142,   155,   156,   157,   159,   160,
     163,   164,   169,   135,   137,    28,   133,   134,   136,     0,
       0,     0,     0,     0,     0,     0,   602,     0,   282,     0,
     265,   292,    85,   286,   767,     0,   647,   648,     0,   310,
     767,   731,    86,   755,    83,     0,   767,   453,    82,   755,
     756,     0,     0,    23,   251,     0,    10,     0,   353,   354,
     326,   454,     0,   233,     0,   323,   234,   224,   225,   320,
       0,    21,     0,     0,   753,    17,    20,   755,    89,    16,
     316,   755,     0,   760,   760,   266,     0,     0,   760,   729,
     755,     0,     0,     0,   662,   662,    97,   357,     0,   107,
     108,   115,   434,   626,   625,   627,   624,     0,   623,     0,
       0,     0,   589,   598,   594,   600,   630,    56,   245,   246,
     763,   764,     4,   765,   754,     0,     0,     0,     0,     0,
       0,     0,   684,     0,   661,     0,   684,   657,     0,     0,
     368,   458,   447,    76,   462,   331,   369,   462,   443,     0,
     103,     0,    95,    92,     0,     0,     0,     0,     0,     0,
     261,   262,     0,     0,     0,     0,   222,   223,     0,     0,
      57,     0,   259,   260,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   749,   750,     0,   767,     0,     0,
      66,     0,     0,     0,     0,     0,   753,   341,   754,     0,
     387,   386,     0,     0,   647,   648,   310,   125,   126,     0,
       0,   128,   655,     0,   647,   648,   310,   349,   200,   193,
     203,   188,   170,   171,   172,   130,   131,   727,    63,    62,
     726,     0,    84,   752,   751,     0,   333,   584,     0,     0,
     138,   734,   320,   293,   736,   289,     0,     0,     0,     0,
     283,   291,     0,   767,     0,     0,     0,     0,   284,   755,
       0,   325,   288,   685,   755,   278,   767,   755,   767,   277,
     755,   330,    55,    25,    27,    26,     0,   327,     0,     0,
       0,     0,     0,     0,    19,     0,   755,   318,    15,   754,
      88,   755,   315,   321,   762,   761,   267,   762,   269,   322,
     730,     0,   114,   630,   105,   100,   661,     0,     0,   767,
       0,   435,   608,   628,   611,   609,   603,   585,   586,   605,
     587,   607,     0,     0,     0,     0,     0,   766,     7,    29,
      30,    31,    32,    33,    53,    54,   691,   688,   687,   686,
     689,   697,   706,   685,     0,   718,   707,   722,   721,   717,
     767,   683,   755,   755,   690,   692,   693,   695,   669,   699,
     704,   767,   710,   400,   399,   715,   669,   720,   669,     0,
     667,     0,     0,   767,     0,   669,     0,     0,     0,   459,
     458,    77,     0,   463,     0,     0,    36,   297,     0,    39,
     296,   755,     0,    93,   104,    51,    41,    49,     0,   270,
     292,   211,    37,     0,   310,   231,   238,   240,   241,   242,
     249,   250,   243,   244,   220,   221,   247,   248,    58,   755,
     235,   236,   237,   226,   227,   228,   229,   230,   263,   264,
     740,   742,   741,   743,   452,   287,   450,   755,   767,   740,
     742,   741,   743,   451,   287,     0,   378,     0,   377,     0,
       0,     0,     0,   339,     0,   320,     0,   767,     0,    67,
     347,   125,   126,   127,   653,   345,     0,   767,     0,     0,
       0,   747,   748,    64,   740,   741,   287,    42,   270,   212,
      48,   219,     0,     0,   733,     0,   294,   290,   767,   740,
     741,   755,   740,   741,   732,   324,   757,   272,   279,   274,
     281,   329,    24,     0,   252,    11,    34,     0,   767,   218,
      22,    90,    18,   317,   760,     0,    98,   744,   113,   755,
     740,   741,   684,   612,     0,   588,     0,   591,     0,   596,
     593,     0,     0,   597,   232,     0,   398,   390,   392,   755,
     395,   388,     0,   666,   725,   658,   660,     0,     0,   676,
     698,     0,   665,   708,   709,     0,   679,   719,     0,   681,
     723,   254,   375,   351,   370,   767,   767,   571,   663,     0,
     256,   352,   456,   460,     0,   457,   464,   442,   300,     0,
       0,   102,    96,     0,     0,     0,     0,     0,   767,     0,
       0,     0,     0,     0,   449,    80,     0,   455,   279,     0,
       0,   276,   448,    78,   275,   313,   767,   379,   767,   337,
     381,    68,   380,   338,   473,     0,     0,   372,     0,     0,
     744,   319,   755,   740,   741,     0,     0,     0,     0,   125,
     126,   129,   755,     0,   755,     0,   444,    73,   139,   735,
     295,   285,     0,     0,   455,     0,     0,   755,   767,   755,
     268,   106,   455,   684,   436,   439,   613,   617,   618,   619,
     610,   620,   590,   592,   599,   595,   601,   755,   397,     0,
     694,     0,   724,   711,   668,   755,   696,   669,   669,   705,
     710,   767,   669,   716,   669,   693,   669,     0,   572,   573,
     767,   574,   371,   373,     0,    12,    14,   578,     0,   767,
      74,   767,     0,   298,    38,    94,    50,   271,   740,   741,
     755,   740,   741,   565,   569,     0,     0,     0,   510,   755,
     507,     0,   564,    59,   488,   490,   492,   495,   545,   550,
     551,   552,   555,   556,   557,   558,   559,   561,   560,   562,
     563,     0,    47,   216,    46,   217,    81,   758,     0,    44,
     214,    45,   215,    79,   374,     0,     0,     0,   382,   384,
       0,    69,   474,     0,   343,     0,   466,     0,   342,   455,
       0,     0,     0,     0,   455,   350,   728,    65,   445,   446,
       0,   273,   280,   328,   755,     0,   614,   389,   391,   393,
     396,   659,     0,   672,     0,   674,   664,     0,   680,     0,
     677,   682,   255,     0,     0,   576,   577,     0,   257,   755,
       0,   424,   423,     0,   299,   455,   553,   554,   126,   567,
       0,   526,   512,   755,   513,   519,   755,   522,     0,   506,
       0,     0,   509,   566,     0,    60,     0,   548,   549,     0,
     494,   493,     0,     0,   253,    43,   213,   385,     0,   335,
     336,     0,   340,   467,     0,     0,   344,     0,   654,   346,
       0,   427,     0,     0,   437,   615,     0,     0,   669,   669,
     669,   669,     0,   575,     0,   647,   648,   310,     0,   767,
     767,   422,   755,     0,   693,   406,   701,   702,   767,   713,
     406,   406,   404,   461,   465,   755,   525,   504,   517,   529,
     514,   505,     0,   520,   538,   602,   755,   531,   534,   537,
     543,   544,   533,   541,   759,   489,   491,   546,   547,   568,
     503,   499,   602,   755,   755,   755,   755,   755,   755,     0,
       0,   479,   482,   486,   485,   487,     0,   468,   767,   348,
     438,     0,     0,     0,     0,   394,   673,     0,   670,   675,
     678,   320,     0,   767,     0,   767,    13,   403,     0,   425,
       0,   407,   415,   413,     0,   700,     0,   402,     0,   418,
       0,   420,   511,   515,     0,   521,   523,   527,     0,   508,
     532,   536,   542,   540,     0,   496,   497,   498,   500,   501,
     502,   767,   475,     0,     0,   483,   469,   471,   472,   470,
     431,   755,   429,   432,   441,   440,     0,   669,   744,   319,
     755,   740,   741,   570,   426,   712,   405,   406,   406,   320,
       0,   703,   767,   406,   714,   406,   406,     0,   518,     0,
     539,   535,   530,   383,     0,   480,   481,   484,   522,     0,
     428,   616,   671,   455,     0,   410,     0,   412,   744,   319,
     401,     0,   419,     0,   416,   421,   516,     0,   524,   528,
     767,     0,   430,   406,   406,   406,   406,   477,   478,   476,
     523,   411,     0,   408,   414,   417,   406,   409
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1036, -1036, -1036,   969, -1036,    31,   741,  -298, -1036,   -31,
   -1036,   740, -1036,    70, -1036,  -291,  -387,    50, -1036, -1036,
   -1036,   418,   -29,    -7, -1036,   -15,   -79, -1036, -1036,   -30,
   -1036,  -350,  1188,   -13,  1130,  -145,    22,   -55, -1036,  -452,
     -23,  2402,  -388,  1144,   -44,   -12, -1036, -1036,     1, -1036,
    3302, -1036,  1157, -1036,  1121, -1036,   560,    43,   581,  -370,
     145,    36, -1036,  -415,  -217,   111, -1036,  -318,     9, -1036,
   -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036,   636, -1036,
   -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036, -1036,
   -1036, -1036, -1036, -1036,   492, -1036,   730,  1605,  -374, -1036,
      76,  -754, -1036,  -783,  -799,   530,   408,  -829,   272,    61,
   -1036, -1036,   389, -1036,  -835, -1036,    58,   586, -1036, -1036,
   -1036, -1036, -1036, -1036,   445, -1036, -1036,  -100,   719, -1036,
   -1036, -1036,   955, -1036, -1036, -1036, -1036,  -737, -1036,    41,
   -1036, -1036, -1036, -1036, -1036, -1036,   -92, -1036, -1036, -1036,
   -1036,   268, -1036, -1036,  -919, -1036,   260,  -454, -1035,  -996,
    -146,  -183, -1036,   140, -1036, -1036, -1036,   141, -1036,  -731,
     288, -1036, -1036,   168, -1036, -1036,   249,   983,  1026, -1036,
    1211,  1487,  1589,  1785, -1036,   807,  2041, -1036,  2079,  2090,
   -1036, -1036,   -59, -1036, -1036,  -155, -1036, -1036, -1036, -1036,
   -1036, -1036, -1036,    12, -1036, -1036, -1036, -1036,    23,  2204,
      -2,  1221,  2642,  1853, -1036, -1036,  1246,   652,     0, -1036,
    -283,  -436,  -301,  -268, -1028,  -501,  -299,  -679,  -304,   128,
     583,   169, -1036, -1036,  -616, -1036,  -674,  -699, -1004,   175,
    -290, -1036,  -627, -1036,  -646,  -537, -1036, -1036, -1036,    32,
    -426, -1036,  -266, -1036, -1036,   -85, -1036,   -50,   182,   343,
     148,   234,  -174,   -60,   -35,     8
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,    70,    71,    72,   246,   562,   997,   563,
     264,   265,   475,   266,   466,    74,   586,    75,   608,   780,
    1025,   418,    76,    77,   825,   381,   383,   384,   951,    78,
      79,   571,   252,    81,    82,   267,    83,    84,    85,   495,
      86,   219,   401,   402,   203,   204,   205,   672,   623,   207,
      88,   468,   372,    89,   221,   272,   591,   624,   792,   454,
     455,   234,   235,   223,   440,   628,   579,   580,    90,   379,
     271,   481,   693,   805,   644,   818,   816,   659,   254,    92,
      93,    94,    95,    96,    97,    98,    99,   100,   101,   334,
     337,   755,   884,   808,   945,   946,   753,   255,   637,   801,
     947,   948,   393,   727,   728,   729,   730,   540,  1206,  1152,
    1153,  1072,  1000,  1001,  1050,  1191,  1192,   103,   292,   501,
     712,   975,   844,  1054,   338,   104,   105,   335,   568,   569,
     763,   889,   572,   573,   766,   891,   957,   809,  1189,   806,
     952,  1041,  1224,  1259,  1120,  1121,  1089,   914,   915,  1032,
    1033,   916,  1020,  1010,  1013,  1014,  1015,  1016,  1017,  1166,
    1018,  1096,  1097,  1098,  1099,  1100,  1101,  1102,   917,   918,
     919,   920,   921,   756,   880,   994,   886,   106,   107,   108,
     109,   110,   111,   112,   512,   716,   113,   514,   114,   115,
     513,   515,   297,   300,   301,   506,   714,   713,   846,   976,
    1055,  1134,   850,   116,   117,   298,   118,   119,   120,   226,
     227,   123,   228,   229,   655,   817,   323,   324,   325,   326,
     864,   739,   542,   457,   544,   545,   875,   547,   565,   549,
     550,  1077,  1078,   551,   552,   553,   554,   555,  1079,  1080,
     556,   557,   558,   559,   560,   733,   421,   660,   277,   458,
     231,   126,   697,   626,   663,   658,   425,   312,   450,   451,
     787,  1022,   486,   638,   388,   269
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     122,   299,   122,   218,   218,   426,   378,   627,   639,   206,
     127,   313,   653,   448,   742,   296,   243,   385,   548,   424,
     208,   684,   263,   546,   677,   564,   592,   546,   284,   206,
     343,   567,   677,   731,   286,   128,   314,   313,   307,   541,
     208,   667,   670,   541,   950,   706,   873,   387,   230,   230,
     285,   122,   122,   585,   543,   290,   284,   419,   238,   866,
     206,   684,   980,   275,   279,   237,   287,   871,   953,   284,
     284,   284,    73,   681,    73,   268,   978,   329,   285,   382,
     866,   635,   386,   290,   327,   327,   862,   274,   278,   218,
     306,   285,   285,   285,   287,   862,   395,   405,   405,   405,
     488,   330,   206,  1193,   490,  -116,  -117,   287,   287,   287,
     625,   868,   634,  1113,  1116,  -124,   333,   476,   874,  -120,
     746,  -120,   749,   332,   270,   870,   800,   224,   224,   709,
     772,   636,   868,   502,   230,   320,   321,   585,   585,  1052,
     444,  -123,  -116,   380,   380,   509,   511,   380,  -117,  -122,
    1228,  1029,  1163,   484,  1214,   718,  -641,   436,  -119,   666,
     485,   222,   232,  -649,  1073,  -649,   273,  -650,  1218,   472,
    1070,  1024,  -121,   446,  1006,  1007,   625,  -118,   634,     3,
    -124,   263,   330,  -767,  1248,  -641,   722,   883,   504,   505,
    -116,   537,  -116,   239,  -117,   240,   581,   804,   240,  1030,
    -124,  1193,  1031,   500,   313,   322,   698,   259,   719,   241,
     122,   244,  -107,   224,   478,   538,   470,  -529,   331,   852,
     218,  1246,   218,   218,   731,   858,  -107,  -108,   494,   479,
    1214,   263,   441,  1053,   698,   245,  -115,  1148,   441,   723,
    -111,   448,  -740,   122,   459,   122,  1164,   422,  -123,   247,
     310,   240,   311,   127,   284,  1260,  -740,  1135,   122,  -120,
     122,  -120,  -114,  1024,   684,   230,  1227,   230,   821,   310,
    -113,   311,  -741,   677,   978,   677,   285,   812,  1024,  -110,
    -116,   290,  -116,   873,   268,   329,  -117,   822,  -117,   460,
     302,   462,   287,  -112,   237,   332,  1107,  1108,  -109,   331,
     519,   520,   521,   522,   236,   493,   473,   648,   866,   576,
     263,   429,   122,   284,   587,    73,   240,   122,  -124,   583,
    -124,   895,  1070,   122,  1185,   636,   313,   122,   -88,  1070,
     474,  1070,   463,  -123,   986,   285,   643,   303,  -124,   122,
     290,  -122,   464,   465,   224,   518,   224,   456,  -102,   835,
     304,   287,   640,  1082,   642,  1059,  1094,   717,   731,   717,
     731,   823,  -119,   268,   218,   380,   380,   380,   380,   641,
     524,   525,  1081,   677,  1187,   532,  -119,   122,   449,   632,
     452,   122,   698,   122,    73,   459,  -123,   336,  -123,   523,
     374,  1188,   698,  -640,  -121,   932,   934,  -118,   587,   587,
    -650,   502,   939,   941,  1103,  1070,   536,  1070,   900,   882,
    1095,   845,  1070,   546,  1070,   883,  1103,  1103,   375,   423,
     837,   633,  -640,  -642,  -643,  1103,   632,  -117,   305,   541,
     218,   983,   985,  1070,   380,   936,   988,   879,   990,   546,
     991,   872,   494,   943,   876,   632,   546,  -108,   867,   650,
     578,   459,  -642,  -643,   692,   578,   504,   505,  1044,  1220,
     546,   831,   491,   310,   441,   311,   441,   206,   284,   867,
     865,   678,   866,   632,  -645,  -111,  -741,   122,   208,  -122,
    -644,  -122,  1212,   502,   585,   340,   218,   633,  -646,  -635,
     285,   585,   585,  -638,   502,   290,  1257,  -319,   585,   585,
    -119,   632,  -119,  -645,   758,  1082,   287,   459,   779,  -644,
     862,   625,  1082,   634,  1082,   633,   704,  -646,  -635,  1009,
     963,   487,  -638,   339,  1208,  -319,  -319,   494,   485,   344,
     840,  1215,  -121,  1147,  -121,  -118,  -357,  -118,   504,   505,
     870,  1157,   974,   633,   546,   429,   684,   507,   734,   504,
     505,  1035,   673,   284,  1103,   731,   731,   677,   473,   734,
     541,   853,   866,   651,  -357,  -357,   853,   652,   428,   855,
     815,   757,  -124,   502,  -319,   285,  -111,  -741,  1114,  1117,
     290,   802,   461,  1045,   836,  -123,   661,  1124,  1082,   374,
    1082,   287,  -115,   320,   321,  1082,  -111,  1082,  -740,  -111,
    -755,   743,   374,  -111,   374,  -114,   796,  1254,   798,   240,
     480,  -121,   970,  -357,   482,   662,  1082,   375,   376,   744,
    1068,   831,  1136,  1138,  1139,  1140,   503,  -110,   504,   505,
     375,   442,   375,   471,   218,   122,   441,   122,    91,  -118,
      91,  1109,   794,    60,   218,   789,  -112,   585,   430,   632,
     841,  1229,   225,   225,   239,   459,   345,   122,   240,   632,
     828,   785,   432,  1047,  1119,   459,   377,   789,   784,   502,
     793,   437,  -119,   206,   794,  1240,   284,   791,  1058,   443,
    1060,   443,   438,   546,   208,  1061,   441,  -109,   913,    91,
      91,   633,  -110,   288,  -119,  1075,   790,   883,   285,   807,
    1074,   633,   827,   290,   225,  1084,   794,   651,  -755,   791,
     824,  1008,   240,  1172,   287,   369,   370,   371,   494,   439,
     838,   288,   507,  -121,   504,   505,   968,  1220,  -110,   225,
     225,  1173,   445,   225,   392,   403,   403,   447,   225,   531,
     784,   791,   938,   839,   284,   587,  1182,  -112,  -110,   944,
     883,  -110,   587,   587,  1132,  -110,   961,   467,   532,   587,
     587,  1232,   233,   881,   887,   218,   285,  -112,   122,  -118,
    -112,   290,   122,  1200,  -112,   502,   510,   310,   930,   311,
     632,   419,   287,   236,   960,   345,   459,   477,  -109,   536,
     537,   781,   685,   -87,  -737,   350,   351,   687,  -755,   374,
     689,   240,   962,   691,   949,   489,   949,   483,  -109,   786,
    -647,  -109,   492,   122,   538,  -109,   122,  1012,   322,   701,
     838,   497,   633,   561,   703,   516,   206,   375,   498,   834,
     504,   505,  -648,  1197,  1027,  1028,  -635,   502,  -647,  -647,
     894,  -656,   362,   363,   896,  -310,   441,   566,    91,  1115,
    1118,   570,  1074,   996,  1213,   517,  1216,   842,  1125,  1074,
    -648,  -648,   584,   786,  -635,  -635,   578,   380,  1090,   225,
     502,   225,   225,  -310,  -310,   225,   499,   225,   698,   734,
     645,    91,   122,    91,   649,   735,   736,  -647,   995,   502,
     507,   786,   504,   505,  1183,  1184,    91,  1002,    91,  1002,
     790,   938,  1048,   930,   930,   374,   654,   930,   587,  -648,
     686,  -737,   688,  -635,  1037,   690,  -737,   695,  1085,   288,
    -102,   705,  -310,   508,   771,   504,   505,  -121,  -118,   696,
    1253,  1038,  1255,   375,   646,  1074,   496,   496,   732,  1256,
    1012,  1012,   715,   122,   504,   505,  1167,  -112,  -109,  1122,
      91,   225,   225,   225,   225,    91,   225,   225,  1266,   122,
     959,    91,   737,   738,   284,    91,  1069,   741,   745,  1083,
     964,   748,   747,   847,   848,   225,   849,    91,   288,  1144,
     593,   374,   647,    46,    47,  -738,   285,   750,   669,   671,
     752,  1066,  -744,   754,   786,   122,   380,   759,   122,   762,
     770,   122,   287,  -292,   786,   669,   671,  1171,   930,   375,
     656,   769,   930,   225,  1128,    91,   773,  -320,  1167,    91,
     225,    91,  1133,   774,   930,   930,   930,  -638,   775,   788,
     930,   930,   797,   699,  -744,   225,  1168,  1145,   804,   930,
     807,   854,   856,   122,   859,  -320,  -320,   284,  1005,   861,
     122,   122,   374,  1174,   890,  -638,  -638,  1019,   657,   931,
    -293,   937,  -744,  -744,   593,   593,   122,   374,   883,   285,
     374,  1167,   860,  1249,   405,   972,   982,   887,   734,   225,
     375,   777,   786,   984,   502,   287,   734,   744,  1181,   987,
     930,   786,  1021,  1012,  -320,   375,  1142,   930,   375,  1210,
     989,  1249,  -738,  1196,  -638,  1167,   993,  -738,   999,  -744,
    -294,  -744,  1023,   345,  -740,    91,  1024,   122,  1026,  1056,
     526,   284,   527,   528,   529,   530,  1086,  1076,  1057,   778,
     218,   903,   122,   288,  -295,   225,   949,   720,  1092,   504,
     505,  1159,  1161,   285,  1143,   632,  1093,   657,   405,  1104,
    1130,   459,  1141,   757,  1131,  1137,  1051,   882,  1149,   287,
    1150,  1087,   930,  1151,  1091,  1195,  1156,   966,  1225,  1226,
     367,   368,   369,   370,   371,  1158,  1160,   276,  1110,   345,
     971,  1051,   973,   930,  1165,  1170,  1204,   633,  1209,   949,
      80,  1217,    80,  1250,  1219,   786,   358,   359,   786,  -740,
     977,  1231,  1234,  1236,    80,    80,  -741,  1241,   981,  1243,
    1247,  1251,   786,  1262,   469,   930,   700,   930,   288,   702,
     734,   526,   122,   527,   528,   529,   530,  1190,   390,   527,
     528,   529,   530,   380,   380,   366,   367,   368,   369,   370,
     371,    80,    80,   967,   407,   930,   373,   826,   885,   930,
      41,    42,    43,    44,  1051,   857,    80,  1223,   949,   315,
     316,   317,   318,   319,  1178,  1179,  1180,   979,  1235,  1237,
     725,  1071,   969,    91,  1242,    91,  1244,  1245,  1021,  1076,
    1003,    80,    80,   225,  1076,    80,  1076,  1252,  1076,   764,
      80,  1258,   574,   225,  1106,    91,   225,   761,   786,   786,
     786,  1123,   765,   526,   767,   527,   528,   529,   530,   531,
    1221,  1222,  1105,  1203,  1261,  1263,  1264,  1265,  1146,   427,
     345,   721,   420,   328,   869,  1211,  1207,  1267,   532,   225,
    1169,   431,     0,     0,   433,   434,   435,   358,   359,     0,
       0,   288,   933,   935,     0,     0,     0,     0,  1233,   940,
     942,     0,   534,     0,     0,     0,     0,     0,   535,   536,
     537,     0,  1076,     0,  1076,   795,   912,     0,     0,  1076,
     799,  1076,   803,  1230,  1111,     0,     0,   367,   368,   369,
     370,   371,   786,     0,   538,     0,     0,   539,     0,     0,
    1076,     0,   933,   935,     0,   940,   942,     0,     0,     0,
      80,     0,     0,     0,   240,     0,    91,     0,     0,   288,
      91,   593,     0,   345,   225,     0,     0,     0,   593,   593,
       0,    80,     0,    80,    80,   593,   593,    80,  1162,    80,
     358,   359,     0,    80,     0,    80,     0,   526,     0,   527,
     528,   529,   530,   531,     0,     0,     0,     0,    80,     0,
      80,    91,     0,   225,    91,     0,  1175,  1176,  1177,     0,
     577,     0,   532,     0,     0,   590,     0,   364,   365,   366,
     367,   368,   369,   370,   371,     0,   526,     0,   527,   528,
     529,   530,     0,     0,     0,     0,   534,     0,     0,     0,
       0,   912,   912,   536,   537,   912,     0,     0,  1036,     0,
       0,     0,    80,    80,    80,    80,    80,    80,    80,    80,
       0,     0,     0,    80,     0,     0,     0,    80,   538,     0,
      91,     0,     0,     0,     0,   725,     0,    80,     0,    80,
    1036,   726,    80,     0,     0,     0,   954,     0,     0,   958,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   965,     0,     0,     0,     0,     0,   674,
     676,     0,     0,     0,     0,    80,     0,    80,   276,     0,
       0,    80,    80,    80,   593,     0,     0,     0,     0,     0,
       0,    91,   225,     0,     0,     0,     0,    80,     0,     0,
       0,     0,     0,     0,     0,     0,   912,    91,     0,     0,
     912,     0,     0,     0,     0,   676,     0,   102,   276,   102,
       0,     0,   912,   912,   912,     0,    80,    80,   912,   912,
       0,   102,   102,     0,     0,     0,     0,   912,     0,  1064,
       0,    80,     0,    91,     0,     0,    91,     0,     0,    91,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   102,   102,
       0,     0,     0,     0,     0,     0,     0,    80,     0,     0,
     740,     0,     0,   102,     0,     0,     0,  1039,   912,     0,
    1040,    91,     0,  1042,     0,   912,     0,    80,    91,    91,
    1046,     0,     0,  1049,     0,     0,   768,     0,   102,   102,
       0,     0,   102,     0,    91,     0,     0,   102,     0,     0,
       0,   526,  1155,   527,   528,   529,   530,   531,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   532,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     912,     0,     0,     0,     0,    91,     0,     0,     0,     0,
     534,     0,     0,   922,     0,     0,   535,   536,   537,     0,
      91,   912,     0,     0,     0,     0,     0,  1129,     0,   225,
       0,     0,     0,     0,     0,     0,  1205,     0,     0,     0,
       0,     0,   538,     0,   829,   539,   830,     0,     0,     0,
       0,     0,     0,   912,     0,   912,   923,   843,     0,   676,
       0,   276,     0,     0,     0,     0,     0,   102,     0,   225,
     225,     0,     0,     0,     0,    80,     0,    80,     0,     0,
       0,     0,     0,   912,     0,    80,     0,   912,   102,     0,
     102,   102,     0,     0,   102,    80,   102,    80,    80,     0,
     102,     0,   102,     0,     0,   125,     0,   125,   863,     0,
      91,     0,  1194,     0,     0,   102,     0,   102,     0,     0,
       0,     0,     0,     0,     0,     0,   878,     0,     0,     0,
       0,    80,     0,     0,     0,     0,     0,     0,   922,   922,
     893,     0,   922,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   125,   125,     0,   676,
     291,     0,     0,     0,     0,     0,     0,     0,     0,   102,
     102,   102,   102,   102,   102,   102,   102,     0,     0,   956,
     102,   923,   923,     0,   102,   923,     0,     0,   291,     0,
       0,     0,     0,     0,   102,     0,   102,     0,     0,   102,
       0,   396,   406,   406,     0,     0,     0,     0,    80,     0,
       0,     0,    80,    80,     0,     0,    80,     0,     0,     0,
      80,    80,     0,     0,     0,     0,     0,    80,    80,     0,
       0,     0,   102,     0,   102,     0,     0,     0,   102,   102,
     102,     0,     0,   922,     0,     0,     0,   922,     0,     0,
       0,     0,     0,    80,   102,    80,    80,     0,     0,   922,
     922,   922,     0,  1004,     0,   922,   922,     0,     0,     0,
       0,     0,     0,     0,   922,     0,     0,     0,     0,     0,
       0,     0,     0,   102,   102,     0,   923,     0,     0,   526,
     923,   527,   528,   529,   530,   531,     0,     0,   102,     0,
       0,     0,   923,   923,   923,     0,     0,     0,   923,   923,
       0,     0,     0,     0,   532,   125,     0,   923,     0,     0,
       0,     0,    80,     0,     0,   922,  1043,     0,   533,     0,
       0,     0,   922,     0,   102,     0,     0,     0,   534,     0,
       0,     0,     0,   276,   535,   536,   537,     0,   125,     0,
     125,     0,     0,   526,   102,   527,   528,   529,   530,   531,
       0,     0,     0,   125,     0,   125,     0,     0,   923,     0,
     538,     0,     0,   539,     0,   923,    80,     0,   532,     0,
       0,     0,     0,    80,    80,     0,   291,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   922,     0,    80,
       0,     0,   534,     0,     0,     0,     0,     0,   535,   536,
     537,     0,     0,     0,     0,  1127,     0,   125,   922,     0,
       0,     0,   125,     0,     0,     0,     0,     0,   125,     0,
       0,     0,   125,     0,   538,    80,     0,   539,    80,     0,
     923,    80,     0,     0,   125,   291,     0,   594,     0,     0,
     922,     0,   922,     0,     0,     0,   121,     0,   121,     0,
       0,   923,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     922,     0,   125,    80,   922,     0,   125,     0,   125,     0,
      80,    80,   102,   923,   102,   923,     0,  1186,     0,     0,
       0,     0,   102,     0,     0,     0,    80,   121,   121,     0,
       0,   289,   102,     0,   102,   102,     0,   924,     0,     0,
       0,     0,     0,   923,     0,     0,     0,   923,     0,     0,
       0,   594,   594,    23,    24,    25,    26,     0,     0,   289,
       0,     0,     0,     0,     0,     0,     0,     0,   102,    32,
      33,    34,   394,   404,   404,   404,     0,    80,     0,    41,
      42,    43,    44,    45,     0,     0,     0,     0,     0,     0,
       0,     0,    80,     0,     0,     0,     0,     0,     0,     0,
       0,    80,   125,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     291,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      58,    59,    60,    61,    62,    63,    64,    65,    66,   925,
       0,    80,    80,     0,     0,   102,     0,     0,     0,   102,
     102,     0,     0,   102,     0,     0,     0,   102,   102,   282,
       0,     0,   924,   924,   102,   102,   924,     0,     0,     0,
       0,     0,     0,     0,    87,     0,    87,     0,     0,     0,
       0,     0,    80,     0,     0,     0,   121,     0,     0,     0,
     102,     0,   102,   102,   345,   346,   347,   348,   349,   350,
     351,   352,     0,   354,   355,   291,     0,     0,     0,     0,
       0,   358,   359,     0,     0,     0,     0,     0,     0,   121,
       0,   121,     0,     0,     0,    87,    87,     0,     0,     0,
       0,     0,     0,     0,   121,     0,   121,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   362,   363,   364,   365,
     366,   367,   368,   369,   370,   371,     0,   289,     0,   102,
     125,     0,   125,     0,   925,   925,     0,   924,   925,     0,
     391,   924,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   125,   924,   924,   924,     0,     0,   121,   924,
     924,     0,     0,   121,     0,     0,     0,     0,   924,   121,
       0,     0,     0,   121,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   102,     0,   121,   289,     0,     0,     0,
     102,   102,     0,     0,     0,     0,     0,     0,   291,     0,
       0,     0,     0,     0,     0,   926,   102,   851,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   924,
       0,     0,     0,   121,     0,     0,   924,   121,     0,   121,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   925,
       0,     0,   102,   925,     0,   102,     0,     0,   102,     0,
       0,     0,     0,     0,    87,   925,   925,   925,     0,     0,
       0,   925,   925,   125,     0,     0,   291,   125,   594,     0,
     925,     0,     0,     0,     0,   594,   594,     0,     0,     0,
       0,     0,   594,   594,   124,     0,   124,    87,     0,    87,
     102,   924,     0,     0,     0,     0,     0,   102,   102,     0,
       0,     0,    87,     0,    87,     0,     0,     0,   125,     0,
       0,   125,   924,   102,     0,     0,     0,     0,     0,     0,
       0,   925,     0,   121,     0,     0,     0,     0,   925,     0,
     926,   926,     0,     0,   926,   124,   124,     0,     0,     0,
       0,   289,     0,     0,   924,     0,   924,     0,   345,  -768,
    -768,  -768,  -768,   350,   351,     0,    87,  -768,  -768,     0,
       0,    87,     0,     0,   102,   358,   359,    87,     0,     0,
       0,    87,     0,     0,   924,     0,     0,   125,   924,   102,
       0,     0,     0,    87,     0,     0,   588,     0,   102,     0,
       0,     0,     0,   925,     0,     0,     0,     0,     0,     0,
     362,   363,   364,   365,   366,   367,   368,   369,   370,   371,
       0,     0,     0,     0,   925,     0,     0,     0,     0,     0,
       0,    87,     0,     0,     0,    87,   289,    87,   102,   102,
       0,   594,     0,     0,     0,   926,     0,     0,   125,   926,
       0,     0,     0,     0,     0,     0,   925,     0,   925,     0,
       0,   926,   926,   926,   125,     0,     0,   926,   926,     0,
       0,   927,     0,     0,     0,     0,   926,     0,     0,   102,
     588,   588,     0,     0,     0,     0,   925,     0,     0,     0,
     925,   121,     0,   121,     0,     0,  1067,     0,     0,     0,
     125,     0,     0,   125,   124,     0,   125,     0,     0,   928,
       0,     0,     0,   121,     0,     0,     0,     0,     0,     0,
     929,     0,     0,     0,     0,     0,     0,   926,     0,     0,
       0,    87,     0,     0,   926,     0,     0,   124,     0,   124,
       0,     0,     0,     0,     0,     0,     0,     0,   125,     0,
       0,     0,   124,     0,   124,   125,   125,     0,     0,   289,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   125,     0,     0,     0,     0,     0,     0,     0,   406,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   927,   927,     0,   926,
     927,     0,     0,     0,     0,     0,   124,     0,     0,     0,
       0,   124,     0,     0,     0,     0,     0,   124,     0,     0,
     926,   124,   125,     0,   121,     0,     0,   289,   121,     0,
       0,     0,     0,   124,   928,   928,   124,   125,   928,     0,
       0,     0,     0,     0,     0,   929,   929,     0,     0,   929,
       0,     0,   926,   406,   926,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   121,
       0,   124,   121,     0,     0,   124,     0,   124,     0,     0,
       0,     0,   926,     0,     0,     0,   926,     0,     0,    87,
       0,    87,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   927,     0,     0,     0,   927,     0,     0,     0,     0,
       0,    87,     0,     0,     0,     0,     0,   927,   927,   927,
     124,   124,     0,   927,   927,     0,     0,   125,     0,     0,
       0,     0,   927,     0,     0,     0,     0,     0,   121,   928,
       0,     0,     0,   928,     0,     0,     0,     0,     0,     0,
     929,     0,     0,     0,   929,   928,   928,   928,     0,     0,
       0,   928,   928,     0,     0,     0,   929,   929,   929,     0,
     928,   124,   929,   929,     0,     0,     0,     0,     0,     0,
       0,   929,     0,   927,     0,     0,     0,     0,     0,     0,
     927,     0,     0,     0,     0,     0,     0,     0,     0,   121,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   121,     0,     0,     0,     0,
       0,   928,    87,     0,     0,     0,    87,   588,   928,     0,
       0,     0,   929,     0,   588,   588,     0,     0,     0,   929,
       0,   588,   588,     0,     0,     0,     0,  1065,     0,     0,
       0,   121,     0,     0,   121,   927,     0,   121,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    87,     0,     0,
      87,     0,     0,     0,     0,  -767,   927,     0,     0,     0,
       0,     0,     0,  -767,  -767,  -767,     0,     0,  -767,  -767,
    -767,     0,  -767,   928,     0,     0,     0,     0,     0,   121,
    -767,  -767,  -767,     0,   929,     0,   121,   121,   927,     0,
     927,     0,  -767,  -767,   928,  -767,  -767,  -767,  -767,  -767,
       0,     0,   121,     0,     0,   929,     0,     0,     0,   124,
     404,   124,     0,     0,     0,     0,    87,     0,   927,     0,
       0,     0,   927,  -767,     0,     0,   928,     0,   928,     0,
       0,   124,     0,     0,     0,     0,     0,   929,     0,   929,
       0,     0,     0,     0,     0,     0,     0,     0,   220,   220,
       0,  -767,  -767,   121,     0,     0,   928,     0,     0,     0,
     928,     0,     0,     0,     0,     0,     0,   929,   121,     0,
     588,   929,     0,     0,     0,  -767,     0,    87,     0,     0,
     253,   256,   257,   258,   404,     0,     0,   220,   220,     0,
       0,     0,     0,    87,     0,     0,     0,     0,  -767,  -767,
     308,   309,     0,   236,  -767,     0,  -767,     0,  -767,     0,
       0,     0,     0,   345,   346,   347,   348,   349,   350,   351,
       0,     0,   354,   355,     0,  1063,     0,     0,     0,    87,
     358,   359,    87,     0,   220,    87,     0,     0,     0,     0,
       0,     0,   124,     0,     0,     0,   124,   124,     0,     0,
       0,     0,     0,     0,   124,   124,     0,     0,   121,     0,
       0,   124,   124,     0,     0,   362,   363,   364,   365,   366,
     367,   368,   369,   370,   371,     0,     0,    87,     0,     0,
       0,     0,     0,     0,    87,    87,     0,   124,     0,     0,
     124,     0,     0,  -744,     0,     0,     0,     0,     0,     0,
      87,  -744,  -744,  -744,     0,     0,  -744,  -744,  -744,     0,
    -744,     0,     0,     0,     0,     0,     0,     0,  -744,  -744,
    -744,  -744,  -744,     0,     0,     0,     0,     0,     0,     0,
    -744,  -744,     0,  -744,  -744,  -744,  -744,  -744,     0,     0,
       0,     0,   220,     0,     0,   220,   220,   220,     0,   308,
       0,    87,     0,     0,     0,     0,   124,     0,     0,     0,
       0,  -744,     0,     0,     0,   220,    87,   220,   220,     0,
    -744,  -744,  -744,  -744,  -744,  -744,  -744,  -744,  -744,  -744,
    -744,  -744,  -744,     0,     0,     0,     0,  -744,  -744,  -744,
    -744,     0,   832,  -744,     0,     0,     0,     0,     0,  -744,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     124,     0,     0,  -744,     0,     0,  -744,   124,     0,  -120,
    -744,  -744,  -744,  -744,  -744,  -744,  -744,  -744,  -744,  -744,
    -744,  -744,     0,   124,     0,     0,  -744,  -744,  -744,  -744,
       0,     0,  -744,  -744,  -744,     0,  -744,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    87,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   124,
       0,   220,   124,     0,     0,   124,   589,   595,   596,   597,
     598,   599,     0,     0,   600,   601,   602,   603,   604,   605,
     606,   607,     0,   609,     0,     0,   610,   611,   612,   613,
     614,   615,   616,   617,   618,   619,     0,     0,     0,   220,
       0,     0,     0,     0,     0,     0,     0,   124,     0,     0,
       0,     0,     0,     0,   124,   124,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     124,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     668,   668,     0,     0,     0,     0,     0,     0,     0,   668,
     220,   220,     0,     0,     0,   220,     0,   668,   668,   220,
       0,     0,     0,     0,     0,   258,     0,     0,     0,     0,
       0,   124,     0,     0,     0,     0,     0,     0,     0,     0,
     694,     0,     0,     0,     0,   668,   124,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   220,     0,     0,   220,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   220,  -635,     0,     0,     0,     0,     0,     0,     0,
    -635,  -635,  -635,     0,     0,  -635,  -635,  -635,   724,  -635,
       0,     0,     0,     0,     0,     0,     0,  -635,     0,  -635,
    -635,  -635,     0,     0,     0,     0,     0,     0,     0,  -635,
    -635,     0,  -635,  -635,  -635,  -635,  -635,     0,     0,     0,
       0,   220,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   751,     0,     0,   124,     0,   760,     0,
    -635,     0,     0,     0,     0,     0,     0,   220,     0,  -635,
    -635,  -635,  -635,  -635,  -635,  -635,  -635,  -635,  -635,  -635,
    -635,  -635,     0,     0,     0,     0,  -635,  -635,  -635,  -635,
       0,  -635,  -635,     0,     0,     0,     0,     0,  -635,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -635,     0,     0,  -635,     0,     0,  -635,  -635,
    -635,  -635,  -635,  -635,  -635,  -635,  -635,  -635,  -635,  -635,
    -635,     0,     0,     0,     0,     0,  -635,  -635,  -635,   220,
       0,  -635,  -635,  -635,     0,  -635,     0,     0,     0,   220,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      23,    24,    25,    26,     0,   220,     0,   220,     0,     0,
       0,     0,     0,     0,     0,     0,    32,    33,    34,   903,
     220,     0,   220,   904,     0,  1094,    41,    42,    43,    44,
      45,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   532,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   905,   906,     0,
       0,     0,     0,     0,     0,   907,     0,     0,   908,   220,
       0,   909,   910,     0,  1011,   536,     0,    58,    59,  1112,
      61,    62,    63,    64,    65,    66,     0,   220,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   911,
       0,   220,     0,     0,     0,     0,   282,   668,   897,     0,
     220,     0,     0,     0,   668,   668,     0,     0,     0,     0,
     220,   668,   668,     0,     0,     0,     0,  -767,     4,     0,
       5,     6,     7,     8,     9,     0,     0,     0,    10,    11,
     220,     0,     0,    12,     0,    13,    14,    15,    16,    17,
      18,    19,     0,     0,     0,     0,     0,    20,    21,    22,
      23,    24,    25,    26,   668,   668,    27,   668,   668,     0,
       0,     0,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,     0,    41,    42,    43,    44,
      45,    46,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    48,    49,     0,     0,     0,     0,     0,   992,
       0,     0,     0,     0,     0,     0,     0,    50,    51,     0,
     998,     0,     0,     0,   220,    52,     0,     0,    53,    54,
       0,    55,    56,     0,    57,     0,     0,    58,    59,    60,
      61,    62,    63,    64,    65,    66,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1034,     0,     0,    67,    68,    69,     0,
     668,     0,     0,     0,     0,     0,     0,     0,  -767,     0,
    -767,     0,     0,  -638,     0,     0,     0,   220,     0,     0,
       0,  -638,  -638,  -638,     0,     0,  -638,  -638,  -638,     0,
    -638,     0,   668,     0,   220,   776,     0,     0,  -638,     0,
    -638,  -638,  -638,     0,     0,     0,     0,     0,     0,     0,
    -638,  -638,     0,  -638,  -638,  -638,  -638,  -638,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   345,   346,
     347,   348,   349,   350,   351,   352,   353,   354,   355,   356,
     357,  -638,     0,     0,     0,   358,   359,     0,     0,     0,
    -638,  -638,  -638,  -638,  -638,  -638,  -638,  -638,  -638,  -638,
    -638,  -638,  -638,     0,     0,     0,   220,  -638,  -638,  -638,
    -638,     0,  -638,  -638,     0,     0,     0,     0,   361,  -638,
     362,   363,   364,   365,   366,   367,   368,   369,   370,   371,
       0,     0,     0,  -638,     0,     0,  -638,  -265,     0,  -638,
    -638,  -638,  -638,  -638,  -638,  -638,  -638,  -638,  -638,  -638,
    -638,  -638,     0,     0,     0,     0,     0,  -638,  -638,  -638,
       0,     0,  -638,  -638,  -638,     0,  -638,     0,     0,  -745,
       0,     0,     0,     0,     0,     0,     0,  -745,  -745,  -745,
       0,     0,  -745,  -745,  -745,     0,  -745,     0,   220,     0,
       0,   776,     0,     0,  -745,  -745,  -745,  -745,  -745,     0,
       0,     0,     0,     0,     0,   220,  -745,  -745,     0,  -745,
    -745,  -745,  -745,  -745,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   345,   346,   347,   348,   349,   350,
     351,   352,   353,   354,   355,   356,   357,  -745,     0,     0,
       0,   358,   359,     0,     0,     0,  -745,  -745,  -745,  -745,
    -745,  -745,  -745,  -745,  -745,  -745,  -745,  -745,  -745,     0,
       0,     0,     0,  -745,  -745,  -745,  -745,     0,     0,  -745,
       0,     0,     0,     0,   361,  -745,   362,   363,   364,   365,
     366,   367,   368,   369,   370,   371,     0,     0,     0,  -745,
       0,     0,  -745,     0,     0,     0,  -745,  -745,  -745,  -745,
    -745,  -745,  -745,  -745,  -745,  -745,  -745,  -745,     0,     0,
       0,     0,  -745,  -745,  -745,  -745,  -746,     0,  -745,  -745,
    -745,     0,  -745,     0,  -746,  -746,  -746,     0,     0,  -746,
    -746,  -746,     0,  -746,     0,     0,     0,     0,   877,     0,
       0,  -746,  -746,  -746,  -746,  -746,     0,     0,     0,     0,
       0,     0,     0,  -746,  -746,     0,  -746,  -746,  -746,  -746,
    -746,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   345,   346,   347,   348,   349,   350,   351,   352,   353,
     354,   355,   356,   357,  -746,     0,     0,     0,   358,   359,
       0,     0,     0,  -746,  -746,  -746,  -746,  -746,  -746,  -746,
    -746,  -746,  -746,  -746,  -746,  -746,     0,     0,     0,     0,
    -746,  -746,  -746,  -746,     0,     0,  -746,     0,     0,     0,
       0,   361,  -746,   362,   363,   364,   365,   366,   367,   368,
     369,   370,   371,     0,     0,     0,  -746,     0,     0,  -746,
       0,     0,     0,  -746,  -746,  -746,  -746,  -746,  -746,  -746,
    -746,  -746,  -746,  -746,  -746,     0,     0,     0,     0,  -746,
    -746,  -746,  -746,  -319,     0,  -746,  -746,  -746,     0,  -746,
       0,  -319,  -319,  -319,     0,     0,  -319,  -319,  -319,     0,
    -319,     0,     0,     0,     0,   888,     0,     0,  -319,     0,
    -319,  -319,  -319,     0,     0,     0,     0,     0,     0,     0,
    -319,  -319,     0,  -319,  -319,  -319,  -319,  -319,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   345,   346,
     347,   348,   349,   350,   351,   352,   353,   354,   355,   356,
     357,  -319,     0,     0,     0,   358,   359,     0,     0,     0,
    -319,  -319,  -319,  -319,  -319,  -319,  -319,  -319,  -319,  -319,
    -319,  -319,  -319,     0,     0,     0,     0,  -319,  -319,  -319,
    -319,     0,   833,  -319,     0,     0,     0,     0,   361,  -319,
     362,   363,   364,   365,   366,   367,   368,   369,   370,   371,
       0,     0,     0,  -319,     0,     0,  -319,     0,     0,  -122,
    -319,  -319,  -319,  -319,  -319,  -319,  -319,  -319,  -319,  -319,
    -319,  -319,     0,     0,     0,     0,     0,  -319,  -319,  -319,
    -454,     0,  -319,  -319,  -319,     0,  -319,     0,  -454,  -454,
    -454,     0,     0,  -454,  -454,  -454,     0,  -454,     0,     0,
       0,     0,     0,     0,     0,  -454,  -454,  -454,  -454,     0,
       0,     0,     0,     0,     0,     0,     0,  -454,  -454,     0,
    -454,  -454,  -454,  -454,  -454,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   345,   346,   347,   348,   349,
     350,   351,   352,   353,   354,   355,   356,   357,  -454,     0,
       0,     0,   358,   359,     0,     0,     0,  -454,  -454,  -454,
    -454,  -454,  -454,  -454,  -454,  -454,  -454,  -454,  -454,  -454,
       0,     0,     0,     0,  -454,  -454,  -454,  -454,     0,     0,
    -454,     0,     0,     0,     0,   361,  -454,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   371,     0,     0,     0,
    -454,     0,     0,     0,     0,     0,     0,  -454,     0,  -454,
    -454,  -454,  -454,  -454,  -454,  -454,  -454,  -454,  -454,     0,
       0,     0,     0,  -454,  -454,  -454,  -454,  -311,   236,  -454,
    -454,  -454,     0,  -454,     0,  -311,  -311,  -311,     0,     0,
    -311,  -311,  -311,     0,  -311,     0,     0,     0,     0,     0,
       0,     0,  -311,     0,  -311,  -311,  -311,     0,     0,     0,
       0,     0,     0,     0,  -311,  -311,     0,  -311,  -311,  -311,
    -311,  -311,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   345,   346,   347,   348,   349,   350,   351,   352,
     353,   354,   355,  -768,  -768,  -311,     0,     0,     0,   358,
     359,     0,     0,     0,  -311,  -311,  -311,  -311,  -311,  -311,
    -311,  -311,  -311,  -311,  -311,  -311,  -311,     0,     0,     0,
       0,  -311,  -311,  -311,  -311,     0,     0,  -311,     0,     0,
       0,     0,     0,  -311,   362,   363,   364,   365,   366,   367,
     368,   369,   370,   371,     0,     0,     0,  -311,     0,     0,
    -311,     0,     0,     0,  -311,  -311,  -311,  -311,  -311,  -311,
    -311,  -311,  -311,  -311,  -311,  -311,     0,     0,     0,     0,
       0,  -311,  -311,  -311,  -767,     0,  -311,  -311,  -311,     0,
    -311,     0,  -767,  -767,  -767,     0,     0,  -767,  -767,  -767,
       0,  -767,     0,     0,     0,     0,     0,     0,     0,  -767,
    -767,  -767,  -767,     0,     0,     0,     0,     0,     0,     0,
       0,  -767,  -767,     0,  -767,  -767,  -767,  -767,  -767,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -767,     0,     0,     0,     0,     0,     0,     0,
       0,  -767,  -767,  -767,  -767,  -767,  -767,  -767,  -767,  -767,
    -767,  -767,  -767,  -767,     0,     0,     0,     0,  -767,  -767,
    -767,  -767,     0,     0,  -767,     0,     0,     0,     0,     0,
    -767,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -767,     0,     0,     0,     0,     0,
       0,  -767,     0,  -767,  -767,  -767,  -767,  -767,  -767,  -767,
    -767,  -767,  -767,     0,     0,     0,     0,  -767,  -767,  -767,
    -767,  -326,   236,  -767,  -767,  -767,     0,  -767,     0,  -326,
    -326,  -326,     0,     0,  -326,  -326,  -326,     0,  -326,     0,
       0,     0,     0,     0,     0,     0,  -326,     0,  -326,  -326,
       0,     0,     0,     0,     0,     0,     0,     0,  -326,  -326,
       0,  -326,  -326,  -326,  -326,  -326,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -326,
       0,     0,     0,     0,     0,     0,     0,     0,  -326,  -326,
    -326,  -326,  -326,  -326,  -326,  -326,  -326,  -326,  -326,  -326,
    -326,     0,     0,     0,     0,  -326,  -326,  -326,  -326,     0,
       0,  -326,     0,     0,     0,     0,     0,  -326,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -326,     0,     0,     0,     0,     0,     0,  -326,     0,
    -326,  -326,  -326,  -326,  -326,  -326,  -326,  -326,  -326,  -326,
       0,     0,     0,     0,     0,  -326,  -326,  -326,  -744,   233,
    -326,  -326,  -326,     0,  -326,     0,  -744,  -744,  -744,     0,
       0,     0,  -744,  -744,     0,  -744,     0,     0,     0,     0,
       0,     0,     0,  -744,  -744,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -744,  -744,     0,  -744,  -744,
    -744,  -744,  -744,     0,     0,     0,     0,     0,     0,     0,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,     0,     0,     0,  -744,   358,   359,     0,
       0,     0,     0,     0,     0,  -744,  -744,  -744,  -744,  -744,
    -744,  -744,  -744,  -744,  -744,  -744,  -744,  -744,     0,     0,
       0,     0,  -744,  -744,  -744,  -744,     0,   782,  -744,     0,
     361,     0,   362,   363,   364,   365,   366,   367,   368,   369,
     370,   371,     0,     0,     0,     0,     0,     0,  -744,     0,
       0,     0,     0,     0,  -120,  -744,   240,  -744,  -744,  -744,
    -744,  -744,  -744,  -744,  -744,  -744,  -744,     0,     0,     0,
       0,  -744,  -744,  -744,  -111,  -744,     0,  -744,     0,  -744,
       0,  -744,     0,  -744,  -744,  -744,     0,     0,     0,  -744,
    -744,     0,  -744,     0,     0,     0,     0,     0,     0,     0,
    -744,  -744,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -744,  -744,     0,  -744,  -744,  -744,  -744,  -744,
       0,     0,     0,     0,     0,     0,     0,   345,   346,   347,
     348,   349,   350,   351,   352,   353,   354,   355,   356,   357,
       0,     0,     0,  -744,   358,   359,     0,     0,     0,     0,
     360,     0,  -744,  -744,  -744,  -744,  -744,  -744,  -744,  -744,
    -744,  -744,  -744,  -744,  -744,     0,     0,     0,     0,  -744,
    -744,  -744,  -744,     0,   782,  -744,     0,   361,     0,   362,
     363,   364,   365,   366,   367,   368,   369,   370,   371,     0,
       0,     0,     0,     0,     0,  -744,     0,     0,     0,     0,
       0,  -120,  -744,     0,  -744,  -744,  -744,  -744,  -744,  -744,
    -744,  -744,  -744,  -744,     0,     0,     0,     0,  -744,  -744,
    -744,  -744,  -319,     0,  -744,     0,  -744,     0,  -744,     0,
    -319,  -319,  -319,     0,     0,     0,  -319,  -319,     0,  -319,
       0,     0,     0,     0,     0,     0,     0,  -319,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -319,
    -319,     0,  -319,  -319,  -319,  -319,  -319,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -319,     0,     0,     0,     0,     0,     0,     0,     0,  -319,
    -319,  -319,  -319,  -319,  -319,  -319,  -319,  -319,  -319,  -319,
    -319,  -319,     0,     0,     0,     0,  -319,  -319,  -319,  -319,
       0,   783,  -319,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -319,     0,     0,     0,     0,     0,  -122,  -319,
       0,  -319,  -319,  -319,  -319,  -319,  -319,  -319,  -319,  -319,
    -319,     0,     0,     0,     0,     0,  -319,  -319,  -113,  -319,
       0,  -319,     0,  -319,     0,  -319,     0,  -319,  -319,  -319,
       0,     0,     0,  -319,  -319,     0,  -319,     0,     0,     0,
       0,     0,     0,     0,  -319,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -319,  -319,     0,  -319,
    -319,  -319,  -319,  -319,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -319,     0,     0,
       0,     0,     0,     0,     0,     0,  -319,  -319,  -319,  -319,
    -319,  -319,  -319,  -319,  -319,  -319,  -319,  -319,  -319,     0,
       0,     0,     0,  -319,  -319,  -319,  -319,     0,   783,  -319,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -319,
       0,     0,     0,     0,     0,  -122,  -319,     0,  -319,  -319,
    -319,  -319,  -319,  -319,  -319,  -319,  -319,  -319,     0,     0,
       0,     0,     0,  -319,  -319,  -319,     0,     0,  -319,     0,
    -319,   260,  -319,     5,     6,     7,     8,     9,  -767,  -767,
    -767,    10,    11,     0,     0,  -767,    12,     0,    13,    14,
      15,    16,    17,    18,    19,     0,     0,     0,     0,     0,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,    28,    29,   261,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,     0,    41,
      42,    43,    44,    45,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    48,    49,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      50,    51,     0,     0,     0,     0,     0,     0,    52,     0,
       0,    53,    54,     0,    55,    56,     0,    57,     0,     0,
      58,    59,    60,    61,    62,    63,    64,    65,    66,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    67,
      68,    69,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -767,   260,  -767,     5,     6,     7,     8,     9,     0,
       0,  -767,    10,    11,     0,  -767,  -767,    12,     0,    13,
      14,    15,    16,    17,    18,    19,     0,     0,     0,     0,
       0,    20,    21,    22,    23,    24,    25,    26,     0,     0,
      27,     0,     0,     0,     0,     0,    28,    29,   261,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,     0,
      41,    42,    43,    44,    45,    46,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    48,    49,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    50,    51,     0,     0,     0,     0,     0,     0,    52,
       0,     0,    53,    54,     0,    55,    56,     0,    57,     0,
       0,    58,    59,    60,    61,    62,    63,    64,    65,    66,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      67,    68,    69,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -767,   260,  -767,     5,     6,     7,     8,     9,
       0,     0,  -767,    10,    11,     0,     0,  -767,    12,  -767,
      13,    14,    15,    16,    17,    18,    19,     0,     0,     0,
       0,     0,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,    28,    29,   261,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
       0,    41,    42,    43,    44,    45,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    48,    49,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    50,    51,     0,     0,     0,     0,     0,     0,
      52,     0,     0,    53,    54,     0,    55,    56,     0,    57,
       0,     0,    58,    59,    60,    61,    62,    63,    64,    65,
      66,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    67,    68,    69,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -767,   260,  -767,     5,     6,     7,     8,
       9,     0,     0,  -767,    10,    11,     0,     0,  -767,    12,
       0,    13,    14,    15,    16,    17,    18,    19,  -767,     0,
       0,     0,     0,    20,    21,    22,    23,    24,    25,    26,
       0,     0,    27,     0,     0,     0,     0,     0,    28,    29,
     261,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,     0,    41,    42,    43,    44,    45,    46,    47,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    48,    49,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    50,    51,     0,     0,     0,     0,     0,
       0,    52,     0,     0,    53,    54,     0,    55,    56,     0,
      57,     0,     0,    58,    59,    60,    61,    62,    63,    64,
      65,    66,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    67,    68,    69,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -767,   260,  -767,     5,     6,     7,
       8,     9,     0,     0,  -767,    10,    11,     0,     0,  -767,
      12,     0,    13,    14,    15,    16,    17,    18,    19,     0,
       0,     0,     0,     0,    20,    21,    22,    23,    24,    25,
      26,     0,     0,    27,     0,     0,     0,     0,     0,    28,
      29,   261,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,     0,    41,    42,    43,    44,    45,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    48,
      49,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    50,    51,     0,     0,     0,     0,
       0,     0,    52,     0,     0,    53,    54,     0,    55,    56,
       0,    57,     0,     0,    58,    59,    60,    61,    62,    63,
      64,    65,    66,     0,     0,     0,     0,     0,     0,     0,
       0,   260,     0,     5,     6,     7,     8,     9,     0,  -767,
    -767,    10,    11,    67,    68,    69,    12,     0,    13,    14,
      15,    16,    17,    18,    19,  -767,     0,  -767,     0,     0,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,    28,    29,   261,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,     0,    41,
      42,    43,    44,    45,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    48,    49,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      50,    51,     0,     0,     0,     0,     0,     0,    52,     0,
       0,    53,    54,     0,    55,    56,     0,    57,     0,     0,
      58,    59,    60,    61,    62,    63,    64,    65,    66,     0,
       0,     0,     0,     0,     0,     0,     0,   260,     0,     5,
       6,     7,     8,     9,     0,     0,     0,    10,    11,    67,
      68,    69,    12,     0,    13,    14,    15,    16,    17,    18,
      19,  -767,     0,  -767,     0,     0,    20,    21,    22,    23,
      24,    25,    26,     0,     0,    27,     0,     0,     0,     0,
       0,    28,    29,   261,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,     0,    41,    42,    43,    44,    45,
      46,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    48,    49,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    50,    51,     0,     0,
       0,     0,     0,     0,    52,     0,     0,   262,    54,     0,
      55,    56,     0,    57,     0,     0,    58,    59,    60,    61,
      62,    63,    64,    65,    66,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    67,    68,    69,     0,     0,
       0,     0,     0,     0,     0,  -767,     0,  -767,   260,  -767,
       5,     6,     7,     8,     9,     0,     0,     0,    10,    11,
       0,     0,     0,    12,     0,    13,    14,    15,    16,    17,
      18,    19,     0,     0,     0,     0,     0,    20,    21,    22,
      23,    24,    25,    26,     0,     0,    27,     0,     0,     0,
       0,     0,    28,    29,   261,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,     0,    41,    42,    43,    44,
      45,    46,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    48,    49,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    50,    51,     0,
       0,     0,     0,     0,     0,    52,     0,     0,    53,    54,
       0,    55,    56,     0,    57,     0,     0,    58,    59,    60,
      61,    62,    63,    64,    65,    66,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    67,    68,    69,     0,
       0,     0,     0,     0,     0,     0,  -767,     0,  -767,     4,
    -767,     5,     6,     7,     8,     9,     0,     0,     0,    10,
      11,     0,     0,     0,    12,     0,    13,    14,    15,    16,
      17,    18,    19,     0,     0,     0,     0,     0,    20,    21,
      22,    23,    24,    25,    26,     0,     0,    27,     0,     0,
       0,     0,     0,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,     0,    41,    42,    43,
      44,    45,    46,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    48,    49,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    50,    51,
       0,     0,     0,     0,     0,     0,    52,     0,     0,    53,
      54,     0,    55,    56,     0,    57,     0,     0,    58,    59,
      60,    61,    62,    63,    64,    65,    66,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    67,    68,    69,
       0,     0,  -767,     0,     0,     0,     0,     0,     0,  -767,
     260,  -767,     5,     6,     7,     8,     9,     0,     0,     0,
      10,    11,     0,     0,     0,    12,     0,    13,    14,    15,
      16,    17,    18,    19,     0,     0,     0,     0,     0,    20,
      21,    22,    23,    24,    25,    26,     0,     0,    27,     0,
       0,     0,     0,     0,    28,    29,   261,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,     0,    41,    42,
      43,    44,    45,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    48,    49,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    50,
      51,     0,     0,     0,     0,     0,     0,    52,     0,     0,
      53,    54,     0,    55,    56,     0,    57,     0,     0,    58,
      59,    60,    61,    62,    63,    64,    65,    66,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    67,    68,
      69,     0,     0,  -767,     0,     0,     0,     0,     0,     0,
    -767,   260,  -767,     5,     6,     7,     8,     9,     0,     0,
    -767,    10,    11,     0,     0,     0,    12,     0,    13,    14,
      15,    16,    17,    18,    19,     0,     0,     0,     0,     0,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,    28,    29,   261,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,     0,    41,
      42,    43,    44,    45,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    48,    49,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      50,    51,     0,     0,     0,     0,     0,     0,    52,     0,
       0,    53,    54,     0,    55,    56,     0,    57,     0,     0,
      58,    59,    60,    61,    62,    63,    64,    65,    66,     0,
       0,     0,     0,     0,     0,     0,     0,   260,     0,     5,
       6,     7,     8,     9,     0,     0,     0,    10,    11,    67,
      68,    69,    12,     0,    13,    14,    15,    16,    17,    18,
      19,  -767,     0,  -767,     0,     0,    20,    21,    22,    23,
      24,    25,    26,     0,     0,    27,     0,     0,     0,     0,
       0,    28,    29,   261,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,     0,    41,    42,    43,    44,    45,
      46,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    48,    49,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    50,    51,     0,     0,
       0,     0,     0,     0,    52,     0,     0,    53,    54,     0,
      55,    56,     0,    57,     0,     0,    58,    59,    60,    61,
      62,    63,    64,    65,    66,     0,  -767,     0,     0,     0,
       0,     0,     0,     0,     0,     5,     6,     7,     0,     9,
       0,     0,     0,    10,    11,    67,    68,    69,    12,     0,
      13,    14,    15,    16,    17,    18,    19,  -767,     0,  -767,
       0,     0,    20,    21,    22,    23,    24,    25,    26,     0,
       0,   209,     0,     0,     0,     0,     0,     0,    29,     0,
       0,    32,    33,    34,    35,    36,    37,    38,    39,    40,
     210,    41,    42,    43,    44,    45,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    48,    49,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    50,    51,     0,     0,     0,     0,     0,     0,
     211,     0,     0,   212,    54,     0,    55,    56,     0,   213,
     214,   215,    58,    59,   216,    61,    62,    63,    64,    65,
      66,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     5,     6,     7,     0,     9,     0,     0,     0,    10,
      11,    67,   217,    69,    12,     0,    13,    14,    15,    16,
      17,    18,    19,     0,     0,   240,     0,     0,    20,    21,
      22,    23,    24,    25,    26,     0,     0,    27,     0,     0,
       0,     0,     0,     0,    29,     0,     0,    32,    33,    34,
      35,    36,    37,    38,    39,    40,     0,    41,    42,    43,
      44,    45,    46,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    48,    49,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    50,    51,
       0,     0,     0,     0,     0,     0,   211,     0,     0,   212,
      54,     0,    55,    56,     0,     0,     0,     0,    58,    59,
      60,    61,    62,    63,    64,    65,    66,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     5,     6,     7,
       0,     9,     0,     0,     0,    10,    11,    67,    68,    69,
      12,     0,    13,    14,    15,    16,    17,    18,    19,   310,
       0,   311,     0,     0,    20,    21,    22,    23,    24,    25,
      26,     0,     0,    27,     0,     0,     0,     0,     0,     0,
      29,     0,     0,    32,    33,    34,    35,    36,    37,    38,
      39,    40,     0,    41,    42,    43,    44,    45,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    48,
      49,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    50,    51,     0,     0,     0,     0,
       0,     0,   211,     0,     0,   212,    54,     0,    55,    56,
       0,     0,     0,     0,    58,    59,    60,    61,    62,    63,
      64,    65,    66,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     5,     6,     7,     8,     9,     0,     0,
       0,    10,    11,    67,    68,    69,    12,     0,    13,    14,
      15,    16,    17,    18,    19,     0,     0,   240,     0,     0,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,     0,    41,
      42,    43,    44,    45,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    48,    49,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      50,    51,     0,     0,     0,     0,     0,     0,    52,     0,
       0,    53,    54,     0,    55,    56,     0,    57,     0,     0,
      58,    59,    60,    61,    62,    63,    64,    65,    66,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     5,
       6,     7,     8,     9,     0,     0,     0,    10,    11,    67,
      68,    69,    12,     0,    13,    14,    15,    16,    17,    18,
      19,   517,     0,     0,     0,     0,    20,    21,    22,    23,
      24,    25,    26,     0,     0,    27,     0,     0,     0,     0,
       0,    28,    29,   261,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,     0,    41,    42,    43,    44,    45,
      46,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    48,    49,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    50,    51,     0,     0,
       0,     0,     0,     0,    52,     0,     0,    53,    54,     0,
      55,    56,     0,    57,     0,     0,    58,    59,    60,    61,
      62,    63,    64,    65,    66,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    67,    68,    69,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   517,   129,   130,
     131,   132,   133,   134,   135,   136,   137,   138,   139,   140,
     141,   142,   143,   144,   145,   146,   147,   148,   149,   150,
     151,   152,     0,     0,     0,   153,   154,   155,   408,   409,
     410,   411,   160,   161,   162,     0,     0,     0,     0,     0,
     163,   164,   165,   166,   412,   413,   414,   415,   171,    37,
      38,   416,    40,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     173,   174,   175,   176,   177,   178,   179,   180,   181,     0,
       0,   182,   183,     0,     0,     0,     0,   184,   185,   186,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   188,   189,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   190,   191,   192,   193,   194,   195,
     196,   197,   198,   199,     0,   200,   201,     0,     0,     0,
       0,     0,   202,   417,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     145,   146,   147,   148,   149,   150,   151,   152,     0,     0,
       0,   153,   154,   155,   156,   157,   158,   159,   160,   161,
     162,     0,     0,     0,     0,     0,   163,   164,   165,   166,
     167,   168,   169,   170,   171,   293,   294,   172,   295,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   173,   174,   175,   176,
     177,   178,   179,   180,   181,     0,     0,   182,   183,     0,
       0,     0,     0,   184,   185,   186,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   188,   189,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     190,   191,   192,   193,   194,   195,   196,   197,   198,   199,
       0,   200,   201,     0,     0,     0,     0,     0,   202,   129,
     130,   131,   132,   133,   134,   135,   136,   137,   138,   139,
     140,   141,   142,   143,   144,   145,   146,   147,   148,   149,
     150,   151,   152,     0,     0,     0,   153,   154,   155,   156,
     157,   158,   159,   160,   161,   162,     0,     0,     0,     0,
       0,   163,   164,   165,   166,   167,   168,   169,   170,   171,
     242,     0,   172,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   173,   174,   175,   176,   177,   178,   179,   180,   181,
       0,     0,   182,   183,     0,     0,     0,     0,   184,   185,
     186,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   188,   189,     0,     0,    59,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   190,   191,   192,   193,   194,
     195,   196,   197,   198,   199,     0,   200,   201,     0,     0,
       0,     0,     0,   202,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     145,   146,   147,   148,   149,   150,   151,   152,     0,     0,
       0,   153,   154,   155,   156,   157,   158,   159,   160,   161,
     162,     0,     0,     0,     0,     0,   163,   164,   165,   166,
     167,   168,   169,   170,   171,     0,     0,   172,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   173,   174,   175,   176,
     177,   178,   179,   180,   181,     0,     0,   182,   183,     0,
       0,     0,     0,   184,   185,   186,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   188,   189,
       0,     0,    59,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     190,   191,   192,   193,   194,   195,   196,   197,   198,   199,
       0,   200,   201,     0,     0,     0,     0,     0,   202,   129,
     130,   131,   132,   133,   134,   135,   136,   137,   138,   139,
     140,   141,   142,   143,   144,   145,   146,   147,   148,   149,
     150,   151,   152,     0,     0,     0,   153,   154,   155,   156,
     157,   158,   159,   160,   161,   162,     0,     0,     0,     0,
       0,   163,   164,   165,   166,   167,   168,   169,   170,   171,
       0,     0,   172,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   173,   174,   175,   176,   177,   178,   179,   180,   181,
       0,     0,   182,   183,     0,     0,     0,     0,   184,   185,
     186,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   188,   189,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   190,   191,   192,   193,   194,
     195,   196,   197,   198,   199,     0,   200,   201,     5,     6,
       7,     0,     9,   202,     0,     0,    10,    11,     0,     0,
       0,    12,     0,    13,    14,    15,   248,   249,    18,    19,
       0,     0,     0,     0,     0,    20,   250,   251,    23,    24,
      25,    26,     0,     0,   209,     0,     0,     0,     0,     0,
       0,   280,     0,     0,    32,    33,    34,    35,    36,    37,
      38,    39,    40,     0,    41,    42,    43,    44,    45,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   281,     0,     0,   212,    54,     0,    55,
      56,     0,     0,     0,     0,    58,    59,    60,    61,    62,
      63,    64,    65,    66,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     5,     6,     7,
       0,     9,     0,     0,   282,    10,    11,     0,     0,     0,
      12,   283,    13,    14,    15,   248,   249,    18,    19,     0,
       0,     0,     0,     0,    20,   250,   251,    23,    24,    25,
      26,     0,     0,   209,     0,     0,     0,     0,     0,     0,
     280,     0,     0,    32,    33,    34,    35,    36,    37,    38,
      39,    40,     0,    41,    42,    43,    44,    45,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   281,     0,     0,   212,    54,     0,    55,    56,
       0,     0,     0,     0,    58,    59,    60,    61,    62,    63,
      64,    65,    66,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     8,
       9,     0,     0,   282,    10,    11,     0,     0,     0,    12,
     582,    13,    14,    15,    16,    17,    18,    19,     0,     0,
       0,     0,     0,    20,    21,    22,    23,    24,    25,    26,
       0,     0,    27,     0,     0,     0,     0,     0,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,     0,    41,    42,    43,    44,    45,    46,    47,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    48,    49,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    50,    51,     0,     0,     0,     0,     0,
       0,    52,     0,     0,    53,    54,     0,    55,    56,     0,
      57,     0,     0,    58,    59,    60,    61,    62,    63,    64,
      65,    66,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     0,     9,     0,     0,     0,
      10,    11,    67,    68,    69,    12,     0,    13,    14,    15,
      16,    17,    18,    19,     0,     0,     0,     0,     0,    20,
      21,    22,    23,    24,    25,    26,     0,     0,   209,     0,
       0,     0,     0,     0,     0,    29,     0,     0,    32,    33,
      34,    35,    36,    37,    38,    39,    40,   210,    41,    42,
      43,    44,    45,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    48,    49,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    50,
      51,     0,     0,     0,     0,     0,     0,   211,     0,     0,
     212,    54,     0,    55,    56,     0,   213,   214,   215,    58,
      59,   216,    61,    62,    63,    64,    65,    66,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     8,     9,     0,     0,     0,    10,    11,    67,   217,
      69,    12,     0,    13,    14,    15,    16,    17,    18,    19,
       0,     0,     0,     0,     0,    20,    21,    22,    23,    24,
      25,    26,     0,     0,    27,     0,     0,     0,     0,     0,
      28,    29,     0,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,     0,    41,    42,    43,    44,    45,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      48,    49,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    50,    51,     0,     0,     0,
       0,     0,     0,    52,     0,     0,    53,    54,     0,    55,
      56,     0,    57,     0,     0,    58,    59,    60,    61,    62,
      63,    64,    65,    66,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     0,     9,     0,
       0,     0,    10,    11,    67,    68,    69,    12,     0,    13,
      14,    15,    16,    17,    18,    19,     0,     0,     0,     0,
       0,    20,    21,    22,    23,    24,    25,    26,     0,     0,
     209,     0,     0,     0,     0,     0,     0,    29,     0,     0,
      32,    33,    34,    35,    36,    37,    38,    39,    40,   210,
      41,    42,    43,    44,    45,    46,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    48,    49,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    50,   453,     0,     0,     0,     0,     0,     0,   211,
       0,     0,   212,    54,     0,    55,    56,     0,   213,   214,
     215,    58,    59,   216,    61,    62,    63,    64,    65,    66,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       5,     6,     7,     0,     9,     0,     0,     0,    10,    11,
      67,   217,    69,    12,     0,    13,    14,    15,   248,   249,
      18,    19,     0,     0,     0,     0,     0,    20,   250,   251,
      23,    24,    25,    26,     0,     0,   209,     0,     0,     0,
       0,     0,     0,    29,     0,     0,    32,    33,    34,    35,
      36,    37,    38,    39,    40,   210,    41,    42,    43,    44,
      45,    46,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    48,    49,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    50,    51,     0,
       0,     0,     0,     0,     0,   211,     0,     0,   212,    54,
       0,    55,    56,     0,   675,   214,   215,    58,    59,   216,
      61,    62,    63,    64,    65,    66,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     0,
       9,     0,     0,     0,    10,    11,    67,   217,    69,    12,
       0,    13,    14,    15,   248,   249,    18,    19,     0,     0,
       0,     0,     0,    20,   250,   251,    23,    24,    25,    26,
       0,     0,   209,     0,     0,     0,     0,     0,     0,    29,
       0,     0,    32,    33,    34,    35,    36,    37,    38,    39,
      40,   210,    41,    42,    43,    44,    45,    46,    47,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    48,    49,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    50,   453,     0,     0,     0,     0,     0,
       0,   211,     0,     0,   212,    54,     0,    55,    56,     0,
     675,   214,   215,    58,    59,   216,    61,    62,    63,    64,
      65,    66,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     0,     9,     0,     0,     0,
      10,    11,    67,   217,    69,    12,     0,    13,    14,    15,
     248,   249,    18,    19,     0,     0,     0,     0,     0,    20,
     250,   251,    23,    24,    25,    26,     0,     0,   209,     0,
       0,     0,     0,     0,     0,    29,     0,     0,    32,    33,
      34,    35,    36,    37,    38,    39,    40,   210,    41,    42,
      43,    44,    45,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    48,    49,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    50,
      51,     0,     0,     0,     0,     0,     0,   211,     0,     0,
     212,    54,     0,    55,    56,     0,   213,   214,     0,    58,
      59,   216,    61,    62,    63,    64,    65,    66,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     0,     9,     0,     0,     0,    10,    11,    67,   217,
      69,    12,     0,    13,    14,    15,   248,   249,    18,    19,
       0,     0,     0,     0,     0,    20,   250,   251,    23,    24,
      25,    26,     0,     0,   209,     0,     0,     0,     0,     0,
       0,    29,     0,     0,    32,    33,    34,    35,    36,    37,
      38,    39,    40,   210,    41,    42,    43,    44,    45,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      48,    49,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    50,    51,     0,     0,     0,
       0,     0,     0,   211,     0,     0,   212,    54,     0,    55,
      56,     0,     0,   214,   215,    58,    59,   216,    61,    62,
      63,    64,    65,    66,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     0,     9,     0,
       0,     0,    10,    11,    67,   217,    69,    12,     0,    13,
      14,    15,   248,   249,    18,    19,     0,     0,     0,     0,
       0,    20,   250,   251,    23,    24,    25,    26,     0,     0,
     209,     0,     0,     0,     0,     0,     0,    29,     0,     0,
      32,    33,    34,    35,    36,    37,    38,    39,    40,   210,
      41,    42,    43,    44,    45,    46,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    48,    49,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    50,    51,     0,     0,     0,     0,     0,     0,   211,
       0,     0,   212,    54,     0,    55,    56,     0,   675,   214,
       0,    58,    59,   216,    61,    62,    63,    64,    65,    66,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       5,     6,     7,     0,     9,     0,     0,     0,    10,    11,
      67,   217,    69,    12,     0,    13,    14,    15,   248,   249,
      18,    19,     0,     0,     0,     0,     0,    20,   250,   251,
      23,    24,    25,    26,     0,     0,   209,     0,     0,     0,
       0,     0,     0,    29,     0,     0,    32,    33,    34,    35,
      36,    37,    38,    39,    40,   210,    41,    42,    43,    44,
      45,    46,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    48,    49,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    50,    51,     0,
       0,     0,     0,     0,     0,   211,     0,     0,   212,    54,
       0,    55,    56,     0,     0,   214,     0,    58,    59,   216,
      61,    62,    63,    64,    65,    66,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     0,
       9,     0,     0,     0,    10,    11,    67,   217,    69,    12,
       0,    13,    14,    15,    16,    17,    18,    19,     0,     0,
       0,     0,     0,    20,    21,    22,    23,    24,    25,    26,
       0,     0,   209,     0,     0,     0,     0,     0,     0,    29,
       0,     0,    32,    33,    34,    35,    36,    37,    38,    39,
      40,     0,    41,    42,    43,    44,    45,    46,    47,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    48,    49,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    50,    51,     0,     0,     0,     0,     0,
       0,   211,     0,     0,   212,    54,     0,    55,    56,     0,
     575,     0,     0,    58,    59,    60,    61,    62,    63,    64,
      65,    66,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     0,     9,     0,     0,     0,
      10,    11,    67,   217,    69,    12,     0,    13,    14,    15,
     248,   249,    18,    19,     0,     0,     0,     0,     0,    20,
     250,   251,    23,    24,    25,    26,     0,     0,   209,     0,
       0,     0,     0,     0,     0,    29,     0,     0,    32,    33,
      34,    35,    36,    37,    38,    39,    40,     0,    41,    42,
      43,    44,    45,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    48,    49,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    50,
      51,     0,     0,     0,     0,     0,     0,   211,     0,     0,
     212,    54,     0,    55,    56,     0,   575,     0,     0,    58,
      59,    60,    61,    62,    63,    64,    65,    66,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     0,     9,     0,     0,     0,    10,    11,    67,   217,
      69,    12,     0,    13,    14,    15,   248,   249,    18,    19,
       0,     0,     0,     0,     0,    20,   250,   251,    23,    24,
      25,    26,     0,     0,   209,     0,     0,     0,     0,     0,
       0,    29,     0,     0,    32,    33,    34,    35,    36,    37,
      38,    39,    40,     0,    41,    42,    43,    44,    45,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      48,    49,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    50,    51,     0,     0,     0,
       0,     0,     0,   211,     0,     0,   212,    54,     0,    55,
      56,     0,   892,     0,     0,    58,    59,    60,    61,    62,
      63,    64,    65,    66,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     0,     9,     0,
       0,     0,    10,    11,    67,   217,    69,    12,     0,    13,
      14,    15,   248,   249,    18,    19,     0,     0,     0,     0,
       0,    20,   250,   251,    23,    24,    25,    26,     0,     0,
     209,     0,     0,     0,     0,     0,     0,    29,     0,     0,
      32,    33,    34,    35,    36,    37,    38,    39,    40,     0,
      41,    42,    43,    44,    45,    46,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    48,    49,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    50,    51,     0,     0,     0,     0,     0,     0,   211,
       0,     0,   212,    54,     0,    55,    56,     0,   955,     0,
       0,    58,    59,    60,    61,    62,    63,    64,    65,    66,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       5,     6,     7,     0,     9,     0,     0,     0,    10,    11,
      67,   217,    69,    12,     0,    13,    14,    15,   248,   249,
      18,    19,     0,     0,     0,     0,     0,    20,   250,   251,
      23,    24,    25,    26,     0,     0,   209,     0,     0,     0,
       0,     0,     0,    29,     0,     0,    32,    33,    34,    35,
      36,    37,    38,    39,    40,     0,    41,    42,    43,    44,
      45,    46,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    48,    49,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    50,    51,     0,
       0,     0,     0,     0,     0,   211,     0,     0,   212,    54,
       0,    55,    56,     0,  1126,     0,     0,    58,    59,    60,
      61,    62,    63,    64,    65,    66,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     0,
       9,     0,     0,     0,    10,    11,    67,   217,    69,    12,
       0,    13,    14,    15,   248,   249,    18,    19,     0,     0,
       0,     0,     0,    20,   250,   251,    23,    24,    25,    26,
       0,     0,   209,     0,     0,     0,     0,     0,     0,    29,
       0,     0,    32,    33,    34,    35,    36,    37,    38,    39,
      40,     0,    41,    42,    43,    44,    45,    46,    47,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    48,    49,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    50,    51,     0,     0,     0,     0,     0,
       0,   211,     0,     0,   212,    54,     0,    55,    56,     0,
       0,     0,     0,    58,    59,    60,    61,    62,    63,    64,
      65,    66,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     0,     9,     0,     0,     0,
      10,    11,    67,   217,    69,    12,     0,    13,    14,    15,
      16,    17,    18,    19,     0,     0,     0,     0,     0,    20,
      21,    22,    23,    24,    25,    26,     0,     0,   209,     0,
       0,     0,     0,     0,     0,    29,     0,     0,    32,    33,
      34,    35,    36,    37,    38,    39,    40,     0,    41,    42,
      43,    44,    45,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    48,    49,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    50,
      51,     0,     0,     0,     0,     0,     0,   211,     0,     0,
     212,    54,     0,    55,    56,     0,     0,     0,     0,    58,
      59,    60,    61,    62,    63,    64,    65,    66,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     0,     9,     0,     0,     0,    10,    11,    67,   217,
      69,    12,     0,    13,    14,    15,    16,    17,    18,    19,
       0,     0,     0,     0,     0,    20,    21,    22,    23,    24,
      25,    26,     0,     0,    27,     0,     0,     0,     0,     0,
       0,    29,     0,     0,    32,    33,    34,    35,    36,    37,
      38,    39,    40,     0,    41,    42,    43,    44,    45,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      48,    49,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    50,    51,     0,     0,     0,
       0,     0,     0,   211,     0,     0,   212,    54,     0,    55,
      56,     0,     0,     0,     0,    58,    59,    60,    61,    62,
      63,    64,    65,    66,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     0,     9,     0,
       0,     0,    10,    11,    67,    68,    69,    12,     0,    13,
      14,    15,   248,   249,    18,    19,     0,     0,     0,     0,
       0,    20,   250,   251,    23,    24,    25,    26,     0,     0,
     209,     0,     0,     0,     0,     0,     0,   280,     0,     0,
      32,    33,    34,    35,    36,    37,    38,    39,    40,     0,
      41,    42,    43,    44,    45,    46,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   281,
       0,     0,   341,    54,     0,    55,    56,     0,   342,     0,
       0,    58,    59,    60,    61,    62,    63,    64,    65,    66,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     0,
       9,     0,     0,     0,    10,    11,     0,     0,     0,    12,
     282,    13,    14,    15,   248,   249,    18,    19,     0,     0,
       0,     0,     0,    20,   250,   251,    23,    24,    25,    26,
       0,     0,   209,     0,     0,     0,     0,     0,     0,   280,
       0,     0,    32,    33,    34,    35,    36,    37,    38,    39,
      40,     0,    41,    42,    43,    44,    45,    46,    47,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   389,     0,     0,    53,    54,     0,    55,    56,     0,
      57,     0,     0,    58,    59,    60,    61,    62,    63,    64,
      65,    66,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     0,     9,     0,     0,     0,    10,    11,     0,     0,
       0,    12,   282,    13,    14,    15,   248,   249,    18,    19,
       0,     0,     0,     0,     0,    20,   250,   251,    23,    24,
      25,    26,     0,     0,   209,     0,     0,     0,     0,     0,
       0,   280,     0,     0,    32,    33,    34,   397,    36,    37,
      38,   398,    40,     0,    41,    42,    43,    44,    45,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   399,
       0,     0,     0,   400,     0,     0,   212,    54,     0,    55,
      56,     0,     0,     0,     0,    58,    59,    60,    61,    62,
      63,    64,    65,    66,     0,     0,     0,     0,     0,     0,
       5,     6,     7,     0,     9,     0,     0,     0,    10,    11,
       0,     0,     0,    12,   282,    13,    14,    15,   248,   249,
      18,    19,     0,     0,     0,     0,     0,    20,   250,   251,
      23,    24,    25,    26,     0,     0,   209,     0,     0,     0,
       0,     0,     0,   280,     0,     0,    32,    33,    34,   397,
      36,    37,    38,   398,    40,     0,    41,    42,    43,    44,
      45,    46,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   400,     0,     0,   212,    54,
       0,    55,    56,     0,     0,     0,     0,    58,    59,    60,
      61,    62,    63,    64,    65,    66,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     0,     9,     0,     0,     0,
      10,    11,     0,     0,     0,    12,   282,    13,    14,    15,
     248,   249,    18,    19,     0,     0,     0,     0,     0,    20,
     250,   251,    23,    24,    25,    26,     0,     0,   209,     0,
       0,     0,     0,     0,     0,   280,     0,     0,    32,    33,
      34,    35,    36,    37,    38,    39,    40,     0,    41,    42,
      43,    44,    45,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   281,     0,     0,
     341,    54,     0,    55,    56,     0,     0,     0,     0,    58,
      59,    60,    61,    62,    63,    64,    65,    66,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     0,     9,     0,
       0,     0,    10,    11,     0,     0,     0,    12,   282,    13,
      14,    15,   248,   249,    18,    19,     0,     0,     0,     0,
       0,    20,   250,   251,    23,    24,    25,    26,     0,     0,
     209,     0,     0,     0,     0,     0,     0,   280,     0,     0,
      32,    33,    34,    35,    36,    37,    38,    39,    40,     0,
      41,    42,    43,    44,    45,    46,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1062,
       0,     0,   212,    54,     0,    55,    56,     0,     0,     0,
       0,    58,    59,    60,    61,    62,    63,    64,    65,    66,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     0,
       9,     0,     0,     0,    10,    11,     0,     0,     0,    12,
     282,    13,    14,    15,   248,   249,    18,    19,     0,     0,
       0,     0,     0,    20,   250,   251,    23,    24,    25,    26,
       0,     0,   209,     0,     0,     0,     0,     0,     0,   280,
       0,     0,    32,    33,    34,    35,    36,    37,    38,    39,
      40,     0,    41,    42,    43,    44,    45,    46,    47,    23,
      24,    25,    26,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    32,    33,    34,   903,     0,
       0,     0,   904,     0,     0,    41,    42,    43,    44,    45,
       0,  1154,     0,     0,   212,    54,     0,    55,    56,     0,
       0,     0,     0,    58,    59,    60,    61,    62,    63,    64,
      65,    66,     0,     0,     0,     0,   905,   906,     0,     0,
       0,     0,     0,     0,   907,     0,     0,   908,     0,     0,
     909,   910,   282,  1011,     0,     0,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    23,    24,    25,    26,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   911,     0,
       0,    32,    33,    34,   903,   282,     0,     0,   904,     0,
       0,    41,    42,    43,    44,    45,     0,     0,     0,   240,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   905,   906,     0,     0,     0,     0,     0,     0,
     907,     0,     0,   908,     0,     0,   909,   910,     0,  1088,
       0,     0,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    23,    24,    25,    26,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   911,     0,     0,    32,    33,    34,
     903,   282,     0,     0,   904,     0,     0,    41,    42,    43,
      44,    45,     0,     0,    23,    24,    25,    26,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      32,    33,    34,   903,     0,     0,     0,   904,   905,   906,
      41,    42,    43,    44,    45,     0,   907,     0,     0,   908,
       0,     0,   909,   910,     0,  1011,     0,     0,    58,    59,
      60,    61,    62,    63,    64,    65,    66,     0,     0,     0,
       0,   905,   906,     0,     0,     0,     0,     0,     0,   907,
     911,     0,   908,     0,     0,   909,   910,   282,     0,     0,
       0,    58,    59,    60,    61,    62,    63,    64,    65,    66,
     620,   621,     0,     0,   622,     0,     0,     0,     0,     0,
       0,     0,     0,   911,     0,     0,     0,     0,     0,     0,
     282,     0,     0,   173,   174,   175,   176,   177,   178,   179,
     180,   181,     0,     0,   182,   183,     0,     0,     0,     0,
     184,   185,   186,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   190,   191,   192,
     193,   194,   195,   196,   197,   198,   199,     0,   200,   201,
     629,   630,     0,     0,   631,   202,   236,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   173,   174,   175,   176,   177,   178,   179,
     180,   181,     0,     0,   182,   183,     0,     0,     0,     0,
     184,   185,   186,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   190,   191,   192,
     193,   194,   195,   196,   197,   198,   199,     0,   200,   201,
     679,   621,     0,     0,   680,   202,   236,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   173,   174,   175,   176,   177,   178,   179,
     180,   181,     0,     0,   182,   183,     0,     0,     0,     0,
     184,   185,   186,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   190,   191,   192,
     193,   194,   195,   196,   197,   198,   199,     0,   200,   201,
     682,   630,     0,     0,   683,   202,   236,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   173,   174,   175,   176,   177,   178,   179,
     180,   181,     0,     0,   182,   183,     0,     0,     0,     0,
     184,   185,   186,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   190,   191,   192,
     193,   194,   195,   196,   197,   198,   199,     0,   200,   201,
     707,   621,     0,     0,   708,   202,   236,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   173,   174,   175,   176,   177,   178,   179,
     180,   181,     0,     0,   182,   183,     0,     0,     0,     0,
     184,   185,   186,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   190,   191,   192,
     193,   194,   195,   196,   197,   198,   199,     0,   200,   201,
     710,   630,     0,     0,   711,   202,   236,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   173,   174,   175,   176,   177,   178,   179,
     180,   181,     0,     0,   182,   183,     0,     0,     0,     0,
     184,   185,   186,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   190,   191,   192,
     193,   194,   195,   196,   197,   198,   199,     0,   200,   201,
     810,   621,     0,     0,   811,   202,   236,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   173,   174,   175,   176,   177,   178,   179,
     180,   181,     0,     0,   182,   183,     0,     0,     0,     0,
     184,   185,   186,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   190,   191,   192,
     193,   194,   195,   196,   197,   198,   199,     0,   200,   201,
     813,   630,     0,     0,   814,   202,   236,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   173,   174,   175,   176,   177,   178,   179,
     180,   181,     0,     0,   182,   183,     0,     0,     0,     0,
     184,   185,   186,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   190,   191,   192,
     193,   194,   195,   196,   197,   198,   199,     0,   200,   201,
     819,   621,     0,     0,   820,   202,   236,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   173,   174,   175,   176,   177,   178,   179,
     180,   181,     0,     0,   182,   183,     0,     0,     0,     0,
     184,   185,   186,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   190,   191,   192,
     193,   194,   195,   196,   197,   198,   199,     0,   200,   201,
     664,   630,     0,     0,   665,   202,   236,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   173,   174,   175,   176,   177,   178,   179,
     180,   181,     0,     0,   182,   183,     0,     0,     0,     0,
     184,   185,   186,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   190,   191,   192,
     193,   194,   195,   196,   197,   198,   199,     0,   200,   201,
     898,   621,     0,     0,   899,   202,   236,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   173,   174,   175,   176,   177,   178,   179,
     180,   181,     0,     0,   182,   183,     0,     0,     0,     0,
     184,   185,   186,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   190,   191,   192,
     193,   194,   195,   196,   197,   198,   199,     0,   200,   201,
     901,   630,     0,     0,   902,   202,   236,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   173,   174,   175,   176,   177,   178,   179,
     180,   181,     0,     0,   182,   183,     0,     0,     0,     0,
     184,   185,   186,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   190,   191,   192,
     193,   194,   195,   196,   197,   198,   199,     0,   200,   201,
    1198,   621,     0,     0,  1199,   202,   236,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   173,   174,   175,   176,   177,   178,   179,
     180,   181,     0,     0,   182,   183,     0,     0,     0,     0,
     184,   185,   186,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   190,   191,   192,
     193,   194,   195,   196,   197,   198,   199,     0,   200,   201,
    1201,   630,     0,     0,  1202,   202,   236,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   173,   174,   175,   176,   177,   178,   179,
     180,   181,     0,     0,   182,   183,     0,     0,     0,     0,
     184,   185,   186,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   190,   191,   192,
     193,   194,   195,   196,   197,   198,   199,     0,   200,   201,
    1238,   621,     0,     0,  1239,   202,   236,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   173,   174,   175,   176,   177,   178,   179,
     180,   181,     0,     0,   182,   183,     0,     0,     0,     0,
     184,   185,   186,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   190,   191,   192,
     193,   194,   195,   196,   197,   198,   199,     0,   200,   201,
     664,   630,     0,     0,   665,   202,   236,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   173,   174,   175,   176,   177,   178,   179,
     180,   181,     0,     0,   182,   183,     0,     0,     0,     0,
     184,   185,   186,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   190,   191,   192,
     193,   194,   195,   196,   197,   198,   199,     0,   200,   201,
       0,     0,     0,     0,     0,   202
};

static const yytype_int16 yycheck[] =
{
       2,    60,     4,    16,    17,   105,    91,   377,   382,     8,
       2,    71,   400,   230,   551,    59,    28,    96,   322,   104,
       8,   447,    53,   322,   439,   326,   344,   326,    57,    28,
      85,   329,   447,   534,    57,     4,    71,    97,    68,   322,
      28,   428,   429,   326,   798,   497,   745,    97,    16,    17,
      57,    53,    54,   344,   322,    57,    85,   101,    22,   738,
      59,   487,   861,    55,    56,    22,    57,   741,   805,    98,
      99,   100,     2,   443,     4,    53,   859,    77,    85,    94,
     759,   379,    97,    85,    76,    77,   732,    55,    56,   102,
      67,    98,    99,   100,    85,   741,    98,    99,   100,   101,
     274,    26,   101,  1131,   278,    25,    25,    98,    99,   100,
     376,   738,   378,  1032,  1033,    25,    80,   262,   745,    25,
     556,    13,   558,    80,    54,   741,    27,    16,    17,   499,
     582,    13,   759,    66,   102,    37,    38,   428,   429,    29,
     225,    25,    13,    93,    94,   300,   301,    97,    13,    25,
    1185,    97,    52,   146,  1158,    66,    68,   216,    25,   425,
     153,    16,    17,    99,   999,    99,    55,    99,  1164,   254,
     999,   100,    25,    99,   905,   906,   442,    25,   444,     0,
      13,   212,    26,   126,  1219,    97,    66,    15,   121,   122,
     126,   109,   126,   148,   126,   153,   341,    25,   153,   145,
     126,  1229,   148,   288,   264,   148,   472,    56,   119,    27,
     212,    29,   146,   102,   264,   133,   247,   146,   143,   152,
     233,  1217,   235,   236,   725,   726,   146,   146,   283,   264,
    1234,   262,   224,   123,   500,   143,   146,  1072,   230,   119,
     146,   458,   148,   245,   236,   247,   146,   102,    13,   143,
     151,   153,   153,   245,   283,  1251,   148,  1056,   260,   151,
     262,   153,   146,   100,   690,   233,  1185,   235,   656,   151,
     146,   153,   148,   688,  1057,   690,   283,   647,   100,   146,
     151,   283,   153,   982,   262,   285,   151,   657,   153,   239,
     152,   241,   283,   146,   251,   252,  1027,  1028,   146,   143,
     315,   316,   317,   318,   148,   282,    99,   392,   987,   339,
     341,    99,   314,   342,   344,   245,   153,   319,   151,   342,
     153,   773,  1151,   325,   146,    13,   386,   329,   126,  1158,
     260,  1160,    54,   126,   871,   342,   386,   152,   126,   341,
     342,    13,    64,    65,   233,   314,   235,   236,   146,    99,
     152,   342,   383,   999,   385,   982,    58,   512,   859,   514,
     861,   659,    13,   341,   377,   315,   316,   317,   318,   384,
     320,   321,   999,   788,  1128,    77,   126,   379,   233,   378,
     235,   383,   648,   385,   314,   377,   151,    28,   153,   319,
      68,  1128,   658,    68,    13,   782,   783,    13,   428,   429,
      99,    66,   789,   790,  1020,  1234,   108,  1236,   778,     9,
     112,   712,  1241,   712,  1243,    15,  1032,  1033,    96,    97,
     688,   378,    97,    68,    68,  1041,   425,   126,   152,   712,
     443,   867,   868,  1262,   384,   785,   872,   755,   874,   738,
     876,   745,   497,   793,   748,   444,   745,   146,   738,   399,
     339,   443,    97,    97,   466,   344,   121,   122,   146,   124,
     759,   678,   280,   151,   456,   153,   458,   466,   497,   759,
     738,   439,  1151,   472,    68,    25,   148,   479,   466,   151,
      68,   153,  1156,    66,   775,   146,   499,   444,    68,    68,
     497,   782,   783,    68,    66,   497,  1250,    68,   789,   790,
     151,   500,   153,    97,   564,  1151,   497,   499,   593,    97,
    1156,   777,  1158,   779,  1160,   472,   484,    97,    97,   907,
     818,   146,    97,   126,  1151,    96,    97,   582,   153,   126,
     704,  1158,   151,  1070,   153,   151,    68,   153,   121,   122,
    1156,  1078,   843,   500,   843,    99,   972,   119,   540,   121,
     122,   938,   124,   582,  1170,  1056,  1057,   972,    99,   551,
     843,   716,  1241,    52,    96,    97,   721,    56,    99,   152,
     649,   563,   126,    66,   145,   582,   126,   148,  1032,  1033,
     582,   641,   239,   957,    99,   126,    68,  1041,  1234,    68,
    1236,   582,   146,    37,    38,  1241,   146,  1243,   148,   149,
     144,    34,    68,   153,    68,   146,   637,  1234,   639,   153,
     267,   126,    99,   145,   271,    97,  1262,    96,    97,    52,
     994,   838,  1058,  1059,  1060,  1061,   119,    25,   121,   122,
      96,    97,    96,    97,   647,   637,   628,   639,     2,   126,
       4,  1029,   634,   112,   657,    99,    25,   938,   146,   648,
     705,   146,    16,    17,   148,   647,    77,   659,   153,   658,
     672,   625,    56,   961,  1038,   657,   145,    99,   625,    66,
     634,   126,   126,   672,   666,  1212,   705,   634,   982,   145,
     984,   145,   100,   982,   672,   989,   678,    25,   780,    53,
      54,   648,   146,    57,   126,   999,    99,    15,   705,    17,
     999,   658,   666,   705,    68,  1003,   698,    52,   149,   666,
     660,    56,   153,    34,   705,   136,   137,   138,   773,   146,
     688,    85,   119,   126,   121,   122,   826,   124,   126,    93,
      94,    52,    99,    97,    98,    99,   100,   146,   102,    58,
     697,   698,    99,   693,   773,   775,  1120,   126,   146,    14,
      15,   149,   782,   783,  1052,   153,   816,   148,    77,   789,
     790,  1197,   148,   755,   756,   778,   773,   146,   770,   126,
     149,   773,   774,  1143,   153,    66,    67,   151,   780,   153,
     779,   825,   773,   148,   815,    77,   778,   149,   126,   108,
     109,   609,   449,   126,    26,    82,    83,   454,   150,    68,
     457,   153,   817,   460,   796,   144,   798,   150,   146,   627,
      68,   149,    56,   815,   133,   153,   818,   909,   148,   476,
     788,   146,   779,   126,   481,    77,   825,    96,    97,   681,
     121,   122,    68,  1137,    88,    89,    68,    66,    96,    97,
     770,   126,   129,   130,   774,    68,   838,   126,   212,  1032,
    1033,   106,  1151,   884,  1158,   151,  1160,   709,  1041,  1158,
      96,    97,   146,   681,    96,    97,   755,   817,  1014,   233,
      66,   235,   236,    96,    97,   239,   145,   241,  1144,   871,
      56,   245,   884,   247,    25,   542,   543,   145,   880,    66,
     119,   709,   121,   122,    40,    41,   260,   889,   262,   891,
      99,    99,   962,   905,   906,    68,   130,   909,   938,   145,
     149,   143,   146,   145,   945,   146,   148,   144,  1010,   283,
     146,   146,   145,   119,   581,   121,   122,   126,   126,   144,
    1234,   946,  1236,    96,    97,  1234,   284,   285,   146,  1243,
    1032,  1033,   119,   945,   121,   122,  1092,   146,   146,  1041,
     314,   315,   316,   317,   318,   319,   320,   321,  1262,   961,
     812,   325,   126,   146,   993,   329,   997,   146,   146,  1000,
     822,   146,    52,    54,    55,   339,    57,   341,   342,  1064,
     344,    68,   145,    64,    65,    26,   993,    52,   428,   429,
      10,   993,    26,     8,   812,   997,   946,   146,  1000,   144,
      44,  1003,   993,   146,   822,   445,   446,  1099,  1010,    96,
      97,   146,  1014,   377,  1045,   379,   146,    68,  1164,   383,
     384,   385,  1053,    44,  1026,  1027,  1028,    68,   126,   146,
    1032,  1033,    13,   473,    68,   399,  1095,  1068,    25,  1041,
      17,   152,   152,  1045,   146,    96,    97,  1076,   900,   146,
    1052,  1053,    68,  1112,   144,    96,    97,   909,   145,   128,
     146,   150,    96,    97,   428,   429,  1068,    68,    15,  1076,
      68,  1217,   729,  1219,  1076,   146,   146,  1069,  1070,   443,
      96,    97,   900,   146,    66,  1076,  1078,    52,  1119,   146,
    1092,   909,   910,  1185,   145,    96,    97,  1099,    96,    97,
     146,  1247,   143,  1134,   145,  1251,   100,   148,   131,   143,
     146,   145,    52,    77,   148,   479,   100,  1119,   131,   146,
      52,  1150,    54,    55,    56,    57,    52,   999,   146,   145,
    1143,    52,  1134,   497,   146,   499,  1128,   119,   146,   121,
     122,  1080,  1081,  1150,   145,  1144,   146,   145,  1150,   144,
     149,  1143,    56,  1145,   151,   146,   974,     9,   131,  1150,
     126,  1013,  1164,   146,  1016,   144,   146,   824,  1183,  1184,
     134,   135,   136,   137,   138,   146,   146,    56,  1030,    77,
     837,   999,   839,  1185,   146,   146,   131,  1144,    56,  1181,
       2,   146,     4,  1224,   146,  1013,    94,    95,  1016,   148,
     857,   120,   146,   146,    16,    17,   148,   146,   865,   146,
     146,   146,  1030,   146,   245,  1217,   475,  1219,   582,   479,
    1212,    52,  1224,    54,    55,    56,    57,    52,    98,    54,
      55,    56,    57,  1183,  1184,   133,   134,   135,   136,   137,
     138,    53,    54,   825,   100,  1247,    89,   666,   756,  1251,
      59,    60,    61,    62,  1072,   725,    68,  1181,  1250,    40,
      41,    42,    43,    44,  1116,  1117,  1118,   859,  1207,  1208,
     101,   999,   827,   637,  1213,   639,  1215,  1216,  1096,  1151,
     891,    93,    94,   647,  1156,    97,  1158,  1229,  1160,   570,
     102,  1250,   337,   657,  1026,   659,   660,   567,  1116,  1117,
    1118,  1041,   572,    52,   574,    54,    55,    56,    57,    58,
    1170,  1170,  1024,  1145,  1253,  1254,  1255,  1256,  1069,   108,
      77,   514,   101,    77,   741,  1156,  1151,  1266,    77,   693,
    1096,   210,    -1,    -1,   213,   214,   215,    94,    95,    -1,
      -1,   705,   782,   783,    -1,    -1,    -1,    -1,  1200,   789,
     790,    -1,   101,    -1,    -1,    -1,    -1,    -1,   107,   108,
     109,    -1,  1234,    -1,  1236,   635,   780,    -1,    -1,  1241,
     640,  1243,   642,  1191,  1031,    -1,    -1,   134,   135,   136,
     137,   138,  1200,    -1,   133,    -1,    -1,   136,    -1,    -1,
    1262,    -1,   832,   833,    -1,   835,   836,    -1,    -1,    -1,
     212,    -1,    -1,    -1,   153,    -1,   770,    -1,    -1,   773,
     774,   775,    -1,    77,   778,    -1,    -1,    -1,   782,   783,
      -1,   233,    -1,   235,   236,   789,   790,   239,  1085,   241,
      94,    95,    -1,   245,    -1,   247,    -1,    52,    -1,    54,
      55,    56,    57,    58,    -1,    -1,    -1,    -1,   260,    -1,
     262,   815,    -1,   817,   818,    -1,  1113,  1114,  1115,    -1,
     339,    -1,    77,    -1,    -1,   344,    -1,   131,   132,   133,
     134,   135,   136,   137,   138,    -1,    52,    -1,    54,    55,
      56,    57,    -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,
      -1,   905,   906,   108,   109,   909,    -1,    -1,   938,    -1,
      -1,    -1,   314,   315,   316,   317,   318,   319,   320,   321,
      -1,    -1,    -1,   325,    -1,    -1,    -1,   329,   133,    -1,
     884,    -1,    -1,    -1,    -1,   101,    -1,   339,    -1,   341,
     970,   107,   344,    -1,    -1,    -1,   806,    -1,    -1,   809,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   823,    -1,    -1,    -1,    -1,    -1,   438,
     439,    -1,    -1,    -1,    -1,   377,    -1,   379,   447,    -1,
      -1,   383,   384,   385,   938,    -1,    -1,    -1,    -1,    -1,
      -1,   945,   946,    -1,    -1,    -1,    -1,   399,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1010,   961,    -1,    -1,
    1014,    -1,    -1,    -1,    -1,   484,    -1,     2,   487,     4,
      -1,    -1,  1026,  1027,  1028,    -1,   428,   429,  1032,  1033,
      -1,    16,    17,    -1,    -1,    -1,    -1,  1041,    -1,   993,
      -1,   443,    -1,   997,    -1,    -1,  1000,    -1,    -1,  1003,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    53,    54,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   479,    -1,    -1,
     549,    -1,    -1,    68,    -1,    -1,    -1,   947,  1092,    -1,
     950,  1045,    -1,   953,    -1,  1099,    -1,   499,  1052,  1053,
     960,    -1,    -1,   963,    -1,    -1,   575,    -1,    93,    94,
      -1,    -1,    97,    -1,  1068,    -1,    -1,   102,    -1,    -1,
      -1,    52,  1076,    54,    55,    56,    57,    58,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    77,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1164,    -1,    -1,    -1,    -1,  1119,    -1,    -1,    -1,    -1,
     101,    -1,    -1,   780,    -1,    -1,   107,   108,   109,    -1,
    1134,  1185,    -1,    -1,    -1,    -1,    -1,  1047,    -1,  1143,
      -1,    -1,    -1,    -1,    -1,    -1,  1150,    -1,    -1,    -1,
      -1,    -1,   133,    -1,   673,   136,   675,    -1,    -1,    -1,
      -1,    -1,    -1,  1217,    -1,  1219,   780,   148,    -1,   688,
      -1,   690,    -1,    -1,    -1,    -1,    -1,   212,    -1,  1183,
    1184,    -1,    -1,    -1,    -1,   637,    -1,   639,    -1,    -1,
      -1,    -1,    -1,  1247,    -1,   647,    -1,  1251,   233,    -1,
     235,   236,    -1,    -1,   239,   657,   241,   659,   660,    -1,
     245,    -1,   247,    -1,    -1,     2,    -1,     4,   737,    -1,
    1224,    -1,  1132,    -1,    -1,   260,    -1,   262,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   755,    -1,    -1,    -1,
      -1,   693,    -1,    -1,    -1,    -1,    -1,    -1,   905,   906,
     769,    -1,   909,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    53,    54,    -1,   788,
      57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   314,
     315,   316,   317,   318,   319,   320,   321,    -1,    -1,   808,
     325,   905,   906,    -1,   329,   909,    -1,    -1,    85,    -1,
      -1,    -1,    -1,    -1,   339,    -1,   341,    -1,    -1,   344,
      -1,    98,    99,   100,    -1,    -1,    -1,    -1,   770,    -1,
      -1,    -1,   774,   775,    -1,    -1,   778,    -1,    -1,    -1,
     782,   783,    -1,    -1,    -1,    -1,    -1,   789,   790,    -1,
      -1,    -1,   377,    -1,   379,    -1,    -1,    -1,   383,   384,
     385,    -1,    -1,  1010,    -1,    -1,    -1,  1014,    -1,    -1,
      -1,    -1,    -1,   815,   399,   817,   818,    -1,    -1,  1026,
    1027,  1028,    -1,   892,    -1,  1032,  1033,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1041,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   428,   429,    -1,  1010,    -1,    -1,    52,
    1014,    54,    55,    56,    57,    58,    -1,    -1,   443,    -1,
      -1,    -1,  1026,  1027,  1028,    -1,    -1,    -1,  1032,  1033,
      -1,    -1,    -1,    -1,    77,   212,    -1,  1041,    -1,    -1,
      -1,    -1,   884,    -1,    -1,  1092,   955,    -1,    91,    -1,
      -1,    -1,  1099,    -1,   479,    -1,    -1,    -1,   101,    -1,
      -1,    -1,    -1,   972,   107,   108,   109,    -1,   245,    -1,
     247,    -1,    -1,    52,   499,    54,    55,    56,    57,    58,
      -1,    -1,    -1,   260,    -1,   262,    -1,    -1,  1092,    -1,
     133,    -1,    -1,   136,    -1,  1099,   938,    -1,    77,    -1,
      -1,    -1,    -1,   945,   946,    -1,   283,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1164,    -1,   961,
      -1,    -1,   101,    -1,    -1,    -1,    -1,    -1,   107,   108,
     109,    -1,    -1,    -1,    -1,  1044,    -1,   314,  1185,    -1,
      -1,    -1,   319,    -1,    -1,    -1,    -1,    -1,   325,    -1,
      -1,    -1,   329,    -1,   133,   997,    -1,   136,  1000,    -1,
    1164,  1003,    -1,    -1,   341,   342,    -1,   344,    -1,    -1,
    1217,    -1,  1219,    -1,    -1,    -1,     2,    -1,     4,    -1,
      -1,  1185,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1247,    -1,   379,  1045,  1251,    -1,   383,    -1,   385,    -1,
    1052,  1053,   637,  1217,   639,  1219,    -1,  1126,    -1,    -1,
      -1,    -1,   647,    -1,    -1,    -1,  1068,    53,    54,    -1,
      -1,    57,   657,    -1,   659,   660,    -1,   780,    -1,    -1,
      -1,    -1,    -1,  1247,    -1,    -1,    -1,  1251,    -1,    -1,
      -1,   428,   429,    33,    34,    35,    36,    -1,    -1,    85,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   693,    49,
      50,    51,    98,    99,   100,   101,    -1,  1119,    -1,    59,
      60,    61,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1143,   479,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     497,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   780,
      -1,  1183,  1184,    -1,    -1,   770,    -1,    -1,    -1,   774,
     775,    -1,    -1,   778,    -1,    -1,    -1,   782,   783,   139,
      -1,    -1,   905,   906,   789,   790,   909,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     2,    -1,     4,    -1,    -1,    -1,
      -1,    -1,  1224,    -1,    -1,    -1,   212,    -1,    -1,    -1,
     815,    -1,   817,   818,    77,    78,    79,    80,    81,    82,
      83,    84,    -1,    86,    87,   582,    -1,    -1,    -1,    -1,
      -1,    94,    95,    -1,    -1,    -1,    -1,    -1,    -1,   245,
      -1,   247,    -1,    -1,    -1,    53,    54,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   260,    -1,   262,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   129,   130,   131,   132,
     133,   134,   135,   136,   137,   138,    -1,   283,    -1,   884,
     637,    -1,   639,    -1,   905,   906,    -1,  1010,   909,    -1,
      98,  1014,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   659,  1026,  1027,  1028,    -1,    -1,   314,  1032,
    1033,    -1,    -1,   319,    -1,    -1,    -1,    -1,  1041,   325,
      -1,    -1,    -1,   329,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   938,    -1,   341,   342,    -1,    -1,    -1,
     945,   946,    -1,    -1,    -1,    -1,    -1,    -1,   705,    -1,
      -1,    -1,    -1,    -1,    -1,   780,   961,   714,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1092,
      -1,    -1,    -1,   379,    -1,    -1,  1099,   383,    -1,   385,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1010,
      -1,    -1,   997,  1014,    -1,  1000,    -1,    -1,  1003,    -1,
      -1,    -1,    -1,    -1,   212,  1026,  1027,  1028,    -1,    -1,
      -1,  1032,  1033,   770,    -1,    -1,   773,   774,   775,    -1,
    1041,    -1,    -1,    -1,    -1,   782,   783,    -1,    -1,    -1,
      -1,    -1,   789,   790,     2,    -1,     4,   245,    -1,   247,
    1045,  1164,    -1,    -1,    -1,    -1,    -1,  1052,  1053,    -1,
      -1,    -1,   260,    -1,   262,    -1,    -1,    -1,   815,    -1,
      -1,   818,  1185,  1068,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1092,    -1,   479,    -1,    -1,    -1,    -1,  1099,    -1,
     905,   906,    -1,    -1,   909,    53,    54,    -1,    -1,    -1,
      -1,   497,    -1,    -1,  1217,    -1,  1219,    -1,    77,    78,
      79,    80,    81,    82,    83,    -1,   314,    86,    87,    -1,
      -1,   319,    -1,    -1,  1119,    94,    95,   325,    -1,    -1,
      -1,   329,    -1,    -1,  1247,    -1,    -1,   884,  1251,  1134,
      -1,    -1,    -1,   341,    -1,    -1,   344,    -1,  1143,    -1,
      -1,    -1,    -1,  1164,    -1,    -1,    -1,    -1,    -1,    -1,
     129,   130,   131,   132,   133,   134,   135,   136,   137,   138,
      -1,    -1,    -1,    -1,  1185,    -1,    -1,    -1,    -1,    -1,
      -1,   379,    -1,    -1,    -1,   383,   582,   385,  1183,  1184,
      -1,   938,    -1,    -1,    -1,  1010,    -1,    -1,   945,  1014,
      -1,    -1,    -1,    -1,    -1,    -1,  1217,    -1,  1219,    -1,
      -1,  1026,  1027,  1028,   961,    -1,    -1,  1032,  1033,    -1,
      -1,   780,    -1,    -1,    -1,    -1,  1041,    -1,    -1,  1224,
     428,   429,    -1,    -1,    -1,    -1,  1247,    -1,    -1,    -1,
    1251,   637,    -1,   639,    -1,    -1,   993,    -1,    -1,    -1,
     997,    -1,    -1,  1000,   212,    -1,  1003,    -1,    -1,   780,
      -1,    -1,    -1,   659,    -1,    -1,    -1,    -1,    -1,    -1,
     780,    -1,    -1,    -1,    -1,    -1,    -1,  1092,    -1,    -1,
      -1,   479,    -1,    -1,  1099,    -1,    -1,   245,    -1,   247,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1045,    -1,
      -1,    -1,   260,    -1,   262,  1052,  1053,    -1,    -1,   705,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1068,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1076,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   905,   906,    -1,  1164,
     909,    -1,    -1,    -1,    -1,    -1,   314,    -1,    -1,    -1,
      -1,   319,    -1,    -1,    -1,    -1,    -1,   325,    -1,    -1,
    1185,   329,  1119,    -1,   770,    -1,    -1,   773,   774,    -1,
      -1,    -1,    -1,   341,   905,   906,   344,  1134,   909,    -1,
      -1,    -1,    -1,    -1,    -1,   905,   906,    -1,    -1,   909,
      -1,    -1,  1217,  1150,  1219,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   815,
      -1,   379,   818,    -1,    -1,   383,    -1,   385,    -1,    -1,
      -1,    -1,  1247,    -1,    -1,    -1,  1251,    -1,    -1,   637,
      -1,   639,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1010,    -1,    -1,    -1,  1014,    -1,    -1,    -1,    -1,
      -1,   659,    -1,    -1,    -1,    -1,    -1,  1026,  1027,  1028,
     428,   429,    -1,  1032,  1033,    -1,    -1,  1224,    -1,    -1,
      -1,    -1,  1041,    -1,    -1,    -1,    -1,    -1,   884,  1010,
      -1,    -1,    -1,  1014,    -1,    -1,    -1,    -1,    -1,    -1,
    1010,    -1,    -1,    -1,  1014,  1026,  1027,  1028,    -1,    -1,
      -1,  1032,  1033,    -1,    -1,    -1,  1026,  1027,  1028,    -1,
    1041,   479,  1032,  1033,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1041,    -1,  1092,    -1,    -1,    -1,    -1,    -1,    -1,
    1099,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   945,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   961,    -1,    -1,    -1,    -1,
      -1,  1092,   770,    -1,    -1,    -1,   774,   775,  1099,    -1,
      -1,    -1,  1092,    -1,   782,   783,    -1,    -1,    -1,  1099,
      -1,   789,   790,    -1,    -1,    -1,    -1,   993,    -1,    -1,
      -1,   997,    -1,    -1,  1000,  1164,    -1,  1003,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   815,    -1,    -1,
     818,    -1,    -1,    -1,    -1,     0,  1185,    -1,    -1,    -1,
      -1,    -1,    -1,     8,     9,    10,    -1,    -1,    13,    14,
      15,    -1,    17,  1164,    -1,    -1,    -1,    -1,    -1,  1045,
      25,    26,    27,    -1,  1164,    -1,  1052,  1053,  1217,    -1,
    1219,    -1,    37,    38,  1185,    40,    41,    42,    43,    44,
      -1,    -1,  1068,    -1,    -1,  1185,    -1,    -1,    -1,   637,
    1076,   639,    -1,    -1,    -1,    -1,   884,    -1,  1247,    -1,
      -1,    -1,  1251,    68,    -1,    -1,  1217,    -1,  1219,    -1,
      -1,   659,    -1,    -1,    -1,    -1,    -1,  1217,    -1,  1219,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    16,    17,
      -1,    96,    97,  1119,    -1,    -1,  1247,    -1,    -1,    -1,
    1251,    -1,    -1,    -1,    -1,    -1,    -1,  1247,  1134,    -1,
     938,  1251,    -1,    -1,    -1,   120,    -1,   945,    -1,    -1,
      48,    49,    50,    51,  1150,    -1,    -1,    55,    56,    -1,
      -1,    -1,    -1,   961,    -1,    -1,    -1,    -1,   143,   144,
      68,    69,    -1,   148,   149,    -1,   151,    -1,   153,    -1,
      -1,    -1,    -1,    77,    78,    79,    80,    81,    82,    83,
      -1,    -1,    86,    87,    -1,   993,    -1,    -1,    -1,   997,
      94,    95,  1000,    -1,   102,  1003,    -1,    -1,    -1,    -1,
      -1,    -1,   770,    -1,    -1,    -1,   774,   775,    -1,    -1,
      -1,    -1,    -1,    -1,   782,   783,    -1,    -1,  1224,    -1,
      -1,   789,   790,    -1,    -1,   129,   130,   131,   132,   133,
     134,   135,   136,   137,   138,    -1,    -1,  1045,    -1,    -1,
      -1,    -1,    -1,    -1,  1052,  1053,    -1,   815,    -1,    -1,
     818,    -1,    -1,     0,    -1,    -1,    -1,    -1,    -1,    -1,
    1068,     8,     9,    10,    -1,    -1,    13,    14,    15,    -1,
      17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,    26,
      27,    28,    29,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      37,    38,    -1,    40,    41,    42,    43,    44,    -1,    -1,
      -1,    -1,   210,    -1,    -1,   213,   214,   215,    -1,   217,
      -1,  1119,    -1,    -1,    -1,    -1,   884,    -1,    -1,    -1,
      -1,    68,    -1,    -1,    -1,   233,  1134,   235,   236,    -1,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    -1,    -1,    -1,    -1,    94,    95,    96,
      97,    -1,    99,   100,    -1,    -1,    -1,    -1,    -1,   106,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     938,    -1,    -1,   120,    -1,    -1,   123,   945,    -1,   126,
     127,   128,   129,   130,   131,   132,   133,   134,   135,   136,
     137,   138,    -1,   961,    -1,    -1,   143,   144,   145,   146,
      -1,    -1,   149,   150,   151,    -1,   153,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1224,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   997,
      -1,   339,  1000,    -1,    -1,  1003,   344,   345,   346,   347,
     348,   349,    -1,    -1,   352,   353,   354,   355,   356,   357,
     358,   359,    -1,   361,    -1,    -1,   364,   365,   366,   367,
     368,   369,   370,   371,   372,   373,    -1,    -1,    -1,   377,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1045,    -1,    -1,
      -1,    -1,    -1,    -1,  1052,  1053,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1068,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     428,   429,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   437,
     438,   439,    -1,    -1,    -1,   443,    -1,   445,   446,   447,
      -1,    -1,    -1,    -1,    -1,   453,    -1,    -1,    -1,    -1,
      -1,  1119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     468,    -1,    -1,    -1,    -1,   473,  1134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   484,    -1,    -1,   487,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   499,     0,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       8,     9,    10,    -1,    -1,    13,    14,    15,   516,    17,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,    -1,    27,
      28,    29,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,
      38,    -1,    40,    41,    42,    43,    44,    -1,    -1,    -1,
      -1,   549,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   561,    -1,    -1,  1224,    -1,   566,    -1,
      68,    -1,    -1,    -1,    -1,    -1,    -1,   575,    -1,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    -1,    -1,    -1,    -1,    94,    95,    96,    97,
      -1,    99,   100,    -1,    -1,    -1,    -1,    -1,   106,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   120,    -1,    -1,   123,    -1,    -1,   126,   127,
     128,   129,   130,   131,   132,   133,   134,   135,   136,   137,
     138,    -1,    -1,    -1,    -1,    -1,   144,   145,   146,   647,
      -1,   149,   150,   151,    -1,   153,    -1,    -1,    -1,   657,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      33,    34,    35,    36,    -1,   673,    -1,   675,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    49,    50,    51,    52,
     688,    -1,   690,    56,    -1,    58,    59,    60,    61,    62,
      63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    77,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    91,    -1,
      -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,   101,   737,
      -1,   104,   105,    -1,   107,   108,    -1,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   755,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,   769,    -1,    -1,    -1,    -1,   139,   775,   776,    -1,
     778,    -1,    -1,    -1,   782,   783,    -1,    -1,    -1,    -1,
     788,   789,   790,    -1,    -1,    -1,    -1,     0,     1,    -1,
       3,     4,     5,     6,     7,    -1,    -1,    -1,    11,    12,
     808,    -1,    -1,    16,    -1,    18,    19,    20,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,   832,   833,    39,   835,   836,    -1,
      -1,    -1,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    -1,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    75,    76,    -1,    -1,    -1,    -1,    -1,   877,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    91,    -1,
     888,    -1,    -1,    -1,   892,    98,    -1,    -1,   101,   102,
      -1,   104,   105,    -1,   107,    -1,    -1,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   931,    -1,    -1,   139,   140,   141,    -1,
     938,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,    -1,
     153,    -1,    -1,     0,    -1,    -1,    -1,   955,    -1,    -1,
      -1,     8,     9,    10,    -1,    -1,    13,    14,    15,    -1,
      17,    -1,   970,    -1,   972,    44,    -1,    -1,    25,    -1,
      27,    28,    29,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      37,    38,    -1,    40,    41,    42,    43,    44,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    68,    -1,    -1,    -1,    94,    95,    -1,    -1,    -1,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    -1,    -1,    -1,  1044,    94,    95,    96,
      97,    -1,    99,   100,    -1,    -1,    -1,    -1,   127,   106,
     129,   130,   131,   132,   133,   134,   135,   136,   137,   138,
      -1,    -1,    -1,   120,    -1,    -1,   123,   146,    -1,   126,
     127,   128,   129,   130,   131,   132,   133,   134,   135,   136,
     137,   138,    -1,    -1,    -1,    -1,    -1,   144,   145,   146,
      -1,    -1,   149,   150,   151,    -1,   153,    -1,    -1,     0,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     8,     9,    10,
      -1,    -1,    13,    14,    15,    -1,    17,    -1,  1126,    -1,
      -1,    44,    -1,    -1,    25,    26,    27,    28,    29,    -1,
      -1,    -1,    -1,    -1,    -1,  1143,    37,    38,    -1,    40,
      41,    42,    43,    44,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    68,    -1,    -1,
      -1,    94,    95,    -1,    -1,    -1,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    -1,
      -1,    -1,    -1,    94,    95,    96,    97,    -1,    -1,   100,
      -1,    -1,    -1,    -1,   127,   106,   129,   130,   131,   132,
     133,   134,   135,   136,   137,   138,    -1,    -1,    -1,   120,
      -1,    -1,   123,    -1,    -1,    -1,   127,   128,   129,   130,
     131,   132,   133,   134,   135,   136,   137,   138,    -1,    -1,
      -1,    -1,   143,   144,   145,   146,     0,    -1,   149,   150,
     151,    -1,   153,    -1,     8,     9,    10,    -1,    -1,    13,
      14,    15,    -1,    17,    -1,    -1,    -1,    -1,    44,    -1,
      -1,    25,    26,    27,    28,    29,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    37,    38,    -1,    40,    41,    42,    43,
      44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    68,    -1,    -1,    -1,    94,    95,
      -1,    -1,    -1,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    -1,    -1,    -1,    -1,
      94,    95,    96,    97,    -1,    -1,   100,    -1,    -1,    -1,
      -1,   127,   106,   129,   130,   131,   132,   133,   134,   135,
     136,   137,   138,    -1,    -1,    -1,   120,    -1,    -1,   123,
      -1,    -1,    -1,   127,   128,   129,   130,   131,   132,   133,
     134,   135,   136,   137,   138,    -1,    -1,    -1,    -1,   143,
     144,   145,   146,     0,    -1,   149,   150,   151,    -1,   153,
      -1,     8,     9,    10,    -1,    -1,    13,    14,    15,    -1,
      17,    -1,    -1,    -1,    -1,    44,    -1,    -1,    25,    -1,
      27,    28,    29,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      37,    38,    -1,    40,    41,    42,    43,    44,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    68,    -1,    -1,    -1,    94,    95,    -1,    -1,    -1,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    -1,    -1,    -1,    -1,    94,    95,    96,
      97,    -1,    99,   100,    -1,    -1,    -1,    -1,   127,   106,
     129,   130,   131,   132,   133,   134,   135,   136,   137,   138,
      -1,    -1,    -1,   120,    -1,    -1,   123,    -1,    -1,   126,
     127,   128,   129,   130,   131,   132,   133,   134,   135,   136,
     137,   138,    -1,    -1,    -1,    -1,    -1,   144,   145,   146,
       0,    -1,   149,   150,   151,    -1,   153,    -1,     8,     9,
      10,    -1,    -1,    13,    14,    15,    -1,    17,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    25,    26,    27,    28,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,
      40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    68,    -1,
      -1,    -1,    94,    95,    -1,    -1,    -1,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      -1,    -1,    -1,    -1,    94,    95,    96,    97,    -1,    -1,
     100,    -1,    -1,    -1,    -1,   127,   106,   129,   130,   131,
     132,   133,   134,   135,   136,   137,   138,    -1,    -1,    -1,
     120,    -1,    -1,    -1,    -1,    -1,    -1,   127,    -1,   129,
     130,   131,   132,   133,   134,   135,   136,   137,   138,    -1,
      -1,    -1,    -1,   143,   144,   145,   146,     0,   148,   149,
     150,   151,    -1,   153,    -1,     8,     9,    10,    -1,    -1,
      13,    14,    15,    -1,    17,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    25,    -1,    27,    28,    29,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,    42,
      43,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    68,    -1,    -1,    -1,    94,
      95,    -1,    -1,    -1,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    -1,    -1,    -1,
      -1,    94,    95,    96,    97,    -1,    -1,   100,    -1,    -1,
      -1,    -1,    -1,   106,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,    -1,    -1,    -1,   120,    -1,    -1,
     123,    -1,    -1,    -1,   127,   128,   129,   130,   131,   132,
     133,   134,   135,   136,   137,   138,    -1,    -1,    -1,    -1,
      -1,   144,   145,   146,     0,    -1,   149,   150,   151,    -1,
     153,    -1,     8,     9,    10,    -1,    -1,    13,    14,    15,
      -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,
      26,    27,    28,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    37,    38,    -1,    40,    41,    42,    43,    44,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    68,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    -1,    -1,    -1,    -1,    94,    95,
      96,    97,    -1,    -1,   100,    -1,    -1,    -1,    -1,    -1,
     106,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   120,    -1,    -1,    -1,    -1,    -1,
      -1,   127,    -1,   129,   130,   131,   132,   133,   134,   135,
     136,   137,   138,    -1,    -1,    -1,    -1,   143,   144,   145,
     146,     0,   148,   149,   150,   151,    -1,   153,    -1,     8,
       9,    10,    -1,    -1,    13,    14,    15,    -1,    17,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    25,    -1,    27,    28,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,
      -1,    40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    68,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    -1,    -1,    -1,    -1,    94,    95,    96,    97,    -1,
      -1,   100,    -1,    -1,    -1,    -1,    -1,   106,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   120,    -1,    -1,    -1,    -1,    -1,    -1,   127,    -1,
     129,   130,   131,   132,   133,   134,   135,   136,   137,   138,
      -1,    -1,    -1,    -1,    -1,   144,   145,   146,     0,   148,
     149,   150,   151,    -1,   153,    -1,     8,     9,    10,    -1,
      -1,    -1,    14,    15,    -1,    17,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    25,    26,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    -1,    -1,    -1,    68,    94,    95,    -1,
      -1,    -1,    -1,    -1,    -1,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    -1,    -1,
      -1,    -1,    94,    95,    96,    97,    -1,    99,   100,    -1,
     127,    -1,   129,   130,   131,   132,   133,   134,   135,   136,
     137,   138,    -1,    -1,    -1,    -1,    -1,    -1,   120,    -1,
      -1,    -1,    -1,    -1,   126,   127,   153,   129,   130,   131,
     132,   133,   134,   135,   136,   137,   138,    -1,    -1,    -1,
      -1,   143,   144,   145,   146,     0,    -1,   149,    -1,   151,
      -1,   153,    -1,     8,     9,    10,    -1,    -1,    -1,    14,
      15,    -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      25,    26,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    37,    38,    -1,    40,    41,    42,    43,    44,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      -1,    -1,    -1,    68,    94,    95,    -1,    -1,    -1,    -1,
     100,    -1,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    -1,    -1,    -1,    -1,    94,
      95,    96,    97,    -1,    99,   100,    -1,   127,    -1,   129,
     130,   131,   132,   133,   134,   135,   136,   137,   138,    -1,
      -1,    -1,    -1,    -1,    -1,   120,    -1,    -1,    -1,    -1,
      -1,   126,   127,    -1,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,    -1,    -1,    -1,    -1,   143,   144,
     145,   146,     0,    -1,   149,    -1,   151,    -1,   153,    -1,
       8,     9,    10,    -1,    -1,    -1,    14,    15,    -1,    17,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,
      38,    -1,    40,    41,    42,    43,    44,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      68,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    -1,    -1,    -1,    -1,    94,    95,    96,    97,
      -1,    99,   100,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   120,    -1,    -1,    -1,    -1,    -1,   126,   127,
      -1,   129,   130,   131,   132,   133,   134,   135,   136,   137,
     138,    -1,    -1,    -1,    -1,    -1,   144,   145,   146,     0,
      -1,   149,    -1,   151,    -1,   153,    -1,     8,     9,    10,
      -1,    -1,    -1,    14,    15,    -1,    17,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    25,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,
      41,    42,    43,    44,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    68,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    -1,
      -1,    -1,    -1,    94,    95,    96,    97,    -1,    99,   100,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   120,
      -1,    -1,    -1,    -1,    -1,   126,   127,    -1,   129,   130,
     131,   132,   133,   134,   135,   136,   137,   138,    -1,    -1,
      -1,    -1,    -1,   144,   145,   146,    -1,    -1,   149,    -1,
     151,     1,   153,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    -1,    -1,    15,    16,    -1,    18,    19,
      20,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    -1,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    75,    76,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    98,    -1,
      -1,   101,   102,    -1,   104,   105,    -1,   107,    -1,    -1,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   139,
     140,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   151,     1,   153,     3,     4,     5,     6,     7,    -1,
      -1,    10,    11,    12,    -1,    14,    15,    16,    -1,    18,
      19,    20,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    -1,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    75,    76,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    98,
      -1,    -1,   101,   102,    -1,   104,   105,    -1,   107,    -1,
      -1,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     139,   140,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   151,     1,   153,     3,     4,     5,     6,     7,
      -1,    -1,    10,    11,    12,    -1,    -1,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,
      98,    -1,    -1,   101,   102,    -1,   104,   105,    -1,   107,
      -1,    -1,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   139,   140,   141,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   151,     1,   153,     3,     4,     5,     6,
       7,    -1,    -1,    10,    11,    12,    -1,    -1,    15,    16,
      -1,    18,    19,    20,    21,    22,    23,    24,    25,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    -1,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    91,    -1,    -1,    -1,    -1,    -1,
      -1,    98,    -1,    -1,   101,   102,    -1,   104,   105,    -1,
     107,    -1,    -1,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   139,   140,   141,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   151,     1,   153,     3,     4,     5,
       6,     7,    -1,    -1,    10,    11,    12,    -1,    -1,    15,
      16,    -1,    18,    19,    20,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    -1,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,
      76,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    90,    91,    -1,    -1,    -1,    -1,
      -1,    -1,    98,    -1,    -1,   101,   102,    -1,   104,   105,
      -1,   107,    -1,    -1,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     1,    -1,     3,     4,     5,     6,     7,    -1,     9,
      10,    11,    12,   139,   140,   141,    16,    -1,    18,    19,
      20,    21,    22,    23,    24,   151,    -1,   153,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    -1,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    75,    76,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    98,    -1,
      -1,   101,   102,    -1,   104,   105,    -1,   107,    -1,    -1,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,     3,
       4,     5,     6,     7,    -1,    -1,    -1,    11,    12,   139,
     140,   141,    16,    -1,    18,    19,    20,    21,    22,    23,
      24,   151,    -1,   153,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    -1,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    75,    76,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    91,    -1,    -1,
      -1,    -1,    -1,    -1,    98,    -1,    -1,   101,   102,    -1,
     104,   105,    -1,   107,    -1,    -1,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   139,   140,   141,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   149,    -1,   151,     1,   153,
       3,     4,     5,     6,     7,    -1,    -1,    -1,    11,    12,
      -1,    -1,    -1,    16,    -1,    18,    19,    20,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    -1,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    75,    76,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    91,    -1,
      -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,   101,   102,
      -1,   104,   105,    -1,   107,    -1,    -1,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   139,   140,   141,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,   151,     1,
     153,     3,     4,     5,     6,     7,    -1,    -1,    -1,    11,
      12,    -1,    -1,    -1,    16,    -1,    18,    19,    20,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    -1,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    75,    76,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    91,
      -1,    -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,   101,
     102,    -1,   104,   105,    -1,   107,    -1,    -1,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   139,   140,   141,
      -1,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,   151,
       1,   153,     3,     4,     5,     6,     7,    -1,    -1,    -1,
      11,    12,    -1,    -1,    -1,    16,    -1,    18,    19,    20,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    -1,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    75,    76,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      91,    -1,    -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,
     101,   102,    -1,   104,   105,    -1,   107,    -1,    -1,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   139,   140,
     141,    -1,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,
     151,     1,   153,     3,     4,     5,     6,     7,    -1,    -1,
      10,    11,    12,    -1,    -1,    -1,    16,    -1,    18,    19,
      20,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    -1,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    75,    76,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    98,    -1,
      -1,   101,   102,    -1,   104,   105,    -1,   107,    -1,    -1,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,     3,
       4,     5,     6,     7,    -1,    -1,    -1,    11,    12,   139,
     140,   141,    16,    -1,    18,    19,    20,    21,    22,    23,
      24,   151,    -1,   153,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    -1,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    75,    76,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    91,    -1,    -1,
      -1,    -1,    -1,    -1,    98,    -1,    -1,   101,   102,    -1,
     104,   105,    -1,   107,    -1,    -1,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,    -1,     7,
      -1,    -1,    -1,    11,    12,   139,   140,   141,    16,    -1,
      18,    19,    20,    21,    22,    23,    24,   151,    -1,   153,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,
      -1,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,
      98,    -1,    -1,   101,   102,    -1,   104,   105,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,    11,
      12,   139,   140,   141,    16,    -1,    18,    19,    20,    21,
      22,    23,    24,    -1,    -1,   153,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    -1,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    75,    76,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    91,
      -1,    -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,   101,
     102,    -1,   104,   105,    -1,    -1,    -1,    -1,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,
      -1,     7,    -1,    -1,    -1,    11,    12,   139,   140,   141,
      16,    -1,    18,    19,    20,    21,    22,    23,    24,   151,
      -1,   153,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      46,    -1,    -1,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    -1,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,
      76,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    90,    91,    -1,    -1,    -1,    -1,
      -1,    -1,    98,    -1,    -1,   101,   102,    -1,   104,   105,
      -1,    -1,    -1,    -1,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,     6,     7,    -1,    -1,
      -1,    11,    12,   139,   140,   141,    16,    -1,    18,    19,
      20,    21,    22,    23,    24,    -1,    -1,   153,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    -1,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    75,    76,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    98,    -1,
      -1,   101,   102,    -1,   104,   105,    -1,   107,    -1,    -1,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,     7,    -1,    -1,    -1,    11,    12,   139,
     140,   141,    16,    -1,    18,    19,    20,    21,    22,    23,
      24,   151,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    -1,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    75,    76,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    91,    -1,    -1,
      -1,    -1,    -1,    -1,    98,    -1,    -1,   101,   102,    -1,
     104,   105,    -1,   107,    -1,    -1,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   139,   140,   141,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    -1,    -1,    -1,    -1,    -1,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    -1,
      -1,    86,    87,    -1,    -1,    -1,    -1,    92,    93,    94,
      95,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   107,   108,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,    -1,   140,   141,    -1,    -1,    -1,
      -1,    -1,   147,   148,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    -1,    -1,    86,    87,    -1,
      -1,    -1,    -1,    92,    93,    94,    95,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,   108,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,   130,   131,   132,   133,   134,   135,   136,   137,   138,
      -1,   140,   141,    -1,    -1,    -1,    -1,    -1,   147,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    -1,    -1,    -1,    -1,
      -1,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      -1,    -1,    86,    87,    -1,    -1,    -1,    -1,    92,    93,
      94,    95,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   107,   108,    -1,    -1,   111,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   129,   130,   131,   132,   133,
     134,   135,   136,   137,   138,    -1,   140,   141,    -1,    -1,
      -1,    -1,    -1,   147,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    -1,    -1,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    -1,    -1,    86,    87,    -1,
      -1,    -1,    -1,    92,    93,    94,    95,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,   108,
      -1,    -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,   130,   131,   132,   133,   134,   135,   136,   137,   138,
      -1,   140,   141,    -1,    -1,    -1,    -1,    -1,   147,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    -1,    -1,    -1,    -1,
      -1,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      -1,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      -1,    -1,    86,    87,    -1,    -1,    -1,    -1,    92,    93,
      94,    95,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   107,   108,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   129,   130,   131,   132,   133,
     134,   135,   136,   137,   138,    -1,   140,   141,     3,     4,
       5,    -1,     7,   147,    -1,    -1,    11,    12,    -1,    -1,
      -1,    16,    -1,    18,    19,    20,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    46,    -1,    -1,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    98,    -1,    -1,   101,   102,    -1,   104,
     105,    -1,    -1,    -1,    -1,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,
      -1,     7,    -1,    -1,   139,    11,    12,    -1,    -1,    -1,
      16,   146,    18,    19,    20,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      46,    -1,    -1,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    -1,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    98,    -1,    -1,   101,   102,    -1,   104,   105,
      -1,    -1,    -1,    -1,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,    -1,    -1,   139,    11,    12,    -1,    -1,    -1,    16,
     146,    18,    19,    20,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    -1,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    91,    -1,    -1,    -1,    -1,    -1,
      -1,    98,    -1,    -1,   101,   102,    -1,   104,   105,    -1,
     107,    -1,    -1,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,
      11,    12,   139,   140,   141,    16,    -1,    18,    19,    20,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    75,    76,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      91,    -1,    -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,
     101,   102,    -1,   104,   105,    -1,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,    -1,    -1,    -1,    11,    12,   139,   140,
     141,    16,    -1,    18,    19,    20,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      45,    46,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      75,    76,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    91,    -1,    -1,    -1,
      -1,    -1,    -1,    98,    -1,    -1,   101,   102,    -1,   104,
     105,    -1,   107,    -1,    -1,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,    -1,     7,    -1,
      -1,    -1,    11,    12,   139,   140,   141,    16,    -1,    18,
      19,    20,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    75,    76,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    98,
      -1,    -1,   101,   102,    -1,   104,   105,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,    -1,     7,    -1,    -1,    -1,    11,    12,
     139,   140,   141,    16,    -1,    18,    19,    20,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    46,    -1,    -1,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    75,    76,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    91,    -1,
      -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,   101,   102,
      -1,   104,   105,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,    -1,
       7,    -1,    -1,    -1,    11,    12,   139,   140,   141,    16,
      -1,    18,    19,    20,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    46,
      -1,    -1,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    91,    -1,    -1,    -1,    -1,    -1,
      -1,    98,    -1,    -1,   101,   102,    -1,   104,   105,    -1,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,
      11,    12,   139,   140,   141,    16,    -1,    18,    19,    20,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    75,    76,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      91,    -1,    -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,
     101,   102,    -1,   104,   105,    -1,   107,   108,    -1,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,    -1,     7,    -1,    -1,    -1,    11,    12,   139,   140,
     141,    16,    -1,    18,    19,    20,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    46,    -1,    -1,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      75,    76,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    91,    -1,    -1,    -1,
      -1,    -1,    -1,    98,    -1,    -1,   101,   102,    -1,   104,
     105,    -1,    -1,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,    -1,     7,    -1,
      -1,    -1,    11,    12,   139,   140,   141,    16,    -1,    18,
      19,    20,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    75,    76,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    98,
      -1,    -1,   101,   102,    -1,   104,   105,    -1,   107,   108,
      -1,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,    -1,     7,    -1,    -1,    -1,    11,    12,
     139,   140,   141,    16,    -1,    18,    19,    20,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    46,    -1,    -1,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    75,    76,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    91,    -1,
      -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,   101,   102,
      -1,   104,   105,    -1,    -1,   108,    -1,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,    -1,
       7,    -1,    -1,    -1,    11,    12,   139,   140,   141,    16,
      -1,    18,    19,    20,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    46,
      -1,    -1,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    -1,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    91,    -1,    -1,    -1,    -1,    -1,
      -1,    98,    -1,    -1,   101,   102,    -1,   104,   105,    -1,
     107,    -1,    -1,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,
      11,    12,   139,   140,   141,    16,    -1,    18,    19,    20,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    -1,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    75,    76,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      91,    -1,    -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,
     101,   102,    -1,   104,   105,    -1,   107,    -1,    -1,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,    -1,     7,    -1,    -1,    -1,    11,    12,   139,   140,
     141,    16,    -1,    18,    19,    20,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    46,    -1,    -1,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      75,    76,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    91,    -1,    -1,    -1,
      -1,    -1,    -1,    98,    -1,    -1,   101,   102,    -1,   104,
     105,    -1,   107,    -1,    -1,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,    -1,     7,    -1,
      -1,    -1,    11,    12,   139,   140,   141,    16,    -1,    18,
      19,    20,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    -1,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    75,    76,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    98,
      -1,    -1,   101,   102,    -1,   104,   105,    -1,   107,    -1,
      -1,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,    -1,     7,    -1,    -1,    -1,    11,    12,
     139,   140,   141,    16,    -1,    18,    19,    20,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    46,    -1,    -1,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    -1,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    75,    76,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    91,    -1,
      -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,   101,   102,
      -1,   104,   105,    -1,   107,    -1,    -1,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,    -1,
       7,    -1,    -1,    -1,    11,    12,   139,   140,   141,    16,
      -1,    18,    19,    20,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    46,
      -1,    -1,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    -1,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    91,    -1,    -1,    -1,    -1,    -1,
      -1,    98,    -1,    -1,   101,   102,    -1,   104,   105,    -1,
      -1,    -1,    -1,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,
      11,    12,   139,   140,   141,    16,    -1,    18,    19,    20,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    -1,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    75,    76,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      91,    -1,    -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,
     101,   102,    -1,   104,   105,    -1,    -1,    -1,    -1,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,    -1,     7,    -1,    -1,    -1,    11,    12,   139,   140,
     141,    16,    -1,    18,    19,    20,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    46,    -1,    -1,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      75,    76,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    91,    -1,    -1,    -1,
      -1,    -1,    -1,    98,    -1,    -1,   101,   102,    -1,   104,
     105,    -1,    -1,    -1,    -1,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,    -1,     7,    -1,
      -1,    -1,    11,    12,   139,   140,   141,    16,    -1,    18,
      19,    20,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    -1,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,
      -1,    -1,   101,   102,    -1,   104,   105,    -1,   107,    -1,
      -1,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,    -1,
       7,    -1,    -1,    -1,    11,    12,    -1,    -1,    -1,    16,
     139,    18,    19,    20,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    46,
      -1,    -1,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    -1,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    98,    -1,    -1,   101,   102,    -1,   104,   105,    -1,
     107,    -1,    -1,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,    -1,     7,    -1,    -1,    -1,    11,    12,    -1,    -1,
      -1,    16,   139,    18,    19,    20,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    46,    -1,    -1,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    94,
      -1,    -1,    -1,    98,    -1,    -1,   101,   102,    -1,   104,
     105,    -1,    -1,    -1,    -1,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,    -1,     7,    -1,    -1,    -1,    11,    12,
      -1,    -1,    -1,    16,   139,    18,    19,    20,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    46,    -1,    -1,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    -1,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,   101,   102,
      -1,   104,   105,    -1,    -1,    -1,    -1,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,
      11,    12,    -1,    -1,    -1,    16,   139,    18,    19,    20,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    -1,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,
     101,   102,    -1,   104,   105,    -1,    -1,    -1,    -1,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,    -1,     7,    -1,
      -1,    -1,    11,    12,    -1,    -1,    -1,    16,   139,    18,
      19,    20,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    -1,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,
      -1,    -1,   101,   102,    -1,   104,   105,    -1,    -1,    -1,
      -1,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,    -1,
       7,    -1,    -1,    -1,    11,    12,    -1,    -1,    -1,    16,
     139,    18,    19,    20,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    46,
      -1,    -1,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    -1,    59,    60,    61,    62,    63,    64,    65,    33,
      34,    35,    36,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    49,    50,    51,    52,    -1,
      -1,    -1,    56,    -1,    -1,    59,    60,    61,    62,    63,
      -1,    98,    -1,    -1,   101,   102,    -1,   104,   105,    -1,
      -1,    -1,    -1,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,    -1,    -1,    -1,    90,    91,    -1,    -1,
      -1,    -1,    -1,    -1,    98,    -1,    -1,   101,    -1,    -1,
     104,   105,   139,   107,    -1,    -1,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    33,    34,    35,    36,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    49,    50,    51,    52,   139,    -1,    -1,    56,    -1,
      -1,    59,    60,    61,    62,    63,    -1,    -1,    -1,   153,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,
      98,    -1,    -1,   101,    -1,    -1,   104,   105,    -1,   107,
      -1,    -1,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    33,    34,    35,    36,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,    -1,    49,    50,    51,
      52,   139,    -1,    -1,    56,    -1,    -1,    59,    60,    61,
      62,    63,    -1,    -1,    33,    34,    35,    36,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      49,    50,    51,    52,    -1,    -1,    -1,    56,    90,    91,
      59,    60,    61,    62,    63,    -1,    98,    -1,    -1,   101,
      -1,    -1,   104,   105,    -1,   107,    -1,    -1,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,    -1,    -1,
      -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    98,
     132,    -1,   101,    -1,    -1,   104,   105,   139,    -1,    -1,
      -1,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      52,    53,    -1,    -1,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
     139,    -1,    -1,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    86,    87,    -1,    -1,    -1,    -1,
      92,    93,    94,    95,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   107,   108,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,   130,   131,
     132,   133,   134,   135,   136,   137,   138,    -1,   140,   141,
      52,    53,    -1,    -1,    56,   147,   148,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    86,    87,    -1,    -1,    -1,    -1,
      92,    93,    94,    95,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   107,   108,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,   130,   131,
     132,   133,   134,   135,   136,   137,   138,    -1,   140,   141,
      52,    53,    -1,    -1,    56,   147,   148,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    86,    87,    -1,    -1,    -1,    -1,
      92,    93,    94,    95,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   107,   108,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,   130,   131,
     132,   133,   134,   135,   136,   137,   138,    -1,   140,   141,
      52,    53,    -1,    -1,    56,   147,   148,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    86,    87,    -1,    -1,    -1,    -1,
      92,    93,    94,    95,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   107,   108,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,   130,   131,
     132,   133,   134,   135,   136,   137,   138,    -1,   140,   141,
      52,    53,    -1,    -1,    56,   147,   148,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    86,    87,    -1,    -1,    -1,    -1,
      92,    93,    94,    95,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   107,   108,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,   130,   131,
     132,   133,   134,   135,   136,   137,   138,    -1,   140,   141,
      52,    53,    -1,    -1,    56,   147,   148,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    86,    87,    -1,    -1,    -1,    -1,
      92,    93,    94,    95,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   107,   108,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,   130,   131,
     132,   133,   134,   135,   136,   137,   138,    -1,   140,   141,
      52,    53,    -1,    -1,    56,   147,   148,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    86,    87,    -1,    -1,    -1,    -1,
      92,    93,    94,    95,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   107,   108,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,   130,   131,
     132,   133,   134,   135,   136,   137,   138,    -1,   140,   141,
      52,    53,    -1,    -1,    56,   147,   148,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    86,    87,    -1,    -1,    -1,    -1,
      92,    93,    94,    95,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   107,   108,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,   130,   131,
     132,   133,   134,   135,   136,   137,   138,    -1,   140,   141,
      52,    53,    -1,    -1,    56,   147,   148,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    86,    87,    -1,    -1,    -1,    -1,
      92,    93,    94,    95,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   107,   108,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,   130,   131,
     132,   133,   134,   135,   136,   137,   138,    -1,   140,   141,
      52,    53,    -1,    -1,    56,   147,   148,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    86,    87,    -1,    -1,    -1,    -1,
      92,    93,    94,    95,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   107,   108,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,   130,   131,
     132,   133,   134,   135,   136,   137,   138,    -1,   140,   141,
      52,    53,    -1,    -1,    56,   147,   148,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    86,    87,    -1,    -1,    -1,    -1,
      92,    93,    94,    95,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   107,   108,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,   130,   131,
     132,   133,   134,   135,   136,   137,   138,    -1,   140,   141,
      52,    53,    -1,    -1,    56,   147,   148,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    86,    87,    -1,    -1,    -1,    -1,
      92,    93,    94,    95,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   107,   108,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,   130,   131,
     132,   133,   134,   135,   136,   137,   138,    -1,   140,   141,
      52,    53,    -1,    -1,    56,   147,   148,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    86,    87,    -1,    -1,    -1,    -1,
      92,    93,    94,    95,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   107,   108,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,   130,   131,
     132,   133,   134,   135,   136,   137,   138,    -1,   140,   141,
      52,    53,    -1,    -1,    56,   147,   148,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    86,    87,    -1,    -1,    -1,    -1,
      92,    93,    94,    95,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   107,   108,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,   130,   131,
     132,   133,   134,   135,   136,   137,   138,    -1,   140,   141,
      52,    53,    -1,    -1,    56,   147,   148,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    86,    87,    -1,    -1,    -1,    -1,
      92,    93,    94,    95,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   107,   108,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,   130,   131,
     132,   133,   134,   135,   136,   137,   138,    -1,   140,   141,
      52,    53,    -1,    -1,    56,   147,   148,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    86,    87,    -1,    -1,    -1,    -1,
      92,    93,    94,    95,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   107,   108,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,   130,   131,
     132,   133,   134,   135,   136,   137,   138,    -1,   140,   141,
      -1,    -1,    -1,    -1,    -1,   147
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint16 yystos[] =
{
       0,   155,   156,     0,     1,     3,     4,     5,     6,     7,
      11,    12,    16,    18,    19,    20,    21,    22,    23,    24,
      30,    31,    32,    33,    34,    35,    36,    39,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    59,    60,    61,    62,    63,    64,    65,    75,    76,
      90,    91,    98,   101,   102,   104,   105,   107,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   139,   140,   141,
     157,   158,   159,   167,   169,   171,   176,   177,   183,   184,
     186,   187,   188,   190,   191,   192,   194,   195,   204,   207,
     222,   232,   233,   234,   235,   236,   237,   238,   239,   240,
     241,   242,   251,   271,   279,   280,   331,   332,   333,   334,
     335,   336,   337,   340,   342,   343,   357,   358,   360,   361,
     362,   363,   364,   365,   366,   367,   405,   419,   159,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    56,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    86,    87,    92,    93,    94,    95,   107,   108,
     129,   130,   131,   132,   133,   134,   135,   136,   137,   138,
     140,   141,   147,   198,   199,   200,   202,   203,   357,    39,
      58,    98,   101,   107,   108,   109,   112,   140,   187,   195,
     204,   208,   214,   217,   219,   232,   363,   364,   366,   367,
     403,   404,   214,   148,   215,   216,   148,   211,   215,   148,
     153,   412,    54,   199,   412,   143,   160,   143,    21,    22,
      31,    32,   186,   204,   232,   251,   204,   204,   204,    56,
       1,    47,   101,   163,   164,   165,   167,   189,   190,   419,
     167,   224,   209,   219,   403,   419,   208,   402,   403,   419,
      46,    98,   139,   146,   176,   177,   194,   222,   232,   363,
     364,   367,   272,    54,    55,    57,   198,   346,   359,   346,
     347,   348,   152,   152,   152,   152,   362,   183,   204,   204,
     151,   153,   411,   417,   418,    40,    41,    42,    43,    44,
      37,    38,   148,   370,   371,   372,   373,   419,   370,   372,
      26,   143,   211,   215,   243,   281,    28,   244,   278,   126,
     146,   101,   107,   191,   126,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    94,    95,
     100,   127,   129,   130,   131,   132,   133,   134,   135,   136,
     137,   138,   206,   206,    68,    96,    97,   145,   409,   223,
     171,   179,   179,   180,   181,   180,   179,   411,   418,    98,
     188,   195,   232,   256,   363,   364,   367,    52,    56,    94,
      98,   196,   197,   232,   363,   364,   367,   197,    33,    34,
      35,    36,    49,    50,    51,    52,    56,   148,   175,   198,
     365,   400,   214,    97,   409,   410,   281,   334,    99,    99,
     146,   208,    56,   208,   208,   208,   346,   126,   100,   146,
     218,   419,    97,   145,   409,    99,    99,   146,   218,   214,
     412,   413,   214,    91,   213,   214,   219,   377,   403,   419,
     171,   413,   171,    54,    64,    65,   168,   148,   205,   157,
     163,    97,   409,    99,   167,   166,   189,   149,   411,   418,
     413,   225,   413,   150,   146,   153,   416,   146,   416,   144,
     416,   412,    56,   362,   191,   193,   371,   146,    97,   145,
     409,   273,    66,   119,   121,   122,   349,   119,   119,   349,
      67,   349,   338,   344,   341,   345,    77,   151,   159,   179,
     179,   179,   179,   167,   171,   171,    52,    54,    55,    56,
      57,    58,    77,    91,   101,   107,   108,   109,   133,   136,
     261,   374,   376,   377,   378,   379,   380,   381,   382,   383,
     384,   387,   388,   389,   390,   391,   394,   395,   396,   397,
     398,   126,   161,   163,   376,   382,   126,   161,   282,   283,
     106,   185,   286,   287,   286,   107,   183,   208,   219,   220,
     221,   189,   146,   194,   146,   169,   170,   183,   195,   204,
     208,   210,   221,   232,   367,   204,   204,   204,   204,   204,
     204,   204,   204,   204,   204,   204,   204,   204,   172,   204,
     204,   204,   204,   204,   204,   204,   204,   204,   204,   204,
      52,    53,    56,   202,   211,   406,   407,   213,   219,    52,
      53,    56,   202,   211,   406,   161,    13,   252,   417,   252,
     163,   179,   163,   411,   228,    56,    97,   145,   409,    25,
     171,    52,    56,   196,   130,   368,    97,   145,   409,   231,
     401,    68,    97,   408,    52,    56,   406,   170,   204,   210,
     170,   210,   201,   124,   208,   107,   208,   217,   403,    52,
      56,   213,    52,    56,   404,   413,   149,   413,   146,   413,
     146,   413,   199,   226,   204,   144,   144,   406,   406,   210,
     160,   413,   165,   413,   403,   146,   193,    52,    56,   213,
      52,    56,   274,   351,   350,   119,   339,   349,    66,   119,
     119,   339,    66,   119,   204,   101,   107,   257,   258,   259,
     260,   379,   146,   399,   419,   413,   413,   126,   146,   375,
     208,   146,   399,    34,    52,   146,   375,    52,   146,   375,
      52,   204,    10,   250,     8,   245,   327,   419,   417,   146,
     204,   250,   144,   284,   282,   250,   288,   250,   208,   146,
      44,   413,   193,   146,    44,   126,    44,    97,   145,   409,
     173,   412,    99,    99,   211,   215,   412,   414,   146,    99,
      99,   211,   212,   215,   419,   250,   163,    13,   163,   250,
      27,   253,   417,   250,    25,   227,   293,    17,   247,   291,
      52,    56,   213,    52,    56,   180,   230,   369,   229,    52,
      56,   196,   213,   161,   171,   178,   212,   215,   199,   208,
     208,   218,    99,    99,   414,    99,    99,   377,   403,   171,
     416,   191,   414,   148,   276,   376,   352,    54,    55,    57,
     356,   367,   152,   349,   152,   152,   152,   259,   379,   146,
     413,   146,   398,   208,   374,   377,   381,   394,   396,   384,
     388,   390,   382,   391,   396,   380,   382,    44,   208,   221,
     328,   419,     9,    15,   246,   248,   330,   419,    44,   285,
     144,   289,   107,   208,   167,   193,   167,   204,    52,    56,
     213,    52,    56,    52,    56,    90,    91,    98,   101,   104,
     105,   132,   271,   300,   301,   302,   305,   322,   323,   324,
     325,   326,   331,   332,   335,   336,   337,   340,   342,   343,
     364,   128,   170,   210,   170,   210,   185,   150,    99,   170,
     210,   170,   210,   185,    14,   248,   249,   254,   255,   419,
     255,   182,   294,   291,   250,   107,   208,   290,   250,   414,
     163,   417,   179,   161,   414,   250,   413,   175,   281,   278,
      99,   413,   146,   413,   376,   275,   353,   413,   257,   260,
     258,   413,   146,   375,   146,   375,   399,   146,   375,   146,
     375,   375,   204,   100,   329,   419,   163,   162,   204,   131,
     266,   267,   419,   266,   208,   414,   323,   323,    56,   196,
     307,   107,   300,   308,   309,   310,   311,   312,   314,   414,
     306,   412,   415,    52,   100,   174,   131,    88,    89,    97,
     145,   148,   303,   304,   204,   170,   210,   163,   179,   250,
     250,   295,   250,   208,   146,   252,   250,   161,   417,   250,
     268,   412,    29,   123,   277,   354,   146,   146,   382,   396,
     382,   382,    98,   195,   232,   363,   364,   367,   252,   163,
     261,   262,   265,   268,   380,   382,   383,   385,   386,   392,
     393,   396,   398,   163,   161,   300,    52,   414,   107,   300,
     314,   414,   146,   146,    58,   112,   315,   316,   317,   318,
     319,   320,   321,   388,   144,   324,   305,   323,   323,   196,
     414,   413,   112,   308,   311,   315,   308,   311,   315,   252,
     298,   299,   300,   310,   311,   315,   107,   208,   163,   250,
     149,   151,   161,   163,   355,   258,   375,   146,   375,   375,
     375,    56,    97,   145,   409,   163,   330,   399,   268,   131,
     126,   146,   263,   264,    98,   232,   146,   399,   146,   263,
     146,   263,   413,    52,   146,   146,   313,   314,   346,   415,
     146,   300,    34,    52,   346,   413,   413,   413,   414,   414,
     414,   163,   252,    40,    41,   146,   208,   255,   291,   292,
      52,   269,   270,   378,   250,   144,   163,   382,    52,    56,
     213,    52,    56,   327,   131,   232,   262,   393,   396,    56,
      97,   385,   390,   382,   392,   396,   382,   146,   313,   146,
     124,   317,   321,   254,   296,   179,   179,   308,   312,   146,
     412,   120,   375,   414,   146,   263,   146,   263,    52,    56,
     399,   146,   263,   146,   263,   263,   313,   146,   312,   314,
     163,   146,   270,   382,   396,   382,   382,   255,   293,   297,
     313,   263,   146,   263,   263,   263,   382,   263
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint16 yyr1[] =
{
       0,   154,   156,   155,   157,   158,   158,   158,   158,   159,
     159,   160,   162,   161,   161,   163,   164,   164,   164,   164,
     165,   166,   165,   168,   167,   167,   167,   167,   167,   167,
     167,   167,   167,   167,   167,   167,   167,   167,   167,   167,
     167,   169,   169,   169,   169,   169,   169,   169,   169,   170,
     170,   170,   171,   171,   171,   171,   171,   172,   173,   174,
     171,   171,   175,   176,   178,   177,   179,   181,   182,   180,
     183,   183,   184,   184,   185,   186,   187,   187,   187,   187,
     187,   187,   187,   187,   187,   187,   187,   188,   188,   189,
     189,   190,   190,   190,   190,   190,   190,   190,   190,   190,
     190,   191,   191,   192,   192,   193,   193,   194,   194,   194,
     194,   194,   194,   194,   194,   194,   195,   195,   195,   195,
     195,   195,   195,   195,   195,   196,   196,   197,   197,   197,
     198,   198,   198,   198,   198,   199,   199,   200,   201,   200,
     202,   202,   202,   202,   202,   202,   202,   202,   202,   202,
     202,   202,   202,   202,   202,   202,   202,   202,   202,   202,
     202,   202,   202,   202,   202,   202,   202,   202,   202,   202,
     203,   203,   203,   203,   203,   203,   203,   203,   203,   203,
     203,   203,   203,   203,   203,   203,   203,   203,   203,   203,
     203,   203,   203,   203,   203,   203,   203,   203,   203,   203,
     203,   203,   203,   203,   203,   203,   203,   203,   203,   203,
     203,   204,   204,   204,   204,   204,   204,   204,   204,   204,
     204,   204,   204,   204,   204,   204,   204,   204,   204,   204,
     204,   204,   204,   204,   204,   204,   204,   204,   204,   204,
     204,   204,   204,   204,   204,   204,   204,   204,   204,   204,
     204,   205,   204,   204,   204,   204,   204,   204,   204,   206,
     206,   206,   206,   207,   207,   208,   209,   209,   209,   209,
     210,   210,   211,   211,   211,   212,   212,   213,   213,   213,
     213,   213,   214,   214,   214,   214,   214,   216,   215,   217,
     218,   218,   219,   219,   219,   219,   220,   220,   221,   221,
     221,   222,   222,   222,   222,   222,   222,   222,   222,   222,
     222,   222,   223,   222,   224,   222,   225,   222,   222,   222,
     222,   222,   222,   222,   222,   222,   222,   226,   222,   222,
     222,   222,   222,   222,   222,   222,   222,   222,   222,   227,
     222,   228,   222,   222,   222,   229,   222,   230,   222,   231,
     222,   222,   222,   222,   222,   222,   222,   232,   233,   234,
     235,   236,   237,   238,   239,   240,   241,   242,   243,   244,
     245,   246,   247,   248,   249,   250,   251,   252,   252,   252,
     253,   253,   254,   254,   255,   255,   256,   256,   257,   257,
     258,   258,   259,   259,   259,   259,   259,   260,   260,   261,
     261,   262,   262,   262,   262,   263,   263,   264,   265,   265,
     265,   265,   265,   265,   265,   265,   265,   265,   265,   265,
     265,   265,   265,   266,   266,   267,   267,   268,   268,   269,
     269,   270,   270,   272,   273,   274,   275,   271,   276,   276,
     277,   277,   278,   279,   279,   279,   279,   280,   280,   280,
     280,   280,   280,   280,   280,   280,   281,   281,   283,   284,
     285,   282,   287,   288,   289,   286,   290,   290,   290,   290,
     291,   292,   292,   294,   295,   296,   293,   297,   297,   298,
     298,   298,   299,   299,   299,   299,   299,   299,   300,   301,
     301,   302,   302,   303,   304,   305,   305,   305,   305,   305,
     305,   305,   305,   305,   305,   305,   305,   306,   305,   305,
     307,   305,   308,   308,   308,   308,   308,   308,   308,   308,
     309,   309,   310,   310,   311,   312,   312,   313,   313,   314,
     315,   315,   315,   315,   316,   316,   317,   317,   318,   318,
     319,   319,   320,   321,   321,   322,   322,   322,   322,   322,
     322,   322,   322,   322,   322,   323,   323,   323,   323,   323,
     323,   323,   323,   323,   323,   324,   325,   326,   326,   326,
     327,   327,   328,   328,   328,   329,   329,   330,   330,   331,
     331,   332,   333,   333,   333,   334,   335,   336,   337,   338,
     338,   339,   339,   340,   341,   341,   342,   343,   344,   344,
     345,   345,   346,   346,   347,   347,   348,   348,   349,   350,
     349,   351,   352,   353,   354,   355,   349,   356,   356,   356,
     356,   357,   357,   358,   359,   359,   359,   359,   360,   361,
     361,   362,   362,   362,   362,   363,   363,   363,   363,   363,
     364,   364,   364,   364,   364,   364,   364,   365,   365,   366,
     366,   367,   367,   369,   368,   368,   370,   370,   371,   371,
     371,   372,   373,   372,   374,   374,   374,   374,   375,   375,
     376,   376,   376,   376,   376,   376,   376,   376,   376,   376,
     376,   376,   376,   376,   376,   377,   378,   378,   378,   378,
     379,   379,   380,   381,   381,   382,   382,   383,   384,   384,
     385,   385,   386,   386,   387,   387,   388,   388,   389,   390,
     390,   391,   392,   393,   393,   394,   394,   395,   395,   396,
     396,   397,   397,   398,   399,   399,   400,   401,   400,   402,
     402,   403,   403,   404,   404,   404,   404,   405,   405,   405,
     406,   406,   406,   406,   407,   407,   407,   408,   408,   409,
     409,   410,   410,   411,   411,   412,   412,   413,   414,   415,
     416,   416,   416,   417,   417,   418,   418,   419
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     2,     1,     1,     3,     2,     1,
       2,     3,     0,     6,     3,     2,     1,     1,     3,     2,
       1,     0,     3,     0,     4,     3,     3,     3,     2,     3,
       3,     3,     3,     3,     4,     1,     3,     3,     5,     3,
       1,     3,     3,     6,     5,     5,     5,     5,     3,     1,
       3,     1,     1,     3,     3,     3,     2,     0,     0,     0,
       6,     1,     1,     2,     0,     5,     1,     0,     0,     4,
       1,     1,     1,     4,     3,     1,     2,     3,     4,     5,
       4,     5,     2,     2,     2,     2,     2,     1,     3,     1,
       3,     1,     2,     3,     5,     2,     4,     2,     4,     1,
       3,     1,     3,     2,     3,     1,     3,     1,     1,     4,
       3,     3,     3,     3,     2,     1,     1,     1,     4,     3,
       3,     3,     3,     2,     1,     1,     1,     2,     1,     3,
       1,     1,     1,     1,     1,     1,     1,     1,     0,     4,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     3,     3,     6,     5,     5,     5,     5,     4,     3,
       3,     3,     2,     2,     2,     2,     3,     3,     3,     3,
       3,     3,     4,     2,     2,     3,     3,     3,     3,     1,
       3,     3,     3,     3,     3,     2,     2,     3,     3,     3,
       3,     0,     4,     6,     4,     6,     4,     6,     1,     1,
       1,     1,     1,     3,     3,     1,     1,     2,     4,     2,
       1,     3,     3,     5,     3,     1,     1,     1,     1,     2,
       4,     2,     1,     2,     2,     4,     1,     0,     2,     2,
       2,     1,     1,     2,     3,     4,     1,     1,     3,     4,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     0,     4,     0,     3,     0,     4,     3,     3,
       2,     3,     3,     1,     4,     3,     1,     0,     6,     4,
       3,     2,     1,     2,     1,     6,     6,     4,     4,     0,
       6,     0,     5,     5,     6,     0,     6,     0,     7,     0,
       5,     4,     4,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     2,
       1,     1,     1,     5,     1,     2,     1,     1,     1,     3,
       1,     3,     1,     3,     5,     1,     3,     2,     1,     1,
       1,     4,     2,     2,     1,     2,     0,     1,     6,     8,
       4,     6,     4,     2,     6,     2,     4,     6,     2,     4,
       2,     4,     1,     1,     1,     3,     4,     1,     4,     1,
       3,     1,     1,     0,     0,     0,     0,     7,     4,     1,
       3,     3,     3,     2,     4,     5,     5,     2,     4,     4,
       3,     3,     3,     2,     1,     4,     3,     3,     0,     0,
       0,     5,     0,     0,     0,     5,     1,     2,     3,     4,
       5,     1,     1,     0,     0,     0,     8,     1,     1,     1,
       3,     3,     1,     2,     3,     1,     1,     1,     1,     3,
       1,     3,     1,     1,     1,     1,     4,     4,     4,     3,
       4,     4,     4,     3,     3,     3,     2,     0,     4,     2,
       0,     4,     1,     1,     2,     3,     5,     2,     4,     1,
       2,     3,     1,     3,     5,     2,     1,     1,     3,     1,
       3,     1,     2,     1,     1,     3,     2,     1,     1,     3,
       2,     1,     2,     1,     1,     1,     3,     3,     2,     2,
       1,     1,     1,     2,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     2,     2,     3,     1,
       6,     1,     1,     1,     1,     2,     1,     2,     1,     1,
       1,     1,     1,     1,     2,     3,     3,     3,     4,     0,
       3,     1,     2,     4,     0,     3,     4,     4,     0,     3,
       0,     3,     0,     2,     0,     2,     0,     2,     1,     0,
       3,     0,     0,     0,     0,     0,     8,     1,     1,     1,
       1,     1,     1,     2,     1,     1,     1,     1,     3,     1,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     0,     4,     0,     1,     1,     3,     5,
       3,     1,     0,     3,     4,     2,     2,     1,     2,     0,
       6,     8,     4,     6,     4,     6,     2,     4,     6,     2,
       4,     2,     4,     1,     0,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     3,     1,     3,     1,     2,     1,
       2,     1,     1,     3,     1,     3,     1,     1,     2,     2,
       1,     3,     3,     1,     3,     1,     3,     1,     1,     2,
       1,     1,     1,     2,     2,     1,     1,     0,     4,     1,
       2,     1,     3,     3,     2,     4,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     0,     1,     0,     1,     2,     2,     2,
       0,     1,     1,     1,     1,     1,     2,     0
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
      yyerror (&yylloc, p, YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)                                \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;        \
          (Current).first_column = YYRHSLOC (Rhs, 1).first_column;      \
          (Current).last_line    = YYRHSLOC (Rhs, N).last_line;         \
          (Current).last_column  = YYRHSLOC (Rhs, N).last_column;       \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).first_line   = (Current).last_line   =              \
            YYRHSLOC (Rhs, 0).last_line;                                \
          (Current).first_column = (Current).last_column =              \
            YYRHSLOC (Rhs, 0).last_column;                              \
        }                                                               \
    while (0)
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K])


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


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL

/* Print *YYLOCP on YYO.  Private, do not rely on its existence. */

YY_ATTRIBUTE_UNUSED
static unsigned
yy_location_print_ (FILE *yyo, YYLTYPE const * const yylocp)
{
  unsigned res = 0;
  int end_col = 0 != yylocp->last_column ? yylocp->last_column - 1 : 0;
  if (0 <= yylocp->first_line)
    {
      res += YYFPRINTF (p, "%d", yylocp->first_line);
      if (0 <= yylocp->first_column)
        res += YYFPRINTF (p, ".%d", yylocp->first_column);
    }
  if (0 <= yylocp->last_line)
    {
      if (yylocp->first_line < yylocp->last_line)
        {
          res += YYFPRINTF (p, "-%d", yylocp->last_line);
          if (0 <= end_col)
            res += YYFPRINTF (p, ".%d", end_col);
        }
      else if (0 <= end_col && yylocp->first_column < end_col)
        res += YYFPRINTF (p, "-%d", end_col);
    }
  return res;
 }

#  define YY_LOCATION_PRINT(File, Loc)          \
  yy_location_print_ (File, &(Loc))

# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (p, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value, Location, p); \
      YYFPRINTF (p, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, struct parser_params *p)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  YYUSE (yylocationp);
  YYUSE (p);
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
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, struct parser_params *p)
{
  YYFPRINTF (p, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  YY_LOCATION_PRINT (yyoutput, *yylocationp);
  YYFPRINTF (p, ": ");
  yy_symbol_value_print (yyoutput, yytype, yyvaluep, yylocationp, p);
  YYFPRINTF (p, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
ruby_parser_yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop, struct parser_params *p)
#define yy_stack_print(b, t) ruby_parser_yy_stack_print(b, t, p)
{
  YYFPRINTF (p, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (p, " %d", yybot);
    }
  YYFPRINTF (p, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, YYLTYPE *yylsp, int yyrule, struct parser_params *p)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (p, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (p, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                       , &(yylsp[(yyi + 1) - (yynrhs)])                       , p);
      YYFPRINTF (p, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, yylsp, Rule, p); \
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
yysyntax_error (struct parser_params *p, YYSIZE_T *yymsg_alloc, char **yymsg,
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
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep, YYLTYPE *yylocationp, struct parser_params *p)
{
  YYUSE (yyvaluep);
  YYUSE (yylocationp);
  YYUSE (p);
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
yyparse (struct parser_params *p)
{
/* The lookahead symbol.  */
int yychar;


/* The semantic value of the lookahead symbol.  */
/* Default value used for initialization, for pacifying older GCCs
   or non-GCC compilers.  */
YY_INITIAL_VALUE (static YYSTYPE yyval_default;)
YYSTYPE yylval YY_INITIAL_VALUE (= yyval_default);

/* Location data for the lookahead symbol.  */
static YYLTYPE yyloc_default
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
  = { 1, 1, 1, 1 }
# endif
;
YYLTYPE yylloc = yyloc_default;

    /* Number of syntax errors so far.  */
    int yynerrs;

    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.
       'yyls': related to locations.

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

    /* The location stack.  */
    YYLTYPE yylsa[YYINITDEPTH];
    YYLTYPE *yyls;
    YYLTYPE *yylsp;

    /* The locations where the error started and ended.  */
    YYLTYPE yyerror_range[3];

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yylsp = yyls = yylsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((p, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */

/* User initialization code.  */
#line 1065 "parse.y" /* yacc.c:1429  */
{
    RUBY_SET_YYLLOC_OF_NONE(yylloc);
}

#line 5900 "parse.c" /* yacc.c:1429  */
  yylsp[0] = yylloc;
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
        YYLTYPE *yyls1 = yyls;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yyls1, yysize * sizeof (*yylsp),
                    &yystacksize);

        yyls = yyls1;
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
        YYSTACK_RELOCATE (yyls_alloc, yyls);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YYDPRINTF ((p, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((p, "Entering state %d\n", yystate));

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
      YYDPRINTF ((p, "Reading a token: "));
      yychar = yylex (&yylval, &yylloc, p);
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((p, "Now at end of input.\n"));
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
  *++yylsp = yylloc;
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

  /* Default location.  */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 1269 "parse.y" /* yacc.c:1646  */
    {
			SET_LEX_STATE(EXPR_BEG);
			local_push(p, ifndef_ripper(1)+0);
		    }
#line 6092 "parse.c" /* yacc.c:1646  */
    break;

  case 3:
#line 1274 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			if ((yyvsp[0].node) && !compile_for_eval) {
			    NODE *node = (yyvsp[0].node);
			    /* last expression should not be void */
			    if (nd_type(node) == NODE_BLOCK) {
				while (node->nd_next) {
				    node = node->nd_next;
				}
				node = node->nd_head;
			    }
			    node = remove_begin(node);
			    void_expr(p, node);
			}
			p->eval_tree = NEW_SCOPE(0, block_append(p, p->eval_tree, (yyvsp[0].node)), &(yyloc));
		    /*% %*/
		    /*% ripper[final]: program!($2) %*/
			local_pop(p);
		    }
#line 6116 "parse.c" /* yacc.c:1646  */
    break;

  case 4:
#line 1296 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = void_stmts(p, (yyvsp[-1].node));
		    }
#line 6124 "parse.c" /* yacc.c:1646  */
    break;

  case 5:
#line 1302 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_BEGIN(0, &(yyloc));
		    /*% %*/
		    /*% ripper: stmts_add!(stmts_new!, void_stmt!) %*/
		    }
#line 6135 "parse.c" /* yacc.c:1646  */
    break;

  case 6:
#line 1309 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = newline_node((yyvsp[0].node));
		    /*% %*/
		    /*% ripper: stmts_add!(stmts_new!, $1) %*/
		    }
#line 6146 "parse.c" /* yacc.c:1646  */
    break;

  case 7:
#line 1316 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = block_append(p, (yyvsp[-2].node), newline_node((yyvsp[0].node)));
		    /*% %*/
		    /*% ripper: stmts_add!($1, $3) %*/
		    }
#line 6157 "parse.c" /* yacc.c:1646  */
    break;

  case 8:
#line 1323 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = remove_begin((yyvsp[0].node));
		    }
#line 6165 "parse.c" /* yacc.c:1646  */
    break;

  case 10:
#line 1330 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = (yyvsp[0].node);
		    }
#line 6173 "parse.c" /* yacc.c:1646  */
    break;

  case 11:
#line 1336 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			p->eval_tree_begin = block_append(p, p->eval_tree_begin,
							  NEW_BEGIN((yyvsp[-1].node), &(yyloc)));
			(yyval.node) = NEW_BEGIN(0, &(yyloc));
		    /*% %*/
		    /*% ripper: BEGIN!($2) %*/
		    }
#line 6186 "parse.c" /* yacc.c:1646  */
    break;

  case 12:
#line 1348 "parse.y" /* yacc.c:1646  */
    {if (!(yyvsp[-1].node)) {yyerror1(&(yylsp[0]), "else without rescue is useless");}}
#line 6192 "parse.c" /* yacc.c:1646  */
    break;

  case 13:
#line 1351 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_bodystmt(p, (yyvsp[-5].node), (yyvsp[-4].node), (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: bodystmt!(escape_Qundef($1), escape_Qundef($2), escape_Qundef($5), escape_Qundef($6)) %*/
		    }
#line 6203 "parse.c" /* yacc.c:1646  */
    break;

  case 14:
#line 1360 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_bodystmt(p, (yyvsp[-2].node), (yyvsp[-1].node), 0, (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: bodystmt!(escape_Qundef($1), escape_Qundef($2), Qnil, escape_Qundef($3)) %*/
		    }
#line 6214 "parse.c" /* yacc.c:1646  */
    break;

  case 15:
#line 1369 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = void_stmts(p, (yyvsp[-1].node));
		    }
#line 6222 "parse.c" /* yacc.c:1646  */
    break;

  case 16:
#line 1375 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_BEGIN(0, &(yyloc));
		    /*% %*/
		    /*% ripper: stmts_add!(stmts_new!, void_stmt!) %*/
		    }
#line 6233 "parse.c" /* yacc.c:1646  */
    break;

  case 17:
#line 1382 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = newline_node((yyvsp[0].node));
		    /*% %*/
		    /*% ripper: stmts_add!(stmts_new!, $1) %*/
		    }
#line 6244 "parse.c" /* yacc.c:1646  */
    break;

  case 18:
#line 1389 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = block_append(p, (yyvsp[-2].node), newline_node((yyvsp[0].node)));
		    /*% %*/
		    /*% ripper: stmts_add!($1, $3) %*/
		    }
#line 6255 "parse.c" /* yacc.c:1646  */
    break;

  case 19:
#line 1396 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = remove_begin((yyvsp[0].node));
		    }
#line 6263 "parse.c" /* yacc.c:1646  */
    break;

  case 20:
#line 1402 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = (yyvsp[0].node);
		    }
#line 6271 "parse.c" /* yacc.c:1646  */
    break;

  case 21:
#line 1406 "parse.y" /* yacc.c:1646  */
    {
			yyerror1(&(yylsp[0]), "BEGIN is permitted only at toplevel");
		    }
#line 6279 "parse.c" /* yacc.c:1646  */
    break;

  case 22:
#line 1410 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = (yyvsp[0].node);
		    }
#line 6287 "parse.c" /* yacc.c:1646  */
    break;

  case 23:
#line 1415 "parse.y" /* yacc.c:1646  */
    {SET_LEX_STATE(EXPR_FNAME|EXPR_FITEM);}
#line 6293 "parse.c" /* yacc.c:1646  */
    break;

  case 24:
#line 1416 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_ALIAS((yyvsp[-2].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: alias!($2, $4) %*/
		    }
#line 6304 "parse.c" /* yacc.c:1646  */
    break;

  case 25:
#line 1423 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_VALIAS((yyvsp[-1].id), (yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: var_alias!($2, $3) %*/
		    }
#line 6315 "parse.c" /* yacc.c:1646  */
    break;

  case 26:
#line 1430 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			char buf[2];
			buf[0] = '$';
			buf[1] = (char)(yyvsp[0].node)->nd_nth;
			(yyval.node) = NEW_VALIAS((yyvsp[-1].id), rb_intern2(buf, 2), &(yyloc));
		    /*% %*/
		    /*% ripper: var_alias!($2, $3) %*/
		    }
#line 6329 "parse.c" /* yacc.c:1646  */
    break;

  case 27:
#line 1440 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			yyerror1(&(yylsp[0]), "can't make alias for the number variables");
			(yyval.node) = NEW_BEGIN(0, &(yyloc));
		    /*% %*/
		    /*% ripper[error]: alias_error!(var_alias!($2, $3)) %*/
		    }
#line 6341 "parse.c" /* yacc.c:1646  */
    break;

  case 28:
#line 1448 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[0].node);
		    /*% %*/
		    /*% ripper: undef!($2) %*/
		    }
#line 6352 "parse.c" /* yacc.c:1646  */
    break;

  case 29:
#line 1455 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_if(p, (yyvsp[0].node), remove_begin((yyvsp[-2].node)), 0, &(yyloc));
			fixpos((yyval.node), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: if_mod!($3, $1) %*/
		    }
#line 6364 "parse.c" /* yacc.c:1646  */
    break;

  case 30:
#line 1463 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_unless(p, (yyvsp[0].node), remove_begin((yyvsp[-2].node)), 0, &(yyloc));
			fixpos((yyval.node), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: unless_mod!($3, $1) %*/
		    }
#line 6376 "parse.c" /* yacc.c:1646  */
    break;

  case 31:
#line 1471 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			if ((yyvsp[-2].node) && nd_type((yyvsp[-2].node)) == NODE_BEGIN) {
			    (yyval.node) = NEW_WHILE(cond(p, (yyvsp[0].node), &(yylsp[0])), (yyvsp[-2].node)->nd_body, 0, &(yyloc));
			}
			else {
			    (yyval.node) = NEW_WHILE(cond(p, (yyvsp[0].node), &(yylsp[0])), (yyvsp[-2].node), 1, &(yyloc));
			}
		    /*% %*/
		    /*% ripper: while_mod!($3, $1) %*/
		    }
#line 6392 "parse.c" /* yacc.c:1646  */
    break;

  case 32:
#line 1483 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			if ((yyvsp[-2].node) && nd_type((yyvsp[-2].node)) == NODE_BEGIN) {
			    (yyval.node) = NEW_UNTIL(cond(p, (yyvsp[0].node), &(yylsp[0])), (yyvsp[-2].node)->nd_body, 0, &(yyloc));
			}
			else {
			    (yyval.node) = NEW_UNTIL(cond(p, (yyvsp[0].node), &(yylsp[0])), (yyvsp[-2].node), 1, &(yyloc));
			}
		    /*% %*/
		    /*% ripper: until_mod!($3, $1) %*/
		    }
#line 6408 "parse.c" /* yacc.c:1646  */
    break;

  case 33:
#line 1495 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			NODE *resq;
			YYLTYPE loc = code_loc_gen(&(yylsp[-1]), &(yylsp[0]));
			resq = NEW_RESBODY(0, remove_begin((yyvsp[0].node)), 0, &loc);
			(yyval.node) = NEW_RESCUE(remove_begin((yyvsp[-2].node)), resq, 0, &(yyloc));
		    /*% %*/
		    /*% ripper: rescue_mod!($1, $3) %*/
		    }
#line 6422 "parse.c" /* yacc.c:1646  */
    break;

  case 34:
#line 1505 "parse.y" /* yacc.c:1646  */
    {
			if (p->ctxt.in_def) {
			    rb_warn0("END in method; use at_exit");
			}
		    /*%%%*/
			{
			    NODE *scope = NEW_NODE(
				NODE_SCOPE, 0 /* tbl */, (yyvsp[-1].node) /* body */, 0 /* args */, &(yyloc));
			    (yyval.node) = NEW_POSTEXE(scope, &(yyloc));
			}
		    /*% %*/
		    /*% ripper: END!($3) %*/
		    }
#line 6440 "parse.c" /* yacc.c:1646  */
    break;

  case 36:
#line 1520 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			value_expr((yyvsp[0].node));
			(yyval.node) = node_assign(p, (yyvsp[-2].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: massign!($1, $3) %*/
		    }
#line 6452 "parse.c" /* yacc.c:1646  */
    break;

  case 37:
#line 1528 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			value_expr((yyvsp[0].node));
			(yyval.node) = node_assign(p, (yyvsp[-2].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: assign!($1, $3) %*/
		    }
#line 6464 "parse.c" /* yacc.c:1646  */
    break;

  case 38:
#line 1536 "parse.y" /* yacc.c:1646  */
    {
                    /*%%%*/
                        YYLTYPE loc = code_loc_gen(&(yylsp[-1]), &(yylsp[0]));
                        value_expr((yyvsp[-2].node));
			(yyval.node) = node_assign(p, (yyvsp[-4].node), NEW_RESCUE((yyvsp[-2].node), NEW_RESBODY(0, remove_begin((yyvsp[0].node)), 0, &loc), 0, &(yyloc)), &(yyloc));
                    /*% %*/
                    /*% ripper: massign!($1, rescue_mod!($3, $5)) %*/
                    }
#line 6477 "parse.c" /* yacc.c:1646  */
    break;

  case 39:
#line 1545 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = node_assign(p, (yyvsp[-2].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: massign!($1, $3) %*/
		    }
#line 6488 "parse.c" /* yacc.c:1646  */
    break;

  case 41:
#line 1555 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = node_assign(p, (yyvsp[-2].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: assign!($1, $3) %*/
		    }
#line 6499 "parse.c" /* yacc.c:1646  */
    break;

  case 42:
#line 1562 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_op_assign(p, (yyvsp[-2].node), (yyvsp[-1].id), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: opassign!($1, $2, $3) %*/
		    }
#line 6510 "parse.c" /* yacc.c:1646  */
    break;

  case 43:
#line 1569 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_ary_op_assign(p, (yyvsp[-5].node), (yyvsp[-3].node), (yyvsp[-1].id), (yyvsp[0].node), &(yylsp[-3]), &(yyloc));
		    /*% %*/
		    /*% ripper: opassign!(aref_field!($1, escape_Qundef($3)), $5, $6) %*/

		    }
#line 6522 "parse.c" /* yacc.c:1646  */
    break;

  case 44:
#line 1577 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_attr_op_assign(p, (yyvsp[-4].node), (yyvsp[-3].id), (yyvsp[-2].id), (yyvsp[-1].id), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: opassign!(field!($1, $2, $3), $4, $5) %*/
		    }
#line 6533 "parse.c" /* yacc.c:1646  */
    break;

  case 45:
#line 1584 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_attr_op_assign(p, (yyvsp[-4].node), (yyvsp[-3].id), (yyvsp[-2].id), (yyvsp[-1].id), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: opassign!(field!($1, $2, $3), $4, $5) %*/
		    }
#line 6544 "parse.c" /* yacc.c:1646  */
    break;

  case 46:
#line 1591 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			YYLTYPE loc = code_loc_gen(&(yylsp[-4]), &(yylsp[-2]));
			(yyval.node) = new_const_op_assign(p, NEW_COLON2((yyvsp[-4].node), (yyvsp[-2].id), &loc), (yyvsp[-1].id), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: opassign!(const_path_field!($1, $3), $4, $5) %*/
		    }
#line 6556 "parse.c" /* yacc.c:1646  */
    break;

  case 47:
#line 1599 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_attr_op_assign(p, (yyvsp[-4].node), ID2VAL(idCOLON2), (yyvsp[-2].id), (yyvsp[-1].id), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: opassign!(field!($1, ID2VAL(idCOLON2), $3), $4, $5) %*/
		    }
#line 6567 "parse.c" /* yacc.c:1646  */
    break;

  case 48:
#line 1606 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			rb_backref_error(p, (yyvsp[-2].node));
			(yyval.node) = NEW_BEGIN(0, &(yyloc));
		    /*% %*/
		    /*% ripper[error]: assign_error!(assign!(var_field(p, $1), $3)) %*/
		    }
#line 6579 "parse.c" /* yacc.c:1646  */
    break;

  case 49:
#line 1616 "parse.y" /* yacc.c:1646  */
    {
			value_expr((yyvsp[0].node));
			(yyval.node) = (yyvsp[0].node);
		    }
#line 6588 "parse.c" /* yacc.c:1646  */
    break;

  case 50:
#line 1621 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			YYLTYPE loc = code_loc_gen(&(yylsp[-1]), &(yylsp[0]));
			value_expr((yyvsp[-2].node));
			(yyval.node) = NEW_RESCUE((yyvsp[-2].node), NEW_RESBODY(0, remove_begin((yyvsp[0].node)), 0, &loc), 0, &(yyloc));
		    /*% %*/
		    /*% ripper: rescue_mod!($1, $3) %*/
		    }
#line 6601 "parse.c" /* yacc.c:1646  */
    break;

  case 53:
#line 1634 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = logop(p, idAND, (yyvsp[-2].node), (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 6609 "parse.c" /* yacc.c:1646  */
    break;

  case 54:
#line 1638 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = logop(p, idOR, (yyvsp[-2].node), (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 6617 "parse.c" /* yacc.c:1646  */
    break;

  case 55:
#line 1642 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = call_uni_op(p, method_cond(p, (yyvsp[0].node), &(yylsp[0])), METHOD_NOT, &(yylsp[-2]), &(yyloc));
		    }
#line 6625 "parse.c" /* yacc.c:1646  */
    break;

  case 56:
#line 1646 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = call_uni_op(p, method_cond(p, (yyvsp[0].node), &(yylsp[0])), '!', &(yylsp[-1]), &(yyloc));
		    }
#line 6633 "parse.c" /* yacc.c:1646  */
    break;

  case 57:
#line 1650 "parse.y" /* yacc.c:1646  */
    {
			value_expr((yyvsp[-1].node));
			SET_LEX_STATE(EXPR_BEG|EXPR_LABEL);
			p->command_start = FALSE;
			(yyval.ctxt) = p->ctxt;
			p->ctxt.in_kwarg = 1;
		    }
#line 6645 "parse.c" /* yacc.c:1646  */
    break;

  case 58:
#line 1657 "parse.y" /* yacc.c:1646  */
    {(yyval.tbl) = push_pvtbl(p);}
#line 6651 "parse.c" /* yacc.c:1646  */
    break;

  case 59:
#line 1659 "parse.y" /* yacc.c:1646  */
    {pop_pvtbl(p, (yyvsp[-1].tbl));}
#line 6657 "parse.c" /* yacc.c:1646  */
    break;

  case 60:
#line 1660 "parse.y" /* yacc.c:1646  */
    {
			p->ctxt.in_kwarg = (yyvsp[-3].ctxt).in_kwarg;
		    /*%%%*/
			(yyval.node) = NEW_CASE3((yyvsp[-5].node), NEW_IN((yyvsp[-1].node), 0, 0, &(yylsp[-1])), &(yyloc));

			if (rb_warning_category_enabled_p(RB_WARN_CATEGORY_EXPERIMENTAL))
			    rb_warn0L(nd_line((yyval.node)), "One-line pattern matching is experimental, and the behavior may change in future versions of Ruby!");

		    /*% %*/
		    /*% ripper: case!($1, in!($5, Qnil, Qnil)) %*/
		    }
#line 6673 "parse.c" /* yacc.c:1646  */
    break;

  case 62:
#line 1675 "parse.y" /* yacc.c:1646  */
    {
			ID fname = get_id((yyvsp[0].id));
			ID cur_arg = p->cur_arg;
			YYSTYPE c = {.ctxt = p->ctxt};
			numparam_name(p, fname);
			local_push(p, 0);
			p->cur_arg = 0;
			p->ctxt.in_def = 1;
			(yyval.node) = NEW_NODE(NODE_SELF, /*vid*/cur_arg, /*mid*/fname, /*cval*/c.val, &(yyloc));
		    /*%%%*/
		    /*%
			$$ = NEW_RIPPER(fname, get_value($1), $$, &NULL_LOC);
		    %*/
		    }
#line 6692 "parse.c" /* yacc.c:1646  */
    break;

  case 63:
#line 1692 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = (yyvsp[0].node);
		    /*%%%*/
			(yyval.node) = NEW_NODE(NODE_DEFN, 0, (yyval.node)->nd_mid, (yyval.node), &(yyloc));
		    /*% %*/
		    }
#line 6703 "parse.c" /* yacc.c:1646  */
    break;

  case 64:
#line 1700 "parse.y" /* yacc.c:1646  */
    {SET_LEX_STATE(EXPR_FNAME);}
#line 6709 "parse.c" /* yacc.c:1646  */
    break;

  case 65:
#line 1701 "parse.y" /* yacc.c:1646  */
    {
			SET_LEX_STATE(EXPR_ENDFN|EXPR_LABEL); /* force for args */
			(yyval.node) = (yyvsp[0].node);
		    /*%%%*/
			(yyval.node) = NEW_NODE(NODE_DEFS, (yyvsp[-3].node), (yyval.node)->nd_mid, (yyval.node), &(yyloc));
		    /*%
			VALUE ary = rb_ary_new_from_args(3, $2, $3, get_value($$));
			add_mark_object(p, ary);
			$<node>$->nd_rval = ary;
		    %*/
		    }
#line 6725 "parse.c" /* yacc.c:1646  */
    break;

  case 66:
#line 1715 "parse.y" /* yacc.c:1646  */
    {
			value_expr((yyvsp[0].node));
			(yyval.node) = (yyvsp[0].node);
		    }
#line 6734 "parse.c" /* yacc.c:1646  */
    break;

  case 67:
#line 1721 "parse.y" /* yacc.c:1646  */
    {COND_PUSH(1);}
#line 6740 "parse.c" /* yacc.c:1646  */
    break;

  case 68:
#line 1721 "parse.y" /* yacc.c:1646  */
    {COND_POP();}
#line 6746 "parse.c" /* yacc.c:1646  */
    break;

  case 69:
#line 1722 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = (yyvsp[-2].node);
		    }
#line 6754 "parse.c" /* yacc.c:1646  */
    break;

  case 73:
#line 1733 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_qcall(p, (yyvsp[-2].id), (yyvsp[-3].node), (yyvsp[-1].id), (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    /*% %*/
		    /*% ripper: method_add_arg!(call!($1, $2, $3), $4) %*/
		    }
#line 6765 "parse.c" /* yacc.c:1646  */
    break;

  case 74:
#line 1742 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = (yyvsp[-1].node);
		    /*%%%*/
			(yyval.node)->nd_body->nd_loc = code_loc_gen(&(yylsp[-2]), &(yylsp[0]));
			nd_set_line((yyval.node), (yylsp[-2]).end_pos.lineno);
		    /*% %*/
		    }
#line 6777 "parse.c" /* yacc.c:1646  */
    break;

  case 75:
#line 1752 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_FCALL((yyvsp[0].id), 0, &(yyloc));
			nd_set_line((yyval.node), p->tokline);
		    /*% %*/
		    /*% ripper: $1 %*/
		    }
#line 6789 "parse.c" /* yacc.c:1646  */
    break;

  case 76:
#line 1762 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyvsp[-1].node)->nd_args = (yyvsp[0].node);
			nd_set_last_loc((yyvsp[-1].node), (yylsp[0]).end_pos);
			(yyval.node) = (yyvsp[-1].node);
		    /*% %*/
		    /*% ripper: command!($1, $2) %*/
		    }
#line 6802 "parse.c" /* yacc.c:1646  */
    break;

  case 77:
#line 1771 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			block_dup_check(p, (yyvsp[-1].node), (yyvsp[0].node));
			(yyvsp[-2].node)->nd_args = (yyvsp[-1].node);
			(yyval.node) = method_add_block(p, (yyvsp[-2].node), (yyvsp[0].node), &(yyloc));
			fixpos((yyval.node), (yyvsp[-2].node));
			nd_set_last_loc((yyvsp[-2].node), (yylsp[-1]).end_pos);
		    /*% %*/
		    /*% ripper: method_add_block!(command!($1, $2), $3) %*/
		    }
#line 6817 "parse.c" /* yacc.c:1646  */
    break;

  case 78:
#line 1782 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_command_qcall(p, (yyvsp[-2].id), (yyvsp[-3].node), (yyvsp[-1].id), (yyvsp[0].node), Qnull, &(yylsp[-1]), &(yyloc));
		    /*% %*/
		    /*% ripper: command_call!($1, $2, $3, $4) %*/
		    }
#line 6828 "parse.c" /* yacc.c:1646  */
    break;

  case 79:
#line 1789 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_command_qcall(p, (yyvsp[-3].id), (yyvsp[-4].node), (yyvsp[-2].id), (yyvsp[-1].node), (yyvsp[0].node), &(yylsp[-2]), &(yyloc));
		    /*% %*/
		    /*% ripper: method_add_block!(command_call!($1, $2, $3, $4), $5) %*/
		    }
#line 6839 "parse.c" /* yacc.c:1646  */
    break;

  case 80:
#line 1796 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_command_qcall(p, ID2VAL(idCOLON2), (yyvsp[-3].node), (yyvsp[-1].id), (yyvsp[0].node), Qnull, &(yylsp[-1]), &(yyloc));
		    /*% %*/
		    /*% ripper: command_call!($1, ID2VAL(idCOLON2), $3, $4) %*/
		    }
#line 6850 "parse.c" /* yacc.c:1646  */
    break;

  case 81:
#line 1803 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_command_qcall(p, ID2VAL(idCOLON2), (yyvsp[-4].node), (yyvsp[-2].id), (yyvsp[-1].node), (yyvsp[0].node), &(yylsp[-2]), &(yyloc));
		    /*% %*/
		    /*% ripper: method_add_block!(command_call!($1, ID2VAL(idCOLON2), $3, $4), $5) %*/
		   }
#line 6861 "parse.c" /* yacc.c:1646  */
    break;

  case 82:
#line 1810 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_SUPER((yyvsp[0].node), &(yyloc));
			fixpos((yyval.node), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: super!($2) %*/
		    }
#line 6873 "parse.c" /* yacc.c:1646  */
    break;

  case 83:
#line 1818 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_yield(p, (yyvsp[0].node), &(yyloc));
			fixpos((yyval.node), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: yield!($2) %*/
		    }
#line 6885 "parse.c" /* yacc.c:1646  */
    break;

  case 84:
#line 1826 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_RETURN(ret_args(p, (yyvsp[0].node)), &(yyloc));
		    /*% %*/
		    /*% ripper: return!($2) %*/
		    }
#line 6896 "parse.c" /* yacc.c:1646  */
    break;

  case 85:
#line 1833 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_BREAK(ret_args(p, (yyvsp[0].node)), &(yyloc));
		    /*% %*/
		    /*% ripper: break!($2) %*/
		    }
#line 6907 "parse.c" /* yacc.c:1646  */
    break;

  case 86:
#line 1840 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_NEXT(ret_args(p, (yyvsp[0].node)), &(yyloc));
		    /*% %*/
		    /*% ripper: next!($2) %*/
		    }
#line 6918 "parse.c" /* yacc.c:1646  */
    break;

  case 88:
#line 1850 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node);
		    /*% %*/
		    /*% ripper: mlhs_paren!($2) %*/
		    }
#line 6929 "parse.c" /* yacc.c:1646  */
    break;

  case 90:
#line 1860 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN(NEW_LIST((yyvsp[-1].node), &(yyloc)), 0, &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_paren!($2) %*/
		    }
#line 6940 "parse.c" /* yacc.c:1646  */
    break;

  case 91:
#line 1869 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN((yyvsp[0].node), 0, &(yyloc));
		    /*% %*/
		    /*% ripper: $1 %*/
		    }
#line 6951 "parse.c" /* yacc.c:1646  */
    break;

  case 92:
#line 1876 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN(list_append(p, (yyvsp[-1].node),(yyvsp[0].node)), 0, &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_add!($1, $2) %*/
		    }
#line 6962 "parse.c" /* yacc.c:1646  */
    break;

  case 93:
#line 1883 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN((yyvsp[-2].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_add_star!($1, $3) %*/
		    }
#line 6973 "parse.c" /* yacc.c:1646  */
    break;

  case 94:
#line 1890 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN((yyvsp[-4].node), NEW_POSTARG((yyvsp[-2].node),(yyvsp[0].node),&(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_add_post!(mlhs_add_star!($1, $3), $5) %*/
		    }
#line 6984 "parse.c" /* yacc.c:1646  */
    break;

  case 95:
#line 1897 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN((yyvsp[-1].node), NODE_SPECIAL_NO_NAME_REST, &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_add_star!($1, Qnil) %*/
		    }
#line 6995 "parse.c" /* yacc.c:1646  */
    break;

  case 96:
#line 1904 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN((yyvsp[-3].node), NEW_POSTARG(NODE_SPECIAL_NO_NAME_REST, (yyvsp[0].node), &(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_add_post!(mlhs_add_star!($1, Qnil), $4) %*/
		    }
#line 7006 "parse.c" /* yacc.c:1646  */
    break;

  case 97:
#line 1911 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN(0, (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_add_star!(mlhs_new!, $2) %*/
		    }
#line 7017 "parse.c" /* yacc.c:1646  */
    break;

  case 98:
#line 1918 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN(0, NEW_POSTARG((yyvsp[-2].node),(yyvsp[0].node),&(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_add_post!(mlhs_add_star!(mlhs_new!, $2), $4) %*/
		    }
#line 7028 "parse.c" /* yacc.c:1646  */
    break;

  case 99:
#line 1925 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN(0, NODE_SPECIAL_NO_NAME_REST, &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_add_star!(mlhs_new!, Qnil) %*/
		    }
#line 7039 "parse.c" /* yacc.c:1646  */
    break;

  case 100:
#line 1932 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN(0, NEW_POSTARG(NODE_SPECIAL_NO_NAME_REST, (yyvsp[0].node), &(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_add_post!(mlhs_add_star!(mlhs_new!, Qnil), $3) %*/
		    }
#line 7050 "parse.c" /* yacc.c:1646  */
    break;

  case 102:
#line 1942 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node);
		    /*% %*/
		    /*% ripper: mlhs_paren!($2) %*/
		    }
#line 7061 "parse.c" /* yacc.c:1646  */
    break;

  case 103:
#line 1951 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_LIST((yyvsp[-1].node), &(yylsp[-1]));
		    /*% %*/
		    /*% ripper: mlhs_add!(mlhs_new!, $1) %*/
		    }
#line 7072 "parse.c" /* yacc.c:1646  */
    break;

  case 104:
#line 1958 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = list_append(p, (yyvsp[-2].node), (yyvsp[-1].node));
		    /*% %*/
		    /*% ripper: mlhs_add!($1, $2) %*/
		    }
#line 7083 "parse.c" /* yacc.c:1646  */
    break;

  case 105:
#line 1967 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_LIST((yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_add!(mlhs_new!, $1) %*/
		    }
#line 7094 "parse.c" /* yacc.c:1646  */
    break;

  case 106:
#line 1974 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = list_append(p, (yyvsp[-2].node), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: mlhs_add!($1, $3) %*/
		    }
#line 7105 "parse.c" /* yacc.c:1646  */
    break;

  case 107:
#line 1983 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = assignable(p, (yyvsp[0].id), 0, &(yyloc));
		    /*% %*/
		    /*% ripper: assignable(p, var_field(p, $1)) %*/
		    }
#line 7116 "parse.c" /* yacc.c:1646  */
    break;

  case 108:
#line 1990 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = assignable(p, (yyvsp[0].id), 0, &(yyloc));
		    /*% %*/
		    /*% ripper: assignable(p, var_field(p, $1)) %*/
		    }
#line 7127 "parse.c" /* yacc.c:1646  */
    break;

  case 109:
#line 1997 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = aryset(p, (yyvsp[-3].node), (yyvsp[-1].node), &(yyloc));
		    /*% %*/
		    /*% ripper: aref_field!($1, escape_Qundef($3)) %*/
		    }
#line 7138 "parse.c" /* yacc.c:1646  */
    break;

  case 110:
#line 2004 "parse.y" /* yacc.c:1646  */
    {
			if ((yyvsp[-1].id) == tANDDOT) {
			    yyerror1(&(yylsp[-1]), "&. inside multiple assignment destination");
			}
		    /*%%%*/
			(yyval.node) = attrset(p, (yyvsp[-2].node), (yyvsp[-1].id), (yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: field!($1, $2, $3) %*/
		    }
#line 7152 "parse.c" /* yacc.c:1646  */
    break;

  case 111:
#line 2014 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = attrset(p, (yyvsp[-2].node), idCOLON2, (yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: const_path_field!($1, $3) %*/
		    }
#line 7163 "parse.c" /* yacc.c:1646  */
    break;

  case 112:
#line 2021 "parse.y" /* yacc.c:1646  */
    {
			if ((yyvsp[-1].id) == tANDDOT) {
			    yyerror1(&(yylsp[-1]), "&. inside multiple assignment destination");
			}
		    /*%%%*/
			(yyval.node) = attrset(p, (yyvsp[-2].node), (yyvsp[-1].id), (yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: field!($1, $2, $3) %*/
		    }
#line 7177 "parse.c" /* yacc.c:1646  */
    break;

  case 113:
#line 2031 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = const_decl(p, NEW_COLON2((yyvsp[-2].node), (yyvsp[0].id), &(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: const_decl(p, const_path_field!($1, $3)) %*/
		    }
#line 7188 "parse.c" /* yacc.c:1646  */
    break;

  case 114:
#line 2038 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = const_decl(p, NEW_COLON3((yyvsp[0].id), &(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: const_decl(p, top_const_field!($2)) %*/
		    }
#line 7199 "parse.c" /* yacc.c:1646  */
    break;

  case 115:
#line 2045 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			rb_backref_error(p, (yyvsp[0].node));
			(yyval.node) = NEW_BEGIN(0, &(yyloc));
		    /*% %*/
		    /*% ripper[error]: assign_error!(var_field(p, $1)) %*/
		    }
#line 7211 "parse.c" /* yacc.c:1646  */
    break;

  case 116:
#line 2055 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = assignable(p, (yyvsp[0].id), 0, &(yyloc));
		    /*% %*/
		    /*% ripper: assignable(p, var_field(p, $1)) %*/
		    }
#line 7222 "parse.c" /* yacc.c:1646  */
    break;

  case 117:
#line 2062 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = assignable(p, (yyvsp[0].id), 0, &(yyloc));
		    /*% %*/
		    /*% ripper: assignable(p, var_field(p, $1)) %*/
		    }
#line 7233 "parse.c" /* yacc.c:1646  */
    break;

  case 118:
#line 2069 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = aryset(p, (yyvsp[-3].node), (yyvsp[-1].node), &(yyloc));
		    /*% %*/
		    /*% ripper: aref_field!($1, escape_Qundef($3)) %*/
		    }
#line 7244 "parse.c" /* yacc.c:1646  */
    break;

  case 119:
#line 2076 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = attrset(p, (yyvsp[-2].node), (yyvsp[-1].id), (yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: field!($1, $2, $3) %*/
		    }
#line 7255 "parse.c" /* yacc.c:1646  */
    break;

  case 120:
#line 2083 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = attrset(p, (yyvsp[-2].node), idCOLON2, (yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: field!($1, ID2VAL(idCOLON2), $3) %*/
		    }
#line 7266 "parse.c" /* yacc.c:1646  */
    break;

  case 121:
#line 2090 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = attrset(p, (yyvsp[-2].node), (yyvsp[-1].id), (yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: field!($1, $2, $3) %*/
		    }
#line 7277 "parse.c" /* yacc.c:1646  */
    break;

  case 122:
#line 2097 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = const_decl(p, NEW_COLON2((yyvsp[-2].node), (yyvsp[0].id), &(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: const_decl(p, const_path_field!($1, $3)) %*/
		    }
#line 7288 "parse.c" /* yacc.c:1646  */
    break;

  case 123:
#line 2104 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = const_decl(p, NEW_COLON3((yyvsp[0].id), &(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: const_decl(p, top_const_field!($2)) %*/
		    }
#line 7299 "parse.c" /* yacc.c:1646  */
    break;

  case 124:
#line 2111 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			rb_backref_error(p, (yyvsp[0].node));
			(yyval.node) = NEW_BEGIN(0, &(yyloc));
		    /*% %*/
		    /*% ripper[error]: assign_error!(var_field(p, $1)) %*/
		    }
#line 7311 "parse.c" /* yacc.c:1646  */
    break;

  case 125:
#line 2121 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			yyerror1(&(yylsp[0]), "class/module name must be CONSTANT");
		    /*% %*/
		    /*% ripper[error]: class_name_error!($1) %*/
		    }
#line 7322 "parse.c" /* yacc.c:1646  */
    break;

  case 127:
#line 2131 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_COLON3((yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: top_const_ref!($2) %*/
		    }
#line 7333 "parse.c" /* yacc.c:1646  */
    break;

  case 128:
#line 2138 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_COLON2(0, (yyval.node), &(yyloc));
		    /*% %*/
		    /*% ripper: const_ref!($1) %*/
		    }
#line 7344 "parse.c" /* yacc.c:1646  */
    break;

  case 129:
#line 2145 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_COLON2((yyvsp[-2].node), (yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: const_path_ref!($1, $3) %*/
		    }
#line 7355 "parse.c" /* yacc.c:1646  */
    break;

  case 133:
#line 2157 "parse.y" /* yacc.c:1646  */
    {
			SET_LEX_STATE(EXPR_ENDFN);
			(yyval.id) = (yyvsp[0].id);
		    }
#line 7364 "parse.c" /* yacc.c:1646  */
    break;

  case 135:
#line 2165 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_LIT(ID2SYM((yyvsp[0].id)), &(yyloc));
		    /*% %*/
		    /*% ripper: symbol_literal!($1) %*/
		    }
#line 7375 "parse.c" /* yacc.c:1646  */
    break;

  case 137:
#line 2175 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_UNDEF((yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: rb_ary_new3(1, get_value($1)) %*/
		    }
#line 7386 "parse.c" /* yacc.c:1646  */
    break;

  case 138:
#line 2181 "parse.y" /* yacc.c:1646  */
    {SET_LEX_STATE(EXPR_FNAME|EXPR_FITEM);}
#line 7392 "parse.c" /* yacc.c:1646  */
    break;

  case 139:
#line 2182 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			NODE *undef = NEW_UNDEF((yyvsp[0].node), &(yylsp[0]));
			(yyval.node) = block_append(p, (yyvsp[-3].node), undef);
		    /*% %*/
		    /*% ripper: rb_ary_push($1, get_value($4)) %*/
		    }
#line 7404 "parse.c" /* yacc.c:1646  */
    break;

  case 140:
#line 2191 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = '|'); }
#line 7410 "parse.c" /* yacc.c:1646  */
    break;

  case 141:
#line 2192 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = '^'); }
#line 7416 "parse.c" /* yacc.c:1646  */
    break;

  case 142:
#line 2193 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = '&'); }
#line 7422 "parse.c" /* yacc.c:1646  */
    break;

  case 143:
#line 2194 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = tCMP); }
#line 7428 "parse.c" /* yacc.c:1646  */
    break;

  case 144:
#line 2195 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = tEQ); }
#line 7434 "parse.c" /* yacc.c:1646  */
    break;

  case 145:
#line 2196 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = tEQQ); }
#line 7440 "parse.c" /* yacc.c:1646  */
    break;

  case 146:
#line 2197 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = tMATCH); }
#line 7446 "parse.c" /* yacc.c:1646  */
    break;

  case 147:
#line 2198 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = tNMATCH); }
#line 7452 "parse.c" /* yacc.c:1646  */
    break;

  case 148:
#line 2199 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = '>'); }
#line 7458 "parse.c" /* yacc.c:1646  */
    break;

  case 149:
#line 2200 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = tGEQ); }
#line 7464 "parse.c" /* yacc.c:1646  */
    break;

  case 150:
#line 2201 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = '<'); }
#line 7470 "parse.c" /* yacc.c:1646  */
    break;

  case 151:
#line 2202 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = tLEQ); }
#line 7476 "parse.c" /* yacc.c:1646  */
    break;

  case 152:
#line 2203 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = tNEQ); }
#line 7482 "parse.c" /* yacc.c:1646  */
    break;

  case 153:
#line 2204 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = tLSHFT); }
#line 7488 "parse.c" /* yacc.c:1646  */
    break;

  case 154:
#line 2205 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = tRSHFT); }
#line 7494 "parse.c" /* yacc.c:1646  */
    break;

  case 155:
#line 2206 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = '+'); }
#line 7500 "parse.c" /* yacc.c:1646  */
    break;

  case 156:
#line 2207 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = '-'); }
#line 7506 "parse.c" /* yacc.c:1646  */
    break;

  case 157:
#line 2208 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = '*'); }
#line 7512 "parse.c" /* yacc.c:1646  */
    break;

  case 158:
#line 2209 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = '*'); }
#line 7518 "parse.c" /* yacc.c:1646  */
    break;

  case 159:
#line 2210 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = '/'); }
#line 7524 "parse.c" /* yacc.c:1646  */
    break;

  case 160:
#line 2211 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = '%'); }
#line 7530 "parse.c" /* yacc.c:1646  */
    break;

  case 161:
#line 2212 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = tPOW); }
#line 7536 "parse.c" /* yacc.c:1646  */
    break;

  case 162:
#line 2213 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = tDSTAR); }
#line 7542 "parse.c" /* yacc.c:1646  */
    break;

  case 163:
#line 2214 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = '!'); }
#line 7548 "parse.c" /* yacc.c:1646  */
    break;

  case 164:
#line 2215 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = '~'); }
#line 7554 "parse.c" /* yacc.c:1646  */
    break;

  case 165:
#line 2216 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = tUPLUS); }
#line 7560 "parse.c" /* yacc.c:1646  */
    break;

  case 166:
#line 2217 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = tUMINUS); }
#line 7566 "parse.c" /* yacc.c:1646  */
    break;

  case 167:
#line 2218 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = tAREF); }
#line 7572 "parse.c" /* yacc.c:1646  */
    break;

  case 168:
#line 2219 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = tASET); }
#line 7578 "parse.c" /* yacc.c:1646  */
    break;

  case 169:
#line 2220 "parse.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.id) = '`'); }
#line 7584 "parse.c" /* yacc.c:1646  */
    break;

  case 211:
#line 2238 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = node_assign(p, (yyvsp[-2].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: assign!($1, $3) %*/
		    }
#line 7595 "parse.c" /* yacc.c:1646  */
    break;

  case 212:
#line 2245 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_op_assign(p, (yyvsp[-2].node), (yyvsp[-1].id), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: opassign!($1, $2, $3) %*/
		    }
#line 7606 "parse.c" /* yacc.c:1646  */
    break;

  case 213:
#line 2252 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_ary_op_assign(p, (yyvsp[-5].node), (yyvsp[-3].node), (yyvsp[-1].id), (yyvsp[0].node), &(yylsp[-3]), &(yyloc));
		    /*% %*/
		    /*% ripper: opassign!(aref_field!($1, escape_Qundef($3)), $5, $6) %*/
		    }
#line 7617 "parse.c" /* yacc.c:1646  */
    break;

  case 214:
#line 2259 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_attr_op_assign(p, (yyvsp[-4].node), (yyvsp[-3].id), (yyvsp[-2].id), (yyvsp[-1].id), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: opassign!(field!($1, $2, $3), $4, $5) %*/
		    }
#line 7628 "parse.c" /* yacc.c:1646  */
    break;

  case 215:
#line 2266 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_attr_op_assign(p, (yyvsp[-4].node), (yyvsp[-3].id), (yyvsp[-2].id), (yyvsp[-1].id), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: opassign!(field!($1, $2, $3), $4, $5) %*/
		    }
#line 7639 "parse.c" /* yacc.c:1646  */
    break;

  case 216:
#line 2273 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_attr_op_assign(p, (yyvsp[-4].node), ID2VAL(idCOLON2), (yyvsp[-2].id), (yyvsp[-1].id), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: opassign!(field!($1, ID2VAL(idCOLON2), $3), $4, $5) %*/
		    }
#line 7650 "parse.c" /* yacc.c:1646  */
    break;

  case 217:
#line 2280 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			YYLTYPE loc = code_loc_gen(&(yylsp[-4]), &(yylsp[-2]));
			(yyval.node) = new_const_op_assign(p, NEW_COLON2((yyvsp[-4].node), (yyvsp[-2].id), &loc), (yyvsp[-1].id), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: opassign!(const_path_field!($1, $3), $4, $5) %*/
		    }
#line 7662 "parse.c" /* yacc.c:1646  */
    break;

  case 218:
#line 2288 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_const_op_assign(p, NEW_COLON3((yyvsp[-2].id), &(yyloc)), (yyvsp[-1].id), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: opassign!(top_const_field!($2), $3, $4) %*/
		    }
#line 7673 "parse.c" /* yacc.c:1646  */
    break;

  case 219:
#line 2295 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			rb_backref_error(p, (yyvsp[-2].node));
			(yyval.node) = NEW_BEGIN(0, &(yyloc));
		    /*% %*/
		    /*% ripper[error]: assign_error!(opassign!(var_field(p, $1), $2, $3)) %*/
		    }
#line 7685 "parse.c" /* yacc.c:1646  */
    break;

  case 220:
#line 2303 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			value_expr((yyvsp[-2].node));
			value_expr((yyvsp[0].node));
			(yyval.node) = NEW_DOT2((yyvsp[-2].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: dot2!($1, $3) %*/
		    }
#line 7698 "parse.c" /* yacc.c:1646  */
    break;

  case 221:
#line 2312 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			value_expr((yyvsp[-2].node));
			value_expr((yyvsp[0].node));
			(yyval.node) = NEW_DOT3((yyvsp[-2].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: dot3!($1, $3) %*/
		    }
#line 7711 "parse.c" /* yacc.c:1646  */
    break;

  case 222:
#line 2321 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			value_expr((yyvsp[-1].node));
			(yyval.node) = NEW_DOT2((yyvsp[-1].node), new_nil_at(p, &(yylsp[0]).end_pos), &(yyloc));
		    /*% %*/
		    /*% ripper: dot2!($1, Qnil) %*/
		    }
#line 7723 "parse.c" /* yacc.c:1646  */
    break;

  case 223:
#line 2329 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			value_expr((yyvsp[-1].node));
			(yyval.node) = NEW_DOT3((yyvsp[-1].node), new_nil_at(p, &(yylsp[0]).end_pos), &(yyloc));
		    /*% %*/
		    /*% ripper: dot3!($1, Qnil) %*/
		    }
#line 7735 "parse.c" /* yacc.c:1646  */
    break;

  case 224:
#line 2337 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			value_expr((yyvsp[0].node));
			(yyval.node) = NEW_DOT2(new_nil_at(p, &(yylsp[-1]).beg_pos), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: dot2!(Qnil, $2) %*/
		    }
#line 7747 "parse.c" /* yacc.c:1646  */
    break;

  case 225:
#line 2345 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			value_expr((yyvsp[0].node));
			(yyval.node) = NEW_DOT3(new_nil_at(p, &(yylsp[-1]).beg_pos), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: dot3!(Qnil, $2) %*/
		    }
#line 7759 "parse.c" /* yacc.c:1646  */
    break;

  case 226:
#line 2353 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), '+', (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7767 "parse.c" /* yacc.c:1646  */
    break;

  case 227:
#line 2357 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), '-', (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7775 "parse.c" /* yacc.c:1646  */
    break;

  case 228:
#line 2361 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), '*', (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7783 "parse.c" /* yacc.c:1646  */
    break;

  case 229:
#line 2365 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), '/', (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7791 "parse.c" /* yacc.c:1646  */
    break;

  case 230:
#line 2369 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), '%', (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7799 "parse.c" /* yacc.c:1646  */
    break;

  case 231:
#line 2373 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), idPow, (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7807 "parse.c" /* yacc.c:1646  */
    break;

  case 232:
#line 2377 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = call_uni_op(p, call_bin_op(p, (yyvsp[-2].node), idPow, (yyvsp[0].node), &(yylsp[-2]), &(yyloc)), idUMinus, &(yylsp[-3]), &(yyloc));
		    }
#line 7815 "parse.c" /* yacc.c:1646  */
    break;

  case 233:
#line 2381 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = call_uni_op(p, (yyvsp[0].node), idUPlus, &(yylsp[-1]), &(yyloc));
		    }
#line 7823 "parse.c" /* yacc.c:1646  */
    break;

  case 234:
#line 2385 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = call_uni_op(p, (yyvsp[0].node), idUMinus, &(yylsp[-1]), &(yyloc));
		    }
#line 7831 "parse.c" /* yacc.c:1646  */
    break;

  case 235:
#line 2389 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), '|', (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7839 "parse.c" /* yacc.c:1646  */
    break;

  case 236:
#line 2393 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), '^', (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7847 "parse.c" /* yacc.c:1646  */
    break;

  case 237:
#line 2397 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), '&', (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7855 "parse.c" /* yacc.c:1646  */
    break;

  case 238:
#line 2401 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), idCmp, (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7863 "parse.c" /* yacc.c:1646  */
    break;

  case 240:
#line 2406 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), idEq, (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7871 "parse.c" /* yacc.c:1646  */
    break;

  case 241:
#line 2410 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), idEqq, (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7879 "parse.c" /* yacc.c:1646  */
    break;

  case 242:
#line 2414 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), idNeq, (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7887 "parse.c" /* yacc.c:1646  */
    break;

  case 243:
#line 2418 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = match_op(p, (yyvsp[-2].node), (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7895 "parse.c" /* yacc.c:1646  */
    break;

  case 244:
#line 2422 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), idNeqTilde, (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7903 "parse.c" /* yacc.c:1646  */
    break;

  case 245:
#line 2426 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = call_uni_op(p, method_cond(p, (yyvsp[0].node), &(yylsp[0])), '!', &(yylsp[-1]), &(yyloc));
		    }
#line 7911 "parse.c" /* yacc.c:1646  */
    break;

  case 246:
#line 2430 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = call_uni_op(p, (yyvsp[0].node), '~', &(yylsp[-1]), &(yyloc));
		    }
#line 7919 "parse.c" /* yacc.c:1646  */
    break;

  case 247:
#line 2434 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), idLTLT, (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7927 "parse.c" /* yacc.c:1646  */
    break;

  case 248:
#line 2438 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), idGTGT, (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7935 "parse.c" /* yacc.c:1646  */
    break;

  case 249:
#line 2442 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = logop(p, idANDOP, (yyvsp[-2].node), (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7943 "parse.c" /* yacc.c:1646  */
    break;

  case 250:
#line 2446 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = logop(p, idOROP, (yyvsp[-2].node), (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7951 "parse.c" /* yacc.c:1646  */
    break;

  case 251:
#line 2449 "parse.y" /* yacc.c:1646  */
    {p->ctxt.in_defined = 1;}
#line 7957 "parse.c" /* yacc.c:1646  */
    break;

  case 252:
#line 2450 "parse.y" /* yacc.c:1646  */
    {
			p->ctxt.in_defined = 0;
			(yyval.node) = new_defined(p, (yyvsp[0].node), &(yyloc));
		    }
#line 7966 "parse.c" /* yacc.c:1646  */
    break;

  case 253:
#line 2455 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			value_expr((yyvsp[-5].node));
			(yyval.node) = new_if(p, (yyvsp[-5].node), (yyvsp[-3].node), (yyvsp[0].node), &(yyloc));
			fixpos((yyval.node), (yyvsp[-5].node));
		    /*% %*/
		    /*% ripper: ifop!($1, $3, $6) %*/
		    }
#line 7979 "parse.c" /* yacc.c:1646  */
    break;

  case 254:
#line 2464 "parse.y" /* yacc.c:1646  */
    {
			endless_method_name(p, (yyvsp[-3].node), &(yylsp[-3]));
			token_info_drop(p, "def", (yylsp[-3]).beg_pos);
			restore_defun(p, (yyvsp[-3].node)->nd_defn);
		    /*%%%*/
			(yyval.node) = set_defun_body(p, (yyvsp[-3].node), (yyvsp[-2].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: def!(get_value($1), $2, $4) %*/
			local_pop(p);
		    }
#line 7994 "parse.c" /* yacc.c:1646  */
    break;

  case 255:
#line 2475 "parse.y" /* yacc.c:1646  */
    {
			endless_method_name(p, (yyvsp[-5].node), &(yylsp[-5]));
			token_info_drop(p, "def", (yylsp[-5]).beg_pos);
			restore_defun(p, (yyvsp[-5].node)->nd_defn);
		    /*%%%*/
			(yyvsp[-2].node) = rescued_expr(p, (yyvsp[-2].node), (yyvsp[0].node), &(yylsp[-2]), &(yylsp[-1]), &(yylsp[0]));
			(yyval.node) = set_defun_body(p, (yyvsp[-5].node), (yyvsp[-4].node), (yyvsp[-2].node), &(yyloc));
		    /*% %*/
		    /*% ripper: def!(get_value($1), $2, rescue_mod!($4, $6)) %*/
			local_pop(p);
		    }
#line 8010 "parse.c" /* yacc.c:1646  */
    break;

  case 256:
#line 2487 "parse.y" /* yacc.c:1646  */
    {
			endless_method_name(p, (yyvsp[-3].node), &(yylsp[-3]));
			restore_defun(p, (yyvsp[-3].node)->nd_defn);
		    /*%%%*/
			(yyval.node) = set_defun_body(p, (yyvsp[-3].node), (yyvsp[-2].node), (yyvsp[0].node), &(yyloc));
		    /*%
			$1 = get_value($1);
		    %*/
		    /*% ripper: defs!(AREF($1, 0), AREF($1, 1), AREF($1, 2), $2, $4) %*/
			local_pop(p);
		    }
#line 8026 "parse.c" /* yacc.c:1646  */
    break;

  case 257:
#line 2499 "parse.y" /* yacc.c:1646  */
    {
			endless_method_name(p, (yyvsp[-5].node), &(yylsp[-5]));
			restore_defun(p, (yyvsp[-5].node)->nd_defn);
		    /*%%%*/
			(yyvsp[-2].node) = rescued_expr(p, (yyvsp[-2].node), (yyvsp[0].node), &(yylsp[-2]), &(yylsp[-1]), &(yylsp[0]));
			(yyval.node) = set_defun_body(p, (yyvsp[-5].node), (yyvsp[-4].node), (yyvsp[-2].node), &(yyloc));
		    /*%
			$1 = get_value($1);
		    %*/
		    /*% ripper: defs!(AREF($1, 0), AREF($1, 1), AREF($1, 2), $2, rescue_mod!($4, $6)) %*/
			local_pop(p);
		    }
#line 8043 "parse.c" /* yacc.c:1646  */
    break;

  case 258:
#line 2512 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = (yyvsp[0].node);
		    }
#line 8051 "parse.c" /* yacc.c:1646  */
    break;

  case 259:
#line 2517 "parse.y" /* yacc.c:1646  */
    {(yyval.id) = '>';}
#line 8057 "parse.c" /* yacc.c:1646  */
    break;

  case 260:
#line 2518 "parse.y" /* yacc.c:1646  */
    {(yyval.id) = '<';}
#line 8063 "parse.c" /* yacc.c:1646  */
    break;

  case 261:
#line 2519 "parse.y" /* yacc.c:1646  */
    {(yyval.id) = idGE;}
#line 8069 "parse.c" /* yacc.c:1646  */
    break;

  case 262:
#line 2520 "parse.y" /* yacc.c:1646  */
    {(yyval.id) = idLE;}
#line 8075 "parse.c" /* yacc.c:1646  */
    break;

  case 263:
#line 2524 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), (yyvsp[-1].id), (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 8083 "parse.c" /* yacc.c:1646  */
    break;

  case 264:
#line 2528 "parse.y" /* yacc.c:1646  */
    {
			rb_warning1("comparison '%s' after comparison", WARN_ID((yyvsp[-1].id)));
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), (yyvsp[-1].id), (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 8092 "parse.c" /* yacc.c:1646  */
    break;

  case 265:
#line 2535 "parse.y" /* yacc.c:1646  */
    {
			value_expr((yyvsp[0].node));
			(yyval.node) = (yyvsp[0].node);
		    }
#line 8101 "parse.c" /* yacc.c:1646  */
    break;

  case 267:
#line 2543 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = (yyvsp[-1].node);
		    }
#line 8109 "parse.c" /* yacc.c:1646  */
    break;

  case 268:
#line 2547 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node) ? arg_append(p, (yyvsp[-3].node), new_hash(p, (yyvsp[-1].node), &(yylsp[-1])), &(yyloc)) : (yyvsp[-3].node);
		    /*% %*/
		    /*% ripper: args_add!($1, bare_assoc_hash!($3)) %*/
		    }
#line 8120 "parse.c" /* yacc.c:1646  */
    break;

  case 269:
#line 2554 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node) ? NEW_LIST(new_hash(p, (yyvsp[-1].node), &(yylsp[-1])), &(yyloc)) : 0;
		    /*% %*/
		    /*% ripper: args_add!(args_new!, bare_assoc_hash!($1)) %*/
		    }
#line 8131 "parse.c" /* yacc.c:1646  */
    break;

  case 270:
#line 2563 "parse.y" /* yacc.c:1646  */
    {
			value_expr((yyvsp[0].node));
			(yyval.node) = (yyvsp[0].node);
		    }
#line 8140 "parse.c" /* yacc.c:1646  */
    break;

  case 271:
#line 2568 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			value_expr((yyvsp[-2].node));
			(yyval.node) = rescued_expr(p, (yyvsp[-2].node), (yyvsp[0].node), &(yylsp[-2]), &(yylsp[-1]), &(yylsp[0]));
		    /*% %*/
		    /*% ripper: rescue_mod!($1, $3) %*/
		    }
#line 8152 "parse.c" /* yacc.c:1646  */
    break;

  case 272:
#line 2578 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node);
		    /*% %*/
		    /*% ripper: arg_paren!(escape_Qundef($2)) %*/
		    }
#line 8163 "parse.c" /* yacc.c:1646  */
    break;

  case 273:
#line 2585 "parse.y" /* yacc.c:1646  */
    {
			if (!check_forwarding_args(p)) {
			    (yyval.node) = Qnone;
			}
			else {
			/*%%%*/
			    (yyval.node) = new_args_forward_call(p, (yyvsp[-3].node), &(yylsp[-1]), &(yyloc));
			/*% %*/
			/*% ripper: arg_paren!(args_add!($2, $4)) %*/
			}
		    }
#line 8179 "parse.c" /* yacc.c:1646  */
    break;

  case 274:
#line 2597 "parse.y" /* yacc.c:1646  */
    {
			if (!check_forwarding_args(p)) {
			    (yyval.node) = Qnone;
			}
			else {
			/*%%%*/
			    (yyval.node) = new_args_forward_call(p, 0, &(yylsp[-1]), &(yyloc));
			/*% %*/
			/*% ripper: arg_paren!($2) %*/
			}
		    }
#line 8195 "parse.c" /* yacc.c:1646  */
    break;

  case 279:
#line 2617 "parse.y" /* yacc.c:1646  */
    {
		      (yyval.node) = (yyvsp[-1].node);
		    }
#line 8203 "parse.c" /* yacc.c:1646  */
    break;

  case 280:
#line 2621 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node) ? arg_append(p, (yyvsp[-3].node), new_hash(p, (yyvsp[-1].node), &(yylsp[-1])), &(yyloc)) : (yyvsp[-3].node);
		    /*% %*/
		    /*% ripper: args_add!($1, bare_assoc_hash!($3)) %*/
		    }
#line 8214 "parse.c" /* yacc.c:1646  */
    break;

  case 281:
#line 2628 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node) ? NEW_LIST(new_hash(p, (yyvsp[-1].node), &(yylsp[-1])), &(yylsp[-1])) : 0;
		    /*% %*/
		    /*% ripper: args_add!(args_new!, bare_assoc_hash!($1)) %*/
		    }
#line 8225 "parse.c" /* yacc.c:1646  */
    break;

  case 282:
#line 2637 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			value_expr((yyvsp[0].node));
			(yyval.node) = NEW_LIST((yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: args_add!(args_new!, $1) %*/
		    }
#line 8237 "parse.c" /* yacc.c:1646  */
    break;

  case 283:
#line 2645 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = arg_blk_pass((yyvsp[-1].node), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: args_add_block!($1, $2) %*/
		    }
#line 8248 "parse.c" /* yacc.c:1646  */
    break;

  case 284:
#line 2652 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node) ? NEW_LIST(new_hash(p, (yyvsp[-1].node), &(yylsp[-1])), &(yylsp[-1])) : 0;
			(yyval.node) = arg_blk_pass((yyval.node), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: args_add_block!(args_add!(args_new!, bare_assoc_hash!($1)), $2) %*/
		    }
#line 8260 "parse.c" /* yacc.c:1646  */
    break;

  case 285:
#line 2660 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node) ? arg_append(p, (yyvsp[-3].node), new_hash(p, (yyvsp[-1].node), &(yylsp[-1])), &(yyloc)) : (yyvsp[-3].node);
			(yyval.node) = arg_blk_pass((yyval.node), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: args_add_block!(args_add!($1, bare_assoc_hash!($3)), $4) %*/
		    }
#line 8272 "parse.c" /* yacc.c:1646  */
    break;

  case 287:
#line 2671 "parse.y" /* yacc.c:1646  */
    {
			/* If call_args starts with a open paren '(' or '[',
			 * look-ahead reading of the letters calls CMDARG_PUSH(0),
			 * but the push must be done after CMDARG_PUSH(1).
			 * So this code makes them consistent by first cancelling
			 * the premature CMDARG_PUSH(0), doing CMDARG_PUSH(1),
			 * and finally redoing CMDARG_PUSH(0).
			 */
			int lookahead = 0;
			switch (yychar) {
			  case '(': case tLPAREN: case tLPAREN_ARG: case '[': case tLBRACK:
			    lookahead = 1;
			}
			if (lookahead) CMDARG_POP();
			CMDARG_PUSH(1);
			if (lookahead) CMDARG_PUSH(0);
		    }
#line 8294 "parse.c" /* yacc.c:1646  */
    break;

  case 288:
#line 2689 "parse.y" /* yacc.c:1646  */
    {
			/* call_args can be followed by tLBRACE_ARG (that does CMDARG_PUSH(0) in the lexer)
			 * but the push must be done after CMDARG_POP() in the parser.
			 * So this code does CMDARG_POP() to pop 0 pushed by tLBRACE_ARG,
			 * CMDARG_POP() to pop 1 pushed by command_args,
			 * and CMDARG_PUSH(0) to restore back the flag set by tLBRACE_ARG.
			 */
			int lookahead = 0;
			switch (yychar) {
			  case tLBRACE_ARG:
			    lookahead = 1;
			}
			if (lookahead) CMDARG_POP();
			CMDARG_POP();
			if (lookahead) CMDARG_PUSH(0);
			(yyval.node) = (yyvsp[0].node);
		    }
#line 8316 "parse.c" /* yacc.c:1646  */
    break;

  case 289:
#line 2709 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_BLOCK_PASS((yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: $2 %*/
		    }
#line 8327 "parse.c" /* yacc.c:1646  */
    break;

  case 290:
#line 2718 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = (yyvsp[0].node);
		    }
#line 8335 "parse.c" /* yacc.c:1646  */
    break;

  case 291:
#line 2722 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = 0;
		    }
#line 8343 "parse.c" /* yacc.c:1646  */
    break;

  case 292:
#line 2728 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_LIST((yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: args_add!(args_new!, $1) %*/
		    }
#line 8354 "parse.c" /* yacc.c:1646  */
    break;

  case 293:
#line 2735 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_SPLAT((yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: args_add_star!(args_new!, $2) %*/
		    }
#line 8365 "parse.c" /* yacc.c:1646  */
    break;

  case 294:
#line 2742 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = last_arg_append(p, (yyvsp[-2].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: args_add!($1, $3) %*/
		    }
#line 8376 "parse.c" /* yacc.c:1646  */
    break;

  case 295:
#line 2749 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = rest_arg_append(p, (yyvsp[-3].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: args_add_star!($1, $4) %*/
		    }
#line 8387 "parse.c" /* yacc.c:1646  */
    break;

  case 298:
#line 2762 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = last_arg_append(p, (yyvsp[-2].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: mrhs_add!(mrhs_new_from_args!($1), $3) %*/
		    }
#line 8398 "parse.c" /* yacc.c:1646  */
    break;

  case 299:
#line 2769 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = rest_arg_append(p, (yyvsp[-3].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: mrhs_add_star!(mrhs_new_from_args!($1), $4) %*/
		    }
#line 8409 "parse.c" /* yacc.c:1646  */
    break;

  case 300:
#line 2776 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_SPLAT((yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: mrhs_add_star!(mrhs_new!, $2) %*/
		    }
#line 8420 "parse.c" /* yacc.c:1646  */
    break;

  case 311:
#line 2795 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_FCALL((yyvsp[0].id), 0, &(yyloc));
		    /*% %*/
		    /*% ripper: method_add_arg!(fcall!($1), args_new!) %*/
		    }
#line 8431 "parse.c" /* yacc.c:1646  */
    break;

  case 312:
#line 2802 "parse.y" /* yacc.c:1646  */
    {
			CMDARG_PUSH(0);
		    }
#line 8439 "parse.c" /* yacc.c:1646  */
    break;

  case 313:
#line 2807 "parse.y" /* yacc.c:1646  */
    {
			CMDARG_POP();
		    /*%%%*/
			set_line_body((yyvsp[-1].node), (yylsp[-3]).end_pos.lineno);
			(yyval.node) = NEW_BEGIN((yyvsp[-1].node), &(yyloc));
			nd_set_line((yyval.node), (yylsp[-3]).end_pos.lineno);
		    /*% %*/
		    /*% ripper: begin!($3) %*/
		    }
#line 8453 "parse.c" /* yacc.c:1646  */
    break;

  case 314:
#line 2816 "parse.y" /* yacc.c:1646  */
    {SET_LEX_STATE(EXPR_ENDARG);}
#line 8459 "parse.c" /* yacc.c:1646  */
    break;

  case 315:
#line 2817 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_BEGIN(0, &(yyloc));
		    /*% %*/
		    /*% ripper: paren!(0) %*/
		    }
#line 8470 "parse.c" /* yacc.c:1646  */
    break;

  case 316:
#line 2823 "parse.y" /* yacc.c:1646  */
    {SET_LEX_STATE(EXPR_ENDARG);}
#line 8476 "parse.c" /* yacc.c:1646  */
    break;

  case 317:
#line 2824 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			if (nd_type((yyvsp[-2].node)) == NODE_SELF) (yyvsp[-2].node)->nd_state = 0;
			(yyval.node) = (yyvsp[-2].node);
		    /*% %*/
		    /*% ripper: paren!($2) %*/
		    }
#line 8488 "parse.c" /* yacc.c:1646  */
    break;

  case 318:
#line 2832 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			if (nd_type((yyvsp[-1].node)) == NODE_SELF) (yyvsp[-1].node)->nd_state = 0;
			(yyval.node) = (yyvsp[-1].node);
		    /*% %*/
		    /*% ripper: paren!($2) %*/
		    }
#line 8500 "parse.c" /* yacc.c:1646  */
    break;

  case 319:
#line 2840 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_COLON2((yyvsp[-2].node), (yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: const_path_ref!($1, $3) %*/
		    }
#line 8511 "parse.c" /* yacc.c:1646  */
    break;

  case 320:
#line 2847 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_COLON3((yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: top_const_ref!($2) %*/
		    }
#line 8522 "parse.c" /* yacc.c:1646  */
    break;

  case 321:
#line 2854 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = make_list((yyvsp[-1].node), &(yyloc));
		    /*% %*/
		    /*% ripper: array!(escape_Qundef($2)) %*/
		    }
#line 8533 "parse.c" /* yacc.c:1646  */
    break;

  case 322:
#line 2861 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_hash(p, (yyvsp[-1].node), &(yyloc));
			(yyval.node)->nd_brace = TRUE;
		    /*% %*/
		    /*% ripper: hash!(escape_Qundef($2)) %*/
		    }
#line 8545 "parse.c" /* yacc.c:1646  */
    break;

  case 323:
#line 2869 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_RETURN(0, &(yyloc));
		    /*% %*/
		    /*% ripper: return0! %*/
		    }
#line 8556 "parse.c" /* yacc.c:1646  */
    break;

  case 324:
#line 2876 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_yield(p, (yyvsp[-1].node), &(yyloc));
		    /*% %*/
		    /*% ripper: yield!(paren!($3)) %*/
		    }
#line 8567 "parse.c" /* yacc.c:1646  */
    break;

  case 325:
#line 2883 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_YIELD(0, &(yyloc));
		    /*% %*/
		    /*% ripper: yield!(paren!(args_new!)) %*/
		    }
#line 8578 "parse.c" /* yacc.c:1646  */
    break;

  case 326:
#line 2890 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_YIELD(0, &(yyloc));
		    /*% %*/
		    /*% ripper: yield0! %*/
		    }
#line 8589 "parse.c" /* yacc.c:1646  */
    break;

  case 327:
#line 2896 "parse.y" /* yacc.c:1646  */
    {p->ctxt.in_defined = 1;}
#line 8595 "parse.c" /* yacc.c:1646  */
    break;

  case 328:
#line 2897 "parse.y" /* yacc.c:1646  */
    {
			p->ctxt.in_defined = 0;
			(yyval.node) = new_defined(p, (yyvsp[-1].node), &(yyloc));
		    }
#line 8604 "parse.c" /* yacc.c:1646  */
    break;

  case 329:
#line 2902 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = call_uni_op(p, method_cond(p, (yyvsp[-1].node), &(yylsp[-1])), METHOD_NOT, &(yylsp[-3]), &(yyloc));
		    }
#line 8612 "parse.c" /* yacc.c:1646  */
    break;

  case 330:
#line 2906 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = call_uni_op(p, method_cond(p, new_nil(&(yylsp[-1])), &(yylsp[-1])), METHOD_NOT, &(yylsp[-2]), &(yyloc));
		    }
#line 8620 "parse.c" /* yacc.c:1646  */
    break;

  case 331:
#line 2910 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = method_add_block(p, (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: method_add_block!(method_add_arg!(fcall!($1), args_new!), $2) %*/
		    }
#line 8631 "parse.c" /* yacc.c:1646  */
    break;

  case 333:
#line 2918 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			block_dup_check(p, (yyvsp[-1].node)->nd_args, (yyvsp[0].node));
			(yyval.node) = method_add_block(p, (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: method_add_block!($1, $2) %*/
		    }
#line 8643 "parse.c" /* yacc.c:1646  */
    break;

  case 335:
#line 2930 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_if(p, (yyvsp[-4].node), (yyvsp[-2].node), (yyvsp[-1].node), &(yyloc));
			fixpos((yyval.node), (yyvsp[-4].node));
		    /*% %*/
		    /*% ripper: if!($2, $4, escape_Qundef($5)) %*/
		    }
#line 8655 "parse.c" /* yacc.c:1646  */
    break;

  case 336:
#line 2941 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_unless(p, (yyvsp[-4].node), (yyvsp[-2].node), (yyvsp[-1].node), &(yyloc));
			fixpos((yyval.node), (yyvsp[-4].node));
		    /*% %*/
		    /*% ripper: unless!($2, $4, escape_Qundef($5)) %*/
		    }
#line 8667 "parse.c" /* yacc.c:1646  */
    break;

  case 337:
#line 2951 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_WHILE(cond(p, (yyvsp[-2].node), &(yylsp[-2])), (yyvsp[-1].node), 1, &(yyloc));
			fixpos((yyval.node), (yyvsp[-2].node));
		    /*% %*/
		    /*% ripper: while!($2, $3) %*/
		    }
#line 8679 "parse.c" /* yacc.c:1646  */
    break;

  case 338:
#line 2961 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_UNTIL(cond(p, (yyvsp[-2].node), &(yylsp[-2])), (yyvsp[-1].node), 1, &(yyloc));
			fixpos((yyval.node), (yyvsp[-2].node));
		    /*% %*/
		    /*% ripper: until!($2, $3) %*/
		    }
#line 8691 "parse.c" /* yacc.c:1646  */
    break;

  case 339:
#line 2969 "parse.y" /* yacc.c:1646  */
    {
			(yyval.val) = p->case_labels;
			p->case_labels = Qnil;
		    }
#line 8700 "parse.c" /* yacc.c:1646  */
    break;

  case 340:
#line 2975 "parse.y" /* yacc.c:1646  */
    {
			if (RTEST(p->case_labels)) rb_hash_clear(p->case_labels);
			p->case_labels = (yyvsp[-2].val);
		    /*%%%*/
			(yyval.node) = NEW_CASE((yyvsp[-4].node), (yyvsp[-1].node), &(yyloc));
			fixpos((yyval.node), (yyvsp[-4].node));
		    /*% %*/
		    /*% ripper: case!($2, $5) %*/
		    }
#line 8714 "parse.c" /* yacc.c:1646  */
    break;

  case 341:
#line 2985 "parse.y" /* yacc.c:1646  */
    {
			(yyval.val) = p->case_labels;
			p->case_labels = 0;
		    }
#line 8723 "parse.c" /* yacc.c:1646  */
    break;

  case 342:
#line 2991 "parse.y" /* yacc.c:1646  */
    {
			if (RTEST(p->case_labels)) rb_hash_clear(p->case_labels);
			p->case_labels = (yyvsp[-2].val);
		    /*%%%*/
			(yyval.node) = NEW_CASE2((yyvsp[-1].node), &(yyloc));
		    /*% %*/
		    /*% ripper: case!(Qnil, $4) %*/
		    }
#line 8736 "parse.c" /* yacc.c:1646  */
    break;

  case 343:
#line 3002 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_CASE3((yyvsp[-3].node), (yyvsp[-1].node), &(yyloc));
		    /*% %*/
		    /*% ripper: case!($2, $4) %*/
		    }
#line 8747 "parse.c" /* yacc.c:1646  */
    break;

  case 344:
#line 3011 "parse.y" /* yacc.c:1646  */
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
			ID id = internal_id(p);
			NODE *m = NEW_ARGS_AUX(0, 0, &NULL_LOC);
			NODE *args, *scope, *internal_var = NEW_DVAR(id, &(yylsp[-4]));
			ID *tbl = ALLOC_N(ID, 3);
			tbl[0] = 1 /* length of local var table */; tbl[1] = id /* internal id */;
                        rb_ast_add_local_table(p->ast, tbl);

			switch (nd_type((yyvsp[-4].node))) {
			  case NODE_LASGN:
			  case NODE_DASGN:
			  case NODE_DASGN_CURR: /* e.each {|internal_var| a = internal_var; ... } */
			    (yyvsp[-4].node)->nd_value = internal_var;
			    id = 0;
			    m->nd_plen = 1;
			    m->nd_next = (yyvsp[-4].node);
			    break;
			  case NODE_MASGN: /* e.each {|*internal_var| a, b, c = (internal_var.length == 1 && Array === (tmp = internal_var[0]) ? tmp : internal_var); ... } */
			    m->nd_next = node_assign(p, (yyvsp[-4].node), NEW_FOR_MASGN(internal_var, &(yylsp[-4])), &(yylsp[-4]));
			    break;
			  default: /* e.each {|*internal_var| @a, B, c[1], d.attr = internal_val; ... } */
			    m->nd_next = node_assign(p, NEW_MASGN(NEW_LIST((yyvsp[-4].node), &(yylsp[-4])), 0, &(yylsp[-4])), internal_var, &(yylsp[-4]));
			}
			/* {|*internal_id| <m> = internal_id; ... } */
			args = new_args(p, m, 0, id, 0, new_args_tail(p, 0, 0, 0, &(yylsp[-4])), &(yylsp[-4]));
			scope = NEW_NODE(NODE_SCOPE, tbl, (yyvsp[-1].node), args, &(yyloc));
			(yyval.node) = NEW_FOR((yyvsp[-2].node), scope, &(yyloc));
			fixpos((yyval.node), (yyvsp[-4].node));
		    /*% %*/
		    /*% ripper: for!($2, $4, $5) %*/
		    }
#line 8793 "parse.c" /* yacc.c:1646  */
    break;

  case 345:
#line 3053 "parse.y" /* yacc.c:1646  */
    {
			if (p->ctxt.in_def) {
			    YYLTYPE loc = code_loc_gen(&(yylsp[-2]), &(yylsp[-1]));
			    yyerror1(&loc, "class definition in method body");
			}
			p->ctxt.in_class = 1;
			local_push(p, 0);
		    }
#line 8806 "parse.c" /* yacc.c:1646  */
    break;

  case 346:
#line 3063 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_CLASS((yyvsp[-4].node), (yyvsp[-1].node), (yyvsp[-3].node), &(yyloc));
			nd_set_line((yyval.node)->nd_body, (yylsp[0]).end_pos.lineno);
			set_line_body((yyvsp[-1].node), (yylsp[-3]).end_pos.lineno);
			nd_set_line((yyval.node), (yylsp[-3]).end_pos.lineno);
		    /*% %*/
		    /*% ripper: class!($2, $3, $5) %*/
			local_pop(p);
			p->ctxt.in_class = (yyvsp[-5].ctxt).in_class;
		    }
#line 8822 "parse.c" /* yacc.c:1646  */
    break;

  case 347:
#line 3075 "parse.y" /* yacc.c:1646  */
    {
			p->ctxt.in_def = 0;
			p->ctxt.in_class = 0;
			local_push(p, 0);
		    }
#line 8832 "parse.c" /* yacc.c:1646  */
    break;

  case 348:
#line 3083 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_SCLASS((yyvsp[-4].node), (yyvsp[-1].node), &(yyloc));
			nd_set_line((yyval.node)->nd_body, (yylsp[0]).end_pos.lineno);
			set_line_body((yyvsp[-1].node), nd_line((yyvsp[-4].node)));
			fixpos((yyval.node), (yyvsp[-4].node));
		    /*% %*/
		    /*% ripper: sclass!($3, $6) %*/
			local_pop(p);
			p->ctxt.in_def = (yyvsp[-6].ctxt).in_def;
			p->ctxt.in_class = (yyvsp[-6].ctxt).in_class;
		    }
#line 8849 "parse.c" /* yacc.c:1646  */
    break;

  case 349:
#line 3096 "parse.y" /* yacc.c:1646  */
    {
			if (p->ctxt.in_def) {
			    YYLTYPE loc = code_loc_gen(&(yylsp[-1]), &(yylsp[0]));
			    yyerror1(&loc, "module definition in method body");
			}
			p->ctxt.in_class = 1;
			local_push(p, 0);
		    }
#line 8862 "parse.c" /* yacc.c:1646  */
    break;

  case 350:
#line 3106 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MODULE((yyvsp[-3].node), (yyvsp[-1].node), &(yyloc));
			nd_set_line((yyval.node)->nd_body, (yylsp[0]).end_pos.lineno);
			set_line_body((yyvsp[-1].node), (yylsp[-3]).end_pos.lineno);
			nd_set_line((yyval.node), (yylsp[-3]).end_pos.lineno);
		    /*% %*/
		    /*% ripper: module!($2, $4) %*/
			local_pop(p);
			p->ctxt.in_class = (yyvsp[-4].ctxt).in_class;
		    }
#line 8878 "parse.c" /* yacc.c:1646  */
    break;

  case 351:
#line 3121 "parse.y" /* yacc.c:1646  */
    {
			restore_defun(p, (yyvsp[-3].node)->nd_defn);
		    /*%%%*/
			(yyval.node) = set_defun_body(p, (yyvsp[-3].node), (yyvsp[-2].node), (yyvsp[-1].node), &(yyloc));
		    /*% %*/
		    /*% ripper: def!(get_value($1), $2, $3) %*/
			local_pop(p);
		    }
#line 8891 "parse.c" /* yacc.c:1646  */
    break;

  case 352:
#line 3133 "parse.y" /* yacc.c:1646  */
    {
			restore_defun(p, (yyvsp[-3].node)->nd_defn);
		    /*%%%*/
			(yyval.node) = set_defun_body(p, (yyvsp[-3].node), (yyvsp[-2].node), (yyvsp[-1].node), &(yyloc));
		    /*%
			$1 = get_value($1);
		    %*/
		    /*% ripper: defs!(AREF($1, 0), AREF($1, 1), AREF($1, 2), $2, $3) %*/
			local_pop(p);
		    }
#line 8906 "parse.c" /* yacc.c:1646  */
    break;

  case 353:
#line 3144 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_BREAK(0, &(yyloc));
		    /*% %*/
		    /*% ripper: break!(args_new!) %*/
		    }
#line 8917 "parse.c" /* yacc.c:1646  */
    break;

  case 354:
#line 3151 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_NEXT(0, &(yyloc));
		    /*% %*/
		    /*% ripper: next!(args_new!) %*/
		    }
#line 8928 "parse.c" /* yacc.c:1646  */
    break;

  case 355:
#line 3158 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_REDO(&(yyloc));
		    /*% %*/
		    /*% ripper: redo! %*/
		    }
#line 8939 "parse.c" /* yacc.c:1646  */
    break;

  case 356:
#line 3165 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_RETRY(&(yyloc));
		    /*% %*/
		    /*% ripper: retry! %*/
		    }
#line 8950 "parse.c" /* yacc.c:1646  */
    break;

  case 357:
#line 3174 "parse.y" /* yacc.c:1646  */
    {
			value_expr((yyvsp[0].node));
			(yyval.node) = (yyvsp[0].node);
		    }
#line 8959 "parse.c" /* yacc.c:1646  */
    break;

  case 358:
#line 3181 "parse.y" /* yacc.c:1646  */
    {
			token_info_push(p, "begin", &(yyloc));
		    }
#line 8967 "parse.c" /* yacc.c:1646  */
    break;

  case 359:
#line 3187 "parse.y" /* yacc.c:1646  */
    {
			WARN_EOL("if");
			token_info_push(p, "if", &(yyloc));
			if (p->token_info && p->token_info->nonspc &&
			    p->token_info->next && !strcmp(p->token_info->next->token, "else")) {
			    const char *tok = p->lex.ptok;
			    const char *beg = p->lex.pbeg + p->token_info->next->beg.column;
			    beg += rb_strlen_lit("else");
			    while (beg < tok && ISSPACE(*beg)) beg++;
			    if (beg == tok) {
				p->token_info->nonspc = 0;
			    }
			}
		    }
#line 8986 "parse.c" /* yacc.c:1646  */
    break;

  case 360:
#line 3204 "parse.y" /* yacc.c:1646  */
    {
			token_info_push(p, "unless", &(yyloc));
		    }
#line 8994 "parse.c" /* yacc.c:1646  */
    break;

  case 361:
#line 3210 "parse.y" /* yacc.c:1646  */
    {
			token_info_push(p, "while", &(yyloc));
		    }
#line 9002 "parse.c" /* yacc.c:1646  */
    break;

  case 362:
#line 3216 "parse.y" /* yacc.c:1646  */
    {
			token_info_push(p, "until", &(yyloc));
		    }
#line 9010 "parse.c" /* yacc.c:1646  */
    break;

  case 363:
#line 3222 "parse.y" /* yacc.c:1646  */
    {
			token_info_push(p, "case", &(yyloc));
		    }
#line 9018 "parse.c" /* yacc.c:1646  */
    break;

  case 364:
#line 3228 "parse.y" /* yacc.c:1646  */
    {
			token_info_push(p, "for", &(yyloc));
		    }
#line 9026 "parse.c" /* yacc.c:1646  */
    break;

  case 365:
#line 3234 "parse.y" /* yacc.c:1646  */
    {
			token_info_push(p, "class", &(yyloc));
			(yyval.ctxt) = p->ctxt;
		    }
#line 9035 "parse.c" /* yacc.c:1646  */
    break;

  case 366:
#line 3241 "parse.y" /* yacc.c:1646  */
    {
			token_info_push(p, "module", &(yyloc));
			(yyval.ctxt) = p->ctxt;
		    }
#line 9044 "parse.c" /* yacc.c:1646  */
    break;

  case 367:
#line 3248 "parse.y" /* yacc.c:1646  */
    {
			token_info_push(p, "def", &(yyloc));
		    }
#line 9052 "parse.c" /* yacc.c:1646  */
    break;

  case 368:
#line 3254 "parse.y" /* yacc.c:1646  */
    {
			token_info_push(p, "do", &(yyloc));
		    }
#line 9060 "parse.c" /* yacc.c:1646  */
    break;

  case 369:
#line 3260 "parse.y" /* yacc.c:1646  */
    {
			token_info_push(p, "do", &(yyloc));
		    }
#line 9068 "parse.c" /* yacc.c:1646  */
    break;

  case 370:
#line 3266 "parse.y" /* yacc.c:1646  */
    {
			token_info_warn(p, "rescue", p->token_info, 1, &(yyloc));
		    }
#line 9076 "parse.c" /* yacc.c:1646  */
    break;

  case 371:
#line 3272 "parse.y" /* yacc.c:1646  */
    {
			token_info_warn(p, "ensure", p->token_info, 1, &(yyloc));
		    }
#line 9084 "parse.c" /* yacc.c:1646  */
    break;

  case 372:
#line 3278 "parse.y" /* yacc.c:1646  */
    {
			token_info_warn(p, "when", p->token_info, 0, &(yyloc));
		    }
#line 9092 "parse.c" /* yacc.c:1646  */
    break;

  case 373:
#line 3284 "parse.y" /* yacc.c:1646  */
    {
			token_info *ptinfo_beg = p->token_info;
			int same = ptinfo_beg && strcmp(ptinfo_beg->token, "case") != 0;
			token_info_warn(p, "else", p->token_info, same, &(yyloc));
			if (same) {
			    token_info e;
			    e.next = ptinfo_beg->next;
			    e.token = "else";
			    token_info_setup(&e, p->lex.pbeg, &(yyloc));
			    if (!e.nonspc) *ptinfo_beg = e;
			}
		    }
#line 9109 "parse.c" /* yacc.c:1646  */
    break;

  case 374:
#line 3299 "parse.y" /* yacc.c:1646  */
    {
			WARN_EOL("elsif");
			token_info_warn(p, "elsif", p->token_info, 1, &(yyloc));
		    }
#line 9118 "parse.c" /* yacc.c:1646  */
    break;

  case 375:
#line 3306 "parse.y" /* yacc.c:1646  */
    {
			token_info_pop(p, "end", &(yyloc));
		    }
#line 9126 "parse.c" /* yacc.c:1646  */
    break;

  case 376:
#line 3312 "parse.y" /* yacc.c:1646  */
    {
			if (p->ctxt.in_class && !p->ctxt.in_def && !dyna_in_block(p))
			    yyerror1(&(yylsp[0]), "Invalid return in class/module body");
		    }
#line 9135 "parse.c" /* yacc.c:1646  */
    break;

  case 383:
#line 3331 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_if(p, (yyvsp[-3].node), (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
			fixpos((yyval.node), (yyvsp[-3].node));
		    /*% %*/
		    /*% ripper: elsif!($2, $4, escape_Qundef($5)) %*/
		    }
#line 9147 "parse.c" /* yacc.c:1646  */
    break;

  case 385:
#line 3342 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[0].node);
		    /*% %*/
		    /*% ripper: else!($2) %*/
		    }
#line 9158 "parse.c" /* yacc.c:1646  */
    break;

  case 388:
#line 3355 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = assignable(p, (yyvsp[0].id), 0, &(yyloc));
			mark_lvar_used(p, (yyval.node));
		    /*% %*/
		    /*% ripper: assignable(p, $1) %*/
		    }
#line 9170 "parse.c" /* yacc.c:1646  */
    break;

  case 389:
#line 3363 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node);
		    /*% %*/
		    /*% ripper: mlhs_paren!($2) %*/
		    }
#line 9181 "parse.c" /* yacc.c:1646  */
    break;

  case 390:
#line 3372 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_LIST((yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_add!(mlhs_new!, $1) %*/
		    }
#line 9192 "parse.c" /* yacc.c:1646  */
    break;

  case 391:
#line 3379 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = list_append(p, (yyvsp[-2].node), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: mlhs_add!($1, $3) %*/
		    }
#line 9203 "parse.c" /* yacc.c:1646  */
    break;

  case 392:
#line 3388 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN((yyvsp[0].node), 0, &(yyloc));
		    /*% %*/
		    /*% ripper: $1 %*/
		    }
#line 9214 "parse.c" /* yacc.c:1646  */
    break;

  case 393:
#line 3395 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN((yyvsp[-2].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_add_star!($1, $3) %*/
		    }
#line 9225 "parse.c" /* yacc.c:1646  */
    break;

  case 394:
#line 3402 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN((yyvsp[-4].node), NEW_POSTARG((yyvsp[-2].node), (yyvsp[0].node), &(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_add_post!(mlhs_add_star!($1, $3), $5) %*/
		    }
#line 9236 "parse.c" /* yacc.c:1646  */
    break;

  case 395:
#line 3409 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN(0, (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_add_star!(mlhs_new!, $1) %*/
		    }
#line 9247 "parse.c" /* yacc.c:1646  */
    break;

  case 396:
#line 3416 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN(0, NEW_POSTARG((yyvsp[-2].node), (yyvsp[0].node), &(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_add_post!(mlhs_add_star!(mlhs_new!, $1), $3) %*/
		    }
#line 9258 "parse.c" /* yacc.c:1646  */
    break;

  case 397:
#line 3425 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = assignable(p, (yyvsp[0].id), 0, &(yyloc));
			mark_lvar_used(p, (yyval.node));
		    /*% %*/
		    /*% ripper: assignable(p, $2) %*/
		    }
#line 9270 "parse.c" /* yacc.c:1646  */
    break;

  case 398:
#line 3433 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NODE_SPECIAL_NO_NAME_REST;
		    /*% %*/
		    /*% ripper: Qnil %*/
		    }
#line 9281 "parse.c" /* yacc.c:1646  */
    break;

  case 400:
#line 3442 "parse.y" /* yacc.c:1646  */
    {(yyval.id) = ID2VAL(idNil);}
#line 9287 "parse.c" /* yacc.c:1646  */
    break;

  case 401:
#line 3446 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args_tail(p, (yyvsp[-3].node), (yyvsp[-1].id), (yyvsp[0].id), &(yylsp[-1]));
		    }
#line 9295 "parse.c" /* yacc.c:1646  */
    break;

  case 402:
#line 3450 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args_tail(p, (yyvsp[-1].node), Qnone, (yyvsp[0].id), &(yylsp[-1]));
		    }
#line 9303 "parse.c" /* yacc.c:1646  */
    break;

  case 403:
#line 3454 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args_tail(p, Qnone, (yyvsp[-1].id), (yyvsp[0].id), &(yylsp[-1]));
		    }
#line 9311 "parse.c" /* yacc.c:1646  */
    break;

  case 404:
#line 3458 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args_tail(p, Qnone, Qnone, (yyvsp[0].id), &(yylsp[0]));
		    }
#line 9319 "parse.c" /* yacc.c:1646  */
    break;

  case 405:
#line 3464 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = (yyvsp[0].node);
		    }
#line 9327 "parse.c" /* yacc.c:1646  */
    break;

  case 406:
#line 3468 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args_tail(p, Qnone, Qnone, Qnone, &(yylsp[0]));
		    }
#line 9335 "parse.c" /* yacc.c:1646  */
    break;

  case 407:
#line 3474 "parse.y" /* yacc.c:1646  */
    {
			/* magic number for rest_id in iseq_set_arguments() */
		    /*%%%*/
			(yyval.id) = NODE_SPECIAL_EXCESSIVE_COMMA;
		    /*% %*/
		    /*% ripper: excessed_comma! %*/
		    }
#line 9347 "parse.c" /* yacc.c:1646  */
    break;

  case 408:
#line 3484 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(p, (yyvsp[-5].node), (yyvsp[-3].node), (yyvsp[-1].id), Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 9355 "parse.c" /* yacc.c:1646  */
    break;

  case 409:
#line 3488 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(p, (yyvsp[-7].node), (yyvsp[-5].node), (yyvsp[-3].id), (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    }
#line 9363 "parse.c" /* yacc.c:1646  */
    break;

  case 410:
#line 3492 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(p, (yyvsp[-3].node), (yyvsp[-1].node), Qnone, Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 9371 "parse.c" /* yacc.c:1646  */
    break;

  case 411:
#line 3496 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(p, (yyvsp[-5].node), (yyvsp[-3].node), Qnone, (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    }
#line 9379 "parse.c" /* yacc.c:1646  */
    break;

  case 412:
#line 3500 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(p, (yyvsp[-3].node), Qnone, (yyvsp[-1].id), Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 9387 "parse.c" /* yacc.c:1646  */
    break;

  case 413:
#line 3504 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args_tail(p, Qnone, Qnone, Qnone, &(yylsp[0]));
			(yyval.node) = new_args(p, (yyvsp[-1].node), Qnone, (yyvsp[0].id), Qnone, (yyval.node), &(yyloc));
		    }
#line 9396 "parse.c" /* yacc.c:1646  */
    break;

  case 414:
#line 3509 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(p, (yyvsp[-5].node), Qnone, (yyvsp[-3].id), (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    }
#line 9404 "parse.c" /* yacc.c:1646  */
    break;

  case 415:
#line 3513 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(p, (yyvsp[-1].node), Qnone, Qnone, Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 9412 "parse.c" /* yacc.c:1646  */
    break;

  case 416:
#line 3517 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(p, Qnone, (yyvsp[-3].node), (yyvsp[-1].id), Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 9420 "parse.c" /* yacc.c:1646  */
    break;

  case 417:
#line 3521 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(p, Qnone, (yyvsp[-5].node), (yyvsp[-3].id), (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    }
#line 9428 "parse.c" /* yacc.c:1646  */
    break;

  case 418:
#line 3525 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(p, Qnone, (yyvsp[-1].node), Qnone, Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 9436 "parse.c" /* yacc.c:1646  */
    break;

  case 419:
#line 3529 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(p, Qnone, (yyvsp[-3].node), Qnone, (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    }
#line 9444 "parse.c" /* yacc.c:1646  */
    break;

  case 420:
#line 3533 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(p, Qnone, Qnone, (yyvsp[-1].id), Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 9452 "parse.c" /* yacc.c:1646  */
    break;

  case 421:
#line 3537 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(p, Qnone, Qnone, (yyvsp[-3].id), (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    }
#line 9460 "parse.c" /* yacc.c:1646  */
    break;

  case 422:
#line 3541 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(p, Qnone, Qnone, Qnone, Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 9468 "parse.c" /* yacc.c:1646  */
    break;

  case 424:
#line 3548 "parse.y" /* yacc.c:1646  */
    {
			p->command_start = TRUE;
		    }
#line 9476 "parse.c" /* yacc.c:1646  */
    break;

  case 425:
#line 3554 "parse.y" /* yacc.c:1646  */
    {
			p->cur_arg = 0;
			p->max_numparam = ORDINAL_PARAM;
		    /*%%%*/
			(yyval.node) = 0;
		    /*% %*/
		    /*% ripper: block_var!(params!(Qnil,Qnil,Qnil,Qnil,Qnil,Qnil,Qnil), escape_Qundef($2)) %*/
		    }
#line 9489 "parse.c" /* yacc.c:1646  */
    break;

  case 426:
#line 3563 "parse.y" /* yacc.c:1646  */
    {
			p->cur_arg = 0;
			p->max_numparam = ORDINAL_PARAM;
		    /*%%%*/
			(yyval.node) = (yyvsp[-2].node);
		    /*% %*/
		    /*% ripper: block_var!(escape_Qundef($2), escape_Qundef($3)) %*/
		    }
#line 9502 "parse.c" /* yacc.c:1646  */
    break;

  case 427:
#line 3575 "parse.y" /* yacc.c:1646  */
    {
		      (yyval.node) = 0;
		    }
#line 9510 "parse.c" /* yacc.c:1646  */
    break;

  case 428:
#line 3579 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = 0;
		    /*% %*/
		    /*% ripper: $3 %*/
		    }
#line 9521 "parse.c" /* yacc.c:1646  */
    break;

  case 431:
#line 3594 "parse.y" /* yacc.c:1646  */
    {
			new_bv(p, get_id((yyvsp[0].id)));
		    /*% ripper: get_value($1) %*/
		    }
#line 9530 "parse.c" /* yacc.c:1646  */
    break;

  case 432:
#line 3599 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = 0;
		    }
#line 9538 "parse.c" /* yacc.c:1646  */
    break;

  case 433:
#line 3605 "parse.y" /* yacc.c:1646  */
    {
			token_info_push(p, "->", &(yylsp[0]));
			(yyvsp[0].vars) = dyna_push(p);
			(yyval.num) = p->lex.lpar_beg;
			p->lex.lpar_beg = p->lex.paren_nest;
		    }
#line 9549 "parse.c" /* yacc.c:1646  */
    break;

  case 434:
#line 3611 "parse.y" /* yacc.c:1646  */
    {
			(yyval.num) = p->max_numparam;
			p->max_numparam = 0;
		    }
#line 9558 "parse.c" /* yacc.c:1646  */
    break;

  case 435:
#line 3615 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = numparam_push(p);
		    }
#line 9566 "parse.c" /* yacc.c:1646  */
    break;

  case 436:
#line 3619 "parse.y" /* yacc.c:1646  */
    {
			CMDARG_PUSH(0);
		    }
#line 9574 "parse.c" /* yacc.c:1646  */
    break;

  case 437:
#line 3623 "parse.y" /* yacc.c:1646  */
    {
			int max_numparam = p->max_numparam;
			p->lex.lpar_beg = (yyvsp[-5].num);
			p->max_numparam = (yyvsp[-4].num);
			CMDARG_POP();
			(yyvsp[-2].node) = args_with_numbered(p, (yyvsp[-2].node), max_numparam);
		    /*%%%*/
                        {
                            YYLTYPE loc = code_loc_gen(&(yylsp[-2]), &(yylsp[0]));
                            (yyval.node) = NEW_LAMBDA((yyvsp[-2].node), (yyvsp[0].node), &loc);
                            nd_set_line((yyval.node)->nd_body, (yylsp[0]).end_pos.lineno);
                            nd_set_line((yyval.node), (yylsp[-2]).end_pos.lineno);
			    nd_set_first_loc((yyval.node), (yylsp[-6]).beg_pos);
                        }
		    /*% %*/
		    /*% ripper: lambda!($5, $7) %*/
			numparam_pop(p, (yyvsp[-3].node));
			dyna_pop(p, (yyvsp[-6].vars));
		    }
#line 9598 "parse.c" /* yacc.c:1646  */
    break;

  case 438:
#line 3645 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-2].node);
			p->max_numparam = ORDINAL_PARAM;
		    /*% %*/
		    /*% ripper: paren!($2) %*/
		    }
#line 9610 "parse.c" /* yacc.c:1646  */
    break;

  case 439:
#line 3653 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			if (!args_info_empty_p((yyvsp[0].node)->nd_ainfo))
			    p->max_numparam = ORDINAL_PARAM;
		    /*% %*/
			(yyval.node) = (yyvsp[0].node);
		    }
#line 9622 "parse.c" /* yacc.c:1646  */
    break;

  case 440:
#line 3663 "parse.y" /* yacc.c:1646  */
    {
			token_info_pop(p, "}", &(yylsp[0]));
			(yyval.node) = (yyvsp[-1].node);
		    }
#line 9631 "parse.c" /* yacc.c:1646  */
    break;

  case 441:
#line 3668 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = (yyvsp[-1].node);
		    }
#line 9639 "parse.c" /* yacc.c:1646  */
    break;

  case 442:
#line 3674 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = (yyvsp[-1].node);
		    /*%%%*/
			(yyval.node)->nd_body->nd_loc = code_loc_gen(&(yylsp[-2]), &(yylsp[0]));
			nd_set_line((yyval.node), (yylsp[-2]).end_pos.lineno);
		    /*% %*/
		    }
#line 9651 "parse.c" /* yacc.c:1646  */
    break;

  case 443:
#line 3684 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			if (nd_type((yyvsp[-1].node)) == NODE_YIELD) {
			    compile_error(p, "block given to yield");
			}
			else {
			    block_dup_check(p, (yyvsp[-1].node)->nd_args, (yyvsp[0].node));
			}
			(yyval.node) = method_add_block(p, (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
			fixpos((yyval.node), (yyvsp[-1].node));
		    /*% %*/
		    /*% ripper: method_add_block!($1, $2) %*/
		    }
#line 9669 "parse.c" /* yacc.c:1646  */
    break;

  case 444:
#line 3698 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_qcall(p, (yyvsp[-2].id), (yyvsp[-3].node), (yyvsp[-1].id), (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    /*% %*/
		    /*% ripper: opt_event(:method_add_arg!, call!($1, $2, $3), $4) %*/
		    }
#line 9680 "parse.c" /* yacc.c:1646  */
    break;

  case 445:
#line 3705 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_command_qcall(p, (yyvsp[-3].id), (yyvsp[-4].node), (yyvsp[-2].id), (yyvsp[-1].node), (yyvsp[0].node), &(yylsp[-2]), &(yyloc));
		    /*% %*/
		    /*% ripper: opt_event(:method_add_block!, command_call!($1, $2, $3, $4), $5) %*/
		    }
#line 9691 "parse.c" /* yacc.c:1646  */
    break;

  case 446:
#line 3712 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_command_qcall(p, (yyvsp[-3].id), (yyvsp[-4].node), (yyvsp[-2].id), (yyvsp[-1].node), (yyvsp[0].node), &(yylsp[-2]), &(yyloc));
		    /*% %*/
		    /*% ripper: method_add_block!(command_call!($1, $2, $3, $4), $5) %*/
		    }
#line 9702 "parse.c" /* yacc.c:1646  */
    break;

  case 447:
#line 3721 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node);
			(yyval.node)->nd_args = (yyvsp[0].node);
			nd_set_last_loc((yyvsp[-1].node), (yylsp[0]).end_pos);
		    /*% %*/
		    /*% ripper: method_add_arg!(fcall!($1), $2) %*/
		    }
#line 9715 "parse.c" /* yacc.c:1646  */
    break;

  case 448:
#line 3730 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_qcall(p, (yyvsp[-2].id), (yyvsp[-3].node), (yyvsp[-1].id), (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
			nd_set_line((yyval.node), (yylsp[-1]).end_pos.lineno);
		    /*% %*/
		    /*% ripper: opt_event(:method_add_arg!, call!($1, $2, $3), $4) %*/
		    }
#line 9727 "parse.c" /* yacc.c:1646  */
    break;

  case 449:
#line 3738 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_qcall(p, ID2VAL(idCOLON2), (yyvsp[-3].node), (yyvsp[-1].id), (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
			nd_set_line((yyval.node), (yylsp[-1]).end_pos.lineno);
		    /*% %*/
		    /*% ripper: method_add_arg!(call!($1, ID2VAL(idCOLON2), $3), $4) %*/
		    }
#line 9739 "parse.c" /* yacc.c:1646  */
    break;

  case 450:
#line 3746 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_qcall(p, ID2VAL(idCOLON2), (yyvsp[-2].node), (yyvsp[0].id), Qnull, &(yylsp[0]), &(yyloc));
		    /*% %*/
		    /*% ripper: call!($1, ID2VAL(idCOLON2), $3) %*/
		    }
#line 9750 "parse.c" /* yacc.c:1646  */
    break;

  case 451:
#line 3753 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_qcall(p, (yyvsp[-1].id), (yyvsp[-2].node), ID2VAL(idCall), (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
			nd_set_line((yyval.node), (yylsp[-1]).end_pos.lineno);
		    /*% %*/
		    /*% ripper: method_add_arg!(call!($1, $2, ID2VAL(idCall)), $3) %*/
		    }
#line 9762 "parse.c" /* yacc.c:1646  */
    break;

  case 452:
#line 3761 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_qcall(p, ID2VAL(idCOLON2), (yyvsp[-2].node), ID2VAL(idCall), (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
			nd_set_line((yyval.node), (yylsp[-1]).end_pos.lineno);
		    /*% %*/
		    /*% ripper: method_add_arg!(call!($1, ID2VAL(idCOLON2), ID2VAL(idCall)), $3) %*/
		    }
#line 9774 "parse.c" /* yacc.c:1646  */
    break;

  case 453:
#line 3769 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_SUPER((yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: super!($2) %*/
		    }
#line 9785 "parse.c" /* yacc.c:1646  */
    break;

  case 454:
#line 3776 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_ZSUPER(&(yyloc));
		    /*% %*/
		    /*% ripper: zsuper! %*/
		    }
#line 9796 "parse.c" /* yacc.c:1646  */
    break;

  case 455:
#line 3783 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			if ((yyvsp[-3].node) && nd_type((yyvsp[-3].node)) == NODE_SELF)
			    (yyval.node) = NEW_FCALL(tAREF, (yyvsp[-1].node), &(yyloc));
			else
			    (yyval.node) = NEW_CALL((yyvsp[-3].node), tAREF, (yyvsp[-1].node), &(yyloc));
			fixpos((yyval.node), (yyvsp[-3].node));
		    /*% %*/
		    /*% ripper: aref!($1, escape_Qundef($3)) %*/
		    }
#line 9811 "parse.c" /* yacc.c:1646  */
    break;

  case 456:
#line 3796 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = (yyvsp[-1].node);
		    /*%%%*/
			(yyval.node)->nd_body->nd_loc = code_loc_gen(&(yylsp[-2]), &(yylsp[0]));
			nd_set_line((yyval.node), (yylsp[-2]).end_pos.lineno);
		    /*% %*/
		    }
#line 9823 "parse.c" /* yacc.c:1646  */
    break;

  case 457:
#line 3804 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = (yyvsp[-1].node);
		    /*%%%*/
			(yyval.node)->nd_body->nd_loc = code_loc_gen(&(yylsp[-2]), &(yylsp[0]));
			nd_set_line((yyval.node), (yylsp[-2]).end_pos.lineno);
		    /*% %*/
		    }
#line 9835 "parse.c" /* yacc.c:1646  */
    break;

  case 458:
#line 3813 "parse.y" /* yacc.c:1646  */
    {(yyval.vars) = dyna_push(p);}
#line 9841 "parse.c" /* yacc.c:1646  */
    break;

  case 459:
#line 3814 "parse.y" /* yacc.c:1646  */
    {
			(yyval.num) = p->max_numparam;
			p->max_numparam = 0;
		    }
#line 9850 "parse.c" /* yacc.c:1646  */
    break;

  case 460:
#line 3818 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = numparam_push(p);
		    }
#line 9858 "parse.c" /* yacc.c:1646  */
    break;

  case 461:
#line 3822 "parse.y" /* yacc.c:1646  */
    {
			int max_numparam = p->max_numparam;
			p->max_numparam = (yyvsp[-3].num);
			(yyvsp[-1].node) = args_with_numbered(p, (yyvsp[-1].node), max_numparam);
		    /*%%%*/
			(yyval.node) = NEW_ITER((yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: brace_block!(escape_Qundef($4), $5) %*/
			numparam_pop(p, (yyvsp[-2].node));
			dyna_pop(p, (yyvsp[-4].vars));
		    }
#line 9874 "parse.c" /* yacc.c:1646  */
    break;

  case 462:
#line 3835 "parse.y" /* yacc.c:1646  */
    {(yyval.vars) = dyna_push(p);}
#line 9880 "parse.c" /* yacc.c:1646  */
    break;

  case 463:
#line 3836 "parse.y" /* yacc.c:1646  */
    {
			(yyval.num) = p->max_numparam;
			p->max_numparam = 0;
		    }
#line 9889 "parse.c" /* yacc.c:1646  */
    break;

  case 464:
#line 3840 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = numparam_push(p);
			CMDARG_PUSH(0);
		    }
#line 9898 "parse.c" /* yacc.c:1646  */
    break;

  case 465:
#line 3845 "parse.y" /* yacc.c:1646  */
    {
			int max_numparam = p->max_numparam;
			p->max_numparam = (yyvsp[-3].num);
			(yyvsp[-1].node) = args_with_numbered(p, (yyvsp[-1].node), max_numparam);
		    /*%%%*/
			(yyval.node) = NEW_ITER((yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: do_block!(escape_Qundef($4), $5) %*/
			CMDARG_POP();
			numparam_pop(p, (yyvsp[-2].node));
			dyna_pop(p, (yyvsp[-4].vars));
		    }
#line 9915 "parse.c" /* yacc.c:1646  */
    break;

  case 466:
#line 3860 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			check_literal_when(p, (yyvsp[0].node), &(yylsp[0]));
			(yyval.node) = NEW_LIST((yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: args_add!(args_new!, $1) %*/
		    }
#line 9927 "parse.c" /* yacc.c:1646  */
    break;

  case 467:
#line 3868 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_SPLAT((yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: args_add_star!(args_new!, $2) %*/
		    }
#line 9938 "parse.c" /* yacc.c:1646  */
    break;

  case 468:
#line 3875 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			check_literal_when(p, (yyvsp[0].node), &(yylsp[0]));
			(yyval.node) = last_arg_append(p, (yyvsp[-2].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: args_add!($1, $3) %*/
		    }
#line 9950 "parse.c" /* yacc.c:1646  */
    break;

  case 469:
#line 3883 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = rest_arg_append(p, (yyvsp[-3].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: args_add_star!($1, $4) %*/
		    }
#line 9961 "parse.c" /* yacc.c:1646  */
    break;

  case 470:
#line 3894 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_WHEN((yyvsp[-3].node), (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
			fixpos((yyval.node), (yyvsp[-3].node));
		    /*% %*/
		    /*% ripper: when!($2, $4, escape_Qundef($5)) %*/
		    }
#line 9973 "parse.c" /* yacc.c:1646  */
    break;

  case 473:
#line 3908 "parse.y" /* yacc.c:1646  */
    {
			SET_LEX_STATE(EXPR_BEG|EXPR_LABEL);
			p->command_start = FALSE;
			(yyvsp[0].ctxt) = p->ctxt;
			p->ctxt.in_kwarg = 1;
			(yyval.tbl) = push_pvtbl(p);
		    }
#line 9985 "parse.c" /* yacc.c:1646  */
    break;

  case 474:
#line 3915 "parse.y" /* yacc.c:1646  */
    {
			(yyval.tbl) = push_pktbl(p);
		    }
#line 9993 "parse.c" /* yacc.c:1646  */
    break;

  case 475:
#line 3919 "parse.y" /* yacc.c:1646  */
    {
			pop_pktbl(p, (yyvsp[-2].tbl));
			pop_pvtbl(p, (yyvsp[-3].tbl));
			p->ctxt.in_kwarg = (yyvsp[-4].ctxt).in_kwarg;
		    }
#line 10003 "parse.c" /* yacc.c:1646  */
    break;

  case 476:
#line 3926 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_IN((yyvsp[-4].node), (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: in!($4, $7, escape_Qundef($8)) %*/
		    }
#line 10014 "parse.c" /* yacc.c:1646  */
    break;

  case 480:
#line 3940 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_if(p, (yyvsp[0].node), remove_begin((yyvsp[-2].node)), 0, &(yyloc));
			fixpos((yyval.node), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: if_mod!($3, $1) %*/
		    }
#line 10026 "parse.c" /* yacc.c:1646  */
    break;

  case 481:
#line 3948 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_unless(p, (yyvsp[0].node), remove_begin((yyvsp[-2].node)), 0, &(yyloc));
			fixpos((yyval.node), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: unless_mod!($3, $1) %*/
		    }
#line 10038 "parse.c" /* yacc.c:1646  */
    break;

  case 483:
#line 3959 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_array_pattern_tail(p, Qnone, 1, 0, Qnone, &(yyloc));
			(yyval.node) = new_array_pattern(p, Qnone, get_value((yyvsp[-1].node)), (yyval.node), &(yyloc));
		    }
#line 10047 "parse.c" /* yacc.c:1646  */
    break;

  case 484:
#line 3964 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_array_pattern(p, Qnone, get_value((yyvsp[-2].node)), (yyvsp[0].node), &(yyloc));
		    /*%%%*/
			nd_set_first_loc((yyval.node), (yylsp[-2]).beg_pos);
		    /*%
		    %*/
		    }
#line 10059 "parse.c" /* yacc.c:1646  */
    break;

  case 485:
#line 3972 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_find_pattern(p, Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 10067 "parse.c" /* yacc.c:1646  */
    break;

  case 486:
#line 3976 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_array_pattern(p, Qnone, Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 10075 "parse.c" /* yacc.c:1646  */
    break;

  case 487:
#line 3980 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_hash_pattern(p, Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 10083 "parse.c" /* yacc.c:1646  */
    break;

  case 489:
#line 3989 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			NODE *n = NEW_LIST((yyvsp[-2].node), &(yyloc));
			n = list_append(p, n, (yyvsp[0].node));
			(yyval.node) = new_hash(p, n, &(yyloc));
		    /*% %*/
		    /*% ripper: binary!($1, STATIC_ID2SYM((id_assoc)), $3) %*/
		    }
#line 10096 "parse.c" /* yacc.c:1646  */
    break;

  case 491:
#line 4001 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_NODE(NODE_OR, (yyvsp[-2].node), (yyvsp[0].node), 0, &(yyloc));
		    /*% %*/
		    /*% ripper: binary!($1, STATIC_ID2SYM(idOr), $3) %*/
		    }
#line 10107 "parse.c" /* yacc.c:1646  */
    break;

  case 493:
#line 4010 "parse.y" /* yacc.c:1646  */
    {(yyval.tbl) = push_pktbl(p);}
#line 10113 "parse.c" /* yacc.c:1646  */
    break;

  case 494:
#line 4011 "parse.y" /* yacc.c:1646  */
    {(yyval.tbl) = push_pktbl(p);}
#line 10119 "parse.c" /* yacc.c:1646  */
    break;

  case 496:
#line 4015 "parse.y" /* yacc.c:1646  */
    {
			pop_pktbl(p, (yyvsp[-2].tbl));
			(yyval.node) = new_array_pattern(p, (yyvsp[-3].node), Qnone, (yyvsp[-1].node), &(yyloc));
		    /*%%%*/
			nd_set_first_loc((yyval.node), (yylsp[-3]).beg_pos);
		    /*%
		    %*/
		    }
#line 10132 "parse.c" /* yacc.c:1646  */
    break;

  case 497:
#line 4024 "parse.y" /* yacc.c:1646  */
    {
			pop_pktbl(p, (yyvsp[-2].tbl));
			(yyval.node) = new_find_pattern(p, (yyvsp[-3].node), (yyvsp[-1].node), &(yyloc));
		    /*%%%*/
			nd_set_first_loc((yyval.node), (yylsp[-3]).beg_pos);
		    /*%
		    %*/
		    }
#line 10145 "parse.c" /* yacc.c:1646  */
    break;

  case 498:
#line 4033 "parse.y" /* yacc.c:1646  */
    {
			pop_pktbl(p, (yyvsp[-2].tbl));
			(yyval.node) = new_hash_pattern(p, (yyvsp[-3].node), (yyvsp[-1].node), &(yyloc));
		    /*%%%*/
			nd_set_first_loc((yyval.node), (yylsp[-3]).beg_pos);
		    /*%
		    %*/
		    }
#line 10158 "parse.c" /* yacc.c:1646  */
    break;

  case 499:
#line 4042 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_array_pattern_tail(p, Qnone, 0, 0, Qnone, &(yyloc));
			(yyval.node) = new_array_pattern(p, (yyvsp[-2].node), Qnone, (yyval.node), &(yyloc));
		    }
#line 10167 "parse.c" /* yacc.c:1646  */
    break;

  case 500:
#line 4047 "parse.y" /* yacc.c:1646  */
    {
			pop_pktbl(p, (yyvsp[-2].tbl));
			(yyval.node) = new_array_pattern(p, (yyvsp[-3].node), Qnone, (yyvsp[-1].node), &(yyloc));
		    /*%%%*/
			nd_set_first_loc((yyval.node), (yylsp[-3]).beg_pos);
		    /*%
		    %*/
		    }
#line 10180 "parse.c" /* yacc.c:1646  */
    break;

  case 501:
#line 4056 "parse.y" /* yacc.c:1646  */
    {
			pop_pktbl(p, (yyvsp[-2].tbl));
			(yyval.node) = new_find_pattern(p, (yyvsp[-3].node), (yyvsp[-1].node), &(yyloc));
		    /*%%%*/
			nd_set_first_loc((yyval.node), (yylsp[-3]).beg_pos);
		    /*%
		    %*/
		    }
#line 10193 "parse.c" /* yacc.c:1646  */
    break;

  case 502:
#line 4065 "parse.y" /* yacc.c:1646  */
    {
			pop_pktbl(p, (yyvsp[-2].tbl));
			(yyval.node) = new_hash_pattern(p, (yyvsp[-3].node), (yyvsp[-1].node), &(yyloc));
		    /*%%%*/
			nd_set_first_loc((yyval.node), (yylsp[-3]).beg_pos);
		    /*%
		    %*/
		    }
#line 10206 "parse.c" /* yacc.c:1646  */
    break;

  case 503:
#line 4074 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_array_pattern_tail(p, Qnone, 0, 0, Qnone, &(yyloc));
			(yyval.node) = new_array_pattern(p, (yyvsp[-2].node), Qnone, (yyval.node), &(yyloc));
		    }
#line 10215 "parse.c" /* yacc.c:1646  */
    break;

  case 504:
#line 4079 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_array_pattern(p, Qnone, Qnone, (yyvsp[-1].node), &(yyloc));
		    }
#line 10223 "parse.c" /* yacc.c:1646  */
    break;

  case 505:
#line 4083 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_find_pattern(p, Qnone, (yyvsp[-1].node), &(yyloc));
		    }
#line 10231 "parse.c" /* yacc.c:1646  */
    break;

  case 506:
#line 4087 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_array_pattern_tail(p, Qnone, 0, 0, Qnone, &(yyloc));
			(yyval.node) = new_array_pattern(p, Qnone, Qnone, (yyval.node), &(yyloc));
		    }
#line 10240 "parse.c" /* yacc.c:1646  */
    break;

  case 507:
#line 4092 "parse.y" /* yacc.c:1646  */
    {
			(yyval.tbl) = push_pktbl(p);
			(yyvsp[0].ctxt) = p->ctxt;
			p->ctxt.in_kwarg = 0;
		    }
#line 10250 "parse.c" /* yacc.c:1646  */
    break;

  case 508:
#line 4098 "parse.y" /* yacc.c:1646  */
    {
			pop_pktbl(p, (yyvsp[-2].tbl));
			p->ctxt.in_kwarg = (yyvsp[-3].ctxt).in_kwarg;
			(yyval.node) = new_hash_pattern(p, Qnone, (yyvsp[-1].node), &(yyloc));
		    }
#line 10260 "parse.c" /* yacc.c:1646  */
    break;

  case 509:
#line 4104 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_hash_pattern_tail(p, Qnone, 0, &(yyloc));
			(yyval.node) = new_hash_pattern(p, Qnone, (yyval.node), &(yyloc));
		    }
#line 10269 "parse.c" /* yacc.c:1646  */
    break;

  case 510:
#line 4108 "parse.y" /* yacc.c:1646  */
    {(yyval.tbl) = push_pktbl(p);}
#line 10275 "parse.c" /* yacc.c:1646  */
    break;

  case 511:
#line 4109 "parse.y" /* yacc.c:1646  */
    {
			pop_pktbl(p, (yyvsp[-2].tbl));
			(yyval.node) = (yyvsp[-1].node);
		    }
#line 10284 "parse.c" /* yacc.c:1646  */
    break;

  case 512:
#line 4116 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			NODE *pre_args = NEW_LIST((yyvsp[0].node), &(yyloc));
			(yyval.node) = new_array_pattern_tail(p, pre_args, 0, 0, Qnone, &(yyloc));
		    /*%
			$$ = new_array_pattern_tail(p, rb_ary_new_from_args(1, get_value($1)), 0, 0, Qnone, &@$);
		    %*/
		    }
#line 10297 "parse.c" /* yacc.c:1646  */
    break;

  case 513:
#line 4125 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_array_pattern_tail(p, (yyvsp[0].node), 1, 0, Qnone, &(yyloc));
		    }
#line 10305 "parse.c" /* yacc.c:1646  */
    break;

  case 514:
#line 4129 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_array_pattern_tail(p, list_concat((yyvsp[-1].node), (yyvsp[0].node)), 0, 0, Qnone, &(yyloc));
		    /*%
			VALUE pre_args = rb_ary_concat($1, get_value($2));
			$$ = new_array_pattern_tail(p, pre_args, 0, 0, Qnone, &@$);
		    %*/
		    }
#line 10318 "parse.c" /* yacc.c:1646  */
    break;

  case 515:
#line 4138 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_array_pattern_tail(p, (yyvsp[-2].node), 1, (yyvsp[0].id), Qnone, &(yyloc));
		    }
#line 10326 "parse.c" /* yacc.c:1646  */
    break;

  case 516:
#line 4142 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_array_pattern_tail(p, (yyvsp[-4].node), 1, (yyvsp[-2].id), (yyvsp[0].node), &(yyloc));
		    }
#line 10334 "parse.c" /* yacc.c:1646  */
    break;

  case 517:
#line 4146 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_array_pattern_tail(p, (yyvsp[-1].node), 1, 0, Qnone, &(yyloc));
		    }
#line 10342 "parse.c" /* yacc.c:1646  */
    break;

  case 518:
#line 4150 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_array_pattern_tail(p, (yyvsp[-3].node), 1, 0, (yyvsp[0].node), &(yyloc));
		    }
#line 10350 "parse.c" /* yacc.c:1646  */
    break;

  case 520:
#line 4157 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = (yyvsp[-1].node);
		    }
#line 10358 "parse.c" /* yacc.c:1646  */
    break;

  case 521:
#line 4161 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = list_concat((yyvsp[-2].node), (yyvsp[-1].node));
		    /*% %*/
		    /*% ripper: rb_ary_concat($1, get_value($2)) %*/
		    }
#line 10369 "parse.c" /* yacc.c:1646  */
    break;

  case 522:
#line 4170 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_array_pattern_tail(p, Qnone, 1, (yyvsp[0].id), Qnone, &(yyloc));
		    }
#line 10377 "parse.c" /* yacc.c:1646  */
    break;

  case 523:
#line 4174 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_array_pattern_tail(p, Qnone, 1, (yyvsp[-2].id), (yyvsp[0].node), &(yyloc));
		    }
#line 10385 "parse.c" /* yacc.c:1646  */
    break;

  case 524:
#line 4180 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_find_pattern_tail(p, (yyvsp[-4].id), (yyvsp[-2].node), (yyvsp[0].id), &(yyloc));

			if (rb_warning_category_enabled_p(RB_WARN_CATEGORY_EXPERIMENTAL))
			    rb_warn0L(nd_line((yyval.node)), "Find pattern is experimental, and the behavior may change in future versions of Ruby!");
		    }
#line 10396 "parse.c" /* yacc.c:1646  */
    break;

  case 525:
#line 4190 "parse.y" /* yacc.c:1646  */
    {
			(yyval.id) = (yyvsp[0].id);
		    }
#line 10404 "parse.c" /* yacc.c:1646  */
    break;

  case 526:
#line 4194 "parse.y" /* yacc.c:1646  */
    {
			(yyval.id) = 0;
		    }
#line 10412 "parse.c" /* yacc.c:1646  */
    break;

  case 528:
#line 4201 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = list_concat((yyvsp[-2].node), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: rb_ary_concat($1, get_value($3)) %*/
		    }
#line 10423 "parse.c" /* yacc.c:1646  */
    break;

  case 529:
#line 4210 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_LIST((yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: rb_ary_new_from_args(1, get_value($1)) %*/
		    }
#line 10434 "parse.c" /* yacc.c:1646  */
    break;

  case 530:
#line 4219 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) =  new_hash_pattern_tail(p, new_unique_key_hash(p, (yyvsp[-2].node), &(yyloc)), (yyvsp[0].id), &(yyloc));
		    }
#line 10442 "parse.c" /* yacc.c:1646  */
    break;

  case 531:
#line 4223 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) =  new_hash_pattern_tail(p, new_unique_key_hash(p, (yyvsp[0].node), &(yyloc)), 0, &(yyloc));
		    }
#line 10450 "parse.c" /* yacc.c:1646  */
    break;

  case 532:
#line 4227 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) =  new_hash_pattern_tail(p, new_unique_key_hash(p, (yyvsp[-1].node), &(yyloc)), 0, &(yyloc));
		    }
#line 10458 "parse.c" /* yacc.c:1646  */
    break;

  case 533:
#line 4231 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) =  new_hash_pattern_tail(p, new_hash(p, Qnone, &(yyloc)), (yyvsp[0].id), &(yyloc));
		    }
#line 10466 "parse.c" /* yacc.c:1646  */
    break;

  case 535:
#line 4239 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = list_concat((yyvsp[-2].node), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: rb_ary_push($1, $3) %*/
		    }
#line 10477 "parse.c" /* yacc.c:1646  */
    break;

  case 536:
#line 4248 "parse.y" /* yacc.c:1646  */
    {
			error_duplicate_pattern_key(p, get_id((yyvsp[-1].id)), &(yylsp[-1]));
		    /*%%%*/
			(yyval.node) = list_append(p, NEW_LIST(NEW_LIT(ID2SYM((yyvsp[-1].id)), &(yyloc)), &(yyloc)), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: rb_ary_new_from_args(2, get_value($1), get_value($2)) %*/
		    }
#line 10489 "parse.c" /* yacc.c:1646  */
    break;

  case 537:
#line 4256 "parse.y" /* yacc.c:1646  */
    {
			error_duplicate_pattern_key(p, get_id((yyvsp[0].id)), &(yylsp[0]));
			if ((yyvsp[0].id) && !is_local_id(get_id((yyvsp[0].id)))) {
			    yyerror1(&(yylsp[0]), "key must be valid as local variables");
			}
			error_duplicate_pattern_variable(p, get_id((yyvsp[0].id)), &(yylsp[0]));
		    /*%%%*/
			(yyval.node) = list_append(p, NEW_LIST(NEW_LIT(ID2SYM((yyvsp[0].id)), &(yyloc)), &(yyloc)), assignable(p, (yyvsp[0].id), 0, &(yyloc)));
		    /*% %*/
		    /*% ripper: rb_ary_new_from_args(2, get_value($1), Qnil) %*/
		    }
#line 10505 "parse.c" /* yacc.c:1646  */
    break;

  case 539:
#line 4271 "parse.y" /* yacc.c:1646  */
    {
			YYLTYPE loc = code_loc_gen(&(yylsp[-2]), &(yylsp[0]));
		    /*%%%*/
			if (!(yyvsp[-1].node) || nd_type((yyvsp[-1].node)) == NODE_STR) {
			    NODE *node = dsym_node(p, (yyvsp[-1].node), &loc);
			    (yyval.id) = SYM2ID(node->nd_lit);
			}
		    /*%
			if (ripper_is_node_yylval($2) && RNODE($2)->nd_cval) {
			    VALUE label = RNODE($2)->nd_cval;
			    VALUE rval = RNODE($2)->nd_rval;
			    $$ = ripper_new_yylval(p, rb_intern_str(label), rval, label);
			    RNODE($$)->nd_loc = loc;
			}
		    %*/
			else {
			    yyerror1(&loc, "symbol literal with interpolation is not allowed");
			    (yyval.id) = 0;
			}
		    }
#line 10530 "parse.c" /* yacc.c:1646  */
    break;

  case 540:
#line 4294 "parse.y" /* yacc.c:1646  */
    {
		        (yyval.id) = (yyvsp[0].id);
		    }
#line 10538 "parse.c" /* yacc.c:1646  */
    break;

  case 541:
#line 4298 "parse.y" /* yacc.c:1646  */
    {
		        (yyval.id) = 0;
		    }
#line 10546 "parse.c" /* yacc.c:1646  */
    break;

  case 542:
#line 4304 "parse.y" /* yacc.c:1646  */
    {
		        (yyval.id) = 0;
		    }
#line 10554 "parse.c" /* yacc.c:1646  */
    break;

  case 544:
#line 4310 "parse.y" /* yacc.c:1646  */
    {(yyval.id) = ID2VAL(idNil);}
#line 10560 "parse.c" /* yacc.c:1646  */
    break;

  case 546:
#line 4315 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			value_expr((yyvsp[-2].node));
			value_expr((yyvsp[0].node));
			(yyval.node) = NEW_DOT2((yyvsp[-2].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: dot2!($1, $3) %*/
		    }
#line 10573 "parse.c" /* yacc.c:1646  */
    break;

  case 547:
#line 4324 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			value_expr((yyvsp[-2].node));
			value_expr((yyvsp[0].node));
			(yyval.node) = NEW_DOT3((yyvsp[-2].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: dot3!($1, $3) %*/
		    }
#line 10586 "parse.c" /* yacc.c:1646  */
    break;

  case 548:
#line 4333 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			value_expr((yyvsp[-1].node));
			(yyval.node) = NEW_DOT2((yyvsp[-1].node), new_nil_at(p, &(yylsp[0]).end_pos), &(yyloc));
		    /*% %*/
		    /*% ripper: dot2!($1, Qnil) %*/
		    }
#line 10598 "parse.c" /* yacc.c:1646  */
    break;

  case 549:
#line 4341 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			value_expr((yyvsp[-1].node));
			(yyval.node) = NEW_DOT3((yyvsp[-1].node), new_nil_at(p, &(yylsp[0]).end_pos), &(yyloc));
		    /*% %*/
		    /*% ripper: dot3!($1, Qnil) %*/
		    }
#line 10610 "parse.c" /* yacc.c:1646  */
    break;

  case 553:
#line 4352 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			value_expr((yyvsp[0].node));
			(yyval.node) = NEW_DOT2(new_nil_at(p, &(yylsp[-1]).beg_pos), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: dot2!(Qnil, $2) %*/
		    }
#line 10622 "parse.c" /* yacc.c:1646  */
    break;

  case 554:
#line 4360 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			value_expr((yyvsp[0].node));
			(yyval.node) = NEW_DOT3(new_nil_at(p, &(yylsp[-1]).beg_pos), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: dot3!(Qnil, $2) %*/
		    }
#line 10634 "parse.c" /* yacc.c:1646  */
    break;

  case 563:
#line 4378 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			if (!((yyval.node) = gettable(p, (yyvsp[0].id), &(yyloc)))) (yyval.node) = NEW_BEGIN(0, &(yyloc));
		    /*% %*/
		    /*% ripper: var_ref!($1) %*/
		    }
#line 10645 "parse.c" /* yacc.c:1646  */
    break;

  case 565:
#line 4388 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			error_duplicate_pattern_variable(p, (yyvsp[0].id), &(yylsp[0]));
			(yyval.node) = assignable(p, (yyvsp[0].id), 0, &(yyloc));
		    /*% %*/
		    /*% ripper: assignable(p, var_field(p, $1)) %*/
		    }
#line 10657 "parse.c" /* yacc.c:1646  */
    break;

  case 566:
#line 4398 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			NODE *n = gettable(p, (yyvsp[0].id), &(yyloc));
			if (!(nd_type(n) == NODE_LVAR || nd_type(n) == NODE_DVAR)) {
			    compile_error(p, "%"PRIsVALUE": no such local variable", rb_id2str((yyvsp[0].id)));
			}
			(yyval.node) = n;
		    /*% %*/
		    /*% ripper: var_ref!($2) %*/
		    }
#line 10672 "parse.c" /* yacc.c:1646  */
    break;

  case 567:
#line 4411 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_COLON3((yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: top_const_ref!($2) %*/
		    }
#line 10683 "parse.c" /* yacc.c:1646  */
    break;

  case 568:
#line 4418 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_COLON2((yyvsp[-2].node), (yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: const_path_ref!($1, $3) %*/
		    }
#line 10694 "parse.c" /* yacc.c:1646  */
    break;

  case 569:
#line 4425 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = gettable(p, (yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: var_ref!($1) %*/
		   }
#line 10705 "parse.c" /* yacc.c:1646  */
    break;

  case 570:
#line 4436 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_RESBODY((yyvsp[-4].node),
					 (yyvsp[-3].node) ? block_append(p, node_assign(p, (yyvsp[-3].node), NEW_ERRINFO(&(yylsp[-3])), &(yylsp[-3])), (yyvsp[-1].node)) : (yyvsp[-1].node),
					 (yyvsp[0].node), &(yyloc));
			fixpos((yyval.node), (yyvsp[-4].node)?(yyvsp[-4].node):(yyvsp[-1].node));
		    /*% %*/
		    /*% ripper: rescue!(escape_Qundef($2), escape_Qundef($3), escape_Qundef($5), escape_Qundef($6)) %*/
		    }
#line 10719 "parse.c" /* yacc.c:1646  */
    break;

  case 572:
#line 4449 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_LIST((yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: rb_ary_new3(1, get_value($1)) %*/
		    }
#line 10730 "parse.c" /* yacc.c:1646  */
    break;

  case 573:
#line 4456 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			if (!((yyval.node) = splat_array((yyvsp[0].node)))) (yyval.node) = (yyvsp[0].node);
		    /*% %*/
		    /*% ripper: $1 %*/
		    }
#line 10741 "parse.c" /* yacc.c:1646  */
    break;

  case 575:
#line 4466 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = (yyvsp[0].node);
		    }
#line 10749 "parse.c" /* yacc.c:1646  */
    break;

  case 577:
#line 4473 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[0].node);
		    /*% %*/
		    /*% ripper: ensure!($2) %*/
		    }
#line 10760 "parse.c" /* yacc.c:1646  */
    break;

  case 581:
#line 4487 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			NODE *node = (yyvsp[0].node);
			if (!node) {
			    node = NEW_STR(STR_NEW0(), &(yyloc));
                            RB_OBJ_WRITTEN(p->ast, Qnil, node->nd_lit);
			}
			else {
			    node = evstr2dstr(p, node);
			}
			(yyval.node) = node;
		    /*% %*/
		    /*% ripper: $1 %*/
		    }
#line 10779 "parse.c" /* yacc.c:1646  */
    break;

  case 584:
#line 4506 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = literal_concat(p, (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: string_concat!($1, $2) %*/
		    }
#line 10790 "parse.c" /* yacc.c:1646  */
    break;

  case 585:
#line 4515 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = heredoc_dedent(p, (yyvsp[-1].node));
			if ((yyval.node)) nd_set_loc((yyval.node), &(yyloc));
		    /*% %*/
		    /*% ripper: string_literal!(heredoc_dedent(p, $2)) %*/
		    }
#line 10802 "parse.c" /* yacc.c:1646  */
    break;

  case 586:
#line 4525 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_xstring(p, heredoc_dedent(p, (yyvsp[-1].node)), &(yyloc));
		    /*% %*/
		    /*% ripper: xstring_literal!(heredoc_dedent(p, $2)) %*/
		    }
#line 10813 "parse.c" /* yacc.c:1646  */
    break;

  case 587:
#line 4534 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_regexp(p, (yyvsp[-1].node), (yyvsp[0].num), &(yyloc));
		    }
#line 10821 "parse.c" /* yacc.c:1646  */
    break;

  case 588:
#line 4540 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = make_list((yyvsp[-1].node), &(yyloc));
		    /*% %*/
		    /*% ripper: array!($3) %*/
		    }
#line 10832 "parse.c" /* yacc.c:1646  */
    break;

  case 589:
#line 4549 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = 0;
		    /*% %*/
		    /*% ripper: words_new! %*/
		    }
#line 10843 "parse.c" /* yacc.c:1646  */
    break;

  case 590:
#line 4556 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = list_append(p, (yyvsp[-2].node), evstr2dstr(p, (yyvsp[-1].node)));
		    /*% %*/
		    /*% ripper: words_add!($1, $2) %*/
		    }
#line 10854 "parse.c" /* yacc.c:1646  */
    break;

  case 592:
#line 4567 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = literal_concat(p, (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: word_add!($1, $2) %*/
		    }
#line 10865 "parse.c" /* yacc.c:1646  */
    break;

  case 593:
#line 4576 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = make_list((yyvsp[-1].node), &(yyloc));
		    /*% %*/
		    /*% ripper: array!($3) %*/
		    }
#line 10876 "parse.c" /* yacc.c:1646  */
    break;

  case 594:
#line 4585 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = 0;
		    /*% %*/
		    /*% ripper: symbols_new! %*/
		    }
#line 10887 "parse.c" /* yacc.c:1646  */
    break;

  case 595:
#line 4592 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = symbol_append(p, (yyvsp[-2].node), evstr2dstr(p, (yyvsp[-1].node)));
		    /*% %*/
		    /*% ripper: symbols_add!($1, $2) %*/
		    }
#line 10898 "parse.c" /* yacc.c:1646  */
    break;

  case 596:
#line 4601 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = make_list((yyvsp[-1].node), &(yyloc));
		    /*% %*/
		    /*% ripper: array!($3) %*/
		    }
#line 10909 "parse.c" /* yacc.c:1646  */
    break;

  case 597:
#line 4610 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = make_list((yyvsp[-1].node), &(yyloc));
		    /*% %*/
		    /*% ripper: array!($3) %*/
		    }
#line 10920 "parse.c" /* yacc.c:1646  */
    break;

  case 598:
#line 4619 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = 0;
		    /*% %*/
		    /*% ripper: qwords_new! %*/
		    }
#line 10931 "parse.c" /* yacc.c:1646  */
    break;

  case 599:
#line 4626 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = list_append(p, (yyvsp[-2].node), (yyvsp[-1].node));
		    /*% %*/
		    /*% ripper: qwords_add!($1, $2) %*/
		    }
#line 10942 "parse.c" /* yacc.c:1646  */
    break;

  case 600:
#line 4635 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = 0;
		    /*% %*/
		    /*% ripper: qsymbols_new! %*/
		    }
#line 10953 "parse.c" /* yacc.c:1646  */
    break;

  case 601:
#line 4642 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = symbol_append(p, (yyvsp[-2].node), (yyvsp[-1].node));
		    /*% %*/
		    /*% ripper: qsymbols_add!($1, $2) %*/
		    }
#line 10964 "parse.c" /* yacc.c:1646  */
    break;

  case 602:
#line 4651 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = 0;
		    /*% %*/
		    /*% ripper: string_content! %*/
		    /*%%%*/
		    /*%
			$$ = ripper_new_yylval(p, 0, $$, 0);
		    %*/
		    }
#line 10979 "parse.c" /* yacc.c:1646  */
    break;

  case 603:
#line 4662 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = literal_concat(p, (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: string_add!($1, $2) %*/
		    /*%%%*/
		    /*%
			if (ripper_is_node_yylval($1) && ripper_is_node_yylval($2) &&
			    !RNODE($1)->nd_cval) {
			    RNODE($1)->nd_cval = RNODE($2)->nd_cval;
			    RNODE($1)->nd_rval = add_mark_object(p, $$);
			    $$ = $1;
			}
		    %*/
		    }
#line 10999 "parse.c" /* yacc.c:1646  */
    break;

  case 604:
#line 4680 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = 0;
		    /*% %*/
		    /*% ripper: xstring_new! %*/
		    }
#line 11010 "parse.c" /* yacc.c:1646  */
    break;

  case 605:
#line 4687 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = literal_concat(p, (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: xstring_add!($1, $2) %*/
		    }
#line 11021 "parse.c" /* yacc.c:1646  */
    break;

  case 606:
#line 4696 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = 0;
		    /*% %*/
		    /*% ripper: regexp_new! %*/
		    /*%%%*/
		    /*%
			$$ = ripper_new_yylval(p, 0, $$, 0);
		    %*/
		    }
#line 11036 "parse.c" /* yacc.c:1646  */
    break;

  case 607:
#line 4707 "parse.y" /* yacc.c:1646  */
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
				head = list_append(p, NEW_DSTR(Qnil, &(yyloc)), head);
				break;
			    }
			    (yyval.node) = list_append(p, head, tail);
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
			    $$ = ripper_new_yylval(p, 0, $$, s2);
			}
		    %*/
		    }
#line 11079 "parse.c" /* yacc.c:1646  */
    break;

  case 609:
#line 4750 "parse.y" /* yacc.c:1646  */
    {
			/* need to backup p->lex.strterm so that a string literal `%&foo,#$&,bar&` can be parsed */
			(yyval.strterm) = p->lex.strterm;
			p->lex.strterm = 0;
			SET_LEX_STATE(EXPR_BEG);
		    }
#line 11090 "parse.c" /* yacc.c:1646  */
    break;

  case 610:
#line 4757 "parse.y" /* yacc.c:1646  */
    {
			p->lex.strterm = (yyvsp[-1].strterm);
		    /*%%%*/
			(yyval.node) = NEW_EVSTR((yyvsp[0].node), &(yyloc));
			nd_set_line((yyval.node), (yylsp[0]).end_pos.lineno);
		    /*% %*/
		    /*% ripper: string_dvar!($3) %*/
		    }
#line 11103 "parse.c" /* yacc.c:1646  */
    break;

  case 611:
#line 4766 "parse.y" /* yacc.c:1646  */
    {
			CMDARG_PUSH(0);
			COND_PUSH(0);
		    }
#line 11112 "parse.c" /* yacc.c:1646  */
    break;

  case 612:
#line 4770 "parse.y" /* yacc.c:1646  */
    {
			/* need to backup p->lex.strterm so that a string literal `%!foo,#{ !0 },bar!` can be parsed */
			(yyval.strterm) = p->lex.strterm;
			p->lex.strterm = 0;
		    }
#line 11122 "parse.c" /* yacc.c:1646  */
    break;

  case 613:
#line 4775 "parse.y" /* yacc.c:1646  */
    {
			(yyval.num) = p->lex.state;
			SET_LEX_STATE(EXPR_BEG);
		    }
#line 11131 "parse.c" /* yacc.c:1646  */
    break;

  case 614:
#line 4779 "parse.y" /* yacc.c:1646  */
    {
			(yyval.num) = p->lex.brace_nest;
			p->lex.brace_nest = 0;
		    }
#line 11140 "parse.c" /* yacc.c:1646  */
    break;

  case 615:
#line 4783 "parse.y" /* yacc.c:1646  */
    {
			(yyval.num) = p->heredoc_indent;
			p->heredoc_indent = 0;
		    }
#line 11149 "parse.c" /* yacc.c:1646  */
    break;

  case 616:
#line 4788 "parse.y" /* yacc.c:1646  */
    {
			COND_POP();
			CMDARG_POP();
			p->lex.strterm = (yyvsp[-5].strterm);
			SET_LEX_STATE((yyvsp[-4].num));
			p->lex.brace_nest = (yyvsp[-3].num);
			p->heredoc_indent = (yyvsp[-2].num);
			p->heredoc_line_indent = -1;
		    /*%%%*/
			if ((yyvsp[-1].node)) (yyvsp[-1].node)->flags &= ~NODE_FL_NEWLINE;
			(yyval.node) = new_evstr(p, (yyvsp[-1].node), &(yyloc));
		    /*% %*/
		    /*% ripper: string_embexpr!($7) %*/
		    }
#line 11168 "parse.c" /* yacc.c:1646  */
    break;

  case 617:
#line 4805 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_GVAR((yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: var_ref!($1) %*/
		    }
#line 11179 "parse.c" /* yacc.c:1646  */
    break;

  case 618:
#line 4812 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_IVAR((yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: var_ref!($1) %*/
		    }
#line 11190 "parse.c" /* yacc.c:1646  */
    break;

  case 619:
#line 4819 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = NEW_CVAR((yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: var_ref!($1) %*/
		    }
#line 11201 "parse.c" /* yacc.c:1646  */
    break;

  case 623:
#line 4833 "parse.y" /* yacc.c:1646  */
    {
			SET_LEX_STATE(EXPR_END);
		    /*%%%*/
			(yyval.node) = NEW_LIT(ID2SYM((yyvsp[0].id)), &(yyloc));
		    /*% %*/
		    /*% ripper: symbol_literal!(symbol!($2)) %*/
		    }
#line 11213 "parse.c" /* yacc.c:1646  */
    break;

  case 628:
#line 4849 "parse.y" /* yacc.c:1646  */
    {
			SET_LEX_STATE(EXPR_END);
		    /*%%%*/
			(yyval.node) = dsym_node(p, (yyvsp[-1].node), &(yyloc));
		    /*% %*/
		    /*% ripper: dyna_symbol!($2) %*/
		    }
#line 11225 "parse.c" /* yacc.c:1646  */
    break;

  case 630:
#line 4860 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[0].node);
			RB_OBJ_WRITE(p->ast, &(yyval.node)->nd_lit, negate_lit(p, (yyval.node)->nd_lit));
		    /*% %*/
		    /*% ripper: unary!(ID2VAL(idUMinus), $2) %*/
		    }
#line 11237 "parse.c" /* yacc.c:1646  */
    break;

  case 640:
#line 4882 "parse.y" /* yacc.c:1646  */
    {(yyval.id) = KWD2EID(nil, (yyvsp[0].id));}
#line 11243 "parse.c" /* yacc.c:1646  */
    break;

  case 641:
#line 4883 "parse.y" /* yacc.c:1646  */
    {(yyval.id) = KWD2EID(self, (yyvsp[0].id));}
#line 11249 "parse.c" /* yacc.c:1646  */
    break;

  case 642:
#line 4884 "parse.y" /* yacc.c:1646  */
    {(yyval.id) = KWD2EID(true, (yyvsp[0].id));}
#line 11255 "parse.c" /* yacc.c:1646  */
    break;

  case 643:
#line 4885 "parse.y" /* yacc.c:1646  */
    {(yyval.id) = KWD2EID(false, (yyvsp[0].id));}
#line 11261 "parse.c" /* yacc.c:1646  */
    break;

  case 644:
#line 4886 "parse.y" /* yacc.c:1646  */
    {(yyval.id) = KWD2EID(_FILE__, (yyvsp[0].id));}
#line 11267 "parse.c" /* yacc.c:1646  */
    break;

  case 645:
#line 4887 "parse.y" /* yacc.c:1646  */
    {(yyval.id) = KWD2EID(_LINE__, (yyvsp[0].id));}
#line 11273 "parse.c" /* yacc.c:1646  */
    break;

  case 646:
#line 4888 "parse.y" /* yacc.c:1646  */
    {(yyval.id) = KWD2EID(_ENCODING__, (yyvsp[0].id));}
#line 11279 "parse.c" /* yacc.c:1646  */
    break;

  case 647:
#line 4892 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			if (!((yyval.node) = gettable(p, (yyvsp[0].id), &(yyloc)))) (yyval.node) = NEW_BEGIN(0, &(yyloc));
		    /*%
			if (id_is_var(p, get_id($1))) {
			    $$ = dispatch1(var_ref, $1);
			}
			else {
			    $$ = dispatch1(vcall, $1);
			}
		    %*/
		    }
#line 11296 "parse.c" /* yacc.c:1646  */
    break;

  case 648:
#line 4905 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			if (!((yyval.node) = gettable(p, (yyvsp[0].id), &(yyloc)))) (yyval.node) = NEW_BEGIN(0, &(yyloc));
		    /*% %*/
		    /*% ripper: var_ref!($1) %*/
		    }
#line 11307 "parse.c" /* yacc.c:1646  */
    break;

  case 649:
#line 4914 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = assignable(p, (yyvsp[0].id), 0, &(yyloc));
		    /*% %*/
		    /*% ripper: assignable(p, var_field(p, $1)) %*/
		    }
#line 11318 "parse.c" /* yacc.c:1646  */
    break;

  case 650:
#line 4921 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = assignable(p, (yyvsp[0].id), 0, &(yyloc));
		    /*% %*/
		    /*% ripper: assignable(p, var_field(p, $1)) %*/
		    }
#line 11329 "parse.c" /* yacc.c:1646  */
    break;

  case 653:
#line 4934 "parse.y" /* yacc.c:1646  */
    {
			SET_LEX_STATE(EXPR_BEG);
			p->command_start = TRUE;
		    }
#line 11338 "parse.c" /* yacc.c:1646  */
    break;

  case 654:
#line 4939 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = (yyvsp[-1].node);
		    }
#line 11346 "parse.c" /* yacc.c:1646  */
    break;

  case 655:
#line 4943 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = 0;
		    /*% %*/
		    /*% ripper: Qnil %*/
		    }
#line 11357 "parse.c" /* yacc.c:1646  */
    break;

  case 658:
#line 4954 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node);
		    /*% %*/
		    /*% ripper: paren!($2) %*/
			SET_LEX_STATE(EXPR_BEG);
			p->command_start = TRUE;
		    }
#line 11370 "parse.c" /* yacc.c:1646  */
    break;

  case 659:
#line 4963 "parse.y" /* yacc.c:1646  */
    {
			add_forwarding_args(p);
		    /*%%%*/
			(yyval.node) = new_args_forward_def(p, (yyvsp[-3].node), &(yyloc));
		    /*% %*/
		    /*% ripper: paren!(params!($2, Qnone, $4, Qnone, Qnone, Qnone, Qnone)) %*/
			SET_LEX_STATE(EXPR_BEG);
			p->command_start = TRUE;
		    }
#line 11384 "parse.c" /* yacc.c:1646  */
    break;

  case 660:
#line 4973 "parse.y" /* yacc.c:1646  */
    {
			add_forwarding_args(p);
		    /*%%%*/
			(yyval.node) = new_args_forward_def(p, 0, &(yyloc));
		    /*% %*/
		    /*% ripper: paren!(params!(Qnone, Qnone, $2, Qnone, Qnone, Qnone, Qnone)) %*/
			SET_LEX_STATE(EXPR_BEG);
			p->command_start = TRUE;
		    }
#line 11398 "parse.c" /* yacc.c:1646  */
    break;

  case 662:
#line 4985 "parse.y" /* yacc.c:1646  */
    {
			(yyval.ctxt) = p->ctxt;
			p->ctxt.in_kwarg = 1;
			SET_LEX_STATE(p->lex.state|EXPR_LABEL); /* force for args */
		    }
#line 11408 "parse.c" /* yacc.c:1646  */
    break;

  case 663:
#line 4991 "parse.y" /* yacc.c:1646  */
    {
			p->ctxt.in_kwarg = (yyvsp[-2].ctxt).in_kwarg;
			(yyval.node) = (yyvsp[-1].node);
			SET_LEX_STATE(EXPR_BEG);
			p->command_start = TRUE;
		    }
#line 11419 "parse.c" /* yacc.c:1646  */
    break;

  case 664:
#line 5000 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args_tail(p, (yyvsp[-3].node), (yyvsp[-1].id), (yyvsp[0].id), &(yylsp[-1]));
		    }
#line 11427 "parse.c" /* yacc.c:1646  */
    break;

  case 665:
#line 5004 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args_tail(p, (yyvsp[-1].node), Qnone, (yyvsp[0].id), &(yylsp[-1]));
		    }
#line 11435 "parse.c" /* yacc.c:1646  */
    break;

  case 666:
#line 5008 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args_tail(p, Qnone, (yyvsp[-1].id), (yyvsp[0].id), &(yylsp[-1]));
		    }
#line 11443 "parse.c" /* yacc.c:1646  */
    break;

  case 667:
#line 5012 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args_tail(p, Qnone, Qnone, (yyvsp[0].id), &(yylsp[0]));
		    }
#line 11451 "parse.c" /* yacc.c:1646  */
    break;

  case 668:
#line 5018 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = (yyvsp[0].node);
		    }
#line 11459 "parse.c" /* yacc.c:1646  */
    break;

  case 669:
#line 5022 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args_tail(p, Qnone, Qnone, Qnone, &(yylsp[0]));
		    }
#line 11467 "parse.c" /* yacc.c:1646  */
    break;

  case 670:
#line 5028 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(p, (yyvsp[-5].node), (yyvsp[-3].node), (yyvsp[-1].id), Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 11475 "parse.c" /* yacc.c:1646  */
    break;

  case 671:
#line 5032 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(p, (yyvsp[-7].node), (yyvsp[-5].node), (yyvsp[-3].id), (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    }
#line 11483 "parse.c" /* yacc.c:1646  */
    break;

  case 672:
#line 5036 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(p, (yyvsp[-3].node), (yyvsp[-1].node), Qnone, Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 11491 "parse.c" /* yacc.c:1646  */
    break;

  case 673:
#line 5040 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(p, (yyvsp[-5].node), (yyvsp[-3].node), Qnone, (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    }
#line 11499 "parse.c" /* yacc.c:1646  */
    break;

  case 674:
#line 5044 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(p, (yyvsp[-3].node), Qnone, (yyvsp[-1].id), Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 11507 "parse.c" /* yacc.c:1646  */
    break;

  case 675:
#line 5048 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(p, (yyvsp[-5].node), Qnone, (yyvsp[-3].id), (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    }
#line 11515 "parse.c" /* yacc.c:1646  */
    break;

  case 676:
#line 5052 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(p, (yyvsp[-1].node), Qnone, Qnone, Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 11523 "parse.c" /* yacc.c:1646  */
    break;

  case 677:
#line 5056 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(p, Qnone, (yyvsp[-3].node), (yyvsp[-1].id), Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 11531 "parse.c" /* yacc.c:1646  */
    break;

  case 678:
#line 5060 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(p, Qnone, (yyvsp[-5].node), (yyvsp[-3].id), (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    }
#line 11539 "parse.c" /* yacc.c:1646  */
    break;

  case 679:
#line 5064 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(p, Qnone, (yyvsp[-1].node), Qnone, Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 11547 "parse.c" /* yacc.c:1646  */
    break;

  case 680:
#line 5068 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(p, Qnone, (yyvsp[-3].node), Qnone, (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    }
#line 11555 "parse.c" /* yacc.c:1646  */
    break;

  case 681:
#line 5072 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(p, Qnone, Qnone, (yyvsp[-1].id), Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 11563 "parse.c" /* yacc.c:1646  */
    break;

  case 682:
#line 5076 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(p, Qnone, Qnone, (yyvsp[-3].id), (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    }
#line 11571 "parse.c" /* yacc.c:1646  */
    break;

  case 683:
#line 5080 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args(p, Qnone, Qnone, Qnone, Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 11579 "parse.c" /* yacc.c:1646  */
    break;

  case 684:
#line 5084 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = new_args_tail(p, Qnone, Qnone, Qnone, &(yylsp[0]));
			(yyval.node) = new_args(p, Qnone, Qnone, Qnone, Qnone, (yyval.node), &(yylsp[0]));
		    }
#line 11588 "parse.c" /* yacc.c:1646  */
    break;

  case 685:
#line 5091 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.id) = idDot3;
		    /*% %*/
		    /*% ripper: args_forward! %*/
		    }
#line 11599 "parse.c" /* yacc.c:1646  */
    break;

  case 686:
#line 5100 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			yyerror1(&(yylsp[0]), "formal argument cannot be a constant");
			(yyval.id) = 0;
		    /*% %*/
		    /*% ripper[error]: param_error!($1) %*/
		    }
#line 11611 "parse.c" /* yacc.c:1646  */
    break;

  case 687:
#line 5108 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			yyerror1(&(yylsp[0]), "formal argument cannot be an instance variable");
			(yyval.id) = 0;
		    /*% %*/
		    /*% ripper[error]: param_error!($1) %*/
		    }
#line 11623 "parse.c" /* yacc.c:1646  */
    break;

  case 688:
#line 5116 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			yyerror1(&(yylsp[0]), "formal argument cannot be a global variable");
			(yyval.id) = 0;
		    /*% %*/
		    /*% ripper[error]: param_error!($1) %*/
		    }
#line 11635 "parse.c" /* yacc.c:1646  */
    break;

  case 689:
#line 5124 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			yyerror1(&(yylsp[0]), "formal argument cannot be a class variable");
			(yyval.id) = 0;
		    /*% %*/
		    /*% ripper[error]: param_error!($1) %*/
		    }
#line 11647 "parse.c" /* yacc.c:1646  */
    break;

  case 691:
#line 5135 "parse.y" /* yacc.c:1646  */
    {
			formal_argument(p, get_id((yyvsp[0].id)));
			p->max_numparam = ORDINAL_PARAM;
			(yyval.id) = (yyvsp[0].id);
		    }
#line 11657 "parse.c" /* yacc.c:1646  */
    break;

  case 692:
#line 5143 "parse.y" /* yacc.c:1646  */
    {
			ID id = get_id((yyvsp[0].id));
			arg_var(p, id);
			p->cur_arg = id;
			(yyval.id) = (yyvsp[0].id);
		    }
#line 11668 "parse.c" /* yacc.c:1646  */
    break;

  case 693:
#line 5152 "parse.y" /* yacc.c:1646  */
    {
			p->cur_arg = 0;
		    /*%%%*/
			(yyval.node) = NEW_ARGS_AUX((yyvsp[0].id), 1, &NULL_LOC);
		    /*% %*/
		    /*% ripper: get_value($1) %*/
		    }
#line 11680 "parse.c" /* yacc.c:1646  */
    break;

  case 694:
#line 5160 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			ID tid = internal_id(p);
			YYLTYPE loc;
			loc.beg_pos = (yylsp[-1]).beg_pos;
			loc.end_pos = (yylsp[-1]).beg_pos;
			arg_var(p, tid);
			if (dyna_in_block(p)) {
			    (yyvsp[-1].node)->nd_value = NEW_DVAR(tid, &loc);
			}
			else {
			    (yyvsp[-1].node)->nd_value = NEW_LVAR(tid, &loc);
			}
			(yyval.node) = NEW_ARGS_AUX(tid, 1, &NULL_LOC);
			(yyval.node)->nd_next = (yyvsp[-1].node);
		    /*% %*/
		    /*% ripper: mlhs_paren!($2) %*/
		    }
#line 11703 "parse.c" /* yacc.c:1646  */
    break;

  case 696:
#line 5183 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-2].node);
			(yyval.node)->nd_plen++;
			(yyval.node)->nd_next = block_append(p, (yyval.node)->nd_next, (yyvsp[0].node)->nd_next);
			rb_discard_node(p, (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: rb_ary_push($1, get_value($3)) %*/
		    }
#line 11717 "parse.c" /* yacc.c:1646  */
    break;

  case 697:
#line 5196 "parse.y" /* yacc.c:1646  */
    {
			ID id = get_id((yyvsp[0].id));
			arg_var(p, formal_argument(p, id));
			p->cur_arg = id;
			p->max_numparam = ORDINAL_PARAM;
			(yyval.id) = (yyvsp[0].id);
		    }
#line 11729 "parse.c" /* yacc.c:1646  */
    break;

  case 698:
#line 5206 "parse.y" /* yacc.c:1646  */
    {
			p->cur_arg = 0;
		    /*%%%*/
			(yyval.node) = new_kw_arg(p, assignable(p, (yyvsp[-1].id), (yyvsp[0].node), &(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: rb_assoc_new(get_value(assignable(p, $1)), get_value($2)) %*/
		    }
#line 11741 "parse.c" /* yacc.c:1646  */
    break;

  case 699:
#line 5214 "parse.y" /* yacc.c:1646  */
    {
			p->cur_arg = 0;
		    /*%%%*/
			(yyval.node) = new_kw_arg(p, assignable(p, (yyvsp[0].id), NODE_SPECIAL_REQUIRED_KEYWORD, &(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: rb_assoc_new(get_value(assignable(p, $1)), 0) %*/
		    }
#line 11753 "parse.c" /* yacc.c:1646  */
    break;

  case 700:
#line 5224 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_kw_arg(p, assignable(p, (yyvsp[-1].id), (yyvsp[0].node), &(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: rb_assoc_new(get_value(assignable(p, $1)), get_value($2)) %*/
		    }
#line 11764 "parse.c" /* yacc.c:1646  */
    break;

  case 701:
#line 5231 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = new_kw_arg(p, assignable(p, (yyvsp[0].id), NODE_SPECIAL_REQUIRED_KEYWORD, &(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: rb_assoc_new(get_value(assignable(p, $1)), 0) %*/
		    }
#line 11775 "parse.c" /* yacc.c:1646  */
    break;

  case 702:
#line 5240 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[0].node);
		    /*% %*/
		    /*% ripper: rb_ary_new3(1, get_value($1)) %*/
		    }
#line 11786 "parse.c" /* yacc.c:1646  */
    break;

  case 703:
#line 5247 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = kwd_append((yyvsp[-2].node), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: rb_ary_push($1, get_value($3)) %*/
		    }
#line 11797 "parse.c" /* yacc.c:1646  */
    break;

  case 704:
#line 5257 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[0].node);
		    /*% %*/
		    /*% ripper: rb_ary_new3(1, get_value($1)) %*/
		    }
#line 11808 "parse.c" /* yacc.c:1646  */
    break;

  case 705:
#line 5264 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = kwd_append((yyvsp[-2].node), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: rb_ary_push($1, get_value($3)) %*/
		    }
#line 11819 "parse.c" /* yacc.c:1646  */
    break;

  case 708:
#line 5277 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
		    /*% %*/
		    /*% ripper: nokw_param!(Qnil) %*/
		    }
#line 11829 "parse.c" /* yacc.c:1646  */
    break;

  case 709:
#line 5285 "parse.y" /* yacc.c:1646  */
    {
			arg_var(p, shadowing_lvar(p, get_id((yyvsp[0].id))));
		    /*%%%*/
			(yyval.id) = (yyvsp[0].id);
		    /*% %*/
		    /*% ripper: kwrest_param!($2) %*/
		    }
#line 11841 "parse.c" /* yacc.c:1646  */
    break;

  case 710:
#line 5293 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.id) = internal_id(p);
			arg_var(p, (yyval.id));
		    /*% %*/
		    /*% ripper: kwrest_param!(Qnil) %*/
		    }
#line 11853 "parse.c" /* yacc.c:1646  */
    break;

  case 711:
#line 5303 "parse.y" /* yacc.c:1646  */
    {
			p->cur_arg = 0;
		    /*%%%*/
			(yyval.node) = NEW_OPT_ARG(0, assignable(p, (yyvsp[-2].id), (yyvsp[0].node), &(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: rb_assoc_new(get_value(assignable(p, $1)), get_value($3)) %*/
		    }
#line 11865 "parse.c" /* yacc.c:1646  */
    break;

  case 712:
#line 5313 "parse.y" /* yacc.c:1646  */
    {
			p->cur_arg = 0;
		    /*%%%*/
			(yyval.node) = NEW_OPT_ARG(0, assignable(p, (yyvsp[-2].id), (yyvsp[0].node), &(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: rb_assoc_new(get_value(assignable(p, $1)), get_value($3)) %*/
		    }
#line 11877 "parse.c" /* yacc.c:1646  */
    break;

  case 713:
#line 5323 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[0].node);
		    /*% %*/
		    /*% ripper: rb_ary_new3(1, get_value($1)) %*/
		    }
#line 11888 "parse.c" /* yacc.c:1646  */
    break;

  case 714:
#line 5330 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = opt_arg_append((yyvsp[-2].node), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: rb_ary_push($1, get_value($3)) %*/
		    }
#line 11899 "parse.c" /* yacc.c:1646  */
    break;

  case 715:
#line 5339 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[0].node);
		    /*% %*/
		    /*% ripper: rb_ary_new3(1, get_value($1)) %*/
		    }
#line 11910 "parse.c" /* yacc.c:1646  */
    break;

  case 716:
#line 5346 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = opt_arg_append((yyvsp[-2].node), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: rb_ary_push($1, get_value($3)) %*/
		    }
#line 11921 "parse.c" /* yacc.c:1646  */
    break;

  case 719:
#line 5359 "parse.y" /* yacc.c:1646  */
    {
			arg_var(p, shadowing_lvar(p, get_id((yyvsp[0].id))));
		    /*%%%*/
			(yyval.id) = (yyvsp[0].id);
		    /*% %*/
		    /*% ripper: rest_param!($2) %*/
		    }
#line 11933 "parse.c" /* yacc.c:1646  */
    break;

  case 720:
#line 5367 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.id) = internal_id(p);
			arg_var(p, (yyval.id));
		    /*% %*/
		    /*% ripper: rest_param!(Qnil) %*/
		    }
#line 11945 "parse.c" /* yacc.c:1646  */
    break;

  case 723:
#line 5381 "parse.y" /* yacc.c:1646  */
    {
			arg_var(p, shadowing_lvar(p, get_id((yyvsp[0].id))));
		    /*%%%*/
			(yyval.id) = (yyvsp[0].id);
		    /*% %*/
		    /*% ripper: blockarg!($2) %*/
		    }
#line 11957 "parse.c" /* yacc.c:1646  */
    break;

  case 724:
#line 5391 "parse.y" /* yacc.c:1646  */
    {
			(yyval.id) = (yyvsp[0].id);
		    }
#line 11965 "parse.c" /* yacc.c:1646  */
    break;

  case 725:
#line 5395 "parse.y" /* yacc.c:1646  */
    {
			(yyval.id) = Qnull;
		    }
#line 11973 "parse.c" /* yacc.c:1646  */
    break;

  case 726:
#line 5401 "parse.y" /* yacc.c:1646  */
    {
			value_expr((yyvsp[0].node));
			(yyval.node) = (yyvsp[0].node);
		    }
#line 11982 "parse.c" /* yacc.c:1646  */
    break;

  case 727:
#line 5405 "parse.y" /* yacc.c:1646  */
    {SET_LEX_STATE(EXPR_BEG);}
#line 11988 "parse.c" /* yacc.c:1646  */
    break;

  case 728:
#line 5406 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			switch (nd_type((yyvsp[-1].node))) {
			  case NODE_STR:
			  case NODE_DSTR:
			  case NODE_XSTR:
			  case NODE_DXSTR:
			  case NODE_DREGX:
			  case NODE_LIT:
			  case NODE_LIST:
			  case NODE_ZLIST:
			    yyerror1(&(yylsp[-1]), "can't define singleton method for literals");
			    break;
			  default:
			    value_expr((yyvsp[-1].node));
			    break;
			}
			(yyval.node) = (yyvsp[-1].node);
		    /*% %*/
		    /*% ripper: paren!($3) %*/
		    }
#line 12014 "parse.c" /* yacc.c:1646  */
    break;

  case 730:
#line 5431 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node);
		    /*% %*/
		    /*% ripper: assoclist_from_args!($1) %*/
		    }
#line 12025 "parse.c" /* yacc.c:1646  */
    break;

  case 732:
#line 5442 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			NODE *assocs = (yyvsp[-2].node);
			NODE *tail = (yyvsp[0].node);
			if (!assocs) {
			    assocs = tail;
			}
			else if (tail) {
                            if (assocs->nd_head &&
                                !tail->nd_head && nd_type(tail->nd_next) == NODE_LIST &&
                                nd_type(tail->nd_next->nd_head) == NODE_HASH) {
                                /* DSTAR */
                                tail = tail->nd_next->nd_head->nd_head;
                            }
			    assocs = list_concat(assocs, tail);
			}
			(yyval.node) = assocs;
		    /*% %*/
		    /*% ripper: rb_ary_push($1, get_value($3)) %*/
		    }
#line 12050 "parse.c" /* yacc.c:1646  */
    break;

  case 733:
#line 5465 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			if (nd_type((yyvsp[-2].node)) == NODE_STR) {
			    nd_set_type((yyvsp[-2].node), NODE_LIT);
			    RB_OBJ_WRITE(p->ast, &(yyvsp[-2].node)->nd_lit, rb_fstring((yyvsp[-2].node)->nd_lit));
			}
			(yyval.node) = list_append(p, NEW_LIST((yyvsp[-2].node), &(yyloc)), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: assoc_new!($1, $3) %*/
		    }
#line 12065 "parse.c" /* yacc.c:1646  */
    break;

  case 734:
#line 5476 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			(yyval.node) = list_append(p, NEW_LIST(NEW_LIT(ID2SYM((yyvsp[-1].id)), &(yylsp[-1])), &(yyloc)), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: assoc_new!($1, $2) %*/
		    }
#line 12076 "parse.c" /* yacc.c:1646  */
    break;

  case 735:
#line 5483 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
			YYLTYPE loc = code_loc_gen(&(yylsp[-3]), &(yylsp[-1]));
			(yyval.node) = list_append(p, NEW_LIST(dsym_node(p, (yyvsp[-2].node), &loc), &loc), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: assoc_new!(dyna_symbol!($2), $4) %*/
		    }
#line 12088 "parse.c" /* yacc.c:1646  */
    break;

  case 736:
#line 5491 "parse.y" /* yacc.c:1646  */
    {
		    /*%%%*/
                        if (nd_type((yyvsp[0].node)) == NODE_HASH &&
                            !((yyvsp[0].node)->nd_head && (yyvsp[0].node)->nd_head->nd_alen)) {
                            static VALUE empty_hash;
                            if (!empty_hash) {
                                empty_hash = rb_obj_freeze(rb_hash_new());
                                rb_gc_register_mark_object(empty_hash);
                            }
                            (yyval.node) = list_append(p, NEW_LIST(0, &(yyloc)), NEW_LIT(empty_hash, &(yyloc)));
                        }
                        else
                            (yyval.node) = list_append(p, NEW_LIST(0, &(yyloc)), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: assoc_splat!($2) %*/
		    }
#line 12109 "parse.c" /* yacc.c:1646  */
    break;

  case 763:
#line 5559 "parse.y" /* yacc.c:1646  */
    {yyerrok;token_flush(p);}
#line 12115 "parse.c" /* yacc.c:1646  */
    break;

  case 764:
#line 5560 "parse.y" /* yacc.c:1646  */
    {token_flush(p);}
#line 12121 "parse.c" /* yacc.c:1646  */
    break;

  case 766:
#line 5564 "parse.y" /* yacc.c:1646  */
    {yyerrok;}
#line 12127 "parse.c" /* yacc.c:1646  */
    break;

  case 767:
#line 5568 "parse.y" /* yacc.c:1646  */
    {
			(yyval.node) = Qnull;
		    }
#line 12135 "parse.c" /* yacc.c:1646  */
    break;


#line 12139 "parse.c" /* yacc.c:1646  */
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
  *++yylsp = yyloc;

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
      yyerror (&yylloc, p, YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (p, &yymsg_alloc, &yymsg, \
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
        yyerror (&yylloc, p, yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }

  yyerror_range[1] = yylloc;

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
                      yytoken, &yylval, &yylloc, p);
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

  yyerror_range[1] = yylsp[1-yylen];
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

      yyerror_range[1] = *yylsp;
      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp, yylsp, p);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  yyerror_range[2] = yylloc;
  /* Using YYLLOC is tempting, but would change the location of
     the lookahead.  YYLOC is available though.  */
  YYLLOC_DEFAULT (yyloc, yyerror_range, 2);
  *++yylsp = yyloc;

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
  yyerror (&yylloc, p, YY_("memory exhausted"));
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
                  yytoken, &yylval, &yylloc, p);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp, yylsp, p);
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
#line 5572 "parse.y" /* yacc.c:1906  */

# undef p
# undef yylex
# undef yylval
# define yylval  (*p->lval)

static int regx_options(struct parser_params*);
static int tokadd_string(struct parser_params*,int,int,int,long*,rb_encoding**,rb_encoding**);
static void tokaddmbc(struct parser_params *p, int c, rb_encoding *enc);
static enum yytokentype parse_string(struct parser_params*,rb_strterm_literal_t*);
static enum yytokentype here_document(struct parser_params*,rb_strterm_heredoc_t*);

#ifndef RIPPER
# define set_yylval_node(x) {				\
  YYLTYPE _cur_loc;					\
  rb_parser_set_location(p, &_cur_loc);			\
  yylval.node = (x);					\
}
# define set_yylval_str(x) \
do { \
  set_yylval_node(NEW_STR(x, &_cur_loc)); \
  RB_OBJ_WRITTEN(p->ast, Qnil, x); \
} while(0)
# define set_yylval_literal(x) \
do { \
  set_yylval_node(NEW_LIT(x, &_cur_loc)); \
  RB_OBJ_WRITTEN(p->ast, Qnil, x); \
} while(0)
# define set_yylval_num(x) (yylval.num = (x))
# define set_yylval_id(x)  (yylval.id = (x))
# define set_yylval_name(x)  (yylval.id = (x))
# define yylval_id() (yylval.id)
#else
static inline VALUE
ripper_yylval_id(struct parser_params *p, ID x)
{
    return ripper_new_yylval(p, x, ID2SYM(x), 0);
}
# define set_yylval_str(x) (yylval.val = add_mark_object(p, (x)))
# define set_yylval_num(x) (yylval.val = ripper_new_yylval(p, (x), 0, 0))
# define set_yylval_id(x)  (void)(x)
# define set_yylval_name(x) (void)(yylval.val = ripper_yylval_id(p, x))
# define set_yylval_literal(x) add_mark_object(p, (x))
# define set_yylval_node(x) (void)(x)
# define yylval_id() yylval.id
# define _cur_loc NULL_LOC /* dummy */
#endif

#define set_yylval_noname() set_yylval_id(keyword_nil)

#ifndef RIPPER
#define literal_flush(p, ptr) ((p)->lex.ptok = (ptr))
#define dispatch_scan_event(p, t) ((void)0)
#define dispatch_delayed_token(p, t) ((void)0)
#define has_delayed_token(p) (0)
#else
#define literal_flush(p, ptr) ((void)(ptr))

#define yylval_rval (*(RB_TYPE_P(yylval.val, T_NODE) ? &yylval.node->nd_rval : &yylval.val))

static inline VALUE
intern_sym(const char *name)
{
    ID id = rb_intern_const(name);
    return ID2SYM(id);
}

static int
ripper_has_scan_event(struct parser_params *p)
{
    if (p->lex.pcur < p->lex.ptok) rb_raise(rb_eRuntimeError, "lex.pcur < lex.ptok");
    return p->lex.pcur > p->lex.ptok;
}

static VALUE
ripper_scan_event_val(struct parser_params *p, enum yytokentype t)
{
    VALUE str = STR_NEW(p->lex.ptok, p->lex.pcur - p->lex.ptok);
    VALUE rval = ripper_dispatch1(p, ripper_token2eventid(t), str);
    token_flush(p);
    return rval;
}

static void
ripper_dispatch_scan_event(struct parser_params *p, enum yytokentype t)
{
    if (!ripper_has_scan_event(p)) return;
    add_mark_object(p, yylval_rval = ripper_scan_event_val(p, t));
}
#define dispatch_scan_event(p, t) ripper_dispatch_scan_event(p, t)

static void
ripper_dispatch_delayed_token(struct parser_params *p, enum yytokentype t)
{
    int saved_line = p->ruby_sourceline;
    const char *saved_tokp = p->lex.ptok;

    if (NIL_P(p->delayed.token)) return;
    p->ruby_sourceline = p->delayed.line;
    p->lex.ptok = p->lex.pbeg + p->delayed.col;
    add_mark_object(p, yylval_rval = ripper_dispatch1(p, ripper_token2eventid(t), p->delayed.token));
    p->delayed.token = Qnil;
    p->ruby_sourceline = saved_line;
    p->lex.ptok = saved_tokp;
}
#define dispatch_delayed_token(p, t) ripper_dispatch_delayed_token(p, t)
#define has_delayed_token(p) (!NIL_P(p->delayed.token))
#endif /* RIPPER */

static inline int
is_identchar(const char *ptr, const char *MAYBE_UNUSED(ptr_end), rb_encoding *enc)
{
    return rb_enc_isalnum((unsigned char)*ptr, enc) || *ptr == '_' || !ISASCII(*ptr);
}

static inline int
parser_is_identchar(struct parser_params *p)
{
    return !(p)->eofp && is_identchar(p->lex.pcur-1, p->lex.pend, p->enc);
}

static inline int
parser_isascii(struct parser_params *p)
{
    return ISASCII(*(p->lex.pcur-1));
}

static void
token_info_setup(token_info *ptinfo, const char *ptr, const rb_code_location_t *loc)
{
    int column = 1, nonspc = 0, i;
    for (i = 0; i < loc->beg_pos.column; i++, ptr++) {
	if (*ptr == '\t') {
	    column = (((column - 1) / TAB_WIDTH) + 1) * TAB_WIDTH;
	}
	column++;
	if (*ptr != ' ' && *ptr != '\t') {
	    nonspc = 1;
	}
    }

    ptinfo->beg = loc->beg_pos;
    ptinfo->indent = column;
    ptinfo->nonspc = nonspc;
}

static void
token_info_push(struct parser_params *p, const char *token, const rb_code_location_t *loc)
{
    token_info *ptinfo;

    if (!p->token_info_enabled) return;
    ptinfo = ALLOC(token_info);
    ptinfo->token = token;
    ptinfo->next = p->token_info;
    token_info_setup(ptinfo, p->lex.pbeg, loc);

    p->token_info = ptinfo;
}

static void
token_info_pop(struct parser_params *p, const char *token, const rb_code_location_t *loc)
{
    token_info *ptinfo_beg = p->token_info;

    if (!ptinfo_beg) return;
    p->token_info = ptinfo_beg->next;

    /* indentation check of matched keywords (begin..end, if..end, etc.) */
    token_info_warn(p, token, ptinfo_beg, 1, loc);
    ruby_sized_xfree(ptinfo_beg, sizeof(*ptinfo_beg));
}

static void
token_info_drop(struct parser_params *p, const char *token, rb_code_position_t beg_pos)
{
    token_info *ptinfo_beg = p->token_info;

    if (!ptinfo_beg) return;
    p->token_info = ptinfo_beg->next;

    if (ptinfo_beg->beg.lineno != beg_pos.lineno ||
	ptinfo_beg->beg.column != beg_pos.column ||
	strcmp(ptinfo_beg->token, token)) {
	compile_error(p, "token position mismatch: %d:%d:%s expected but %d:%d:%s",
		      beg_pos.lineno, beg_pos.column, token,
		      ptinfo_beg->beg.lineno, ptinfo_beg->beg.column,
		      ptinfo_beg->token);
    }

    ruby_sized_xfree(ptinfo_beg, sizeof(*ptinfo_beg));
}

static void
token_info_warn(struct parser_params *p, const char *token, token_info *ptinfo_beg, int same, const rb_code_location_t *loc)
{
    token_info ptinfo_end_body, *ptinfo_end = &ptinfo_end_body;
    if (!p->token_info_enabled) return;
    if (!ptinfo_beg) return;
    token_info_setup(ptinfo_end, p->lex.pbeg, loc);
    if (ptinfo_beg->beg.lineno == ptinfo_end->beg.lineno) return; /* ignore one-line block */
    if (ptinfo_beg->nonspc || ptinfo_end->nonspc) return; /* ignore keyword in the middle of a line */
    if (ptinfo_beg->indent == ptinfo_end->indent) return; /* the indents are matched */
    if (!same && ptinfo_beg->indent < ptinfo_end->indent) return;
    rb_warn3L(ptinfo_end->beg.lineno,
	      "mismatched indentations at '%s' with '%s' at %d",
	      WARN_S(token), WARN_S(ptinfo_beg->token), WARN_I(ptinfo_beg->beg.lineno));
}

static int
parser_precise_mbclen(struct parser_params *p, const char *ptr)
{
    int len = rb_enc_precise_mbclen(ptr, p->lex.pend, p->enc);
    if (!MBCLEN_CHARFOUND_P(len)) {
	compile_error(p, "invalid multibyte char (%s)", rb_enc_name(p->enc));
	return -1;
    }
    return len;
}

#ifndef RIPPER
static void ruby_show_error_line(VALUE errbuf, const YYLTYPE *yylloc, int lineno, VALUE str);

static inline void
parser_show_error_line(struct parser_params *p, const YYLTYPE *yylloc)
{
    VALUE str;
    int lineno = p->ruby_sourceline;
    if (!yylloc) {
	return;
    }
    else if (yylloc->beg_pos.lineno == lineno) {
	str = p->lex.lastline;
    }
    else {
	return;
    }
    ruby_show_error_line(p->error_buffer, yylloc, lineno, str);
}

static int
parser_yyerror(struct parser_params *p, const YYLTYPE *yylloc, const char *msg)
{
    YYLTYPE current;

    if (!yylloc) {
	yylloc = RUBY_SET_YYLLOC(current);
    }
    else if ((p->ruby_sourceline != yylloc->beg_pos.lineno &&
	      p->ruby_sourceline != yylloc->end_pos.lineno) ||
	     (yylloc->beg_pos.lineno == yylloc->end_pos.lineno &&
	      yylloc->beg_pos.column == yylloc->end_pos.column)) {
	yylloc = 0;
    }
    compile_error(p, "%s", msg);
    parser_show_error_line(p, yylloc);
    return 0;
}

static void
ruby_show_error_line(VALUE errbuf, const YYLTYPE *yylloc, int lineno, VALUE str)
{
    VALUE mesg;
    const int max_line_margin = 30;
    const char *ptr, *ptr_end, *pt, *pb;
    const char *pre = "", *post = "", *pend;
    const char *code = "", *caret = "";
    const char *lim;
    const char *const pbeg = RSTRING_PTR(str);
    char *buf;
    long len;
    int i;

    if (!yylloc) return;
    pend = RSTRING_END(str);
    if (pend > pbeg && pend[-1] == '\n') {
	if (--pend > pbeg && pend[-1] == '\r') --pend;
    }

    pt = pend;
    if (lineno == yylloc->end_pos.lineno &&
	(pend - pbeg) > yylloc->end_pos.column) {
	pt = pbeg + yylloc->end_pos.column;
    }

    ptr = ptr_end = pt;
    lim = ptr - pbeg > max_line_margin ? ptr - max_line_margin : pbeg;
    while ((lim < ptr) && (*(ptr-1) != '\n')) ptr--;

    lim = pend - ptr_end > max_line_margin ? ptr_end + max_line_margin : pend;
    while ((ptr_end < lim) && (*ptr_end != '\n') && (*ptr_end != '\r')) ptr_end++;

    len = ptr_end - ptr;
    if (len > 4) {
	if (ptr > pbeg) {
	    ptr = rb_enc_prev_char(pbeg, ptr, pt, rb_enc_get(str));
	    if (ptr > pbeg) pre = "...";
	}
	if (ptr_end < pend) {
	    ptr_end = rb_enc_prev_char(pt, ptr_end, pend, rb_enc_get(str));
	    if (ptr_end < pend) post = "...";
	}
    }
    pb = pbeg;
    if (lineno == yylloc->beg_pos.lineno) {
	pb += yylloc->beg_pos.column;
	if (pb > pt) pb = pt;
    }
    if (pb < ptr) pb = ptr;
    if (len <= 4 && yylloc->beg_pos.lineno == yylloc->end_pos.lineno) {
	return;
    }
    if (RTEST(errbuf)) {
	mesg = rb_attr_get(errbuf, idMesg);
	if (RSTRING_LEN(mesg) > 0 && *(RSTRING_END(mesg)-1) != '\n')
	    rb_str_cat_cstr(mesg, "\n");
    }
    else {
	mesg = rb_enc_str_new(0, 0, rb_enc_get(str));
    }
    if (!errbuf && rb_stderr_tty_p()) {
#define CSI_BEGIN "\033["
#define CSI_SGR "m"
	rb_str_catf(mesg,
		    CSI_BEGIN""CSI_SGR"%s" /* pre */
		    CSI_BEGIN"1"CSI_SGR"%.*s"
		    CSI_BEGIN"1;4"CSI_SGR"%.*s"
		    CSI_BEGIN";1"CSI_SGR"%.*s"
		    CSI_BEGIN""CSI_SGR"%s" /* post */
		    "\n",
		    pre,
		    (int)(pb - ptr), ptr,
		    (int)(pt - pb), pb,
		    (int)(ptr_end - pt), pt,
		    post);
    }
    else {
	char *p2;

	len = ptr_end - ptr;
	lim = pt < pend ? pt : pend;
	i = (int)(lim - ptr);
	buf = ALLOCA_N(char, i+2);
	code = ptr;
	caret = p2 = buf;
	if (ptr <= pb) {
	    while (ptr < pb) {
		*p2++ = *ptr++ == '\t' ? '\t' : ' ';
	    }
	    *p2++ = '^';
	    ptr++;
	}
	if (lim > ptr) {
	    memset(p2, '~', (lim - ptr));
	    p2 += (lim - ptr);
	}
	*p2 = '\0';
	rb_str_catf(mesg, "%s%.*s%s\n""%s%s\n",
		    pre, (int)len, code, post,
		    pre, caret);
    }
    if (!errbuf) rb_write_error_str(mesg);
}
#else
static int
parser_yyerror(struct parser_params *p, const YYLTYPE *yylloc, const char *msg)
{
    const char *pcur = 0, *ptok = 0;
    if (yylloc &&
	p->ruby_sourceline == yylloc->beg_pos.lineno &&
	p->ruby_sourceline == yylloc->end_pos.lineno) {
	pcur = p->lex.pcur;
	ptok = p->lex.ptok;
	p->lex.ptok = p->lex.pbeg + yylloc->beg_pos.column;
	p->lex.pcur = p->lex.pbeg + yylloc->end_pos.column;
    }
    dispatch1(parse_error, STR_NEW2(msg));
    ripper_error(p);
    if (pcur) {
	p->lex.ptok = ptok;
	p->lex.pcur = pcur;
    }
    return 0;
}

static inline void
parser_show_error_line(struct parser_params *p, const YYLTYPE *yylloc)
{
}
#endif /* !RIPPER */

#ifndef RIPPER
static int
vtable_size(const struct vtable *tbl)
{
    if (!DVARS_TERMINAL_P(tbl)) {
	return tbl->pos;
    }
    else {
	return 0;
    }
}
#endif

static struct vtable *
vtable_alloc_gen(struct parser_params *p, int line, struct vtable *prev)
{
    struct vtable *tbl = ALLOC(struct vtable);
    tbl->pos = 0;
    tbl->capa = 8;
    tbl->tbl = ALLOC_N(ID, tbl->capa);
    tbl->prev = prev;
#ifndef RIPPER
    if (p->debug) {
	rb_parser_printf(p, "vtable_alloc:%d: %p\n", line, (void *)tbl);
    }
#endif
    return tbl;
}
#define vtable_alloc(prev) vtable_alloc_gen(p, __LINE__, prev)

static void
vtable_free_gen(struct parser_params *p, int line, const char *name,
		struct vtable *tbl)
{
#ifndef RIPPER
    if (p->debug) {
	rb_parser_printf(p, "vtable_free:%d: %s(%p)\n", line, name, (void *)tbl);
    }
#endif
    if (!DVARS_TERMINAL_P(tbl)) {
	if (tbl->tbl) {
	    ruby_sized_xfree(tbl->tbl, tbl->capa * sizeof(ID));
	}
	ruby_sized_xfree(tbl, sizeof(*tbl));
    }
}
#define vtable_free(tbl) vtable_free_gen(p, __LINE__, #tbl, tbl)

static void
vtable_add_gen(struct parser_params *p, int line, const char *name,
	       struct vtable *tbl, ID id)
{
#ifndef RIPPER
    if (p->debug) {
	rb_parser_printf(p, "vtable_add:%d: %s(%p), %s\n",
			 line, name, (void *)tbl, rb_id2name(id));
    }
#endif
    if (DVARS_TERMINAL_P(tbl)) {
	rb_parser_fatal(p, "vtable_add: vtable is not allocated (%p)", (void *)tbl);
	return;
    }
    if (tbl->pos == tbl->capa) {
	tbl->capa = tbl->capa * 2;
	SIZED_REALLOC_N(tbl->tbl, ID, tbl->capa, tbl->pos);
    }
    tbl->tbl[tbl->pos++] = id;
}
#define vtable_add(tbl, id) vtable_add_gen(p, __LINE__, #tbl, tbl, id)

#ifndef RIPPER
static void
vtable_pop_gen(struct parser_params *p, int line, const char *name,
	       struct vtable *tbl, int n)
{
    if (p->debug) {
	rb_parser_printf(p, "vtable_pop:%d: %s(%p), %d\n",
			 line, name, (void *)tbl, n);
    }
    if (tbl->pos < n) {
	rb_parser_fatal(p, "vtable_pop: unreachable (%d < %d)", tbl->pos, n);
	return;
    }
    tbl->pos -= n;
}
#define vtable_pop(tbl, n) vtable_pop_gen(p, __LINE__, #tbl, tbl, n)
#endif

static int
vtable_included(const struct vtable * tbl, ID id)
{
    int i;

    if (!DVARS_TERMINAL_P(tbl)) {
	for (i = 0; i < tbl->pos; i++) {
	    if (tbl->tbl[i] == id) {
		return i+1;
	    }
	}
    }
    return 0;
}

static void parser_prepare(struct parser_params *p);

#ifndef RIPPER
static NODE *parser_append_options(struct parser_params *p, NODE *node);

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

static int
e_option_supplied(struct parser_params *p)
{
    return strcmp(p->ruby_sourcefile, "-e") == 0;
}

static VALUE
yycompile0(VALUE arg)
{
    int n;
    NODE *tree;
    struct parser_params *p = (struct parser_params *)arg;
    VALUE cov = Qfalse;

    if (!compile_for_eval && !NIL_P(p->ruby_sourcefile_string)) {
	p->debug_lines = debug_lines(p->ruby_sourcefile_string);
	if (p->debug_lines && p->ruby_sourceline > 0) {
	    VALUE str = rb_default_rs;
	    n = p->ruby_sourceline;
	    do {
		rb_ary_push(p->debug_lines, str);
	    } while (--n);
	}

	if (!e_option_supplied(p)) {
	    cov = Qtrue;
	}
    }

    parser_prepare(p);
#define RUBY_DTRACE_PARSE_HOOK(name) \
    if (RUBY_DTRACE_PARSE_##name##_ENABLED()) { \
	RUBY_DTRACE_PARSE_##name(p->ruby_sourcefile, p->ruby_sourceline); \
    }
    RUBY_DTRACE_PARSE_HOOK(BEGIN);
    n = yyparse(p);
    RUBY_DTRACE_PARSE_HOOK(END);
    p->debug_lines = 0;

    p->lex.strterm = 0;
    p->lex.pcur = p->lex.pbeg = p->lex.pend = 0;
    p->lex.prevline = p->lex.lastline = p->lex.nextline = 0;
    if (n || p->error_p) {
	VALUE mesg = p->error_buffer;
	if (!mesg) {
	    mesg = rb_class_new_instance(0, 0, rb_eSyntaxError);
	}
	rb_set_errinfo(mesg);
	return FALSE;
    }
    tree = p->eval_tree;
    if (!tree) {
	tree = NEW_NIL(&NULL_LOC);
    }
    else {
	VALUE opt = p->compile_option;
	NODE *prelude;
	NODE *body = parser_append_options(p, tree->nd_body);
	if (!opt) opt = rb_obj_hide(rb_ident_hash_new());
	rb_hash_aset(opt, rb_sym_intern_ascii_cstr("coverage_enabled"), cov);
	prelude = block_append(p, p->eval_tree_begin, body);
	tree->nd_body = prelude;
        RB_OBJ_WRITE(p->ast, &p->ast->body.compile_option, opt);
    }
    p->ast->body.root = tree;
    p->ast->body.line_count = p->line_count;
    return TRUE;
}

static rb_ast_t *
yycompile(VALUE vparser, struct parser_params *p, VALUE fname, int line)
{
    rb_ast_t *ast;
    if (NIL_P(fname)) {
	p->ruby_sourcefile_string = Qnil;
	p->ruby_sourcefile = "(none)";
    }
    else {
	p->ruby_sourcefile_string = rb_fstring(fname);
	p->ruby_sourcefile = StringValueCStr(fname);
    }
    p->ruby_sourceline = line - 1;

    p->ast = ast = rb_ast_new();
    rb_suppress_tracing(yycompile0, (VALUE)p);
    p->ast = 0;
    RB_GC_GUARD(vparser); /* prohibit tail call optimization */

    return ast;
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
lex_get_str(struct parser_params *p, VALUE s)
{
    char *beg, *end, *start;
    long len;

    beg = RSTRING_PTR(s);
    len = RSTRING_LEN(s);
    start = beg;
    if (p->lex.gets_.ptr) {
	if (len == p->lex.gets_.ptr) return Qnil;
	beg += p->lex.gets_.ptr;
	len -= p->lex.gets_.ptr;
    }
    end = memchr(beg, '\n', len);
    if (end) len = ++end - beg;
    p->lex.gets_.ptr += len;
    return rb_str_subseq(s, beg - start, len);
}

static VALUE
lex_getline(struct parser_params *p)
{
    VALUE line = (*p->lex.gets)(p, p->lex.input);
    if (NIL_P(line)) return line;
    must_be_ascii_compatible(line);
#ifndef RIPPER
    if (p->debug_lines) {
	rb_enc_associate(line, p->enc);
	rb_ary_push(p->debug_lines, line);
    }
#endif
    p->line_count++;
    return line;
}

static const rb_data_type_t parser_data_type;

#ifndef RIPPER
static rb_ast_t*
parser_compile_string(VALUE vparser, VALUE fname, VALUE s, int line)
{
    struct parser_params *p;

    TypedData_Get_Struct(vparser, struct parser_params, &parser_data_type, p);

    p->lex.gets = lex_get_str;
    p->lex.gets_.ptr = 0;
    p->lex.input = rb_str_new_frozen(s);
    p->lex.pbeg = p->lex.pcur = p->lex.pend = 0;

    return yycompile(vparser, p, fname, line);
}

rb_ast_t*
rb_parser_compile_string(VALUE vparser, const char *f, VALUE s, int line)
{
    return rb_parser_compile_string_path(vparser, rb_filesystem_str_new_cstr(f), s, line);
}

rb_ast_t*
rb_parser_compile_string_path(VALUE vparser, VALUE f, VALUE s, int line)
{
    must_be_ascii_compatible(s);
    return parser_compile_string(vparser, f, s, line);
}

VALUE rb_io_gets_internal(VALUE io);

static VALUE
lex_io_gets(struct parser_params *p, VALUE io)
{
    return rb_io_gets_internal(io);
}

rb_ast_t*
rb_parser_compile_file_path(VALUE vparser, VALUE fname, VALUE file, int start)
{
    struct parser_params *p;

    TypedData_Get_Struct(vparser, struct parser_params, &parser_data_type, p);

    p->lex.gets = lex_io_gets;
    p->lex.input = file;
    p->lex.pbeg = p->lex.pcur = p->lex.pend = 0;

    return yycompile(vparser, p, fname, start);
}

static VALUE
lex_generic_gets(struct parser_params *p, VALUE input)
{
    return (*p->lex.gets_.call)(input, p->line_count);
}

rb_ast_t*
rb_parser_compile_generic(VALUE vparser, VALUE (*lex_gets)(VALUE, int), VALUE fname, VALUE input, int start)
{
    struct parser_params *p;

    TypedData_Get_Struct(vparser, struct parser_params, &parser_data_type, p);

    p->lex.gets = lex_generic_gets;
    p->lex.gets_.call = lex_gets;
    p->lex.input = input;
    p->lex.pbeg = p->lex.pcur = p->lex.pend = 0;

    return yycompile(vparser, p, fname, start);
}
#endif  /* !RIPPER */

#define STR_FUNC_ESCAPE 0x01
#define STR_FUNC_EXPAND 0x02
#define STR_FUNC_REGEXP 0x04
#define STR_FUNC_QWORDS 0x08
#define STR_FUNC_SYMBOL 0x10
#define STR_FUNC_INDENT 0x20
#define STR_FUNC_LABEL  0x40
#define STR_FUNC_LIST   0x4000
#define STR_FUNC_TERM   0x8000

enum string_type {
    str_label  = STR_FUNC_LABEL,
    str_squote = (0),
    str_dquote = (STR_FUNC_EXPAND),
    str_xquote = (STR_FUNC_EXPAND),
    str_regexp = (STR_FUNC_REGEXP|STR_FUNC_ESCAPE|STR_FUNC_EXPAND),
    str_sword  = (STR_FUNC_QWORDS|STR_FUNC_LIST),
    str_dword  = (STR_FUNC_QWORDS|STR_FUNC_EXPAND|STR_FUNC_LIST),
    str_ssym   = (STR_FUNC_SYMBOL),
    str_dsym   = (STR_FUNC_SYMBOL|STR_FUNC_EXPAND)
};

static VALUE
parser_str_new(const char *ptr, long len, rb_encoding *enc, int func, rb_encoding *enc0)
{
    VALUE str;

    str = rb_enc_str_new(ptr, len, enc);
    if (!(func & STR_FUNC_REGEXP) && rb_enc_asciicompat(enc)) {
	if (rb_enc_str_coderange(str) == ENC_CODERANGE_7BIT) {
	}
	else if (enc0 == rb_usascii_encoding() && enc != rb_utf8_encoding()) {
	    rb_enc_associate(str, rb_ascii8bit_encoding());
	}
    }

    return str;
}

#define lex_goto_eol(p) ((p)->lex.pcur = (p)->lex.pend)
#define lex_eol_p(p) ((p)->lex.pcur >= (p)->lex.pend)
#define lex_eol_n_p(p,n) ((p)->lex.pcur+(n) >= (p)->lex.pend)
#define peek(p,c) peek_n(p, (c), 0)
#define peek_n(p,c,n) (!lex_eol_n_p(p, n) && (c) == (unsigned char)(p)->lex.pcur[n])
#define peekc(p) peekc_n(p, 0)
#define peekc_n(p,n) (lex_eol_n_p(p, n) ? -1 : (unsigned char)(p)->lex.pcur[n])

#ifdef RIPPER
static void
add_delayed_token(struct parser_params *p, const char *tok, const char *end)
{
    if (tok < end) {
	if (!has_delayed_token(p)) {
	    p->delayed.token = rb_str_buf_new(end - tok);
	    rb_enc_associate(p->delayed.token, p->enc);
	    p->delayed.line = p->ruby_sourceline;
	    p->delayed.col = rb_long2int(tok - p->lex.pbeg);
	}
	rb_str_buf_cat(p->delayed.token, tok, end - tok);
	p->lex.ptok = end;
    }
}
#else
#define add_delayed_token(p, tok, end) ((void)(tok), (void)(end))
#endif

static int
nextline(struct parser_params *p)
{
    VALUE v = p->lex.nextline;
    p->lex.nextline = 0;
    if (!v) {
	if (p->eofp)
	    return -1;

	if (p->lex.pend > p->lex.pbeg && *(p->lex.pend-1) != '\n') {
	    goto end_of_input;
	}

	if (!p->lex.input || NIL_P(v = lex_getline(p))) {
	  end_of_input:
	    p->eofp = 1;
	    lex_goto_eol(p);
	    return -1;
	}
	p->cr_seen = FALSE;
    }
    else if (NIL_P(v)) {
	/* after here-document without terminator */
	goto end_of_input;
    }
    add_delayed_token(p, p->lex.ptok, p->lex.pend);
    if (p->heredoc_end > 0) {
	p->ruby_sourceline = p->heredoc_end;
	p->heredoc_end = 0;
    }
    p->ruby_sourceline++;
    p->lex.pbeg = p->lex.pcur = RSTRING_PTR(v);
    p->lex.pend = p->lex.pcur + RSTRING_LEN(v);
    token_flush(p);
    p->lex.prevline = p->lex.lastline;
    p->lex.lastline = v;
    return 0;
}

static int
parser_cr(struct parser_params *p, int c)
{
    if (peek(p, '\n')) {
	p->lex.pcur++;
	c = '\n';
    }
    return c;
}

static inline int
nextc(struct parser_params *p)
{
    int c;

    if (UNLIKELY((p->lex.pcur == p->lex.pend) || p->eofp || RTEST(p->lex.nextline))) {
	if (nextline(p)) return -1;
    }
    c = (unsigned char)*p->lex.pcur++;
    if (UNLIKELY(c == '\r')) {
	c = parser_cr(p, c);
    }

    return c;
}

static void
pushback(struct parser_params *p, int c)
{
    if (c == -1) return;
    p->lex.pcur--;
    if (p->lex.pcur > p->lex.pbeg && p->lex.pcur[0] == '\n' && p->lex.pcur[-1] == '\r') {
	p->lex.pcur--;
    }
}

#define was_bol(p) ((p)->lex.pcur == (p)->lex.pbeg + 1)

#define tokfix(p) ((p)->tokenbuf[(p)->tokidx]='\0')
#define tok(p) (p)->tokenbuf
#define toklen(p) (p)->tokidx

static int
looking_at_eol_p(struct parser_params *p)
{
    const char *ptr = p->lex.pcur;
    while (ptr < p->lex.pend) {
	int c = (unsigned char)*ptr++;
	int eol = (c == '\n' || c == '#');
	if (eol || !ISSPACE(c)) {
	    return eol;
	}
    }
    return TRUE;
}

static char*
newtok(struct parser_params *p)
{
    p->tokidx = 0;
    p->tokline = p->ruby_sourceline;
    if (!p->tokenbuf) {
	p->toksiz = 60;
	p->tokenbuf = ALLOC_N(char, 60);
    }
    if (p->toksiz > 4096) {
	p->toksiz = 60;
	REALLOC_N(p->tokenbuf, char, 60);
    }
    return p->tokenbuf;
}

static char *
tokspace(struct parser_params *p, int n)
{
    p->tokidx += n;

    if (p->tokidx >= p->toksiz) {
	do {p->toksiz *= 2;} while (p->toksiz < p->tokidx);
	REALLOC_N(p->tokenbuf, char, p->toksiz);
    }
    return &p->tokenbuf[p->tokidx-n];
}

static void
tokadd(struct parser_params *p, int c)
{
    p->tokenbuf[p->tokidx++] = (char)c;
    if (p->tokidx >= p->toksiz) {
	p->toksiz *= 2;
	REALLOC_N(p->tokenbuf, char, p->toksiz);
    }
}

static int
tok_hex(struct parser_params *p, size_t *numlen)
{
    int c;

    c = scan_hex(p->lex.pcur, 2, numlen);
    if (!*numlen) {
	yyerror0("invalid hex escape");
	token_flush(p);
	return 0;
    }
    p->lex.pcur += *numlen;
    return c;
}

#define tokcopy(p, n) memcpy(tokspace(p, n), (p)->lex.pcur - (n), (n))

static int
escaped_control_code(int c)
{
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
    return c2;
}

#define WARN_SPACE_CHAR(c, prefix) \
    rb_warn1("invalid character syntax; use "prefix"\\%c", WARN_I(c2))

static int
tokadd_codepoint(struct parser_params *p, rb_encoding **encp,
		 int regexp_literal, int wide)
{
    size_t numlen;
    int codepoint = scan_hex(p->lex.pcur, wide ? p->lex.pend - p->lex.pcur : 4, &numlen);
    literal_flush(p, p->lex.pcur);
    p->lex.pcur += numlen;
    if (wide ? (numlen == 0 || numlen > 6) : (numlen < 4))  {
	yyerror0("invalid Unicode escape");
	return wide && numlen > 0;
    }
    if (codepoint > 0x10ffff) {
	yyerror0("invalid Unicode codepoint (too large)");
	return wide;
    }
    if ((codepoint & 0xfffff800) == 0xd800) {
	yyerror0("invalid Unicode codepoint");
	return wide;
    }
    if (regexp_literal) {
	tokcopy(p, (int)numlen);
    }
    else if (codepoint >= 0x80) {
	rb_encoding *utf8 = rb_utf8_encoding();
	if (*encp && utf8 != *encp) {
	    YYLTYPE loc = RUBY_INIT_YYLLOC();
	    compile_error(p, "UTF-8 mixed within %s source", rb_enc_name(*encp));
	    parser_show_error_line(p, &loc);
	    return wide;
	}
	*encp = utf8;
	tokaddmbc(p, codepoint, *encp);
    }
    else {
	tokadd(p, codepoint);
    }
    return TRUE;
}

/* return value is for ?\u3042 */
static void
tokadd_utf8(struct parser_params *p, rb_encoding **encp,
	    int term, int symbol_literal, int regexp_literal)
{
    /*
     * If `term` is not -1, then we allow multiple codepoints in \u{}
     * upto `term` byte, otherwise we're parsing a character literal.
     * And then add the codepoints to the current token.
     */
    static const char multiple_codepoints[] = "Multiple codepoints at single character literal";

    const int open_brace = '{', close_brace = '}';

    if (regexp_literal) { tokadd(p, '\\'); tokadd(p, 'u'); }

    if (peek(p, open_brace)) {  /* handle \u{...} form */
	const char *second = NULL;
	int c, last = nextc(p);
	if (p->lex.pcur >= p->lex.pend) goto unterminated;
	while (ISSPACE(c = *p->lex.pcur) && ++p->lex.pcur < p->lex.pend);
	while (c != close_brace) {
	    if (c == term) goto unterminated;
	    if (second == multiple_codepoints)
		second = p->lex.pcur;
	    if (regexp_literal) tokadd(p, last);
	    if (!tokadd_codepoint(p, encp, regexp_literal, TRUE)) {
		break;
	    }
	    while (ISSPACE(c = *p->lex.pcur)) {
		if (++p->lex.pcur >= p->lex.pend) goto unterminated;
		last = c;
	    }
	    if (term == -1 && !second)
		second = multiple_codepoints;
	}

	if (c != close_brace) {
	  unterminated:
	    token_flush(p);
	    yyerror0("unterminated Unicode escape");
	    return;
	}
	if (second && second != multiple_codepoints) {
	    const char *pcur = p->lex.pcur;
	    p->lex.pcur = second;
	    dispatch_scan_event(p, tSTRING_CONTENT);
	    token_flush(p);
	    p->lex.pcur = pcur;
	    yyerror0(multiple_codepoints);
	    token_flush(p);
	}

	if (regexp_literal) tokadd(p, close_brace);
	nextc(p);
    }
    else {			/* handle \uxxxx form */
	if (!tokadd_codepoint(p, encp, regexp_literal, FALSE)) {
	    token_flush(p);
	    return;
	}
    }
}

#define ESCAPE_CONTROL 1
#define ESCAPE_META    2

static int
read_escape(struct parser_params *p, int flags, rb_encoding **encp)
{
    int c;
    size_t numlen;

    switch (c = nextc(p)) {
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
	pushback(p, c);
	c = scan_oct(p->lex.pcur, 3, &numlen);
	p->lex.pcur += numlen;
	return c;

      case 'x':	/* hex constant */
	c = tok_hex(p, &numlen);
	if (numlen == 0) return 0;
	return c;

      case 'b':	/* backspace */
	return '\010';

      case 's':	/* space */
	return ' ';

      case 'M':
	if (flags & ESCAPE_META) goto eof;
	if ((c = nextc(p)) != '-') {
	    goto eof;
	}
	if ((c = nextc(p)) == '\\') {
	    if (peek(p, 'u')) goto eof;
	    return read_escape(p, flags|ESCAPE_META, encp) | 0x80;
	}
	else if (c == -1 || !ISASCII(c)) goto eof;
	else {
	    int c2 = escaped_control_code(c);
	    if (c2) {
		if (ISCNTRL(c) || !(flags & ESCAPE_CONTROL)) {
		    WARN_SPACE_CHAR(c2, "\\M-");
		}
		else {
		    WARN_SPACE_CHAR(c2, "\\C-\\M-");
		}
	    }
	    else if (ISCNTRL(c)) goto eof;
	    return ((c & 0xff) | 0x80);
	}

      case 'C':
	if ((c = nextc(p)) != '-') {
	    goto eof;
	}
      case 'c':
	if (flags & ESCAPE_CONTROL) goto eof;
	if ((c = nextc(p))== '\\') {
	    if (peek(p, 'u')) goto eof;
	    c = read_escape(p, flags|ESCAPE_CONTROL, encp);
	}
	else if (c == '?')
	    return 0177;
	else if (c == -1 || !ISASCII(c)) goto eof;
	else {
	    int c2 = escaped_control_code(c);
	    if (c2) {
		if (ISCNTRL(c)) {
		    if (flags & ESCAPE_META) {
			WARN_SPACE_CHAR(c2, "\\M-");
		    }
		    else {
			WARN_SPACE_CHAR(c2, "");
		    }
		}
		else {
		    if (flags & ESCAPE_META) {
			WARN_SPACE_CHAR(c2, "\\M-\\C-");
		    }
		    else {
			WARN_SPACE_CHAR(c2, "\\C-");
		    }
		}
	    }
	    else if (ISCNTRL(c)) goto eof;
	}
	return c & 0x9f;

      eof:
      case -1:
        yyerror0("Invalid escape character syntax");
	token_flush(p);
	return '\0';

      default:
	return c;
    }
}

static void
tokaddmbc(struct parser_params *p, int c, rb_encoding *enc)
{
    int len = rb_enc_codelen(c, enc);
    rb_enc_mbcput(c, tokspace(p, len), enc);
}

static int
tokadd_escape(struct parser_params *p, rb_encoding **encp)
{
    int c;
    int flags = 0;
    size_t numlen;

  first:
    switch (c = nextc(p)) {
      case '\n':
	return 0;		/* just ignore */

      case '0': case '1': case '2': case '3': /* octal constant */
      case '4': case '5': case '6': case '7':
	{
	    ruby_scan_oct(--p->lex.pcur, 3, &numlen);
	    if (numlen == 0) goto eof;
	    p->lex.pcur += numlen;
	    tokcopy(p, (int)numlen + 1);
	}
	return 0;

      case 'x':	/* hex constant */
	{
	    tok_hex(p, &numlen);
	    if (numlen == 0) return -1;
	    tokcopy(p, (int)numlen + 2);
	}
	return 0;

      case 'M':
	if (flags & ESCAPE_META) goto eof;
	if ((c = nextc(p)) != '-') {
	    pushback(p, c);
	    goto eof;
	}
	tokcopy(p, 3);
	flags |= ESCAPE_META;
	goto escaped;

      case 'C':
	if (flags & ESCAPE_CONTROL) goto eof;
	if ((c = nextc(p)) != '-') {
	    pushback(p, c);
	    goto eof;
	}
	tokcopy(p, 3);
	goto escaped;

      case 'c':
	if (flags & ESCAPE_CONTROL) goto eof;
	tokcopy(p, 2);
	flags |= ESCAPE_CONTROL;
      escaped:
	if ((c = nextc(p)) == '\\') {
	    goto first;
	}
	else if (c == -1) goto eof;
	tokadd(p, c);
	return 0;

      eof:
      case -1:
        yyerror0("Invalid escape character syntax");
	token_flush(p);
	return -1;

      default:
	tokadd(p, '\\');
	tokadd(p, c);
    }
    return 0;
}

static int
regx_options(struct parser_params *p)
{
    int kcode = 0;
    int kopt = 0;
    int options = 0;
    int c, opt, kc;

    newtok(p);
    while (c = nextc(p), ISALPHA(c)) {
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
	    tokadd(p, c);
        }
    }
    options |= kopt;
    pushback(p, c);
    if (toklen(p)) {
	YYLTYPE loc = RUBY_INIT_YYLLOC();
	tokfix(p);
	compile_error(p, "unknown regexp option%s - %*s",
		      toklen(p) > 1 ? "s" : "", toklen(p), tok(p));
	parser_show_error_line(p, &loc);
    }
    return options | RE_OPTION_ENCODING(kcode);
}

static int
tokadd_mbchar(struct parser_params *p, int c)
{
    int len = parser_precise_mbclen(p, p->lex.pcur-1);
    if (len < 0) return -1;
    tokadd(p, c);
    p->lex.pcur += --len;
    if (len > 0) tokcopy(p, len);
    return c;
}

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
parser_update_heredoc_indent(struct parser_params *p, int c)
{
    if (p->heredoc_line_indent == -1) {
	if (c == '\n') p->heredoc_line_indent = 0;
    }
    else {
	if (c == ' ') {
	    p->heredoc_line_indent++;
	    return TRUE;
	}
	else if (c == '\t') {
	    int w = (p->heredoc_line_indent / TAB_WIDTH) + 1;
	    p->heredoc_line_indent = w * TAB_WIDTH;
	    return TRUE;
	}
	else if (c != '\n') {
	    if (p->heredoc_indent > p->heredoc_line_indent) {
		p->heredoc_indent = p->heredoc_line_indent;
	    }
	    p->heredoc_line_indent = -1;
	}
    }
    return FALSE;
}

static void
parser_mixed_error(struct parser_params *p, rb_encoding *enc1, rb_encoding *enc2)
{
    YYLTYPE loc = RUBY_INIT_YYLLOC();
    const char *n1 = rb_enc_name(enc1), *n2 = rb_enc_name(enc2);
    compile_error(p, "%s mixed within %s source", n1, n2);
    parser_show_error_line(p, &loc);
}

static void
parser_mixed_escape(struct parser_params *p, const char *beg, rb_encoding *enc1, rb_encoding *enc2)
{
    const char *pos = p->lex.pcur;
    p->lex.pcur = beg;
    parser_mixed_error(p, enc1, enc2);
    p->lex.pcur = pos;
}

static int
tokadd_string(struct parser_params *p,
	      int func, int term, int paren, long *nest,
	      rb_encoding **encp, rb_encoding **enc)
{
    int c;
    bool erred = false;

#define mixed_error(enc1, enc2) \
    (void)(erred || (parser_mixed_error(p, enc1, enc2), erred = true))
#define mixed_escape(beg, enc1, enc2) \
    (void)(erred || (parser_mixed_escape(p, beg, enc1, enc2), erred = true))

    while ((c = nextc(p)) != -1) {
	if (p->heredoc_indent > 0) {
	    parser_update_heredoc_indent(p, c);
	}

	if (paren && c == paren) {
	    ++*nest;
	}
	else if (c == term) {
	    if (!nest || !*nest) {
		pushback(p, c);
		break;
	    }
	    --*nest;
	}
	else if ((func & STR_FUNC_EXPAND) && c == '#' && p->lex.pcur < p->lex.pend) {
	    int c2 = *p->lex.pcur;
	    if (c2 == '$' || c2 == '@' || c2 == '{') {
		pushback(p, c);
		break;
	    }
	}
	else if (c == '\\') {
	    literal_flush(p, p->lex.pcur - 1);
	    c = nextc(p);
	    switch (c) {
	      case '\n':
		if (func & STR_FUNC_QWORDS) break;
		if (func & STR_FUNC_EXPAND) {
		    if (!(func & STR_FUNC_INDENT) || (p->heredoc_indent < 0))
			continue;
		    if (c == term) {
			c = '\\';
			goto terminate;
		    }
		}
		tokadd(p, '\\');
		break;

	      case '\\':
		if (func & STR_FUNC_ESCAPE) tokadd(p, c);
		break;

	      case 'u':
		if ((func & STR_FUNC_EXPAND) == 0) {
		    tokadd(p, '\\');
		    break;
		}
		tokadd_utf8(p, enc, term,
			    func & STR_FUNC_SYMBOL,
			    func & STR_FUNC_REGEXP);
		continue;

	      default:
		if (c == -1) return -1;
		if (!ISASCII(c)) {
		    if ((func & STR_FUNC_EXPAND) == 0) tokadd(p, '\\');
		    goto non_ascii;
		}
		if (func & STR_FUNC_REGEXP) {
		    if (c == term && !simple_re_meta(c)) {
			tokadd(p, c);
			continue;
		    }
		    pushback(p, c);
		    if ((c = tokadd_escape(p, enc)) < 0)
			return -1;
		    if (*enc && *enc != *encp) {
			mixed_escape(p->lex.ptok+2, *enc, *encp);
		    }
		    continue;
		}
		else if (func & STR_FUNC_EXPAND) {
		    pushback(p, c);
		    if (func & STR_FUNC_ESCAPE) tokadd(p, '\\');
		    c = read_escape(p, 0, enc);
		}
		else if ((func & STR_FUNC_QWORDS) && ISSPACE(c)) {
		    /* ignore backslashed spaces in %w */
		}
		else if (c != term && !(paren && c == paren)) {
		    tokadd(p, '\\');
		    pushback(p, c);
		    continue;
		}
	    }
	}
	else if (!parser_isascii(p)) {
	  non_ascii:
	    if (!*enc) {
		*enc = *encp;
	    }
	    else if (*enc != *encp) {
		mixed_error(*enc, *encp);
		continue;
	    }
	    if (tokadd_mbchar(p, c) == -1) return -1;
	    continue;
	}
	else if ((func & STR_FUNC_QWORDS) && ISSPACE(c)) {
	    pushback(p, c);
	    break;
	}
        if (c & 0x80) {
	    if (!*enc) {
		*enc = *encp;
	    }
	    else if (*enc != *encp) {
		mixed_error(*enc, *encp);
		continue;
	    }
        }
	tokadd(p, c);
    }
  terminate:
    if (*enc) *encp = *enc;
    return c;
}

static inline rb_strterm_t *
new_strterm(VALUE v1, VALUE v2, VALUE v3, VALUE v0)
{
    return (rb_strterm_t*)rb_imemo_new(imemo_parser_strterm, v1, v2, v3, v0);
}

/* imemo_parser_strterm for literal */
#define NEW_STRTERM(func, term, paren) \
    new_strterm((VALUE)(func), (VALUE)(paren), (VALUE)(term), 0)

#ifdef RIPPER
static void
flush_string_content(struct parser_params *p, rb_encoding *enc)
{
    VALUE content = yylval.val;
    if (!ripper_is_node_yylval(content))
	content = ripper_new_yylval(p, 0, 0, content);
    if (has_delayed_token(p)) {
	ptrdiff_t len = p->lex.pcur - p->lex.ptok;
	if (len > 0) {
	    rb_enc_str_buf_cat(p->delayed.token, p->lex.ptok, len, enc);
	}
	dispatch_delayed_token(p, tSTRING_CONTENT);
	p->lex.ptok = p->lex.pcur;
	RNODE(content)->nd_rval = yylval.val;
    }
    dispatch_scan_event(p, tSTRING_CONTENT);
    if (yylval.val != content)
	RNODE(content)->nd_rval = yylval.val;
    yylval.val = content;
}
#else
#define flush_string_content(p, enc) ((void)(enc))
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

static enum yytokentype
parser_peek_variable_name(struct parser_params *p)
{
    int c;
    const char *ptr = p->lex.pcur;

    if (ptr + 1 >= p->lex.pend) return 0;
    c = *ptr++;
    switch (c) {
      case '$':
	if ((c = *ptr) == '-') {
	    if (++ptr >= p->lex.pend) return 0;
	    c = *ptr;
	}
	else if (is_global_name_punct(c) || ISDIGIT(c)) {
	    return tSTRING_DVAR;
	}
	break;
      case '@':
	if ((c = *ptr) == '@') {
	    if (++ptr >= p->lex.pend) return 0;
	    c = *ptr;
	}
	break;
      case '{':
	p->lex.pcur = ptr;
	p->command_start = TRUE;
	return tSTRING_DBEG;
      default:
	return 0;
    }
    if (!ISASCII(c) || c == '_' || ISALPHA(c))
	return tSTRING_DVAR;
    return 0;
}

#define IS_ARG() IS_lex_state(EXPR_ARG_ANY)
#define IS_END() IS_lex_state(EXPR_END_ANY)
#define IS_BEG() (IS_lex_state(EXPR_BEG_ANY) || IS_lex_state_all(EXPR_ARG|EXPR_LABELED))
#define IS_SPCARG(c) (IS_ARG() && space_seen && !ISSPACE(c))
#define IS_LABEL_POSSIBLE() (\
	(IS_lex_state(EXPR_LABEL|EXPR_ENDFN) && !cmd_state) || \
	IS_ARG())
#define IS_LABEL_SUFFIX(n) (peek_n(p, ':',(n)) && !peek_n(p, ':', (n)+1))
#define IS_AFTER_OPERATOR() IS_lex_state(EXPR_FNAME | EXPR_DOT)

static inline enum yytokentype
parser_string_term(struct parser_params *p, int func)
{
    p->lex.strterm = 0;
    if (func & STR_FUNC_REGEXP) {
	set_yylval_num(regx_options(p));
	dispatch_scan_event(p, tREGEXP_END);
	SET_LEX_STATE(EXPR_END);
	return tREGEXP_END;
    }
    if ((func & STR_FUNC_LABEL) && IS_LABEL_SUFFIX(0)) {
	nextc(p);
	SET_LEX_STATE(EXPR_BEG|EXPR_LABEL);
	return tLABEL_END;
    }
    SET_LEX_STATE(EXPR_END);
    return tSTRING_END;
}

static enum yytokentype
parse_string(struct parser_params *p, rb_strterm_literal_t *quote)
{
    int func = (int)quote->u1.func;
    int term = (int)quote->u3.term;
    int paren = (int)quote->u2.paren;
    int c, space = 0;
    rb_encoding *enc = p->enc;
    rb_encoding *base_enc = 0;
    VALUE lit;

    if (func & STR_FUNC_TERM) {
	if (func & STR_FUNC_QWORDS) nextc(p); /* delayed term */
	SET_LEX_STATE(EXPR_END);
	p->lex.strterm = 0;
	return func & STR_FUNC_REGEXP ? tREGEXP_END : tSTRING_END;
    }
    c = nextc(p);
    if ((func & STR_FUNC_QWORDS) && ISSPACE(c)) {
	do {c = nextc(p);} while (ISSPACE(c));
	space = 1;
    }
    if (func & STR_FUNC_LIST) {
	quote->u1.func &= ~STR_FUNC_LIST;
	space = 1;
    }
    if (c == term && !quote->u0.nest) {
	if (func & STR_FUNC_QWORDS) {
	    quote->u1.func |= STR_FUNC_TERM;
	    pushback(p, c); /* dispatch the term at tSTRING_END */
	    add_delayed_token(p, p->lex.ptok, p->lex.pcur);
	    return ' ';
	}
	return parser_string_term(p, func);
    }
    if (space) {
	pushback(p, c);
	add_delayed_token(p, p->lex.ptok, p->lex.pcur);
	return ' ';
    }
    newtok(p);
    if ((func & STR_FUNC_EXPAND) && c == '#') {
	int t = parser_peek_variable_name(p);
	if (t) return t;
	tokadd(p, '#');
	c = nextc(p);
    }
    pushback(p, c);
    if (tokadd_string(p, func, term, paren, &quote->u0.nest,
		      &enc, &base_enc) == -1) {
	if (p->eofp) {
#ifndef RIPPER
# define unterminated_literal(mesg) yyerror0(mesg)
#else
# define unterminated_literal(mesg) compile_error(p,  mesg)
#endif
	    literal_flush(p, p->lex.pcur);
	    if (func & STR_FUNC_QWORDS) {
		/* no content to add, bailing out here */
		unterminated_literal("unterminated list meets end of file");
		p->lex.strterm = 0;
		return tSTRING_END;
	    }
	    if (func & STR_FUNC_REGEXP) {
		unterminated_literal("unterminated regexp meets end of file");
	    }
	    else {
		unterminated_literal("unterminated string meets end of file");
	    }
	    quote->u1.func |= STR_FUNC_TERM;
	}
    }

    tokfix(p);
    lit = STR_NEW3(tok(p), toklen(p), enc, func);
    set_yylval_str(lit);
    flush_string_content(p, enc);

    return tSTRING_CONTENT;
}

static enum yytokentype
heredoc_identifier(struct parser_params *p)
{
    /*
     * term_len is length of `<<"END"` except `END`,
     * in this case term_len is 4 (<, <, " and ").
     */
    long len, offset = p->lex.pcur - p->lex.pbeg;
    int c = nextc(p), term, func = 0, quote = 0;
    enum yytokentype token = tSTRING_BEG;
    int indent = 0;

    if (c == '-') {
	c = nextc(p);
	func = STR_FUNC_INDENT;
	offset++;
    }
    else if (c == '~') {
	c = nextc(p);
	func = STR_FUNC_INDENT;
	offset++;
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
	quote++;
	offset++;
	term = c;
	len = 0;
	while ((c = nextc(p)) != term) {
	    if (c == -1 || c == '\r' || c == '\n') {
		yyerror(NULL, p, "unterminated here document identifier");
		return -1;
	    }
	}
	break;

      default:
	if (!parser_is_identchar(p)) {
	    pushback(p, c);
	    if (func & STR_FUNC_INDENT) {
		pushback(p, indent > 0 ? '~' : '-');
	    }
	    return 0;
	}
	func |= str_dquote;
	do {
	    int n = parser_precise_mbclen(p, p->lex.pcur-1);
	    if (n < 0) return 0;
	    p->lex.pcur += --n;
	} while ((c = nextc(p)) != -1 && parser_is_identchar(p));
	pushback(p, c);
	break;
    }

    len = p->lex.pcur - (p->lex.pbeg + offset) - quote;
    if ((unsigned long)len >= HERETERM_LENGTH_MAX)
	yyerror(NULL, p, "too long here document identifier");
    dispatch_scan_event(p, tHEREDOC_BEG);
    lex_goto_eol(p);

    p->lex.strterm = new_strterm(0, 0, 0, p->lex.lastline);
    p->lex.strterm->flags |= STRTERM_HEREDOC;
    rb_strterm_heredoc_t *here = &p->lex.strterm->u.heredoc;
    here->offset = offset;
    here->sourceline = p->ruby_sourceline;
    here->length = (int)len;
    here->quote = quote;
    here->func = func;

    token_flush(p);
    p->heredoc_indent = indent;
    p->heredoc_line_indent = 0;
    return token;
}

static void
heredoc_restore(struct parser_params *p, rb_strterm_heredoc_t *here)
{
    VALUE line;

    p->lex.strterm = 0;
    line = here->lastline;
    p->lex.lastline = line;
    p->lex.pbeg = RSTRING_PTR(line);
    p->lex.pend = p->lex.pbeg + RSTRING_LEN(line);
    p->lex.pcur = p->lex.pbeg + here->offset + here->length + here->quote;
    p->lex.ptok = p->lex.pbeg + here->offset - here->quote;
    p->heredoc_end = p->ruby_sourceline;
    p->ruby_sourceline = (int)here->sourceline;
    if (p->eofp) p->lex.nextline = Qnil;
    p->eofp = 0;
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
heredoc_dedent(struct parser_params *p, NODE *root)
{
    NODE *node, *str_node, *prev_node;
    int indent = p->heredoc_indent;
    VALUE prev_lit = 0;

    if (indent <= 0) return root;
    p->heredoc_indent = 0;
    if (!root) return root;

    prev_node = node = str_node = root;
    if (nd_type(root) == NODE_LIST) str_node = root->nd_head;

    while (str_node) {
	VALUE lit = str_node->nd_lit;
	if (str_node->flags & NODE_FL_NEWLINE) {
	    dedent_string(lit, indent);
	}
	if (!prev_lit) {
	    prev_lit = lit;
	}
	else if (!literal_concat0(p, prev_lit, lit)) {
	    return 0;
	}
	else {
	    NODE *end = node->nd_end;
	    node = prev_node->nd_next = node->nd_next;
	    if (!node) {
		if (nd_type(prev_node) == NODE_DSTR)
		    nd_set_type(prev_node, NODE_STR);
		break;
	    }
	    node->nd_end = end;
	    goto next_str;
	}

	str_node = 0;
	while ((node = (prev_node = node)->nd_next) != 0) {
	  next_str:
	    if (nd_type(node) != NODE_LIST) break;
	    if ((str_node = node->nd_head) != 0) {
		enum node_type type = nd_type(str_node);
		if (type == NODE_STR || type == NODE_DSTR) break;
		prev_lit = 0;
		str_node = 0;
	    }
	}
    }
    return root;
}
#else /* RIPPER */
static VALUE
heredoc_dedent(struct parser_params *p, VALUE array)
{
    int indent = p->heredoc_indent;

    if (indent <= 0) return array;
    p->heredoc_indent = 0;
    dispatch2(heredoc_dedent, array, INT2NUM(indent));
    return array;
}

/*
 *  call-seq:
 *    Ripper.dedent_string(input, width)   -> Integer
 *
 *  USE OF RIPPER LIBRARY ONLY.
 *
 *  Strips up to +width+ leading whitespaces from +input+,
 *  and returns the stripped column width.
 */
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
whole_match_p(struct parser_params *p, const char *eos, long len, int indent)
{
    const char *ptr = p->lex.pbeg;
    long n;

    if (indent) {
	while (*ptr && ISSPACE(*ptr)) ptr++;
    }
    n = p->lex.pend - (ptr + len);
    if (n < 0) return FALSE;
    if (n > 0 && ptr[len] != '\n') {
	if (ptr[len] != '\r') return FALSE;
	if (n <= 1 || ptr[len+1] != '\n') return FALSE;
    }
    return strncmp(eos, ptr, len) == 0;
}

static int
word_match_p(struct parser_params *p, const char *word, long len)
{
    if (strncmp(p->lex.pcur, word, len)) return 0;
    if (p->lex.pcur + len == p->lex.pend) return 1;
    int c = (unsigned char)p->lex.pcur[len];
    if (ISSPACE(c)) return 1;
    switch (c) {
      case '\0': case '\004': case '\032': return 1;
    }
    return 0;
}

#define NUM_SUFFIX_R   (1<<0)
#define NUM_SUFFIX_I   (1<<1)
#define NUM_SUFFIX_ALL 3

static int
number_literal_suffix(struct parser_params *p, int mask)
{
    int c, result = 0;
    const char *lastp = p->lex.pcur;

    while ((c = nextc(p)) != -1) {
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
	    p->lex.pcur = lastp;
	    literal_flush(p, p->lex.pcur);
	    return 0;
	}
	pushback(p, c);
	break;
    }
    return result;
}

static enum yytokentype
set_number_literal(struct parser_params *p, VALUE v,
		   enum yytokentype type, int suffix)
{
    if (suffix & NUM_SUFFIX_I) {
	v = rb_complex_raw(INT2FIX(0), v);
	type = tIMAGINARY;
    }
    set_yylval_literal(v);
    SET_LEX_STATE(EXPR_END);
    return type;
}

static enum yytokentype
set_integer_literal(struct parser_params *p, VALUE v, int suffix)
{
    enum yytokentype type = tINTEGER;
    if (suffix & NUM_SUFFIX_R) {
	v = rb_rational_raw1(v);
	type = tRATIONAL;
    }
    return set_number_literal(p, v, type, suffix);
}

#ifdef RIPPER
static void
dispatch_heredoc_end(struct parser_params *p)
{
    VALUE str;
    if (has_delayed_token(p))
	dispatch_delayed_token(p, tSTRING_CONTENT);
    str = STR_NEW(p->lex.ptok, p->lex.pend - p->lex.ptok);
    ripper_dispatch1(p, ripper_token2eventid(tHEREDOC_END), str);
    lex_goto_eol(p);
    token_flush(p);
}

#else
#define dispatch_heredoc_end(p) ((void)0)
#endif

static enum yytokentype
here_document(struct parser_params *p, rb_strterm_heredoc_t *here)
{
    int c, func, indent = 0;
    const char *eos, *ptr, *ptr_end;
    long len;
    VALUE str = 0;
    rb_encoding *enc = p->enc;
    rb_encoding *base_enc = 0;
    int bol;

    eos = RSTRING_PTR(here->lastline) + here->offset;
    len = here->length;
    indent = (func = here->func) & STR_FUNC_INDENT;

    if ((c = nextc(p)) == -1) {
      error:
#ifdef RIPPER
	if (!has_delayed_token(p)) {
	    dispatch_scan_event(p, tSTRING_CONTENT);
	}
	else {
	    if ((len = p->lex.pcur - p->lex.ptok) > 0) {
		if (!(func & STR_FUNC_REGEXP) && rb_enc_asciicompat(enc)) {
		    int cr = ENC_CODERANGE_UNKNOWN;
		    rb_str_coderange_scan_restartable(p->lex.ptok, p->lex.pcur, enc, &cr);
		    if (cr != ENC_CODERANGE_7BIT &&
			p->enc == rb_usascii_encoding() &&
			enc != rb_utf8_encoding()) {
			enc = rb_ascii8bit_encoding();
		    }
		}
		rb_enc_str_buf_cat(p->delayed.token, p->lex.ptok, len, enc);
	    }
	    dispatch_delayed_token(p, tSTRING_CONTENT);
	}
	lex_goto_eol(p);
#endif
	heredoc_restore(p, &p->lex.strterm->u.heredoc);
	compile_error(p, "can't find string \"%.*s\" anywhere before EOF",
		      (int)len, eos);
	token_flush(p);
	p->lex.strterm = 0;
	SET_LEX_STATE(EXPR_END);
	return tSTRING_END;
    }
    bol = was_bol(p);
    if (!bol) {
	/* not beginning of line, cannot be the terminator */
    }
    else if (p->heredoc_line_indent == -1) {
	/* `heredoc_line_indent == -1` means
	 * - "after an interpolation in the same line", or
	 * - "in a continuing line"
	 */
	p->heredoc_line_indent = 0;
    }
    else if (whole_match_p(p, eos, len, indent)) {
	dispatch_heredoc_end(p);
      restore:
	heredoc_restore(p, &p->lex.strterm->u.heredoc);
	token_flush(p);
	p->lex.strterm = 0;
	SET_LEX_STATE(EXPR_END);
	return tSTRING_END;
    }

    if (!(func & STR_FUNC_EXPAND)) {
	do {
	    ptr = RSTRING_PTR(p->lex.lastline);
	    ptr_end = p->lex.pend;
	    if (ptr_end > ptr) {
		switch (ptr_end[-1]) {
		  case '\n':
		    if (--ptr_end == ptr || ptr_end[-1] != '\r') {
			ptr_end++;
			break;
		    }
		  case '\r':
		    --ptr_end;
		}
	    }

	    if (p->heredoc_indent > 0) {
		long i = 0;
		while (ptr + i < ptr_end && parser_update_heredoc_indent(p, ptr[i]))
		    i++;
		p->heredoc_line_indent = 0;
	    }

	    if (str)
		rb_str_cat(str, ptr, ptr_end - ptr);
	    else
		str = STR_NEW(ptr, ptr_end - ptr);
	    if (ptr_end < p->lex.pend) rb_str_cat(str, "\n", 1);
	    lex_goto_eol(p);
	    if (p->heredoc_indent > 0) {
		goto flush_str;
	    }
	    if (nextc(p) == -1) {
		if (str) {
		    str = 0;
		}
		goto error;
	    }
	} while (!whole_match_p(p, eos, len, indent));
    }
    else {
	/*	int mb = ENC_CODERANGE_7BIT, *mbp = &mb;*/
	newtok(p);
	if (c == '#') {
	    int t = parser_peek_variable_name(p);
	    if (p->heredoc_line_indent != -1) {
		if (p->heredoc_indent > p->heredoc_line_indent) {
		    p->heredoc_indent = p->heredoc_line_indent;
		}
		p->heredoc_line_indent = -1;
	    }
	    if (t) return t;
	    tokadd(p, '#');
	    c = nextc(p);
	}
	do {
	    pushback(p, c);
	    enc = p->enc;
	    if ((c = tokadd_string(p, func, '\n', 0, NULL, &enc, &base_enc)) == -1) {
		if (p->eofp) goto error;
		goto restore;
	    }
	    if (c != '\n') {
		if (c == '\\') p->heredoc_line_indent = -1;
	      flush:
		str = STR_NEW3(tok(p), toklen(p), enc, func);
	      flush_str:
		set_yylval_str(str);
#ifndef RIPPER
		if (bol) yylval.node->flags |= NODE_FL_NEWLINE;
#endif
		flush_string_content(p, enc);
		return tSTRING_CONTENT;
	    }
	    tokadd(p, nextc(p));
	    if (p->heredoc_indent > 0) {
		lex_goto_eol(p);
		goto flush;
	    }
	    /*	    if (mbp && mb == ENC_CODERANGE_UNKNOWN) mbp = 0;*/
	    if ((c = nextc(p)) == -1) goto error;
	} while (!whole_match_p(p, eos, len, indent));
	str = STR_NEW3(tok(p), toklen(p), enc, func);
    }
    dispatch_heredoc_end(p);
#ifdef RIPPER
    str = ripper_new_yylval(p, ripper_token2eventid(tSTRING_CONTENT),
			    yylval.val, str);
#endif
    heredoc_restore(p, &p->lex.strterm->u.heredoc);
    token_flush(p);
    p->lex.strterm = NEW_STRTERM(func | STR_FUNC_TERM, 0, 0);
    set_yylval_str(str);
#ifndef RIPPER
    if (bol) yylval.node->flags |= NODE_FL_NEWLINE;
#endif
    return tSTRING_CONTENT;
}

#include "lex.c"

static int
arg_ambiguous(struct parser_params *p, char c)
{
#ifndef RIPPER
    if (c == '/') {
        rb_warning1("ambiguity between regexp and two divisions: wrap regexp in parentheses or add a space after `%c' operator", WARN_I(c));
    }
    else {
        rb_warning1("ambiguous first argument; put parentheses or a space even after `%c' operator", WARN_I(c));
    }
#else
    dispatch1(arg_ambiguous, rb_usascii_str_new(&c, 1));
#endif
    return TRUE;
}

static ID
formal_argument(struct parser_params *p, ID lhs)
{
    switch (id_type(lhs)) {
      case ID_LOCAL:
	break;
#ifndef RIPPER
      case ID_CONST:
	yyerror0("formal argument cannot be a constant");
	return 0;
      case ID_INSTANCE:
	yyerror0("formal argument cannot be an instance variable");
	return 0;
      case ID_GLOBAL:
	yyerror0("formal argument cannot be a global variable");
	return 0;
      case ID_CLASS:
	yyerror0("formal argument cannot be a class variable");
	return 0;
      default:
	yyerror0("formal argument must be local variable");
	return 0;
#else
      default:
	lhs = dispatch1(param_error, lhs);
	ripper_error(p);
	return 0;
#endif
    }
    shadowing_lvar(p, lhs);
    return lhs;
}

static int
lvar_defined(struct parser_params *p, ID id)
{
    return (dyna_in_block(p) && dvar_defined(p, id)) || local_id(p, id);
}

/* emacsen -*- hack */
static long
parser_encode_length(struct parser_params *p, const char *name, long len)
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
parser_set_encode(struct parser_params *p, const char *name)
{
    int idx = rb_enc_find_index(name);
    rb_encoding *enc;
    VALUE excargs[3];

    if (idx < 0) {
	excargs[1] = rb_sprintf("unknown encoding name: %s", name);
      error:
	excargs[0] = rb_eArgError;
	excargs[2] = rb_make_backtrace();
	rb_ary_unshift(excargs[2], rb_sprintf("%"PRIsVALUE":%d", p->ruby_sourcefile_string, p->ruby_sourceline));
	rb_exc_raise(rb_make_exception(3, excargs));
    }
    enc = rb_enc_from_index(idx);
    if (!rb_enc_asciicompat(enc)) {
	excargs[1] = rb_sprintf("%s is not ASCII compatible", rb_enc_name(enc));
	goto error;
    }
    p->enc = enc;
#ifndef RIPPER
    if (p->debug_lines) {
	VALUE lines = p->debug_lines;
	long i, n = RARRAY_LEN(lines);
	for (i = 0; i < n; ++i) {
	    rb_enc_associate_index(RARRAY_AREF(lines, i), idx);
	}
    }
#endif
}

static int
comment_at_top(struct parser_params *p)
{
    const char *ptr = p->lex.pbeg, *ptr_end = p->lex.pcur - 1;
    if (p->line_count != (p->has_shebang ? 2 : 1)) return 0;
    while (ptr < ptr_end) {
	if (!ISSPACE(*ptr)) return 0;
	ptr++;
    }
    return 1;
}

typedef long (*rb_magic_comment_length_t)(struct parser_params *p, const char *name, long len);
typedef void (*rb_magic_comment_setter_t)(struct parser_params *p, const char *name, const char *val);

static void
magic_comment_encoding(struct parser_params *p, const char *name, const char *val)
{
    if (!comment_at_top(p)) {
	return;
    }
    parser_set_encode(p, val);
}

static int
parser_get_bool(struct parser_params *p, const char *name, const char *val)
{
    switch (*val) {
      case 't': case 'T':
	if (STRCASECMP(val, "true") == 0) {
	    return TRUE;
	}
	break;
      case 'f': case 'F':
	if (STRCASECMP(val, "false") == 0) {
	    return FALSE;
	}
	break;
    }
    rb_warning2("invalid value for %s: %s", WARN_S(name), WARN_S(val));
    return -1;
}

static void
parser_set_token_info(struct parser_params *p, const char *name, const char *val)
{
    int b = parser_get_bool(p, name, val);
    if (b >= 0) p->token_info_enabled = b;
}

static void
parser_set_compile_option_flag(struct parser_params *p, const char *name, const char *val)
{
    int b;

    if (p->token_seen) {
	rb_warning1("`%s' is ignored after any tokens", WARN_S(name));
	return;
    }

    b = parser_get_bool(p, name, val);
    if (b < 0) return;

    if (!p->compile_option)
	p->compile_option = rb_obj_hide(rb_ident_hash_new());
    rb_hash_aset(p->compile_option, ID2SYM(rb_intern(name)),
		 (b ? Qtrue : Qfalse));
}

# if WARN_PAST_SCOPE
static void
parser_set_past_scope(struct parser_params *p, const char *name, const char *val)
{
    int b = parser_get_bool(p, name, val);
    if (b >= 0) p->past_scope_enabled = b;
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
parser_magic_comment(struct parser_params *p, const char *str, long len)
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
	const struct magic_comment *mc = magic_comments;
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
	    if (STRNCASECMP(mc->name, s, n) == 0 && !mc->name[n]) {
		n = vend - vbeg;
		if (mc->length) {
		    n = (*mc->length)(p, vbeg, n);
		}
		str_copy(val, vbeg, n);
		(*mc->func)(p, mc->name, RSTRING_PTR(val));
		break;
	    }
	} while (++mc < magic_comments + numberof(magic_comments));
#ifdef RIPPER
	str_copy(val, vbeg, vend - vbeg);
	dispatch2(magic_comment, name, val);
#endif
    }

    return TRUE;
}

static void
set_file_encoding(struct parser_params *p, const char *str, const char *send)
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
    s = rb_str_new(beg, parser_encode_length(p, beg, str - beg));
    parser_set_encode(p, RSTRING_PTR(s));
    rb_str_resize(s, 0);
}

static void
parser_prepare(struct parser_params *p)
{
    int c = nextc(p);
    p->token_info_enabled = !compile_for_eval && RTEST(ruby_verbose);
    switch (c) {
      case '#':
	if (peek(p, '!')) p->has_shebang = 1;
	break;
      case 0xef:		/* UTF-8 BOM marker */
	if (p->lex.pend - p->lex.pcur >= 2 &&
	    (unsigned char)p->lex.pcur[0] == 0xbb &&
	    (unsigned char)p->lex.pcur[1] == 0xbf) {
	    p->enc = rb_utf8_encoding();
	    p->lex.pcur += 2;
	    p->lex.pbeg = p->lex.pcur;
	    return;
	}
	break;
      case EOF:
	return;
    }
    pushback(p, c);
    p->enc = rb_enc_get(p->lex.lastline);
}

#ifndef RIPPER
#define ambiguous_operator(tok, op, syn) ( \
    rb_warning0("`"op"' after local variable or literal is interpreted as binary operator"), \
    rb_warning0("even though it seems like "syn""))
#else
#define ambiguous_operator(tok, op, syn) \
    dispatch2(operator_ambiguous, TOKEN2VAL(tok), rb_str_new_cstr(syn))
#endif
#define warn_balanced(tok, op, syn) ((void) \
    (!IS_lex_state_for(last_state, EXPR_CLASS|EXPR_DOT|EXPR_FNAME|EXPR_ENDFN) && \
     space_seen && !ISSPACE(c) && \
     (ambiguous_operator(tok, op, syn), 0)), \
     (enum yytokentype)(tok))

static VALUE
parse_rational(struct parser_params *p, char *str, int len, int seen_point)
{
    VALUE v;
    char *point = &str[seen_point];
    size_t fraclen = len-seen_point-1;
    memmove(point, point+1, fraclen+1);
    v = rb_cstr_to_inum(str, 10, FALSE);
    return rb_rational_new(v, rb_int_positive_pow(10, fraclen));
}

static enum yytokentype
no_digits(struct parser_params *p)
{
    yyerror0("numeric literal without digits");
    if (peek(p, '_')) nextc(p);
    /* dummy 0, for tUMINUS_NUM at numeric */
    return set_integer_literal(p, INT2FIX(0), 0);
}

static enum yytokentype
parse_numeric(struct parser_params *p, int c)
{
    int is_float, seen_point, seen_e, nondigit;
    int suffix;

    is_float = seen_point = seen_e = nondigit = 0;
    SET_LEX_STATE(EXPR_END);
    newtok(p);
    if (c == '-' || c == '+') {
	tokadd(p, c);
	c = nextc(p);
    }
    if (c == '0') {
	int start = toklen(p);
	c = nextc(p);
	if (c == 'x' || c == 'X') {
	    /* hexadecimal */
	    c = nextc(p);
	    if (c != -1 && ISXDIGIT(c)) {
		do {
		    if (c == '_') {
			if (nondigit) break;
			nondigit = c;
			continue;
		    }
		    if (!ISXDIGIT(c)) break;
		    nondigit = 0;
		    tokadd(p, c);
		} while ((c = nextc(p)) != -1);
	    }
	    pushback(p, c);
	    tokfix(p);
	    if (toklen(p) == start) {
		return no_digits(p);
	    }
	    else if (nondigit) goto trailing_uc;
	    suffix = number_literal_suffix(p, NUM_SUFFIX_ALL);
	    return set_integer_literal(p, rb_cstr_to_inum(tok(p), 16, FALSE), suffix);
	}
	if (c == 'b' || c == 'B') {
	    /* binary */
	    c = nextc(p);
	    if (c == '0' || c == '1') {
		do {
		    if (c == '_') {
			if (nondigit) break;
			nondigit = c;
			continue;
		    }
		    if (c != '0' && c != '1') break;
		    nondigit = 0;
		    tokadd(p, c);
		} while ((c = nextc(p)) != -1);
	    }
	    pushback(p, c);
	    tokfix(p);
	    if (toklen(p) == start) {
		return no_digits(p);
	    }
	    else if (nondigit) goto trailing_uc;
	    suffix = number_literal_suffix(p, NUM_SUFFIX_ALL);
	    return set_integer_literal(p, rb_cstr_to_inum(tok(p), 2, FALSE), suffix);
	}
	if (c == 'd' || c == 'D') {
	    /* decimal */
	    c = nextc(p);
	    if (c != -1 && ISDIGIT(c)) {
		do {
		    if (c == '_') {
			if (nondigit) break;
			nondigit = c;
			continue;
		    }
		    if (!ISDIGIT(c)) break;
		    nondigit = 0;
		    tokadd(p, c);
		} while ((c = nextc(p)) != -1);
	    }
	    pushback(p, c);
	    tokfix(p);
	    if (toklen(p) == start) {
		return no_digits(p);
	    }
	    else if (nondigit) goto trailing_uc;
	    suffix = number_literal_suffix(p, NUM_SUFFIX_ALL);
	    return set_integer_literal(p, rb_cstr_to_inum(tok(p), 10, FALSE), suffix);
	}
	if (c == '_') {
	    /* 0_0 */
	    goto octal_number;
	}
	if (c == 'o' || c == 'O') {
	    /* prefixed octal */
	    c = nextc(p);
	    if (c == -1 || c == '_' || !ISDIGIT(c)) {
		return no_digits(p);
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
		tokadd(p, c);
	    } while ((c = nextc(p)) != -1);
	    if (toklen(p) > start) {
		pushback(p, c);
		tokfix(p);
		if (nondigit) goto trailing_uc;
		suffix = number_literal_suffix(p, NUM_SUFFIX_ALL);
		return set_integer_literal(p, rb_cstr_to_inum(tok(p), 8, FALSE), suffix);
	    }
	    if (nondigit) {
		pushback(p, c);
		goto trailing_uc;
	    }
	}
	if (c > '7' && c <= '9') {
	  invalid_octal:
	    yyerror0("Invalid octal digit");
	}
	else if (c == '.' || c == 'e' || c == 'E') {
	    tokadd(p, '0');
	}
	else {
	    pushback(p, c);
	    suffix = number_literal_suffix(p, NUM_SUFFIX_ALL);
	    return set_integer_literal(p, INT2FIX(0), suffix);
	}
    }

    for (;;) {
	switch (c) {
	  case '0': case '1': case '2': case '3': case '4':
	  case '5': case '6': case '7': case '8': case '9':
	    nondigit = 0;
	    tokadd(p, c);
	    break;

	  case '.':
	    if (nondigit) goto trailing_uc;
	    if (seen_point || seen_e) {
		goto decode_num;
	    }
	    else {
		int c0 = nextc(p);
		if (c0 == -1 || !ISDIGIT(c0)) {
		    pushback(p, c0);
		    goto decode_num;
		}
		c = c0;
	    }
	    seen_point = toklen(p);
	    tokadd(p, '.');
	    tokadd(p, c);
	    is_float++;
	    nondigit = 0;
	    break;

	  case 'e':
	  case 'E':
	    if (nondigit) {
		pushback(p, c);
		c = nondigit;
		goto decode_num;
	    }
	    if (seen_e) {
		goto decode_num;
	    }
	    nondigit = c;
	    c = nextc(p);
	    if (c != '-' && c != '+' && !ISDIGIT(c)) {
		pushback(p, c);
		nondigit = 0;
		goto decode_num;
	    }
	    tokadd(p, nondigit);
	    seen_e++;
	    is_float++;
	    tokadd(p, c);
	    nondigit = (c == '-' || c == '+') ? c : 0;
	    break;

	  case '_':	/* `_' in number just ignored */
	    if (nondigit) goto decode_num;
	    nondigit = c;
	    break;

	  default:
	    goto decode_num;
	}
	c = nextc(p);
    }

  decode_num:
    pushback(p, c);
    if (nondigit) {
      trailing_uc:
	literal_flush(p, p->lex.pcur - 1);
	YYLTYPE loc = RUBY_INIT_YYLLOC();
	compile_error(p, "trailing `%c' in number", nondigit);
	parser_show_error_line(p, &loc);
    }
    tokfix(p);
    if (is_float) {
	enum yytokentype type = tFLOAT;
	VALUE v;

	suffix = number_literal_suffix(p, seen_e ? NUM_SUFFIX_I : NUM_SUFFIX_ALL);
	if (suffix & NUM_SUFFIX_R) {
	    type = tRATIONAL;
	    v = parse_rational(p, tok(p), toklen(p), seen_point);
	}
	else {
	    double d = strtod(tok(p), 0);
	    if (errno == ERANGE) {
		rb_warning1("Float %s out of range", WARN_S(tok(p)));
		errno = 0;
	    }
	    v = DBL2NUM(d);
	}
	return set_number_literal(p, v, type, suffix);
    }
    suffix = number_literal_suffix(p, NUM_SUFFIX_ALL);
    return set_integer_literal(p, rb_cstr_to_inum(tok(p), 10, FALSE), suffix);
}

static enum yytokentype
parse_qmark(struct parser_params *p, int space_seen)
{
    rb_encoding *enc;
    register int c;
    VALUE lit;

    if (IS_END()) {
	SET_LEX_STATE(EXPR_VALUE);
	return '?';
    }
    c = nextc(p);
    if (c == -1) {
	compile_error(p, "incomplete character syntax");
	return 0;
    }
    if (rb_enc_isspace(c, p->enc)) {
	if (!IS_ARG()) {
	    int c2 = escaped_control_code(c);
	    if (c2) {
		WARN_SPACE_CHAR(c2, "?");
	    }
	}
      ternary:
	pushback(p, c);
	SET_LEX_STATE(EXPR_VALUE);
	return '?';
    }
    newtok(p);
    enc = p->enc;
    if (!parser_isascii(p)) {
	if (tokadd_mbchar(p, c) == -1) return 0;
    }
    else if ((rb_enc_isalnum(c, p->enc) || c == '_') &&
	     p->lex.pcur < p->lex.pend && is_identchar(p->lex.pcur, p->lex.pend, p->enc)) {
	if (space_seen) {
	    const char *start = p->lex.pcur - 1, *ptr = start;
	    do {
		int n = parser_precise_mbclen(p, ptr);
		if (n < 0) return -1;
		ptr += n;
	    } while (ptr < p->lex.pend && is_identchar(ptr, p->lex.pend, p->enc));
	    rb_warn2("`?' just followed by `%.*s' is interpreted as" \
		     " a conditional operator, put a space after `?'",
		     WARN_I((int)(ptr - start)), WARN_S_L(start, (ptr - start)));
	}
	goto ternary;
    }
    else if (c == '\\') {
	if (peek(p, 'u')) {
	    nextc(p);
	    enc = rb_utf8_encoding();
	    tokadd_utf8(p, &enc, -1, 0, 0);
	}
	else if (!lex_eol_p(p) && !(c = *p->lex.pcur, ISASCII(c))) {
	    nextc(p);
	    if (tokadd_mbchar(p, c) == -1) return 0;
	}
	else {
	    c = read_escape(p, 0, &enc);
	    tokadd(p, c);
	}
    }
    else {
	tokadd(p, c);
    }
    tokfix(p);
    lit = STR_NEW3(tok(p), toklen(p), enc, 0);
    set_yylval_str(lit);
    SET_LEX_STATE(EXPR_END);
    return tCHAR;
}

static enum yytokentype
parse_percent(struct parser_params *p, const int space_seen, const enum lex_state_e last_state)
{
    register int c;
    const char *ptok = p->lex.pcur;

    if (IS_BEG()) {
	int term;
	int paren;

	c = nextc(p);
      quotation:
	if (c == -1 || !ISALNUM(c)) {
	    term = c;
	    if (!ISASCII(c)) goto unknown;
	    c = 'Q';
	}
	else {
	    term = nextc(p);
	    if (rb_enc_isalnum(term, p->enc) || !parser_isascii(p)) {
	      unknown:
		pushback(p, term);
		c = parser_precise_mbclen(p, p->lex.pcur);
		if (c < 0) return 0;
		p->lex.pcur += c;
		yyerror0("unknown type of %string");
		return 0;
	    }
	}
	if (term == -1) {
	    compile_error(p, "unterminated quoted string meets end of file");
	    return 0;
	}
	paren = term;
	if (term == '(') term = ')';
	else if (term == '[') term = ']';
	else if (term == '{') term = '}';
	else if (term == '<') term = '>';
	else paren = 0;

	p->lex.ptok = ptok-1;
	switch (c) {
	  case 'Q':
	    p->lex.strterm = NEW_STRTERM(str_dquote, term, paren);
	    return tSTRING_BEG;

	  case 'q':
	    p->lex.strterm = NEW_STRTERM(str_squote, term, paren);
	    return tSTRING_BEG;

	  case 'W':
	    p->lex.strterm = NEW_STRTERM(str_dword, term, paren);
	    return tWORDS_BEG;

	  case 'w':
	    p->lex.strterm = NEW_STRTERM(str_sword, term, paren);
	    return tQWORDS_BEG;

	  case 'I':
	    p->lex.strterm = NEW_STRTERM(str_dword, term, paren);
	    return tSYMBOLS_BEG;

	  case 'i':
	    p->lex.strterm = NEW_STRTERM(str_sword, term, paren);
	    return tQSYMBOLS_BEG;

	  case 'x':
	    p->lex.strterm = NEW_STRTERM(str_xquote, term, paren);
	    return tXSTRING_BEG;

	  case 'r':
	    p->lex.strterm = NEW_STRTERM(str_regexp, term, paren);
	    return tREGEXP_BEG;

	  case 's':
	    p->lex.strterm = NEW_STRTERM(str_ssym, term, paren);
	    SET_LEX_STATE(EXPR_FNAME|EXPR_FITEM);
	    return tSYMBEG;

	  default:
	    yyerror0("unknown type of %string");
	    return 0;
	}
    }
    if ((c = nextc(p)) == '=') {
	set_yylval_id('%');
	SET_LEX_STATE(EXPR_BEG);
	return tOP_ASGN;
    }
    if (IS_SPCARG(c) || (IS_lex_state(EXPR_FITEM) && c == 's')) {
	goto quotation;
    }
    SET_LEX_STATE(IS_AFTER_OPERATOR() ? EXPR_ARG : EXPR_BEG);
    pushback(p, c);
    return warn_balanced('%', "%%", "string literal");
}

static int
tokadd_ident(struct parser_params *p, int c)
{
    do {
	if (tokadd_mbchar(p, c) == -1) return -1;
	c = nextc(p);
    } while (parser_is_identchar(p));
    pushback(p, c);
    return 0;
}

static ID
tokenize_ident(struct parser_params *p, const enum lex_state_e last_state)
{
    ID ident = TOK_INTERN();

    set_yylval_name(ident);

    return ident;
}

static int
parse_numvar(struct parser_params *p)
{
    size_t len;
    int overflow;
    unsigned long n = ruby_scan_digits(tok(p)+1, toklen(p)-1, 10, &len, &overflow);
    const unsigned long nth_ref_max =
	((FIXNUM_MAX < INT_MAX) ? FIXNUM_MAX : INT_MAX) >> 1;
    /* NTH_REF is left-shifted to be ORed with back-ref flag and
     * turned into a Fixnum, in compile.c */

    if (overflow || n > nth_ref_max) {
	/* compile_error()? */
	rb_warn1("`%s' is too big for a number variable, always nil", WARN_S(tok(p)));
	return 0;		/* $0 is $PROGRAM_NAME, not NTH_REF */
    }
    else {
	return (int)n;
    }
}

static enum yytokentype
parse_gvar(struct parser_params *p, const enum lex_state_e last_state)
{
    const char *ptr = p->lex.pcur;
    register int c;

    SET_LEX_STATE(EXPR_END);
    p->lex.ptok = ptr - 1; /* from '$' */
    newtok(p);
    c = nextc(p);
    switch (c) {
      case '_':		/* $_: last read line string */
	c = nextc(p);
	if (parser_is_identchar(p)) {
	    tokadd(p, '$');
	    tokadd(p, '_');
	    break;
	}
	pushback(p, c);
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
	tokadd(p, '$');
	tokadd(p, c);
	goto gvar;

      case '-':
	tokadd(p, '$');
	tokadd(p, c);
	c = nextc(p);
	if (parser_is_identchar(p)) {
	    if (tokadd_mbchar(p, c) == -1) return 0;
	}
	else {
	    pushback(p, c);
	    pushback(p, '-');
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
	    tokadd(p, '$');
	    tokadd(p, c);
	    goto gvar;
	}
	set_yylval_node(NEW_BACK_REF(c, &_cur_loc));
	return tBACK_REF;

      case '1': case '2': case '3':
      case '4': case '5': case '6':
      case '7': case '8': case '9':
	tokadd(p, '$');
	do {
	    tokadd(p, c);
	    c = nextc(p);
	} while (c != -1 && ISDIGIT(c));
	pushback(p, c);
	if (IS_lex_state_for(last_state, EXPR_FNAME)) goto gvar;
	tokfix(p);
	set_yylval_node(NEW_NTH_REF(parse_numvar(p), &_cur_loc));
	return tNTH_REF;

      default:
	if (!parser_is_identchar(p)) {
	    YYLTYPE loc = RUBY_INIT_YYLLOC();
	    if (c == -1 || ISSPACE(c)) {
		compile_error(p, "`$' without identifiers is not allowed as a global variable name");
	    }
	    else {
		pushback(p, c);
		compile_error(p, "`$%c' is not allowed as a global variable name", c);
	    }
	    parser_show_error_line(p, &loc);
	    set_yylval_noname();
	    return tGVAR;
	}
	/* fall through */
      case '0':
	tokadd(p, '$');
    }

    if (tokadd_ident(p, c)) return 0;
    SET_LEX_STATE(EXPR_END);
    tokenize_ident(p, last_state);
    return tGVAR;
}

#ifndef RIPPER
static bool
parser_numbered_param(struct parser_params *p, int n)
{
    if (n < 0) return false;

    if (DVARS_TERMINAL_P(p->lvtbl->args) || DVARS_TERMINAL_P(p->lvtbl->args->prev)) {
	return false;
    }
    if (p->max_numparam == ORDINAL_PARAM) {
	compile_error(p, "ordinary parameter is defined");
	return false;
    }
    struct vtable *args = p->lvtbl->args;
    if (p->max_numparam < n) {
	p->max_numparam = n;
    }
    while (n > args->pos) {
	vtable_add(args, NUMPARAM_IDX_TO_ID(args->pos+1));
    }
    return true;
}
#endif

static enum yytokentype
parse_atmark(struct parser_params *p, const enum lex_state_e last_state)
{
    const char *ptr = p->lex.pcur;
    enum yytokentype result = tIVAR;
    register int c = nextc(p);
    YYLTYPE loc;

    p->lex.ptok = ptr - 1; /* from '@' */
    newtok(p);
    tokadd(p, '@');
    if (c == '@') {
	result = tCVAR;
	tokadd(p, '@');
	c = nextc(p);
    }
    SET_LEX_STATE(IS_lex_state_for(last_state, EXPR_FNAME) ? EXPR_ENDFN : EXPR_END);
    if (c == -1 || !parser_is_identchar(p)) {
	pushback(p, c);
	RUBY_SET_YYLLOC(loc);
	if (result == tIVAR) {
	    compile_error(p, "`@' without identifiers is not allowed as an instance variable name");
	}
	else {
	    compile_error(p, "`@@' without identifiers is not allowed as a class variable name");
	}
	parser_show_error_line(p, &loc);
	set_yylval_noname();
	SET_LEX_STATE(EXPR_END);
	return result;
    }
    else if (ISDIGIT(c)) {
	pushback(p, c);
	RUBY_SET_YYLLOC(loc);
	if (result == tIVAR) {
	    compile_error(p, "`@%c' is not allowed as an instance variable name", c);
	}
	else {
	    compile_error(p, "`@@%c' is not allowed as a class variable name", c);
	}
	parser_show_error_line(p, &loc);
	set_yylval_noname();
	SET_LEX_STATE(EXPR_END);
	return result;
    }

    if (tokadd_ident(p, c)) return 0;
    tokenize_ident(p, last_state);
    return result;
}

static enum yytokentype
parse_ident(struct parser_params *p, int c, int cmd_state)
{
    enum yytokentype result;
    int mb = ENC_CODERANGE_7BIT;
    const enum lex_state_e last_state = p->lex.state;
    ID ident;

    do {
	if (!ISASCII(c)) mb = ENC_CODERANGE_UNKNOWN;
	if (tokadd_mbchar(p, c) == -1) return 0;
	c = nextc(p);
    } while (parser_is_identchar(p));
    if ((c == '!' || c == '?') && !peek(p, '=')) {
	result = tFID;
	tokadd(p, c);
    }
    else if (c == '=' && IS_lex_state(EXPR_FNAME) &&
	     (!peek(p, '~') && !peek(p, '>') && (!peek(p, '=') || (peek_n(p, '>', 1))))) {
	result = tIDENTIFIER;
	tokadd(p, c);
    }
    else {
	result = tCONSTANT;	/* assume provisionally */
	pushback(p, c);
    }
    tokfix(p);

    if (IS_LABEL_POSSIBLE()) {
	if (IS_LABEL_SUFFIX(0)) {
	    SET_LEX_STATE(EXPR_ARG|EXPR_LABELED);
	    nextc(p);
	    set_yylval_name(TOK_INTERN());
	    return tLABEL;
	}
    }
    if (mb == ENC_CODERANGE_7BIT && !IS_lex_state(EXPR_DOT)) {
	const struct kwtable *kw;

	/* See if it is a reserved word.  */
	kw = rb_reserved_word(tok(p), toklen(p));
	if (kw) {
	    enum lex_state_e state = p->lex.state;
	    if (IS_lex_state_for(state, EXPR_FNAME)) {
		SET_LEX_STATE(EXPR_ENDFN);
		set_yylval_name(rb_intern2(tok(p), toklen(p)));
		return kw->id[0];
	    }
	    SET_LEX_STATE(kw->state);
	    if (IS_lex_state(EXPR_BEG)) {
		p->command_start = TRUE;
	    }
	    if (kw->id[0] == keyword_do) {
		if (lambda_beginning_p()) {
		    p->lex.lpar_beg = -1; /* make lambda_beginning_p() == FALSE in the body of "-> do ... end" */
		    return keyword_do_LAMBDA;
		}
		if (COND_P()) return keyword_do_cond;
		if (CMDARG_P() && !IS_lex_state_for(state, EXPR_CMDARG))
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
    else if (p->lex.state == EXPR_FNAME) {
	SET_LEX_STATE(EXPR_ENDFN);
    }
    else {
	SET_LEX_STATE(EXPR_END);
    }

    ident = tokenize_ident(p, last_state);
    if (result == tCONSTANT && is_local_id(ident)) result = tIDENTIFIER;
    if (!IS_lex_state_for(last_state, EXPR_DOT|EXPR_FNAME) &&
	(result == tIDENTIFIER) && /* not EXPR_FNAME, not attrasgn */
	lvar_defined(p, ident)) {
	SET_LEX_STATE(EXPR_END|EXPR_LABEL);
    }
    return result;
}

static enum yytokentype
parser_yylex(struct parser_params *p)
{
    register int c;
    int space_seen = 0;
    int cmd_state;
    int label;
    enum lex_state_e last_state;
    int fallthru = FALSE;
    int token_seen = p->token_seen;

    if (p->lex.strterm) {
	if (p->lex.strterm->flags & STRTERM_HEREDOC) {
	    return here_document(p, &p->lex.strterm->u.heredoc);
	}
	else {
	    token_flush(p);
	    return parse_string(p, &p->lex.strterm->u.literal);
	}
    }
    cmd_state = p->command_start;
    p->command_start = FALSE;
    p->token_seen = TRUE;
  retry:
    last_state = p->lex.state;
#ifndef RIPPER
    token_flush(p);
#endif
    switch (c = nextc(p)) {
      case '\0':		/* NUL */
      case '\004':		/* ^D */
      case '\032':		/* ^Z */
      case -1:			/* end of script. */
	return 0;

	/* white spaces */
      case '\r':
	if (!p->cr_seen) {
	    p->cr_seen = TRUE;
	    /* carried over with p->lex.nextline for nextc() */
	    rb_warn0("encountered \\r in middle of line, treated as a mere space");
	}
	/* fall through */
      case ' ': case '\t': case '\f':
      case '\13': /* '\v' */
	space_seen = 1;
#ifdef RIPPER
	while ((c = nextc(p))) {
	    switch (c) {
	      case ' ': case '\t': case '\f': case '\r':
	      case '\13': /* '\v' */
		break;
	      default:
		goto outofloop;
	    }
	}
      outofloop:
	pushback(p, c);
	dispatch_scan_event(p, tSP);
#endif
	goto retry;

      case '#':		/* it's a comment */
	p->token_seen = token_seen;
	/* no magic_comment in shebang line */
	if (!parser_magic_comment(p, p->lex.pcur, p->lex.pend - p->lex.pcur)) {
	    if (comment_at_top(p)) {
		set_file_encoding(p, p->lex.pcur, p->lex.pend);
	    }
	}
	lex_goto_eol(p);
        dispatch_scan_event(p, tCOMMENT);
        fallthru = TRUE;
	/* fall through */
      case '\n':
	p->token_seen = token_seen;
	c = (IS_lex_state(EXPR_BEG|EXPR_CLASS|EXPR_FNAME|EXPR_DOT) &&
	     !IS_lex_state(EXPR_LABELED));
	if (c || IS_lex_state_all(EXPR_ARG|EXPR_LABELED)) {
            if (!fallthru) {
                dispatch_scan_event(p, tIGNORED_NL);
            }
            fallthru = FALSE;
	    if (!c && p->ctxt.in_kwarg) {
		goto normal_newline;
	    }
	    goto retry;
	}
	while (1) {
	    switch (c = nextc(p)) {
	      case ' ': case '\t': case '\f': case '\r':
	      case '\13': /* '\v' */
		space_seen = 1;
		break;
	      case '#':
		pushback(p, c);
		if (space_seen) dispatch_scan_event(p, tSP);
		goto retry;
	      case '&':
	      case '.': {
		dispatch_delayed_token(p, tIGNORED_NL);
		if (peek(p, '.') == (c == '&')) {
		    pushback(p, c);
		    dispatch_scan_event(p, tSP);
		    goto retry;
		}
	      }
	      default:
		p->ruby_sourceline--;
		p->lex.nextline = p->lex.lastline;
	      case -1:		/* EOF no decrement*/
#ifndef RIPPER
		if (p->lex.prevline && !p->eofp) p->lex.lastline = p->lex.prevline;
		p->lex.pbeg = RSTRING_PTR(p->lex.lastline);
		p->lex.pend = p->lex.pcur = p->lex.pbeg + RSTRING_LEN(p->lex.lastline);
		pushback(p, 1); /* always pushback */
		p->lex.ptok = p->lex.pcur;
#else
		lex_goto_eol(p);
		if (c != -1) {
		    p->lex.ptok = p->lex.pcur;
		}
#endif
		goto normal_newline;
	    }
	}
      normal_newline:
	p->command_start = TRUE;
	SET_LEX_STATE(EXPR_BEG);
	return '\n';

      case '*':
	if ((c = nextc(p)) == '*') {
	    if ((c = nextc(p)) == '=') {
		set_yylval_id(idPow);
		SET_LEX_STATE(EXPR_BEG);
		return tOP_ASGN;
	    }
	    pushback(p, c);
	    if (IS_SPCARG(c)) {
		rb_warning0("`**' interpreted as argument prefix");
		c = tDSTAR;
	    }
	    else if (IS_BEG()) {
		c = tDSTAR;
	    }
	    else {
		c = warn_balanced((enum ruby_method_ids)tPOW, "**", "argument prefix");
	    }
	}
	else {
	    if (c == '=') {
                set_yylval_id('*');
		SET_LEX_STATE(EXPR_BEG);
		return tOP_ASGN;
	    }
	    pushback(p, c);
	    if (IS_SPCARG(c)) {
		rb_warning0("`*' interpreted as argument prefix");
		c = tSTAR;
	    }
	    else if (IS_BEG()) {
		c = tSTAR;
	    }
	    else {
		c = warn_balanced('*', "*", "argument prefix");
	    }
	}
	SET_LEX_STATE(IS_AFTER_OPERATOR() ? EXPR_ARG : EXPR_BEG);
	return c;

      case '!':
	c = nextc(p);
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
	pushback(p, c);
	return '!';

      case '=':
	if (was_bol(p)) {
	    /* skip embedded rd document */
	    if (word_match_p(p, "begin", 5)) {
		int first_p = TRUE;

		lex_goto_eol(p);
		dispatch_scan_event(p, tEMBDOC_BEG);
		for (;;) {
		    lex_goto_eol(p);
		    if (!first_p) {
			dispatch_scan_event(p, tEMBDOC);
		    }
		    first_p = FALSE;
		    c = nextc(p);
		    if (c == -1) {
			compile_error(p, "embedded document meets end of file");
			return 0;
		    }
		    if (c == '=' && word_match_p(p, "end", 3)) {
			break;
		    }
		    pushback(p, c);
		}
		lex_goto_eol(p);
		dispatch_scan_event(p, tEMBDOC_END);
		goto retry;
	    }
	}

	SET_LEX_STATE(IS_AFTER_OPERATOR() ? EXPR_ARG : EXPR_BEG);
	if ((c = nextc(p)) == '=') {
	    if ((c = nextc(p)) == '=') {
		return tEQQ;
	    }
	    pushback(p, c);
	    return tEQ;
	}
	if (c == '~') {
	    return tMATCH;
	}
	else if (c == '>') {
	    return tASSOC;
	}
	pushback(p, c);
	return '=';

      case '<':
	c = nextc(p);
	if (c == '<' &&
	    !IS_lex_state(EXPR_DOT | EXPR_CLASS) &&
	    !IS_END() &&
	    (!IS_ARG() || IS_lex_state(EXPR_LABELED) || space_seen)) {
	    int token = heredoc_identifier(p);
	    if (token) return token < 0 ? 0 : token;
	}
	if (IS_AFTER_OPERATOR()) {
	    SET_LEX_STATE(EXPR_ARG);
	}
	else {
	    if (IS_lex_state(EXPR_CLASS))
		p->command_start = TRUE;
	    SET_LEX_STATE(EXPR_BEG);
	}
	if (c == '=') {
	    if ((c = nextc(p)) == '>') {
		return tCMP;
	    }
	    pushback(p, c);
	    return tLEQ;
	}
	if (c == '<') {
	    if ((c = nextc(p)) == '=') {
		set_yylval_id(idLTLT);
		SET_LEX_STATE(EXPR_BEG);
		return tOP_ASGN;
	    }
	    pushback(p, c);
	    return warn_balanced((enum ruby_method_ids)tLSHFT, "<<", "here document");
	}
	pushback(p, c);
	return '<';

      case '>':
	SET_LEX_STATE(IS_AFTER_OPERATOR() ? EXPR_ARG : EXPR_BEG);
	if ((c = nextc(p)) == '=') {
	    return tGEQ;
	}
	if (c == '>') {
	    if ((c = nextc(p)) == '=') {
		set_yylval_id(idGTGT);
		SET_LEX_STATE(EXPR_BEG);
		return tOP_ASGN;
	    }
	    pushback(p, c);
	    return tRSHFT;
	}
	pushback(p, c);
	return '>';

      case '"':
	label = (IS_LABEL_POSSIBLE() ? str_label : 0);
	p->lex.strterm = NEW_STRTERM(str_dquote | label, '"', 0);
	p->lex.ptok = p->lex.pcur-1;
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
	p->lex.strterm = NEW_STRTERM(str_xquote, '`', 0);
	return tXSTRING_BEG;

      case '\'':
	label = (IS_LABEL_POSSIBLE() ? str_label : 0);
	p->lex.strterm = NEW_STRTERM(str_squote | label, '\'', 0);
	p->lex.ptok = p->lex.pcur-1;
	return tSTRING_BEG;

      case '?':
	return parse_qmark(p, space_seen);

      case '&':
	if ((c = nextc(p)) == '&') {
	    SET_LEX_STATE(EXPR_BEG);
	    if ((c = nextc(p)) == '=') {
                set_yylval_id(idANDOP);
		SET_LEX_STATE(EXPR_BEG);
		return tOP_ASGN;
	    }
	    pushback(p, c);
	    return tANDOP;
	}
	else if (c == '=') {
            set_yylval_id('&');
	    SET_LEX_STATE(EXPR_BEG);
	    return tOP_ASGN;
	}
	else if (c == '.') {
	    set_yylval_id(idANDDOT);
	    SET_LEX_STATE(EXPR_DOT);
	    return tANDDOT;
	}
	pushback(p, c);
	if (IS_SPCARG(c)) {
	    if ((c != ':') ||
		(c = peekc_n(p, 1)) == -1 ||
		!(c == '\'' || c == '"' ||
		  is_identchar((p->lex.pcur+1), p->lex.pend, p->enc))) {
		rb_warning0("`&' interpreted as argument prefix");
	    }
	    c = tAMPER;
	}
	else if (IS_BEG()) {
	    c = tAMPER;
	}
	else {
	    c = warn_balanced('&', "&", "argument prefix");
	}
	SET_LEX_STATE(IS_AFTER_OPERATOR() ? EXPR_ARG : EXPR_BEG);
	return c;

      case '|':
	if ((c = nextc(p)) == '|') {
	    SET_LEX_STATE(EXPR_BEG);
	    if ((c = nextc(p)) == '=') {
                set_yylval_id(idOROP);
		SET_LEX_STATE(EXPR_BEG);
		return tOP_ASGN;
	    }
	    pushback(p, c);
	    if (IS_lex_state_for(last_state, EXPR_BEG)) {
		c = '|';
		pushback(p, '|');
		return c;
	    }
	    return tOROP;
	}
	if (c == '=') {
            set_yylval_id('|');
	    SET_LEX_STATE(EXPR_BEG);
	    return tOP_ASGN;
	}
	SET_LEX_STATE(IS_AFTER_OPERATOR() ? EXPR_ARG : EXPR_BEG|EXPR_LABEL);
	pushback(p, c);
	return '|';

      case '+':
	c = nextc(p);
	if (IS_AFTER_OPERATOR()) {
	    SET_LEX_STATE(EXPR_ARG);
	    if (c == '@') {
		return tUPLUS;
	    }
	    pushback(p, c);
	    return '+';
	}
	if (c == '=') {
            set_yylval_id('+');
	    SET_LEX_STATE(EXPR_BEG);
	    return tOP_ASGN;
	}
	if (IS_BEG() || (IS_SPCARG(c) && arg_ambiguous(p, '+'))) {
	    SET_LEX_STATE(EXPR_BEG);
	    pushback(p, c);
	    if (c != -1 && ISDIGIT(c)) {
		return parse_numeric(p, '+');
	    }
	    return tUPLUS;
	}
	SET_LEX_STATE(EXPR_BEG);
	pushback(p, c);
	return warn_balanced('+', "+", "unary operator");

      case '-':
	c = nextc(p);
	if (IS_AFTER_OPERATOR()) {
	    SET_LEX_STATE(EXPR_ARG);
	    if (c == '@') {
		return tUMINUS;
	    }
	    pushback(p, c);
	    return '-';
	}
	if (c == '=') {
            set_yylval_id('-');
	    SET_LEX_STATE(EXPR_BEG);
	    return tOP_ASGN;
	}
	if (c == '>') {
	    SET_LEX_STATE(EXPR_ENDFN);
	    return tLAMBDA;
	}
	if (IS_BEG() || (IS_SPCARG(c) && arg_ambiguous(p, '-'))) {
	    SET_LEX_STATE(EXPR_BEG);
	    pushback(p, c);
	    if (c != -1 && ISDIGIT(c)) {
		return tUMINUS_NUM;
	    }
	    return tUMINUS;
	}
	SET_LEX_STATE(EXPR_BEG);
	pushback(p, c);
	return warn_balanced('-', "-", "unary operator");

      case '.': {
        int is_beg = IS_BEG();
	SET_LEX_STATE(EXPR_BEG);
	if ((c = nextc(p)) == '.') {
	    if ((c = nextc(p)) == '.') {
		if (p->lex.paren_nest == 0 && looking_at_eol_p(p)) {
		    rb_warn0("... at EOL, should be parenthesized?");
		}
		else if (p->lex.lpar_beg >= 0 && p->lex.lpar_beg+1 == p->lex.paren_nest) {
		    if (IS_lex_state_for(last_state, EXPR_LABEL))
			return tDOT3;
		}
		return is_beg ? tBDOT3 : tDOT3;
	    }
	    pushback(p, c);
	    return is_beg ? tBDOT2 : tDOT2;
	}
	pushback(p, c);
	if (c != -1 && ISDIGIT(c)) {
	    char prev = p->lex.pcur-1 > p->lex.pbeg ? *(p->lex.pcur-2) : 0;
	    parse_numeric(p, '.');
	    if (ISDIGIT(prev)) {
		yyerror0("unexpected fraction part after numeric literal");
	    }
	    else {
		yyerror0("no .<digit> floating literal anymore; put 0 before dot");
	    }
	    SET_LEX_STATE(EXPR_END);
	    p->lex.ptok = p->lex.pcur;
	    goto retry;
	}
	set_yylval_id('.');
	SET_LEX_STATE(EXPR_DOT);
	return '.';
      }

      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
	return parse_numeric(p, c);

      case ')':
	COND_POP();
	CMDARG_POP();
	SET_LEX_STATE(EXPR_ENDFN);
	p->lex.paren_nest--;
	return c;

      case ']':
	COND_POP();
	CMDARG_POP();
	SET_LEX_STATE(EXPR_END);
	p->lex.paren_nest--;
	return c;

      case '}':
	/* tSTRING_DEND does COND_POP and CMDARG_POP in the yacc's rule */
	if (!p->lex.brace_nest--) return tSTRING_DEND;
	COND_POP();
	CMDARG_POP();
	SET_LEX_STATE(EXPR_END);
	p->lex.paren_nest--;
	return c;

      case ':':
	c = nextc(p);
	if (c == ':') {
	    if (IS_BEG() || IS_lex_state(EXPR_CLASS) || IS_SPCARG(-1)) {
		SET_LEX_STATE(EXPR_BEG);
		return tCOLON3;
	    }
	    set_yylval_id(idCOLON2);
	    SET_LEX_STATE(EXPR_DOT);
	    return tCOLON2;
	}
	if (IS_END() || ISSPACE(c) || c == '#') {
	    pushback(p, c);
	    c = warn_balanced(':', ":", "symbol literal");
	    SET_LEX_STATE(EXPR_BEG);
	    return c;
	}
	switch (c) {
	  case '\'':
	    p->lex.strterm = NEW_STRTERM(str_ssym, c, 0);
	    break;
	  case '"':
	    p->lex.strterm = NEW_STRTERM(str_dsym, c, 0);
	    break;
	  default:
	    pushback(p, c);
	    break;
	}
	SET_LEX_STATE(EXPR_FNAME);
	return tSYMBEG;

      case '/':
	if (IS_BEG()) {
	    p->lex.strterm = NEW_STRTERM(str_regexp, '/', 0);
	    return tREGEXP_BEG;
	}
	if ((c = nextc(p)) == '=') {
            set_yylval_id('/');
	    SET_LEX_STATE(EXPR_BEG);
	    return tOP_ASGN;
	}
	pushback(p, c);
	if (IS_SPCARG(c)) {
	    arg_ambiguous(p, '/');
	    p->lex.strterm = NEW_STRTERM(str_regexp, '/', 0);
	    return tREGEXP_BEG;
	}
	SET_LEX_STATE(IS_AFTER_OPERATOR() ? EXPR_ARG : EXPR_BEG);
	return warn_balanced('/', "/", "regexp literal");

      case '^':
	if ((c = nextc(p)) == '=') {
            set_yylval_id('^');
	    SET_LEX_STATE(EXPR_BEG);
	    return tOP_ASGN;
	}
	SET_LEX_STATE(IS_AFTER_OPERATOR() ? EXPR_ARG : EXPR_BEG);
	pushback(p, c);
	return '^';

      case ';':
	SET_LEX_STATE(EXPR_BEG);
	p->command_start = TRUE;
	return ';';

      case ',':
	SET_LEX_STATE(EXPR_BEG|EXPR_LABEL);
	return ',';

      case '~':
	if (IS_AFTER_OPERATOR()) {
	    if ((c = nextc(p)) != '@') {
		pushback(p, c);
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
	else if (!space_seen) {
	    /* foo( ... ) => method call, no ambiguity */
	}
	else if (IS_ARG() || IS_lex_state_all(EXPR_END|EXPR_LABEL)) {
	    c = tLPAREN_ARG;
	}
	else if (IS_lex_state(EXPR_ENDFN) && !lambda_beginning_p()) {
	    rb_warning0("parentheses after method name is interpreted as "
			"an argument list, not a decomposed argument");
	}
	p->lex.paren_nest++;
	COND_PUSH(0);
	CMDARG_PUSH(0);
	SET_LEX_STATE(EXPR_BEG|EXPR_LABEL);
	return c;

      case '[':
	p->lex.paren_nest++;
	if (IS_AFTER_OPERATOR()) {
	    if ((c = nextc(p)) == ']') {
		SET_LEX_STATE(EXPR_ARG);
		if ((c = nextc(p)) == '=') {
		    return tASET;
		}
		pushback(p, c);
		return tAREF;
	    }
	    pushback(p, c);
	    SET_LEX_STATE(EXPR_ARG|EXPR_LABEL);
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
	++p->lex.brace_nest;
	if (lambda_beginning_p())
	    c = tLAMBEG;
	else if (IS_lex_state(EXPR_LABELED))
	    c = tLBRACE;      /* hash */
	else if (IS_lex_state(EXPR_ARG_ANY | EXPR_END | EXPR_ENDFN))
	    c = '{';          /* block (primary) */
	else if (IS_lex_state(EXPR_ENDARG))
	    c = tLBRACE_ARG;  /* block (expr) */
	else
	    c = tLBRACE;      /* hash */
	if (c != tLBRACE) {
	    p->command_start = TRUE;
	    SET_LEX_STATE(EXPR_BEG);
	}
	else {
	    SET_LEX_STATE(EXPR_BEG|EXPR_LABEL);
	}
	++p->lex.paren_nest;  /* after lambda_beginning_p() */
	COND_PUSH(0);
	CMDARG_PUSH(0);
	return c;

      case '\\':
	c = nextc(p);
	if (c == '\n') {
	    space_seen = 1;
	    dispatch_scan_event(p, tSP);
	    goto retry; /* skip \\n */
	}
	if (c == ' ') return tSP;
	if (ISSPACE(c)) return c;
	pushback(p, c);
	return '\\';

      case '%':
	return parse_percent(p, space_seen, last_state);

      case '$':
	return parse_gvar(p, last_state);

      case '@':
	return parse_atmark(p, last_state);

      case '_':
	if (was_bol(p) && whole_match_p(p, "__END__", 7, 0)) {
	    p->ruby__end__seen = 1;
	    p->eofp = 1;
#ifndef RIPPER
	    return -1;
#else
            lex_goto_eol(p);
            dispatch_scan_event(p, k__END__);
            return 0;
#endif
	}
	newtok(p);
	break;

      default:
	if (!parser_is_identchar(p)) {
	    compile_error(p, "Invalid char `\\x%02X' in expression", c);
            token_flush(p);
	    goto retry;
	}

	newtok(p);
	break;
    }

    return parse_ident(p, c, cmd_state);
}

static enum yytokentype
yylex(YYSTYPE *lval, YYLTYPE *yylloc, struct parser_params *p)
{
    enum yytokentype t;

    p->lval = lval;
    lval->val = Qundef;
    t = parser_yylex(p);

    if (p->lex.strterm && (p->lex.strterm->flags & STRTERM_HEREDOC))
	RUBY_SET_YYLLOC_FROM_STRTERM_HEREDOC(*yylloc);
    else
	RUBY_SET_YYLLOC(*yylloc);

    if (has_delayed_token(p))
	dispatch_delayed_token(p, t);
    else if (t != 0)
	dispatch_scan_event(p, t);

    return t;
}

#define LVAR_USED ((ID)1 << (sizeof(ID) * CHAR_BIT - 1))

static NODE*
node_newnode(struct parser_params *p, enum node_type type, VALUE a0, VALUE a1, VALUE a2, const rb_code_location_t *loc)
{
    NODE *n = rb_ast_newnode(p->ast, type);

    rb_node_init(n, type, a0, a1, a2);

    nd_set_loc(n, loc);
    nd_set_node_id(n, parser_get_node_id(p));
    return n;
}

static NODE *
nd_set_loc(NODE *nd, const YYLTYPE *loc)
{
    nd->nd_loc = *loc;
    nd_set_line(nd, loc->beg_pos.lineno);
    return nd;
}

#ifndef RIPPER
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
    nd_set_line(node, nd_line(orig));
}

static void
parser_warning(struct parser_params *p, NODE *node, const char *mesg)
{
    rb_compile_warning(p->ruby_sourcefile, nd_line(node), "%s", mesg);
}

static void
parser_warn(struct parser_params *p, NODE *node, const char *mesg)
{
    rb_compile_warn(p->ruby_sourcefile, nd_line(node), "%s", mesg);
}

static NODE*
block_append(struct parser_params *p, NODE *head, NODE *tail)
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
	parser_warning(p, h, "unused literal ignored");
	return tail;
      default:
	h = end = NEW_BLOCK(head, &head->nd_loc);
	end->nd_end = end;
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
	    parser_warning(p, tail, "statement not reached");
	}
	break;

      default:
	break;
    }

    if (nd_type(tail) != NODE_BLOCK) {
	tail = NEW_BLOCK(tail, &tail->nd_loc);
	tail->nd_end = tail;
    }
    end->nd_next = tail;
    h->nd_end = tail->nd_end;
    nd_set_last_loc(head, nd_last_loc(tail));
    return head;
}

/* append item to the list */
static NODE*
list_append(struct parser_params *p, NODE *list, NODE *item)
{
    NODE *last;

    if (list == 0) return NEW_LIST(item, &item->nd_loc);
    if (list->nd_next) {
	last = list->nd_next->nd_end;
    }
    else {
	last = list;
    }

    list->nd_alen += 1;
    last->nd_next = NEW_LIST(item, &item->nd_loc);
    list->nd_next->nd_end = last->nd_next;

    nd_set_last_loc(list, nd_last_loc(item));

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

    nd_set_last_loc(head, nd_last_loc(tail));

    return head;
}

static int
literal_concat0(struct parser_params *p, VALUE head, VALUE tail)
{
    if (NIL_P(tail)) return 1;
    if (!rb_enc_compatible(head, tail)) {
	compile_error(p, "string literal encodings differ (%s / %s)",
		      rb_enc_name(rb_enc_get(head)),
		      rb_enc_name(rb_enc_get(tail)));
	rb_str_resize(head, 0);
	rb_str_resize(tail, 0);
	return 0;
    }
    rb_str_buf_append(head, tail);
    return 1;
}

static VALUE
string_literal_head(enum node_type htype, NODE *head)
{
    if (htype != NODE_DSTR) return Qfalse;
    if (head->nd_next) {
	head = head->nd_next->nd_end->nd_head;
	if (!head || nd_type(head) != NODE_STR) return Qfalse;
    }
    const VALUE lit = head->nd_lit;
    ASSUME(lit != Qfalse);
    return lit;
}

/* concat two string literals */
static NODE *
literal_concat(struct parser_params *p, NODE *head, NODE *tail, const YYLTYPE *loc)
{
    enum node_type htype;
    VALUE lit;

    if (!head) return tail;
    if (!tail) return head;

    htype = nd_type(head);
    if (htype == NODE_EVSTR) {
	head = new_dstr(p, head, loc);
	htype = NODE_DSTR;
    }
    if (p->heredoc_indent > 0) {
	switch (htype) {
	  case NODE_STR:
	    nd_set_type(head, NODE_DSTR);
	  case NODE_DSTR:
	    return list_append(p, head, tail);
	  default:
	    break;
	}
    }
    switch (nd_type(tail)) {
      case NODE_STR:
	if ((lit = string_literal_head(htype, head)) != Qfalse) {
	    htype = NODE_STR;
	}
	else {
	    lit = head->nd_lit;
	}
	if (htype == NODE_STR) {
	    if (!literal_concat0(p, lit, tail->nd_lit)) {
	      error:
		rb_discard_node(p, head);
		rb_discard_node(p, tail);
		return 0;
	    }
	    rb_discard_node(p, tail);
	}
	else {
	    list_append(p, head, tail);
	}
	break;

      case NODE_DSTR:
	if (htype == NODE_STR) {
	    if (!literal_concat0(p, head->nd_lit, tail->nd_lit))
		goto error;
	    tail->nd_lit = head->nd_lit;
	    rb_discard_node(p, head);
	    head = tail;
	}
	else if (NIL_P(tail->nd_lit)) {
	  append:
	    head->nd_alen += tail->nd_alen - 1;
	    if (!head->nd_next) {
		head->nd_next = tail->nd_next;
	    }
	    else if (tail->nd_next) {
		head->nd_next->nd_end->nd_next = tail->nd_next;
		head->nd_next->nd_end = tail->nd_next->nd_end;
	    }
	    rb_discard_node(p, tail);
	}
	else if ((lit = string_literal_head(htype, head)) != Qfalse) {
	    if (!literal_concat0(p, lit, tail->nd_lit))
		goto error;
	    tail->nd_lit = Qnil;
	    goto append;
	}
	else {
	    list_concat(head, NEW_NODE(NODE_LIST, NEW_STR(tail->nd_lit, loc), tail->nd_alen, tail->nd_next, loc));
	}
	break;

      case NODE_EVSTR:
	if (htype == NODE_STR) {
	    nd_set_type(head, NODE_DSTR);
	    head->nd_alen = 1;
	}
	list_append(p, head, tail);
	break;
    }
    return head;
}

static NODE *
evstr2dstr(struct parser_params *p, NODE *node)
{
    if (nd_type(node) == NODE_EVSTR) {
	node = new_dstr(p, node, &node->nd_loc);
    }
    return node;
}

static NODE *
new_evstr(struct parser_params *p, NODE *node, const YYLTYPE *loc)
{
    NODE *head = node;

    if (node) {
	switch (nd_type(node)) {
	  case NODE_STR:
	    nd_set_type(node, NODE_DSTR);
	  case NODE_DSTR: case NODE_EVSTR:
	    return node;
	}
    }
    return NEW_EVSTR(head, loc);
}

static NODE *
new_dstr(struct parser_params *p, NODE *node, const YYLTYPE *loc)
{
    VALUE lit = STR_NEW0();
    NODE *dstr = NEW_DSTR(lit, loc);
    RB_OBJ_WRITTEN(p->ast, Qnil, lit);
    return list_append(p, dstr, node);
}

static NODE *
call_bin_op(struct parser_params *p, NODE *recv, ID id, NODE *arg1,
		const YYLTYPE *op_loc, const YYLTYPE *loc)
{
    NODE *expr;
    value_expr(recv);
    value_expr(arg1);
    expr = NEW_OPCALL(recv, id, NEW_LIST(arg1, &arg1->nd_loc), loc);
    nd_set_line(expr, op_loc->beg_pos.lineno);
    return expr;
}

static NODE *
call_uni_op(struct parser_params *p, NODE *recv, ID id, const YYLTYPE *op_loc, const YYLTYPE *loc)
{
    NODE *opcall;
    value_expr(recv);
    opcall = NEW_OPCALL(recv, id, 0, loc);
    nd_set_line(opcall, op_loc->beg_pos.lineno);
    return opcall;
}

static NODE *
new_qcall(struct parser_params* p, ID atype, NODE *recv, ID mid, NODE *args, const YYLTYPE *op_loc, const YYLTYPE *loc)
{
    NODE *qcall = NEW_QCALL(atype, recv, mid, args, loc);
    nd_set_line(qcall, op_loc->beg_pos.lineno);
    return qcall;
}

static NODE*
new_command_qcall(struct parser_params* p, ID atype, NODE *recv, ID mid, NODE *args, NODE *block, const YYLTYPE *op_loc, const YYLTYPE *loc)
{
    NODE *ret;
    if (block) block_dup_check(p, args, block);
    ret = new_qcall(p, atype, recv, mid, args, op_loc, loc);
    if (block) ret = method_add_block(p, ret, block, loc);
    fixpos(ret, recv);
    return ret;
}

#define nd_once_body(node) (nd_type(node) == NODE_ONCE ? (node)->nd_body : node)
static NODE*
match_op(struct parser_params *p, NODE *node1, NODE *node2, const YYLTYPE *op_loc, const YYLTYPE *loc)
{
    NODE *n;
    int line = op_loc->beg_pos.lineno;

    value_expr(node1);
    value_expr(node2);
    if (node1 && (n = nd_once_body(node1)) != 0) {
	switch (nd_type(n)) {
	  case NODE_DREGX:
	    {
		NODE *match = NEW_MATCH2(node1, node2, loc);
		nd_set_line(match, line);
		return match;
	    }

	  case NODE_LIT:
	    if (RB_TYPE_P(n->nd_lit, T_REGEXP)) {
		const VALUE lit = n->nd_lit;
		NODE *match = NEW_MATCH2(node1, node2, loc);
		match->nd_args = reg_named_capture_assign(p, lit, loc);
		nd_set_line(match, line);
		return match;
	    }
	}
    }

    if (node2 && (n = nd_once_body(node2)) != 0) {
        NODE *match3;

	switch (nd_type(n)) {
	  case NODE_LIT:
	    if (!RB_TYPE_P(n->nd_lit, T_REGEXP)) break;
	    /* fallthru */
	  case NODE_DREGX:
	    match3 = NEW_MATCH3(node2, node1, loc);
	    return match3;
	}
    }

    n = NEW_CALL(node1, tMATCH, NEW_LIST(node2, &node2->nd_loc), loc);
    nd_set_line(n, line);
    return n;
}

# if WARN_PAST_SCOPE
static int
past_dvar_p(struct parser_params *p, ID id)
{
    struct vtable *past = p->lvtbl->past;
    while (past) {
	if (vtable_included(past, id)) return 1;
	past = past->prev;
    }
    return 0;
}
# endif

static int
numparam_nested_p(struct parser_params *p)
{
    struct local_vars *local = p->lvtbl;
    NODE *outer = local->numparam.outer;
    NODE *inner = local->numparam.inner;
    if (outer || inner) {
	NODE *used = outer ? outer : inner;
	compile_error(p, "numbered parameter is already used in\n"
		      "%s:%d: %s block here",
		      p->ruby_sourcefile, nd_line(used),
		      outer ? "outer" : "inner");
	parser_show_error_line(p, &used->nd_loc);
	return 1;
    }
    return 0;
}

static NODE*
gettable(struct parser_params *p, ID id, const YYLTYPE *loc)
{
    ID *vidp = NULL;
    NODE *node;
    switch (id) {
      case keyword_self:
	return NEW_SELF(loc);
      case keyword_nil:
	return NEW_NIL(loc);
      case keyword_true:
	return NEW_TRUE(loc);
      case keyword_false:
	return NEW_FALSE(loc);
      case keyword__FILE__:
	{
	    VALUE file = p->ruby_sourcefile_string;
	    if (NIL_P(file))
		file = rb_str_new(0, 0);
	    else
		file = rb_str_dup(file);
	    node = NEW_STR(file, loc);
            RB_OBJ_WRITTEN(p->ast, Qnil, file);
	}
	return node;
      case keyword__LINE__:
	return NEW_LIT(INT2FIX(p->tokline), loc);
      case keyword__ENCODING__:
        node = NEW_LIT(rb_enc_from_encoding(p->enc), loc);
        RB_OBJ_WRITTEN(p->ast, Qnil, node->nd_lit);
        return node;

    }
    switch (id_type(id)) {
      case ID_LOCAL:
	if (dyna_in_block(p) && dvar_defined_ref(p, id, &vidp)) {
	    if (NUMPARAM_ID_P(id) && numparam_nested_p(p)) return 0;
	    if (id == p->cur_arg) {
                compile_error(p, "circular argument reference - %"PRIsWARN, rb_id2str(id));
                return 0;
	    }
	    if (vidp) *vidp |= LVAR_USED;
	    node = NEW_DVAR(id, loc);
	    return node;
	}
	if (local_id_ref(p, id, &vidp)) {
	    if (id == p->cur_arg) {
                compile_error(p, "circular argument reference - %"PRIsWARN, rb_id2str(id));
                return 0;
	    }
	    if (vidp) *vidp |= LVAR_USED;
	    node = NEW_LVAR(id, loc);
	    return node;
	}
	if (dyna_in_block(p) && NUMPARAM_ID_P(id) &&
	    parser_numbered_param(p, NUMPARAM_ID_TO_IDX(id))) {
	    if (numparam_nested_p(p)) return 0;
	    node = NEW_DVAR(id, loc);
	    struct local_vars *local = p->lvtbl;
	    if (!local->numparam.current) local->numparam.current = node;
	    return node;
	}
# if WARN_PAST_SCOPE
	if (!p->ctxt.in_defined && RTEST(ruby_verbose) && past_dvar_p(p, id)) {
	    rb_warning1("possible reference to past scope - %"PRIsWARN, rb_id2str(id));
	}
# endif
	/* method call without arguments */
	return NEW_VCALL(id, loc);
      case ID_GLOBAL:
	return NEW_GVAR(id, loc);
      case ID_INSTANCE:
	return NEW_IVAR(id, loc);
      case ID_CONST:
	return NEW_CONST(id, loc);
      case ID_CLASS:
	return NEW_CVAR(id, loc);
    }
    compile_error(p, "identifier %"PRIsVALUE" is not valid to get", rb_id2str(id));
    return 0;
}

static NODE *
opt_arg_append(NODE *opt_list, NODE *opt)
{
    NODE *opts = opt_list;
    opts->nd_loc.end_pos = opt->nd_loc.end_pos;

    while (opts->nd_next) {
	opts = opts->nd_next;
	opts->nd_loc.end_pos = opt->nd_loc.end_pos;
    }
    opts->nd_next = opt;

    return opt_list;
}

static NODE *
kwd_append(NODE *kwlist, NODE *kw)
{
    if (kwlist) {
	NODE *kws = kwlist;
	kws->nd_loc.end_pos = kw->nd_loc.end_pos;
	while (kws->nd_next) {
	    kws = kws->nd_next;
	    kws->nd_loc.end_pos = kw->nd_loc.end_pos;
	}
	kws->nd_next = kw;
    }
    return kwlist;
}

static NODE *
new_defined(struct parser_params *p, NODE *expr, const YYLTYPE *loc)
{
    return NEW_DEFINED(remove_begin_all(expr), loc);
}

static NODE*
symbol_append(struct parser_params *p, NODE *symbols, NODE *symbol)
{
    if (nd_type(symbol) == NODE_DSTR) {
	nd_set_type(symbol, NODE_DSYM);
    }
    else {
	nd_set_type(symbol, NODE_LIT);
	RB_OBJ_WRITTEN(p->ast, Qnil, symbol->nd_lit = rb_str_intern(symbol->nd_lit));
    }
    return list_append(p, symbols, symbol);
}

static NODE *
new_regexp(struct parser_params *p, NODE *node, int options, const YYLTYPE *loc)
{
    NODE *list, *prev;
    VALUE lit;

    if (!node) {
	node = NEW_LIT(reg_compile(p, STR_NEW0(), options), loc);
	RB_OBJ_WRITTEN(p->ast, Qnil, node->nd_lit);
        return node;
    }
    switch (nd_type(node)) {
      case NODE_STR:
	{
	    VALUE src = node->nd_lit;
	    nd_set_type(node, NODE_LIT);
	    nd_set_loc(node, loc);
	    RB_OBJ_WRITTEN(p->ast, Qnil, node->nd_lit = reg_compile(p, src, options));
	}
	break;
      default:
	lit = STR_NEW0();
	node = NEW_NODE(NODE_DSTR, lit, 1, NEW_LIST(node, loc), loc);
        RB_OBJ_WRITTEN(p->ast, Qnil, lit);
	/* fall through */
      case NODE_DSTR:
	nd_set_type(node, NODE_DREGX);
	nd_set_loc(node, loc);
	node->nd_cflag = options & RE_OPTION_MASK;
	if (!NIL_P(node->nd_lit)) reg_fragment_check(p, node->nd_lit, options);
	for (list = (prev = node)->nd_next; list; list = list->nd_next) {
	    NODE *frag = list->nd_head;
	    enum node_type type = nd_type(frag);
	    if (type == NODE_STR || (type == NODE_DSTR && !frag->nd_next)) {
		VALUE tail = frag->nd_lit;
		if (reg_fragment_check(p, tail, options) && prev && !NIL_P(prev->nd_lit)) {
		    VALUE lit = prev == node ? prev->nd_lit : prev->nd_head->nd_lit;
		    if (!literal_concat0(p, lit, tail)) {
			return NEW_NIL(loc); /* dummy node on error */
		    }
		    rb_str_resize(tail, 0);
		    prev->nd_next = list->nd_next;
		    rb_discard_node(p, list->nd_head);
		    rb_discard_node(p, list);
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
	    RB_OBJ_WRITTEN(p->ast, Qnil, node->nd_lit = reg_compile(p, src, options));
	}
	if (options & RE_OPTION_ONCE) {
	    node = NEW_NODE(NODE_ONCE, 0, node, 0, loc);
	}
	break;
    }
    return node;
}

static NODE *
new_kw_arg(struct parser_params *p, NODE *k, const YYLTYPE *loc)
{
    if (!k) return 0;
    return NEW_KW_ARG(0, (k), loc);
}

static NODE *
new_xstring(struct parser_params *p, NODE *node, const YYLTYPE *loc)
{
    if (!node) {
	VALUE lit = STR_NEW0();
	NODE *xstr = NEW_XSTR(lit, loc);
	RB_OBJ_WRITTEN(p->ast, Qnil, lit);
	return xstr;
    }
    switch (nd_type(node)) {
      case NODE_STR:
	nd_set_type(node, NODE_XSTR);
	nd_set_loc(node, loc);
	break;
      case NODE_DSTR:
	nd_set_type(node, NODE_DXSTR);
	nd_set_loc(node, loc);
	break;
      default:
	node = NEW_NODE(NODE_DXSTR, Qnil, 1, NEW_LIST(node, loc), loc);
	break;
    }
    return node;
}

static void
check_literal_when(struct parser_params *p, NODE *arg, const YYLTYPE *loc)
{
    VALUE lit;

    if (!arg || !p->case_labels) return;

    lit = rb_node_case_when_optimizable_literal(arg);
    if (lit == Qundef) return;
    if (nd_type(arg) == NODE_STR) {
	RB_OBJ_WRITTEN(p->ast, Qnil, arg->nd_lit = lit);
    }

    if (NIL_P(p->case_labels)) {
	p->case_labels = rb_obj_hide(rb_hash_new());
    }
    else {
	VALUE line = rb_hash_lookup(p->case_labels, lit);
	if (!NIL_P(line)) {
	    rb_warning1("duplicated `when' clause with line %d is ignored",
			WARN_IVAL(line));
	    return;
	}
    }
    rb_hash_aset(p->case_labels, lit, INT2NUM(p->ruby_sourceline));
}

#else  /* !RIPPER */
static int
id_is_var(struct parser_params *p, ID id)
{
    if (is_notop_id(id)) {
	switch (id & ID_SCOPE_MASK) {
	  case ID_GLOBAL: case ID_INSTANCE: case ID_CONST: case ID_CLASS:
	    return 1;
	  case ID_LOCAL:
	    if (dyna_in_block(p)) {
		if (NUMPARAM_ID_P(id) || dvar_defined(p, id)) return 1;
	    }
	    if (local_id(p, id)) return 1;
	    /* method call without arguments */
	    return 0;
	}
    }
    compile_error(p, "identifier %"PRIsVALUE" is not valid to get", rb_id2str(id));
    return 0;
}

static VALUE
new_regexp(struct parser_params *p, VALUE re, VALUE opt, const YYLTYPE *loc)
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
    if (src && NIL_P(parser_reg_compile(p, src, options, &err))) {
	compile_error(p, "%"PRIsVALUE, err);
    }
    return dispatch2(regexp_literal, re, opt);
}
#endif /* !RIPPER */


#ifndef RIPPER
static const char rb_parser_lex_state_names[][8] = {
    "BEG",    "END",    "ENDARG", "ENDFN",  "ARG",
    "CMDARG", "MID",    "FNAME",  "DOT",    "CLASS",
    "LABEL",  "LABELED","FITEM",
};

static VALUE
append_lex_state_name(enum lex_state_e state, VALUE buf)
{
    int i, sep = 0;
    unsigned int mask = 1;
    static const char none[] = "NONE";

    for (i = 0; i < EXPR_MAX_STATE; ++i, mask <<= 1) {
	if ((unsigned)state & mask) {
	    if (sep) {
		rb_str_cat(buf, "|", 1);
	    }
	    sep = 1;
	    rb_str_cat_cstr(buf, rb_parser_lex_state_names[i]);
	}
    }
    if (!sep) {
	rb_str_cat(buf, none, sizeof(none)-1);
    }
    return buf;
}

static void
flush_debug_buffer(struct parser_params *p, VALUE out, VALUE str)
{
    VALUE mesg = p->debug_buffer;

    if (!NIL_P(mesg) && RSTRING_LEN(mesg)) {
	p->debug_buffer = Qnil;
	rb_io_puts(1, &mesg, out);
    }
    if (!NIL_P(str) && RSTRING_LEN(str)) {
	rb_io_write(p->debug_output, str);
    }
}

enum lex_state_e
rb_parser_trace_lex_state(struct parser_params *p, enum lex_state_e from,
			  enum lex_state_e to, int line)
{
    VALUE mesg;
    mesg = rb_str_new_cstr("lex_state: ");
    append_lex_state_name(from, mesg);
    rb_str_cat_cstr(mesg, " -> ");
    append_lex_state_name(to, mesg);
    rb_str_catf(mesg, " at line %d\n", line);
    flush_debug_buffer(p, p->debug_output, mesg);
    return to;
}

VALUE
rb_parser_lex_state_name(enum lex_state_e state)
{
    return rb_fstring(append_lex_state_name(state, rb_str_new(0, 0)));
}

static void
append_bitstack_value(stack_type stack, VALUE mesg)
{
    if (stack == 0) {
	rb_str_cat_cstr(mesg, "0");
    }
    else {
	stack_type mask = (stack_type)1U << (CHAR_BIT * sizeof(stack_type) - 1);
	for (; mask && !(stack & mask); mask >>= 1) continue;
	for (; mask; mask >>= 1) rb_str_cat(mesg, stack & mask ? "1" : "0", 1);
    }
}

void
rb_parser_show_bitstack(struct parser_params *p, stack_type stack,
			const char *name, int line)
{
    VALUE mesg = rb_sprintf("%s: ", name);
    append_bitstack_value(stack, mesg);
    rb_str_catf(mesg, " at line %d\n", line);
    flush_debug_buffer(p, p->debug_output, mesg);
}

void
rb_parser_fatal(struct parser_params *p, const char *fmt, ...)
{
    va_list ap;
    VALUE mesg = rb_str_new_cstr("internal parser error: ");

    va_start(ap, fmt);
    rb_str_vcatf(mesg, fmt, ap);
    va_end(ap);
    parser_yyerror(p, NULL, RSTRING_PTR(mesg));
    RB_GC_GUARD(mesg);

    mesg = rb_str_new(0, 0);
    append_lex_state_name(p->lex.state, mesg);
    compile_error(p, "lex.state: %"PRIsVALUE, mesg);
    rb_str_resize(mesg, 0);
    append_bitstack_value(p->cond_stack, mesg);
    compile_error(p, "cond_stack: %"PRIsVALUE, mesg);
    rb_str_resize(mesg, 0);
    append_bitstack_value(p->cmdarg_stack, mesg);
    compile_error(p, "cmdarg_stack: %"PRIsVALUE, mesg);
    if (p->debug_output == rb_ractor_stdout())
	p->debug_output = rb_ractor_stderr();
    p->debug = TRUE;
}

YYLTYPE *
rb_parser_set_location_from_strterm_heredoc(struct parser_params *p, rb_strterm_heredoc_t *here, YYLTYPE *yylloc)
{
    int sourceline = here->sourceline;
    int beg_pos = (int)here->offset - here->quote
	- (rb_strlen_lit("<<-") - !(here->func & STR_FUNC_INDENT));
    int end_pos = (int)here->offset + here->length + here->quote;

    yylloc->beg_pos.lineno = sourceline;
    yylloc->beg_pos.column = beg_pos;
    yylloc->end_pos.lineno = sourceline;
    yylloc->end_pos.column = end_pos;
    return yylloc;
}

YYLTYPE *
rb_parser_set_location_of_none(struct parser_params *p, YYLTYPE *yylloc)
{
    yylloc->beg_pos.lineno = p->ruby_sourceline;
    yylloc->beg_pos.column = (int)(p->lex.ptok - p->lex.pbeg);
    yylloc->end_pos.lineno = p->ruby_sourceline;
    yylloc->end_pos.column = (int)(p->lex.ptok - p->lex.pbeg);
    return yylloc;
}

YYLTYPE *
rb_parser_set_location(struct parser_params *p, YYLTYPE *yylloc)
{
    yylloc->beg_pos.lineno = p->ruby_sourceline;
    yylloc->beg_pos.column = (int)(p->lex.ptok - p->lex.pbeg);
    yylloc->end_pos.lineno = p->ruby_sourceline;
    yylloc->end_pos.column = (int)(p->lex.pcur - p->lex.pbeg);
    return yylloc;
}
#endif /* !RIPPER */

static void
parser_token_value_print(struct parser_params *p, enum yytokentype type, const YYSTYPE *valp)
{
    VALUE v;

    switch (type) {
      case tIDENTIFIER: case tFID: case tGVAR: case tIVAR:
      case tCONSTANT: case tCVAR: case tLABEL: case tOP_ASGN:
#ifndef RIPPER
	v = rb_id2str(valp->id);
#else
	v = valp->node->nd_rval;
#endif
	rb_parser_printf(p, "%"PRIsVALUE, v);
	break;
      case tINTEGER: case tFLOAT: case tRATIONAL: case tIMAGINARY:
      case tSTRING_CONTENT: case tCHAR:
#ifndef RIPPER
	v = valp->node->nd_lit;
#else
	v = valp->val;
#endif
	rb_parser_printf(p, "%+"PRIsVALUE, v);
	break;
      case tNTH_REF:
#ifndef RIPPER
	rb_parser_printf(p, "$%ld", valp->node->nd_nth);
#else
	rb_parser_printf(p, "%"PRIsVALUE, valp->val);
#endif
	break;
      case tBACK_REF:
#ifndef RIPPER
	rb_parser_printf(p, "$%c", (int)valp->node->nd_nth);
#else
	rb_parser_printf(p, "%"PRIsVALUE, valp->val);
#endif
	break;
      default:
	break;
    }
}

static int
assignable0(struct parser_params *p, ID id, const char **err)
{
    if (!id) return -1;
    switch (id) {
      case keyword_self:
	*err = "Can't change the value of self";
	return -1;
      case keyword_nil:
	*err = "Can't assign to nil";
	return -1;
      case keyword_true:
	*err = "Can't assign to true";
	return -1;
      case keyword_false:
	*err = "Can't assign to false";
	return -1;
      case keyword__FILE__:
	*err = "Can't assign to __FILE__";
	return -1;
      case keyword__LINE__:
	*err = "Can't assign to __LINE__";
	return -1;
      case keyword__ENCODING__:
	*err = "Can't assign to __ENCODING__";
	return -1;
    }
    switch (id_type(id)) {
      case ID_LOCAL:
	if (dyna_in_block(p)) {
	    if (p->max_numparam > NO_PARAM && NUMPARAM_ID_P(id)) {
		compile_error(p, "Can't assign to numbered parameter _%d",
			      NUMPARAM_ID_TO_IDX(id));
		return -1;
	    }
	    if (dvar_curr(p, id)) return NODE_DASGN_CURR;
	    if (dvar_defined(p, id)) return NODE_DASGN;
	    if (local_id(p, id)) return NODE_LASGN;
	    dyna_var(p, id);
	    return NODE_DASGN_CURR;
	}
	else {
	    if (!local_id(p, id)) local_var(p, id);
	    return NODE_LASGN;
	}
	break;
      case ID_GLOBAL: return NODE_GASGN;
      case ID_INSTANCE: return NODE_IASGN;
      case ID_CONST:
	if (!p->ctxt.in_def) return NODE_CDECL;
	*err = "dynamic constant assignment";
	return -1;
      case ID_CLASS: return NODE_CVASGN;
      default:
	compile_error(p, "identifier %"PRIsVALUE" is not valid to set", rb_id2str(id));
    }
    return -1;
}

#ifndef RIPPER
static NODE*
assignable(struct parser_params *p, ID id, NODE *val, const YYLTYPE *loc)
{
    const char *err = 0;
    int node_type = assignable0(p, id, &err);
    switch (node_type) {
      case NODE_DASGN_CURR: return NEW_DASGN_CURR(id, val, loc);
      case NODE_DASGN: return NEW_DASGN(id, val, loc);
      case NODE_LASGN: return NEW_LASGN(id, val, loc);
      case NODE_GASGN: return NEW_GASGN(id, val, loc);
      case NODE_IASGN: return NEW_IASGN(id, val, loc);
      case NODE_CDECL: return NEW_CDECL(id, val, 0, loc);
      case NODE_CVASGN: return NEW_CVASGN(id, val, loc);
    }
    if (err) yyerror1(loc, err);
    return NEW_BEGIN(0, loc);
}
#else
static VALUE
assignable(struct parser_params *p, VALUE lhs)
{
    const char *err = 0;
    assignable0(p, get_id(lhs), &err);
    if (err) lhs = assign_error(p, lhs);
    return lhs;
}
#endif

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

static int
shadowing_lvar_0(struct parser_params *p, ID name)
{
    if (is_private_local_id(name)) return 1;
    if (dyna_in_block(p)) {
	if (dvar_curr(p, name)) {
	    yyerror0("duplicated argument name");
	}
	else if (dvar_defined(p, name) || local_id(p, name)) {
	    vtable_add(p->lvtbl->vars, name);
	    if (p->lvtbl->used) {
		vtable_add(p->lvtbl->used, (ID)p->ruby_sourceline | LVAR_USED);
	    }
	    return 0;
	}
    }
    else {
	if (local_id(p, name)) {
	    yyerror0("duplicated argument name");
	}
    }
    return 1;
}

static ID
shadowing_lvar(struct parser_params *p, ID name)
{
    shadowing_lvar_0(p, name);
    return name;
}

static void
new_bv(struct parser_params *p, ID name)
{
    if (!name) return;
    if (!is_local_id(name)) {
	compile_error(p, "invalid local variable - %"PRIsVALUE,
		      rb_id2str(name));
	return;
    }
    if (!shadowing_lvar_0(p, name)) return;
    dyna_var(p, name);
}

#ifndef RIPPER
static NODE *
aryset(struct parser_params *p, NODE *recv, NODE *idx, const YYLTYPE *loc)
{
    return NEW_ATTRASGN(recv, tASET, idx, loc);
}

static void
block_dup_check(struct parser_params *p, NODE *node1, NODE *node2)
{
    if (node2 && node1 && nd_type(node1) == NODE_BLOCK_PASS) {
	compile_error(p, "both block arg and actual block given");
    }
}

static NODE *
attrset(struct parser_params *p, NODE *recv, ID atype, ID id, const YYLTYPE *loc)
{
    if (!CALL_Q_P(atype)) id = rb_id_attrset(id);
    return NEW_ATTRASGN(recv, id, 0, loc);
}

static void
rb_backref_error(struct parser_params *p, NODE *node)
{
    switch (nd_type(node)) {
      case NODE_NTH_REF:
	compile_error(p, "Can't set variable $%ld", node->nd_nth);
	break;
      case NODE_BACK_REF:
	compile_error(p, "Can't set variable $%c", (int)node->nd_nth);
	break;
    }
}

static NODE *
arg_append(struct parser_params *p, NODE *node1, NODE *node2, const YYLTYPE *loc)
{
    if (!node1) return NEW_LIST(node2, &node2->nd_loc);
    switch (nd_type(node1))  {
      case NODE_LIST:
	return list_append(p, node1, node2);
      case NODE_BLOCK_PASS:
	node1->nd_head = arg_append(p, node1->nd_head, node2, loc);
	node1->nd_loc.end_pos = node1->nd_head->nd_loc.end_pos;
	return node1;
      case NODE_ARGSPUSH:
	node1->nd_body = list_append(p, NEW_LIST(node1->nd_body, &node1->nd_body->nd_loc), node2);
	node1->nd_loc.end_pos = node1->nd_body->nd_loc.end_pos;
	nd_set_type(node1, NODE_ARGSCAT);
	return node1;
      case NODE_ARGSCAT:
        if (nd_type(node1->nd_body) != NODE_LIST) break;
        node1->nd_body = list_append(p, node1->nd_body, node2);
        node1->nd_loc.end_pos = node1->nd_body->nd_loc.end_pos;
        return node1;
    }
    return NEW_ARGSPUSH(node1, node2, loc);
}

static NODE *
arg_concat(struct parser_params *p, NODE *node1, NODE *node2, const YYLTYPE *loc)
{
    if (!node2) return node1;
    switch (nd_type(node1)) {
      case NODE_BLOCK_PASS:
	if (node1->nd_head)
	    node1->nd_head = arg_concat(p, node1->nd_head, node2, loc);
	else
	    node1->nd_head = NEW_LIST(node2, loc);
	return node1;
      case NODE_ARGSPUSH:
	if (nd_type(node2) != NODE_LIST) break;
	node1->nd_body = list_concat(NEW_LIST(node1->nd_body, loc), node2);
	nd_set_type(node1, NODE_ARGSCAT);
	return node1;
      case NODE_ARGSCAT:
	if (nd_type(node2) != NODE_LIST ||
	    nd_type(node1->nd_body) != NODE_LIST) break;
	node1->nd_body = list_concat(node1->nd_body, node2);
	return node1;
    }
    return NEW_ARGSCAT(node1, node2, loc);
}

static NODE *
last_arg_append(struct parser_params *p, NODE *args, NODE *last_arg, const YYLTYPE *loc)
{
    NODE *n1;
    if ((n1 = splat_array(args)) != 0) {
	return list_append(p, n1, last_arg);
    }
    return arg_append(p, args, last_arg, loc);
}

static NODE *
rest_arg_append(struct parser_params *p, NODE *args, NODE *rest_arg, const YYLTYPE *loc)
{
    NODE *n1;
    if ((nd_type(rest_arg) == NODE_LIST) && (n1 = splat_array(args)) != 0) {
	return list_concat(n1, rest_arg);
    }
    return arg_concat(p, args, rest_arg, loc);
}

static NODE *
splat_array(NODE* node)
{
    if (nd_type(node) == NODE_SPLAT) node = node->nd_head;
    if (nd_type(node) == NODE_LIST) return node;
    return 0;
}

static void
mark_lvar_used(struct parser_params *p, NODE *rhs)
{
    ID *vidp = NULL;
    if (!rhs) return;
    switch (nd_type(rhs)) {
      case NODE_LASGN:
	if (local_id_ref(p, rhs->nd_vid, &vidp)) {
	    if (vidp) *vidp |= LVAR_USED;
	}
	break;
      case NODE_DASGN:
      case NODE_DASGN_CURR:
	if (dvar_defined_ref(p, rhs->nd_vid, &vidp)) {
	    if (vidp) *vidp |= LVAR_USED;
	}
	break;
#if 0
      case NODE_MASGN:
	for (rhs = rhs->nd_head; rhs; rhs = rhs->nd_next) {
	    mark_lvar_used(p, rhs->nd_head);
	}
	break;
#endif
    }
}

static NODE *
node_assign(struct parser_params *p, NODE *lhs, NODE *rhs, const YYLTYPE *loc)
{
    if (!lhs) return 0;

    switch (nd_type(lhs)) {
      case NODE_GASGN:
      case NODE_IASGN:
      case NODE_LASGN:
      case NODE_DASGN:
      case NODE_DASGN_CURR:
      case NODE_MASGN:
      case NODE_CDECL:
      case NODE_CVASGN:
	lhs->nd_value = rhs;
	nd_set_loc(lhs, loc);
	break;

      case NODE_ATTRASGN:
	lhs->nd_args = arg_append(p, lhs->nd_args, rhs, loc);
	nd_set_loc(lhs, loc);
	break;

      default:
	/* should not happen */
	break;
    }

    return lhs;
}

static NODE *
value_expr_check(struct parser_params *p, NODE *node)
{
    NODE *void_node = 0, *vn;

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
	    return void_node ? void_node : node;

	  case NODE_CASE3:
	    if (!node->nd_body || nd_type(node->nd_body) != NODE_IN) {
		compile_error(p, "unexpected node");
		return NULL;
	    }
	    if (node->nd_body->nd_body) {
		return NULL;
	    }
	    /* single line pattern matching */
	    return void_node ? void_node : node;

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
	  case NODE_UNLESS:
	    if (!node->nd_body) {
		return NULL;
	    }
	    else if (!node->nd_else) {
		return NULL;
	    }
	    vn = value_expr_check(p, node->nd_body);
	    if (!vn) return NULL;
	    if (!void_node) void_node = vn;
	    node = node->nd_else;
	    break;

	  case NODE_AND:
	  case NODE_OR:
	    node = node->nd_1st;
	    break;

	  case NODE_LASGN:
	  case NODE_DASGN:
	  case NODE_DASGN_CURR:
	  case NODE_MASGN:
	    mark_lvar_used(p, node);
	    return NULL;

	  default:
	    return NULL;
	}
    }

    return NULL;
}

static int
value_expr_gen(struct parser_params *p, NODE *node)
{
    NODE *void_node = value_expr_check(p, node);
    if (void_node) {
	yyerror1(&void_node->nd_loc, "void value expression");
	/* or "control never reach"? */
	return FALSE;
    }
    return TRUE;
}
static void
void_expr(struct parser_params *p, NODE *node)
{
    const char *useless = 0;

    if (!RTEST(ruby_verbose)) return;

    if (!node || !(node = nd_once_body(node))) return;
    switch (nd_type(node)) {
      case NODE_OPCALL:
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

static NODE *
void_stmts(struct parser_params *p, NODE *node)
{
    NODE *const n = node;
    if (!RTEST(ruby_verbose)) return n;
    if (!node) return n;
    if (nd_type(node) != NODE_BLOCK) return n;

    while (node->nd_next) {
	void_expr(p, node->nd_head);
	node = node->nd_next;
    }
    return n;
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
reduce_nodes(struct parser_params *p, NODE **body)
{
    NODE *node = *body;

    if (!node) {
	*body = NEW_NIL(&NULL_LOC);
	return;
    }
#define subnodes(n1, n2) \
    ((!node->n1) ? (node->n2 ? (body = &node->n2, 1) : 0) : \
     (!node->n2) ? (body = &node->n1, 1) : \
     (reduce_nodes(p, &node->n1), body = &node->n2, 1))

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
	  case NODE_UNLESS:
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
      case NODE_LIST:
	do {
	    if (!is_static_content(node->nd_head)) return 0;
	} while ((node = node->nd_next) != 0);
      case NODE_LIT:
      case NODE_STR:
      case NODE_NIL:
      case NODE_TRUE:
      case NODE_FALSE:
      case NODE_ZLIST:
	break;
      default:
	return 0;
    }
    return 1;
}

static int
assign_in_cond(struct parser_params *p, NODE *node)
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
	parser_warn(p, node->nd_value, "found `= literal' in conditional, should be ==");
    }
    return 1;
}

enum cond_type {
    COND_IN_OP,
    COND_IN_COND,
    COND_IN_FF
};

#define SWITCH_BY_COND_TYPE(t, w, arg) \
    switch (t) { \
      case COND_IN_OP: break; \
      case COND_IN_COND: rb_##w##0(arg "literal in condition"); break; \
      case COND_IN_FF: rb_##w##0(arg "literal in flip-flop"); break; \
    }

static NODE *cond0(struct parser_params*,NODE*,enum cond_type,const YYLTYPE*);

static NODE*
range_op(struct parser_params *p, NODE *node, const YYLTYPE *loc)
{
    enum node_type type;

    if (node == 0) return 0;

    type = nd_type(node);
    value_expr(node);
    if (type == NODE_LIT && FIXNUM_P(node->nd_lit)) {
	if (!e_option_supplied(p)) parser_warn(p, node, "integer literal in flip-flop");
	return NEW_CALL(node, tEQ, NEW_LIST(NEW_GVAR(rb_intern("$."), loc), loc), loc);
    }
    return cond0(p, node, COND_IN_FF, loc);
}

static NODE*
cond0(struct parser_params *p, NODE *node, enum cond_type type, const YYLTYPE *loc)
{
    if (node == 0) return 0;
    if (!(node = nd_once_body(node))) return 0;
    assign_in_cond(p, node);

    switch (nd_type(node)) {
      case NODE_DSTR:
      case NODE_EVSTR:
      case NODE_STR:
	SWITCH_BY_COND_TYPE(type, warn, "string ")
	break;

      case NODE_DREGX:
	if (!e_option_supplied(p)) SWITCH_BY_COND_TYPE(type, warning, "regex ")

	return NEW_MATCH2(node, NEW_GVAR(idLASTLINE, loc), loc);

      case NODE_AND:
      case NODE_OR:
	node->nd_1st = cond0(p, node->nd_1st, COND_IN_COND, loc);
	node->nd_2nd = cond0(p, node->nd_2nd, COND_IN_COND, loc);
	break;

      case NODE_DOT2:
      case NODE_DOT3:
	node->nd_beg = range_op(p, node->nd_beg, loc);
	node->nd_end = range_op(p, node->nd_end, loc);
	if (nd_type(node) == NODE_DOT2) nd_set_type(node,NODE_FLIP2);
	else if (nd_type(node) == NODE_DOT3) nd_set_type(node, NODE_FLIP3);
	break;

      case NODE_DSYM:
	SWITCH_BY_COND_TYPE(type, warning, "string ")
	break;

      case NODE_LIT:
	if (RB_TYPE_P(node->nd_lit, T_REGEXP)) {
	    if (!e_option_supplied(p)) SWITCH_BY_COND_TYPE(type, warn, "regex ")
	    nd_set_type(node, NODE_MATCH);
	}
	else if (node->nd_lit == Qtrue ||
		 node->nd_lit == Qfalse) {
	    /* booleans are OK, e.g., while true */
	}
	else {
	    SWITCH_BY_COND_TYPE(type, warning, "")
	}
      default:
	break;
    }
    return node;
}

static NODE*
cond(struct parser_params *p, NODE *node, const YYLTYPE *loc)
{
    if (node == 0) return 0;
    return cond0(p, node, COND_IN_COND, loc);
}

static NODE*
method_cond(struct parser_params *p, NODE *node, const YYLTYPE *loc)
{
    if (node == 0) return 0;
    return cond0(p, node, COND_IN_OP, loc);
}

static NODE*
new_nil_at(struct parser_params *p, const rb_code_position_t *pos)
{
    YYLTYPE loc = {*pos, *pos};
    return NEW_NIL(&loc);
}

static NODE*
new_if(struct parser_params *p, NODE *cc, NODE *left, NODE *right, const YYLTYPE *loc)
{
    if (!cc) return right;
    cc = cond0(p, cc, COND_IN_COND, loc);
    return newline_node(NEW_IF(cc, left, right, loc));
}

static NODE*
new_unless(struct parser_params *p, NODE *cc, NODE *left, NODE *right, const YYLTYPE *loc)
{
    if (!cc) return right;
    cc = cond0(p, cc, COND_IN_COND, loc);
    return newline_node(NEW_UNLESS(cc, left, right, loc));
}

static NODE*
logop(struct parser_params *p, ID id, NODE *left, NODE *right,
	  const YYLTYPE *op_loc, const YYLTYPE *loc)
{
    enum node_type type = id == idAND || id == idANDOP ? NODE_AND : NODE_OR;
    NODE *op;
    value_expr(left);
    if (left && (enum node_type)nd_type(left) == type) {
	NODE *node = left, *second;
	while ((second = node->nd_2nd) != 0 && (enum node_type)nd_type(second) == type) {
	    node = second;
	}
	node->nd_2nd = NEW_NODE(type, second, right, 0, loc);
	nd_set_line(node->nd_2nd, op_loc->beg_pos.lineno);
	left->nd_loc.end_pos = loc->end_pos;
	return left;
    }
    op = NEW_NODE(type, left, right, 0, loc);
    nd_set_line(op, op_loc->beg_pos.lineno);
    return op;
}

static void
no_blockarg(struct parser_params *p, NODE *node)
{
    if (node && nd_type(node) == NODE_BLOCK_PASS) {
	compile_error(p, "block argument should not be given");
    }
}

static NODE *
ret_args(struct parser_params *p, NODE *node)
{
    if (node) {
	no_blockarg(p, node);
	if (nd_type(node) == NODE_LIST) {
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
new_yield(struct parser_params *p, NODE *node, const YYLTYPE *loc)
{
    if (node) no_blockarg(p, node);

    return NEW_YIELD(node, loc);
}

static VALUE
negate_lit(struct parser_params *p, VALUE lit)
{
    if (FIXNUM_P(lit)) {
	return LONG2FIX(-FIX2LONG(lit));
    }
    if (SPECIAL_CONST_P(lit)) {
#if USE_FLONUM
	if (FLONUM_P(lit)) {
	    return DBL2NUM(-RFLOAT_VALUE(lit));
	}
#endif
	goto unknown;
    }
    switch (BUILTIN_TYPE(lit)) {
      case T_BIGNUM:
	BIGNUM_NEGATE(lit);
	lit = rb_big_norm(lit);
	break;
      case T_RATIONAL:
	RATIONAL_SET_NUM(lit, negate_lit(p, RRATIONAL(lit)->num));
	break;
      case T_COMPLEX:
	RCOMPLEX_SET_REAL(lit, negate_lit(p, RCOMPLEX(lit)->real));
	RCOMPLEX_SET_IMAG(lit, negate_lit(p, RCOMPLEX(lit)->imag));
	break;
      case T_FLOAT:
	RFLOAT(lit)->float_value = -RFLOAT_VALUE(lit);
	break;
      unknown:
      default:
	rb_parser_fatal(p, "unknown literal type (%s) passed to negate_lit",
			rb_builtin_class_name(lit));
	break;
    }
    return lit;
}

static NODE *
arg_blk_pass(NODE *node1, NODE *node2)
{
    if (node2) {
        if (!node1) return node2;
	node2->nd_head = node1;
	nd_set_first_lineno(node2, nd_first_lineno(node1));
	nd_set_first_column(node2, nd_first_column(node1));
	return node2;
    }
    return node1;
}

static bool
args_info_empty_p(struct rb_args_info *args)
{
    if (args->pre_args_num) return false;
    if (args->post_args_num) return false;
    if (args->rest_arg) return false;
    if (args->opt_args) return false;
    if (args->block_arg) return false;
    if (args->kw_args) return false;
    if (args->kw_rest_arg) return false;
    return true;
}

static NODE*
new_args(struct parser_params *p, NODE *pre_args, NODE *opt_args, ID rest_arg, NODE *post_args, NODE *tail, const YYLTYPE *loc)
{
    int saved_line = p->ruby_sourceline;
    struct rb_args_info *args = tail->nd_ainfo;

    args->pre_args_num   = pre_args ? rb_long2int(pre_args->nd_plen) : 0;
    args->pre_init       = pre_args ? pre_args->nd_next : 0;

    args->post_args_num  = post_args ? rb_long2int(post_args->nd_plen) : 0;
    args->post_init      = post_args ? post_args->nd_next : 0;
    args->first_post_arg = post_args ? post_args->nd_pid : 0;

    args->rest_arg       = rest_arg;

    args->opt_args       = opt_args;

    args->ruby2_keywords = rest_arg == idFWD_REST;

    p->ruby_sourceline = saved_line;
    nd_set_loc(tail, loc);

    return tail;
}

static NODE*
new_args_tail(struct parser_params *p, NODE *kw_args, ID kw_rest_arg, ID block, const YYLTYPE *kw_rest_loc)
{
    int saved_line = p->ruby_sourceline;
    NODE *node;
    VALUE tmpbuf = rb_imemo_tmpbuf_auto_free_pointer();
    struct rb_args_info *args = ZALLOC(struct rb_args_info);
    rb_imemo_tmpbuf_set_ptr(tmpbuf, args);
    args->imemo = tmpbuf;
    node = NEW_NODE(NODE_ARGS, 0, 0, args, &NULL_LOC);
    RB_OBJ_WRITTEN(p->ast, Qnil, tmpbuf);
    if (p->error_p) return node;

    args->block_arg      = block;
    args->kw_args        = kw_args;

    if (kw_args) {
	/*
	 * def foo(k1: 1, kr1:, k2: 2, **krest, &b)
	 * variable order: k1, kr1, k2, &b, internal_id, krest
	 * #=> <reorder>
	 * variable order: kr1, k1, k2, internal_id, krest, &b
	 */
	ID kw_bits = internal_id(p), *required_kw_vars, *kw_vars;
	struct vtable *vtargs = p->lvtbl->args;
	NODE *kwn = kw_args;

	vtable_pop(vtargs, !!block + !!kw_rest_arg);
	required_kw_vars = kw_vars = &vtargs->tbl[vtargs->pos];
	while (kwn) {
	    if (!NODE_REQUIRED_KEYWORD_P(kwn->nd_body))
		--kw_vars;
	    --required_kw_vars;
	    kwn = kwn->nd_next;
	}

	for (kwn = kw_args; kwn; kwn = kwn->nd_next) {
	    ID vid = kwn->nd_body->nd_vid;
	    if (NODE_REQUIRED_KEYWORD_P(kwn->nd_body)) {
		*required_kw_vars++ = vid;
	    }
	    else {
		*kw_vars++ = vid;
	    }
	}

	arg_var(p, kw_bits);
	if (kw_rest_arg) arg_var(p, kw_rest_arg);
	if (block) arg_var(p, block);

	args->kw_rest_arg = NEW_DVAR(kw_rest_arg, kw_rest_loc);
	args->kw_rest_arg->nd_cflag = kw_bits;
    }
    else if (kw_rest_arg == idNil) {
	args->no_kwarg = 1;
    }
    else if (kw_rest_arg) {
	args->kw_rest_arg = NEW_DVAR(kw_rest_arg, kw_rest_loc);
    }

    p->ruby_sourceline = saved_line;
    return node;
}

static NODE *
args_with_numbered(struct parser_params *p, NODE *args, int max_numparam)
{
    if (max_numparam > NO_PARAM) {
	if (!args) {
	    YYLTYPE loc = RUBY_INIT_YYLLOC();
	    args = new_args_tail(p, 0, 0, 0, 0);
	    nd_set_loc(args, &loc);
	}
	args->nd_ainfo->pre_args_num = max_numparam;
    }
    return args;
}

static NODE*
new_array_pattern(struct parser_params *p, NODE *constant, NODE *pre_arg, NODE *aryptn, const YYLTYPE *loc)
{
    struct rb_ary_pattern_info *apinfo = aryptn->nd_apinfo;

    aryptn->nd_pconst = constant;

    if (pre_arg) {
	NODE *pre_args = NEW_LIST(pre_arg, loc);
	if (apinfo->pre_args) {
	    apinfo->pre_args = list_concat(pre_args, apinfo->pre_args);
	}
	else {
	    apinfo->pre_args = pre_args;
	}
    }
    return aryptn;
}

static NODE*
new_array_pattern_tail(struct parser_params *p, NODE *pre_args, int has_rest, ID rest_arg, NODE *post_args, const YYLTYPE *loc)
{
    int saved_line = p->ruby_sourceline;
    NODE *node;
    VALUE tmpbuf = rb_imemo_tmpbuf_auto_free_pointer();
    struct rb_ary_pattern_info *apinfo = ZALLOC(struct rb_ary_pattern_info);
    rb_imemo_tmpbuf_set_ptr(tmpbuf, apinfo);
    node = NEW_NODE(NODE_ARYPTN, 0, tmpbuf, apinfo, loc);
    RB_OBJ_WRITTEN(p->ast, Qnil, tmpbuf);

    apinfo->pre_args = pre_args;

    if (has_rest) {
	if (rest_arg) {
	    apinfo->rest_arg = assignable(p, rest_arg, 0, loc);
	}
	else {
	    apinfo->rest_arg = NODE_SPECIAL_NO_NAME_REST;
	}
    }
    else {
	apinfo->rest_arg = NULL;
    }

    apinfo->post_args = post_args;

    p->ruby_sourceline = saved_line;
    return node;
}

static NODE*
new_find_pattern(struct parser_params *p, NODE *constant, NODE *fndptn, const YYLTYPE *loc)
{
    fndptn->nd_pconst = constant;

    return fndptn;
}

static NODE*
new_find_pattern_tail(struct parser_params *p, ID pre_rest_arg, NODE *args, ID post_rest_arg, const YYLTYPE *loc)
{
    int saved_line = p->ruby_sourceline;
    NODE *node;
    VALUE tmpbuf = rb_imemo_tmpbuf_auto_free_pointer();
    struct rb_fnd_pattern_info *fpinfo = ZALLOC(struct rb_fnd_pattern_info);
    rb_imemo_tmpbuf_set_ptr(tmpbuf, fpinfo);
    node = NEW_NODE(NODE_FNDPTN, 0, tmpbuf, fpinfo, loc);
    RB_OBJ_WRITTEN(p->ast, Qnil, tmpbuf);

    fpinfo->pre_rest_arg = pre_rest_arg ? assignable(p, pre_rest_arg, 0, loc) : NODE_SPECIAL_NO_NAME_REST;
    fpinfo->args = args;
    fpinfo->post_rest_arg = post_rest_arg ? assignable(p, post_rest_arg, 0, loc) : NODE_SPECIAL_NO_NAME_REST;

    p->ruby_sourceline = saved_line;
    return node;
}

static NODE*
new_hash_pattern(struct parser_params *p, NODE *constant, NODE *hshptn, const YYLTYPE *loc)
{
    hshptn->nd_pconst = constant;
    return hshptn;
}

static NODE*
new_hash_pattern_tail(struct parser_params *p, NODE *kw_args, ID kw_rest_arg, const YYLTYPE *loc)
{
    int saved_line = p->ruby_sourceline;
    NODE *node, *kw_rest_arg_node;

    if (kw_rest_arg == idNil) {
	kw_rest_arg_node = NODE_SPECIAL_NO_REST_KEYWORD;
    }
    else if (kw_rest_arg) {
	kw_rest_arg_node = assignable(p, kw_rest_arg, 0, loc);
    }
    else {
	kw_rest_arg_node = NULL;
    }

    node = NEW_NODE(NODE_HSHPTN, 0, kw_args, kw_rest_arg_node, loc);

    p->ruby_sourceline = saved_line;
    return node;
}

static NODE*
dsym_node(struct parser_params *p, NODE *node, const YYLTYPE *loc)
{
    VALUE lit;

    if (!node) {
	return NEW_LIT(ID2SYM(idNULL), loc);
    }

    switch (nd_type(node)) {
      case NODE_DSTR:
	nd_set_type(node, NODE_DSYM);
	nd_set_loc(node, loc);
	break;
      case NODE_STR:
	lit = node->nd_lit;
	RB_OBJ_WRITTEN(p->ast, Qnil, node->nd_lit = ID2SYM(rb_intern_str(lit)));
	nd_set_type(node, NODE_LIT);
	nd_set_loc(node, loc);
	break;
      default:
	node = NEW_NODE(NODE_DSYM, Qnil, 1, NEW_LIST(node, loc), loc);
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
remove_duplicate_keys(struct parser_params *p, NODE *hash)
{
    st_table *literal_keys = st_init_numtable_with_size(hash->nd_alen / 2);
    NODE *result = 0;
    rb_code_location_t loc = hash->nd_loc;
    while (hash && hash->nd_head && hash->nd_next) {
	NODE *head = hash->nd_head;
	NODE *value = hash->nd_next;
	NODE *next = value->nd_next;
	VALUE key = (VALUE)head;
	st_data_t data;
	if (nd_type(head) == NODE_LIT &&
	    st_lookup(literal_keys, (key = head->nd_lit), &data)) {
	    rb_compile_warn(p->ruby_sourcefile, nd_line((NODE *)data),
			    "key %+"PRIsVALUE" is duplicated and overwritten on line %d",
			    head->nd_lit, nd_line(head));
	    head = ((NODE *)data)->nd_next;
	    head->nd_head = block_append(p, head->nd_head, value->nd_head);
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
    result->nd_loc = loc;
    return result;
}

static NODE *
new_hash(struct parser_params *p, NODE *hash, const YYLTYPE *loc)
{
    if (hash) hash = remove_duplicate_keys(p, hash);
    return NEW_HASH(hash, loc);
}
#endif

static void
error_duplicate_pattern_variable(struct parser_params *p, ID id, const YYLTYPE *loc)
{
    if (is_private_local_id(id)) {
	return;
    }
    if (st_is_member(p->pvtbl, id)) {
	yyerror1(loc, "duplicated variable name");
    }
    else {
	st_insert(p->pvtbl, (st_data_t)id, 0);
    }
}

static void
error_duplicate_pattern_key(struct parser_params *p, VALUE key, const YYLTYPE *loc)
{
    if (!p->pktbl) {
	p->pktbl = st_init_numtable();
    }
    else if (st_is_member(p->pktbl, key)) {
	yyerror1(loc, "duplicated key name");
	return;
    }
    st_insert(p->pktbl, (st_data_t)key, 0);
}

#ifndef RIPPER
static NODE *
new_unique_key_hash(struct parser_params *p, NODE *hash, const YYLTYPE *loc)
{
    return NEW_HASH(hash, loc);
}
#endif /* !RIPPER */

#ifndef RIPPER
static NODE *
new_op_assign(struct parser_params *p, NODE *lhs, ID op, NODE *rhs, const YYLTYPE *loc)
{
    NODE *asgn;

    if (lhs) {
	ID vid = lhs->nd_vid;
	YYLTYPE lhs_loc = lhs->nd_loc;
	if (op == tOROP) {
	    lhs->nd_value = rhs;
	    nd_set_loc(lhs, loc);
	    asgn = NEW_OP_ASGN_OR(gettable(p, vid, &lhs_loc), lhs, loc);
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
	    nd_set_loc(lhs, loc);
	    asgn = NEW_OP_ASGN_AND(gettable(p, vid, &lhs_loc), lhs, loc);
	}
	else {
	    asgn = lhs;
	    asgn->nd_value = NEW_CALL(gettable(p, vid, &lhs_loc), op, NEW_LIST(rhs, &rhs->nd_loc), loc);
	    nd_set_loc(asgn, loc);
	}
    }
    else {
	asgn = NEW_BEGIN(0, loc);
    }
    return asgn;
}

static NODE *
new_ary_op_assign(struct parser_params *p, NODE *ary,
		  NODE *args, ID op, NODE *rhs, const YYLTYPE *args_loc, const YYLTYPE *loc)
{
    NODE *asgn;

    args = make_list(args, args_loc);
    if (nd_type(args) == NODE_BLOCK_PASS) {
	args = NEW_ARGSCAT(args, rhs, loc);
    }
    else {
	args = arg_concat(p, args, rhs, loc);
    }
    asgn = NEW_OP_ASGN1(ary, op, args, loc);
    fixpos(asgn, ary);
    return asgn;
}

static NODE *
new_attr_op_assign(struct parser_params *p, NODE *lhs,
		   ID atype, ID attr, ID op, NODE *rhs, const YYLTYPE *loc)
{
    NODE *asgn;

    asgn = NEW_OP_ASGN2(lhs, CALL_Q_P(atype), attr, op, rhs, loc);
    fixpos(asgn, lhs);
    return asgn;
}

static NODE *
new_const_op_assign(struct parser_params *p, NODE *lhs, ID op, NODE *rhs, const YYLTYPE *loc)
{
    NODE *asgn;

    if (lhs) {
	asgn = NEW_OP_CDECL(lhs, op, rhs, loc);
    }
    else {
	asgn = NEW_BEGIN(0, loc);
    }
    fixpos(asgn, lhs);
    return asgn;
}

static NODE *
const_decl(struct parser_params *p, NODE *path, const YYLTYPE *loc)
{
    if (p->ctxt.in_def) {
	yyerror1(loc, "dynamic constant assignment");
    }
    return NEW_CDECL(0, 0, (path), loc);
}
#else
static VALUE
const_decl(struct parser_params *p, VALUE path)
{
    if (p->ctxt.in_def) {
	path = dispatch1(assign_error, path);
	ripper_error(p);
    }
    return path;
}

static VALUE
assign_error(struct parser_params *p, VALUE a)
{
    a = dispatch1(assign_error, a);
    ripper_error(p);
    return a;
}

static VALUE
var_field(struct parser_params *p, VALUE a)
{
    return ripper_new_yylval(p, get_id(a), dispatch1(var_field, a), 0);
}
#endif

#ifndef RIPPER
static NODE *
new_bodystmt(struct parser_params *p, NODE *head, NODE *rescue, NODE *rescue_else, NODE *ensure, const YYLTYPE *loc)
{
    NODE *result = head;
    if (rescue) {
        NODE *tmp = rescue_else ? rescue_else : rescue;
        YYLTYPE rescue_loc = code_loc_gen(&head->nd_loc, &tmp->nd_loc);

        result = NEW_RESCUE(head, rescue, rescue_else, &rescue_loc);
        nd_set_line(result, rescue->nd_loc.beg_pos.lineno);
    }
    else if (rescue_else) {
        result = block_append(p, result, rescue_else);
    }
    if (ensure) {
        result = NEW_ENSURE(result, ensure, loc);
    }
    fixpos(result, head);
    return result;
}
#endif

static void
warn_unused_var(struct parser_params *p, struct local_vars *local)
{
    int cnt;

    if (!local->used) return;
    cnt = local->used->pos;
    if (cnt != local->vars->pos) {
	rb_parser_fatal(p, "local->used->pos != local->vars->pos");
    }
#ifndef RIPPER
    ID *v = local->vars->tbl;
    ID *u = local->used->tbl;
    for (int i = 0; i < cnt; ++i) {
	if (!v[i] || (u[i] & LVAR_USED)) continue;
	if (is_private_local_id(v[i])) continue;
	rb_warn1L((int)u[i], "assigned but unused variable - %"PRIsWARN, rb_id2str(v[i]));
    }
#endif
}

static void
local_push(struct parser_params *p, int toplevel_scope)
{
    struct local_vars *local;
    int inherits_dvars = toplevel_scope && compile_for_eval;
    int warn_unused_vars = RTEST(ruby_verbose);

    local = ALLOC(struct local_vars);
    local->prev = p->lvtbl;
    local->args = vtable_alloc(0);
    local->vars = vtable_alloc(inherits_dvars ? DVARS_INHERIT : DVARS_TOPSCOPE);
#ifndef RIPPER
    if (toplevel_scope && compile_for_eval) warn_unused_vars = 0;
    if (toplevel_scope && e_option_supplied(p)) warn_unused_vars = 0;
    local->numparam.outer = 0;
    local->numparam.inner = 0;
    local->numparam.current = 0;
#endif
    local->used = warn_unused_vars ? vtable_alloc(0) : 0;

# if WARN_PAST_SCOPE
    local->past = 0;
# endif
    CMDARG_PUSH(0);
    COND_PUSH(0);
    p->lvtbl = local;
}

static void
local_pop(struct parser_params *p)
{
    struct local_vars *local = p->lvtbl->prev;
    if (p->lvtbl->used) {
	warn_unused_var(p, p->lvtbl);
	vtable_free(p->lvtbl->used);
    }
# if WARN_PAST_SCOPE
    while (p->lvtbl->past) {
	struct vtable *past = p->lvtbl->past;
	p->lvtbl->past = past->prev;
	vtable_free(past);
    }
# endif
    vtable_free(p->lvtbl->args);
    vtable_free(p->lvtbl->vars);
    CMDARG_POP();
    COND_POP();
    ruby_sized_xfree(p->lvtbl, sizeof(*p->lvtbl));
    p->lvtbl = local;
}

#ifndef RIPPER
static ID*
local_tbl(struct parser_params *p)
{
    int cnt_args = vtable_size(p->lvtbl->args);
    int cnt_vars = vtable_size(p->lvtbl->vars);
    int cnt = cnt_args + cnt_vars;
    int i, j;
    ID *buf;

    if (cnt <= 0) return 0;
    buf = ALLOC_N(ID, cnt + 2);
    MEMCPY(buf+1, p->lvtbl->args->tbl, ID, cnt_args);
    /* remove IDs duplicated to warn shadowing */
    for (i = 0, j = cnt_args+1; i < cnt_vars; ++i) {
	ID id = p->lvtbl->vars->tbl[i];
	if (!vtable_included(p->lvtbl->args, id)) {
	    buf[j++] = id;
	}
    }
    if (--j < cnt) {
	REALLOC_N(buf, ID, (cnt = j) + 2);
    }
    buf[0] = cnt;
    rb_ast_add_local_table(p->ast, buf);

    return buf;
}

static NODE*
node_newnode_with_locals(struct parser_params *p, enum node_type type, VALUE a1, VALUE a2, const rb_code_location_t *loc)
{
    ID *a0;
    NODE *n;

    a0 = local_tbl(p);
    n = NEW_NODE(type, a0, a1, a2, loc);
    return n;
}

#endif

static void
numparam_name(struct parser_params *p, ID id)
{
    if (!NUMPARAM_ID_P(id)) return;
    compile_error(p, "_%d is reserved for numbered parameter",
        NUMPARAM_ID_TO_IDX(id));
}

static void
arg_var(struct parser_params *p, ID id)
{
    numparam_name(p, id);
    vtable_add(p->lvtbl->args, id);
}

static void
local_var(struct parser_params *p, ID id)
{
    numparam_name(p, id);
    vtable_add(p->lvtbl->vars, id);
    if (p->lvtbl->used) {
	vtable_add(p->lvtbl->used, (ID)p->ruby_sourceline);
    }
}

static int
local_id_ref(struct parser_params *p, ID id, ID **vidrefp)
{
    struct vtable *vars, *args, *used;

    vars = p->lvtbl->vars;
    args = p->lvtbl->args;
    used = p->lvtbl->used;

    while (vars && !DVARS_TERMINAL_P(vars->prev)) {
	vars = vars->prev;
	args = args->prev;
	if (used) used = used->prev;
    }

    if (vars && vars->prev == DVARS_INHERIT) {
	return rb_local_defined(id, p->parent_iseq);
    }
    else if (vtable_included(args, id)) {
	return 1;
    }
    else {
	int i = vtable_included(vars, id);
	if (i && used && vidrefp) *vidrefp = &used->tbl[i-1];
	return i != 0;
    }
}

static int
local_id(struct parser_params *p, ID id)
{
    return local_id_ref(p, id, NULL);
}

static int
check_forwarding_args(struct parser_params *p)
{
    if (local_id(p, idFWD_REST) &&
#if idFWD_KWREST
        local_id(p, idFWD_KWREST) &&
#endif
        local_id(p, idFWD_BLOCK)) return TRUE;
    compile_error(p, "unexpected ...");
    return FALSE;
}

static void
add_forwarding_args(struct parser_params *p)
{
    arg_var(p, idFWD_REST);
#if idFWD_KWREST
    arg_var(p, idFWD_KWREST);
#endif
    arg_var(p, idFWD_BLOCK);
}

#ifndef RIPPER
static NODE *
new_args_forward_call(struct parser_params *p, NODE *leading, const YYLTYPE *loc, const YYLTYPE *argsloc)
{
    NODE *splat = NEW_SPLAT(NEW_LVAR(idFWD_REST, loc), loc);
#if idFWD_KWREST
    NODE *kwrest = list_append(p, NEW_LIST(0, loc), NEW_LVAR(idFWD_KWREST, loc));
#endif
    NODE *block = NEW_BLOCK_PASS(NEW_LVAR(idFWD_BLOCK, loc), loc);
    NODE *args = leading ? rest_arg_append(p, leading, splat, argsloc) : splat;
#if idFWD_KWREST
    args = arg_append(p, splat, new_hash(p, kwrest, loc), loc);
#endif
    return arg_blk_pass(args, block);
}

static NODE *
new_args_forward_def(struct parser_params *p, NODE *leading, const YYLTYPE *loc)
{
    NODE *n = new_args_tail(p, Qnone, idFWD_KWREST, idFWD_BLOCK, loc);
    return new_args(p, leading, Qnone, idFWD_REST, Qnone, n, loc);
}
#endif

static NODE *
numparam_push(struct parser_params *p)
{
#ifndef RIPPER
    struct local_vars *local = p->lvtbl;
    NODE *inner = local->numparam.inner;
    if (!local->numparam.outer) {
	local->numparam.outer = local->numparam.current;
    }
    local->numparam.inner = 0;
    local->numparam.current = 0;
    return inner;
#else
    return 0;
#endif
}

static void
numparam_pop(struct parser_params *p, NODE *prev_inner)
{
#ifndef RIPPER
    struct local_vars *local = p->lvtbl;
    if (prev_inner) {
	/* prefer first one */
	local->numparam.inner = prev_inner;
    }
    else if (local->numparam.current) {
	/* current and inner are exclusive */
	local->numparam.inner = local->numparam.current;
    }
    if (p->max_numparam > NO_PARAM) {
	/* current and outer are exclusive */
	local->numparam.current = local->numparam.outer;
	local->numparam.outer = 0;
    }
    else {
	/* no numbered parameter */
	local->numparam.current = 0;
    }
#endif
}

static const struct vtable *
dyna_push(struct parser_params *p)
{
    p->lvtbl->args = vtable_alloc(p->lvtbl->args);
    p->lvtbl->vars = vtable_alloc(p->lvtbl->vars);
    if (p->lvtbl->used) {
	p->lvtbl->used = vtable_alloc(p->lvtbl->used);
    }
    return p->lvtbl->args;
}

static void
dyna_pop_vtable(struct parser_params *p, struct vtable **vtblp)
{
    struct vtable *tmp = *vtblp;
    *vtblp = tmp->prev;
# if WARN_PAST_SCOPE
    if (p->past_scope_enabled) {
	tmp->prev = p->lvtbl->past;
	p->lvtbl->past = tmp;
	return;
    }
# endif
    vtable_free(tmp);
}

static void
dyna_pop_1(struct parser_params *p)
{
    struct vtable *tmp;

    if ((tmp = p->lvtbl->used) != 0) {
	warn_unused_var(p, p->lvtbl);
	p->lvtbl->used = p->lvtbl->used->prev;
	vtable_free(tmp);
    }
    dyna_pop_vtable(p, &p->lvtbl->args);
    dyna_pop_vtable(p, &p->lvtbl->vars);
}

static void
dyna_pop(struct parser_params *p, const struct vtable *lvargs)
{
    while (p->lvtbl->args != lvargs) {
	dyna_pop_1(p);
	if (!p->lvtbl->args) {
	    struct local_vars *local = p->lvtbl->prev;
	    ruby_sized_xfree(p->lvtbl, sizeof(*p->lvtbl));
	    p->lvtbl = local;
	}
    }
    dyna_pop_1(p);
}

static int
dyna_in_block(struct parser_params *p)
{
    return !DVARS_TERMINAL_P(p->lvtbl->vars) && p->lvtbl->vars->prev != DVARS_TOPSCOPE;
}

static int
dvar_defined_ref(struct parser_params *p, ID id, ID **vidrefp)
{
    struct vtable *vars, *args, *used;
    int i;

    args = p->lvtbl->args;
    vars = p->lvtbl->vars;
    used = p->lvtbl->used;

    while (!DVARS_TERMINAL_P(vars)) {
	if (vtable_included(args, id)) {
	    return 1;
	}
	if ((i = vtable_included(vars, id)) != 0) {
	    if (used && vidrefp) *vidrefp = &used->tbl[i-1];
	    return 1;
	}
	args = args->prev;
	vars = vars->prev;
	if (!vidrefp) used = 0;
	if (used) used = used->prev;
    }

    if (vars == DVARS_INHERIT && !NUMPARAM_ID_P(id)) {
        return rb_dvar_defined(id, p->parent_iseq);
    }

    return 0;
}

static int
dvar_defined(struct parser_params *p, ID id)
{
    return dvar_defined_ref(p, id, NULL);
}

static int
dvar_curr(struct parser_params *p, ID id)
{
    return (vtable_included(p->lvtbl->args, id) ||
	    vtable_included(p->lvtbl->vars, id));
}

static void
reg_fragment_enc_error(struct parser_params* p, VALUE str, int c)
{
    compile_error(p,
        "regexp encoding option '%c' differs from source encoding '%s'",
        c, rb_enc_name(rb_enc_get(str)));
}

#ifndef RIPPER
int
rb_reg_fragment_setenc(struct parser_params* p, VALUE str, int options)
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
    else if (p->enc == rb_usascii_encoding()) {
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
reg_fragment_setenc(struct parser_params* p, VALUE str, int options)
{
    int c = rb_reg_fragment_setenc(p, str, options);
    if (c) reg_fragment_enc_error(p, str, c);
}

static int
reg_fragment_check(struct parser_params* p, VALUE str, int options)
{
    VALUE err;
    reg_fragment_setenc(p, str, options);
    err = rb_reg_check_preprocess(str);
    if (err != Qnil) {
        err = rb_obj_as_string(err);
        compile_error(p, "%"PRIsVALUE, err);
	return 0;
    }
    return 1;
}

typedef struct {
    struct parser_params* parser;
    rb_encoding *enc;
    NODE *succ_block;
    const YYLTYPE *loc;
} reg_named_capture_assign_t;

static int
reg_named_capture_assign_iter(const OnigUChar *name, const OnigUChar *name_end,
          int back_num, int *back_refs, OnigRegex regex, void *arg0)
{
    reg_named_capture_assign_t *arg = (reg_named_capture_assign_t*)arg0;
    struct parser_params* p = arg->parser;
    rb_encoding *enc = arg->enc;
    long len = name_end - name;
    const char *s = (const char *)name;
    ID var;
    NODE *node, *succ;

    if (!len) return ST_CONTINUE;
    if (len < MAX_WORD_LENGTH && rb_reserved_word(s, (int)len))
        return ST_CONTINUE;
    if (rb_enc_symname_type(s, len, enc, (1U<<ID_LOCAL)) != ID_LOCAL)
        return ST_CONTINUE;

    var = intern_cstr(s, len, enc);
    node = node_assign(p, assignable(p, var, 0, arg->loc), NEW_LIT(ID2SYM(var), arg->loc), arg->loc);
    succ = arg->succ_block;
    if (!succ) succ = NEW_BEGIN(0, arg->loc);
    succ = block_append(p, succ, node);
    arg->succ_block = succ;
    return ST_CONTINUE;
}

static NODE *
reg_named_capture_assign(struct parser_params* p, VALUE regexp, const YYLTYPE *loc)
{
    reg_named_capture_assign_t arg;

    arg.parser = p;
    arg.enc = rb_enc_get(regexp);
    arg.succ_block = 0;
    arg.loc = loc;
    onig_foreach_name(RREGEXP_PTR(regexp), reg_named_capture_assign_iter, &arg);

    if (!arg.succ_block) return 0;
    return arg.succ_block->nd_next;
}

static VALUE
parser_reg_compile(struct parser_params* p, VALUE str, int options)
{
    reg_fragment_setenc(p, str, options);
    return rb_parser_reg_compile(p, str, options);
}

VALUE
rb_parser_reg_compile(struct parser_params* p, VALUE str, int options)
{
    return rb_reg_compile(str, options & RE_OPTION_MASK, p->ruby_sourcefile, p->ruby_sourceline);
}

static VALUE
reg_compile(struct parser_params* p, VALUE str, int options)
{
    VALUE re;
    VALUE err;

    err = rb_errinfo();
    re = parser_reg_compile(p, str, options);
    if (NIL_P(re)) {
	VALUE m = rb_attr_get(rb_errinfo(), idMesg);
	rb_set_errinfo(err);
	compile_error(p, "%"PRIsVALUE, m);
	return Qnil;
    }
    return re;
}
#else
static VALUE
parser_reg_compile(struct parser_params* p, VALUE str, int options, VALUE *errmsg)
{
    VALUE err = rb_errinfo();
    VALUE re;
    str = ripper_is_node_yylval(str) ? RNODE(str)->nd_cval : str;
    int c = rb_reg_fragment_setenc(p, str, options);
    if (c) reg_fragment_enc_error(p, str, c);
    re = rb_parser_reg_compile(p, str, options);
    if (NIL_P(re)) {
	*errmsg = rb_attr_get(rb_errinfo(), idMesg);
	rb_set_errinfo(err);
    }
    return re;
}
#endif

#ifndef RIPPER
void
rb_parser_set_options(VALUE vparser, int print, int loop, int chomp, int split)
{
    struct parser_params *p;
    TypedData_Get_Struct(vparser, struct parser_params, &parser_data_type, p);
    p->do_print = print;
    p->do_loop = loop;
    p->do_chomp = chomp;
    p->do_split = split;
}

static NODE *
parser_append_options(struct parser_params *p, NODE *node)
{
    static const YYLTYPE default_location = {{1, 0}, {1, 0}};
    const YYLTYPE *const LOC = &default_location;

    if (p->do_print) {
	NODE *print = NEW_FCALL(rb_intern("print"),
				NEW_LIST(NEW_GVAR(idLASTLINE, LOC), LOC),
				LOC);
	node = block_append(p, node, print);
    }

    if (p->do_loop) {
	if (p->do_split) {
	    NODE *args = NEW_LIST(NEW_GVAR(rb_intern("$;"), LOC), LOC);
	    NODE *split = NEW_GASGN(rb_intern("$F"),
				    NEW_CALL(NEW_GVAR(idLASTLINE, LOC),
					     rb_intern("split"), args, LOC),
				    LOC);
	    node = block_append(p, split, node);
	}
	if (p->do_chomp) {
	    NODE *chomp = NEW_CALL(NEW_GVAR(idLASTLINE, LOC),
				   rb_intern("chomp!"), 0, LOC);
	    node = block_append(p, chomp, node);
	}

	node = NEW_WHILE(NEW_VCALL(idGets, LOC), node, 1, LOC);
    }

    return node;
}

void
rb_init_parse(void)
{
    /* just to suppress unused-function warnings */
    (void)nodetype;
    (void)nodeline;
}

static ID
internal_id(struct parser_params *p)
{
    const ID max_id = RB_ID_SERIAL_MAX & ~0xffff;
    ID id = (ID)vtable_size(p->lvtbl->args) + (ID)vtable_size(p->lvtbl->vars);
    id = max_id - id;
    return ID_STATIC_SYM | ID_INTERNAL | (id << ID_SCOPE_SHIFT);
}
#endif /* !RIPPER */

static void
parser_initialize(struct parser_params *p)
{
    /* note: we rely on TypedData_Make_Struct to set most fields to 0 */
    p->command_start = TRUE;
    p->ruby_sourcefile_string = Qnil;
    p->lex.lpar_beg = -1; /* make lambda_beginning_p() == FALSE at first */
    p->node_id = 0;
#ifdef RIPPER
    p->delayed.token = Qnil;
    p->result = Qnil;
    p->parsing_thread = Qnil;
#else
    p->error_buffer = Qfalse;
#endif
    p->debug_buffer = Qnil;
    p->debug_output = rb_ractor_stdout();
    p->enc = rb_utf8_encoding();
}

#ifdef RIPPER
#define parser_mark ripper_parser_mark
#define parser_free ripper_parser_free
#endif

static void
parser_mark(void *ptr)
{
    struct parser_params *p = (struct parser_params*)ptr;

    rb_gc_mark(p->lex.input);
    rb_gc_mark(p->lex.prevline);
    rb_gc_mark(p->lex.lastline);
    rb_gc_mark(p->lex.nextline);
    rb_gc_mark(p->ruby_sourcefile_string);
    rb_gc_mark((VALUE)p->lex.strterm);
    rb_gc_mark((VALUE)p->ast);
    rb_gc_mark(p->case_labels);
#ifndef RIPPER
    rb_gc_mark(p->debug_lines);
    rb_gc_mark(p->compile_option);
    rb_gc_mark(p->error_buffer);
#else
    rb_gc_mark(p->delayed.token);
    rb_gc_mark(p->value);
    rb_gc_mark(p->result);
    rb_gc_mark(p->parsing_thread);
#endif
    rb_gc_mark(p->debug_buffer);
    rb_gc_mark(p->debug_output);
#ifdef YYMALLOC
    rb_gc_mark((VALUE)p->heap);
#endif
}

static void
parser_free(void *ptr)
{
    struct parser_params *p = (struct parser_params*)ptr;
    struct local_vars *local, *prev;

    if (p->tokenbuf) {
        ruby_sized_xfree(p->tokenbuf, p->toksiz);
    }
    for (local = p->lvtbl; local; local = prev) {
	if (local->vars) xfree(local->vars);
	prev = local->prev;
	xfree(local);
    }
    {
	token_info *ptinfo;
	while ((ptinfo = p->token_info) != 0) {
	    p->token_info = ptinfo->next;
	    xfree(ptinfo);
	}
    }
    xfree(ptr);
}

static size_t
parser_memsize(const void *ptr)
{
    struct parser_params *p = (struct parser_params*)ptr;
    struct local_vars *local;
    size_t size = sizeof(*p);

    size += p->toksiz;
    for (local = p->lvtbl; local; local = local->prev) {
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
rb_parser_set_context(VALUE vparser, const struct rb_iseq_struct *base, int main)
{
    struct parser_params *p;

    TypedData_Get_Struct(vparser, struct parser_params, &parser_data_type, p);
    p->error_buffer = main ? Qfalse : Qnil;
    p->parent_iseq = base;
    return vparser;
}
#endif

#ifdef RIPPER
#define rb_parser_end_seen_p ripper_parser_end_seen_p
#define rb_parser_encoding ripper_parser_encoding
#define rb_parser_get_yydebug ripper_parser_get_yydebug
#define rb_parser_set_yydebug ripper_parser_set_yydebug
#define rb_parser_get_debug_output ripper_parser_get_debug_output
#define rb_parser_set_debug_output ripper_parser_set_debug_output
static VALUE ripper_parser_end_seen_p(VALUE vparser);
static VALUE ripper_parser_encoding(VALUE vparser);
static VALUE ripper_parser_get_yydebug(VALUE self);
static VALUE ripper_parser_set_yydebug(VALUE self, VALUE flag);
static VALUE ripper_parser_get_debug_output(VALUE self);
static VALUE ripper_parser_set_debug_output(VALUE self, VALUE output);

/*
 *  call-seq:
 *    ripper.error?   -> Boolean
 *
 *  Return true if parsed source has errors.
 */
static VALUE
ripper_error_p(VALUE vparser)
{
    struct parser_params *p;

    TypedData_Get_Struct(vparser, struct parser_params, &parser_data_type, p);
    return p->error_p ? Qtrue : Qfalse;
}
#endif

/*
 *  call-seq:
 *    ripper.end_seen?   -> Boolean
 *
 *  Return true if parsed source ended by +\_\_END\_\_+.
 */
VALUE
rb_parser_end_seen_p(VALUE vparser)
{
    struct parser_params *p;

    TypedData_Get_Struct(vparser, struct parser_params, &parser_data_type, p);
    return p->ruby__end__seen ? Qtrue : Qfalse;
}

/*
 *  call-seq:
 *    ripper.encoding   -> encoding
 *
 *  Return encoding of the source.
 */
VALUE
rb_parser_encoding(VALUE vparser)
{
    struct parser_params *p;

    TypedData_Get_Struct(vparser, struct parser_params, &parser_data_type, p);
    return rb_enc_from_encoding(p->enc);
}

#ifdef RIPPER
/*
 *  call-seq:
 *    ripper.yydebug   -> true or false
 *
 *  Get yydebug.
 */
VALUE
rb_parser_get_yydebug(VALUE self)
{
    struct parser_params *p;

    TypedData_Get_Struct(self, struct parser_params, &parser_data_type, p);
    return p->debug ? Qtrue : Qfalse;
}
#endif

/*
 *  call-seq:
 *    ripper.yydebug = flag
 *
 *  Set yydebug.
 */
VALUE
rb_parser_set_yydebug(VALUE self, VALUE flag)
{
    struct parser_params *p;

    TypedData_Get_Struct(self, struct parser_params, &parser_data_type, p);
    p->debug = RTEST(flag);
    return flag;
}

/*
 *  call-seq:
 *    ripper.debug_output   -> obj
 *
 *  Get debug output.
 */
VALUE
rb_parser_get_debug_output(VALUE self)
{
    struct parser_params *p;

    TypedData_Get_Struct(self, struct parser_params, &parser_data_type, p);
    return p->debug_output;
}

/*
 *  call-seq:
 *    ripper.debug_output = obj
 *
 *  Set debug output.
 */
VALUE
rb_parser_set_debug_output(VALUE self, VALUE output)
{
    struct parser_params *p;

    TypedData_Get_Struct(self, struct parser_params, &parser_data_type, p);
    return p->debug_output = output;
}

#ifndef RIPPER
#ifdef YYMALLOC
#define HEAPCNT(n, size) ((n) * (size) / sizeof(YYSTYPE))
/* Keep the order; NEWHEAP then xmalloc and ADD2HEAP to get rid of
 * potential memory leak */
#define NEWHEAP() rb_imemo_tmpbuf_parser_heap(0, p->heap, 0)
#define ADD2HEAP(new, cnt, ptr) ((p->heap = (new))->ptr = (ptr), \
			   (new)->cnt = (cnt), (ptr))

void *
rb_parser_malloc(struct parser_params *p, size_t size)
{
    size_t cnt = HEAPCNT(1, size);
    rb_imemo_tmpbuf_t *n = NEWHEAP();
    void *ptr = xmalloc(size);

    return ADD2HEAP(n, cnt, ptr);
}

void *
rb_parser_calloc(struct parser_params *p, size_t nelem, size_t size)
{
    size_t cnt = HEAPCNT(nelem, size);
    rb_imemo_tmpbuf_t *n = NEWHEAP();
    void *ptr = xcalloc(nelem, size);

    return ADD2HEAP(n, cnt, ptr);
}

void *
rb_parser_realloc(struct parser_params *p, void *ptr, size_t size)
{
    rb_imemo_tmpbuf_t *n;
    size_t cnt = HEAPCNT(1, size);

    if (ptr && (n = p->heap) != NULL) {
	do {
	    if (n->ptr == ptr) {
		n->ptr = ptr = xrealloc(ptr, size);
		if (n->cnt) n->cnt = cnt;
		return ptr;
	    }
	} while ((n = n->next) != NULL);
    }
    n = NEWHEAP();
    ptr = xrealloc(ptr, size);
    return ADD2HEAP(n, cnt, ptr);
}

void
rb_parser_free(struct parser_params *p, void *ptr)
{
    rb_imemo_tmpbuf_t **prev = &p->heap, *n;

    while ((n = *prev) != NULL) {
	if (n->ptr == ptr) {
	    *prev = n->next;
	    rb_gc_force_recycle((VALUE)n);
	    break;
	}
	prev = &n->next;
    }
    xfree(ptr);
}
#endif

void
rb_parser_printf(struct parser_params *p, const char *fmt, ...)
{
    va_list ap;
    VALUE mesg = p->debug_buffer;

    if (NIL_P(mesg)) p->debug_buffer = mesg = rb_str_new(0, 0);
    va_start(ap, fmt);
    rb_str_vcatf(mesg, fmt, ap);
    va_end(ap);
    if (RSTRING_END(mesg)[-1] == '\n') {
	rb_io_write(p->debug_output, mesg);
	p->debug_buffer = Qnil;
    }
}

static void
parser_compile_error(struct parser_params *p, const char *fmt, ...)
{
    va_list ap;

    rb_io_flush(p->debug_output);
    p->error_p = 1;
    va_start(ap, fmt);
    p->error_buffer =
	rb_syntax_error_append(p->error_buffer,
			       p->ruby_sourcefile_string,
			       p->ruby_sourceline,
			       rb_long2int(p->lex.pcur - p->lex.pbeg),
			       p->enc, fmt, ap);
    va_end(ap);
}

static size_t
count_char(const char *str, int c)
{
    int n = 0;
    while (str[n] == c) ++n;
    return n;
}

/*
 * strip enclosing double-quotes, same as the default yytnamerr except
 * for that single-quotes matching back-quotes do not stop stripping.
 *
 *  "\"`class' keyword\"" => "`class' keyword"
 */
RUBY_FUNC_EXPORTED size_t
rb_yytnamerr(struct parser_params *p, char *yyres, const char *yystr)
{
    YYUSE(p);
    if (*yystr == '"') {
	size_t yyn = 0, bquote = 0;
	const char *yyp = yystr;

	while (*++yyp) {
	    switch (*yyp) {
	      case '`':
		if (!bquote) {
		    bquote = count_char(yyp+1, '`') + 1;
		    if (yyres) memcpy(&yyres[yyn], yyp, bquote);
		    yyn += bquote;
		    yyp += bquote - 1;
		    break;
		}
		goto default_char;

	      case '\'':
		if (bquote && count_char(yyp+1, '\'') + 1 == bquote) {
		    if (yyres) memcpy(yyres + yyn, yyp, bquote);
		    yyn += bquote;
		    yyp += bquote - 1;
		    bquote = 0;
		    break;
		}
		if (yyp[1] && yyp[1] != '\'' && yyp[2] == '\'') {
		    if (yyres) memcpy(yyres + yyn, yyp, 3);
		    yyn += 3;
		    yyp += 2;
		    break;
		}
		goto do_not_strip_quotes;

	      case ',':
		goto do_not_strip_quotes;

	      case '\\':
		if (*++yyp != '\\')
		    goto do_not_strip_quotes;
		/* Fall through.  */
	      default_char:
	      default:
		if (yyres)
		    yyres[yyn] = *yyp;
		yyn++;
		break;

	      case '"':
	      case '\0':
		if (yyres)
		    yyres[yyn] = '\0';
		return yyn;
	    }
	}
      do_not_strip_quotes: ;
    }

    if (!yyres) return strlen(yystr);

    return (YYSIZE_T)(yystpcpy(yyres, yystr) - yyres);
}
#endif

#ifdef RIPPER
#ifdef RIPPER_DEBUG
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
    switch (BUILTIN_TYPE(x)) {
      case T_STRING:
      case T_OBJECT:
      case T_ARRAY:
      case T_BIGNUM:
      case T_FLOAT:
      case T_COMPLEX:
      case T_RATIONAL:
	break;
      case T_NODE:
	if (nd_type((NODE *)x) != NODE_RIPPER) {
	    rb_raise(rb_eArgError, "NODE given: %p", (void *)x);
	}
	x = ((NODE *)x)->nd_rval;
	break;
      default:
	rb_raise(rb_eArgError, "wrong type of ruby object: %p (%s)",
		 (void *)x, rb_obj_classname(x));
    }
    if (!RBASIC_CLASS(x)) {
	rb_raise(rb_eArgError, "hidden ruby object: %p (%s)",
		 (void *)x, rb_builtin_type_name(TYPE(x)));
    }
    return x;
}
#endif

#define validate(x) ((x) = get_value(x))

static VALUE
ripper_dispatch0(struct parser_params *p, ID mid)
{
    return rb_funcall(p->value, mid, 0);
}

static VALUE
ripper_dispatch1(struct parser_params *p, ID mid, VALUE a)
{
    validate(a);
    return rb_funcall(p->value, mid, 1, a);
}

static VALUE
ripper_dispatch2(struct parser_params *p, ID mid, VALUE a, VALUE b)
{
    validate(a);
    validate(b);
    return rb_funcall(p->value, mid, 2, a, b);
}

static VALUE
ripper_dispatch3(struct parser_params *p, ID mid, VALUE a, VALUE b, VALUE c)
{
    validate(a);
    validate(b);
    validate(c);
    return rb_funcall(p->value, mid, 3, a, b, c);
}

static VALUE
ripper_dispatch4(struct parser_params *p, ID mid, VALUE a, VALUE b, VALUE c, VALUE d)
{
    validate(a);
    validate(b);
    validate(c);
    validate(d);
    return rb_funcall(p->value, mid, 4, a, b, c, d);
}

static VALUE
ripper_dispatch5(struct parser_params *p, ID mid, VALUE a, VALUE b, VALUE c, VALUE d, VALUE e)
{
    validate(a);
    validate(b);
    validate(c);
    validate(d);
    validate(e);
    return rb_funcall(p->value, mid, 5, a, b, c, d, e);
}

static VALUE
ripper_dispatch7(struct parser_params *p, ID mid, VALUE a, VALUE b, VALUE c, VALUE d, VALUE e, VALUE f, VALUE g)
{
    validate(a);
    validate(b);
    validate(c);
    validate(d);
    validate(e);
    validate(f);
    validate(g);
    return rb_funcall(p->value, mid, 7, a, b, c, d, e, f, g);
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
ripper_error(struct parser_params *p)
{
    p->error_p = TRUE;
}

static void
ripper_compile_error(struct parser_params *p, const char *fmt, ...)
{
    VALUE str;
    va_list args;

    va_start(args, fmt);
    str = rb_vsprintf(fmt, args);
    va_end(args);
    rb_funcall(p->value, rb_intern("compile_error"), 1, str);
    ripper_error(p);
}

static VALUE
ripper_lex_get_generic(struct parser_params *p, VALUE src)
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
ripper_lex_io_get(struct parser_params *p, VALUE src)
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
    struct parser_params *p;
    VALUE src, fname, lineno;

    TypedData_Get_Struct(self, struct parser_params, &parser_data_type, p);
    rb_scan_args(argc, argv, "12", &src, &fname, &lineno);
    if (RB_TYPE_P(src, T_FILE)) {
        p->lex.gets = ripper_lex_io_get;
    }
    else if (rb_respond_to(src, id_gets)) {
        p->lex.gets = ripper_lex_get_generic;
    }
    else {
        StringValue(src);
        p->lex.gets = lex_get_str;
    }
    p->lex.input = src;
    p->eofp = 0;
    if (NIL_P(fname)) {
        fname = STR_NEW2("(ripper)");
	OBJ_FREEZE(fname);
    }
    else {
	StringValueCStr(fname);
	fname = rb_str_new_frozen(fname);
    }
    parser_initialize(p);

    p->ruby_sourcefile_string = fname;
    p->ruby_sourcefile = RSTRING_PTR(fname);
    p->ruby_sourceline = NIL_P(lineno) ? 0 : NUM2INT(lineno) - 1;

    return Qnil;
}

static VALUE
ripper_parse0(VALUE parser_v)
{
    struct parser_params *p;

    TypedData_Get_Struct(parser_v, struct parser_params, &parser_data_type, p);
    parser_prepare(p);
    p->ast = rb_ast_new();
    ripper_yyparse((void*)p);
    rb_ast_dispose(p->ast);
    p->ast = 0;
    return p->result;
}

static VALUE
ripper_ensure(VALUE parser_v)
{
    struct parser_params *p;

    TypedData_Get_Struct(parser_v, struct parser_params, &parser_data_type, p);
    p->parsing_thread = Qnil;
    return Qnil;
}

/*
 *  call-seq:
 *    ripper.parse
 *
 *  Start parsing and returns the value of the root action.
 */
static VALUE
ripper_parse(VALUE self)
{
    struct parser_params *p;

    TypedData_Get_Struct(self, struct parser_params, &parser_data_type, p);
    if (!ripper_initialized_p(p)) {
        rb_raise(rb_eArgError, "method called for uninitialized object");
    }
    if (!NIL_P(p->parsing_thread)) {
        if (p->parsing_thread == rb_thread_current())
            rb_raise(rb_eArgError, "Ripper#parse is not reentrant");
        else
            rb_raise(rb_eArgError, "Ripper#parse is not multithread-safe");
    }
    p->parsing_thread = rb_thread_current();
    rb_ensure(ripper_parse0, self, ripper_ensure, self);

    return p->result;
}

/*
 *  call-seq:
 *    ripper.column   -> Integer
 *
 *  Return column number of current parsing line.
 *  This number starts from 0.
 */
static VALUE
ripper_column(VALUE self)
{
    struct parser_params *p;
    long col;

    TypedData_Get_Struct(self, struct parser_params, &parser_data_type, p);
    if (!ripper_initialized_p(p)) {
        rb_raise(rb_eArgError, "method called for uninitialized object");
    }
    if (NIL_P(p->parsing_thread)) return Qnil;
    col = p->lex.ptok - p->lex.pbeg;
    return LONG2NUM(col);
}

/*
 *  call-seq:
 *    ripper.filename   -> String
 *
 *  Return current parsing filename.
 */
static VALUE
ripper_filename(VALUE self)
{
    struct parser_params *p;

    TypedData_Get_Struct(self, struct parser_params, &parser_data_type, p);
    if (!ripper_initialized_p(p)) {
        rb_raise(rb_eArgError, "method called for uninitialized object");
    }
    return p->ruby_sourcefile_string;
}

/*
 *  call-seq:
 *    ripper.lineno   -> Integer
 *
 *  Return line number of current parsing line.
 *  This number starts from 1.
 */
static VALUE
ripper_lineno(VALUE self)
{
    struct parser_params *p;

    TypedData_Get_Struct(self, struct parser_params, &parser_data_type, p);
    if (!ripper_initialized_p(p)) {
        rb_raise(rb_eArgError, "method called for uninitialized object");
    }
    if (NIL_P(p->parsing_thread)) return Qnil;
    return INT2NUM(p->ruby_sourceline);
}

/*
 *  call-seq:
 *    ripper.state   -> Integer
 *
 *  Return scanner state of current token.
 */
static VALUE
ripper_state(VALUE self)
{
    struct parser_params *p;

    TypedData_Get_Struct(self, struct parser_params, &parser_data_type, p);
    if (!ripper_initialized_p(p)) {
	rb_raise(rb_eArgError, "method called for uninitialized object");
    }
    if (NIL_P(p->parsing_thread)) return Qnil;
    return INT2NUM(p->lex.state);
}

/*
 *  call-seq:
 *    ripper.token   -> String
 *
 *  Return the current token string.
 */
static VALUE
ripper_token(VALUE self)
{
    struct parser_params *p;
    long pos, len;

    TypedData_Get_Struct(self, struct parser_params, &parser_data_type, p);
    if (!ripper_initialized_p(p)) {
        rb_raise(rb_eArgError, "method called for uninitialized object");
    }
    if (NIL_P(p->parsing_thread)) return Qnil;
    pos = p->lex.ptok - p->lex.pbeg;
    len = p->lex.pcur - p->lex.ptok;
    return rb_str_subseq(p->lex.lastline, pos, len);
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

/*
 *  call-seq:
 *    Ripper.lex_state_name(integer)   -> string
 *
 *  Returns a string representation of lex_state.
 */
static VALUE
ripper_lex_state_name(VALUE self, VALUE state)
{
    return rb_parser_lex_state_name(NUM2INT(state));
}

void
Init_ripper(void)
{
    ripper_init_eventids1();
    ripper_init_eventids2();
    id_warn = rb_intern_const("warn");
    id_warning = rb_intern_const("warning");
    id_gets = rb_intern_const("gets");
    id_assoc = rb_intern_const("=>");

    (void)yystpcpy; /* may not used in newer bison */

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
    rb_define_method(Ripper, "state", ripper_state, 0);
    rb_define_method(Ripper, "token", ripper_token, 0);
    rb_define_method(Ripper, "end_seen?", rb_parser_end_seen_p, 0);
    rb_define_method(Ripper, "encoding", rb_parser_encoding, 0);
    rb_define_method(Ripper, "yydebug", rb_parser_get_yydebug, 0);
    rb_define_method(Ripper, "yydebug=", rb_parser_set_yydebug, 1);
    rb_define_method(Ripper, "debug_output", rb_parser_get_debug_output, 0);
    rb_define_method(Ripper, "debug_output=", rb_parser_set_debug_output, 1);
    rb_define_method(Ripper, "error?", ripper_error_p, 0);
#ifdef RIPPER_DEBUG
    rb_define_method(Ripper, "assert_Qundef", ripper_assert_Qundef, 2);
    rb_define_method(Ripper, "rawVALUE", ripper_value, 1);
    rb_define_method(Ripper, "validate_object", ripper_validate_object, 1);
#endif

    rb_define_singleton_method(Ripper, "dedent_string", parser_dedent_string, 2);
    rb_define_private_method(Ripper, "dedent_string", parser_dedent_string, 2);

    rb_define_singleton_method(Ripper, "lex_state_name", ripper_lex_state_name, 1);

<% @exprs.each do |expr, desc| -%>
    /* <%=desc%> */
    rb_define_const(Ripper, "<%=expr%>", INT2NUM(<%=expr%>));
<% end %>
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

/*
 * Local variables:
 * mode: c
 * c-file-style: "ruby"
 * End:
 */
