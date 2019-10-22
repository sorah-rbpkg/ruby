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
#line 12 "ripper.y" /* yacc.c:339  */


#if !YYPURE
# error needs pure parser
#endif
#define YYDEBUG 1
#define YYERROR_VERBOSE 1
#define YYSTACK_USE_ALLOCA 0
#define YYLTYPE rb_code_location_t
#define YYLTYPE_IS_DECLARED 1

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

    unsigned int command_start:1;
    unsigned int eofp: 1;
    unsigned int ruby__end__seen: 1;
    unsigned int debug: 1;
    unsigned int has_shebang: 1;
    unsigned int in_defined: 1;
    unsigned int in_kwarg: 1;
    unsigned int in_def: 1;
    unsigned int in_class: 1;
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
    unsigned int warn_location: 1;

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

static inline VALUE
ripper_new_yylval(struct parser_params *p, ID a, VALUE b, VALUE c)
{
    add_mark_object(p, b);
    add_mark_object(p, c);
    return (VALUE)NEW_CDECL(a, b, c, &NULL_LOC);
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
#endif

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
#define idFWD_KWREST idPow /* Use simple "**", as tDSTAR is "**arg" */
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
    struct rb_ary_pattern_info *apinfo = t->nd_apinfo;
    VALUE pre_args = Qnil, rest_arg = Qnil, post_args = Qnil;

    if (apinfo) {
        pre_args = rb_ary_entry(apinfo->imemo, 0);
        rest_arg = rb_ary_entry(apinfo->imemo, 1);
        post_args = rb_ary_entry(apinfo->imemo, 2);
    }

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
    struct rb_ary_pattern_info *apinfo;

    if (has_rest) {
	rest_arg = dispatch1(var_field, rest_arg ? rest_arg : Qnil);
    }
    else {
	rest_arg = Qnil;
    }

    VALUE tmpbuf = rb_imemo_tmpbuf_auto_free_pointer();
    apinfo = ZALLOC(struct rb_ary_pattern_info);
    rb_imemo_tmpbuf_set_ptr(tmpbuf, apinfo);
    apinfo->imemo = rb_ary_new_from_args(4, pre_args, rest_arg, post_args, tmpbuf);

    t = rb_node_newnode(NODE_ARYPTN, Qnil, Qnil, (VALUE)apinfo, &NULL_LOC);
    RB_OBJ_WRITTEN(p->ast, Qnil, apinfo->imemo);

    return (VALUE)t;
}

#define new_hash(p,h,l) rb_ary_new_from_args(0)

static VALUE
new_unique_key_hash(struct parser_params *p, VALUE ary, const YYLTYPE *loc)
{
    const long len = RARRAY_LEN(ary);
    st_table *tbl;
    long i;

    tbl = st_init_strtable_with_size(len);
    for (i = 0; i < len; i++) {
	VALUE key, a1, a2, a3;
	a1 = RARRAY_AREF(ary, i);
	if (!(RB_TYPE_P(a1, T_ARRAY) && RARRAY_LEN(a1) == 2)) goto error;
	a2 = RARRAY_AREF(a1, 0);
	if (!RB_TYPE_P(a2, T_ARRAY)) goto error;
	switch (RARRAY_LEN(a2)) {
	  case 2: /* "key": */
	    a3 = RARRAY_AREF(a2, 1);
	    if (!(RB_TYPE_P(a3, T_ARRAY) && RARRAY_LEN(a3) == 3)) goto error;
	    key = RARRAY_AREF(a3, 1);
	    break;
	  case 3: /* key: */
	    key = RARRAY_AREF(a2, 1);
	    break;
	  default:
	    goto error;
	}
	if (!RB_TYPE_P(key, T_STRING)) goto error;
	if (st_is_member(tbl, (st_data_t)RSTRING_PTR(key))) goto error;
	st_insert(tbl, (st_data_t)RSTRING_PTR(key), (st_data_t)ary);
    }
    st_free_table(tbl);
    return ary;

  error:
    ripper_error(p);
    st_free_table(tbl);
    return Qnil;
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
#endif /* RIPPER */

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

#line 1007 "ripper.c" /* yacc.c:339  */

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


/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
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
    tMETHREF = 151,
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
#line 962 "ripper.y" /* yacc.c:355  */

    VALUE val;
    NODE *node;
    ID id;
    int num;
    const struct vtable *vars;
    struct rb_strterm_struct *strterm;

#line 1177 "ripper.c" /* yacc.c:355  */
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



/* Copy the second part of user declarations.  */

#line 1207 "ripper.c" /* yacc.c:358  */

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
#define YYLAST   14009

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  155
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  253
/* YYNRULES -- Number of rules.  */
#define YYNRULES  748
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  1232

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
     154,    74,    72,    73,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,   153,   141,     2,     2,     2,   139,   134,     2,
     149,   150,   137,   135,   147,   136,    68,   138,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   129,   152,
     131,   127,   130,   128,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   146,    69,   151,   133,     2,   148,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   144,   132,   145,   142,     2,    88,    89,
      90,    91,    75,    76,    77,    78,    94,    95,    83,    82,
      79,    80,    81,    86,    87,    92,    93,    97,    84,    85,
      96,    98,     2,     2,     2,     2,     2,     2,     2,     2,
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
      65,    66,    67,    70,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   140,   143
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,  1160,  1160,  1160,  1186,  1192,  1199,  1206,  1213,  1219,
    1220,  1226,  1239,  1237,  1248,  1259,  1265,  1272,  1279,  1286,
    1292,  1297,  1296,  1306,  1306,  1313,  1320,  1330,  1338,  1345,
    1353,  1361,  1373,  1385,  1395,  1409,  1410,  1418,  1426,  1435,
    1442,  1445,  1453,  1461,  1470,  1478,  1486,  1494,  1502,  1512,
    1517,  1526,  1529,  1530,  1534,  1538,  1542,  1547,  1546,  1563,
    1566,  1573,  1573,  1573,  1579,  1580,  1583,  1584,  1593,  1603,
    1613,  1622,  1633,  1640,  1647,  1654,  1661,  1669,  1677,  1684,
    1691,  1700,  1701,  1710,  1711,  1720,  1727,  1734,  1741,  1748,
    1755,  1762,  1769,  1776,  1783,  1792,  1793,  1802,  1809,  1818,
    1825,  1834,  1841,  1848,  1855,  1865,  1872,  1882,  1889,  1896,
    1906,  1913,  1920,  1927,  1934,  1941,  1948,  1955,  1962,  1972,
    1979,  1982,  1989,  1996,  2005,  2006,  2007,  2008,  2013,  2020,
    2027,  2030,  2037,  2037,  2047,  2048,  2049,  2050,  2051,  2052,
    2053,  2054,  2055,  2056,  2057,  2058,  2059,  2060,  2061,  2062,
    2063,  2064,  2065,  2066,  2067,  2068,  2069,  2070,  2071,  2072,
    2073,  2074,  2075,  2076,  2079,  2079,  2079,  2080,  2080,  2081,
    2081,  2081,  2082,  2082,  2082,  2082,  2083,  2083,  2083,  2083,
    2084,  2084,  2084,  2085,  2085,  2085,  2085,  2086,  2086,  2086,
    2086,  2087,  2087,  2087,  2087,  2088,  2088,  2088,  2088,  2089,
    2089,  2089,  2089,  2090,  2090,  2093,  2100,  2107,  2115,  2123,
    2131,  2139,  2147,  2154,  2162,  2171,  2180,  2192,  2204,  2216,
    2228,  2232,  2236,  2240,  2244,  2248,  2252,  2256,  2260,  2264,
    2268,  2272,  2276,  2280,  2281,  2285,  2289,  2293,  2297,  2301,
    2305,  2309,  2313,  2317,  2321,  2325,  2325,  2330,  2339,  2345,
    2346,  2347,  2348,  2351,  2355,  2362,  2369,  2370,  2374,  2381,
    2390,  2395,  2406,  2413,  2432,  2433,  2436,  2437,  2438,  2442,
    2449,  2458,  2466,  2473,  2481,  2489,  2493,  2493,  2530,  2539,
    2543,  2549,  2556,  2563,  2570,  2579,  2580,  2583,  2590,  2597,
    2606,  2607,  2608,  2609,  2610,  2611,  2612,  2613,  2614,  2615,
    2616,  2624,  2623,  2638,  2638,  2645,  2645,  2653,  2661,  2668,
    2675,  2682,  2690,  2697,  2704,  2711,  2718,  2718,  2723,  2727,
    2731,  2738,  2739,  2748,  2747,  2758,  2769,  2780,  2790,  2801,
    2800,  2817,  2816,  2831,  2841,  2889,  2888,  2912,  2911,  2934,
    2933,  2957,  2962,  2956,  2982,  2983,  2982,  3007,  3014,  3021,
    3028,  3035,  3044,  3051,  3057,  3073,  3079,  3085,  3091,  3097,
    3103,  3109,  3115,  3121,  3127,  3133,  3139,  3145,  3151,  3166,
    3172,  3178,  3185,  3186,  3187,  3190,  3191,  3194,  3195,  3207,
    3208,  3217,  3218,  3221,  3229,  3238,  3245,  3254,  3261,  3268,
    3275,  3282,  3291,  3299,  3308,  3312,  3316,  3320,  3324,  3330,
    3335,  3340,  3344,  3348,  3352,  3356,  3360,  3368,  3372,  3376,
    3380,  3384,  3388,  3392,  3396,  3400,  3406,  3407,  3413,  3422,
    3430,  3442,  3446,  3455,  3457,  3461,  3466,  3472,  3475,  3479,
    3483,  3487,  3472,  3511,  3519,  3529,  3534,  3540,  3550,  3564,
    3571,  3578,  3587,  3596,  3604,  3612,  3619,  3627,  3635,  3642,
    3649,  3662,  3670,  3680,  3681,  3685,  3680,  3702,  3703,  3707,
    3702,  3726,  3734,  3741,  3749,  3758,  3770,  3771,  3775,  3782,
    3774,  3795,  3796,  3799,  3800,  3808,  3818,  3819,  3824,  3832,
    3836,  3842,  3845,  3854,  3857,  3864,  3867,  3868,  3876,  3884,
    3889,  3897,  3905,  3910,  3914,  3919,  3923,  3928,  3934,  3943,
    3947,  3956,  3960,  3964,  3968,  3972,  3975,  3979,  3988,  3992,
    3996,  4000,  4005,  4006,  4015,  4024,  4028,  4032,  4036,  4040,
    4046,  4047,  4056,  4063,  4073,  4088,  4110,  4114,  4120,  4126,
    4127,  4136,  4145,  4157,  4169,  4170,  4171,  4172,  4184,  4198,
    4199,  4200,  4201,  4202,  4203,  4204,  4205,  4206,  4214,  4213,
    4226,  4235,  4248,  4255,  4262,  4271,  4283,  4286,  4293,  4300,
    4303,  4307,  4310,  4317,  4320,  4321,  4324,  4341,  4342,  4343,
    4352,  4362,  4371,  4377,  4387,  4393,  4402,  4404,  4413,  4423,
    4429,  4438,  4447,  4457,  4463,  4473,  4479,  4489,  4495,  4505,
    4511,  4521,  4531,  4572,  4574,  4573,  4590,  4594,  4599,  4603,
    4607,  4589,  4628,  4635,  4642,  4649,  4652,  4653,  4656,  4666,
    4667,  4668,  4669,  4672,  4682,  4683,  4693,  4694,  4695,  4696,
    4699,  4700,  4701,  4702,  4703,  4706,  4707,  4708,  4709,  4710,
    4711,  4712,  4715,  4728,  4737,  4744,  4753,  4754,  4758,  4757,
    4767,  4775,  4784,  4784,  4798,  4802,  4806,  4810,  4814,  4820,
    4825,  4830,  4834,  4838,  4842,  4846,  4850,  4854,  4858,  4862,
    4866,  4870,  4874,  4878,  4882,  4886,  4898,  4904,  4913,  4921,
    4929,  4937,  4947,  4948,  4956,  4965,  4973,  4994,  4996,  5009,
    5019,  5027,  5037,  5044,  5053,  5060,  5070,  5077,  5086,  5087,
    5090,  5098,  5106,  5116,  5126,  5136,  5143,  5152,  5159,  5168,
    5169,  5172,  5180,  5190,  5191,  5194,  5204,  5208,  5214,  5219,
    5219,  5243,  5244,  5253,  5255,  5278,  5289,  5296,  5304,  5323,
    5324,  5325,  5328,  5329,  5330,  5331,  5334,  5335,  5336,  5339,
    5340,  5343,  5344,  5347,  5348,  5351,  5352,  5355,  5356,  5359,
    5362,  5365,  5366,  5367,  5370,  5371,  5374,  5375,  5379
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
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
  "\"constant\"", "\"class variable\"", "tLABEL", "\"integer literal\"",
  "\"float literal\"", "\"rational literal\"", "\"imaginary literal\"",
  "\"char literal\"", "\"numbered reference\"", "\"back reference\"",
  "\"literal content\"", "tREGEXP_END", "'.'", "\"backslash\"",
  "\"escaped space\"", "\"escaped horizontal tab\"",
  "\"escaped form feed\"", "\"escaped carriage return\"",
  "\"escaped vertical tab\"", "\"unary+\"", "\"unary-\"", "\"**\"",
  "\"<=>\"", "\"==\"", "\"===\"", "\"!=\"", "\">=\"", "\"<=\"", "\"&&\"",
  "\"||\"", "\"=~\"", "\"!~\"", "\"..\"", "\"...\"", "\"(..\"", "\"(...\"",
  "\"[]\"", "\"[]=\"", "\"<<\"", "\">>\"", "\"&.\"", "\"::\"", "\".:\"",
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
  "$@4", "command_asgn", "command_rhs", "expr", "@5", "expr_value",
  "expr_value_do", "$@6", "$@7", "command_call", "block_command",
  "cmd_brace_block", "fcall", "command", "mlhs", "mlhs_inner",
  "mlhs_basic", "mlhs_item", "mlhs_head", "mlhs_post", "mlhs_node", "lhs",
  "cname", "cpath", "fname", "fitem", "undef_list", "$@8", "op",
  "reswords", "arg", "$@9", "relop", "rel_expr", "arg_value", "aref_args",
  "arg_rhs", "paren_args", "opt_paren_args", "opt_call_args", "call_args",
  "command_args", "$@10", "block_arg", "opt_block_arg", "args", "mrhs_arg",
  "mrhs", "primary", "$@11", "$@12", "$@13", "$@14", "$@15", "@16", "@17",
  "$@18", "@19", "$@20", "@21", "@22", "@23", "@24", "primary_value",
  "k_begin", "k_if", "k_unless", "k_while", "k_until", "k_case", "k_for",
  "k_class", "k_module", "k_def", "k_do", "k_do_block", "k_rescue",
  "k_ensure", "k_when", "k_else", "k_elsif", "k_end", "k_return", "then",
  "do", "if_tail", "opt_else", "for_var", "f_marg", "f_marg_list",
  "f_margs", "f_rest_marg", "block_args_tail", "opt_block_args_tail",
  "block_param", "opt_block_param", "block_param_def", "opt_bv_decl",
  "bv_decls", "bvar", "lambda", "@25", "@26", "@27", "@28", "$@29",
  "f_larglist", "lambda_body", "do_block", "block_call", "method_call",
  "brace_block", "brace_body", "@30", "@31", "@32", "do_body", "@33",
  "@34", "@35", "case_args", "case_body", "cases", "p_case_body", "@36",
  "$@37", "p_cases", "p_top_expr", "p_top_expr_body", "p_expr", "p_as",
  "p_alt", "p_expr_basic", "p_args", "p_args_head", "p_args_tail",
  "p_args_post", "p_arg", "p_kwargs", "p_kwarg", "p_kw", "p_kwrest",
  "p_kwnorest", "p_value", "p_primitive", "$@38", "p_variable",
  "p_var_ref", "p_const", "opt_rescue", "exc_list", "exc_var",
  "opt_ensure", "literal", "strings", "string", "string1", "xstring",
  "regexp", "words", "word_list", "word", "symbols", "symbol_list",
  "qwords", "qsymbols", "qword_list", "qsym_list", "string_contents",
  "xstring_contents", "regexp_contents", "string_content", "@39", "$@40",
  "@41", "@42", "@43", "@44", "string_dvar", "symbol", "ssym", "sym",
  "dsym", "numeric", "simple_numeric", "user_variable", "keyword_variable",
  "var_ref", "var_lhs", "backref", "superclass", "$@45", "f_arglist",
  "@46", "args_tail", "opt_args_tail", "f_args", "args_forward",
  "f_bad_arg", "f_norm_arg", "f_arg_asgn", "f_arg_item", "f_arg",
  "f_label", "f_kw", "f_block_kw", "f_block_kwarg", "f_kwarg",
  "kwrest_mark", "f_no_kwarg", "f_kwrest", "f_opt", "f_block_opt",
  "f_block_optarg", "f_optarg", "restarg_mark", "f_rest_arg",
  "blkarg_mark", "f_block_arg", "opt_f_block_arg", "singleton", "$@47",
  "assoc_list", "assocs", "assoc", "operation", "operation2", "operation3",
  "dot_or_colon", "call_op", "call_op2", "opt_terms", "opt_nl", "rparen",
  "rbracket", "trailer", "term", "terms", "none", YY_NULLPTR
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
     130,   131,   145,   146,   136,   137,   150,   147,   151,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,    61,    63,    58,
      62,    60,   124,    94,    38,    43,    45,    42,    47,    37,
     352,    33,   126,   353,   123,   125,    91,    44,    96,    40,
      41,    93,    59,    32,    10
};
# endif

#define YYPACT_NINF -1045

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-1045)))

#define YYTABLE_NINF -749

#define yytable_value_is_error(Yytable_value) \
  (!!((Yytable_value) == (-749)))

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
   -1045,   110,  3965, -1045,  9381, -1045, -1045, -1045,  8835, -1045,
   -1045, -1045, -1045, -1045, -1045, -1045,  9508,  9508, -1045, -1045,
   -1045,  5270,  4826, -1045, -1045, -1045, -1045,   410,  8689,   -33,
      44,   194, -1045, -1045, -1045,  4086,  4974, -1045, -1045,  4234,
   -1045, -1045, -1045, -1045, -1045, -1045, -1045, -1045, 11159, 11159,
   11159, 11159,   294,  6876,  9635, 10016, 10397,  9121, -1045,  8543,
   -1045, -1045, -1045,   209,   244,   258,   284,   939, 11286, 11159,
   -1045,   306, -1045,  1299, -1045,   776, -1045, -1045,    51,   388,
     368, -1045,   351, 11540, -1045,   401,  3278,   538,   444,   557,
   -1045, 11413, 11413, -1045, -1045,  7865, 11663, 11786, 11909,  8396,
    9508,   437,    35, -1045, -1045,   425, -1045, -1045, -1045, -1045,
   -1045, -1045, -1045, -1045, -1045, -1045, -1045, -1045,   439,   499,
   -1045,   450,   544, -1045, -1045, -1045, -1045, -1045, -1045, -1045,
   -1045, -1045, -1045, -1045, -1045, -1045, -1045, -1045, -1045, -1045,
   -1045, -1045, -1045, -1045, -1045, -1045, -1045, -1045, -1045, -1045,
   -1045, -1045, -1045, -1045, -1045, -1045, -1045, -1045, -1045, -1045,
   -1045, -1045, -1045, -1045, -1045, -1045, -1045, -1045, -1045, -1045,
   -1045, -1045, -1045, -1045, -1045, -1045, -1045, -1045, -1045, -1045,
   -1045, -1045, -1045, -1045, -1045, -1045, -1045, -1045, -1045, -1045,
   -1045, -1045, -1045, -1045, -1045, -1045, -1045, -1045, -1045, -1045,
   -1045, -1045,   416, -1045, -1045, -1045,   434, 11159,   589,  7028,
   11159, 11159, 11159, -1045, 11159, -1045,   536,  4508,   579, -1045,
   -1045,   530,   636,    22,   253,   612,   363,   542, -1045, -1045,
    7738, -1045,  9508,  9762, -1045, -1045,  7992, -1045, 11413,   832,
   -1045,   587,  7180, -1045,  7332, -1045, -1045,   600,   602,    51,
   -1045,   659, -1045,   648,  4656,  4656,   681,  9635, -1045,  6876,
     604,   306, -1045,  1299,   -33,   656, -1045,  1299,   -33,   608,
     442,   495, -1045,   579,   630,   495, -1045,   -33,   743,   939,
   12032,   672, -1045,   697,   704,   794,   811, -1045, -1045, -1045,
   -1045, -1045,   448, -1045,   574,   853,   308, -1045, -1045, -1045,
   -1045,   753, -1045, -1045, -1045, -1045, -1045, -1045, -1045,  8119,
   11413, 11413, 11413, 11413,  9635, 11413, 11413, -1045, -1045, -1045,
     725, -1045, -1045, -1045, -1045, -1045, 10524, -1045,  6876,  9251,
     700, 10524, -1045, 11159, 11159, 11159, 11159, 11159, -1045, -1045,
   11159, 11159, 11159, 11159, 11159, 11159, 11159, 11159, 11159, -1045,
   -1045, 11159, 11159, 11159, 11159, 11159, 11159, 11159, 11159, 11159,
   11159, -1045, -1045, 12586,  3413,  9508, 12677,  6014,   776,    24,
      24,  7484, 11413,  7484,   306, -1045,   728,   802, -1045, -1045,
     833,   845,    63,    87,    94,  1057,  1081, 11413,   595, -1045,
     756,   838, -1045, -1045, -1045, -1045,    29,   225,   262,   357,
     405,   454,   464,   480,   539, -1045, -1045, -1045,   573, -1045,
   -1045, -1045,  3413, -1045, -1045, 11286, 11286, -1045, -1045,   445,
   -1045, -1045, -1045,   346, 11159, 11159,  9889, -1045, -1045, 12768,
    9508, 12859, 11159, 11159, 10143, -1045,   -33,   750, -1045, -1045,
   11159,   -33, -1045,   755,   -33,   757, -1045,    45, -1045, -1045,
   -1045, -1045, -1045,  8835, -1045, 11159,   765,   768, 12768, 12859,
   11159,  1299,    44,   -33, -1045, -1045,  8246,   770,   -33, -1045,
   -1045, 10270, -1045, -1045, 10397, -1045, -1045, -1045,   587,   850,
   -1045, -1045,   777, 12032, 12950,  9508, 13041, -1045, -1045, -1045,
   -1045, -1045, -1045, -1045, -1045, -1045, -1045, -1045, -1045,   888,
     263,   902,   276, 11159, -1045, -1045, -1045, -1045, -1045, -1045,
   -1045, -1045, -1045,   787, -1045, -1045, -1045,   927, -1045,   927,
   11159, -1045,   792,   796,   905, -1045,   -33, 12032,   804, -1045,
   -1045, -1045,   914,   834,  4212, -1045, -1045, -1045,  1073,   508,
   12311,   648,  2569,  2569,  2569,  2569,  4064,  3800,  2569,  2569,
    4656,  4656,   632,   632,  5389,  1259,  1259,  1283,   479,   479,
     648,   648,   648,  1403,  1403,  5418,  4382,  5714,  4530, -1045,
     602, -1045, -1045, -1045, -1045, -1045, -1045,   -33,   706,   712,
   -1045,  5122,   927,   956, -1045,  6166,   953,  6622,   927,    62,
     927,   940,   955,    98, 13132,  9508, 13223, -1045,   776, -1045,
     850, -1045, -1045, -1045, 13314,  9508, 13405,  6014, 11413, -1045,
   -1045, -1045, -1045,  3230, -1045,  4360, -1045, -1045, -1045,  8835,
   11159, -1045, 11159,   579, -1045,   542,  3673,  4678,   -33,   534,
     647, -1045, -1045, -1045, -1045,  9889, -1045, 10143, -1045, -1045,
   11413,  4508, -1045, -1045,   602,   602, -1045, -1045,   -29, -1045,
   -1045,   495, 12032,   777,   359,   643,   -33,   548,   566, -1045,
   -1045,   952, -1045,    40, -1045,   821, -1045, -1045,   453,   824,
   -1045,   648, -1045, -1045,   841, -1045, -1045, -1045, -1045,   836,
   10651,  9635, -1045,   777, 12032,  9635, 11286, 11159, 13496,  9508,
   13587, -1045, -1045, 12518, -1045,  2199,  2199,   842, 12518,  3810,
     615,    -1, -1045, -1045, -1045,   935, -1045,    34, -1045,   856,
   -1045, -1045, -1045,   847, -1045, -1045, -1045, -1045,   775, -1045,
   -1045,   215, -1045, -1045, -1045, -1045, -1045, -1045, -1045, -1045,
   -1045,    65,   883, 11286, 11286, -1045,   725,   867,   742, 11286,
   11286, -1045, -1045,   725, -1045, -1045, -1045, 10778,   699, -1045,
     913, -1045,  1004, -1045, -1045, -1045, -1045, -1045, -1045,   955,
     927, -1045, 10905,   927,   134,   229,   -33,   122,   129,  7484,
     306, 11413,  6014,  1141,   643, -1045,   -33,   927,    45,   877,
    8981,    35,   388, -1045, -1045, -1045, -1045, 11159, 11159,   729,
   11159, 11159,   881,    45, -1045, -1045,   588, -1045, -1045, -1045,
   -1045, -1045, -1045, -1045, -1045, -1045, -1045, -1045, -1045,    26,
   -1045,    26, 11159,   885, -1045,   777, -1045,  4508,  5566,  5862,
     -33,   738,   744,   929, -1045, -1045, -1045, -1045,   -17,   323,
     -33, 12398, -1045,   887, -1045, -1045, -1045,   892,   891, 12518,
   -1045,   698, -1045,   987, 12431, 12518,   344,  2199,  2199,   842,
    3530,  3530, -1045, -1045, 11159, -1045, -1045, -1045, -1045, -1045,
   -1045, 11286, -1045, -1045, -1045, -1045, -1045,   792, -1045,   941,
   -1045, -1045, -1045,  7484, -1045, -1045, -1045, -1045,  7484, 11413,
     927, -1045, -1045,   927, -1045, 12311,   927, -1045, 11159, -1045,
     103, -1045,   164,   927,  6014,   306,   927, -1045, -1045, -1045,
    1832,  6014,  1832, -1045, -1045, -1045, 11159, 10143, -1045,  1671,
   -1045, -1045,  1632,  7332, -1045, -1045,  6014,   896,   766, -1045,
   -1045,    20,   929,   898, -1045,   759, -1045, 12518,   899, -1045,
   -1045, 12518, -1045, -1045, -1045, -1045, -1045, -1045, -1045, -1045,
   -1045,   -33,   -33, -1045,   -33,   -33, -1045,  4508, -1045, -1045,
   12155,    24, -1045, -1045,  6749, -1045,    24, -1045, -1045,    24,
     901, -1045, -1045, 11032,  6318, -1045,   927, -1045, -1045, -1045,
   -1045, -1045, -1045, -1045, -1045, -1045,  1459, -1045, -1045, -1045,
   -1045, -1045,   -33, -1045, -1045, -1045,   922, -1045,   904, 11159,
   -1045,   907,   104,   908,   908, -1045,   916,  1008,   918,  1015,
   -1045,   927,   306,   877,  1832, -1045, -1045, -1045, -1045,   -33,
     937,   943,   928, 12278, -1045,   931,   908,   908, -1045,   934,
     938, -1045,   936, -1045, -1045,   942, 12518, -1045,   899, 12518,
     929, -1045, -1045, -1045, -1045,  1018, -1045,  1126,    90,   112,
     123,  6014,  1077,  6166, -1045, 11413, 11413, 11159, -1045,   614,
   -1045,  1459,  1210, -1045,   945,   -33,   951, -1045, -1045, 11159,
    1922, -1045, -1045,   300, -1045, -1045, -1045, -1045,   172, -1045,
   -1045,  1922, -1045, -1045,  1386, -1045, -1045, -1045, -1045,  6014,
     -33,    56,  7611,   969, -1045, 12278,  1922, -1045,  1052,  1145,
     300, -1045, -1045, -1045,  1922, -1045,  1386, -1045,  1227, 12518,
     899, -1045,   246, 13678,  9508, 13769,   956, -1045,   913,  6470,
   -1045, -1045, -1045, -1045, -1045, -1045,   -33, -1045,  1459, -1045,
    1270, -1045, -1045, -1045,   962,   965, -1045,  1061,   908, -1045,
     968, -1045,   970, -1045,   968,   927,   966,  6014,  7332, -1045,
    1000, -1045,  1145, -1045,   977,   980, -1045, 13860, -1045,   908,
     981, -1045,   990,   981, -1045,   576, -1045, -1045,   899,   191,
     195,   -33,   247,   286, -1045, -1045,   379, -1045, -1045,   991,
     992,  1922, -1045,  1386, -1045, -1045,  1386, -1045,  1386, -1045,
   -1045, -1045, -1045,   927,   997, -1045,  1922, -1045,  1386, -1045,
     994,   995, -1045,  1386, -1045,  1386, -1045, -1045,  1227, -1045,
     313, -1045, -1045, -1045,  1270,  1270,   968,   998,   968,   968,
   -1045, -1045,   981,  1003,   981,   981, -1045,   992, -1045,  1386,
   -1045, -1045, -1045, -1045,  1386, -1045, -1045, -1045,   968,   981,
   -1045, -1045
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       2,     0,     0,     1,     0,   360,   361,   362,     0,   353,
     354,   355,   358,   356,   357,   359,   347,   348,   349,   350,
     371,   276,   276,   626,   625,   627,   628,   737,     0,   737,
       0,     0,   630,   629,   631,   719,   721,   622,   621,   720,
     624,   616,   617,   618,   619,   567,   636,   637,     0,     0,
       0,     0,     0,     0,   303,   748,   748,    93,   323,   587,
     587,   589,   591,     0,     0,     0,     0,     0,     0,     0,
       3,   735,     6,     9,    35,    40,    52,    65,   276,    64,
       0,    81,     0,    85,    95,     0,    59,   233,   248,     0,
     301,     0,     0,    61,    61,   735,     0,     0,     0,     0,
     312,    66,   321,   290,   291,   566,   568,   292,   293,   294,
     296,   295,   297,   565,   606,   607,   564,   614,   632,   633,
     298,     0,   299,    69,     5,     8,   174,   185,   175,   198,
     171,   191,   181,   180,   201,   202,   196,   179,   178,   173,
     199,   203,   204,   183,   172,   186,   190,   192,   184,   177,
     193,   200,   195,   194,   187,   197,   182,   170,   189,   188,
     169,   176,   167,   168,   164,   165,   166,   124,   126,   125,
     159,   160,   155,   137,   138,   139,   146,   143,   145,   140,
     141,   161,   162,   147,   148,   152,   156,   142,   144,   134,
     135,   136,   149,   150,   151,   153,   154,   157,   158,   163,
     129,   131,    28,   127,   128,   130,     0,     0,     0,     0,
       0,     0,     0,   587,     0,   271,     0,   255,   281,    79,
     275,   748,     0,   632,   633,     0,   299,   748,   713,    80,
     737,    77,     0,   748,   448,    76,   737,   738,     0,     0,
      23,   245,     0,    10,     0,   347,   348,   315,   449,     0,
     227,     0,   312,   228,   218,   219,   309,     0,    21,     0,
       0,   735,    17,    20,   737,    83,    16,   305,   737,     0,
     741,   741,   256,     0,     0,   741,   711,   737,     0,     0,
       0,    91,   352,     0,   101,   102,   109,   427,   611,   610,
     612,   609,     0,   608,     0,     0,     0,   574,   583,   579,
     585,   615,    56,   239,   240,   744,   745,     4,   746,   736,
       0,     0,     0,     0,     0,     0,     0,   363,   453,   442,
      70,   457,   320,   364,   457,   438,     0,    97,     0,    89,
      86,     0,    57,     0,     0,     0,     0,     0,   251,   252,
       0,     0,     0,     0,   216,   217,     0,     0,     0,   249,
     250,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   731,   732,     0,     0,   748,     0,     0,    60,     0,
       0,     0,     0,     0,   735,   331,   736,     0,   382,   381,
       0,     0,   632,   633,   299,   119,   120,     0,     0,   122,
     640,     0,   632,   633,   299,   339,   194,   187,   197,   182,
     164,   165,   166,   124,   125,   709,   341,   708,     0,    78,
     734,   733,     0,   322,   569,     0,     0,   132,   716,   309,
     282,   718,   278,     0,     0,     0,     0,   272,   280,     0,
     748,     0,     0,     0,     0,   273,   737,     0,   314,   277,
     667,   737,   267,   748,   737,   748,   266,   737,   319,    55,
      25,    27,    26,     0,   316,     0,     0,     0,     0,     0,
       0,    19,     0,   737,   307,    15,   736,    82,   737,   304,
     310,   743,   742,   257,   743,   259,   311,   712,     0,   108,
     615,    99,    94,     0,     0,   748,     0,   324,   428,   593,
     613,   596,   594,   588,   570,   571,   590,   572,   592,     0,
       0,     0,     0,     0,   747,     7,    29,    30,    31,    32,
      33,    53,    54,     0,   454,   453,    71,     0,   458,     0,
       0,    36,   286,     0,    39,   285,   737,     0,    87,    98,
      51,    41,    49,     0,   260,   281,   205,    37,     0,   299,
       0,   225,   232,   234,   235,   236,   243,   244,   237,   238,
     214,   215,   241,   242,   737,   229,   230,   231,   220,   221,
     222,   223,   224,   253,   254,   722,   724,   723,   725,   447,
     276,   445,   722,   724,   723,   725,   351,   737,   722,   723,
     446,   276,     0,   748,   373,     0,   372,     0,     0,     0,
       0,   329,     0,   309,     0,   748,     0,    61,   337,   119,
     120,   121,   638,   335,     0,   748,     0,     0,     0,   342,
     729,   730,   344,   276,    42,   260,   206,    48,   213,     0,
       0,   715,     0,   283,   279,   748,   722,   723,   737,   722,
     723,   714,   313,   739,   262,   268,   263,   270,   318,    24,
       0,   246,    11,    34,     0,   748,   212,    22,    84,    18,
     306,   741,     0,    92,   726,   107,   737,   722,   723,   429,
     597,     0,   573,     0,   576,     0,   581,   578,     0,     0,
     582,   226,   451,   455,     0,   370,   452,   459,   437,   289,
       0,     0,    96,    90,     0,     0,     0,     0,     0,   748,
       0,   550,   554,   523,   688,     0,     0,     0,     0,   737,
       0,   510,   689,   548,   587,     0,    58,   476,   481,   483,
     485,   479,   480,   516,   520,   517,   519,   486,   529,   534,
     535,   536,   539,   540,   541,   542,   543,   545,   544,   546,
     547,   527,     0,     0,     0,   444,    74,     0,   450,     0,
       0,   265,   443,    72,   264,   302,   365,   748,   748,   556,
     748,   374,   748,   327,   376,    62,   375,   328,   468,     0,
       0,   367,     0,     0,   726,   308,   737,   722,   723,     0,
       0,     0,     0,   119,   120,   123,   737,     0,   737,   642,
       0,   439,    67,   133,   717,   284,   274,     0,     0,   450,
       0,     0,   748,   737,   258,   100,   450,   430,   598,   602,
     603,   604,   595,   605,   575,   577,   584,   580,   586,   748,
      68,   748,     0,   287,    38,    88,    50,   261,   722,   723,
     737,   722,   723,   522,   537,   538,   120,   552,   737,   498,
     737,   499,   505,     0,   494,   587,   496,     0,   508,     0,
     427,     0,   551,     0,   477,     0,     0,   532,   533,     0,
     737,   737,   528,   526,     0,    47,   210,    46,   211,    75,
     740,     0,    44,   208,    45,   209,    73,   557,   558,   748,
     559,   366,   368,     0,    12,    14,   563,   369,     0,     0,
       0,   377,   379,     0,    63,     0,     0,   333,     0,   461,
       0,   332,   450,     0,     0,     0,     0,   450,   340,   710,
     666,     0,   666,   345,   440,   441,     0,   269,   317,   666,
     599,   419,   737,     0,   417,   416,     0,   288,   450,   497,
     493,   503,   514,   500,   506,     0,   495,     0,   511,   512,
     549,   525,   482,   478,   484,   521,   515,   518,   530,   531,
     553,   737,   737,   492,   737,   737,   489,   247,    43,   207,
       0,     0,   561,   562,     0,   380,     0,   325,   326,     0,
     473,   330,   462,     0,     0,   334,     0,   639,   336,   673,
     670,   669,   668,   671,   679,   667,     0,   700,   704,   703,
     699,   664,   737,   665,   672,   674,   675,   677,   650,   681,
     686,   748,   692,   748,   748,   697,   650,   702,   650,     0,
     648,     0,     0,   642,   666,   431,   434,   600,   415,   737,
       0,   675,   400,   683,   684,   748,   748,   748,   695,   400,
     400,   398,   421,   456,   460,   501,     0,   507,   509,     0,
     524,   490,   491,   487,   488,     0,   560,     0,   632,   633,
     299,     0,   748,     0,   469,     0,     0,     0,   463,   748,
     338,     0,   393,   385,   387,   737,   390,   383,   641,     0,
       0,   657,   680,     0,   645,   707,   690,   691,     0,   647,
     646,     0,   660,   701,     0,   662,   705,   343,   643,     0,
     737,     0,     0,     0,   418,     0,   406,   408,     0,   682,
       0,   395,   397,   396,     0,   411,     0,   413,     0,     0,
     504,   513,   309,     0,   748,     0,   748,    13,   748,     0,
     474,   475,   464,   466,   467,   465,   737,   392,     0,   676,
       0,   693,   649,   678,   650,   650,   687,   692,   748,   706,
     650,   698,   650,   675,   650,     0,     0,     0,     0,   432,
       0,   420,   694,   399,   400,   400,   309,     0,   685,   748,
     400,   696,   400,   400,   425,   737,   423,   426,   502,   726,
     308,   737,   722,   723,   555,   378,   748,   384,   386,   388,
     391,     0,   653,     0,   655,   644,     0,   661,     0,   658,
     663,   346,   433,     0,     0,   601,     0,   403,     0,   405,
     726,   308,   394,     0,   412,     0,   409,   414,     0,   422,
     450,   471,   472,   470,     0,     0,   650,   650,   650,   650,
     436,   435,   400,   400,   400,   400,   424,   389,   654,     0,
     651,   656,   659,   404,     0,   401,   407,   410,   650,   400,
     652,   402
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1045, -1045, -1045,   910, -1045,    18,   696,  -487, -1045,   -38,
   -1045,   694, -1045,    37, -1045,  -307,  -203,   -83, -1045,   -64,
     -76, -1045, -1045,   -34, -1045,     1,   820,   -10,  1065,  -164,
       3,   -24, -1045,  -423,   -27,  2150,  -365,  1067,   -54,   -14,
   -1045, -1045,    -7, -1045,  3102, -1045,  1076, -1045,  1375, -1045,
      64,   -12,   555,  -352,    48,    -3, -1045,  -363,  -210,    41,
   -1045,  -311,   -28, -1045, -1045, -1045, -1045, -1045, -1045, -1045,
   -1045, -1045, -1045, -1045, -1045, -1045, -1045,    77, -1045, -1045,
   -1045, -1045, -1045, -1045, -1045, -1045, -1045, -1045, -1045, -1045,
   -1045, -1045, -1045,   427, -1045,  -392,  1019,  -337, -1045,    68,
    -710, -1045, -1044, -1034,   131,    66,   268,   154, -1045,   372,
   -1045,  -936, -1045,    -9,   348, -1045, -1045, -1045, -1045, -1045,
   -1045, -1045,   408, -1045, -1045,   -91,   683, -1045, -1045, -1045,
     872, -1045, -1045, -1045, -1045,  -732, -1045,    33, -1045, -1045,
   -1045, -1045,   315,  -333, -1045, -1045,   360,  -185, -1045,  -537,
    -902,  -726,  -665, -1045,   362,   366,   375, -1045,  -478, -1045,
     383, -1045, -1045,   125, -1045, -1045,   187,   617,   718, -1045,
    1127,   979,  1740,  1758, -1045,   733,  1847, -1045,  2056,  2118,
   -1045, -1045,   -56, -1045, -1045,  -181, -1045, -1045, -1045, -1045,
   -1045, -1045, -1045,     8, -1045, -1045, -1045, -1045,   -21,  2141,
    1266,  1146,  2267,  1759, -1045, -1045,   237, -1045,  -424,    86,
    -822,  1011,  -911,  -909,  -226, -1010,   305,     9,   183,   157,
   -1045, -1045,  -433,  -127,  -310,  -880,  -962,   168,   200, -1045,
    -571, -1045,  -191,  -865, -1045, -1045, -1045,    84,  -372, -1045,
    -161, -1045, -1045,   -57, -1045,   -46,    11,   812,  -304,  -228,
     -69,   -23,    -2
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,    70,    71,    72,   243,   582,   954,   583,
     261,   262,   462,   263,   453,    74,   531,    75,   540,   369,
     371,   372,   884,    76,    77,   516,   249,    79,    80,   264,
      81,    82,    83,   482,    84,   216,   389,   390,   200,   201,
     202,   619,   575,   204,    86,   455,   359,    87,   218,   269,
     536,   569,   742,   441,   442,   231,   232,   220,   427,   443,
     524,   525,    88,   367,   268,   468,   640,   287,   759,   592,
     772,   770,   607,   609,   779,   780,  1003,   251,    90,    91,
      92,    93,    94,    95,    96,    97,    98,    99,   321,   324,
     747,   873,   762,   878,   879,   676,   252,   585,   755,   880,
     881,   381,  1053,  1054,  1055,  1056,  1143,  1087,  1009,   913,
     914,  1010,  1155,  1156,   487,   488,   659,   797,   909,  1081,
    1005,  1139,   325,   101,   102,   322,   513,   514,   673,   809,
     517,   518,   677,   811,   890,   763,  1115,   760,   885,  1109,
    1203,   959,   706,   922,   708,   709,   710,   830,   831,   832,
     928,   833,   712,   713,   714,   715,   716,   717,   718,   840,
     719,   720,   721,   748,   869,   951,   875,   103,   104,   105,
     106,   107,   108,   109,   499,   663,   110,   501,   111,   112,
     500,   502,   292,   295,   296,   493,   661,   660,   798,   910,
    1007,  1082,   802,   113,   114,   293,   115,   116,   117,   223,
     224,   120,   225,   226,   603,   771,   901,   902,  1122,  1061,
     982,   983,   984,   985,  1133,   987,   988,   989,   990,  1014,
    1015,   991,   992,   993,   994,   995,  1018,  1019,   996,   997,
     998,   999,  1000,  1064,   408,   608,   274,   445,   228,   123,
     644,   571,   612,   606,   412,   307,   437,   438,   738,   473,
     586,   376,   266
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     124,   203,   308,   711,   294,   291,   215,   215,   368,   368,
     234,   413,   368,   577,   240,   260,   205,   435,   373,   235,
     537,   203,   125,   601,   530,  1028,   308,   886,   370,   282,
     281,   374,   366,   587,   302,   837,   205,   584,   238,    73,
     241,    73,   883,   475,   411,   406,   301,   477,   309,   375,
    1123,   838,   203,   272,   276,   282,   265,   221,   221,   330,
     653,   317,   631,   624,   219,   229,   319,  1057,   282,   282,
     282,   624,  1025,  1083,  1168,   320,  1123,   317,   628,    89,
    1002,    89,   315,   316,   843,  1137,  1170,  1006,  -110,   754,
     215,   267,   203,   222,   222,   463,   270,  -626,   -82,   852,
     227,   227,   631,  -110,   683,   923,   489,   731,   530,   530,
       3,   911,  -111,   929,   496,   498,   584,   853,   -96,  -118,
     777,   237,  -634,  -117,  1100,  -111,  -626,   678,  1069,  1070,
      89,    89,  1151,   656,   283,   843,  -118,   237,  1066,   271,
     275,   221,  1057,  1117,  1136,   222,   839,  -113,   409,  -110,
    1091,  1092,  1093,   447,  -115,   449,  1067,   423,   912,  -114,
     283,  1168,   491,   492,   526,   431,  1123,  1026,   222,   222,
    1217,   260,   222,   380,   391,   391,   305,   222,   306,   318,
    1138,   844,  1080,  1123,   227,   942,   945,  1157,   242,  -112,
     745,  1131,   308,   804,   459,   318,   753,  1158,   757,   237,
     233,   929,   570,   576,  -114,   581,   457,   707,  -116,  1057,
    -101,  1057,   614,   617,   305,   465,   306,   824,   825,   428,
     215,   260,   215,   215,  1151,   428,   486,   368,   368,   368,
     368,   446,   511,   512,  -102,   435,   234,   319,   466,   775,
     124,  -109,  -110,   766,  -110,  -108,   506,   507,   508,   509,
     963,   613,   282,   776,  -116,   305,   481,   306,   480,  -117,
    -113,   815,   265,  1175,  -111,   631,  -111,   731,   570,  -104,
     581,   221,   624,   221,   624,  -118,  -106,  -118,   436,    73,
     439,  -105,   978,  -722,  1192,   896,    89,  1157,   478,   368,
     260,  1131,   521,  -625,   461,  1057,  1057,   532,   645,  -115,
     929,   282,   528,  1101,   598,   308,   979,   222,   589,   222,
     222,  -103,   849,   222,   227,   222,   227,  1114,   664,    89,
     664,    89,  -625,   596,   789,   645,  -112,   505,   591,   665,
    -627,   265,   827,   588,    89,   590,    89,   820,   244,  1113,
    -722,  1020,   669,  -114,  -723,  -114,    73,  -116,   711,  -116,
     256,   510,   796,  -635,   580,   215,   568,   283,   974,  -627,
     823,   850,   297,   446,   851,   828,   829,   523,   887,   938,
     939,   891,   523,   929,   489,   497,  -107,   694,  -723,   530,
    -111,   532,   532,   666,  -105,   898,    89,   222,   222,   222,
     222,    89,   222,   222,   872,   834,   670,   298,  -117,  -113,
    -117,  -113,   693,   222,   758,    89,   283,   966,   538,   702,
     978,   299,   489,   731,  1001,   786,   323,   731,   731,   580,
     215,   694,   568,   794,   843,  -628,   530,   530,   446,  1024,
     491,   492,   530,   530,   979,   645,   868,   300,  -115,   639,
    -115,   428,   222,   428,    89,   645,   203,   580,    89,   222,
      89,   568,   731,   702,  -628,   282,  1201,   835,   305,   481,
     306,   205,   892,   433,   222,  -112,   494,  -112,   491,   492,
    -514,   620,   897,  -630,   580,   215,   981,   568,   981,   616,
     618,   690,   805,   446,   940,   981,  -105,   805,   957,  1125,
    -118,   958,   538,   538,   961,   326,   616,   618,   327,   282,
    1132,   965,  -630,   481,   968,   361,  -105,   222,  -722,  -105,
     625,   829,  -352,  -105,   489,  1145,   918,   829,   829,   489,
     756,   769,  -629,  1152,   646,   778,   920,   570,   331,   581,
     855,   857,  -631,   362,   410,   631,   862,   864,    60,  -634,
    -352,  -352,  -352,    89,   624,   460,   943,   750,  -620,   752,
     415,  -629,   707,   964,   530,   651,   333,   793,   735,   236,
     283,  -631,   222,   417,   237,   732,  -110,   736,   490,   741,
     491,   492,  -117,  -104,  1050,   491,   492,  -620,   743,   744,
     981,   749,   786,   236,   580,   215,  -101,   568,   737,   471,
    -352,  -106,  1135,   446,   580,   215,   472,   568,  1030,  -635,
    1207,   741,  1017,   446,   283,   783,   807,  -623,   416,  1077,
     782,   744,   203,  -103,  1041,  1213,   356,   357,   358,  1043,
     338,   339,  1044,   428,   282,   361,  -111,   205,   795,   872,
    1127,   761,   735,   741,   790,  -118,  -623,  1031,  1032,   737,
     489,   610,   474,   744,   416,   419,  -102,   599,   841,   472,
    1183,   600,   532,   362,   363,   364,   282,  1127,   948,   933,
     481,  -113,    89,   424,    89,   941,   944,   737,   349,   350,
     611,  -118,   222,   693,   986,  -104,   986,   426,   580,   215,
     425,   568,   222,   986,    89,   222,  1011,   446,   368,   434,
     904,  -109,   694,  -106,   494,  -104,   491,   492,  -104,   532,
     532,   894,  -104,   365,   361,   532,   532,   895,   871,   333,
     737,  -308,   432,  -106,   872,  -103,  -106,   222,   814,   792,
    -106,  1021,   816,  1198,   702,   333,   903,   361,   835,   283,
     237,   893,   362,   429,   364,  -103,   454,   859,  -103,  -308,
    -308,  -308,  -103,  1181,   866,   870,   876,   791,   882,   230,
     882,   233,  1161,  1128,   464,   362,   458,   364,    89,   470,
     836,   283,    89,   538,   489,   361,   222,   354,   355,   356,
     357,   358,  -632,   203,  -115,   476,  1017,   737,   986,   925,
    1149,   460,   430,   -81,  1017,  1016,  1017,   737,   523,  -308,
     428,  1210,  -723,   362,   484,   364,   368,   856,   858,   479,
    -632,  -632,  -632,   863,   865,   430,   739,   915,  -117,   915,
     538,   538,   740,   315,   316,   956,   538,   538,   494,   483,
     491,   492,    78,   931,    78,   489,   967,   532,  -108,   906,
     503,   737,   515,  -113,   986,   953,    78,    78,   739,  -115,
     955,   737,   861,   485,   740,   986,    89,   529,   222,    89,
    -632,   856,   858,  -104,   863,   865,  -112,  1200,   593,  -106,
    1011,   737,  -633,   847,   848,  -113,   861,   952,  1011,  -112,
     597,  -115,  1129,    78,    78,  1023,  1017,  1129,  1017,  -299,
     504,   491,   492,  1017,   931,  1017,   450,   602,    78,  -103,
    -633,  -633,  -633,  -112,   599,  1021,   451,   452,   826,  1129,
     633,   361,   635,  1021,   637,  1021,   361,  -299,  -299,  -299,
     642,    78,    78,   643,  1017,    78,  1042,   -96,  -309,   489,
      78,  1013,   282,  1022,   652,   949,  1049,   877,   872,   362,
     594,   364,   672,  1078,   362,   604,   364,   675,   538,  -281,
    -633,  1045,  1046,   680,   645,   986,  -309,  -309,  -309,   681,
      89,   684,   737,   737,   489,    89,   222,  -299,   685,  1016,
    1011,   686,   368,   368,   746,   758,   751,  1016,   489,  1016,
     949,    89,   761,   495,   806,   491,   492,   808,    89,   595,
    1105,  1110,  1111,  -282,   605,   282,   810,   842,   845,  1065,
      89,  1065,  1065,    89,   846,  1021,  -309,  1021,    41,    42,
      43,    44,  1021,  1106,  1021,  1108,   799,   800,   662,   801,
     491,   492,   854,  1065,  1065,  1065,    46,    47,   860,   872,
    1022,   100,   667,   100,   491,   492,   900,  1037,   907,    78,
     843,    89,  -283,  1021,   924,   100,   100,   926,   927,   691,
     876,    89,   950,  -284,  1140,  1027,  1029,   882,   448,  1059,
      78,  1060,    78,    78,  1063,  1068,    78,   282,    78,  1016,
    1073,  1016,    78,  1071,    78,  1074,  1016,  1076,  1016,  1084,
    1085,  1166,   100,   100,  1102,  1086,   467,    78,  1090,    78,
     469,  1094,  1072,  -719,  1075,  1096,   871,   100,  1098,  1099,
    1089,  1022,  1118,   580,   215,  1013,   568,  1016,  1120,  1013,
    1184,  1141,   446,  1013,   749,  1013,   882,  -720,  1146,  1171,
     100,   100,  1173,  1067,   100,  1176,  1182,  1178,    89,   100,
      89,  1185,   222,   222,  1186,  -620,  1065,  1188,  1193,    78,
      78,    78,    78,    78,    78,    78,    78,  1195,  1204,  1205,
     568,   361,  1211,  -722,  -723,  1219,    78,  1065,    78,  -623,
    1224,    78,   456,  -620,  -620,  -620,    89,   722,   647,    89,
     649,   378,  1142,   360,   882,   395,  1199,  -726,   781,   362,
     688,   364,   737,  1095,  1097,   874,  1165,  -623,  -623,  -623,
    1008,   222,  1116,   916,  1169,    78,    89,    78,   930,  1216,
     905,    78,    78,    78,   361,  1013,   519,  1013,   674,  1202,
     960,  -719,  1013,  -620,  1013,   934,  -719,    78,   935,  -726,
    1172,  1174,   936,   361,    89,    89,  1177,  1012,  1179,   689,
    1180,   937,   362,  1103,   364,  -720,   932,  -623,   100,  1107,
    -720,  1164,   414,  1013,   668,    78,    78,  -726,  -726,  -726,
    1079,   362,  1147,   364,   444,   407,  1126,  1148,   632,   100,
      78,   100,   100,   634,  1144,   100,   636,   100,   723,   638,
    1124,   100,   969,   100,   970,   971,   972,   973,   119,     0,
     119,     0,  1104,     0,     0,   648,   100,     0,   100,  1154,
     650,   970,   971,   972,   973,  -726,    78,  -726,     0,     0,
    -722,   605,  1218,  1220,  1221,  1222,     0,     0,  1187,  1189,
       0,     0,     0,     0,  1194,    78,  1196,  1197,     0,     0,
     722,     0,   722,   722,  1230,   722,   722,     0,     0,   119,
     119,     0,   969,   285,   970,   971,   972,   973,   100,   100,
     100,   100,   100,   100,   100,   100,   333,     0,   682,   310,
     311,   312,   313,   314,     0,   100,     0,   100,     0,   285,
     100,     0,     0,   346,   347,     0,     0,     0,     0,     0,
     333,     0,   383,   393,   393,   393,  1223,  1225,  1226,  1227,
       0,     0,  1051,     0,     0,     0,  1130,   346,   347,  1134,
       0,     0,     0,  1231,   100,     0,   100,     0,     0,     0,
     100,   100,   100,   353,   354,   355,   356,   357,   358,  1150,
       0,  1153,     0,     0,     0,    78,   100,    78,     0,     0,
       0,   723,     0,   723,   723,    78,   723,   723,   354,   355,
     356,   357,   358,     0,     0,    78,     0,    78,    78,     0,
       0,   273,     0,     0,   100,   100,     0,     0,   969,     0,
     970,   971,   972,   973,   974,     0,     0,     0,   722,   100,
       0,     0,     0,     0,     0,     0,   722,     0,     0,     0,
      78,   722,   722,   694,   722,   722,     0,   722,   722,     0,
       0,     0,     0,     0,     0,   119,  1206,     0,  1208,     0,
     333,     0,     0,  1209,     0,   100,     0,     0,   976,     0,
       0,  1212,     0,  1214,     0,   702,   978,   346,   347,     0,
    1215,    78,   722,     0,   100,    78,    78,     0,   119,    78,
     119,   969,     0,   970,   971,   972,   973,     0,     0,   724,
     979,     0,     0,   119,  1228,   119,     0,     0,     0,  1229,
       0,     0,     0,     0,     0,   351,   352,   353,   354,   355,
     356,   357,   358,     0,   722,     0,   285,     0,   722,   723,
       0,     0,     0,    78,    78,     0,     0,   723,     0,    78,
      78,  1051,   723,   723,     0,   723,   723,  1052,   723,   723,
       0,     0,     0,     0,     0,   119,     0,     0,     0,     0,
     119,     0,   418,     0,     0,   420,   421,   422,     0,    78,
     899,    78,    78,     0,   119,   285,     0,     0,     0,     0,
       0,     0,     0,   723,   100,   908,   100,     0,     0,     0,
       0,     0,     0,     0,   100,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   100,     0,   100,   100,     0,     0,
       0,     0,     0,   119,     0,     0,     0,   119,     0,   119,
     919,     0,     0,   722,     0,   723,   722,     0,     0,   723,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   100,
       0,     0,     0,   946,     0,     0,     0,     0,     0,     0,
       0,     0,   724,     0,   724,   724,     0,   724,   724,     0,
       0,    78,     0,     0,   969,     0,   970,   971,   972,   973,
     974,     0,     0,    78,     0,     0,     0,     0,    78,    78,
     100,   522,     0,     0,   100,   100,   535,     0,   100,   694,
       0,     0,     0,     0,    78,     0,   722,     0,     0,     0,
       0,    78,     0,   969,     0,   970,   971,   972,   973,   974,
       0,     0,   119,    78,   976,     0,    78,     0,     0,     0,
     977,   702,   978,     0,   723,     0,     0,   723,   694,   285,
       0,     0,   100,   100,     0,     0,  1033,  1034,   100,   100,
       0,   122,   975,   122,     0,     0,   979,     0,     0,   980,
       0,     0,     0,   976,    78,     0,     0,     0,     0,   977,
     702,   978,     0,     0,    78,     0,   237,     0,   100,     0,
     100,   100,     0,   285,  1058,     0,     0,     0,     0,     0,
     621,   623,     0,     0,     0,   979,   730,     0,   980,   273,
     724,     0,   122,   122,     0,     0,   286,   723,   724,     0,
    1004,     0,     0,   724,   724,     0,   724,   724,     0,   724,
     724,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   286,     0,     0,     0,   623,     0,     0,   273,
       0,   119,     0,   119,     0,   384,   394,   394,     0,     0,
       0,    78,     0,    78,   724,    78,    78,  1119,     0,     0,
       0,     0,     0,   119,     0,     0,     0,     0,     0,     0,
     100,     0,     0,     0,   969,     0,   970,   971,   972,   973,
     974,     0,   100,     0,     0,   679,     0,   100,   100,    78,
       0,     0,    78,     0,     0,     0,   724,     0,     0,   694,
     724,     0,     0,   100,     0,     0,     0,     0,   285,     0,
     100,     0,     0,   975,    78,     0,     0,     0,  1167,    78,
       0,     0,   100,     0,   976,   100,     0,     0,     0,     0,
     977,   702,   978,     0,     0,     0,     0,   119,     0,     0,
     285,   119,     0,     0,     0,     0,     0,    78,    78,   730,
       0,   730,   730,     0,   730,   730,   979,     0,   122,   980,
       0,     0,     0,   100,   969,     0,   970,   971,   972,   973,
     974,     0,     0,   100,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   784,     0,   785,     0,   694,
       0,   122,     0,   122,     0,   724,     0,     0,   724,     0,
     623,     0,   273,     0,     0,     0,   122,     0,   122,     0,
       0,     0,     0,     0,   976,     0,     0,     0,     0,     0,
     977,   702,   978,     0,     0,   119,     0,     0,   119,   286,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   813,   979,     0,     0,   980,
     100,     0,   100,     0,   100,   100,     0,     0,   122,     0,
       0,     0,     0,   122,     0,     0,     0,     0,   724,     0,
       0,     0,     0,     0,     0,     0,     0,   122,   286,     0,
     539,     0,     0,     0,     0,     0,     0,   730,   100,     0,
       0,   100,     0,     0,     0,   730,     0,     0,     0,     0,
     730,   730,     0,   730,   730,     0,   730,   730,     0,     0,
       0,     0,   867,   100,     0,     0,   122,     0,   100,     0,
     122,     0,   122,     0,     0,     0,     0,   889,     0,   119,
       0,     0,     0,   118,   119,   118,     0,     0,     0,     0,
       0,   730,    85,     0,    85,     0,   100,   100,     0,     0,
     119,     0,     0,     0,     0,     0,     0,   119,     0,     0,
       0,     0,     0,     0,   539,   539,     0,     0,     0,   119,
       0,     0,   119,     0,     0,     0,     0,   917,     0,     0,
       0,     0,     0,   730,   118,   118,     0,   730,   284,     0,
       0,     0,     0,    85,    85,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1039,     0,     0,     0,
     119,     0,     0,     0,   284,   122,     0,     0,     0,     0,
     119,     0,    23,    24,    25,    26,     0,   382,   392,   392,
     392,     0,   286,     0,     0,     0,   379,     0,    32,    33,
      34,     0,     0,     0,     0,     0,     0,     0,    41,    42,
      43,    44,    45,   962,     0,     0,     0,     0,     0,   121,
       0,   121,     0,     0,     0,     0,     0,     0,     0,   393,
     725,     0,   273,     0,     0,     0,   286,     0,     0,     0,
       0,     0,   730,     0,     0,   730,     0,     0,   726,     0,
       0,     0,     0,     0,     0,     0,     0,   119,     0,   119,
     703,    59,    60,    61,    62,    63,    64,    65,    66,     0,
     121,   121,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1048,   279,
       0,     0,     0,     0,   122,   119,   122,     0,   119,     0,
     118,   393,     0,     0,     0,     0,     0,     0,     0,    85,
       0,     0,     0,     0,  1062,   730,   122,     0,     0,     0,
       0,     0,     0,     0,     0,   119,     0,     0,     0,     0,
       0,     0,     0,   118,     0,   118,     0,   727,     0,     0,
       0,     0,    85,     0,    85,     0,     0,     0,   118,     0,
     118,     0,     0,   119,   119,     0,     0,    85,     0,    85,
       0,   286,     0,     0,     0,     0,     0,     0,     0,     0,
     803,   284,  1112,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   725,  1121,   725,   725,     0,   725,   725,
     122,     0,     0,   286,   122,   539,     0,     0,     0,     0,
     118,   726,     0,   726,   726,   118,   726,   726,     0,    85,
       0,     0,     0,     0,    85,     0,     0,     0,     0,   118,
     284,     0,     0,     0,     0,     0,   121,     0,    85,     0,
       0,   533,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   539,   539,     0,     0,     0,     0,   539,   539,
       0,     0,     0,     0,     0,     0,     0,     0,   118,   121,
       0,   121,   118,     0,   118,     0,     0,    85,     0,     0,
       0,    85,     0,    85,   121,     0,   121,     0,   122,     0,
       0,   122,     0,     0,     0,     0,     0,     0,     0,     0,
     727,     0,   727,   727,     0,   727,   727,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   533,   533,     0,     0,     0,
       0,   725,     0,     0,     0,     0,   121,     0,     0,   725,
       0,   121,     0,     0,   725,   725,     0,   725,   725,   726,
     725,   725,     0,     0,     0,   121,   728,   726,   121,     0,
       0,     0,   726,   726,     0,   726,   726,   118,   726,   726,
       0,     0,     0,     0,     0,     0,    85,     0,     0,     0,
     539,     0,     0,     0,   284,   725,     0,     0,     0,     0,
       0,     0,   122,     0,   121,     0,     0,   122,   121,     0,
     121,     0,     0,   726,     0,     0,   333,  -749,  -749,  -749,
    -749,   338,   339,   122,     0,  -749,  -749,     0,   729,     0,
     122,     0,     0,   346,   347,     0,     0,   725,   284,     0,
       0,   725,   122,     0,     0,   122,     0,     0,   727,     0,
       0,     0,   121,   121,     0,   726,   727,     0,     0,   726,
       0,   727,   727,     0,   727,   727,     0,   727,   727,   349,
     350,   351,   352,   353,   354,   355,   356,   357,   358,  1040,
       0,     0,     0,   122,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   122,     0,     0,   118,     0,   118,     0,
       0,     0,   727,   121,     0,    85,     0,    85,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   118,   728,
       0,   728,   728,     0,   728,   728,     0,    85,     0,     0,
       0,     0,     0,     0,     0,     0,   725,     0,     0,   725,
       0,     0,   394,     0,   727,     0,     0,     0,   727,     0,
       0,     0,     0,     0,   726,     0,     0,   726,     0,     0,
       0,     0,     0,   284,     0,     0,     0,     0,     0,     0,
     122,     0,   122,     0,     0,     0,     0,     0,     0,     0,
       0,   729,     0,   729,   729,     0,   729,   729,     0,     0,
       0,     0,   118,     0,     0,   284,   118,     0,     0,     0,
       0,    85,     0,     0,     0,    85,   533,     0,   122,   725,
       0,   122,     0,     0,   394,     0,     0,     0,     0,     0,
       0,     0,   121,     0,   121,     0,     0,   726,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   122,     0,
       0,     0,     0,   727,   121,     0,   727,     0,     0,     0,
       0,     0,     0,   533,   533,     0,     0,   728,     0,   533,
     533,     0,     0,     0,     0,   728,   122,   122,     0,     0,
     728,   728,     0,   728,   728,     0,   728,   728,     0,     0,
     118,     0,     0,   118,     0,     0,     0,     0,     0,    85,
       0,     0,    85,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   728,     0,     0,     0,     0,   727,     0,   121,   729,
       0,     0,   121,   121,     0,     0,     0,   729,     0,     0,
       0,     0,   729,   729,     0,   729,   729,     0,   729,   729,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   728,     0,     0,     0,   728,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     121,   121,     0,   729,     0,     0,   121,   121,     0,     0,
       0,   533,     0,     0,   118,     0,     0,     0,     0,   118,
       0,     0,     0,    85,     0,     0,     0,     0,    85,     0,
       0,     0,     0,     0,     0,   118,   121,     0,     0,   121,
       0,     0,   118,     0,    85,   729,     0,     0,     0,   729,
       0,    85,     0,     0,   118,     0,     0,   118,     0,     0,
       0,     0,     0,    85,     0,     0,    85,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   728,     0,     0,   728,     0,     0,     0,     0,
       0,  1038,     0,     0,     0,   118,     0,     0,     0,     0,
    1036,     0,     0,     0,    85,   118,     0,     0,     0,     0,
       0,     0,     0,     0,    85,     0,     0,     0,   217,   217,
       0,     0,     0,     0,     0,     0,     0,     0,   121,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     121,     0,     0,     0,   729,   121,     0,   729,     0,     0,
     250,   253,   254,   255,   392,   728,     0,   217,   217,     0,
       0,   121,     0,     0,     0,     0,     0,     0,   121,     0,
     303,   304,     0,     0,     0,     0,     0,     0,     0,     0,
     121,     0,   118,   121,   118,     0,     0,     0,     0,     0,
       0,    85,     0,    85,     0,     0,     0,     0,     0,     0,
       0,     0,   217,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   729,     0,     0,
     118,   121,     0,   118,     0,     0,   392,     0,     0,    85,
    -748,   121,    85,     0,     0,     0,     0,     0,  -748,  -748,
    -748,     0,     0,  -748,  -748,  -748,     0,  -748,     0,     0,
     118,     0,     0,     0,     0,  -748,  -748,  -748,     0,    85,
       0,     0,     0,     0,     0,     0,     0,  -748,  -748,     0,
    -748,  -748,  -748,  -748,  -748,     0,     0,     0,   118,   118,
       0,     0,     0,     0,     0,     0,     0,    85,    85,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -748,     0,
       0,     0,     0,   332,     0,     0,     0,     0,   121,   217,
     121,     0,   217,   217,   217,     0,   303,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -748,  -748,     0,     0,
       0,     0,   217,     0,   217,   217,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   121,     0,     0,   121,
       0,  -748,     0,     0,     0,   333,   334,   335,   336,   337,
     338,   339,   340,   341,   342,   343,   344,   345,     0,     0,
       0,     0,   346,   347,  -748,  -748,   121,     0,     0,   233,
    -748,     0,  -748,     0,  -748,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   121,   121,   348,     0,   349,   350,
     351,   352,   353,   354,   355,   356,   357,   358,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   217,     0,
       0,     0,     0,   534,     0,   541,   542,   543,   544,   545,
       0,     0,   546,   547,   548,   549,   550,   551,   552,   553,
     554,     0,     0,   555,   556,   557,   558,   559,   560,   561,
     562,   563,   564,     0,     0,   572,   573,   217,     0,   574,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   170,   171,
     172,   173,   174,   175,   176,   177,   178,     0,     0,   179,
     180,     0,     0,     0,     0,   181,   182,   183,   184,     0,
       0,     0,     0,     0,     0,     0,     0,   615,   615,     0,
       0,   185,   186,     0,     0,     0,   615,   217,   217,     0,
       0,     0,   217,     0,   615,   615,   217,     0,     0,     0,
       0,     0,   255,   187,   188,   189,   190,   191,   192,   193,
     194,   195,   196,     0,   197,   198,     0,   641,     0,     0,
       0,   199,   615,    23,    24,    25,    26,     0,     0,     0,
       0,     0,     0,   217,     0,     0,   217,     0,     0,    32,
      33,    34,   691,     0,     0,     0,   692,   217,   693,    41,
      42,    43,    44,    45,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   671,     0,   694,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     695,   696,   217,     0,     0,     0,     0,     0,     0,   697,
       0,     0,   698,     0,     0,   699,   700,     0,   701,   702,
       0,   703,    59,   704,    61,    62,    63,    64,    65,    66,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   705,     0,     0,     0,     0,     0,     0,
     279,     0,     0,  -726,     0,     0,     0,     0,     0,     0,
       0,  -726,  -726,  -726,   237,     0,  -726,  -726,  -726,     0,
    -726,     0,     0,     0,     0,     0,     0,   217,  -726,  -726,
    -726,  -726,  -726,     0,     0,     0,     0,   217,     0,     0,
    -726,  -726,     0,  -726,  -726,  -726,  -726,  -726,     0,     0,
       0,     0,   217,     0,   217,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   217,     0,   217,
       0,  -726,     0,     0,     0,     0,     0,     0,     0,     0,
    -726,  -726,  -726,  -726,  -726,  -726,  -726,  -726,  -726,  -726,
    -726,  -726,  -726,     0,     0,     0,     0,  -726,  -726,  -726,
    -726,  -726,     0,   787,  -726,     0,     0,     0,     0,     0,
    -726,     0,   217,     0,     0,     0,     0,     0,   615,   817,
       0,   217,     0,     0,  -726,     0,     0,  -726,     0,     0,
    -114,  -726,  -726,  -726,  -726,  -726,  -726,  -726,  -726,  -726,
    -726,  -726,  -726,     0,     0,     0,     0,  -726,  -726,  -726,
    -726,     0,     0,  -726,  -726,  -726,     0,  -726,     0,     0,
       0,     0,     0,     0,     0,   615,   615,     0,     0,     0,
       0,   615,   615,    23,    24,    25,    26,     0,     0,   217,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    32,
      33,    34,   691,     0,   217,     0,   692,     0,     0,    41,
      42,    43,    44,    45,     0,     0,     0,   333,   334,   335,
     336,   337,   338,   339,   340,     0,   342,   343,     0,   615,
     615,     0,   615,   615,   346,   347,     0,     0,     0,     0,
     695,   696,     0,     0,     0,     0,     0,     0,     0,   697,
       0,     0,   698,     0,   217,   699,   700,     0,   701,     0,
       0,   703,    59,    60,    61,    62,    63,    64,    65,    66,
     349,   350,   351,   352,   353,   354,   355,   356,   357,   358,
       0,     0,     0,   705,     0,     0,     0,     0,     0,     0,
     279,     0,     0,     0,     0,     0,   947,     0,     0,     0,
       0,     0,     0,   615,   237,  -748,     4,     0,     5,     6,
       7,     8,     9,     0,     0,     0,    10,    11,     0,     0,
       0,    12,     0,    13,    14,    15,    16,    17,    18,    19,
     217,     0,     0,     0,     0,    20,    21,    22,    23,    24,
      25,    26,     0,     0,    27,     0,     0,     0,   615,   217,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,     0,    41,    42,    43,    44,    45,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      48,    49,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    50,    51,     0,     0,     0,
       0,     0,     0,     0,    52,   217,     0,    53,    54,     0,
      55,    56,     0,    57,     0,     0,    58,    59,    60,    61,
      62,    63,    64,    65,    66,     0,  -620,     0,     0,     0,
       0,   217,     0,     0,  -620,  -620,  -620,     0,     0,  -620,
    -620,  -620,     0,  -620,     0,    67,    68,    69,     0,     0,
       0,  -620,     0,  -620,  -620,  -620,     0,  -748,     0,  -748,
       0,     0,     0,  -620,  -620,     0,  -620,  -620,  -620,  -620,
    -620,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   333,   334,   335,   336,   337,   338,   339,     0,   217,
     342,   343,     0,     0,  -620,     0,     0,     0,   346,   347,
       0,   217,     0,  -620,  -620,  -620,  -620,  -620,  -620,  -620,
    -620,  -620,  -620,  -620,  -620,  -620,     0,     0,     0,     0,
    -620,  -620,  -620,  -620,  -620,     0,  -620,  -620,     0,     0,
       0,     0,     0,  -620,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,     0,     0,   217,  -620,     0,     0,
    -620,     0,     0,  -620,  -620,  -620,  -620,  -620,  -620,  -620,
    -620,  -620,  -620,  -620,  -620,  -620,     0,     0,     0,     0,
       0,  -620,  -620,  -620,  -623,     0,  -620,  -620,  -620,     0,
    -620,     0,  -623,  -623,  -623,     0,     0,  -623,  -623,  -623,
       0,  -623,     0,     0,     0,     0,   687,     0,     0,  -623,
       0,  -623,  -623,  -623,     0,     0,     0,     0,     0,     0,
       0,  -623,  -623,     0,  -623,  -623,  -623,  -623,  -623,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   333,
     334,   335,   336,   337,   338,   339,   340,   341,   342,   343,
     344,   345,  -623,     0,     0,     0,   346,   347,     0,     0,
       0,  -623,  -623,  -623,  -623,  -623,  -623,  -623,  -623,  -623,
    -623,  -623,  -623,  -623,     0,     0,     0,     0,  -623,  -623,
    -623,  -623,  -623,     0,  -623,  -623,     0,     0,     0,     0,
     348,  -623,   349,   350,   351,   352,   353,   354,   355,   356,
     357,   358,     0,     0,     0,  -623,     0,     0,  -623,  -255,
       0,  -623,  -623,  -623,  -623,  -623,  -623,  -623,  -623,  -623,
    -623,  -623,  -623,  -623,     0,     0,     0,     0,     0,  -623,
    -623,  -623,  -727,     0,  -623,  -623,  -623,     0,  -623,     0,
    -727,  -727,  -727,     0,     0,  -727,  -727,  -727,     0,  -727,
       0,     0,     0,     0,   687,     0,     0,  -727,  -727,  -727,
    -727,  -727,     0,     0,     0,     0,     0,     0,     0,  -727,
    -727,     0,  -727,  -727,  -727,  -727,  -727,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   333,   334,   335,
     336,   337,   338,   339,   340,   341,   342,   343,   344,   345,
    -727,     0,     0,     0,   346,   347,     0,     0,     0,  -727,
    -727,  -727,  -727,  -727,  -727,  -727,  -727,  -727,  -727,  -727,
    -727,  -727,     0,     0,     0,     0,  -727,  -727,  -727,  -727,
    -727,     0,     0,  -727,     0,     0,     0,     0,   348,  -727,
     349,   350,   351,   352,   353,   354,   355,   356,   357,   358,
       0,     0,     0,  -727,     0,     0,  -727,     0,     0,     0,
    -727,  -727,  -727,  -727,  -727,  -727,  -727,  -727,  -727,  -727,
    -727,  -727,     0,     0,     0,     0,  -727,  -727,  -727,  -727,
    -728,     0,  -727,  -727,  -727,     0,  -727,     0,  -728,  -728,
    -728,     0,     0,  -728,  -728,  -728,     0,  -728,     0,     0,
       0,     0,     0,     0,     0,  -728,  -728,  -728,  -728,  -728,
       0,     0,     0,     0,     0,     0,     0,  -728,  -728,     0,
    -728,  -728,  -728,  -728,  -728,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   333,   334,   335,   336,   337,
     338,   339,   340,   341,   342,   343,   344,   345,  -728,     0,
       0,     0,   346,   347,     0,     0,     0,  -728,  -728,  -728,
    -728,  -728,  -728,  -728,  -728,  -728,  -728,  -728,  -728,  -728,
       0,     0,     0,     0,  -728,  -728,  -728,  -728,  -728,     0,
       0,  -728,     0,     0,     0,     0,   348,  -728,   349,   350,
     351,   352,   353,   354,   355,   356,   357,   358,     0,     0,
       0,  -728,     0,     0,  -728,     0,     0,     0,  -728,  -728,
    -728,  -728,  -728,  -728,  -728,  -728,  -728,  -728,  -728,  -728,
       0,     0,     0,     0,  -728,  -728,  -728,  -728,  -308,     0,
    -728,  -728,  -728,     0,  -728,     0,  -308,  -308,  -308,     0,
       0,  -308,  -308,  -308,     0,  -308,     0,     0,     0,     0,
       0,     0,     0,  -308,     0,  -308,  -308,  -308,     0,     0,
       0,     0,     0,     0,     0,  -308,  -308,     0,  -308,  -308,
    -308,  -308,  -308,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   333,   334,   335,   336,   337,   338,   339,
     340,   341,   342,   343,  -749,  -749,  -308,     0,     0,     0,
     346,   347,     0,     0,     0,  -308,  -308,  -308,  -308,  -308,
    -308,  -308,  -308,  -308,  -308,  -308,  -308,  -308,     0,     0,
       0,     0,  -308,  -308,  -308,  -308,  -308,     0,   788,  -308,
       0,     0,     0,     0,     0,  -308,   349,   350,   351,   352,
     353,   354,   355,   356,   357,   358,     0,     0,     0,  -308,
       0,     0,  -308,     0,     0,  -116,  -308,  -308,  -308,  -308,
    -308,  -308,  -308,  -308,  -308,  -308,  -308,  -308,     0,     0,
       0,     0,     0,  -308,  -308,  -308,  -449,     0,  -308,  -308,
    -308,     0,  -308,     0,  -449,  -449,  -449,     0,     0,  -449,
    -449,  -449,     0,  -449,     0,     0,     0,     0,     0,     0,
       0,  -449,  -449,  -449,  -449,     0,     0,     0,     0,     0,
       0,     0,     0,  -449,  -449,     0,  -449,  -449,  -449,  -449,
    -449,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -449,     0,     0,     0,     0,     0,
       0,     0,     0,  -449,  -449,  -449,  -449,  -449,  -449,  -449,
    -449,  -449,  -449,  -449,  -449,  -449,     0,     0,     0,     0,
    -449,  -449,  -449,  -449,  -449,     0,     0,  -449,     0,     0,
       0,     0,     0,  -449,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -449,     0,     0,
       0,     0,     0,     0,  -449,     0,  -449,  -449,  -449,  -449,
    -449,  -449,  -449,  -449,  -449,  -449,     0,     0,     0,     0,
    -449,  -449,  -449,  -449,  -300,   233,  -449,  -449,  -449,     0,
    -449,     0,  -300,  -300,  -300,     0,     0,  -300,  -300,  -300,
       0,  -300,     0,     0,     0,     0,     0,     0,     0,  -300,
       0,  -300,  -300,  -300,     0,     0,     0,     0,     0,     0,
       0,  -300,  -300,     0,  -300,  -300,  -300,  -300,  -300,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -300,     0,     0,     0,     0,     0,     0,     0,
       0,  -300,  -300,  -300,  -300,  -300,  -300,  -300,  -300,  -300,
    -300,  -300,  -300,  -300,     0,     0,     0,     0,  -300,  -300,
    -300,  -300,  -300,     0,     0,  -300,     0,     0,     0,     0,
       0,  -300,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -300,     0,     0,  -300,     0,
       0,     0,  -300,  -300,  -300,  -300,  -300,  -300,  -300,  -300,
    -300,  -300,  -300,  -300,     0,     0,     0,     0,     0,  -300,
    -300,  -300,  -748,     0,  -300,  -300,  -300,     0,  -300,     0,
    -748,  -748,  -748,     0,     0,  -748,  -748,  -748,     0,  -748,
       0,     0,     0,     0,     0,     0,     0,  -748,  -748,  -748,
    -748,     0,     0,     0,     0,     0,     0,     0,     0,  -748,
    -748,     0,  -748,  -748,  -748,  -748,  -748,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -748,     0,     0,     0,     0,     0,     0,     0,     0,  -748,
    -748,  -748,  -748,  -748,  -748,  -748,  -748,  -748,  -748,  -748,
    -748,  -748,     0,     0,     0,     0,  -748,  -748,  -748,  -748,
    -748,     0,     0,  -748,     0,     0,     0,     0,     0,  -748,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -748,     0,     0,     0,     0,     0,     0,
    -748,     0,  -748,  -748,  -748,  -748,  -748,  -748,  -748,  -748,
    -748,  -748,     0,     0,     0,     0,  -748,  -748,  -748,  -748,
    -315,   233,  -748,  -748,  -748,     0,  -748,     0,  -315,  -315,
    -315,     0,     0,  -315,  -315,  -315,     0,  -315,     0,     0,
       0,     0,     0,     0,     0,  -315,     0,  -315,  -315,     0,
       0,     0,     0,     0,     0,     0,     0,  -315,  -315,     0,
    -315,  -315,  -315,  -315,  -315,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -315,     0,
       0,     0,     0,     0,     0,     0,     0,  -315,  -315,  -315,
    -315,  -315,  -315,  -315,  -315,  -315,  -315,  -315,  -315,  -315,
       0,     0,     0,     0,  -315,  -315,  -315,  -315,  -315,     0,
       0,  -315,     0,     0,     0,     0,     0,  -315,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -315,     0,     0,     0,     0,     0,     0,  -315,     0,
    -315,  -315,  -315,  -315,  -315,  -315,  -315,  -315,  -315,  -315,
       0,     0,     0,     0,     0,  -315,  -315,  -315,  -726,   230,
    -315,  -315,  -315,     0,  -315,     0,  -726,  -726,  -726,     0,
       0,     0,  -726,  -726,     0,  -726,     0,     0,     0,     0,
       0,     0,     0,  -726,  -726,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -726,  -726,     0,  -726,  -726,
    -726,  -726,  -726,     0,     0,     0,   333,   334,   335,   336,
     337,   338,   339,   340,   341,   342,   343,   344,   345,     0,
       0,     0,     0,   346,   347,     0,  -726,     0,     0,     0,
       0,     0,     0,     0,     0,  -726,  -726,  -726,  -726,  -726,
    -726,  -726,  -726,  -726,  -726,  -726,  -726,  -726,     0,     0,
       0,     0,  -726,  -726,  -726,  -726,  -726,   348,   733,   349,
     350,   351,   352,   353,   354,   355,   356,   357,   358,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -726,
       0,     0,     0,   237,     0,  -114,  -726,     0,  -726,  -726,
    -726,  -726,  -726,  -726,  -726,  -726,  -726,  -726,     0,     0,
       0,     0,  -726,  -726,  -726,  -105,  -726,     0,  -726,     0,
    -726,     0,  -726,     0,  -726,  -726,  -726,     0,     0,     0,
    -726,  -726,     0,  -726,     0,     0,     0,     0,     0,     0,
       0,  -726,  -726,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -726,  -726,     0,  -726,  -726,  -726,  -726,
    -726,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -726,     0,     0,     0,     0,     0,
       0,     0,     0,  -726,  -726,  -726,  -726,  -726,  -726,  -726,
    -726,  -726,  -726,  -726,  -726,  -726,     0,     0,     0,     0,
    -726,  -726,  -726,  -726,  -726,     0,   733,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -726,     0,     0,
       0,     0,     0,  -114,  -726,     0,  -726,  -726,  -726,  -726,
    -726,  -726,  -726,  -726,  -726,  -726,     0,     0,     0,     0,
    -726,  -726,  -726,  -726,  -308,     0,  -726,     0,  -726,     0,
    -726,     0,  -308,  -308,  -308,     0,     0,     0,  -308,  -308,
       0,  -308,     0,     0,     0,     0,     0,     0,     0,  -308,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -308,  -308,     0,  -308,  -308,  -308,  -308,  -308,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -308,     0,     0,     0,     0,     0,     0,     0,
       0,  -308,  -308,  -308,  -308,  -308,  -308,  -308,  -308,  -308,
    -308,  -308,  -308,  -308,     0,     0,     0,     0,  -308,  -308,
    -308,  -308,  -308,     0,   734,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -308,     0,     0,     0,     0,
       0,  -116,  -308,     0,  -308,  -308,  -308,  -308,  -308,  -308,
    -308,  -308,  -308,  -308,     0,     0,     0,     0,     0,  -308,
    -308,  -107,  -308,     0,  -308,     0,  -308,     0,  -308,     0,
    -308,  -308,  -308,     0,     0,     0,  -308,  -308,     0,  -308,
       0,     0,     0,     0,     0,     0,     0,  -308,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -308,
    -308,     0,  -308,  -308,  -308,  -308,  -308,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -308,     0,     0,     0,     0,     0,     0,     0,     0,  -308,
    -308,  -308,  -308,  -308,  -308,  -308,  -308,  -308,  -308,  -308,
    -308,  -308,     0,     0,     0,     0,  -308,  -308,  -308,  -308,
    -308,     0,   734,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -308,     0,     0,     0,     0,     0,  -116,
    -308,     0,  -308,  -308,  -308,  -308,  -308,  -308,  -308,  -308,
    -308,  -308,     0,     0,     0,     0,     0,  -308,  -308,  -308,
       0,     0,  -308,     0,  -308,   257,  -308,     5,     6,     7,
       8,     9,  -748,  -748,  -748,    10,    11,     0,     0,  -748,
      12,     0,    13,    14,    15,    16,    17,    18,    19,     0,
       0,     0,     0,     0,    20,    21,    22,    23,    24,    25,
      26,     0,     0,    27,     0,     0,     0,     0,     0,    28,
      29,   258,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,     0,    41,    42,    43,    44,    45,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    48,
      49,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    50,    51,     0,     0,     0,     0,
       0,     0,     0,    52,     0,     0,    53,    54,     0,    55,
      56,     0,    57,     0,     0,    58,    59,    60,    61,    62,
      63,    64,    65,    66,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    67,    68,    69,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -748,   257,  -748,     5,
       6,     7,     8,     9,     0,     0,  -748,    10,    11,     0,
    -748,  -748,    12,     0,    13,    14,    15,    16,    17,    18,
      19,     0,     0,     0,     0,     0,    20,    21,    22,    23,
      24,    25,    26,     0,     0,    27,     0,     0,     0,     0,
       0,    28,    29,   258,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,     0,    41,    42,    43,    44,    45,
      46,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    48,    49,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    50,    51,     0,     0,
       0,     0,     0,     0,     0,    52,     0,     0,    53,    54,
       0,    55,    56,     0,    57,     0,     0,    58,    59,    60,
      61,    62,    63,    64,    65,    66,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    67,    68,    69,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -748,   257,
    -748,     5,     6,     7,     8,     9,     0,     0,  -748,    10,
      11,     0,     0,  -748,    12,  -748,    13,    14,    15,    16,
      17,    18,    19,     0,     0,     0,     0,     0,    20,    21,
      22,    23,    24,    25,    26,     0,     0,    27,     0,     0,
       0,     0,     0,    28,    29,   258,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,     0,    41,    42,    43,
      44,    45,    46,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    48,    49,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    50,    51,
       0,     0,     0,     0,     0,     0,     0,    52,     0,     0,
      53,    54,     0,    55,    56,     0,    57,     0,     0,    58,
      59,    60,    61,    62,    63,    64,    65,    66,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    67,    68,
      69,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -748,   257,  -748,     5,     6,     7,     8,     9,     0,     0,
    -748,    10,    11,     0,     0,  -748,    12,     0,    13,    14,
      15,    16,    17,    18,    19,  -748,     0,     0,     0,     0,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,    28,    29,   258,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,     0,    41,
      42,    43,    44,    45,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    48,    49,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      50,    51,     0,     0,     0,     0,     0,     0,     0,    52,
       0,     0,    53,    54,     0,    55,    56,     0,    57,     0,
       0,    58,    59,    60,    61,    62,    63,    64,    65,    66,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      67,    68,    69,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -748,   257,  -748,     5,     6,     7,     8,     9,
       0,     0,  -748,    10,    11,     0,     0,  -748,    12,     0,
      13,    14,    15,    16,    17,    18,    19,     0,     0,     0,
       0,     0,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,    28,    29,   258,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
       0,    41,    42,    43,    44,    45,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    48,    49,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    50,    51,     0,     0,     0,     0,     0,     0,
       0,    52,     0,     0,    53,    54,     0,    55,    56,     0,
      57,     0,     0,    58,    59,    60,    61,    62,    63,    64,
      65,    66,     0,     0,     0,     0,     0,     0,     0,     0,
     257,     0,     5,     6,     7,     8,     9,     0,  -748,  -748,
      10,    11,    67,    68,    69,    12,     0,    13,    14,    15,
      16,    17,    18,    19,  -748,     0,  -748,     0,     0,    20,
      21,    22,    23,    24,    25,    26,     0,     0,    27,     0,
       0,     0,     0,     0,    28,    29,   258,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,     0,    41,    42,
      43,    44,    45,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    48,    49,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    50,
      51,     0,     0,     0,     0,     0,     0,     0,    52,     0,
       0,    53,    54,     0,    55,    56,     0,    57,     0,     0,
      58,    59,    60,    61,    62,    63,    64,    65,    66,     0,
       0,     0,     0,     0,     0,     0,     0,   257,     0,     5,
       6,     7,     8,     9,     0,     0,     0,    10,    11,    67,
      68,    69,    12,     0,    13,    14,    15,    16,    17,    18,
      19,  -748,     0,  -748,     0,     0,    20,    21,    22,    23,
      24,    25,    26,     0,     0,    27,     0,     0,     0,     0,
       0,    28,    29,   258,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,     0,    41,    42,    43,    44,    45,
      46,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    48,    49,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    50,    51,     0,     0,
       0,     0,     0,     0,     0,    52,     0,     0,   259,    54,
       0,    55,    56,     0,    57,     0,     0,    58,    59,    60,
      61,    62,    63,    64,    65,    66,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    67,    68,    69,     0,
       0,     0,     0,     0,     0,     0,  -748,     0,  -748,   257,
    -748,     5,     6,     7,     8,     9,     0,     0,     0,    10,
      11,     0,     0,     0,    12,     0,    13,    14,    15,    16,
      17,    18,    19,     0,     0,     0,     0,     0,    20,    21,
      22,    23,    24,    25,    26,     0,     0,    27,     0,     0,
       0,     0,     0,    28,    29,   258,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,     0,    41,    42,    43,
      44,    45,    46,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    48,    49,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    50,    51,
       0,     0,     0,     0,     0,     0,     0,    52,     0,     0,
      53,    54,     0,    55,    56,     0,    57,     0,     0,    58,
      59,    60,    61,    62,    63,    64,    65,    66,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    67,    68,
      69,     0,     0,     0,     0,     0,     0,     0,  -748,     0,
    -748,     4,  -748,     5,     6,     7,     8,     9,     0,     0,
       0,    10,    11,     0,     0,     0,    12,     0,    13,    14,
      15,    16,    17,    18,    19,     0,     0,     0,     0,     0,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,     0,    41,
      42,    43,    44,    45,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    48,    49,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      50,    51,     0,     0,     0,     0,     0,     0,     0,    52,
       0,     0,    53,    54,     0,    55,    56,     0,    57,     0,
       0,    58,    59,    60,    61,    62,    63,    64,    65,    66,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      67,    68,    69,     0,     0,  -748,     0,     0,     0,     0,
       0,     0,  -748,   257,  -748,     5,     6,     7,     8,     9,
       0,     0,     0,    10,    11,     0,     0,     0,    12,     0,
      13,    14,    15,    16,    17,    18,    19,     0,     0,     0,
       0,     0,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,    28,    29,   258,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
       0,    41,    42,    43,    44,    45,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    48,    49,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    50,    51,     0,     0,     0,     0,     0,     0,
       0,    52,     0,     0,    53,    54,     0,    55,    56,     0,
      57,     0,     0,    58,    59,    60,    61,    62,    63,    64,
      65,    66,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    67,    68,    69,     0,     0,  -748,     0,     0,
       0,     0,     0,     0,  -748,   257,  -748,     5,     6,     7,
       8,     9,     0,     0,  -748,    10,    11,     0,     0,     0,
      12,     0,    13,    14,    15,    16,    17,    18,    19,     0,
       0,     0,     0,     0,    20,    21,    22,    23,    24,    25,
      26,     0,     0,    27,     0,     0,     0,     0,     0,    28,
      29,   258,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,     0,    41,    42,    43,    44,    45,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    48,
      49,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    50,    51,     0,     0,     0,     0,
       0,     0,     0,    52,     0,     0,    53,    54,     0,    55,
      56,     0,    57,     0,     0,    58,    59,    60,    61,    62,
      63,    64,    65,    66,     0,     0,     0,     0,     0,     0,
       0,     0,   257,     0,     5,     6,     7,     8,     9,     0,
       0,     0,    10,    11,    67,    68,    69,    12,     0,    13,
      14,    15,    16,    17,    18,    19,  -748,     0,  -748,     0,
       0,    20,    21,    22,    23,    24,    25,    26,     0,     0,
      27,     0,     0,     0,     0,     0,    28,    29,   258,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,     0,
      41,    42,    43,    44,    45,    46,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    48,    49,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    50,    51,     0,     0,     0,     0,     0,     0,     0,
      52,     0,     0,    53,    54,     0,    55,    56,     0,    57,
       0,     0,    58,    59,    60,    61,    62,    63,    64,    65,
      66,     0,  -748,     0,     0,     0,     0,     0,     0,     0,
       0,     5,     6,     7,     0,     9,     0,     0,     0,    10,
      11,    67,    68,    69,    12,     0,    13,    14,    15,    16,
      17,    18,    19,  -748,     0,  -748,     0,     0,    20,    21,
      22,    23,    24,    25,    26,     0,     0,   206,     0,     0,
       0,     0,     0,     0,    29,     0,     0,    32,    33,    34,
      35,    36,    37,    38,    39,    40,   207,    41,    42,    43,
      44,    45,    46,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    48,    49,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    50,    51,
       0,     0,     0,     0,     0,     0,     0,   208,     0,     0,
     209,    54,     0,    55,    56,     0,   210,   211,   212,    58,
      59,   213,    61,    62,    63,    64,    65,    66,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     0,     9,     0,     0,     0,    10,    11,    67,   214,
      69,    12,     0,    13,    14,    15,    16,    17,    18,    19,
       0,     0,   237,     0,     0,    20,    21,    22,    23,    24,
      25,    26,     0,     0,    27,     0,     0,     0,     0,     0,
       0,    29,     0,     0,    32,    33,    34,    35,    36,    37,
      38,    39,    40,     0,    41,    42,    43,    44,    45,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      48,    49,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    50,    51,     0,     0,     0,
       0,     0,     0,     0,   208,     0,     0,   209,    54,     0,
      55,    56,     0,     0,     0,     0,    58,    59,    60,    61,
      62,    63,    64,    65,    66,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     5,     6,     7,     0,     9,
       0,     0,     0,    10,    11,    67,    68,    69,    12,     0,
      13,    14,    15,    16,    17,    18,    19,   305,     0,   306,
       0,     0,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,     0,    29,     0,
       0,    32,    33,    34,    35,    36,    37,    38,    39,    40,
       0,    41,    42,    43,    44,    45,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    48,    49,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    50,    51,     0,     0,     0,     0,     0,     0,
       0,   208,     0,     0,   209,    54,     0,    55,    56,     0,
       0,     0,     0,    58,    59,    60,    61,    62,    63,    64,
      65,    66,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     8,     9,     0,     0,     0,
      10,    11,    67,    68,    69,    12,     0,    13,    14,    15,
      16,    17,    18,    19,     0,     0,   237,     0,     0,    20,
      21,    22,    23,    24,    25,    26,     0,     0,    27,     0,
       0,     0,     0,     0,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,     0,    41,    42,
      43,    44,    45,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    48,    49,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    50,
      51,     0,     0,     0,     0,     0,     0,     0,    52,     0,
       0,    53,    54,     0,    55,    56,     0,    57,     0,     0,
      58,    59,    60,    61,    62,    63,    64,    65,    66,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     5,
       6,     7,     8,     9,     0,     0,     0,    10,    11,    67,
      68,    69,    12,     0,    13,    14,    15,    16,    17,    18,
      19,   504,     0,     0,     0,     0,    20,    21,    22,    23,
      24,    25,    26,     0,     0,    27,     0,     0,     0,     0,
       0,    28,    29,   258,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,     0,    41,    42,    43,    44,    45,
      46,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    48,    49,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    50,    51,     0,     0,
       0,     0,     0,     0,     0,    52,     0,     0,    53,    54,
       0,    55,    56,     0,    57,     0,     0,    58,    59,    60,
      61,    62,    63,    64,    65,    66,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    67,    68,    69,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   504,   126,
     127,   128,   129,   130,   131,   132,   133,   134,   135,   136,
     137,   138,   139,   140,   141,   142,   143,   144,   145,   146,
     147,   148,   149,     0,     0,     0,   150,   151,   152,   396,
     397,   398,   399,   157,   158,   159,     0,     0,     0,     0,
       0,   160,   161,   162,   163,   400,   401,   402,   403,   168,
      37,    38,   404,    40,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   170,   171,   172,   173,   174,   175,   176,   177,   178,
       0,     0,   179,   180,     0,     0,     0,     0,   181,   182,
     183,   184,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   185,   186,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   187,   188,   189,   190,
     191,   192,   193,   194,   195,   196,     0,   197,   198,     0,
       0,     0,     0,     0,   199,   405,   126,   127,   128,   129,
     130,   131,   132,   133,   134,   135,   136,   137,   138,   139,
     140,   141,   142,   143,   144,   145,   146,   147,   148,   149,
       0,     0,     0,   150,   151,   152,   153,   154,   155,   156,
     157,   158,   159,     0,     0,     0,     0,     0,   160,   161,
     162,   163,   164,   165,   166,   167,   168,   288,   289,   169,
     290,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   170,   171,
     172,   173,   174,   175,   176,   177,   178,     0,     0,   179,
     180,     0,     0,     0,     0,   181,   182,   183,   184,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   185,   186,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   187,   188,   189,   190,   191,   192,   193,
     194,   195,   196,     0,   197,   198,     0,     0,     0,     0,
       0,   199,   126,   127,   128,   129,   130,   131,   132,   133,
     134,   135,   136,   137,   138,   139,   140,   141,   142,   143,
     144,   145,   146,   147,   148,   149,     0,     0,     0,   150,
     151,   152,   153,   154,   155,   156,   157,   158,   159,     0,
       0,     0,     0,     0,   160,   161,   162,   163,   164,   165,
     166,   167,   168,   239,     0,   169,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   170,   171,   172,   173,   174,   175,
     176,   177,   178,     0,     0,   179,   180,     0,     0,     0,
       0,   181,   182,   183,   184,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   185,   186,     0,
       0,    59,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   187,
     188,   189,   190,   191,   192,   193,   194,   195,   196,     0,
     197,   198,     0,     0,     0,     0,     0,   199,   126,   127,
     128,   129,   130,   131,   132,   133,   134,   135,   136,   137,
     138,   139,   140,   141,   142,   143,   144,   145,   146,   147,
     148,   149,     0,     0,     0,   150,   151,   152,   153,   154,
     155,   156,   157,   158,   159,     0,     0,     0,     0,     0,
     160,   161,   162,   163,   164,   165,   166,   167,   168,     0,
       0,   169,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     170,   171,   172,   173,   174,   175,   176,   177,   178,     0,
       0,   179,   180,     0,     0,     0,     0,   181,   182,   183,
     184,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   185,   186,     0,     0,    59,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   187,   188,   189,   190,   191,
     192,   193,   194,   195,   196,     0,   197,   198,     0,     0,
       0,     0,     0,   199,   126,   127,   128,   129,   130,   131,
     132,   133,   134,   135,   136,   137,   138,   139,   140,   141,
     142,   143,   144,   145,   146,   147,   148,   149,     0,     0,
       0,   150,   151,   152,   153,   154,   155,   156,   157,   158,
     159,     0,     0,     0,     0,     0,   160,   161,   162,   163,
     164,   165,   166,   167,   168,     0,     0,   169,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   170,   171,   172,   173,
     174,   175,   176,   177,   178,     0,     0,   179,   180,     0,
       0,     0,     0,   181,   182,   183,   184,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   185,
     186,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   187,   188,   189,   190,   191,   192,   193,   194,   195,
     196,     0,   197,   198,     5,     6,     7,     0,     9,   199,
       0,     0,    10,    11,     0,     0,     0,    12,     0,    13,
      14,    15,   245,   246,    18,    19,     0,     0,     0,     0,
       0,    20,   247,   248,    23,    24,    25,    26,     0,     0,
     206,     0,     0,     0,     0,     0,     0,   277,     0,     0,
      32,    33,    34,    35,    36,    37,    38,    39,    40,     0,
      41,    42,    43,    44,    45,    46,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     278,     0,     0,   209,    54,     0,    55,    56,     0,     0,
       0,     0,    58,    59,    60,    61,    62,    63,    64,    65,
      66,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     0,     9,     0,
       0,   279,    10,    11,     0,     0,     0,    12,   280,    13,
      14,    15,   245,   246,    18,    19,     0,     0,     0,     0,
       0,    20,   247,   248,    23,    24,    25,    26,     0,     0,
     206,     0,     0,     0,     0,     0,     0,   277,     0,     0,
      32,    33,    34,    35,    36,    37,    38,    39,    40,     0,
      41,    42,    43,    44,    45,    46,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     278,     0,     0,   209,    54,     0,    55,    56,     0,     0,
       0,     0,    58,    59,    60,    61,    62,    63,    64,    65,
      66,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     8,     9,     0,
       0,   279,    10,    11,     0,     0,     0,    12,   527,    13,
      14,    15,    16,    17,    18,    19,     0,     0,     0,     0,
       0,    20,    21,    22,    23,    24,    25,    26,     0,     0,
      27,     0,     0,     0,     0,     0,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,     0,
      41,    42,    43,    44,    45,    46,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    48,    49,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    50,    51,     0,     0,     0,     0,     0,     0,     0,
      52,     0,     0,    53,    54,     0,    55,    56,     0,    57,
       0,     0,    58,    59,    60,    61,    62,    63,    64,    65,
      66,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     5,     6,     7,     0,     9,     0,     0,     0,    10,
      11,    67,    68,    69,    12,     0,    13,    14,    15,    16,
      17,    18,    19,     0,     0,     0,     0,     0,    20,    21,
      22,    23,    24,    25,    26,     0,     0,   206,     0,     0,
       0,     0,     0,     0,    29,     0,     0,    32,    33,    34,
      35,    36,    37,    38,    39,    40,   207,    41,    42,    43,
      44,    45,    46,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    48,    49,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    50,    51,
       0,     0,     0,     0,     0,     0,     0,   208,     0,     0,
     209,    54,     0,    55,    56,     0,   210,   211,   212,    58,
      59,   213,    61,    62,    63,    64,    65,    66,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     8,     9,     0,     0,     0,    10,    11,    67,   214,
      69,    12,     0,    13,    14,    15,    16,    17,    18,    19,
       0,     0,     0,     0,     0,    20,    21,    22,    23,    24,
      25,    26,     0,     0,    27,     0,     0,     0,     0,     0,
      28,    29,     0,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,     0,    41,    42,    43,    44,    45,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      48,    49,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    50,    51,     0,     0,     0,
       0,     0,     0,     0,    52,     0,     0,    53,    54,     0,
      55,    56,     0,    57,     0,     0,    58,    59,    60,    61,
      62,    63,    64,    65,    66,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     5,     6,     7,     0,     9,
       0,     0,     0,    10,    11,    67,    68,    69,    12,     0,
      13,    14,    15,    16,    17,    18,    19,     0,     0,     0,
       0,     0,    20,    21,    22,    23,    24,    25,    26,     0,
       0,   206,     0,     0,     0,     0,     0,     0,    29,     0,
       0,    32,    33,    34,    35,    36,    37,    38,    39,    40,
     207,    41,    42,    43,    44,    45,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    48,    49,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    50,   440,     0,     0,     0,     0,     0,     0,
       0,   208,     0,     0,   209,    54,     0,    55,    56,     0,
     210,   211,   212,    58,    59,   213,    61,    62,    63,    64,
      65,    66,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     0,     9,     0,     0,     0,
      10,    11,    67,   214,    69,    12,     0,    13,    14,    15,
     245,   246,    18,    19,     0,     0,     0,     0,     0,    20,
     247,   248,    23,    24,    25,    26,     0,     0,   206,     0,
       0,     0,     0,     0,     0,    29,     0,     0,    32,    33,
      34,    35,    36,    37,    38,    39,    40,   207,    41,    42,
      43,    44,    45,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    48,    49,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    50,
      51,     0,     0,     0,     0,     0,     0,     0,   208,     0,
       0,   209,    54,     0,    55,    56,     0,   622,   211,   212,
      58,    59,   213,    61,    62,    63,    64,    65,    66,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     5,
       6,     7,     0,     9,     0,     0,     0,    10,    11,    67,
     214,    69,    12,     0,    13,    14,    15,   245,   246,    18,
      19,     0,     0,     0,     0,     0,    20,   247,   248,    23,
      24,    25,    26,     0,     0,   206,     0,     0,     0,     0,
       0,     0,    29,     0,     0,    32,    33,    34,    35,    36,
      37,    38,    39,    40,   207,    41,    42,    43,    44,    45,
      46,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    48,    49,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    50,    51,     0,     0,
       0,     0,     0,     0,     0,   208,     0,     0,   209,    54,
       0,    55,    56,     0,   210,   211,     0,    58,    59,   213,
      61,    62,    63,    64,    65,    66,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     0,
       9,     0,     0,     0,    10,    11,    67,   214,    69,    12,
       0,    13,    14,    15,   245,   246,    18,    19,     0,     0,
       0,     0,     0,    20,   247,   248,    23,    24,    25,    26,
       0,     0,   206,     0,     0,     0,     0,     0,     0,    29,
       0,     0,    32,    33,    34,    35,    36,    37,    38,    39,
      40,   207,    41,    42,    43,    44,    45,    46,    47,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    48,    49,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    50,    51,     0,     0,     0,     0,     0,
       0,     0,   208,     0,     0,   209,    54,     0,    55,    56,
       0,     0,   211,   212,    58,    59,   213,    61,    62,    63,
      64,    65,    66,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     5,     6,     7,     0,     9,     0,     0,
       0,    10,    11,    67,   214,    69,    12,     0,    13,    14,
      15,   245,   246,    18,    19,     0,     0,     0,     0,     0,
      20,   247,   248,    23,    24,    25,    26,     0,     0,   206,
       0,     0,     0,     0,     0,     0,    29,     0,     0,    32,
      33,    34,    35,    36,    37,    38,    39,    40,   207,    41,
      42,    43,    44,    45,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    48,    49,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      50,    51,     0,     0,     0,     0,     0,     0,     0,   208,
       0,     0,   209,    54,     0,    55,    56,     0,   622,   211,
       0,    58,    59,   213,    61,    62,    63,    64,    65,    66,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       5,     6,     7,     0,     9,     0,     0,     0,    10,    11,
      67,   214,    69,    12,     0,    13,    14,    15,   245,   246,
      18,    19,     0,     0,     0,     0,     0,    20,   247,   248,
      23,    24,    25,    26,     0,     0,   206,     0,     0,     0,
       0,     0,     0,    29,     0,     0,    32,    33,    34,    35,
      36,    37,    38,    39,    40,   207,    41,    42,    43,    44,
      45,    46,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    48,    49,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    50,    51,     0,
       0,     0,     0,     0,     0,     0,   208,     0,     0,   209,
      54,     0,    55,    56,     0,     0,   211,     0,    58,    59,
     213,    61,    62,    63,    64,    65,    66,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     5,     6,     7,
       0,     9,     0,     0,     0,    10,    11,    67,   214,    69,
      12,     0,    13,    14,    15,    16,    17,    18,    19,     0,
       0,     0,     0,     0,    20,    21,    22,    23,    24,    25,
      26,     0,     0,   206,     0,     0,     0,     0,     0,     0,
      29,     0,     0,    32,    33,    34,    35,    36,    37,    38,
      39,    40,     0,    41,    42,    43,    44,    45,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    48,
      49,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    50,    51,     0,     0,     0,     0,
       0,     0,     0,   208,     0,     0,   209,    54,     0,    55,
      56,     0,   520,     0,     0,    58,    59,    60,    61,    62,
      63,    64,    65,    66,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     0,     9,     0,
       0,     0,    10,    11,    67,   214,    69,    12,     0,    13,
      14,    15,   245,   246,    18,    19,     0,     0,     0,     0,
       0,    20,   247,   248,    23,    24,    25,    26,     0,     0,
     206,     0,     0,     0,     0,     0,     0,    29,     0,     0,
      32,    33,    34,    35,    36,    37,    38,    39,    40,     0,
      41,    42,    43,    44,    45,    46,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    48,    49,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    50,    51,     0,     0,     0,     0,     0,     0,     0,
     208,     0,     0,   209,    54,     0,    55,    56,     0,   812,
       0,     0,    58,    59,    60,    61,    62,    63,    64,    65,
      66,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     5,     6,     7,     0,     9,     0,     0,     0,    10,
      11,    67,   214,    69,    12,     0,    13,    14,    15,   245,
     246,    18,    19,     0,     0,     0,     0,     0,    20,   247,
     248,    23,    24,    25,    26,     0,     0,   206,     0,     0,
       0,     0,     0,     0,    29,     0,     0,    32,    33,    34,
      35,    36,    37,    38,    39,    40,     0,    41,    42,    43,
      44,    45,    46,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    48,    49,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    50,    51,
       0,     0,     0,     0,     0,     0,     0,   208,     0,     0,
     209,    54,     0,    55,    56,     0,   520,     0,     0,    58,
      59,    60,    61,    62,    63,    64,    65,    66,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     0,     9,     0,     0,     0,    10,    11,    67,   214,
      69,    12,     0,    13,    14,    15,   245,   246,    18,    19,
       0,     0,     0,     0,     0,    20,   247,   248,    23,    24,
      25,    26,     0,     0,   206,     0,     0,     0,     0,     0,
       0,    29,     0,     0,    32,    33,    34,    35,    36,    37,
      38,    39,    40,     0,    41,    42,    43,    44,    45,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      48,    49,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    50,    51,     0,     0,     0,
       0,     0,     0,     0,   208,     0,     0,   209,    54,     0,
      55,    56,     0,   888,     0,     0,    58,    59,    60,    61,
      62,    63,    64,    65,    66,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     5,     6,     7,     0,     9,
       0,     0,     0,    10,    11,    67,   214,    69,    12,     0,
      13,    14,    15,   245,   246,    18,    19,     0,     0,     0,
       0,     0,    20,   247,   248,    23,    24,    25,    26,     0,
       0,   206,     0,     0,     0,     0,     0,     0,    29,     0,
       0,    32,    33,    34,    35,    36,    37,    38,    39,    40,
       0,    41,    42,    43,    44,    45,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    48,    49,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    50,    51,     0,     0,     0,     0,     0,     0,
       0,   208,     0,     0,   209,    54,     0,    55,    56,     0,
    1047,     0,     0,    58,    59,    60,    61,    62,    63,    64,
      65,    66,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     0,     9,     0,     0,     0,
      10,    11,    67,   214,    69,    12,     0,    13,    14,    15,
     245,   246,    18,    19,     0,     0,     0,     0,     0,    20,
     247,   248,    23,    24,    25,    26,     0,     0,   206,     0,
       0,     0,     0,     0,     0,    29,     0,     0,    32,    33,
      34,    35,    36,    37,    38,    39,    40,     0,    41,    42,
      43,    44,    45,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    48,    49,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    50,
      51,     0,     0,     0,     0,     0,     0,     0,   208,     0,
       0,   209,    54,     0,    55,    56,     0,     0,     0,     0,
      58,    59,    60,    61,    62,    63,    64,    65,    66,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     5,
       6,     7,     0,     9,     0,     0,     0,    10,    11,    67,
     214,    69,    12,     0,    13,    14,    15,    16,    17,    18,
      19,     0,     0,     0,     0,     0,    20,    21,    22,    23,
      24,    25,    26,     0,     0,   206,     0,     0,     0,     0,
       0,     0,    29,     0,     0,    32,    33,    34,    35,    36,
      37,    38,    39,    40,     0,    41,    42,    43,    44,    45,
      46,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    48,    49,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    50,    51,     0,     0,
       0,     0,     0,     0,     0,   208,     0,     0,   209,    54,
       0,    55,    56,     0,     0,     0,     0,    58,    59,    60,
      61,    62,    63,    64,    65,    66,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     0,
       9,     0,     0,     0,    10,    11,    67,   214,    69,    12,
       0,    13,    14,    15,    16,    17,    18,    19,     0,     0,
       0,     0,     0,    20,    21,    22,    23,    24,    25,    26,
       0,     0,    27,     0,     0,     0,     0,     0,     0,    29,
       0,     0,    32,    33,    34,    35,    36,    37,    38,    39,
      40,     0,    41,    42,    43,    44,    45,    46,    47,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    48,    49,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    50,    51,     0,     0,     0,     0,     0,
       0,     0,   208,     0,     0,   209,    54,     0,    55,    56,
       0,     0,     0,     0,    58,    59,    60,    61,    62,    63,
      64,    65,    66,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     5,     6,     7,     0,     9,     0,     0,
       0,    10,    11,    67,    68,    69,    12,     0,    13,    14,
      15,   245,   246,    18,    19,     0,     0,     0,     0,     0,
      20,   247,   248,    23,    24,    25,    26,     0,     0,   206,
       0,     0,     0,     0,     0,     0,   277,     0,     0,    32,
      33,    34,    35,    36,    37,    38,    39,    40,     0,    41,
      42,    43,    44,    45,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   278,
       0,     0,   328,    54,     0,    55,    56,     0,   329,     0,
       0,    58,    59,    60,    61,    62,    63,    64,    65,    66,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     0,
       9,     0,     0,     0,    10,    11,     0,     0,     0,    12,
     279,    13,    14,    15,   245,   246,    18,    19,     0,     0,
       0,     0,     0,    20,   247,   248,    23,    24,    25,    26,
       0,     0,   206,     0,     0,     0,     0,     0,     0,   277,
       0,     0,    32,    33,    34,    35,    36,    37,    38,    39,
      40,     0,    41,    42,    43,    44,    45,    46,    47,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   377,     0,     0,    53,    54,     0,    55,    56,
       0,    57,     0,     0,    58,    59,    60,    61,    62,    63,
      64,    65,    66,     0,     0,     0,     0,     0,     0,     5,
       6,     7,     0,     9,     0,     0,     0,    10,    11,     0,
       0,     0,    12,   279,    13,    14,    15,   245,   246,    18,
      19,     0,     0,     0,     0,     0,    20,   247,   248,    23,
      24,    25,    26,     0,     0,   206,     0,     0,     0,     0,
       0,     0,   277,     0,     0,    32,    33,    34,   385,    36,
      37,    38,   386,    40,     0,    41,    42,    43,    44,    45,
      46,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     387,     0,     0,     0,     0,   388,     0,     0,   209,    54,
       0,    55,    56,     0,     0,     0,     0,    58,    59,    60,
      61,    62,    63,    64,    65,    66,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     0,     9,     0,     0,     0,
      10,    11,     0,     0,     0,    12,   279,    13,    14,    15,
     245,   246,    18,    19,     0,     0,     0,     0,     0,    20,
     247,   248,    23,    24,    25,    26,     0,     0,   206,     0,
       0,     0,     0,     0,     0,   277,     0,     0,    32,    33,
      34,   385,    36,    37,    38,   386,    40,     0,    41,    42,
      43,    44,    45,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   388,     0,
       0,   209,    54,     0,    55,    56,     0,     0,     0,     0,
      58,    59,    60,    61,    62,    63,    64,    65,    66,     0,
       0,     0,     0,     0,     0,     5,     6,     7,     0,     9,
       0,     0,     0,    10,    11,     0,     0,     0,    12,   279,
      13,    14,    15,   245,   246,    18,    19,     0,     0,     0,
       0,     0,    20,   247,   248,    23,    24,    25,    26,     0,
       0,   206,     0,     0,     0,     0,     0,     0,   277,     0,
       0,    32,    33,    34,    35,    36,    37,    38,    39,    40,
       0,    41,    42,    43,    44,    45,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   278,     0,     0,   328,    54,     0,    55,    56,     0,
       0,     0,     0,    58,    59,    60,    61,    62,    63,    64,
      65,    66,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     0,     9,     0,     0,     0,    10,    11,     0,     0,
       0,    12,   279,    13,    14,    15,   245,   246,    18,    19,
       0,     0,     0,     0,     0,    20,   247,   248,    23,    24,
      25,    26,     0,     0,   206,     0,     0,     0,     0,     0,
       0,   277,     0,     0,    32,    33,    34,    35,    36,    37,
      38,    39,    40,     0,    41,    42,    43,    44,    45,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1035,     0,     0,   209,    54,     0,
      55,    56,     0,     0,     0,     0,    58,    59,    60,    61,
      62,    63,    64,    65,    66,     0,     0,     0,     0,     0,
       0,     5,     6,     7,     0,     9,     0,     0,     0,    10,
      11,     0,     0,     0,    12,   279,    13,    14,    15,   245,
     246,    18,    19,     0,     0,     0,     0,     0,    20,   247,
     248,    23,    24,    25,    26,     0,     0,   206,     0,     0,
       0,     0,     0,     0,   277,     0,     0,    32,    33,    34,
      35,    36,    37,    38,    39,    40,     0,    41,    42,    43,
      44,    45,    46,    47,    23,    24,    25,    26,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      32,    33,    34,   691,     0,     0,     0,   692,     0,   693,
      41,    42,    43,    44,    45,     0,     0,  1088,     0,     0,
     209,    54,     0,    55,    56,     0,     0,     0,   694,    58,
      59,    60,    61,    62,    63,    64,    65,    66,     0,     0,
       0,   695,   696,     0,     0,     0,     0,     0,     0,     0,
     697,     0,     0,   698,     0,     0,   699,   700,   279,   701,
     702,     0,   703,    59,   704,    61,    62,    63,    64,    65,
      66,    23,    24,    25,    26,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   705,     0,     0,    32,    33,    34,
     691,   279,     0,     0,   692,     0,     0,    41,    42,    43,
      44,    45,     0,     0,    23,    24,    25,    26,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      32,    33,    34,   691,     0,     0,     0,   692,   695,   696,
      41,    42,    43,    44,    45,     0,     0,   697,     0,     0,
     698,     0,     0,   699,   700,     0,   921,     0,     0,   703,
      59,    60,    61,    62,    63,    64,    65,    66,     0,     0,
       0,   695,   696,     0,     0,     0,     0,     0,     0,     0,
     697,   705,     0,   698,     0,     0,   699,   700,   279,   701,
       0,     0,   703,    59,    60,    61,    62,    63,    64,    65,
      66,    23,    24,    25,    26,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   705,     0,     0,    32,    33,    34,
     691,   279,     0,     0,   692,     0,     0,    41,    42,    43,
      44,    45,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   695,   696,
       0,     0,     0,     0,     0,     0,     0,   697,     0,     0,
     698,     0,     0,   699,   700,     0,     0,     0,     0,   703,
      59,    60,    61,    62,    63,    64,    65,    66,   565,   566,
       0,     0,   567,     0,     0,     0,     0,     0,     0,     0,
       0,   705,     0,     0,     0,     0,     0,     0,   279,     0,
       0,   170,   171,   172,   173,   174,   175,   176,   177,   178,
       0,     0,   179,   180,     0,     0,     0,     0,   181,   182,
     183,   184,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   185,   186,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   187,   188,   189,   190,
     191,   192,   193,   194,   195,   196,     0,   197,   198,   578,
     573,     0,     0,   579,   199,   233,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   170,   171,   172,   173,   174,   175,   176,   177,
     178,     0,     0,   179,   180,     0,     0,     0,     0,   181,
     182,   183,   184,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   185,   186,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   187,   188,   189,
     190,   191,   192,   193,   194,   195,   196,     0,   197,   198,
     626,   566,     0,     0,   627,   199,   233,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   170,   171,   172,   173,   174,   175,   176,
     177,   178,     0,     0,   179,   180,     0,     0,     0,     0,
     181,   182,   183,   184,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   185,   186,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   187,   188,
     189,   190,   191,   192,   193,   194,   195,   196,     0,   197,
     198,   629,   573,     0,     0,   630,   199,   233,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   170,   171,   172,   173,   174,   175,
     176,   177,   178,     0,     0,   179,   180,     0,     0,     0,
       0,   181,   182,   183,   184,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   185,   186,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   187,
     188,   189,   190,   191,   192,   193,   194,   195,   196,     0,
     197,   198,   654,   566,     0,     0,   655,   199,   233,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   170,   171,   172,   173,   174,
     175,   176,   177,   178,     0,     0,   179,   180,     0,     0,
       0,     0,   181,   182,   183,   184,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   185,   186,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     187,   188,   189,   190,   191,   192,   193,   194,   195,   196,
       0,   197,   198,   657,   573,     0,     0,   658,   199,   233,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   170,   171,   172,   173,
     174,   175,   176,   177,   178,     0,     0,   179,   180,     0,
       0,     0,     0,   181,   182,   183,   184,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   185,
     186,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   187,   188,   189,   190,   191,   192,   193,   194,   195,
     196,     0,   197,   198,   764,   566,     0,     0,   765,   199,
     233,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   170,   171,   172,
     173,   174,   175,   176,   177,   178,     0,     0,   179,   180,
       0,     0,     0,     0,   181,   182,   183,   184,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     185,   186,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   187,   188,   189,   190,   191,   192,   193,   194,
     195,   196,     0,   197,   198,   767,   573,     0,     0,   768,
     199,   233,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   170,   171,
     172,   173,   174,   175,   176,   177,   178,     0,     0,   179,
     180,     0,     0,     0,     0,   181,   182,   183,   184,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   185,   186,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   187,   188,   189,   190,   191,   192,   193,
     194,   195,   196,     0,   197,   198,   773,   566,     0,     0,
     774,   199,   233,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   170,
     171,   172,   173,   174,   175,   176,   177,   178,     0,     0,
     179,   180,     0,     0,     0,     0,   181,   182,   183,   184,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   185,   186,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   187,   188,   189,   190,   191,   192,
     193,   194,   195,   196,     0,   197,   198,   572,   573,     0,
       0,   574,   199,   233,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     170,   171,   172,   173,   174,   175,   176,   177,   178,     0,
       0,   179,   180,     0,     0,     0,     0,   181,   182,   183,
     184,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   185,   186,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   187,   188,   189,   190,   191,
     192,   193,   194,   195,   196,     0,   197,   198,   818,   566,
       0,     0,   819,   199,   233,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   170,   171,   172,   173,   174,   175,   176,   177,   178,
       0,     0,   179,   180,     0,     0,     0,     0,   181,   182,
     183,   184,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   185,   186,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   187,   188,   189,   190,
     191,   192,   193,   194,   195,   196,     0,   197,   198,   821,
     573,     0,     0,   822,   199,   233,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   170,   171,   172,   173,   174,   175,   176,   177,
     178,     0,     0,   179,   180,     0,     0,     0,     0,   181,
     182,   183,   184,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   185,   186,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   187,   188,   189,
     190,   191,   192,   193,   194,   195,   196,     0,   197,   198,
    1159,   566,     0,     0,  1160,   199,   233,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   170,   171,   172,   173,   174,   175,   176,
     177,   178,     0,     0,   179,   180,     0,     0,     0,     0,
     181,   182,   183,   184,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   185,   186,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   187,   188,
     189,   190,   191,   192,   193,   194,   195,   196,     0,   197,
     198,  1162,   573,     0,     0,  1163,   199,   233,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   170,   171,   172,   173,   174,   175,
     176,   177,   178,     0,     0,   179,   180,     0,     0,     0,
       0,   181,   182,   183,   184,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   185,   186,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   187,
     188,   189,   190,   191,   192,   193,   194,   195,   196,     0,
     197,   198,  1190,   566,     0,     0,  1191,   199,   233,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   170,   171,   172,   173,   174,
     175,   176,   177,   178,     0,     0,   179,   180,     0,     0,
       0,     0,   181,   182,   183,   184,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   185,   186,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     187,   188,   189,   190,   191,   192,   193,   194,   195,   196,
       0,   197,   198,     0,     0,     0,     0,     0,   199,   233
};

static const yytype_int16 yycheck[] =
{
       2,     8,    71,   540,    60,    59,    16,    17,    91,    92,
      22,   102,    95,   365,    28,    53,     8,   227,    94,    22,
     331,    28,     4,   388,   331,   927,    95,   759,    92,    57,
      57,    95,    89,   370,    68,   700,    28,    13,    27,     2,
      29,     4,   752,   271,   101,    99,    67,   275,    71,    95,
    1060,    52,    59,    55,    56,    83,    53,    16,    17,    83,
     483,    26,   434,   426,    16,    17,    78,   976,    96,    97,
      98,   434,    52,  1009,  1118,    78,  1086,    26,   430,     2,
     902,     4,    37,    38,   101,    29,  1120,   909,    25,    27,
     100,    54,    99,    16,    17,   259,    55,    68,   127,    34,
      16,    17,   474,    13,   527,   831,    66,   540,   415,   416,
       0,    85,    25,   839,   295,   296,    13,    52,   147,    25,
     607,   154,   100,    25,  1026,    13,    97,   519,   993,   994,
      53,    54,  1094,   485,    57,   101,    13,   154,    34,    55,
      56,   100,  1051,  1052,  1080,    68,   147,    25,   100,   127,
    1015,  1016,  1017,   236,    25,   238,    52,   213,   132,    25,
      83,  1205,   122,   123,   328,   222,  1176,   147,    91,    92,
    1204,   209,    95,    96,    97,    98,   152,   100,   154,   144,
     124,   147,  1004,  1193,   100,   850,   851,  1098,   144,    25,
     582,  1071,   261,   153,   251,   144,   588,  1099,   590,   154,
     149,   927,   363,   364,    13,   366,   244,   540,    13,  1118,
     147,  1120,   415,   416,   152,   261,   154,   695,   696,   221,
     230,   259,   232,   233,  1186,   227,   283,   310,   311,   312,
     313,   233,   315,   316,   147,   445,   248,   249,   261,   604,
     242,   147,   152,   595,   154,   147,   310,   311,   312,   313,
     147,   412,   280,   605,    25,   152,   280,   154,   279,    13,
      13,   684,   259,  1128,   152,   637,   154,   700,   429,   147,
     431,   230,   635,   232,   637,   152,   147,   154,   230,   242,
     232,   147,   110,   149,  1149,   772,   209,  1198,   277,   372,
     328,  1171,   326,    68,   257,  1204,  1205,   331,   459,    13,
    1026,   329,   329,  1029,   387,   374,   134,   230,   372,   232,
     233,   147,    97,   236,   230,   238,   232,  1049,   499,   242,
     501,   244,    97,   380,   628,   486,    13,   309,   374,    66,
      68,   328,   697,   371,   257,   373,   259,   689,   144,  1049,
     149,   912,    66,   152,   149,   154,   309,   152,   885,   154,
      56,   314,   656,   100,   366,   365,   363,   280,    58,    97,
     693,   146,   153,   365,   149,   698,   699,   326,   760,   847,
     848,   763,   331,  1099,    66,    67,   147,    77,   149,   686,
     127,   415,   416,   120,    25,   777,   309,   310,   311,   312,
     313,   314,   315,   316,    15,   699,   120,   153,   152,   152,
     154,   154,    58,   326,    25,   328,   329,   894,   331,   109,
     110,   153,    66,   846,   901,   625,    28,   850,   851,   431,
     430,    77,   429,   651,   101,    68,   733,   734,   430,   916,
     122,   123,   739,   740,   134,   596,   747,   153,   152,   453,
     154,   443,   365,   445,   367,   606,   453,   459,   371,   372,
     373,   458,   885,   109,    97,   483,  1166,   113,   152,   483,
     154,   453,   766,   100,   387,   152,   120,   154,   122,   123,
     147,   125,   776,    68,   486,   485,   900,   484,   902,   415,
     416,   538,   663,   485,   849,   909,   127,   668,   880,  1060,
     127,   883,   415,   416,   886,   127,   432,   433,   147,   527,
    1071,   893,    97,   527,   896,    68,   147,   430,   149,   150,
     426,   844,    68,   154,    66,  1086,   820,   850,   851,    66,
     589,   597,    68,  1094,   460,   608,   830,   688,   127,   690,
     733,   734,    68,    96,    97,   907,   739,   740,   113,   100,
      96,    97,    98,   466,   907,   100,   850,   585,    68,   587,
     100,    97,   885,   890,   861,   471,    77,   640,   570,   149,
     483,    97,   485,   147,   154,   554,   127,   570,   120,   581,
     122,   123,   127,    25,   966,   122,   123,    97,   581,   581,
    1004,   583,   792,   149,   596,   595,   147,   594,   577,   147,
     146,    25,  1079,   595,   606,   605,   154,   604,   931,   100,
    1171,   613,   912,   605,   527,   619,   153,    68,   100,  1001,
     613,   613,   619,    25,   951,  1186,   137,   138,   139,   956,
      82,    83,   959,   625,   652,    68,   127,   619,   652,    15,
    1063,    17,   644,   645,   100,   127,    97,   941,   942,   628,
      66,    68,   147,   645,   100,    56,   147,    52,   704,   154,
    1137,    56,   686,    96,    97,    98,   684,  1090,   861,   844,
     684,   127,   585,   127,   587,   850,   851,   656,   130,   131,
      97,   127,   595,    58,   900,   127,   902,   147,   690,   689,
     101,   688,   605,   909,   607,   608,   912,   689,   771,   147,
     781,   147,    77,   127,   120,   147,   122,   123,   150,   733,
     734,   770,   154,   146,    68,   739,   740,   771,     9,    77,
     699,    68,   100,   147,    15,   127,   150,   640,   681,   635,
     154,   912,   685,   147,   109,    77,   780,    68,   113,   652,
     154,   769,    96,    97,    98,   147,   149,   736,   150,    96,
      97,    98,   154,  1135,   743,   747,   748,   100,   750,   149,
     752,   149,  1104,  1063,   150,    96,    97,    98,   681,   151,
     145,   684,   685,   686,    66,    68,   689,   135,   136,   137,
     138,   139,    68,   780,   127,   145,  1086,   766,  1004,   835,
    1090,   100,   146,   127,  1094,   912,  1096,   776,   747,   146,
     792,  1183,   149,    96,    97,    98,   879,   733,   734,    56,
      96,    97,    98,   739,   740,   146,   100,   809,   127,   811,
     733,   734,   100,    37,    38,   879,   739,   740,   120,   147,
     122,   123,     2,   125,     4,    66,   895,   861,   147,   100,
      77,   820,   107,   127,  1060,   873,    16,    17,   100,   127,
     878,   830,   100,   146,   100,  1071,   769,   147,   771,   772,
     146,   787,   788,   147,   790,   791,   127,  1161,    56,   147,
    1086,   850,    68,    88,    89,   127,   100,   869,  1094,   127,
      25,   127,  1063,    53,    54,   913,  1186,  1068,  1188,    68,
     152,   122,   123,  1193,   125,  1195,    54,   131,    68,   147,
      96,    97,    98,   127,    52,  1086,    64,    65,    56,  1090,
     150,    68,   147,  1094,   147,  1096,    68,    96,    97,    98,
     145,    91,    92,   145,  1224,    95,   954,   147,    68,    66,
     100,   912,   950,   912,   147,   861,   964,    14,    15,    96,
      97,    98,   145,  1002,    96,    97,    98,    10,   861,   147,
     146,    40,    41,   147,  1105,  1171,    96,    97,    98,    44,
     873,   147,   941,   942,    66,   878,   879,   146,    44,  1086,
    1186,   127,  1045,  1046,     8,    25,    13,  1094,    66,  1096,
     906,   894,    17,   120,   153,   122,   123,   153,   901,   146,
    1037,  1045,  1046,   147,   146,  1013,   145,    52,   132,   991,
     913,   993,   994,   916,   147,  1186,   146,  1188,    59,    60,
      61,    62,  1193,  1041,  1195,  1043,    54,    55,   120,    57,
     122,   123,   129,  1015,  1016,  1017,    64,    65,   151,    15,
    1009,     2,   120,     4,   122,   123,   149,   950,   147,   209,
     101,   954,   147,  1224,   147,    16,    17,   145,   147,    52,
    1042,   964,   101,   147,  1082,   147,   147,  1049,   236,   127,
     230,   147,   232,   233,   147,   147,   236,  1085,   238,  1186,
      52,  1188,   242,   147,   244,   147,  1193,    52,  1195,   132,
     127,  1109,    53,    54,    56,   147,   264,   257,   147,   259,
     268,   147,   996,    26,   998,   147,     9,    68,   152,   147,
    1013,  1080,   147,  1105,  1104,  1086,  1103,  1224,   147,  1090,
    1138,   132,  1104,  1094,  1106,  1096,  1108,    26,    56,   147,
      91,    92,   147,    52,    95,   147,   150,   147,  1041,   100,
    1043,   121,  1045,  1046,   147,    68,  1128,   147,   147,   309,
     310,   311,   312,   313,   314,   315,   316,   147,   147,   147,
    1147,    68,   145,   149,   149,   147,   326,  1149,   328,    68,
     147,   331,   242,    96,    97,    98,  1079,   540,   462,  1082,
     466,    96,  1085,    87,  1166,    98,  1155,    26,   613,    96,
      97,    98,  1161,  1019,  1020,   748,  1108,    96,    97,    98,
     912,  1104,  1051,   811,  1118,   365,  1109,   367,   840,  1198,
     782,   371,   372,   373,    68,  1186,   324,  1188,   515,  1166,
     885,   144,  1193,   146,  1195,   845,   149,   387,   846,    68,
    1124,  1125,   846,    68,  1137,  1138,  1130,   912,  1132,   146,
    1134,   846,    96,    97,    98,   144,   843,   146,   209,  1042,
     149,  1106,   105,  1224,   501,   415,   416,    96,    97,    98,
    1003,    96,    97,    98,   233,    99,  1063,  1090,   436,   230,
     430,   232,   233,   441,  1086,   236,   444,   238,   540,   447,
    1060,   242,    52,   244,    54,    55,    56,    57,     2,    -1,
       4,    -1,   146,    -1,    -1,   463,   257,    -1,   259,    52,
     468,    54,    55,    56,    57,   144,   466,   146,    -1,    -1,
     149,   146,  1206,  1207,  1208,  1209,    -1,    -1,  1144,  1145,
      -1,    -1,    -1,    -1,  1150,   485,  1152,  1153,    -1,    -1,
     693,    -1,   695,   696,  1228,   698,   699,    -1,    -1,    53,
      54,    -1,    52,    57,    54,    55,    56,    57,   309,   310,
     311,   312,   313,   314,   315,   316,    77,    -1,   526,    40,
      41,    42,    43,    44,    -1,   326,    -1,   328,    -1,    83,
     331,    -1,    -1,    94,    95,    -1,    -1,    -1,    -1,    -1,
      77,    -1,    96,    97,    98,    99,  1212,  1213,  1214,  1215,
      -1,    -1,   102,    -1,    -1,    -1,  1071,    94,    95,  1074,
      -1,    -1,    -1,  1229,   365,    -1,   367,    -1,    -1,    -1,
     371,   372,   373,   134,   135,   136,   137,   138,   139,  1094,
      -1,  1096,    -1,    -1,    -1,   585,   387,   587,    -1,    -1,
      -1,   693,    -1,   695,   696,   595,   698,   699,   135,   136,
     137,   138,   139,    -1,    -1,   605,    -1,   607,   608,    -1,
      -1,    56,    -1,    -1,   415,   416,    -1,    -1,    52,    -1,
      54,    55,    56,    57,    58,    -1,    -1,    -1,   831,   430,
      -1,    -1,    -1,    -1,    -1,    -1,   839,    -1,    -1,    -1,
     640,   844,   845,    77,   847,   848,    -1,   850,   851,    -1,
      -1,    -1,    -1,    -1,    -1,   209,  1171,    -1,  1173,    -1,
      77,    -1,    -1,  1178,    -1,   466,    -1,    -1,   102,    -1,
      -1,  1186,    -1,  1188,    -1,   109,   110,    94,    95,    -1,
    1195,   681,   885,    -1,   485,   685,   686,    -1,   242,   689,
     244,    52,    -1,    54,    55,    56,    57,    -1,    -1,   540,
     134,    -1,    -1,   257,  1219,   259,    -1,    -1,    -1,  1224,
      -1,    -1,    -1,    -1,    -1,   132,   133,   134,   135,   136,
     137,   138,   139,    -1,   927,    -1,   280,    -1,   931,   831,
      -1,    -1,    -1,   733,   734,    -1,    -1,   839,    -1,   739,
     740,   102,   844,   845,    -1,   847,   848,   108,   850,   851,
      -1,    -1,    -1,    -1,    -1,   309,    -1,    -1,    -1,    -1,
     314,    -1,   207,    -1,    -1,   210,   211,   212,    -1,   769,
     778,   771,   772,    -1,   328,   329,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   885,   585,   793,   587,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   595,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   605,    -1,   607,   608,    -1,    -1,
      -1,    -1,    -1,   367,    -1,    -1,    -1,   371,    -1,   373,
     828,    -1,    -1,  1026,    -1,   927,  1029,    -1,    -1,   931,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   640,
      -1,    -1,    -1,   851,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   693,    -1,   695,   696,    -1,   698,   699,    -1,
      -1,   861,    -1,    -1,    52,    -1,    54,    55,    56,    57,
      58,    -1,    -1,   873,    -1,    -1,    -1,    -1,   878,   879,
     681,   326,    -1,    -1,   685,   686,   331,    -1,   689,    77,
      -1,    -1,    -1,    -1,   894,    -1,  1099,    -1,    -1,    -1,
      -1,   901,    -1,    52,    -1,    54,    55,    56,    57,    58,
      -1,    -1,   466,   913,   102,    -1,   916,    -1,    -1,    -1,
     108,   109,   110,    -1,  1026,    -1,    -1,  1029,    77,   483,
      -1,    -1,   733,   734,    -1,    -1,   944,   945,   739,   740,
      -1,     2,    91,     4,    -1,    -1,   134,    -1,    -1,   137,
      -1,    -1,    -1,   102,   954,    -1,    -1,    -1,    -1,   108,
     109,   110,    -1,    -1,   964,    -1,   154,    -1,   769,    -1,
     771,   772,    -1,   527,   982,    -1,    -1,    -1,    -1,    -1,
     425,   426,    -1,    -1,    -1,   134,   540,    -1,   137,   434,
     831,    -1,    53,    54,    -1,    -1,    57,  1099,   839,    -1,
     149,    -1,    -1,   844,   845,    -1,   847,   848,    -1,   850,
     851,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    83,    -1,    -1,    -1,   471,    -1,    -1,   474,
      -1,   585,    -1,   587,    -1,    96,    97,    98,    -1,    -1,
      -1,  1041,    -1,  1043,   885,  1045,  1046,  1055,    -1,    -1,
      -1,    -1,    -1,   607,    -1,    -1,    -1,    -1,    -1,    -1,
     861,    -1,    -1,    -1,    52,    -1,    54,    55,    56,    57,
      58,    -1,   873,    -1,    -1,   520,    -1,   878,   879,  1079,
      -1,    -1,  1082,    -1,    -1,    -1,   927,    -1,    -1,    77,
     931,    -1,    -1,   894,    -1,    -1,    -1,    -1,   652,    -1,
     901,    -1,    -1,    91,  1104,    -1,    -1,    -1,  1116,  1109,
      -1,    -1,   913,    -1,   102,   916,    -1,    -1,    -1,    -1,
     108,   109,   110,    -1,    -1,    -1,    -1,   681,    -1,    -1,
     684,   685,    -1,    -1,    -1,    -1,    -1,  1137,  1138,   693,
      -1,   695,   696,    -1,   698,   699,   134,    -1,   209,   137,
      -1,    -1,    -1,   954,    52,    -1,    54,    55,    56,    57,
      58,    -1,    -1,   964,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   620,    -1,   622,    -1,    77,
      -1,   242,    -1,   244,    -1,  1026,    -1,    -1,  1029,    -1,
     635,    -1,   637,    -1,    -1,    -1,   257,    -1,   259,    -1,
      -1,    -1,    -1,    -1,   102,    -1,    -1,    -1,    -1,    -1,
     108,   109,   110,    -1,    -1,   769,    -1,    -1,   772,   280,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   680,   134,    -1,    -1,   137,
    1041,    -1,  1043,    -1,  1045,  1046,    -1,    -1,   309,    -1,
      -1,    -1,    -1,   314,    -1,    -1,    -1,    -1,  1099,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   328,   329,    -1,
     331,    -1,    -1,    -1,    -1,    -1,    -1,   831,  1079,    -1,
      -1,  1082,    -1,    -1,    -1,   839,    -1,    -1,    -1,    -1,
     844,   845,    -1,   847,   848,    -1,   850,   851,    -1,    -1,
      -1,    -1,   747,  1104,    -1,    -1,   367,    -1,  1109,    -1,
     371,    -1,   373,    -1,    -1,    -1,    -1,   762,    -1,   873,
      -1,    -1,    -1,     2,   878,     4,    -1,    -1,    -1,    -1,
      -1,   885,     2,    -1,     4,    -1,  1137,  1138,    -1,    -1,
     894,    -1,    -1,    -1,    -1,    -1,    -1,   901,    -1,    -1,
      -1,    -1,    -1,    -1,   415,   416,    -1,    -1,    -1,   913,
      -1,    -1,   916,    -1,    -1,    -1,    -1,   812,    -1,    -1,
      -1,    -1,    -1,   927,    53,    54,    -1,   931,    57,    -1,
      -1,    -1,    -1,    53,    54,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   950,    -1,    -1,    -1,
     954,    -1,    -1,    -1,    83,   466,    -1,    -1,    -1,    -1,
     964,    -1,    33,    34,    35,    36,    -1,    96,    97,    98,
      99,    -1,   483,    -1,    -1,    -1,    96,    -1,    49,    50,
      51,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    59,    60,
      61,    62,    63,   888,    -1,    -1,    -1,    -1,    -1,     2,
      -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1013,
     540,    -1,   907,    -1,    -1,    -1,   527,    -1,    -1,    -1,
      -1,    -1,  1026,    -1,    -1,  1029,    -1,    -1,   540,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1041,    -1,  1043,
     111,   112,   113,   114,   115,   116,   117,   118,   119,    -1,
      53,    54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   963,   140,
      -1,    -1,    -1,    -1,   585,  1079,   587,    -1,  1082,    -1,
     209,  1085,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   209,
      -1,    -1,    -1,    -1,   989,  1099,   607,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   242,    -1,   244,    -1,   540,    -1,    -1,
      -1,    -1,   242,    -1,   244,    -1,    -1,    -1,   257,    -1,
     259,    -1,    -1,  1137,  1138,    -1,    -1,   257,    -1,   259,
      -1,   652,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     661,   280,  1047,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   693,  1059,   695,   696,    -1,   698,   699,
     681,    -1,    -1,   684,   685,   686,    -1,    -1,    -1,    -1,
     309,   693,    -1,   695,   696,   314,   698,   699,    -1,   309,
      -1,    -1,    -1,    -1,   314,    -1,    -1,    -1,    -1,   328,
     329,    -1,    -1,    -1,    -1,    -1,   209,    -1,   328,    -1,
      -1,   331,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   733,   734,    -1,    -1,    -1,    -1,   739,   740,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   367,   242,
      -1,   244,   371,    -1,   373,    -1,    -1,   367,    -1,    -1,
      -1,   371,    -1,   373,   257,    -1,   259,    -1,   769,    -1,
      -1,   772,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     693,    -1,   695,   696,    -1,   698,   699,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   415,   416,    -1,    -1,    -1,
      -1,   831,    -1,    -1,    -1,    -1,   309,    -1,    -1,   839,
      -1,   314,    -1,    -1,   844,   845,    -1,   847,   848,   831,
     850,   851,    -1,    -1,    -1,   328,   540,   839,   331,    -1,
      -1,    -1,   844,   845,    -1,   847,   848,   466,   850,   851,
      -1,    -1,    -1,    -1,    -1,    -1,   466,    -1,    -1,    -1,
     861,    -1,    -1,    -1,   483,   885,    -1,    -1,    -1,    -1,
      -1,    -1,   873,    -1,   367,    -1,    -1,   878,   371,    -1,
     373,    -1,    -1,   885,    -1,    -1,    77,    78,    79,    80,
      81,    82,    83,   894,    -1,    86,    87,    -1,   540,    -1,
     901,    -1,    -1,    94,    95,    -1,    -1,   927,   527,    -1,
      -1,   931,   913,    -1,    -1,   916,    -1,    -1,   831,    -1,
      -1,    -1,   415,   416,    -1,   927,   839,    -1,    -1,   931,
      -1,   844,   845,    -1,   847,   848,    -1,   850,   851,   130,
     131,   132,   133,   134,   135,   136,   137,   138,   139,   950,
      -1,    -1,    -1,   954,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   964,    -1,    -1,   585,    -1,   587,    -1,
      -1,    -1,   885,   466,    -1,   585,    -1,   587,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   607,   693,
      -1,   695,   696,    -1,   698,   699,    -1,   607,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1026,    -1,    -1,  1029,
      -1,    -1,  1013,    -1,   927,    -1,    -1,    -1,   931,    -1,
      -1,    -1,    -1,    -1,  1026,    -1,    -1,  1029,    -1,    -1,
      -1,    -1,    -1,   652,    -1,    -1,    -1,    -1,    -1,    -1,
    1041,    -1,  1043,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   693,    -1,   695,   696,    -1,   698,   699,    -1,    -1,
      -1,    -1,   681,    -1,    -1,   684,   685,    -1,    -1,    -1,
      -1,   681,    -1,    -1,    -1,   685,   686,    -1,  1079,  1099,
      -1,  1082,    -1,    -1,  1085,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   585,    -1,   587,    -1,    -1,  1099,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1109,    -1,
      -1,    -1,    -1,  1026,   607,    -1,  1029,    -1,    -1,    -1,
      -1,    -1,    -1,   733,   734,    -1,    -1,   831,    -1,   739,
     740,    -1,    -1,    -1,    -1,   839,  1137,  1138,    -1,    -1,
     844,   845,    -1,   847,   848,    -1,   850,   851,    -1,    -1,
     769,    -1,    -1,   772,    -1,    -1,    -1,    -1,    -1,   769,
      -1,    -1,   772,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   885,    -1,    -1,    -1,    -1,  1099,    -1,   681,   831,
      -1,    -1,   685,   686,    -1,    -1,    -1,   839,    -1,    -1,
      -1,    -1,   844,   845,    -1,   847,   848,    -1,   850,   851,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   927,    -1,    -1,    -1,   931,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     733,   734,    -1,   885,    -1,    -1,   739,   740,    -1,    -1,
      -1,   861,    -1,    -1,   873,    -1,    -1,    -1,    -1,   878,
      -1,    -1,    -1,   873,    -1,    -1,    -1,    -1,   878,    -1,
      -1,    -1,    -1,    -1,    -1,   894,   769,    -1,    -1,   772,
      -1,    -1,   901,    -1,   894,   927,    -1,    -1,    -1,   931,
      -1,   901,    -1,    -1,   913,    -1,    -1,   916,    -1,    -1,
      -1,    -1,    -1,   913,    -1,    -1,   916,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1026,    -1,    -1,  1029,    -1,    -1,    -1,    -1,
      -1,   950,    -1,    -1,    -1,   954,    -1,    -1,    -1,    -1,
     950,    -1,    -1,    -1,   954,   964,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   964,    -1,    -1,    -1,    16,    17,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   861,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     873,    -1,    -1,    -1,  1026,   878,    -1,  1029,    -1,    -1,
      48,    49,    50,    51,  1013,  1099,    -1,    55,    56,    -1,
      -1,   894,    -1,    -1,    -1,    -1,    -1,    -1,   901,    -1,
      68,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     913,    -1,  1041,   916,  1043,    -1,    -1,    -1,    -1,    -1,
      -1,  1041,    -1,  1043,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   100,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1099,    -1,    -1,
    1079,   954,    -1,  1082,    -1,    -1,  1085,    -1,    -1,  1079,
       0,   964,  1082,    -1,    -1,    -1,    -1,    -1,     8,     9,
      10,    -1,    -1,    13,    14,    15,    -1,    17,    -1,    -1,
    1109,    -1,    -1,    -1,    -1,    25,    26,    27,    -1,  1109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,
      40,    41,    42,    43,    44,    -1,    -1,    -1,  1137,  1138,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1137,  1138,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    68,    -1,
      -1,    -1,    -1,    25,    -1,    -1,    -1,    -1,  1041,   207,
    1043,    -1,   210,   211,   212,    -1,   214,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    96,    97,    -1,    -1,
      -1,    -1,   230,    -1,   232,   233,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1079,    -1,    -1,  1082,
      -1,   121,    -1,    -1,    -1,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    -1,    -1,
      -1,    -1,    94,    95,   144,   145,  1109,    -1,    -1,   149,
     150,    -1,   152,    -1,   154,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1137,  1138,   128,    -1,   130,   131,
     132,   133,   134,   135,   136,   137,   138,   139,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   326,    -1,
      -1,    -1,    -1,   331,    -1,   333,   334,   335,   336,   337,
      -1,    -1,   340,   341,   342,   343,   344,   345,   346,   347,
     348,    -1,    -1,   351,   352,   353,   354,   355,   356,   357,
     358,   359,   360,    -1,    -1,    52,    53,   365,    -1,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    -1,    -1,    86,
      87,    -1,    -1,    -1,    -1,    92,    93,    94,    95,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   415,   416,    -1,
      -1,   108,   109,    -1,    -1,    -1,   424,   425,   426,    -1,
      -1,    -1,   430,    -1,   432,   433,   434,    -1,    -1,    -1,
      -1,    -1,   440,   130,   131,   132,   133,   134,   135,   136,
     137,   138,   139,    -1,   141,   142,    -1,   455,    -1,    -1,
      -1,   148,   460,    33,    34,    35,    36,    -1,    -1,    -1,
      -1,    -1,    -1,   471,    -1,    -1,   474,    -1,    -1,    49,
      50,    51,    52,    -1,    -1,    -1,    56,   485,    58,    59,
      60,    61,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   503,    -1,    77,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    91,   520,    -1,    -1,    -1,    -1,    -1,    -1,    99,
      -1,    -1,   102,    -1,    -1,   105,   106,    -1,   108,   109,
      -1,   111,   112,   113,   114,   115,   116,   117,   118,   119,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   133,    -1,    -1,    -1,    -1,    -1,    -1,
     140,    -1,    -1,     0,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     8,     9,    10,   154,    -1,    13,    14,    15,    -1,
      17,    -1,    -1,    -1,    -1,    -1,    -1,   595,    25,    26,
      27,    28,    29,    -1,    -1,    -1,    -1,   605,    -1,    -1,
      37,    38,    -1,    40,    41,    42,    43,    44,    -1,    -1,
      -1,    -1,   620,    -1,   622,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   635,    -1,   637,
      -1,    68,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    -1,    -1,    -1,    -1,    94,    95,    96,
      97,    98,    -1,   100,   101,    -1,    -1,    -1,    -1,    -1,
     107,    -1,   680,    -1,    -1,    -1,    -1,    -1,   686,   687,
      -1,   689,    -1,    -1,   121,    -1,    -1,   124,    -1,    -1,
     127,   128,   129,   130,   131,   132,   133,   134,   135,   136,
     137,   138,   139,    -1,    -1,    -1,    -1,   144,   145,   146,
     147,    -1,    -1,   150,   151,   152,    -1,   154,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   733,   734,    -1,    -1,    -1,
      -1,   739,   740,    33,    34,    35,    36,    -1,    -1,   747,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,
      50,    51,    52,    -1,   762,    -1,    56,    -1,    -1,    59,
      60,    61,    62,    63,    -1,    -1,    -1,    77,    78,    79,
      80,    81,    82,    83,    84,    -1,    86,    87,    -1,   787,
     788,    -1,   790,   791,    94,    95,    -1,    -1,    -1,    -1,
      90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,
      -1,    -1,   102,    -1,   812,   105,   106,    -1,   108,    -1,
      -1,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     130,   131,   132,   133,   134,   135,   136,   137,   138,   139,
      -1,    -1,    -1,   133,    -1,    -1,    -1,    -1,    -1,    -1,
     140,    -1,    -1,    -1,    -1,    -1,   854,    -1,    -1,    -1,
      -1,    -1,    -1,   861,   154,     0,     1,    -1,     3,     4,
       5,     6,     7,    -1,    -1,    -1,    11,    12,    -1,    -1,
      -1,    16,    -1,    18,    19,    20,    21,    22,    23,    24,
     888,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,   906,   907,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      75,    76,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    91,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    99,   963,    -1,   102,   103,    -1,
     105,   106,    -1,   108,    -1,    -1,   111,   112,   113,   114,
     115,   116,   117,   118,   119,    -1,     0,    -1,    -1,    -1,
      -1,   989,    -1,    -1,     8,     9,    10,    -1,    -1,    13,
      14,    15,    -1,    17,    -1,   140,   141,   142,    -1,    -1,
      -1,    25,    -1,    27,    28,    29,    -1,   152,    -1,   154,
      -1,    -1,    -1,    37,    38,    -1,    40,    41,    42,    43,
      44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    77,    78,    79,    80,    81,    82,    83,    -1,  1047,
      86,    87,    -1,    -1,    68,    -1,    -1,    -1,    94,    95,
      -1,  1059,    -1,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    -1,    -1,    -1,    -1,
      94,    95,    96,    97,    98,    -1,   100,   101,    -1,    -1,
      -1,    -1,    -1,   107,   130,   131,   132,   133,   134,   135,
     136,   137,   138,   139,    -1,    -1,  1104,   121,    -1,    -1,
     124,    -1,    -1,   127,   128,   129,   130,   131,   132,   133,
     134,   135,   136,   137,   138,   139,    -1,    -1,    -1,    -1,
      -1,   145,   146,   147,     0,    -1,   150,   151,   152,    -1,
     154,    -1,     8,     9,    10,    -1,    -1,    13,    14,    15,
      -1,    17,    -1,    -1,    -1,    -1,    44,    -1,    -1,    25,
      -1,    27,    28,    29,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    37,    38,    -1,    40,    41,    42,    43,    44,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    68,    -1,    -1,    -1,    94,    95,    -1,    -1,
      -1,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    -1,    -1,    -1,    -1,    94,    95,
      96,    97,    98,    -1,   100,   101,    -1,    -1,    -1,    -1,
     128,   107,   130,   131,   132,   133,   134,   135,   136,   137,
     138,   139,    -1,    -1,    -1,   121,    -1,    -1,   124,   147,
      -1,   127,   128,   129,   130,   131,   132,   133,   134,   135,
     136,   137,   138,   139,    -1,    -1,    -1,    -1,    -1,   145,
     146,   147,     0,    -1,   150,   151,   152,    -1,   154,    -1,
       8,     9,    10,    -1,    -1,    13,    14,    15,    -1,    17,
      -1,    -1,    -1,    -1,    44,    -1,    -1,    25,    26,    27,
      28,    29,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,
      38,    -1,    40,    41,    42,    43,    44,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      68,    -1,    -1,    -1,    94,    95,    -1,    -1,    -1,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    -1,    -1,    -1,    -1,    94,    95,    96,    97,
      98,    -1,    -1,   101,    -1,    -1,    -1,    -1,   128,   107,
     130,   131,   132,   133,   134,   135,   136,   137,   138,   139,
      -1,    -1,    -1,   121,    -1,    -1,   124,    -1,    -1,    -1,
     128,   129,   130,   131,   132,   133,   134,   135,   136,   137,
     138,   139,    -1,    -1,    -1,    -1,   144,   145,   146,   147,
       0,    -1,   150,   151,   152,    -1,   154,    -1,     8,     9,
      10,    -1,    -1,    13,    14,    15,    -1,    17,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    25,    26,    27,    28,    29,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,
      40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    68,    -1,
      -1,    -1,    94,    95,    -1,    -1,    -1,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      -1,    -1,    -1,    -1,    94,    95,    96,    97,    98,    -1,
      -1,   101,    -1,    -1,    -1,    -1,   128,   107,   130,   131,
     132,   133,   134,   135,   136,   137,   138,   139,    -1,    -1,
      -1,   121,    -1,    -1,   124,    -1,    -1,    -1,   128,   129,
     130,   131,   132,   133,   134,   135,   136,   137,   138,   139,
      -1,    -1,    -1,    -1,   144,   145,   146,   147,     0,    -1,
     150,   151,   152,    -1,   154,    -1,     8,     9,    10,    -1,
      -1,    13,    14,    15,    -1,    17,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    25,    -1,    27,    28,    29,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    68,    -1,    -1,    -1,
      94,    95,    -1,    -1,    -1,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    -1,    -1,
      -1,    -1,    94,    95,    96,    97,    98,    -1,   100,   101,
      -1,    -1,    -1,    -1,    -1,   107,   130,   131,   132,   133,
     134,   135,   136,   137,   138,   139,    -1,    -1,    -1,   121,
      -1,    -1,   124,    -1,    -1,   127,   128,   129,   130,   131,
     132,   133,   134,   135,   136,   137,   138,   139,    -1,    -1,
      -1,    -1,    -1,   145,   146,   147,     0,    -1,   150,   151,
     152,    -1,   154,    -1,     8,     9,    10,    -1,    -1,    13,
      14,    15,    -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    25,    26,    27,    28,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    37,    38,    -1,    40,    41,    42,    43,
      44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    68,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    -1,    -1,    -1,    -1,
      94,    95,    96,    97,    98,    -1,    -1,   101,    -1,    -1,
      -1,    -1,    -1,   107,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   121,    -1,    -1,
      -1,    -1,    -1,    -1,   128,    -1,   130,   131,   132,   133,
     134,   135,   136,   137,   138,   139,    -1,    -1,    -1,    -1,
     144,   145,   146,   147,     0,   149,   150,   151,   152,    -1,
     154,    -1,     8,     9,    10,    -1,    -1,    13,    14,    15,
      -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,
      -1,    27,    28,    29,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    37,    38,    -1,    40,    41,    42,    43,    44,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    68,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    -1,    -1,    -1,    -1,    94,    95,
      96,    97,    98,    -1,    -1,   101,    -1,    -1,    -1,    -1,
      -1,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   121,    -1,    -1,   124,    -1,
      -1,    -1,   128,   129,   130,   131,   132,   133,   134,   135,
     136,   137,   138,   139,    -1,    -1,    -1,    -1,    -1,   145,
     146,   147,     0,    -1,   150,   151,   152,    -1,   154,    -1,
       8,     9,    10,    -1,    -1,    13,    14,    15,    -1,    17,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,    26,    27,
      28,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,
      38,    -1,    40,    41,    42,    43,    44,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      68,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    -1,    -1,    -1,    -1,    94,    95,    96,    97,
      98,    -1,    -1,   101,    -1,    -1,    -1,    -1,    -1,   107,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   121,    -1,    -1,    -1,    -1,    -1,    -1,
     128,    -1,   130,   131,   132,   133,   134,   135,   136,   137,
     138,   139,    -1,    -1,    -1,    -1,   144,   145,   146,   147,
       0,   149,   150,   151,   152,    -1,   154,    -1,     8,     9,
      10,    -1,    -1,    13,    14,    15,    -1,    17,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    25,    -1,    27,    28,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,
      40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    68,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      -1,    -1,    -1,    -1,    94,    95,    96,    97,    98,    -1,
      -1,   101,    -1,    -1,    -1,    -1,    -1,   107,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   121,    -1,    -1,    -1,    -1,    -1,    -1,   128,    -1,
     130,   131,   132,   133,   134,   135,   136,   137,   138,   139,
      -1,    -1,    -1,    -1,    -1,   145,   146,   147,     0,   149,
     150,   151,   152,    -1,   154,    -1,     8,     9,    10,    -1,
      -1,    -1,    14,    15,    -1,    17,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    25,    26,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    -1,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    -1,
      -1,    -1,    -1,    94,    95,    -1,    68,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    -1,    -1,
      -1,    -1,    94,    95,    96,    97,    98,   128,   100,   130,
     131,   132,   133,   134,   135,   136,   137,   138,   139,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   121,
      -1,    -1,    -1,   154,    -1,   127,   128,    -1,   130,   131,
     132,   133,   134,   135,   136,   137,   138,   139,    -1,    -1,
      -1,    -1,   144,   145,   146,   147,     0,    -1,   150,    -1,
     152,    -1,   154,    -1,     8,     9,    10,    -1,    -1,    -1,
      14,    15,    -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    25,    26,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    37,    38,    -1,    40,    41,    42,    43,
      44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    68,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    -1,    -1,    -1,    -1,
      94,    95,    96,    97,    98,    -1,   100,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   121,    -1,    -1,
      -1,    -1,    -1,   127,   128,    -1,   130,   131,   132,   133,
     134,   135,   136,   137,   138,   139,    -1,    -1,    -1,    -1,
     144,   145,   146,   147,     0,    -1,   150,    -1,   152,    -1,
     154,    -1,     8,     9,    10,    -1,    -1,    -1,    14,    15,
      -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    37,    38,    -1,    40,    41,    42,    43,    44,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    68,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    -1,    -1,    -1,    -1,    94,    95,
      96,    97,    98,    -1,   100,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   121,    -1,    -1,    -1,    -1,
      -1,   127,   128,    -1,   130,   131,   132,   133,   134,   135,
     136,   137,   138,   139,    -1,    -1,    -1,    -1,    -1,   145,
     146,   147,     0,    -1,   150,    -1,   152,    -1,   154,    -1,
       8,     9,    10,    -1,    -1,    -1,    14,    15,    -1,    17,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,
      38,    -1,    40,    41,    42,    43,    44,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      68,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    -1,    -1,    -1,    -1,    94,    95,    96,    97,
      98,    -1,   100,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   121,    -1,    -1,    -1,    -1,    -1,   127,
     128,    -1,   130,   131,   132,   133,   134,   135,   136,   137,
     138,   139,    -1,    -1,    -1,    -1,    -1,   145,   146,   147,
      -1,    -1,   150,    -1,   152,     1,   154,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    -1,    -1,    15,
      16,    -1,    18,    19,    20,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    -1,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,
      76,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    90,    91,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    99,    -1,    -1,   102,   103,    -1,   105,
     106,    -1,   108,    -1,    -1,   111,   112,   113,   114,   115,
     116,   117,   118,   119,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   140,   141,   142,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,     1,   154,     3,
       4,     5,     6,     7,    -1,    -1,    10,    11,    12,    -1,
      14,    15,    16,    -1,    18,    19,    20,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    -1,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    75,    76,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    91,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,   102,   103,
      -1,   105,   106,    -1,   108,    -1,    -1,   111,   112,   113,
     114,   115,   116,   117,   118,   119,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   140,   141,   142,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,     1,
     154,     3,     4,     5,     6,     7,    -1,    -1,    10,    11,
      12,    -1,    -1,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    -1,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    75,    76,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    91,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,
     102,   103,    -1,   105,   106,    -1,   108,    -1,    -1,   111,
     112,   113,   114,   115,   116,   117,   118,   119,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   140,   141,
     142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,     1,   154,     3,     4,     5,     6,     7,    -1,    -1,
      10,    11,    12,    -1,    -1,    15,    16,    -1,    18,    19,
      20,    21,    22,    23,    24,    25,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    -1,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    75,    76,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,
      -1,    -1,   102,   103,    -1,   105,   106,    -1,   108,    -1,
      -1,   111,   112,   113,   114,   115,   116,   117,   118,   119,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     140,   141,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   152,     1,   154,     3,     4,     5,     6,     7,
      -1,    -1,    10,    11,    12,    -1,    -1,    15,    16,    -1,
      18,    19,    20,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    99,    -1,    -1,   102,   103,    -1,   105,   106,    -1,
     108,    -1,    -1,   111,   112,   113,   114,   115,   116,   117,
     118,   119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       1,    -1,     3,     4,     5,     6,     7,    -1,     9,    10,
      11,    12,   140,   141,   142,    16,    -1,    18,    19,    20,
      21,    22,    23,    24,   152,    -1,   154,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    -1,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    75,    76,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,
      -1,   102,   103,    -1,   105,   106,    -1,   108,    -1,    -1,
     111,   112,   113,   114,   115,   116,   117,   118,   119,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,     3,
       4,     5,     6,     7,    -1,    -1,    -1,    11,    12,   140,
     141,   142,    16,    -1,    18,    19,    20,    21,    22,    23,
      24,   152,    -1,   154,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    -1,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    75,    76,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    91,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,   102,   103,
      -1,   105,   106,    -1,   108,    -1,    -1,   111,   112,   113,
     114,   115,   116,   117,   118,   119,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   140,   141,   142,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   150,    -1,   152,     1,
     154,     3,     4,     5,     6,     7,    -1,    -1,    -1,    11,
      12,    -1,    -1,    -1,    16,    -1,    18,    19,    20,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    -1,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    75,    76,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    91,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,
     102,   103,    -1,   105,   106,    -1,   108,    -1,    -1,   111,
     112,   113,   114,   115,   116,   117,   118,   119,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   140,   141,
     142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   150,    -1,
     152,     1,   154,     3,     4,     5,     6,     7,    -1,    -1,
      -1,    11,    12,    -1,    -1,    -1,    16,    -1,    18,    19,
      20,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    -1,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    75,    76,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,
      -1,    -1,   102,   103,    -1,   105,   106,    -1,   108,    -1,
      -1,   111,   112,   113,   114,   115,   116,   117,   118,   119,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     140,   141,   142,    -1,    -1,   145,    -1,    -1,    -1,    -1,
      -1,    -1,   152,     1,   154,     3,     4,     5,     6,     7,
      -1,    -1,    -1,    11,    12,    -1,    -1,    -1,    16,    -1,
      18,    19,    20,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    99,    -1,    -1,   102,   103,    -1,   105,   106,    -1,
     108,    -1,    -1,   111,   112,   113,   114,   115,   116,   117,
     118,   119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   140,   141,   142,    -1,    -1,   145,    -1,    -1,
      -1,    -1,    -1,    -1,   152,     1,   154,     3,     4,     5,
       6,     7,    -1,    -1,    10,    11,    12,    -1,    -1,    -1,
      16,    -1,    18,    19,    20,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    -1,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,
      76,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    90,    91,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    99,    -1,    -1,   102,   103,    -1,   105,
     106,    -1,   108,    -1,    -1,   111,   112,   113,   114,   115,
     116,   117,   118,   119,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     1,    -1,     3,     4,     5,     6,     7,    -1,
      -1,    -1,    11,    12,   140,   141,   142,    16,    -1,    18,
      19,    20,    21,    22,    23,    24,   152,    -1,   154,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    -1,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    75,    76,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      99,    -1,    -1,   102,   103,    -1,   105,   106,    -1,   108,
      -1,    -1,   111,   112,   113,   114,   115,   116,   117,   118,
     119,    -1,   121,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,    11,
      12,   140,   141,   142,    16,    -1,    18,    19,    20,    21,
      22,    23,    24,   152,    -1,   154,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    75,    76,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    91,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,
     102,   103,    -1,   105,   106,    -1,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,    -1,     7,    -1,    -1,    -1,    11,    12,   140,   141,
     142,    16,    -1,    18,    19,    20,    21,    22,    23,    24,
      -1,    -1,   154,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    46,    -1,    -1,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      75,    76,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    91,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    99,    -1,    -1,   102,   103,    -1,
     105,   106,    -1,    -1,    -1,    -1,   111,   112,   113,   114,
     115,   116,   117,   118,   119,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,    -1,     7,
      -1,    -1,    -1,    11,    12,   140,   141,   142,    16,    -1,
      18,    19,    20,    21,    22,    23,    24,   152,    -1,   154,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,
      -1,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    99,    -1,    -1,   102,   103,    -1,   105,   106,    -1,
      -1,    -1,    -1,   111,   112,   113,   114,   115,   116,   117,
     118,   119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,     6,     7,    -1,    -1,    -1,
      11,    12,   140,   141,   142,    16,    -1,    18,    19,    20,
      21,    22,    23,    24,    -1,    -1,   154,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    -1,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    75,    76,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,
      -1,   102,   103,    -1,   105,   106,    -1,   108,    -1,    -1,
     111,   112,   113,   114,   115,   116,   117,   118,   119,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,     7,    -1,    -1,    -1,    11,    12,   140,
     141,   142,    16,    -1,    18,    19,    20,    21,    22,    23,
      24,   152,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    -1,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    75,    76,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    91,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,   102,   103,
      -1,   105,   106,    -1,   108,    -1,    -1,   111,   112,   113,
     114,   115,   116,   117,   118,   119,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   140,   141,   142,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    -1,    -1,    -1,    -1,
      -1,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      -1,    -1,    86,    87,    -1,    -1,    -1,    -1,    92,    93,
      94,    95,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   108,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   130,   131,   132,   133,
     134,   135,   136,   137,   138,   139,    -1,   141,   142,    -1,
      -1,    -1,    -1,    -1,   148,   149,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    -1,    -1,    86,
      87,    -1,    -1,    -1,    -1,    92,    93,    94,    95,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   130,   131,   132,   133,   134,   135,   136,
     137,   138,   139,    -1,   141,   142,    -1,    -1,    -1,    -1,
      -1,   148,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    -1,
      -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    -1,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    -1,    -1,    86,    87,    -1,    -1,    -1,
      -1,    92,    93,    94,    95,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,   109,    -1,
      -1,   112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   130,
     131,   132,   133,   134,   135,   136,   137,   138,   139,    -1,
     141,   142,    -1,    -1,    -1,    -1,    -1,   148,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    -1,    -1,    -1,    -1,    -1,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    -1,
      -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    -1,
      -1,    86,    87,    -1,    -1,    -1,    -1,    92,    93,    94,
      95,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   108,   109,    -1,    -1,   112,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   130,   131,   132,   133,   134,
     135,   136,   137,   138,   139,    -1,   141,   142,    -1,    -1,
      -1,    -1,    -1,   148,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    -1,    -1,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    -1,    -1,    86,    87,    -1,
      -1,    -1,    -1,    92,    93,    94,    95,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   130,   131,   132,   133,   134,   135,   136,   137,   138,
     139,    -1,   141,   142,     3,     4,     5,    -1,     7,   148,
      -1,    -1,    11,    12,    -1,    -1,    -1,    16,    -1,    18,
      19,    20,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    -1,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      99,    -1,    -1,   102,   103,    -1,   105,   106,    -1,    -1,
      -1,    -1,   111,   112,   113,   114,   115,   116,   117,   118,
     119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,    -1,     7,    -1,
      -1,   140,    11,    12,    -1,    -1,    -1,    16,   147,    18,
      19,    20,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    -1,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      99,    -1,    -1,   102,   103,    -1,   105,   106,    -1,    -1,
      -1,    -1,   111,   112,   113,   114,   115,   116,   117,   118,
     119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,    -1,
      -1,   140,    11,    12,    -1,    -1,    -1,    16,   147,    18,
      19,    20,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    -1,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    75,    76,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      99,    -1,    -1,   102,   103,    -1,   105,   106,    -1,   108,
      -1,    -1,   111,   112,   113,   114,   115,   116,   117,   118,
     119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,    11,
      12,   140,   141,   142,    16,    -1,    18,    19,    20,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    75,    76,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    91,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,
     102,   103,    -1,   105,   106,    -1,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,    -1,    -1,    -1,    11,    12,   140,   141,
     142,    16,    -1,    18,    19,    20,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      45,    46,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      75,    76,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    91,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    99,    -1,    -1,   102,   103,    -1,
     105,   106,    -1,   108,    -1,    -1,   111,   112,   113,   114,
     115,   116,   117,   118,   119,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,    -1,     7,
      -1,    -1,    -1,    11,    12,   140,   141,   142,    16,    -1,
      18,    19,    20,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,
      -1,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    99,    -1,    -1,   102,   103,    -1,   105,   106,    -1,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,
      11,    12,   140,   141,   142,    16,    -1,    18,    19,    20,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    75,    76,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,
      -1,   102,   103,    -1,   105,   106,    -1,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,    -1,     7,    -1,    -1,    -1,    11,    12,   140,
     141,   142,    16,    -1,    18,    19,    20,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    46,    -1,    -1,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    75,    76,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    91,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,   102,   103,
      -1,   105,   106,    -1,   108,   109,    -1,   111,   112,   113,
     114,   115,   116,   117,   118,   119,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,    -1,
       7,    -1,    -1,    -1,    11,    12,   140,   141,   142,    16,
      -1,    18,    19,    20,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    46,
      -1,    -1,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    91,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    99,    -1,    -1,   102,   103,    -1,   105,   106,
      -1,    -1,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,    -1,     7,    -1,    -1,
      -1,    11,    12,   140,   141,   142,    16,    -1,    18,    19,
      20,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    75,    76,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,
      -1,    -1,   102,   103,    -1,   105,   106,    -1,   108,   109,
      -1,   111,   112,   113,   114,   115,   116,   117,   118,   119,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,    -1,     7,    -1,    -1,    -1,    11,    12,
     140,   141,   142,    16,    -1,    18,    19,    20,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    46,    -1,    -1,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    75,    76,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    91,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,   102,
     103,    -1,   105,   106,    -1,    -1,   109,    -1,   111,   112,
     113,   114,   115,   116,   117,   118,   119,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,
      -1,     7,    -1,    -1,    -1,    11,    12,   140,   141,   142,
      16,    -1,    18,    19,    20,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      46,    -1,    -1,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    -1,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,
      76,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    90,    91,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    99,    -1,    -1,   102,   103,    -1,   105,
     106,    -1,   108,    -1,    -1,   111,   112,   113,   114,   115,
     116,   117,   118,   119,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,    -1,     7,    -1,
      -1,    -1,    11,    12,   140,   141,   142,    16,    -1,    18,
      19,    20,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    -1,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    75,    76,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      99,    -1,    -1,   102,   103,    -1,   105,   106,    -1,   108,
      -1,    -1,   111,   112,   113,   114,   115,   116,   117,   118,
     119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,    11,
      12,   140,   141,   142,    16,    -1,    18,    19,    20,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    -1,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    75,    76,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    91,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,
     102,   103,    -1,   105,   106,    -1,   108,    -1,    -1,   111,
     112,   113,   114,   115,   116,   117,   118,   119,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,    -1,     7,    -1,    -1,    -1,    11,    12,   140,   141,
     142,    16,    -1,    18,    19,    20,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    46,    -1,    -1,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      75,    76,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    91,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    99,    -1,    -1,   102,   103,    -1,
     105,   106,    -1,   108,    -1,    -1,   111,   112,   113,   114,
     115,   116,   117,   118,   119,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,    -1,     7,
      -1,    -1,    -1,    11,    12,   140,   141,   142,    16,    -1,
      18,    19,    20,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,
      -1,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    99,    -1,    -1,   102,   103,    -1,   105,   106,    -1,
     108,    -1,    -1,   111,   112,   113,   114,   115,   116,   117,
     118,   119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,
      11,    12,   140,   141,   142,    16,    -1,    18,    19,    20,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    -1,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    75,    76,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,
      -1,   102,   103,    -1,   105,   106,    -1,    -1,    -1,    -1,
     111,   112,   113,   114,   115,   116,   117,   118,   119,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,    -1,     7,    -1,    -1,    -1,    11,    12,   140,
     141,   142,    16,    -1,    18,    19,    20,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    46,    -1,    -1,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    -1,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    75,    76,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    91,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,   102,   103,
      -1,   105,   106,    -1,    -1,    -1,    -1,   111,   112,   113,
     114,   115,   116,   117,   118,   119,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,    -1,
       7,    -1,    -1,    -1,    11,    12,   140,   141,   142,    16,
      -1,    18,    19,    20,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    46,
      -1,    -1,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    -1,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    91,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    99,    -1,    -1,   102,   103,    -1,   105,   106,
      -1,    -1,    -1,    -1,   111,   112,   113,   114,   115,   116,
     117,   118,   119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,    -1,     7,    -1,    -1,
      -1,    11,    12,   140,   141,   142,    16,    -1,    18,    19,
      20,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    -1,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,
      -1,    -1,   102,   103,    -1,   105,   106,    -1,   108,    -1,
      -1,   111,   112,   113,   114,   115,   116,   117,   118,   119,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,    -1,
       7,    -1,    -1,    -1,    11,    12,    -1,    -1,    -1,    16,
     140,    18,    19,    20,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    46,
      -1,    -1,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    -1,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    99,    -1,    -1,   102,   103,    -1,   105,   106,
      -1,   108,    -1,    -1,   111,   112,   113,   114,   115,   116,
     117,   118,   119,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,    -1,     7,    -1,    -1,    -1,    11,    12,    -1,
      -1,    -1,    16,   140,    18,    19,    20,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    46,    -1,    -1,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    -1,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      94,    -1,    -1,    -1,    -1,    99,    -1,    -1,   102,   103,
      -1,   105,   106,    -1,    -1,    -1,    -1,   111,   112,   113,
     114,   115,   116,   117,   118,   119,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,
      11,    12,    -1,    -1,    -1,    16,   140,    18,    19,    20,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    -1,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,
      -1,   102,   103,    -1,   105,   106,    -1,    -1,    -1,    -1,
     111,   112,   113,   114,   115,   116,   117,   118,   119,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,    -1,     7,
      -1,    -1,    -1,    11,    12,    -1,    -1,    -1,    16,   140,
      18,    19,    20,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,
      -1,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    99,    -1,    -1,   102,   103,    -1,   105,   106,    -1,
      -1,    -1,    -1,   111,   112,   113,   114,   115,   116,   117,
     118,   119,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,    -1,     7,    -1,    -1,    -1,    11,    12,    -1,    -1,
      -1,    16,   140,    18,    19,    20,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    46,    -1,    -1,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    99,    -1,    -1,   102,   103,    -1,
     105,   106,    -1,    -1,    -1,    -1,   111,   112,   113,   114,
     115,   116,   117,   118,   119,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,    11,
      12,    -1,    -1,    -1,    16,   140,    18,    19,    20,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    -1,    59,    60,    61,
      62,    63,    64,    65,    33,    34,    35,    36,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      49,    50,    51,    52,    -1,    -1,    -1,    56,    -1,    58,
      59,    60,    61,    62,    63,    -1,    -1,    99,    -1,    -1,
     102,   103,    -1,   105,   106,    -1,    -1,    -1,    77,   111,
     112,   113,   114,   115,   116,   117,   118,   119,    -1,    -1,
      -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      99,    -1,    -1,   102,    -1,    -1,   105,   106,   140,   108,
     109,    -1,   111,   112,   113,   114,   115,   116,   117,   118,
     119,    33,    34,    35,    36,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   133,    -1,    -1,    49,    50,    51,
      52,   140,    -1,    -1,    56,    -1,    -1,    59,    60,    61,
      62,    63,    -1,    -1,    33,    34,    35,    36,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      49,    50,    51,    52,    -1,    -1,    -1,    56,    90,    91,
      59,    60,    61,    62,    63,    -1,    -1,    99,    -1,    -1,
     102,    -1,    -1,   105,   106,    -1,   108,    -1,    -1,   111,
     112,   113,   114,   115,   116,   117,   118,   119,    -1,    -1,
      -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      99,   133,    -1,   102,    -1,    -1,   105,   106,   140,   108,
      -1,    -1,   111,   112,   113,   114,   115,   116,   117,   118,
     119,    33,    34,    35,    36,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   133,    -1,    -1,    49,    50,    51,
      52,   140,    -1,    -1,    56,    -1,    -1,    59,    60,    61,
      62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    91,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,
     102,    -1,    -1,   105,   106,    -1,    -1,    -1,    -1,   111,
     112,   113,   114,   115,   116,   117,   118,   119,    52,    53,
      -1,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   133,    -1,    -1,    -1,    -1,    -1,    -1,   140,    -1,
      -1,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      -1,    -1,    86,    87,    -1,    -1,    -1,    -1,    92,    93,
      94,    95,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   108,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   130,   131,   132,   133,
     134,   135,   136,   137,   138,   139,    -1,   141,   142,    52,
      53,    -1,    -1,    56,   148,   149,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    -1,    -1,    86,    87,    -1,    -1,    -1,    -1,    92,
      93,    94,    95,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   108,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   130,   131,   132,
     133,   134,   135,   136,   137,   138,   139,    -1,   141,   142,
      52,    53,    -1,    -1,    56,   148,   149,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    86,    87,    -1,    -1,    -1,    -1,
      92,    93,    94,    95,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   108,   109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   130,   131,
     132,   133,   134,   135,   136,   137,   138,   139,    -1,   141,
     142,    52,    53,    -1,    -1,    56,   148,   149,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    -1,    -1,    86,    87,    -1,    -1,    -1,
      -1,    92,    93,    94,    95,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   130,
     131,   132,   133,   134,   135,   136,   137,   138,   139,    -1,
     141,   142,    52,    53,    -1,    -1,    56,   148,   149,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    -1,    -1,    86,    87,    -1,    -1,
      -1,    -1,    92,    93,    94,    95,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     130,   131,   132,   133,   134,   135,   136,   137,   138,   139,
      -1,   141,   142,    52,    53,    -1,    -1,    56,   148,   149,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    -1,    -1,    86,    87,    -1,
      -1,    -1,    -1,    92,    93,    94,    95,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   130,   131,   132,   133,   134,   135,   136,   137,   138,
     139,    -1,   141,   142,    52,    53,    -1,    -1,    56,   148,
     149,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    -1,    -1,    86,    87,
      -1,    -1,    -1,    -1,    92,    93,    94,    95,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   130,   131,   132,   133,   134,   135,   136,   137,
     138,   139,    -1,   141,   142,    52,    53,    -1,    -1,    56,
     148,   149,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    -1,    -1,    86,
      87,    -1,    -1,    -1,    -1,    92,    93,    94,    95,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   130,   131,   132,   133,   134,   135,   136,
     137,   138,   139,    -1,   141,   142,    52,    53,    -1,    -1,
      56,   148,   149,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    -1,    -1,
      86,    87,    -1,    -1,    -1,    -1,    92,    93,    94,    95,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   130,   131,   132,   133,   134,   135,
     136,   137,   138,   139,    -1,   141,   142,    52,    53,    -1,
      -1,    56,   148,   149,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    -1,
      -1,    86,    87,    -1,    -1,    -1,    -1,    92,    93,    94,
      95,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   108,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   130,   131,   132,   133,   134,
     135,   136,   137,   138,   139,    -1,   141,   142,    52,    53,
      -1,    -1,    56,   148,   149,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      -1,    -1,    86,    87,    -1,    -1,    -1,    -1,    92,    93,
      94,    95,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   108,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   130,   131,   132,   133,
     134,   135,   136,   137,   138,   139,    -1,   141,   142,    52,
      53,    -1,    -1,    56,   148,   149,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    -1,    -1,    86,    87,    -1,    -1,    -1,    -1,    92,
      93,    94,    95,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   108,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   130,   131,   132,
     133,   134,   135,   136,   137,   138,   139,    -1,   141,   142,
      52,    53,    -1,    -1,    56,   148,   149,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    86,    87,    -1,    -1,    -1,    -1,
      92,    93,    94,    95,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   108,   109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   130,   131,
     132,   133,   134,   135,   136,   137,   138,   139,    -1,   141,
     142,    52,    53,    -1,    -1,    56,   148,   149,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    -1,    -1,    86,    87,    -1,    -1,    -1,
      -1,    92,    93,    94,    95,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   130,
     131,   132,   133,   134,   135,   136,   137,   138,   139,    -1,
     141,   142,    52,    53,    -1,    -1,    56,   148,   149,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    -1,    -1,    86,    87,    -1,    -1,
      -1,    -1,    92,    93,    94,    95,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     130,   131,   132,   133,   134,   135,   136,   137,   138,   139,
      -1,   141,   142,    -1,    -1,    -1,    -1,    -1,   148,   149
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint16 yystos[] =
{
       0,   156,   157,     0,     1,     3,     4,     5,     6,     7,
      11,    12,    16,    18,    19,    20,    21,    22,    23,    24,
      30,    31,    32,    33,    34,    35,    36,    39,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    59,    60,    61,    62,    63,    64,    65,    75,    76,
      90,    91,    99,   102,   103,   105,   106,   108,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   140,   141,   142,
     158,   159,   160,   168,   170,   172,   178,   179,   181,   182,
     183,   185,   186,   187,   189,   190,   199,   202,   217,   232,
     233,   234,   235,   236,   237,   238,   239,   240,   241,   242,
     251,   278,   279,   322,   323,   324,   325,   326,   327,   328,
     331,   333,   334,   348,   349,   351,   352,   353,   354,   355,
     356,   357,   358,   394,   407,   160,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    56,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    86,
      87,    92,    93,    94,    95,   108,   109,   130,   131,   132,
     133,   134,   135,   136,   137,   138,   139,   141,   142,   148,
     193,   194,   195,   197,   198,   348,    39,    58,    99,   102,
     108,   109,   110,   113,   141,   182,   190,   199,   203,   209,
     212,   214,   232,   354,   355,   357,   358,   392,   393,   209,
     149,   210,   211,   149,   206,   210,   149,   154,   401,    54,
     194,   401,   144,   161,   144,    21,    22,    31,    32,   181,
     199,   232,   251,   199,   199,   199,    56,     1,    47,   102,
     164,   165,   166,   168,   184,   185,   407,   168,   219,   204,
     214,   392,   407,   203,   391,   392,   407,    46,    99,   140,
     147,   189,   217,   232,   354,   355,   358,   222,    54,    55,
      57,   193,   337,   350,   337,   338,   339,   153,   153,   153,
     153,   353,   178,   199,   199,   152,   154,   400,   405,   406,
      40,    41,    42,    43,    44,    37,    38,    26,   144,   206,
     210,   243,   280,    28,   244,   277,   127,   147,   102,   108,
     186,   127,    25,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    94,    95,   128,   130,
     131,   132,   133,   134,   135,   136,   137,   138,   139,   201,
     201,    68,    96,    97,    98,   146,   398,   218,   172,   174,
     174,   175,   176,   175,   174,   400,   406,    99,   183,   190,
     232,   256,   354,   355,   358,    52,    56,    94,    99,   191,
     192,   232,   354,   355,   358,   192,    33,    34,    35,    36,
      49,    50,    51,    52,    56,   149,   193,   356,   389,   209,
      97,   398,   399,   280,   325,   100,   100,   147,   203,    56,
     203,   203,   203,   337,   127,   101,   147,   213,   407,    97,
     146,   398,   100,   100,   147,   213,   209,   401,   402,   209,
      91,   208,   209,   214,   366,   392,   407,   172,   402,   172,
      54,    64,    65,   169,   149,   200,   158,   164,    97,   398,
     100,   168,   167,   184,   150,   400,   406,   402,   220,   402,
     151,   147,   154,   404,   147,   404,   145,   404,   401,    56,
     353,   186,   188,   147,    97,   146,   398,   269,   270,    66,
     120,   122,   123,   340,   120,   120,   340,    67,   340,   329,
     335,   332,   336,    77,   152,   160,   174,   174,   174,   174,
     168,   172,   172,   281,   282,   107,   180,   285,   286,   285,
     108,   178,   203,   214,   215,   216,   184,   147,   189,   147,
     170,   171,   178,   190,   199,   203,   205,   216,   232,   358,
     173,   199,   199,   199,   199,   199,   199,   199,   199,   199,
     199,   199,   199,   199,   199,   199,   199,   199,   199,   199,
     199,   199,   199,   199,   199,    52,    53,    56,   197,   206,
     395,   396,    52,    53,    56,   197,   395,   208,    52,    56,
     206,   395,   162,   164,    13,   252,   405,   252,   164,   174,
     164,   400,   224,    56,    97,   146,   398,    25,   172,    52,
      56,   191,   131,   359,    97,   146,   398,   227,   390,   228,
      68,    97,   397,   395,   171,   199,   205,   171,   205,   196,
     125,   203,   108,   203,   212,   392,    52,    56,   208,    52,
      56,   393,   402,   150,   402,   147,   402,   147,   402,   194,
     221,   199,   145,   145,   395,   395,   205,   161,   402,   166,
     402,   392,   147,   188,    52,    56,   208,    52,    56,   271,
     342,   341,   120,   330,   340,    66,   120,   120,   330,    66,
     120,   199,   145,   283,   281,    10,   250,   287,   250,   203,
     147,    44,   402,   188,   147,    44,   127,    44,    97,   146,
     398,    52,    56,    58,    77,    90,    91,    99,   102,   105,
     106,   108,   109,   111,   113,   133,   297,   298,   299,   300,
     301,   304,   307,   308,   309,   310,   311,   312,   313,   315,
     316,   317,   322,   323,   326,   327,   328,   331,   333,   334,
     355,   377,   401,   100,   100,   206,   210,   401,   403,   100,
     100,   206,   207,   210,   407,   250,     8,   245,   318,   407,
     164,    13,   164,   250,    27,   253,   405,   250,    25,   223,
     292,    17,   247,   290,    52,    56,   208,    52,    56,   175,
     226,   360,   225,    52,    56,   191,   208,   162,   172,   229,
     230,   207,   210,   194,   203,   203,   213,   100,   100,   403,
     100,   100,   392,   172,   404,   186,   403,   272,   343,    54,
      55,    57,   347,   358,   153,   340,   153,   153,   153,   284,
     145,   288,   108,   203,   168,   188,   168,   199,    52,    56,
     208,    52,    56,   298,   313,   313,    56,   191,   298,   298,
     302,   303,   304,   306,   403,   113,   145,   307,    52,   147,
     314,   337,    52,   101,   147,   132,   147,    88,    89,    97,
     146,   149,    34,    52,   129,   171,   205,   171,   205,   180,
     151,   100,   171,   205,   171,   205,   180,   203,   216,   319,
     407,     9,    15,   246,   248,   321,   407,    14,   248,   249,
     254,   255,   407,   255,   177,   293,   290,   250,   108,   203,
     289,   250,   403,   164,   405,   174,   162,   403,   250,   402,
     149,   361,   362,   193,   280,   277,   100,   147,   402,   273,
     344,    85,   132,   264,   265,   407,   264,   203,   403,   402,
     403,   108,   298,   306,   147,   337,   145,   147,   305,   306,
     269,   125,   315,   302,   301,   309,   310,   311,   313,   313,
     191,   302,   307,   403,   302,   307,   402,   199,   171,   205,
     101,   320,   407,   164,   163,   164,   174,   250,   250,   296,
     297,   250,   203,   147,   252,   250,   162,   405,   250,    52,
      54,    55,    56,    57,    58,    91,   102,   108,   110,   134,
     137,   363,   365,   366,   367,   368,   369,   370,   371,   372,
     373,   376,   377,   378,   379,   380,   383,   384,   385,   386,
     387,   162,   365,   231,   149,   275,   365,   345,   261,   263,
     266,   369,   371,   372,   374,   375,   378,   379,   381,   382,
     385,   387,   401,   164,   162,    52,   147,   147,   305,   147,
     298,   403,   403,   402,   402,    99,   190,   232,   354,   355,
     358,   252,   164,   252,   252,    40,    41,   108,   203,   164,
     250,   102,   108,   257,   258,   259,   260,   368,   402,   127,
     147,   364,   203,   147,   388,   407,    34,    52,   147,   388,
     388,   147,   364,    52,   147,   364,    52,   250,   405,   361,
     365,   274,   346,   266,   132,   127,   147,   262,    99,   232,
     147,   388,   388,   388,   147,   262,   147,   262,   152,   147,
     305,   306,    56,    97,   146,   398,   164,   321,   164,   294,
     174,   174,   203,   255,   290,   291,   259,   368,   147,   402,
     147,   203,   363,   370,   383,   385,   373,   377,   379,   387,
     371,   380,   385,   369,   371,   162,   266,    29,   124,   276,
     164,   132,   232,   261,   382,   385,    56,    97,   374,   379,
     371,   381,   385,   371,    52,   267,   268,   367,   305,    52,
      56,   208,    52,    56,   318,   254,   164,   402,   257,   260,
     258,   147,   364,   147,   364,   388,   147,   364,   147,   364,
     364,   250,   150,   162,   164,   121,   147,   262,   147,   262,
      52,    56,   388,   147,   262,   147,   262,   262,   147,   401,
     403,   255,   292,   295,   147,   147,   371,   385,   371,   371,
     250,   145,   371,   385,   371,   371,   268,   258,   364,   147,
     364,   364,   364,   262,   147,   262,   262,   262,   371,   371,
     364,   262
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint16 yyr1[] =
{
       0,   155,   157,   156,   158,   159,   159,   159,   159,   160,
     160,   161,   163,   162,   162,   164,   165,   165,   165,   165,
     166,   167,   166,   169,   168,   168,   168,   168,   168,   168,
     168,   168,   168,   168,   168,   168,   168,   168,   168,   168,
     168,   170,   170,   170,   170,   170,   170,   170,   170,   171,
     171,   171,   172,   172,   172,   172,   172,   173,   172,   172,
     174,   176,   177,   175,   178,   178,   179,   179,   180,   181,
     182,   182,   182,   182,   182,   182,   182,   182,   182,   182,
     182,   183,   183,   184,   184,   185,   185,   185,   185,   185,
     185,   185,   185,   185,   185,   186,   186,   187,   187,   188,
     188,   189,   189,   189,   189,   189,   189,   189,   189,   189,
     190,   190,   190,   190,   190,   190,   190,   190,   190,   191,
     191,   192,   192,   192,   193,   193,   193,   193,   193,   194,
     194,   195,   196,   195,   197,   197,   197,   197,   197,   197,
     197,   197,   197,   197,   197,   197,   197,   197,   197,   197,
     197,   197,   197,   197,   197,   197,   197,   197,   197,   197,
     197,   197,   197,   197,   198,   198,   198,   198,   198,   198,
     198,   198,   198,   198,   198,   198,   198,   198,   198,   198,
     198,   198,   198,   198,   198,   198,   198,   198,   198,   198,
     198,   198,   198,   198,   198,   198,   198,   198,   198,   198,
     198,   198,   198,   198,   198,   199,   199,   199,   199,   199,
     199,   199,   199,   199,   199,   199,   199,   199,   199,   199,
     199,   199,   199,   199,   199,   199,   199,   199,   199,   199,
     199,   199,   199,   199,   199,   199,   199,   199,   199,   199,
     199,   199,   199,   199,   199,   200,   199,   199,   199,   201,
     201,   201,   201,   202,   202,   203,   204,   204,   204,   204,
     205,   205,   206,   206,   207,   207,   208,   208,   208,   208,
     208,   209,   209,   209,   209,   209,   211,   210,   212,   213,
     213,   214,   214,   214,   214,   215,   215,   216,   216,   216,
     217,   217,   217,   217,   217,   217,   217,   217,   217,   217,
     217,   218,   217,   219,   217,   220,   217,   217,   217,   217,
     217,   217,   217,   217,   217,   217,   221,   217,   217,   217,
     217,   217,   217,   222,   217,   217,   217,   217,   217,   223,
     217,   224,   217,   217,   217,   225,   217,   226,   217,   227,
     217,   228,   229,   217,   230,   231,   217,   217,   217,   217,
     217,   217,   232,   233,   234,   235,   236,   237,   238,   239,
     240,   241,   242,   243,   244,   245,   246,   247,   248,   249,
     250,   251,   252,   252,   252,   253,   253,   254,   254,   255,
     255,   256,   256,   257,   257,   258,   258,   259,   259,   259,
     259,   259,   260,   260,   261,   261,   261,   261,   261,   262,
     262,   263,   263,   263,   263,   263,   263,   263,   263,   263,
     263,   263,   263,   263,   263,   263,   264,   264,   265,   265,
     265,   266,   266,   267,   267,   268,   268,   270,   271,   272,
     273,   274,   269,   275,   275,   276,   276,   277,   278,   278,
     278,   278,   279,   279,   279,   279,   279,   279,   279,   279,
     279,   280,   280,   282,   283,   284,   281,   286,   287,   288,
     285,   289,   289,   289,   289,   290,   291,   291,   293,   294,
     292,   295,   295,   296,   296,   296,   297,   297,   297,   297,
     297,   298,   299,   299,   300,   300,   301,   301,   301,   301,
     301,   301,   301,   301,   301,   301,   301,   301,   302,   302,
     302,   302,   302,   302,   302,   302,   303,   303,   304,   304,
     304,   304,   305,   305,   306,   307,   307,   307,   307,   307,
     308,   308,   309,   309,   309,   309,   310,   310,   311,   312,
     312,   312,   312,   312,   312,   312,   312,   312,   312,   313,
     313,   313,   313,   313,   313,   313,   313,   313,   314,   313,
     315,   316,   317,   317,   317,   318,   318,   319,   319,   319,
     320,   320,   321,   321,   322,   322,   323,   324,   324,   324,
     325,   326,   327,   328,   329,   329,   330,   330,   331,   332,
     332,   333,   334,   335,   335,   336,   336,   337,   337,   338,
     338,   339,   339,   340,   341,   340,   342,   343,   344,   345,
     346,   340,   347,   347,   347,   347,   348,   348,   349,   350,
     350,   350,   350,   351,   352,   352,   353,   353,   353,   353,
     354,   354,   354,   354,   354,   355,   355,   355,   355,   355,
     355,   355,   356,   356,   357,   357,   358,   358,   360,   359,
     359,   361,   362,   361,   363,   363,   363,   363,   363,   364,
     364,   365,   365,   365,   365,   365,   365,   365,   365,   365,
     365,   365,   365,   365,   365,   365,   365,   366,   367,   367,
     367,   367,   368,   368,   369,   370,   370,   371,   371,   372,
     373,   373,   374,   374,   375,   375,   376,   376,   377,   377,
     378,   379,   379,   380,   381,   382,   382,   383,   383,   384,
     384,   385,   385,   386,   386,   387,   388,   388,   389,   390,
     389,   391,   391,   392,   392,   393,   393,   393,   393,   394,
     394,   394,   395,   395,   395,   395,   396,   396,   396,   397,
     397,   398,   398,   399,   399,   400,   400,   401,   401,   402,
     403,   404,   404,   404,   405,   405,   406,   406,   407
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     2,     1,     1,     3,     2,     1,
       2,     3,     0,     6,     3,     2,     1,     1,     3,     2,
       1,     0,     3,     0,     4,     3,     3,     3,     2,     3,
       3,     3,     3,     3,     4,     1,     3,     3,     5,     3,
       1,     3,     3,     6,     5,     5,     5,     5,     3,     1,
       3,     1,     1,     3,     3,     3,     2,     0,     4,     1,
       1,     0,     0,     4,     1,     1,     1,     4,     3,     1,
       2,     3,     4,     5,     4,     5,     2,     2,     2,     2,
       2,     1,     3,     1,     3,     1,     2,     3,     5,     2,
       4,     2,     4,     1,     3,     1,     3,     2,     3,     1,
       3,     1,     1,     4,     3,     3,     3,     3,     2,     1,
       1,     1,     4,     3,     3,     3,     3,     2,     1,     1,
       1,     2,     1,     3,     1,     1,     1,     1,     1,     1,
       1,     1,     0,     4,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     3,     3,     6,     5,     5,
       5,     5,     4,     3,     3,     3,     2,     2,     2,     2,
       3,     3,     3,     3,     3,     3,     4,     2,     2,     3,
       3,     3,     3,     1,     3,     3,     3,     3,     3,     2,
       2,     3,     3,     3,     3,     0,     4,     6,     1,     1,
       1,     1,     1,     3,     3,     1,     1,     2,     4,     2,
       1,     3,     3,     3,     1,     1,     1,     1,     2,     4,
       2,     1,     2,     2,     4,     1,     0,     2,     2,     2,
       1,     1,     2,     3,     4,     1,     1,     3,     4,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     0,     4,     0,     3,     0,     4,     3,     3,     2,
       3,     3,     1,     4,     3,     1,     0,     6,     4,     3,
       2,     1,     2,     0,     3,     6,     6,     4,     4,     0,
       6,     0,     5,     5,     6,     0,     6,     0,     7,     0,
       5,     0,     0,     7,     0,     0,     9,     1,     1,     1,
       1,     3,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     2,     1,     1,     1,     5,     1,
       2,     1,     1,     1,     3,     1,     3,     1,     3,     5,
       1,     3,     2,     1,     4,     2,     2,     2,     1,     2,
       0,     6,     8,     4,     6,     4,     2,     6,     2,     4,
       6,     2,     4,     2,     4,     1,     1,     1,     3,     1,
       4,     1,     4,     1,     3,     1,     1,     0,     0,     0,
       0,     0,     7,     4,     1,     3,     3,     3,     2,     4,
       5,     5,     2,     4,     4,     3,     3,     3,     2,     1,
       4,     3,     3,     0,     0,     0,     5,     0,     0,     0,
       5,     1,     2,     3,     4,     5,     1,     1,     0,     0,
       7,     1,     1,     1,     3,     3,     1,     2,     3,     1,
       1,     1,     3,     1,     3,     1,     1,     4,     4,     3,
       4,     4,     3,     3,     2,     3,     2,     3,     1,     1,
       2,     3,     5,     2,     4,     1,     2,     3,     2,     4,
       1,     3,     1,     3,     1,     3,     1,     1,     3,     1,
       1,     3,     2,     1,     4,     3,     2,     1,     2,     1,
       3,     3,     2,     2,     1,     1,     1,     2,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     0,     3,
       1,     2,     2,     3,     1,     6,     1,     1,     1,     1,
       2,     1,     2,     1,     1,     1,     1,     1,     1,     2,
       3,     3,     3,     4,     0,     3,     1,     2,     4,     0,
       3,     4,     4,     0,     3,     0,     3,     0,     2,     0,
       2,     0,     2,     1,     0,     3,     0,     0,     0,     0,
       0,     8,     1,     1,     1,     1,     1,     1,     2,     1,
       1,     1,     1,     3,     1,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     0,     4,
       0,     3,     0,     3,     4,     2,     2,     2,     1,     2,
       0,     6,     8,     4,     6,     4,     6,     2,     4,     6,
       2,     4,     2,     4,     1,     1,     0,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     3,     1,     3,     1,
       2,     1,     2,     1,     1,     3,     1,     3,     1,     1,
       2,     2,     1,     3,     3,     1,     3,     1,     3,     1,
       1,     2,     1,     1,     1,     2,     2,     1,     1,     0,
       4,     1,     2,     1,     3,     3,     2,     4,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     0,     1,     0,     1,     2,
       2,     0,     1,     1,     1,     1,     1,     2,     0
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
#line 958 "ripper.y" /* yacc.c:1429  */
{
    RUBY_SET_YYLLOC_OF_NONE(yylloc);
}

#line 5735 "ripper.c" /* yacc.c:1429  */
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
#line 1160 "ripper.y" /* yacc.c:1646  */
    {
			SET_LEX_STATE(EXPR_BEG);
			local_push(p, ifndef_ripper(1)+0);
		    }
#line 5927 "ripper.c" /* yacc.c:1646  */
    break;

  case 3:
#line 1165 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			if ((yyvsp[0].val) && !compile_for_eval) {
			    NODE *node = (yyvsp[0].val);
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
			p->eval_tree = NEW_SCOPE(0, block_append(p, p->eval_tree, (yyvsp[0].val)), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(program,v1);p->result=v2;}
			local_pop(p);
		    }
#line 5951 "ripper.c" /* yacc.c:1646  */
    break;

  case 4:
#line 1187 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = void_stmts(p, (yyvsp[-1].val));
		    }
#line 5959 "ripper.c" /* yacc.c:1646  */
    break;

  case 5:
#line 1193 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_BEGIN(0, &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5;v1=dispatch0(stmts_new);v2=dispatch0(void_stmt);v3=v1;v4=v2;v5=dispatch2(stmts_add,v3,v4);(yyval.val)=v5;}
		    }
#line 5970 "ripper.c" /* yacc.c:1646  */
    break;

  case 6:
#line 1200 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = newline_node((yyvsp[0].val));
#endif
			{VALUE v1,v2,v3,v4;v1=dispatch0(stmts_new);v2=v1;v3=(yyvsp[0].val);v4=dispatch2(stmts_add,v2,v3);(yyval.val)=v4;}
		    }
#line 5981 "ripper.c" /* yacc.c:1646  */
    break;

  case 7:
#line 1207 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = block_append(p, (yyvsp[-2].val), newline_node((yyvsp[0].val)));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(stmts_add,v1,v2);(yyval.val)=v3;}
		    }
#line 5992 "ripper.c" /* yacc.c:1646  */
    break;

  case 8:
#line 1214 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = remove_begin((yyvsp[0].val));
		    }
#line 6000 "ripper.c" /* yacc.c:1646  */
    break;

  case 10:
#line 1221 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = (yyvsp[0].val);
		    }
#line 6008 "ripper.c" /* yacc.c:1646  */
    break;

  case 11:
#line 1227 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			p->eval_tree_begin = block_append(p, p->eval_tree_begin,
							  NEW_BEGIN((yyvsp[-1].val), &(yyloc)));
			(yyval.val) = NEW_BEGIN(0, &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(BEGIN,v1);(yyval.val)=v2;}
		    }
#line 6021 "ripper.c" /* yacc.c:1646  */
    break;

  case 12:
#line 1239 "ripper.y" /* yacc.c:1646  */
    {if (!(yyvsp[-1].val)) {yyerror1(&(yylsp[0]), "else without rescue is useless");}}
#line 6027 "ripper.c" /* yacc.c:1646  */
    break;

  case 13:
#line 1242 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = new_bodystmt(p, (yyvsp[-5].val), (yyvsp[-4].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5;v1=escape_Qundef((yyvsp[-5].val));v2=escape_Qundef((yyvsp[-4].val));v3=escape_Qundef((yyvsp[-1].val));v4=escape_Qundef((yyvsp[0].val));v5=dispatch4(bodystmt,v1,v2,v3,v4);(yyval.val)=v5;}
		    }
#line 6038 "ripper.c" /* yacc.c:1646  */
    break;

  case 14:
#line 1251 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = new_bodystmt(p, (yyvsp[-2].val), (yyvsp[-1].val), 0, (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5;v1=escape_Qundef((yyvsp[-2].val));v2=escape_Qundef((yyvsp[-1].val));v3=Qnil;v4=escape_Qundef((yyvsp[0].val));v5=dispatch4(bodystmt,v1,v2,v3,v4);(yyval.val)=v5;}
		    }
#line 6049 "ripper.c" /* yacc.c:1646  */
    break;

  case 15:
#line 1260 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = void_stmts(p, (yyvsp[-1].val));
		    }
#line 6057 "ripper.c" /* yacc.c:1646  */
    break;

  case 16:
#line 1266 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_BEGIN(0, &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5;v1=dispatch0(stmts_new);v2=dispatch0(void_stmt);v3=v1;v4=v2;v5=dispatch2(stmts_add,v3,v4);(yyval.val)=v5;}
		    }
#line 6068 "ripper.c" /* yacc.c:1646  */
    break;

  case 17:
#line 1273 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = newline_node((yyvsp[0].val));
#endif
			{VALUE v1,v2,v3,v4;v1=dispatch0(stmts_new);v2=v1;v3=(yyvsp[0].val);v4=dispatch2(stmts_add,v2,v3);(yyval.val)=v4;}
		    }
#line 6079 "ripper.c" /* yacc.c:1646  */
    break;

  case 18:
#line 1280 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = block_append(p, (yyvsp[-2].val), newline_node((yyvsp[0].val)));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(stmts_add,v1,v2);(yyval.val)=v3;}
		    }
#line 6090 "ripper.c" /* yacc.c:1646  */
    break;

  case 19:
#line 1287 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = remove_begin((yyvsp[0].val));
		    }
#line 6098 "ripper.c" /* yacc.c:1646  */
    break;

  case 20:
#line 1293 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = (yyvsp[0].val);
		    }
#line 6106 "ripper.c" /* yacc.c:1646  */
    break;

  case 21:
#line 1297 "ripper.y" /* yacc.c:1646  */
    {
			yyerror1(&(yylsp[0]), "BEGIN is permitted only at toplevel");
		    }
#line 6114 "ripper.c" /* yacc.c:1646  */
    break;

  case 22:
#line 1301 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = (yyvsp[0].val);
		    }
#line 6122 "ripper.c" /* yacc.c:1646  */
    break;

  case 23:
#line 1306 "ripper.y" /* yacc.c:1646  */
    {SET_LEX_STATE(EXPR_FNAME|EXPR_FITEM);}
#line 6128 "ripper.c" /* yacc.c:1646  */
    break;

  case 24:
#line 1307 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_ALIAS((yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(alias,v1,v2);(yyval.val)=v3;}
		    }
#line 6139 "ripper.c" /* yacc.c:1646  */
    break;

  case 25:
#line 1314 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_VALIAS((yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-1].val);v2=(yyvsp[0].val);v3=dispatch2(var_alias,v1,v2);(yyval.val)=v3;}
		    }
#line 6150 "ripper.c" /* yacc.c:1646  */
    break;

  case 26:
#line 1321 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			char buf[2];
			buf[0] = '$';
			buf[1] = (char)(yyvsp[0].val)->nd_nth;
			(yyval.val) = NEW_VALIAS((yyvsp[-1].val), rb_intern2(buf, 2), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-1].val);v2=(yyvsp[0].val);v3=dispatch2(var_alias,v1,v2);(yyval.val)=v3;}
		    }
#line 6164 "ripper.c" /* yacc.c:1646  */
    break;

  case 27:
#line 1331 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			yyerror1(&(yylsp[0]), "can't make alias for the number variables");
			(yyval.val) = NEW_BEGIN(0, &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5;v1=(yyvsp[-1].val);v2=(yyvsp[0].val);v3=dispatch2(var_alias,v1,v2);v4=v3;v5=dispatch1(alias_error,v4);(yyval.val)=v5;}ripper_error(p);
		    }
#line 6176 "ripper.c" /* yacc.c:1646  */
    break;

  case 28:
#line 1339 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = (yyvsp[0].val);
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(undef,v1);(yyval.val)=v2;}
		    }
#line 6187 "ripper.c" /* yacc.c:1646  */
    break;

  case 29:
#line 1346 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = new_if(p, (yyvsp[0].val), remove_begin((yyvsp[-2].val)), 0, &(yyloc));
			fixpos((yyval.val), (yyvsp[0].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[0].val);v2=(yyvsp[-2].val);v3=dispatch2(if_mod,v1,v2);(yyval.val)=v3;}
		    }
#line 6199 "ripper.c" /* yacc.c:1646  */
    break;

  case 30:
#line 1354 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = new_unless(p, (yyvsp[0].val), remove_begin((yyvsp[-2].val)), 0, &(yyloc));
			fixpos((yyval.val), (yyvsp[0].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[0].val);v2=(yyvsp[-2].val);v3=dispatch2(unless_mod,v1,v2);(yyval.val)=v3;}
		    }
#line 6211 "ripper.c" /* yacc.c:1646  */
    break;

  case 31:
#line 1362 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			if ((yyvsp[-2].val) && nd_type((yyvsp[-2].val)) == NODE_BEGIN) {
			    (yyval.val) = NEW_WHILE(cond(p, (yyvsp[0].val), &(yylsp[0])), (yyvsp[-2].val)->nd_body, 0, &(yyloc));
			}
			else {
			    (yyval.val) = NEW_WHILE(cond(p, (yyvsp[0].val), &(yylsp[0])), (yyvsp[-2].val), 1, &(yyloc));
			}
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[0].val);v2=(yyvsp[-2].val);v3=dispatch2(while_mod,v1,v2);(yyval.val)=v3;}
		    }
#line 6227 "ripper.c" /* yacc.c:1646  */
    break;

  case 32:
#line 1374 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			if ((yyvsp[-2].val) && nd_type((yyvsp[-2].val)) == NODE_BEGIN) {
			    (yyval.val) = NEW_UNTIL(cond(p, (yyvsp[0].val), &(yylsp[0])), (yyvsp[-2].val)->nd_body, 0, &(yyloc));
			}
			else {
			    (yyval.val) = NEW_UNTIL(cond(p, (yyvsp[0].val), &(yylsp[0])), (yyvsp[-2].val), 1, &(yyloc));
			}
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[0].val);v2=(yyvsp[-2].val);v3=dispatch2(until_mod,v1,v2);(yyval.val)=v3;}
		    }
#line 6243 "ripper.c" /* yacc.c:1646  */
    break;

  case 33:
#line 1386 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			NODE *resq;
			YYLTYPE loc = code_loc_gen(&(yylsp[-1]), &(yylsp[0]));
			resq = NEW_RESBODY(0, remove_begin((yyvsp[0].val)), 0, &loc);
			(yyval.val) = NEW_RESCUE(remove_begin((yyvsp[-2].val)), resq, 0, &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(rescue_mod,v1,v2);(yyval.val)=v3;}
		    }
#line 6257 "ripper.c" /* yacc.c:1646  */
    break;

  case 34:
#line 1396 "ripper.y" /* yacc.c:1646  */
    {
			if (p->in_def) {
			    rb_warn0("END in method; use at_exit");
			}
#if 0
			{
			    NODE *scope = NEW_NODE(
				NODE_SCOPE, 0 /* tbl */, (yyvsp[-1].val) /* body */, 0 /* args */, &(yyloc));
			    (yyval.val) = NEW_POSTEXE(scope, &(yyloc));
			}
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(END,v1);(yyval.val)=v2;}
		    }
#line 6275 "ripper.c" /* yacc.c:1646  */
    break;

  case 36:
#line 1411 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			value_expr((yyvsp[0].val));
			(yyval.val) = node_assign(p, (yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(massign,v1,v2);(yyval.val)=v3;}
		    }
#line 6287 "ripper.c" /* yacc.c:1646  */
    break;

  case 37:
#line 1419 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			value_expr((yyvsp[0].val));
			(yyval.val) = node_assign(p, (yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(assign,v1,v2);(yyval.val)=v3;}
		    }
#line 6299 "ripper.c" /* yacc.c:1646  */
    break;

  case 38:
#line 1427 "ripper.y" /* yacc.c:1646  */
    {
#if 0
                        YYLTYPE loc = code_loc_gen(&(yylsp[-1]), &(yylsp[0]));
                        value_expr((yyvsp[-2].val));
			(yyval.val) = node_assign(p, (yyvsp[-4].val), NEW_RESCUE((yyvsp[-2].val), NEW_RESBODY(0, remove_begin((yyvsp[0].val)), 0, &loc), 0, &(yyloc)), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(rescue_mod,v1,v2);v4=(yyvsp[-4].val);v5=v3;v6=dispatch2(massign,v4,v5);(yyval.val)=v6;}
                    }
#line 6312 "ripper.c" /* yacc.c:1646  */
    break;

  case 39:
#line 1436 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = node_assign(p, (yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(massign,v1,v2);(yyval.val)=v3;}
		    }
#line 6323 "ripper.c" /* yacc.c:1646  */
    break;

  case 41:
#line 1446 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			value_expr((yyvsp[0].val));
			(yyval.val) = node_assign(p, (yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(assign,v1,v2);(yyval.val)=v3;}
		    }
#line 6335 "ripper.c" /* yacc.c:1646  */
    break;

  case 42:
#line 1454 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			value_expr((yyvsp[0].val));
			(yyval.val) = new_op_assign(p, (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-2].val);v2=(yyvsp[-1].val);v3=(yyvsp[0].val);v4=dispatch3(opassign,v1,v2,v3);(yyval.val)=v4;}
		    }
#line 6347 "ripper.c" /* yacc.c:1646  */
    break;

  case 43:
#line 1462 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			value_expr((yyvsp[0].val));
			(yyval.val) = new_ary_op_assign(p, (yyvsp[-5].val), (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-3]), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7;v1=(yyvsp[-5].val);v2=escape_Qundef((yyvsp[-3].val));v3=dispatch2(aref_field,v1,v2);v4=v3;v5=(yyvsp[-1].val);v6=(yyvsp[0].val);v7=dispatch3(opassign,v4,v5,v6);(yyval.val)=v7;}

		    }
#line 6360 "ripper.c" /* yacc.c:1646  */
    break;

  case 44:
#line 1471 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			value_expr((yyvsp[0].val));
			(yyval.val) = new_attr_op_assign(p, (yyvsp[-4].val), (yyvsp[-3].val), (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7,v8;v1=(yyvsp[-4].val);v2=(yyvsp[-3].val);v3=(yyvsp[-2].val);v4=dispatch3(field,v1,v2,v3);v5=v4;v6=(yyvsp[-1].val);v7=(yyvsp[0].val);v8=dispatch3(opassign,v5,v6,v7);(yyval.val)=v8;}
		    }
#line 6372 "ripper.c" /* yacc.c:1646  */
    break;

  case 45:
#line 1479 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			value_expr((yyvsp[0].val));
			(yyval.val) = new_attr_op_assign(p, (yyvsp[-4].val), (yyvsp[-3].val), (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7,v8;v1=(yyvsp[-4].val);v2=(yyvsp[-3].val);v3=(yyvsp[-2].val);v4=dispatch3(field,v1,v2,v3);v5=v4;v6=(yyvsp[-1].val);v7=(yyvsp[0].val);v8=dispatch3(opassign,v5,v6,v7);(yyval.val)=v8;}
		    }
#line 6384 "ripper.c" /* yacc.c:1646  */
    break;

  case 46:
#line 1487 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			YYLTYPE loc = code_loc_gen(&(yylsp[-4]), &(yylsp[-2]));
			(yyval.val) = new_const_op_assign(p, NEW_COLON2((yyvsp[-4].val), (yyvsp[-2].val), &loc), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7;v1=(yyvsp[-4].val);v2=(yyvsp[-2].val);v3=dispatch2(const_path_field,v1,v2);v4=v3;v5=(yyvsp[-1].val);v6=(yyvsp[0].val);v7=dispatch3(opassign,v4,v5,v6);(yyval.val)=v7;}
		    }
#line 6396 "ripper.c" /* yacc.c:1646  */
    break;

  case 47:
#line 1495 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			value_expr((yyvsp[0].val));
			(yyval.val) = new_attr_op_assign(p, (yyvsp[-4].val), ID2VAL(idCOLON2), (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7,v8;v1=(yyvsp[-4].val);v2=ID2VAL(idCOLON2);v3=(yyvsp[-2].val);v4=dispatch3(field,v1,v2,v3);v5=v4;v6=(yyvsp[-1].val);v7=(yyvsp[0].val);v8=dispatch3(opassign,v5,v6,v7);(yyval.val)=v8;}
		    }
#line 6408 "ripper.c" /* yacc.c:1646  */
    break;

  case 48:
#line 1503 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			rb_backref_error(p, (yyvsp[-2].val));
			(yyval.val) = NEW_BEGIN(0, &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5;v1=var_field(p, (yyvsp[-2].val));v2=(yyvsp[0].val);v3=dispatch2(assign,v1,v2);v4=v3;v5=dispatch1(assign_error,v4);(yyval.val)=v5;}ripper_error(p);
		    }
#line 6420 "ripper.c" /* yacc.c:1646  */
    break;

  case 49:
#line 1513 "ripper.y" /* yacc.c:1646  */
    {
			value_expr((yyvsp[0].val));
			(yyval.val) = (yyvsp[0].val);
		    }
#line 6429 "ripper.c" /* yacc.c:1646  */
    break;

  case 50:
#line 1518 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			YYLTYPE loc = code_loc_gen(&(yylsp[-1]), &(yylsp[0]));
			value_expr((yyvsp[-2].val));
			(yyval.val) = NEW_RESCUE((yyvsp[-2].val), NEW_RESBODY(0, remove_begin((yyvsp[0].val)), 0, &loc), 0, &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(rescue_mod,v1,v2);(yyval.val)=v3;}
		    }
#line 6442 "ripper.c" /* yacc.c:1646  */
    break;

  case 53:
#line 1531 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = logop(p, idAND, (yyvsp[-2].val), (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 6450 "ripper.c" /* yacc.c:1646  */
    break;

  case 54:
#line 1535 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = logop(p, idOR, (yyvsp[-2].val), (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 6458 "ripper.c" /* yacc.c:1646  */
    break;

  case 55:
#line 1539 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = call_uni_op(p, method_cond(p, (yyvsp[0].val), &(yylsp[0])), METHOD_NOT, &(yylsp[-2]), &(yyloc));
		    }
#line 6466 "ripper.c" /* yacc.c:1646  */
    break;

  case 56:
#line 1543 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = call_uni_op(p, method_cond(p, (yyvsp[0].val), &(yylsp[0])), '!', &(yylsp[-1]), &(yyloc));
		    }
#line 6474 "ripper.c" /* yacc.c:1646  */
    break;

  case 57:
#line 1547 "ripper.y" /* yacc.c:1646  */
    {
			value_expr((yyvsp[-1].val));
			SET_LEX_STATE(EXPR_BEG|EXPR_LABEL);
			p->command_start = FALSE;
			(yyval.num) = p->in_kwarg;
			p->in_kwarg = 1;
		    }
#line 6486 "ripper.c" /* yacc.c:1646  */
    break;

  case 58:
#line 1555 "ripper.y" /* yacc.c:1646  */
    {
			p->in_kwarg = !!(yyvsp[-2].num);
#if 0
			(yyval.val) = NEW_CASE3((yyvsp[-3].val), NEW_IN((yyvsp[0].val), NEW_TRUE(&(yylsp[0])), NEW_FALSE(&(yylsp[0])), &(yylsp[0])), &(yyloc));
			rb_warn0L(nd_line((yyval.val)), "Pattern matching is experimental, and the behavior may change in future versions of Ruby!");
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7;v1=(yyvsp[0].val);v2=Qnil;v3=Qnil;v4=dispatch3(in,v1,v2,v3);v5=(yyvsp[-3].val);v6=v4;v7=dispatch2(case,v5,v6);(yyval.val)=v7;}
		    }
#line 6499 "ripper.c" /* yacc.c:1646  */
    break;

  case 60:
#line 1567 "ripper.y" /* yacc.c:1646  */
    {
			value_expr((yyvsp[0].val));
			(yyval.val) = (yyvsp[0].val);
		    }
#line 6508 "ripper.c" /* yacc.c:1646  */
    break;

  case 61:
#line 1573 "ripper.y" /* yacc.c:1646  */
    {COND_PUSH(1);}
#line 6514 "ripper.c" /* yacc.c:1646  */
    break;

  case 62:
#line 1573 "ripper.y" /* yacc.c:1646  */
    {COND_POP();}
#line 6520 "ripper.c" /* yacc.c:1646  */
    break;

  case 63:
#line 1574 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = (yyvsp[-2].val);
		    }
#line 6528 "ripper.c" /* yacc.c:1646  */
    break;

  case 67:
#line 1585 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = new_qcall(p, (yyvsp[-2].val), (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7;v1=(yyvsp[-3].val);v2=(yyvsp[-2].val);v3=(yyvsp[-1].val);v4=dispatch3(call,v1,v2,v3);v5=v4;v6=(yyvsp[0].val);v7=dispatch2(method_add_arg,v5,v6);(yyval.val)=v7;}
		    }
#line 6539 "ripper.c" /* yacc.c:1646  */
    break;

  case 68:
#line 1594 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = (yyvsp[-1].val);
#if 0
			(yyval.val)->nd_body->nd_loc = code_loc_gen(&(yylsp[-2]), &(yylsp[0]));
			nd_set_line((yyval.val), (yylsp[-2]).end_pos.lineno);
#endif
		    }
#line 6551 "ripper.c" /* yacc.c:1646  */
    break;

  case 69:
#line 1604 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_FCALL((yyvsp[0].val), 0, &(yyloc));
			nd_set_line((yyval.val), p->tokline);
#endif
			(yyval.val)=(yyvsp[0].val);
		    }
#line 6563 "ripper.c" /* yacc.c:1646  */
    break;

  case 70:
#line 1614 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyvsp[-1].val)->nd_args = (yyvsp[0].val);
			nd_set_last_loc((yyvsp[-1].val), (yylsp[0]).end_pos);
			(yyval.val) = (yyvsp[-1].val);
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-1].val);v2=(yyvsp[0].val);v3=dispatch2(command,v1,v2);(yyval.val)=v3;}
		    }
#line 6576 "ripper.c" /* yacc.c:1646  */
    break;

  case 71:
#line 1623 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			block_dup_check(p, (yyvsp[-1].val), (yyvsp[0].val));
			(yyvsp[-2].val)->nd_args = (yyvsp[-1].val);
			(yyval.val) = method_add_block(p, (yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
			fixpos((yyval.val), (yyvsp[-2].val));
			nd_set_last_loc((yyvsp[-2].val), (yylsp[-1]).end_pos);
#endif
			{VALUE v1,v2,v3,v4,v5,v6;v1=(yyvsp[-2].val);v2=(yyvsp[-1].val);v3=dispatch2(command,v1,v2);v4=v3;v5=(yyvsp[0].val);v6=dispatch2(method_add_block,v4,v5);(yyval.val)=v6;}
		    }
#line 6591 "ripper.c" /* yacc.c:1646  */
    break;

  case 72:
#line 1634 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = new_command_qcall(p, (yyvsp[-2].val), (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), Qnull, &(yylsp[-1]), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5;v1=(yyvsp[-3].val);v2=(yyvsp[-2].val);v3=(yyvsp[-1].val);v4=(yyvsp[0].val);v5=dispatch4(command_call,v1,v2,v3,v4);(yyval.val)=v5;}
		    }
#line 6602 "ripper.c" /* yacc.c:1646  */
    break;

  case 73:
#line 1641 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = new_command_qcall(p, (yyvsp[-3].val), (yyvsp[-4].val), (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-2]), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7,v8;v1=(yyvsp[-4].val);v2=(yyvsp[-3].val);v3=(yyvsp[-2].val);v4=(yyvsp[-1].val);v5=dispatch4(command_call,v1,v2,v3,v4);v6=v5;v7=(yyvsp[0].val);v8=dispatch2(method_add_block,v6,v7);(yyval.val)=v8;}
		    }
#line 6613 "ripper.c" /* yacc.c:1646  */
    break;

  case 74:
#line 1648 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = new_command_qcall(p, ID2VAL(idCOLON2), (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), Qnull, &(yylsp[-1]), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5;v1=(yyvsp[-3].val);v2=ID2VAL(idCOLON2);v3=(yyvsp[-1].val);v4=(yyvsp[0].val);v5=dispatch4(command_call,v1,v2,v3,v4);(yyval.val)=v5;}
		    }
#line 6624 "ripper.c" /* yacc.c:1646  */
    break;

  case 75:
#line 1655 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = new_command_qcall(p, ID2VAL(idCOLON2), (yyvsp[-4].val), (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-2]), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7,v8;v1=(yyvsp[-4].val);v2=ID2VAL(idCOLON2);v3=(yyvsp[-2].val);v4=(yyvsp[-1].val);v5=dispatch4(command_call,v1,v2,v3,v4);v6=v5;v7=(yyvsp[0].val);v8=dispatch2(method_add_block,v6,v7);(yyval.val)=v8;}
		   }
#line 6635 "ripper.c" /* yacc.c:1646  */
    break;

  case 76:
#line 1662 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_SUPER((yyvsp[0].val), &(yyloc));
			fixpos((yyval.val), (yyvsp[0].val));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(super,v1);(yyval.val)=v2;}
		    }
#line 6647 "ripper.c" /* yacc.c:1646  */
    break;

  case 77:
#line 1670 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = new_yield(p, (yyvsp[0].val), &(yyloc));
			fixpos((yyval.val), (yyvsp[0].val));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(yield,v1);(yyval.val)=v2;}
		    }
#line 6659 "ripper.c" /* yacc.c:1646  */
    break;

  case 78:
#line 1678 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_RETURN(ret_args(p, (yyvsp[0].val)), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(return,v1);(yyval.val)=v2;}
		    }
#line 6670 "ripper.c" /* yacc.c:1646  */
    break;

  case 79:
#line 1685 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_BREAK(ret_args(p, (yyvsp[0].val)), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(break,v1);(yyval.val)=v2;}
		    }
#line 6681 "ripper.c" /* yacc.c:1646  */
    break;

  case 80:
#line 1692 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_NEXT(ret_args(p, (yyvsp[0].val)), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(next,v1);(yyval.val)=v2;}
		    }
#line 6692 "ripper.c" /* yacc.c:1646  */
    break;

  case 82:
#line 1702 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = (yyvsp[-1].val);
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(mlhs_paren,v1);(yyval.val)=v2;}
		    }
#line 6703 "ripper.c" /* yacc.c:1646  */
    break;

  case 84:
#line 1712 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_MASGN(NEW_LIST((yyvsp[-1].val), &(yyloc)), 0, &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(mlhs_paren,v1);(yyval.val)=v2;}
		    }
#line 6714 "ripper.c" /* yacc.c:1646  */
    break;

  case 85:
#line 1721 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_MASGN((yyvsp[0].val), 0, &(yyloc));
#endif
			(yyval.val)=(yyvsp[0].val);
		    }
#line 6725 "ripper.c" /* yacc.c:1646  */
    break;

  case 86:
#line 1728 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_MASGN(list_append(p, (yyvsp[-1].val),(yyvsp[0].val)), 0, &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-1].val);v2=(yyvsp[0].val);v3=dispatch2(mlhs_add,v1,v2);(yyval.val)=v3;}
		    }
#line 6736 "ripper.c" /* yacc.c:1646  */
    break;

  case 87:
#line 1735 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_MASGN((yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(mlhs_add_star,v1,v2);(yyval.val)=v3;}
		    }
#line 6747 "ripper.c" /* yacc.c:1646  */
    break;

  case 88:
#line 1742 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_MASGN((yyvsp[-4].val), NEW_POSTARG((yyvsp[-2].val),(yyvsp[0].val),&(yyloc)), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6;v1=(yyvsp[-4].val);v2=(yyvsp[-2].val);v3=dispatch2(mlhs_add_star,v1,v2);v4=v3;v5=(yyvsp[0].val);v6=dispatch2(mlhs_add_post,v4,v5);(yyval.val)=v6;}
		    }
#line 6758 "ripper.c" /* yacc.c:1646  */
    break;

  case 89:
#line 1749 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_MASGN((yyvsp[-1].val), NODE_SPECIAL_NO_NAME_REST, &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-1].val);v2=Qnil;v3=dispatch2(mlhs_add_star,v1,v2);(yyval.val)=v3;}
		    }
#line 6769 "ripper.c" /* yacc.c:1646  */
    break;

  case 90:
#line 1756 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_MASGN((yyvsp[-3].val), NEW_POSTARG(NODE_SPECIAL_NO_NAME_REST, (yyvsp[0].val), &(yyloc)), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6;v1=(yyvsp[-3].val);v2=Qnil;v3=dispatch2(mlhs_add_star,v1,v2);v4=v3;v5=(yyvsp[0].val);v6=dispatch2(mlhs_add_post,v4,v5);(yyval.val)=v6;}
		    }
#line 6780 "ripper.c" /* yacc.c:1646  */
    break;

  case 91:
#line 1763 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_MASGN(0, (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=dispatch0(mlhs_new);v2=v1;v3=(yyvsp[0].val);v4=dispatch2(mlhs_add_star,v2,v3);(yyval.val)=v4;}
		    }
#line 6791 "ripper.c" /* yacc.c:1646  */
    break;

  case 92:
#line 1770 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_MASGN(0, NEW_POSTARG((yyvsp[-2].val),(yyvsp[0].val),&(yyloc)), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7;v1=dispatch0(mlhs_new);v2=v1;v3=(yyvsp[-2].val);v4=dispatch2(mlhs_add_star,v2,v3);v5=v4;v6=(yyvsp[0].val);v7=dispatch2(mlhs_add_post,v5,v6);(yyval.val)=v7;}
		    }
#line 6802 "ripper.c" /* yacc.c:1646  */
    break;

  case 93:
#line 1777 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_MASGN(0, NODE_SPECIAL_NO_NAME_REST, &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=dispatch0(mlhs_new);v2=v1;v3=Qnil;v4=dispatch2(mlhs_add_star,v2,v3);(yyval.val)=v4;}
		    }
#line 6813 "ripper.c" /* yacc.c:1646  */
    break;

  case 94:
#line 1784 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_MASGN(0, NEW_POSTARG(NODE_SPECIAL_NO_NAME_REST, (yyvsp[0].val), &(yyloc)), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7;v1=dispatch0(mlhs_new);v2=v1;v3=Qnil;v4=dispatch2(mlhs_add_star,v2,v3);v5=v4;v6=(yyvsp[0].val);v7=dispatch2(mlhs_add_post,v5,v6);(yyval.val)=v7;}
		    }
#line 6824 "ripper.c" /* yacc.c:1646  */
    break;

  case 96:
#line 1794 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = (yyvsp[-1].val);
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(mlhs_paren,v1);(yyval.val)=v2;}
		    }
#line 6835 "ripper.c" /* yacc.c:1646  */
    break;

  case 97:
#line 1803 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_LIST((yyvsp[-1].val), &(yylsp[-1]));
#endif
			{VALUE v1,v2,v3,v4;v1=dispatch0(mlhs_new);v2=v1;v3=(yyvsp[-1].val);v4=dispatch2(mlhs_add,v2,v3);(yyval.val)=v4;}
		    }
#line 6846 "ripper.c" /* yacc.c:1646  */
    break;

  case 98:
#line 1810 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = list_append(p, (yyvsp[-2].val), (yyvsp[-1].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[-1].val);v3=dispatch2(mlhs_add,v1,v2);(yyval.val)=v3;}
		    }
#line 6857 "ripper.c" /* yacc.c:1646  */
    break;

  case 99:
#line 1819 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_LIST((yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=dispatch0(mlhs_new);v2=v1;v3=(yyvsp[0].val);v4=dispatch2(mlhs_add,v2,v3);(yyval.val)=v4;}
		    }
#line 6868 "ripper.c" /* yacc.c:1646  */
    break;

  case 100:
#line 1826 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = list_append(p, (yyvsp[-2].val), (yyvsp[0].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(mlhs_add,v1,v2);(yyval.val)=v3;}
		    }
#line 6879 "ripper.c" /* yacc.c:1646  */
    break;

  case 101:
#line 1835 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = assignable(p, (yyvsp[0].val), 0, &(yyloc));
#endif
			(yyval.val)=assignable(p, var_field(p, (yyvsp[0].val)));
		    }
#line 6890 "ripper.c" /* yacc.c:1646  */
    break;

  case 102:
#line 1842 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = assignable(p, (yyvsp[0].val), 0, &(yyloc));
#endif
			(yyval.val)=assignable(p, var_field(p, (yyvsp[0].val)));
		    }
#line 6901 "ripper.c" /* yacc.c:1646  */
    break;

  case 103:
#line 1849 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = aryset(p, (yyvsp[-3].val), (yyvsp[-1].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-3].val);v2=escape_Qundef((yyvsp[-1].val));v3=dispatch2(aref_field,v1,v2);(yyval.val)=v3;}
		    }
#line 6912 "ripper.c" /* yacc.c:1646  */
    break;

  case 104:
#line 1856 "ripper.y" /* yacc.c:1646  */
    {
			if ((yyvsp[-1].val) == tANDDOT) {
			    yyerror1(&(yylsp[-1]), "&. inside multiple assignment destination");
			}
#if 0
			(yyval.val) = attrset(p, (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-2].val);v2=(yyvsp[-1].val);v3=(yyvsp[0].val);v4=dispatch3(field,v1,v2,v3);(yyval.val)=v4;}
		    }
#line 6926 "ripper.c" /* yacc.c:1646  */
    break;

  case 105:
#line 1866 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = attrset(p, (yyvsp[-2].val), idCOLON2, (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(const_path_field,v1,v2);(yyval.val)=v3;}
		    }
#line 6937 "ripper.c" /* yacc.c:1646  */
    break;

  case 106:
#line 1873 "ripper.y" /* yacc.c:1646  */
    {
			if ((yyvsp[-1].val) == tANDDOT) {
			    yyerror1(&(yylsp[-1]), "&. inside multiple assignment destination");
			}
#if 0
			(yyval.val) = attrset(p, (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-2].val);v2=(yyvsp[-1].val);v3=(yyvsp[0].val);v4=dispatch3(field,v1,v2,v3);(yyval.val)=v4;}
		    }
#line 6951 "ripper.c" /* yacc.c:1646  */
    break;

  case 107:
#line 1883 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = const_decl(p, NEW_COLON2((yyvsp[-2].val), (yyvsp[0].val), &(yyloc)), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(const_path_field,v1,v2);(yyval.val)=const_decl(p, v3);}
		    }
#line 6962 "ripper.c" /* yacc.c:1646  */
    break;

  case 108:
#line 1890 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = const_decl(p, NEW_COLON3((yyvsp[0].val), &(yyloc)), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(top_const_field,v1);(yyval.val)=const_decl(p, v2);}
		    }
#line 6973 "ripper.c" /* yacc.c:1646  */
    break;

  case 109:
#line 1897 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			rb_backref_error(p, (yyvsp[0].val));
			(yyval.val) = NEW_BEGIN(0, &(yyloc));
#endif
			{VALUE v1,v2;v1=var_field(p, (yyvsp[0].val));v2=dispatch1(assign_error,v1);(yyval.val)=v2;}ripper_error(p);
		    }
#line 6985 "ripper.c" /* yacc.c:1646  */
    break;

  case 110:
#line 1907 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = assignable(p, (yyvsp[0].val), 0, &(yyloc));
#endif
			(yyval.val)=assignable(p, var_field(p, (yyvsp[0].val)));
		    }
#line 6996 "ripper.c" /* yacc.c:1646  */
    break;

  case 111:
#line 1914 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = assignable(p, (yyvsp[0].val), 0, &(yyloc));
#endif
			(yyval.val)=assignable(p, var_field(p, (yyvsp[0].val)));
		    }
#line 7007 "ripper.c" /* yacc.c:1646  */
    break;

  case 112:
#line 1921 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = aryset(p, (yyvsp[-3].val), (yyvsp[-1].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-3].val);v2=escape_Qundef((yyvsp[-1].val));v3=dispatch2(aref_field,v1,v2);(yyval.val)=v3;}
		    }
#line 7018 "ripper.c" /* yacc.c:1646  */
    break;

  case 113:
#line 1928 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = attrset(p, (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-2].val);v2=(yyvsp[-1].val);v3=(yyvsp[0].val);v4=dispatch3(field,v1,v2,v3);(yyval.val)=v4;}
		    }
#line 7029 "ripper.c" /* yacc.c:1646  */
    break;

  case 114:
#line 1935 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = attrset(p, (yyvsp[-2].val), idCOLON2, (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-2].val);v2=ID2VAL(idCOLON2);v3=(yyvsp[0].val);v4=dispatch3(field,v1,v2,v3);(yyval.val)=v4;}
		    }
#line 7040 "ripper.c" /* yacc.c:1646  */
    break;

  case 115:
#line 1942 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = attrset(p, (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-2].val);v2=(yyvsp[-1].val);v3=(yyvsp[0].val);v4=dispatch3(field,v1,v2,v3);(yyval.val)=v4;}
		    }
#line 7051 "ripper.c" /* yacc.c:1646  */
    break;

  case 116:
#line 1949 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = const_decl(p, NEW_COLON2((yyvsp[-2].val), (yyvsp[0].val), &(yyloc)), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(const_path_field,v1,v2);(yyval.val)=const_decl(p, v3);}
		    }
#line 7062 "ripper.c" /* yacc.c:1646  */
    break;

  case 117:
#line 1956 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = const_decl(p, NEW_COLON3((yyvsp[0].val), &(yyloc)), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(top_const_field,v1);(yyval.val)=const_decl(p, v2);}
		    }
#line 7073 "ripper.c" /* yacc.c:1646  */
    break;

  case 118:
#line 1963 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			rb_backref_error(p, (yyvsp[0].val));
			(yyval.val) = NEW_BEGIN(0, &(yyloc));
#endif
			{VALUE v1,v2;v1=var_field(p, (yyvsp[0].val));v2=dispatch1(assign_error,v1);(yyval.val)=v2;}ripper_error(p);
		    }
#line 7085 "ripper.c" /* yacc.c:1646  */
    break;

  case 119:
#line 1973 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			yyerror1(&(yylsp[0]), "class/module name must be CONSTANT");
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(class_name_error,v1);(yyval.val)=v2;}ripper_error(p);
		    }
#line 7096 "ripper.c" /* yacc.c:1646  */
    break;

  case 121:
#line 1983 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_COLON3((yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(top_const_ref,v1);(yyval.val)=v2;}
		    }
#line 7107 "ripper.c" /* yacc.c:1646  */
    break;

  case 122:
#line 1990 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_COLON2(0, (yyval.val), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(const_ref,v1);(yyval.val)=v2;}
		    }
#line 7118 "ripper.c" /* yacc.c:1646  */
    break;

  case 123:
#line 1997 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_COLON2((yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(const_path_ref,v1,v2);(yyval.val)=v3;}
		    }
#line 7129 "ripper.c" /* yacc.c:1646  */
    break;

  case 127:
#line 2009 "ripper.y" /* yacc.c:1646  */
    {
			SET_LEX_STATE(EXPR_ENDFN);
			(yyval.val) = (yyvsp[0].val);
		    }
#line 7138 "ripper.c" /* yacc.c:1646  */
    break;

  case 128:
#line 2014 "ripper.y" /* yacc.c:1646  */
    {
			SET_LEX_STATE(EXPR_ENDFN);
			(yyval.val) = (yyvsp[0].val);
		    }
#line 7147 "ripper.c" /* yacc.c:1646  */
    break;

  case 129:
#line 2021 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_LIT(ID2SYM((yyvsp[0].val)), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(symbol_literal,v1);(yyval.val)=v2;}
		    }
#line 7158 "ripper.c" /* yacc.c:1646  */
    break;

  case 131:
#line 2031 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_UNDEF((yyvsp[0].val), &(yyloc));
#endif
			(yyval.val)=rb_ary_new3(1, get_value((yyvsp[0].val)));
		    }
#line 7169 "ripper.c" /* yacc.c:1646  */
    break;

  case 132:
#line 2037 "ripper.y" /* yacc.c:1646  */
    {SET_LEX_STATE(EXPR_FNAME|EXPR_FITEM);}
#line 7175 "ripper.c" /* yacc.c:1646  */
    break;

  case 133:
#line 2038 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			NODE *undef = NEW_UNDEF((yyvsp[0].val), &(yylsp[0]));
			(yyval.val) = block_append(p, (yyvsp[-3].val), undef);
#endif
			(yyval.val)=rb_ary_push((yyvsp[-3].val), get_value((yyvsp[0].val)));
		    }
#line 7187 "ripper.c" /* yacc.c:1646  */
    break;

  case 134:
#line 2047 "ripper.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.val) = '|'); }
#line 7193 "ripper.c" /* yacc.c:1646  */
    break;

  case 135:
#line 2048 "ripper.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.val) = '^'); }
#line 7199 "ripper.c" /* yacc.c:1646  */
    break;

  case 136:
#line 2049 "ripper.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.val) = '&'); }
#line 7205 "ripper.c" /* yacc.c:1646  */
    break;

  case 137:
#line 2050 "ripper.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.val) = tCMP); }
#line 7211 "ripper.c" /* yacc.c:1646  */
    break;

  case 138:
#line 2051 "ripper.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.val) = tEQ); }
#line 7217 "ripper.c" /* yacc.c:1646  */
    break;

  case 139:
#line 2052 "ripper.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.val) = tEQQ); }
#line 7223 "ripper.c" /* yacc.c:1646  */
    break;

  case 140:
#line 2053 "ripper.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.val) = tMATCH); }
#line 7229 "ripper.c" /* yacc.c:1646  */
    break;

  case 141:
#line 2054 "ripper.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.val) = tNMATCH); }
#line 7235 "ripper.c" /* yacc.c:1646  */
    break;

  case 142:
#line 2055 "ripper.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.val) = '>'); }
#line 7241 "ripper.c" /* yacc.c:1646  */
    break;

  case 143:
#line 2056 "ripper.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.val) = tGEQ); }
#line 7247 "ripper.c" /* yacc.c:1646  */
    break;

  case 144:
#line 2057 "ripper.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.val) = '<'); }
#line 7253 "ripper.c" /* yacc.c:1646  */
    break;

  case 145:
#line 2058 "ripper.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.val) = tLEQ); }
#line 7259 "ripper.c" /* yacc.c:1646  */
    break;

  case 146:
#line 2059 "ripper.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.val) = tNEQ); }
#line 7265 "ripper.c" /* yacc.c:1646  */
    break;

  case 147:
#line 2060 "ripper.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.val) = tLSHFT); }
#line 7271 "ripper.c" /* yacc.c:1646  */
    break;

  case 148:
#line 2061 "ripper.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.val) = tRSHFT); }
#line 7277 "ripper.c" /* yacc.c:1646  */
    break;

  case 149:
#line 2062 "ripper.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.val) = '+'); }
#line 7283 "ripper.c" /* yacc.c:1646  */
    break;

  case 150:
#line 2063 "ripper.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.val) = '-'); }
#line 7289 "ripper.c" /* yacc.c:1646  */
    break;

  case 151:
#line 2064 "ripper.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.val) = '*'); }
#line 7295 "ripper.c" /* yacc.c:1646  */
    break;

  case 152:
#line 2065 "ripper.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.val) = '*'); }
#line 7301 "ripper.c" /* yacc.c:1646  */
    break;

  case 153:
#line 2066 "ripper.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.val) = '/'); }
#line 7307 "ripper.c" /* yacc.c:1646  */
    break;

  case 154:
#line 2067 "ripper.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.val) = '%'); }
#line 7313 "ripper.c" /* yacc.c:1646  */
    break;

  case 155:
#line 2068 "ripper.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.val) = tPOW); }
#line 7319 "ripper.c" /* yacc.c:1646  */
    break;

  case 156:
#line 2069 "ripper.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.val) = tDSTAR); }
#line 7325 "ripper.c" /* yacc.c:1646  */
    break;

  case 157:
#line 2070 "ripper.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.val) = '!'); }
#line 7331 "ripper.c" /* yacc.c:1646  */
    break;

  case 158:
#line 2071 "ripper.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.val) = '~'); }
#line 7337 "ripper.c" /* yacc.c:1646  */
    break;

  case 159:
#line 2072 "ripper.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.val) = tUPLUS); }
#line 7343 "ripper.c" /* yacc.c:1646  */
    break;

  case 160:
#line 2073 "ripper.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.val) = tUMINUS); }
#line 7349 "ripper.c" /* yacc.c:1646  */
    break;

  case 161:
#line 2074 "ripper.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.val) = tAREF); }
#line 7355 "ripper.c" /* yacc.c:1646  */
    break;

  case 162:
#line 2075 "ripper.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.val) = tASET); }
#line 7361 "ripper.c" /* yacc.c:1646  */
    break;

  case 163:
#line 2076 "ripper.y" /* yacc.c:1646  */
    { ifndef_ripper((yyval.val) = '`'); }
#line 7367 "ripper.c" /* yacc.c:1646  */
    break;

  case 205:
#line 2094 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = node_assign(p, (yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(assign,v1,v2);(yyval.val)=v3;}
		    }
#line 7378 "ripper.c" /* yacc.c:1646  */
    break;

  case 206:
#line 2101 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = new_op_assign(p, (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-2].val);v2=(yyvsp[-1].val);v3=(yyvsp[0].val);v4=dispatch3(opassign,v1,v2,v3);(yyval.val)=v4;}
		    }
#line 7389 "ripper.c" /* yacc.c:1646  */
    break;

  case 207:
#line 2108 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			value_expr((yyvsp[0].val));
			(yyval.val) = new_ary_op_assign(p, (yyvsp[-5].val), (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-3]), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7;v1=(yyvsp[-5].val);v2=escape_Qundef((yyvsp[-3].val));v3=dispatch2(aref_field,v1,v2);v4=v3;v5=(yyvsp[-1].val);v6=(yyvsp[0].val);v7=dispatch3(opassign,v4,v5,v6);(yyval.val)=v7;}
		    }
#line 7401 "ripper.c" /* yacc.c:1646  */
    break;

  case 208:
#line 2116 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			value_expr((yyvsp[0].val));
			(yyval.val) = new_attr_op_assign(p, (yyvsp[-4].val), (yyvsp[-3].val), (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7,v8;v1=(yyvsp[-4].val);v2=(yyvsp[-3].val);v3=(yyvsp[-2].val);v4=dispatch3(field,v1,v2,v3);v5=v4;v6=(yyvsp[-1].val);v7=(yyvsp[0].val);v8=dispatch3(opassign,v5,v6,v7);(yyval.val)=v8;}
		    }
#line 7413 "ripper.c" /* yacc.c:1646  */
    break;

  case 209:
#line 2124 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			value_expr((yyvsp[0].val));
			(yyval.val) = new_attr_op_assign(p, (yyvsp[-4].val), (yyvsp[-3].val), (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7,v8;v1=(yyvsp[-4].val);v2=(yyvsp[-3].val);v3=(yyvsp[-2].val);v4=dispatch3(field,v1,v2,v3);v5=v4;v6=(yyvsp[-1].val);v7=(yyvsp[0].val);v8=dispatch3(opassign,v5,v6,v7);(yyval.val)=v8;}
		    }
#line 7425 "ripper.c" /* yacc.c:1646  */
    break;

  case 210:
#line 2132 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			value_expr((yyvsp[0].val));
			(yyval.val) = new_attr_op_assign(p, (yyvsp[-4].val), ID2VAL(idCOLON2), (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7,v8;v1=(yyvsp[-4].val);v2=ID2VAL(idCOLON2);v3=(yyvsp[-2].val);v4=dispatch3(field,v1,v2,v3);v5=v4;v6=(yyvsp[-1].val);v7=(yyvsp[0].val);v8=dispatch3(opassign,v5,v6,v7);(yyval.val)=v8;}
		    }
#line 7437 "ripper.c" /* yacc.c:1646  */
    break;

  case 211:
#line 2140 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			YYLTYPE loc = code_loc_gen(&(yylsp[-4]), &(yylsp[-2]));
			(yyval.val) = new_const_op_assign(p, NEW_COLON2((yyvsp[-4].val), (yyvsp[-2].val), &loc), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7;v1=(yyvsp[-4].val);v2=(yyvsp[-2].val);v3=dispatch2(const_path_field,v1,v2);v4=v3;v5=(yyvsp[-1].val);v6=(yyvsp[0].val);v7=dispatch3(opassign,v4,v5,v6);(yyval.val)=v7;}
		    }
#line 7449 "ripper.c" /* yacc.c:1646  */
    break;

  case 212:
#line 2148 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = new_const_op_assign(p, NEW_COLON3((yyvsp[-2].val), &(yyloc)), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6;v1=(yyvsp[-2].val);v2=dispatch1(top_const_field,v1);v3=v2;v4=(yyvsp[-1].val);v5=(yyvsp[0].val);v6=dispatch3(opassign,v3,v4,v5);(yyval.val)=v6;}
		    }
#line 7460 "ripper.c" /* yacc.c:1646  */
    break;

  case 213:
#line 2155 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			rb_backref_error(p, (yyvsp[-2].val));
			(yyval.val) = NEW_BEGIN(0, &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6;v1=var_field(p, (yyvsp[-2].val));v2=(yyvsp[-1].val);v3=(yyvsp[0].val);v4=dispatch3(opassign,v1,v2,v3);v5=v4;v6=dispatch1(assign_error,v5);(yyval.val)=v6;}ripper_error(p);
		    }
#line 7472 "ripper.c" /* yacc.c:1646  */
    break;

  case 214:
#line 2163 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			value_expr((yyvsp[-2].val));
			value_expr((yyvsp[0].val));
			(yyval.val) = NEW_DOT2((yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(dot2,v1,v2);(yyval.val)=v3;}
		    }
#line 7485 "ripper.c" /* yacc.c:1646  */
    break;

  case 215:
#line 2172 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			value_expr((yyvsp[-2].val));
			value_expr((yyvsp[0].val));
			(yyval.val) = NEW_DOT3((yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(dot3,v1,v2);(yyval.val)=v3;}
		    }
#line 7498 "ripper.c" /* yacc.c:1646  */
    break;

  case 216:
#line 2181 "ripper.y" /* yacc.c:1646  */
    {
#if 0
                        YYLTYPE loc;
                        loc.beg_pos = (yylsp[0]).end_pos;
                        loc.end_pos = (yylsp[0]).end_pos;

			value_expr((yyvsp[-1].val));
			(yyval.val) = NEW_DOT2((yyvsp[-1].val), new_nil(&loc), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-1].val);v2=Qnil;v3=dispatch2(dot2,v1,v2);(yyval.val)=v3;}
		    }
#line 7514 "ripper.c" /* yacc.c:1646  */
    break;

  case 217:
#line 2193 "ripper.y" /* yacc.c:1646  */
    {
#if 0
                        YYLTYPE loc;
                        loc.beg_pos = (yylsp[0]).end_pos;
                        loc.end_pos = (yylsp[0]).end_pos;

			value_expr((yyvsp[-1].val));
			(yyval.val) = NEW_DOT3((yyvsp[-1].val), new_nil(&loc), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-1].val);v2=Qnil;v3=dispatch2(dot3,v1,v2);(yyval.val)=v3;}
		    }
#line 7530 "ripper.c" /* yacc.c:1646  */
    break;

  case 218:
#line 2205 "ripper.y" /* yacc.c:1646  */
    {
#if 0
                        YYLTYPE loc;
                        loc.beg_pos = (yylsp[-1]).beg_pos;
                        loc.end_pos = (yylsp[-1]).beg_pos;

			value_expr((yyvsp[0].val));
			(yyval.val) = NEW_DOT2(new_nil(&loc), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=Qnil;v2=(yyvsp[0].val);v3=dispatch2(dot2,v1,v2);(yyval.val)=v3;}
		    }
#line 7546 "ripper.c" /* yacc.c:1646  */
    break;

  case 219:
#line 2217 "ripper.y" /* yacc.c:1646  */
    {
#if 0
                        YYLTYPE loc;
                        loc.beg_pos = (yylsp[-1]).beg_pos;
                        loc.end_pos = (yylsp[-1]).beg_pos;

			value_expr((yyvsp[0].val));
			(yyval.val) = NEW_DOT3(new_nil(&loc), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=Qnil;v2=(yyvsp[0].val);v3=dispatch2(dot3,v1,v2);(yyval.val)=v3;}
		    }
#line 7562 "ripper.c" /* yacc.c:1646  */
    break;

  case 220:
#line 2229 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = call_bin_op(p, (yyvsp[-2].val), '+', (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7570 "ripper.c" /* yacc.c:1646  */
    break;

  case 221:
#line 2233 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = call_bin_op(p, (yyvsp[-2].val), '-', (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7578 "ripper.c" /* yacc.c:1646  */
    break;

  case 222:
#line 2237 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = call_bin_op(p, (yyvsp[-2].val), '*', (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7586 "ripper.c" /* yacc.c:1646  */
    break;

  case 223:
#line 2241 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = call_bin_op(p, (yyvsp[-2].val), '/', (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7594 "ripper.c" /* yacc.c:1646  */
    break;

  case 224:
#line 2245 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = call_bin_op(p, (yyvsp[-2].val), '%', (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7602 "ripper.c" /* yacc.c:1646  */
    break;

  case 225:
#line 2249 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = call_bin_op(p, (yyvsp[-2].val), idPow, (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7610 "ripper.c" /* yacc.c:1646  */
    break;

  case 226:
#line 2253 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = call_uni_op(p, call_bin_op(p, (yyvsp[-2].val), idPow, (yyvsp[0].val), &(yylsp[-2]), &(yyloc)), idUMinus, &(yylsp[-3]), &(yyloc));
		    }
#line 7618 "ripper.c" /* yacc.c:1646  */
    break;

  case 227:
#line 2257 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = call_uni_op(p, (yyvsp[0].val), idUPlus, &(yylsp[-1]), &(yyloc));
		    }
#line 7626 "ripper.c" /* yacc.c:1646  */
    break;

  case 228:
#line 2261 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = call_uni_op(p, (yyvsp[0].val), idUMinus, &(yylsp[-1]), &(yyloc));
		    }
#line 7634 "ripper.c" /* yacc.c:1646  */
    break;

  case 229:
#line 2265 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = call_bin_op(p, (yyvsp[-2].val), '|', (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7642 "ripper.c" /* yacc.c:1646  */
    break;

  case 230:
#line 2269 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = call_bin_op(p, (yyvsp[-2].val), '^', (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7650 "ripper.c" /* yacc.c:1646  */
    break;

  case 231:
#line 2273 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = call_bin_op(p, (yyvsp[-2].val), '&', (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7658 "ripper.c" /* yacc.c:1646  */
    break;

  case 232:
#line 2277 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = call_bin_op(p, (yyvsp[-2].val), idCmp, (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7666 "ripper.c" /* yacc.c:1646  */
    break;

  case 234:
#line 2282 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = call_bin_op(p, (yyvsp[-2].val), idEq, (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7674 "ripper.c" /* yacc.c:1646  */
    break;

  case 235:
#line 2286 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = call_bin_op(p, (yyvsp[-2].val), idEqq, (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7682 "ripper.c" /* yacc.c:1646  */
    break;

  case 236:
#line 2290 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = call_bin_op(p, (yyvsp[-2].val), idNeq, (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7690 "ripper.c" /* yacc.c:1646  */
    break;

  case 237:
#line 2294 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = match_op(p, (yyvsp[-2].val), (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7698 "ripper.c" /* yacc.c:1646  */
    break;

  case 238:
#line 2298 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = call_bin_op(p, (yyvsp[-2].val), idNeqTilde, (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7706 "ripper.c" /* yacc.c:1646  */
    break;

  case 239:
#line 2302 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = call_uni_op(p, method_cond(p, (yyvsp[0].val), &(yylsp[0])), '!', &(yylsp[-1]), &(yyloc));
		    }
#line 7714 "ripper.c" /* yacc.c:1646  */
    break;

  case 240:
#line 2306 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = call_uni_op(p, (yyvsp[0].val), '~', &(yylsp[-1]), &(yyloc));
		    }
#line 7722 "ripper.c" /* yacc.c:1646  */
    break;

  case 241:
#line 2310 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = call_bin_op(p, (yyvsp[-2].val), idLTLT, (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7730 "ripper.c" /* yacc.c:1646  */
    break;

  case 242:
#line 2314 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = call_bin_op(p, (yyvsp[-2].val), idGTGT, (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7738 "ripper.c" /* yacc.c:1646  */
    break;

  case 243:
#line 2318 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = logop(p, idANDOP, (yyvsp[-2].val), (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7746 "ripper.c" /* yacc.c:1646  */
    break;

  case 244:
#line 2322 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = logop(p, idOROP, (yyvsp[-2].val), (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7754 "ripper.c" /* yacc.c:1646  */
    break;

  case 245:
#line 2325 "ripper.y" /* yacc.c:1646  */
    {p->in_defined = 1;}
#line 7760 "ripper.c" /* yacc.c:1646  */
    break;

  case 246:
#line 2326 "ripper.y" /* yacc.c:1646  */
    {
			p->in_defined = 0;
			(yyval.val) = new_defined(p, (yyvsp[0].val), &(yyloc));
		    }
#line 7769 "ripper.c" /* yacc.c:1646  */
    break;

  case 247:
#line 2331 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			value_expr((yyvsp[-5].val));
			(yyval.val) = new_if(p, (yyvsp[-5].val), (yyvsp[-3].val), (yyvsp[0].val), &(yyloc));
			fixpos((yyval.val), (yyvsp[-5].val));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-5].val);v2=(yyvsp[-3].val);v3=(yyvsp[0].val);v4=dispatch3(ifop,v1,v2,v3);(yyval.val)=v4;}
		    }
#line 7782 "ripper.c" /* yacc.c:1646  */
    break;

  case 248:
#line 2340 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = (yyvsp[0].val);
		    }
#line 7790 "ripper.c" /* yacc.c:1646  */
    break;

  case 249:
#line 2345 "ripper.y" /* yacc.c:1646  */
    {(yyval.val) = '>';}
#line 7796 "ripper.c" /* yacc.c:1646  */
    break;

  case 250:
#line 2346 "ripper.y" /* yacc.c:1646  */
    {(yyval.val) = '<';}
#line 7802 "ripper.c" /* yacc.c:1646  */
    break;

  case 251:
#line 2347 "ripper.y" /* yacc.c:1646  */
    {(yyval.val) = idGE;}
#line 7808 "ripper.c" /* yacc.c:1646  */
    break;

  case 252:
#line 2348 "ripper.y" /* yacc.c:1646  */
    {(yyval.val) = idLE;}
#line 7814 "ripper.c" /* yacc.c:1646  */
    break;

  case 253:
#line 2352 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = call_bin_op(p, (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7822 "ripper.c" /* yacc.c:1646  */
    break;

  case 254:
#line 2356 "ripper.y" /* yacc.c:1646  */
    {
			rb_warning1("comparison '%s' after comparison", WARN_ID((yyvsp[-1].val)));
			(yyval.val) = call_bin_op(p, (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7831 "ripper.c" /* yacc.c:1646  */
    break;

  case 255:
#line 2363 "ripper.y" /* yacc.c:1646  */
    {
			value_expr((yyvsp[0].val));
			(yyval.val) = (yyvsp[0].val);
		    }
#line 7840 "ripper.c" /* yacc.c:1646  */
    break;

  case 257:
#line 2371 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = (yyvsp[-1].val);
		    }
#line 7848 "ripper.c" /* yacc.c:1646  */
    break;

  case 258:
#line 2375 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = (yyvsp[-1].val) ? arg_append(p, (yyvsp[-3].val), new_hash(p, (yyvsp[-1].val), &(yylsp[-1])), &(yyloc)) : (yyvsp[-3].val);
#endif
			{VALUE v1,v2,v3,v4,v5;v1=(yyvsp[-1].val);v2=dispatch1(bare_assoc_hash,v1);v3=(yyvsp[-3].val);v4=v2;v5=dispatch2(args_add,v3,v4);(yyval.val)=v5;}
		    }
#line 7859 "ripper.c" /* yacc.c:1646  */
    break;

  case 259:
#line 2382 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = (yyvsp[-1].val) ? NEW_LIST(new_hash(p, (yyvsp[-1].val), &(yylsp[-1])), &(yyloc)) : 0;
#endif
			{VALUE v1,v2,v3,v4,v5,v6;v1=dispatch0(args_new);v2=(yyvsp[-1].val);v3=dispatch1(bare_assoc_hash,v2);v4=v1;v5=v3;v6=dispatch2(args_add,v4,v5);(yyval.val)=v6;}
		    }
#line 7870 "ripper.c" /* yacc.c:1646  */
    break;

  case 260:
#line 2391 "ripper.y" /* yacc.c:1646  */
    {
			value_expr((yyvsp[0].val));
			(yyval.val) = (yyvsp[0].val);
		    }
#line 7879 "ripper.c" /* yacc.c:1646  */
    break;

  case 261:
#line 2396 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			YYLTYPE loc = code_loc_gen(&(yylsp[-1]), &(yylsp[0]));
			value_expr((yyvsp[-2].val));
			(yyval.val) = NEW_RESCUE((yyvsp[-2].val), NEW_RESBODY(0, remove_begin((yyvsp[0].val)), 0, &loc), 0, &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(rescue_mod,v1,v2);(yyval.val)=v3;}
		    }
#line 7892 "ripper.c" /* yacc.c:1646  */
    break;

  case 262:
#line 2407 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = (yyvsp[-1].val);
#endif
			{VALUE v1,v2;v1=escape_Qundef((yyvsp[-1].val));v2=dispatch1(arg_paren,v1);(yyval.val)=v2;}
		    }
#line 7903 "ripper.c" /* yacc.c:1646  */
    break;

  case 263:
#line 2414 "ripper.y" /* yacc.c:1646  */
    {
			if (!local_id(p, idFWD_REST) || !local_id(p, idFWD_KWREST) || !local_id(p, idFWD_BLOCK)) {
			    compile_error(p, "unexpected ...");
			    (yyval.val) = Qnone;
			}
			else {
#if 0
			    NODE *splat = NEW_SPLAT(NEW_LVAR(idFWD_REST, &(yylsp[-1])), &(yylsp[-1]));
			    NODE *kwrest = list_append(p, NEW_LIST(0, &(yylsp[-1])), NEW_LVAR(idFWD_KWREST, &(yylsp[-1])));
			    NODE *block = NEW_BLOCK_PASS(NEW_LVAR(idFWD_BLOCK, &(yylsp[-1])), &(yylsp[-1]));
			    (yyval.val) = arg_append(p, splat, new_hash(p, kwrest, &(yylsp[-1])), &(yylsp[-1]));
			    (yyval.val) = arg_blk_pass((yyval.val), block);
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(arg_paren,v1);(yyval.val)=v2;}
			}
		    }
#line 7924 "ripper.c" /* yacc.c:1646  */
    break;

  case 268:
#line 2439 "ripper.y" /* yacc.c:1646  */
    {
		      (yyval.val) = (yyvsp[-1].val);
		    }
#line 7932 "ripper.c" /* yacc.c:1646  */
    break;

  case 269:
#line 2443 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = (yyvsp[-1].val) ? arg_append(p, (yyvsp[-3].val), new_hash(p, (yyvsp[-1].val), &(yylsp[-1])), &(yyloc)) : (yyvsp[-3].val);
#endif
			{VALUE v1,v2,v3,v4,v5;v1=(yyvsp[-1].val);v2=dispatch1(bare_assoc_hash,v1);v3=(yyvsp[-3].val);v4=v2;v5=dispatch2(args_add,v3,v4);(yyval.val)=v5;}
		    }
#line 7943 "ripper.c" /* yacc.c:1646  */
    break;

  case 270:
#line 2450 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = (yyvsp[-1].val) ? NEW_LIST(new_hash(p, (yyvsp[-1].val), &(yylsp[-1])), &(yylsp[-1])) : 0;
#endif
			{VALUE v1,v2,v3,v4,v5,v6;v1=dispatch0(args_new);v2=(yyvsp[-1].val);v3=dispatch1(bare_assoc_hash,v2);v4=v1;v5=v3;v6=dispatch2(args_add,v4,v5);(yyval.val)=v6;}
		    }
#line 7954 "ripper.c" /* yacc.c:1646  */
    break;

  case 271:
#line 2459 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			value_expr((yyvsp[0].val));
			(yyval.val) = NEW_LIST((yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=dispatch0(args_new);v2=v1;v3=(yyvsp[0].val);v4=dispatch2(args_add,v2,v3);(yyval.val)=v4;}
		    }
#line 7966 "ripper.c" /* yacc.c:1646  */
    break;

  case 272:
#line 2467 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = arg_blk_pass((yyvsp[-1].val), (yyvsp[0].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-1].val);v2=(yyvsp[0].val);v3=dispatch2(args_add_block,v1,v2);(yyval.val)=v3;}
		    }
#line 7977 "ripper.c" /* yacc.c:1646  */
    break;

  case 273:
#line 2474 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = (yyvsp[-1].val) ? NEW_LIST(new_hash(p, (yyvsp[-1].val), &(yylsp[-1])), &(yylsp[-1])) : 0;
			(yyval.val) = arg_blk_pass((yyval.val), (yyvsp[0].val));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7,v8,v9;v1=dispatch0(args_new);v2=(yyvsp[-1].val);v3=dispatch1(bare_assoc_hash,v2);v4=v1;v5=v3;v6=dispatch2(args_add,v4,v5);v7=v6;v8=(yyvsp[0].val);v9=dispatch2(args_add_block,v7,v8);(yyval.val)=v9;}
		    }
#line 7989 "ripper.c" /* yacc.c:1646  */
    break;

  case 274:
#line 2482 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = (yyvsp[-1].val) ? arg_append(p, (yyvsp[-3].val), new_hash(p, (yyvsp[-1].val), &(yylsp[-1])), &(yyloc)) : (yyvsp[-3].val);
			(yyval.val) = arg_blk_pass((yyval.val), (yyvsp[0].val));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7,v8;v1=(yyvsp[-1].val);v2=dispatch1(bare_assoc_hash,v1);v3=(yyvsp[-3].val);v4=v2;v5=dispatch2(args_add,v3,v4);v6=v5;v7=(yyvsp[0].val);v8=dispatch2(args_add_block,v6,v7);(yyval.val)=v8;}
		    }
#line 8001 "ripper.c" /* yacc.c:1646  */
    break;

  case 275:
#line 2490 "ripper.y" /* yacc.c:1646  */
    {{VALUE v1,v2,v3,v4;v1=dispatch0(args_new);v2=v1;v3=(yyvsp[0].val);v4=dispatch2(args_add_block,v2,v3);(yyval.val)=v4;}}
#line 8007 "ripper.c" /* yacc.c:1646  */
    break;

  case 276:
#line 2493 "ripper.y" /* yacc.c:1646  */
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
#line 8029 "ripper.c" /* yacc.c:1646  */
    break;

  case 277:
#line 2511 "ripper.y" /* yacc.c:1646  */
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
			(yyval.val) = (yyvsp[0].val);
		    }
#line 8051 "ripper.c" /* yacc.c:1646  */
    break;

  case 278:
#line 2531 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_BLOCK_PASS((yyvsp[0].val), &(yyloc));
#endif
			(yyval.val)=(yyvsp[0].val);
		    }
#line 8062 "ripper.c" /* yacc.c:1646  */
    break;

  case 279:
#line 2540 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = (yyvsp[0].val);
		    }
#line 8070 "ripper.c" /* yacc.c:1646  */
    break;

  case 280:
#line 2544 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = 0;
		    }
#line 8078 "ripper.c" /* yacc.c:1646  */
    break;

  case 281:
#line 2550 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_LIST((yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=dispatch0(args_new);v2=v1;v3=(yyvsp[0].val);v4=dispatch2(args_add,v2,v3);(yyval.val)=v4;}
		    }
#line 8089 "ripper.c" /* yacc.c:1646  */
    break;

  case 282:
#line 2557 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_SPLAT((yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=dispatch0(args_new);v2=v1;v3=(yyvsp[0].val);v4=dispatch2(args_add_star,v2,v3);(yyval.val)=v4;}
		    }
#line 8100 "ripper.c" /* yacc.c:1646  */
    break;

  case 283:
#line 2564 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = last_arg_append(p, (yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(args_add,v1,v2);(yyval.val)=v3;}
		    }
#line 8111 "ripper.c" /* yacc.c:1646  */
    break;

  case 284:
#line 2571 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = rest_arg_append(p, (yyvsp[-3].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-3].val);v2=(yyvsp[0].val);v3=dispatch2(args_add_star,v1,v2);(yyval.val)=v3;}
		    }
#line 8122 "ripper.c" /* yacc.c:1646  */
    break;

  case 287:
#line 2584 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = last_arg_append(p, (yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5;v1=(yyvsp[-2].val);v2=dispatch1(mrhs_new_from_args,v1);v3=v2;v4=(yyvsp[0].val);v5=dispatch2(mrhs_add,v3,v4);(yyval.val)=v5;}
		    }
#line 8133 "ripper.c" /* yacc.c:1646  */
    break;

  case 288:
#line 2591 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = rest_arg_append(p, (yyvsp[-3].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5;v1=(yyvsp[-3].val);v2=dispatch1(mrhs_new_from_args,v1);v3=v2;v4=(yyvsp[0].val);v5=dispatch2(mrhs_add_star,v3,v4);(yyval.val)=v5;}
		    }
#line 8144 "ripper.c" /* yacc.c:1646  */
    break;

  case 289:
#line 2598 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_SPLAT((yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=dispatch0(mrhs_new);v2=v1;v3=(yyvsp[0].val);v4=dispatch2(mrhs_add_star,v2,v3);(yyval.val)=v4;}
		    }
#line 8155 "ripper.c" /* yacc.c:1646  */
    break;

  case 300:
#line 2617 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_FCALL((yyvsp[0].val), 0, &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6;v1=(yyvsp[0].val);v2=dispatch1(fcall,v1);v3=dispatch0(args_new);v4=v2;v5=v3;v6=dispatch2(method_add_arg,v4,v5);(yyval.val)=v6;}
		    }
#line 8166 "ripper.c" /* yacc.c:1646  */
    break;

  case 301:
#line 2624 "ripper.y" /* yacc.c:1646  */
    {
			CMDARG_PUSH(0);
		    }
#line 8174 "ripper.c" /* yacc.c:1646  */
    break;

  case 302:
#line 2629 "ripper.y" /* yacc.c:1646  */
    {
			CMDARG_POP();
#if 0
			set_line_body((yyvsp[-1].val), (yylsp[-3]).end_pos.lineno);
			(yyval.val) = NEW_BEGIN((yyvsp[-1].val), &(yyloc));
			nd_set_line((yyval.val), (yylsp[-3]).end_pos.lineno);
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(begin,v1);(yyval.val)=v2;}
		    }
#line 8188 "ripper.c" /* yacc.c:1646  */
    break;

  case 303:
#line 2638 "ripper.y" /* yacc.c:1646  */
    {SET_LEX_STATE(EXPR_ENDARG);}
#line 8194 "ripper.c" /* yacc.c:1646  */
    break;

  case 304:
#line 2639 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_BEGIN(0, &(yyloc));
#endif
			{VALUE v1,v2;v1=0;v2=dispatch1(paren,v1);(yyval.val)=v2;}
		    }
#line 8205 "ripper.c" /* yacc.c:1646  */
    break;

  case 305:
#line 2645 "ripper.y" /* yacc.c:1646  */
    {SET_LEX_STATE(EXPR_ENDARG);}
#line 8211 "ripper.c" /* yacc.c:1646  */
    break;

  case 306:
#line 2646 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			if (nd_type((yyvsp[-2].val)) == NODE_SELF) (yyvsp[-2].val)->nd_state = 0;
			(yyval.val) = (yyvsp[-2].val);
#endif
			{VALUE v1,v2;v1=(yyvsp[-2].val);v2=dispatch1(paren,v1);(yyval.val)=v2;}
		    }
#line 8223 "ripper.c" /* yacc.c:1646  */
    break;

  case 307:
#line 2654 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			if (nd_type((yyvsp[-1].val)) == NODE_SELF) (yyvsp[-1].val)->nd_state = 0;
			(yyval.val) = (yyvsp[-1].val);
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(paren,v1);(yyval.val)=v2;}
		    }
#line 8235 "ripper.c" /* yacc.c:1646  */
    break;

  case 308:
#line 2662 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_COLON2((yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(const_path_ref,v1,v2);(yyval.val)=v3;}
		    }
#line 8246 "ripper.c" /* yacc.c:1646  */
    break;

  case 309:
#line 2669 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_COLON3((yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(top_const_ref,v1);(yyval.val)=v2;}
		    }
#line 8257 "ripper.c" /* yacc.c:1646  */
    break;

  case 310:
#line 2676 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = make_list((yyvsp[-1].val), &(yyloc));
#endif
			{VALUE v1,v2;v1=escape_Qundef((yyvsp[-1].val));v2=dispatch1(array,v1);(yyval.val)=v2;}
		    }
#line 8268 "ripper.c" /* yacc.c:1646  */
    break;

  case 311:
#line 2683 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = new_hash(p, (yyvsp[-1].val), &(yyloc));
			(yyval.val)->nd_brace = TRUE;
#endif
			{VALUE v1,v2;v1=escape_Qundef((yyvsp[-1].val));v2=dispatch1(hash,v1);(yyval.val)=v2;}
		    }
#line 8280 "ripper.c" /* yacc.c:1646  */
    break;

  case 312:
#line 2691 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_RETURN(0, &(yyloc));
#endif
			{VALUE v1;v1=dispatch0(return0);(yyval.val)=v1;}
		    }
#line 8291 "ripper.c" /* yacc.c:1646  */
    break;

  case 313:
#line 2698 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = new_yield(p, (yyvsp[-1].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-1].val);v2=dispatch1(paren,v1);v3=v2;v4=dispatch1(yield,v3);(yyval.val)=v4;}
		    }
#line 8302 "ripper.c" /* yacc.c:1646  */
    break;

  case 314:
#line 2705 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_YIELD(0, &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5;v1=dispatch0(args_new);v2=v1;v3=dispatch1(paren,v2);v4=v3;v5=dispatch1(yield,v4);(yyval.val)=v5;}
		    }
#line 8313 "ripper.c" /* yacc.c:1646  */
    break;

  case 315:
#line 2712 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_YIELD(0, &(yyloc));
#endif
			{VALUE v1;v1=dispatch0(yield0);(yyval.val)=v1;}
		    }
#line 8324 "ripper.c" /* yacc.c:1646  */
    break;

  case 316:
#line 2718 "ripper.y" /* yacc.c:1646  */
    {p->in_defined = 1;}
#line 8330 "ripper.c" /* yacc.c:1646  */
    break;

  case 317:
#line 2719 "ripper.y" /* yacc.c:1646  */
    {
			p->in_defined = 0;
			(yyval.val) = new_defined(p, (yyvsp[-1].val), &(yyloc));
		    }
#line 8339 "ripper.c" /* yacc.c:1646  */
    break;

  case 318:
#line 2724 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = call_uni_op(p, method_cond(p, (yyvsp[-1].val), &(yylsp[-1])), METHOD_NOT, &(yylsp[-3]), &(yyloc));
		    }
#line 8347 "ripper.c" /* yacc.c:1646  */
    break;

  case 319:
#line 2728 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = call_uni_op(p, method_cond(p, new_nil(&(yylsp[-1])), &(yylsp[-1])), METHOD_NOT, &(yylsp[-2]), &(yyloc));
		    }
#line 8355 "ripper.c" /* yacc.c:1646  */
    break;

  case 320:
#line 2732 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = method_add_block(p, (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7,v8,v9;v1=(yyvsp[-1].val);v2=dispatch1(fcall,v1);v3=dispatch0(args_new);v4=v2;v5=v3;v6=dispatch2(method_add_arg,v4,v5);v7=v6;v8=(yyvsp[0].val);v9=dispatch2(method_add_block,v7,v8);(yyval.val)=v9;}
		    }
#line 8366 "ripper.c" /* yacc.c:1646  */
    break;

  case 322:
#line 2740 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			block_dup_check(p, (yyvsp[-1].val)->nd_args, (yyvsp[0].val));
			(yyval.val) = method_add_block(p, (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-1].val);v2=(yyvsp[0].val);v3=dispatch2(method_add_block,v1,v2);(yyval.val)=v3;}
		    }
#line 8378 "ripper.c" /* yacc.c:1646  */
    break;

  case 323:
#line 2748 "ripper.y" /* yacc.c:1646  */
    {
			token_info_push(p, "->", &(yylsp[0]));
		    }
#line 8386 "ripper.c" /* yacc.c:1646  */
    break;

  case 324:
#line 2752 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = (yyvsp[0].val);
#if 0
                        nd_set_first_loc((yyval.val), (yylsp[-2]).beg_pos);
#endif
		    }
#line 8397 "ripper.c" /* yacc.c:1646  */
    break;

  case 325:
#line 2762 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = new_if(p, (yyvsp[-4].val), (yyvsp[-2].val), (yyvsp[-1].val), &(yyloc));
			fixpos((yyval.val), (yyvsp[-4].val));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-4].val);v2=(yyvsp[-2].val);v3=escape_Qundef((yyvsp[-1].val));v4=dispatch3(if,v1,v2,v3);(yyval.val)=v4;}
		    }
#line 8409 "ripper.c" /* yacc.c:1646  */
    break;

  case 326:
#line 2773 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = new_unless(p, (yyvsp[-4].val), (yyvsp[-2].val), (yyvsp[-1].val), &(yyloc));
			fixpos((yyval.val), (yyvsp[-4].val));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-4].val);v2=(yyvsp[-2].val);v3=escape_Qundef((yyvsp[-1].val));v4=dispatch3(unless,v1,v2,v3);(yyval.val)=v4;}
		    }
#line 8421 "ripper.c" /* yacc.c:1646  */
    break;

  case 327:
#line 2783 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_WHILE(cond(p, (yyvsp[-2].val), &(yylsp[-2])), (yyvsp[-1].val), 1, &(yyloc));
			fixpos((yyval.val), (yyvsp[-2].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[-1].val);v3=dispatch2(while,v1,v2);(yyval.val)=v3;}
		    }
#line 8433 "ripper.c" /* yacc.c:1646  */
    break;

  case 328:
#line 2793 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_UNTIL(cond(p, (yyvsp[-2].val), &(yylsp[-2])), (yyvsp[-1].val), 1, &(yyloc));
			fixpos((yyval.val), (yyvsp[-2].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[-1].val);v3=dispatch2(until,v1,v2);(yyval.val)=v3;}
		    }
#line 8445 "ripper.c" /* yacc.c:1646  */
    break;

  case 329:
#line 2801 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = p->case_labels;
			p->case_labels = Qnil;
		    }
#line 8454 "ripper.c" /* yacc.c:1646  */
    break;

  case 330:
#line 2807 "ripper.y" /* yacc.c:1646  */
    {
			if (RTEST(p->case_labels)) rb_hash_clear(p->case_labels);
			p->case_labels = (yyvsp[-2].val);
#if 0
			(yyval.val) = NEW_CASE((yyvsp[-4].val), (yyvsp[-1].val), &(yyloc));
			fixpos((yyval.val), (yyvsp[-4].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-4].val);v2=(yyvsp[-1].val);v3=dispatch2(case,v1,v2);(yyval.val)=v3;}
		    }
#line 8468 "ripper.c" /* yacc.c:1646  */
    break;

  case 331:
#line 2817 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = p->case_labels;
			p->case_labels = 0;
		    }
#line 8477 "ripper.c" /* yacc.c:1646  */
    break;

  case 332:
#line 2823 "ripper.y" /* yacc.c:1646  */
    {
			if (RTEST(p->case_labels)) rb_hash_clear(p->case_labels);
			p->case_labels = (yyvsp[-2].val);
#if 0
			(yyval.val) = NEW_CASE2((yyvsp[-1].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=Qnil;v2=(yyvsp[-1].val);v3=dispatch2(case,v1,v2);(yyval.val)=v3;}
		    }
#line 8490 "ripper.c" /* yacc.c:1646  */
    break;

  case 333:
#line 2834 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_CASE3((yyvsp[-3].val), (yyvsp[-1].val), &(yyloc));
			rb_warn0L(nd_line((yyval.val)), "Pattern matching is experimental, and the behavior may change in future versions of Ruby!");
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-3].val);v2=(yyvsp[-1].val);v3=dispatch2(case,v1,v2);(yyval.val)=v3;}
		    }
#line 8502 "ripper.c" /* yacc.c:1646  */
    break;

  case 334:
#line 2844 "ripper.y" /* yacc.c:1646  */
    {
#if 0
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
			VALUE tmpbuf = rb_imemo_tmpbuf_auto_free_pointer();
			ID *tbl = ALLOC_N(ID, 3);
			rb_imemo_tmpbuf_set_ptr(tmpbuf, tbl);
			tbl[0] = 1 /* length of local var table */; tbl[1] = id /* internal id */;
                        tbl[2] = tmpbuf;

			switch (nd_type((yyvsp[-4].val))) {
			  case NODE_LASGN:
			  case NODE_DASGN:
			  case NODE_DASGN_CURR: /* e.each {|internal_var| a = internal_var; ... } */
			    (yyvsp[-4].val)->nd_value = internal_var;
			    id = 0;
			    m->nd_plen = 1;
			    m->nd_next = (yyvsp[-4].val);
			    break;
			  case NODE_MASGN: /* e.each {|*internal_var| a, b, c = (internal_var.length == 1 && Array === (tmp = internal_var[0]) ? tmp : internal_var); ... } */
			    m->nd_next = node_assign(p, (yyvsp[-4].val), NEW_FOR_MASGN(internal_var, &(yylsp[-4])), &(yylsp[-4]));
			    break;
			  default: /* e.each {|*internal_var| @a, B, c[1], d.attr = internal_val; ... } */
			    m->nd_next = node_assign(p, NEW_MASGN(NEW_LIST((yyvsp[-4].val), &(yylsp[-4])), 0, &(yylsp[-4])), internal_var, &(yylsp[-4]));
			}
			/* {|*internal_id| <m> = internal_id; ... } */
			args = new_args(p, m, 0, id, 0, new_args_tail(p, 0, 0, 0, &(yylsp[-4])), &(yylsp[-4]));
			scope = NEW_NODE(NODE_SCOPE, tbl, (yyvsp[-1].val), args, &(yyloc));
                        RB_OBJ_WRITTEN(p->ast, Qnil, tmpbuf);
			(yyval.val) = NEW_FOR((yyvsp[-2].val), scope, &(yyloc));
			fixpos((yyval.val), (yyvsp[-4].val));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-4].val);v2=(yyvsp[-2].val);v3=(yyvsp[-1].val);v4=dispatch3(for,v1,v2,v3);(yyval.val)=v4;}
		    }
#line 8551 "ripper.c" /* yacc.c:1646  */
    break;

  case 335:
#line 2889 "ripper.y" /* yacc.c:1646  */
    {
			if (p->in_def) {
			    YYLTYPE loc = code_loc_gen(&(yylsp[-2]), &(yylsp[-1]));
			    yyerror1(&loc, "class definition in method body");
			}
			(yyvsp[-2].num) = p->in_class;
			p->in_class = 1;
			local_push(p, 0);
		    }
#line 8565 "ripper.c" /* yacc.c:1646  */
    break;

  case 336:
#line 2900 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_CLASS((yyvsp[-4].val), (yyvsp[-1].val), (yyvsp[-3].val), &(yyloc));
			nd_set_line((yyval.val)->nd_body, (yylsp[0]).end_pos.lineno);
			set_line_body((yyvsp[-1].val), (yylsp[-3]).end_pos.lineno);
			nd_set_line((yyval.val), (yylsp[-3]).end_pos.lineno);
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-4].val);v2=(yyvsp[-3].val);v3=(yyvsp[-1].val);v4=dispatch3(class,v1,v2,v3);(yyval.val)=v4;}
			local_pop(p);
			p->in_class = (yyvsp[-5].num) & 1;
		    }
#line 8581 "ripper.c" /* yacc.c:1646  */
    break;

  case 337:
#line 2912 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.num) = (p->in_class << 1) | p->in_def;
			p->in_def = 0;
			p->in_class = 0;
			local_push(p, 0);
		    }
#line 8592 "ripper.c" /* yacc.c:1646  */
    break;

  case 338:
#line 2921 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_SCLASS((yyvsp[-4].val), (yyvsp[-1].val), &(yyloc));
			nd_set_line((yyval.val)->nd_body, (yylsp[0]).end_pos.lineno);
			set_line_body((yyvsp[-1].val), nd_line((yyvsp[-4].val)));
			fixpos((yyval.val), (yyvsp[-4].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-4].val);v2=(yyvsp[-1].val);v3=dispatch2(sclass,v1,v2);(yyval.val)=v3;}
			local_pop(p);
			p->in_def = (yyvsp[-3].num) & 1;
			p->in_class = ((yyvsp[-3].num) >> 1) & 1;
		    }
#line 8609 "ripper.c" /* yacc.c:1646  */
    break;

  case 339:
#line 2934 "ripper.y" /* yacc.c:1646  */
    {
			if (p->in_def) {
			    YYLTYPE loc = code_loc_gen(&(yylsp[-1]), &(yylsp[0]));
			    yyerror1(&loc, "module definition in method body");
			}
			(yyvsp[-1].num) = p->in_class;
			p->in_class = 1;
			local_push(p, 0);
		    }
#line 8623 "ripper.c" /* yacc.c:1646  */
    break;

  case 340:
#line 2945 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_MODULE((yyvsp[-3].val), (yyvsp[-1].val), &(yyloc));
			nd_set_line((yyval.val)->nd_body, (yylsp[0]).end_pos.lineno);
			set_line_body((yyvsp[-1].val), (yylsp[-3]).end_pos.lineno);
			nd_set_line((yyval.val), (yylsp[-3]).end_pos.lineno);
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-3].val);v2=(yyvsp[-1].val);v3=dispatch2(module,v1,v2);(yyval.val)=v3;}
			local_pop(p);
			p->in_class = (yyvsp[-4].num) & 1;
		    }
#line 8639 "ripper.c" /* yacc.c:1646  */
    break;

  case 341:
#line 2957 "ripper.y" /* yacc.c:1646  */
    {
			local_push(p, 0);
			(yyval.id) = p->cur_arg;
			p->cur_arg = 0;
		    }
#line 8649 "ripper.c" /* yacc.c:1646  */
    break;

  case 342:
#line 2962 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.num) = p->in_def;
			p->in_def = 1;
		    }
#line 8658 "ripper.c" /* yacc.c:1646  */
    break;

  case 343:
#line 2969 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			NODE *body = remove_begin((yyvsp[-1].val));
			reduce_nodes(p, &body);
			(yyval.val) = NEW_DEFN((yyvsp[-5].val), (yyvsp[-2].val), body, &(yyloc));
			nd_set_line((yyval.val)->nd_defn, (yylsp[0]).end_pos.lineno);
			set_line_body(body, (yylsp[-6]).beg_pos.lineno);
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-5].val);v2=(yyvsp[-2].val);v3=(yyvsp[-1].val);v4=dispatch3(def,v1,v2,v3);(yyval.val)=v4;}
			local_pop(p);
			p->in_def = (yyvsp[-3].num) & 1;
			p->cur_arg = (yyvsp[-4].id);
		    }
#line 8676 "ripper.c" /* yacc.c:1646  */
    break;

  case 344:
#line 2982 "ripper.y" /* yacc.c:1646  */
    {SET_LEX_STATE(EXPR_FNAME);}
#line 8682 "ripper.c" /* yacc.c:1646  */
    break;

  case 345:
#line 2983 "ripper.y" /* yacc.c:1646  */
    {
			(yyvsp[-1].num) = p->in_def;
			p->in_def = 1;
			SET_LEX_STATE(EXPR_ENDFN|EXPR_LABEL); /* force for args */
			local_push(p, 0);
			(yyval.id) = p->cur_arg;
			p->cur_arg = 0;
		    }
#line 8695 "ripper.c" /* yacc.c:1646  */
    break;

  case 346:
#line 2994 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			NODE *body = remove_begin((yyvsp[-1].val));
			reduce_nodes(p, &body);
			(yyval.val) = NEW_DEFS((yyvsp[-7].val), (yyvsp[-4].val), (yyvsp[-2].val), body, &(yyloc));
			nd_set_line((yyval.val)->nd_defn, (yylsp[0]).end_pos.lineno);
			set_line_body(body, (yylsp[-8]).beg_pos.lineno);
#endif
			{VALUE v1,v2,v3,v4,v5,v6;v1=(yyvsp[-7].val);v2=(yyvsp[-6].val);v3=(yyvsp[-4].val);v4=(yyvsp[-2].val);v5=(yyvsp[-1].val);v6=dispatch5(defs,v1,v2,v3,v4,v5);(yyval.val)=v6;}
			local_pop(p);
			p->in_def = (yyvsp[-5].num) & 1;
			p->cur_arg = (yyvsp[-3].id);
		    }
#line 8713 "ripper.c" /* yacc.c:1646  */
    break;

  case 347:
#line 3008 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_BREAK(0, &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=dispatch0(args_new);v2=v1;v3=dispatch1(break,v2);(yyval.val)=v3;}
		    }
#line 8724 "ripper.c" /* yacc.c:1646  */
    break;

  case 348:
#line 3015 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_NEXT(0, &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=dispatch0(args_new);v2=v1;v3=dispatch1(next,v2);(yyval.val)=v3;}
		    }
#line 8735 "ripper.c" /* yacc.c:1646  */
    break;

  case 349:
#line 3022 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_REDO(&(yyloc));
#endif
			{VALUE v1;v1=dispatch0(redo);(yyval.val)=v1;}
		    }
#line 8746 "ripper.c" /* yacc.c:1646  */
    break;

  case 350:
#line 3029 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_RETRY(&(yyloc));
#endif
			{VALUE v1;v1=dispatch0(retry);(yyval.val)=v1;}
		    }
#line 8757 "ripper.c" /* yacc.c:1646  */
    break;

  case 351:
#line 3036 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_METHREF((yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(methref,v1,v2);(yyval.val)=v3;}
		    }
#line 8768 "ripper.c" /* yacc.c:1646  */
    break;

  case 352:
#line 3045 "ripper.y" /* yacc.c:1646  */
    {
			value_expr((yyvsp[0].val));
			(yyval.val) = (yyvsp[0].val);
		    }
#line 8777 "ripper.c" /* yacc.c:1646  */
    break;

  case 353:
#line 3052 "ripper.y" /* yacc.c:1646  */
    {
			token_info_push(p, "begin", &(yyloc));
		    }
#line 8785 "ripper.c" /* yacc.c:1646  */
    break;

  case 354:
#line 3058 "ripper.y" /* yacc.c:1646  */
    {
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
#line 8803 "ripper.c" /* yacc.c:1646  */
    break;

  case 355:
#line 3074 "ripper.y" /* yacc.c:1646  */
    {
			token_info_push(p, "unless", &(yyloc));
		    }
#line 8811 "ripper.c" /* yacc.c:1646  */
    break;

  case 356:
#line 3080 "ripper.y" /* yacc.c:1646  */
    {
			token_info_push(p, "while", &(yyloc));
		    }
#line 8819 "ripper.c" /* yacc.c:1646  */
    break;

  case 357:
#line 3086 "ripper.y" /* yacc.c:1646  */
    {
			token_info_push(p, "until", &(yyloc));
		    }
#line 8827 "ripper.c" /* yacc.c:1646  */
    break;

  case 358:
#line 3092 "ripper.y" /* yacc.c:1646  */
    {
			token_info_push(p, "case", &(yyloc));
		    }
#line 8835 "ripper.c" /* yacc.c:1646  */
    break;

  case 359:
#line 3098 "ripper.y" /* yacc.c:1646  */
    {
			token_info_push(p, "for", &(yyloc));
		    }
#line 8843 "ripper.c" /* yacc.c:1646  */
    break;

  case 360:
#line 3104 "ripper.y" /* yacc.c:1646  */
    {
			token_info_push(p, "class", &(yyloc));
		    }
#line 8851 "ripper.c" /* yacc.c:1646  */
    break;

  case 361:
#line 3110 "ripper.y" /* yacc.c:1646  */
    {
			token_info_push(p, "module", &(yyloc));
		    }
#line 8859 "ripper.c" /* yacc.c:1646  */
    break;

  case 362:
#line 3116 "ripper.y" /* yacc.c:1646  */
    {
			token_info_push(p, "def", &(yyloc));
		    }
#line 8867 "ripper.c" /* yacc.c:1646  */
    break;

  case 363:
#line 3122 "ripper.y" /* yacc.c:1646  */
    {
			token_info_push(p, "do", &(yyloc));
		    }
#line 8875 "ripper.c" /* yacc.c:1646  */
    break;

  case 364:
#line 3128 "ripper.y" /* yacc.c:1646  */
    {
			token_info_push(p, "do", &(yyloc));
		    }
#line 8883 "ripper.c" /* yacc.c:1646  */
    break;

  case 365:
#line 3134 "ripper.y" /* yacc.c:1646  */
    {
			token_info_warn(p, "rescue", p->token_info, 1, &(yyloc));
		    }
#line 8891 "ripper.c" /* yacc.c:1646  */
    break;

  case 366:
#line 3140 "ripper.y" /* yacc.c:1646  */
    {
			token_info_warn(p, "ensure", p->token_info, 1, &(yyloc));
		    }
#line 8899 "ripper.c" /* yacc.c:1646  */
    break;

  case 367:
#line 3146 "ripper.y" /* yacc.c:1646  */
    {
			token_info_warn(p, "when", p->token_info, 0, &(yyloc));
		    }
#line 8907 "ripper.c" /* yacc.c:1646  */
    break;

  case 368:
#line 3152 "ripper.y" /* yacc.c:1646  */
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
#line 8924 "ripper.c" /* yacc.c:1646  */
    break;

  case 369:
#line 3167 "ripper.y" /* yacc.c:1646  */
    {
			token_info_warn(p, "elsif", p->token_info, 1, &(yyloc));
		    }
#line 8932 "ripper.c" /* yacc.c:1646  */
    break;

  case 370:
#line 3173 "ripper.y" /* yacc.c:1646  */
    {
			token_info_pop(p, "end", &(yyloc));
		    }
#line 8940 "ripper.c" /* yacc.c:1646  */
    break;

  case 371:
#line 3179 "ripper.y" /* yacc.c:1646  */
    {
			if (p->in_class && !p->in_def && !dyna_in_block(p))
			    yyerror1(&(yylsp[0]), "Invalid return in class/module body");
		    }
#line 8949 "ripper.c" /* yacc.c:1646  */
    break;

  case 378:
#line 3198 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = new_if(p, (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
			fixpos((yyval.val), (yyvsp[-3].val));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-3].val);v2=(yyvsp[-1].val);v3=escape_Qundef((yyvsp[0].val));v4=dispatch3(elsif,v1,v2,v3);(yyval.val)=v4;}
		    }
#line 8961 "ripper.c" /* yacc.c:1646  */
    break;

  case 380:
#line 3209 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = (yyvsp[0].val);
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(else,v1);(yyval.val)=v2;}
		    }
#line 8972 "ripper.c" /* yacc.c:1646  */
    break;

  case 383:
#line 3222 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = assignable(p, (yyvsp[0].val), 0, &(yyloc));
			mark_lvar_used(p, (yyval.val));
#endif
			(yyval.val)=assignable(p, (yyvsp[0].val));
		    }
#line 8984 "ripper.c" /* yacc.c:1646  */
    break;

  case 384:
#line 3230 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = (yyvsp[-1].val);
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(mlhs_paren,v1);(yyval.val)=v2;}
		    }
#line 8995 "ripper.c" /* yacc.c:1646  */
    break;

  case 385:
#line 3239 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_LIST((yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=dispatch0(mlhs_new);v2=v1;v3=(yyvsp[0].val);v4=dispatch2(mlhs_add,v2,v3);(yyval.val)=v4;}
		    }
#line 9006 "ripper.c" /* yacc.c:1646  */
    break;

  case 386:
#line 3246 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = list_append(p, (yyvsp[-2].val), (yyvsp[0].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(mlhs_add,v1,v2);(yyval.val)=v3;}
		    }
#line 9017 "ripper.c" /* yacc.c:1646  */
    break;

  case 387:
#line 3255 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_MASGN((yyvsp[0].val), 0, &(yyloc));
#endif
			(yyval.val)=(yyvsp[0].val);
		    }
#line 9028 "ripper.c" /* yacc.c:1646  */
    break;

  case 388:
#line 3262 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_MASGN((yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(mlhs_add_star,v1,v2);(yyval.val)=v3;}
		    }
#line 9039 "ripper.c" /* yacc.c:1646  */
    break;

  case 389:
#line 3269 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_MASGN((yyvsp[-4].val), NEW_POSTARG((yyvsp[-2].val), (yyvsp[0].val), &(yyloc)), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6;v1=(yyvsp[-4].val);v2=(yyvsp[-2].val);v3=dispatch2(mlhs_add_star,v1,v2);v4=v3;v5=(yyvsp[0].val);v6=dispatch2(mlhs_add_post,v4,v5);(yyval.val)=v6;}
		    }
#line 9050 "ripper.c" /* yacc.c:1646  */
    break;

  case 390:
#line 3276 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_MASGN(0, (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=dispatch0(mlhs_new);v2=v1;v3=(yyvsp[0].val);v4=dispatch2(mlhs_add_star,v2,v3);(yyval.val)=v4;}
		    }
#line 9061 "ripper.c" /* yacc.c:1646  */
    break;

  case 391:
#line 3283 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_MASGN(0, NEW_POSTARG((yyvsp[-2].val), (yyvsp[0].val), &(yyloc)), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7;v1=dispatch0(mlhs_new);v2=v1;v3=(yyvsp[-2].val);v4=dispatch2(mlhs_add_star,v2,v3);v5=v4;v6=(yyvsp[0].val);v7=dispatch2(mlhs_add_post,v5,v6);(yyval.val)=v7;}
		    }
#line 9072 "ripper.c" /* yacc.c:1646  */
    break;

  case 392:
#line 3292 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = assignable(p, (yyvsp[0].val), 0, &(yyloc));
			mark_lvar_used(p, (yyval.val));
#endif
			(yyval.val)=assignable(p, (yyvsp[0].val));
		    }
#line 9084 "ripper.c" /* yacc.c:1646  */
    break;

  case 393:
#line 3300 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NODE_SPECIAL_NO_NAME_REST;
#endif
			(yyval.val)=Qnil;
		    }
#line 9095 "ripper.c" /* yacc.c:1646  */
    break;

  case 394:
#line 3309 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args_tail(p, (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-1]));
		    }
#line 9103 "ripper.c" /* yacc.c:1646  */
    break;

  case 395:
#line 3313 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args_tail(p, (yyvsp[-1].val), Qnone, (yyvsp[0].val), &(yylsp[-1]));
		    }
#line 9111 "ripper.c" /* yacc.c:1646  */
    break;

  case 396:
#line 3317 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args_tail(p, Qnone, (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-1]));
		    }
#line 9119 "ripper.c" /* yacc.c:1646  */
    break;

  case 397:
#line 3321 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args_tail(p, Qnone, ID2VAL(idNil), (yyvsp[0].val), &(yylsp[-1]));
		    }
#line 9127 "ripper.c" /* yacc.c:1646  */
    break;

  case 398:
#line 3325 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args_tail(p, Qnone, Qnone, (yyvsp[0].val), &(yylsp[0]));
		    }
#line 9135 "ripper.c" /* yacc.c:1646  */
    break;

  case 399:
#line 3331 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = (yyvsp[0].val);
		    }
#line 9143 "ripper.c" /* yacc.c:1646  */
    break;

  case 400:
#line 3335 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args_tail(p, Qnone, Qnone, Qnone, &(yylsp[0]));
		    }
#line 9151 "ripper.c" /* yacc.c:1646  */
    break;

  case 401:
#line 3341 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args(p, (yyvsp[-5].val), (yyvsp[-3].val), (yyvsp[-1].val), Qnone, (yyvsp[0].val), &(yyloc));
		    }
#line 9159 "ripper.c" /* yacc.c:1646  */
    break;

  case 402:
#line 3345 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args(p, (yyvsp[-7].val), (yyvsp[-5].val), (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 9167 "ripper.c" /* yacc.c:1646  */
    break;

  case 403:
#line 3349 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args(p, (yyvsp[-3].val), (yyvsp[-1].val), Qnone, Qnone, (yyvsp[0].val), &(yyloc));
		    }
#line 9175 "ripper.c" /* yacc.c:1646  */
    break;

  case 404:
#line 3353 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args(p, (yyvsp[-5].val), (yyvsp[-3].val), Qnone, (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 9183 "ripper.c" /* yacc.c:1646  */
    break;

  case 405:
#line 3357 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args(p, (yyvsp[-3].val), Qnone, (yyvsp[-1].val), Qnone, (yyvsp[0].val), &(yyloc));
		    }
#line 9191 "ripper.c" /* yacc.c:1646  */
    break;

  case 406:
#line 3361 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			/* magic number for rest_id in iseq_set_arguments() */
			(yyval.val) = new_args(p, (yyvsp[-1].val), Qnone, NODE_SPECIAL_EXCESSIVE_COMMA, Qnone, new_args_tail(p, Qnone, Qnone, Qnone, &(yylsp[-1])), &(yyloc));
#endif
			{VALUE v1;v1=dispatch0(excessed_comma);(yyval.val)=new_args(p, (yyvsp[-1].val), Qnone, v1, Qnone, new_args_tail(p, Qnone, Qnone, Qnone, NULL), NULL);}
		    }
#line 9203 "ripper.c" /* yacc.c:1646  */
    break;

  case 407:
#line 3369 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args(p, (yyvsp[-5].val), Qnone, (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 9211 "ripper.c" /* yacc.c:1646  */
    break;

  case 408:
#line 3373 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args(p, (yyvsp[-1].val), Qnone, Qnone, Qnone, (yyvsp[0].val), &(yyloc));
		    }
#line 9219 "ripper.c" /* yacc.c:1646  */
    break;

  case 409:
#line 3377 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args(p, Qnone, (yyvsp[-3].val), (yyvsp[-1].val), Qnone, (yyvsp[0].val), &(yyloc));
		    }
#line 9227 "ripper.c" /* yacc.c:1646  */
    break;

  case 410:
#line 3381 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args(p, Qnone, (yyvsp[-5].val), (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 9235 "ripper.c" /* yacc.c:1646  */
    break;

  case 411:
#line 3385 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args(p, Qnone, (yyvsp[-1].val), Qnone, Qnone, (yyvsp[0].val), &(yyloc));
		    }
#line 9243 "ripper.c" /* yacc.c:1646  */
    break;

  case 412:
#line 3389 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args(p, Qnone, (yyvsp[-3].val), Qnone, (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 9251 "ripper.c" /* yacc.c:1646  */
    break;

  case 413:
#line 3393 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args(p, Qnone, Qnone, (yyvsp[-1].val), Qnone, (yyvsp[0].val), &(yyloc));
		    }
#line 9259 "ripper.c" /* yacc.c:1646  */
    break;

  case 414:
#line 3397 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args(p, Qnone, Qnone, (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 9267 "ripper.c" /* yacc.c:1646  */
    break;

  case 415:
#line 3401 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args(p, Qnone, Qnone, Qnone, Qnone, (yyvsp[0].val), &(yyloc));
		    }
#line 9275 "ripper.c" /* yacc.c:1646  */
    break;

  case 417:
#line 3408 "ripper.y" /* yacc.c:1646  */
    {
			p->command_start = TRUE;
		    }
#line 9283 "ripper.c" /* yacc.c:1646  */
    break;

  case 418:
#line 3414 "ripper.y" /* yacc.c:1646  */
    {
			p->cur_arg = 0;
			p->max_numparam = ORDINAL_PARAM;
#if 0
			(yyval.val) = 0;
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11;v1=Qnil;v2=Qnil;v3=Qnil;v4=Qnil;v5=Qnil;v6=Qnil;v7=Qnil;v8=dispatch7(params,v1,v2,v3,v4,v5,v6,v7);v9=v8;v10=escape_Qundef((yyvsp[-1].val));v11=dispatch2(block_var,v9,v10);(yyval.val)=v11;}
		    }
#line 9296 "ripper.c" /* yacc.c:1646  */
    break;

  case 419:
#line 3423 "ripper.y" /* yacc.c:1646  */
    {
			p->max_numparam = ORDINAL_PARAM;
#if 0
			(yyval.val) = 0;
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11;v1=Qnil;v2=Qnil;v3=Qnil;v4=Qnil;v5=Qnil;v6=Qnil;v7=Qnil;v8=dispatch7(params,v1,v2,v3,v4,v5,v6,v7);v9=v8;v10=Qnil;v11=dispatch2(block_var,v9,v10);(yyval.val)=v11;}
		    }
#line 9308 "ripper.c" /* yacc.c:1646  */
    break;

  case 420:
#line 3431 "ripper.y" /* yacc.c:1646  */
    {
			p->cur_arg = 0;
			p->max_numparam = ORDINAL_PARAM;
#if 0
			(yyval.val) = (yyvsp[-2].val);
#endif
			{VALUE v1,v2,v3;v1=escape_Qundef((yyvsp[-2].val));v2=escape_Qundef((yyvsp[-1].val));v3=dispatch2(block_var,v1,v2);(yyval.val)=v3;}
		    }
#line 9321 "ripper.c" /* yacc.c:1646  */
    break;

  case 421:
#line 3443 "ripper.y" /* yacc.c:1646  */
    {
		      (yyval.val) = 0;
		    }
#line 9329 "ripper.c" /* yacc.c:1646  */
    break;

  case 422:
#line 3447 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = 0;
#endif
			(yyval.val)=(yyvsp[-1].val);
		    }
#line 9340 "ripper.c" /* yacc.c:1646  */
    break;

  case 423:
#line 3456 "ripper.y" /* yacc.c:1646  */
    {(yyval.val)=rb_ary_new3(1, get_value((yyvsp[0].val)));}
#line 9346 "ripper.c" /* yacc.c:1646  */
    break;

  case 424:
#line 3458 "ripper.y" /* yacc.c:1646  */
    {(yyval.val)=rb_ary_push((yyvsp[-2].val), get_value((yyvsp[0].val)));}
#line 9352 "ripper.c" /* yacc.c:1646  */
    break;

  case 425:
#line 3462 "ripper.y" /* yacc.c:1646  */
    {
			new_bv(p, get_id((yyvsp[0].val)));
			(yyval.val)=get_value((yyvsp[0].val));
		    }
#line 9361 "ripper.c" /* yacc.c:1646  */
    break;

  case 426:
#line 3467 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = 0;
		    }
#line 9369 "ripper.c" /* yacc.c:1646  */
    break;

  case 427:
#line 3472 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.vars) = dyna_push(p);
		    }
#line 9377 "ripper.c" /* yacc.c:1646  */
    break;

  case 428:
#line 3475 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.num) = p->lex.lpar_beg;
			p->lex.lpar_beg = p->lex.paren_nest;
		    }
#line 9386 "ripper.c" /* yacc.c:1646  */
    break;

  case 429:
#line 3479 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.num) = p->max_numparam;
			p->max_numparam = 0;
		    }
#line 9395 "ripper.c" /* yacc.c:1646  */
    break;

  case 430:
#line 3483 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.node) = numparam_push(p);
		    }
#line 9403 "ripper.c" /* yacc.c:1646  */
    break;

  case 431:
#line 3487 "ripper.y" /* yacc.c:1646  */
    {
			CMDARG_PUSH(0);
		    }
#line 9411 "ripper.c" /* yacc.c:1646  */
    break;

  case 432:
#line 3491 "ripper.y" /* yacc.c:1646  */
    {
			int max_numparam = p->max_numparam;
			p->lex.lpar_beg = (yyvsp[-5].num);
			p->max_numparam = (yyvsp[-4].num);
			CMDARG_POP();
			(yyvsp[-2].val) = args_with_numbered(p, (yyvsp[-2].val), max_numparam);
#if 0
                        {
                            YYLTYPE loc = code_loc_gen(&(yylsp[-2]), &(yylsp[0]));
                            (yyval.val) = NEW_LAMBDA((yyvsp[-2].val), (yyvsp[0].val), &loc);
                            nd_set_line((yyval.val)->nd_body, (yylsp[0]).end_pos.lineno);
                            nd_set_line((yyval.val), (yylsp[-2]).end_pos.lineno);
                        }
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(lambda,v1,v2);(yyval.val)=v3;}
			numparam_pop(p, (yyvsp[-3].node));
			dyna_pop(p, (yyvsp[-6].vars));
		    }
#line 9434 "ripper.c" /* yacc.c:1646  */
    break;

  case 433:
#line 3512 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = (yyvsp[-2].val);
			p->max_numparam = ORDINAL_PARAM;
#endif
			{VALUE v1,v2;v1=(yyvsp[-2].val);v2=dispatch1(paren,v1);(yyval.val)=v2;}
		    }
#line 9446 "ripper.c" /* yacc.c:1646  */
    break;

  case 434:
#line 3520 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			if (!args_info_empty_p((yyvsp[0].val)->nd_ainfo))
			    p->max_numparam = ORDINAL_PARAM;
#endif
			(yyval.val) = (yyvsp[0].val);
		    }
#line 9458 "ripper.c" /* yacc.c:1646  */
    break;

  case 435:
#line 3530 "ripper.y" /* yacc.c:1646  */
    {
			token_info_pop(p, "}", &(yylsp[0]));
			(yyval.val) = (yyvsp[-1].val);
		    }
#line 9467 "ripper.c" /* yacc.c:1646  */
    break;

  case 436:
#line 3535 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = (yyvsp[-1].val);
		    }
#line 9475 "ripper.c" /* yacc.c:1646  */
    break;

  case 437:
#line 3541 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = (yyvsp[-1].val);
#if 0
			(yyval.val)->nd_body->nd_loc = code_loc_gen(&(yylsp[-2]), &(yylsp[0]));
			nd_set_line((yyval.val), (yylsp[-2]).end_pos.lineno);
#endif
		    }
#line 9487 "ripper.c" /* yacc.c:1646  */
    break;

  case 438:
#line 3551 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			if (nd_type((yyvsp[-1].val)) == NODE_YIELD) {
			    compile_error(p, "block given to yield");
			}
			else {
			    block_dup_check(p, (yyvsp[-1].val)->nd_args, (yyvsp[0].val));
			}
			(yyval.val) = method_add_block(p, (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
			fixpos((yyval.val), (yyvsp[-1].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-1].val);v2=(yyvsp[0].val);v3=dispatch2(method_add_block,v1,v2);(yyval.val)=v3;}
		    }
#line 9505 "ripper.c" /* yacc.c:1646  */
    break;

  case 439:
#line 3565 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = new_qcall(p, (yyvsp[-2].val), (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7;v1=(yyvsp[-3].val);v2=(yyvsp[-2].val);v3=(yyvsp[-1].val);v4=dispatch3(call,v1,v2,v3);v5=v4;v6=(yyvsp[0].val);v7=v6==Qundef ? v5 : dispatch2(method_add_arg,v5,v6);(yyval.val)=v7;}
		    }
#line 9516 "ripper.c" /* yacc.c:1646  */
    break;

  case 440:
#line 3572 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = new_command_qcall(p, (yyvsp[-3].val), (yyvsp[-4].val), (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-2]), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7,v8;v1=(yyvsp[-4].val);v2=(yyvsp[-3].val);v3=(yyvsp[-2].val);v4=(yyvsp[-1].val);v5=dispatch4(command_call,v1,v2,v3,v4);v6=v5;v7=(yyvsp[0].val);v8=v7==Qundef ? v6 : dispatch2(method_add_block,v6,v7);(yyval.val)=v8;}
		    }
#line 9527 "ripper.c" /* yacc.c:1646  */
    break;

  case 441:
#line 3579 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = new_command_qcall(p, (yyvsp[-3].val), (yyvsp[-4].val), (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-2]), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7,v8;v1=(yyvsp[-4].val);v2=(yyvsp[-3].val);v3=(yyvsp[-2].val);v4=(yyvsp[-1].val);v5=dispatch4(command_call,v1,v2,v3,v4);v6=v5;v7=(yyvsp[0].val);v8=dispatch2(method_add_block,v6,v7);(yyval.val)=v8;}
		    }
#line 9538 "ripper.c" /* yacc.c:1646  */
    break;

  case 442:
#line 3588 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = (yyvsp[-1].val);
			(yyval.val)->nd_args = (yyvsp[0].val);
			nd_set_last_loc((yyvsp[-1].val), (yylsp[0]).end_pos);
#endif
			{VALUE v1,v2,v3,v4,v5;v1=(yyvsp[-1].val);v2=dispatch1(fcall,v1);v3=v2;v4=(yyvsp[0].val);v5=dispatch2(method_add_arg,v3,v4);(yyval.val)=v5;}
		    }
#line 9551 "ripper.c" /* yacc.c:1646  */
    break;

  case 443:
#line 3597 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = new_qcall(p, (yyvsp[-2].val), (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
			nd_set_line((yyval.val), (yylsp[-1]).end_pos.lineno);
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7;v1=(yyvsp[-3].val);v2=(yyvsp[-2].val);v3=(yyvsp[-1].val);v4=dispatch3(call,v1,v2,v3);v5=v4;v6=(yyvsp[0].val);v7=v6==Qundef ? v5 : dispatch2(method_add_arg,v5,v6);(yyval.val)=v7;}
		    }
#line 9563 "ripper.c" /* yacc.c:1646  */
    break;

  case 444:
#line 3605 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = new_qcall(p, ID2VAL(idCOLON2), (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
			nd_set_line((yyval.val), (yylsp[-1]).end_pos.lineno);
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7;v1=(yyvsp[-3].val);v2=ID2VAL(idCOLON2);v3=(yyvsp[-1].val);v4=dispatch3(call,v1,v2,v3);v5=v4;v6=(yyvsp[0].val);v7=dispatch2(method_add_arg,v5,v6);(yyval.val)=v7;}
		    }
#line 9575 "ripper.c" /* yacc.c:1646  */
    break;

  case 445:
#line 3613 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = new_qcall(p, ID2VAL(idCOLON2), (yyvsp[-2].val), (yyvsp[0].val), Qnull, &(yylsp[0]), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-2].val);v2=ID2VAL(idCOLON2);v3=(yyvsp[0].val);v4=dispatch3(call,v1,v2,v3);(yyval.val)=v4;}
		    }
#line 9586 "ripper.c" /* yacc.c:1646  */
    break;

  case 446:
#line 3620 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = new_qcall(p, (yyvsp[-1].val), (yyvsp[-2].val), ID2VAL(idCall), (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
			nd_set_line((yyval.val), (yylsp[-1]).end_pos.lineno);
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7;v1=(yyvsp[-2].val);v2=(yyvsp[-1].val);v3=ID2VAL(idCall);v4=dispatch3(call,v1,v2,v3);v5=v4;v6=(yyvsp[0].val);v7=dispatch2(method_add_arg,v5,v6);(yyval.val)=v7;}
		    }
#line 9598 "ripper.c" /* yacc.c:1646  */
    break;

  case 447:
#line 3628 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = new_qcall(p, ID2VAL(idCOLON2), (yyvsp[-2].val), ID2VAL(idCall), (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
			nd_set_line((yyval.val), (yylsp[-1]).end_pos.lineno);
#endif
			{VALUE v1,v2,v3,v4,v5,v6,v7;v1=(yyvsp[-2].val);v2=ID2VAL(idCOLON2);v3=ID2VAL(idCall);v4=dispatch3(call,v1,v2,v3);v5=v4;v6=(yyvsp[0].val);v7=dispatch2(method_add_arg,v5,v6);(yyval.val)=v7;}
		    }
#line 9610 "ripper.c" /* yacc.c:1646  */
    break;

  case 448:
#line 3636 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_SUPER((yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(super,v1);(yyval.val)=v2;}
		    }
#line 9621 "ripper.c" /* yacc.c:1646  */
    break;

  case 449:
#line 3643 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_ZSUPER(&(yyloc));
#endif
			{VALUE v1;v1=dispatch0(zsuper);(yyval.val)=v1;}
		    }
#line 9632 "ripper.c" /* yacc.c:1646  */
    break;

  case 450:
#line 3650 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			if ((yyvsp[-3].val) && nd_type((yyvsp[-3].val)) == NODE_SELF)
			    (yyval.val) = NEW_FCALL(tAREF, (yyvsp[-1].val), &(yyloc));
			else
			    (yyval.val) = NEW_CALL((yyvsp[-3].val), tAREF, (yyvsp[-1].val), &(yyloc));
			fixpos((yyval.val), (yyvsp[-3].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-3].val);v2=escape_Qundef((yyvsp[-1].val));v3=dispatch2(aref,v1,v2);(yyval.val)=v3;}
		    }
#line 9647 "ripper.c" /* yacc.c:1646  */
    break;

  case 451:
#line 3663 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = (yyvsp[-1].val);
#if 0
			(yyval.val)->nd_body->nd_loc = code_loc_gen(&(yylsp[-2]), &(yylsp[0]));
			nd_set_line((yyval.val), (yylsp[-2]).end_pos.lineno);
#endif
		    }
#line 9659 "ripper.c" /* yacc.c:1646  */
    break;

  case 452:
#line 3671 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = (yyvsp[-1].val);
#if 0
			(yyval.val)->nd_body->nd_loc = code_loc_gen(&(yylsp[-2]), &(yylsp[0]));
			nd_set_line((yyval.val), (yylsp[-2]).end_pos.lineno);
#endif
		    }
#line 9671 "ripper.c" /* yacc.c:1646  */
    break;

  case 453:
#line 3680 "ripper.y" /* yacc.c:1646  */
    {(yyval.vars) = dyna_push(p);}
#line 9677 "ripper.c" /* yacc.c:1646  */
    break;

  case 454:
#line 3681 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.num) = p->max_numparam;
			p->max_numparam = 0;
		    }
#line 9686 "ripper.c" /* yacc.c:1646  */
    break;

  case 455:
#line 3685 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.node) = numparam_push(p);
		    }
#line 9694 "ripper.c" /* yacc.c:1646  */
    break;

  case 456:
#line 3689 "ripper.y" /* yacc.c:1646  */
    {
			int max_numparam = p->max_numparam;
			p->max_numparam = (yyvsp[-3].num);
			(yyvsp[-1].val) = args_with_numbered(p, (yyvsp[-1].val), max_numparam);
#if 0
			(yyval.val) = NEW_ITER((yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=escape_Qundef((yyvsp[-1].val));v2=(yyvsp[0].val);v3=dispatch2(brace_block,v1,v2);(yyval.val)=v3;}
			numparam_pop(p, (yyvsp[-2].node));
			dyna_pop(p, (yyvsp[-4].vars));
		    }
#line 9710 "ripper.c" /* yacc.c:1646  */
    break;

  case 457:
#line 3702 "ripper.y" /* yacc.c:1646  */
    {(yyval.vars) = dyna_push(p);}
#line 9716 "ripper.c" /* yacc.c:1646  */
    break;

  case 458:
#line 3703 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.num) = p->max_numparam;
			p->max_numparam = 0;
		    }
#line 9725 "ripper.c" /* yacc.c:1646  */
    break;

  case 459:
#line 3707 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.node) = numparam_push(p);
			CMDARG_PUSH(0);
		    }
#line 9734 "ripper.c" /* yacc.c:1646  */
    break;

  case 460:
#line 3712 "ripper.y" /* yacc.c:1646  */
    {
			int max_numparam = p->max_numparam;
			p->max_numparam = (yyvsp[-3].num);
			(yyvsp[-1].val) = args_with_numbered(p, (yyvsp[-1].val), max_numparam);
#if 0
			(yyval.val) = NEW_ITER((yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=escape_Qundef((yyvsp[-1].val));v2=(yyvsp[0].val);v3=dispatch2(do_block,v1,v2);(yyval.val)=v3;}
			CMDARG_POP();
			numparam_pop(p, (yyvsp[-2].node));
			dyna_pop(p, (yyvsp[-4].vars));
		    }
#line 9751 "ripper.c" /* yacc.c:1646  */
    break;

  case 461:
#line 3727 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			check_literal_when(p, (yyvsp[0].val), &(yylsp[0]));
			(yyval.val) = NEW_LIST((yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=dispatch0(args_new);v2=v1;v3=(yyvsp[0].val);v4=dispatch2(args_add,v2,v3);(yyval.val)=v4;}
		    }
#line 9763 "ripper.c" /* yacc.c:1646  */
    break;

  case 462:
#line 3735 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_SPLAT((yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=dispatch0(args_new);v2=v1;v3=(yyvsp[0].val);v4=dispatch2(args_add_star,v2,v3);(yyval.val)=v4;}
		    }
#line 9774 "ripper.c" /* yacc.c:1646  */
    break;

  case 463:
#line 3742 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			check_literal_when(p, (yyvsp[0].val), &(yylsp[0]));
			(yyval.val) = last_arg_append(p, (yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(args_add,v1,v2);(yyval.val)=v3;}
		    }
#line 9786 "ripper.c" /* yacc.c:1646  */
    break;

  case 464:
#line 3750 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = rest_arg_append(p, (yyvsp[-3].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-3].val);v2=(yyvsp[0].val);v3=dispatch2(args_add_star,v1,v2);(yyval.val)=v3;}
		    }
#line 9797 "ripper.c" /* yacc.c:1646  */
    break;

  case 465:
#line 3761 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_WHEN((yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
			fixpos((yyval.val), (yyvsp[-3].val));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-3].val);v2=(yyvsp[-1].val);v3=escape_Qundef((yyvsp[0].val));v4=dispatch3(when,v1,v2,v3);(yyval.val)=v4;}
		    }
#line 9809 "ripper.c" /* yacc.c:1646  */
    break;

  case 468:
#line 3775 "ripper.y" /* yacc.c:1646  */
    {
			SET_LEX_STATE(EXPR_BEG|EXPR_LABEL);
			p->command_start = FALSE;
			(yyval.num) = p->in_kwarg;
			p->in_kwarg = 1;
		    }
#line 9820 "ripper.c" /* yacc.c:1646  */
    break;

  case 469:
#line 3782 "ripper.y" /* yacc.c:1646  */
    {
			p->in_kwarg = !!(yyvsp[-2].num);
		    }
#line 9828 "ripper.c" /* yacc.c:1646  */
    break;

  case 470:
#line 3787 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_IN((yyvsp[-4].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-4].val);v2=(yyvsp[-1].val);v3=escape_Qundef((yyvsp[0].val));v4=dispatch3(in,v1,v2,v3);(yyval.val)=v4;}
		    }
#line 9839 "ripper.c" /* yacc.c:1646  */
    break;

  case 474:
#line 3801 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = new_if(p, (yyvsp[0].val), remove_begin((yyvsp[-2].val)), 0, &(yyloc));
			fixpos((yyval.val), (yyvsp[0].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[0].val);v2=(yyvsp[-2].val);v3=dispatch2(if_mod,v1,v2);(yyval.val)=v3;}
		    }
#line 9851 "ripper.c" /* yacc.c:1646  */
    break;

  case 475:
#line 3809 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = new_unless(p, (yyvsp[0].val), remove_begin((yyvsp[-2].val)), 0, &(yyloc));
			fixpos((yyval.val), (yyvsp[0].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[0].val);v2=(yyvsp[-2].val);v3=dispatch2(unless_mod,v1,v2);(yyval.val)=v3;}
		    }
#line 9863 "ripper.c" /* yacc.c:1646  */
    break;

  case 477:
#line 3820 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_array_pattern_tail(p, Qnone, 1, 0, Qnone, &(yyloc));
			(yyval.val) = new_array_pattern(p, Qnone, get_value((yyvsp[-1].val)), (yyval.val), &(yyloc));
		    }
#line 9872 "ripper.c" /* yacc.c:1646  */
    break;

  case 478:
#line 3825 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_array_pattern(p, Qnone, get_value((yyvsp[-2].val)), (yyvsp[0].val), &(yyloc));
#if 0
			nd_set_first_loc((yyval.val), (yylsp[-2]).beg_pos);
#endif

		    }
#line 9884 "ripper.c" /* yacc.c:1646  */
    break;

  case 479:
#line 3833 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_array_pattern(p, Qnone, Qnone, (yyvsp[0].val), &(yyloc));
		    }
#line 9892 "ripper.c" /* yacc.c:1646  */
    break;

  case 480:
#line 3837 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_hash_pattern(p, Qnone, (yyvsp[0].val), &(yyloc));
		    }
#line 9900 "ripper.c" /* yacc.c:1646  */
    break;

  case 482:
#line 3846 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			NODE *n = NEW_LIST((yyvsp[-2].val), &(yyloc));
			n = list_append(p, n, (yyvsp[0].val));
			(yyval.val) = new_hash(p, n, &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-2].val);v2=STATIC_ID2SYM(id_assoc);v3=(yyvsp[0].val);v4=dispatch3(binary,v1,v2,v3);(yyval.val)=v4;}
		    }
#line 9913 "ripper.c" /* yacc.c:1646  */
    break;

  case 484:
#line 3858 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_NODE(NODE_OR, (yyvsp[-2].val), (yyvsp[0].val), 0, &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[-2].val);v2=STATIC_ID2SYM(idOr);v3=(yyvsp[0].val);v4=dispatch3(binary,v1,v2,v3);(yyval.val)=v4;}
		    }
#line 9924 "ripper.c" /* yacc.c:1646  */
    break;

  case 487:
#line 3869 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_array_pattern(p, (yyvsp[-3].val), Qnone, (yyvsp[-1].val), &(yyloc));
#if 0
			nd_set_first_loc((yyval.val), (yylsp[-3]).beg_pos);
#endif

		    }
#line 9936 "ripper.c" /* yacc.c:1646  */
    break;

  case 488:
#line 3877 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_hash_pattern(p, (yyvsp[-3].val), (yyvsp[-1].val), &(yyloc));
#if 0
			nd_set_first_loc((yyval.val), (yylsp[-3]).beg_pos);
#endif

		    }
#line 9948 "ripper.c" /* yacc.c:1646  */
    break;

  case 489:
#line 3885 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_array_pattern_tail(p, Qnone, 0, 0, Qnone, &(yyloc));
			(yyval.val) = new_array_pattern(p, (yyvsp[-2].val), Qnone, (yyval.val), &(yyloc));
		    }
#line 9957 "ripper.c" /* yacc.c:1646  */
    break;

  case 490:
#line 3890 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_array_pattern(p, (yyvsp[-3].val), Qnone, (yyvsp[-1].val), &(yyloc));
#if 0
			nd_set_first_loc((yyval.val), (yylsp[-3]).beg_pos);
#endif

		    }
#line 9969 "ripper.c" /* yacc.c:1646  */
    break;

  case 491:
#line 3898 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_hash_pattern(p, (yyvsp[-3].val), (yyvsp[-1].val), &(yyloc));
#if 0
			nd_set_first_loc((yyval.val), (yylsp[-3]).beg_pos);
#endif

		    }
#line 9981 "ripper.c" /* yacc.c:1646  */
    break;

  case 492:
#line 3906 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_array_pattern_tail(p, Qnone, 0, 0, Qnone, &(yyloc));
			(yyval.val) = new_array_pattern(p, (yyvsp[-2].val), Qnone, (yyval.val), &(yyloc));
		    }
#line 9990 "ripper.c" /* yacc.c:1646  */
    break;

  case 493:
#line 3911 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_array_pattern(p, Qnone, Qnone, (yyvsp[-1].val), &(yyloc));
		    }
#line 9998 "ripper.c" /* yacc.c:1646  */
    break;

  case 494:
#line 3915 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_array_pattern_tail(p, Qnone, 0, 0, Qnone, &(yyloc));
			(yyval.val) = new_array_pattern(p, Qnone, Qnone, (yyval.val), &(yyloc));
		    }
#line 10007 "ripper.c" /* yacc.c:1646  */
    break;

  case 495:
#line 3920 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_hash_pattern(p, Qnone, (yyvsp[-1].val), &(yyloc));
		    }
#line 10015 "ripper.c" /* yacc.c:1646  */
    break;

  case 496:
#line 3924 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_hash_pattern_tail(p, Qnone, 0, &(yyloc));
			(yyval.val) = new_hash_pattern(p, Qnone, (yyval.val), &(yyloc));
		    }
#line 10024 "ripper.c" /* yacc.c:1646  */
    break;

  case 497:
#line 3929 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = (yyvsp[-1].val);
		    }
#line 10032 "ripper.c" /* yacc.c:1646  */
    break;

  case 498:
#line 3935 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			NODE *pre_args = NEW_LIST((yyvsp[0].val), &(yyloc));
			(yyval.val) = new_array_pattern_tail(p, pre_args, 0, 0, Qnone, &(yyloc));
#endif
			(yyval.val) = new_array_pattern_tail(p, rb_ary_new_from_args(1, get_value((yyvsp[0].val))), 0, 0, Qnone, &(yyloc));

		    }
#line 10045 "ripper.c" /* yacc.c:1646  */
    break;

  case 499:
#line 3944 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_array_pattern_tail(p, (yyvsp[0].val), 1, 0, Qnone, &(yyloc));
		    }
#line 10053 "ripper.c" /* yacc.c:1646  */
    break;

  case 500:
#line 3948 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = new_array_pattern_tail(p, list_concat((yyvsp[-1].val), (yyvsp[0].val)), 0, 0, Qnone, &(yyloc));
#endif
			VALUE pre_args = rb_ary_concat((yyvsp[-1].val), get_value((yyvsp[0].val)));
			(yyval.val) = new_array_pattern_tail(p, pre_args, 0, 0, Qnone, &(yyloc));

		    }
#line 10066 "ripper.c" /* yacc.c:1646  */
    break;

  case 501:
#line 3957 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_array_pattern_tail(p, (yyvsp[-2].val), 1, (yyvsp[0].val), Qnone, &(yyloc));
		    }
#line 10074 "ripper.c" /* yacc.c:1646  */
    break;

  case 502:
#line 3961 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_array_pattern_tail(p, (yyvsp[-4].val), 1, (yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
		    }
#line 10082 "ripper.c" /* yacc.c:1646  */
    break;

  case 503:
#line 3965 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_array_pattern_tail(p, (yyvsp[-1].val), 1, 0, Qnone, &(yyloc));
		    }
#line 10090 "ripper.c" /* yacc.c:1646  */
    break;

  case 504:
#line 3969 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_array_pattern_tail(p, (yyvsp[-3].val), 1, 0, (yyvsp[0].val), &(yyloc));
		    }
#line 10098 "ripper.c" /* yacc.c:1646  */
    break;

  case 506:
#line 3976 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = (yyvsp[-1].val);
		    }
#line 10106 "ripper.c" /* yacc.c:1646  */
    break;

  case 507:
#line 3980 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = list_concat((yyvsp[-2].val), (yyvsp[-1].val));
#endif
			(yyval.val)=rb_ary_concat((yyvsp[-2].val), get_value((yyvsp[-1].val)));
		    }
#line 10117 "ripper.c" /* yacc.c:1646  */
    break;

  case 508:
#line 3989 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_array_pattern_tail(p, Qnone, 1, (yyvsp[0].val), Qnone, &(yyloc));
		    }
#line 10125 "ripper.c" /* yacc.c:1646  */
    break;

  case 509:
#line 3993 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_array_pattern_tail(p, Qnone, 1, (yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
		    }
#line 10133 "ripper.c" /* yacc.c:1646  */
    break;

  case 510:
#line 3997 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_array_pattern_tail(p, Qnone, 1, 0, Qnone, &(yyloc));
		    }
#line 10141 "ripper.c" /* yacc.c:1646  */
    break;

  case 511:
#line 4001 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_array_pattern_tail(p, Qnone, 1, 0, (yyvsp[0].val), &(yyloc));
		    }
#line 10149 "ripper.c" /* yacc.c:1646  */
    break;

  case 513:
#line 4007 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = list_concat((yyvsp[-2].val), (yyvsp[0].val));
#endif
			(yyval.val)=rb_ary_concat((yyvsp[-2].val), get_value((yyvsp[0].val)));
		    }
#line 10160 "ripper.c" /* yacc.c:1646  */
    break;

  case 514:
#line 4016 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_LIST((yyvsp[0].val), &(yyloc));
#endif
			(yyval.val)=rb_ary_new_from_args(1, get_value((yyvsp[0].val)));
		    }
#line 10171 "ripper.c" /* yacc.c:1646  */
    break;

  case 515:
#line 4025 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) =  new_hash_pattern_tail(p, new_unique_key_hash(p, (yyvsp[-2].val), &(yyloc)), (yyvsp[0].val), &(yyloc));
		    }
#line 10179 "ripper.c" /* yacc.c:1646  */
    break;

  case 516:
#line 4029 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) =  new_hash_pattern_tail(p, new_unique_key_hash(p, (yyvsp[0].val), &(yyloc)), 0, &(yyloc));
		    }
#line 10187 "ripper.c" /* yacc.c:1646  */
    break;

  case 517:
#line 4033 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) =  new_hash_pattern_tail(p, new_hash(p, Qnone, &(yyloc)), (yyvsp[0].val), &(yyloc));
		    }
#line 10195 "ripper.c" /* yacc.c:1646  */
    break;

  case 518:
#line 4037 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) =  new_hash_pattern_tail(p, new_unique_key_hash(p, (yyvsp[-2].val), &(yyloc)), ID2VAL(idNil), &(yyloc));
		    }
#line 10203 "ripper.c" /* yacc.c:1646  */
    break;

  case 519:
#line 4041 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) =  new_hash_pattern_tail(p, new_hash(p, Qnone, &(yyloc)), ID2VAL(idNil), &(yyloc));
		    }
#line 10211 "ripper.c" /* yacc.c:1646  */
    break;

  case 521:
#line 4048 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = list_concat((yyvsp[-2].val), (yyvsp[0].val));
#endif
			(yyval.val)=rb_ary_concat((yyvsp[-2].val), (yyvsp[0].val));
		    }
#line 10222 "ripper.c" /* yacc.c:1646  */
    break;

  case 522:
#line 4057 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = list_append(p, NEW_LIST(NEW_LIT(ID2SYM((yyvsp[-1].val)), &(yyloc)), &(yyloc)), (yyvsp[0].val));
#endif
			(yyval.val)=rb_ary_new_from_args(1, rb_ary_new_from_args(2, get_value((yyvsp[-1].val)), get_value((yyvsp[0].val))));
		    }
#line 10233 "ripper.c" /* yacc.c:1646  */
    break;

  case 523:
#line 4064 "ripper.y" /* yacc.c:1646  */
    {
			if (!is_local_id(get_id((yyvsp[0].val)))) {
			    yyerror1(&(yylsp[0]), "key must be valid as local variables");
			}
#if 0
			(yyval.val) = list_append(p, NEW_LIST(NEW_LIT(ID2SYM((yyvsp[0].val)), &(yyloc)), &(yyloc)), assignable(p, (yyvsp[0].val), 0, &(yyloc)));
#endif
			(yyval.val)=rb_ary_new_from_args(1, rb_ary_new_from_args(2, get_value((yyvsp[0].val)), Qnil));
		    }
#line 10247 "ripper.c" /* yacc.c:1646  */
    break;

  case 524:
#line 4074 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			YYLTYPE loc = code_loc_gen(&(yylsp[-3]), &(yylsp[-1]));
			NODE *node = dsym_node(p, (yyvsp[-2].val), &loc);
			if (nd_type(node) == NODE_LIT) {
			    (yyval.val) = list_append(p, NEW_LIST(node, &loc), (yyvsp[0].val));
			}
			else {
			    yyerror1(&loc, "symbol literal with interpolation is not allowed");
			    (yyval.val) = 0;
			}
#endif
			(yyval.val)=rb_ary_new_from_args(1, rb_ary_new_from_args(2, (yyvsp[-2].val), get_value((yyvsp[0].val))));
		    }
#line 10266 "ripper.c" /* yacc.c:1646  */
    break;

  case 525:
#line 4089 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			YYLTYPE loc = code_loc_gen(&(yylsp[-2]), &(yylsp[0]));
			NODE *node = dsym_node(p, (yyvsp[-1].val), &loc);
			ID id;
			if (nd_type(node) == NODE_LIT) {
			    id = SYM2ID(node->nd_lit);
			    if (!is_local_id(id)) {
				yyerror1(&loc, "key must be valid as local variables");
			    }
			    (yyval.val) = list_append(p, NEW_LIST(node, &loc), assignable(p, id, 0, &(yyloc)));
			}
			else {
			    yyerror1(&loc, "symbol literal with interpolation is not allowed");
			    (yyval.val) = 0;
			}
#endif
			(yyval.val)=rb_ary_new_from_args(1, rb_ary_new_from_args(2, (yyvsp[-1].val), Qnil));
		    }
#line 10290 "ripper.c" /* yacc.c:1646  */
    break;

  case 526:
#line 4111 "ripper.y" /* yacc.c:1646  */
    {
		        (yyval.val) = (yyvsp[0].val);
		    }
#line 10298 "ripper.c" /* yacc.c:1646  */
    break;

  case 527:
#line 4115 "ripper.y" /* yacc.c:1646  */
    {
		        (yyval.val) = 0;
		    }
#line 10306 "ripper.c" /* yacc.c:1646  */
    break;

  case 528:
#line 4121 "ripper.y" /* yacc.c:1646  */
    {
		        (yyval.val) = 0;
		    }
#line 10314 "ripper.c" /* yacc.c:1646  */
    break;

  case 530:
#line 4128 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			value_expr((yyvsp[-2].val));
			value_expr((yyvsp[0].val));
			(yyval.val) = NEW_DOT2((yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(dot2,v1,v2);(yyval.val)=v3;}
		    }
#line 10327 "ripper.c" /* yacc.c:1646  */
    break;

  case 531:
#line 4137 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			value_expr((yyvsp[-2].val));
			value_expr((yyvsp[0].val));
			(yyval.val) = NEW_DOT3((yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(dot3,v1,v2);(yyval.val)=v3;}
		    }
#line 10340 "ripper.c" /* yacc.c:1646  */
    break;

  case 532:
#line 4146 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			YYLTYPE loc;
			loc.beg_pos = (yylsp[0]).end_pos;
			loc.end_pos = (yylsp[0]).end_pos;

			value_expr((yyvsp[-1].val));
			(yyval.val) = NEW_DOT2((yyvsp[-1].val), new_nil(&loc), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-1].val);v2=Qnil;v3=dispatch2(dot2,v1,v2);(yyval.val)=v3;}
		    }
#line 10356 "ripper.c" /* yacc.c:1646  */
    break;

  case 533:
#line 4158 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			YYLTYPE loc;
			loc.beg_pos = (yylsp[0]).end_pos;
			loc.end_pos = (yylsp[0]).end_pos;

			value_expr((yyvsp[-1].val));
			(yyval.val) = NEW_DOT3((yyvsp[-1].val), new_nil(&loc), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-1].val);v2=Qnil;v3=dispatch2(dot3,v1,v2);(yyval.val)=v3;}
		    }
#line 10372 "ripper.c" /* yacc.c:1646  */
    break;

  case 537:
#line 4173 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			YYLTYPE loc;
			loc.beg_pos = (yylsp[-1]).beg_pos;
			loc.end_pos = (yylsp[-1]).beg_pos;

			value_expr((yyvsp[0].val));
			(yyval.val) = NEW_DOT2(new_nil(&loc), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=Qnil;v2=(yyvsp[0].val);v3=dispatch2(dot2,v1,v2);(yyval.val)=v3;}
		    }
#line 10388 "ripper.c" /* yacc.c:1646  */
    break;

  case 538:
#line 4185 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			YYLTYPE loc;
			loc.beg_pos = (yylsp[-1]).beg_pos;
			loc.end_pos = (yylsp[-1]).beg_pos;

			value_expr((yyvsp[0].val));
			(yyval.val) = NEW_DOT3(new_nil(&loc), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=Qnil;v2=(yyvsp[0].val);v3=dispatch2(dot3,v1,v2);(yyval.val)=v3;}
		    }
#line 10404 "ripper.c" /* yacc.c:1646  */
    break;

  case 547:
#line 4207 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			if (!((yyval.val) = gettable(p, (yyvsp[0].val), &(yyloc)))) (yyval.val) = NEW_BEGIN(0, &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(var_ref,v1);(yyval.val)=v2;}
		    }
#line 10415 "ripper.c" /* yacc.c:1646  */
    break;

  case 548:
#line 4214 "ripper.y" /* yacc.c:1646  */
    {
			token_info_push(p, "->", &(yylsp[0]));
		    }
#line 10423 "ripper.c" /* yacc.c:1646  */
    break;

  case 549:
#line 4218 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = (yyvsp[0].val);
#if 0
			nd_set_first_loc((yyval.val), (yylsp[-2]).beg_pos);
#endif
		    }
#line 10434 "ripper.c" /* yacc.c:1646  */
    break;

  case 550:
#line 4227 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = assignable(p, (yyvsp[0].val), 0, &(yyloc));
#endif
			(yyval.val)=assignable(p, var_field(p, (yyvsp[0].val)));
		    }
#line 10445 "ripper.c" /* yacc.c:1646  */
    break;

  case 551:
#line 4236 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			NODE *n = gettable(p, (yyvsp[0].val), &(yyloc));
			if (!(nd_type(n) == NODE_LVAR || nd_type(n) == NODE_DVAR)) {
			    compile_error(p, "%"PRIsVALUE": no such local variable", rb_id2str((yyvsp[0].val)));
			}
			(yyval.val) = n;
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(var_ref,v1);(yyval.val)=v2;}
		    }
#line 10460 "ripper.c" /* yacc.c:1646  */
    break;

  case 552:
#line 4249 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_COLON3((yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(top_const_ref,v1);(yyval.val)=v2;}
		    }
#line 10471 "ripper.c" /* yacc.c:1646  */
    break;

  case 553:
#line 4256 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_COLON2((yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(const_path_ref,v1,v2);(yyval.val)=v3;}
		    }
#line 10482 "ripper.c" /* yacc.c:1646  */
    break;

  case 554:
#line 4263 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = gettable(p, (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(var_ref,v1);(yyval.val)=v2;}
		   }
#line 10493 "ripper.c" /* yacc.c:1646  */
    break;

  case 555:
#line 4274 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_RESBODY((yyvsp[-4].val),
					 (yyvsp[-3].val) ? block_append(p, node_assign(p, (yyvsp[-3].val), NEW_ERRINFO(&(yylsp[-3])), &(yylsp[-3])), (yyvsp[-1].val)) : (yyvsp[-1].val),
					 (yyvsp[0].val), &(yyloc));
			fixpos((yyval.val), (yyvsp[-4].val)?(yyvsp[-4].val):(yyvsp[-1].val));
#endif
			{VALUE v1,v2,v3,v4,v5;v1=escape_Qundef((yyvsp[-4].val));v2=escape_Qundef((yyvsp[-3].val));v3=escape_Qundef((yyvsp[-1].val));v4=escape_Qundef((yyvsp[0].val));v5=dispatch4(rescue,v1,v2,v3,v4);(yyval.val)=v5;}
		    }
#line 10507 "ripper.c" /* yacc.c:1646  */
    break;

  case 557:
#line 4287 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_LIST((yyvsp[0].val), &(yyloc));
#endif
			(yyval.val)=rb_ary_new3(1, get_value((yyvsp[0].val)));
		    }
#line 10518 "ripper.c" /* yacc.c:1646  */
    break;

  case 558:
#line 4294 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			if (!((yyval.val) = splat_array((yyvsp[0].val)))) (yyval.val) = (yyvsp[0].val);
#endif
			(yyval.val)=(yyvsp[0].val);
		    }
#line 10529 "ripper.c" /* yacc.c:1646  */
    break;

  case 560:
#line 4304 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = (yyvsp[0].val);
		    }
#line 10537 "ripper.c" /* yacc.c:1646  */
    break;

  case 562:
#line 4311 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = (yyvsp[0].val);
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(ensure,v1);(yyval.val)=v2;}
		    }
#line 10548 "ripper.c" /* yacc.c:1646  */
    break;

  case 566:
#line 4325 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			NODE *node = (yyvsp[0].val);
			if (!node) {
			    node = NEW_STR(STR_NEW0(), &(yyloc));
                            RB_OBJ_WRITTEN(p->ast, Qnil, node->nd_lit);
			}
			else {
			    node = evstr2dstr(p, node);
			}
			(yyval.val) = node;
#endif
			(yyval.val)=(yyvsp[0].val);
		    }
#line 10567 "ripper.c" /* yacc.c:1646  */
    break;

  case 569:
#line 4344 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = literal_concat(p, (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-1].val);v2=(yyvsp[0].val);v3=dispatch2(string_concat,v1,v2);(yyval.val)=v3;}
		    }
#line 10578 "ripper.c" /* yacc.c:1646  */
    break;

  case 570:
#line 4353 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = heredoc_dedent(p, (yyvsp[-1].val));
			if ((yyval.val)) nd_set_loc((yyval.val), &(yyloc));
#endif
			{VALUE v1,v2;v1=heredoc_dedent(p, (yyvsp[-1].val));v2=dispatch1(string_literal,v1);(yyval.val)=v2;}
		    }
#line 10590 "ripper.c" /* yacc.c:1646  */
    break;

  case 571:
#line 4363 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = new_xstring(p, heredoc_dedent(p, (yyvsp[-1].val)), &(yyloc));
#endif
			{VALUE v1,v2;v1=heredoc_dedent(p, (yyvsp[-1].val));v2=dispatch1(xstring_literal,v1);(yyval.val)=v2;}
		    }
#line 10601 "ripper.c" /* yacc.c:1646  */
    break;

  case 572:
#line 4372 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_regexp(p, (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 10609 "ripper.c" /* yacc.c:1646  */
    break;

  case 573:
#line 4378 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = make_list((yyvsp[-1].val), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(array,v1);(yyval.val)=v2;}
		    }
#line 10620 "ripper.c" /* yacc.c:1646  */
    break;

  case 574:
#line 4387 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = 0;
#endif
			{VALUE v1;v1=dispatch0(words_new);(yyval.val)=v1;}
		    }
#line 10631 "ripper.c" /* yacc.c:1646  */
    break;

  case 575:
#line 4394 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = list_append(p, (yyvsp[-2].val), evstr2dstr(p, (yyvsp[-1].val)));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[-1].val);v3=dispatch2(words_add,v1,v2);(yyval.val)=v3;}
		    }
#line 10642 "ripper.c" /* yacc.c:1646  */
    break;

  case 576:
#line 4403 "ripper.y" /* yacc.c:1646  */
    {{VALUE v1,v2,v3,v4;v1=dispatch0(word_new);v2=v1;v3=(yyvsp[0].val);v4=dispatch2(word_add,v2,v3);(yyval.val)=v4;}}
#line 10648 "ripper.c" /* yacc.c:1646  */
    break;

  case 577:
#line 4405 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = literal_concat(p, (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-1].val);v2=(yyvsp[0].val);v3=dispatch2(word_add,v1,v2);(yyval.val)=v3;}
		    }
#line 10659 "ripper.c" /* yacc.c:1646  */
    break;

  case 578:
#line 4414 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = make_list((yyvsp[-1].val), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(array,v1);(yyval.val)=v2;}
		    }
#line 10670 "ripper.c" /* yacc.c:1646  */
    break;

  case 579:
#line 4423 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = 0;
#endif
			{VALUE v1;v1=dispatch0(symbols_new);(yyval.val)=v1;}
		    }
#line 10681 "ripper.c" /* yacc.c:1646  */
    break;

  case 580:
#line 4430 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = symbol_append(p, (yyvsp[-2].val), evstr2dstr(p, (yyvsp[-1].val)));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[-1].val);v3=dispatch2(symbols_add,v1,v2);(yyval.val)=v3;}
		    }
#line 10692 "ripper.c" /* yacc.c:1646  */
    break;

  case 581:
#line 4439 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = make_list((yyvsp[-1].val), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(array,v1);(yyval.val)=v2;}
		    }
#line 10703 "ripper.c" /* yacc.c:1646  */
    break;

  case 582:
#line 4448 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = make_list((yyvsp[-1].val), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(array,v1);(yyval.val)=v2;}
		    }
#line 10714 "ripper.c" /* yacc.c:1646  */
    break;

  case 583:
#line 4457 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = 0;
#endif
			{VALUE v1;v1=dispatch0(qwords_new);(yyval.val)=v1;}
		    }
#line 10725 "ripper.c" /* yacc.c:1646  */
    break;

  case 584:
#line 4464 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = list_append(p, (yyvsp[-2].val), (yyvsp[-1].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[-1].val);v3=dispatch2(qwords_add,v1,v2);(yyval.val)=v3;}
		    }
#line 10736 "ripper.c" /* yacc.c:1646  */
    break;

  case 585:
#line 4473 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = 0;
#endif
			{VALUE v1;v1=dispatch0(qsymbols_new);(yyval.val)=v1;}
		    }
#line 10747 "ripper.c" /* yacc.c:1646  */
    break;

  case 586:
#line 4480 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = symbol_append(p, (yyvsp[-2].val), (yyvsp[-1].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[-1].val);v3=dispatch2(qsymbols_add,v1,v2);(yyval.val)=v3;}
		    }
#line 10758 "ripper.c" /* yacc.c:1646  */
    break;

  case 587:
#line 4489 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = 0;
#endif
			{VALUE v1;v1=dispatch0(string_content);(yyval.val)=v1;}
		    }
#line 10769 "ripper.c" /* yacc.c:1646  */
    break;

  case 588:
#line 4496 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = literal_concat(p, (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-1].val);v2=(yyvsp[0].val);v3=dispatch2(string_add,v1,v2);(yyval.val)=v3;}
		    }
#line 10780 "ripper.c" /* yacc.c:1646  */
    break;

  case 589:
#line 4505 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = 0;
#endif
			{VALUE v1;v1=dispatch0(xstring_new);(yyval.val)=v1;}
		    }
#line 10791 "ripper.c" /* yacc.c:1646  */
    break;

  case 590:
#line 4512 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = literal_concat(p, (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-1].val);v2=(yyvsp[0].val);v3=dispatch2(xstring_add,v1,v2);(yyval.val)=v3;}
		    }
#line 10802 "ripper.c" /* yacc.c:1646  */
    break;

  case 591:
#line 4521 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = 0;
#endif
			{VALUE v1;v1=dispatch0(regexp_new);(yyval.val)=v1;}
#if 0
#endif
			(yyval.val) = ripper_new_yylval(p, 0, (yyval.val), 0);

		    }
#line 10817 "ripper.c" /* yacc.c:1646  */
    break;

  case 592:
#line 4532 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			NODE *head = (yyvsp[-1].val), *tail = (yyvsp[0].val);
			if (!head) {
			    (yyval.val) = tail;
			}
			else if (!tail) {
			    (yyval.val) = head;
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
			    (yyval.val) = list_append(p, head, tail);
			}
#endif
			VALUE s1 = 1, s2 = 0, n1 = (yyvsp[-1].val), n2 = (yyvsp[0].val);
			if (ripper_is_node_yylval(n1)) {
			    s1 = RNODE(n1)->nd_cval;
			    n1 = RNODE(n1)->nd_rval;
			}
			if (ripper_is_node_yylval(n2)) {
			    s2 = RNODE(n2)->nd_cval;
			    n2 = RNODE(n2)->nd_rval;
			}
			(yyval.val) = dispatch2(regexp_add, n1, n2);
			if (!s1 && s2) {
			    (yyval.val) = ripper_new_yylval(p, 0, (yyval.val), s2);
			}

		    }
#line 10860 "ripper.c" /* yacc.c:1646  */
    break;

  case 594:
#line 4574 "ripper.y" /* yacc.c:1646  */
    {
			/* need to backup p->lex.strterm so that a string literal `%&foo,#$&,bar&` can be parsed */
			(yyval.strterm) = p->lex.strterm;
			p->lex.strterm = 0;
			SET_LEX_STATE(EXPR_BEG);
		    }
#line 10871 "ripper.c" /* yacc.c:1646  */
    break;

  case 595:
#line 4581 "ripper.y" /* yacc.c:1646  */
    {
			p->lex.strterm = (yyvsp[-1].strterm);
#if 0
			(yyval.val) = NEW_EVSTR((yyvsp[0].val), &(yyloc));
			nd_set_line((yyval.val), (yylsp[0]).end_pos.lineno);
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(string_dvar,v1);(yyval.val)=v2;}
		    }
#line 10884 "ripper.c" /* yacc.c:1646  */
    break;

  case 596:
#line 4590 "ripper.y" /* yacc.c:1646  */
    {
			CMDARG_PUSH(0);
			COND_PUSH(0);
		    }
#line 10893 "ripper.c" /* yacc.c:1646  */
    break;

  case 597:
#line 4594 "ripper.y" /* yacc.c:1646  */
    {
			/* need to backup p->lex.strterm so that a string literal `%!foo,#{ !0 },bar!` can be parsed */
			(yyval.strterm) = p->lex.strterm;
			p->lex.strterm = 0;
		    }
#line 10903 "ripper.c" /* yacc.c:1646  */
    break;

  case 598:
#line 4599 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.num) = p->lex.state;
			SET_LEX_STATE(EXPR_BEG);
		    }
#line 10912 "ripper.c" /* yacc.c:1646  */
    break;

  case 599:
#line 4603 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.num) = p->lex.brace_nest;
			p->lex.brace_nest = 0;
		    }
#line 10921 "ripper.c" /* yacc.c:1646  */
    break;

  case 600:
#line 4607 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.num) = p->heredoc_indent;
			p->heredoc_indent = 0;
		    }
#line 10930 "ripper.c" /* yacc.c:1646  */
    break;

  case 601:
#line 4612 "ripper.y" /* yacc.c:1646  */
    {
			COND_POP();
			CMDARG_POP();
			p->lex.strterm = (yyvsp[-5].strterm);
			SET_LEX_STATE((yyvsp[-4].num));
			p->lex.brace_nest = (yyvsp[-3].num);
			p->heredoc_indent = (yyvsp[-2].num);
			p->heredoc_line_indent = -1;
#if 0
			if ((yyvsp[-1].val)) (yyvsp[-1].val)->flags &= ~NODE_FL_NEWLINE;
			(yyval.val) = new_evstr(p, (yyvsp[-1].val), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(string_embexpr,v1);(yyval.val)=v2;}
		    }
#line 10949 "ripper.c" /* yacc.c:1646  */
    break;

  case 602:
#line 4629 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_GVAR((yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(var_ref,v1);(yyval.val)=v2;}
		    }
#line 10960 "ripper.c" /* yacc.c:1646  */
    break;

  case 603:
#line 4636 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_IVAR((yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(var_ref,v1);(yyval.val)=v2;}
		    }
#line 10971 "ripper.c" /* yacc.c:1646  */
    break;

  case 604:
#line 4643 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = NEW_CVAR((yyvsp[0].val), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(var_ref,v1);(yyval.val)=v2;}
		    }
#line 10982 "ripper.c" /* yacc.c:1646  */
    break;

  case 608:
#line 4657 "ripper.y" /* yacc.c:1646  */
    {
			SET_LEX_STATE(EXPR_END);
#if 0
			(yyval.val) = NEW_LIT(ID2SYM((yyvsp[0].val)), &(yyloc));
#endif
			{VALUE v1,v2,v3,v4;v1=(yyvsp[0].val);v2=dispatch1(symbol,v1);v3=v2;v4=dispatch1(symbol_literal,v3);(yyval.val)=v4;}
		    }
#line 10994 "ripper.c" /* yacc.c:1646  */
    break;

  case 613:
#line 4673 "ripper.y" /* yacc.c:1646  */
    {
			SET_LEX_STATE(EXPR_END);
#if 0
			(yyval.val) = dsym_node(p, (yyvsp[-1].val), &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(dyna_symbol,v1);(yyval.val)=v2;}
		    }
#line 11006 "ripper.c" /* yacc.c:1646  */
    break;

  case 615:
#line 4684 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = (yyvsp[0].val);
			RB_OBJ_WRITE(p->ast, &(yyval.val)->nd_lit, negate_lit(p, (yyval.val)->nd_lit));
#endif
			{VALUE v1,v2,v3;v1=ID2VAL(idUMinus);v2=(yyvsp[0].val);v3=dispatch2(unary,v1,v2);(yyval.val)=v3;}
		    }
#line 11018 "ripper.c" /* yacc.c:1646  */
    break;

  case 625:
#line 4706 "ripper.y" /* yacc.c:1646  */
    {(yyval.val) = KWD2EID(nil, (yyvsp[0].val));}
#line 11024 "ripper.c" /* yacc.c:1646  */
    break;

  case 626:
#line 4707 "ripper.y" /* yacc.c:1646  */
    {(yyval.val) = KWD2EID(self, (yyvsp[0].val));}
#line 11030 "ripper.c" /* yacc.c:1646  */
    break;

  case 627:
#line 4708 "ripper.y" /* yacc.c:1646  */
    {(yyval.val) = KWD2EID(true, (yyvsp[0].val));}
#line 11036 "ripper.c" /* yacc.c:1646  */
    break;

  case 628:
#line 4709 "ripper.y" /* yacc.c:1646  */
    {(yyval.val) = KWD2EID(false, (yyvsp[0].val));}
#line 11042 "ripper.c" /* yacc.c:1646  */
    break;

  case 629:
#line 4710 "ripper.y" /* yacc.c:1646  */
    {(yyval.val) = KWD2EID(_FILE__, (yyvsp[0].val));}
#line 11048 "ripper.c" /* yacc.c:1646  */
    break;

  case 630:
#line 4711 "ripper.y" /* yacc.c:1646  */
    {(yyval.val) = KWD2EID(_LINE__, (yyvsp[0].val));}
#line 11054 "ripper.c" /* yacc.c:1646  */
    break;

  case 631:
#line 4712 "ripper.y" /* yacc.c:1646  */
    {(yyval.val) = KWD2EID(_ENCODING__, (yyvsp[0].val));}
#line 11060 "ripper.c" /* yacc.c:1646  */
    break;

  case 632:
#line 4716 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			if (!((yyval.val) = gettable(p, (yyvsp[0].val), &(yyloc)))) (yyval.val) = NEW_BEGIN(0, &(yyloc));
#endif
			if (id_is_var(p, get_id((yyvsp[0].val)))) {
			    (yyval.val) = dispatch1(var_ref, (yyvsp[0].val));
			}
			else {
			    (yyval.val) = dispatch1(vcall, (yyvsp[0].val));
			}

		    }
#line 11077 "ripper.c" /* yacc.c:1646  */
    break;

  case 633:
#line 4729 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			if (!((yyval.val) = gettable(p, (yyvsp[0].val), &(yyloc)))) (yyval.val) = NEW_BEGIN(0, &(yyloc));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(var_ref,v1);(yyval.val)=v2;}
		    }
#line 11088 "ripper.c" /* yacc.c:1646  */
    break;

  case 634:
#line 4738 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = assignable(p, (yyvsp[0].val), 0, &(yyloc));
#endif
			(yyval.val)=assignable(p, var_field(p, (yyvsp[0].val)));
		    }
#line 11099 "ripper.c" /* yacc.c:1646  */
    break;

  case 635:
#line 4745 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = assignable(p, (yyvsp[0].val), 0, &(yyloc));
#endif
			(yyval.val)=assignable(p, var_field(p, (yyvsp[0].val)));
		    }
#line 11110 "ripper.c" /* yacc.c:1646  */
    break;

  case 638:
#line 4758 "ripper.y" /* yacc.c:1646  */
    {
			SET_LEX_STATE(EXPR_BEG);
			p->command_start = TRUE;
		    }
#line 11119 "ripper.c" /* yacc.c:1646  */
    break;

  case 639:
#line 4763 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = (yyvsp[-1].val);
		    }
#line 11127 "ripper.c" /* yacc.c:1646  */
    break;

  case 640:
#line 4767 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = 0;
#endif
			(yyval.val)=Qnil;
		    }
#line 11138 "ripper.c" /* yacc.c:1646  */
    break;

  case 641:
#line 4776 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = (yyvsp[-1].val);
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(paren,v1);(yyval.val)=v2;}
			SET_LEX_STATE(EXPR_BEG);
			p->command_start = TRUE;
		    }
#line 11151 "ripper.c" /* yacc.c:1646  */
    break;

  case 642:
#line 4784 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.num) = p->in_kwarg;
			p->in_kwarg = 1;
			SET_LEX_STATE(p->lex.state|EXPR_LABEL); /* force for args */
		    }
#line 11161 "ripper.c" /* yacc.c:1646  */
    break;

  case 643:
#line 4790 "ripper.y" /* yacc.c:1646  */
    {
			p->in_kwarg = !!(yyvsp[-2].num);
			(yyval.val) = (yyvsp[-1].val);
			SET_LEX_STATE(EXPR_BEG);
			p->command_start = TRUE;
		    }
#line 11172 "ripper.c" /* yacc.c:1646  */
    break;

  case 644:
#line 4799 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args_tail(p, (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-1]));
		    }
#line 11180 "ripper.c" /* yacc.c:1646  */
    break;

  case 645:
#line 4803 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args_tail(p, (yyvsp[-1].val), Qnone, (yyvsp[0].val), &(yylsp[-1]));
		    }
#line 11188 "ripper.c" /* yacc.c:1646  */
    break;

  case 646:
#line 4807 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args_tail(p, Qnone, (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-1]));
		    }
#line 11196 "ripper.c" /* yacc.c:1646  */
    break;

  case 647:
#line 4811 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args_tail(p, Qnone, ID2VAL(idNil), (yyvsp[0].val), &(yylsp[-1]));
		    }
#line 11204 "ripper.c" /* yacc.c:1646  */
    break;

  case 648:
#line 4815 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args_tail(p, Qnone, Qnone, (yyvsp[0].val), &(yylsp[0]));
		    }
#line 11212 "ripper.c" /* yacc.c:1646  */
    break;

  case 649:
#line 4821 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = (yyvsp[0].val);
		    }
#line 11220 "ripper.c" /* yacc.c:1646  */
    break;

  case 650:
#line 4825 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args_tail(p, Qnone, Qnone, Qnone, &(yylsp[0]));
		    }
#line 11228 "ripper.c" /* yacc.c:1646  */
    break;

  case 651:
#line 4831 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args(p, (yyvsp[-5].val), (yyvsp[-3].val), (yyvsp[-1].val), Qnone, (yyvsp[0].val), &(yyloc));
		    }
#line 11236 "ripper.c" /* yacc.c:1646  */
    break;

  case 652:
#line 4835 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args(p, (yyvsp[-7].val), (yyvsp[-5].val), (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 11244 "ripper.c" /* yacc.c:1646  */
    break;

  case 653:
#line 4839 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args(p, (yyvsp[-3].val), (yyvsp[-1].val), Qnone, Qnone, (yyvsp[0].val), &(yyloc));
		    }
#line 11252 "ripper.c" /* yacc.c:1646  */
    break;

  case 654:
#line 4843 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args(p, (yyvsp[-5].val), (yyvsp[-3].val), Qnone, (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 11260 "ripper.c" /* yacc.c:1646  */
    break;

  case 655:
#line 4847 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args(p, (yyvsp[-3].val), Qnone, (yyvsp[-1].val), Qnone, (yyvsp[0].val), &(yyloc));
		    }
#line 11268 "ripper.c" /* yacc.c:1646  */
    break;

  case 656:
#line 4851 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args(p, (yyvsp[-5].val), Qnone, (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 11276 "ripper.c" /* yacc.c:1646  */
    break;

  case 657:
#line 4855 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args(p, (yyvsp[-1].val), Qnone, Qnone, Qnone, (yyvsp[0].val), &(yyloc));
		    }
#line 11284 "ripper.c" /* yacc.c:1646  */
    break;

  case 658:
#line 4859 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args(p, Qnone, (yyvsp[-3].val), (yyvsp[-1].val), Qnone, (yyvsp[0].val), &(yyloc));
		    }
#line 11292 "ripper.c" /* yacc.c:1646  */
    break;

  case 659:
#line 4863 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args(p, Qnone, (yyvsp[-5].val), (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 11300 "ripper.c" /* yacc.c:1646  */
    break;

  case 660:
#line 4867 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args(p, Qnone, (yyvsp[-1].val), Qnone, Qnone, (yyvsp[0].val), &(yyloc));
		    }
#line 11308 "ripper.c" /* yacc.c:1646  */
    break;

  case 661:
#line 4871 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args(p, Qnone, (yyvsp[-3].val), Qnone, (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 11316 "ripper.c" /* yacc.c:1646  */
    break;

  case 662:
#line 4875 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args(p, Qnone, Qnone, (yyvsp[-1].val), Qnone, (yyvsp[0].val), &(yyloc));
		    }
#line 11324 "ripper.c" /* yacc.c:1646  */
    break;

  case 663:
#line 4879 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args(p, Qnone, Qnone, (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 11332 "ripper.c" /* yacc.c:1646  */
    break;

  case 664:
#line 4883 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args(p, Qnone, Qnone, Qnone, Qnone, (yyvsp[0].val), &(yyloc));
		    }
#line 11340 "ripper.c" /* yacc.c:1646  */
    break;

  case 665:
#line 4887 "ripper.y" /* yacc.c:1646  */
    {
			arg_var(p, idFWD_REST);
			arg_var(p, idFWD_KWREST);
			arg_var(p, idFWD_BLOCK);
#if 0
			(yyval.val) = new_args_tail(p, Qnone, idFWD_KWREST, idFWD_BLOCK, &(yylsp[0]));
			(yyval.val) = new_args(p, Qnone, Qnone, idFWD_REST, Qnone, (yyval.val), &(yyloc));
#endif
			(yyval.val)=params_new(Qnone, Qnone, (yyvsp[0].val), Qnone, Qnone, Qnone, Qnone);
		    }
#line 11355 "ripper.c" /* yacc.c:1646  */
    break;

  case 666:
#line 4898 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = new_args_tail(p, Qnone, Qnone, Qnone, &(yylsp[0]));
			(yyval.val) = new_args(p, Qnone, Qnone, Qnone, Qnone, (yyval.val), &(yylsp[0]));
		    }
#line 11364 "ripper.c" /* yacc.c:1646  */
    break;

  case 667:
#line 4905 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = idDot3;
#endif
			{VALUE v1;v1=dispatch0(args_forward);(yyval.val)=v1;}
		    }
#line 11375 "ripper.c" /* yacc.c:1646  */
    break;

  case 668:
#line 4914 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			yyerror1(&(yylsp[0]), "formal argument cannot be a constant");
			(yyval.val) = 0;
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(param_error,v1);(yyval.val)=v2;}ripper_error(p);
		    }
#line 11387 "ripper.c" /* yacc.c:1646  */
    break;

  case 669:
#line 4922 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			yyerror1(&(yylsp[0]), "formal argument cannot be an instance variable");
			(yyval.val) = 0;
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(param_error,v1);(yyval.val)=v2;}ripper_error(p);
		    }
#line 11399 "ripper.c" /* yacc.c:1646  */
    break;

  case 670:
#line 4930 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			yyerror1(&(yylsp[0]), "formal argument cannot be a global variable");
			(yyval.val) = 0;
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(param_error,v1);(yyval.val)=v2;}ripper_error(p);
		    }
#line 11411 "ripper.c" /* yacc.c:1646  */
    break;

  case 671:
#line 4938 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			yyerror1(&(yylsp[0]), "formal argument cannot be a class variable");
			(yyval.val) = 0;
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(param_error,v1);(yyval.val)=v2;}ripper_error(p);
		    }
#line 11423 "ripper.c" /* yacc.c:1646  */
    break;

  case 673:
#line 4949 "ripper.y" /* yacc.c:1646  */
    {
			formal_argument(p, get_id((yyvsp[0].val)));
			p->max_numparam = ORDINAL_PARAM;
			(yyval.val) = (yyvsp[0].val);
		    }
#line 11433 "ripper.c" /* yacc.c:1646  */
    break;

  case 674:
#line 4957 "ripper.y" /* yacc.c:1646  */
    {
			ID id = get_id((yyvsp[0].val));
			arg_var(p, id);
			p->cur_arg = id;
			(yyval.val) = (yyvsp[0].val);
		    }
#line 11444 "ripper.c" /* yacc.c:1646  */
    break;

  case 675:
#line 4966 "ripper.y" /* yacc.c:1646  */
    {
			p->cur_arg = 0;
#if 0
			(yyval.val) = NEW_ARGS_AUX((yyvsp[0].val), 1, &NULL_LOC);
#endif
			(yyval.val)=get_value((yyvsp[0].val));
		    }
#line 11456 "ripper.c" /* yacc.c:1646  */
    break;

  case 676:
#line 4974 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			ID tid = internal_id(p);
			YYLTYPE loc;
			loc.beg_pos = (yylsp[-1]).beg_pos;
			loc.end_pos = (yylsp[-1]).beg_pos;
			arg_var(p, tid);
			if (dyna_in_block(p)) {
			    (yyvsp[-1].val)->nd_value = NEW_DVAR(tid, &loc);
			}
			else {
			    (yyvsp[-1].val)->nd_value = NEW_LVAR(tid, &loc);
			}
			(yyval.val) = NEW_ARGS_AUX(tid, 1, &NULL_LOC);
			(yyval.val)->nd_next = (yyvsp[-1].val);
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(mlhs_paren,v1);(yyval.val)=v2;}
		    }
#line 11479 "ripper.c" /* yacc.c:1646  */
    break;

  case 677:
#line 4995 "ripper.y" /* yacc.c:1646  */
    {(yyval.val)=rb_ary_new3(1, get_value((yyvsp[0].val)));}
#line 11485 "ripper.c" /* yacc.c:1646  */
    break;

  case 678:
#line 4997 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = (yyvsp[-2].val);
			(yyval.val)->nd_plen++;
			(yyval.val)->nd_next = block_append(p, (yyval.val)->nd_next, (yyvsp[0].val)->nd_next);
			rb_discard_node(p, (yyvsp[0].val));
#endif
			(yyval.val)=rb_ary_push((yyvsp[-2].val), get_value((yyvsp[0].val)));
		    }
#line 11499 "ripper.c" /* yacc.c:1646  */
    break;

  case 679:
#line 5010 "ripper.y" /* yacc.c:1646  */
    {
			ID id = get_id((yyvsp[0].val));
			arg_var(p, formal_argument(p, id));
			p->cur_arg = id;
			p->max_numparam = ORDINAL_PARAM;
			(yyval.val) = (yyvsp[0].val);
		    }
#line 11511 "ripper.c" /* yacc.c:1646  */
    break;

  case 680:
#line 5020 "ripper.y" /* yacc.c:1646  */
    {
			p->cur_arg = 0;
#if 0
			(yyval.val) = new_kw_arg(p, assignable(p, (yyvsp[-1].val), (yyvsp[0].val), &(yyloc)), &(yyloc));
#endif
			(yyval.val)=rb_assoc_new(get_value(assignable(p, (yyvsp[-1].val))), get_value((yyvsp[0].val)));
		    }
#line 11523 "ripper.c" /* yacc.c:1646  */
    break;

  case 681:
#line 5028 "ripper.y" /* yacc.c:1646  */
    {
			p->cur_arg = 0;
#if 0
			(yyval.val) = new_kw_arg(p, assignable(p, (yyvsp[0].val), NODE_SPECIAL_REQUIRED_KEYWORD, &(yyloc)), &(yyloc));
#endif
			(yyval.val)=rb_assoc_new(get_value(assignable(p, (yyvsp[0].val))), 0);
		    }
#line 11535 "ripper.c" /* yacc.c:1646  */
    break;

  case 682:
#line 5038 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = new_kw_arg(p, assignable(p, (yyvsp[-1].val), (yyvsp[0].val), &(yyloc)), &(yyloc));
#endif
			(yyval.val)=rb_assoc_new(get_value(assignable(p, (yyvsp[-1].val))), get_value((yyvsp[0].val)));
		    }
#line 11546 "ripper.c" /* yacc.c:1646  */
    break;

  case 683:
#line 5045 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = new_kw_arg(p, assignable(p, (yyvsp[0].val), NODE_SPECIAL_REQUIRED_KEYWORD, &(yyloc)), &(yyloc));
#endif
			(yyval.val)=rb_assoc_new(get_value(assignable(p, (yyvsp[0].val))), 0);
		    }
#line 11557 "ripper.c" /* yacc.c:1646  */
    break;

  case 684:
#line 5054 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = (yyvsp[0].val);
#endif
			(yyval.val)=rb_ary_new3(1, get_value((yyvsp[0].val)));
		    }
#line 11568 "ripper.c" /* yacc.c:1646  */
    break;

  case 685:
#line 5061 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = kwd_append((yyvsp[-2].val), (yyvsp[0].val));
#endif
			(yyval.val)=rb_ary_push((yyvsp[-2].val), get_value((yyvsp[0].val)));
		    }
#line 11579 "ripper.c" /* yacc.c:1646  */
    break;

  case 686:
#line 5071 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = (yyvsp[0].val);
#endif
			(yyval.val)=rb_ary_new3(1, get_value((yyvsp[0].val)));
		    }
#line 11590 "ripper.c" /* yacc.c:1646  */
    break;

  case 687:
#line 5078 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = kwd_append((yyvsp[-2].val), (yyvsp[0].val));
#endif
			(yyval.val)=rb_ary_push((yyvsp[-2].val), get_value((yyvsp[0].val)));
		    }
#line 11601 "ripper.c" /* yacc.c:1646  */
    break;

  case 690:
#line 5091 "ripper.y" /* yacc.c:1646  */
    {
#if 0
#endif
			{VALUE v1,v2;v1=Qnil;v2=dispatch1(nokw_param,v1);(yyval.val)=v2;}
		    }
#line 11611 "ripper.c" /* yacc.c:1646  */
    break;

  case 691:
#line 5099 "ripper.y" /* yacc.c:1646  */
    {
			arg_var(p, shadowing_lvar(p, get_id((yyvsp[0].val))));
#if 0
			(yyval.val) = (yyvsp[0].val);
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(kwrest_param,v1);(yyval.val)=v2;}
		    }
#line 11623 "ripper.c" /* yacc.c:1646  */
    break;

  case 692:
#line 5107 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = internal_id(p);
			arg_var(p, (yyval.val));
#endif
			{VALUE v1,v2;v1=Qnil;v2=dispatch1(kwrest_param,v1);(yyval.val)=v2;}
		    }
#line 11635 "ripper.c" /* yacc.c:1646  */
    break;

  case 693:
#line 5117 "ripper.y" /* yacc.c:1646  */
    {
			p->cur_arg = 0;
#if 0
			(yyval.val) = NEW_OPT_ARG(0, assignable(p, (yyvsp[-2].val), (yyvsp[0].val), &(yyloc)), &(yyloc));
#endif
			(yyval.val)=rb_assoc_new(get_value(assignable(p, (yyvsp[-2].val))), get_value((yyvsp[0].val)));
		    }
#line 11647 "ripper.c" /* yacc.c:1646  */
    break;

  case 694:
#line 5127 "ripper.y" /* yacc.c:1646  */
    {
			p->cur_arg = 0;
#if 0
			(yyval.val) = NEW_OPT_ARG(0, assignable(p, (yyvsp[-2].val), (yyvsp[0].val), &(yyloc)), &(yyloc));
#endif
			(yyval.val)=rb_assoc_new(get_value(assignable(p, (yyvsp[-2].val))), get_value((yyvsp[0].val)));
		    }
#line 11659 "ripper.c" /* yacc.c:1646  */
    break;

  case 695:
#line 5137 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = (yyvsp[0].val);
#endif
			(yyval.val)=rb_ary_new3(1, get_value((yyvsp[0].val)));
		    }
#line 11670 "ripper.c" /* yacc.c:1646  */
    break;

  case 696:
#line 5144 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = opt_arg_append((yyvsp[-2].val), (yyvsp[0].val));
#endif
			(yyval.val)=rb_ary_push((yyvsp[-2].val), get_value((yyvsp[0].val)));
		    }
#line 11681 "ripper.c" /* yacc.c:1646  */
    break;

  case 697:
#line 5153 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = (yyvsp[0].val);
#endif
			(yyval.val)=rb_ary_new3(1, get_value((yyvsp[0].val)));
		    }
#line 11692 "ripper.c" /* yacc.c:1646  */
    break;

  case 698:
#line 5160 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = opt_arg_append((yyvsp[-2].val), (yyvsp[0].val));
#endif
			(yyval.val)=rb_ary_push((yyvsp[-2].val), get_value((yyvsp[0].val)));
		    }
#line 11703 "ripper.c" /* yacc.c:1646  */
    break;

  case 701:
#line 5173 "ripper.y" /* yacc.c:1646  */
    {
			arg_var(p, shadowing_lvar(p, get_id((yyvsp[0].val))));
#if 0
			(yyval.val) = (yyvsp[0].val);
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(rest_param,v1);(yyval.val)=v2;}
		    }
#line 11715 "ripper.c" /* yacc.c:1646  */
    break;

  case 702:
#line 5181 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = internal_id(p);
			arg_var(p, (yyval.val));
#endif
			{VALUE v1,v2;v1=Qnil;v2=dispatch1(rest_param,v1);(yyval.val)=v2;}
		    }
#line 11727 "ripper.c" /* yacc.c:1646  */
    break;

  case 705:
#line 5195 "ripper.y" /* yacc.c:1646  */
    {
			arg_var(p, shadowing_lvar(p, get_id((yyvsp[0].val))));
#if 0
			(yyval.val) = (yyvsp[0].val);
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(blockarg,v1);(yyval.val)=v2;}
		    }
#line 11739 "ripper.c" /* yacc.c:1646  */
    break;

  case 706:
#line 5205 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = (yyvsp[0].val);
		    }
#line 11747 "ripper.c" /* yacc.c:1646  */
    break;

  case 707:
#line 5209 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = Qnull;
		    }
#line 11755 "ripper.c" /* yacc.c:1646  */
    break;

  case 708:
#line 5215 "ripper.y" /* yacc.c:1646  */
    {
			value_expr((yyvsp[0].val));
			(yyval.val) = (yyvsp[0].val);
		    }
#line 11764 "ripper.c" /* yacc.c:1646  */
    break;

  case 709:
#line 5219 "ripper.y" /* yacc.c:1646  */
    {SET_LEX_STATE(EXPR_BEG);}
#line 11770 "ripper.c" /* yacc.c:1646  */
    break;

  case 710:
#line 5220 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			switch (nd_type((yyvsp[-1].val))) {
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
			    value_expr((yyvsp[-1].val));
			    break;
			}
			(yyval.val) = (yyvsp[-1].val);
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(paren,v1);(yyval.val)=v2;}
		    }
#line 11796 "ripper.c" /* yacc.c:1646  */
    break;

  case 712:
#line 5245 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = (yyvsp[-1].val);
#endif
			{VALUE v1,v2;v1=(yyvsp[-1].val);v2=dispatch1(assoclist_from_args,v1);(yyval.val)=v2;}
		    }
#line 11807 "ripper.c" /* yacc.c:1646  */
    break;

  case 713:
#line 5254 "ripper.y" /* yacc.c:1646  */
    {(yyval.val)=rb_ary_new3(1, get_value((yyvsp[0].val)));}
#line 11813 "ripper.c" /* yacc.c:1646  */
    break;

  case 714:
#line 5256 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			NODE *assocs = (yyvsp[-2].val);
			NODE *tail = (yyvsp[0].val);
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
			(yyval.val) = assocs;
#endif
			(yyval.val)=rb_ary_push((yyvsp[-2].val), get_value((yyvsp[0].val)));
		    }
#line 11838 "ripper.c" /* yacc.c:1646  */
    break;

  case 715:
#line 5279 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			if (nd_type((yyvsp[-2].val)) == NODE_STR) {
			    nd_set_type((yyvsp[-2].val), NODE_LIT);
			    RB_OBJ_WRITE(p->ast, &(yyvsp[-2].val)->nd_lit, rb_fstring((yyvsp[-2].val)->nd_lit));
			}
			(yyval.val) = list_append(p, NEW_LIST((yyvsp[-2].val), &(yyloc)), (yyvsp[0].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-2].val);v2=(yyvsp[0].val);v3=dispatch2(assoc_new,v1,v2);(yyval.val)=v3;}
		    }
#line 11853 "ripper.c" /* yacc.c:1646  */
    break;

  case 716:
#line 5290 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			(yyval.val) = list_append(p, NEW_LIST(NEW_LIT(ID2SYM((yyvsp[-1].val)), &(yylsp[-1])), &(yyloc)), (yyvsp[0].val));
#endif
			{VALUE v1,v2,v3;v1=(yyvsp[-1].val);v2=(yyvsp[0].val);v3=dispatch2(assoc_new,v1,v2);(yyval.val)=v3;}
		    }
#line 11864 "ripper.c" /* yacc.c:1646  */
    break;

  case 717:
#line 5297 "ripper.y" /* yacc.c:1646  */
    {
#if 0
			YYLTYPE loc = code_loc_gen(&(yylsp[-3]), &(yylsp[-1]));
			(yyval.val) = list_append(p, NEW_LIST(dsym_node(p, (yyvsp[-2].val), &loc), &loc), (yyvsp[0].val));
#endif
			{VALUE v1,v2,v3,v4,v5;v1=(yyvsp[-2].val);v2=dispatch1(dyna_symbol,v1);v3=v2;v4=(yyvsp[0].val);v5=dispatch2(assoc_new,v3,v4);(yyval.val)=v5;}
		    }
#line 11876 "ripper.c" /* yacc.c:1646  */
    break;

  case 718:
#line 5305 "ripper.y" /* yacc.c:1646  */
    {
#if 0
                        if (nd_type((yyvsp[0].val)) == NODE_HASH &&
                            !((yyvsp[0].val)->nd_head && (yyvsp[0].val)->nd_head->nd_alen)) {
                            static VALUE empty_hash;
                            if (!empty_hash) {
                                empty_hash = rb_obj_freeze(rb_hash_new());
                                rb_gc_register_mark_object(empty_hash);
                            }
                            (yyval.val) = list_append(p, NEW_LIST(0, &(yyloc)), NEW_LIT(empty_hash, &(yyloc)));
                        }
                        else
                            (yyval.val) = list_append(p, NEW_LIST(0, &(yyloc)), (yyvsp[0].val));
#endif
			{VALUE v1,v2;v1=(yyvsp[0].val);v2=dispatch1(assoc_splat,v1);(yyval.val)=v2;}
		    }
#line 11897 "ripper.c" /* yacc.c:1646  */
    break;

  case 744:
#line 5370 "ripper.y" /* yacc.c:1646  */
    {yyerrok;token_flush(p);}
#line 11903 "ripper.c" /* yacc.c:1646  */
    break;

  case 745:
#line 5371 "ripper.y" /* yacc.c:1646  */
    {token_flush(p);}
#line 11909 "ripper.c" /* yacc.c:1646  */
    break;

  case 747:
#line 5375 "ripper.y" /* yacc.c:1646  */
    {yyerrok;}
#line 11915 "ripper.c" /* yacc.c:1646  */
    break;

  case 748:
#line 5379 "ripper.y" /* yacc.c:1646  */
    {
			(yyval.val) = Qnull;
		    }
#line 11923 "ripper.c" /* yacc.c:1646  */
    break;


#line 11927 "ripper.c" /* yacc.c:1646  */
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
#line 5383 "ripper.y" /* yacc.c:1906  */

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

#include "ruby/regex.h"
#include "ruby/util.h"

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
    ruby_show_error_line(p->error_buffer, yylloc, p->ruby_sourceline, p->lex.lastline);
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
    dispatch1(parse_error, STR_NEW2(msg));
    ripper_error(p);
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
	ruby_sized_xfree(tbl, sizeof(tbl));
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

    if (!compile_for_eval && rb_safe_level() == 0 && !NIL_P(p->ruby_sourcefile_string)) {
	p->debug_lines = debug_lines(p->ruby_sourcefile_string);
	if (p->debug_lines && p->ruby_sourceline > 0) {
	    VALUE str = STR_NEW0();
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
rb_compile_string(const char *f, VALUE s, int line)
{
    must_be_ascii_compatible(s);
    return parser_compile_string(rb_parser_new(), rb_filesystem_str_new_cstr(f), s, line);
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

rb_ast_t*
rb_compile_cstr(const char *f, const char *s, int len, int line)
{
    VALUE str = rb_str_new(s, len);
    return parser_compile_string(rb_parser_new(), rb_filesystem_str_new_cstr(f), str, line);
}

rb_ast_t*
rb_parser_compile_cstr(VALUE vparser, const char *f, const char *s, int len, int line)
{
    VALUE str = rb_str_new(s, len);
    return parser_compile_string(vparser, rb_filesystem_str_new_cstr(f), str, line);
}

VALUE rb_io_gets_internal(VALUE io);

static VALUE
lex_io_gets(struct parser_params *p, VALUE io)
{
    return rb_io_gets_internal(io);
}

rb_ast_t*
rb_compile_file(const char *f, VALUE file, int start)
{
    VALUE vparser = rb_parser_new();

    return rb_parser_compile_file(vparser, f, file, start);
}

rb_ast_t*
rb_parser_compile_file(VALUE vparser, const char *f, VALUE file, int start)
{
    return rb_parser_compile_file_path(vparser, rb_filesystem_str_new_cstr(f), file, start);
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
    else if (!p->cr_seen) {
	p->cr_seen = TRUE;
	/* carried over with p->lex.nextline for nextc() */
	rb_warn0("encountered \\r in middle of line, treated as a mere space");
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
	else if (ISDIGIT(c)) {
	    return tSTRING_DVAR;
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
    rb_warning1("ambiguous first argument; put parentheses or a space even after `%c' operator", WARN_I(c));
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
    rb_compile_warning(p->ruby_sourcefile, p->ruby_sourceline, "invalid value for %s: %s", name, val);
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

    if (IS_BEG()) {
	int term;
	int paren;

	c = nextc(p);
      quotation:
	if (c == -1 || !ISALNUM(c)) {
	    term = c;
	    c = 'Q';
	}
	else {
	    term = nextc(p);
	    if (rb_enc_isalnum(term, p->enc) || !parser_isascii(p)) {
		yyerror0("unknown type of %string");
		return 0;
	    }
	}
	if (c == -1 || term == -1) {
	    compile_error(p, "unterminated quoted string meets end of file");
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
	compile_error(p, "numbered parameter outside block");
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
	    SET_LEX_STATE(kw->state);
	    if (IS_lex_state_for(state, EXPR_FNAME)) {
		set_yylval_name(rb_intern2(tok(p), toklen(p)));
		return kw->id[0];
	    }
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
      case ' ': case '\t': case '\f': case '\r':
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
	    if (!c && p->in_kwarg) {
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
	    if (strncmp(p->lex.pcur, "begin", 5) == 0 && ISSPACE(p->lex.pcur[5])) {
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
		    if (c != '=') continue;
		    if (c == '=' && strncmp(p->lex.pcur, "end", 3) == 0 &&
			(p->lex.pcur + 3 == p->lex.pend || ISSPACE(p->lex.pcur[3]))) {
			break;
		    }
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
	switch (c = nextc(p)) {
	  case '.':
	    if ((c = nextc(p)) == '.') {
		return is_beg ? tBDOT3 : tDOT3;
	    }
	    pushback(p, c);
	    return is_beg ? tBDOT2 : tDOT2;
	  case ':':
	    switch (c = nextc(p)) {
	      default:
		if (!parser_is_identchar(p)) break;
		/* fallthru */
	      case '!': case '%': case '&': case '*': case '+':
	      case '-': case '/': case '<': case '=': case '>':
	      case '[': case '^': case '`': case '|': case '~':
		pushback(p, c);
		SET_LEX_STATE(EXPR_DOT);
		return tMETHREF;
	      case -1:
		break;
	    }
	    pushback(p, c);
	    c = ':';
	    break;
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
    if (has_delayed_token(p))
	dispatch_delayed_token(p, t);
    else if (t != 0)
	dispatch_scan_event(p, t);

    if (p->lex.strterm && (p->lex.strterm->flags & STRTERM_HEREDOC))
	RUBY_SET_YYLLOC_FROM_STRTERM_HEREDOC(*yylloc);
    else
	RUBY_SET_YYLLOC(*yylloc);

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

/* concat two string literals */
static NODE *
literal_concat(struct parser_params *p, NODE *head, NODE *tail, const YYLTYPE *loc)
{
    enum node_type htype;
    NODE *headlast;
    VALUE lit;

    if (!head) return tail;
    if (!tail) return head;

    htype = nd_type(head);
    if (htype == NODE_EVSTR) {
	NODE *node = NEW_DSTR(STR_NEW0(), loc);
        RB_OBJ_WRITTEN(p->ast, Qnil, node->nd_lit);
	head = list_append(p, node, head);
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
	if (htype == NODE_DSTR && (headlast = head->nd_next->nd_end->nd_head) &&
	    nd_type(headlast) == NODE_STR) {
	    htype = NODE_STR;
	    lit = headlast->nd_lit;
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
	    head->nd_next->nd_end->nd_next = tail->nd_next;
	    head->nd_next->nd_end = tail->nd_next->nd_end;
	    rb_discard_node(p, tail);
	}
	else if (htype == NODE_DSTR && (headlast = head->nd_next->nd_end->nd_head) &&
		 nd_type(headlast) == NODE_STR) {
	    lit = headlast->nd_lit;
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
	NODE * dstr = NEW_DSTR(STR_NEW0(), &node->nd_loc);
        RB_OBJ_WRITTEN(p->ast, Qnil, dstr->nd_lit);
	node = list_append(p, dstr, node);
    }
    return node;
}

static NODE *
new_evstr(struct parser_params *p, NODE *node, const YYLTYPE *loc)
{
    NODE *head = node;

    if (node) {
	switch (nd_type(node)) {
	  case NODE_STR: case NODE_DSTR: case NODE_EVSTR:
	    return node;
	}
    }
    return NEW_EVSTR(head, loc);
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

/* As Ripper#warn does not have arguments for the location, so the
 * following messages cannot be separated */
#define WARN_LOCATION(type) do { \
    if (p->warn_location) { \
	int line; \
	VALUE file = rb_source_location(&line); \
	rb_warn3(type" in eval may not return location in binding;" \
		 " use Binding#source_location instead\n" \
		 "%"PRIsWARN":%d: warning: in `%"PRIsWARN"'", \
		 file, WARN_I(line), rb_id2str(rb_frame_this_func())); \
    } \
} while (0)

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
	WARN_LOCATION("__FILE__");
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
	WARN_LOCATION("__LINE__");
	return NEW_LIT(INT2FIX(p->tokline), loc);
      case keyword__ENCODING__:
        node = NEW_LIT(rb_enc_from_encoding(p->enc), loc);
        RB_OBJ_WRITTEN(p->ast, Qnil, node->nd_lit);
        return node;

    }
    switch (id_type(id)) {
      case ID_INTERNAL:
	{
	    int idx = vtable_included(p->lvtbl->args, id);
	    if (idx) return NEW_DVAR(id, loc);
	}
	break;
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
	if (!p->in_defined && RTEST(ruby_verbose) && past_dvar_p(p, id)) {
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
	    if (nd_type(list->nd_head) == NODE_STR) {
		VALUE tail = list->nd_head->nd_lit;
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
	  case ID_INTERNAL:
	    return vtable_included(p->lvtbl->args, id);
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
    if (p->debug_output == rb_stdout)
	p->debug_output = rb_stderr;
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
	if (!p->in_def) return NODE_CDECL;
	*err = "dynamic constant assignment";
	return -1;
      case ID_CLASS: return NODE_CVASGN;
      case ID_INTERNAL:
	{
	    int idx = vtable_included(p->lvtbl->args, id);
	    if (idx) {
		compile_error(p, "Can't assign to numbered parameter @%d", idx);
		break;
	    }
	}
	/* fallthru */
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
	RRATIONAL_SET_NUM(lit, negate_lit(p, RRATIONAL(lit)->num));
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

    p->ruby_sourceline = saved_line;
    nd_set_loc(tail, loc);

    return tail;
}

static NODE*
new_args_tail(struct parser_params *p, NODE *kw_args, ID kw_rest_arg, ID block, const YYLTYPE *loc)
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

	args->kw_rest_arg = NEW_DVAR(kw_rest_arg, loc);
	args->kw_rest_arg->nd_cflag = kw_bits;
    }
    else if (kw_rest_arg == idNil) {
	args->no_kwarg = 1;
    }
    else if (kw_rest_arg) {
	args->kw_rest_arg = NEW_DVAR(kw_rest_arg, loc);
    }

    p->ruby_sourceline = saved_line;
    return node;
}

static NODE *
args_with_numbered(struct parser_params *p, NODE *args, int max_numparam)
{
    if (max_numparam > NO_PARAM) {
	if (!args) args = new_args_tail(p, 0, 0, 0, 0);
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
    node = NEW_NODE(NODE_ARYPTN, 0, 0, apinfo, loc);
    apinfo->imemo = tmpbuf;
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

static void
error_duplicate_keys(struct parser_params *p, NODE *hash)
{
    st_table *literal_keys = st_init_numtable_with_size(hash->nd_alen / 2);
    while (hash && hash->nd_head && hash->nd_next) {
	NODE *head = hash->nd_head;
	NODE *next = hash->nd_next->nd_next;
	VALUE key = (VALUE)head;
	if (nd_type(head) != NODE_LIT) {
	    yyerror1(&head->nd_loc, "key must be symbol literal");
	}
	if (st_is_member(literal_keys, (key = head->nd_lit))) {
	    yyerror1(&head->nd_loc, "duplicated key name");
	}
	else {
	    st_insert(literal_keys, (st_data_t)key, (st_data_t)hash);
	}
	hash = next;
    }
    st_free_table(literal_keys);
    return;
}

static NODE *
new_unique_key_hash(struct parser_params *p, NODE *hash, const YYLTYPE *loc)
{
    if (hash) {
        error_duplicate_keys(p, hash);
    }
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
    if (p->in_def) {
	yyerror1(loc, "dynamic constant assignment");
    }
    return NEW_CDECL(0, 0, (path), loc);
}
#else
static VALUE
const_decl(struct parser_params *p, VALUE path)
{
    if (p->in_def) {
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
    int i, cnt;
    ID *v, *u;

    if (!local->used) return;
    v = local->vars->tbl;
    u = local->used->tbl;
    cnt = local->used->pos;
    if (cnt != local->vars->pos) {
	rb_parser_fatal(p, "local->used->pos != local->vars->pos");
    }
    for (i = 0; i < cnt; ++i) {
	if (!v[i] || (u[i] & LVAR_USED)) continue;
	if (is_private_local_id(v[i])) continue;
	rb_warn1L((int)u[i], "assigned but unused variable - %"PRIsWARN, rb_id2str(v[i]));
    }
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
    VALUE tbl = 0;

    if (cnt <= 0) return 0;
    tbl = rb_imemo_tmpbuf_auto_free_pointer();
    buf = ALLOC_N(ID, cnt + 2);
    rb_imemo_tmpbuf_set_ptr(tbl, buf);
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
	rb_imemo_tmpbuf_set_ptr(tbl, buf);
    }
    buf[0] = cnt;
    buf[cnt + 1] = (ID)tbl;
    RB_OBJ_WRITTEN(p->ast, Qnil, tbl);

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
arg_var(struct parser_params *p, ID id)
{
    vtable_add(p->lvtbl->args, id);
}

static void
local_var(struct parser_params *p, ID id)
{
    if (NUMPARAM_ID_P(id)) {
	rb_warn1("`_%d' is used as numbered parameter",
		 WARN_I(NUMPARAM_ID_TO_IDX(id)));
    }
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

    if (vars == DVARS_INHERIT) {
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

void
rb_parser_warn_location(VALUE vparser, int warn)
{
    struct parser_params *p;
    TypedData_Get_Struct(vparser, struct parser_params, &parser_data_type, p);
    p->warn_location = warn;
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
    p->debug_output = rb_stdout;
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

    /* ignore newline, +/- is a sign. */
    rb_define_const(Ripper, "EXPR_BEG", INT2NUM(EXPR_BEG));
    /* newline significant, +/- is an operator. */
    rb_define_const(Ripper, "EXPR_END", INT2NUM(EXPR_END));
    /* ditto, and unbound braces. */
    rb_define_const(Ripper, "EXPR_ENDARG", INT2NUM(EXPR_ENDARG));
    /* ditto, and unbound braces. */
    rb_define_const(Ripper, "EXPR_ENDFN", INT2NUM(EXPR_ENDFN));
    /* newline significant, +/- is an operator. */
    rb_define_const(Ripper, "EXPR_ARG", INT2NUM(EXPR_ARG));
    /* newline significant, +/- is an operator. */
    rb_define_const(Ripper, "EXPR_CMDARG", INT2NUM(EXPR_CMDARG));
    /* newline significant, +/- is an operator. */
    rb_define_const(Ripper, "EXPR_MID", INT2NUM(EXPR_MID));
    /* ignore newline, no reserved words. */
    rb_define_const(Ripper, "EXPR_FNAME", INT2NUM(EXPR_FNAME));
    /* right after `.' or `::', no reserved words. */
    rb_define_const(Ripper, "EXPR_DOT", INT2NUM(EXPR_DOT));
    /* immediate after `class', no here document. */
    rb_define_const(Ripper, "EXPR_CLASS", INT2NUM(EXPR_CLASS));
    /* flag bit, label is allowed. */
    rb_define_const(Ripper, "EXPR_LABEL", INT2NUM(EXPR_LABEL));
    /* flag bit, just after a label. */
    rb_define_const(Ripper, "EXPR_LABELED", INT2NUM(EXPR_LABELED));
    /* symbol literal as FNAME. */
    rb_define_const(Ripper, "EXPR_FITEM", INT2NUM(EXPR_FITEM));
    /* equals to +EXPR_BEG+ */
    rb_define_const(Ripper, "EXPR_VALUE", INT2NUM(EXPR_VALUE));
    /* equals to <tt>(EXPR_BEG | EXPR_MID | EXPR_CLASS)</tt> */
    rb_define_const(Ripper, "EXPR_BEG_ANY", INT2NUM(EXPR_BEG_ANY));
    /* equals to <tt>(EXPR_ARG | EXPR_CMDARG)</tt> */
    rb_define_const(Ripper, "EXPR_ARG_ANY", INT2NUM(EXPR_ARG_ANY));
    /* equals to <tt>(EXPR_END | EXPR_ENDARG | EXPR_ENDFN)</tt> */
    rb_define_const(Ripper, "EXPR_END_ANY", INT2NUM(EXPR_END_ANY));
    /* equals to +0+ */
    rb_define_const(Ripper, "EXPR_NONE", INT2NUM(EXPR_NONE));

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
