/* A Bison parser, made by GNU Bison 3.3.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2019 Free Software Foundation,
   Inc.

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

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.3.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 1

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 12 "parse.y" /* yacc.c:337  */


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
};

#define NUMPARAM_MAX 100 /* INT_MAX */

#define DVARS_INHERIT ((void*)1)
#define DVARS_TOPSCOPE NULL
#define DVARS_TERMINAL_P(tbl) ((tbl) == DVARS_INHERIT || (tbl) == DVARS_TOPSCOPE)

typedef struct token_info {
    const char *token;
    int linenum;
    int column;
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
    unsigned int in_main: 1;
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
    const struct rb_block *base_block;
#else
    /* Ripper only */

    VALUE delayed;
    int delayed_line;
    int delayed_col;

    VALUE value;
    VALUE result;
    VALUE parsing_thread;
#endif
};

#define new_tmpbuf() \
    (rb_imemo_tmpbuf_t *)add_mark_object(p, rb_imemo_tmpbuf_auto_free_pointer(NULL))

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
#define compile_for_eval	(p->base_block != 0 && !p->in_main)
#endif

#define token_column		((int)(p->lex.ptok - p->lex.pbeg))

#define CALL_Q_P(q) ((q) == TOKEN2VAL(tANDDOT))
#define NODE_CALL_Q(q) (CALL_Q_P(q) ? NODE_QCALL : NODE_CALL)
#define NEW_QCALL(q,r,m,a,loc) NEW_NODE(NODE_CALL_Q(q),r,m,a,loc)

#define lambda_beginning_p() (p->lex.lpar_beg == p->lex.paren_nest)

#ifndef RIPPER
static const ID excessed_comma = 1;
#endif

static enum yytokentype yylex(YYSTYPE*, YYLTYPE*, struct parser_params*);

#ifndef RIPPER
static inline void
rb_discard_node(struct parser_params *p, NODE *n)
{
    rb_ast_delete_node(p->ast, n);
}
#endif

static inline VALUE
add_mark_object(struct parser_params *p, VALUE obj)
{
    if (!SPECIAL_CONST_P(obj)
#ifdef RIPPER
	&& !RB_TYPE_P(obj, T_NODE) /* Ripper jumbles NODE objects and other objects... */
#endif
    ) {
	rb_ast_add_mark_object(p->ast, obj);
    }
    return obj;
}

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

#define make_array(ary, loc) ((ary) ? (nd_set_loc(ary, loc), ary) : NEW_ZARRAY(loc))

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
ID rb_parser_numparam_id(struct parser_params *p, int num);
RUBY_SYMBOL_EXPORT_END

#define numparam_id rb_parser_numparam_id

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

#ifdef RIPPER
# define METHOD_NOT idNOT
#else
# define METHOD_NOT '!'
#endif

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
    } else {
	rest_arg = Qnil;
    }
    t = rb_node_newnode(NODE_ARYPTN, pre_args, rest_arg, post_args, &NULL_LOC);

    add_mark_object(p, pre_args);
    add_mark_object(p, rest_arg);
    add_mark_object(p, post_args);
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
	if (st_lookup(tbl, (st_data_t)RSTRING_PTR(key), 0)) goto error;
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
#define ID2VAL(id) ((VALUE)(id))
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
static ID id_warn, id_warning, id_gets, id_assoc, id_or;
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

static void token_info_push(struct parser_params*, const char *token, const rb_code_location_t *loc);
static void token_info_pop(struct parser_params*, const char *token, const rb_code_location_t *loc);
static void token_info_warn(struct parser_params *p, const char *token, token_info *ptinfo_beg, int same, const rb_code_location_t *loc);

#line 974 "parse.c" /* yacc.c:337  */
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
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
    tNUMPARAM = 314,
    tINTEGER = 315,
    tFLOAT = 316,
    tRATIONAL = 317,
    tIMAGINARY = 318,
    tCHAR = 319,
    tNTH_REF = 320,
    tBACK_REF = 321,
    tSTRING_CONTENT = 322,
    tREGEXP_END = 323,
    tSP = 324,
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
    tCOLON3 = 325,
    tOP_ASGN = 326,
    tASSOC = 327,
    tLPAREN = 328,
    tLPAREN_ARG = 329,
    tRPAREN = 330,
    tLBRACK = 331,
    tLBRACE = 332,
    tLBRACE_ARG = 333,
    tSTAR = 334,
    tDSTAR = 335,
    tAMPER = 336,
    tLAMBDA = 337,
    tSYMBEG = 338,
    tSTRING_BEG = 339,
    tXSTRING_BEG = 340,
    tREGEXP_BEG = 341,
    tWORDS_BEG = 342,
    tQWORDS_BEG = 343,
    tSYMBOLS_BEG = 344,
    tQSYMBOLS_BEG = 345,
    tSTRING_END = 346,
    tSTRING_DEND = 347,
    tSTRING_DBEG = 348,
    tSTRING_DVAR = 349,
    tLAMBEG = 350,
    tLABEL_END = 351,
    tLOWEST = 352,
    tUMINUS_NUM = 353,
    tLAST_TOKEN = 354
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 925 "parse.y" /* yacc.c:352  */

    VALUE val;
    NODE *node;
    ID id;
    int num;
    const struct vtable *vars;
    struct rb_strterm_struct *strterm;

#line 1151 "parse.c" /* yacc.c:352  */
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
typedef unsigned short yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short yytype_int16;
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
#  define YYSIZE_T unsigned
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

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && ! defined __ICC && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
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
#define YYLAST   14121

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  156
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  245
/* YYNRULES -- Number of rules.  */
#define YYNRULES  737
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  1216

#define YYUNDEFTOK  2
#define YYMAXUTOK   354

/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                                \
  ((unsigned) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,    72,
     155,    75,    73,    74,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,   154,   142,     2,     2,     2,   140,   135,     2,
     150,   151,   138,   136,   148,   137,    69,   139,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   130,   153,
     132,   128,   131,   129,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   147,    70,   152,   134,     2,   149,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   145,   133,   146,   143,     2,    89,    90,
      91,    92,    76,    77,    78,    79,    95,    96,    84,    83,
      80,    81,    82,    87,    88,    93,    94,    98,    85,    86,
      97,    99,     2,     2,     2,     2,     2,     2,     2,     2,
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
      65,    66,    67,    68,    71,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   141,   144
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,  1123,  1123,  1123,  1149,  1155,  1162,  1169,  1176,  1182,
    1183,  1189,  1202,  1200,  1211,  1222,  1228,  1235,  1242,  1249,
    1255,  1260,  1259,  1269,  1269,  1276,  1283,  1293,  1301,  1308,
    1316,  1324,  1336,  1348,  1358,  1372,  1373,  1381,  1389,  1396,
    1399,  1407,  1415,  1424,  1432,  1440,  1448,  1456,  1466,  1471,
    1480,  1483,  1484,  1488,  1492,  1496,  1500,  1503,  1510,  1510,
    1510,  1516,  1517,  1520,  1521,  1530,  1540,  1550,  1559,  1570,
    1577,  1584,  1591,  1598,  1606,  1614,  1621,  1628,  1637,  1638,
    1647,  1648,  1657,  1664,  1671,  1678,  1685,  1692,  1699,  1706,
    1713,  1720,  1729,  1730,  1739,  1746,  1755,  1762,  1771,  1778,
    1785,  1792,  1799,  1806,  1813,  1820,  1827,  1837,  1844,  1851,
    1858,  1865,  1872,  1879,  1886,  1893,  1903,  1910,  1913,  1920,
    1927,  1936,  1937,  1938,  1939,  1944,  1951,  1958,  1961,  1968,
    1968,  1978,  1979,  1980,  1981,  1982,  1983,  1984,  1985,  1986,
    1987,  1988,  1989,  1990,  1991,  1992,  1993,  1994,  1995,  1996,
    1997,  1998,  1999,  2000,  2001,  2002,  2003,  2004,  2005,  2006,
    2007,  2010,  2010,  2010,  2011,  2011,  2012,  2012,  2012,  2013,
    2013,  2013,  2013,  2014,  2014,  2014,  2014,  2015,  2015,  2015,
    2016,  2016,  2016,  2016,  2017,  2017,  2017,  2017,  2018,  2018,
    2018,  2018,  2019,  2019,  2019,  2019,  2020,  2020,  2020,  2020,
    2021,  2021,  2024,  2031,  2038,  2046,  2054,  2062,  2070,  2078,
    2085,  2093,  2102,  2111,  2123,  2135,  2147,  2159,  2163,  2167,
    2171,  2175,  2179,  2183,  2187,  2191,  2195,  2199,  2203,  2207,
    2211,  2212,  2216,  2220,  2224,  2228,  2232,  2236,  2240,  2244,
    2248,  2252,  2256,  2256,  2261,  2270,  2276,  2277,  2278,  2279,
    2282,  2286,  2293,  2300,  2301,  2305,  2312,  2321,  2326,  2337,
    2346,  2347,  2350,  2351,  2352,  2356,  2363,  2372,  2380,  2387,
    2395,  2403,  2407,  2407,  2444,  2453,  2457,  2463,  2470,  2477,
    2484,  2493,  2494,  2497,  2504,  2511,  2520,  2521,  2522,  2523,
    2524,  2525,  2526,  2527,  2528,  2529,  2530,  2538,  2537,  2552,
    2552,  2559,  2559,  2566,  2573,  2580,  2587,  2594,  2602,  2609,
    2616,  2623,  2630,  2630,  2635,  2639,  2643,  2650,  2651,  2660,
    2659,  2670,  2681,  2692,  2702,  2713,  2712,  2729,  2728,  2743,
    2753,  2799,  2798,  2822,  2821,  2844,  2843,  2867,  2872,  2866,
    2892,  2893,  2892,  2917,  2924,  2931,  2938,  2945,  2954,  2961,
    2967,  2973,  2979,  2985,  2991,  2997,  3003,  3009,  3015,  3021,
    3027,  3033,  3039,  3045,  3051,  3059,  3065,  3071,  3078,  3079,
    3080,  3083,  3084,  3087,  3088,  3100,  3101,  3110,  3111,  3114,
    3121,  3130,  3137,  3146,  3153,  3160,  3167,  3174,  3181,  3188,
    3195,  3202,  3212,  3216,  3220,  3224,  3230,  3235,  3240,  3244,
    3248,  3252,  3256,  3260,  3268,  3272,  3276,  3280,  3284,  3288,
    3292,  3296,  3300,  3306,  3307,  3313,  3322,  3330,  3342,  3346,
    3355,  3357,  3361,  3366,  3372,  3375,  3379,  3384,  3372,  3407,
    3415,  3425,  3430,  3436,  3446,  3460,  3467,  3474,  3483,  3492,
    3500,  3508,  3515,  3523,  3531,  3538,  3545,  3558,  3566,  3576,
    3577,  3576,  3594,  3595,  3594,  3614,  3622,  3629,  3637,  3646,
    3658,  3659,  3663,  3670,  3662,  3683,  3684,  3687,  3688,  3696,
    3706,  3707,  3712,  3720,  3724,  3730,  3733,  3742,  3745,  3752,
    3755,  3756,  3764,  3772,  3777,  3785,  3793,  3798,  3802,  3807,
    3811,  3816,  3822,  3831,  3835,  3844,  3848,  3852,  3856,  3860,
    3863,  3867,  3876,  3880,  3884,  3888,  3893,  3894,  3903,  3912,
    3916,  3920,  3926,  3927,  3936,  3943,  3953,  3968,  3990,  3994,
    4000,  4001,  4010,  4019,  4031,  4043,  4044,  4045,  4046,  4058,
    4072,  4073,  4074,  4075,  4076,  4077,  4078,  4079,  4080,  4088,
    4087,  4100,  4109,  4122,  4129,  4136,  4145,  4157,  4160,  4167,
    4174,  4177,  4181,  4184,  4191,  4194,  4195,  4198,  4214,  4215,
    4216,  4225,  4235,  4244,  4250,  4260,  4266,  4275,  4277,  4286,
    4296,  4302,  4311,  4320,  4330,  4336,  4346,  4352,  4362,  4368,
    4378,  4384,  4394,  4404,  4445,  4447,  4446,  4463,  4467,  4472,
    4476,  4480,  4462,  4501,  4508,  4515,  4522,  4529,  4532,  4533,
    4536,  4546,  4547,  4548,  4549,  4552,  4562,  4563,  4573,  4574,
    4575,  4576,  4579,  4580,  4581,  4582,  4583,  4584,  4587,  4588,
    4589,  4590,  4591,  4592,  4593,  4596,  4609,  4618,  4625,  4634,
    4635,  4639,  4638,  4648,  4656,  4665,  4665,  4679,  4683,  4687,
    4691,  4697,  4702,  4707,  4711,  4715,  4719,  4723,  4727,  4731,
    4735,  4739,  4743,  4747,  4751,  4755,  4759,  4764,  4770,  4778,
    4786,  4794,  4804,  4805,  4813,  4822,  4830,  4851,  4853,  4866,
    4876,  4884,  4894,  4901,  4910,  4917,  4927,  4934,  4943,  4944,
    4947,  4955,  4965,  4975,  4985,  4992,  5001,  5008,  5017,  5018,
    5021,  5029,  5039,  5040,  5043,  5053,  5057,  5066,  5071,  5071,
    5095,  5096,  5105,  5107,  5130,  5141,  5148,  5156,  5169,  5170,
    5171,  5174,  5175,  5176,  5177,  5180,  5181,  5182,  5185,  5186,
    5189,  5190,  5193,  5194,  5197,  5198,  5201,  5202,  5205,  5208,
    5211,  5212,  5213,  5216,  5217,  5220,  5221,  5225
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
  "\"constant\"", "\"class variable\"", "tLABEL", "\"numbered parameter\"",
  "\"integer literal\"", "\"float literal\"", "\"rational literal\"",
  "\"imaginary literal\"", "\"char literal\"", "\"numbered reference\"",
  "\"back reference\"", "\"literal content\"", "tREGEXP_END", "'.'",
  "\"backslash\"", "\"escaped space\"", "\"escaped horizontal tab\"",
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
  "$@4", "command_asgn", "command_rhs", "expr", "expr_value",
  "expr_value_do", "$@5", "$@6", "command_call", "block_command",
  "cmd_brace_block", "fcall", "command", "mlhs", "mlhs_inner",
  "mlhs_basic", "mlhs_item", "mlhs_head", "mlhs_post", "mlhs_node", "lhs",
  "cname", "cpath", "fname", "fitem", "undef_list", "$@7", "op",
  "reswords", "arg", "$@8", "relop", "rel_expr", "arg_value", "aref_args",
  "arg_rhs", "paren_args", "opt_paren_args", "opt_call_args", "call_args",
  "command_args", "$@9", "block_arg", "opt_block_arg", "args", "mrhs_arg",
  "mrhs", "primary", "$@10", "$@11", "$@12", "$@13", "$@14", "@15", "@16",
  "$@17", "@18", "$@19", "@20", "@21", "@22", "@23", "primary_value",
  "k_begin", "k_if", "k_unless", "k_while", "k_until", "k_case", "k_for",
  "k_class", "k_module", "k_def", "k_do", "k_do_block", "k_rescue",
  "k_ensure", "k_when", "k_else", "k_elsif", "k_end", "k_return", "then",
  "do", "if_tail", "opt_else", "for_var", "f_marg", "f_marg_list",
  "f_margs", "block_args_tail", "opt_block_args_tail", "block_param",
  "opt_block_param", "block_param_def", "opt_bv_decl", "bv_decls", "bvar",
  "lambda", "@24", "@25", "@26", "$@27", "f_larglist", "lambda_body",
  "do_block", "block_call", "method_call", "brace_block", "brace_body",
  "@28", "@29", "do_body", "@30", "@31", "case_args", "case_body", "cases",
  "p_case_body", "@32", "$@33", "p_cases", "p_top_expr", "p_top_expr_body",
  "p_expr", "p_as", "p_alt", "p_expr_basic", "p_args", "p_args_head",
  "p_args_tail", "p_args_post", "p_arg", "p_kwargs", "p_kwarg", "p_kw",
  "p_kwrest", "p_value", "p_primitive", "$@34", "p_variable", "p_var_ref",
  "p_const", "opt_rescue", "exc_list", "exc_var", "opt_ensure", "literal",
  "strings", "string", "string1", "xstring", "regexp", "words",
  "word_list", "word", "symbols", "symbol_list", "qwords", "qsymbols",
  "qword_list", "qsym_list", "string_contents", "xstring_contents",
  "regexp_contents", "string_content", "@35", "$@36", "@37", "@38", "@39",
  "@40", "string_dvar", "symbol", "ssym", "sym", "dsym", "numeric",
  "simple_numeric", "user_variable", "keyword_variable", "var_ref",
  "var_lhs", "backref", "superclass", "$@41", "f_arglist", "@42",
  "args_tail", "opt_args_tail", "f_args", "f_bad_arg", "f_norm_arg",
  "f_arg_asgn", "f_arg_item", "f_arg", "f_label", "f_kw", "f_block_kw",
  "f_block_kwarg", "f_kwarg", "kwrest_mark", "f_kwrest", "f_opt",
  "f_block_opt", "f_block_optarg", "f_optarg", "restarg_mark",
  "f_rest_arg", "blkarg_mark", "f_block_arg", "opt_f_block_arg",
  "singleton", "$@43", "assoc_list", "assocs", "assoc", "operation",
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
     315,   316,   317,   318,   319,   320,   321,   322,   323,    46,
      92,   324,     9,    12,    13,    11,   132,   133,   134,   135,
     140,   141,   142,   139,   138,   148,   149,   143,   144,   128,
     129,   130,   131,   145,   146,   136,   137,   150,   147,   151,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,    61,    63,
      58,    62,    60,   124,    94,    38,    43,    45,    42,    47,
      37,   353,    33,   126,   354,   123,   125,    91,    44,    96,
      40,    41,    93,    59,    32,    10
};
# endif

#define YYPACT_NINF -960

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-960)))

#define YYTABLE_NINF -738

#define yytable_value_is_error(Yytable_value) \
  (!!((Yytable_value) == (-738)))

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -960,   133,  3788,  -960,  9467,  -960,  -960,  -960,  8917,  -960,
    -960,  -960,  -960,  -960,  -960,  -960,  9595,  9595,  -960,  -960,
    -960,  5327,  4880,  -960,  -960,  -960,  -960,   275,  8770,    -8,
     -18,    15,  -960,  -960,  -960,  4127,  5029,  -960,  -960,  4284,
    -960,  -960,  -960,  -960,  -960,  -960,  -960,  -960,  -960, 11131,
   11131, 11131, 11131,   113,  6944,  9723,  9979, 10363,  9205,  -960,
    8623,  -960,  -960,  -960,    95,   143,   158,   216,  1201, 11259,
   11131,  -960,   197,  -960,  1076,  -960,   567,  -960,  -960,   130,
     149,    86,  -960,   252, 11515,  -960,   311,  4262,   259,   845,
     870,  -960, 11387, 11387,  -960,  -960,  7940, 11639, 11763, 11887,
    8475,  9595,    74,    55,  -960,  -960,   351,  -960,  -960,  -960,
    -960,  -960,  -960,  -960,  -960,  -960,  -960,  -960,  -960,    47,
     376,  -960,   368,   417,  -960,  -960,  -960,  -960,  -960,  -960,
    -960,  -960,  -960,  -960,  -960,  -960,  -960,  -960,  -960,  -960,
    -960,  -960,  -960,  -960,  -960,  -960,  -960,  -960,  -960,  -960,
    -960,  -960,  -960,  -960,  -960,  -960,  -960,  -960,  -960,  -960,
    -960,  -960,  -960,  -960,  -960,  -960,  -960,  -960,  -960,  -960,
    -960,  -960,  -960,  -960,  -960,  -960,  -960,  -960,  -960,  -960,
    -960,  -960,  -960,  -960,  -960,  -960,  -960,  -960,  -960,  -960,
    -960,  -960,  -960,  -960,  -960,  -960,  -960,  -960,  -960,  -960,
    -960,  -960,  -960,   326,  -960,  -960,  -960,   336, 11131,   439,
    7097, 11131, 11131, 11131,  -960, 11131,  -960,   387,  4262,   418,
    -960,  -960,   395,  1016,    65,   374,   491,   382,   478,  -960,
    -960,  7812,  -960,  9595,  9595,  -960,  -960,  8068,  -960, 11387,
     917,  -960,   446,  7250,  -960,  7403,  -960,  -960,   490,   495,
     130,  -960,  1026,  -960,   570,  4411,  4411,   488,  9723,  -960,
    6944,   506,   197,  -960,  1076,    -8,   535,  -960,  1076,    -8,
     552,   589,   695,  -960,   418,   541,   695,  -960,    -8,   664,
    1201, 12011,   575,  -960,  1034,  1038,  1079,  1106,  -960,  -960,
    -960,  -960,  -960,   367,  -960,   639,   672,   489,  -960,  -960,
    -960,  -960,   650,  -960,  -960,  -960,  -960,  -960,  -960,  -960,
    8196, 11387, 11387, 11387, 11387,  9723, 11387, 11387,  -960,  -960,
    -960,   626,  -960,  -960,  -960,  -960,  -960, 10491,  -960,  6944,
    9336,   603, 10491, 11131, 11131, 11131, 11131, 11131,  -960,  -960,
   11131, 11131, 11131, 11131, 11131, 11131, 11131, 11131, 11131,  -960,
    -960, 11131, 11131, 11131, 11131, 11131, 11131, 11131, 11131, 11131,
   11131,  -960,  -960, 12592, 13972,  9595, 12684,  6076,   567,    79,
      79,  7556, 11387,  7556,   197,  -960,   616,   723,  -960,  -960,
    1110,   769,    69,   111,   112,   369,   717, 11387,   107,  -960,
     657,  1113,  -960,  -960,  -960,  -960,    88,   298,   355,   448,
     493,   602,   658,   661,   707,  -960,  -960,  -960,   731,  -960,
    -960,  -960, 13972,  -960,  -960, 11259, 11259,  -960,  -960,   538,
    -960,  -960,  -960,   290, 11131, 11131,  9851,  -960,  -960, 12776,
    9595, 12868, 11131, 11131, 10107,  -960,    -8,   662,  -960,  -960,
      -8,  -960,   684,   708,  -960,    66,  -960,  -960,  -960,  -960,
    -960,  8917,  -960, 11131,   729,   745, 12776, 12868, 11131,  1076,
     -18,    -8,  -960,  -960,  8324,   721,    -8,  -960,  -960, 10235,
    -960,  -960, 10363,  -960,  -960,  -960,   446,  1122,  -960,  -960,
     755, 12011, 12960,  9595, 13052,  -960,  -960,  -960,  -960,  -960,
    -960,  -960,  -960,  -960,  -960,  -960,  -960,   753,    44,   786,
      62, 11131,  -960,  -960,  -960,  -960,  -960,  -960,  -960,  -960,
    -960,   766,  -960,  -960,  -960,   905,  -960,   905, 11131,  -960,
     771,   781,  -960,  -960,    -8, 12011,   783,  -960,  -960,  -960,
     873,   805,  2131,  -960,  -960,  -960,  1130,   644,   570,  2576,
    2576,  2576,  2576,  3028,  2399,  2576,  2576,  4411,  4411,   911,
     911,  5447,  1322,  1322,  1408,   283,   283,   570,   570,   570,
    1576,  1576,  5476,  4433,  5774,  4582,  -960,   495,  -960,  -960,
    -960,  -960,  -960,  -960,    -8,   516,   551,  -960,  5178,   905,
     930,  -960,  6229,   928,  6688,   905,    51,   905,   920,   933,
     120, 13144,  9595, 13236,  -960,   567,  -960,  1122,  -960,  -960,
    -960, 13328,  9595, 13420,  6076, 11387,  -960,  -960,  -960,  -960,
    3031,  -960,  4105,  -960,  -960,  -960,  8917, 11131,  -960, 11131,
     418,  -960,   478,  3504,  4731,    -8,   674,   716,  -960,  -960,
    -960,  -960,  9851, 10107,  -960,  -960, 11387,  4262,  -960,  -960,
     495,   495,  -960,  -960,    -7,  -960,  -960,   695, 12011,   755,
     482,   773,    -8,   525,   622,  -960,  -960,   899,  -960,   511,
    -960,   801,  -960,  -960,   587,   808,  -960,   570,  -960,   343,
     820,  -960,  -960,   343,  -960,   822, 10619,  -960,   755, 12011,
    9723, 11259, 11131, 13512,  9595, 13604,   849, 11259, 11259,  -960,
     626,   823,   607, 11259, 11259,  -960,  -960,   626,  -960,  -960,
    -960, 10747,   146,  -960,   922,  -960,   973,  -960,  -960,  -960,
    -960,  -960,  -960,   933,   905,  -960, 10875,   905,    41,    76,
      -8,   145,   171,  7556,   197, 11387,  6076,  1007,   773,  -960,
      -8,   905,    66,   848,  9064,    55,   149,  -960,  -960,  -960,
    -960, 11131, 11131,   724, 11131, 11131,   854,    66,  -960,  -960,
     633,  1287,  -960,  -960,  -960,  -960,  -960,  -960,  -960,  -960,
    -960,  -960,  -960,  -960,  -960,  1194,  7403,  -960,  -960,  -960,
    6076, 11131,   859,   755,  -960,  4262,  5625,  5923,    -8,   726,
     764, 11131,  -960,  -960,  -960,  -960,  -960,  -960, 11259,  -960,
    -960,  -960,  -960,  -960,   771,  -960,   906,  -960,  -960,  -960,
    7556,  -960,  -960,  -960,  -960,  7556, 11387,   905,  -960,  -960,
     905,  -960, 12334,   905,  -960, 11131,  -960,   287,  -960,   176,
     905,  6076,   197,   905,  -960,  -960,  -960,  1418,  6076,  1418,
    -960,  -960,  -960, 11131, 10107,  -960,  -960,  -960,  -960,  -960,
    -960,  -960,  -960,  1305,  -960,  -960,  -960,  -960,  -960,  1418,
    -960,  -960,  -960,  -960,  -960,   885,  -960,   872, 11131,  -960,
     876,   966,   879,  -960,   883,   969,   886,   983,  -960,  -960,
    -960,    -8,   904,   914,   895, 12135,  -960,   896,   879,  -960,
     907,   910,  -960,   912,  -960,  -960,   916,   767,  4262,  -960,
    -960, 12259,    79,  -960,  -960,  6816,  -960,    79,  -960,  -960,
    -960,  -960, 12543,  2964,  2964,   393, 12543,  3950,   383,    36,
    -960,  -960,  1000,    79,   945,   189,  -960,   926,  -960,  -960,
    -960,   923,  -960,  -960,  -960,   915,  -960,  -960,    32,  -960,
    -960,  -960,  -960,  -960,  -960,  -960,  -960,  -960,  1018,  -960,
    -960, 11003,  6382,  -960,   905,  -960,  -960,    -8,   905,   197,
     848,  1305,   660,  -960,   927,    -8,  -960,    -8,   103, 11131,
    1418,  -960,  -960,   713,  -960,  -960,  -960,   253,  -960,  1418,
    -960,  -960,  1694,  -960,  -960,  -960,   946,  -960, 12135,  1418,
    -960,  1022,  1222,   713,  -960,  -960,  1418,  -960,  1694,  -960,
    1132,  1035,  -960,  1237,   177,   257,   293,  6076,  1073,  6229,
     990,  -960,  -960,  -960,  -960,    23,   237,    -8, 12422,  -960,
     948,  -960,  -960,  -960,   951,   960, 12543,  -960,   659,  -960,
    -960, 11387, 11387,  1057, 12455, 12543,   510,  2964,  2964,   393,
    3219,  3219,  -960, 11131,  -960,   612,  -960,  -960,  -960,  -960,
    6076,    -8,  1112,   978,  1320,  -960,   977,  6076,  7403,  -960,
    -960,  -960,  -960,   986,  1001,  -960,   879,  -960,  1002,  -960,
    1003,  -960,  1002,  7684,  -960,  1222,  -960,  1005,  1012,  -960,
   13696,  -960,   879,  1023,  -960,  1024,  1023,  -960,   711,  -960,
    -960,   394, 13788,  9595, 13880,   930,  -960,   922,  -960,  -960,
      97,   990,  1032,  -960,   415,  -960, 12543,  1044,  -960,  -960,
   12543,  6535,  -960,  -960,  -960,  -960,  -960,  -960,  -960,  -960,
    -960,  -960,    -8,    -8,  -960,    -8,    -8,  -960,  -960,  -960,
    -960,  -960,   905,  -960,  1045,  1112,   784,  -960,  -960,   905,
     992,  1418,  -960,  1694,  -960,  -960,  1694,  -960,  1694,  -960,
    -960,  1061,  1418,  -960,  1694,  -960,  1047,  1056,  -960,  1694,
    -960,  1694,  -960,  -960,  1132,  -960,    60,   251,    -8,   406,
     419,  -960,  -960,  1065, 12543,  -960,  1044, 12543,   990,   127,
    -960,  -960,  -960,  -960,  -960,  1112,  1045,  1112,  1069,  -960,
    -960,  1002,  1074,  1002,  1002,  -960,  1023,  1077,  1023,  1023,
    -960,   420, 12543,  1044,  -960,  -960,  -960,  -960,  1045,  1112,
    -960,  1694,  -960,  -960,  -960,  -960,  1694,  -960,  -960,  -960,
    1044,  1045,  1002,  1023,  -960,  -960
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       2,     0,     0,     1,     0,   356,   357,   358,     0,   349,
     350,   351,   354,   352,   353,   355,   343,   344,   345,   346,
     367,   272,   272,   619,   618,   620,   621,   726,     0,   726,
       0,     0,   623,   622,   624,   708,   710,   614,   613,   709,
     616,   617,   608,   609,   610,   611,   558,   629,   630,     0,
       0,     0,     0,     0,     0,   299,   737,   737,    90,   319,
     578,   578,   580,   582,     0,     0,     0,     0,     0,     0,
       0,     3,   724,     6,     9,    35,    39,    51,    62,   272,
      61,     0,    78,     0,    82,    92,     0,    56,   230,   245,
       0,   297,     0,     0,    58,    58,   724,     0,     0,     0,
       0,   308,    63,   317,   286,   287,   557,   559,   288,   289,
     290,   292,   291,   293,   556,   598,   599,   555,   606,   625,
     626,   294,     0,   295,    66,     5,     8,   171,   182,   172,
     195,   168,   188,   178,   177,   198,   199,   193,   176,   175,
     170,   196,   200,   201,   180,   169,   183,   187,   189,   181,
     174,   190,   197,   192,   191,   184,   194,   179,   167,   186,
     185,   166,   173,   164,   165,   161,   162,   163,   121,   123,
     122,   156,   157,   152,   134,   135,   136,   143,   140,   142,
     137,   138,   158,   159,   144,   145,   149,   153,   139,   141,
     131,   132,   133,   146,   147,   148,   150,   151,   154,   155,
     160,   126,   128,    28,   124,   125,   127,     0,     0,     0,
       0,     0,     0,     0,   578,     0,   267,     0,   252,   277,
      76,   271,   737,     0,   625,   626,     0,   295,   737,   702,
      77,   726,    74,     0,   737,   444,    73,   726,   727,     0,
       0,    23,   242,     0,    10,     0,   343,   344,   311,   445,
       0,   224,     0,   308,   225,   215,   216,   305,     0,    21,
       0,     0,   724,    17,    20,   726,    80,    16,   301,   726,
       0,   730,   730,   253,     0,     0,   730,   700,   726,     0,
       0,     0,    88,   348,     0,    98,    99,   106,   424,   603,
     602,   604,   601,     0,   600,     0,     0,     0,   565,   574,
     570,   576,   607,    55,   236,   237,   733,   734,     4,   735,
     725,     0,     0,     0,     0,     0,     0,     0,   359,   449,
     438,    67,   452,   316,   360,   452,   434,     0,    94,     0,
      86,    83,     0,     0,     0,     0,     0,     0,   248,   249,
       0,     0,     0,     0,   213,   214,     0,     0,     0,   246,
     247,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   720,   721,     0,     0,   737,     0,     0,    57,     0,
       0,     0,     0,     0,   724,   327,   725,     0,   378,   377,
       0,     0,   625,   626,   295,   116,   117,     0,     0,   119,
     633,     0,   625,   626,   295,   335,   191,   184,   194,   179,
     161,   162,   163,   121,   122,   698,   337,   697,     0,    75,
     723,   722,     0,   318,   560,     0,     0,   129,   705,   305,
     278,   707,   274,     0,     0,     0,     0,   268,   276,     0,
     737,     0,     0,     0,     0,   269,   726,     0,   310,   273,
     726,   263,   737,   737,   262,   726,   315,    54,    25,    27,
      26,     0,   312,     0,     0,     0,     0,     0,     0,    19,
       0,   726,   303,    15,   725,    79,   726,   300,   306,   732,
     731,   254,   732,   256,   307,   701,     0,   105,   607,    96,
      91,     0,     0,   737,     0,   320,   425,   584,   605,   587,
     585,   579,   561,   562,   581,   563,   583,     0,     0,     0,
       0,     0,   736,     7,    29,    30,    31,    32,    33,    52,
      53,     0,   450,   449,    68,     0,   453,     0,     0,    36,
     282,     0,    38,   281,   726,     0,    84,    95,    50,    40,
      48,     0,   257,   277,   202,    37,     0,   295,   222,   229,
     231,   232,   233,   240,   241,   234,   235,   211,   212,   238,
     239,   726,   226,   227,   228,   217,   218,   219,   220,   221,
     250,   251,   711,   713,   712,   714,   443,   272,   441,   711,
     713,   712,   714,   347,   726,   711,   712,   442,   272,     0,
     737,   369,     0,   368,     0,     0,     0,     0,   325,     0,
     305,     0,   737,     0,    58,   333,   116,   117,   118,   631,
     331,     0,   737,     0,     0,     0,   338,   718,   719,   340,
     272,    41,   257,   203,    47,   210,     0,     0,   704,     0,
     279,   275,   737,   711,   712,   726,   711,   712,   703,   309,
     728,   259,   264,   266,   314,    24,     0,   243,    11,    34,
       0,   737,   209,    22,    81,    18,   302,   730,     0,    89,
     715,   104,   726,   711,   712,   426,   588,     0,   564,     0,
     567,     0,   572,   569,     0,     0,   573,   223,   447,   737,
       0,   366,   448,   737,   433,   285,     0,    93,    87,     0,
       0,     0,     0,     0,   737,     0,     0,     0,     0,   440,
      71,     0,   446,     0,     0,   261,   439,    69,   260,   298,
     361,   737,   737,   547,   737,   370,   737,   323,   372,    59,
     371,   324,   462,     0,     0,   363,     0,     0,   715,   304,
     726,   711,   712,     0,     0,     0,     0,   116,   117,   120,
     726,     0,   726,   635,     0,   435,    64,   130,   706,   280,
     270,     0,     0,   446,     0,     0,   737,   726,   255,    97,
     446,   657,   589,   593,   594,   595,   596,   586,   597,   566,
     568,   575,   571,   577,   416,   726,     0,   414,   413,    65,
       0,     0,   283,    85,    49,   258,   711,   712,   726,   711,
     712,     0,    46,   207,    45,   208,    72,   729,     0,    43,
     205,    44,   206,    70,   548,   549,   737,   550,   362,   364,
       0,    12,    14,   554,   365,     0,     0,     0,   373,   375,
       0,    60,     0,     0,   329,     0,   455,     0,   328,   446,
       0,     0,     0,     0,   446,   336,   699,   657,     0,   657,
     341,   436,   437,     0,   265,   313,   663,   660,   659,   658,
     661,   669,   678,     0,   689,   679,   693,   692,   688,   657,
     427,   656,   430,   662,   664,   665,   667,   642,   671,   676,
     737,   681,   737,   686,   642,   691,   642,     0,   640,   590,
     412,   726,     0,   665,   397,   673,   674,   737,   737,   684,
     397,   397,   395,   418,   451,   454,   284,   446,   244,    42,
     204,     0,     0,   552,   553,     0,   376,     0,   321,   322,
     541,   545,   515,     0,     0,     0,     0,   726,     0,   504,
     539,   578,     0,     0,   467,   470,   475,   477,   479,   473,
     474,   510,   512,   511,   480,   520,   525,   526,   527,   530,
     531,   532,   533,   534,   536,   535,   537,   538,   519,   326,
     456,     0,     0,   330,     0,   632,   332,   726,     0,     0,
     635,     0,   390,   381,   383,   726,   379,   726,     0,     0,
       0,   649,   670,     0,   638,   696,   680,     0,   639,     0,
     652,   690,     0,   654,   694,   591,     0,   415,     0,   403,
     405,     0,   672,     0,   393,   394,     0,   408,     0,   410,
       0,     0,   551,     0,   625,   626,   295,     0,   737,     0,
     514,   528,   529,   117,   543,   726,   492,   726,   493,   499,
       0,   488,   578,   490,     0,   502,     0,   424,     0,   542,
     463,     0,     0,     0,   471,     0,     0,   523,   524,     0,
     726,   726,   518,     0,   457,   737,   334,   634,   339,   636,
       0,   726,     0,   388,     0,   666,     0,     0,     0,   428,
     682,   641,   668,   642,   642,   677,   737,   695,   642,   687,
     642,   665,   642,     0,   417,   683,   396,   397,   397,   305,
       0,   675,   737,   397,   685,   397,   397,   422,   726,   420,
     423,   305,     0,   737,     0,   737,    13,   737,   491,   487,
     497,   508,   494,   500,     0,   489,     0,   505,   506,   540,
     517,     0,   468,   469,   476,   472,   478,   513,   509,   521,
     522,   544,   726,   726,   486,   726,   726,   483,   458,   460,
     461,   459,     0,   380,   391,     0,   386,   382,   429,     0,
       0,     0,   645,     0,   647,   637,     0,   653,     0,   650,
     655,     0,     0,   400,     0,   402,   715,   304,   392,     0,
     409,     0,   406,   411,     0,   419,   715,   304,   726,   711,
     712,   546,   374,   495,     0,   501,   503,     0,   516,   737,
     484,   485,   481,   482,   342,     0,   389,     0,   384,   432,
     431,   642,   642,   642,   642,   592,   397,   397,   397,   397,
     421,   446,     0,   498,   507,   465,   466,   464,   387,     0,
     646,     0,   643,   648,   651,   401,     0,   398,   404,   407,
     496,   385,   642,   397,   644,   399
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -960,  -960,  -960,   952,  -960,    45,   770,  -534,  -960,   -48,
    -960,   760,  -960,    58,  -960,  -301,  -310,   -28,   -74,   -83,
    -960,  -960,     2,  -960,   221,  1254,    17,  1134,   -87,    -3,
     -61,  -960,  -397,     3,  2175,  -379,  1135,   -46,   -15,  -960,
    -960,    -1,  -960,  3345,  -960,  1147,  -960,   340,  -960,   545,
      28,   627,  -330,   118,   -12,  -960,  -258,  -213,    21,  -960,
    -296,   -54,  -960,  -960,  -960,  -960,  -960,  -960,  -960,  -960,
    -960,  -960,  -960,  -960,  -960,  -960,   829,  -960,  -960,  -960,
    -960,  -960,  -960,  -960,  -960,  -960,  -960,  -960,  -960,  -960,
    -960,  -960,   553,  -960,   270,  1723,  -328,  -960,   160,  -677,
    -960,  -959,  -878,   303,   500,    54,  -960,   593,  -960,  -769,
    -960,   114,   256,  -960,  -960,  -960,  -960,  -960,  -960,   539,
    -960,  -960,   -92,   761,  -960,  -960,   953,  -960,  -960,  -960,
    -689,  -960,   110,  -960,  -960,  -960,  -960,  -960,  -375,  -960,
    -960,   255,  -196,  -960,   464,  -946,  -694,  -828,  -960,   258,
     262,  -960,  -723,  -960,   260,  -960,  -960,   200,  -960,  -960,
     284,   411,   424,  -960,  1186,   577,   635,   670,  -960,   795,
     855,  -960,  1090,  1177,  -960,  -960,   -60,  -960,  -960,  -207,
    -960,  -960,  -960,  -960,  -960,  -960,  -960,    12,  -960,  -960,
    -960,  -960,   -27,  2370,    -2,  1195,  2621,  1918,  -960,  -960,
     346,  -960,  -642,  -172,  -369,  -921,  -145,  -168,  -840,  -402,
    -648,   335,   317,  -960,  -960,  -757,  -458,  -906,  -911,   322,
     342,  -960,  -571,  -960,  -607,  -754,  -960,  -960,  -960,    30,
    -362,  -960,  -338,  -960,  -960,   -85,  -960,   -57,   -11,  -125,
    -604,  -193,   -64,     4,     1
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,    71,    72,    73,   244,   579,   895,   580,
     262,   263,   460,   264,   451,    75,   529,    76,   369,   371,
     372,   811,    77,    78,   514,   250,    80,    81,   265,    82,
      83,    84,   480,    85,   217,   389,   390,   201,   202,   203,
     616,   572,   205,    87,   453,   359,    88,   219,   270,   534,
     566,   696,   440,   441,   232,   233,   221,   427,   442,   522,
     523,    89,   367,   269,   466,   636,   288,   713,   589,   726,
     724,   604,   606,   733,   734,   950,   252,    91,    92,    93,
      94,    95,    96,    97,    98,    99,   100,   322,   325,   701,
     800,   716,   805,   806,   672,   253,   582,   709,   807,   808,
     381,   953,   954,   955,  1066,   980,   871,   766,   767,   872,
    1078,  1079,   485,   486,   655,   751,   958,   850,  1049,   326,
     102,   103,   323,   511,   512,   669,   515,   516,   673,   817,
     717,  1121,   714,   812,  1101,  1197,   913,   914,  1091,   916,
     917,   918,  1007,  1008,  1009,  1097,  1010,   920,   921,   922,
     923,   924,   925,  1017,   926,   927,   928,   702,   796,   892,
     802,   104,   105,   106,   107,   108,   109,   110,   497,   659,
     111,   499,   112,   113,   498,   500,   293,   296,   297,   491,
     657,   656,   752,   869,   975,  1063,   757,   114,   115,   294,
     116,   117,   118,   224,   225,   121,   226,   227,   600,   725,
     828,   829,  1051,   961,   852,   853,   854,  1061,   856,   857,
     858,   859,   876,   877,   860,   861,   862,   863,   879,   880,
     864,   865,   866,   867,   868,   964,   408,   605,   275,   443,
     229,   124,   640,   568,   609,   603,   412,   308,   437,   438,
     692,   471,   583,   376,   267
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     120,   295,   120,   125,   283,   366,   261,   204,   309,   598,
     236,   413,   373,   241,   292,   435,   239,   411,   242,   370,
     206,   743,   374,   331,   813,   567,   573,   204,   578,   810,
     283,   528,   309,   216,   216,   574,   535,   222,   222,   375,
     206,   302,   584,   283,   283,   283,   228,   228,   750,   126,
     235,   266,   120,   120,   406,   938,   286,   273,   277,   204,
      74,   282,    74,  1059,   368,   368,  -111,   321,   368,  1080,
     731,   303,   628,  -111,   610,  1074,   310,   271,   708,   473,
    1014,   318,   286,   475,   649,  1127,   272,   276,  1015,   494,
     496,   567,   581,   578,  -107,   383,   393,   393,   393,   204,
     625,  -113,   976,   316,   317,   611,   614,   320,   968,   851,
     628,   661,   446,   268,   528,   528,   819,   875,   216,   641,
    1052,   -79,   222,   984,   985,  1023,   824,   243,   678,   665,
    1029,   228,  1047,     3,   220,   230,  -108,  -115,   431,  1052,
     465,   -93,   799,   361,   467,  -114,   641,   238,  -627,  1163,
    1166,   938,   712,   652,   423,   798,   318,  -619,   882,   596,
     245,   799,   261,   597,  1124,   662,  -627,   457,   621,   257,
    -110,   362,   410,   461,   887,  -107,   621,   324,   238,  1030,
    1001,  1002,  1031,   666,  1016,   851,  -619,   851,  1046,  -102,
    -107,  -711,   823,  -107,   881,   -98,  -112,   455,   309,   484,
     319,  -109,  1113,  1116,   306,   463,   307,   851,   120,   445,
    -711,   447,   261,  -111,   327,  -111,  1127,   -98,  1193,   409,
     479,   238,   729,   428,  -104,  1059,  -712,   283,  1048,   428,
     435,  1074,   306,  1080,   307,   444,   885,   504,   505,   506,
     507,   120,   524,   120,   125,  1164,  1210,  1176,   216,   298,
     216,   216,   222,   478,   222,   641,   120,   266,   120,   -99,
    -106,   228,   720,   228,  -113,   641,   464,   476,  -105,   938,
    -108,   628,   730,   938,   938,   319,   283,   235,   320,   286,
     234,   261,   773,   368,   368,   368,   368,   944,   509,   510,
     660,  1023,   660,  -101,   948,   593,  1052,   299,   586,  1198,
     581,    74,  1135,  1011,  1109,  1110,  -115,   878,   120,  1052,
     309,   629,   300,   120,  1092,   631,   459,   588,  1148,  -103,
     634,  1211,  1098,   585,  -100,   587,   266,   120,   286,   519,
    -107,   875,  -107,   526,   530,   875,   644,  1024,   875,  1023,
     875,   646,   338,   339,   368,   567,  1120,   578,   521,   436,
     306,   439,   307,   521,   778,   503,  1057,   487,  1119,   595,
    1057,   333,   565,   874,   846,   120,   444,  -618,    74,   120,
     301,   120,   882,   508,   621,   621,  1057,   782,   784,   882,
     528,   882,   216,   789,   791,  -508,   528,   528,   847,  1054,
     349,   350,   528,   528,   577,  -708,  -618,   274,  1060,   677,
     328,  -712,  1098,  1089,  -113,   795,  -113,  -114,  1068,   740,
    -108,   492,  -108,   489,   490,  1075,   617,   530,   530,  -110,
     479,   356,   357,   358,  -620,   237,  1114,   283,   565,   764,
     238,   444,  -112,  -109,   487,   941,   635,   915,  -612,   332,
     306,   902,   307,   428,   428,   596,  -115,   216,  -115,  1003,
     204,   685,   760,  -620,   748,   565,   622,   760,   947,   577,
     949,   842,   120,   206,   479,    61,  -612,  -612,  -612,   415,
    1098,   283,   628,  1194,   417,  -628,   765,  -628,   889,   286,
     957,   565,   487,   433,   444,   577,   237,   528,   488,   942,
     489,   490,  1195,   845,   875,   419,   875,  1012,  1098,   647,
     216,   875,  -108,   875,  -108,  1056,  1122,  -102,  1170,  1171,
    -115,   723,   577,  1129,  -708,   424,  -612,  -621,   416,  -708,
     425,   878,   710,   286,   -99,  1072,  1004,  1000,   878,  1013,
     878,  1005,  1006,   740,   704,   882,   706,   882,   489,   490,
     686,  1100,   882,   426,   882,  -115,  -621,  -114,   418,  -114,
    -101,   420,   421,   422,  1191,   690,   487,   495,   875,  -110,
    1182,  -110,  -623,   691,   997,  -106,   697,  1058,   902,   999,
    1062,  1187,  -112,  -109,  -112,  -109,   621,   732,   487,   698,
     120,   703,   120,   855,  1073,  1020,  1076,   749,   842,   458,
     565,  -623,   432,   444,   283,   689,   452,   873,   736,   882,
     565,   737,   120,   444,   316,   317,   695,   826,   747,   216,
    -102,   698,   489,   490,   691,   204,  -114,   693,   479,   216,
     845,   577,   835,   428,  1012,   283,   434,   799,   206,   715,
    -102,   577,  -711,  -102,   489,   490,  -105,  -102,   695,   458,
     231,   691,   698,   831,  -110,   234,   286,  -103,   333,  1006,
    1111,   822,   694,  -101,   487,  1006,  1006,   462,  -100,   855,
     821,   855,   746,   -78,  -101,   759,  -114,   520,   689,   695,
     768,  -622,   533,  -101,   768,   820,  -101,   286,   120,  -112,
    -101,   855,   565,   530,   878,   444,   878,   474,   830,   530,
     530,   878,   970,   878,   973,   530,   530,   368,   956,  -103,
    -622,   216,   797,   803,   468,   809,   487,   809,   788,   691,
     489,   490,   836,   577,   837,   838,   839,   840,   884,   691,
     477,   120,   521,   481,   120,  1168,   487,  -624,   501,  1181,
    -612,  1183,   897,   204,   513,  -109,  1184,   469,   774,   487,
    1186,   762,  1188,  -709,   470,   416,   641,   428,   878,  1189,
    -103,   527,   894,  1158,   883,  -100,  -624,   896,   945,  -612,
     492,  -100,   489,   490,   120,   618,   620,   691,   120,   502,
    -103,   841,  -115,  -103,   274,   744,  -615,  -103,   368,   590,
     492,  -100,   489,   490,  -100,  1100,  -615,   674,  -100,   599,
     530,   842,   855,   493,   594,   489,   490,   893,   120,  1212,
     607,   855,  -110,   120,  1213,  -615,   956,  1043,  1042,   620,
     937,   873,   274,   630,  -615,  -615,  -615,   745,   873,   120,
     487,   283,  1037,   845,   846,   833,   120,   693,  1105,   608,
    1045,    90,   632,    90,  1112,  1115,   836,   283,   837,   838,
     839,   840,  -304,   472,  -112,   223,   223,   998,   847,   699,
     470,  1018,  -109,   487,  -110,   707,   633,   711,   675,  1154,
     883,   965,  -709,   965,  -615,   694,   238,  -709,   788,   -93,
    -304,  -304,  -304,   393,   658,   638,   489,   490,   965,   965,
    1088,  1132,  1134,    90,    90,  1039,  1137,   284,  1139,   995,
    1140,   639,  -112,   120,  1035,  -109,   691,   956,   223,   956,
     937,   937,   937,   648,   937,   937,  1117,   663,  1084,   489,
     490,   786,   668,   284,  -348,   671,  1123,   680,   793,  -277,
    -304,   223,   223,  -712,   283,   223,   380,   391,   391,   676,
     223,   679,  1177,   681,   987,   989,   804,   799,   700,   361,
     120,   705,  -348,  -348,  -348,   712,   883,  1102,  1103,  1085,
     715,  1087,  1094,   753,   754,   761,   755,   738,   756,   739,
     613,   615,   763,   855,    47,    48,   769,   362,   363,   364,
    -278,   448,   620,   274,   873,   787,   393,   613,   615,   781,
     956,  1178,   449,   450,   814,  1021,  1022,   818,   799,   333,
    1172,  1173,  -348,   368,   368,   120,   691,   120,   827,   803,
    1130,   825,   834,   642,  1027,  1028,   937,  -279,   891,  1200,
    1202,  1203,  1204,   959,   937,  1141,   772,   365,   966,   691,
     960,   971,   937,   937,   963,   937,   937,   967,   937,   937,
     956,   969,   956,  -715,   972,   974,   809,   977,   120,    90,
    1214,   794,   978,   979,   983,   120,   120,   354,   355,   356,
     357,   358,  1019,  1169,   956,   986,   816,   965,   988,  1025,
     223,   120,   223,   223,  -280,   990,   223,  1155,   223,   565,
    1032,  1026,    90,   965,    90,  1044,  -715,   898,  1069,  1064,
     899,   565,   798,   939,   444,   361,   703,    90,   809,    90,
     943,  1081,  1023,   946,   937,   361,  1093,  1095,   937,   120,
     216,   691,   691,   361,  -715,  -715,  -715,  -625,  1096,   900,
     284,   886,   577,   362,   429,   364,   311,   312,   313,   314,
     315,  1143,  1145,   362,   456,   364,  1125,  1150,  1128,  1152,
    1153,   362,   482,   364,  1131,  -625,  -625,  -625,  1180,    90,
     223,   223,   223,   223,    90,   223,   223,   691,  -626,  1133,
    1136,  1138,  -715,  1142,  -715,   940,   223,  -711,    90,   284,
    1144,   536,   937,   430,   836,   937,   837,   838,   839,   840,
     809,  1149,  1151,   430,   274,  -295,  -626,  -626,  -626,   361,
    1165,   483,   361,  1185,  1077,  -625,   837,   838,   839,   840,
     937,  -305,  1167,  1175,   223,   454,    90,  -711,   962,   361,
      90,   223,    90,  -295,  -295,  -295,  -712,   362,   591,   364,
     362,   601,   364,  1192,  1036,   951,   223,  1199,  1038,  -305,
    -305,  -305,  1201,   929,   645,  1206,  -626,   362,   683,   364,
     643,   378,   783,   785,   395,   360,   930,   735,   790,   792,
    1205,  1207,  1208,  1209,   536,   536,   836,  1162,   837,   838,
     839,   840,   841,  -295,  1041,   801,    79,   592,    79,   223,
     602,    42,    43,    44,    45,   870,   770,  1215,  1190,  -305,
      79,    79,   842,  1099,   670,   832,   919,   684,   517,  1196,
    1106,  1034,  1086,  1104,  1107,  1161,   783,   785,  1108,   790,
     792,   361,   414,    90,   664,   407,  1040,   843,  1055,  1050,
    1071,  1067,  1053,   844,   845,   846,   361,     0,    79,    79,
     284,     0,   223,   929,   929,   929,     0,   929,   929,   362,
    1070,   364,     0,    79,     0,     0,   930,   930,   930,   847,
     930,   930,   848,   890,   362,  1082,   364,     0,     0,   836,
       0,   837,   838,   839,   840,   841,    79,    79,     0,   238,
      79,     0,     0,     0,   284,    79,     0,   836,     0,   837,
     838,   839,   840,     0,     0,   842,     0,     0,     0,   602,
       0,     0,   836,  1118,   837,   838,   839,   840,   890,     0,
       0,     0,     0,     0,  1083,     0,     0,     0,     0,   931,
     843,     0,  1174,     0,     0,     0,   844,   845,   846,  1179,
     333,     0,     0,     0,     0,     0,     0,     0,   951,     0,
       0,    90,     0,    90,   952,     0,     0,   346,   347,   929,
       0,   223,   847,   951,     0,   848,     0,   929,     0,  1126,
       0,   223,   930,    90,   223,   929,   929,   849,   929,   929,
     930,   929,   929,     0,     0,     0,     0,   932,   930,   930,
       0,   930,   930,     0,   930,   930,     0,   353,   354,   355,
     356,   357,   358,     0,    79,   223,     0,     0,     0,     0,
     836,     0,   837,   838,   839,   840,   841,   284,     0,   931,
     931,   931,   933,   931,   931,    79,   333,    79,    79,     0,
       0,    79,     0,    79,     0,     0,   842,    79,     0,    79,
       0,     0,     0,   346,   347,     0,     0,   929,   284,    90,
     536,   929,    79,   223,    79,     0,   536,   536,     0,     0,
     930,   843,   536,   536,   930,     0,     0,   844,   845,   846,
       0,     0,     0,     0,     0,     0,     0,   932,   932,   932,
       0,   932,   932,     0,   354,   355,   356,   357,   358,     0,
       0,     0,    90,   847,   223,    90,   848,     0,     0,     0,
       0,     0,     0,     0,    79,    79,    79,    79,    79,    79,
      79,    79,   933,   933,   933,   929,   933,   933,   929,     0,
       0,    79,     0,    79,     0,   931,    79,     0,   930,     0,
       0,   930,     0,   931,     0,    90,     0,     0,     0,    90,
       0,   931,   931,   929,   931,   931,     0,   931,   931,     0,
       0,     0,     0,     0,     0,     0,   930,   536,     0,    79,
       0,    79,     0,     0,     0,    79,    79,    79,     0,    90,
       0,     0,     0,     0,    90,   223,     0,     0,     0,     0,
       0,    79,     0,   932,     0,     0,     0,     0,     0,     0,
      90,   932,     0,     0,   333,     0,     0,    90,     0,   932,
     932,     0,   932,   932,     0,   932,   932,   934,     0,    79,
      79,   346,   347,   931,     0,     0,     0,   931,   933,     0,
       0,     0,     0,     0,    79,     0,   933,     0,     0,     0,
       0,     0,     0,     0,   933,   933,     0,   933,   933,     0,
     933,   933,     0,     0,   982,     0,     0,     0,     0,   351,
     352,   353,   354,   355,   356,   357,   358,     0,    79,     0,
     993,     0,     0,     0,    90,   101,     0,   101,     0,     0,
       0,   932,     0,     0,     0,   932,     0,    79,     0,   101,
     101,   931,     0,     0,   931,     0,   836,     0,   837,   838,
     839,   840,   841,     0,     0,     0,     0,   934,   934,   934,
       0,   934,   934,     0,     0,     0,   933,     0,     0,   931,
     933,    90,   842,     0,     0,     0,     0,   101,   101,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   101,     0,     0,     0,     0,   843,     0,   932,
       0,     0,   932,     0,   845,   846,     0,  1065,     0,     0,
       0,     0,     0,     0,     0,   101,   101,     0,     0,   101,
       0,     0,     0,     0,   101,     0,    90,   932,    90,   847,
       0,     0,     0,     0,   933,     0,    79,   933,    79,     0,
       0,     0,     0,     0,     0,     0,    79,     0,     0,     0,
     223,   223,     0,     0,     0,     0,    79,     0,    79,    79,
       0,     0,   933,   934,     0,     0,     0,     0,     0,    90,
       0,   934,     0,     0,     0,     0,    90,    90,     0,   934,
     934,     0,   934,   934,     0,   934,   934,     0,     0,     0,
      79,     0,    90,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   935,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   223,     0,     0,     0,     0,     0,     0,     0,
     123,     0,   123,     0,     0,     0,     0,     0,     0,     0,
      90,     0,     0,   101,    79,    79,     0,     0,    79,     0,
       0,    79,    79,     0,     0,     0,     0,    79,    79,     0,
       0,   934,     0,     0,   101,   934,   101,   101,     0,     0,
     101,     0,   101,     0,     0,     0,   101,     0,   101,     0,
       0,     0,   123,   123,     0,     0,   287,    79,     0,    79,
      79,   101,     0,   101,     0,     0,     0,     0,     0,   936,
       0,     0,   935,   935,   935,     0,   935,   935,     0,     0,
       0,     0,   287,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   384,   394,   394,     0,   934,
      79,     0,   934,     0,    79,     0,     0,     0,     0,     0,
       0,     0,     0,   101,   101,   101,   101,   101,   101,   101,
     101,     0,    79,     0,     0,     0,     0,   934,     0,     0,
     101,     0,   101,     0,    79,   101,     0,     0,     0,    79,
      79,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    79,     0,     0,     0,   936,
     936,   936,    79,   936,   936,     0,     0,     0,   101,     0,
     101,     0,     0,     0,   101,   101,   101,     0,   935,     0,
       0,     0,     0,     0,     0,     0,   935,     0,     0,     0,
     101,     0,     0,     0,   935,   935,     0,   935,   935,     0,
     935,   935,     0,     0,     0,     0,     0,     0,   123,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   101,   101,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    79,
       0,     0,     0,   101,     0,     0,     0,     0,     0,     0,
       0,   123,     0,   123,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   682,   123,    86,   123,    86,
       0,     0,     0,     0,     0,   936,   935,   101,     0,     0,
     935,     0,     0,   936,     0,     0,    79,     0,     0,   287,
       0,   936,   936,     0,   936,   936,   101,   936,   936,   333,
     334,   335,   336,   337,   338,   339,   340,   341,   342,   343,
     344,   345,     0,     0,     0,     0,   346,   347,   123,    86,
      86,     0,     0,   123,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   123,   287,     0,
     537,    79,     0,    79,   935,     0,     0,   935,     0,     0,
     348,     0,   349,   350,   351,   352,   353,   354,   355,   356,
     357,   358,   379,   936,     0,    79,    79,   936,     0,  -252,
       0,     0,   935,     0,     0,   123,     0,     0,     0,   123,
       0,   123,     0,     0,    79,     0,     0,     0,     0,     0,
       0,    79,    79,     0,     0,   101,     0,   101,     0,     0,
       0,     0,     0,     0,     0,   101,     0,    79,     0,     0,
       0,     0,     0,     0,     0,   101,     0,   101,   101,     0,
       0,     0,     0,   537,   537,     0,     0,    79,     0,     0,
       0,   936,     0,     0,   936,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    79,     0,     0,     0,   101,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   936,
       0,     0,   119,     0,   119,     0,     0,     0,     0,     0,
       0,     0,   123,     0,     0,    86,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   287,
       0,     0,     0,   101,   101,     0,     0,   101,     0,     0,
     101,   101,     0,     0,     0,     0,   101,   101,    86,     0,
      86,     0,     0,     0,   119,   119,     0,     0,   285,     0,
       0,     0,     0,    86,     0,    86,     0,     0,     0,     0,
       0,     0,     0,   287,     0,     0,   101,     0,   101,   101,
       0,     0,     0,     0,   285,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   382,   392,   392,
     392,     0,     0,     0,     0,     0,     0,   333,   334,   335,
     336,   337,   338,   339,   340,    86,   342,   343,     0,   101,
      86,     0,     0,   101,   346,   347,     0,     0,     0,     0,
     123,     0,   123,     0,    86,     0,     0,   531,     0,     0,
       0,   101,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   123,   101,     0,     0,     0,     0,   101,   101,
     349,   350,   351,   352,   353,   354,   355,   356,   357,   358,
       0,     0,    86,     0,   101,     0,    86,     0,    86,     0,
       0,   101,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   287,     0,     0,     0,
       0,     0,     0,     0,     0,   758,     0,     0,     0,     0,
     119,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     531,   531,     0,     0,     0,     0,     0,   287,   123,   537,
       0,     0,     0,     0,     0,   537,   537,     0,     0,     0,
       0,   537,   537,   119,     0,   119,     0,     0,   101,     0,
       0,     0,     0,   122,     0,   122,     0,     0,   119,     0,
     119,     0,     0,     0,     0,     0,     0,     0,     0,    86,
       0,   123,     0,     0,   123,     0,     0,     0,     0,     0,
       0,   285,     0,     0,   333,  -738,  -738,  -738,  -738,   338,
     339,     0,     0,  -738,  -738,   101,     0,     0,     0,     0,
       0,   346,   347,     0,     0,   122,   122,     0,     0,     0,
     119,     0,     0,     0,   123,   119,     0,     0,   123,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   119,
     285,     0,     0,     0,     0,     0,   537,   349,   350,   351,
     352,   353,   354,   355,   356,   357,   358,     0,   123,     0,
     101,     0,   101,   123,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   119,     0,   123,
       0,   119,     0,   119,   101,   101,   123,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    86,     0,    86,
       0,     0,     0,   101,     0,     0,     0,     0,     0,     0,
     101,   101,     0,     0,     0,     0,     0,     0,     0,    86,
       0,     0,     0,     0,     0,     0,   101,     0,     0,     0,
       0,     0,     0,   394,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   101,     0,     0,   996,
       0,     0,     0,   123,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   101,     0,     0,     0,     0,     0,
       0,   122,     0,     0,   119,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   285,     0,     0,     0,    86,   531,     0,     0,     0,
     123,     0,   531,   531,   122,     0,   122,     0,   531,   531,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   122,
       0,   122,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   285,   394,     0,    86,     0,
       0,    86,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   123,     0,   123,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   122,     0,     0,     0,     0,   122,     0,     0,     0,
       0,    86,     0,     0,     0,    86,     0,     0,     0,     0,
     122,     0,   119,   122,   119,     0,     0,     0,   123,     0,
       0,     0,     0,   531,     0,   123,   123,     0,     0,     0,
       0,     0,     0,     0,   119,    86,     0,     0,     0,     0,
      86,   123,     0,     0,     0,     0,     0,     0,   122,     0,
       0,     0,   122,     0,   122,     0,    86,    23,    24,    25,
      26,     0,     0,    86,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    32,    33,    34,     0,     0,   285,   123,
       0,     0,     0,     0,    42,    43,    44,    45,    46,     0,
       0,  -737,     0,     0,     0,     0,   122,   122,     0,  -737,
    -737,  -737,     0,     0,  -737,  -737,  -737,     0,  -737,   285,
     119,     0,     0,     0,     0,     0,  -737,  -737,  -737,     0,
       0,     0,     0,     0,     0,     0,   992,     0,  -737,  -737,
      86,  -737,  -737,  -737,  -737,  -737,   910,    60,    61,    62,
      63,    64,    65,    66,    67,   122,     0,     0,     0,     0,
       0,     0,     0,   119,     0,     0,   119,     0,     0,     0,
    -737,     0,     0,     0,     0,   280,   333,   334,   335,   336,
     337,   338,   339,     0,     0,   342,   343,    86,     0,     0,
       0,     0,     0,   346,   347,     0,     0,     0,  -737,  -737,
       0,     0,     0,     0,     0,     0,   119,     0,     0,     0,
     119,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -737,     0,     0,     0,     0,     0,   349,
     350,   351,   352,   353,   354,   355,   356,   357,   358,     0,
     119,     0,    86,     0,    86,   119,  -737,  -737,     0,     0,
       0,   234,  -737,     0,  -737,     0,  -737,     0,     0,     0,
       0,   119,     0,     0,     0,     0,     0,     0,   119,     0,
       0,     0,     0,   122,     0,   122,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    86,     0,     0,     0,     0,
       0,     0,    86,    86,     0,   122,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    86,     0,
       0,     0,     0,     0,     0,   392,     0,     0,     0,     0,
       0,     0,    23,    24,    25,    26,     0,     0,     0,     0,
       0,   994,     0,     0,     0,   119,     0,     0,    32,    33,
      34,   900,     0,     0,     0,   901,    86,   902,     0,    42,
      43,    44,    45,    46,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   842,     0,     0,
       0,   122,   122,     0,     0,     0,     0,     0,   122,   122,
     903,   904,   119,     0,   122,   122,     0,     0,     0,   905,
       0,     0,   906,     0,     0,   907,   908,     0,   909,   845,
       0,   910,    60,   911,    62,    63,    64,    65,    66,    67,
       0,     0,     0,     0,   122,     0,     0,   122,   392,     0,
       0,     0,     0,   912,     0,     0,     0,     0,     0,     0,
     280,   218,   218,     0,     0,     0,     0,   119,     0,   119,
       0,     0,     0,     0,   238,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   122,     0,     0,
       0,   122,     0,     0,   251,   254,   255,   256,     0,     0,
       0,   218,   218,     0,     0,     0,     0,     0,     0,   122,
     119,     0,     0,     0,   304,   305,     0,   119,   119,     0,
       0,   122,     0,     0,     0,     0,   122,     0,     0,     0,
       0,     0,     0,   119,     0,     0,     0,     0,     0,     0,
       0,     0,   122,     0,     0,     0,   218,     0,     0,   122,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   119,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -715,     0,     0,     0,     0,     0,
       0,     0,  -715,  -715,  -715,     0,   122,  -715,  -715,  -715,
       0,  -715,     0,     0,     0,     0,     0,     0,     0,  -715,
    -715,  -715,  -715,  -715,     0,     0,     0,     0,     0,     0,
       0,  -715,  -715,     0,  -715,  -715,  -715,  -715,  -715,     0,
       0,     0,     0,   218,     0,     0,   218,   218,   218,     0,
     304,     0,     0,   122,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -715,     0,     0,   218,     0,   218,   218,
       0,     0,  -715,  -715,  -715,  -715,  -715,  -715,  -715,  -715,
    -715,  -715,  -715,  -715,  -715,     0,     0,     0,     0,  -715,
    -715,  -715,  -715,  -715,     0,   741,  -715,     0,     0,     0,
       0,     0,  -715,     0,     0,     0,     0,     0,   122,     0,
     122,     0,     0,     0,     0,     0,  -715,     0,     0,  -715,
       0,     0,  -111,  -715,  -715,  -715,  -715,  -715,  -715,  -715,
    -715,  -715,  -715,  -715,  -715,     0,     0,     0,     0,  -715,
    -715,  -715,  -715,     0,     0,  -715,  -715,  -715,     0,  -715,
       0,   122,     0,     0,     0,     0,     0,     0,   122,   122,
       0,     0,   218,     0,     0,     0,     0,   532,   538,   539,
     540,   541,   542,     0,   122,   543,   544,   545,   546,   547,
     548,   549,   550,   551,     0,     0,   552,   553,   554,   555,
     556,   557,   558,   559,   560,   561,     0,     0,     0,     0,
     218,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   122,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     612,   612,     0,     0,     0,     0,     0,     0,     0,   612,
     218,   218,     0,     0,     0,   218,     0,   612,   612,   218,
       0,     0,     0,     0,     0,     0,     0,     0,  -737,     4,
       0,     5,     6,     7,     8,     9,     0,     0,   637,    10,
      11,     0,     0,   612,    12,     0,    13,    14,    15,    16,
      17,    18,    19,     0,   218,     0,     0,   218,    20,    21,
      22,    23,    24,    25,    26,     0,     0,    27,   218,     0,
       0,     0,     0,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,   667,    41,    42,    43,
      44,    45,    46,    47,    48,     0,     0,     0,     0,     0,
       0,     0,     0,   218,    49,    50,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    51,
      52,     0,     0,     0,     0,     0,     0,     0,    53,     0,
       0,    54,    55,     0,    56,    57,     0,    58,     0,     0,
      59,    60,    61,    62,    63,    64,    65,    66,    67,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    68,
      69,    70,     0,     0,     0,     0,     0,   218,     0,     0,
       0,  -737,     0,  -737,     0,     0,     0,   218,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   218,     0,   218,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   218,   218,     0,
       0,     0,     0,    23,    24,    25,    26,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    32,
      33,    34,   900,     0,     0,     0,   901,     0,     0,     0,
      42,    43,    44,    45,    46,     0,     0,     0,     0,     0,
       0,   218,     0,     0,     0,     0,   612,   775,     0,   218,
       0,     0,   612,   612,     0,     0,     0,     0,   612,   612,
       0,   903,   904,     0,     0,     0,   218,     0,     0,     0,
     905,     0,     0,   906,     0,     0,   907,   908,     0,   909,
       0,   218,   910,    60,    61,    62,    63,    64,    65,    66,
      67,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   912,     0,   612,   612,     0,   612,
     612,   280,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   238,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   218,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   888,  -612,     0,     0,
       0,     0,     0,   612,     0,  -612,  -612,  -612,     0,     0,
    -612,  -612,  -612,     0,  -612,     0,     0,     0,     0,   682,
       0,     0,  -612,     0,  -612,  -612,  -612,     0,     0,     0,
     218,     0,     0,     0,  -612,  -612,     0,  -612,  -612,  -612,
    -612,  -612,     0,     0,     0,     0,     0,     0,   612,   218,
       0,     0,     0,   333,   334,   335,   336,   337,   338,   339,
     340,   341,   342,   343,   344,   345,  -612,     0,     0,     0,
     346,   347,     0,   218,     0,  -612,  -612,  -612,  -612,  -612,
    -612,  -612,  -612,  -612,  -612,  -612,  -612,  -612,     0,     0,
       0,     0,  -612,  -612,  -612,  -612,  -612,     0,  -612,  -612,
       0,     0,     0,     0,   348,  -612,   349,   350,   351,   352,
     353,   354,   355,   356,   357,   358,     0,     0,     0,  -612,
       0,     0,  -612,     0,     0,  -612,  -612,  -612,  -612,  -612,
    -612,  -612,  -612,  -612,  -612,  -612,  -612,  -612,     0,     0,
       0,     0,     0,  -612,  -612,  -612,     0,     0,  -612,  -612,
    -612,     0,  -612,     0,  -615,     0,   218,     0,     0,     0,
       0,     0,  -615,  -615,  -615,     0,     0,  -615,  -615,  -615,
       0,  -615,     0,     0,   218,     0,     0,     0,     0,  -615,
       0,  -615,  -615,  -615,     0,     0,     0,     0,     0,     0,
       0,  -615,  -615,     0,  -615,  -615,  -615,  -615,  -615,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     333,   334,   335,   336,   337,   338,   339,   340,   341,   342,
     343,   344,   345,  -615,     0,     0,     0,   346,   347,     0,
       0,     0,  -615,  -615,  -615,  -615,  -615,  -615,  -615,  -615,
    -615,  -615,  -615,  -615,  -615,     0,     0,     0,   218,  -615,
    -615,  -615,  -615,  -615,     0,  -615,  -615,     0,     0,     0,
       0,   348,  -615,   349,   350,   351,   352,   353,   354,   355,
     356,   357,   358,     0,     0,     0,  -615,     0,     0,  -615,
       0,     0,  -615,  -615,  -615,  -615,  -615,  -615,  -615,  -615,
    -615,  -615,  -615,  -615,  -615,     0,     0,     0,   218,     0,
    -615,  -615,  -615,  -716,     0,  -615,  -615,  -615,     0,  -615,
       0,  -716,  -716,  -716,     0,     0,  -716,  -716,  -716,     0,
    -716,     0,     0,     0,     0,     0,     0,     0,  -716,  -716,
    -716,  -716,  -716,     0,     0,     0,     0,     0,     0,     0,
    -716,  -716,     0,  -716,  -716,  -716,  -716,  -716,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   333,
     334,   335,   336,   337,   338,   339,   340,   341,   342,   343,
    -738,  -738,  -716,     0,     0,     0,   346,   347,     0,     0,
       0,  -716,  -716,  -716,  -716,  -716,  -716,  -716,  -716,  -716,
    -716,  -716,  -716,  -716,     0,     0,     0,     0,  -716,  -716,
    -716,  -716,  -716,     0,     0,  -716,     0,     0,     0,     0,
       0,  -716,   349,   350,   351,   352,   353,   354,   355,   356,
     357,   358,     0,     0,     0,  -716,     0,     0,  -716,     0,
       0,     0,  -716,  -716,  -716,  -716,  -716,  -716,  -716,  -716,
    -716,  -716,  -716,  -716,     0,     0,     0,     0,  -716,  -716,
    -716,  -716,  -717,     0,  -716,  -716,  -716,     0,  -716,     0,
    -717,  -717,  -717,     0,     0,  -717,  -717,  -717,     0,  -717,
       0,     0,     0,     0,     0,     0,     0,  -717,  -717,  -717,
    -717,  -717,     0,     0,     0,     0,     0,     0,     0,  -717,
    -717,     0,  -717,  -717,  -717,  -717,  -717,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -717,     0,     0,     0,     0,     0,     0,     0,     0,
    -717,  -717,  -717,  -717,  -717,  -717,  -717,  -717,  -717,  -717,
    -717,  -717,  -717,     0,     0,     0,     0,  -717,  -717,  -717,
    -717,  -717,     0,     0,  -717,     0,     0,     0,     0,     0,
    -717,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -717,     0,     0,  -717,     0,     0,
       0,  -717,  -717,  -717,  -717,  -717,  -717,  -717,  -717,  -717,
    -717,  -717,  -717,     0,     0,     0,     0,  -717,  -717,  -717,
    -717,  -304,     0,  -717,  -717,  -717,     0,  -717,     0,  -304,
    -304,  -304,     0,     0,  -304,  -304,  -304,     0,  -304,     0,
       0,     0,     0,     0,     0,     0,  -304,     0,  -304,  -304,
    -304,     0,     0,     0,     0,     0,     0,     0,  -304,  -304,
       0,  -304,  -304,  -304,  -304,  -304,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -304,     0,     0,     0,     0,     0,     0,     0,     0,  -304,
    -304,  -304,  -304,  -304,  -304,  -304,  -304,  -304,  -304,  -304,
    -304,  -304,     0,     0,     0,     0,  -304,  -304,  -304,  -304,
    -304,     0,   742,  -304,     0,     0,     0,     0,     0,  -304,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -304,     0,     0,  -304,     0,     0,  -113,
    -304,  -304,  -304,  -304,  -304,  -304,  -304,  -304,  -304,  -304,
    -304,  -304,     0,     0,     0,     0,     0,  -304,  -304,  -304,
    -445,     0,  -304,  -304,  -304,     0,  -304,     0,  -445,  -445,
    -445,     0,     0,  -445,  -445,  -445,     0,  -445,     0,     0,
       0,     0,     0,     0,     0,  -445,  -445,  -445,  -445,     0,
       0,     0,     0,     0,     0,     0,     0,  -445,  -445,     0,
    -445,  -445,  -445,  -445,  -445,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -445,
       0,     0,     0,     0,     0,     0,     0,     0,  -445,  -445,
    -445,  -445,  -445,  -445,  -445,  -445,  -445,  -445,  -445,  -445,
    -445,     0,     0,     0,     0,  -445,  -445,  -445,  -445,  -445,
       0,     0,  -445,     0,     0,     0,     0,     0,  -445,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -445,     0,     0,     0,     0,     0,     0,  -445,
       0,  -445,  -445,  -445,  -445,  -445,  -445,  -445,  -445,  -445,
    -445,     0,     0,     0,     0,  -445,  -445,  -445,  -445,  -296,
     234,  -445,  -445,  -445,     0,  -445,     0,  -296,  -296,  -296,
       0,     0,  -296,  -296,  -296,     0,  -296,     0,     0,     0,
       0,     0,     0,     0,  -296,     0,  -296,  -296,  -296,     0,
       0,     0,     0,     0,     0,     0,  -296,  -296,     0,  -296,
    -296,  -296,  -296,  -296,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -296,     0,
       0,     0,     0,     0,     0,     0,     0,  -296,  -296,  -296,
    -296,  -296,  -296,  -296,  -296,  -296,  -296,  -296,  -296,  -296,
       0,     0,     0,     0,  -296,  -296,  -296,  -296,  -296,     0,
       0,  -296,     0,     0,     0,     0,     0,  -296,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -296,     0,     0,  -296,     0,     0,     0,  -296,  -296,
    -296,  -296,  -296,  -296,  -296,  -296,  -296,  -296,  -296,  -296,
       0,     0,     0,     0,     0,  -296,  -296,  -296,  -737,     0,
    -296,  -296,  -296,     0,  -296,     0,  -737,  -737,  -737,     0,
       0,  -737,  -737,  -737,     0,  -737,     0,     0,     0,     0,
       0,     0,     0,  -737,  -737,  -737,  -737,     0,     0,     0,
       0,     0,     0,     0,     0,  -737,  -737,     0,  -737,  -737,
    -737,  -737,  -737,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -737,     0,     0,
       0,     0,     0,     0,     0,     0,  -737,  -737,  -737,  -737,
    -737,  -737,  -737,  -737,  -737,  -737,  -737,  -737,  -737,     0,
       0,     0,     0,  -737,  -737,  -737,  -737,  -737,     0,     0,
    -737,     0,     0,     0,     0,     0,  -737,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -737,     0,     0,     0,     0,     0,     0,  -737,     0,  -737,
    -737,  -737,  -737,  -737,  -737,  -737,  -737,  -737,  -737,     0,
       0,     0,     0,  -737,  -737,  -737,  -737,  -311,   234,  -737,
    -737,  -737,     0,  -737,     0,  -311,  -311,  -311,     0,     0,
    -311,  -311,  -311,     0,  -311,     0,     0,     0,     0,     0,
       0,     0,  -311,     0,  -311,  -311,     0,     0,     0,     0,
       0,     0,     0,     0,  -311,  -311,     0,  -311,  -311,  -311,
    -311,  -311,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -311,     0,     0,     0,
       0,     0,     0,     0,     0,  -311,  -311,  -311,  -311,  -311,
    -311,  -311,  -311,  -311,  -311,  -311,  -311,  -311,     0,     0,
       0,     0,  -311,  -311,  -311,  -311,  -311,     0,     0,  -311,
       0,     0,     0,     0,     0,  -311,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -311,
       0,     0,     0,     0,     0,     0,  -311,     0,  -311,  -311,
    -311,  -311,  -311,  -311,  -311,  -311,  -311,  -311,     0,     0,
       0,     0,     0,  -311,  -311,  -311,  -715,   231,  -311,  -311,
    -311,     0,  -311,     0,  -715,  -715,  -715,     0,     0,     0,
    -715,  -715,     0,  -715,     0,     0,     0,     0,     0,     0,
       0,  -715,  -715,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -715,  -715,     0,  -715,  -715,  -715,  -715,
    -715,     0,     0,     0,     0,   333,   334,   335,   336,   337,
     338,   339,   340,   341,   342,   343,   344,   345,     0,     0,
       0,     0,   346,   347,     0,  -715,     0,     0,     0,     0,
       0,     0,     0,     0,  -715,  -715,  -715,  -715,  -715,  -715,
    -715,  -715,  -715,  -715,  -715,  -715,  -715,     0,     0,     0,
       0,  -715,  -715,  -715,  -715,  -715,   348,   687,   349,   350,
     351,   352,   353,   354,   355,   356,   357,   358,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -715,     0,
       0,     0,   238,     0,  -111,  -715,     0,  -715,  -715,  -715,
    -715,  -715,  -715,  -715,  -715,  -715,  -715,     0,     0,     0,
       0,  -715,  -715,  -715,  -102,  -715,     0,  -715,     0,  -715,
       0,  -715,     0,  -715,  -715,  -715,     0,     0,     0,  -715,
    -715,     0,  -715,     0,     0,     0,     0,     0,     0,     0,
    -715,  -715,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -715,  -715,     0,  -715,  -715,  -715,  -715,  -715,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -715,     0,     0,     0,     0,     0,
       0,     0,     0,  -715,  -715,  -715,  -715,  -715,  -715,  -715,
    -715,  -715,  -715,  -715,  -715,  -715,     0,     0,     0,     0,
    -715,  -715,  -715,  -715,  -715,     0,   687,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -715,     0,     0,
       0,     0,     0,  -111,  -715,     0,  -715,  -715,  -715,  -715,
    -715,  -715,  -715,  -715,  -715,  -715,     0,     0,     0,     0,
    -715,  -715,  -715,  -715,  -304,     0,  -715,     0,  -715,     0,
    -715,     0,  -304,  -304,  -304,     0,     0,     0,  -304,  -304,
       0,  -304,     0,     0,     0,     0,     0,     0,     0,  -304,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -304,  -304,     0,  -304,  -304,  -304,  -304,  -304,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -304,     0,     0,     0,     0,     0,     0,
       0,     0,  -304,  -304,  -304,  -304,  -304,  -304,  -304,  -304,
    -304,  -304,  -304,  -304,  -304,     0,     0,     0,     0,  -304,
    -304,  -304,  -304,  -304,     0,   688,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -304,     0,     0,     0,
       0,     0,  -113,  -304,     0,  -304,  -304,  -304,  -304,  -304,
    -304,  -304,  -304,  -304,  -304,     0,     0,     0,     0,     0,
    -304,  -304,  -104,  -304,     0,  -304,     0,  -304,     0,  -304,
       0,  -304,  -304,  -304,     0,     0,     0,  -304,  -304,     0,
    -304,     0,     0,     0,     0,     0,     0,     0,  -304,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -304,  -304,     0,  -304,  -304,  -304,  -304,  -304,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -304,     0,     0,     0,     0,     0,     0,     0,
       0,  -304,  -304,  -304,  -304,  -304,  -304,  -304,  -304,  -304,
    -304,  -304,  -304,  -304,     0,     0,     0,     0,  -304,  -304,
    -304,  -304,  -304,     0,   688,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -304,     0,     0,     0,     0,
       0,  -113,  -304,     0,  -304,  -304,  -304,  -304,  -304,  -304,
    -304,  -304,  -304,  -304,     0,     0,     0,     0,     0,  -304,
    -304,  -304,     0,     0,  -304,     0,  -304,   258,  -304,     5,
       6,     7,     8,     9,  -737,  -737,  -737,    10,    11,     0,
       0,  -737,    12,     0,    13,    14,    15,    16,    17,    18,
      19,     0,     0,     0,     0,     0,    20,    21,    22,    23,
      24,    25,    26,     0,     0,    27,     0,     0,     0,     0,
       0,    28,    29,   259,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,     0,    41,    42,    43,    44,    45,
      46,    47,    48,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    49,    50,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    51,    52,     0,
       0,     0,     0,     0,     0,     0,    53,     0,     0,    54,
      55,     0,    56,    57,     0,    58,     0,     0,    59,    60,
      61,    62,    63,    64,    65,    66,    67,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    68,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -737,
     258,  -737,     5,     6,     7,     8,     9,     0,     0,  -737,
      10,    11,     0,  -737,  -737,    12,     0,    13,    14,    15,
      16,    17,    18,    19,     0,     0,     0,     0,     0,    20,
      21,    22,    23,    24,    25,    26,     0,     0,    27,     0,
       0,     0,     0,     0,    28,    29,   259,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,     0,    41,    42,
      43,    44,    45,    46,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    49,    50,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      51,    52,     0,     0,     0,     0,     0,     0,     0,    53,
       0,     0,    54,    55,     0,    56,    57,     0,    58,     0,
       0,    59,    60,    61,    62,    63,    64,    65,    66,    67,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      68,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -737,   258,  -737,     5,     6,     7,     8,     9,
       0,     0,  -737,    10,    11,     0,     0,  -737,    12,  -737,
      13,    14,    15,    16,    17,    18,    19,     0,     0,     0,
       0,     0,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,    28,    29,   259,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
       0,    41,    42,    43,    44,    45,    46,    47,    48,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    49,    50,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    51,    52,     0,     0,     0,     0,     0,
       0,     0,    53,     0,     0,    54,    55,     0,    56,    57,
       0,    58,     0,     0,    59,    60,    61,    62,    63,    64,
      65,    66,    67,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    68,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -737,   258,  -737,     5,     6,
       7,     8,     9,     0,     0,  -737,    10,    11,     0,     0,
    -737,    12,     0,    13,    14,    15,    16,    17,    18,    19,
    -737,     0,     0,     0,     0,    20,    21,    22,    23,    24,
      25,    26,     0,     0,    27,     0,     0,     0,     0,     0,
      28,    29,   259,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,     0,    41,    42,    43,    44,    45,    46,
      47,    48,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    49,    50,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    51,    52,     0,     0,
       0,     0,     0,     0,     0,    53,     0,     0,    54,    55,
       0,    56,    57,     0,    58,     0,     0,    59,    60,    61,
      62,    63,    64,    65,    66,    67,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    68,    69,    70,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -737,   258,
    -737,     5,     6,     7,     8,     9,     0,     0,  -737,    10,
      11,     0,     0,  -737,    12,     0,    13,    14,    15,    16,
      17,    18,    19,     0,     0,     0,     0,     0,    20,    21,
      22,    23,    24,    25,    26,     0,     0,    27,     0,     0,
       0,     0,     0,    28,    29,   259,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,     0,    41,    42,    43,
      44,    45,    46,    47,    48,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    49,    50,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    51,
      52,     0,     0,     0,     0,     0,     0,     0,    53,     0,
       0,    54,    55,     0,    56,    57,     0,    58,     0,     0,
      59,    60,    61,    62,    63,    64,    65,    66,    67,     0,
       0,     0,     0,     0,     0,     0,     0,   258,     0,     5,
       6,     7,     8,     9,     0,  -737,  -737,    10,    11,    68,
      69,    70,    12,     0,    13,    14,    15,    16,    17,    18,
      19,  -737,     0,  -737,     0,     0,    20,    21,    22,    23,
      24,    25,    26,     0,     0,    27,     0,     0,     0,     0,
       0,    28,    29,   259,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,     0,    41,    42,    43,    44,    45,
      46,    47,    48,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    49,    50,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    51,    52,     0,
       0,     0,     0,     0,     0,     0,    53,     0,     0,    54,
      55,     0,    56,    57,     0,    58,     0,     0,    59,    60,
      61,    62,    63,    64,    65,    66,    67,     0,     0,     0,
       0,     0,     0,     0,     0,   258,     0,     5,     6,     7,
       8,     9,     0,     0,     0,    10,    11,    68,    69,    70,
      12,     0,    13,    14,    15,    16,    17,    18,    19,  -737,
       0,  -737,     0,     0,    20,    21,    22,    23,    24,    25,
      26,     0,     0,    27,     0,     0,     0,     0,     0,    28,
      29,   259,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,     0,    41,    42,    43,    44,    45,    46,    47,
      48,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      49,    50,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    51,    52,     0,     0,     0,
       0,     0,     0,     0,    53,     0,     0,   260,    55,     0,
      56,    57,     0,    58,     0,     0,    59,    60,    61,    62,
      63,    64,    65,    66,    67,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    68,    69,    70,     0,     0,
       0,     0,     0,     0,     0,  -737,     0,  -737,   258,  -737,
       5,     6,     7,     8,     9,     0,     0,     0,    10,    11,
       0,     0,     0,    12,     0,    13,    14,    15,    16,    17,
      18,    19,     0,     0,     0,     0,     0,    20,    21,    22,
      23,    24,    25,    26,     0,     0,    27,     0,     0,     0,
       0,     0,    28,    29,   259,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,     0,    41,    42,    43,    44,
      45,    46,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    49,    50,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    51,    52,
       0,     0,     0,     0,     0,     0,     0,    53,     0,     0,
      54,    55,     0,    56,    57,     0,    58,     0,     0,    59,
      60,    61,    62,    63,    64,    65,    66,    67,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    68,    69,
      70,     0,     0,     0,     0,     0,     0,     0,  -737,     0,
    -737,     4,  -737,     5,     6,     7,     8,     9,     0,     0,
       0,    10,    11,     0,     0,     0,    12,     0,    13,    14,
      15,    16,    17,    18,    19,     0,     0,     0,     0,     0,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,     0,    41,
      42,    43,    44,    45,    46,    47,    48,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    49,    50,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    51,    52,     0,     0,     0,     0,     0,     0,     0,
      53,     0,     0,    54,    55,     0,    56,    57,     0,    58,
       0,     0,    59,    60,    61,    62,    63,    64,    65,    66,
      67,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    68,    69,    70,     0,     0,  -737,     0,     0,     0,
       0,     0,     0,  -737,   258,  -737,     5,     6,     7,     8,
       9,     0,     0,     0,    10,    11,     0,     0,     0,    12,
       0,    13,    14,    15,    16,    17,    18,    19,     0,     0,
       0,     0,     0,    20,    21,    22,    23,    24,    25,    26,
       0,     0,    27,     0,     0,     0,     0,     0,    28,    29,
     259,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,     0,    41,    42,    43,    44,    45,    46,    47,    48,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    49,
      50,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    51,    52,     0,     0,     0,     0,
       0,     0,     0,    53,     0,     0,    54,    55,     0,    56,
      57,     0,    58,     0,     0,    59,    60,    61,    62,    63,
      64,    65,    66,    67,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    68,    69,    70,     0,     0,  -737,
       0,     0,     0,     0,     0,     0,  -737,   258,  -737,     5,
       6,     7,     8,     9,     0,     0,  -737,    10,    11,     0,
       0,     0,    12,     0,    13,    14,    15,    16,    17,    18,
      19,     0,     0,     0,     0,     0,    20,    21,    22,    23,
      24,    25,    26,     0,     0,    27,     0,     0,     0,     0,
       0,    28,    29,   259,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,     0,    41,    42,    43,    44,    45,
      46,    47,    48,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    49,    50,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    51,    52,     0,
       0,     0,     0,     0,     0,     0,    53,     0,     0,    54,
      55,     0,    56,    57,     0,    58,     0,     0,    59,    60,
      61,    62,    63,    64,    65,    66,    67,     0,     0,     0,
       0,     0,     0,     0,     0,   258,     0,     5,     6,     7,
       8,     9,     0,     0,     0,    10,    11,    68,    69,    70,
      12,     0,    13,    14,    15,    16,    17,    18,    19,  -737,
       0,  -737,     0,     0,    20,    21,    22,    23,    24,    25,
      26,     0,     0,    27,     0,     0,     0,     0,     0,    28,
      29,   259,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,     0,    41,    42,    43,    44,    45,    46,    47,
      48,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      49,    50,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    51,    52,     0,     0,     0,
       0,     0,     0,     0,    53,     0,     0,    54,    55,     0,
      56,    57,     0,    58,     0,     0,    59,    60,    61,    62,
      63,    64,    65,    66,    67,     0,  -737,     0,     0,     0,
       0,     0,     0,     0,     0,     5,     6,     7,     0,     9,
       0,     0,     0,    10,    11,    68,    69,    70,    12,     0,
      13,    14,    15,    16,    17,    18,    19,  -737,     0,  -737,
       0,     0,    20,    21,    22,    23,    24,    25,    26,     0,
       0,   207,     0,     0,     0,     0,     0,     0,    29,     0,
       0,    32,    33,    34,    35,    36,    37,    38,    39,    40,
     208,    41,    42,    43,    44,    45,    46,    47,    48,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    49,    50,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    51,    52,     0,     0,     0,     0,     0,
       0,     0,   209,     0,     0,   210,    55,     0,    56,    57,
       0,   211,   212,   213,    59,    60,   214,    62,    63,    64,
      65,    66,    67,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     5,     6,     7,     0,     9,     0,     0,
       0,    10,    11,    68,   215,    70,    12,     0,    13,    14,
      15,    16,    17,    18,    19,     0,     0,   238,     0,     0,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,     0,    29,     0,     0,    32,
      33,    34,    35,    36,    37,    38,    39,    40,     0,    41,
      42,    43,    44,    45,    46,    47,    48,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    49,    50,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    51,    52,     0,     0,     0,     0,     0,     0,     0,
     209,     0,     0,   210,    55,     0,    56,    57,     0,     0,
       0,     0,    59,    60,    61,    62,    63,    64,    65,    66,
      67,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     5,     6,     7,     0,     9,     0,     0,     0,    10,
      11,    68,    69,    70,    12,     0,    13,    14,    15,    16,
      17,    18,    19,   306,     0,   307,     0,     0,    20,    21,
      22,    23,    24,    25,    26,     0,     0,    27,     0,     0,
       0,     0,     0,     0,    29,     0,     0,    32,    33,    34,
      35,    36,    37,    38,    39,    40,     0,    41,    42,    43,
      44,    45,    46,    47,    48,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    49,    50,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    51,
      52,     0,     0,     0,     0,     0,     0,     0,   209,     0,
       0,   210,    55,     0,    56,    57,     0,     0,     0,     0,
      59,    60,    61,    62,    63,    64,    65,    66,    67,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     5,
       6,     7,     8,     9,     0,     0,     0,    10,    11,    68,
      69,    70,    12,     0,    13,    14,    15,    16,    17,    18,
      19,     0,     0,   238,     0,     0,    20,    21,    22,    23,
      24,    25,    26,     0,     0,    27,     0,     0,     0,     0,
       0,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,     0,    41,    42,    43,    44,    45,
      46,    47,    48,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    49,    50,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    51,    52,     0,
       0,     0,     0,     0,     0,     0,    53,     0,     0,    54,
      55,     0,    56,    57,     0,    58,     0,     0,    59,    60,
      61,    62,    63,    64,    65,    66,    67,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     5,     6,     7,
       8,     9,     0,     0,     0,    10,    11,    68,    69,    70,
      12,     0,    13,    14,    15,    16,    17,    18,    19,   502,
       0,     0,     0,     0,    20,    21,    22,    23,    24,    25,
      26,     0,     0,    27,     0,     0,     0,     0,     0,    28,
      29,   259,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,     0,    41,    42,    43,    44,    45,    46,    47,
      48,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      49,    50,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    51,    52,     0,     0,     0,
       0,     0,     0,     0,    53,     0,     0,    54,    55,     0,
      56,    57,     0,    58,     0,     0,    59,    60,    61,    62,
      63,    64,    65,    66,    67,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    68,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   502,   127,   128,
     129,   130,   131,   132,   133,   134,   135,   136,   137,   138,
     139,   140,   141,   142,   143,   144,   145,   146,   147,   148,
     149,   150,     0,     0,     0,   151,   152,   153,   396,   397,
     398,   399,   158,   159,   160,     0,     0,     0,     0,     0,
     161,   162,   163,   164,   400,   401,   402,   403,   169,    37,
      38,   404,    40,     0,    41,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   171,   172,   173,   174,   175,   176,   177,   178,   179,
       0,     0,   180,   181,     0,     0,     0,     0,   182,   183,
     184,   185,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   186,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   188,   189,   190,   191,
     192,   193,   194,   195,   196,   197,     0,   198,   199,     0,
       0,     0,     0,     0,   200,   405,   127,   128,   129,   130,
     131,   132,   133,   134,   135,   136,   137,   138,   139,   140,
     141,   142,   143,   144,   145,   146,   147,   148,   149,   150,
       0,     0,     0,   151,   152,   153,   154,   155,   156,   157,
     158,   159,   160,     0,     0,     0,     0,     0,   161,   162,
     163,   164,   165,   166,   167,   168,   169,   289,   290,   170,
     291,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   171,
     172,   173,   174,   175,   176,   177,   178,   179,     0,     0,
     180,   181,     0,     0,     0,     0,   182,   183,   184,   185,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   186,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   188,   189,   190,   191,   192,   193,
     194,   195,   196,   197,     0,   198,   199,     0,     0,     0,
       0,     0,   200,   127,   128,   129,   130,   131,   132,   133,
     134,   135,   136,   137,   138,   139,   140,   141,   142,   143,
     144,   145,   146,   147,   148,   149,   150,     0,     0,     0,
     151,   152,   153,   154,   155,   156,   157,   158,   159,   160,
       0,     0,     0,     0,     0,   161,   162,   163,   164,   165,
     166,   167,   168,   169,   240,     0,   170,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   171,   172,   173,   174,
     175,   176,   177,   178,   179,     0,     0,   180,   181,     0,
       0,     0,     0,   182,   183,   184,   185,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   186,
     187,     0,     0,    60,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   188,   189,   190,   191,   192,   193,   194,   195,   196,
     197,     0,   198,   199,     0,     0,     0,     0,     0,   200,
     127,   128,   129,   130,   131,   132,   133,   134,   135,   136,
     137,   138,   139,   140,   141,   142,   143,   144,   145,   146,
     147,   148,   149,   150,     0,     0,     0,   151,   152,   153,
     154,   155,   156,   157,   158,   159,   160,     0,     0,     0,
       0,     0,   161,   162,   163,   164,   165,   166,   167,   168,
     169,     0,     0,   170,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   171,   172,   173,   174,   175,   176,   177,
     178,   179,     0,     0,   180,   181,     0,     0,     0,     0,
     182,   183,   184,   185,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   186,   187,     0,     0,
      60,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   188,   189,
     190,   191,   192,   193,   194,   195,   196,   197,     0,   198,
     199,     0,     0,     0,     0,     0,   200,   127,   128,   129,
     130,   131,   132,   133,   134,   135,   136,   137,   138,   139,
     140,   141,   142,   143,   144,   145,   146,   147,   148,   149,
     150,     0,     0,     0,   151,   152,   153,   154,   155,   156,
     157,   158,   159,   160,     0,     0,     0,     0,     0,   161,
     162,   163,   164,   165,   166,   167,   168,   169,     0,     0,
     170,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     171,   172,   173,   174,   175,   176,   177,   178,   179,     0,
       0,   180,   181,     0,     0,     0,     0,   182,   183,   184,
     185,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   186,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,   190,   191,   192,
     193,   194,   195,   196,   197,     0,   198,   199,     5,     6,
       7,     0,     9,   200,     0,     0,    10,    11,     0,     0,
       0,    12,     0,    13,    14,    15,   246,   247,    18,    19,
       0,     0,     0,     0,     0,    20,   248,   249,    23,    24,
      25,    26,     0,     0,   207,     0,     0,     0,     0,     0,
       0,   278,     0,     0,    32,    33,    34,    35,    36,    37,
      38,    39,    40,     0,    41,    42,    43,    44,    45,    46,
      47,    48,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   279,     0,     0,   210,    55,
       0,    56,    57,     0,     0,     0,     0,    59,    60,    61,
      62,    63,    64,    65,    66,    67,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     5,
       6,     7,     0,     9,     0,     0,   280,    10,    11,     0,
       0,     0,    12,   281,    13,    14,    15,   246,   247,    18,
      19,     0,     0,     0,     0,     0,    20,   248,   249,    23,
      24,    25,    26,     0,     0,   207,     0,     0,     0,     0,
       0,     0,   278,     0,     0,    32,    33,    34,    35,    36,
      37,    38,    39,    40,     0,    41,    42,    43,    44,    45,
      46,    47,    48,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   279,     0,     0,   210,
      55,     0,    56,    57,     0,     0,     0,     0,    59,    60,
      61,    62,    63,    64,    65,    66,    67,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       5,     6,     7,     8,     9,     0,     0,   280,    10,    11,
       0,     0,     0,    12,   525,    13,    14,    15,    16,    17,
      18,    19,     0,     0,     0,     0,     0,    20,    21,    22,
      23,    24,    25,    26,     0,     0,    27,     0,     0,     0,
       0,     0,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,     0,    41,    42,    43,    44,
      45,    46,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    49,    50,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    51,    52,
       0,     0,     0,     0,     0,     0,     0,    53,     0,     0,
      54,    55,     0,    56,    57,     0,    58,     0,     0,    59,
      60,    61,    62,    63,    64,    65,    66,    67,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     0,     9,     0,     0,     0,    10,    11,    68,    69,
      70,    12,     0,    13,    14,    15,    16,    17,    18,    19,
       0,     0,     0,     0,     0,    20,    21,    22,    23,    24,
      25,    26,     0,     0,   207,     0,     0,     0,     0,     0,
       0,    29,     0,     0,    32,    33,    34,    35,    36,    37,
      38,    39,    40,   208,    41,    42,    43,    44,    45,    46,
      47,    48,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    49,    50,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    51,    52,     0,     0,
       0,     0,     0,     0,     0,   209,     0,     0,   210,    55,
       0,    56,    57,     0,   211,   212,   213,    59,    60,   214,
      62,    63,    64,    65,    66,    67,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     8,
       9,     0,     0,     0,    10,    11,    68,   215,    70,    12,
       0,    13,    14,    15,    16,    17,    18,    19,     0,     0,
       0,     0,     0,    20,    21,    22,    23,    24,    25,    26,
       0,     0,    27,     0,     0,     0,     0,     0,    28,    29,
       0,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,     0,    41,    42,    43,    44,    45,    46,    47,    48,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    49,
      50,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    51,    52,     0,     0,     0,     0,
       0,     0,     0,    53,     0,     0,    54,    55,     0,    56,
      57,     0,    58,     0,     0,    59,    60,    61,    62,    63,
      64,    65,    66,    67,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     0,     9,     0,
       0,     0,    10,    11,    68,    69,    70,    12,     0,    13,
      14,    15,   246,   247,    18,    19,     0,     0,     0,     0,
       0,    20,   248,   249,    23,    24,    25,    26,     0,     0,
     207,     0,     0,     0,     0,     0,     0,    29,     0,     0,
      32,    33,    34,    35,    36,    37,    38,    39,    40,   208,
      41,    42,    43,    44,    45,    46,    47,    48,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    49,    50,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    51,    52,     0,     0,     0,     0,     0,     0,
       0,   209,     0,     0,   210,    55,     0,    56,    57,     0,
     619,   212,   213,    59,    60,   214,    62,    63,    64,    65,
      66,    67,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     0,     9,     0,     0,     0,
      10,    11,    68,   215,    70,    12,     0,    13,    14,    15,
     246,   247,    18,    19,     0,     0,     0,     0,     0,    20,
     248,   249,    23,    24,    25,    26,     0,     0,   207,     0,
       0,     0,     0,     0,     0,    29,     0,     0,    32,    33,
      34,    35,    36,    37,    38,    39,    40,   208,    41,    42,
      43,    44,    45,    46,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    49,    50,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      51,    52,     0,     0,     0,     0,     0,     0,     0,   209,
       0,     0,   210,    55,     0,    56,    57,     0,   211,   212,
       0,    59,    60,   214,    62,    63,    64,    65,    66,    67,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       5,     6,     7,     0,     9,     0,     0,     0,    10,    11,
      68,   215,    70,    12,     0,    13,    14,    15,   246,   247,
      18,    19,     0,     0,     0,     0,     0,    20,   248,   249,
      23,    24,    25,    26,     0,     0,   207,     0,     0,     0,
       0,     0,     0,    29,     0,     0,    32,    33,    34,    35,
      36,    37,    38,    39,    40,   208,    41,    42,    43,    44,
      45,    46,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    49,    50,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    51,    52,
       0,     0,     0,     0,     0,     0,     0,   209,     0,     0,
     210,    55,     0,    56,    57,     0,     0,   212,   213,    59,
      60,   214,    62,    63,    64,    65,    66,    67,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     0,     9,     0,     0,     0,    10,    11,    68,   215,
      70,    12,     0,    13,    14,    15,   246,   247,    18,    19,
       0,     0,     0,     0,     0,    20,   248,   249,    23,    24,
      25,    26,     0,     0,   207,     0,     0,     0,     0,     0,
       0,    29,     0,     0,    32,    33,    34,    35,    36,    37,
      38,    39,    40,   208,    41,    42,    43,    44,    45,    46,
      47,    48,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    49,    50,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    51,    52,     0,     0,
       0,     0,     0,     0,     0,   209,     0,     0,   210,    55,
       0,    56,    57,     0,   619,   212,     0,    59,    60,   214,
      62,    63,    64,    65,    66,    67,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     0,
       9,     0,     0,     0,    10,    11,    68,   215,    70,    12,
       0,    13,    14,    15,   246,   247,    18,    19,     0,     0,
       0,     0,     0,    20,   248,   249,    23,    24,    25,    26,
       0,     0,   207,     0,     0,     0,     0,     0,     0,    29,
       0,     0,    32,    33,    34,    35,    36,    37,    38,    39,
      40,   208,    41,    42,    43,    44,    45,    46,    47,    48,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    49,
      50,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    51,    52,     0,     0,     0,     0,
       0,     0,     0,   209,     0,     0,   210,    55,     0,    56,
      57,     0,     0,   212,     0,    59,    60,   214,    62,    63,
      64,    65,    66,    67,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     0,     9,     0,
       0,     0,    10,    11,    68,   215,    70,    12,     0,    13,
      14,    15,    16,    17,    18,    19,     0,     0,     0,     0,
       0,    20,    21,    22,    23,    24,    25,    26,     0,     0,
     207,     0,     0,     0,     0,     0,     0,    29,     0,     0,
      32,    33,    34,    35,    36,    37,    38,    39,    40,     0,
      41,    42,    43,    44,    45,    46,    47,    48,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    49,    50,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    51,    52,     0,     0,     0,     0,     0,     0,
       0,   209,     0,     0,   210,    55,     0,    56,    57,     0,
     518,     0,     0,    59,    60,    61,    62,    63,    64,    65,
      66,    67,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     0,     9,     0,     0,     0,
      10,    11,    68,   215,    70,    12,     0,    13,    14,    15,
     246,   247,    18,    19,     0,     0,     0,     0,     0,    20,
     248,   249,    23,    24,    25,    26,     0,     0,   207,     0,
       0,     0,     0,     0,     0,    29,     0,     0,    32,    33,
      34,    35,    36,    37,    38,    39,    40,     0,    41,    42,
      43,    44,    45,    46,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    49,    50,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      51,    52,     0,     0,     0,     0,     0,     0,     0,   209,
       0,     0,   210,    55,     0,    56,    57,     0,   771,     0,
       0,    59,    60,    61,    62,    63,    64,    65,    66,    67,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       5,     6,     7,     0,     9,     0,     0,     0,    10,    11,
      68,   215,    70,    12,     0,    13,    14,    15,   246,   247,
      18,    19,     0,     0,     0,     0,     0,    20,   248,   249,
      23,    24,    25,    26,     0,     0,   207,     0,     0,     0,
       0,     0,     0,    29,     0,     0,    32,    33,    34,    35,
      36,    37,    38,    39,    40,     0,    41,    42,    43,    44,
      45,    46,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    49,    50,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    51,    52,
       0,     0,     0,     0,     0,     0,     0,   209,     0,     0,
     210,    55,     0,    56,    57,     0,   518,     0,     0,    59,
      60,    61,    62,    63,    64,    65,    66,    67,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     0,     9,     0,     0,     0,    10,    11,    68,   215,
      70,    12,     0,    13,    14,    15,   246,   247,    18,    19,
       0,     0,     0,     0,     0,    20,   248,   249,    23,    24,
      25,    26,     0,     0,   207,     0,     0,     0,     0,     0,
       0,    29,     0,     0,    32,    33,    34,    35,    36,    37,
      38,    39,    40,     0,    41,    42,    43,    44,    45,    46,
      47,    48,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    49,    50,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    51,    52,     0,     0,
       0,     0,     0,     0,     0,   209,     0,     0,   210,    55,
       0,    56,    57,     0,   815,     0,     0,    59,    60,    61,
      62,    63,    64,    65,    66,    67,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     0,
       9,     0,     0,     0,    10,    11,    68,   215,    70,    12,
       0,    13,    14,    15,   246,   247,    18,    19,     0,     0,
       0,     0,     0,    20,   248,   249,    23,    24,    25,    26,
       0,     0,   207,     0,     0,     0,     0,     0,     0,    29,
       0,     0,    32,    33,    34,    35,    36,    37,    38,    39,
      40,     0,    41,    42,    43,    44,    45,    46,    47,    48,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    49,
      50,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    51,    52,     0,     0,     0,     0,
       0,     0,     0,   209,     0,     0,   210,    55,     0,    56,
      57,     0,  1033,     0,     0,    59,    60,    61,    62,    63,
      64,    65,    66,    67,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     0,     9,     0,
       0,     0,    10,    11,    68,   215,    70,    12,     0,    13,
      14,    15,   246,   247,    18,    19,     0,     0,     0,     0,
       0,    20,   248,   249,    23,    24,    25,    26,     0,     0,
     207,     0,     0,     0,     0,     0,     0,    29,     0,     0,
      32,    33,    34,    35,    36,    37,    38,    39,    40,     0,
      41,    42,    43,    44,    45,    46,    47,    48,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    49,    50,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    51,    52,     0,     0,     0,     0,     0,     0,
       0,   209,     0,     0,   210,    55,     0,    56,    57,     0,
       0,     0,     0,    59,    60,    61,    62,    63,    64,    65,
      66,    67,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     0,     9,     0,     0,     0,
      10,    11,    68,   215,    70,    12,     0,    13,    14,    15,
      16,    17,    18,    19,     0,     0,     0,     0,     0,    20,
      21,    22,    23,    24,    25,    26,     0,     0,   207,     0,
       0,     0,     0,     0,     0,    29,     0,     0,    32,    33,
      34,    35,    36,    37,    38,    39,    40,     0,    41,    42,
      43,    44,    45,    46,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    49,    50,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      51,    52,     0,     0,     0,     0,     0,     0,     0,   209,
       0,     0,   210,    55,     0,    56,    57,     0,     0,     0,
       0,    59,    60,    61,    62,    63,    64,    65,    66,    67,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       5,     6,     7,     0,     9,     0,     0,     0,    10,    11,
      68,   215,    70,    12,     0,    13,    14,    15,    16,    17,
      18,    19,     0,     0,     0,     0,     0,    20,    21,    22,
      23,    24,    25,    26,     0,     0,    27,     0,     0,     0,
       0,     0,     0,    29,     0,     0,    32,    33,    34,    35,
      36,    37,    38,    39,    40,     0,    41,    42,    43,    44,
      45,    46,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    49,    50,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    51,    52,
       0,     0,     0,     0,     0,     0,     0,   209,     0,     0,
     210,    55,     0,    56,    57,     0,     0,     0,     0,    59,
      60,    61,    62,    63,    64,    65,    66,    67,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     0,     9,     0,     0,     0,    10,    11,    68,    69,
      70,    12,     0,    13,    14,    15,   246,   247,    18,    19,
       0,     0,     0,     0,     0,    20,   248,   249,    23,    24,
      25,    26,     0,     0,   207,     0,     0,     0,     0,     0,
       0,   278,     0,     0,    32,    33,    34,    35,    36,    37,
      38,    39,    40,     0,    41,    42,    43,    44,    45,    46,
      47,    48,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   279,     0,     0,   329,    55,
       0,    56,    57,     0,   330,     0,     0,    59,    60,    61,
      62,    63,    64,    65,    66,    67,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     0,     9,     0,     0,     0,
      10,    11,     0,     0,     0,    12,   280,    13,    14,    15,
     246,   247,    18,    19,     0,     0,     0,     0,     0,    20,
     248,   249,    23,    24,    25,    26,     0,     0,   207,     0,
       0,     0,     0,     0,     0,   278,     0,     0,    32,    33,
      34,    35,    36,    37,    38,    39,    40,     0,    41,    42,
      43,    44,    45,    46,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   377,
       0,     0,    54,    55,     0,    56,    57,     0,    58,     0,
       0,    59,    60,    61,    62,    63,    64,    65,    66,    67,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     0,
       9,     0,     0,     0,    10,    11,     0,     0,     0,    12,
     280,    13,    14,    15,   246,   247,    18,    19,     0,     0,
       0,     0,     0,    20,   248,   249,    23,    24,    25,    26,
       0,     0,   207,     0,     0,     0,     0,     0,     0,   278,
       0,     0,    32,    33,    34,   385,    36,    37,    38,   386,
      40,     0,    41,    42,    43,    44,    45,    46,    47,    48,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   387,     0,
       0,     0,     0,   388,     0,     0,   210,    55,     0,    56,
      57,     0,     0,     0,     0,    59,    60,    61,    62,    63,
      64,    65,    66,    67,     0,     0,     0,     0,     0,     0,
       5,     6,     7,     0,     9,     0,     0,     0,    10,    11,
       0,     0,     0,    12,   280,    13,    14,    15,   246,   247,
      18,    19,     0,     0,     0,     0,     0,    20,   248,   249,
      23,    24,    25,    26,     0,     0,   207,     0,     0,     0,
       0,     0,     0,   278,     0,     0,    32,    33,    34,   385,
      36,    37,    38,   386,    40,     0,    41,    42,    43,    44,
      45,    46,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   388,     0,     0,
     210,    55,     0,    56,    57,     0,     0,     0,     0,    59,
      60,    61,    62,    63,    64,    65,    66,    67,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     0,     9,     0,
       0,     0,    10,    11,     0,     0,     0,    12,   280,    13,
      14,    15,   246,   247,    18,    19,     0,     0,     0,     0,
       0,    20,   248,   249,    23,    24,    25,    26,     0,     0,
     207,     0,     0,     0,     0,     0,     0,   278,     0,     0,
      32,    33,    34,    35,    36,    37,    38,    39,    40,     0,
      41,    42,    43,    44,    45,    46,    47,    48,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   279,     0,     0,   329,    55,     0,    56,    57,     0,
       0,     0,     0,    59,    60,    61,    62,    63,    64,    65,
      66,    67,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     0,     9,     0,     0,     0,    10,    11,     0,     0,
       0,    12,   280,    13,    14,    15,   246,   247,    18,    19,
       0,     0,     0,     0,     0,    20,   248,   249,    23,    24,
      25,    26,     0,     0,   207,     0,     0,     0,     0,     0,
       0,   278,     0,     0,    32,    33,    34,    35,    36,    37,
      38,    39,    40,     0,    41,    42,    43,    44,    45,    46,
      47,    48,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   981,     0,     0,   210,    55,
       0,    56,    57,     0,     0,     0,     0,    59,    60,    61,
      62,    63,    64,    65,    66,    67,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     0,     9,     0,     0,     0,
      10,    11,     0,     0,     0,    12,   280,    13,    14,    15,
     246,   247,    18,    19,     0,     0,     0,     0,     0,    20,
     248,   249,    23,    24,    25,    26,     0,     0,   207,     0,
       0,     0,     0,     0,     0,   278,     0,     0,    32,    33,
      34,    35,    36,    37,    38,    39,    40,     0,    41,    42,
      43,    44,    45,    46,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   991,
       0,     0,   210,    55,     0,    56,    57,    23,    24,    25,
      26,    59,    60,    61,    62,    63,    64,    65,    66,    67,
       0,     0,     0,    32,    33,    34,   900,     0,     0,     0,
     901,     0,   902,     0,    42,    43,    44,    45,    46,     0,
     280,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   842,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   903,   904,     0,     0,     0,
       0,     0,     0,     0,   905,     0,     0,   906,     0,     0,
     907,   908,     0,   909,   845,     0,   910,    60,   911,    62,
      63,    64,    65,    66,    67,    23,    24,    25,    26,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   912,     0,
       0,    32,    33,    34,   900,   280,     0,     0,   901,     0,
       0,     0,    42,    43,    44,    45,    46,     0,    23,    24,
      25,    26,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    32,    33,    34,   900,     0,     0,
       0,   901,     0,   903,   904,    42,    43,    44,    45,    46,
       0,     0,   905,     0,     0,   906,     0,     0,   907,   908,
       0,  1090,     0,     0,   910,    60,    61,    62,    63,    64,
      65,    66,    67,     0,     0,     0,   903,   904,     0,     0,
       0,     0,     0,     0,     0,   905,   912,     0,   906,     0,
       0,   907,   908,   280,   909,     0,     0,   910,    60,    61,
      62,    63,    64,    65,    66,    67,    23,    24,    25,    26,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   912,
       0,     0,    32,    33,    34,   900,   280,     0,     0,   901,
       0,     0,     0,    42,    43,    44,    45,    46,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   903,   904,     0,     0,     0,     0,
       0,     0,     0,   905,   562,   563,   906,     0,   564,   907,
     908,     0,     0,     0,     0,   910,    60,    61,    62,    63,
      64,    65,    66,    67,     0,     0,     0,     0,   171,   172,
     173,   174,   175,   176,   177,   178,   179,   912,     0,   180,
     181,     0,     0,     0,   280,   182,   183,   184,   185,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   186,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   188,   189,   190,   191,   192,   193,   194,
     195,   196,   197,     0,   198,   199,   575,   570,     0,     0,
     576,   200,   234,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     171,   172,   173,   174,   175,   176,   177,   178,   179,     0,
       0,   180,   181,     0,     0,     0,     0,   182,   183,   184,
     185,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   186,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,   190,   191,   192,
     193,   194,   195,   196,   197,     0,   198,   199,   623,   563,
       0,     0,   624,   200,   234,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   171,   172,   173,   174,   175,   176,   177,   178,
     179,     0,     0,   180,   181,     0,     0,     0,     0,   182,
     183,   184,   185,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   186,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   188,   189,   190,
     191,   192,   193,   194,   195,   196,   197,     0,   198,   199,
     626,   570,     0,     0,   627,   200,   234,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   171,   172,   173,   174,   175,   176,
     177,   178,   179,     0,     0,   180,   181,     0,     0,     0,
       0,   182,   183,   184,   185,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   186,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   188,
     189,   190,   191,   192,   193,   194,   195,   196,   197,     0,
     198,   199,   650,   563,     0,     0,   651,   200,   234,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   171,   172,   173,   174,
     175,   176,   177,   178,   179,     0,     0,   180,   181,     0,
       0,     0,     0,   182,   183,   184,   185,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   186,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   188,   189,   190,   191,   192,   193,   194,   195,   196,
     197,     0,   198,   199,   653,   570,     0,     0,   654,   200,
     234,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   171,   172,
     173,   174,   175,   176,   177,   178,   179,     0,     0,   180,
     181,     0,     0,     0,     0,   182,   183,   184,   185,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   186,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   188,   189,   190,   191,   192,   193,   194,
     195,   196,   197,     0,   198,   199,   718,   563,     0,     0,
     719,   200,   234,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     171,   172,   173,   174,   175,   176,   177,   178,   179,     0,
       0,   180,   181,     0,     0,     0,     0,   182,   183,   184,
     185,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   186,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,   190,   191,   192,
     193,   194,   195,   196,   197,     0,   198,   199,   721,   570,
       0,     0,   722,   200,   234,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   171,   172,   173,   174,   175,   176,   177,   178,
     179,     0,     0,   180,   181,     0,     0,     0,     0,   182,
     183,   184,   185,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   186,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   188,   189,   190,
     191,   192,   193,   194,   195,   196,   197,     0,   198,   199,
     727,   563,     0,     0,   728,   200,   234,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   171,   172,   173,   174,   175,   176,
     177,   178,   179,     0,     0,   180,   181,     0,     0,     0,
       0,   182,   183,   184,   185,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   186,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   188,
     189,   190,   191,   192,   193,   194,   195,   196,   197,     0,
     198,   199,   569,   570,     0,     0,   571,   200,   234,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   171,   172,   173,   174,
     175,   176,   177,   178,   179,     0,     0,   180,   181,     0,
       0,     0,     0,   182,   183,   184,   185,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   186,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   188,   189,   190,   191,   192,   193,   194,   195,   196,
     197,     0,   198,   199,   776,   563,     0,     0,   777,   200,
     234,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   171,   172,
     173,   174,   175,   176,   177,   178,   179,     0,     0,   180,
     181,     0,     0,     0,     0,   182,   183,   184,   185,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   186,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   188,   189,   190,   191,   192,   193,   194,
     195,   196,   197,     0,   198,   199,   779,   570,     0,     0,
     780,   200,   234,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     171,   172,   173,   174,   175,   176,   177,   178,   179,     0,
       0,   180,   181,     0,     0,     0,     0,   182,   183,   184,
     185,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   186,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,   190,   191,   192,
     193,   194,   195,   196,   197,     0,   198,   199,  1146,   563,
       0,     0,  1147,   200,   234,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   171,   172,   173,   174,   175,   176,   177,   178,
     179,     0,     0,   180,   181,     0,     0,     0,     0,   182,
     183,   184,   185,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   186,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   188,   189,   190,
     191,   192,   193,   194,   195,   196,   197,     0,   198,   199,
    1156,   563,     0,     0,  1157,   200,   234,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   171,   172,   173,   174,   175,   176,
     177,   178,   179,     0,     0,   180,   181,     0,     0,     0,
       0,   182,   183,   184,   185,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   186,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   188,
     189,   190,   191,   192,   193,   194,   195,   196,   197,     0,
     198,   199,  1159,   570,     0,     0,  1160,   200,   234,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   171,   172,   173,   174,
     175,   176,   177,   178,   179,     0,     0,   180,   181,     0,
       0,     0,     0,   182,   183,   184,   185,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   186,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   188,   189,   190,   191,   192,   193,   194,   195,   196,
     197,     0,   198,   199,   569,   570,     0,     0,   571,   200,
     234,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   171,   172,
     173,   174,   175,   176,   177,   178,   179,     0,     0,   180,
     181,     0,     0,     0,     0,   182,   183,   184,   185,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   186,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   188,   189,   190,   191,   192,   193,   194,
     195,   196,   197,     0,   198,   199,     0,     0,     0,     0,
       0,   200
};

static const yytype_int16 yycheck[] =
{
       2,    61,     4,     2,    58,    90,    54,     8,    72,   388,
      22,   103,    95,    28,    60,   228,    27,   102,    29,    93,
       8,   625,    96,    84,   713,   363,   364,    28,   366,   706,
      84,   332,    96,    16,    17,   365,   332,    16,    17,    96,
      28,    68,   370,    97,    98,    99,    16,    17,   652,     4,
      22,    54,    54,    55,   100,   812,    58,    56,    57,    60,
       2,    58,     4,   969,    92,    93,    25,    79,    96,   990,
     604,    69,   434,    13,   412,   986,    72,    56,    27,   272,
     908,    26,    84,   276,   481,  1044,    56,    57,    52,   296,
     297,   429,    13,   431,    25,    97,    98,    99,   100,   100,
     430,    25,   871,    37,    38,   415,   416,    79,   862,   751,
     472,    67,   237,    55,   415,   416,   720,   765,   101,   457,
     960,   128,   101,   877,   878,   102,   730,   145,   525,    67,
      98,   101,    29,     0,    16,    17,    25,    25,   223,   979,
     265,   148,    15,    69,   269,    25,   484,   155,   101,    52,
    1096,   908,    25,   483,   214,     9,    26,    69,   765,    52,
     145,    15,   210,    56,  1042,   121,   101,   252,   426,    56,
      25,    97,    98,   260,   778,   128,   434,    28,   155,   147,
     903,   904,   150,   121,   148,   827,    98,   829,   957,   148,
      13,   150,   726,   128,   765,   148,    25,   245,   262,   284,
     145,    25,  1030,  1031,   153,   262,   155,   849,   210,   237,
     150,   239,   260,   153,   128,   155,  1175,   148,  1164,   101,
     281,   155,   601,   222,   148,  1131,   150,   281,   125,   228,
     443,  1142,   153,  1154,   155,   234,   770,   311,   312,   313,
     314,   243,   329,   245,   243,   148,  1192,  1125,   231,   154,
     233,   234,   231,   280,   233,   593,   258,   260,   260,   148,
     148,   231,   592,   233,    13,   603,   262,   278,   148,  1026,
      13,   633,   602,  1030,  1031,   145,   330,   249,   250,   281,
     150,   329,   679,   311,   312,   313,   314,   821,   316,   317,
     497,   102,   499,   148,   828,   380,  1136,   154,   372,  1177,
      13,   243,  1056,   907,  1027,  1028,    13,   765,   310,  1149,
     374,   436,   154,   315,  1008,   440,   258,   374,  1072,   148,
     445,  1199,  1016,   371,   148,   373,   329,   329,   330,   327,
     153,   979,   155,   330,   332,   983,   461,   148,   986,   102,
     988,   466,    83,    84,   372,   683,  1035,   685,   327,   231,
     153,   233,   155,   332,   684,   310,   963,    67,  1035,   387,
     967,    78,   363,   765,   111,   367,   365,    69,   310,   371,
     154,   373,   979,   315,   632,   633,   983,   687,   688,   986,
     681,   988,   365,   693,   694,   148,   687,   688,   135,   960,
     131,   132,   693,   694,   366,    26,    98,    57,   969,   524,
     148,   150,  1096,  1007,   153,   701,   155,    13,   979,   622,
     153,   121,   155,   123,   124,   986,   126,   415,   416,    13,
     481,   138,   139,   140,    69,   150,  1030,   481,   429,    86,
     155,   430,    13,    13,    67,   148,   451,   812,    69,   128,
     153,    58,   155,   442,   443,    52,   153,   430,   155,    56,
     451,   536,   659,    98,   647,   456,   426,   664,   827,   431,
     829,    78,   464,   451,   525,   114,    97,    98,    99,   101,
    1164,   525,   834,  1167,   148,   101,   133,   101,   788,   481,
     849,   482,    67,   101,   483,   457,   150,   788,   121,   817,
     123,   124,  1169,   110,  1142,    56,  1144,   114,  1192,   469,
     483,  1149,   128,  1151,   128,   963,  1040,    25,  1112,  1113,
     128,   594,   484,  1047,   145,   128,   147,    69,   101,   150,
     102,   979,   586,   525,   148,   983,   905,   902,   986,   146,
     988,   906,   907,   746,   582,  1142,   584,  1144,   123,   124,
     551,   126,  1149,   148,  1151,   128,    98,   153,   208,   155,
      25,   211,   212,   213,  1158,   567,    67,    68,  1206,   153,
    1131,   155,    69,   574,   892,   148,   578,   969,    58,   897,
     972,  1142,   153,   153,   155,   155,   834,   605,    67,   578,
     582,   580,   584,   751,   986,   913,   988,   648,    78,   101,
     591,    98,   101,   592,   648,   567,   150,   765,   610,  1206,
     601,   616,   604,   602,    37,    38,   578,   732,   636,   592,
     128,   610,   123,   124,   625,   616,   128,   101,   679,   602,
     110,   593,   747,   622,   114,   679,   148,    15,   616,    17,
     148,   603,   150,   151,   123,   124,   148,   155,   610,   101,
     150,   652,   641,   735,   128,   150,   648,    25,    78,  1024,
    1029,   725,   101,   128,    67,  1030,  1031,   151,    25,   827,
     724,   829,   632,   128,   148,   154,   128,   327,   640,   641,
     669,    69,   332,   148,   673,   723,   151,   679,   680,   128,
     155,   849,   683,   681,  1142,   684,  1144,   146,   734,   687,
     688,  1149,   864,  1151,   866,   693,   694,   725,   843,   148,
      98,   684,   701,   702,   152,   704,    67,   706,   101,   720,
     123,   124,    52,   685,    54,    55,    56,    57,   766,   730,
      56,   723,   701,   148,   726,  1100,    67,    69,    78,  1131,
      69,  1133,   806,   734,   108,   128,  1138,   148,   680,    67,
    1142,   154,  1144,    26,   155,   101,  1084,   746,  1206,  1151,
     128,   148,   800,  1083,   765,   148,    98,   805,   822,    98,
     121,   128,   123,   124,   766,   425,   426,   778,   770,   153,
     148,    58,   128,   151,   434,   101,    69,   155,   806,    56,
     121,   148,   123,   124,   151,   126,    69,   517,   155,   132,
     788,    78,   960,   121,    25,   123,   124,   796,   800,  1201,
      69,   969,   128,   805,  1206,    98,   951,   952,   148,   469,
     812,   979,   472,   151,    97,    98,    99,   101,   986,   821,
      67,   875,   947,   110,   111,   101,   828,   101,  1024,    98,
     955,     2,   148,     4,  1030,  1031,    52,   891,    54,    55,
      56,    57,    69,   148,   128,    16,    17,   895,   135,   579,
     155,   911,   128,    67,   128,   585,   148,   587,   518,   148,
     871,   860,   145,   862,   147,   101,   155,   150,   101,   148,
      97,    98,    99,   875,   121,   146,   123,   124,   877,   878,
    1005,  1053,  1054,    54,    55,   949,  1058,    58,  1060,   891,
    1062,   146,   128,   895,   942,   128,   907,  1042,    69,  1044,
     902,   903,   904,   148,   906,   907,  1031,   121,   993,   123,
     124,   690,   146,    84,    69,    10,  1041,    44,   697,   148,
     147,    92,    93,   150,   978,    96,    97,    98,    99,   148,
     101,   148,   148,   128,   880,   881,    14,    15,     8,    69,
     942,    13,    97,    98,    99,    25,   957,  1021,  1022,   997,
      17,   999,  1012,    54,    55,   154,    57,   617,    59,   619,
     415,   416,   154,  1131,    65,    66,   146,    97,    98,    99,
     148,    54,   632,   633,  1142,   152,   978,   432,   433,   130,
    1125,  1126,    65,    66,   714,    40,    41,   717,    15,    78,
    1115,  1116,   147,  1021,  1022,   997,  1007,   999,   150,   998,
    1048,   731,   148,   458,    89,    90,  1008,   148,   102,  1181,
    1182,  1183,  1184,   128,  1016,  1063,   676,   147,    52,  1030,
     148,    52,  1024,  1025,   148,  1027,  1028,   148,  1030,  1031,
    1175,   148,  1177,    26,   148,    52,  1035,   133,  1040,   210,
    1212,   701,   128,   148,   148,  1047,  1048,   136,   137,   138,
     139,   140,    52,  1101,  1199,   148,   716,  1056,   148,   133,
     231,  1063,   233,   234,   148,   153,   237,  1078,   239,  1070,
      52,   148,   243,  1072,   245,   148,    69,   807,    56,   133,
     810,  1082,     9,   813,  1083,    69,  1085,   258,  1087,   260,
     820,    56,   102,   823,  1096,    69,   148,   146,  1100,  1101,
    1083,  1112,  1113,    69,    97,    98,    99,    69,   148,    52,
     281,   771,  1084,    97,    98,    99,    40,    41,    42,    43,
      44,  1067,  1068,    97,    98,    99,   148,  1073,   151,  1075,
    1076,    97,    98,    99,   148,    97,    98,    99,   146,   310,
     311,   312,   313,   314,   315,   316,   317,  1158,    69,   148,
     148,   148,   145,   148,   147,   815,   327,   150,   329,   330,
     148,   332,  1164,   147,    52,  1167,    54,    55,    56,    57,
    1169,   148,   148,   147,   834,    69,    97,    98,    99,    69,
     148,   147,    69,   122,    52,   147,    54,    55,    56,    57,
    1192,    69,   148,   148,   365,   243,   367,   150,   858,    69,
     371,   372,   373,    97,    98,    99,   150,    97,    98,    99,
      97,    98,    99,   148,   944,   103,   387,   148,   948,    97,
      98,    99,   148,   812,   464,   148,   147,    97,    98,    99,
     460,    97,   687,   688,    99,    88,   812,   610,   693,   694,
    1186,  1187,  1188,  1189,   415,   416,    52,  1087,    54,    55,
      56,    57,    58,   147,   951,   702,     2,   147,     4,   430,
     147,    60,    61,    62,    63,   765,   673,  1213,  1154,   147,
      16,    17,    78,  1017,   513,   736,   812,   147,   325,  1169,
    1025,   941,   998,  1023,  1026,  1085,   741,   742,  1026,   744,
     745,    69,   106,   464,   499,   100,   950,   103,   963,   959,
     983,   979,   960,   109,   110,   111,    69,    -1,    54,    55,
     481,    -1,   483,   902,   903,   904,    -1,   906,   907,    97,
      98,    99,    -1,    69,    -1,    -1,   902,   903,   904,   135,
     906,   907,   138,   788,    97,    98,    99,    -1,    -1,    52,
      -1,    54,    55,    56,    57,    58,    92,    93,    -1,   155,
      96,    -1,    -1,    -1,   525,   101,    -1,    52,    -1,    54,
      55,    56,    57,    -1,    -1,    78,    -1,    -1,    -1,   147,
      -1,    -1,    52,  1033,    54,    55,    56,    57,   833,    -1,
      -1,    -1,    -1,    -1,   147,    -1,    -1,    -1,    -1,   812,
     103,    -1,  1122,    -1,    -1,    -1,   109,   110,   111,  1129,
      78,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,
      -1,   582,    -1,   584,   109,    -1,    -1,    95,    96,  1008,
      -1,   592,   135,   103,    -1,   138,    -1,  1016,    -1,   109,
      -1,   602,  1008,   604,   605,  1024,  1025,   150,  1027,  1028,
    1016,  1030,  1031,    -1,    -1,    -1,    -1,   812,  1024,  1025,
      -1,  1027,  1028,    -1,  1030,  1031,    -1,   135,   136,   137,
     138,   139,   140,    -1,   210,   636,    -1,    -1,    -1,    -1,
      52,    -1,    54,    55,    56,    57,    58,   648,    -1,   902,
     903,   904,   812,   906,   907,   231,    78,   233,   234,    -1,
      -1,   237,    -1,   239,    -1,    -1,    78,   243,    -1,   245,
      -1,    -1,    -1,    95,    96,    -1,    -1,  1096,   679,   680,
     681,  1100,   258,   684,   260,    -1,   687,   688,    -1,    -1,
    1096,   103,   693,   694,  1100,    -1,    -1,   109,   110,   111,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   902,   903,   904,
      -1,   906,   907,    -1,   136,   137,   138,   139,   140,    -1,
      -1,    -1,   723,   135,   725,   726,   138,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   310,   311,   312,   313,   314,   315,
     316,   317,   902,   903,   904,  1164,   906,   907,  1167,    -1,
      -1,   327,    -1,   329,    -1,  1008,   332,    -1,  1164,    -1,
      -1,  1167,    -1,  1016,    -1,   766,    -1,    -1,    -1,   770,
      -1,  1024,  1025,  1192,  1027,  1028,    -1,  1030,  1031,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1192,   788,    -1,   365,
      -1,   367,    -1,    -1,    -1,   371,   372,   373,    -1,   800,
      -1,    -1,    -1,    -1,   805,   806,    -1,    -1,    -1,    -1,
      -1,   387,    -1,  1008,    -1,    -1,    -1,    -1,    -1,    -1,
     821,  1016,    -1,    -1,    78,    -1,    -1,   828,    -1,  1024,
    1025,    -1,  1027,  1028,    -1,  1030,  1031,   812,    -1,   415,
     416,    95,    96,  1096,    -1,    -1,    -1,  1100,  1008,    -1,
      -1,    -1,    -1,    -1,   430,    -1,  1016,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1024,  1025,    -1,  1027,  1028,    -1,
    1030,  1031,    -1,    -1,   875,    -1,    -1,    -1,    -1,   133,
     134,   135,   136,   137,   138,   139,   140,    -1,   464,    -1,
     891,    -1,    -1,    -1,   895,     2,    -1,     4,    -1,    -1,
      -1,  1096,    -1,    -1,    -1,  1100,    -1,   483,    -1,    16,
      17,  1164,    -1,    -1,  1167,    -1,    52,    -1,    54,    55,
      56,    57,    58,    -1,    -1,    -1,    -1,   902,   903,   904,
      -1,   906,   907,    -1,    -1,    -1,  1096,    -1,    -1,  1192,
    1100,   942,    78,    -1,    -1,    -1,    -1,    54,    55,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    69,    -1,    -1,    -1,    -1,   103,    -1,  1164,
      -1,    -1,  1167,    -1,   110,   111,    -1,   978,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    92,    93,    -1,    -1,    96,
      -1,    -1,    -1,    -1,   101,    -1,   997,  1192,   999,   135,
      -1,    -1,    -1,    -1,  1164,    -1,   582,  1167,   584,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   592,    -1,    -1,    -1,
    1021,  1022,    -1,    -1,    -1,    -1,   602,    -1,   604,   605,
      -1,    -1,  1192,  1008,    -1,    -1,    -1,    -1,    -1,  1040,
      -1,  1016,    -1,    -1,    -1,    -1,  1047,  1048,    -1,  1024,
    1025,    -1,  1027,  1028,    -1,  1030,  1031,    -1,    -1,    -1,
     636,    -1,  1063,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   812,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1083,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       2,    -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1101,    -1,    -1,   210,   680,   681,    -1,    -1,   684,    -1,
      -1,   687,   688,    -1,    -1,    -1,    -1,   693,   694,    -1,
      -1,  1096,    -1,    -1,   231,  1100,   233,   234,    -1,    -1,
     237,    -1,   239,    -1,    -1,    -1,   243,    -1,   245,    -1,
      -1,    -1,    54,    55,    -1,    -1,    58,   723,    -1,   725,
     726,   258,    -1,   260,    -1,    -1,    -1,    -1,    -1,   812,
      -1,    -1,   902,   903,   904,    -1,   906,   907,    -1,    -1,
      -1,    -1,    84,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    97,    98,    99,    -1,  1164,
     766,    -1,  1167,    -1,   770,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   310,   311,   312,   313,   314,   315,   316,
     317,    -1,   788,    -1,    -1,    -1,    -1,  1192,    -1,    -1,
     327,    -1,   329,    -1,   800,   332,    -1,    -1,    -1,   805,
     806,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   821,    -1,    -1,    -1,   902,
     903,   904,   828,   906,   907,    -1,    -1,    -1,   365,    -1,
     367,    -1,    -1,    -1,   371,   372,   373,    -1,  1008,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1016,    -1,    -1,    -1,
     387,    -1,    -1,    -1,  1024,  1025,    -1,  1027,  1028,    -1,
    1030,  1031,    -1,    -1,    -1,    -1,    -1,    -1,   210,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   415,   416,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   895,
      -1,    -1,    -1,   430,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   243,    -1,   245,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    44,   258,     2,   260,     4,
      -1,    -1,    -1,    -1,    -1,  1008,  1096,   464,    -1,    -1,
    1100,    -1,    -1,  1016,    -1,    -1,   942,    -1,    -1,   281,
      -1,  1024,  1025,    -1,  1027,  1028,   483,  1030,  1031,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    -1,    -1,    -1,    -1,    95,    96,   310,    54,
      55,    -1,    -1,   315,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   329,   330,    -1,
     332,   997,    -1,   999,  1164,    -1,    -1,  1167,    -1,    -1,
     129,    -1,   131,   132,   133,   134,   135,   136,   137,   138,
     139,   140,    97,  1096,    -1,  1021,  1022,  1100,    -1,   148,
      -1,    -1,  1192,    -1,    -1,   367,    -1,    -1,    -1,   371,
      -1,   373,    -1,    -1,  1040,    -1,    -1,    -1,    -1,    -1,
      -1,  1047,  1048,    -1,    -1,   582,    -1,   584,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   592,    -1,  1063,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   602,    -1,   604,   605,    -1,
      -1,    -1,    -1,   415,   416,    -1,    -1,  1083,    -1,    -1,
      -1,  1164,    -1,    -1,  1167,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1101,    -1,    -1,    -1,   636,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1192,
      -1,    -1,     2,    -1,     4,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   464,    -1,    -1,   210,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   481,
      -1,    -1,    -1,   680,   681,    -1,    -1,   684,    -1,    -1,
     687,   688,    -1,    -1,    -1,    -1,   693,   694,   243,    -1,
     245,    -1,    -1,    -1,    54,    55,    -1,    -1,    58,    -1,
      -1,    -1,    -1,   258,    -1,   260,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   525,    -1,    -1,   723,    -1,   725,   726,
      -1,    -1,    -1,    -1,    84,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    97,    98,    99,
     100,    -1,    -1,    -1,    -1,    -1,    -1,    78,    79,    80,
      81,    82,    83,    84,    85,   310,    87,    88,    -1,   766,
     315,    -1,    -1,   770,    95,    96,    -1,    -1,    -1,    -1,
     582,    -1,   584,    -1,   329,    -1,    -1,   332,    -1,    -1,
      -1,   788,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   604,   800,    -1,    -1,    -1,    -1,   805,   806,
     131,   132,   133,   134,   135,   136,   137,   138,   139,   140,
      -1,    -1,   367,    -1,   821,    -1,   371,    -1,   373,    -1,
      -1,   828,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   648,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   657,    -1,    -1,    -1,    -1,
     210,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     415,   416,    -1,    -1,    -1,    -1,    -1,   679,   680,   681,
      -1,    -1,    -1,    -1,    -1,   687,   688,    -1,    -1,    -1,
      -1,   693,   694,   243,    -1,   245,    -1,    -1,   895,    -1,
      -1,    -1,    -1,     2,    -1,     4,    -1,    -1,   258,    -1,
     260,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   464,
      -1,   723,    -1,    -1,   726,    -1,    -1,    -1,    -1,    -1,
      -1,   281,    -1,    -1,    78,    79,    80,    81,    82,    83,
      84,    -1,    -1,    87,    88,   942,    -1,    -1,    -1,    -1,
      -1,    95,    96,    -1,    -1,    54,    55,    -1,    -1,    -1,
     310,    -1,    -1,    -1,   766,   315,    -1,    -1,   770,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   329,
     330,    -1,    -1,    -1,    -1,    -1,   788,   131,   132,   133,
     134,   135,   136,   137,   138,   139,   140,    -1,   800,    -1,
     997,    -1,   999,   805,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   367,    -1,   821,
      -1,   371,    -1,   373,  1021,  1022,   828,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   582,    -1,   584,
      -1,    -1,    -1,  1040,    -1,    -1,    -1,    -1,    -1,    -1,
    1047,  1048,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   604,
      -1,    -1,    -1,    -1,    -1,    -1,  1063,    -1,    -1,    -1,
      -1,    -1,    -1,   875,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1083,    -1,    -1,   891,
      -1,    -1,    -1,   895,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1101,    -1,    -1,    -1,    -1,    -1,
      -1,   210,    -1,    -1,   464,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   481,    -1,    -1,    -1,   680,   681,    -1,    -1,    -1,
     942,    -1,   687,   688,   243,    -1,   245,    -1,   693,   694,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   258,
      -1,   260,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   525,   978,    -1,   723,    -1,
      -1,   726,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   997,    -1,   999,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   310,    -1,    -1,    -1,    -1,   315,    -1,    -1,    -1,
      -1,   766,    -1,    -1,    -1,   770,    -1,    -1,    -1,    -1,
     329,    -1,   582,   332,   584,    -1,    -1,    -1,  1040,    -1,
      -1,    -1,    -1,   788,    -1,  1047,  1048,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   604,   800,    -1,    -1,    -1,    -1,
     805,  1063,    -1,    -1,    -1,    -1,    -1,    -1,   367,    -1,
      -1,    -1,   371,    -1,   373,    -1,   821,    33,    34,    35,
      36,    -1,    -1,   828,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    49,    50,    51,    -1,    -1,   648,  1101,
      -1,    -1,    -1,    -1,    60,    61,    62,    63,    64,    -1,
      -1,     0,    -1,    -1,    -1,    -1,   415,   416,    -1,     8,
       9,    10,    -1,    -1,    13,    14,    15,    -1,    17,   679,
     680,    -1,    -1,    -1,    -1,    -1,    25,    26,    27,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   891,    -1,    37,    38,
     895,    40,    41,    42,    43,    44,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   464,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   723,    -1,    -1,   726,    -1,    -1,    -1,
      69,    -1,    -1,    -1,    -1,   141,    78,    79,    80,    81,
      82,    83,    84,    -1,    -1,    87,    88,   942,    -1,    -1,
      -1,    -1,    -1,    95,    96,    -1,    -1,    -1,    97,    98,
      -1,    -1,    -1,    -1,    -1,    -1,   766,    -1,    -1,    -1,
     770,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   122,    -1,    -1,    -1,    -1,    -1,   131,
     132,   133,   134,   135,   136,   137,   138,   139,   140,    -1,
     800,    -1,   997,    -1,   999,   805,   145,   146,    -1,    -1,
      -1,   150,   151,    -1,   153,    -1,   155,    -1,    -1,    -1,
      -1,   821,    -1,    -1,    -1,    -1,    -1,    -1,   828,    -1,
      -1,    -1,    -1,   582,    -1,   584,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1040,    -1,    -1,    -1,    -1,
      -1,    -1,  1047,  1048,    -1,   604,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1063,    -1,
      -1,    -1,    -1,    -1,    -1,   875,    -1,    -1,    -1,    -1,
      -1,    -1,    33,    34,    35,    36,    -1,    -1,    -1,    -1,
      -1,   891,    -1,    -1,    -1,   895,    -1,    -1,    49,    50,
      51,    52,    -1,    -1,    -1,    56,  1101,    58,    -1,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    78,    -1,    -1,
      -1,   680,   681,    -1,    -1,    -1,    -1,    -1,   687,   688,
      91,    92,   942,    -1,   693,   694,    -1,    -1,    -1,   100,
      -1,    -1,   103,    -1,    -1,   106,   107,    -1,   109,   110,
      -1,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,    -1,    -1,    -1,   723,    -1,    -1,   726,   978,    -1,
      -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    16,    17,    -1,    -1,    -1,    -1,   997,    -1,   999,
      -1,    -1,    -1,    -1,   155,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   766,    -1,    -1,
      -1,   770,    -1,    -1,    49,    50,    51,    52,    -1,    -1,
      -1,    56,    57,    -1,    -1,    -1,    -1,    -1,    -1,   788,
    1040,    -1,    -1,    -1,    69,    70,    -1,  1047,  1048,    -1,
      -1,   800,    -1,    -1,    -1,    -1,   805,    -1,    -1,    -1,
      -1,    -1,    -1,  1063,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   821,    -1,    -1,    -1,   101,    -1,    -1,   828,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1101,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     0,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     8,     9,    10,    -1,   895,    13,    14,    15,
      -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,
      26,    27,    28,    29,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    37,    38,    -1,    40,    41,    42,    43,    44,    -1,
      -1,    -1,    -1,   208,    -1,    -1,   211,   212,   213,    -1,
     215,    -1,    -1,   942,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    69,    -1,    -1,   231,    -1,   233,   234,
      -1,    -1,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    -1,    -1,    -1,    -1,    95,
      96,    97,    98,    99,    -1,   101,   102,    -1,    -1,    -1,
      -1,    -1,   108,    -1,    -1,    -1,    -1,    -1,   997,    -1,
     999,    -1,    -1,    -1,    -1,    -1,   122,    -1,    -1,   125,
      -1,    -1,   128,   129,   130,   131,   132,   133,   134,   135,
     136,   137,   138,   139,   140,    -1,    -1,    -1,    -1,   145,
     146,   147,   148,    -1,    -1,   151,   152,   153,    -1,   155,
      -1,  1040,    -1,    -1,    -1,    -1,    -1,    -1,  1047,  1048,
      -1,    -1,   327,    -1,    -1,    -1,    -1,   332,   333,   334,
     335,   336,   337,    -1,  1063,   340,   341,   342,   343,   344,
     345,   346,   347,   348,    -1,    -1,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,    -1,    -1,    -1,    -1,
     365,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1101,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     415,   416,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   424,
     425,   426,    -1,    -1,    -1,   430,    -1,   432,   433,   434,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     0,     1,
      -1,     3,     4,     5,     6,     7,    -1,    -1,   453,    11,
      12,    -1,    -1,   458,    16,    -1,    18,    19,    20,    21,
      22,    23,    24,    -1,   469,    -1,    -1,   472,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,   483,    -1,
      -1,    -1,    -1,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    57,   501,    59,    60,    61,
      62,    63,    64,    65,    66,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   518,    76,    77,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,
      92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,
      -1,   103,   104,    -1,   106,   107,    -1,   109,    -1,    -1,
     112,   113,   114,   115,   116,   117,   118,   119,   120,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,
     142,   143,    -1,    -1,    -1,    -1,    -1,   592,    -1,    -1,
      -1,   153,    -1,   155,    -1,    -1,    -1,   602,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   617,    -1,   619,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   632,   633,    -1,
      -1,    -1,    -1,    33,    34,    35,    36,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,
      50,    51,    52,    -1,    -1,    -1,    56,    -1,    -1,    -1,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,   676,    -1,    -1,    -1,    -1,   681,   682,    -1,   684,
      -1,    -1,   687,   688,    -1,    -1,    -1,    -1,   693,   694,
      -1,    91,    92,    -1,    -1,    -1,   701,    -1,    -1,    -1,
     100,    -1,    -1,   103,    -1,    -1,   106,   107,    -1,   109,
      -1,   716,   112,   113,   114,   115,   116,   117,   118,   119,
     120,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   134,    -1,   741,   742,    -1,   744,
     745,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   771,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   781,     0,    -1,    -1,
      -1,    -1,    -1,   788,    -1,     8,     9,    10,    -1,    -1,
      13,    14,    15,    -1,    17,    -1,    -1,    -1,    -1,    44,
      -1,    -1,    25,    -1,    27,    28,    29,    -1,    -1,    -1,
     815,    -1,    -1,    -1,    37,    38,    -1,    40,    41,    42,
      43,    44,    -1,    -1,    -1,    -1,    -1,    -1,   833,   834,
      -1,    -1,    -1,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    69,    -1,    -1,    -1,
      95,    96,    -1,   858,    -1,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    -1,    -1,
      -1,    -1,    95,    96,    97,    98,    99,    -1,   101,   102,
      -1,    -1,    -1,    -1,   129,   108,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,    -1,    -1,    -1,   122,
      -1,    -1,   125,    -1,    -1,   128,   129,   130,   131,   132,
     133,   134,   135,   136,   137,   138,   139,   140,    -1,    -1,
      -1,    -1,    -1,   146,   147,   148,    -1,    -1,   151,   152,
     153,    -1,   155,    -1,     0,    -1,   941,    -1,    -1,    -1,
      -1,    -1,     8,     9,    10,    -1,    -1,    13,    14,    15,
      -1,    17,    -1,    -1,   959,    -1,    -1,    -1,    -1,    25,
      -1,    27,    28,    29,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    37,    38,    -1,    40,    41,    42,    43,    44,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    69,    -1,    -1,    -1,    95,    96,    -1,
      -1,    -1,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    -1,    -1,    -1,  1033,    95,
      96,    97,    98,    99,    -1,   101,   102,    -1,    -1,    -1,
      -1,   129,   108,   131,   132,   133,   134,   135,   136,   137,
     138,   139,   140,    -1,    -1,    -1,   122,    -1,    -1,   125,
      -1,    -1,   128,   129,   130,   131,   132,   133,   134,   135,
     136,   137,   138,   139,   140,    -1,    -1,    -1,  1083,    -1,
     146,   147,   148,     0,    -1,   151,   152,   153,    -1,   155,
      -1,     8,     9,    10,    -1,    -1,    13,    14,    15,    -1,
      17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,    26,
      27,    28,    29,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      37,    38,    -1,    40,    41,    42,    43,    44,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    69,    -1,    -1,    -1,    95,    96,    -1,    -1,
      -1,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    -1,    -1,    -1,    -1,    95,    96,
      97,    98,    99,    -1,    -1,   102,    -1,    -1,    -1,    -1,
      -1,   108,   131,   132,   133,   134,   135,   136,   137,   138,
     139,   140,    -1,    -1,    -1,   122,    -1,    -1,   125,    -1,
      -1,    -1,   129,   130,   131,   132,   133,   134,   135,   136,
     137,   138,   139,   140,    -1,    -1,    -1,    -1,   145,   146,
     147,   148,     0,    -1,   151,   152,   153,    -1,   155,    -1,
       8,     9,    10,    -1,    -1,    13,    14,    15,    -1,    17,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,    26,    27,
      28,    29,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,
      38,    -1,    40,    41,    42,    43,    44,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    -1,    -1,    -1,    -1,    95,    96,    97,
      98,    99,    -1,    -1,   102,    -1,    -1,    -1,    -1,    -1,
     108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   122,    -1,    -1,   125,    -1,    -1,
      -1,   129,   130,   131,   132,   133,   134,   135,   136,   137,
     138,   139,   140,    -1,    -1,    -1,    -1,   145,   146,   147,
     148,     0,    -1,   151,   152,   153,    -1,   155,    -1,     8,
       9,    10,    -1,    -1,    13,    14,    15,    -1,    17,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    25,    -1,    27,    28,
      29,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,
      -1,    40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    -1,    -1,    -1,    -1,    95,    96,    97,    98,
      99,    -1,   101,   102,    -1,    -1,    -1,    -1,    -1,   108,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   122,    -1,    -1,   125,    -1,    -1,   128,
     129,   130,   131,   132,   133,   134,   135,   136,   137,   138,
     139,   140,    -1,    -1,    -1,    -1,    -1,   146,   147,   148,
       0,    -1,   151,   152,   153,    -1,   155,    -1,     8,     9,
      10,    -1,    -1,    13,    14,    15,    -1,    17,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    25,    26,    27,    28,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,
      40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    69,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    -1,    -1,    -1,    -1,    95,    96,    97,    98,    99,
      -1,    -1,   102,    -1,    -1,    -1,    -1,    -1,   108,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   122,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,   131,   132,   133,   134,   135,   136,   137,   138,   139,
     140,    -1,    -1,    -1,    -1,   145,   146,   147,   148,     0,
     150,   151,   152,   153,    -1,   155,    -1,     8,     9,    10,
      -1,    -1,    13,    14,    15,    -1,    17,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    25,    -1,    27,    28,    29,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,
      41,    42,    43,    44,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    69,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      -1,    -1,    -1,    -1,    95,    96,    97,    98,    99,    -1,
      -1,   102,    -1,    -1,    -1,    -1,    -1,   108,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   122,    -1,    -1,   125,    -1,    -1,    -1,   129,   130,
     131,   132,   133,   134,   135,   136,   137,   138,   139,   140,
      -1,    -1,    -1,    -1,    -1,   146,   147,   148,     0,    -1,
     151,   152,   153,    -1,   155,    -1,     8,     9,    10,    -1,
      -1,    13,    14,    15,    -1,    17,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    25,    26,    27,    28,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    69,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    -1,
      -1,    -1,    -1,    95,    96,    97,    98,    99,    -1,    -1,
     102,    -1,    -1,    -1,    -1,    -1,   108,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     122,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,   131,
     132,   133,   134,   135,   136,   137,   138,   139,   140,    -1,
      -1,    -1,    -1,   145,   146,   147,   148,     0,   150,   151,
     152,   153,    -1,   155,    -1,     8,     9,    10,    -1,    -1,
      13,    14,    15,    -1,    17,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    25,    -1,    27,    28,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,    42,
      43,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    69,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    -1,    -1,
      -1,    -1,    95,    96,    97,    98,    99,    -1,    -1,   102,
      -1,    -1,    -1,    -1,    -1,   108,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,   131,   132,
     133,   134,   135,   136,   137,   138,   139,   140,    -1,    -1,
      -1,    -1,    -1,   146,   147,   148,     0,   150,   151,   152,
     153,    -1,   155,    -1,     8,     9,    10,    -1,    -1,    -1,
      14,    15,    -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    25,    26,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    37,    38,    -1,    40,    41,    42,    43,
      44,    -1,    -1,    -1,    -1,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    -1,    -1,
      -1,    -1,    95,    96,    -1,    69,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    -1,    -1,    -1,
      -1,    95,    96,    97,    98,    99,   129,   101,   131,   132,
     133,   134,   135,   136,   137,   138,   139,   140,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,    -1,
      -1,    -1,   155,    -1,   128,   129,    -1,   131,   132,   133,
     134,   135,   136,   137,   138,   139,   140,    -1,    -1,    -1,
      -1,   145,   146,   147,   148,     0,    -1,   151,    -1,   153,
      -1,   155,    -1,     8,     9,    10,    -1,    -1,    -1,    14,
      15,    -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      25,    26,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    37,    38,    -1,    40,    41,    42,    43,    44,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    69,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    -1,    -1,    -1,    -1,
      95,    96,    97,    98,    99,    -1,   101,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,    -1,    -1,
      -1,    -1,    -1,   128,   129,    -1,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,    -1,    -1,    -1,    -1,
     145,   146,   147,   148,     0,    -1,   151,    -1,   153,    -1,
     155,    -1,     8,     9,    10,    -1,    -1,    -1,    14,    15,
      -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    37,    38,    -1,    40,    41,    42,    43,    44,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    69,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    -1,    -1,    -1,    -1,    95,
      96,    97,    98,    99,    -1,   101,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   122,    -1,    -1,    -1,
      -1,    -1,   128,   129,    -1,   131,   132,   133,   134,   135,
     136,   137,   138,   139,   140,    -1,    -1,    -1,    -1,    -1,
     146,   147,   148,     0,    -1,   151,    -1,   153,    -1,   155,
      -1,     8,     9,    10,    -1,    -1,    -1,    14,    15,    -1,
      17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      37,    38,    -1,    40,    41,    42,    43,    44,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    -1,    -1,    -1,    -1,    95,    96,
      97,    98,    99,    -1,   101,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   122,    -1,    -1,    -1,    -1,
      -1,   128,   129,    -1,   131,   132,   133,   134,   135,   136,
     137,   138,   139,   140,    -1,    -1,    -1,    -1,    -1,   146,
     147,   148,    -1,    -1,   151,    -1,   153,     1,   155,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    -1,
      -1,    15,    16,    -1,    18,    19,    20,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    -1,    59,    60,    61,    62,    63,
      64,    65,    66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,    92,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,   103,
     104,    -1,   106,   107,    -1,   109,    -1,    -1,   112,   113,
     114,   115,   116,   117,   118,   119,   120,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,   142,   143,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   153,
       1,   155,     3,     4,     5,     6,     7,    -1,    -1,    10,
      11,    12,    -1,    14,    15,    16,    -1,    18,    19,    20,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    -1,    59,    60,
      61,    62,    63,    64,    65,    66,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    76,    77,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,
      -1,    -1,   103,   104,    -1,   106,   107,    -1,   109,    -1,
      -1,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     141,   142,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   153,     1,   155,     3,     4,     5,     6,     7,
      -1,    -1,    10,    11,    12,    -1,    -1,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    59,    60,    61,    62,    63,    64,    65,    66,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,    77,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    91,    92,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   100,    -1,    -1,   103,   104,    -1,   106,   107,
      -1,   109,    -1,    -1,   112,   113,   114,   115,   116,   117,
     118,   119,   120,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   141,   142,   143,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   153,     1,   155,     3,     4,
       5,     6,     7,    -1,    -1,    10,    11,    12,    -1,    -1,
      15,    16,    -1,    18,    19,    20,    21,    22,    23,    24,
      25,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    59,    60,    61,    62,    63,    64,
      65,    66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    91,    92,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,   103,   104,
      -1,   106,   107,    -1,   109,    -1,    -1,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   141,   142,   143,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   153,     1,
     155,     3,     4,     5,     6,     7,    -1,    -1,    10,    11,
      12,    -1,    -1,    15,    16,    -1,    18,    19,    20,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    -1,    59,    60,    61,
      62,    63,    64,    65,    66,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    76,    77,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,
      92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,
      -1,   103,   104,    -1,   106,   107,    -1,   109,    -1,    -1,
     112,   113,   114,   115,   116,   117,   118,   119,   120,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,     3,
       4,     5,     6,     7,    -1,     9,    10,    11,    12,   141,
     142,   143,    16,    -1,    18,    19,    20,    21,    22,    23,
      24,   153,    -1,   155,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    -1,    59,    60,    61,    62,    63,
      64,    65,    66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,    92,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,   103,
     104,    -1,   106,   107,    -1,   109,    -1,    -1,   112,   113,
     114,   115,   116,   117,   118,   119,   120,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     1,    -1,     3,     4,     5,
       6,     7,    -1,    -1,    -1,    11,    12,   141,   142,   143,
      16,    -1,    18,    19,    20,    21,    22,    23,    24,   153,
      -1,   155,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    -1,    59,    60,    61,    62,    63,    64,    65,
      66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      76,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    91,    92,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   100,    -1,    -1,   103,   104,    -1,
     106,   107,    -1,   109,    -1,    -1,   112,   113,   114,   115,
     116,   117,   118,   119,   120,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   141,   142,   143,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   151,    -1,   153,     1,   155,
       3,     4,     5,     6,     7,    -1,    -1,    -1,    11,    12,
      -1,    -1,    -1,    16,    -1,    18,    19,    20,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    -1,    59,    60,    61,    62,
      63,    64,    65,    66,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    76,    77,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,    92,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,
     103,   104,    -1,   106,   107,    -1,   109,    -1,    -1,   112,
     113,   114,   115,   116,   117,   118,   119,   120,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,   142,
     143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,    -1,
     153,     1,   155,     3,     4,     5,     6,     7,    -1,    -1,
      -1,    11,    12,    -1,    -1,    -1,    16,    -1,    18,    19,
      20,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    -1,    59,
      60,    61,    62,    63,    64,    65,    66,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    76,    77,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     100,    -1,    -1,   103,   104,    -1,   106,   107,    -1,   109,
      -1,    -1,   112,   113,   114,   115,   116,   117,   118,   119,
     120,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   141,   142,   143,    -1,    -1,   146,    -1,    -1,    -1,
      -1,    -1,    -1,   153,     1,   155,     3,     4,     5,     6,
       7,    -1,    -1,    -1,    11,    12,    -1,    -1,    -1,    16,
      -1,    18,    19,    20,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    -1,    59,    60,    61,    62,    63,    64,    65,    66,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,
      77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    91,    92,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   100,    -1,    -1,   103,   104,    -1,   106,
     107,    -1,   109,    -1,    -1,   112,   113,   114,   115,   116,
     117,   118,   119,   120,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   141,   142,   143,    -1,    -1,   146,
      -1,    -1,    -1,    -1,    -1,    -1,   153,     1,   155,     3,
       4,     5,     6,     7,    -1,    -1,    10,    11,    12,    -1,
      -1,    -1,    16,    -1,    18,    19,    20,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    -1,    59,    60,    61,    62,    63,
      64,    65,    66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,    92,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,   103,
     104,    -1,   106,   107,    -1,   109,    -1,    -1,   112,   113,
     114,   115,   116,   117,   118,   119,   120,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     1,    -1,     3,     4,     5,
       6,     7,    -1,    -1,    -1,    11,    12,   141,   142,   143,
      16,    -1,    18,    19,    20,    21,    22,    23,    24,   153,
      -1,   155,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    -1,    59,    60,    61,    62,    63,    64,    65,
      66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      76,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    91,    92,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   100,    -1,    -1,   103,   104,    -1,
     106,   107,    -1,   109,    -1,    -1,   112,   113,   114,   115,
     116,   117,   118,   119,   120,    -1,   122,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,    -1,     7,
      -1,    -1,    -1,    11,    12,   141,   142,   143,    16,    -1,
      18,    19,    20,    21,    22,    23,    24,   153,    -1,   155,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,
      -1,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,    77,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    91,    92,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   100,    -1,    -1,   103,   104,    -1,   106,   107,
      -1,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,    -1,     7,    -1,    -1,
      -1,    11,    12,   141,   142,   143,    16,    -1,    18,    19,
      20,    21,    22,    23,    24,    -1,    -1,   155,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    -1,    59,
      60,    61,    62,    63,    64,    65,    66,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    76,    77,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     100,    -1,    -1,   103,   104,    -1,   106,   107,    -1,    -1,
      -1,    -1,   112,   113,   114,   115,   116,   117,   118,   119,
     120,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,    11,
      12,   141,   142,   143,    16,    -1,    18,    19,    20,    21,
      22,    23,    24,   153,    -1,   155,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    -1,    59,    60,    61,
      62,    63,    64,    65,    66,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    76,    77,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,
      92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,
      -1,   103,   104,    -1,   106,   107,    -1,    -1,    -1,    -1,
     112,   113,   114,   115,   116,   117,   118,   119,   120,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,     7,    -1,    -1,    -1,    11,    12,   141,
     142,   143,    16,    -1,    18,    19,    20,    21,    22,    23,
      24,    -1,    -1,   155,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    -1,    59,    60,    61,    62,    63,
      64,    65,    66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,    92,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,   103,
     104,    -1,   106,   107,    -1,   109,    -1,    -1,   112,   113,
     114,   115,   116,   117,   118,   119,   120,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,
       6,     7,    -1,    -1,    -1,    11,    12,   141,   142,   143,
      16,    -1,    18,    19,    20,    21,    22,    23,    24,   153,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    -1,    59,    60,    61,    62,    63,    64,    65,
      66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      76,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    91,    92,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   100,    -1,    -1,   103,   104,    -1,
     106,   107,    -1,   109,    -1,    -1,   112,   113,   114,   115,
     116,   117,   118,   119,   120,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   141,   142,   143,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   153,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    -1,    -1,    -1,    -1,    -1,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    59,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      -1,    -1,    87,    88,    -1,    -1,    -1,    -1,    93,    94,
      95,    96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,    -1,   142,   143,    -1,
      -1,    -1,    -1,    -1,   149,   150,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    -1,    -1,
      87,    88,    -1,    -1,    -1,    -1,    93,    94,    95,    96,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   131,   132,   133,   134,   135,   136,
     137,   138,   139,   140,    -1,   142,   143,    -1,    -1,    -1,
      -1,    -1,   149,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    -1,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    -1,    -1,    87,    88,    -1,
      -1,    -1,    -1,    93,    94,    95,    96,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,
     110,    -1,    -1,   113,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   131,   132,   133,   134,   135,   136,   137,   138,   139,
     140,    -1,   142,   143,    -1,    -1,    -1,    -1,    -1,   149,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    -1,    -1,    -1,
      -1,    -1,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    -1,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    -1,    -1,    87,    88,    -1,    -1,    -1,    -1,
      93,    94,    95,    96,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   109,   110,    -1,    -1,
     113,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,   132,
     133,   134,   135,   136,   137,   138,   139,   140,    -1,   142,
     143,    -1,    -1,    -1,    -1,    -1,   149,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    -1,    -1,    -1,    -1,    -1,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    -1,    -1,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    -1,
      -1,    87,    88,    -1,    -1,    -1,    -1,    93,    94,    95,
      96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   131,   132,   133,   134,   135,
     136,   137,   138,   139,   140,    -1,   142,   143,     3,     4,
       5,    -1,     7,   149,    -1,    -1,    11,    12,    -1,    -1,
      -1,    16,    -1,    18,    19,    20,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    46,    -1,    -1,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    59,    60,    61,    62,    63,    64,
      65,    66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,   103,   104,
      -1,   106,   107,    -1,    -1,    -1,    -1,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,    -1,     7,    -1,    -1,   141,    11,    12,    -1,
      -1,    -1,    16,   148,    18,    19,    20,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    46,    -1,    -1,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    -1,    59,    60,    61,    62,    63,
      64,    65,    66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,   103,
     104,    -1,   106,   107,    -1,    -1,    -1,    -1,   112,   113,
     114,   115,   116,   117,   118,   119,   120,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,     6,     7,    -1,    -1,   141,    11,    12,
      -1,    -1,    -1,    16,   148,    18,    19,    20,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    -1,    59,    60,    61,    62,
      63,    64,    65,    66,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    76,    77,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,    92,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,
     103,   104,    -1,   106,   107,    -1,   109,    -1,    -1,   112,
     113,   114,   115,   116,   117,   118,   119,   120,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,    -1,     7,    -1,    -1,    -1,    11,    12,   141,   142,
     143,    16,    -1,    18,    19,    20,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    46,    -1,    -1,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    91,    92,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,   103,   104,
      -1,   106,   107,    -1,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,    -1,    -1,    -1,    11,    12,   141,   142,   143,    16,
      -1,    18,    19,    20,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    -1,    59,    60,    61,    62,    63,    64,    65,    66,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,
      77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    91,    92,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   100,    -1,    -1,   103,   104,    -1,   106,
     107,    -1,   109,    -1,    -1,   112,   113,   114,   115,   116,
     117,   118,   119,   120,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,    -1,     7,    -1,
      -1,    -1,    11,    12,   141,   142,   143,    16,    -1,    18,
      19,    20,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,    77,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   100,    -1,    -1,   103,   104,    -1,   106,   107,    -1,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,
      11,    12,   141,   142,   143,    16,    -1,    18,    19,    20,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    76,    77,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,
      -1,    -1,   103,   104,    -1,   106,   107,    -1,   109,   110,
      -1,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,    -1,     7,    -1,    -1,    -1,    11,    12,
     141,   142,   143,    16,    -1,    18,    19,    20,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    46,    -1,    -1,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    76,    77,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,    92,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,
     103,   104,    -1,   106,   107,    -1,    -1,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,    -1,     7,    -1,    -1,    -1,    11,    12,   141,   142,
     143,    16,    -1,    18,    19,    20,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    46,    -1,    -1,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    91,    92,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,   103,   104,
      -1,   106,   107,    -1,   109,   110,    -1,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,    -1,
       7,    -1,    -1,    -1,    11,    12,   141,   142,   143,    16,
      -1,    18,    19,    20,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    46,
      -1,    -1,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,
      77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    91,    92,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   100,    -1,    -1,   103,   104,    -1,   106,
     107,    -1,    -1,   110,    -1,   112,   113,   114,   115,   116,
     117,   118,   119,   120,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,    -1,     7,    -1,
      -1,    -1,    11,    12,   141,   142,   143,    16,    -1,    18,
      19,    20,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    -1,
      59,    60,    61,    62,    63,    64,    65,    66,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,    77,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   100,    -1,    -1,   103,   104,    -1,   106,   107,    -1,
     109,    -1,    -1,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,
      11,    12,   141,   142,   143,    16,    -1,    18,    19,    20,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    -1,    59,    60,
      61,    62,    63,    64,    65,    66,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    76,    77,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,
      -1,    -1,   103,   104,    -1,   106,   107,    -1,   109,    -1,
      -1,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,    -1,     7,    -1,    -1,    -1,    11,    12,
     141,   142,   143,    16,    -1,    18,    19,    20,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    46,    -1,    -1,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    -1,    59,    60,    61,    62,
      63,    64,    65,    66,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    76,    77,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,    92,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,
     103,   104,    -1,   106,   107,    -1,   109,    -1,    -1,   112,
     113,   114,   115,   116,   117,   118,   119,   120,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,    -1,     7,    -1,    -1,    -1,    11,    12,   141,   142,
     143,    16,    -1,    18,    19,    20,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    46,    -1,    -1,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    59,    60,    61,    62,    63,    64,
      65,    66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    91,    92,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,   103,   104,
      -1,   106,   107,    -1,   109,    -1,    -1,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,    -1,
       7,    -1,    -1,    -1,    11,    12,   141,   142,   143,    16,
      -1,    18,    19,    20,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    46,
      -1,    -1,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    -1,    59,    60,    61,    62,    63,    64,    65,    66,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,
      77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    91,    92,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   100,    -1,    -1,   103,   104,    -1,   106,
     107,    -1,   109,    -1,    -1,   112,   113,   114,   115,   116,
     117,   118,   119,   120,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,    -1,     7,    -1,
      -1,    -1,    11,    12,   141,   142,   143,    16,    -1,    18,
      19,    20,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    -1,
      59,    60,    61,    62,    63,    64,    65,    66,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,    77,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   100,    -1,    -1,   103,   104,    -1,   106,   107,    -1,
      -1,    -1,    -1,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,
      11,    12,   141,   142,   143,    16,    -1,    18,    19,    20,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    -1,    59,    60,
      61,    62,    63,    64,    65,    66,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    76,    77,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,
      -1,    -1,   103,   104,    -1,   106,   107,    -1,    -1,    -1,
      -1,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,    -1,     7,    -1,    -1,    -1,    11,    12,
     141,   142,   143,    16,    -1,    18,    19,    20,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    46,    -1,    -1,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    -1,    59,    60,    61,    62,
      63,    64,    65,    66,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    76,    77,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,    92,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,
     103,   104,    -1,   106,   107,    -1,    -1,    -1,    -1,   112,
     113,   114,   115,   116,   117,   118,   119,   120,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,    -1,     7,    -1,    -1,    -1,    11,    12,   141,   142,
     143,    16,    -1,    18,    19,    20,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    46,    -1,    -1,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    59,    60,    61,    62,    63,    64,
      65,    66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,   103,   104,
      -1,   106,   107,    -1,   109,    -1,    -1,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,
      11,    12,    -1,    -1,    -1,    16,   141,    18,    19,    20,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    -1,    59,    60,
      61,    62,    63,    64,    65,    66,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,
      -1,    -1,   103,   104,    -1,   106,   107,    -1,   109,    -1,
      -1,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,    -1,
       7,    -1,    -1,    -1,    11,    12,    -1,    -1,    -1,    16,
     141,    18,    19,    20,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    46,
      -1,    -1,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    -1,    59,    60,    61,    62,    63,    64,    65,    66,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    95,    -1,
      -1,    -1,    -1,   100,    -1,    -1,   103,   104,    -1,   106,
     107,    -1,    -1,    -1,    -1,   112,   113,   114,   115,   116,
     117,   118,   119,   120,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,    -1,     7,    -1,    -1,    -1,    11,    12,
      -1,    -1,    -1,    16,   141,    18,    19,    20,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    46,    -1,    -1,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    -1,    59,    60,    61,    62,
      63,    64,    65,    66,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,
     103,   104,    -1,   106,   107,    -1,    -1,    -1,    -1,   112,
     113,   114,   115,   116,   117,   118,   119,   120,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,    -1,     7,    -1,
      -1,    -1,    11,    12,    -1,    -1,    -1,    16,   141,    18,
      19,    20,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    -1,
      59,    60,    61,    62,    63,    64,    65,    66,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   100,    -1,    -1,   103,   104,    -1,   106,   107,    -1,
      -1,    -1,    -1,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,    -1,     7,    -1,    -1,    -1,    11,    12,    -1,    -1,
      -1,    16,   141,    18,    19,    20,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    46,    -1,    -1,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    59,    60,    61,    62,    63,    64,
      65,    66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,   103,   104,
      -1,   106,   107,    -1,    -1,    -1,    -1,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,
      11,    12,    -1,    -1,    -1,    16,   141,    18,    19,    20,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    -1,    59,    60,
      61,    62,    63,    64,    65,    66,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,
      -1,    -1,   103,   104,    -1,   106,   107,    33,    34,    35,
      36,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,    -1,    -1,    49,    50,    51,    52,    -1,    -1,    -1,
      56,    -1,    58,    -1,    60,    61,    62,    63,    64,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    78,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    91,    92,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   100,    -1,    -1,   103,    -1,    -1,
     106,   107,    -1,   109,   110,    -1,   112,   113,   114,   115,
     116,   117,   118,   119,   120,    33,    34,    35,    36,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
      -1,    49,    50,    51,    52,   141,    -1,    -1,    56,    -1,
      -1,    -1,    60,    61,    62,    63,    64,    -1,    33,    34,
      35,    36,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    49,    50,    51,    52,    -1,    -1,
      -1,    56,    -1,    91,    92,    60,    61,    62,    63,    64,
      -1,    -1,   100,    -1,    -1,   103,    -1,    -1,   106,   107,
      -1,   109,    -1,    -1,   112,   113,   114,   115,   116,   117,
     118,   119,   120,    -1,    -1,    -1,    91,    92,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   100,   134,    -1,   103,    -1,
      -1,   106,   107,   141,   109,    -1,    -1,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    33,    34,    35,    36,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,    -1,    49,    50,    51,    52,   141,    -1,    -1,    56,
      -1,    -1,    -1,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    91,    92,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   100,    52,    53,   103,    -1,    56,   106,
     107,    -1,    -1,    -1,    -1,   112,   113,   114,   115,   116,
     117,   118,   119,   120,    -1,    -1,    -1,    -1,    76,    77,
      78,    79,    80,    81,    82,    83,    84,   134,    -1,    87,
      88,    -1,    -1,    -1,   141,    93,    94,    95,    96,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   109,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   131,   132,   133,   134,   135,   136,   137,
     138,   139,   140,    -1,   142,   143,    52,    53,    -1,    -1,
      56,   149,   150,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    -1,
      -1,    87,    88,    -1,    -1,    -1,    -1,    93,    94,    95,
      96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   131,   132,   133,   134,   135,
     136,   137,   138,   139,   140,    -1,   142,   143,    52,    53,
      -1,    -1,    56,   149,   150,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    -1,    -1,    87,    88,    -1,    -1,    -1,    -1,    93,
      94,    95,    96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   109,   110,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,   132,   133,
     134,   135,   136,   137,   138,   139,   140,    -1,   142,   143,
      52,    53,    -1,    -1,    56,   149,   150,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    -1,    -1,    87,    88,    -1,    -1,    -1,
      -1,    93,    94,    95,    96,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,
     132,   133,   134,   135,   136,   137,   138,   139,   140,    -1,
     142,   143,    52,    53,    -1,    -1,    56,   149,   150,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    -1,    -1,    87,    88,    -1,
      -1,    -1,    -1,    93,    94,    95,    96,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,
     110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   131,   132,   133,   134,   135,   136,   137,   138,   139,
     140,    -1,   142,   143,    52,    53,    -1,    -1,    56,   149,
     150,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    -1,    -1,    87,
      88,    -1,    -1,    -1,    -1,    93,    94,    95,    96,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   109,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   131,   132,   133,   134,   135,   136,   137,
     138,   139,   140,    -1,   142,   143,    52,    53,    -1,    -1,
      56,   149,   150,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    -1,
      -1,    87,    88,    -1,    -1,    -1,    -1,    93,    94,    95,
      96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   131,   132,   133,   134,   135,
     136,   137,   138,   139,   140,    -1,   142,   143,    52,    53,
      -1,    -1,    56,   149,   150,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    -1,    -1,    87,    88,    -1,    -1,    -1,    -1,    93,
      94,    95,    96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   109,   110,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,   132,   133,
     134,   135,   136,   137,   138,   139,   140,    -1,   142,   143,
      52,    53,    -1,    -1,    56,   149,   150,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    -1,    -1,    87,    88,    -1,    -1,    -1,
      -1,    93,    94,    95,    96,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,
     132,   133,   134,   135,   136,   137,   138,   139,   140,    -1,
     142,   143,    52,    53,    -1,    -1,    56,   149,   150,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    -1,    -1,    87,    88,    -1,
      -1,    -1,    -1,    93,    94,    95,    96,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,
     110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   131,   132,   133,   134,   135,   136,   137,   138,   139,
     140,    -1,   142,   143,    52,    53,    -1,    -1,    56,   149,
     150,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    -1,    -1,    87,
      88,    -1,    -1,    -1,    -1,    93,    94,    95,    96,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   109,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   131,   132,   133,   134,   135,   136,   137,
     138,   139,   140,    -1,   142,   143,    52,    53,    -1,    -1,
      56,   149,   150,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    -1,
      -1,    87,    88,    -1,    -1,    -1,    -1,    93,    94,    95,
      96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   131,   132,   133,   134,   135,
     136,   137,   138,   139,   140,    -1,   142,   143,    52,    53,
      -1,    -1,    56,   149,   150,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    -1,    -1,    87,    88,    -1,    -1,    -1,    -1,    93,
      94,    95,    96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   109,   110,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,   132,   133,
     134,   135,   136,   137,   138,   139,   140,    -1,   142,   143,
      52,    53,    -1,    -1,    56,   149,   150,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    -1,    -1,    87,    88,    -1,    -1,    -1,
      -1,    93,    94,    95,    96,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,
     132,   133,   134,   135,   136,   137,   138,   139,   140,    -1,
     142,   143,    52,    53,    -1,    -1,    56,   149,   150,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    -1,    -1,    87,    88,    -1,
      -1,    -1,    -1,    93,    94,    95,    96,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,
     110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   131,   132,   133,   134,   135,   136,   137,   138,   139,
     140,    -1,   142,   143,    52,    53,    -1,    -1,    56,   149,
     150,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    -1,    -1,    87,
      88,    -1,    -1,    -1,    -1,    93,    94,    95,    96,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   109,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   131,   132,   133,   134,   135,   136,   137,
     138,   139,   140,    -1,   142,   143,    -1,    -1,    -1,    -1,
      -1,   149
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint16 yystos[] =
{
       0,   157,   158,     0,     1,     3,     4,     5,     6,     7,
      11,    12,    16,    18,    19,    20,    21,    22,    23,    24,
      30,    31,    32,    33,    34,    35,    36,    39,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    59,    60,    61,    62,    63,    64,    65,    66,    76,
      77,    91,    92,   100,   103,   104,   106,   107,   109,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   141,   142,
     143,   159,   160,   161,   169,   171,   173,   178,   179,   181,
     182,   183,   185,   186,   187,   189,   190,   199,   202,   217,
     232,   233,   234,   235,   236,   237,   238,   239,   240,   241,
     242,   251,   276,   277,   317,   318,   319,   320,   321,   322,
     323,   326,   328,   329,   343,   344,   346,   347,   348,   349,
     350,   351,   352,   353,   387,   400,   161,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      56,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      87,    88,    93,    94,    95,    96,   109,   110,   131,   132,
     133,   134,   135,   136,   137,   138,   139,   140,   142,   143,
     149,   193,   194,   195,   197,   198,   343,    39,    58,   100,
     103,   109,   110,   111,   114,   142,   182,   190,   199,   203,
     209,   212,   214,   232,   349,   350,   352,   353,   385,   386,
     209,   150,   210,   211,   150,   206,   210,   150,   155,   394,
      54,   194,   394,   145,   162,   145,    21,    22,    31,    32,
     181,   199,   232,   251,   199,   199,   199,    56,     1,    47,
     103,   165,   166,   167,   169,   184,   185,   400,   169,   219,
     204,   214,   385,   400,   203,   384,   385,   400,    46,   100,
     141,   148,   189,   217,   232,   349,   350,   353,   222,    54,
      55,    57,   193,   332,   345,   332,   333,   334,   154,   154,
     154,   154,   348,   178,   199,   199,   153,   155,   393,   398,
     399,    40,    41,    42,    43,    44,    37,    38,    26,   145,
     206,   210,   243,   278,    28,   244,   275,   128,   148,   103,
     109,   186,   128,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    95,    96,   129,   131,
     132,   133,   134,   135,   136,   137,   138,   139,   140,   201,
     201,    69,    97,    98,    99,   147,   391,   218,   173,   174,
     174,   175,   176,   175,   174,   393,   399,   100,   183,   190,
     232,   256,   349,   350,   353,    52,    56,    95,   100,   191,
     192,   232,   349,   350,   353,   192,    33,    34,    35,    36,
      49,    50,    51,    52,    56,   150,   193,   351,   382,   209,
      98,   391,   392,   278,   320,   101,   101,   148,   203,    56,
     203,   203,   203,   332,   128,   102,   148,   213,   400,    98,
     147,   391,   101,   101,   148,   213,   209,   394,   395,   209,
     208,   209,   214,   385,   400,   173,   395,   173,    54,    65,
      66,   170,   150,   200,   159,   165,    98,   391,   101,   169,
     168,   184,   151,   393,   399,   395,   220,   395,   152,   148,
     155,   397,   148,   397,   146,   397,   394,    56,   348,   186,
     188,   148,    98,   147,   391,   268,   269,    67,   121,   123,
     124,   335,   121,   121,   335,    68,   335,   324,   330,   327,
     331,    78,   153,   161,   174,   174,   174,   174,   169,   173,
     173,   279,   280,   108,   180,   282,   283,   282,   109,   178,
     203,   214,   215,   216,   184,   148,   189,   148,   171,   172,
     178,   190,   199,   203,   205,   216,   232,   353,   199,   199,
     199,   199,   199,   199,   199,   199,   199,   199,   199,   199,
     199,   199,   199,   199,   199,   199,   199,   199,   199,   199,
     199,   199,    52,    53,    56,   197,   206,   388,   389,    52,
      53,    56,   197,   388,   208,    52,    56,   206,   388,   163,
     165,    13,   252,   398,   252,   165,   174,   165,   393,   224,
      56,    98,   147,   391,    25,   173,    52,    56,   191,   132,
     354,    98,   147,   391,   227,   383,   228,    69,    98,   390,
     388,   172,   199,   205,   172,   205,   196,   126,   203,   109,
     203,   212,   385,    52,    56,   208,    52,    56,   386,   395,
     151,   395,   148,   148,   395,   194,   221,   199,   146,   146,
     388,   388,   205,   162,   395,   167,   395,   385,   148,   188,
      52,    56,   208,    52,    56,   270,   337,   336,   121,   325,
     335,    67,   121,   121,   325,    67,   121,   199,   146,   281,
     279,    10,   250,   284,   250,   203,   148,   395,   188,   148,
      44,   128,    44,    98,   147,   391,   394,   101,   101,   206,
     210,   394,   396,   101,   101,   206,   207,   210,   400,   250,
       8,   245,   313,   400,   165,    13,   165,   250,    27,   253,
     398,   250,    25,   223,   288,    17,   247,   286,    52,    56,
     208,    52,    56,   175,   226,   355,   225,    52,    56,   191,
     208,   163,   173,   229,   230,   207,   210,   194,   203,   203,
     213,   101,   101,   396,   101,   101,   385,   173,   397,   186,
     396,   271,   338,    54,    55,    57,    59,   342,   353,   154,
     335,   154,   154,   154,    86,   133,   263,   264,   400,   146,
     263,   109,   203,   188,   169,   199,    52,    56,   208,    52,
      56,   130,   172,   205,   172,   205,   180,   152,   101,   172,
     205,   172,   205,   180,   203,   216,   314,   400,     9,    15,
     246,   248,   316,   400,    14,   248,   249,   254,   255,   400,
     255,   177,   289,   286,   250,   109,   203,   285,   250,   396,
     165,   398,   174,   163,   396,   250,   395,   150,   356,   357,
     193,   278,   275,   101,   148,   395,    52,    54,    55,    56,
      57,    58,    78,   103,   109,   110,   111,   135,   138,   150,
     273,   358,   360,   361,   362,   363,   364,   365,   366,   367,
     370,   371,   372,   373,   376,   377,   378,   379,   380,   339,
     260,   262,   265,   363,   365,   366,   368,   369,   372,   374,
     375,   378,   380,   394,   165,   163,   203,   396,   199,   172,
     205,   102,   315,   400,   165,   164,   165,   174,   250,   250,
      52,    56,    58,    91,    92,   100,   103,   106,   107,   109,
     112,   114,   134,   292,   293,   294,   295,   296,   297,   300,
     303,   304,   305,   306,   307,   308,   310,   311,   312,   317,
     318,   321,   322,   323,   326,   328,   329,   350,   371,   250,
     203,   148,   252,   250,   163,   398,   250,   360,   163,   360,
     231,   103,   109,   257,   258,   259,   362,   360,   272,   128,
     148,   359,   203,   148,   381,   400,    52,   148,   381,   148,
     359,    52,   148,   359,    52,   340,   265,   133,   128,   148,
     261,   100,   232,   148,   381,   381,   148,   261,   148,   261,
     153,   100,   190,   232,   349,   350,   353,   252,   165,   252,
     294,   308,   308,    56,   191,   294,   294,   298,   299,   300,
     302,   396,   114,   146,   303,    52,   148,   309,   332,    52,
     252,    40,    41,   102,   148,   133,   148,    89,    90,    98,
     147,   150,    52,   109,   203,   165,   250,   395,   250,   398,
     356,   259,   148,   362,   148,   395,   265,    29,   125,   274,
     203,   358,   364,   376,   378,   367,   372,   380,   365,   373,
     378,   363,   365,   341,   133,   232,   260,   375,   378,    56,
      98,   368,   372,   365,   374,   378,   365,    52,   266,   267,
     361,    56,    98,   147,   391,   165,   316,   165,   395,   396,
     109,   294,   302,   148,   332,   146,   148,   301,   302,   268,
     126,   290,   174,   174,   310,   298,   297,   305,   306,   308,
     308,   191,   298,   303,   396,   298,   303,   395,   203,   255,
     286,   287,   163,   395,   258,   148,   109,   257,   151,   163,
     165,   148,   359,   148,   359,   381,   148,   359,   148,   359,
     359,   165,   148,   261,   148,   261,    52,    56,   381,   148,
     261,   148,   261,   261,   148,   394,    52,    56,   208,    52,
      56,   313,   254,    52,   148,   148,   301,   148,   294,   165,
     396,   396,   395,   395,   250,   148,   258,   148,   362,   250,
     146,   365,   378,   365,   365,   122,   365,   378,   365,   365,
     267,   396,   148,   301,   302,   255,   288,   291,   258,   148,
     359,   148,   359,   359,   359,   261,   148,   261,   261,   261,
     301,   258,   365,   365,   359,   261
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint16 yyr1[] =
{
       0,   156,   158,   157,   159,   160,   160,   160,   160,   161,
     161,   162,   164,   163,   163,   165,   166,   166,   166,   166,
     167,   168,   167,   170,   169,   169,   169,   169,   169,   169,
     169,   169,   169,   169,   169,   169,   169,   169,   169,   169,
     171,   171,   171,   171,   171,   171,   171,   171,   172,   172,
     172,   173,   173,   173,   173,   173,   173,   174,   176,   177,
     175,   178,   178,   179,   179,   180,   181,   182,   182,   182,
     182,   182,   182,   182,   182,   182,   182,   182,   183,   183,
     184,   184,   185,   185,   185,   185,   185,   185,   185,   185,
     185,   185,   186,   186,   187,   187,   188,   188,   189,   189,
     189,   189,   189,   189,   189,   189,   189,   190,   190,   190,
     190,   190,   190,   190,   190,   190,   191,   191,   192,   192,
     192,   193,   193,   193,   193,   193,   194,   194,   195,   196,
     195,   197,   197,   197,   197,   197,   197,   197,   197,   197,
     197,   197,   197,   197,   197,   197,   197,   197,   197,   197,
     197,   197,   197,   197,   197,   197,   197,   197,   197,   197,
     197,   198,   198,   198,   198,   198,   198,   198,   198,   198,
     198,   198,   198,   198,   198,   198,   198,   198,   198,   198,
     198,   198,   198,   198,   198,   198,   198,   198,   198,   198,
     198,   198,   198,   198,   198,   198,   198,   198,   198,   198,
     198,   198,   199,   199,   199,   199,   199,   199,   199,   199,
     199,   199,   199,   199,   199,   199,   199,   199,   199,   199,
     199,   199,   199,   199,   199,   199,   199,   199,   199,   199,
     199,   199,   199,   199,   199,   199,   199,   199,   199,   199,
     199,   199,   200,   199,   199,   199,   201,   201,   201,   201,
     202,   202,   203,   204,   204,   204,   204,   205,   205,   206,
     207,   207,   208,   208,   208,   208,   208,   209,   209,   209,
     209,   209,   211,   210,   212,   213,   213,   214,   214,   214,
     214,   215,   215,   216,   216,   216,   217,   217,   217,   217,
     217,   217,   217,   217,   217,   217,   217,   218,   217,   219,
     217,   220,   217,   217,   217,   217,   217,   217,   217,   217,
     217,   217,   221,   217,   217,   217,   217,   217,   217,   222,
     217,   217,   217,   217,   217,   223,   217,   224,   217,   217,
     217,   225,   217,   226,   217,   227,   217,   228,   229,   217,
     230,   231,   217,   217,   217,   217,   217,   217,   232,   233,
     234,   235,   236,   237,   238,   239,   240,   241,   242,   243,
     244,   245,   246,   247,   248,   249,   250,   251,   252,   252,
     252,   253,   253,   254,   254,   255,   255,   256,   256,   257,
     257,   258,   258,   259,   259,   259,   259,   259,   259,   259,
     259,   259,   260,   260,   260,   260,   261,   261,   262,   262,
     262,   262,   262,   262,   262,   262,   262,   262,   262,   262,
     262,   262,   262,   263,   263,   264,   264,   264,   265,   265,
     266,   266,   267,   267,   269,   270,   271,   272,   268,   273,
     273,   274,   274,   275,   276,   276,   276,   276,   277,   277,
     277,   277,   277,   277,   277,   277,   277,   278,   278,   280,
     281,   279,   283,   284,   282,   285,   285,   285,   285,   286,
     287,   287,   289,   290,   288,   291,   291,   292,   292,   292,
     293,   293,   293,   293,   293,   294,   295,   295,   296,   296,
     297,   297,   297,   297,   297,   297,   297,   297,   297,   297,
     297,   297,   298,   298,   298,   298,   298,   298,   298,   298,
     299,   299,   300,   300,   300,   300,   301,   301,   302,   303,
     303,   303,   304,   304,   305,   305,   305,   305,   306,   306,
     307,   307,   307,   307,   307,   307,   307,   307,   307,   307,
     308,   308,   308,   308,   308,   308,   308,   308,   308,   309,
     308,   310,   311,   312,   312,   312,   313,   313,   314,   314,
     314,   315,   315,   316,   316,   317,   317,   318,   319,   319,
     319,   320,   321,   322,   323,   324,   324,   325,   325,   326,
     327,   327,   328,   329,   330,   330,   331,   331,   332,   332,
     333,   333,   334,   334,   335,   336,   335,   337,   338,   339,
     340,   341,   335,   342,   342,   342,   342,   342,   343,   343,
     344,   345,   345,   345,   345,   346,   347,   347,   348,   348,
     348,   348,   349,   349,   349,   349,   349,   349,   350,   350,
     350,   350,   350,   350,   350,   351,   351,   352,   352,   353,
     353,   355,   354,   354,   356,   357,   356,   358,   358,   358,
     358,   359,   359,   360,   360,   360,   360,   360,   360,   360,
     360,   360,   360,   360,   360,   360,   360,   360,   361,   361,
     361,   361,   362,   362,   363,   364,   364,   365,   365,   366,
     367,   367,   368,   368,   369,   369,   370,   370,   371,   371,
     372,   372,   373,   374,   375,   375,   376,   376,   377,   377,
     378,   378,   379,   379,   380,   381,   381,   382,   383,   382,
     384,   384,   385,   385,   386,   386,   386,   386,   387,   387,
     387,   388,   388,   388,   388,   389,   389,   389,   390,   390,
     391,   391,   392,   392,   393,   393,   394,   394,   395,   396,
     397,   397,   397,   398,   398,   399,   399,   400
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     2,     1,     1,     3,     2,     1,
       2,     3,     0,     6,     3,     2,     1,     1,     3,     2,
       1,     0,     3,     0,     4,     3,     3,     3,     2,     3,
       3,     3,     3,     3,     4,     1,     3,     3,     3,     1,
       3,     3,     6,     5,     5,     5,     5,     3,     1,     3,
       1,     1,     3,     3,     3,     2,     1,     1,     0,     0,
       4,     1,     1,     1,     4,     3,     1,     2,     3,     4,
       5,     4,     5,     2,     2,     2,     2,     2,     1,     3,
       1,     3,     1,     2,     3,     5,     2,     4,     2,     4,
       1,     3,     1,     3,     2,     3,     1,     3,     1,     1,
       4,     3,     3,     3,     3,     2,     1,     1,     1,     4,
       3,     3,     3,     3,     2,     1,     1,     1,     2,     1,
       3,     1,     1,     1,     1,     1,     1,     1,     1,     0,
       4,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     3,     3,     6,     5,     5,     5,     5,     4,
       3,     3,     3,     2,     2,     2,     2,     3,     3,     3,
       3,     3,     3,     4,     2,     2,     3,     3,     3,     3,
       1,     3,     3,     3,     3,     3,     2,     2,     3,     3,
       3,     3,     0,     4,     6,     1,     1,     1,     1,     1,
       3,     3,     1,     1,     2,     4,     2,     1,     3,     3,
       1,     1,     1,     1,     2,     4,     2,     1,     2,     2,
       4,     1,     0,     2,     2,     2,     1,     1,     2,     3,
       4,     1,     1,     3,     4,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     0,     4,     0,
       3,     0,     4,     3,     3,     2,     3,     3,     1,     4,
       3,     1,     0,     6,     4,     3,     2,     1,     2,     0,
       3,     6,     6,     4,     4,     0,     6,     0,     5,     5,
       6,     0,     6,     0,     7,     0,     5,     0,     0,     7,
       0,     0,     9,     1,     1,     1,     1,     3,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       2,     1,     1,     1,     5,     1,     2,     1,     1,     1,
       3,     1,     3,     1,     4,     6,     3,     5,     2,     4,
       1,     3,     4,     2,     2,     1,     2,     0,     6,     8,
       4,     6,     4,     2,     6,     2,     4,     6,     2,     4,
       2,     4,     1,     1,     1,     3,     1,     4,     1,     4,
       1,     3,     1,     1,     0,     0,     0,     0,     6,     4,
       1,     3,     3,     3,     2,     4,     5,     5,     2,     4,
       4,     3,     3,     3,     2,     1,     4,     3,     3,     0,
       0,     4,     0,     0,     4,     1,     2,     3,     4,     5,
       1,     1,     0,     0,     7,     1,     1,     1,     3,     3,
       1,     2,     3,     1,     1,     1,     3,     1,     3,     1,
       1,     4,     4,     3,     4,     4,     3,     3,     2,     3,
       2,     3,     1,     1,     2,     3,     5,     2,     4,     1,
       2,     3,     2,     4,     1,     3,     1,     3,     1,     3,
       1,     1,     1,     3,     2,     1,     4,     3,     2,     1,
       1,     3,     3,     2,     2,     1,     1,     1,     2,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     0,
       3,     1,     2,     2,     3,     1,     6,     1,     1,     1,
       1,     2,     1,     2,     1,     1,     1,     1,     1,     1,
       2,     3,     3,     3,     4,     0,     3,     1,     2,     4,
       0,     3,     4,     4,     0,     3,     0,     3,     0,     2,
       0,     2,     0,     2,     1,     0,     3,     0,     0,     0,
       0,     0,     8,     1,     1,     1,     1,     1,     1,     1,
       2,     1,     1,     1,     1,     3,     1,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     0,     4,     0,     3,     0,     3,     4,     2,     2,
       1,     2,     0,     6,     8,     4,     6,     4,     6,     2,
       4,     6,     2,     4,     2,     4,     1,     0,     1,     1,
       1,     1,     1,     1,     1,     1,     3,     1,     3,     1,
       2,     1,     2,     1,     1,     3,     1,     3,     1,     1,
       2,     1,     3,     3,     1,     3,     1,     3,     1,     1,
       2,     1,     1,     1,     2,     2,     1,     1,     0,     4,
       1,     2,     1,     3,     3,     2,     4,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     0,     1,     0,     1,     2,     2,
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

#define YYBACKUP(Token, Value)                                    \
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
static int
yy_location_print_ (FILE *yyo, YYLTYPE const * const yylocp)
{
  int res = 0;
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


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, struct parser_params *p)
{
  FILE *yyoutput = yyo;
  YYUSE (yyoutput);
  YYUSE (yylocationp);
  YYUSE (p);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyo, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, struct parser_params *p)
{
  YYFPRINTF (p, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  YY_LOCATION_PRINT (yyo, *yylocationp);
  YYFPRINTF (p, ": ");
  yy_symbol_value_print (yyo, yytype, yyvaluep, yylocationp, p);
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
  unsigned long yylno = yyrline[yyrule];
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
                       &yyvsp[(yyi + 1) - (yynrhs)]
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
            else
              goto append;

          append:
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

  return (YYSIZE_T) (yystpcpy (yyres, yystr) - yyres);
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
                  if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
                    yysize = yysize1;
                  else
                    return 2;
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
    default: /* Avoid compiler warnings. */
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
    if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
      yysize = yysize1;
    else
      return 2;
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
#line 921 "parse.y" /* yacc.c:1431  */
{
    RUBY_SET_YYLLOC_OF_NONE(yylloc);
}

#line 5715 "parse.c" /* yacc.c:1431  */
  yylsp[0] = yylloc;
  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yynewstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  *yyssp = (yytype_int16) yystate;

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    goto yyexhaustedlab;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = (YYSIZE_T) (yyssp - yyss + 1);

# if defined yyoverflow
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
        yyss = yyss1;
        yyvs = yyvs1;
        yyls = yyls1;
      }
# else /* defined YYSTACK_RELOCATE */
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
# undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YYDPRINTF ((p, "Stack size increased to %lu\n",
                  (unsigned long) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */

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
| yyreduce -- do a reduction.  |
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

  /* Default location. */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  yyerror_range[1] = yyloc;
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 1123 "parse.y" /* yacc.c:1652  */
    {
			SET_LEX_STATE(EXPR_BEG);
			local_push(p, ifndef_ripper(1)+0);
		    }
#line 5912 "parse.c" /* yacc.c:1652  */
    break;

  case 3:
#line 1128 "parse.y" /* yacc.c:1652  */
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
#line 5936 "parse.c" /* yacc.c:1652  */
    break;

  case 4:
#line 1150 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = void_stmts(p, (yyvsp[-1].node));
		    }
#line 5944 "parse.c" /* yacc.c:1652  */
    break;

  case 5:
#line 1156 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_BEGIN(0, &(yyloc));
		    /*% %*/
		    /*% ripper: stmts_add!(stmts_new!, void_stmt!) %*/
		    }
#line 5955 "parse.c" /* yacc.c:1652  */
    break;

  case 6:
#line 1163 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = newline_node((yyvsp[0].node));
		    /*% %*/
		    /*% ripper: stmts_add!(stmts_new!, $1) %*/
		    }
#line 5966 "parse.c" /* yacc.c:1652  */
    break;

  case 7:
#line 1170 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = block_append(p, (yyvsp[-2].node), newline_node((yyvsp[0].node)));
		    /*% %*/
		    /*% ripper: stmts_add!($1, $3) %*/
		    }
#line 5977 "parse.c" /* yacc.c:1652  */
    break;

  case 8:
#line 1177 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = remove_begin((yyvsp[0].node));
		    }
#line 5985 "parse.c" /* yacc.c:1652  */
    break;

  case 10:
#line 1184 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = (yyvsp[0].node);
		    }
#line 5993 "parse.c" /* yacc.c:1652  */
    break;

  case 11:
#line 1190 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			p->eval_tree_begin = block_append(p, p->eval_tree_begin,
							  NEW_BEGIN((yyvsp[-1].node), &(yyloc)));
			(yyval.node) = NEW_BEGIN(0, &(yyloc));
		    /*% %*/
		    /*% ripper: BEGIN!($2) %*/
		    }
#line 6006 "parse.c" /* yacc.c:1652  */
    break;

  case 12:
#line 1202 "parse.y" /* yacc.c:1652  */
    {if (!(yyvsp[-1].node)) {yyerror1(&(yylsp[0]), "else without rescue is useless");}}
#line 6012 "parse.c" /* yacc.c:1652  */
    break;

  case 13:
#line 1205 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = new_bodystmt(p, (yyvsp[-5].node), (yyvsp[-4].node), (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: bodystmt!(escape_Qundef($1), escape_Qundef($2), escape_Qundef($5), escape_Qundef($6)) %*/
		    }
#line 6023 "parse.c" /* yacc.c:1652  */
    break;

  case 14:
#line 1214 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = new_bodystmt(p, (yyvsp[-2].node), (yyvsp[-1].node), 0, (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: bodystmt!(escape_Qundef($1), escape_Qundef($2), Qnil, escape_Qundef($3)) %*/
		    }
#line 6034 "parse.c" /* yacc.c:1652  */
    break;

  case 15:
#line 1223 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = void_stmts(p, (yyvsp[-1].node));
		    }
#line 6042 "parse.c" /* yacc.c:1652  */
    break;

  case 16:
#line 1229 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_BEGIN(0, &(yyloc));
		    /*% %*/
		    /*% ripper: stmts_add!(stmts_new!, void_stmt!) %*/
		    }
#line 6053 "parse.c" /* yacc.c:1652  */
    break;

  case 17:
#line 1236 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = newline_node((yyvsp[0].node));
		    /*% %*/
		    /*% ripper: stmts_add!(stmts_new!, $1) %*/
		    }
#line 6064 "parse.c" /* yacc.c:1652  */
    break;

  case 18:
#line 1243 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = block_append(p, (yyvsp[-2].node), newline_node((yyvsp[0].node)));
		    /*% %*/
		    /*% ripper: stmts_add!($1, $3) %*/
		    }
#line 6075 "parse.c" /* yacc.c:1652  */
    break;

  case 19:
#line 1250 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = remove_begin((yyvsp[0].node));
		    }
#line 6083 "parse.c" /* yacc.c:1652  */
    break;

  case 20:
#line 1256 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = (yyvsp[0].node);
		    }
#line 6091 "parse.c" /* yacc.c:1652  */
    break;

  case 21:
#line 1260 "parse.y" /* yacc.c:1652  */
    {
			yyerror1(&(yylsp[0]), "BEGIN is permitted only at toplevel");
		    }
#line 6099 "parse.c" /* yacc.c:1652  */
    break;

  case 22:
#line 1264 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = (yyvsp[0].node);
		    }
#line 6107 "parse.c" /* yacc.c:1652  */
    break;

  case 23:
#line 1269 "parse.y" /* yacc.c:1652  */
    {SET_LEX_STATE(EXPR_FNAME|EXPR_FITEM);}
#line 6113 "parse.c" /* yacc.c:1652  */
    break;

  case 24:
#line 1270 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_ALIAS((yyvsp[-2].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: alias!($2, $4) %*/
		    }
#line 6124 "parse.c" /* yacc.c:1652  */
    break;

  case 25:
#line 1277 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_VALIAS((yyvsp[-1].id), (yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: var_alias!($2, $3) %*/
		    }
#line 6135 "parse.c" /* yacc.c:1652  */
    break;

  case 26:
#line 1284 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			char buf[2];
			buf[0] = '$';
			buf[1] = (char)(yyvsp[0].node)->nd_nth;
			(yyval.node) = NEW_VALIAS((yyvsp[-1].id), rb_intern2(buf, 2), &(yyloc));
		    /*% %*/
		    /*% ripper: var_alias!($2, $3) %*/
		    }
#line 6149 "parse.c" /* yacc.c:1652  */
    break;

  case 27:
#line 1294 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			yyerror1(&(yylsp[0]), "can't make alias for the number variables");
			(yyval.node) = NEW_BEGIN(0, &(yyloc));
		    /*% %*/
		    /*% ripper[error]: alias_error!(var_alias!($2, $3)) %*/
		    }
#line 6161 "parse.c" /* yacc.c:1652  */
    break;

  case 28:
#line 1302 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[0].node);
		    /*% %*/
		    /*% ripper: undef!($2) %*/
		    }
#line 6172 "parse.c" /* yacc.c:1652  */
    break;

  case 29:
#line 1309 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = new_if(p, (yyvsp[0].node), remove_begin((yyvsp[-2].node)), 0, &(yyloc));
			fixpos((yyval.node), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: if_mod!($3, $1) %*/
		    }
#line 6184 "parse.c" /* yacc.c:1652  */
    break;

  case 30:
#line 1317 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = new_unless(p, (yyvsp[0].node), remove_begin((yyvsp[-2].node)), 0, &(yyloc));
			fixpos((yyval.node), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: unless_mod!($3, $1) %*/
		    }
#line 6196 "parse.c" /* yacc.c:1652  */
    break;

  case 31:
#line 1325 "parse.y" /* yacc.c:1652  */
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
#line 6212 "parse.c" /* yacc.c:1652  */
    break;

  case 32:
#line 1337 "parse.y" /* yacc.c:1652  */
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
#line 6228 "parse.c" /* yacc.c:1652  */
    break;

  case 33:
#line 1349 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			NODE *resq;
			YYLTYPE loc = code_loc_gen(&(yylsp[-1]), &(yylsp[0]));
			resq = NEW_RESBODY(0, remove_begin((yyvsp[0].node)), 0, &loc);
			(yyval.node) = NEW_RESCUE(remove_begin((yyvsp[-2].node)), resq, 0, &(yyloc));
		    /*% %*/
		    /*% ripper: rescue_mod!($1, $3) %*/
		    }
#line 6242 "parse.c" /* yacc.c:1652  */
    break;

  case 34:
#line 1359 "parse.y" /* yacc.c:1652  */
    {
			if (p->in_def) {
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
#line 6260 "parse.c" /* yacc.c:1652  */
    break;

  case 36:
#line 1374 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			value_expr((yyvsp[0].node));
			(yyval.node) = node_assign(p, (yyvsp[-2].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: massign!($1, $3) %*/
		    }
#line 6272 "parse.c" /* yacc.c:1652  */
    break;

  case 37:
#line 1382 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			value_expr((yyvsp[0].node));
			(yyval.node) = node_assign(p, (yyvsp[-2].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: assign!($1, $3) %*/
		    }
#line 6284 "parse.c" /* yacc.c:1652  */
    break;

  case 38:
#line 1390 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = node_assign(p, (yyvsp[-2].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: massign!($1, $3) %*/
		    }
#line 6295 "parse.c" /* yacc.c:1652  */
    break;

  case 40:
#line 1400 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			value_expr((yyvsp[0].node));
			(yyval.node) = node_assign(p, (yyvsp[-2].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: assign!($1, $3) %*/
		    }
#line 6307 "parse.c" /* yacc.c:1652  */
    break;

  case 41:
#line 1408 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			value_expr((yyvsp[0].node));
			(yyval.node) = new_op_assign(p, (yyvsp[-2].node), (yyvsp[-1].id), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: opassign!($1, $2, $3) %*/
		    }
#line 6319 "parse.c" /* yacc.c:1652  */
    break;

  case 42:
#line 1416 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			value_expr((yyvsp[0].node));
			(yyval.node) = new_ary_op_assign(p, (yyvsp[-5].node), (yyvsp[-3].node), (yyvsp[-1].id), (yyvsp[0].node), &(yylsp[-3]), &(yyloc));
		    /*% %*/
		    /*% ripper: opassign!(aref_field!($1, escape_Qundef($3)), $5, $6) %*/

		    }
#line 6332 "parse.c" /* yacc.c:1652  */
    break;

  case 43:
#line 1425 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			value_expr((yyvsp[0].node));
			(yyval.node) = new_attr_op_assign(p, (yyvsp[-4].node), (yyvsp[-3].id), (yyvsp[-2].id), (yyvsp[-1].id), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: opassign!(field!($1, $2, $3), $4, $5) %*/
		    }
#line 6344 "parse.c" /* yacc.c:1652  */
    break;

  case 44:
#line 1433 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			value_expr((yyvsp[0].node));
			(yyval.node) = new_attr_op_assign(p, (yyvsp[-4].node), (yyvsp[-3].id), (yyvsp[-2].id), (yyvsp[-1].id), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: opassign!(field!($1, $2, $3), $4, $5) %*/
		    }
#line 6356 "parse.c" /* yacc.c:1652  */
    break;

  case 45:
#line 1441 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			YYLTYPE loc = code_loc_gen(&(yylsp[-4]), &(yylsp[-2]));
			(yyval.node) = new_const_op_assign(p, NEW_COLON2((yyvsp[-4].node), (yyvsp[-2].id), &loc), (yyvsp[-1].id), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: opassign!(const_path_field!($1, $3), $4, $5) %*/
		    }
#line 6368 "parse.c" /* yacc.c:1652  */
    break;

  case 46:
#line 1449 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			value_expr((yyvsp[0].node));
			(yyval.node) = new_attr_op_assign(p, (yyvsp[-4].node), ID2VAL(idCOLON2), (yyvsp[-2].id), (yyvsp[-1].id), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: opassign!(field!($1, ID2VAL(idCOLON2), $3), $4, $5) %*/
		    }
#line 6380 "parse.c" /* yacc.c:1652  */
    break;

  case 47:
#line 1457 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			rb_backref_error(p, (yyvsp[-2].node));
			(yyval.node) = NEW_BEGIN(0, &(yyloc));
		    /*% %*/
		    /*% ripper[error]: assign_error!(assign!(var_field(p, $1), $3)) %*/
		    }
#line 6392 "parse.c" /* yacc.c:1652  */
    break;

  case 48:
#line 1467 "parse.y" /* yacc.c:1652  */
    {
			value_expr((yyvsp[0].node));
			(yyval.node) = (yyvsp[0].node);
		    }
#line 6401 "parse.c" /* yacc.c:1652  */
    break;

  case 49:
#line 1472 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			YYLTYPE loc = code_loc_gen(&(yylsp[-1]), &(yylsp[0]));
			value_expr((yyvsp[-2].node));
			(yyval.node) = NEW_RESCUE((yyvsp[-2].node), NEW_RESBODY(0, remove_begin((yyvsp[0].node)), 0, &loc), 0, &(yyloc));
		    /*% %*/
		    /*% ripper: rescue_mod!($1, $3) %*/
		    }
#line 6414 "parse.c" /* yacc.c:1652  */
    break;

  case 52:
#line 1485 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = logop(p, idAND, (yyvsp[-2].node), (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 6422 "parse.c" /* yacc.c:1652  */
    break;

  case 53:
#line 1489 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = logop(p, idOR, (yyvsp[-2].node), (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 6430 "parse.c" /* yacc.c:1652  */
    break;

  case 54:
#line 1493 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = call_uni_op(p, method_cond(p, (yyvsp[0].node), &(yylsp[0])), METHOD_NOT, &(yylsp[-2]), &(yyloc));
		    }
#line 6438 "parse.c" /* yacc.c:1652  */
    break;

  case 55:
#line 1497 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = call_uni_op(p, method_cond(p, (yyvsp[0].node), &(yylsp[0])), '!', &(yylsp[-1]), &(yyloc));
		    }
#line 6446 "parse.c" /* yacc.c:1652  */
    break;

  case 57:
#line 1504 "parse.y" /* yacc.c:1652  */
    {
			value_expr((yyvsp[0].node));
			(yyval.node) = (yyvsp[0].node);
		    }
#line 6455 "parse.c" /* yacc.c:1652  */
    break;

  case 58:
#line 1510 "parse.y" /* yacc.c:1652  */
    {COND_PUSH(1);}
#line 6461 "parse.c" /* yacc.c:1652  */
    break;

  case 59:
#line 1510 "parse.y" /* yacc.c:1652  */
    {COND_POP();}
#line 6467 "parse.c" /* yacc.c:1652  */
    break;

  case 60:
#line 1511 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = (yyvsp[-2].node);
		    }
#line 6475 "parse.c" /* yacc.c:1652  */
    break;

  case 64:
#line 1522 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = new_qcall(p, (yyvsp[-2].id), (yyvsp[-3].node), (yyvsp[-1].id), (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    /*% %*/
		    /*% ripper: method_add_arg!(call!($1, $2, $3), $4) %*/
		    }
#line 6486 "parse.c" /* yacc.c:1652  */
    break;

  case 65:
#line 1531 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = (yyvsp[-1].node);
		    /*%%%*/
			(yyval.node)->nd_body->nd_loc = code_loc_gen(&(yylsp[-2]), &(yylsp[0]));
			nd_set_line((yyval.node), (yylsp[-2]).end_pos.lineno);
		    /*% %*/
		    }
#line 6498 "parse.c" /* yacc.c:1652  */
    break;

  case 66:
#line 1541 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_FCALL((yyvsp[0].id), 0, &(yyloc));
			nd_set_line((yyval.node), p->tokline);
		    /*% %*/
		    /*% ripper: $1 %*/
		    }
#line 6510 "parse.c" /* yacc.c:1652  */
    break;

  case 67:
#line 1551 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyvsp[-1].node)->nd_args = (yyvsp[0].node);
			nd_set_last_loc((yyvsp[-1].node), (yylsp[0]).end_pos);
			(yyval.node) = (yyvsp[-1].node);
		    /*% %*/
		    /*% ripper: command!($1, $2) %*/
		    }
#line 6523 "parse.c" /* yacc.c:1652  */
    break;

  case 68:
#line 1560 "parse.y" /* yacc.c:1652  */
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
#line 6538 "parse.c" /* yacc.c:1652  */
    break;

  case 69:
#line 1571 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = new_command_qcall(p, (yyvsp[-2].id), (yyvsp[-3].node), (yyvsp[-1].id), (yyvsp[0].node), Qnull, &(yylsp[-1]), &(yyloc));
		    /*% %*/
		    /*% ripper: command_call!($1, $2, $3, $4) %*/
		    }
#line 6549 "parse.c" /* yacc.c:1652  */
    break;

  case 70:
#line 1578 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = new_command_qcall(p, (yyvsp[-3].id), (yyvsp[-4].node), (yyvsp[-2].id), (yyvsp[-1].node), (yyvsp[0].node), &(yylsp[-2]), &(yyloc));
		    /*% %*/
		    /*% ripper: method_add_block!(command_call!($1, $2, $3, $4), $5) %*/
		    }
#line 6560 "parse.c" /* yacc.c:1652  */
    break;

  case 71:
#line 1585 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = new_command_qcall(p, ID2VAL(idCOLON2), (yyvsp[-3].node), (yyvsp[-1].id), (yyvsp[0].node), Qnull, &(yylsp[-1]), &(yyloc));
		    /*% %*/
		    /*% ripper: command_call!($1, ID2VAL(idCOLON2), $3, $4) %*/
		    }
#line 6571 "parse.c" /* yacc.c:1652  */
    break;

  case 72:
#line 1592 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = new_command_qcall(p, ID2VAL(idCOLON2), (yyvsp[-4].node), (yyvsp[-2].id), (yyvsp[-1].node), (yyvsp[0].node), &(yylsp[-2]), &(yyloc));
		    /*% %*/
		    /*% ripper: method_add_block!(command_call!($1, ID2VAL(idCOLON2), $3, $4), $5) %*/
		   }
#line 6582 "parse.c" /* yacc.c:1652  */
    break;

  case 73:
#line 1599 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_SUPER((yyvsp[0].node), &(yyloc));
			fixpos((yyval.node), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: super!($2) %*/
		    }
#line 6594 "parse.c" /* yacc.c:1652  */
    break;

  case 74:
#line 1607 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = new_yield(p, (yyvsp[0].node), &(yyloc));
			fixpos((yyval.node), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: yield!($2) %*/
		    }
#line 6606 "parse.c" /* yacc.c:1652  */
    break;

  case 75:
#line 1615 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_RETURN(ret_args(p, (yyvsp[0].node)), &(yyloc));
		    /*% %*/
		    /*% ripper: return!($2) %*/
		    }
#line 6617 "parse.c" /* yacc.c:1652  */
    break;

  case 76:
#line 1622 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_BREAK(ret_args(p, (yyvsp[0].node)), &(yyloc));
		    /*% %*/
		    /*% ripper: break!($2) %*/
		    }
#line 6628 "parse.c" /* yacc.c:1652  */
    break;

  case 77:
#line 1629 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_NEXT(ret_args(p, (yyvsp[0].node)), &(yyloc));
		    /*% %*/
		    /*% ripper: next!($2) %*/
		    }
#line 6639 "parse.c" /* yacc.c:1652  */
    break;

  case 79:
#line 1639 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node);
		    /*% %*/
		    /*% ripper: mlhs_paren!($2) %*/
		    }
#line 6650 "parse.c" /* yacc.c:1652  */
    break;

  case 81:
#line 1649 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN(NEW_LIST((yyvsp[-1].node), &(yyloc)), 0, &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_paren!($2) %*/
		    }
#line 6661 "parse.c" /* yacc.c:1652  */
    break;

  case 82:
#line 1658 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN((yyvsp[0].node), 0, &(yyloc));
		    /*% %*/
		    /*% ripper: $1 %*/
		    }
#line 6672 "parse.c" /* yacc.c:1652  */
    break;

  case 83:
#line 1665 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN(list_append(p, (yyvsp[-1].node),(yyvsp[0].node)), 0, &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_add!($1, $2) %*/
		    }
#line 6683 "parse.c" /* yacc.c:1652  */
    break;

  case 84:
#line 1672 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN((yyvsp[-2].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_add_star!($1, $3) %*/
		    }
#line 6694 "parse.c" /* yacc.c:1652  */
    break;

  case 85:
#line 1679 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN((yyvsp[-4].node), NEW_POSTARG((yyvsp[-2].node),(yyvsp[0].node),&(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_add_post!(mlhs_add_star!($1, $3), $5) %*/
		    }
#line 6705 "parse.c" /* yacc.c:1652  */
    break;

  case 86:
#line 1686 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN((yyvsp[-1].node), NODE_SPECIAL_NO_NAME_REST, &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_add_star!($1, Qnil) %*/
		    }
#line 6716 "parse.c" /* yacc.c:1652  */
    break;

  case 87:
#line 1693 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN((yyvsp[-3].node), NEW_POSTARG(NODE_SPECIAL_NO_NAME_REST, (yyvsp[0].node), &(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_add_post!(mlhs_add_star!($1, Qnil), $4) %*/
		    }
#line 6727 "parse.c" /* yacc.c:1652  */
    break;

  case 88:
#line 1700 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN(0, (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_add_star!(mlhs_new!, $2) %*/
		    }
#line 6738 "parse.c" /* yacc.c:1652  */
    break;

  case 89:
#line 1707 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN(0, NEW_POSTARG((yyvsp[-2].node),(yyvsp[0].node),&(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_add_post!(mlhs_add_star!(mlhs_new!, $2), $4) %*/
		    }
#line 6749 "parse.c" /* yacc.c:1652  */
    break;

  case 90:
#line 1714 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN(0, NODE_SPECIAL_NO_NAME_REST, &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_add_star!(mlhs_new!, Qnil) %*/
		    }
#line 6760 "parse.c" /* yacc.c:1652  */
    break;

  case 91:
#line 1721 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN(0, NEW_POSTARG(NODE_SPECIAL_NO_NAME_REST, (yyvsp[0].node), &(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_add_post!(mlhs_add_star!(mlhs_new!, Qnil), $3) %*/
		    }
#line 6771 "parse.c" /* yacc.c:1652  */
    break;

  case 93:
#line 1731 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node);
		    /*% %*/
		    /*% ripper: mlhs_paren!($2) %*/
		    }
#line 6782 "parse.c" /* yacc.c:1652  */
    break;

  case 94:
#line 1740 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_LIST((yyvsp[-1].node), &(yylsp[-1]));
		    /*% %*/
		    /*% ripper: mlhs_add!(mlhs_new!, $1) %*/
		    }
#line 6793 "parse.c" /* yacc.c:1652  */
    break;

  case 95:
#line 1747 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = list_append(p, (yyvsp[-2].node), (yyvsp[-1].node));
		    /*% %*/
		    /*% ripper: mlhs_add!($1, $2) %*/
		    }
#line 6804 "parse.c" /* yacc.c:1652  */
    break;

  case 96:
#line 1756 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_LIST((yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_add!(mlhs_new!, $1) %*/
		    }
#line 6815 "parse.c" /* yacc.c:1652  */
    break;

  case 97:
#line 1763 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = list_append(p, (yyvsp[-2].node), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: mlhs_add!($1, $3) %*/
		    }
#line 6826 "parse.c" /* yacc.c:1652  */
    break;

  case 98:
#line 1772 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = assignable(p, (yyvsp[0].id), 0, &(yyloc));
		    /*% %*/
		    /*% ripper: assignable(p, var_field(p, $1)) %*/
		    }
#line 6837 "parse.c" /* yacc.c:1652  */
    break;

  case 99:
#line 1779 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = assignable(p, (yyvsp[0].id), 0, &(yyloc));
		    /*% %*/
		    /*% ripper: assignable(p, var_field(p, $1)) %*/
		    }
#line 6848 "parse.c" /* yacc.c:1652  */
    break;

  case 100:
#line 1786 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = aryset(p, (yyvsp[-3].node), (yyvsp[-1].node), &(yyloc));
		    /*% %*/
		    /*% ripper: aref_field!($1, escape_Qundef($3)) %*/
		    }
#line 6859 "parse.c" /* yacc.c:1652  */
    break;

  case 101:
#line 1793 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = attrset(p, (yyvsp[-2].node), (yyvsp[-1].id), (yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: field!($1, $2, $3) %*/
		    }
#line 6870 "parse.c" /* yacc.c:1652  */
    break;

  case 102:
#line 1800 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = attrset(p, (yyvsp[-2].node), idCOLON2, (yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: const_path_field!($1, $3) %*/
		    }
#line 6881 "parse.c" /* yacc.c:1652  */
    break;

  case 103:
#line 1807 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = attrset(p, (yyvsp[-2].node), (yyvsp[-1].id), (yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: field!($1, $2, $3) %*/
		    }
#line 6892 "parse.c" /* yacc.c:1652  */
    break;

  case 104:
#line 1814 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = const_decl(p, NEW_COLON2((yyvsp[-2].node), (yyvsp[0].id), &(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: const_decl(p, const_path_field!($1, $3)) %*/
		    }
#line 6903 "parse.c" /* yacc.c:1652  */
    break;

  case 105:
#line 1821 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = const_decl(p, NEW_COLON3((yyvsp[0].id), &(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: const_decl(p, top_const_field!($2)) %*/
		    }
#line 6914 "parse.c" /* yacc.c:1652  */
    break;

  case 106:
#line 1828 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			rb_backref_error(p, (yyvsp[0].node));
			(yyval.node) = NEW_BEGIN(0, &(yyloc));
		    /*% %*/
		    /*% ripper[error]: assign_error!(var_field(p, $1)) %*/
		    }
#line 6926 "parse.c" /* yacc.c:1652  */
    break;

  case 107:
#line 1838 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = assignable(p, (yyvsp[0].id), 0, &(yyloc));
		    /*% %*/
		    /*% ripper: assignable(p, var_field(p, $1)) %*/
		    }
#line 6937 "parse.c" /* yacc.c:1652  */
    break;

  case 108:
#line 1845 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = assignable(p, (yyvsp[0].id), 0, &(yyloc));
		    /*% %*/
		    /*% ripper: assignable(p, var_field(p, $1)) %*/
		    }
#line 6948 "parse.c" /* yacc.c:1652  */
    break;

  case 109:
#line 1852 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = aryset(p, (yyvsp[-3].node), (yyvsp[-1].node), &(yyloc));
		    /*% %*/
		    /*% ripper: aref_field!($1, escape_Qundef($3)) %*/
		    }
#line 6959 "parse.c" /* yacc.c:1652  */
    break;

  case 110:
#line 1859 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = attrset(p, (yyvsp[-2].node), (yyvsp[-1].id), (yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: field!($1, $2, $3) %*/
		    }
#line 6970 "parse.c" /* yacc.c:1652  */
    break;

  case 111:
#line 1866 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = attrset(p, (yyvsp[-2].node), idCOLON2, (yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: field!($1, ID2VAL(idCOLON2), $3) %*/
		    }
#line 6981 "parse.c" /* yacc.c:1652  */
    break;

  case 112:
#line 1873 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = attrset(p, (yyvsp[-2].node), (yyvsp[-1].id), (yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: field!($1, $2, $3) %*/
		    }
#line 6992 "parse.c" /* yacc.c:1652  */
    break;

  case 113:
#line 1880 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = const_decl(p, NEW_COLON2((yyvsp[-2].node), (yyvsp[0].id), &(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: const_decl(p, const_path_field!($1, $3)) %*/
		    }
#line 7003 "parse.c" /* yacc.c:1652  */
    break;

  case 114:
#line 1887 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = const_decl(p, NEW_COLON3((yyvsp[0].id), &(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: const_decl(p, top_const_field!($2)) %*/
		    }
#line 7014 "parse.c" /* yacc.c:1652  */
    break;

  case 115:
#line 1894 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			rb_backref_error(p, (yyvsp[0].node));
			(yyval.node) = NEW_BEGIN(0, &(yyloc));
		    /*% %*/
		    /*% ripper[error]: assign_error!(var_field(p, $1)) %*/
		    }
#line 7026 "parse.c" /* yacc.c:1652  */
    break;

  case 116:
#line 1904 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			yyerror1(&(yylsp[0]), "class/module name must be CONSTANT");
		    /*% %*/
		    /*% ripper[error]: class_name_error!($1) %*/
		    }
#line 7037 "parse.c" /* yacc.c:1652  */
    break;

  case 118:
#line 1914 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_COLON3((yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: top_const_ref!($2) %*/
		    }
#line 7048 "parse.c" /* yacc.c:1652  */
    break;

  case 119:
#line 1921 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_COLON2(0, (yyval.node), &(yyloc));
		    /*% %*/
		    /*% ripper: const_ref!($1) %*/
		    }
#line 7059 "parse.c" /* yacc.c:1652  */
    break;

  case 120:
#line 1928 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_COLON2((yyvsp[-2].node), (yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: const_path_ref!($1, $3) %*/
		    }
#line 7070 "parse.c" /* yacc.c:1652  */
    break;

  case 124:
#line 1940 "parse.y" /* yacc.c:1652  */
    {
			SET_LEX_STATE(EXPR_ENDFN);
			(yyval.id) = (yyvsp[0].id);
		    }
#line 7079 "parse.c" /* yacc.c:1652  */
    break;

  case 125:
#line 1945 "parse.y" /* yacc.c:1652  */
    {
			SET_LEX_STATE(EXPR_ENDFN);
			(yyval.id) = (yyvsp[0].id);
		    }
#line 7088 "parse.c" /* yacc.c:1652  */
    break;

  case 126:
#line 1952 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_LIT(ID2SYM((yyvsp[0].id)), &(yyloc));
		    /*% %*/
		    /*% ripper: symbol_literal!($1) %*/
		    }
#line 7099 "parse.c" /* yacc.c:1652  */
    break;

  case 128:
#line 1962 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_UNDEF((yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: rb_ary_new3(1, get_value($1)) %*/
		    }
#line 7110 "parse.c" /* yacc.c:1652  */
    break;

  case 129:
#line 1968 "parse.y" /* yacc.c:1652  */
    {SET_LEX_STATE(EXPR_FNAME|EXPR_FITEM);}
#line 7116 "parse.c" /* yacc.c:1652  */
    break;

  case 130:
#line 1969 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			NODE *undef = NEW_UNDEF((yyvsp[0].node), &(yylsp[0]));
			(yyval.node) = block_append(p, (yyvsp[-3].node), undef);
		    /*% %*/
		    /*% ripper: rb_ary_push($1, get_value($4)) %*/
		    }
#line 7128 "parse.c" /* yacc.c:1652  */
    break;

  case 131:
#line 1978 "parse.y" /* yacc.c:1652  */
    { ifndef_ripper((yyval.id) = '|'); }
#line 7134 "parse.c" /* yacc.c:1652  */
    break;

  case 132:
#line 1979 "parse.y" /* yacc.c:1652  */
    { ifndef_ripper((yyval.id) = '^'); }
#line 7140 "parse.c" /* yacc.c:1652  */
    break;

  case 133:
#line 1980 "parse.y" /* yacc.c:1652  */
    { ifndef_ripper((yyval.id) = '&'); }
#line 7146 "parse.c" /* yacc.c:1652  */
    break;

  case 134:
#line 1981 "parse.y" /* yacc.c:1652  */
    { ifndef_ripper((yyval.id) = tCMP); }
#line 7152 "parse.c" /* yacc.c:1652  */
    break;

  case 135:
#line 1982 "parse.y" /* yacc.c:1652  */
    { ifndef_ripper((yyval.id) = tEQ); }
#line 7158 "parse.c" /* yacc.c:1652  */
    break;

  case 136:
#line 1983 "parse.y" /* yacc.c:1652  */
    { ifndef_ripper((yyval.id) = tEQQ); }
#line 7164 "parse.c" /* yacc.c:1652  */
    break;

  case 137:
#line 1984 "parse.y" /* yacc.c:1652  */
    { ifndef_ripper((yyval.id) = tMATCH); }
#line 7170 "parse.c" /* yacc.c:1652  */
    break;

  case 138:
#line 1985 "parse.y" /* yacc.c:1652  */
    { ifndef_ripper((yyval.id) = tNMATCH); }
#line 7176 "parse.c" /* yacc.c:1652  */
    break;

  case 139:
#line 1986 "parse.y" /* yacc.c:1652  */
    { ifndef_ripper((yyval.id) = '>'); }
#line 7182 "parse.c" /* yacc.c:1652  */
    break;

  case 140:
#line 1987 "parse.y" /* yacc.c:1652  */
    { ifndef_ripper((yyval.id) = tGEQ); }
#line 7188 "parse.c" /* yacc.c:1652  */
    break;

  case 141:
#line 1988 "parse.y" /* yacc.c:1652  */
    { ifndef_ripper((yyval.id) = '<'); }
#line 7194 "parse.c" /* yacc.c:1652  */
    break;

  case 142:
#line 1989 "parse.y" /* yacc.c:1652  */
    { ifndef_ripper((yyval.id) = tLEQ); }
#line 7200 "parse.c" /* yacc.c:1652  */
    break;

  case 143:
#line 1990 "parse.y" /* yacc.c:1652  */
    { ifndef_ripper((yyval.id) = tNEQ); }
#line 7206 "parse.c" /* yacc.c:1652  */
    break;

  case 144:
#line 1991 "parse.y" /* yacc.c:1652  */
    { ifndef_ripper((yyval.id) = tLSHFT); }
#line 7212 "parse.c" /* yacc.c:1652  */
    break;

  case 145:
#line 1992 "parse.y" /* yacc.c:1652  */
    { ifndef_ripper((yyval.id) = tRSHFT); }
#line 7218 "parse.c" /* yacc.c:1652  */
    break;

  case 146:
#line 1993 "parse.y" /* yacc.c:1652  */
    { ifndef_ripper((yyval.id) = '+'); }
#line 7224 "parse.c" /* yacc.c:1652  */
    break;

  case 147:
#line 1994 "parse.y" /* yacc.c:1652  */
    { ifndef_ripper((yyval.id) = '-'); }
#line 7230 "parse.c" /* yacc.c:1652  */
    break;

  case 148:
#line 1995 "parse.y" /* yacc.c:1652  */
    { ifndef_ripper((yyval.id) = '*'); }
#line 7236 "parse.c" /* yacc.c:1652  */
    break;

  case 149:
#line 1996 "parse.y" /* yacc.c:1652  */
    { ifndef_ripper((yyval.id) = '*'); }
#line 7242 "parse.c" /* yacc.c:1652  */
    break;

  case 150:
#line 1997 "parse.y" /* yacc.c:1652  */
    { ifndef_ripper((yyval.id) = '/'); }
#line 7248 "parse.c" /* yacc.c:1652  */
    break;

  case 151:
#line 1998 "parse.y" /* yacc.c:1652  */
    { ifndef_ripper((yyval.id) = '%'); }
#line 7254 "parse.c" /* yacc.c:1652  */
    break;

  case 152:
#line 1999 "parse.y" /* yacc.c:1652  */
    { ifndef_ripper((yyval.id) = tPOW); }
#line 7260 "parse.c" /* yacc.c:1652  */
    break;

  case 153:
#line 2000 "parse.y" /* yacc.c:1652  */
    { ifndef_ripper((yyval.id) = tDSTAR); }
#line 7266 "parse.c" /* yacc.c:1652  */
    break;

  case 154:
#line 2001 "parse.y" /* yacc.c:1652  */
    { ifndef_ripper((yyval.id) = '!'); }
#line 7272 "parse.c" /* yacc.c:1652  */
    break;

  case 155:
#line 2002 "parse.y" /* yacc.c:1652  */
    { ifndef_ripper((yyval.id) = '~'); }
#line 7278 "parse.c" /* yacc.c:1652  */
    break;

  case 156:
#line 2003 "parse.y" /* yacc.c:1652  */
    { ifndef_ripper((yyval.id) = tUPLUS); }
#line 7284 "parse.c" /* yacc.c:1652  */
    break;

  case 157:
#line 2004 "parse.y" /* yacc.c:1652  */
    { ifndef_ripper((yyval.id) = tUMINUS); }
#line 7290 "parse.c" /* yacc.c:1652  */
    break;

  case 158:
#line 2005 "parse.y" /* yacc.c:1652  */
    { ifndef_ripper((yyval.id) = tAREF); }
#line 7296 "parse.c" /* yacc.c:1652  */
    break;

  case 159:
#line 2006 "parse.y" /* yacc.c:1652  */
    { ifndef_ripper((yyval.id) = tASET); }
#line 7302 "parse.c" /* yacc.c:1652  */
    break;

  case 160:
#line 2007 "parse.y" /* yacc.c:1652  */
    { ifndef_ripper((yyval.id) = '`'); }
#line 7308 "parse.c" /* yacc.c:1652  */
    break;

  case 202:
#line 2025 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = node_assign(p, (yyvsp[-2].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: assign!($1, $3) %*/
		    }
#line 7319 "parse.c" /* yacc.c:1652  */
    break;

  case 203:
#line 2032 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = new_op_assign(p, (yyvsp[-2].node), (yyvsp[-1].id), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: opassign!($1, $2, $3) %*/
		    }
#line 7330 "parse.c" /* yacc.c:1652  */
    break;

  case 204:
#line 2039 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			value_expr((yyvsp[0].node));
			(yyval.node) = new_ary_op_assign(p, (yyvsp[-5].node), (yyvsp[-3].node), (yyvsp[-1].id), (yyvsp[0].node), &(yylsp[-3]), &(yyloc));
		    /*% %*/
		    /*% ripper: opassign!(aref_field!($1, escape_Qundef($3)), $5, $6) %*/
		    }
#line 7342 "parse.c" /* yacc.c:1652  */
    break;

  case 205:
#line 2047 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			value_expr((yyvsp[0].node));
			(yyval.node) = new_attr_op_assign(p, (yyvsp[-4].node), (yyvsp[-3].id), (yyvsp[-2].id), (yyvsp[-1].id), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: opassign!(field!($1, $2, $3), $4, $5) %*/
		    }
#line 7354 "parse.c" /* yacc.c:1652  */
    break;

  case 206:
#line 2055 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			value_expr((yyvsp[0].node));
			(yyval.node) = new_attr_op_assign(p, (yyvsp[-4].node), (yyvsp[-3].id), (yyvsp[-2].id), (yyvsp[-1].id), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: opassign!(field!($1, $2, $3), $4, $5) %*/
		    }
#line 7366 "parse.c" /* yacc.c:1652  */
    break;

  case 207:
#line 2063 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			value_expr((yyvsp[0].node));
			(yyval.node) = new_attr_op_assign(p, (yyvsp[-4].node), ID2VAL(idCOLON2), (yyvsp[-2].id), (yyvsp[-1].id), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: opassign!(field!($1, ID2VAL(idCOLON2), $3), $4, $5) %*/
		    }
#line 7378 "parse.c" /* yacc.c:1652  */
    break;

  case 208:
#line 2071 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			YYLTYPE loc = code_loc_gen(&(yylsp[-4]), &(yylsp[-2]));
			(yyval.node) = new_const_op_assign(p, NEW_COLON2((yyvsp[-4].node), (yyvsp[-2].id), &loc), (yyvsp[-1].id), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: opassign!(const_path_field!($1, $3), $4, $5) %*/
		    }
#line 7390 "parse.c" /* yacc.c:1652  */
    break;

  case 209:
#line 2079 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = new_const_op_assign(p, NEW_COLON3((yyvsp[-2].id), &(yyloc)), (yyvsp[-1].id), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: opassign!(top_const_field!($2), $3, $4) %*/
		    }
#line 7401 "parse.c" /* yacc.c:1652  */
    break;

  case 210:
#line 2086 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			rb_backref_error(p, (yyvsp[-2].node));
			(yyval.node) = NEW_BEGIN(0, &(yyloc));
		    /*% %*/
		    /*% ripper[error]: assign_error!(opassign!(var_field(p, $1), $2, $3)) %*/
		    }
#line 7413 "parse.c" /* yacc.c:1652  */
    break;

  case 211:
#line 2094 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			value_expr((yyvsp[-2].node));
			value_expr((yyvsp[0].node));
			(yyval.node) = NEW_DOT2((yyvsp[-2].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: dot2!($1, $3) %*/
		    }
#line 7426 "parse.c" /* yacc.c:1652  */
    break;

  case 212:
#line 2103 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			value_expr((yyvsp[-2].node));
			value_expr((yyvsp[0].node));
			(yyval.node) = NEW_DOT3((yyvsp[-2].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: dot3!($1, $3) %*/
		    }
#line 7439 "parse.c" /* yacc.c:1652  */
    break;

  case 213:
#line 2112 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
                        YYLTYPE loc;
                        loc.beg_pos = (yylsp[0]).end_pos;
                        loc.end_pos = (yylsp[0]).end_pos;

			value_expr((yyvsp[-1].node));
			(yyval.node) = NEW_DOT2((yyvsp[-1].node), new_nil(&loc), &(yyloc));
		    /*% %*/
		    /*% ripper: dot2!($1, Qnil) %*/
		    }
#line 7455 "parse.c" /* yacc.c:1652  */
    break;

  case 214:
#line 2124 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
                        YYLTYPE loc;
                        loc.beg_pos = (yylsp[0]).end_pos;
                        loc.end_pos = (yylsp[0]).end_pos;

			value_expr((yyvsp[-1].node));
			(yyval.node) = NEW_DOT3((yyvsp[-1].node), new_nil(&loc), &(yyloc));
		    /*% %*/
		    /*% ripper: dot3!($1, Qnil) %*/
		    }
#line 7471 "parse.c" /* yacc.c:1652  */
    break;

  case 215:
#line 2136 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
                        YYLTYPE loc;
                        loc.beg_pos = (yylsp[-1]).beg_pos;
                        loc.end_pos = (yylsp[-1]).beg_pos;

			value_expr((yyvsp[0].node));
			(yyval.node) = NEW_DOT2(new_nil(&loc), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: dot2!(Qnil, $2) %*/
		    }
#line 7487 "parse.c" /* yacc.c:1652  */
    break;

  case 216:
#line 2148 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
                        YYLTYPE loc;
                        loc.beg_pos = (yylsp[-1]).beg_pos;
                        loc.end_pos = (yylsp[-1]).beg_pos;

			value_expr((yyvsp[0].node));
			(yyval.node) = NEW_DOT3(new_nil(&loc), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: dot3!(Qnil, $2) %*/
		    }
#line 7503 "parse.c" /* yacc.c:1652  */
    break;

  case 217:
#line 2160 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), '+', (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7511 "parse.c" /* yacc.c:1652  */
    break;

  case 218:
#line 2164 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), '-', (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7519 "parse.c" /* yacc.c:1652  */
    break;

  case 219:
#line 2168 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), '*', (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7527 "parse.c" /* yacc.c:1652  */
    break;

  case 220:
#line 2172 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), '/', (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7535 "parse.c" /* yacc.c:1652  */
    break;

  case 221:
#line 2176 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), '%', (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7543 "parse.c" /* yacc.c:1652  */
    break;

  case 222:
#line 2180 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), idPow, (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7551 "parse.c" /* yacc.c:1652  */
    break;

  case 223:
#line 2184 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = call_uni_op(p, call_bin_op(p, (yyvsp[-2].node), idPow, (yyvsp[0].node), &(yylsp[-2]), &(yyloc)), idUMinus, &(yylsp[-3]), &(yyloc));
		    }
#line 7559 "parse.c" /* yacc.c:1652  */
    break;

  case 224:
#line 2188 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = call_uni_op(p, (yyvsp[0].node), idUPlus, &(yylsp[-1]), &(yyloc));
		    }
#line 7567 "parse.c" /* yacc.c:1652  */
    break;

  case 225:
#line 2192 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = call_uni_op(p, (yyvsp[0].node), idUMinus, &(yylsp[-1]), &(yyloc));
		    }
#line 7575 "parse.c" /* yacc.c:1652  */
    break;

  case 226:
#line 2196 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), '|', (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7583 "parse.c" /* yacc.c:1652  */
    break;

  case 227:
#line 2200 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), '^', (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7591 "parse.c" /* yacc.c:1652  */
    break;

  case 228:
#line 2204 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), '&', (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7599 "parse.c" /* yacc.c:1652  */
    break;

  case 229:
#line 2208 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), idCmp, (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7607 "parse.c" /* yacc.c:1652  */
    break;

  case 231:
#line 2213 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), idEq, (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7615 "parse.c" /* yacc.c:1652  */
    break;

  case 232:
#line 2217 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), idEqq, (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7623 "parse.c" /* yacc.c:1652  */
    break;

  case 233:
#line 2221 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), idNeq, (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7631 "parse.c" /* yacc.c:1652  */
    break;

  case 234:
#line 2225 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = match_op(p, (yyvsp[-2].node), (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7639 "parse.c" /* yacc.c:1652  */
    break;

  case 235:
#line 2229 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), idNeqTilde, (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7647 "parse.c" /* yacc.c:1652  */
    break;

  case 236:
#line 2233 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = call_uni_op(p, method_cond(p, (yyvsp[0].node), &(yylsp[0])), '!', &(yylsp[-1]), &(yyloc));
		    }
#line 7655 "parse.c" /* yacc.c:1652  */
    break;

  case 237:
#line 2237 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = call_uni_op(p, (yyvsp[0].node), '~', &(yylsp[-1]), &(yyloc));
		    }
#line 7663 "parse.c" /* yacc.c:1652  */
    break;

  case 238:
#line 2241 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), idLTLT, (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7671 "parse.c" /* yacc.c:1652  */
    break;

  case 239:
#line 2245 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), idGTGT, (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7679 "parse.c" /* yacc.c:1652  */
    break;

  case 240:
#line 2249 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = logop(p, idANDOP, (yyvsp[-2].node), (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7687 "parse.c" /* yacc.c:1652  */
    break;

  case 241:
#line 2253 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = logop(p, idOROP, (yyvsp[-2].node), (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7695 "parse.c" /* yacc.c:1652  */
    break;

  case 242:
#line 2256 "parse.y" /* yacc.c:1652  */
    {p->in_defined = 1;}
#line 7701 "parse.c" /* yacc.c:1652  */
    break;

  case 243:
#line 2257 "parse.y" /* yacc.c:1652  */
    {
			p->in_defined = 0;
			(yyval.node) = new_defined(p, (yyvsp[0].node), &(yyloc));
		    }
#line 7710 "parse.c" /* yacc.c:1652  */
    break;

  case 244:
#line 2262 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			value_expr((yyvsp[-5].node));
			(yyval.node) = new_if(p, (yyvsp[-5].node), (yyvsp[-3].node), (yyvsp[0].node), &(yyloc));
			fixpos((yyval.node), (yyvsp[-5].node));
		    /*% %*/
		    /*% ripper: ifop!($1, $3, $6) %*/
		    }
#line 7723 "parse.c" /* yacc.c:1652  */
    break;

  case 245:
#line 2271 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = (yyvsp[0].node);
		    }
#line 7731 "parse.c" /* yacc.c:1652  */
    break;

  case 246:
#line 2276 "parse.y" /* yacc.c:1652  */
    {(yyval.id) = '>';}
#line 7737 "parse.c" /* yacc.c:1652  */
    break;

  case 247:
#line 2277 "parse.y" /* yacc.c:1652  */
    {(yyval.id) = '<';}
#line 7743 "parse.c" /* yacc.c:1652  */
    break;

  case 248:
#line 2278 "parse.y" /* yacc.c:1652  */
    {(yyval.id) = idGE;}
#line 7749 "parse.c" /* yacc.c:1652  */
    break;

  case 249:
#line 2279 "parse.y" /* yacc.c:1652  */
    {(yyval.id) = idLE;}
#line 7755 "parse.c" /* yacc.c:1652  */
    break;

  case 250:
#line 2283 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), (yyvsp[-1].id), (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7763 "parse.c" /* yacc.c:1652  */
    break;

  case 251:
#line 2287 "parse.y" /* yacc.c:1652  */
    {
			rb_warning1("comparison '%s' after comparison", WARN_ID((yyvsp[-1].id)));
			(yyval.node) = call_bin_op(p, (yyvsp[-2].node), (yyvsp[-1].id), (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    }
#line 7772 "parse.c" /* yacc.c:1652  */
    break;

  case 252:
#line 2294 "parse.y" /* yacc.c:1652  */
    {
			value_expr((yyvsp[0].node));
			(yyval.node) = (yyvsp[0].node);
		    }
#line 7781 "parse.c" /* yacc.c:1652  */
    break;

  case 254:
#line 2302 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = (yyvsp[-1].node);
		    }
#line 7789 "parse.c" /* yacc.c:1652  */
    break;

  case 255:
#line 2306 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node) ? arg_append(p, (yyvsp[-3].node), new_hash(p, (yyvsp[-1].node), &(yylsp[-1])), &(yyloc)) : (yyvsp[-3].node);
		    /*% %*/
		    /*% ripper: args_add!($1, bare_assoc_hash!($3)) %*/
		    }
#line 7800 "parse.c" /* yacc.c:1652  */
    break;

  case 256:
#line 2313 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node) ? NEW_LIST(new_hash(p, (yyvsp[-1].node), &(yylsp[-1])), &(yyloc)) : 0;
		    /*% %*/
		    /*% ripper: args_add!(args_new!, bare_assoc_hash!($1)) %*/
		    }
#line 7811 "parse.c" /* yacc.c:1652  */
    break;

  case 257:
#line 2322 "parse.y" /* yacc.c:1652  */
    {
			value_expr((yyvsp[0].node));
			(yyval.node) = (yyvsp[0].node);
		    }
#line 7820 "parse.c" /* yacc.c:1652  */
    break;

  case 258:
#line 2327 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			YYLTYPE loc = code_loc_gen(&(yylsp[-1]), &(yylsp[0]));
			value_expr((yyvsp[-2].node));
			(yyval.node) = NEW_RESCUE((yyvsp[-2].node), NEW_RESBODY(0, remove_begin((yyvsp[0].node)), 0, &loc), 0, &(yyloc));
		    /*% %*/
		    /*% ripper: rescue_mod!($1, $3) %*/
		    }
#line 7833 "parse.c" /* yacc.c:1652  */
    break;

  case 259:
#line 2338 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node);
		    /*% %*/
		    /*% ripper: arg_paren!(escape_Qundef($2)) %*/
		    }
#line 7844 "parse.c" /* yacc.c:1652  */
    break;

  case 264:
#line 2353 "parse.y" /* yacc.c:1652  */
    {
		      (yyval.node) = (yyvsp[-1].node);
		    }
#line 7852 "parse.c" /* yacc.c:1652  */
    break;

  case 265:
#line 2357 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node) ? arg_append(p, (yyvsp[-3].node), new_hash(p, (yyvsp[-1].node), &(yylsp[-1])), &(yyloc)) : (yyvsp[-3].node);
		    /*% %*/
		    /*% ripper: args_add!($1, bare_assoc_hash!($3)) %*/
		    }
#line 7863 "parse.c" /* yacc.c:1652  */
    break;

  case 266:
#line 2364 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node) ? NEW_LIST(new_hash(p, (yyvsp[-1].node), &(yylsp[-1])), &(yylsp[-1])) : 0;
		    /*% %*/
		    /*% ripper: args_add!(args_new!, bare_assoc_hash!($1)) %*/
		    }
#line 7874 "parse.c" /* yacc.c:1652  */
    break;

  case 267:
#line 2373 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			value_expr((yyvsp[0].node));
			(yyval.node) = NEW_LIST((yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: args_add!(args_new!, $1) %*/
		    }
#line 7886 "parse.c" /* yacc.c:1652  */
    break;

  case 268:
#line 2381 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = arg_blk_pass((yyvsp[-1].node), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: args_add_block!($1, $2) %*/
		    }
#line 7897 "parse.c" /* yacc.c:1652  */
    break;

  case 269:
#line 2388 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node) ? NEW_LIST(new_hash(p, (yyvsp[-1].node), &(yylsp[-1])), &(yylsp[-1])) : 0;
			(yyval.node) = arg_blk_pass((yyval.node), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: args_add_block!(args_add!(args_new!, bare_assoc_hash!($1)), $2) %*/
		    }
#line 7909 "parse.c" /* yacc.c:1652  */
    break;

  case 270:
#line 2396 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node) ? arg_append(p, (yyvsp[-3].node), new_hash(p, (yyvsp[-1].node), &(yylsp[-1])), &(yyloc)) : (yyvsp[-3].node);
			(yyval.node) = arg_blk_pass((yyval.node), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: args_add_block!(args_add!($1, bare_assoc_hash!($3)), $4) %*/
		    }
#line 7921 "parse.c" /* yacc.c:1652  */
    break;

  case 272:
#line 2407 "parse.y" /* yacc.c:1652  */
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
#line 7943 "parse.c" /* yacc.c:1652  */
    break;

  case 273:
#line 2425 "parse.y" /* yacc.c:1652  */
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
#line 7965 "parse.c" /* yacc.c:1652  */
    break;

  case 274:
#line 2445 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_BLOCK_PASS((yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: $2 %*/
		    }
#line 7976 "parse.c" /* yacc.c:1652  */
    break;

  case 275:
#line 2454 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = (yyvsp[0].node);
		    }
#line 7984 "parse.c" /* yacc.c:1652  */
    break;

  case 276:
#line 2458 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = 0;
		    }
#line 7992 "parse.c" /* yacc.c:1652  */
    break;

  case 277:
#line 2464 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_LIST((yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: args_add!(args_new!, $1) %*/
		    }
#line 8003 "parse.c" /* yacc.c:1652  */
    break;

  case 278:
#line 2471 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_SPLAT((yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: args_add_star!(args_new!, $2) %*/
		    }
#line 8014 "parse.c" /* yacc.c:1652  */
    break;

  case 279:
#line 2478 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = last_arg_append(p, (yyvsp[-2].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: args_add!($1, $3) %*/
		    }
#line 8025 "parse.c" /* yacc.c:1652  */
    break;

  case 280:
#line 2485 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = rest_arg_append(p, (yyvsp[-3].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: args_add_star!($1, $4) %*/
		    }
#line 8036 "parse.c" /* yacc.c:1652  */
    break;

  case 283:
#line 2498 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = last_arg_append(p, (yyvsp[-2].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: mrhs_add!(mrhs_new_from_args!($1), $3) %*/
		    }
#line 8047 "parse.c" /* yacc.c:1652  */
    break;

  case 284:
#line 2505 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = rest_arg_append(p, (yyvsp[-3].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: mrhs_add_star!(mrhs_new_from_args!($1), $4) %*/
		    }
#line 8058 "parse.c" /* yacc.c:1652  */
    break;

  case 285:
#line 2512 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_SPLAT((yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: mrhs_add_star!(mrhs_new!, $2) %*/
		    }
#line 8069 "parse.c" /* yacc.c:1652  */
    break;

  case 296:
#line 2531 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_FCALL((yyvsp[0].id), 0, &(yyloc));
		    /*% %*/
		    /*% ripper: method_add_arg!(fcall!($1), args_new!) %*/
		    }
#line 8080 "parse.c" /* yacc.c:1652  */
    break;

  case 297:
#line 2538 "parse.y" /* yacc.c:1652  */
    {
			CMDARG_PUSH(0);
		    }
#line 8088 "parse.c" /* yacc.c:1652  */
    break;

  case 298:
#line 2543 "parse.y" /* yacc.c:1652  */
    {
			CMDARG_POP();
		    /*%%%*/
			set_line_body((yyvsp[-1].node), (yylsp[-3]).end_pos.lineno);
			(yyval.node) = NEW_BEGIN((yyvsp[-1].node), &(yyloc));
			nd_set_line((yyval.node), (yylsp[-3]).end_pos.lineno);
		    /*% %*/
		    /*% ripper: begin!($3) %*/
		    }
#line 8102 "parse.c" /* yacc.c:1652  */
    break;

  case 299:
#line 2552 "parse.y" /* yacc.c:1652  */
    {SET_LEX_STATE(EXPR_ENDARG);}
#line 8108 "parse.c" /* yacc.c:1652  */
    break;

  case 300:
#line 2553 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_BEGIN(0, &(yyloc));
		    /*% %*/
		    /*% ripper: paren!(0) %*/
		    }
#line 8119 "parse.c" /* yacc.c:1652  */
    break;

  case 301:
#line 2559 "parse.y" /* yacc.c:1652  */
    {SET_LEX_STATE(EXPR_ENDARG);}
#line 8125 "parse.c" /* yacc.c:1652  */
    break;

  case 302:
#line 2560 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-2].node);
		    /*% %*/
		    /*% ripper: paren!($2) %*/
		    }
#line 8136 "parse.c" /* yacc.c:1652  */
    break;

  case 303:
#line 2567 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node);
		    /*% %*/
		    /*% ripper: paren!($2) %*/
		    }
#line 8147 "parse.c" /* yacc.c:1652  */
    break;

  case 304:
#line 2574 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_COLON2((yyvsp[-2].node), (yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: const_path_ref!($1, $3) %*/
		    }
#line 8158 "parse.c" /* yacc.c:1652  */
    break;

  case 305:
#line 2581 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_COLON3((yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: top_const_ref!($2) %*/
		    }
#line 8169 "parse.c" /* yacc.c:1652  */
    break;

  case 306:
#line 2588 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = make_array((yyvsp[-1].node), &(yyloc));
		    /*% %*/
		    /*% ripper: array!(escape_Qundef($2)) %*/
		    }
#line 8180 "parse.c" /* yacc.c:1652  */
    break;

  case 307:
#line 2595 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = new_hash(p, (yyvsp[-1].node), &(yyloc));
			(yyval.node)->nd_alen = TRUE;
		    /*% %*/
		    /*% ripper: hash!(escape_Qundef($2)) %*/
		    }
#line 8192 "parse.c" /* yacc.c:1652  */
    break;

  case 308:
#line 2603 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_RETURN(0, &(yyloc));
		    /*% %*/
		    /*% ripper: return0! %*/
		    }
#line 8203 "parse.c" /* yacc.c:1652  */
    break;

  case 309:
#line 2610 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = new_yield(p, (yyvsp[-1].node), &(yyloc));
		    /*% %*/
		    /*% ripper: yield!(paren!($3)) %*/
		    }
#line 8214 "parse.c" /* yacc.c:1652  */
    break;

  case 310:
#line 2617 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_YIELD(0, &(yyloc));
		    /*% %*/
		    /*% ripper: yield!(paren!(args_new!)) %*/
		    }
#line 8225 "parse.c" /* yacc.c:1652  */
    break;

  case 311:
#line 2624 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_YIELD(0, &(yyloc));
		    /*% %*/
		    /*% ripper: yield0! %*/
		    }
#line 8236 "parse.c" /* yacc.c:1652  */
    break;

  case 312:
#line 2630 "parse.y" /* yacc.c:1652  */
    {p->in_defined = 1;}
#line 8242 "parse.c" /* yacc.c:1652  */
    break;

  case 313:
#line 2631 "parse.y" /* yacc.c:1652  */
    {
			p->in_defined = 0;
			(yyval.node) = new_defined(p, (yyvsp[-1].node), &(yyloc));
		    }
#line 8251 "parse.c" /* yacc.c:1652  */
    break;

  case 314:
#line 2636 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = call_uni_op(p, method_cond(p, (yyvsp[-1].node), &(yylsp[-1])), METHOD_NOT, &(yylsp[-3]), &(yyloc));
		    }
#line 8259 "parse.c" /* yacc.c:1652  */
    break;

  case 315:
#line 2640 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = call_uni_op(p, method_cond(p, new_nil(&(yylsp[-1])), &(yylsp[-1])), METHOD_NOT, &(yylsp[-2]), &(yyloc));
		    }
#line 8267 "parse.c" /* yacc.c:1652  */
    break;

  case 316:
#line 2644 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = method_add_block(p, (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: method_add_block!(method_add_arg!(fcall!($1), args_new!), $2) %*/
		    }
#line 8278 "parse.c" /* yacc.c:1652  */
    break;

  case 318:
#line 2652 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			block_dup_check(p, (yyvsp[-1].node)->nd_args, (yyvsp[0].node));
			(yyval.node) = method_add_block(p, (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: method_add_block!($1, $2) %*/
		    }
#line 8290 "parse.c" /* yacc.c:1652  */
    break;

  case 319:
#line 2660 "parse.y" /* yacc.c:1652  */
    {
			token_info_push(p, "->", &(yylsp[0]));
		    }
#line 8298 "parse.c" /* yacc.c:1652  */
    break;

  case 320:
#line 2664 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = (yyvsp[0].node);
                    /*%%%*/
                        nd_set_first_loc((yyval.node), (yylsp[-2]).beg_pos);
                    /*% %*/
		    }
#line 8309 "parse.c" /* yacc.c:1652  */
    break;

  case 321:
#line 2674 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = new_if(p, (yyvsp[-4].node), (yyvsp[-2].node), (yyvsp[-1].node), &(yyloc));
			fixpos((yyval.node), (yyvsp[-4].node));
		    /*% %*/
		    /*% ripper: if!($2, $4, escape_Qundef($5)) %*/
		    }
#line 8321 "parse.c" /* yacc.c:1652  */
    break;

  case 322:
#line 2685 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = new_unless(p, (yyvsp[-4].node), (yyvsp[-2].node), (yyvsp[-1].node), &(yyloc));
			fixpos((yyval.node), (yyvsp[-4].node));
		    /*% %*/
		    /*% ripper: unless!($2, $4, escape_Qundef($5)) %*/
		    }
#line 8333 "parse.c" /* yacc.c:1652  */
    break;

  case 323:
#line 2695 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_WHILE(cond(p, (yyvsp[-2].node), &(yylsp[-2])), (yyvsp[-1].node), 1, &(yyloc));
			fixpos((yyval.node), (yyvsp[-2].node));
		    /*% %*/
		    /*% ripper: while!($2, $3) %*/
		    }
#line 8345 "parse.c" /* yacc.c:1652  */
    break;

  case 324:
#line 2705 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_UNTIL(cond(p, (yyvsp[-2].node), &(yylsp[-2])), (yyvsp[-1].node), 1, &(yyloc));
			fixpos((yyval.node), (yyvsp[-2].node));
		    /*% %*/
		    /*% ripper: until!($2, $3) %*/
		    }
#line 8357 "parse.c" /* yacc.c:1652  */
    break;

  case 325:
#line 2713 "parse.y" /* yacc.c:1652  */
    {
			(yyval.val) = p->case_labels;
			p->case_labels = Qnil;
		    }
#line 8366 "parse.c" /* yacc.c:1652  */
    break;

  case 326:
#line 2719 "parse.y" /* yacc.c:1652  */
    {
			if (RTEST(p->case_labels)) rb_hash_clear(p->case_labels);
			p->case_labels = (yyvsp[-2].val);
		    /*%%%*/
			(yyval.node) = NEW_CASE((yyvsp[-4].node), (yyvsp[-1].node), &(yyloc));
			fixpos((yyval.node), (yyvsp[-4].node));
		    /*% %*/
		    /*% ripper: case!($2, $5) %*/
		    }
#line 8380 "parse.c" /* yacc.c:1652  */
    break;

  case 327:
#line 2729 "parse.y" /* yacc.c:1652  */
    {
			(yyval.val) = p->case_labels;
			p->case_labels = 0;
		    }
#line 8389 "parse.c" /* yacc.c:1652  */
    break;

  case 328:
#line 2735 "parse.y" /* yacc.c:1652  */
    {
			if (RTEST(p->case_labels)) rb_hash_clear(p->case_labels);
			p->case_labels = (yyvsp[-2].val);
		    /*%%%*/
			(yyval.node) = NEW_CASE2((yyvsp[-1].node), &(yyloc));
		    /*% %*/
		    /*% ripper: case!(Qnil, $4) %*/
		    }
#line 8402 "parse.c" /* yacc.c:1652  */
    break;

  case 329:
#line 2746 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_CASE3((yyvsp[-3].node), (yyvsp[-1].node), &(yyloc));
			rb_warn0L(nd_line((yyval.node)), "Pattern matching is experimental, and the behavior may change in future versions of Ruby!");
		    /*% %*/
		    /*% ripper: case!($2, $4) %*/
		    }
#line 8414 "parse.c" /* yacc.c:1652  */
    break;

  case 330:
#line 2756 "parse.y" /* yacc.c:1652  */
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
			rb_imemo_tmpbuf_t *tmpbuf = new_tmpbuf();
			ID *tbl = ALLOC_N(ID, 2);
			tbl[0] = 1 /* length of local var table */; tbl[1] = id /* internal id */;
			tmpbuf->ptr = (VALUE *)tbl;

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
#line 8461 "parse.c" /* yacc.c:1652  */
    break;

  case 331:
#line 2799 "parse.y" /* yacc.c:1652  */
    {
			if (p->in_def) {
			    YYLTYPE loc = code_loc_gen(&(yylsp[-2]), &(yylsp[-1]));
			    yyerror1(&loc, "class definition in method body");
			}
			(yyvsp[-2].num) = p->in_class;
			p->in_class = 1;
			local_push(p, 0);
		    }
#line 8475 "parse.c" /* yacc.c:1652  */
    break;

  case 332:
#line 2810 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_CLASS((yyvsp[-4].node), (yyvsp[-1].node), (yyvsp[-3].node), &(yyloc));
			nd_set_line((yyval.node)->nd_body, (yylsp[0]).end_pos.lineno);
			set_line_body((yyvsp[-1].node), (yylsp[-3]).end_pos.lineno);
			nd_set_line((yyval.node), (yylsp[-3]).end_pos.lineno);
		    /*% %*/
		    /*% ripper: class!($2, $3, $5) %*/
			local_pop(p);
			p->in_class = (yyvsp[-5].num) & 1;
		    }
#line 8491 "parse.c" /* yacc.c:1652  */
    break;

  case 333:
#line 2822 "parse.y" /* yacc.c:1652  */
    {
			(yyval.num) = (p->in_class << 1) | p->in_def;
			p->in_def = 0;
			p->in_class = 0;
			local_push(p, 0);
		    }
#line 8502 "parse.c" /* yacc.c:1652  */
    break;

  case 334:
#line 2831 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_SCLASS((yyvsp[-4].node), (yyvsp[-1].node), &(yyloc));
			nd_set_line((yyval.node)->nd_body, (yylsp[0]).end_pos.lineno);
			set_line_body((yyvsp[-1].node), nd_line((yyvsp[-4].node)));
			fixpos((yyval.node), (yyvsp[-4].node));
		    /*% %*/
		    /*% ripper: sclass!($3, $6) %*/
			local_pop(p);
			p->in_def = (yyvsp[-3].num) & 1;
			p->in_class = ((yyvsp[-3].num) >> 1) & 1;
		    }
#line 8519 "parse.c" /* yacc.c:1652  */
    break;

  case 335:
#line 2844 "parse.y" /* yacc.c:1652  */
    {
			if (p->in_def) {
			    YYLTYPE loc = code_loc_gen(&(yylsp[-1]), &(yylsp[0]));
			    yyerror1(&loc, "module definition in method body");
			}
			(yyvsp[-1].num) = p->in_class;
			p->in_class = 1;
			local_push(p, 0);
		    }
#line 8533 "parse.c" /* yacc.c:1652  */
    break;

  case 336:
#line 2855 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MODULE((yyvsp[-3].node), (yyvsp[-1].node), &(yyloc));
			nd_set_line((yyval.node)->nd_body, (yylsp[0]).end_pos.lineno);
			set_line_body((yyvsp[-1].node), (yylsp[-3]).end_pos.lineno);
			nd_set_line((yyval.node), (yylsp[-3]).end_pos.lineno);
		    /*% %*/
		    /*% ripper: module!($2, $4) %*/
			local_pop(p);
			p->in_class = (yyvsp[-4].num) & 1;
		    }
#line 8549 "parse.c" /* yacc.c:1652  */
    break;

  case 337:
#line 2867 "parse.y" /* yacc.c:1652  */
    {
			local_push(p, 0);
			(yyval.id) = p->cur_arg;
			p->cur_arg = 0;
		    }
#line 8559 "parse.c" /* yacc.c:1652  */
    break;

  case 338:
#line 2872 "parse.y" /* yacc.c:1652  */
    {
			(yyval.num) = p->in_def;
			p->in_def = 1;
		    }
#line 8568 "parse.c" /* yacc.c:1652  */
    break;

  case 339:
#line 2879 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			NODE *body = remove_begin((yyvsp[-1].node));
			reduce_nodes(p, &body);
			(yyval.node) = NEW_DEFN((yyvsp[-5].id), (yyvsp[-2].node), body, &(yyloc));
			nd_set_line((yyval.node)->nd_defn, (yylsp[0]).end_pos.lineno);
			set_line_body(body, (yylsp[-6]).beg_pos.lineno);
		    /*% %*/
		    /*% ripper: def!($2, $5, $6) %*/
			local_pop(p);
			p->in_def = (yyvsp[-3].num) & 1;
			p->cur_arg = (yyvsp[-4].id);
		    }
#line 8586 "parse.c" /* yacc.c:1652  */
    break;

  case 340:
#line 2892 "parse.y" /* yacc.c:1652  */
    {SET_LEX_STATE(EXPR_FNAME);}
#line 8592 "parse.c" /* yacc.c:1652  */
    break;

  case 341:
#line 2893 "parse.y" /* yacc.c:1652  */
    {
			(yyvsp[-1].num) = p->in_def;
			p->in_def = 1;
			SET_LEX_STATE(EXPR_ENDFN|EXPR_LABEL); /* force for args */
			local_push(p, 0);
			(yyval.id) = p->cur_arg;
			p->cur_arg = 0;
		    }
#line 8605 "parse.c" /* yacc.c:1652  */
    break;

  case 342:
#line 2904 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			NODE *body = remove_begin((yyvsp[-1].node));
			reduce_nodes(p, &body);
			(yyval.node) = NEW_DEFS((yyvsp[-7].node), (yyvsp[-4].id), (yyvsp[-2].node), body, &(yyloc));
			nd_set_line((yyval.node)->nd_defn, (yylsp[0]).end_pos.lineno);
			set_line_body(body, (yylsp[-8]).beg_pos.lineno);
		    /*% %*/
		    /*% ripper: defs!($2, $3, $5, $7, $8) %*/
			local_pop(p);
			p->in_def = (yyvsp[-5].num) & 1;
			p->cur_arg = (yyvsp[-3].id);
		    }
#line 8623 "parse.c" /* yacc.c:1652  */
    break;

  case 343:
#line 2918 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_BREAK(0, &(yyloc));
		    /*% %*/
		    /*% ripper: break!(args_new!) %*/
		    }
#line 8634 "parse.c" /* yacc.c:1652  */
    break;

  case 344:
#line 2925 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_NEXT(0, &(yyloc));
		    /*% %*/
		    /*% ripper: next!(args_new!) %*/
		    }
#line 8645 "parse.c" /* yacc.c:1652  */
    break;

  case 345:
#line 2932 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_REDO(&(yyloc));
		    /*% %*/
		    /*% ripper: redo! %*/
		    }
#line 8656 "parse.c" /* yacc.c:1652  */
    break;

  case 346:
#line 2939 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_RETRY(&(yyloc));
		    /*% %*/
		    /*% ripper: retry! %*/
		    }
#line 8667 "parse.c" /* yacc.c:1652  */
    break;

  case 347:
#line 2946 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_METHREF((yyvsp[-2].node), (yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: methref!($1, $3) %*/
		    }
#line 8678 "parse.c" /* yacc.c:1652  */
    break;

  case 348:
#line 2955 "parse.y" /* yacc.c:1652  */
    {
			value_expr((yyvsp[0].node));
			(yyval.node) = (yyvsp[0].node);
		    }
#line 8687 "parse.c" /* yacc.c:1652  */
    break;

  case 349:
#line 2962 "parse.y" /* yacc.c:1652  */
    {
			token_info_push(p, "begin", &(yyloc));
		    }
#line 8695 "parse.c" /* yacc.c:1652  */
    break;

  case 350:
#line 2968 "parse.y" /* yacc.c:1652  */
    {
			token_info_push(p, "if", &(yyloc));
		    }
#line 8703 "parse.c" /* yacc.c:1652  */
    break;

  case 351:
#line 2974 "parse.y" /* yacc.c:1652  */
    {
			token_info_push(p, "unless", &(yyloc));
		    }
#line 8711 "parse.c" /* yacc.c:1652  */
    break;

  case 352:
#line 2980 "parse.y" /* yacc.c:1652  */
    {
			token_info_push(p, "while", &(yyloc));
		    }
#line 8719 "parse.c" /* yacc.c:1652  */
    break;

  case 353:
#line 2986 "parse.y" /* yacc.c:1652  */
    {
			token_info_push(p, "until", &(yyloc));
		    }
#line 8727 "parse.c" /* yacc.c:1652  */
    break;

  case 354:
#line 2992 "parse.y" /* yacc.c:1652  */
    {
			token_info_push(p, "case", &(yyloc));
		    }
#line 8735 "parse.c" /* yacc.c:1652  */
    break;

  case 355:
#line 2998 "parse.y" /* yacc.c:1652  */
    {
			token_info_push(p, "for", &(yyloc));
		    }
#line 8743 "parse.c" /* yacc.c:1652  */
    break;

  case 356:
#line 3004 "parse.y" /* yacc.c:1652  */
    {
			token_info_push(p, "class", &(yyloc));
		    }
#line 8751 "parse.c" /* yacc.c:1652  */
    break;

  case 357:
#line 3010 "parse.y" /* yacc.c:1652  */
    {
			token_info_push(p, "module", &(yyloc));
		    }
#line 8759 "parse.c" /* yacc.c:1652  */
    break;

  case 358:
#line 3016 "parse.y" /* yacc.c:1652  */
    {
			token_info_push(p, "def", &(yyloc));
		    }
#line 8767 "parse.c" /* yacc.c:1652  */
    break;

  case 359:
#line 3022 "parse.y" /* yacc.c:1652  */
    {
			token_info_push(p, "do", &(yyloc));
		    }
#line 8775 "parse.c" /* yacc.c:1652  */
    break;

  case 360:
#line 3028 "parse.y" /* yacc.c:1652  */
    {
			token_info_push(p, "do", &(yyloc));
		    }
#line 8783 "parse.c" /* yacc.c:1652  */
    break;

  case 361:
#line 3034 "parse.y" /* yacc.c:1652  */
    {
			token_info_warn(p, "rescue", p->token_info, 1, &(yyloc));
		    }
#line 8791 "parse.c" /* yacc.c:1652  */
    break;

  case 362:
#line 3040 "parse.y" /* yacc.c:1652  */
    {
			token_info_warn(p, "ensure", p->token_info, 1, &(yyloc));
		    }
#line 8799 "parse.c" /* yacc.c:1652  */
    break;

  case 363:
#line 3046 "parse.y" /* yacc.c:1652  */
    {
			token_info_warn(p, "when", p->token_info, 0, &(yyloc));
		    }
#line 8807 "parse.c" /* yacc.c:1652  */
    break;

  case 364:
#line 3052 "parse.y" /* yacc.c:1652  */
    {
			token_info *ptinfo_beg = p->token_info;
			int same = ptinfo_beg && strcmp(ptinfo_beg->token, "case") != 0;
			token_info_warn(p, "else", p->token_info, same, &(yyloc));
		    }
#line 8817 "parse.c" /* yacc.c:1652  */
    break;

  case 365:
#line 3060 "parse.y" /* yacc.c:1652  */
    {
			token_info_warn(p, "elsif", p->token_info, 1, &(yyloc));
		    }
#line 8825 "parse.c" /* yacc.c:1652  */
    break;

  case 366:
#line 3066 "parse.y" /* yacc.c:1652  */
    {
			token_info_pop(p, "end", &(yyloc));
		    }
#line 8833 "parse.c" /* yacc.c:1652  */
    break;

  case 367:
#line 3072 "parse.y" /* yacc.c:1652  */
    {
			if (p->in_class && !p->in_def && !dyna_in_block(p))
			    yyerror1(&(yylsp[0]), "Invalid return in class/module body");
		    }
#line 8842 "parse.c" /* yacc.c:1652  */
    break;

  case 374:
#line 3091 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = new_if(p, (yyvsp[-3].node), (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
			fixpos((yyval.node), (yyvsp[-3].node));
		    /*% %*/
		    /*% ripper: elsif!($2, $4, escape_Qundef($5)) %*/
		    }
#line 8854 "parse.c" /* yacc.c:1652  */
    break;

  case 376:
#line 3102 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[0].node);
		    /*% %*/
		    /*% ripper: else!($2) %*/
		    }
#line 8865 "parse.c" /* yacc.c:1652  */
    break;

  case 379:
#line 3115 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = assignable(p, (yyvsp[0].id), 0, &(yyloc));
		    /*% %*/
		    /*% ripper: assignable(p, $1) %*/
		    }
#line 8876 "parse.c" /* yacc.c:1652  */
    break;

  case 380:
#line 3122 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node);
		    /*% %*/
		    /*% ripper: mlhs_paren!($2) %*/
		    }
#line 8887 "parse.c" /* yacc.c:1652  */
    break;

  case 381:
#line 3131 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_LIST((yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_add!(mlhs_new!, $1) %*/
		    }
#line 8898 "parse.c" /* yacc.c:1652  */
    break;

  case 382:
#line 3138 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = list_append(p, (yyvsp[-2].node), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: mlhs_add!($1, $3) %*/
		    }
#line 8909 "parse.c" /* yacc.c:1652  */
    break;

  case 383:
#line 3147 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN((yyvsp[0].node), 0, &(yyloc));
		    /*% %*/
		    /*% ripper: $1 %*/
		    }
#line 8920 "parse.c" /* yacc.c:1652  */
    break;

  case 384:
#line 3154 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN((yyvsp[-3].node), assignable(p, (yyvsp[0].id), 0, &(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_add_star!($1, assignable(p, $4)) %*/
		    }
#line 8931 "parse.c" /* yacc.c:1652  */
    break;

  case 385:
#line 3161 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN((yyvsp[-5].node), NEW_POSTARG(assignable(p, (yyvsp[-2].id), 0, &(yyloc)), (yyvsp[0].node), &(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_add_post!(mlhs_add_star!($1, assignable(p, $4)), $6) %*/
		    }
#line 8942 "parse.c" /* yacc.c:1652  */
    break;

  case 386:
#line 3168 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN((yyvsp[-2].node), NODE_SPECIAL_NO_NAME_REST, &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_add_star!($1, Qnil) %*/
		    }
#line 8953 "parse.c" /* yacc.c:1652  */
    break;

  case 387:
#line 3175 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN((yyvsp[-4].node), NEW_POSTARG(NODE_SPECIAL_NO_NAME_REST, (yyvsp[0].node), &(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_add_post!(mlhs_add_star!($1, Qnil), $5) %*/
		    }
#line 8964 "parse.c" /* yacc.c:1652  */
    break;

  case 388:
#line 3182 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN(0, assignable(p, (yyvsp[0].id), 0, &(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_add_star!(mlhs_new!, assignable(p, $2)) %*/
		    }
#line 8975 "parse.c" /* yacc.c:1652  */
    break;

  case 389:
#line 3189 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN(0, NEW_POSTARG(assignable(p, (yyvsp[-2].id), 0, &(yyloc)), (yyvsp[0].node), &(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_add_post!(mlhs_add_star!(mlhs_new!, assignable(p, $2)), $4) %*/
		    }
#line 8986 "parse.c" /* yacc.c:1652  */
    break;

  case 390:
#line 3196 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN(0, NODE_SPECIAL_NO_NAME_REST, &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_add_star!(mlhs_new!, Qnil) %*/
		    }
#line 8997 "parse.c" /* yacc.c:1652  */
    break;

  case 391:
#line 3203 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_MASGN(0, NEW_POSTARG(NODE_SPECIAL_NO_NAME_REST, (yyvsp[0].node), &(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: mlhs_add_post!(mlhs_add_star!(mlhs_new!, Qnil), $3) %*/
		    }
#line 9008 "parse.c" /* yacc.c:1652  */
    break;

  case 392:
#line 3213 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args_tail(p, (yyvsp[-3].node), (yyvsp[-1].id), (yyvsp[0].id), &(yylsp[-1]));
		    }
#line 9016 "parse.c" /* yacc.c:1652  */
    break;

  case 393:
#line 3217 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args_tail(p, (yyvsp[-1].node), Qnone, (yyvsp[0].id), &(yylsp[-1]));
		    }
#line 9024 "parse.c" /* yacc.c:1652  */
    break;

  case 394:
#line 3221 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args_tail(p, Qnone, (yyvsp[-1].id), (yyvsp[0].id), &(yylsp[-1]));
		    }
#line 9032 "parse.c" /* yacc.c:1652  */
    break;

  case 395:
#line 3225 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args_tail(p, Qnone, Qnone, (yyvsp[0].id), &(yylsp[0]));
		    }
#line 9040 "parse.c" /* yacc.c:1652  */
    break;

  case 396:
#line 3231 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = (yyvsp[0].node);
		    }
#line 9048 "parse.c" /* yacc.c:1652  */
    break;

  case 397:
#line 3235 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args_tail(p, Qnone, Qnone, Qnone, &(yylsp[0]));
		    }
#line 9056 "parse.c" /* yacc.c:1652  */
    break;

  case 398:
#line 3241 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args(p, (yyvsp[-5].node), (yyvsp[-3].node), (yyvsp[-1].id), Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 9064 "parse.c" /* yacc.c:1652  */
    break;

  case 399:
#line 3245 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args(p, (yyvsp[-7].node), (yyvsp[-5].node), (yyvsp[-3].id), (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    }
#line 9072 "parse.c" /* yacc.c:1652  */
    break;

  case 400:
#line 3249 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args(p, (yyvsp[-3].node), (yyvsp[-1].node), Qnone, Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 9080 "parse.c" /* yacc.c:1652  */
    break;

  case 401:
#line 3253 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args(p, (yyvsp[-5].node), (yyvsp[-3].node), Qnone, (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    }
#line 9088 "parse.c" /* yacc.c:1652  */
    break;

  case 402:
#line 3257 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args(p, (yyvsp[-3].node), Qnone, (yyvsp[-1].id), Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 9096 "parse.c" /* yacc.c:1652  */
    break;

  case 403:
#line 3261 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			/* magic number for rest_id in iseq_set_arguments() */
			(yyval.node) = new_args(p, (yyvsp[-1].node), Qnone, excessed_comma, Qnone, new_args_tail(p, Qnone, Qnone, Qnone, &(yylsp[-1])), &(yyloc));
		    /*% %*/
		    /*% ripper: new_args(p, $1, Qnone, excessed_comma!, Qnone, new_args_tail(p, Qnone, Qnone, Qnone, NULL), NULL) %*/
		    }
#line 9108 "parse.c" /* yacc.c:1652  */
    break;

  case 404:
#line 3269 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args(p, (yyvsp[-5].node), Qnone, (yyvsp[-3].id), (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    }
#line 9116 "parse.c" /* yacc.c:1652  */
    break;

  case 405:
#line 3273 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args(p, (yyvsp[-1].node), Qnone, Qnone, Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 9124 "parse.c" /* yacc.c:1652  */
    break;

  case 406:
#line 3277 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args(p, Qnone, (yyvsp[-3].node), (yyvsp[-1].id), Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 9132 "parse.c" /* yacc.c:1652  */
    break;

  case 407:
#line 3281 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args(p, Qnone, (yyvsp[-5].node), (yyvsp[-3].id), (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    }
#line 9140 "parse.c" /* yacc.c:1652  */
    break;

  case 408:
#line 3285 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args(p, Qnone, (yyvsp[-1].node), Qnone, Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 9148 "parse.c" /* yacc.c:1652  */
    break;

  case 409:
#line 3289 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args(p, Qnone, (yyvsp[-3].node), Qnone, (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    }
#line 9156 "parse.c" /* yacc.c:1652  */
    break;

  case 410:
#line 3293 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args(p, Qnone, Qnone, (yyvsp[-1].id), Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 9164 "parse.c" /* yacc.c:1652  */
    break;

  case 411:
#line 3297 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args(p, Qnone, Qnone, (yyvsp[-3].id), (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    }
#line 9172 "parse.c" /* yacc.c:1652  */
    break;

  case 412:
#line 3301 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args(p, Qnone, Qnone, Qnone, Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 9180 "parse.c" /* yacc.c:1652  */
    break;

  case 414:
#line 3308 "parse.y" /* yacc.c:1652  */
    {
			p->command_start = TRUE;
		    }
#line 9188 "parse.c" /* yacc.c:1652  */
    break;

  case 415:
#line 3314 "parse.y" /* yacc.c:1652  */
    {
			p->cur_arg = 0;
			p->max_numparam = -1;
		    /*%%%*/
			(yyval.node) = 0;
		    /*% %*/
		    /*% ripper: block_var!(params_new(Qnil,Qnil,Qnil,Qnil,Qnil,Qnil,Qnil), escape_Qundef($2)) %*/
		    }
#line 9201 "parse.c" /* yacc.c:1652  */
    break;

  case 416:
#line 3323 "parse.y" /* yacc.c:1652  */
    {
			p->max_numparam = -1;
		    /*%%%*/
			(yyval.node) = 0;
		    /*% %*/
		    /*% ripper: block_var!(params_new(Qnil,Qnil,Qnil,Qnil,Qnil,Qnil,Qnil), Qnil) %*/
		    }
#line 9213 "parse.c" /* yacc.c:1652  */
    break;

  case 417:
#line 3331 "parse.y" /* yacc.c:1652  */
    {
			p->cur_arg = 0;
			p->max_numparam = -1;
		    /*%%%*/
			(yyval.node) = (yyvsp[-2].node);
		    /*% %*/
		    /*% ripper: block_var!(escape_Qundef($2), escape_Qundef($3)) %*/
		    }
#line 9226 "parse.c" /* yacc.c:1652  */
    break;

  case 418:
#line 3343 "parse.y" /* yacc.c:1652  */
    {
		      (yyval.node) = 0;
		    }
#line 9234 "parse.c" /* yacc.c:1652  */
    break;

  case 419:
#line 3347 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = 0;
		    /*% %*/
		    /*% ripper: $3 %*/
		    }
#line 9245 "parse.c" /* yacc.c:1652  */
    break;

  case 422:
#line 3362 "parse.y" /* yacc.c:1652  */
    {
			new_bv(p, get_id((yyvsp[0].id)));
		    /*% ripper: get_value($1) %*/
		    }
#line 9254 "parse.c" /* yacc.c:1652  */
    break;

  case 423:
#line 3367 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = 0;
		    }
#line 9262 "parse.c" /* yacc.c:1652  */
    break;

  case 424:
#line 3372 "parse.y" /* yacc.c:1652  */
    {
			(yyval.vars) = dyna_push(p);
		    }
#line 9270 "parse.c" /* yacc.c:1652  */
    break;

  case 425:
#line 3375 "parse.y" /* yacc.c:1652  */
    {
			(yyval.num) = p->lex.lpar_beg;
			p->lex.lpar_beg = p->lex.paren_nest;
		    }
#line 9279 "parse.c" /* yacc.c:1652  */
    break;

  case 426:
#line 3379 "parse.y" /* yacc.c:1652  */
    {
			(yyval.num) = p->max_numparam;
			p->max_numparam = 0;
		    }
#line 9288 "parse.c" /* yacc.c:1652  */
    break;

  case 427:
#line 3384 "parse.y" /* yacc.c:1652  */
    {
			CMDARG_PUSH(0);
		    }
#line 9296 "parse.c" /* yacc.c:1652  */
    break;

  case 428:
#line 3388 "parse.y" /* yacc.c:1652  */
    {
			int max_numparam = p->max_numparam;
			p->lex.lpar_beg = (yyvsp[-4].num);
			p->max_numparam = (yyvsp[-3].num);
			CMDARG_POP();
			(yyvsp[-2].node) = args_with_numbered(p, (yyvsp[-2].node), max_numparam);
		    /*%%%*/
                        {
                            YYLTYPE loc = code_loc_gen(&(yylsp[-2]), &(yylsp[0]));
                            (yyval.node) = NEW_LAMBDA((yyvsp[-2].node), (yyvsp[0].node), &loc);
                            nd_set_line((yyval.node)->nd_body, (yylsp[0]).end_pos.lineno);
                            nd_set_line((yyval.node), (yylsp[-2]).end_pos.lineno);
                        }
		    /*% %*/
		    /*% ripper: lambda!($4, $6) %*/
			dyna_pop(p, (yyvsp[-5].vars));
		    }
#line 9318 "parse.c" /* yacc.c:1652  */
    break;

  case 429:
#line 3408 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-2].node);
			p->max_numparam = -1;
		    /*% %*/
		    /*% ripper: paren!($2) %*/
		    }
#line 9330 "parse.c" /* yacc.c:1652  */
    break;

  case 430:
#line 3416 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			if (!args_info_empty_p((yyvsp[0].node)->nd_ainfo))
			    p->max_numparam = -1;
		    /*% %*/
			(yyval.node) = (yyvsp[0].node);
		    }
#line 9342 "parse.c" /* yacc.c:1652  */
    break;

  case 431:
#line 3426 "parse.y" /* yacc.c:1652  */
    {
			token_info_pop(p, "}", &(yylsp[0]));
			(yyval.node) = (yyvsp[-1].node);
		    }
#line 9351 "parse.c" /* yacc.c:1652  */
    break;

  case 432:
#line 3431 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = (yyvsp[-1].node);
		    }
#line 9359 "parse.c" /* yacc.c:1652  */
    break;

  case 433:
#line 3437 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = (yyvsp[-1].node);
		    /*%%%*/
			(yyval.node)->nd_body->nd_loc = code_loc_gen(&(yylsp[-2]), &(yylsp[0]));
			nd_set_line((yyval.node), (yylsp[-2]).end_pos.lineno);
		    /*% %*/
		    }
#line 9371 "parse.c" /* yacc.c:1652  */
    break;

  case 434:
#line 3447 "parse.y" /* yacc.c:1652  */
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
#line 9389 "parse.c" /* yacc.c:1652  */
    break;

  case 435:
#line 3461 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = new_qcall(p, (yyvsp[-2].id), (yyvsp[-3].node), (yyvsp[-1].id), (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
		    /*% %*/
		    /*% ripper: opt_event(:method_add_arg!, call!($1, $2, $3), $4) %*/
		    }
#line 9400 "parse.c" /* yacc.c:1652  */
    break;

  case 436:
#line 3468 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = new_command_qcall(p, (yyvsp[-3].id), (yyvsp[-4].node), (yyvsp[-2].id), (yyvsp[-1].node), (yyvsp[0].node), &(yylsp[-2]), &(yyloc));
		    /*% %*/
		    /*% ripper: opt_event(:method_add_block!, command_call!($1, $2, $3, $4), $5) %*/
		    }
#line 9411 "parse.c" /* yacc.c:1652  */
    break;

  case 437:
#line 3475 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = new_command_qcall(p, (yyvsp[-3].id), (yyvsp[-4].node), (yyvsp[-2].id), (yyvsp[-1].node), (yyvsp[0].node), &(yylsp[-2]), &(yyloc));
		    /*% %*/
		    /*% ripper: method_add_block!(command_call!($1, $2, $3, $4), $5) %*/
		    }
#line 9422 "parse.c" /* yacc.c:1652  */
    break;

  case 438:
#line 3484 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node);
			(yyval.node)->nd_args = (yyvsp[0].node);
			nd_set_last_loc((yyvsp[-1].node), (yylsp[0]).end_pos);
		    /*% %*/
		    /*% ripper: method_add_arg!(fcall!($1), $2) %*/
		    }
#line 9435 "parse.c" /* yacc.c:1652  */
    break;

  case 439:
#line 3493 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = new_qcall(p, (yyvsp[-2].id), (yyvsp[-3].node), (yyvsp[-1].id), (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
			nd_set_line((yyval.node), (yylsp[-1]).end_pos.lineno);
		    /*% %*/
		    /*% ripper: opt_event(:method_add_arg!, call!($1, $2, $3), $4) %*/
		    }
#line 9447 "parse.c" /* yacc.c:1652  */
    break;

  case 440:
#line 3501 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = new_qcall(p, ID2VAL(idCOLON2), (yyvsp[-3].node), (yyvsp[-1].id), (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
			nd_set_line((yyval.node), (yylsp[-1]).end_pos.lineno);
		    /*% %*/
		    /*% ripper: method_add_arg!(call!($1, ID2VAL(idCOLON2), $3), $4) %*/
		    }
#line 9459 "parse.c" /* yacc.c:1652  */
    break;

  case 441:
#line 3509 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = new_qcall(p, ID2VAL(idCOLON2), (yyvsp[-2].node), (yyvsp[0].id), Qnull, &(yylsp[0]), &(yyloc));
		    /*% %*/
		    /*% ripper: call!($1, ID2VAL(idCOLON2), $3) %*/
		    }
#line 9470 "parse.c" /* yacc.c:1652  */
    break;

  case 442:
#line 3516 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = new_qcall(p, (yyvsp[-1].id), (yyvsp[-2].node), ID2VAL(idCall), (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
			nd_set_line((yyval.node), (yylsp[-1]).end_pos.lineno);
		    /*% %*/
		    /*% ripper: method_add_arg!(call!($1, $2, ID2VAL(idCall)), $3) %*/
		    }
#line 9482 "parse.c" /* yacc.c:1652  */
    break;

  case 443:
#line 3524 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = new_qcall(p, ID2VAL(idCOLON2), (yyvsp[-2].node), ID2VAL(idCall), (yyvsp[0].node), &(yylsp[-1]), &(yyloc));
			nd_set_line((yyval.node), (yylsp[-1]).end_pos.lineno);
		    /*% %*/
		    /*% ripper: method_add_arg!(call!($1, ID2VAL(idCOLON2), ID2VAL(idCall)), $3) %*/
		    }
#line 9494 "parse.c" /* yacc.c:1652  */
    break;

  case 444:
#line 3532 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_SUPER((yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: super!($2) %*/
		    }
#line 9505 "parse.c" /* yacc.c:1652  */
    break;

  case 445:
#line 3539 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_ZSUPER(&(yyloc));
		    /*% %*/
		    /*% ripper: zsuper! %*/
		    }
#line 9516 "parse.c" /* yacc.c:1652  */
    break;

  case 446:
#line 3546 "parse.y" /* yacc.c:1652  */
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
#line 9531 "parse.c" /* yacc.c:1652  */
    break;

  case 447:
#line 3559 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = (yyvsp[-1].node);
		    /*%%%*/
			(yyval.node)->nd_body->nd_loc = code_loc_gen(&(yylsp[-2]), &(yylsp[0]));
			nd_set_line((yyval.node), (yylsp[-2]).end_pos.lineno);
		    /*% %*/
		    }
#line 9543 "parse.c" /* yacc.c:1652  */
    break;

  case 448:
#line 3567 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = (yyvsp[-1].node);
		    /*%%%*/
			(yyval.node)->nd_body->nd_loc = code_loc_gen(&(yylsp[-2]), &(yylsp[0]));
			nd_set_line((yyval.node), (yylsp[-2]).end_pos.lineno);
		    /*% %*/
		    }
#line 9555 "parse.c" /* yacc.c:1652  */
    break;

  case 449:
#line 3576 "parse.y" /* yacc.c:1652  */
    {(yyval.vars) = dyna_push(p);}
#line 9561 "parse.c" /* yacc.c:1652  */
    break;

  case 450:
#line 3577 "parse.y" /* yacc.c:1652  */
    {
			(yyval.num) = p->max_numparam;
			p->max_numparam = 0;
		    }
#line 9570 "parse.c" /* yacc.c:1652  */
    break;

  case 451:
#line 3582 "parse.y" /* yacc.c:1652  */
    {
			int max_numparam = p->max_numparam;
			p->max_numparam = (yyvsp[-2].num);
			(yyvsp[-1].node) = args_with_numbered(p, (yyvsp[-1].node), max_numparam);
		    /*%%%*/
			(yyval.node) = NEW_ITER((yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: brace_block!(escape_Qundef($3), $4) %*/
			dyna_pop(p, (yyvsp[-3].vars));
		    }
#line 9585 "parse.c" /* yacc.c:1652  */
    break;

  case 452:
#line 3594 "parse.y" /* yacc.c:1652  */
    {(yyval.vars) = dyna_push(p);}
#line 9591 "parse.c" /* yacc.c:1652  */
    break;

  case 453:
#line 3595 "parse.y" /* yacc.c:1652  */
    {
			(yyval.num) = p->max_numparam;
			p->max_numparam = 0;
			CMDARG_PUSH(0);
		    }
#line 9601 "parse.c" /* yacc.c:1652  */
    break;

  case 454:
#line 3601 "parse.y" /* yacc.c:1652  */
    {
			int max_numparam = p->max_numparam;
			p->max_numparam = (yyvsp[-2].num);
			(yyvsp[-1].node) = args_with_numbered(p, (yyvsp[-1].node), max_numparam);
		    /*%%%*/
			(yyval.node) = NEW_ITER((yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: do_block!(escape_Qundef($3), $4) %*/
			CMDARG_POP();
			dyna_pop(p, (yyvsp[-3].vars));
		    }
#line 9617 "parse.c" /* yacc.c:1652  */
    break;

  case 455:
#line 3615 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			check_literal_when(p, (yyvsp[0].node), &(yylsp[0]));
			(yyval.node) = NEW_LIST((yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: args_add!(args_new!, $1) %*/
		    }
#line 9629 "parse.c" /* yacc.c:1652  */
    break;

  case 456:
#line 3623 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_SPLAT((yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: args_add_star!(args_new!, $2) %*/
		    }
#line 9640 "parse.c" /* yacc.c:1652  */
    break;

  case 457:
#line 3630 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			check_literal_when(p, (yyvsp[0].node), &(yylsp[0]));
			(yyval.node) = last_arg_append(p, (yyvsp[-2].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: args_add!($1, $3) %*/
		    }
#line 9652 "parse.c" /* yacc.c:1652  */
    break;

  case 458:
#line 3638 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = rest_arg_append(p, (yyvsp[-3].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: args_add_star!($1, $4) %*/
		    }
#line 9663 "parse.c" /* yacc.c:1652  */
    break;

  case 459:
#line 3649 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_WHEN((yyvsp[-3].node), (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
			fixpos((yyval.node), (yyvsp[-3].node));
		    /*% %*/
		    /*% ripper: when!($2, $4, escape_Qundef($5)) %*/
		    }
#line 9675 "parse.c" /* yacc.c:1652  */
    break;

  case 462:
#line 3663 "parse.y" /* yacc.c:1652  */
    {
			SET_LEX_STATE(EXPR_BEG|EXPR_LABEL);
			p->command_start = FALSE;
			(yyval.num) = p->in_kwarg;
			p->in_kwarg = 1;
		    }
#line 9686 "parse.c" /* yacc.c:1652  */
    break;

  case 463:
#line 3670 "parse.y" /* yacc.c:1652  */
    {
			p->in_kwarg = !!(yyvsp[-2].num);
		    }
#line 9694 "parse.c" /* yacc.c:1652  */
    break;

  case 464:
#line 3675 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_IN((yyvsp[-4].node), (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: in!($3, $6, escape_Qundef($7)) %*/
		    }
#line 9705 "parse.c" /* yacc.c:1652  */
    break;

  case 468:
#line 3689 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = new_if(p, (yyvsp[0].node), remove_begin((yyvsp[-2].node)), 0, &(yyloc));
			fixpos((yyval.node), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: if_mod!($3, $1) %*/
		    }
#line 9717 "parse.c" /* yacc.c:1652  */
    break;

  case 469:
#line 3697 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = new_unless(p, (yyvsp[0].node), remove_begin((yyvsp[-2].node)), 0, &(yyloc));
			fixpos((yyval.node), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: unless_mod!($3, $1) %*/
		    }
#line 9729 "parse.c" /* yacc.c:1652  */
    break;

  case 471:
#line 3708 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_array_pattern_tail(p, Qnone, 1, 0, Qnone, &(yyloc));
			(yyval.node) = new_array_pattern(p, Qnone, get_value((yyvsp[-1].node)), (yyval.node), &(yyloc));
		    }
#line 9738 "parse.c" /* yacc.c:1652  */
    break;

  case 472:
#line 3713 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_array_pattern(p, Qnone, get_value((yyvsp[-2].node)), (yyvsp[0].node), &(yyloc));
		    /*%%%*/
			nd_set_first_loc((yyval.node), (yylsp[-2]).beg_pos);
		    /*%
		    %*/
		    }
#line 9750 "parse.c" /* yacc.c:1652  */
    break;

  case 473:
#line 3721 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_array_pattern(p, Qnone, Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 9758 "parse.c" /* yacc.c:1652  */
    break;

  case 474:
#line 3725 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_hash_pattern(p, Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 9766 "parse.c" /* yacc.c:1652  */
    break;

  case 476:
#line 3734 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			NODE *n = NEW_LIST((yyvsp[-2].node), &(yyloc));
			n = list_append(p, n, (yyvsp[0].node));
			(yyval.node) = new_hash(p, n, &(yyloc));
		    /*% %*/
		    /*% ripper: binary!($1, STATIC_ID2SYM((id_assoc)), $3) %*/
		    }
#line 9779 "parse.c" /* yacc.c:1652  */
    break;

  case 478:
#line 3746 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_NODE(NODE_OR, (yyvsp[-2].node), (yyvsp[0].node), 0, &(yyloc));
		    /*% %*/
		    /*% ripper: binary!($1, STATIC_ID2SYM((id_or)), $3) %*/
		    }
#line 9790 "parse.c" /* yacc.c:1652  */
    break;

  case 481:
#line 3757 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_array_pattern(p, (yyvsp[-3].node), Qnone, (yyvsp[-1].node), &(yyloc));
		    /*%%%*/
			nd_set_first_loc((yyval.node), (yylsp[-3]).beg_pos);
		    /*%
		    %*/
		    }
#line 9802 "parse.c" /* yacc.c:1652  */
    break;

  case 482:
#line 3765 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_hash_pattern(p, (yyvsp[-3].node), (yyvsp[-1].node), &(yyloc));
		    /*%%%*/
			nd_set_first_loc((yyval.node), (yylsp[-3]).beg_pos);
		    /*%
		    %*/
		    }
#line 9814 "parse.c" /* yacc.c:1652  */
    break;

  case 483:
#line 3773 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_array_pattern_tail(p, Qnone, 0, 0, Qnone, &(yyloc));
			(yyval.node) = new_array_pattern(p, (yyvsp[-2].node), Qnone, (yyval.node), &(yyloc));
		    }
#line 9823 "parse.c" /* yacc.c:1652  */
    break;

  case 484:
#line 3778 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_array_pattern(p, (yyvsp[-3].node), Qnone, (yyvsp[-1].node), &(yyloc));
		    /*%%%*/
			nd_set_first_loc((yyval.node), (yylsp[-3]).beg_pos);
		    /*%
		    %*/
		    }
#line 9835 "parse.c" /* yacc.c:1652  */
    break;

  case 485:
#line 3786 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_hash_pattern(p, (yyvsp[-3].node), (yyvsp[-1].node), &(yyloc));
		    /*%%%*/
			nd_set_first_loc((yyval.node), (yylsp[-3]).beg_pos);
		    /*%
		    %*/
		    }
#line 9847 "parse.c" /* yacc.c:1652  */
    break;

  case 486:
#line 3794 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_array_pattern_tail(p, Qnone, 0, 0, Qnone, &(yyloc));
			(yyval.node) = new_array_pattern(p, (yyvsp[-2].node), Qnone, (yyval.node), &(yyloc));
		    }
#line 9856 "parse.c" /* yacc.c:1652  */
    break;

  case 487:
#line 3799 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_array_pattern(p, Qnone, Qnone, (yyvsp[-1].node), &(yyloc));
		    }
#line 9864 "parse.c" /* yacc.c:1652  */
    break;

  case 488:
#line 3803 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_array_pattern_tail(p, Qnone, 0, 0, Qnone, &(yyloc));
			(yyval.node) = new_array_pattern(p, Qnone, Qnone, (yyval.node), &(yyloc));
		    }
#line 9873 "parse.c" /* yacc.c:1652  */
    break;

  case 489:
#line 3808 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_hash_pattern(p, Qnone, (yyvsp[-1].node), &(yyloc));
		    }
#line 9881 "parse.c" /* yacc.c:1652  */
    break;

  case 490:
#line 3812 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_hash_pattern_tail(p, Qnone, 0, &(yyloc));
			(yyval.node) = new_hash_pattern(p, Qnone, (yyval.node), &(yyloc));
		    }
#line 9890 "parse.c" /* yacc.c:1652  */
    break;

  case 491:
#line 3817 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = (yyvsp[-1].node);
		    }
#line 9898 "parse.c" /* yacc.c:1652  */
    break;

  case 492:
#line 3823 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			NODE *pre_args = NEW_LIST((yyvsp[0].node), &(yyloc));
			(yyval.node) = new_array_pattern_tail(p, pre_args, 0, 0, Qnone, &(yyloc));
		    /*%
			$$ = new_array_pattern_tail(p, rb_ary_new_from_args(1, get_value($1)), 0, 0, Qnone, &@$);
		    %*/
		    }
#line 9911 "parse.c" /* yacc.c:1652  */
    break;

  case 493:
#line 3832 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_array_pattern_tail(p, (yyvsp[0].node), 1, 0, Qnone, &(yyloc));
		    }
#line 9919 "parse.c" /* yacc.c:1652  */
    break;

  case 494:
#line 3836 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = new_array_pattern_tail(p, list_concat((yyvsp[-1].node), (yyvsp[0].node)), 0, 0, Qnone, &(yyloc));
		    /*%
			VALUE pre_args = rb_ary_concat($1, get_value($2));
			$$ = new_array_pattern_tail(p, pre_args, 0, 0, Qnone, &@$);
		    %*/
		    }
#line 9932 "parse.c" /* yacc.c:1652  */
    break;

  case 495:
#line 3845 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_array_pattern_tail(p, (yyvsp[-2].node), 1, (yyvsp[0].id), Qnone, &(yyloc));
		    }
#line 9940 "parse.c" /* yacc.c:1652  */
    break;

  case 496:
#line 3849 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_array_pattern_tail(p, (yyvsp[-4].node), 1, (yyvsp[-2].id), (yyvsp[0].node), &(yyloc));
		    }
#line 9948 "parse.c" /* yacc.c:1652  */
    break;

  case 497:
#line 3853 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_array_pattern_tail(p, (yyvsp[-1].node), 1, 0, Qnone, &(yyloc));
		    }
#line 9956 "parse.c" /* yacc.c:1652  */
    break;

  case 498:
#line 3857 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_array_pattern_tail(p, (yyvsp[-3].node), 1, 0, (yyvsp[0].node), &(yyloc));
		    }
#line 9964 "parse.c" /* yacc.c:1652  */
    break;

  case 500:
#line 3864 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = (yyvsp[-1].node);
		    }
#line 9972 "parse.c" /* yacc.c:1652  */
    break;

  case 501:
#line 3868 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = list_concat((yyvsp[-2].node), (yyvsp[-1].node));
		    /*% %*/
		    /*% ripper: rb_ary_concat($1, get_value($2)) %*/
		    }
#line 9983 "parse.c" /* yacc.c:1652  */
    break;

  case 502:
#line 3877 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_array_pattern_tail(p, Qnone, 1, (yyvsp[0].id), Qnone, &(yyloc));
		    }
#line 9991 "parse.c" /* yacc.c:1652  */
    break;

  case 503:
#line 3881 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_array_pattern_tail(p, Qnone, 1, (yyvsp[-2].id), (yyvsp[0].node), &(yyloc));
		    }
#line 9999 "parse.c" /* yacc.c:1652  */
    break;

  case 504:
#line 3885 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_array_pattern_tail(p, Qnone, 1, 0, Qnone, &(yyloc));
		    }
#line 10007 "parse.c" /* yacc.c:1652  */
    break;

  case 505:
#line 3889 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_array_pattern_tail(p, Qnone, 1, 0, (yyvsp[0].node), &(yyloc));
		    }
#line 10015 "parse.c" /* yacc.c:1652  */
    break;

  case 507:
#line 3895 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = list_concat((yyvsp[-2].node), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: rb_ary_concat($1, get_value($3)) %*/
		    }
#line 10026 "parse.c" /* yacc.c:1652  */
    break;

  case 508:
#line 3904 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_LIST((yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: rb_ary_new_from_args(1, get_value($1)) %*/
		    }
#line 10037 "parse.c" /* yacc.c:1652  */
    break;

  case 509:
#line 3913 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) =  new_hash_pattern_tail(p, new_unique_key_hash(p, (yyvsp[-2].node), &(yyloc)), (yyvsp[0].id), &(yyloc));
		    }
#line 10045 "parse.c" /* yacc.c:1652  */
    break;

  case 510:
#line 3917 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) =  new_hash_pattern_tail(p, new_unique_key_hash(p, (yyvsp[0].node), &(yyloc)), 0, &(yyloc));
		    }
#line 10053 "parse.c" /* yacc.c:1652  */
    break;

  case 511:
#line 3921 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) =  new_hash_pattern_tail(p, new_hash(p, Qnone, &(yyloc)), (yyvsp[0].id), &(yyloc));
		    }
#line 10061 "parse.c" /* yacc.c:1652  */
    break;

  case 513:
#line 3928 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = list_concat((yyvsp[-2].node), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: rb_ary_concat($1, $3) %*/
		    }
#line 10072 "parse.c" /* yacc.c:1652  */
    break;

  case 514:
#line 3937 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = list_append(p, NEW_LIST(NEW_LIT(ID2SYM((yyvsp[-1].id)), &(yyloc)), &(yyloc)), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: rb_ary_new_from_args(1, rb_ary_new_from_args(2, get_value($1), get_value($2))) %*/
		    }
#line 10083 "parse.c" /* yacc.c:1652  */
    break;

  case 515:
#line 3944 "parse.y" /* yacc.c:1652  */
    {
			if (!is_local_id(get_id((yyvsp[0].id)))) {
			    yyerror1(&(yylsp[0]), "key must be valid as local variables");
			}
		    /*%%%*/
			(yyval.node) = list_append(p, NEW_LIST(NEW_LIT(ID2SYM((yyvsp[0].id)), &(yyloc)), &(yyloc)), assignable(p, (yyvsp[0].id), 0, &(yyloc)));
		    /*% %*/
		    /*% ripper: rb_ary_new_from_args(1, rb_ary_new_from_args(2, get_value($1), Qnil)) %*/
		    }
#line 10097 "parse.c" /* yacc.c:1652  */
    break;

  case 516:
#line 3954 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			YYLTYPE loc = code_loc_gen(&(yylsp[-3]), &(yylsp[-1]));
			NODE *node = dsym_node(p, (yyvsp[-2].node), &loc);
			if (nd_type(node) == NODE_LIT) {
			    (yyval.node) = list_append(p, NEW_LIST(node, &loc), (yyvsp[0].node));
			}
			else {
			    yyerror1(&loc, "symbol literal with interpolation is not allowed");
			    (yyval.node) = 0;
			}
		    /*% %*/
		    /*% ripper: rb_ary_new_from_args(1, rb_ary_new_from_args(2, $2, get_value($4))) %*/
		    }
#line 10116 "parse.c" /* yacc.c:1652  */
    break;

  case 517:
#line 3969 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			YYLTYPE loc = code_loc_gen(&(yylsp[-2]), &(yylsp[0]));
			NODE *node = dsym_node(p, (yyvsp[-1].node), &loc);
			ID id;
			if (nd_type(node) == NODE_LIT) {
			    id = SYM2ID(node->nd_lit);
			    if (!is_local_id(id)) {
				yyerror1(&loc, "key must be valid as local variables");
			    }
			    (yyval.node) = list_append(p, NEW_LIST(node, &loc), assignable(p, id, 0, &(yyloc)));
			}
			else {
			    yyerror1(&loc, "symbol literal with interpolation is not allowed");
			    (yyval.node) = 0;
			}
		    /*% %*/
		    /*% ripper: rb_ary_new_from_args(1, rb_ary_new_from_args(2, $2, Qnil)) %*/
		    }
#line 10140 "parse.c" /* yacc.c:1652  */
    break;

  case 518:
#line 3991 "parse.y" /* yacc.c:1652  */
    {
		        (yyval.id) = (yyvsp[0].id);
		    }
#line 10148 "parse.c" /* yacc.c:1652  */
    break;

  case 519:
#line 3995 "parse.y" /* yacc.c:1652  */
    {
		        (yyval.id) = 0;
		    }
#line 10156 "parse.c" /* yacc.c:1652  */
    break;

  case 521:
#line 4002 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			value_expr((yyvsp[-2].node));
			value_expr((yyvsp[0].node));
			(yyval.node) = NEW_DOT2((yyvsp[-2].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: dot2!($1, $3) %*/
		    }
#line 10169 "parse.c" /* yacc.c:1652  */
    break;

  case 522:
#line 4011 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			value_expr((yyvsp[-2].node));
			value_expr((yyvsp[0].node));
			(yyval.node) = NEW_DOT3((yyvsp[-2].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: dot3!($1, $3) %*/
		    }
#line 10182 "parse.c" /* yacc.c:1652  */
    break;

  case 523:
#line 4020 "parse.y" /* yacc.c:1652  */
    {
			/*%%%*/
			YYLTYPE loc;
			loc.beg_pos = (yylsp[0]).end_pos;
			loc.end_pos = (yylsp[0]).end_pos;

			value_expr((yyvsp[-1].node));
			(yyval.node) = NEW_DOT2((yyvsp[-1].node), new_nil(&loc), &(yyloc));
		    /*% %*/
		    /*% ripper: dot2!($1, Qnil) %*/
		    }
#line 10198 "parse.c" /* yacc.c:1652  */
    break;

  case 524:
#line 4032 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			YYLTYPE loc;
			loc.beg_pos = (yylsp[0]).end_pos;
			loc.end_pos = (yylsp[0]).end_pos;

			value_expr((yyvsp[-1].node));
			(yyval.node) = NEW_DOT3((yyvsp[-1].node), new_nil(&loc), &(yyloc));
		    /*% %*/
		    /*% ripper: dot3!($1, Qnil) %*/
		    }
#line 10214 "parse.c" /* yacc.c:1652  */
    break;

  case 528:
#line 4047 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			YYLTYPE loc;
			loc.beg_pos = (yylsp[-1]).beg_pos;
			loc.end_pos = (yylsp[-1]).beg_pos;

			value_expr((yyvsp[0].node));
			(yyval.node) = NEW_DOT2(new_nil(&loc), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: dot2!(Qnil, $2) %*/
		    }
#line 10230 "parse.c" /* yacc.c:1652  */
    break;

  case 529:
#line 4059 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			YYLTYPE loc;
			loc.beg_pos = (yylsp[-1]).beg_pos;
			loc.end_pos = (yylsp[-1]).beg_pos;

			value_expr((yyvsp[0].node));
			(yyval.node) = NEW_DOT3(new_nil(&loc), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: dot3!(Qnil, $2) %*/
		    }
#line 10246 "parse.c" /* yacc.c:1652  */
    break;

  case 538:
#line 4081 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			if (!((yyval.node) = gettable(p, (yyvsp[0].id), &(yyloc)))) (yyval.node) = NEW_BEGIN(0, &(yyloc));
		    /*% %*/
		    /*% ripper: var_ref!($1) %*/
		    }
#line 10257 "parse.c" /* yacc.c:1652  */
    break;

  case 539:
#line 4088 "parse.y" /* yacc.c:1652  */
    {
			token_info_push(p, "->", &(yylsp[0]));
		    }
#line 10265 "parse.c" /* yacc.c:1652  */
    break;

  case 540:
#line 4092 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = (yyvsp[0].node);
		    /*%%%*/
			nd_set_first_loc((yyval.node), (yylsp[-2]).beg_pos);
		    /*% %*/
		    }
#line 10276 "parse.c" /* yacc.c:1652  */
    break;

  case 541:
#line 4101 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = assignable(p, (yyvsp[0].id), 0, &(yyloc));
		    /*% %*/
		    /*% ripper: assignable(p, var_field(p, $1)) %*/
		    }
#line 10287 "parse.c" /* yacc.c:1652  */
    break;

  case 542:
#line 4110 "parse.y" /* yacc.c:1652  */
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
#line 10302 "parse.c" /* yacc.c:1652  */
    break;

  case 543:
#line 4123 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_COLON3((yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: top_const_ref!($2) %*/
		    }
#line 10313 "parse.c" /* yacc.c:1652  */
    break;

  case 544:
#line 4130 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_COLON2((yyvsp[-2].node), (yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: const_path_ref!($1, $3) %*/
		    }
#line 10324 "parse.c" /* yacc.c:1652  */
    break;

  case 545:
#line 4137 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = gettable(p, (yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: var_ref!($1) %*/
		   }
#line 10335 "parse.c" /* yacc.c:1652  */
    break;

  case 546:
#line 4148 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_RESBODY((yyvsp[-4].node),
					 (yyvsp[-3].node) ? block_append(p, node_assign(p, (yyvsp[-3].node), NEW_ERRINFO(&(yylsp[-3])), &(yylsp[-3])), (yyvsp[-1].node)) : (yyvsp[-1].node),
					 (yyvsp[0].node), &(yyloc));
			fixpos((yyval.node), (yyvsp[-4].node)?(yyvsp[-4].node):(yyvsp[-1].node));
		    /*% %*/
		    /*% ripper: rescue!(escape_Qundef($2), escape_Qundef($3), escape_Qundef($5), escape_Qundef($6)) %*/
		    }
#line 10349 "parse.c" /* yacc.c:1652  */
    break;

  case 548:
#line 4161 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_LIST((yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: rb_ary_new3(1, get_value($1)) %*/
		    }
#line 10360 "parse.c" /* yacc.c:1652  */
    break;

  case 549:
#line 4168 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			if (!((yyval.node) = splat_array((yyvsp[0].node)))) (yyval.node) = (yyvsp[0].node);
		    /*% %*/
		    /*% ripper: $1 %*/
		    }
#line 10371 "parse.c" /* yacc.c:1652  */
    break;

  case 551:
#line 4178 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = (yyvsp[0].node);
		    }
#line 10379 "parse.c" /* yacc.c:1652  */
    break;

  case 553:
#line 4185 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[0].node);
		    /*% %*/
		    /*% ripper: ensure!($2) %*/
		    }
#line 10390 "parse.c" /* yacc.c:1652  */
    break;

  case 557:
#line 4199 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			NODE *node = (yyvsp[0].node);
			if (!node) {
			    node = NEW_STR(add_mark_object(p, STR_NEW0()), &(yyloc));
			}
			else {
			    node = evstr2dstr(p, node);
			}
			(yyval.node) = node;
		    /*% %*/
		    /*% ripper: $1 %*/
		    }
#line 10408 "parse.c" /* yacc.c:1652  */
    break;

  case 560:
#line 4217 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = literal_concat(p, (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: string_concat!($1, $2) %*/
		    }
#line 10419 "parse.c" /* yacc.c:1652  */
    break;

  case 561:
#line 4226 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = heredoc_dedent(p, (yyvsp[-1].node));
			if ((yyval.node)) nd_set_loc((yyval.node), &(yyloc));
		    /*% %*/
		    /*% ripper: string_literal!(heredoc_dedent(p, $2)) %*/
		    }
#line 10431 "parse.c" /* yacc.c:1652  */
    break;

  case 562:
#line 4236 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = new_xstring(p, heredoc_dedent(p, (yyvsp[-1].node)), &(yyloc));
		    /*% %*/
		    /*% ripper: xstring_literal!(heredoc_dedent(p, $2)) %*/
		    }
#line 10442 "parse.c" /* yacc.c:1652  */
    break;

  case 563:
#line 4245 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_regexp(p, (yyvsp[-1].node), (yyvsp[0].num), &(yyloc));
		    }
#line 10450 "parse.c" /* yacc.c:1652  */
    break;

  case 564:
#line 4251 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = make_array((yyvsp[-1].node), &(yyloc));
		    /*% %*/
		    /*% ripper: array!($3) %*/
		    }
#line 10461 "parse.c" /* yacc.c:1652  */
    break;

  case 565:
#line 4260 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = 0;
		    /*% %*/
		    /*% ripper: words_new! %*/
		    }
#line 10472 "parse.c" /* yacc.c:1652  */
    break;

  case 566:
#line 4267 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = list_append(p, (yyvsp[-2].node), evstr2dstr(p, (yyvsp[-1].node)));
		    /*% %*/
		    /*% ripper: words_add!($1, $2) %*/
		    }
#line 10483 "parse.c" /* yacc.c:1652  */
    break;

  case 568:
#line 4278 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = literal_concat(p, (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: word_add!($1, $2) %*/
		    }
#line 10494 "parse.c" /* yacc.c:1652  */
    break;

  case 569:
#line 4287 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = make_array((yyvsp[-1].node), &(yyloc));
		    /*% %*/
		    /*% ripper: array!($3) %*/
		    }
#line 10505 "parse.c" /* yacc.c:1652  */
    break;

  case 570:
#line 4296 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = 0;
		    /*% %*/
		    /*% ripper: symbols_new! %*/
		    }
#line 10516 "parse.c" /* yacc.c:1652  */
    break;

  case 571:
#line 4303 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = symbol_append(p, (yyvsp[-2].node), evstr2dstr(p, (yyvsp[-1].node)));
		    /*% %*/
		    /*% ripper: symbols_add!($1, $2) %*/
		    }
#line 10527 "parse.c" /* yacc.c:1652  */
    break;

  case 572:
#line 4312 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = make_array((yyvsp[-1].node), &(yyloc));
		    /*% %*/
		    /*% ripper: array!($3) %*/
		    }
#line 10538 "parse.c" /* yacc.c:1652  */
    break;

  case 573:
#line 4321 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = make_array((yyvsp[-1].node), &(yyloc));
		    /*% %*/
		    /*% ripper: array!($3) %*/
		    }
#line 10549 "parse.c" /* yacc.c:1652  */
    break;

  case 574:
#line 4330 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = 0;
		    /*% %*/
		    /*% ripper: qwords_new! %*/
		    }
#line 10560 "parse.c" /* yacc.c:1652  */
    break;

  case 575:
#line 4337 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = list_append(p, (yyvsp[-2].node), (yyvsp[-1].node));
		    /*% %*/
		    /*% ripper: qwords_add!($1, $2) %*/
		    }
#line 10571 "parse.c" /* yacc.c:1652  */
    break;

  case 576:
#line 4346 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = 0;
		    /*% %*/
		    /*% ripper: qsymbols_new! %*/
		    }
#line 10582 "parse.c" /* yacc.c:1652  */
    break;

  case 577:
#line 4353 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = symbol_append(p, (yyvsp[-2].node), (yyvsp[-1].node));
		    /*% %*/
		    /*% ripper: qsymbols_add!($1, $2) %*/
		    }
#line 10593 "parse.c" /* yacc.c:1652  */
    break;

  case 578:
#line 4362 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = 0;
		    /*% %*/
		    /*% ripper: string_content! %*/
		    }
#line 10604 "parse.c" /* yacc.c:1652  */
    break;

  case 579:
#line 4369 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = literal_concat(p, (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: string_add!($1, $2) %*/
		    }
#line 10615 "parse.c" /* yacc.c:1652  */
    break;

  case 580:
#line 4378 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = 0;
		    /*% %*/
		    /*% ripper: xstring_new! %*/
		    }
#line 10626 "parse.c" /* yacc.c:1652  */
    break;

  case 581:
#line 4385 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = literal_concat(p, (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    /*% %*/
		    /*% ripper: xstring_add!($1, $2) %*/
		    }
#line 10637 "parse.c" /* yacc.c:1652  */
    break;

  case 582:
#line 4394 "parse.y" /* yacc.c:1652  */
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
#line 10652 "parse.c" /* yacc.c:1652  */
    break;

  case 583:
#line 4405 "parse.y" /* yacc.c:1652  */
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
#line 10695 "parse.c" /* yacc.c:1652  */
    break;

  case 585:
#line 4447 "parse.y" /* yacc.c:1652  */
    {
			/* need to backup p->lex.strterm so that a string literal `%&foo,#$&,bar&` can be parsed */
			(yyval.strterm) = p->lex.strterm;
			p->lex.strterm = 0;
			SET_LEX_STATE(EXPR_BEG);
		    }
#line 10706 "parse.c" /* yacc.c:1652  */
    break;

  case 586:
#line 4454 "parse.y" /* yacc.c:1652  */
    {
			p->lex.strterm = (yyvsp[-1].strterm);
		    /*%%%*/
			(yyval.node) = NEW_EVSTR((yyvsp[0].node), &(yyloc));
			nd_set_line((yyval.node), (yylsp[0]).end_pos.lineno);
		    /*% %*/
		    /*% ripper: string_dvar!($3) %*/
		    }
#line 10719 "parse.c" /* yacc.c:1652  */
    break;

  case 587:
#line 4463 "parse.y" /* yacc.c:1652  */
    {
			CMDARG_PUSH(0);
			COND_PUSH(0);
		    }
#line 10728 "parse.c" /* yacc.c:1652  */
    break;

  case 588:
#line 4467 "parse.y" /* yacc.c:1652  */
    {
			/* need to backup p->lex.strterm so that a string literal `%!foo,#{ !0 },bar!` can be parsed */
			(yyval.strterm) = p->lex.strterm;
			p->lex.strterm = 0;
		    }
#line 10738 "parse.c" /* yacc.c:1652  */
    break;

  case 589:
#line 4472 "parse.y" /* yacc.c:1652  */
    {
			(yyval.num) = p->lex.state;
			SET_LEX_STATE(EXPR_BEG);
		    }
#line 10747 "parse.c" /* yacc.c:1652  */
    break;

  case 590:
#line 4476 "parse.y" /* yacc.c:1652  */
    {
			(yyval.num) = p->lex.brace_nest;
			p->lex.brace_nest = 0;
		    }
#line 10756 "parse.c" /* yacc.c:1652  */
    break;

  case 591:
#line 4480 "parse.y" /* yacc.c:1652  */
    {
			(yyval.num) = p->heredoc_indent;
			p->heredoc_indent = 0;
		    }
#line 10765 "parse.c" /* yacc.c:1652  */
    break;

  case 592:
#line 4485 "parse.y" /* yacc.c:1652  */
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
#line 10784 "parse.c" /* yacc.c:1652  */
    break;

  case 593:
#line 4502 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_GVAR((yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: var_ref!($1) %*/
		    }
#line 10795 "parse.c" /* yacc.c:1652  */
    break;

  case 594:
#line 4509 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_IVAR((yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: var_ref!($1) %*/
		    }
#line 10806 "parse.c" /* yacc.c:1652  */
    break;

  case 595:
#line 4516 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_CVAR((yyvsp[0].id), &(yyloc));
		    /*% %*/
		    /*% ripper: var_ref!($1) %*/
		    }
#line 10817 "parse.c" /* yacc.c:1652  */
    break;

  case 596:
#line 4523 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = NEW_DVAR((yyvsp[0].id), &(yylsp[0]));
		    /*% %*/
		    /*% ripper: var_ref!($1) %*/
		    }
#line 10828 "parse.c" /* yacc.c:1652  */
    break;

  case 600:
#line 4537 "parse.y" /* yacc.c:1652  */
    {
			SET_LEX_STATE(EXPR_END);
		    /*%%%*/
			(yyval.node) = NEW_LIT(ID2SYM((yyvsp[0].id)), &(yyloc));
		    /*% %*/
		    /*% ripper: symbol_literal!(symbol!($2)) %*/
		    }
#line 10840 "parse.c" /* yacc.c:1652  */
    break;

  case 605:
#line 4553 "parse.y" /* yacc.c:1652  */
    {
			SET_LEX_STATE(EXPR_END);
		    /*%%%*/
			(yyval.node) = dsym_node(p, (yyvsp[-1].node), &(yyloc));
		    /*% %*/
		    /*% ripper: dyna_symbol!($2) %*/
		    }
#line 10852 "parse.c" /* yacc.c:1652  */
    break;

  case 607:
#line 4564 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[0].node);
			add_mark_object(p, (yyval.node)->nd_lit = negate_lit(p, (yyval.node)->nd_lit));
		    /*% %*/
		    /*% ripper: unary!(ID2VAL(idUMinus), $2) %*/
		    }
#line 10864 "parse.c" /* yacc.c:1652  */
    break;

  case 618:
#line 4587 "parse.y" /* yacc.c:1652  */
    {(yyval.id) = KWD2EID(nil, (yyvsp[0].id));}
#line 10870 "parse.c" /* yacc.c:1652  */
    break;

  case 619:
#line 4588 "parse.y" /* yacc.c:1652  */
    {(yyval.id) = KWD2EID(self, (yyvsp[0].id));}
#line 10876 "parse.c" /* yacc.c:1652  */
    break;

  case 620:
#line 4589 "parse.y" /* yacc.c:1652  */
    {(yyval.id) = KWD2EID(true, (yyvsp[0].id));}
#line 10882 "parse.c" /* yacc.c:1652  */
    break;

  case 621:
#line 4590 "parse.y" /* yacc.c:1652  */
    {(yyval.id) = KWD2EID(false, (yyvsp[0].id));}
#line 10888 "parse.c" /* yacc.c:1652  */
    break;

  case 622:
#line 4591 "parse.y" /* yacc.c:1652  */
    {(yyval.id) = KWD2EID(_FILE__, (yyvsp[0].id));}
#line 10894 "parse.c" /* yacc.c:1652  */
    break;

  case 623:
#line 4592 "parse.y" /* yacc.c:1652  */
    {(yyval.id) = KWD2EID(_LINE__, (yyvsp[0].id));}
#line 10900 "parse.c" /* yacc.c:1652  */
    break;

  case 624:
#line 4593 "parse.y" /* yacc.c:1652  */
    {(yyval.id) = KWD2EID(_ENCODING__, (yyvsp[0].id));}
#line 10906 "parse.c" /* yacc.c:1652  */
    break;

  case 625:
#line 4597 "parse.y" /* yacc.c:1652  */
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
#line 10923 "parse.c" /* yacc.c:1652  */
    break;

  case 626:
#line 4610 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			if (!((yyval.node) = gettable(p, (yyvsp[0].id), &(yyloc)))) (yyval.node) = NEW_BEGIN(0, &(yyloc));
		    /*% %*/
		    /*% ripper: var_ref!($1) %*/
		    }
#line 10934 "parse.c" /* yacc.c:1652  */
    break;

  case 627:
#line 4619 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = assignable(p, (yyvsp[0].id), 0, &(yyloc));
		    /*% %*/
		    /*% ripper: assignable(p, var_field(p, $1)) %*/
		    }
#line 10945 "parse.c" /* yacc.c:1652  */
    break;

  case 628:
#line 4626 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = assignable(p, (yyvsp[0].id), 0, &(yyloc));
		    /*% %*/
		    /*% ripper: assignable(p, var_field(p, $1)) %*/
		    }
#line 10956 "parse.c" /* yacc.c:1652  */
    break;

  case 631:
#line 4639 "parse.y" /* yacc.c:1652  */
    {
			SET_LEX_STATE(EXPR_BEG);
			p->command_start = TRUE;
		    }
#line 10965 "parse.c" /* yacc.c:1652  */
    break;

  case 632:
#line 4644 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = (yyvsp[-1].node);
		    }
#line 10973 "parse.c" /* yacc.c:1652  */
    break;

  case 633:
#line 4648 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = 0;
		    /*% %*/
		    /*% ripper: Qnil %*/
		    }
#line 10984 "parse.c" /* yacc.c:1652  */
    break;

  case 634:
#line 4657 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node);
		    /*% %*/
		    /*% ripper: paren!($2) %*/
			SET_LEX_STATE(EXPR_BEG);
			p->command_start = TRUE;
		    }
#line 10997 "parse.c" /* yacc.c:1652  */
    break;

  case 635:
#line 4665 "parse.y" /* yacc.c:1652  */
    {
			(yyval.num) = p->in_kwarg;
			p->in_kwarg = 1;
			SET_LEX_STATE(p->lex.state|EXPR_LABEL); /* force for args */
		    }
#line 11007 "parse.c" /* yacc.c:1652  */
    break;

  case 636:
#line 4671 "parse.y" /* yacc.c:1652  */
    {
			p->in_kwarg = !!(yyvsp[-2].num);
			(yyval.node) = (yyvsp[-1].node);
			SET_LEX_STATE(EXPR_BEG);
			p->command_start = TRUE;
		    }
#line 11018 "parse.c" /* yacc.c:1652  */
    break;

  case 637:
#line 4680 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args_tail(p, (yyvsp[-3].node), (yyvsp[-1].id), (yyvsp[0].id), &(yylsp[-1]));
		    }
#line 11026 "parse.c" /* yacc.c:1652  */
    break;

  case 638:
#line 4684 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args_tail(p, (yyvsp[-1].node), Qnone, (yyvsp[0].id), &(yylsp[-1]));
		    }
#line 11034 "parse.c" /* yacc.c:1652  */
    break;

  case 639:
#line 4688 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args_tail(p, Qnone, (yyvsp[-1].id), (yyvsp[0].id), &(yylsp[-1]));
		    }
#line 11042 "parse.c" /* yacc.c:1652  */
    break;

  case 640:
#line 4692 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args_tail(p, Qnone, Qnone, (yyvsp[0].id), &(yylsp[0]));
		    }
#line 11050 "parse.c" /* yacc.c:1652  */
    break;

  case 641:
#line 4698 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = (yyvsp[0].node);
		    }
#line 11058 "parse.c" /* yacc.c:1652  */
    break;

  case 642:
#line 4702 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args_tail(p, Qnone, Qnone, Qnone, &(yylsp[0]));
		    }
#line 11066 "parse.c" /* yacc.c:1652  */
    break;

  case 643:
#line 4708 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args(p, (yyvsp[-5].node), (yyvsp[-3].node), (yyvsp[-1].id), Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 11074 "parse.c" /* yacc.c:1652  */
    break;

  case 644:
#line 4712 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args(p, (yyvsp[-7].node), (yyvsp[-5].node), (yyvsp[-3].id), (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    }
#line 11082 "parse.c" /* yacc.c:1652  */
    break;

  case 645:
#line 4716 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args(p, (yyvsp[-3].node), (yyvsp[-1].node), Qnone, Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 11090 "parse.c" /* yacc.c:1652  */
    break;

  case 646:
#line 4720 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args(p, (yyvsp[-5].node), (yyvsp[-3].node), Qnone, (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    }
#line 11098 "parse.c" /* yacc.c:1652  */
    break;

  case 647:
#line 4724 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args(p, (yyvsp[-3].node), Qnone, (yyvsp[-1].id), Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 11106 "parse.c" /* yacc.c:1652  */
    break;

  case 648:
#line 4728 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args(p, (yyvsp[-5].node), Qnone, (yyvsp[-3].id), (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    }
#line 11114 "parse.c" /* yacc.c:1652  */
    break;

  case 649:
#line 4732 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args(p, (yyvsp[-1].node), Qnone, Qnone, Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 11122 "parse.c" /* yacc.c:1652  */
    break;

  case 650:
#line 4736 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args(p, Qnone, (yyvsp[-3].node), (yyvsp[-1].id), Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 11130 "parse.c" /* yacc.c:1652  */
    break;

  case 651:
#line 4740 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args(p, Qnone, (yyvsp[-5].node), (yyvsp[-3].id), (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    }
#line 11138 "parse.c" /* yacc.c:1652  */
    break;

  case 652:
#line 4744 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args(p, Qnone, (yyvsp[-1].node), Qnone, Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 11146 "parse.c" /* yacc.c:1652  */
    break;

  case 653:
#line 4748 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args(p, Qnone, (yyvsp[-3].node), Qnone, (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    }
#line 11154 "parse.c" /* yacc.c:1652  */
    break;

  case 654:
#line 4752 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args(p, Qnone, Qnone, (yyvsp[-1].id), Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 11162 "parse.c" /* yacc.c:1652  */
    break;

  case 655:
#line 4756 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args(p, Qnone, Qnone, (yyvsp[-3].id), (yyvsp[-1].node), (yyvsp[0].node), &(yyloc));
		    }
#line 11170 "parse.c" /* yacc.c:1652  */
    break;

  case 656:
#line 4760 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args(p, Qnone, Qnone, Qnone, Qnone, (yyvsp[0].node), &(yyloc));
		    }
#line 11178 "parse.c" /* yacc.c:1652  */
    break;

  case 657:
#line 4764 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = new_args_tail(p, Qnone, Qnone, Qnone, &(yylsp[0]));
			(yyval.node) = new_args(p, Qnone, Qnone, Qnone, Qnone, (yyval.node), &(yylsp[0]));
		    }
#line 11187 "parse.c" /* yacc.c:1652  */
    break;

  case 658:
#line 4771 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			yyerror1(&(yylsp[0]), "formal argument cannot be a constant");
			(yyval.id) = 0;
		    /*% %*/
		    /*% ripper[error]: param_error!($1) %*/
		    }
#line 11199 "parse.c" /* yacc.c:1652  */
    break;

  case 659:
#line 4779 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			yyerror1(&(yylsp[0]), "formal argument cannot be an instance variable");
			(yyval.id) = 0;
		    /*% %*/
		    /*% ripper[error]: param_error!($1) %*/
		    }
#line 11211 "parse.c" /* yacc.c:1652  */
    break;

  case 660:
#line 4787 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			yyerror1(&(yylsp[0]), "formal argument cannot be a global variable");
			(yyval.id) = 0;
		    /*% %*/
		    /*% ripper[error]: param_error!($1) %*/
		    }
#line 11223 "parse.c" /* yacc.c:1652  */
    break;

  case 661:
#line 4795 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			yyerror1(&(yylsp[0]), "formal argument cannot be a class variable");
			(yyval.id) = 0;
		    /*% %*/
		    /*% ripper[error]: param_error!($1) %*/
		    }
#line 11235 "parse.c" /* yacc.c:1652  */
    break;

  case 663:
#line 4806 "parse.y" /* yacc.c:1652  */
    {
			formal_argument(p, get_id((yyvsp[0].id)));
			p->max_numparam = -1;
			(yyval.id) = (yyvsp[0].id);
		    }
#line 11245 "parse.c" /* yacc.c:1652  */
    break;

  case 664:
#line 4814 "parse.y" /* yacc.c:1652  */
    {
			ID id = get_id((yyvsp[0].id));
			arg_var(p, id);
			p->cur_arg = id;
			(yyval.id) = (yyvsp[0].id);
		    }
#line 11256 "parse.c" /* yacc.c:1652  */
    break;

  case 665:
#line 4823 "parse.y" /* yacc.c:1652  */
    {
			p->cur_arg = 0;
		    /*%%%*/
			(yyval.node) = NEW_ARGS_AUX((yyvsp[0].id), 1, &NULL_LOC);
		    /*% %*/
		    /*% ripper: get_value($1) %*/
		    }
#line 11268 "parse.c" /* yacc.c:1652  */
    break;

  case 666:
#line 4831 "parse.y" /* yacc.c:1652  */
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
#line 11291 "parse.c" /* yacc.c:1652  */
    break;

  case 668:
#line 4854 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-2].node);
			(yyval.node)->nd_plen++;
			(yyval.node)->nd_next = block_append(p, (yyval.node)->nd_next, (yyvsp[0].node)->nd_next);
			rb_discard_node(p, (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: rb_ary_push($1, get_value($3)) %*/
		    }
#line 11305 "parse.c" /* yacc.c:1652  */
    break;

  case 669:
#line 4867 "parse.y" /* yacc.c:1652  */
    {
			ID id = get_id((yyvsp[0].id));
			arg_var(p, formal_argument(p, id));
			p->cur_arg = id;
			p->max_numparam = -1;
			(yyval.id) = (yyvsp[0].id);
		    }
#line 11317 "parse.c" /* yacc.c:1652  */
    break;

  case 670:
#line 4877 "parse.y" /* yacc.c:1652  */
    {
			p->cur_arg = 0;
		    /*%%%*/
			(yyval.node) = new_kw_arg(p, assignable(p, (yyvsp[-1].id), (yyvsp[0].node), &(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: rb_assoc_new(get_value(assignable(p, $1)), get_value($2)) %*/
		    }
#line 11329 "parse.c" /* yacc.c:1652  */
    break;

  case 671:
#line 4885 "parse.y" /* yacc.c:1652  */
    {
			p->cur_arg = 0;
		    /*%%%*/
			(yyval.node) = new_kw_arg(p, assignable(p, (yyvsp[0].id), NODE_SPECIAL_REQUIRED_KEYWORD, &(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: rb_assoc_new(get_value(assignable(p, $1)), 0) %*/
		    }
#line 11341 "parse.c" /* yacc.c:1652  */
    break;

  case 672:
#line 4895 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = new_kw_arg(p, assignable(p, (yyvsp[-1].id), (yyvsp[0].node), &(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: rb_assoc_new(get_value(assignable(p, $1)), get_value($2)) %*/
		    }
#line 11352 "parse.c" /* yacc.c:1652  */
    break;

  case 673:
#line 4902 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = new_kw_arg(p, assignable(p, (yyvsp[0].id), NODE_SPECIAL_REQUIRED_KEYWORD, &(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: rb_assoc_new(get_value(assignable(p, $1)), 0) %*/
		    }
#line 11363 "parse.c" /* yacc.c:1652  */
    break;

  case 674:
#line 4911 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[0].node);
		    /*% %*/
		    /*% ripper: rb_ary_new3(1, get_value($1)) %*/
		    }
#line 11374 "parse.c" /* yacc.c:1652  */
    break;

  case 675:
#line 4918 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = kwd_append((yyvsp[-2].node), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: rb_ary_push($1, get_value($3)) %*/
		    }
#line 11385 "parse.c" /* yacc.c:1652  */
    break;

  case 676:
#line 4928 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[0].node);
		    /*% %*/
		    /*% ripper: rb_ary_new3(1, get_value($1)) %*/
		    }
#line 11396 "parse.c" /* yacc.c:1652  */
    break;

  case 677:
#line 4935 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = kwd_append((yyvsp[-2].node), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: rb_ary_push($1, get_value($3)) %*/
		    }
#line 11407 "parse.c" /* yacc.c:1652  */
    break;

  case 680:
#line 4948 "parse.y" /* yacc.c:1652  */
    {
			arg_var(p, shadowing_lvar(p, get_id((yyvsp[0].id))));
		    /*%%%*/
			(yyval.id) = (yyvsp[0].id);
		    /*% %*/
		    /*% ripper: kwrest_param!($2) %*/
		    }
#line 11419 "parse.c" /* yacc.c:1652  */
    break;

  case 681:
#line 4956 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.id) = internal_id(p);
			arg_var(p, (yyval.id));
		    /*% %*/
		    /*% ripper: kwrest_param!(Qnil) %*/
		    }
#line 11431 "parse.c" /* yacc.c:1652  */
    break;

  case 682:
#line 4966 "parse.y" /* yacc.c:1652  */
    {
			p->cur_arg = 0;
		    /*%%%*/
			(yyval.node) = NEW_OPT_ARG(0, assignable(p, (yyvsp[-2].id), (yyvsp[0].node), &(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: rb_assoc_new(get_value(assignable(p, $1)), get_value($3)) %*/
		    }
#line 11443 "parse.c" /* yacc.c:1652  */
    break;

  case 683:
#line 4976 "parse.y" /* yacc.c:1652  */
    {
			p->cur_arg = 0;
		    /*%%%*/
			(yyval.node) = NEW_OPT_ARG(0, assignable(p, (yyvsp[-2].id), (yyvsp[0].node), &(yyloc)), &(yyloc));
		    /*% %*/
		    /*% ripper: rb_assoc_new(get_value(assignable(p, $1)), get_value($3)) %*/
		    }
#line 11455 "parse.c" /* yacc.c:1652  */
    break;

  case 684:
#line 4986 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[0].node);
		    /*% %*/
		    /*% ripper: rb_ary_new3(1, get_value($1)) %*/
		    }
#line 11466 "parse.c" /* yacc.c:1652  */
    break;

  case 685:
#line 4993 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = opt_arg_append((yyvsp[-2].node), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: rb_ary_push($1, get_value($3)) %*/
		    }
#line 11477 "parse.c" /* yacc.c:1652  */
    break;

  case 686:
#line 5002 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[0].node);
		    /*% %*/
		    /*% ripper: rb_ary_new3(1, get_value($1)) %*/
		    }
#line 11488 "parse.c" /* yacc.c:1652  */
    break;

  case 687:
#line 5009 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = opt_arg_append((yyvsp[-2].node), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: rb_ary_push($1, get_value($3)) %*/
		    }
#line 11499 "parse.c" /* yacc.c:1652  */
    break;

  case 690:
#line 5022 "parse.y" /* yacc.c:1652  */
    {
			arg_var(p, shadowing_lvar(p, get_id((yyvsp[0].id))));
		    /*%%%*/
			(yyval.id) = (yyvsp[0].id);
		    /*% %*/
		    /*% ripper: rest_param!($2) %*/
		    }
#line 11511 "parse.c" /* yacc.c:1652  */
    break;

  case 691:
#line 5030 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.id) = internal_id(p);
			arg_var(p, (yyval.id));
		    /*% %*/
		    /*% ripper: rest_param!(Qnil) %*/
		    }
#line 11523 "parse.c" /* yacc.c:1652  */
    break;

  case 694:
#line 5044 "parse.y" /* yacc.c:1652  */
    {
			arg_var(p, shadowing_lvar(p, get_id((yyvsp[0].id))));
		    /*%%%*/
			(yyval.id) = (yyvsp[0].id);
		    /*% %*/
		    /*% ripper: blockarg!($2) %*/
		    }
#line 11535 "parse.c" /* yacc.c:1652  */
    break;

  case 695:
#line 5054 "parse.y" /* yacc.c:1652  */
    {
			(yyval.id) = (yyvsp[0].id);
		    }
#line 11543 "parse.c" /* yacc.c:1652  */
    break;

  case 696:
#line 5058 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.id) = 0;
		    /*% %*/
		    /*% ripper: Qundef %*/
		    }
#line 11554 "parse.c" /* yacc.c:1652  */
    break;

  case 697:
#line 5067 "parse.y" /* yacc.c:1652  */
    {
			value_expr((yyvsp[0].node));
			(yyval.node) = (yyvsp[0].node);
		    }
#line 11563 "parse.c" /* yacc.c:1652  */
    break;

  case 698:
#line 5071 "parse.y" /* yacc.c:1652  */
    {SET_LEX_STATE(EXPR_BEG);}
#line 11569 "parse.c" /* yacc.c:1652  */
    break;

  case 699:
#line 5072 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			switch (nd_type((yyvsp[-1].node))) {
			  case NODE_STR:
			  case NODE_DSTR:
			  case NODE_XSTR:
			  case NODE_DXSTR:
			  case NODE_DREGX:
			  case NODE_LIT:
			  case NODE_ARRAY:
			  case NODE_ZARRAY:
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
#line 11595 "parse.c" /* yacc.c:1652  */
    break;

  case 701:
#line 5097 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = (yyvsp[-1].node);
		    /*% %*/
		    /*% ripper: assoclist_from_args!($1) %*/
		    }
#line 11606 "parse.c" /* yacc.c:1652  */
    break;

  case 703:
#line 5108 "parse.y" /* yacc.c:1652  */
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
		    /*% %*/
		    /*% ripper: rb_ary_push($1, get_value($3)) %*/
		    }
#line 11631 "parse.c" /* yacc.c:1652  */
    break;

  case 704:
#line 5131 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			if (nd_type((yyvsp[-2].node)) == NODE_STR) {
			    nd_set_type((yyvsp[-2].node), NODE_LIT);
			    add_mark_object(p, (yyvsp[-2].node)->nd_lit = rb_fstring((yyvsp[-2].node)->nd_lit));
			}
			(yyval.node) = list_append(p, NEW_LIST((yyvsp[-2].node), &(yyloc)), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: assoc_new!($1, $3) %*/
		    }
#line 11646 "parse.c" /* yacc.c:1652  */
    break;

  case 705:
#line 5142 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			(yyval.node) = list_append(p, NEW_LIST(NEW_LIT(ID2SYM((yyvsp[-1].id)), &(yylsp[-1])), &(yyloc)), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: assoc_new!($1, $2) %*/
		    }
#line 11657 "parse.c" /* yacc.c:1652  */
    break;

  case 706:
#line 5149 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			YYLTYPE loc = code_loc_gen(&(yylsp[-3]), &(yylsp[-1]));
			(yyval.node) = list_append(p, NEW_LIST(dsym_node(p, (yyvsp[-2].node), &loc), &loc), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: assoc_new!(dyna_symbol!($2), $4) %*/
		    }
#line 11669 "parse.c" /* yacc.c:1652  */
    break;

  case 707:
#line 5157 "parse.y" /* yacc.c:1652  */
    {
		    /*%%%*/
			if (nd_type((yyvsp[0].node)) == NODE_HASH &&
			    !((yyvsp[0].node)->nd_head && (yyvsp[0].node)->nd_head->nd_alen))
			    (yyval.node) = 0;
			else
			    (yyval.node) = list_append(p, NEW_LIST(0, &(yyloc)), (yyvsp[0].node));
		    /*% %*/
		    /*% ripper: assoc_splat!($2) %*/
		    }
#line 11684 "parse.c" /* yacc.c:1652  */
    break;

  case 733:
#line 5216 "parse.y" /* yacc.c:1652  */
    {yyerrok;token_flush(p);}
#line 11690 "parse.c" /* yacc.c:1652  */
    break;

  case 734:
#line 5217 "parse.y" /* yacc.c:1652  */
    {token_flush(p);}
#line 11696 "parse.c" /* yacc.c:1652  */
    break;

  case 736:
#line 5221 "parse.y" /* yacc.c:1652  */
    {yyerrok;}
#line 11702 "parse.c" /* yacc.c:1652  */
    break;

  case 737:
#line 5225 "parse.y" /* yacc.c:1652  */
    {
			(yyval.node) = Qnull;
		    }
#line 11710 "parse.c" /* yacc.c:1652  */
    break;


#line 11714 "parse.c" /* yacc.c:1652  */
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
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

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
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;

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


/*-----------------------------------------------------.
| yyreturn -- parsing is finished, return the result.  |
`-----------------------------------------------------*/
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
#line 5229 "parse.y" /* yacc.c:1918  */

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
# define set_yylval_str(x) set_yylval_node(NEW_STR(add_mark_object(p, (x)), &_cur_loc))
# define set_yylval_literal(x) set_yylval_node(NEW_LIT(add_mark_object(p, (x)), &_cur_loc))
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
ripper_scan_event_val(struct parser_params *p, int t)
{
    VALUE str = STR_NEW(p->lex.ptok, p->lex.pcur - p->lex.ptok);
    VALUE rval = ripper_dispatch1(p, ripper_token2eventid(t), str);
    token_flush(p);
    return rval;
}

static void
ripper_dispatch_scan_event(struct parser_params *p, int t)
{
    if (!ripper_has_scan_event(p)) return;
    add_mark_object(p, yylval_rval = ripper_scan_event_val(p, t));
}
#define dispatch_scan_event(p, t) ripper_dispatch_scan_event(p, t)

static void
ripper_dispatch_delayed_token(struct parser_params *p, int t)
{
    int saved_line = p->ruby_sourceline;
    const char *saved_tokp = p->lex.ptok;

    p->ruby_sourceline = p->delayed_line;
    p->lex.ptok = p->lex.pbeg + p->delayed_col;
    add_mark_object(p, yylval_rval = ripper_dispatch1(p, ripper_token2eventid(t), p->delayed));
    p->delayed = Qnil;
    p->ruby_sourceline = saved_line;
    p->lex.ptok = saved_tokp;
}
#define dispatch_delayed_token(p, t) ripper_dispatch_delayed_token(p, t)
#define has_delayed_token(p) (!NIL_P(p->delayed))
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
setup_token_info(token_info *ptinfo, const char *ptr, const rb_code_location_t *loc)
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

    ptinfo->linenum = loc->beg_pos.lineno;
    ptinfo->column = column;
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
    setup_token_info(ptinfo, p->lex.pbeg, loc);

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
    setup_token_info(ptinfo_end, p->lex.pbeg, loc);
    if (ptinfo_beg->linenum == ptinfo_end->linenum) return; /* ignore one-line block */
    if (ptinfo_beg->nonspc || ptinfo_end->nonspc) return; /* ignore keyword in the middle of a line */
    if (ptinfo_beg->column == ptinfo_end->column) return; /* the indents are matched */
    if (!same && ptinfo_beg->column < ptinfo_end->column) return;
    rb_warn3L(ptinfo_end->linenum,
	      "mismatched indentations at '%s' with '%s' at %d",
	      WARN_S(token), WARN_S(ptinfo_beg->token), WARN_I(ptinfo_beg->linenum));
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
	rb_str_catf(mesg, "%s%.*s%s\n""%s%s",
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
	add_mark_object(p, opt);
	tree->nd_body = prelude;
	p->ast->body.compile_option = opt;
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
	p->ruby_sourcefile_string = rb_str_new_frozen(fname);
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
	    p->delayed = rb_str_buf_new(1024);
	    rb_enc_associate(p->delayed, p->enc);
	    p->delayed_line = p->ruby_sourceline;
	    p->delayed_col = (int)(tok - p->lex.pbeg);
	}
	rb_str_buf_cat(p->delayed, tok, end - tok);
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
	p->lex.ptok = p->lex.pcur;
	yyerror0("invalid hex escape");
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
	    int string_literal, int symbol_literal, int regexp_literal)
{
    /*
     * If string_literal is true, then we allow multiple codepoints
     * in \u{}, and add the codepoints to the current token.
     * Otherwise we're parsing a character literal and return a single
     * codepoint without adding it
     */

    const int open_brace = '{', close_brace = '}';

    if (regexp_literal) { tokadd(p, '\\'); tokadd(p, 'u'); }

    if (peek(p, open_brace)) {  /* handle \u{...} form */
	int c, last = nextc(p);
	if (p->lex.pcur >= p->lex.pend) goto unterminated;
	while (ISSPACE(c = *p->lex.pcur) && ++p->lex.pcur < p->lex.pend);
	while (c != close_brace) {
	    if (regexp_literal) tokadd(p, last);
	    if (!tokadd_codepoint(p, encp, regexp_literal, TRUE)) {
		break;
	    }
	    while (ISSPACE(c = *p->lex.pcur)) {
		if (++p->lex.pcur >= p->lex.pend) goto unterminated;
		last = c;
	    }
	}

	if (c != close_brace) {
	  unterminated:
	    token_flush(p);
	    yyerror0("unterminated Unicode escape");
	    return;
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
	    rb_enc_str_buf_cat(p->delayed, p->lex.ptok, len, enc);
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
    if (nd_type(root) == NODE_ARRAY) str_node = root->nd_head;

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
	    if (nd_type(node) != NODE_ARRAY) break;
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
	    if (str) {
		rb_str_append(p->delayed, str);
	    }
	    else if ((len = p->lex.pcur - p->lex.ptok) > 0) {
		if (!(func & STR_FUNC_REGEXP) && rb_enc_asciicompat(enc)) {
		    int cr = ENC_CODERANGE_UNKNOWN;
		    rb_str_coderange_scan_restartable(p->lex.ptok, p->lex.pcur, enc, &cr);
		    if (cr != ENC_CODERANGE_7BIT &&
			p->enc == rb_usascii_encoding() &&
			enc != rb_utf8_encoding()) {
			enc = rb_ascii8bit_encoding();
		    }
		}
		rb_enc_str_buf_cat(p->delayed, p->lex.ptok, len, enc);
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
	/* not beginning of line, cannot be the terminater */
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
      case '0':
	tokadd(p, '$');
    }

    if (tokadd_ident(p, c)) return 0;
    SET_LEX_STATE(EXPR_END);
    tokenize_ident(p, last_state);
    return tGVAR;
}

static bool
parser_numbered_param(struct parser_params *p, unsigned long n)
{
    if (DVARS_TERMINAL_P(p->lvtbl->args) || DVARS_TERMINAL_P(p->lvtbl->args->prev)) {
	compile_error(p, "numbered parameter outside block");
	return false;
    }
    if (p->max_numparam < 0) {
	compile_error(p, "ordinary parameter is defined");
	return false;
    }
    set_yylval_name(numparam_id(p, (int)n));
    SET_LEX_STATE(EXPR_ARG);
    return true;
}

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
	const char *ptr = p->lex.pcur - 1;
	size_t len = p->lex.pend - ptr;
	int overflow;
	unsigned long n = ruby_scan_digits(ptr, len, 10, &len, &overflow);
	p->lex.pcur = ptr + len;
	RUBY_SET_YYLLOC(loc);
	if (result == tIVAR) {
	    if (IS_lex_state_for(last_state, EXPR_FNAME)) {
		compile_error(p, "`@%c' is not allowed as an instance variable name", c);
	    }
	    else if (ptr[0] == '0') {
		compile_error(p, "leading zero is not allowed as a numbered parameter");
	    }
	    else if (overflow || n > NUMPARAM_MAX) {
		compile_error(p, "too large numbered parameter");
	    }
	    else if (parser_numbered_param(p, n)) {
		return tNUMPARAM;
	    }
	}
	else {
	    compile_error(p, "`@@%c' is not allowed as a class variable name", c);
	}
	parser_show_error_line(p, &loc);
	set_yylval_noname();
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
	last_state = p->lex.state;
	c = nextc(p);
	if (c == '<' &&
	    !IS_lex_state(EXPR_DOT | EXPR_CLASS) &&
	    !IS_END() &&
	    (!IS_ARG() || IS_lex_state(EXPR_LABELED) || space_seen)) {
	    int token = heredoc_identifier(p);
	    if (token) return token;
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
	if (lambda_beginning_p()) {
	    SET_LEX_STATE(EXPR_BEG);
	    COND_PUSH(0);
	    CMDARG_PUSH(0);
	    p->lex.paren_nest++;
	    return tLAMBEG;
	}
	p->lex.paren_nest++;
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
	SET_LEX_STATE(c != tLBRACE ? EXPR_BEG : EXPR_BEG|EXPR_LABEL);
	if (c != tLBRACE) p->command_start = TRUE;
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
    NODE *n = rb_ast_newnode(p->ast);

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
	NODE *node = NEW_DSTR(add_mark_object(p, STR_NEW0()), loc);
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
	    nd_set_type(tail, NODE_ARRAY);
	    tail->nd_head = NEW_STR(tail->nd_lit, loc);
	    list_concat(head, tail);
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
	node = list_append(p, NEW_DSTR(add_mark_object(p, STR_NEW0()), &node->nd_loc), node);
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

#define WARN_LOCATION(type) do { \
    if (p->warn_location) { \
	rb_warning0(type" in eval may not return location in binding;" \
		    " use Binding#source_location instead"); \
    } \
} while (0)

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
	    node = NEW_STR(add_mark_object(p, file), loc);
	}
	return node;
      case keyword__LINE__:
	WARN_LOCATION("__LINE__");
	return NEW_LIT(INT2FIX(p->tokline), loc);
      case keyword__ENCODING__:
	return NEW_LIT(add_mark_object(p, rb_enc_from_encoding(p->enc)), loc);
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
	    if (id == p->cur_arg) {
		rb_warn1("circular argument reference - %"PRIsWARN, rb_id2str(id));
	    }
	    if (vidp) *vidp |= LVAR_USED;
	    node = NEW_DVAR(id, loc);
	    return node;
	}
	if (local_id_ref(p, id, &vidp)) {
	    if (id == p->cur_arg) {
		rb_warn1("circular argument reference - %"PRIsWARN, rb_id2str(id));
	    }
	    if (vidp) *vidp |= LVAR_USED;
	    node = NEW_LVAR(id, loc);
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
	symbol->nd_lit = add_mark_object(p, rb_str_intern(symbol->nd_lit));
    }
    return list_append(p, symbols, symbol);
}

static NODE *
new_regexp(struct parser_params *p, NODE *node, int options, const YYLTYPE *loc)
{
    NODE *list, *prev;
    VALUE lit;

    if (!node) {
	return NEW_LIT(add_mark_object(p, reg_compile(p, STR_NEW0(), options)), loc);
    }
    switch (nd_type(node)) {
      case NODE_STR:
	{
	    VALUE src = node->nd_lit;
	    nd_set_type(node, NODE_LIT);
	    nd_set_loc(node, loc);
	    add_mark_object(p, node->nd_lit = reg_compile(p, src, options));
	}
	break;
      default:
	add_mark_object(p, lit = STR_NEW0());
	node = NEW_NODE(NODE_DSTR, lit, 1, NEW_LIST(node, loc), loc);
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
	    add_mark_object(p, node->nd_lit = reg_compile(p, src, options));
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
	add_mark_object(p, lit);
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
	arg->nd_lit = add_mark_object(p, lit);
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
	    if (dyna_in_block(p) && dvar_defined(p, id)) return 1;
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
static const char rb_parser_lex_state_names[][13] = {
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
      case NODE_ARRAY:
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
        if (nd_type(node1->nd_body) != NODE_ARRAY) break;
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
	if (nd_type(node2) != NODE_ARRAY) break;
	node1->nd_body = list_concat(NEW_LIST(node1->nd_body, loc), node2);
	nd_set_type(node1, NODE_ARGSCAT);
	return node1;
      case NODE_ARGSCAT:
	if (nd_type(node2) != NODE_ARRAY ||
	    nd_type(node1->nd_body) != NODE_ARRAY) break;
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
    if ((nd_type(rest_arg) == NODE_ARRAY) && (n1 = splat_array(args)) != 0) {
	return list_concat(n1, rest_arg);
    }
    return arg_concat(p, args, rest_arg, loc);
}

static NODE *
splat_array(NODE* node)
{
    if (nd_type(node) == NODE_SPLAT) node = node->nd_head;
    if (nd_type(node) == NODE_ARRAY) return node;
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

static int
value_expr_gen(struct parser_params *p, NODE *node)
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
	    if (!cond) yyerror1(&node->nd_loc, "void value expression");
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
	  case NODE_UNLESS:
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

	  case NODE_LASGN:
	  case NODE_DASGN:
	  case NODE_DASGN_CURR:
	  case NODE_MASGN:
	    mark_lvar_used(p, node);
	    return TRUE;

	  default:
	    return TRUE;
	}
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

static void
warn_unless_e_option(struct parser_params *p, NODE *node, const char *str)
{
    if (!e_option_supplied(p)) parser_warn(p, node, str);
}

static void
warning_unless_e_option(struct parser_params *p, NODE *node, const char *str)
{
    if (!e_option_supplied(p)) parser_warning(p, node, str);
}

static NODE *cond0(struct parser_params*,NODE*,int,const YYLTYPE*);

static NODE*
range_op(struct parser_params *p, NODE *node, const YYLTYPE *loc)
{
    enum node_type type;

    if (node == 0) return 0;

    type = nd_type(node);
    value_expr(node);
    if (type == NODE_LIT && FIXNUM_P(node->nd_lit)) {
	warn_unless_e_option(p, node, "integer literal in conditional range");
	return NEW_CALL(node, tEQ, NEW_LIST(NEW_GVAR(rb_intern("$."), loc), loc), loc);
    }
    return cond0(p, node, FALSE, loc);
}

static int
literal_node(NODE *node)
{
    if (!node) return 1;	/* same as NODE_NIL */
    if (!(node = nd_once_body(node))) return 1;
    switch (nd_type(node)) {
      case NODE_LIT:
      case NODE_STR:
      case NODE_DSTR:
      case NODE_EVSTR:
      case NODE_DREGX:
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
cond0(struct parser_params *p, NODE *node, int method_op, const YYLTYPE *loc)
{
    if (node == 0) return 0;
    if (!(node = nd_once_body(node))) return 0;
    assign_in_cond(p, node);

    switch (nd_type(node)) {
      case NODE_DSTR:
      case NODE_EVSTR:
      case NODE_STR:
	if (!method_op) rb_warn0("string literal in condition");
	break;

      case NODE_DREGX:
	{
	    if (!method_op)
		warning_unless_e_option(p, node, "regex literal in condition");

	    return NEW_MATCH2(node, NEW_GVAR(idLASTLINE, loc), loc);
	}

      case NODE_AND:
      case NODE_OR:
	node->nd_1st = cond0(p, node->nd_1st, FALSE, loc);
	node->nd_2nd = cond0(p, node->nd_2nd, FALSE, loc);
	break;

      case NODE_DOT2:
      case NODE_DOT3:
	node->nd_beg = range_op(p, node->nd_beg, loc);
	node->nd_end = range_op(p, node->nd_end, loc);
	if (nd_type(node) == NODE_DOT2 || nd_type(node) == NODE_DOT3) {
	    nd_set_type(node, nd_type(node) == NODE_DOT2 ? NODE_FLIP2 : NODE_FLIP3);
	    parser_warn(p, node, "flip-flop is deprecated");
	}
	if (!method_op && !e_option_supplied(p)) {
	    int b = literal_node(node->nd_beg);
	    int e = literal_node(node->nd_end);
	    if ((b == 1 && e == 1) || (b + e >= 2 && RTEST(ruby_verbose))) {
		parser_warn(p, node, "range literal in condition");
	    }
	}
	break;

      case NODE_DSYM:
	if (!method_op) parser_warning(p, node, "literal in condition");
	break;

      case NODE_LIT:
	if (RB_TYPE_P(node->nd_lit, T_REGEXP)) {
	    if (!method_op)
		warn_unless_e_option(p, node, "regex literal in condition");
	    nd_set_type(node, NODE_MATCH);
	}
	else if (node->nd_lit == Qtrue ||
		 node->nd_lit == Qfalse) {
	    /* booleans are OK, e.g., while true */
	}
	else {
	    if (!method_op)
		parser_warning(p, node, "literal in condition");
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
    return cond0(p, node, FALSE, loc);
}

static NODE*
method_cond(struct parser_params *p, NODE *node, const YYLTYPE *loc)
{
    if (node == 0) return 0;
    return cond0(p, node, TRUE, loc);
}

static NODE*
new_if(struct parser_params *p, NODE *cc, NODE *left, NODE *right, const YYLTYPE *loc)
{
    if (!cc) return right;
    cc = cond0(p, cc, FALSE, loc);
    return newline_node(NEW_IF(cc, left, right, loc));
}

static NODE*
new_unless(struct parser_params *p, NODE *cc, NODE *left, NODE *right, const YYLTYPE *loc)
{
    if (!cc) return right;
    cc = cond0(p, cc, FALSE, loc);
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
    struct rb_args_info *args;
    NODE *node;
    rb_imemo_tmpbuf_t *tmpbuf = new_tmpbuf();

    args = ZALLOC(struct rb_args_info);
    tmpbuf->ptr = (VALUE *)args;
    node = NEW_NODE(NODE_ARGS, 0, 0, args, &NULL_LOC);
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
    else if (kw_rest_arg) {
	args->kw_rest_arg = NEW_DVAR(kw_rest_arg, loc);
    }

    p->ruby_sourceline = saved_line;
    return node;
}

static NODE *
args_with_numbered(struct parser_params *p, NODE *args, int max_numparam)
{
    if (max_numparam > 0) {
	if (!args) args = new_args_tail(p, 0, 0, 0, 0);
	args->nd_ainfo->pre_args_num = max_numparam;
	args->nd_ainfo->rest_arg = excessed_comma;
    }
    return args;
}

ID
rb_parser_numparam_id(struct parser_params *p, int idx)
{
    struct vtable *args;
    if (idx <= 0) return (ID)0;
    if (p->max_numparam < idx) {
	p->max_numparam = idx;
    }
    args = p->lvtbl->args;
    while (idx > args->pos) {
	vtable_add(args, internal_id(p));
    }
    return args->tbl[idx-1];
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
	} else {
	    apinfo->pre_args = pre_args;
	}
    }
    return aryptn;
}

static NODE*
new_array_pattern_tail(struct parser_params *p, NODE *pre_args, int has_rest, ID rest_arg, NODE *post_args, const YYLTYPE *loc)
{
    int saved_line = p->ruby_sourceline;
    struct rb_ary_pattern_info *apinfo;
    NODE *node;
    rb_imemo_tmpbuf_t *tmpbuf = new_tmpbuf();

    apinfo = ZALLOC(struct rb_ary_pattern_info);
    tmpbuf->ptr = (VALUE *)apinfo;
    node = NEW_NODE(NODE_ARYPTN, 0, 0, apinfo, loc);

    apinfo->pre_args = pre_args;

    if (has_rest) {
	if (rest_arg) {
	    apinfo->rest_arg = assignable(p, rest_arg, 0, loc);
	} else {
	    apinfo->rest_arg = NODE_SPECIAL_NO_NAME_REST;
	}
    } else {
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

    if (kw_rest_arg) {
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
	add_mark_object(p, node->nd_lit = ID2SYM(rb_intern_str(lit)));
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
	if (st_lookup(literal_keys, (key = head->nd_lit), 0)) {
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

    args = make_array(args, args_loc);
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
    int inherits_dvars = toplevel_scope && (compile_for_eval || p->in_main /* is p->in_main really needed? */);
    int warn_unused_vars = RTEST(ruby_verbose);

    local = ALLOC(struct local_vars);
    local->prev = p->lvtbl;
    local->args = vtable_alloc(0);
    local->vars = vtable_alloc(inherits_dvars ? DVARS_INHERIT : DVARS_TOPSCOPE);
#ifndef RIPPER
    if (toplevel_scope && compile_for_eval) warn_unused_vars = 0;
    if (toplevel_scope && e_option_supplied(p)) warn_unused_vars = 0;
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
    rb_imemo_tmpbuf_t *tmpbuf = new_tmpbuf();

    if (cnt <= 0) return 0;
    buf = ALLOC_N(ID, cnt + 1);
    tmpbuf->ptr = (void *)buf;
    MEMCPY(buf+1, p->lvtbl->args->tbl, ID, cnt_args);
    /* remove IDs duplicated to warn shadowing */
    for (i = 0, j = cnt_args+1; i < cnt_vars; ++i) {
	ID id = p->lvtbl->vars->tbl[i];
	if (!vtable_included(p->lvtbl->args, id)) {
	    buf[j++] = id;
	}
    }
    if (--j < cnt) tmpbuf->ptr = (void *)REALLOC_N(buf, ID, (cnt = j) + 1);
    buf[0] = cnt;

    return buf;
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
	return rb_local_defined(id, p->base_block);
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
        return rb_dvar_defined(id, p->base_block);
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
				NEW_ARRAY(NEW_GVAR(idLASTLINE, LOC), LOC),
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
    p->delayed = Qnil;
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
    rb_gc_mark(p->delayed);
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
rb_parser_set_context(VALUE vparser, const struct rb_block *base, int main)
{
    struct parser_params *p;

    TypedData_Get_Struct(vparser, struct parser_params, &parser_data_type, p);
    p->error_buffer = main ? Qfalse : Qnil;
    p->base_block = base;
    p->in_main = main;
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
    id_or = rb_intern_const("|");

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
    rb_define_method(rb_mKernel, "assert_Qundef", ripper_assert_Qundef, 2);
    rb_define_method(rb_mKernel, "rawVALUE", ripper_value, 1);
    rb_define_method(rb_mKernel, "validate_object", ripper_validate_object, 1);
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
