/* A Bison parser, made by GNU Bison 3.5.1.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2020 Free Software Foundation,
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
#define YYBISON_VERSION "3.5.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 1

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 12 "ripper.y"


#if !YYPURE
# error needs pure parser
#endif
#ifndef PARSER_DEBUG
#define PARSER_DEBUG 0
#endif
#define YYDEBUG 1
#define YYERROR_VERBOSE 1
#define YYSTACK_USE_ALLOCA 0
#define YYLTYPE rb_code_range_t
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

#define YYMALLOC(size)		rb_parser_malloc(parser, (size))
#define YYREALLOC(ptr, size)	rb_parser_realloc(parser, (ptr), (size))
#define YYCALLOC(nelem, size)	rb_parser_calloc(parser, (nelem), (size))
#define YYFREE(ptr)		rb_parser_free(parser, (ptr))
#define YYFPRINTF		rb_parser_printf
#define YY_LOCATION_PRINT(File, Loc) \
     rb_parser_printf(parser, "%d.%d-%d.%d", \
		      (Loc).first_loc.lineno, (Loc).first_loc.column,\
		      (Loc).last_loc.lineno, (Loc).last_loc.column)
#define YYLLOC_DEFAULT(Current, Rhs, N)					\
    do									\
      if (N)								\
	{								\
	  (Current).first_loc = YYRHSLOC(Rhs, 1).first_loc;		\
	  (Current).last_loc  = YYRHSLOC(Rhs, N).last_loc;		\
	}								\
      else								\
	RUBY_SET_YYLLOC_OF_NONE(Current);				\
    while (0)

#define RUBY_SET_YYLLOC_FROM_STRTERM_HEREDOC(Current)			\
    rb_parser_set_location_from_strterm_heredoc(parser, &lex_strterm->u.heredoc, &(Current))
#define RUBY_SET_YYLLOC_OF_NONE(Current)					\
    rb_parser_set_location_of_none(parser, &(Current))
#define RUBY_SET_YYLLOC(Current)					\
    rb_parser_set_location(parser, &(Current))

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
    (lex_state = \
     (yydebug ? \
      rb_parser_trace_lex_state(parser, lex_state, (ls), __LINE__) : \
      (enum lex_state_e)(ls)))

typedef VALUE stack_type;

# define SHOW_BITSTACK(stack, name) (yydebug ? rb_parser_show_bitstack(parser, stack, name, __LINE__) : (void)0)
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

 lex_pbeg      tokp         lex_p        lex_pend
    |           |              |            |
    |-----------+--------------+------------|
                |<------------>|
                     token
*/
struct parser_params {
    rb_imemo_alloc_t *heap;

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
    VALUE debug_output;

    ID cur_arg;

    rb_ast_t *ast;

    unsigned int command_start:1;
    unsigned int eofp: 1;
    unsigned int ruby__end__seen: 1;
    unsigned int yydebug: 1;
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

    NODE *eval_tree_begin;
    NODE *eval_tree;
    VALUE error_buffer;
    VALUE debug_lines;
    VALUE coverage;
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

#define intern_cstr(n,l,en) rb_intern3(n,l,en)

#define STR_NEW(p,n) rb_enc_str_new((p),(n),current_enc)
#define STR_NEW0() rb_enc_str_new(0,0,current_enc)
#define STR_NEW2(p) rb_enc_str_new((p),strlen(p),current_enc)
#define STR_NEW3(p,n,e,func) parser_str_new((p),(n),(e),(func),current_enc)
#define TOK_INTERN() intern_cstr(tok(), toklen(), current_enc)

static int parser_yyerror(struct parser_params*, const char*);
#define yyerror0(msg) parser_yyerror(parser, (msg))
#define yyerror(yylloc, parser, msg) yyerror0(msg)
#define token_flush(p) ((p)->lex.ptok = (p)->lex.pcur)

#define lex_strterm		(parser->lex.strterm)
#define lex_state		(parser->lex.state)
#define cond_stack		(parser->cond_stack)
#define cmdarg_stack		(parser->cmdarg_stack)
#define paren_nest		(parser->lex.paren_nest)
#define lpar_beg		(parser->lex.lpar_beg)
#define brace_nest		(parser->lex.brace_nest)
#define in_def			(parser->in_def)
#define in_class		(parser->in_class)
#define in_main 		(parser->in_main)
#define in_defined		(parser->in_defined)
#define tokenbuf		(parser->tokenbuf)
#define tokidx			(parser->tokidx)
#define toksiz			(parser->toksiz)
#define tokline 		(parser->tokline)
#define lex_input		(parser->lex.input)
#define lex_prevline		(parser->lex.prevline)
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
#define tokp			lex.ptok

#define token_column		((int)(parser->tokp - lex_pbeg))

#define CALL_Q_P(q) ((q) == TOKEN2VAL(tANDDOT))
#define NODE_CALL_Q(q) (CALL_Q_P(q) ? NODE_QCALL : NODE_CALL)
#define NEW_QCALL(q,r,m,a) NEW_NODE(NODE_CALL_Q(q),r,m,a)

#define lambda_beginning_p() (lpar_beg && lpar_beg == paren_nest)

static enum yytokentype yylex(YYSTYPE*, YYLTYPE*, struct parser_params*);

#ifndef RIPPER
static inline void
rb_discard_node_gen(struct parser_params *parser, NODE *n)
{
    rb_ast_delete_node(parser->ast, n);
}
#define rb_discard_node(n) rb_discard_node_gen(parser, (n))
#endif

static inline void
add_mark_object_gen(struct parser_params *parser, VALUE obj)
{
    if (!SPECIAL_CONST_P(obj)
#ifdef RIPPER
	&& !RB_TYPE_P(obj, T_NODE) /* Ripper jumbles NODE objects and other objects... */
#endif
    ) {
	rb_ast_add_mark_object(parser->ast, obj);
    }
}
#define add_mark_object(obj) add_mark_object_gen(parser, (obj))

static NODE* node_newnode(struct parser_params *, enum node_type, VALUE, VALUE, VALUE);
#define rb_node_newnode(type, a1, a2, a3) node_newnode(parser, (type), (a1), (a2), (a3))

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

static NODE *cond_gen(struct parser_params*,NODE*,int,const YYLTYPE*);
#define cond(node,location) cond_gen(parser, (node), FALSE, location)
#define method_cond(node,location) cond_gen(parser, (node), TRUE, location)
static NODE *new_nil_gen(struct parser_params*,const YYLTYPE*);
#define new_nil(location) new_nil_gen(parser,location)
static NODE *new_if_gen(struct parser_params*,NODE*,NODE*,NODE*,const YYLTYPE*);
#define new_if(cc,left,right,location) new_if_gen(parser, (cc), (left), (right), (location))
static NODE *new_unless_gen(struct parser_params*,NODE*,NODE*,NODE*,const YYLTYPE*);
#define new_unless(cc,left,right,location) new_unless_gen(parser, (cc), (left), (right), (location))
static NODE *logop_gen(struct parser_params*,enum node_type,NODE*,NODE*,const YYLTYPE*,const YYLTYPE*);
#define logop(id,node1,node2,op_loc,location) \
    logop_gen(parser, ((id)==idAND||(id)==idANDOP)?NODE_AND:NODE_OR, \
	      (node1), (node2), (op_loc), (location))

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

static NODE *block_append_gen(struct parser_params*,NODE*,NODE*,const YYLTYPE*);
#define block_append(h,t,location) block_append_gen(parser,(h),(t),(location))
static NODE *list_append_gen(struct parser_params*,NODE*,NODE*);
#define list_append(l,i) list_append_gen(parser,(l),(i))
static NODE *list_concat(NODE*,NODE*);
static NODE *arg_append_gen(struct parser_params*,NODE*,NODE*,const YYLTYPE*);
#define arg_append(h,t,location) arg_append_gen(parser,(h),(t),(location))
static NODE *arg_concat_gen(struct parser_params*,NODE*,NODE*,const YYLTYPE*);
#define arg_concat(h,t,location) arg_concat_gen(parser,(h),(t),(location))
static NODE *literal_concat_gen(struct parser_params*,NODE*,NODE*,const YYLTYPE*);
#define literal_concat(h,t,location) literal_concat_gen(parser,(h),(t),(location))
static int literal_concat0(struct parser_params *, VALUE, VALUE);
static NODE *new_evstr_gen(struct parser_params*,NODE*,const YYLTYPE*);
#define new_evstr(n, location) new_evstr_gen(parser,(n),(location))
static NODE *evstr2dstr_gen(struct parser_params*,NODE*);
#define evstr2dstr(n) evstr2dstr_gen(parser,(n))
static NODE *splat_array(NODE*);

static NODE *call_bin_op_gen(struct parser_params*,NODE*,ID,NODE*,const YYLTYPE*,const YYLTYPE*);
#define call_bin_op(recv,id,arg1,op_loc,location) call_bin_op_gen(parser, (recv),(id),(arg1),(op_loc),(location))
static NODE *call_uni_op_gen(struct parser_params*,NODE*,ID,const YYLTYPE*,const YYLTYPE*);
#define call_uni_op(recv,id,op_loc,location) call_uni_op_gen(parser, (recv),(id),(op_loc),(location))
static NODE *new_qcall_gen(struct parser_params* parser, ID atype, NODE *recv, ID mid, NODE *args, const YYLTYPE *location);
#define new_qcall(q,r,m,a,location) new_qcall_gen(parser,q,r,m,a,location)
#define new_command_qcall(q,r,m,a,location) new_qcall_gen(parser,q,r,m,a,location)
static NODE *new_command_gen(struct parser_params*parser, NODE *m, NODE *a) {m->nd_args = a; return m;}
#define new_command(m,a) new_command_gen(parser, m, a)
static NODE *method_add_block_gen(struct parser_params*parser, NODE *m, NODE *b) {b->nd_iter = m; return b;}
#define method_add_block(m,b) method_add_block_gen(parser, m, b)

static NODE *new_args_gen(struct parser_params*,NODE*,NODE*,ID,NODE*,NODE*,const YYLTYPE*);
#define new_args(f,o,r,p,t,location) new_args_gen(parser, (f),(o),(r),(p),(t),(location))
static NODE *new_args_tail_gen(struct parser_params*,NODE*,ID,ID,const YYLTYPE*);
#define new_args_tail(k,kr,b,location) new_args_tail_gen(parser, (k),(kr),(b),(location))
static NODE *new_kw_arg_gen(struct parser_params *parser, NODE *k, const YYLTYPE *location);
#define new_kw_arg(k,location) new_kw_arg_gen(parser, k, location)

static VALUE negate_lit_gen(struct parser_params*, VALUE);
#define negate_lit(lit) negate_lit_gen(parser, lit)
static NODE *ret_args_gen(struct parser_params*,NODE*);
#define ret_args(node) ret_args_gen(parser, (node))
static NODE *arg_blk_pass(NODE*,NODE*);
static NODE *new_yield_gen(struct parser_params*,NODE*,const YYLTYPE*);
#define new_yield(node,location) new_yield_gen(parser, (node), (location))
static NODE *dsym_node_gen(struct parser_params*,NODE*,const YYLTYPE*);
#define dsym_node(node,location) dsym_node_gen(parser, (node), (location))

static NODE *gettable_gen(struct parser_params*,ID,const YYLTYPE*);
#define gettable(id,location) gettable_gen(parser,(id),(location))
static NODE *assignable_gen(struct parser_params*,ID,NODE*,const YYLTYPE*);
#define assignable(id,node,location) assignable_gen(parser, (id), (node), (location))

static NODE *aryset_gen(struct parser_params*,NODE*,NODE*,const YYLTYPE*);
#define aryset(node1,node2,location) aryset_gen(parser, (node1), (node2), (location))
static NODE *attrset_gen(struct parser_params*,NODE*,ID,ID,const YYLTYPE*);
#define attrset(node,q,id,location) attrset_gen(parser, (node), (q), (id), (location))

static void rb_backref_error_gen(struct parser_params*,NODE*);
#define rb_backref_error(n) rb_backref_error_gen(parser,(n))
static NODE *node_assign_gen(struct parser_params*,NODE*,NODE*,const YYLTYPE*);
#define node_assign(node1, node2, location) node_assign_gen(parser, (node1), (node2), (location))

static NODE *new_op_assign_gen(struct parser_params *parser, NODE *lhs, ID op, NODE *rhs, const YYLTYPE *location);
#define new_op_assign(lhs, op, rhs, location) new_op_assign_gen(parser, (lhs), (op), (rhs), (location))
static NODE *new_attr_op_assign_gen(struct parser_params *parser, NODE *lhs, ID atype, ID attr, ID op, NODE *rhs, const YYLTYPE *location);
#define new_attr_op_assign(lhs, type, attr, op, rhs, location) new_attr_op_assign_gen(parser, (lhs), (type), (attr), (op), (rhs), (location))
static NODE *new_const_op_assign_gen(struct parser_params *parser, NODE *lhs, ID op, NODE *rhs, const YYLTYPE *location);
#define new_const_op_assign(lhs, op, rhs, location) new_const_op_assign_gen(parser, (lhs), (op), (rhs), (location))

static NODE *const_path_field_gen(struct parser_params *parser, NODE *head, ID mid, const YYLTYPE *location);
#define const_path_field(w, n, location) const_path_field_gen(parser, w, n, location)
#define top_const_field(n) NEW_COLON3(n)
static NODE *const_decl_gen(struct parser_params *parser, NODE* path, const YYLTYPE *location);
#define const_decl(path, location) const_decl_gen(parser, path, location)

#define var_field(n) (n)
#define backref_assign_error(n, a, location) (rb_backref_error(n), new_begin(0, location))

static NODE *opt_arg_append(NODE*, NODE*);
static NODE *kwd_append(NODE*, NODE*);

static NODE *new_hash_gen(struct parser_params *parser, NODE *hash, const YYLTYPE *location);
#define new_hash(hash, location) new_hash_gen(parser, (hash), location)

static NODE *new_defined_gen(struct parser_params *parser, NODE *expr, const YYLTYPE *location);
#define new_defined(expr, location) new_defined_gen(parser, expr, location)

static NODE *new_regexp_gen(struct parser_params *, NODE *, int, const YYLTYPE *);
#define new_regexp(node, opt, location) new_regexp_gen(parser, node, opt, location)

static NODE *new_lit_gen(struct parser_params *parser, VALUE sym, const YYLTYPE *location);
#define new_lit(sym, location) new_lit_gen(parser, sym, location)

static NODE *new_list_gen(struct parser_params *parser, NODE *item, const YYLTYPE *location);
#define new_list(item, location) new_list_gen(parser, item, location)

static NODE *new_str_gen(struct parser_params *parser, VALUE str, const YYLTYPE *location);
#define new_str(s,location) new_str_gen(parser, s, location)

static NODE *new_dvar_gen(struct parser_params *parser, ID id, const YYLTYPE *location);
#define new_dvar(id, location) new_dvar_gen(parser, id, location)

static NODE *new_resbody_gen(struct parser_params *parser, NODE *exc_list, NODE *stmt, NODE *rescue, const YYLTYPE *location);
#define new_resbody(e,s,r,location) new_resbody_gen(parser, (e),(s),(r),(location))

static NODE *new_errinfo_gen(struct parser_params *parser, const YYLTYPE *location);
#define new_errinfo(location) new_errinfo_gen(parser, location)

static NODE *new_call_gen(struct parser_params *parser, NODE *recv, ID mid, NODE *args, const YYLTYPE *location);
#define new_call(recv,mid,args,location) new_call_gen(parser, recv,mid,args,location)

static NODE *new_fcall_gen(struct parser_params *parser, ID mid, NODE *args, const YYLTYPE *location);
#define new_fcall(mid,args,location) new_fcall_gen(parser, mid, args, location)

static NODE *new_for_gen(struct parser_params *parser, NODE *var, NODE *iter, NODE *body, const YYLTYPE *location);
#define new_for(var,iter,body,location) new_for_gen(parser, var, iter, body, location)

static NODE *new_gvar_gen(struct parser_params *parser, ID id, const YYLTYPE *location);
#define new_gvar(id, location) new_gvar_gen(parser, id, location)

static NODE *new_lvar_gen(struct parser_params *parser, ID id, const YYLTYPE *location);
#define new_lvar(id, location) new_lvar_gen(parser, id, location)

static NODE *new_dstr_gen(struct parser_params *parser, VALUE str, const YYLTYPE *location);
#define new_dstr(s, location) new_dstr_gen(parser, s, location)

static NODE *new_rescue_gen(struct parser_params *parser, NODE *b, NODE *res, NODE *e, const YYLTYPE *location);
#define new_rescue(b,res,e,location) new_rescue_gen(parser,b,res,e,location)

static NODE *new_undef_gen(struct parser_params *parser, NODE *i, const YYLTYPE *location);
#define new_undef(i, location) new_undef_gen(parser, i, location)

static NODE *nd_set_loc(NODE *nd, const YYLTYPE *location);
static NODE *new_zarray_gen(struct parser_params *parser, const YYLTYPE *location);
#define new_zarray(location) new_zarray_gen(parser, location)
#define make_array(ary, location) ((ary) ? (nd_set_loc(ary, location), ary) : new_zarray(location))

static NODE *new_ivar_gen(struct parser_params *parser, ID id, const YYLTYPE *location);
#define new_ivar(id, location) new_ivar_gen(parser,id,location)

static NODE *new_postarg_gen(struct parser_params *parser, NODE *i, NODE *v, const YYLTYPE *location);
#define new_postarg(i,v,location) new_postarg_gen(parser,i,v,location)

static NODE *new_cdecl_gen(struct parser_params *parser, ID v, NODE *val, NODE *path, const YYLTYPE *location);
#define new_cdecl(v,val,path,location) new_cdecl_gen(parser,v,val,path,location)

static NODE *new_scope_gen(struct parser_params *parser, NODE *a, NODE *b, const YYLTYPE *location);
#define new_scope(a,b,location) new_scope_gen(parser,a,b,location)

static NODE *new_begin_gen(struct parser_params *parser, NODE *b, const YYLTYPE *location);
#define new_begin(b,location) new_begin_gen(parser,b,location)

static NODE *new_masgn_gen(struct parser_params *parser, NODE *l, NODE *r, const YYLTYPE *location);
#define new_masgn(l,r,location) new_masgn_gen(parser,l,r,location)

static NODE *new_xstring_gen(struct parser_params *, NODE *, const YYLTYPE *location);
#define new_xstring(node, location) new_xstring_gen(parser, node, location)
#define new_string1(str) (str)

static NODE *new_body_gen(struct parser_params *parser, NODE *param, NODE *stmt, const YYLTYPE *location);
#define new_brace_body(param, stmt, location) new_body_gen(parser, param, stmt, location)
#define new_do_body(param, stmt, location) new_body_gen(parser, param, stmt, location)

static NODE *match_op_gen(struct parser_params*,NODE*,NODE*,const YYLTYPE*,const YYLTYPE*);
#define match_op(node1,node2,op_loc,location) match_op_gen(parser, (node1), (node2), (op_loc), (location))

static ID  *local_tbl_gen(struct parser_params*);
#define local_tbl() local_tbl_gen(parser)

static VALUE reg_compile_gen(struct parser_params*, VALUE, int);
#define reg_compile(str,options) reg_compile_gen(parser, (str), (options))
static void reg_fragment_setenc_gen(struct parser_params*, VALUE, int);
#define reg_fragment_setenc(str,options) reg_fragment_setenc_gen(parser, (str), (options))
static int reg_fragment_check_gen(struct parser_params*, VALUE, int);
#define reg_fragment_check(str,options) reg_fragment_check_gen(parser, (str), (options))
static NODE *reg_named_capture_assign_gen(struct parser_params* parser, VALUE regexp, const YYLTYPE *location);
#define reg_named_capture_assign(regexp,location) reg_named_capture_assign_gen(parser,(regexp),location)

static NODE *parser_heredoc_dedent(struct parser_params*,NODE*);
# define heredoc_dedent(str) parser_heredoc_dedent(parser, (str))

#define get_id(id) (id)
#define get_value(val) (val)
#else  /* RIPPER */
#define NODE_RIPPER NODE_CDECL

static inline VALUE
ripper_new_yylval_gen(struct parser_params *parser, ID a, VALUE b, VALUE c)
{
    add_mark_object(b);
    add_mark_object(c);
    return (VALUE)NEW_CDECL(a, b, c);
}
#define ripper_new_yylval(a, b, c) ripper_new_yylval_gen(parser, a, b, c)

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
#define assignable(lhs,node,location) assignable_gen(parser, (lhs))
static int id_is_var_gen(struct parser_params *parser, ID id);
#define id_is_var(id) id_is_var_gen(parser, (id))

#define method_cond(node,location) (node)
#define call_bin_op(recv,id,arg1,op_loc,location) dispatch3(binary, (recv), STATIC_ID2SYM(id), (arg1))
#define match_op(node1,node2,op_loc,location) call_bin_op((node1), idEqTilde, (node2), op_loc, location)
#define call_uni_op(recv,id,op_loc,location) dispatch2(unary, STATIC_ID2SYM(id), (recv))
#define logop(id,node1,node2,op_loc,location) call_bin_op((node1), (id), (node2), op_loc, location)
#define node_assign(node1, node2, location) dispatch2(assign, (node1), (node2))
static VALUE new_qcall_gen(struct parser_params *parser, VALUE q, VALUE r, VALUE m, VALUE a);
#define new_qcall(q,r,m,a,location) new_qcall_gen(parser, (r), (q), (m), (a))
#define new_command_qcall(q,r,m,a,location) dispatch4(command_call, (r), (q), (m), (a))
#define new_command_call(q,r,m,a) dispatch4(command_call, (r), (q), (m), (a))
#define new_command(m,a) dispatch2(command, (m), (a));

#define new_nil(location) Qnil
static VALUE new_op_assign_gen(struct parser_params *parser, VALUE lhs, VALUE op, VALUE rhs);
#define new_op_assign(lhs, op, rhs, location) new_op_assign_gen(parser, (lhs), (op), (rhs))
static VALUE new_attr_op_assign_gen(struct parser_params *parser, VALUE lhs, VALUE type, VALUE attr, VALUE op, VALUE rhs);
#define new_attr_op_assign(lhs, type, attr, op, rhs, location) new_attr_op_assign_gen(parser, (lhs), (type), (attr), (op), (rhs))
#define new_const_op_assign(lhs, op, rhs, location) new_op_assign(lhs, op, rhs, location)

static VALUE new_regexp_gen(struct parser_params *, VALUE, VALUE);
#define new_regexp(node, opt, location) new_regexp_gen(parser, node, opt)

static VALUE new_xstring_gen(struct parser_params *, VALUE);
#define new_xstring(str, location) new_xstring_gen(parser, str)
#define new_string1(str) dispatch1(string_literal, str)

#define new_brace_body(param, stmt, location) dispatch2(brace_block, escape_Qundef(param), stmt)
#define new_do_body(param, stmt, location) dispatch2(do_block, escape_Qundef(param), stmt)

#define const_path_field(w, n, location) dispatch2(const_path_field, (w), (n))
#define top_const_field(n) dispatch1(top_const_field, (n))
static VALUE const_decl_gen(struct parser_params *parser, VALUE path);
#define const_decl(path, location) const_decl_gen(parser, path)

static VALUE var_field_gen(struct parser_params *parser, VALUE a);
#define var_field(a) var_field_gen(parser, (a))
static VALUE assign_error_gen(struct parser_params *parser, VALUE a);
#define assign_error(a) assign_error_gen(parser, (a))
#define backref_assign_error(n, a, location) assign_error(a)

#define block_dup_check(n1,n2) ((void)(n1), (void)(n2))
#define fixpos(n1,n2) ((void)(n1), (void)(n2))
#undef nd_set_line
#define nd_set_line(n,l) ((void)(n))

static VALUE parser_reg_compile(struct parser_params*, VALUE, int, VALUE *);

#endif /* !RIPPER */

/* forward declaration */
typedef struct rb_strterm_heredoc_struct rb_strterm_heredoc_t;

RUBY_SYMBOL_EXPORT_BEGIN
VALUE rb_parser_reg_compile(struct parser_params* parser, VALUE str, int options);
int rb_reg_fragment_setenc(struct parser_params*, VALUE, int);
enum lex_state_e rb_parser_trace_lex_state(struct parser_params *, enum lex_state_e, enum lex_state_e, int);
VALUE rb_parser_lex_state_name(enum lex_state_e state);
void rb_parser_show_bitstack(struct parser_params *, stack_type, const char *, int);
PRINTF_ARGS(void rb_parser_fatal(struct parser_params *parser, const char *fmt, ...), 2, 3);
void rb_parser_set_location_from_strterm_heredoc(struct parser_params *parser, rb_strterm_heredoc_t *here, YYLTYPE *yylloc);
void rb_parser_set_location_of_none(struct parser_params *parser, YYLTYPE *yylloc);
void rb_parser_set_location(struct parser_params *parser, YYLTYPE *yylloc);
RUBY_SYMBOL_EXPORT_END

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
static int  local_id_gen(struct parser_params*, ID, ID **);
#define local_id_ref(id, vidp) local_id_gen(parser, (id), &(vidp))
#define local_id(id) local_id_gen(parser, (id), NULL)
static ID   internal_id_gen(struct parser_params*);
#define internal_id() internal_id_gen(parser)

static const struct vtable *dyna_push_gen(struct parser_params *);
#define dyna_push() dyna_push_gen(parser)
static void dyna_pop_gen(struct parser_params*, const struct vtable *);
#define dyna_pop(node) dyna_pop_gen(parser, (node))
static int dyna_in_block_gen(struct parser_params*);
#define dyna_in_block() dyna_in_block_gen(parser)
#define dyna_var(id) local_var(id)
static int dvar_defined_gen(struct parser_params*, ID, ID**);
#define dvar_defined_ref(id, vidp) dvar_defined_gen(parser, (id), &(vidp))
#define dvar_defined(id) dvar_defined_gen(parser, (id), NULL)
static int dvar_curr_gen(struct parser_params*,ID);
#define dvar_curr(id) dvar_curr_gen(parser, (id))

static int lvar_defined_gen(struct parser_params*, ID);
#define lvar_defined(id) lvar_defined_gen(parser, (id))

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

struct rb_strterm_heredoc_struct {
    SIGNED_VALUE sourceline;
    VALUE term;		/* `"END"` of `<<"END"` */
    VALUE lastline;	/* the string of line that contains `<<"END"` */
    union {
	VALUE dummy;
	long lastidx;	/* the column of `<<"END"` */
    } u3;
};

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
	rb_gc_mark(heredoc->term);
	rb_gc_mark(heredoc->lastline);
    }
}
#endif

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

#define ID2VAL(id) STATIC_ID2SYM(id)
#define TOKEN2VAL(t) ID2VAL(TOKEN2ID(t))
#define KWD2EID(t, v) ripper_new_yylval(keyword_##t, get_value(v), 0)

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
#define mlhs_add_post(l,a) dispatch2(mlhs_add_post, (l), (a))

#define params_new(pars, opts, rest, pars2, kws, kwrest, blk) \
        dispatch7(params, (pars), (opts), (rest), (pars2), (kws), (kwrest), (blk))

#define blockvar_new(p,v) dispatch2(block_var, (p), (v))

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
#define new_args(f,o,r,p,t,location) new_args_gen(parser, (f),(o),(r),(p),(t))

static inline VALUE
new_args_tail_gen(struct parser_params *parser, VALUE k, VALUE kr, VALUE b)
{
    NODE *t = rb_node_newnode(NODE_ARGS_AUX, k, kr, b);
    add_mark_object(k);
    add_mark_object(kr);
    add_mark_object(b);
    return (VALUE)t;
}
#define new_args_tail(k,kr,b,location) new_args_tail_gen(parser, (k),(kr),(b))

#define new_defined(expr,location) dispatch1(defined, (expr))

static VALUE parser_heredoc_dedent(struct parser_params*,VALUE);
# define heredoc_dedent(str) parser_heredoc_dedent(parser, (str))

#define FIXME 0

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
static ID id_warn, id_warning, id_gets;
# define WARN_S_L(s,l) STR_NEW(s,l)
# define WARN_S(s) STR_NEW2(s)
# define WARN_I(i) INT2NUM(i)
# define WARN_ID(i) rb_id2str(i)
# define PRIsWARN "s"
# define WARN_ARGS(fmt,n) parser->value, id_warn, n, rb_usascii_str_new_lit(fmt)
# define WARN_ARGS_L(l,fmt,n) WARN_ARGS(fmt,n)
# ifdef HAVE_VA_ARGS_MACRO
# define WARN_CALL(...) rb_funcall(__VA_ARGS__)
# else
# define WARN_CALL rb_funcall
# endif
# define WARNING_ARGS(fmt,n) parser->value, id_warning, n, rb_usascii_str_new_lit(fmt)
# define WARNING_ARGS_L(l, fmt,n) WARNING_ARGS(fmt,n)
# ifdef HAVE_VA_ARGS_MACRO
# define WARNING_CALL(...) rb_funcall(__VA_ARGS__)
# else
# define WARNING_CALL rb_funcall
# endif
PRINTF_ARGS(static void ripper_compile_error(struct parser_params*, const char *fmt, ...), 2, 3);
# define compile_error ripper_compile_error
# define PARSER_ARG parser,
#else
# define WARN_S_L(s,l) s
# define WARN_S(s) s
# define WARN_I(i) i
# define WARN_ID(i) rb_id2name(i)
# define PRIsWARN PRIsVALUE
# define WARN_ARGS(fmt,n) WARN_ARGS_L(ruby_sourceline,fmt,n)
# define WARN_ARGS_L(l,fmt,n) ruby_sourcefile, (l), (fmt)
# define WARN_CALL rb_compile_warn
# define WARNING_ARGS(fmt,n) WARN_ARGS(fmt,n)
# define WARNING_ARGS_L(l,fmt,n) WARN_ARGS_L(l,fmt,n)
# define WARNING_CALL rb_compile_warning
PRINTF_ARGS(static void parser_compile_error(struct parser_params*, const char *fmt, ...), 2, 3);
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

#line 1055 "ripper.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
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
    tSTRING_CONTENT = 318,
    tCHAR = 319,
    tNTH_REF = 320,
    tBACK_REF = 321,
    tREGEXP_END = 322,
    tUPLUS = 130,
    tUMINUS = 131,
    tPOW = 132,
    tCMP = 133,
    tEQ = 138,
    tEQQ = 139,
    tNEQ = 140,
    tGEQ = 137,
    tLEQ = 136,
    tANDOP = 146,
    tOROP = 147,
    tMATCH = 141,
    tNMATCH = 142,
    tDOT2 = 128,
    tDOT3 = 129,
    tAREF = 143,
    tASET = 144,
    tLSHFT = 134,
    tRSHFT = 135,
    tANDDOT = 148,
    tCOLON2 = 145,
    tCOLON3 = 323,
    tOP_ASGN = 324,
    tASSOC = 325,
    tLPAREN = 326,
    tLPAREN_ARG = 327,
    tRPAREN = 328,
    tLBRACK = 329,
    tLBRACE = 330,
    tLBRACE_ARG = 331,
    tSTAR = 332,
    tDSTAR = 333,
    tAMPER = 334,
    tLAMBDA = 335,
    tSYMBEG = 336,
    tSTRING_BEG = 337,
    tXSTRING_BEG = 338,
    tREGEXP_BEG = 339,
    tWORDS_BEG = 340,
    tQWORDS_BEG = 341,
    tSYMBOLS_BEG = 342,
    tQSYMBOLS_BEG = 343,
    tSTRING_DBEG = 344,
    tSTRING_DEND = 345,
    tSTRING_DVAR = 346,
    tSTRING_END = 347,
    tLAMBEG = 348,
    tLABEL_END = 349,
    tLOWEST = 350,
    tUMINUS_NUM = 351,
    tLAST_TOKEN = 352
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 1001 "ripper.y"

    VALUE val;
    NODE *node;
    ID id;
    int num;
    const struct vtable *vars;
    struct rb_strterm_struct *strterm;

#line 1233 "ripper.c"

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



int yyparse (struct parser_params *parser);





#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))

/* Stored state numbers (used for stacks). */
typedef yytype_int16 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

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

#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && ! defined __ICC && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                            \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
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

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

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
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
  YYLTYPE yyls_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE) \
             + YYSIZEOF (YYLTYPE)) \
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
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
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
#define YYLAST   12080

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  146
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  220
/* YYNRULES -- Number of rules.  */
#define YYNRULES  642
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  1083

#define YYUNDEFTOK  2
#define YYMAXUTOK   352


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
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
      68,    69,    70,    71,    85,    86,    76,    75,    72,    73,
      74,    79,    80,    83,    84,    88,    77,    78,    87,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
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
      65,    66,    67,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,   101,   102,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   130,   133
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,  1164,  1164,  1164,  1195,  1205,  1214,  1222,  1230,  1236,
    1238,  1237,  1258,  1294,  1304,  1313,  1321,  1329,  1335,  1340,
    1339,  1361,  1361,  1370,  1379,  1391,  1402,  1410,  1419,  1428,
    1442,  1456,  1469,  1486,  1487,  1496,  1501,  1509,  1512,  1516,
    1520,  1541,  1545,  1549,  1560,  1564,  1571,  1579,  1591,  1594,
    1595,  1599,  1603,  1607,  1611,  1614,  1626,  1627,  1630,  1631,
    1638,  1637,  1655,  1665,  1675,  1687,  1692,  1703,  1708,  1719,
    1729,  1738,  1747,  1756,  1767,  1768,  1778,  1779,  1789,  1797,
    1805,  1813,  1822,  1830,  1839,  1847,  1856,  1864,  1875,  1876,
    1886,  1894,  1904,  1912,  1922,  1926,  1930,  1938,  1946,  1954,
    1962,  1966,  1970,  1977,  1981,  1985,  1993,  2001,  2009,  2017,
    2021,  2025,  2032,  2041,  2044,  2053,  2062,  2073,  2074,  2075,
    2076,  2081,  2088,  2089,  2092,  2100,  2103,  2111,  2111,  2122,
    2123,  2124,  2125,  2126,  2127,  2128,  2129,  2130,  2131,  2132,
    2133,  2134,  2135,  2136,  2137,  2138,  2139,  2140,  2141,  2142,
    2143,  2144,  2145,  2146,  2147,  2148,  2149,  2150,  2151,  2154,
    2154,  2154,  2155,  2155,  2156,  2156,  2156,  2157,  2157,  2157,
    2157,  2158,  2158,  2158,  2158,  2159,  2159,  2159,  2160,  2160,
    2160,  2160,  2161,  2161,  2161,  2161,  2162,  2162,  2162,  2162,
    2163,  2163,  2163,  2163,  2164,  2164,  2164,  2164,  2165,  2165,
    2168,  2172,  2176,  2204,  2209,  2214,  2219,  2230,  2235,  2240,
    2251,  2262,  2266,  2270,  2274,  2278,  2282,  2286,  2290,  2294,
    2298,  2302,  2306,  2310,  2314,  2315,  2319,  2323,  2327,  2331,
    2335,  2339,  2343,  2347,  2351,  2355,  2359,  2359,  2364,  2374,
    2380,  2381,  2382,  2383,  2386,  2390,  2397,  2409,  2410,  2414,
    2422,  2432,  2440,  2454,  2464,  2465,  2468,  2469,  2470,  2474,
    2482,  2492,  2501,  2509,  2519,  2528,  2537,  2537,  2549,  2560,
    2564,  2570,  2578,  2587,  2601,  2617,  2618,  2621,  2635,  2650,
    2661,  2662,  2663,  2664,  2665,  2666,  2667,  2668,  2669,  2670,
    2671,  2680,  2679,  2706,  2706,  2715,  2719,  2714,  2728,  2736,
    2745,  2754,  2762,  2771,  2780,  2788,  2797,  2806,  2806,  2811,
    2815,  2819,  2830,  2831,  2842,  2846,  2858,  2870,  2870,  2870,
    2882,  2882,  2882,  2894,  2906,  2917,  2919,  2916,  2970,  2969,
    2997,  2996,  3021,  3020,  3048,  3053,  3047,  3076,  3077,  3076,
    3104,  3113,  3122,  3131,  3142,  3154,  3160,  3166,  3172,  3178,
    3184,  3194,  3200,  3206,  3212,  3222,  3228,  3235,  3240,  3241,
    3248,  3253,  3256,  3257,  3270,  3271,  3281,  3282,  3285,  3292,
    3302,  3310,  3320,  3328,  3337,  3347,  3355,  3364,  3373,  3383,
    3391,  3403,  3407,  3411,  3415,  3421,  3426,  3431,  3435,  3439,
    3443,  3447,  3451,  3459,  3463,  3467,  3471,  3475,  3479,  3483,
    3487,  3491,  3497,  3498,  3504,  3514,  3523,  3535,  3539,  3549,
    3556,  3565,  3573,  3579,  3582,  3587,  3590,  3579,  3611,  3619,
    3625,  3630,  3637,  3636,  3653,  3670,  3674,  3687,  3702,  3713,
    3712,  3724,  3723,  3734,  3739,  3738,  3750,  3749,  3760,  3769,
    3778,  3793,  3792,  3808,  3807,  3824,  3825,  3824,  3834,  3835,
    3834,  3844,  3857,  3858,  3861,  3883,  3886,  3894,  3902,  3905,
    3909,  3912,  3920,  3923,  3924,  3932,  3935,  3952,  3959,  3960,
    3970,  3980,  3986,  3992,  4003,  4010,  4020,  4028,  4038,  4049,
    4056,  4074,  4084,  4095,  4102,  4114,  4121,  4137,  4144,  4155,
    4162,  4173,  4180,  4221,  4229,  4228,  4246,  4252,  4257,  4261,
    4265,  4245,  4287,  4295,  4303,  4312,  4315,  4326,  4327,  4328,
    4329,  4332,  4343,  4344,  4355,  4362,  4369,  4376,  4385,  4386,
    4387,  4388,  4389,  4392,  4393,  4394,  4395,  4396,  4397,  4398,
    4401,  4414,  4424,  4428,  4434,  4441,  4451,  4450,  4460,  4469,
    4479,  4479,  4493,  4497,  4501,  4505,  4511,  4516,  4521,  4525,
    4529,  4533,  4537,  4541,  4545,  4549,  4553,  4557,  4561,  4565,
    4569,  4573,  4578,  4584,  4594,  4604,  4614,  4626,  4627,  4634,
    4643,  4652,  4677,  4684,  4698,  4707,  4717,  4729,  4738,  4749,
    4757,  4768,  4776,  4786,  4787,  4790,  4799,  4810,  4823,  4836,
    4844,  4854,  4862,  4872,  4873,  4876,  4889,  4900,  4901,  4904,
    4921,  4925,  4935,  4945,  4945,  4975,  4976,  4986,  4993,  5017,
    5029,  5037,  5048,  5062,  5063,  5064,  5067,  5068,  5069,  5070,
    5073,  5074,  5075,  5078,  5079,  5082,  5086,  5092,  5093,  5099,
    5100,  5103,  5104,  5107,  5110,  5113,  5114,  5115,  5118,  5119,
    5122,  5123,  5127
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
  "undef_list", "$@6", "op", "reswords", "arg", "$@7", "relop", "rel_expr",
  "arg_value", "aref_args", "arg_rhs", "paren_args", "opt_paren_args",
  "opt_call_args", "call_args", "command_args", "@8", "block_arg",
  "opt_block_arg", "args", "mrhs_arg", "mrhs", "primary", "@9", "$@10",
  "$@11", "$@12", "$@13", "$@14", "$@15", "$@16", "$@17", "$@18", "$@19",
  "@20", "@21", "@22", "@23", "@24", "@25", "@26", "primary_value",
  "k_begin", "k_if", "k_unless", "k_while", "k_until", "k_case", "k_for",
  "k_class", "k_module", "k_def", "k_end", "k_return", "then", "do",
  "if_tail", "opt_else", "for_var", "f_marg", "f_marg_list", "f_margs",
  "block_args_tail", "opt_block_args_tail", "block_param",
  "opt_block_param", "block_param_def", "opt_bv_decl", "bv_decls", "bvar",
  "lambda", "@27", "@28", "@29", "@30", "f_larglist", "lambda_body",
  "do_block", "@31", "block_call", "method_call", "@32", "@33", "@34",
  "@35", "brace_block", "@36", "@37", "brace_body", "@38", "@39",
  "do_body", "@40", "@41", "case_body", "cases", "opt_rescue", "exc_list",
  "exc_var", "opt_ensure", "literal", "strings", "string", "string1",
  "xstring", "regexp", "words", "word_list", "word", "symbols",
  "symbol_list", "qwords", "qsymbols", "qword_list", "qsym_list",
  "string_contents", "xstring_contents", "regexp_contents",
  "string_content", "@42", "@43", "@44", "@45", "@46", "@47",
  "string_dvar", "symbol", "sym", "dsym", "numeric", "simple_numeric",
  "user_variable", "keyword_variable", "var_ref", "var_lhs", "backref",
  "superclass", "$@48", "f_arglist", "@49", "args_tail", "opt_args_tail",
  "f_args", "f_bad_arg", "f_norm_arg", "f_arg_asgn", "f_arg_item", "f_arg",
  "f_label", "f_kw", "f_block_kw", "f_block_kwarg", "f_kwarg",
  "kwrest_mark", "f_kwrest", "f_opt", "f_block_opt", "f_block_optarg",
  "f_optarg", "restarg_mark", "f_rest_arg", "blkarg_mark", "f_block_arg",
  "opt_f_block_arg", "singleton", "$@50", "assoc_list", "assocs", "assoc",
  "operation", "operation2", "operation3", "dot_or_colon", "call_op",
  "call_op2", "opt_terms", "opt_nl", "rparen", "rbracket", "trailer",
  "term", "terms", "none", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_int16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   130,   131,
     132,   133,   138,   139,   140,   137,   136,   146,   147,   141,
     142,   128,   129,   143,   144,   134,   135,   148,   145,   323,
     324,   325,   326,   327,   328,   329,   330,   331,   332,   333,
     334,   335,   336,   337,   338,   339,   340,   341,   342,   343,
     344,   345,   346,   347,   348,   349,   350,    61,    63,    58,
      62,    60,   124,    94,    38,    43,    45,    42,    47,    37,
     351,    33,   126,   352,   123,   125,    91,    44,    96,    40,
      41,    93,    59,    32,    46,    10
};
# endif

#define YYPACT_NINF (-884)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-643)

#define yytable_value_is_error(Yyn) \
  ((Yyn) == YYTABLE_NINF)

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -884,   130,  3303,  -884,  8357,  -884,  -884,  -884,  7851,  -884,
    -884,  -884,  -884,  -884,  -884,  -884,  8474,  8474,  -884,  -884,
    -884,  4690,  4276,  -884,  -884,  -884,  -884,   389,  7715,   109,
    -884,    11,  -884,  -884,  -884,  3586,  4414,  -884,  -884,  3724,
    -884,  -884,  -884,  -884,  -884,  -884,  -884,  -884,  9878,  9878,
     102,  6050,   169,  8825,  9176,  8117,  -884,  7579,  -884,  -884,
    -884,    69,   199,   259,   280,   630,  9995,  9878,  -884,   264,
    -884,  1108,  -884,   136,  -884,  -884,    67,   397,   325,  -884,
     310, 10229,  -884,   337,  2565,   638,   534,   581,  -884, 10112,
   10112,  -884,  -884,  6951, 10342, 10455, 10568,  7442,  8474,    34,
     157,  -884,  -884,   373,  -884,  -884,  -884,  -884,  -884,  -884,
    -884,  -884,  -884,  -884,  -884,    50,   268,  -884,   396,   473,
    -884,  -884,  -884,  -884,  -884,  -884,  -884,  -884,  -884,  -884,
    -884,  -884,  -884,  -884,  -884,  -884,  -884,  -884,  -884,  -884,
    -884,  -884,  -884,  -884,  -884,  -884,  -884,  -884,  -884,  -884,
    -884,  -884,  -884,  -884,  -884,  -884,  -884,  -884,  -884,  -884,
    -884,  -884,  -884,  -884,  -884,  -884,  -884,  -884,  -884,  -884,
    -884,  -884,  -884,  -884,  -884,  -884,  -884,  -884,  -884,  -884,
    -884,  -884,  -884,  -884,  -884,  -884,  -884,  -884,  -884,  -884,
    -884,  -884,  -884,  -884,  -884,  -884,  -884,  -884,  -884,  -884,
     350,  -884,  -884,  -884,  -884,   365,  9878,   450,  6182,  9878,
    9878,  9878,  -884,  9878,  -884,   432,  2565,   475,  -884,  -884,
     454,   691,    42,    43,   487,    44,   476,  -884,  -884,  6834,
    -884,  8474,  8474,  -884,  -884,  7068,  -884, 10112,   643,  -884,
     477,   489,  6314,  -884,  -884,   497,   516,    67,  -884,   723,
    -884,   556,   542,  8591,  -884,  6050,   523,   264,  -884,  1108,
     109,   555,  -884,   109,  8591,   536,   239,   381,  -884,   475,
     564,   381,  -884,   109,   648,   630, 10681,   575,  -884,   786,
     850,   872,   881,  -884,  -884,  -884,  -884,  -884,  -884,   472,
    -884,   529,   740,   590,  -884,  -884,  -884,  -884,   646,  -884,
    -884,  -884,  -884,  -884,  -884,  -884,  7185, 10112, 10112, 10112,
   10112,  8591, 10112, 10112,  -884,  -884,  -884,   625,  -884,  -884,
    -884,  9293,  -884,  6050,  8237,   591,  9293,  9878,  9878,  9878,
    9878,  9878,  -884,  -884,  9878,  9878,  9878,  9878,  9878,  9878,
    9878,  9878,  9878,  -884,  -884,  9878,  9878,  9878,  9878,  9878,
    9878,  9878,  9878,  9878,  9878,  -884,  2995,  8474,  -884,  3090,
    5387,   136,    83,    83, 10112, 10112,   264,   715,   598,   686,
    -884,  -884,   897,   718,   123,   124,   132,   685,   806, 10112,
     321,  -884,   624,   916,  -884,  -884,  -884,  -884,    41,    51,
     251,   253,   255,   349,   379,   400,   404,  -884,  -884,  -884,
     421,  -884,  -884,  -884, 10970,  -884,  -884,  9995,  9995,  -884,
    -884,    46,  -884,  -884,  -884,   713,  9878,  9878,  8708,  -884,
    -884, 11051,  8474, 11132,  9878,  9878,  8942,  -884,   109,   607,
    -884,  -884,   109,  -884,   609,   612,  -884,   323,  -884,  -884,
    -884,  -884,  -884,  7851,  -884,  9878,  6457,   616, 11051, 11132,
    9878,  1108,   632,   109,  -884,  -884,  7302,   631,  -884,  1108,
    -884,  9059,  -884,  -884,  9176,  -884,  -884,  -884,   477,   932,
    -884,  -884,   654, 10681, 11213,  8474, 11294,  1262,  -884,  -884,
    -884,  -884,  -884,  -884,  -884,  -884,  -884,  -884,   813,    52,
     836,    91,  9878,  -884,  -884,  -884,  -884,  -884,  -884,  -884,
    -884,  -884,  -884,  -884,  -884,  -884,  -884,  9878,  -884,   659,
     664,  -884,  -884,   109, 10681,   665,  -884,  -884,  -884,   741,
     689,  1952,  -884,  -884,  -884,   942,   235,   556,  3564,  3564,
    3564,  3564,  3702,  3170,  3564,  3564,  2755,  2755,   413,   413,
    2443,   610,   610,  1038,   558,   558,   556,   556,   556,  1367,
    1367,  4828,  3862,  5104,  4000,   516,   675,  -884,   109,   637,
    -884,   672,  -884,   516,  4552,   805,   812,  -884,  5530,   809,
    5816,    55,    55,   715,  9410,   805,   164, 11375,  8474, 11456,
    -884,   136,  -884,   932,  -884,  -884,  -884, 11537,  8474, 10970,
    5387, 10112,  -884,  -884,  -884,  -884,  -884,  -884,  2415,  -884,
    1977,  -884,  -884,  -884,  7851,  9878,  -884,  9878,   475,  -884,
     476,  3447,  4138,   109,   394,   399,  -884,  -884,  -884,  -884,
    8708,  8942,  -884,  -884, 10112,  2565,   698,  -884,  -884,  -884,
    -884,  6457,    39,  -884,   109,   381, 10681,   654,   466,   700,
     109,   433,   457,  -884,  -884,  -884,  -884,  -884,  -884,  -884,
    1180,  -884,  -884,  -884,  -884,  -884,  1324,  -884,  -884,  -884,
    -884,  -884,   717,  -884,   701,  9878,  -884,   709,   804,   720,
    -884,   729,   816,   735,   825,  -884,  -884,  1076,  -884,    56,
    -884,   737,  -884,  -884,    78,   742,  -884,   556,   874,  -884,
     751,  -884,  -884,   882,   754,  9527,  -884,   654, 10681,  8591,
    9995,  9878, 11618,  8474, 11699,   777,  9995,  9995,  -884,   625,
     516,   759,   726,  9995,  9995,  -884,   625,   516,  -884,  -884,
    9644,   889,  -884,   342,  -884,   889,  -884,  -884,  -884,  -884,
     805,    66,  -884,    95,   145,   109,   167,   189, 10112,   264,
   10112,  5387,   844,   700,  -884,   109,   805,   323,   768,  7987,
    -884,   157,   397,  -884,  -884,  -884,  -884,  -884,  9878,  9878,
     430,  9878,  9878,   774,   323,  -884,   778,  -884,  -884,  -884,
     521,  1180,   443,  -884,   781,   109,  -884,   109,  -884,  9878,
    1324,  -884,  -884,   606,  -884,  -884,  -884,   451,  -884,  1324,
    -884,  -884,  1363,  -884,  -884,  -884,  -884,  -884,  -884,  -884,
    -884,  -884,  -884,  -884,  -884,  -884,  -884,    49,  -884,    49,
     779,  -884,  9878,   783,   654,  -884,  2565,  4966,  5242,   109,
     435,   464,  9878,  -884,  -884,  -884,  -884,  -884,  -884,  -884,
    9995,  -884,  -884,  -884,  -884,  -884,  -884,   659,  -884,   830,
    -884,  5933,   925,  -884, 10112,   805,  -884,   805,  6600,  6600,
    -884,  9761,  5673,   225,    55,  5387,   264,   805,  -884,  -884,
    -884,  1324,  5387,  1324,  -884,  -884,  -884,  9878,  8942,  -884,
    -884,   109,   990,   798,  1203,  -884,   796,   801,    68,  -884,
    -884,  -884,   807,   810,  -884,   720,  -884,   814,  -884,   820,
    -884,   814,  -884,  -884,  1121,  5387,  -884,  -884,  6314,  -884,
     821,   478,  2565,  -884,  -884, 10794,    83,  -884,  -884,  6600,
    -884,  -884,    83,  -884,  -884,   805,   805,  -884,   320,  -884,
     805,  -884,  -884,   109,   805,   264,   768,  -884,   824,   990,
     503,  -884,  -884,  1103,  6600,  6314,  -884,  1324,  -884,  1363,
    -884,  -884,  1363,  -884,  1363,  -884,  -884,  -884,  -884,   109,
     845,   854,   835, 10907,  -884,   842,   720,  -884,   853,   860,
    -884,  -884,  -884,   926,  -884,   996,    94,   129,   171,  5387,
    -884,  5530,  -884,  -884,  -884,  -884,  -884,  6600,  -884,  -884,
    -884,  -884,  5387,   990,   824,   990,   861,  -884,   509,  -884,
    -884,   805,   846,   814,   863,   814,   814,  6717,   880,  -884,
   10907,  1324,  -884,   950,  1000,   606,  -884,  -884,  1324,  -884,
    1363,  -884,   246, 11780,  8474, 11861,   812,   342,   805,   805,
     824,   990,  1103,  -884,  -884,  -884,  -884,  1363,  -884,  -884,
    -884,   898,  -884,  1000,  -884,   876,   884,  -884, 11942,  -884,
     720,   886,  -884,   887,   886,    71,   166,   109,   270,   274,
    -884,  -884,  -884,  -884,   824,  -884,   814,  -884,  1324,  -884,
    1363,  -884,   879,   892,  -884,  1363,  -884,  1363,  -884,  -884,
     336,  -884,   886,   895,   886,   886,  -884,  1363,  -884,  -884,
    -884,   886,  -884
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,     0,     0,     1,     0,   352,   353,   354,     0,   345,
     346,   347,   350,   348,   349,   351,   340,   341,   342,   343,
     356,   266,   266,   524,   523,   525,   526,   631,     0,   631,
      10,     0,   528,   527,   529,   613,   615,   520,   519,   614,
     522,   514,   515,   516,   517,   467,   534,   535,     0,     0,
       0,     0,   295,   642,   642,    86,   413,   487,   487,   489,
     491,     0,     0,     0,     0,     0,     0,     0,     3,   629,
       6,     9,    33,    37,    49,    57,   266,    56,     0,    74,
       0,    78,    88,     0,    54,   224,   239,     0,   291,     0,
       0,   317,   320,   629,     0,     0,     0,     0,   303,    58,
     312,   280,   281,   466,   468,   282,   283,   284,   286,   285,
     287,   464,   465,   463,   512,   530,   531,   288,     0,   289,
      62,     5,     8,   169,   180,   170,   193,   166,   186,   176,
     175,   196,   197,   191,   174,   173,   168,   194,   198,   199,
     178,   167,   181,   185,   187,   179,   172,   188,   195,   190,
     189,   182,   192,   177,   165,   184,   183,   164,   171,   162,
     163,   159,   160,   161,   117,   119,   118,   154,   155,   150,
     132,   133,   134,   141,   138,   140,   135,   136,   156,   157,
     142,   143,   147,   151,   137,   139,   129,   130,   131,   144,
     145,   146,   148,   149,   152,   153,   158,   122,   124,   126,
      26,   120,   121,   123,   125,     0,     0,     0,     0,     0,
       0,     0,   487,     0,   261,     0,   246,   271,    72,   265,
     642,     0,   530,   531,     0,   289,   642,   607,    73,   631,
      70,     0,   642,   438,    69,   631,   632,     0,     0,    21,
     236,     0,     0,   340,   341,   306,   439,     0,   218,     0,
     303,   219,   300,     0,    19,     0,     0,   629,    15,    18,
     631,    76,    14,   631,     0,     0,   635,   635,   247,     0,
       0,   635,   605,   631,     0,     0,     0,    84,   344,     0,
      94,    95,   102,   314,   414,   509,   508,   510,   507,     0,
     506,     0,     0,     0,   474,   483,   479,   485,   513,    53,
     230,   231,   638,   639,     4,   640,   630,     0,     0,     0,
       0,     0,     0,     0,   443,   441,   428,    63,   311,   422,
     424,     0,    90,     0,    82,    79,     0,     0,     0,     0,
       0,     0,   242,   243,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   240,   241,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   626,   436,   642,   625,   434,
       0,    55,     0,     0,     0,     0,   629,     0,   630,     0,
     367,   366,     0,     0,   530,   531,   289,   112,   113,     0,
       0,   115,   538,     0,   530,   531,   289,   332,   189,   182,
     192,   177,   159,   160,   161,   117,   118,   603,   334,   602,
       0,    71,   628,   627,     0,   313,   469,     0,     0,   127,
     610,   300,   272,   612,   268,     0,     0,     0,     0,   262,
     270,   436,   642,   434,     0,     0,     0,   263,   631,     0,
     305,   267,   631,   257,   642,   642,   256,   631,   310,    52,
      23,    25,    24,     0,   307,     0,     0,     0,   436,   434,
       0,    17,     0,   631,   298,    13,   630,    75,   294,   296,
     301,   637,   636,   248,   637,   250,   302,   606,     0,   101,
     513,    92,    87,     0,   436,   642,   434,   562,   493,   496,
     494,   511,   488,   470,   471,   490,   472,   492,     0,     0,
       0,     0,     0,   641,     7,    27,    28,    29,    30,    31,
      50,    51,   448,   445,    60,    64,   448,     0,    34,   276,
       0,    36,   275,   631,     0,    80,    91,    48,    38,    46,
       0,   251,   271,   200,    35,     0,   289,   216,   223,   225,
     226,   227,   234,   235,   228,   229,   209,   210,   232,   233,
     631,   220,   221,   222,   211,   212,   213,   214,   215,   244,
     245,   616,   618,   617,   619,     0,   266,   433,   631,   616,
     618,   617,   619,     0,   266,     0,   642,   358,     0,   357,
       0,     0,     0,     0,     0,     0,   300,   436,   642,   434,
     325,   330,   112,   113,   114,   536,   328,   436,   642,   434,
       0,     0,   335,   624,   623,   337,   616,   617,   266,    39,
     251,   201,    45,   208,     0,     0,   609,     0,   273,   269,
     642,   616,   617,   631,   616,   617,   608,   304,   633,   253,
     258,   260,   309,    22,     0,   237,     0,    32,   431,   429,
     207,     0,    77,    16,   631,   635,     0,    85,   620,   100,
     631,   616,   617,   568,   565,   564,   563,   566,   574,   583,
       0,   594,   584,   598,   597,   593,   562,   415,   561,   419,
     567,   569,   570,   572,   547,   576,   581,   642,   586,   642,
     591,   547,   596,   547,     0,   545,   497,     0,   473,     0,
     476,     0,   481,   478,     0,     0,   482,   217,     0,   449,
       0,   446,   445,     0,   279,     0,    89,    83,     0,     0,
       0,     0,   436,   642,   434,     0,     0,     0,   437,    67,
       0,     0,   440,     0,     0,   435,    65,   642,   355,   292,
     642,   642,   455,   642,   359,   642,   361,   318,   360,   321,
       0,     0,   324,   620,   299,   631,   616,   617,     0,     0,
       0,     0,   112,   113,   116,   631,     0,   631,   540,     0,
     255,   425,    59,   254,   128,   611,   274,   264,     0,     0,
     440,     0,     0,   642,   631,    11,     0,   297,   249,    93,
     440,     0,   379,   370,   372,   631,   368,   631,   416,     0,
       0,   554,   575,     0,   543,   601,   585,     0,   544,     0,
     557,   595,     0,   559,   599,   498,   502,   503,   504,   495,
     505,   475,   477,   484,   480,   486,   444,   642,   442,   642,
       0,   423,     0,   277,    81,    47,   252,   616,   617,   631,
     616,   617,     0,    44,   205,    43,   206,    68,   432,   634,
       0,    41,   203,    42,   204,    66,   430,   456,   457,   642,
     458,     0,   642,   364,     0,     0,   362,     0,     0,     0,
     323,     0,     0,   440,     0,     0,     0,     0,   440,   333,
     604,   562,     0,   562,   338,   426,   427,     0,   259,   308,
      20,   631,     0,   377,     0,   571,     0,   407,     0,   587,
     546,   573,   547,   547,   582,   642,   600,   547,   592,   547,
     570,   547,   499,   405,   631,     0,   403,   402,     0,    61,
     278,   440,   238,    40,   202,     0,     0,   460,   365,     0,
      12,   462,     0,   315,   316,     0,     0,   273,   642,   326,
       0,   537,   329,   631,     0,     0,   540,   369,   380,     0,
     375,   371,   418,     0,     0,     0,   417,     0,   550,     0,
     552,   542,     0,   558,     0,   555,   560,   500,   401,   631,
       0,   570,   386,   578,   579,   642,   642,   589,   386,   386,
     384,   450,   447,     0,   459,     0,   530,   531,   289,     0,
     461,     0,   319,   322,   452,   453,   451,     0,   331,   539,
     336,   541,     0,     0,   378,     0,   373,   411,   631,   409,
     412,     0,     0,   547,   547,   547,   547,     0,     0,   404,
       0,   392,   394,     0,   577,     0,   382,   383,     0,   397,
       0,   399,   300,   436,   642,   434,   642,   642,     0,     0,
     376,     0,     0,   408,   421,   420,   551,     0,   548,   553,
     556,     0,   406,   588,   385,   386,   386,   300,   436,   580,
     642,   386,   590,   386,   386,   620,   299,   631,   616,   617,
     454,   363,   327,   339,   374,   410,   547,   501,     0,   389,
       0,   391,   620,   299,   381,     0,   398,     0,   395,   400,
     440,   549,   386,   386,   386,   386,   390,     0,   387,   393,
     396,   386,   388
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -884,  -884,  -884,  -416,  -884,    32,  -884,  -410,    30,  -884,
     587,  -884,    76,  -884,  -306,  -303,    10,   -67,   -63,  -884,
    -581,  -884,  2280,    -9,   917,  -161,    24,   -80,  -884,  -454,
     -18,  1410,  -297,   941,   -51,  -884,   -15,  -884,  -884,    20,
    -884,   764,  -884,   963,  -884,  1193,  -884,   440,   -61,   332,
    -332,    26,   -11,  -884,  -394,  -159,    33,  -884,  -293,   -24,
    -884,  -884,  -884,  -884,  -884,  -884,  -884,  -884,  -884,  -884,
    -884,  -884,  -884,  -884,  -884,  -884,  -884,  -884,    57,  -884,
    -884,  -884,  -884,  -884,  -884,  -884,  -884,  -884,  -884,  -116,
    2680,  -345,  -558,    36,  -615,  -884,  -789,  -816,   279,   161,
     249,  -884,   242,  -884,  -768,  -884,    40,  -884,  -884,  -884,
    -884,  -884,  -884,  -884,   306,  -884,  -884,  -884,  -884,  -884,
    -884,  -884,   -91,  -884,  -884,   369,  -884,  -884,   559,  -884,
    -884,  -546,  -884,    48,  -884,  -884,  -884,  -884,  -884,  -884,
     966,  -884,  -884,  -884,  -884,   583,  -884,  -884,  -884,  -884,
    -884,  -884,    37,  -884,  -884,  -253,  -884,  -884,  -884,  -884,
    -884,  -884,  -884,    13,  -884,    27,  -884,    -7,  1278,  1501,
     977,  1962,   985,  -884,  -884,   149,  -884,  -386,  -555,  -588,
    -845,   -88,  -465,  -735,   267,  -548,   294,    84,  -884,  -884,
    -884,  -150,  -720,  -883,    96,   316,  -884,  -275,  -884,    62,
    -605,  -884,  -884,  -884,     0,  -388,  -884,  -312,  -884,  -884,
     -77,  -884,   -59,   -25,    18,  -526,  -205,   -64,    -6,    -2
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,    68,    69,    70,   241,   565,   566,   257,
     258,   452,   259,   443,    72,   518,    73,   362,    74,    75,
     505,   692,    76,    77,    78,   260,    79,    80,    81,   472,
      82,   215,   381,   382,   197,   198,   199,   200,   604,   554,
     202,    84,   445,   353,    85,   217,   265,   523,   233,   751,
     432,   433,   230,   231,   219,   419,   434,   511,   512,    86,
     360,   263,   264,   634,   624,   364,   848,   365,   849,   738,
     977,   741,   739,   590,   592,   748,   749,   926,   249,    88,
      89,    90,    91,    92,    93,    94,    95,    96,    97,   719,
      98,   568,   727,   845,   846,   373,   773,   774,   775,  1034,
    1002,   949,   895,   896,   876,   988,   989,   283,   284,   477,
     778,   878,   657,   936,   320,   506,    99,   100,   717,   710,
     563,   555,   318,   503,   502,   690,   691,   809,   688,   689,
     807,   575,   976,   721,   839,   906,   910,   101,   102,   103,
     104,   105,   106,   107,   488,   679,   108,   490,   109,   110,
     489,   491,   289,   292,   293,   482,   677,   676,   795,   892,
     947,   997,   799,   111,   290,   112,   113,   114,   222,   223,
     117,   224,   225,   586,   740,   862,   863,   880,   781,   659,
     660,   661,   890,   663,   664,   665,   666,   954,   955,   667,
     668,   669,   670,   957,   958,   671,   672,   673,   674,   675,
     784,   400,   591,   270,   435,   227,   120,   628,   557,   595,
     589,   404,   304,   429,   430,   712,   463,   569,   368,   262
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     121,   325,   237,   299,   240,   305,   288,   214,   214,   405,
     359,   234,   662,   239,   729,   316,   226,   226,   570,   637,
     517,   203,   403,   363,   609,   558,   366,   730,   201,   305,
     626,   278,   609,   524,   367,   204,   122,   277,   616,   485,
     487,   203,   218,   228,   556,   881,   398,   564,   201,   220,
     220,   268,   272,   267,   271,   204,   928,   278,   298,    87,
     697,    87,   465,   306,   788,   317,   467,   427,   777,   888,
     278,   278,   278,   221,   221,   261,   616,   201,    71,   567,
      71,   256,   726,   584,  -107,   931,   266,   760,   990,   214,
     613,   658,   598,   314,   453,   291,   567,   934,   226,   361,
     361,   517,   517,   361,   599,   602,   842,  -103,    87,   556,
     847,   564,   279,   984,   770,   681,   790,   201,   793,   478,
    -107,   355,   402,   221,   401,  1042,   950,   893,   827,  -524,
       3,   220,  -532,  -533,   425,   835,   450,   629,   279,  -523,
    -532,   478,  -104,   640,   423,   242,   221,   221,  -103,  -104,
     221,   372,   383,   383,   685,   221,   -75,  -111,   252,  -103,
    -104,  -111,   513,  -110,   629,   682,   479,  -103,   480,  1020,
    -109,   894,   449,   312,   313,  1042,   -89,   990,   358,  -109,
     746,   998,   935,   314,  -111,  -524,   316,   -94,   479,  -110,
     480,   662,  -106,   305,   931,  -523,   471,   302,   455,   801,
     303,   315,   476,   851,   686,  1054,   232,   881,   302,   853,
    -616,   303,   294,  -107,  -108,   766,  -107,   888,   420,   858,
     214,   804,   214,   214,   420,   302,   609,   609,   303,   226,
     436,   226,   -98,   616,  -616,   680,  -103,   680,   256,  -103,
     495,   496,   497,   498,   814,   437,   735,   439,   468,   415,
    -105,   456,   278,   438,   236,   428,   745,   431,   508,  -110,
     -94,   -95,   220,   519,   220,    87,   881,   629,   470,  -102,
     658,  -104,   447,   923,  -104,   925,   427,   629,   457,   261,
     941,   458,  -100,  -106,  -617,   256,   221,  -108,   221,   221,
     744,   315,   221,   901,   221,   579,   919,   571,   572,    87,
     278,  -101,   305,   974,   -97,  -617,   515,   573,  -109,  -293,
      87,  -109,    87,  -111,  -293,   662,  -111,   361,   361,   361,
     361,    87,   500,   501,   662,   408,   -99,   938,   940,   451,
     881,   857,   943,   279,   945,   841,   946,   574,   494,  -525,
     459,  -526,   295,  -528,   519,   519,   953,   261,   214,  -105,
    1006,  1007,  -111,   256,   510,   436,   844,   841,  -533,   510,
     312,   313,   -96,    87,   221,   221,   221,   221,    87,   221,
     221,   819,   975,   582,   361,   361,   461,   583,   221,   562,
      87,   279,    71,   525,   462,  -104,   852,   499,  -110,   581,
     556,  -110,   564,   471,   517,  -525,   662,  -526,   662,  -528,
     517,   517,   296,   823,   825,   -95,   302,   517,   517,   303,
     831,   833,  -106,   214,   221,  -106,  -108,    87,   610,  -108,
     436,   221,   221,   297,   562,   319,   802,   838,   623,   951,
     768,   802,   420,   420,   471,  1064,   221,  -527,  1026,  1028,
    1029,  1030,   321,   562,   121,   920,   617,   322,   704,   278,
     619,   757,   924,   953,   326,   622,   203,   953,   -97,   732,
     953,   635,   953,   201,   525,   525,   214,  -529,   236,   562,
     204,   632,   662,   436,   609,   658,    58,   658,  -105,   221,
     616,  -105,   -99,   327,   761,   961,   407,   409,  -518,   762,
     278,   -98,  -521,  -527,   708,   643,   562,   644,   645,   646,
     647,  1071,   715,    87,   235,   883,   411,   728,   728,   593,
     953,  -106,   953,    87,   889,   705,  -108,   953,   464,   953,
     867,  1070,    71,  -529,   517,   713,   462,   903,   235,   953,
     279,   696,   221,   711,   236,   478,   951,   750,   348,   349,
     350,   351,   352,   951,  -518,   709,   -96,  -105,  -521,   416,
     -97,   653,  -106,   716,   714,   643,   769,   644,   645,   646,
     647,   969,   776,   408,   722,   594,   417,   971,   830,   214,
     -97,   279,  1019,   -97,   -99,   654,   436,   424,   -97,   214,
     872,  -108,   479,   -98,   480,   481,   436,   752,   711,   754,
    -111,   418,   478,   951,   -99,  -105,   753,   -99,   723,   562,
     725,   747,   -99,   -98,   757,  -616,   -98,   731,   420,   562,
    -102,   -98,   278,   426,   850,   711,   444,   203,   471,   959,
     763,  -344,  -344,   446,   201,    87,   327,    87,   327,   121,
     859,   204,   450,   885,   764,   221,   229,   519,   -96,   479,
     985,   480,   483,   519,   519,   221,  1022,    87,   221,   828,
     519,   519,   767,   478,   236,   232,   750,   486,   -96,  -110,
     865,   -96,   994,   454,   648,   785,   -96,   785,   355,   356,
    -344,   854,   -74,   856,   278,   855,   649,   460,  -344,  -101,
     327,   221,  1047,   776,   873,   350,   351,   352,    87,    41,
      42,    43,    44,   279,   214,   340,   341,   440,   864,   466,
     479,   436,   480,   629,   469,   652,   653,    71,   441,   442,
     711,  -613,   473,   332,   333,   753,   492,   357,   840,   843,
     711,   843,   504,   843,   562,   358,  1036,   713,   516,   913,
     654,   914,   574,  1043,   347,   348,   349,   350,   351,   352,
     493,   922,   576,   580,   956,   585,   620,   618,   361,   621,
     361,   627,   877,   510,  -106,   279,    87,   525,   343,   344,
     221,   420,   714,   525,   525,   860,   631,   519,   -89,   201,
     525,   525,  -518,  -518,   -97,   815,   478,   912,   355,   421,
     216,   216,   869,  1073,   776,   699,   776,  -299,  -299,  -108,
     728,   636,   921,   875,   711,   221,  -271,   221,    87,   972,
     973,   695,   698,   478,   978,   897,   700,   897,   980,   -99,
     355,   448,   248,   251,  -431,   718,   830,   216,   216,  -613,
     720,  -518,   724,   479,  -613,   480,   483,   422,   605,  -518,
     300,   301,  -614,   765,   779,   358,  -299,   907,   780,  -617,
     911,   776,   986,  -105,  -299,   886,   783,   601,   603,   886,
     479,   956,   480,   484,   361,  1040,   786,   787,   956,   422,
     956,   981,   216,   -96,   601,   603,   789,   358,   791,   877,
    -620,   908,   792,   355,   474,  1024,   478,   794,   915,   916,
     803,   278,   918,   785,   806,   805,   808,   525,  1015,   927,
     630,  -272,   811,  -521,  -521,   776,   822,   776,    87,   478,
     829,   221,  1052,  1053,   841,    87,    87,   861,   956,    87,
     956,   868,    87,   870,   899,   956,   843,   956,   874,    87,
    -273,   905,   475,   479,   877,   480,   678,   956,   962,   278,
     358,  -620,  -620,   776,   909,   929,   932,  -530,  -530,   970,
    -614,   979,  -521,   933,   937,  -614,   479,   939,   480,   683,
    -521,   942,    87,   785,   785,    87,   960,   944,  -274,  -531,
    -531,   983,   965,  1023,   991,   992,    87,   999,  -289,  -289,
     216,  1000,  1001,   216,   216,   216,   278,   300,  -620,  1005,
    -620,  1025,  1012,  -616,   355,   577,  -530,   119,  -620,   119,
    1008,    87,    87,   216,  -530,   216,   216,  1010,  1021,  1016,
    1027,  1017,  1032,   355,   587,   214,  1037,  1018,  -531,  1057,
    1004,   370,   436,  1058,   722,   843,  -531,  -289,  -616,  -300,
    -300,  1060,   711,  1065,  1067,  -289,    87,  1031,    87,   355,
     702,  -617,  1077,   578,    87,   562,   119,   387,   785,    87,
     282,   358,   643,   633,   644,   645,   646,   647,   354,   836,
     871,   898,   588,  1051,    87,   948,   887,  1033,   866,   891,
     358,   810,  1055,   960,  1050,   693,   282,   886,  -300,   406,
     960,   221,   960,   684,   399,   982,  -300,   884,   703,   376,
     386,   386,   771,   355,  1013,   216,   358,   355,  1038,  1039,
     521,   527,   528,   529,   530,   531,   882,  1035,   532,   533,
     534,   535,   536,   537,   538,   539,   540,     0,   327,   541,
     542,   543,   544,   545,   546,   547,   548,   549,   550,     0,
     960,   216,   960,   340,   341,     0,     0,   960,     0,   960,
     796,   797,  1014,   798,     0,     0,   588,     0,     0,   960,
     358,    46,    47,     0,   358,     0,   824,   826,   307,   308,
     309,   310,   311,   832,   834,   987,     0,   644,   645,   646,
     647,   952,     0,   348,   349,   350,   351,   352,     0,     0,
       0,   600,   600,   643,     0,   644,   645,   646,   647,   648,
     600,   216,   216,     0,     0,     0,   216,     0,   600,   600,
     216,   649,     0,   119,     0,     0,     0,     0,   824,   826,
       0,   832,   834,     0,   993,     0,   995,  1009,  1011,   625,
       0,   996,     0,   650,   600,     0,     0,     0,     0,   651,
     652,   653,     0,     0,     0,   216,     0,   119,   216,     0,
       0,     0,   643,     0,   644,   645,   646,   647,   119,   216,
     119,     0,     0,     0,     0,   654,     0,   269,   655,   119,
       0,     0,     0,     0,     0,   643,   687,   644,   645,   646,
     647,   282,     0,     0,     0,     0,   236,     0,     0,     0,
     904,   216,   771,     0,     0,  1041,     0,  1044,   772,     0,
     115,     0,   115,     0,  1059,  1061,     0,     0,     0,     0,
    1066,   119,  1068,  1069,  1056,   771,   119,     0,     0,     0,
       0,   930,     0,     0,     0,     0,     0,   904,   119,   282,
       0,   526,     0,     0,   643,     0,   644,   645,   646,   647,
     648,  1076,  1078,  1079,  1080,  1072,     0,  1074,     0,   115,
    1082,     0,   649,   280,  1075,     0,     0,     0,   216,     0,
       0,     0,   216,     0,  1081,   119,     0,     0,     0,     0,
       0,     0,   216,     0,   650,     0,     0,     0,     0,   280,
     651,   652,   653,     0,     0,     0,     0,     0,     0,   216,
       0,   216,   374,   384,   384,   384,   643,     0,   644,   645,
     646,   647,   648,     0,   216,   216,   654,     0,     0,   655,
       0,     0,   526,   526,   649,     0,     0,     0,     0,   410,
       0,   656,   412,   413,   414,     0,     0,     0,     0,     0,
       0,     0,    83,     0,    83,   643,   650,   644,   645,   646,
     647,   648,   651,   652,   653,     0,     0,     0,     0,   216,
       0,   119,     0,   649,     0,     0,     0,   327,     0,     0,
       0,   119,     0,     0,     0,     0,     0,     0,   654,     0,
       0,   655,   340,   341,     0,   650,     0,     0,   282,   216,
       0,    83,   652,   653,   600,   816,     0,   216,     0,     0,
     600,   600,     0,     0,     0,     0,     0,   600,   600,     0,
       0,     0,     0,     0,   216,     0,   115,   654,     0,   345,
     346,   347,   348,   349,   350,   351,   352,     0,     0,   282,
       0,     0,     0,   116,   371,   116,     0,     0,     0,     0,
       0,     0,     0,     0,   509,     0,     0,     0,     0,   522,
     115,     0,   600,   600,     0,   600,   600,     0,     0,     0,
       0,   115,     0,   115,     0,     0,     0,     0,     0,     0,
       0,     0,   115,   216,     0,     0,     0,     0,     0,     0,
       0,     0,   116,   119,   280,   119,   281,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   119,   216,     0,     0,     0,
       0,     0,   281,     0,   115,     0,   902,     0,     0,   115,
       0,     0,     0,     0,   600,   375,   385,   385,   385,     0,
       0,   115,   280,     0,     0,     0,     0,     0,     0,     0,
     606,   608,     0,     0,     0,   216,   119,     0,    83,   269,
       0,   282,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   600,   216,     0,     0,     0,     0,     0,   115,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    83,     0,   608,     0,     0,   269,     0,     0,
       0,     0,   800,    83,     0,    83,     0,     0,     0,     0,
       0,     0,     0,     0,    83,     0,     0,     0,     0,     0,
       0,     0,     0,   282,   119,   526,     0,     0,     0,     0,
       0,   526,   526,     0,     0,     0,     0,     0,   526,   526,
     694,     0,     0,     0,     0,     0,     0,     0,     0,   116,
       0,     0,     0,     0,     0,     0,    83,     0,     0,     0,
       0,    83,     0,     0,   115,     0,   119,     0,     0,     0,
       0,     0,     0,    83,   115,     0,   520,     0,     0,     0,
       0,     0,     0,   116,     0,     0,     0,     0,     0,     0,
       0,   280,     0,     0,   116,     0,   116,     0,     0,     0,
       0,     0,     0,     0,     0,   116,     0,   522,     0,     0,
      83,     0,     0,     0,     0,     0,     0,   281,   216,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   280,     0,     0,     0,     0,     0,   755,     0,
     756,     0,     0,     0,     0,     0,     0,   116,     0,     0,
       0,     0,   116,   608,   269,   526,     0,   520,   520,     0,
       0,     0,     0,     0,   116,   281,   119,     0,     0,     0,
       0,     0,     0,   119,   119,     0,     0,   119,     0,     0,
     119,     0,     0,     0,     0,     0,   115,   119,   115,     0,
       0,     0,     0,     0,     0,     0,    83,     0,   782,     0,
       0,   116,     0,     0,     0,     0,    83,     0,   115,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     119,     0,     0,   119,     0,     0,     0,     0,   813,     0,
     968,     0,     0,     0,   119,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   115,
       0,     0,     0,   837,   280,     0,     0,     0,     0,   119,
     119,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   386,     0,
       0,     0,     0,     0,     0,     0,     0,   116,     0,     0,
       0,     0,     0,     0,   119,     0,   119,   116,     0,     0,
       0,     0,   119,     0,   118,     0,   118,   119,     0,     0,
       0,     0,   879,     0,   281,     0,   280,   115,    83,     0,
      83,     0,   119,     0,     0,   386,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   701,     0,     0,     0,
      83,     0,     0,     0,     0,   900,     0,     0,     0,     0,
       0,     0,     0,   118,     0,   281,     0,     0,     0,   115,
       0,   701,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,     0,     0,   340,   341,     0,
       0,    83,     0,     0,   917,     0,     0,   327,   328,   329,
     330,   331,   332,   333,   334,   335,   336,   337,   338,   339,
       0,   269,   340,   341,     0,     0,     0,     0,     0,   116,
     342,   116,   343,   344,   345,   346,   347,   348,   349,   350,
     351,   352,     0,     0,     0,     0,     0,     0,     0,  -246,
       0,   116,     0,     0,     0,   342,     0,   343,   344,   345,
     346,   347,   348,   349,   350,   351,   352,     0,     0,    83,
     520,     0,     0,     0,     0,     0,   520,   520,     0,   115,
       0,     0,     0,   520,   520,     0,   115,   115,     0,     0,
     115,     0,   116,   115,     0,     0,     0,   281,     0,     0,
     115,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    83,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     118,     0,     0,   115,     0,     0,   115,     0,     0,     0,
       0,     0,     0,   966,     0,     0,     0,   115,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   281,
     116,     0,     0,     0,   118,     0,     0,     0,     0,     0,
       0,     0,   115,   115,     0,   118,     0,   118,     0,     0,
       0,     0,     0,     0,     0,     0,   118,     0,     0,     0,
       0,   384,     0,     0,     0,     0,     0,     0,     0,     0,
     520,     0,   116,     0,     0,     0,     0,   115,     0,   115,
       0,    83,     0,     0,     0,   115,     0,     0,    83,    83,
     115,     0,    83,     0,     0,    83,     0,     0,   118,     0,
       0,     0,    83,   118,     0,   115,     0,     0,   384,     0,
       0,     0,     0,     0,     0,   118,     0,     0,   118,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    83,     0,     0,    83,     0,
       0,     0,     0,     0,     0,   964,     0,     0,     0,    83,
       0,     0,   118,     0,     0,     0,     0,     0,   247,   247,
       0,     0,     0,   247,   247,   247,     0,     0,     0,     0,
       0,     0,   116,     0,    83,    83,     0,   247,     0,   116,
     116,     0,     0,   116,     0,     0,   116,     0,     0,     0,
       0,   247,     0,   116,     0,     0,     0,     0,     0,   118,
     118,     0,     0,     0,   247,   247,   247,     0,     0,    83,
       0,    83,     0,     0,     0,     0,     0,    83,     0,     0,
       0,     0,    83,     0,     0,     0,   116,     0,     0,   116,
       0,     0,     0,     0,     0,     0,   967,    83,   118,     0,
     116,     0,     0,     0,     0,  -642,     0,     0,   118,     0,
       0,     0,     0,  -642,  -642,  -642,     0,     0,  -642,  -642,
    -642,     0,  -642,     0,     0,   116,   116,     0,     0,     0,
       0,  -642,  -642,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -642,  -642,   385,  -642,  -642,  -642,  -642,  -642,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     116,     0,   116,     0,     0,     0,     0,     0,   116,     0,
       0,     0,     0,   116,     0,     0,   247,     0,     0,   247,
     247,   247,     0,   247,     0,     0,     0,     0,   116,     0,
       0,   385,  -642,  -642,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   327,   328,   329,   330,   331,   332,   333,
     334,   335,   336,   337,   338,   339,  -642,     0,   340,   341,
     118,     0,   118,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -642,
    -642,     0,   118,     0,   232,  -642,   247,  -642,     0,  -642,
    -642,   342,     0,   343,   344,   345,   346,   347,   348,   349,
     350,   351,   352,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   236,     0,
       0,     0,     0,   118,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   247,     0,     0,   247,   247,   247,
     247,   247,     0,     0,   247,   247,   247,   247,   247,   247,
     247,   247,   247,     0,     0,   247,   247,   247,   247,   247,
     247,   247,   247,   247,   247,   327,   328,   329,   330,   331,
     332,   333,   334,   335,   336,   337,   338,   339,     0,     0,
     340,   341,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   118,   118,     0,     0,     0,     0,     0,   118,   118,
       0,     0,     0,     0,     0,   118,   118,     0,     0,     0,
       0,     0,     0,   342,     0,   343,   344,   345,   346,   347,
     348,   349,   350,   351,   352,     0,   247,   247,   247,     0,
       0,     0,     0,   118,   247,   247,   247,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   247,     0,     0,   250,   250,
     247,     0,     0,   250,   250,   250,     0,     0,     0,     0,
       0,   247,     0,     0,   247,     0,     0,   250,     0,     0,
       0,     0,     0,   247,     0,     0,     0,     0,     0,     0,
       0,   250,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   247,     0,   250,   250,   250,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   247,     0,     0,
       0,     0,   118,     0,   247,     0,     0,     0,     0,     0,
       0,     0,     0,   118,     0,     0,     0,     0,     0,     0,
     118,   118,     0,     0,   118,     0,     0,   118,     0,     0,
       0,     0,     0,     0,   118,   327,   328,   329,   330,   331,
     332,   333,   334,   335,   336,   337,  -643,  -643,     0,     0,
     340,   341,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   247,     0,     0,   118,     0,     0,
     118,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   118,     0,     0,     0,   343,   344,   345,   346,   347,
     348,   349,   350,   351,   352,   247,   250,   247,     0,   250,
     250,   250,     0,   250,     0,     0,   118,   118,     0,     0,
     247,   247,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   247,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   118,     0,   118,     0,     0,     0,     0,     0,   118,
       0,     0,     0,     0,   118,   247,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   250,     0,     0,   118,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   247,     0,     0,   247,     0,
       0,   247,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     247,     0,     0,     0,   250,     0,     0,   250,   250,   250,
     250,   250,     0,     0,   250,   250,   250,   250,   250,   250,
     250,   250,   250,     0,     0,   250,   250,   250,   250,   250,
     250,   250,   250,   250,   250,     0,     0,     0,   247,   247,
       0,   247,   247,     0,     0,     0,     0,   551,   552,     0,
       0,   553,     0,     0,     0,     0,     0,     0,     0,   247,
       0,     0,     0,   167,   168,   169,   170,   171,   172,   173,
     174,   175,     0,     0,   176,   177,     0,     0,   178,   179,
     180,   181,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   247,   182,   183,     0,   250,   250,   250,     0,
       0,     0,   247,     0,   250,   250,   250,     0,     0,     0,
       0,     0,     0,     0,     0,   184,   185,   186,   187,   188,
     189,   190,   191,   192,   193,   250,   194,   195,     0,     0,
     250,   247,     0,   196,     0,     0,     0,     0,     0,     0,
       0,   250,   559,   560,   250,     0,   561,   247,   247,     0,
       0,     0,     0,   250,     0,     0,     0,     0,   167,   168,
     169,   170,   171,   172,   173,   174,   175,     0,     0,   176,
     177,     0,   250,   178,   179,   180,   181,     0,     0,     0,
       0,     0,     0,     0,     0,   247,     0,   250,   182,   183,
       0,     0,     0,     0,   250,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     184,   185,   186,   187,   188,   189,   190,   191,   192,   193,
       0,   194,   195,     0,     0,     0,     0,     0,   196,     0,
       0,     0,     0,   247,     0,     0,     0,     0,     0,     0,
     327,   328,   329,   330,   331,   332,   333,   334,     0,   336,
     337,     0,     0,     0,   250,   340,   341,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     247,     0,     0,     0,     0,   250,     0,   250,     0,     0,
     343,   344,   345,   346,   347,   348,   349,   350,   351,   352,
     250,   250,     0,  -642,     4,     0,     5,     6,     7,     8,
       9,     0,     0,     0,    10,    11,   250,     0,     0,    12,
       0,    13,    14,    15,    16,    17,    18,    19,     0,     0,
       0,     0,     0,    20,    21,    22,    23,    24,    25,    26,
       0,     0,    27,     0,     0,   250,     0,     0,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,     0,    41,    42,    43,    44,     0,    45,    46,    47,
       0,    48,    49,     0,     0,   250,     0,     0,   250,     0,
       0,   250,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    50,     0,     0,    51,    52,     0,    53,    54,
     250,    55,     0,     0,    56,    57,    58,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    65,    66,    67,     0,     0,   250,   250,
       0,   250,   250,     0,     0,  -642,     0,  -620,  -642,     0,
       0,     0,     0,     0,     0,  -620,  -620,  -620,     0,   250,
    -620,  -620,  -620,     0,  -620,     0,     0,     0,     0,     0,
       0,     0,     0,  -620,  -620,  -620,  -620,     0,     0,     0,
       0,     0,     0,     0,  -620,  -620,     0,  -620,  -620,  -620,
    -620,  -620,   250,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   250,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -620,  -620,  -620,
    -620,  -620,  -620,  -620,  -620,  -620,  -620,  -620,  -620,  -620,
       0,   250,  -620,  -620,  -620,  -620,     0,   758,  -620,     0,
       0,     0,     0,     0,  -620,     0,     0,   250,   250,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -620,     0,
       0,  -620,     0,     0,  -107,  -620,  -620,  -620,  -620,  -620,
    -620,  -620,  -620,  -620,  -620,  -620,  -620,     0,     0,     0,
       0,  -620,  -620,  -620,  -620,   250,  -518,  -620,  -620,  -620,
       0,  -620,  -620,     0,  -518,  -518,  -518,     0,     0,  -518,
    -518,  -518,     0,  -518,     0,     0,     0,     0,     0,     0,
       0,  -518,     0,  -518,  -518,  -518,     0,     0,     0,     0,
       0,     0,     0,  -518,  -518,     0,  -518,  -518,  -518,  -518,
    -518,     0,     0,   250,   327,  -643,  -643,  -643,  -643,   332,
     333,     0,     0,  -643,  -643,     0,     0,     0,     0,   340,
     341,     0,     0,     0,     0,     0,  -518,  -518,  -518,  -518,
    -518,  -518,  -518,  -518,  -518,  -518,  -518,  -518,  -518,     0,
       0,  -518,  -518,  -518,  -518,     0,  -518,  -518,     0,     0,
     250,     0,     0,  -518,   343,   344,   345,   346,   347,   348,
     349,   350,   351,   352,     0,     0,     0,  -518,     0,     0,
    -518,     0,     0,  -518,  -518,  -518,  -518,  -518,  -518,  -518,
    -518,  -518,  -518,  -518,  -518,  -518,     0,     0,     0,     0,
       0,  -518,  -518,  -518,  -521,     0,  -518,  -518,  -518,     0,
    -518,  -518,  -521,  -521,  -521,     0,     0,  -521,  -521,  -521,
       0,  -521,     0,     0,     0,     0,     0,     0,     0,  -521,
       0,  -521,  -521,  -521,     0,     0,     0,     0,     0,     0,
       0,  -521,  -521,     0,  -521,  -521,  -521,  -521,  -521,     0,
       0,     0,   327,   328,   329,   330,   331,   332,   333,     0,
       0,   336,   337,     0,     0,     0,     0,   340,   341,     0,
       0,     0,     0,     0,  -521,  -521,  -521,  -521,  -521,  -521,
    -521,  -521,  -521,  -521,  -521,  -521,  -521,     0,     0,  -521,
    -521,  -521,  -521,     0,  -521,  -521,     0,     0,     0,     0,
       0,  -521,   343,   344,   345,   346,   347,   348,   349,   350,
     351,   352,     0,     0,     0,  -521,     0,     0,  -521,     0,
       0,  -521,  -521,  -521,  -521,  -521,  -521,  -521,  -521,  -521,
    -521,  -521,  -521,  -521,     0,     0,     0,     0,     0,  -521,
    -521,  -521,  -621,     0,  -521,  -521,  -521,     0,  -521,  -521,
    -621,  -621,  -621,     0,     0,  -621,  -621,  -621,     0,  -621,
       0,     0,     0,     0,     0,     0,     0,     0,  -621,  -621,
    -621,  -621,     0,     0,     0,     0,     0,     0,     0,  -621,
    -621,     0,  -621,  -621,  -621,  -621,  -621,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -621,  -621,  -621,  -621,  -621,  -621,  -621,  -621,
    -621,  -621,  -621,  -621,  -621,     0,     0,  -621,  -621,  -621,
    -621,     0,     0,  -621,     0,     0,     0,     0,     0,  -621,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -621,     0,     0,  -621,     0,     0,     0,
    -621,  -621,  -621,  -621,  -621,  -621,  -621,  -621,  -621,  -621,
    -621,  -621,     0,     0,     0,     0,  -621,  -621,  -621,  -621,
    -622,     0,  -621,  -621,  -621,     0,  -621,  -621,  -622,  -622,
    -622,     0,     0,  -622,  -622,  -622,     0,  -622,     0,     0,
       0,     0,     0,     0,     0,     0,  -622,  -622,  -622,  -622,
       0,     0,     0,     0,     0,     0,     0,  -622,  -622,     0,
    -622,  -622,  -622,  -622,  -622,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -622,  -622,  -622,  -622,  -622,  -622,  -622,  -622,  -622,  -622,
    -622,  -622,  -622,     0,     0,  -622,  -622,  -622,  -622,     0,
       0,  -622,     0,     0,     0,     0,     0,  -622,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -622,     0,     0,  -622,     0,     0,     0,  -622,  -622,
    -622,  -622,  -622,  -622,  -622,  -622,  -622,  -622,  -622,  -622,
       0,     0,     0,     0,  -622,  -622,  -622,  -622,  -299,     0,
    -622,  -622,  -622,     0,  -622,  -622,  -299,  -299,  -299,     0,
       0,  -299,  -299,  -299,     0,  -299,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -299,  -299,  -299,     0,     0,
       0,     0,     0,     0,     0,  -299,  -299,     0,  -299,  -299,
    -299,  -299,  -299,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -299,  -299,
    -299,  -299,  -299,  -299,  -299,  -299,  -299,  -299,  -299,  -299,
    -299,     0,     0,  -299,  -299,  -299,  -299,     0,   759,  -299,
       0,     0,     0,     0,     0,  -299,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -299,
       0,     0,  -299,     0,     0,  -109,  -299,  -299,  -299,  -299,
    -299,  -299,  -299,  -299,  -299,  -299,  -299,  -299,     0,     0,
       0,     0,     0,  -299,  -299,  -299,  -439,     0,  -299,  -299,
    -299,     0,  -299,  -299,  -439,  -439,  -439,     0,     0,  -439,
    -439,  -439,     0,  -439,     0,     0,     0,     0,     0,     0,
       0,     0,  -439,  -439,  -439,     0,     0,     0,     0,     0,
       0,     0,     0,  -439,  -439,     0,  -439,  -439,  -439,  -439,
    -439,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -439,  -439,  -439,  -439,
    -439,  -439,  -439,  -439,  -439,  -439,  -439,  -439,  -439,     0,
       0,  -439,  -439,  -439,  -439,     0,     0,  -439,     0,     0,
       0,     0,     0,  -439,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -439,     0,     0,
       0,     0,     0,     0,  -439,     0,  -439,  -439,  -439,  -439,
    -439,  -439,  -439,  -439,  -439,  -439,     0,     0,     0,     0,
    -439,  -439,  -439,  -439,  -290,   232,  -439,  -439,  -439,     0,
    -439,  -439,  -290,  -290,  -290,     0,     0,  -290,  -290,  -290,
       0,  -290,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -290,  -290,  -290,     0,     0,     0,     0,     0,     0,
       0,  -290,  -290,     0,  -290,  -290,  -290,  -290,  -290,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -290,  -290,  -290,  -290,  -290,  -290,
    -290,  -290,  -290,  -290,  -290,  -290,  -290,     0,     0,  -290,
    -290,  -290,  -290,     0,     0,  -290,     0,     0,     0,     0,
       0,  -290,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -290,     0,     0,  -290,     0,
       0,     0,  -290,  -290,  -290,  -290,  -290,  -290,  -290,  -290,
    -290,  -290,  -290,  -290,     0,     0,     0,     0,     0,  -290,
    -290,  -290,  -429,     0,  -290,  -290,  -290,     0,  -290,  -290,
    -429,  -429,  -429,     0,     0,  -429,  -429,  -429,     0,  -429,
       0,     0,     0,     0,     0,     0,     0,     0,  -429,  -429,
    -429,     0,     0,     0,     0,     0,     0,     0,     0,  -429,
    -429,     0,  -429,  -429,  -429,  -429,  -429,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -429,  -429,  -429,  -429,  -429,  -429,  -429,  -429,
    -429,  -429,  -429,  -429,  -429,     0,     0,  -429,  -429,  -429,
    -429,     0,     0,  -429,     0,     0,     0,     0,     0,  -429,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -429,     0,     0,     0,     0,     0,     0,
    -429,     0,  -429,  -429,  -429,  -429,  -429,  -429,  -429,  -429,
    -429,  -429,     0,     0,     0,     0,  -429,  -429,  -429,  -429,
    -306,  -429,  -429,  -429,  -429,     0,  -429,  -429,  -306,  -306,
    -306,     0,     0,  -306,  -306,  -306,     0,  -306,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -306,  -306,     0,
       0,     0,     0,     0,     0,     0,     0,  -306,  -306,     0,
    -306,  -306,  -306,  -306,  -306,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -306,  -306,  -306,  -306,  -306,  -306,  -306,  -306,  -306,  -306,
    -306,  -306,  -306,     0,     0,  -306,  -306,  -306,  -306,     0,
       0,  -306,     0,     0,     0,     0,     0,  -306,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -306,     0,     0,     0,     0,     0,     0,  -306,     0,
    -306,  -306,  -306,  -306,  -306,  -306,  -306,  -306,  -306,  -306,
       0,     0,     0,     0,     0,  -306,  -306,  -306,  -620,   229,
    -306,  -306,  -306,     0,  -306,  -306,  -620,  -620,  -620,     0,
       0,     0,  -620,  -620,     0,  -620,     0,     0,     0,     0,
       0,     0,     0,     0,  -620,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -620,  -620,     0,  -620,  -620,
    -620,  -620,  -620,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -620,  -620,
    -620,  -620,  -620,  -620,  -620,  -620,  -620,  -620,  -620,  -620,
    -620,     0,     0,  -620,  -620,  -620,  -620,     0,   706,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -620,
       0,     0,     0,     0,     0,  -107,  -620,     0,  -620,  -620,
    -620,  -620,  -620,  -620,  -620,  -620,  -620,  -620,     0,     0,
       0,     0,  -620,  -620,  -620,   -98,  -620,     0,  -620,     0,
    -620,     0,  -620,  -620,  -620,  -620,  -620,     0,     0,     0,
    -620,  -620,     0,  -620,     0,     0,     0,     0,     0,     0,
       0,     0,  -620,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -620,  -620,     0,  -620,  -620,  -620,  -620,
    -620,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -620,  -620,  -620,  -620,
    -620,  -620,  -620,  -620,  -620,  -620,  -620,  -620,  -620,     0,
       0,  -620,  -620,  -620,  -620,     0,   706,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -620,     0,     0,
       0,     0,     0,  -107,  -620,     0,  -620,  -620,  -620,  -620,
    -620,  -620,  -620,  -620,  -620,  -620,     0,     0,     0,     0,
    -620,  -620,  -620,  -620,  -299,     0,  -620,     0,  -620,     0,
    -620,  -620,  -299,  -299,  -299,     0,     0,     0,  -299,  -299,
       0,  -299,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -299,  -299,     0,  -299,  -299,  -299,  -299,  -299,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -299,  -299,  -299,  -299,  -299,  -299,
    -299,  -299,  -299,  -299,  -299,  -299,  -299,     0,     0,  -299,
    -299,  -299,  -299,     0,   707,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -299,     0,     0,     0,     0,
       0,  -109,  -299,     0,  -299,  -299,  -299,  -299,  -299,  -299,
    -299,  -299,  -299,  -299,     0,     0,     0,     0,     0,  -299,
    -299,  -100,  -299,     0,  -299,     0,  -299,     0,  -299,  -299,
    -299,  -299,  -299,     0,     0,     0,  -299,  -299,     0,  -299,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -299,
    -299,     0,  -299,  -299,  -299,  -299,  -299,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -299,  -299,  -299,  -299,  -299,  -299,  -299,  -299,
    -299,  -299,  -299,  -299,  -299,     0,     0,  -299,  -299,  -299,
    -299,     0,   707,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -299,     0,     0,     0,     0,     0,  -109,
    -299,     0,  -299,  -299,  -299,  -299,  -299,  -299,  -299,  -299,
    -299,  -299,     0,     0,     0,     0,     0,  -299,  -299,  -299,
       0,     0,  -299,     0,  -299,     0,  -299,  -299,   253,     0,
       5,     6,     7,     8,     9,  -642,  -642,  -642,    10,    11,
       0,     0,  -642,    12,     0,    13,    14,    15,    16,    17,
      18,    19,     0,     0,     0,     0,     0,    20,    21,    22,
      23,    24,    25,    26,     0,     0,    27,     0,     0,     0,
       0,     0,    28,    29,   254,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,     0,    41,    42,    43,    44,
       0,    45,    46,    47,     0,    48,    49,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    50,     0,     0,    51,
      52,     0,    53,    54,     0,    55,     0,     0,    56,    57,
      58,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    65,    66,    67,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -642,
       0,   253,  -642,     5,     6,     7,     8,     9,     0,     0,
    -642,    10,    11,     0,  -642,  -642,    12,     0,    13,    14,
      15,    16,    17,    18,    19,     0,     0,     0,     0,     0,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,    28,    29,   254,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,     0,    41,
      42,    43,    44,     0,    45,    46,    47,     0,    48,    49,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    50,
       0,     0,    51,    52,     0,    53,    54,     0,    55,     0,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      65,    66,    67,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -642,     0,   253,  -642,     5,     6,     7,     8,
       9,     0,     0,  -642,    10,    11,     0,     0,  -642,    12,
    -642,    13,    14,    15,    16,    17,    18,    19,     0,     0,
       0,     0,     0,    20,    21,    22,    23,    24,    25,    26,
       0,     0,    27,     0,     0,     0,     0,     0,    28,    29,
     254,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,     0,    41,    42,    43,    44,     0,    45,    46,    47,
       0,    48,    49,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    50,     0,     0,    51,    52,     0,    53,    54,
       0,    55,     0,     0,    56,    57,    58,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    65,    66,    67,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -642,     0,   253,  -642,     5,
       6,     7,     8,     9,     0,     0,  -642,    10,    11,     0,
       0,  -642,    12,     0,    13,    14,    15,    16,    17,    18,
      19,     0,     0,     0,     0,     0,    20,    21,    22,    23,
      24,    25,    26,     0,     0,    27,     0,     0,     0,     0,
       0,    28,    29,   254,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,     0,    41,    42,    43,    44,     0,
      45,    46,    47,     0,    48,    49,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    50,     0,     0,    51,    52,
       0,    53,    54,     0,    55,     0,     0,    56,    57,    58,
      59,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,   253,     0,     5,     6,     7,     8,
       9,     0,  -642,  -642,    10,    11,    65,    66,    67,    12,
       0,    13,    14,    15,    16,    17,    18,    19,  -642,     0,
       0,  -642,     0,    20,    21,    22,    23,    24,    25,    26,
       0,     0,    27,     0,     0,     0,     0,     0,    28,    29,
     254,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,     0,    41,    42,    43,    44,     0,    45,    46,    47,
       0,    48,    49,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    50,     0,     0,    51,    52,     0,    53,    54,
       0,    55,     0,     0,    56,    57,    58,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,   253,     0,     5,     6,     7,     8,     9,     0,     0,
       0,    10,    11,    65,    66,    67,    12,     0,    13,    14,
      15,    16,    17,    18,    19,  -642,     0,     0,  -642,     0,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,    28,    29,   254,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,     0,    41,
      42,    43,    44,     0,    45,    46,    47,     0,    48,    49,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    50,
       0,     0,   255,    52,     0,    53,    54,     0,    55,     0,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      65,    66,    67,   253,     0,     5,     6,     7,     8,     9,
    -642,     0,  -642,    10,    11,  -642,     0,     0,    12,     0,
      13,    14,    15,    16,    17,    18,    19,     0,     0,     0,
       0,     0,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,    28,    29,   254,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
       0,    41,    42,    43,    44,     0,    45,    46,    47,     0,
      48,    49,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    50,     0,     0,    51,    52,     0,    53,    54,     0,
      55,     0,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    65,    66,    67,   253,     0,     5,     6,     7,
       8,     9,  -642,     0,  -642,    10,    11,  -642,     0,     0,
      12,     0,    13,    14,    15,    16,    17,    18,    19,     0,
       0,     0,     0,     0,    20,    21,    22,    23,    24,    25,
      26,     0,     0,    27,     0,     0,     0,     0,     0,    28,
      29,   254,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,     0,    41,    42,    43,    44,     0,    45,    46,
      47,     0,    48,    49,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    50,     0,     0,    51,    52,     0,    53,
      54,     0,    55,     0,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    65,    66,    67,     0,     0,  -642,
       0,     0,     0,     0,     0,     0,  -642,     0,     4,  -642,
       5,     6,     7,     8,     9,     0,     0,     0,    10,    11,
       0,     0,     0,    12,     0,    13,    14,    15,    16,    17,
      18,    19,     0,     0,     0,     0,     0,    20,    21,    22,
      23,    24,    25,    26,     0,     0,    27,     0,     0,     0,
       0,     0,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,     0,    41,    42,    43,    44,
       0,    45,    46,    47,     0,    48,    49,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    50,     0,     0,    51,
      52,     0,    53,    54,     0,    55,     0,     0,    56,    57,
      58,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    65,    66,    67,
       0,     0,  -642,     0,     0,     0,     0,     0,     0,  -642,
       0,   253,  -642,     5,     6,     7,     8,     9,     0,     0,
    -642,    10,    11,     0,     0,     0,    12,     0,    13,    14,
      15,    16,    17,    18,    19,     0,     0,     0,     0,     0,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,    28,    29,   254,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,     0,    41,
      42,    43,    44,     0,    45,    46,    47,     0,    48,    49,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    50,
       0,     0,    51,    52,     0,    53,    54,     0,    55,     0,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,   253,     0,
       5,     6,     7,     8,     9,     0,     0,     0,    10,    11,
      65,    66,    67,    12,     0,    13,    14,    15,    16,    17,
      18,    19,  -642,     0,     0,  -642,     0,    20,    21,    22,
      23,    24,    25,    26,     0,     0,    27,     0,     0,     0,
       0,     0,    28,    29,   254,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,     0,    41,    42,    43,    44,
       0,    45,    46,    47,     0,    48,    49,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    50,     0,     0,    51,
      52,     0,    53,    54,     0,    55,     0,     0,    56,    57,
      58,    59,    60,    61,    62,    63,    64,     0,  -642,     0,
       0,     0,     0,     0,     0,     0,     0,     5,     6,     7,
       0,     9,     0,     0,     0,    10,    11,    65,    66,    67,
      12,     0,    13,    14,    15,    16,    17,    18,    19,  -642,
       0,     0,  -642,     0,    20,    21,    22,    23,    24,    25,
      26,     0,     0,   205,     0,     0,     0,     0,     0,     0,
      29,     0,     0,    32,    33,    34,    35,    36,    37,    38,
      39,    40,   206,    41,    42,    43,    44,     0,    45,    46,
      47,     0,    48,    49,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   207,     0,     0,   208,    52,     0,    53,
      54,     0,   209,   210,   211,    56,    57,   212,    59,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     0,     9,     0,
       0,     0,    10,    11,    65,   213,    67,    12,     0,    13,
      14,    15,    16,    17,    18,    19,     0,     0,     0,   236,
       0,    20,    21,    22,    23,    24,    25,    26,     0,     0,
      27,     0,     0,     0,     0,     0,     0,    29,     0,     0,
      32,    33,    34,    35,    36,    37,    38,    39,    40,     0,
      41,    42,    43,    44,     0,    45,    46,    47,     0,    48,
      49,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     207,     0,     0,   208,    52,     0,    53,    54,     0,     0,
       0,     0,    56,    57,    58,    59,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     5,     6,     7,     0,     9,     0,     0,     0,    10,
      11,    65,    66,    67,    12,     0,    13,    14,    15,    16,
      17,    18,    19,   302,     0,     0,   303,     0,    20,    21,
      22,    23,    24,    25,    26,     0,     0,    27,     0,     0,
       0,     0,     0,     0,    29,     0,     0,    32,    33,    34,
      35,    36,    37,    38,    39,    40,     0,    41,    42,    43,
      44,     0,    45,    46,    47,     0,    48,    49,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   207,     0,     0,
     208,    52,     0,    53,    54,     0,     0,     0,     0,    56,
      57,    58,    59,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     8,     9,     0,     0,     0,    10,    11,    65,    66,
      67,    12,     0,    13,    14,    15,    16,    17,    18,    19,
       0,     0,     0,   236,     0,    20,    21,    22,    23,    24,
      25,    26,     0,     0,    27,     0,     0,     0,     0,     0,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,     0,    41,    42,    43,    44,     0,    45,
      46,    47,     0,    48,    49,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    50,     0,     0,    51,    52,     0,
      53,    54,     0,    55,     0,     0,    56,    57,    58,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     5,     6,     7,     8,     9,
       0,     0,     0,    10,    11,    65,    66,    67,    12,     0,
      13,    14,    15,    16,    17,    18,    19,   493,     0,     0,
       0,     0,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,    28,    29,   254,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
       0,    41,    42,    43,    44,     0,    45,    46,    47,     0,
      48,    49,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    50,     0,     0,    51,    52,     0,    53,    54,     0,
      55,     0,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    65,    66,    67,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   493,   123,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,   134,   135,   136,   137,
     138,   139,   140,   141,   142,   143,   144,   145,   146,     0,
       0,     0,   147,   148,   149,   388,   389,   390,   391,   154,
     155,   156,     0,     0,     0,     0,     0,   157,   158,   159,
     160,   392,   393,   394,   395,   165,    37,    38,   396,    40,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     167,   168,   169,   170,   171,   172,   173,   174,   175,     0,
       0,   176,   177,     0,     0,   178,   179,   180,   181,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     182,   183,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   184,   185,   186,   187,   188,   189,   190,   191,
     192,   193,     0,   194,   195,     0,     0,     0,     0,     0,
     196,   397,   123,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,   134,   135,   136,   137,   138,   139,   140,
     141,   142,   143,   144,   145,   146,     0,     0,     0,   147,
     148,   149,   150,   151,   152,   153,   154,   155,   156,     0,
       0,     0,     0,     0,   157,   158,   159,   160,   161,   162,
     163,   164,   165,   285,   286,   166,   287,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   167,   168,   169,
     170,   171,   172,   173,   174,   175,     0,     0,   176,   177,
       0,     0,   178,   179,   180,   181,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   182,   183,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   184,
     185,   186,   187,   188,   189,   190,   191,   192,   193,     0,
     194,   195,     0,     0,     0,     0,     0,   196,   123,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     145,   146,     0,     0,     0,   147,   148,   149,   150,   151,
     152,   153,   154,   155,   156,     0,     0,     0,     0,     0,
     157,   158,   159,   160,   161,   162,   163,   164,   165,   238,
       0,   166,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   167,   168,   169,   170,   171,   172,   173,
     174,   175,     0,     0,   176,   177,     0,     0,   178,   179,
     180,   181,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   182,   183,     0,     0,    57,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   184,   185,   186,   187,   188,
     189,   190,   191,   192,   193,     0,   194,   195,     0,     0,
       0,     0,     0,   196,   123,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,   134,   135,   136,   137,   138,
     139,   140,   141,   142,   143,   144,   145,   146,     0,     0,
       0,   147,   148,   149,   150,   151,   152,   153,   154,   155,
     156,     0,     0,     0,     0,     0,   157,   158,   159,   160,
     161,   162,   163,   164,   165,     0,     0,   166,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   167,
     168,   169,   170,   171,   172,   173,   174,   175,     0,     0,
     176,   177,     0,     0,   178,   179,   180,   181,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   182,
     183,     0,     0,    57,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   184,   185,   186,   187,   188,   189,   190,   191,   192,
     193,     0,   194,   195,     0,     0,     0,     0,     0,   196,
     123,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,   134,   135,   136,   137,   138,   139,   140,   141,   142,
     143,   144,   145,   146,     0,     0,     0,   147,   148,   149,
     150,   151,   152,   153,   154,   155,   156,     0,     0,     0,
       0,     0,   157,   158,   159,   160,   161,   162,   163,   164,
     165,     0,     0,   166,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   167,   168,   169,   170,   171,
     172,   173,   174,   175,     0,     0,   176,   177,     0,     0,
     178,   179,   180,   181,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   182,   183,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   184,   185,   186,
     187,   188,   189,   190,   191,   192,   193,     0,   194,   195,
       5,     6,     7,     0,     9,   196,     0,     0,    10,    11,
       0,     0,     0,    12,     0,    13,    14,    15,   243,   244,
      18,    19,     0,     0,     0,     0,     0,    20,   245,   246,
      23,    24,    25,    26,     0,     0,   205,     0,     0,     0,
       0,     0,     0,   273,     0,     0,    32,    33,    34,    35,
      36,    37,    38,    39,    40,     0,    41,    42,    43,    44,
       0,    45,    46,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   274,     0,     0,   208,
      52,     0,    53,    54,     0,     0,     0,     0,    56,    57,
      58,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       5,     6,     7,     0,     9,     0,     0,   275,    10,    11,
       0,     0,     0,    12,   276,    13,    14,    15,   243,   244,
      18,    19,     0,     0,     0,     0,     0,    20,   245,   246,
      23,    24,    25,    26,     0,     0,   205,     0,     0,     0,
       0,     0,     0,   273,     0,     0,    32,    33,    34,    35,
      36,    37,    38,    39,    40,     0,    41,    42,    43,    44,
       0,    45,    46,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   274,     0,     0,   208,
      52,     0,    53,    54,     0,     0,     0,     0,    56,    57,
      58,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       5,     6,     7,     8,     9,     0,     0,   275,    10,    11,
       0,     0,     0,    12,   514,    13,    14,    15,    16,    17,
      18,    19,     0,     0,     0,     0,     0,    20,    21,    22,
      23,    24,    25,    26,     0,     0,    27,     0,     0,     0,
       0,     0,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,     0,    41,    42,    43,    44,
       0,    45,    46,    47,     0,    48,    49,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    50,     0,     0,    51,
      52,     0,    53,    54,     0,    55,     0,     0,    56,    57,
      58,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     5,     6,     7,
       0,     9,     0,     0,     0,    10,    11,    65,    66,    67,
      12,     0,    13,    14,    15,    16,    17,    18,    19,     0,
       0,     0,     0,     0,    20,    21,    22,    23,    24,    25,
      26,     0,     0,   205,     0,     0,     0,     0,     0,     0,
      29,     0,     0,    32,    33,    34,    35,    36,    37,    38,
      39,    40,   206,    41,    42,    43,    44,     0,    45,    46,
      47,     0,    48,    49,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   207,     0,     0,   208,    52,     0,    53,
      54,     0,   209,   210,   211,    56,    57,   212,    59,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     8,     9,     0,
       0,     0,    10,    11,    65,   213,    67,    12,     0,    13,
      14,    15,    16,    17,    18,    19,     0,     0,     0,     0,
       0,    20,    21,    22,    23,    24,    25,    26,     0,     0,
      27,     0,     0,     0,     0,     0,    28,    29,     0,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,     0,
      41,    42,    43,    44,     0,    45,    46,    47,     0,    48,
      49,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      50,     0,     0,    51,    52,     0,    53,    54,     0,    55,
       0,     0,    56,    57,    58,    59,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     5,     6,     7,     0,     9,     0,     0,     0,    10,
      11,    65,    66,    67,    12,     0,    13,    14,    15,   243,
     244,    18,    19,     0,     0,     0,     0,     0,    20,   245,
     246,    23,    24,    25,    26,     0,     0,   205,     0,     0,
       0,     0,     0,     0,    29,     0,     0,    32,    33,    34,
      35,    36,    37,    38,    39,    40,   206,    41,    42,    43,
      44,     0,    45,    46,    47,     0,    48,    49,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   207,     0,     0,
     208,    52,     0,    53,    54,     0,   607,   210,   211,    56,
      57,   212,    59,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     0,     9,     0,     0,     0,    10,    11,    65,   213,
      67,    12,     0,    13,    14,    15,   243,   244,    18,    19,
       0,     0,     0,     0,     0,    20,   245,   246,    23,    24,
      25,    26,     0,     0,   205,     0,     0,     0,     0,     0,
       0,    29,     0,     0,    32,    33,    34,    35,    36,    37,
      38,    39,    40,   206,    41,    42,    43,    44,     0,    45,
      46,    47,     0,    48,    49,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   207,     0,     0,   208,    52,     0,
      53,    54,     0,   209,   210,     0,    56,    57,   212,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     5,     6,     7,     0,     9,
       0,     0,     0,    10,    11,    65,   213,    67,    12,     0,
      13,    14,    15,   243,   244,    18,    19,     0,     0,     0,
       0,     0,    20,   245,   246,    23,    24,    25,    26,     0,
       0,   205,     0,     0,     0,     0,     0,     0,    29,     0,
       0,    32,    33,    34,    35,    36,    37,    38,    39,    40,
     206,    41,    42,    43,    44,     0,    45,    46,    47,     0,
      48,    49,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   207,     0,     0,   208,    52,     0,    53,    54,     0,
       0,   210,   211,    56,    57,   212,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     0,     9,     0,     0,     0,
      10,    11,    65,   213,    67,    12,     0,    13,    14,    15,
     243,   244,    18,    19,     0,     0,     0,     0,     0,    20,
     245,   246,    23,    24,    25,    26,     0,     0,   205,     0,
       0,     0,     0,     0,     0,    29,     0,     0,    32,    33,
      34,    35,    36,    37,    38,    39,    40,   206,    41,    42,
      43,    44,     0,    45,    46,    47,     0,    48,    49,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   207,     0,
       0,   208,    52,     0,    53,    54,     0,   607,   210,     0,
      56,    57,   212,    59,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     5,
       6,     7,     0,     9,     0,     0,     0,    10,    11,    65,
     213,    67,    12,     0,    13,    14,    15,   243,   244,    18,
      19,     0,     0,     0,     0,     0,    20,   245,   246,    23,
      24,    25,    26,     0,     0,   205,     0,     0,     0,     0,
       0,     0,    29,     0,     0,    32,    33,    34,    35,    36,
      37,    38,    39,    40,   206,    41,    42,    43,    44,     0,
      45,    46,    47,     0,    48,    49,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   207,     0,     0,   208,    52,
       0,    53,    54,     0,     0,   210,     0,    56,    57,   212,
      59,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     0,
       9,     0,     0,     0,    10,    11,    65,   213,    67,    12,
       0,    13,    14,    15,    16,    17,    18,    19,     0,     0,
       0,     0,     0,    20,    21,    22,    23,    24,    25,    26,
       0,     0,   205,     0,     0,     0,     0,     0,     0,    29,
       0,     0,    32,    33,    34,    35,    36,    37,    38,    39,
      40,     0,    41,    42,    43,    44,     0,    45,    46,    47,
       0,    48,    49,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   207,     0,     0,   208,    52,     0,    53,    54,
       0,   507,     0,     0,    56,    57,    58,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     5,     6,     7,     0,     9,     0,     0,
       0,    10,    11,    65,   213,    67,    12,     0,    13,    14,
      15,   243,   244,    18,    19,     0,     0,     0,     0,     0,
      20,   245,   246,    23,    24,    25,    26,     0,     0,   205,
       0,     0,     0,     0,     0,     0,    29,     0,     0,    32,
      33,    34,    35,    36,    37,    38,    39,    40,     0,    41,
      42,    43,    44,     0,    45,    46,    47,     0,    48,    49,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   207,
       0,     0,   208,    52,     0,    53,    54,     0,   209,     0,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       5,     6,     7,     0,     9,     0,     0,     0,    10,    11,
      65,   213,    67,    12,     0,    13,    14,    15,   243,   244,
      18,    19,     0,     0,     0,     0,     0,    20,   245,   246,
      23,    24,    25,    26,     0,     0,   205,     0,     0,     0,
       0,     0,     0,    29,     0,     0,    32,    33,    34,    35,
      36,    37,    38,    39,    40,     0,    41,    42,    43,    44,
       0,    45,    46,    47,     0,    48,    49,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   207,     0,     0,   208,
      52,     0,    53,    54,     0,   812,     0,     0,    56,    57,
      58,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     5,     6,     7,
       0,     9,     0,     0,     0,    10,    11,    65,   213,    67,
      12,     0,    13,    14,    15,   243,   244,    18,    19,     0,
       0,     0,     0,     0,    20,   245,   246,    23,    24,    25,
      26,     0,     0,   205,     0,     0,     0,     0,     0,     0,
      29,     0,     0,    32,    33,    34,    35,    36,    37,    38,
      39,    40,     0,    41,    42,    43,    44,     0,    45,    46,
      47,     0,    48,    49,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   207,     0,     0,   208,    52,     0,    53,
      54,     0,   507,     0,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     0,     9,     0,
       0,     0,    10,    11,    65,   213,    67,    12,     0,    13,
      14,    15,   243,   244,    18,    19,     0,     0,     0,     0,
       0,    20,   245,   246,    23,    24,    25,    26,     0,     0,
     205,     0,     0,     0,     0,     0,     0,    29,     0,     0,
      32,    33,    34,    35,    36,    37,    38,    39,    40,     0,
      41,    42,    43,    44,     0,    45,    46,    47,     0,    48,
      49,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     207,     0,     0,   208,    52,     0,    53,    54,     0,   607,
       0,     0,    56,    57,    58,    59,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     5,     6,     7,     0,     9,     0,     0,     0,    10,
      11,    65,   213,    67,    12,     0,    13,    14,    15,   243,
     244,    18,    19,     0,     0,     0,     0,     0,    20,   245,
     246,    23,    24,    25,    26,     0,     0,   205,     0,     0,
       0,     0,     0,     0,    29,     0,     0,    32,    33,    34,
      35,    36,    37,    38,    39,    40,     0,    41,    42,    43,
      44,     0,    45,    46,    47,     0,    48,    49,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   207,     0,     0,
     208,    52,     0,    53,    54,     0,     0,     0,     0,    56,
      57,    58,    59,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     0,     9,     0,     0,     0,    10,    11,    65,   213,
      67,    12,     0,    13,    14,    15,    16,    17,    18,    19,
       0,     0,     0,     0,     0,    20,    21,    22,    23,    24,
      25,    26,     0,     0,   205,     0,     0,     0,     0,     0,
       0,    29,     0,     0,    32,    33,    34,    35,    36,    37,
      38,    39,    40,     0,    41,    42,    43,    44,     0,    45,
      46,    47,     0,    48,    49,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   207,     0,     0,   208,    52,     0,
      53,    54,     0,     0,     0,     0,    56,    57,    58,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     5,     6,     7,     0,     9,
       0,     0,     0,    10,    11,    65,   213,    67,    12,     0,
      13,    14,    15,    16,    17,    18,    19,     0,     0,     0,
       0,     0,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,     0,    29,     0,
       0,    32,    33,    34,    35,    36,    37,    38,    39,    40,
       0,    41,    42,    43,    44,     0,    45,    46,    47,     0,
      48,    49,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   207,     0,     0,   208,    52,     0,    53,    54,     0,
       0,     0,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     0,     9,     0,     0,     0,
      10,    11,    65,    66,    67,    12,     0,    13,    14,    15,
     243,   244,    18,    19,     0,     0,     0,     0,     0,    20,
     245,   246,    23,    24,    25,    26,     0,     0,   205,     0,
       0,     0,     0,     0,     0,   273,     0,     0,    32,    33,
      34,    35,    36,    37,    38,    39,    40,     0,    41,    42,
      43,    44,     0,    45,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   274,     0,
       0,   323,    52,     0,    53,    54,     0,   324,     0,     0,
      56,    57,    58,    59,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     5,     6,     7,     0,     9,
       0,     0,     0,    10,    11,     0,     0,     0,    12,   275,
      13,    14,    15,   243,   244,    18,    19,     0,     0,     0,
       0,     0,    20,   245,   246,    23,    24,    25,    26,     0,
       0,   205,     0,     0,     0,     0,     0,     0,   273,     0,
       0,    32,    33,    34,    35,    36,    37,    38,    39,    40,
       0,    41,    42,    43,    44,     0,    45,    46,    47,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   369,     0,     0,    51,    52,     0,    53,    54,     0,
      55,     0,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     0,     9,     0,     0,     0,    10,    11,     0,     0,
       0,    12,   275,    13,    14,    15,   243,   244,    18,    19,
       0,     0,     0,     0,     0,    20,   245,   246,    23,    24,
      25,    26,     0,     0,   205,     0,     0,     0,     0,     0,
       0,   273,     0,     0,    32,    33,    34,   377,    36,    37,
      38,   378,    40,     0,    41,    42,    43,    44,     0,    45,
      46,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     379,     0,     0,     0,   380,     0,     0,   208,    52,     0,
      53,    54,     0,     0,     0,     0,    56,    57,    58,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     5,     6,     7,     0,     9,     0,     0,     0,    10,
      11,     0,     0,     0,    12,   275,    13,    14,    15,   243,
     244,    18,    19,     0,     0,     0,     0,     0,    20,   245,
     246,    23,    24,    25,    26,     0,     0,   205,     0,     0,
       0,     0,     0,     0,   273,     0,     0,    32,    33,    34,
     377,    36,    37,    38,   378,    40,     0,    41,    42,    43,
      44,     0,    45,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   380,     0,     0,
     208,    52,     0,    53,    54,     0,     0,     0,     0,    56,
      57,    58,    59,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     0,     9,     0,
       0,     0,    10,    11,     0,     0,     0,    12,   275,    13,
      14,    15,   243,   244,    18,    19,     0,     0,     0,     0,
       0,    20,   245,   246,    23,    24,    25,    26,     0,     0,
     205,     0,     0,     0,     0,     0,     0,   273,     0,     0,
      32,    33,    34,    35,    36,    37,    38,    39,    40,     0,
      41,    42,    43,    44,     0,    45,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     274,     0,     0,   323,    52,     0,    53,    54,     0,     0,
       0,     0,    56,    57,    58,    59,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     5,     6,     7,
       0,     9,     0,     0,     0,    10,    11,     0,     0,     0,
      12,   275,    13,    14,    15,   243,   244,    18,    19,     0,
       0,     0,     0,     0,    20,   245,   246,    23,    24,    25,
      26,     0,     0,   205,     0,     0,     0,     0,     0,     0,
     273,     0,     0,    32,    33,    34,    35,    36,    37,    38,
      39,    40,     0,    41,    42,    43,    44,     0,    45,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   963,     0,     0,   208,    52,     0,    53,
      54,     0,     0,     0,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       5,     6,     7,     0,     9,     0,     0,     0,    10,    11,
       0,     0,     0,    12,   275,    13,    14,    15,   243,   244,
      18,    19,     0,     0,     0,     0,     0,    20,   245,   246,
      23,    24,    25,    26,     0,     0,   205,     0,     0,     0,
       0,     0,     0,   273,     0,     0,    32,    33,    34,    35,
      36,    37,    38,    39,    40,     0,    41,    42,    43,    44,
       0,    45,    46,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1003,     0,     0,   208,
      52,     0,    53,    54,     0,     0,     0,     0,    56,    57,
      58,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,   596,   560,     0,     0,   597,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   275,   167,   168,
     169,   170,   171,   172,   173,   174,   175,     0,     0,   176,
     177,     0,     0,   178,   179,   180,   181,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   182,   183,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     184,   185,   186,   187,   188,   189,   190,   191,   192,   193,
       0,   194,   195,   611,   552,     0,     0,   612,   196,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   167,
     168,   169,   170,   171,   172,   173,   174,   175,     0,     0,
     176,   177,     0,     0,   178,   179,   180,   181,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   182,
     183,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   184,   185,   186,   187,   188,   189,   190,   191,   192,
     193,     0,   194,   195,   614,   560,     0,     0,   615,   196,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     167,   168,   169,   170,   171,   172,   173,   174,   175,     0,
       0,   176,   177,     0,     0,   178,   179,   180,   181,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     182,   183,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   184,   185,   186,   187,   188,   189,   190,   191,
     192,   193,     0,   194,   195,   638,   552,     0,     0,   639,
     196,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   167,   168,   169,   170,   171,   172,   173,   174,   175,
       0,     0,   176,   177,     0,     0,   178,   179,   180,   181,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   182,   183,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   184,   185,   186,   187,   188,   189,   190,
     191,   192,   193,     0,   194,   195,   641,   560,     0,     0,
     642,   196,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   167,   168,   169,   170,   171,   172,   173,   174,
     175,     0,     0,   176,   177,     0,     0,   178,   179,   180,
     181,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   182,   183,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   184,   185,   186,   187,   188,   189,
     190,   191,   192,   193,     0,   194,   195,   733,   552,     0,
       0,   734,   196,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   167,   168,   169,   170,   171,   172,   173,
     174,   175,     0,     0,   176,   177,     0,     0,   178,   179,
     180,   181,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   182,   183,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   184,   185,   186,   187,   188,
     189,   190,   191,   192,   193,     0,   194,   195,   736,   560,
       0,     0,   737,   196,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   167,   168,   169,   170,   171,   172,
     173,   174,   175,     0,     0,   176,   177,     0,     0,   178,
     179,   180,   181,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   182,   183,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   184,   185,   186,   187,
     188,   189,   190,   191,   192,   193,     0,   194,   195,   742,
     552,     0,     0,   743,   196,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   167,   168,   169,   170,   171,
     172,   173,   174,   175,     0,     0,   176,   177,     0,     0,
     178,   179,   180,   181,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   182,   183,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   184,   185,   186,
     187,   188,   189,   190,   191,   192,   193,     0,   194,   195,
     817,   552,     0,     0,   818,   196,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   167,   168,   169,   170,
     171,   172,   173,   174,   175,     0,     0,   176,   177,     0,
       0,   178,   179,   180,   181,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   182,   183,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   184,   185,
     186,   187,   188,   189,   190,   191,   192,   193,     0,   194,
     195,   820,   560,     0,     0,   821,   196,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   167,   168,   169,
     170,   171,   172,   173,   174,   175,     0,     0,   176,   177,
       0,     0,   178,   179,   180,   181,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   182,   183,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   184,
     185,   186,   187,   188,   189,   190,   191,   192,   193,     0,
     194,   195,  1045,   552,     0,     0,  1046,   196,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   167,   168,
     169,   170,   171,   172,   173,   174,   175,     0,     0,   176,
     177,     0,     0,   178,   179,   180,   181,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   182,   183,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     184,   185,   186,   187,   188,   189,   190,   191,   192,   193,
       0,   194,   195,  1048,   560,     0,     0,  1049,   196,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   167,
     168,   169,   170,   171,   172,   173,   174,   175,     0,     0,
     176,   177,     0,     0,   178,   179,   180,   181,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   182,
     183,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   184,   185,   186,   187,   188,   189,   190,   191,   192,
     193,     0,   194,   195,  1062,   552,     0,     0,  1063,   196,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     167,   168,   169,   170,   171,   172,   173,   174,   175,     0,
       0,   176,   177,     0,     0,   178,   179,   180,   181,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     182,   183,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   184,   185,   186,   187,   188,   189,   190,   191,
     192,   193,     0,   194,   195,     0,     0,     0,     0,     0,
     196
};

static const yytype_int16 yycheck[] =
{
       2,    81,    27,    66,    29,    69,    57,    16,    17,   100,
      87,    22,   477,    28,   572,    76,    16,    17,   363,   473,
     326,     8,    99,    90,   418,   357,    93,   573,     8,    93,
     446,    55,   426,   326,    93,     8,     4,    55,   426,   292,
     293,    28,    16,    17,   356,   780,    97,   359,    28,    16,
      17,    53,    54,    53,    54,    28,   872,    81,    65,     2,
     514,     4,   267,    69,   669,    76,   271,   226,   656,   789,
      94,    95,    96,    16,    17,    51,   464,    57,     2,    13,
       4,    51,    27,   380,    13,   874,    53,   613,   933,    98,
     422,   477,   404,    26,   255,    58,    13,    29,    98,    89,
      90,   407,   408,    93,   407,   408,   721,    13,    51,   421,
     725,   423,    55,   929,   640,    63,   671,    97,   673,    63,
      25,    87,    88,    66,    98,  1008,   894,    78,   709,    88,
       0,    98,    90,    90,    90,   716,    90,   449,    81,    88,
      90,    63,    13,   475,   221,   134,    89,    90,    25,    25,
      93,    94,    95,    96,    63,    98,   117,    25,    56,   117,
     117,   117,   323,   117,   476,   113,   110,   117,   112,   985,
      25,   122,   249,    37,    38,  1058,   137,  1022,   144,    13,
     590,   949,   114,    26,    13,   144,   247,   137,   110,    25,
     112,   656,    25,   257,   983,   144,   276,   142,   257,   143,
     145,   134,   279,   137,   113,  1021,   139,   942,   142,   735,
     139,   145,   143,   142,    25,   631,   145,   937,   220,   745,
     229,   143,   231,   232,   226,   142,   620,   621,   145,   229,
     232,   231,   137,   621,   139,   488,   142,   490,   208,   145,
     307,   308,   309,   310,   698,   235,   578,   237,   273,   212,
      25,   257,   276,   235,   145,   229,   588,   231,   321,    13,
     137,   137,   229,   326,   231,   208,  1001,   579,   275,   137,
     656,   142,   242,   861,   145,   863,   435,   589,   260,   255,
     885,   263,   137,    13,   139,   255,   229,    13,   231,   232,
     587,   134,   235,   819,   237,   372,   854,   364,   365,   242,
     324,   137,   366,   918,   137,   139,   324,   366,   142,   140,
     253,   145,   255,   142,   145,   780,   145,   307,   308,   309,
     310,   264,   312,   313,   789,    90,   137,   882,   883,   253,
    1065,   741,   887,   276,   889,    15,   891,    17,   306,    88,
     264,    88,   143,    88,   407,   408,   894,   323,   357,    13,
     955,   956,   117,   323,   321,   357,    14,    15,    90,   326,
      37,    38,   137,   306,   307,   308,   309,   310,   311,   312,
     313,   703,   918,    52,   364,   365,   137,    56,   321,   359,
     323,   324,   306,   326,   145,   117,   731,   311,   142,   379,
     702,   145,   704,   473,   700,   144,   861,   144,   863,   144,
     706,   707,   143,   706,   707,   137,   142,   713,   714,   145,
     713,   714,   142,   422,   357,   145,   142,   360,   418,   145,
     422,   364,   365,   143,   404,    28,   679,   720,   443,   894,
     635,   684,   434,   435,   514,  1040,   379,    88,   993,   994,
     995,   996,   117,   423,   446,   855,   428,   137,   525,   473,
     432,   610,   862,  1001,   117,   437,   443,  1005,    25,   575,
    1008,   461,  1010,   443,   407,   408,   475,    88,   145,   449,
     443,   453,   937,   475,   868,   861,   103,   863,   142,   422,
     868,   145,    25,    70,    90,   895,    90,   137,    88,    90,
     514,    25,    88,   144,   555,    52,   476,    54,    55,    56,
      57,  1056,   563,   446,   139,   780,    56,   571,   572,    88,
    1058,   117,  1060,   456,   789,   540,   117,  1065,   137,  1067,
      90,  1047,   446,   144,   830,    90,   145,   830,   139,  1077,
     473,   513,   475,   558,   145,    63,  1001,   598,   125,   126,
     127,   128,   129,  1008,   144,   556,    25,   117,   144,   117,
     117,   100,   117,   564,    90,    52,   636,    54,    55,    56,
      57,   906,   650,    90,   566,   144,    91,   912,    90,   578,
     137,   514,   982,   140,   117,   124,   578,    90,   145,   588,
     137,   117,   110,   117,   112,   113,   588,   598,   613,   604,
     117,   137,    63,  1058,   137,   117,   598,   140,   568,   579,
     570,   591,   145,   137,   763,   139,   140,   574,   610,   589,
     137,   145,   636,   137,   730,   640,   139,   604,   698,   894,
     620,    87,    88,   134,   604,   568,    70,   570,    70,   631,
     746,   604,    90,   783,   624,   578,   139,   700,   117,   110,
     137,   112,   113,   706,   707,   588,   137,   590,   591,   710,
     713,   714,   634,    63,   145,   139,   717,    67,   137,   117,
     751,   140,   937,   140,    58,   667,   145,   669,    87,    88,
     136,   738,   117,   740,   698,   739,    70,   141,   144,   137,
      70,   624,  1014,   771,   772,   127,   128,   129,   631,    59,
      60,    61,    62,   636,   703,    85,    86,    54,   749,   135,
     110,   703,   112,  1015,    56,    99,   100,   631,    65,    66,
     735,    26,   137,    75,    76,   717,    70,   136,   720,   721,
     745,   723,    97,   725,   704,   144,  1001,    90,   137,   845,
     124,   847,    17,  1008,   124,   125,   126,   127,   128,   129,
     142,   857,    56,    25,   894,   121,   137,   140,   738,   137,
     740,   135,   777,   720,   117,   698,   699,   700,   120,   121,
     703,   763,    90,   706,   707,   747,   134,   830,   137,   749,
     713,   714,    87,    88,   137,   699,    63,   844,    87,    88,
      16,    17,   764,  1058,   872,    44,   874,    87,    88,   117,
     854,   137,   856,   775,   819,   738,   137,   740,   741,   915,
     916,   137,   137,    63,   920,   807,   117,   809,   924,   137,
      87,    88,    48,    49,   139,    10,    90,    53,    54,   134,
       8,   136,    13,   110,   139,   112,   113,   136,   115,   144,
      66,    67,    26,   135,   117,   144,   136,   839,   137,   139,
     842,   929,   930,   117,   144,   783,   137,   407,   408,   787,
     110,  1001,   112,   113,   844,  1005,    52,   137,  1008,   136,
    1010,   925,    98,   137,   424,   425,   137,   144,    52,   894,
      26,   841,   137,    87,    88,   991,    63,    52,   848,   849,
     143,   905,   852,   885,    10,   143,   135,   830,   965,   871,
     450,   137,    10,    87,    88,   983,   119,   985,   841,    63,
     141,   844,  1018,  1019,    15,   848,   849,   139,  1058,   852,
    1060,   137,   855,   135,   135,  1065,   918,  1067,   137,   862,
     137,    91,   136,   110,   949,   112,   113,  1077,   898,   953,
     144,    87,    88,  1021,     9,   137,   140,    87,    88,   909,
     134,   923,   136,   142,   137,   139,   110,   137,   112,   113,
     144,   137,   895,   955,   956,   898,   894,   137,   137,    87,
      88,   137,   905,   988,   934,   935,   909,   122,    87,    88,
     206,   117,   137,   209,   210,   211,  1000,   213,   134,   137,
     136,   135,    56,   139,    87,    88,   136,     2,   144,     4,
     137,   934,   935,   229,   144,   231,   232,   137,   137,   969,
     137,   971,   122,    87,    88,  1014,    56,   977,   136,   111,
     953,    94,  1014,   137,  1016,  1017,   144,   136,   139,    87,
      88,   137,  1047,   137,   137,   144,   969,   997,   971,    87,
      88,   139,   137,   136,   977,  1015,    51,    96,  1040,   982,
      55,   144,    52,   456,    54,    55,    56,    57,    85,   717,
     771,   809,   136,  1017,   997,   894,   789,  1000,   752,   792,
     144,   692,  1022,  1001,  1016,   506,    81,  1005,   136,   103,
    1008,  1014,  1010,   490,    97,   926,   144,   783,   136,    94,
      95,    96,    92,    87,    88,   321,   144,    87,    88,  1005,
     326,   327,   328,   329,   330,   331,   780,  1001,   334,   335,
     336,   337,   338,   339,   340,   341,   342,    -1,    70,   345,
     346,   347,   348,   349,   350,   351,   352,   353,   354,    -1,
    1058,   357,  1060,    85,    86,    -1,    -1,  1065,    -1,  1067,
      54,    55,   136,    57,    -1,    -1,   136,    -1,    -1,  1077,
     144,    65,    66,    -1,   144,    -1,   706,   707,    40,    41,
      42,    43,    44,   713,   714,    52,    -1,    54,    55,    56,
      57,   894,    -1,   125,   126,   127,   128,   129,    -1,    -1,
      -1,   407,   408,    52,    -1,    54,    55,    56,    57,    58,
     416,   417,   418,    -1,    -1,    -1,   422,    -1,   424,   425,
     426,    70,    -1,   208,    -1,    -1,    -1,    -1,   758,   759,
      -1,   761,   762,    -1,   937,    -1,   939,   958,   959,   445,
      -1,   944,    -1,    92,   450,    -1,    -1,    -1,    -1,    98,
      99,   100,    -1,    -1,    -1,   461,    -1,   242,   464,    -1,
      -1,    -1,    52,    -1,    54,    55,    56,    57,   253,   475,
     255,    -1,    -1,    -1,    -1,   124,    -1,    54,   127,   264,
      -1,    -1,    -1,    -1,    -1,    52,   492,    54,    55,    56,
      57,   276,    -1,    -1,    -1,    -1,   145,    -1,    -1,    -1,
     830,   507,    92,    -1,    -1,  1008,    -1,  1010,    98,    -1,
       2,    -1,     4,    -1,  1035,  1036,    -1,    -1,    -1,    -1,
    1041,   306,  1043,  1044,  1027,    92,   311,    -1,    -1,    -1,
      -1,    98,    -1,    -1,    -1,    -1,    -1,   867,   323,   324,
      -1,   326,    -1,    -1,    52,    -1,    54,    55,    56,    57,
      58,  1072,  1073,  1074,  1075,  1058,    -1,  1060,    -1,    51,
    1081,    -1,    70,    55,  1067,    -1,    -1,    -1,   574,    -1,
      -1,    -1,   578,    -1,  1077,   360,    -1,    -1,    -1,    -1,
      -1,    -1,   588,    -1,    92,    -1,    -1,    -1,    -1,    81,
      98,    99,   100,    -1,    -1,    -1,    -1,    -1,    -1,   605,
      -1,   607,    94,    95,    96,    97,    52,    -1,    54,    55,
      56,    57,    58,    -1,   620,   621,   124,    -1,    -1,   127,
      -1,    -1,   407,   408,    70,    -1,    -1,    -1,    -1,   206,
      -1,   139,   209,   210,   211,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     2,    -1,     4,    52,    92,    54,    55,    56,
      57,    58,    98,    99,   100,    -1,    -1,    -1,    -1,   665,
      -1,   446,    -1,    70,    -1,    -1,    -1,    70,    -1,    -1,
      -1,   456,    -1,    -1,    -1,    -1,    -1,    -1,   124,    -1,
      -1,   127,    85,    86,    -1,    92,    -1,    -1,   473,   695,
      -1,    51,    99,   100,   700,   701,    -1,   703,    -1,    -1,
     706,   707,    -1,    -1,    -1,    -1,    -1,   713,   714,    -1,
      -1,    -1,    -1,    -1,   720,    -1,   208,   124,    -1,   122,
     123,   124,   125,   126,   127,   128,   129,    -1,    -1,   514,
      -1,    -1,    -1,     2,    94,     4,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   321,    -1,    -1,    -1,    -1,   326,
     242,    -1,   758,   759,    -1,   761,   762,    -1,    -1,    -1,
      -1,   253,    -1,   255,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   264,   779,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    51,   568,   276,   570,    55,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   590,   812,    -1,    -1,    -1,
      -1,    -1,    81,    -1,   306,    -1,   822,    -1,    -1,   311,
      -1,    -1,    -1,    -1,   830,    94,    95,    96,    97,    -1,
      -1,   323,   324,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     417,   418,    -1,    -1,    -1,   851,   631,    -1,   208,   426,
      -1,   636,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   867,   868,    -1,    -1,    -1,    -1,    -1,   360,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   242,    -1,   461,    -1,    -1,   464,    -1,    -1,
      -1,    -1,   677,   253,    -1,   255,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   264,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   698,   699,   700,    -1,    -1,    -1,    -1,
      -1,   706,   707,    -1,    -1,    -1,    -1,    -1,   713,   714,
     507,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   208,
      -1,    -1,    -1,    -1,    -1,    -1,   306,    -1,    -1,    -1,
      -1,   311,    -1,    -1,   446,    -1,   741,    -1,    -1,    -1,
      -1,    -1,    -1,   323,   456,    -1,   326,    -1,    -1,    -1,
      -1,    -1,    -1,   242,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   473,    -1,    -1,   253,    -1,   255,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   264,    -1,   574,    -1,    -1,
     360,    -1,    -1,    -1,    -1,    -1,    -1,   276,  1014,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   514,    -1,    -1,    -1,    -1,    -1,   605,    -1,
     607,    -1,    -1,    -1,    -1,    -1,    -1,   306,    -1,    -1,
      -1,    -1,   311,   620,   621,   830,    -1,   407,   408,    -1,
      -1,    -1,    -1,    -1,   323,   324,   841,    -1,    -1,    -1,
      -1,    -1,    -1,   848,   849,    -1,    -1,   852,    -1,    -1,
     855,    -1,    -1,    -1,    -1,    -1,   568,   862,   570,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   446,    -1,   665,    -1,
      -1,   360,    -1,    -1,    -1,    -1,   456,    -1,   590,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     895,    -1,    -1,   898,    -1,    -1,    -1,    -1,   695,    -1,
     905,    -1,    -1,    -1,   909,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   631,
      -1,    -1,    -1,   720,   636,    -1,    -1,    -1,    -1,   934,
     935,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   953,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   446,    -1,    -1,
      -1,    -1,    -1,    -1,   969,    -1,   971,   456,    -1,    -1,
      -1,    -1,   977,    -1,     2,    -1,     4,   982,    -1,    -1,
      -1,    -1,   779,    -1,   473,    -1,   698,   699,   568,    -1,
     570,    -1,   997,    -1,    -1,  1000,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    44,    -1,    -1,    -1,
     590,    -1,    -1,    -1,    -1,   812,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    51,    -1,   514,    -1,    -1,    -1,   741,
      -1,    44,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    -1,    -1,    85,    86,    -1,
      -1,   631,    -1,    -1,   851,    -1,    -1,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      -1,   868,    85,    86,    -1,    -1,    -1,    -1,    -1,   568,
     118,   570,   120,   121,   122,   123,   124,   125,   126,   127,
     128,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,
      -1,   590,    -1,    -1,    -1,   118,    -1,   120,   121,   122,
     123,   124,   125,   126,   127,   128,   129,    -1,    -1,   699,
     700,    -1,    -1,    -1,    -1,    -1,   706,   707,    -1,   841,
      -1,    -1,    -1,   713,   714,    -1,   848,   849,    -1,    -1,
     852,    -1,   631,   855,    -1,    -1,    -1,   636,    -1,    -1,
     862,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   741,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     208,    -1,    -1,   895,    -1,    -1,   898,    -1,    -1,    -1,
      -1,    -1,    -1,   905,    -1,    -1,    -1,   909,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   698,
     699,    -1,    -1,    -1,   242,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   934,   935,    -1,   253,    -1,   255,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   264,    -1,    -1,    -1,
      -1,   953,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     830,    -1,   741,    -1,    -1,    -1,    -1,   969,    -1,   971,
      -1,   841,    -1,    -1,    -1,   977,    -1,    -1,   848,   849,
     982,    -1,   852,    -1,    -1,   855,    -1,    -1,   306,    -1,
      -1,    -1,   862,   311,    -1,   997,    -1,    -1,  1000,    -1,
      -1,    -1,    -1,    -1,    -1,   323,    -1,    -1,   326,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   895,    -1,    -1,   898,    -1,
      -1,    -1,    -1,    -1,    -1,   905,    -1,    -1,    -1,   909,
      -1,    -1,   360,    -1,    -1,    -1,    -1,    -1,    48,    49,
      -1,    -1,    -1,    53,    54,    55,    -1,    -1,    -1,    -1,
      -1,    -1,   841,    -1,   934,   935,    -1,    67,    -1,   848,
     849,    -1,    -1,   852,    -1,    -1,   855,    -1,    -1,    -1,
      -1,    81,    -1,   862,    -1,    -1,    -1,    -1,    -1,   407,
     408,    -1,    -1,    -1,    94,    95,    96,    -1,    -1,   969,
      -1,   971,    -1,    -1,    -1,    -1,    -1,   977,    -1,    -1,
      -1,    -1,   982,    -1,    -1,    -1,   895,    -1,    -1,   898,
      -1,    -1,    -1,    -1,    -1,    -1,   905,   997,   446,    -1,
     909,    -1,    -1,    -1,    -1,     0,    -1,    -1,   456,    -1,
      -1,    -1,    -1,     8,     9,    10,    -1,    -1,    13,    14,
      15,    -1,    17,    -1,    -1,   934,   935,    -1,    -1,    -1,
      -1,    26,    27,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    37,    38,   953,    40,    41,    42,    43,    44,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     969,    -1,   971,    -1,    -1,    -1,    -1,    -1,   977,    -1,
      -1,    -1,    -1,   982,    -1,    -1,   206,    -1,    -1,   209,
     210,   211,    -1,   213,    -1,    -1,    -1,    -1,   997,    -1,
      -1,  1000,    87,    88,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,   111,    -1,    85,    86,
     568,    -1,   570,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,
     135,    -1,   590,    -1,   139,   140,   276,   142,    -1,   144,
     145,   118,    -1,   120,   121,   122,   123,   124,   125,   126,
     127,   128,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   145,    -1,
      -1,    -1,    -1,   631,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   324,    -1,    -1,   327,   328,   329,
     330,   331,    -1,    -1,   334,   335,   336,   337,   338,   339,
     340,   341,   342,    -1,    -1,   345,   346,   347,   348,   349,
     350,   351,   352,   353,   354,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    -1,    -1,
      85,    86,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   699,   700,    -1,    -1,    -1,    -1,    -1,   706,   707,
      -1,    -1,    -1,    -1,    -1,   713,   714,    -1,    -1,    -1,
      -1,    -1,    -1,   118,    -1,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,    -1,   416,   417,   418,    -1,
      -1,    -1,    -1,   741,   424,   425,   426,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   445,    -1,    -1,    48,    49,
     450,    -1,    -1,    53,    54,    55,    -1,    -1,    -1,    -1,
      -1,   461,    -1,    -1,   464,    -1,    -1,    67,    -1,    -1,
      -1,    -1,    -1,   473,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    81,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   492,    -1,    94,    95,    96,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   507,    -1,    -1,
      -1,    -1,   830,    -1,   514,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   841,    -1,    -1,    -1,    -1,    -1,    -1,
     848,   849,    -1,    -1,   852,    -1,    -1,   855,    -1,    -1,
      -1,    -1,    -1,    -1,   862,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    -1,    -1,
      85,    86,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   574,    -1,    -1,   895,    -1,    -1,
     898,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   909,    -1,    -1,    -1,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   605,   206,   607,    -1,   209,
     210,   211,    -1,   213,    -1,    -1,   934,   935,    -1,    -1,
     620,   621,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   636,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   969,    -1,   971,    -1,    -1,    -1,    -1,    -1,   977,
      -1,    -1,    -1,    -1,   982,   665,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   276,    -1,    -1,   997,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   695,    -1,    -1,   698,    -1,
      -1,   701,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     720,    -1,    -1,    -1,   324,    -1,    -1,   327,   328,   329,
     330,   331,    -1,    -1,   334,   335,   336,   337,   338,   339,
     340,   341,   342,    -1,    -1,   345,   346,   347,   348,   349,
     350,   351,   352,   353,   354,    -1,    -1,    -1,   758,   759,
      -1,   761,   762,    -1,    -1,    -1,    -1,    52,    53,    -1,
      -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   779,
      -1,    -1,    -1,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    -1,    -1,    79,    80,    -1,    -1,    83,    84,
      85,    86,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   812,    98,    99,    -1,   416,   417,   418,    -1,
      -1,    -1,   822,    -1,   424,   425,   426,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   445,   131,   132,    -1,    -1,
     450,   851,    -1,   138,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   461,    52,    53,   464,    -1,    56,   867,   868,    -1,
      -1,    -1,    -1,   473,    -1,    -1,    -1,    -1,    68,    69,
      70,    71,    72,    73,    74,    75,    76,    -1,    -1,    79,
      80,    -1,   492,    83,    84,    85,    86,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   905,    -1,   507,    98,    99,
      -1,    -1,    -1,    -1,   514,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     120,   121,   122,   123,   124,   125,   126,   127,   128,   129,
      -1,   131,   132,    -1,    -1,    -1,    -1,    -1,   138,    -1,
      -1,    -1,    -1,   953,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    71,    72,    73,    74,    75,    76,    77,    -1,    79,
      80,    -1,    -1,    -1,   574,    85,    86,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1000,    -1,    -1,    -1,    -1,   605,    -1,   607,    -1,    -1,
     120,   121,   122,   123,   124,   125,   126,   127,   128,   129,
     620,   621,    -1,     0,     1,    -1,     3,     4,     5,     6,
       7,    -1,    -1,    -1,    11,    12,   636,    -1,    -1,    16,
      -1,    18,    19,    20,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,   665,    -1,    -1,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    -1,    59,    60,    61,    62,    -1,    64,    65,    66,
      -1,    68,    69,    -1,    -1,   695,    -1,    -1,   698,    -1,
      -1,   701,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,
     720,    98,    -1,    -1,   101,   102,   103,   104,   105,   106,
     107,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   130,   131,   132,    -1,    -1,   758,   759,
      -1,   761,   762,    -1,    -1,   142,    -1,     0,   145,    -1,
      -1,    -1,    -1,    -1,    -1,     8,     9,    10,    -1,   779,
      13,    14,    15,    -1,    17,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    26,    27,    28,    29,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,    42,
      43,    44,   812,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   822,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      -1,   851,    85,    86,    87,    88,    -1,    90,    91,    -1,
      -1,    -1,    -1,    -1,    97,    -1,    -1,   867,   868,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   111,    -1,
      -1,   114,    -1,    -1,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,   128,   129,    -1,    -1,    -1,
      -1,   134,   135,   136,   137,   905,     0,   140,   141,   142,
      -1,   144,   145,    -1,     8,     9,    10,    -1,    -1,    13,
      14,    15,    -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    25,    -1,    27,    28,    29,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    37,    38,    -1,    40,    41,    42,    43,
      44,    -1,    -1,   953,    70,    71,    72,    73,    74,    75,
      76,    -1,    -1,    79,    80,    -1,    -1,    -1,    -1,    85,
      86,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    -1,
      -1,    85,    86,    87,    88,    -1,    90,    91,    -1,    -1,
    1000,    -1,    -1,    97,   120,   121,   122,   123,   124,   125,
     126,   127,   128,   129,    -1,    -1,    -1,   111,    -1,    -1,
     114,    -1,    -1,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,   128,   129,    -1,    -1,    -1,    -1,
      -1,   135,   136,   137,     0,    -1,   140,   141,   142,    -1,
     144,   145,     8,     9,    10,    -1,    -1,    13,    14,    15,
      -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,
      -1,    27,    28,    29,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    37,    38,    -1,    40,    41,    42,    43,    44,    -1,
      -1,    -1,    70,    71,    72,    73,    74,    75,    76,    -1,
      -1,    79,    80,    -1,    -1,    -1,    -1,    85,    86,    -1,
      -1,    -1,    -1,    -1,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    -1,    -1,    85,
      86,    87,    88,    -1,    90,    91,    -1,    -1,    -1,    -1,
      -1,    97,   120,   121,   122,   123,   124,   125,   126,   127,
     128,   129,    -1,    -1,    -1,   111,    -1,    -1,   114,    -1,
      -1,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,   128,   129,    -1,    -1,    -1,    -1,    -1,   135,
     136,   137,     0,    -1,   140,   141,   142,    -1,   144,   145,
       8,     9,    10,    -1,    -1,    13,    14,    15,    -1,    17,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    26,    27,
      28,    29,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,
      38,    -1,    40,    41,    42,    43,    44,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    -1,    -1,    85,    86,    87,
      88,    -1,    -1,    91,    -1,    -1,    -1,    -1,    -1,    97,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   111,    -1,    -1,   114,    -1,    -1,    -1,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
     128,   129,    -1,    -1,    -1,    -1,   134,   135,   136,   137,
       0,    -1,   140,   141,   142,    -1,   144,   145,     8,     9,
      10,    -1,    -1,    13,    14,    15,    -1,    17,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    26,    27,    28,    29,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,
      40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    -1,    -1,    85,    86,    87,    88,    -1,
      -1,    91,    -1,    -1,    -1,    -1,    -1,    97,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   111,    -1,    -1,   114,    -1,    -1,    -1,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   127,   128,   129,
      -1,    -1,    -1,    -1,   134,   135,   136,   137,     0,    -1,
     140,   141,   142,    -1,   144,   145,     8,     9,    10,    -1,
      -1,    13,    14,    15,    -1,    17,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    27,    28,    29,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    -1,    -1,    85,    86,    87,    88,    -1,    90,    91,
      -1,    -1,    -1,    -1,    -1,    97,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   111,
      -1,    -1,   114,    -1,    -1,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,   128,   129,    -1,    -1,
      -1,    -1,    -1,   135,   136,   137,     0,    -1,   140,   141,
     142,    -1,   144,   145,     8,     9,    10,    -1,    -1,    13,
      14,    15,    -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    26,    27,    28,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    37,    38,    -1,    40,    41,    42,    43,
      44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    -1,
      -1,    85,    86,    87,    88,    -1,    -1,    91,    -1,    -1,
      -1,    -1,    -1,    97,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   111,    -1,    -1,
      -1,    -1,    -1,    -1,   118,    -1,   120,   121,   122,   123,
     124,   125,   126,   127,   128,   129,    -1,    -1,    -1,    -1,
     134,   135,   136,   137,     0,   139,   140,   141,   142,    -1,
     144,   145,     8,     9,    10,    -1,    -1,    13,    14,    15,
      -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    27,    28,    29,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    37,    38,    -1,    40,    41,    42,    43,    44,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    -1,    -1,    85,
      86,    87,    88,    -1,    -1,    91,    -1,    -1,    -1,    -1,
      -1,    97,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   111,    -1,    -1,   114,    -1,
      -1,    -1,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,   128,   129,    -1,    -1,    -1,    -1,    -1,   135,
     136,   137,     0,    -1,   140,   141,   142,    -1,   144,   145,
       8,     9,    10,    -1,    -1,    13,    14,    15,    -1,    17,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    26,    27,
      28,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,
      38,    -1,    40,    41,    42,    43,    44,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    -1,    -1,    85,    86,    87,
      88,    -1,    -1,    91,    -1,    -1,    -1,    -1,    -1,    97,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,
     118,    -1,   120,   121,   122,   123,   124,   125,   126,   127,
     128,   129,    -1,    -1,    -1,    -1,   134,   135,   136,   137,
       0,   139,   140,   141,   142,    -1,   144,   145,     8,     9,
      10,    -1,    -1,    13,    14,    15,    -1,    17,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    27,    28,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,
      40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    -1,    -1,    85,    86,    87,    88,    -1,
      -1,    91,    -1,    -1,    -1,    -1,    -1,    97,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,   118,    -1,
     120,   121,   122,   123,   124,   125,   126,   127,   128,   129,
      -1,    -1,    -1,    -1,    -1,   135,   136,   137,     0,   139,
     140,   141,   142,    -1,   144,   145,     8,     9,    10,    -1,
      -1,    -1,    14,    15,    -1,    17,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    26,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    -1,    -1,    85,    86,    87,    88,    -1,    90,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   111,
      -1,    -1,    -1,    -1,    -1,   117,   118,    -1,   120,   121,
     122,   123,   124,   125,   126,   127,   128,   129,    -1,    -1,
      -1,    -1,   134,   135,   136,   137,     0,    -1,   140,    -1,
     142,    -1,   144,   145,     8,     9,    10,    -1,    -1,    -1,
      14,    15,    -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    26,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    37,    38,    -1,    40,    41,    42,    43,
      44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    -1,
      -1,    85,    86,    87,    88,    -1,    90,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   111,    -1,    -1,
      -1,    -1,    -1,   117,   118,    -1,   120,   121,   122,   123,
     124,   125,   126,   127,   128,   129,    -1,    -1,    -1,    -1,
     134,   135,   136,   137,     0,    -1,   140,    -1,   142,    -1,
     144,   145,     8,     9,    10,    -1,    -1,    -1,    14,    15,
      -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    37,    38,    -1,    40,    41,    42,    43,    44,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    -1,    -1,    85,
      86,    87,    88,    -1,    90,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   111,    -1,    -1,    -1,    -1,
      -1,   117,   118,    -1,   120,   121,   122,   123,   124,   125,
     126,   127,   128,   129,    -1,    -1,    -1,    -1,    -1,   135,
     136,   137,     0,    -1,   140,    -1,   142,    -1,   144,   145,
       8,     9,    10,    -1,    -1,    -1,    14,    15,    -1,    17,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,
      38,    -1,    40,    41,    42,    43,    44,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    -1,    -1,    85,    86,    87,
      88,    -1,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   111,    -1,    -1,    -1,    -1,    -1,   117,
     118,    -1,   120,   121,   122,   123,   124,   125,   126,   127,
     128,   129,    -1,    -1,    -1,    -1,    -1,   135,   136,   137,
      -1,    -1,   140,    -1,   142,    -1,   144,   145,     1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      -1,    -1,    15,    16,    -1,    18,    19,    20,    21,    22,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,     1,   145,     3,     4,     5,     6,     7,    -1,    -1,
      10,    11,    12,    -1,    14,    15,    16,    -1,    18,    19,
      20,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    -1,    59,
      60,    61,    62,    -1,    64,    65,    66,    -1,    68,    69,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    93,    -1,    95,    96,    -1,    98,    -1,
      -1,   101,   102,   103,   104,   105,   106,   107,   108,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     130,   131,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   142,    -1,     1,   145,     3,     4,     5,     6,
       7,    -1,    -1,    10,    11,    12,    -1,    -1,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    -1,    59,    60,    61,    62,    -1,    64,    65,    66,
      -1,    68,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,
      -1,    98,    -1,    -1,   101,   102,   103,   104,   105,   106,
     107,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   130,   131,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,     1,   145,     3,
       4,     5,     6,     7,    -1,    -1,    10,    11,    12,    -1,
      -1,    15,    16,    -1,    18,    19,    20,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    -1,    59,    60,    61,    62,    -1,
      64,    65,    66,    -1,    68,    69,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,
      -1,    95,    96,    -1,    98,    -1,    -1,   101,   102,   103,
     104,   105,   106,   107,   108,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     1,    -1,     3,     4,     5,     6,
       7,    -1,     9,    10,    11,    12,   130,   131,   132,    16,
      -1,    18,    19,    20,    21,    22,    23,    24,   142,    -1,
      -1,   145,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    -1,    59,    60,    61,    62,    -1,    64,    65,    66,
      -1,    68,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,
      -1,    98,    -1,    -1,   101,   102,   103,   104,   105,   106,
     107,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     1,    -1,     3,     4,     5,     6,     7,    -1,    -1,
      -1,    11,    12,   130,   131,   132,    16,    -1,    18,    19,
      20,    21,    22,    23,    24,   142,    -1,    -1,   145,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    -1,    59,
      60,    61,    62,    -1,    64,    65,    66,    -1,    68,    69,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    93,    -1,    95,    96,    -1,    98,    -1,
      -1,   101,   102,   103,   104,   105,   106,   107,   108,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     130,   131,   132,     1,    -1,     3,     4,     5,     6,     7,
     140,    -1,   142,    11,    12,   145,    -1,    -1,    16,    -1,
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
      -1,    -1,   130,   131,   132,     1,    -1,     3,     4,     5,
       6,     7,   140,    -1,   142,    11,    12,   145,    -1,    -1,
      16,    -1,    18,    19,    20,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    -1,    59,    60,    61,    62,    -1,    64,    65,
      66,    -1,    68,    69,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,
      96,    -1,    98,    -1,    -1,   101,   102,   103,   104,   105,
     106,   107,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   130,   131,   132,    -1,    -1,   135,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,     1,   145,
       3,     4,     5,     6,     7,    -1,    -1,    -1,    11,    12,
      -1,    -1,    -1,    16,    -1,    18,    19,    20,    21,    22,
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
      -1,    -1,   135,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,     1,   145,     3,     4,     5,     6,     7,    -1,    -1,
      10,    11,    12,    -1,    -1,    -1,    16,    -1,    18,    19,
      20,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    -1,    59,
      60,    61,    62,    -1,    64,    65,    66,    -1,    68,    69,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    93,    -1,    95,    96,    -1,    98,    -1,
      -1,   101,   102,   103,   104,   105,   106,   107,   108,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,
       3,     4,     5,     6,     7,    -1,    -1,    -1,    11,    12,
     130,   131,   132,    16,    -1,    18,    19,    20,    21,    22,
      23,    24,   142,    -1,    -1,   145,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    -1,    59,    60,    61,    62,
      -1,    64,    65,    66,    -1,    68,    69,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      93,    -1,    95,    96,    -1,    98,    -1,    -1,   101,   102,
     103,   104,   105,   106,   107,   108,   109,    -1,   111,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,
      -1,     7,    -1,    -1,    -1,    11,    12,   130,   131,   132,
      16,    -1,    18,    19,    20,    21,    22,    23,    24,   142,
      -1,    -1,   145,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      46,    -1,    -1,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    58,    59,    60,    61,    62,    -1,    64,    65,
      66,    -1,    68,    69,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,
      96,    -1,    98,    99,   100,   101,   102,   103,   104,   105,
     106,   107,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,    -1,     7,    -1,
      -1,    -1,    11,    12,   130,   131,   132,    16,    -1,    18,
      19,    20,    21,    22,    23,    24,    -1,    -1,    -1,   145,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    -1,
      59,    60,    61,    62,    -1,    64,    65,    66,    -1,    68,
      69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,    -1,
      -1,    -1,   101,   102,   103,   104,   105,   106,   107,   108,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,    11,
      12,   130,   131,   132,    16,    -1,    18,    19,    20,    21,
      22,    23,    24,   142,    -1,    -1,   145,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    -1,    59,    60,    61,
      62,    -1,    64,    65,    66,    -1,    68,    69,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,
      92,    93,    -1,    95,    96,    -1,    -1,    -1,    -1,   101,
     102,   103,   104,   105,   106,   107,   108,   109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,    -1,    -1,    -1,    11,    12,   130,   131,
     132,    16,    -1,    18,    19,    20,    21,    22,    23,    24,
      -1,    -1,    -1,   145,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    59,    60,    61,    62,    -1,    64,
      65,    66,    -1,    68,    69,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,
      95,    96,    -1,    98,    -1,    -1,   101,   102,   103,   104,
     105,   106,   107,   108,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,
      -1,    -1,    -1,    11,    12,   130,   131,   132,    16,    -1,
      18,    19,    20,    21,    22,    23,    24,   142,    -1,    -1,
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
      -1,    -1,    -1,    -1,   142,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      68,    69,    70,    71,    72,    73,    74,    75,    76,    -1,
      -1,    79,    80,    -1,    -1,    83,    84,    85,    86,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      98,    99,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   120,   121,   122,   123,   124,   125,   126,   127,
     128,   129,    -1,   131,   132,    -1,    -1,    -1,    -1,    -1,
     138,   139,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    -1,
      -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    -1,    -1,    79,    80,
      -1,    -1,    83,    84,    85,    86,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    99,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,    -1,
     131,   132,    -1,    -1,    -1,    -1,    -1,   138,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    -1,    -1,    -1,    -1,    -1,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    -1,    -1,    79,    80,    -1,    -1,    83,    84,
      85,    86,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    98,    99,    -1,    -1,   102,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,    -1,   131,   132,    -1,    -1,
      -1,    -1,    -1,   138,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    -1,    -1,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    -1,    -1,
      79,    80,    -1,    -1,    83,    84,    85,    86,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,
      99,    -1,    -1,   102,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   120,   121,   122,   123,   124,   125,   126,   127,   128,
     129,    -1,   131,   132,    -1,    -1,    -1,    -1,    -1,   138,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    -1,    -1,    -1,
      -1,    -1,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    -1,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    -1,    -1,    79,    80,    -1,    -1,
      83,    84,    85,    86,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    98,    99,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   120,   121,   122,
     123,   124,   125,   126,   127,   128,   129,    -1,   131,   132,
       3,     4,     5,    -1,     7,   138,    -1,    -1,    11,    12,
      -1,    -1,    -1,    16,    -1,    18,    19,    20,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    46,    -1,    -1,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    -1,    59,    60,    61,    62,
      -1,    64,    65,    66,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      93,    -1,    95,    96,    -1,    -1,    -1,    -1,   101,   102,
     103,   104,   105,   106,   107,   108,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,    -1,     7,    -1,    -1,   130,    11,    12,
      -1,    -1,    -1,    16,   137,    18,    19,    20,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    46,    -1,    -1,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    -1,    59,    60,    61,    62,
      -1,    64,    65,    66,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      93,    -1,    95,    96,    -1,    -1,    -1,    -1,   101,   102,
     103,   104,   105,   106,   107,   108,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,     6,     7,    -1,    -1,   130,    11,    12,
      -1,    -1,    -1,    16,   137,    18,    19,    20,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    -1,    59,    60,    61,    62,
      -1,    64,    65,    66,    -1,    68,    69,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      93,    -1,    95,    96,    -1,    98,    -1,    -1,   101,   102,
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
      96,    -1,    98,    99,   100,   101,   102,   103,   104,   105,
     106,   107,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,    -1,
      -1,    -1,    11,    12,   130,   131,   132,    16,    -1,    18,
      19,    20,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    45,    46,    -1,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    -1,
      59,    60,    61,    62,    -1,    64,    65,    66,    -1,    68,
      69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,    98,
      -1,    -1,   101,   102,   103,   104,   105,   106,   107,   108,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,    11,
      12,   130,   131,   132,    16,    -1,    18,    19,    20,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    58,    59,    60,    61,
      62,    -1,    64,    65,    66,    -1,    68,    69,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,
      92,    93,    -1,    95,    96,    -1,    98,    99,   100,   101,
     102,   103,   104,   105,   106,   107,   108,   109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,    -1,     7,    -1,    -1,    -1,    11,    12,   130,   131,
     132,    16,    -1,    18,    19,    20,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    46,    -1,    -1,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    -1,    64,
      65,    66,    -1,    68,    69,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,
      95,    96,    -1,    98,    99,    -1,   101,   102,   103,   104,
     105,   106,   107,   108,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,    -1,     7,
      -1,    -1,    -1,    11,    12,   130,   131,   132,    16,    -1,
      18,    19,    20,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,
      -1,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      58,    59,    60,    61,    62,    -1,    64,    65,    66,    -1,
      68,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,
      -1,    99,   100,   101,   102,   103,   104,   105,   106,   107,
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
      -1,    92,    93,    -1,    95,    96,    -1,    98,    99,    -1,
     101,   102,   103,   104,   105,   106,   107,   108,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,    -1,     7,    -1,    -1,    -1,    11,    12,   130,
     131,   132,    16,    -1,    18,    19,    20,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    46,    -1,    -1,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    58,    59,    60,    61,    62,    -1,
      64,    65,    66,    -1,    68,    69,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,
      -1,    95,    96,    -1,    -1,    99,    -1,   101,   102,   103,
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
      -1,    98,    -1,    -1,   101,   102,   103,   104,   105,   106,
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
      -1,    -1,    92,    93,    -1,    95,    96,    -1,    98,    -1,
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
      93,    -1,    95,    96,    -1,    98,    -1,    -1,   101,   102,
     103,   104,   105,   106,   107,   108,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,
      -1,     7,    -1,    -1,    -1,    11,    12,   130,   131,   132,
      16,    -1,    18,    19,    20,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      46,    -1,    -1,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    -1,    59,    60,    61,    62,    -1,    64,    65,
      66,    -1,    68,    69,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,
      96,    -1,    98,    -1,    -1,   101,   102,   103,   104,   105,
     106,   107,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,    -1,     7,    -1,
      -1,    -1,    11,    12,   130,   131,   132,    16,    -1,    18,
      19,    20,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    -1,
      59,    60,    61,    62,    -1,    64,    65,    66,    -1,    68,
      69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,    98,
      -1,    -1,   101,   102,   103,   104,   105,   106,   107,   108,
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
      92,    93,    -1,    95,    96,    -1,    -1,    -1,    -1,   101,
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
      95,    96,    -1,    -1,    -1,    -1,   101,   102,   103,   104,
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
      -1,    -1,    -1,   101,   102,   103,   104,   105,   106,   107,
     108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,
      11,    12,   130,   131,   132,    16,    -1,    18,    19,    20,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    -1,    59,    60,
      61,    62,    -1,    64,    65,    66,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      -1,    92,    93,    -1,    95,    96,    -1,    98,    -1,    -1,
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
      98,    -1,    -1,   101,   102,   103,   104,   105,   106,   107,
     108,   109,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,    -1,     7,    -1,    -1,    -1,    11,    12,    -1,    -1,
      -1,    16,   130,    18,    19,    20,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    46,    -1,    -1,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    59,    60,    61,    62,    -1,    64,
      65,    66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      85,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,
      95,    96,    -1,    -1,    -1,    -1,   101,   102,   103,   104,
     105,   106,   107,   108,   109,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,    -1,     7,    -1,    -1,    -1,    11,
      12,    -1,    -1,    -1,    16,   130,    18,    19,    20,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    46,    -1,    -1,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    -1,    59,    60,    61,
      62,    -1,    64,    65,    66,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,
      92,    93,    -1,    95,    96,    -1,    -1,    -1,    -1,   101,
     102,   103,   104,   105,   106,   107,   108,   109,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,    -1,     7,    -1,
      -1,    -1,    11,    12,    -1,    -1,    -1,    16,   130,    18,
      19,    20,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    -1,
      59,    60,    61,    62,    -1,    64,    65,    66,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,    -1,
      -1,    -1,   101,   102,   103,   104,   105,   106,   107,   108,
     109,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,
      -1,     7,    -1,    -1,    -1,    11,    12,    -1,    -1,    -1,
      16,   130,    18,    19,    20,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      46,    -1,    -1,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    -1,    59,    60,    61,    62,    -1,    64,    65,
      66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,
      96,    -1,    -1,    -1,    -1,   101,   102,   103,   104,   105,
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
      93,    -1,    95,    96,    -1,    -1,    -1,    -1,   101,   102,
     103,   104,   105,   106,   107,   108,   109,    -1,    -1,    -1,
      -1,    -1,    52,    53,    -1,    -1,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   130,    68,    69,
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
     128,   129,    -1,   131,   132,    -1,    -1,    -1,    -1,    -1,
     138
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   147,   148,     0,     1,     3,     4,     5,     6,     7,
      11,    12,    16,    18,    19,    20,    21,    22,    23,    24,
      30,    31,    32,    33,    34,    35,    36,    39,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    59,    60,    61,    62,    64,    65,    66,    68,    69,
      89,    92,    93,    95,    96,    98,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   130,   131,   132,   149,   150,
     151,   158,   160,   162,   164,   165,   168,   169,   170,   172,
     173,   174,   176,   177,   187,   190,   205,   224,   225,   226,
     227,   228,   229,   230,   231,   232,   233,   234,   236,   262,
     263,   283,   284,   285,   286,   287,   288,   289,   292,   294,
     295,   309,   311,   312,   313,   314,   315,   316,   317,   318,
     352,   365,   151,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    56,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    79,    80,    83,    84,
      85,    86,    98,    99,   120,   121,   122,   123,   124,   125,
     126,   127,   128,   129,   131,   132,   138,   180,   181,   182,
     183,   185,   186,   309,   311,    39,    58,    89,    92,    98,
      99,   100,   103,   131,   169,   177,   187,   191,   197,   200,
     202,   224,   314,   315,   317,   318,   350,   351,   197,   139,
     198,   199,   139,   194,   198,   139,   145,   359,    54,   182,
     359,   152,   134,    21,    22,    31,    32,   168,   187,   224,
     236,   187,    56,     1,    47,    92,   154,   155,   156,   158,
     171,   172,   365,   207,   208,   192,   202,   350,   365,   191,
     349,   350,   365,    46,    89,   130,   137,   176,   205,   224,
     314,   315,   318,   253,   254,    54,    55,    57,   180,   298,
     310,   298,   299,   300,   143,   143,   143,   143,   313,   164,
     187,   187,   142,   145,   358,   363,   364,    40,    41,    42,
      43,    44,    37,    38,    26,   134,   194,   198,   268,    28,
     260,   117,   137,    92,    98,   173,   117,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      85,    86,   118,   120,   121,   122,   123,   124,   125,   126,
     127,   128,   129,   189,   189,    87,    88,   136,   144,   356,
     206,   162,   163,   163,   211,   213,   163,   358,   364,    89,
     170,   177,   224,   241,   314,   315,   318,    52,    56,    85,
      89,   178,   179,   224,   314,   315,   318,   179,    33,    34,
      35,    36,    49,    50,    51,    52,    56,   139,   180,   316,
     347,   197,    88,   356,   357,   268,   286,    90,    90,   137,
     191,    56,   191,   191,   191,   298,   117,    91,   137,   201,
     365,    88,   136,   356,    90,    90,   137,   201,   197,   359,
     360,   197,   196,   197,   202,   350,   365,   162,   360,   162,
      54,    65,    66,   159,   139,   188,   134,   154,    88,   356,
      90,   158,   157,   171,   140,   358,   364,   360,   360,   158,
     141,   137,   145,   362,   137,   362,   135,   362,   359,    56,
     313,   173,   175,   137,    88,   136,   356,   255,    63,   110,
     112,   113,   301,   113,   113,   301,    67,   301,   290,   296,
     293,   297,    70,   142,   151,   163,   163,   163,   163,   158,
     162,   162,   270,   269,    97,   166,   261,    98,   164,   191,
     202,   203,   204,   171,   137,   176,   137,   160,   161,   164,
     177,   187,   191,   193,   204,   224,   318,   187,   187,   187,
     187,   187,   187,   187,   187,   187,   187,   187,   187,   187,
     187,   187,   187,   187,   187,   187,   187,   187,   187,   187,
     187,    52,    53,    56,   185,   267,   353,   354,   196,    52,
      53,    56,   185,   266,   353,   153,   154,    13,   237,   363,
     237,   163,   163,   358,    17,   277,    56,    88,   136,   356,
      25,   162,    52,    56,   178,   121,   319,    88,   136,   356,
     219,   348,   220,    88,   144,   355,    52,    56,   353,   161,
     187,   193,   161,   193,   184,   115,   191,    98,   191,   200,
     350,    52,    56,   196,    52,    56,   351,   360,   140,   360,
     137,   137,   360,   182,   210,   187,   149,   135,   353,   353,
     193,   134,   360,   156,   209,   350,   137,   175,    52,    56,
     196,    52,    56,    52,    54,    55,    56,    57,    58,    70,
      92,    98,    99,   100,   124,   127,   139,   258,   323,   325,
     326,   327,   328,   329,   330,   331,   332,   335,   336,   337,
     338,   341,   342,   343,   344,   345,   303,   302,   113,   291,
     301,    63,   113,   113,   291,    63,   113,   187,   274,   275,
     271,   272,   167,   274,   191,   137,   360,   175,   137,    44,
     117,    44,    88,   136,   356,   359,    90,    90,   194,   198,
     265,   359,   361,    90,    90,   194,   198,   264,    10,   235,
       8,   279,   365,   154,    13,   154,    27,   238,   363,   238,
     277,   202,   235,    52,    56,   196,    52,    56,   215,   218,
     320,   217,    52,    56,   178,   196,   153,   162,   221,   222,
     194,   195,   198,   365,   182,   191,   191,   201,    90,    90,
     361,    90,    90,   350,   162,   135,   149,   360,   362,   173,
     361,    92,    98,   242,   243,   244,   327,   325,   256,   117,
     137,   324,   191,   137,   346,   365,    52,   137,   346,   137,
     324,    52,   137,   324,    52,   304,    54,    55,    57,   308,
     318,   143,   301,   143,   143,   143,    10,   276,   135,   273,
     271,    10,    98,   191,   175,   158,   187,    52,    56,   196,
      52,    56,   119,   161,   193,   161,   193,   166,   194,   141,
      90,   161,   193,   161,   193,   166,   195,   191,   204,   280,
     365,    15,   240,   365,    14,   239,   240,   240,   212,   214,
     235,   137,   237,   361,   163,   363,   163,   153,   361,   235,
     360,   139,   321,   322,   180,   268,   260,    90,   137,   360,
     135,   244,   137,   327,   137,   360,   250,   359,   257,   191,
     323,   329,   341,   343,   332,   337,   345,   330,   338,   343,
     328,   330,   305,    78,   122,   248,   249,   365,   248,   135,
     191,   361,   187,   161,   193,    91,   281,   365,   154,     9,
     282,   365,   163,   235,   235,   154,   154,   191,   154,   238,
     153,   363,   235,   325,   153,   325,   223,   360,   243,   137,
      98,   242,   140,   142,    29,   114,   259,   137,   324,   137,
     324,   346,   137,   324,   137,   324,   324,   306,   245,   247,
     250,   328,   330,   331,   333,   334,   337,   339,   340,   343,
     345,   153,   154,    89,   177,   224,   314,   315,   318,   237,
     154,   237,   235,   235,   240,   277,   278,   216,   235,   360,
     235,   363,   321,   137,   243,   137,   327,    52,   251,   252,
     326,   154,   154,   330,   343,   330,   330,   307,   250,   122,
     117,   137,   246,    89,   224,   137,   346,   346,   137,   246,
     137,   246,    56,    88,   136,   356,   154,   154,   154,   153,
     243,   137,   137,   359,   235,   135,   324,   137,   324,   324,
     324,   154,   122,   224,   245,   340,   343,    56,    88,   333,
     337,   330,   339,   343,   330,    52,    56,   196,    52,    56,
     279,   239,   235,   235,   243,   252,   330,   111,   137,   246,
     137,   246,    52,    56,   346,   137,   246,   137,   246,   246,
     361,   324,   330,   343,   330,   330,   246,   137,   246,   246,
     246,   330,   246
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
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
     187,   187,   187,   187,   187,   187,   188,   187,   187,   187,
     189,   189,   189,   189,   190,   190,   191,   192,   192,   192,
     192,   193,   193,   194,   195,   195,   196,   196,   196,   196,
     196,   197,   197,   197,   197,   197,   199,   198,   200,   201,
     201,   202,   202,   202,   202,   203,   203,   204,   204,   204,
     205,   205,   205,   205,   205,   205,   205,   205,   205,   205,
     205,   206,   205,   207,   205,   208,   209,   205,   205,   205,
     205,   205,   205,   205,   205,   205,   205,   210,   205,   205,
     205,   205,   205,   205,   205,   205,   205,   211,   212,   205,
     213,   214,   205,   205,   205,   215,   216,   205,   217,   205,
     218,   205,   219,   205,   220,   221,   205,   222,   223,   205,
     205,   205,   205,   205,   224,   225,   226,   227,   228,   229,
     230,   231,   232,   233,   234,   235,   236,   237,   237,   237,
     238,   238,   239,   239,   240,   240,   241,   241,   242,   242,
     243,   243,   244,   244,   244,   244,   244,   244,   244,   244,
     244,   245,   245,   245,   245,   246,   246,   247,   247,   247,
     247,   247,   247,   247,   247,   247,   247,   247,   247,   247,
     247,   247,   248,   248,   249,   249,   249,   250,   250,   251,
     251,   252,   252,   254,   255,   256,   257,   253,   258,   258,
     259,   259,   261,   260,   262,   262,   262,   262,   263,   264,
     263,   265,   263,   263,   266,   263,   267,   263,   263,   263,
     263,   269,   268,   270,   268,   272,   273,   271,   275,   276,
     274,   277,   278,   278,   279,   279,   280,   280,   280,   281,
     281,   282,   282,   283,   283,   283,   284,   285,   285,   285,
     286,   287,   288,   289,   290,   290,   291,   291,   292,   293,
     293,   294,   295,   296,   296,   297,   297,   298,   298,   299,
     299,   300,   300,   301,   302,   301,   303,   304,   305,   306,
     307,   301,   308,   308,   308,   308,   309,   310,   310,   310,
     310,   311,   312,   312,   313,   313,   313,   313,   314,   314,
     314,   314,   314,   315,   315,   315,   315,   315,   315,   315,
     316,   316,   317,   317,   318,   318,   320,   319,   319,   321,
     322,   321,   323,   323,   323,   323,   324,   324,   325,   325,
     325,   325,   325,   325,   325,   325,   325,   325,   325,   325,
     325,   325,   325,   326,   326,   326,   326,   327,   327,   328,
     329,   329,   330,   330,   331,   332,   332,   333,   333,   334,
     334,   335,   335,   336,   336,   337,   337,   338,   339,   340,
     340,   341,   341,   342,   342,   343,   343,   344,   344,   345,
     346,   346,   347,   348,   347,   349,   349,   350,   350,   351,
     351,   351,   351,   352,   352,   352,   353,   353,   353,   353,
     354,   354,   354,   355,   355,   356,   356,   357,   357,   358,
     358,   359,   359,   360,   361,   362,   362,   362,   363,   363,
     364,   364,   365
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
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
       3,     3,     3,     3,     1,     3,     3,     3,     3,     3,
       2,     2,     3,     3,     3,     3,     0,     4,     6,     1,
       1,     1,     1,     1,     3,     3,     1,     1,     2,     4,
       2,     1,     3,     3,     1,     1,     1,     1,     2,     4,
       2,     1,     2,     2,     4,     1,     0,     2,     2,     2,
       1,     1,     2,     3,     4,     1,     1,     3,     4,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     0,     4,     0,     3,     0,     0,     5,     3,     3,
       2,     3,     3,     1,     4,     3,     1,     0,     6,     4,
       3,     2,     1,     2,     2,     6,     6,     0,     0,     7,
       0,     0,     7,     5,     4,     0,     0,     9,     0,     6,
       0,     7,     0,     5,     0,     0,     7,     0,     0,     9,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     2,
       1,     1,     1,     5,     1,     2,     1,     1,     1,     3,
       1,     3,     1,     4,     6,     3,     5,     2,     4,     1,
       3,     4,     2,     2,     1,     2,     0,     6,     8,     4,
       6,     4,     2,     6,     2,     4,     6,     2,     4,     2,
       4,     1,     1,     1,     3,     1,     4,     1,     4,     1,
       3,     1,     1,     0,     0,     0,     0,     6,     4,     1,
       3,     3,     0,     4,     2,     4,     5,     5,     2,     0,
       5,     0,     5,     3,     0,     4,     0,     4,     2,     1,
       4,     0,     4,     0,     4,     0,     0,     4,     0,     0,
       4,     5,     1,     1,     6,     1,     1,     1,     1,     2,
       1,     2,     1,     1,     1,     1,     1,     1,     1,     2,
       3,     3,     3,     4,     0,     3,     1,     2,     4,     0,
       3,     4,     4,     0,     3,     0,     3,     0,     2,     0,
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
        yyerror (&yylloc, parser, YY_("syntax error: cannot back up")); \
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
      res += YYFPRINTF (parser, "%d", yylocp->first_line);
      if (0 <= yylocp->first_column)
        res += YYFPRINTF (parser, ".%d", yylocp->first_column);
    }
  if (0 <= yylocp->last_line)
    {
      if (yylocp->first_line < yylocp->last_line)
        {
          res += YYFPRINTF (parser, "-%d", yylocp->last_line);
          if (0 <= end_col)
            res += YYFPRINTF (parser, ".%d", end_col);
        }
      else if (0 <= end_col && yylocp->first_column < end_col)
        res += YYFPRINTF (parser, "-%d", end_col);
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
      YYFPRINTF (parser, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value, Location, parser); \
      YYFPRINTF (parser, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, struct parser_params *parser)
{
  FILE *yyoutput = yyo;
  YYUSE (yyoutput);
  YYUSE (yylocationp);
  YYUSE (parser);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyo, yytoknum[yytype], *yyvaluep);
# endif
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, struct parser_params *parser)
{
  YYFPRINTF (parser, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  YY_LOCATION_PRINT (yyo, *yylocationp);
  YYFPRINTF (parser, ": ");
  yy_symbol_value_print (yyo, yytype, yyvaluep, yylocationp, parser);
  YYFPRINTF (parser, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop, struct parser_params *parser)
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
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, YYLTYPE *yylsp, int yyrule, struct parser_params *parser)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (parser, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (parser, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[+yyssp[yyi + 1 - yynrhs]],
                       &yyvsp[(yyi + 1) - (yynrhs)]
                       , &(yylsp[(yyi + 1) - (yynrhs)])                       , parser);
      YYFPRINTF (parser, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, yylsp, Rule, parser); \
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
#   define yystrlen(S) (YY_CAST (YYPTRDIFF_T, strlen (S)))
#  else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T
yystrlen (const char *yystr)
{
  YYPTRDIFF_T yylen;
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
static YYPTRDIFF_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYPTRDIFF_T yyn = 0;
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

  if (yyres)
    return yystpcpy (yyres, yystr) - yyres;
  else
    return yystrlen (yystr);
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
yysyntax_error (YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                yy_state_t *yyssp, int yytoken)
{
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Actual size of YYARG. */
  int yycount = 0;
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

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
      int yyn = yypact[+*yyssp];
      YYPTRDIFF_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
      yysize = yysize0;
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
                  YYPTRDIFF_T yysize1
                    = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
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
    /* Don't count the "%s"s in the final size, but reserve room for
       the terminator.  */
    YYPTRDIFF_T yysize1 = yysize + (yystrlen (yyformat) - 2 * yycount) + 1;
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
          ++yyp;
          ++yyformat;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep, YYLTYPE *yylocationp, struct parser_params *parser)
{
  YYUSE (yyvaluep);
  YYUSE (yylocationp);
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

/* Location data for the lookahead symbol.  */
static YYLTYPE yyloc_default
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
  = { 1, 1, 1, 1 }
# endif
;
YYLTYPE yylloc = yyloc_default;

    /* Number of syntax errors so far.  */
    int yynerrs;

    yy_state_fast_t yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.
       'yyls': related to locations.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss;
    yy_state_t *yyssp;

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

    YYPTRDIFF_T yystacksize;

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
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yylsp = yyls = yylsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((parser, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
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
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((parser, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    goto yyexhaustedlab;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;
        YYLTYPE *yyls1 = yyls;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yyls1, yysize * YYSIZEOF (*yylsp),
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
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
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

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((parser, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */

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
      yychar = yylex (&yylval, &yylloc, parser);
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
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END
  *++yylsp = yylloc;

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
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
#line 1164 "ripper.y"
                   {
			SET_LEX_STATE(EXPR_BEG);
#if 0
			local_push(compile_for_eval || in_main);
#endif
			local_push(0);

		    }
#line 5582 "ripper.c"
    break;

  case 3:
#line 1173 "ripper.y"
                    {
#if 0
			if ((yyvsp[0].val) && !compile_for_eval) {
			    /* last expression should not be void */
			    if (nd_type((yyvsp[0].val)) != NODE_BLOCK) void_expr((yyvsp[0].val));
			    else {
				NODE *node = (yyvsp[0].val);
				while (node->nd_next) {
				    node = node->nd_next;
				}
				void_expr(node->nd_head);
			    }
			}
			ruby_eval_tree = new_scope(0, block_append(ruby_eval_tree, (yyvsp[0].val), &(yyloc)), &(yyloc));
#endif
			(yyval.val) = (yyvsp[0].val);
			parser->result = dispatch1(program, (yyval.val));

			local_pop();
		    }
#line 5607 "ripper.c"
    break;

  case 4:
#line 1196 "ripper.y"
                    {
#if 0
			void_stmts((yyvsp[-1].val));
#endif

			(yyval.val) = (yyvsp[-1].val);
		    }
#line 5619 "ripper.c"
    break;

  case 5:
#line 1206 "ripper.y"
                    {
#if 0
			(yyval.val) = new_begin(0, &(yyloc));
#endif
			(yyval.val) = dispatch2(stmts_add, dispatch0(stmts_new),
						  dispatch0(void_stmt));

		    }
#line 5632 "ripper.c"
    break;

  case 6:
#line 1215 "ripper.y"
                    {
#if 0
			(yyval.val) = newline_node((yyvsp[0].val));
#endif
			(yyval.val) = dispatch2(stmts_add, dispatch0(stmts_new), (yyvsp[0].val));

		    }
#line 5644 "ripper.c"
    break;

  case 7:
#line 1223 "ripper.y"
                    {
#if 0
			(yyval.val) = block_append((yyvsp[-2].val), newline_node((yyvsp[0].val)), &(yyloc));
#endif
			(yyval.val) = dispatch2(stmts_add, (yyvsp[-2].val), (yyvsp[0].val));

		    }
#line 5656 "ripper.c"
    break;

  case 8:
#line 1231 "ripper.y"
                    {
			(yyval.val) = remove_begin((yyvsp[0].val));
		    }
#line 5664 "ripper.c"
    break;

  case 10:
#line 1238 "ripper.y"
                    {
#if 0
			/* local_push(0); */
#endif

		    }
#line 5675 "ripper.c"
    break;

  case 11:
#line 1245 "ripper.y"
                    {
#if 0
			ruby_eval_tree_begin = block_append(ruby_eval_tree_begin,
							    new_begin((yyvsp[-1].val), &(yyloc)), &(yyloc));
			/* NEW_PREEXE($4)); */
			/* local_pop(); */
			(yyval.val) = new_begin(0, &(yyloc));
#endif
			(yyval.val) = dispatch1(BEGIN, (yyvsp[-1].val));

		    }
#line 5691 "ripper.c"
    break;

  case 12:
#line 1262 "ripper.y"
                    {
#if 0
			(yyval.val) = (yyvsp[-3].val);
			if ((yyvsp[-2].val)) {
			    (yyval.val) = new_rescue((yyvsp[-3].val), (yyvsp[-2].val), (yyvsp[-1].val), &(yyloc));
			}
			else if ((yyvsp[-1].val)) {
			    rb_warn0("else without rescue is useless");
			    (yyval.val) = block_append((yyval.val), (yyvsp[-1].val), &(yyloc));
			}
			if ((yyvsp[0].val)) {
			    if ((yyval.val)) {
				(yyval.val) = NEW_ENSURE((yyval.val), (yyvsp[0].val));
				(yyval.val)->nd_loc = (yyloc);
			    }
			    else {
				NODE *nil = NEW_NIL();
				nil->nd_loc = (yyloc);
				(yyval.val) = block_append((yyvsp[0].val), nil, &(yyloc));
			    }
			}
			fixpos((yyval.val), (yyvsp[-3].val));
#endif
			(yyval.val) = dispatch4(bodystmt,
				       escape_Qundef((yyvsp[-3].val)),
				       escape_Qundef((yyvsp[-2].val)),
				       escape_Qundef((yyvsp[-1].val)),
				       escape_Qundef((yyvsp[0].val)));

		    }
#line 5726 "ripper.c"
    break;

  case 13:
#line 1295 "ripper.y"
                    {
#if 0
			void_stmts((yyvsp[-1].val));
#endif

			(yyval.val) = (yyvsp[-1].val);
		    }
#line 5738 "ripper.c"
    break;

  case 14:
#line 1305 "ripper.y"
                    {
#if 0
			(yyval.val) = new_begin(0, &(yyloc));
#endif
			(yyval.val) = dispatch2(stmts_add, dispatch0(stmts_new),
						  dispatch0(void_stmt));

		    }
#line 5751 "ripper.c"
    break;

  case 15:
#line 1314 "ripper.y"
                    {
#if 0
			(yyval.val) = newline_node((yyvsp[0].val));
#endif
			(yyval.val) = dispatch2(stmts_add, dispatch0(stmts_new), (yyvsp[0].val));

		    }
#line 5763 "ripper.c"
    break;

  case 16:
#line 1322 "ripper.y"
                    {
#if 0
			(yyval.val) = block_append((yyvsp[-2].val), newline_node((yyvsp[0].val)), &(yyloc));
#endif
			(yyval.val) = dispatch2(stmts_add, (yyvsp[-2].val), (yyvsp[0].val));

		    }
#line 5775 "ripper.c"
    break;

  case 17:
#line 1330 "ripper.y"
                    {
			(yyval.val) = remove_begin((yyvsp[0].val));
		    }
#line 5783 "ripper.c"
    break;

  case 18:
#line 1336 "ripper.y"
                    {
			(yyval.val) = (yyvsp[0].val);
		    }
#line 5791 "ripper.c"
    break;

  case 19:
#line 1340 "ripper.y"
                    {
			yyerror0("BEGIN is permitted only at toplevel");
#if 0
			/* local_push(0); */
#endif

		    }
#line 5803 "ripper.c"
    break;

  case 20:
#line 1348 "ripper.y"
                    {
#if 0
			ruby_eval_tree_begin = block_append(ruby_eval_tree_begin,
							    (yyvsp[-1].val), &(yyloc));
			/* NEW_PREEXE($4)); */
			/* local_pop(); */
			(yyval.val) = new_begin(0, &(yyloc));
#endif
			(yyval.val) = dispatch1(BEGIN, (yyvsp[-1].val));

		    }
#line 5819 "ripper.c"
    break;

  case 21:
#line 1361 "ripper.y"
                                      {SET_LEX_STATE(EXPR_FNAME|EXPR_FITEM);}
#line 5825 "ripper.c"
    break;

  case 22:
#line 1362 "ripper.y"
                    {
#if 0
			(yyval.val) = NEW_ALIAS((yyvsp[-2].val), (yyvsp[0].val));
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch2(alias, (yyvsp[-2].val), (yyvsp[0].val));

		    }
#line 5838 "ripper.c"
    break;

  case 23:
#line 1371 "ripper.y"
                    {
#if 0
			(yyval.val) = NEW_VALIAS((yyvsp[-1].val), (yyvsp[0].val));
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch2(var_alias, (yyvsp[-1].val), (yyvsp[0].val));

		    }
#line 5851 "ripper.c"
    break;

  case 24:
#line 1380 "ripper.y"
                    {
#if 0
			char buf[2];
			buf[0] = '$';
			buf[1] = (char)(yyvsp[0].val)->nd_nth;
			(yyval.val) = NEW_VALIAS((yyvsp[-1].val), rb_intern2(buf, 2));
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch2(var_alias, (yyvsp[-1].val), (yyvsp[0].val));

		    }
#line 5867 "ripper.c"
    break;

  case 25:
#line 1392 "ripper.y"
                    {
#if 0
			yyerror0("can't make alias for the number variables");
			(yyval.val) = new_begin(0, &(yyloc));
#endif
			(yyval.val) = dispatch2(var_alias, (yyvsp[-1].val), (yyvsp[0].val));
			(yyval.val) = dispatch1(alias_error, (yyval.val));
			ripper_error();

		    }
#line 5882 "ripper.c"
    break;

  case 26:
#line 1403 "ripper.y"
                    {
#if 0
			(yyval.val) = (yyvsp[0].val);
#endif
			(yyval.val) = dispatch1(undef, (yyvsp[0].val));

		    }
#line 5894 "ripper.c"
    break;

  case 27:
#line 1411 "ripper.y"
                    {
#if 0
			(yyval.val) = new_if((yyvsp[0].val), remove_begin((yyvsp[-2].val)), 0, &(yyloc));
			fixpos((yyval.val), (yyvsp[0].val));
#endif
			(yyval.val) = dispatch2(if_mod, (yyvsp[0].val), (yyvsp[-2].val));

		    }
#line 5907 "ripper.c"
    break;

  case 28:
#line 1420 "ripper.y"
                    {
#if 0
			(yyval.val) = new_unless((yyvsp[0].val), remove_begin((yyvsp[-2].val)), 0, &(yyloc));
			fixpos((yyval.val), (yyvsp[0].val));
#endif
			(yyval.val) = dispatch2(unless_mod, (yyvsp[0].val), (yyvsp[-2].val));

		    }
#line 5920 "ripper.c"
    break;

  case 29:
#line 1429 "ripper.y"
                    {
#if 0
			if ((yyvsp[-2].val) && nd_type((yyvsp[-2].val)) == NODE_BEGIN) {
			    (yyval.val) = NEW_WHILE(cond((yyvsp[0].val), &(yylsp[0])), (yyvsp[-2].val)->nd_body, 0);
			}
			else {
			    (yyval.val) = NEW_WHILE(cond((yyvsp[0].val), &(yylsp[0])), (yyvsp[-2].val), 1);
			}
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch2(while_mod, (yyvsp[0].val), (yyvsp[-2].val));

		    }
#line 5938 "ripper.c"
    break;

  case 30:
#line 1443 "ripper.y"
                    {
#if 0
			if ((yyvsp[-2].val) && nd_type((yyvsp[-2].val)) == NODE_BEGIN) {
			    (yyval.val) = NEW_UNTIL(cond((yyvsp[0].val), &(yylsp[0])), (yyvsp[-2].val)->nd_body, 0);
			}
			else {
			    (yyval.val) = NEW_UNTIL(cond((yyvsp[0].val), &(yylsp[0])), (yyvsp[-2].val), 1);
			}
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch2(until_mod, (yyvsp[0].val), (yyvsp[-2].val));

		    }
#line 5956 "ripper.c"
    break;

  case 31:
#line 1457 "ripper.y"
                    {
#if 0
			NODE *resq;
			YYLTYPE location;
			location.first_loc = (yylsp[-1]).first_loc;
			location.last_loc = (yylsp[0]).last_loc;
			resq = new_resbody(0, remove_begin((yyvsp[0].val)), 0, &location);
			(yyval.val) = new_rescue(remove_begin((yyvsp[-2].val)), resq, 0, &(yyloc));
#endif
			(yyval.val) = dispatch2(rescue_mod, (yyvsp[-2].val), (yyvsp[0].val));

		    }
#line 5973 "ripper.c"
    break;

  case 32:
#line 1470 "ripper.y"
                    {
			if (in_def) {
			    rb_warn0("END in method; use at_exit");
			}
#if 0
			{
			    NODE *scope = NEW_NODE(
				NODE_SCOPE, 0 /* tbl */, (yyvsp[-1].val) /* body */, 0 /* args */);
			    (yyval.val) = NEW_POSTEXE(scope);
			    scope->nd_loc = (yyloc);
			    (yyval.val)->nd_loc = (yyloc);
			}
#endif
			(yyval.val) = dispatch1(END, (yyvsp[-1].val));

		    }
#line 5994 "ripper.c"
    break;

  case 34:
#line 1488 "ripper.y"
                    {
#if 0
			value_expr((yyvsp[0].val));
			(yyval.val) = node_assign((yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			(yyval.val) = dispatch2(massign, (yyvsp[-2].val), (yyvsp[0].val));

		    }
#line 6007 "ripper.c"
    break;

  case 35:
#line 1497 "ripper.y"
                    {
			value_expr((yyvsp[0].val));
			(yyval.val) = node_assign((yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
		    }
#line 6016 "ripper.c"
    break;

  case 36:
#line 1502 "ripper.y"
                    {
#if 0
			(yyval.val) = node_assign((yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			(yyval.val) = dispatch2(massign, (yyvsp[-2].val), (yyvsp[0].val));

		    }
#line 6028 "ripper.c"
    break;

  case 38:
#line 1513 "ripper.y"
                    {
			(yyval.val) = node_assign((yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
		    }
#line 6036 "ripper.c"
    break;

  case 39:
#line 1517 "ripper.y"
                    {
			(yyval.val) = new_op_assign((yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 6044 "ripper.c"
    break;

  case 40:
#line 1521 "ripper.y"
                    {
#if 0
			NODE *args;

			(yyvsp[-3].val) = make_array((yyvsp[-3].val), &(yylsp[-3]));
			args = arg_concat((yyvsp[-3].val), (yyvsp[0].val), &(yyloc));
			if ((yyvsp[-1].val) == tOROP) {
			    (yyvsp[-1].val) = 0;
			}
			else if ((yyvsp[-1].val) == tANDOP) {
			    (yyvsp[-1].val) = 1;
			}
			(yyval.val) = NEW_OP_ASGN1((yyvsp[-5].val), (yyvsp[-1].val), args);
			fixpos((yyval.val), (yyvsp[-5].val));
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch2(aref_field, (yyvsp[-5].val), escape_Qundef((yyvsp[-3].val)));
			(yyval.val) = dispatch3(opassign, (yyval.val), (yyvsp[-1].val), (yyvsp[0].val));

		    }
#line 6069 "ripper.c"
    break;

  case 41:
#line 1542 "ripper.y"
                    {
			(yyval.val) = new_attr_op_assign((yyvsp[-4].val), (yyvsp[-3].val), (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 6077 "ripper.c"
    break;

  case 42:
#line 1546 "ripper.y"
                    {
			(yyval.val) = new_attr_op_assign((yyvsp[-4].val), (yyvsp[-3].val), (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 6085 "ripper.c"
    break;

  case 43:
#line 1550 "ripper.y"
                    {
#if 0
			YYLTYPE location;
			location.first_loc = (yylsp[-4]).first_loc;
			location.last_loc = (yylsp[-2]).last_loc;
#endif

			(yyval.val) = const_path_field((yyvsp[-4].val), (yyvsp[-2].val), &location);
			(yyval.val) = new_const_op_assign((yyval.val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 6100 "ripper.c"
    break;

  case 44:
#line 1561 "ripper.y"
                    {
			(yyval.val) = new_attr_op_assign((yyvsp[-4].val), ID2VAL(idCOLON2), (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 6108 "ripper.c"
    break;

  case 45:
#line 1565 "ripper.y"
                    {
			(yyvsp[-2].val) = var_field((yyvsp[-2].val));
			(yyval.val) = backref_assign_error((yyvsp[-2].val), node_assign((yyvsp[-2].val), (yyvsp[0].val), &(yyloc)), &(yyloc));
		    }
#line 6117 "ripper.c"
    break;

  case 46:
#line 1572 "ripper.y"
                    {
#if 0
			value_expr((yyvsp[0].val));
			(yyval.val) = (yyvsp[0].val);
#endif

		    }
#line 6129 "ripper.c"
    break;

  case 47:
#line 1580 "ripper.y"
                    {
#if 0
			YYLTYPE location;
			location.first_loc = (yylsp[-1]).first_loc;
			location.last_loc = (yylsp[0]).last_loc;
			value_expr((yyvsp[-2].val));
			(yyval.val) = new_rescue((yyvsp[-2].val), new_resbody(0, remove_begin((yyvsp[0].val)), 0, &location), 0, &(yyloc));
#endif
			(yyval.val) = dispatch2(rescue_mod, (yyvsp[-2].val), (yyvsp[0].val));

		    }
#line 6145 "ripper.c"
    break;

  case 50:
#line 1596 "ripper.y"
                    {
			(yyval.val) = logop(idAND, (yyvsp[-2].val), (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 6153 "ripper.c"
    break;

  case 51:
#line 1600 "ripper.y"
                    {
			(yyval.val) = logop(idOR, (yyvsp[-2].val), (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 6161 "ripper.c"
    break;

  case 52:
#line 1604 "ripper.y"
                    {
			(yyval.val) = call_uni_op(method_cond((yyvsp[0].val), &(yylsp[0])), METHOD_NOT, &(yylsp[-2]), &(yyloc));
		    }
#line 6169 "ripper.c"
    break;

  case 53:
#line 1608 "ripper.y"
                    {
			(yyval.val) = call_uni_op(method_cond((yyvsp[0].val), &(yylsp[0])), '!', &(yylsp[-1]), &(yyloc));
		    }
#line 6177 "ripper.c"
    break;

  case 55:
#line 1615 "ripper.y"
                    {
#if 0
			value_expr((yyvsp[0].val));
			(yyval.val) = (yyvsp[0].val);
			if (!(yyval.val)) (yyval.val) = NEW_NIL();
#endif
			(yyval.val) = (yyvsp[0].val);

		    }
#line 6191 "ripper.c"
    break;

  case 59:
#line 1632 "ripper.y"
                    {
			(yyval.val) = new_qcall((yyvsp[-2].val), (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 6199 "ripper.c"
    break;

  case 60:
#line 1638 "ripper.y"
                    {
#if 0
			(yyval.num) = ruby_sourceline;
#endif

		    }
#line 6210 "ripper.c"
    break;

  case 61:
#line 1645 "ripper.y"
                    {
			(yyval.val) = (yyvsp[-1].val);
#if 0
			(yyvsp[-1].val)->nd_body->nd_loc.first_loc = (yylsp[-3]).first_loc;
			(yyvsp[-1].val)->nd_body->nd_loc.last_loc = (yylsp[0]).last_loc;
			nd_set_line((yyval.val), (yyvsp[-2].num));
#endif
		    }
#line 6223 "ripper.c"
    break;

  case 62:
#line 1656 "ripper.y"
                    {
#if 0
			(yyval.val) = new_fcall((yyvsp[0].val), 0, &(yyloc));
			nd_set_line((yyval.val), tokline);
#endif

		    }
#line 6235 "ripper.c"
    break;

  case 63:
#line 1666 "ripper.y"
                    {
#if 0
			(yyval.val) = (yyvsp[-1].val);
			(yyval.val)->nd_args = (yyvsp[0].val);
			nd_set_last_loc((yyvsp[-1].val), (yylsp[0]).last_loc);
#endif
			(yyval.val) = dispatch2(command, (yyvsp[-1].val), (yyvsp[0].val));

		    }
#line 6249 "ripper.c"
    break;

  case 64:
#line 1676 "ripper.y"
                    {
			block_dup_check((yyvsp[-1].val),(yyvsp[0].val));
			(yyval.val) = new_command((yyvsp[-2].val), (yyvsp[-1].val));
			(yyval.val) = method_add_block((yyval.val), (yyvsp[0].val));
			fixpos((yyval.val), (yyvsp[-2].val));
#if 0
			(yyval.val)->nd_loc = (yyloc);
			nd_set_last_loc((yyvsp[-2].val), (yylsp[-1]).last_loc);
#endif

		    }
#line 6265 "ripper.c"
    break;

  case 65:
#line 1688 "ripper.y"
                    {
			(yyval.val) = new_command_qcall((yyvsp[-2].val), (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
			fixpos((yyval.val), (yyvsp[-3].val));
		    }
#line 6274 "ripper.c"
    break;

  case 66:
#line 1693 "ripper.y"
                    {
			block_dup_check((yyvsp[-1].val),(yyvsp[0].val));
			(yyval.val) = new_command_qcall((yyvsp[-3].val), (yyvsp[-4].val), (yyvsp[-2].val), (yyvsp[-1].val), &(yyloc));
			(yyval.val) = method_add_block((yyval.val), (yyvsp[0].val));
			fixpos((yyval.val), (yyvsp[-4].val));
#if 0
			(yyval.val)->nd_loc = (yyloc);
#endif

		   }
#line 6289 "ripper.c"
    break;

  case 67:
#line 1704 "ripper.y"
                    {
			(yyval.val) = new_command_qcall(ID2VAL(idCOLON2), (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
			fixpos((yyval.val), (yyvsp[-3].val));
		    }
#line 6298 "ripper.c"
    break;

  case 68:
#line 1709 "ripper.y"
                    {
			block_dup_check((yyvsp[-1].val),(yyvsp[0].val));
			(yyval.val) = new_command_qcall(ID2VAL(idCOLON2), (yyvsp[-4].val), (yyvsp[-2].val), (yyvsp[-1].val), &(yyloc));
			(yyval.val) = method_add_block((yyval.val), (yyvsp[0].val));
			fixpos((yyval.val), (yyvsp[-4].val));
#if 0
			(yyval.val)->nd_loc = (yyloc);
#endif

		   }
#line 6313 "ripper.c"
    break;

  case 69:
#line 1720 "ripper.y"
                    {
#if 0
			(yyval.val) = NEW_SUPER((yyvsp[0].val));
			fixpos((yyval.val), (yyvsp[0].val));
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch1(super, (yyvsp[0].val));

		    }
#line 6327 "ripper.c"
    break;

  case 70:
#line 1730 "ripper.y"
                    {
#if 0
			(yyval.val) = new_yield((yyvsp[0].val), &(yyloc));
			fixpos((yyval.val), (yyvsp[0].val));
#endif
			(yyval.val) = dispatch1(yield, (yyvsp[0].val));

		    }
#line 6340 "ripper.c"
    break;

  case 71:
#line 1739 "ripper.y"
                    {
#if 0
			(yyval.val) = NEW_RETURN(ret_args((yyvsp[0].val)));
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch1(return, (yyvsp[0].val));

		    }
#line 6353 "ripper.c"
    break;

  case 72:
#line 1748 "ripper.y"
                    {
#if 0
			(yyval.val) = NEW_BREAK(ret_args((yyvsp[0].val)));
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch1(break, (yyvsp[0].val));

		    }
#line 6366 "ripper.c"
    break;

  case 73:
#line 1757 "ripper.y"
                    {
#if 0
			(yyval.val) = NEW_NEXT(ret_args((yyvsp[0].val)));
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch1(next, (yyvsp[0].val));

		    }
#line 6379 "ripper.c"
    break;

  case 75:
#line 1769 "ripper.y"
                    {
#if 0
			(yyval.val) = (yyvsp[-1].val);
#endif
			(yyval.val) = dispatch1(mlhs_paren, (yyvsp[-1].val));

		    }
#line 6391 "ripper.c"
    break;

  case 77:
#line 1780 "ripper.y"
                    {
#if 0
			(yyval.val) = new_masgn(new_list((yyvsp[-1].val), &(yyloc)), 0, &(yyloc));
#endif
			(yyval.val) = dispatch1(mlhs_paren, (yyvsp[-1].val));

		    }
#line 6403 "ripper.c"
    break;

  case 78:
#line 1790 "ripper.y"
                    {
#if 0
			(yyval.val) = new_masgn((yyvsp[0].val), 0, &(yyloc));
#endif
			(yyval.val) = (yyvsp[0].val);

		    }
#line 6415 "ripper.c"
    break;

  case 79:
#line 1798 "ripper.y"
                    {
#if 0
			(yyval.val) = new_masgn(list_append((yyvsp[-1].val),(yyvsp[0].val)), 0, &(yyloc));
#endif
			(yyval.val) = mlhs_add((yyvsp[-1].val), (yyvsp[0].val));

		    }
#line 6427 "ripper.c"
    break;

  case 80:
#line 1806 "ripper.y"
                    {
#if 0
			(yyval.val) = new_masgn((yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#endif
			(yyval.val) = mlhs_add_star((yyvsp[-2].val), (yyvsp[0].val));

		    }
#line 6439 "ripper.c"
    break;

  case 81:
#line 1814 "ripper.y"
                    {
#if 0
			(yyval.val) = new_masgn((yyvsp[-4].val), new_postarg((yyvsp[-2].val),(yyvsp[0].val),&(yyloc)), &(yyloc));
#endif
			(yyvsp[-4].val) = mlhs_add_star((yyvsp[-4].val), (yyvsp[-2].val));
			(yyval.val) = mlhs_add_post((yyvsp[-4].val), (yyvsp[0].val));

		    }
#line 6452 "ripper.c"
    break;

  case 82:
#line 1823 "ripper.y"
                    {
#if 0
			(yyval.val) = new_masgn((yyvsp[-1].val), NODE_SPECIAL_NO_NAME_REST, &(yyloc));
#endif
			(yyval.val) = mlhs_add_star((yyvsp[-1].val), Qnil);

		    }
#line 6464 "ripper.c"
    break;

  case 83:
#line 1831 "ripper.y"
                    {
#if 0
			(yyval.val) = new_masgn((yyvsp[-3].val), new_postarg(NODE_SPECIAL_NO_NAME_REST, (yyvsp[0].val), &(yyloc)), &(yyloc));
#endif
			(yyvsp[-3].val) = mlhs_add_star((yyvsp[-3].val), Qnil);
			(yyval.val) = mlhs_add_post((yyvsp[-3].val), (yyvsp[0].val));

		    }
#line 6477 "ripper.c"
    break;

  case 84:
#line 1840 "ripper.y"
                    {
#if 0
			(yyval.val) = new_masgn(0, (yyvsp[0].val), &(yyloc));
#endif
			(yyval.val) = mlhs_add_star(mlhs_new(), (yyvsp[0].val));

		    }
#line 6489 "ripper.c"
    break;

  case 85:
#line 1848 "ripper.y"
                    {
#if 0
			(yyval.val) = new_masgn(0, new_postarg((yyvsp[-2].val),(yyvsp[0].val),&(yyloc)), &(yyloc));
#endif
			(yyvsp[-2].val) = mlhs_add_star(mlhs_new(), (yyvsp[-2].val));
			(yyval.val) = mlhs_add_post((yyvsp[-2].val), (yyvsp[0].val));

		    }
#line 6502 "ripper.c"
    break;

  case 86:
#line 1857 "ripper.y"
                    {
#if 0
			(yyval.val) = new_masgn(0, NODE_SPECIAL_NO_NAME_REST, &(yyloc));
#endif
			(yyval.val) = mlhs_add_star(mlhs_new(), Qnil);

		    }
#line 6514 "ripper.c"
    break;

  case 87:
#line 1865 "ripper.y"
                    {
#if 0
			(yyval.val) = new_masgn(0, new_postarg(NODE_SPECIAL_NO_NAME_REST, (yyvsp[0].val), &(yyloc)), &(yyloc));
#endif
			(yyval.val) = mlhs_add_star(mlhs_new(), Qnil);
			(yyval.val) = mlhs_add_post((yyval.val), (yyvsp[0].val));

		    }
#line 6527 "ripper.c"
    break;

  case 89:
#line 1877 "ripper.y"
                    {
#if 0
			(yyval.val) = (yyvsp[-1].val);
#endif
			(yyval.val) = dispatch1(mlhs_paren, (yyvsp[-1].val));

		    }
#line 6539 "ripper.c"
    break;

  case 90:
#line 1887 "ripper.y"
                    {
#if 0
			(yyval.val) = new_list((yyvsp[-1].val), &(yylsp[-1]));
#endif
			(yyval.val) = mlhs_add(mlhs_new(), (yyvsp[-1].val));

		    }
#line 6551 "ripper.c"
    break;

  case 91:
#line 1895 "ripper.y"
                    {
#if 0
			(yyval.val) = list_append((yyvsp[-2].val), (yyvsp[-1].val));
#endif
			(yyval.val) = mlhs_add((yyvsp[-2].val), (yyvsp[-1].val));

		    }
#line 6563 "ripper.c"
    break;

  case 92:
#line 1905 "ripper.y"
                    {
#if 0
			(yyval.val) = new_list((yyvsp[0].val), &(yyloc));
#endif
			(yyval.val) = mlhs_add(mlhs_new(), (yyvsp[0].val));

		    }
#line 6575 "ripper.c"
    break;

  case 93:
#line 1913 "ripper.y"
                    {
#if 0
			(yyval.val) = list_append((yyvsp[-2].val), (yyvsp[0].val));
#endif
			(yyval.val) = mlhs_add((yyvsp[-2].val), (yyvsp[0].val));

		    }
#line 6587 "ripper.c"
    break;

  case 94:
#line 1923 "ripper.y"
                    {
			(yyval.val) = assignable(var_field((yyvsp[0].val)), 0, &(yyloc));
		    }
#line 6595 "ripper.c"
    break;

  case 95:
#line 1927 "ripper.y"
                    {
			(yyval.val) = assignable(var_field((yyvsp[0].val)), 0, &(yyloc));
		    }
#line 6603 "ripper.c"
    break;

  case 96:
#line 1931 "ripper.y"
                    {
#if 0
			(yyval.val) = aryset((yyvsp[-3].val), (yyvsp[-1].val), &(yyloc));
#endif
			(yyval.val) = dispatch2(aref_field, (yyvsp[-3].val), escape_Qundef((yyvsp[-1].val)));

		    }
#line 6615 "ripper.c"
    break;

  case 97:
#line 1939 "ripper.y"
                    {
#if 0
			(yyval.val) = attrset((yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			(yyval.val) = dispatch3(field, (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val));

		    }
#line 6627 "ripper.c"
    break;

  case 98:
#line 1947 "ripper.y"
                    {
#if 0
			(yyval.val) = attrset((yyvsp[-2].val), idCOLON2, (yyvsp[0].val), &(yyloc));
#endif
			(yyval.val) = dispatch2(const_path_field, (yyvsp[-2].val), (yyvsp[0].val));

		    }
#line 6639 "ripper.c"
    break;

  case 99:
#line 1955 "ripper.y"
                    {
#if 0
			(yyval.val) = attrset((yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			(yyval.val) = dispatch3(field, (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val));

		    }
#line 6651 "ripper.c"
    break;

  case 100:
#line 1963 "ripper.y"
                    {
			(yyval.val) = const_decl(const_path_field((yyvsp[-2].val), (yyvsp[0].val), &(yyloc)), &(yyloc));
		    }
#line 6659 "ripper.c"
    break;

  case 101:
#line 1967 "ripper.y"
                    {
			(yyval.val) = const_decl(top_const_field((yyvsp[0].val)), &(yyloc));
		    }
#line 6667 "ripper.c"
    break;

  case 102:
#line 1971 "ripper.y"
                    {
			(yyvsp[0].val) = var_field((yyvsp[0].val));
			(yyval.val) = backref_assign_error((yyvsp[0].val), (yyvsp[0].val), &(yyloc));
		    }
#line 6676 "ripper.c"
    break;

  case 103:
#line 1978 "ripper.y"
                    {
			(yyval.val) = assignable(var_field((yyvsp[0].val)), 0, &(yyloc));
		    }
#line 6684 "ripper.c"
    break;

  case 104:
#line 1982 "ripper.y"
                    {
			(yyval.val) = assignable(var_field((yyvsp[0].val)), 0, &(yyloc));
		    }
#line 6692 "ripper.c"
    break;

  case 105:
#line 1986 "ripper.y"
                    {
#if 0
			(yyval.val) = aryset((yyvsp[-3].val), (yyvsp[-1].val), &(yyloc));
#endif
			(yyval.val) = dispatch2(aref_field, (yyvsp[-3].val), escape_Qundef((yyvsp[-1].val)));

		    }
#line 6704 "ripper.c"
    break;

  case 106:
#line 1994 "ripper.y"
                    {
#if 0
			(yyval.val) = attrset((yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			(yyval.val) = dispatch3(field, (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val));

		    }
#line 6716 "ripper.c"
    break;

  case 107:
#line 2002 "ripper.y"
                    {
#if 0
			(yyval.val) = attrset((yyvsp[-2].val), idCOLON2, (yyvsp[0].val), &(yyloc));
#endif
			(yyval.val) = dispatch3(field, (yyvsp[-2].val), ID2VAL(idCOLON2), (yyvsp[0].val));

		    }
#line 6728 "ripper.c"
    break;

  case 108:
#line 2010 "ripper.y"
                    {
#if 0
			(yyval.val) = attrset((yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			(yyval.val) = dispatch3(field, (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val));

		    }
#line 6740 "ripper.c"
    break;

  case 109:
#line 2018 "ripper.y"
                    {
			(yyval.val) = const_decl(const_path_field((yyvsp[-2].val), (yyvsp[0].val), &(yyloc)), &(yyloc));
		    }
#line 6748 "ripper.c"
    break;

  case 110:
#line 2022 "ripper.y"
                    {
			(yyval.val) = const_decl(top_const_field((yyvsp[0].val)), &(yyloc));
		    }
#line 6756 "ripper.c"
    break;

  case 111:
#line 2026 "ripper.y"
                    {
			(yyvsp[0].val) = var_field((yyvsp[0].val));
			(yyval.val) = backref_assign_error((yyvsp[0].val), (yyvsp[0].val), &(yyloc));
		    }
#line 6765 "ripper.c"
    break;

  case 112:
#line 2033 "ripper.y"
                    {
#if 0
			yyerror0("class/module name must be CONSTANT");
#endif
			(yyval.val) = dispatch1(class_name_error, (yyvsp[0].val));
			ripper_error();

		    }
#line 6778 "ripper.c"
    break;

  case 114:
#line 2045 "ripper.y"
                    {
#if 0
			(yyval.val) = NEW_COLON3((yyvsp[0].val));
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch1(top_const_ref, (yyvsp[0].val));

		    }
#line 6791 "ripper.c"
    break;

  case 115:
#line 2054 "ripper.y"
                    {
#if 0
			(yyval.val) = NEW_COLON2(0, (yyval.val));
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch1(const_ref, (yyvsp[0].val));

		    }
#line 6804 "ripper.c"
    break;

  case 116:
#line 2063 "ripper.y"
                    {
#if 0
			(yyval.val) = NEW_COLON2((yyvsp[-2].val), (yyvsp[0].val));
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch2(const_path_ref, (yyvsp[-2].val), (yyvsp[0].val));

		    }
#line 6817 "ripper.c"
    break;

  case 120:
#line 2077 "ripper.y"
                    {
			SET_LEX_STATE(EXPR_ENDFN);
			(yyval.val) = (yyvsp[0].val);
		    }
#line 6826 "ripper.c"
    break;

  case 121:
#line 2082 "ripper.y"
                    {
			SET_LEX_STATE(EXPR_ENDFN);
			(yyval.val) = (yyvsp[0].val);
		    }
#line 6835 "ripper.c"
    break;

  case 124:
#line 2093 "ripper.y"
                    {
#if 0
			(yyval.val) = new_lit(ID2SYM((yyvsp[0].val)), &(yyloc));
#endif
			(yyval.val) = dispatch1(symbol_literal, (yyvsp[0].val));

		    }
#line 6847 "ripper.c"
    break;

  case 126:
#line 2104 "ripper.y"
                    {
#if 0
			(yyval.val) = new_undef((yyvsp[0].val), &(yyloc));
#endif
			(yyval.val) = rb_ary_new3(1, get_value((yyvsp[0].val)));

		    }
#line 6859 "ripper.c"
    break;

  case 127:
#line 2111 "ripper.y"
                                 {SET_LEX_STATE(EXPR_FNAME|EXPR_FITEM);}
#line 6865 "ripper.c"
    break;

  case 128:
#line 2112 "ripper.y"
                    {
#if 0
			NODE *undef = new_undef((yyvsp[0].val), &(yyloc));
			(yyval.val) = block_append((yyvsp[-3].val), undef, &(yyloc));
#endif
			rb_ary_push((yyvsp[-3].val), get_value((yyvsp[0].val)));

		    }
#line 6878 "ripper.c"
    break;

  case 129:
#line 2122 "ripper.y"
                                { ifndef_ripper((yyval.val) = '|'); }
#line 6884 "ripper.c"
    break;

  case 130:
#line 2123 "ripper.y"
                                { ifndef_ripper((yyval.val) = '^'); }
#line 6890 "ripper.c"
    break;

  case 131:
#line 2124 "ripper.y"
                                { ifndef_ripper((yyval.val) = '&'); }
#line 6896 "ripper.c"
    break;

  case 132:
#line 2125 "ripper.y"
                                { ifndef_ripper((yyval.val) = tCMP); }
#line 6902 "ripper.c"
    break;

  case 133:
#line 2126 "ripper.y"
                                { ifndef_ripper((yyval.val) = tEQ); }
#line 6908 "ripper.c"
    break;

  case 134:
#line 2127 "ripper.y"
                                { ifndef_ripper((yyval.val) = tEQQ); }
#line 6914 "ripper.c"
    break;

  case 135:
#line 2128 "ripper.y"
                                { ifndef_ripper((yyval.val) = tMATCH); }
#line 6920 "ripper.c"
    break;

  case 136:
#line 2129 "ripper.y"
                                { ifndef_ripper((yyval.val) = tNMATCH); }
#line 6926 "ripper.c"
    break;

  case 137:
#line 2130 "ripper.y"
                                { ifndef_ripper((yyval.val) = '>'); }
#line 6932 "ripper.c"
    break;

  case 138:
#line 2131 "ripper.y"
                                { ifndef_ripper((yyval.val) = tGEQ); }
#line 6938 "ripper.c"
    break;

  case 139:
#line 2132 "ripper.y"
                                { ifndef_ripper((yyval.val) = '<'); }
#line 6944 "ripper.c"
    break;

  case 140:
#line 2133 "ripper.y"
                                { ifndef_ripper((yyval.val) = tLEQ); }
#line 6950 "ripper.c"
    break;

  case 141:
#line 2134 "ripper.y"
                                { ifndef_ripper((yyval.val) = tNEQ); }
#line 6956 "ripper.c"
    break;

  case 142:
#line 2135 "ripper.y"
                                { ifndef_ripper((yyval.val) = tLSHFT); }
#line 6962 "ripper.c"
    break;

  case 143:
#line 2136 "ripper.y"
                                { ifndef_ripper((yyval.val) = tRSHFT); }
#line 6968 "ripper.c"
    break;

  case 144:
#line 2137 "ripper.y"
                                { ifndef_ripper((yyval.val) = '+'); }
#line 6974 "ripper.c"
    break;

  case 145:
#line 2138 "ripper.y"
                                { ifndef_ripper((yyval.val) = '-'); }
#line 6980 "ripper.c"
    break;

  case 146:
#line 2139 "ripper.y"
                                { ifndef_ripper((yyval.val) = '*'); }
#line 6986 "ripper.c"
    break;

  case 147:
#line 2140 "ripper.y"
                                { ifndef_ripper((yyval.val) = '*'); }
#line 6992 "ripper.c"
    break;

  case 148:
#line 2141 "ripper.y"
                                { ifndef_ripper((yyval.val) = '/'); }
#line 6998 "ripper.c"
    break;

  case 149:
#line 2142 "ripper.y"
                                { ifndef_ripper((yyval.val) = '%'); }
#line 7004 "ripper.c"
    break;

  case 150:
#line 2143 "ripper.y"
                                { ifndef_ripper((yyval.val) = tPOW); }
#line 7010 "ripper.c"
    break;

  case 151:
#line 2144 "ripper.y"
                                { ifndef_ripper((yyval.val) = tDSTAR); }
#line 7016 "ripper.c"
    break;

  case 152:
#line 2145 "ripper.y"
                                { ifndef_ripper((yyval.val) = '!'); }
#line 7022 "ripper.c"
    break;

  case 153:
#line 2146 "ripper.y"
                                { ifndef_ripper((yyval.val) = '~'); }
#line 7028 "ripper.c"
    break;

  case 154:
#line 2147 "ripper.y"
                                { ifndef_ripper((yyval.val) = tUPLUS); }
#line 7034 "ripper.c"
    break;

  case 155:
#line 2148 "ripper.y"
                                { ifndef_ripper((yyval.val) = tUMINUS); }
#line 7040 "ripper.c"
    break;

  case 156:
#line 2149 "ripper.y"
                                { ifndef_ripper((yyval.val) = tAREF); }
#line 7046 "ripper.c"
    break;

  case 157:
#line 2150 "ripper.y"
                                { ifndef_ripper((yyval.val) = tASET); }
#line 7052 "ripper.c"
    break;

  case 158:
#line 2151 "ripper.y"
                                { ifndef_ripper((yyval.val) = '`'); }
#line 7058 "ripper.c"
    break;

  case 200:
#line 2169 "ripper.y"
                    {
			(yyval.val) = node_assign((yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
		    }
#line 7066 "ripper.c"
    break;

  case 201:
#line 2173 "ripper.y"
                    {
			(yyval.val) = new_op_assign((yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 7074 "ripper.c"
    break;

  case 202:
#line 2177 "ripper.y"
                    {
#if 0
			NODE *args;

			value_expr((yyvsp[0].val));
			(yyvsp[-3].val) = make_array((yyvsp[-3].val), &(yylsp[-3]));
			if (nd_type((yyvsp[-3].val)) == NODE_BLOCK_PASS) {
			    args = NEW_ARGSCAT((yyvsp[-3].val), (yyvsp[0].val));
			    args->nd_loc = (yyloc);
			}
			else {
			    args = arg_concat((yyvsp[-3].val), (yyvsp[0].val), &(yyloc));
			}
			if ((yyvsp[-1].val) == tOROP) {
			    (yyvsp[-1].val) = 0;
			}
			else if ((yyvsp[-1].val) == tANDOP) {
			    (yyvsp[-1].val) = 1;
			}
			(yyval.val) = NEW_OP_ASGN1((yyvsp[-5].val), (yyvsp[-1].val), args);
			fixpos((yyval.val), (yyvsp[-5].val));
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyvsp[-5].val) = dispatch2(aref_field, (yyvsp[-5].val), escape_Qundef((yyvsp[-3].val)));
			(yyval.val) = dispatch3(opassign, (yyvsp[-5].val), (yyvsp[-1].val), (yyvsp[0].val));

		    }
#line 7106 "ripper.c"
    break;

  case 203:
#line 2205 "ripper.y"
                    {
			value_expr((yyvsp[0].val));
			(yyval.val) = new_attr_op_assign((yyvsp[-4].val), (yyvsp[-3].val), (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 7115 "ripper.c"
    break;

  case 204:
#line 2210 "ripper.y"
                    {
			value_expr((yyvsp[0].val));
			(yyval.val) = new_attr_op_assign((yyvsp[-4].val), (yyvsp[-3].val), (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 7124 "ripper.c"
    break;

  case 205:
#line 2215 "ripper.y"
                    {
			value_expr((yyvsp[0].val));
			(yyval.val) = new_attr_op_assign((yyvsp[-4].val), ID2VAL(idCOLON2), (yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 7133 "ripper.c"
    break;

  case 206:
#line 2220 "ripper.y"
                    {
#if 0
			YYLTYPE location;
			location.first_loc = (yylsp[-4]).first_loc;
			location.last_loc = (yylsp[-2]).last_loc;
#endif

			(yyval.val) = const_path_field((yyvsp[-4].val), (yyvsp[-2].val), &location);
			(yyval.val) = new_const_op_assign((yyval.val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 7148 "ripper.c"
    break;

  case 207:
#line 2231 "ripper.y"
                    {
			(yyval.val) = top_const_field((yyvsp[-2].val));
			(yyval.val) = new_const_op_assign((yyval.val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 7157 "ripper.c"
    break;

  case 208:
#line 2236 "ripper.y"
                    {
			(yyvsp[-2].val) = var_field((yyvsp[-2].val));
			(yyval.val) = backref_assign_error((yyvsp[-2].val), new_op_assign((yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc)), &(yyloc));
		    }
#line 7166 "ripper.c"
    break;

  case 209:
#line 2241 "ripper.y"
                    {
#if 0
			value_expr((yyvsp[-2].val));
			value_expr((yyvsp[0].val));
			(yyval.val) = NEW_DOT2((yyvsp[-2].val), (yyvsp[0].val));
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch2(dot2, (yyvsp[-2].val), (yyvsp[0].val));

		    }
#line 7181 "ripper.c"
    break;

  case 210:
#line 2252 "ripper.y"
                    {
#if 0
			value_expr((yyvsp[-2].val));
			value_expr((yyvsp[0].val));
			(yyval.val) = NEW_DOT3((yyvsp[-2].val), (yyvsp[0].val));
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch2(dot3, (yyvsp[-2].val), (yyvsp[0].val));

		    }
#line 7196 "ripper.c"
    break;

  case 211:
#line 2263 "ripper.y"
                    {
			(yyval.val) = call_bin_op((yyvsp[-2].val), '+', (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7204 "ripper.c"
    break;

  case 212:
#line 2267 "ripper.y"
                    {
			(yyval.val) = call_bin_op((yyvsp[-2].val), '-', (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7212 "ripper.c"
    break;

  case 213:
#line 2271 "ripper.y"
                    {
			(yyval.val) = call_bin_op((yyvsp[-2].val), '*', (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7220 "ripper.c"
    break;

  case 214:
#line 2275 "ripper.y"
                    {
			(yyval.val) = call_bin_op((yyvsp[-2].val), '/', (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7228 "ripper.c"
    break;

  case 215:
#line 2279 "ripper.y"
                    {
			(yyval.val) = call_bin_op((yyvsp[-2].val), '%', (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7236 "ripper.c"
    break;

  case 216:
#line 2283 "ripper.y"
                    {
			(yyval.val) = call_bin_op((yyvsp[-2].val), idPow, (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7244 "ripper.c"
    break;

  case 217:
#line 2287 "ripper.y"
                    {
			(yyval.val) = call_uni_op(call_bin_op((yyvsp[-2].val), idPow, (yyvsp[0].val), &(yylsp[-2]), &(yyloc)), idUMinus, &(yylsp[-3]), &(yyloc));
		    }
#line 7252 "ripper.c"
    break;

  case 218:
#line 2291 "ripper.y"
                    {
			(yyval.val) = call_uni_op((yyvsp[0].val), idUPlus, &(yylsp[-1]), &(yyloc));
		    }
#line 7260 "ripper.c"
    break;

  case 219:
#line 2295 "ripper.y"
                    {
			(yyval.val) = call_uni_op((yyvsp[0].val), idUMinus, &(yylsp[-1]), &(yyloc));
		    }
#line 7268 "ripper.c"
    break;

  case 220:
#line 2299 "ripper.y"
                    {
			(yyval.val) = call_bin_op((yyvsp[-2].val), '|', (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7276 "ripper.c"
    break;

  case 221:
#line 2303 "ripper.y"
                    {
			(yyval.val) = call_bin_op((yyvsp[-2].val), '^', (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7284 "ripper.c"
    break;

  case 222:
#line 2307 "ripper.y"
                    {
			(yyval.val) = call_bin_op((yyvsp[-2].val), '&', (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7292 "ripper.c"
    break;

  case 223:
#line 2311 "ripper.y"
                    {
			(yyval.val) = call_bin_op((yyvsp[-2].val), idCmp, (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7300 "ripper.c"
    break;

  case 225:
#line 2316 "ripper.y"
                    {
			(yyval.val) = call_bin_op((yyvsp[-2].val), idEq, (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7308 "ripper.c"
    break;

  case 226:
#line 2320 "ripper.y"
                    {
			(yyval.val) = call_bin_op((yyvsp[-2].val), idEqq, (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7316 "ripper.c"
    break;

  case 227:
#line 2324 "ripper.y"
                    {
			(yyval.val) = call_bin_op((yyvsp[-2].val), idNeq, (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7324 "ripper.c"
    break;

  case 228:
#line 2328 "ripper.y"
                    {
			(yyval.val) = match_op((yyvsp[-2].val), (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7332 "ripper.c"
    break;

  case 229:
#line 2332 "ripper.y"
                    {
			(yyval.val) = call_bin_op((yyvsp[-2].val), idNeqTilde, (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7340 "ripper.c"
    break;

  case 230:
#line 2336 "ripper.y"
                    {
			(yyval.val) = call_uni_op(method_cond((yyvsp[0].val), &(yylsp[0])), '!', &(yylsp[-1]), &(yyloc));
		    }
#line 7348 "ripper.c"
    break;

  case 231:
#line 2340 "ripper.y"
                    {
			(yyval.val) = call_uni_op((yyvsp[0].val), '~', &(yylsp[-1]), &(yyloc));
		    }
#line 7356 "ripper.c"
    break;

  case 232:
#line 2344 "ripper.y"
                    {
			(yyval.val) = call_bin_op((yyvsp[-2].val), idLTLT, (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7364 "ripper.c"
    break;

  case 233:
#line 2348 "ripper.y"
                    {
			(yyval.val) = call_bin_op((yyvsp[-2].val), idGTGT, (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7372 "ripper.c"
    break;

  case 234:
#line 2352 "ripper.y"
                    {
			(yyval.val) = logop(idANDOP, (yyvsp[-2].val), (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7380 "ripper.c"
    break;

  case 235:
#line 2356 "ripper.y"
                    {
			(yyval.val) = logop(idOROP, (yyvsp[-2].val), (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7388 "ripper.c"
    break;

  case 236:
#line 2359 "ripper.y"
                                         {in_defined = 1;}
#line 7394 "ripper.c"
    break;

  case 237:
#line 2360 "ripper.y"
                    {
			in_defined = 0;
			(yyval.val) = new_defined((yyvsp[0].val), &(yyloc));
		    }
#line 7403 "ripper.c"
    break;

  case 238:
#line 2365 "ripper.y"
                    {
#if 0
			value_expr((yyvsp[-5].val));
			(yyval.val) = new_if((yyvsp[-5].val), (yyvsp[-3].val), (yyvsp[0].val), &(yyloc));
			fixpos((yyval.val), (yyvsp[-5].val));
#endif
			(yyval.val) = dispatch3(ifop, (yyvsp[-5].val), (yyvsp[-3].val), (yyvsp[0].val));

		    }
#line 7417 "ripper.c"
    break;

  case 239:
#line 2375 "ripper.y"
                    {
			(yyval.val) = (yyvsp[0].val);
		    }
#line 7425 "ripper.c"
    break;

  case 240:
#line 2380 "ripper.y"
                       {(yyval.val) = '>';}
#line 7431 "ripper.c"
    break;

  case 241:
#line 2381 "ripper.y"
                       {(yyval.val) = '<';}
#line 7437 "ripper.c"
    break;

  case 242:
#line 2382 "ripper.y"
                       {(yyval.val) = idGE;}
#line 7443 "ripper.c"
    break;

  case 243:
#line 2383 "ripper.y"
                       {(yyval.val) = idLE;}
#line 7449 "ripper.c"
    break;

  case 244:
#line 2387 "ripper.y"
                    {
			(yyval.val) = call_bin_op((yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7457 "ripper.c"
    break;

  case 245:
#line 2391 "ripper.y"
                    {
			rb_warning1("comparison '%s' after comparison", WARN_ID((yyvsp[-1].val)));
			(yyval.val) = call_bin_op((yyvsp[-2].val), (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-1]), &(yyloc));
		    }
#line 7466 "ripper.c"
    break;

  case 246:
#line 2398 "ripper.y"
                    {
#if 0
			value_expr((yyvsp[0].val));
			(yyval.val) = (yyvsp[0].val);
			if (!(yyval.val)) (yyval.val) = NEW_NIL();
#endif
			(yyval.val) = (yyvsp[0].val);

		    }
#line 7480 "ripper.c"
    break;

  case 248:
#line 2411 "ripper.y"
                    {
			(yyval.val) = (yyvsp[-1].val);
		    }
#line 7488 "ripper.c"
    break;

  case 249:
#line 2415 "ripper.y"
                    {
#if 0
			(yyval.val) = (yyvsp[-1].val) ? arg_append((yyvsp[-3].val), new_hash((yyvsp[-1].val), &(yylsp[-1])), &(yyloc)) : (yyvsp[-3].val);
#endif
			(yyval.val) = arg_add_assocs((yyvsp[-3].val), (yyvsp[-1].val));

		    }
#line 7500 "ripper.c"
    break;

  case 250:
#line 2423 "ripper.y"
                    {
#if 0
			(yyval.val) = (yyvsp[-1].val) ? new_list(new_hash((yyvsp[-1].val), &(yylsp[-1])), &(yyloc)) : 0;
#endif
			(yyval.val) = arg_add_assocs(arg_new(), (yyvsp[-1].val));

		    }
#line 7512 "ripper.c"
    break;

  case 251:
#line 2433 "ripper.y"
                    {
#if 0
			value_expr((yyvsp[0].val));
			(yyval.val) = (yyvsp[0].val);
#endif

		    }
#line 7524 "ripper.c"
    break;

  case 252:
#line 2441 "ripper.y"
                    {
#if 0
			YYLTYPE location;
			location.first_loc = (yylsp[-1]).first_loc;
			location.last_loc = (yylsp[0]).last_loc;
			value_expr((yyvsp[-2].val));
			(yyval.val) = new_rescue((yyvsp[-2].val), new_resbody(0, remove_begin((yyvsp[0].val)), 0, &location), 0, &(yyloc));
#endif
			(yyval.val) = dispatch2(rescue_mod, (yyvsp[-2].val), (yyvsp[0].val));

		    }
#line 7540 "ripper.c"
    break;

  case 253:
#line 2455 "ripper.y"
                    {
#if 0
			(yyval.val) = (yyvsp[-1].val);
#endif
			(yyval.val) = dispatch1(arg_paren, escape_Qundef((yyvsp[-1].val)));

		    }
#line 7552 "ripper.c"
    break;

  case 258:
#line 2471 "ripper.y"
                    {
		      (yyval.val) = (yyvsp[-1].val);
		    }
#line 7560 "ripper.c"
    break;

  case 259:
#line 2475 "ripper.y"
                    {
#if 0
			(yyval.val) = (yyvsp[-1].val) ? arg_append((yyvsp[-3].val), new_hash((yyvsp[-1].val), &(yylsp[-1])), &(yyloc)) : (yyvsp[-3].val);
#endif
			(yyval.val) = arg_add_assocs((yyvsp[-3].val), (yyvsp[-1].val));

		    }
#line 7572 "ripper.c"
    break;

  case 260:
#line 2483 "ripper.y"
                    {
#if 0
			(yyval.val) = (yyvsp[-1].val) ? new_list(new_hash((yyvsp[-1].val), &(yylsp[-1])), &(yylsp[-1])) : 0;
#endif
			(yyval.val) = arg_add_assocs(arg_new(), (yyvsp[-1].val));

		    }
#line 7584 "ripper.c"
    break;

  case 261:
#line 2493 "ripper.y"
                    {
#if 0
			value_expr((yyvsp[0].val));
			(yyval.val) = new_list((yyvsp[0].val), &(yyloc));
#endif
			(yyval.val) = arg_add(arg_new(), (yyvsp[0].val));

		    }
#line 7597 "ripper.c"
    break;

  case 262:
#line 2502 "ripper.y"
                    {
#if 0
			(yyval.val) = arg_blk_pass((yyvsp[-1].val), (yyvsp[0].val));
#endif
			(yyval.val) = arg_add_optblock((yyvsp[-1].val), (yyvsp[0].val));

		    }
#line 7609 "ripper.c"
    break;

  case 263:
#line 2510 "ripper.y"
                    {
#if 0
			(yyval.val) = (yyvsp[-1].val) ? new_list(new_hash((yyvsp[-1].val), &(yylsp[-1])), &(yylsp[-1])) : 0;
			(yyval.val) = arg_blk_pass((yyval.val), (yyvsp[0].val));
#endif
			(yyval.val) = arg_add_assocs(arg_new(), (yyvsp[-1].val));
			(yyval.val) = arg_add_optblock((yyval.val), (yyvsp[0].val));

		    }
#line 7623 "ripper.c"
    break;

  case 264:
#line 2520 "ripper.y"
                    {
#if 0
			(yyval.val) = (yyvsp[-1].val) ? arg_append((yyvsp[-3].val), new_hash((yyvsp[-1].val), &(yylsp[-1])), &(yyloc)) : (yyvsp[-3].val);
			(yyval.val) = arg_blk_pass((yyval.val), (yyvsp[0].val));
#endif
			(yyval.val) = arg_add_optblock(arg_add_assocs((yyvsp[-3].val), (yyvsp[-1].val)), (yyvsp[0].val));

		    }
#line 7636 "ripper.c"
    break;

  case 265:
#line 2531 "ripper.y"
                    {
			(yyval.val) = arg_add_block(arg_new(), (yyvsp[0].val));
		    }
#line 7644 "ripper.c"
    break;

  case 266:
#line 2537 "ripper.y"
                    {
			(yyval.val) = cmdarg_stack;
			CMDARG_PUSH(1);
		    }
#line 7653 "ripper.c"
    break;

  case 267:
#line 2542 "ripper.y"
                    {
			/* CMDARG_POP() */
			CMDARG_SET((yyvsp[-1].val));
			(yyval.val) = (yyvsp[0].val);
		    }
#line 7663 "ripper.c"
    break;

  case 268:
#line 2550 "ripper.y"
                    {
#if 0
			(yyval.val) = NEW_BLOCK_PASS((yyvsp[0].val));
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = (yyvsp[0].val);

		    }
#line 7676 "ripper.c"
    break;

  case 269:
#line 2561 "ripper.y"
                    {
			(yyval.val) = (yyvsp[0].val);
		    }
#line 7684 "ripper.c"
    break;

  case 270:
#line 2565 "ripper.y"
                    {
			(yyval.val) = 0;
		    }
#line 7692 "ripper.c"
    break;

  case 271:
#line 2571 "ripper.y"
                    {
#if 0
			(yyval.val) = new_list((yyvsp[0].val), &(yyloc));
#endif
			(yyval.val) = arg_add(arg_new(), (yyvsp[0].val));

		    }
#line 7704 "ripper.c"
    break;

  case 272:
#line 2579 "ripper.y"
                    {
#if 0
			(yyval.val) = NEW_SPLAT((yyvsp[0].val));
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = arg_add_star(arg_new(), (yyvsp[0].val));

		    }
#line 7717 "ripper.c"
    break;

  case 273:
#line 2588 "ripper.y"
                    {
#if 0
			NODE *n1;
			if ((n1 = splat_array((yyvsp[-2].val))) != 0) {
			    (yyval.val) = list_append(n1, (yyvsp[0].val));
			}
			else {
			    (yyval.val) = arg_append((yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
			}
#endif
			(yyval.val) = arg_add((yyvsp[-2].val), (yyvsp[0].val));

		    }
#line 7735 "ripper.c"
    break;

  case 274:
#line 2602 "ripper.y"
                    {
#if 0
			NODE *n1;
			if ((nd_type((yyvsp[0].val)) == NODE_ARRAY) && (n1 = splat_array((yyvsp[-3].val))) != 0) {
			    (yyval.val) = list_concat(n1, (yyvsp[0].val));
			}
			else {
			    (yyval.val) = arg_concat((yyvsp[-3].val), (yyvsp[0].val), &(yyloc));
			}
#endif
			(yyval.val) = arg_add_star((yyvsp[-3].val), (yyvsp[0].val));

		    }
#line 7753 "ripper.c"
    break;

  case 277:
#line 2622 "ripper.y"
                    {
#if 0
			NODE *n1;
			if ((n1 = splat_array((yyvsp[-2].val))) != 0) {
			    (yyval.val) = list_append(n1, (yyvsp[0].val));
			}
			else {
			    (yyval.val) = arg_append((yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
			}
#endif
			(yyval.val) = mrhs_add(args2mrhs((yyvsp[-2].val)), (yyvsp[0].val));

		    }
#line 7771 "ripper.c"
    break;

  case 278:
#line 2636 "ripper.y"
                    {
#if 0
			NODE *n1;
			if (nd_type((yyvsp[0].val)) == NODE_ARRAY &&
			    (n1 = splat_array((yyvsp[-3].val))) != 0) {
			    (yyval.val) = list_concat(n1, (yyvsp[0].val));
			}
			else {
			    (yyval.val) = arg_concat((yyvsp[-3].val), (yyvsp[0].val), &(yyloc));
			}
#endif
			(yyval.val) = mrhs_add_star(args2mrhs((yyvsp[-3].val)), (yyvsp[0].val));

		    }
#line 7790 "ripper.c"
    break;

  case 279:
#line 2651 "ripper.y"
                    {
#if 0
			(yyval.val) = NEW_SPLAT((yyvsp[0].val));
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = mrhs_add_star(mrhs_new(), (yyvsp[0].val));

		    }
#line 7803 "ripper.c"
    break;

  case 290:
#line 2672 "ripper.y"
                    {
#if 0
			(yyval.val) = new_fcall((yyvsp[0].val), 0, &(yyloc));
#endif
			(yyval.val) = method_arg(dispatch1(fcall, (yyvsp[0].val)), arg_new());

		    }
#line 7815 "ripper.c"
    break;

  case 291:
#line 2680 "ripper.y"
                    {
			(yyvsp[0].val) = cmdarg_stack;
			CMDARG_SET(0);
#if 0
			(yyval.num) = ruby_sourceline;
#endif

		    }
#line 7828 "ripper.c"
    break;

  case 292:
#line 2690 "ripper.y"
                    {
			CMDARG_SET((yyvsp[-3].val));
#if 0
			if ((yyvsp[-1].val) == NULL) {
			    (yyval.val) = NEW_NIL();
			    (yyval.val)->nd_loc = (yyloc);
			}
			else {
			    set_line_body((yyvsp[-1].val), (yyvsp[-2].num));
			    (yyval.val) = new_begin((yyvsp[-1].val), &(yyloc));
			}
			nd_set_line((yyval.val), (yyvsp[-2].num));
#endif
			(yyval.val) = dispatch1(begin, (yyvsp[-1].val));

		    }
#line 7849 "ripper.c"
    break;

  case 293:
#line 2706 "ripper.y"
                              {SET_LEX_STATE(EXPR_ENDARG);}
#line 7855 "ripper.c"
    break;

  case 294:
#line 2707 "ripper.y"
                    {
#if 0
			(yyval.val) = new_begin(0, &(yyloc));
#endif
			(yyval.val) = dispatch1(paren, 0);

		    }
#line 7867 "ripper.c"
    break;

  case 295:
#line 2715 "ripper.y"
                    {
			(yyvsp[0].val) = cmdarg_stack;
			CMDARG_SET(0);
		    }
#line 7876 "ripper.c"
    break;

  case 296:
#line 2719 "ripper.y"
                       {SET_LEX_STATE(EXPR_ENDARG);}
#line 7882 "ripper.c"
    break;

  case 297:
#line 2720 "ripper.y"
                    {
			CMDARG_SET((yyvsp[-4].val));
#if 0
			(yyval.val) = (yyvsp[-2].val);
#endif
			(yyval.val) = dispatch1(paren, (yyvsp[-2].val));

		    }
#line 7895 "ripper.c"
    break;

  case 298:
#line 2729 "ripper.y"
                    {
#if 0
			(yyval.val) = (yyvsp[-1].val);
#endif
			(yyval.val) = dispatch1(paren, (yyvsp[-1].val));

		    }
#line 7907 "ripper.c"
    break;

  case 299:
#line 2737 "ripper.y"
                    {
#if 0
			(yyval.val) = NEW_COLON2((yyvsp[-2].val), (yyvsp[0].val));
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch2(const_path_ref, (yyvsp[-2].val), (yyvsp[0].val));

		    }
#line 7920 "ripper.c"
    break;

  case 300:
#line 2746 "ripper.y"
                    {
#if 0
			(yyval.val) = NEW_COLON3((yyvsp[0].val));
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch1(top_const_ref, (yyvsp[0].val));

		    }
#line 7933 "ripper.c"
    break;

  case 301:
#line 2755 "ripper.y"
                    {
#if 0
			(yyval.val) = make_array((yyvsp[-1].val), &(yyloc));
#endif
			(yyval.val) = dispatch1(array, escape_Qundef((yyvsp[-1].val)));

		    }
#line 7945 "ripper.c"
    break;

  case 302:
#line 2763 "ripper.y"
                    {
#if 0
			(yyval.val) = new_hash((yyvsp[-1].val), &(yyloc));
			(yyval.val)->nd_alen = TRUE;
#endif
			(yyval.val) = dispatch1(hash, escape_Qundef((yyvsp[-1].val)));

		    }
#line 7958 "ripper.c"
    break;

  case 303:
#line 2772 "ripper.y"
                    {
#if 0
			(yyval.val) = NEW_RETURN(0);
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch0(return0);

		    }
#line 7971 "ripper.c"
    break;

  case 304:
#line 2781 "ripper.y"
                    {
#if 0
			(yyval.val) = new_yield((yyvsp[-1].val), &(yyloc));
#endif
			(yyval.val) = dispatch1(yield, dispatch1(paren, (yyvsp[-1].val)));

		    }
#line 7983 "ripper.c"
    break;

  case 305:
#line 2789 "ripper.y"
                    {
#if 0
			(yyval.val) = NEW_YIELD(0);
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch1(yield, dispatch1(paren, arg_new()));

		    }
#line 7996 "ripper.c"
    break;

  case 306:
#line 2798 "ripper.y"
                    {
#if 0
			(yyval.val) = NEW_YIELD(0);
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch0(yield0);

		    }
#line 8009 "ripper.c"
    break;

  case 307:
#line 2806 "ripper.y"
                                             {in_defined = 1;}
#line 8015 "ripper.c"
    break;

  case 308:
#line 2807 "ripper.y"
                    {
			in_defined = 0;
			(yyval.val) = new_defined((yyvsp[-1].val), &(yyloc));
		    }
#line 8024 "ripper.c"
    break;

  case 309:
#line 2812 "ripper.y"
                    {
			(yyval.val) = call_uni_op(method_cond((yyvsp[-1].val), &(yylsp[-1])), METHOD_NOT, &(yylsp[-3]), &(yyloc));
		    }
#line 8032 "ripper.c"
    break;

  case 310:
#line 2816 "ripper.y"
                    {
			(yyval.val) = call_uni_op(method_cond(new_nil(&(yylsp[-1])), &(yylsp[-1])), METHOD_NOT, &(yylsp[-2]), &(yyloc));
		    }
#line 8040 "ripper.c"
    break;

  case 311:
#line 2820 "ripper.y"
                    {
#if 0
			(yyvsp[0].val)->nd_iter = (yyvsp[-1].val);
			(yyvsp[0].val)->nd_loc = (yyloc);
			(yyval.val) = (yyvsp[0].val);
#endif
			(yyval.val) = method_arg(dispatch1(fcall, (yyvsp[-1].val)), arg_new());
			(yyval.val) = method_add_block((yyval.val), (yyvsp[0].val));

		    }
#line 8055 "ripper.c"
    break;

  case 313:
#line 2832 "ripper.y"
                    {
#if 0
			block_dup_check((yyvsp[-1].val)->nd_args, (yyvsp[0].val));
			(yyvsp[0].val)->nd_iter = (yyvsp[-1].val);
			(yyvsp[0].val)->nd_loc = (yyloc);
			(yyval.val) = (yyvsp[0].val);
#endif
			(yyval.val) = method_add_block((yyvsp[-1].val), (yyvsp[0].val));

		    }
#line 8070 "ripper.c"
    break;

  case 314:
#line 2843 "ripper.y"
                    {
			(yyval.val) = (yyvsp[0].val);
		    }
#line 8078 "ripper.c"
    break;

  case 315:
#line 2850 "ripper.y"
                    {
#if 0
			(yyval.val) = new_if((yyvsp[-4].val), (yyvsp[-2].val), (yyvsp[-1].val), &(yyloc));
			fixpos((yyval.val), (yyvsp[-4].val));
#endif
			(yyval.val) = dispatch3(if, (yyvsp[-4].val), (yyvsp[-2].val), escape_Qundef((yyvsp[-1].val)));

		    }
#line 8091 "ripper.c"
    break;

  case 316:
#line 2862 "ripper.y"
                    {
#if 0
			(yyval.val) = new_unless((yyvsp[-4].val), (yyvsp[-2].val), (yyvsp[-1].val), &(yyloc));
			fixpos((yyval.val), (yyvsp[-4].val));
#endif
			(yyval.val) = dispatch3(unless, (yyvsp[-4].val), (yyvsp[-2].val), escape_Qundef((yyvsp[-1].val)));

		    }
#line 8104 "ripper.c"
    break;

  case 317:
#line 2870 "ripper.y"
                          {COND_PUSH(1);}
#line 8110 "ripper.c"
    break;

  case 318:
#line 2870 "ripper.y"
                                                        {COND_POP();}
#line 8116 "ripper.c"
    break;

  case 319:
#line 2873 "ripper.y"
                    {
#if 0
			(yyval.val) = NEW_WHILE(cond((yyvsp[-4].val), &(yylsp[-4])), (yyvsp[-1].val), 1);
			fixpos((yyval.val), (yyvsp[-4].val));
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch2(while, (yyvsp[-4].val), (yyvsp[-1].val));

		    }
#line 8130 "ripper.c"
    break;

  case 320:
#line 2882 "ripper.y"
                          {COND_PUSH(1);}
#line 8136 "ripper.c"
    break;

  case 321:
#line 2882 "ripper.y"
                                                        {COND_POP();}
#line 8142 "ripper.c"
    break;

  case 322:
#line 2885 "ripper.y"
                    {
#if 0
			(yyval.val) = NEW_UNTIL(cond((yyvsp[-4].val), &(yylsp[-4])), (yyvsp[-1].val), 1);
			fixpos((yyval.val), (yyvsp[-4].val));
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch2(until, (yyvsp[-4].val), (yyvsp[-1].val));

		    }
#line 8156 "ripper.c"
    break;

  case 323:
#line 2897 "ripper.y"
                    {
#if 0
			(yyval.val) = NEW_CASE((yyvsp[-3].val), (yyvsp[-1].val));
			fixpos((yyval.val), (yyvsp[-3].val));
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch2(case, (yyvsp[-3].val), (yyvsp[-1].val));

		    }
#line 8170 "ripper.c"
    break;

  case 324:
#line 2907 "ripper.y"
                    {
#if 0
			(yyval.val) = NEW_CASE2((yyvsp[-1].val));
			nd_set_line((yyvsp[-1].val), (yyvsp[-3].num));
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch2(case, Qnil, (yyvsp[-1].val));

		    }
#line 8184 "ripper.c"
    break;

  case 325:
#line 2917 "ripper.y"
                  {COND_PUSH(1);}
#line 8190 "ripper.c"
    break;

  case 326:
#line 2919 "ripper.y"
                  {COND_POP();}
#line 8196 "ripper.c"
    break;

  case 327:
#line 2922 "ripper.y"
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
			ID id = internal_id();
			ID *tbl = ALLOC_N(ID, 2);
			NODE *m = NEW_ARGS_AUX(0, 0);
			NODE *args, *scope;

			switch (nd_type((yyvsp[-7].val))) {
			  case NODE_MASGN:
			    m->nd_next = node_assign((yyvsp[-7].val), new_for(new_dvar(id, &(yylsp[-7])), 0, 0, &(yylsp[-7])), &(yylsp[-7]));
			    args = new_args(m, 0, id, 0, new_args_tail(0, 0, 0, &(yylsp[-7])), &(yylsp[-7]));
			    break;
			  case NODE_LASGN:
			  case NODE_DASGN:
			  case NODE_DASGN_CURR:
			    (yyvsp[-7].val)->nd_value = new_dvar(id, &(yylsp[-7]));
			    m->nd_plen = 1;
			    m->nd_next = (yyvsp[-7].val);
			    args = new_args(m, 0, 0, 0, new_args_tail(0, 0, 0, &(yylsp[-7])), &(yylsp[-7]));
			    break;
			  default:
			    {
				NODE *masgn = new_masgn(new_list((yyvsp[-7].val), &(yylsp[-7])), 0, &(yylsp[-7]));
				m->nd_next = node_assign(masgn, new_dvar(id, &(yylsp[-7])), &(yylsp[-7]));
				args = new_args(m, 0, id, 0, new_args_tail(0, 0, 0, &(yylsp[-7])), &(yylsp[-7]));
				break;
			    }
			}
			add_mark_object((VALUE)rb_imemo_alloc_new((VALUE)tbl, 0, 0, 0));
			scope = NEW_NODE(NODE_SCOPE, tbl, (yyvsp[-1].val), args);
			scope->nd_loc = (yyloc);
			tbl[0] = 1; tbl[1] = id;
			(yyval.val) = new_for(0, (yyvsp[-4].val), scope, &(yyloc));
			fixpos((yyval.val), (yyvsp[-7].val));
#endif
			(yyval.val) = dispatch3(for, (yyvsp[-7].val), (yyvsp[-4].val), (yyvsp[-1].val));

		    }
#line 8248 "ripper.c"
    break;

  case 328:
#line 2970 "ripper.y"
                    {
			if (in_def)
			    yyerror0("class definition in method body");
			(yyvsp[-2].num) = in_class;
			in_class = 1;
			local_push(0);
#if 0
			(yyval.num) = ruby_sourceline;
#endif

		    }
#line 8264 "ripper.c"
    break;

  case 329:
#line 2983 "ripper.y"
                    {
#if 0
			(yyval.val) = NEW_CLASS((yyvsp[-4].val), (yyvsp[-1].val), (yyvsp[-3].val));
			(yyval.val)->nd_body->nd_loc = (yyloc);
			set_line_body((yyvsp[-1].val), (yyvsp[-2].num));
			nd_set_line((yyval.val), (yyvsp[-2].num));
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch3(class, (yyvsp[-4].val), (yyvsp[-3].val), (yyvsp[-1].val));

			local_pop();
			in_class = (yyvsp[-5].num) & 1;
		    }
#line 8282 "ripper.c"
    break;

  case 330:
#line 2997 "ripper.y"
                    {
			(yyval.num) = (in_class << 1) | in_def;
			in_def = 0;
			in_class = 0;
			local_push(0);
		    }
#line 8293 "ripper.c"
    break;

  case 331:
#line 3006 "ripper.y"
                    {
#if 0
			(yyval.val) = NEW_SCLASS((yyvsp[-4].val), (yyvsp[-1].val));
			(yyval.val)->nd_body->nd_loc = (yyloc);
			set_line_body((yyvsp[-1].val), nd_line((yyvsp[-4].val)));
			fixpos((yyval.val), (yyvsp[-4].val));
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch2(sclass, (yyvsp[-4].val), (yyvsp[-1].val));

			local_pop();
			in_def = (yyvsp[-3].num) & 1;
			in_class = ((yyvsp[-3].num) >> 1) & 1;
		    }
#line 8312 "ripper.c"
    break;

  case 332:
#line 3021 "ripper.y"
                    {
			if (in_def)
			    yyerror0("module definition in method body");
			(yyvsp[-1].num) = in_class;
			in_class = 1;
			local_push(0);
#if 0
			(yyval.num) = ruby_sourceline;
#endif

		    }
#line 8328 "ripper.c"
    break;

  case 333:
#line 3034 "ripper.y"
                    {
#if 0
			(yyval.val) = NEW_MODULE((yyvsp[-3].val), (yyvsp[-1].val));
			(yyval.val)->nd_body->nd_loc = (yyloc);
			set_line_body((yyvsp[-1].val), (yyvsp[-2].num));
			nd_set_line((yyval.val), (yyvsp[-2].num));
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch2(module, (yyvsp[-3].val), (yyvsp[-1].val));

			local_pop();
			in_class = (yyvsp[-4].num) & 1;
		    }
#line 8346 "ripper.c"
    break;

  case 334:
#line 3048 "ripper.y"
                    {
			local_push(0);
			(yyval.id) = current_arg;
			current_arg = 0;
		    }
#line 8356 "ripper.c"
    break;

  case 335:
#line 3053 "ripper.y"
                    {
			(yyval.num) = in_def;
			in_def = 1;
		    }
#line 8365 "ripper.c"
    break;

  case 336:
#line 3060 "ripper.y"
                    {
#if 0
			NODE *body = remove_begin((yyvsp[-1].val));
			reduce_nodes(&body);
			(yyval.val) = NEW_DEFN((yyvsp[-5].val), (yyvsp[-2].val), body, METHOD_VISI_PRIVATE);
			(yyval.val)->nd_defn->nd_loc = (yyloc);
			set_line_body(body, (yyvsp[-6].num));
			nd_set_line((yyval.val), (yyvsp[-6].num));
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch3(def, (yyvsp[-5].val), (yyvsp[-2].val), (yyvsp[-1].val));

			local_pop();
			in_def = (yyvsp[-3].num) & 1;
			current_arg = (yyvsp[-4].id);
		    }
#line 8386 "ripper.c"
    break;

  case 337:
#line 3076 "ripper.y"
                                               {SET_LEX_STATE(EXPR_FNAME);}
#line 8392 "ripper.c"
    break;

  case 338:
#line 3077 "ripper.y"
                    {
			(yyvsp[-1].num) = in_def;
			in_def = 1;
			SET_LEX_STATE(EXPR_ENDFN|EXPR_LABEL); /* force for args */
			local_push(0);
			(yyval.id) = current_arg;
			current_arg = 0;
		    }
#line 8405 "ripper.c"
    break;

  case 339:
#line 3088 "ripper.y"
                    {
#if 0
			NODE *body = remove_begin((yyvsp[-1].val));
			reduce_nodes(&body);
			(yyval.val) = NEW_DEFS((yyvsp[-7].val), (yyvsp[-4].val), (yyvsp[-2].val), body);
			(yyval.val)->nd_defn->nd_loc = (yyloc);
			set_line_body(body, (yyvsp[-8].num));
			nd_set_line((yyval.val), (yyvsp[-8].num));
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch5(defs, (yyvsp[-7].val), (yyvsp[-6].val), (yyvsp[-4].val), (yyvsp[-2].val), (yyvsp[-1].val));

			local_pop();
			in_def = (yyvsp[-5].num) & 1;
			current_arg = (yyvsp[-3].id);
		    }
#line 8426 "ripper.c"
    break;

  case 340:
#line 3105 "ripper.y"
                    {
#if 0
			(yyval.val) = NEW_BREAK(0);
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch1(break, arg_new());

		    }
#line 8439 "ripper.c"
    break;

  case 341:
#line 3114 "ripper.y"
                    {
#if 0
			(yyval.val) = NEW_NEXT(0);
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch1(next, arg_new());

		    }
#line 8452 "ripper.c"
    break;

  case 342:
#line 3123 "ripper.y"
                    {
#if 0
			(yyval.val) = NEW_REDO();
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch0(redo);

		    }
#line 8465 "ripper.c"
    break;

  case 343:
#line 3132 "ripper.y"
                    {
#if 0
			(yyval.val) = NEW_RETRY();
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch0(retry);

		    }
#line 8478 "ripper.c"
    break;

  case 344:
#line 3143 "ripper.y"
                    {
#if 0
			value_expr((yyvsp[0].val));
			(yyval.val) = (yyvsp[0].val);
			if (!(yyval.val)) (yyval.val) = NEW_NIL();
#endif
			(yyval.val) = (yyvsp[0].val);

		    }
#line 8492 "ripper.c"
    break;

  case 345:
#line 3155 "ripper.y"
                    {
			token_info_push("begin");
		    }
#line 8500 "ripper.c"
    break;

  case 346:
#line 3161 "ripper.y"
                    {
			token_info_push("if");
		    }
#line 8508 "ripper.c"
    break;

  case 347:
#line 3167 "ripper.y"
                    {
			token_info_push("unless");
		    }
#line 8516 "ripper.c"
    break;

  case 348:
#line 3173 "ripper.y"
                    {
			token_info_push("while");
		    }
#line 8524 "ripper.c"
    break;

  case 349:
#line 3179 "ripper.y"
                    {
			token_info_push("until");
		    }
#line 8532 "ripper.c"
    break;

  case 350:
#line 3185 "ripper.y"
                    {
			token_info_push("case");
#if 0
			(yyval.num) = ruby_sourceline;
#endif

		    }
#line 8544 "ripper.c"
    break;

  case 351:
#line 3195 "ripper.y"
                    {
			token_info_push("for");
		    }
#line 8552 "ripper.c"
    break;

  case 352:
#line 3201 "ripper.y"
                    {
			token_info_push("class");
		    }
#line 8560 "ripper.c"
    break;

  case 353:
#line 3207 "ripper.y"
                    {
			token_info_push("module");
		    }
#line 8568 "ripper.c"
    break;

  case 354:
#line 3213 "ripper.y"
                    {
			token_info_push("def");
#if 0
			(yyval.num) = ruby_sourceline;
#endif

		    }
#line 8580 "ripper.c"
    break;

  case 355:
#line 3223 "ripper.y"
                    {
			token_info_pop("end");
		    }
#line 8588 "ripper.c"
    break;

  case 356:
#line 3229 "ripper.y"
                    {
			if (in_class && !in_def && !dyna_in_block())
			    yyerror0("Invalid return in class/module body");
		    }
#line 8597 "ripper.c"
    break;

  case 357:
#line 3238 "ripper.y"
                    { (yyval.val) = Qnil; }
#line 8603 "ripper.c"
    break;

  case 359:
#line 3244 "ripper.y"
                    { (yyval.val) = (yyvsp[0].val); }
#line 8609 "ripper.c"
    break;

  case 360:
#line 3251 "ripper.y"
                    { (yyval.val) = Qnil; }
#line 8615 "ripper.c"
    break;

  case 363:
#line 3260 "ripper.y"
                    {
#if 0
			(yyval.val) = new_if((yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
			fixpos((yyval.val), (yyvsp[-3].val));
#endif
			(yyval.val) = dispatch3(elsif, (yyvsp[-3].val), (yyvsp[-1].val), escape_Qundef((yyvsp[0].val)));

		    }
#line 8628 "ripper.c"
    break;

  case 365:
#line 3272 "ripper.y"
                    {
#if 0
			(yyval.val) = (yyvsp[0].val);
#endif
			(yyval.val) = dispatch1(else, (yyvsp[0].val));

		    }
#line 8640 "ripper.c"
    break;

  case 368:
#line 3286 "ripper.y"
                    {
			(yyval.val) = assignable((yyvsp[0].val), 0, &(yyloc));
#if 0
#endif

		    }
#line 8651 "ripper.c"
    break;

  case 369:
#line 3293 "ripper.y"
                    {
#if 0
			(yyval.val) = (yyvsp[-1].val);
#endif
			(yyval.val) = dispatch1(mlhs_paren, (yyvsp[-1].val));

		    }
#line 8663 "ripper.c"
    break;

  case 370:
#line 3303 "ripper.y"
                    {
#if 0
			(yyval.val) = new_list((yyvsp[0].val), &(yyloc));
#endif
			(yyval.val) = mlhs_add(mlhs_new(), (yyvsp[0].val));

		    }
#line 8675 "ripper.c"
    break;

  case 371:
#line 3311 "ripper.y"
                    {
#if 0
			(yyval.val) = list_append((yyvsp[-2].val), (yyvsp[0].val));
#endif
			(yyval.val) = mlhs_add((yyvsp[-2].val), (yyvsp[0].val));

		    }
#line 8687 "ripper.c"
    break;

  case 372:
#line 3321 "ripper.y"
                    {
#if 0
			(yyval.val) = new_masgn((yyvsp[0].val), 0, &(yyloc));
#endif
			(yyval.val) = (yyvsp[0].val);

		    }
#line 8699 "ripper.c"
    break;

  case 373:
#line 3329 "ripper.y"
                    {
			(yyval.val) = assignable((yyvsp[0].val), 0, &(yyloc));
#if 0
			(yyval.val) = new_masgn((yyvsp[-3].val), (yyval.val), &(yyloc));
#endif
			(yyval.val) = mlhs_add_star((yyvsp[-3].val), (yyval.val));

		    }
#line 8712 "ripper.c"
    break;

  case 374:
#line 3338 "ripper.y"
                    {
			(yyval.val) = assignable((yyvsp[-2].val), 0, &(yyloc));
#if 0
			(yyval.val) = new_masgn((yyvsp[-5].val), new_postarg((yyval.val), (yyvsp[0].val), &(yyloc)), &(yyloc));
#endif
			(yyval.val) = mlhs_add_star((yyvsp[-5].val), (yyval.val));
			(yyval.val) = mlhs_add_post((yyval.val), (yyvsp[0].val));

		    }
#line 8726 "ripper.c"
    break;

  case 375:
#line 3348 "ripper.y"
                    {
#if 0
			(yyval.val) = new_masgn((yyvsp[-2].val), NODE_SPECIAL_NO_NAME_REST, &(yyloc));
#endif
			(yyval.val) = mlhs_add_star((yyvsp[-2].val), Qnil);

		    }
#line 8738 "ripper.c"
    break;

  case 376:
#line 3356 "ripper.y"
                    {
#if 0
			(yyval.val) = new_masgn((yyvsp[-4].val), new_postarg(NODE_SPECIAL_NO_NAME_REST, (yyvsp[0].val), &(yyloc)), &(yyloc));
#endif
			(yyval.val) = mlhs_add_star((yyvsp[-4].val), Qnil);
			(yyval.val) = mlhs_add_post((yyval.val), (yyvsp[0].val));

		    }
#line 8751 "ripper.c"
    break;

  case 377:
#line 3365 "ripper.y"
                    {
			(yyval.val) = assignable((yyvsp[0].val), 0, &(yyloc));
#if 0
			(yyval.val) = new_masgn(0, (yyval.val), &(yyloc));
#endif
			(yyval.val) = mlhs_add_star(mlhs_new(), (yyval.val));

		    }
#line 8764 "ripper.c"
    break;

  case 378:
#line 3374 "ripper.y"
                    {
			(yyval.val) = assignable((yyvsp[-2].val), 0, &(yyloc));
#if 0
			(yyval.val) = new_masgn(0, new_postarg((yyval.val), (yyvsp[0].val), &(yyloc)), &(yyloc));
#endif
			(yyval.val) = mlhs_add_star(mlhs_new(), (yyval.val));
			(yyval.val) = mlhs_add_post((yyval.val), (yyvsp[0].val));

		    }
#line 8778 "ripper.c"
    break;

  case 379:
#line 3384 "ripper.y"
                    {
#if 0
			(yyval.val) = new_masgn(0, NODE_SPECIAL_NO_NAME_REST, &(yyloc));
#endif
			(yyval.val) = mlhs_add_star(mlhs_new(), Qnil);

		    }
#line 8790 "ripper.c"
    break;

  case 380:
#line 3392 "ripper.y"
                    {
#if 0
			(yyval.val) = new_masgn(0, new_postarg(NODE_SPECIAL_NO_NAME_REST, (yyvsp[0].val), &(yyloc)), &(yyloc));
#endif
			(yyval.val) = mlhs_add_star(mlhs_new(), Qnil);
			(yyval.val) = mlhs_add_post((yyval.val), (yyvsp[0].val));

		    }
#line 8803 "ripper.c"
    break;

  case 381:
#line 3404 "ripper.y"
                    {
			(yyval.val) = new_args_tail((yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-1]));
		    }
#line 8811 "ripper.c"
    break;

  case 382:
#line 3408 "ripper.y"
                    {
			(yyval.val) = new_args_tail((yyvsp[-1].val), Qnone, (yyvsp[0].val), &(yylsp[-1]));
		    }
#line 8819 "ripper.c"
    break;

  case 383:
#line 3412 "ripper.y"
                    {
			(yyval.val) = new_args_tail(Qnone, (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-1]));
		    }
#line 8827 "ripper.c"
    break;

  case 384:
#line 3416 "ripper.y"
                    {
			(yyval.val) = new_args_tail(Qnone, Qnone, (yyvsp[0].val), &(yylsp[0]));
		    }
#line 8835 "ripper.c"
    break;

  case 385:
#line 3422 "ripper.y"
                    {
			(yyval.val) = (yyvsp[0].val);
		    }
#line 8843 "ripper.c"
    break;

  case 386:
#line 3426 "ripper.y"
                    {
			(yyval.val) = new_args_tail(Qnone, Qnone, Qnone, &(yylsp[0]));
		    }
#line 8851 "ripper.c"
    break;

  case 387:
#line 3432 "ripper.y"
                    {
			(yyval.val) = new_args((yyvsp[-5].val), (yyvsp[-3].val), (yyvsp[-1].val), Qnone, (yyvsp[0].val), &(yyloc));
		    }
#line 8859 "ripper.c"
    break;

  case 388:
#line 3436 "ripper.y"
                    {
			(yyval.val) = new_args((yyvsp[-7].val), (yyvsp[-5].val), (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 8867 "ripper.c"
    break;

  case 389:
#line 3440 "ripper.y"
                    {
			(yyval.val) = new_args((yyvsp[-3].val), (yyvsp[-1].val), Qnone, Qnone, (yyvsp[0].val), &(yyloc));
		    }
#line 8875 "ripper.c"
    break;

  case 390:
#line 3444 "ripper.y"
                    {
			(yyval.val) = new_args((yyvsp[-5].val), (yyvsp[-3].val), Qnone, (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 8883 "ripper.c"
    break;

  case 391:
#line 3448 "ripper.y"
                    {
			(yyval.val) = new_args((yyvsp[-3].val), Qnone, (yyvsp[-1].val), Qnone, (yyvsp[0].val), &(yyloc));
		    }
#line 8891 "ripper.c"
    break;

  case 392:
#line 3452 "ripper.y"
                    {
			(yyval.val) = new_args((yyvsp[-1].val), Qnone, 1, Qnone, new_args_tail(Qnone, Qnone, Qnone, &(yylsp[-1])), &(yyloc));
#if 0
#endif
                        dispatch1(excessed_comma, (yyval.val));

		    }
#line 8903 "ripper.c"
    break;

  case 393:
#line 3460 "ripper.y"
                    {
			(yyval.val) = new_args((yyvsp[-5].val), Qnone, (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 8911 "ripper.c"
    break;

  case 394:
#line 3464 "ripper.y"
                    {
			(yyval.val) = new_args((yyvsp[-1].val), Qnone, Qnone, Qnone, (yyvsp[0].val), &(yyloc));
		    }
#line 8919 "ripper.c"
    break;

  case 395:
#line 3468 "ripper.y"
                    {
			(yyval.val) = new_args(Qnone, (yyvsp[-3].val), (yyvsp[-1].val), Qnone, (yyvsp[0].val), &(yyloc));
		    }
#line 8927 "ripper.c"
    break;

  case 396:
#line 3472 "ripper.y"
                    {
			(yyval.val) = new_args(Qnone, (yyvsp[-5].val), (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 8935 "ripper.c"
    break;

  case 397:
#line 3476 "ripper.y"
                    {
			(yyval.val) = new_args(Qnone, (yyvsp[-1].val), Qnone, Qnone, (yyvsp[0].val), &(yyloc));
		    }
#line 8943 "ripper.c"
    break;

  case 398:
#line 3480 "ripper.y"
                    {
			(yyval.val) = new_args(Qnone, (yyvsp[-3].val), Qnone, (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 8951 "ripper.c"
    break;

  case 399:
#line 3484 "ripper.y"
                    {
			(yyval.val) = new_args(Qnone, Qnone, (yyvsp[-1].val), Qnone, (yyvsp[0].val), &(yyloc));
		    }
#line 8959 "ripper.c"
    break;

  case 400:
#line 3488 "ripper.y"
                    {
			(yyval.val) = new_args(Qnone, Qnone, (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 8967 "ripper.c"
    break;

  case 401:
#line 3492 "ripper.y"
                    {
			(yyval.val) = new_args(Qnone, Qnone, Qnone, Qnone, (yyvsp[0].val), &(yyloc));
		    }
#line 8975 "ripper.c"
    break;

  case 403:
#line 3499 "ripper.y"
                    {
			command_start = TRUE;
		    }
#line 8983 "ripper.c"
    break;

  case 404:
#line 3505 "ripper.y"
                    {
			current_arg = 0;
#if 0
			(yyval.val) = 0;
#endif
			(yyval.val) = blockvar_new(params_new(Qnil,Qnil,Qnil,Qnil,Qnil,Qnil,Qnil),
                                          escape_Qundef((yyvsp[-1].val)));

		    }
#line 8997 "ripper.c"
    break;

  case 405:
#line 3515 "ripper.y"
                    {
#if 0
			(yyval.val) = 0;
#endif
			(yyval.val) = blockvar_new(params_new(Qnil,Qnil,Qnil,Qnil,Qnil,Qnil,Qnil),
                                          Qnil);

		    }
#line 9010 "ripper.c"
    break;

  case 406:
#line 3524 "ripper.y"
                    {
			current_arg = 0;
#if 0
			(yyval.val) = (yyvsp[-2].val);
#endif
			(yyval.val) = blockvar_new(escape_Qundef((yyvsp[-2].val)), escape_Qundef((yyvsp[-1].val)));

		    }
#line 9023 "ripper.c"
    break;

  case 407:
#line 3536 "ripper.y"
                    {
		      (yyval.val) = 0;
		    }
#line 9031 "ripper.c"
    break;

  case 408:
#line 3540 "ripper.y"
                    {
#if 0
			(yyval.val) = 0;
#endif
			(yyval.val) = (yyvsp[-1].val);

		    }
#line 9043 "ripper.c"
    break;

  case 409:
#line 3552 "ripper.y"
                    {
			(yyval.val) = rb_ary_new3(1, get_value((yyvsp[0].val)));
		    }
#line 9051 "ripper.c"
    break;

  case 410:
#line 3559 "ripper.y"
                    {
			rb_ary_push((yyvsp[-2].val), get_value((yyvsp[0].val)));
		    }
#line 9059 "ripper.c"
    break;

  case 411:
#line 3566 "ripper.y"
                    {
			new_bv(get_id((yyvsp[0].val)));
#if 0
#endif
			(yyval.val) = get_value((yyvsp[0].val));

		    }
#line 9071 "ripper.c"
    break;

  case 412:
#line 3574 "ripper.y"
                    {
			(yyval.val) = 0;
		    }
#line 9079 "ripper.c"
    break;

  case 413:
#line 3579 "ripper.y"
                    {
			(yyval.vars) = dyna_push();
		    }
#line 9087 "ripper.c"
    break;

  case 414:
#line 3582 "ripper.y"
                    {
			(yyval.num) = lpar_beg;
			lpar_beg = ++paren_nest;
		    }
#line 9096 "ripper.c"
    break;

  case 415:
#line 3587 "ripper.y"
                    {
			(yyval.num) = ruby_sourceline;
		    }
#line 9104 "ripper.c"
    break;

  case 416:
#line 3590 "ripper.y"
                    {
			(yyval.val) = cmdarg_stack;
			CMDARG_SET(0);
		    }
#line 9113 "ripper.c"
    break;

  case 417:
#line 3595 "ripper.y"
                    {
			lpar_beg = (yyvsp[-4].num);
			CMDARG_SET((yyvsp[-1].val));
			CMDARG_LEXPOP();
#if 0
			(yyval.val) = NEW_LAMBDA((yyvsp[-3].val), (yyvsp[0].val));
			nd_set_line((yyval.val), (yyvsp[-2].num));
			(yyval.val)->nd_loc = (yyloc);
			(yyval.val)->nd_body->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch2(lambda, (yyvsp[-3].val), (yyvsp[0].val));

			dyna_pop((yyvsp[-5].vars));
		    }
#line 9132 "ripper.c"
    break;

  case 418:
#line 3612 "ripper.y"
                    {
#if 0
			(yyval.val) = (yyvsp[-2].val);
#endif
			(yyval.val) = dispatch1(paren, (yyvsp[-2].val));

		    }
#line 9144 "ripper.c"
    break;

  case 419:
#line 3620 "ripper.y"
                    {
			(yyval.val) = (yyvsp[0].val);
		    }
#line 9152 "ripper.c"
    break;

  case 420:
#line 3626 "ripper.y"
                    {
			token_info_pop("}");
			(yyval.val) = (yyvsp[-1].val);
		    }
#line 9161 "ripper.c"
    break;

  case 421:
#line 3631 "ripper.y"
                    {
			(yyval.val) = (yyvsp[-1].val);
		    }
#line 9169 "ripper.c"
    break;

  case 422:
#line 3637 "ripper.y"
                    {
#if 0
			(yyval.num) = ruby_sourceline;
#endif
		    }
#line 9179 "ripper.c"
    break;

  case 423:
#line 3643 "ripper.y"
                    {
			(yyval.val) = (yyvsp[-1].val);
#if 0
			(yyvsp[-1].val)->nd_body->nd_loc.first_loc = (yylsp[-3]).first_loc;
			(yyvsp[-1].val)->nd_body->nd_loc.last_loc = (yylsp[0]).last_loc;
			nd_set_line((yyval.val), (yyvsp[-2].num));
#endif
		    }
#line 9192 "ripper.c"
    break;

  case 424:
#line 3654 "ripper.y"
                    {
#if 0
			if (nd_type((yyvsp[-1].val)) == NODE_YIELD) {
			    compile_error(PARSER_ARG "block given to yield");
			}
			else {
			    block_dup_check((yyvsp[-1].val)->nd_args, (yyvsp[0].val));
			}
			(yyvsp[0].val)->nd_iter = (yyvsp[-1].val);
			(yyvsp[0].val)->nd_loc = (yyloc);
			(yyval.val) = (yyvsp[0].val);
			fixpos((yyval.val), (yyvsp[-1].val));
#endif
			(yyval.val) = method_add_block((yyvsp[-1].val), (yyvsp[0].val));

		    }
#line 9213 "ripper.c"
    break;

  case 425:
#line 3671 "ripper.y"
                    {
			(yyval.val) = new_qcall((yyvsp[-2].val), (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 9221 "ripper.c"
    break;

  case 426:
#line 3675 "ripper.y"
                    {
#if 0
			block_dup_check((yyvsp[-1].val), (yyvsp[0].val));
			(yyvsp[0].val)->nd_iter = new_command_qcall((yyvsp[-3].val), (yyvsp[-4].val), (yyvsp[-2].val), (yyvsp[-1].val), &(yyloc));
			(yyvsp[0].val)->nd_loc = (yyloc);
			(yyval.val) = (yyvsp[0].val);
			fixpos((yyval.val), (yyvsp[-4].val));
#endif
			(yyval.val) = dispatch4(command_call, (yyvsp[-4].val), (yyvsp[-3].val), (yyvsp[-2].val), (yyvsp[-1].val));
			(yyval.val) = method_add_block((yyval.val), (yyvsp[0].val));

		    }
#line 9238 "ripper.c"
    break;

  case 427:
#line 3688 "ripper.y"
                    {
#if 0
			block_dup_check((yyvsp[-1].val), (yyvsp[0].val));
			(yyvsp[0].val)->nd_iter = new_command_qcall((yyvsp[-3].val), (yyvsp[-4].val), (yyvsp[-2].val), (yyvsp[-1].val), &(yyloc));
			(yyvsp[0].val)->nd_loc = (yyloc);
			(yyval.val) = (yyvsp[0].val);
			fixpos((yyval.val), (yyvsp[-4].val));
#endif
			(yyval.val) = dispatch4(command_call, (yyvsp[-4].val), (yyvsp[-3].val), (yyvsp[-2].val), (yyvsp[-1].val));
			(yyval.val) = method_add_block((yyval.val), (yyvsp[0].val));

		    }
#line 9255 "ripper.c"
    break;

  case 428:
#line 3703 "ripper.y"
                    {
#if 0
			(yyval.val) = (yyvsp[-1].val);
			(yyval.val)->nd_args = (yyvsp[0].val);
			nd_set_last_loc((yyvsp[-1].val), (yylsp[0]).last_loc);
#endif
			(yyval.val) = method_arg(dispatch1(fcall, (yyvsp[-1].val)), (yyvsp[0].val));

		    }
#line 9269 "ripper.c"
    break;

  case 429:
#line 3713 "ripper.y"
                    {
#if 0
			(yyval.num) = ruby_sourceline;
#endif
		    }
#line 9279 "ripper.c"
    break;

  case 430:
#line 3719 "ripper.y"
                    {
			(yyval.val) = new_qcall((yyvsp[-3].val), (yyvsp[-4].val), (yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
			nd_set_line((yyval.val), (yyvsp[-1].num));
		    }
#line 9288 "ripper.c"
    break;

  case 431:
#line 3724 "ripper.y"
                    {
#if 0
			(yyval.num) = ruby_sourceline;
#endif
		    }
#line 9298 "ripper.c"
    break;

  case 432:
#line 3730 "ripper.y"
                    {
			(yyval.val) = new_qcall(ID2VAL(idCOLON2), (yyvsp[-4].val), (yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
			nd_set_line((yyval.val), (yyvsp[-1].num));
		    }
#line 9307 "ripper.c"
    break;

  case 433:
#line 3735 "ripper.y"
                    {
			(yyval.val) = new_qcall(ID2VAL(idCOLON2), (yyvsp[-2].val), (yyvsp[0].val), Qnull, &(yyloc));
		    }
#line 9315 "ripper.c"
    break;

  case 434:
#line 3739 "ripper.y"
                    {
#if 0
			(yyval.num) = ruby_sourceline;
#endif
		    }
#line 9325 "ripper.c"
    break;

  case 435:
#line 3745 "ripper.y"
                    {
			(yyval.val) = new_qcall((yyvsp[-2].val), (yyvsp[-3].val), ID2VAL(idCall), (yyvsp[0].val), &(yyloc));
			nd_set_line((yyval.val), (yyvsp[-1].num));
		    }
#line 9334 "ripper.c"
    break;

  case 436:
#line 3750 "ripper.y"
                    {
#if 0
			(yyval.num) = ruby_sourceline;
#endif
		    }
#line 9344 "ripper.c"
    break;

  case 437:
#line 3756 "ripper.y"
                    {
			(yyval.val) = new_qcall(ID2VAL(idCOLON2), (yyvsp[-3].val), ID2VAL(idCall), (yyvsp[0].val), &(yyloc));
			nd_set_line((yyval.val), (yyvsp[-1].num));
		    }
#line 9353 "ripper.c"
    break;

  case 438:
#line 3761 "ripper.y"
                    {
#if 0
			(yyval.val) = NEW_SUPER((yyvsp[0].val));
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch1(super, (yyvsp[0].val));

		    }
#line 9366 "ripper.c"
    break;

  case 439:
#line 3770 "ripper.y"
                    {
#if 0
			(yyval.val) = NEW_ZSUPER();
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch0(zsuper);

		    }
#line 9379 "ripper.c"
    break;

  case 440:
#line 3779 "ripper.y"
                    {
#if 0
			if ((yyvsp[-3].val) && nd_type((yyvsp[-3].val)) == NODE_SELF)
			    (yyval.val) = new_fcall(tAREF, (yyvsp[-1].val), &(yyloc));
			else
			    (yyval.val) = new_call((yyvsp[-3].val), tAREF, (yyvsp[-1].val), &(yyloc));
			fixpos((yyval.val), (yyvsp[-3].val));
#endif
			(yyval.val) = dispatch2(aref, (yyvsp[-3].val), escape_Qundef((yyvsp[-1].val)));

		    }
#line 9395 "ripper.c"
    break;

  case 441:
#line 3793 "ripper.y"
                    {
#if 0
			(yyval.num) = ruby_sourceline;
#endif
		    }
#line 9405 "ripper.c"
    break;

  case 442:
#line 3799 "ripper.y"
                    {
			(yyval.val) = (yyvsp[-1].val);
#if 0
			(yyvsp[-1].val)->nd_body->nd_loc.first_loc = (yylsp[-3]).first_loc;
			(yyvsp[-1].val)->nd_body->nd_loc.last_loc = (yylsp[0]).last_loc;
			nd_set_line((yyval.val), (yyvsp[-2].num));
#endif
		    }
#line 9418 "ripper.c"
    break;

  case 443:
#line 3808 "ripper.y"
                    {
#if 0
			(yyval.num) = ruby_sourceline;
#endif
		    }
#line 9428 "ripper.c"
    break;

  case 444:
#line 3814 "ripper.y"
                    {
			(yyval.val) = (yyvsp[-1].val);
#if 0
			(yyvsp[-1].val)->nd_body->nd_loc.first_loc = (yylsp[-3]).first_loc;
			(yyvsp[-1].val)->nd_body->nd_loc.last_loc = (yylsp[0]).last_loc;
			nd_set_line((yyval.val), (yyvsp[-2].num));
#endif
		    }
#line 9441 "ripper.c"
    break;

  case 445:
#line 3824 "ripper.y"
                  {(yyval.vars) = dyna_push();}
#line 9447 "ripper.c"
    break;

  case 446:
#line 3825 "ripper.y"
                  {(yyval.val) = cmdarg_stack >> 1; CMDARG_SET(0);}
#line 9453 "ripper.c"
    break;

  case 447:
#line 3827 "ripper.y"
                    {
			(yyval.val) = new_brace_body((yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
			dyna_pop((yyvsp[-3].vars));
			CMDARG_SET((yyvsp[-2].val));
		    }
#line 9463 "ripper.c"
    break;

  case 448:
#line 3834 "ripper.y"
                  {(yyval.vars) = dyna_push();}
#line 9469 "ripper.c"
    break;

  case 449:
#line 3835 "ripper.y"
                  {(yyval.val) = cmdarg_stack; CMDARG_SET(0);}
#line 9475 "ripper.c"
    break;

  case 450:
#line 3837 "ripper.y"
                    {
			(yyval.val) = new_do_body((yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
			dyna_pop((yyvsp[-3].vars));
			CMDARG_SET((yyvsp[-2].val));
		    }
#line 9485 "ripper.c"
    break;

  case 451:
#line 3847 "ripper.y"
                    {
#if 0
			(yyval.val) = NEW_WHEN((yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val));
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch3(when, (yyvsp[-3].val), (yyvsp[-1].val), escape_Qundef((yyvsp[0].val)));

		    }
#line 9498 "ripper.c"
    break;

  case 454:
#line 3864 "ripper.y"
                    {
#if 0
			if ((yyvsp[-3].val)) {
			    YYLTYPE location;
			    location.first_loc = (yylsp[-3]).first_loc;
			    location.last_loc = (yylsp[-1]).last_loc;
			    (yyvsp[-3].val) = node_assign((yyvsp[-3].val), new_errinfo(&(yylsp[-3])), &(yylsp[-3]));
			    (yyvsp[-1].val) = block_append((yyvsp[-3].val), (yyvsp[-1].val), &location);
			}
			(yyval.val) = new_resbody((yyvsp[-4].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
			fixpos((yyval.val), (yyvsp[-4].val)?(yyvsp[-4].val):(yyvsp[-1].val));
#endif
			(yyval.val) = dispatch4(rescue,
				       escape_Qundef((yyvsp[-4].val)),
				       escape_Qundef((yyvsp[-3].val)),
				       escape_Qundef((yyvsp[-1].val)),
				       escape_Qundef((yyvsp[0].val)));

		    }
#line 9522 "ripper.c"
    break;

  case 456:
#line 3887 "ripper.y"
                    {
#if 0
			(yyval.val) = new_list((yyvsp[0].val), &(yyloc));
#endif
			(yyval.val) = rb_ary_new3(1, get_value((yyvsp[0].val)));

		    }
#line 9534 "ripper.c"
    break;

  case 457:
#line 3895 "ripper.y"
                    {
#if 0
			if (!((yyval.val) = splat_array((yyvsp[0].val)))) (yyval.val) = (yyvsp[0].val);
#endif
			(yyval.val) = (yyvsp[0].val);

		    }
#line 9546 "ripper.c"
    break;

  case 459:
#line 3906 "ripper.y"
                    {
			(yyval.val) = (yyvsp[0].val);
		    }
#line 9554 "ripper.c"
    break;

  case 461:
#line 3913 "ripper.y"
                    {
#if 0
			(yyval.val) = (yyvsp[0].val);
#endif
			(yyval.val) = dispatch1(ensure, (yyvsp[0].val));

		    }
#line 9566 "ripper.c"
    break;

  case 464:
#line 3925 "ripper.y"
                    {
#if 0
			(yyval.val) = new_lit(ID2SYM((yyvsp[0].val)), &(yyloc));
#endif
			(yyval.val) = dispatch1(symbol_literal, (yyvsp[0].val));

		    }
#line 9578 "ripper.c"
    break;

  case 466:
#line 3936 "ripper.y"
                    {
#if 0
			NODE *node = (yyvsp[0].val);
			if (!node) {
			    node = new_str(STR_NEW0(), &(yyloc));
			}
			else {
			    node = evstr2dstr(node);
			}
			(yyval.val) = node;
#endif
			(yyval.val) = (yyvsp[0].val);

		    }
#line 9597 "ripper.c"
    break;

  case 467:
#line 3953 "ripper.y"
                    {
#if 0
			(yyval.val)->nd_loc = (yyloc);
#endif

		    }
#line 9608 "ripper.c"
    break;

  case 469:
#line 3961 "ripper.y"
                    {
#if 0
			(yyval.val) = literal_concat((yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			(yyval.val) = dispatch2(string_concat, (yyvsp[-1].val), (yyvsp[0].val));

		    }
#line 9620 "ripper.c"
    break;

  case 470:
#line 3971 "ripper.y"
                    {
			(yyval.val) = new_string1(heredoc_dedent((yyvsp[-1].val)));
#if 0
			if ((yyval.val)) nd_set_loc((yyval.val), &(yyloc));
#endif

		    }
#line 9632 "ripper.c"
    break;

  case 471:
#line 3981 "ripper.y"
                    {
			(yyval.val) = new_xstring(heredoc_dedent((yyvsp[-1].val)), &(yyloc));
		    }
#line 9640 "ripper.c"
    break;

  case 472:
#line 3987 "ripper.y"
                    {
			(yyval.val) = new_regexp((yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 9648 "ripper.c"
    break;

  case 473:
#line 3993 "ripper.y"
                    {
#if 0
			(yyval.val) = make_array((yyvsp[-1].val), &(yyloc));
#endif
			(yyval.val) = dispatch1(array, (yyvsp[-1].val));

		    }
#line 9660 "ripper.c"
    break;

  case 474:
#line 4003 "ripper.y"
                    {
#if 0
			(yyval.val) = 0;
#endif
			(yyval.val) = dispatch0(words_new);

		    }
#line 9672 "ripper.c"
    break;

  case 475:
#line 4011 "ripper.y"
                    {
#if 0
			(yyval.val) = list_append((yyvsp[-2].val), evstr2dstr((yyvsp[-1].val)));
#endif
			(yyval.val) = dispatch2(words_add, (yyvsp[-2].val), (yyvsp[-1].val));

		    }
#line 9684 "ripper.c"
    break;

  case 476:
#line 4023 "ripper.y"
                    {
			(yyval.val) = dispatch0(word_new);
			(yyval.val) = dispatch2(word_add, (yyval.val), (yyvsp[0].val));
		    }
#line 9693 "ripper.c"
    break;

  case 477:
#line 4029 "ripper.y"
                    {
#if 0
			(yyval.val) = literal_concat((yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			(yyval.val) = dispatch2(word_add, (yyvsp[-1].val), (yyvsp[0].val));

		    }
#line 9705 "ripper.c"
    break;

  case 478:
#line 4039 "ripper.y"
                    {
#if 0
			(yyval.val) = make_array((yyvsp[-1].val), &(yyloc));
#endif
			(yyval.val) = dispatch1(array, (yyvsp[-1].val));

		    }
#line 9717 "ripper.c"
    break;

  case 479:
#line 4049 "ripper.y"
                    {
#if 0
			(yyval.val) = 0;
#endif
			(yyval.val) = dispatch0(symbols_new);

		    }
#line 9729 "ripper.c"
    break;

  case 480:
#line 4057 "ripper.y"
                    {
#if 0
			(yyvsp[-1].val) = evstr2dstr((yyvsp[-1].val));
			if (nd_type((yyvsp[-1].val)) == NODE_DSTR) {
			    nd_set_type((yyvsp[-1].val), NODE_DSYM);
			}
			else {
			    nd_set_type((yyvsp[-1].val), NODE_LIT);
			    add_mark_object((yyvsp[-1].val)->nd_lit = rb_str_intern((yyvsp[-1].val)->nd_lit));
			}
			(yyval.val) = list_append((yyvsp[-2].val), (yyvsp[-1].val));
#endif
			(yyval.val) = dispatch2(symbols_add, (yyvsp[-2].val), (yyvsp[-1].val));

		    }
#line 9749 "ripper.c"
    break;

  case 481:
#line 4075 "ripper.y"
                    {
#if 0
			(yyval.val) = make_array((yyvsp[-1].val), &(yyloc));
#endif
			(yyval.val) = dispatch1(array, (yyvsp[-1].val));

		    }
#line 9761 "ripper.c"
    break;

  case 482:
#line 4085 "ripper.y"
                    {
#if 0
			(yyval.val) = make_array((yyvsp[-1].val), &(yyloc));
#endif
			(yyval.val) = dispatch1(array, (yyvsp[-1].val));

		    }
#line 9773 "ripper.c"
    break;

  case 483:
#line 4095 "ripper.y"
                    {
#if 0
			(yyval.val) = 0;
#endif
			(yyval.val) = dispatch0(qwords_new);

		    }
#line 9785 "ripper.c"
    break;

  case 484:
#line 4103 "ripper.y"
                    {
#if 0
			(yyvsp[-1].val)->nd_loc = (yylsp[-1]);
			(yyval.val) = list_append((yyvsp[-2].val), (yyvsp[-1].val));
#endif
			(yyval.val) = dispatch2(qwords_add, (yyvsp[-2].val), (yyvsp[-1].val));

		    }
#line 9798 "ripper.c"
    break;

  case 485:
#line 4114 "ripper.y"
                    {
#if 0
			(yyval.val) = 0;
#endif
			(yyval.val) = dispatch0(qsymbols_new);

		    }
#line 9810 "ripper.c"
    break;

  case 486:
#line 4122 "ripper.y"
                    {
#if 0
			VALUE lit;
			lit = (yyvsp[-1].val)->nd_lit;
			nd_set_type((yyvsp[-1].val), NODE_LIT);
			add_mark_object((yyvsp[-1].val)->nd_lit = ID2SYM(rb_intern_str(lit)));
			(yyvsp[-1].val)->nd_loc = (yylsp[-1]);
			(yyval.val) = list_append((yyvsp[-2].val), (yyvsp[-1].val));
#endif
			(yyval.val) = dispatch2(qsymbols_add, (yyvsp[-2].val), (yyvsp[-1].val));

		    }
#line 9827 "ripper.c"
    break;

  case 487:
#line 4137 "ripper.y"
                    {
#if 0
			(yyval.val) = 0;
#endif
			(yyval.val) = dispatch0(string_content);

		    }
#line 9839 "ripper.c"
    break;

  case 488:
#line 4145 "ripper.y"
                    {
#if 0
			(yyval.val) = literal_concat((yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			(yyval.val) = dispatch2(string_add, (yyvsp[-1].val), (yyvsp[0].val));

		    }
#line 9851 "ripper.c"
    break;

  case 489:
#line 4155 "ripper.y"
                    {
#if 0
			(yyval.val) = 0;
#endif
			(yyval.val) = dispatch0(xstring_new);

		    }
#line 9863 "ripper.c"
    break;

  case 490:
#line 4163 "ripper.y"
                    {
#if 0
			(yyval.val) = literal_concat((yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#endif
			(yyval.val) = dispatch2(xstring_add, (yyvsp[-1].val), (yyvsp[0].val));

		    }
#line 9875 "ripper.c"
    break;

  case 491:
#line 4173 "ripper.y"
                    {
#if 0
			(yyval.val) = 0;
#endif
			(yyval.val) = ripper_new_yylval(0, dispatch0(regexp_new), 0);

		    }
#line 9887 "ripper.c"
    break;

  case 492:
#line 4181 "ripper.y"
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
				head = list_append(new_dstr(Qnil, &(yyloc)), head);
				break;
			    }
			    (yyval.val) = list_append(head, tail);
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
			    (yyval.val) = ripper_new_yylval(0, (yyval.val), s2);
			}

		    }
#line 9930 "ripper.c"
    break;

  case 493:
#line 4222 "ripper.y"
                    {
#if 0
			(yyval.val)->nd_loc = (yyloc);
#endif

		    }
#line 9941 "ripper.c"
    break;

  case 494:
#line 4229 "ripper.y"
                    {
			/* need to backup lex_strterm so that a string literal `%&foo,#$&,bar&` can be parsed */
			(yyval.strterm) = lex_strterm;
			lex_strterm = 0;
			SET_LEX_STATE(EXPR_BEG);
		    }
#line 9952 "ripper.c"
    break;

  case 495:
#line 4236 "ripper.y"
                    {
			lex_strterm = (yyvsp[-1].strterm);
#if 0
			(yyval.val) = NEW_EVSTR((yyvsp[0].val));
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch1(string_dvar, (yyvsp[0].val));

		    }
#line 9966 "ripper.c"
    break;

  case 496:
#line 4246 "ripper.y"
                    {
			(yyvsp[0].val) = cond_stack;
			(yyval.val) = cmdarg_stack;
			COND_SET(0);
			CMDARG_SET(0);
		    }
#line 9977 "ripper.c"
    break;

  case 497:
#line 4252 "ripper.y"
                    {
			/* need to backup lex_strterm so that a string literal `%!foo,#{ !0 },bar!` can be parsed */
			(yyval.strterm) = lex_strterm;
			lex_strterm = 0;
		    }
#line 9987 "ripper.c"
    break;

  case 498:
#line 4257 "ripper.y"
                    {
			(yyval.num) = lex_state;
			SET_LEX_STATE(EXPR_BEG);
		    }
#line 9996 "ripper.c"
    break;

  case 499:
#line 4261 "ripper.y"
                    {
			(yyval.num) = brace_nest;
			brace_nest = 0;
		    }
#line 10005 "ripper.c"
    break;

  case 500:
#line 4265 "ripper.y"
                    {
			(yyval.num) = heredoc_indent;
			heredoc_indent = 0;
		    }
#line 10014 "ripper.c"
    break;

  case 501:
#line 4270 "ripper.y"
                    {
			COND_SET((yyvsp[-7].val));
			CMDARG_SET((yyvsp[-6].val));
			lex_strterm = (yyvsp[-5].strterm);
			SET_LEX_STATE((yyvsp[-4].num));
			brace_nest = (yyvsp[-3].num);
			heredoc_indent = (yyvsp[-2].num);
			heredoc_line_indent = -1;
#if 0
			if ((yyvsp[-1].val)) (yyvsp[-1].val)->flags &= ~NODE_FL_NEWLINE;
			(yyval.val) = new_evstr((yyvsp[-1].val), &(yyloc));
#endif
			(yyval.val) = dispatch1(string_embexpr, (yyvsp[-1].val));

		    }
#line 10034 "ripper.c"
    break;

  case 502:
#line 4288 "ripper.y"
                    {
#if 0
			(yyval.val) = new_gvar((yyvsp[0].val), &(yyloc));
#endif
			(yyval.val) = dispatch1(var_ref, (yyvsp[0].val));

		    }
#line 10046 "ripper.c"
    break;

  case 503:
#line 4296 "ripper.y"
                    {
#if 0
			(yyval.val) = new_ivar((yyvsp[0].val), &(yyloc));
#endif
			(yyval.val) = dispatch1(var_ref, (yyvsp[0].val));

		    }
#line 10058 "ripper.c"
    break;

  case 504:
#line 4304 "ripper.y"
                    {
#if 0
			(yyval.val) = NEW_CVAR((yyvsp[0].val));
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = dispatch1(var_ref, (yyvsp[0].val));

		    }
#line 10071 "ripper.c"
    break;

  case 506:
#line 4316 "ripper.y"
                    {
			SET_LEX_STATE(EXPR_END|EXPR_ENDARG);
#if 0
			(yyval.val) = (yyvsp[0].val);
#endif
			(yyval.val) = dispatch1(symbol, (yyvsp[0].val));

		    }
#line 10084 "ripper.c"
    break;

  case 511:
#line 4333 "ripper.y"
                    {
			SET_LEX_STATE(EXPR_END|EXPR_ENDARG);
#if 0
			(yyval.val) = dsym_node((yyvsp[-1].val), &(yyloc));
#endif
			(yyval.val) = dispatch1(dyna_symbol, (yyvsp[-1].val));

		    }
#line 10097 "ripper.c"
    break;

  case 513:
#line 4345 "ripper.y"
                    {
#if 0
			(yyval.val) = (yyvsp[0].val);
			add_mark_object((yyval.val)->nd_lit = negate_lit((yyval.val)->nd_lit));
#endif
			(yyval.val) = dispatch2(unary, ID2VAL(idUMinus), (yyvsp[0].val));

		    }
#line 10110 "ripper.c"
    break;

  case 514:
#line 4356 "ripper.y"
                    {
#if 0
			(yyval.val)->nd_loc = (yyloc);
#endif

		    }
#line 10121 "ripper.c"
    break;

  case 515:
#line 4363 "ripper.y"
                    {
#if 0
			(yyval.val)->nd_loc = (yyloc);
#endif

		    }
#line 10132 "ripper.c"
    break;

  case 516:
#line 4370 "ripper.y"
                    {
#if 0
			(yyval.val)->nd_loc = (yyloc);
#endif

		    }
#line 10143 "ripper.c"
    break;

  case 517:
#line 4377 "ripper.y"
                    {
#if 0
			(yyval.val)->nd_loc = (yyloc);
#endif

		    }
#line 10154 "ripper.c"
    break;

  case 523:
#line 4392 "ripper.y"
                              {(yyval.val) = KWD2EID(nil, (yyvsp[0].val));}
#line 10160 "ripper.c"
    break;

  case 524:
#line 4393 "ripper.y"
                               {(yyval.val) = KWD2EID(self, (yyvsp[0].val));}
#line 10166 "ripper.c"
    break;

  case 525:
#line 4394 "ripper.y"
                               {(yyval.val) = KWD2EID(true, (yyvsp[0].val));}
#line 10172 "ripper.c"
    break;

  case 526:
#line 4395 "ripper.y"
                                {(yyval.val) = KWD2EID(false, (yyvsp[0].val));}
#line 10178 "ripper.c"
    break;

  case 527:
#line 4396 "ripper.y"
                                  {(yyval.val) = KWD2EID(_FILE__, (yyvsp[0].val));}
#line 10184 "ripper.c"
    break;

  case 528:
#line 4397 "ripper.y"
                                  {(yyval.val) = KWD2EID(_LINE__, (yyvsp[0].val));}
#line 10190 "ripper.c"
    break;

  case 529:
#line 4398 "ripper.y"
                                      {(yyval.val) = KWD2EID(_ENCODING__, (yyvsp[0].val));}
#line 10196 "ripper.c"
    break;

  case 530:
#line 4402 "ripper.y"
                    {
#if 0
			if (!((yyval.val) = gettable((yyvsp[0].val), &(yyloc)))) (yyval.val) = new_begin(0, &(yyloc));
#endif
			if (id_is_var(get_id((yyvsp[0].val)))) {
			    (yyval.val) = dispatch1(var_ref, (yyvsp[0].val));
			}
			else {
			    (yyval.val) = dispatch1(vcall, (yyvsp[0].val));
			}

		    }
#line 10213 "ripper.c"
    break;

  case 531:
#line 4415 "ripper.y"
                    {
#if 0
			if (!((yyval.val) = gettable((yyvsp[0].val), &(yyloc)))) (yyval.val) = new_begin(0, &(yyloc));
#endif
			(yyval.val) = dispatch1(var_ref, (yyvsp[0].val));

		    }
#line 10225 "ripper.c"
    break;

  case 532:
#line 4425 "ripper.y"
                    {
			(yyval.val) = assignable(var_field((yyvsp[0].val)), 0, &(yyloc));
		    }
#line 10233 "ripper.c"
    break;

  case 533:
#line 4429 "ripper.y"
                    {
			(yyval.val) = assignable(var_field((yyvsp[0].val)), 0, &(yyloc));
		    }
#line 10241 "ripper.c"
    break;

  case 534:
#line 4435 "ripper.y"
                    {
#if 0
			(yyval.val)->nd_loc = (yyloc);
#endif

		    }
#line 10252 "ripper.c"
    break;

  case 535:
#line 4442 "ripper.y"
                    {
#if 0
			(yyval.val)->nd_loc = (yyloc);
#endif

		    }
#line 10263 "ripper.c"
    break;

  case 536:
#line 4451 "ripper.y"
                    {
			SET_LEX_STATE(EXPR_BEG);
			command_start = TRUE;
		    }
#line 10272 "ripper.c"
    break;

  case 537:
#line 4456 "ripper.y"
                    {
			(yyval.val) = (yyvsp[-1].val);
		    }
#line 10280 "ripper.c"
    break;

  case 538:
#line 4460 "ripper.y"
                    {
#if 0
			(yyval.val) = 0;
#endif
			(yyval.val) = Qnil;

		    }
#line 10292 "ripper.c"
    break;

  case 539:
#line 4470 "ripper.y"
                    {
#if 0
			(yyval.val) = (yyvsp[-1].val);
#endif
			(yyval.val) = dispatch1(paren, (yyvsp[-1].val));

			SET_LEX_STATE(EXPR_BEG);
			command_start = TRUE;
		    }
#line 10306 "ripper.c"
    break;

  case 540:
#line 4479 "ripper.y"
                    {
			(yyval.num) = parser->in_kwarg;
			parser->in_kwarg = 1;
			SET_LEX_STATE(lex_state|EXPR_LABEL); /* force for args */
		    }
#line 10316 "ripper.c"
    break;

  case 541:
#line 4485 "ripper.y"
                    {
			parser->in_kwarg = !!(yyvsp[-2].num);
			(yyval.val) = (yyvsp[-1].val);
			SET_LEX_STATE(EXPR_BEG);
			command_start = TRUE;
		    }
#line 10327 "ripper.c"
    break;

  case 542:
#line 4494 "ripper.y"
                    {
			(yyval.val) = new_args_tail((yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-1]));
		    }
#line 10335 "ripper.c"
    break;

  case 543:
#line 4498 "ripper.y"
                    {
			(yyval.val) = new_args_tail((yyvsp[-1].val), Qnone, (yyvsp[0].val), &(yylsp[-1]));
		    }
#line 10343 "ripper.c"
    break;

  case 544:
#line 4502 "ripper.y"
                    {
			(yyval.val) = new_args_tail(Qnone, (yyvsp[-1].val), (yyvsp[0].val), &(yylsp[-1]));
		    }
#line 10351 "ripper.c"
    break;

  case 545:
#line 4506 "ripper.y"
                    {
			(yyval.val) = new_args_tail(Qnone, Qnone, (yyvsp[0].val), &(yylsp[0]));
		    }
#line 10359 "ripper.c"
    break;

  case 546:
#line 4512 "ripper.y"
                    {
			(yyval.val) = (yyvsp[0].val);
		    }
#line 10367 "ripper.c"
    break;

  case 547:
#line 4516 "ripper.y"
                    {
			(yyval.val) = new_args_tail(Qnone, Qnone, Qnone, &(yylsp[0]));
		    }
#line 10375 "ripper.c"
    break;

  case 548:
#line 4522 "ripper.y"
                    {
			(yyval.val) = new_args((yyvsp[-5].val), (yyvsp[-3].val), (yyvsp[-1].val), Qnone, (yyvsp[0].val), &(yyloc));
		    }
#line 10383 "ripper.c"
    break;

  case 549:
#line 4526 "ripper.y"
                    {
			(yyval.val) = new_args((yyvsp[-7].val), (yyvsp[-5].val), (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 10391 "ripper.c"
    break;

  case 550:
#line 4530 "ripper.y"
                    {
			(yyval.val) = new_args((yyvsp[-3].val), (yyvsp[-1].val), Qnone, Qnone, (yyvsp[0].val), &(yyloc));
		    }
#line 10399 "ripper.c"
    break;

  case 551:
#line 4534 "ripper.y"
                    {
			(yyval.val) = new_args((yyvsp[-5].val), (yyvsp[-3].val), Qnone, (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 10407 "ripper.c"
    break;

  case 552:
#line 4538 "ripper.y"
                    {
			(yyval.val) = new_args((yyvsp[-3].val), Qnone, (yyvsp[-1].val), Qnone, (yyvsp[0].val), &(yyloc));
		    }
#line 10415 "ripper.c"
    break;

  case 553:
#line 4542 "ripper.y"
                    {
			(yyval.val) = new_args((yyvsp[-5].val), Qnone, (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 10423 "ripper.c"
    break;

  case 554:
#line 4546 "ripper.y"
                    {
			(yyval.val) = new_args((yyvsp[-1].val), Qnone, Qnone, Qnone, (yyvsp[0].val), &(yyloc));
		    }
#line 10431 "ripper.c"
    break;

  case 555:
#line 4550 "ripper.y"
                    {
			(yyval.val) = new_args(Qnone, (yyvsp[-3].val), (yyvsp[-1].val), Qnone, (yyvsp[0].val), &(yyloc));
		    }
#line 10439 "ripper.c"
    break;

  case 556:
#line 4554 "ripper.y"
                    {
			(yyval.val) = new_args(Qnone, (yyvsp[-5].val), (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 10447 "ripper.c"
    break;

  case 557:
#line 4558 "ripper.y"
                    {
			(yyval.val) = new_args(Qnone, (yyvsp[-1].val), Qnone, Qnone, (yyvsp[0].val), &(yyloc));
		    }
#line 10455 "ripper.c"
    break;

  case 558:
#line 4562 "ripper.y"
                    {
			(yyval.val) = new_args(Qnone, (yyvsp[-3].val), Qnone, (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 10463 "ripper.c"
    break;

  case 559:
#line 4566 "ripper.y"
                    {
			(yyval.val) = new_args(Qnone, Qnone, (yyvsp[-1].val), Qnone, (yyvsp[0].val), &(yyloc));
		    }
#line 10471 "ripper.c"
    break;

  case 560:
#line 4570 "ripper.y"
                    {
			(yyval.val) = new_args(Qnone, Qnone, (yyvsp[-3].val), (yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
		    }
#line 10479 "ripper.c"
    break;

  case 561:
#line 4574 "ripper.y"
                    {
			(yyval.val) = new_args(Qnone, Qnone, Qnone, Qnone, (yyvsp[0].val), &(yyloc));
		    }
#line 10487 "ripper.c"
    break;

  case 562:
#line 4578 "ripper.y"
                    {
			(yyval.val) = new_args_tail(Qnone, Qnone, Qnone, &(yylsp[0]));
			(yyval.val) = new_args(Qnone, Qnone, Qnone, Qnone, (yyval.val), &(yylsp[0]));
		    }
#line 10496 "ripper.c"
    break;

  case 563:
#line 4585 "ripper.y"
                    {
#if 0
			yyerror0("formal argument cannot be a constant");
			(yyval.val) = 0;
#endif
			(yyval.val) = dispatch1(param_error, (yyvsp[0].val));
			ripper_error();

		    }
#line 10510 "ripper.c"
    break;

  case 564:
#line 4595 "ripper.y"
                    {
#if 0
			yyerror0("formal argument cannot be an instance variable");
			(yyval.val) = 0;
#endif
			(yyval.val) = dispatch1(param_error, (yyvsp[0].val));
			ripper_error();

		    }
#line 10524 "ripper.c"
    break;

  case 565:
#line 4605 "ripper.y"
                    {
#if 0
			yyerror0("formal argument cannot be a global variable");
			(yyval.val) = 0;
#endif
			(yyval.val) = dispatch1(param_error, (yyvsp[0].val));
			ripper_error();

		    }
#line 10538 "ripper.c"
    break;

  case 566:
#line 4615 "ripper.y"
                    {
#if 0
			yyerror0("formal argument cannot be a class variable");
			(yyval.val) = 0;
#endif
			(yyval.val) = dispatch1(param_error, (yyvsp[0].val));
			ripper_error();

		    }
#line 10552 "ripper.c"
    break;

  case 568:
#line 4628 "ripper.y"
                    {
			formal_argument(get_id((yyvsp[0].val)));
			(yyval.val) = (yyvsp[0].val);
		    }
#line 10561 "ripper.c"
    break;

  case 569:
#line 4635 "ripper.y"
                    {
			ID id = get_id((yyvsp[0].val));
			arg_var(id);
			current_arg = id;
			(yyval.val) = (yyvsp[0].val);
		    }
#line 10572 "ripper.c"
    break;

  case 570:
#line 4644 "ripper.y"
                    {
			current_arg = 0;
#if 0
			(yyval.val) = NEW_ARGS_AUX((yyvsp[0].val), 1);
#endif
			(yyval.val) = get_value((yyvsp[0].val));

		    }
#line 10585 "ripper.c"
    break;

  case 571:
#line 4653 "ripper.y"
                    {
			ID tid = internal_id();
#if 0
			YYLTYPE location;
			location.first_loc = (yylsp[-1]).first_loc;
			location.last_loc = (yylsp[-1]).first_loc;
#endif

			arg_var(tid);
#if 0
			if (dyna_in_block()) {
			    (yyvsp[-1].val)->nd_value = new_dvar(tid, &location);
			}
			else {
			    (yyvsp[-1].val)->nd_value = new_lvar(tid, &location);
			}
			(yyval.val) = NEW_ARGS_AUX(tid, 1);
			(yyval.val)->nd_next = (yyvsp[-1].val);
#endif
			(yyval.val) = dispatch1(mlhs_paren, (yyvsp[-1].val));

		    }
#line 10612 "ripper.c"
    break;

  case 572:
#line 4680 "ripper.y"
                    {
			(yyval.val) = rb_ary_new3(1, get_value((yyvsp[0].val)));
		    }
#line 10620 "ripper.c"
    break;

  case 573:
#line 4685 "ripper.y"
                    {
#if 0
			(yyval.val) = (yyvsp[-2].val);
			(yyval.val)->nd_plen++;
			(yyval.val)->nd_next = block_append((yyval.val)->nd_next, (yyvsp[0].val)->nd_next, &(yyloc));
			rb_discard_node((yyvsp[0].val));
#endif
			(yyval.val) = rb_ary_push((yyvsp[-2].val), get_value((yyvsp[0].val)));

		    }
#line 10635 "ripper.c"
    break;

  case 574:
#line 4699 "ripper.y"
                    {
			ID id = get_id((yyvsp[0].val));
			arg_var(formal_argument(id));
			current_arg = id;
			(yyval.val) = (yyvsp[0].val);
		    }
#line 10646 "ripper.c"
    break;

  case 575:
#line 4708 "ripper.y"
                    {
			current_arg = 0;
			(yyval.val) = assignable((yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#if 0
			(yyval.val) = new_kw_arg((yyval.val), &(yyloc));
#endif
			(yyval.val) = rb_assoc_new(get_value((yyval.val)), get_value((yyvsp[0].val)));

		    }
#line 10660 "ripper.c"
    break;

  case 576:
#line 4718 "ripper.y"
                    {
			current_arg = 0;
			(yyval.val) = assignable((yyvsp[0].val), NODE_SPECIAL_REQUIRED_KEYWORD, &(yyloc));
#if 0
			(yyval.val) = new_kw_arg((yyval.val), &(yyloc));
#endif
			(yyval.val) = rb_assoc_new(get_value((yyval.val)), 0);

		    }
#line 10674 "ripper.c"
    break;

  case 577:
#line 4730 "ripper.y"
                    {
			(yyval.val) = assignable((yyvsp[-1].val), (yyvsp[0].val), &(yyloc));
#if 0
			(yyval.val) = new_kw_arg((yyval.val), &(yyloc));
#endif
			(yyval.val) = rb_assoc_new(get_value((yyval.val)), get_value((yyvsp[0].val)));

		    }
#line 10687 "ripper.c"
    break;

  case 578:
#line 4739 "ripper.y"
                    {
			(yyval.val) = assignable((yyvsp[0].val), NODE_SPECIAL_REQUIRED_KEYWORD, &(yyloc));
#if 0
			(yyval.val) = new_kw_arg((yyval.val), &(yyloc));
#endif
			(yyval.val) = rb_assoc_new(get_value((yyval.val)), 0);

		    }
#line 10700 "ripper.c"
    break;

  case 579:
#line 4750 "ripper.y"
                    {
#if 0
			(yyval.val) = (yyvsp[0].val);
#endif
			(yyval.val) = rb_ary_new3(1, get_value((yyvsp[0].val)));

		    }
#line 10712 "ripper.c"
    break;

  case 580:
#line 4758 "ripper.y"
                    {
#if 0
			(yyval.val) = kwd_append((yyvsp[-2].val), (yyvsp[0].val));
#endif
			(yyval.val) = rb_ary_push((yyvsp[-2].val), get_value((yyvsp[0].val)));

		    }
#line 10724 "ripper.c"
    break;

  case 581:
#line 4769 "ripper.y"
                    {
#if 0
			(yyval.val) = (yyvsp[0].val);
#endif
			(yyval.val) = rb_ary_new3(1, get_value((yyvsp[0].val)));

		    }
#line 10736 "ripper.c"
    break;

  case 582:
#line 4777 "ripper.y"
                    {
#if 0
			(yyval.val) = kwd_append((yyvsp[-2].val), (yyvsp[0].val));
#endif
			(yyval.val) = rb_ary_push((yyvsp[-2].val), get_value((yyvsp[0].val)));

		    }
#line 10748 "ripper.c"
    break;

  case 585:
#line 4791 "ripper.y"
                    {
			shadowing_lvar(get_id((yyvsp[0].val)));
#if 0
			(yyval.val) = (yyvsp[0].val);
#endif
			(yyval.val) = dispatch1(kwrest_param, (yyvsp[0].val));

		    }
#line 10761 "ripper.c"
    break;

  case 586:
#line 4800 "ripper.y"
                    {
#if 0
			(yyval.val) = internal_id();
			arg_var((yyval.val));
#endif
			(yyval.val) = dispatch1(kwrest_param, Qnil);

		    }
#line 10774 "ripper.c"
    break;

  case 587:
#line 4811 "ripper.y"
                    {
			current_arg = 0;
			(yyval.val) = assignable((yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#if 0
			(yyval.val) = NEW_OPT_ARG(0, (yyval.val));
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = rb_assoc_new(get_value((yyval.val)), get_value((yyvsp[0].val)));

		    }
#line 10789 "ripper.c"
    break;

  case 588:
#line 4824 "ripper.y"
                    {
			current_arg = 0;
			(yyval.val) = assignable((yyvsp[-2].val), (yyvsp[0].val), &(yyloc));
#if 0
			(yyval.val) = NEW_OPT_ARG(0, (yyval.val));
			(yyval.val)->nd_loc = (yyloc);
#endif
			(yyval.val) = rb_assoc_new(get_value((yyval.val)), get_value((yyvsp[0].val)));

		    }
#line 10804 "ripper.c"
    break;

  case 589:
#line 4837 "ripper.y"
                    {
#if 0
			(yyval.val) = (yyvsp[0].val);
#endif
			(yyval.val) = rb_ary_new3(1, get_value((yyvsp[0].val)));

		    }
#line 10816 "ripper.c"
    break;

  case 590:
#line 4845 "ripper.y"
                    {
#if 0
			(yyval.val) = opt_arg_append((yyvsp[-2].val), (yyvsp[0].val));
#endif
			(yyval.val) = rb_ary_push((yyvsp[-2].val), get_value((yyvsp[0].val)));

		    }
#line 10828 "ripper.c"
    break;

  case 591:
#line 4855 "ripper.y"
                    {
#if 0
			(yyval.val) = (yyvsp[0].val);
#endif
			(yyval.val) = rb_ary_new3(1, get_value((yyvsp[0].val)));

		    }
#line 10840 "ripper.c"
    break;

  case 592:
#line 4863 "ripper.y"
                    {
#if 0
			(yyval.val) = opt_arg_append((yyvsp[-2].val), (yyvsp[0].val));
#endif
			(yyval.val) = rb_ary_push((yyvsp[-2].val), get_value((yyvsp[0].val)));

		    }
#line 10852 "ripper.c"
    break;

  case 595:
#line 4877 "ripper.y"
                    {
#if 0
			if (!is_local_id((yyvsp[0].val)))
			    yyerror0("rest argument must be local variable");
#endif
			arg_var(shadowing_lvar(get_id((yyvsp[0].val))));
#if 0
			(yyval.val) = (yyvsp[0].val);
#endif
			(yyval.val) = dispatch1(rest_param, (yyvsp[0].val));

		    }
#line 10869 "ripper.c"
    break;

  case 596:
#line 4890 "ripper.y"
                    {
#if 0
			(yyval.val) = internal_id();
			arg_var((yyval.val));
#endif
			(yyval.val) = dispatch1(rest_param, Qnil);

		    }
#line 10882 "ripper.c"
    break;

  case 599:
#line 4905 "ripper.y"
                    {
#if 0
			if (!is_local_id((yyvsp[0].val)))
			    yyerror0("block argument must be local variable");
			else if (!dyna_in_block() && local_id((yyvsp[0].val)))
			    yyerror0("duplicated block argument name");
#endif
			arg_var(shadowing_lvar(get_id((yyvsp[0].val))));
#if 0
			(yyval.val) = (yyvsp[0].val);
#endif
			(yyval.val) = dispatch1(blockarg, (yyvsp[0].val));

		    }
#line 10901 "ripper.c"
    break;

  case 600:
#line 4922 "ripper.y"
                    {
			(yyval.val) = (yyvsp[0].val);
		    }
#line 10909 "ripper.c"
    break;

  case 601:
#line 4926 "ripper.y"
                    {
#if 0
			(yyval.val) = 0;
#endif
			(yyval.val) = Qundef;

		    }
#line 10921 "ripper.c"
    break;

  case 602:
#line 4936 "ripper.y"
                    {
#if 0
			value_expr((yyvsp[0].val));
			(yyval.val) = (yyvsp[0].val);
			if (!(yyval.val)) (yyval.val) = NEW_NIL();
#endif
			(yyval.val) = (yyvsp[0].val);

		    }
#line 10935 "ripper.c"
    break;

  case 603:
#line 4945 "ripper.y"
                      {SET_LEX_STATE(EXPR_BEG);}
#line 10941 "ripper.c"
    break;

  case 604:
#line 4946 "ripper.y"
                    {
#if 0
			if ((yyvsp[-1].val) == 0) {
			    yyerror0("can't define singleton method for ().");
			}
			else {
			    switch (nd_type((yyvsp[-1].val))) {
			      case NODE_STR:
			      case NODE_DSTR:
			      case NODE_XSTR:
			      case NODE_DXSTR:
			      case NODE_DREGX:
			      case NODE_LIT:
			      case NODE_ARRAY:
			      case NODE_ZARRAY:
				yyerror0("can't define singleton method for literals");
				break;
			      default:
				value_expr((yyvsp[-1].val));
				break;
			    }
			}
			(yyval.val) = (yyvsp[-1].val);
#endif
			(yyval.val) = dispatch1(paren, (yyvsp[-1].val));

		    }
#line 10973 "ripper.c"
    break;

  case 606:
#line 4977 "ripper.y"
                    {
#if 0
			(yyval.val) = (yyvsp[-1].val);
#endif
			(yyval.val) = dispatch1(assoclist_from_args, (yyvsp[-1].val));

		    }
#line 10985 "ripper.c"
    break;

  case 607:
#line 4989 "ripper.y"
                    {
			(yyval.val) = rb_ary_new3(1, get_value((yyvsp[0].val)));
		    }
#line 10993 "ripper.c"
    break;

  case 608:
#line 4994 "ripper.y"
                    {
#if 0
			NODE *assocs = (yyvsp[-2].val);
			NODE *tail = (yyvsp[0].val);
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
			(yyval.val) = assocs;
#endif
			(yyval.val) = rb_ary_push((yyvsp[-2].val), get_value((yyvsp[0].val)));

		    }
#line 11019 "ripper.c"
    break;

  case 609:
#line 5018 "ripper.y"
                    {
#if 0
			if (nd_type((yyvsp[-2].val)) == NODE_STR) {
			    nd_set_type((yyvsp[-2].val), NODE_LIT);
			    add_mark_object((yyvsp[-2].val)->nd_lit = rb_fstring((yyvsp[-2].val)->nd_lit));
			}
			(yyval.val) = list_append(new_list((yyvsp[-2].val), &(yyloc)), (yyvsp[0].val));
#endif
			(yyval.val) = dispatch2(assoc_new, (yyvsp[-2].val), (yyvsp[0].val));

		    }
#line 11035 "ripper.c"
    break;

  case 610:
#line 5030 "ripper.y"
                    {
#if 0
			(yyval.val) = list_append(new_list(new_lit(ID2SYM((yyvsp[-1].val)), &(yylsp[-1])), &(yyloc)), (yyvsp[0].val));
#endif
			(yyval.val) = dispatch2(assoc_new, (yyvsp[-1].val), (yyvsp[0].val));

		    }
#line 11047 "ripper.c"
    break;

  case 611:
#line 5038 "ripper.y"
                    {
#if 0
			YYLTYPE location;
			location.first_loc = (yylsp[-3]).first_loc;
			location.last_loc = (yylsp[-1]).last_loc;
			(yyval.val) = list_append(new_list(dsym_node((yyvsp[-2].val), &location), &location), (yyvsp[0].val));
#endif
			(yyval.val) = dispatch2(assoc_new, dispatch1(dyna_symbol, (yyvsp[-2].val)), (yyvsp[0].val));

		    }
#line 11062 "ripper.c"
    break;

  case 612:
#line 5049 "ripper.y"
                    {
#if 0
			if (nd_type((yyvsp[0].val)) == NODE_HASH &&
			    !((yyvsp[0].val)->nd_head && (yyvsp[0].val)->nd_head->nd_alen))
			    (yyval.val) = 0;
			else
			    (yyval.val) = list_append(new_list(0, &(yyloc)), (yyvsp[0].val));
#endif
			(yyval.val) = dispatch1(assoc_splat, (yyvsp[0].val));

		    }
#line 11078 "ripper.c"
    break;

  case 625:
#line 5083 "ripper.y"
                    {
			(yyval.val) = TOKEN2VAL('.');
		    }
#line 11086 "ripper.c"
    break;

  case 626:
#line 5087 "ripper.y"
                    {
			(yyval.val) = ID2VAL(idANDDOT);
		    }
#line 11094 "ripper.c"
    break;

  case 628:
#line 5094 "ripper.y"
                    {
			(yyval.val) = ID2VAL(idCOLON2);
		    }
#line 11102 "ripper.c"
    break;

  case 638:
#line 5118 "ripper.y"
                      {yyerrok;token_flush(parser);}
#line 11108 "ripper.c"
    break;

  case 639:
#line 5119 "ripper.y"
                       {token_flush(parser);}
#line 11114 "ripper.c"
    break;

  case 641:
#line 5123 "ripper.y"
                            {yyerrok;}
#line 11120 "ripper.c"
    break;

  case 642:
#line 5127 "ripper.y"
                    {
			(yyval.val) = Qnull;
		    }
#line 11128 "ripper.c"
    break;


#line 11132 "ripper.c"

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
      yyerror (&yylloc, parser, YY_("syntax error"));
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
            yymsg = YY_CAST (char *, YYSTACK_ALLOC (YY_CAST (YYSIZE_T, yymsg_alloc)));
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
        yyerror (&yylloc, parser, yymsgp);
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
                      yytoken, &yylval, &yylloc, parser);
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
                  yystos[yystate], yyvsp, yylsp, parser);
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
  yyerror (&yylloc, parser, YY_("memory exhausted"));
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
                  yytoken, &yylval, &yylloc, parser);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[+*yyssp], yyvsp, yylsp, parser);
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
#line 5131 "ripper.y"

# undef parser
# undef yylex
# undef yylval
# define yylval  (*parser->lval)

static int parser_regx_options(struct parser_params*);
static int parser_tokadd_string(struct parser_params*,int,int,int,long*,rb_encoding**,rb_encoding**);
static void parser_tokaddmbc(struct parser_params *parser, int c, rb_encoding *enc);
static enum yytokentype parser_parse_string(struct parser_params*,rb_strterm_literal_t*);
static enum yytokentype parser_here_document(struct parser_params*,rb_strterm_heredoc_t*);


# define nextc()                      parser_nextc(parser)
# define pushback(c)                  parser_pushback(parser, (c))
# define newtok()                     parser_newtok(parser)
# define tokspace(n)                  parser_tokspace(parser, (n))
# define tokadd(c)                    parser_tokadd(parser, (c))
# define tok_hex(numlen)              parser_tok_hex(parser, (numlen))
# define read_escape(flags,e)         parser_read_escape(parser, (flags), (e))
# define tokadd_escape(e)             parser_tokadd_escape(parser, (e))
# define regx_options()               parser_regx_options(parser)
# define tokadd_string(f,t,p,n,e,e2)  parser_tokadd_string(parser,(f),(t),(p),(n),(e),(e2))
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
ripper_yylval_id_gen(struct parser_params *parser, ID x)
{
    return ripper_new_yylval(x, ID2SYM(x), 0);
}
#define ripper_yylval_id(x) ripper_yylval_id_gen(parser, x)
# define set_yylval_str(x) (yylval.val = (x))
# define set_yylval_num(x) (yylval.val = ripper_new_yylval((x), 0, 0))
# define set_yylval_id(x)  (void)(x)
# define set_yylval_name(x) (void)(yylval.val = ripper_yylval_id(x))
# define set_yylval_literal(x) (void)(x)
# define set_yylval_node(x) (void)(x)
# define yylval_id() yylval.id
#endif

#ifndef RIPPER
#define literal_flush(p) (parser->tokp = (p))
#define dispatch_scan_event(t) ((void)0)
#define dispatch_delayed_token(t) ((void)0)
#define has_delayed_token() (0)
#else
#define literal_flush(p) ((void)0)

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
    token_flush(parser);
    return rval;
}

static void
ripper_dispatch_scan_event(struct parser_params *parser, int t)
{
    if (!ripper_has_scan_event(parser)) return;
    add_mark_object(yylval_rval = ripper_scan_event_val(parser, t));
}
#define dispatch_scan_event(t) ripper_dispatch_scan_event(parser, t)

static void
ripper_dispatch_delayed_token(struct parser_params *parser, int t)
{
    int saved_line = ruby_sourceline;
    const char *saved_tokp = parser->tokp;

    ruby_sourceline = parser->delayed_line;
    parser->tokp = lex_pbeg + parser->delayed_col;
    add_mark_object(yylval_rval = ripper_dispatch1(parser, ripper_token2eventid(t), parser->delayed));
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
    const char *pre = "", *post = "", *pend;
    const char *code = "", *caret = "", *newline = "";
    const char *lim;
    char *buf;
    long len;
    int i;

    pend = lex_pend;
    if (pend > lex_pbeg && pend[-1] == '\n') {
	if (--pend > lex_pbeg && pend[-1] == '\r') --pend;
    }

    p = pe = lex_p < pend ? lex_p : pend;
    lim = p - lex_pbeg > max_line_margin ? p - max_line_margin : lex_pbeg;
    while ((lim < p) && (*(p-1) != '\n')) p--;

    lim = pend - pe > max_line_margin ? pe + max_line_margin : pend;
    while ((pe < lim) && (*pe != '\n')) pe++;

    len = pe - p;
    if (len > 4) {
	char *p2;

	if (p > lex_pbeg) {
	    p = rb_enc_prev_char(lex_pbeg, p, lex_p, rb_enc_get(lex_lastline));
	    if (p > lex_pbeg) pre = "...";
	}
	if (pe < pend) {
	    pe = rb_enc_prev_char(lex_p, pe, pend, rb_enc_get(lex_lastline));
	    if (pe < pend) post = "...";
	}
	len = pe - p;
	lim = lex_p < pend ? lex_p : pend;
	i = (int)(lim - p);
	buf = ALLOCA_N(char, i+2);
	code = p;
	caret = p2 = buf;
	pe = (parser->tokp < lim ? parser->tokp : lim);
	if (p <= pe) {
	    while (p < pe) {
		*p2++ = *p++ == '\t' ? '\t' : ' ';
	    }
	    *p2++ = '^';
	    p++;
	}
	if (lim > p) {
	    memset(p2, '~', (lim - p));
	    p2 += (lim - p);
	}
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

static struct vtable *
vtable_alloc_gen(struct parser_params *parser, int line, struct vtable *prev)
{
    struct vtable *tbl = ALLOC(struct vtable);
    tbl->pos = 0;
    tbl->capa = 8;
    tbl->tbl = ALLOC_N(ID, tbl->capa);
    tbl->prev = prev;
#ifndef RIPPER
    if (yydebug) {
	rb_parser_printf(parser, "vtable_alloc:%d: %p\n", line, tbl);
    }
#endif
    return tbl;
}
#define vtable_alloc(prev) vtable_alloc_gen(parser, __LINE__, prev)

static void
vtable_free_gen(struct parser_params *parser, int line, const char *name,
		struct vtable *tbl)
{
#ifndef RIPPER
    if (yydebug) {
	rb_parser_printf(parser, "vtable_free:%d: %s(%p)\n", line, name, tbl);
    }
#endif
    if (POINTER_P(tbl)) {
	if (tbl->tbl) {
	    xfree(tbl->tbl);
	}
	xfree(tbl);
    }
}
#define vtable_free(tbl) vtable_free_gen(parser, __LINE__, #tbl, tbl)

static void
vtable_add_gen(struct parser_params *parser, int line, const char *name,
	       struct vtable *tbl, ID id)
{
#ifndef RIPPER
    if (yydebug) {
	rb_parser_printf(parser, "vtable_add:%d: %s(%p), %s\n",
			 line, name, tbl, rb_id2name(id));
    }
#endif
    if (!POINTER_P(tbl)) {
	rb_parser_fatal(parser, "vtable_add: vtable is not allocated (%p)", (void *)tbl);
	return;
    }
    if (tbl->pos == tbl->capa) {
	tbl->capa = tbl->capa * 2;
	REALLOC_N(tbl->tbl, ID, tbl->capa);
    }
    tbl->tbl[tbl->pos++] = id;
}
#define vtable_add(tbl, id) vtable_add_gen(parser, __LINE__, #tbl, tbl, id)

#ifndef RIPPER
static void
vtable_pop_gen(struct parser_params *parser, int line, const char *name,
	       struct vtable *tbl, int n)
{
    if (yydebug) {
	rb_parser_printf(parser, "vtable_pop:%d: %s(%p), %d\n",
			 line, name, tbl, n);
    }
    if (tbl->pos < n) {
	rb_parser_fatal(parser, "vtable_pop: unreachable (%d < %d)", tbl->pos, n);
	return;
    }
    tbl->pos -= n;
}
#define vtable_pop(tbl, n) vtable_pop_gen(parser, __LINE__, #tbl, tbl, n)
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

static void parser_prepare(struct parser_params *parser);

#ifndef RIPPER
static NODE *parser_append_options(struct parser_params *parser, NODE *node);

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
	VALUE coverage = rb_default_coverage(n);
	VALUE lines = RARRAY_AREF(coverage, COVERAGE_INDEX_LINES);

	rb_hash_aset(coverages, fname, coverage);

	return lines == Qnil ? Qfalse : lines;
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
    lex_prevline = lex_lastline = lex_nextline = 0;
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
	NODE *prelude;
	NODE *body = parser_append_options(parser, tree->nd_body);
	if (!opt) opt = rb_obj_hide(rb_ident_hash_new());
	rb_hash_aset(opt, rb_sym_intern_ascii_cstr("coverage_enabled"), cov);
	prelude = NEW_PRELUDE(ruby_eval_tree_begin, body, opt);
	add_mark_object(opt);
	prelude->nd_loc = body->nd_loc;
	tree->nd_body = prelude;
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
static rb_ast_t*
parser_compile_string(VALUE vparser, VALUE fname, VALUE s, int line)
{
    struct parser_params *parser;
    rb_ast_t *ast;

    TypedData_Get_Struct(vparser, struct parser_params, &parser_data_type, parser);
    parser->ast = ast = rb_ast_new();

    lex_gets = lex_get_str;
    lex_gets_ptr = 0;
    lex_input = rb_str_new_frozen(s);
    lex_pbeg = lex_p = lex_pend = 0;

    ast->root = yycompile(parser, fname, line);
    parser->ast = 0;
    RB_GC_GUARD(vparser); /* prohibit tail call optimization */

    return ast;
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
lex_io_gets(struct parser_params *parser, VALUE io)
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
    struct parser_params *parser;
    rb_ast_t *ast;

    TypedData_Get_Struct(vparser, struct parser_params, &parser_data_type, parser);
    parser->ast = ast = rb_ast_new();

    lex_gets = lex_io_gets;
    lex_input = file;
    lex_pbeg = lex_p = lex_pend = 0;

    ast->root = yycompile(parser, fname, start);
    parser->ast = 0;
    RB_GC_GUARD(vparser); /* prohibit tail call optimization */

    return ast;
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

#ifdef RIPPER
static void
parser_add_delayed_token(struct parser_params *parser, const char *tok, const char *end)
{
    if (tok < end) {
	if (!has_delayed_token()) {
	    parser->delayed = rb_str_buf_new(1024);
	    rb_enc_associate(parser->delayed, current_enc);
	    parser->delayed_line = ruby_sourceline;
	    parser->delayed_col = (int)(tok - lex_pbeg);
	}
	rb_str_buf_cat(parser->delayed, tok, end - tok);
	parser->tokp = end;
    }
}
#define add_delayed_token(tok, end) parser_add_delayed_token(parser, (tok), (end))
#else
#define add_delayed_token(tok, end) ((void)(tok), (void)(end))
#endif

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
    add_delayed_token(parser->tokp, lex_pend);
    if (heredoc_end > 0) {
	ruby_sourceline = heredoc_end;
	heredoc_end = 0;
    }
    ruby_sourceline++;
    parser->line_count++;
    lex_pbeg = lex_p = RSTRING_PTR(v);
    lex_pend = lex_p + RSTRING_LEN(v);
    token_flush(parser);
    lex_prevline = lex_lastline;
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

    if (UNLIKELY((lex_p == lex_pend) || parser->eofp || lex_nextline)) {
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
	parser->tokp = lex_p;
	yyerror0("invalid hex escape");
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
    int codepoint = scan_hex(lex_p, wide ? lex_pend - lex_p : 4, &numlen);
    literal_flush(lex_p);
    lex_p += numlen;
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
	tokcopy((int)numlen);
    }
    else if (codepoint >= 0x80) {
	rb_encoding *utf8 = rb_utf8_encoding();
	if (*encp && utf8 != *encp) {
	    static const char mixed_utf8[] = "UTF-8 mixed within %s source";
	    size_t len = sizeof(mixed_utf8) - 2 + strlen(rb_enc_name(*encp));
	    char *mesg = alloca(len);
	    snprintf(mesg, len, mixed_utf8, rb_enc_name(*encp));
	    yyerror0(mesg);
	    return wide;
	}
	*encp = utf8;
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
	if (lex_p >= lex_pend) goto unterminated;
	while (ISSPACE(c = *lex_p) && ++lex_p < lex_pend);
	while (c != close_brace) {
	    if (regexp_literal) tokadd(last);
	    if (!parser_tokadd_codepoint(parser, encp, regexp_literal, TRUE)) {
		break;
	    }
	    while (ISSPACE(c = *lex_p)) {
		if (++lex_p >= lex_pend) goto unterminated;
		last = c;
	    }
	}

	if (c != close_brace) {
	  unterminated:
	    literal_flush(lex_p);
	    yyerror0("unterminated Unicode escape");
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
        yyerror0("Invalid escape character syntax");
	pushback(c);
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
        yyerror0("Invalid escape character syntax");
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
dispose_string(struct parser_params *parser, VALUE str)
{
    rb_ast_delete_mark_object(parser->ast, str);
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

static void
parser_mixed_error(struct parser_params *parser, rb_encoding *enc1, rb_encoding *enc2)
{
    static const char mixed_msg[] = "%s mixed within %s source";
    const char *n1 = rb_enc_name(enc1), *n2 = rb_enc_name(enc2);
    const size_t len = sizeof(mixed_msg) - 4 + strlen(n1) + strlen(n2);
    char *errbuf = ALLOCA_N(char, len);
    snprintf(errbuf, len, mixed_msg, n1, n2);
    yyerror0(errbuf);
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
parser_tokadd_string(struct parser_params *parser,
		     int func, int term, int paren, long *nest,
		     rb_encoding **encp, rb_encoding **enc)
{
    int c;
    bool erred = false;

#define mixed_error(enc1, enc2) \
    (void)(erred || (parser_mixed_error(parser, enc1, enc2), erred = true))
#define mixed_escape(beg, enc1, enc2) \
    (void)(erred || (parser_mixed_escape(parser, beg, enc1, enc2), erred = true))

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
	    literal_flush(lex_p - 1);
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
		if (!parser_tokadd_utf8(parser, enc, term,
					func & STR_FUNC_SYMBOL,
					func & STR_FUNC_REGEXP)) {
		    return -1;
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
		    if ((c = tokadd_escape(enc)) < 0)
			return -1;
		    if (*enc && *enc != *encp) {
			mixed_escape(parser->tokp+2, *enc, *encp);
		    }
		    continue;
		}
		else if (func & STR_FUNC_EXPAND) {
		    pushback(c);
		    if (func & STR_FUNC_ESCAPE) tokadd('\\');
		    c = read_escape(0, enc);
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
	    if (!*enc) {
		*enc = *encp;
	    }
	    else if (*enc != *encp) {
		mixed_error(*enc, *encp);
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
	    if (!*enc) {
		*enc = *encp;
	    }
	    else if (*enc != *encp) {
		mixed_error(*enc, *encp);
		continue;
	    }
        }
	tokadd(c);
    }
  terminate:
    if (*enc) *encp = *enc;
    return c;
}

/* imemo_parser_strterm for literal */
#define NEW_STRTERM(func, term, paren) \
	(rb_strterm_t*)rb_imemo_new(imemo_parser_strterm, (VALUE)(func), (VALUE)(paren), (VALUE)(term), 0)

#ifdef RIPPER
static void
token_flush_string_content(struct parser_params *parser, rb_encoding *enc)
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

#define flush_string_content(enc) token_flush_string_content(parser, (enc))
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

static enum yytokentype
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

#define IS_ARG() IS_lex_state(EXPR_ARG_ANY)
#define IS_END() IS_lex_state(EXPR_END_ANY)
#define IS_BEG() (IS_lex_state(EXPR_BEG_ANY) || IS_lex_state_all(EXPR_ARG|EXPR_LABELED))
#define IS_SPCARG(c) (IS_ARG() && space_seen && !ISSPACE(c))
#define IS_LABEL_POSSIBLE() (\
	(IS_lex_state(EXPR_LABEL|EXPR_ENDFN) && !cmd_state) || \
	IS_ARG())
#define IS_LABEL_SUFFIX(n) (peek_n(':',(n)) && !peek_n(':', (n)+1))
#define IS_AFTER_OPERATOR() IS_lex_state(EXPR_FNAME | EXPR_DOT)

static inline enum yytokentype
parser_string_term(struct parser_params *parser, int func)
{
    lex_strterm = 0;
    if (func & STR_FUNC_REGEXP) {
	set_yylval_num(regx_options());
	dispatch_scan_event(tREGEXP_END);
	SET_LEX_STATE(EXPR_END|EXPR_ENDARG);
	return tREGEXP_END;
    }
    if ((func & STR_FUNC_LABEL) && IS_LABEL_SUFFIX(0)) {
	nextc();
	SET_LEX_STATE(EXPR_BEG|EXPR_LABEL);
	return tLABEL_END;
    }
    SET_LEX_STATE(EXPR_END|EXPR_ENDARG);
    return tSTRING_END;
}

static enum yytokentype
parser_parse_string(struct parser_params *parser, rb_strterm_literal_t *quote)
{
    int func = (int)quote->u1.func;
    int term = (int)quote->u3.term;
    int paren = (int)quote->u2.paren;
    int c, space = 0;
    rb_encoding *enc = current_enc;
    rb_encoding *base_enc = 0;
    VALUE lit;

    if (func & STR_FUNC_TERM) {
	if (func & STR_FUNC_QWORDS) nextc(); /* delayed term */
	SET_LEX_STATE(EXPR_END|EXPR_ENDARG);
	lex_strterm = 0;
	return func & STR_FUNC_REGEXP ? tREGEXP_END : tSTRING_END;
    }
    c = nextc();
    if ((func & STR_FUNC_QWORDS) && ISSPACE(c)) {
	do {c = nextc();} while (ISSPACE(c));
	space = 1;
    }
    if (func & STR_FUNC_LIST) {
	quote->u1.func &= ~STR_FUNC_LIST;
	space = 1;
    }
    if (c == term && !quote->u0.nest) {
	if (func & STR_FUNC_QWORDS) {
	    quote->u1.func |= STR_FUNC_TERM;
	    pushback(c); /* dispatch the term at tSTRING_END */
	    add_delayed_token(parser->tokp, lex_p);
	    return ' ';
	}
	return parser_string_term(parser, func);
    }
    if (space) {
	pushback(c);
	add_delayed_token(parser->tokp, lex_p);
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
    if (tokadd_string(func, term, paren, &quote->u0.nest,
		      &enc, &base_enc) == -1) {
	if (parser->eofp) {
#ifndef RIPPER
# define unterminated_literal(mesg) yyerror0(mesg)
#else
# define unterminated_literal(mesg) compile_error(PARSER_ARG  mesg)
#endif
	    literal_flush(lex_p);
	    if (func & STR_FUNC_REGEXP) {
		unterminated_literal("unterminated regexp meets end of file");
	    }
	    else {
		unterminated_literal("unterminated string meets end of file");
	    }
	    quote->u1.func |= STR_FUNC_TERM;
	}
    }

    tokfix();
    add_mark_object(lit = STR_NEW3(tok(), toklen(), enc, func));
    set_yylval_str(lit);
    flush_string_content(enc);

    return tSTRING_CONTENT;
}

static enum yytokentype
parser_heredoc_identifier(struct parser_params *parser)
{
    int c = nextc(), term, func = 0, term_len = 2; /* length of "<<" */
    enum yytokentype token = tSTRING_BEG;
    long len;
    int newline = 0;
    int indent = 0;

    if (c == '-') {
	c = nextc();
	term_len++;
	func = STR_FUNC_INDENT;
    }
    else if (c == '~') {
	c = nextc();
	term_len++;
	func = STR_FUNC_INDENT;
	indent = INT_MAX;
    }
    switch (c) {
      case '\'':
	term_len++;
	func |= str_squote; goto quoted;
      case '"':
	term_len++;
	func |= str_dquote; goto quoted;
      case '`':
	term_len++;
	token = tXSTRING_BEG;
	func |= str_xquote; goto quoted;

      quoted:
	term_len++;
	newtok();
	tokadd(term_len);
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
	tokadd(term_len);
	tokadd(func |= str_dquote);
	do {
	    if (tokadd_mbchar(c) == -1) return 0;
	} while ((c = nextc()) != -1 && parser_is_identchar());
	pushback(c);
	break;
    }

    tokenbuf[0] = tokenbuf[0] + toklen() - 2;
    tokfix();
    dispatch_scan_event(tHEREDOC_BEG);
    len = lex_p - lex_pbeg;
    lex_goto_eol(parser);

    lex_strterm = (rb_strterm_t*)rb_imemo_new(imemo_parser_strterm,
					      STR_NEW(tok(), toklen()),	/* term */
					      lex_lastline,		/* lastline */
					      len,			/* lastidx */
					      ruby_sourceline);
    lex_strterm->flags |= STRTERM_HEREDOC;

    token_flush(parser);
    heredoc_indent = indent;
    heredoc_line_indent = 0;
    return token;
}

static void
parser_heredoc_restore(struct parser_params *parser, rb_strterm_heredoc_t *here)
{
    VALUE line;

    lex_strterm = 0;
    line = here->lastline;
    lex_lastline = line;
    lex_pbeg = RSTRING_PTR(line);
    lex_pend = lex_pbeg + RSTRING_LEN(line);
    lex_p = lex_pbeg + here->u3.lastidx;
    heredoc_end = ruby_sourceline;
    ruby_sourceline = (int)here->sourceline;
    token_flush(parser);
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
    int indent = heredoc_indent;

    if (indent <= 0) return root;
    heredoc_indent = 0;
    if (!root) return root;

    node = str_node = root;
    if (nd_type(root) == NODE_ARRAY) str_node = root->nd_head;

    while (str_node) {
	VALUE lit = str_node->nd_lit;
	if (str_node->flags & NODE_FL_NEWLINE) {
	    dedent_string(lit, indent);
	}

	str_node = 0;
	while ((node = node->nd_next) != 0 && nd_type(node) == NODE_ARRAY) {
	    if ((str_node = node->nd_head) != 0) {
		enum node_type type = nd_type(str_node);
		if (type == NODE_STR || type == NODE_DSTR) break;
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
	    literal_flush(lex_p);
	    return 0;
	}
	pushback(c);
	if (c == '.') {
	    c = peekc_n(1);
	    if (ISDIGIT(c)) {
		yyerror0("unexpected fraction part after numeric literal");
		lex_p += 2;
		while (parser_is_identchar()) nextc();
	    }
	}
	break;
    }
    return result;
}

static enum yytokentype
parser_set_number_literal(struct parser_params *parser, VALUE v,
			  enum yytokentype type, int suffix)
{
    if (suffix & NUM_SUFFIX_I) {
	v = rb_complex_raw(INT2FIX(0), v);
	type = tIMAGINARY;
    }
    set_yylval_literal(v);
    add_mark_object(v);
    SET_LEX_STATE(EXPR_END|EXPR_ENDARG);
    return type;
}

static int
parser_set_integer_literal(struct parser_params *parser, VALUE v, int suffix)
{
    enum yytokentype type = tINTEGER;
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
    token_flush(parser);
}

#define dispatch_heredoc_end() ripper_dispatch_heredoc_end(parser)
#else
#define dispatch_heredoc_end() ((void)0)
#endif

static enum yytokentype
parser_here_document(struct parser_params *parser, rb_strterm_heredoc_t *here)
{
    int c, func, indent = 0;
    const char *eos, *p, *pend;
    long len;
    VALUE str = 0;
    rb_encoding *enc = current_enc;
    rb_encoding *base_enc = 0;
    int bol;

    eos = RSTRING_PTR(here->term);
    len = RSTRING_LEN(here->term) - 2; /* here->term includes term_len and func */
    eos++; /* skip term_len */
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
	heredoc_restore(&lex_strterm->u.heredoc);
	lex_strterm = 0;
	return 0;
    }
    bol = was_bol();
    /* `heredoc_line_indent == -1` means
     * - "after an interpolation in the same line", or
     * - "in a continuing line"
     */
    if (bol &&
        (heredoc_line_indent != -1 || (heredoc_line_indent = 0)) &&
	whole_match_p(eos, len, indent)) {
	dispatch_heredoc_end();
	heredoc_restore(&lex_strterm->u.heredoc);
	lex_strterm = 0;
	SET_LEX_STATE(EXPR_END);
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
		goto flush_str;
	    }
	    if (nextc() == -1) {
		if (str) {
		    dispose_string(parser, str);
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
            enc = current_enc;
	    if ((c = tokadd_string(func, '\n', 0, NULL, &enc, &base_enc)) == -1) {
		if (parser->eofp) goto error;
		goto restore;
	    }
	    if (c != '\n') {
		if (c == '\\') heredoc_line_indent = -1;
	      flush:
		str = STR_NEW3(tok(), toklen(), enc, func);
	      flush_str:
		set_yylval_str(str);
		add_mark_object(str);
#ifndef RIPPER
		if (bol) yylval.node->flags |= NODE_FL_NEWLINE;
#endif
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
    heredoc_restore(&lex_strterm->u.heredoc);
    lex_strterm = NEW_STRTERM(func | STR_FUNC_TERM, 0, 0);
    set_yylval_str(str);
    add_mark_object(str);
#ifndef RIPPER
    if (bol) yylval.node->flags |= NODE_FL_NEWLINE;
#endif
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
    return (dyna_in_block() && dvar_defined(id)) || local_id(id);
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
#define no_digits() do {yyerror0("numeric literal without digits"); return 0;} while (0)
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
	    yyerror0("Invalid octal digit");
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
	literal_flush(lex_p - 1);
	snprintf(tmp, sizeof(tmp), "trailing `%c' in number", nondigit);
	yyerror0(tmp);
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

static enum yytokentype
parse_qmark(struct parser_params *parser, int space_seen)
{
    rb_encoding *enc;
    register int c;
    VALUE lit;

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
	    enc = rb_utf8_encoding();
	    if (!parser_tokadd_utf8(parser, &enc, -1, 0, 0))
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
    add_mark_object(lit = STR_NEW3(tok(), toklen(), enc, 0));
    set_yylval_str(lit);
    SET_LEX_STATE(EXPR_END);
    return tCHAR;
}

static enum yytokentype
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
		yyerror0("unknown type of %string");
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
	    return tWORDS_BEG;

	  case 'w':
	    lex_strterm = NEW_STRTERM(str_sword, term, paren);
	    return tQWORDS_BEG;

	  case 'I':
	    lex_strterm = NEW_STRTERM(str_dword, term, paren);
	    return tSYMBOLS_BEG;

	  case 'i':
	    lex_strterm = NEW_STRTERM(str_sword, term, paren);
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
	    yyerror0("unknown type of %string");
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
    return warn_balanced('%', "%%", "string literal");
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

static enum yytokentype
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

static enum yytokentype
parse_atmark(struct parser_params *parser, const enum lex_state_e last_state)
{
    enum yytokentype result = tIVAR;
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

static enum yytokentype
parse_ident(struct parser_params *parser, int c, int cmd_state)
{
    enum yytokentype result;
    int mb = ENC_CODERANGE_7BIT;
    const enum lex_state_e last_state = lex_state;
    ID ident;

    do {
	if (!ISASCII(c)) mb = ENC_CODERANGE_UNKNOWN;
	if (tokadd_mbchar(c) == -1) return 0;
	c = nextc();
    } while (parser_is_identchar());
    if ((c == '!' || c == '?') && !peek('=')) {
	result = tFID;
	tokadd(c);
    }
    else if (c == '=' && IS_lex_state(EXPR_FNAME) &&
	     (!peek('~') && !peek('>') && (!peek('=') || (peek_n('>', 1))))) {
	result = tIDENTIFIER;
	tokadd(c);
    }
    else {
	result = tCONSTANT;	/* assume provisionally */
	pushback(c);
    }
    tokfix();

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
    if (result == tCONSTANT && is_local_id(ident)) result = tIDENTIFIER;
    if (!IS_lex_state_for(last_state, EXPR_DOT|EXPR_FNAME) &&
	(result == tIDENTIFIER) && /* not EXPR_FNAME, not attrasgn */
	lvar_defined(ident)) {
	SET_LEX_STATE(EXPR_END|EXPR_LABEL);
    }
    return result;
}

static enum yytokentype
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
	if (lex_strterm->flags & STRTERM_HEREDOC) {
	    return here_document(&lex_strterm->u.heredoc);
	}
	else {
	    token_flush(parser);
	    return parse_string(&lex_strterm->u.literal);
	}
    }
    cmd_state = command_start;
    command_start = FALSE;
    parser->token_seen = TRUE;
  retry:
    last_state = lex_state;
#ifndef RIPPER
    token_flush(parser);
#endif
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
#ifndef RIPPER
		if (lex_prevline && !parser->eofp) lex_lastline = lex_prevline;
		lex_pbeg = RSTRING_PTR(lex_lastline);
		lex_pend = lex_p = lex_pbeg + RSTRING_LEN(lex_lastline);
		pushback(1); /* always pushback */
		parser->tokp = lex_p;
#else
		lex_goto_eol(parser);
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
		c = warn_balanced((enum ruby_method_ids)tPOW, "**", "argument prefix");
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
		c = warn_balanced('*', "*", "argument prefix");
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
	    return warn_balanced((enum ruby_method_ids)tLSHFT, "<<", "here document");
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
	    if ((c != ':') ||
		(c = peekc_n(1)) == -1 ||
		!(c == '\'' || c == '"' ||
		  is_identchar((lex_p+1), lex_pend, current_enc))) {
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
	return warn_balanced('+', "+", "unary operator");

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
	return warn_balanced('-', "-", "unary operator");

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
	    yyerror0("no .<digit> floating literal anymore; put 0 before dot");
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
	    SET_LEX_STATE(EXPR_END);
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
	    c = warn_balanced(':', ":", "symbol literal");
	    SET_LEX_STATE(EXPR_BEG);
	    return c;
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
	return warn_balanced('/', "/", "regexp literal");

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
	paren_nest++;
	COND_PUSH(0);
	CMDARG_PUSH(0);
	SET_LEX_STATE(EXPR_BEG|EXPR_LABEL);
	return c;

      case '[':
	paren_nest++;
	if (IS_AFTER_OPERATOR()) {
	    if ((c = nextc()) == ']') {
		SET_LEX_STATE(EXPR_ARG);
		if ((c = nextc()) == '=') {
		    return tASET;
		}
		pushback(c);
		return tAREF;
	    }
	    pushback(c);
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
	SET_LEX_STATE(c == tLBRACE_ARG ? EXPR_BEG : EXPR_BEG|EXPR_LABEL);
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

static enum yytokentype
yylex(YYSTYPE *lval, YYLTYPE *yylloc, struct parser_params *parser)
{
    enum yytokentype t;

    parser->lval = lval;
    lval->val = Qundef;
    t = parser_yylex(parser);
    if (has_delayed_token())
	dispatch_delayed_token(t);
    else if (t != 0)
	dispatch_scan_event(t);

    if (lex_strterm && (lex_strterm->flags & STRTERM_HEREDOC))
	RUBY_SET_YYLLOC_FROM_STRTERM_HEREDOC(*yylloc);
    else
	RUBY_SET_YYLLOC(*yylloc);

    return t;
}

#define LVAR_USED ((ID)1 << (sizeof(ID) * CHAR_BIT - 1))

static NODE*
node_newnode(struct parser_params *parser, enum node_type type, VALUE a0, VALUE a1, VALUE a2)
{
    NODE *n = rb_ast_newnode(parser->ast);

    rb_node_init(n, type, a0, a1, a2);

    nd_set_line(n, ruby_sourceline);
    /* mark not cared lineno to 0 and column to -1 */
    nd_set_first_lineno(n,  0);
    nd_set_first_column(n, -1);
    nd_set_last_lineno(n,  0);
    nd_set_last_column(n, -1);
    return n;
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

static NODE *
nd_set_loc(NODE *nd, const YYLTYPE *location)
{
    nd->nd_loc = *location;
    nd_set_line(nd, location->first_loc.lineno);
    return nd;
}

static NODE*
block_append_gen(struct parser_params *parser, NODE *head, NODE *tail, const YYLTYPE *location)
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
	nd_set_loc(end, location);
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
	nd_set_loc(tail, location);
	tail->nd_end = tail;
    }
    end->nd_next = tail;
    h->nd_end = tail->nd_end;
    nd_set_last_loc(head, nd_last_loc(tail));
    return head;
}

/* append item to the list */
static NODE*
list_append_gen(struct parser_params *parser, NODE *list, NODE *item)
{
    NODE *last;

    if (list == 0) return new_list(item, &item->nd_loc);
    if (list->nd_next) {
	last = list->nd_next->nd_end;
    }
    else {
	last = list;
    }

    list->nd_alen += 1;
    last->nd_next = new_list(item, &item->nd_loc);
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
literal_concat_gen(struct parser_params *parser, NODE *head, NODE *tail, const YYLTYPE *location)
{
    enum node_type htype;
    NODE *headlast;
    VALUE lit;

    if (!head) return tail;
    if (!tail) return head;

    htype = nd_type(head);
    if (htype == NODE_EVSTR) {
	NODE *node = new_dstr(STR_NEW0(), location);
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
		rb_discard_node(head);
		rb_discard_node(tail);
		return 0;
	    }
	    rb_discard_node(tail);
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
	    rb_discard_node(head);
	    head = tail;
	}
	else if (NIL_P(tail->nd_lit)) {
	  append:
	    head->nd_alen += tail->nd_alen - 1;
	    head->nd_next->nd_end->nd_next = tail->nd_next;
	    head->nd_next->nd_end = tail->nd_next->nd_end;
	    rb_discard_node(tail);
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
	    tail->nd_head = new_str(tail->nd_lit, location);
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
	node = list_append(new_dstr(STR_NEW0(), &node->nd_loc), node);
    }
    return node;
}

static NODE *
new_evstr_gen(struct parser_params *parser, NODE *node, const YYLTYPE *location)
{
    NODE *head = node;
    NODE *evstr;

    if (node) {
	switch (nd_type(node)) {
	  case NODE_STR: case NODE_DSTR: case NODE_EVSTR:
	    return node;
	}
    }
    evstr = NEW_EVSTR(head);
    nd_set_loc(evstr, location);
    return evstr;
}

static NODE *
call_bin_op_gen(struct parser_params *parser, NODE *recv, ID id, NODE *arg1,
		const YYLTYPE *op_loc, const YYLTYPE *location)
{
    NODE *expr;
    value_expr(recv);
    value_expr(arg1);
    expr = NEW_OPCALL(recv, id, new_list(arg1, &arg1->nd_loc));
    nd_set_line(expr, op_loc->first_loc.lineno);
    expr->nd_loc = *location;
    return expr;
}

static NODE *
call_uni_op_gen(struct parser_params *parser, NODE *recv, ID id, const YYLTYPE *op_loc, const YYLTYPE *location)
{
    NODE *opcall;
    value_expr(recv);
    opcall = NEW_OPCALL(recv, id, 0);
    opcall->nd_loc = *location;
    nd_set_line(opcall, op_loc->first_loc.lineno);
    return opcall;
}

static NODE *
new_qcall_gen(struct parser_params* parser, ID atype, NODE *recv, ID mid, NODE *args, const YYLTYPE *location)
{
    NODE *qcall = NEW_QCALL(atype, recv, mid, args);
    qcall->nd_loc = *location;
    return qcall;
}

#define nd_once_body(node) (nd_type(node) == NODE_SCOPE ? (node)->nd_body : node)
static NODE*
match_op_gen(struct parser_params *parser, NODE *node1, NODE *node2, const YYLTYPE *op_loc, const YYLTYPE *location)
{
    NODE *n;
    int line = op_loc->first_loc.lineno;

    value_expr(node1);
    value_expr(node2);
    if (node1 && (n = nd_once_body(node1)) != 0) {
	switch (nd_type(n)) {
	  case NODE_DREGX:
	    {
		NODE *match = NEW_MATCH2(node1, node2);
		match->nd_loc = *location;
		nd_set_line(match, line);
		return match;
	    }

	  case NODE_LIT:
	    if (RB_TYPE_P(n->nd_lit, T_REGEXP)) {
		const VALUE lit = n->nd_lit;
		NODE *match = NEW_MATCH2(node1, node2);
		match->nd_args = reg_named_capture_assign(lit, location);
		match->nd_loc = *location;
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
	    match3 = NEW_MATCH3(node2, node1);
	    match3->nd_loc = *location;
	    nd_set_line(match3, line);
	    return match3;
	}
    }

    n = new_call(node1, tMATCH, new_list(node2, &node2->nd_loc), location);
    nd_set_line(n, line);
    return n;
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
gettable_gen(struct parser_params *parser, ID id, const YYLTYPE *location)
{
    ID *vidp = NULL;
    NODE *node;
    switch (id) {
      case keyword_self:
	node = NEW_SELF();
	nd_set_loc(node, location);
	return node;
      case keyword_nil:
	node = NEW_NIL();
	nd_set_loc(node, location);
	return node;
      case keyword_true:
	node = NEW_TRUE();
	nd_set_loc(node, location);
	return node;
      case keyword_false:
	node = NEW_FALSE();
	nd_set_loc(node, location);
	return node;
      case keyword__FILE__:
	node = new_str(rb_str_dup(ruby_sourcefile_string), location);
	return node;
      case keyword__LINE__:
	return new_lit(INT2FIX(tokline), location);
      case keyword__ENCODING__:
	return new_lit(rb_enc_from_encoding(current_enc), location);
    }
    switch (id_type(id)) {
      case ID_LOCAL:
	if (dyna_in_block() && dvar_defined_ref(id, vidp)) {
	    if (id == current_arg) {
		rb_warn1("circular argument reference - %"PRIsWARN, rb_id2str(id));
	    }
	    if (vidp) *vidp |= LVAR_USED;
	    node = new_dvar(id, location);
	    return node;
	}
	if (local_id_ref(id, vidp)) {
	    if (id == current_arg) {
		rb_warn1("circular argument reference - %"PRIsWARN, rb_id2str(id));
	    }
	    if (vidp) *vidp |= LVAR_USED;
	    node = new_lvar(id, location);
	    return node;
	}
# if WARN_PAST_SCOPE
	if (!in_defined && RTEST(ruby_verbose) && past_dvar_p(parser, id)) {
	    rb_warning1("possible reference to past scope - %"PRIsWARN, rb_id2str(id));
	}
# endif
	/* method call without arguments */
	node = NEW_VCALL(id);
	nd_set_loc(node, location);
	return node;
      case ID_GLOBAL:
	node = new_gvar(id, location);
	return node;
      case ID_INSTANCE:
	node = new_ivar(id, location);
	return node;
      case ID_CONST:
	node = NEW_CONST(id);
	nd_set_loc(node, location);
	return node;
      case ID_CLASS:
	node = NEW_CVAR(id);
	nd_set_loc(node, location);
	return node;
    }
    compile_error(PARSER_ARG "identifier %"PRIsVALUE" is not valid to get", rb_id2str(id));
    return 0;
}

static NODE *
opt_arg_append(NODE *opt_list, NODE *opt)
{
    NODE *opts = opt_list;
    opts->nd_loc.last_loc = opt->nd_loc.last_loc;

    while (opts->nd_next) {
	opts = opts->nd_next;
	opts->nd_loc.last_loc = opt->nd_loc.last_loc;
    }
    opts->nd_next = opt;

    return opt_list;
}

static NODE *
kwd_append(NODE *kwlist, NODE *kw)
{
    if (kwlist) {
	NODE *kws = kwlist;
	kws->nd_loc.last_loc = kw->nd_loc.last_loc;
	while (kws->nd_next) {
	    kws = kws->nd_next;
	    kws->nd_loc.last_loc = kw->nd_loc.last_loc;
	}
	kws->nd_next = kw;
    }
    return kwlist;
}

static NODE *
new_defined_gen(struct parser_params *parser, NODE *expr, const YYLTYPE *location)
{
    NODE *defined = NEW_DEFINED(remove_begin_all(expr));
    nd_set_loc(defined, location);
    return defined;
}

static NODE *
new_regexp_gen(struct parser_params *parser, NODE *node, int options, const YYLTYPE *location)
{
    NODE *list, *prev;
    VALUE lit;

    if (!node) {
	return new_lit(reg_compile(STR_NEW0(), options), location);
    }
    switch (nd_type(node)) {
      case NODE_STR:
	{
	    VALUE src = node->nd_lit;
	    nd_set_type(node, NODE_LIT);
	    nd_set_loc(node, location);
	    add_mark_object(node->nd_lit = reg_compile(src, options));
	}
	break;
      default:
	add_mark_object(lit = STR_NEW0());
	node = NEW_NODE(NODE_DSTR, lit, 1, new_list(node, location));
      case NODE_DSTR:
	nd_set_type(node, NODE_DREGX);
	nd_set_loc(node, location);
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
		    rb_discard_node(list->nd_head);
		    rb_discard_node(list);
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
	    add_mark_object(node->nd_lit = reg_compile(src, options));
	}
	if (options & RE_OPTION_ONCE) {
	    node = NEW_NODE(NODE_SCOPE, 0, node, 0);
	    nd_set_loc(node, location);
	}
	break;
    }
    return node;
}

static NODE *
new_lit_gen(struct parser_params *parser, VALUE sym, const YYLTYPE *location)
{
    NODE *lit = NEW_LIT(sym);
    add_mark_object(sym);
    nd_set_loc(lit, location);
    return lit;
}

static NODE *
new_list_gen(struct parser_params *parser, NODE *item, const YYLTYPE *location)
{
    NODE *list = NEW_LIST(item);
    nd_set_loc(list, location);
    return list;
}

static NODE *
new_str_gen(struct parser_params *parser, VALUE str, const YYLTYPE *location)
{
    NODE *nd_str = NEW_STR(str);
    add_mark_object(str);
    nd_set_loc(nd_str, location);
    return nd_str;
}

static NODE *
new_dvar_gen(struct parser_params *parser, ID id, const YYLTYPE *location)
{
    NODE *dvar = NEW_DVAR(id);
    nd_set_loc(dvar, location);
    return dvar;
}

static NODE *
new_resbody_gen(struct parser_params *parser, NODE *exc_list, NODE *stmt, NODE *rescue, const YYLTYPE *location)
{
    NODE *resbody = NEW_RESBODY(exc_list, stmt, rescue);
    nd_set_loc(resbody, location);
    return resbody;
}

static NODE *
new_errinfo_gen(struct parser_params *parser, const YYLTYPE *location)
{
    NODE *errinfo = NEW_ERRINFO();
    nd_set_loc(errinfo, location);
    return errinfo;
}

static NODE *
new_call_gen(struct parser_params *parser, NODE *recv, ID mid, NODE *args, const YYLTYPE *location)
{
    NODE *call = NEW_CALL(recv, mid, args);
    nd_set_loc(call, location);
    return call;
}

static NODE *
new_fcall_gen(struct parser_params *parser, ID mid, NODE *args, const YYLTYPE *location)
{
    NODE *fcall = NEW_FCALL(mid, args);
    nd_set_loc(fcall, location);
    return fcall;
}

static NODE *
new_for_gen(struct parser_params *parser, NODE *var, NODE *iter, NODE *body, const YYLTYPE *location)
{
    NODE *nd_for = NEW_FOR(var, iter, body);
    nd_set_loc(nd_for, location);
    return nd_for;
}

static NODE *
new_gvar_gen(struct parser_params *parser, ID id, const YYLTYPE *location)
{
    NODE *gvar = NEW_GVAR(id);
    nd_set_loc(gvar, location);
    return gvar;
}

static NODE *
new_lvar_gen(struct parser_params *parser, ID id, const YYLTYPE *location)
{
    NODE *lvar = NEW_LVAR(id);
    nd_set_loc(lvar, location);
    return lvar;
}

static NODE *
new_dstr_gen(struct parser_params *parser, VALUE str, const YYLTYPE *location)
{
    NODE *dstr = NEW_DSTR(str);
    add_mark_object(str);
    nd_set_loc(dstr, location);
    return dstr;
}

static NODE *
new_rescue_gen(struct parser_params *parser, NODE *b, NODE *res, NODE *e, const YYLTYPE *location)
{
    NODE *rescue = NEW_RESCUE(b, res, e);
    nd_set_loc(rescue, location);
    return rescue;
}

static NODE *
new_undef_gen(struct parser_params *parser, NODE *i, const YYLTYPE *location)
{
    NODE *undef = NEW_UNDEF(i);
    nd_set_loc(undef, location);
    return undef;
}

static NODE *
new_zarray_gen(struct parser_params *parser, const YYLTYPE *location)
{
    NODE *zarray = NEW_ZARRAY();
    nd_set_loc(zarray, location);
    return zarray;
}

static NODE *
new_ivar_gen(struct parser_params *parser, ID id, const YYLTYPE *location)
{
    NODE *ivar = NEW_IVAR(id);
    nd_set_loc(ivar, location);
    return ivar;
}

static NODE *
new_postarg_gen(struct parser_params *parser, NODE *i, NODE *v, const YYLTYPE *location)
{
    NODE *postarg = NEW_POSTARG(i, v);
    nd_set_loc(postarg, location);
    return postarg;
}

static NODE *
new_cdecl_gen(struct parser_params *parser, ID v, NODE *val, NODE *path, const YYLTYPE *location)
{
    NODE *nd_cdecl = NEW_CDECL(v, val, path);
    nd_set_loc(nd_cdecl, location);
    return nd_cdecl;
}

static NODE *
new_scope_gen(struct parser_params *parser, NODE *a, NODE *b, const YYLTYPE *location)
{
    NODE *scope = NEW_SCOPE(a, b);
    nd_set_loc(scope, location);
    return scope;
}

static NODE *
new_begin_gen(struct parser_params *parser, NODE *b, const YYLTYPE *location)
{
    NODE *begin = NEW_BEGIN(b);
    nd_set_loc(begin, location);
    return begin;
}

static NODE *
new_masgn_gen(struct parser_params *parser, NODE *l, NODE *r, const YYLTYPE *location)
{
    NODE *masgn = NEW_MASGN(l, r);
    nd_set_loc(masgn, location);
    return masgn;
}


static NODE *
new_kw_arg_gen(struct parser_params *parser, NODE *k, const YYLTYPE *location)
{
    NODE *kw_arg;
    if (!k) return 0;
    kw_arg = NEW_KW_ARG(0, (k));
    nd_set_loc(kw_arg, location);
    return kw_arg;
}

static NODE *
new_xstring_gen(struct parser_params *parser, NODE *node, const YYLTYPE *location)
{
    if (!node) {
	VALUE lit = STR_NEW0();
	NODE *xstr = NEW_XSTR(lit);
	add_mark_object(lit);
	xstr->nd_loc = *location;
	return xstr;
    }
    switch (nd_type(node)) {
      case NODE_STR:
	nd_set_type(node, NODE_XSTR);
	nd_set_loc(node, location);
	break;
      case NODE_DSTR:
	nd_set_type(node, NODE_DXSTR);
	nd_set_loc(node, location);
	break;
      default:
	node = NEW_NODE(NODE_DXSTR, Qnil, 1, new_list(node, location));
	nd_set_loc(node, location);
	break;
    }
    return node;
}

static NODE *
new_body_gen(struct parser_params *parser, NODE *param, NODE *stmt, const YYLTYPE *location)
{
    NODE *iter = NEW_ITER(param, stmt);
    nd_set_loc(iter->nd_body, location);
    nd_set_loc(iter, location);
    return iter;

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
    compile_error(PARSER_ARG "identifier %"PRIsVALUE" is not valid to get", rb_id2str(id));
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

#ifndef RIPPER
const char rb_parser_lex_state_names[][13] = {
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
flush_debug_buffer(struct parser_params *parser, VALUE out, VALUE str)
{
    VALUE mesg = parser->debug_buffer;

    if (!NIL_P(mesg) && RSTRING_LEN(mesg)) {
	parser->debug_buffer = Qnil;
	rb_io_puts(1, &mesg, out);
    }
    if (!NIL_P(str) && RSTRING_LEN(str)) {
	rb_io_write(parser->debug_output, str);
    }
}

enum lex_state_e
rb_parser_trace_lex_state(struct parser_params *parser, enum lex_state_e from,
			  enum lex_state_e to, int line)
{
    VALUE mesg;
    mesg = rb_str_new_cstr("lex_state: ");
    append_lex_state_name(from, mesg);
    rb_str_cat_cstr(mesg, " -> ");
    append_lex_state_name(to, mesg);
    rb_str_catf(mesg, " at line %d\n", line);
    flush_debug_buffer(parser, parser->debug_output, mesg);
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
rb_parser_show_bitstack(struct parser_params *parser, stack_type stack,
			const char *name, int line)
{
    VALUE mesg = rb_sprintf("%s: ", name);
    append_bitstack_value(stack, mesg);
    rb_str_catf(mesg, " at line %d\n", line);
    flush_debug_buffer(parser, parser->debug_output, mesg);
}

void
rb_parser_fatal(struct parser_params *parser, const char *fmt, ...)
{
    va_list ap;
    VALUE mesg = rb_str_new_cstr("internal parser error: ");

    va_start(ap, fmt);
    rb_str_vcatf(mesg, fmt, ap);
    va_end(ap);
#ifndef RIPPER
    parser_yyerror(parser, RSTRING_PTR(mesg));
    RB_GC_GUARD(mesg);
#else
    dispatch1(parse_error, mesg);
    ripper_error();
#endif /* !RIPPER */

    mesg = rb_str_new(0, 0);
    append_lex_state_name(lex_state, mesg);
    compile_error(PARSER_ARG "lex_state: %"PRIsVALUE, mesg);
    rb_str_resize(mesg, 0);
    append_bitstack_value(cond_stack, mesg);
    compile_error(PARSER_ARG "cond_stack: %"PRIsVALUE, mesg);
    rb_str_resize(mesg, 0);
    append_bitstack_value(cmdarg_stack, mesg);
    compile_error(PARSER_ARG "cmdarg_stack: %"PRIsVALUE, mesg);
    if (parser->debug_output == rb_stdout)
	parser->debug_output = rb_stderr;
    yydebug = TRUE;
}

void
rb_parser_set_location_from_strterm_heredoc(struct parser_params *parser, rb_strterm_heredoc_t *here, YYLTYPE *yylloc)
{
    const char *eos = RSTRING_PTR(here->term);
    int term_len = (int)eos[0];

    yylloc->first_loc.lineno = (int)here->sourceline;
    yylloc->first_loc.column = (int)(here->u3.lastidx - term_len);
    yylloc->last_loc.lineno  = (int)here->sourceline;
    yylloc->last_loc.column  = (int)(here->u3.lastidx);
}

void
rb_parser_set_location_of_none(struct parser_params *parser, YYLTYPE *yylloc)
{
    yylloc->first_loc.lineno = ruby_sourceline;
    yylloc->first_loc.column = (int)(parser->tokp - lex_pbeg);
    yylloc->last_loc.lineno = ruby_sourceline;
    yylloc->last_loc.column = (int)(parser->tokp - lex_pbeg);
}

void
rb_parser_set_location(struct parser_params *parser, YYLTYPE *yylloc)
{
    yylloc->first_loc.lineno = ruby_sourceline;
    yylloc->first_loc.column = (int)(parser->tokp - lex_pbeg);
    yylloc->last_loc.lineno = ruby_sourceline;
    yylloc->last_loc.column = (int)(lex_p - lex_pbeg);
}
#endif /* !RIPPER */

#ifndef RIPPER
static NODE*
assignable_result0(NODE *node, const YYLTYPE *location)
{
    if (node) {
	nd_set_loc(node, location);
    }
    return node;
}
#endif /* !RIPPER */

#ifdef RIPPER
static VALUE
assignable_gen(struct parser_params *parser, VALUE lhs)
#else
static NODE*
assignable_gen(struct parser_params *parser, ID id, NODE *val, const YYLTYPE *location)
#endif
{
#ifdef RIPPER
    ID id = get_id(lhs);
# define assignable_result(x) (lhs)
# define assignable_error() (lhs)
# define parser_yyerror(parser, x) (lhs = assign_error_gen(parser, lhs))
#else
# define assignable_result(x) assignable_result0(x, location)
# define assignable_error() new_begin(0, location)
#endif
    if (!id) return assignable_error();
    switch (id) {
      case keyword_self:
	yyerror0("Can't change the value of self");
	goto error;
      case keyword_nil:
	yyerror0("Can't assign to nil");
	goto error;
      case keyword_true:
	yyerror0("Can't assign to true");
	goto error;
      case keyword_false:
	yyerror0("Can't assign to false");
	goto error;
      case keyword__FILE__:
	yyerror0("Can't assign to __FILE__");
	goto error;
      case keyword__LINE__:
	yyerror0("Can't assign to __LINE__");
	goto error;
      case keyword__ENCODING__:
	yyerror0("Can't assign to __ENCODING__");
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
	if (!in_def)
	    return assignable_result(new_cdecl(id, val, 0, location));
	yyerror0("dynamic constant assignment");
	break;
      case ID_CLASS:
	return assignable_result(NEW_CVASGN(id, val));
      default:
	compile_error(PARSER_ARG "identifier %"PRIsVALUE" is not valid to set", rb_id2str(id));
    }
  error:
    return assignable_error();
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

static int
shadowing_lvar_0(struct parser_params *parser, ID name)
{
    if (is_private_local_id(name)) return 1;
    if (dyna_in_block()) {
	if (dvar_curr(name)) {
	    yyerror0("duplicated argument name");
	}
	else if (dvar_defined(name) || local_id(name)) {
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
	    yyerror0("duplicated argument name");
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
aryset_gen(struct parser_params *parser, NODE *recv, NODE *idx, const YYLTYPE *location)
{
    NODE *attrasgn = NEW_ATTRASGN(recv, tASET, idx);
    nd_set_loc(attrasgn, location);
    return attrasgn;
}

static void
block_dup_check_gen(struct parser_params *parser, NODE *node1, NODE *node2)
{
    if (node2 && node1 && nd_type(node1) == NODE_BLOCK_PASS) {
	compile_error(PARSER_ARG "both block arg and actual block given");
    }
}

static NODE *
attrset_gen(struct parser_params *parser, NODE *recv, ID atype, ID id, const YYLTYPE *location)
{
    NODE *attrasgn;
    if (!CALL_Q_P(atype)) id = rb_id_attrset(id);
    attrasgn = NEW_ATTRASGN(recv, id, 0);
    nd_set_loc(attrasgn, location);
    return attrasgn;
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
arg_concat_gen(struct parser_params *parser, NODE *node1, NODE *node2, const YYLTYPE *location)
{
    NODE *argscat;

    if (!node2) return node1;
    switch (nd_type(node1)) {
      case NODE_BLOCK_PASS:
	if (node1->nd_head)
	    node1->nd_head = arg_concat(node1->nd_head, node2, location);
	else
	    node1->nd_head = new_list(node2, location);
	return node1;
      case NODE_ARGSPUSH:
	if (nd_type(node2) != NODE_ARRAY) break;
	node1->nd_body = list_concat(new_list(node1->nd_body, location), node2);
	nd_set_type(node1, NODE_ARGSCAT);
	return node1;
      case NODE_ARGSCAT:
	if (nd_type(node2) != NODE_ARRAY ||
	    nd_type(node1->nd_body) != NODE_ARRAY) break;
	node1->nd_body = list_concat(node1->nd_body, node2);
	return node1;
    }
    argscat = NEW_ARGSCAT(node1, node2);
    nd_set_loc(argscat, location);
    return argscat;
}

static NODE *
arg_append_gen(struct parser_params *parser, NODE *node1, NODE *node2, const YYLTYPE *location)
{
    NODE *argspush;

    if (!node1) return new_list(node2, &node2->nd_loc);
    switch (nd_type(node1))  {
      case NODE_ARRAY:
	return list_append(node1, node2);
      case NODE_BLOCK_PASS:
	node1->nd_head = arg_append(node1->nd_head, node2, location);
	node1->nd_loc.last_loc = node1->nd_head->nd_loc.last_loc;
	return node1;
      case NODE_ARGSPUSH:
	node1->nd_body = list_append(new_list(node1->nd_body, &node1->nd_body->nd_loc), node2);
	node1->nd_loc.last_loc = node1->nd_body->nd_loc.last_loc;
	nd_set_type(node1, NODE_ARGSCAT);
	return node1;
    }
    argspush = NEW_ARGSPUSH(node1, node2);
    nd_set_loc(argspush, location);
    return argspush;
}

static NODE *
splat_array(NODE* node)
{
    if (nd_type(node) == NODE_SPLAT) node = node->nd_head;
    if (nd_type(node) == NODE_ARRAY) return node;
    return 0;
}

static void
mark_lvar_used(struct parser_params *parser, NODE *rhs)
{
    ID *vidp = NULL;
    if (!rhs) return;
    switch (nd_type(rhs)) {
      case NODE_LASGN:
	if (local_id_ref(rhs->nd_vid, vidp)) {
	    if (vidp) *vidp |= LVAR_USED;
	}
	break;
      case NODE_DASGN:
      case NODE_DASGN_CURR:
	if (dvar_defined_ref(rhs->nd_vid, vidp)) {
	    if (vidp) *vidp |= LVAR_USED;
	}
	break;
#if 0
      case NODE_MASGN:
	for (rhs = rhs->nd_head; rhs; rhs = rhs->nd_next) {
	    mark_lvar_used(parser, rhs->nd_head);
	}
	break;
#endif
    }
}

static NODE *
node_assign_gen(struct parser_params *parser, NODE *lhs, NODE *rhs, const YYLTYPE *location)
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
	nd_set_loc(lhs, location);
	break;

      case NODE_ATTRASGN:
	lhs->nd_args = arg_append(lhs->nd_args, rhs, location);
	nd_set_loc(lhs, location);
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
	    if (!cond) yyerror0("void value expression");
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
	    mark_lvar_used(parser, node);
	    return TRUE;

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

static NODE *cond0(struct parser_params*,NODE*,int,const YYLTYPE*);

static NODE*
range_op(struct parser_params *parser, NODE *node, const YYLTYPE *location)
{
    enum node_type type;

    if (node == 0) return 0;

    type = nd_type(node);
    value_expr(node);
    if (type == NODE_LIT && FIXNUM_P(node->nd_lit)) {
	warn_unless_e_option(parser, node, "integer literal in conditional range");
	return new_call(node, tEQ, new_list(new_gvar(rb_intern("$."), location), location), location);
    }
    return cond0(parser, node, FALSE, location);
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
cond0(struct parser_params *parser, NODE *node, int method_op, const YYLTYPE *location)
{
    if (node == 0) return 0;
    if (!(node = nd_once_body(node))) return 0;
    assign_in_cond(parser, node);

    switch (nd_type(node)) {
      case NODE_DSTR:
      case NODE_EVSTR:
      case NODE_STR:
	if (!method_op) rb_warn0("string literal in condition");
	break;

      case NODE_DREGX:
	{
	    NODE *match;
	    if (!method_op)
		warning_unless_e_option(parser, node, "regex literal in condition");

	    match = NEW_MATCH2(node, new_gvar(idLASTLINE, location));
	    nd_set_loc(match, location);
	    return match;
	}

      case NODE_AND:
      case NODE_OR:
	node->nd_1st = cond0(parser, node->nd_1st, FALSE, location);
	node->nd_2nd = cond0(parser, node->nd_2nd, FALSE, location);
	break;

      case NODE_DOT2:
      case NODE_DOT3:
	node->nd_beg = range_op(parser, node->nd_beg, location);
	node->nd_end = range_op(parser, node->nd_end, location);
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
cond_gen(struct parser_params *parser, NODE *node, int method_op, const YYLTYPE *location)
{
    if (node == 0) return 0;
    return cond0(parser, node, method_op, location);
}

static NODE*
new_nil_gen(struct parser_params *parser, const YYLTYPE *location)
{
    NODE *node_nil = NEW_NIL();
    nd_set_loc(node_nil, location);
    return node_nil;
}

static NODE*
new_if_gen(struct parser_params *parser, NODE *cc, NODE *left, NODE *right, const YYLTYPE *location)
{
    NODE *node_if;

    if (!cc) return right;
    cc = cond0(parser, cc, FALSE, location);
    node_if = NEW_IF(cc, left, right);
    nd_set_loc(node_if, location);
    return newline_node(node_if);
}

static NODE*
new_unless_gen(struct parser_params *parser, NODE *cc, NODE *left, NODE *right, const YYLTYPE *location)
{
    NODE *node_unless;

    if (!cc) return right;
    cc = cond0(parser, cc, FALSE, location);
    node_unless = NEW_UNLESS(cc, left, right);
    nd_set_loc(node_unless, location);
    return newline_node(node_unless);
}

static NODE*
logop_gen(struct parser_params *parser, enum node_type type, NODE *left, NODE *right,
	  const YYLTYPE *op_loc, const YYLTYPE *location)
{
    NODE *op;
    value_expr(left);
    if (left && (enum node_type)nd_type(left) == type) {
	NODE *node = left, *second;
	while ((second = node->nd_2nd) != 0 && (enum node_type)nd_type(second) == type) {
	    node = second;
	}
	node->nd_2nd = NEW_NODE(type, second, right, 0);
	node->nd_2nd->nd_loc = *location;
	nd_set_line(node->nd_2nd, op_loc->first_loc.lineno);
	left->nd_loc.last_loc = location->last_loc;
	return left;
    }
    op = NEW_NODE(type, left, right, 0);
    op->nd_loc = *location;
    nd_set_line(op, op_loc->first_loc.lineno);
    return op;
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
new_yield_gen(struct parser_params *parser, NODE *node, const YYLTYPE *location)
{
    NODE *yield;
    if (node) no_blockarg(parser, node);

    yield = NEW_YIELD(node);
    nd_set_loc(yield, location);
    return yield;
}

static VALUE
negate_lit_gen(struct parser_params *parser, VALUE lit)
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
	rb_parser_fatal(parser, "unknown literal type (%d) passed to negate_lit", type);
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


static NODE*
new_args_gen(struct parser_params *parser, NODE *m, NODE *o, ID r, NODE *p, NODE *tail, const YYLTYPE *location)
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
    nd_set_loc(tail, location);

    return tail;
}

static NODE*
new_args_tail_gen(struct parser_params *parser, NODE *k, ID kr, ID b, const YYLTYPE *kr_location)
{
    int saved_line = ruby_sourceline;
    struct rb_args_info *args;
    NODE *node;

    args = ZALLOC(struct rb_args_info);
    add_mark_object((VALUE)rb_imemo_alloc_new((VALUE)args, 0, 0, 0));
    node = NEW_NODE(NODE_ARGS, 0, 0, args);
    if (parser->error_p) return node;

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

	    if (val_node == NODE_SPECIAL_REQUIRED_KEYWORD) {
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

	args->kw_rest_arg = new_dvar(kr, kr_location);
	args->kw_rest_arg->nd_cflag = kw_bits;
    }
    else if (kr) {
	if (b) vtable_pop(lvtbl->args, 1); /* reorder */
	arg_var(kr);
	if (b) arg_var(b);
	args->kw_rest_arg = new_dvar(kr, kr_location);
    }

    ruby_sourceline = saved_line;
    return node;
}

static NODE*
dsym_node_gen(struct parser_params *parser, NODE *node, const YYLTYPE *location)
{
    VALUE lit;

    if (!node) {
	return new_lit(ID2SYM(idNULL), location);
    }

    switch (nd_type(node)) {
      case NODE_DSTR:
	nd_set_type(node, NODE_DSYM);
	nd_set_loc(node, location);
	break;
      case NODE_STR:
	lit = node->nd_lit;
	add_mark_object(node->nd_lit = ID2SYM(rb_intern_str(lit)));
	nd_set_type(node, NODE_LIT);
	nd_set_loc(node, location);
	break;
      default:
	node = NEW_NODE(NODE_DSYM, Qnil, 1, new_list(node, location));
	nd_set_loc(node, location);
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
remove_duplicate_keys(struct parser_params *parser, NODE *hash, const YYLTYPE *location)
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
	    head->nd_head = block_append(head->nd_head, value->nd_head, location);
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
new_hash_gen(struct parser_params *parser, NODE *hash, const YYLTYPE *location)
{
    NODE *nd_hash;
    if (hash) hash = remove_duplicate_keys(parser, hash, location);
    nd_hash = NEW_HASH(hash);
    nd_set_loc(nd_hash, location);
    return nd_hash;
}
#endif /* !RIPPER */

#ifndef RIPPER
static NODE *
new_op_assign_gen(struct parser_params *parser, NODE *lhs, ID op, NODE *rhs, const YYLTYPE *location)
{
    NODE *asgn;

    if (lhs) {
	ID vid = lhs->nd_vid;
	YYLTYPE lhs_location = lhs->nd_loc;
	if (op == tOROP) {
	    lhs->nd_value = rhs;
	    nd_set_loc(lhs, location);
	    asgn = NEW_OP_ASGN_OR(gettable(vid, &lhs_location), lhs);
	    nd_set_loc(asgn, location);
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
	    nd_set_loc(lhs, location);
	    asgn = NEW_OP_ASGN_AND(gettable(vid, &lhs_location), lhs);
	    nd_set_loc(asgn, location);
	}
	else {
	    asgn = lhs;
	    asgn->nd_value = new_call(gettable(vid, &lhs_location), op, new_list(rhs, &rhs->nd_loc), location);
	    nd_set_loc(asgn, location);
	}
    }
    else {
	asgn = new_begin(0, location);
    }
    return asgn;
}

static NODE *
new_attr_op_assign_gen(struct parser_params *parser, NODE *lhs,
		       ID atype, ID attr, ID op, NODE *rhs, const YYLTYPE *location)
{
    NODE *asgn;

    if (op == tOROP) {
	op = 0;
    }
    else if (op == tANDOP) {
	op = 1;
    }
    asgn = NEW_OP_ASGN2(lhs, CALL_Q_P(atype), attr, op, rhs);
    nd_set_loc(asgn, location);
    fixpos(asgn, lhs);
    return asgn;
}

static NODE *
new_const_op_assign_gen(struct parser_params *parser, NODE *lhs, ID op, NODE *rhs, const YYLTYPE *location)
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
	asgn = new_begin(0, location);
    }
    fixpos(asgn, lhs);
    nd_set_loc(asgn, location);
    return asgn;
}

static NODE *
const_path_field_gen(struct parser_params *parser, NODE *head, ID mid, const YYLTYPE *location)
{
    NODE *colon2 = NEW_COLON2(head, mid);
    nd_set_loc(colon2, location);
    return colon2;
}

static NODE *
const_decl_gen(struct parser_params *parser, NODE *path, const YYLTYPE *location)
{
    if (in_def) {
	yyerror0("dynamic constant assignment");
    }
    return new_cdecl(0, 0, (path), location);
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
new_qcall_gen(struct parser_params *parser, VALUE r, VALUE q, VALUE m, VALUE a)
{
    VALUE ret = dispatch3(call, (r), (q), (m));
    return method_optarg(ret, (a));
}

static VALUE
const_decl_gen(struct parser_params *parser, VALUE path)
{
    if (in_def) {
	path = dispatch1(assign_error, path);
	ripper_error();
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

static VALUE
var_field_gen(struct parser_params *parser, VALUE a)
{
    return ripper_new_yylval(get_id(a), dispatch1(var_field, a), 0);
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
	rb_parser_fatal(parser, "local->used->pos != local->vars->pos");
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

    add_mark_object((VALUE)rb_imemo_alloc_new((VALUE)buf, 0, 0, 0));

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
local_id_gen(struct parser_params *parser, ID id, ID **vidrefp)
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
	if (i && used && vidrefp) *vidrefp = &used->tbl[i-1];
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
dvar_defined_gen(struct parser_params *parser, ID id, ID **vidrefp)
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
	    if (used && vidrefp) *vidrefp = &used->tbl[i-1];
	    return 1;
	}
	args = args->prev;
	vars = vars->prev;
	if (!vidrefp) used = 0;
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
    const YYLTYPE *location;
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
    node = node_assign(assignable(var, 0, arg->location), new_lit(ID2SYM(var), arg->location), arg->location);
    succ = arg->succ_block;
    if (!succ) succ = new_begin(0, arg->location);
    succ = block_append(succ, node, arg->location);
    arg->succ_block = succ;
    return ST_CONTINUE;
}

static NODE *
reg_named_capture_assign_gen(struct parser_params* parser, VALUE regexp, const YYLTYPE *location)
{
    reg_named_capture_assign_t arg;

    arg.parser = parser;
    arg.enc = rb_enc_get(regexp);
    arg.succ_block = 0;
    arg.location = location;
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
void
rb_parser_set_options(VALUE vparser, int print, int loop, int chomp, int split)
{
    struct parser_params *parser;
    TypedData_Get_Struct(vparser, struct parser_params, &parser_data_type, parser);
    parser->do_print = print;
    parser->do_loop = loop;
    parser->do_chomp = chomp;
    parser->do_split = split;
}

static NODE *
parser_append_options(struct parser_params *parser, NODE *node)
{
    static const YYLTYPE default_location = {{1, 0}, {1, 0}};

    if (parser->do_print) {
	node = block_append(node,
			    new_fcall(rb_intern("print"),
				      NEW_ARRAY(new_gvar(idLASTLINE, &default_location)), &default_location),
			    &default_location);
    }

    if (parser->do_loop) {
	if (parser->do_split) {
	    node = block_append(NEW_GASGN(rb_intern("$F"),
					  new_call(new_gvar(idLASTLINE, &default_location),
						   rb_intern("split"), 0, &default_location)),
				node, &default_location);
	}
	if (parser->do_chomp) {
	    node = block_append(new_call(new_gvar(idLASTLINE, &default_location),
					 rb_intern("chomp!"), 0, &default_location), node, &default_location);
	}

	node = NEW_WHILE(NEW_VCALL(idGets), node, 1);
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
#endif /* !RIPPER */

static ID
internal_id_gen(struct parser_params *parser)
{
    const ID max_id = RB_ID_SERIAL_MAX & ~0xffff;
    ID id = (ID)vtable_size(lvtbl->args) + (ID)vtable_size(lvtbl->vars);
    id = max_id - id;
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
    parser->debug_output = rb_stdout;
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

    rb_gc_mark(lex_input);
    rb_gc_mark(lex_prevline);
    rb_gc_mark(lex_lastline);
    rb_gc_mark(lex_nextline);
    rb_gc_mark(ruby_sourcefile_string);
    rb_gc_mark((VALUE)lex_strterm);
    rb_gc_mark((VALUE)parser->ast);
#ifndef RIPPER
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
    rb_gc_mark(parser->debug_output);
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
 *    ripper.error?   -> Boolean
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
 *    ripper.end_seen?   -> Boolean
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
 *    ripper.encoding   -> encoding
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
#define NEWHEAP() rb_imemo_alloc_new(0, (VALUE)parser->heap, 0, 0)
#define ADD2HEAP(n, c, p) ((parser->heap = (n))->ptr = (p), \
			   (n)->cnt = (c), (p))

void *
rb_parser_malloc(struct parser_params *parser, size_t size)
{
    size_t cnt = HEAPCNT(1, size);
    rb_imemo_alloc_t *n = NEWHEAP();
    void *ptr = xmalloc(size);

    return ADD2HEAP(n, cnt, ptr);
}

void *
rb_parser_calloc(struct parser_params *parser, size_t nelem, size_t size)
{
    size_t cnt = HEAPCNT(nelem, size);
    rb_imemo_alloc_t *n = NEWHEAP();
    void *ptr = xcalloc(nelem, size);

    return ADD2HEAP(n, cnt, ptr);
}

void *
rb_parser_realloc(struct parser_params *parser, void *ptr, size_t size)
{
    rb_imemo_alloc_t *n;
    size_t cnt = HEAPCNT(1, size);

    if (ptr && (n = parser->heap) != NULL) {
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
rb_parser_free(struct parser_params *parser, void *ptr)
{
    rb_imemo_alloc_t **prev = &parser->heap, *n;

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
rb_parser_printf(struct parser_params *parser, const char *fmt, ...)
{
    va_list ap;
    VALUE mesg = parser->debug_buffer;

    if (NIL_P(mesg)) parser->debug_buffer = mesg = rb_str_new(0, 0);
    va_start(ap, fmt);
    rb_str_vcatf(mesg, fmt, ap);
    va_end(ap);
    if (RSTRING_END(mesg)[-1] == '\n') {
	rb_io_write(parser->debug_output, mesg);
	parser->debug_buffer = Qnil;
    }
}

static void
parser_compile_error(struct parser_params *parser, const char *fmt, ...)
{
    va_list ap;

    rb_io_flush(parser->debug_output);
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
    parser->ast = rb_ast_new();
    ripper_yyparse((void*)parser);
    rb_ast_dispose(parser->ast);
    parser->ast = 0;
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
 *    ripper.parse
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
 *    ripper.column   -> Integer
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
 *    ripper.filename   -> String
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
 *    ripper.lineno   -> Integer
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

/*
 *  call-seq:
 *    ripper.state   -> Integer
 *
 *  Return scanner state of current token.
 */
static VALUE
ripper_state(VALUE self)
{
    struct parser_params *parser;

    TypedData_Get_Struct(self, struct parser_params, &parser_data_type, parser);
    if (!ripper_initialized_p(parser)) {
	rb_raise(rb_eArgError, "method called for uninitialized object");
    }
    if (NIL_P(parser->parsing_thread)) return Qnil;
    return INT2NUM(lex_state);
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
