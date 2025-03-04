#include "ruby/ruby.h"
#include "ruby/encoding.h"
#include "internal.h"
#include "rubyparser.h"
#define YYSTYPE_IS_DECLARED
#include "parse.h"
#include "internal/parse.h"
#include "internal/ruby_parser.h"
#include "node.h"
#include "eventids1.h"
#include "eventids2.h"
#include "ripper_init.h"

#define STR_NEW2(ptr) rb_enc_str_new((ptr),strlen(ptr),rb_ruby_parser_enc(p))
#define RIPPER_VERSION "0.1.0"

ID id_warn, id_warning, id_gets, id_assoc;

enum lex_type {
    lex_type_str,
    lex_type_io,
    lex_type_generic,
};

struct ripper {
    rb_parser_t *p;
    enum lex_type type;
    union {
       struct lex_pointer_string ptr_str;
       VALUE val;
    } data;
};

static void
ripper_parser_mark2(void *ptr)
{
    struct ripper *r = (struct ripper*)ptr;
    if (r->p) {
        ripper_parser_mark(r->p);

        switch (r->type) {
          case lex_type_str:
            rb_gc_mark(r->data.ptr_str.str);
            break;
          case lex_type_io:
            rb_gc_mark(r->data.val);
            break;
          case lex_type_generic:
            rb_gc_mark(r->data.val);
            break;
        }
    }
}

static void
ripper_parser_free2(void *ptr)
{
    struct ripper *r = (struct ripper*)ptr;
    if (r->p) ripper_parser_free(r->p);
    xfree(r);
}

static size_t
ripper_parser_memsize2(const void *ptr)
{
    struct ripper *r = (struct ripper*)ptr;
    return (r->p) ? ripper_parser_memsize(r->p) : 0;
}

static const rb_data_type_t parser_data_type = {
    "ripper",
    {
        ripper_parser_mark2,
        ripper_parser_free2,
        ripper_parser_memsize2,
    },
    0, 0, RUBY_TYPED_FREE_IMMEDIATELY
};

static rb_parser_string_t *
ripper_lex_get_generic(struct parser_params *p, rb_parser_input_data input, int line_count)
{
    VALUE src = (VALUE)input;
    VALUE line = rb_funcallv_public(src, id_gets, 0, 0);
    if (NIL_P(line)) return 0;
    if (!RB_TYPE_P(line, T_STRING)) {
        rb_raise(rb_eTypeError,
                 "gets returned %"PRIsVALUE" (expected String or nil)",
                 rb_obj_class(line));
    }
    return rb_str_to_parser_string(p, line);
}

void
ripper_compile_error(struct parser_params *p, const char *fmt, ...)
{
    VALUE str;
    va_list args;

    va_start(args, fmt);
    str = rb_vsprintf(fmt, args);
    va_end(args);
    rb_funcall(ripper_value(p), rb_intern("compile_error"), 1, str);
    ripper_error(p);
}

static rb_parser_string_t *
ripper_lex_io_get(struct parser_params *p, rb_parser_input_data input, int line_count)
{
    VALUE src = (VALUE)input;
    VALUE line = rb_io_gets(src);
    if (NIL_P(line)) return 0;
    return rb_str_to_parser_string(p, line);
}

static rb_parser_string_t *
ripper_lex_get_str(struct parser_params *p, rb_parser_input_data input, int line_count)
{
    return rb_parser_lex_get_str(p, (struct lex_pointer_string *)input);
}

static VALUE
ripper_s_allocate(VALUE klass)
{
    struct ripper *r;

    VALUE self = TypedData_Make_Struct(klass, struct ripper,
                                       &parser_data_type, r);

#ifdef UNIVERSAL_PARSER
    const rb_parser_config_t *config = rb_ruby_parser_config();
    r->p = rb_ripper_parser_params_allocate(config);
#else
    r->p = rb_ruby_ripper_parser_allocate();
#endif
    rb_ruby_parser_set_value(r->p, self);
    return self;
}

static struct parser_params *
ripper_parser_params(VALUE self, bool initialized)
{
    struct ripper *r;
    struct parser_params *p;

    TypedData_Get_Struct(self, struct ripper, &parser_data_type, r);
    p = r->p;
    if (initialized && !rb_ruby_ripper_initialized_p(p)) {
        rb_raise(rb_eArgError, "method called for uninitialized object");
    }
    return p;
}

/*
 *  call-seq:
 *    ripper.error?   -> Boolean
 *
 *  Return true if parsed source has errors.
 */
static VALUE
ripper_error_p(VALUE vparser)
{
    struct parser_params *p = ripper_parser_params(vparser, false);

    return RBOOL(rb_ruby_parser_error_p(p));
}

/*
 *  call-seq:
 *    ripper.end_seen?   -> Boolean
 *
 *  Return true if parsed source ended by +\_\_END\_\_+.
 */
static VALUE
ripper_parser_end_seen_p(VALUE vparser)
{
    struct parser_params *p = ripper_parser_params(vparser, false);

    return RBOOL(rb_ruby_parser_end_seen_p(p));
}

/*
 *  call-seq:
 *    ripper.encoding   -> encoding
 *
 *  Return encoding of the source.
 */
static VALUE
ripper_parser_encoding(VALUE vparser)
{
    struct parser_params *p = ripper_parser_params(vparser, false);

    return rb_enc_from_encoding(rb_ruby_parser_encoding(p));
}

/*
 *  call-seq:
 *    ripper.yydebug   -> true or false
 *
 *  Get yydebug.
 */
static VALUE
ripper_parser_get_yydebug(VALUE self)
{
    struct parser_params *p = ripper_parser_params(self, false);

    return RBOOL(rb_ruby_parser_get_yydebug(p));
}

/*
 *  call-seq:
 *    ripper.yydebug = flag
 *
 *  Set yydebug.
 */
static VALUE
ripper_parser_set_yydebug(VALUE self, VALUE flag)
{
    struct parser_params *p = ripper_parser_params(self, false);

    rb_ruby_parser_set_yydebug(p, RTEST(flag));
    return flag;
}

/*
 *  call-seq:
 *    ripper.debug_output   -> obj
 *
 *  Get debug output.
 */
static VALUE
ripper_parser_get_debug_output(VALUE self)
{
    struct parser_params *p = ripper_parser_params(self, false);

    return rb_ruby_parser_debug_output(p);
}

/*
 *  call-seq:
 *    ripper.debug_output = obj
 *
 *  Set debug output.
 */
static VALUE
ripper_parser_set_debug_output(VALUE self, VALUE output)
{
    struct parser_params *p = ripper_parser_params(self, false);

    rb_ruby_parser_set_debug_output(p, output);
    return output;
}

static int
ripper_parser_dedent_string(struct parser_params *p, VALUE string, int width)
{
    int col;
    rb_parser_string_t *str;
    str = rb_str_to_parser_string(p, string);
    col = rb_ruby_ripper_dedent_string(p, str, width);
    rb_str_replace(string, rb_str_new_parser_string(str));
    rb_parser_string_free(p, str);
    return col;
}

#ifdef UNIVERSAL_PARSER
struct dedent_string_arg {
    struct parser_params *p;
    VALUE input;
    VALUE width;
};

static VALUE
parser_dedent_string0(VALUE a)
{
    struct dedent_string_arg *arg = (void *)a;
    int wid, col;

    StringValue(arg->input);
    wid = NUM2UINT(arg->width);
    col = ripper_parser_dedent_string(arg->p, arg->input, wid);
    return INT2NUM(col);
}

static VALUE
parser_free(VALUE a)
{
    struct parser_params *p = (void *)a;

    rb_ruby_parser_free(p);
    return Qnil;
}
#endif

/*
 *  call-seq:
 *    Ripper.dedent_string(input, width)   -> Integer
 *
 *  USE OF RIPPER LIBRARY ONLY.
 *
 *  Strips up to +width+ leading whitespaces from +input+,
 *  and returns the stripped column width.
 */
#ifdef UNIVERSAL_PARSER
static VALUE
parser_dedent_string(VALUE self, VALUE input, VALUE width)
{
    struct parser_params *p;
    struct dedent_string_arg args;

    p = rb_parser_params_new();

    args.p = p;
    args.input = input;
    args.width = width;
    return rb_ensure(parser_dedent_string0, (VALUE)&args, parser_free, (VALUE)p);
}
#else
static VALUE
parser_dedent_string(VALUE self, VALUE input, VALUE width)
{
    int wid, col;

    StringValue(input);
    wid = NUM2UINT(width);
    col = ripper_parser_dedent_string(0, input, wid);
    return INT2NUM(col);
}
#endif

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
    struct ripper *r;
    struct parser_params *p;
    VALUE src, fname, lineno;
    rb_parser_lex_gets_func *gets;
    VALUE sourcefile_string;
    const char *sourcefile;
    int sourceline;
    rb_parser_input_data input;

    p = ripper_parser_params(self, false);
    TypedData_Get_Struct(self, struct ripper, &parser_data_type, r);
    rb_scan_args(argc, argv, "12", &src, &fname, &lineno);
    if (RB_TYPE_P(src, T_FILE)) {
        gets = ripper_lex_io_get;
        r->type = lex_type_io;
        r->data.val = src;
        input = (rb_parser_input_data)src;
    }
    else if (rb_respond_to(src, id_gets)) {
        gets = ripper_lex_get_generic;
        r->type = lex_type_generic;
        r->data.val = src;
        input = (rb_parser_input_data)src;
    }
    else {
        StringValue(src);
        gets = ripper_lex_get_str;
        r->type = lex_type_str;
        r->data.ptr_str.str = src;
        r->data.ptr_str.ptr = 0;
        input = (rb_parser_input_data)&r->data.ptr_str;
    }
    if (NIL_P(fname)) {
        fname = STR_NEW2("(ripper)");
        OBJ_FREEZE(fname);
    }
    else {
        StringValueCStr(fname);
        fname = rb_str_new_frozen(fname);
    }
    rb_ruby_ripper_parser_initialize(p);

    sourcefile_string = fname;
    sourcefile = RSTRING_PTR(fname);
    sourceline = NIL_P(lineno) ? 0 : NUM2INT(lineno) - 1;

    rb_ruby_parser_ripper_initialize(p, gets, input, sourcefile_string, sourcefile, sourceline);

    return Qnil;
}

static VALUE
ripper_parse0(VALUE vparser)
{
    struct parser_params *p = ripper_parser_params(vparser, false);

    rb_ruby_ripper_parse0(p);
    return rb_ruby_parser_result(p);
}

static VALUE
ripper_ensure(VALUE vparser)
{
    struct parser_params *p = ripper_parser_params(vparser, false);

    rb_ruby_parser_set_parsing_thread(p, Qnil);
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
    struct parser_params *p = ripper_parser_params(self, true);
    VALUE result;

    if (!NIL_P(rb_ruby_parser_parsing_thread(p))) {
        if (rb_ruby_parser_parsing_thread(p) == rb_thread_current())
            rb_raise(rb_eArgError, "Ripper#parse is not reentrant");
        else
            rb_raise(rb_eArgError, "Ripper#parse is not multithread-safe");
    }
    rb_ruby_parser_set_parsing_thread(p, rb_thread_current());
    result = rb_ensure(ripper_parse0, self, ripper_ensure, self);
    RB_GC_GUARD(self);

    return result;
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
    struct parser_params *p = ripper_parser_params(self, true);
    long col;

    if (NIL_P(rb_ruby_parser_parsing_thread(p))) return Qnil;
    col = rb_ruby_ripper_column(p);
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
    struct parser_params *p = ripper_parser_params(self, true);

    return rb_ruby_parser_ruby_sourcefile_string(p);
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
    struct parser_params *p = ripper_parser_params(self, true);

    if (NIL_P(rb_ruby_parser_parsing_thread(p))) return Qnil;
    return INT2NUM(rb_ruby_parser_ruby_sourceline(p));
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
    struct parser_params *p = ripper_parser_params(self, true);

    if (NIL_P(rb_ruby_parser_parsing_thread(p))) return Qnil;
    return INT2NUM(rb_ruby_parser_lex_state(p));
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
    struct parser_params *p = ripper_parser_params(self, true);
    long pos, len;
    VALUE str;

    if (NIL_P(rb_ruby_parser_parsing_thread(p))) return Qnil;
    pos = rb_ruby_ripper_column(p);
    len = rb_ruby_ripper_token_len(p);
    str = rb_str_new_parser_string(rb_ruby_ripper_lex_lastline(p));
    return rb_str_subseq(str, pos, len);
}

#ifdef RIPPER_DEBUG
/* :nodoc: */
static VALUE
ripper_assert_Qundef(VALUE self, VALUE obj, VALUE msg)
{
    StringValue(msg);
    if (UNDEF_P(obj)) {
        rb_raise(rb_eArgError, "%"PRIsVALUE, msg);
    }
    return Qnil;
}

/* :nodoc: */
static VALUE
ripper_raw_value(VALUE self, VALUE obj)
{
    return ULONG2NUM(obj);
}

/* :nodoc: */
static VALUE
ripper_validate_object(VALUE self, VALUE x)
{
    if (x == Qfalse) return x;
    if (x == Qtrue) return x;
    if (NIL_P(x)) return x;
    if (UNDEF_P(x))
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

#ifdef UNIVERSAL_PARSER
struct lex_state_name_arg {
    struct parser_params *p;
    VALUE state;
};

static VALUE
lex_state_name0(VALUE a)
{
    struct lex_state_name_arg *arg = (void *)a;

    return rb_ruby_ripper_lex_state_name(arg->p, NUM2INT(arg->state));
}
#endif

/*
 *  call-seq:
 *    Ripper.lex_state_name(integer)   -> string
 *
 *  Returns a string representation of lex_state.
 */
#ifdef UNIVERSAL_PARSER
static VALUE
ripper_lex_state_name(VALUE self, VALUE state)
{
    struct parser_params *p;
    struct lex_state_name_arg args;

    p = rb_parser_params_new();

    args.p = p;
    args.state = state;

    return rb_ensure(lex_state_name0, (VALUE)&args, parser_free, (VALUE)p);
}
#else
static VALUE
ripper_lex_state_name(VALUE self, VALUE state)
{
    return rb_ruby_ripper_lex_state_name(0, NUM2INT(state));
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
    id_assoc = rb_intern_const("=>");

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
    rb_define_method(Ripper, "end_seen?", ripper_parser_end_seen_p, 0);
    rb_define_method(Ripper, "encoding", ripper_parser_encoding, 0);
    rb_define_method(Ripper, "yydebug", ripper_parser_get_yydebug, 0);
    rb_define_method(Ripper, "yydebug=", ripper_parser_set_yydebug, 1);
    rb_define_method(Ripper, "debug_output", ripper_parser_get_debug_output, 0);
    rb_define_method(Ripper, "debug_output=", ripper_parser_set_debug_output, 1);
    rb_define_method(Ripper, "error?", ripper_error_p, 0);
#ifdef RIPPER_DEBUG
    rb_define_method(Ripper, "assert_Qundef", ripper_assert_Qundef, 2);
    rb_define_method(Ripper, "rawVALUE", ripper_raw_value, 1);
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
    /* right after `.', `&.' or `::', no reserved words. */
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
