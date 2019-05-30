/* -*-c-*-
 THIS FILE WAS AUTOGENERATED BY template/prelude.c.tmpl. DO NOT EDIT.

 sources: golf_prelude
*/
#include "ruby/ruby.h"
#include "internal.h"
#include "vm_core.h"
#include "iseq.h"


static const char prelude_name0[] = "<internal:golf_prelude>";
static const struct {
    char L0[494]; /* 1..18 */
    char L18[460]; /* 19..33 */
    char L33[499]; /* 34..63 */
    char L63[475]; /* 64..96 */
    char L96[494]; /* 97..117 */
    char L117[175]; /* 118..129 */
} prelude_code0 = {
#line 1 "golf_prelude.rb"
"class Object\n"
"  @@golf_hash = {}\n"
"\n"
"  def method_missing m, *a, &b\n"
"    t = @@golf_hash[ [m, self.class] ] ||= matching_methods(m)[0]\n"
"    if t && b\n"
"      __send__(t, *a) {|*args|\n"
"        b.binding.eval(\"proc{|golf_matchdata| $~ = golf_matchdata }\").call($~) if $~\n"
"        b.call(*args)\n"
"      }\n"
"    else\n"
"      t ? __send__(t, *a, &b) : super\n"
"    end\n"
"  end\n"
"\n"
"  def matching_methods(s = '', m = callable_methods)\n"
"    r = /^#{s.to_s.gsub(/./){\"(.*?)\" + Regexp.escape($&)}}/\n"
"    m.grep(r).sort_by do |i|\n"
,
#line 19 "golf_prelude.rb"
"      i.to_s.match(r).captures.map(&:size) << i\n"
"    end\n"
"  end\n"
"\n"
"  def self.const_missing c\n"
"    t = @@golf_hash[ [c,self.class] ] ||= matching_methods(c, constants)[0]\n"
"    t and return const_get(t)\n"
"    raise NameError, \"uninitialized constant #{c}\", caller(1)\n"
"  end\n"
"\n"
"  def shortest_abbreviation(s = '', m = callable_methods)\n"
"    s = s.to_s\n"
"    our_case = (?A..?Z) === s[0]\n"
"    if m.index(s.to_sym)\n"
"      1.upto(s.size){|z| s.scan(/./).combination(z).map{|trial|\n"
,
#line 34 "golf_prelude.rb"
"        next unless ((?A..?Z) === trial[0]) == our_case\n"
"        trial *= ''\n"
"        return trial if matching_methods(trial, m)[0].to_s == s\n"
"      }}\n"
"    else\n"
"      nil\n"
"    end\n"
"  end\n"
"\n"
"  def callable_methods\n"
"    self.class == Object ? methods + private_methods : methods\n"
"  end\n"
"\n"
"  private\n"
"\n"
"  def h(a = 'H', b = 'w', c = '!')\n"
"    puts \"#{a}ello, #{b}orld#{c}\"\n"
"  end\n"
"\n"
"  def f(m = 100)\n"
"    1.upto(m){|n|puts'FizzBuzz\n"
"'[i=n**4%-15,i+13]||n}\n"
"  end\n"
"\n"
"  alias say puts\n"
"\n"
"  def do_while\n"
"    0 while yield\n"
"  end\n"
"\n"
,
#line 64 "golf_prelude.rb"
"  def do_until\n"
"    0 until yield\n"
"  end\n"
"end\n"
"\n"
"class Array\n"
"  alias old_to_s to_s\n"
"  alias to_s join\n"
"end\n"
"\n"
"class FalseClass\n"
"  alias old_to_s to_s\n"
"  def to_s\n"
"    \"\"\n"
"  end\n"
"end\n"
"\n"
"class Integer\n"
"  alias each times\n"
"  include Enumerable\n"
"end\n"
"\n"
"class String\n"
"  alias / split\n"
"\n"
"  def to_a\n"
"    split('')\n"
"  end\n"
"\n"
"  (Array.instance_methods - instance_methods - %i[to_ary transpose flatten flatten! compact compact! assoc rassoc]).each{|meth|\n"
"    eval \"\n"
"    def #{meth}(*args, &block)\n"
"      a = to_a\n"
,
#line 97 "golf_prelude.rb"
"      result = a.#{meth}(*args, &block)\n"
"      replace(a.join)\n"
"      if result.class == Array\n"
"        Integer === result[0] ? result.pack('c*') : result.join\n"
"      elsif result.class == Enumerator\n"
"        result.map(&:join).to_enum\n"
"      else\n"
"        result\n"
"      end\n"
"    end\"\n"
"  }\n"
"end\n"
"\n"
"class Enumerator\n"
"  alias old_to_s to_s\n"
"  (Array.instance_methods - instance_methods - [:replace] + [:to_s]).each{|meth|\n"
"    eval \"\n"
"    def #{meth}(*args, &block)\n"
"      to_a.#{meth}(*args, &block)\n"
"    end\"\n"
"  }\n"
,
#line 118 "golf_prelude.rb"
"  alias old_inspect inspect\n"
"  alias inspect old_to_s\n"
"end\n"
"\n"
"class Symbol\n"
"  def call(*args, &block)\n"
"    proc do |recv|\n"
"      recv.__send__(self, *args, &block)\n"
"    end\n"
"  end\n"
"end\n"
#line 161 "golf.c"
};


#define PRELUDE_NAME(n) rb_usascii_str_new_static(prelude_name##n, sizeof(prelude_name##n)-1)
#define PRELUDE_CODE(n) rb_utf8_str_new_static(prelude_code##n.L0, sizeof(prelude_code##n))
COMPILER_WARNING_PUSH
#if GCC_VERSION_SINCE(4, 2, 0)
COMPILER_WARNING_ERROR(-Wmissing-field-initializers)
#endif
static void
prelude_eval(VALUE code, VALUE name, int line)
{
    static const rb_compile_option_t optimization = {
	TRUE, /* int inline_const_cache; */
	TRUE, /* int peephole_optimization; */
	FALSE,/* int tailcall_optimization; */
	TRUE, /* int specialized_instruction; */
	TRUE, /* int operands_unification; */
	TRUE, /* int instructions_unification; */
	TRUE, /* int stack_caching; */
	TRUE, /* int frozen_string_literal; */
	FALSE, /* int debug_frozen_string_literal; */
	FALSE, /* unsigned int coverage_enabled; */
	0, /* int debug_level; */
    };

    rb_ast_t *ast = rb_parser_compile_string_path(rb_parser_new(), name, code, line);
    if (!ast->body.root) {
	rb_ast_dispose(ast);
	rb_exc_raise(rb_errinfo());
    }
    rb_iseq_eval(rb_iseq_new_with_opt(&ast->body, name, name, Qnil, INT2FIX(line),
				      NULL, ISEQ_TYPE_TOP, &optimization));
    rb_ast_dispose(ast);
}
COMPILER_WARNING_POP

void
Init_golf(void)
{
    prelude_eval(PRELUDE_CODE(0), PRELUDE_NAME(0), 1);

#if 0
    printf("%.*s", (int)sizeof(prelude_code0), prelude_code0.L0);
#endif
}
