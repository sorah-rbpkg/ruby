# -*- encoding: utf-8 -*-
# stub: typeprof 0.30.1 ruby lib

Gem::Specification.new do |s|
  s.name = "typeprof".freeze
  s.version = "0.30.1"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.metadata = { "homepage_uri" => "https://github.com/ruby/typeprof", "source_code_uri" => "https://github.com/ruby/typeprof" } if s.respond_to? :metadata=
  s.require_paths = ["lib".freeze]
  s.authors = ["Yusuke Endoh".freeze]
  s.date = "2024-12-23"
  s.description = "TypeProf performs a type analysis of non-annotated Ruby code.\n\nIt abstractly executes input Ruby code in a level of types instead of values, gathers what types are passed to and returned by methods, and prints the analysis result in RBS format, a standard type description format for Ruby 3.0.\n".freeze
  s.email = ["mame@ruby-lang.org".freeze]
  s.executables = ["typeprof".freeze]
  s.files = ["LICENSE".freeze, "README.md".freeze, "bin/typeprof".freeze, "doc/doc.ja.md".freeze, "doc/doc.md".freeze, "lib/typeprof.rb".freeze, "lib/typeprof/cli.rb".freeze, "lib/typeprof/cli/cli.rb".freeze, "lib/typeprof/code_range.rb".freeze, "lib/typeprof/core.rb".freeze, "lib/typeprof/core/ast.rb".freeze, "lib/typeprof/core/ast/base.rb".freeze, "lib/typeprof/core/ast/call.rb".freeze, "lib/typeprof/core/ast/const.rb".freeze, "lib/typeprof/core/ast/control.rb".freeze, "lib/typeprof/core/ast/meta.rb".freeze, "lib/typeprof/core/ast/method.rb".freeze, "lib/typeprof/core/ast/misc.rb".freeze, "lib/typeprof/core/ast/module.rb".freeze, "lib/typeprof/core/ast/pattern.rb".freeze, "lib/typeprof/core/ast/sig_decl.rb".freeze, "lib/typeprof/core/ast/sig_type.rb".freeze, "lib/typeprof/core/ast/value.rb".freeze, "lib/typeprof/core/ast/variable.rb".freeze, "lib/typeprof/core/builtin.rb".freeze, "lib/typeprof/core/env.rb".freeze, "lib/typeprof/core/env/method.rb".freeze, "lib/typeprof/core/env/method_entity.rb".freeze, "lib/typeprof/core/env/module_entity.rb".freeze, "lib/typeprof/core/env/static_read.rb".freeze, "lib/typeprof/core/env/type_alias_entity.rb".freeze, "lib/typeprof/core/env/value_entity.rb".freeze, "lib/typeprof/core/graph/box.rb".freeze, "lib/typeprof/core/graph/change_set.rb".freeze, "lib/typeprof/core/graph/filter.rb".freeze, "lib/typeprof/core/graph/vertex.rb".freeze, "lib/typeprof/core/service.rb".freeze, "lib/typeprof/core/type.rb".freeze, "lib/typeprof/core/util.rb".freeze, "lib/typeprof/diagnostic.rb".freeze, "lib/typeprof/lsp.rb".freeze, "lib/typeprof/lsp/messages.rb".freeze, "lib/typeprof/lsp/server.rb".freeze, "lib/typeprof/lsp/text.rb".freeze, "lib/typeprof/lsp/util.rb".freeze, "lib/typeprof/version.rb".freeze, "typeprof.gemspec".freeze]
  s.homepage = "https://github.com/ruby/typeprof".freeze
  s.licenses = ["MIT".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 3.3".freeze)
  s.rubygems_version = "3.4.20".freeze
  s.summary = "TypeProf is a type analysis tool for Ruby code based on abstract interpretation".freeze

  s.specification_version = 4

  s.add_runtime_dependency(%q<rbs>.freeze, [">= 3.6.0"])
end
