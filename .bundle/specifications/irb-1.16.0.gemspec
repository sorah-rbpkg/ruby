# -*- encoding: utf-8 -*-
# stub: irb 1.16.0 ruby lib

Gem::Specification.new do |s|
  s.name = "irb".freeze
  s.version = "1.16.0"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.metadata = { "changelog_uri" => "https://github.com/ruby/irb/releases", "documentation_uri" => "https://ruby.github.io/irb/", "homepage_uri" => "https://github.com/ruby/irb", "source_code_uri" => "https://github.com/ruby/irb" } if s.respond_to? :metadata=
  s.require_paths = ["lib".freeze]
  s.authors = ["aycabta".freeze, "Keiju ISHITSUKA".freeze]
  s.bindir = "exe".freeze
  s.date = "1980-01-02"
  s.description = "Interactive Ruby command-line tool for REPL (Read Eval Print Loop).".freeze
  s.email = ["aycabta@gmail.com".freeze, "keiju@ruby-lang.org".freeze]
  s.executables = ["irb".freeze]
  s.files = [".rdoc_options".freeze, "CONTRIBUTING.md".freeze, "EXTEND_IRB.md".freeze, "Gemfile".freeze, "LICENSE.txt".freeze, "README.md".freeze, "doc/COMMAND_LINE_OPTIONS.md".freeze, "doc/COMPARED_WITH_PRY.md".freeze, "doc/Configurations.md".freeze, "doc/EXTEND_IRB.md".freeze, "doc/Index.md".freeze, "doc/irb/irb-tools.rd.ja".freeze, "doc/irb/irb.rd.ja".freeze, "exe/irb".freeze, "lib/irb.rb".freeze, "lib/irb/cmd/nop.rb".freeze, "lib/irb/color.rb".freeze, "lib/irb/color_printer.rb".freeze, "lib/irb/command.rb".freeze, "lib/irb/command/backtrace.rb".freeze, "lib/irb/command/base.rb".freeze, "lib/irb/command/break.rb".freeze, "lib/irb/command/catch.rb".freeze, "lib/irb/command/cd.rb".freeze, "lib/irb/command/chws.rb".freeze, "lib/irb/command/context.rb".freeze, "lib/irb/command/continue.rb".freeze, "lib/irb/command/copy.rb".freeze, "lib/irb/command/debug.rb".freeze, "lib/irb/command/delete.rb".freeze, "lib/irb/command/disable_irb.rb".freeze, "lib/irb/command/edit.rb".freeze, "lib/irb/command/exit.rb".freeze, "lib/irb/command/finish.rb".freeze, "lib/irb/command/force_exit.rb".freeze, "lib/irb/command/help.rb".freeze, "lib/irb/command/history.rb".freeze, "lib/irb/command/info.rb".freeze, "lib/irb/command/internal_helpers.rb".freeze, "lib/irb/command/irb_info.rb".freeze, "lib/irb/command/load.rb".freeze, "lib/irb/command/ls.rb".freeze, "lib/irb/command/measure.rb".freeze, "lib/irb/command/next.rb".freeze, "lib/irb/command/pushws.rb".freeze, "lib/irb/command/show_doc.rb".freeze, "lib/irb/command/show_source.rb".freeze, "lib/irb/command/step.rb".freeze, "lib/irb/command/subirb.rb".freeze, "lib/irb/command/whereami.rb".freeze, "lib/irb/completion.rb".freeze, "lib/irb/context.rb".freeze, "lib/irb/debug.rb".freeze, "lib/irb/debug/ui.rb".freeze, "lib/irb/default_commands.rb".freeze, "lib/irb/easter-egg.rb".freeze, "lib/irb/ext/change-ws.rb".freeze, "lib/irb/ext/eval_history.rb".freeze, "lib/irb/ext/loader.rb".freeze, "lib/irb/ext/multi-irb.rb".freeze, "lib/irb/ext/tracer.rb".freeze, "lib/irb/ext/use-loader.rb".freeze, "lib/irb/ext/workspaces.rb".freeze, "lib/irb/frame.rb".freeze, "lib/irb/help.rb".freeze, "lib/irb/helper_method.rb".freeze, "lib/irb/helper_method/base.rb".freeze, "lib/irb/helper_method/conf.rb".freeze, "lib/irb/history.rb".freeze, "lib/irb/init.rb".freeze, "lib/irb/input-method.rb".freeze, "lib/irb/inspector.rb".freeze, "lib/irb/lc/error.rb".freeze, "lib/irb/lc/help-message".freeze, "lib/irb/lc/ja/error.rb".freeze, "lib/irb/lc/ja/help-message".freeze, "lib/irb/locale.rb".freeze, "lib/irb/nesting_parser.rb".freeze, "lib/irb/notifier.rb".freeze, "lib/irb/output-method.rb".freeze, "lib/irb/pager.rb".freeze, "lib/irb/ruby-lex.rb".freeze, "lib/irb/ruby_logo.aa".freeze, "lib/irb/source_finder.rb".freeze, "lib/irb/statement.rb".freeze, "lib/irb/version.rb".freeze, "lib/irb/workspace.rb".freeze, "lib/irb/ws-for-case-2.rb".freeze, "lib/irb/xmp.rb".freeze, "man/irb.1".freeze]
  s.homepage = "https://github.com/ruby/irb".freeze
  s.licenses = ["Ruby".freeze, "BSD-2-Clause".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 2.7".freeze)
  s.rubygems_version = "3.4.20".freeze
  s.summary = "Interactive Ruby command-line tool for REPL (Read Eval Print Loop).".freeze

  s.specification_version = 4

  s.add_runtime_dependency(%q<reline>.freeze, [">= 0.4.2"])
  s.add_runtime_dependency(%q<rdoc>.freeze, [">= 4.0.0"])
  s.add_runtime_dependency(%q<pp>.freeze, [">= 0.6.0"])
end
