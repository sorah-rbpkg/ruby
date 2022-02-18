# -*- encoding: utf-8 -*-
# stub: typeprof 0.21.2 ruby lib

Gem::Specification.new do |s|
  s.name = "typeprof".freeze
  s.version = "0.21.2"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.metadata = { "homepage_uri" => "https://github.com/ruby/typeprof", "source_code_uri" => "https://github.com/ruby/typeprof" } if s.respond_to? :metadata=
  s.require_paths = ["lib".freeze]
  s.authors = ["Yusuke Endoh".freeze]
  s.bindir = "exe".freeze
  s.date = "2021-12-28"
  s.description = "TypeProf performs a type analysis of non-annotated Ruby code.\n\nIt abstractly executes input Ruby code in a level of types instead of values, gathers what types are passed to and returned by methods, and prints the analysis result in RBS format, a standard type description format for Ruby 3.0.\n\nThis tool is planned to be bundled with Ruby 3.0.\n".freeze
  s.email = ["mame@ruby-lang.org".freeze]
  s.executables = ["typeprof".freeze]
  s.files = [".github/workflows/main.yml".freeze, ".gitignore".freeze, "Gemfile".freeze, "Gemfile.lock".freeze, "LICENSE".freeze, "README.md".freeze, "Rakefile".freeze, "exe/typeprof".freeze, "lib/typeprof.rb".freeze, "lib/typeprof/analyzer.rb".freeze, "lib/typeprof/arguments.rb".freeze, "lib/typeprof/block.rb".freeze, "lib/typeprof/builtin.rb".freeze, "lib/typeprof/cli.rb".freeze, "lib/typeprof/code-range.rb".freeze, "lib/typeprof/config.rb".freeze, "lib/typeprof/container-type.rb".freeze, "lib/typeprof/export.rb".freeze, "lib/typeprof/import.rb".freeze, "lib/typeprof/insns-def.rb".freeze, "lib/typeprof/iseq.rb".freeze, "lib/typeprof/lsp.rb".freeze, "lib/typeprof/method.rb".freeze, "lib/typeprof/type.rb".freeze, "lib/typeprof/utils.rb".freeze, "lib/typeprof/version.rb".freeze, "tools/coverage.rb".freeze, "tools/setup-insns-def.rb".freeze, "typeprof-lsp".freeze, "typeprof.gemspec".freeze, "vscode/.gitignore".freeze, "vscode/.vscode/launch.json".freeze, "vscode/.vscodeignore".freeze, "vscode/README.md".freeze, "vscode/development.md".freeze, "vscode/package-lock.json".freeze, "vscode/package.json".freeze, "vscode/sandbox/test.rb".freeze, "vscode/src/extension.ts".freeze, "vscode/tsconfig.json".freeze]
  s.homepage = "https://github.com/ruby/typeprof".freeze
  s.licenses = ["MIT".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 2.7".freeze)
  s.rubygems_version = "3.1.2".freeze
  s.summary = "TypeProf is a type analysis tool for Ruby code based on abstract interpretation".freeze

  if s.respond_to? :specification_version then
    s.specification_version = 4
  end

  if s.respond_to? :add_runtime_dependency then
    s.add_runtime_dependency(%q<rbs>.freeze, [">= 1.8.1"])
  else
    s.add_dependency(%q<rbs>.freeze, [">= 1.8.1"])
  end
end
