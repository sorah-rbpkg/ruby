# -*- encoding: utf-8 -*-
# stub: repl_type_completor 0.1.9 ruby lib

Gem::Specification.new do |s|
  s.name = "repl_type_completor".freeze
  s.version = "0.1.9"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.metadata = { "documentation_uri" => "https://github.com/ruby/repl_type_completor", "homepage_uri" => "https://github.com/ruby/repl_type_completor", "source_code_uri" => "https://github.com/ruby/repl_type_completor" } if s.respond_to? :metadata=
  s.require_paths = ["lib".freeze]
  s.authors = ["tompng".freeze]
  s.bindir = "exe".freeze
  s.date = "2024-12-16"
  s.description = "Type based completion for REPL.".freeze
  s.email = ["tomoyapenguin@gmail.com".freeze]
  s.files = ["Gemfile".freeze, "LICENSE.txt".freeze, "README.md".freeze, "Rakefile".freeze, "lib/repl_type_completor.rb".freeze, "lib/repl_type_completor/methods.rb".freeze, "lib/repl_type_completor/require_paths.rb".freeze, "lib/repl_type_completor/result.rb".freeze, "lib/repl_type_completor/scope.rb".freeze, "lib/repl_type_completor/type_analyzer.rb".freeze, "lib/repl_type_completor/types.rb".freeze, "lib/repl_type_completor/version.rb".freeze, "sig/repl_type_completor.rbs".freeze]
  s.homepage = "https://github.com/ruby/repl_type_completor".freeze
  s.licenses = ["MIT".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 3.0.0".freeze)
  s.rubygems_version = "3.4.20".freeze
  s.summary = "Type based completion for REPL.".freeze

  s.specification_version = 4

  s.add_runtime_dependency(%q<prism>.freeze, ["~> 1.0"])
  s.add_runtime_dependency(%q<rbs>.freeze, [">= 2.7.0", "< 4.0.0"])
end
