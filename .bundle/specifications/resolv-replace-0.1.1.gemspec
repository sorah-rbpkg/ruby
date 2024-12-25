# -*- encoding: utf-8 -*-
# stub: resolv-replace 0.1.1 ruby lib

Gem::Specification.new do |s|
  s.name = "resolv-replace".freeze
  s.version = "0.1.1"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.metadata = { "homepage_uri" => "https://github.com/ruby/resolv-replace", "source_code_uri" => "https://github.com/ruby/resolv-replace" } if s.respond_to? :metadata=
  s.require_paths = ["lib".freeze]
  s.authors = ["Tanaka Akira".freeze]
  s.date = "2022-12-14"
  s.description = "Replace Socket DNS with Resolv.".freeze
  s.email = ["akr@fsij.org".freeze]
  s.files = [".gitignore".freeze, "Gemfile".freeze, "LICENSE.txt".freeze, "README.md".freeze, "Rakefile".freeze, "bin/console".freeze, "bin/setup".freeze, "lib/resolv-replace.rb".freeze, "resolv-replace.gemspec".freeze]
  s.homepage = "https://github.com/ruby/resolv-replace".freeze
  s.licenses = ["Ruby".freeze, "BSD-2-Clause".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 2.3.0".freeze)
  s.rubygems_version = "3.4.20".freeze
  s.summary = "Replace Socket DNS with Resolv.".freeze

  s.specification_version = 4

  s.add_runtime_dependency(%q<resolv>.freeze, [">= 0"])
end
