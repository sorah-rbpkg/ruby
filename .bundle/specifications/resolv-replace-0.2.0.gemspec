# -*- encoding: utf-8 -*-
# stub: resolv-replace 0.2.0 ruby lib

Gem::Specification.new do |s|
  s.name = "resolv-replace".freeze
  s.version = "0.2.0"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.metadata = { "homepage_uri" => "https://github.com/ruby/resolv-replace", "source_code_uri" => "https://github.com/ruby/resolv-replace" } if s.respond_to? :metadata=
  s.require_paths = ["lib".freeze]
  s.authors = ["Tanaka Akira".freeze]
  s.date = "1980-01-02"
  s.description = "Replace Socket DNS with Resolv.".freeze
  s.email = ["akr@fsij.org".freeze]
  s.files = [".github/release.yml".freeze, ".github/workflows/push_gem.yml".freeze, ".gitignore".freeze, "BSDL".freeze, "COPYING".freeze, "Gemfile".freeze, "README.md".freeze, "Rakefile".freeze, "lib/resolv-replace.rb".freeze, "resolv-replace.gemspec".freeze]
  s.homepage = "https://github.com/ruby/resolv-replace".freeze
  s.licenses = ["Ruby".freeze, "BSD-2-Clause".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 2.3.0".freeze)
  s.rubygems_version = "3.4.20".freeze
  s.summary = "Replace Socket DNS with Resolv.".freeze

  s.specification_version = 4

  s.add_runtime_dependency(%q<resolv>.freeze, [">= 0"])
end
