# -*- encoding: utf-8 -*-
# stub: time 0.5.0 ruby lib
# default: ./lib/time.gemspec 1791726996.000000000

Gem::Specification.new do |s|
  s.name = "time".freeze
  s.version = "0.5.0"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.metadata = { "changelog_uri" => "https://github.com/ruby/time/releases", "homepage_uri" => "https://github.com/ruby/time", "source_code_uri" => "https://github.com/ruby/time" } if s.respond_to? :metadata=
  s.require_paths = ["lib".freeze]
  s.authors = ["Tanaka Akira".freeze]
  s.bindir = "exe".freeze
  s.date = "2026-10-11"
  s.description = "Extends the Time class with methods for parsing and conversion.".freeze
  s.email = ["akr@fsij.org".freeze]
  s.homepage = "https://github.com/ruby/time".freeze
  s.licenses = ["Ruby".freeze, "BSD-2-Clause".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 2.4.0".freeze)
  s.rubygems_version = "3.4.20".freeze
  s.summary = "Extends the Time class with methods for parsing and conversion.".freeze

  s.specification_version = 4

  s.add_runtime_dependency(%q<date>.freeze, [">= 0"])
end
