# -*- encoding: utf-8 -*-
# stub: net-protocol 0.4.0 ruby lib
# default: ./lib/net/net-protocol.gemspec 1791726996.000000000

Gem::Specification.new do |s|
  s.name = "net-protocol".freeze
  s.version = "0.4.0"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.metadata = { "changelog_uri" => "https://github.com/ruby/net-protocol/releases", "homepage_uri" => "https://github.com/ruby/net-protocol", "source_code_uri" => "https://github.com/ruby/net-protocol" } if s.respond_to? :metadata=
  s.require_paths = ["lib".freeze]
  s.authors = ["Yukihiro Matsumoto".freeze]
  s.date = "2026-10-11"
  s.description = "The abstract interface for net-* client.".freeze
  s.email = ["matz@ruby-lang.org".freeze]
  s.homepage = "https://github.com/ruby/net-protocol".freeze
  s.licenses = ["Ruby".freeze, "BSD-2-Clause".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 2.6.0".freeze)
  s.rubygems_version = "3.4.20".freeze
  s.summary = "The abstract interface for net-* client.".freeze

  s.specification_version = 4

  s.add_runtime_dependency(%q<timeout>.freeze, [">= 0"])
end
