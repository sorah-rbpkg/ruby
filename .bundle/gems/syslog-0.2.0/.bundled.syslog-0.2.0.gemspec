# -*- encoding: utf-8 -*-
# stub: syslog 0.2.0 ruby lib
# stub: ext/syslog/extconf.rb

Gem::Specification.new do |s|
  s.name = "syslog".freeze
  s.version = "0.2.0"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.metadata = { "homepage_uri" => "https://github.com/ruby/syslog", "source_code_uri" => "https://github.com/ruby/syslog" } if s.respond_to? :metadata=
  s.require_paths = ["lib".freeze]
  s.authors = ["Akinori MUSHA".freeze]
  s.date = "2024-12-10"
  s.description = "Ruby interface for the POSIX system logging facility.".freeze
  s.email = ["knu@idaemons.org".freeze]
  s.extensions = ["ext/syslog/extconf.rb".freeze]
  s.files = [".git-blame-ignore-revs".freeze, ".github/CODEOWNERS".freeze, ".github/dependabot.yml".freeze, ".github/workflows/push_gem.yml".freeze, ".github/workflows/test.yml".freeze, ".gitignore".freeze, "Gemfile".freeze, "LICENSE.txt".freeze, "README.md".freeze, "Rakefile".freeze, "bin/console".freeze, "bin/setup".freeze, "ext/syslog/extconf.rb".freeze, "ext/syslog/syslog.c".freeze, "ext/syslog/syslog.txt".freeze, "lib/syslog.rb".freeze, "lib/syslog/logger.rb".freeze, "syslog.gemspec".freeze]
  s.homepage = "https://github.com/ruby/syslog".freeze
  s.licenses = ["Ruby".freeze, "BSD-2-Clause".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 2.5.0".freeze)
  s.rubygems_version = "3.4.20".freeze
  s.summary = "Ruby interface for the POSIX system logging facility.".freeze
end
