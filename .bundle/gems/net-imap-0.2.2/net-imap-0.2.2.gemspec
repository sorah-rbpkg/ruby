# -*- encoding: utf-8 -*-
# stub: net-imap 0.2.2 ruby lib

Gem::Specification.new do |s|
  s.name = "net-imap".freeze
  s.version = "0.2.2"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.metadata = { "homepage_uri" => "https://github.com/ruby/net-imap", "source_code_uri" => "https://github.com/ruby/net-imap" } if s.respond_to? :metadata=
  s.require_paths = ["lib".freeze]
  s.authors = ["Shugo Maeda".freeze]
  s.bindir = "exe".freeze
  s.date = "2021-07-07"
  s.description = "Ruby client api for Internet Message Access Protocol".freeze
  s.email = ["shugo@ruby-lang.org".freeze]
  s.files = [".github/workflows/test.yml".freeze, ".gitignore".freeze, "Gemfile".freeze, "LICENSE.txt".freeze, "README.md".freeze, "Rakefile".freeze, "bin/console".freeze, "bin/setup".freeze, "lib/net/imap.rb".freeze, "lib/net/imap/authenticators.rb".freeze, "lib/net/imap/authenticators/cram_md5.rb".freeze, "lib/net/imap/authenticators/digest_md5.rb".freeze, "lib/net/imap/authenticators/login.rb".freeze, "lib/net/imap/authenticators/plain.rb".freeze, "lib/net/imap/command_data.rb".freeze, "lib/net/imap/data_encoding.rb".freeze, "lib/net/imap/flags.rb".freeze, "lib/net/imap/response_data.rb".freeze, "lib/net/imap/response_parser.rb".freeze, "net-imap.gemspec".freeze]
  s.homepage = "https://github.com/ruby/net-imap".freeze
  s.licenses = ["Ruby".freeze, "BSD-2-Clause".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 2.5.0".freeze)
  s.rubygems_version = "3.1.2".freeze
  s.summary = "Ruby client api for Internet Message Access Protocol".freeze

  if s.respond_to? :specification_version then
    s.specification_version = 4
  end

  if s.respond_to? :add_runtime_dependency then
    s.add_runtime_dependency(%q<net-protocol>.freeze, [">= 0"])
    s.add_runtime_dependency(%q<digest>.freeze, [">= 0"])
    s.add_runtime_dependency(%q<strscan>.freeze, [">= 0"])
  else
    s.add_dependency(%q<net-protocol>.freeze, [">= 0"])
    s.add_dependency(%q<digest>.freeze, [">= 0"])
    s.add_dependency(%q<strscan>.freeze, [">= 0"])
  end
end
