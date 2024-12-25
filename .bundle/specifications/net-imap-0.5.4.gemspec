# -*- encoding: utf-8 -*-
# stub: net-imap 0.5.4 ruby lib

Gem::Specification.new do |s|
  s.name = "net-imap".freeze
  s.version = "0.5.4"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.metadata = { "changelog_uri" => "https://github.com/ruby/net-imap/releases", "homepage_uri" => "https://github.com/ruby/net-imap", "source_code_uri" => "https://github.com/ruby/net-imap" } if s.respond_to? :metadata=
  s.require_paths = ["lib".freeze]
  s.authors = ["Shugo Maeda".freeze, "nicholas a. evans".freeze]
  s.bindir = "exe".freeze
  s.date = "2024-12-22"
  s.description = "Ruby client api for Internet Message Access Protocol".freeze
  s.email = ["shugo@ruby-lang.org".freeze, "nick@rubinick.dev".freeze]
  s.files = ["BSDL".freeze, "COPYING".freeze, "Gemfile".freeze, "LICENSE.txt".freeze, "README.md".freeze, "Rakefile".freeze, "docs/styles.css".freeze, "lib/net/imap.rb".freeze, "lib/net/imap/authenticators.rb".freeze, "lib/net/imap/command_data.rb".freeze, "lib/net/imap/config.rb".freeze, "lib/net/imap/config/attr_accessors.rb".freeze, "lib/net/imap/config/attr_inheritance.rb".freeze, "lib/net/imap/config/attr_type_coercion.rb".freeze, "lib/net/imap/data_encoding.rb".freeze, "lib/net/imap/data_lite.rb".freeze, "lib/net/imap/deprecated_client_options.rb".freeze, "lib/net/imap/errors.rb".freeze, "lib/net/imap/esearch_result.rb".freeze, "lib/net/imap/fetch_data.rb".freeze, "lib/net/imap/flags.rb".freeze, "lib/net/imap/response_data.rb".freeze, "lib/net/imap/response_parser.rb".freeze, "lib/net/imap/response_parser/parser_utils.rb".freeze, "lib/net/imap/sasl.rb".freeze, "lib/net/imap/sasl/anonymous_authenticator.rb".freeze, "lib/net/imap/sasl/authentication_exchange.rb".freeze, "lib/net/imap/sasl/authenticators.rb".freeze, "lib/net/imap/sasl/client_adapter.rb".freeze, "lib/net/imap/sasl/cram_md5_authenticator.rb".freeze, "lib/net/imap/sasl/digest_md5_authenticator.rb".freeze, "lib/net/imap/sasl/external_authenticator.rb".freeze, "lib/net/imap/sasl/gs2_header.rb".freeze, "lib/net/imap/sasl/login_authenticator.rb".freeze, "lib/net/imap/sasl/oauthbearer_authenticator.rb".freeze, "lib/net/imap/sasl/plain_authenticator.rb".freeze, "lib/net/imap/sasl/protocol_adapters.rb".freeze, "lib/net/imap/sasl/scram_algorithm.rb".freeze, "lib/net/imap/sasl/scram_authenticator.rb".freeze, "lib/net/imap/sasl/stringprep.rb".freeze, "lib/net/imap/sasl/xoauth2_authenticator.rb".freeze, "lib/net/imap/sasl_adapter.rb".freeze, "lib/net/imap/search_result.rb".freeze, "lib/net/imap/sequence_set.rb".freeze, "lib/net/imap/stringprep.rb".freeze, "lib/net/imap/stringprep/nameprep.rb".freeze, "lib/net/imap/stringprep/saslprep.rb".freeze, "lib/net/imap/stringprep/saslprep_tables.rb".freeze, "lib/net/imap/stringprep/tables.rb".freeze, "lib/net/imap/stringprep/trace.rb".freeze, "lib/net/imap/vanished_data.rb".freeze, "net-imap.gemspec".freeze, "rakelib/benchmarks.rake".freeze, "rakelib/rdoc.rake".freeze, "rakelib/rfcs.rake".freeze, "rakelib/saslprep.rake".freeze, "rakelib/string_prep_tables_generator.rb".freeze, "sample/net-imap.rb".freeze]
  s.homepage = "https://github.com/ruby/net-imap".freeze
  s.licenses = ["Ruby".freeze, "BSD-2-Clause".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 3.1.0".freeze)
  s.rubygems_version = "3.4.20".freeze
  s.summary = "Ruby client api for Internet Message Access Protocol".freeze

  s.specification_version = 4

  s.add_runtime_dependency(%q<date>.freeze, [">= 0"])
end
