# -*- encoding: utf-8 -*-
# stub: fiddle 1.1.8 ruby lib
# stub: ext/fiddle/extconf.rb

Gem::Specification.new do |s|
  s.name = "fiddle".freeze
  s.version = "1.1.8"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.metadata = { "changelog_uri" => "https://github.com/ruby/fiddle/releases", "msys2_mingw_dependencies" => "libffi" } if s.respond_to? :metadata=
  s.require_paths = ["lib".freeze]
  s.authors = ["Aaron Patterson".freeze, "SHIBATA Hiroshi".freeze]
  s.date = "1980-01-02"
  s.description = "A libffi wrapper for Ruby.".freeze
  s.email = ["aaron@tenderlovemaking.com".freeze, "hsbt@ruby-lang.org".freeze]
  s.extensions = ["ext/fiddle/extconf.rb".freeze]
  s.files = ["LICENSE.txt".freeze, "README.md".freeze, "Rakefile".freeze, "ext/fiddle/closure.c".freeze, "ext/fiddle/closure.h".freeze, "ext/fiddle/conversions.c".freeze, "ext/fiddle/conversions.h".freeze, "ext/fiddle/depend".freeze, "ext/fiddle/extconf.rb".freeze, "ext/fiddle/fiddle.c".freeze, "ext/fiddle/fiddle.h".freeze, "ext/fiddle/function.c".freeze, "ext/fiddle/function.h".freeze, "ext/fiddle/handle.c".freeze, "ext/fiddle/memory_view.c".freeze, "ext/fiddle/pinned.c".freeze, "ext/fiddle/pointer.c".freeze, "fiddle.gemspec".freeze, "lib/fiddle.rb".freeze, "lib/fiddle/closure.rb".freeze, "lib/fiddle/cparser.rb".freeze, "lib/fiddle/ffi_backend.rb".freeze, "lib/fiddle/function.rb".freeze, "lib/fiddle/import.rb".freeze, "lib/fiddle/pack.rb".freeze, "lib/fiddle/struct.rb".freeze, "lib/fiddle/types.rb".freeze, "lib/fiddle/value.rb".freeze, "lib/fiddle/version.rb".freeze]
  s.homepage = "https://github.com/ruby/fiddle".freeze
  s.licenses = ["Ruby".freeze, "BSD-2-Clause".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 2.5.0".freeze)
  s.rubygems_version = "3.4.20".freeze
  s.summary = "A libffi wrapper for Ruby.".freeze
end
