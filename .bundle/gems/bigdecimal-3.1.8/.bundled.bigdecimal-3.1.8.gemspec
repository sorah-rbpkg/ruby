# -*- encoding: utf-8 -*-
# stub: bigdecimal 3.1.8 ruby lib
# stub: ext/bigdecimal/extconf.rb

Gem::Specification.new do |s|
  s.name = "bigdecimal".freeze
  s.version = "3.1.8"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.metadata = { "changelog_uri" => "https://github.com/ruby/bigdecimal/blob/master/CHANGES.md" } if s.respond_to? :metadata=
  s.require_paths = ["lib".freeze]
  s.authors = ["Kenta Murata".freeze, "Zachary Scott".freeze, "Shigeo Kobayashi".freeze]
  s.date = "2024-05-07"
  s.description = "This library provides arbitrary-precision decimal floating-point number class.".freeze
  s.email = ["mrkn@mrkn.jp".freeze]
  s.extensions = ["ext/bigdecimal/extconf.rb".freeze]
  s.files = ["LICENSE".freeze, "bigdecimal.gemspec".freeze, "ext/bigdecimal/bigdecimal.c".freeze, "ext/bigdecimal/bigdecimal.h".freeze, "ext/bigdecimal/bits.h".freeze, "ext/bigdecimal/extconf.rb".freeze, "ext/bigdecimal/feature.h".freeze, "ext/bigdecimal/missing.c".freeze, "ext/bigdecimal/missing.h".freeze, "ext/bigdecimal/missing/dtoa.c".freeze, "ext/bigdecimal/static_assert.h".freeze, "lib/bigdecimal.rb".freeze, "lib/bigdecimal/jacobian.rb".freeze, "lib/bigdecimal/ludcmp.rb".freeze, "lib/bigdecimal/math.rb".freeze, "lib/bigdecimal/newton.rb".freeze, "lib/bigdecimal/util.rb".freeze, "sample/linear.rb".freeze, "sample/nlsolve.rb".freeze, "sample/pi.rb".freeze]
  s.homepage = "https://github.com/ruby/bigdecimal".freeze
  s.licenses = ["Ruby".freeze, "BSD-2-Clause".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 2.5.0".freeze)
  s.rubygems_version = "3.4.20".freeze
  s.summary = "Arbitrary-precision decimal floating-point number library.".freeze
end
