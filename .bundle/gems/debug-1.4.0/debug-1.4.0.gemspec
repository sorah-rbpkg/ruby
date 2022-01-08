# -*- encoding: utf-8 -*-
# stub: debug 1.4.0 ruby lib
# stub: ext/debug/extconf.rb

Gem::Specification.new do |s|
  s.name = "debug".freeze
  s.version = "1.4.0"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.metadata = { "homepage_uri" => "https://github.com/ruby/debug", "source_code_uri" => "https://github.com/ruby/debug" } if s.respond_to? :metadata=
  s.require_paths = ["lib".freeze]
  s.authors = ["Koichi Sasada".freeze]
  s.bindir = "exe".freeze
  s.date = "2021-12-17"
  s.description = "Debugging functionality for Ruby. This is completely rewritten debug.rb which was contained by the encient Ruby versions.".freeze
  s.email = ["ko1@atdot.net".freeze]
  s.executables = ["rdbg".freeze]
  s.extensions = ["ext/debug/extconf.rb".freeze]
  s.files = [".github/ISSUE_TEMPLATE/bug_report.md".freeze, ".github/ISSUE_TEMPLATE/custom.md".freeze, ".github/ISSUE_TEMPLATE/feature_request.md".freeze, ".github/pull_request_template.md".freeze, ".github/workflows/ruby.yml".freeze, ".gitignore".freeze, "CONTRIBUTING.md".freeze, "Gemfile".freeze, "LICENSE.txt".freeze, "README.md".freeze, "Rakefile".freeze, "TODO.md".freeze, "bin/console".freeze, "bin/gentest".freeze, "bin/setup".freeze, "debug.gemspec".freeze, "exe/rdbg".freeze, "ext/debug/debug.c".freeze, "ext/debug/extconf.rb".freeze, "ext/debug/iseq_collector.c".freeze, "lib/debug.rb".freeze, "lib/debug/bp.vim".freeze, "lib/debug/breakpoint.rb".freeze, "lib/debug/client.rb".freeze, "lib/debug/color.rb".freeze, "lib/debug/config.rb".freeze, "lib/debug/console.rb".freeze, "lib/debug/frame_info.rb".freeze, "lib/debug/local.rb".freeze, "lib/debug/open.rb".freeze, "lib/debug/open_nonstop.rb".freeze, "lib/debug/prelude.rb".freeze, "lib/debug/server.rb".freeze, "lib/debug/server_cdp.rb".freeze, "lib/debug/server_dap.rb".freeze, "lib/debug/session.rb".freeze, "lib/debug/source_repository.rb".freeze, "lib/debug/start.rb".freeze, "lib/debug/thread_client.rb".freeze, "lib/debug/tracer.rb".freeze, "lib/debug/version.rb".freeze, "misc/README.md.erb".freeze]
  s.homepage = "https://github.com/ruby/debug".freeze
  s.licenses = ["Ruby".freeze, "BSD-2-Clause".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 2.6.0".freeze)
  s.rubygems_version = "3.1.2".freeze
  s.summary = "Debugging functionality for Ruby".freeze

  if s.respond_to? :specification_version then
    s.specification_version = 4
  end

  if s.respond_to? :add_runtime_dependency then
    s.add_runtime_dependency(%q<irb>.freeze, [">= 1.3.6"])
    s.add_runtime_dependency(%q<reline>.freeze, [">= 0.2.7"])
  else
    s.add_dependency(%q<irb>.freeze, [">= 1.3.6"])
    s.add_dependency(%q<reline>.freeze, [">= 0.2.7"])
  end
end
