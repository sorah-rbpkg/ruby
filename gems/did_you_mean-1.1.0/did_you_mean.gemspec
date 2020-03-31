# -*- encoding: utf-8 -*-
# stub: did_you_mean 1.1.0 ruby lib

Gem::Specification.new do |s|
  s.name = "did_you_mean".freeze
  s.version = "1.1.0"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.require_paths = ["lib".freeze]
  s.authors = ["Yuki Nishijima".freeze]
  s.date = "2016-12-19"
  s.description = "The gem that has been saving people from typos since 2014.".freeze
  s.email = ["mail@yukinishijima.net".freeze]
  s.files = [".gitignore".freeze, ".ruby-version".freeze, ".travis.yml".freeze, "CHANGELOG.md".freeze, "Gemfile".freeze, "LICENSE.txt".freeze, "README.md".freeze, "Rakefile".freeze, "benchmark/jaro_winkler/memory_usage.rb".freeze, "benchmark/jaro_winkler/speed.rb".freeze, "benchmark/levenshtein/memory_usage.rb".freeze, "benchmark/levenshtein/speed.rb".freeze, "benchmark/memory_usage.rb".freeze, "did_you_mean.gemspec".freeze, "doc/CHANGELOG.md.erb".freeze, "doc/changelog_generator.rb".freeze, "evaluation/calculator.rb".freeze, "evaluation/dictionary_generator.rb".freeze, "evaluation/incorrect_words.yaml".freeze, "lib/did_you_mean.rb".freeze, "lib/did_you_mean/core_ext/name_error.rb".freeze, "lib/did_you_mean/experimental.rb".freeze, "lib/did_you_mean/experimental/initializer_name_correction.rb".freeze, "lib/did_you_mean/experimental/ivar_name_correction.rb".freeze, "lib/did_you_mean/experimental/key_error_name_correction.rb".freeze, "lib/did_you_mean/formatter.rb".freeze, "lib/did_you_mean/jaro_winkler.rb".freeze, "lib/did_you_mean/levenshtein.rb".freeze, "lib/did_you_mean/spell_checker.rb".freeze, "lib/did_you_mean/spell_checkers/method_name_checker.rb".freeze, "lib/did_you_mean/spell_checkers/name_error_checkers.rb".freeze, "lib/did_you_mean/spell_checkers/name_error_checkers/class_name_checker.rb".freeze, "lib/did_you_mean/spell_checkers/name_error_checkers/variable_name_checker.rb".freeze, "lib/did_you_mean/spell_checkers/null_checker.rb".freeze, "lib/did_you_mean/verbose_formatter.rb".freeze, "lib/did_you_mean/version.rb".freeze, "test/core_ext/name_error_extension_test.rb".freeze, "test/edit_distance/jaro_winkler_test.rb".freeze, "test/experimental/initializer_name_correction_test.rb".freeze, "test/experimental/key_error_test.rb".freeze, "test/experimental/method_name_checker_test.rb".freeze, "test/spell_checker_test.rb".freeze, "test/spell_checking/class_name_test.rb".freeze, "test/spell_checking/method_name_test.rb".freeze, "test/spell_checking/uncorrectable_name_test.rb".freeze, "test/spell_checking/variable_name_test.rb".freeze, "test/test_helper.rb".freeze, "test/verbose_formatter_test.rb".freeze]
  s.homepage = "https://github.com/yuki24/did_you_mean".freeze
  s.licenses = ["MIT".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 2.4.0dev".freeze)
  s.rubygems_version = "3.1.2".freeze
  s.summary = "\"Did you mean?\" experience in Ruby".freeze
  s.test_files = ["test/core_ext/name_error_extension_test.rb".freeze, "test/edit_distance/jaro_winkler_test.rb".freeze, "test/experimental/initializer_name_correction_test.rb".freeze, "test/experimental/key_error_test.rb".freeze, "test/experimental/method_name_checker_test.rb".freeze, "test/spell_checker_test.rb".freeze, "test/spell_checking/class_name_test.rb".freeze, "test/spell_checking/method_name_test.rb".freeze, "test/spell_checking/uncorrectable_name_test.rb".freeze, "test/spell_checking/variable_name_test.rb".freeze, "test/test_helper.rb".freeze, "test/verbose_formatter_test.rb".freeze]

  if s.respond_to? :specification_version then
    s.specification_version = 4
  end

  if s.respond_to? :add_runtime_dependency then
    s.add_development_dependency(%q<bundler>.freeze, [">= 0"])
    s.add_development_dependency(%q<rake>.freeze, [">= 0"])
    s.add_development_dependency(%q<minitest>.freeze, [">= 0"])
  else
    s.add_dependency(%q<bundler>.freeze, [">= 0"])
    s.add_dependency(%q<rake>.freeze, [">= 0"])
    s.add_dependency(%q<minitest>.freeze, [">= 0"])
  end
end
