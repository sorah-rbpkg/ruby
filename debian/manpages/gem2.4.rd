=begin
= NAME

gem.4 - frontend to RubyGems, the Ruby package manager

= SYNOPSIS

gem.4 command [arguments...] [options...]

= DESCRIPTION

gem.4 is the frontend to RubyGems, the standard package manager for Ruby.
This is a basic help message containing pointers to more information.

Further help:

: gem.4 help commands
  list all gem.4 commands

: gem.4 help examples
  shows some examples of usage

: gem.4 help ((|COMMAND|))
  show help on COMMAND, (e.g. 'gem.4 help install')

= LINKS

http://rubygems.org/

= EXAMPLES

gem.4 install rake
gem.4 list --local
gem.4 build package.gemspec
gem.4 help install

= SEE ALSO

bundle(1)

=end
