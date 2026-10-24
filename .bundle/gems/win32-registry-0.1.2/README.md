# Win32::Registry

win32-registry is registry accessor library for the Windows platform.
It uses Fiddle to call Windows Registry APIs.

## Installation

win32-registry is a bundled gem of the ruby standard library, so that the latest version on the ruby release date is preinstalled on Windows.
Other versions of the gem can be installed in addition like so:

Install the gem and add to the application's Gemfile by executing:

  ```bash
  bundle add win32-registry
  ```

If bundler is not being used to manage dependencies, install the gem by executing:

  ```bash
  gem install win32-registry
  ```

## Usage

```ruby
  require "win32/registry"

  Win32::Registry::HKEY_CURRENT_USER.open('SOFTWARE\foo') do |reg|
    value = reg['foo']                               # read a value
    value = reg['foo', Win32::Registry::REG_SZ]      # read a value with type
    type, value = reg.read('foo')                    # read a value
    reg['foo'] = 'bar'                               # write a value
    reg['foo', Win32::Registry::REG_SZ] = 'bar'      # write a value with type
    reg.write('foo', Win32::Registry::REG_SZ, 'bar') # write a value

    reg.each_value { |name, type, data| ... }        # Enumerate values
    reg.each_key { |key, wtime| ... }                # Enumerate subkeys

    reg.delete_value(name)                         # Delete a value
    reg.delete_key(name)                           # Delete a subkey
    reg.delete_key(name, true)                     # Delete a subkey recursively
  end
```

Be sure to use backslashes "\\" as the path separator. Forward slashes "/" will not work.
This is in contrast to file paths, which accept forward and backward slashes at will on Windows.

## Development

After checking out the repo, run `bin/setup` to install dependencies. Then, run `rake test` to run the tests. You can also run `bin/console` for an interactive prompt that will allow you to experiment.

To install this gem onto your local machine, run `bundle exec rake install`. To release a new version, update the version number in `version.rb`, and then run `bundle exec rake release`, which will create a git tag for the version, push git commits and the created tag, and push the `.gem` file to [rubygems.org](https://rubygems.org).

## Contributing

Bug reports and pull requests are welcome on GitHub at https://github.com/ruby/win32-registry.
