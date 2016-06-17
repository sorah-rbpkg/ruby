# Found on Debian mips* buildds, this test consumes ~2GB RAM and
# a lot of CPU time before failing. Note that the test failure
# may point to an issue in the Array implementation.
# https://bugs.ruby-lang.org/issues/12500
exclude :test_aspawn_too_long_path, "RAM and time consuming test"
