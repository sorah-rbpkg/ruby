# pid 46356 killed by SIGABRT (signal 6)
# | -:8: [BUG] rb_maygvl_fd_fix_cloexec: fcntl(-1, F_GETFD) failed: Bad file descriptor
# | ruby 2.5.1p57 (2018-03-29 revision 63029) [i386-kfreebsd-gnu]
# | [NOTE]
# | You may have encountered a bug in the Ruby interpreter or extension libraries.
# | Bug reports are welcome.
# | For details: http://www.ruby-lang.org/bugreport.html
exclude :test_dup_many, 'fails on kfreebsd-i386'
