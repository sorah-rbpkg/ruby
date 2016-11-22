# frozen_string_literal: false
require 'optparse'
require 'erb'

C_ESC = {
  "\\" => "\\\\",
  '"' => '\"',
  "\n" => '\n',
}

0x00.upto(0x1f) {|ch| C_ESC[[ch].pack("C")] ||= "\\%03o" % ch }
0x7f.upto(0xff) {|ch| C_ESC[[ch].pack("C")] = "\\%03o" % ch }
C_ESC_PAT = Regexp.union(*C_ESC.keys)

def c_str(str)
  '"' + str.gsub(C_ESC_PAT) {|s| C_ESC[s]} + '"'
end

opt = OptionParser.new

opt.def_option('-h', 'help') {
  puts opt
  exit 0
}

opt_o = nil
opt.def_option('-o FILE', 'specify output file') {|filename|
  opt_o = filename
}

opt_H = nil
opt.def_option('-H FILE', 'specify output header file') {|filename|
  opt_H = filename
}

opt.parse!

h = {}
COMMENTS = {}

DATA.each_line {|s|
  next if /\A\s*(\#|\z)/ =~ s
  name, default_value, comment = s.chomp.split(/\s+/, 3)

  default_value = nil if default_value == 'nil'

  if h.has_key? name
    warn "#{$.}: warning: duplicate name: #{name}"
    next
  end
  h[name] = default_value
  COMMENTS[name] = comment if comment
}
DEFS = h.to_a

def each_const
  DEFS.each {|name, default_value|
    yield name, default_value
  }
end

def each_name(pat)
  DEFS.each {|name, default_value|
    next if pat !~ name
    yield name
  }
end

ERB.new(<<'EOS', nil, '%').def_method(Object, "gen_const_decls")
% each_const {|name, default_value|
#if !defined(<%=name%>)
# if defined(HAVE_CONST_<%=name.upcase%>)
#  define <%=name%> <%=name%>
%if default_value
# else
#  define <%=name%> <%=default_value%>
%end
# endif
#endif
% }
EOS

ERB.new(<<'EOS', nil, '%').def_method(Object, "gen_const_defs")
% each_const {|name, default_value|
#if defined(<%=name%>)
%   if comment = COMMENTS[name]
    /* <%=comment%> */
%   end
    rb_define_const(mod, <%=c_str name.sub(/\A_*/, '')%>, INTEGER2NUM(<%=name%>));
#endif
% }
EOS

header_result = ERB.new(<<'EOS', nil, '%').result(binding)
/* autogenerated file */

<%= gen_const_decls %>
EOS

result = ERB.new(<<'EOS', nil, '%').result(binding)
/* autogenerated file */

#ifdef HAVE_LONG_LONG
#define INTEGER2NUM(n) \
    (FIXNUM_MAX < (n) ? ULL2NUM(n) : \
     FIXNUM_MIN > (LONG_LONG)(n) ? LL2NUM(n) : \
     LONG2FIX(n))
#else
#define INTEGER2NUM(n) \
    (FIXNUM_MAX < (n) ? ULONG2NUM(n) : \
     FIXNUM_MIN > (long)(n) ? LONG2NUM(n) : \
     LONG2FIX(n))
#endif

static void
init_constants(VALUE mod)
{
<%= gen_const_defs %>
}
EOS

if opt_H
  File.open(opt_H, 'w') {|f|
    f << header_result
  }
else
  result = header_result + result
end

if opt_o
  File.open(opt_o, 'w') {|f|
    f << result
  }
else
  $stdout << result
end

__END__
# SUSv4
_SC_AIO_LISTIO_MAX
_SC_AIO_MAX
_SC_AIO_PRIO_DELTA_MAX
_SC_ARG_MAX
_SC_ATEXIT_MAX
_SC_BC_BASE_MAX
_SC_BC_DIM_MAX
_SC_BC_SCALE_MAX
_SC_BC_STRING_MAX
_SC_CHILD_MAX
_SC_CLK_TCK
_SC_COLL_WEIGHTS_MAX
_SC_DELAYTIMER_MAX
_SC_EXPR_NEST_MAX
_SC_HOST_NAME_MAX
_SC_IOV_MAX
_SC_LINE_MAX
_SC_LOGIN_NAME_MAX
_SC_NGROUPS_MAX
_SC_GETGR_R_SIZE_MAX
_SC_GETPW_R_SIZE_MAX
_SC_MQ_OPEN_MAX
_SC_MQ_PRIO_MAX
_SC_OPEN_MAX
_SC_ADVISORY_INFO
_SC_BARRIERS
_SC_ASYNCHRONOUS_IO
_SC_CLOCK_SELECTION
_SC_CPUTIME
_SC_FSYNC
_SC_IPV6
_SC_JOB_CONTROL
_SC_MAPPED_FILES
_SC_MEMLOCK
_SC_MEMLOCK_RANGE
_SC_MEMORY_PROTECTION
_SC_MESSAGE_PASSING
_SC_MONOTONIC_CLOCK
_SC_PRIORITIZED_IO
_SC_PRIORITY_SCHEDULING
_SC_RAW_SOCKETS
_SC_READER_WRITER_LOCKS
_SC_REALTIME_SIGNALS
_SC_REGEXP
_SC_SAVED_IDS
_SC_SEMAPHORES
_SC_SHARED_MEMORY_OBJECTS
_SC_SHELL
_SC_SPAWN
_SC_SPIN_LOCKS
_SC_SPORADIC_SERVER
_SC_SS_REPL_MAX
_SC_SYNCHRONIZED_IO
_SC_THREAD_ATTR_STACKADDR
_SC_THREAD_ATTR_STACKSIZE
_SC_THREAD_CPUTIME
_SC_THREAD_PRIO_INHERIT
_SC_THREAD_PRIO_PROTECT
_SC_THREAD_PRIORITY_SCHEDULING
_SC_THREAD_PROCESS_SHARED
_SC_THREAD_ROBUST_PRIO_INHERIT
_SC_THREAD_ROBUST_PRIO_PROTECT
_SC_THREAD_SAFE_FUNCTIONS
_SC_THREAD_SPORADIC_SERVER
_SC_THREADS
_SC_TIMEOUTS
_SC_TIMERS
_SC_TRACE
_SC_TRACE_EVENT_FILTER
_SC_TRACE_EVENT_NAME_MAX
_SC_TRACE_INHERIT
_SC_TRACE_LOG
_SC_TRACE_NAME_MAX
_SC_TRACE_SYS_MAX
_SC_TRACE_USER_EVENT_MAX
_SC_TYPED_MEMORY_OBJECTS
_SC_VERSION
_SC_V7_ILP32_OFF32
_SC_V7_ILP32_OFFBIG
_SC_V7_LP64_OFF64
_SC_V7_LPBIG_OFFBIG
_SC_V6_ILP32_OFF32
_SC_V6_ILP32_OFFBIG
_SC_V6_LP64_OFF64
_SC_V6_LPBIG_OFFBIG
_SC_2_C_BIND
_SC_2_C_DEV
_SC_2_CHAR_TERM
_SC_2_FORT_DEV
_SC_2_FORT_RUN
_SC_2_LOCALEDEF
_SC_2_PBS
_SC_2_PBS_ACCOUNTING
_SC_2_PBS_CHECKPOINT
_SC_2_PBS_LOCATE
_SC_2_PBS_MESSAGE
_SC_2_PBS_TRACK
_SC_2_SW_DEV
_SC_2_UPE
_SC_2_VERSION
_SC_PAGE_SIZE
_SC_PAGESIZE
_SC_THREAD_DESTRUCTOR_ITERATIONS
_SC_THREAD_KEYS_MAX
_SC_THREAD_STACK_MIN
_SC_THREAD_THREADS_MAX
_SC_RE_DUP_MAX
_SC_RTSIG_MAX
_SC_SEM_NSEMS_MAX
_SC_SEM_VALUE_MAX
_SC_SIGQUEUE_MAX
_SC_STREAM_MAX
_SC_SYMLOOP_MAX
_SC_TIMER_MAX
_SC_TTY_NAME_MAX
_SC_TZNAME_MAX
_SC_XOPEN_CRYPT
_SC_XOPEN_ENH_I18N
_SC_XOPEN_REALTIME
_SC_XOPEN_REALTIME_THREADS
_SC_XOPEN_SHM
_SC_XOPEN_STREAMS
_SC_XOPEN_UNIX
_SC_XOPEN_UUCP
_SC_XOPEN_VERSION

# non-standard
_SC_PHYS_PAGES
_SC_AVPHYS_PAGES
_SC_NPROCESSORS_CONF
_SC_NPROCESSORS_ONLN
_SC_CPUSET_SIZE

# SUSv4
_CS_PATH
_CS_POSIX_V7_ILP32_OFF32_CFLAGS
_CS_POSIX_V7_ILP32_OFF32_LDFLAGS
_CS_POSIX_V7_ILP32_OFF32_LIBS
_CS_POSIX_V7_ILP32_OFFBIG_CFLAGS
_CS_POSIX_V7_ILP32_OFFBIG_LDFLAGS
_CS_POSIX_V7_ILP32_OFFBIG_LIBS
_CS_POSIX_V7_LP64_OFF64_CFLAGS
_CS_POSIX_V7_LP64_OFF64_LDFLAGS
_CS_POSIX_V7_LP64_OFF64_LIBS
_CS_POSIX_V7_LPBIG_OFFBIG_CFLAGS
_CS_POSIX_V7_LPBIG_OFFBIG_LDFLAGS
_CS_POSIX_V7_LPBIG_OFFBIG_LIBS
_CS_POSIX_V7_THREADS_CFLAGS
_CS_POSIX_V7_THREADS_LDFLAGS
_CS_POSIX_V7_WIDTH_RESTRICTED_ENVS
_CS_V7_ENV
_CS_POSIX_V6_ILP32_OFF32_CFLAGS
_CS_POSIX_V6_ILP32_OFF32_LDFLAGS
_CS_POSIX_V6_ILP32_OFF32_LIBS
_CS_POSIX_V6_ILP32_OFFBIG_CFLAGS
_CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS
_CS_POSIX_V6_ILP32_OFFBIG_LIBS
_CS_POSIX_V6_LP64_OFF64_CFLAGS
_CS_POSIX_V6_LP64_OFF64_LDFLAGS
_CS_POSIX_V6_LP64_OFF64_LIBS
_CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS
_CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS
_CS_POSIX_V6_LPBIG_OFFBIG_LIBS
_CS_POSIX_V6_WIDTH_RESTRICTED_ENVS
_CS_V6_ENV

# non-standard
_CS_GNU_LIBC_VERSION
_CS_GNU_LIBPTHREAD_VERSION

# SUSv4
_PC_FILESIZEBITS
_PC_LINK_MAX
_PC_MAX_CANON
_PC_MAX_INPUT
_PC_NAME_MAX
_PC_PATH_MAX
_PC_PIPE_BUF
_PC_2_SYMLINKS
_PC_ALLOC_SIZE_MIN
_PC_REC_INCR_XFER_SIZE
_PC_REC_MAX_XFER_SIZE
_PC_REC_MIN_XFER_SIZE
_PC_REC_XFER_ALIGN
_PC_SYMLINK_MAX
_PC_CHOWN_RESTRICTED
_PC_NO_TRUNC
_PC_VDISABLE
_PC_ASYNC_IO
_PC_PRIO_IO
_PC_SYNC_IO
_PC_TIMESTAMP_RESOLUTION

