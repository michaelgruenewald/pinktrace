#!/usr/bin/env ruby
# coding: utf-8
# vim: set sw=2 sts=2 et nowrap fenc=utf-8 :

require 'PinkTrace'

SIGSTOP = Signal.list['STOP']
SIGTRAP = Signal.list['TRAP']

def print_ret ret
  if ret >= 0
    print "= #{ret}"
  else
    print "= #{ret} #{Errno.constants[-ret]}"
  end
end

def decode_open pid, bitness
  path = PinkTrace::String.decode pid, 0, -1, bitness
  flags = PinkTrace::Syscall.get_arg pid, 1, bitness

  print "open(\"#{path}\", #{flags})"
end

unless ARGV.size > 0
  puts "Usage: #{$0} program [arguments..]"
  exit 1
end

pid = fork do
  PinkTrace::Trace.me
  Process.kill 'STOP', Process.pid

  exec(*ARGV)
end

Process.waitpid pid
unless $?.stopped?
  puts(sprintf("%#x", $?.to_i))
  exit 1
end
unless $?.stopsig == Signal.list['STOP']
  puts(sprintf("%#x", $?.to_i))
  exit 2
end

# Figure out the bitness of the child.
bitness = PinkTrace::Bitness.get pid
puts "Child #{pid} runs in #{PinkTrace::Bitness.name bitness} mode"

inexecve = false
insyscall = false
sig = 0
exit_code = 0

loop do
  # At this point the traced child is stopped and needs to be resumed.
  PinkTrace::Trace.syscall pid, sig
  sig = 0
  Process.waitpid pid

  if $?.stopped?
    if $?.stopsig == SIGTRAP
      # We get this event twice, one at entering a
      # system call and one at exiting a system call.
      if insyscall
        ret = PinkTrace::Syscall.get_ret pid
        if inexecve
          inexecve = false
          if ret == 0
            # Update bitness
            bitness = PinkTrace::Bitness.get pid
            next
          end
        end
        print ' '
        print_ret(ret)
        puts
        insyscall = false
      else
        # Get the system call number and call the appropriate decoder.
        scno = PinkTrace::Syscall.get_no pid
        scname = PinkTrace::Syscall.name scno
        inexecve = true if scname == 'execve'
        if not scname
          print "#{scno}()"
        elsif scname == 'open'
          decode_open pid, bitness
        else
          print "#{scname}()"
        end
        insyscall = true
      end
    else
      # Child has received a genuine signal, send it to the child.
      sig = $?.stopsig
    end
  elsif $?.exited?
    puts "Child #{pid} exited normally with return code #{exit_code}"
    exit $?.exitstatus
  elsif $?.signaled?
    puts "Child #{pid} exited with signal #{$?.termsig}"
    exit 128 + $?.termsig
  end
end
