#!/usr/bin/env ruby
# coding: utf-8

require 'socket'
require 'PinkTrace'

PF_INET6   = (PinkTrace::HAVE_IPV6 and Socket.const_defined? 'AF_INET6') ? Socket::AF_INET6 : -999
PF_NETLINK = (PinkTrace::HAVE_NETLINK and Socket.const_defined? 'AF_NETLINK') ? Socket::AF_NETLINK : -9999

def print_ret pid
  ret = PinkTrace::Syscall.get_ret pid

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

def decode_execve pid, bitness
  path = PinkTrace::String.decode pid, 0, -1, bitness
  addr = PinkTrace::Syscall.get_arg pid, 1, bitness

  print "execve(\"#{path}\", ["

  i = 0
  sep = ''
  loop do
    path = PinkTrace::StringArray.decode pid, addr, i
    if path
      print "#{sep}\"#{path}\""
      i += 1
      sep = ', '
    else
      print '], envp[])'
      break
    end
  end
end

def decode_socketcall pid, bitness, scname
  subname = nil
  if PinkTrace::Socket.has_socketcall? bitness
    subcall = PinkTrace::Socket.decode_call pid, bitness
    subname = PinkTrace::Socket.name subcall

    unless subname =~ /(bind|connect)/
      print subname + '()'
      return
    end
  end

  addr, fd = PinkTrace::Socket.decode_address_fd pid, 1, bitness
  print (subname ? subname : scname) + '(' + fd.to_s + ', '

  case addr.family
  when -1
    print 'NULL'
  when Socket::AF_UNIX
    print '{sa_family=AF_UNIX, path=' + (addr.abstract? ? '@' + addr.path : addr.path) + '}'
  when Socket::AF_INET
    print '{sa_family=AF_INET, sin_port=htons(' + addr.port.to_s + '), sin_addr=inet("' + addr.ip + '")}'
  when PF_INET6
    print '{sa_family=AF_INET6, sin6_port=htons(' + addr.port.to_s + '), inet_pton(AF_INET6, "' + addr.ipv6 + ', &sin6_addr)}'
  when PF_NETLINK
    print '{sa_family=AF_NETLINK, pid=' + addr.port.to_s + ', groups=' + sprintf('%08x', addr.groups) + '}'
  else
    print '{sa_family=???}'
  end

  print ', ' + addr.length.to_s
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
PinkTrace::Trace.setup pid, (PinkTrace::Trace::OPTION_SYSGOOD | PinkTrace::Trace::OPTION_EXEC)

# Figure out the bitness of the child.
bitness = PinkTrace::Bitness.get pid
puts "Child #{pid} runs in #{PinkTrace::Bitness.name bitness} mode"

dead = false
insyscall = false
sig = 0
exit_code = 0

loop do
  # At this point the traced child is stopped and needs to be resumed.
  PinkTrace::Trace.syscall pid, sig
  sig = 0
  Process.waitpid pid

  # Check the event, if no argument is given PinkTrace::Event.decide uses
  # $?.status and Process.waitpid sets $?.
  event = PinkTrace::Event.decide

  case event
  when PinkTrace::Event::EVENT_SYSCALL
    # We get this event twice, one at entering a system call and one at
    # exiting a system call.
    if insyscall
      print ' '
      print_ret pid
      puts
    else
      # Get the system call number and call the appropriate decoder.
      scno = PinkTrace::Syscall.get_no pid
      scname = PinkTrace::Syscall.name scno
      case scname
      when nil then print "#{scno}()"
      when 'open' then decode_open pid, bitness
      when 'execve' then decode_execve pid, bitness
      when 'socketcall'
      when 'bind'
      when 'connect' then decode_socketcall pid, bitness, scname
      else print "#{scname}()"
      end
    end
    insyscall = (not insyscall)
  when PinkTrace::Event::EVENT_EXEC
    # Update bitness
    bitness = PinkTrace::Bitness.get pid
  when PinkTrace::Event::EVENT_GENUINE
  when PinkTrace::Event::EVENT_UNKNOWN
    # Send the signal to the traced child as it was a genuine signal.
    sig = $?.stopsig
  when PinkTrace::Event::EVENT_EXIT_GENUINE
    exit_code = $?.exitstatus
    puts "Child #{pid} exited normally with return code #{exit_code}"
    dead = true
  when PinkTrace::Event::EVENT_EXIT_SIGNAL
    exit_code = 128 + $?.termsig
    puts "Child #{pid} exited with signal #{$?.termsig}"
    dead = true
  end

  break if dead
end
