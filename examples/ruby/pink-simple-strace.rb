#!/usr/bin/env ruby
# coding: utf-8
# vim: set sw=2 sts=2 et nowrap fenc=utf-8 :

require 'PinkTrace'

def print_ret pid
  ret = PinkTrace::SysCall.get_ret pid

  if ret >= 0
    print "= #{ret}"
  else
    print "= #{ret} #{Errno.constants[-ret]}"
  end
end

def decode_open pid, bitness
  path = PinkTrace::String.decode pid, 0, -1, bitness
  flags = PinkTrace::SysCall.get_arg pid, 1, bitness

  print "open(\"#{path}\", #{flags})"
end

unless ARGV.size > 0
  puts "Usage: #{$0} program [arguments..]"
  exit 1
end

pid = PinkTrace::Fork.fork(PinkTrace::Trace::OPTION_EXEC) { exec(*ARGV) }

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
      scno = PinkTrace::SysCall.get_no pid
      scname = PinkTrace::SysCall.name scno
      if not scname
        print "#{scno}()"
      elsif scname == 'open'
        decode_open pid, bitness
      else
        print "#{scname}()"
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
