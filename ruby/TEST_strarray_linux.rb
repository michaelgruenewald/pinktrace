#!/usr/bin/env ruby
# coding: utf-8
# vim: set sw=2 sts=2 et nowrap fenc=utf-8 :

require 'test/unit'

$:.insert(0, '.libs')
require 'PinkTrace'

class TestPinkStringArray
  def test_string_array_decode
    pid = fork do
      PinkTrace::Trace.me
      Process.kill 'STOP', Process.pid

      exec 'true' ['/dev/null']
    end
    Process.waitpid pid
    PinkTrace::Trace.setup pid

    # Loop until we get to the open() system call as there's no guarantee that
    # other system calls won't be called beforehand.
    event = -1
    while event != PinkTrace::Event::EVENT_EXIT_GENUINE
      PinkTrace::Trace.syscall pid
      Process.waitpid pid

      event = PinkTrace::Event.decide
      if event == PinkTrace::Event::EVENT_SYSCALL then
        scno = PinkTrace::Syscall.get_no pid
        name = PinkTrace::Syscall.name scno
        if name == 'execve' then
          arg = PinkTrace::Syscall.get_arg pid, 1
          str = PinkTrace::StringArray.decode pid, arg, 0
          assert(str == '/dev/null', "Wrong string, expected: /dev/null got: '#{str}'")
          break
        end
      end
    end

    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_strarray_decode_max
    pid = fork do
      PinkTrace::Trace.me
      Process.kill 'STOP', Process.pid

      exec 'true' ['/dev/null']
    end
    Process.waitpid pid
    PinkTrace::Trace.setup pid

    # Loop until we get to the open() system call as there's no guarantee that
    # other system calls won't be called beforehand.
    event = -1
    while event != PinkTrace::Event::EVENT_EXIT_GENUINE
      PinkTrace::Trace.syscall pid
      Process.waitpid pid

      event = PinkTrace::Event.decide
      if event == PinkTrace::Event::EVENT_SYSCALL then
        scno = PinkTrace::Syscall.get_no pid
        name = PinkTrace::Syscall.name scno
        if name == 'execve' then
          arg = PinkTrace::Syscall.get_arg pid, 1
          str = PinkTrace::StringArray.decode pid, arg, 1, 10
          assert(str == '/dev/null', "Wrong string, expected: /dev/null got: '#{str}'")
          break
        end
      end
    end

    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end
end
