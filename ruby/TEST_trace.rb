#!/usr/bin/env ruby
# coding: utf-8
# vim: set sw=2 sts=2 et nowrap fenc=utf-8 :

require 'test/unit'

$:.insert(0, '.libs')
require 'PinkTrace'

class TestPinkTrace < Test::Unit::TestCase
  def test_trace_me_invalid
    assert_raise ArgumentError do
      PinkTrace::Trace.me 0
    end
  end

  #def test_trace_me_twice
  #  assert_raise Errno::EPERM do
  #    PinkTrace::Trace.me
  #    # The second call should fail with EPERM.
  #    PinkTrace::Trace.me
  #  end
  #end

  def test_trace_me
    pid = fork do
      PinkTrace::Trace.me
    end

    Process.waitpid pid
    assert($?.exitstatus == 0, "Wrong exit status #{$?.exitstatus}")
  end

  def test_trace_signal
    pid = fork do
      PinkTrace::Trace.me
      Process.kill 'TTIN', Process.pid
    end

    Process.waitpid pid
    assert $?.stopped?, "Child hasn't been stopped"
    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_trace_execve
    pid = fork do
      PinkTrace::Trace.me
      exec "true"
    end

    Process.waitpid pid
    assert $?.stopped?, "Child hasn't been stopped"
    assert($?.stopsig == Signal.list["TRAP"], "Wrong signal, expected: TRAP, got #{$?.stopsig}")

    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_cont_invalid
    assert_raise TypeError do
      PinkTrace::Trace.cont 'pink', 0
    end
    assert_raise TypeError do
      PinkTrace::Trace.cont 0, 'pink'
    end
    assert_raise ArgumentError do
      PinkTrace::Trace.cont 1, 2, 3, 4
    end
  end

  def test_trace_cont_basic
    pid = fork do
      PinkTrace::Trace.me
      Process.kill 'STOP', Process.pid
      exit 13
    end

    Process.waitpid pid
    assert $?.stopped?, "Child hasn't stopped"
    assert($?.stopsig == Signal.list['STOP'], "Wrong signal, expected: STOP, got: #{$?.stopsig}")

    PinkTrace::Trace.cont pid
    Process.waitpid pid
    assert $?.exited?, "Child hasn't exited"
    assert($?.exitstatus == 13, "Wrong exit status, expected: 13, got: #{$?.exitstatus}")
  end

  def test_trace_cont_signal
    pid = fork do
      PinkTrace::Trace.me
      Process.kill 'STOP', Process.pid
      exit 13
    end

    Process.waitpid pid
    assert $?.stopped?, "Child hasn't stopped"
    assert($?.stopsig == Signal.list['STOP'], "Wrong signal, expected: STOP, got: #{$?.stopsig}")

    PinkTrace::Trace.cont pid, Signal.list['KILL']
    Process.waitpid pid
    assert $?.signaled?, "Child hasn't been signaled"
    assert($?.termsig == Signal.list['KILL'], "Wrong termsig, expected: KILL, got: #{$?.termsig}")
  end

  def test_trace_kill_invalid
    assert_raise ArgumentError do
      PinkTrace::Trace.kill
    end
    assert_raise ArgumentError do
      PinkTrace::Trace.kill 0, 1
    end
    assert_raise TypeError do
      PinkTrace::Trace.kill 'pink'
    end
  end

  def test_trace_kill
    pid = fork do
      PinkTrace::Trace.me
      Process.kill 'STOP', Process.pid
      exit 13
    end

    Process.waitpid pid
    assert $?.stopped?, "Child hasn't stopped"
    assert($?.stopsig == Signal.list['STOP'], "Wrong signal, expected: STOP, got: #{$?.stopsig}")

    PinkTrace::Trace.kill pid
    Process.waitpid pid
    assert $?.signaled?, "Child hasn't been signaled"
    assert($?.termsig == Signal.list['KILL'], "Wrong termsig, expected: KILL, got: #{$?.termsig}")
  end

  def test_trace_attach_invalid
    assert_raise ArgumentError do
      PinkTrace::Trace.attach
    end
    assert_raise ArgumentError do
      PinkTrace::Trace.attach 0, 1
    end
    assert_raise TypeError do
      PinkTrace::Trace.attach 'pink'
    end
  end

  def test_trace_detach_invalid
    assert_raise ArgumentError do
      PinkTrace::Trace.detach
    end
    assert_raise ArgumentError do
      PinkTrace::Trace.detach 0, 1, 2
    end
    assert_raise TypeError do
      PinkTrace::Trace.detach 'pink'
    end
    assert_raise TypeError do
      PinkTrace::Trace.detach 0, 'pink'
    end
  end

=begin
  def test_trace_geteventmsg_invalid
    assert_raise ArgumentError do
      PinkTrace::Trace.geteventmsg
    end
    assert_raise ArgumentError do
      PinkTrace::Trace.geteventmsg 0, 1
    end
    assert_raise TypeError do
      PinkTrace::Trace.geteventmsg 'pink'
    end
  end

  def test_trace_setup_invalid
    assert_raise ArgumentError do
      PinkTrace::Trace.setup
    end
    assert_raise ArgumentError do
      PinkTrace::Trace.setup 0, 1, 2
    end
    assert_raise TypeError do
      PinkTrace::Trace.setup 'pink'
    end
    assert_raise TypeError do
      PinkTrace::Trace.setup 0, 'pink'
    end
  end
=end

  def test_trace_singlestep_invalid
    assert_raise ArgumentError do
      PinkTrace::Trace.singlestep
    end
    assert_raise ArgumentError do
      PinkTrace::Trace.singlestep 0, 1, 2
    end
    assert_raise TypeError do
      PinkTrace::Trace.singlestep 'pink'
    end
    assert_raise TypeError do
      PinkTrace::Trace.singlestep 0, 'pink'
    end
  end

  def test_trace_singlestep_basic
  end

  def test_trace_singlestep_signal
  end

  def test_trace_syscall_invalid
    assert_raise ArgumentError do
      PinkTrace::Trace.syscall
    end
    assert_raise ArgumentError do
      PinkTrace::Trace.syscall 0, 1, 2
    end
    assert_raise TypeError do
      PinkTrace::Trace.syscall 'pink'
    end
    assert_raise TypeError do
      PinkTrace::Trace.syscall 0, 'pink'
    end
  end

  def test_trace_syscall_basic
  end

  def test_trace_syscall_signal
  end

  def test_trace_syscall_sysgood
  end

  def test_trace_syscall_fork
  end

  def test_trace_syscall_vfork
  end

  def test_trace_syscall_clone
  end

  def test_trace_syscall_vfork_done
  end

  def test_trace_syscall_exit
  end
end
