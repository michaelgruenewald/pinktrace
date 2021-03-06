#!/usr/bin/env ruby
# coding: utf-8

require 'test/unit'

$:.insert(0, '.libs')
require 'PinkTrace'

class TestPinkTrace < Test::Unit::TestCase
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

  def test_trace_resume_basic
    pid = fork do
      PinkTrace::Trace.me
      Process.kill 'STOP', Process.pid
      exit 13
    end

    Process.waitpid pid
    assert $?.stopped?, "Child hasn't stopped"
    assert($?.stopsig == Signal.list['STOP'], "Wrong signal, expected: STOP, got: #{$?.stopsig}")

    PinkTrace::Trace.resume pid
    Process.waitpid pid
    assert $?.exited?, "Child hasn't exited"
    assert($?.exitstatus == 13, "Wrong exit status, expected: 13, got: #{$?.exitstatus}")
  end

  def test_trace_resume_signal
    pid = fork do
      PinkTrace::Trace.me
      Process.kill 'STOP', Process.pid
      exit 13
    end

    Process.waitpid pid
    assert $?.stopped?, "Child hasn't stopped"
    assert($?.stopsig == Signal.list['STOP'], "Wrong signal, expected: STOP, got: #{$?.stopsig}")

    PinkTrace::Trace.resume pid, Signal.list['KILL']
    Process.waitpid pid
    assert $?.signaled?, "Child hasn't been signaled"
    assert($?.termsig == Signal.list['KILL'], "Wrong termsig, expected: KILL, got: #{$?.termsig}")
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

  def test_trace_singlestep_basic
  end

  def test_trace_singlestep_signal
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
