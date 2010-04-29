#!/usr/bin/env ruby
# coding: utf-8
# vim: set sw=2 sts=2 et nowrap fenc=utf-8 :
# Copyright 2010 Ali Polatel <alip@exherbo.org>
# Distributed under the terms of the GNU General Public License v2

require 'test/unit'

$: << File.expand_path('.libs')
require 'PinkTrace'

class TestPinkTrace < Test::Unit::TestCase
  def test_trace_me
    pid = fork do
      PinkTrace::Trace.me
    end

    Process.waitpid pid
    assert($?.exitstatus == 0, 'Wrong exit status ' + $?.exitstatus.to_s)
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
      exec "/bin/true"
    end

    Process.waitpid pid
    assert $?.stopped?, "Child hasn't been stopped"
    assert($?.stopsig == Signal.list["TRAP"], 'Wrong signal, expected: TRAP, got ' + $?.stopsig.to_s)

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
    assert($?.stopsig == Signal.list['STOP'], "Wrong signal, expected: STOP, got: " + $?.stopsig.to_s)

    PinkTrace::Trace.cont pid
    Process.waitpid pid
    assert $?.exited?, "Child hasn't exited"
    assert($?.exitstatus == 13, 'Wrong exit status, expected: 13, got: ' + $?.exitstatus.to_s)
  end

  def test_trace_cont_signal
    pid = fork do
      PinkTrace::Trace.me
      Process.kill 'STOP', Process.pid
      exit 13
    end

    Process.waitpid pid
    assert $?.stopped?, "Child hasn't stopped"
    assert($?.stopsig == Signal.list['STOP'], "Wrong signal, expected: STOP, got: " + $?.stopsig.to_s)

    PinkTrace::Trace.cont pid, Signal.list['KILL']
    Process.waitpid pid
    assert $?.signaled?, "Child hasn't been signaled"
    assert($?.termsig == Signal.list['KILL'], "Wrong termsig, expected: KILL, got: " + $?.termsig.to_s)
  end

  def test_trace_kill
    pid = fork do
      PinkTrace::Trace.me
      Process.kill 'STOP', Process.pid
      exit 13
    end

    Process.waitpid pid
    assert $?.stopped?, "Child hasn't stopped"
    assert($?.stopsig == Signal.list['STOP'], "Wrong signal, expected: STOP, got: " + $?.stopsig.to_s)

    PinkTrace::Trace.kill pid
    Process.waitpid pid
    assert $?.signaled?, "Child hasn't been signaled"
    assert($?.termsig == Signal.list['KILL'], "Wrong termsig, expected: KILL, got: " + $?.termsig.to_s)
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
