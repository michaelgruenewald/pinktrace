#!/usr/bin/env ruby
# coding: utf-8
# vim: set sw=2 sts=2 et nowrap fenc=utf-8 :

require 'test/unit'

$:.insert(0, '.libs')
require 'PinkTrace'

class TestPinkEvent < Test::Unit::TestCase
  def test_event_unknown
    event = PinkTrace::Event.decide -1
    assert(event == PinkTrace::Event::EVENT_UNKNOWN, "Wrong event, expected: UNKNOWN got: #{event}")
  end

  def test_event_stop
    pid = fork do
      PinkTrace::Trace.me
      Process.kill 'STOP', Process.pid
    end
    Process.waitpid pid
    event = PinkTrace::Event.decide
    assert(event == PinkTrace::Event::EVENT_STOP, "Wrong event, expected: STOP got: #{event}")

    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_event_syscall
    pid = fork do
      PinkTrace::Trace.me
      Process.kill 'STOP', Process.pid

      sleep 1
    end
    Process.waitpid pid
    PinkTrace::Trace.setup pid

    PinkTrace::Trace.syscall pid
    Process.waitpid pid
    event = PinkTrace::Event.decide
    assert(event == PinkTrace::Event::EVENT_SYSCALL, "Wrong event, expected: SYSCALL got: #{event}")

    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_event_fork
    pid = fork do
      PinkTrace::Trace.me
      Process.kill 'STOP', Process.pid

      fork { exit 0 }
    end
    Process.waitpid pid
    PinkTrace::Trace.setup pid, (PinkTrace::Trace::OPTION_SYSGOOD | PinkTrace::Trace::OPTION_FORK)

    PinkTrace::Trace.cont pid
    Process.waitpid pid
    event = PinkTrace::Event.decide
    assert(event == PinkTrace::Event::EVENT_FORK, "Wrong event, expected: FORK got: #{event}")

    begin
      child = PinkTrace::Trace.geteventmsg pid
      PinkTrace::Trace.kill child
      PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_event_vfork
  end

  def test_event_clone
  end

  def test_event_vfork_done
  end

  def test_event_exec
    pid = fork do
      PinkTrace::Trace.me
      Process.kill 'STOP', Process.pid

      exec '/bin/true'
    end
    Process.waitpid pid
    PinkTrace::Trace.setup pid, (PinkTrace::Trace::OPTION_SYSGOOD | PinkTrace::Trace::OPTION_EXEC)

    PinkTrace::Trace.cont pid
    Process.waitpid pid
    event = PinkTrace::Event.decide
    assert(event == PinkTrace::Event::EVENT_EXEC, "Wrong event, expected: EXEC got: #{event}")

    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_event_exit
    pid = fork do
      PinkTrace::Trace.me
      Process.kill 'STOP', Process.pid

      exit 0
    end
    Process.waitpid pid
    PinkTrace::Trace.setup pid, (PinkTrace::Trace::OPTION_SYSGOOD | PinkTrace::Trace::OPTION_EXIT)

    PinkTrace::Trace.cont pid
    Process.waitpid pid
    event = PinkTrace::Event.decide
    assert(event == PinkTrace::Event::EVENT_EXIT, "Wrong event, expected: EXIT got: #{event}")

    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_event_genuine
    pid = fork do
      PinkTrace::Trace.me
      Process.kill 'STOP', Process.pid

      Process.kill 'TSTP', Process.pid
    end
    Process.waitpid pid
    PinkTrace::Trace.setup pid

    PinkTrace::Trace.cont pid
    Process.waitpid pid
    event = PinkTrace::Event.decide
    assert(event == PinkTrace::Event::EVENT_GENUINE, "Wrong event, expected: GENUINE got: #{event}")

    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_event_exit_genuine
    pid = fork do
      PinkTrace::Trace.me
      Process.kill 'STOP', Process.pid

      exit 0
    end
    Process.waitpid pid
    PinkTrace::Trace.setup pid

    PinkTrace::Trace.cont pid
    Process.waitpid pid
    event = PinkTrace::Event.decide
    assert(event == PinkTrace::Event::EVENT_EXIT_GENUINE, "Wrong event, expected: EXIT_GENUINE got: #{event}")
    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_event_exit_signal
    pid = fork do
      PinkTrace::Trace.me
      Process.kill 'STOP', Process.pid

      Process.kill 'KILL', Process.pid
    end
    Process.waitpid pid
    PinkTrace::Trace.setup pid

    PinkTrace::Trace.cont pid
    Process.waitpid pid
    event = PinkTrace::Event.decide
    assert(event == PinkTrace::Event::EVENT_EXIT_SIGNAL, "Wrong event, expected: EXIT_SIGNAL got: #{event}")
    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end
end
