#!/usr/bin/env ruby
# coding: utf-8
# vim: set sw=2 sts=2 et nowrap fenc=utf-8 :

require 'test/unit'

$:.insert(0, '.libs')
require 'PinkTrace'

class TestPinkEvent < Test::Unit::TestCase
  def test_event_decide_invalid
    assert_raise ArgumentError do
      PinkTrace::Event.decide 0, 1
    end
    assert_raise TypeError do
      PinkTrace::Event.decide 'pink'
    end
  end

  def test_event_unknown
    event = PinkTrace::Event.decide -1
    assert(event == PinkTrace::Event::EVENT_UNKNOWN, "Wrong event, expected: UNKNOWN got: #{event}")
  end

  def test_event_stop
    pid = PinkTrace::Fork.fork do
      Process.kill Signal.list['STOP'], Process.pid
    end
    PinkTrace::Trace.cont pid
    Process.waitpid pid
    event = PinkTrace::Event.decide
    assert(event == PinkTrace::Event::EVENT_STOP, "Wrong event, expected: STOP got: #{event}")
    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_event_syscall
    pid = PinkTrace::Fork.fork do
      sleep 1
    end
    PinkTrace::Trace.syscall pid
    Process.waitpid pid
    event = PinkTrace::Event.decide
    assert(event == PinkTrace::Event::EVENT_SYSCALL, "Wrong event, expected: SYSCALL got: #{event}")
    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_event_fork
    pid = PinkTrace::Fork.fork(PinkTrace::Trace::OPTION_FORK) do
      fork { exit 0 }
    end
    PinkTrace::Trace.cont pid
    Process.waitpid pid
    event = PinkTrace::Event.decide
    assert(event == PinkTrace::Event::EVENT_FORK, "Wrong event, expected: FORK got: #{event}")
    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_event_vfork
  end

  def test_event_clone
  end

  def test_event_vfork_done
  end

  def test_event_exec
    pid = PinkTrace::Fork.fork(PinkTrace::Trace::OPTION_EXEC) do
      exec '/bin/true'
    end
    PinkTrace::Trace.cont pid
    Process.waitpid pid
    event = PinkTrace::Event.decide
    assert(event == PinkTrace::Event::EVENT_EXEC, "Wrong event, expected: EXEC got: #{event}")
    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_event_exit
    pid = PinkTrace::Fork.fork(PinkTrace::Trace::OPTION_EXIT) do
      exit 0
    end
    PinkTrace::Trace.cont pid
    Process.waitpid pid
    event = PinkTrace::Event.decide
    assert(event == PinkTrace::Event::EVENT_EXIT, "Wrong event, expected: EXIT got: #{event}")
    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_event_genuine
    pid = PinkTrace::Fork.fork do
      Process.kill Signal.list['INT'], Process.pid
    end
    PinkTrace::Trace.cont pid
    Process.waitpid pid
    event = PinkTrace::Event.decide
    assert(event == PinkTrace::Event::EVENT_GENUINE, "Wrong event, expected: GENUINE got: #{event}")
    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_event_exit_genuine
    pid = PinkTrace::Fork.fork do
      exit 0
    end
    PinkTrace::Trace.cont pid
    Process.waitpid pid
    event = PinkTrace::Event.decide
    assert(event == PinkTrace::Event::EVENT_EXIT_GENUINE, "Wrong event, expected: EXIT_GENUINE got: #{event}")
    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_event_exit_signal
    pid = PinkTrace::Fork.fork do
      Process.kill Signal.list['KILL'], Process.pid
    end
    PinkTrace::Trace.cont pid
    Process.waitpid pid
    event = PinkTrace::Event.decide
    assert(event == PinkTrace::Event::EVENT_EXIT_SIGNAL, "Wrong event, expected: EXIT_SIGNAL got: #{event}")
    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end
end
