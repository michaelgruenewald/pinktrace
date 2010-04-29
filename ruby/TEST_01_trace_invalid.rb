#!/usr/bin/env ruby
# coding: utf-8
# vim: set sw=2 sts=2 et nowrap fenc=utf-8 :
# Copyright 2010 Ali Polatel <alip@exherbo.org>
# Distributed under the terms of the GNU General Public License v2

require 'test/unit'

$: << File.expand_path('.libs')
require 'PinkTrace'
include PinkTrace

class TestTraceMe < Test::Unit::TestCase
  def test_trace_me_twice
    assert_raise Errno::EPERM do
      Trace.me
      # The second call should fail with EPERM.
      Trace.me
    end
  end
end

class TestTraceCont < Test::Unit::TestCase
  def test_cont_invalid
    assert_raise TypeError do
      Trace.cont 'pink', 0
    end
    assert_raise TypeError do
      Trace.cont 0, 'pink'
    end
    assert_raise ArgumentError do
      Trace.cont 1, 2, 3
    end
  end

  def test_cont_esrch
    assert_raise Errno::ESRCH do
      Trace.cont 0, Signal.list['CONT']
    end
  end
end

class TestTraceKill < Test::Unit::TestCase
  def test_trace_kill_invalid
    assert_raise TypeError do
      Trace.kill 'pink'
    end
  end

  def test_trace_kill_esrch
    assert_raise Errno::ESRCH do
      Trace.kill 0
    end
  end
end
