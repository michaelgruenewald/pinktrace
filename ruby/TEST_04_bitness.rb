#!/usr/bin/env ruby
# coding: utf-8
# vim: set sw=2 sts=2 et nowrap fenc=utf-8 :
# Copyright 2010 Ali Polatel <alip@exherbo.org>
# Distributed under the terms of the GNU General Public License v2

require 'test/unit'

$: << File.expand_path('.libs')
require 'PinkTrace'

class TestPinkBitness < Test::Unit::TestCase
  def test_bitness_get_invalid
    assert_raise ArgumentError do
      PinkTrace::Bitness.get
    end
    assert_raise ArgumentError do
      PinkTrace::Bitness.get 0, 1
    end
    assert_raise TypeError do
      PinkTrace::Bitness.get 'pink'
    end
  end

  def test_bitness_get_esrch
    assert_raise Errno::ESRCH do
      PinkTrace::Bitness.get 0
    end
  end

  def test_bitness_get
    pid = PinkTrace.fork {}
    bitness = PinkTrace::Bitness.get pid
    assert(bitness == PinkTrace::Bitness::DEFAULT, "Wrong bitness, expected: #{PinkTrace::Bitness::DEFAULT} got: #{bitness}")

    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end
end
