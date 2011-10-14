#!/usr/bin/env ruby
# coding: utf-8

require 'test/unit'

$:.insert(0, '.libs')
require 'PinkTrace'

class TestPinkBitness < Test::Unit::TestCase
  def test_bitness_get
    pid = fork do
      PinkTrace::Trace.me
      Process.kill 'STOP', Process.pid
    end
    Process.waitpid pid
    bitness = PinkTrace::Bitness.get pid
    assert(bitness == PinkTrace::Bitness::DEFAULT, "Wrong bitness, expected: #{PinkTrace::Bitness::DEFAULT} got: #{bitness}")

    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end
end
