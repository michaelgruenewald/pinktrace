#!/usr/bin/env ruby
# coding: utf-8
# vim: set sw=2 sts=2 et nowrap fenc=utf-8 :

require 'test/unit'

$:.insert(0, '.libs')
require 'PinkTrace'

class TestPinkFork < Test::Unit::TestCase
  def test_fork
    pid = PinkTrace::Fork.fork do
      exit 13
    end

    PinkTrace::Trace.cont pid
    Process.waitpid pid
    assert($?.exitstatus == 13, "Wrong exit status, expected: 13 got: #{$?.exitstatus}")
  end
end
