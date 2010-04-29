#!/usr/bin/env ruby
# coding: utf-8
# vim: set sw=2 sts=2 et nowrap fenc=utf-8 :
# Copyright 2010 Ali Polatel <alip@exherbo.org>
# Distributed under the terms of the GNU General Public License v2

require 'test/unit'

$: << File.expand_path('.libs')
require 'PinkTrace'

class TestPinkFork < Test::Unit::TestCase
  def test_fork
    pid = PinkTrace.fork do
      exit 13
    end
    PinkTrace::Trace.cont pid
    Process.waitpid pid
    assert($?.exitstatus == 13, 'Wrong exit status, expected: 13 got: ' + $?.exitstatus.to_s)
  end
end
