#!/usr/bin/env ruby
# coding: utf-8
# vim: set sw=2 sts=2 et nowrap fenc=utf-8 :
# Copyright 2010 Ali Polatel <alip@exherbo.org>
# Distributed under the terms of the GNU General Public License v2

require 'test/unit'

$: << File.expand_path('.libs')
require 'PinkTrace'

class TestPinkBitness < Test::Unit::TestCase
  def test_bitness_get
    pid = PinkTrace.fork {}
    bitness = PinkTrace::Bitness.get pid
    assert(bitness == PinkTrace::Bitness::DEFAULT, 'Wrong bitness, expected: ' + PinkTrace::Bitness::DEFAULT.to_s + ' got: ' + bitness.to_s)

    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end
end
