#!/usr/bin/env ruby
# coding: utf-8
# vim: set sw=2 sts=2 et nowrap fenc=utf-8 :
# Copyright 2010 Ali Polatel <alip@exherbo.org>
# Distributed under the terms of the GNU General Public License v2

require 'test/unit'

$: << File.expand_path('.libs')
require 'PinkTrace'

# These test cases depend on generated system call names.
# Don't run them if they weren't generated.
unless PinkTrace::SysCall.name 0
  exit 0
end

class TestPinkString < Test::Unit::TestCase
  def test_string_decode
    pid = PinkTrace.fork do
      File.open '/dev/null'
    end

    # Loop until we get to the open() system call as there's no guarantee that
    # other system calls won't be called beforehand.
    event = -1
    while event != PinkTrace::Event::EXIT_GENUINE
      PinkTrace::Trace.syscall pid
      Process.waitpid pid

      event = PinkTrace::Event.decide
      if event == PinkTrace::Event::SYSCALL then
        scno = PinkTrace::SysCall.get_no pid
        name = PinkTrace::SysCall.name scno
        if name == 'open' then
          s = PinkTrace::String.decode pid, 0
          assert(s == '/dev/null', 'Wrong string, expected: /dev/null got: "' + s + '"')
          break
        end
      end
    end

    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_string_decode_max
    pid = PinkTrace.fork do
      File.open '/dev/null'
    end

    # Loop until we get to the open() system call as there's no guarantee that
    # other system calls won't be called beforehand.
    event = -1
    while event != PinkTrace::Event::EXIT_GENUINE
      PinkTrace::Trace.syscall pid
      Process.waitpid pid

      event = PinkTrace::Event.decide
      if event == PinkTrace::Event::SYSCALL then
        scno = PinkTrace::SysCall.get_no pid
        name = PinkTrace::SysCall.name scno
        if name == 'open' then
          s = PinkTrace::String.decode pid, 0, 2
          assert(s == '/d', 'Wrong string, expected: /de got: "' + s + '"')
          break
        end
      end
    end

    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_string_encode
    pid = PinkTrace.fork do
      begin
        File.open '/dev/null'
      rescue Errno::ENOENT
        exit 0
      end
      exit 1
    end

    # Loop until we get to the open() system call as there's no guarantee that
    # other system calls won't be called beforehand.
    event = -1
    while event != PinkTrace::Event::EXIT_GENUINE
      PinkTrace::Trace.syscall pid
      Process.waitpid pid

      event = PinkTrace::Event.decide
      if event == PinkTrace::Event::SYSCALL then
        scno = PinkTrace::SysCall.get_no pid
        name = PinkTrace::SysCall.name scno
        if name == 'open' then
          PinkTrace::String.encode pid, 0, '/dev/NULL'
        end
      end
    end

    assert $?.exited?, "Child hasn't exited!"
    assert($?.exitstatus == 0, 'Wrong exit status, expected: 0 got: ' + $?.exitstatus.to_s)

    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end
end
