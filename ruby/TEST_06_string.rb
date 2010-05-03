#!/usr/bin/env ruby
# coding: utf-8
# vim: set sw=2 sts=2 et nowrap fenc=utf-8 :
# Copyright 2010 Ali Polatel <alip@exherbo.org>
# Distributed under the terms of the GNU General Public License v2

require 'test/unit'

$: << File.expand_path('.libs')
require 'PinkTrace'

class TestPinkString < Test::Unit::TestCase
  def test_string_decode_invalid
    assert_raise ArgumentError do
      PinkTrace::String.decode
    end
    assert_raise ArgumentError do
      PinkTrace::String.decode 0
    end
    assert_raise ArgumentError do
      PinkTrace::String.decode 0, 1, 2, 3, 4
    end
    assert_raise TypeError do
      PinkTrace::String.decode 'pink', 0
    end
    assert_raise TypeError do
      PinkTrace::String.decode 0, 'pink'
    end
    assert_raise TypeError do
      PinkTrace::String.decode 0, 1, 'pink'
    end
    assert_raise TypeError do
      PinkTrace::String.decode 0, 1, 2, 'pink'
    end
    assert_raise PinkTrace::BitnessError do
      PinkTrace::String.decode 0, 1, 2, 13
    end
    assert_raise PinkTrace::IndexError do
      PinkTrace::String.decode 0, PinkTrace::MAX_INDEX
    end
  end

  def test_string_decode_esrch
    assert_raise Errno::ESRCH do
      PinkTrace::String.decode 0, 1
    end
  end

  def test_string_encode_invalid
    assert_raise ArgumentError do
      PinkTrace::String.encode
    end
    assert_raise ArgumentError do
      PinkTrace::String.encode 0
    end
    assert_raise ArgumentError do
      PinkTrace::String.encode 0, 1
    end
    assert_raise ArgumentError do
      PinkTrace::String.encode 0, 1, 2, 3, 4
    end
    assert_raise TypeError do
      PinkTrace::String.encode 'pink', 1, 'floyd'
    end
    assert_raise TypeError do
      PinkTrace::String.encode 0, 'pink', 'floyd'
    end
    assert_raise TypeError do
      PinkTrace::String.encode 0, 1, Object
    end
    assert_raise PinkTrace::BitnessError do
      PinkTrace::String.encode 0, 1, 'pink', 13
    end
    assert_raise PinkTrace::IndexError do
      PinkTrace::String.encode 0, PinkTrace::MAX_INDEX, 'pink'
    end
  end

  def test_string_encode_esrch
    assert_raise Errno::ESRCH do
      PinkTrace::String.encode 0, 1, 'pink'
    end
  end

  def test_string_encode_unsafe_invalid
    assert_raise ArgumentError do
      PinkTrace::String.encode!
    end
    assert_raise ArgumentError do
      PinkTrace::String.encode! 0
    end
    assert_raise ArgumentError do
      PinkTrace::String.encode! 0, 1
    end
    assert_raise ArgumentError do
      PinkTrace::String.encode! 0, 1, 2, 3, 4
    end
    assert_raise TypeError do
      PinkTrace::String.encode! 'pink', 1, 'floyd'
    end
    assert_raise TypeError do
      PinkTrace::String.encode! 0, 'pink', 'floyd'
    end
    assert_raise TypeError do
      PinkTrace::String.encode! 0, 1, Object
    end
    assert_raise PinkTrace::BitnessError do
      PinkTrace::String.encode! 0, 1, 'pink', 13
    end
    assert_raise PinkTrace::IndexError do
      PinkTrace::String.encode! 0, PinkTrace::MAX_INDEX, 'pink'
    end
  end

  def test_string_encode_unsafe_esrch
    assert_raise Errno::ESRCH do
      PinkTrace::String.encode! 0, 1, 'pink'
    end
  end
end

# These test cases depend on generated system call names.
# Don't run them if they weren't generated.
unless PinkTrace::SysCall.name 0
  exit 0
end

class TestPinkString
  def test_string_decode
    pid = PinkTrace::Fork.fork do
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
          assert(s == '/dev/null', "Wrong string, expected: /dev/null got: '#{s}'")
          break
        end
      end
    end

    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_string_decode_max
    pid = PinkTrace::Fork.fork do
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
          s = PinkTrace::String.decode pid, 0, 10
          assert(s == '/dev/null', "Wrong string, expected: /dev/null got: '#{s}'")
          break
        end
      end
    end

    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_string_encode
    pid = PinkTrace::Fork.fork do
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
    assert($?.exitstatus == 0, "Wrong exit status, expected: 0 got: #{$?.exitstatus}")

    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end
end
