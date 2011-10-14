#!/usr/bin/env ruby
# coding: utf-8

require 'test/unit'

$:.insert(0, '.libs')
require 'PinkTrace'

class TestPinkString < Test::Unit::TestCase
  def test_string_decode_invalid
    assert_raise PinkTrace::BitnessError do
      PinkTrace::String.decode 0, 1, 2, 13
    end
    assert_raise PinkTrace::IndexError do
      PinkTrace::String.decode 0, PinkTrace::Syscall::MAX_INDEX
    end
  end

  def test_string_decode_eperm
    assert_raise Errno::EPERM do
      PinkTrace::String.decode 0, 1
    end
  end

  def test_string_encode_unsafe_invalid
    assert_raise PinkTrace::BitnessError do
      PinkTrace::String.encode! 0, 1, 'pink', 13
    end
    assert_raise PinkTrace::IndexError do
      PinkTrace::String.encode! 0, PinkTrace::Syscall::MAX_INDEX, 'pink'
    end
  end

  def test_string_encode_unsafe_eperm
    assert_raise Errno::EPERM do
      PinkTrace::String.encode! 0, 1, 'pink'
    end
  end
end

class TestPinkString
  def test_string_decode
    pid = fork do
      PinkTrace::Trace.me
      Process.kill 'STOP', Process.pid

      File.open '/dev/null'
    end
    Process.waitpid pid
    assert $?.stopped?, "#{$?}"
    assert($?.stopsig == Signal.list['STOP'], "#{$?}")

    # Loop until we get to the open() system call as there's no guarantee that
    # other system calls won't be called beforehand.
    loop do
      PinkTrace::Trace.syscall_entry pid
      Process.waitpid pid
      assert $?.stopped?, "#{$?}"
      assert($?.stopsig == Signal.list['TRAP'], "#{$?}")

      scno = PinkTrace::Syscall.get_no pid
      name = PinkTrace::Syscall.name scno
      if name == 'open'
        path = PinkTrace::String.decode pid, 0
        assert(path == '/dev/null', "#{path}")
        break
      end
    end

    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_string_decode_max
    pid = fork do
      PinkTrace::Trace.me
      Process.kill 'STOP', Process.pid

      File.open '/dev/null'
    end
    Process.waitpid pid
    assert $?.stopped?, "#{$?}"
    assert($?.stopsig == Signal.list['STOP'], "#{$?}")

    # Loop until we get to the open() system call as there's no guarantee that
    # other system calls won't be called beforehand.
    loop do
      PinkTrace::Trace.syscall_entry pid
      Process.waitpid pid
      assert $?.stopped?, "#{$?}"
      assert($?.stopsig == Signal.list['TRAP'], "#{$?}")

      scno = PinkTrace::Syscall.get_no pid
      name = PinkTrace::Syscall.name scno
      if name == 'open'
        path = PinkTrace::String.decode pid, 0, 10
        assert(path == '/dev/null', "#{path}")
        break
      end
    end

    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_string_encode_unsafe
    pid = fork do
      PinkTrace::Trace.me
      Process.kill 'STOP', Process.pid

      File.open '/dev/null'
    end
    Process.waitpid pid
    assert $?.stopped?, "#{$?}"
    assert($?.stopsig == Signal.list['STOP'], "#{$?}")

    # Loop until we get to the open() system call as there's no guarantee that
    # other system calls won't be called beforehand.
    loop do
      PinkTrace::Trace.syscall_entry pid
      Process.waitpid pid
      assert $?.stopped?, "#{$?}"
      assert($?.stopsig == Signal.list['TRAP'], "#{$?}")

      scno = PinkTrace::Syscall.get_no pid
      name = PinkTrace::Syscall.name scno
      if name == 'open'
        PinkTrace::String.encode! pid, 0, '/dev/NULL'
        path = PinkTrace::String.decode pid, 0
        assert(path == '/dev/NULL', "#{path}")
        break
      end
    end

    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end
end
