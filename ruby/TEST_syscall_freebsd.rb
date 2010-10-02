#!/usr/bin/env ruby
# coding: utf-8
# vim: set sw=2 sts=2 et nowrap fenc=utf-8 :

require 'test/unit'

$:.insert(0, '.libs')
require 'PinkTrace'

class TestPinkSyscall < Test::Unit::TestCase
  def test_syscall_get_arg_invalid
    assert_raise ArgumentError do
      PinkTrace::Syscall.get_arg
    end
    assert_raise ArgumentError do
      PinkTrace::Syscall.get_arg 0, 1, 2, 3
    end
    assert_raise TypeError do
      PinkTrace::Syscall.get_arg 'pink', 0
    end
    assert_raise TypeError do
      PinkTrace::Syscall.get_arg 0, 'pink'
    end
    assert_raise TypeError do
      PinkTrace::Syscall.get_arg 0, 1, 'pink'
    end
    assert_raise PinkTrace::BitnessError do
      PinkTrace::Syscall.get_arg 0, 1, 13
    end
    assert_raise PinkTrace::IndexError do
      PinkTrace::Syscall.get_arg 0, PinkTrace::Syscall::MAX_INDEX
    end
  end

  def test_syscall_get_arg_eperm
    assert_raise Errno::EPERM do
      PinkTrace::Syscall.get_arg 0, 1
    end
  end

  def test_syscall_get_no
    assert_raise ArgumentError do
      PinkTrace::Syscall.get_no
    end
    assert_raise ArgumentError do
      PinkTrace::Syscall.get_no 0, 1
    end
    assert_raise TypeError do
      PinkTrace::Syscall.get_no 'pink'
    end
  end

  def test_syscall_get_no_eperm
    assert_raise Errno::EPERM do
      PinkTrace::Syscall.get_no 0
    end
  end

  def test_syscall_get_ret
    assert_raise ArgumentError do
      PinkTrace::Syscall.get_ret
    end
    assert_raise ArgumentError do
      PinkTrace::Syscall.get_ret 0, 1
    end
    assert_raise TypeError do
      PinkTrace::Syscall.get_ret 'pink'
    end
  end

  def test_syscall_get_ret_eperm
    assert_raise Errno::EPERM do
      PinkTrace::Syscall.get_ret 0
    end
  end

  def test_syscall_name_invalid
    assert_raise ArgumentError do
      PinkTrace::Syscall.name
    end
    assert_raise ArgumentError do
      PinkTrace::Syscall.name 0, 1, 2
    end
    assert_raise TypeError do
      PinkTrace::Syscall.name 'pink'
    end
    assert_raise TypeError do
      PinkTrace::Syscall.name 0, 'pink'
    end
    assert_raise PinkTrace::BitnessError do
      PinkTrace::Syscall.name 0, 13
    end
  end

  def test_syscall_set_no_invalid
    assert_raise ArgumentError do
      PinkTrace::Syscall.set_no
    end
    assert_raise ArgumentError do
      PinkTrace::Syscall.set_no 0
    end
    assert_raise ArgumentError do
      PinkTrace::Syscall.set_no 0, 1, 2, 3
    end
    assert_raise TypeError do
      PinkTrace::Syscall.set_no 'pink', 1
    end
    assert_raise TypeError do
      PinkTrace::Syscall.set_no 0, 'pink'
    end
    assert_raise PinkTrace::BitnessError do
      PinkTrace::Syscall.set_no 0, 1, 13
    end
  end

  def test_syscall_set_no_eperm
    assert_raise Errno::EPERM do
      PinkTrace::Syscall.set_no 0, 1
    end
  end

  def test_syscall_set_ret_invalid
    assert_raise ArgumentError do
      PinkTrace::Syscall.set_ret
    end
    assert_raise ArgumentError do
      PinkTrace::Syscall.set_ret 0
    end
    assert_raise ArgumentError do
      PinkTrace::Syscall.set_ret 0, 1, 2
    end
    assert_raise TypeError do
      PinkTrace::Syscall.set_ret 'pink', 1
    end
    assert_raise TypeError do
      PinkTrace::Syscall.set_ret 0, 'pink'
    end
  end

  def test_syscall_set_ret_eperm
    assert_raise Errno::EPERM do
      PinkTrace::Syscall.set_ret 0, 1
    end
  end
end

class TestPinkSyscall
  def test_syscall_get_no
    pid = fork do
      PinkTrace::Trace.me
      Process.kill 'STOP', Process.pid

      Process.kill 0, Process.pid
    end
    Process.waitpid pid
    assert $?.stopped?, "#{$?}"
    assert($?.stopsig == Signal.list['STOP'], "#{$?}")

    # Loop until we get to the kill() system call as there's no guarantee that
    # other system calls won't be called beforehand.
    found = false
    loop do
      PinkTrace::Trace.syscall_entry pid
      Process.waitpid pid
      assert $?.stopped?, "#{$?}"
      assert($?.stopsig == Signal.list['TRAP'], "#{$?}")

      scno = PinkTrace::Syscall.get_no pid
      name = PinkTrace::Syscall.name scno
      if name == 'kill' then
        found = true
        break
      end
    end

    assert found

    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_syscall_set_no
    pid = fork do
      PinkTrace::Trace.me
      Process.kill 'STOP', Process.pid

      Process.kill 0, Process.pid
    end
    Process.waitpid pid
    assert $?.stopped?, "#{$?}"
    assert($?.stopsig == Signal.list['STOP'], "#{$?}")

    # Loop until we get to the kill() system call as there's no guarantee that
    # other system calls won't be called beforehand.
    loop do
      PinkTrace::Trace.syscall_entry pid
      Process.waitpid pid
      assert $?.stopped?, "#{$?}"
      assert($?.stopsig == Signal.list['TRAP'], "#{$?}")

      scno = PinkTrace::Syscall.get_no pid
      name = PinkTrace::Syscall.name scno
      if name == 'kill' then
        PinkTrace::Syscall.set_no pid, PinkTrace::Syscall::INVALID
        scno = PinkTrace::Syscall.get_no pid
        assert(scno == PinkTrace::Syscall::INVALID, "#{PinkTrace::Syscall::INVALID} != #{scno}")
        break
      end
    end

    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_syscall_get_ret_success
    pid = fork do
      PinkTrace::Trace.me
      Process.kill 'STOP', Process.pid

      Process.kill 0, Process.pid
    end
    Process.waitpid pid
    assert $?.stopped?, "#{$?}"
    assert($?.stopsig == Signal.list['STOP'], "#{$?}")

    # Loop until we get to the kill() system call as there's no guarantee that
    # other system calls won't be called beforehand.
    stop_at_exit = false
    loop do
      if stop_at_exit then PinkTrace::Trace.syscall_exit pid
      else PinkTrace::Trace.syscall_entry pid end
      Process.waitpid pid
      assert $?.stopped?, "#{$?}"
      assert($?.stopsig == Signal.list['TRAP'], "#{$?}")

      scno = PinkTrace::Syscall.get_no pid
      name = PinkTrace::Syscall.name scno
      if name == 'kill'
        stop_at_exit = true
        next
      elsif stop_at_exit
        ret = PinkTrace::Syscall.get_ret pid
        assert(ret == 0, "#{ret}")
        break
      end
    end

    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_syscall_get_ret_fail
    pid = fork do
      PinkTrace::Trace.me
      Process.kill 'STOP', Process.pid

      begin File.open ''
      rescue Errno::ENOENT ;end
    end
    Process.waitpid pid
    assert $?.stopped?, "#{$?}"
    assert($?.stopsig == Signal.list['STOP'], "#{$?}")

    # Loop until we get to the open() system call as there's no guarantee that
    # other system calls won't be called beforehand.
    stop_at_exit = false
    loop do
      if stop_at_exit then PinkTrace::Trace.syscall_exit pid
      else PinkTrace::Trace.syscall_entry pid end
      Process.waitpid pid
      assert $?.stopped?, "#{$?}"
      assert($?.stopsig == Signal.list['TRAP'], "#{$?}")

      scno = PinkTrace::Syscall.get_no pid
      name = PinkTrace::Syscall.name scno
      if name == 'open'
        stop_at_exit = true
        next
      elsif stop_at_exit
        ret = PinkTrace::Syscall.get_ret pid
        assert(ret == -Errno::ENOENT::Errno, "#{ret}")
        break
      end
    end

    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_syscall_set_ret_success
    pid = fork do
      PinkTrace::Trace.me
      Process.kill 'STOP', Process.pid

      Process.kill 0, Process.pid
    end
    Process.waitpid pid
    assert $?.stopped?, "#{$?}"
    assert($?.stopsig == Signal.list['STOP'], "#{$?}")

    # Loop until we get to the kill() system call as there's no guarantee that
    # other system calls won't be called beforehand.
    stop_at_exit = false
    loop do
      if stop_at_exit then PinkTrace::Trace.syscall_exit pid
      else PinkTrace::Trace.syscall_entry pid end
      Process.waitpid pid
      assert $?.stopped?, "#{$?}"
      assert($?.stopsig == Signal.list['TRAP'], "#{$?}")

      scno = PinkTrace::Syscall.get_no pid
      name = PinkTrace::Syscall.name scno
      if name == 'kill'
        stop_at_exit = true
        next
      elsif stop_at_exit
        PinkTrace::Syscall.set_ret pid, 13
        ret = PinkTrace::Syscall.get_ret pid
        assert(ret == 13, "#{ret}")
        break
      end
    end
  end

  def test_syscall_set_ret_fail
    pid = fork do
      PinkTrace::Trace.me
      Process.kill 'STOP', Process.pid

      Process.kill 0, Process.pid
    end
    Process.waitpid pid
    assert $?.stopped?, "#{$?}"
    assert($?.stopsig == Signal.list['STOP'], "#{$?}")

    # Loop until we get to the kill() system call as there's no guarantee that
    # other system calls won't be called beforehand.
    stop_at_exit = false
    loop do
      if stop_at_exit then PinkTrace::Trace.syscall_exit pid
      else PinkTrace::Trace.syscall_entry pid end
      Process.waitpid pid
      assert $?.stopped?, "#{$?}"
      assert($?.stopsig == Signal.list['TRAP'], "#{$?}")

      scno = PinkTrace::Syscall.get_no pid
      name = PinkTrace::Syscall.name scno
      if name == 'kill'
        stop_at_exit = true
        next
      elsif stop_at_exit
        PinkTrace::Syscall.set_ret pid, -Errno::ENAMETOOLONG::Errno
        ret = PinkTrace::Syscall.get_ret pid
        assert(ret == -Errno::ENAMETOOLONG::Errno, "#{ret}")
        break
      end
    end

    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_syscall_get_arg_first
  end

  def test_syscall_get_arg_second
  end

  def test_syscall_get_arg_third
  end

  def test_syscall_get_arg_fourth
  end

  def test_syscall_get_arg_fifth
  end

  def test_syscall_get_arg_sixth
  end
end
