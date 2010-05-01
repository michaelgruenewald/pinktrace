#!/usr/bin/env ruby
# coding: utf-8
# vim: set sw=2 sts=2 et nowrap fenc=utf-8 :
# Copyright 2010 Ali Polatel <alip@exherbo.org>
# Distributed under the terms of the GNU General Public License v2

require 'test/unit'

$: << File.expand_path('.libs')
require 'PinkTrace'

class TestPinkSyscall < Test::Unit::TestCase
  def test_syscall_get_arg_invalid
    assert_raise ArgumentError do
      PinkTrace::SysCall.get_arg
    end
    assert_raise ArgumentError do
      PinkTrace::SysCall.get_arg 0, 1, 2, 3
    end
    assert_raise TypeError do
      PinkTrace::SysCall.get_arg 'pink', 0
    end
    assert_raise TypeError do
      PinkTrace::SysCall.get_arg 0, 'pink'
    end
    assert_raise TypeError do
      PinkTrace::SysCall.get_arg 0, 1, 'pink'
    end
    assert_raise PinkTrace::BitnessError do
      PinkTrace::SysCall.get_arg 0, 1, 13
    end
    assert_raise PinkTrace::IndexError do
      PinkTrace::SysCall.get_arg 0, PinkTrace::MAX_INDEX
    end
  end

  def test_syscall_get_arg_esrch
    assert_raise Errno::ESRCH do
      PinkTrace::SysCall.get_arg 0, 1
    end
  end

  def test_syscall_get_no
    assert_raise ArgumentError do
      PinkTrace::SysCall.get_no
    end
    assert_raise ArgumentError do
      PinkTrace::SysCall.get_no 0, 1
    end
    assert_raise TypeError do
      PinkTrace::SysCall.get_no 'pink'
    end
  end

  def test_syscall_get_no_esrch
    assert_raise Errno::ESRCH do
      PinkTrace::SysCall.get_no 0
    end
  end

  def test_syscall_get_ret
    assert_raise ArgumentError do
      PinkTrace::SysCall.get_ret
    end
    assert_raise ArgumentError do
      PinkTrace::SysCall.get_ret 0, 1
    end
    assert_raise TypeError do
      PinkTrace::SysCall.get_ret 'pink'
    end
  end

  def test_syscall_get_ret_esrch
    assert_raise Errno::ESRCH do
      PinkTrace::SysCall.get_ret 0
    end
  end

  def test_syscall_name_invalid
    assert_raise ArgumentError do
      PinkTrace::SysCall.name
    end
    assert_raise ArgumentError do
      PinkTrace::SysCall.name 0, 1, 2
    end
    assert_raise TypeError do
      PinkTrace::SysCall.name 'pink'
    end
    assert_raise TypeError do
      PinkTrace::SysCall.name 0, 'pink'
    end
    assert_raise PinkTrace::BitnessError do
      PinkTrace::SysCall.name 0, 13
    end
  end

  def test_syscall_set_no_invalid
    assert_raise ArgumentError do
      PinkTrace::SysCall.set_no
    end
    assert_raise ArgumentError do
      PinkTrace::SysCall.set_no 0
    end
    assert_raise ArgumentError do
      PinkTrace::SysCall.set_no 0, 1, 2
    end
    assert_raise TypeError do
      PinkTrace::SysCall.set_no 'pink', 1
    end
    assert_raise TypeError do
      PinkTrace::SysCall.set_no 0, 'pink'
    end
  end

  def test_syscall_set_no_esrch
    assert_raise Errno::ESRCH do
      PinkTrace::SysCall.set_no 0, 1
    end
  end

  def test_syscall_set_ret_invalid
    assert_raise ArgumentError do
      PinkTrace::SysCall.set_ret
    end
    assert_raise ArgumentError do
      PinkTrace::SysCall.set_ret 0
    end
    assert_raise ArgumentError do
      PinkTrace::SysCall.set_ret 0, 1, 2
    end
    assert_raise TypeError do
      PinkTrace::SysCall.set_ret 'pink', 1
    end
    assert_raise TypeError do
      PinkTrace::SysCall.set_ret 0, 'pink'
    end
  end

  def test_syscall_set_ret_esrch
    assert_raise Errno::ESRCH do
      PinkTrace::SysCall.set_ret 0, 1
    end
  end
end

# These test cases depend on generated system call names.
# Don't run them if they weren't generated.
unless PinkTrace::SysCall.name 0
  exit 0
end

class TestPinkSysCall
  def test_syscall_get_no
    pid = PinkTrace.fork do
      Process.kill 0, Process.pid
    end

    # Loop until we get to the kill() system call as there's no guarantee that
    # other system calls won't be called beforehand.
    event = -1
    found = false
    while event != PinkTrace::Event::EXIT_GENUINE
      PinkTrace::Trace.syscall pid
      Process.waitpid pid

      event = PinkTrace::Event.decide
      if event == PinkTrace::Event::SYSCALL then
        scno = PinkTrace::SysCall.get_no pid
        name = PinkTrace::SysCall.name scno
        if name == 'kill' then
          found = true
          break
        end
      end
    end

    assert found, 'Failed to get system call number for kill'

    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_syscall_set_no
    pid = PinkTrace.fork do
      Process.kill 0, Process.pid
    end

    # Loop until we get to the kill() system call as there's no guarantee that
    # other system calls won't be called beforehand.
    event = -1
    while event != PinkTrace::Event::EXIT_GENUINE
      PinkTrace::Trace.syscall pid
      Process.waitpid pid

      event = PinkTrace::Event.decide
      if event == PinkTrace::Event::SYSCALL then
        scno = PinkTrace::SysCall.get_no pid
        name = PinkTrace::SysCall.name scno
        if name == 'kill' then
          PinkTrace::SysCall.set_no pid, 0xbadca11
          scno = PinkTrace::SysCall.get_no pid
          assert(scno == 0xbadca11, 'Wrong system call no, expected: 0xbadca11 got: ' + scno.to_s)
          break
        end
      end
    end

    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_syscall_get_ret_success
    pid = PinkTrace.fork do
      Process.kill 0, Process.pid
    end

    # Loop until we get to the kill() system call as there's no guarantee that
    # other system calls won't be called beforehand.
    event = -1
    insyscall = false
    while event != PinkTrace::Event::EXIT_GENUINE
      PinkTrace::Trace.syscall pid
      Process.waitpid pid

      event = PinkTrace::Event.decide
      if event == PinkTrace::Event::SYSCALL then
        scno = PinkTrace::SysCall.get_no pid
        name = PinkTrace::SysCall.name scno
        if insyscall and name == 'kill' then
          ret = PinkTrace::SysCall.get_ret pid
          assert(ret == 0, 'Wrong system call return, expected: 0 got: ' + ret.to_s)
          break
        end
        insyscall = true unless insyscall
      end
    end

    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_syscall_get_ret_fail
    pid = PinkTrace.fork do
      begin File.open ''
      rescue Errno::ENOENT ;end
    end

    # Loop until we get to the kill() system call as there's no guarantee that
    # other system calls won't be called beforehand.
    event = -1
    insyscall = false
    while event != PinkTrace::Event::EXIT_GENUINE
      PinkTrace::Trace.syscall pid
      Process.waitpid pid

      event = PinkTrace::Event.decide
      if event == PinkTrace::Event::SYSCALL then
        scno = PinkTrace::SysCall.get_no pid
        name = PinkTrace::SysCall.name scno
        if insyscall and name == 'open' then
          ret = PinkTrace::SysCall.get_ret pid
          assert(ret == -Errno::ENOENT::Errno, 'Wrong system call return, expected: ' + (-Errno::ENOENT::Errno).to_s + 'got: ' + ret.to_s)
          break
        end
        insyscall = true unless insyscall
      end
    end

    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_syscall_set_ret_success
    pid = PinkTrace.fork do
      ret = Process.kill 0, Process.pid
      exit 0 if ret == 1
      exit 1
    end
    # Loop until we get to the kill() system call as there's no guarantee that
    # other system calls won't be called beforehand.
    event = -1
    insyscall = false
    while event != PinkTrace::Event::EXIT_GENUINE
      PinkTrace::Trace.syscall pid
      Process.waitpid pid

      event = PinkTrace::Event.decide
      if event == PinkTrace::Event::SYSCALL then
        scno = PinkTrace::SysCall.get_no pid
        name = PinkTrace::SysCall.name scno
        if insyscall and name == 'kill' then
          PinkTrace::SysCall.set_ret pid, 1
        end
        insyscall = true unless insyscall
      end
    end

    assert $?.exited?, "Child hasn't exited!"
    assert($?.exitstatus == 0, 'Wrong exit status, expected: 0 got: ' + $?.exitstatus.to_s)

    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_syscall_set_ret_fail
    pid = PinkTrace.fork do
      begin
        Process.kill 0, Process.pid
      rescue Errno::EPERM
        exit 0
      end
      exit 1
    end
    # Loop until we get to the kill() system call as there's no guarantee that
    # other system calls won't be called beforehand.
    event = -1
    insyscall = false
    while event != PinkTrace::Event::EXIT_GENUINE
      PinkTrace::Trace.syscall pid
      Process.waitpid pid

      event = PinkTrace::Event.decide
      if event == PinkTrace::Event::SYSCALL then
        scno = PinkTrace::SysCall.get_no pid
        name = PinkTrace::SysCall.name scno
        if insyscall and name == 'kill' then
          PinkTrace::SysCall.set_ret pid, -Errno::EPERM::Errno
        end
        insyscall = true unless insyscall
      end
    end

    assert $?.exited?, "Child hasn't exited!"
    assert($?.exitstatus == 0, 'Wrong exit status, expected: 0 got: ' + $?.exitstatus.to_s)

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
