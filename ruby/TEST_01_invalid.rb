#!/usr/bin/env ruby
# coding: utf-8
# vim: set sw=2 sts=2 et nowrap fenc=utf-8 :
# Copyright 2010 Ali Polatel <alip@exherbo.org>
# Distributed under the terms of the GNU General Public License v2

require 'test/unit'

$: << File.expand_path('.libs')
require 'PinkTrace'

#
# PinkTrace::Trace
#
class TestTrace < Test::Unit::TestCase
  def test_trace_me_invalid
    assert_raise ArgumentError do
      PinkTrace::Trace.me 0
    end
  end

  def test_trace_me_twice
    assert_raise Errno::EPERM do
      PinkTrace::Trace.me
      # The second call should fail with EPERM.
      PinkTrace::Trace.me
    end
  end

  def test_cont_invalid
    assert_raise TypeError do
      PinkTrace::Trace.cont 'pink', 0
    end
    assert_raise TypeError do
      PinkTrace::Trace.cont 0, 'pink'
    end
    assert_raise ArgumentError do
      PinkTrace::Trace.cont 1, 2, 3
    end
  end

  def test_cont_esrch
    assert_raise Errno::ESRCH do
      PinkTrace::Trace.cont 0, Signal.list['CONT']
    end
  end

  def test_trace_kill_invalid
    assert_raise ArgumentError do
      PinkTrace::Trace.kill
    end
    assert_raise ArgumentError do
      PinkTrace::Trace.kill 0, 1
    end
    assert_raise TypeError do
      PinkTrace::Trace.kill 'pink'
    end
  end

  def test_trace_kill_esrch
    assert_raise Errno::ESRCH do
      PinkTrace::Trace.kill 0
    end
  end

  def test_trace_attach_invalid
    assert_raise ArgumentError do
      PinkTrace::Trace.attach
    end
    assert_raise ArgumentError do
      PinkTrace::Trace.attach 0, 1
    end
    assert_raise TypeError do
      PinkTrace::Trace.attach 'pink'
    end
  end

  def test_trace_attach_esrch
    assert_raise Errno::ESRCH do
      PinkTrace::Trace.attach 0
    end
  end

  def test_trace_detach_invalid
    assert_raise ArgumentError do
      PinkTrace::Trace.detach
    end
    assert_raise ArgumentError do
      PinkTrace::Trace.detach 0, 1, 2
    end
    assert_raise TypeError do
      PinkTrace::Trace.detach 'pink'
    end
    assert_raise TypeError do
      PinkTrace::Trace.detach 0, 'pink'
    end
  end

  def test_trace_detach_esrch
    assert_raise Errno::ESRCH do
      PinkTrace::Trace.detach 0
    end
  end

  def test_trace_geteventmsg_invalid
    assert_raise ArgumentError do
      PinkTrace::Trace.geteventmsg
    end
    assert_raise ArgumentError do
      PinkTrace::Trace.geteventmsg 0, 1
    end
    assert_raise TypeError do
      PinkTrace::Trace.geteventmsg 'pink'
    end
  end

  def test_trace_geteventmsg_esrch
    assert_raise Errno::ESRCH do
      PinkTrace::Trace.geteventmsg 0
    end
  end

  def test_trace_setup_invalid
    assert_raise ArgumentError do
      PinkTrace::Trace.setup
    end
    assert_raise ArgumentError do
      PinkTrace::Trace.setup 0, 1, 2
    end
    assert_raise TypeError do
      PinkTrace::Trace.setup 'pink'
    end
    assert_raise TypeError do
      PinkTrace::Trace.setup 0, 'pink'
    end
  end

  def test_trace_setup_esrch
    assert_raise Errno::ESRCH do
      PinkTrace::Trace.setup 0
    end
  end

  def test_trace_singlestep_invalid
    assert_raise ArgumentError do
      PinkTrace::Trace.singlestep
    end
    assert_raise ArgumentError do
      PinkTrace::Trace.singlestep 0, 1, 2
    end
    assert_raise TypeError do
      PinkTrace::Trace.singlestep 'pink'
    end
    assert_raise TypeError do
      PinkTrace::Trace.singlestep 0, 'pink'
    end
  end

  def test_trace_singlestep_esrch
    assert_raise Errno::ESRCH do
      PinkTrace::Trace.singlestep 0
    end
  end

  def test_trace_syscall_invalid
    assert_raise ArgumentError do
      PinkTrace::Trace.syscall
    end
    assert_raise ArgumentError do
      PinkTrace::Trace.syscall 0, 1, 2
    end
    assert_raise TypeError do
      PinkTrace::Trace.syscall 'pink'
    end
    assert_raise TypeError do
      PinkTrace::Trace.syscall 0, 'pink'
    end
  end

  def test_trace_syscall_esrch
    assert_raise Errno::ESRCH do
      PinkTrace::Trace.syscall 0
    end
  end
end

#
# PinkTrace::Bitness
#
class TestBitness < Test::Unit::TestCase
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
end

#
# PinkTrace::Event
#
class TestEvent < Test::Unit::TestCase
  def test_event_decide_invalid
    assert_raise ArgumentError do
      PinkTrace::Event.decide 0, 1
    end
    assert_raise TypeError do
      PinkTrace::Event.decide 'pink'
    end
  end

  def test_event_decide_unknown
    assert_raise PinkTrace::EventError do
      PinkTrace::Event.decide -1
    end
  end
end

#
# PinkTrace::String
#
class TestString < Test::Unit::TestCase
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
  end

  def test_string_encode_esrch
    assert_raise Errno::ESRCH do
      PinkTrace::String.encode 0, 1, 'pink'
    end
  end

  def test_string_encode!_invalid
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
  end

  def test_string_encode!_esrch
    assert_raise Errno::ESRCH do
      PinkTrace::String.encode! 0, 1, 'pink'
    end
  end
end

#
# PinkTrace::SysCall
#
class TestSysCall < Test::Unit::TestCase
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
