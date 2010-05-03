#!/usr/bin/env ruby
# coding: utf-8
# vim: set sw=2 sts=2 et nowrap fenc=utf-8 :
# Copyright 2010 Ali Polatel <alip@exherbo.org>
# Distributed under the terms of the GNU General Public License v2

require 'socket'
require 'test/unit'

$: << File.expand_path('.libs')
require 'PinkTrace'

class TestPinkSocketAddress < Test::Unit::TestCase
  def test_address_no_initialize
    assert !(PinkTrace::Socket::Address.respond_to? :initialize)
  end

  def test_address_decode_invalid
    assert_raise ArgumentError do
      PinkTrace::Socket.decode_address
    end
    assert_raise ArgumentError do
      PinkTrace::Socket.decode_address 0
    end
    assert_raise ArgumentError do
      PinkTrace::Socket.decode_address 0, 1, 2, 3
    end
    assert_raise TypeError do
      PinkTrace::Socket.decode_address 'pink', 1
    end
    assert_raise TypeError do
      PinkTrace::Socket.decode_address 0, 'pink'
    end
    assert_raise TypeError do
      PinkTrace::Socket.decode_address 0, 1, 'pink'
    end
    assert_raise PinkTrace::IndexError do
      PinkTrace::Socket.decode_address 0, PinkTrace::MAX_INDEX
    end
    assert_raise PinkTrace::BitnessError do
      PinkTrace::Socket.decode_address 0, 1, 13
    end
  end

  def test_address_decode_esrch
    assert_raise Errno::ESRCH do
      PinkTrace::Socket.decode_address 0, 1
    end
  end

  def test_address_decode2_invalid
    assert_raise ArgumentError do
      PinkTrace::Socket.decode_address_fd
    end
    assert_raise ArgumentError do
      PinkTrace::Socket.decode_address_fd 0
    end
    assert_raise ArgumentError do
      PinkTrace::Socket.decode_address_fd 0, 1, 2, 3
    end
    assert_raise TypeError do
      PinkTrace::Socket.decode_address_fd 'pink', 1
    end
    assert_raise TypeError do
      PinkTrace::Socket.decode_address_fd 0, 'pink'
    end
    assert_raise TypeError do
      PinkTrace::Socket.decode_address_fd 0, 1, 'pink'
    end
    assert_raise PinkTrace::IndexError do
      PinkTrace::Socket.decode_address_fd 0, PinkTrace::MAX_INDEX
    end
    assert_raise PinkTrace::BitnessError do
      PinkTrace::Socket.decode_address_fd 0, 1, 13
    end
  end

  def test_address_decode_esrch
    assert_raise Errno::ESRCH do
      PinkTrace::Socket.decode_address_fd 0, 1
    end
  end

end

# These test cases depend on generated system call names.
# Don't run them if they weren't generated.
unless PinkTrace::SysCall.name 0
  exit 0
end

class TestPinkSocketAddress
  TEST_UNIX_SOCKET = './TEST_UNIX_SOCKET'

  def test_address_decode
    pid = PinkTrace::Fork.fork do
      UNIXSocket.new TEST_UNIX_SOCKET
    end

    # Loop until we get to the connect() system call.
    event = -1
    while event != PinkTrace::Event::EVENT_EXIT_GENUINE
      PinkTrace::Trace.syscall pid
      Process.waitpid pid

      event = PinkTrace::Event.decide
      if event == PinkTrace::Event::EVENT_SYSCALL
        scno = PinkTrace::SysCall.get_no pid
        name = PinkTrace::SysCall.name scno

        if name == 'socketcall'
          subcall = PinkTrace::Socket.decode_call pid
          subname = PinkTrace::Socket.name subcall
          next unless subname == 'connect'
        else
          next unless name == 'connect'
        end

        # We are at the beginning of the connect() call!
        addr = PinkTrace::Socket.decode_address pid, 1
        assert(addr.class == PinkTrace::Socket::Address, "#{addr.class}")
        assert(addr.family == Socket::AF_UNIX, "Wrong family, expected: AF_UNIX got: #{addr.family}")
        assert !addr.abstract?, "Expected non-abstract socket, got abstract"
        assert(addr.to_s == TEST_UNIX_SOCKET, "Wrong path, expected: '#{TEST_UNIX_SOCKET}' got: '#{addr.to_s}'")
        break
      end
    end

    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_decode_address_fd
    pid = PinkTrace::Fork.fork do
      UNIXSocket.new TEST_UNIX_SOCKET
    end

    # Loop until we get to the connect() system call.
    event = -1
    while event != PinkTrace::Event::EVENT_EXIT_GENUINE
      PinkTrace::Trace.syscall pid
      Process.waitpid pid

      event = PinkTrace::Event.decide
      if event == PinkTrace::Event::EVENT_SYSCALL
        scno = PinkTrace::SysCall.get_no pid
        name = PinkTrace::SysCall.name scno

        if name == 'socketcall'
          subcall = PinkTrace::Socket.decode_call pid
          subname = PinkTrace::Socket.name subcall
          next unless subname == 'connect'
        else
          next unless name == 'connect'
        end

        # We are at the beginning of the connect() call!
        addr, fd = PinkTrace::Socket.decode_address_fd pid, 1
        assert(fd.class == Fixnum, "#{fd.class}")
        assert(addr.class == PinkTrace::Socket::Address, "#{addr.class}")
        assert(addr.family == Socket::AF_UNIX, "Wrong family, expected: AF_UNIX got: #{addr.family}")
        assert(!addr.abstract?, "Expected non-abstract socket, got abstract")
        assert(addr.to_s == TEST_UNIX_SOCKET, "Wrong path, expected: '#{TEST_UNIX_SOCKET}' got: '#{addr.to_s}'")
        break
      end
    end

    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end
end
