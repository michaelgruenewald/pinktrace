#!/usr/bin/env ruby
# coding: utf-8
# vim: set sw=2 sts=2 et nowrap fenc=utf-8 :
# Copyright 2010 Ali Polatel <alip@exherbo.org>
# Distributed under the terms of the GNU General Public License v2

%w{socket test/unit}.each {|m| require m}

$: << File.expand_path('.libs')
require 'PinkTrace'

class TestPinkSocket < Test::Unit::TestCase
  def test_socket_name_invalid
    assert_raise ArgumentError do
      PinkTrace::Socket.name
    end
    assert_raise ArgumentError do
      PinkTrace::Socket.name 1, 2
    end
    assert_raise TypeError do
      PinkTrace::Socket.name 'pink'
    end
  end

  def test_socket_decode_call_invalid
    assert_raise ArgumentError do
      PinkTrace::Socket.decode_call
    end
    assert_raise ArgumentError do
      PinkTrace::Socket.decode_call 0, 1, 2
    end
    assert_raise TypeError do
      PinkTrace::Socket.decode_call 'pink'
    end
    assert_raise TypeError do
      PinkTrace::Socket.decode_call 0, 'pink'
    end
  end

  def test_socket_decode_call_esrch
    assert_raise Errno::ESRCH do
      PinkTrace::Socket.decode_call 0
    end
  end

  def test_socket_decode_fd_invalid
    assert_raise ArgumentError do
      PinkTrace::Socket.decode_fd
    end
    assert_raise ArgumentError do
      PinkTrace::Socket.decode_fd 0, 1, 2, 3
    end
    assert_raise TypeError do
      PinkTrace::Socket.decode_fd 'pink'
    end
    assert_raise TypeError do
      PinkTrace::Socket.decode_fd 0, 'pink'
    end
    assert_raise TypeError do
      PinkTrace::Socket.decode_fd 0, 1, 'pink'
    end
    assert_raise PinkTrace::BitnessError do
      PinkTrace::Socket.decode_fd 0, 1, 13
    end
    assert_raise PinkTrace::IndexError do
      PinkTrace::Socket.decode_fd 0, PinkTrace::MAX_INDEX
    end
  end

  def test_socket_decode_fd_esrch
    assert_raise Errno::ESRCH do
      PinkTrace::Socket.decode_fd 0
    end
  end
end

# These test cases depend on generated system call names.
# Don't run them if they weren't generated.
unless PinkTrace::SysCall.name 0
  exit 0
end

class TestPinkSocket
  TEST_SOCKET = './TEST_unix_socket'

  def teardown
    begin
      File.unlink TEST_SOCKET
    rescue Errno::ENOENT
    end
  end

  def test_decode_socket_call
    pid = PinkTrace::Fork.fork do
      s = UNIXServer.new './TEST_unix_socket'
    end

    # Loop until we get to the socket() system call as there's no guarantee
    # that other system calls won't be called beforehand.
    event = -1
    while event != PinkTrace::Event::EXIT_GENUINE
      PinkTrace::Trace.syscall pid
      Process.waitpid pid

      event = PinkTrace::Event.decide
      if event == PinkTrace::Event::SYSCALL then
        scno = PinkTrace::SysCall.get_no pid
        name = PinkTrace::SysCall.name scno
        if name == 'socketcall' or name == 'socket' then
          subcall = PinkTrace::Socket.decode_call pid
          if name == 'socketcall' then
            # The call must have given the decoded socket call.
            subname = PinkTrace::Socket.name subcall
          else
            subname = PinkTrace::SysCall.name subcall
          end
          assert(subname == 'socket', "Wrong subcall name, expected: socket got: '#{subname}'")
          break
        end
      end
    end

    begin PinkTrace::Trace.kill pid
    rescue Errno::ESRCH ;end
  end

  def test_decode_socket_fd
  end
end
