#!/usr/bin/env ruby
# coding: utf-8

require 'socket'
require 'test/unit'

$:.insert(0, '.libs')
require 'PinkTrace'

class TestPinkSocketAddress < Test::Unit::TestCase
  def test_address_no_initialize
    assert !(PinkTrace::Socket::Address.respond_to? :initialize)
  end

  def test_address_decode_invalid
    assert_raise PinkTrace::IndexError do
      PinkTrace::Socket.decode_address 0, PinkTrace::Syscall::MAX_INDEX
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
    assert_raise PinkTrace::IndexError do
      PinkTrace::Socket.decode_address_fd 0, PinkTrace::Syscall::MAX_INDEX
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
if PinkTrace::Syscall.name 0
  class TestPinkSocketAddress
    TEST_UNIX_SOCKET = './TEST_UNIX_SOCKET'

    def test_address_decode
      pid = fork do
        PinkTrace::Trace.me
        Process.kill 'STOP', Process.pid

        UNIXSocket.new TEST_UNIX_SOCKET
      end
      Process.waitpid pid
      PinkTrace::Trace.setup pid, PinkTrace::Trace::OPTION_SYSGOOD

      # Loop until we get to the connect() system call.
      event = -1
      while event != PinkTrace::Event::EVENT_EXIT_GENUINE
        PinkTrace::Trace.syscall pid
        Process.waitpid pid

        event = PinkTrace::Event.decide
        if event == PinkTrace::Event::EVENT_SYSCALL
          scno = PinkTrace::Syscall.get_no pid
          name = PinkTrace::Syscall.name scno

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
          assert addr.unix?, "#{addr.family}"
          assert !addr.abstract?, "Expected non-abstract socket, got abstract"
          assert(addr.path == TEST_UNIX_SOCKET, "Wrong path, expected: '#{TEST_UNIX_SOCKET}' got: '#{addr.path}'")
          break
        end
      end

      begin PinkTrace::Trace.kill pid
      rescue Errno::ESRCH ;end
    end

    def test_decode_address_fd
      pid = fork do
        PinkTrace::Trace.me
        Process.kill 'STOP', Process.pid

        UNIXSocket.new TEST_UNIX_SOCKET
      end
      Process.waitpid pid
      PinkTrace::Trace.setup pid, PinkTrace::Trace::OPTION_SYSGOOD

      # Loop until we get to the connect() system call.
      event = -1
      while event != PinkTrace::Event::EVENT_EXIT_GENUINE
        PinkTrace::Trace.syscall pid
        Process.waitpid pid

        event = PinkTrace::Event.decide
        if event == PinkTrace::Event::EVENT_SYSCALL
          scno = PinkTrace::Syscall.get_no pid
          name = PinkTrace::Syscall.name scno

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
          assert addr.unix?, "#{addr.family}"
          assert(!addr.abstract?, "Expected non-abstract socket, got abstract")
          assert(addr.path == TEST_UNIX_SOCKET, "Wrong path, expected: '#{TEST_UNIX_SOCKET}' got: '#{addr.path}'")
          break
        end
      end

      begin PinkTrace::Trace.kill pid
      rescue Errno::ESRCH ;end
    end
  end
end
