#!/usr/bin/env ruby
# coding: utf-8
# vim: set sw=2 sts=2 et fenc=utf-8 :

=begin
An example demonstrating the tracing fork
=end

require 'PinkTrace'

pid = PinkTrace::Fork.fork(PinkTrace::Trace::OPTION_ALL) do
  puts "hello world"
end

# At this point the child has been stopped for tracing and stopped itself
# using SIGSTOP. We don't do anything interesting for this example.

# Kill the child, the puts function will never be called.
PinkTrace::Trace.kill pid
