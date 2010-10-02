#!/usr/bin/env ruby
# coding: utf-8
# vim: set sw=2 sts=2 et fenc=utf-8 :

=begin
An example demonstrating the tracing fork
=end

require 'PinkTrace'

pid = fork do
  # Prepare for tracing.
  PinkTrace::Trace.me
  # Stop to give the parent a chance to resume execution after setting options.
  Process.kill 'STOP', Process.pid

  puts "hello world"
end

Process.wait # sets $?
event = PinkTrace::Event.decide # uses $?.status by default
unless event == PinkTrace::Event::EVENT_STOP
  puts "wtf?"
  PinkTrace::Trace.kill pid
  exit 1
end

# Set tracing options
PinkTrace::Trace.setup pid
# Let the child resume its execution.
PinkTrace::Trace.resume pid
Process.wait
