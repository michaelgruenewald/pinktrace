#!/usr/bin/env ruby
# coding: utf-8
# vim: set sw=2 sts=2 et fenc=utf-8 :

=begin
Simple example showing how to use PinkTrace version constants
=end

require 'PinkTrace'

print 'Built using ', PinkTrace::PACKAGE, " ",
  PinkTrace::VERSION_MAJOR, ".",
  PinkTrace::VERSION_MINOR, ".",
  PinkTrace::VERSION_MICRO,
  PinkTrace::VERSION_SUFFIX

unless PinkTrace::GIT_HEAD.empty?
  print ' git-', PinkTrace::GIT_HEAD
end

puts
