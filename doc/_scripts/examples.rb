#!/usr/bin/env ruby
# coding: utf-8
# vim: set sw=2 sts=2 et nowrap fenc=utf-8 :

require 'rubygems'
require 'liquid'

ARGV.each do |example|
  unless File.readable? example
    $stderr.puts "File '#{example}' not readable!"
    exit 1
  end

  ext = File.extname example
  case ext
  when /\.c$/
    lang = "c"
  when /\.py$/
    lang = "python"
  when /\.rb$/
    lang = "ruby"
  else
    $stderr.puts "Unknown file extension '#{ext}'"
    exit 1
  end

  fp = File.open(example)
  data = fp.read
  fp.close
  puts <<EOF
---
layout: nil"
title: #{File.basename example}
---
<!-- vim: set ro : -->
<html>
  <body>
    {% highlight #{lang} %}
      #{data}
    {% endhighlight %}
  </body>"
</html>
EOF
end
