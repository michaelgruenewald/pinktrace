#!/usr/bin/env ruby
# coding: utf-8
# vim: set sw=2 sts=2 et nowrap fenc=utf-8 :

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

  puts <<EOF
---
layout: default
title: #{File.basename example}
---
<h3>#{File.basename example}</h3>

{% highlight #{lang} linenos %}
EOF
  IO.foreach(example) {|line| puts "\t#{line}"}
  puts
  puts '{% endhighlight %}'
end
