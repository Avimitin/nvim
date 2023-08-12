#!/usr/bin/env ruby

require 'fileutils'

changes = 0
Dir.glob("**/*.fnl", base: Dir.pwd)
  .map {|fnl|
    [File.absolute_path(fnl),
     File.absolute_path(fnl.gsub!("fnl", "lua"))]
  }
  .filter {|pair|
    fnl = pair[0]
    lua = pair[1]

    !File.exists?(lua) ||
      File.mtime(fnl) > File.mtime(lua)
  }
  .each {|pair|
    fnl = pair[0]
    lua = pair[1]
    dir = File.dirname(lua)
    if !Dir.exists?(dir)
      FileUtils.mkdir_p(dir)
    end
    `fennel --compile #{fnl} > #{lua}`
    changes += 1
  }

puts "#{changes} file compiled"
