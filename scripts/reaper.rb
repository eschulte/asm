#!/usr/bin/env ruby
## reap long-running processes and all of their descendants
##
##   (?_^
##    |\ )
##    |/_\
##
def variants
  ["tmp/variant", "exec_bad", "exec_good"].map do |pattern|
    %x{ps auxwww |grep -i #{pattern}}.split("\n").map do |l|
      if l.match(/^[\S]+[\s]+([\d]+)[\s]+/)
        # puts l
        $1
      end
    end.compact
  end.flatten
end
def and_descendants(pid)
  %x{ps ax -o pid,ppid |grep -i #{pid}}.split("\n").map{|l| $1 if l.match(/^([\d]+)[\s]+([\d]+)/)}.compact
end
old = []
while true do
  variants.select{|p| old.include?(p)}.each{|p| and_descendants(p).each{|p| %x{kill -9 #{p}}}}
  # variants.select{|p| old.include?(p)}.each{|p| and_descendants(p).each{|p| puts %Q{kill -9 #{p}}}}
  old = variants
  sleep 4
end
