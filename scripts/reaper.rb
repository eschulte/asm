#!/usr/bin/env ruby
## reap long-running processes and all of their descendants
##
##   (?_^
##    |\ )
##    |/_\
##
def variants
  %x{ps auxwww |grep -i "tmp/variant"}.split("\n").map{|l| $1 if l.match(/^[\S]+[\s]+([\d]+)[\s]+/)}
end
def and_descendants(pid)
  %x{ps -axo pid,ppid |grep -i #{pid}}.split("\n").map{|l| $1 if l.match(/^([\d]+)[\s]+([\d]+)/)}
end
old_procs = []
while true do
  variants.select{|p| old_procs.include?(p)}.each{|p| p.and_descendants.each{|p| %x{kill -9 #{p}}}}
  old_procs = variant_procs
  sleep 4
end
