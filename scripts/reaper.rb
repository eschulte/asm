#!/usr/bin/env ruby
## reap long-running processes
def variant_procs
  %x{ps auxwww |grep -i "tmp/variant"}.split("\n").map{ |l| $1 if l.match(/^[\S]+[\s]+([\d]+)[\s]+/) }
end
old_procs = []
while true do
  # say it
  # puts :collecting
  # puts variant_procs.size
  # variant_procs.select{ |p| old_procs.include?(p) }.each{ |p| puts p }
  # do it
  variant_procs.select{ |p| old_procs.include?(p) }.each{ |p| %x{kill -9 #{p}} }
  old_procs = variant_procs
  sleep 4
end
