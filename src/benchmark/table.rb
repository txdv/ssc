require 'json'

#puts JSON.parse(File.read("resi;t)
puts "Classes & Methods & Official & Prototype & Cached & Diff \\\\"
lines = File.read("results.json").lines
lines.each do |line|
  obj = JSON.parse(line, object_class: OpenStruct)
  classes = obj.classes.to_s.rjust(4, ' ')
  methods = obj.methods.to_s.rjust(3, ' ')
  new = obj.new.clock.to_s.rjust(5, ' ')
  cache = obj.new_cached.clock.to_s.rjust(5, ' ')
  old = obj.old.clock.to_s.rjust(5, ' ')

  diff = (obj.new.clock - obj.new_cached.clock).to_s.rjust(5, ' ')

  puts "#{classes} & #{methods} & #{old} & #{new} & #{cache} & #{diff} \\\\"
end
