

require 'json'

#puts JSON.parse(File.read("resi;t)
puts "Classes & Methods & Prototyp & Cached & Official \\\\"
lines = File.read("results.json").lines
lines.each do |line|
  result = JSON.parse(line)
  classes = result["classes"].to_s.rjust(4, ' ')
  methods = result["methods"].to_s.rjust(3, ' ')
  new = result["new"]["clock"].to_s.rjust(5, ' ')
  cache = result["new_cached"]["clock"].to_s.rjust(5, ' ')
  old = result["old"]["clock"].to_s.rjust(5, ' ')
  puts "#{classes} & #{methods} & #{new} & #{cache} & #{old} \\\\"
end
