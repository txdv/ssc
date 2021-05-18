require 'json'


#puts "Classes & Methods & Prototyp & Cached & Diff & Official \\\\"
lines = File.read("results.json").lines

plots = { }

lines.each do |line|
  obj = JSON.parse(line, object_class: OpenStruct)
  diff = obj.new.clock - obj.new_cached.clock
	pct = ((diff.to_f / obj.new.clock) * 100).to_i

  methods = obj.methods
  classes = obj.classes

  plots[methods] = plots[methods] || []

  plots[methods].push({ :classes => classes, :diff => diff, :pct => pct })
end

color = {
  0 => "blue",
  1 => "red",
  5 => "green",
  10 => "purple",
  20 => "brown",
  50 => "pink",
  100 => "violet"
}

plots.each do |methods, diff|

puts """
\\addplot[color=#{color[methods]}, mark=square]
  table {"""
  diff.each do |s|
    if s[:classes] != 2000 && s[:classes] != 1000
      classes = s[:classes].to_s.rjust(4, ' ')
      pct = s[:pct].to_s.rjust(4, ' ')
      puts "    #{classes} #{pct}"
    end
  end
puts """  };"""
end
