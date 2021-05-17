
def simple(f, i)
  f.puts("object A#{i} { }")
end

def main(i)
j = i == 1 ? "" : i.to_s
"""  def main#{j}(args: Array[String]): Unit = {
    println(\"Hello world %i\")
  }
"""
end

def hard(f, i, method_count)
  f.puts("object A#{i} {")
  method_count.times do |imethod|
    f.puts(main(imethod))
  end
  f.puts("}")
end

arg1 = ARGV.first || "100"
arg2 = ARGV[1] || "10"

classes = arg1.to_i
methods = arg2.to_i

File.open("MultipleSimple.scala", "w+") do |f|
  classes.times do |iclass|
    hard(f, iclass, methods)
  end
end
