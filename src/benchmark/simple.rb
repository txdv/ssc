
def simple(f, i)
  f.puts("object A#{i} { }")
end

def main(i)
"""
  def main#{i}(args: Array[String]): Unit = {
    println(\"Hello world\")
  }
"""
end

def hard(f, i)
  f.puts("object A#{i} {}")
end

arg = ARGV.first || "100"
count = arg.to_i

File.open("MultipleSimple.scala", "w+") do |f|
  count.times do |i|
    hard(f, i)
  end
end
