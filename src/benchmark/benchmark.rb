require 'json'

def gen(classes, methods)
  `ruby simple.rb #{classes} #{methods}`
end

classes = [1, 5, 10, 20, 50, 100]
methods = [0, 1, 5, 10, 20, 50, 100]


def new_compiler
  jar = "/Users/andriusb/Projects/latex/darbas/bazel-bin/src/compiler_deploy.jar"
  file = "/Users/andriusb/Projects/latex/darbas/src/benchmark/MultipleSimple.scala"

  start = Time.now
  res = `java -Xss10m -Xmx12g -jar #{jar} #{file} 2>&1`
  result = { }
  res.lines.each do |line|
    tmp = line.split("\t")
    result[tmp[0]] = tmp[1].to_i
  end
  finish = Time.now

  result["clock"] = ((finish - start)*1000).to_i
  result
end

def old_compiler
  file = "/Users/andriusb/Projects/latex/darbas/src/benchmark/MultipleSimple.scala"
  start = Time.now
  res = `JAVA_OPTS="-Xmx12g" scalac #{file}`
  finish = Time.now
  finish - start
  result = { }
  result["clock"] = ((finish - start)*1000).to_i
  result
end

f = File.open("results.json", "w")

classes.each do |iclass|
  methods.each do |imethod|
    puts "#{iclass} classes #{imethod} methods"
    gen(iclass, imethod)
    `rm -f class_cache`
    r = { }
    r['classes'] = iclass
    r['methods'] = imethod
    r['new'] = new_compiler
    puts "new #{r['new']}"
    r['new_cached'] = new_compiler
    puts "cac #{r['new_cached']}"
    r['old'] = old_compiler
    puts "old #{r['old']}"
    f.puts r.to_json
    f.flush
  end
end

f.close
