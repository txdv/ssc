
TIME=/usr/bin/time -p

run:
	# 10
	ruby simple.rb 10
	rm class_cache
	$(TIME) java -jar ../../bazel-bin/src/compiler_deploy.jar /Users/andriusb/Projects/latex/darbas/src/benchmark/MultipleSimple.scala
	rm -f *.class
	$(TIME) java -jar ../../bazel-bin/src/compiler_deploy.jar /Users/andriusb/Projects/latex/darbas/src/benchmark/MultipleSimple.scala
	rm -f *.class
	$(TIME) scalac MultipleSimple.scala
	rm -f *.class
	# 100
	ruby simple.rb 100
	rm class_cache
	$(TIME) java -jar ../../bazel-bin/src/compiler_deploy.jar /Users/andriusb/Projects/latex/darbas/src/benchmark/MultipleSimple.scala
	rm -f *.class
	$(TIME) java -jar ../../bazel-bin/src/compiler_deploy.jar /Users/andriusb/Projects/latex/darbas/src/benchmark/MultipleSimple.scala
	rm -f *.class
	$(TIME) scalac MultipleSimple.scala
	rm -f *.class
	# 1000
	ruby simple.rb 1000
	rm class_cache
	$(TIME) java -jar ../../bazel-bin/src/compiler_deploy.jar /Users/andriusb/Projects/latex/darbas/src/benchmark/MultipleSimple.scala
	rm -f *.class
	$(TIME) java -jar ../../bazel-bin/src/compiler_deploy.jar /Users/andriusb/Projects/latex/darbas/src/benchmark/MultipleSimple.scala
	rm -f *.class
	$(TIME) scalac MultipleSimple.scala
	rm -f *.class
	# 5000
	ruby simple.rb 5000
	rm class_cache
	$(TIME) java -Xss10m -Xmx12g -jar ../../bazel-bin/src/compiler_deploy.jar /Users/andriusb/Projects/latex/darbas/src/benchmark/MultipleSimple.scala
	rm -f *.class
	$(TIME) java -Xss10m -Xmx12g -jar ../../bazel-bin/src/compiler_deploy.jar /Users/andriusb/Projects/latex/darbas/src/benchmark/MultipleSimple.scala
	rm -f *.class
	$(TIME) scalac MultipleSimple.scala
	rm -f *.class

bla:
	ruby simple.rb 1000
	rm -f *.class
	rm class_cache
	$(TIME) java -jar ../../bazel-bin/src/compiler_deploy.jar /Users/andriusb/Projects/latex/darbas/src/benchmark/MultipleSimple.scala
	$(TIME) java -jar ../../bazel-bin/src/compiler_deploy.jar /Users/andriusb/Projects/latex/darbas/src/benchmark/MultipleSimple.scala
