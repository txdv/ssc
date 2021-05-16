
TIME=/usr/bin/time -p

run:
	#time bazel run //src:compiler -- /Users/andriusb/Projects/latex/darbas/src/benchmark/MultipleSimple.scala
	ruby simple.rb 10
	$(TIME) java -jar ../../bazel-bin/src/compiler_deploy.jar /Users/andriusb/Projects/latex/darbas/src/benchmark/MultipleSimple.scala
	rm -f *.class
	$(TIME) scalac MultipleSimple.scala
	rm -f *.class
	# echo asd
	ruby simple.rb 100
	$(TIME) java -jar ../../bazel-bin/src/compiler_deploy.jar /Users/andriusb/Projects/latex/darbas/src/benchmark/MultipleSimple.scala
	rm -f *.class
	$(TIME) scalac MultipleSimple.scala
	rm -f *.class
	# echo asd
	ruby simple.rb 1000
	$(TIME) java -jar ../../bazel-bin/src/compiler_deploy.jar /Users/andriusb/Projects/latex/darbas/src/benchmark/MultipleSimple.scala
	rm -f *.class
	$(TIME) scalac MultipleSimple.scala
	rm -f *.class
	# echo asd
	ruby simple.rb 5000
	$(TIME) java -Xss10m -Xmx12g -jar ../../bazel-bin/src/compiler_deploy.jar /Users/andriusb/Projects/latex/darbas/src/benchmark/MultipleSimple.scala
	rm -f *.class
	$(TIME) scalac MultipleSimple.scala
	rm -f *.class
