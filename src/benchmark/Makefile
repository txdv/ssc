

run:
	#time bazel run //src:compiler -- /Users/andriusb/Projects/latex/darbas/src/benchmark/MultipleSimple.scala
	ruby simple.rb 10
	rm -f *.class
	time java -jar ../../bazel-bin/src/compiler_deploy.jar /Users/andriusb/Projects/latex/darbas/src/benchmark/MultipleSimple.scala
	rm -f *.class
	time scalac MultipleSimple.scala
	# echo asd
	ruby simple.rb 100
	rm -f *.class
	time java -jar ../../bazel-bin/src/compiler_deploy.jar /Users/andriusb/Projects/latex/darbas/src/benchmark/MultipleSimple.scala
	rm -f *.class
	time scalac MultipleSimple.scala
	# echo asd
	ruby simple.rb 1000
	rm -f *.class
	time java -jar ../../bazel-bin/src/compiler_deploy.jar /Users/andriusb/Projects/latex/darbas/src/benchmark/MultipleSimple.scala
	rm -f *.class
	time scalac MultipleSimple.scala
