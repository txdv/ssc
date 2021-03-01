load("@io_bazel_rules_scala//scala:scala.bzl", "scala_binary", "scala_library", "scala_test")
load("//:warnings.bzl", "fatal_warnings")
package(default_visibility = ["//visibility:public"])

cats_version = "2_4_1"

scala_library(
    name = "cats",
    exports = [
        "@maven//:org_typelevel_cats_core_2_13_%s" % cats_version,
        "@maven//:org_typelevel_cats_kernel_2_13_%s" % cats_version,
    ])

scala_library(
    name = "scalaz",
    exports = [
      "@maven//:org_scalaz_scalaz_core_2_13",
    ])

scala_binary(
    name = "version",
    srcs = ["Version.scala"],
    main_class = "Version")

scala_binary(
    name = "lexer",
    srcs = ["Lexer.scala"],
    scalacopts = fatal_warnings(),
    main_class = "lt.vu.mif.bentkus.bachelor.compiler.lexer.LexerApp")

scala_test(
    name = "lexer_spec",
    srcs = ["LexerSpec.scala"],
    scalacopts = fatal_warnings(),
    deps = [":lexer"])

scala_library(
    name = "util",
    srcs = ["Util.scala"],
    scalacopts = fatal_warnings())

scala_library(
    name = "parser",
    srcs = ["Parser.scala"],
    scalacopts = fatal_warnings(),
    deps = [":lexer", ":scalaz"],
    exports = [":lexer", ":scalaz", ":util"])

scala_test(
    name = "parser_spec",
    srcs = ["ParserSpec.scala"],
    scalacopts = fatal_warnings(),
    deps = [":parser", ":util"])

scala_library(
    name = "scala",
    srcs = ["Scala.scala"],
    scalacopts = fatal_warnings(),
    deps = [":parser"],
    exports = [":parser"])

scala_test(
    name = "scala_spec",
    srcs = ["ScalaSpec.scala"],
    scalacopts = fatal_warnings(),
    deps = [":scala"])

scala_binary(
    name = "scala_app",
    srcs = ["ScalaApp.scala"],
    scalacopts = fatal_warnings(),
    deps = [":scala", ":lexer"],
    main_class = "lt.vu.mif.bentkus.bachelor.compiler.parser.scala.ScalaApp")

scala_binary(
    name = "classfile",
    srcs = ["ClassFile.scala"],
    scalacopts = fatal_warnings(),
    main_class = "lt.vu.mif.bentkus.bachelor.compiler.classfile.MainApp")

scala_binary(
    name = "class",
    srcs = ["Class.scala"],
    deps = [":classfile"],
    scalacopts = fatal_warnings(),
    main_class = "lt.vu.mif.bentkus.bachelor.compiler.classfile.higher.MainApp")

scala_binary(
    name = "compiler",
    srcs = ["Compiler.scala"],
    deps = [":scala", ":class"],
    scalacopts = fatal_warnings(),
    main_class = "lt.vu.mif.bentkus.bachelor.compiler.MainApp")
