load("@io_bazel_rules_scala//scala:scala.bzl", "scala_binary", "scala_library", "scala_test")
load("//:warnings.bzl", "fatal_warnings")

cats_version = "2_4_1"

scala_library(
    name = "cats",
    exports = [
        "@maven//:org_typelevel_cats_core_2_13_%s" % cats_version,
    ])

scala_binary(
    name = "version",
    srcs = ["Version.scala"],
    main_class = "Version")

scala_binary(
    name = "lexer",
    srcs = ["Lexer.scala"],
    scalacopts = fatal_warnings(),
    main_class = "lt.vu.mif.bentkus.bachelor.compiler.lexer.LexerApp",
    deps = ["cats"])

scala_test(
    name = "lexer_test",
    srcs = ["LexerTest.scala"],
    scalacopts = fatal_warnings(),
    deps = [":lexer"])
