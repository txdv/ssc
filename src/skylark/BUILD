load("@io_bazel_rules_scala//scala:scala.bzl", "scala_binary", "scala_library", "scala_test")
load("//:warnings.bzl", "fatal_warnings")

scala_library(
    name = "skylark",
    srcs = ["Skylark.scala"],
    scalacopts = fatal_warnings(),
    deps = ["//src:parser", "//src:scalaz", "//src:lexer", "//src:span"],
    exports = ["//src:parser", "//src:lexer", "//src:span"])

scala_binary(
    name = "app",
    srcs = ["SkylarkApp.scala"],
    scalacopts = fatal_warnings(),
    main_class = "lt.vu.mif.bentkus.bachelor.compiler.parser.skylark.SkylarkApp",
    deps = [":skylark"])
