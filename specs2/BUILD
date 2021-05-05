load("@io_bazel_rules_scala//scala:scala.bzl", "scala_binary", "scala_library", "scala_test")
load("//:warnings.bzl", "fatal_warnings")
package(default_visibility = ["//visibility:public"])

specs2_version = "4_11_0"
scala_library(
    name = "specs2",
    exports = [
        "@maven//:org_specs2_specs2_core_2_13_%s" % specs2_version,
        "@maven//:org_specs2_specs2_junit_2_13_%s" % specs2_version,
        "@maven//:org_specs2_specs2_matcher_2_13_%s" % specs2_version,
        "@maven//:org_specs2_specs2_mock_2_13_%s" % specs2_version,
        "@maven//:org_specs2_specs2_analysis_2_13_%s" % specs2_version,
        "@maven//:org_specs2_specs2_common_2_13_%s" % specs2_version,
        "@maven//:org_specs2_specs2_fp_2_13_%s" % specs2_version,
        "@maven//:org_mockito_mockito_core_3_9_0",
        "@maven//:org_hamcrest_hamcrest_all_1_3",
    ])

scala_test(
    name = "sample",
    srcs = ["SampleTest.scala"],
    deps = [":specs2"],
    scalacopts = fatal_warnings())
