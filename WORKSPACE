# WORKSPACE
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "bazel_skylib",
    sha256 = "b8a1527901774180afc798aeb28c4634bdccf19c4d98e7bdd1ce79d1fe9aaad7",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/bazel-skylib/releases/download/1.4.1/bazel-skylib-1.4.1.tar.gz",
        "https://github.com/bazelbuild/bazel-skylib/releases/download/1.4.1/bazel-skylib-1.4.1.tar.gz",
    ],
)

# See https://github.com/bazelbuild/rules_scala/releases for up to date version information.
http_archive(
    name = "io_bazel_rules_scala",
    sha256 = "71324bef9bc5a885097e2960d5b8effed63399b55572219919d25f43f468c716",
    strip_prefix = "rules_scala-6.2.1",
    url = "https://github.com/bazelbuild/rules_scala/releases/download/v6.2.1/rules_scala-v6.2.1.tar.gz",
)

load("@io_bazel_rules_scala//:scala_config.bzl", "scala_config")
# Stores Scala version and other configuration
# 2.12 is a default version, other versions can be use by passing them explicitly:
# scala_config(scala_version = "2.11.12")
# Scala 3 requires extras...
#   3.2 should be supported on master. Please note that Scala artifacts for version (3.2.2) are not defined in
#   Rules Scala, they need to be provided by your WORKSPACE. You can use external loader like
#   https://github.com/bazelbuild/rules_jvm_external
scala_config(scala_version = "2.13.12")

load("@io_bazel_rules_scala//scala:scala.bzl", "rules_scala_setup", "rules_scala_toolchain_deps_repositories")

# loads other rules Rules Scala depends on 
rules_scala_setup()

# Loads Maven deps like Scala compiler and standard libs. On production projects you should consider 
# defining a custom deps toolchains to use your project libs instead 
rules_scala_toolchain_deps_repositories(fetch_sources = True)

load("@rules_proto//proto:repositories.bzl", "rules_proto_dependencies", "rules_proto_toolchains")
rules_proto_dependencies()
rules_proto_toolchains()

load("@io_bazel_rules_scala//scala:toolchains.bzl", "scala_register_toolchains")
scala_register_toolchains()

# optional: setup ScalaTest toolchain and dependencies
load("@io_bazel_rules_scala//testing:scalatest.bzl", "scalatest_repositories", "scalatest_toolchain")
scalatest_repositories()
scalatest_toolchain()

RULES_JVM_EXTERNAL_TAG = "4.5"
RULES_JVM_EXTERNAL_SHA = "b17d7388feb9bfa7f2fa09031b32707df529f26c91ab9e5d909eb1676badd9a6"

http_archive(
    name = "rules_jvm_external",
    strip_prefix = "rules_jvm_external-%s" % RULES_JVM_EXTERNAL_TAG,
    sha256 = RULES_JVM_EXTERNAL_SHA,
    url = "https://github.com/bazelbuild/rules_jvm_external/archive/%s.zip" % RULES_JVM_EXTERNAL_TAG,
)

load("@rules_jvm_external//:repositories.bzl", "rules_jvm_external_deps")

rules_jvm_external_deps()

load("@rules_jvm_external//:setup.bzl", "rules_jvm_external_setup")

rules_jvm_external_setup()

load("@rules_jvm_external//:defs.bzl", "maven_install")

maven_install(
    artifacts = [
        "org.typelevel:cats-core_2.13:jar:2.4.1",
        "org.scalaz:scalaz-core_2.13:jar:7.3.0-M32",
        "org.specs2:specs2-core_2.13:jar:4.11.0",
        "org.specs2:specs2-matcher_2.13:jar:4.11.0",
        "org.specs2:specs2-mock_2.13:jar:4.11.0",
        "org.specs2:specs2-junit_2.13:jar:4.11.0",
        "org.specs2:specs2-analysis_2.13:jar:4.11.0",
        "org.specs2:specs2-common_2.13:jar:4.11.0",
        "org.specs2:specs2-fp_2.13:jar:4.11.0",
        "org.mockito:mockito-core:jar:3.9.0",
        "org.hamcrest:hamcrest-all:jar:1.3",
        "org.json4s:json4s-scalap_2.13:jar:4.0.3",
    ],
    repositories = [
        "https://jcenter.bintray.com/",
        "https://repo1.maven.org/maven2",
    ],
)
