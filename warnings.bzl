def fatal_warnings(scalacopts = []):
    """Chooses which scalac opts to use"""

    return select({
        "//:scalac_opts_fatal_warnings": ["-Xfatal-warnings", "-deprecation:false"],
        "//:scalac_opts_deprecations": ["-deprecation:true"],
        "//conditions:default": ["-Xfatal-warnings", "-deprecation:false"],
    }) + scalacopts
