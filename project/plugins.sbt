resolvers +=
  Resolver.url(
    "bintray-plugins",
    url("https://dl.bintray.com/resisttheurge/sbt-plugins/")
  )(Resolver.ivyStylePatterns)

addSbtPlugin("com.simplytyped" % "sbt-antlr4" % "0.8.1")