addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "1.1.0")

//addSbtPlugin("org.playframework" % "sbt-plugin" % "3.0.5")

addCompilerPlugin(
  "org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full
)