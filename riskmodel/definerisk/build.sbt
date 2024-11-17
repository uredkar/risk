val scala3Version = "3.5.2"
resolvers += "Akka library repository".at("https://repo.akka.io/maven")

//Compile / mainClass := Some("summonExample")
// runMain summonExample (this works)
val jarName = "definerisk.jar"
assembly/assemblyJarName := jarName

lazy val root = project
  .in(file("."))
  .settings(
    name := "definerisk",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,
    //assemblyOption in assembly := (assemblyOption in assembly).value.copy(includeScala = false) // Optional: Exclude Scala library from the JAR
    
    libraryDependencies +=
      ("com.typesafe.akka" %% "akka-actor" % "2.10.0").cross(CrossVersion.for3Use2_13),

    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test,
    
    //libraryDependencies += "com.typesafe.akka" %% "akka-http-spray-json" % "2.10.0",
    //libraryDependencies += "com.typesafe.akka" %% "akka-http" % "10.2.10",

    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % Test,
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.10.0"
  )
