import sbt.Keys._
//import play.sbt.PlaySettings
val scala3Version = "3.5.2"
resolvers += "Akka library repository".at("https://repo.akka.io/maven")

//Compile / mainClass := Some("summonExample")
// runMain summonExample (this works)
val jarName = "definerisk.jar"
assembly/assemblyJarName := jarName

val Versions = new {
  val circe = "0.14.9"
  val discipline = "1.7.0"
  val scalaCheck = "1.18.0"
  val scalaTest = "3.2.17"
  val scalaTestPlus = "3.2.18.0"
  val snakeYaml = "2.2"
  val snakeYamlEngine = "2.7"
  val previousCirceYamls = Set("0.14.0", "0.14.1", "0.14.2")

  val scala213 = "2.13.14"
  val scala3 = "3.3.3"

  val scalaVersions = Seq(scala213, scala3)
}

lazy val root = project
  .in(file("."))
  //.enablePlugins(PlayService, PlayLayoutPlugin, Common)
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
    libraryDependencies += "org.typelevel" %% "discipline-core" % Versions.discipline % Test,
    libraryDependencies += "org.scalacheck" %% "scalacheck" % Versions.scalaCheck % Test,
    libraryDependencies += "org.scalatestplus" %% "scalacheck-1-17" % Versions.scalaTestPlus % Test,
    libraryDependencies += "io.circe" %% "circe-core" % "0.14.5",
    libraryDependencies += "io.circe" %% "circe-yaml" % "0.14.2",
    libraryDependencies += "io.circe" %% "circe-generic" % "0.14.6",
    libraryDependencies += "io.circe" %% "circe-parser" % "0.14.6",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.10.0",
    libraryDependencies += "org.typelevel" %% "cats-free" % "2.10.0",
    libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.8"

  )
