import sbt.internal.ProjectMatrix

val scalaVer = "2.12.18"
//"3.6.1"
//val scalaVersion = "3.5.2"
val sparkVersion = "3.5.4"
val icebergVersion = "1.5.0"

ThisBuild / scalaVersion := scalaVer
ThisBuild / semanticdbEnabled := true
val inputDirectory = Def.settingKey[File]("")

//def sparkSqlDep(ver: String) =
//  ("org.apache.spark" %% "spark-sql" % ver).cross(CrossVersion.for3Use2_13)


//val munit = "org.scalameta" %% "munit" % "0.7.29"
ThisBuild / evictionErrorLevel := Level.Warn
//libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
libraryDependencies += "org.apache.spark" %% "spark-sql" % sparkVersion
libraryDependencies += "org.apache.spark" %% "spark-avro" % sparkVersion
libraryDependencies += "org.apache.spark" %% "spark-sql-kafka-0-10" % sparkVersion
libraryDependencies += "org.apache.spark" %% "spark-streaming-kafka-0-10" % sparkVersion
libraryDependencies += "org.apache.spark" %% "spark-streaming" % sparkVersion
libraryDependencies += "org.apache.kafka" % "kafka-clients" % "3.5.2"
libraryDependencies += "org.apache.parquet" % "parquet-hadoop" % "1.14.1"
libraryDependencies += "org.apache.spark" %% "spark-core" % sparkVersion
libraryDependencies += "org.apache.iceberg" % "iceberg-spark-runtime-3.5_2.12" % icebergVersion
//libraryDependencies += "org.apache.iceberg" %% "iceberg-spark-runtime-3.5" % "1.8.0" % "test"
dependencyOverrides += "org.apache.hadoop" % "hadoop-client" % "3.4.1" // Adjust versions
//libraryDependencies += "io.delta" %% "delta-spark" % sparkVersion
libraryDependencies += "io.delta" %% "delta-spark" % "3.3.0"
dependencyOverrides += "com.fasterxml.jackson.core" % "jackson-databind" % "2.15.0"
dependencyOverrides += "com.fasterxml.jackson.core" % "jackson-core" % "2.15.0"
