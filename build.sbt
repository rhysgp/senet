enablePlugins(ScalaJSPlugin)

scalaJSStage in Global := FastOptStage

name := """senet"""

version := "1.0"

scalaVersion := "2.12.1"

scalaJSUseMainModuleInitializer := true

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.9.1",
  "org.singlespaced" %%% "scalajs-d3" % "0.3.4",
  "org.scala-js" %%% "scalajs-dom" % "0.9.1",

  "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0",
  "ch.qos.logback" % "logback-classic" % "1.1.7",

  "org.webjars" % "d3js" % "3.5.17",

  "org.specs2" %% "specs2-core" % "3.8.9" % "test"
)


resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

scalacOptions in Test ++= Seq("-Yrangepos")

// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.3.9"

jsDependencies += RuntimeDOM