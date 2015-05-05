enablePlugins(ScalaJSPlugin)

scalaJSStage in Global := FastOptStage

name := """senet"""

version := "1.0"

scalaVersion := "2.11.6"

// Change this to another test framework if you prefer
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.8.0"

// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.3.9"

