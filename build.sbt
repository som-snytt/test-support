
organization := "com.github.maqicode"

version := "0.1"

scalaVersion := "2.11.0-RC4"

scalacOptions ++= Seq(/*"-Xdev",*/ /*"-Ymacro-debug-verbose",*/ "-deprecation", "-feature", "-Xlint", "-Xfatal-warnings", "-Xfuture")

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "com.novocode" % "junit-interface" % "0.10" % "test",
  "junit" % "junit" % "4.10"
)
