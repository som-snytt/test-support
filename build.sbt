
scalaVersion := "2.11.0-M8"
//scalaVersion := "2.11.0-SNAPSHOT"
//scalaVersion := "2.11.0-20140115-161534-681308a3aa"

//scalaHome := Some(file("/home/apm/projects/snytt/build/pack"))
//scalaHome := Some(file("/home/apm/projects/edge/build/pack"))
//scalaHome := Some(file("/home/apm/clones/scala/build/pack"))

//logBuffered in Test := false

//resolvers += "Local Maven Repository" at "file:///home/apm/.m2/repository"

scalacOptions ++= Seq(/*"-Xdev",*/ /*"-Ymacro-debug-verbose",*/ "-deprecation", "-feature", "-Xlint", "-Xfatal-warnings", "-Xfuture")

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "com.novocode" % "junit-interface" % "0.10" % "test",
  "junit" % "junit" % "4.10"
)
