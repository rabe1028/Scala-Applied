name := "scala-fp"

version := "0.1"

scalaVersion := "2.12.10"

scalacOptions in Global += "-language:experimental.macros"

scalastyleSources in Compile := (unmanagedSourceDirectories in Compile).value

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "org.mockito" % "mockito-core" % "2.21.0" % "test"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

scalacOptions ++= Seq("-encoding", "UTF-8")
