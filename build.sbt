name := "aoc24"
version := "0.1"

scalaVersion := "3.5.2"
scalacOptions ++= Seq("-explain", "-explain-types")

Compile / scalaSource := baseDirectory.value

fork := true
run / connectInput := true

libraryDependencies += "org.typelevel" %% "cats-core" % "2.12.0"
libraryDependencies += "org.typelevel" %% "cats-parse" % "1.0.0"
