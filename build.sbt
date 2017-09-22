name := "landscaper"

organization := "com.github.arthur-bit-monnot"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.3"

scalacOptions ++= Seq(
  "-feature",
  "-language:higherKinds"
)

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.2",
  "com.lihaoyi" %% "utest" % "0.5.3" % "test",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

testFrameworks += new TestFramework("utest.runner.Framework")