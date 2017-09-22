name := "landscaper"

organization := "com.github.arthur-bit-monnot"

version := "0.1"

scalaVersion := "2.12.3"

scalacOptions ++= Seq(
  "-feature",
  "-language:higherKinds"
)

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.2",
  "com.lihaoyi" %% "utest" % "0.5.3" % "test"
)

testFrameworks += new TestFramework("utest.runner.Framework")

crossPaths := true

// To sync with Maven central
publishMavenStyle := true

// POM settings for Sonatype
homepage := Some(url("https://github.com/arthur-bit-monnot/landscaper"))
scmInfo := Some(ScmInfo(url("https://github.com/arthur-bit-monnot/landscaper"), "git@github.com:arthur-bit-monnot/landscaper.git"))
developers += Developer("abitmonn", "Arthur Bit-Monnot", "arthur.bit-monnot@laas.fr", url("https://github.com/arthur-bit-monnot"))
licenses += ("BSD-2-Clause", url("https://opensource.org/licenses/BSD-2-Clause"))