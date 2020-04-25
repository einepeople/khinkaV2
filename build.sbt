name := "khinkaV2"

version := "0.1"

scalaVersion := "2.13.1"

//scalacOptions += "-Ypartial-unification" // 2.11.9+

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

fork := true

libraryDependencies ++= Seq(
  "org.tpolecat" %% "doobie-core" % "0.8.8",
  "org.tpolecat" %% "doobie-h2" % "0.8.8",
  "org.tpolecat" %% "doobie-scalatest" % "0.8.8" % "test",
  "com.github.pureconfig" %% "pureconfig" % "0.12.3"
)
