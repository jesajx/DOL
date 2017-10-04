import Dependencies._

lazy val root = (project in file(".")).
  settings(
    name         := "exjobb",
    scalaVersion := "2.11.0",
    version      := "0.1.0-SNAPSHOT",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
  ).
  dependsOn(reactiveAsync)


