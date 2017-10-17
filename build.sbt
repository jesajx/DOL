import Dependencies._

enablePlugins(JmhPlugin)

lazy val dol = (project in file(".")).
  settings(
    name         := "exjobb",
    scalaVersion := "2.11.0",
    version      := "0.1.0-SNAPSHOT",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
    libraryDependencies += "com.lihaoyi" %% "pprint" % "0.5.3",
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "2"), // To print stacktraces in tests.
    scalacOptions := Seq("-unchecked", "-deprecation"),
    parallelExecution in Test := false
  ).
  dependsOn(reactiveAsync)


