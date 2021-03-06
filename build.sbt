import Dependencies._

enablePlugins(JmhPlugin)

lazy val dol = (project in file(".")).
  settings(
    name         := "exjobb",
    scalaVersion := "2.12.4",
    version      := "0.1.0-SNAPSHOT",
    //libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test", // conflicts with scalacheck.
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
    libraryDependencies += "com.lihaoyi" %% "pprint" % "0.5.3",
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "2"), // To print stacktraces in tests.
    scalacOptions := Seq("-unchecked", "-deprecation", "-feature")
    //parallelExecution in Test := false
  ).
  dependsOn(reactiveAsync)
