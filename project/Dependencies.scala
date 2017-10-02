import sbt._

object Dependencies {
  lazy val reactiveAsync = ProjectRef(uri("git://github.com/phaller/reactive-async.git#master"), "core")
}
