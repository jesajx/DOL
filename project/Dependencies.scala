import sbt._

object Dependencies {
  lazy val reactiveAsync = ProjectRef(uri("git://github.com/jesajx/reactive-async.git#master"), "core")
  //lazy val reactiveAsync = ProjectRef(uri("git:file:///home/jesajx/prg/reactive-async-joh#master"), "core")
}
