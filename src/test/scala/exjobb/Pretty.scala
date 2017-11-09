package exjobb

object Pretty {

  val pprintTerminalWidth = 78
  val pprintWidth = pprintTerminalWidth
  val pprintHeight = 100000

  object P extends pprint.PPrinter(defaultWidth=pprintWidth, defaultHeight=pprintHeight) {
    def named[T](name: String, x: T): String = {
      val prefix = name + " = "
      prefix + P(x, initialOffset=prefix.size).render
    }
    def namedln[T](name: String, x: T): Unit = {
      val prefix = name + " = "
      print(prefix)
      P.pprintln(x, initialOffset=prefix.size)
    }
  }
}
