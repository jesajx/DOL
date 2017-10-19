package exjobb

import Dol._

import scala.annotation.tailrec
import scala.util.control.NonFatal
import org.scalacheck.Properties
import org.scalacheck.Prop
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.Gen
import org.scalacheck.Gen.const
import org.scalacheck.Gen.oneOf
import org.scalacheck.Gen.someOf
import org.scalacheck.Gen.listOf
import org.scalacheck.Shrink

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Await
import scala.util.Try
import scala.concurrent.duration._

object ScalaCheckUtils {

  def splitSizeNonZero(size: Int, min: Int = 1): Gen[(Int, Int)] = {
    if (size < 2*min || size < 0 || min < 0)
      ??? // This indicates a bug in the caller's code.
    else for {
      left <- Gen.choose(min, size-min)
    } yield (left, size - left)
  }

  def timeoutProp[T](timeout: Duration)(f: => Prop): Prop =
    Prop.protect{
      Await.result(Future{Try{f}}, timeout).get
    }

  // TODO Maybe make a wrapper around Gen to keep track of "minsize"?
  // Alternatively: don't write so many generators....

  def oneOfGens[T](xs: Seq[Gen[T]]): Gen[T] = xs.size match {
    case 0 => ??? // should not happen.
    case 1 => xs(0)
    case _ => Gen.oneOf(xs(0), xs(1), xs.drop(2): _*)
  }


  val pprintTerminalWidth = 80
  val pprintSbtLogPrefix = "[info] "
  val pprintWidth = pprintTerminalWidth - pprintSbtLogPrefix.size
  val pprintHeight = 1000

  def prettyNamed[T](name: String, x: T): String = {
    val prefix = name + " = "
    prefix + pprint.apply(x, width=pprintWidth, height=pprintHeight, initialOffset=prefix.size).render
  }

  def pretty[T](x: T): String = pprint.apply(x, width=pprintWidth, height=pprintHeight).render

  def prettyProp[A](f: A => Prop)(x: A): Prop =
    prettyNamed("input", x) |: Prop.protect(f(x))
}
