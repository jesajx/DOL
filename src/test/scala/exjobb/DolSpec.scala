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

import DolGenerators._
import DolShrinkers._
import ScalaCheckUtils._

object DolSpec extends Properties("DolSpec") { // TODO REM and use other specs directly instead? include other specs here?

  def includeMatching(properties: Properties, regex: String): Unit = {
    for ((name, prop) <- properties.properties if name.matches(regex)) {
      property(name) = prop
    }
  }

  //includeMatching(DolTypecheckingSpec, ".*")
  //includeMatching(DolSubtypingSpec, ".*varRaise.*")
  //includeMatching(DolSubtypingSpec, ".*varLower.*")
  //includeMatching(DolSubtypingSpec, ".*varIsSubtypeOf.*")
  //includeMatching(DolSubtypingSpec, ".*leastCommonSupertype.*")
  //includeMatching(DolSubtypingSpec, ".*simplify.*")
  //includeMatching(DolSubtypingSpec, ".*"+java.util.regex.Pattern.quote("NoFuture.leastCommonSupertype -- a <: c, b <: c ==> lub(a, b) <: c"))
}
