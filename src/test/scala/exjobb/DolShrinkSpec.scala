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

object DolShrinkSpec extends Properties("DolShrinkSpec") {

  property("shrinkDoesNotThrow") = { // TODO
    val su = new SymbolUniverse()
    val generator = genInferenceProblem(su, Map())
    Prop.forAllNoShrink(generator) { (problem: InferenceProblem) =>
      try {
        shrinkInferenceProblem(su, problem, isRoot=true)
        true
      } catch {
        case NonFatal(e) =>
          e.printStackTrace();
          false
      }
    }
  }

  property("shrinkPreservesTypecorrectness") = { // TODO
    val su = new SymbolUniverse()
    val generator: Gen[(InferenceProblem, InferenceProblem)] = for {
      problem <- genInferenceProblem(su, Map()) //if typecheckSequentially(su, problem.term, problem.prototype, problem.scope) != None
      shrunkProblems <- const(shrinkInferenceProblem(su, problem, isRoot=true)) if !shrunkProblems.isEmpty
      newProblem <- oneOf(shrunkProblems)
    } yield (problem, newProblem)
    Prop.forAllNoShrink(generator) { (origAndShrunk: (InferenceProblem, InferenceProblem)) =>
      val (origProblem, newProblem) = origAndShrunk
      (s"newProblem = $newProblem"
        |: s"origProblem = $origProblem"
        |: Prop.all(
          (typecheckSequentially(su, origProblem.term, origProblem.prototype, origProblem.scope) != None)
          ==> (typecheckSequentially(su, newProblem.term, newProblem.prototype, newProblem.scope) != None)))
    }
  }
}
