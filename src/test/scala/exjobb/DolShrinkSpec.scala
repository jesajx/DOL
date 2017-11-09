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
import org.scalacheck.rng.Seed

import DolGenerators._
import DolShrinkers._
import ScalaCheckUtils._

object DolShrinkSpec extends Properties("DolShrinkSpec") {

  // TODO
//  property("shrinkDoesNotThrow") = { // TODO
//    val generator: Gen[(SymbolUniverse, InferenceProblem)] = for {
//      ctx <- genGlobalScope()
//      (ctx2, problem) <- genInferenceProblem(ctx, Map())
//      su <- ctx2.toSymbolUniverse()
//    } yield (su, problem.copy(scope=problem.scope ++ ctx2.globalScope))
//    Prop.forAllNoShrink(generator) { case (su, problem) =>
//      try {
//        shrinkInferenceProblem(su, problem, isRoot=true)
//        true
//      } catch {
//        case NonFatal(e) =>
//          e.printStackTrace();
//          false
//      }
//    }
//  }
//
//  property("shrinkPreservesTypecorrectness") = { // TODO
//    val generator: Gen[(SymbolUniverse, InferenceProblem, InferenceProblem)] = for {
//      ctx <- genGlobalScope()
//      (ctx2, problem) <- genInferenceProblem(ctx, Map()) //if typecheckSequentially(su, problem.term, problem.prototype, problem.scope) != None
//      su <- ctx2.toSymbolUniverse()
//      shrunkProblems <- const(shrinkInferenceProblem(su, problem, isRoot=true)) if !shrunkProblems.isEmpty
//      newProblem <- oneOf(shrunkProblems)
//    } yield (su, problem, newProblem)
//    Prop.forAllNoShrink(generator){prettyProp{ case (su, origProblem, newProblem) =>
//      val origRes = typecheckSequentially(su, origProblem.term, origProblem.prototype, origProblem.scope)
//      val newRes = typecheckSequentially(su, origProblem.term, origProblem.prototype, origProblem.scope)
//      (prettyNamed("origRes", origRes)
//        |: prettyNamed("newRes", newRes)
//        |: Prop.all(origRes == newRes))
//    }}
//  }
  property("shrinkInferenceProblem does not throw") = {
    val generator: Gen[(GlobalContext, InferenceProblem.Term)] = for{
      ctx1  <- genGlobalScope()
      (ctx2, problem) <- genInferenceProblemFromPrototype(ctx1, Map(), Que)
    } yield (ctx2, problem)
    Prop.forAllNoShrink(generator){prettyProp{ case (ctx, problem) =>
      try {
        for (_ <- shrinkInferenceProblem(ctx, problem, isRoot=true)) {
          // just expand stream in case it is lazy
        }
        true
      } catch {
        case NonFatal(e) =>
          e.printStackTrace();
          false
      }
    }}
  }

  property("gen is well-formed") = {
    Prop.forAllNoShrink{ (seed: Long) =>
      Prop.protect{
        val opt = genInferenceProblemFromPrototype(GlobalContext(), Map(), Que)(Gen.Parameters.default, Seed(seed))
        !opt.isEmpty ==> {
          val (ctx, p) = opt.get
          Prop.all(
            InferenceProblem.wellFormed(ctx, p)
          )
        }
      }
    }
  }

}
