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

object DolTypecheckingSpec extends Properties("DolTypecheckingSpec") { // TODO REM and use other specs directly instead? include other specs here?
  // TODO some ideas: <http://www.michaelburge.us/2017/09/27/delta-debugging-in-haskell.html>

  // TODO Include type sizes in Gen size?

  // TODO Maybe just define generators like normal function and let caller
  // wrap in Gen as necessary? The problem is distributing the randomseed
  // manually...







  //property("genCanonicalTypeSizeIsConsistentWithDolNotionOfSize") = { // TODO better name
  //  // TODO check that if we run a Gen[CanonicalType]-generator with size N,
  //  // the resulting generate type has _.totNumNodes <= N
  //}



  // TODO raise type to exact prototype (i.e. supertype of type)

//  property("NoFuture.raise -- simply fill in gaps") = { // TODO vs testing constraintSolver directly?
//    val su = new SymbolUniverse()
//    val scope: Scope = Map()
//    val generator: Gen[(Type, Prototype)] = for {
//      typ <- genType(su, scope)
//      prototype <- genUpperPrototypeFromType(su, scope, typ)
//    } yield (typ, prototype)
//
//    Prop.forAllNoShrink(generator) {case (typ, prototype) =>
//      val res = NoFuture.raise(su, scope, typ, prototype)
//      s"res = $res" |: Prop.all(res != None && NoFuture.equalTypes(scope, res.get, typ))
//    }
//  }
//
//  property("NoFuture.lower -- simply fill in gaps") = { // TODO vs testing constraintSolver directly?
//    val su = new SymbolUniverse()
//    val scope: Scope = Map()
//    val generator: Gen[(Type, Prototype)] = for {
//      typ <- genType(su, scope)
//      prototype <- genLowerPrototypeFromType(su, scope, typ)
//    } yield (typ, prototype)
//
//    Prop.forAllNoShrink(generator) {case (typ, prototype) =>
//      val res = NoFuture.lower(su, scope, typ, prototype)
//      s"res = $res" |: Prop.all(res != None && NoFuture.equalTypes(scope, res.get, typ))
//    }
//  }

  // TODO if x does not typecheck, then f(x) should not typecheck either.

  property("positiveSequentialInferenceProblem2") = {
    val generator: Gen[(GlobalContext, InferenceProblem.Term)] = for{
      ctx1  <- genGlobalScope()
      (ctx2, problem) <- genInferenceProblemFromPrototype(ctx1, Map(), Que)
    } yield (ctx2, problem)
    def shrink(tuple: (GlobalContext, InferenceProblem.Term)): Stream[(GlobalContext, InferenceProblem.Term)] = {
      val (ctx, problem) = tuple
      shrinkInferenceProblem(ctx, problem, isRoot=true)
    }
    Prop.forAllShrink(generator, shrink){ case (ctx, problem) =>
      val p = InferenceProblem.assemble(ctx, problem)
      prettyNamed("input", p) |: Prop.protect{
        val res = NoFuture.typecheckTerm(p.su(), p.term, p.prototype, p.scope)

        (prettyNamed("res", res) |:
          prettyNamed("eqcheck", eqcheck(p.scope, res, p.expected)) |:
          Prop.protect(
            NoFuture.equalTerms(p.scope, res, p.expected)
          )
        )
      }
    }
  }

  property("positiveSequentialInferenceProblem3") = {
    Prop.forAllNoShrink{(seed: Long) =>
      val g = genInferenceProblemFromPrototype(GlobalContext(), Map(), Que)(Gen.Parameters.default, Seed(seed))
      (g != None) ==> {
        val (ctx, problem) = g.get

        val p = InferenceProblem.assemble(ctx, problem)
        prettyNamed("input", p) |: Prop.protect{
          val res = NoFuture.typecheckTerm(p.su(), p.term, p.prototype, p.scope)

          (prettyNamed("res", res) |:
            prettyNamed("eqcheck", eqcheck(p.scope, res, p.expected)) |:
            Prop.protect(
              NoFuture.equalTerms(p.scope, res, p.expected)
            )
          )
        }
      }
    }
  }

  property("positiveParallelInferenceProblem") = {
    val generator: Gen[(GlobalContext, InferenceProblem.Term)] = for{
      ctx1  <- genGlobalScope()
      (ctx2, problem) <- genInferenceProblemFromPrototype(ctx1, Map(), Que)
    } yield (ctx2, problem)
    def shrink(tuple: (GlobalContext, InferenceProblem.Term)): Stream[(GlobalContext, InferenceProblem.Term)] = {
      val (ctx, problem) = tuple
      shrinkInferenceProblem(ctx, problem, isRoot=true)
    }
    Prop.forAllShrink(generator, shrink){ case (ctx, problem) =>
      val p = InferenceProblem.assemble(ctx, problem)
      prettyNamed("input", p) |: Prop.protect{
        val resOpt = typecheckInParallel(p.su(), p.term, p.prototype, p.scope)
        resOpt match {
          case None => prettyNamed("res", None) |: Prop.all(false)
          case Some(res) =>
            (prettyNamed("res", res)
              |: prettyNamed("eqcheck", eqcheck(p.scope, res, p.expected))
              |: Prop.all(NoFuture.equalTerms(p.scope, res, p.expected)))
        }
      }
    }
  }

  property("meh") = {
    val generator: Gen[(GlobalContext, InferenceProblem.Term)] = for{
      ctx1  <- genGlobalScope()
      (ctx2, problem) <- Gen.resize(100000, genInferenceProblemFromPrototype(ctx1, Map(), Que)) if InferenceProblem.assemble(ctx2, problem).term.totNumNodes >= 1000
    } yield (ctx2, problem)
    Prop.forAllNoShrink(generator){ case (ctx, problem) =>
      val p = InferenceProblem.assemble(ctx, problem)
      prettyNamed("input", p) |: Prop.protect{
        false
      }
    }
  }


  // TODO
//  property("positiveParallelInferenceProblem") = {
//    val su = new SymbolUniverse()
//    val generator: Gen[InferenceProblem] = genInferenceProblem(su, Map())
//    val shrink: InferenceProblem => Stream[InferenceProblem] = shrinkInferenceProblem(su, _, isRoot=true)
//    Prop.forAllShrink(generator, shrink)[prettyProp{(problem: InferenceProblem) =>
//      val res = typecheckInParallel(su, problem.term, problem.prototype, problem.scope)
//
//      val resString = res match {
//        case Some(resTerm) => s"res = Some(${NoFuture.stringExprWithTypeIfExists(resTerm)})"
//        case None => "res = None"
//      }
//
//      resString |: Prop.all(
//        res != None && NoFuture.equalTerms(su, res.get, problem.expected))
//    }}
//  }


  // TODO To be more FP-pure, the symboluniverse should probably be wrapped
  // together with other stuff in a monad...



}
