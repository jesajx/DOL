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

object DolShrinkers {

  def decomposeType(su: SymbolUniverse, scope: Scope, typ: Type): Stream[(Scope, Type)] = typ match {
    case RecType(x, xType) =>
      // TODO shrinkScopeIfPossible
      Stream((scope + (x -> xType), xType))
    case FunType(x, xType, xResType) =>
      ((scope, xType)
        #:: (scope, xResType)
        #:: Stream()
        //#:: for { // TODO only if x not free in xResType? only if done in such away that everything xResType depends on is still in xType? // TODO replace TypeProjs with upper/lower bounds?
        //  newXType <- decomposeType(su, scope, xType)
        //} yield (scope, FunType(x, newXType, xResType))
        //#::: for {
        //  newXResType <- decomposeType(su, scope, xResType)
        //} yield (scope, FunType(x, xType, newXResType))
        )
    case _ => Stream()
  }

  def shrinkScopeIfPossible(problem: InferenceProblem): InferenceProblem = {

    def bfs(usedVars: Set[Symbol]): Set[Symbol] = {
      val reachable = usedVars.flatMap{x =>
        NoFuture.allFreeVarsInType(problem.scope(x))
      }
      val newUsedVars: Set[Symbol] = usedVars ++ reachable
      if (newUsedVars.size == usedVars.size)
        usedVars
      else
        bfs(newUsedVars)
    }

    val directlyUsedVars = problem.scope.keys.filter{x =>
      (NoFuture.isVarFreeInExpr(x, problem.term)
        || NoFuture.isVarFreeInType(x, problem.prototype))
    }.toSet
    val allUsedVars = bfs(directlyUsedVars)

    val newScope = allUsedVars.map{x => (x, problem.scope(x))}.toMap
    InferenceProblem(problem.term, problem.prototype, newScope, problem.expected)
  }

  def shrinkScope(problem: InferenceProblem): Stream[InferenceProblem] = {
    val newProblem = shrinkScopeIfPossible(problem)
    if (newProblem.scope.size < problem.scope.size) Stream(newProblem) else Stream()
  }

  def shrinkInferenceProblem(su: SymbolUniverse, problem: InferenceProblem, isRoot: Boolean = false): Stream[InferenceProblem] = {
    var res = Stream[InferenceProblem]()

    // TODO use flatMap to make lazystreams?

    res = res #::: (problem match {
      case InferenceProblem(Var(_), _, _, _) => Stream()
      case InferenceProblem(_, prototype, scope, expected) =>
        val w = su.newSymbol()
        val expectedType = expected.assignedType
        Stream(InferenceProblem(Var(w), Que, scope + (w -> expectedType), Var(w).withType(expectedType)))
      case _ => Stream()
    })

    //res = res #::: (problem match {
    //  case InferenceProblem(Obj(x, xType, defs), prototype, scope, expected) =>
    //    // TODO replace some FieldDef(a, t) with FieldDef(a, Var(w))
    //  case _ => Stream()
    //})

//    res = res #::: (problem match {
//      case InferenceProblem(App(x, y), prototype, scope, expected) =>
//        scope.get(x) match {
//          case Some(CanonicalFunType(z, zType, zResType, projs)) =>
//            val w = su.newSymbol()
//            Stream(InferenceProblem(Var(w), prototype, scope + (w -> zResType), Var(w).withType(zResType)))
//          case _ =>
//            Stream()
//        }
//      case _ => Stream()
//    })
    res = res #::: (problem match {
      case InferenceProblem(Let(x, xTerm, resTerm), prototype, scope, expected @ Let(_, expectedXTerm, expectedResTerm)) =>
        val xProblem = InferenceProblem(xTerm, Que, scope, expectedXTerm)
        val resProblem = InferenceProblem(resTerm, prototype, scope + (x -> expectedXTerm.assignedType), expectedResTerm)

        val expectedType = problem.expectedType

        val leftShrinks = for {
          newXProblem <- shrinkInferenceProblem(su, xProblem)
        } yield InferenceProblem(Let(x, newXProblem.term, resTerm), prototype, scope ++ newXProblem.scope, Let(x, newXProblem.expected, expectedResTerm).withType(expectedType))

        val rightShrinks = for {
          newResProblem <- shrinkInferenceProblem(su, resProblem)
        } yield InferenceProblem(Let(x, xTerm, newResProblem.term), prototype, scope ++ newResProblem.scope, Let(x, expectedXTerm, newResProblem.expected).withType(expectedType))

        if (isRoot) {
          xProblem #:: resProblem #:: leftShrinks #::: rightShrinks
        } else {
          resProblem #:: rightShrinks
        }
      case _ => Stream()
    })
    res = res #::: (problem match {
      case InferenceProblem(Fun(x, xType, resTerm), prototype, scope, expected @ Fun(_, _, expectedResTerm)) =>

        val resPrototype = Que // TODO base on prototype
        val resProblem = InferenceProblem(resTerm, resPrototype, scope + (x -> xType), expectedResTerm)
        val resShrinks = for {
          // TODO shrink xType somehow? replace with sub-/supertype?
          newResProblem <- shrinkInferenceProblem(su, resProblem)
        } yield InferenceProblem(Fun(x, xType, newResProblem.term), prototype, scope ++ newResProblem.scope, Fun(x, xType, newResProblem.expected).withType(problem.expectedType)) // TODO do something with prototype

        if (isRoot)
          resProblem #:: resShrinks
        else
          resShrinks
      case _ => Stream()
    })
    // TODO remove function if root?
    // TODO replace typ with subtype that does not reference x.
    // TODO remove stuff in scope that are not referenced.
    //if (res.size == 0)
    //  shrinkScope(problem)
    //else
    res.map{shrinkScopeIfPossible}
  }
}
