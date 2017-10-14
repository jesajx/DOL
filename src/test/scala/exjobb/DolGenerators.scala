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

object DolGenerators {
  // (su, prototype) -> (term, scope, typedterm)

  /* // TODO better?
  def genTerm(su: SymbolUniverse, scope: Scope, prototype: Prototype): (Term, Scope, TypedTerm) = ???

  def genLowerType(su: SymbolUniverse, scope: Scope, prototype: Prototype): (Scope, Type) = ???
  def genUpperType(su: SymbolUniverse, scope: Scope, prototype: Prototype): (Scope, Type) = ???

  def genVarFromScopeAndType(su: SymbolUniverse, scope: Scope, exactPrototype: Type): Seq[(Term, Scope, TypedTerm)] = {
    if (NoFuture.isPrototype(exactPrototype)) ???
    val available = scope.filter{case (x, xType) => NoFuture.isSubtypeOf(scope, xType, exactPrototype)}
    available.map{case (x, xType) =>
      (Var(x), scope, Var(x).withType(exactPrototype))
    }.toSeq
  }

  def genVarFromThinAir(su: SymbolUniverse, scope: Scope, prototype: Prototype): (Term, Scope, TypedTerm) = {
    val x = su.newSymbol()
    val (newScope, xType) = genLowerType(su, scope, prototype)
    (Var(x), newScope + (x -> xType), Var(x).withType(xType))
  }

  def genLet(su: SymbolUniverse, scope: Scope, prototype: Prototype): (Term, Scope, TypedTerm) = {
    val x = su.newSymbol()
    val (xTerm, scope2, xTypedTerm) = genTerm(su, scope, Que)
    val (term, scope3, typedTerm) = genTerm(su, scope2 + (x -> xTypedTerm.assignedType), prototype)
    val letType = NoFuture.eliminateVarUp(scope + (x -> xTypedTerm.assignedType), x, typedTerm.assignedType, Set())
    (Let(x, xTerm, term), scope, Let(x, xTypedTerm, typedTerm).withType(letType))
  }

  def genFun(su: SymbolUniverse, scope: Scope, prototype: Prototype): (Term, Scope, TypedTerm) = {
    val x = su.newSymbol()
    val (argPrototype, resPrototype) = prototype match {
      case fun @ FunType(y, yPrototype, yResPrototype) => (yPrototype, NoFuture.typeRenameVar(y, x, yResPrototype))
      case Top                                         => (Bot, Top)
      case Que                                         => (Que, Que)
      case _ => ???; (ErrorType, ErrorType)
    }

    val (scope2, yType) = genUpperType(su, scope, argPrototype)

    val (term, scope3, typedTerm) = genTerm(su, scope + (x -> yType), resPrototype)

    val funType = FunType(x, yType, typedTerm.assignedType)

    // TODO if argPrototype is exact, then also generate LightFun(x, typedTerm).
    // TODO (LightFun(x, term), scope3 - x, LightFun(x, typedTerm).withType(funType))

    (Fun(x, yType, term), scope3 - x, Fun(x, yType, typedTerm).withType(funType))
  }
  */

}
