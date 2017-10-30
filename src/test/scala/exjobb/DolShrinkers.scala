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

  @tailrec
  def scopeBfs(scope: Scope, usedVars: Set[Symbol]): Set[Symbol] = {
    val reachable = usedVars.flatMap{x =>
      NoFuture.allFreeVarsInType(scope(x))
    }
    val newUsedVars: Set[Symbol] = usedVars ++ reachable
    if (newUsedVars.size == usedVars.size)
      usedVars
    else
      scopeBfs(scope, newUsedVars)
  }

  def shrinkScopeIfPossible(scope: Scope, directlyUsedVars: Set[Symbol]): Scope = {
    val allUsedVars = scopeBfs(scope, directlyUsedVars)
    allUsedVars.map{x => (x, scope(x))}.toMap
  }

//  def shrinkInferenceProblemScopeIfPossible(problem: InferenceProblem): InferenceProblem = {
//    val directlyUsedVars = problem.scope.keys.filter{x =>
//      (NoFuture.isVarFreeInTerm(x, problem.term)
//        || NoFuture.isVarFreeInType(x, problem.prototype))
//    }.toSet
//    val newScope = shrinkScopeIfPossible(problem.scope, directlyUsedVars)
//    InferenceProblem(problem.term, problem.prototype, newScope, problem.expected)
//  }

//  def shrinkScope(problem: InferenceProblem): Stream[InferenceProblem] = {
//    val newProblem = shrinkInferenceProblemScopeIfPossible(problem)
//    if (newProblem.scope.size < problem.scope.size) Stream(newProblem) else Stream()
//  }

//  def shrinkSubtype(ctx: GlobalContext, scope: Scope, subtype: Type, typ: Type): Stream[(GlobalContext, Type)] = (subtype, typ) match {
//    case _ => Stream()
//  }
//
//  /** Make `supertype` "smaller" (in nodes) s.t. breaking `typ <: newSupertype`.
//   *
//   * Note this allows the case typ <: newSupertype <: supertype
//   * For example, if `typ == Bot`, then newSupertype can be any type.
//   *
//   *  Assumes `typ <: supertype`.
//   */
//  def shrinkSupertype(ctx: GlobalContext, scope: Scope, typ: Type, supertype: Type): Stream[(GlobalContext, Type)] = (typ, supertype) match {
//    case (Bot, FunType(x, xType, xResType)) =>
//      ((ctx, xType)
//        #:: (ctx.copy(globalScope = ctx.globalScope+(x -> xType)), xResType)
//        #:: (for {
//          (ctx2, newXType) <- shrinkSubtype(ctx, scope, xType, Top)
//        } yield (ctx2, FunType(x, newXType, xResType)): (GlobalContext, Type))
//        #::: (for {
//          (ctx2, newXResType) <- shrinkSupertype(ctx, scope, Bot, xResType)
//        } yield (ctx2, FunType(x, xType, newXResType)): (GlobalContext, Type)))
//    case (Bot, FieldDecl(a, aType)) =>
//      ((ctx, aType)
//        #:: (for {
//          (ctx2, newAType) <- shrinkSupertype(ctx, scope, Bot, aType)
//        } yield (ctx2, FieldDecl(a, newAType)): (GlobalContext, Type)))
//    case (Bot, TypeDecl(a, aLowerType, aUpperType)) =>
//      (//(ctx, aLowerType) #::
//        (ctx, aUpperType)
//        #:: (for {
//          (ctx2, newALowerType, newAUpperType) <- shrinkTypePair(ctx, scope, aLowerType, aUpperType)
//        } yield (ctx2, TypeDecl(a, newALowerType, newAUpperType)): (GlobalContext, Type)))
//    case (Bot, RecType(x, xType)) =>
//      (
//        (if (!NoFuture.allFreeVarsInType(xType).contains(x)) Stream((ctx, xType)) else Stream())
//        // TODO vs "isVarFreeInType then don't"? we may want to just shrinkSupertype to test eliminateVarUp, but probably not to test isVarFreeInType...
//        #::: (for {
//          (ctx2, newXType) <- shrinkSupertype(ctx, scope + (x -> xType), Bot, xType)
//        } yield (ctx2, RecType(x, xType)): (GlobalContext, Type)))
//    case (Bot, AndType(left, right)) =>
//      ((ctx, left)
//        #:: (ctx, right)
//        #:: (for {
//          (ctx2, newLeft) <- shrinkSupertype(ctx, scope, Bot, left)
//        } yield (ctx2, NoFuture.andType(newLeft, right)): (GlobalContext, Type))
//        #::: (for {
//          (ctx2, newRight) <- shrinkSupertype(ctx, scope, Bot, right)
//        } yield (ctx2, NoFuture.andType(left, newRight)): (GlobalContext, Type)))
//    case (Bot, TypeProj(x, a)) =>
//      ((ctx, NoFuture.typeProjectUpper(ctx.globalScope ++ scope, x, a).get)
//        #:: Stream())
//    case _ => Stream()
//  }
//
//  def shrinkTypePair(ctx: GlobalContext, scope: Scope, subtype: Type, supertype: Type): Stream[(GlobalContext, Type, Type)] =
//    (shrinkSupertype(ctx, scope, subtype, supertype).map{case (ctx2, supertype2) => (ctx2, subtype, supertype2)}
//      #::: shrinkSubtype(ctx, scope, subtype, supertype).map{case (ctx2, subtype2) => (ctx2, subtype2, supertype)})
//
//  sealed case class DefInferenceProblem(ctx: GlobalContext, d: Def, expected: TypedDef)
//
//  def shrinkInferenceProblem(ctx: GlobalContext, problem: InferenceProblem, isRoot: Boolean = false): Stream[(GlobalContext, InferenceProblem)] = {
//    var res = Stream[(GlobalContext, InferenceProblem)]()
//
//    // TODO use flatMap to make lazystreams?
//
//    res = res #::: (problem match {
//      case InferenceProblem(Var(_), _, _, _) => Stream()
//      case InferenceProblem(_, prototype, scope, expected) =>
//        val (ctx2, w) = ctx.withNewSymbol()
//        val expectedType = expected.assignedType
//        Stream((ctx2.withNewBinding(w -> expectedType), InferenceProblem(Var(w), Que, scope, Var(w).withType(expectedType))))
//      case _ => Stream()
//    })
//
////    res = res #::: (problem match {
////      case InferenceProblem(Obj(x, xType, defs), prototype, scope, expected) =>
////        // TODO replace some FieldDef(a, t) with FieldDef(a, Var(w))
////      case _ => Stream()
////    })
//
////    res = res #::: (problem match {
////      case InferenceProblem(App(x, y), prototype, scope, expected) =>
////        scope.get(x) match {
////          case Some(CanonicalFunType(z, zType, zResType, projs)) =>
////            val w = su.newSymbol()
////            Stream(InferenceProblem(Var(w), prototype, scope + (w -> zResType), Var(w).withType(zResType)))
////          case _ =>
////            Stream()
////        }
////      case _ => Stream()
////    })
//    res = res #::: (problem match {
//      case InferenceProblem(Let(x, xTerm, resTerm), prototype, scope, expected @ Let(_, expectedXTerm, expectedResTerm)) =>
//        val xProblem = (ctx, InferenceProblem(xTerm, Que, scope, expectedXTerm))
//        val resProblem = (ctx.withNewBinding(x -> expectedXTerm.assignedType), InferenceProblem(resTerm, prototype, scope, expectedResTerm))
//
//        val expectedType = problem.expectedType
//
//        val leftShrinks = for {
//          (ctx2, newXProblem) <- shrinkInferenceProblem(xProblem._1, xProblem._2)
//        } yield (ctx2, InferenceProblem(Let(x, newXProblem.term, resTerm), prototype, scope, Let(x, newXProblem.expected, expectedResTerm).withType(expectedType)))
//
//        val rightShrinks = for {
//          (ctx2, newResProblem) <- shrinkInferenceProblem(resProblem._1, resProblem._2)
//        } yield (ctx2, InferenceProblem(Let(x, xTerm, newResProblem.term), prototype, scope, Let(x, expectedXTerm, newResProblem.expected).withType(expectedType)))
//
//        if (isRoot) {
//          xProblem #:: resProblem #:: leftShrinks #::: rightShrinks
//        } else {
//          resProblem #:: rightShrinks
//        }
//      case _ => Stream()
//    })
//    res = res #::: (problem match {
//      case InferenceProblem(Fun(x, xType, resTerm), prototype, scope, expected @ Fun(_, _, expectedResTerm)) =>
//
//        val resPrototype = Que // TODO base on prototype
//        val resProblem = (ctx.withNewBinding(x -> xType), InferenceProblem(resTerm, resPrototype, scope, expectedResTerm))
//        val resShrinks = for {
//          // TODO shrink xType somehow? replace with sub-/supertype?
//          (ctx2, newResProblem) <- shrinkInferenceProblem(resProblem._1, resProblem._2)
//        } yield (ctx2, InferenceProblem(Fun(x, xType, newResProblem.term), prototype, scope, Fun(x, xType, newResProblem.expected).withType(problem.expectedType))) // TODO do something with prototype
//
//        if (isRoot)
//          resProblem #:: resShrinks
//        else
//          resShrinks
//      case _ => Stream()
//    })
//    // TODO remove function if root?
//    // TODO replace typ with subtype that does not reference x.
//    // TODO remove stuff in scope that are not referenced.
//    //if (res.size == 0)
//    //  shrinkScope(problem)
//    //else
//    res.map{case (ctx2, p2) => (ctx2, shrinkInferenceProblemScopeIfPossible(p2))}
//  }


  def shrinkDefInferenceProblem(ctx: GlobalContext, p: InferenceProblem.Def, localScope: Scope): Stream[(GlobalContext, InferenceProblem.Def)] = p match {
    case (IFieldDef(a, aTerm) :% proto :? typ) =>
      for {
        (ctx2, aTerm2) <- shrinkInferenceProblem(ctx, aTerm, isRoot=false, localScope)
      } yield (ctx2, IFieldDef(a, aTerm2) :% proto :? typ)
    case (ITypeDef(a, aType) :% proto :? typ) =>
      Stream()
    case (IAndDef(left, right) :% proto :? typ) =>
      val leftShrinks = for {
        (ctx2, left2)  <- shrinkDefInferenceProblem(ctx, left, localScope)
      } yield (ctx2, IAndDef(left2, right) :% proto :? typ)
      val rightShrinks = for {
        (ctx2, right2)  <- shrinkDefInferenceProblem(ctx, right, localScope)
      } yield (ctx2, IAndDef(left, right2) :% proto :? typ)
      leftShrinks #::: rightShrinks
  }

  def shrinkInferenceProblem(ctx: GlobalContext, p: InferenceProblem.Term, isRoot: Boolean, localScope: Scope = Map()): Stream[(GlobalContext, InferenceProblem.Term)] = {
    // TODO Handle prototypes properly!!!


    val reduced = p match {
      case (IObj(x, xType, defs) :% proto :? typ) =>
        val shrinkDefs = for {
          (ctx2, defs2) <- shrinkDefInferenceProblem(ctx, defs, localScope + (x -> xType))
        } yield (ctx2, IObj(x, xType, defs2) :% proto :? typ)

        // TODO also check xType for fields that can be removed
        val neededFields = NoFuture.directFieldDecls(ctx.globalScope ++ localScope + (x -> xType), x).keys.map{_._2}
        val haveFields = InferenceProblem.fieldnames(defs)
        val deadFields = haveFields -- neededFields
        val shrinkNumDefs = for {
          a <- deadFields.toStream
          defs2 <- InferenceProblem.remDef(defs, a)
        } yield (ctx, IObj(x, xType, defs2) :% proto :? typ)

        shrinkDefs #::: shrinkNumDefs
      case (IFun(x, xType, body) :% proto :? typ) =>
        if ((ctx.globalScope ++ localScope).contains(x)) ???
        for {
          (ctx2, body2) <- shrinkInferenceProblem(ctx, body, isRoot=false, localScope + (x -> xType))
        } yield (ctx2, IFun(x, xType, body2) :% proto :? typ)
      case (ILet(x, xTerm, resTerm) :% proto :? typ) =>
        val xTermShrinks = for {
          (ctx2, xTerm2) <- shrinkInferenceProblem(ctx, xTerm, isRoot=false, localScope)
        } yield (ctx2, ILet(x, xTerm2, resTerm) :% proto :? typ)
        val resTermShrinks = for {
          (ctx2, resTerm2) <- shrinkInferenceProblem(ctx, resTerm, isRoot=false, localScope + (x -> xTerm.typ))
        } yield (ctx2, ILet(x, xTerm, resTerm2) :% proto :? typ)
        xTermShrinks #::: resTermShrinks
      case _ => Stream()
    }

    val replaceWithVar = p match {
      case (IVar(_) :% proto :? _) => Stream()
      case (_ :% proto :? typ) =>
        if (NoFuture.allFreeVarsInType(typ).intersect(localScope.keySet).isEmpty) {
          val (ctx2, x) = ctx.withNewSymbol()
          val ctx3 = ctx2.withNewBinding(x -> typ)
          Stream((ctx3, IVar(x) :% proto :? typ))
        } else {
          Stream()
        }
    }

    val sub = if (isRoot) InferenceProblem.subproblems(ctx, p) else Stream()

    val res = replaceWithVar #::: sub #::: reduced

    if (isRoot && localScope != Map()) ???

    if (isRoot)
      res.map{case (ctx2, p2) =>
        val newGlobalScope = shrinkScopeIfPossible(ctx2.globalScope, InferenceProblem.freeVars(p2))
        // TODO Also shrink scope by substituting TypeProjs with the type they
        // are referencing?
        (ctx2.copy(globalScope=newGlobalScope), p2)
      }
    else
      res
  }

}
