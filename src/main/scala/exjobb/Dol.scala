package exjobb

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.CountDownLatch
import cell._
import lattice._
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global
import scala.util.control.NonFatal

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}
import scala.collection.mutable

// TODO move into "dol" package and call divide into multiple files?
object Dol {
  type Symbol = Int
  type SymbolPath = Seq[Symbol]

  class TypecheckingError(s: String = "error typechecking") extends Exception(s)

  // TODO when matching on a type, maybe always list them in the same order?

  // TODO INTERESTING IDEA:
  //
  //  Make scope global, x : T. (Local scope is a mapping into global scope).
  //
  //  Proving things in DOT is the same as x:T to x:D, for some type D.
  //  With COL this instead becomes the problem of raising x:T to x:P for some
  //  prototype P.
  //
  //  THIS FORMS A LATTICE.
  //
  //  Basically store (x, Prototype, Variance) in a cell, and then "refine" to (x, Type, Variance).
  //
  // The typechecker essentially creates symbols and prototypes.
  // Constraints are basically raising (x1,T1,v1) into (x2,P2,v2).
  //
  // possible premise
  //  x: T    (may not be fully known yet)
  // possible constraint
  //  z: D s.t. raise([x := z]T, P) = D
  //  z: D s.t. lower([x := z]T, P) = D
  //
  // if x:FunType(a#,b#) then a# and b# can be computed independently. This
  // may not be true if we instead have an AndType.
  //
  //


  // TODO Idea: Maybe treat entire typechecker as a constraint-solver and
  // typecheck in two steps: 1) gatherConstraints constraints, 2) solve them. This is
  // similar what I tried with Hindley-Milner. Technically this is actually
  // what is happening in the current implementation (gatherConstraints and solve are
  // done concurrently), but it could be beneficial to make it more explicit.

  // TODO Limitations of lattices.
  // It might be interesting to create something similar to LVars, but with
  // fewer restrictions. For example a cell of monoids.


  sealed trait Tree {
    val treeHeight: Int
    val totNumNodes: Int
    // TODO
  }
  sealed trait Expr extends Tree
  sealed trait Term  extends Expr
  sealed trait Value extends Term
  sealed trait Type  extends Tree
  sealed trait Def   extends Expr


  // Term ::=
  case class Var(x: Symbol) extends Term {
    val treeHeight = 1
    val totNumNodes = 1
  }
  case class App(x: Symbol, y: Symbol) extends Term {
    val treeHeight = 1
    val totNumNodes = 1
  }
  case class Let(x: Symbol, xTerm: Term, resTerm: Term) extends Term {
    val treeHeight = 1 + math.max(xTerm.treeHeight, resTerm.treeHeight)
    val totNumNodes = 1 + xTerm.totNumNodes + resTerm.totNumNodes
  }
  case class Sel(x: Symbol, a: Symbol) extends Term {
    val treeHeight = 1
    val totNumNodes = 1
  }

  // Value ::=
  case class Obj(x: Symbol, xType: Type, body: Def) extends Value {
    val treeHeight = 1 + math.max(xType.treeHeight, body.treeHeight)
    val totNumNodes = 1 + xType.totNumNodes + body.totNumNodes
  }
  case class Fun(x: Symbol, xType: Type, body: Term) extends Value {
    val treeHeight = 1 + math.max(xType.treeHeight, body.treeHeight)
    val totNumNodes = 1 + xType.totNumNodes + body.totNumNodes
  }

  // Def ::=
  case class FieldDef(a: Symbol, aTerm: Term) extends Def {
    val treeHeight = 1 + aTerm.treeHeight
    val totNumNodes = 1 + aTerm.totNumNodes
  }
  case class TypeDef(a: Symbol, aType: Type) extends Def {
    val treeHeight = 1 + aType.treeHeight
    val totNumNodes = 1 + aType.totNumNodes
  }
  case class AndDef(left: Def, right: Def)  extends Def {
    val treeHeight = 1 + math.max(left.treeHeight, right.treeHeight)
    val totNumNodes = 1 + left.totNumNodes + right.totNumNodes
  }

  // DOL Extensions to DOT // TODO
  //case class TSel(t: Term, a: Symbol) extends Term { // Let(x, t, Sel(x,a))
  case class TApp(t: Term, d: Def) extends Term { // Let(f, t, Let(y, Obj(x, <generated>, <generated>), App(f, y)))
    val treeHeight = 1 + d.treeHeight
    val totNumNodes = 1 + d.totNumNodes
  }
  //case class LightFun(x: Symbol, t: Term) extends Value // Fun(x, generated>, Term)
  //case class LightObj(x: Symbol, defs: Def) extends Value // Obj(x, <generated>, defs)
  //
  //case class ClassDef(typeName: Symbol, parent: Type, constructorName: Symbol, constructorParam: Symbol, constructorParamType: Type, self: Symbol, defs: Def) extends Def // C = RecType(x, defsType and parent)


  // Type ::=
  case object Bot extends Type {
    val treeHeight = 1
    val totNumNodes = 1
  }
  case object Top extends Type {
    val treeHeight = 1
    val totNumNodes = 1
  }
  case class TypeProj(x: Symbol, a: Symbol) extends Type {
    val treeHeight = 1
    val totNumNodes = 1
  }
  case class FunType(x: Symbol, xType: Type, resType: Type) extends Type {
    val treeHeight = 1 + math.max(xType.treeHeight, resType.treeHeight)
    val totNumNodes = 1 + xType.totNumNodes + resType.totNumNodes
  }
  case class RecType(x: Symbol, xType: Type) extends Type {
    val treeHeight = 1 + xType.treeHeight
    val totNumNodes = 1 + xType.totNumNodes
  }
  case class AndType(left: Type, right: Type) extends Type {
    val treeHeight = 1 + math.max(left.treeHeight, right.treeHeight)
    val totNumNodes = 1 + left.totNumNodes + right.totNumNodes
  }
  case class FieldDecl(a: Symbol, aType: Type) extends Type {
    val treeHeight = 1 + aType.treeHeight
    val totNumNodes = 1 + aType.totNumNodes
  }
  case class TypeDecl(a: Symbol, aLowerType: Type, aUpperType: Type) extends Type {
    val treeHeight = 1 + math.max(aLowerType.treeHeight, aUpperType.treeHeight)
    val totNumNodes = 1 + aLowerType.treeHeight + aUpperType.treeHeight
  }






  object Typed {
    type Term = Typed.Term.:-
    type Def  = Typed.Def.:-
    object Term {
      sealed case class :-(term: TypedTermLHS, typ: Type) {
        val treeHeight = 1 + math.max(term.treeHeight, typ.treeHeight)
        val totNumNodes = 1 + term.totNumNodes + typ.totNumNodes
      }
    }

    object Def {
      sealed case class :-(d: TypedDefLHS, typ: Type) {
        val treeHeight = 1 + math.max(d.treeHeight, typ.treeHeight)
        val totNumNodes = 1 + d.totNumNodes + typ.totNumNodes
      }
    }
  }

  sealed trait TypedTermLHS {
    def :-(typ: Type) = Typed.Term.:-(this, typ)
    val treeHeight: Int
    val totNumNodes: Int
  }
  sealed trait TypedDefLHS {
    def :-(typ: Type) = Typed.Def.:-(this, typ)
    val treeHeight: Int
    val totNumNodes: Int
  }

  case class TypedVar(x: Symbol)                                         extends TypedTermLHS {
    val treeHeight = 1
    val totNumNodes = 1
  }
  case class TypedApp(x: Symbol, y: Symbol)                              extends TypedTermLHS {
    val treeHeight = 1
    val totNumNodes = 1
  }
  case class TypedLet(x: Symbol, xTerm: Typed.Term, resTerm: Typed.Term) extends TypedTermLHS {
    val treeHeight = 1 + math.max(xTerm.treeHeight, resTerm.treeHeight)
    val totNumNodes = 1 + xTerm.totNumNodes + resTerm.totNumNodes
  }
  case class TypedSel(x: Symbol, a: Symbol)                              extends TypedTermLHS {
    val treeHeight = 1
    val totNumNodes = 1
  }
  case class TypedFun(x: Symbol, xType: Type, body: Typed.Term)          extends TypedTermLHS {
    val treeHeight = 1 + math.max(xType.treeHeight, body.treeHeight)
    val totNumNodes = 1 + xType.totNumNodes + body.totNumNodes
  }
  case class TypedObj(x: Symbol, xType: Type, body: Typed.Def)           extends TypedTermLHS {
    val treeHeight = 1 + math.max(xType.treeHeight, body.treeHeight)
    val totNumNodes = 1 + xType.totNumNodes + body.totNumNodes
  }

  case class TypedFieldDef(a: Symbol, aTerm: Typed.Term)    extends TypedDefLHS {
    val treeHeight = 1 + aTerm.treeHeight
    val totNumNodes = 1 + aTerm.totNumNodes
  }
  case class TypedTypeDef(a: Symbol, aType: Type)           extends TypedDefLHS {
    val treeHeight = 1 + aType.treeHeight
    val totNumNodes = 1 + aType.totNumNodes
  }
  case class TypedAndDef(left: Typed.Def, right: Typed.Def) extends TypedDefLHS {
    val treeHeight = 1 + math.max(left.treeHeight, right.treeHeight)
    val totNumNodes = 1 + left.totNumNodes + right.totNumNodes
  }

  case class TypedTApp(t: Typed.Term, d: Typed.Def) extends TypedTermLHS {
    val treeHeight = 1 + d.treeHeight
    val totNumNodes = 1 + d.totNumNodes
  }

  object :- {
    def apply(lhs: TypedTermLHS, rhs: Type): Typed.Term = lhs :- rhs
    def apply(lhs: TypedDefLHS, rhs: Type): Typed.Def = lhs :- rhs
    def unapply(t: Typed.Term): Option[(TypedTermLHS, Type)] = Some((t.term, t.typ))
    def unapply(t: Typed.Def): Option[(TypedDefLHS, Type)] = Some((t.d, t.typ))
  }















  sealed trait Variance
  case object Covariant extends Variance
  case object Contravariant extends Variance
  case object Invariant extends Variance
  case object ConstantVariance extends Variance
  // TODO RigidVariance?

  def reverseVariance(variance: Variance): Variance = variance match {
    case Covariant        => Contravariant
    case Contravariant    => Covariant
    case Invariant        => Invariant
    case ConstantVariance => ConstantVariance
  }

  def mergeVariance(a: Variance, b: Variance): Variance = (a, b) match {
    // same
    case (Covariant, Covariant)         => Covariant
    case (Contravariant, Contravariant) => Contravariant

    // opposites
    case (Covariant, Contravariant) => ???; Invariant // TODO can this happen in solveConstraint?
    case (Contravariant, Covariant) => ???; Invariant

    // stay still
    case (Invariant, _) => Invariant
    case (_, Invariant) => Invariant

    // whatever
    case (ConstantVariance, _) => b
    case (_, ConstantVariance) => a
  }

  def pairToList[T](pair: (T, T)): List[T] = List(pair._1, pair._2)
  def max(rest: Int*): Int = rest.toList.max

  sealed trait Constraint

  case object TrueConstraint extends Constraint
  case object FalseConstraint extends Constraint
  case class OrConstraint(left: Constraint, right: Constraint) extends Constraint
  //case class SubtypeConstraint(scope: Scope, left: Type, right: Type, variance: Variance) extends Constraint // TODO add Variance here?
  case class AndConstraint(left: Constraint, right: Constraint) extends Constraint
  type ConstraintMap = Map[TypeProj, (Scope, Type, Type)]
  case class MultiAndConstraint(constraints: ConstraintMap) extends Constraint // TODO replace with SubtypeConstraint?





  object NoFuture {


    // TODO implement typeProject upper/lower using raise/lower?

    // With x: T, T <: {a : L..U}, return U. Return None on error (e.g. if not
    // T <: {a = L..U} or if x is not in scope). If there are multiple
    // declarations of "a", return the greatestCommonSubtype of the upper bounds.
    // TODO allow callers to supply visitedSet?
    // TODO can we get rid of the SymbolUniverse somehow?
    def typeProjectUpper(scope: Scope, x: Symbol, a: Symbol, visited: Set[TypeProj] = Set()): Option[Type] = {
      // NOTE: The result may still reference the original TypeProjection.
      // E.g. upper(x.a) = y.b and upper(y.b) = x.a. But the caller may want
      // to see y.b. Therefore we stop there.
      def inner(scope: Scope, typ: Type, a: Symbol, visited: Set[TypeProj]): Option[Type] = typ match {
        case RecType(y, yType) =>
          inner(scope, typeRenameVar(y, x, yType), a, visited)
        case AndType(left, right) =>
          List(
            inner(scope, left, a, visited),
            inner(scope, right, a, visited)
          ).flatten.reduceOption{andType(_,_)}
        case bProj @ TypeProj(y, b) =>
          typeProjectUpper(scope, y, b).map{inner(scope, _, a, visited + bProj)}.flatten
        case TypeDecl(b, _, bUpperType) if a == b => Some(bUpperType)
        case Bot => Some(Bot) // TODO <: TypeDecl(a, x.a..x.a) <: Top?
        case _   => None
      }
      inner(scope, scope(x), a, visited + TypeProj(x, a))
    }

    def typeProjectLower(scope: Scope, x: Symbol, a: Symbol, visited: Set[TypeProj] = Set()): Option[Type] = {
      // NOTE: The result may still reference the original TypeProjection.
      // E.g. upper(x.a) = y.b and upper(y.b) = x.a. But the caller may want
      // to see y.b. Therefore we stop there.
      def inner(scope: Scope, typ: Type, a: Symbol, visited: Set[TypeProj]): Option[Type] = typ match {
        case RecType(y, yType) =>
          inner(scope, typeRenameVar(y, x, yType), a, visited)
        case AndType(left, right) =>
          List(
            inner(scope, left, a, visited),
            inner(scope, right, a, visited)
          ).flatten.reduceOption{leastCommonSupertype(scope, _,_)}
        case bProj @ TypeProj(y, b) =>
          typeProjectUpper(scope, y, b).map{inner(scope, _, a, visited + bProj)}.flatten
        case TypeDecl(b, bLowerType, _) if a == b => Some(bLowerType)
        case Bot => Some(Top) // TODO <: TypeDecl(a, x.a..x.a) <: Top?
        case _   => None
      }
      inner(scope, scope(x), a, visited + TypeProj(x, a))
    }

    def optionUnion[A <: C, B <: C, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
      (a, b) match {
        case (Some(aSome), Some(bSome)) => Some(f(aSome, bSome))
        case (Some(aSome), None)        => Some(aSome)
        case (None, Some(bSome))        => Some(bSome)
        case (None, None)               => None
      }
    }

    // Go grab field "a" in object "x".
    // Specifically: find T s.t. x: D, D <: {a: T}. If there are multiple
    // solutions return their intersection (AndType). On error return None.
    def select(su: SymbolUniverse, scope: Scope, x: Symbol, a: Symbol): Option[Type] = { // TODO REM?
      def inner(scope: Scope, x: Symbol, a: Symbol, visited: Set[TypeProj]): Option[Type] = scope.get(x) match {
        case Some(RecType(y, yType)) =>
          for {
            yAType <- inner(scope + (y -> yType), y, a, visited)
          } yield typeRenameVar(y, x, yAType) // TODO is this correct? will it not add meaning to yAType? Better to eliminate y?
        case Some(AndType(left, right)) =>
          val z = su.newSymbol()
          val leftUpperOption = inner(scope + (z -> left), z, a, visited)
          val rightUpperOption = inner(scope + (z -> right), z, a, visited)
          optionUnion(leftUpperOption, rightUpperOption){andType(_, _)}.map{typeRenameVar(z, x, _: Type)}
        case Some(bProj @ TypeProj(y, b)) if visited(bProj) =>
          None
        case Some(bProj @ TypeProj(y, b)) if !visited(bProj) =>
          for {
            bUpperType <- inner(scope, y, b, visited + bProj)
            z <- Some(su.newSymbol())
            aUpperType <- inner(scope + (z -> bUpperType), z, a, visited + bProj)
          } yield typeRenameVar(z, x, aUpperType)
        case Some(FieldDecl(b, bType)) if a == b => Some(bType)
        case Some(Bot) => Some(Bot) // since Bot <: {a: Bot}
        case Some(_) => None // TODO maybe just return Que instead of none?
        case None => ???; Some(ErrorType)
      }
      inner(scope, x, a, Set(TypeProj(x, a)))
    }

    def minType(scope: Scope, left: Type, right: Type): Option[Type] = ??? // Some(left) if left <: right else Some(right) if right <: left else None

    // TODO replace AndConstraint and OrConstraint with mergine constraints?
    // and({A <: X <: B}, {C <: X <: D}) = {max(A, C) <: X <: min(B, D)}
    // or({A <: X <: B}, {C <: X <: D}) = {min(A, C) <: X <: max(B, D)}?
    // Will glb=min and lub=max work?
    // min=AndType?

    /** Create a `Type` equivalent by subtyping to `AndType(left, right)`.
     *
     * This function helps simplify types in some simpler cases, e.g.
     * `andType(Bot, Bot) == Bot` rather than `AndType(Bot, Bot)`
     * since `Bot <: AndType(Bot, Bot)` and `AndType(Bot, Bot) <: Bot`.
     *
     * Be VERY careful about where this is called. The result of
     * rigidEqualTypes may differ depending on if AndType was used directly or
     * if this function was used.
     *
     */
    def andType(left: Type, right: Type): Type = (left, right) match {
      case (_, Top) => left
      case (Top, _) => right
      case (_, Bot) => Bot
      case (Bot, _) => Bot
      case (_, ErrorType) => ErrorType
      case (ErrorType, _) => ErrorType
      case _ => AndType(left, right)
    }

    def recType(x: Symbol, xType: Type): Type = xType match {
      case inner @ RecType(y, yType) => inner
      case _                         => RecType(x, xType)
    }

    def varEliminatingTransform(su: SymbolUniverse, scope: Scope, fromVar: Symbol, toVar: Symbol, typ: Type): Type = { // TODO REM?
      def inner(typ: Type, visited: Set[TypeProj]): Type = {
        typ match {
          case aProj @ TypeProj(x, a) if x == fromVar && typeProjectUpper(scope, toVar, a) == None =>
            typeProjectUpper(scope, x, a) match {
              case None => ErrorType
              case Some(aUpperType) =>
                inner(aUpperType, visited + aProj)
            }
          case aProj @ TypeProj(x, a) if x == fromVar =>
            TypeProj(toVar, a)
          case RecType(x, xType) if x != fromVar =>
            if (x == toVar) ??? // TODO Not supposed to happen (neither is x==fromVar, actually).
            recType(x, inner(xType, visited))
          case AndType(left, right) =>
            andType(
              inner(left, visited),
              inner(right, visited))
          case FunType(x, xType, xResType) if x != fromVar =>
            if (x == toVar) ??? // TODO Not supposed to happen (neither is x==fromVar, actually).
            val newXType = inner(xType, visited)
            val newXResType = inner(xResType, visited)
            FunType(x, xType, xResType)
          case FieldDecl(a, aType) =>
            FieldDecl(a, inner(aType, visited))
          case TypeDecl(a, aLowerType, aUpperType) =>
            TypeDecl(a, inner(aLowerType, visited), inner(aUpperType, visited))
          case _ => // Top, Bot, ErrorType, TypeProj(notFromVar, a)
            typ
        }
      }
      inner(typ, Set())
    }


    // TODO does lub and glb need to share visited? probably...

    // TODO reintroduce OrType? and have lub(a,b) = simplify(OrType(a, b))?
    def leastCommonSupertype(scope: Scope, lhs: Type, rhs: Type): Type = { // TODO do we need to unwrap rectypes here too?
      // TODO Exploit commutativity to make this shorter.
      // TODO Can we avoid using isSubtypeOf?
      def inner(scope: Scope, lhs: Type, rhs: Type, visitedLeft: Set[(TypeProj, Type)], visitedRight: Set[(Type, TypeProj)]): Type = {
        if (isSubtypeOf(scope, lhs, rhs)) // TODO inefficient?
          rhs
        else if (isSubtypeOf(scope, rhs, lhs))
          lhs
        else (lhs, rhs) match {
          case (Top, _) | (_, Top) => Top
          case (Bot, _) => rhs
          case (_, Bot) => lhs
          case (ErrorType, _) | (_, ErrorType) => ErrorType
          case (_: RecType, _: RecType) if (rigidEqualTypes(lhs, rhs)) => lhs

          case (AndType(l1, r1), AndType(l2, r2)) =>
            andType(
              andType(
                inner(scope, l1, rhs, visitedLeft, visitedRight),
                inner(scope, r1, rhs, visitedLeft, visitedRight)),
              andType(
                inner(scope, lhs, l2, visitedLeft, visitedRight),
                inner(scope, lhs, r2, visitedLeft, visitedRight)))

          case (AndType(left, right), bProj @ TypeProj(y, b))  =>
            val bUpperType = if (visitedRight((lhs, bProj))) Top else typeProjectUpper(scope, y, b).getOrElse{error()}
            andType(
              inner(scope, lhs, bUpperType, visitedLeft, visitedRight + ((lhs, bProj))),
              andType(
                inner(scope, left, rhs, visitedLeft, visitedRight),
                inner(scope, right, rhs, visitedLeft, visitedRight)))

          case (aProj @ TypeProj(x, a), AndType(left, right)) =>
            val aUpperType = if (visitedLeft((aProj, rhs))) Top else typeProjectUpper(scope, x, a).getOrElse{error()}
            andType(
              inner(scope, aUpperType, rhs, visitedLeft + ((aProj, rhs)), visitedRight),
              andType(
                inner(scope, lhs, left, visitedLeft, visitedRight),
                inner(scope, lhs, right, visitedLeft, visitedRight)))

          case (AndType(left, right), _) =>
            andType(
              inner(scope, left, rhs, visitedLeft, visitedRight),
              inner(scope, right, rhs, visitedLeft, visitedRight))

          case (_, AndType(left, right)) =>
            andType(
              inner(scope, lhs, left, visitedLeft, visitedRight),
              inner(scope, lhs, right, visitedLeft, visitedRight))

          case (TypeProj(x, a), TypeProj(y, b)) if lhs == rhs => lhs
          case (aProj @ TypeProj(x, a), bProj @ TypeProj(y, b)) if lhs != rhs =>
            val aUpperType = if (visitedLeft((aProj, bProj)))  Top else typeProjectUpper(scope, x, a).getOrElse{error()}
            val bUpperType = if (visitedRight((aProj, bProj))) Top else typeProjectUpper(scope, y, b).getOrElse{error()}

            andType(
              inner(scope, aUpperType, rhs, visitedLeft + ((aProj, bProj)), visitedRight),
              inner(scope, lhs, bUpperType, visitedLeft, visitedRight + ((aProj, bProj)))) // TODO is this correct?

          case (aProj @ TypeProj(x, a), _) =>
            val aUpperType = if (visitedLeft((aProj, rhs))) Top else typeProjectUpper(scope, x, a).getOrElse{error()}
            inner(scope, aUpperType, rhs, visitedLeft + ((aProj, rhs)), visitedRight)

          case (_, bProj @ TypeProj(y, b))  =>
            val bUpperType = if (visitedRight((lhs, bProj))) Top else typeProjectUpper(scope, y, b).getOrElse{error()}
            inner(scope, lhs, bUpperType, visitedLeft, visitedRight + ((lhs, bProj)))

          case (FunType(x, xType, xResType), FunType(y, yType, yResType)) if x != y =>
            if (doesShadow(scope + (x -> xType), yResType)) return Top // TODO hack.
            if (doesShadow(scope + (y -> xType), xResType)) return Top // TODO hack.

            andType( // TODO hack.
              inner(scope, lhs, typeRenameBoundVarAssumingNonFree(x, rhs), visitedLeft, visitedRight),
              inner(scope, typeRenameBoundVarAssumingNonFree(y, lhs), rhs, visitedLeft, visitedRight))

          case (FunType(x, xType, xResType), FunType(y, yType, yResType)) if x == y =>
            if (scope.contains(x)) ???
            val argType = andType(xType, yType)

            val resType = inner(scope + (x -> argType), xResType, yResType, visitedLeft, visitedRight)
            FunType(x, argType, resType)

          case (FieldDecl(a, aType), FieldDecl(b, bType)) if a == b =>
            FieldDecl(a, inner(scope, aType, bType, visitedLeft, visitedRight))
          case (TypeDecl(a, aLowerType, aUpperType), TypeDecl(b, bLowerType, bUpperType)) if a == b =>
            val commonLowerType = andType(aLowerType, bLowerType)
            val commonUpperType = inner(scope, aUpperType, bUpperType, visitedLeft, visitedRight)
            TypeDecl(a, commonLowerType, commonUpperType)

          case _ =>
            Top
        }
      }
      inner(scope, lhs, rhs, Set(), Set())
      //if (isSubtypeOf(scope, lhs, rhs)) // TODO inefficient?
      //  rhs
      //else if (isSubtypeOf(scope, rhs, lhs))
      //  lhs
      //else
      //  inner(scope, lhs, rhs, Set(), Set())
    }

    def greatestCommonSubtype(scope: Scope, left: Type, right: Type): Type = andType(left, right) // TODO always correct? // TODO simplify?

    // TODO isSubtypeOf(scope, a, b)?


    def mergeConstraints(left: ConstraintMap, right: ConstraintMap): ConstraintMap = {
      mapUnion(left, right){case ((s1, l1, u1), (s2, l2, u2)) =>
        //pprint.pprintln((left, right, s1, s2, mapIntersect(s1, s2){case (t1,t2) => (t1, t2)}.filter{case (x, (t1,t2)) => t1 != t2}), width=75, height=4000)
        if (mapIntersect(s1, s2){case (t1,t2) => (t1 == t2)}.exists{!_._2}) {
          //???
          (Map(), Top, Bot)
        }

        val commonScope = s1 ++ s2 // TODO will concatenating scopes work? If s1(x) == s2(x) and or "x" has otherwise been renamed... having different scopes seems wrong in the first place...
        //val commonScope = mapUnion(s1, s2){case (t1, t2) => andType(t1, t2)} // TODO This looks wrong... lub would probably make more sense, but for that to work we need the scope...
        (commonScope, leastCommonSupertype(commonScope, l1, l2), greatestCommonSubtype(commonScope, u1, u2))
      }
    }

    def mergeConstraintsOption(left: ConstraintMap, right: ConstraintMap): Option[ConstraintMap] = {
      val res = mergeConstraints(left, right)
      if (res.forall{case (p, (s, l, u)) => isSubtypeOf(s, l, u)})
        Some(res)
      else
        None
    }


    sealed trait SolveResult
    case class  Solution(solution: Type) extends SolveResult
    case object Inconsistent extends SolveResult // Meaning: There are multiple solutions, but they can not be merged. E.g. x.T=fun | x.T=obj.
    case object NoSolution extends SolveResult

    def orSolveResult(scope: Scope, r: Symbol, zOption: Option[Symbol], variance: Variance, left: SolveResult, right: SolveResult): SolveResult = (left, right) match {
        case (Inconsistent, _) | (_, Inconsistent) => Inconsistent
        case (Solution(leftType), Solution(rightType)) =>

          val leftSubRight = (None != raise(scope, r, zOption, leftType, rightType))
          val rightSubLeft = (None != raise(scope, r, zOption, rightType, leftType))

          //pprint.pprintln((leftType, rightType), width=78, height=40000000)
          //pprint.pprintln((leftSubRight, rightSubLeft))

          variance match {
            case Covariant =>
              if (leftSubRight)
                left
              else if (rightSubLeft)
                right
              else {
                Solution(leastCommonSupertype(scope, leftType, rightType)) // TODO wrong?
                //pprint.pprintln((leftType, rightType), width=78, height=40000000)
                //pprint.pprintln((leftSubRight, rightSubLeft))
                //Inconsistent
              }
            case Contravariant =>
              if (leftSubRight)
                right
              else if (rightSubLeft)
                left
              else {
                Solution(greatestCommonSubtype(scope, leftType, rightType)) // TODO wrong?
                //Inconsistent
              }
            //case Invariant =>
            //  if (leftSubRight && rightSubLeft)
            //    left
            //  else
            //    Inconsistent
            case _ => ???
          }
        case (Solution(leftType), _)  => Solution(leftType)
        case (_, Solution(rightType)) => Solution(rightType)
        case (NoSolution, NoSolution) => NoSolution
    }

    def orSolveResult2(scope: Scope, r: Symbol, zOption: Option[Symbol], variance: Variance, pattern: Type, left: SolveResult2, right: SolveResult2): SolveResult2 = (left, right) match {
        case (Inconsistent2, _) | (_, Inconsistent2) => Inconsistent2
        case (Solution2(m1), Solution2(m2)) =>

          val leftType = applyConstraintSolution(pattern, m1)
          val rightType = applyConstraintSolution(pattern, m2)
          val leftSubRight = (None != raise(scope, r, zOption, leftType, rightType))
          val rightSubLeft = (None != raise(scope, r, zOption, rightType, leftType))

          //pprint.pprintln((leftType, rightType), width=78, height=40000000)
          //pprint.pprintln((leftSubRight, rightSubLeft))

          variance match {
            case Covariant =>
              if (leftSubRight)
                left
              else if (rightSubLeft)
                right
              else {
                //Solution(leastCommonSupertype(scope, leftType, rightType)) // TODO wrong?
                //pprint.pprintln((leftType, rightType), width=78, height=40000000)
                //pprint.pprintln((leftSubRight, rightSubLeft))
                Inconsistent2
              }
            case Contravariant =>
              if (leftSubRight)
                right
              else if (rightSubLeft)
                left
              else {
                Inconsistent2
              }
            //case Invariant =>
            //  if (leftSubRight && rightSubLeft)
            //    left
            //  else
            //    Inconsistent
            case _ => ???
          }
        case (Solution2(m1), NoSolution2) => Solution2(m1)
        case (NoSolution2, Solution2(m2)) => Solution2(m2)
        case (NoSolution2, NoSolution2)   => NoSolution2
    }

    // TODO constraints and variance should be calculated separately...

    def solveConstraint(topScope: Scope, zOption: Option[Symbol], solveSet: Set[TypeProj], solveSetVariance: Map[TypeProj, Variance], constraint: Constraint, pattern: Type, topVariance: Variance): Option[Type] = {
      val defaults: ConstraintMap = solveSet.map(p => (p -> (Map(): Scope, Bot, Top))).toMap

      dnfStream(constraint).map{defaults ++ _}.map{res =>
        if (res.keys != defaults.keys) ???

        val badBounds = res.map{case (p, (scope, lower, upper)) =>
          val wOption = if (andTypeSeq(pattern).exists{_ == p}) zOption else None // TODO hack.
          // TODO allow zOption p is toplevel?
          //pprint.pprintln((p, scope, lower, upper))
          val lowerSubUpper = (None != raise(scope, -1, wOption, lower, upper))
          val upperSubLower = (None != raise(scope, -1, wOption, upper, lower))
          p -> (
            !lowerSubUpper
            || (solveSetVariance(p) == Invariant
              && !upperSubLower))
        }

        if (badBounds.exists{_._2}) {
          NoSolution
        } else {
          val mapping = res.map{case (proj, (scope, lower, upper)) =>
            val typ = solveSetVariance(proj) match {
              case Covariant        => lower
              case Contravariant    => upper
              case ConstantVariance => lower
              case Invariant        => lower
            }
            proj -> typ
          }
          Solution(applyConstraintSolution(pattern, mapping))
        }
      }.fold[SolveResult](NoSolution){
          orSolveResult(topScope, -1, zOption, topVariance, _, _)
          // TODO *WHY* did it work when minimizing contravariant? Will all
          // OR-alternatives always give the same result when matching or are
          // our tests insufficient???
      } match {
        case Solution(res) => Some(res)
        case Inconsistent  => throw new TypecheckingError("inconsistent"); None // TODO can this actually happen?
        case NoSolution    => None
      }
    }

    sealed trait SolveResult2
    case class  Solution2(m: Map[TypeProj, Type]) extends SolveResult2
    case object Inconsistent2 extends SolveResult2
    case object NoSolution2 extends SolveResult2

    def solveConstraint2(topScope: Scope, zOption: Option[Symbol], solveSet: Set[TypeProj], solveSetVariance: Map[TypeProj, Variance], constraint: Constraint, pattern: Type, topVariance: Variance): Option[Map[TypeProj, Type]] = {
      val defaults: ConstraintMap = solveSet.map(p => (p -> (Map(): Scope, Bot, Top))).toMap

      dnfStream(constraint).map{defaults ++ _}.map{res =>
        if (res.keys != defaults.keys) ???

        val badBounds = res.map{case (p, (scope, lower, upper)) =>
          val wOption = if (andTypeSeq(pattern).exists{_ == p}) zOption else None // TODO hack.
          // TODO allow zOption p is toplevel?
          //pprint.pprintln((p, scope, lower, upper))
          val lowerSubUpper = (None != raise(scope, -1, wOption, lower, upper))
          val upperSubLower = (None != raise(scope, -1, wOption, upper, lower))
          p -> (
            !lowerSubUpper
            || (solveSetVariance(p) == Invariant
              && !upperSubLower))
        }

        if (badBounds.exists{_._2}) {
          NoSolution2
        } else {
          val mapping = res.map{case (proj, (scope, lower, upper)) =>
            val typ = solveSetVariance(proj) match {
              case Covariant        => lower
              case Contravariant    => upper
              case ConstantVariance => lower
              case Invariant        => lower
            }
            proj -> typ
          }
          Solution2(mapping)
        }
      }.fold[SolveResult2](NoSolution2){
          orSolveResult2(topScope, -1, zOption, topVariance, pattern, _, _)
          // TODO *WHY* did it work when minimizing contravariant? Will all
          // OR-alternatives always give the same result when matching or are
          // our tests insufficient???
      } match {
        case Solution2(res) => Some(res)
        case Inconsistent2  => None
        case NoSolution2    => None
      }
    }




    def dnfStream(constraint: Constraint): Stream[ConstraintMap] = constraint match {
      case AndConstraint(left, right) =>
        for {
          a <- dnfStream(left)
          b <- dnfStream(right) // TODO this should be recomputed for each `a` in order to save memory. I.e. don't store the stream.
          z <- mergeConstraintsOption(a, b).toStream
        } yield z
      case OrConstraint(left, right) => dnfStream(left) #::: dnfStream(right)
      case TrueConstraint            => Stream(Map())
      case FalseConstraint           => Stream()
      case MultiAndConstraint(m)     => Stream(m)//Stream(mergeConstraintsOption(m, Map())).flatten
    }

    def dnf(constraint: Constraint): Constraint = constraint match {
      case OrConstraint(left, right)  => orConstraint(dnf(left), dnf(right))
      case AndConstraint(left, right) =>
        (dnf(left), dnf(right)) match {
          case (OrConstraint(leftDnf, rightDnf), other) => orConstraint(dnf(andConstraint(leftDnf, other)), dnf(andConstraint(rightDnf, other)))
          case (other, OrConstraint(leftDnf, rightDnf)) => orConstraint(dnf(andConstraint(other, leftDnf)), dnf(andConstraint(other, rightDnf)))
          case (leftNoOrs, rightNoOrs)                  => andConstraint(leftNoOrs, rightNoOrs)
        }
      case _ => constraint
    }

    def cnf(constraint: Constraint): Constraint = constraint match {
      case AndConstraint(left, right)  => andConstraint(cnf(left), cnf(right))
      case OrConstraint(left, right) =>
        (cnf(left), cnf(right)) match {
          case (AndConstraint(leftCnf, rightCnf), other) => andConstraint(cnf(orConstraint(leftCnf, other)), cnf(orConstraint(rightCnf, other)))
          case (other, AndConstraint(leftCnf, rightCnf)) => andConstraint(cnf(orConstraint(other, leftCnf)), cnf(orConstraint(other, rightCnf)))
          case (leftNoAnds, rightNoAnds)                 => orConstraint(leftNoAnds, rightNoAnds)
        }
      case _ => constraint
    }


    sealed case class ObjStdDecls(fields: Map[Symbol, Type], types: Map[Symbol, Type], hasBot: Boolean, hasTop: Boolean) { // TODO hasQue: Boolean
      def split(leftDom: Set[Symbol], rightDom: Set[Symbol]): (ObjStdDecls, ObjStdDecls) = {
        val leftFields = fields.filterKeys(leftDom)
        val rightFields = fields.filterKeys(rightDom)
        val leftTypes = types.filterKeys(leftDom)
        val rightTypes = types.filterKeys(rightDom)
        if (hasBot) ???
        (ObjStdDecls(leftFields, leftTypes, false, hasTop), ObjStdDecls(rightFields, rightTypes, false, hasTop))
      }

      def toType(): Prototype =
        if (hasBot)
          Bot
        else if (fields.size + types.size > 0)
          (fields.values ++ types.values).reduce{AndType(_, _)}
        else if (hasTop)
          Top
        else
          Que

      def dom: Set[Symbol] = fields.keySet ++ types.keySet
    }

    def objStdDecl(scope: Scope, x: Symbol, typ: Prototype): ObjStdDecls = {
      def inner(scope: Scope, x: Symbol, typ: Prototype, visited: Set[TypeProj]): ObjStdDecls = typ match {
        case RecType(y, yType) if x != y =>
          inner(scope, x, typeRenameBoundVarAssumingNonFree(x, typ), visited)
        case RecType(y, yType) if x == y =>
          inner(scope + (x -> yType), x, yType, visited)
        case FieldDecl(a, _)       => ObjStdDecls(Map(a -> typ), Map(), false, false)
        case TypeDecl(a, _, _)     => ObjStdDecls(Map(), Map(a -> typ), false, false)
        case proj @ TypeProj(y, a) =>
          inner(scope, x, typeProjectUpper(scope, y, a).get, visited + proj)
        case Bot => ObjStdDecls(Map(), Map(), true, false)
        case Top => ObjStdDecls(Map(), Map(), false, true)
        case Que => ObjStdDecls(Map(), Map(), false, false) // TODO add hasQue:Boolean to ObjStdDecls?
        case AndType(left, right) =>
          val leftStd = inner(scope, x, left, visited)
          val rightStd = inner(scope, x, right, visited)
          val allFields = mapUnion(leftStd.fields, rightStd.fields){andType(_, _)}
          val allTypes = mapUnion(leftStd.types, rightStd.types){andType(_, _)}
          ObjStdDecls(allFields, allTypes, leftStd.hasBot || rightStd.hasBot, leftStd.hasTop || rightStd.hasTop)
        case _ => ObjStdDecls(Map(), Map(), false, false)
      }

      val o = inner(scope, x, typ, Set())
      if (o.hasBot)
        ObjStdDecls(
          o.fields.map{case (a, _) => (a -> Bot)},
          o.types.map{case (a, _) => (a -> Bot)},
          o.hasBot,
          o.hasTop)
      else
        ObjStdDecls(
          o.fields.map{case (a, aType) => (a -> simplify(aType))},
          o.types,
          o.hasBot,
          o.hasTop)
    }

    def andConstraint(left: Constraint, right: Constraint): Constraint = (left, right) match {
      case (TrueConstraint, _)  => right
      case (_, TrueConstraint)  => left
      case (FalseConstraint, _) => FalseConstraint
      case (_, FalseConstraint) => FalseConstraint
      //case (MultiAndConstraint(m1), MultiAndConstraint(m2)) => MultiAndConstraint(mergeConstraints(m1, m2)) // TODO incorrect?
      case _ => AndConstraint(left, right)
    }

    def orConstraint(left: Constraint, right: Constraint): Constraint = (left, right) match {
      case (TrueConstraint, _)  => TrueConstraint
      case (_, TrueConstraint)  => TrueConstraint
      case (FalseConstraint, _) => right
      case (_, FalseConstraint) => left
      case _ => OrConstraint(left, right)
    }

    def applyConstraintSolution(typ: Type, solution: Map[TypeProj, Type]): Type = typ match {
      case proj: TypeProj if solution.contains(proj) => solution(proj)
      case AndType(left, right) =>
        AndType(applyConstraintSolution(left, solution), applyConstraintSolution(right, solution))
      case FunType(x, xType, resType) =>
        val newXType = applyConstraintSolution(xType, solution)
        val newResType = applyConstraintSolution(resType, solution)
        FunType(x, newXType, newResType)
      case RecType(x, xType) =>
        val newXType = applyConstraintSolution(xType, solution)
        RecType(x, newXType)
      case FieldDecl(a, aType) =>
        val newAType = applyConstraintSolution(aType, solution)
        FieldDecl(a, newAType)
      case TypeDecl(a, aLowerType, aUpperType) =>
        val newLowerType = applyConstraintSolution(aLowerType, solution)
        val newUpperType = applyConstraintSolution(aUpperType, solution)
        TypeDecl(a, newLowerType, newUpperType)
      case _ => typ
    }

    // TODO merge eliminateVars into gatherConstraints?

    /** Find the least supertype of `typ` such that `killSet` is not free.
     *
     * IMPORTANT: Assumes that `scope -- killSet` does not reference
     * `killSet`.
     *
     * Use `variance=Contravariante` to instead find the greatest subtype of
     * `typ` such that `killSet` is not free.
     *
     * If `typ` contains a `RecType` that can not be unwrapped (i.e. if
     * `zOption = None` or the `RecType` is deeply nested), it may be replaces
     * with Top or Bot as a whole.
     *
     * This is used when typechecking `Let`-expressions in order to make sure
     * that the variable introduced by the `Let` is not used when it goes out of
     * scope. For example if `Let(x, t1, t2)`, `t2: TypeProj(x, a)` and `x:
     * TypeDecl(a, A, B)`, then `Let(x, t1, t2): B`.
     */
    def eliminateVars(scope: Scope, killSet: Set[Symbol], zOption: Option[Symbol], typ: Type, variance: Variance = Covariant, visited: Set[TypeProj] = Set(), changedVars: Map[Symbol, Type] = Map()): Type = {
      //if (false && zOption != None && killSet(zOption.get) && allFreeVarsInType(typ).contains(zOption.get)) { // TODO bad?
      //  val z = zOption.get
      //  val zType = eliminateVars(scope, killSet - z, zOption, typ, variance, visited, changedVars) // TODO inner zOption = None?
      //  RecType(z, zType)
      //} else
        typ match {
        case aProj @ TypeProj(x, a) if killSet(x) =>
          val subst = variance match {
            case Covariant =>
              if (visited(aProj)) Top else typeProjectUpper(scope, x, a).get
            case Contravariant =>
              if (visited(aProj)) Bot else typeProjectLower(scope, x, a).get
            case _ => ??? // Not supposed to happen. // TODO vs just treat as if Covariant?
          }
          eliminateVars(scope, killSet, zOption, subst, variance, visited + aProj, changedVars)
        case aProj @ TypeProj(x, a) if changedVars.contains(x) =>
          variance match {
            case Covariant =>
              if (visited(aProj))
                Top
              else if (typeProjectUpper(scope, x, a) != None) // TODO hack. It is probably better to extend raise...
                typ
              else {
                val subst = typeProjectUpper(scope + (x -> changedVars(x)), x, a).get
                eliminateVars(scope, killSet, zOption, subst, variance, visited + aProj, changedVars)
              }
            case Contravariant =>
              if (visited(aProj))
                Bot
              else if (typeProjectLower(scope, x, a) != None) // TODO hack.
                typ
              else {
                val subst = typeProjectLower(scope + (x -> changedVars(x)), x, a).get
                eliminateVars(scope, killSet, zOption, subst, variance, visited + aProj, changedVars)
              }
            case _ => ??? // Not supposed to happen. // TODO vs just treat as if Covariant?
          }
        case AndType(left, right) =>
          val newLeft  = eliminateVars(scope, killSet, zOption, left, variance, visited, changedVars)
          val newRight = eliminateVars(scope, killSet, zOption, right, variance, visited, changedVars)
          AndType(newLeft, newRight)
        case FunType(x, xType, xResType) =>
          // TODO bug: if xType=RecType, newXType=Top, and xResType=TypeProj(x,_), then the result will not fulfill validTypeInScope.
          if (scope.contains(x)) ???
          val newXType    = eliminateVars(scope, killSet, None, xType, reverseVariance(variance), visited, changedVars)
          val newXResType = eliminateVars(scope + (x -> newXType), killSet - x, None, xResType, variance, visited, changedVars + (x -> xType))
          FunType(x, newXType, newXResType)

        case RecType(x, xType) =>
          zOption match {
            case Some(z) =>
              val newXType = eliminateVars(scope + (x -> xType), killSet - x, zOption, xType, variance, visited, changedVars) // TODO
              RecType(x, newXType)
            case None =>
              if (allFreeVarsInType(xType).intersect(killSet).isEmpty)
                typ
              else variance match {
                case Covariant     => Top
                case Contravariant => Bot
                case _ => ??? // Not supposed to happen. // TODO vs just treat as if Covariant?
              }
          }
        case FieldDecl(a, aType) =>
          val newAType = eliminateVars(scope, killSet, None, aType, variance, visited, changedVars)
          FieldDecl(a, newAType)
        case TypeDecl(a, aLowerType, aUpperType) =>
          val newALowerType = eliminateVars(scope, killSet, None, aLowerType, reverseVariance(variance), visited, changedVars)
          val newAUpperType = eliminateVars(scope, killSet, None, aUpperType, variance, visited, changedVars)
          TypeDecl(a, newALowerType, newAUpperType)
        case _ => typ
      }
    }

    // TODO invent error-reporting similar to line:col, but for arbitrary trees?

    def eliminateVarUpOLD(scope: Scope, z: Symbol, typ: Type): Type = {
      eliminateVars(scope, Set(z), None, typ)
    }

    def dom(d: Def): Set[Symbol] = d match {
      case FieldDef(a, _)      => Set(a)
      case TypeDef(a, _)       => Set(a)
      case AndDef(left, right) => dom(left) ++ dom(right)
    }

    def dom(d: Typed.Def): Set[Symbol] = d match {
      case (TypedFieldDef(a, _) :- _)      => Set(a)
      case (TypedTypeDef(a, _)  :- _)      => Set(a)
      case (TypedAndDef(left, right) :- _) => dom(left) ++ dom(right)
    }

    def domTypes(d: Def): Set[Symbol] = d match {
      case FieldDef(_, _)      => Set()
      case TypeDef(a, _)       => Set(a)
      case AndDef(left, right) => domTypes(left) ++ domTypes(right)
    }

    def replaceProjsWithQue(projsToReplace: Set[TypeProj], typ: Type): Type = typ match {
      case RecType(x, xType) =>
        if (projsToReplace.exists{case TypeProj(y, _) => x == y}) ??? // TODO should not happen?
        if (typeMonoidMap[Boolean](Map(), xType){_ || _}{
          case (_, proj: TypeProj, _) if projsToReplace(proj) => true
          case _ => false
        }) ???
        typ
      case FunType(x, xType, xResType) =>
        if (projsToReplace.exists{case TypeProj(y, _) => x == y}) ??? // TODO should not happen?
        FunType(x,
          replaceProjsWithQue(projsToReplace, xType),
          replaceProjsWithQue(projsToReplace, xResType))
      case AndType(left, right) =>
        AndType(
          replaceProjsWithQue(projsToReplace, left),
          replaceProjsWithQue(projsToReplace, right))
      case FieldDecl(a, aType) =>
        FieldDecl(a, replaceProjsWithQue(projsToReplace, aType))
      case TypeDecl(a, aLowerType, aUpperType) =>
        TypeDecl(a,
          replaceProjsWithQue(projsToReplace, aLowerType),
          replaceProjsWithQue(projsToReplace, aUpperType))
      case proj: TypeProj if projsToReplace(proj) => Que
      case _ => typ
    }

    def error() = {throw new TypecheckingError(); ErrorType}

    def typecheckDef(su: SymbolUniverse, d: Def, prototype: Prototype = Que, scope: Scope = Map()): Typed.Def = (d, prototype) match {
      case (AndDef(left, right), p) =>
        val leftDom = dom(left)
        val rightDom = dom(right)
        val overlap = leftDom.intersect(rightDom)
        if (!overlap.isEmpty)
          throw new TypecheckingError(s"overlapping doms: $overlap")

        val stdPrototype = objStdDecl(scope, -1, p) // NOTE: expect any rectypes to have been unwrapped.

        val missingDefs = stdPrototype.dom -- (leftDom ++ rightDom)
        if (!missingDefs.isEmpty)
          throw new TypecheckingError(s"missing defs: $missingDefs")


        val (leftStdPrototype, rightStdPrototype) = stdPrototype.split(leftDom, rightDom)
        val leftPrototype = leftStdPrototype.toType
        val rightPrototype = rightStdPrototype.toType

        val typedLeft  = typecheckDef(su, left, leftPrototype, scope)
        val typedRight = typecheckDef(su, right, rightPrototype, scope)
        // TODO raise?
        TypedAndDef(typedLeft, typedRight) :- simplify(AndType(typedLeft.typ, typedRight.typ))
      case (FieldDef(a, _), Que) =>
        typecheckDef(su, d, FieldDecl(a, Que), scope)
      case (FieldDef(a, aTerm), Top) =>
        val aTypedTerm = typecheckTerm(su, aTerm, Top, scope)
        TypedFieldDef(a, aTypedTerm) :- Top

      case (FieldDef(a, aTerm), FieldDecl(b, bPrototype)) if a == b =>
        val aTypedTerm = typecheckTerm(su, aTerm, bPrototype, scope)
        TypedFieldDef(a, aTypedTerm) :- simplify(FieldDecl(a, aTypedTerm.typ))

      case (TypeDef(a, aType), Top) =>
        TypedTypeDef(a, aType) :- Top
      case (TypeDef(a, aType), p) =>
        raise(scope, -1, None, TypeDecl(a, aType, aType), p) match {
          case Some(res) => TypedTypeDef(a, aType) :- simplify(res)
          case None => throw new TypecheckingError(s"fail raise(${TypeDecl(a, aType, aType)}, $p)")
        }

      //case (TypeDef(a, aType), p) =>
      //  ??? // TODO
      //case _ => ??? // complains about DefFuture.
    }

    def typecheckTerm(su: SymbolUniverse, term: Term, prototype: Prototype = Que, scope: Scope = Map()): Typed.Term = (term, prototype) match {
      case (Var(x), p) =>
        TypedVar(x) :- simplify(varRaise(scope, su.newSymbol(), x, p).get)
      case (Let(x, xTerm, resTerm), p) =>
        if (scope.contains(x)) ??? // TODO rename
        val typedXTerm   = typecheckTerm(su, xTerm, Que, scope)
        val typedResTerm = typecheckTerm(su, resTerm, p, scope + (x -> typedXTerm.typ))

        val zOption = typedResTerm match {
          case (TypedVar(z) :- _) => Some(z)
          case _ => None
        }
        val letType = eliminateVars(scope + (x -> typedXTerm.typ), Set(x), zOption, typedResTerm.typ)
        TypedLet(x, typedXTerm, typedResTerm) :- simplify(letType)
      case (Sel(x, a), p) =>
        TypedSel(x, a) :- simplify{
          typecheckTerm(su, Var(x), FieldDecl(a, p), scope).typ match {
            case FieldDecl(b, bType) if a == b => bType
            case _ => ???; ErrorType // TODO
          }
        }
      case (App(x, y), p) =>
        TypedApp(x, y) :- {
          val z = su.newSymbol()
          // TODO allow arg to constrain the function? i.e. overloading.
          typecheckTerm(su, Var(x), FunType(z, Que, Que), scope).typ match {
            case FunType(w, wType, wResType) =>
              val yType = typecheckTerm(su, Var(y), wType, scope).typ
              val yResType = typeRenameVar(w, y, wResType)
              raise(scope, su.newSymbol(), None, yResType, p).get
            case _ => // TODO handle Bot and other special cases.
              ???; ErrorType // TODO
          }
        }
      case (Fun(x, _, _), Que) =>
        typecheckTerm(su, term, FunType(x, Que, Que), scope)
      case (Fun(x, _, _), TypeProj(y, a)) =>
        val lower = typeProjectLower(scope, y, a).get
        val typedTerm = typecheckTerm(su, term, lower, scope)
        typedTerm.term :- prototype
      case (Fun(x, _, _), Top) =>
        val typedTerm = typecheckTerm(su, term, FunType(x, Bot, Top), scope)
        typedTerm.term :- Top
      case (Fun(x, _, _), FunType(y, _, _)) if x != y =>
        val (typedTerm :- typ) = typecheckTerm(su, term, typeRenameBoundVarAssumingNonFree(x, prototype), scope)
        typedTerm :- typeRenameBoundVarAssumingNonFree(y, typ)
      case (Fun(x, xType, resTerm), FunType(y, argPrototype, resPrototype)) if x == y =>
        // TODO Is it fine to do lower/raise of arg and res separately? Or is
        // it necessary to do one call to raise at the end using the whole
        // prototype?
        varLower(scope + (x -> xType), su.newSymbol(), x, argPrototype) match { // TODO bad to use (x -> xType) instead of (x -> yType)?
          case None =>
            throw new TypecheckingError(s"fail lower($x, $xType, $argPrototype)")
          case Some(loweredXType) =>
            val localScope = scope + (x -> xType)
            val typedResTerm = typecheckTerm(su, resTerm, resPrototype, localScope)
            TypedFun(x, xType, typedResTerm) :- simplify(FunType(x, loweredXType, typedResTerm.typ))
        }
      case (Obj(x, xType, defs), p) =>
        val localScope = scope + (x -> xType)

        val std = objStdDecl(localScope, x, xType)
        val typedDefs = typecheckDef(su, defs, std.toType, localScope)

        //if (p == Que) // TODO
        //  TypedObj(x, xType, typedDefs) :- RecType(x, xType)
        //else
        raise(scope, su.newSymbol(), None, RecType(x, xType), p) match {
          case Some(typ) =>
            TypedObj(x, xType, typedDefs) :- typ
          case None =>
            throw new TypecheckingError(s"fail raise($x, ${RecType(x, xType)}, $p)")
        }
      // TODO DOL extensions to DOT
      case (TApp(funTerm, argDefs), prototype) =>
        val funTypedTerm = typecheckTerm(su, funTerm, Que, scope)

        val f = su.newSymbol()
        val x = su.newSymbol()
        val funType = varRaise(scope + (f -> funTypedTerm.typ), -1, f, FunType(x, Que, Que)) match {
          case Some(funType) => funType
          case None    => throw new TypecheckingError(s"fail raise($f, ${funTypedTerm.typ}, ${FunType(x, Que, Que)})")
        }

        funType match {
          case FunType(w, wType, wResType) =>


            val o = objStdDecl(scope, x, wType)
            if (o.fields.isEmpty && o.types.isEmpty) ??? // TODO allow Top?

            val oClean = objStdDecl(scope + (x -> wType), x, eliminateVars(scope + (x -> wType), Set(x), None, o.toType, Contravariant))

            val typeArgs = domTypes(argDefs)
            val missingTypeArgs = o.types.keySet -- typeArgs

            val solveSet = missingTypeArgs.map{a => TypeProj(x, a)}.toSet
            val argPrototype = replaceProjsWithQue(solveSet, (oClean.fields ++ oClean.types.filterKeys(typeArgs)).values.reduce{AndType(_, _)}) // TODO or replace only missingTypeArgs with Que? // TODO what if only decls and no fields?
            val argTypedDefs = typecheckDef(su, argDefs, argPrototype, scope)


            val partialArgType = (List(argTypedDefs.typ) ++ missingTypeArgs.map{a => TypeDecl(a, TypeProj(x, a), TypeProj(x, a))}).reduce{AndType(_, _)}
            val lowerBound = simplify(partialArgType) // TODO better names
            val upperBound = simplify(oClean.toType)

            val constraint = gatherConstraints(scope + (x -> lowerBound), solveSet, Some(x), lowerBound, upperBound, patternIsLeft=false) // TODO add {a: Que..Que} to argTypedDefs forall missingTypeArgs? // TODO only solve fielddecl? // TODO manually gather constraints one fielddecl/typedecl at a time? // TODO what happens if typeparams depend on eachother?

            val solveSetVariance = gatherVariance(solveSet, wType, Covariant)
            val argType = solveConstraint(scope, Some(x), solveSet, solveSetVariance, constraint, upperBound, Covariant).map{simplify(_)} match {
              case Some(argType) => argType
              case None      => throw new TypecheckingError(s"fail solve($x, $lowerBound <: $upperBound)")
            }
            val resType = eliminateVars(scope + (x -> argType), Set(x), None, typeRenameVar(w, x, wResType))
            //pprint.pprintln(argPrototype)
            //pprint.pprintln(argType)
            //pprint.pprintln(resType)

            TypedTApp(funTypedTerm, argTypedDefs) :- simplify(resType)
          case _ => ??? // TODO bot, etc.
        }

      case _ =>
        ???
    }

    def typeRenameVar(fromVar: Symbol, toVar: Symbol, typ: Type): Type = typ match {
      case TypeProj(x, a) if x == fromVar =>
        TypeProj(toVar, a)
      case FunType(x, xType, resType) if x != fromVar =>
        FunType(x,
          typeRenameVar(fromVar, toVar, xType),
          typeRenameVar(fromVar, toVar, resType))
      case RecType(x, xType) if x != fromVar =>
        RecType(x, typeRenameVar(fromVar, toVar, xType))
      case FieldDecl(a, aType) =>
        FieldDecl(a, typeRenameVar(fromVar, toVar, aType))
      case TypeDecl(a, aLowerType, aUpperType) =>
        TypeDecl(a,
          typeRenameVar(fromVar, toVar, aLowerType),
          typeRenameVar(fromVar, toVar, aUpperType))
      case AndType(left, right) =>
        AndType( // Instead of andType(), to make it exactly equal if fromVar == toVar.
          typeRenameVar(fromVar, toVar, left),
          typeRenameVar(fromVar, toVar, right))
      case _ =>
        typ
    }

    def typeRenameBoundVarAssumingNonFree(toVar: Symbol, typ: Type): Type = {
      if (isVarFreeInType(toVar, typ)) ??? // TODO Caller's responsibility?
      typ match {
        case RecType(x, xType) if x != toVar =>
          RecType(toVar, typeRenameVar(x, toVar, xType))
        case FunType(x, xType, resType) if x != toVar =>
          FunType(toVar, xType, typeRenameVar(x, toVar, resType))
        case _ =>
          typ
      }
    }

    def termRenameBoundVarAssumingNonFree(toVar: Symbol, term: Term): Term ={
      if (isVarFreeInTerm(toVar, term)) ???
      term match {
        case Let(x, xTerm, resTerm) if x != toVar => termRenameVar(x, toVar, Let(toVar, xTerm, resTerm))
        case Obj(x, xType, d)       if x != toVar => termRenameVar(x, toVar, Obj(toVar, xType, d))
        case Fun(x, xType, resTerm) if x != toVar => termRenameVar(x, toVar, Fun(toVar, xType, resTerm))
        case _ => term
      }
    }

    def defRenameVar(fromVar: Symbol, toVar: Symbol, d: Def): Def = d match {
      case FieldDef(a, aTerm) =>
        FieldDef(a, termRenameVar(fromVar, toVar, aTerm))
      case TypeDef(a, aType) =>
        TypeDef(a, NoFuture.typeRenameVar(fromVar, toVar, aType))
      case AndDef(left, right) =>
        AndDef(
          defRenameVar(fromVar, toVar, left),
          defRenameVar(fromVar, toVar, right))
      case _ => d
    }

    def termRenameVar(fromVar: Symbol, toVar: Symbol, e: Term): Term = e match {
      case Var(x) if x == fromVar => Var(toVar)
      case App(x, y) if x == fromVar || y == fromVar =>
        val newX = if (x == fromVar) toVar else x
        val newY = if (y == fromVar) toVar else y
        App(newX, newY)
      case Let(x, xTerm, t) if x != fromVar =>
        Let(x, termRenameVar(fromVar, toVar, xTerm), termRenameVar(fromVar, toVar, t))
      case Sel(x, a) if x == fromVar =>
        Sel(toVar, a)
      case Obj(x, xType, d) if x != fromVar =>
        Obj(x,
          typeRenameVar(fromVar, toVar, xType),
          defRenameVar(fromVar, toVar, d))
      case Fun(x, xType, t) if x != fromVar =>
        Fun(x,
          typeRenameVar(fromVar, toVar, xType),
          termRenameVar(fromVar, toVar, t))
      case _ => e
    }

    def defAsMap(d: Def): Map[Symbol, Def] = d match {
      case FieldDef(a, _) => Map(a -> d)
      case TypeDef(a, _) => Map(a -> d)
      case AndDef(left, right) => defAsMap(left) ++ defAsMap(right)
      case _ => Map()
    }

    def defHasDuplicates(d: Def): Boolean = {
      def inner(d: Def, counts: (Map[Symbol, Int], Map[Symbol, Int])): (Map[Symbol, Int], Map[Symbol, Int]) = d match {
        case AndDef(left, right) =>
          inner(right, inner(left, counts))
        case FieldDef(a, _) =>
          val (fieldCounts, typeCounts) = counts
          (fieldCounts.updated(a, fieldCounts(a) + 1), typeCounts)
        case TypeDef(a, _) =>
          val (fieldCounts, typeCounts) = counts
          (fieldCounts, typeCounts.updated(a, typeCounts(a) + 1))
        case _ => ??? // complains about DefFuture
      }
      val (fieldCounts, typeCounts) = inner(d, (Map().withDefaultValue(0), Map().withDefaultValue(0)))
      fieldCounts.values.exists{_ >= 2} || typeCounts.values.exists{_ >= 2}
    }

    def isPrototype(t: Type): Boolean = t match {
      case Que => true
      case FunType(_, argType, resType) =>
        isPrototype(argType) || isPrototype(resType)
      case RecType(x, xType) =>
        isPrototype(xType)
      case FieldDecl(a, aType) =>
        isPrototype(aType)
      case TypeDecl(a, aLowerType, aUpperType) =>
        isPrototype(aLowerType) || isPrototype(aUpperType)
      case AndType(left, right) =>
        isPrototype(left) || isPrototype(right)
      case _ => false
    }


    //def exactEqualTypes(first: Type, second: Type) = (first == second)


    /** Check if `first` and `second` are equal.
     *
     * The difference from `first == second` is that all assigned types
     * (including those of nested defs) are compared like:
     * `rigidEqualTypes(first.typ, second.typ)`.
     */
    def equalDefs(scope: Scope, first: Typed.Def, second: Typed.Def): Boolean = {
      val (leftDef :- leftType)   = first
      val (rightDef :- rightType) = second
      val typesAreEqual = equalTypes(scope, leftType, rightType)
      val defsAreEqual = (leftDef, rightDef) match {
        case (TypedAndDef(l1, r1), TypedAndDef(l2, r2)) =>
          (equalDefs(scope, l1, l2) && equalDefs(scope, r1, r2))
        case (TypedFieldDef(a, aTerm), TypedFieldDef(b, bTerm)) =>
          (a == b && equalTerms(scope, aTerm, bTerm))
        case _ => // TypedTypeDef and mismatched
          (leftDef == rightDef)
      }
      (defsAreEqual && typesAreEqual)
    }

    /** Check if `first` and `second` are equal.
     *
     * The difference from `first == second` is that all assigned types
     * (including those of nested terms) are compared like:
     * `rigidEqualTypes(first.typ, second.typ)`.
     * Any other types (e.g. `T` in `Typed.Fun(x, T, res) :- F`),
     * are compared with `==`.
     *
     * This function is mainly used for testing `typecheckTerm` and (i.e. the
     * input term is not "changed", only annotate with a type).
     *
     */
    def equalTerms(scope: Scope, first: Typed.Term, second: Typed.Term): Boolean = {
      val (leftTerm :- leftType) = first
      val (rightTerm :- rightType) = second
      val theTypesAreEqual = equalTypes(scope, leftType, rightType)
      val theTermsAreEqual = (leftTerm, rightTerm) match {
        case (TypedLet(x, xTerm, xResTerm), TypedLet(y, yTerm, yResTerm)) =>
          (x == y
            && equalTerms(scope, xTerm, yTerm)
            && equalTerms(scope + (x -> xTerm.typ), xResTerm, yResTerm))

        case (TypedObj(x, xType, xBody), TypedObj(y, yType, yBody)) =>
          (x == y
            && xType == yType
            && equalDefs(scope + (x -> xType), xBody, yBody))

        case (TypedFun(x, xType, xBody), TypedFun(y, yType, yBody)) =>
          (x == y
            && xType == yType
            && equalTerms(scope + (x -> xType), xBody, yBody))

        case (TypedTApp(leftFunTerm, leftArgDefs), TypedTApp(rightFunTerm, rightArgDefs)) =>
          (equalTerms(scope, leftFunTerm, rightFunTerm)
            && equalDefs(scope, leftArgDefs, rightArgDefs))

        case _ => // Var, App, Sel and mismatched
          (leftTerm == rightTerm)
      }
      (theTermsAreEqual && theTypesAreEqual)
    }

    // TODO def hasNestedRecursiveTypes(scope: Scope, x: Symbol)?
    // TODO def validType?
    // TODO scope should probably have an invariant that all contained types
    // are "valid" for some sense of valid.

    /** Get rid of any (RecType(y, yType)) in typ by renaming y to x.
     * Only searches immediate AndTypes. Any nested RecTypes are ignored.
     */
    def unwrapRecTypes(typ: Type, x: Symbol): Type = typ match {
      case RecType(y, yType) =>
        // TODO do rename and rectype-elimination at the same time. i.e. pass
        // down set of symbols equivalent to x?
        // TODO Alternatively do all renames at end? renameAll(set(y,z,w) -> x)
        unwrapRecTypes(typeRenameVar(y, x, yType), x) // NOTE: it is fine for x to be free in yType.
      case AndType(left, right) =>
        AndType(
          unwrapRecTypes(left, x),
          unwrapRecTypes(right, x))
      case typ => typ
    }

    // TODO Should there be a fixRecursiveTypeProjections? No? There may e.g.
    // be circular references between TypeProjs and that info may be necessary
    // in some cases. For example, probably can't rewrite:
    //    {x.T: x.D..x.D}&{x.D: x.T..x.T}.

    def prepMatch(r: Symbol, prototype: Prototype, count: Int = 0): (Int, Type) = {
      def inner(count: Int, prototype: Prototype): (Int, Type) = prototype match {
        case RecType(x, xType) =>
          val (count2, xType2) = inner(count, xType)
          (count2, RecType(x, xType2))
        case Que =>
          val proj = TypeProj(r, count)
          (count + 1, proj)
        case AndType(left, right) =>
          val (count2, leftType)   = inner(count, left)
          val (count3, rightType) = inner(count2, right)
          // IMPORTANT: We MUST use AndType (case class) and not andType
          // (function) here. The second one optimizes e.g. andType(Bot, _) =
          // Bot. This causes problems if we need to call rigidEqualTypes
          // later.
          (count3, AndType(leftType, rightType))
        case FunType(x, xType, xResType) =>
          val (count2, newXType)    = inner(count, xType)
          val (count3, newXResType) = inner(count2, xResType)
          (count3, FunType(x, newXType, newXResType))
        case FieldDecl(a, aType) =>
          val (count2, newAType) = inner(count, aType)
          (count2, FieldDecl(a, newAType))
        case TypeDecl(a, aLowerType, aUpperType) =>
          val (count2, newLowerType)  = inner(count, aLowerType)
          val (count3, newUpperType) = inner(count2, aUpperType)
          (count3, TypeDecl(a, newLowerType, newUpperType))
        case _ => (count, prototype)
      }
      inner(count, prototype)
    }


    def typeTransform(typ: Type, variance: Variance = Covariant)(f: (Type, Variance) => Option[Type]): Type = {
      f(typ, variance) match {
        case Some(t) => t
        case None => typ match {
          case FunType(x, xType, xResType) =>
            FunType(x,
              typeTransform(xType, reverseVariance(variance))(f),
              typeTransform(xResType, variance)(f))
            case AndType(left, right) =>
              AndType(
                typeTransform(left, variance)(f),
                typeTransform(right, variance)(f))
            case TypeDecl(a, aLowerType, aUpperType) =>
              TypeDecl(a,
                typeTransform(aLowerType, reverseVariance(variance))(f),
                typeTransform(aUpperType, variance)(f))
            case RecType(x, xType) =>
              RecType(x, typeTransform(xType, variance)(f))
            case FieldDecl(a, aType) =>
              FieldDecl(a, typeTransform(aType, variance)(f))
            case _ => typ
        }
      }
    }

    def gatherVariance(solveSet: Set[TypeProj], typ: Type, variance: Variance): Map[TypeProj, Variance] = typ match {
      case FunType(x, xType, xResType) =>
        mapUnion(
          gatherVariance(solveSet, xType, reverseVariance(variance)),
          gatherVariance(solveSet,xResType, variance)
        ){mergeVariance(_, _)}
      case AndType(left, right) =>
        mapUnion(
          gatherVariance(solveSet, left, variance),
          gatherVariance(solveSet, right, variance)
        ){mergeVariance(_, _)}
      case TypeDecl(a, aLowerType, aUpperType) =>
        mapUnion(
          gatherVariance(solveSet, aLowerType, reverseVariance(variance)),
          gatherVariance(solveSet, aUpperType, variance)
        ){mergeVariance(_, _)}
      case FieldDecl(a, aType) =>
        gatherVariance(solveSet, aType, variance)
      case proj: TypeProj if solveSet(proj) =>
        Map(proj -> variance)
      case _ => Map()
    }

    def gatherScope(scope: Scope, r: Symbol, typ: Type): Map[TypeProj, Scope] = typ match {
      case FunType(x, xType, xResType) =>
        mapUnion(
          gatherScope(scope, r, xType),
          gatherScope(scope, r, xResType)
        ){
          mapUnion(_, _){(t1, t2) =>
            ???
          }
        }
      case AndType(left, right) =>
        mapUnion(
          gatherScope(scope, r, left),
          gatherScope(scope, r, right)
        ){
          mapUnion(_, _){(t1, t2) =>
            ???
          }
        }
      case TypeDecl(a, aLowerType, aUpperType) =>
        mapUnion(
          gatherScope(scope, r, aLowerType),
          gatherScope(scope, r, aUpperType)
        ){
          mapUnion(_, _){(t1, t2) =>
            ???
          }
        }
      case FieldDecl(a, aType) =>
        gatherScope(scope, r, aType)
      case proj @ TypeProj(x, _) if x == r =>
        Map(proj -> scope)
      case _ => Map()
    }


    def liftVars(su: SymbolUniverse, typ: Type, subst: Map[Symbol, Symbol]): Type = typ match { // TODO
      case TypeProj(x, a) =>
        TypeProj(subst.getOrElse(x, x), a)
      case FunType(x, xType, resType) =>
        val z = su.newSymbol()
        FunType(z,
          liftVars(su, xType, subst),
          liftVars(su, resType, subst + (x -> z)))
      case RecType(x, xType) =>
        val z = su.newSymbol()
        RecType(z, liftVars(su, xType, subst + (x -> z)))
      case AndType(left, right) =>
        AndType(
          liftVars(su, left, subst),
          liftVars(su, right, subst))
      case FieldDecl(a, aType) =>
        FieldDecl(a, liftVars(su, aType, subst))
      case TypeDecl(a, aLowerType, aUpperType) =>
        TypeDecl(a,
          liftVars(su, aLowerType, subst),
          liftVars(su, aUpperType, subst))
      case _ => typ
    }

//    final case class UnionFind[T](parents: Map[T, T], rank: Map[T, Int] = Map().withDefaultValue(0)) {
//      @tailrec
//      def find(a: T): T = parents.get(a) match {
//        case Some(b) => find(b)
//        case None    => a
//      }
//
//      def apply(a: T): T = find(a)
//
//      def union(a: T, b: T): UnionFind[T] = {
//        val (z, w) = (find(a), find(b))
//
//        if (rank(z) < rank(w))
//          copy(parents = parents+(w -> z))
//        else if (rank(w) < rank(z))
//          copy(parents = parents+(z -> w))
//        else
//          copy(
//            parents = parents+(z -> w),
//            rank = rank+(w -> (rank(w)+1)))
//      }
//
//      /** Make `root` the root of its set.
//       *
//       *  If `find(x) == find(root)` then afterwards: `find(x) == root`.
//       */
//      def makeRoot(root: T): UnionFind[T] = {
//        val rootRoot = find(root)
//        if (rootRoot == root)
//          return this
//        val newRank = max(rank(root), rank(rootRoot)+1)
//        copy(
//          parents = parents + (rootRoot -> root),
//          rank = rank + (root -> newRank))
//      }
//
//    }
//
//    def doRemap(remap: Map[Symbol, Symbol], typ: Type): Type = typeTransform(typ) {
//      case (TypeProj(x, a), _) => Some(TypeProj(remap(x), a))
//      case _                   => None
//    }

    def largestOfPrototype(typ: Type) = typeTransform(typ, Covariant) {
      case (Que, Covariant)     => Some(Top)
      case (Que, Contravariant) => Some(Bot)
      case _ => None
    }
    def smallestOfPrototype(typ: Type) = typeTransform(typ, Covariant) {
      case (Que, Covariant)     => Some(Bot)
      case (Que, Contravariant) => Some(Top)
      case _ => None
    }

    def doesShadow(scope: Scope, typ: Type): Boolean = typ match {
      case FunType(x, _, _) if scope.contains(x) => true
      case FunType(x, xType, xResType) =>
        (doesShadow(scope, xType)
          || doesShadow(scope + (x -> xType), xResType))
      case RecType(x, _) if scope.contains(x) => true
      case RecType(x, xType) =>
        doesShadow(scope + (x -> xType), xType)
      case AndType(left, right) =>
        (doesShadow(scope, left)
          || doesShadow(scope, right))
      case FieldDecl(a, aType) =>
        doesShadow(scope, aType)
      case TypeDecl(a, aLowerType, aUpperType) =>
        (doesShadow(scope, aLowerType)
          || doesShadow(scope, aUpperType))
      case _: TypeProj | Bot | Top | Que => false
      case _ => ???
    }

    def gatherConstraints(scope: Scope, solveSet: Set[TypeProj], zOption: Option[Symbol], from: Type, to: Type, patternIsLeft: Boolean, visitedLeft: Set[(Type, Type)] = Set(), visitedRight: Set[(Type, Type)] = Set()): Constraint = {


      // TODO rename ("from", "to") --> ("lhs", "rhs")?

      // TODO What if right in AndType(left, right) gets raised and left
      // projected a type in right? If we check from left to right we might
      // not notice...

      def rec(scope: Scope = scope, zOption: Option[Symbol] = zOption, from: Type = from, to: Type = to, patternIsLeft: Boolean = patternIsLeft, visitedLeft: Set[(Type, Type)] = visitedLeft, visitedRight: Set[(Type, Type)] = visitedRight) = gatherConstraints(scope, solveSet, zOption, from, to, patternIsLeft, visitedLeft, visitedRight)

      def largest(typ: Type) = typeTransform(typ, Covariant) {
        case (proj: TypeProj, Contravariant) if solveSet(proj) => Some(Bot)
        case (proj: TypeProj, _)             if solveSet(proj) => Some(Top)
        case _ => None
      }
      def smallest(typ: Type) = typeTransform(typ, Contravariant) {
        case (proj: TypeProj, Contravariant) if solveSet(proj) => Some(Bot)
        case (proj: TypeProj, _)             if solveSet(proj) => Some(Top)
        case _ => None
      }

      // TODO instead of storing scope in everyconstraint, just make sure
      // bound vars are unique and share a map for all local scopes?
      // Probably need to generate constraints on variables too?
      // z: {T: ?..?}

      // NOTE: In terms of the DOT inference rules, we are essentially moving
      // left-to-right. That is, we can't actually replace RHS with a subtype
      // X of RHS -- instead we use transitivity: LHS <: X, X <: RHS ==> LHS
      // <: RHS. The computations below look as if we are moving towards the
      // center (to X), but in terms of the actual proof there are a lot of
      // subtleties (which also why there are so many cases).

      (from, to) match {
        case (Bot, _) =>
          if (doesShadow(scope, to)) return FalseConstraint // TODO hack.
          TrueConstraint // Even if RHS is a prototype unwrapping RHS here would not constrain it.
        case (_, Top) =>
          if (doesShadow(scope, from)) return FalseConstraint // TODO hack.
          TrueConstraint

        case (FunType(x, _, _), FunType(y, _, _)) if x != y =>
          // TODO mayb generate "x=y" as a constraint? (and make sure x and y
          // ar unique everywhere using SymbolUniverse).
          if (patternIsLeft) { // TODO hack.
            if (scope.contains(x)) return FalseConstraint // TODO hack.
            //if (scope.contains(x)) throw new TypecheckingError(s"shadowing $x") // TODO
            rec(to=typeRenameBoundVarAssumingNonFree(x, to)) // TODO problem: whether to rename `from` or `to` depends on which side is `labeledPrototype`.
          } else {
            if (scope.contains(y)) return FalseConstraint // TODO hack.
            rec(from=typeRenameBoundVarAssumingNonFree(y, from)) // TODO problem: whether to rename `from` or `to` depends on which side is `labeledPrototype`.
          }

        case (FunType(x, xType, xResType), FunType(y, yType, yResType)) if x == y =>
          // TODO is it fine to just add yType to scope here? Do we need to
          // remove and solveSet-projs from it?
          if (scope.contains(x)) return FalseConstraint // TODO hack.
          //if (scope.contains(x)) throw new TypecheckingError(s"shadowing $x") // TODO
          // TODO flip visited?

          andConstraint(
            rec(from=yType, to=xType, zOption=None, patternIsLeft=(!patternIsLeft)),  // TODO Extension to DOT: zOption = Some(x)?
            rec(scope = scope+(x -> largest(yType)), from=xResType, to=yResType, zOption=None))


        case (FieldDecl(a, aType), FieldDecl(b, bType)) if a == b =>
          rec(from=aType, to=bType, zOption=None)

        case (TypeDecl(a, aLowerType, aUpperType), TypeDecl(b, bLowerType, bUpperType)) if a == b =>
          andConstraint(
            rec(from=bLowerType, to=aLowerType, zOption=None, patternIsLeft=(!patternIsLeft)),
            rec(from=aUpperType, to=bUpperType, zOption=None))


        case (aProj @ TypeProj(x, a), bProj @ TypeProj(y, b)) =>
          if (aProj == bProj) { // NOTE: Even for solveSet.
            TrueConstraint
          } else if (solveSet(aProj) && solveSet(bProj)) {
            ??? // TODO Can this actually happen?
            FalseConstraint
          } else if (solveSet(aProj) && !solveSet(bProj)) {
            MultiAndConstraint(Map(aProj -> (scope, Bot, smallest(to))))
          } else if (!solveSet(aProj) && solveSet(bProj)) {
            MultiAndConstraint(Map(bProj -> (scope, largest(from), Top)))
          } else { // if (!solveSet(aProj) && !solveSet(bProj))
            val newFromOption = if (visitedLeft((aProj, bProj))) Some(Top) else typeProjectUpper(scope, x, a)
            val newToOption = if (visitedRight((aProj, bProj))) Some(Bot) else typeProjectLower(scope, y, b)
            List(
              newFromOption.map{newFrom =>
                rec(from=newFrom, visitedLeft = visitedLeft+((aProj, bProj)))
              },
              newToOption.map{newTo =>
                rec(to=newTo, visitedRight = visitedRight+((aProj, bProj)))
              }
            ).flatten.fold(FalseConstraint){orConstraint(_, _)}
          }

        case (aProj @ TypeProj(x, a), _) if solveSet(aProj) =>
          MultiAndConstraint(Map(aProj -> (scope, Bot, smallest(to))))

        case (_, bProj @ TypeProj(y, b)) if solveSet(bProj) =>
          MultiAndConstraint(Map(bProj -> (scope, largest(from), Top)))


        case (RecType(x, xType), RecType(y, yType)) if x != y =>
          if (!scope.contains(x)) // TODO hack.
            rec(to=typeRenameBoundVarAssumingNonFree(x, to))
          else if (!scope.contains(y))
            rec(from=typeRenameBoundVarAssumingNonFree(y, from))
          else
            FalseConstraint // TODO

          //if (scope.contains(x) || scope.contains(y)) return FalseConstraint // TODO
          //rec(to=typeRenameBoundVarAssumingNonFree(x, to)) // TODO
          //rec(from=typeRenameBoundVarAssumingNonFree(y, from))

        case (RecType(x, xType), RecType(y, yType)) if x == y && zOption == None =>
          if (rigidEqualTypes(from, to)) TrueConstraint else FalseConstraint

        case (RecType(x, xType), RecType(y, yType)) if x == y && zOption != None =>
          // NOTE: We need to be careful of renaming TypeProj in xType that
          // reference "x" (i.e. "y"). That is:
          // TypeProj(y, a) == TypeProj(y, a)
          // but
          // TypeProj(z, a) != TypeProj(y, a).
          if (scope.contains(y)) return FalseConstraint //throw new TypecheckingError(s"$y") // TODO hack.
          rec(scope = scope + (y -> yType), from=xType, to=yType) // TODO Is this just waiting to explode? // TODO check for solveSet?


        case (RecType(x, xType), bProj @ TypeProj(y, b)) if !solveSet(bProj) && zOption != None =>
          val newToOption = if (visitedRight((from, bProj))) Some(Bot) else typeProjectLower(scope, y, b)
          newToOption match {
            case None => FalseConstraint
            case Some(newTo) =>
              orConstraint(
                rec(from=typeRenameVar(x, zOption.get, xType)),
                rec(to=newTo, visitedRight = visitedRight+((from, bProj))))
          }

        case (aProj @ TypeProj(x, a), RecType(y, yType)) if !solveSet(aProj) =>
          val newFromOption = if (visitedRight((aProj, to))) Some(Bot) else typeProjectUpper(scope, x, a)
          newFromOption match {
            case None => FalseConstraint
            case Some(newFrom) =>
              if (scope.contains(y)) return FalseConstraint //throw new TypecheckingError(s"$y") // TODO hack.
              orConstraint(
                rec(from=newFrom, visitedLeft = visitedLeft+((aProj, to))),
                rec(scope + (y -> yType), to=yType)) // TODO check for solveSet in `to`?
          }

        case (AndType(left, right), RecType(y, yType)) if zOption != None =>
          if (scope.contains(y)) return FalseConstraint // throw new TypecheckingError(s"$y") // TODO may need SymbolUniverse after all... // TODO hack.
          orConstraint(
            rec(scope, from=RecType(y, from)), // TODO wrong? What if `from` contains solveSet?
            orConstraint(
              rec(from=left),
              rec(from=right)
            )
          )
        case (AndType(left, right), RecType(y, yType)) =>
          if (scope.contains(y)) return FalseConstraint // throw new TypecheckingError(s"$y") // TODO may need SymbolUniverse after all... // TODO hack.
          orConstraint(
            rec(from=left),
            rec(from=right)
          )

        case (AndType(left, right), bProj @ TypeProj(y, b)) if !solveSet(bProj) =>  // TODO similar for the (_, AndType)-case above?
          val newToOption = if (visitedRight((from, bProj))) Some(Bot) else typeProjectLower(scope, y, b)
          newToOption match {
            case None => FalseConstraint
            case Some(newTo) =>
              orConstraint(
                rec(to=newTo, visitedRight = visitedRight+((from, bProj))),
                orConstraint(
                  rec(from=left),
                  rec(from=right)))
          }


        case (Top | _: FunType | _: RecType | _: FieldDecl | _: TypeDecl | _: AndType | _: TypeProj, AndType(left, right)) =>
          andConstraint(
            rec(to=left),
            rec(to=right))

        case (AndType(left, right), Bot | _: FieldDecl | _: TypeDecl | _: FunType) =>
          orConstraint(
            rec(from=left),
            rec(from=right))


        case (aProj @ TypeProj(x, a), Bot | _: FieldDecl | _: TypeDecl | _: FunType) if !solveSet(aProj) =>
          // TODO problem if: scope=(x -> p) and solveSet(p).
          val newFromOption = if (visitedRight((aProj, to))) Some(Bot) else typeProjectUpper(scope, x, a)
          newFromOption match {
            case None          => FalseConstraint
            case Some(newFrom) =>
              rec(from=newFrom, visitedLeft = visitedLeft+((aProj, to)))
          }

        case (Top | _: FieldDecl | _: TypeDecl | _: FunType | _: RecType, bProj @ TypeProj(y, b)) if !solveSet(bProj) =>
          val newToOption = if (visitedRight((from, bProj))) Some(Bot) else typeProjectLower(scope, y, b)
          newToOption match {
            case None        => FalseConstraint
            case Some(newTo) => rec(to=newTo, visitedRight = visitedRight+((from, bProj)))
          }


        case (Top | _: FunType | _: FieldDecl | _: TypeDecl, RecType(y, yType)) if zOption != None =>
          if (scope.contains(y)) return FalseConstraint //throw new TypecheckingError(s"$y") // TODO hack.
          rec(scope + (y -> yType), to=yType) // TODO check for solveSet in `to`?

        case (RecType(x, xType), Bot | _: FieldDecl | _: TypeDecl | _: FunType | _: RecType | _: TypeProj) if zOption != None =>
          rec(from=typeRenameVar(x, zOption.get, xType))


        case _ => FalseConstraint
      }
    }

    /** Find an equivalent prototype, with fewer nodes.
     */
    def simplify(prototype: Prototype): Prototype = prototype match {
      case _: AndType =>

        def simplifyFieldDecls(seq: Seq[FieldDecl]): Seq[FieldDecl] = {
          seq.groupBy{case FieldDecl(a, _) => a}.map{case (a, ls) =>
            FieldDecl(a, simplify(ls.map{case FieldDecl(_, aType) => aType}.reduce{andType(_, _)}))
          }.toSeq
        }
        def simplifyTypeDecls(seq: Seq[TypeDecl]): Seq[TypeDecl] = {
          seq.map{simplify}.asInstanceOf[Seq[TypeDecl]]
          // TODO a -> TypeDecl(lub(lowerBounds), glb(upperBounds))?
        }
        def simplifyFunTypes(seq: Seq[FunType]): Seq[FunType] = {
          seq.map{simplify}.asInstanceOf[Seq[FunType]]
          // TODO if f and g are in seq, and f <: g, then remove g from seq?
        }

        def simplifyRecTypes(seq: Seq[RecType]): Seq[RecType] = {
          seq.map{simplify}.asInstanceOf[Seq[RecType]]
          // NOTE: We can't merge RecTypes or a big scary monster will come
          // and eat us.
        }

        sealed trait Group
        object GroupQue       extends Group
        object GroupTop       extends Group
        object GroupBot       extends Group
        object GroupErrorType extends Group
        object GroupTypeProj  extends Group
        object GroupFieldDecl extends Group
        object GroupTypeDecl  extends Group
        object GroupFunType   extends Group
        object GroupRecType   extends Group
        object GroupMisc      extends Group

        val grouped = andTypeSeq(prototype).groupBy{
          case Que          => GroupQue
          case Top          => GroupTop
          case Bot          => GroupBot
          case ErrorType    => GroupErrorType
          case _: TypeProj  => GroupTypeProj
          case _: FieldDecl => GroupFieldDecl
          case _: TypeDecl  => GroupTypeDecl
          case _: FunType   => GroupFunType
          case _: RecType   => GroupRecType
          case _            => GroupMisc
        }
        if (grouped.contains(GroupErrorType))
          ErrorType
        else if (grouped.contains(GroupBot))
          Bot // TODO don't?
        else if (grouped.contains(GroupTop) && grouped.size == 1)
          Top
        else grouped.map{
          case (GroupQue,       ls) => GroupQue       -> ls.toSet.toSeq
          case (GroupTop,       ls) => GroupTop       -> Seq()
          case (GroupTypeProj,  ls) => GroupTypeProj  -> ls.toSet.toSeq
          case (GroupFieldDecl, ls) => GroupFieldDecl -> simplifyFieldDecls(ls.asInstanceOf[Seq[FieldDecl]])
          case (GroupTypeDecl,  ls) => GroupTypeDecl  -> simplifyTypeDecls(ls.asInstanceOf[Seq[TypeDecl]])
          case (GroupFunType,   ls) => GroupFunType   -> simplifyFunTypes(ls.asInstanceOf[Seq[FunType]])
          case (GroupRecType,   ls) => GroupRecType   -> simplifyRecTypes(ls.asInstanceOf[Seq[RecType]])
          case (misc @ _,      ls)  => misc           -> ls
        }.values.flatten.reduce{AndType(_, _)}
      case TypeDecl(a, aLowerType, aUpperType) =>
        TypeDecl(a, simplify(aLowerType), simplify(aUpperType))
      case FieldDecl(a, aType) =>
        FieldDecl(a, simplify(aType))
      case FunType(x, xType, xResType) =>
        FunType(x, simplify(xType), simplify(xResType))
      //case RecType(x, xType) =>
      //  RecType(x, simplify(xType)) // TODO Is it okay to change the inside of a RecType? NO.
      case _ => prototype
    }

    /** Least supertype R of `lowerType` such that R matches `upperPrototype`.
     * Return None if R does not exists.
     */
    def raise(scope: Scope, r: Symbol, zOption: Option[Symbol], lowerType: Type, upperPrototype: Prototype): Option[Type] = {
      if (scope.contains(r))
        throw new TypecheckingError(s"var $r is already in use")
      val cleanedPrototype = upperPrototype
      val (numQues, labeledPrototype) = prepMatch(r, simplify(cleanedPrototype))
      val solveSet = (0 until numQues).map{TypeProj(r, _)}.toSet // TODO get rid of solveSet somehow?
      val solveSetVariance = gatherVariance(solveSet, labeledPrototype, Covariant)
      val constraint = gatherConstraints(scope, solveSet, zOption, lowerType, labeledPrototype, patternIsLeft=false) // TODO simplify(lowerType)?
      solveConstraint(scope, zOption, solveSet, solveSetVariance, constraint, labeledPrototype, Covariant).map{simplify(_)}
    }

    /** Greatest subtype L of `upperType` such that L matches `lowerPrototype`.
     * Return None if L does not exists.
     */
    def lower(scope: Scope, r: Symbol, zOption: Option[Symbol], lowerPrototype: Prototype, upperType: Type): Option[Type] = {
      // TODO if lowerPrototype is a recursive type, we may need to pass down
      // a seperate symbol, wOption?
      // TODO But are we actually allowed to unwrap the lower bound of upperType?
      // e.g. if upperType = TypeProj(x,a)
      // then the lowerbound of x.a may be a recursive type. In which case we
      // must wrap the lowerType in rectype to reach lowerbound and by
      // transitivity reach upperType.
      // TODO using zOption=None for lower (but not raise) always, always seems to work!?!?

      if (isPrototype(upperType))
        throw new TypecheckingError(s"tried to lower prototype")

      if (scope.contains(r))
        throw new TypecheckingError(s"var $r is already in use")
      val cleanedPrototype = zOption.map{z => unwrapRecTypes(lowerPrototype, z)}.getOrElse(lowerPrototype)
      val (numQues, labeledPrototype) = prepMatch(r, simplify(cleanedPrototype))
      val solveSet = (0 until numQues).map{TypeProj(r, _)}.toSet // TODO get rid of solveSet somehow?
      val solveSetVariance = gatherVariance(solveSet, labeledPrototype, Contravariant)
      val constraint = gatherConstraints(scope, solveSet, zOption, labeledPrototype, upperType, patternIsLeft=true)
      solveConstraint(scope, zOption, solveSet, solveSetVariance, constraint, labeledPrototype, Contravariant).map{simplify(_)}
    }

    /** Like raise(scope(z), prototype), but does unwrapRecTypes(_, z) first.
     */
    def varRaise(scope: Scope, r: Symbol, z: Symbol, prototype: Prototype): Option[Type] = {
      raise(scope, r, Some(z), scope(z), prototype)
    }

    /** Like lower(prototype, scope(z)), but does unwrapRecTypes(_, z) first.
     */
    def varLower(scope: Scope, r: Symbol, z: Symbol, prototype: Prototype): Option[Type] = {
      lower(scope, r, None, prototype, scope(z))
    }

    /** Check if `first <: second`, but RecTypes need to be rigidEqualTypes.
     */
    def isSubtypeOf(scope: Scope, first: Type, second: Type, visitedLeft: Set[TypeProj] = Set(), visitedRight: Set[TypeProj] = Set()): Boolean = {
      if (scope.contains(-1)) ???
      raise(scope, -1, None, first, second) != None
//        // TODO Confirm whether r is efficient. I expect r to get inlined.
//        def r(scope: Scope = scope, first: Type = first, second: Type = second, visitedLeft: Set[TypeProj] = visitedLeft, visitedRight: Set[TypeProj] =
//          visitedRight) = isSubtypeOf(scope, first, second, visitedLeft, visitedRight)
//        (first, second) match {
//          case (Bot, _) => true
//          case (_, Top) => true
//
//          case (AndType(left, right), bProj @ TypeProj(y, b)) =>
//            val bLowerType = if (visitedRight(bProj)) Bot else typeProjectLower(scope, y, b).getOrElse{error()}
//            (r(first=left)
//              || r(first=right)
//              || r(second=bLowerType, visitedRight = visitedRight+bProj))
//
//          case (_, AndType(left, right)) => r(second=left) && r(second=right) // IMPORTANT: This must be checked before the other AndType-case.
//          case (AndType(left, right), _) => r(first=left) || r(first=right)
//
//          case (FieldDecl(a, aType), FieldDecl(b, bType)) if a == b => r(first=aType, second=bType)
//
//          case (TypeDecl(a, aLowerType, aUpperType), TypeDecl(b, bLower, bUpper)) if a == b =>
//           r(first=bLower, second=aLowerType) && r(first=aUpperType, second=bUpper)
//
//          case (aProj @ TypeProj(x, a), bProj @ TypeProj(y, b)) =>
//            aProj == bProj || {
//              val aUpperType = if (visitedLeft(aProj)) Top else typeProjectUpper(scope, x, a).getOrElse{error()} // TODO pass along visitedLeft to typeProjectUpper
//              // TODO eliminate rectypes!
//              r(first=aUpperType, visitedLeft=visitedLeft+aProj)
//            } || {
//              val bLowerType = if (visitedRight(bProj)) Bot else typeProjectLower(scope, y, b).getOrElse{error()} // TODO pass along visitedRight to typeProjectUpper
//              // TODO eliminate rectypes!
//              r(second=bLowerType, visitedRight=visitedRight+bProj)
//            }
//
//          case (aProj @ TypeProj(x, a), _) =>
//            val aUpperType = if (visitedLeft(aProj)) Top else typeProjectUpper(scope, x, a).getOrElse{error()}
//            r(first=aUpperType, visitedLeft = visitedLeft+aProj)
//          case (_, bProj @ TypeProj(y, b)) =>
//            val bLowerType = if (visitedRight(bProj)) Bot else typeProjectLower(scope, y, b).getOrElse{error()}
//            r(second=bLowerType, visitedRight = visitedRight+bProj)
//
//          case (FunType(x, xType, _), FunType(y, _, _)) if x != y =>
//            // TODO Maybe better to create new var "w", and rename {x,y}->w?
//            if (scope.contains(x) && !(isSubtypeOf(scope, scope(x), xType))) {
//              ???
//            }
//            // TODO is isVarFreeInType(x, second)-check necessary?
//            (!isVarFreeInType(x, second)
//              && r(second=typeRenameBoundVarAssumingNonFree(x, second)))
//
//          case (FunType(x, xType, xResType), FunType(y, yType, yResType)) if x == y =>
//            (r(first=yType, second=xType)
//              && r(scope = scope+(x -> yType), first=xResType, second=yResType))
//          case (_: RecType, _) | (_, _: RecType) =>
//            rigidEqualTypes(first, second)
//          case _ => false
//        }
      }

    /** Check if scope(x) <: subtype.
     *  Formally, x: xType, xType <: To.
     */
    def varIsSubtypeOf(scope: Scope, z: Symbol, supertype: Type): Boolean = {
      //val zType = unwrapRecTypes(scope(z), z)
      //val zSupertype = unwrapRecTypes(supertype, z)
      //isSubtypeOf(scope, zType, zSupertype)
      // TODO need to unwrap rectypes when we upcast rectype!

      if (scope.contains(-1)) ???
      varRaise(scope, -1, z, supertype) != None
    }

    /** Check if types are equal by subtyping.
     * Formally, first <: second AND second <: first.
     */
    def varEqualTypes(scope: Scope, z: Symbol, second: Type): Boolean = {
      val first = scope(z)
      (varIsSubtypeOf(scope, z, second)
        && varIsSubtypeOf(scope + (z -> second), z, first))
    }


    def equalTypes(scope: Scope, first: Type, second: Type): Boolean = {
      if (scope.contains(-1)) ???
      (raise(scope, -1, None, first, second) != None
        && raise(scope, -1, None, second, first) != None)
    }

    /** Check if first and second are exactly equal, except for renaming some bound vars.
     */
    def rigidEqualTypes(first: Type, second: Type): Boolean = {
      (first, second) match {
      case (Bot, Bot) => true
      case (Top, Top) => true
      case (AndType(l1, r1), AndType(l2, r2)) =>
        // TODO Allow orders to differ? Probably best not to.
        (rigidEqualTypes(l1, l2)
          && rigidEqualTypes(r1, r2))
      case (FieldDecl(a, aType), FieldDecl(b, bType)) =>
        (a == b
          && rigidEqualTypes(aType, bType))

      case (TypeDecl(a, aLowerType, aUpperType), TypeDecl(b, bLowerType, bUpperType)) =>
        (a == b
          && rigidEqualTypes(aLowerType, bLowerType)
          && rigidEqualTypes(aUpperType, bUpperType))

      case (aProj: TypeProj, bProj: TypeProj) =>
        aProj == bProj

      case (FunType(x, _, _), FunType(y, _, _)) if x != y =>
        // TODO Allow x free in second? That would mean x is in the current
        // scope... Maybe best to translate {x,y} to new var "w", after all?
        // Would be nice to keep it as a pure function instead of passing SU
        // everywhere.
        (!isVarFreeInType(x, second)
          && rigidEqualTypes(first, typeRenameBoundVarAssumingNonFree(x, second)))

      case (FunType(x, xType, xResType), FunType(y, yType, yResType)) if x == y =>
        (rigidEqualTypes(yType, xType)
          && rigidEqualTypes(xResType, yResType))
      case (RecType(x, xType), RecType(y, yType)) if x != y =>
        // TODO Allow x free in second? That would mean x is in the current
        // scope... Maybe best to translate {x,y} to new var "w", after all?
        // Would be nice to keep it as a pure function instead of passing SU
        // everywhere.
        (!isVarFreeInType(x, second)
          && rigidEqualTypes(first, typeRenameBoundVarAssumingNonFree(x, second)))
      case (RecType(x, xType), RecType(y, yType)) if x == y =>
        rigidEqualTypes(xType, yType)
      case _ => false
    }
    }

    // TODO rename to: isSubtypeOfAssumingNoRecTypes?
//    def isSubtypeOf(scope: Scope, first: Type, second: Type): Boolean = { // TODO REM. use varIsSubtypeOf instead.
//      def inner(scope: Scope, first: Type, second: Type, visitedLeft: Set[TypeProj], visitedRight: Set[TypeProj]): Boolean = (first, second) match {
//        case (_: RecType, _) | (_, _: RecType) =>
//          throw new TypecheckingError("nested RecType")
//
//        case (Bot, _) => true
//        case (_, Top) => true
//
//        case (TypeProj(x, a), TypeProj(y, b)) if x == y && a == b => true
//        case (aProj @ TypeProj(x, a), bProj @ TypeProj(y, b)) if !(x == y && a == b) =>
//          val aUpperType = if (visitedLeft(aProj)) Top else typeProjectUpper(scope, x, a).getOrElse{error()}
//          val bLowerType = if (visitedRight(bProj)) Bot else typeProjectLower(scope, y, b).getOrElse{error()}
//          (inner(scope, aUpperType, bProj, visitedLeft + aProj, visitedRight)
//            || inner(scope, aProj, bLowerType, visitedLeft, visitedRight + bProj))
//
//        case (aProj @ TypeProj(x, a), _) =>
//          val aUpperType = if (visitedLeft(aProj)) Top else typeProjectUpper(scope, x, a).getOrElse{error()}
//          inner(scope, aUpperType, second, visitedLeft + aProj, visitedRight)
//        case (_, bProj @ TypeProj(y, b)) =>
//          val bLowerType = if (visitedRight(bProj)) Bot else typeProjectLower(scope, y, b).getOrElse{error()}
//          inner(scope, first, bLowerType, visitedLeft, visitedRight + bProj)
//
//        case (AndType(left, right), _) =>
//          (inner(scope, left, second, visitedLeft, visitedRight)
//            || inner(scope, right, second, visitedLeft, visitedRight))
//        case (_, AndType(left, right)) =>
//          (inner(scope, first, left, visitedLeft, visitedRight)
//            && inner(scope, first, right, visitedLeft, visitedRight))
//
//        case (FunType(x, _, _), FunType(y, _, _)) if x != y =>
//          (!isVarFreeInType(x, second)
//            && inner(scope, first, typeRenameBoundVarAssumingNonFree(x, second), visitedLeft, visitedRight))
//        case (FunType(x, xType, xResType), FunType(y, yType, yResType)) if x == y =>
//          (inner(scope, yType, xType, visitedRight, visitedLeft)
//            && inner(scope, xResType, yResType, visitedLeft, visitedRight))
//
//        case (FieldDecl(a, aType), FieldDecl(b, bType)) if a == b =>
//          inner(scope, aType, bType, visitedLeft, visitedRight)
//        case (TypeDecl(a, aLowerType, aUpperType), TypeDecl(b, bLowerType, bUpperType)) if a == b =>
//          (inner(scope, bLowerType, aLowerType, visitedRight, visitedLeft)
//            && inner(scope, aUpperType, bUpperType, visitedLeft, visitedRight))
//        case _ => false
//      }
//      inner(scope, first, second, Set(), Set())
//    }


    def isVarFreeInType(z: Symbol, typ: Type): Boolean = typ match {
      case TypeProj(x, a) => (x == z)
      case FunType(x, xType, resType) if x != z =>
        isVarFreeInType(z, xType) || isVarFreeInType(z, resType)
      case RecType(x, xType) if x != z =>
        isVarFreeInType(z, xType)
      case FieldDecl(a, aType) =>
        isVarFreeInType(z, aType)
      case TypeDecl(a, aLowerType, aUpperType) =>
        isVarFreeInType(z, aLowerType) || isVarFreeInType(z, aUpperType)
      case AndType(left, right) =>
        isVarFreeInType(z, left) || isVarFreeInType(z, right)
      case _ => false // Bot, Top, Que
    }

    def isVarFreeInDef(z: Symbol, e: Def): Boolean = e match {
      case FieldDef(a, aTerm)     => isVarFreeInTerm(z, aTerm)
      case TypeDef(a, aType)      => isVarFreeInType(z, aType)
      case AndDef(left, right)    => (isVarFreeInDef(z, left) || isVarFreeInDef(z, right))
      case _ => false
    }

    def isVarFreeInTerm(z: Symbol, e: Expr): Boolean = e match {
      case Var(x)                 => (x == z)
      case Sel(x, a)              => (x == z)
      case App(x, y)              => (x == z || y == z)
      case Let(x, xTerm, resTerm) if x != z => isVarFreeInTerm(z, xTerm) || isVarFreeInTerm(z, resTerm)
      case Obj(x, xType, d)       if x != z => isVarFreeInType(z, xType) || isVarFreeInDef(z, d)
      case Fun(x, xType, resTerm) if x != z => isVarFreeInType(z, xType) || isVarFreeInTerm(z, resTerm)
      case _ => false
    }

    def typeMonoidMap[T](scope: Scope, typ: Type, variance: Variance = Covariant)(mappend: (T, T) => T)(f: (Scope, Type, Variance) => T): T = typ match {
      case FunType(x, xType, resType) =>
        mappend(
          f(scope, typ, variance),
          mappend(
            typeMonoidMap(scope, xType, reverseVariance(variance))(mappend)(f),
            typeMonoidMap(scope + (x -> xType), resType, variance)(mappend)(f)))
      case RecType(x, xType) =>
        mappend(
          f(scope, typ, variance),
          typeMonoidMap(scope + (x -> xType), xType, variance)(mappend)(f))
      case FieldDecl(a, aType) =>
        mappend(
          f(scope, typ, variance),
          typeMonoidMap(scope, aType, variance)(mappend)(f))
      case TypeDecl(a, aLowerType, aUpperType) =>
        mappend(
          f(scope, typ, variance),
          mappend(
            typeMonoidMap(scope, aLowerType, reverseVariance(variance))(mappend)(f),
            typeMonoidMap(scope, aUpperType, variance)(mappend)(f)))
      case AndType(left, right) =>
        mappend(
          f(scope, typ, variance),
          mappend(
            typeMonoidMap(scope, left, variance)(mappend)(f),
            typeMonoidMap(scope, right, variance)(mappend)(f)))
      case _ => f(scope, typ, variance) // Bot, Top, Que, TypeProj, ErrorType
    }


    // TODO make a variant that expands TypeProjs?
    //def typeDfsSet[T](typ: Type)(f: Type => Set[T]): Set[T] = f(typ) ++ (typ match {
    //  case FunType(x, xType, resType) =>
    //    typeDfsSet(xType)(f) ++ typeDfsSet(resType)(f)
    //  case RecType(x, xType) =>
    //    typeDfsSet(xType)(f)
    //  case FieldDecl(a, aType) =>
    //    typeDfsSet(aType)(f)
    //  case TypeDecl(a, aLowerType, aUpperType) =>
    //    typeDfsSet(aLowerType)(f) ++ typeDfsSet(aUpperType)(f)
    //  case AndType(left, right) =>
    //    typeDfsSet(left)(f) ++ typeDfsSet(right)(f)
    //  case _ => Set() // Bot, Top, Que, TypeProj, ErrorType
    //})

    def typeDfsSet[T](typ: Type)(f: Type => Set[T]): Set[T] = NoFuture.typeMonoidMap[Set[T]](Map(), typ){_ ++ _}{
      case (_, typ, _) => f(typ)
    }

    def validTypeInScope(scope: Scope, typ: Type, variance: Variance = Covariant): Boolean = NoFuture.typeMonoidMap[Boolean](scope, typ, variance){_ && _}{
      case (scope, TypeProj(x, a), variance) =>
        scope.contains(x) && (variance match {
          case Covariant =>
            NoFuture.typeProjectUpper(scope, x, a) != None
          case Contravariant =>
            NoFuture.typeProjectLower(scope, x, a) != None
          case _ => ???
        })
      case _ => true
    }

    def allTypeMemberSymbolsInType(typ: Type): Set[Symbol] = typeDfsSet(typ) {
      case TypeProj(_, a)    => Set(a)
      case TypeDecl(a, _, _) => Set(a)
      case _                 => Set()
    }

    def allFieldMemberSymbolsInType(typ: Type): Set[Symbol] = typeDfsSet(typ) {
      case FieldDecl(a, _) => Set(a)
      case _               => Set()
    }

    def allBoundVarsInType(typ: Type): Set[Symbol] = typeDfsSet(typ) {
      case FunType(x, _, _) => Set(x)
      case RecType(x, _)    => Set(x)
      case _                => Set()
    }

    def allFreeVarsInType(typ: Type): Set[Symbol] = typeDfsSet[Symbol](typ) {
      case TypeProj(x, _) => Set(x)
      case _ => Set()
    } -- allBoundVarsInType(typ)

    def allVarsAndMembersInType(typ: Type): Set[Symbol] = Set(
      allTypeMemberSymbolsInType(typ),
      allFieldMemberSymbolsInType(typ),
      allBoundVarsInType(typ),
      allFreeVarsInType(typ)
    ).flatten


    // TODO also do indirect? The problem is that we may need this when
    // testing typeProjectUpper...
    def allDirectTypeMembers(scope: Scope, typ: Type, visited: Set[TypeProj]): Set[Symbol] = typ match {
      case RecType(x, xType) =>
        allDirectTypeMembers(scope + (x -> xType), xType, visited)
      case AndType(left, right) =>
        (allDirectTypeMembers(scope, left, visited)
          ++ allDirectTypeMembers(scope, right, visited))
      case TypeDecl(a, _, _) => Set(a)
      case _ => Set()
    }

    def directFieldDecls(scope: Scope, x: Symbol): Map[(Symbol, Symbol), Type] = {
      def inner(scope: Scope, typ: Type, visited: Set[TypeProj]): Map[(Symbol, Symbol), Type] = {
        val seq: Seq[Map[(Symbol, Symbol), Type]] = andTypeSeq(unwrapRecTypes(scope(x), x)).map{
          case FieldDecl(a, aType) => Map((x, a) -> aType)
          case proj @ TypeProj(y, a) if !visited(proj) =>
            inner(scope, typeProjectUpper(scope, y, a).get, visited + proj)
          case _ => Map[(Symbol, Symbol), Type]()
        }
        seq.fold(Map()){
          mapUnion(_, _){
            andType(_, _)
          }
        }
      }
      inner(scope, scope(x), Set())
    }
    def directFieldDeclsInScope(scope: Scope): Map[(Symbol, Symbol), Type] = {
      scope.keys.map{directFieldDecls(scope, _)}.fold(Map()){
        mapUnion(_, _){
          andType(_, _)
        }
      }
    }

    def allReferenceableTerms(su: SymbolUniverse, scope: Scope, x: Symbol): Map[SymbolPath, Type] = { // TODO REM?
      def inner(scope: Scope, path: SymbolPath, typ: Type, visited: Set[TypeProj]): Map[SymbolPath, Type] = typ match {
        case AndType(left, right) =>
          val leftMap = inner(scope, path, left, visited)
          val rightMap = inner(scope, path, right, visited)
          mapUnion(leftMap, rightMap){AndType(_, _)}
        case RecType(x, xType) =>
          inner(scope, path, xType, visited)
        case aProj @ TypeProj(x, a) =>
          typeProjectUpper(scope, x, a) match {
            case Some(aUpperType) => inner(scope, path, aUpperType, visited + aProj)
            case None => ???; Map()
          }
        case FieldDecl(a, aType) =>
          inner(scope, path, typ, visited) + ((path :+ a) -> aType)
        case _ =>
          Map()
      }
      inner(scope, Vector(x), scope(x), Set())
    }

    def allReferenceableTermsInScope(su: SymbolUniverse, scope: Scope): Map[SymbolPath, Type] = {
      scope.keys.map{allReferenceableTerms(su, scope, _)}.fold(Map()){case (leftMap, rightMap) =>
        mapUnion(leftMap, rightMap){AndType(_, _)}
      }
    }


    def selPath(su: SymbolUniverse, path: SymbolPath): Term = path match { // TODO REM?
      case Seq(x) =>
        Var(x)
      case x +: a +: Seq() =>
        Sel(x, a)
      case x +: a +: rest =>
        val y = su.newSymbol()
        Let(y, Sel(x, a), selPath(su, y +: rest))
    }
  }

  type Prototype = Type
  case object Que extends Prototype {
    val treeHeight = 1
    val totNumNodes = 1
  }

  case object ErrorType extends Type { // TODO vs some other errorhandling?
    val treeHeight = 1
    val totNumNodes = 1
  }
  case class FutureType(cell: DefaultCell[Type]) extends Type {
    val treeHeight = 1
    val totNumNodes = 1
    // TODO approximations
  }

  case class TypeSymbol(a: Symbol) extends Type {
    val treeHeight = 1
    val totNumNodes = 1
    // TODO approximations
  }

  // TODO typed terms. {var mytpe} like in Dotty? vs cell?

  type Scope = Map[Symbol, Type]
  object Scope {
    def apply(args: (Symbol, Type)*): Scope = Map(args: _*)
  }

  type DefaultCell[T]          = Cell[DefaultKey[T], T]
  type DefaultCellCompleter[T] = CellCompleter[DefaultKey[T], T]

  def contZip[A, B](a: ((A) => Unit) => Unit, b: ((B) => Unit) => Unit)(cont: (A, B) => Unit): Unit = {
    // TODO fork?
    a{aRes =>
      b{bRes =>
        cont(aRes, bRes)
      }
    }
  }

  def mapUnion[K, T, A <: T, B <: T](a: Map[K, A], b: Map[K, B])(f: (A, B) => T): Map[K, T] = {
    a ++ b.map{
      case (k, kValueInB) =>
        k -> (a.get(k) match {
          case Some(kValueInA) =>
            f(kValueInA, kValueInB)
          case None =>
            kValueInB
        })
    }
  }

  def mapIntersect[K, T, A, B](a: Map[K, A], b: Map[K, B])(f: (A, B) => T): Map[K, T] = {
    b.flatMap{
      case (k, kValueInB) =>
        a.get(k) match {
          case Some(kValueInA) =>
            Some(k -> f(kValueInA, kValueInB))
          case None =>
            None
        }
    }
  }


  object EmptyMap {
    def unapply[K, V](x: Map[K, V]): Boolean = {
      x.isEmpty
    }
  }

  object EmptySet {
    def unapply[K](x: Set[K]): Boolean = {
      x.isEmpty
    }
  }

  class SymbolUniverse(init: Int = 0) {
    private val counter = new AtomicInteger(init)
    def newSymbol(): Int = counter.getAndIncrement()
    def count(): Int = counter.get()
    override def toString() = s"SymbolUniverse(${count()})"
  }

  def await[T](awaitable: Awaitable[T]): T =
    Await.result(awaitable, Duration.Inf)

  def newCellCompleter[V](pool: HandlerPool, lattice: Lattice[V]): DefaultCellCompleter[V] = {
    CellCompleter[DefaultKey[V], V](pool, new DefaultKey[V])(lattice)
  }

  def onComplete[T](cell: DefaultCell[T])(f: T => Unit): Unit = {
    cell.onComplete{
      case Success(x) => f(x)
      case _ => ??? // TODO
    }
  }

  def onNext[T](cell: DefaultCell[T])(f: T => Unit): Unit = {
    cell.onNext{
      case Success(x) => f(x)
      case _ => ??? // TODO
    }
  }


  def expandTypeFutures(t: Type)(cont: (Type) => Unit): Unit = t match {
    case FutureType(cell) =>
      onComplete(cell) {actualType =>
        expandTypeFutures(actualType) {actualType =>
          cont(actualType)
        }
      }
    case FunType(x, xType, resType) =>
      expandTypeFutures(xType) {newXType =>
        expandTypeFutures(resType) {newResType =>
          cont(FunType(x, newXType, newResType))
        }
      }
    case RecType(x, xType) =>
      expandTypeFutures(xType){newXType =>
        cont(RecType(x, newXType))
      }
    case FieldDecl(a, aType) =>
      expandTypeFutures(aType){aType =>
        cont(FieldDecl(a, aType))
      }
    case TypeDecl(a, aLowerType, aUpperType) =>
      expandTypeFutures(aLowerType){newALowerType =>
        expandTypeFutures(aUpperType){newAUpperType =>
          cont(TypeDecl(a, newALowerType, newAUpperType))
        }
      }
    case AndType(leftType, rightType) =>
      expandTypeFutures(leftType){newLeftType =>
        expandTypeFutures(rightType){newRightType =>
          cont(AndType(newLeftType, newRightType))
        }
      }
    case _ => cont(t)
  }

  def expandFutureOnce(t: Type)(cont: (Type) => Unit): Unit = t match {
    case FutureType(cell) =>
      onComplete(cell) {
        expandFutureOnce(_)(cont)
      }
    case _ => cont(t)
  }

  def expandFutureScope(scope: Scope)(cont: (Scope) => Unit): Unit = {
    def r(s: List[(Symbol, Type)])(c: (List[(Symbol, Type)]) => Unit): Unit = s match {
      case (x, t) :: rest =>
        expandTypeFutures(t){newT =>
          r(rest){newRest =>
            c((x, newT) :: newRest)
          }
        }
      case Nil =>
        c(Nil)
    }
    r(scope.toList){newListScope =>
      cont(newListScope.toMap)
    }
  }

  def andTypeSeq(t: Type): Seq[Type] = t match {
    case AndType(left, right) =>
      andTypeSeq(left) ++ andTypeSeq(right)
    case _ => Seq(t)
  }

  def defSize(d: Def): Int = d match {
    case FieldDef(a, aTerm)  => 1
    case TypeDef(a, aType)   => 1
    case AndDef(left, right) => defSize(left) + defSize(right)
    case _ => ???
  }

  def defKeysAsSet(d: Def): Set[Symbol] = d match {
    case FieldDef(a, aTerm)  => Set(a)
    case TypeDef(a, aType)   => Set(a)
    case AndDef(left, right) => defKeysAsSet(left) ++ defKeysAsSet(right)
    case _ => ???
  }

  def defTermAsMap(d: Def): Map[Symbol, Def] = d match {
    case FieldDef(a, _) => Map(a -> d)
    case TypeDef(a, _)  => Map(a -> d)
    case AndDef(left, right) => defTermAsMap(left) ++ defTermAsMap(right)
    case _ => ???
  }

  def defHasDuplicates(d: Def): Boolean = {
    // TODO efficiency? parallel?
    defSize(d) != defKeysAsSet(d).size
  }

  def paramsKeysAsSet(params: List[List[(Symbol, Type)]]): Set[Symbol] =
    params.flatMap{p => p.map{case (a,t) => a}.toSet}.toSet

  def paramsHasDuplicates(d: Def): Boolean = {
    // TODO efficiency? parallel?
    defSize(d) != defKeysAsSet(d).size
  }

  class StupidWorkCounter(initialValue: Int, wheneverZero: => Unit) {
    private val atom = new AtomicInteger(initialValue)
    def add(x: Int): Unit = if (atom.addAndGet(x) == 0) wheneverZero
    def inc() = add(1)
    def dec() = add(-1)
  }

  def freeze[K <: Key[T], T](completer: CellCompleter[K, T]): Unit = {
    val res = completer.cell.getResult()
    completer.putFinal(res)
  }

  // TODO fun params: List[(Symbol, Type)] vs List[List[(Symbol, Type)]] vs Map[Symbol, Type]

  // TODO sequential type checker

  // TODO sequential infer

  // TODO a field can only have on DEFINITION, but multiple DECLARATIONS
  // (potentially of different types -- in which case the field must be a subtype
  // of both of them).

  // TODO if x: AndType(l, r), then x.a must match EITHER RecType. If l and r
  // only consist of DefTypes, then x will have exactly one occurence. But if
  // there is a

  object BoolOrLattice extends Lattice[Boolean] {
    def join(a: Boolean, b: Boolean): Boolean = (a || b)
    def empty = false
  }

  object BoolAndLattice extends Lattice[Boolean] {
    def join(a: Boolean, b: Boolean): Boolean = (a && b)
    def empty = true
  }

  sealed trait TypeMap
  case object FailureTypeMap extends TypeMap
  case class PartialTypeMap(m: Map[Symbol, Type]) extends TypeMap

  object TypeMapLattice extends Lattice[TypeMap] {
    def join(current: TypeMap, next: TypeMap): TypeMap = (current, next) match {
      case (FailureTypeMap, _) => FailureTypeMap
      case (_, FailureTypeMap) => FailureTypeMap
      case (PartialTypeMap(m1), PartialTypeMap(m2)) =>
        // NOTE: In most cases `m2.size == 1` so this is fast.
        if (m1.keySet.intersect(m2.keySet) != Set.empty)
          FailureTypeMap
        else
          PartialTypeMap(m1 ++ m2)
    }
    def empty: TypeMap = PartialTypeMap(Map.empty)
  }

  class DumbMapLattice[K, V] extends Lattice[Map[K, V]] {
    def join(current: Map[K, V], next: Map[K, V]): Map[K, V] = current ++ next
    def empty: Map[K, V] = Map.empty
  }

  object DumbTypeMapLattice extends DumbMapLattice[Symbol, DefaultCellCompleter[Type]]


  case class Parallel(val su: SymbolUniverse, parallelism: Int = 1) {

    val hasErrorsAtom: AtomicBoolean = new AtomicBoolean(false)

    val pool = new HandlerPool(parallelism, {(e: Throwable) =>
      e.printStackTrace() // TODO vs store exception
      hasErrorsAtom.lazySet(true)
    })

    // TODO smarter futureType that does not always fork? e.g. r(..., asFutureTypePlease=true) and chooses to make a future or not

    //val failureTypeMap = Map(su.newSymbol() -> Top)

    /* Eventually maps each TypeSymbol to a Type (which may contain other
     * TypeSymbols).
     */
    //val typeMapCellCompleter: DefaultCellCompleter[TypeMap] = newCellCompleter(pool, TypeMapLattice)

    // TODO maybe put everything in a single big lattice like:
    // Map[TypeSymbol, Thing]
    // Unknown <: Thing
    // Dep(dependencies: Set[TypeSymbol], action: => Unit) <: Thing
    // Done(typ: Type) <: Thing
    //
    // Then we can move all callbacks into big.onNext

    // TODO maybe put everything in a single big lattice like:
    // Map[TypeSymbol, Thing]
    // ThingConstraint(constraint: Constraint) <: Thing
    // Unknown <: Thing
    // Done(typ: Type) <: Thing
    //
    // Then we can move all callbacks into big.onNext

    val typeSymbolCellCompleters: DefaultCellCompleter[Map[Symbol, DefaultCellCompleter[Type]]] = newCellCompleter(pool, DumbTypeMapLattice)

    def newTypeSymbol(): Symbol = {
      val ts = su.newSymbol()
      val completer = newCellCompleter(pool, Lattice.trivial[Type])
      typeSymbolCellCompleters.putNext(Map(ts -> completer))
      ts
    }

    def onTypeSymbolSet(ts: Symbol)(f: Type => Unit): Unit = {
      onComplete(typeSymbolCellCompleters.cell.getResult()(ts).cell)(f)
    }

    def expandRoot(typ: Type, except: Set[Symbol] = Set())(cont: Type => Unit): Unit = typ match {
      case TypeSymbol(ts) if !except.contains(ts) =>
        onTypeSymbolSet(ts){typ =>
          expandRoot(typ, except)(cont)
        }
      case _ => cont(typ)
    }

    def expand(typ: Type, except: Set[Symbol] = Set())(cont: Type => Unit): Unit = typ match {
      case TypeSymbol(ts) if !except.contains(ts) =>
        onTypeSymbolSet(ts){typ =>
          expand(typ, except)(cont)
        }
      case FunType(x, xType, xResType) =>
        expand(xType, except){xType =>
          expand(xResType, except){xResType =>
            cont(FunType(x, xType, xResType))
          }
        }
      case RecType(x, xType) =>
        expand(xType, except){xType =>
          cont(RecType(x, xType))
        }
      case AndType(left, right) =>
        expand(left, except){left =>
          expand(right, except){right =>
            cont(AndType(left, right))
          }
        }
      case FieldDecl(a, aType) =>
        expand(aType, except){aType =>
          cont(FieldDecl(a, aType))
        }
      case TypeDecl(a, aLowerType, aUpperType) =>
        expand(aLowerType, except){aLowerType =>
          expand(aUpperType, except){aUpperType =>
            cont(TypeDecl(a, aLowerType, aUpperType))
          }
        }
      case _ => cont(typ)
    }

    def expandScope(scope: Scope, directlyUsedVars: Set[Symbol], exceptTypeSymbols: Set[Symbol] = Set(), alreadyExpanded: Scope = Map())(cont: Scope => Unit): Unit = {
      // TODO use DumbMapLattice to gather final scope?
      def expandList(list: List[(Symbol, Type)])(cont: List[(Symbol, Type)] => Unit): Unit = list match {
        case (ts, typ) :: rest =>
          expand(typ, exceptTypeSymbols){typ =>
            expandList(rest){rest =>
              cont((ts, typ) :: rest)
            }
          }
        case Nil => cont(Nil)
      }
      val varsToExpand = directlyUsedVars -- alreadyExpanded.keys
      if (varsToExpand.isEmpty)
        cont(alreadyExpanded)
      else
        expandList(varsToExpand.toList.map{x => (x, scope(x))}) { l1 =>
          val l2 = alreadyExpanded.toList
          val moreVars = (l1 ++ l2).flatMap{case (x, typ) =>
            NoFuture.allFreeVarsInType(typ)
          }.toSet
          val scope2 = (l1 ++ l2).toMap
          expandScope(scope, (moreVars -- directlyUsedVars) -- scope2.keySet, exceptTypeSymbols, scope2)(cont)
        }
    }

    def setTypeSymbol(ts: Symbol, typ: Type): Unit =
      typeSymbolCellCompleters.cell.getResult()(ts).putFinal(typ)

    def launch(f: => Unit): Unit = pool.execute{() => f}

    def parIf(c: Boolean)(f: => Unit): Unit =
      if (c) pool.execute{() => f} else f

    val parMinTotNumNodes = 10 // TODO

    def contCell[T](lattice: Lattice[T])(f: (T => Unit) => Unit): DefaultCell[T] = {
      val completer = newCellCompleter(pool, lattice)
      //pprint.pprintln(completer.cell)
      launch {
        f {res =>
            completer.putFinal(res)
        }
      }
      completer.cell
    }

//    def contZipFork[A >: Null, B >: Null](a: (A => Unit) => Unit, b: (B => Unit) => Unit)(cont: (A, B) => Unit): Unit = { // TODO test
//      val aCell = contCell(Lattice.trivial[A])(a)
//      val bCell = contCell(Lattice.trivial[B])(b)
//      onComplete(aCell) {aRes =>
//        onComplete(bCell) {bRes =>
//          cont(aRes, bRes)
//        }
//      }
//    }

//    def futureType(f: ((Type) => Unit) => Unit): Type = {
//      FutureType(contCell(Lattice.trivial[Type])(f))
//    }


//    def expandTermFutures(term: Term)(cont: (Term) => Unit): Unit = term match {
//      case TermFuture(cell) =>
//        onComplete(cell){
//          expandTermFutures(_)(cont)
//        }
//      case _ =>
//        def expandAssignedType(t: Term)(c: (Term) => Unit): Unit = {
//          t.assignedTypeOption match {
//            case Some(termType) =>
//              expandTypeFutures(termType){termType =>
//                c(t.withType(termType))
//              }
//            case None =>
//              c(t)
//          }
//        }
//
//        term match {
//          case Let(x, xTerm, resTerm) =>
//            expandTermFutures(xTerm){xTerm =>
//              expandTermFutures(resTerm){resTerm =>
//                expandAssignedType(Let(x, xTerm, resTerm).withTypeOption(term.assignedTypeOption))(cont)
//              }
//            }
//          case Obj(x, xType, body) =>
//            expandTypeFutures(xType){xType =>
//              expandDefFutures(body){body =>
//                expandAssignedType(Obj(x, xType, body).withTypeOption(term.assignedTypeOption))(cont)
//              }
//            }
//          case Fun(x, xType, body) =>
//            expandTypeFutures(xType){xType =>
//              expandTermFutures(body){body =>
//                expandAssignedType(Fun(x, xType, body).withTypeOption(term.assignedTypeOption))(cont)
//              }
//            }
//          case _ => // Var, App, Sel
//            expandAssignedType(term)(cont)
//        }
//    }
//
//    def expandDefFutures(d: Def)(cont: (Def) => Unit): Unit = d match {
//      case DefFuture(cell) =>
//        onComplete(cell){
//          expandDefFutures(_)(cont)
//        }
//      case _ =>
//        def expandAssignedType(d2: Def)(c: (Def) => Unit): Unit = {
//          d2.assignedTypeOption match {
//            case Some(termType) =>
//              expandTypeFutures(termType){termType =>
//                c(d2.withType(termType))
//              }
//            case None =>
//              c(d2)
//          }
//        }
//        d match {
//          case FieldDef(a, aTerm) =>
//            expandTermFutures(aTerm){aTerm =>
//              expandAssignedType(FieldDef(a, aTerm).withTypeOption(d.assignedTypeOption))(cont)
//            }
//          case AndDef(left, right) =>
//            expandDefFutures(left){left =>
//              expandDefFutures(right){right =>
//                expandAssignedType(AndDef(left, right).withTypeOption(d.assignedTypeOption))(cont)
//              }
//            }
//          case TypeDef(a, aType) =>
//            expandTypeFutures(aType){aType =>
//              expandAssignedType(TypeDef(a, aType).withTypeOption(d.assignedTypeOption))(cont)
//            }
//          case _ =>
//            ???
//        }
//    }
//
//
//    def contFold[A](first: ((A) => Unit) => Unit, seq: List[((A) => Unit) => Unit])(f: (A, A) => ((A) => Unit) => Unit)(cont: (A) => Unit): Unit = {
//      seq match {
//        case second :: rest =>
//          first {firstRes =>
//            second {secondRes =>
//              contFold(f(firstRes, secondRes), rest)(f)(cont)
//            }
//          }
//        case Nil => first(cont)
//      }
//    }
//
//    // TODO contCommutativeAssociativeReduce
//    def contReduce[A](seq: List[((A) => Unit) => Unit])(f: (A, A) => ((A) => Unit) => Unit)(cont: (A) => Unit): Unit = {
//      seq match {
//        case Nil => throw new UnsupportedOperationException()
//        case first :: rest =>
//          contFold(first, rest)(f)(cont)
//      }
//    }
//
//
//    def renameToUniqueVar(fromVar: Symbol, toVar: Symbol, t: Type)(cont: (Type) => Unit): Unit = t match {
//      case FutureType(cell) =>
//        onComplete(cell){actualT =>
//          renameToUniqueVar(fromVar, toVar, actualT)(cont)
//        }
//      case TypeProj(x, a) if x == fromVar =>
//        cont(TypeProj(toVar, a))
//      case FunType(x, xType, resType) if x != fromVar =>
//        // TODO assert(x != toVar)
//        val lazyRenamedXType = futureType{
//          renameToUniqueVar(fromVar, toVar, xType)(_)
//        }
//        val lazyRenamedResType = futureType{
//          renameToUniqueVar(fromVar, toVar, resType)(_)
//        }
//        cont(FunType(x, lazyRenamedXType, lazyRenamedResType))
//      case RecType(x, xType) if x != fromVar =>
//        // TODO assert(x != toVar)
//        val lazyRenamedXType = futureType{
//          renameToUniqueVar(fromVar, toVar, xType)(_)
//        }
//        cont(RecType(x, lazyRenamedXType)) // TODO vs non-lazy?
//      case FieldDecl(a, aType) =>
//        val lazyRenamedAType = futureType{
//          renameToUniqueVar(fromVar, toVar, aType)(_)
//        }
//        cont(FieldDecl(a, lazyRenamedAType)) // TODO vs non-lazy?
//      case TypeDecl(a, aLowerType, aUpperType) =>
//        val lazyRenamedALowerType = futureType{
//          renameToUniqueVar(fromVar, toVar, aLowerType)(_)
//        }
//        val lazyRenamedAUpperType = futureType{
//          renameToUniqueVar(fromVar, toVar, aUpperType)(_)
//        }
//        cont(TypeDecl(a, lazyRenamedALowerType, lazyRenamedAUpperType))
//      case AndType(left, right) =>
//        val lazyRenamedLeft = futureType{
//          renameToUniqueVar(fromVar, toVar, left)(_)
//        }
//        val lazyRenamedRight = futureType{
//          renameToUniqueVar(fromVar, toVar, right)(_)
//        }
//        cont(AndType(lazyRenamedLeft, lazyRenamedRight))
//      case _ =>
//        cont(t)
//    }
//
//    def exprRenameVar[T <: Expr](fromVar: Symbol, toVar: Symbol, e: T): T = e match { // TODO do in parallel and use TermFuture
//      case Var(x) if x == fromVar => Var(toVar).asInstanceOf[T]
//      case App(x, y) if x == fromVar || y == fromVar =>
//        val newX = if (x == fromVar) toVar else x
//        val newY = if (y == fromVar) toVar else y
//        App(newX, newY).asInstanceOf[T]
//      case Let(x, xTerm, t) if x != fromVar =>
//        Let(x, exprRenameVar(fromVar, toVar, xTerm), exprRenameVar(fromVar, toVar, t)).asInstanceOf[T]
//      case Sel(x, a) if x == fromVar =>
//        Sel(toVar, a).asInstanceOf[T]
//      case Obj(x, xType, d) if x != fromVar =>
//        val newXType = futureType{
//          renameToUniqueVar(fromVar, toVar, xType)(_)
//        }
//        Obj(x, newXType, exprRenameVar(fromVar, toVar, d)).asInstanceOf[T]
//      case Fun(x, xType, t) if x != fromVar =>
//        val newXType = futureType{
//          renameToUniqueVar(fromVar, toVar, xType)(_)
//        }
//        Fun(x, newXType, exprRenameVar(fromVar, toVar, t)).asInstanceOf[T]
//      case FieldDef(a, aTerm) =>
//        FieldDef(a, exprRenameVar(fromVar, toVar, aTerm)).asInstanceOf[T]
//      case TypeDef(a, aType) =>
//        val newAType = futureType{
//          renameToUniqueVar(fromVar, toVar, aType)(_)
//        }
//        TypeDef(a, newAType).asInstanceOf[T]
//      case AndDef(left, right) =>
//        AndDef(
//          exprRenameVar(fromVar, toVar, left),
//          exprRenameVar(fromVar, toVar, right)).asInstanceOf[T]
//      case _ => e
//    }
//
//    // TODO will raiseTo(..., classA, classA) when classA is partially lazy lead to problems?
//
//    // TODO replace killScope:Scope with killSet:Set[Symbol]?
//
    def contFuture[T >: Null](f: ((T) => Unit) => Unit): DefaultCell[T] = {
      contCell(Lattice.trivial[T])(f)
    }
//
//    private case class TermFuture(cell: DefaultCell[Term]) extends Term { // NOTE: Nested, so that Parallel.pool is in scope. Don't mix with other Parallel instances! // TODO pool as argument instead?
//      val treeHeight = 1
//      val totNumNodes = 1
//      assignedTypeOption = Some(futureType{cont =>
//        onComplete(cell){actualTerm =>
//          cont(actualTerm.assignedTypeOption.getOrElse(error()))
//        }
//      })
//      def withTypeOption(typeOption: Option[Type]) = {
//        val res = this.copy()
//        res.assignedTypeOption = typeOption
//        res
//      }
//    }
//
//    private case class DefFuture(cell: DefaultCell[Def]) extends Def { // NOTE: Nested, so that Parallel.pool is in scope. Don't mix with other Parallel instances!
//      val treeHeight = 1
//      val totNumNodes = 1
//      def withTypeOption(typeOption: Option[Type]) = {
//        val res = this.copy()
//        res.assignedTypeOption = typeOption
//        res
//      }
//    }
//
//    private def termFuture(f: ((Term) => Unit) => Unit) = this.TermFuture(contFuture(f))
//    private def defFuture(f: ((Def) => Unit) => Unit) = this.DefFuture(contFuture(f))
//
////    class LazyFutureType(atom: AtomicReference[FutureType], putter: (Type => Unit) => Unit) extends CanonicalPrototype {
////      val treeHeight = 2
////      val totNumNodes = 2
////
////      def get: CanonicalFuture = {
////        var current = atom.get()
////        while (current == null) {
////          val completer = newCellCompleter[Type](pool, Lattice.trivial)
////          val future = CanonicalFuture(completer.cell)
////          if (atom.compareAndSet(null, future)) {
////            launch {
////              putter {typ =>
////                completer.putFinal(typ)
////              }
////            }
////            current = future
////          } else {
////            freeze(completer)
////          }
////        }
////        current
////      }
////    }
//
//
    def error(): Type = {
      hasErrorsAtom.lazySet(true)
      ???
      ErrorType
    }
//
//
//
//    // TODO assert that types are not prototypes
//
//    // TODO INV: never return a type that is not in the given scope
//    // e.g.:
//    //   f: fun(x: {A: Int..Int})x.A
//    //   f({A = Int}): Int  // i.e. not x.A
//    // TODO circular dependencies between fields should always result in ErrorType?
//

    def annotateDef(d: Def): Typed.Def = {
      val typ = TypeSymbol(newTypeSymbol())
      d match {
        case FieldDef(a, aTerm)  => TypedFieldDef(a, annotateTerm(aTerm))              :- typ
        case TypeDef(a, aType)   => TypedTypeDef(a, aType)                             :- typ
        case AndDef(left, right) => TypedAndDef(annotateDef(left), annotateDef(right)) :- typ
      }
    }

    def annotateTerm(term: Term): Typed.Term = {
      val typ = TypeSymbol(newTypeSymbol())
      term match {
        case Var(x)                 => TypedVar(x)                                             :- typ
        case App(x, y)              => TypedApp(x, y)                                          :- typ
        case Let(x, xTerm, resTerm) => TypedLet(x, annotateTerm(xTerm), annotateTerm(resTerm)) :- typ
        case Sel(x, a)              => TypedSel(x, a)                                          :- typ
        case Obj(x, xType, body)    => TypedObj(x, xType, annotateDef(body))                   :- typ
        case Fun(x, xType, body)    => TypedFun(x, xType, annotateTerm(body))                  :- typ
        case TApp(funTerm, argDef)  => TypedTApp(annotateTerm(funTerm), annotateDef(argDef))   :- typ
      }
    }

    def expandDef(d: Typed.Def)(cont: Typed.Def => Unit): Unit = {
      expand(d.typ){typ =>
        d.d match {
          case TypedFieldDef(a, aTerm)  =>
            expandTerm(aTerm) { aTerm =>
              cont(TypedFieldDef(a, aTerm) :- typ)
            }
          case TypedTypeDef(a, aType)   =>
            cont(TypedTypeDef(a, aType) :- typ)
          case TypedAndDef(left, right) =>
            expandDef(left) { left =>
              expandDef(right) { right =>
                cont(TypedAndDef(left, right) :- typ)
              }
            }
        }
      }
    }

    def expandTerm(term: Typed.Term)(cont: Typed.Term => Unit): Unit = {
      expand(term.typ){typ =>
        term.term match {
          case TypedVar(x) =>
            cont(TypedVar(x) :- typ)
          case TypedApp(x, y) =>
            cont(TypedApp(x, y) :- typ)
          case TypedLet(x, xTerm, resTerm) =>
            expandTerm(xTerm) {xTerm =>
              expandTerm(resTerm) {resTerm =>
                cont(TypedLet(x, xTerm, resTerm) :- typ)
              }
            }
          case TypedSel(x, a) =>
            cont(TypedSel(x, a) :- typ)
          case TypedObj(x, xType, body) =>
            expandDef(body) {body =>
              cont(TypedObj(x, xType, body) :- typ)
            }
          case TypedFun(x, xType, body) =>
            expandTerm(body) {body =>
              cont(TypedFun(x, xType, body) :- typ)
            }
          case TypedTApp(funTerm, argDef) =>
            expandTerm(funTerm) {funTerm =>
              expandDef(argDef) {argDef =>
                cont(TypedTApp(funTerm, argDef)   :- typ)
              }
            }
        }
      }
    }

    def prototypeToType(prototype: Prototype): (Type, Set[Symbol]) = prototype match {
      case Que =>
        val ts = newTypeSymbol()
        (TypeSymbol(ts), Set(ts))
      case FunType(x, xType, xResType) =>
        val (newXType, leftSet) = prototypeToType(xType)
        val (newXResType, rightSet) = prototypeToType(xResType)
        (FunType(x, newXType, newXResType), leftSet ++ rightSet)
      case RecType(x, xType) =>
        val (newXType, innerSet) = prototypeToType(xType)
        (RecType(x, newXType), innerSet)
      case AndType(left, right) =>
        val (newLeft, leftSet) = prototypeToType(left)
        val (newRight, rightSet) = prototypeToType(right)
        (AndType(newLeft, newRight), leftSet ++ rightSet)
      case FieldDecl(a, aType) =>
        val (newAType, innerSet) = prototypeToType(aType)
        (FieldDecl(a, newAType), innerSet)
      case TypeDecl(a, aLowerType, aUpperType) =>
        val (newALowerType, leftSet) = prototypeToType(aLowerType)
        val (newAUpperType, rightSet) = prototypeToType(aUpperType)
        (TypeDecl(a, newALowerType, newAUpperType), leftSet ++ rightSet)
      case typ => (typ, Set())
    }

    def typeSymbolPatternToTypeProjPattern(typ: Type, solveSet: Set[Symbol], r: Int): Type = typ match {
      case TypeSymbol(ts) if solveSet(ts) =>
        TypeProj(r, ts)
      case FunType(x, xType, xResType) =>
        FunType(x,
          typeSymbolPatternToTypeProjPattern(xType, solveSet, r),
          typeSymbolPatternToTypeProjPattern(xResType, solveSet, r))
      case RecType(x, xType) =>
        RecType(x, typeSymbolPatternToTypeProjPattern(xType, solveSet, r))
      case AndType(left, right) =>
        AndType(
         typeSymbolPatternToTypeProjPattern(left, solveSet, r),
         typeSymbolPatternToTypeProjPattern(right, solveSet, r))
      case FieldDecl(a, aType) =>
        FieldDecl(a,  typeSymbolPatternToTypeProjPattern(aType, solveSet, r))
      case TypeDecl(a, aLowerType, aUpperType) =>
        TypeDecl(a,
         typeSymbolPatternToTypeProjPattern(aLowerType, solveSet, r),
         typeSymbolPatternToTypeProjPattern(aUpperType, solveSet, r))
      case typ => typ
    }

    def gatherConstraint(scope: Scope, zOption: Option[Symbol], solveSet: Set[Symbol], lower: Type, upper: Type)(cont: Constraint => Unit): Unit = {
      expand(lower, solveSet) { lower =>
        expand(upper, solveSet) { upper =>
          val directlyUsedVars = NoFuture.allFreeVarsInType(lower) ++ NoFuture.allFreeVarsInType(upper)
          expandScope(scope, directlyUsedVars, solveSet) { relevantScope =>
            // TODO make NoFuture.gather use TypeSymbol instead of TypeProj
            // for unknown-types-that-are-being-solved-for.
            val r = -1
            if (relevantScope.contains(r))
              throw new TypecheckingError(s"var $r is already in use")
            val newLower = typeSymbolPatternToTypeProjPattern(lower, solveSet, r)
            val newUpper = typeSymbolPatternToTypeProjPattern(upper, solveSet, r)
            val newSolveSet = solveSet.map{ts => TypeProj(r, ts)}
            val constraint = NoFuture.gatherConstraints(relevantScope, newSolveSet, zOption, newLower, newUpper, patternIsLeft=false) // TODO simplify(lowerType)?
            cont(constraint)
          }
        }
      }
    }
    def solveConstraint(topScope: Scope, zOption: Option[Symbol], solveSet: Set[Symbol], solveSetVariance: Map[Symbol, Variance], constraint: Constraint, topVariance: Variance, typ: Type): Unit = {
      def freeVarsInConstraint(constraint: Constraint): Set[Symbol] = constraint match {
        case OrConstraint(left, right) =>
          freeVarsInConstraint(left) ++ freeVarsInConstraint(right)
        case AndConstraint(left, right) =>
          freeVarsInConstraint(left) ++ freeVarsInConstraint(right)
        case MultiAndConstraint(m) =>
          m.flatMap{case (p, (s, l, u)) => s.keySet}.toSet.intersect(topScope.keySet)
        case TrueConstraint => Set()
        case FalseConstraint => Set()
      }
      expand(typ, solveSet) { typ =>
        expandScope(topScope, freeVarsInConstraint(constraint), solveSet) { relevantScope =>
          val r = -1
          val newSolveSet = solveSet.map{ts => TypeProj(r, ts)}
          val newSolveSetVariance = solveSetVariance.map{case (ts, v) => TypeProj(r, ts) -> v}
          val resOpt = NoFuture.solveConstraint2(relevantScope, zOption, newSolveSet, newSolveSetVariance, constraint, typeSymbolPatternToTypeProjPattern(typ, solveSet, r), topVariance)
          resOpt match {
            case Some(res) =>
              res.foreach{case (TypeProj(_, ts), tsType) =>
                setTypeSymbol(ts, tsType)
              }
            case None =>
              throw new TypecheckingError()
          }
        }
      }
    }

    def gatherVariance(solveSet: Set[Symbol], typ: Type, variance: Variance)(cont: Map[Symbol, Variance] => Unit): Unit = {
      def inner(solveSet: Set[Symbol], typ: Type, variance: Variance): Map[Symbol, Variance] = typ match {
        case FunType(x, xType, xResType) =>
          mapUnion(
            inner(solveSet, xType, reverseVariance(variance)),
            inner(solveSet,xResType, variance)
          ){mergeVariance(_, _)}
        case AndType(left, right) =>
          mapUnion(
            inner(solveSet, left, variance),
            inner(solveSet, right, variance)
          ){mergeVariance(_, _)}
        case TypeDecl(a, aLowerType, aUpperType) =>
          mapUnion(
            inner(solveSet, aLowerType, reverseVariance(variance)),
            inner(solveSet, aUpperType, variance)
          ){mergeVariance(_, _)}
        case FieldDecl(a, aType) =>
          inner(solveSet, aType, variance)
        case TypeSymbol(ts) if solveSet(ts) =>
          Map(ts -> variance)
        case _ => Map()
      }

      expand(typ, solveSet){typ =>
        cont(inner(solveSet, typ, variance))
      }
    }

    def raise(scope: Scope, zOption: Option[Symbol], solveSet: Set[Symbol], lower: Type, upper: Type): Unit = {
      val variance = Covariant
      gatherVariance(solveSet, upper, variance) { solveSetVariance =>
        gatherConstraint(scope, zOption, solveSet, lower, upper) { constraint =>
          solveConstraint(scope, zOption, solveSet, solveSetVariance, constraint, variance, upper)
        }
      }
    }

    /** Find the least supertype of `typ` such that `killSet` is not free.
     * The result will be put to `typeMapCellCompleter` as `ts`.
     */
    def elimUp(scope: Scope, killSet: Set[Symbol], zOption: Option[Symbol], typ: Type, ts: Symbol): Unit = {
      expand(typ) { typ =>
        val directlyUsedVars = NoFuture.allFreeVarsInType(typ)
        expandScope(scope, directlyUsedVars) { relevantScope =>
          setTypeSymbol(ts, NoFuture.eliminateVars(relevantScope, killSet, zOption, typ))
        }
      }
    }

    def typeRenameVar(fromVar: Symbol, toVar: Symbol, typ: Type, ts: Symbol): Unit = {
      expand(typ) { typ =>
        setTypeSymbol(ts, NoFuture.typeRenameVar(fromVar, toVar, typ))
      }
    }

    def typecheckDef(d: Typed.Def, prototype: Prototype = Que, scope: Scope = Map()): Unit = (d, prototype) match {
      case (TypedAndDef(left, right) :- TypeSymbol(ts), p) =>
        val (typ, solveSet) = prototypeToType(p)
        setTypeSymbol(ts, typ)

        val leftDom = NoFuture.dom(left)
        val rightDom = NoFuture.dom(right)
        val overlap = leftDom.intersect(rightDom)
        if (!overlap.isEmpty)
          throw new TypecheckingError(s"overlapping doms: $overlap")

        val stdPrototype = NoFuture.objStdDecl(scope, -1, p) // NOTE: expect any rectypes and typesymbols to have been unwrapped.

        val missingDefs = stdPrototype.dom -- (leftDom ++ rightDom)
        if (!missingDefs.isEmpty)
          throw new TypecheckingError(s"missing defs: $missingDefs")


        val (leftStdPrototype, rightStdPrototype) = stdPrototype.split(leftDom, rightDom)
        val leftPrototype = leftStdPrototype.toType
        val rightPrototype = rightStdPrototype.toType

        parIf(left.totNumNodes >= parMinTotNumNodes) {
          typecheckDef(left, leftPrototype, scope)
        }
        parIf(right.totNumNodes >= parMinTotNumNodes) {
          typecheckDef(right, leftPrototype, scope)
        }

        raise(scope, None, solveSet, AndType(left.typ, right.typ), typ)

      case (TypedFieldDef(a, aTerm) :- TypeSymbol(ts), Que) =>
        setTypeSymbol(ts, FieldDecl(a, aTerm.typ))
        typecheckTerm(aTerm, Que, scope)
      case (TypedFieldDef(a, aTerm) :- TypeSymbol(ts), Top) =>
        setTypeSymbol(ts, Top)
        typecheckTerm(aTerm, Top, scope)
      case (TypedFieldDef(a, aTerm) :- TypeSymbol(ts), FieldDecl(b, bPrototype)) if a == b =>
        val (typ, solveSet) = prototypeToType(FieldDecl(a, bPrototype))
        setTypeSymbol(ts, typ)

        launch {
          typecheckTerm(aTerm, bPrototype, scope)
        }

        raise(scope, None, solveSet, FieldDecl(a, aTerm.typ), typ)

      case (TypedTypeDef(a, aType) :- TypeSymbol(ts), Top) =>
        setTypeSymbol(ts, Top)
      case (TypedTypeDef(a, aType) :- TypeSymbol(ts), p) =>
        val (typ, solveSet) = prototypeToType(p)
        setTypeSymbol(ts, typ)
        raise(scope, None, solveSet, TypeDecl(a, aType, aType), typ)

      case _ => ???
    }

    def typecheckTerm(term: Typed.Term, prototype: Prototype = Que, scope: Scope = Map()): Unit = (term, prototype) match {
      case (TypedVar(x) :- TypeSymbol(ts), p) =>
        val (typ, solveSet) = prototypeToType(p)
        setTypeSymbol(ts, typ)
        raise(scope, Some(x), solveSet, scope(x), typ)

      case (TypedLet(x, xTerm, resTerm) :- TypeSymbol(ts), p) =>
        if (scope.contains(x)) ??? // TODO rename

        parIf(xTerm.totNumNodes >= parMinTotNumNodes) {
          typecheckTerm(xTerm, Que, scope)
        }

        parIf(resTerm.totNumNodes >= parMinTotNumNodes) {
          typecheckTerm(resTerm, p, scope + (x -> xTerm.typ))

          // TODO when resTerm.typ complete,
          // setTypeSymbol(ts, elimUp(scope + (x -> xTerm.typ), resTerm.typ, zOption, resTerm.typ))

          val zOption = resTerm.term match {
            case TypedVar(z) => Some(z)
            case _           => None
          }
          elimUp(scope + (x -> xTerm.typ), Set(x), zOption, resTerm.typ, ts)
        }

      case (TypedSel(x, a) :- TypeSymbol(ts), p) =>
        val (aType, solveSet) = prototypeToType(p)
        setTypeSymbol(ts, aType)
        launch {
          raise(scope, Some(x), solveSet, scope(x), FieldDecl(a, aType))
        }

      case (TypedApp(x, y) :- TypeSymbol(ts), p) =>
        val (appResType, solveSet2) = prototypeToType(p)
        setTypeSymbol(ts, appResType)

        val z = su.newSymbol()
        val (funType @ FunType(_, funArgType, funResType), solveSet) = prototypeToType(FunType(z, Que, Que))

        launch {
          gatherVariance(solveSet, funType, Covariant) { solveSetVariance  =>
            gatherConstraint(scope, Some(x), solveSet, scope(x), funType) { c1 =>
              gatherConstraint(scope, Some(y), solveSet, scope(y), funArgType) { c2 => // TODO vs allow arg to constrain function? e.g. overloading.
                solveConstraint(scope, Some(x), solveSet, solveSetVariance, AndConstraint(c1, c2), Covariant, funType)
              }
            }
          }
        }

        launch {
          val ts1 = newTypeSymbol()
          typeRenameVar(z, y, funResType, ts1)
          raise(scope, None, solveSet2, TypeSymbol(ts1), appResType)
        }

      case (TypedFun(x, xType, resTerm) :- TypeSymbol(ts), Que) =>
        setTypeSymbol(ts, FunType(x, xType, resTerm.typ))
        typecheckTerm(resTerm, Que, scope + (x -> xType))

      // TODO TypedFun with p=TypeProj, p=AndType, etc...

      case (TypedFun(x, xType, resTerm) :- TypeSymbol(ts), Top) =>
        setTypeSymbol(ts, Top)
        typecheckTerm(resTerm, Top, scope + (x -> xType))

      case (TypedFun(x, xType, xResType) :- TypeSymbol(ts), FunType(y, _, _)) if x != y =>
        ??? // TODO rename y to x and back?

      case (TypedFun(x, xType, resTerm) :- TypeSymbol(ts), p @ FunType(y, argPrototype, resPrototype)) if x == y =>
        val (funType, solveSet) = prototypeToType(p)
        setTypeSymbol(ts, funType)

        parIf(resTerm.totNumNodes >= parMinTotNumNodes) {
          typecheckTerm(resTerm, resPrototype, scope + (x -> xType))
          raise(scope, None, solveSet, FunType(x, xType, resTerm.typ), funType)
        }

      case (TypedObj(x, xType, defs) :- TypeSymbol(ts), p) =>
        setTypeSymbol(ts, RecType(x, xType))

        launch {
          val (objType, solveSet) = prototypeToType(p)
          raise(scope, None, solveSet, RecType(x, xType), objType)

          val localScope = scope + (x -> xType)
          val std = NoFuture.objStdDecl(localScope, x, xType)
          typecheckDef(defs, std.toType, localScope)
        }

      // TODO TApp, etc.
      case _ =>
        ???
    }

    def run[T >: Null](f: ((T) => Unit) => Unit): Option[T] = {
      val rootPromise = Promise[Option[T]]()
      launch {
        val rootCell = contFuture[T](f)
        onComplete(rootCell){rootType =>
          val res = if (hasErrorsAtom.get()) None else Some(rootType)
          rootPromise.success(res)
        }
      }
      try {
        Await.result(pool.quiescentIncompleteCells, 10.seconds)
        typeSymbolCellCompleters.putFinal(Map()) // TODO hack.
        val incompleteCellsAtTheEnd = Await.result(pool.quiescentIncompleteCells, 10.seconds)
        if (incompleteCellsAtTheEnd.size != 0) {
          if (hasErrorsAtom.get())
            throw new NotImplementedError("quiescent with incomplete cells AND errors")
          else
            throw new TimeoutException("quiescent with incomplete cells")
          None
        }
        await(rootPromise.future)
      } catch {
        case e: TimeoutException => e.printStackTrace(); None
        case e: NotImplementedError => e.printStackTrace(); None
      }
    }

  } // end class Parallel

  def typecheckInParallel(su: SymbolUniverse, rootExpr: Term, rootPrototype: Prototype = Que, rootScope: Map[Symbol, Type] = Map(), parallelism: Int = 1): Option[Typed.Term] = {
    val par = Parallel(su, parallelism)

    par.run[Typed.Term]{cont =>
      val typedTerm = par.annotateTerm(rootExpr)
      par.typecheckTerm(typedTerm, rootPrototype, rootScope)
      par.expandTerm(typedTerm)(cont)
    ////  val lazyTypedTerm = par.typecheckTerm(rootExpr, rootPrototype, rootScope)
    ////  ???
    ////  //par.expandTermFutures(lazyTypedTerm) { typedTerm =>
    ////  //  cont(typedTerm)
    ////  //}
    }
  }
}
