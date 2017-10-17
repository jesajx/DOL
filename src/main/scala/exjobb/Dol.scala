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
  // typecheck in two steps: 1) gather constraints, 2) solve them. This is
  // similar what I tried with Hindley-Milner. Technically this is actually
  // what is happening in the current implementation (gather and solve are
  // done concurrently), but it could be beneficial to make it more explicit
  // (simpler and more machine-like code).


  sealed trait Tree {
    val treeHeight: Int
    val totNumNodes: Int
    // TODO
  }
  sealed trait Expr extends Tree {
    @volatile var assignedTypeOption: Option[Type] = None // TODO does it need to be volatile? // TODO vs trait TypedExpr?

    type ThisType <: Expr
    def withTypeOption(typeOption: Option[Type]): ThisType

    def withType(typ: Type): ThisType = withTypeOption(Some(typ))
    def assignedType = assignedTypeOption.get

    def withTypeOptionHelper[E <: Expr](clonedExpr: E, typeOption: Option[Type]): E = {clonedExpr.assignedTypeOption = typeOption; clonedExpr}
  }
  sealed trait Term  extends Expr {
    type ThisType <: Term
  }
  sealed trait Value extends Term {
    type ThisType <: Value
  }
  sealed trait Type  extends Tree
  sealed trait Decl  extends Type
  sealed trait Def   extends Expr {
    type ThisType <: Def
  }


  // Term ::=
  case class Var(x: Symbol) extends Term {
    type ThisType = Var
    val treeHeight = 1
    val totNumNodes = 1
    def withTypeOption(typeOption: Option[Type]) = {
      val res = this.copy()
      res.assignedTypeOption = typeOption
      res
    }
  }
  case class App(x: Symbol, y: Symbol) extends Term {
    type ThisType = App
    val treeHeight = 1
    val totNumNodes = 1
    def withTypeOption(typeOption: Option[Type]) = withTypeOptionHelper(copy(), typeOption)
  }
  final case class Let(x: Symbol, xTerm: Term, resTerm: Term) extends Term {
    type ThisType = Let
    val treeHeight = 1 + math.max(xTerm.treeHeight, resTerm.treeHeight)
    val totNumNodes = 1 + xTerm.totNumNodes + resTerm.totNumNodes
    def withTypeOption(typeOption: Option[Type]) = withTypeOptionHelper(copy(), typeOption)
  }
  case class Sel(x: Symbol, a: Symbol) extends Term {
    type ThisType = Sel
    val treeHeight = 1
    val totNumNodes = 1
    def withTypeOption(typeOption: Option[Type]) = withTypeOptionHelper(copy(), typeOption)
  }

  // Value ::=
  case class Obj(x: Symbol, xType: Type, body: Def) extends Value {
    type ThisType = Obj
    val treeHeight = 1 + math.max(xType.treeHeight, body.treeHeight)
    val totNumNodes = 1 + xType.totNumNodes + body.totNumNodes
    def withTypeOption(typeOption: Option[Type]) = withTypeOptionHelper(copy(), typeOption)
  }
  case class Fun(x: Symbol, xType: Type, body: Term) extends Value {
    type ThisType = Fun
    val treeHeight = 1 + math.max(xType.treeHeight, body.treeHeight)
    val totNumNodes = 1 + xType.totNumNodes + body.totNumNodes
    def withTypeOption(typeOption: Option[Type]) = withTypeOptionHelper(copy(), typeOption)
  }

  // Def ::=
  case class FieldDef(a: Symbol, aTerm: Term) extends Def {
    type ThisType = FieldDef
    val treeHeight = 1 + aTerm.treeHeight
    val totNumNodes = 1 + aTerm.totNumNodes
    def withTypeOption(typeOption: Option[Type]) = withTypeOptionHelper(copy(), typeOption)
  }
  case class TypeDef(a: Symbol, aType: Type) extends Def {
    type ThisType = TypeDef
    val treeHeight = 1 + aType.treeHeight
    val totNumNodes = 1 + aType.totNumNodes
    def withTypeOption(typeOption: Option[Type]) = withTypeOptionHelper(copy(), typeOption)
  }
  case class AndDef(left: Def, right: Def)  extends Def {
    type ThisType = AndDef
    val treeHeight = 1 + math.max(left.treeHeight, right.treeHeight)
    val totNumNodes = 1 + left.totNumNodes + right.totNumNodes
    def withTypeOption(typeOption: Option[Type]) = withTypeOptionHelper(copy(), typeOption)
  }

  // DOL Extensions to DOT // TODO
  //case class TSel(t: Term, a: Symbol) extends Term { // Let(x, t, Sel(x,a))
  //case class TApp(t: Term, d: Def) extends Term // Let(x, t, Let(y, Obj(r, <generated>, <generated>), App(x, y)))
  //
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
    case (Covariant, Contravariant) => Invariant
    case (Contravariant, Covariant) => Invariant

    // stay still
    case (Invariant, _) => Invariant
    case (_, Invariant) => Invariant

    // whatever
    case (ConstantVariance, _) => b
    case (_, ConstantVariance) => a
  }

  type TypedTerm = Term // TODO Do something a bit more interesting here? Only serves as documentation right now.
  type TypedDef = Def // TODO Do something a bit more interesting here? Only serves as documentation right now.
  //sealed trait Typed { // TODO maybe something like this?
  //  val assignedTypeOption: Type
  //}
  //trait TypedTerm extends Term with Typed

  def pairToList[T](pair: (T, T)): List[T] = List(pair._1, pair._2)
  def max(rest: Int*): Int = rest.toList.max







  object NoFuture {

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
      for {
        xType <- scope.get(x)
        aUpperType <- inner(scope, xType, a, visited + TypeProj(x, a))
      } yield aUpperType
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
        case Bot => Some(Bot) // TODO <: TypeDecl(a, x.a..x.a) <: Top?
        case _   => None
      }
      for {
        xType <- scope.get(x)
        aUpperType <- inner(scope, xType, a, visited + TypeProj(x, a))
      } yield aUpperType
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
    def select(su: SymbolUniverse, scope: Scope, x: Symbol, a: Symbol): Option[Type] = {
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

    sealed trait Constraint

    case object TrueConstraint extends Constraint
    case object FalseConstraint extends Constraint
    case class OrConstraint(left: Constraint, right: Constraint) extends Constraint
    case class AndConstraint(left: Constraint, right: Constraint) extends Constraint
    case class SubtypeConstraint(scope: Scope, left: Type, right: Type, variance: Variance) extends Constraint // TODO add Variance here?
    // TODO replace AndConstraint and OrConstraint with mergine constraints?
    // and({A <: X <: B}, {C <: X <: D}) = {max(A, C) <: X <: min(B, D)}
    // or({A <: X <: B}, {C <: X <: D}) = {min(A, C) <: X <: max(B, D)}?
    // Will glb=min and lub=max work?
    // min=AndType?

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

    def varEliminatingTransform(su: SymbolUniverse, scope: Scope, fromVar: Symbol, toVar: Symbol, typ: Type): Type = { // TODO better name
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
    def leastCommonSupertype(scope: Scope, lhs: Type, rhs: Type): Type = {
      def inner(scope: Scope, lhs: Type, rhs: Type, visitedLeft: Set[TypeProj], visitedRight: Set[TypeProj]): Type = (lhs, rhs) match {
        case (Top, _) | (_, Top) => Top
        case (Bot, _) => rhs
        case (_, Bot) => lhs
        case (ErrorType, _) | (_, ErrorType) => ErrorType
        case (_: RecType, _: RecType) if (rigidEqualTypes(lhs, rhs)) => lhs

        case (AndType(left, right), _) =>
          andType(
            inner(scope, left, rhs, visitedLeft, visitedRight),
            inner(scope, right, rhs, visitedLeft, visitedRight))
        case (_, AndType(_, _)) =>
          inner(scope, rhs, lhs, visitedRight, visitedLeft)

        case (TypeProj(x, a), TypeProj(y, b)) if lhs == rhs => lhs
        case (aProj @ TypeProj(x, a), bProj @ TypeProj(y, b)) if lhs != rhs =>
          val aUpperType = if (visitedLeft(aProj)) Top else typeProjectUpper(scope, x, a).getOrElse{error()}
          val bUpperType = if (visitedRight(bProj)) Bot else typeProjectUpper(scope, y, b).getOrElse{error()}

          andType(
            inner(scope, aUpperType, rhs, visitedLeft + aProj, visitedRight),
            inner(scope, lhs, bUpperType, visitedLeft, visitedRight + bProj)) // TODO is this correct?

        case (aProj @ TypeProj(x, a), _) =>
          val aUpperType = if (visitedLeft(aProj)) Top else typeProjectUpper(scope, x, a).getOrElse{error()}
          inner(scope, aUpperType, rhs, visitedLeft + aProj, visitedRight)
        case (_, bProj @ TypeProj(y, b))  =>
          val bUpperType = if (visitedRight(bProj)) Bot else typeProjectUpper(scope, y, b).getOrElse{error()}
          inner(scope, lhs, bUpperType, visitedLeft, visitedRight + bProj)

        case (FunType(x, xType, xResType), FunType(y, yType, yResType)) if x != y =>
          inner(scope, lhs, typeRenameBoundVarAssumingNonFree(x, rhs), visitedLeft, visitedRight)
        case (FunType(x, xType, xResType), FunType(y, yType, yResType)) if x == y =>
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
      inner(scope, lhs, rhs, Set(), Set())
    }

    def greatestCommonSubtype(scope: Scope, left: Type, right: Type): Type = andType(left, right) // TODO always correct? // TODO simplify?

    // TODO isSubtypeOf(scope, a, b)?

    def solveConstraint(su: SymbolUniverse, solveSet: Set[TypeProj], constraint: Constraint): Option[Map[TypeProj, Type]] = { // TODO

      def mergeConstraints(left: Map[TypeProj, (Scope, Type, Type, Variance)], right: Map[TypeProj, (Scope, Type, Type, Variance)]): Map[TypeProj, (Scope, Type, Type, Variance)] = {
        mapUnion(left, right){case ((s1, l1, u1, v1), (s2, l2, u2, v2)) =>
          val commonScope = s1 ++ s2 // TODO will concatenating scopes work? If s1(x) == s2(x) and or "x" has otherwise been renamed... having different scopes seems wrong in the first place...
          val commonVariance = mergeVariance(v1, v2)
          (commonScope, leastCommonSupertype(commonScope, l1, l2), greatestCommonSubtype(commonScope, u1, u2), commonVariance)
        }
      }

      val defaults = solveSet.map(p => (p -> (Map(): Scope, Bot, Top, ConstantVariance))).toMap

      def subsolve(constraint: Constraint): Option[Map[TypeProj, (Scope, Type, Type, Variance)]] = constraint match {
        case AndConstraint(left, right) =>
          for {
            leftRes  <- subsolve(left)
            rightRes <- subsolve(right)
            merged <- Some(mergeConstraints(leftRes, rightRes)) // TODO check if !(lower <: upper) here?
          } yield merged
        case TrueConstraint =>
          Some(defaults)
        case SubtypeConstraint(scope, left, proj: TypeProj, variance)  if solveSet(proj) =>
          Some(defaults + (proj -> (scope, left, Top, variance)))
        case SubtypeConstraint(scope, proj: TypeProj, right, variance) if solveSet(proj) =>
          Some(defaults + (proj -> (scope, Bot, right, variance)))
        case _: OrConstraint =>
          ??? // should not happen if DNF.
        case _ => // FalseConstraints and bad SubtypeConstraints
          None
      }

      sealed trait SolveResult
      case class  Solution(solution: Map[TypeProj, (Scope, Type, Variance)]) extends SolveResult
      case object Inconsistent extends SolveResult // Meaning: There are multiple solutions, but they can not be merged. E.g. x.T=fun | x.T=obj.
      case object NoSolution extends SolveResult

      def solve(constraint: Constraint): SolveResult = constraint match {
        case OrConstraint(left, right) =>
          (solve(left), solve(right)) match {
            case (Inconsistent, _) => Inconsistent
            case (_, Inconsistent) => Inconsistent
            case (NoSolution, rightRes) => rightRes
            case (leftRes, NoSolution)  => leftRes
            case (Solution(leftSolution), Solution(rightSolution)) =>
              // TODO assert (leftSolution.keys == rightSolution.keys)
              // TODO min solution? how? what if some left and some right are min? check [pierce00]...
              // TODO depends on variance somehow?
              // TODO what if difference variance on each side?
              // TODO is this correct?

              //println(leftSolution, rightSolution)

              if (leftSolution.keys != rightSolution.keys) ??? // not supposed to happen.

              val variancesMatch = mapIntersect(leftSolution, rightSolution){
                case ((_,_,v1), (_,_,v2)) if v1 == v2 => true
                case ((_,_,_), (_,_,ConstantVariance)) => true
                case ((_,_,ConstantVariance), (_,_,_)) => true
                case _ => false
              }.forall{_._2}

              val leftIsMin = mapIntersect(leftSolution, rightSolution){case ((s1,t1,v1), (s2,t2,v2)) =>
                val scope = s1 ++ s2 // TODO correct?
                if (mergeVariance(v1, v2) == Contravariant)
                  isSubtypeOf(scope, t2, t1)
                else
                  isSubtypeOf(scope, t1, t2)
              }.forall{_._2}

              val rightIsMin = mapIntersect(leftSolution, rightSolution){case ((s1,t1,v1), (s2,t2,v2)) =>
                val scope = s1 ++ s2 // TODO correct?
                isSubtypeOf(scope, t1, t2)
              }.forall{_._2}

              if (!variancesMatch)
                NoSolution
              else if (leftIsMin)
                Solution(leftSolution)
              else if (rightIsMin)
                Solution(rightSolution)
              else
                NoSolution
          }
        case _ =>
          subsolve(constraint) match {
            case Some(res) =>
              val badBounds = res.exists{case (_, (scope, lower, upper, variance)) =>
                (!isSubtypeOf(scope, lower, upper)
                  || (variance == Invariant
                    && !isSubtypeOf(scope, upper, lower)))
              }
              if (badBounds) {
                NoSolution
              } else {
                Solution(res.map{case (proj, (scope, lower, upper, variance)) =>
                  val typ = variance match {
                    case Covariant        => lower
                    case Contravariant    => upper
                    case ConstantVariance => lower
                    case Invariant        => lower
                  }
                  proj -> (scope, typ, variance)
                })
              }
            case None => NoSolution
          }
      }

      solve(dnf(constraint)) match {
        case Solution(res) => Some(res.map{case (p, (_, typ, _)) => p -> typ})
        case Inconsistent  => ???; None
        case NoSolution    => None
      }
    }

    def dnf(constraint: Constraint): Constraint = constraint match { // TODO maybe dnf instead?
      case OrConstraint(left, right)                       => orConstraint(dnf(left), dnf(right))
      case AndConstraint(OrConstraint(left, right), other) => orConstraint(dnf(andConstraint(left, other)), dnf(andConstraint(right, other)))
      case AndConstraint(other, OrConstraint(left, right)) => orConstraint(dnf(andConstraint(other, left)), dnf(andConstraint(other, right)))
      case _                                               => constraint
    }

    def andConstraint(left: Constraint, right: Constraint): Constraint = (left, right) match {
      case (TrueConstraint, _)  => right
      case (_, TrueConstraint)  => left
      case (FalseConstraint, _) => FalseConstraint
      case (_, FalseConstraint) => FalseConstraint
      case _ => AndConstraint(left, right)
    }

    def orConstraint(left: Constraint, right: Constraint): Constraint = (left, right) match {
      case (TrueConstraint, _)  => TrueConstraint
      case (_, TrueConstraint)  => TrueConstraint
      case (FalseConstraint, _) => right
      case (_, FalseConstraint) => left
      case _ => OrConstraint(left, right)
    }

    def prep(su: SymbolUniverse, z: Symbol, to: Prototype): (Type, Set[TypeProj]) = { // TODO better name.
      def r(to: Prototype): (Type, Set[TypeProj]) = to match {
        case Que =>
          val a = su.newSymbol()
          val proj = TypeProj(z, a)
          (proj, Set(proj))
        case AndType(left, right) =>
          val (leftRes, leftSet)   = r(left)
          val (rightRes, rightSet) = r(right)
          (andType(leftRes, rightRes), leftSet ++ rightSet)
        case FunType(x, xType, resType) =>
          val (newXType, leftSet) = r(xType)
          val (newResType, rightSet) = r(resType)
          (FunType(x, newXType, newResType), leftSet ++ rightSet)
        case RecType(x, xType) =>
          val (newXType, set) = r(xType)
          (RecType(x, newXType), set)
        case FieldDecl(a, aType) =>
          val (newAType, set) = r(aType)
          (FieldDecl(a, newAType), set)
        case TypeDecl(a, aLowerType, aUpperType) =>
          val (newLowerType, leftSet) = r(aLowerType)
          val (newUpperType, rightSet) = r(aUpperType)
          (TypeDecl(a, newLowerType, newUpperType), leftSet ++ rightSet)
        case _ => (to, Set())
      }
      r(to)
    }

    def applyConstraintSolution(typ: Type, solution: Map[TypeProj, Type]): Type = typ match {
      case AndType(left, right) =>
        andType(applyConstraintSolution(left, solution), applyConstraintSolution(right, solution))
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
      case proj: TypeProj if solution.contains(proj) =>
        solution(proj)
      case _ => typ
    }

    def gather(su: SymbolUniverse, rootScope: Scope, solveSet: Set[TypeProj], rootFrom: Type, rootTo: Prototype, variance: Variance): Constraint = {
      // TODO Instead of passing down variance, maybe just flip the order of
      // from and to? (from,to) -> (to, from)?
      def gath(scope: Scope, from: Type, to: Type, visitedUp: Set[TypeProj], visitedDown: Set[TypeProj], variance: Variance): Constraint = (from, to) match {
        case (FieldDecl(a, aType), Top) => gath(scope, from, FieldDecl(a, Top), visitedUp, visitedDown, variance) // TODO TrueConstraint? still necessary to decide on variance?
        case (Bot, FieldDecl(a, aType)) => gath(scope, FieldDecl(a, Bot), to, visitedUp, visitedDown, variance)

        case (Bot, TypeDecl(a, aLowerType, aUpperType)) => ??? // TODO What IS THE smallest TypeDecl? TypeDecl(a, x.a, x.a)?
        case (TypeDecl(a, aLowerType, aUpperType), Top) => gath(scope, from, TypeDecl(a, Bot, Top), visitedUp, visitedDown, variance)

        case (FunType(x, _, _), Top) => gath(scope, from, FunType(x, Bot, Top), visitedUp, visitedDown, variance)
        case (Bot, FunType(x, _, _)) => gath(scope, FunType(x, Top, Bot), to, visitedUp, visitedDown, variance)

        case (RecType(x, xType), _) =>
          if (scope.contains(x)) ??? // TODO
          gath(scope + (x -> xType), xType, to, visitedUp, visitedDown, variance)
        case (_, RecType(y, yType)) =>
          if (scope.contains(y)) ??? // TODO
          gath(scope + (y -> yType), from, yType, visitedUp, visitedDown, variance)

        case (FunType(x, xType, xResType), FunType(y, yType, yResType)) if x != y =>
          andConstraint(
            gath(scope, yType, xType, visitedUp, visitedDown, reverseVariance(variance)),
            gath(scope + (x -> xType) + (y -> yType), xResType, yResType, visitedUp, visitedDown, variance)) // TODO technically: x==y && (x -> min(xType, yType))
        case (FunType(x, xType, xResType), FunType(y, yType, yResType)) if x == y =>
          andConstraint(
            gath(scope, yType, xType, visitedUp, visitedDown, reverseVariance(variance)),
            gath(scope + (x -> andType(xType, yType)), xResType, yResType, visitedUp, visitedDown, variance)) // TODO

        case (_, AndType(left, right)) =>
          andConstraint(
            gath(scope, from, left, visitedUp, visitedDown, variance),
            gath(scope, from, right, visitedUp, visitedDown, variance))
        case (AndType(left, right), _) =>
          orConstraint(
            gath(scope, left, to, visitedUp, visitedDown, variance),
            gath(scope, right, to, visitedUp, visitedDown, variance)) // TODO can we avoid the or by expanding further somehow? e.g. min(leftC,rightC)

        case (aProj @ TypeProj(x, a), bProj @ TypeProj(y, b))  =>
          if (aProj == bProj) {
            TrueConstraint
          } else if (solveSet(aProj) && solveSet(bProj)) {
            FalseConstraint
          } else if (solveSet(aProj) && !solveSet(bProj)) {
            val bLower = ??? // TODO get rid of other type-projections in solveSet.
            SubtypeConstraint(scope, from, bLower, variance)
          } else if (!solveSet(aProj) && solveSet(bProj)) {
            val aUpperType = ??? // TODO get rid of other type-projections in solveSet.
            SubtypeConstraint(scope, aUpperType, to, variance) // TODO correct? vs reverse?
          } else { // if (!solveSet(aProj) && !solveSet(bProj))
            val aUpperType = if (visitedUp(aProj)) Top else typeProjectUpper(scope, x, a).getOrElse{error()}
            val bLowerType = if (visitedDown(bProj)) Bot else typeProjectLower(scope, y, b).getOrElse{error()}

            orConstraint(
              gath(scope, aUpperType, bProj, visitedUp + aProj, visitedDown, variance),
              gath(scope, aProj, bLowerType, visitedUp, visitedDown + bProj, variance))
          }

        case (aProj @ TypeProj(x, a), _) =>
          if (solveSet(aProj)) {
            val bLower = to // TODO get rid of all type-projections?
            SubtypeConstraint(scope, from, bLower, variance)
          } else {
            val aUpperType = if (visitedUp(aProj)) Top else typeProjectUpper(scope, x, a).getOrElse{error()}
            gath(scope, aUpperType, to, visitedUp + aProj, visitedDown, variance)
          }

        case (_, bProj @ TypeProj(y, b)) =>
          if (solveSet(bProj)) {
            val aUpperType = from // TODO get rid of all type-projections?
            SubtypeConstraint(scope, aUpperType, to, variance)
          } else {
            val bLowerType = if (visitedDown(bProj)) Bot else typeProjectLower(scope, y, b).getOrElse{error()}
            gath(scope, from, bLowerType, visitedUp, visitedDown, variance)
          }

        case (FieldDecl(a, aType), FieldDecl(b, bType)) if a == b => gath(scope, aType, bType, visitedUp, visitedDown, variance)

        case (TypeDecl(a, aLowerType, aUpperType), TypeDecl(b, bLowerType, bUpperType)) if a == b =>
          andConstraint(
            gath(scope, bLowerType, aLowerType, visitedDown, visitedUp, reverseVariance(variance)),
            gath(scope, aUpperType, bUpperType, visitedUp, visitedDown, variance))

        case (Top, Top) => TrueConstraint
        case (Bot, Top) => TrueConstraint
        case (Bot, Bot) => TrueConstraint

        case _ => FalseConstraint
      }
      gath(rootScope, rootFrom, rootTo, Set(), Set(), variance)
    }


    def eliminateVarUp(scope: Scope, z: Symbol, typ: Type, visited: Set[TypeProj]): Type = typ match {
      case aProj @ TypeProj(x, a) if x == z =>
        val aUpperType = typeProjectUpper(scope, x, a).getOrElse{error()}
        eliminateVarUp(scope, z, aUpperType, visited + aProj)
      case TypeProj(x, a) if x != z =>
        typ
      case AndType(left, right) =>
        AndType(eliminateVarUp(scope, z, left, visited), eliminateVarUp(scope, z, right, visited))
      case FunType(x, xType, xResType) if x != z =>
        val newXType = eliminateVarDown(scope, z, xType, visited)
        val newXResType = eliminateVarUp(scope, z, xResType, visited)
        FunType(x, newXType, newXResType)
      case RecType(x, xType) if x != z =>
        RecType(z, eliminateVarUp(scope, z, xType, visited))
      case FieldDecl(a, aType) =>
        FieldDecl(a, eliminateVarUp(scope, z, aType, visited))
      case TypeDecl(a, aLowerType, aUpperType) =>
        val newALowerType = eliminateVarDown(scope, z, aLowerType, visited)
        val newAUpperType = eliminateVarUp(scope, z, aUpperType, visited)
        TypeDecl(a, newALowerType, newAUpperType)
      case _ => typ // Bot, Top, Que, ErrorType
    }

    def eliminateVarDown(scope: Scope, z: Symbol, typ: Type, visited: Set[TypeProj]): Type = typ match {
        case aProj @ TypeProj(x, a) if x == z =>
          val aLowerType = typeProjectLower(scope, x, a).getOrElse{error()}
          eliminateVarDown(scope, z, aLowerType, visited + aProj)
        case TypeProj(x, a) if x != z =>
          typ
        case AndType(left, right) =>
          AndType(eliminateVarDown(scope, z, left, visited), eliminateVarDown(scope, z, right, visited))
        case FunType(x, xType, xResType) if x != z =>
          // TODO Is this correct? Can we end up deleting decls in xType that xResType needs?
          val newXType = eliminateVarUp(scope, z, xType, visited)
          val newXResType = eliminateVarDown(scope, z, xResType, visited)
          FunType(x, newXType, newXResType)
        case RecType(x, xType) if x != z =>
          RecType(z, eliminateVarDown(scope, z, xType, visited))
        case FieldDecl(a, aType) =>
          FieldDecl(a, eliminateVarDown(scope, z, aType, visited))
        case TypeDecl(a, aLowerType, aUpperType) =>
          // TODO Can we end up narrowing too much so that no longer
          // lower <: upper?
          val newALowerType = eliminateVarUp(scope, z, aLowerType, visited)
          val newAUpperType = eliminateVarDown(scope, z, aUpperType, visited)
          TypeDecl(a, newALowerType, newAUpperType)
        case _ => typ // Bot, Top, Que, ErrorType
    }


    def error() = {throw new TypecheckingError(); ErrorType}

    def typecheckTerm(su: SymbolUniverse, term: Term, prototype: Prototype = Que, scope: Scope = Map()): TypedTerm = (term, prototype) match {
      case (Var(x), p) =>
        term.withType{
          scope.get(x) match {
            case Some(xType) =>
              raise(su, scope, xType, p) match {
                case Some(res) => res
                case None => ???; ErrorType
              }
            case None => ???; ErrorType // TODO
          }
        }
      case (Let(x, xTerm, resTerm), p) =>
        if (scope.contains(x)) ??? // TODO rename
        val typedXTerm = typecheckTerm(su, xTerm, Que, scope)
        val xType = typedXTerm.assignedTypeOption.get // TODO Annoying, choose some typesafe way to do this instead...
        val typedResTerm = typecheckTerm(su, resTerm, p, scope + (x -> xType))
        Let(x, typedXTerm, typedResTerm).withType(eliminateVarUp(scope, x, typedResTerm.assignedType, Set()))
      case (Sel(x, a), p) =>
        term.withType {
          typecheckTerm(su, Var(x), FieldDecl(a, p), scope).assignedType match {
            case FieldDecl(b, bType) if a == b =>
              bType
            case _ =>
              ???; ErrorType // TODO
          }
        }
      case (App(x, y), p) =>
        val z = su.newSymbol()
        term.withType{
          typecheckTerm(su, Var(x), FunType(z, Que, Que), scope).assignedType match {
            case FunType(w, zType, zResType) if w == z =>
              if (scope.contains(z)) ??? // TODO rename
              val yType = typecheckTerm(su, Var(y), zType, scope).assignedType
              raise(su, scope, typeRenameVar(z, y, zResType), p) match {
                case Some(res) => res
                case None => ???; ErrorType
              }
            case _ =>
              ???; ErrorType // TODO
          }
        }
      case (Fun(x, _, _), Que) =>
        typecheckTerm(su, term, FunType(x, Que, Que), scope)
      case (Fun(x, _, _), Top) =>
        typecheckTerm(su, term, FunType(x, Bot, Top), scope)
      case (Fun(x, _, _), FunType(y, _, _)) if x != y =>
        val typedTerm = typecheckTerm(su, term, typeRenameBoundVarAssumingNonFree(x, prototype), scope)
        val typ = typedTerm.assignedType
        typedTerm.withType(typeRenameBoundVarAssumingNonFree(y, typ))
      case (Fun(x, xType, resTerm), FunType(y, argPrototype, resPrototype)) if x == y =>
        // TODO Is it fine to do lower/raise of arg and res separately? Or is
        // it necessary to do one call to raise at the end using the whole
        // prototype?
        lower(su, scope, xType, argPrototype) match {
          case None =>
            term.withType(error())
          case Some(loweredXType) =>
            val localScope = scope + (x -> xType) // TODO xType vs loweredXType? what happens if loweredXType is Bot? special case that?
            val typedResTerm = typecheckTerm(su, resTerm, resPrototype, localScope)
            val resType = typedResTerm.assignedType
            Fun(x, loweredXType, typedResTerm).withType(FunType(x, loweredXType, resType))
        }
      case (Obj(x, xType, defs), p) =>
        val localScope = scope + (x -> xType)
        // TODO check that there are no duplicatesd
        def typecheckDef(d: Def): Def = d match {
          case AndDef(left, right) =>
            val typedLeft = typecheckDef(left)
            val typedRight = typecheckDef(right)
            val typ = AndType(typedLeft.assignedType, typedRight.assignedType)
            AndDef(typedLeft, typedRight).withType(typ)
          case FieldDef(a, aTerm) =>
            val aPrototype = raise(su, scope, xType, FieldDecl(a, Que)) match {
              case Some(FieldDecl(b, bPrototype)) if a == b => bPrototype
              case None => Que
              case _ => error()
            }
            val aTypedTerm = typecheckTerm(su, aTerm, aPrototype, localScope)
            FieldDef(a, aTypedTerm).withType(FieldDecl(a, aTypedTerm.assignedType))
          case TypeDef(a, aType) =>
            val typ = TypeDecl(a, aType, aType)
            val declPrototype = raise(su, scope, xType, TypeDecl(a, Que, Que)) match {
              case Some(decl @ TypeDecl(b, _, _)) if a == b => decl
              case None => TypeDecl(a, Que, Que)
              case _ => error()
            }
            val myType = raise(su, scope, TypeDecl(a, aType, aType), declPrototype) match {
              case Some(decl @ TypeDecl(b, _, _)) if a == b => decl
              case _ => error()
            }
            d.withType(myType)
          case _ => d.withType(error()) // complains about DefFuture.
        }
        // TODO check everything in xType is in defs

        if (defHasDuplicates(defs)) {
          term.withType(error())
        } else {
          val typedDefs = typecheckDef(defs)
          if (!varIsSubtypeOf(scope + (x -> typedDefs.assignedType), x, xType)) {
            term.withType(error())
          } else {
            val raisedType = raise(su, scope + (x -> xType), xType, p).getOrElse{error()}
            Obj(x, xType, typedDefs).withType(raisedType)
          }
        }
      // TODO DOL extensions to DOT
      case _ =>
        term.withType(error())
    }

    def raise(su: SymbolUniverse, scope: Scope, from: Type, to: Prototype): Option[Type] = {
      val z = su.newSymbol()
      val (newTo, solveSet) = prep(su, z, to)
      val constraint = gather(su, scope, solveSet, from, newTo, Covariant)
      solveConstraint(su, solveSet, constraint) match {
        case Some(solution) => Some(applyConstraintSolution(newTo, solution))
        case None           => None
      }
    }

    def lower(su: SymbolUniverse, scope: Scope, from: Type, to: Prototype): Option[Type] = {
      val z = su.newSymbol()
      val (newTo, solveSet) = prep(su, z, to)
      val constraint = gather(su, scope, solveSet, newTo, from, Contravariant)
      solveConstraint(su, solveSet, constraint) match {
        case Some(solution) => Some(applyConstraintSolution(newTo, solution))
        case None           => None
      }
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
        andType(
          typeRenameVar(fromVar, toVar, left),
          typeRenameVar(fromVar, toVar, right))
      case _ =>
        typ
    }

    def typeRenameBoundVarAssumingNonFree(toVar: Symbol, typ: Type): Type = {
      if (isVarFreeIn(toVar, typ)) ??? // TODO Caller's responsibility?
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
      if (isVarFreeIn(toVar, term)) ???
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

    def equalDefs(su: SymbolUniverse, scope: Scope, first: Def, second: Def): Boolean = {
      if (defHasDuplicates(first)) ???
      if (defHasDuplicates(second)) ???
      val firstMap = defAsMap(first)
      val secondMap = defAsMap(second)

      (firstMap.keys == secondMap.keys
        && mapIntersect(firstMap, secondMap){
          case (FieldDef(a, aTerm), FieldDef(b, bTerm)) if a == b => equalTerms(su, scope, aTerm, bTerm)
          case (TypeDef(a, aType), TypeDef(b, bType)) if a == b   => equalTypes(su, scope, aType, bType)
          case _ => false
        }.values.forall{(x: Boolean) => x})
    }

    // TODO
    def equalTerms(su: SymbolUniverse, scope: Scope, first: Term, second: Term): Boolean = {
      val assignedTypesEqual = (for {
        firstType <- first.assignedTypeOption
        secondType <- second.assignedTypeOption
      } yield equalTypes(su, scope, firstType, secondType)).getOrElse(false)

      assignedTypesEqual && ((first, second) match {
        case (Var(_), Var(_))       => (first == second)
        case (App(_, _), App(_, _)) => (first == second)
        case (Sel(_, _), Sel(_, _)) => (first == second)
        case (Let(x, xTerm, xResTerm), Let(y, yTerm, yResTerm)) if x == y =>
          (equalTerms(su, scope, xTerm, yTerm)
            && equalTerms(su, scope, xResTerm, yResTerm))
        case (Obj(x, xType, xBody), Obj(y, yType, yBody)) if x == y =>
          (equalTypes(su, scope, xType, yType)
            && equalDefs(su, scope, xBody, yBody))
        case (Fun(x, xType, xBody), Fun(y, yType, yBody)) if x == y =>
          (equalTypes(su, scope, xType, yType)
            && equalTerms(su, scope, xBody, yBody))
        case (Let(x, xTerm, xResTerm), Let(y, yTerm, yResTerm)) if x != y =>
          equalTerms(su, scope, first, NoFuture.termRenameBoundVarAssumingNonFree(x, second))
        case (Obj(x, xType, xBody), Obj(y, yType, yBody)) if x != y =>
          equalTerms(su, scope, first, NoFuture.termRenameBoundVarAssumingNonFree(x, second))
        case (Fun(x, xType, xBody), Fun(y, yType, yBody)) if x != y =>
          equalTerms(su, scope, first, NoFuture.termRenameBoundVarAssumingNonFree(x, second))
        case _ =>
          false
      })
    }


    // TODO def hasNestedRecursiveTypes(scope: Scope, x: Symbol)?
    // TODO def validType?
    // TODO scope should probably have an invariant that all contained types
    // are "valid" for some sense of valid.

    /** Get rid of any (RecType(y, yType)) in typ by renaming y to x.
     * Only searches immediate AndTypes. Any nested RecTypes are ignored.
     */
    def eliminateRecursiveTypes(typ: Type, x: Symbol): Type = typ match {
      case RecType(y, yType) =>
        // TODO do rename and rectype-elimination at the same time. i.e. pass
        // down set of symbols equivalent to x?
        // TODO Alternatively do all renames at end? renameAll(set(y,z,w) -> x)
        eliminateRecursiveTypes(typeRenameVar(y, x, yType), x) // NOTE: it is fine for x to be free in yType.
      case AndType(left, right) =>
        AndType(
          eliminateRecursiveTypes(left, x),
          eliminateRecursiveTypes(right, x))
      case typ => typ
    }

    // TODO Should there be a fixRecursiveTypeProjections? No? There may e.g.
    // be circular references between TypeProjs and that info may be necessary
    // in some cases. For example, probably can't rewrite:
    //    {x.T: x.D..x.D}&{x.D: x.T..x.T}.


    /** Replace each Que in x:prototype with a TypeProj(z,_).
     *
     * The Que is replaced with TypeProj(z, k), where k is a number unique to
     * that Que.
     *
     * If z is already in scope, an exception is thrown.
     */
    def varPrep(scope: Scope, z: Symbol, x: Symbol, prototype: Prototype): (Int, Type) = {
//      final case class State(projs: Set[TypeProj], typ: Type) { // TODO Cool, but overkill. Unless.... it can be reused across functions?
//        def map(f: Type => Type): State = State(projs, f(typ))
//        def flatMap(f: (Type) => State): State = {
//          val second  = f(typ)
//          State(projs ++ second.projs, second.typ)
//        }
//        def toTraversable: Traversable[Type] = List(typ)
//      }
//      def doTwo(a: Prototype, b: Prototype)(f: (Type, Type) => Type): State = for {
//        newA <- inner(a) ;
//        newB <- inner(b)
//      } yield f(newA, newB)
//      def inner(prototype: Prototype): State = prototype match {
//        case Que =>
//          val proj = TypeProj(z, su.newSymbol())
//          State(Set(proj), proj)
//        case AndType(left, right)        => doTwo(left, right){AndType(_, _)}
//        case FunType(x, xType, xResType) => doTwo(xType, xResType){FunType(x, _, _)}
//        case FieldDecl(a, aType)         => inner(aType).map{FieldDecl(a, _)}
//        case TypeDecl(a, aLowerType, aUpperType) => doTwo(aLowerType, aUpperType){TypeDecl(a, _, _)}
//        case RecType(x, xType) => throw new TypecheckingError("nested RecType")
//        case _ => State(Set(), prototype)
//      }
//      val State(projSet, typ) = inner(eliminateRecursiveTypes(prototype, x))
//      (projSet, typ)
      def inner(count: Int, prototype: Prototype): (Int, Type) = prototype match {
        case RecType(x, xType) => throw new TypecheckingError("nested RecType")
        case Que =>
          val proj = TypeProj(z, count)
          (count + 1, proj)
        case AndType(left, right) =>
          val (count2, leftType)   = inner(count, left)
          val (count3, rightType) = inner(count2, right)
          (count3, andType(leftType, rightType))
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
      if (scope.contains(z))
        throw new TypecheckingError(s"var $z is already in use")
      inner(0, eliminateRecursiveTypes(prototype, x))
    }

    def varGather(scope: Scope, r: Symbol, z: Symbol, upper: Type, variance: Variance): Constraint = {
      // TODO Instead of passing down variance, maybe just flip the order of
      // from and to? (from,to) -> (to, from)?
      def solveSet(proj: TypeProj): Boolean = proj.x == r
      def gath(scope: Scope, from: Type, to: Type, visitedUp: Set[TypeProj], visitedDown: Set[TypeProj], variance: Variance): Constraint = {
        def rec(scope: Scope = scope, from: Type = from, to: Type = to, visitedUp: Set[TypeProj] = visitedUp, visitedDown: Set[TypeProj] = visitedDown, variance: Variance = variance) = gath(scope, from, to, visitedUp, visitedDown, variance)
        (from, to) match {
          case (_: RecType, _) | (_, _: RecType) =>  throw new TypecheckingError("nested RecType")

          case (FieldDecl(a, aType), Top) => rec(to=FieldDecl(a, Top)) // TODO TrueConstraint? still necessary to decide on variance?
          case (Bot, FieldDecl(a, aType)) => rec(from=FieldDecl(a, Bot))

          case (Bot, TypeDecl(a, aLowerType, aUpperType)) => ??? // TODO What IS THE smallest TypeDecl? TypeDecl(a, x.a, x.a)?
          case (TypeDecl(a, aLowerType, aUpperType), Top) => rec(to=TypeDecl(a, Bot, Top))

          case (FunType(x, _, _), Top) => rec(to=FunType(x, Bot, Top))
          case (Bot, FunType(x, _, _)) => rec(from=FunType(x, Top, Bot))


          case (FunType(x, _, _), FunType(y, _, _)) if x != y =>
            rec(to=typeRenameBoundVarAssumingNonFree(x, to))

          case (FunType(x, xType, xResType), FunType(y, yType, yResType)) if x == y =>
            andConstraint(
              rec(from=yType, to=xType),
              rec(scope = scope+(x -> yType), from=xResType, to=yResType))

          case (_, AndType(left, right)) =>
            andConstraint(
              rec(to=left),
              rec(to=right))

          case (AndType(left, right), _) =>
            // TODO factor duplicate declarations first? AndType(FieldDef(a,A), FieldDef(a,B)) --> FieldDef(a, AndType(A,B))
            // probably a bad idea in case of typeprojs...
            orConstraint(
              rec(from=left),
              rec(from=right)) // TODO can we avoid the or by expanding further somehow? e.g. min(leftC,rightC)

          case (aProj @ TypeProj(x, a), bProj @ TypeProj(y, b))  =>
            if (aProj == bProj) {
              TrueConstraint
            } else if (solveSet(aProj) && solveSet(bProj)) {
              FalseConstraint
            } else if (solveSet(aProj) && !solveSet(bProj)) {
              val bLower = ??? // TODO get rid of other type-projections in solveSet.
              SubtypeConstraint(scope, from, bLower, variance)
            } else if (!solveSet(aProj) && solveSet(bProj)) {
              val aUpperType = ??? // TODO get rid of other type-projections in solveSet.
              SubtypeConstraint(scope, aUpperType, to, variance) // TODO correct? vs reverse?
            } else { // if (!solveSet(aProj) && !solveSet(bProj))
              val aUpperType = if (visitedUp(aProj)) Top else typeProjectUpper(scope, x, a).getOrElse{error()}
              val bLowerType = if (visitedDown(bProj)) Bot else typeProjectLower(scope, y, b).getOrElse{error()}

              orConstraint(
                rec(from=aUpperType, visitedUp = visitedUp+aProj),
                rec(to=bLowerType, visitedDown = visitedDown+bProj))
            }

          case (aProj @ TypeProj(x, a), _) =>
            if (solveSet(aProj)) {
              val bLower = to // TODO get rid of all type-projections?
              SubtypeConstraint(scope, from, bLower, variance)
            } else {
              val aUpperType = if (visitedUp(aProj)) Top else typeProjectUpper(scope, x, a).getOrElse{error()}
              rec(from=aUpperType, visitedUp = visitedUp+aProj)
            }

          case (_, bProj @ TypeProj(y, b)) =>
            if (solveSet(bProj)) {
              val aUpperType = from // TODO get rid of all type-projections?
              SubtypeConstraint(scope, aUpperType, to, variance)
            } else {
              val bLowerType = if (visitedDown(bProj)) Bot else typeProjectLower(scope, y, b).getOrElse{error()}
              rec(to=bLowerType, visitedDown = visitedDown+bProj)
            }

          case (FieldDecl(a, aType), FieldDecl(b, bType)) if a == b => gath(scope, aType, bType, visitedUp, visitedDown, variance)

          case (TypeDecl(a, aLowerType, aUpperType), TypeDecl(b, bLowerType, bUpperType)) if a == b =>
            andConstraint(
              rec(from=bLowerType, to=aLowerType, variance=reverseVariance(variance)),
              rec(from=aUpperType, to=bUpperType))

          case (Top, Top) => TrueConstraint
          case (Bot, Top) => TrueConstraint
          case (Bot, Bot) => TrueConstraint

          case _ => FalseConstraint
        }
      }
      val zType      = eliminateRecursiveTypes(scope(z), z)
      val zUppertype = eliminateRecursiveTypes(upper, z)
      if (variance == Contravariant)
        gath(scope, zUppertype, zType, Set(), Set(), variance)
      else
        gath(scope, zType, zUppertype, Set(), Set(), variance)
    }

    // TODO What happens when we raise to AndType(Que, _)? Is the Que redundant? Should it be skipped?

    /** Find subtype/supertype T of scope(x) that matches prototype.
     * If variance is Covariant, T is the least supertype of scope(x) that
     * matches.
     * If variance is Contravariant, T is the gratest subtype of scope(x) that
     * matches.
     * TODO explain Invariant, ConstantVariance.
     * Return None if T does not exists.
     */
    def varMatch(scope: Scope, r: Symbol, z: Symbol, prototype: Prototype, variance: Variance): Option[Type] = {
      val recFreePrototype = eliminateRecursiveTypes(prototype, z)
      val (numQues, labeledPrototype) = varPrep(scope, r, z, prototype)
      val constraint = varGather(scope, r, z, labeledPrototype, variance)
      val solveSet = (0 until numQues).map{TypeProj(r, _)}.toSet // TODO get rid of solveSet somehow?
      for {
        solution <- solveConstraint(???, solveSet, constraint) // TODO is su necessary?
      } yield applyConstraintSolution(labeledPrototype, solution)
    }

    /** Least supertype T of scope(x) such that T matches prototype.
     * Return None if T does not exists.
     */
    def varRaise(scope: Scope, r: Symbol, z: Symbol, prototype: Prototype): Option[Type] = varMatch(scope, r, z, prototype, Covariant)

    /** Greatest subtype T of scope(x) such that T matches prototype.
     * Return None if T does not exists.
     */
    def varLower(scope: Scope, r: Symbol, z: Symbol, prototype: Prototype): Option[Type] = varMatch(scope, r, z, prototype, Contravariant)


    /** Check if scope(x) <: subtype.
     *  Formally, x: xType, xType <: To.
     */
    def varIsSubtypeOf(scope: Scope, z: Symbol, supertype: Type): Boolean = {
      // TODO maybe factor inner out into separate function?
      def inner(scope: Scope, first: Type, second: Type, visitedLeft: Set[TypeProj], visitedRight: Set[TypeProj]): Boolean = {
        // TODO Confirm whether r is efficient. I expect r to get inlined.
        def r(scope: Scope = scope, first: Type = first, second: Type = second, visitedLeft: Set[TypeProj] = visitedLeft, visitedRight: Set[TypeProj] =
          visitedRight) = inner(scope, first, second, visitedLeft, visitedRight)
        (first, second) match {
          case (Bot, _) => true
          case (_, Top) => true
          case (_, AndType(left, right)) => r(second=left) && r(second=right) // IMPORTANT: This must be checked before the other AndType-case.
          case (AndType(left, right), _) => r(first=left) || r(first=right)
          case (FieldDecl(a, aType), FieldDecl(b, bType)) if a == b => r(first=aType, second=bType)

          case (TypeDecl(a, aLowerType, aUpperType), TypeDecl(b, bLower, bUpper)) if a == b =>
           r(first=bLower, second=aLowerType) && r(first=aUpperType, second=bUpper)

          case (aProj @ TypeProj(x, a), bProj @ TypeProj(y, b)) =>
            aProj == bProj || {
              val aUpperType = if (visitedLeft(aProj)) Top else typeProjectUpper(scope, x, a).getOrElse{error()} // TODO pass along visitedLeft to typeProjectUpper
              // TODO eliminate rectypes!
              r(first=aUpperType, visitedLeft=visitedLeft+aProj)
            } || {
              val bLowerType = if (visitedRight(bProj)) Bot else typeProjectLower(scope, y, b).getOrElse{error()} // TODO pass along visitedRight to typeProjectUpper
              // TODO eliminate rectypes!
              r(second=bLowerType, visitedRight=visitedRight+bProj)
            }

          case (aProj @ TypeProj(x, a), _) =>
            val aUpperType = if (visitedLeft(aProj)) Top else typeProjectUpper(scope, x, a).getOrElse{error()}
            r(first=aUpperType, visitedLeft = visitedLeft+aProj)
          case (_, bProj @ TypeProj(y, b)) =>
            val bLowerType = if (visitedRight(bProj)) Bot else typeProjectLower(scope, y, b).getOrElse{error()}
            r(second=bLowerType, visitedRight = visitedRight+bProj)

          case (FunType(x, _, _), FunType(y, _, _)) if x != y =>
            if (scope.contains(x)) ??? // TODO Maybe better to create new var "w", and rename {x,y}->w?
            // TODO is isVarFreeInType(x, second)-check necessary?
            (!isVarFreeInType(x, second)
              && r(second=typeRenameBoundVarAssumingNonFree(x, second)))

          case (FunType(x, xType, xResType), FunType(y, yType, yResType)) if x == y =>
            (r(first=yType, second=xType)
              && r(scope = scope+(x -> yType), first=xResType, second=yResType))
          case (_: RecType, _) | (_, _: RecType) =>
            rigidEqualTypes(first, second)
          case _ => false
        }
      }

      val zType = eliminateRecursiveTypes(scope(z), z)
      val zSupertype = eliminateRecursiveTypes(supertype, z)
      inner(scope, zType, zSupertype, Set(), Set())
    }

    /** Check if types are equal by subtyping.
     * Formally, first <: second AND second <: first.
     */
    def varEqualTypes(scope: Scope, z: Symbol, second: Type): Boolean = {
      val zType = scope(z)
      (varIsSubtypeOf(scope, z, second)
        && varIsSubtypeOf(scope + (z -> second), z, zType))
    }

    def equalTypes(su: SymbolUniverse, scope: Scope, first: Type, second: Type): Boolean = {
      val z = su.newSymbol()
      varEqualTypes(scope + (z -> first), z, second)
    }

    /** Check if first and second are exactly equal, except for renaming some bound vars.
     */
    def rigidEqualTypes(first: Type, second: Type): Boolean = (first, second) match {
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

    // TODO rename to: isSubtypeOfAssumingNoRecTypes?
    def isSubtypeOf(scope: Scope, first: Type, second: Type): Boolean = { // TODO REM. use varIsSubtypeOf instead.
      def inner(scope: Scope, first: Type, second: Type, visitedLeft: Set[TypeProj], visitedRight: Set[TypeProj]): Boolean = (first, second) match {
        case (_: RecType, _) | (_, _: RecType) =>
          throw new TypecheckingError("nested RecType")

        case (Bot, _) => true
        case (_, Top) => true

        case (TypeProj(x, a), TypeProj(y, b)) if x == y && a == b => true
        case (aProj @ TypeProj(x, a), bProj @ TypeProj(y, b)) if !(x == y && a == b) =>
          val aUpperType = if (visitedLeft(aProj)) Top else typeProjectUpper(scope, x, a).getOrElse{error()}
          val bLowerType = if (visitedRight(bProj)) Bot else typeProjectLower(scope, y, b).getOrElse{error()}
          (inner(scope, aUpperType, bProj, visitedLeft + aProj, visitedRight)
            || inner(scope, aProj, bLowerType, visitedLeft, visitedRight + bProj))

        case (aProj @ TypeProj(x, a), _) =>
          val aUpperType = if (visitedLeft(aProj)) Top else typeProjectUpper(scope, x, a).getOrElse{error()}
          inner(scope, aUpperType, second, visitedLeft + aProj, visitedRight)
        case (_, bProj @ TypeProj(y, b)) =>
          val bLowerType = if (visitedRight(bProj)) Bot else typeProjectLower(scope, y, b).getOrElse{error()}
          inner(scope, first, bLowerType, visitedLeft, visitedRight + bProj)

        case (AndType(left, right), _) =>
          (inner(scope, left, second, visitedLeft, visitedRight)
            || inner(scope, right, second, visitedLeft, visitedRight))
        case (_, AndType(left, right)) =>
          (inner(scope, first, left, visitedLeft, visitedRight)
            && inner(scope, first, right, visitedLeft, visitedRight))

        case (FunType(x, _, _), FunType(y, _, _)) if x != y =>
          (!isVarFreeInType(x, second)
            && inner(scope, first, typeRenameBoundVarAssumingNonFree(x, second), visitedLeft, visitedRight))
        case (FunType(x, xType, xResType), FunType(y, yType, yResType)) if x == y =>
          (inner(scope, yType, xType, visitedRight, visitedLeft)
            && inner(scope, xResType, yResType, visitedLeft, visitedRight))

        case (FieldDecl(a, aType), FieldDecl(b, bType)) if a == b =>
          inner(scope, aType, bType, visitedLeft, visitedRight)
        case (TypeDecl(a, aLowerType, aUpperType), TypeDecl(b, bLowerType, bUpperType)) if a == b =>
          (inner(scope, bLowerType, aLowerType, visitedRight, visitedLeft)
            && inner(scope, aUpperType, bUpperType, visitedLeft, visitedRight))
        case _ => false
      }
      inner(scope, first, second, Set(), Set())
    }


    def isVarFreeIn(z: Symbol, t: Tree): Boolean = t match {
      case expr: Expr => isVarFreeInExpr(z, expr)
      case typ: Type => isVarFreeInType(z, typ)
    }

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

    def isVarFreeInExpr(z: Symbol, e: Expr): Boolean = e match {
      case Var(x)                 => (x == z)
      case Sel(x, a)              => (x == z)
      case App(x, y)              => (x == z || y == z)
      case Let(x, xTerm, resTerm) if x != z => isVarFreeInExpr(z, xTerm) || isVarFreeInExpr(z, resTerm)
      case Obj(x, xType, d)       if x != z => isVarFreeInType(z, xType) || isVarFreeInExpr(z, d)
      case Fun(x, xType, resTerm) if x != z => isVarFreeInType(z, xType) || isVarFreeInExpr(z, resTerm)
      case FieldDef(a, aTerm)     => isVarFreeInExpr(z, aTerm)
      case TypeDef(a, aType)      => isVarFreeInType(z, aType)
      case AndDef(left, right)    => (isVarFreeInExpr(z, left) || isVarFreeInExpr(z, right))
      case _ => false
    }

    // TODO make a variant that expands TypeProjs?
    def typeDfsSet[T](typ: Type)(f: Type => Set[T]): Set[T] = f(typ) ++ (typ match {
      case FunType(x, xType, resType) =>
        typeDfsSet(xType)(f) ++ typeDfsSet(resType)(f)
      case RecType(x, xType) =>
        typeDfsSet(xType)(f)
      case FieldDecl(a, aType) =>
        typeDfsSet(aType)(f)
      case TypeDecl(a, aLowerType, aUpperType) =>
        typeDfsSet(aLowerType)(f) ++ typeDfsSet(aUpperType)(f)
      case AndType(left, right) =>
        typeDfsSet(left)(f) ++ typeDfsSet(right)(f)
      case _ => Set() // Bot, Top, Que, TypeProj, ErrorType
    })

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

    def stringExprWithType(expr: Expr): String = {
      val s = expr match {
        case Let(x, xTerm, resTerm) => s"Let($x, ${stringExprWithType(xTerm)}, ${stringExprWithType(resTerm)})"
        case Obj(x, xType, d)       => s"Obj($x, $xType, ${stringExprWithType(d)})"
        case Fun(x, xType, resTerm) => s"Fun($x, $xType, ${stringExprWithType(resTerm)})"
        case FieldDef(a, aTerm)     => s"FieldDef($a, ${stringExprWithType(aTerm)})"
        case AndDef(left, right)    => s"AndDef(${stringExprWithType(left)}, ${stringExprWithType(right)})"
        case _ => s"$expr"
      }
      s"$s.withTypeOption(${expr.assignedTypeOption})"
    }

    def stringExprWithTypeIfExists(expr: Expr): String = {
      val s = expr match {
        case Let(x, xTerm, resTerm) => s"Let($x, ${stringExprWithTypeIfExists(xTerm)}, ${stringExprWithTypeIfExists(resTerm)})"
        case Obj(x, xType, d)       => s"Obj($x, $xType, ${stringExprWithTypeIfExists(d)})"
        case Fun(x, xType, resTerm) => s"Fun($x, $xType, ${stringExprWithTypeIfExists(resTerm)})"
        case FieldDef(a, aTerm)     => s"FieldDef($a, ${stringExprWithTypeIfExists(aTerm)})"
        case AndDef(left, right)    => s"AndDef(${stringExprWithTypeIfExists(left)}, ${stringExprWithTypeIfExists(right)})"
        case _ => s"$expr"
      }
      expr.assignedTypeOption match {
        case Some(typ) =>
          s"$s.withType($typ)"
        case None =>
          s
      }
    }

    def directFieldDecls(scope: Scope, x: Symbol): Map[SymbolPath, Type] = {
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
          Map((path :+ a) -> aType)
        case _ =>
          Map()
      }
      inner(scope, Vector(x), scope(x), Set())
    }
    def directFieldDeclsInScope(scope: Scope): Map[SymbolPath, Type] = {
      scope.keys.map{directFieldDecls(scope, _)}.fold(Map()){case (leftMap, rightMap) =>
        mapUnion(leftMap, rightMap){AndType(_, _)}
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


    def selPath(su: SymbolUniverse, path: SymbolPath): Term = path match {
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


  // TODO typed terms. {var mytpe} like in Dotty? vs cell?

  type Scope = Map[Symbol, Type]

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

  def isPrototype(t: Type)(cont: (Boolean) => Unit): Unit = {
    def zipIsPrototype(a: Type, b: Type): Unit = {
      // TODO parallel?
      isPrototype(a){yesA =>
        isPrototype(b){yesB =>
          cont(yesA || yesB)
        }
      }
    }
    t match {
      case Que =>
        cont(true)
      case FunType(_, argType, resType) =>
        zipIsPrototype(argType, resType)
      case RecType(x, xType) =>
        isPrototype(xType)(cont)
      case FieldDecl(a, aType) =>
        isPrototype(aType)(cont)
      case TypeDecl(a, aLowerType, aUpperType) =>
        zipIsPrototype(aLowerType, aUpperType)
      case AndType(left, right) =>
        zipIsPrototype(left, right)
      case FutureType(cell) =>
        onComplete(cell){actualType =>
          isPrototype(actualType)(cont)
        }
      case _ => cont(false)
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

  def mapIntersect[K, T, A, B](a: Map[K, A], b: Map[K, B])(f: ((A, B)) => T): Map[K, T] = {
    b.flatMap{
      case (k, kValueInB) =>
        a.get(k) match {
          case Some(kValueInA) =>
            Some(k -> f((kValueInA, kValueInB)))
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
    //case PolyFunType(typeParams, params, resType) =>
    //  def k2(innerList: List[(Symbol, Type)])(cont: (List[(Symbol, Type)]) => Unit): Unit = innerList match {
    //    case (x, xType) :: rest =>
    //      expandTypeFutures(xType) {newXType =>
    //        k2(rest) {rest =>
    //          cont((x, newXType) :: rest)
    //        }
    //      }
    //    case Nil => cont(Nil)
    //  }
    //  def k(params: List[List[(Symbol, Type)]])(cont: (List[List[(Symbol, Type)]]) => Unit): Unit = params match {
    //    case innerList :: rest =>
    //      k2(innerList) {innerList =>
    //        k(rest) {rest =>
    //          cont(innerList :: rest)
    //        }
    //      }
    //    case Nil => cont(Nil)
    //  }
    //  k(params) {params =>
    //    expandTypeFutures(resType) {newResType =>
    //      cont(PolyFunType(typeParams, params, newResType))
    //    }
    //  }
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

  def typeApply(fromTypes: List[Type], toTypes: List[Type], params: Type): Type = {
    ???
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

  case class Parallel(val symbolUniverse: SymbolUniverse, val pool: HandlerPool, val hasErrorsAtom:  AtomicBoolean) {
    // TODO smarter futureType that does not always fork? e.g. r(..., asFutureTypePlease=true) and chooses to make a future or not

    def launch(f: => Unit) {
      pool.execute{() => f}
    }

    def contCell[T](lattice: Lattice[T])(f: (T => Unit) => Unit): DefaultCell[T] = {
      val completer = newCellCompleter(pool, lattice)
      launch {
        f {res =>
            completer.putFinal(res)
        }
        // TODO catch and rethrow exceptions? should probably not call error()
        // since we are not expecting errors. --- WHAT?
      }
      completer.cell
    }

    def contZipFork[A >: Null, B >: Null](a: (A => Unit) => Unit, b: (B => Unit) => Unit)(cont: (A, B) => Unit): Unit = { // TODO test
      val aCell = contCell(Lattice.trivial[A])(a)
      val bCell = contCell(Lattice.trivial[B])(b)
      onComplete(aCell) {aRes =>
        onComplete(bCell) {bRes =>
          cont(aRes, bRes)
        }
      }
    }

    def futureType(f: ((Type) => Unit) => Unit): Type = {
      FutureType(contCell(Lattice.trivial[Type])(f))
    }


    def expandTermFutures(term: Term)(cont: (Term) => Unit): Unit = term match {
      case TermFuture(cell) =>
        onComplete(cell){
          expandTermFutures(_)(cont)
        }
      case _ =>
        def expandAssignedType(t: Term)(c: (Term) => Unit): Unit = {
          t.assignedTypeOption match {
            case Some(termType) =>
              expandTypeFutures(termType){termType =>
                c(t.withType(termType))
              }
            case None =>
              c(t)
          }
        }

        term match {
          case Let(x, xTerm, resTerm) =>
            expandTermFutures(xTerm){xTerm =>
              expandTermFutures(resTerm){resTerm =>
                expandAssignedType(Let(x, xTerm, resTerm).withTypeOption(term.assignedTypeOption))(cont)
              }
            }
          case Obj(x, xType, body) =>
            expandTypeFutures(xType){xType =>
              expandDefFutures(body){body =>
                expandAssignedType(Obj(x, xType, body).withTypeOption(term.assignedTypeOption))(cont)
              }
            }
          case Fun(x, xType, body) =>
            expandTypeFutures(xType){xType =>
              expandTermFutures(body){body =>
                expandAssignedType(Fun(x, xType, body).withTypeOption(term.assignedTypeOption))(cont)
              }
            }
          case _ => // Var, App, Sel
            expandAssignedType(term)(cont)
        }
    }

    def expandDefFutures(d: Def)(cont: (Def) => Unit): Unit = d match {
      case DefFuture(cell) =>
        onComplete(cell){
          expandDefFutures(_)(cont)
        }
      case _ =>
        def expandAssignedType(d2: Def)(c: (Def) => Unit): Unit = {
          d2.assignedTypeOption match {
            case Some(termType) =>
              expandTypeFutures(termType){termType =>
                c(d2.withType(termType))
              }
            case None =>
              c(d2)
          }
        }
        d match {
          case FieldDef(a, aTerm) =>
            expandTermFutures(aTerm){aTerm =>
              expandAssignedType(FieldDef(a, aTerm).withTypeOption(d.assignedTypeOption))(cont)
            }
          case AndDef(left, right) =>
            expandDefFutures(left){left =>
              expandDefFutures(right){right =>
                expandAssignedType(AndDef(left, right).withTypeOption(d.assignedTypeOption))(cont)
              }
            }
          case TypeDef(a, aType) =>
            expandTypeFutures(aType){aType =>
              expandAssignedType(TypeDef(a, aType).withTypeOption(d.assignedTypeOption))(cont)
            }
          case _ =>
            ???
        }
    }


    def contFold[A](first: ((A) => Unit) => Unit, seq: List[((A) => Unit) => Unit])(f: (A, A) => ((A) => Unit) => Unit)(cont: (A) => Unit): Unit = {
      seq match {
        case second :: rest =>
          first {firstRes =>
            second {secondRes =>
              contFold(f(firstRes, secondRes), rest)(f)(cont)
            }
          }
        case Nil => first(cont)
      }
    }

    // TODO contCommutativeAssociativeReduce
    def contReduce[A](seq: List[((A) => Unit) => Unit])(f: (A, A) => ((A) => Unit) => Unit)(cont: (A) => Unit): Unit = {
      seq match {
        case Nil => throw new UnsupportedOperationException()
        case first :: rest =>
          contFold(first, rest)(f)(cont)
      }
    }


    def renameToUniqueVar(fromVar: Symbol, toVar: Symbol, t: Type)(cont: (Type) => Unit): Unit = t match {
      case FutureType(cell) =>
        onComplete(cell){actualT =>
          renameToUniqueVar(fromVar, toVar, actualT)(cont)
        }
      case TypeProj(x, a) if x == fromVar =>
        cont(TypeProj(toVar, a))
      case FunType(x, xType, resType) if x != fromVar =>
        // TODO assert(x != toVar)
        val lazyRenamedXType = futureType{
          renameToUniqueVar(fromVar, toVar, xType)(_)
        }
        val lazyRenamedResType = futureType{
          renameToUniqueVar(fromVar, toVar, resType)(_)
        }
        cont(FunType(x, lazyRenamedXType, lazyRenamedResType))
      case RecType(x, xType) if x != fromVar =>
        // TODO assert(x != toVar)
        val lazyRenamedXType = futureType{
          renameToUniqueVar(fromVar, toVar, xType)(_)
        }
        cont(RecType(x, lazyRenamedXType)) // TODO vs non-lazy?
      case FieldDecl(a, aType) =>
        val lazyRenamedAType = futureType{
          renameToUniqueVar(fromVar, toVar, aType)(_)
        }
        cont(FieldDecl(a, lazyRenamedAType)) // TODO vs non-lazy?
      case TypeDecl(a, aLowerType, aUpperType) =>
        val lazyRenamedALowerType = futureType{
          renameToUniqueVar(fromVar, toVar, aLowerType)(_)
        }
        val lazyRenamedAUpperType = futureType{
          renameToUniqueVar(fromVar, toVar, aUpperType)(_)
        }
        cont(TypeDecl(a, lazyRenamedALowerType, lazyRenamedAUpperType))
      case AndType(left, right) =>
        val lazyRenamedLeft = futureType{
          renameToUniqueVar(fromVar, toVar, left)(_)
        }
        val lazyRenamedRight = futureType{
          renameToUniqueVar(fromVar, toVar, right)(_)
        }
        cont(AndType(lazyRenamedLeft, lazyRenamedRight))
      case _ =>
        cont(t)
    }

    def exprRenameVar[T <: Expr](fromVar: Symbol, toVar: Symbol, e: T): T = e match { // TODO do in parallel and use TermFuture
      case Var(x) if x == fromVar => Var(toVar).asInstanceOf[T]
      case App(x, y) if x == fromVar || y == fromVar =>
        val newX = if (x == fromVar) toVar else x
        val newY = if (y == fromVar) toVar else y
        App(newX, newY).asInstanceOf[T]
      case Let(x, xTerm, t) if x != fromVar =>
        Let(x, exprRenameVar(fromVar, toVar, xTerm), exprRenameVar(fromVar, toVar, t)).asInstanceOf[T]
      case Sel(x, a) if x == fromVar =>
        Sel(toVar, a).asInstanceOf[T]
      case Obj(x, xType, d) if x != fromVar =>
        val newXType = futureType{
          renameToUniqueVar(fromVar, toVar, xType)(_)
        }
        Obj(x, newXType, exprRenameVar(fromVar, toVar, d)).asInstanceOf[T]
      case Fun(x, xType, t) if x != fromVar =>
        val newXType = futureType{
          renameToUniqueVar(fromVar, toVar, xType)(_)
        }
        Fun(x, newXType, exprRenameVar(fromVar, toVar, t)).asInstanceOf[T]
      case FieldDef(a, aTerm) =>
        FieldDef(a, exprRenameVar(fromVar, toVar, aTerm)).asInstanceOf[T]
      case TypeDef(a, aType) =>
        val newAType = futureType{
          renameToUniqueVar(fromVar, toVar, aType)(_)
        }
        TypeDef(a, newAType).asInstanceOf[T]
      case AndDef(left, right) =>
        AndDef(
          exprRenameVar(fromVar, toVar, left),
          exprRenameVar(fromVar, toVar, right)).asInstanceOf[T]
      case _ => e
    }

    // TODO will raiseTo(..., classA, classA) when classA is partially lazy lead to problems?

    // TODO replace killScope:Scope with killSet:Set[Symbol]?

    def contFuture[T >: Null](f: ((T) => Unit) => Unit): DefaultCell[T] = {
      val lattice = Lattice.trivial[T]
      contCell(lattice)(f)
    }

    case class TermFuture(cell: DefaultCell[Term]) extends Term { // NOTE: Nested, so that Parallel.pool is in scope. Don't mix with other Parallel instances! // TODO pool as argument instead?
      type ThisType = TermFuture
      val treeHeight = 1
      val totNumNodes = 1
      assignedTypeOption = Some(futureType{cont =>
        onComplete(cell){actualTerm =>
          cont(actualTerm.assignedTypeOption.getOrElse(error()))
        }
      })
      def withTypeOption(typeOption: Option[Type]) = {
        val res = this.copy()
        res.assignedTypeOption = typeOption
        res
      }
    }

    case class DefFuture(cell: DefaultCell[Def]) extends Def { // NOTE: Nested, so that Parallel.pool is in scope. Don't mix with other Parallel instances!
      type ThisType = DefFuture
      val treeHeight = 1
      val totNumNodes = 1
      def withTypeOption(typeOption: Option[Type]) = {
        val res = this.copy()
        res.assignedTypeOption = typeOption
        res
      }
    }

    def termFuture(f: ((Term) => Unit) => Unit) = this.TermFuture(contFuture(f))
    def defFuture(f: ((Def) => Unit) => Unit) = this.DefFuture(contFuture(f))

//    class LazyFutureType(atom: AtomicReference[FutureType], putter: (Type => Unit) => Unit) extends CanonicalPrototype {
//      val treeHeight = 2
//      val totNumNodes = 2
//
//      def get: CanonicalFuture = {
//        var current = atom.get()
//        while (current == null) {
//          val completer = newCellCompleter[Type](pool, Lattice.trivial)
//          val future = CanonicalFuture(completer.cell)
//          if (atom.compareAndSet(null, future)) {
//            launch {
//              putter {typ =>
//                completer.putFinal(typ)
//              }
//            }
//            current = future
//          } else {
//            freeze(completer)
//          }
//        }
//        current
//      }
//    }


    def error(): Type = {
      hasErrorsAtom.lazySet(true)
      ???
      ErrorType
    }



    // TODO assert that types are not prototypes

    // TODO INV: never return a type that is not in the given scope
    // e.g.:
    //   f: fun(x: {A: Int..Int})x.A
    //   f({A = Int}): Int  // i.e. not x.A
    // TODO circular dependencies between fields should always result in ErrorType?

    def typecheckTerm(e: Term, p: Prototype = Que, scope: Scope = Map()): TypedTerm = ??? // TODO term-continuations?

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
        val incompleteCellsAtTheEnd = Await.result(pool.quiescentIncompleteCells, 10.seconds) // TODO
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

  def typecheckInParallel(symbolUniverse: SymbolUniverse, rootExpr: Term, rootPrototype: Prototype = Que, rootScope: Map[Symbol, Type] = Map()): Option[Term] = {
    val pool         = new HandlerPool(1) // TODO
    val typeChecker  = Parallel(symbolUniverse, pool, new AtomicBoolean(false))
    typeChecker.run[Term]{cont =>
      val lazyTypedTerm = typeChecker.typecheckTerm(rootExpr, rootPrototype, rootScope)
      typeChecker.expandTermFutures(lazyTypedTerm) { typedTerm =>
        cont(typedTerm)
      }
    }
  }


  def typecheckSequentially(symbolUniverse: SymbolUniverse, rootExpr: Term, rootPrototype: Prototype = Que, rootScope: Map[Symbol, Type] = Map()): Option[Term] = {
    try {
      Some(NoFuture.typecheckTerm(symbolUniverse, rootExpr, rootPrototype, rootScope))
    } catch {
      case e: TypecheckingError => e.printStackTrace(); None
      case e: NotImplementedError => e.printStackTrace(); None
    }
  }
}
