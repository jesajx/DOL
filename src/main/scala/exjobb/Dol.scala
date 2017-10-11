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

object Dol {
  type Symbol = Int

  // TODO Idea: Maybe treat entire typechecker as a constraint-solver and
  // typecheck in two steps: 1) gather constraints, 2) solve them. This is
  // similar what I tried with Hindley-Milner. Technically this is actually
  // what is happening in the current implementation (gather and solve are
  // done concurrently), but it could be beneficial to make it more explicit
  // (simpler, more machine-like code).


  sealed trait Tree {
    val treeHeight: Int
    val totNumNodes: Int
    // TODO
  }
  sealed trait Expr extends Tree {
    @volatile var assignedType: Option[CanonicalType] = None // TODO does it need to be volatile? // TODO vs trait TypedExpr?
    def withType(typ: CanonicalType): Expr // TODO as standalone function? that way we don't have to redeclare it in every subclass...
    def withTypeOption(typeOption: Option[CanonicalType]): Expr
  }
  sealed trait Term  extends Expr {
    def withType(typ: CanonicalType): Term
    def withTypeOption(typeOption: Option[CanonicalType]): Term
  }
  sealed trait Value extends Term {
    def withType(typ: CanonicalType): Value
    def withTypeOption(typeOption: Option[CanonicalType]): Value
  }
  sealed trait Type  extends Tree
  sealed trait Decl  extends Type
  sealed trait Def   extends Expr {
    def withType(typ: CanonicalType): Def
    def withTypeOption(typeOption: Option[CanonicalType]): Def
  }

  sealed trait CanonicalType extends Tree // TODO CanonicalType does not correctly represent DOT and will be removed.

  // Term ::=
  case class Var(x: Symbol) extends Term {
    val treeHeight = 1
    val totNumNodes = 1
    def withTypeOption(typeOption: Option[CanonicalType]) = {
      val res = this.copy()
      res.assignedType = typeOption
      res
    }
    def withType(typ: CanonicalType) = withTypeOption(Some(typ))
  }
  case class App(x: Symbol, y: Symbol) extends Term {
    val treeHeight = 1
    val totNumNodes = 1
    def withTypeOption(typeOption: Option[CanonicalType]) = {
      val res = this.copy()
      res.assignedType = typeOption
      res
    }
    def withType(typ: CanonicalType) = withTypeOption(Some(typ))
  }
  case class Let(x: Symbol, xTerm: Term, resTerm: Term) extends Term {
    val treeHeight = 1 + math.max(xTerm.treeHeight, resTerm.treeHeight)
    val totNumNodes = 1 + xTerm.totNumNodes + resTerm.totNumNodes
    def withTypeOption(typeOption: Option[CanonicalType]) = {
      val res = this.copy()
      res.assignedType = typeOption
      res
    }
    def withType(typ: CanonicalType) = withTypeOption(Some(typ))
  }
  case class Sel(x: Symbol, a: Symbol) extends Term {
    val treeHeight = 1
    val totNumNodes = 1
    def withTypeOption(typeOption: Option[CanonicalType]) = {
      val res = this.copy()
      res.assignedType = typeOption
      res
    }
    def withType(typ: CanonicalType) = withTypeOption(Some(typ))
  }

  // Value ::=
  case class Obj(x: Symbol, xType: Type, body: Def) extends Value {
    val treeHeight = 1 + math.max(xType.treeHeight, body.treeHeight)
    val totNumNodes = 1 + xType.totNumNodes + body.totNumNodes
    def withTypeOption(typeOption: Option[CanonicalType]) = {
      val res = this.copy()
      res.assignedType = typeOption
      res
    }
    def withType(typ: CanonicalType) = withTypeOption(Some(typ))
  }
  case class Fun(x: Symbol, xType: Type, body: Term) extends Value {
    val treeHeight = 1 + math.max(xType.treeHeight, body.treeHeight)
    val totNumNodes = 1 + xType.totNumNodes + body.totNumNodes
    def withTypeOption(typeOption: Option[CanonicalType]) = {
      val res = this.copy()
      res.assignedType = typeOption
      res
    }
    def withType(typ: CanonicalType) = withTypeOption(Some(typ))
  }

  // Def ::=
  case class FieldDef(a: Symbol, aTerm: Term) extends Def {
    val treeHeight = 1 + aTerm.treeHeight
    val totNumNodes = 1 + aTerm.totNumNodes
    def withTypeOption(typeOption: Option[CanonicalType]) = {
      val res = this.copy()
      res.assignedType = typeOption
      res
    }
    def withType(typ: CanonicalType) = withTypeOption(Some(typ))
  }
  case class TypeDef(a: Symbol, aType: Type) extends Def {
    val treeHeight = 1 + aType.treeHeight
    val totNumNodes = 1 + aType.totNumNodes
    def withTypeOption(typeOption: Option[CanonicalType]) = {
      val res = this.copy()
      res.assignedType = typeOption
      res
    }
    def withType(typ: CanonicalType) = withTypeOption(Some(typ))
  }
  case class AndDef(left: Def, right: Def)  extends Def {
    val treeHeight = 1 + math.max(left.treeHeight, right.treeHeight)
    val totNumNodes = 1 + left.totNumNodes + right.totNumNodes
    def withTypeOption(typeOption: Option[CanonicalType]) = {
      val res = this.copy()
      res.assignedType = typeOption
      res
    }
    def withType(typ: CanonicalType) = withTypeOption(Some(typ))
  }

  // ExtraTerm ::=
  case class TFun(t: Symbol, tType: Type, x: Symbol, xType: Type, body: Term) extends Value { // Fun(r, RecType(r, t and {x=...}))
    val treeHeight = 1 + math.max(tType.treeHeight, math.max(xType.treeHeight, body.treeHeight))
    val totNumNodes = 1 + tType.treeHeight + xType.totNumNodes + body.totNumNodes
    def withTypeOption(typeOption: Option[CanonicalType]) = {
      val res = this.copy()
      res.assignedType = typeOption
      res
    }
    def withType(typ: CanonicalType) = withTypeOption(Some(typ))
  }
  case class TLightFun(t: Symbol, x: Symbol, body: Term) extends Value { // TFun(t, <generated>, x, <generated>, body)
    val treeHeight = 1 + body.treeHeight
    val totNumNodes = 1 + body.totNumNodes
    def withTypeOption(typeOption: Option[CanonicalType]) = {
      val res = this.copy()
      res.assignedType = typeOption
      res
    }
    def withType(typ: CanonicalType) = withTypeOption(Some(typ))
  }
  case class TApp(x: Symbol, t: Def, y: Symbol) extends Value { // App(x, Obj(r, t and {y=...})
    val treeHeight = 1 + t.treeHeight
    val totNumNodes = 1 + t.totNumNodes
    def withTypeOption(typeOption: Option[CanonicalType]) = {
      val res = this.copy()
      res.assignedType = typeOption
      res
    }
    def withType(typ: CanonicalType) = withTypeOption(Some(typ))
  }
  case class TLightApp(x: Symbol, y: Symbol) extends Value { // TApp(x, <generated>, y)
    val treeHeight = 1
    val totNumNodes = 1
    def withTypeOption(typeOption: Option[CanonicalType]) = {
      val res = this.copy()
      res.assignedType = typeOption
      res
    }
    def withType(typ: CanonicalType) = withTypeOption(Some(typ))
  }
  case class ClassDecl(x: Symbol, parent: Type, defs: Def) extends Value { // C = RecType(x, defsType and parent)
    val treeHeight = 1 + defs.treeHeight
    val totNumNodes = 1 + defs.totNumNodes
    def withTypeOption(typeOption: Option[CanonicalType]) = {
      val res = this.copy()
      res.assignedType = typeOption
      res
    }
    def withType(typ: CanonicalType) = withTypeOption(Some(typ))
  }


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

  type TypedTerm = Term // TODO Do something a bit more special here? Only serves as documentation right now.
  //sealed trait Typed { // TODO maybe something like this?
  //  val assignedType: Type
  //}
  //trait TypedTerm extends Term with Typed

  def pairToList[T](pair: (T, T)): List[T] = List(pair._1, pair._2)
  def max(rest: Int*): Int = rest.toList.max

  // TODO what about TypeProj that is not an object?
  case class CanonicalObjType(x: Symbol, xFields: Map[Symbol, CanonicalType], xTypes: Map[Symbol, (CanonicalType, CanonicalType)], xProjs: Set[TypeProj]) extends CanonicalType { // TODO store scope? // TODO aka "ScaryIntersectionType" // TODO rename to "CanonicalObjIntersectionType"?
    val treeHeight = 1 + (xFields.values ++ xTypes.values.flatMap{pairToList}).map{_.treeHeight}.fold(0){math.max}
    val totNumNodes = 1 + (xFields.values ++ xTypes.values.flatMap{pairToList}).map{_.totNumNodes}.sum + xProjs.size
  }
  case class CanonicalFunType(x: Symbol, xType: CanonicalType, resType: CanonicalType, projs: Set[TypeProj]) extends CanonicalType { // TODO differentiate between projs that have subtypes and projs that don't (opaqueProjs). TODO maybe Map[TypeProj, (Option[CanonicalObjType], Option[CanonicalObjType])]
    val treeHeight = 1 + max(xType.treeHeight, resType.treeHeight, 2)
    val totNumNodes = 1 + xType.totNumNodes + resType.totNumNodes + projs.size
  }
  case object CanonicalTop extends CanonicalType {
    val treeHeight = 1
    val totNumNodes = 1
  }
  case object CanonicalBot extends CanonicalType {
    val treeHeight = 1
    val totNumNodes = 1
  }
  case object CanonicalErrorType extends CanonicalType {
    val treeHeight = 1
    val totNumNodes = 1
  }
  case class CanonicalFuture(cell: Meh[CanonicalType]) extends CanonicalType {
    val treeHeight = 1
    val totNumNodes = 1
  }
  type CanonicalPrototype = CanonicalType
  case object CanonicalQue extends CanonicalPrototype {
    val treeHeight = 1
    val totNumNodes = 1
  }
  // TODO lazy futures? i.e. wrap future in lazy type such that the cell is never created.

  object NoFuture {

    // With x: T, T <: {a = L..U}, return U. Return None on error (e.g. if not
    // T <: {a = L..U} or if x is not in scope). If there are multiple
    // declarations of "a", return the intersection (AndType) of their upper
    // bounds.
    def typeProjectUpper2(su: SymbolUniverse, scope: Scope, x: Symbol, a: Symbol): Option[Type] = {
      // NOTE: The result may still reference the original TypeProjection.
      // E.g. upper(x.a) = y.b and upper(y.b) = x.a. But the caller may want
      // to see y.b. Therefore we do not upcast here.
      def inner(scope: Scope, x: Symbol, a: Symbol, visited: Set[TypeProj]): Option[Type] = scope.get(x) match {
        case Some(RecType(y, yType)) =>
          for {
            yaUpper <- inner(scope + (y -> yType), y, a, visited)
          } yield typeRenameVar(y, x, yaUpper)
        case Some(AndType(left, right)) =>
          val z = su.newSymbol()
          val leftUpperOption = inner(scope + (z -> left), x, a, visited)
          val rightUpperOption = inner(scope + (z -> right), x, a, visited)
          (leftUpperOption, rightUpperOption) match {
            case (Some(leftUpper), Some(rightUpper)) => Some(typeRenameVar(z, x, andType(leftUpper, rightUpper)))
            case (Some(leftUpper), None)             => Some(leftUpper)
            case (None, Some(rightUpper))            => Some(rightUpper)
            case (None, None)                        => None
          }
        case Some(bProj @ TypeProj(y, b)) if visited(bProj) =>
          None
        case Some(bProj @ TypeProj(y, b)) if !visited(bProj) =>
          for {
            bUpperType <- inner(scope, y, b, visited + bProj)
            z <- Some(su.newSymbol())
            aUpperType <- inner(scope + (z -> bUpperType), z, a, visited + bProj)
          } yield typeRenameVar(z, x, aUpperType)
        case Some(TypeDecl(b, _, bUpperType)) if a == b => Some(bUpperType)
        case Some(Bot) => ??? // TODO <: TypeDecl(a, x.a..x.a)? the upper bound is Top?
        case Some(_) => None
        case None => ???; Some(ErrorType)
      }
      inner(scope, x, a, Set(TypeProj(x, a)))
    }

    def typeProjectLower2(su: SymbolUniverse, scope: Scope, x: Symbol, a: Symbol): Option[Type] = ???

    def minType(scope: Scope, left: Type, right: Type): Option[Type] = ??? // Some(left) if left <: right else Some(right) if right <: left else None

    sealed trait Constraint

    case object TrueConstraint extends Constraint
    case object FalseConstraint extends Constraint
    case class OrConstraint(left: Constraint, right: Constraint) extends Constraint
    case class AndConstraint(left: Constraint, right: Constraint) extends Constraint
    case class SubtypeConstraint(scope: Scope, left: Type, right: Type) extends Constraint // TODO add Variance here?
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
          case aProj @ TypeProj(x, a) if x == fromVar && typeProjectUpper2(su, scope, toVar, a) == None =>
            typeProjectUpper2(su, scope, x, a) match {
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

    // TODO reintroduce OrType? and have lub(a,b) = simplify(OrType(a, b))?
    def leastCommonSupertype(su: SymbolUniverse, scope: Scope, lhs: Type, rhs: Type): Type = {
      def inner(scope: Scope, lhs: Type, rhs: Type, visitedLeft: Set[TypeProj], visitedRight: Set[TypeProj]): Type = (lhs, rhs) match {
        case (Top, _) | (_, Top) => Top
        case (Bot, _) => rhs
        case (_, Bot) => lhs
        case (ErrorType, _) | (_, ErrorType) =>
          ErrorType

        case (RecType(x, xType), _) =>
          // TODO is this correct?
          val newXType = inner(scope + (x -> xType), xType, rhs, visitedLeft, visitedRight)
          val z = su.newSymbol()
          recType(z, varEliminatingTransform(su, scope + (x -> xType) + (z -> newXType), x, z, newXType))
        case (_, RecType(y, yType)) =>
          // TODO is this correct?
          val newYType = inner(scope + (y -> yType), lhs, yType, visitedLeft, visitedRight)
          val z = su.newSymbol()
          recType(z, varEliminatingTransform(su, scope + (y -> yType) + (z -> newYType), y, z, newYType))

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
          val aUpperType = if (visitedLeft(aProj)) Top else typeProjectUpper2(su, scope, x, a).getOrElse{???; ErrorType}
          val bLowerType = if (visitedRight(bProj)) Bot else typeProjectLower2(su, scope, y, b).getOrElse{???; ErrorType}

          andType(
            inner(scope, aUpperType, rhs, visitedLeft + aProj, visitedRight),
            inner(scope, lhs, bLowerType, visitedLeft, visitedRight + bProj)) // TODO is this correct?

        case (aProj @ TypeProj(x, a), _) =>
          val aUpperType = if (visitedLeft(aProj)) Top else typeProjectUpper2(su, scope, x, a).getOrElse{???; ErrorType}
          inner(scope, aUpperType, rhs, visitedLeft + aProj, visitedRight)
        case (_, bProj @ TypeProj(y, b))  =>
          val bLowerType = if (visitedRight(bProj)) Bot else typeProjectLower2(su, scope, y, b).getOrElse{???; ErrorType}
          inner(scope, lhs, bLowerType, visitedLeft, visitedRight + bProj)

        case (FunType(x, xType, xResType), FunType(y, yType, yResType)) if x != y =>
          inner(scope, lhs, renameTypeBoundVar(x, rhs), visitedLeft, visitedRight)
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


    def max(x: Int): Int = ???
    def max(x: Double): Double = ???

    def greatestCommonSubtype(su: SymbolUniverse, scope: Scope, left: Type, right: Type): Type = andType(left, right) // TODO always correct? // TODO simplify?

    // TODO isSubtypeOf(scope, a, b)?

    def solveConstraint(su: SymbolUniverse, solveSet: Set[TypeProj], constraint: Constraint): Option[Map[TypeProj, Type]] = { // TODO

      def mergeConstraints(left: Map[TypeProj, (Scope, Type, Type)], right: Map[TypeProj, (Scope, Type, Type)]): Map[TypeProj, (Scope, Type, Type)] = {
        mapUnion(left, right){case ((s1, l1, u1), (s2, l2, u2)) =>
          val commonScope = s1 ++ s2 // TODO will concatenating scopes work? If the same var map to the same type and has otherwise been renamed... having different scopes seems wrong in the first place...
          (commonScope, leastCommonSupertype(su, commonScope, l1, l2), greatestCommonSubtype(su, commonScope, u1, u2))
        }
      }

      val defaults = solveSet.map(p => (p -> (Map(): Scope, Bot, Top))).toMap

      def subsolve(constraint: Constraint): Option[Map[TypeProj, (Scope, Type, Type)]] = constraint match {
        case AndConstraint(left, right) =>
          for {
            leftRes  <- subsolve(left)
            rightRes <- subsolve(right)
            merged <- Some(mergeConstraints(leftRes, rightRes)) // TODO check if !(lower <: upper) here?
          } yield merged
        case TrueConstraint =>
          Some(defaults)
        case SubtypeConstraint(scope, left, proj: TypeProj)  if solveSet(proj) =>
          Some(defaults + (proj -> (scope, left, Top)))
        case SubtypeConstraint(scope, proj: TypeProj, right) if solveSet(proj) =>
          Some(defaults + (proj -> (scope, Bot, right)))
        case _: OrConstraint =>
          ??? // should not happen if DNF.
        case _ => // FalseConstraints and bad SubtypeConstraints
          None
      }

      sealed trait SolveResult
      case class  Solution(solution: Map[TypeProj, Type]) extends SolveResult
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
              ???
          }
        case _ =>
          subsolve(constraint) match {
            case Some(res) =>
              Solution(res.map{case (p, (s, l, u)) => (p -> l)}) // TODO check VARIANCE!!!!
            case None => NoSolution
          }
      }

      solve(dnf(constraint)) match {
        case Solution(res) => Some(res)
        case _ => None
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

    def gather(su: SymbolUniverse, rootScope: Scope, solveSet: Set[TypeProj], rootFrom: Type, rootTo: Prototype): Constraint = {
      def gath(scope: Scope, from: Type, to: Type, visitedUp: Set[TypeProj], visitedDown: Set[TypeProj]): Constraint = (from, to) match {
        case (FieldDecl(a, aType), Top) => gath(scope, from, FieldDecl(a, Top), visitedUp, visitedDown) // TODO TrueConstraint? still necessary to decide on variance?
        case (Bot, FieldDecl(a, aType)) => gath(scope, FieldDecl(a, Bot), to, visitedUp, visitedDown)

        case (Bot, TypeDecl(a, aLowerType, aUpperType)) => ??? // TODO TrueConstraint?
        case (TypeDecl(a, aLowerType, aUpperType), Top) => gath(scope, from, TypeDecl(a, Bot, Top), visitedUp, visitedDown)

        case (FunType(x, _, _), Top) => gath(scope, from, FunType(x, Bot, Top), visitedUp, visitedDown)
        case (Bot, FunType(x, _, _)) => gath(scope, FunType(x, Top, Bot), to, visitedUp, visitedDown)

        case (RecType(x, xType), _) =>
          if (!scope.contains(x)) ??? // TODO
          gath(scope + (x -> xType), xType, to, visitedUp, visitedDown)
        case (_, RecType(y, yType)) =>
          if (!scope.contains(y)) ??? // TODO
          gath(scope + (y -> yType), from, yType, visitedUp, visitedDown)

        case (FunType(x, xType, xResType), FunType(y, yType, yResType)) if x != y =>
          andConstraint(
            gath(scope, xType, yType, visitedUp, visitedDown),
            gath(scope + (x -> xType) + (y -> yType), xResType, yResType, visitedUp, visitedDown)) // TODO technically: x==y && (x -> min(xType, yType))
        case (FunType(x, xType, xResType), FunType(y, yType, yResType)) if x == y => ???
          andConstraint(
            gath(scope, xType, yType, visitedUp, visitedDown),
            gath(scope + (x -> andType(xType, yType)), xResType, yResType, visitedUp, visitedDown)) // TODO

        case (_, AndType(left, right)) => andConstraint(gath(scope, from, left, visitedUp, visitedDown), gath(scope, from, right, visitedUp, visitedDown))
        case (AndType(left, right), _) => orConstraint(gath(scope, left, to, visitedUp, visitedDown), gath(scope, right, to, visitedUp, visitedDown)) // TODO can we avoid the or by expanding further somehow? e.g. min(leftC,rightC)

        case (aProj @ TypeProj(x, a), bProj @ TypeProj(y, b))  =>
          if (aProj == bProj) {
            TrueConstraint
          } else if (solveSet(aProj) && solveSet(bProj)) {
            FalseConstraint
          } else if (solveSet(aProj) && !solveSet(bProj)) {
            val bLower = ??? // TODO get rid of other type-projections in solveSet.
            SubtypeConstraint(scope, from, bLower)
          } else if (!solveSet(aProj) && solveSet(bProj)) {
            val aUpper = ??? // TODO get rid of other type-projections in solveSet.
            SubtypeConstraint(scope, aUpper, to)
          } else { // if (!solveSet(aProj) && !solveSet(bProj))
            val aUpperType = if (visitedUp(aProj)) Top else typeProjectUpper2(su, scope, x, a).getOrElse{???; ErrorType}
            val bLowerType = if (visitedDown(bProj)) Bot else typeProjectLower2(su, scope, y, b).getOrElse{???; ErrorType}

            orConstraint(
              gath(scope, aUpperType, bProj, visitedUp + aProj, visitedDown),
              gath(scope, aProj, bLowerType, visitedUp, visitedDown + bProj))
          }

        case (aProj @ TypeProj(x, a), _) =>
          if (solveSet(aProj)) {
            val bLower = ??? // TODO get rid of all type-projections.
            SubtypeConstraint(scope, from, bLower)
          } else {
            val aUpperType = if (visitedUp(aProj)) Top else typeProjectUpper2(su, scope, x, a).getOrElse{???; ErrorType}
            gath(scope, aUpperType, to, visitedUp + aProj, visitedDown)
          }

        case (_, bProj @ TypeProj(y, b)) =>
          if (solveSet(bProj)) {
            val aUpper = ??? // TODO get rid of all type-projections.
            SubtypeConstraint(scope, aUpper, to)
          } else {
            val bLowerType = if (visitedDown(bProj)) Bot else typeProjectLower2(su, scope, y, b).getOrElse{???; ErrorType}
            gath(scope, from, bLowerType, visitedUp, visitedDown)
          }

        case (FieldDecl(a, aType), FieldDecl(b, bType)) if a == b => gath(scope, aType, bType, visitedUp, visitedDown)

        case (TypeDecl(a, aLowerType, aUpperType), TypeDecl(b, bLowerType, bUpperType)) if a == b =>
          andConstraint(
            gath(scope, bLowerType, aLowerType, visitedUp, visitedDown),
            gath(scope, aUpperType, bUpperType, visitedUp, visitedDown))

        case (Top, Top) => TrueConstraint
        case (Bot, Top) => TrueConstraint
        case (Bot, Bot) => TrueConstraint

        case _ => FalseConstraint
      }
      gath(rootScope, rootFrom, rootTo, Set(), Set())
    }



//    def typecheckTerm(su: SymbolUniverse, term: Term, prototype: Prototype = Que, scope: Scope = Map()): TypedTerm = (term, prototype) match {
//      case (Var(x), p) =>
//        term.withType{
//          scope.get(x) match {
//            case Some(xType) =>
//              raise(scope, xType, p) match {
//                case Some(res) => res
//                case None => ???; ErrorType
//              }
//            case None => ???; ErrorType // TODO
//          }
//        }
//      case (Let(x, xTerm, resTerm), p) =>
//        if (scope.contains(x)) ??? // TODO rename
//        val typedXTerm = typecheckTerm(xTerm, Que, scope)
//        val xType = typedXTerm.assignedType.get // TODO Annoying, choose some typesafe way to do this instead...
//        val typedResTerm = typecheckTerm(resTerm, p, scope + (x -> xType))
//        Let(x, typedXTerm, typedResTerm).withType(eliminateVarUp(su, scope, x, typedRes))
//      case (Sel(x, a), p) =>
//        term.withType {
//          typecheckTerm(Var(x), FieldDecl(a, p), scope).assignedType.get match {
//            case FieldDecl(b, bType) if a == b =>
//              bType
//            case _ =>
//              ???; ErrorType // TODO
//          }
//        }
//      case (App(x, y), p) =>
//        val z = su.newSymbol()
//        term.withType{
//          typecheckTerm(Var(x), FunType(z, Que, Que), scope).assignedType.get match {
//            case FunType(w, zType, zResType) if w == z =>
//              if (scope.contains(z)) ??? // TODO rename
//              val yType = typecheckTerm(Var(y), zType, scope).assignedType.get
//            raise(scope, renameToUniqueVar(z, y, zResType), p)
//            case _ =>
//              ???; ErrorType // TODO
//          }
//        }
//      case (Fun(x, _, _), Top) =>
//        typecheckTerm(term, Fun(x, Bot, Top), scope)
//      case (Fun(x, _, _), FunType(y, _, _)) if x != y =>
//        val typedTerm = typecheckTerm(term, renameTypeBoundVar(x, p), scope)
//        val typ = typedTerm.assignedType.get
//        typedTerm.withType(renameTypeBoundVar(y, typ))
//      case (Fun(x, xType, resTerm), FunType(y, argPrototype, resPrototype)) if x == y =>
//        // TODO Is it fine to do lower/raise of arg and res separately? Or is
//        // it necessary to do one call to raise at the end using the whole
//        // prototype?
//        val loweredXType = lower(scope, xType, argPrototype)
//        val localScope = scope + (x -> xType) // TODO xType vs loweredXType? what happens if loweredXType is Bot? special case that?
//        val typedResTerm = typecheckTerm(resTerm, resPrototype, localScope)
//        val resType = typedResTerm.assignedType.get
//        Fun(x, raisedXType, typedResTerm).withType(FunType(x, loweredXType, resType))
//      case (Obj(x, xType, defs), p) =>
//        val localScope = scope + (x -> xType)
//        def typecheckDef(d: Def): Def = d match {
//          case AndDef(left, right) =>
//            AndDef(typecheckDef(left), typecheckDef(right))
//          case FieldDef(a, aTerm) =>
//            val declPrototypes = ???
//            val aPrototype = ???
//            val typedATerm = typecheckTerm(aTerm, aPrototype, localScope)
//            ???
//          case TypeDef(a, aType) =>
//            val typ = TypeDecl(a, aType, aType)
//            val declPrototypes = ???
//            ???
//        }
//        val typedDefs = typecheckDef(defs)
//        Obj(x, xType, typedDefs).withType(raise(scope, xType, p))
//      // TODO TFun
//      // TODO TApp
//      // TODO TLightFun
//      // TODO TLightApp
//      // TODO LightObj
//      case _ =>
//        ???; term.withType(ErrorType) // TODO
//    }

    def raise(su: SymbolUniverse, scope: Scope, from: Type, to: Prototype): Option[Type] = {
      val z = su.newSymbol()
      val (newTo, solveSet) = prep(su, z, to)
      val constraint = gather(su, scope, solveSet, from, newTo)
      solveConstraint(su, solveSet, constraint) match {
        case Some(solution) => Some(applyConstraintSolution(to, solution))
        case None           => None
      }
    }

//    def lower(scope: Scope, from: Type, to: Prototype, visitedUp: Set[TypeProj], visitedDown: Set[TypeProj]): Option[Type] = ???
//
//    // TODO INV: visitedUp.map{p => p <: from}
//    // TODO INV: visitedDown.map{p => to <: p}
//    def raise(scope: Scope, from: Type, to: Prototype, visitedUp: Set[TypeProj], visitedDown: Set[TypeProj]): Option[Type] = (from, to) match {
//      case (_, Que) =>
//        Some(from)
//      case (_, Top) =>
//        Some(Top)
//      case (RecType(x, xType), RecType(y, yType)) if x == y => // TODO use z = result of raise? what happens when we do y.T?
//        for {
//          res <- raise(scope, xType, yType, visitedUp, visitedDown)
//        } yield RecType(x, res)
//      case (RecType(x, xType), _) =>
//        ??? // TODO canonicalize? No, Rec(x, ... Rec(y, y.T)) then y.T is NOT the same as x.T? No, x: RecType(x, xType) means x: xType. so RecTypes can be canonicalized.
//      case (_, RecType(y, yType)) =>
//        ???
//      case (_, AndType(left, right)) =>
//        for {
//          leftRaise <- raise(from, left)
//          rightRaise <- raise(from, right)
//        } yield AndType(leftRaise, rightRaise)
//      case (AndType(left, right), _) =>
//        (raise(left, to), raise(right, to)) match {
//          case (Some(leftRaise), Some(rightRaise)) => typeMin(leftRaise, rightRaise)
//          case (Some(leftRaise), None)             => leftRaise
//          case (None, Some(rightRaise))            => rightRaise
//          case (None, None)                        => None
//        }
//      case (TypeProj(x, a), TypeProj(y, b)) if x == y && a == b =>
//        Some(to)
//      case (from @ TypeProj(x, a), to @ TypeProj(y, b)) if !(x == y && a == b) =>
//        // TODO What if y.b = Que..Que?
//        // Maybe we should treat raise() as a constraint-solving problem...
//        // Replace each Que (even outside TypeDecls) with a TypeProj.
//
//
//        val aUpperAlts = if (visitedUp(from)) Seq(Top) else typeProjectUpper2(scope, x, a)
//        val leftAlts = aUpperAtls.map{aUpper => raise(scope, aUpper, to, visitedUp + from, visitedDown)}.flatten
//
//        val bLowerAlts = if (visitedDown(to)) Seq(Bot) else typeProjectLower2(scope, y, b)
//        val rightAlts = bLowerAlts.map{bLower => raise(scope, from, bLower, visitedDown, visitedUp + to)}.flatten
//
//        // TODO Is it bad to use raise(a,b) in place of isSubtypeOf(a,b)?
//        // Won't raise(a,b) fail when there are multiple options while
//        // isSubtypeOf(a,b) is fine as long as there _are_ options?
//        // The problem is that b might reduce to Que in our case,
//        // e.g. y: {T: Que..Que}).
//        // Let raise return Seq[Type] instead of Option[Type]?
//
//        if ((leftAlts ++ rightAlts).flatten.size != 0)
//          Some(To)
//        else
//          None
//      case (_, to @ TypeProj(y, b)) if !(x == y && a == b) =>
//        val aUpper = if (visitedUp(from)) Top else typeProjectUpper2(scope, x, a)
//        raise(scope, aUpper, to, visitedUp + from, visitedDown)
//      case (from @ TypeProj(x, a), _) if !(x == y && a == b) =>
//        val aUpper = if (visitedUp(from)) Top else typeProjectUpper2(scope, x, a)
//        raise(scope, aUpper, to, visitedUp + from, visitedDown)
//      case (FunType(x, xType, xResType), FunType(y, yType, yResType)) if x != y =>
//        ??? // TODO rename
//      case (FunType(x, xType, xResType), FunType(y, yType, yResType)) if x == y =>
//        for {
//          argType <- lower(scope, xType, yType)
//          resType <- raise(scope, xResType, yResType)
//        } yield FunType(x, argType, resType)
//      case (Bot, FunType(x, xType, resType)) =>
//        raise(scope, FunType(x, Top, Bot), to)
//      case (FieldDecl(a, aType), FieldDecl(b, bType)) if a == b =>
//        for {
//          cType <- raise(scope, aType, bType)
//        } yield FieldDecl(a, cType)
//      case (Bot, FieldDecl(a, aType)) =>
//        raise(scope, FieldDecl(a, Bot), to)
//      case (TypeDecl(a, aLowerType, aUpperType), TypeDecl(b, bLowerType, bUpperType)) if a == b =>
//        for {
//          lowerBound <- lower(scope, aLowerType, bLowerType, visitedUp, visitedDown)
//          upperBound <- raise(scope, aUpperType, bUpperType, visitedUp, visitedDown)
//          lowerBoundRaisedToUpperBound <- raise(scope, lowerBound, upperBound, visitedUp, visitedDown) // NOTE: only to check for errors.
//        } yield TypeDecl(a, lowerBound, upperBound)
//      case (Bot, TypeDecl(a, aLowerType, aUpperType)) =>
//        ??? // TODO is this correct? what if to=TypeDecl(a, Que, Que)? res=TypeDecl(a, Bot, Top)?, res=TypeDecl(a, x.a, x.a)?
//        for {
//          lowerBound <- lower(scope, Top, aLowerType, visitedUp, visitedDown)
//          upperBound <- raise(scope, Bot, aUpperType, visitedUp, visitedDown)
//          lowerBoundRaisedToUpperBound <- raise(scope, lowerBound, upperBound, visitedUp, visitedDown) // NOTE: only to check for errors.
//        } yield TypeDecl(a, lowerBound, upperBound)
//      case _ =>
//        None
//    }


    def canonicalTypeRenameVar(fromVar: Symbol, toVar: Symbol, t: CanonicalType): CanonicalType = t match {
      case CanonicalObjType(x, fields, types, projs) if x != fromVar =>
        val newFields = fields.map{case (a, aType) =>
          a -> NoFuture.canonicalTypeRenameVar(fromVar, toVar, aType)
        }
        val newTypes = types.map{case (a, (aLowerType, aUpperType)) =>
          a -> (NoFuture.canonicalTypeRenameVar(fromVar, toVar, aLowerType),
                NoFuture.canonicalTypeRenameVar(fromVar, toVar, aUpperType))
        }
        val newProjs = projs.map{
          case TypeProj(y, a) if fromVar == y => TypeProj(toVar, a)
          case t => t
        }
        CanonicalObjType(x, newFields, newTypes, newProjs)
      case CanonicalFunType(x, xType, resType, projs) if x != fromVar =>
        val newXType = NoFuture.canonicalTypeRenameVar(fromVar, toVar, xType)
        val newResType = NoFuture.canonicalTypeRenameVar(fromVar, toVar, resType)
        val newProjs = projs.map{
          case TypeProj(y, a) if fromVar == y => TypeProj(toVar, a)
          case t => t
        }
        CanonicalFunType(x, newXType, newResType, newProjs)
      case _ =>
        t
    }

    def canonicalTypeRenameBoundVar(toVar: Symbol, t: CanonicalType): CanonicalType = t match {
      case CanonicalObjType(x, fields, types, projs) if x != toVar =>
        canonicalTypeRenameVar(x, toVar, CanonicalObjType(toVar, fields, types, projs))
      case CanonicalFunType(x, xType, resType, projs) if x != toVar =>
        canonicalTypeRenameVar(x, toVar, CanonicalFunType(toVar, xType, resType, projs))
      case _ =>
        t
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

    def renameTypeBoundVar(toVar: Symbol, typ: Type): Type = typ match {
      case RecType(x, xType) if x != toVar =>
        RecType(toVar, typeRenameVar(x, toVar, xType))
      case FunType(x, xType, resType) if x != toVar =>
        FunType(toVar, xType, typeRenameVar(x, toVar, resType))
      case _ =>
        typ
    }
    def renameTermBoundVar(toVar: Symbol, term: Term): Term = term match {
      case Let(x, xTerm, resTerm) if x != toVar => termRenameVar(x, toVar, Let(toVar, xTerm, resTerm))
      case Obj(x, xType, d)       if x != toVar => termRenameVar(x, toVar, Obj(toVar, xType, d))
      case Fun(x, xType, resTerm) if x != toVar => termRenameVar(x, toVar, Fun(toVar, xType, resTerm))
      case _ => term
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

    def equalDefs(su: SymbolUniverse, first: Def, second: Def): Boolean = {
      val firstMap = defAsMap(first)
      val secondMap = defAsMap(second)

      (firstMap.keys == secondMap.keys
        && mapIntersect(firstMap, secondMap){(left, right) =>
          (left, right) match {
            case (FieldDef(a, aTerm), FieldDef(b, bTerm)) if a == b => equalTerms(su, aTerm, bTerm)
            case (TypeDef(a, aType), TypeDef(b, bType)) if a == b   => equalTypes(su, aType, bType)
            case _ => false
          }
        }.values.forall{(x: Boolean) => x})
    }

    // TODO
    def equalTerms(su: SymbolUniverse, first: Term, second: Term): Boolean = {
      val assignedTypesEqual = (for {
        firstType <- first.assignedType
        secondType <- second.assignedType
      } yield canonicalEqualTypes(su, firstType, secondType)).getOrElse(true)

      assignedTypesEqual && ((first, second) match {
        case (Var(_), Var(_))       => (first == second)
        case (App(_, _), App(_, _)) => (first == second)
        case (Sel(_, _), Sel(_, _)) => (first == second)
        case (Let(x, xTerm, xResTerm), Let(y, yTerm, yResTerm)) if x == y =>
          (equalTerms(su, xTerm, yTerm)
            && equalTerms(su, xResTerm, yResTerm))
        case (Obj(x, xType, xBody), Obj(y, yType, yBody)) if x == y =>
          (equalTypes(su, xType, yType)
            && equalDefs(su, xBody, yBody))
        case (Fun(x, xType, xBody), Fun(y, yType, yBody)) if x == y =>
          (equalTypes(su, xType, yType)
            && equalTerms(su, xBody, yBody))
        case (Let(x, xTerm, xResTerm), Let(y, yTerm, yResTerm)) if x != y =>
          val z = su.newSymbol()
          equalTerms(su,
            NoFuture.termRenameVar(x, z, Let(z, xTerm, xResTerm)),
            NoFuture.termRenameVar(y, z, Let(z, yTerm, yResTerm)))
        case (Obj(x, xType, xBody), Obj(y, yType, yBody)) if x != y =>
          val z = su.newSymbol()
          equalTerms(su,
            NoFuture.termRenameVar(x, z, Obj(z, xType, xBody)),
            NoFuture.termRenameVar(y, z, Obj(z, yType, yBody)))
        case (Fun(x, xType, xBody), Fun(y, yType, yBody)) if x != y =>
          val z = su.newSymbol()
          equalTerms(su,
            NoFuture.termRenameVar(x, z, Fun(z, xType, xBody)),
            NoFuture.termRenameVar(y, z, Fun(z, yType, yBody)))
        case _ =>
          false
      })
    }



    def equalTypes(su: SymbolUniverse, first: Type, second: Type): Boolean = (first, second) match {
      case (Bot, Bot) => true
      case (Top, Top) => true
      case (TypeProj(_, _), TypeProj(_, _)) => (first == second)
      case (FunType(x, xType, xResType), FunType(y, yType, yResType)) if x != y =>
        val z = su.newSymbol()
        equalTypes(su,
          FunType(z, xType, typeRenameVar(x, z, xResType)),
          FunType(z, yType, typeRenameVar(y, z, yResType)))
      case (FunType(x, xType, xResType), FunType(y, yType, yResType)) if x == y =>
        (equalTypes(su, xType, yType)
          && equalTypes(su, xResType, yResType))
      case (RecType(x, xType), RecType(y, yType)) if x != y =>
        val z = su.newSymbol()
        equalTypes(su,
          RecType(z, typeRenameVar(x, z, xType)),
          RecType(z, typeRenameVar(y, z, yType)))
      case (RecType(x, xType), RecType(y, yType)) if x == y =>
        equalTypes(su, xType, yType)
      case (AndType(ll, lr), AndType(rl, rr)) =>
        equalTypes(su, ll, rl) && equalTypes(su, lr, rr) // TODO order-independence!!!! // TODO or make sure AndType are in a canonical form everywhere?
      case (FieldDecl(a, aType), FieldDecl(b, bType)) if a == b =>
        equalTypes(su, aType, bType)
      case (TypeDecl(a, aLowerType, aUpperType), TypeDecl(b, bLowerType, bUpperType)) if a == b =>
        (equalTypes(su, aLowerType, bLowerType)
          && equalTypes(su, aUpperType, bUpperType))
      case _ => false
    }

    def canonicalEqualTypes(su: SymbolUniverse, first: CanonicalType, second: CanonicalType): Boolean = (first, second) match {
      case (CanonicalObjType(x, xFields, xTypes, xProjs), CanonicalObjType(y, yFields, yTypes, yProjs)) if x != y =>
        val z = su.newSymbol()
        canonicalEqualTypes(su,
          NoFuture.canonicalTypeRenameVar(x, z, CanonicalObjType(z, xFields, xTypes, xProjs)),
          NoFuture.canonicalTypeRenameVar(y, z, CanonicalObjType(z, yFields, yTypes, yProjs)))
      case (CanonicalObjType(x, xFields, xTypes, xProjs), CanonicalObjType(y, yFields, yTypes, yProjs)) if x == y =>
        (xFields.keys == yFields.keys
          && xTypes.keys == yTypes.keys
          && mapIntersect(xFields, yFields){canonicalEqualTypes(su, _, _)}.forall{_._2}
          && mapIntersect(xTypes, yTypes){
            case ((xALower, xAUpper), (yALower, yAUpper)) =>
              (canonicalEqualTypes(su, xALower, yALower)
                && canonicalEqualTypes(su, xAUpper, yAUpper))
          }.forall{_._2}
          && xProjs == yProjs)
      case (CanonicalFunType(x, xType, xResType, xAlias), CanonicalFunType(y, yType, yResType, yAlias)) if x != y =>
        val z = su.newSymbol()
        canonicalEqualTypes(su,
          NoFuture.canonicalTypeRenameVar(x, z, CanonicalFunType(z, xType, xResType, xAlias)),
          NoFuture.canonicalTypeRenameVar(y, z, CanonicalFunType(z, yType, yResType, yAlias)))
      case (CanonicalFunType(x, xType, xResType, xAlias), CanonicalFunType(y, yType, yResType, yAlias)) if x == y =>
        (canonicalEqualTypes(su, xType, yType)
          && canonicalEqualTypes(su, xResType, yResType)
          && xAlias == yAlias)
      case (CanonicalTop, CanonicalTop) => true
      case (CanonicalBot, CanonicalBot) => true
      case (CanonicalQue, CanonicalQue) => true
      case _ => false
    }

    def decanonicalize(typ: CanonicalType): Type = typ match {
      case CanonicalObjType(x, xFields, xTypes, xProjs) if !xFields.isEmpty || !xTypes.isEmpty || !xProjs.isEmpty =>
        val fields = xFields.map{case (a, aType) =>
          FieldDecl(a, NoFuture.decanonicalize(aType))
        }.toSeq
        val types = xTypes.map{case (a, (aLowerType, aUpperType)) =>
          TypeDecl(a, NoFuture.decanonicalize(aLowerType), NoFuture.decanonicalize(aUpperType))
        }.toSeq
        val projs = xProjs

        RecType(x, (fields ++ types ++ projs).reduce{AndType(_, _)})
      case CanonicalFunType(x, xType, resType, projs) =>
        projs.fold[Type](FunType(x, NoFuture.decanonicalize(xType), NoFuture.decanonicalize(resType))){andType(_, _)}
      case CanonicalTop => Top
      case CanonicalBot => Bot
      case CanonicalQue => Que
      case _ =>
        ErrorType
    }


    def typeProject(scope: CanonicalScope, x: Symbol, a: Symbol): Option[(CanonicalType, CanonicalType)] = for {
      xType <- scope.get(x)
      aType <- xType match {
        case CanonicalObjType(y, _, yTypes, _) => yTypes.get(a)
        case _ => None
      }
    } yield aType

    def typeProjectUpper(scope: CanonicalScope, x: Symbol, a: Symbol): Option[CanonicalType] = for {
      (_, aUpperType) <- typeProject(scope, x, a)
    } yield aUpperType

    def typeProjectLower(scope: CanonicalScope, x: Symbol, a: Symbol): Option[CanonicalType] = for {
      (aLowerType, _) <- typeProject(scope, x, a)
    } yield aLowerType

    def canonicalLeastCommonSupertype(su: SymbolUniverse, scope: CanonicalScope, firstType: CanonicalType, secondType: CanonicalType): CanonicalType = ???

    def canonicalGreatestCommonSubtype(su: SymbolUniverse, scope: CanonicalScope, firstType: CanonicalType, secondType: CanonicalType): CanonicalType = (firstType, secondType) match {
      case (CanonicalObjType(x, _, _, _), CanonicalObjType(y, _, _, _)) if x != y || scope.contains(x) =>
        val z = su.newSymbol()
        canonicalGreatestCommonSubtype(su, scope,
          canonicalTypeRenameBoundVar(z, firstType),
          canonicalTypeRenameBoundVar(z, secondType))
      case (CanonicalObjType(x, xFields, xTypes, xProjs), CanonicalObjType(y, yFields, yTypes, yProjs)) if x == y =>
        // TODO do we need to add x to scope?
        val fields = mapUnion(xFields, yFields){(left, right) =>
          canonicalGreatestCommonSubtype(su, scope, left, right)
        }

        val types = mapUnion(xTypes, yTypes){(left, right) =>
          (left, right) match {
            case ((leftLower, leftUpper), (rightLower, rightUpper)) =>
              (canonicalLeastCommonSupertype(su, scope, leftLower, rightLower),
               canonicalGreatestCommonSubtype(su, scope, leftUpper, rightUpper))
          }
        }

        val projs = xProjs ++ yProjs // TODO what if yProjs refer to x?

        CanonicalObjType(x, fields, types, projs)
      case (CanonicalFunType(x, xType, xResType, xProjs), CanonicalFunType(y, yType, yResType, yProjs)) if x != y =>
        val z = su.newSymbol()
        canonicalGreatestCommonSubtype(su, scope,
          canonicalTypeRenameBoundVar(z, firstType),
          canonicalTypeRenameBoundVar(z, secondType))
      case (CanonicalFunType(x, xType, firstResType, xProjs), CanonicalFunType(y, yType, secondResType, yProjs)) if x == y =>
        val commonArgType = canonicalLeastCommonSupertype(su, scope, xType, yType) // TODO is lub correct here? probably since glb of res can't refer to something that only exists in one of xType/yType. maybe have to double check in commonResType?
        val commonResType = canonicalGreatestCommonSubtype(su, scope + (x -> commonArgType), firstResType, secondResType)
        val projs = xProjs ++ yProjs // TODO what if yProjs refer to x?
        CanonicalFunType(x, commonArgType, commonResType, projs)
      case (CanonicalTop, _) => secondType
      case (_, CanonicalTop) => firstType
      case (CanonicalBot, _) => firstType
      case (_, CanonicalBot) => secondType
      case _ => ???;  CanonicalErrorType // TODO
    }

    // TODO
    def canonicalize(su: SymbolUniverse, scope: CanonicalScope, z: Symbol, zType: Type): CanonicalType = zType match {
      case RecType(x, xType) =>
        canonicalize(su, scope, z, typeRenameVar(x, z, xType))
      case AndType(left, right) =>
        val canonicalLeft = canonicalize(su, scope, z, left)
        val canonicalRight = canonicalize(su, scope, z, right)
        val commonType = canonicalGreatestCommonSubtype(su, scope, canonicalLeft, canonicalRight)

        canonicalTypeRenameBoundVar(z, commonType)
      case proj @ TypeProj(x, a) =>
        // TODO check scope.contains(x)?
        CanonicalObjType(z, Map(), Map(), Set(proj))
      case decl @ FieldDecl(a, aType) =>
        val aCanonicalType = canonicalize(su, scope, su.newSymbol(), aType)
        CanonicalObjType(z, Map(a -> aCanonicalType), Map(), Set())
      case decl @ TypeDecl(a, aLowerType, aUpperType) =>
        val aLowerCanonicalType = canonicalize(su, scope, su.newSymbol(), aLowerType)
        val aUpperCanonicalType = canonicalize(su, scope, su.newSymbol(), aUpperType)
        CanonicalObjType(z, Map(), Map(a -> (aLowerCanonicalType, aUpperCanonicalType)), Set())
      case FunType(x, _, _) if scope.contains(x) =>
        canonicalize(su, scope, z, renameTypeBoundVar(su.newSymbol(), zType))
      case FunType(x, xType, resType) if !scope.contains(x) =>
        val canonicalXType = canonicalize(su, scope, su.newSymbol(), xType) // TODO vs using "x"?
        val y = su.newSymbol()
        val canonicalResType = canonicalize(su, scope + (x -> canonicalXType), y, resType)
        CanonicalFunType(y, canonicalXType, canonicalResType, Set())
      case Top => CanonicalTop
      case Bot => CanonicalBot
      case Que => CanonicalQue
      case _ =>
        ??? ; CanonicalErrorType // TODO
    }

    def freeVarsInCanonicalType(typ: CanonicalType): Set[Symbol] = typ match {
      case CanonicalFunType(x, xType, resType, projs) =>
        (freeVarsInCanonicalType(xType)
          ++ freeVarsInCanonicalType(resType)
          ++ projs.map{case TypeProj(y, _) => y}).toSet - x
      case CanonicalObjType(x, fields, types, projs) =>
        (fields.values.flatMap{freeVarsInCanonicalType}
          ++ types.values.flatMap{case (a,b) => Seq(a,b)}.flatMap{freeVarsInCanonicalType}
          ++ projs.map{case TypeProj(y, _) => y}).toSet - x
      case _ => Set() // Bot, Top, Que
    }

    def isVarFreeIn(z: Symbol, t: Tree): Boolean = t match {
      case expr: Expr => isVarFreeInExpr(z, expr)
      case typ: Type => isVarFreeInType(z, typ)
      case typ: CanonicalType => isVarFreeInCanonicalType(z, typ)
    }

    def isVarFreeInCanonicalType(z: Symbol, typ: CanonicalType): Boolean = typ match {
      case CanonicalFunType(x, xType, resType, projs) if x != z =>
        (isVarFreeInCanonicalType(z, xType)
          || isVarFreeInCanonicalType(z, resType)
          || projs.map{case TypeProj(y, a) => (y == z)}.fold(false){_ || _})
      case CanonicalObjType(x, fields, types, projs) if x != z =>
        (fields.values.map{isVarFreeInCanonicalType(z, _)}.fold(false){_ || _}
          || types.values.flatMap{case (a,b) => Seq(a,b)}.map{isVarFreeInCanonicalType(z, _)}.fold(false){_ || _}
          || projs.map{case TypeProj(y, a) => (y == z)}.fold(false){_ || _})
      case _ => false // Bot, Top, Que
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

    def stringExprWithType(expr: Expr): String = {
      val s = expr match {
        case Let(x, xTerm, resTerm) => s"Let($x, ${stringExprWithType(xTerm)}, ${stringExprWithType(resTerm)})"
        case Obj(x, xType, d)       => s"Obj($x, $xType, ${stringExprWithType(d)})"
        case Fun(x, xType, resTerm) => s"Fun($x, $xType, ${stringExprWithType(resTerm)})"
        case FieldDef(a, aTerm)     => s"FieldDef($a, ${stringExprWithType(aTerm)})"
        case AndDef(left, right)    => s"AndDef(${stringExprWithType(left)}, ${stringExprWithType(right)})"
        case _ => s"$expr"
      }
      s"$s.withTypeOption(${expr.assignedType})"
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
      expr.assignedType match {
        case Some(typ) =>
          s"$s.withType($typ)"
        case None =>
          s
      }
    }

    def alphaRenameType(su: SymbolUniverse, typ: Type): Type = typ match {
      case FunType(x, xType, resType)          => renameTypeBoundVar(su.newSymbol(), FunType(x, alphaRenameType(su, xType), alphaRenameType(su, resType)))
      case RecType(x, xType)                   => renameTypeBoundVar(su.newSymbol(), RecType(x, alphaRenameType(su, xType)))
      case FieldDecl(a, aType)                 => FieldDecl(a, alphaRenameType(su, aType))
      case TypeDecl(a, aLowerType, aUpperType) => TypeDecl(a, alphaRenameType(su, aLowerType), alphaRenameType(su, aUpperType))
      case AndType(left, right)                => andType(alphaRenameType(su, left), alphaRenameType(su, right))
      case _ => typ
    }

    def alphaRenameDef(su: SymbolUniverse, d: Def): Def = d match {
      case FieldDef(a, aTerm)  => FieldDef(a, alphaRenameTerm(su, aTerm))
      case AndDef(left, right) => AndDef(alphaRenameDef(su, left), alphaRenameDef(su, right))
      case TypeDef(a, aType)   => TypeDef(a, alphaRenameType(su, aType))
      case _ => d
    }

    def alphaRenameTerm(su: SymbolUniverse, term: Term): Term = term match {
      case Let(x, xTerm, resTerm) => renameTermBoundVar(su.newSymbol(), Let(x, alphaRenameTerm(su, xTerm), alphaRenameTerm(su, resTerm)))
      case Obj(x, xType, d)       => renameTermBoundVar(su.newSymbol(), Obj(x, alphaRenameType(su, xType), alphaRenameDef(su, d)))
      case Fun(x, xType, resTerm) => renameTermBoundVar(su.newSymbol(), Fun(x, alphaRenameType(su, xType), alphaRenameTerm(su, resTerm)))
      case _ => term
    }

    def eliminateVarUp(su: SymbolUniverse, scope: CanonicalScope, z: Symbol, typ: CanonicalType, visited: Set[TypeProj]): CanonicalType = typ match {
      case CanonicalObjType(x, xFields, xTypes, xProjs) if x != z =>
        ???
//        val localScope = scope + (x -> typ)
//
//        val fields = xFields.map{case (a, aType) =>
//          a -> eliminateVarUp(su, localScope, z, aType, visited)
//        }
//        val types = xTypes.map{case (a, (aLowerType, aUpperType)) =>
//          a -> (eliminateVarDown(su, localScope, z, aLowerType, visited)},
//                eliminateVarUp(su, localScope, z, aUpperType, visited)})
//        }
//
//        val projsToRaise = projs.filter{case TypeProj(y, a) => (y == z)}
//        val keptProjs = projs -- projsToRaise
//
//        val raised = projsToRaise.map{
//          case proj @ TypeProj(y, a) =>
//            if (visited(proj))
//              CanonicalBot // Note that this is equivalent to removing the projection (with lub, below).
//            else
//              eliminateVarUp(su, scope, z, typeProjectUpper(scope, y, a).getOrElse(CanonicalBot), visited + proj)
//        }
//        // TODO bot+lub is probably wrong here.... e.g. what if there is only
//        // one proj? then res should be Top, NOT Bot.
//
//        val withoutRaisedProjs = CanonicalFunType(x, newXType, newResType, keptProjs)
//        val withRaisedProjs = raised.fold(withoutRaisedProjs){canonicalLeastCommonSupertype(su, scope, _, _)} // TODO is lub correct here?
//        withRaisedProjs
      case CanonicalFunType(x, xType, resType, projs) if x != z =>
        val newXType = eliminateVarDown(su, scope, x, xType, visited)
        val newResType = eliminateVarUp(su, scope + (x -> xType), x, resType, visited)
        /** TODO we may need to eliminate x from resType?
         * y.T: {R: A..D}..{R: B..C}
         * fun(x: y.T): x.R
         * eliminate y.
         * fun(x: {R: B..C}): x.R
         *
         * -------------
         *
         * y.T: {R: A..D}..top
         * fun(x: y.T): x.R
         * assertion: for x.R to work, x <: {R: A..D}, which it is not since {R: A..D} <: y.T <: top, i.e. {R: A..D} <: x, not the other way around.
         * TODO: but we can still construct types like that, so special-case? -- No, let the part of the code that tries to use x.R handle that.
         *
         * -------------
         *
         * y.T: {R: A..D}..y.T
         * equivalent to: y.T: {R: A..D}..{R: A..D}
         *
         * -------------
         *
         * y.T: y.T..y.T
         * equivalent to: y.T: Bot..Top
         *
         * -------------
         *
         * y.D: y.T..y.T
         * y.T: y.D..y.D
         * equivalent to:
         * y.D: Bot..Top
         * y.T: y.D..y.D
         * or:
         * y.D: y.D..y.D
         * y.T: Bot..Top
         *
         * -------------
         *
         * y.D: y.D..y.T
         * y.T: y.T..y.D
         * equal to: // ???
         * y.D: Bot..y.T
         * y.T: Bot..y.D
         *
         * -------------
         *
         * y.T: {R: A..D}..Top
         * fun(x: fun(y.T, y.T)): fun(y.T, y.T)
         * eliminate y.
         * fun(x: fun(Bot, Top)): fun(A, D) ???
         *
         * So if x is raised to something that is nolonger a object (top),
         * then x has to be eliminated from res.
         *
         */

        /** TODO (x -> xType) vs (x -> newXType) ??
         *
         * Does not matter since we are not eliminating on x? newResType will never look up x?
         * What about glb? It does not lookup x either (only does unions on proj-sets).
         *
         * (However: when in parallel it is better to use xType, such that resType does
         * not have to wait for newXType to compute.)
         *
         */

        val projsToRaise = projs.filter{case TypeProj(y, a) => (y == z)}
        val keptProjs = projs -- projsToRaise

        val raised = projsToRaise.map{
          case proj @ TypeProj(y, a) =>
            if (visited(proj))
              CanonicalTop // Note that this is equivalent to removing the projection (with glb, below).
            else
              eliminateVarUp(su, scope, z, typeProjectUpper(scope, y, a).getOrElse(CanonicalTop), visited + proj)
            // x.T: bot..x.D
            // x.D: A..top
            // then x.T should be replaced with Top, and NOT A, since:
            // x.T is a subtype of SOME x.D, not necesarily A.
            // e.g. x.T <: D, A <: D <: Top
        }

        // A & B & C
        // becomes:
        // upper(A) & upper(B) & upper(C)
        // TODO double-, triple- and quadruple-check this.
        // TODO hmmm... whenever someone uses a function, don't they have to expand to a supertype?
        //
        //
        // The least restrictive function is the lowest one. Therefore glb
        // should make sense here....
        /** TODO Is lib correct here?
         *
         * f: A => B
         * g: C => D
         *
         * (f & g)   ===   glb(f, g) ???
         *
         * ----
         *
         * ((A => B) & (C => D)) <: lub(A, C) => glb(B, D)  ???
         *
         * (A => B) <: lub(A, C) => glb(B, D)   ???
         * lub(A, C) <: A   (NO)
         * B <: glb(B, D)   (NO)
         *
         * (C => D) <: lub(A, C) => glb(B, D)   ???
         * ... (NO)
         *
         * ----
         *
         * (lub(A, C)) => (glb(B, D))  <: ((A => B) & (C => D))   ???
         *
         * (lub(A, C)) => (glb(B, D))  <: (A => B)   ???
         * A <: lub(A, C)        (true)
         * glb(B, D) <: B        (true)
         *
         * (lub(A, C)) => (glb(B, D))  <: (C => D)   ???
         * C <: (lub(A, C))        (true)
         * (glb(B, D)) <: D        (true)
         *
         * ---
         *
         * glb(f, g) <: (f & g) <: lub(f, g)   ???
         *
         * ---
         * f <: f'
         * g <: g'
         *
         * f & g <: f' & g' ???
         *
         * f & g <: f' ???
         * f & g <: f <: f'    (yes)
         *
         * f & g <: g' ???
         * f & g <: g <: g'    (yes)
         *
         * ---
         * f <: f'
         *
         * f & g <: f' ???
         * f & g <: f <: f' (yes)
         *
         *
         *
         * TODO CanonicalFunType can probably not represent all funtypes in
         * DOT. Need AndTypes...
         */

        val withoutRaisedProjs = CanonicalFunType(x, newXType, newResType, keptProjs)
        val withRaisedProjs = raised.fold(withoutRaisedProjs){canonicalGreatestCommonSubtype(su, scope, _, _)} // TODO is lub correct here?
        withRaisedProjs
      case _ =>
        typ
    }

    def eliminateVarDown(su: SymbolUniverse, scope: CanonicalScope, z: Symbol, typ: CanonicalType, visited: Set[TypeProj]): CanonicalType = typ match {
      case CanonicalObjType(x, xFields, xTypes, xProjs) if x != z =>
        ??? // TODO
      case CanonicalFunType(x, xType, resType, projs) if x != z =>
        val newXType = eliminateVarUp(su, scope, x, xType, visited)
        val newResType = eliminateVarDown(su, scope + (x -> xType), x, resType, visited)
        /** TODO we may need to eliminate x from resType?
         * y.T: {R: A..D}..{R: B..C}
         * fun(x: y.T): x.R
         * eliminate y down.
         * fun(x: {R: B..C}): x.R
         *
         * ------------
         *
         * y.T: Bot..{R: B..C}
         * fun(x: y.T): x.R
         * eliminate y down.
         * fun(x: {R: B..C}): x.R
         * But probably not a minimal elimination...
         *
         * So it should be fine to change the type of x, as long as the change
         * abides subtyping-constraints.
         *
         * ------------
         */

        /** TODO Is it fine to pass visited as-is between eliminateVarUp and eliminateVarDown? YES, e.g.:
         * y.T: fun(y.T, y.T)..R
         * eliminate y down.
         * fun(y.T, y.T)
         * eliminate y down.
         * fun(Top, Bot)
         */

        val projsToLower = projs.filter{case TypeProj(y, a) => (y == z)}
        val keptProjs = projs -- projsToLower

        val lowered = projsToLower.map{
          case proj @ TypeProj(y, a) =>
            if (visited(proj))
              CanonicalTop
            else
              eliminateVarDown(su, scope, z, typeProjectLower(scope, y, a).getOrElse(CanonicalTop), visited + proj)
        }

        val withoutLoweredProjs = CanonicalFunType(x, newXType, newResType, keptProjs)
        val withLoweredProjs = lowered.fold[CanonicalType](withoutLoweredProjs){canonicalGreatestCommonSubtype(su, scope, _, _)}  // TODO is glb correct here?
        withLoweredProjs
      case _ =>
        typ
    }

  }


  //case class   OrType(left: Type, right: Type)                       extends Type // TODO REM.

  // extra Term ::=
  //case class LightApp(fun: Term, args: List[List[Term]])                      extends Term
  //case class PolyApp(fun: Term, typeArgs: List[Type], args: List[List[Term]]) extends Term
  //case class TermSel(e: Term, a: Symbol)                                      extends Term

  // extra Value ::=
  //case class LightObj(self: Symbol, d: Def)                                                 extends Value // TODO gives both classtype and constructor type?
  //case class LightFun(params: List[List[Symbol]], res: Term)                                extends Value
  //case class PolyFun(typeParams: List[TypeVar], params: List[List[(Symbol, Type)]], res: Term) extends Value
  // TODO LightObjWithParents
  // TODO "class"-value

  // extra Type ::=
  //case class PolyFunType(typeParams: List[TypeVar], params: List[List[(Symbol, Type)]], resType: Type) extends Type // TODO vs encode both as object-arg?
  //case class TypeVar(a: Symbol) extends Type
  // TODO path-type?

  type Prototype = Type
  case object Que extends Prototype {
    val treeHeight = 1
    val totNumNodes = 1
  }

  case object ErrorType extends Type { // TODO vs some other errorhandling?
    val treeHeight = 1
    val totNumNodes = 1
  }
  case class FutureType(cell: Meh[Type]) extends Type {
    val treeHeight = 1
    val totNumNodes = 1
    // TODO approximations
  }


  // TODO typed terms. {var mytpe} like in Dotty? vs cell?

  type Scope = Map[Symbol, Type]
  type TypeSubst = Map[Symbol, Type]
  type CanonicalScope = Map[Symbol, CanonicalType]

  type Meh[T] = Cell[DefaultKey[T], T]
  type Geh[T] = CellCompleter[DefaultKey[T], T]

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
    val counter = new AtomicInteger(init)
    def newSymbol(): Int = counter.getAndIncrement()
  }

  def await[T](awaitable: Awaitable[T]): T =
    Await.result(awaitable, Duration.Inf)

  def newCellCompleter[V](pool: HandlerPool, lattice: Lattice[V]): Geh[V] = {
    CellCompleter[DefaultKey[V], V](pool, new DefaultKey[V])(lattice)
  }

  def onComplete[T](cell: Meh[T])(f: T => Unit): Unit = {
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

    def contCell[T](lattice: Lattice[T])(f: ((T) => Unit) => Unit): Meh[T] = {
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

    def contZipFork[A >: Null, B >: Null](a: ((A) => Unit) => Unit, b: ((B) => Unit) => Unit)(cont: (A, B) => Unit): Unit = { // TODO test
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


    def canonicalError() = {error() ; CanonicalErrorType}


    def canonicalize(su: SymbolUniverse, scope: CanonicalScope, z: Symbol, zType: Type): CanonicalFuture = {
        val zCompleter = newCellCompleter(pool, Lattice.trivial[CanonicalType])
        val zCanonicalType = CanonicalFuture(zCompleter.cell)
        launch {
          canonicalType(su, scope + (z -> zCanonicalType), z, zType) {
            zCompleter.putFinal(_)
          }
        }
        zCanonicalType
    }

    // TODO visitedProjs-param unnecessary?
    def canonicalType(su: SymbolUniverse, scope: CanonicalScope, z: Symbol, zType: Type)(cont: CanonicalType => Unit): Unit = zType match {
      case RecType(x, xType) =>
        renameToUniqueVar(x, z, xType){
          canonicalType(su, scope, z, _)(cont)
        }
      case AndType(left, right) =>
        val leftCell = contFuture[CanonicalType] {
          canonicalType(su, scope, z, left)(_)
        }
        val rightCell = contFuture[CanonicalType] {
          canonicalType(su, scope, z, right)(_)
        }
        onComplete(leftCell.zipFinal(rightCell)) {case (leftType, rightType) =>
          canonicalGreatestCommonSubtype(scope, leftType, rightType){
            expandCanonicalFutureOnce(_) {
              case CanonicalObjType(x, fields, types, projs) =>
                canonicalRenameToUniqueVar(x, z, CanonicalObjType(z, fields, types, projs))(cont)
              case t =>
                cont(t)
            }
          }
        }
      case proj @ TypeProj(x, a) if scope.contains(x) =>
        cont(CanonicalObjType(z, Map(), Map(), Set(proj)))
      case decl @ FieldDecl(a, aType) =>
        val aCanonicalType = canonicalize(su, scope, su.newSymbol(), aType)
        cont(CanonicalObjType(z, Map(a -> aCanonicalType), Map(), Set()))
      case decl @ TypeDecl(a, aLowerType, aUpperType) =>
        val aLowerCanonicalType = canonicalize(su, scope, su.newSymbol(), aLowerType)
        val aUpperCanonicalType = canonicalize(su, scope, su.newSymbol(), aUpperType)
        cont(CanonicalObjType(z, Map(), Map(a -> (aLowerCanonicalType, aUpperCanonicalType)), Set()))
      case FutureType(cell) =>
        onComplete(cell){
          canonicalType(su, scope, z, _)(cont)
        }
      case FunType(x, xType, resType) =>
        val y = symbolUniverse.newSymbol()
        val canonicalXType = canonicalize(su, scope, su.newSymbol(), xType)
        val canonicalResType = CanonicalFuture(contFuture{c =>
          renameToUniqueVar(x, y, resType){
            canonicalType(su, scope, y, _)(c)
          }
        })
        cont(CanonicalFunType(y, canonicalXType, canonicalResType, Set()))
      case Top => cont(CanonicalTop)
      case Bot => cont(CanonicalBot)
      case Que => cont(CanonicalQue)
      case _ => cont(canonicalError())
    }

    def canonicalFuture(f: (CanonicalType => Unit) => Unit) = CanonicalFuture(contFuture(f))

    def typeProject(scope: CanonicalScope, x: Symbol, a: Symbol)(cont: (CanonicalType, CanonicalType) => Unit): Unit = scope.get(x) match {
      case Some(xType) =>
        expandCanonicalFutureOnce(xType) {
          case CanonicalObjType(y, _, yTypes, _) =>
            yTypes.get(a) match {
              case Some((aLowerType, aUpperType)) =>
                cont(aLowerType, aUpperType)
              case None =>
                cont(canonicalError(), canonicalError())
            }
          case _ =>
            cont(canonicalError(), canonicalError())
        }
      case None =>
        cont(canonicalError(), canonicalError())
    }

    def typeProjectUpper(scope: CanonicalScope, x: Symbol, a: Symbol)(cont: CanonicalType => Unit): Unit = {
      typeProject(scope, x, a){(_, aUpperType) => cont(aUpperType)}
    }
    def typeProjectLower(scope: CanonicalScope, x: Symbol, a: Symbol)(cont: CanonicalType => Unit): Unit = {
      typeProject(scope, x, a){(aLowerType, _) => cont(aLowerType)}
    }

    def upperTypeProjections(scope: CanonicalScope, t: CanonicalType, visitedProjs: Set[TypeProj] = Set())(cont: (Set[TypeProj]) => Unit): Unit = t match {
      case CanonicalObjType(x, _, _, xProjs) =>
        val localScope = scope + (x -> t)
        val projUpperBounds = xProjs.map{proj =>
          {(c: (Set[TypeProj]) => Unit) =>
            proj match {
              case proj @ TypeProj(y, a) if !visitedProjs.contains(proj) =>
                typeProjectUpper(localScope, y, a) {
                  upperTypeProjections(scope, _, visitedProjs + proj)(c)
                }
              case _ =>
                c(Set())
            }
          }
        }.toList
        contFold[Set[TypeProj]]({c => c(xProjs)}, projUpperBounds){(a, b) => {c => a ++ b}}(cont)
      case _ : FunType =>
        canonicalError()
        cont(Set())
      case _ =>
        cont(Set())
    }

    def lowerTypeProjections(scope: CanonicalScope, t: CanonicalType, visitedProjs: Set[TypeProj] = Set())(cont: (Set[TypeProj]) => Unit): Unit = t match {
      case CanonicalObjType(x, _, _, xProjs) =>
        val localScope = scope + (x -> t)
        val projUpperBounds = xProjs.map{proj =>
          {(c: (Set[TypeProj]) => Unit) =>
            proj match {
              case proj @ TypeProj(y, a) if !visitedProjs.contains(proj) =>
                typeProjectLower(localScope, y, a) {
                  lowerTypeProjections(scope, _, visitedProjs + proj)(c)
                }
              case _ =>
                c(Set())
            }
          }
        }.toList
        contFold[Set[TypeProj]]({c => c(xProjs)}, projUpperBounds){(a, b) => {c => a ++ b}}(cont)
      case _ : FunType =>
        canonicalError()
        cont(Set())
      case _ =>
        cont(Set())
    }


    def canonicalRaise(scope: CanonicalScope, t: CanonicalType, p: CanonicalType)(cont: CanonicalType => Unit): Unit = {
      //expandCanonicalTypeFutures(t){t =>
      //  expandCanonicalTypeFutures(p){p =>
      //    //println(s"canonicalRaise($scope, $t, $p)")
          canonicalRaise2(scope, t, p) { res =>
      //      expandCanonicalTypeFutures(res){res =>
      //        println(s"canonicalRaise($scope, $t, $p) = $res")
              cont(res)
            }
      //    }
      //  }
      //}
    }

    def canonicalRaise2(scope: CanonicalScope, t: CanonicalType, p: CanonicalType)(cont: CanonicalType => Unit): Unit = (t, p) match {
      case (t @ CanonicalObjType(x, xFields, xTypes, xProjs), p @ CanonicalObjType(y, yFields, yTypes, yProjs)) =>
        val z = symbolUniverse.newSymbol()
        val zCompleter = newCellCompleter(pool, Lattice.trivial[CanonicalType])
        val zType = CanonicalFuture(zCompleter.cell)

        // TODO point y to new skeletal object
        canonicalRenameToUniqueVar(y, z, CanonicalObjType(z, yFields, yTypes, yProjs)){
          case CanonicalObjType(_, yFields, yTypes, yProjs) =>
            val localScope = Map(x -> t, z -> zType)
            val xIsMissingFields = yFields.exists{case (a, _) => !xFields.contains(a)}
            val xIsMissingTypes = yTypes.exists{case (a, _) => !xTypes.contains(a)}
            if (xIsMissingFields || xIsMissingTypes) {
              cont(canonicalError())
            } else {
              // TODO okay to add TypeDecls when raising, but not okay to add FieldDecls? probably better to do that specially in another function.
              val fields = yFields.map{case (a, aPrototype) =>
                xFields.get(a) match {
                  case Some(aType) => a -> canonicalFuture{canonicalRaise(localScope, aType, aPrototype)(_)}
                  case None => (a, canonicalError())
                }
              }.toMap
              val types = yTypes.map{case (a, (aLowerPrototype, aUpperPrototype)) =>
                xTypes.get(a) match {
                  case Some((aLowerType, aUpperType)) =>
                    a -> (canonicalFuture{canonicalLower(localScope, aLowerType, aLowerPrototype)(_)},
                          canonicalFuture{canonicalRaise(localScope, aUpperType, aUpperPrototype)(_)})
                  case None => a -> (canonicalError(), canonicalError())
                }
              }.toMap

              upperTypeProjections(scope, t){upperProjs =>
                val missingProjs = yProjs -- upperProjs.map{
                  case TypeProj(w, a) if w == x => TypeProj(z, a)
                  case u => u
                }
                missingProjs.foreach{case TypeProj(r, a) =>
                  typeProjectLower(localScope, r, a){
                    ???
                  }
                }
              }


              // continue while checking projs in background.
              val projs = yProjs

              zCompleter.putFinal(CanonicalObjType(y, fields, types, projs))
              cont(zType)
            }
          case _ =>
            ???
        }
      case (CanonicalFunType(x, xType, xResType, xProjs), CanonicalFunType(y, yType, yResType, yProjs)) if x != y =>
        val z = symbolUniverse.newSymbol()
        val newXResType = CanonicalFuture(contFuture{ canonicalRenameToUniqueVar(x, z, xResType)(_) })
        val newYResType = CanonicalFuture(contFuture{ canonicalRenameToUniqueVar(y, z, yResType)(_) })
        canonicalRaise(scope, CanonicalFunType(z, xType, newXResType, xProjs), CanonicalFunType(z, yType, newYResType, yProjs))(cont)
      case (CanonicalFunType(x, xType, xResType, xProjs), CanonicalFunType(y, yType, yResType, yProjs)) if x == y =>
        // TODO projs
        canonicalLower(scope, xType, yType){argType =>
          canonicalRaise(scope + (x -> argType), xResType, yResType){resType =>
            val projs = yProjs
            cont(CanonicalFunType(x, argType, resType, projs))
          }
        }
      case (_, CanonicalQue) => cont(t)
      case (_, CanonicalTop) => cont(p)
      case (CanonicalBot, CanonicalBot) => cont(p)
      case (CanonicalFuture(cell), _) =>
        onComplete(cell) {
          canonicalRaise(scope, _, p)(cont)
        }
      case (_, CanonicalFuture(cell)) =>
        onComplete(cell) {
          canonicalRaise(scope, t, _)(cont)
        }
      case (CanonicalBot, CanonicalFunType(x, _, _, _)) =>
        canonicalRaise(scope, CanonicalFunType(x, CanonicalTop, CanonicalBot, Set()), p)(cont)
      case (CanonicalBot, CanonicalObjType(x, xFields, xTypes, xProjs)) =>
        //val fields = xFields.mapValues{_ => CanonicalBot}
        //val types = ??? // (Que,Que) --> (Bot,Top)? (Top, Bot)? forbid (Que,Que)?
        //canonicalRaise(CanonicalObjType(x, fields, types, Set()), p)(cont)
        ??? // TODO
      case _ => // TODO
        println(s"canonicalRaise(_, $t, $p) fail")
        cont(canonicalError())
    }

    def canonicalLower(scope: CanonicalScope, t: CanonicalType, p: CanonicalType)(cont: CanonicalType => Unit): Unit = (t, p) match {
      case (t @ CanonicalObjType(x, xFields, xTypes, xProjs), p @ CanonicalObjType(y, yFields, yTypes, yProjs)) =>
        val z = symbolUniverse.newSymbol()
        val zCompleter = newCellCompleter(pool, Lattice.trivial[CanonicalType])
        val zType = CanonicalFuture(zCompleter.cell)

        // TODO point y to new skeletal object
        canonicalRenameToUniqueVar(y, z, CanonicalObjType(z, yFields, yTypes, yProjs)){
          case CanonicalObjType(_, yFields, yTypes, yProjs) =>
            val localScope = Map(x -> t, z -> zType)


            // Note: Subtyping a object means ADDING decls. We are no allowed
            // to remove decls. Therefore y must have all decls, and may also have
            // some decls that x does not have yet.
            val yIsMissingFields = xFields.keys.exists{!yFields.contains(_)}
            val yIsMissingTypes = xTypes.keys.exists{!yTypes.contains(_)}
            // TODO check if the missing decls are in typeprojections.
            if (yIsMissingFields || yIsMissingTypes) {
              cont(canonicalError())
            } else {
              val loweredFields = xFields.map{case (a, aType) =>
                yFields.get(a) match {
                  case Some(aPrototype) => a -> canonicalFuture{canonicalLower(localScope, aType, aPrototype)(_)}
                  case None => (a, canonicalError())
                }
              }.toMap
              val loweredTypes = xTypes.map{case (a, (aLowerType, aUpperType)) =>
                yTypes.get(a) match {
                  case Some((aLowerPrototype, aUpperPrototype)) =>
                    a -> (canonicalFuture{canonicalRaise(localScope, aLowerType, aLowerPrototype)(_)},
                          canonicalFuture{canonicalLower(localScope, aUpperType, aUpperPrototype)(_)})
                  case None => a -> (canonicalError(), canonicalError())
                }
              }.toMap


              val newFields = (yFields -- xFields.keys).map{case (a, aPrototype) =>
                a -> canonicalFuture{
                  canonicalLower(localScope, CanonicalTop, aPrototype)(_)
                }
              }
              val newTypes = (yTypes -- xTypes.keys).map{case (a, (aLowerPrototype, aUpperPrototype)) =>
                a -> (canonicalFuture{
                  canonicalRaise(localScope, CanonicalBot, aLowerPrototype)(_)
                }, canonicalFuture{
                  canonicalLower(localScope, CanonicalTop, aUpperPrototype)(_)
                })
              }

              val fields = loweredFields ++ newFields
              val types = loweredTypes ++ newTypes

              lowerTypeProjections(scope, zType){lowerProjs =>
                val missingProjs = yProjs -- lowerProjs.map{
                  case TypeProj(w, a) if w == x => TypeProj(z, a)
                  case u => u
                }
                missingProjs.foreach{case TypeProj(r, a) =>
                  typeProjectUpper(localScope, r, a){
                    ??? // TODO
                  }
                }
              }

              // continue while checking projs in background.
              val projs = yProjs

              zCompleter.putFinal(CanonicalObjType(y, fields, types, projs))
              cont(zType)
            }
          case _ =>
            ???
        }
      case (CanonicalFunType(x, xType, xResType, xProjs), CanonicalFunType(y, yType, yResType, yProjs)) if x != y =>
        val z = symbolUniverse.newSymbol()
        val newXResType = CanonicalFuture(contFuture{ canonicalRenameToUniqueVar(x, z, xResType)(_) })
        val newYResType = CanonicalFuture(contFuture{ canonicalRenameToUniqueVar(y, z, yResType)(_) })
        canonicalLower(scope, CanonicalFunType(z, xType, newXResType, xProjs), CanonicalFunType(z, yType, newYResType, yProjs))(cont)
      case (CanonicalFunType(x, xType, xResType, xProjs), CanonicalFunType(y, yType, yResType, yProjs)) if x == y =>
        canonicalRaise(scope, xType, yType){argType =>
          canonicalLower(scope + (x -> argType), xResType, yResType){resType =>
            val projs = yProjs
            cont(CanonicalFunType(x, argType, resType, projs))
          }
        }
      case (_, CanonicalQue) => cont(t)
      case (_, CanonicalBot) => cont(p)
      case (CanonicalTop, CanonicalTop) => cont(p)
      case (CanonicalFuture(cell), _) =>
        onComplete(cell) {
          canonicalLower(scope, _, p)(cont)
        }
      case (_, CanonicalFuture(cell)) =>
        onComplete(cell) {
          canonicalLower(scope, t, _)(cont)
        }
//      case (CanonicalTop, CanonicalFunType(x, _, _, _)) =>
//        ???
//      case (CanonicalTop, CanonicalObjType(x, xFields, xTypes, xProjs)) =>
//        ???
      case _ =>
        cont(canonicalError())
    }

    def expandCanonicalTypeFutures(t: CanonicalType)(cont: CanonicalType => Unit): Unit = t match {
      case CanonicalObjType(z, fields, types, projs) =>
        val fieldConts = fields.map{case (a, aType) =>
          {(c: (Map[Symbol, CanonicalType]) => Unit) =>
            expandCanonicalTypeFutures(aType){res =>
              c(Map(a -> res))
            }
          }
        }.toList
        val typeConts = types.map{case (a, (aLowerType, aUpperType)) =>
          {(c: (Map[Symbol, (CanonicalType, CanonicalType)]) => Unit) =>
            expandCanonicalTypeFutures(aLowerType) {aLowerType =>
              expandCanonicalTypeFutures(aUpperType) {aUpperType =>
                c(Map(a -> (aLowerType, aUpperType)))
              }
            }
          }
        }.toList
        contFold[Map[Symbol, CanonicalType]](_(Map()), fieldConts){(a,b) => c => c(a ++ b)} {fields =>
          contFold[Map[Symbol, (CanonicalType, CanonicalType)]](_(Map()), typeConts){(a,b) => c => c(a ++ b)} {types =>
            cont(CanonicalObjType(z, fields, types, projs))
          }
        }
      case CanonicalFunType(x, xType, resType, projs) =>
        expandCanonicalTypeFutures(xType) {xType =>
          expandCanonicalTypeFutures(resType) {resType =>
            cont(CanonicalFunType(x, xType, resType, projs))
          }
        }
      case CanonicalFuture(cell) =>
        onComplete(cell){
          expandCanonicalTypeFutures(_)(cont)
        }
      case _ =>
        cont(t)
    }

    def expandCanonicalFutureOnce(t: CanonicalType)(cont: CanonicalType => Unit): Unit = t match {
      case CanonicalFuture(cell) =>
        onComplete(cell) {
          expandCanonicalFutureOnce(_)(cont)
        }
      case _ => cont(t)
    }

    def expandTermFutures(term: Term)(cont: (Term) => Unit): Unit = term match {
      case TermFuture(cell) =>
        onComplete(cell){
          expandTermFutures(_)(cont)
        }
      case _ =>
        def expandAssignedType(t: Term)(c: (Term) => Unit): Unit = {
          t.assignedType match {
            case Some(termType) =>
              expandCanonicalTypeFutures(termType){termType =>
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
                expandAssignedType(Let(x, xTerm, resTerm).withTypeOption(term.assignedType))(cont)
              }
            }
          case Obj(x, xType, body) =>
            expandTypeFutures(xType){xType =>
              expandDefFutures(body){body =>
                expandAssignedType(Obj(x, xType, body).withTypeOption(term.assignedType))(cont)
              }
            }
          case Fun(x, xType, body) =>
            expandTypeFutures(xType){xType =>
              expandTermFutures(body){body =>
                expandAssignedType(Fun(x, xType, body).withTypeOption(term.assignedType))(cont)
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
          d2.assignedType match {
            case Some(termType) =>
              expandCanonicalTypeFutures(termType){termType =>
                c(d2.withType(termType))
              }
            case None =>
              c(d2)
          }
        }
        d match {
          case FieldDef(a, aTerm) =>
            expandTermFutures(aTerm){aTerm =>
              expandAssignedType(FieldDef(a, aTerm).withTypeOption(d.assignedType))(cont)
            }
          case AndDef(left, right) =>
            expandDefFutures(left){left =>
              expandDefFutures(right){right =>
                expandAssignedType(AndDef(left, right).withTypeOption(d.assignedType))(cont)
              }
            }
          case TypeDef(a, aType) =>
            expandTypeFutures(aType){aType =>
              expandAssignedType(TypeDef(a, aType).withTypeOption(d.assignedType))(cont)
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


    def canonicalGreatestCommonSubtype(scope: CanonicalScope, firstType: CanonicalType, secondType: CanonicalType)(cont: CanonicalType => Unit): Unit = (firstType, secondType) match {
      case (CanonicalObjType(x, xFields, xTypes, xProjs), CanonicalObjType(y, yFields, yTypes, yProjs)) if x != y || scope.contains(x) =>
        val z = symbolUniverse.newSymbol()
        canonicalRenameToUniqueVar(x, z, CanonicalObjType(z, xFields, xTypes, xProjs)) {left =>
          canonicalRenameToUniqueVar(y, z, CanonicalObjType(z, yFields, yTypes, yProjs)) {right =>
            canonicalGreatestCommonSubtype(scope, left, right)(cont)
          }
        }
      case (CanonicalObjType(x, xFields, xTypes, xProjs), CanonicalObjType(y, yFields, yTypes, yProjs)) if x == y =>
        val z = x
        val zCompleter = newCellCompleter(pool, Lattice.trivial[CanonicalType])
        val localScope = scope + (z -> CanonicalFuture(zCompleter.cell))

        val fields = mapUnion(xFields, yFields){(left, right) =>
          CanonicalFuture(contFuture{
            canonicalGreatestCommonSubtype(localScope, left, right)(_)
          })
        }

        val types = mapUnion(xTypes, yTypes){(left, right) =>
          (left, right) match {
            case ((leftLower, leftUpper), (rightLower, rightUpper)) =>
              (CanonicalFuture(contFuture{canonicalLeastCommonSupertype(localScope, leftLower, rightLower)(_)}),
               CanonicalFuture(contFuture{canonicalGreatestCommonSubtype(localScope, leftUpper, rightUpper)(_)}))
          }
        }

        val projs = xProjs ++ yProjs

        // TODO vs creating cells for fields and handing them explicit type for zCompleter?
        val zType = CanonicalObjType(z, fields, types, projs)
        zCompleter.putFinal(zType)
        cont(zType)
      case (CanonicalFunType(x, xType, xResType, xProjs), CanonicalFunType(y, yType, yResType, yProjs)) if x != y || scope.contains(x) =>
        val z = symbolUniverse.newSymbol()
        canonicalRenameToUniqueVar(x, z, xResType) {newXResType =>
          canonicalRenameToUniqueVar(y, z, yResType) {newYResType =>
            canonicalGreatestCommonSubtype(scope, CanonicalFunType(z, xType, newXResType, xProjs), CanonicalFunType(z, yType, newYResType, yProjs))(cont)
          }
        }
      case (CanonicalFunType(x, xType, firstResType, xProjs), CanonicalFunType(y, yType, secondResType, yProjs)) if x == y =>
        val z = x
        canonicalLeastCommonSupertype(scope, xType, yType) {commonArgType => // TODO is lub correct here? probably since glb of res can't refer to something that only exists in one of xType/yType. maybe have to double check in commonResType?
          canonicalGreatestCommonSubtype(scope + (z -> commonArgType), firstResType, secondResType) {commonResType =>
            val projs = xProjs ++ yProjs
            cont(CanonicalFunType(z, commonArgType, commonResType, projs))
          }
        }
      case (CanonicalTop, _) => cont(secondType)
      case (_, CanonicalTop) => cont(firstType)
      case (CanonicalBot, _) => cont(firstType)
      case (_, CanonicalBot) => cont(secondType)
      case (CanonicalFuture(cell), _) =>
        onComplete(cell){
          canonicalGreatestCommonSubtype(scope, _, secondType)(cont)
        }
      case (_, CanonicalFuture(cell)) =>
        onComplete(cell){
          canonicalGreatestCommonSubtype(scope, firstType, _)(cont)
        }
    }

    def canonicalLeastCommonSupertype(scope: CanonicalScope, firstType: CanonicalType, secondType: CanonicalType)(cont: CanonicalType => Unit): Unit = (firstType, secondType) match {
      case (CanonicalObjType(x, xFields, xTypes, xProjs), CanonicalObjType(y, yFields, yTypes, yProjs)) if x != y || scope.contains(x) =>
        val z = symbolUniverse.newSymbol()
        canonicalRenameToUniqueVar(x, z, CanonicalObjType(z, xFields, xTypes, xProjs)) {left =>
          canonicalRenameToUniqueVar(y, z, CanonicalObjType(z, yFields, yTypes, yProjs)) {right =>
            canonicalLeastCommonSupertype(scope, left, right)(cont)
          }
        }
      case (CanonicalObjType(x, xFields, xTypes, xProjs), CanonicalObjType(y, yFields, yTypes, yProjs)) if x == y =>
        val z = x
        val zCompleter = newCellCompleter(pool, Lattice.trivial[CanonicalType])
        val localScope = scope + (z -> CanonicalFuture(zCompleter.cell))

        val fields = mapIntersect(xFields, yFields){(left, right) =>
          CanonicalFuture(contFuture{
            canonicalGreatestCommonSubtype(localScope, left, right)(_)
          })
        }

        val types = mapIntersect(xTypes, yTypes){(left, right) =>
          (left, right) match {
            case ((leftLower, leftUpper), (rightLower, rightUpper)) =>
              (CanonicalFuture(contFuture{canonicalLeastCommonSupertype(localScope, leftLower, rightLower)(_)}),
               CanonicalFuture(contFuture{canonicalGreatestCommonSubtype(localScope, leftUpper, rightUpper)(_)}))
          }
        }

        val projs = xProjs.intersect(yProjs)

        // TODO vs creating cells for fields and handing them explicit type for zCompleter?
        val zType = CanonicalObjType(z, fields, types, projs)
        zCompleter.putFinal(zType)
        cont(zType)
      case (CanonicalFunType(x, xType, xResType, xProjs), CanonicalFunType(y, yType, yResType, yProjs)) if x != y || scope.contains(x) =>
        val z = symbolUniverse.newSymbol()
        canonicalRenameToUniqueVar(x, z, xResType) {newXResType =>
          canonicalRenameToUniqueVar(x, z, yResType) {newYResType =>
            canonicalLeastCommonSupertype(scope, CanonicalFunType(x, xType, newXResType, xProjs), CanonicalFunType(y, yType, newYResType, yProjs))(cont)
          }
        }
      case (CanonicalFunType(x, xType, firstResType, xProjs), CanonicalFunType(y, yType, secondResType, yProjs)) if x == y =>
        val z = x
        canonicalGreatestCommonSubtype(scope, xType, yType) {commonArgType =>
          canonicalLeastCommonSupertype(scope + (z -> commonArgType), firstResType, secondResType) {commonResType =>
            val projs = xProjs.intersect(yProjs) // TODO check if stuff outside the intersection should be kept. e.g. if one is a subtype without knowning it
            cont(CanonicalFunType(z, commonArgType, commonResType, projs))
          }
        }
      case (CanonicalTop, _) => cont(firstType)
      case (_, CanonicalTop) => cont(secondType)
      case (CanonicalBot, _) => cont(secondType)
      case (_, CanonicalBot) => cont(firstType)
      case (CanonicalFuture(cell), _) =>
        onComplete(cell){
          canonicalLeastCommonSupertype(scope, _, secondType)(cont)
        }
      case (_, CanonicalFuture(cell)) =>
        onComplete(cell){
          canonicalLeastCommonSupertype(scope, firstType, _)(cont)
        }
    }

    // TODO is perhaps scope-shadowing wrong? TODO barendregt variable convention


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

//    def alphaRenameType(t: Type)(cont: (Type) => Unit): Unit = t match {
//      case FutureType(cell) =>
//        onComplete(cell){typ =>
//          alphaRenameType(typ)(cont)
//        }
//      case FunType(x, xType, resType) =>
//        alphaRenameType(xType){xType =>
//          alphaRenameType(resType){resType =>
//            val z = symbolUniverse.newSymbol()
//            renameToUniqueVar(x, z, FunType(z, xType, resType))(cont)
//          }
//        }
//      case RecType(x, xType) =>
//        alphaRenameType(xType){xType =>
//          val z = symbolUniverse.newSymbol()
//          renameToUniqueVar(x, z, RecType(z, xType))(cont)
//        }
//      case FieldDecl(a, aType) =>
//        alphaRenameType(aType){newAType =>
//          cont(FieldDecl(a, newAType))
//        }
//      case TypeDecl(a, aLowerType, aUpperType) =>
//        val newALowerType = futureType{
//          alphaRenameType(aLowerType)(_)
//        }
//        val newAUpperType = futureType{
//          alphaRenameType(aUpperType)(_)
//        }
//        cont(TypeDecl(a, newALowerType, newAUpperType))
//      case AndType(left, right) =>
//        val newLeft = futureType{
//          alphaRenameType(left)(_)
//        }
//        val newRight = futureType{
//          alphaRenameType(right)(_)
//        }
//        cont(AndType(newLeft, newRight))
//      case _ =>
//        cont(t)
//    }
//
//    def alphaRenameDef(d: Def)(cont: (Def) => Unit): Unit = d match {
//      case DefFuture(cell) =>
//        onComplete(cell){d =>
//          alphaRenameDef(d)(cont)
//        }
//      case FieldDef(a, aTerm) =>
//        alphaRenameTerm(aTerm){aTerm =>
//          cont(FieldDef(a, aTerm))
//        }
//      case TypeDef(a, aType) =>
//        alphaRenameType(aType){aType =>
//          cont(TypeDef(a, aType))
//        }
//      case AndDef(left, right) =>
//        alphaRenameDef(left){left =>
//          alphaRenameDef(right){right =>
//            cont(AndDef(left, right))
//          }
//        }
//      case _ => ???
//    }
//
//    def alphaRenameTerm(t: Term)(cont: (Term) => Unit): Unit = t match {
//      case TermFuture(cell) =>
//        onComplete(cell){t =>
//          alphaRenameTerm(t)(cont)
//        }
//      case Let(x, xTerm, resTerm) =>
//        val z = symbolUniverse.newSymbol()
//        alphaRenameTerm(xTerm){xTerm =>
//          alphaRenameTerm(resTerm){resTerm =>
//            cont(exprRenameVar(x, z, Let(z, xTerm, resTerm)))
//          }
//        }
//      case Obj(x, xType, d) =>
//        alphaRenameType(xType){xType =>
//          alphaRenameDef(d){d =>
//            val z = symbolUniverse.newSymbol()
//            cont(exprRenameVar(x, z, Obj(z, xType, d)))
//          }
//        }
//      case Fun(x, xType, body) =>
//        alphaRenameType(xType){xType =>
//          alphaRenameTerm(body){body =>
//            val z = symbolUniverse.newSymbol()
//            cont(exprRenameVar(x, z, Fun(z, xType, body)))
//          }
//        }
//      case _ =>
//        cont(t)
//    }

//    def alphaRename[T <: Tree](t: T)(cont: (T) => Unit): Unit = t match {
//      case FutureType(cell) =>
//        onComplete(cell){typ =>
//          alphaRename(typ)(cont.asInstanceOf[(Type) => Unit])
//        }
//      case FunType(x, xType, resType) =>
//        alphaRename(xType){xType =>
//          alphaRename(resType){resType =>
//            val z = symbolUniverse.newSymbol()
//            renameToUniqueVar(x, z, FunType(z, xType, resType))(cont.asInstanceOf[(Type) => Unit])
//          }
//        }
//      case RecType(x, xType) =>
//        alphaRename(xType){xType =>
//          val z = symbolUniverse.newSymbol()
//          renameToUniqueVar(x, z, RecType(z, xType))(cont.asInstanceOf[(Type) => Unit])
//        }
//      case FieldDecl(a, aType) =>
//        alphaRename(aType){newAType =>
//          cont.asInstanceOf[(Type) => Unit](FieldDecl(a, newAType))
//        }
//      case TypeDecl(a, aLowerType, aUpperType) =>
//        val newALowerType = futureType{
//          alphaRename(aLowerType)(_)
//        }
//        val newAUpperType = futureType{
//          alphaRename(aUpperType)(_)
//        }
//        cont.asInstanceOf[(Type) => Unit](TypeDecl(a, newALowerType, newAUpperType))
//      case AndType(left, right) =>
//        val newLeft = futureType{
//          alphaRename(left)(_)
//        }
//        val newRight = futureType{
//          alphaRename(right)(_)
//        }
//        cont.asInstanceOf[(Type) => Unit](AndType(newLeft, newRight))
//      case Let(x, xTerm, t) =>
//        val z = symbolUniverse.newSymbol()
//        alphaRename(xTerm){xTerm =>
//          alphaRename(t){t =>
//            cont.asInstanceOf[(Term) => Unit](exprRenameVar(x, z, Let(z, xTerm, t)))
//          }
//        }
//      case Obj(x, xType, d) =>
//        alphaRename(xType){xType =>
//          alphaRename(d){d =>
//            val z = symbolUniverse.newSymbol()
//            cont.asInstanceOf[(Value) => Unit](exprRenameVar(x, z, Obj(z, xType, d)))
//          }
//        }
//      case Fun(x, xType, body) =>
//        alphaRename(xType){xType =>
//          alphaRename(body){body =>
//            val z = symbolUniverse.newSymbol()
//            cont.asInstanceOf[(Value) => Unit](exprRenameVar(x, z, Fun(z, xType, body)))
//          }
//        }
//      case FieldDef(a, aTerm) =>
//        alphaRename(aTerm){aTerm =>
//          cont.asInstanceOf[(Def) => Unit](FieldDef(a, aTerm))
//        }
//      case TypeDef(a, aType) =>
//        alphaRename(aType){aType =>
//          cont.asInstanceOf[(Def) => Unit](TypeDef(a, aType))
//        }
//      case AndDef(left, right) =>
//        alphaRename(left){left =>
//          alphaRename(right){right =>
//            cont.asInstanceOf[(Def) => Unit](AndDef(left, right))
//          }
//        }
//      case _ =>
//        cont(t)
//    }

    def canonicalRenameToUniqueVar(fromVar: Symbol, toVar: Symbol, t: CanonicalType)(cont: CanonicalType => Unit): Unit = t match {
      //case CanonicalObjType(x, fields, types, projs) if x == fromVar =>
      //  // TODO this shouldn't happen if we did alpha-rename properly?
      //  println(s"fromVar=$fromVar, toVar=$toVar, t=$t")
      //  ???
      //  canonicalRenameToUniqueVar(fromVar, toVar, CanonicalObjType(toVar, fields, types, projs))(cont)
      case CanonicalObjType(x, fields, types, projs) if x != fromVar =>

        val newFields = fields.map{case (a, aType) =>
          a -> CanonicalFuture(contFuture{
            canonicalRenameToUniqueVar(fromVar, toVar, aType)(_)
          })
        }
        val newTypes = types.map{case (a, (aLowerType, aUpperType)) =>
          a -> (CanonicalFuture(contFuture{canonicalRenameToUniqueVar(fromVar, toVar, aLowerType)(_)}),
                CanonicalFuture(contFuture{canonicalRenameToUniqueVar(fromVar, toVar, aUpperType)(_)}))
        }
        val newProjs = projs.map{
          case TypeProj(y, a) if fromVar == y => TypeProj(toVar, a)
          case t => t
        }

        cont(CanonicalObjType(x, newFields, newTypes, newProjs))
      case CanonicalFunType(x, xType, resType, projs) if x == fromVar =>
        ??? // TODO this shouldn't happen if we did alpha-rename properly?
      case CanonicalFunType(x, xType, resType, projs) if x != fromVar =>
        val newXType = CanonicalFuture(contFuture{
          canonicalRenameToUniqueVar(fromVar, toVar, xType)(_)
        })
        val newResType = CanonicalFuture(contFuture{
          canonicalRenameToUniqueVar(fromVar, toVar, resType)(_)
        })
        val newProjs = projs.map{
          case TypeProj(y, a) if fromVar == y => TypeProj(toVar, a)
          case t => t
        }
        cont(CanonicalFunType(x, newXType, newResType, newProjs))
      case CanonicalFuture(cell) =>
        onComplete(cell){
          canonicalRenameToUniqueVar(fromVar, toVar, _)(cont)
        }
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
    def eliminateScopeUp(scope: CanonicalScope, killScope: CanonicalScope, t: CanonicalType)(cont: CanonicalType => Unit): Unit = t match {
      case CanonicalObjType(x, xFields, xTypes, xProjs) =>
        val localKillScope = killScope - x
        val localScope = scope + (x -> t)

        val fields = xFields.map{case (a, aType) =>
          a -> canonicalFuture{
            eliminateScopeUp(localScope, localKillScope, aType)(_)
          }
        }
        val types = xTypes.map{case (a, (aLowerType, aUpperType)) =>
          a -> (canonicalFuture{eliminateScopeDown(localScope, localKillScope, aLowerType)(_)},
                canonicalFuture{eliminateScopeUp(localScope, localKillScope, aUpperType)(_)})
        }

        val projs = xProjs.map{
          case TypeProj(y, a) if localKillScope.contains(y) =>
            canonicalFuture{c =>
              typeProjectUpper(localScope, y, a){aUpperType =>
                // TODO Check if aUpperType ----> TypeProj(y, a) again and
                // raise to Top as necessary. Otherwise this may recurse infinitely...
                eliminateScopeUp(scope, localKillScope, aUpperType)(c)
              }
            }
          case proj @ _ => CanonicalObjType(x, Map(), Map(), Set(proj))
        }
        cont(CanonicalObjType(x, fields, types, Set())) // TODO canonicalLeastCommonSupertype(..., projs)
      case CanonicalFunType(x, xType, resType, projs) =>
        val newXType = canonicalFuture{
          eliminateScopeDown(scope, killScope, xType)(_)
        }
        val localScope = scope + (x -> xType) // TODO vs newXType?
        val newResType = canonicalFuture{
          eliminateScopeUp(localScope, killScope - x, resType)(_)
        }

        val newProjs = projs // TODO

        // TODO lub with eliminatedUp(projs)
        cont(CanonicalFunType(x, newXType, newResType, newProjs))
      case CanonicalFuture(cell) =>
        onComplete(cell){
          eliminateScopeUp(scope, killScope, _)(cont)
        }
      case _ =>
        cont(t)
    }

    def eliminateScopeDown(scope: CanonicalScope, killScope: CanonicalScope, t: CanonicalType)(cont: CanonicalType => Unit): Unit = t match {
      case CanonicalObjType(x, xFields, xTypes, xProjs) =>
        val localKillScope = killScope - x
        val localScope = scope + (x -> t)

        val fields = xFields.map{case (a, aType) =>
          a -> canonicalFuture{
            eliminateScopeDown(localScope, localKillScope, aType)(_) // TODO
          }
        }
        val types = xTypes.map{case (a, (aLowerType, aUpperType)) =>
          a -> (canonicalFuture{eliminateScopeUp(localScope, localKillScope, aLowerType)(_)},
                canonicalFuture{eliminateScopeDown(localScope, localKillScope, aUpperType)(_)})
        }

        val projs = xProjs.map{
          case TypeProj(y, a) if localKillScope.contains(y) =>
            canonicalFuture{c =>
              typeProjectLower(localScope, y, a){aLowerType =>
                // TODO Check if aLowerType ----> TypeProj(y, a) again and
                // raise to Top as necessary. Otherwise this may recurse infinitely...
                eliminateScopeDown(scope, localKillScope, aLowerType)(c)
              }
            }
          case proj @ _ => CanonicalObjType(x, Map(), Map(), Set(proj))
        }
        cont(CanonicalObjType(x, fields, types, Set())) // TODO glb(..., projs)
      case CanonicalFunType(x, xType, resType, projs) =>
        val newXType = canonicalFuture{
          eliminateScopeUp(scope, killScope, xType)(_) // TODO
        }
        val newResType = canonicalFuture{
          val localScope = scope + (x -> xType) // TODO vs newXType?
          eliminateScopeDown(localScope, killScope, resType)(_) // TODO
        }

        val newProjs = projs // TODO

        // TODO glb with eliminatedDown(projs)
        cont(CanonicalFunType(x, newXType, newResType, newProjs))
      case CanonicalFuture(cell) =>
        onComplete(cell){
          eliminateScopeDown(scope, killScope, _)(cont)
        }
      case _ =>
        cont(t)
    }

    def contFuture[T >: Null](f: ((T) => Unit) => Unit): Meh[T] = {
      val lattice = Lattice.trivial[T]
      contCell(lattice)(f)
    }

    case class TermFuture(cell: Meh[Term]) extends Term { // NOTE: Nested, so that Parallel.pool is in scope. Don't mix with other Parallel instances! // TODO pool as argument instead?
      val treeHeight = 1
      val totNumNodes = 1
      assignedType = Some(canonicalFuture{cont =>
        onComplete(cell){actualTerm =>
          cont(actualTerm.assignedType.getOrElse(canonicalError()))
        }
      })
      def withTypeOption(typeOption: Option[CanonicalType]) = {
        val res = this.copy()
        res.assignedType = typeOption
        res
      }
      def withType(typ: CanonicalType) = withTypeOption(Some(typ))
    }

    case class DefFuture(cell: Meh[Def]) extends Def { // NOTE: Nested, so that Parallel.pool is in scope. Don't mix with other Parallel instances!
      val treeHeight = 1
      val totNumNodes = 1
      def withTypeOption(typeOption: Option[CanonicalType]) = {
        val res = this.copy()
        res.assignedType = typeOption
        res
      }
      def withType(typ: CanonicalType) = withTypeOption(Some(typ))
    }

    def termFuture(f: ((Term) => Unit) => Unit) = this.TermFuture(contFuture(f))
    def defFuture(f: ((Def) => Unit) => Unit) = this.DefFuture(contFuture(f))

//    class CanonicalLazyFuture(atom: AtomicReference[CanonicalFuture], putter: (CanonicalType => Unit) => Unit) extends CanonicalPrototype {
//      val treeHeight = 2
//      val totNumNodes = 2
//
//      def get: CanonicalFuture = {
//        var current = atom.get()
//        while (current == null) {
//          val completer = newCellCompleter[CanonicalType](pool, Lattice.trivial)
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


    def solve(types: List[Type], constraints: List[(Type, Type)])(cont: (TypeSubst) => Unit): Unit = {
      ???
    }

    def subst(s: TypeSubst, t: Type)(cont: (Type) => Unit): Unit = {
      ???
    }

    // TODO assert that types are not prototypes

    // TODO INV: never return a type that is not in the given scope
    // e.g.:
    //   f: fun(x: {A: Int..Int})x.A
    //   f({A = Int}): Int  // i.e. not x.A
    // TODO circular dependencies between fields should always result in ErrorType?

    def typecheckTerm(e: Term, p: CanonicalPrototype = CanonicalQue, scope: CanonicalScope = Map()): TypedTerm = { // TODO term-continuations?
      (e,p) match {
        case (_, CanonicalFuture(cell)) =>
          e.withType(
            canonicalFuture{cont =>
              onComplete(cell) {actualP =>
                val typedTerm = typecheckTerm(e, actualP, scope)
                cont(typedTerm.assignedType.getOrElse(canonicalError()))
              }
            }
          )
        case (Var(x), p) =>
          e.withType(
            scope.get(x) match {
              case Some(xType) =>
                canonicalFuture{
                  canonicalRaise(scope, xType, p)(_)
                }
              case None => canonicalError()
            }
          )
        case (Let(x, xTerm, resTerm), p) =>
          val typedXTerm = termFuture{cont =>
            cont(typecheckTerm(xTerm, CanonicalQue, scope))
          }

          val xType = typedXTerm.assignedType.getOrElse(canonicalError())
          if (scope.contains(x)) { // assume alpha-renamed.
            ???
            canonicalError()
          }
          val localScope = scope + (x -> xType)

          val typedResTerm = termFuture{cont =>
            cont(typecheckTerm(resTerm, p, localScope))
          }

          val resType = typedResTerm.assignedType.getOrElse(canonicalError())
          Let(x, typedXTerm, typedResTerm).withType{
            canonicalFuture{cont =>
              eliminateScopeUp(localScope, Map(x -> xType), resType)(cont)
            }
          }
        case (Sel(x, a), p) =>
          val xType = typecheckTerm(Var(x), CanonicalObjType(x, Map(a -> p), Map(), Set()), scope).assignedType.getOrElse(canonicalError())

          e.withType{
            canonicalFuture{cont =>
              expandCanonicalFutureOnce(xType) {
                case CanonicalObjType(y, fields, EmptyMap(), EmptySet()) if x != y && fields.contains(a) => // TODO can x != y? ensure that by canonicalRaise? TODO if aType references y and y is in scope, then it is fine. But if y is not in scope we need to eliminate y. it is probably better that canonicalRaise does this.
                  val aType = fields(a)
                  canonicalRenameToUniqueVar(y, x, aType)(cont)
                case CanonicalObjType(y, fields, EmptyMap(), EmptySet()) if x == y && fields.contains(a) =>
                  val aType = fields(a)
                  cont(aType)
                case _ => cont(canonicalError())
              }
            }
          }
        case (Obj(x, xType, objBody), p) =>
          if (scope.contains(x)) {
            ??? // TODO
          }
          val xCanonicalType = canonicalize(symbolUniverse, scope, x, xType)

          val localScope = scope + (x -> xCanonicalType)

          if (defHasDuplicates(objBody)) {
            e.withType(canonicalError())
          } else {
            def typecheckDef(fields: Map[Symbol, CanonicalPrototype], types: Map[Symbol, (CanonicalPrototype, CanonicalPrototype)], member: Def): Def = member match {
              case FieldDef(a, aTerm) =>
                val aPrototype = fields.getOrElse(a, CanonicalQue) // TODO make sure we count projs.
                val typedATerm = typecheckTerm(aTerm, aPrototype, localScope)
                FieldDef(a, typedATerm) // TODO Invent non-recursive CanonicalObjType?
              case TypeDef(a, aType) =>
                val (aLowerPrototype, aUpperPrototype) = types.getOrElse(a, (CanonicalQue, CanonicalQue))
                val aCanonicalType = canonicalize(symbolUniverse, localScope, symbolUniverse.newSymbol(), aType)
                val aLowerType = canonicalFuture{
                  canonicalLower(localScope, aCanonicalType, aLowerPrototype)(_)
                }
                val aUpperType = canonicalFuture{
                  canonicalRaise(localScope, aCanonicalType, aUpperPrototype)(_)
                }
                member // TODO replace with canonicaltype?
              case AndDef(left, right) =>
                AndDef(
                  defFuture{
                    _(typecheckDef(fields, types, left))
                  },
                  defFuture{
                    _(typecheckDef(fields, types, right))
                  }
                )
              case this.DefFuture(cell) =>
                defFuture{c =>
                  onComplete(cell) {actualMember =>
                    c(typecheckDef(fields, types, actualMember))
                  }
                }
              case _ => ???
            }

            val objType = canonicalFuture{
              canonicalRaise(scope, xCanonicalType, p)(_)
            }

            val typedObjBody = defFuture{cont =>
              expandCanonicalFutureOnce(xCanonicalType) {
                case CanonicalTop =>
                  cont(typecheckDef(Map(), Map(), objBody))
                case tmp @ CanonicalObjType(k, fields, types, projs) if k == x =>
                  // TODO grab fields and types from projs
                  cont(typecheckDef(fields, types, objBody))
                case _ =>
                  cont(objBody.withType(canonicalError()))
              }
            }
            Obj(x, xType, typedObjBody).withType(objType)
          }
        case (Fun(x, _, _), CanonicalQue) =>
          typecheckTerm(e, CanonicalFunType(x, CanonicalQue, CanonicalQue, Set()), scope)
        case (Fun(x, xType, body), CanonicalFunType(y, yPrototype, resPrototype, projs)) if x != y =>
          val z = symbolUniverse.newSymbol()
          val renamedBody = exprRenameVar(x, z, body)
          val renamedYResPrototype = canonicalFuture{canonicalRenameToUniqueVar(y, z, resPrototype)(_)}
          typecheckTerm(Fun(z, xType, renamedBody), CanonicalFunType(z, yPrototype, renamedYResPrototype, projs), scope)
        case (Fun(x, xType, body), CanonicalFunType(y, argPrototype, resPrototype, projs)) if x == y =>
          val argType = canonicalize(symbolUniverse, scope, symbolUniverse.newSymbol(), xType)
          val typedBody = termFuture{cont =>
            cont(typecheckTerm(body, resPrototype, scope + (x -> argType)))
          }

          val loweredArgType = canonicalFuture{
            canonicalLower(scope, argType, argPrototype)(_)
          }
          // TODO do something with projs?
          Fun(x, xType, typedBody).withType{
            CanonicalFunType(x, loweredArgType, typedBody.assignedType.getOrElse(canonicalError()), projs)
          }
        case (Fun(x, xType, body), CanonicalTop) =>
          val argType = canonicalize(symbolUniverse, scope, symbolUniverse.newSymbol(), xType)
          val typedBody = termFuture{cont =>
            cont(typecheckTerm(body, CanonicalTop, scope + (x -> argType)))
          }
          Fun(x, xType, typedBody).withType(CanonicalTop)
        case (App(x, y), p) =>
          val z = symbolUniverse.newSymbol()
          val xPrototype = CanonicalFunType(z, CanonicalQue, p, Set())

          val xType = typecheckTerm(Var(x), xPrototype, scope).assignedType.getOrElse(canonicalError())

          e.withType{
            canonicalFuture{cont =>
              expandCanonicalFutureOnce(xType) {
                case CanonicalFunType(z, zType, resType, projs) =>
                  launch {
                    typecheckTerm(Var(y), zType, scope) // only check for errors.
                  }
                  cont(resType)
                case _ =>
                  cont(canonicalError())
              }
            }
          }
        // TODO light tfun
        // TODO full tfun
        // TODO light tapp
        // TODO full tapp
        case _ =>
          canonicalError()
          ???
      }
    }

    def run[T >: Null](f: ((T) => Unit) => Unit): Option[T] = {
      //println("tc")
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

    def fullTypecheck(term: Term, rootPrototype: CanonicalPrototype = CanonicalQue, rootScope: Map[Symbol, CanonicalType] = Map())(cont: (Term) => Unit): Unit = {
      val lazyTypedTerm = typecheckTerm(term, rootPrototype, rootScope)
      expandTermFutures(lazyTypedTerm) { typedTerm =>
        cont(typedTerm)
      }
    }

  } // end class Parallel

  def typecheckInParallel(symbolUniverse: SymbolUniverse, rootExpr: Term, rootPrototype: CanonicalPrototype = CanonicalQue, rootScope: Map[Symbol, CanonicalType] = Map()): Option[Term] = {
    val pool         = new HandlerPool(1) // TODO
    val typeChecker  = Parallel(symbolUniverse, pool, new AtomicBoolean(false))
    typeChecker.run[Term]{
      typeChecker.fullTypecheck(rootExpr, rootPrototype, rootScope)(_)
    }
  }

  //def pinfer(symbolUniverse: SymbolUniverse, rootExpr: Term, rootPrototype: CanonicalPrototype = CanonicalQue, rootScope: Map[Symbol, CanonicalType] = Map()): Option[CanonicalType] = {
  //  val pool         = new HandlerPool(1) // TODO
  //  val typeChecker  = Parallel(symbolUniverse, pool, new AtomicBoolean(false))
  //  val res = typeChecker.run[CanonicalType]{typeChecker.fullInfer(rootExpr, rootPrototype, rootScope)(_)}
  //  res
  //}
}
