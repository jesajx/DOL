package exjobb

// reactive async
//import cell._
//import lattice._
//import scala.concurrent._
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


  sealed trait Tree {
    val treeHeight: Int
    val totNumSubnodes: Int
    // TODO
  }
  sealed trait Expr extends Tree {
    //val assignedType: Option[CanonicalType]
    @volatile var assignedType: CanonicalType = null // TODO this it need to be volatile?
    def withType(typ: CanonicalType): Expr
  }
  sealed trait Term  extends Expr {
    def withType(typ: CanonicalType): Term
  }
  sealed trait Value extends Term {
    def withType(typ: CanonicalType): Value
  }
  sealed trait Type  extends Tree
  sealed trait Decl  extends Type
  sealed trait Def   extends Expr {
    def withType(typ: CanonicalType): Def
  }

  sealed trait CanonicalType extends Tree

  // Term ::=
  case class Var(x: Symbol) extends Term {
    val treeHeight = 1
    val totNumSubnodes = 1
    def withType(typ: CanonicalType) = {
      val res = this.copy()
      res.assignedType = typ
      res
    }
  }
  case class App(x: Symbol, y: Symbol) extends Term {
    val treeHeight = 1
    val totNumSubnodes = 1
    def withType(typ: CanonicalType) = {
      val res = this.copy()
      res.assignedType = typ
      res
    }
  }
  case class Let(x: Symbol, xTerm: Term, t: Term) extends Term {
    val treeHeight = 1 + math.max(xTerm.treeHeight, t.treeHeight)
    val totNumSubnodes = 1 + xTerm.totNumSubnodes + t.totNumSubnodes
    def withType(typ: CanonicalType) = {
      val res = this.copy()
      res.assignedType = typ
      res
    }
  }
  case class Sel(x: Symbol, a: Symbol) extends Term {
    val treeHeight = 1
    val totNumSubnodes = 1
    def withType(typ: CanonicalType) = {
      val res = this.copy()
      res.assignedType = typ
      res
    }
  }

  // Value ::=
  case class Obj(x: Symbol, xType: Type, body: Def) extends Value {
    val treeHeight = 1 + math.max(xType.treeHeight, body.treeHeight)
    val totNumSubnodes = 1 + xType.totNumSubnodes + body.totNumSubnodes
    def withType(typ: CanonicalType) = {
      val res = this.copy()
      res.assignedType = typ
      res
    }
  }
  case class Fun(x: Symbol, xType: Type, body: Term) extends Value {
    val treeHeight = 1 + math.max(xType.treeHeight, body.treeHeight)
    val totNumSubnodes = 1 + xType.totNumSubnodes + body.totNumSubnodes
    def withType(typ: CanonicalType) = {
      val res = this.copy()
      res.assignedType = typ
      res
    }
  }

  // Def ::=
  case class FieldDef(a: Symbol, aTerm: Term) extends Def {
    val treeHeight = 1 + aTerm.treeHeight
    val totNumSubnodes = 1 + aTerm.totNumSubnodes
    def withType(typ: CanonicalType) = {
      val res = this.copy()
      res.assignedType = typ
      res
    }
  }
  case class TypeDef(a: Symbol, aType: Type) extends Def {
    val treeHeight = 1 + aType.treeHeight
    val totNumSubnodes = 1 + aType.totNumSubnodes
    def withType(typ: CanonicalType) = {
      val res = this.copy()
      res.assignedType = typ
      res
    }
  }
  case class AndDef(left: Def, right: Def)  extends Def {
    val treeHeight = 1 + math.max(left.treeHeight, right.treeHeight)
    val totNumSubnodes = 1 + left.totNumSubnodes + right.totNumSubnodes
    def withType(typ: CanonicalType) = {
      val res = this.copy()
      res.assignedType = typ
      res
    }
  }

  // Type ::=
  case object Bot extends Type {
    val treeHeight = 1
    val totNumSubnodes = 1
  }
  case object Top extends Type {
    val treeHeight = 1
    val totNumSubnodes = 1
  }
  case class TypeProj(x: Symbol, a: Symbol) extends Type {
    val treeHeight = 1
    val totNumSubnodes = 1
  }
  case class FunType(x: Symbol, xType: Type, resType: Type) extends Type {
    val treeHeight = 1 + math.max(xType.treeHeight, resType.treeHeight)
    val totNumSubnodes = 1 + xType.totNumSubnodes + resType.totNumSubnodes
  }
  case class RecType(x: Symbol, xType: Type) extends Type {
    val treeHeight = 1 + xType.treeHeight
    val totNumSubnodes = 1 + xType.totNumSubnodes
  }
  case class AndType(left: Type, right: Type) extends Type {
    val treeHeight = 1 + math.max(left.treeHeight, right.treeHeight)
    val totNumSubnodes = 1 + left.totNumSubnodes + right.totNumSubnodes
  }
  case class FieldDecl(a: Symbol, aType: Type) extends Type {
    val treeHeight = 1 + aType.treeHeight
    val totNumSubnodes = 1 + aType.totNumSubnodes
  }
  case class TypeDecl(a: Symbol, aLowerType: Type, aUpperType: Type) extends Type {
    val treeHeight = 1 + math.max(aLowerType.treeHeight, aUpperType.treeHeight)
    val totNumSubnodes = 1 + aLowerType.treeHeight + aUpperType.treeHeight
  }


  def pairToList[T](pair: (T, T)): List[T] = List(pair._1, pair._2)
  def max(rest: Int*): Int = rest.toList.max

  // TODO what about TypeProj that is not an object?
  case class CanonicalObjType(x: Symbol, xFields: Map[Symbol, CanonicalType], xTypes: Map[Symbol, (CanonicalType, CanonicalType)], xProjs: Set[TypeProj]) extends CanonicalType { // TODO store scope? // TODO aka "ScaryIntersectionType"
    val treeHeight = 1 + (xFields.values ++ xTypes.values.flatMap{pairToList}).map{_.treeHeight}.fold(0){math.max}
    val totNumSubnodes = 1 + (xFields.values ++ xTypes.values.flatMap{pairToList}).map{_.totNumSubnodes}.sum + xProjs.size
  }
  case class CanonicalFunType(x: Symbol, xType: CanonicalType, resType: CanonicalType, alias: Set[TypeProj]) extends CanonicalType { // TODO differentiate between projs that have subtypes and projs that don't (opaqueProjs). TODO maybe Map[TypeProj, (Option[CanonicalObjType], Option[CanonicalObjType])]
    val treeHeight = 1 + max(xType.treeHeight, resType.treeHeight, 2)
    val totNumSubnodes = 1 + xType.totNumSubnodes + resType.totNumSubnodes + alias.size
  }
  case object CanonicalTop extends CanonicalType {
    val treeHeight = 1
    val totNumSubnodes = 1
  }
  case object CanonicalBot extends CanonicalType {
    val treeHeight = 1
    val totNumSubnodes = 1
  }
  case object CanonicalErrorType extends CanonicalType {
    val treeHeight = 1
    val totNumSubnodes = 1
  }
  case class CanonicalFuture(cell: Meh[CanonicalType]) extends CanonicalType {
    val treeHeight = 1
    val totNumSubnodes = 1
  }
  type CanonicalPrototype = CanonicalType
  case object CanonicalQue extends CanonicalPrototype {
    val treeHeight = 1
    val totNumSubnodes = 1
  }

  object NoFuture {
    def canonicalRenameToUniqueVar(fromVar: Symbol, toVar: Symbol, t: CanonicalType): CanonicalType = t match {
      case CanonicalObjType(x, fields, types, projs) if x != fromVar =>
        val newFields = fields.map{case (a, aType) =>
          a -> NoFuture.canonicalRenameToUniqueVar(fromVar, toVar, aType)
        }
        val newTypes = types.map{case (a, (aLowerType, aUpperType)) =>
          a -> (NoFuture.canonicalRenameToUniqueVar(fromVar, toVar, aLowerType),
                NoFuture.canonicalRenameToUniqueVar(fromVar, toVar, aUpperType))
        }
        val newProjs = projs.map{
          case TypeProj(y, a) if fromVar == y => TypeProj(toVar, a)
          case t => t
        }
        CanonicalObjType(x, newFields, newTypes, newProjs)
      case CanonicalFunType(x, xType, resType, projs) if x != fromVar =>
        val newXType = NoFuture.canonicalRenameToUniqueVar(fromVar, toVar, xType)
        val newResType = NoFuture.canonicalRenameToUniqueVar(fromVar, toVar, resType)
        val newProjs = projs.map{
          case TypeProj(y, a) if fromVar == y => TypeProj(toVar, a)
          case t => t
        }
        CanonicalFunType(x, newXType, newResType, newProjs)
      case _ =>
        t
    }

    def canonicalEqualTypes(first: CanonicalType, second: CanonicalType): Boolean = (first, second) match {
      case (CanonicalObjType(x, xFields, xTypes, xProjs), CanonicalObjType(y, yFields, yTypes, yProjs)) =>
        false // TODO rename and then compare
      case (CanonicalFunType(x, xType, xResType, xAlias), CanonicalFunType(y, yType, yResType, yAlias)) if x != y =>
        val z = -1 // TODO
        canonicalEqualTypes(
          NoFuture.canonicalRenameToUniqueVar(x, z, CanonicalFunType(z, xType, xResType, xAlias)),
          NoFuture.canonicalRenameToUniqueVar(y, z, CanonicalFunType(z, yType, yResType, yAlias)))
      case (CanonicalFunType(x, xType, xResType, xAlias), CanonicalFunType(y, yType, yResType, yAlias)) if x == y =>
        canonicalEqualTypes(xType, yType) && canonicalEqualTypes(xResType, yResType) && xAlias == yAlias
      case (CanonicalTop, CanonicalTop) => true
      case (CanonicalBot, CanonicalBot) => true
      case (CanonicalQue, CanonicalQue) => true
      case _ =>
        false // TODO
        //case _ =>
        //  ErrorType
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

        RecType(x, (fields ++ types ++ projs).reduce(AndType(_, _)))
      case CanonicalFunType(x, xType, resType, alias) =>
        alias.fold[Type](FunType(x, NoFuture.decanonicalize(xType), NoFuture.decanonicalize(resType))){AndType(_, _)}
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
    val totNumSubnodes = 1
  }

  case object ErrorType extends Type { // TODO vs some other errorhandling?
    val treeHeight = 1
    val totNumSubnodes = 1
  }
  case class FutureType(cell: Meh[Type]) extends Type {
    val treeHeight = 1
    val totNumSubnodes = 1
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

  def expandFutureTypes(t: Type)(cont: (Type) => Unit): Unit = t match {
    case FutureType(cell) =>
      onComplete(cell) {actualType =>
        expandFutureTypes(actualType) {actualType =>
          cont(actualType)
        }
      }
    case FunType(x, xType, resType) =>
      expandFutureTypes(xType) {newXType =>
        expandFutureTypes(resType) {newResType =>
          cont(FunType(x, newXType, newResType))
        }
      }
    case RecType(x, xType) =>
      expandFutureTypes(xType){newXType =>
        cont(RecType(x, newXType))
      }
    case FieldDecl(a, aType) =>
      expandFutureTypes(aType){aType =>
        cont(FieldDecl(a, aType))
      }
    case TypeDecl(a, aLowerType, aUpperType) =>
      expandFutureTypes(aLowerType){newALowerType =>
        expandFutureTypes(aUpperType){newAUpperType =>
          cont(TypeDecl(a, newALowerType, newAUpperType))
        }
      }
    case AndType(leftType, rightType) =>
      expandFutureTypes(leftType){newLeftType =>
        expandFutureTypes(rightType){newRightType =>
          cont(AndType(newLeftType, newRightType))
        }
      }
    //case PolyFunType(typeParams, params, resType) =>
    //  def k2(innerList: List[(Symbol, Type)])(cont: (List[(Symbol, Type)]) => Unit): Unit = innerList match {
    //    case (x, xType) :: rest =>
    //      expandFutureTypes(xType) {newXType =>
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
    //    expandFutureTypes(resType) {newResType =>
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
        expandFutureTypes(t){newT =>
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
  }

  def defKeysAsSet(d: Def): Set[Symbol] = d match {
    case FieldDef(a, aTerm)  => Set(a)
    case TypeDef(a, aType)   => Set(a)
    case AndDef(left, right) => defKeysAsSet(left) ++ defKeysAsSet(right)
  }

  def defTermAsMap(d: Def): Map[Symbol, Def] = d match {
    case FieldDef(a, _) => Map(a -> d)
    case TypeDef(a, _)  => Map(a -> d)
    case AndDef(left, right) => defTermAsMap(left) ++ defTermAsMap(right)
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

  class TypeChecker(val symbolUniverse: SymbolUniverse, val pool: HandlerPool, val hasErrorsAtom:  AtomicBoolean) {
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


    def canonicalize(scope: CanonicalScope, typ: Type): CanonicalFuture = {
        val z = symbolUniverse.newSymbol()
        val zCompleter = newCellCompleter(pool, Lattice.trivial[CanonicalType])
        val zCanonicalType = CanonicalFuture(zCompleter.cell)
        launch {
          canonicalType(scope + (z -> zCanonicalType), z, typ, Set()) {
            zCompleter.putFinal(_)
          }
        }
        zCanonicalType
    }

    def canonicalType(scope: CanonicalScope, z: Symbol, zType: Type, visitedProjs: Set[TypeProj])(cont: (CanonicalType) => Unit): Unit = zType match {
      case RecType(x, xType) =>
        renameToUniqueVar(x, z, xType){
          canonicalType(scope, z, _, visitedProjs)(cont)
        }
      case AndType(left, right) =>
        val leftCell = contFuture[CanonicalType] {
          canonicalType(scope, z, left, visitedProjs)(_)
        }
        val rightCell = contFuture[CanonicalType] {
          canonicalType(scope, z, right, visitedProjs)(_)
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
      case proj @ TypeProj(x, a) =>
        // TODO expand to glb(x.a, upper(x.a)) here?
        cont(CanonicalObjType(z, Map(), Map(), Set(proj)))
      case decl @ FieldDecl(a, aType) =>
        val aCanonicalType = canonicalize(scope, aType)
        cont(CanonicalObjType(z, Map(a -> aCanonicalType), Map(), Set()))
      case decl @ TypeDecl(a, aLowerType, aUpperType) =>
        val aLowerCanonicalType = canonicalize(scope, aLowerType)
        val aUpperCanonicalType = canonicalize(scope, aUpperType)
        cont(CanonicalObjType(z, Map(), Map(a -> (aLowerCanonicalType, aUpperCanonicalType)), Set()))
      case FutureType(cell) =>
        onComplete(cell){
          canonicalType(scope, z, _, visitedProjs)(cont)
        }
      case FunType(x, xType, resType) =>
        val y = symbolUniverse.newSymbol()
        val canonicalXType = canonicalize(scope, xType)
        val canonicalResType = CanonicalFuture(contFuture{c =>
          renameToUniqueVar(x, y, resType){
            canonicalType(scope, y, _, visitedProjs)(c)
          }
        })
        cont(CanonicalFunType(y, canonicalXType, canonicalResType, Set()))
      case Top => cont(CanonicalTop)
      case Bot => cont(CanonicalBot)
      case Que => cont(CanonicalQue)
      case ErrorType => cont(canonicalError())
    }

    def canonicalFuture(f: ((CanonicalType) => Unit) => Unit) = CanonicalFuture(contFuture(f))

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

    def typeProjectUpper(scope: CanonicalScope, x: Symbol, a: Symbol)(cont: (CanonicalType) => Unit): Unit = {
      typeProject(scope, x, a){(_, aUpperType) => cont(aUpperType)}
    }
    def typeProjectLower(scope: CanonicalScope, x: Symbol, a: Symbol)(cont: (CanonicalType) => Unit): Unit = {
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


    def canonicalRaise(scope: CanonicalScope, t: CanonicalType, p: CanonicalType)(cont: (CanonicalType) => Unit): Unit = {
      //expandCanonicalFutureTypes(t){t =>
      //  expandCanonicalFutureTypes(p){p =>
      //    //println(s"canonicalRaise($scope, $t, $p)")
          canonicalRaise2(scope, t, p) { res =>
      //      expandCanonicalFutureTypes(res){res =>
      //        println(s"canonicalRaise($scope, $t, $p) = $res")
              cont(res)
            }
      //    }
      //  }
      //}
    }

    def canonicalRaise2(scope: CanonicalScope, t: CanonicalType, p: CanonicalType)(cont: (CanonicalType) => Unit): Unit = (t, p) match {
      case (t @ CanonicalObjType(x, xFields, xTypes, xProjs), p @ CanonicalObjType(y, yFields, yTypes, yProjs)) =>
        val z = symbolUniverse.newSymbol()
        val zCompleter = newCellCompleter(pool, Lattice.trivial[CanonicalType])
        val zType = CanonicalFuture(zCompleter.cell)

        // TODO point y to new skeletal object
        canonicalRenameToUniqueVar(y, z, CanonicalObjType(z, yFields, yTypes, yProjs)){
          case CanonicalObjType(_, yFields, yTypes, yProjs) =>
            val localScope = Map(x -> t, z -> zType) // TODO bad to put prototype in scope? // TODO reference result of canonicalRaise instead?
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
                lowerTypeProjections(scope, t){lowerProjs => // TODO is it correct to use lower?
                  val missingProjs = yProjs -- (lowerProjs ++ upperProjs).map{
                    case TypeProj(w, a) if w == x => TypeProj(z, a)
                    case u => u
                  }
                  missingProjs.foreach{case TypeProj(r, a) =>
                    typeProjectLower(localScope, r, a){
                      ???
                    }
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
        ???
      case _ =>
        cont(canonicalError())
    }

    def canonicalLower(scope: CanonicalScope, t: CanonicalType, p: CanonicalType)(cont: (CanonicalType) => Unit): Unit = (t, p) match {
//      case (t @ CanonicalObjType(x, xFields, xTypes, xProjs), p @ CanonicalObjType(y, yFields, yTypes, yProjs)) =>
//        val z = symbolUniverse.newSymbol()
//        val zCompleter = newCellCompleter(pool, Lattice.trivial[CanonicalType])
//        val zType = CanonicalFuture(zCompleter.cell)
//
//        // TODO point y to new skeletal object
//        canonicalRenameToUniqueVar(y, z, CanonicalObjType(z, yFields, yTypes, yProjs)){
//          case CanonicalObjType(_, yFields, yTypes, yProjs) =>
//            val localScope = Map(x -> t, z -> zType) // TODO bad to put prototype in scope? // TODO reference result of canonicalRaise instead?
//            val xIsMissingFields = yFields.exists{case (a, _) => !xFields.contains(a) || xFields(a) == Set()}
//            val xIsMissingTypes = yTypes.exists{case (a, _) => !xTypes.contains(a) || xTypes(a) == Set()}
//            if (xIsMissingFields || xIsMissingTypes) {
//              cont(canonicalError())
//            } else {
//
//
//              // TODO
//              // t <: A <: y.T
//              // t <: A <: y.T
//
//
//              // TODO okay to add TypeDecls when raising, but not okay to add FieldDecls?
//              val fields = yFields.map{case (a, aPrototype) =>
//                xFields.get(a) match {
//                  case Some(aType) => a -> canonicalFuture{canonicalRaise(localScope, aType, aPrototype)(_)}
//                  case None => (a, canonicalError())
//                }
//              }.toMap
//              val types = yTypes.map{case (a, (aLowerPrototype, aUpperPrototype)) =>
//                xTypes.get(a) match {
//                  case Some((aLowerType, aUpperType)) =>
//                    a -> (canonicalFuture{canonicalRaise(localScope, aLowerType, aLowerPrototype)(_)},
//                          canonicalFuture{canonicalRaise(localScope, aUpperType, aUpperPrototype)(_)})
//                  case None => a -> (canonicalError(), canonicalError())
//                }
//              }.toMap
//
//              // TODO check lower too
//              upperTypeProjections(scope, t){upperProjs =>
//                lowerTypeProjections(scope, t){lowerProjs =>
//                  val missingProjs = yProjs -- (lowerProjs ++ upperProjs).map{
//                    case TypeProj(w, a) if w == x => TypeProj(z, a)
//                    case u => u
//                  }
//                  missingProjs.foreach{case TypeProj(r, a) =>
//                    typeProjectLower(localScope, r, a){
//                      ???
//                    }
//                  }
//                }
//              }
//
//
//              // continue while checking projs in background.
//              val projs = yProjs
//
//              zCompleter.putFinal(CanonicalObjType(y, fields, types, projs))
//              cont(zType)
//            }
//          case _ =>
//            ???
//        }
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
//      case _ =>
//        cont(canonicalError())
    }

    def expandCanonicalFutureTypes(t: CanonicalType)(cont: (CanonicalType) => Unit): Unit = t match {
      case CanonicalObjType(z, fields, types, projs) =>
        val fieldConts = fields.map{case (a, aType) =>
          {(c: (Map[Symbol, CanonicalType]) => Unit) =>
            expandCanonicalFutureTypes(aType){res =>
              c(Map(a -> res))
            }
          }
        }.toList
        val typeConts = types.map{case (a, (aLowerType, aUpperType)) =>
          {(c: (Map[Symbol, (CanonicalType, CanonicalType)]) => Unit) =>
            expandCanonicalFutureTypes(aLowerType) {aLowerType =>
              expandCanonicalFutureTypes(aUpperType) {aUpperType =>
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
        expandCanonicalFutureTypes(xType) {xType =>
          expandCanonicalFutureTypes(resType) {resType =>
            cont(CanonicalFunType(x, xType, resType, projs))
          }
        }
      case CanonicalFuture(cell) =>
        onComplete(cell){
          expandCanonicalFutureTypes(_)(cont)
        }
      case _ =>
        cont(t)
    }

    def expandCanonicalFutureOnce(t: CanonicalType)(cont: (CanonicalType) => Unit): Unit = t match {
      case CanonicalFuture(cell) =>
        onComplete(cell) {
          expandCanonicalFutureOnce(_)(cont)
        }
      case _ => cont(t)
    }


    def canonicalScope(scope: Scope)(cont: (CanonicalScope) => Unit): Unit = {
      val typesAndCompleters = scope.map{case (x, t) =>
        (x, t, newCellCompleter(pool, Lattice.trivial[CanonicalType]))
      }
      val canonicalScope = typesAndCompleters.map{case (x, _, c) => (x, CanonicalFuture(c.cell))}.toMap
      typesAndCompleters.foreach{case (x, t, c) =>
        canonicalType(canonicalScope, x, t, Set()) {
          c.putFinal(_)
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

    def mapIntersect[K, T, A <: T, B <: T](a: Map[K, A], b: Map[K, B])(f: (A, B) => T): Map[K, T] = {
      a ++ b.flatMap{
        case (k, kValueInB) =>
          a.get(k) match {
            case Some(kValueInA) =>
              Some(k -> f(kValueInA, kValueInB))
            case None =>
             None
          }
      }
    }

    // TODO decanonicalize?

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


    def canonicalGreatestCommonSubtype(scope: CanonicalScope, firstType: CanonicalType, secondType: CanonicalType)(cont: (CanonicalType) => Unit): Unit = (firstType, secondType) match {
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

    def canonicalLeastCommonSupertype(scope: CanonicalScope, firstType: CanonicalType, secondType: CanonicalType)(cont: (CanonicalType) => Unit): Unit = (firstType, secondType) match {
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

    def alphaRename[T <: Tree](t: T)(cont: (T) => Unit): Unit = t match {
      case FutureType(cell) =>
        onComplete(cell){typ =>
          alphaRename(typ)(cont.asInstanceOf[(Type) => Unit])
        }
      case FunType(x, xType, resType) =>
        alphaRename(xType){xType =>
          alphaRename(resType){resType =>
            val z = symbolUniverse.newSymbol()
            renameToUniqueVar(x, z, FunType(z, xType, resType))(cont.asInstanceOf[(Type) => Unit])
          }
        }
      case RecType(x, xType) =>
        alphaRename(xType){xType =>
          val z = symbolUniverse.newSymbol()
          renameToUniqueVar(x, z, RecType(z, xType))(cont.asInstanceOf[(Type) => Unit])
        }
      case FieldDecl(a, aType) =>
        alphaRename(aType){newAType =>
          cont.asInstanceOf[(Type) => Unit](FieldDecl(a, newAType))
        }
      case TypeDecl(a, aLowerType, aUpperType) =>
        val newALowerType = futureType{
          alphaRename(aLowerType)(_)
        }
        val newAUpperType = futureType{
          alphaRename(aUpperType)(_)
        }
        cont.asInstanceOf[(Type) => Unit](TypeDecl(a, newALowerType, newAUpperType))
      case AndType(left, right) =>
        val newLeft = futureType{
          alphaRename(left)(_)
        }
        val newRight = futureType{
          alphaRename(right)(_)
        }
        cont.asInstanceOf[(Type) => Unit](AndType(newLeft, newRight))
      case Let(x, xTerm, t) =>
        val z = symbolUniverse.newSymbol()
        alphaRename(xTerm){xTerm =>
          alphaRename(t){t =>
            cont.asInstanceOf[(Term) => Unit](renameToUniqueVarInExpr(x, z, Let(z, xTerm, t)))
          }
        }
      case Obj(x, xType, d) =>
        alphaRename(xType){xType =>
          alphaRename(d){d =>
            val z = symbolUniverse.newSymbol()
            cont.asInstanceOf[(Value) => Unit](renameToUniqueVarInExpr(x, z, Obj(z, xType, d)))
          }
        }
      case Fun(x, xType, body) =>
        alphaRename(xType){xType =>
          alphaRename(body){body =>
            val z = symbolUniverse.newSymbol()
            cont.asInstanceOf[(Value) => Unit](renameToUniqueVarInExpr(x, z, Fun(z, xType, body)))
          }
        }
      case FieldDef(a, aTerm) =>
        alphaRename(aTerm){aTerm =>
          cont.asInstanceOf[(Def) => Unit](FieldDef(a, aTerm))
        }
      case TypeDef(a, aType) =>
        alphaRename(aType){aType =>
          cont.asInstanceOf[(Def) => Unit](TypeDef(a, aType))
        }
      case AndDef(left, right) =>
        alphaRename(left){left =>
          alphaRename(right){right =>
            cont.asInstanceOf[(Def) => Unit](AndDef(left, right))
          }
        }
      case _ =>
        cont(t)
    }

    def canonicalRenameToUniqueVar(fromVar: Symbol, toVar: Symbol, t: CanonicalType)(cont: (CanonicalType) => Unit): Unit = t match {
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

    def renameToUniqueVarInExpr[T <: Expr](fromVar: Symbol, toVar: Symbol, e: T): T = e match {
      case Var(x) if x == fromVar => Var(toVar).asInstanceOf[T]
      case App(x, y) if x == fromVar || y == fromVar =>
        val newX = if (x == fromVar) toVar else x
        val newY = if (y == fromVar) toVar else y
        App(newX, newY).asInstanceOf[T]
      case Let(x, xTerm, t) if x != fromVar =>
        Let(x, renameToUniqueVarInExpr(fromVar, toVar, xTerm), renameToUniqueVarInExpr(fromVar, toVar, t)).asInstanceOf[T]
      case Sel(x, a) if x == fromVar =>
        Sel(toVar, a).asInstanceOf[T]
      case Obj(x, xType, d) if x != fromVar =>
        val newXType = futureType{
          renameToUniqueVar(fromVar, toVar, xType)(_)
        }
        Obj(x, newXType, renameToUniqueVarInExpr(fromVar, toVar, d)).asInstanceOf[T]
      case Fun(x, xType, t) if x != fromVar =>
        val newXType = futureType{
          renameToUniqueVar(fromVar, toVar, xType)(_)
        }
        Fun(x, newXType, renameToUniqueVarInExpr(fromVar, toVar, t)).asInstanceOf[T]
      case FieldDef(a, aTerm) =>
        FieldDef(a, renameToUniqueVarInExpr(fromVar, toVar, aTerm)).asInstanceOf[T]
      case TypeDef(a, aType) =>
        val newAType = futureType{
          renameToUniqueVar(fromVar, toVar, aType)(_)
        }
        TypeDef(a, newAType).asInstanceOf[T]
      case AndDef(left, right) =>
        AndDef(
          renameToUniqueVarInExpr(fromVar, toVar, left),
          renameToUniqueVarInExpr(fromVar, toVar, right)).asInstanceOf[T]
      case _ => e
    }

    // TODO will raiseTo(..., classA, classA) when classA is partially lazy lead to problems?

    // TODO replace killScope:Scope with killSet:Set[Symbol]?
    def eliminateScopeUp(scope: CanonicalScope, killScope: CanonicalScope, t: CanonicalType)(cont: (CanonicalType) => Unit): Unit = t match {
      case CanonicalObjType(x, xFields, xTypes, xProjs) =>
        ???
//      case CanonicalObjType(x, xFields, xTypes, xProjs) if !killScope.contains(x) =>
//        val localScope = scope + (x -> t)
//
//        val fields = xFields.map{case (a, aType) =>
//          eliminateScopeUp(localScope, killScope, aType)(_) // TODO
//        }
//        val types = xTypes.map{case (a, (aLowerType, aUpperType)) =>
//          eliminateScopeDown(localScope, killScope, aLowerType)(_) // TODO
//          eliminateScopeUp(localScope, killScope, aUpperType)(_) // TODO
//        }
//
//        val projs = xProjs.map{
//          case TypeProj(y, a) if killScope.contains(y) =>
//            typeProjectUpper(localScope, y, a)(_) // TODO
//          case proj @ _ => CanonicalObjType(x, Map(), Map(), Set(proj))
//        }
//        CanonicalObjType(x, fields, types, Set()) // TODO canonicalLeastCommonSupertype(..., projs)
//
      case CanonicalFunType(x, xType, resType, alias) =>
        val newXType = canonicalFuture{
          eliminateScopeDown(scope, killScope, xType)(_) // TODO
        }
        val newResType = canonicalFuture{
          val localScope = scope + (x -> xType) // TODO vs newXType?
          eliminateScopeUp(localScope, killScope, resType)(_) // TODO
        }

        val newAlias = alias // TODO

        // TODO lub with eliminatedUp(alias)
        cont(CanonicalFunType(x, newXType, newResType, newAlias))
      case CanonicalFuture(cell) =>
        onComplete(cell){
          eliminateScopeUp(scope, killScope, _)(cont)
        }
      case _ =>
        cont(t)
    }

    def eliminateScopeDown(scope: CanonicalScope, killScope: CanonicalScope, t: CanonicalType)(cont: (CanonicalType) => Unit): Unit = t match {
      case CanonicalObjType(x, xFields, xTypes, xProjs) =>
        ???
//      case CanonicalObjType(x, xFields, xTypes, xProjs) if !killScope.contains(x) =>
//        val localScope = scope + (x -> t)
//
//        val fields = xFields.map{case (a, aType) =>
//          eliminateScopeDown(localScope, killScope, aType)(_) // TODO
//        }
//        val types = xTypes.map{case (a, (aLowerType, aUpperType)) =>
//          eliminateScopeUp(localScope, killScope, aLowerType)(_) // TODO
//          eliminateScopeDown(localScope, killScope, aUpperType)(_) // TODO
//        }
//
//        val projs = xProjs.map{
//          case TypeProj(y, a) if killScope.contains(y) =>
//            typeProjectLower(localScope, y, a)(_) // TODO
//          case proj @ _ => CanonicalObjType(x, Map(), Map(), Set(proj))
//        }
//        CanonicalObjType(x, fields, types, Set()) // TODO glb(..., projs)
//
      case CanonicalFunType(x, xType, resType, alias) =>
        val newXType = canonicalFuture{
          eliminateScopeUp(scope, killScope, xType)(_) // TODO
        }
        val newResType = canonicalFuture{
          val localScope = scope + (x -> xType) // TODO vs newXType?
          eliminateScopeDown(localScope, killScope, resType)(_) // TODO
        }

        val newAlias = alias // TODO

        // TODO glb with eliminatedDown(alias)
        cont(CanonicalFunType(x, newXType, newResType, newAlias))
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

    def error(): Type = {
      hasErrorsAtom.lazySet(true)
      //???
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

    def r(e: Term, p: CanonicalPrototype = CanonicalQue, scope: CanonicalScope = Map())(cont: (CanonicalType) => Unit): Unit = { // TODO return terms with assigned type
      (e,p) match {
        case (_, CanonicalFuture(cell)) =>
          onComplete(cell) {
            r(e, _, scope)(cont)
          }
        case (Var(x), p) =>
          scope.get(x) match {
            case Some(xType) =>
              canonicalRaise(scope, xType, p)(cont)
            case None => e.withType(canonicalError())
          }
        case (Let(x, xTerm, t), p) =>
          val xType = CanonicalFuture(contFuture[CanonicalType] {
            r(xTerm, CanonicalQue, scope)(_) // pass down "x"?
          })
          val z = symbolUniverse.newSymbol()
          val renamedT = renameToUniqueVarInExpr(x, z, t)
          val localScope = scope + (z -> xType)
          r(renamedT, p, localScope) {tType =>
            canonicalRaise(localScope, tType, p){
              eliminateScopeUp(localScope, Map(z -> xType), _)(cont)
            }
          }
        case (Sel(x, a), p) =>
          r(Var(x), CanonicalObjType(x, Map(a -> p), Map(), Set()), scope) {
            expandCanonicalFutureOnce(_) {
              case CanonicalObjType(y, fields, EmptyMap(), EmptySet()) if x == y && fields.contains(a) => // TODO can x != y? ensure that by canonicalRaise?
                cont(fields(a))
              case _ => cont(canonicalError())
            }
          }
        case (Obj(x, xType, d), p) =>
          val z = symbolUniverse.newSymbol()
          if (defHasDuplicates(d)) {
            cont(canonicalError())
          } else {
            alphaRename(xType) {xType =>
              alphaRename(d) {dType =>
                renameToUniqueVar(x, z, xType){zType =>
                  val zCompleter = newCellCompleter(pool, Lattice.trivial[CanonicalType])
                  val zCanonicalType = CanonicalFuture(zCompleter.cell)
                  launch {
                    canonicalType(scope + (z -> zCanonicalType), z, zType, Set()) {res =>
                      zCompleter.putFinal(res)
                    }
                  }
                  val localScope = scope + (z -> zCanonicalType)
                  expandCanonicalFutureOnce(zCanonicalType) {
                    case CanonicalTop =>
                      ???
                    case tmp @ CanonicalObjType(k, fields, types, alias) if k == z =>
                      def k(member: Def)(c: (CanonicalType) => Unit): Unit = member match {
                        case FieldDef(a, aTerm) =>
                          expandCanonicalFutureOnce(fields.getOrElse(a, CanonicalQue)) {aPrototype =>
                            r(aTerm, aPrototype, localScope) {aType =>
                              c(CanonicalObjType(z, Map(a -> aType), Map(), Set()))
                            }
                          }
                        case TypeDef(a, aType) =>
                          val aPrototype = CanonicalObjType(z, Map(), Map(a -> types.getOrElse(a, (CanonicalQue, CanonicalQue))), Set())
                          val aCanonicalType = canonicalize(localScope, aType)
                          canonicalRaise(localScope, CanonicalObjType(z, Map(), Map(a -> (aCanonicalType, aCanonicalType)), Set()), aPrototype)(c)
                        case AndDef(left, right) =>
                          val leftType = CanonicalFuture(contFuture[CanonicalType] {
                            k(left)(_)
                          })
                          val rightType = CanonicalFuture(contFuture[CanonicalType] {
                            k(right)(_)
                          })
                          canonicalGreatestCommonSubtype(scope, leftType, rightType)(c)
                      }
                      // TODO need to check that type-projections are fulfilled!!!

                      val dType = CanonicalFuture(contFuture[CanonicalType] {
                        k(d)(_)
                      }) // TODO e.type = dType
                      canonicalRaise(scope, zCanonicalType, p)(cont)
                  }
                }
              }
            }
          }
        case (Fun(x, _, _), CanonicalQue) =>
          r(e, CanonicalFunType(x, CanonicalQue, CanonicalQue, Set()), scope)(cont)
        case (Fun(x, xType, res), CanonicalFunType(y, yPrototype, yResPrototype, projs)) =>
          val z = symbolUniverse.newSymbol()
          val argType = canonicalize(scope, xType) // TODO assert x not free in xType? maybe sufficient to simply not add x to scope? what if some other x is in scope?
          val renamedRes = renameToUniqueVarInExpr(x, z, res) // TODO maybe handle all renaming before typechecking?

          val resType = CanonicalFuture(contFuture{c =>
            canonicalRenameToUniqueVar(y, z, yResPrototype){zResPrototype =>
              r(renamedRes, zResPrototype, scope + (z -> argType))(c)
            }
          })
          // TODO projs. needs renaming.
          canonicalRaise(scope, CanonicalFunType(z, argType, resType, Set()), p)(cont)
        case (Fun(x, xType, res), CanonicalTop) =>
          val funType = CanonicalFuture(contFuture{
            r(e, CanonicalFunType(x, CanonicalQue, CanonicalTop, Set()), scope)(_)
          }) // TODO e.type = ...
          cont(CanonicalTop)
        case (App(x, y), p) =>
          r(Var(x), CanonicalQue, scope) {
            expandCanonicalFutureOnce(_) {
              case CanonicalFunType(z, zType, resType, projs) =>
                r(Var(y), zType, scope) {yType =>
                  canonicalRenameToUniqueVar(z, y, resType){appResType =>
                    canonicalRaise(scope, appResType, p)(cont)
                  }
                }
              case CanonicalBot =>
                r(Var(y), CanonicalTop, scope) {yType =>
                  canonicalRaise(scope, CanonicalBot, p)(cont)
                }
              case _ =>
                cont(canonicalError())
            }
          }
        case _ =>
          ???
      }
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
        val incompleteCellsAtTheEnd = Await.result(pool.quiescentIncompleteCells, 10.seconds) // TODO
        if (incompleteCellsAtTheEnd.size != 0) {
          None
        } else {
          await(rootPromise.future)
        }
      } catch {
        case e: TimeoutException => None
      }
    }

    def fullInfer(rootExpr: Term, rootPrototype: CanonicalPrototype = CanonicalQue, rootScope: Map[Symbol, CanonicalType] = Map())(cont: (CanonicalType) => Unit): Unit = {
      alphaRename(rootExpr){rootExpr =>
        r(rootExpr, rootPrototype, rootScope) {rootType =>
          expandCanonicalFutureTypes(rootType) {res =>
            cont(res)
          }
        }
      }
    }
  } // end class TypeChecker

  def pinfer(symbolUniverse: SymbolUniverse, rootExpr: Term, rootPrototype: CanonicalPrototype = CanonicalQue, rootScope: Map[Symbol, CanonicalType] = Map()): Option[CanonicalType] = {
    val pool         = new HandlerPool(1) // TODO
    val typeChecker  = new TypeChecker(symbolUniverse, pool, new AtomicBoolean(false))
    val res = typeChecker.run[CanonicalType]{typeChecker.fullInfer(rootExpr, rootPrototype, rootScope)(_)}
    res
  }
}
