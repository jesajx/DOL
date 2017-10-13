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

object DolSpec extends Properties("DolSpec") {


  // TODO Maybe just define generators like normal function and let caller
  // wrap in Gen as necessary? The problem is distributing the randomseed
  // manually...

  def genScope(su: SymbolUniverse): Gen[Scope] = const(Map()) // TODO

  // TODO should some of these be Cogen rather then Gen?

  def genLowerPrototypeFromType(su: SymbolUniverse, scope: Scope, typ: Type): Gen[Prototype] = typ match {
    case FunType(x, xType, resType) =>
      oneOf(
        const(typ),
        const(Que),
        for {
          xPrototype <- genUpperPrototypeFromType(su, scope, xType)
          resPrototype <- genLowerPrototypeFromType(su, scope, resType)
        } yield FunType(x, xPrototype, resPrototype))
    case _ => oneOf(const(Que), const(typ))
  }
  def genUpperPrototypeFromType(su: SymbolUniverse, scope: Scope, typ: Type): Gen[Prototype] = typ match {
    // TODO obj-types probably need special handling...
    case FunType(x, xType, resType) =>
      oneOf(
        const(typ),
        const(Que),
        for {
          xPrototype <- genLowerPrototypeFromType(su, scope, xType)
          resPrototype <- genUpperPrototypeFromType(su, scope, resType)
        } yield FunType(x, xPrototype, resPrototype))
    case _ => oneOf(const(Que), const(typ))
  }


  def genFunType(su: SymbolUniverse, scope: Scope): Gen[Type] = Gen.sized{ size =>
    if (size < 3)
      Gen.fail
    else for {
      x <- const(su.newSymbol())
      subSize <- size - 1 // The fun-node itself counts as one. 1+argSize+resSize == size.
      argSize <- Gen.choose(1, subSize)
      resSize <- subSize - argSize
      arg <- Gen.resize(argSize, genType(su, scope))
      res <- Gen.resize(resSize, genType(su, scope + (x -> arg)))
    } yield FunType(x, arg, res)
  }


  def genSupertype(su: SymbolUniverse, scope: Scope, subtype: Type, visited: Set[TypeProj]): Gen[Type] = Gen.sized { size =>
    if (size == 0)
      Gen.fail
    else if (size == 1) {
      if (subtype.totNumNodes == 1)
        oneOf(const(subtype), const(Top))
      else
        const(Top)
    } else {
      val extra: Seq[Gen[Type]] = subtype match {
//        case FunType(x, xType, resType) if (size >= 3) =>
//          Seq(for {
//            subSize <- size - 1
//            argSize <- Gen.choose(1, subSize)
//            resSize <- subSize - argSize
//            xSubtype     <- Gen.resize(argSize, genSubtype(su, scope, xType, visited))
//            resSupertype <- Gen.resize(resSize, genSupertype(su, scope + (x -> xSubtype), resType, visited))
//            // TODO supertypes of projs? // remove/keep/supertype
//          } yield FunType(x, xSubtype, resSupertype))
//        case AndType(left, right) if (size >= 3) =>
//          val leftGen = genSupertype(su, scope, left, visited)
//          val rightGen = genSupertype(su, scope, right, visited)
//          Seq(oneOf(leftGen, rightGen)) // TODO also and(leftGen, rightGen)? beware of infinite recursion...
//        case RecType(x, xType) if (size >= 2) =>
//          val z = su.newSymbol()
//          Seq(for {
//            xSupertype <- genSupertype(su, scope + (x -> xType), xType, visited)
//          } yield RecType(z, NoFuture.varEliminatingTransform(su, scope + (x -> xType) + (z -> xSupertype), x, z, xSupertype)))
//        case aProj @ TypeProj(x, a) if !visited(aProj) => // TODO visitedSet
//          NoFuture.typeProjectUpper(su, scope, x, a) match {
//            case Some(aUpperType) => Seq(genSupertype(su, scope, aUpperType, visited + aProj))
//            case None => ???; Seq()
//          }
//        case FieldDecl(a, aType) =>
//          Seq(for {
//            aSupertype <- genSupertype(su, scope, aType, visited)
//          } yield FieldDecl(a, aSupertype))
//        case TypeDecl(a, aLowerType, aUpperType) =>
//          Seq(for {
//            aSubtype <- genSubtype(su, scope, aLowerType, visited)
//            aSupertype <- genSupertype(su, scope, aUpperType, visited)
//          } yield TypeDecl(a, aSubtype, aSupertype))
//        case Bot =>
//          val x = su.newSymbol()
//          Seq(genSupertype(su, scope, FunType(x, Top, Bot), visited))
        case _ =>
          Seq()
      }
      // TODO and(self, super(self))? and(super(self), super(self))?

      oneOf(const(subtype), const(Top), extra: _*)
    }
  }

  def genSubtype(su: SymbolUniverse, scope: Scope, supertype: Type, visited: Set[TypeProj]): Gen[Type] = Gen.sized { size =>
    if (size == 0)
      Gen.fail
    else if (size <= 10) { // TODO bad!!! needs to be a bit bigger.
      if (supertype.totNumNodes == 1)
        oneOf(const(supertype), const(Bot))
      else
        const(Bot)
    } else {
      val extra: Seq[Gen[Type]] = supertype match {
//        case FunType(x, xType, resType) if (size >= 3) =>
//          Seq(for {
//            subSize <- size - 1
//            argSize <- Gen.choose(1, subSize)
//            resSize <- subSize - argSize
//            // TODO is this correct or is it necessary to do a
//            // varEliminatingTransform or similar?
//            xSupertype <- Gen.resize(argSize, genSupertype(su, scope, xType, visited))
//            resSubtype <- Gen.resize(resSize, genSupertype(su, scope + (x -> xSupertype), resType, visited))
//            // TODO supertypes of projs? // remove/keep/supertype
//          } yield FunType(x, xSupertype, resSubtype))
//        case AndType(left, right) if (size >= 3) =>
//          Seq(for {
//            leftGen <- genSupertype(su, scope, left, visited)
//            rightGen <- genSupertype(su, scope, right, visited)
//          } yield AndType(leftGen, rightGen))
//        case RecType(x, xType) if (size >= 2) =>
//          Seq(for {
//            xSubtype <- genSubtype(su, scope + (x -> xType), xType, visited)
//          } yield RecType(x, xSubtype))
//        case aProj @ TypeProj(x, a) if !visited(aProj) => // TODO visitedSet
//          NoFuture.typeProjectLower(su, scope, x, a) match {
//            case Some(aLowerType) => Seq(genSubtype(su, scope, aLowerType, visited + aProj))
//            case None => ???; Seq()
//          }
//        case FieldDecl(a, aType) =>
//          Seq(for {
//            aSubtype <- genSubtype(su, scope, aType, visited)
//          } yield FieldDecl(a, aSubtype))
//        case TypeDecl(a, aLowerType, aUpperType) =>
//          Seq() // TODO Need to generate tighter interval without getting into the situation where upper <: lower.
//        case Top =>
//          val x = su.newSymbol()
//          Seq(genSubtype(su, scope, FunType(x, Bot, Top), visited))
        case _ =>
          Seq()
      }

      oneOf(const(supertype), const(Bot), extra: _*)
    }
  }

  def chooseOneOf[T](gs: Seq[Gen[T]]): Gen[T] = gs.size match{
    case 0 => Gen.fail
    case 1 => gs(0)
    case _ => Gen.oneOf(gs(0), gs(1), gs.drop(2): _*)
  }

//  def genCanonicalDecl(su: SymbolUniverse, scope: Scope): Gen[(FieldDeclMap, TypeDeclMap, Set[TypeProj])] = Gen.sized{ size =>
//    val a = su.newSymbol()
//    val genField = for {
//      aType <- Gen.resize(size, genCanonicalType(su, scope))
//    } yield (Map(a -> aType), Map(), Set()): (FieldDeclMap, TypeDeclMap, Set[TypeProj])
//
//    val possibleProjs = scope.flatMap{
//      case (x, CanonicalObjType(_, _, types, _)) =>
//        types.keys.map{a => TypeProj(x, a)} // TODO only use a if lower(a) is object?
//      case _ => Seq()
//    }
//    val genProj = for {
//      n <- const(possibleProjs.size) if n != 0
//      proj <- oneOf(possibleProjs.toSeq)
//    } yield (Map(), Map(), Set(proj)): (FieldDeclMap, TypeDeclMap, Set[TypeProj])
//
//    val genType = for {
//      n <- const(size) if n >= 2
//      upperSize <- Gen.choose(1, size-1)
//      lowerSize <- size - upperSize
//      aUpperType <- Gen.resize(upperSize, genCanonicalType(su, scope))
//      aLowerType <- Gen.resize(lowerSize, genCanonicalSubtype(su, scope, aUpperType))
//    } yield (Map(), Map(a -> (aLowerType, aUpperType)), Set()): (FieldDeclMap, TypeDeclMap, Set[TypeProj])
//
//
//    chooseOneOf(
//      Seq(genProj).filter{_ => possibleProjs.size != 0}
//      ++ Seq(genType).filter{_ => size >= 2}
//      ++ Seq(genField))
//  }
//
//  def genManyCanonicalDecls(su: SymbolUniverse, scope: Scope, z: Symbol): Gen[(FieldDeclMap, TypeDeclMap, Set[TypeProj])] = Gen.sized { size =>
//    if (size < 2) {
//      genCanonicalDecl(su, scope)
//    } else {
//      oneOf(
//        genCanonicalDecl(su, scope),
//        for {
//          numLeft <- Gen.choose(1, size-1) // NOTE: non-empty.
//          numRight <- const(size - numLeft)
//          (leftFields, leftTypes, leftProjs) <- Gen.resize(numLeft, genManyCanonicalDecls(su, scope, z))
//          rightScope <- const(scope + (z -> CanonicalObjType(z, leftFields, leftTypes, leftProjs)))
//          (rightFields, rightTypes, rightProjs) <- Gen.resize(numRight, genManyCanonicalDecls(su, rightScope, z))
//        } yield (leftFields ++ rightFields, leftTypes ++ rightTypes, leftProjs ++ rightProjs)
//      )
//    }


//    if (size <= 0) {
//      const(Map(), Map())
//    } else if (size == 1) {
//      genCanonicalDecl(su, scope)
//    } else {
//      for {
//        numLeft <- Gen.choose(1, size-1) // NOTE: non-empty.
//        numRight <- const(size - numLeft)
//        (leftFields, leftTypes) <- Gen.resize(numLeft, genManyCanonicalDecls(su, scope, z))
//        rightScope <- const(scope + (z -> CanonicalObjType(z, leftFields, leftTypes, Set())))
//        (rightFields, rightTypes) <- Gen.resize(numRight, genManyCanonicalDecls(su, rightScope, z))
//      } yield (leftFields ++ rightFields, leftTypes ++ rightTypes)
//    }
//  }

  // TODO first figure out number of fields,types, then generate names, then generate types?
//  def genObjType(su: SymbolUniverse, scope: Scope): Gen[Type] = Gen.sized{ size =>
//    for {
//      x <- const(su.newSymbol())
//
//      (fields, types, projs) <- Gen.resize(size - 1, genManyCanonicalDecls(su, scope, x))
//      // TODO also generate decls that are mutually recursive, by first generating types and then generating terms with those types.
//
//    } yield CanonicalObjType(x, fields, types, projs.toSet)
//  }

  // TODO easier to generate normal types? genCanonicalType = canonicalize(genType)?

  // TODO generate non-canonical types. e.g. with intersections of multiple decls on the same member.
  def genType(su: SymbolUniverse, scope: Scope): Gen[Type] = Gen.sized { size =>
    if (size == 0)
      Gen.fail
    else if (size == 1)
      oneOf(const(Top), const(Bot))
    else
      //oneOf(const(Top), const(Bot), genFunType(su, scope), genObjType(su, scope)) // TODO
      oneOf(const(Top), const(Bot), genFunType(su, scope))
  }

//  def nextSubtype(su: SymbolUniverse, scope: Scope, tp: CanonicalType): Stream[CanonicalType] = tp match {
//    case CanonicalFunType(x, Top, Bot, EmptySet()) =>
//      Some(Bot)
//    case CanonicalFunType(x, xType, resType, projs) =>
//      val shrinkArg = for {
//        xSuperType <- nextSupertype(xType)
//      } yield CanonicalFunType(x, xSuperType, resType, projs)
//      val shrinkRes = for {
//        resSubType <- nextSubtype(resType)
//      } yield CanonicalFunType(x, xType, resSubType, projs)
//      val shrinkProjs = for {
//        newProjs <- someOf(projs)
//      } yield CanonicalFunType(x, xType, resType, newProjs)
//      shrinkArg #::: shrinkRes #::: shrinkProjs
//    case Bot =>
//      Stream()
//    case _ =>
//      Stream(Bot)
//  }
//
//  def nextSupertype(su: SymbolUniverse, scope: Scope, tp: CanonicalType): Stream[CanonicalType] = tp match {
//    case CanonicalFunType(x, Bot, Top, EmptySet()) =>
//      Some(Top)
//    case CanonicalObjType(x, fields, types, projs) =>
//      val shrinkFieldsByRemoving = for {
//        newFields <- someOf(fields) if newFields.size < fields.size // TODO only remove one?
//      } yield CanonicalObjType(x, newFields, types, projs)
//      val shrinkFieldsBySuperTyping = for {
//        newFields <- fields.map{case (a, aType) =>
//          ???
//        }
//      } yield CanonicalObjType(x, newFields, types, projs)
//      val shrinkTypesByRemoving = for {
//        newTypes <- someOf(types) if newTypes.size < types.size // TODO check if someone is using them...
//      } yield CanonicalObjType(x, newFields, newTypes, projs)
//      val shrinkProjsByRemoving = for {
//        newProjs <- someOf(projs) if newProjs.size < projs.size
//      } yield CanonicalObjType(x, newFields, types, newProjs)
//    //case CanonicalFunType(x, xType, resType, projs) =>
//    case _ =>
//      None
//  }

  //def shrinkCanonicalType(su: SymbolUniverse, scope: Scope): Shrink[CanonicalType] = Shrink { typ =>
  //  for {
  //    subtype <- nextSubtype(typ)
  //  } yield Stream(subtype)
  //}



  // TODO maybe nest InferenceProblems instead of terms? can always translate
  // to term.
  sealed case class InferenceProblem(term: Term, prototype: Prototype, scope: Scope, expected: Term) {
    if (!isTermFullyTyped(expected)) {
      throw new Exception(s"not fully typed: $expected")
    }
    // TODO assert properly alpha-renamed
    override def toString() = {
      s"InferenceProblem($term, $prototype, $scope, ${NoFuture.stringExprWithTypeIfExists(expected)})"
    }
    def expectedType = expected.assignedType
  }

  def genVarInferenceProblem(su: SymbolUniverse, scope: Scope): Gen[InferenceProblem] = {
    val justMakeSomethingUp = for {
      x <- const(su.newSymbol())
      xType <- genType(su, scope)
    } yield InferenceProblem(Var(x), Que, scope + (x -> xType), Var(x).withType(xType))
    if (scope.isEmpty) {
      justMakeSomethingUp
    } else {
      val pickSomethingFromScope = for {
        x <- oneOf(scope.keys.toSeq)
        xType <- scope(x)
      } yield InferenceProblem(Var(x), Que, scope, Var(x).withType(xType))
      oneOf(justMakeSomethingUp, pickSomethingFromScope)
    }
  }

  def genLetInferenceProblem(su: SymbolUniverse, scope: Scope): Gen[InferenceProblem] = for {
    x <- const(su.newSymbol)
    p1 <- genInferenceProblem(su, scope)
    p2 <- genInferenceProblem(su, scope + (x -> p1.expectedType))
    resType <- NoFuture.eliminateVarUp(su, scope, x, p2.expectedType, Set())
  } yield InferenceProblem(Let(x, p1.term, p2.term), p2.prototype, (p1.scope ++ p2.scope) - x, Let(x, p1.expected, p2.expected).withType(resType))

  def genSelInferenceProblem(su: SymbolUniverse, scope: Scope): Gen[InferenceProblem] = { // TODO Gen.sized
    val justMakeSomethingUp = for {
      x <- const(su.newSymbol())
      a <- const(su.newSymbol())
      aType <- genType(su, scope)
      xType <- const(FieldDecl(a, aType))
    } yield InferenceProblem(Var(x), Que, scope + (x -> xType), Var(x).withType(xType))
    val fieldRefs = NoFuture.directFieldDeclsInScope(su, scope)
    if (fieldRefs.isEmpty) {
      justMakeSomethingUp
    } else {
      val pickSomethingFromScope = for {
        (Seq(x, a), aType) <- oneOf(fieldRefs.toSeq)
      } yield InferenceProblem(Sel(x, a), Que, scope, Sel(x, a).withType(aType))
      oneOf(justMakeSomethingUp, pickSomethingFromScope)
    }
  }


  // TODO def genObjInferenceProblem(su: SymbolUniverse, scope: Scope): Gen[InferenceProblem] // TODO generate fun then def, or vice versa?
  // TODO def genAppInferenceProblem(su: SymbolUniverse, scope: Scope): Gen[InferenceProblem] // TODO search for funs in scope and generate term from argtype // TODO gen arg, gen super of arg.type, gen function taking super giving {gen term with type}

  //def genAppInferenceProblemFromScope(su: SymbolUniverse, scope: Scope): Gen[InferenceProblem] = // TODO find function in scope (possibly as field in object), generate term with that argtype

  //def genTermFromType


//  def genFieldDef(su: SymbolUniverse, scope: Scope): Gen[(Symbol, InferenceProblem)] = Gen.sized{ size =>
//    for {
//      a <- const(su.newSymbol())
//      p <- Gen.resize(size-1, genInferenceProblem(su, scope))
//    } yield (a, InferenceProblem(p.term, p.prototype, p.scope, p.expected))
//  }
//
//  def genTypeDef(su: SymbolUniverse, scope: Scope): Gen[(Symbol, CanonicalType)] = Gen.sized{ size =>
//    for {
//      a <- const(su.newSymbol())
//      aType <- Gen.resize(size - 1, genCanonicalType(su, scope))
//    } yield (a, aType)
//  }
//
//  def genDef(su: SymbolUniverse, scope: Scope): Gen[(Map[Symbol, InferenceProblem], Map[Symbol, CanonicalType])] = oneOf(
//    for {
//      (a, p) <- genFieldDef(su, scope)
//    } yield (Map(a -> p), Map()): (Map[Symbol, InferenceProblem], Map[Symbol, CanonicalType]),
//    for {
//      (a, aType) <- genTypeDef(su, scope)
//    } yield (Map(), Map(a -> aType)): (Map[Symbol, InferenceProblem], Map[Symbol, CanonicalType])
//  )
//
//  def genManyDefs(su: SymbolUniverse, scope: Scope, z: Symbol): Gen[(Map[Symbol, InferenceProblem], Map[Symbol, CanonicalType])] = Gen.sized { size =>
//    if (size <= 0)
//      const(Map(), Map())
//    else if (size == 1)
//      genDef(su, scope)
//    else for {
//      numLeft <- Gen.choose(1, size-1) // NOTE: non-empty.
//      numRight <- const(size - numLeft)
//      (leftFieldMap, leftTypeMap) <- Gen.resize(numLeft, genManyDefs(su, scope, z))
//
//      leftFields <- const(leftFieldMap.map{case (a, p) => (a, p.expectedType)})
//      leftTypes <- const(leftTypeMap.map{case (a, aType) => (a, (aType, aType))})
//
//      rightScope <- const(scope + (z -> CanonicalObjType(z, leftFields, leftTypes, Set())))
//      (rightFieldMap, rightTypeMap) <- Gen.resize(numRight, genManyDefs(su, rightScope, z))
//    } yield (leftFieldMap ++ rightFieldMap, leftTypeMap ++ rightTypeMap)
//  }
//
//  // TODO genComplicatedObjInferenceProblem with scary recursive dependencies between fields, types and typeprojs.
//
//  def genObjInferenceProblem(su: SymbolUniverse, scope: Scope): Gen[InferenceProblem] = Gen.sized{ size =>
//    for {
//      x <- const(su.newSymbol())
//      (fields, types) <- Gen.resize(size / 2, genManyDefs(su, scope, x)) if fields.size + types.size >= 1
//
//      fieldDefs <- const(fields.map{case (a, p) => FieldDef(a, p.term)}) // TODO prototypes
//      typeDefs <- const(types.map{case (a, aType) => TypeDef(a, NoFuture.decanonicalize(aType))})
//      defs <- const((fieldDefs ++ typeDefs).reduce(AndDef(_, _)))
//
//      newScope <- const(scope ++ fields.flatMap{case (a, p) => p.scope})
//
//      typedFieldDefs <- const(fields.map{case (a, p) => FieldDef(a, p.expected)})
//      typedDefs <- const((typedFieldDefs ++ typeDefs).reduce(AndDef(_, _)))
//
//      fieldDecls <- const(fields.map{case (a, p) => FieldDecl(a, NoFuture.decanonicalize(p.expectedType))})
//      typeDecls <- const(types.map{case (a, aType) => TypeDecl(a, NoFuture.decanonicalize(aType), NoFuture.decanonicalize(aType))})
//      xType <- const(RecType(x, (fieldDecls ++ typeDecls).reduce(AndType(_, _))))
//
//      canonicalFields <- const(fields.map{case (a, p) => (a, p.expectedType)})
//      canonicalTypes <- const(types.map{case (a, aType) => (a, (aType, aType))})
//      projs <- const(Set[TypeProj]()) // TODO add some?
//      xCanonicalType <- const(CanonicalObjType(x, canonicalFields, canonicalTypes, projs))
//    } yield InferenceProblem(Obj(x, xType, defs), Que, newScope - x, Obj(x, xType, typedDefs).withType(xCanonicalType))
//  }
//

  // TODO genInferenceProblemFromType: Modify genInferenceProblem to take a prototype and use fixed-point
  // to eliminate the prototype?

  def genAppInferenceProblem(su: SymbolUniverse, scope: Scope): Gen[InferenceProblem] = {
    val genFromArg = for { // TODO better to add fun to scope?
      f <- const(su.newSymbol())
      x <- const(su.newSymbol())
      y <- const(su.newSymbol())
      arg <- genInferenceProblem(su, scope)
      argSupertype <- genSupertype(su, scope, arg.expectedType, Set())
      res <- genInferenceProblem(su, scope + (x -> argSupertype))
      resType <- const(res.expectedType)
    } yield InferenceProblem(
      Let(f, Fun(x, argSupertype, res.term), Let(y, arg.term, App(f, y))),
      res.prototype,
      (arg.scope ++ res.scope) - x - f - y,
      Let(f,
        Fun(x, argSupertype, res.expected).withType(FunType(x, argSupertype, resType)),
        Let(y, arg.expected, App(f, y).withType(resType)).withType(resType)
        ).withType(resType))

    var funsInScope = scope.filter{
      case (_, _: FunType) => true
      case _ => false
    }

    if (funsInScope.size == 0) {
      genFromArg
    } else {
      val genFromScope = for {
        (f, FunType(x, xType, xResType)) <- oneOf(funsInScope.toSeq)
        y <- const(su.newSymbol()) // TODO vs picking y from scope?
        yType <- genSubtype(su, scope, xType, Set())
        appResType <- const(NoFuture.typeRenameVar(x, y, xResType))
      } yield InferenceProblem(App(f, y), Que, scope + (y -> yType), App(f, y).withType(appResType))
      oneOf(genFromArg, genFromScope)
    }
  }

    // TODO also: genAppInferenceProblemFromScope

  def genFunInferenceProblem(su: SymbolUniverse, scope: Scope): Gen[InferenceProblem] = for {
    x         <- const(su.newSymbol)
    xType     <- genType(su, scope)
    body      <- genInferenceProblem(su, scope + (x -> xType))
    prototype <- const(if (body.prototype == Que) Que else FunType(x, Que, body.prototype))
  } yield InferenceProblem(Fun(x, xType, body.term), prototype, body.scope - x, Fun(x, xType, body.expected).withType(FunType(x, xType, body.expectedType)))

//  def genFixedPointInferenceProblem(su: SymbolUniverse, scope: Scope): Gen[InferenceProblem] = for { // TODO use LightweightApp instead.
//    f <- const(su.newSymbol)
//    x <- const(su.newSymbol)
//    y <- const(su.newSymbol)
//    p <- genInferenceProblem(su, scope)
//    typ <- const(p.expectedType)
//  } yield InferenceProblem(
//    Let(f, Fun(x, NoFuture.decanonicalize(typ), Var(x)), Let(y, p.term, App(f, y))),
//    Que,
//    p.scope - f - x - y,
//    Let(f,
//      Fun(x, NoFuture.decanonicalize(typ), Var(x).withType(typ)).withType(CanonicalFunType(x, typ, typ, Set())),
//      Let(y, p.expected, App(f, y).withType(typ)).withType(typ)).withType(typ))

  def genInferenceProblem(su: SymbolUniverse, scope: Scope): Gen[InferenceProblem] = oneOf(
    genVarInferenceProblem(su, scope),
    genLetInferenceProblem(su, scope),
    genSelInferenceProblem(su, scope),
    genFunInferenceProblem(su, scope),
    genAppInferenceProblem(su, scope)//,
    //genObjInferenceProblem(su, scope),
    //genFixedPointInferenceProblem(su, scope)
  )

  def isTermFullyTyped(term: Term): Boolean = {
    if (term.assignedTypeOption == None) {
      return false
    }
    term match {
      case Let(x, xTerm, resTerm) =>
        isTermFullyTyped(xTerm) && isTermFullyTyped(resTerm)
      case Obj(x, xType, d) =>
        NoFuture.defAsMap(d).values.map{
          case FieldDef(a, aTerm) => isTermFullyTyped(aTerm)
          case _ => true
        }.reduce{_ && _}
      case Fun(x, xType, resTerm) =>
        isTermFullyTyped(resTerm)
      case _ => true // Var, App, Sel
    }
  }

  def shrinkScopeIfPossible(problem: InferenceProblem): InferenceProblem = {

    def bfs(usedVars: Set[Symbol]): Set[Symbol] = {
      val reachable = usedVars.flatMap{x =>
        NoFuture.freeVarsInType(problem.scope(x))
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
      case InferenceProblem(Sel(x, a), prototype, scope, expected) =>
        NoFuture.select(su, scope, x, a) match {
          case Some(aType) =>
            val w = su.newSymbol()
            val newScope = scope + (w -> aType)
            Stream(InferenceProblem(Var(w), prototype, newScope, Var(w).withType(aType)))
          case None =>
            println(s"problem= $problem")
            ???; Stream()
        }
      case _ => Stream()
    })

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

  // TODO include SymbolUniverse as field in InferenceProblem?
  val genSUAndInferenceProblem: Gen[(SymbolUniverse, InferenceProblem)] = for{
    su <- Gen.sized(_ => new SymbolUniverse())
    scope <- const(Map(): Scope) // TODO genScope?
    problem <- genInferenceProblem(su, scope)
  } yield (su, problem)
  def shrinkSUAndInferenceProblem(tuple: (SymbolUniverse, InferenceProblem)): Stream[(SymbolUniverse, InferenceProblem)] = {
    val (su, problem) = tuple
    shrinkInferenceProblem(su, problem, isRoot=true).map{newProblem => (su, newProblem)}
  }

  property("positiveSequentialInferenceProblem") = {
    Prop.forAllShrink(genSUAndInferenceProblem, shrinkSUAndInferenceProblem) { case (su, problem) =>
      val res = typecheckSequentially(su, problem.term, problem.prototype, problem.scope)

      val resString = res match {
        case Some(resTerm) => s"res = Some(${NoFuture.stringExprWithTypeIfExists(resTerm)})"
        case None => "res = None"
      }

      resString |: Prop.all(
        res != None && NoFuture.equalTerms(problem.scope, res.get, problem.expected))
    }
  }

  // TODO
//  property("positiveParallelInferenceProblem") = {
//    val su = new SymbolUniverse()
//    val generator: Gen[InferenceProblem] = genInferenceProblem(su, Map())
//    val shrink: InferenceProblem => Stream[InferenceProblem] = shrinkInferenceProblem(su, _, isRoot=true)
//    Prop.forAllShrink(generator, shrink) { (problem: InferenceProblem) =>
//      val res = typecheckInParallel(su, problem.term, problem.prototype, problem.scope)
//
//      val resString = res match {
//        case Some(resTerm) => s"res = Some(${NoFuture.stringExprWithTypeIfExists(resTerm)})"
//        case None => "res = None"
//      }
//
//      resString |: Prop.all(
//        res != None && NoFuture.equalTerms(su, res.get, problem.expected))
//    }
//  }

  //property("genCanonicalTypeSizeIsConsistentWithDolNotionOfSize") = { // TODO better name
  //  // TODO check that if we run a Gen[CanonicalType]-generator with size N,
  //  // the resulting generate type has _.totNumNodes <= N
  //}

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

  property("NoFuture.projectUpperType") = {
    val su = new SymbolUniverse()
    val generator: Gen[(Scope, Symbol, Symbol, Type)] = for {
      x <- const(su.newSymbol())
      a <- const(su.newSymbol())
      aUpperType <- genType(su, Map()) // TODO populate scope?
      aLowerType <- genSubtype(su, Map(), aUpperType, Set()) // TODO populate scope?
      scope <- const(Map(x -> TypeDecl(a, aLowerType, aUpperType)))
    } yield (scope, x, a, aUpperType)
    // TODO test multiple declarations (should result in AndType)
    Prop.forAllNoShrink(generator) {case (scope, x, a, aUpperType) =>
      val res = NoFuture.typeProjectUpper(scope, x, a)
      s"res = $res" |: Prop.all(res != None && NoFuture.equalTypes(scope, res.get, aUpperType))
    }
  }

  property("NoFuture.raise -- simply fill in gaps") = { // TODO vs testing constraintSolver directly?
    val su = new SymbolUniverse()
    val scope: Scope = Map()
    val generator: Gen[(Type, Prototype)] = for {
      typ <- genType(su, scope)
      prototype <- genUpperPrototypeFromType(su, scope, typ)
    } yield (typ, prototype)

    Prop.forAllNoShrink(generator) {case (typ, prototype) =>
      val res = NoFuture.raise(su, scope, typ, prototype)
      s"res = $res" |: Prop.all(res != None && NoFuture.equalTypes(scope, res.get, typ))
    }
  }

  property("NoFuture.lower -- simply fill in gaps") = { // TODO vs testing constraintSolver directly?
    val su = new SymbolUniverse()
    val scope: Scope = Map()
    val generator: Gen[(Type, Prototype)] = for {
      typ <- genType(su, scope)
      prototype <- genLowerPrototypeFromType(su, scope, typ)
    } yield (typ, prototype)

    Prop.forAllNoShrink(generator) {case (typ, prototype) =>
      val res = NoFuture.lower(su, scope, typ, prototype)
      s"res = $res" |: Prop.all(res != None && NoFuture.equalTypes(scope, res.get, typ))
    }
  }

  def withSymbolUniverse[T](f: SymbolUniverse => Gen[T]): Gen[(SymbolUniverse, T)] = for {
    su <- const(new SymbolUniverse())
    res <- f(su)
  } yield (su, res)

  property("NoFuture.allVarsInType") = {
    val generator: Gen[(SymbolUniverse, Type)] = withSymbolUniverse{genType(_, Map())}
    Prop.forAllNoShrink(generator) {case (su, typ) =>
      val res = NoFuture.allVarsInType(typ)
      val expected = (0 until su.count()).toSet
      s"res = $res, expected = $expected" |: Prop.all(res == expected)
    }
  }

  property("NoFuture.allVarsInType - boundVarsInType == freeVarsInType") = {
    val generator: Gen[(SymbolUniverse, Type)] = withSymbolUniverse{genType(_, Map())}
    Prop.forAllNoShrink(generator) {case (su, typ) =>
      val expected = (0 until su.count()).toSet
      val allVars = NoFuture.allVarsInType(typ)
      val boundVars = NoFuture.boundVarsInType(typ)
      val freeVars = NoFuture.freeVarsInType(typ)

      s"expected = $expected, allVars = $allVars, boundVars = $boundVars, freeVars = $freeVars" |: Prop.all(
        allVars == expected,
        boundVars.intersect(freeVars) == Set(),
        allVars -- boundVars == freeVars
      )
    }
  }

  property("NoFuture.isSubtypeOf") = {
    val generator: Gen[(Scope, Type, Type)] = for {
      su <- const(new SymbolUniverse())
      scope <- genScope(su)
      lower <- genType(su, scope)
      upper <- genSupertype(su, scope, lower, Set())
    } yield (scope, lower, upper)
    Prop.forAllNoShrink(generator) { case (scope, lower, upper) =>
      NoFuture.isSubtypeOf(scope, lower, upper)
    }
  }

  // TODO to be more FP-pure, symboluniverse should probably be wrapped
  // together with other stuff in a monad...

  property("NoFuture.typeRenameVar") = {
    val generator: Gen[(SymbolUniverse, Symbol, Symbol, Type)] = for {
      su <- const(new SymbolUniverse())
      typ <- genType(su, Map())
      x <- Gen.oneOf((NoFuture.freeVarsInType(typ) + su.newSymbol()).toSeq) // NOTE: x != y. Had to check scalacheck src to know that....
      y <- Gen.oneOf(((NoFuture.freeVarsInType(typ) + x) + su.newSymbol()).toSeq)
    } yield (su, x, y, typ)
    // TODO shrink type?
    Prop.forAllNoShrink(generator) { case (su, x, y, typ) =>
      val freeBefore = NoFuture.freeVarsInType(typ)
      val res = NoFuture.typeRenameVar(x, y, typ)
      val freeMiddle = NoFuture.freeVarsInType(res)
      val res2 = NoFuture.typeRenameVar(y, x, res)
      val freeAfter = NoFuture.freeVarsInType(res2)

      Prop.all(
        !(x != y && freeBefore(x)) || (!freeMiddle(x) && freeMiddle(y)),
        !(x != y && freeMiddle(y)) || (!freeAfter(y) && freeAfter(x)),
        !(x != y && freeBefore(x) && !freeBefore(y)) || (typ == res2),
        !(x == y) || (typ == res && res == res2)
      )
    }
  }

  // TODO check utility-functions. raise, lower, glb, lub, etc.

  // TODO check eliminateVarUp by starting with a type with no vars, and then
  // replacing parts with vars.  scope={},Int  --> scope={x->{T=Int}},x.T
}

/*
*/
