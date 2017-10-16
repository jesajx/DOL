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


  def genFunType(su: SymbolUniverse, scope: Scope): Gen[(Scope, Type)] = Gen.sized{ size =>
    if (size < 3)
      Gen.fail
    else for {
      x <- const(su.newSymbol())
      subSize <- size - 1 // The fun-node itself counts as one. 1+argSize+resSize == size.
      argSize <- Gen.choose(1, subSize)
      resSize <- subSize - argSize
      (argScope, arg) <- Gen.resize(argSize, genType(su, scope))
      (resScope, res) <- Gen.resize(resSize, genType(su, argScope + (x -> arg)))
    } yield (resScope - x, FunType(x, arg, res))
  }


  def genSupertype(su: SymbolUniverse, scope: Scope, subtype: Type, visited: Set[TypeProj] = Set()): Gen[Type] = Gen.sized { size => // TODO Gen[(Scope,Type)]?
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

  def genSubtype(su: SymbolUniverse, scope: Scope, supertype: Type, visited: Set[TypeProj] = Set()): Gen[Type] = Gen.sized { size => // TODO Gen[(Scope,Type)]?
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

  def genTypeProj(su: SymbolUniverse, scope: Scope): Gen[(Scope, TypeProj)] = Gen.sized{ size =>
    // TODO NOTE: We run the risk of adding spilling the local scope into the
    // global scope when  we do this. E.g. RecType(x, y.T), y ->
    // TypeDecl(...., x.T).
    // TODO when generating types, we need to differentiate localscope and
    // global scope.
    //
    //val justMakeSomethingUp = for { // TODO sizes...
    //  x <- const(su.newSymbol())
    //  a <- const(su.newSymbol())
    //  (scope2, aUpperType) <- genType(su, scope)
    //  aLowerType <- genSubtype(su, scope2, aUpperType)
    //  decl <- const(TypeDecl(a, aLowerType, aUpperType))
    //  declSubtype <- genSubtype(su, scope2, decl)
    //} yield (scope2 + (x -> declSubtype), TypeProj(x, a))
    // TODO pick object from scope...

    val typeMembersInScope = scope.flatMap{case (x, xType) =>
      NoFuture.allDirectTypeMembers(scope, xType, Set()).map{
        case a => TypeProj(x, a)
      }
    }.toSet.toSeq

    if (typeMembersInScope.isEmpty) {
      Gen.fail //justMakeSomethingUp
    } else {
      val grabSomethingFromScope = for {
        p <- oneOf(typeMembersInScope)
      } yield (scope, p)
      //oneOf(justMakeSomethingUp, grabSomethingFromScope)
      grabSomethingFromScope
    }
  }

  def genSimpleDecls(su: SymbolUniverse, scope: Scope, z: Symbol): Gen[Scope] = Gen.sized{ size =>
    val genFieldDecl = for {
      n <- const(size) if n >= 2 // 1 for FieldDecl-node and 1 for aTerm.
      a <- const(su.newSymbol())
      (scope2, aType) <- Gen.resize(n-1, genType(su, scope))
      myType     <- const(FieldDecl(a, aType))
    } yield scopePush(scope2, z, myType)

    val genTypeDecl = for {
      n   <- const(size) if n >= 3 // 1 for TypeDecl-node, 1 for aLowerType and 1 for aUpperType.
      sub <- const(n - 1)
      upperSize <- Gen.choose(1, sub-1)
      lowerSize <- sub - upperSize
      (scope2, aUpperType) <- Gen.resize(upperSize, genType(su, scope))
      aLowerType <- Gen.resize(lowerSize, genSubtype(su, scope2, aUpperType))
      a <- const(su.newSymbol())
      myType <- const(TypeDecl(a, aLowerType, aUpperType))
    } yield scopePush(scope2, z, myType)

    val genDeclIntersection = for {
      n   <- const(size) if n >= 5 // 1 for AndDef and 2 for each side (since FieldDef and TypeDef needs atleast 2 each).
      sub <- const(n - 1)
      leftSize   <- Gen.choose(2, sub-2)
      rightSize  <- const(sub - leftSize)
      leftScope   <- Gen.resize(leftSize, genSimpleDecls(su, scope, z))
      rightScope  <- Gen.resize(rightSize, genSimpleDecls(su, leftScope, z))
    } yield rightScope

    if (size >= 5)
      oneOf(genFieldDecl, genTypeDecl, genDeclIntersection)
    else if (size >= 3)
      oneOf(genFieldDecl, genTypeDecl)
    else if (size >= 2)
      genFieldDecl
    else
      Gen.fail
  }

  def scopePush(scope: Scope, x: Symbol, anotherXType: Type): Scope = scope.get(x) match {
    case Some(oldXType) => scope + (x -> AndType(oldXType, anotherXType))
    case None           => scope + (x -> anotherXType)
  }


  def genNonRecObjType(su: SymbolUniverse, scope: Scope): Gen[(Scope, Type)] = Gen.sized{ size =>
    val genFieldDecl = for {
      n <- const(size) if n >= 2 // 1 for FieldDecl-node and 1 for aTerm.
      a <- const(su.newSymbol())
      (scope2, aType) <- Gen.resize(n-1, genType(su, scope))
      myType     <- const(FieldDecl(a, aType))
    } yield (scope2, myType)

    val genTypeDecl = for {
      n   <- const(size) if n >= 3 // 1 for TypeDecl-node, 1 for aLowerType and 1 for aUpperType.
      sub <- const(n - 1)
      upperSize <- Gen.choose(1, sub-1)
      lowerSize <- sub - upperSize
      (scope2, aUpperType) <- Gen.resize(upperSize, genType(su, scope))
      aLowerType <- Gen.resize(lowerSize, genSubtype(su, scope2, aUpperType))
      a <- const(su.newSymbol())
      myType <- const(TypeDecl(a, aLowerType, aUpperType))
    } yield (scope2, myType)

    val genDeclIntersection = for {
      n   <- const(size) if n >= 5 // 1 for AndDef and 2 for each side (since FieldDef and TypeDef needs atleast 2 each).
      sub <- const(n - 1)
      leftSize   <- Gen.choose(2, sub-2)
      rightSize  <- const(sub - leftSize)
      (leftScope, leftType)   <- Gen.resize(leftSize, genNonRecObjType(su, scope))
      (rightScope, rightType)  <- Gen.resize(rightSize, genNonRecObjType(su, leftScope))
    } yield (rightScope, AndType(leftType, rightType))

    if (size >= 5)
      oneOf(genFieldDecl, genTypeDecl, genDeclIntersection)
    else if (size >= 3)
      oneOf(genFieldDecl, genTypeDecl)
    else if (size >= 2)
      genFieldDecl
    else
      Gen.fail
  }

  def genSimpleRecObjType(su: SymbolUniverse, scope: Scope): Gen[(Scope, Type)] = Gen.sized{ size =>
    // TODO subtract 1 from size for rectype?
    for {
      z <- const(su.newSymbol())
      scope2 <- genSimpleDecls(su, scope, z)
    } yield (scope2 - z, RecType(z, scope2(z)))
  }


  // TODO genTypeWithoutScope and genType

  // TODO easier to generate normal types? genCanonicalType = canonicalize(genType)?

  // TODO genAnyType: version of genType that may generate recursive types.

  // TODO allow genType to add types to scope? Gen[(Type, updatedScope)]
  def genType(su: SymbolUniverse, scope: Scope): Gen[(Scope, Type)] = Gen.sized { size =>
    if (size <= 0)
      Gen.fail
    else if (size <= 1)
      oneOf(const((scope, Top)), const((scope, Bot)), genTypeProj(su, scope))
    else if (size <= 2)
      oneOf(const((scope, Top)), const((scope, Bot)), genTypeProj(su, scope),genNonRecObjType(su, scope))
    else
      oneOf(const((scope, Top)), const((scope, Bot)), genFunType(su, scope), genTypeProj(su, scope), genSimpleRecObjType(su, scope), genNonRecObjType(su, scope))
    // TODO genSimpleRecObjType?
    // TODO genNonRecursiveObject
  }










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
      (scope2, xType) <- genType(su, scope)
    } yield InferenceProblem(Var(x), Que, scope2 + (x -> xType), Var(x).withType(xType))
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
    resType <- NoFuture.eliminateVarUp(scope, x, p2.expectedType, Set())
  } yield InferenceProblem(Let(x, p1.term, p2.term), p2.prototype, (p1.scope ++ p2.scope) - x, Let(x, p1.expected, p2.expected).withType(resType))

  def genSelInferenceProblem(su: SymbolUniverse, scope: Scope): Gen[InferenceProblem] = { // TODO Gen.sized
    val justMakeSomethingUp = for {
      x <- const(su.newSymbol())
      a <- const(su.newSymbol())
      (scope2, aType) <- genType(su, scope)
      xType <- const(FieldDecl(a, aType))
    } yield InferenceProblem(Var(x), Que, scope2 + (x -> xType), Var(x).withType(xType))
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


  // TODO def genAppInferenceProblem(su: SymbolUniverse, scope: Scope): Gen[InferenceProblem] // TODO search for funs in scope and generate term from argtype // TODO gen arg, gen super of arg.type, gen function taking super giving {gen term with type}

  //def genAppInferenceProblemFromScope(su: SymbolUniverse, scope: Scope): Gen[InferenceProblem] = // TODO find function in scope (possibly as field in object), generate term with that argtype

  //TODO def genTermFromType



  //TODO def genComplexObjType(su: SymbolUniverse, scope: Scope): Gen[Type]

  def genSimpleObjInferenceProblem(su: SymbolUniverse, scope: Scope): Gen[InferenceProblem] = Gen.sized{ size =>
    def genSimpleDef(su: SymbolUniverse, scope: Scope, z: Symbol): Gen[(Def, Scope, TypedDef)] = Gen.sized{ size =>
      val genFieldDef = for {
        n <- const(size) if n >= 2 // 1 for FieldDef-node and 1 for aTerm.
        a <- const(su.newSymbol())
        p <- Gen.resize(n-1, genInferenceProblem(su, scope))
        myType     <- const(FieldDecl(a, p.expectedType))
      } yield (FieldDef(a, p.term), scopePush(p.scope, z, myType), FieldDef(a, p.expected).withType(myType))

      val genTypeDef = for {
        n <- const(size) if n >= 2 // 1 for TypeDef-node and 1 for aType.
        a <- const(su.newSymbol())
        (scope2, aType)      <- Gen.resize(n-1, genType(su, scope))
        myType     <- const(TypeDecl(a, aType, aType))
      } yield (TypeDef(a, aType), scopePush(scope2, z, myType), TypeDef(a, aType).withType(myType))

      val genAndDef = for {
        n   <- const(size) if n >= 5 // 1 for AndDef and 2 for each side (since FieldDef and TypeDef needs atleast 2 each).
        sub <- const(n - 1)
        leftSize  <- Gen.choose(2, sub-2)
        rightSize <- const(sub - leftSize)
        (leftDef, leftScope, leftTypedDef)    <- Gen.resize(leftSize, genSimpleDef(su, scope, z))
        (rightDef, rightScope, rightTypedDef) <- Gen.resize(rightSize, genSimpleDef(su, leftScope, z))
        myType     <- const(AndType(leftTypedDef.assignedType, rightTypedDef.assignedType))
      } yield (AndDef(leftDef, rightDef), scopePush(rightScope, z, myType), AndDef(leftTypedDef, rightTypedDef).withType(myType))

      if (size >= 5)
        oneOf(genFieldDef, genTypeDef, genAndDef)
      else if (size >= 2)
        oneOf(genFieldDef, genTypeDef)
      else
        Gen.fail
    }

    if (size == 0) Gen.fail
    for {
      x <- const(su.newSymbol())
      (defs, defsScope, typedDefs) <- Gen.resize(size - 1, genSimpleDef(su, scope, x))
      xType <- const(typedDefs.assignedType)
    } yield InferenceProblem(Obj(x, xType, defs), Que, defsScope - x, Obj(x, xType, typedDefs).withType(xType))
  }

  // TODO genComplexObjInferenceProblem
  // membersWithKnownTypes <- gen some (Symbol, Decl)
  // gen all other members:
  //  member x can see membersWithKnownTypes
  //  member x can see also member y if y < x
  //
  // gen defs for membersWithKnownTypes.
  //  member x can see all members.

  def genObjInferenceProblem(su: SymbolUniverse, scope: Scope): Gen[InferenceProblem] =
    genSimpleObjInferenceProblem(su, scope)
    // TODO genComplexObjInferenceProblem

  // TODO genInferenceProblemFromType: Modify genInferenceProblem to take a prototype and use fixed-point
  // to eliminate the prototype?

  def genAppInferenceProblem(su: SymbolUniverse, scope: Scope): Gen[InferenceProblem] = {
    val genFromArg = for { // TODO better to add fun to scope?
      f <- const(su.newSymbol())
      x <- const(su.newSymbol())
      y <- const(su.newSymbol())
      arg <- genInferenceProblem(su, scope)
      argSupertype <- genSupertype(su, scope, arg.expectedType)
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
        yType <- genSubtype(su, scope, xType)
        appResType <- const(NoFuture.typeRenameVar(x, y, xResType))
      } yield InferenceProblem(App(f, y), Que, scope + (y -> yType), App(f, y).withType(appResType))
      oneOf(genFromArg, genFromScope)
    }
  }

    // TODO also: genAppInferenceProblemFromScope

  def genFunInferenceProblem(su: SymbolUniverse, scope: Scope): Gen[InferenceProblem] = for {
    x                 <- const(su.newSymbol)
    (scope2, xType) <- genType(su, scope)
    body              <- genInferenceProblem(su, scope2 + (x -> xType))
    prototype         <- const(if (body.prototype == Que) Que else FunType(x, Que, body.prototype))
  } yield InferenceProblem(Fun(x, xType, body.term), prototype, body.scope - x, Fun(x, xType, body.expected).withType(FunType(x, xType, body.expectedType)))

  def genInferenceProblem(su: SymbolUniverse, scope: Scope): Gen[InferenceProblem] = oneOf(
    genVarInferenceProblem(su, scope),
    genLetInferenceProblem(su, scope),
    genSelInferenceProblem(su, scope),
    genFunInferenceProblem(su, scope),
    genAppInferenceProblem(su, scope),
    genObjInferenceProblem(su, scope)
  )


}
