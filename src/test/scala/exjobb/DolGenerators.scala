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

import ScalaCheckUtils._

object DolGenerators {
  // (su, prototype) -> (term, scope, typedterm)

  // TODO INV: globalScope.keys.intersect(scope.keys) == Set()
  sealed case class GlobalContext(globalScope: Scope = Map(), nextSymbol: Int = 0) {
    def withBinding(binding: (Symbol, Type)): Gen[GlobalContext] =
      const(copy(globalScope = globalScope+binding))
    def newSymbol(): Gen[(GlobalContext, Symbol)] =
      const((copy(nextSymbol = nextSymbol+1), nextSymbol))
    def toSymbolUniverse(): Gen[SymbolUniverse] =
      const(new SymbolUniverse(nextSymbol))
  }
  // TODO Hide GlobalContext in monad?

  def genGlobalScope(ctx: GlobalContext = GlobalContext()): Gen[GlobalContext] = const(ctx)

  def genFunType(ctx: GlobalContext, scope: Scope): Gen[(GlobalContext, Type)] = Gen.sized{ size =>
    if (size < 3)
      Gen.fail
    else for {
      (argSize, resSize) <- splitSizeNonZero(size - 1)
      (ctx2, x)   <- ctx.newSymbol()
      (ctx3, arg) <- Gen.resize(argSize, genType(ctx2, scope))
      (ctx4, res) <- Gen.resize(resSize, genType(ctx3, scope))
    } yield (ctx4, FunType(x, arg, res))
  }






  // TODO should some of these be Cogen rather then Gen?

//  def genLowerPrototypeFromType(su: SymbolUniverse, scope: Scope, typ: Type): Gen[Prototype] = typ match {
//    case FunType(x, xType, resType) =>
//      oneOf(
//        const(typ),
//        const(Que),
//        for {
//          xPrototype <- genUpperPrototypeFromType(su, scope, xType)
//          resPrototype <- genLowerPrototypeFromType(su, scope, resType)
//        } yield FunType(x, xPrototype, resPrototype))
//    case _ => oneOf(const(Que), const(typ))
//  }
//  def genUpperPrototypeFromType(su: SymbolUniverse, scope: Scope, typ: Type): Gen[Prototype] = typ match {
//    // TODO obj-types probably need special handling...
//    case FunType(x, xType, resType) =>
//      oneOf(
//        const(typ),
//        const(Que),
//        for {
//          xPrototype <- genLowerPrototypeFromType(su, scope, xType)
//          resPrototype <- genUpperPrototypeFromType(su, scope, resType)
//        } yield FunType(x, xPrototype, resPrototype))
//    case _ => oneOf(const(Que), const(typ))
//  }



  def genSupertype(ctx: GlobalContext, scope: Scope, subtype: Type, visited: Set[TypeProj] = Set()): Gen[(GlobalContext, Type)] = Gen.sized { size => // TODO Gen[(Scope,Type)]?
    if (size == 0)
      Gen.fail
    else if (size == 1) {
      if (subtype.totNumNodes < 1)
        const((ctx, Top))
      else
        oneOf(const((ctx, subtype)), const((ctx, Top)))
    } else {
      val extra: Seq[Gen[(GlobalContext, Type)]] = subtype match {
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

      oneOf(const((ctx, subtype)), const((ctx, Top)), extra: _*)
    }
  }

  def genSubtype(ctx: GlobalContext, scope: Scope, supertype: Type, visited: Set[TypeProj] = Set()): Gen[(GlobalContext, Type)] = Gen.sized { size => // TODO Gen[(Scope,Type)]?
    if (size == 0)
      Gen.fail
    else if (size <= 10) { // TODO bad!!! needs to be a bit bigger.
      if (supertype.totNumNodes == 1)
        oneOf(const((ctx, supertype)), const((ctx, Bot)))
      else
        const((ctx, Bot))
    } else {
      val extra: Seq[Gen[(GlobalContext, Type)]] = supertype match {
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

      oneOf(const((ctx, supertype)), const((ctx, Bot)), extra: _*)
    }
  }

  def genTypeProj(ctx: GlobalContext, scope: Scope): Gen[(GlobalContext, TypeProj)] = Gen.sized{ size =>
    val typeMembersInScope = (ctx.globalScope ++ scope).flatMap{case (x, xType) =>
      NoFuture.allDirectTypeMembers(scope, xType, Set()).map{
        case a => TypeProj(x, a)
      }
    }.toSet.toSeq

    val justMakeSomethingUp: Gen[(GlobalContext, TypeProj)] = for { // TODO sized...
      (ctx2, x) <- ctx.newSymbol()
      (ctx3, a) <- ctx2.newSymbol()
      (ctx4, aUpperType) <- genType(ctx3, scope) // TODO obfuscate the TypeDecl with extra fields?
      (ctx5, aLowerType) <- genSubtype(ctx4, scope, aUpperType) // TODO obfuscate the TypeDecl with extra fields?
      ctx6 <- ctx5.withBinding(x -> TypeDecl(a, aLowerType, aUpperType))
    } yield (ctx6, TypeProj(x, a))

    if (typeMembersInScope.isEmpty) {
      justMakeSomethingUp
    } else {
      val grabSomethingFromScope = for {
        p <- oneOf(typeMembersInScope)
      } yield (ctx, p)
      Gen.frequency((1, justMakeSomethingUp), (typeMembersInScope.size, grabSomethingFromScope))
    }
  }


  def genFieldDecl(ctx: GlobalContext, scope: Scope): Gen[(GlobalContext, Type)] = Gen.sized{ size =>
    if (size < 2) // 1 FieldDecl + 1 aType = 2
      Gen.fail
    else for {
      (ctx2, a)     <- ctx.newSymbol()
      (ctx3, aType) <- Gen.resize(size-1, genType(ctx, scope))
    } yield (ctx3, FieldDecl(a, aType))
  }

  def genTypeDecl(ctx: GlobalContext, scope: Scope): Gen[(GlobalContext, Type)] = Gen.sized{ size =>
    if (size < 3) // 1 TypeDecl + 1 aLowerType + 1 aUpperType = 3
      Gen.fail
    else for {
      (lowerSize, upperSize) <- splitSizeNonZero(size-1)
      (ctx2, a) <- ctx.newSymbol()
      (ctx3, aUpperType) <- Gen.resize(upperSize, genType(ctx2, scope))
      (ctx4, aLowerType) <- Gen.resize(lowerSize, genSubtype(ctx3, scope, aUpperType))
    } yield (ctx4, TypeDecl(a, aLowerType, aUpperType))
  }

  // TODO genComplexAccTypeDecls where decls can see themselves too.

  // Generate an intersection of TypeDecls recursive on z.
  //
  // The decls are generated in such a way that each decl can "see" the decls
  // before it. The final scope contains z with the intersection of all decls.
  def genSimpleAccTypeDecls(ctx: GlobalContext, scope: Scope, z: Symbol): Gen[(GlobalContext, Type)] = Gen.sized{ size =>

    def updatedType(scope: Scope, typ: Type) = scope.get(z) match {
      case Some(zType) => AndType(zType, typ)
      case None => typ
    }

    val genAccAndType: Gen[(GlobalContext, Type)] = Gen.delay(for {
      (leftSize, rightSize) <- splitSizeNonZero(size-1, min=2)
      (ctx2, left)  <- Gen.resize(leftSize, genSimpleAccTypeDecls(ctx, scope, z))
      scopeIncludingLeft <- const(scope + (z -> updatedType(scope, left)))
      (ctx3, right) <- Gen.resize(rightSize, genSimpleAccTypeDecls(ctx2, scopeIncludingLeft, z))
    } yield (ctx3, updatedType(scope, AndType(left, right))))

    // minSize(genFieldDecl)  = 2
    // minSize(genTypeDecl)   = 3
    // minSize(genAccAndType) = 5 = 1 AndType + 2 min(minSize(genFieldDecl), minSize(genTypeDecl))
    if (size >= 5)
      oneOf(genFieldDecl(ctx, scope), genTypeDecl(ctx, scope), genAccAndType)
    else if (size >= 3)
      oneOf(genFieldDecl(ctx, scope), genTypeDecl(ctx, scope))
    else if (size >= 2)
      genFieldDecl(ctx, scope)
    else
      Gen.fail
  }

  def scopePush(scope: Scope, x: Symbol, anotherXType: Type): Scope = scope.get(x) match {
    case Some(oldXType) => scope + (x -> AndType(oldXType, anotherXType))
    case None           => scope + (x -> anotherXType)
  }


  def genAndType(ctx: GlobalContext, scope: Scope, g: (GlobalContext, Scope) => Gen[(GlobalContext, Type)], gMinSize: Int): Gen[(GlobalContext, Type)] = Gen.sized{ size =>
    // minSize(genAndType) = 1 + 2*minSize(g)
    if (size < 1 + 2*gMinSize)
      Gen.fail
    else for {
      (leftSize, rightSize) <- splitSizeNonZero(size-1, min=gMinSize)
      (ctx2, left)  <- Gen.resize(leftSize, g(ctx, scope))
      (ctx3, right) <- Gen.resize(rightSize, g(ctx2, scope))
    } yield (ctx3, AndType(left, right))
  }

  // TODO SizedGen(minSize: Int) extends Gen ?
  def genNonRecObjType(ctx: GlobalContext, scope: Scope): Gen[(GlobalContext, Type)] = Gen.sized{ size =>
    if (size >= 5)
      Gen.resize(size, oneOf(
        genFieldDecl(ctx, scope),
        genTypeDecl(ctx, scope),
        genAndType(ctx, scope, genNonRecObjType(_, _), 2)
      ))
    else if (size >= 3)
      Gen.resize(size, oneOf(genFieldDecl(ctx, scope), genTypeDecl(ctx, scope)))
    else if (size >= 2)
      Gen.resize(size, genFieldDecl(ctx, scope))
    else
      Gen.fail
  }

  def genTypeDecls(ctx: GlobalContext, scope: Scope): Gen[(GlobalContext, Type)] = Gen.sized{ size =>
    if (size >= 5)
      oneOf(
        genTypeDecl(ctx, scope),
        genAndType(ctx, scope, genTypeDecls(_, _), 3)
      )
    else if (size >= 3)
      genTypeDecl(ctx, scope)
    else
      Gen.fail
  }

  def genFieldDecls(ctx: GlobalContext, scope: Scope): Gen[(GlobalContext, Type)] = Gen.sized{ size =>
    if (size >= 5)
      oneOf(
        genFieldDecl(ctx, scope),
        genAndType(ctx, scope, genFieldDecls(_, _), 2)
      )
    else if (size >= 2)
      genFieldDecl(ctx, scope)
    else
      Gen.fail
  }

  // TODO def genObfuscatedType(typ). Generates equivalent type. Changes order
  // of AndTypes, replaces types with TypeProjs, etc.

  def genRecType(ctx: GlobalContext, scope: Scope): Gen[(GlobalContext, Type)] = Gen.sized{ size =>
    if (size < 2)
      Gen.fail
    else if (size == 2)
      Gen.resize(size, genFieldDecl(ctx, scope))
    else
      oneOf(
        Gen.resize(size, genFieldDecl(ctx, scope)),
        Gen.resize(size, genTypeDecl(ctx, scope)),
        for {
          // fieldsSize + typesSize + 1 <= size
          // fieldsSize >= 2
          // typesSize >= 3


          fieldDeclsSize <- // TODO Yuck. Would be nice to have a function that just figures out the combinatorics of this.
            if (2 <= size-1-3) // If we can have both typedecls and fieldecls.
              Gen.frequency(
                (1, const(0)),                          // only TypeDecls
                (1, const(size-1)),                     // only FieldDecls
                (size-1-3-2+1, Gen.choose(2, size-1-3)) // both Typedecls and fieldDecls
              )
            else if (3 <= size -1) // If we can have only TypeDecls.
              Gen.oneOf(0, size - 1) // only TypeDecls or only FieldDecls.
            else
              const(size-1) // only FieldDecls.

          typeDeclsSize <- const(size - 1 - fieldDeclsSize)

          (ctx2, z)   <- ctx.newSymbol()
          (finalCtx, typ) <- (fieldDeclsSize, typeDeclsSize) match {
            case (0, _) => Gen.resize(typeDeclsSize, genSimpleAccTypeDecls(ctx2, scope, z))
            case (_, 0) => Gen.resize(fieldDeclsSize, genFieldDecls(ctx2, scope))
            case _ => for {
              (ctx3, typeDeclIntersection)  <- Gen.resize(typeDeclsSize, genSimpleAccTypeDecls(ctx2, scope, z))
              (ctx4, fieldDeclIntersection) <- Gen.resize(fieldDeclsSize, genFieldDecls(ctx3, scope + (z -> typeDeclIntersection)))
            } yield (ctx4, AndType(typeDeclIntersection, fieldDeclIntersection))
          }

        } yield (finalCtx, RecType(z, typ))
      )
  }


  // TODO genTypeWithoutScope and genType

  // TODO easier to generate normal types? genCanonicalType = canonicalize(genType)?

  // TODO genAnyType: version of genType that may generate recursive types.

  // TODO allow genType to add types to scope? Gen[(Type, updatedScope)]
  def genType(ctx: GlobalContext, scope: Scope): Gen[(GlobalContext, Type)] = Gen.sized { size =>
    if (size <= 0)
      Gen.fail
    else if (size <= 1)
      oneOf(const((ctx, Top)), const((ctx, Bot)), genTypeProj(ctx, scope))
    else if (size <= 2)
      oneOf(const((ctx, Top)), const((ctx, Bot)), genTypeProj(ctx, scope), genNonRecObjType(ctx, scope))
    else
      oneOf(const((ctx, Top)), const((ctx, Bot)), genFunType(ctx, scope), genTypeProj(ctx, scope), genRecType(ctx, scope), genNonRecObjType(ctx, scope))
    // TODO genRecType?
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

  // TODO gen with prototype.

  def genVarInferenceProblem(ctx: GlobalContext, scope: Scope): Gen[(GlobalContext, InferenceProblem)] = {
    val justMakeSomethingUp = for {
      (ctx2, x) <- ctx.newSymbol
      (ctx3, xType) <- genType(ctx2, scope)
      ctx4 <- ctx3.withBinding(x -> xType)
    } yield (ctx4, InferenceProblem(Var(x), Que, scope, Var(x).withType(xType)))

    if (scope.isEmpty) {
      justMakeSomethingUp
    } else {
      val pickSomethingFromScope = for {
        (x, xType) <- oneOf(ctx.globalScope.toSeq ++ scope.toSeq)
      } yield (ctx, InferenceProblem(Var(x), Que, scope, Var(x).withType(xType)))
      oneOf(justMakeSomethingUp, pickSomethingFromScope)
    }
  }

  def genLetInferenceProblem(ctx: GlobalContext, scope: Scope): Gen[(GlobalContext, InferenceProblem)] = for {
    (ctx2, x) <- ctx.newSymbol()
    (ctx3, p1) <- genInferenceProblem(ctx2, scope)
    (ctx4, p2) <- genInferenceProblem(ctx3, scope + (x -> p1.expectedType))
    resType <- NoFuture.eliminateVarUp((ctx4.globalScope ++ scope), x, p2.expectedType, Set())
  } yield (ctx4, InferenceProblem(Let(x, p1.term, p2.term), Que, scope, Let(x, p1.expected, p2.expected).withType(resType)))

  def genSelInferenceProblem(ctx: GlobalContext, scope: Scope): Gen[(GlobalContext, InferenceProblem)] = { // TODO Gen.sized
    val justMakeSomethingUp = for {
      (ctx2, x) <- ctx.newSymbol()
      (ctx3, a) <- ctx2.newSymbol()
      (ctx4, aType) <- genType(ctx3, scope) // TODO obfuscate by adding other fields?
      ctx5 <- ctx4.withBinding(x -> FieldDecl(a, aType))
    } yield (ctx5, InferenceProblem(Sel(x, a), Que, scope, Sel(x, a).withType(aType)))
    val fieldRefs = NoFuture.directFieldDeclsInScope(ctx.globalScope ++ scope)
    if (fieldRefs.isEmpty) {
      justMakeSomethingUp
    } else {
      val pickSomethingFromScope = for {
        (Seq(x, a), aType) <- oneOf(fieldRefs.toSeq)
      } yield (ctx, InferenceProblem(Sel(x, a), Que, scope, Sel(x, a).withType(aType)))
      Gen.frequency((1, justMakeSomethingUp), (fieldRefs.size, pickSomethingFromScope))
    }
  }


  // TODO def genAppInferenceProblem(su: SymbolUniverse, scope: Scope): Gen[InferenceProblem] // TODO search for funs in scope and generate term from argtype // TODO gen arg, gen super of arg.type, gen function taking super giving {gen term with type}

  //def genAppInferenceProblemFromScope(su: SymbolUniverse, scope: Scope): Gen[InferenceProblem] = // TODO find function in scope (possibly as field in object), generate term with that argtype

  //TODO def genTermFromType



  //TODO def genComplexObjType(su: SymbolUniverse, scope: Scope): Gen[Type]

  def genSimpleObjInferenceProblem(ctx: GlobalContext, scope: Scope): Gen[(GlobalContext, InferenceProblem)] = Gen.sized{ size =>
    ??? // TODO
//    def genSimpleDef(su: SymbolUniverse, scope: Scope, z: Symbol): Gen[(Def, Scope, TypedDef)] = Gen.sized{ size =>
//      val genFieldDef = for {
//        n <- const(size) if n >= 2 // 1 for FieldDef-node and 1 for aTerm.
//        a <- const(su.newSymbol())
//        p <- Gen.resize(n-1, genInferenceProblem(su, scope))
//        myType     <- const(FieldDecl(a, p.expectedType))
//      } yield (FieldDef(a, p.term), scopePush(p.scope, z, myType), FieldDef(a, p.expected).withType(myType))
//
//      val genTypeDef = for {
//        n <- const(size) if n >= 2 // 1 for TypeDef-node and 1 for aType.
//        a <- const(su.newSymbol())
//        (scope2, aType) <- Gen.resize(n-1, genType(su, scope))
//        myType          <- const(TypeDecl(a, aType, aType))
//      } yield (TypeDef(a, aType), scopePush(scope2, z, myType), TypeDef(a, aType).withType(myType))
//
//      val genAndDef = for {
//        n   <- const(size) if n >= 5 // 1 for AndDef and 2 for each side (since FieldDef and TypeDef needs atleast 2 each).
//        sub <- const(n - 1)
//        leftSize  <- Gen.choose(2, sub-2)
//        rightSize <- const(sub - leftSize)
//        (leftDef, leftScope, leftTypedDef)    <- Gen.resize(leftSize, genSimpleDef(su, scope, z))
//        (rightDef, rightScope, rightTypedDef) <- Gen.resize(rightSize, genSimpleDef(su, leftScope, z))
//        myType     <- const(AndType(leftTypedDef.assignedType, rightTypedDef.assignedType))
//      } yield (AndDef(leftDef, rightDef), scopePush(rightScope, z, myType), AndDef(leftTypedDef, rightTypedDef).withType(myType))
//
//      if (size >= 5)
//        oneOf(genFieldDef, genTypeDef, genAndDef)
//      else if (size >= 2)
//        oneOf(genFieldDef, genTypeDef)
//      else
//        Gen.fail
//    }
//
//    if (size == 0) Gen.fail
//    for {
//      x <- const(su.newSymbol())
//      (defs, defsScope, typedDefs) <- Gen.resize(size - 1, genSimpleDef(su, scope, x))
//      xType <- const(typedDefs.assignedType)
//    } yield InferenceProblem(Obj(x, xType, defs), Que, defsScope - x, Obj(x, xType, typedDefs).withType(xType))
  }

  // TODO genComplexObjInferenceProblem
  // membersWithKnownTypes <- gen some (Symbol, Decl)
  // gen all other members:
  //  member x can see membersWithKnownTypes
  //  member x can see also member y if y < x
  //
  // gen defs for membersWithKnownTypes.
  //  member x can see all members.

  def genObjInferenceProblem(ctx: GlobalContext, scope: Scope): Gen[(GlobalContext, InferenceProblem)] =
    genSimpleObjInferenceProblem(ctx, scope)
    // TODO genComplexObjInferenceProblem

  // TODO genInferenceProblemFromType: Modify genInferenceProblem to take a prototype and use fixed-point
  // to eliminate the prototype?

  def genAppInferenceProblem(ctx: GlobalContext, scope: Scope): Gen[(GlobalContext, InferenceProblem)] = {
    val genFromArg = for {
      (ctx2, f) <- ctx.newSymbol()
      (ctx3, y) <- ctx2.newSymbol()
      (ctx4, funType @ FunType(x, xType, xResType)) <- genFunType(ctx3, scope) // TODO make it so that this can generate a typeproj to a funtype
      ctx5 <- ctx4.withBinding(f -> funType)
      (ctx6, argSubtype) <- genSubtype(ctx5, scope, xType)
      ctx7 <- ctx6.withBinding(y -> argSubtype)
      appResType <- const(NoFuture.typeRenameVar(x, y, xResType))
    } yield (ctx7, InferenceProblem(App(f, y), Que, scope, App(f, y).withType(appResType)))

    var funsInScope = scope.filter{
      case (_, _: FunType) => true
      case _ => false
    }

    if (funsInScope.isEmpty) {
      genFromArg
    } else {
      val genFromScope = for {
        (f, FunType(x, xType, xResType)) <- oneOf(funsInScope.toSeq)
        (ctx2, y) <- ctx.newSymbol()
        (ctx3, yType) <- genSubtype(ctx2, scope, xType) // TODO make it so that this can pick yType from scope.
        ctx4 <- ctx3.withBinding(y -> yType)
        appResType <- const(NoFuture.typeRenameVar(x, y, xResType))
      } yield (ctx2, InferenceProblem(App(f, y), Que, scope + (y -> yType), App(f, y).withType(appResType)))
      oneOf(genFromArg, genFromScope)
    }
  }

    // TODO also: genAppInferenceProblemFromScope

  def genFunInferenceProblem(ctx: GlobalContext, scope: Scope): Gen[(GlobalContext, InferenceProblem)] = for {
    (ctx2, x)     <- ctx.newSymbol()
    (ctx3, xType) <- genType(ctx2, scope)
    (ctx4, body)  <- genInferenceProblem(ctx3, scope + (x -> xType))
  } yield (ctx4, InferenceProblem(Fun(x, xType, body.term), Que, scope, Fun(x, xType, body.expected).withType(FunType(x, xType, body.expectedType))))

  def genInferenceProblem(ctx: GlobalContext, scope: Scope): Gen[(GlobalContext, InferenceProblem)] = oneOf(
    genVarInferenceProblem(ctx, scope),
    genLetInferenceProblem(ctx, scope),
    genSelInferenceProblem(ctx, scope),
    genFunInferenceProblem(ctx, scope),
    genAppInferenceProblem(ctx, scope),
    genObjInferenceProblem(ctx, scope)
  )


}
