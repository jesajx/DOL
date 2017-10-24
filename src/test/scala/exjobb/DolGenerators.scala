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

  // TODO reuse symbols for bound vars

  // TODO Maybe let "1 node" == "size 0"? Since trees with zero nodes do not
  // make sense in DOL. This way some Gen.fail-cases can be avoided.

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
  //
  // TODO Generator[T]: (seed,size) => T
  // TODO Generator[T]: {minSize: Int}
  // TODO Generator[T,B]: {state: B}  // e.g. B=GlobalContext
  // TODO oneOf(a,b).minSize = min(a.minSize, b.minSize)

  def genGlobalScope(ctx: GlobalContext = GlobalContext()): Gen[GlobalContext] = const(ctx)

  def genScope(ctx: GlobalContext, scope: Scope = Map()): Gen[(GlobalContext, Scope)] = Gen.sized{ size =>
    if (size <= 0)
      const((ctx, Map()))
    else if (size == 1) for {
        (ctx2, x) <- ctx.newSymbol()
        (ctx3, typ) <- Gen.resize(size, genType(ctx2, scope))
        // TODO unwrapRecTypes?
        // TODO cyclic variables? x.A <: y.A <: x.A
      } yield (ctx3, scope + (x -> typ))
    else Gen.oneOf(
      for {
        (ctx2, x) <- ctx.newSymbol()
        (ctx3, typ) <- Gen.resize(size, genType(ctx2, scope))
        // TODO unwrapRecTypes?
        // TODO cyclic variables? x.A <: y.A <: x.A
      } yield (ctx3, scope + (x -> typ)),
      for {
        (leftSize, rightSize) <- splitSizeNonZero(size, min=1)
        (ctx2, scope2) <- Gen.resize(leftSize, genScope(ctx, scope))
        (ctx3, scope3) <- Gen.resize(rightSize, genScope(ctx2, scope2))
      } yield (ctx3, scope3))
  }

  def genFunType(ctx: GlobalContext, scope: Scope): Gen[(GlobalContext, Type)] = Gen.sized{ size =>
    if (size < 3)
      Gen.fail
    else for {
      (argSize, resSize) <- splitSizeNonZero(size - 1)
      (ctx2, x)   <- ctx.newSymbol()
      (ctx3, arg) <- Gen.resize(argSize, genType(ctx2, scope))
      (ctx4, res) <- Gen.resize(resSize, genType(ctx3, scope + (x -> arg)))
    } yield (ctx4, FunType(x, arg, res))
  }


  // TODO cyclic typedecls. x: TypeDecl(a, x.a, x.a), x: AndType(TypeDecl(a, x.b, x.b), TypeDecl(b, x.a, x.a))

  // TODO reuse typemember symbols. maybe have a set of typemembers in
  // globalscope, and generate like oneOf(existingMemberSymbols, newMember)


  /** Punch holes (`Que`) in `typ` to get a prototype.
   */
  def genPrototypeFromType(typ: Prototype): Gen[Prototype] = // TODO sized?
    oneOf(
      const(Que),
      typ match {
        // NOTE: RecTypes may not contain Prototypes. // TODO Or could they?
        case FieldDecl(a, aType) =>
          for {
            aPrototype <- genPrototypeFromType(aType)
          } yield FieldDecl(a, aPrototype)
        case TypeDecl(a, aLowerType, aUpperType) =>
          //for {
          //  (ctx2, aLowerPrototype) <- genPrototypeFromType(ctx, aLowerType)
          //  (ctx3, aUpperPrototype) <- genPrototypeFromType(ctx2, aUpperType)
          //} yield (ctx3, TypeDecl(a, aLowerPrototype, aUpperPrototype))
          oneOf(
            for {
              aLowerPrototype <- genPrototypeFromType(aLowerType)
            } yield TypeDecl(a, aLowerPrototype, aUpperType),
            for {
              aUpperPrototype <- genPrototypeFromType(aUpperType)
            } yield TypeDecl(a, aLowerType, aUpperPrototype)
          )
        case AndType(left, right) =>
          for {
            leftPrototype <- genPrototypeFromType(left)
            rightPrototype <- genPrototypeFromType(right)
          } yield AndType(leftPrototype, rightPrototype)
        case FunType(x, xType, resType) =>
          for {
            xPrototype <- genPrototypeFromType(xType)
            resPrototype <- genPrototypeFromType(resType)
          } yield FunType(x, xPrototype, resPrototype)
        case _ => const(typ)
      }
    )

  /** Generate a supertype of `typ`.
   */
  def genSupertype(ctx: GlobalContext, scope: Scope, typ: Type, visited: Set[TypeProj] = Set()): Gen[(GlobalContext, Type)] = Gen.sized { size => // TODO Gen[(Scope,Type)]?
    if (size == 0)
      Gen.fail
    else if (size == 1) {
      if (typ.totNumNodes < 1)
        const((ctx, Top))
      else
        oneOf(const((ctx, typ)), const((ctx, Top)))
    } else {
      val extra: Seq[Gen[(GlobalContext, Type)]] = typ match { // TODO!!!
        case FunType(x, xType, resType) if (size >= 3) =>
          Seq(for {
            (argSize, resSize)   <- splitSizeNonZero(size - 1)
            (ctx2, xSubtype)     <- Gen.resize(argSize, genSubtype(ctx, scope, xType, visited))
            (ctx3, resSupertype) <- Gen.resize(resSize, genSupertype(ctx2, scope + (x -> xSubtype), resType, visited))
            // TODO supertypes of projs? // remove/keep/supertype
          } yield (ctx3, FunType(x, xSubtype, resSupertype)))
        case AndType(left, right) =>
          Seq(
              Gen.resize(size,  genSupertype(ctx, scope, left, visited)),
              Gen.resize(size,  genSupertype(ctx, scope, right, visited))
          ) ++ (
            if(size >= 3)
              Some(for {
                (leftSize, rightSize) <- splitSizeNonZero(size - 1)
                (ctx2, leftSuper)     <- Gen.resize(leftSize,  genSupertype(ctx, scope, left, visited))
                (ctx3, rightSuper)    <- Gen.resize(rightSize, genSupertype(ctx2, scope, right, visited))
              } yield (ctx3, AndType(leftSuper, rightSuper)))
            else
              None
          )

        case aProj @ TypeProj(x, a) if !visited(aProj) =>
          val aUpperType = NoFuture.typeProjectUpper(ctx.globalScope ++ scope, x, a).get
          Seq(Gen.resize(size, genSupertype(ctx, scope, aUpperType, visited + aProj)))
        case FieldDecl(a, aType) if (size >= 2) =>
          Seq(for {
            (ctx2, aSupertype) <- Gen.resize(size - 1, genSupertype(ctx, scope, aType, visited))
          } yield (ctx2, FieldDecl(a, aSupertype)))
        case TypeDecl(a, aLowerType, aUpperType) if (size >= 3) =>
          Seq(for {
            (lowerSize, upperSize) <- splitSizeNonZero(size - 1)
            (ctx2, aSubtype)       <- Gen.resize(lowerSize, genSubtype(ctx, scope, aLowerType, visited))
            (ctx3, aSupertype)     <- Gen.resize(upperSize, genSupertype(ctx2, scope, aUpperType, visited))
          } yield (ctx3, TypeDecl(a, aSubtype, aSupertype)))
        case Bot =>
          Seq(genType(ctx, scope))
        case _ =>
          Seq()
      }
      // TODO and(self, super(self))? and(super(self), super(self))?

      oneOf(const((ctx, typ)), const((ctx, Top)), extra.toSeq: _*)
    }
  }

  /** Generate a subtype of `typ`.
   */
  def genSubtype(ctx: GlobalContext, scope: Scope, typ: Type, visited: Set[TypeProj] = Set()): Gen[(GlobalContext, Type)] = Gen.sized { size => // TODO Gen[(Scope,Type)]?
    if (size <= 0)
      Gen.fail
    else {
      val extra: Option[Gen[(GlobalContext, Type)]] = typ match {
        case FunType(x, xType, resType) if (size >= 3) =>
          None // TODO
          //Some(for {
          //  (argSize, resSize) <- splitSizeNonZero(size - 1)
          //  // TODO is this correct or is it necessary to do a
          //  // varEliminatingTransform or similar?
          //  // TODO YES: it leads to problems. e.g. Fun(x, TypeDecl(a,A,B), x.a) being replaced by Fun(x, Top, x.a)
          //  (ctx2, xSupertype) <- Gen.resize(argSize, genSupertype(ctx, scope, xType, visited))
          //  (ctx3, resSubtype) <- Gen.resize(resSize, genSubtype(ctx2, scope + (x -> xSupertype), resType, visited))
          //} yield (ctx3, FunType(x, xSupertype, resSubtype)))
        case AndType(left, right) if (size >= 3) =>
          Some(for {
            (leftSize, rightSize) <- splitSizeNonZero(size - 1)
            (ctx2, leftSub)  <- Gen.resize(leftSize,  genSubtype(ctx, scope, left, visited))
            (ctx3, rightSub) <- Gen.resize(rightSize, genSubtype(ctx2, scope, right, visited))
          } yield (ctx3, AndType(leftSub, rightSub)))
        case aProj @ TypeProj(x, a) if !visited(aProj) =>
          val aLowerType = NoFuture.typeProjectLower(ctx.globalScope ++ scope, x, a).get
          Some(Gen.resize(size, genSubtype(ctx, scope, aLowerType, visited + aProj))) // TODO is it necessary to track visited?
        case FieldDecl(a, aType) =>
          Some(for {
            (ctx2, aSubtype) <- Gen.resize(size - 1, genSubtype(ctx, scope, aType, visited))
          } yield (ctx2, FieldDecl(a, aSubtype)))
//        case TypeDecl(a, aLowerType, aUpperType) =>
//          Some() // TODO Need to generate tighter interval without getting into the situation where !(lower <: upper).
        case Top =>
          Some(genType(ctx, scope))
        case _ =>
          None
      }

      val extra2: Option[Gen[(GlobalContext, Type)]] =
        if (size >= 3)
          Some(for {
            (leftSize, rightSize) <- splitSizeNonZero(size - 1)
            (ctx2, sub)      <- Gen.resize(leftSize, genSubtype(ctx, scope, typ)) // NOTE: Since size decreases will not inf rec.
            (ctx3, nonsense) <- Gen.resize(rightSize, genType(ctx2, scope))
            Seq(left, right) <- Gen.pick(2, Seq(sub, nonsense))
          } yield (ctx3, AndType(left, right)))
        else
          None

      val hideBehindTypeProj =
        if (size >= 4 && NoFuture.allFreeVarsInType(typ).intersect(scope.keySet).isEmpty)
          Seq(for {
            (lowerSize, upperSize) <- splitSizeNonZero(size - 2)
            (ctx2, x) <- ctx.newSymbol()
            (ctx3, a) <- ctx2.newSymbol()
            (ctx4, upperType) <- Gen.resize(upperSize, genSubtype(ctx3, Map(), typ))
            (ctx5, lowerType) <- Gen.resize(lowerSize, genSubtype(ctx4, Map(), upperType))
            ctx6 <- ctx5.withBinding(x -> TypeDecl(a, lowerType, upperType))
          } yield (ctx6, TypeProj(x, a)))
        else
          Seq()

      val noChange =
        if (typ.totNumNodes <= size)
          Seq(const((ctx, typ)))
        else
          Seq()

      // TODO replace type with typeproj


      oneOfGens(const((ctx, Bot)) +: (noChange ++ hideBehindTypeProj ++ extra.toSeq ++ extra2.toSeq))
    }
  }

  // TODO genTypePair: oneOf: (genSubtype(b), b), (a, genSupertype(a))

  def genTypeProj(ctx: GlobalContext, scope: Scope): Gen[(GlobalContext, TypeProj)] = Gen.sized{ size =>
    val typeMembersInScope = (ctx.globalScope ++ scope).flatMap{case (x, xType) =>
      NoFuture.allDirectTypeMembers(scope, xType, Set()).map{
        case a => TypeProj(x, a)
      }
    }.toSet.toSeq

    // TODO generate subtype of typedecl and then reference the subtype?

    val justMakeSomethingUp: Gen[(GlobalContext, TypeProj)] = for { // TODO sized...
      (ctx2, x) <- ctx.newSymbol()
      (ctx3, a) <- ctx2.newSymbol()
      // NOTE: We may only use the globalScope here, since we don't want to
      // leak references to the localScope (e.g. globalScope={_ -> TypeProj(y,_)},
      // localScope{y->_}).
      (ctx4, aUpperType) <- genType(ctx3, Map()) // TODO obfuscate the TypeDecl with extra fields?
      (ctx5, aLowerType) <- genSubtype(ctx4, Map(), aUpperType) // TODO obfuscate the TypeDecl with extra fields?
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
      (ctx3, aType) <- Gen.resize(size-1, genType(ctx2, scope))
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

  // TODO genComplexAccTypeDecls where decls can see themselves too. Cyclic
  // typedecls.

  /** Generate an intersection of TypeDecls recursive on z.
   *
   * The decls are generated in such a way that each decl can "see" the decls
   * before it. The final scope contains z with the intersection of all decls.
   */
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

    // TODO add arbitrary types (e.g. funtypes)? not just typedecls and fielddecls?

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

  def genUselessRecType(ctx: GlobalContext, scope: Scope): Gen[(GlobalContext, Type)] = Gen.sized { size =>
    if (size < 2)
      Gen.fail
    else for {
      (ctx2, x)   <- ctx.newSymbol()
      (ctx3, typ) <- Gen.resize(size - 1, genType(ctx2, scope))
    } yield (ctx3, RecType(x, typ))
  }

  // TODO genTypeWithoutScope and genType

  // TODO easier to generate normal types? genCanonicalType = canonicalize(genType)?

  // TODO genAnyType: version of genType that may generate recursive types.

  // TODO allow genType to add types to scope? Gen[(Type, updatedScope)]
  def genType(ctx: GlobalContext, scope: Scope): Gen[(GlobalContext, Type)] = Gen.sized { size =>
    if (size <= 0)
      Gen.fail
    else if (size == 1)
      oneOf(const((ctx, Top)), const((ctx, Bot)), genTypeProj(ctx, scope))
    else if (size == 2)
      oneOf(const((ctx, Top)), const((ctx, Bot)), genTypeProj(ctx, scope), genNonRecObjType(ctx, scope), genUselessRecType(ctx, scope))
    else
      oneOf(const((ctx, Top)), const((ctx, Bot)), genTypeProj(ctx, scope), genNonRecObjType(ctx, scope), genUselessRecType(ctx, scope), genFunType(ctx, scope), genRecType(ctx, scope), genAndType(ctx, scope, genType, 1))
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


  // TODO maybe only return TypedTerm? untype(TypedTerm):Term?
  // TODO maybe nest InferenceProblems instead of terms? can always translate
  // to term.
  sealed case class InferenceProblem(term: Term, prototype: Prototype, scope: Scope, expected: TypedTerm) {
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
      (ctx3, xType) <- genType(ctx2, Map()) // NOTE: globalScope to prevent the localScope leaking.
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
    resType <- const(NoFuture.eliminateVars((ctx4.globalScope ++ scope), Set(x), None, p2.expectedType)) // TODO zOption=Some(z) if p1.term is Var(z)?
  } yield (ctx4, InferenceProblem(Let(x, p1.term, p2.term), Que, scope, Let(x, p1.expected, p2.expected).withType(resType)))

  def genSelInferenceProblem(ctx: GlobalContext, scope: Scope): Gen[(GlobalContext, InferenceProblem)] = { // TODO Gen.sized
    val justMakeSomethingUp = for {
      (ctx2, x) <- ctx.newSymbol()
      (ctx3, a) <- ctx2.newSymbol()
      (ctx4, aType) <- genType(ctx3, Map()) // TODO obfuscate by adding other fields?
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

  def genFieldDef(ctx: GlobalContext, scope: Scope): Gen[(GlobalContext, Def, TypedDef)] = Gen.sized{ size =>
    if (size < 2)
      Gen.fail
    else for { // minSize = 2 = 1 FieldDef + 1 aTerm.
      (ctx2, a) <- ctx.newSymbol()
      (ctx3, p) <- Gen.resize(size-1, genInferenceProblem(ctx2, scope))
      myType <- const(FieldDecl(a, p.expectedType))
    } yield (ctx3, FieldDef(a, p.term), FieldDef(a, p.expected).withType(myType))
  }

  def genTypeDef(ctx: GlobalContext, scope: Scope): Gen[(GlobalContext, Def, TypedDef)] = Gen.sized{ size =>
    if (size < 2)
      Gen.fail
    else for { // minSize = 2 = 1 TypeDef + 1 aType.
      (ctx2, a) <- ctx.newSymbol()
      (ctx3, aType) <- Gen.resize(size-1, genType(ctx2, scope))
      myType <- const(TypeDecl(a, aType, aType))
    } yield (ctx3, TypeDef(a, aType), TypeDef(a, aType).withType(myType))
  }

  def genSimpleDef(ctx: GlobalContext, scope: Scope, z: Symbol): Gen[(GlobalContext, Def, TypedDef)] = Gen.sized{ size =>
    def updatedType(scope: Scope, typ: Type) = scope.get(z) match {
      case Some(zType) => AndType(zType, typ)
      case None => typ
    }
    val genAndDef = for { // minSize = 5 = 1 AndDef + 2*min(FieldDef, TypeDef, AndDef).
      (leftSize, rightSize) <- splitSizeNonZero(size-1, min=2)
      (ctx2, leftDef, leftTypedDef)   <- Gen.resize(leftSize, genSimpleDef(ctx, scope, z))
      scopeWithLeft <- const(scope + (z -> updatedType(scope, leftTypedDef.assignedType)))
      (ctx3, rightDef, rightTypedDef) <- Gen.resize(rightSize, genSimpleDef(ctx2, scopeWithLeft, z))
      myType <- const(AndType(leftTypedDef.assignedType, rightTypedDef.assignedType))
    } yield (ctx3, AndDef(leftDef, rightDef), AndDef(leftTypedDef, rightTypedDef).withType(myType))

    if (size >= 5)
      oneOf(genFieldDef(ctx, scope), genTypeDef(ctx, scope), genAndDef)
    else if (size >= 2)
      oneOf(genFieldDef(ctx, scope), genTypeDef(ctx, scope))
    else
      Gen.fail
  }

  def genSimpleObjInferenceProblem(ctx: GlobalContext, scope: Scope): Gen[(GlobalContext, InferenceProblem)] = Gen.sized{ size =>
    if (size == 0)
      Gen.fail
    else for {
      (ctx2, x) <- ctx.newSymbol()
      (ctx3, defs, typedDefs) <- Gen.resize(size - 1, genSimpleDef(ctx2, scope, x))
      xType <- const(typedDefs.assignedType)
    } yield (ctx3, InferenceProblem(Obj(x, xType, defs), Que, scope, Obj(x, xType, typedDefs).withType(xType)))
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
      (ctx4, funType @ FunType(x, xType, xResType)) <- genFunType(ctx3, Map()) // TODO make it so that this can generate a typeproj to a funtype
      ctx5 <- ctx4.withBinding(f -> funType)
      (ctx6, argSubtype) <- genSubtype(ctx5, Map(), xType)
      ctx7 <- ctx6.withBinding(y -> argSubtype)
      appResType <- const(NoFuture.typeRenameVar(x, y, xResType))
    } yield (ctx7, InferenceProblem(App(f, y), Que, scope, App(f, y).withType(appResType)))

    var funsInScope = ctx.globalScope.filter{ // TODO Can't use localScope since we need to introduce y, below.... Must either do Let(y, ...) or eliminate all vars in localScope before binding....
      case (_, _: FunType) => true
      case _ => false
    }

    if (funsInScope.isEmpty) {
      genFromArg
    } else {
      val genFromScope = for {
        (f, FunType(x, xType, xResType)) <- oneOf(funsInScope.toSeq)
        (ctx2, y) <- ctx.newSymbol()
        (ctx3, yType) <- genSubtype(ctx2, Map(), xType) // TODO make it so that this can pick yType from scope.
        ctx4 <- ctx3.withBinding(y -> yType)
        appResType <- const(NoFuture.typeRenameVar(x, y, xResType))
      } yield (ctx4, InferenceProblem(App(f, y), Que, scope, App(f, y).withType(appResType)))
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
