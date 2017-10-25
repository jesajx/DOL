package exjobb

import exjobb.Dol._

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
    def withNewBinding(binding: (Symbol, Type)): GlobalContext =
      copy(globalScope = globalScope+binding)

    def withNewSymbol(): (GlobalContext, Symbol) =
      (copy(nextSymbol = nextSymbol+1), nextSymbol)

    def newBinding(binding: (Symbol, Type)): Gen[GlobalContext] =
      const(withNewBinding(binding))

    def newSymbol(): Gen[(GlobalContext, Symbol)] = // TODO rename to "genSymbol"?
      const(withNewSymbol())
  }



  sealed case class InferenceProblem(ctx: GlobalContext, term: Dol.Term, prototype: Prototype, expected: Typed.Term) {
    def scope = ctx.globalScope
    def su() = new SymbolUniverse(ctx.nextSymbol)
  }

  object InferenceProblem {
    type Term = InferenceProblem.Term.:-
    type Def  = InferenceProblem.Def.:-

    object Term {
      sealed case class :-(term: ITermLHS, typ: Type)
    }
    object Def {
      sealed case class :-(d: IDefLHS, typ: Type)
    }




    def assembleDef(p: InferenceProblem.Def): Dol.Def = p.d match {
      case IFieldDef(a, aTerm)  => Dol.FieldDef(a, assembleTerm(aTerm))
      case ITypeDef(a, aType)   => Dol.TypeDef(a, aType)
      case IAndDef(left, right) => Dol.AndDef(assembleDef(left), assembleDef(right))
    }

    def assembleTerm(p: InferenceProblem.Term): Dol.Term = p.term match {
      case IVar(x)                 => Dol.Var(x)
      case IApp(x, y)              => Dol.App(x, y)
      case ILet(x, xTerm, resTerm) => Dol.Let(x, assembleTerm(xTerm), assembleTerm(resTerm))
      case ISel(x, a)              => Dol.Sel(x, a)
      case IFun(x, xType, body)    => Dol.Fun(x, xType, assembleTerm(body))
      case IObj(x, xType, body)    => Dol.Obj(x, xType, assembleDef(body))
    }

    def assembleTypedDef(p: InferenceProblem.Def): Typed.Def = p.d match {
      case IFieldDef(a, aTerm)  => TypedFieldDef(a, assembleTypedTerm(aTerm))                   :- p.typ
      case ITypeDef(a, aType)   => TypedTypeDef(a, aType)                                       :- p.typ
      case IAndDef(left, right) => TypedAndDef(assembleTypedDef(left), assembleTypedDef(right)) :- p.typ
    }

    def assembleTypedTerm(p: InferenceProblem.Term): Typed.Term = p.term match {
      case IVar(x)                 => TypedVar(x)                                                       :- p.typ
      case IApp(x, y)              => TypedApp(x, y)                                                    :- p.typ
      case ILet(x, xTerm, resTerm) => TypedLet(x, assembleTypedTerm(xTerm), assembleTypedTerm(resTerm)) :- p.typ
      case ISel(x, a)              => TypedSel(x, a)                                                    :- p.typ
      case IFun(x, xType, body)    => TypedFun(x, xType, assembleTypedTerm(body))                       :- p.typ
      case IObj(x, xType, body)    => TypedObj(x, xType, assembleTypedDef(body))                        :- p.typ
    }


    // TODO generate stream of all subproblems, with local scope?

    def assemble(ctx: GlobalContext, p: InferenceProblem.Term): InferenceProblem =
      InferenceProblem(ctx, assembleTerm(p), Que, assembleTypedTerm(p))
  }

  sealed trait ITermLHS {
    def :-(typ: Type) = InferenceProblem.Term.:-(this, typ)
  }
  sealed trait IDefLHS {
    def :-(typ: Type) = InferenceProblem.Def.:-(this, typ)
  }

  case class IVar(x: Symbol)                                                               extends ITermLHS
  case class IApp(x: Symbol, y: Symbol)                                                    extends ITermLHS
  case class ILet(x: Symbol, xTerm: InferenceProblem.Term, resTerm: InferenceProblem.Term) extends ITermLHS
  case class ISel(x: Symbol, a: Symbol)                                                    extends ITermLHS
  case class IFun(x: Symbol, xType: Type, body: InferenceProblem.Term)                     extends ITermLHS
  case class IObj(x: Symbol, xType: Type, body: InferenceProblem.Def)                      extends ITermLHS

  case class IFieldDef(a: Symbol, aTerm: InferenceProblem.Term)               extends IDefLHS
  case class ITypeDef(a: Symbol, aType: Type)                                 extends IDefLHS
  case class IAndDef(left: InferenceProblem.Def, right: InferenceProblem.Def) extends IDefLHS

  object :- {
    def unapply(p: InferenceProblem.Term): Option[(ITermLHS, Type)] = Some((p.term, p.typ))
    def unapply(p: InferenceProblem.Def):  Option[(IDefLHS, Type)]  = Some((p.d, p.typ))
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
      (ctx4, res) <- Gen.resize(resSize, genType(ctx3, scope + (x -> arg))) // TODO two genSimpleFunType and genComplexFunType that lets res reference arg or not?
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
          // TODO If both `lower` and `upper` are prototypes it can lead to
          // scenarios where different constraint-variables are compared
          // against each other. It is probably easiest to just forbid this.
          // But can it occur naturally in the typechecker?
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
          if (NoFuture.allFreeVarsInType(resType).contains(x))
            for {
              // TODO If `res` references `arg` and `arg` is a prototype it can lead
              // to scenarios where we compare different constraint variables
              // against each-other... Not clear how to deal with this. Pierce
              // & Turner generally forbid constraints A <: B where both A and
              // B contain unknowns.
              // Perhaps we should differentiate prototypes from types. For
              // example `ProtoFun(arg,res)`, such that `res` can't reference
              // `arg`?
              // In general this seems to be the case in actual Scala, since
              // it needs to differentiate method-types from function-types
              // (since these are treated differently in the JVM).
              xPrototype <- const(xType)
              resPrototype <- genPrototypeFromType(resType)
            } yield FunType(x, xPrototype, resPrototype)
          else for {
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
            ctx6 <- ctx5.newBinding(x -> TypeDecl(a, lowerType, upperType))
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
      ctx6 <- ctx5.newBinding(x -> TypeDecl(a, aLowerType, aUpperType))
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




  // TODO IDEA: Maybe actually generate a proof (as a tree-datastructure), and
  // then generate inference-problems from that? The risk is that this becomes
  // as complicated as prolog...


  // TODO maybe only return TypedTerm? untype(TypedTerm):Term?
  // TODO maybe nest InferenceProblem instead of terms? can always translate
  // to term.




  // TODO gen with prototype.

  def genVarInferenceProblem(ctx: GlobalContext, localScope: Scope): Gen[(GlobalContext, InferenceProblem.Term)] = {
    val scope = ctx.globalScope ++ localScope

    val justMakeSomethingUp = for {
      (ctx2, x)     <- ctx.newSymbol()
      (ctx3, xType) <- genType(ctx2, Map()) // NOTE: globalScope to prevent the localScope leaking.
      ctx4          <- ctx3.newBinding(x -> xType)
    } yield (ctx4, IVar(x) :- xType)

    if (scope.isEmpty) {
      justMakeSomethingUp
    } else {
      val pickSomethingFromScope = for {
        (x, xType) <- oneOf(scope.toSeq)
      } yield (ctx, IVar(x) :- xType)
      Gen.frequency(
        (1, justMakeSomethingUp),
        (ctx.globalScope.size, pickSomethingFromScope))
    }
  }

  def genLetInferenceProblem(ctx: GlobalContext, localScope: Scope): Gen[(GlobalContext, InferenceProblem.Term)] = for {
    (ctx2, x) <- ctx.newSymbol()
    (ctx3, p1) <- genInferenceProblem(ctx2, localScope)
    (ctx4, p2) <- genInferenceProblem(ctx3, localScope + (x -> p1.typ))
    // TODO We would prefer not to call `eliminateVars` from the generator
    // since it is being tested...

    resType <- const(NoFuture.eliminateVars(ctx4.globalScope ++ localScope, Set(x), None, p2.typ)) // TODO zOption=Some(z) if p1.term is Var(z)?

  } yield (ctx4, ILet(x, p1, p2) :- resType)

  def genSelInferenceProblem(ctx: GlobalContext, localScope: Scope): Gen[(GlobalContext, InferenceProblem.Term)] = { // TODO Gen.sized
    val justMakeSomethingUp = for {
      (ctx2, x) <- ctx.newSymbol()
      (ctx3, a) <- ctx2.newSymbol()
      (ctx4, aType) <- genType(ctx3, Map()) // TODO obfuscate by adding other fields?
      ctx5 <- ctx4.newBinding(x -> FieldDecl(a, aType))
    } yield (ctx5, ISel(x, a) :- aType)
    val fieldRefs = NoFuture.directFieldDeclsInScope(ctx.globalScope ++ localScope)
    if (fieldRefs.isEmpty) {
      justMakeSomethingUp
    } else {
      val pickSomethingFromScope = for {
        (Seq(x, a), aType) <- oneOf(fieldRefs.toSeq)
      } yield (ctx, ISel(x, a) :- aType)
      Gen.frequency((1, justMakeSomethingUp), (fieldRefs.size, pickSomethingFromScope))
    }
  }


  // TODO def genAppInferenceProblem(su: SymbolUniverse, scope: Scope): Gen[InferenceProblem] // TODO search for funs in scope and generate term from argtype // TODO gen arg, gen super of arg.type, gen function taking super giving {gen term with type}

  //def genAppInferenceProblemFromScope(su: SymbolUniverse, scope: Scope): Gen[InferenceProblem] = // TODO find function in scope (possibly as field in object), generate term with that argtype

  //TODO def genTermFromType



  //TODO def genComplexObjType(su: SymbolUniverse, scope: Scope): Gen[Type]

  def genFieldDef(ctx: GlobalContext, localScope: Scope): Gen[(GlobalContext, InferenceProblem.Def)] = Gen.sized{ size =>
    if (size < 2)
      Gen.fail
    else for { // minSize = 2 = 1 FieldDef + 1 aTerm.
      (ctx2, a) <- ctx.newSymbol()
      (ctx3, p) <- Gen.resize(size-1, genInferenceProblem(ctx2, localScope))
      myType    <- const(FieldDecl(a, p.typ))
    } yield (ctx3, IFieldDef(a, p) :- myType)
  }

  def genTypeDef(ctx: GlobalContext, localScope: Scope): Gen[(GlobalContext, InferenceProblem.Def)] = Gen.sized{ size =>
    if (size < 2)
      Gen.fail
    else for { // minSize = 2 = 1 TypeDef + 1 aType.
      (ctx2, a)     <- ctx.newSymbol()
      (ctx3, aType) <- Gen.resize(size-1, genType(ctx2, localScope))
      myType        <- const(TypeDecl(a, aType, aType))
    } yield (ctx3, ITypeDef(a, aType) :- myType)
  }

  def genSimpleDef(ctx: GlobalContext, localScope: Scope, z: Symbol): Gen[(GlobalContext, InferenceProblem.Def)] = Gen.sized{ size =>
    if (size >= 5) {
      val genAndDef = for { // minSize = 5 = 1 AndDef + 2*min(FieldDef, TypeDef, AndDef).
        (leftSize, rightSize) <- splitSizeNonZero(size-1, min=2)
        (ctx2, left)  <- Gen.resize(leftSize, genSimpleDef(ctx, localScope, z))
        (ctx3, right) <- Gen.resize(rightSize, genSimpleDef(ctx2, localScope + (z -> left.typ), z))
      } yield (ctx3, IAndDef(left, right) :- AndType(left.typ, right.typ))
      oneOf(genFieldDef(ctx, localScope), genTypeDef(ctx, localScope), genAndDef)
    } else if (size >= 2)
      oneOf(genFieldDef(ctx, localScope), genTypeDef(ctx, localScope))
    else
      Gen.fail
  }

  def genSimpleObjInferenceProblem(ctx: GlobalContext, scope: Scope): Gen[(GlobalContext, InferenceProblem.Term)] = Gen.sized{ size =>
    if (size == 0)
      Gen.fail
    else for {
      (ctx2, x)    <- ctx.newSymbol()
      (ctx3, defs) <- Gen.resize(size - 1, genSimpleDef(ctx2, scope, x))
      xType        <- const(defs.typ) // TODO vs supertype. // TODO problem: how to determine size?
    } yield (ctx3, IObj(x, xType, defs) :- xType)
  }

  // TODO genComplexObjInferenceProblem
  // membersWithKnownTypes <- gen some (Symbol, Decl)
  // gen all other members:
  //  member x can see membersWithKnownTypes
  //  member x can see also member y if y < x
  //
  // gen defs for membersWithKnownTypes.
  //  member x can see all members.

  def genObjInferenceProblem(ctx: GlobalContext, scope: Scope): Gen[(GlobalContext, InferenceProblem.Term)] =
    genSimpleObjInferenceProblem(ctx, scope)
    // TODO genComplexObjInferenceProblem

  // TODO genInferenceProblemFromType: Modify genInferenceProblem to take a prototype and use fixed-point
  // to eliminate the prototype?

  def genAppInferenceProblem(ctx: GlobalContext, scope: Scope): Gen[(GlobalContext, InferenceProblem.Term)] = {
    val genFromArg = for {
      (ctx2, f)                                     <- ctx.newSymbol()
      (ctx3, y)                                     <- ctx2.newSymbol()
      (ctx4, funType @ FunType(x, xType, xResType)) <- genFunType(ctx3, Map()) // TODO make it so that this can generate a typeproj to a funtype
      ctx5                                          <- ctx4.newBinding(f -> funType)
      (ctx6, argSubtype)                            <- genSubtype(ctx5, Map(), xType)
      ctx7                                          <- ctx6.newBinding(y -> argSubtype)
      appResType                                    <- const(NoFuture.typeRenameVar(x, y, xResType))
    } yield (ctx7, IApp(f, y) :- appResType)

    var funsInScope = ctx.globalScope.filter{ // TODO Can't use localScope since we need to introduce y, below.... Must either do Let(y, ...) or eliminate all vars in localScope before binding....
      case (_, _: FunType) => true
      case _ => false
    }

    if (funsInScope.isEmpty) {
      genFromArg
    } else {
      val genFromScope = for {
        (f, FunType(x, xType, xResType)) <- oneOf(funsInScope.toSeq)
        (ctx2, y)                        <- ctx.newSymbol()
        (ctx3, yType)                    <- genSubtype(ctx2, Map(), xType) // TODO make it so that this can pick yType from scope.
        ctx4                             <- ctx3.newBinding(y -> yType)
        appResType                       <- const(NoFuture.typeRenameVar(x, y, xResType))
      } yield (ctx4, IApp(f, y) :- appResType)
      oneOf(genFromArg, genFromScope)
    }
  }


    // TODO also: genAppInferenceProblemFromScope

  def genFunInferenceProblem(ctx: GlobalContext, scope: Scope): Gen[(GlobalContext, InferenceProblem.Term)] = for {
    (ctx2, x)     <- ctx.newSymbol()
    (ctx3, xType) <- genType(ctx2, scope)
    (ctx4, body)  <- genInferenceProblem(ctx3, scope + (x -> xType))
  } yield (ctx4, IFun(x, xType, body) :- FunType(x, xType, body.typ))

  def genInferenceProblem(ctx: GlobalContext, scope: Scope): Gen[(GlobalContext, InferenceProblem.Term)] = oneOf(
    genVarInferenceProblem(ctx, scope),
    genLetInferenceProblem(ctx, scope),
    genSelInferenceProblem(ctx, scope),
    genFunInferenceProblem(ctx, scope),
    genAppInferenceProblem(ctx, scope),
    genObjInferenceProblem(ctx, scope)
  )

}
