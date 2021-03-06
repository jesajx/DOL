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
import Pretty._

object DolGenerators {
  // (su, prototype) -> (term, scope, typedterm)

  // TODO reuse symbols for bound vars

  // TODO Maybe let "1 node" == "size 0"? Since trees with zero nodes do not
  // make sense in DOL. This way some Gen.fail-cases can be avoided.

  // TODO INV: globalScope.keys.intersect(scope.keys) == Set()

  sealed case class GlobalContext(globalScope: Scope = Map(), nextSymbol: Int = 0) {
    def withNewBinding(binding: (Symbol, Type)): GlobalContext = {
      val (x, xType) = binding
      val freeVars = NoFuture.allFreeVarsInType(xType)
      if (!freeVars.subsetOf(globalScope.keySet + x))
        throw new Exception(s"leaking local scope $x -> ${freeVars -- (globalScope.keySet + x)}")
      if (x >= nextSymbol)
        throw new Exception(s"using symbol $x >= current counter $nextSymbol")
      copy(globalScope = globalScope+binding)
    }

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
    type Term = InferenceProblem.Term.:?
    type Def  = InferenceProblem.Def.:?

    type TermPrototype = InferenceProblem.Term.:%
    type DefPrototype  = InferenceProblem.Def.:%

    object Term {
      sealed case class :?(lhs: TermPrototype, rhs: Type) {
        def term = lhs.term
        def typ = rhs
      }
      sealed case class :%(lhs: ITermLHS, rhs: Prototype) {
        def term = lhs
        def prototype = rhs
        def :?(typ: Type): InferenceProblem.Term = InferenceProblem.Term.:?(this, typ)
      }
    }
    object Def {
      sealed case class :?(lhs: DefPrototype, rhs: Type) {
        def d = lhs.d
        def typ = rhs
      }
      sealed case class :%(lhs: IDefLHS, rhs: Prototype) {
        def d = lhs
        def prototype = rhs
        def :?(typ: Type): InferenceProblem.Def = InferenceProblem.Def.:?(this, typ)
      }
    }

    def fieldnames(p: InferenceProblem.Def): Set[Symbol] = p.d match {
      case IFieldDef(a, _)      => Set(a)
      case ITypeDef(_, _)       => Set()
      case IAndDef(left, right) => fieldnames(left) ++ fieldnames(right)
    }

    def remDef(p: InferenceProblem.Def, a: Symbol): Option[InferenceProblem.Def] = p match {
      case (IFieldDef(b, _) :% proto :? typ) => if (b == a) None else Some(p)
      case (ITypeDef(b, _)  :% proto :? typ) => if (b == a) None else Some(p)
      case (IAndDef(left, right) :% proto :? typ) =>
        List(remDef(left, a), remDef(right, a)).flatten.reduceOption[InferenceProblem.Def]{
          case (q1, q2) =>
            IAndDef(q1, q2) :% proto :? AndType(q1.typ, q2.typ)
        }
    }

    def typenames(p: InferenceProblem.Def): Set[Symbol] = p.d match {
      case IFieldDef(_, _)      => Set()
      case ITypeDef(a, _)       => Set(a)
      case IAndDef(left, right) => typenames(left) ++ typenames(right)
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
      case ITApp(funTerm, argDefs) => Dol.TApp(assembleTerm(funTerm), assembleDef(argDefs))
    }

    def assembleTypedDef(p: InferenceProblem.Def): Typed.Def = p.d match {
      case IFieldDef(a, aTerm)  => TypedFieldDef(a, assembleTypedTerm(aTerm))                   :- p.typ
      case ITypeDef(a, aType)   => TypedTypeDef(a, aType)                                       :- p.typ
      case IAndDef(left, right) => TypedAndDef(assembleTypedDef(left), assembleTypedDef(right)) :- p.typ
    }

    def assembleTypedTerm(p: InferenceProblem.Term): Typed.Term = p.term match {
      case IVar(x)                 => TypedVar(x)                                                         :- p.typ
      case IApp(x, y)              => TypedApp(x, y)                                                      :- p.typ
      case ILet(x, xTerm, resTerm) => TypedLet(x, assembleTypedTerm(xTerm), assembleTypedTerm(resTerm))   :- p.typ
      case ISel(x, a)              => TypedSel(x, a)                                                      :- p.typ
      case IFun(x, xType, body)    => TypedFun(x, xType, assembleTypedTerm(body))                         :- p.typ
      case IObj(x, xType, body)    => TypedObj(x, xType, assembleTypedDef(body))                          :- p.typ
      case ITApp(funTerm, argDefs) => TypedTApp(assembleTypedTerm(funTerm), assembleTypedDef(argDefs)) :- p.typ
    }


    // TODO generate stream of all subproblems, with local scope?

    def assemble(ctx: GlobalContext, p: InferenceProblem.Term): InferenceProblem =
      InferenceProblem(ctx, assembleTerm(p), Que, assembleTypedTerm(p))

    def defSubproblems(ctx: GlobalContext, p: InferenceProblem.Def): Stream[(GlobalContext, InferenceProblem.Term)] = p.d match {
      case IFieldDef(a, aTerm)  => Stream((ctx, aTerm))
      case ITypeDef(a, aType)   => Stream()
      case IAndDef(left, right) => defSubproblems(ctx, left) #::: defSubproblems(ctx, right)
    }

    def subproblems(ctx: GlobalContext, p: InferenceProblem.Term): Stream[(GlobalContext, InferenceProblem.Term)] = p.term match {
      case ILet(x, xTerm, resTerm) => Stream((ctx, xTerm), (ctx.withNewBinding(x -> xTerm.typ), resTerm))
      case IFun(x, xType, body)    => Stream((ctx.withNewBinding(x -> xType), body))
      case IObj(x, xType, body)    => defSubproblems(ctx.withNewBinding(x -> xType), body)
      case ITApp(funTerm, argDefs) => (ctx, funTerm) #:: defSubproblems(ctx, argDefs)
      case _ => Stream()
    }

    def defFreeVars(p: InferenceProblem.Def): Set[Symbol] = {
      val defFree = p.d match {
        case IFieldDef(a, aTerm)  => freeVars(aTerm)
        case ITypeDef(a, aType)   => NoFuture.allFreeVarsInType(aType)
        case IAndDef(left, right) => defFreeVars(left) ++ defFreeVars(right)
      }
      val typeFree = NoFuture.allFreeVarsInType(p.typ)
      defFree ++ typeFree
    }

    def freeVars(p: InferenceProblem.Term): Set[Symbol] = {
      val termFree = p.term match {
        case IVar(x)                 => Set(x)
        case IApp(x, y)              => Set(x, y)
        case ILet(x, xTerm, resTerm) => (freeVars(xTerm) ++ freeVars(resTerm)) - x
        case ISel(x, a)              => Set(x)
        case IFun(x, xType, body)    => (NoFuture.allFreeVarsInType(xType) ++ freeVars(body)) - x
        case IObj(x, xType, body)    => (NoFuture.allFreeVarsInType(xType) ++ defFreeVars(body)) - x
        case ITApp(funTerm, argDefs) => freeVars(funTerm) ++ defFreeVars(argDefs)
      }
      val typeFree = NoFuture.allFreeVarsInType(p.typ)
      termFree ++ typeFree
    }

    def wellFormed(ctx: GlobalContext, p: InferenceProblem.Term): Boolean = {
      val scope = ctx.globalScope
      freeVars(p).subsetOf(scope.keySet)
      subproblems(ctx, p).forall{case (ctx2, p2) => wellFormed(ctx2, p2)}
    }
  }

  sealed trait ITermLHS {
    def :%(rhs: Prototype) = InferenceProblem.Term.:%(this, rhs)
    //def :?(rhs: Type) = (this :% Que) :? rhs
  }
  sealed trait IDefLHS {
    def :%(rhs: Prototype) = InferenceProblem.Def.:%(this, rhs)
    //def :?(rhs: Type) = (this :% Que) :? rhs
  }

  object :? {
    def apply(lhs: InferenceProblem.TermPrototype, rhs: Type): InferenceProblem.Term = InferenceProblem.Term.:?(lhs, rhs)
    def apply(lhs: InferenceProblem.DefPrototype, rhs: Type): InferenceProblem.Def = InferenceProblem.Def.:?(lhs, rhs)

    def unapply(p: InferenceProblem.Term): Option[(InferenceProblem.TermPrototype, Type)] = Some((p.lhs, p.rhs))
    def unapply(p: InferenceProblem.Def):  Option[(InferenceProblem.DefPrototype, Type)]  = Some((p.lhs, p.rhs))
  }

  object :% {
    def apply(lhs: ITermLHS, rhs: Prototype): InferenceProblem.TermPrototype = InferenceProblem.Term.:%(lhs, rhs)
    def apply(lhs: IDefLHS, rhs: Prototype): InferenceProblem.DefPrototype = InferenceProblem.Def.:%(lhs, rhs)

    def unapply(p: InferenceProblem.TermPrototype): Option[(ITermLHS, Type)] = Some((p.lhs, p.rhs))
    def unapply(p: InferenceProblem.DefPrototype):  Option[(IDefLHS, Type)]  = Some((p.lhs, p.rhs))
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

  case class ITApp(funTerm: InferenceProblem.Term, argDefs: InferenceProblem.Def)      extends ITermLHS

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
    } yield (ctx4, IVar(x) :% Que :? xType)

    if (scope.isEmpty) {
      justMakeSomethingUp
    } else {
      val pickSomethingFromScope = for {
        (x, xType) <- oneOf(scope.toSeq)
      } yield (ctx, IVar(x) :% Que :? xType)
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

    resType <- const(NoFuture.eliminateVars(ctx4.globalScope ++ localScope + (x -> p1.typ), Set(x), None, p2.typ)) // TODO zOption=Some(z) if p1.term is Var(z)?

  } yield (ctx4, ILet(x, p1, p2) :% Que :? resType)

  def genSelInferenceProblem(ctx: GlobalContext, localScope: Scope): Gen[(GlobalContext, InferenceProblem.Term)] = { // TODO Gen.sized
    val justMakeSomethingUp = for {
      (ctx2, x) <- ctx.newSymbol()
      (ctx3, a) <- ctx2.newSymbol()
      (ctx4, aType) <- genType(ctx3, Map()) // TODO obfuscate by adding other fields?
      ctx5 <- ctx4.newBinding(x -> FieldDecl(a, aType))
    } yield (ctx5, ISel(x, a) :% Que :? aType)
    val scope = ctx.globalScope ++ localScope
    val selectableObjectsInScope = scope.map{case (x, xType) =>
      x -> NoFuture.objStdDecl(scope, x, xType)
    }.filter{case (_, o) => !o.fields.isEmpty || o.hasBot}

    if (selectableObjectsInScope.isEmpty) {
      justMakeSomethingUp
    } else {
      val pickSomethingFromScope = for {
        (x, o) <- oneOf(selectableObjectsInScope.toSeq)

        (ctx2, a, aType) <- {
          val alt1 = o.fields.toSeq.map{
            case (a, FieldDecl(_, aType)) => (ctx, a, aType)
            case (a, Bot) => (ctx, a, Bot)
            case _ => ???
          }
          val (ctx2, b) = ctx.withNewSymbol
          val alt2 = if (o.hasBot) Seq((ctx, b, Bot)) else Seq()
          oneOf(alt1 ++ alt2)
        }

      } yield (ctx2, ISel(x, a) :% Que :? aType)
      val pickSomethingFromScopeCount = selectableObjectsInScope.map{case (_, o) =>
        o.fields.size + (if (o.hasBot) 1 else 0)
      }.sum
      Gen.frequency((1, justMakeSomethingUp), (pickSomethingFromScopeCount, pickSomethingFromScope))
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
      (ctx3, p) <- Gen.resize(size-1, genInferenceProblem(ctx2, localScope)) // TODO this may end up generating a var that references the enclosing object, but the type will be wrong since we do not know the whole type here.
      myType    <- const(FieldDecl(a, p.typ))
    } yield (ctx3, IFieldDef(a, p) :% Que :? myType) // TODO Que is wrong
  }

  def genTypeDef(ctx: GlobalContext, localScope: Scope): Gen[(GlobalContext, InferenceProblem.Def)] = Gen.sized{ size =>
    if (size < 2)
      Gen.fail
    else for { // minSize = 2 = 1 TypeDef + 1 aType.
      (ctx2, a)     <- ctx.newSymbol()
      (ctx3, aType) <- Gen.resize(size-1, genType(ctx2, localScope))
      myType        <- const(TypeDecl(a, aType, aType))
    } yield (ctx3, ITypeDef(a, aType) :% Que :? myType) // TODO Que is wrong
  }

  def genSimpleDef(ctx: GlobalContext, localScope: Scope, z: Symbol): Gen[(GlobalContext, InferenceProblem.Def)] = Gen.sized{ size =>
    if (size >= 5) {
      val genAndDef = for { // minSize = 5 = 1 AndDef + 2*min(FieldDef, TypeDef, AndDef).
        (leftSize, rightSize) <- splitSizeNonZero(size-1, min=2)
        (ctx2, left)  <- Gen.resize(leftSize, genSimpleDef(ctx, localScope, z))
        zTypeIncludingLeft <- const(localScope.get(z) match {
          case Some(zType) => AndType(zType, left.typ)
          case None => left.typ
        })
        (ctx3, right) <- Gen.resize(rightSize, genSimpleDef(ctx2, localScope + (z -> zTypeIncludingLeft), z))
      } yield (ctx3, IAndDef(left, right) :% Que :? AndType(left.typ, right.typ)) // TODO Que is wrong
      oneOf(genFieldDef(ctx, localScope - z), genTypeDef(ctx, localScope - z), genAndDef)
    } else if (size >= 2)
      oneOf(genFieldDef(ctx, localScope - z), genTypeDef(ctx, localScope - z))
    else
      Gen.fail
  }

  def genSimpleObjInferenceProblem(ctx: GlobalContext, scope: Scope): Gen[(GlobalContext, InferenceProblem.Term)] = Gen.sized{ size =>
    if (size == 0)
      Gen.fail
    else for {
      (ctx2, x)    <- ctx.newSymbol()
      (ctx3, defs) <- Gen.resize(size - 1, genSimpleDef(ctx2, scope, x))
      xType        <- const(defs.typ) // TODO vs supertype. // TODO problem: how to determine size for supertype?
    } yield (ctx3, IObj(x, xType, defs) :% Que :? RecType(x, xType))
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
      (ctx2, f) <- ctx.newSymbol()
      (ctx3, y) <- ctx2.newSymbol()
      (ctx4, funType @ FunType(x, xType, xResType)) <- genFunType(ctx3, Map()) // TODO make it so that this can generate a typeproj to a funtype
      ctx5 <- ctx4.newBinding(f -> funType)
      (ctx6, argSubtype)                            <- genSubtype(ctx5, Map(), xType)
      ctx7 <- ctx6.newBinding(y -> argSubtype)
      appResType <- const(NoFuture.typeRenameVar(x, y, xResType))
    } yield (ctx7, IApp(f, y) :% Que :? appResType)

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
        ctx4 <- ctx3.newBinding(y -> yType)
        appResType <- const(NoFuture.typeRenameVar(x, y, xResType))
      } yield (ctx4, IApp(f, y) :% Que :? appResType)
      oneOf(genFromArg, genFromScope)
    }
  }


    // TODO also: genAppInferenceProblemFromScope

  def genFunInferenceProblem(ctx: GlobalContext, scope: Scope): Gen[(GlobalContext, InferenceProblem.Term)] = for {
    (ctx2, x)     <- ctx.newSymbol()
    (ctx3, xType) <- genType(ctx2, scope)
    (ctx4, body)  <- genInferenceProblem(ctx3, scope + (x -> xType))
  } yield (ctx4, IFun(x, xType, body) :% Que :? FunType(x, xType, body.typ))

  def genInferenceProblem(ctx: GlobalContext, scope: Scope): Gen[(GlobalContext, InferenceProblem.Term)] = oneOf(
    genVarInferenceProblem(ctx, scope),
    genLetInferenceProblem(ctx, scope),
    genSelInferenceProblem(ctx, scope),
    genFunInferenceProblem(ctx, scope),
    genAppInferenceProblem(ctx, scope),
    genObjInferenceProblem(ctx, scope)
  )

  def genTypeFromPrototype(ctx: GlobalContext, localScope: Scope, prototype: Prototype): Gen[(GlobalContext, Type)] = prototype match {
    case Que => genType(ctx, localScope)
    case FunType(x, xPrototype, xResPrototype) =>
      for { // TODO sized split
        (ctx2, xType) <- genTypeFromPrototype(ctx, localScope, xPrototype)
        (ctx3, xResType) <- genTypeFromPrototype(ctx2, localScope + (x -> xType), xResPrototype)
      } yield (ctx3, FunType(x, xType, xResType))
    case AndType(leftPrototype, rightPrototype) =>
      for { // TODO sized split
        (ctx2, leftType) <- genTypeFromPrototype(ctx, localScope, leftPrototype)
        (ctx3, rightType) <- genTypeFromPrototype(ctx2, localScope, rightPrototype)
      } yield (ctx3, AndType(leftType, rightType))
    case FieldDecl(a, aPrototype) =>
      for {
        (ctx2, aType) <- genTypeFromPrototype(ctx, localScope, aPrototype)
      } yield (ctx2, FieldDecl(a, aType))
    case TypeDecl(a, aLowerPrototype, aUpperProtoype) =>
      if (!NoFuture.isPrototype(prototype)) const(ctx -> prototype)
      else ??? // TODO must make sure aLower <: aUpper...
    case _ => // Bot, Top, TypeProj, RecType
      if (NoFuture.isPrototype(prototype)) ???
      const(ctx -> prototype)
  }

  def genDefFromPrototype(ctx: GlobalContext, localScope: Scope, prototype: Prototype): Gen[(GlobalContext, InferenceProblem.Def)] = Gen.sized {size =>
    prototype match {
      case Que =>
        oneOf(
          for {
            (ctx2, a) <- ctx.newSymbol()
            (ctx3, aTerm) <- genInferenceProblemFromPrototype(ctx2, localScope, Que)
          } yield (ctx3, IFieldDef(a, aTerm) :% Que :? FieldDecl(a, aTerm.typ)),
          for {
            (ctx2, a) <- ctx.newSymbol()
            (ctx3, aType) <- genType(ctx2, localScope)
          } yield (ctx3, ITypeDef(a, aType) :% Que :? TypeDecl(a, aType, aType)),
          for {
            (ctx2, left) <- genDefFromPrototype(ctx, localScope, Que)
            (ctx3, right) <- genDefFromPrototype(ctx2, localScope, Que)
          } yield (ctx3, IAndDef(left, right) :% Que :? AndType(left.typ, right.typ))
        )
      case Top =>
        oneOf(
          for {
            (ctx2, a) <- ctx.newSymbol()
            (ctx3, aTerm) <- genInferenceProblemFromPrototype(ctx2, localScope, Top)
          } yield (ctx3, IFieldDef(a, aTerm) :% Top :? Top),
          for {
            (ctx2, a) <- ctx.newSymbol()
            (ctx3, aType) <- genType(ctx2, localScope)
          } yield (ctx3, ITypeDef(a, aType) :% Top :? Top),
          for {
            (ctx2, left) <- genDefFromPrototype(ctx, localScope, Top)
            (ctx3, right) <- genDefFromPrototype(ctx2, localScope, Top)
          } yield (ctx3, IAndDef(left, right) :% Top :? Top)
        )
      case FieldDecl(a, aPrototype) =>
        for {
          (ctx2, aTerm) <- genInferenceProblemFromPrototype(ctx, localScope, aPrototype)
        } yield (ctx2, IFieldDef(a, aTerm) :% prototype :? FieldDecl(a, aTerm.typ))
      case TypeDecl(a, aLowerPrototype, Que) =>
        for {
          (ctx2, aType) <- genTypeFromPrototype(ctx, localScope, aLowerPrototype)
        } yield (ctx2, ITypeDef(a, aType) :% prototype :? TypeDecl(a, aType, aType))
      case TypeDecl(a, Que, aUpperPrototype) =>
        for {
          (ctx2, aType) <- genTypeFromPrototype(ctx, localScope, aUpperPrototype)
        } yield (ctx2, ITypeDef(a, aType) :% prototype :? TypeDecl(a, aType, aType))
      case TypeDecl(a, aLowerType, aUpperType) if !NoFuture.isPrototype(prototype) =>
        for {
          (ctx2, aType) <- genTypeFromPrototype(ctx, localScope, aLowerType) // TODO vs upper? vs somewhere between?
        } yield (ctx2, ITypeDef(a, aType) :% prototype :? TypeDecl(a, aLowerType, aUpperType))
      //case TypeDecl(a, aLowerPrototype, aUpperPrototype) =>
      //  ??? // TODO aLowerPrototype <: aType <: aUpperPrototype
      case AndType(leftPrototype, rightPrototype) =>
        for { // TODO size split
          (ctx2, left) <- genDefFromPrototype(ctx, localScope, leftPrototype)
          (ctx3, right) <- genDefFromPrototype(ctx2, localScope, rightPrototype)
        } yield (ctx3, IAndDef(left, right) :% prototype :? AndType(left.typ, right.typ))
      //case TypeProj(x, a) =>
      //  // TODO Can't this lead to duplicate definitions? caller should check...
      //  val aLowerType = NoFuture.typeProjectLower(ctx.globalScope ++ localScope, x, a).get // TODO watch out for inf rec?
      //  for {
      //    (ctx2, d) <- genDefFromPrototype(ctx, localScope, aLowerType)
      //  } yield (ctx2, d)
      //case _ => ???
    }
  }

  def genVarInferenceProblemFromPrototype(ctx: GlobalContext, localScope: Scope, prototype: Prototype): Gen[(GlobalContext, InferenceProblem.Term)] = {
    // TODO search scope? only if !isPrototype(prototype) or prototype=Que?
    // TODO use NoFuture.varRaise? Should be fine assuming it has been tested "sufficiently"...

    val prototype2 = NoFuture.eliminateVars(ctx.globalScope ++ localScope, localScope.keySet, None, prototype, Contravariant)
    val justMakeSomethingUp = for {
      (ctx2, x)     <- ctx.newSymbol()
      (ctx3, xType) <- genTypeFromPrototype(ctx2, Map(), prototype2)
      ctx4          <- ctx3.newBinding(x -> xType)
    } yield (ctx4, IVar(x) :% prototype :? xType)

    justMakeSomethingUp
  }


  def genLetInferenceProblemFromPrototype(ctx: GlobalContext, localScope: Scope, prototype: Prototype): Gen[(GlobalContext, InferenceProblem.Term)] = for {
    (ctx2, x) <- ctx.newSymbol()
    (ctx3, p1) <- genInferenceProblemFromPrototype(ctx2, localScope, Que) // TODO RecType(x, prototype)?
    (ctx4, p2) <- genInferenceProblemFromPrototype(ctx3, localScope + (x -> p1.typ), prototype)

    resType <- const(NoFuture.eliminateVars(ctx4.globalScope ++ localScope + (x -> p1.typ), Set(x), None, p2.typ)) // TODO zOption=Some(z) if p1.term is Var(z)?
    // TODO if `p2` generated global var `x`, make it local by letting `p1=Var(x)`?
  } yield (ctx4, ILet(x, p1, p2) :% prototype :? resType)



  def genSelInferenceProblemFromPrototype(ctx: GlobalContext, localScope: Scope, prototype: Prototype): Gen[(GlobalContext, InferenceProblem.Term)] = {
    // TODO search scope for matches?
    // TODO genVarInferenceProblemFromPrototype(ctx, localScope, FieldDecl(a, prototype)?

    val prototype2 = NoFuture.eliminateVars(ctx.globalScope ++ localScope, localScope.keySet, None, prototype, Contravariant)
    val justMakeSomethingUp = for {
      (ctx2, x) <- ctx.newSymbol()
      (ctx3, a) <- ctx2.newSymbol()
      (ctx4, aType) <- genTypeFromPrototype(ctx3, Map(), prototype2)
      ctx5 <- ctx4.newBinding(x -> FieldDecl(a, aType))
    } yield (ctx5, ISel(x, a) :% prototype :? aType)
    justMakeSomethingUp
  }

  def genAppInferenceProblemFromPrototype(ctx: GlobalContext, localScope: Scope, prototype: Prototype): Gen[(GlobalContext, InferenceProblem.Term)] = {
    // TODO grab fun from scope?
    // TODO grab arg from scope?

    val prototype2 = NoFuture.eliminateVars(ctx.globalScope ++ localScope, localScope.keySet, None, prototype, Contravariant)
    val genFromRes = for {
      (ctx2, f) <- ctx.newSymbol()
      (ctx3, y) <- ctx2.newSymbol()
      (ctx4, x) <- ctx3.newSymbol()
      (ctx5, funType @ FunType(_, xType, xResType)) <- genTypeFromPrototype(ctx4, Map(), FunType(x, Que, prototype2))
      ctx6 <- ctx5.withNewBinding(f -> funType)
      ctx7 <- ctx6.withNewBinding(y -> xType)

      appResType <- const(NoFuture.typeRenameVar(x, y, xResType))
    } yield (ctx7, IApp(f, y) :% prototype :? appResType)

    //val genTApp = for {
    //  (ctx2, x) <- ctx.newSymbol()
    //  (ctx3, argType) <- genRecType(ctx2, localScope) // TODO since we have a variable for unwrapping argDefs, this does not have to be recursive.
    //  (ctx4, funTerm) <- genInferenceProblemFromPrototype(ctx3, localScope, FunType(x, argType, prototype)) // TODO wrong. must be Que.
    //  (FunType(w, wType, wResType)) <- const(funTerm.typ)
    //  argPrototype <- const(NoFuture.objStdDecl(ctx4.globalScope ++ localScope, x, wType).toType)
    //  argPrototype2 <- const(NoFuture.eliminateVars(ctx4.globalScope ++ localScope + (x -> argPrototype), Set(x), Some(x), argPrototype, Contravariant))
    //  (ctx5, argDefs) <- genDefFromPrototype(ctx4, localScope, argPrototype2) // TODO leave out some typeargs to be inferred
    //  appResType <- const(NoFuture.eliminateVars(ctx5.globalScope ++ localScope + (x -> argDefs.typ), Set(x), None, NoFuture.typeRenameVar(w, x, wResType)))
    //} yield (ctx5, ITApp(funTerm, argDefs) :% prototype :? appResType)

    //oneOf(genFromRes, genTApp)

    genFromRes
  }

  def genObjInferenceProblemFromPrototype(ctx: GlobalContext, localScope: Scope, prototype: Prototype): Gen[(GlobalContext, InferenceProblem.Term)] = prototype match {
    case Que =>
      for {
        (ctx2, RecType(x, xType)) <- genRecType(ctx, localScope)
        defType <- const(NoFuture.objStdDecl(ctx2.globalScope ++ localScope, x, xType).toType)
        (ctx3, defs) <- genDefFromPrototype(ctx2, localScope + (x -> xType), defType) // TODO check that xType makes is not a function or typeproj?
      } yield (ctx3, IObj(x, xType, defs) :% Que :? RecType(x, xType))
    case Top =>
      for {
        (ctx2, x) <- ctx.newSymbol()
        (ctx3, defs) <- genDefFromPrototype(ctx2, localScope, Top)
      } yield (ctx3, IObj(x, Top, defs) :% Top :? Top)
    case RecType(x, xType) =>
      if (NoFuture.isPrototype(xType))
        ???
      for {
        defType <- const(NoFuture.objStdDecl(ctx.globalScope ++ localScope, x, xType).toType)
        (ctx2, defs) <- genDefFromPrototype(ctx, localScope + (x -> xType), defType) // TODO check that xType makes is not a function or typeproj?
      } yield (ctx2, IObj(x, xType, defs) :% prototype :? prototype)
    case _ => ??? // NOTE: We can't raise a rectype to fielddecl/typedecl without a var.
  }

  def commonsubFun(scope: Scope, left: Type, right: Type): Option[Type] = (left, right) match {
    case (Top, Que) => Some(Top) // TODO vs Que?
    case (Que, Top) => Some(Top)
    case (Top | Que, _) => Some(right)
    case (_, Top | Que) => Some(left)
    case (FunType(x, xType, xResType), FunType(y, yType, yResType)) =>
      None
      // TODO bad: may end up with argType=Top and resType=arg.T
      //Some(FunType(x,
      //  NoFuture.leastCommonSupertype(scope, xType, yType),
      //  NoFuture.greatestCommonSubtype(scope, xResType, NoFuture.typeRenameBoundVarAssumingNonFree(x, yResType))))
    case _ => None
  }

  def asFun(scope: Scope, typ: Prototype): Option[Type] = typ match {
    case fun: FunType => Some(fun)
    case Top => Some(Top)
    case Que => Some(Que)
    case TypeProj(x, a) =>
      asFun(scope, NoFuture.typeProjectLower(scope, x, a).get) // TODO watch out for inf rec?
    case AndType(left, right) =>
      for {
        leftFun <- asFun(scope, left)
        rightFun <- asFun(scope, right)
        commonFun <- commonsubFun(scope, leftFun, rightFun)
      } yield commonFun
    case _ => None
  }

  def commonsubObj(scope: Scope, left: Type, right: Type): Option[Type] = (left, right) match {
    case (Top | Que, _) => Some(right)
    case (_, Top | Que) => Some(left)
    case _ => None
  }

  def asObj(scope: Scope, typ: Prototype): Option[Type] = typ match {
    case rec: RecType => Some(rec) // TODO check inside rec?
    case Top => Some(Top)
    case Que => Some(Que)
    case TypeProj(x, a) =>
      asObj(scope, NoFuture.typeProjectLower(scope, x, a).get) // TODO watch out for inf rec?
    case AndType(left, right) =>
      for {
        leftObj <- asObj(scope, left)
        rightObj <- asObj(scope, right)
        res <- commonsubObj(scope, leftObj, rightObj)
      } yield res
    case _ => None
  }

  def genFunInferenceProblemFromPrototype(ctx: GlobalContext, localScope: Scope, prototype: Prototype): Gen[(GlobalContext, InferenceProblem.Term)] = prototype match {
    case Top =>
      for {
        (ctx2, x) <- ctx.newSymbol()
        (ctx3, p) <- genFunInferenceProblemFromPrototype(ctx2, localScope, FunType(x, Bot, Top))
      } yield (ctx3, p.term :% Top :? Top)
    case Que =>
      for {
        (ctx2, x) <- ctx.newSymbol()
        (ctx3, p) <- genFunInferenceProblemFromPrototype(ctx2, localScope, FunType(x, Que, Que))
      } yield (ctx3, p.term :% Que :? p.typ)
    case FunType(y, yPrototype, yResPrototype) =>
      if (yPrototype == Top && NoFuture.allFreeVarsInType(yResPrototype).contains(y))???
      for { // TODO what if yResPrototype=TypeProj(y,a) and yPrototype=Que? can this happen?
        (ctx2, x) <- ctx.newSymbol()
        (ctx3, yType) <- genTypeFromPrototype(ctx2, localScope, yPrototype)
        xResPrototype  <- const(NoFuture.typeRenameVar(y, x, yResPrototype))
        (ctx4, body) <- genInferenceProblemFromPrototype(ctx3, localScope + (x -> yType), xResPrototype)
      } yield (ctx4, IFun(x, yType, body) :% prototype :? FunType(x, yType, body.typ))
    case _ =>
      ???
  }


  def cond[T](c: Boolean)(x: => T): Option[T] = if (c) Some(x) else None

  sealed case class TypeClassification(isRecursive: Boolean = false, isField: Boolean = false, isTypeDecl: Boolean = false, isFunction: Boolean = false, isProj: Boolean = false, isBot: Boolean = false, isTop: Boolean = false, isQue: Boolean = false) {
    def merge(that: TypeClassification): TypeClassification =
      TypeClassification(
        this.isRecursive || that.isRecursive,
        this.isField || that.isField,
        this.isTypeDecl || that.isTypeDecl,
        this.isFunction || that.isFunction,
        this.isProj || that.isProj,
        this.isBot || that.isBot,
        this.isTop || that.isTop,
        this.isQue || that.isQue)
  }

  def typeClassify(scope: Scope, typ: Prototype): TypeClassification = {
    def inner(scope: Scope, typ: Prototype, visited: Set[TypeProj]): TypeClassification = typ match {
      case RecType(x, xType) =>
        inner(scope + (x -> xType), xType, visited).merge(TypeClassification(isRecursive=true))
      case FieldDecl(a, _)       => TypeClassification(isField=true)
      case TypeDecl(a, _, _)     => TypeClassification(isTypeDecl=true)
      case proj @ TypeProj(y, a) =>
        val upperCl = inner(scope, NoFuture.typeProjectUpper(scope, y, a).get, visited + proj)
        val lowerCl = inner(scope, NoFuture.typeProjectLower(scope, y, a).get, visited + proj)
        upperCl.merge(lowerCl).merge(TypeClassification(isProj=true))
      case Bot => TypeClassification(isBot=true)
      case Top => TypeClassification(isTop=true)
      case Que => TypeClassification(isBot=true)
      case AndType(left, right) =>
        inner(scope, left, visited).merge(inner(scope, right, visited))
      case FunType(x, xType, xResType) => TypeClassification(isFunction=true)
      case _ => ???
    }

    inner(scope, typ, Set())
  }


  def genInferenceProblemFromPrototype(ctx: GlobalContext, localScope: Scope, prototype: Prototype): Gen[(GlobalContext, InferenceProblem.Term)] = {
    val cl = typeClassify(ctx.globalScope ++ localScope, prototype)

    val isDecl = cl.isField || cl.isTypeDecl

    // TODO problem: might be andtype where only one part is recursive while
    // the rest is not.

    val funOpt = asFun(ctx.globalScope ++ localScope, prototype).map{prototype2 =>
      for {
        (ctx2, p) <- genFunInferenceProblemFromPrototype(ctx, localScope, prototype2)
        //_ <- const{
        //  if (prototype == Top && p.typ == Top) ???
        //}
      } yield (ctx2, p.term :% prototype :? p.typ)
    }
    val objOpt = asObj(ctx.globalScope ++ localScope, prototype).filter{_ => !cl.isBot && !cl.isFunction}.map{prototype2 =>
      for {
        (ctx2, p) <- genObjInferenceProblemFromPrototype(ctx, localScope, prototype2)
        //_ <- const{
        //  if (prototype == Top && p.typ == Top) ???
        //}
      } yield (ctx2, p.term :% prototype :? p.typ)
    }


    oneOfGens(
      List(funOpt, objOpt).flatten ++ List(
        genVarInferenceProblemFromPrototype(ctx, localScope, prototype),
        genLetInferenceProblemFromPrototype(ctx, localScope, prototype),
        genSelInferenceProblemFromPrototype(ctx, localScope, prototype),
        genAppInferenceProblemFromPrototype(ctx, localScope, prototype)))
  }

  object EqCheck {
    type Term = EqCheck.Term.:!
    type Def  = EqCheck.Def.:!
    object Term {
      sealed case class :!(term: EqCheckTermLHS, ok: EqCheck.Res)
    }
    object Def {
      sealed case class :!(d: EqCheckDefLHS, ok: EqCheck.Res)
    }

    sealed trait Res
    case object OK extends Res
    case class Wrong(res: Type, expected: Type) extends Res
  }

  sealed trait EqCheckTermLHS {
    def :!(ok: EqCheck.Res) = EqCheck.Term.:!(this, ok)
  }
  sealed trait EqCheckDefLHS {
    def :!(ok: EqCheck.Res) = EqCheck.Def.:!(this, ok)
  }

  case class EqCheckVar(x: Symbol)                                         extends EqCheckTermLHS
  case class EqCheckApp(x: Symbol, y: Symbol)                              extends EqCheckTermLHS
  case class EqCheckLet(x: Symbol, xTerm: EqCheck.Term, resTerm: EqCheck.Term) extends EqCheckTermLHS
  case class EqCheckSel(x: Symbol, a: Symbol)                              extends EqCheckTermLHS
  case class EqCheckFun(x: Symbol, xType: Type, body: EqCheck.Term)          extends EqCheckTermLHS
  case class EqCheckObj(x: Symbol, xType: Type, body: EqCheck.Def)           extends EqCheckTermLHS

  case class EqCheckFieldDef(a: Symbol, aTerm: EqCheck.Term)    extends EqCheckDefLHS
  case class EqCheckTypeDef(a: Symbol, aType: Type)           extends EqCheckDefLHS
  case class EqCheckAndDef(left: EqCheck.Def, right: EqCheck.Def) extends EqCheckDefLHS

  case class EqCheckTApp(funTerm: EqCheck.Term, argDef: EqCheck.Def)   extends EqCheckTermLHS

  object :! {
    def apply(lhs: EqCheckTermLHS, rhs: EqCheck.Res): EqCheck.Term = lhs :! rhs
    def apply(lhs: EqCheckDefLHS, rhs: EqCheck.Res): EqCheck.Def = lhs :! rhs
    def unapply(t: EqCheck.Term): Option[(EqCheckTermLHS, EqCheck.Res)] = Some((t.term, t.ok))
    def unapply(t: EqCheck.Def): Option[(EqCheckDefLHS, EqCheck.Res)] = Some((t.d, t.ok))
  }

  def eqcheckDef(scope: Scope, first: Typed.Def, second: Typed.Def): EqCheck.Def = {
      val (leftDef :- leftType)   = first
      val (rightDef :- rightType) = second
      val res =
        if (NoFuture.equalTypes(scope, leftType, rightType))
          EqCheck.OK
        else
          EqCheck.Wrong(leftType, rightType)
      val lhs = (leftDef, rightDef) match {
        case (TypedAndDef(l1, r1), TypedAndDef(l2, r2)) =>
          EqCheckAndDef(
            eqcheckDef(scope, l1, l2),
            eqcheckDef(scope, r1, r2)
          )
        case (TypedFieldDef(a, aTerm), TypedFieldDef(b, bTerm)) =>
          EqCheckFieldDef(a,
            eqcheck(scope, aTerm, bTerm)
          )
        case (TypedTypeDef(a, aType), TypedTypeDef(b, bType)) =>
          EqCheckTypeDef(a, aType)
        case _ => ???
      }
      lhs :! res
  }


  def eqcheck(scope: Scope, first: Typed.Term, second: Typed.Term): EqCheck.Term = {
    val (leftTerm :- leftType) = first
    val (rightTerm :- rightType) = second
      val res =
        if (NoFuture.equalTypes(scope, leftType, rightType))
          EqCheck.OK
        else
          EqCheck.Wrong(leftType, rightType)
    val lhs = (leftTerm, rightTerm) match {
      case (TypedLet(x, xTerm, xResTerm), TypedLet(y, yTerm, yResTerm)) =>
        EqCheckLet(x,
          eqcheck(scope, xTerm, yTerm),
          eqcheck(scope + (x -> xTerm.typ), xResTerm, yResTerm))
      case (TypedObj(x, xType, xBody), TypedObj(y, yType, yBody)) =>
        EqCheckObj(x,
          xType,
          eqcheckDef(scope + (x -> xType), xBody, yBody))
      case (TypedFun(x, xType, xBody), TypedFun(y, yType, yBody)) =>
        EqCheckFun(x,
          xType,
          eqcheck(scope + (x -> xType), xBody, yBody))
      case (TypedTApp(leftFunTerm, leftArgDefs), TypedTApp(rightFunTerm, rightArgDefs)) =>
        EqCheckTApp(
          eqcheck(scope, leftFunTerm, rightFunTerm),
          eqcheckDef(scope, leftArgDefs, rightArgDefs))
      case (TypedVar(x), TypedVar(y)) =>
        EqCheckVar(x)
      case (TypedApp(x, y), TypedApp(_, _)) =>
        EqCheckApp(x, y)
      case (TypedSel(x, a), TypedSel(_, _)) =>
        EqCheckSel(x, a)
      case _ => ???
    }
    lhs :! res
  }

}
