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

import DolGenerators._
import ScalaCheckUtils._
import DumbGenerators._
import DolShrinkers._

object DolUtilSpec extends Properties("DolUtilSpec") {

  property("NoFuture.allVarsAndMembersInType = disjoinUnion(allFreeVarsInType, allBoundVarsInType, allFieldMemberSymbolsInType, allTypeMemberSymbolsInType)") = { // TODO Is it meaningful to test these?
    val generator: Gen[(Int, Type)] = genNonsenseType(0, true)
    Prop.forAllNoShrink(generator){prettyProp{case (count, typ) =>
      val expectedAllVars = (0 until count).toSet

      val allVars      = NoFuture.allVarsAndMembersInType(typ)
      val freeVars     = NoFuture.allFreeVarsInType(typ)
      val boundVars    = NoFuture.allBoundVarsInType(typ)
      val fieldMembers = NoFuture.allFieldMemberSymbolsInType(typ)
      val typeMembers  = NoFuture.allTypeMemberSymbolsInType(typ)

      val supposedToBeDisjoin = List(
        freeVars,
        boundVars,
        fieldMembers,
        typeMembers
      )
      val counts = expectedAllVars.map{x =>
        x -> supposedToBeDisjoin.map{_.contains(x)}.map{if (_) 1 else 0}.sum
      }.toMap


      val label = pretty(Map(
        "expectedAllVars" -> expectedAllVars,
        "allVars        " -> allVars,
        "freeVars       " -> freeVars,
        "boundVars      " -> boundVars,
        "fieldMembers   " -> fieldMembers,
        "typeMembers    " -> typeMembers,
        "counts         " -> counts
      ))


      label |: Prop.all(
        expectedAllVars == allVars,
        counts.values.forall{_ == 1})
    }}
  }

  // TODO property("NoFuture.equalTypes -- negative")

  property("NoFuture.equalTypes -- positive") = {
    val generator: Gen[(SymbolUniverse, Scope, Type, Type)] = for {
      ctx <- genGlobalScope()
      (ctx2, typ) <- genType(ctx, Map())
    } yield (new SymbolUniverse(ctx2.nextSymbol), ctx2.globalScope, typ, typ)
    Prop.forAllNoShrink(generator){prettyProp{ case (su, scope, x, upper) =>
      NoFuture.equalTypes(su, scope, x, upper)
    }}
  }

  // TODO allTypeMembers

  // TODO also generate strict subtypes? i.e. T <: U s.t. !(U <: T)

  property("NoFuture.varIsSubtypeOf -- positive") = {
    val generator: Gen[(Scope, Symbol, Type)] = for {
      ctx <- genGlobalScope()
      (ctx2, x) <- ctx.newSymbol()
      (ctx3, lower) <- genType(ctx2, Map())
      (ctx4, upper) <- genSupertype(ctx3, Map(), lower)
    } yield (ctx4.globalScope + (x -> lower), x, upper)
    Prop.forAllNoShrink(generator){prettyProp{ case (scope, x, upper) =>
      NoFuture.varIsSubtypeOf(scope, x, upper)
    }}
  }

  //property("NoFuture.varIsSubtypeOf -- negative") = {
  //  val generator: Gen[(SymbolUniverse, Scope, Symbol, Type)] = for {
  //    su <- const(new SymbolUniverse())
  //    x <- const(su.newSymbol())
  //    scope <- genScope(su)
  //    (newScope, lower) <- genType(su, scope)
  //    upper <- genStrictSupertype(su, newScope, lower, Set())
  //  } yield (su, newScope + (x -> upper), x, lower)
  //  Prop.forAllNoShrink(generator){prettyProp{ case (su, scope, x, upper) =>
  //    !NoFuture.varIsSubtypeOf(scope, x, upper)
  //  }}
  //}


  property("NoFuture.typeRenameVar") = {
    val generator: Gen[(Scope, Symbol, Symbol, Type)] = for {
      ctx <- genGlobalScope()
      (ctx2, typ) <- genType(ctx, Map())
      freeVars <- const(NoFuture.allFreeVarsInType(typ))

      (ctx3, x) <- Gen.frequency[(GlobalContext, Symbol)](
        (1, ctx2.newSymbol()),
        (freeVars.size, Gen.delay{Gen.oneOf(freeVars.toSeq).map{(ctx2, _)}})
      )

      (ctx4, y) <- Gen.frequency(
        (1, ctx2.newSymbol()),
        (1, (ctx3, x)),
        (freeVars.size, Gen.delay{Gen.oneOf(freeVars.toSeq).map{(ctx2, _)}})
      )
    } yield (ctx4.globalScope, x, y, typ)
    Prop.forAllNoShrink(generator){prettyProp{ case (scope, x, y, typ) =>
      val res  = NoFuture.typeRenameVar(x, y, typ)
      val res2 = NoFuture.typeRenameVar(y, x, res)

      val freeBefore = NoFuture.allFreeVarsInType(typ)
      val freeMiddle = NoFuture.allFreeVarsInType(res)
      val freeAfter  = NoFuture.allFreeVarsInType(res2)

      Prop.all(
        !(x != y && freeBefore(x))                   || (!freeMiddle(x) && freeMiddle(y)),
        !(x != y && freeMiddle(y))                   || (!freeAfter(y) && freeAfter(x)),
        !(x != y && freeBefore(x) && !freeBefore(y)) || (typ == res2),
        !(x == y)                                    || (typ == res && res == res2)
      )
    }
  }}


  property("NoFuture.typeProjectUpper -- single") = {
    val generator: Gen[(SymbolUniverse, Scope, Symbol, Symbol, Type)] = for {
      ctx <- genGlobalScope()
      (ctx2, x) <- ctx.newSymbol()
      (ctx3, a) <- ctx2.newSymbol()
      (ctx4, aUpperType) <- genType(ctx3, Map())
      (ctx5, aLowerType) <- genSubtype(ctx4, Map(), aUpperType)
      ctx6 <- ctx5.withBinding(x -> TypeDecl(a, aLowerType, aUpperType))
      su <- ctx6.toSymbolUniverse()
      // TODO test multiple TypeDecls
      // TODO test inbetween typeprojs
      // TODO test rectypes
    } yield (su, ctx6.globalScope, x, a, aUpperType)
    // TODO test multiple declarations (should result in AndType)
    Prop.forAllNoShrink(generator){prettyProp{case (su, scope, x, a, aUpperType) =>
      val res = NoFuture.typeProjectUpper(scope, x, a)
      s"res = $res" |: Prop.all(res != None && NoFuture.equalTypes(su, scope, res.get, aUpperType))
    }}
  }

  property("NoFuture.typeProjectLower -- single") = {
    val generator: Gen[(SymbolUniverse, Scope, Symbol, Symbol, Type)] = for {
      ctx <- genGlobalScope()
      (ctx2, x) <- ctx.newSymbol()
      (ctx3, a) <- ctx2.newSymbol()
      (ctx4, aUpperType) <- genType(ctx3, Map())
      (ctx5, aLowerType) <- genSubtype(ctx4, Map(), aUpperType)
      ctx6 <- ctx5.withBinding(x -> TypeDecl(a, aLowerType, aUpperType))
      su <- ctx6.toSymbolUniverse()
      // TODO test multiple TypeDecls
      // TODO test inbetween typeprojs
      // TODO test rectypes
    } yield (su, ctx6.globalScope, x, a, aLowerType)
    // TODO test multiple declarations (should result in AndType)
    Prop.forAllNoShrink(generator){prettyProp{case (su, scope, x, a, aUpperType) =>
      val res = NoFuture.typeProjectLower(scope, x, a)
      s"res = $res" |: Prop.all(res != None && NoFuture.equalTypes(su, scope, res.get, aUpperType))
    }}
  }


  property("NoFuture.typeProjectUpper -- multi") = {
    val generator: Gen[(SymbolUniverse, Scope, Symbol, Symbol, Type)] = for {
      ctx <- genGlobalScope()
      (ctx2, x) <- ctx.newSymbol()
      (ctx3, a) <- ctx2.newSymbol()
      (ctx4, aUpperType1) <- genType(ctx3, Map())
      (ctx5, aLowerType1) <- genSubtype(ctx4, Map(), aUpperType1)
      (ctx6, aUpperType2) <- genType(ctx5, Map())
      (ctx7, aLowerType2) <- genSubtype(ctx6, Map(), aUpperType2)

      decl1 <- const(TypeDecl(a, aLowerType1, aUpperType1))
      decl2 <- const(TypeDecl(a, aLowerType2, aUpperType2))
      ctx8 <- ctx7.withBinding(x -> AndType(decl1, decl2))
      su <- ctx8.toSymbolUniverse()
      // TODO test multiple TypeDecls
      // TODO test inbetween typeprojs
      // TODO test rectypes
    } yield (su, ctx8.globalScope, x, a, AndType(aUpperType1, aUpperType2))
    Prop.forAllNoShrink(generator){prettyProp{case (su, scope, x, a, aUpperType) =>
      val res = NoFuture.typeProjectUpper(scope, x, a)
      val resLabel = prettyNamed("res", res)
      resLabel |: Prop.all(res != None && NoFuture.equalTypes(su, scope, res.get, aUpperType))
    }}
  }

  property("NoFuture.leastCommonSupertype -- commutative") = {
    val generator: Gen[(SymbolUniverse, Scope, Type, Type)] = for {
      ctx <- genGlobalScope()
      (ctx2, a) <- genType(ctx, Map())
      (ctx3, b) <- genType(ctx2, Map())
      su <- ctx3.toSymbolUniverse()
    } yield (su, ctx3.globalScope, a, b)
    Prop.forAllNoShrink(generator){prettyProp{case (su, scope, a, b) =>
      def lub(left: Type, right: Type) = NoFuture.leastCommonSupertype(scope, left, right)
      val lub_ab = lub(a, b)
      val lub_ba = lub(b, a)
      (prettyNamed("lub(a, b)", lub_ab)
        |: prettyNamed("lub(b, a)", lub_ba)
        |: Prop.all(NoFuture.equalTypes(su, scope, lub_ab, lub_ba)))
    }}
  }

  property("NoFuture.leastCommonSupertype -- associative") = {
    val generator: Gen[(SymbolUniverse, Scope, Type, Type, Type)] = for {
      ctx <- genGlobalScope()
      (ctx2, a) <- genType(ctx, Map())
      (ctx3, b) <- genType(ctx2, Map())
      (ctx4, c) <- genType(ctx3, Map())
      su <- ctx4.toSymbolUniverse()
    } yield (su, ctx4.globalScope, a, b, c)
    Prop.forAllNoShrink(generator){prettyProp{case (su, scope, a, b, c) =>
      def lub(left: Type, right: Type) = NoFuture.leastCommonSupertype(scope, left, right)
      val bc   = lub(b, c)
      val a_bc = lub(a, bc)
      val ab   = lub(a, b)
      val ab_c = lub(ab, c)

      (prettyNamed("lub(b, c)", bc)
        |: prettyNamed("lub(a, lub(b, c))", a_bc)
        |: prettyNamed("lub(a, b)", ab)
        |: prettyNamed("lub(lub(a, b), c)", ab_c)
        |: Prop.all(NoFuture.equalTypes(su, scope, a_bc, ab_c)))
    }}
  }

  property("NoFuture.{leastCommonSupertype, greatestCommonSubtype} -- absorbation") = {
    val generator: Gen[(SymbolUniverse, Scope, Type, Type)] = for {
      ctx <- genGlobalScope()
      (ctx2, a) <- genType(ctx, Map())
      (ctx3, b) <- genType(ctx2, Map())
      su <- ctx3.toSymbolUniverse()
    } yield (su, ctx3.globalScope, a, b)
    Prop.forAllNoShrink(generator){prettyProp{case (su, scope, a, b) =>
      def lub(left: Type, right: Type) = NoFuture.leastCommonSupertype(scope, left, right)
      def glb(left: Type, right: Type) = NoFuture.greatestCommonSubtype(scope, left, right)

      val glb_ab = glb(a, b)
      val lub_ab = lub(a, b)

      val lubRes = lub(a, glb_ab)
      val glbRes = glb(a, lub_ab)

      (prettyNamed("glb(a, b)", glb_ab)
        |: prettyNamed("lub(a, b))", lub_ab)
        |: prettyNamed("lub(a, glb(a, b))", lubRes)
        |: prettyNamed("lub(lub(a, b), c)", glbRes)
        |: Prop.all(NoFuture.equalTypes(su, scope, lubRes, glbRes)))
    }}
  }

  property("NoFuture.leastCommonSupertype -- a <: b ==> lub(a, b) == b") = {
    val generator: Gen[(SymbolUniverse, Scope, Type, Type)] = for {
      ctx <- genGlobalScope()
      (ctx2, b) <- genType(ctx, Map())
      (ctx3, a) <- genSubtype(ctx2, Map(), b)
      su <- ctx3.toSymbolUniverse()
    } yield (su, ctx3.globalScope, a, b)
    Prop.forAllNoShrink(generator){prettyProp{case (su, scope, a, b) =>
      def lub(left: Type, right: Type) = NoFuture.leastCommonSupertype(scope, left, right)
      val lub_ab = lub(a, b)
      (prettyNamed("lub(a, b)", lub_ab)
        |: Prop.all(NoFuture.equalTypes(su, scope, lub_ab, b)))
    }}
  }

  property("NoFuture.greatestCommonSubtype -- a <: b ==> glb(a, b) == a") = {
    val generator: Gen[(SymbolUniverse, Scope, Type, Type)] = for {
      ctx <- genGlobalScope()
      (ctx2, b) <- genType(ctx, Map())
      (ctx3, a) <- genSubtype(ctx2, Map(), b)
      su <- ctx3.toSymbolUniverse()
    } yield (su, ctx3.globalScope, a, b)
    Prop.forAllNoShrink(generator){prettyProp{case (su, scope, a, b) =>
      def glb(left: Type, right: Type) = NoFuture.greatestCommonSubtype(scope, left, right)
      val glb_ab = glb(a, b)
      (prettyNamed("glb(a, b)", glb_ab)
        |: Prop.all(NoFuture.equalTypes(su, scope, glb_ab, a)))
    }}
  }

  property("NoFuture.varGather -- a <: b ==> gather(a, b, Covariant) == TrueConstraint") = {
    val generator: Gen[(GlobalContext, Symbol, Symbol, Type, Type)] = for {
      ctx1 <- genGlobalScope()
      (ctx2, z) <- ctx1.newSymbol()
      (ctx3, r) <- ctx2.newSymbol()
      (ctx4, b) <- genType(ctx3, Map())
      (ctx5, a) <- genSubtype(ctx4, Map(), b)
    } yield (ctx5, r, z, NoFuture.eliminateRecursiveTypes(a, z), NoFuture.eliminateRecursiveTypes(b, z))
    //def shrink(tuple: (GlobalContext, Symbol, Symbol, Type, Type)): Stream[(GlobalContext, Symbol, Symbol, Type, Type)] = {
    //  val (ctx, r, z, a, b) = tuple
    //  shrinkTypePair(ctx, ctx.globalScope, a, b).map{case (ctx2, a2, b2) =>
    //    (ctx2, r, z, a2, b2)
    //  }
    //}
    Prop.forAllNoShrink(generator){prettyProp{case (ctx, r, z, a, b) =>
      val scope = ctx.globalScope
      val res = NoFuture.varGather(scope + (z -> a), r, z, b, Covariant)
      (prettyNamed("res", res)
        |: Prop.protect(res == TrueConstraint))
    }}
  }

  // TODO property("NoFuture.varGather -- !(a <: b) ==> gather(a, b, Covariant) == FalseConstraint")

  // TODO property("NoFuture.varRaise -- raise(AndType(A, B), P) = raise(AndType(B, A), P)") = {

  property("NoFuture.varRaise -- A <: B ==> raise(A, P) <: raise(B, P)") = {
    val generator: Gen[(GlobalContext, Symbol, Symbol, Type, Type, Prototype)] = for {
      ctx1 <- genGlobalScope()
      (ctx2, z) <- ctx1.newSymbol()
      (ctx3, r) <- ctx2.newSymbol()
      (ctx4, b) <- genType(ctx3, Map())
      (ctx5, a) <- genSubtype(ctx4, Map(), b)
      (ctx6, p) <- genPrototypeFromType(ctx5, Map(), b)
    } yield (ctx6, r, z, NoFuture.eliminateRecursiveTypes(a, z), NoFuture.eliminateRecursiveTypes(b, z), p)
    //def shrink(tuple: (GlobalContext, Symbol, Symbol, Type, Type)): Stream[(GlobalContext, Symbol, Symbol, Type, Type)] = {
    //  val (ctx, r, z, a, b) = tuple
    //  shrinkTypePair(ctx, ctx.globalScope, a, b).map{case (ctx2, a2, b2) => (ctx2, r, z, a2, b2)}
    //}
    Prop.forAllNoShrink(generator){prettyProp{case (ctx, r, z, a, b, p) =>
      val scope = ctx.globalScope
      val aRaiseP = NoFuture.varRaise(scope + (z -> a), r, z, p)
      val bRaiseP = NoFuture.varRaise(scope + (z -> b), r, z, p)
      (prettyNamed("bRaiseP", bRaiseP)
        |: prettyNamed("aRaiseP", aRaiseP)
        |: Prop.protect(aRaiseP != None && bRaiseP != None && NoFuture.varIsSubtypeOf(scope + (z -> aRaiseP.get), z, bRaiseP.get)))
    }}
  }

  property("NoFuture.varRaise -- a <: b ==> raise(a, b) == b") = {
    val generator: Gen[(GlobalContext, Symbol, Symbol, Type, Type)] = for {
      ctx1 <- genGlobalScope()
      (ctx2, z) <- ctx1.newSymbol()
      (ctx3, r) <- ctx2.newSymbol()
      (ctx4, b) <- genType(ctx3, Map())
      (ctx5, a) <- genSubtype(ctx4, Map(), b)
    } yield (ctx5, r, z, NoFuture.eliminateRecursiveTypes(a, z), NoFuture.eliminateRecursiveTypes(b, z))
    //def shrink(tuple: (GlobalContext, Symbol, Symbol, Type, Type)): Stream[(GlobalContext, Symbol, Symbol, Type, Type)] = {
    //  val (ctx, r, z, a, b) = tuple
    //  shrinkTypePair(ctx, ctx.globalScope, a, b).map{case (ctx2, a2, b2) => (ctx2, r, z, a2, b2)}
    //}
    Prop.forAllNoShrink(generator){prettyProp{case (ctx, r, z, a, b) =>
      val scope = ctx.globalScope
      val res = NoFuture.varRaise(scope + (z -> a), r, z, b)
      (prettyNamed("res", res)
        |: Prop.protect(res != None && NoFuture.equalTypes(new SymbolUniverse(ctx.nextSymbol), scope, res.get, b)))
    }}
  }

  // TODO solveConstraint

  // TODO typeProjectUpper,typeProjectLower,glb,lub: are not really "utils".
  // Should probably be moved do separate.

  // TODO typeProjectUpper: result does not contains rectypes?
  // TODO typeProjectLower -- multi.  (TODO: depends on lub....)
  // TODO lub: lattice properties
}
