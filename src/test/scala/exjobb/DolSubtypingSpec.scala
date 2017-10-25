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
import scala.concurrent.duration._

import scala.concurrent.ExecutionContext
import DolGenerators._
import ScalaCheckUtils._
import DumbGenerators._
import DolShrinkers._

object DolSubtypingSpec extends Properties("DolSubtypingSpec") {

  // TODO property("NoFuture.equalTypesOLD -- negative")

  property("NoFuture.varEqualTypes -- positive") = {
    val generator: Gen[(GlobalContext, Symbol, Type, Type)] = for {
      ctx1 <- genGlobalScope()
      (ctx2, z) <- ctx1.newSymbol()
      (ctx3, typ) <- genType(ctx2, Map())
    } yield (ctx3, z, typ, typ)
    Prop.forAllNoShrink(generator){prettyProp{case (ctx, z, left, right) =>
      NoFuture.varEqualTypes(ctx.globalScope + (z -> left), z, right)
    }}
  }

  // TODO varEqualType: a == obfuscate(a)

  // TODO allTypeMembers

  // TODO also generate strict subtypes? i.e. T <: U s.t. !(U <: T)

  property("NoFuture.varIsSubtypeOf -- a <: b ==> varIsSubtypeOf(a, b)") = {
    val generator: Gen[(GlobalContext, Symbol, Type, Type)] = for {
      ctx <- genGlobalScope()
      (ctx2, x) <- ctx.newSymbol()
      (ctx3, b) <- genType(ctx2, Map())
      (ctx4, a) <- genSubtype(ctx3, Map(), b)
    } yield (ctx4, x, a, b)
    Prop.forAllNoShrink(generator){prettyProp{ case (ctx, x, a, b) =>
      NoFuture.varIsSubtypeOf(ctx.globalScope + (x -> a), x, b)
    }}
  }

  property("NoFuture.varIsSubtypeOf -- transitivity -- a <: b <: c ==> varIsSubtypeOf(a, c)") = {
    val generator: Gen[(GlobalContext, Symbol, Type, Type, Type)] = for {
      ctx <- genGlobalScope()
      (ctx2, x) <- ctx.newSymbol()
      (ctx3, c) <- genType(ctx2, Map())
      (ctx4, b) <- genSubtype(ctx3, Map(), c)
      (ctx5, a) <- genSubtype(ctx4, Map(), b)
    } yield (ctx5, x, a, b, c)
    Prop.forAllNoShrink(generator){prettyProp{ case (ctx, x, a, b, c) =>
      NoFuture.varIsSubtypeOf(ctx.globalScope + (x -> a), x, c)
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


  property("NoFuture.typeProjectUpper -- single") = {
    val generator: Gen[(GlobalContext, Symbol, Symbol, Symbol, Type)] = for {
      ctx <- genGlobalScope()
      (ctx2, z) <- ctx.newSymbol()
      (ctx3, x) <- ctx2.newSymbol()
      (ctx4, a) <- ctx3.newSymbol()
      (ctx5, aUpperType) <- genType(ctx4, Map())
      (ctx6, aLowerType) <- genSubtype(ctx5, Map(), aUpperType)
      ctx7 <- ctx6.withBinding(x -> TypeDecl(a, aLowerType, aUpperType))
      // TODO test multiple TypeDecls
      // TODO test inbetween typeprojs
      // TODO test rectypes
    } yield (ctx7, z, x, a, aUpperType)
    // TODO test multiple declarations (should result in AndType)
    Prop.forAllNoShrink(generator){prettyProp{case (ctx, z, x, a, aUpperType) =>
      val scope = ctx.globalScope
      val res = NoFuture.typeProjectUpper(scope, x, a)
      s"res = $res" |: Prop.all(res != None && NoFuture.varEqualTypes(scope + (z -> res.get), z, aUpperType))
    }}
  }

  property("NoFuture.typeProjectLower -- single") = {
    val generator: Gen[(GlobalContext, Symbol, Symbol, Symbol, Type)] = for {
      ctx <- genGlobalScope()
      (ctx2, z) <- ctx.newSymbol()
      (ctx3, x) <- ctx2.newSymbol()
      (ctx4, a) <- ctx3.newSymbol()
      (ctx5, aUpperType) <- genType(ctx4, Map())
      (ctx6, aLowerType) <- genSubtype(ctx5, Map(), aUpperType)
      ctx7 <- ctx6.withBinding(x -> TypeDecl(a, aLowerType, aUpperType))
      // TODO gen subtype if the TypeDecl (i.e. obfuscate with unneccessary
      // type info)
      // TODO test multiple TypeDecls
      // TODO test inbetween typeprojs
      // TODO test rectypes
    } yield (ctx7, z, x, a, aLowerType)
    // TODO test multiple declarations (should result in AndType)
    Prop.forAllNoShrink(generator){prettyProp{case (ctx, z, x, a, aLowerType) =>
      val scope = ctx.globalScope
      val res = NoFuture.typeProjectLower(scope, x, a)
      s"res = $res" |: Prop.all(res != None && NoFuture.varEqualTypes(scope + (z -> res.get), z, aLowerType))
    }}
  }


  property("NoFuture.typeProjectUpper -- multi") = {
    val generator: Gen[(GlobalContext, Symbol, Symbol, Symbol, Type)] = for {
      ctx <- genGlobalScope()
      (ctx2, z) <- ctx.newSymbol()
      (ctx3, x) <- ctx2.newSymbol()
      (ctx4, a) <- ctx3.newSymbol()
      (ctx5, aUpperType1) <- genType(ctx4, Map())
      (ctx6, aLowerType1) <- genSubtype(ctx5, Map(), aUpperType1)
      (ctx7, aUpperType2) <- genType(ctx6, Map())
      (ctx8, aLowerType2) <- genSubtype(ctx7, Map(), aUpperType2)

      decl1 <- const(TypeDecl(a, aLowerType1, aUpperType1))
      decl2 <- const(TypeDecl(a, aLowerType2, aUpperType2))
      ctx9 <- ctx8.withBinding(x -> AndType(decl1, decl2))
      // TODO test multiple TypeDecls
      // TODO test inbetween typeprojs
      // TODO test rectypes
    } yield (ctx9, z, x, a, AndType(aUpperType1, aUpperType2))
    Prop.forAllNoShrink(generator){prettyProp{case (ctx, z, x, a, aUpperType) =>
      val scope = ctx.globalScope
      val res = NoFuture.typeProjectUpper(scope, x, a)
      val resLabel = prettyNamed("res", res)
      resLabel |: Prop.all(res != None && NoFuture.varEqualTypes(scope + (z -> res.get), z, aUpperType))
    }}
  }

  property("NoFuture.leastCommonSupertype -- commutative") = {
    val generator: Gen[(GlobalContext, Symbol, Type, Type)] = for {
      ctx <- genGlobalScope()
      (ctx2, z) <- ctx.newSymbol()
      (ctx3, a) <- genType(ctx2, Map())
      (ctx4, b) <- genType(ctx3, Map())
    } yield (ctx4, z, a, b)
    Prop.forAllNoShrink(generator){prettyProp{case (ctx, z, a, b) =>
      val scope = ctx.globalScope
      def lub(left: Type, right: Type) = NoFuture.leastCommonSupertype(scope, left, right)
      val lub_ab = lub(a, b)
      val lub_ba = lub(b, a)
      (prettyNamed("lub(a, b)", lub_ab)
        |: prettyNamed("lub(b, a)", lub_ba)
        |: Prop.all(NoFuture.varEqualTypes(scope + (z -> lub_ab), z, lub_ba)))
    }}
  }

  property("NoFuture.leastCommonSupertype -- associative") = {
    val generator: Gen[(GlobalContext, Symbol, Type, Type, Type)] = for {
      ctx <- genGlobalScope()
      (ctx2, z) <- ctx.newSymbol()
      (ctx3, a) <- genType(ctx2, Map())
      (ctx4, b) <- genType(ctx3, Map())
      (ctx5, c) <- genType(ctx4, Map())
    } yield (ctx5, z, a, b, c)
    Prop.forAllNoShrink(generator){prettyProp{case (ctx, z, a, b, c) =>
      val scope = ctx.globalScope
      def lub(left: Type, right: Type) = NoFuture.leastCommonSupertype(scope, left, right)
      val bc   = lub(b, c)
      val a_bc = lub(a, bc)
      val ab   = lub(a, b)
      val ab_c = lub(ab, c)

      (prettyNamed("lub(b, c)", bc)
        |: prettyNamed("lub(a, lub(b, c))", a_bc)
        |: prettyNamed("lub(a, b)", ab)
        |: prettyNamed("lub(lub(a, b), c)", ab_c)
        |: Prop.all(NoFuture.varEqualTypes(scope + (z -> a_bc), z, ab_c)))
    }}
  }

  property("NoFuture.{leastCommonSupertype, greatestCommonSubtype} -- absorbation") = {
    val generator: Gen[(GlobalContext, Symbol, Type, Type)] = for {
      ctx <- genGlobalScope()
      (ctx2, z) <- ctx.newSymbol()
      (ctx3, a) <- genType(ctx2, Map())
      (ctx4, b) <- genType(ctx3, Map())
    } yield (ctx4, z, a, b)
    Prop.forAllNoShrink(generator){prettyProp{case (ctx, z, a, b) =>
      val scope = ctx.globalScope
      def lub(left: Type, right: Type) = NoFuture.leastCommonSupertype(scope, left, right)
      def glb(left: Type, right: Type) = NoFuture.greatestCommonSubtype(scope, left, right)

      val glb_ab = glb(a, b)
      val lub_ab = lub(a, b)

      val lubRes = lub(a, glb_ab)
      val glbRes = glb(a, lub_ab)

      (prettyNamed("glb(a, b)", glb_ab)
        |: prettyNamed("lub(a, b))", lub_ab)
        |: prettyNamed("lub(a, glb(a, b))", lubRes)
        |: prettyNamed("glb(lub(a, b), a)", glbRes)
        |: Prop.all(
          NoFuture.varEqualTypes(scope + (z -> a), z, glbRes),
          NoFuture.varEqualTypes(scope + (z -> lubRes), z, a)))
    }}
  }

  property("NoFuture.leastCommonSupertype -- a <: b ==> lub(a, b) = b") = {
    val generator: Gen[(GlobalContext, Symbol, Type, Type)] = for {
      ctx <- genGlobalScope()
      (ctx2, z) <- ctx.newSymbol()
      (ctx3, b) <- genType(ctx2, Map())
      (ctx4, a) <- genSubtype(ctx3, Map(), b)
    } yield (ctx4, z, a, b)
    Prop.forAllNoShrink(generator){prettyProp{case (ctx, z, a, b) =>
      val scope = ctx.globalScope
      def lub(left: Type, right: Type) = NoFuture.leastCommonSupertype(scope, left, right)
      val lub_ab = lub(a, b)
      (prettyNamed("lub(a, b)", lub_ab)
        |: Prop.all(NoFuture.varEqualTypes(scope + (z -> lub_ab), z, b)))
    }}
  }

  property("NoFuture.greatestCommonSubtype -- a <: b ==> glb(a, b) = a") = {
    val generator: Gen[(GlobalContext, Symbol, Type, Type)] = for {
      ctx <- genGlobalScope()
      (ctx2, z) <- ctx.newSymbol()
      (ctx3, b) <- genType(ctx2, Map())
      (ctx4, a) <- genSubtype(ctx3, Map(), b)
    } yield (ctx4, z, a, b)
    Prop.forAllNoShrink(generator){prettyProp{case (ctx, z, a, b) =>
      val scope = ctx.globalScope
      def glb(left: Type, right: Type) = NoFuture.greatestCommonSubtype(scope, left, right)
      val glb_ab = glb(a, b)
      (prettyNamed("glb(a, b)", glb_ab)
        |: Prop.all(NoFuture.varEqualTypes(scope + (z -> glb_ab), z, a)))
    }}
  }

  property("NoFuture.leastCommonSupertype -- a <: c, b <: c ==> lub(a, b) <: c") = {
    val generator: Gen[(GlobalContext, Symbol, Type, Type, Type)] = for {
      ctx <- genGlobalScope()
      (ctx2, z) <- ctx.newSymbol()
      (ctx3, c) <- genType(ctx2, Map())
      (ctx4, a) <- genSubtype(ctx3, Map(), c)
      (ctx5, b) <- genSubtype(ctx4, Map(), c)
    } yield (ctx5, z, a, b, c)
    Prop.forAllNoShrink(generator){prettyProp{case (ctx, z, a, b, c) =>
      val scope = ctx.globalScope
      def lub(left: Type, right: Type) = NoFuture.leastCommonSupertype(scope, left, right)
      val lub_ab = lub(a, b)
      (prettyNamed("lub(a, b)", lub_ab)
        |: Prop.all(NoFuture.varIsSubtypeOf(scope + (z -> lub_ab), z, c)))
    }}
  }

  // TODO alts(dnfStream(gather(a, p))).contains(b) ==> b <: a

  // TODO property("NoFuture.varGather -- !(a <: b) ==> gather(a, b, Covariant) == FalseConstraint")

  // TODO property("NoFuture.varRaise -- raise(AndType(A, B), P) = raise(AndType(B, A), P)") = {

  // TODO varRaise -- p = genPrototypeFromType(B), p2 = genPrototypeFromType(P), raise(B, P2) <: raise(B; P)

  property("NoFuture.varRaise -- a <: b; p=gen(b) ==> raise(a, p) <: raise(b, p)") = {
    val generator: Gen[(GlobalContext, Scope, Symbol, Symbol, Type, Type, Prototype)] = for {
      ctx1 <- genGlobalScope()
      (ctx2, z) <- ctx1.newSymbol()
      (ctx3, r) <- ctx2.newSymbol()
      (ctx4, localScope) <- genScope(ctx3)
      (ctx5, b) <- genType(ctx4, localScope)
      (ctx6, a) <- genSubtype(ctx5, localScope, b)
      p <- genPrototypeFromType(b)
    } yield (ctx6, localScope, r, z, a, b, p)
    Prop.forAllNoShrink(generator){prettyProp{case (ctx, localScope, r, z, a, b, p) =>
      val scope = ctx.globalScope ++ localScope
      val aRaiseP = timeout(30.seconds){NoFuture.varRaise(scope + (z -> a), r, z, p)}
      val bRaiseP = timeout(30.seconds){NoFuture.varRaise(scope + (z -> b), r, z, p)}
      (prettyNamed("bRaiseP", bRaiseP)
        |: prettyNamed("aRaiseP", aRaiseP)
        |: Prop.protect(aRaiseP != None && bRaiseP != None && NoFuture.varIsSubtypeOf(scope + (z -> aRaiseP.get), z, bRaiseP.get)))
    }}
  }


  property("NoFuture.varLower -- a <: b ; p=gen(a) ==> lower(a, p) <: lower(b, p)") = {
    val generator: Gen[(GlobalContext, Scope, Symbol, Symbol, Type, Type, Prototype)] = for {
      ctx1 <- genGlobalScope()
      (ctx2, z) <- ctx1.newSymbol()
      (ctx3, k) <- ctx2.newSymbol()
      (ctx4, r) <- ctx3.newSymbol()
      (ctx5, localScope) <- genScope(ctx4)
      (ctx6, b) <- genType(ctx5, localScope)
      (ctx7, a) <- genSubtype(ctx6, localScope, b)
      ctx8      <- ctx7.withBinding(k -> a)
      p <- genPrototypeFromType(a)
    } yield (ctx8, localScope, r, z, a, b, p)
    Prop.forAllNoShrink(generator){prettyProp{case (ctx, localScope, r, z, a, b, p) =>
      val scope = ctx.globalScope ++ localScope
      val aLowerP = timeout(30.seconds){NoFuture.varLower(scope + (z -> a), r, z, p)}
      val bLowerP = timeout(30.seconds){NoFuture.varLower(scope + (z -> b), r, z, p)}
      (prettyNamed("bLowerP", bLowerP)
        |: prettyNamed("aLowerP", aLowerP)
        |: Prop.protect(aLowerP != None && bLowerP != None && NoFuture.varIsSubtypeOf(scope + (z -> aLowerP.get), z, bLowerP.get)))
    }}
  }


  property("NoFuture.varRaise -- raise(a, p) == b ==> a <: b") = {
    val generator: Gen[(GlobalContext, Scope, Symbol, Symbol, Type, Prototype)] = for {
      ctx1 <- genGlobalScope()
      (ctx2, z) <- ctx1.newSymbol()
      (ctx3, r) <- ctx2.newSymbol()
      (ctx4, localScope) <- genScope(ctx3)
      (ctx5, a) <- genType(ctx4, localScope)
      p <- genPrototypeFromType(a)
    } yield (ctx5, localScope, r, z, a, p)
    Prop.forAllNoShrink(generator){prettyProp{case (ctx, localScope, r, z, a, p) =>
      val scope = ctx.globalScope ++ localScope
      val b = timeout(30.seconds){NoFuture.varRaise(scope + (z -> a), r, z, p)}
      (prettyNamed("b", b)
        |: Prop.protect(b != None && NoFuture.varIsSubtypeOf(scope + (z -> a), z, b.get)))
    }}
  }

  property("NoFuture.varLower -- lower(b, p) == a ==> a <: b") = {
    val generator = for {
      ctx1 <- genGlobalScope()
      (ctx2, z) <- ctx1.newSymbol()
      (ctx3, r) <- ctx2.newSymbol()
      (ctx4, localScope) <- genScope(ctx3)
      (ctx5, b) <- genType(ctx4, localScope)
      p <- genPrototypeFromType(b)
    } yield (ctx5, localScope, r, z, b, p)
    Prop.forAllNoShrink(generator){prettyProp{case (ctx, localScope, r, z, b, p) =>
      val scope = ctx.globalScope ++ localScope
      val a = timeout(30.seconds){NoFuture.varLower(scope + (z -> b), r, z, p)}
      (prettyNamed("a", a)
        |: Prop.protect(a != None && NoFuture.varIsSubtypeOf(scope + (z -> a.get), z, b)))
    }}
  }


  // property("NoFuture.varRaise -- raise(T, P) = R ==> T <: R")
  // property("NoFuture.varRaise -- raise(T, P) = R ==> raise(R, P) == R")
  // Since: T <: R, R matches P

  property("NoFuture.varRaise -- a <: b ==> raise(a, b) = b") = {
    val generator = for {
      ctx1 <- genGlobalScope()
      (ctx2, z) <- ctx1.newSymbol()
      (ctx3, r) <- ctx2.newSymbol()
      (ctx4, localScope) <- genScope(ctx3)
      (ctx5, b) <- genType(ctx4, localScope)
      (ctx6, a) <- genSubtype(ctx5, localScope, b)
    } yield (ctx6, localScope, r, z, a, b)
    Prop.forAllNoShrink(generator){prettyProp{case (ctx, localScope, r, z, a, b) =>
      val scope = ctx.globalScope ++ localScope
      val res = timeout(30.seconds){NoFuture.varRaise(scope + (z -> a), r, z, b)}
      (prettyNamed("res", res)
        |: Prop.protect(res != None && NoFuture.varEqualTypes(scope + (z -> res.get), z, b)))
    }}
  }

  property("NoFuture.varLower -- a <: b ==> lower(b, a) = a") = {
    val generator = for {
      ctx1 <- genGlobalScope()
      (ctx2, z) <- ctx1.newSymbol()
      (ctx3, r) <- ctx2.newSymbol()
      (ctx4, localScope) <- genScope(ctx3)
      (ctx5, b) <- genType(ctx4, localScope)
      (ctx6, a) <- genSubtype(ctx5, localScope, b)
    } yield (ctx6, localScope, r, z, b, a)
    Prop.forAllNoShrink(generator){prettyProp{case (ctx, localScope,r, z, b, a) =>
      val scope = ctx.globalScope ++ localScope
      val res = timeout(30.seconds){NoFuture.varLower(scope + (z -> b), r, z, a)}
      (prettyNamed("res", res)
        |: Prop.protect(res != None && NoFuture.varEqualTypes(scope + (z -> res.get), z, a)))
    }}
  }

  property("NoFuture.varRaise -- p2 = genPrototype(p),  raise(a, p2) <: raise(a, p)") = {
    val generator = for {
      ctx1 <- genGlobalScope()
      (ctx2, z) <- ctx1.newSymbol()
      (ctx3, r) <- ctx2.newSymbol()
      (ctx4, localScope) <- genScope(ctx3)
      (ctx5, a) <- genType(ctx4, localScope)
      p <- genPrototypeFromType(a)
      p2 <- genPrototypeFromType(p)
    } yield (ctx5, localScope, r, z, a, p, p2)
    Prop.forAllNoShrink(generator){prettyProp{case (ctx, localScope, r, z, a, p, p2) =>
      val scope = ctx.globalScope ++ localScope
      val aRaiseP = timeout(30.seconds){NoFuture.varRaise(scope + (z -> a), r, z, p)}
      val aRaiseP2 = timeout(30.seconds){NoFuture.varRaise(scope + (z -> a), r, z, p2)}
      (prettyNamed("aRaiseP2", aRaiseP2)
        |: prettyNamed("aRaiseP", aRaiseP)
        |: Prop.protect(aRaiseP != None && aRaiseP2 != None && NoFuture.varIsSubtypeOf(scope + (z -> aRaiseP2.get), z, aRaiseP.get)))
    }}
  }

  property("NoFuture.varLower -- p2 = genPrototype(p),  lower(a, p) <: lower(a, p2)") = {
    val generator = for {
      ctx1 <- genGlobalScope()
      (ctx2, z) <- ctx1.newSymbol()
      (ctx3, r) <- ctx2.newSymbol()
      (ctx4, localScope) <- genScope(ctx3)
      (ctx5, a) <- genType(ctx4, localScope)
      p <- genPrototypeFromType(a)
      p2 <- genPrototypeFromType(p)
    } yield (ctx5, localScope, r, z, a, p, p2)
    Prop.forAllNoShrink(generator){prettyProp{case (ctx, localScope, r, z, a, p, p2) =>
      val scope = ctx.globalScope ++ localScope
      val aLowerP = timeout(30.seconds){NoFuture.varLower(scope + (z -> a), r, z, p)}
      val aLowerP2 = timeout(30.seconds){NoFuture.varLower(scope + (z -> a), r, z, p2)}
      (prettyNamed("aLowerP2", aLowerP2)
        |: prettyNamed("aLowerP", aLowerP)
        |: Prop.protect(aLowerP != None && aLowerP2 != None && NoFuture.varIsSubtypeOf(scope + (z -> aLowerP.get), z, aLowerP2.get)))
    }}
  }

  property("NoFuture.varRaise -- p=gen(a) ; raise(a, p) = a") = {
    val generator = for {
      ctx1 <- genGlobalScope()
      (ctx2, z) <- ctx1.newSymbol()
      (ctx3, r) <- ctx2.newSymbol()
      (ctx4, localScope) <- genScope(ctx3)
      (ctx5, a) <- genType(ctx4, localScope)
      p <- genPrototypeFromType(a)
    } yield (ctx5, localScope, r, z, a, p)
    Prop.forAllNoShrink(generator){prettyProp{case (ctx, localScope, r, z, a, p) =>
      val scope = ctx.globalScope ++ localScope
      val aRaiseP = timeout(30.seconds){NoFuture.varRaise(scope + (z -> a), r, z, p)}
      (prettyNamed("aRaiseP", aRaiseP)
        |: Prop.protect(aRaiseP != None && NoFuture.varEqualTypes(scope + (z -> aRaiseP.get), z, a)))
    }}
  }

  property("NoFuture.varLower -- p=gen(a) ; lower(a, p) = a") = {
    val generator = for {
      ctx1 <- genGlobalScope()
      (ctx2, z) <- ctx1.newSymbol()
      (ctx3, r) <- ctx2.newSymbol()
      (ctx4, localScope) <- genScope(ctx3)
      (ctx5, a) <- genType(ctx4, Map())
      p <- genPrototypeFromType(a)
    } yield (ctx5, localScope, r, z, a, p)
    Prop.forAllNoShrink(generator){prettyProp{case (ctx, localScope, r, z, a, p) =>
      val scope = ctx.globalScope ++ localScope
      val aLowerP = timeout(30.seconds){NoFuture.varLower(scope + (z -> a), r, z, p)}
      (prettyNamed("aLowerP", aLowerP)
        |: Prop.protect(aLowerP != None && NoFuture.varEqualTypes(scope + (z -> aLowerP.get), z, a)))
    }}
  }


  // TODO raise(a,a)==A?

  property("NoFuture.rigidEqualTypes -- rigidEqualTypes(a, a)") = { // TODO unnecessary?
    val generator: Gen[(GlobalContext, Type)] = for {
      ctx1 <- genGlobalScope()
      (ctx2, a) <- genType(ctx1, Map())
    } yield (ctx2, a)
    Prop.forAllNoShrink(generator){prettyProp{case (ctx, a) =>
      Prop.protect(NoFuture.rigidEqualTypes(a, a))
    }}
  }

  property("NoFuture.prepMatch -- !isPrototype(a) ==> prepMatch(_, a, 0) == (0, a)") = {
    val generator: Gen[(GlobalContext, Type)] = for {
      ctx1 <- genGlobalScope()
      (ctx2, a) <- genType(ctx1, Map())
    } yield (ctx2, a)
    Prop.forAllNoShrink(generator){prettyProp{case (ctx, a) =>
      Prop.protect(NoFuture.prepMatch(-1, a, 0) == (0, a))
    }}
  }

  property("NoFuture.simplify -- simplify(a) = a") = {
    val generator: Gen[(GlobalContext, Symbol, Type)] = for {
      ctx1 <- genGlobalScope()
      (ctx2, z) <- ctx1.newSymbol()
      (ctx3, a) <- genType(ctx2, Map())
    } yield (ctx3, z, a)
    Prop.forAllNoShrink(generator){prettyProp{case (ctx, z, a) =>
      val res = NoFuture.simplify(a)
      (prettyNamed("simplify(a)", res)
        |: Prop.protect(NoFuture.varEqualTypes(ctx.globalScope + (z -> a), z, res)))
    }}
  }



  property("NoFuture.eliminateVars -- free(elimUp(a, S)).intersect(S) = {}") = {
    val generator: Gen[(GlobalContext, Scope, Symbol, Type)] = for {
      ctx1 <- genGlobalScope()
      (ctx2, z) <- ctx1.newSymbol()
      (ctx3, k) <- ctx2.newSymbol()
      (ctx4, localScope) <- genScope(ctx3)
      (ctx5, a) <- genType(ctx4, Map())
      killSeq <- const(ctx5.globalScope.keys.toSet + k)
    } yield (ctx5, localScope, z, a)
    Prop.forAllNoShrink(generator){prettyProp{case (ctx, localScope, z, a) =>
      val scope = ctx.globalScope
      val killSet = localScope.keySet
      val res = NoFuture.eliminateVars(scope, killSet, Some(z), a)
      val free = NoFuture.allFreeVarsInType(res)
      (prettyNamed("elimUp(a, S)", res)
        |: prettyNamed("free(elimUp(a))", free)
        |: Prop.protect(free.intersect(killSet).isEmpty))
    }}
  }

  property("NoFuture.eliminateVars -- a <: elimUp(a, S)") = {
    val generator: Gen[(GlobalContext, Scope, Symbol, Type)] = for {
      ctx1 <- genGlobalScope()
      (ctx2, z) <- ctx1.newSymbol()
      (ctx3, k) <- ctx2.newSymbol()
      (ctx4, localScope) <- genScope(ctx3)
      (ctx5, a) <- genType(ctx4, localScope)
      // TODO let zOption=None sometimes?
    } yield (ctx5, localScope, z, a)
    Prop.forAllNoShrink(generator){prettyProp{case (ctx, localScope, z, a) =>
      val scope = ctx.globalScope ++ localScope
      val killSet = localScope.keySet
      val res = NoFuture.eliminateVars(scope, killSet, Some(z), a)
      (prettyNamed("elimUp(a, S)", res)
        |: Prop.protect(NoFuture.varIsSubtypeOf(scope + (z -> a), z, res)))
    }}
  }

  property("NoFuture.eliminateVars -- S not free in a ==> elimUp(a, S) == a") = {
    val generator: Gen[(GlobalContext, Scope, Set[Symbol], Symbol, Type)] = for {
      ctx1 <- genGlobalScope()
      (ctx2, z) <- ctx1.newSymbol()
      (ctx3, k) <- ctx2.newSymbol()
      (ctx4, localScope) <- genScope(ctx3)
      (ctx5, a) <- genType(ctx4, localScope)
      killSeq <- someOf((localScope.keys.toSet -- NoFuture.allFreeVarsInType(a)) + k)
    } yield (ctx5, localScope, killSeq.toSet, z, a)
    Prop.forAllNoShrink(generator){prettyProp{case (ctx, localScope, killSet,z, a) =>
      val scope = ctx.globalScope ++ localScope
      val res = NoFuture.eliminateVars(scope, killSet, Some(z), a)
      (prettyNamed("elimUp(a)", res)
        |: Prop.protect(res == a)) // NOTE: Exact, not varEqualTypes.
    }}
  }

  property("NoFuture.eliminateVars -- a <: b ==> elimUp(a, S) <: elimUp(b, S)") = {
    val generator: Gen[(GlobalContext, Scope, Symbol, Type, Type)] = for {
      ctx1 <- genGlobalScope()
      (ctx2, z) <- ctx1.newSymbol()
      (ctx3, localScope) <- genScope(ctx2)
      (ctx4, b) <- genType(ctx3, localScope)
      (ctx5, a) <- genSubtype(ctx4, localScope, b)
    } yield (ctx5, localScope, z, a, b)
    Prop.forAllNoShrink(generator){prettyProp{case (ctx, localScope, z, a, b) =>
      val scope = ctx.globalScope ++ localScope
      val killSet = localScope.keySet
      val elimA = NoFuture.eliminateVars(scope, killSet, Some(z), a)
      val elimB = NoFuture.eliminateVars(scope, killSet, Some(z), b)
      (prettyNamed("elimUp(b)", elimB)
        |: prettyNamed("elimUp(a)", elimA)
        |: Prop.protect(NoFuture.varIsSubtypeOf(ctx.globalScope + (z -> elimA), z, elimB)))
    }}
  }

  property("NoFuture.eliminateVars -- validInScope: elimUp(a, S)") = {
    val generator: Gen[(GlobalContext, Scope, Symbol, Type)] = for {
      ctx1 <- genGlobalScope()
      (ctx2, z) <- ctx1.newSymbol()
      (ctx3, localScope) <- genScope(ctx2)
      (ctx4, a) <- genType(ctx3, Map())
    } yield (ctx4, localScope, z, a)
    Prop.forAllNoShrink(generator){prettyProp{case (ctx, localScope, z, a) =>
      val scope = ctx.globalScope ++ localScope
      val killSet = localScope.keySet
      val elimA = NoFuture.eliminateVars(scope, killSet, Some(z), a)
      (prettyNamed("elimUp(a)", elimA)
        |: Prop.protect(NoFuture.validTypeInScope(ctx.globalScope, elimA)))
    }}
  }

  property("NoFuture.eliminateVars -- validInScope: elimDown(a, S)") = {
    // TODO Why does this not find `elimUp(`FunType(3, RecType(5, TypeDecl(4, Bot, TypeProj(1, 4))), TypeProj(3, 4)), Set(1))`?
    val generator: Gen[(GlobalContext, Scope, Symbol, Type)] = for {
      ctx1 <- genGlobalScope()
      (ctx2, z) <- ctx1.newSymbol()
      (ctx3, localScope) <- genScope(ctx2)
      (ctx4, a) <- genType(ctx3, Map())
    } yield (ctx4, localScope, z, a)
    Prop.forAllNoShrink(generator){prettyProp{case (ctx, localScope, z, a) =>
      val scope = ctx.globalScope ++ localScope
      val killSet = localScope.keySet
      val elimA = NoFuture.eliminateVars(scope, killSet, Some(z), a, variance=Contravariant)
      (prettyNamed("elimDown(a)", elimA)
        |: Prop.protect(NoFuture.validTypeInScope(ctx.globalScope, elimA)))
    }}
  }




  // TODO check for validTypeInScope for lub and raise too?

  // TODO how to efficiently check minimality of eliminateVars?

  // TODO test eliminateVarsDown separately from eliminateVarsUp?

  // TODO raise(AndType(A,B), AndType(Que, Que)) == simplify(AndType(A, B)) -- i.e. exact?

  // TODO a <: c && b <: c  ==>  lub(a,b) <: c  ???

  // TODO solveConstraint when the vars to be solves appear in multiple
  // places.

  // TODO split this spec into multiple specs?

  // TODO typeProjectUpper: result does not contains rectypes?
  // TODO typeProjectLower -- multi.  (TODO: depends on lub....)
  // TODO lub: lattice properties

  // TODO test eliminateVars by starting with a type with no vars, and then
  // replacing parts with vars?  scope={},Int  --> scope={x->{T=Int}},x.T
}
