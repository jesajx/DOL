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
      su <- const(new SymbolUniverse())
      scope <- genScope(su)
      (newScope, typ) <- genType(su, scope)
    } yield (su, scope, typ, typ) // TODO rename some bound vars in typ?
    Prop.forAllNoShrink(generator){prettyProp{ case (su, scope, x, upper) =>
      NoFuture.equalTypes(su, scope, x, upper)
    }}
  }

  // TODO allTypeMembers

  // TODO also generate strict subtypes? i.e. T <: U s.t. !(U <: T)

  property("NoFuture.varIsSubtypeOf -- positive") = {
    val generator: Gen[(SymbolUniverse, Scope, Symbol, Type)] = for {
      su <- const(new SymbolUniverse())
      x <- const(su.newSymbol())
      scope <- genScope(su)
      (newScope, lower) <- genType(su, scope)
      upper <- genSupertype(su, newScope, lower)
    } yield (su, newScope + (x -> lower), x, upper)
    Prop.forAllNoShrink(generator){prettyProp{ case (su, scope, x, upper) =>
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
    val generator: Gen[(SymbolUniverse, Scope, Symbol, Symbol, Type)] = for {
      su <- const(new SymbolUniverse())
      (scope, typ) <- genType(su, Map())
      x <- Gen.oneOf((NoFuture.allFreeVarsInType(typ) + su.newSymbol()).toSeq) // NOTE: x != y. Had to check scalacheck src to know that....
      y <- Gen.oneOf(((NoFuture.allFreeVarsInType(typ) + x) + su.newSymbol()).toSeq)
    } yield (su, scope, x, y, typ)
    Prop.forAllNoShrink(generator){prettyProp{ case (su, scope, x, y, typ) =>
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
    val su = new SymbolUniverse()
    val generator: Gen[(Scope, Symbol, Symbol, Type)] = for {
      x <- const(su.newSymbol())
      a <- const(su.newSymbol())
      scope <- genScope(su)
      (newScope, aUpperType) <- genType(su, scope)
      aLowerType <- genSubtype(su, newScope, aUpperType)
      newScope <- const(Map(x -> TypeDecl(a, aLowerType, aUpperType)))
      // TODO test multiple TypeDecls
      // TODO test inbetween typeprojs
      // TODO test rectypes
    } yield (newScope, x, a, aUpperType)
    // TODO test multiple declarations (should result in AndType)
    Prop.forAllNoShrink(generator){prettyProp{case (scope, x, a, aUpperType) =>
      val res = NoFuture.typeProjectUpper(scope, x, a)
      s"res = $res" |: Prop.all(res != None && NoFuture.equalTypes(su, scope, res.get, aUpperType))
    }}
  }

  property("NoFuture.typeProjectLower -- single") = {
    val su = new SymbolUniverse()
    val generator: Gen[(Scope, Symbol, Symbol, Type)] = for {
      x <- const(su.newSymbol())
      a <- const(su.newSymbol())
      scope <- genScope(su)
      (newScope, aUpperType) <- genType(su, scope)
      aLowerType <- genSubtype(su, newScope, aUpperType)
      newScope <- const(Map(x -> TypeDecl(a, aLowerType, aUpperType)))
      // TODO test multiple TypeDecls
      // TODO test inbetween typeprojs
      // TODO test rectypes
    } yield (newScope, x, a, aLowerType)
    // TODO test multiple declarations (should result in AndType)
    Prop.forAllNoShrink(generator){prettyProp{case (scope, x, a, aUpperType) =>
      val res = NoFuture.typeProjectLower(scope, x, a)
      s"res = $res" |: Prop.all(res != None && NoFuture.equalTypes(su, scope, res.get, aUpperType))
    }}
  }


  property("NoFuture.typeProjectUpper -- multi") = {
    val generator: Gen[(SymbolUniverse, Scope, Symbol, Symbol, Type)] = for {
      su <- const(new SymbolUniverse())
      x <- const(su.newSymbol())
      a <- const(su.newSymbol())
      scope <- genScope(su)
      (scope2, aUpperType1) <- genType(su, scope)
      aLowerType1 <- genSubtype(su, scope2, aUpperType1)

      (scope3, aUpperType2) <- genType(su, scope2)
      aLowerType2 <- genSubtype(su, scope3, aUpperType2)

      decl1 <- TypeDecl(a, aLowerType1, aUpperType1)
      decl2 <- TypeDecl(a, aLowerType2, aUpperType2)
      scope4 <- const(scope3 + (x -> NoFuture.andType(decl1, decl2)))

      // TODO test multiple TypeDecls
      // TODO test inbetween typeprojs
      // TODO test rectypes
    } yield (su, scope4, x, a, NoFuture.andType(aUpperType1, aUpperType2)) // TODO having to use andType instead of AndType is a bit unstable...
    // TODO test multiple declarations (should result in AndType)
    Prop.forAllNoShrink(generator){prettyProp{case (su, scope, x, a, aUpperType) =>
      val res = NoFuture.typeProjectUpper(scope, x, a)
      val resLabel = prettyNamed("res", res)
      resLabel |: Prop.all(res != None && NoFuture.equalTypes(su, scope, res.get, aUpperType))
    }}
  }

  property("NoFuture.leastCommonSupertype -- commutative") = {
    val generator: Gen[(SymbolUniverse, Scope, Type, Type)] = for {
      su <- const(new SymbolUniverse())
      scope <- genScope(su)
      (scope2, a) <- genType(su, scope)
      (scope3, b) <- genType(su, scope2)
    } yield (su, scope3, a, b)
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
      su <- const(new SymbolUniverse())
      scope <- genScope(su)
      (scope2, a) <- genType(su, scope)
      (scope3, b) <- genType(su, scope2)
      (scope4, c) <- genType(su, scope3)
    } yield (su, scope4, a, b, c)
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
      su <- const(new SymbolUniverse())
      scope <- genScope(su)
      (scope2, a) <- genType(su, scope)
      (scope3, b) <- genType(su, scope2)
    } yield (su, scope3, a, b)
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
      su <- const(new SymbolUniverse())
      scope <- genScope(su)
      (scope2, a) <- genType(su, scope)
      b <- genSupertype(su, scope2, a)
    } yield (su, scope2, a, b)
    Prop.forAllNoShrink(generator){prettyProp{case (su, scope, a, b) =>
      def lub(left: Type, right: Type) = NoFuture.leastCommonSupertype(scope, left, right)
      val lub_ab = lub(a, b)
      (prettyNamed("lub(a, b)", lub_ab)
        |: Prop.all(NoFuture.equalTypes(su, scope, lub_ab, b)))
    }}
  }

  // TODO typeProjectUpper,typeProjectLower,glb,lub: are not really "utils".
  // Should probably be moved do separate.

  // TODO typeProjectUpper: result does not contains rectypes?
  // TODO typeProjectLower -- multi.  (TODO: depends on lub....)
  // TODO lub: lattice properties
}
