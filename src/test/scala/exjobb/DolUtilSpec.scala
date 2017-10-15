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
import DolShrinkers._

object DolUtilSpec extends Properties("DolUtilSpec") {

  property("NoFuture.allVarsAndMembersInType = disjoinUnion(allFreeVarsInType, allBoundVarsInType, allFieldMemberSymbolsInType, allTypeMemberSymbolsInType)") = {
    val generator: Gen[(SymbolUniverse, (Scope, Type))] = withSymbolUniverse{genType(_, Map())}
    Prop.forAllNoShrink(generator){prettyProp{case (su, (scope, typ)) =>
      val expectedAllVars = (0 until su.count()).toSet

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
      val counts = allVars.map{x =>
        x -> supposedToBeDisjoin.map{_.contains(x)}.map{if (_) 1 else 0}.sum
      }.toMap


      val label = pretty(Map(
        "typ            " -> typ,
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

  // TODO also generate strict subtypes? i.e. T <: U s.t. !(U <: T)

  property("NoFuture.varIsSubtypeOf") = {
    val generator: Gen[(Scope, Symbol, Type)] = for {
      su <- const(new SymbolUniverse())
      x <- const(su.newSymbol())
      scope <- genScope(su)
      (newScope, lower) <- genType(su, scope)
      upper <- genSupertype(su, newScope, lower, Set())
    } yield (newScope + (x -> lower), x, upper)
    Prop.forAllNoShrink(generator){prettyProp{ case (newScope, x, upper) =>
      NoFuture.varIsSubtypeOf(newScope, x, upper)
    }}
  }

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
      aLowerType <- genSubtype(su, newScope, aUpperType, Set())
      newScope <- const(Map(x -> TypeDecl(a, aLowerType, aUpperType)))
      // TODO test multiple TypeDecls
      // TODO test inbetween typeprojs
      // TODO test rectypes
    } yield (newScope, x, a, aUpperType)
    // TODO test multiple declarations (should result in AndType)
    Prop.forAllNoShrink(generator){prettyProp{case (scope, x, a, aUpperType) =>
      val res = NoFuture.typeProjectUpper(scope, x, a)
      s"res = $res" |: Prop.all(res != None && NoFuture.equalTypes(scope, res.get, aUpperType))
    }}
  }

  property("NoFuture.typeProjectUpper -- multi") = {
    val su = new SymbolUniverse()
    val generator: Gen[(Scope, Symbol, Symbol, Type)] = for {
      x <- const(su.newSymbol())
      a <- const(su.newSymbol())
      scope <- genScope(su)
      (scope2, aUpperType1) <- genType(su, scope)
      aLowerType1 <- genSubtype(su, scope2, aUpperType1, Set())

      (scope3, aUpperType2) <- genType(su, scope2)
      aLowerType2 <- genSubtype(su, scope3, aUpperType2, Set())

      decl1 <- TypeDecl(a, aLowerType1, aUpperType1)
      decl2 <- TypeDecl(a, aLowerType2, aUpperType2)
      scope4 <- const(scope3 + (x -> AndType(decl1, decl2)))

      // TODO test multiple TypeDecls
      // TODO test inbetween typeprojs
      // TODO test rectypes
    } yield (scope4, x, a, AndType(aUpperType1, aUpperType2))
    // TODO test multiple declarations (should result in AndType)
    Prop.forAllNoShrink(generator){prettyProp{case (scope, x, a, aUpperType) =>
      val res = NoFuture.typeProjectUpper(scope, x, a)
      val resLabel = prettyNamed("res", res)
      resLabel |: Prop.all(res != None && NoFuture.equalTypes(scope, res.get, aUpperType))
    }}
  }


  // TODO typeProjectUpper: result does not contains rectypes.
  // TODO typeProjectUpper
  // TODO typeProjectLower
  // TODO glb
  // TODO lub
}
