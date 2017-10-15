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
    Prop.forAllNoShrink(generator){pretty{case (su, (scope, typ)) =>
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


      val label = pprint.apply(Map(
        "typ            " -> typ,
        "expectedAllVars" -> expectedAllVars,
        "allVars        " -> allVars,
        "freeVars       " -> freeVars,
        "boundVars      " -> boundVars,
        "fieldMembers   " -> fieldMembers,
        "typeMembers    " -> typeMembers,
        "counts         " -> counts
      )).render


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
    Prop.forAllNoShrink(generator){pretty{ case (newScope, x, upper) =>
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
    Prop.forAllNoShrink(generator){pretty{ case (su, scope, x, y, typ) =>
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
}
