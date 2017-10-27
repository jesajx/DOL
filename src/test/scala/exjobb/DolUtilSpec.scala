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


      val label = pretty(Map[String, Any](
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

      (prettyNamed("res", res)
        |: prettyNamed("res2", res2)
        |: prettyNamed("freeBefore", freeBefore)
        |: prettyNamed("freeMiddle", freeMiddle)
        |: prettyNamed("freeAfter", freeAfter)
        |: Prop.all(
          !(x != y && freeBefore(x))                   || (!freeMiddle(x) && freeMiddle(y)),
          !(x != y && freeMiddle(y))                   || (!freeAfter(y) && freeAfter(x)),
          !(x != y && freeBefore(x) && !freeBefore(y)) || (typ == res2),
          !(x == y)                                    || (typ == res && res == res2)
        )
      )
    }
  }}
}
