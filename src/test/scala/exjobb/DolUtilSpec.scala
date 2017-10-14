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
  property("NoFuture.allVarsInType") = {
    val generator: Gen[(SymbolUniverse, Type)] = withSymbolUniverse{genType(_, Map())}
    Prop.forAllNoShrink(generator) {case (su, typ) =>
      val res = NoFuture.allVarsInType(typ)
      val expected = (0 until su.count()).toSet
      s"res = $res, expected = $expected" |: Prop.all(res == expected)
    }
  }

  property("NoFuture.allVarsInType - boundVarsInType == freeVarsInType") = {
    val generator: Gen[(SymbolUniverse, Type)] = withSymbolUniverse{genType(_, Map())}
    Prop.forAllNoShrink(generator) {case (su, typ) =>
      val expected = (0 until su.count()).toSet
      val allVars = NoFuture.allVarsInType(typ)
      val boundVars = NoFuture.boundVarsInType(typ)
      val freeVars = NoFuture.freeVarsInType(typ)

      s"expected = $expected, allVars = $allVars, boundVars = $boundVars, freeVars = $freeVars" |: Prop.all(
        allVars == expected,
        boundVars.intersect(freeVars) == Set(),
        allVars -- boundVars == freeVars
      )
    }
  }

  property("NoFuture.isSubtypeOf(t,t)") = {
    val generator: Gen[(Scope, Type)] = for {
      su <- const(new SymbolUniverse())
      scope <- genScope(su)
      typ <- genType(su, scope)
    } yield (scope, typ)
    Prop.forAllNoShrink(generator) { case (scope, typ) =>
      NoFuture.isSubtypeOf(scope, typ, typ)
    }
  }

  property("NoFuture.isSubtypeOf") = {
    val generator: Gen[(Scope, Type, Type)] = for {
      su <- const(new SymbolUniverse())
      scope <- genScope(su)
      lower <- genType(su, scope)
      upper <- genSupertype(su, scope, lower, Set())
    } yield (scope, lower, upper)
    Prop.forAllNoShrink(generator) { case (scope, lower, upper) =>
      NoFuture.isSubtypeOf(scope, lower, upper)
    }
  }

  property("NoFuture.typeRenameVar") = {
    val generator: Gen[(SymbolUniverse, Symbol, Symbol, Type)] = for {
      su <- const(new SymbolUniverse())
      typ <- genType(su, Map())
      x <- Gen.oneOf((NoFuture.freeVarsInType(typ) + su.newSymbol()).toSeq) // NOTE: x != y. Had to check scalacheck src to know that....
      y <- Gen.oneOf(((NoFuture.freeVarsInType(typ) + x) + su.newSymbol()).toSeq)
    } yield (su, x, y, typ)
    // TODO shrink type?
    Prop.forAllNoShrink(generator) { case (su, x, y, typ) =>
      val freeBefore = NoFuture.freeVarsInType(typ)
      val res = NoFuture.typeRenameVar(x, y, typ)
      val freeMiddle = NoFuture.freeVarsInType(res)
      val res2 = NoFuture.typeRenameVar(y, x, res)
      val freeAfter = NoFuture.freeVarsInType(res2)

      Prop.all(
        !(x != y && freeBefore(x)) || (!freeMiddle(x) && freeMiddle(y)),
        !(x != y && freeMiddle(y)) || (!freeAfter(y) && freeAfter(x)),
        !(x != y && freeBefore(x) && !freeBefore(y)) || (typ == res2),
        !(x == y) || (typ == res && res == res2)
      )
    }
  }
}
