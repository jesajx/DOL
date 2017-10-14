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

object DolSpec extends Properties("DolSpec") { // TODO REM and use other specs directly instead? include other specs here?
  // TODO some ideas: <http://www.michaelburge.us/2017/09/27/delta-debugging-in-haskell.html>

  // TODO Include type sizes in Gen size?

  // TODO Maybe just define generators like normal function and let caller
  // wrap in Gen as necessary? The problem is distributing the randomseed
  // manually...







  //property("genCanonicalTypeSizeIsConsistentWithDolNotionOfSize") = { // TODO better name
  //  // TODO check that if we run a Gen[CanonicalType]-generator with size N,
  //  // the resulting generate type has _.totNumNodes <= N
  //}


  property("NoFuture.projectUpperType") = {
    val su = new SymbolUniverse()
    val generator: Gen[(Scope, Symbol, Symbol, Type)] = for {
      x <- const(su.newSymbol())
      a <- const(su.newSymbol())
      aUpperType <- genType(su, Map()) // TODO populate scope?
      aLowerType <- genSubtype(su, Map(), aUpperType, Set()) // TODO populate scope?
      scope <- const(Map(x -> TypeDecl(a, aLowerType, aUpperType)))
    } yield (scope, x, a, aUpperType)
    // TODO test multiple declarations (should result in AndType)
    Prop.forAllNoShrink(generator) {case (scope, x, a, aUpperType) =>
      val res = NoFuture.typeProjectUpper(scope, x, a)
      s"res = $res" |: Prop.all(res != None && NoFuture.equalTypes(scope, res.get, aUpperType))
    }
  }


  // TODO raise type to exact prototype (i.e. supertype of type)

//  property("NoFuture.raise -- simply fill in gaps") = { // TODO vs testing constraintSolver directly?
//    val su = new SymbolUniverse()
//    val scope: Scope = Map()
//    val generator: Gen[(Type, Prototype)] = for {
//      typ <- genType(su, scope)
//      prototype <- genUpperPrototypeFromType(su, scope, typ)
//    } yield (typ, prototype)
//
//    Prop.forAllNoShrink(generator) {case (typ, prototype) =>
//      val res = NoFuture.raise(su, scope, typ, prototype)
//      s"res = $res" |: Prop.all(res != None && NoFuture.equalTypes(scope, res.get, typ))
//    }
//  }
//
//  property("NoFuture.lower -- simply fill in gaps") = { // TODO vs testing constraintSolver directly?
//    val su = new SymbolUniverse()
//    val scope: Scope = Map()
//    val generator: Gen[(Type, Prototype)] = for {
//      typ <- genType(su, scope)
//      prototype <- genLowerPrototypeFromType(su, scope, typ)
//    } yield (typ, prototype)
//
//    Prop.forAllNoShrink(generator) {case (typ, prototype) =>
//      val res = NoFuture.lower(su, scope, typ, prototype)
//      s"res = $res" |: Prop.all(res != None && NoFuture.equalTypes(scope, res.get, typ))
//    }
//  }




  // TODO To be more FP-pure, the symboluniverse should probably be wrapped
  // together with other stuff in a monad...


  // TODO check utility-functions. raise, lower, glb, lub, etc.

  // TODO check eliminateVarUp by starting with a type with no vars, and then
  // replacing parts with vars.  scope={},Int  --> scope={x->{T=Int}},x.T

}
