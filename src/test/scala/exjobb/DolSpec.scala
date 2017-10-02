package exjobb

import Dol._

import scala.annotation.tailrec
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.all
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.Gen
import org.scalacheck.Gen.const
import org.scalacheck.Gen.oneOf
import org.scalacheck.Gen.listOf
import org.scalacheck.Arbitrary.arbitrary

object DolSpec extends Properties("DolSpec") {
  val genCanonicalTop = const(CanonicalTop)
  val genCanonicalBot = const(CanonicalBot)

  def genCanonicalFunType(su: SymbolUniverse, scope: CanonicalScope): Gen[CanonicalType] = for {
    x <- su.newSymbol()
    arg <- genCanonicalType(su, scope)
    res <- genCanonicalType(su, scope + (x -> arg))
  } yield CanonicalFunType(x, arg, res, Set())


//  sealed case class Graph(numNodes: Int, edges: Set[(Int, Int)])
//
//  def genNonCyclicEdges(numNodes: Int, numEdges: Int): Gen[Set[(Int, Int)]] = {
//    val theoreticalMaxNumEdges = numNodes*(0 + numNodes-1)/2
//    if (numEdges > theoreticalMaxNumEdges) {
//      genNonCyclicEdges(numNodes, theoreticalMaxNumEdges)
//    } else {
//      var res = Set[(Int, Int)]()
//      if (numNodes >= 2) {
//        val edgeGen = for {
//          x <- Gen.choose(1, numNodes-1)
//          y <- Gen.choose(0, x-1)
//        } yield (x, y)
//
//        while (res.size < numEdges) {
//          val Some(edge) = edgeGen.sample // TODO can fail?
//          if (!res.contains(edge)) {
//            res = (res + edge)
//          }
//        }
//      }
//      res
//    }
//  }
//  def genNonCyclicGraph: Gen[Graph] = Gen.sized { size =>
//    val numNodes = size // TODO divide size between numNodes and numEdges
//    for {
//      // Node "x" may only depend node "y" if y is represented by a lower number than x is.
//      // In the worst case "x" depends on all nodes before it (once each).
//      numEdges <- Gen.choose(0, numNodes*(0 + numNodes-1)/2)
//      edges <- genNonCyclicEdges(numNodes, numEdges)
//    } yield Graph(numNodes, edges)
//  }

  // TODO split Gen "size" into width and height using arbitrary int
  // 0..size-1.

  def genCanonicalSubtype(su: SymbolUniverse, scope: CanonicalScope, supertype: CanonicalType): Gen[CanonicalType] = oneOf(
    const(supertype),
    const(CanonicalBot)
    // TODO
  )

  def genCanonicalDecl(su: SymbolUniverse, scope: CanonicalScope): Gen[CanonicalObjType] = oneOf(
    for {
      a <- su.newSymbol()
      aType <- genCanonicalType(su, scope)
    } yield CanonicalObjType(-1, Map(a -> aType), Map(), Set()),
    for {
      a <- su.newSymbol()
      aUpperType <- genCanonicalType(su, scope)
      aLowerType <- genCanonicalSubtype(su, scope, aUpperType)
    } yield CanonicalObjType(-1, Map(), Map(a -> (aLowerType, aUpperType)), Set())
  )

  @tailrec
  def genManyCanonicalDecls(su: SymbolUniverse, scope: CanonicalScope, z: Symbol, numDecls: Int, curFields: Map[Symbol, CanonicalType], curTypes: Map[Symbol, (CanonicalType, CanonicalType)]): (Map[Symbol, CanonicalType], Map[Symbol, (CanonicalType, CanonicalType)]) = {
    val objType = CanonicalObjType(z, curFields, curTypes, Set())
    if (numDecls == 0) {
      (curFields, curTypes)
    } else {
      val CanonicalObjType(_, fields, types, _) = genCanonicalDecl(su, scope + (z -> objType)).sample.get // TODO
      genManyCanonicalDecls(su, scope, z, numDecls - 1, curFields ++ fields, curTypes ++ types)
    }
  }

  // TODO first figure out number of fields,types, then generate names, then generate types?
  def genCanonicalObjType(su: SymbolUniverse, scope: CanonicalScope): Gen[CanonicalType] = Gen.sized{ size =>
    for {
      x <- const(su.newSymbol())

      numDecls <- Gen.choose(0, 100) // TODO base on size?
      (fields, types) <- const(genManyCanonicalDecls(su, scope, x, numDecls, Map(), Map()))

      projs <- const(Set[TypeProj]()) // TODO search all objects
    } yield CanonicalObjType(x, fields, types, projs)
  }

  def genCanonicalType(su: SymbolUniverse, scope: CanonicalScope): Gen[CanonicalType] =
    oneOf(genCanonicalTop, genCanonicalBot, genCanonicalFunType(su, scope))
    //oneOf(genCanonicalTop, genCanonicalBot, genCanonicalFunType(su, scope), genCanonicalObjType(su, scope))





  sealed case class InferenceProblem(term: Term, prototype: CanonicalPrototype, scope: CanonicalScope, typ: CanonicalType)

  def genVarInferenceProblem(su: SymbolUniverse, scope: CanonicalScope): Gen[InferenceProblem] = for {
    // TODO vs picking something from scope
    x <- const(su.newSymbol())
    t <- genCanonicalType(su, Map())
  } yield InferenceProblem(Var(x), CanonicalQue, scope + (x -> t), t)

  def genSelInferenceProblem(su: SymbolUniverse, scope: CanonicalScope): Gen[InferenceProblem] = {
    val possibilities = scope.flatMap{
      case (x, CanonicalObjType(_, fields, _, _)) => fields.toList.map{case (a, aType) => (x, a, aType)}
      case _ => List()
    }.toSeq
    if (possibilities.isEmpty) { // TODO some better way to do this? oneOf throws exceptions if possibilities is empty.
      for {
        x <- 1
        if 1 == 0
      } yield InferenceProblem(Var(x), CanonicalQue, Map(), CanonicalBot)
    } else {
      for {
        someField <- oneOf(possibilities)
        if (possibilities.size != 0)
      } yield InferenceProblem(Sel(someField._1, someField._2), CanonicalQue, scope, someField._3)
    }
  }

  def genLetInferenceProblem(su: SymbolUniverse, scope: CanonicalScope): Gen[InferenceProblem] = for {
    x <- const(su.newSymbol)
    p1 <- genInferenceProblem(su, scope)
    p2 <- genInferenceProblem(su, scope + (x -> p1.typ))
  } yield InferenceProblem(Let(x, p1.term, p2.term), p2.prototype, p1.scope ++ p2.scope, p2.typ)

  // TODO def genObjInferenceProblem(su: SymbolUniverse, scope: CanonicalScope): Gen[InferenceProblem] // TODO generate fun then def, or vice versa?
  // TODO def genAppInferenceProblem(su: SymbolUniverse, scope: CanonicalScope): Gen[InferenceProblem] // TODO search for funs in scope and generate term from argtype
  def genFunInferenceProblem(su: SymbolUniverse, scope: CanonicalScope): Gen[InferenceProblem] = for {
    x <- const(su.newSymbol)
    xCanonicalType <- genCanonicalType(su, scope) // TODO decanonicalize?
    body <- genInferenceProblem(su, scope + (x -> xCanonicalType))
    prototype <- const(if (body.prototype == CanonicalQue) CanonicalQue else CanonicalFunType(x, CanonicalQue, body.prototype, Set()))
  } yield InferenceProblem(Fun(x, NoFuture.decanonicalize(xCanonicalType), body.term), prototype, body.scope, CanonicalFunType(x, xCanonicalType, body.typ, Set()))

  def genFixedPointInferenceProblem(su: SymbolUniverse, scope: CanonicalScope): Gen[InferenceProblem] = for { // TODO use LightweightApp instead.
    f <- const(su.newSymbol)
    x <- const(su.newSymbol)
    y <- const(su.newSymbol)
    p <- genInferenceProblem(su, scope)
  } yield InferenceProblem(Let(f, Fun(x, NoFuture.decanonicalize(p.typ), Var(x)), Let(y, p.term,  App(f, y))), CanonicalQue, p.scope, p.typ)

  def genInferenceProblem(su: SymbolUniverse, scope: CanonicalScope): Gen[InferenceProblem] = oneOf(
    genVarInferenceProblem(su, scope),
    genSelInferenceProblem(su, scope),
    genLetInferenceProblem(su, scope),
    //genObjInferenceProblem(su, scope),
    //genAppInferenceProblem(su, scope),
    genFunInferenceProblem(su, scope),
    genFixedPointInferenceProblem(su, scope)
  )

//  property("var") = {
//    val su = new SymbolUniverse()
//    val x = su.newSymbol
//    forAll(genCanonicalType(su, Map())) { (t: CanonicalType) => // TODO gen type with symboluniverse?
//      pinfer(su, Var(x), CanonicalQue, Map(x -> t)) == Some(t)
//    }
//  }

  property("meh") = {
    val su = new SymbolUniverse()
    forAll(genInferenceProblem(su, Map())) { (problem: InferenceProblem) =>
      val res = pinfer(su, problem.term, problem.prototype, problem.scope)
      ("res = " + res + "") |: all(
        res != None && NoFuture.canonicalEqualTypes(res.get, problem.typ)) // TODO comparison that does not depend on var names.
    }
  }

  //property("let") = forAll(genTermAndType) { (et: TermAndType) =>
  //  val su = new SymbolUniverse(1000) // TODO
  //  val x = su.newSymbol
  //  pinfer(su, Let(x, et.term, Var(x)), CanonicalQue, Map()) == Some(et.typ)
  //}

}
