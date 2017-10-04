package exjobb

import Dol._

import scala.annotation.tailrec
import org.scalacheck.Properties
import org.scalacheck.Prop
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.Gen
import org.scalacheck.Gen.const
import org.scalacheck.Gen.oneOf
import org.scalacheck.Gen.someOf
import org.scalacheck.Gen.listOf
import org.scalacheck.Shrink

object DolSpec extends Properties("DolSpec") {
  val genCanonicalTop = const(CanonicalTop)
  val genCanonicalBot = const(CanonicalBot)

  def genCanonicalFunType(su: SymbolUniverse, scope: CanonicalScope): Gen[CanonicalType] = Gen.sized{ size =>
    if (size < 3)
      Gen.fail
    else for {
      x <- const(su.newSymbol())
      subSize <- size - 1 // The fun-node itself counts as one. 1+argSize+resSize == size.
      argSize <- Gen.choose(1, subSize)
      resSize <- subSize - argSize
      arg <- Gen.resize(argSize, genCanonicalType(su, scope))
      res <- Gen.resize(resSize, genCanonicalType(su, scope + (x -> arg)))
    } yield CanonicalFunType(x, arg, res, Set())
  }


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

  // TODO def custom shrinks

  def genCanonicalSupertype(su: SymbolUniverse, scope: CanonicalScope, subtype: CanonicalType): Gen[CanonicalType] = Gen.sized { size =>
    if (size == 0)
      Gen.fail
    else if (size == 1) {
      if (subtype.totNumNodes == 1)
        oneOf(const(subtype), const(CanonicalTop))
      else
        const(CanonicalBot)
    } else {
      val extra: Seq[Gen[CanonicalType]] = subtype match {
        case CanonicalFunType(x, xType, resType, projs) if (size >= 3) =>
          Seq(for {
            subSize <- size - 1
            argSize <- Gen.choose(1, subSize)
            resSize <- subSize - argSize
            xSubtype <- Gen.resize(argSize, genCanonicalSubtype(su, scope, xType))
            resSupertype <- Gen.resize(resSize, genCanonicalSupertype(su, scope + (x -> xSubtype), resType))
            newProjs <- const(Set()) //newProjs <- someOf(projs) // TODO
            // TODO supertypes of projs? // remove/keep/supertype
          } yield CanonicalFunType(x, xSubtype, resSupertype, newProjs.toSet))
          //case CanonicalObjType(x, xFields, xTypes, xProjs) if (size >= 2) => // TODO remove decls. gen supertypes of existing decls
        case _ =>
          Seq()
      }

      oneOf(const(subtype), const(CanonicalTop), extra: _*)
    }
  }

  def genCanonicalSubtype(su: SymbolUniverse, scope: CanonicalScope, supertype: CanonicalType): Gen[CanonicalType] = Gen.sized { size =>
    if (size == 0)
      Gen.fail
    else if (size == 1) {
      if (supertype.totNumNodes == 1)
        oneOf(const(supertype), const(CanonicalBot))
      else
        const(CanonicalBot)
    } else {
      val extra: Seq[Gen[CanonicalType]] = supertype match {
        case CanonicalFunType(x, xType, resType, projs) if (size >= 3) =>
          Seq(for {
            subSize <- size - 1
            argSize <- Gen.choose(1, subSize)
            resSize <- subSize - argSize
            xSupertype <- Gen.resize(argSize, genCanonicalSupertype(su, scope, xType))
            resSubtype <- Gen.resize(resSize, genCanonicalSubtype(su, scope + (x -> xType), resType))
            // TODO subtypes of projs? // keep/subtype
          } yield CanonicalFunType(x, xSupertype, resSubtype, projs))
        //case CanonicalObjType(x, xFields, xTypes, xProjs) => // TODO gen new decls. gen subtypes of existing decls
        case _ =>
          Seq()
      }

      oneOf(const(supertype), const(CanonicalBot), extra: _*)
    }
  }

  type FieldDeclMap = Map[Symbol, CanonicalType]
  type TypeDeclMap = Map[Symbol, (CanonicalType, CanonicalType)]

  def genCanonicalDecl(su: SymbolUniverse, scope: CanonicalScope): Gen[(FieldDeclMap, TypeDeclMap)] = Gen.sized{ size =>
    val a = su.newSymbol()
    val genField = for {
      aType <- Gen.resize(size, genCanonicalType(su, scope))
    } yield (Map(a -> aType), Map()): (FieldDeclMap, TypeDeclMap)
    if (size >= 2) {
      val genType = for {
        upperSize <- Gen.choose(1, size-1)
        lowerSize <- size - upperSize
        aUpperType <- Gen.resize(upperSize, genCanonicalType(su, scope))
        aLowerType <- Gen.resize(lowerSize, genCanonicalSubtype(su, scope, aUpperType))
      } yield (Map(), Map(a -> (aLowerType, aUpperType))): (FieldDeclMap, TypeDeclMap)
      oneOf(genField, genType)
    } else {
      genField
    }
  }

  def genManyCanonicalDecls(su: SymbolUniverse, scope: CanonicalScope, z: Symbol): Gen[(FieldDeclMap, TypeDeclMap)] = Gen.sized { size =>
    if (size <= 0) {
      const(Map(), Map())
    } else if (size == 1) {
      genCanonicalDecl(su, scope)
    } else {
      for {
        numLeft <- Gen.choose(1, size-1) // NOTE: non-empty.
        numRight <- const(size - numLeft)
        (leftFields, leftTypes) <- Gen.resize(numLeft, genManyCanonicalDecls(su, scope, z))
        rightScope <- const(scope + (z -> CanonicalObjType(z, leftFields, leftTypes, Set())))
        (rightFields, rightTypes) <- Gen.resize(numRight, genManyCanonicalDecls(su, rightScope, z))
      } yield (leftFields ++ rightFields, leftTypes ++ rightTypes)
    }
  }

  // TODO first figure out number of fields,types, then generate names, then generate types?
  def genCanonicalObjType(su: SymbolUniverse, scope: CanonicalScope): Gen[CanonicalType] = Gen.sized{ size =>
    for {
      x <- const(su.newSymbol())

      (fields, types) <- Gen.resize(size - 1, genManyCanonicalDecls(su, scope, x))
      // TODO also generate decls that are mutably recursive, by first generating types and then generating terms with those types.

      projs <- const(Set[TypeProj]()) // TODO pick from types and from types of objects in scope
    } yield CanonicalObjType(x, fields, types, projs)
  }

  // TODO generate non-canonical types. e.g. with intersections of multiple decls on the same member.
  def genCanonicalType(su: SymbolUniverse, scope: CanonicalScope): Gen[CanonicalType] = Gen.sized { size =>
    if (size == 0)
      Gen.fail
    else if (size == 1)
      oneOf(genCanonicalTop, genCanonicalBot)
    else
      oneOf(genCanonicalTop, genCanonicalBot, genCanonicalFunType(su, scope), genCanonicalObjType(su, scope))
  }

//  def nextSubtype(su: SymbolUniverse, scope: CanonicalScope, tp: CanonicalType): Stream[CanonicalType] = tp match {
//    case CanonicalFunType(x, CanonicalTop, CanonicalBot, EmptySet()) =>
//      Some(CanonicalBot)
//    case CanonicalFunType(x, xType, resType, projs) =>
//      val shrinkArg = for {
//        xSuperType <- nextSupertype(xType)
//      } yield CanonicalFunType(x, xSuperType, resType, projs)
//      val shrinkRes = for {
//        resSubType <- nextSubtype(xType)
//      } yield CanonicalFunType(x, xType, resSubType, projs)
//      shrinkArg #::: shrinkRes
//    case CanonicalBot =>
//      Stream()
//    case _ =>
//      Stream(CanonicalBot)
//  }
//
//  def nextSupertype(su: SymbolUniverse, scope: CanonicalScope, tp: CanonicalType): Stream[CanonicalType] = tp match {
//    case CanonicalFunType(x, CanonicalBot, CanonicalTop, EmptySet()) =>
//      Some(CanonicalTop)
//    //case CanonicalFunType(x, xType, resType, projs) =>
//    case _ =>
//      None
//  }

  //def shrinkCanonicalType(su: SymbolUniverse, scope: CanonicalScope): Shrink[CanonicalType] = Shrink { typ =>
  //  for {
  //    subtype <- nextSubtype(typ)
  //  } yield Stream(subtype)
  //}



  sealed case class InferenceProblem(term: Term, prototype: CanonicalPrototype, scope: CanonicalScope, expected: Term)

  def genVarInferenceProblem(su: SymbolUniverse, scope: CanonicalScope): Gen[InferenceProblem] = {
    val justMakeSomethingUp = for {
      x <- const(su.newSymbol())
      xType <- genCanonicalType(su, Map())
    } yield InferenceProblem(Var(x), CanonicalQue, scope + (x -> xType), Var(x).withType(xType))
    if (scope.isEmpty) {
      justMakeSomethingUp
    } else {
      val pickSomethingFromScope = for {
        x <- oneOf(scope.keys.toSeq)
        xType <- scope(x)
      } yield InferenceProblem(Var(x), CanonicalQue, scope, Var(x).withType(xType))
      oneOf(justMakeSomethingUp, pickSomethingFromScope)
    }
  }

  def genSelInferenceProblem(su: SymbolUniverse, scope: CanonicalScope): Gen[InferenceProblem] = {
    val possibilities = scope.flatMap{
      case (x, CanonicalObjType(_, fields, _, _)) => fields.toList.map{case (a, aType) => (x, a, aType)}
      case _ => List()
    }.toSeq
    if (possibilities.isEmpty)
      Gen.fail // TODO make something upp like genVarInferenceProblem() does.
    else for {
      (x, a, aType) <- oneOf(possibilities)
      if (possibilities.size != 0)
    } yield InferenceProblem(Sel(x, a), CanonicalQue, scope, Sel(x, a).withType(aType))
  }

  def genLetInferenceProblem(su: SymbolUniverse, scope: CanonicalScope): Gen[InferenceProblem] = for {
    x <- const(su.newSymbol)
    p1 <- genInferenceProblem(su, scope)
    p2 <- genInferenceProblem(su, scope + (x -> p1.expected.assignedType.get))
  } yield InferenceProblem(Let(x, p1.term, p2.term), p2.prototype, p1.scope ++ p2.scope, Let(x, p1.expected, p2.expected).withType(p2.expected.assignedType.get))

  // TODO def genObjInferenceProblem(su: SymbolUniverse, scope: CanonicalScope): Gen[InferenceProblem] // TODO generate fun then def, or vice versa?
  // TODO def genAppInferenceProblem(su: SymbolUniverse, scope: CanonicalScope): Gen[InferenceProblem] // TODO search for funs in scope and generate term from argtype // TODO gen arg, gen super of arg.type, gen function taking super giving {gen term with type}

  //def genAppInferenceProblemFromScope(su: SymbolUniverse, scope: CanonicalScope): Gen[InferenceProblem] = // TODO find function in scope (possibly as field in object), generate term with that argtype

  //def genTermFromType

  def genAppInferenceProblemFromArg(su: SymbolUniverse, scope: CanonicalScope): Gen[InferenceProblem] = for {
    f <- const(su.newSymbol())
    x <- const(su.newSymbol())
    y <- const(su.newSymbol())
    arg <- genInferenceProblem(su, scope)
    argSuper <- genCanonicalSupertype(su, scope, arg.expected.assignedType.get)
    res <- genInferenceProblem(su, scope + (x -> argSuper))
    resType <- const(res.expected.assignedType.get)
  } yield InferenceProblem(
    Let(f, Fun(x, NoFuture.decanonicalize(argSuper), res.term), Let(y, arg.term, App(f, y))),
    res.prototype,
    arg.scope ++ res.scope - x,
    Let(f,
      Fun(x, NoFuture.decanonicalize(argSuper), res.expected).withType(CanonicalFunType(x, argSuper, resType, Set())),
      Let(y, arg.expected, App(f, y).withType(resType))
      ).withType(resType))

  def genFunInferenceProblem(su: SymbolUniverse, scope: CanonicalScope): Gen[InferenceProblem] = for {
    x <- const(su.newSymbol)
    xCanonicalType <- genCanonicalType(su, scope) // TODO decanonicalize?
    body <- genInferenceProblem(su, scope + (x -> xCanonicalType))
    prototype <- const(if (body.prototype == CanonicalQue) CanonicalQue else CanonicalFunType(x, CanonicalQue, body.prototype, Set()))
  } yield InferenceProblem(
    Fun(x, NoFuture.decanonicalize(xCanonicalType), body.term),
    prototype,
    body.scope,
    Fun(x, NoFuture.decanonicalize(xCanonicalType), body.expected).
      withType(CanonicalFunType(x, xCanonicalType, body.expected.assignedType.get, Set())))

  def genFixedPointInferenceProblem(su: SymbolUniverse, scope: CanonicalScope): Gen[InferenceProblem] = for { // TODO use LightweightApp instead.
    f <- const(su.newSymbol)
    x <- const(su.newSymbol)
    y <- const(su.newSymbol)
    p <- genInferenceProblem(su, scope)
    u <- println(s"$p")
    typ <- const(p.expected.assignedType.get)
  } yield InferenceProblem(
    Let(f, Fun(x, NoFuture.decanonicalize(typ), Var(x)), Let(y, p.term, App(f, y))),
    CanonicalQue,
    p.scope,
    Let(f,
      Fun(x, NoFuture.decanonicalize(typ), Var(x).withType(typ)).withType(CanonicalFunType(x, typ, typ, Set())),
      Let(y, p.expected, App(f, y).withType(typ)).withType(typ)).withType(typ))

  def genInferenceProblem(su: SymbolUniverse, scope: CanonicalScope): Gen[InferenceProblem] = oneOf(
    genVarInferenceProblem(su, scope),
    genSelInferenceProblem(su, scope),
    genLetInferenceProblem(su, scope),
    //genObjInferenceProblem(su, scope),
    genFunInferenceProblem(su, scope),
    genFixedPointInferenceProblem(su, scope),
    genAppInferenceProblemFromArg(su, scope)
  )

  def shrinkInferenceProblem(su: SymbolUniverse, problem: InferenceProblem): Stream[InferenceProblem] = problem match {
    //case InferenceProblem(Fun(x, xType, Var(y)), prototype, scope, typ) if x == y =>
    //  InferenceProblem(Var(y), xType, scope + (x -> xType), typ)
    //case InferenceProblem(Let(x, xTerm, term), prototype, scope, typ) =>
    // TODO if we know the type of xTerm: InferenceProblem(term, prototype, scope + (x -> xType), typ)
    // TODO replace typ with subtype that does not reference x.
    // TODO remove stuff in scope that are not referenced.
    case _ =>
      Stream()
  }

  property("positiveParallelInferenceProblem") = {
    val su = new SymbolUniverse()
    Prop.forAllShrink(genInferenceProblem(su, Map()), shrinkInferenceProblem(su, _: InferenceProblem)) { (problem: InferenceProblem) =>
      // TODO check type of all terms, not just top.
      val res = typecheckInParallel(su, problem.term, problem.prototype, problem.scope)
      ("res = " + res + "") |: Prop.all(
        res != None && NoFuture.equalTerms(su, res.get, problem.expected)) // TODO comparison that does not depend on var names.
    }
  }

  //property("genCanonicalTypeSizeIsConsistentWithDolNotionOfSize") = { // TODO better name
  //  // TODO check that if we run a Gen[CanonicalType]-generator with size N,
  //  // the resulting generate type has _.totNumNodes <= N
  //}

  def shiv(): Unit = { // code to run in repl
    val su = new SymbolUniverse()
    val maybeProblem = genInferenceProblem(su, Map()).sample
    maybeProblem match {
      case Some(problem) =>
          val res = typecheckInParallel(su, problem.term, problem.prototype, problem.scope)
      case None =>
        println("maybeProblem=None")
    }

    Prop.forAllShrink(genInferenceProblem(su, Map()), shrinkInferenceProblem(su, _: InferenceProblem)) { (problem: InferenceProblem) =>
      val res = typecheckInParallel(su, problem.term, problem.prototype, problem.scope)
      ("res = " + res + "") |: Prop.all(
        res != None && NoFuture.equalTerms(su, res.get, problem.expected)) // TODO comparison that does not depend on var names.
    }.check
    println(properties)
  }

}
/* TODO:

[info] Compiling 1 Scala source to /home/jesajx/prg/exjobb-code/target/scala-2.11/test-classes...
[info] ! DolSpec.positiveInferenceProblem: Falsified after 21 passed tests.
[info] > Labels of failing property:
[info] res = None
[info] > ARG_0: InferenceProblem(Fun(2,FunType(5,Bot,ErrorType),Var(2)),CanonicalQue,Map(2 -> CanonicalFunType(5,CanonicalBot,CanonicalObjType(8,Map(),Map(),Set()),Set())),CanonicalFunType(2,CanonicalFunType(5,CanonicalBot,CanonicalObjType(8,Map(),Map(),Set()),Set()),CanonicalFunType(5,CanonicalBot,CanonicalObjType(8,Map(),Map(),Set()),Set()),Set()))    // TODO "ErrorType"???? why?
[info] Failed: Total 1, Failed 1, Errors 0, Passed 0
[error] Failed tests:
[error]         exjobb.DolSpec
[error] (root/test:test) sbt.TestsFailedException: Tests unsuccessful
[error] Total time: 1 s, completed Oct 4, 2017 12:01:07 PM


[info] Compiling 1 Scala source to /home/jesajx/prg/exjobb-code/target/scala-2.11/classes...
[info] ! DolSpec.positiveParallelInferenceProblem: Falsified after 1 passed tests.
[info] > Labels of failing property:
[info] res = None
[info] > ARG_0: InferenceProblem(
  Let(4,Fun(5,Bot,Fun(14,RecType(17,AndType(AndType(AndType(AndType(AndType(AndType(AndType(FieldDecl(24,Top),FieldDecl(25,Bot)),FieldDecl(20,Bot)),FieldDecl(21,Top)),FieldDecl(22,Top)),FieldDecl(18,Top)),FieldDecl(23,Bot)),FieldDecl(19,Bot))),Var(14))),Let(6,Var(7),App(4,6))),
  CanonicalQue,Map(7 -> CanonicalBot, 14 -> CanonicalObjType(17,Map(24 -> CanonicalTop, 25 -> CanonicalBot, 20 -> CanonicalBot, 21 -> CanonicalTop, 22 -> CanonicalTop, 18 -> CanonicalTop, 23 -> CanonicalBot, 19 -> CanonicalBot),Map(),Set())),
  CanonicalFunType(14,CanonicalObjType(17,Map(24 -> CanonicalTop, 25 -> CanonicalBot, 20 -> CanonicalBot, 21 -> CanonicalTop, 22 -> CanonicalTop, 18 -> CanonicalTop, 23 -> CanonicalBot, 19 -> CanonicalBot),Map(),Set()),CanonicalObjType(17,Map(24 -> CanonicalTop, 25 -> CanonicalBot, 20 -> CanonicalBot, 21 -> CanonicalTop, 22 -> CanonicalTop, 18 -> CanonicalTop, 23 -> CanonicalBot, 19 -> CanonicalBot),Map(),Set()),Set()))
[info] Failed: Total 1, Failed 1, Errors 0, Passed 0
[error] Failed tests:
[error]         exjobb.DolSpec
[error] (root/test:test) sbt.TestsFailedException: Tests unsuccessful


  Let(4,
      Fun(5,
          Bot,
          Fun(14,
            RecType(17,
              andType(
                FieldDecl(24,Top),
                FieldDecl(25,Bot),
                FieldDecl(20,Bot),
                FieldDecl(21,Top),
                FieldDecl(22,Top),
                FieldDecl(18,Top),
                FieldDecl(23,Bot),
                FieldDecl(19,Bot))),
            Var(14))),
      Let(6,Var(7),App(4,6))),

  : CanonicalFunType(14,
      CanonicalObjType(17,
        Map(24 -> CanonicalTop, 25 -> CanonicalBot, 20 -> CanonicalBot, 21 -> CanonicalTop, 22 -> CanonicalTop, 18 -> CanonicalTop, 23 -> CanonicalBot, 19 -> CanonicalBot),
        Map(),
        Set()),
      CanonicalObjType(17,
        Map(24 -> CanonicalTop, 25 -> CanonicalBot, 20 -> CanonicalBot, 21 -> CanonicalTop, 22 -> CanonicalTop, 18 -> CanonicalTop, 23 -> CanonicalBot, 19 -> CanonicalBot),
        Map(),
        Set()),
      Set()))

*/
