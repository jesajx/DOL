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
        const(CanonicalTop)
    } else {
      val extra: Seq[Gen[CanonicalType]] = subtype match {
        case CanonicalFunType(x, xType, resType, projs) if (size >= 3) =>
          Seq(for {
            subSize <- size - 1
            argSize <- Gen.choose(1, subSize)
            resSize <- subSize - argSize
            xSubtype     <- Gen.resize(argSize, genCanonicalSubtype(su, scope, xType))
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
//        resSubType <- nextSubtype(resType)
//      } yield CanonicalFunType(x, xType, resSubType, projs)
//      val shrinkProjs = for {
//        newProjs <- someOf(projs)
//      } yield CanonicalFunType(x, xType, resType, newProjs)
//      shrinkArg #::: shrinkRes #::: shrinkProjs
//    case CanonicalBot =>
//      Stream()
//    case _ =>
//      Stream(CanonicalBot)
//  }
//
//  def nextSupertype(su: SymbolUniverse, scope: CanonicalScope, tp: CanonicalType): Stream[CanonicalType] = tp match {
//    case CanonicalFunType(x, CanonicalBot, CanonicalTop, EmptySet()) =>
//      Some(CanonicalTop)
//    case CanonicalObjType(x, fields, types, projs) =>
//      val shrinkFieldsByRemoving = for {
//        newFields <- someOf(fields) if newFields.size < fields.size // TODO only remove one?
//      } yield CanonicalObjType(x, newFields, types, projs)
//      val shrinkFieldsBySuperTyping = for {
//        newFields <- fields.map{case (a, aType) =>
//          ???
//        }
//      } yield CanonicalObjType(x, newFields, types, projs)
//      val shrinkTypesByRemoving = for {
//        newTypes <- someOf(types) if newTypes.size < types.size // TODO check if someone is using them...
//      } yield CanonicalObjType(x, newFields, newTypes, projs)
//      val shrinkProjsByRemoving = for {
//        newProjs <- someOf(projs) if newProjs.size < projs.size
//      } yield CanonicalObjType(x, newFields, types, newProjs)
//    //case CanonicalFunType(x, xType, resType, projs) =>
//    case _ =>
//      None
//  }

  //def shrinkCanonicalType(su: SymbolUniverse, scope: CanonicalScope): Shrink[CanonicalType] = Shrink { typ =>
  //  for {
  //    subtype <- nextSubtype(typ)
  //  } yield Stream(subtype)
  //}



  sealed case class InferenceProblem(term: Term, prototype: CanonicalPrototype, scope: CanonicalScope, expected: Term) {
    if (!isTermFullyTyped(expected)) throw new Exception(s"not fully typed: $expected")
    override def toString() = {
      s"InferenceProblem($term, $prototype, $scope, ${NoFuture.stringExprWithTypeIfExists(expected)})"
    }
  }

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

  def genSelInferenceProblem(su: SymbolUniverse, scope: CanonicalScope): Gen[InferenceProblem] = { // TODO Gen.sized
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
      Let(y, arg.expected, App(f, y).withType(resType)).withType(resType)
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

  def isTermFullyTyped(term: Term): Boolean = {
    if (term.assignedType == None) {
      return false
    }
    term match {
      case Let(x, xTerm, resTerm) =>
        isTermFullyTyped(xTerm) && isTermFullyTyped(resTerm)
      case Obj(x, xType, d) =>
        NoFuture.defAsMap(d).values.map{
          case FieldDef(a, aTerm) => isTermFullyTyped(aTerm)
          case _ => true
        }.reduce{_ && _}
      case Fun(x, xType, resTerm) =>
        isTermFullyTyped(resTerm)
      case _ => true // Var, App, Sel
    }
  }

  def shrinkScopeIfPossible(problem: InferenceProblem): InferenceProblem = {

    def bfs(usedVars: Set[Symbol]): Set[Symbol] = {
      val reachable = usedVars.flatMap{x =>
        NoFuture.freeVarsInCanonicalType(problem.scope(x))
      }
      val newUsedVars: Set[Symbol] = usedVars ++ reachable
      if (newUsedVars.size == usedVars.size)
        usedVars
      else
        bfs(newUsedVars)
    }

    val directlyUsedVars = problem.scope.keys.filter{x =>
      (NoFuture.isVarFreeInExpr(x, problem.term)
        || NoFuture.isVarFreeInCanonicalType(x, problem.prototype))
    }.toSet
    val allUsedVars = bfs(directlyUsedVars)

    val newScope = allUsedVars.map{x => (x, problem.scope(x))}.toMap
    InferenceProblem(problem.term, problem.prototype, newScope, problem.expected)
  }

  def shrinkScope(problem: InferenceProblem): Stream[InferenceProblem] = {
    val newProblem = shrinkScopeIfPossible(problem)
    if (newProblem.scope.size < problem.scope.size) Stream(newProblem) else Stream()
  }

  def shrinkInferenceProblem(su: SymbolUniverse, problem: InferenceProblem, isRoot: Boolean = false): Stream[InferenceProblem] = {
    var res = Stream[InferenceProblem]()

    res = res #::: (problem match {
      case InferenceProblem(Sel(x, a), prototype, scope, expected) =>
        scope.get(x) match {
          case Some(xType @ CanonicalObjType(y, fields, types, projs)) =>
            val aType = fields(a)
            val w = su.newSymbol()
            val newScope =
              if (scope.contains(y))
                scope + (w -> aType)
              else
                scope + (w -> aType) + (y -> xType) // TODO
            Stream(InferenceProblem(Var(w), prototype, newScope, Var(w).withType(expected.assignedType.get)))
          case _ =>
            Stream()
        }
      case _ => Stream()
    })

    res = res #::: (problem match {
      case InferenceProblem(App(x, y), prototype, scope, expected) =>
        scope.get(x) match {
          case Some(CanonicalFunType(z, zType, zResType, projs)) =>
            val w = su.newSymbol()
            Stream(InferenceProblem(Var(w), prototype, scope + (w -> zResType), Var(w).withType(zResType)))
          case _ =>
            Stream()
        }
      case _ => Stream()
    })
    res = res #::: (problem match {
      case InferenceProblem(Let(x, xTerm, resTerm), prototype, scope, expected @ Let(_, expectedXTerm, expectedResTerm)) =>
        val xProblem = InferenceProblem(xTerm, CanonicalQue, scope, expectedXTerm)
        val resProblem = InferenceProblem(resTerm, prototype, scope + (x -> expectedXTerm.assignedType.get), expectedResTerm)

        val expectedType = expected.assignedType.get

        val leftShrinks = for {
          newXProblem <- shrinkInferenceProblem(su, xProblem)
        } yield InferenceProblem(Let(x, newXProblem.term, resTerm), prototype, scope ++ newXProblem.scope, Let(x, newXProblem.expected, expectedResTerm).withType(expectedType))

        val rightShrinks = for {
          newResProblem <- shrinkInferenceProblem(su, resProblem)
        } yield InferenceProblem(Let(x, xTerm, newResProblem.term), prototype, scope ++ newResProblem.scope, Let(x, expectedXTerm, newResProblem.expected).withType(expectedType))

        if (isRoot) {
          xProblem #:: resProblem #:: leftShrinks #::: rightShrinks
        } else {
          resProblem #:: rightShrinks
        }
      case _ => Stream()
    })
    res = res #::: (problem match {
      case InferenceProblem(Fun(x, xType, resTerm), prototype, scope, expected @ Fun(_, _, expectedResTerm)) =>

        val resPrototype = CanonicalQue // TODO base on prototype
        val resProblem = InferenceProblem(resTerm, resPrototype, scope + (x -> NoFuture.canonicalize(su, scope, x, xType)), expectedResTerm)
        val resShrinks = for {
          newResProblem <- shrinkInferenceProblem(su, resProblem)
        } yield InferenceProblem(Fun(x, xType, newResProblem.term), prototype, scope ++ newResProblem.scope, Fun(x, xType, newResProblem.expected).withType(expected.assignedType.get)) // TODO do something with prototype

        if (isRoot)
          resProblem #:: resShrinks
        else
          resShrinks
      case _ => Stream()
    })
    // TODO remove function if root?
    // TODO replace typ with subtype that does not reference x.
    // TODO remove stuff in scope that are not referenced.
    res.map{shrinkScopeIfPossible}
  }

  // TODO
  property("positiveParallelInferenceProblem") = {
    println("p")
    val su = new SymbolUniverse()
    Prop.forAllShrink(Gen.resize(3, genInferenceProblem(su, Map())), shrinkInferenceProblem(su, _: InferenceProblem, isRoot=true)) { (problem: InferenceProblem) =>
      val res = typecheckInParallel(su, problem.term, problem.prototype, problem.scope)

      val resString = res match {
        case Some(resTerm) => s"res = Some(${NoFuture.stringExprWithType(resTerm)})"
        case None => "res = None"
      }
      //val expectedString = s"expected = ${NoFuture.stringExprWithType(problem.expected)}"

      //expectedString |:
      resString |: Prop.all(
        res != None && NoFuture.equalTerms(su, res.get, problem.expected))
    }
  }

  //property("genCanonicalTypeSizeIsConsistentWithDolNotionOfSize") = { // TODO better name
  //  // TODO check that if we run a Gen[CanonicalType]-generator with size N,
  //  // the resulting generate type has _.totNumNodes <= N
  //}

//  property("shrinkPreservesTypecorrectness") = { // TODO
//    //println("s")
//    val su = new SymbolUniverse()
//    val generator: Gen[(InferenceProblem, InferenceProblem)] = for {
//      problem <- genInferenceProblem(su, Map()) //if typecheckInParallel(su, problem.term, problem.prototype, problem.scope) != None
//      shrunkProblems <- const(shrinkInferenceProblem(su, problem, isRoot=true)) if !shrunkProblems.isEmpty
//      newProblem <- oneOf(shrunkProblems)
//    } yield (problem, newProblem)
//    Prop.forAllNoShrink(generator) { (origAndShrunk: (InferenceProblem, InferenceProblem)) =>
//      val (origProblem, newProblem) = origAndShrunk
//      (s"newProblem = $newProblem"
//        |: s"origProblem = $origProblem"
//        |: Prop.all(
//          typecheckInParallel(su, origProblem.term, origProblem.prototype, origProblem.scope) == None
//          || typecheckInParallel(su, newProblem.term, newProblem.prototype, newProblem.scope) != None))
//    }
//  }

  def shiv(): Unit = {
    val su = new SymbolUniverse(4000)
    val term = Fun(9,RecType(12,AndType(FieldDecl(13,Bot),FieldDecl(14,Top))),Fun(20,RecType(37,AndType(FieldDecl(38,Top),FieldDecl(39,Top))),Var(20)))
    val prototype = CanonicalQue
    val scope: CanonicalScope = Map()
    val res = typecheckInParallel(su, term, prototype, scope)
    res match {
      case Some(resTerm) =>
        println(s"res --> ${NoFuture.stringExprWithTypeIfExists(resTerm)}")
      case None =>
        println(s"res --> None")
    }
  }

  // TODO check canonicalize(decanonicalize(_))

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
