package exjobb

import java.util.concurrent.atomic.AtomicBoolean
import Dol._
import cell._

/** For debugging. */
object Main {
  import NoFuture._

  def main(args: Array[String]): Unit = {
lowerdebug()
//raisedebug()
//debugsub()
  }

  sealed case class GlobalContext(scope: Scope, nextSymbol: Int)

  val pprintTerminalWidth = 80
  val pprintWidth = pprintTerminalWidth
  val pprintHeight = 100000

  object P extends pprint.PPrinter(defaultWidth=pprintWidth, defaultHeight=pprintHeight) {
    def named[T](name: String, x: T): String = {
      val prefix = name + " = "
      prefix + P(x, initialOffset=prefix.size).render
    }
    def namedln[T](name: String, x: T): Unit = {
      val prefix = name + " = "
      print(prefix)
      P.pprintln(x, initialOffset=prefix.size)
    }
  }


def lowerdebug(): Unit = {
val (GlobalContext(scope1, nextSymbol), z, r, a, p): (GlobalContext, Symbol, Symbol, Type, Prototype) =

//(GlobalContext(Map(), 2), 1, 0, Bot, Top)

//(
//  GlobalContext(
//    Map(
//      6 -> TypeDecl(7, Top, Top),
//      4 -> TypeDecl(5, TypeProj(6, 7), TypeProj(6, 7)),
//      2 -> TypeDecl(3, TypeProj(4, 5), TypeProj(4, 5))
//    ),
//    8
//  ),
//  1,
//  0,
//  Bot,
//  TypeProj(2, 3)
//)

//( // TODO debug slowness of dnf
//  GlobalContext(
//    Map(
//      6 -> TypeDecl(7, Bot, FieldDecl(8, Bot)),
//      4 -> TypeDecl(5, TypeProj(6, 7), TypeProj(6, 7)),
//      17 -> TypeDecl(
//        18,
//        Bot,
//        FunType(
//          19,
//          Top,
//          FieldDecl(
//            20,
//            TypeDecl(21, Bot, RecType(22, FieldDecl(23, Bot)))
//          )
//        )
//      )
//    ),
//    25
//  ),
//  1,
//  0,
//  AndType(
//    AndType(FieldDecl(2, Top), FieldDecl(3, TypeProj(4, 5))),
//    AndType(
//      AndType(
//        TypeDecl(9, Bot, Bot),
//        AndType(
//          FieldDecl(
//            10,
//            TypeDecl(
//              11,
//              Bot,
//              AndType(
//                FieldDecl(12, Top),
//                FieldDecl(13, FieldDecl(14, Bot))
//              )
//            )
//          ),
//          TypeDecl(15, Bot, Bot)
//        )
//      ),
//      AndType(
//        TypeDecl(16, TypeProj(17, 18), TypeProj(17, 18)),
//        TypeDecl(24, Top, Top)
//      )
//    )
//  ),
//  //AndType(
//  //  AndType(FieldDecl(2, Top), FieldDecl(3, TypeProj(4, 5))),
//  //  AndType(
//  //    AndType(
//  //      TypeDecl(9, Bot, Bot),
//  //      AndType(
//  //        FieldDecl(
//  //          10,
//  //          TypeDecl(
//  //            11,
//  //            Bot,
//  //            AndType(
//  //              FieldDecl(12, Top),
//  //              FieldDecl(13, FieldDecl(14, Bot))
//  //            )
//  //          )
//  //        ),
//  //        TypeDecl(15, Bot, Bot)
//  //      )
//  //    ),
//  //    AndType(
//  //      TypeDecl(16, TypeProj(17, 18), TypeProj(17, 18)),
//  //      TypeDecl(24, Top, Top)
//  //    )
//  //  )
//  //),
//  AndType(
//    AndType(FieldDecl(2, Top), Que),
//    AndType(
//      AndType(TypeDecl(9, Bot, Bot), Que),
//      AndType(Que, TypeDecl(24, Top, Que))
//    )
//  )
//)

( // TODO debug slowness of dnf
  GlobalContext(
    Map(
      33 -> TypeDecl(44, Bot, Top),
      22 -> TypeDecl(55, Bot, Top)
    ),
    1000
  ),
  1,
  0,
  AndType(
    AndType(
      FieldDecl(1, TypeProj(33, 44)),
      FieldDecl(2, TypeProj(22, 55))
    ),
    AndType(
      AndType(
      FieldDecl(3, TypeProj(33, 44)),
      FieldDecl(1, TypeProj(22, 55))
    ),
      AndType(
      FieldDecl(3, TypeProj(33, 44)),
      FieldDecl(1, TypeProj(22, 55))
    )
    )
  ),
  //AndType(
  //  AndType(FieldDecl(2, Top), FieldDecl(3, TypeProj(4, 5))),
  //  AndType(
  //    AndType(
  //      TypeDecl(9, Bot, Bot),
  //      AndType(
  //        FieldDecl(
  //          10,
  //          TypeDecl(
  //            11,
  //            Bot,
  //            AndType(
  //              FieldDecl(12, Top),
  //              FieldDecl(13, FieldDecl(14, Bot))
  //            )
  //          )
  //        ),
  //        TypeDecl(15, Bot, Bot)
  //      )
  //    ),
  //    AndType(
  //      TypeDecl(16, TypeProj(17, 18), TypeProj(17, 18)),
  //      TypeDecl(24, Top, Top)
  //    )
  //  )
  //),
  AndType(
    AndType(
      Que,
      Que
    ),
    AndType(
      Que,
      Que
    )
  )
  //AndType(
  //  FieldDecl(1, Que),
  //  FieldDecl(1, Que)
  //)
)

//( // TODO debug slowness of dnf
//  GlobalContext(
//    Map(
//      33 -> TypeDecl(44, Bot, Top),
//      22 -> TypeDecl(55, Bot, Top)
//    ),
//    1000
//  ),
//  1,
//  0,
//  AndType(
//    AndType(
//      FieldDecl(1, TypeProj(33, 44)),
//      FieldDecl(2, TypeProj(22, 55))
//    ),
//    AndType(
//      FieldDecl(1, TypeProj(22, 55)),
//      FieldDecl(1, TypeProj(22, 55))
//    )
//  ),
//  //AndType(
//  //  AndType(FieldDecl(2, Top), FieldDecl(3, TypeProj(4, 5))),
//  //  AndType(
//  //    AndType(
//  //      TypeDecl(9, Bot, Bot),
//  //      AndType(
//  //        FieldDecl(
//  //          10,
//  //          TypeDecl(
//  //            11,
//  //            Bot,
//  //            AndType(
//  //              FieldDecl(12, Top),
//  //              FieldDecl(13, FieldDecl(14, Bot))
//  //            )
//  //          )
//  //        ),
//  //        TypeDecl(15, Bot, Bot)
//  //      )
//  //    ),
//  //    AndType(
//  //      TypeDecl(16, TypeProj(17, 18), TypeProj(17, 18)),
//  //      TypeDecl(24, Top, Top)
//  //    )
//  //  )
//  //),
//  AndType(
//    Que,
//    Que
//  )
//  //AndType(
//  //  FieldDecl(1, Que),
//  //  FieldDecl(1, Que)
//  //)
//)

//(
//  GlobalContext(Map(13 -> TypeDecl(14, Bot, Top)), 15),
//  1,
//  0,
//  AndType(
//    AndType(
//      FieldDecl(2, Bot),
//      TypeDecl(
//        3,
//        Bot,
//        FieldDecl(
//          4,
//          FunType(
//            5,
//            AndType(
//              FieldDecl(6, Top),
//              FieldDecl(7, FunType(8, Top, FieldDecl(9, Bot)))
//            ),
//            Top
//          )
//        )
//      )
//    ),
//    AndType(
//      TypeDecl(10, Bot, Bot),
//      TypeDecl(
//        11,
//        FunType(12, Top, TypeProj(13, 14)),
//        FunType(12, Top, TypeProj(13, 14))
//      )
//    )
//  ),
//  AndType(AndType(Que, Que), AndType(TypeDecl(10, Bot, Bot), Que))
//)

//(
//  GlobalContext(Map(4 -> TypeDecl(5, Bot, Top)), 6),
//  1,
//  0,
//  FunType(2, Bot, TypeDecl(3, Bot, TypeProj(4, 5))),
//  //FunType(2, Que, TypeDecl(3, Bot, TypeProj(4, 5))),
//  Que
//)

//(
//  GlobalContext(Map(), 8),
//  1,
//  0,
//  AndType(
//    AndType(
//      FieldDecl(2, Bot),
//      FieldDecl(3, FieldDecl(4, RecType(5, FieldDecl(6, Bot))))
//    ),
//    FieldDecl(7, Bot)
//  ),
//  AndType(Que, Que)
//  //Que
//)

//(
//  GlobalContext(
//    Map(
//      8 -> TypeDecl(9, Bot, Bot),
//      6 -> TypeDecl(7, Bot, TypeProj(8, 9))
//    ),
//    15
//  ),
//  1,
//  0,
//  AndType(
//    FieldDecl(3, FunType(4, Bot, Top)),
//    AndType(
//      FieldDecl(5, TypeProj(6, 7)),
//      AndType(
//        AndType(FieldDecl(10, Top), FieldDecl(11, Top)),
//        AndType(FieldDecl(12, FieldDecl(13, Top)), FieldDecl(14, Top))
//      )
//    )
//  ),
//  //AndType(FieldDecl(3, Que), AndType(FieldDecl(5, Que), Que))
//  AndType(FieldDecl(3, Que), AndType(Que, Que))
//)

//(
//  GlobalContext(
//    Map(
//      7 -> TypeDecl(8, Bot, Bot),
//      5 -> TypeDecl(6, Bot, TypeProj(7, 8))
//    ),
//    14
//  ),
//  1,
//  0,
//  AndType(
//    AndType(
//      FieldDecl(3, Top),
//      AndType(
//        FieldDecl(3, Top),
//        AndType(
//          AndType(
//            FieldDecl(3, Top),
//            AndType(FieldDecl(4, TypeProj(5, 6)), FieldDecl(9, Bot))
//          ),
//          TypeDecl(10, Bot, Bot)
//        )
//      )
//    ),
//    FieldDecl(
//      11,
//      AndType(
//        TypeDecl(12, TypeProj(5, 6), TypeProj(5, 6)),
//        FieldDecl(13, TypeProj(5, 6))
//      )
//    )
//  ),
//  //AndType(
//  //  AndType(
//  //    FieldDecl(3, Top),
//  //    AndType(
//  //      FieldDecl(3, Top),
//  //      AndType(
//  //        AndType(
//  //          FieldDecl(3, Top),
//  //          AndType(FieldDecl(4, TypeProj(5, 6)), FieldDecl(9, Bot))
//  //        ),
//  //        TypeDecl(10, Bot, Bot)
//  //      )
//  //    )
//  //  ),
//  //  FieldDecl(
//  //    11,
//  //    AndType(
//  //      TypeDecl(12, TypeProj(5, 6), TypeProj(5, 6)),
//  //      FieldDecl(13, TypeProj(5, 6))
//  //    )
//  //  )
//  //)
//  AndType(
//    AndType(FieldDecl(3, Que), AndType(Que, AndType(Que, Que))),
//    Que
//  )
//)

//(
//  GlobalContext(
//    Map(
//      8 -> TypeDecl(9, Bot, Bot),
//      6 -> TypeDecl(7, Bot, TypeProj(8, 9))
//    ),
//    13
//  ),
//  1,
//  0,
//  AndType(
//    FieldDecl(3, Bot),
//    AndType(
//      AndType(FieldDecl(4, Top), FieldDecl(5, TypeProj(6, 7))),
//      FieldDecl(10, RecType(11, FieldDecl(12, Top)))
//    )
//  ),
//  AndType(FieldDecl(3, Que), AndType(AndType(Que, Que), Que))
//  //Que
//)

//(GlobalContext(Map(), 2), 1, 0, Bot, Que)

val scope = scope1 + (z -> a)

//val res1 = NoFuture.varLower(scope, r, z, p)
//pprint.pprintln(res1, height=4000000)


val (numQues, labeledPrototype) = prepMatch(scope, r, p)

val solveSet = (0 until numQues).map{TypeProj(r, _)}.toSet // TODO get rid of solveSet somehow?
val solveSetVariance = gatherVariance2(r, labeledPrototype, Contravariant)

val constraint = gatherConstraints(scope, solveSet, labeledPrototype, a, Contravariant, Set(), Set())


P.namedln("constraint", constraint)

val dnfStartTime = System.nanoTime()
val dnfConstraint = dnf(constraint)
val dnfEndTime = System.nanoTime()

val cnfStartTime = System.nanoTime()
val cnfConstraint = cnf(constraint)
val cnfEndTime = System.nanoTime()

//P.namedln("cnfConstraint", cnfConstraint)
//pprint.pprintln(dnfConstraint, height=4000000)

def shortenlistlist(listlist: Stream[Stream[Constraint]]) =
  listlist.map{_.map{
  //  case MultiAndConstraint(m) if m.size == 1 =>
  //    (m.toSeq)(0)._1
  //  case MultiAndConstraint(m) if m.size != 1 =>
  //    TypeProj(-1,0)
  //  case TrueConstraint =>
  //    TypeProj(-1,+1)
  //  case FalseConstraint =>
  //    TypeProj(-1,-1)
  //  case _ =>
  //    TypeProj(-4500,-1)
  case MultiAndConstraint(m) if m.size == 1 =>
    val (p, (s, l, u)) = (m.toSeq)(0)
    solveSetVariance(p) match {
      case Covariant =>
        p -> (+1, l, u)
      case Contravariant =>
        p -> (-1, l, u)
      case _ =>
        p -> (0, l, u)
    }
  }}

val dl = dnfLists(dnfConstraint)
//P.namedln("dl", shortenlistlist(dl))
//P.namedln("dl", dl)
P.namedln("dl.size", dl.size)

val cl = cnfLists(cnfConstraint)
//P.namedln("cl", shortenlistlist(cl))
//P.namedln("cl", cl)
P.namedln("cl.size", cl.size)
P.namedln("cl.cartesian.size", cl.map{_.size: BigInt}.product)

//val ta = MultiAndConstraint(Map(TypeProj(0,1) -> (Map(), Bot, Top, Covariant)))
//val tb = MultiAndConstraint(Map(TypeProj(0,2) -> (Map(), Bot, Top, Covariant)))
//val tc = MultiAndConstraint(Map(TypeProj(0,3) -> (Map(), Bot, Top, Covariant)))
//val td = MultiAndConstraint(Map(TypeProj(0,4) -> (Map(), Bot, Top, Covariant)))
//val tcnf = cnfLists(orConstraint(andConstraint(ta, tb), andConstraint(tc, td)))
//val tdnf = dnfLists(orConstraint(andConstraint(ta, tb), andConstraint(tc, td)))
//P.namedln("tcnf", shortenlistlist(tcnf))
//P.namedln("tdnf", shortenlistlist(tdnf))

println(s"DNF TIME = ${(dnfEndTime - dnfStartTime) * 1e-9}s")
println(s"CNF TIME = ${(cnfEndTime - cnfStartTime) * 1e-9}s")

def constraintSize(c: Constraint): Int = c match {
  case OrConstraint(a, b) => constraintSize(a) + constraintSize(b)
  case AndConstraint(a, b) => constraintSize(a) + constraintSize(b)
  case _ => 1
}
println(s"size(constraint) = ${constraintSize(constraint)}")
println(s"size(dnfConstraint) = ${constraintSize(dnfConstraint)}")
println(s"size(cnfConstraint) = ${constraintSize(cnfConstraint)}")


val startTime = System.nanoTime()
val res2 = solveConstraint(scope, solveSet, solveSetVariance, constraint, labeledPrototype, Contravariant)
val endTime = System.nanoTime()

pprint.pprintln(res2, height=4000000)
println(s"RES2 TIME = ${(endTime - startTime) * 1e-9}")

//val res3startTime = System.nanoTime()
//val res3 = solveCnf(scope, solveSet, cnfConstraint, labeledPrototype)
//val res3endTime = System.nanoTime()
//
//pprint.pprintln(res3, height=4000000)
//println(s"RES3 TIME = ${(res3endTime - res3startTime) * 1e-9}")



}

def raisedebug(): Unit = {
val (GlobalContext(scope1, nextSymbol), r, z, a, p): (GlobalContext, Symbol, Symbol, Type, Prototype) =

//(GlobalContext(Map(), 2), 1, 0, Bot, Top)

//(
//  GlobalContext(
//    Map(
//      6 -> TypeDecl(7, Top, Top),
//      4 -> TypeDecl(5, TypeProj(6, 7), TypeProj(6, 7)),
//      2 -> TypeDecl(3, TypeProj(4, 5), TypeProj(4, 5))
//    ),
//    8
//  ),
//  1,
//  0,
//  Bot,
//  TypeProj(2, 3)
//)

//( // TODO debug slowness of dnf
//  GlobalContext(
//    Map(
//      6 -> TypeDecl(7, Bot, FieldDecl(8, Bot)),
//      4 -> TypeDecl(5, TypeProj(6, 7), TypeProj(6, 7)),
//      17 -> TypeDecl(
//        18,
//        Bot,
//        FunType(
//          19,
//          Top,
//          FieldDecl(
//            20,
//            TypeDecl(21, Bot, RecType(22, FieldDecl(23, Bot)))
//          )
//        )
//      )
//    ),
//    25
//  ),
//  1,
//  0,
//  AndType(
//    AndType(FieldDecl(2, Top), FieldDecl(3, TypeProj(4, 5))),
//    AndType(
//      AndType(
//        TypeDecl(9, Bot, Bot),
//        AndType(
//          FieldDecl(
//            10,
//            TypeDecl(
//              11,
//              Bot,
//              AndType(
//                FieldDecl(12, Top),
//                FieldDecl(13, FieldDecl(14, Bot))
//              )
//            )
//          ),
//          TypeDecl(15, Bot, Bot)
//        )
//      ),
//      AndType(
//        TypeDecl(16, TypeProj(17, 18), TypeProj(17, 18)),
//        TypeDecl(24, Top, Top)
//      )
//    )
//  ),
//  //AndType(
//  //  AndType(FieldDecl(2, Top), FieldDecl(3, TypeProj(4, 5))),
//  //  AndType(
//  //    AndType(
//  //      TypeDecl(9, Bot, Bot),
//  //      AndType(
//  //        FieldDecl(
//  //          10,
//  //          TypeDecl(
//  //            11,
//  //            Bot,
//  //            AndType(
//  //              FieldDecl(12, Top),
//  //              FieldDecl(13, FieldDecl(14, Bot))
//  //            )
//  //          )
//  //        ),
//  //        TypeDecl(15, Bot, Bot)
//  //      )
//  //    ),
//  //    AndType(
//  //      TypeDecl(16, TypeProj(17, 18), TypeProj(17, 18)),
//  //      TypeDecl(24, Top, Top)
//  //    )
//  //  )
//  //),
//  AndType(
//    AndType(FieldDecl(2, Top), Que),
//    AndType(
//      AndType(TypeDecl(9, Bot, Bot), Que),
//      AndType(Que, TypeDecl(24, Top, Que))
//    )
//  )
//)

//( // TODO debug slowness of dnf
//  GlobalContext(
//    Map(
//      33 -> TypeDecl(44, Bot, Top)
//    ),
//    100
//  ),
//  1,
//  0,
//  AndType(
//    TypeProj(33, 44),
//    TypeProj(33, 44)
//  ),
//  //AndType(
//  //  AndType(FieldDecl(2, Top), FieldDecl(3, TypeProj(4, 5))),
//  //  AndType(
//  //    AndType(
//  //      TypeDecl(9, Bot, Bot),
//  //      AndType(
//  //        FieldDecl(
//  //          10,
//  //          TypeDecl(
//  //            11,
//  //            Bot,
//  //            AndType(
//  //              FieldDecl(12, Top),
//  //              FieldDecl(13, FieldDecl(14, Bot))
//  //            )
//  //          )
//  //        ),
//  //        TypeDecl(15, Bot, Bot)
//  //      )
//  //    ),
//  //    AndType(
//  //      TypeDecl(16, TypeProj(17, 18), TypeProj(17, 18)),
//  //      TypeDecl(24, Top, Top)
//  //    )
//  //  )
//  //),
//  AndType(
//    Que,
//    Que
//  )
//)

//(
//  GlobalContext(Map(4 -> TypeDecl(5, Bot, FieldDecl(6, Top))), 8),
//  1,
//  0,
//  FunType(
//    2,
//    FunType(3, Top, Top),
//    Top
//  ),
//  FunType(2, FunType(3, Que, Top), Top)
//  //FunType(2, Que, Top)
//)

(
  GlobalContext(
    Map(4 -> TypeDecl(5, Top, Top), 9 -> TypeDecl(10, Top, Top)),
    13
  ),
  1,
  0,
  AndType(
    AndType(
      TypeDecl(3, Bot, TypeProj(4, 5)),
      AndType(
        TypeDecl(3, Bot, TypeProj(4, 5)),
        AndType(
          TypeDecl(
            6,
            Bot,
            FunType(7, Top, FieldDecl(8, TypeProj(9, 10)))
          ),
          FieldDecl(11, TypeProj(0, 3))
        )
      )
    ),
    FieldDecl(12, Bot)
  ),
RecType(2,
  AndType(
    AndType(
      TypeDecl(3, Bot, TypeProj(4, 5)),
      AndType(
        TypeDecl(3, Bot, TypeProj(4, 5)),
        AndType(
          TypeDecl(
            6,
            Bot,
            FunType(7, Top, FieldDecl(8, TypeProj(9, 10)))
          ),
          FieldDecl(11, TypeProj(2, 3))
        )
      )
    ),
    FieldDecl(12, Bot)
  )
)
)

val scope = scope1 + (z -> a)

val res1 = NoFuture.varRaise(scope, r, z, p)
pprint.pprintln(res1, height=4000000)

import NoFuture._

val (numQues, labeledPrototype) = prepMatch(scope, r, eliminateRecursiveTypes(p, z))

val solveSet = (0 until numQues).map{TypeProj(r, _)}.toSet // TODO get rid of solveSet somehow?

val constraint = gatherConstraints(scope, solveSet, eliminateRecursiveTypes(a, z), labeledPrototype, Covariant, Set(), Set())


P.namedln("constraint", constraint)

val dnfStartTime = System.nanoTime()
val dnfConstraint = dnf(constraint)
val dnfEndTime = System.nanoTime()

println(s"DNF TIME = ${(dnfEndTime - dnfStartTime) * 1e-9}s")

//pprint.pprintln(dnfConstraint, height=4000000)

def constraintSize(c: Constraint): Int = c match {
  case OrConstraint(a, b) => constraintSize(a) + constraintSize(b)
  //case AndConstraint(a, b) => constraintSize(a) + constraintSize(b)
  case _ => 1
}
println(s"size(constraint) = ${constraintSize(constraint)}")
println(s"size(dnfConstraint) = ${constraintSize(dnfConstraint)}")


val solveSetVariance = gatherVariance2(r, labeledPrototype, Covariant)

val startTime = System.nanoTime()
val res2 = solveConstraint(scope, solveSet, solveSetVariance, constraint, labeledPrototype, Covariant)
val endTime = System.nanoTime()

pprint.pprintln(res2, height=4000000)
println(s"TIME = ${(endTime - startTime) * 1e-9}s")
}

def debugsub(): Unit = {
  val (scope, a, b): (Scope, Type, Type) =
    (
      Map(4 -> TypeDecl(5, Bot, FieldDecl(6, Top))),
      FunType(2, Top, TypeDecl(7, TypeProj(4, 5), TypeProj(4, 5))),
      FunType(
        2,
        FunType(3, Top, TypeProj(4, 5)),
        TypeDecl(7, TypeProj(4, 5), TypeProj(4, 5))
      )
  )

  val res = isSubtypeOf(scope, a, b)
  println(s"res = $res")
}

}
