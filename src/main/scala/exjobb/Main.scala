package exjobb

import java.util.concurrent.atomic.AtomicBoolean
import Dol._
import cell._

/** For debugging. */
object Main {
  import NoFuture._

  def main(args: Array[String]): Unit = {
//lowerdebug()
raisedebug()
//debugsub()
  }

  sealed case class GlobalContext(scope: Scope, nextSymbol: Int)

def lowerdebug(): Unit ={
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
    FieldDecl(1, TypeProj(33, 44)),
    FieldDecl(1, TypeProj(22, 55))
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
    FieldDecl(1, Que),
    FieldDecl(1, Que)
  )
)

//(GlobalContext(Map(), 2), 1, 0, Bot, Que)

val scope = scope1 + (z -> a)

val res1 = NoFuture.varLower(scope, r, z, p)
pprint.pprintln(res1, height=4000000)


val (numQues, labeledPrototype) = prepMatch(scope, r, p)

val solveSet = (0 until numQues).map{TypeProj(r, _)}.toSet // TODO get rid of solveSet somehow?

val constraint = gatherConstraints(scope, solveSet, labeledPrototype, a, Covariant, Set(), Set())

pprint.pprintln(constraint, height=4000000)

val dnfStartTime = System.nanoTime()
val dnfConstraint = dnf(constraint)
val dnfEndTime = System.nanoTime()

//pprint.pprintln(dnfConstraint, height=4000000)

println(s"DNF TIME = ${(dnfEndTime - dnfStartTime) * 1e-9}s")

def constraintSize(c: Constraint): Int = c match {
  case OrConstraint(a, b) => constraintSize(a) + constraintSize(b)
  //case AndConstraint(a, b) => constraintSize(a) + constraintSize(b)
  case _ => 1
}
println(s"size(constraint) = ${constraintSize(constraint)}")
println(s"size(dnfConstraint) = ${constraintSize(dnfConstraint)}")

val startTime = System.nanoTime()
val res2 = solveConstraint(scope, solveSet, constraint, labeledPrototype)
val endTime = System.nanoTime()

pprint.pprintln(res2, height=4000000)
println(s"TIME = ${(endTime - startTime) * 1e-9}")



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

//(GlobalContext(Map(), 2), 1, 0, Bot, Que)

val scope = scope1 + (z -> a)

val res1 = NoFuture.varRaise(scope, r, z, p)
pprint.pprintln(res1, height=4000000)

import NoFuture._

val (numQues, labeledPrototype) = prepMatch(scope, r, eliminateRecursiveTypes(p, z))

val solveSet = (0 until numQues).map{TypeProj(r, _)}.toSet // TODO get rid of solveSet somehow?

val constraint = gatherConstraints(scope, solveSet, eliminateRecursiveTypes(a, z), labeledPrototype, Covariant, Set(), Set())


pprint.pprintln(constraint, height=4000000)

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


val startTime = System.nanoTime()
val res2 = solveConstraint(scope, solveSet, constraint, labeledPrototype)
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
