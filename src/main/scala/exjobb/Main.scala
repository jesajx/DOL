package exjobb

import java.util.concurrent.atomic.AtomicBoolean
import Dol._
import cell._

object Main {
  def main(args: Array[String]): Unit = {
lowerdebug()
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
//  AndType(
//    AndType(FieldDecl(2, Top), Que),
//    AndType(
//      AndType(TypeDecl(9, Bot, Bot), Que),
//      AndType(Que, TypeDecl(24, Top, Que))
//    )
//  )
//)

(GlobalContext(Map(), 2), 1, 0, Bot, Que)

val scope = scope1 + (z -> a)

val res1 = NoFuture.varLower(scope, r, z, p)
pprint.pprintln(res1, height=4000000)

import NoFuture._

val recFreePrototype = eliminateRecursiveTypes(p, z)
val (numQues, labeledPrototype) = varPrep(scope, r, z, p)
val constraint = varGather(scope, r, z, labeledPrototype, Contravariant)

pprint.pprintln(constraint, height=4000000)

val solveSet = (0 until numQues).map{TypeProj(r, _)}.toSet // TODO get rid of solveSet somehow?
val res2 = solveConstraint(scope, solveSet, constraint, labeledPrototype)

pprint.pprintln(res2, height=4000000)




}

def raisedebug(): Unit ={
val ((scope1, nextSymbol), z, r, a, b, p): ((Scope, Int), Symbol, Symbol, Type, Type, Prototype) =

//(
//  (
//    Map(
//      10 -> TypeDecl(11, TypeProj(12, 13), TypeProj(12, 13)),
//      14 -> TypeDecl(15, Bot, Top),
//      6 -> TypeDecl(7, Bot, TypeProj(8, 9)),
//      12 -> TypeDecl(13, Bot, TypeProj(14, 15)),
//      23 -> TypeDecl(24, Bot, Top),
//      8 -> TypeDecl(9, TypeProj(10, 11), TypeProj(10, 11)),
//      4 -> TypeDecl(5, TypeProj(6, 7), TypeProj(6, 7))
//    ): Scope,
//    31
//  ),
//  1,
//  0,
//  Bot,
//  AndType(
//    AndType(
//      AndType(
//        FieldDecl(2, Top),
//        TypeDecl(3, TypeProj(4, 5), TypeProj(4, 5))
//      ),
//      AndType(
//        FieldDecl(16, TypeProj(4, 5)),
//        FieldDecl(17, TypeProj(4, 5))
//      )
//    ),
//    FieldDecl(
//      18,
//      FieldDecl(
//        19,
//        RecType(
//          20,
//          AndType(
//            TypeDecl(21, Bot, TypeProj(10, 11)),
//            AndType(
//              AndType(
//                AndType(
//                  AndType(
//                    FieldDecl(22, TypeProj(23, 24)),
//                    FieldDecl(25, TypeProj(6, 7))
//                  ),
//                  FieldDecl(26, TypeProj(4, 5))
//                ),
//                FieldDecl(27, Top)
//              ),
//              AndType(
//                AndType(FieldDecl(28, Top), FieldDecl(29, Top)),
//                FieldDecl(30, Top)
//              )
//            )
//          )
//        )
//      )
//    )
//  ),
//  AndType(
//    AndType(
//      AndType(FieldDecl(2, Que), TypeDecl(3, TypeProj(4, 5), Que)),
//      AndType(Que, Que)
//    ),
//    Que
//  )
//)

//(
//  (Map(), 6),
//  1,
//  0,
//  Bot,
//  FieldDecl(
//    2,
//    TypeDecl(
//      3,
//      FieldDecl(4, FieldDecl(5, Top)),
//      FieldDecl(4, FieldDecl(5, Top))
//    )
//  ),
//  FieldDecl(2, TypeDecl(3, FieldDecl(4, Que), Que))
//)

(
  (Map(), 6),
  1,
  0,
  TypeDecl(
    2,
    AndType(FieldDecl(3, Top), FieldDecl(4, FunType(5, Bot, Top))),
    AndType(FieldDecl(3, Top), FieldDecl(4, FunType(5, Bot, Top)))
  ),
  TypeDecl(
    2,
    AndType(FieldDecl(3, Top), FieldDecl(4, FunType(5, Bot, Top))),
    AndType(FieldDecl(3, Top), FieldDecl(4, FunType(5, Bot, Top)))
  ),
  TypeDecl(
    2,
    AndType(Que, FieldDecl(4, FunType(5, Que, Que))),
    AndType(FieldDecl(3, Top), FieldDecl(4, FunType(5, Bot, Top)))
  )
)

val scope = scope1 + (z -> a)
//val res2 = NoFuture.varRaise(scope, r, z, p)


      val recFreePrototype = NoFuture.eliminateRecursiveTypes(p, z)
      val (numQues, labeledPrototype) = NoFuture.varPrep(scope, r, z, p)
      val constraint = NoFuture.varGather(scope, r, z, labeledPrototype, Covariant)
//pprint.pprintln(constraint, height=40000)
val dnfConstraint = NoFuture.dnf(constraint)
pprint.pprintln(dnfConstraint, height=40000)

      val solveSet = (0 until numQues).map{TypeProj(r, _)}.toSet // TODO get rid of solveSet somehow?
      val res = NoFuture.solveConstraint(scope, solveSet, constraint, labeledPrototype)
pprint.pprintln(res, height=40000)
//pprint.pprintln(res2)











  }
}
