package exjobb

import java.util.concurrent.atomic.AtomicBoolean
import Dol._
import cell._

/** For debugging. */
object Main {
  import NoFuture._

  def main(args: Array[String]): Unit = {
//lowerdebug()
//raisedebug()
//debugsub()
//debugvarrename()
debuglub()
//debuglub3()
}

  sealed case class GlobalContext(scope: Scope, nextSymbol: Int)

  val pprintTerminalWidth = 78
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
//      AndType(
//      FieldDecl(3, TypeProj(33, 44)),
//      FieldDecl(1, TypeProj(22, 55))
//    ),
//      AndType(
//      FieldDecl(3, TypeProj(33, 44)),
//      FieldDecl(1, TypeProj(22, 55))
//    )
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
//    AndType(
//      Que,
//      Que
//    ),
//    AndType(
//      Que,
//      Que
//    )
//  )
//  //AndType(
//  //  FieldDecl(1, Que),
//  //  FieldDecl(1, Que)
//  //)
//)

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
//  GlobalContext(Map(5 -> TypeDecl(6, Bot, Bot)), 16),
//  1,
//  0,
//  AndType(
//    AndType(
//      AndType(
//        TypeDecl(2, Bot, Bot),
//        FieldDecl(3, FunType(4, TypeProj(5, 6), FieldDecl(7, Top)))
//      ),
//      AndType(
//        FieldDecl(8, Bot),
//        AndType(FieldDecl(9, TypeProj(5, 6)), FieldDecl(10, Bot))
//      )
//    ),
//    AndType(
//      AndType(FieldDecl(11, Bot), FieldDecl(12, Bot)),
//      FieldDecl(
//        13,
//        FunType(14, TypeDecl(15, TypeProj(5, 6), TypeProj(5, 6)), Top)
//      )
//    )
//  ),
//  AndType(
//    AndType(AndType(Que, Que), AndType(Que, AndType(Que, Que))),
//    AndType(Que, Que)
//  )
//  //Que
//)


//(
//  GlobalContext(
//    Map(
//      9 -> TypeDecl(
//        10,
//        FunType(11, Top, FieldDecl(12, Bot)),
//        FunType(11, Top, FieldDecl(12, Bot))
//      ),
//      29 -> TypeDecl(30, Bot, Bot)
//    ),
//    42
//  ),
//  1,
//  0,
//  AndType(
//    AndType(
//      TypeDecl(2, Bot, Top),
//      AndType(
//        AndType(
//          TypeDecl(
//            3,
//            FunType(
//              4,
//              FieldDecl(
//                5,
//                FunType(
//                  6,
//                  FunType(
//                    7,
//                    TypeDecl(8, Bot, TypeProj(9, 10)),
//                    FieldDecl(13, Bot)
//                  ),
//                  TypeProj(9, 10)
//                )
//              ),
//              Bot
//            ),
//            FunType(
//              4,
//              FieldDecl(
//                5,
//                FunType(
//                  6,
//                  FunType(
//                    7,
//                    TypeDecl(8, Bot, TypeProj(9, 10)),
//                    FieldDecl(13, Bot)
//                  ),
//                  TypeProj(9, 10)
//                )
//              ),
//              Bot
//            )
//          ),
//          FieldDecl(14, Bot)
//        ),
//        AndType(
//          AndType(
//            FieldDecl(
//              15,
//              RecType(
//                16,
//                AndType(
//                  AndType(
//                    FieldDecl(17, TypeProj(9, 10)),
//                    TypeDecl(
//                      18,
//                      Bot,
//                      AndType(
//                        FieldDecl(19, Top),
//                        AndType(
//                          TypeDecl(20, Top, Top),
//                          AndType(FieldDecl(21, Bot), FieldDecl(22, Top))
//                        )
//                      )
//                    )
//                  ),
//                  FieldDecl(23, TypeDecl(24, Bot, Bot))
//                )
//              )
//            ),
//            AndType(
//              TypeDecl(
//                25,
//                Bot,
//                TypeDecl(26, TypeProj(9, 10), TypeProj(9, 10))
//              ),
//              AndType(
//                FieldDecl(27, Top),
//                AndType(
//                  TypeDecl(28, TypeProj(29, 30), TypeProj(29, 30)),
//                  TypeDecl(
//                    31,
//                    Bot,
//                    FunType(
//                      32,
//                      RecType(
//                        33,
//                        AndType(
//                          FieldDecl(34, TypeProj(29, 30)),
//                          FieldDecl(35, Top)
//                        )
//                      ),
//                      Top
//                    )
//                  )
//                )
//              )
//            )
//          ),
//          FieldDecl(36, Top)
//        )
//      )
//    ),
//    FieldDecl(
//      37,
//      FunType(
//        38,
//        TypeProj(9, 10),
//        RecType(39, AndType(FieldDecl(40, Bot), FieldDecl(41, Bot)))
//      )
//    )
//  ),
//  //AndType(
//  //  AndType(
//  //    TypeDecl(2, Bot, Top),
//  //    AndType(
//  //      AndType(
//  //        TypeDecl(
//  //          3,
//  //          FunType(
//  //            4,
//  //            FieldDecl(
//  //              5,
//  //              FunType(
//  //                6,
//  //                FunType(
//  //                  7,
//  //                  TypeDecl(8, Bot, TypeProj(9, 10)),
//  //                  FieldDecl(13, Bot)
//  //                ),
//  //                TypeProj(9, 10)
//  //              )
//  //            ),
//  //            Bot
//  //          ),
//  //          FunType(
//  //            4,
//  //            FieldDecl(
//  //              5,
//  //              FunType(
//  //                6,
//  //                FunType(
//  //                  7,
//  //                  TypeDecl(8, Bot, TypeProj(9, 10)),
//  //                  FieldDecl(13, Bot)
//  //                ),
//  //                TypeProj(9, 10)
//  //              )
//  //            ),
//  //            Bot
//  //          )
//  //        ),
//  //        FieldDecl(14, Bot)
//  //      ),
//  //      AndType(
//  //        AndType(
//  //          FieldDecl(
//  //            15,
//  //            RecType(
//  //              16,
//  //              AndType(
//  //                AndType(
//  //                  FieldDecl(17, TypeProj(9, 10)),
//  //                  TypeDecl(
//  //                    18,
//  //                    Bot,
//  //                    AndType(
//  //                      FieldDecl(19, Top),
//  //                      AndType(
//  //                        TypeDecl(20, Top, Top),
//  //                        AndType(FieldDecl(21, Bot), FieldDecl(22, Top))
//  //                      )
//  //                    )
//  //                  )
//  //                ),
//  //                FieldDecl(23, TypeDecl(24, Bot, Bot))
//  //              )
//  //            )
//  //          ),
//  //          AndType(
//  //            TypeDecl(
//  //              25,
//  //              Bot,
//  //              TypeDecl(26, TypeProj(9, 10), TypeProj(9, 10))
//  //            ),
//  //            AndType(
//  //              FieldDecl(27, Top),
//  //              AndType(
//  //                TypeDecl(28, TypeProj(29, 30), TypeProj(29, 30)),
//  //                TypeDecl(
//  //                  31,
//  //                  Bot,
//  //                  FunType(
//  //                    32,
//  //                    RecType(
//  //                      33,
//  //                      AndType(
//  //                        FieldDecl(34, TypeProj(29, 30)),
//  //                        FieldDecl(35, Top)
//  //                      )
//  //                    ),
//  //                    Top
//  //                  )
//  //                )
//  //              )
//  //            )
//  //          )
//  //        ),
//  //        FieldDecl(36, Top)
//  //      )
//  //    )
//  //  ),
//  //  FieldDecl(
//  //    37,
//  //    FunType(
//  //      38,
//  //      TypeProj(9, 10),
//  //      RecType(39, AndType(FieldDecl(40, Bot), FieldDecl(41, Bot)))
//  //    )
//  //  )
//  //)
//  AndType(
//    AndType(
//      Que,
//      AndType(
//        AndType(
//          TypeDecl(
//            3,
//            Que,
//            FunType(
//              4,
//              FieldDecl(
//                5,
//                FunType(
//                  6,
//                  FunType(
//                    7,
//                    TypeDecl(8, Bot, TypeProj(9, 10)),
//                    FieldDecl(13, Bot)
//                  ),
//                  TypeProj(9, 10)
//                )
//              ),
//              Bot
//            )
//          ),
//          FieldDecl(14, Bot)
//        ),
//        AndType(
//          AndType(Que, AndType(Que, AndType(FieldDecl(27, Que), Que))),
//          Que
//        )
//      )
//    ),
//    FieldDecl(37, Que)
//  )
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


//(
//  GlobalContext(
//    Map(
//      2 -> TypeDecl(
//        3,
//        AndType(
//          TypeDecl(4, TypeDecl(5, Bot, Bot), TypeDecl(5, Bot, Bot)),
//          FieldDecl(6, Bot)
//        ),
//        AndType(
//          TypeDecl(4, TypeDecl(5, Bot, Bot), TypeDecl(5, Bot, Bot)),
//          FieldDecl(6, Bot)
//        )
//      )
//    ),
//    7
//  ),
//  1,
//  0,
//  TypeProj(2, 3),
//  AndType(
//    TypeDecl(4, TypeDecl(5, Bot, Bot), TypeDecl(5, Bot, Bot)),
//    FieldDecl(6, Bot)
//  )
//)

//(
//  GlobalContext(
//    Map(
//      5 -> TypeDecl(6, Bot, Bot),
//      2 -> TypeDecl(
//        3,
//        AndType(
//          FieldDecl(
//            4,
//            TypeProj(5, 6)
//          ),
//          FieldDecl(7, Bot)
//        ),
//        AndType(
//          FieldDecl(
//            4,
//            TypeProj(5, 6)
//          ),
//          FieldDecl(7, Bot)
//        )
//      )
//    ),
//    8
//  ),
//  1,
//  0,
//  //AndType(
//  //  FieldDecl(4, TypeProj(5, 6)),
//  //  FieldDecl(7, Bot)
//  //),
//  TypeProj(2, 3),
//  AndType(Que, FieldDecl(7, Bot))
//)


(
  GlobalContext(
    Map(
      7 -> TypeDecl(8, Bot, Top),
      23 -> TypeDecl(24, Bot, Top),
      19 -> TypeDecl(
        20,
        Bot,
        FieldDecl(
          21,
          FunType(
            22,
            TypeProj(23, 24),
            TypeProj(23, 24)
          )
        )
      )
    ),
    25
  ),
  1,
  0,
  FieldDecl(
    2,
    FieldDecl(
      3,
      FunType(
        4,
        RecType(
          5,
          AndType(
            TypeDecl(
              6,
              Bot,
              TypeProj(7, 8)
            ),
            AndType(
              AndType(
                FieldDecl(
                  9,
                  TypeProj(5, 6)
                ),
                FieldDecl(10, Bot)
              ),
              FieldDecl(
                11,
                AndType(
                  TypeDecl(
                    12,
                    AndType(Bot, Bot),
                    AndType(
                      FieldDecl(
                        13,
                        Top
                      ),
                      FieldDecl(
                        14,
                        FieldDecl(
                          15,
                          TypeDecl(
                            16,
                            TypeProj(
                              7,
                              8
                            ),
                            TypeProj(
                              7,
                              8
                            )
                          )
                        )
                      )
                    )
                  ),
                  TypeDecl(
                    17,
                    Top,
                    Top
                  )
                )
              )
            )
          )
        ),
        FieldDecl(
          18,
          TypeProj(19, 20)
        )
      )
    )
  ),
  FieldDecl(
    2,
    FieldDecl(
      3,
      FunType(
        4,
        RecType(
          5,
          AndType(
            TypeDecl(
              6,
              Bot,
              TypeProj(7, 8)
            ),
            AndType(
              AndType(
                FieldDecl(
                  9,
                  TypeProj(5, 6)
                ),
                FieldDecl(10, Bot)
              ),
              FieldDecl(
                11,
                AndType(
                  TypeDecl(
                    12,
                    AndType(Bot, Bot),
                    AndType(
                      FieldDecl(
                        13,
                        Top
                      ),
                      FieldDecl(
                        14,
                        FieldDecl(
                          15,
                          TypeDecl(
                            16,
                            TypeProj(
                              7,
                              8
                            ),
                            TypeProj(
                              7,
                              8
                            )
                          )
                        )
                      )
                    )
                  ),
                  TypeDecl(
                    17,
                    Top,
                    Top
                  )
                )
              )
            )
          )
        ),
        FieldDecl(
          18,
          TypeProj(19, 20)
        )
      )
    )
  )
)

val scope = scope1 + (z -> a)

val res1 = NoFuture.varLower(scope, r, z, p)
P.namedln("res1", res1)


val (numQues, labeledPrototype) = prepMatch(scope, r, simplify(eliminateRecursiveTypes(p, z)))

val solveSet = (0 until numQues).map{TypeProj(r, _)}.toSet // TODO get rid of solveSet somehow?
val solveSetVariance = gatherVariance(r, labeledPrototype, Contravariant)

val constraint = gatherConstraints(scope, solveSet, Some(z), labeledPrototype, a)


P.namedln("constraint", constraint)

val dnfStartTime = System.nanoTime()
//val dnfConstraint = dnf(constraint)
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

//val dl = dnfLists(dnfConstraint)
//P.namedln("dl", shortenlistlist(dl))
//P.namedln("dl", dl)
//P.namedln("dl.size", dl.size)

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
//println(s"size(dnfConstraint) = ${constraintSize(dnfConstraint)}")
//println(s"size(cnfConstraint) = ${constraintSize(cnfConstraint)}")


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

//(
//  GlobalContext(
//    Map(4 -> TypeDecl(5, Top, Top), 9 -> TypeDecl(10, Top, Top)),
//    13
//  ),
//  1,
//  0,
//  AndType(
//    AndType(
//      TypeDecl(3, Bot, TypeProj(4, 5)),
//      AndType(
//        TypeDecl(3, Bot, TypeProj(4, 5)),
//        AndType(
//          TypeDecl(
//            6,
//            Bot,
//            FunType(7, Top, FieldDecl(8, TypeProj(9, 10)))
//          ),
//          FieldDecl(11, TypeProj(0, 3))
//        )
//      )
//    ),
//    FieldDecl(12, Bot)
//  ),
//RecType(2,
//  AndType(
//    AndType(
//      TypeDecl(3, Bot, TypeProj(4, 5)),
//      AndType(
//        TypeDecl(3, Bot, TypeProj(4, 5)),
//        AndType(
//          TypeDecl(
//            6,
//            Bot,
//            FunType(7, Top, FieldDecl(8, TypeProj(9, 10)))
//          ),
//          FieldDecl(11, TypeProj(2, 3))
//        )
//      )
//    ),
//    FieldDecl(12, Bot)
//  )
//)
//)


//(
//  GlobalContext(
//    Map(
//      24 -> TypeDecl(
//        25,
//        Bot,
//        FieldDecl(
//          26,
//          TypeProj(13, 14)
//        )
//      ),
//      20 -> TypeDecl(21, Bot, TypeProj(17, 18)),
//      13 -> TypeDecl(14, Top, Top),
//      2 -> TypeDecl(
//        3,
//        RecType(
//          4,
//          AndType(
//            FieldDecl(
//              5,
//              RecType(
//                6,
//                AndType(
//                  FieldDecl(
//                    7,
//                    AndType(
//                      AndType(
//                        FieldDecl(
//                          8,
//                          RecType(
//                            9,
//                            FieldDecl(
//                              10,
//                              Top
//                            )
//                          )
//                        ),
//                        TypeDecl(
//                          11,
//                          Top,
//                          Top
//                        )
//                      ),
//                      FieldDecl(
//                        12,
//                        TypeProj(
//                          13,
//                          14
//                        )
//                      )
//                    )
//                  ),
//                  AndType(
//                    FieldDecl(
//                      15,
//                      FieldDecl(
//                        16,
//                        TypeProj(
//                          17,
//                          18
//                        )
//                      )
//                    ),
//                    FieldDecl(
//                      19,
//                      TypeProj(
//                        20,
//                        21
//                      )
//                    )
//                  )
//                )
//              )
//            ),
//            AndType(
//              FieldDecl(
//                22,
//                TypeProj(17, 18)
//              ),
//              FieldDecl(
//                23,
//                TypeProj(24, 25)
//              )
//            )
//          )
//        ),
//        RecType(
//          4,
//          AndType(
//            FieldDecl(
//              5,
//              RecType(
//                6,
//                AndType(
//                  FieldDecl(
//                    7,
//                    AndType(
//                      AndType(
//                        FieldDecl(
//                          8,
//                          RecType(
//                            9,
//                            FieldDecl(
//                              10,
//                              Top
//                            )
//                          )
//                        ),
//                        TypeDecl(
//                          11,
//                          Top,
//                          Top
//                        )
//                      ),
//                      FieldDecl(
//                        12,
//                        TypeProj(
//                          13,
//                          14
//                        )
//                      )
//                    )
//                  ),
//                  AndType(
//                    FieldDecl(
//                      15,
//                      FieldDecl(
//                        16,
//                        TypeProj(
//                          17,
//                          18
//                        )
//                      )
//                    ),
//                    FieldDecl(
//                      19,
//                      TypeProj(
//                        20,
//                        21
//                      )
//                    )
//                  )
//                )
//              )
//            ),
//            AndType(
//              FieldDecl(
//                22,
//                TypeProj(17, 18)
//              ),
//              FieldDecl(
//                23,
//                TypeProj(24, 25)
//              )
//            )
//          )
//        )
//      ),
//      17 -> TypeDecl(18, Bot, TypeProj(13, 14))
//    ),
//    27
//  ),
//  1,
//  0,
//  AndType(
//    FieldDecl(
//      5,
//      RecType(
//        6,
//        AndType(
//          FieldDecl(
//            7,
//            AndType(
//              AndType(
//                FieldDecl(
//                  8,
//                  RecType(
//                    9,
//                    FieldDecl(
//                      10,
//                      Top
//                    )
//                  )
//                ),
//                TypeDecl(
//                  11,
//                  Top,
//                  Top
//                )
//              ),
//              FieldDecl(
//                12,
//                TypeProj(13, 14)
//              )
//            )
//          ),
//          AndType(
//            FieldDecl(
//              15,
//              FieldDecl(
//                16,
//                TypeProj(17, 18)
//              )
//            ),
//            FieldDecl(
//              19,
//              TypeProj(20, 21)
//            )
//          )
//        )
//      )
//    ),
//    AndType(
//      FieldDecl(22, TypeProj(17, 18)),
//      FieldDecl(23, TypeProj(24, 25))
//    )
//  ),
//  TypeProj(2, 3)
//)

//(
//  GlobalContext(Map(), 13),
//  1,
//  0,
//  Bot,
//  AndType(
//    AndType(
//      TypeDecl(
//        3,
//        Bot,
//        FieldDecl(
//          4,
//          TypeDecl(5, Bot, Bot)
//        )
//      ),
//      FieldDecl(6, Top)
//    ),
//    FieldDecl(
//      7,
//      FieldDecl(
//        8,
//        FieldDecl(
//          9,
//          FieldDecl(
//            10,
//            RecType(
//              11,
//              TypeDecl(12, Bot, Bot)
//            )
//          )
//        )
//      )
//    )
//  )
//)

//(
//  GlobalContext(
//    Map(
//      6 -> TypeDecl(7, Bot, Bot),
//      12 -> TypeDecl(13, Top, Top),
//      2 -> TypeDecl(
//        3,
//        RecType(
//          4,
//          AndType(
//            FieldDecl(
//              5,
//              TypeProj(6, 7)
//            ),
//            FieldDecl(
//              8,
//              FieldDecl(
//                9,
//                FieldDecl(
//                  10,
//                  FunType(
//                    11,
//                    TypeProj(12, 13),
//                    Top
//                  )
//                )
//              )
//            )
//          )
//        ),
//        RecType(
//          4,
//          AndType(
//            FieldDecl(
//              5,
//              TypeProj(6, 7)
//            ),
//            FieldDecl(
//              8,
//              FieldDecl(
//                9,
//                FieldDecl(
//                  10,
//                  FunType(
//                    11,
//                    TypeProj(12, 13),
//                    Top
//                  )
//                )
//              )
//            )
//          )
//        )
//      )
//    ),
//    14
//  ),
//  1,
//  0,
//  RecType(
//    4,
//    AndType(
//      FieldDecl(5, TypeProj(6, 7)),
//      FieldDecl(
//        8,
//        FieldDecl(
//          9,
//          FieldDecl(
//            10,
//            FunType(
//              11,
//              TypeProj(12, 13),
//              Top
//            )
//          )
//        )
//      )
//    )
//  ),
//  //TypeProj(2, 3),
//  Que
//)


//(
//  GlobalContext(
//    Map(
//      10 -> TypeDecl(11, Bot, Bot),
//      2 -> TypeDecl(
//        3,
//        RecType(
//          4,
//          AndType(
//            AndType(
//              AndType(
//                TypeDecl(5, Top, Top),
//                TypeDecl(
//                  6,
//                  Bot,
//                  TypeDecl(
//                    7,
//                    Top,
//                    Top
//                  )
//                )
//              ),
//              TypeDecl(
//                8,
//                Bot,
//                FunType(
//                  9,
//                  TypeProj(4, 6),
//                  TypeProj(10, 11)
//                )
//              )
//            ),
//            FieldDecl(
//              12,
//              TypeProj(4, 8)
//            )
//          )
//        ),
//        RecType(
//          4,
//          AndType(
//            AndType(
//              AndType(
//                TypeDecl(5, Top, Top),
//                TypeDecl(
//                  6,
//                  Bot,
//                  TypeDecl(
//                    7,
//                    Top,
//                    Top
//                  )
//                )
//              ),
//              TypeDecl(
//                8,
//                Bot,
//                FunType(
//                  9,
//                  TypeProj(4, 6),
//                  TypeProj(10, 11)
//                )
//              )
//            ),
//            FieldDecl(
//              12,
//              TypeProj(4, 8)
//            )
//          )
//        )
//      )
//    ),
//    13
//  ),
//  1,
//  0,
//  RecType(
//    4,
//    AndType(
//      AndType(
//        AndType(
//          TypeDecl(5, Top, Top),
//          TypeDecl(
//            6,
//            Bot,
//            TypeDecl(7, Top, Top)
//          )
//        ),
//        TypeDecl(
//          8,
//          Bot,
//          FunType(
//            9,
//            TypeProj(4, 6),
//            TypeProj(10, 11)
//          )
//        )
//      ),
//      FieldDecl(12, TypeProj(4, 8))
//    )
//  ),
//  //TypeProj(2, 3),
//  TypeProj(2, 3)
//)


//(
//  GlobalContext(
//    Map(
//      8 -> TypeDecl(
//        9,
//        TypeDecl(
//          10,
//          Bot,
//          RecType(
//            11,
//            AndType(
//              FieldDecl(
//                12,
//                FieldDecl(13, Top)
//              ),
//              AndType(
//                FieldDecl(
//                  14,
//                  FunType(
//                    15,
//                    Top,
//                    FieldDecl(
//                      16,
//                      TypeDecl(
//                        17,
//                        Top,
//                        Top
//                      )
//                    )
//                  )
//                ),
//                FieldDecl(18, Top)
//              )
//            )
//          )
//        ),
//        TypeDecl(
//          10,
//          Bot,
//          RecType(
//            11,
//            AndType(
//              FieldDecl(
//                12,
//                FieldDecl(13, Top)
//              ),
//              AndType(
//                FieldDecl(
//                  14,
//                  FunType(
//                    15,
//                    Top,
//                    FieldDecl(
//                      16,
//                      TypeDecl(
//                        17,
//                        Top,
//                        Top
//                      )
//                    )
//                  )
//                ),
//                FieldDecl(18, Top)
//              )
//            )
//          )
//        )
//      )
//    ),
//    19
//  ),
//  1,
//  0,
//  AndType(
//    FieldDecl(
//      3,
//      TypeDecl(
//        4,
//        Bot,
//        FieldDecl(
//          5,
//          TypeDecl(6, Top, Top)
//        )
//      )
//    ),
//    Bot
//  ),
//  //AndType(
//  //  FieldDecl(
//  //    3,
//  //    TypeDecl(
//  //      4,
//  //      Bot,
//  //      FieldDecl(
//  //        5,
//  //        TypeDecl(6, Top, Top)
//  //      )
//  //    )
//  //  ),
//  //  FieldDecl(7, TypeProj(8, 9))
//  //),
//  AndType(
//    FieldDecl(
//      3,
//      TypeDecl(4, Bot, Que)
//    ),
//    FieldDecl(7, TypeProj(8, 9))
//  )
//)


(
  GlobalContext(
    Map(6 -> TypeDecl(7, Bot, Top)),
    16
  ),
  1,
  0,
  Bot,
  FieldDecl(
    2,
    RecType(
      3,
      AndType(
        TypeDecl(
          4,
          AndType(Bot, Bot),
          AndType(
            TypeDecl(
              5,
              TypeProj(6, 7),
              TypeProj(6, 7)
            ),
            AndType(
              FieldDecl(8, Top),
              FieldDecl(
                9,
                AndType(
                  FieldDecl(10, Top),
                  TypeDecl(
                    11,
                    Bot,
                    Bot
                  )
                )
              )
            )
          )
        ),
        AndType(
          FieldDecl(
            12,
            FieldDecl(
              13,
              FunType(
                14,
                TypeProj(3, 4),
                Top
              )
            )
          ),
          FieldDecl(15, Top)
        )
      )
    )
  )
)

val scope = scope1 + (z -> eliminateRecursiveTypes(a, z))

val res1 = NoFuture.varRaise(scope, r, z, p)
P.namedln("res1", res1)

import NoFuture._

val (numQues, labeledPrototype) = prepMatch(scope, r, simplify(eliminateRecursiveTypes(p, z)))

val solveSet = (0 until numQues).map{TypeProj(r, _)}.toSet // TODO get rid of solveSet somehow?

val constraint = gatherConstraints(scope, solveSet, Some(z), eliminateRecursiveTypes(a, z), labeledPrototype)


//P.namedln("constraint", constraint)

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


val solveSetVariance = gatherVariance(r, labeledPrototype, Covariant)

val startTime = System.nanoTime()
val res2 = solveConstraint(scope, solveSet, solveSetVariance, constraint, labeledPrototype, Covariant)
val endTime = System.nanoTime()

P.namedln("res2", res2)
println(s"TIME = ${(endTime - startTime) * 1e-9}s")

val b = p
P.namedln("raise(z:res2, b) == b", varEqualTypes(scope + (z -> res2.get), z, b))


}

def debugsub(): Unit = {
val (GlobalContext(scope, nextSymbol), z, a, b): (GlobalContext, Symbol, Type, Type) =


//(
//    Map(4 -> TypeDecl(5, Bot, FieldDecl(6, Top))),
//    FunType(2, Top, TypeDecl(7, TypeProj(4, 5), TypeProj(4, 5))),
//    FunType(
//      2,
//      FunType(3, Top, TypeProj(4, 5)),
//      TypeDecl(7, TypeProj(4, 5), TypeProj(4, 5))
//    )
//)

//(
//  GlobalContext(
//    Map(
//      6 -> TypeDecl(
//        7,
//        Bot,
//        TypeDecl(8, Bot, Bot)
//      ),
//      1 -> TypeDecl(
//        2,
//        RecType(
//          3,
//          AndType(
//            TypeDecl(4, Bot, Top),
//            FieldDecl(
//              5,
//              TypeProj(6, 7)
//            )
//          )
//        ),
//        RecType(
//          3,
//          AndType(
//            TypeDecl(4, Bot, Top),
//            FieldDecl(
//              5,
//              TypeProj(6, 7)
//            )
//          )
//        )
//      )
//    ),
//    9
//  ),
//  //0,
//  RecType(2,
//    AndType(
//      TypeDecl(4, Bot, Top),
//      FieldDecl(5, TypeProj(6, 7))
//    )
//  ),
//  //RecType(
//  //  3,
//  //  AndType(
//  //    TypeDecl(4, Bot, Top),
//  //    FieldDecl(5, TypeProj(6, 7))
//  //  )
//  //),
//  TypeProj(1, 2)
//)

//(
//  GlobalContext(
//    Map(
//      6 -> TypeDecl(7, Bot, Bot),
//      12 -> TypeDecl(13, Top, Top),
//      2 -> TypeDecl(
//        3,
//        RecType(
//          4,
//          AndType(
//            FieldDecl(
//              5,
//              TypeProj(6, 7)
//            ),
//            FieldDecl(
//              8,
//              FieldDecl(
//                9,
//                FieldDecl(
//                  10,
//                  FunType(
//                    11,
//                    TypeProj(12, 13),
//                    Top
//                  )
//                )
//              )
//            )
//          )
//        ),
//        RecType(
//          4,
//          AndType(
//            FieldDecl(
//              5,
//              TypeProj(6, 7)
//            ),
//            FieldDecl(
//              8,
//              FieldDecl(
//                9,
//                FieldDecl(
//                  10,
//                  FunType(
//                    11,
//                    TypeProj(12, 13),
//                    Top
//                  )
//                )
//              )
//            )
//          )
//        )
//      )
//    ),
//    14
//  ),
//  //1,
//  0,
//  RecType(
//    4,
//    AndType(
//      FieldDecl(5, TypeProj(6, 7)),
//      FieldDecl(
//        8,
//        FieldDecl(
//          9,
//          FieldDecl(
//            10,
//            FunType(
//              11,
//              TypeProj(12, 13),
//              Top
//            )
//          )
//        )
//      )
//    )
//  ),
//  TypeProj(2, 3)
//)


//(
//  GlobalContext(
//    Map(
//      9 -> TypeDecl(10, Bot, Top),
//      3 -> TypeDecl(
//        4,
//        RecType(
//          5,
//          AndType(
//            AndType(
//              TypeDecl(
//                6,
//                Bot,
//                AndType(
//                  TypeDecl(
//                    7,
//                    Bot,
//                    Bot
//                  ),
//                  AndType(
//                    FieldDecl(
//                      8,
//                      TypeProj(9, 10)
//                    ),
//                    AndType(
//                      FieldDecl(
//                        11,
//                        Top
//                      ),
//                      TypeDecl(
//                        12,
//                        Bot,
//                        Bot
//                      )
//                    )
//                  )
//                )
//              ),
//              TypeDecl(
//                13,
//                Bot,
//                FunType(
//                  14,
//                  TypeProj(9, 10),
//                  Bot
//                )
//              )
//            ),
//            FieldDecl(
//              15,
//              FieldDecl(
//                16,
//                TypeProj(5, 13)
//              )
//            )
//          )
//        ),
//        RecType(
//          5,
//          AndType(
//            AndType(
//              TypeDecl(
//                6,
//                Bot,
//                AndType(
//                  TypeDecl(
//                    7,
//                    Bot,
//                    Bot
//                  ),
//                  AndType(
//                    FieldDecl(
//                      8,
//                      TypeProj(9, 10)
//                    ),
//                    AndType(
//                      FieldDecl(
//                        11,
//                        Top
//                      ),
//                      TypeDecl(
//                        12,
//                        Bot,
//                        Bot
//                      )
//                    )
//                  )
//                )
//              ),
//              TypeDecl(
//                13,
//                Bot,
//                FunType(
//                  14,
//                  TypeProj(9, 10),
//                  Bot
//                )
//              )
//            ),
//            FieldDecl(
//              15,
//              FieldDecl(
//                16,
//                TypeProj(5, 13)
//              )
//            )
//          )
//        )
//      ),
//      1 -> TypeDecl(
//        2,
//        RecType(
//          5,
//          AndType(
//            AndType(
//              TypeDecl(
//                6,
//                Bot,
//                AndType(
//                  TypeDecl(
//                    7,
//                    Bot,
//                    Bot
//                  ),
//                  AndType(
//                    FieldDecl(
//                      8,
//                      TypeProj(9, 10)
//                    ),
//                    AndType(
//                      FieldDecl(
//                        11,
//                        Top
//                      ),
//                      TypeDecl(
//                        12,
//                        Bot,
//                        Bot
//                      )
//                    )
//                  )
//                )
//              ),
//              TypeDecl(
//                13,
//                Bot,
//                FunType(
//                  14,
//                  TypeProj(9, 10),
//                  Bot
//                )
//              )
//            ),
//            FieldDecl(
//              15,
//              FieldDecl(
//                16,
//                TypeProj(5, 13)
//              )
//            )
//          )
//        ),
//        TypeProj(3, 4)
//      )
//    ),
//    17
//  ),
//  0,
//  RecType(
//    5,
//    AndType(
//      AndType(
//        TypeDecl(
//          6,
//          Bot,
//          AndType(
//            TypeDecl(7, Bot, Bot),
//            AndType(
//              FieldDecl(
//                8,
//                TypeProj(9, 10)
//              ),
//              AndType(
//                FieldDecl(11, Top),
//                TypeDecl(
//                  12,
//                  Bot,
//                  Bot
//                )
//              )
//            )
//          )
//        ),
//        TypeDecl(
//          13,
//          Bot,
//          FunType(
//            14,
//            TypeProj(9, 10),
//            Bot
//          )
//        )
//      ),
//      FieldDecl(
//        15,
//        FieldDecl(
//          16,
//          TypeProj(5, 13)
//        )
//      )
//    )
//  ),
//  //RecType(
//  //  5,
//  //  AndType(
//  //    AndType(
//  //      TypeDecl(
//  //        6,
//  //        Bot,
//  //        AndType(
//  //          TypeDecl(7, Bot, Bot),
//  //          AndType(
//  //            FieldDecl(
//  //              8,
//  //              TypeProj(9, 10)
//  //            ),
//  //            AndType(
//  //              FieldDecl(11, Top),
//  //              TypeDecl(
//  //                12,
//  //                Bot,
//  //                Bot
//  //              )
//  //            )
//  //          )
//  //        )
//  //      ),
//  //      TypeDecl(
//  //        13,
//  //        Bot,
//  //        FunType(
//  //          14,
//  //          TypeProj(9, 10),
//  //          Bot
//  //        )
//  //      )
//  //    ),
//  //    FieldDecl(
//  //      15,
//  //      FieldDecl(
//  //        16,
//  //        TypeProj(5, 13)
//  //      )
//  //    )
//  //  )
//  //),
//  TypeProj(1, 2)
//)

//(
//GlobalContext(
//    Map(
//      9 -> TypeDecl(10, Bot, Top),
//      3 -> TypeDecl(
//        4,
//        RecType(
//          5,
//          AndType(
//            AndType(
//              TypeDecl(
//                6,
//                Bot,
//                AndType(
//                  TypeDecl(
//                    7,
//                    Bot,
//                    Bot
//                  ),
//                  AndType(
//                    FieldDecl(
//                      8,
//                      TypeProj(9, 10)
//                    ),
//                    AndType(
//                      FieldDecl(
//                        11,
//                        Top
//                      ),
//                      TypeDecl(
//                        12,
//                        Bot,
//                        Bot
//                      )
//                    )
//                  )
//                )
//              ),
//              TypeDecl(
//                13,
//                Bot,
//                FunType(
//                  14,
//                  TypeProj(9, 10),
//                  Bot
//                )
//              )
//            ),
//            FieldDecl(
//              15,
//              FieldDecl(
//                16,
//                TypeProj(5, 13)
//              )
//            )
//          )
//        ),
//        RecType(
//          5,
//          AndType(
//            AndType(
//              TypeDecl(
//                6,
//                Bot,
//                AndType(
//                  TypeDecl(
//                    7,
//                    Bot,
//                    Bot
//                  ),
//                  AndType(
//                    FieldDecl(
//                      8,
//                      TypeProj(9, 10)
//                    ),
//                    AndType(
//                      FieldDecl(
//                        11,
//                        Top
//                      ),
//                      TypeDecl(
//                        12,
//                        Bot,
//                        Bot
//                      )
//                    )
//                  )
//                )
//              ),
//              TypeDecl(
//                13,
//                Bot,
//                FunType(
//                  14,
//                  TypeProj(9, 10),
//                  Bot
//                )
//              )
//            ),
//            FieldDecl(
//              15,
//              FieldDecl(
//                16,
//                TypeProj(5, 13)
//              )
//            )
//          )
//        )
//      ),
//      1 -> TypeDecl(
//        2,
//        RecType(
//          5,
//          AndType(
//            AndType(
//              TypeDecl(
//                6,
//                Bot,
//                AndType(
//                  TypeDecl(
//                    7,
//                    Bot,
//                    Bot
//                  ),
//                  AndType(
//                    FieldDecl(
//                      8,
//                      TypeProj(9, 10)
//                    ),
//                    AndType(
//                      FieldDecl(
//                        11,
//                        Top
//                      ),
//                      TypeDecl(
//                        12,
//                        Bot,
//                        Bot
//                      )
//                    )
//                  )
//                )
//              ),
//              TypeDecl(
//                13,
//                Bot,
//                FunType(
//                  14,
//                  TypeProj(9, 10),
//                  Bot
//                )
//              )
//            ),
//            FieldDecl(
//              15,
//              FieldDecl(
//                16,
//                TypeProj(5, 13)
//              )
//            )
//          )
//        ),
//        TypeProj(3, 4)
//      )
//    ),
//    17
//  ),
//0,
//  AndType(FieldDecl(7, TypeProj(8, 9)), FieldDecl(3, TypeDecl(4, Bot, Bot))),
//  AndType(
//    FieldDecl(7, TypeProj(8, 9)),
//    FieldDecl(3, TypeDecl(4, Bot, FieldDecl(5, TypeDecl(6, Top, Top))))
//  )
//)

//(
//  GlobalContext(
//    Map(),
//    11
//  ),
//  0,
//  Bot,
//  TypeDecl(
//    8,
//    Top,
//    TypeProj(0, 5)
//  )
//)


//(
//  GlobalContext(
//    Map(),
//    11
//  ),
//  0,
//  FieldDecl(
//    2,
//    RecType(
//      3,
//      AndType(
//        TypeDecl(
//          4,
//          AndType(Bot, Bot),
//          AndType(
//            TypeDecl(5, TypeProj(6, 7), TypeProj(6, 7)),
//            AndType(
//              FieldDecl(8, Top),
//              FieldDecl(
//                9,
//                AndType(FieldDecl(10, Top), TypeDecl(11, Bot, Bot))
//              )
//            )
//          )
//        ),
//        AndType(
//          FieldDecl(12, FieldDecl(13, FunType(14, TypeProj(3, 4), Top))),
//          FieldDecl(15, Top)
//        )
//      )
//    )
//  ),
//  FieldDecl(
//    2,
//    RecType(
//      3,
//      AndType(
//        TypeDecl(
//          4,
//          Bot,
//          AndType(
//            TypeDecl(5, TypeProj(6, 7), TypeProj(6, 7)),
//            AndType(
//              FieldDecl(8, Top),
//              FieldDecl(
//                9,
//                AndType(FieldDecl(10, Top), TypeDecl(11, Bot, Bot))
//              )
//            )
//          )
//        ),
//        AndType(
//          FieldDecl(12, FieldDecl(13, FunType(14, TypeProj(3, 4), Top))),
//          FieldDecl(15, Top)
//        )
//      )
//    )
//  )
//)

//(
//  GlobalContext(
//    Map(),
//    11
//  ),
//  0,
//  FieldDecl(
//    2,
//    RecType(
//      3,
//      AndType(
//        TypeDecl(
//          4,
//          AndType(Bot, Bot),
//          AndType(
//            TypeDecl(5, TypeProj(6, 7), TypeProj(6, 7)),
//            AndType(
//              FieldDecl(8, Top),
//              FieldDecl(
//                9,
//                AndType(FieldDecl(10, Top), TypeDecl(11, Bot, Bot))
//              )
//            )
//          )
//        ),
//        AndType(
//          FieldDecl(12, FieldDecl(13, FunType(14, TypeProj(3, 4), Top))),
//          FieldDecl(15, Top)
//        )
//      )
//    )
//  ),
//  FieldDecl(
//    2,
//    RecType(
//      3,
//      AndType(
//        TypeDecl(
//          4,
//          Bot,
//          AndType(
//            TypeDecl(5, TypeProj(6, 7), TypeProj(6, 7)),
//            AndType(
//              FieldDecl(8, Top),
//              FieldDecl(
//                9,
//                AndType(FieldDecl(10, Top), TypeDecl(11, Bot, Bot))
//              )
//            )
//          )
//        ),
//        AndType(
//          FieldDecl(12, FieldDecl(13, FunType(14, TypeProj(3, 4), Top))),
//          FieldDecl(15, Top)
//        )
//      )
//    )
//  )
//)


(
  GlobalContext(
    Map(
      5 -> TypeDecl(6, Bot, Bot),
      7 -> TypeDecl(8, Top, Top)
    ),
    11
  ),
  0,
  FunType(
    1,
    TypeDecl(
      2,
      FunType(
        3,
        FunType(
          4,
          Bot,
          TypeProj(5, 6)
        ),
        Top
      ),
      FunType(
        3,
        FunType(
          4,
          Bot,
          TypeProj(5, 6)
        ),
        TypeProj(7, 8)
      )
    ),
    FunType(
      9,
      TypeProj(7, 8),
      TypeDecl(
        10,
        Bot,
        TypeProj(7, 8)
      )
    )
  ),
// AndType(
//  FunType(
//    1,
//    AndType(
//      TypeDecl(
//        2,
//        FunType(3, FunType(4, Bot, TypeProj(5, 6)), Top),
//        FunType(3, FunType(4, Bot, TypeProj(5, 6)), TypeProj(7, 8))
//      ),
//      TypeDecl(
//        2,
//        FunType(3, FunType(4, Bot, TypeProj(5, 6)), Top),
//        FunType(3, FunType(4, Bot, TypeProj(5, 6)), TypeProj(7, 8))
//      )
//    ),
//    FunType(
//      9,
//      AndType(TypeProj(7, 8), TypeProj(7, 8)),
//      TypeDecl(10, Bot, TypeProj(7, 8))
//    )
//  ),
//  TypeProj(7, 8)
//)
 //AndType(
 // FunType(
 //   1,
 //   TypeDecl(
 //     2,
 //     FunType(3, FunType(4, Bot, TypeProj(5, 6)), Top),
 //     FunType(3, FunType(4, Bot, TypeProj(5, 6)), TypeProj(7, 8))
 //   ),
 //   FunType(9, TypeProj(7, 8), TypeDecl(10, Bot, TypeProj(7, 8)))
 // ),
  TypeProj(7, 8)
//)
)

val res = varIsSubtypeOf(scope + (z -> a), z, b)
println(s"(z:a ==> z:b) = $res")
val res2 = varIsSubtypeOf(scope + (z -> b), z, a)
println(s"(z:a <== z:b) = $res2")

val res3 = varEqualTypes(scope + (z -> a), z, b)
println(s"varEqualTypes(z:a,b) = $res3")
}


def debugvarrename() = {
val (scope, x, y, typ): (Scope, Symbol, Symbol, Type) =
(
  Map(5 -> TypeDecl(6, Bot, Top), 9 -> TypeDecl(10, Bot, TypeProj(5, 6))),
  9,
  9,
  RecType(
    0,
    AndType(
      AndType(
        TypeDecl(
          1,
          AndType(Bot, Bot),
          AndType(FieldDecl(2, Top), FieldDecl(3, Bot))
        ),
        TypeDecl(4, Bot, TypeProj(5, 6))
      ),
      AndType(
        AndType(FieldDecl(7, Bot), FieldDecl(8, TypeProj(9, 10))),
        FieldDecl(
          11,
          AndType(
            AndType(
              TypeDecl(12, Bot, FunType(13, Bot, Bot)),
              FieldDecl(14, TypeProj(9, 10))
            ),
            FieldDecl(15, Top)
          )
        )
      )
    )
  )
)

      val res  = NoFuture.typeRenameVar(x, y, typ)
      val res2 = NoFuture.typeRenameVar(y, x, res)

      val freeBefore = NoFuture.allFreeVarsInType(typ)
      val freeMiddle = NoFuture.allFreeVarsInType(res)
      val freeAfter  = NoFuture.allFreeVarsInType(res2)

val test1 =       !(x != y && freeBefore(x))                   || (!freeMiddle(x) && freeMiddle(y))
val test2 =       !(x != y && freeMiddle(y))                   || (!freeAfter(y) && freeAfter(x))
val test3 =       !(x != y && freeBefore(x) && !freeBefore(y)) || (typ == res2)
val test4 =       !(x == y)                                    || (typ == res && res == res2)
P.namedln("typ", typ)
P.namedln("res", res)
P.namedln("res2", res2)
println(test1,test2,test3,test4)
}


def debuglub(): Unit =  {

val (GlobalContext(scope, nextSymbol), z, a, b): (GlobalContext, Symbol, Type, Type) =

//(
//  GlobalContext(
//  Map(
//    12 -> TypeDecl(13, Top, Top),
//    10 -> TypeDecl(11, TypeProj(12, 13), TypeProj(12, 13)),
//    7 -> TypeDecl(8, Bot, TypeDecl(9, Bot, TypeProj(10, 11))),
//    0 -> TypeDecl(
//      1,
//      RecType(
//        2,
//        AndType(
//          AndType(FieldDecl(3, FieldDecl(4, Top)), TypeDecl(5, Bot, Bot)),
//          FieldDecl(6, TypeProj(7, 8))
//        )
//      ),
//      RecType(
//        2,
//        AndType(
//          AndType(FieldDecl(3, FieldDecl(4, Top)), TypeDecl(5, Bot, Bot)),
//          FieldDecl(6, TypeProj(7, 8))
//        )
//      )
//    )
//  ), 14),
//  RecType(
//    2,
//    AndType(
//      AndType(FieldDecl(3, FieldDecl(4, Top)), TypeDecl(5, Bot, Bot)),
//      FieldDecl(6, TypeProj(7, 8))
//    )
//  ),
//  TypeProj(0, 1)
//)


//(
//  GlobalContext(
//    Map(
//      14 -> TypeDecl(15, Bot, Top),
//      9 -> TypeDecl(
//        10,
//        FieldDecl(
//          11,
//          FunType(
//            12,
//            Bot,
//            AndType(
//              TypeDecl(13, Bot, TypeProj(14, 15)),
//              FieldDecl(16, TypeDecl(17, Top, Top))
//            )
//          )
//        ),
//        FieldDecl(
//          11,
//          FunType(
//            12,
//            Bot,
//            AndType(
//              TypeDecl(13, Bot, TypeProj(14, 15)),
//              FieldDecl(16, TypeDecl(17, Top, Top))
//            )
//          )
//        )
//      ),
//      6 -> TypeDecl(7, FunType(8, Top, Bot), FunType(8, Bot, TypeProj(9, 10))),
//      0 -> TypeDecl(
//        1,
//        FunType(2, Top, Bot),
//        FunType(
//          2,
//          Top,
//          RecType(
//            3,
//            AndType(FieldDecl(4, Top), FieldDecl(5, TypeProj(6, 7)))
//          )
//        )
//      )
//    ),
//    18
//  ),
//  FunType(2, Top, Bot),
//  TypeProj(0, 1)
//)


//(
//  GlobalContext(
//    Map(
//      3 -> TypeDecl(4, Bot, Bot),
//      1 -> TypeDecl(
//        2,
//        TypeProj(3, 4),
//        TypeProj(3, 4)
//      ),
//      5 -> TypeDecl(6, Bot, Bot)
//    ),
//    7
//  ),
//  FunType(0, TypeProj(1, 2), Top),
//  TypeProj(5, 6)
//)

//(
//  GlobalContext(
//    Map(
//      10 -> TypeDecl(11, Bot, Top),
//      5 -> TypeDecl(
//        6,
//        Bot,
//        FunType(
//          7,
//          Top,
//          TypeDecl(
//            8,
//            Bot,
//            FunType(
//              9,
//              TypeProj(10, 11),
//              FunType(12, Bot, Bot)
//            )
//          )
//        )
//      ),
//      3 -> TypeDecl(4, Bot, TypeProj(5, 6)),
//      1 -> TypeDecl(2, Bot, TypeProj(3, 4))
//    ),
//    13
//  ),
//  FunType(0, Bot, TypeProj(1, 2)),
//  TypeProj(1, 2)
//)

//(
//  GlobalContext(
//    Map(
//      5 -> TypeDecl(6, Bot, Bot),
//      7 -> TypeDecl(8, Top, Top)
//    ),
//    11
//  ),
//  0,
//  FunType(
//    1,
//    TypeDecl(
//      2,
//      FunType(
//        3,
//        FunType(
//          4,
//          Bot,
//          TypeProj(5, 6)
//        ),
//        Top
//      ),
//      FunType(
//        3,
//        FunType(
//          4,
//          Bot,
//          TypeProj(5, 6)
//        ),
//        TypeProj(7, 8)
//      )
//    ),
//    FunType(
//      9,
//      TypeProj(7, 8),
//      TypeDecl(
//        10,
//        Bot,
//        TypeProj(7, 8)
//      )
//    )
//  ),
//  TypeProj(7, 8)
//)


(
  GlobalContext(
    Map(
      6 -> TypeDecl(7, Bot, Bot),
      8 -> TypeDecl(
        9,
        FunType(10, Top, Bot),
        FunType(
          10,
          Top,
          FunType(
            11,
            Top,
            TypeProj(6, 7)
          )
        )
      )
    ),
    12
  ),
  0,
  FunType(
    1,
    FieldDecl(
      2,
      RecType(
        3,
        AndType(
          TypeDecl(4, Top, Top),
          FieldDecl(
            5,
            TypeProj(6, 7)
          )
        )
      )
    ),
    TypeProj(8, 9)
  ),
  TypeProj(8, 9)
)

def lub(left: Type, right: Type) = leastCommonSupertype(scope, left, right)
def glb(left: Type, right: Type) = greatestCommonSubtype(scope, left, right)
val lub_ab = lub(a, b)
val lub_ba = lub(b, a)

val glb_ab = glb(a, b)

val lubRes = lub(a, glb_ab)
val glbRes = glb(a, lub_ab)

P.namedln("glb(a, b)", glb_ab)
P.namedln("lub(a, b))", lub_ab)
P.namedln("lub(a, glb(a, b))", lubRes)
P.namedln("glb(lub(a, b), a)", glbRes)

P.namedln("lub(a, b)", lub_ab)
P.namedln("lub(b, a)", lub_ba)
P.namedln("lub(a, b) == b", varEqualTypes(scope + (z -> lub_ab), z, b))
P.namedln("lub(a, b) == lub(b, a)", varEqualTypes(scope + (z -> lub_ab), z, lub_ba))
P.namedln("glb(a, lub(a, b)) == a", varEqualTypes(scope + (z -> glbRes), z, a))
P.namedln("lub(a, glb(a, b)) == a", varEqualTypes(scope + (z -> lubRes), z, a))
}

def debuglub3(): Unit =  {
val (GlobalContext(scope, nextSymbol), z, a, b, c): (GlobalContext, Symbol, Type, Type, Type) =

//(
//  GlobalContext(
//    Map(4 -> TypeDecl(5, Top, Top)),
//    8
//  ),
//  FunType(0, Bot, Top),
//  FunType(
//    1,
//    FunType(
//      2,
//      FunType(3, TypeProj(4, 5), Bot),
//      Top
//    ),
//    TypeProj(4, 5)
//  ),
//  FunType(
//    6,
//    Bot,
//    TypeDecl(7, Top, Top)
//  )
//)


//(
//  GlobalContext(
//    Map(
//      0 -> TypeDecl(
//        1,
//        Bot,
//        FunType(
//          2,
//          Bot,
//          FunType(
//            3,
//            RecType(
//              4,
//              FieldDecl(5, Top)
//            ),
//            TypeProj(6, 7)
//          )
//        )
//      ),
//      6 -> TypeDecl(
//        7,
//        FunType(
//          8,
//          Top,
//          RecType(
//            11,
//            AndType(
//              TypeDecl(
//                12,
//                Bot,
//                TypeProj(9, 10)
//              ),
//              AndType(
//                FieldDecl(
//                  13,
//                  RecType(
//                    14,
//                    FieldDecl(
//                      15,
//                      FunType(
//                        16,
//                        Top,
//                        TypeProj(
//                          9,
//                          10
//                        )
//                      )
//                    )
//                  )
//                ),
//                FieldDecl(17, Top)
//              )
//            )
//          )
//        ),
//        FunType(
//          8,
//          TypeProj(9, 10),
//          RecType(
//            11,
//            AndType(
//              TypeDecl(
//                12,
//                Bot,
//                TypeProj(9, 10)
//              ),
//              AndType(
//                FieldDecl(
//                  13,
//                  RecType(
//                    14,
//                    FieldDecl(
//                      15,
//                      FunType(
//                        16,
//                        Top,
//                        TypeProj(
//                          9,
//                          10
//                        )
//                      )
//                    )
//                  )
//                ),
//                FieldDecl(17, Top)
//              )
//            )
//          )
//        )
//      ),
//      21 -> TypeDecl(22, Bot, TypeProj(23, 24)),
//      9 -> TypeDecl(10, Bot, Bot),
//      31 -> TypeDecl(32, Bot, TypeProj(23, 24)),
//      23 -> TypeDecl(
//        24,
//        Bot,
//        AndType(
//          FieldDecl(25, Top),
//          TypeDecl(
//            26,
//            Bot,
//            TypeProj(6, 7)
//          )
//        )
//      )
//    ),
//    39
//  ),
//  TypeProj(0, 1),
//  FunType(
//    18,
//    FunType(
//      19,
//      FunType(
//        20,
//        Bot,
//        TypeProj(21, 22)
//      ),
//      Top
//    ),
//    TypeProj(0, 1)
//  ),
//  TypeDecl(
//    27,
//    Bot,
//    FunType(
//      28,
//      TypeProj(9, 10),
//      RecType(
//        29,
//        AndType(
//          AndType(
//            FieldDecl(
//              30,
//              TypeProj(31, 32)
//            ),
//            FieldDecl(33, Top)
//          ),
//          FieldDecl(
//            34,
//            RecType(
//              35,
//              FieldDecl(
//                36,
//                FieldDecl(
//                  37,
//                  TypeDecl(
//                    38,
//                    Top,
//                    Top
//                  )
//                )
//              )
//            )
//          )
//        )
//      )
//    )
//  )
//)


(
  GlobalContext(
    Map(
      10 -> TypeDecl(
        11,
        Bot,
        FunType(
          12,
          FieldDecl(13, Bot),
          Bot
        )
      ),
      5 -> TypeDecl(
        6,
        Bot,
        FunType(
          7,
          FieldDecl(
            8,
            FieldDecl(9, Bot)
          ),
          TypeProj(10, 11)
        )
      ),
      1 -> TypeDecl(
        2,
        FunType(3, Bot, Bot),
        FunType(
          3,
          Bot,
          FieldDecl(
            4,
            TypeProj(5, 6)
          )
        )
      )
    ),
    14
  ),
  4711,
  FunType(
    0,
    TypeProj(1, 2),
    TypeProj(5, 6)
  ),
  TypeProj(5, 6),
  TypeProj(10, 11)
)



def lub(left: Type, right: Type) = leastCommonSupertype(scope, left, right)

val bc   = lub(b, c)
P.namedln("lub(b, c)", bc)
val a_bc = lub(a, bc)
P.namedln("lub(a, lub(b, c))", a_bc)
val ab   = lub(a, b)
P.namedln("lub(a, b)", ab)
val ab_c = lub(ab, c)
P.namedln("lub(lub(a, b), c)", ab_c)

P.pprintln(varEqualTypes(scope + (z -> a_bc), z, ab_c))
}





}
