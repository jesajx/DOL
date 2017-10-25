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
//debuglub()
//debuglub3()
//debuglubmin()
//debugproj()
debugelim()
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

//(
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

//(
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

//(
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


//(
//  GlobalContext(
//    Map(
//      7 -> TypeDecl(8, Bot, Top),
//      23 -> TypeDecl(24, Bot, Top),
//      19 -> TypeDecl(
//        20,
//        Bot,
//        FieldDecl(
//          21,
//          FunType(
//            22,
//            TypeProj(23, 24),
//            TypeProj(23, 24)
//          )
//        )
//      )
//    ),
//    25
//  ),
//  1,
//  0,
//  FieldDecl(
//    2,
//    FieldDecl(
//      3,
//      FunType(
//        4,
//        RecType(
//          5,
//          AndType(
//            TypeDecl(
//              6,
//              Bot,
//              TypeProj(7, 8)
//            ),
//            AndType(
//              AndType(
//                FieldDecl(
//                  9,
//                  TypeProj(5, 6)
//                ),
//                FieldDecl(10, Bot)
//              ),
//              FieldDecl(
//                11,
//                AndType(
//                  TypeDecl(
//                    12,
//                    AndType(Bot, Bot),
//                    AndType(
//                      FieldDecl(
//                        13,
//                        Top
//                      ),
//                      FieldDecl(
//                        14,
//                        FieldDecl(
//                          15,
//                          TypeDecl(
//                            16,
//                            TypeProj(
//                              7,
//                              8
//                            ),
//                            TypeProj(
//                              7,
//                              8
//                            )
//                          )
//                        )
//                      )
//                    )
//                  ),
//                  TypeDecl(
//                    17,
//                    Top,
//                    Top
//                  )
//                )
//              )
//            )
//          )
//        ),
//        FieldDecl(
//          18,
//          TypeProj(19, 20)
//        )
//      )
//    )
//  ),
//  FieldDecl(
//    2,
//    FieldDecl(
//      3,
//      FunType(
//        4,
//        RecType(
//          5,
//          AndType(
//            TypeDecl(
//              6,
//              Bot,
//              TypeProj(7, 8)
//            ),
//            AndType(
//              AndType(
//                FieldDecl(
//                  9,
//                  TypeProj(5, 6)
//                ),
//                FieldDecl(10, Bot)
//              ),
//              FieldDecl(
//                11,
//                AndType(
//                  TypeDecl(
//                    12,
//                    AndType(Bot, Bot),
//                    AndType(
//                      FieldDecl(
//                        13,
//                        Top
//                      ),
//                      FieldDecl(
//                        14,
//                        FieldDecl(
//                          15,
//                          TypeDecl(
//                            16,
//                            TypeProj(
//                              7,
//                              8
//                            ),
//                            TypeProj(
//                              7,
//                              8
//                            )
//                          )
//                        )
//                      )
//                    )
//                  ),
//                  TypeDecl(
//                    17,
//                    Top,
//                    Top
//                  )
//                )
//              )
//            )
//          )
//        ),
//        FieldDecl(
//          18,
//          TypeProj(19, 20)
//        )
//      )
//    )
//  )
//)


//(
//  GlobalContext(Map(6 -> RecType(
//    2,
//    AndType(
//      TypeDecl(
//        3,
//        Bot,
//        TypeDecl(4, Bot, Top)
//      ),
//      FieldDecl(5, TypeProj(2, 3))
//    )
//  )), 7),
//  1,
//  0,
//  //RecType(
//  //  2,
//  //  AndType(
//  //    TypeDecl(
//  //      3,
//  //      Bot,
//  //      TypeDecl(4, Bot, Top)
//  //    ),
//  //    FieldDecl(5, TypeProj(2, 3))
//  //  )
//  //),
//  Top,
//    AndType(
//      TypeDecl(
//        3,
//        Bot,
//        TypeDecl(4, Bot, Top)
//      ),
//      FieldDecl(5, TypeProj(6, 3))
//    )
//)


//( // TODO
//  GlobalContext(
//    Map(
//      4 -> TypeDecl(5, Bot, Bot),
//      12 -> TypeDecl(13, Bot, Bot),
//      14 -> TypeDecl(15, Bot, Bot),
//      16 -> TypeDecl(17, Bot, TypeProj(4, 5))
//    ),
//    26
//  ),
//  1,
//  0,
//  AndType(
//    AndType(
//      AndType(
//        TypeDecl(3, Bot, TypeProj(4, 5)),
//        FieldDecl(6, FunType(7, Bot, Top))
//      ),
//      AndType(
//        AndType(
//          TypeDecl(3, Bot, TypeProj(4, 5)),
//          FieldDecl(6, FunType(7, Bot, Top))
//        ),
//        AndType(
//          FieldDecl(8, TypeProj(0, 3)),
//          FieldDecl(
//            9,
//            FunType(
//              10,
//              FunType(11, TypeProj(12, 13), TypeProj(14, 15)),
//              TypeProj(16, 17)
//            )
//          )
//        )
//      )
//    ),
//    AndType(
//      AndType(
//        FieldDecl(
//          18,
//          RecType(
//            19,
//            AndType(
//              FieldDecl(
//                20,
//                FunType(21, TypeProj(0, 3), TypeProj(14, 15))
//              ),
//              FieldDecl(22, TypeDecl(23, Bot, Bot))
//            )
//          )
//        ),
//        FieldDecl(24, Bot)
//      ),
//      FieldDecl(25, Top)
//    )
//  ),
//  Top
//)

//( // TODO
//  GlobalContext(
//    Map(
//      3 -> TypeDecl(
//        4,
//        RecType(
//          5,
//          AndType(
//            AndType(FieldDecl(6, Top), FieldDecl(7, Bot)),
//            FieldDecl(
//              8,
//              RecType(9, FieldDecl(10, FieldDecl(11, Top)))
//            )
//          )
//        ),
//        RecType(
//          5,
//          AndType(
//            AndType(FieldDecl(6, Top), FieldDecl(7, Bot)),
//            FieldDecl(
//              8,
//              RecType(9, FieldDecl(10, FieldDecl(11, Top)))
//            )
//          )
//        )
//      ),
//      1 -> AndType(
//        AndType(FieldDecl(6, Top), FieldDecl(7, Bot)),
//        FieldDecl(8, RecType(9, FieldDecl(10, FieldDecl(11, Top))))
//      )
//    ),
//    12
//  ),
//  2,
//  0,
//  //AndType(
//  //  AndType(FieldDecl(6, Top), FieldDecl(7, Bot)),
//  //  FieldDecl(8, RecType(9, FieldDecl(10, FieldDecl(11, Top))))
//  //),
//  TypeProj(3, 4),
//  AndType(AndType(Que, Que), FieldDecl(8, Que))
//)

//(
//  GlobalContext(
//    Map(
//      3 -> TypeDecl(
//        4,
//        RecType(
//          5,
//          AndType(
//            AndType(FieldDecl(6, Top), FieldDecl(7, Bot)),
//            FieldDecl(
//              8,
//              RecType(9, FieldDecl(10, FieldDecl(11, Top)))
//            )
//          )
//        ),
//        RecType(
//          5,
//          AndType(
//            AndType(FieldDecl(6, Top), FieldDecl(7, Bot)),
//            FieldDecl(
//              8,
//              RecType(9, FieldDecl(10, FieldDecl(11, Top)))
//            )
//          )
//        )
//      ),
//      1 -> AndType(
//        AndType(FieldDecl(6, Top), FieldDecl(7, Bot)),
//        FieldDecl(8, RecType(9, FieldDecl(10, FieldDecl(11, Top))))
//      )
//    ),
//    12
//  ),
//  2,
//  0,
//  //AndType(
//  //  AndType(FieldDecl(6, Top), FieldDecl(7, Bot)),
//  //  FieldDecl(8, RecType(9, FieldDecl(10, FieldDecl(11, Top))))
//  //),
//  TypeProj(3, 4),
//  AndType(AndType(Que, Que), FieldDecl(8, Que))
//)

//(
//  GlobalContext(
//    Map(
//      10 -> TypeDecl(11, Top, Top),
//      24 -> TypeDecl(
//        25,
//        AndType(TypeProj(17, 18), FunType(26, Bot, Top)),
//        TypeProj(17, 18)
//      ),
//      17 -> TypeDecl(18, Bot, TypeProj(19, 20)),
//      27 -> TypeDecl(28, Bot, FieldDecl(29, Bot)),
//      19 -> TypeDecl(20, Bot, Top)
//    ),
//    34
//  ),
//  1,
//  0,
//  FunType(
//    2,
//    FieldDecl(
//      3,
//      RecType(
//        4,
//        AndType(
//          TypeDecl(
//            5,
//            Bot,
//            TypeDecl(
//              6,
//              Bot,
//              AndType(
//                TypeDecl(7, Bot, Top),
//                FieldDecl(8, FieldDecl(9, TypeProj(10, 11)))
//              )
//            )
//          ),
//          FieldDecl(12, Top)
//        )
//      )
//    ),
//    RecType(
//      13,
//      AndType(
//        TypeDecl(
//          14,
//          Bot,
//          RecType(15, FieldDecl(16, TypeProj(17, 18)))
//        ),
//        FieldDecl(
//          21,
//          FunType(
//            22,
//            FunType(23, TypeProj(24, 25), TypeProj(27, 28)),
//            FieldDecl(
//              30,
//              FunType(
//                31,
//                RecType(32, FieldDecl(33, Bot)),
//                TypeProj(13, 14)
//              )
//            )
//          )
//        )
//      )
//    )
//  ),
//  FunType(2, FieldDecl(3, Que), Que)
//)


//(
//  GlobalContext(
//    Map(9 -> TypeDecl(10, FieldDecl(11, Top), FieldDecl(11, Top))),
//    18
//  ),
//  1,
//  0,
//  FieldDecl(
//    2,
//    RecType(
//      3,
//      AndType(
//        AndType(
//          AndType(
//            TypeDecl(4, Bot, Bot),
//            FieldDecl(5, FieldDecl(6, FieldDecl(7, Top)))
//          ),
//          FieldDecl(8, TypeProj(9, 10))
//        ),
//        AndType(
//          AndType(
//            FieldDecl(12, TypeProj(3, 4)),
//            FieldDecl(13, FunType(14, FieldDecl(15, Bot), Bot))
//          ),
//          AndType(FieldDecl(16, TypeProj(3, 4)), FieldDecl(17, Top))
//        )
//      )
//    )
//  ),
//  FieldDecl(2, Que)
//)

//(
//  GlobalContext(
//    Map(
//      4 -> TypeDecl(5, Bot, Bot),
//      12 -> TypeDecl(13, Bot, Bot),
//      14 -> TypeDecl(15, Bot, Bot),
//      16 -> TypeDecl(17, Bot, TypeProj(4, 5))
//    ),
//    27
//  ),
//  1,
//  0,
//  Top,
//  RecType(26,
//  AndType(
//    AndType(
//      AndType(
//        TypeDecl(3, Bot, TypeProj(4, 5)),
//        FieldDecl(6, FunType(7, Bot, Top))
//      ),
//      AndType(
//        AndType(
//          TypeDecl(3, Bot, TypeProj(4, 5)),
//          FieldDecl(6, FunType(7, Bot, Top))
//        ),
//        AndType(
//          FieldDecl(8, TypeProj(26, 3)),
//          FieldDecl(
//            9,
//            FunType(
//              10,
//              FunType(11, TypeProj(12, 13), TypeProj(14, 15)),
//              TypeProj(16, 17)
//            )
//          )
//        )
//      )
//    ),
//    AndType(
//      AndType(
//        FieldDecl(
//          18,
//          RecType(
//            19,
//            AndType(
//              FieldDecl(
//                20,
//                FunType(21, TypeProj(26, 3), TypeProj(14, 15))
//              ),
//              FieldDecl(22, TypeDecl(23, Bot, Bot))
//            )
//          )
//        ),
//        FieldDecl(24, Bot)
//      ),
//      FieldDecl(25, Top)
//    )
//  )
//)
//)

//(
//  GlobalContext(
//    Map(
//      4 -> TypeDecl(5, Bot, Bot),
//      10 -> TypeDecl(
//        11,
//        FunType(
//          12,
//          TypeDecl(13, Bot, Bot),
//          FunType(14, TypeProj(4, 5), Top)
//        ),
//        Top
//      )
//    ),
//    18
//  ),
//  1,
//  0,
//  RecType(17,
//  AndType(
//    TypeDecl(3, TypeProj(4, 5), Top),
//    AndType(
//      FieldDecl(6, TypeProj(17, 3)),
//      AndType(
//        FieldDecl(7, TypeProj(4, 5)),
//        FieldDecl(
//          8,
//          FunType(
//            9,
//            TypeProj(10, 11),
//            FunType(15, Top, FieldDecl(16, Top))
//          )
//        )
//      )
//    )
//  )),
//  Que
//)

//(
//  GlobalContext(
//    Map(),
//    3
//  ),
//  1,
//  0,
//  RecType(2,
//    Top),
//  Top
//)

//(
//  GlobalContext(
//    Map(
//      5 -> TypeDecl(6, Bot, Top),
//      11 -> TypeDecl(12, Bot, Bot),
//      1 -> AndType(
//        AndType(
//          FunType(
//            3,
//            Top,
//            TypeDecl(4, TypeProj(5, 6), TypeProj(5, 6))
//          ),
//          FunType(
//            7,
//            TypeDecl(8, TypeProj(5, 6), Top),
//            TypeProj(5, 6)
//          )
//        ),
//        FunType(
//          9,
//          TypeProj(5, 6),
//          FunType(10, TypeProj(11, 12), FieldDecl(13, Top))
//        )
//      )
//    ),
//    14
//  ),
//  2,
//  0,
//  AndType(
//    AndType(
//      FunType(3, Top, TypeDecl(4, TypeProj(5, 6), TypeProj(5, 6))),
//      FunType(7, TypeDecl(8, TypeProj(5, 6), Top), TypeProj(5, 6))
//    ),
//    FunType(
//      9,
//      TypeProj(5, 6),
//      FunType(10, TypeProj(11, 12), FieldDecl(13, Top))
//    )
//  ),
//  //FunType(3, Top, TypeDecl(4, TypeProj(5, 6), TypeProj(5, 6))),
//  AndType(AndType(Que, FunType(7, Que, Que)), Que)
//)

//(
//  GlobalContext(Map(), 13),
//  1,
//  0,
//  RecType(
//    2,
//    AndType(
//      TypeDecl(3, Bot, Bot),
//      FieldDecl(
//        4,
//        RecType(
//          5,
//          AndType(
//            TypeDecl(6, AndType(Bot, Bot), TypeProj(2, 3)),
//            FieldDecl(
//              7,
//              FunType(
//                8,
//                TypeDecl(9, TypeProj(5, 6), TypeProj(5, 6)),
//                FunType(10, TypeProj(5, 6), Top)
//              )
//            )
//          )
//        )
//      )
//    )
//  ),
//  AndType(
//    RecType(
//      2,
//      AndType(
//        TypeDecl(3, Bot, Bot),
//        FieldDecl(
//          4,
//          RecType(
//            5,
//            AndType(
//              TypeDecl(6, AndType(Bot, Bot), TypeProj(2, 3)),
//              FieldDecl(
//                7,
//                FunType(
//                  8,
//                  TypeDecl(9, TypeProj(5, 6), TypeProj(5, 6)),
//                  FunType(10, TypeProj(5, 6), Top)
//                )
//              )
//            )
//          )
//        )
//      )
//    ),
//    AndType(FieldDecl(11, Top), FieldDecl(12, Top))
//  )
//)

//(
//  GlobalContext(Map(), 13),
//  1,
//  0,
//  RecType(
//    2,
//    AndType(
//      TypeDecl(3, Bot, Bot),
//      FieldDecl(
//        4,
//        RecType(
//          5,
//          AndType(
//            TypeDecl(6, AndType(Bot, Bot), TypeProj(2, 3)),
//            FieldDecl(
//              7,
//              FunType(
//                8,
//                TypeDecl(9, TypeProj(5, 6), TypeProj(5, 6)),
//                FunType(10, TypeProj(5, 6), Top)
//              )
//            )
//          )
//        )
//      )
//    )
//  ),
//  AndType(
//    RecType(
//      2,
//      AndType(
//        TypeDecl(3, Bot, Bot),
//        FieldDecl(
//          4,
//          RecType(
//            5,
//            AndType(
//              TypeDecl(6, AndType(Bot, Bot), TypeProj(2, 3)),
//              FieldDecl(
//                7,
//                FunType(
//                  8,
//                  TypeDecl(9, TypeProj(5, 6), TypeProj(5, 6)),
//                  FunType(10, TypeProj(5, 6), Top)
//                )
//              )
//            )
//          )
//        )
//      )
//    ),
//    AndType(FieldDecl(11, Top), FieldDecl(12, Top))
//  )
//)

//(
//  GlobalContext(
//    Map(
//      10 -> TypeDecl(11, Top, Top),
//      1 -> AndType(AndType(AndType(Bot, Top), Bot), TypeProj(10, 11))
//    ),
//    12
//  ),
//  2,
//  0,
//  //AndType(AndType(AndType(Bot, Top), Bot), TypeProj(10, 11)),
//  RecType(
//    3,
//    AndType(
//      TypeDecl(4, Bot, Bot),
//      FieldDecl(
//        5,
//        FunType(
//          6,
//          AndType(
//            TypeDecl(7, Bot, Bot),
//            FieldDecl(8, TypeProj(3, 4))
//          ),
//          FieldDecl(9, Bot)
//        )
//      )
//    )
//  ),
//  AndType(Que, TypeProj(10, 11))
//)

//( // TODO
//  GlobalContext(
//    Map(
//      14 -> TypeDecl(15, TypeProj(16, 17), TypeProj(16, 17)),
//      1 -> AndType(
//        AndType(
//          AndType(TypeProj(3, 4), FunType(39, Bot, Bot)),
//          TypeProj(16, 17)
//        ),
//        FunType(
//          40,
//          FunType(41, TypeProj(9, 10), FunType(42, Bot, Bot)),
//          TypeDecl(
//            43,
//            AndType(
//              AndType(Bot, TypeProj(16, 17)),
//              AndType(
//                TypeDecl(
//                  52,
//                  AndType(Bot, FunType(54, Bot, Bot)),
//                  FunType(53, Top, Bot)
//                ),
//                TypeDecl(55, TypeProj(9, 10), TypeProj(9, 10))
//              )
//            ),
//            TypeDecl(
//              44,
//              AndType(
//                Bot,
//                RecType(
//                  45,
//                  AndType(
//                    FieldDecl(46, Top),
//                    AndType(
//                      FieldDecl(47, Top),
//                      AndType(
//                        AndType(
//                          FieldDecl(48, Top),
//                          FieldDecl(49, TypeProj(7, 8))
//                        ),
//                        FieldDecl(50, FieldDecl(51, TypeProj(7, 8)))
//                      )
//                    )
//                  )
//                )
//              ),
//              TypeProj(19, 20)
//            )
//          )
//        )
//      ),
//      9 -> TypeDecl(
//        10,
//        AndType(
//          AndType(
//            Bot,
//            TypeDecl(
//              12,
//              FieldDecl(
//                13,
//                AndType(
//                  TypeProj(16, 17),
//                  FieldDecl(
//                    21,
//                    TypeDecl(
//                      22,
//                      AndType(Bot, Bot),
//                      FieldDecl(23, FunType(24, Top, Top))
//                    )
//                  )
//                )
//              ),
//              FieldDecl(13, TypeProj(14, 15))
//            )
//          ),
//          Top
//        ),
//        TypeDecl(11, Bot, Bot)
//      ),
//      7 -> TypeDecl(8, TypeProj(9, 10), TypeProj(9, 10)),
//      3 -> TypeDecl(
//        4,
//        AndType(
//          FunType(5, FieldDecl(6, TypeProj(7, 8)), Bot),
//          AndType(
//            TypeDecl(25, TypeProj(14, 15), TypeProj(14, 15)),
//            FieldDecl(
//              26,
//              TypeDecl(
//                27,
//                TypeDecl(
//                  28,
//                  Bot,
//                  TypeDecl(
//                    29,
//                    FieldDecl(18, TypeProj(19, 20)),
//                    TypeProj(16, 17)
//                  )
//                ),
//                TypeDecl(
//                  28,
//                  Bot,
//                  TypeDecl(
//                    29,
//                    FieldDecl(18, TypeProj(19, 20)),
//                    TypeProj(16, 17)
//                  )
//                )
//              )
//            )
//          )
//        ),
//        FunType(5, FieldDecl(6, TypeProj(7, 8)), Bot)
//      ),
//      16 -> TypeDecl(17, FieldDecl(18, TypeProj(19, 20)), Top),
//      19 -> TypeDecl(20, Top, Top)
//    ),
//    56
//  ),
//  2,
//  0,
//  //AndType(
//  //  AndType(
//  //    AndType(TypeProj(3, 4), FunType(39, Bot, Bot)),
//  //    TypeProj(16, 17)
//  //  ),
//  //  FunType(
//  //    40,
//  //    FunType(41, TypeProj(9, 10), FunType(42, Bot, Bot)),
//  //    TypeDecl(
//  //      43,
//  //      Bot,
//  //      TypeDecl(
//  //        44,
//  //        Bot,
//  //        TypeProj(19, 20)
//  //      )
//  //    )
//  //  )
//  //),
//  TypeProj(3, 4),
//  AndType(
//    Que,
//    FunType(
//      40,
//      Que,
//      TypeDecl(
//        43,
//        Bot,
//        TypeDecl(
//          44,
//          Bot,
//          Que
//        )
//      )
//    )
//  )
//)


//(
//  GlobalContext(
//    Map(2 -> TypeDecl(3, RecType(4, Bot), RecType(4, Bot))),
//    5
//  ),
//  1,
//  0,
//  TypeProj(2, 3),
//  RecType(4, Bot)
//)

//( // TODO Need SymbolUniverse in gatherConstraint for this to work.
//  GlobalContext(
//    Map(
//      1 -> RecType(
//        5,
//        RecType(
//          6,
//          RecType(7, FunType(8, TypeProj(9, 10), RecType(55, Top)))
//        )
//      ),
//      28 -> TypeDecl(
//        29,
//        AndType(
//          TypeProj(30, 31),
//          AndType(
//            FieldDecl(
//              32,
//              RecType(
//                33,
//                FunType(
//                  34,
//                  TypeProj(17, 18),
//                  RecType(35, TypeProj(36, 37))
//                )
//              )
//            ),
//            AndType(
//              AndType(
//                AndType(
//                  TypeDecl(46, TypeProj(44, 45), TypeProj(44, 45)),
//                  FieldDecl(47, Bot)
//                ),
//                AndType(
//                  TypeDecl(48, Top, TypeProj(41, 42)),
//                  TypeDecl(49, AndType(TypeProj(50, 51), Top), Top)
//                )
//              ),
//              TypeDecl(53, Bot, FieldDecl(54, Top))
//            )
//          )
//        ),
//        Bot
//      ),
//      9 -> TypeDecl(
//        10,
//        TypeProj(26, 27),
//        FunType(
//          11,
//          AndType(
//            AndType(
//              FieldDecl(12, Bot),
//              AndType(
//                FieldDecl(13, RecType(14, Bot)),
//                TypeDecl(
//                  15,
//                  AndType(
//                    AndType(
//                      TypeDecl(16, TypeProj(17, 18), Top),
//                      FieldDecl(19, Bot)
//                    ),
//                    AndType(
//                      FieldDecl(20, RecType(21, Top)),
//                      FieldDecl(22, Top)
//                    )
//                  ),
//                  AndType(
//                    AndType(
//                      TypeDecl(16, TypeProj(17, 18), Top),
//                      FieldDecl(19, Bot)
//                    ),
//                    AndType(
//                      FieldDecl(20, RecType(21, Top)),
//                      FieldDecl(22, Top)
//                    )
//                  )
//                )
//              )
//            ),
//            FieldDecl(23, RecType(24, TypeDecl(25, Bot, Bot)))
//          ),
//          Top
//        )
//      ),
//      41 -> TypeDecl(42, Top, Top),
//      17 -> TypeDecl(18, Bot, Bot),
//      44 -> TypeDecl(45, Bot, AndType(Bot, TypeProj(17, 18))),
//      3 -> TypeDecl(
//        4,
//        RecType(
//          5,
//          RecType(
//            6,
//            RecType(
//              7,
//              FunType(8, TypeProj(9, 10), RecType(55, Top))
//            )
//          )
//        ),
//        RecType(
//          5,
//          RecType(
//            6,
//            RecType(
//              7,
//              FunType(8, TypeProj(9, 10), RecType(55, Top))
//            )
//          )
//        )
//      ),
//      50 -> TypeDecl(51, FieldDecl(52, Bot), Top),
//      26 -> TypeDecl(27, TypeProj(28, 29), TypeProj(28, 29)),
//      36 -> TypeDecl(
//        37,
//        TypeProj(44, 45),
//        RecType(
//          38,
//          AndType(
//            TypeDecl(39, Bot, RecType(40, TypeProj(41, 42))),
//            FieldDecl(43, Top)
//          )
//        )
//      ),
//      30 -> TypeDecl(
//        31,
//        AndType(AndType(Bot, TypeProj(17, 18)), Bot),
//        AndType(Bot, Bot)
//      )
//    ),
//    56
//  ),
//  2,
//  0,
//  //RecType(
//  //  5,
//  //  RecType(
//  //    6,
//  //    RecType(7, FunType(8, TypeProj(9, 10), RecType(55, Top)))
//  //  )
//  //),
//  TypeProj(3, 4),
//  RecType(
//    5,
//    RecType(
//      6,
//      RecType(7, FunType(8, TypeProj(9, 10), RecType(55, Top)))
//    )
//  )
//)

(
  GlobalContext(
    Map(0 -> Top, 1 -> TypeProj(2, 3), 2 -> TypeDecl(3, Top, Top), 6 -> TypeDecl(7, Top, Top)),
    8
  ),
  -2,
  4,
  RecType(5, TypeProj(6, 7)),
  Que
)

P.namedln("simplify(p)", simplify(p))

val scope = scope1 + (z -> a)

val res1 = NoFuture.varLower(scope, r, z, p)
P.namedln("res1", res1)


//val (numQues, labeledPrototype) = prepMatch(r, simplify(unwrapRecTypes(p, z)))
val (numQues, labeledPrototype) = prepMatch(r, simplify(p))

//P.namedln("labeledPrototype", labeledPrototype)

val solveSet = (0 until numQues).map{TypeProj(r, _)}.toSet
val solveSetVariance = gatherVariance(r, labeledPrototype, Contravariant)


//val constraint = gatherConstraints(scope, solveSet, Some(z), labeledPrototype, a)
val constraint = gatherConstraints(scope, solveSet, None, labeledPrototype, a)


//P.namedln("constraint", constraint)

//val dnfStartTime = System.nanoTime()
//val dnfConstraint = dnf(constraint)
//val dnfEndTime = System.nanoTime()

//val cnfStartTime = System.nanoTime()
//val cnfConstraint = cnf(constraint)
//val cnfEndTime = System.nanoTime()

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

//val cl = cnfLists(cnfConstraint)
//P.namedln("cl", shortenlistlist(cl))
//P.namedln("cl", cl)
//P.namedln("cl.size", cl.size)
//P.namedln("cl.cartesian.size", cl.map{_.size: BigInt}.product)

//val ta = MultiAndConstraint(Map(TypeProj(0,1) -> (Map(), Bot, Top, Covariant)))
//val tb = MultiAndConstraint(Map(TypeProj(0,2) -> (Map(), Bot, Top, Covariant)))
//val tc = MultiAndConstraint(Map(TypeProj(0,3) -> (Map(), Bot, Top, Covariant)))
//val td = MultiAndConstraint(Map(TypeProj(0,4) -> (Map(), Bot, Top, Covariant)))
//val tcnf = cnfLists(orConstraint(andConstraint(ta, tb), andConstraint(tc, td)))
//val tdnf = dnfLists(orConstraint(andConstraint(ta, tb), andConstraint(tc, td)))
//P.namedln("tcnf", shortenlistlist(tcnf))
//P.namedln("tdnf", shortenlistlist(tdnf))

//println(s"DNF TIME = ${(dnfEndTime - dnfStartTime) * 1e-9}s")
//println(s"CNF TIME = ${(cnfEndTime - cnfStartTime) * 1e-9}s")

def constraintSize(c: Constraint): Int = c match {
  case OrConstraint(a, b) => constraintSize(a) + constraintSize(b)
  case AndConstraint(a, b) => constraintSize(a) + constraintSize(b)
  case _ => 1
}
println(s"size(constraint) = ${constraintSize(constraint)}")
//println(s"size(dnfConstraint) = ${constraintSize(dnfConstraint)}")
//println(s"size(cnfConstraint) = ${constraintSize(cnfConstraint)}")


val startTime = System.nanoTime()
//val res2 = solveConstraint(scope, r, Some(z), solveSet, solveSetVariance, constraint, labeledPrototype, Contravariant)
val res2 = solveConstraint(scope, r, None, solveSet, solveSetVariance, constraint, labeledPrototype, Contravariant)
val endTime = System.nanoTime()

P.namedln("res2", res2)
println(s"RES2 TIME = ${(endTime - startTime) * 1e-9}")

if (res2 != None) {
  P.namedln("lower(z:a, p) == a", varEqualTypes(scope + (z -> res2.get), z, a))
}

if (res2 != None && !isPrototype(p)) {
  P.namedln("lower(z:b, a) == a", varEqualTypes(scope + (z -> res2.get), z, p))
}

//val res3startTime = System.nanoTime()
//val res3 = solveCnf(scope, solveSet, cnfConstraint, labeledPrototype)
//val res3endTime = System.nanoTime()
//
//pprint.pprintln(res3, height=4000000)
//println(s"RES3 TIME = ${(res3endTime - res3startTime) * 1e-9}")



}

def raisedebug(): Unit = {
val (GlobalContext(globalScope, nextSymbol), r, z, a, p): (GlobalContext, Symbol, Symbol, Type, Prototype) =

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

//(
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

//(
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


//(
//  GlobalContext(
//    Map(6 -> TypeDecl(7, Bot, Top)),
//    16
//  ),
//  1,
//  0,
//  Bot,
//  FieldDecl(
//    2,
//    RecType(
//      3,
//      AndType(
//        TypeDecl(
//          4,
//          AndType(Bot, Bot),
//          AndType(
//            TypeDecl(
//              5,
//              TypeProj(6, 7),
//              TypeProj(6, 7)
//            ),
//            AndType(
//              FieldDecl(8, Top),
//              FieldDecl(
//                9,
//                AndType(
//                  FieldDecl(10, Top),
//                  TypeDecl(
//                    11,
//                    Bot,
//                    Bot
//                  )
//                )
//              )
//            )
//          )
//        ),
//        AndType(
//          FieldDecl(
//            12,
//            FieldDecl(
//              13,
//              FunType(
//                14,
//                TypeProj(3, 4),
//                Top
//              )
//            )
//          ),
//          FieldDecl(15, Top)
//        )
//      )
//    )
//  )
//)

//(
//  GlobalContext(
//    Map(
//      19 -> TypeDecl(20, TypeDecl(21, Bot, FieldDecl(22, Bot)), Top),
//      17 -> TypeDecl(18, AndType(TypeProj(19, 20), Top), TypeProj(19, 20)),
//      24 -> TypeDecl(25, Bot, TypeProj(17, 18))
//    ),
//    28
//  ),
//  1,
//  0,
//  AndType(
//    FunType(
//      2,
//      RecType(
//        3,
//        AndType(
//          FieldDecl(
//            4,
//            FieldDecl(
//              5,
//              FieldDecl(
//                6,
//                RecType(
//                  7,
//                  AndType(
//                    FieldDecl(8, Bot),
//                    FieldDecl(9, TypeDecl(10, Bot, Bot))
//                  )
//                )
//              )
//            )
//          ),
//          FieldDecl(11, Top)
//        )
//      ),
//      Bot
//    ),
//    FunType(
//      15,
//      AndType(
//        AndType(
//          FieldDecl(16, TypeProj(17, 18)),
//          TypeDecl(23, Bot, TypeProj(24, 25))
//        ),
//        TypeDecl(26, Bot, FunType(27, TypeProj(24, 25), Bot))
//      ),
//      TypeProj(24, 25)
//    )
//  ),
//  //FunType(
//  //  2,
//  //  RecType(
//  //    3,
//  //    AndType(
//  //      FieldDecl(
//  //        4,
//  //        FieldDecl(
//  //          5,
//  //          FieldDecl(
//  //            6,
//  //            RecType(
//  //              7,
//  //              AndType(
//  //                FieldDecl(8, Bot),
//  //                FieldDecl(9, TypeDecl(10, Bot, Bot))
//  //              )
//  //            )
//  //          )
//  //        )
//  //      ),
//  //      FieldDecl(11, Top)
//  //    )
//  //  ),
//  //  TypeDecl(
//  //    12,
//  //    RecType(13, FieldDecl(14, Top)),
//  //    RecType(13, FieldDecl(14, Top))
//  //  )
//  //),
//  FunType(2, Que, Que)
//)

//(
//  GlobalContext(
//    Map(),
//    4
//  ),
//  1,
//  0,
//  RecType(2,
//    Top),
//    RecType(3, Top)
//)

//(
//  GlobalContext(Map(), 9),
//  1,
//  0,
//  Bot,
//  TypeDecl(
//    2,
//    AndType(
//      AndType(
//        FunType(3, Bot, Bot),
//        FunType(4, TypeDecl(5, Bot, Bot), TypeDecl(6, Top, Top))
//      ),
//      RecType(7, FieldDecl(8, Bot))
//    ),
//    FunType(3, Bot, Bot)
//  )
//)

//(
//  GlobalContext(Map(), 9),
//  1,
//  0,
//  TypeDecl(
//    2,
//    AndType(
//      AndType(
//        FunType(3, Bot, Bot),
//        FunType(4, TypeDecl(5, Bot, Bot), TypeDecl(6, Top, Top))
//      ),
//      RecType(7, FieldDecl(8, Bot))
//    ),
//    FunType(3, Bot, Bot)
//  ),
//  TypeDecl(
//    2,
//    AndType(
//      AndType(
//        FunType(3, Bot, Bot),
//        FunType(4, TypeDecl(5, Bot, Bot), TypeDecl(6, Top, Top))
//      ),
//      RecType(7, FieldDecl(8, Bot))
//    ),
//    FunType(3, Bot, Bot)
//  )
//)

//(
//  GlobalContext(Map(), 9),
//  1,
//  0,
//    AndType(
//      AndType(
//        FunType(3, Bot, Bot),
//        FunType(4, TypeDecl(5, Bot, Bot), TypeDecl(6, Top, Top))
//      ),
//      RecType(7, FieldDecl(8, Bot))
//    ),
//    AndType(
//      AndType(
//        FunType(3, Bot, Bot),
//        FunType(4, TypeDecl(5, Bot, Bot), TypeDecl(6, Top, Top))
//      ),
//      RecType(7, FieldDecl(8, Bot))
//    )
//)

//(
//  GlobalContext(
//    Map(4 -> TypeDecl(5, FieldDecl(6, Bot), FieldDecl(6, Bot))),
//    10
//  ),
//  1,
//  0,
//  AndType(
//    AndType(Bot, FunType(8, TypeProj(4, 5), Bot)),
//    FunType(9, Top, Top)
//  ),
//  RecType(
//    2,
//    AndType(
//      TypeDecl(3, Bot, TypeProj(4, 5)),
//      FieldDecl(7, TypeProj(4, 5))
//    )
//  )
//)


//(
//  GlobalContext(
//    Map(
//      8 -> TypeDecl(9, Top, Top),
//      6 -> TypeDecl(7, Bot, TypeProj(8, 9)),
//      4 -> TypeDecl(5, Bot, TypeProj(6, 7))
//    ),
//    16
//  ),
//  -1,
//  0,
//  FieldDecl(
//    1,
//    RecType(
//      2,
//      AndType(
//        AndType(
//          TypeDecl(3, Bot, TypeProj(4, 5)),
//          AndType(
//            TypeDecl(3, Bot, TypeProj(4, 5)),
//            AndType(
//              TypeDecl(
//                10,
//                AndType(Bot, Bot),
//                TypeDecl(11, TypeProj(8, 9), TypeProj(8, 9))
//              ),
//              TypeDecl(12, Bot, Bot)
//            )
//          )
//        ),
//        AndType(
//          FieldDecl(13, Bot),
//          FieldDecl(14, FieldDecl(15, Top))
//        )
//      )
//    )
//  ),
//  FieldDecl(
//    1,
//    RecType(
//      2,
//      AndType(
//        AndType(
//          TypeDecl(3, Bot, TypeProj(4, 5)),
//          AndType(
//            TypeDecl(3, Bot, TypeProj(4, 5)),
//            AndType(
//              TypeDecl(
//                10,
//                AndType(Bot, Bot),
//                TypeDecl(11, TypeProj(8, 9), TypeProj(8, 9))
//              ),
//              TypeDecl(12, Bot, Bot)
//            )
//          )
//        ),
//        AndType(
//          FieldDecl(13, Bot),
//          FieldDecl(14, FieldDecl(15, Top))
//        )
//      )
//    )
//  )
//)

//(
//  GlobalContext(
//    Map(
//      7 -> TypeDecl(8, Bot, Bot),
//      12 -> TypeDecl(13, Bot, Bot),
//      10 -> TypeDecl(11, TypeProj(12, 13), Top)
//    ),
//    18
//  ),
//  1,
//  0,
//  RecType(
//    2,
//    FieldDecl(
//      3,
//      RecType(
//        4,
//        AndType(
//          TypeDecl(
//            5,
//            Bot,
//            FunType(
//              6,
//              TypeProj(7, 8),
//              TypeDecl(9, AndType(Bot, TypeProj(10, 11)), Bot)
//            )
//          ),
//          AndType(
//            FieldDecl(14, Top),
//            AndType(
//              FieldDecl(15, FieldDecl(16, Bot)),
//              FieldDecl(17, Bot)
//            )
//          )
//        )
//      )
//    )
//  ),
//  RecType(
//    2,
//    FieldDecl(
//      3,
//      RecType(
//        4,
//        AndType(
//          TypeDecl(
//            5,
//            Bot,
//            FunType(
//              6,
//              TypeProj(7, 8),
//              TypeDecl(9, AndType(Bot, TypeProj(10, 11)), Bot)
//            )
//          ),
//          AndType(
//            FieldDecl(14, Top),
//            AndType(
//              FieldDecl(15, FieldDecl(16, Bot)),
//              FieldDecl(17, Bot)
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
//      7 -> TypeDecl(8, Bot, Top),
//      5 -> TypeDecl(6, TypeProj(7, 8), Top)
//    ),
//    18
//  ),
//  1,
//  0,
//  RecType(
//    2,
//    AndType(
//      AndType(
//        TypeDecl(3, Bot, FunType(4, Bot, TypeProj(5, 6))),
//        AndType(
//          TypeDecl(3, Bot, FunType(4, Bot, TypeProj(5, 6))),
//          AndType(
//            FieldDecl(9, FieldDecl(10, Top)),
//            TypeDecl(
//              11,
//              AndType(Bot, FieldDecl(16, Top)),
//              RecType(
//                12,
//                AndType(
//                  TypeDecl(
//                    13,
//                    FieldDecl(14, TypeProj(2, 3)),
//                    FieldDecl(14, TypeProj(2, 3))
//                  ),
//                  FieldDecl(15, Top)
//                )
//              )
//            )
//          )
//        )
//      ),
//      FieldDecl(17, TypeProj(7, 8))
//    )
//  ),
//  RecType(
//    2,
//    AndType(
//      AndType(
//        TypeDecl(3, Bot, FunType(4, Bot, TypeProj(5, 6))),
//        AndType(
//          TypeDecl(3, Bot, FunType(4, Bot, TypeProj(5, 6))),
//          AndType(
//            FieldDecl(9, FieldDecl(10, Top)),
//            TypeDecl(
//              11,
//              AndType(Bot, FieldDecl(16, Top)),
//              RecType(
//                12,
//                AndType(
//                  TypeDecl(
//                    13,
//                    FieldDecl(14, TypeProj(2, 3)),
//                    FieldDecl(14, TypeProj(2, 3))
//                  ),
//                  FieldDecl(15, Top)
//                )
//              )
//            )
//          )
//        )
//      ),
//      FieldDecl(17, TypeProj(7, 8))
//    )
//  )
//)

//(
//  GlobalContext(
//    Map(
//      6 -> TypeDecl(
//        7,
//        FunType(
//          8,
//          AndType(
//            AndType(FieldDecl(9, Bot), FieldDecl(10, Top)),
//            FieldDecl(11, FunType(12, Bot, Bot))
//          ),
//          Top
//        ),
//        FunType(
//          8,
//          AndType(
//            AndType(FieldDecl(9, Bot), FieldDecl(10, Top)),
//            FieldDecl(11, FunType(12, Bot, Bot))
//          ),
//          Top
//        )
//      )
//    ),
//    19
//  ),
//  1,
//  0,
//  RecType(
//    2,
//    AndType(
//      AndType(
//        TypeDecl(
//          3,
//          FieldDecl(4, Bot),
//          FieldDecl(4, FunType(5, Top, TypeProj(6, 7)))
//        ),
//        FieldDecl(
//          13,
//          AndType(
//            AndType(
//              AndType(
//                FieldDecl(14, TypeProj(2, 3)),
//                FieldDecl(15, Top)
//              ),
//              FieldDecl(16, Top)
//            ),
//            FieldDecl(17, Bot)
//          )
//        )
//      ),
//      FieldDecl(18, Top)
//    )
//  ),
//  Que
//)

//( // TODO
//  GlobalContext(
//    Map(
//      5 -> TypeDecl(6, Bot, Bot),
//      14 -> TypeDecl(15, TypeProj(20, 21), TypeProj(16, 17)),
//      20 -> TypeDecl(21, Bot, TypeProj(22, 23)),
//      22 -> TypeDecl(23, Bot, TypeProj(16, 17)),
//      12 -> TypeDecl(
//        13,
//        TypeProj(14, 15),
//        RecType(
//          1,
//          AndType(
//            TypeDecl(2, TypeProj(3, 4), Top),
//            AndType(
//              FieldDecl(7, TypeProj(1, 2)),
//              AndType(
//                FieldDecl(
//                  8,
//                  TypeDecl(9, Bot, TypeDecl(10, Bot, Bot))
//                ),
//                FieldDecl(11, TypeProj(5, 6))
//              )
//            )
//          )
//        )
//      ),
//      3 -> TypeDecl(4, TypeProj(5, 6), TypeProj(5, 6)),
//      18 -> TypeDecl(19, Bot, Bot),
//      16 -> TypeDecl(17, Bot, TypeProj(18, 19))
//    ),
//    24
//  ),
//  -1,
//  0,
//  TypeProj(12, 13),
//  RecType(
//    1,
//    AndType(
//      TypeDecl(2, TypeProj(3, 4), Top),
//      AndType(
//        FieldDecl(7, TypeProj(1, 2)),
//        AndType(
//          FieldDecl(8, TypeDecl(9, Bot, TypeDecl(10, Bot, Bot))),
//          FieldDecl(11, TypeProj(5, 6))
//        )
//      )
//    )
//  )
//)

//( // TODO
//  GlobalContext(
//    Map(
//      25 -> TypeDecl(26, TypeProj(27, 28), TypeProj(27, 28)),
//      29 -> TypeDecl(30, Bot, Bot),
//      21 -> TypeDecl(22, TypeProj(23, 24), TypeProj(23, 24)),
//      13 -> TypeDecl(
//        14,
//        TypeProj(15, 16),
//        RecType(
//          2,
//          AndType(
//            AndType(
//              FieldDecl(3, FieldDecl(4, Top)),
//              TypeDecl(5, Bot, Top)
//            ),
//            AndType(
//              AndType(
//                FieldDecl(
//                  6,
//                  TypeDecl(7, Bot, FunType(8, TypeProj(2, 5), Top))
//                ),
//                FieldDecl(9, TypeProj(2, 5))
//              ),
//              AndType(
//                AndType(FieldDecl(10, Top), FieldDecl(11, Bot)),
//                FieldDecl(12, TypeProj(2, 5))
//              )
//            )
//          )
//        )
//      ),
//      17 -> TypeDecl(
//        18,
//        TypeProj(29, 30),
//        AndType(FieldDecl(19, Bot), FieldDecl(20, TypeProj(21, 22)))
//      ),
//      27 -> TypeDecl(28, Bot, Bot),
//      23 -> TypeDecl(24, TypeProj(25, 26), Top),
//      15 -> TypeDecl(16, AndType(Bot, TypeProj(17, 18)), Bot)
//    ),
//    31
//  ),
//  1,
//  0,
//  TypeProj(13, 14),
//  //RecType(
//  //  2,
//  //  AndType(
//  //    AndType(
//  //      FieldDecl(3, FieldDecl(4, Top)),
//  //      TypeDecl(5, Bot, Top)
//  //    ),
//  //    AndType(
//  //      AndType(
//  //        FieldDecl(
//  //          6,
//  //          TypeDecl(7, Bot, FunType(8, TypeProj(2, 5), Top))
//  //        ),
//  //        FieldDecl(9, TypeProj(2, 5))
//  //      ),
//  //      AndType(
//  //        AndType(FieldDecl(10, Top), FieldDecl(11, Bot)),
//  //        FieldDecl(12, TypeProj(2, 5))
//  //      )
//  //    )
//  //  )
//  //),
//  RecType(
//    2,
//    AndType(
//      AndType(
//        FieldDecl(3, FieldDecl(4, Top)),
//        TypeDecl(5, Bot, Top)
//      ),
//      AndType(
//        AndType(
//          FieldDecl(
//            6,
//            TypeDecl(7, Bot, FunType(8, TypeProj(2, 5), Top))
//          ),
//          FieldDecl(9, TypeProj(2, 5))
//        ),
//        AndType(
//          AndType(FieldDecl(10, Top), FieldDecl(11, Bot)),
//          FieldDecl(12, TypeProj(2, 5))
//        )
//      )
//    )
//  )
//)

//(
//  GlobalContext(
//    Map(
//    ),
//    5
//  ),
//  1,
//  0,
//  RecType(4, RecType(3, Top)),
//  RecType(3, Top)
//)

//(
//  GlobalContext(
//    Map(
//    ),
//    5
//  ),
//  1,
//  0,
//  RecType(3, Top),
//  RecType(3, RecType(4, Top))
//)

//(
//  GlobalContext(
//    Map(
//    ),
//    5
//  ),
//  1,
//  0,
//  RecType(3, Bot),
//  Bot
//)

//( // TODO
//  GlobalContext(
//    Map(
//      5 -> TypeDecl(6, RecType(7, Top), RecType(7, Top)),
//      2 -> TypeDecl(3, RecType(4, TypeProj(5, 6)), Top),
//      17 -> TypeDecl(18, AndType(Bot, Top), Bot),
//      8 -> TypeDecl(
//        9,
//        TypeProj(15, 16),
//        AndType(
//          AndType(TypeProj(2, 3), TypeProj(5, 6)),
//          FunType(
//            10,
//            RecType(11, FieldDecl(12, RecType(13, Bot))),
//            RecType(14, Bot)
//          )
//        )
//      ),
//      15 -> TypeDecl(
//        16,
//        AndType(TypeProj(17, 18), FieldDecl(19, TypeProj(17, 18))),
//        AndType(TypeProj(17, 18), FieldDecl(19, TypeProj(17, 18)))
//      )
//    ),
//    20
//  ),
//  1,
//  0,
//  TypeProj(8, 9),
//  TypeProj(2, 3)
//)

//(
//  GlobalContext(Map(1 -> TypeDecl(2, Top, Top)), 6),
//  5,
//  0,
//  RecType(3, RecType(4, Top)),
//  TypeProj(1, 2)
//)

//( // TODO
//  GlobalContext(
//    Map(
//      5 -> TypeDecl(6, RecType(7, Top), RecType(7, Top))
//    ),
//    20
//  ),
//  1,
//  0,
//  TypeProj(5, 6),
//  RecType(4, TypeProj(5, 6))
//)


( // TODO fun(x: Que)x.T
  GlobalContext(
    Map(
      11 -> TypeDecl(12, Bot, Bot),
      9 -> TypeDecl(10, TypeProj(11, 12), TypeProj(11, 12)),
      14 -> TypeDecl(15, Bot, Bot),
      0 -> Top,
      1 -> Top,
      6 -> FunType(7, Bot, FieldDecl(8, TypeProj(9, 10))),
      13 -> TypeProj(14, 15),
      2 -> RecType(3, FieldDecl(4, TypeDecl(5, Top, Top))),
      16 -> Bot
    ),
    29
  ),
  18,
  17,
  FunType(
    19,
    AndType(
      FieldDecl(20, TypeDecl(21, Top, Top)),
      TypeDecl(
        22,
        Bot,
        AndType(FieldDecl(23, TypeProj(14, 15)), Bot)
      )
    ),
    AndType(
      FunType(24, TypeProj(19, 22), Bot),
      FunType(
        25,
        FunType(26, RecType(27, Bot), TypeProj(11, 12)),
        RecType(28, Bot)
      )
    )
  ),
  FunType(
    19,
    Que,
    AndType(
      FunType(24, TypeProj(19, 22), Bot),
      FunType(25, Que, RecType(28, Bot))
    )
  )
)

val scope = globalScope + (z -> a)

//val res1 = NoFuture.varRaise(scope, r, z, p)
//P.namedln("res1", res1)

//val res3 = NoFuture.raise(scope, r, None, a, p)
//P.namedln("res3", res3)

import NoFuture._

val (numQues, labeledPrototype) = prepMatch(r, simplify(p))

val solveSet = (0 until numQues).map{TypeProj(r, _)}.toSet

val constraint = gatherConstraints(scope, solveSet, Some(z), a, labeledPrototype)


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
val res2 = solveConstraint(scope, r, Some(z), solveSet, solveSetVariance, constraint, labeledPrototype, Covariant)
val endTime = System.nanoTime()

P.namedln("res2", res2)
println(s"TIME = ${(endTime - startTime) * 1e-9}s")

if (res2 != None && !isPrototype(p)) {
  val b = p
  P.namedln("raise(z:res2, b) == b", varEqualTypes(scope + (z -> res2.get), z, b))
}

if (res2 != None && p == Que) {
  val b = a
  P.namedln("raise(z:res2, b) == b", varEqualTypes(scope + (z -> res2.get), z, b))
}


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
//// AndType(
////  FunType(
////    1,
////    AndType(
////      TypeDecl(
////        2,
////        FunType(3, FunType(4, Bot, TypeProj(5, 6)), Top),
////        FunType(3, FunType(4, Bot, TypeProj(5, 6)), TypeProj(7, 8))
////      ),
////      TypeDecl(
////        2,
////        FunType(3, FunType(4, Bot, TypeProj(5, 6)), Top),
////        FunType(3, FunType(4, Bot, TypeProj(5, 6)), TypeProj(7, 8))
////      )
////    ),
////    FunType(
////      9,
////      AndType(TypeProj(7, 8), TypeProj(7, 8)),
////      TypeDecl(10, Bot, TypeProj(7, 8))
////    )
////  ),
////  TypeProj(7, 8)
////)
// //AndType(
// // FunType(
// //   1,
// //   TypeDecl(
// //     2,
// //     FunType(3, FunType(4, Bot, TypeProj(5, 6)), Top),
// //     FunType(3, FunType(4, Bot, TypeProj(5, 6)), TypeProj(7, 8))
// //   ),
// //   FunType(9, TypeProj(7, 8), TypeDecl(10, Bot, TypeProj(7, 8)))
// // ),
//  TypeProj(7, 8)
////)
//)

//(
//  GlobalContext(
//    Map(
//      5 -> TypeDecl(6, Top, Top),
//      1 -> TypeDecl(
//        2,
//        Bot,
//        TypeDecl(
//          3,
//          AndType(Bot, FunType(7, TypeProj(5, 6), TypeDecl(8, Top, Top))),
//          TypeDecl(4, TypeProj(5, 6), TypeProj(5, 6))
//        )
//      )
//    ),
//    12
//  ),
//  0,
//    TypeDecl(
//      3,
//      AndType(Bot, FunType(7, TypeProj(5, 6), TypeDecl(8, Top, Top))
//      ),
//      TypeDecl(4, TypeProj(5, 6), TypeProj(5, 6))
//    )
//  ,
//  TypeProj(1, 2)
//)

//(
//  GlobalContext(
//    Map(
//      3 -> TypeDecl(
//        4,
//        RecType(
//          5,
//          AndType(
//            AndType(FieldDecl(6, Top), FieldDecl(7, Bot)),
//            FieldDecl(
//              8,
//              RecType(9, FieldDecl(10, FieldDecl(11, Top)))
//            )
//          )
//        ),
//        RecType(
//          5,
//          AndType(
//            AndType(FieldDecl(6, Top), FieldDecl(7, Bot)),
//            FieldDecl(
//              8,
//              RecType(9, FieldDecl(10, FieldDecl(11, Top)))
//            )
//          )
//        )
//      ),
//      1 -> AndType(
//        AndType(FieldDecl(6, Top), FieldDecl(7, Bot)),
//        FieldDecl(8, RecType(9, FieldDecl(10, FieldDecl(11, Top))))
//      )
//    ),
//    12
//  ),
//  0,
//  AndType(
//    AndType(FieldDecl(6, Top), FieldDecl(7, Bot)),
//    FieldDecl(8, RecType(9, FieldDecl(10, FieldDecl(11, Top))))
//  ),
//  AndType(TypeProj(3, 4), FieldDecl(8, Top))
//)

//(
//  GlobalContext(Map(), 5),
//  0,
//  RecType(
//    1,
//    AndType(
//      AndType(FieldDecl(2, Bot), FieldDecl(3, Top)),
//      FieldDecl(4, Bot)
//    )
//  ),
//  RecType(
//    1,
//    AndType(
//      AndType(FieldDecl(2, Bot), FieldDecl(3, Top)),
//      FieldDecl(4, Bot)
//    )
//  )
//)

//(
//  GlobalContext(Map(), 9),
//  0,
//  TypeDecl(
//    2,
//    AndType(
//      AndType(
//        FunType(3, Bot, Bot),
//        FunType(4, TypeDecl(5, Bot, Bot), TypeDecl(6, Top, Top))
//      ),
//      RecType(7, FieldDecl(8, Bot))
//    ),
//    FunType(3, Bot, Bot)
//  ),
//  TypeDecl(
//    2,
//    AndType(
//      AndType(RecType(7, FieldDecl(8, Bot)), FunType(3, Bot, Bot)),
//      FunType(4, TypeDecl(5, Bot, Bot), TypeDecl(6, Top, Top))
//    ),
//    FunType(3, Bot, Bot)
//  )
//)

//(
//  GlobalContext(
//    Map(
//      7 -> TypeDecl(8, Bot, Bot),
//      12 -> TypeDecl(13, Bot, Bot),
//      10 -> TypeDecl(11, TypeProj(12, 13), Top)
//    ),
//    18
//  ),
//  0,
//  RecType(
//    2,
//    FieldDecl(
//      3,
//      RecType(
//        4,
//        AndType(
//          TypeDecl(
//            5,
//            Bot,
//            FunType(
//              6,
//              TypeProj(7, 8),
//              TypeDecl(9, AndType(Bot, TypeProj(10, 11)), Bot)
//            )
//          ),
//          AndType(
//            FieldDecl(14, Top),
//            AndType(
//              FieldDecl(15, FieldDecl(16, Bot)),
//              FieldDecl(17, Bot)
//            )
//          )
//        )
//      )
//    )
//  ),
//  RecType(
//    2,
//    FieldDecl(
//      3,
//      RecType(
//        4,
//        AndType(
//          TypeDecl(
//            5,
//            Bot,
//            FunType(
//              6,
//              TypeProj(7, 8),
//              TypeDecl(9, AndType(Bot, TypeProj(10, 11)), Bot)
//            )
//          ),
//          AndType(
//            FieldDecl(14, Top),
//            AndType(
//              FieldDecl(15, FieldDecl(16, Bot)),
//              FieldDecl(17, Bot)
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
//      1 -> AndType(
//        TypeDecl(
//          2,
//          AndType(
//            AndType(
//              FieldDecl(3, Top),
//              TypeDecl(
//                4,
//                FieldDecl(5, Bot),
//                FieldDecl(5, FunType(6, Bot, Bot))
//              )
//            ),
//            RecType(7, FieldDecl(8, Bot))
//          ),
//          AndType(
//            FieldDecl(3, Top),
//            TypeDecl(
//              4,
//              FieldDecl(5, Bot),
//              FieldDecl(5, FunType(6, Bot, Bot))
//            )
//          )
//        ),
//        TypeDecl(
//          2,
//          Bot,
//          RecType(
//            9,
//            AndType(
//              TypeDecl(
//                10,
//                Bot,
//                FunType(11, TypeDecl(12, Bot, Bot), Top)
//              ),
//              AndType(
//                FieldDecl(13, Top),
//                FieldDecl(14, FieldDecl(15, TypeProj(9, 10)))
//              )
//            )
//          )
//        )
//      )
//    ),
//    16
//  ),
//  0,
//  AndType(
//    AndType(
//      FieldDecl(3, Top),
//      TypeDecl(
//        4,
//        FieldDecl(5, Bot),
//        FieldDecl(5, FunType(6, Bot, Bot))
//      )
//    ),
//    RecType(
//      9,
//      AndType(
//        TypeDecl(10, Bot, FunType(11, TypeDecl(12, Bot, Bot), Top)),
//        AndType(
//          FieldDecl(13, Top),
//          FieldDecl(14, FieldDecl(15, TypeProj(9, 10)))
//        )
//      )
//    )
//  ),
//  AndType(
//    AndType(
//      FieldDecl(3, Top),
//      TypeDecl(
//        4,
//        FieldDecl(5, Bot),
//        FieldDecl(5, FunType(6, Bot, Bot))
//      )
//    ),
//    RecType(
//      9,
//      AndType(
//        TypeDecl(10, Bot, FunType(11, TypeDecl(12, Bot, Bot), Top)),
//        AndType(
//          FieldDecl(13, Top),
//          FieldDecl(14, FieldDecl(15, TypeProj(9, 10)))
//        )
//      )
//    )
//  )
//)

//(
//  GlobalContext(Map(), 13),
//  0,
//  AndType(
//    AndType(FieldDecl(11, Top), FieldDecl(12, Top)),
//    RecType(
//      2,
//      AndType(
//        TypeDecl(3, Bot, Bot),
//        FieldDecl(
//          4,
//          RecType(
//            5,
//            AndType(
//              TypeDecl(6, AndType(Bot, Bot), TypeProj(2, 3)),
//              FieldDecl(
//                7,
//                FunType(
//                  8,
//                  TypeDecl(9, TypeProj(5, 6), TypeProj(5, 6)),
//                  FunType(10, TypeProj(5, 6), Top)
//                )
//              )
//            )
//          )
//        )
//      )
//    )
//  ),
//  AndType(
//    RecType(
//      2,
//      AndType(
//        TypeDecl(3, Bot, Bot),
//        FieldDecl(
//          4,
//          RecType(
//            5,
//            AndType(
//              TypeDecl(6, AndType(Bot, Bot), TypeProj(2, 3)),
//              FieldDecl(
//                7,
//                FunType(
//                  8,
//                  TypeDecl(9, TypeProj(5, 6), TypeProj(5, 6)),
//                  FunType(10, TypeProj(5, 6), Top)
//                )
//              )
//            )
//          )
//        )
//      )
//    ),
//    AndType(FieldDecl(11, Top), FieldDecl(12, Top))
//  )
//)


//(
//  GlobalContext(
//    Map(
//      9 -> TypeDecl(10, Bot, Top),
//      1 -> AndType(
//        RecType(
//          3,
//          AndType(
//            AndType(
//              AndType(
//                TypeDecl(4, Bot, Top),
//                FieldDecl(5, FieldDecl(6, TypeProj(3, 4)))
//              ),
//              FieldDecl(7, Top)
//            ),
//            FieldDecl(8, Top)
//          )
//        ),
//        TypeProj(9, 10)
//      )
//    ),
//    11
//  ),
//  0,
//  AndType(
//    RecType(
//      3,
//      AndType(
//        AndType(
//          AndType(
//            TypeDecl(4, Bot, Top),
//            FieldDecl(5, FieldDecl(6, TypeProj(3, 4)))
//          ),
//          FieldDecl(7, Top)
//        ),
//        FieldDecl(8, Top)
//      )
//    ),
//    TypeProj(9, 10)
//  ),
//  RecType(
//    3,
//    AndType(
//      AndType(
//        AndType(
//          TypeDecl(4, Bot, Top),
//          FieldDecl(5, FieldDecl(6, TypeProj(3, 4)))
//        ),
//        FieldDecl(7, Top)
//      ),
//      FieldDecl(8, Top)
//    )
//  )
//)

//( // TODO
//  GlobalContext(
//    Map(
//      14 -> TypeDecl(15, TypeProj(16, 17), TypeProj(16, 17)),
//      1 -> AndType(
//        AndType(
//          AndType(TypeProj(3, 4), FunType(39, Bot, Bot)),
//          TypeProj(16, 17)
//        ),
//        FunType(
//          40,
//          FunType(41, TypeProj(9, 10), FunType(42, Bot, Bot)),
//          TypeDecl(
//            43,
//            AndType(
//              AndType(Bot, TypeProj(16, 17)),
//              AndType(
//                TypeDecl(
//                  52,
//                  AndType(Bot, FunType(54, Bot, Bot)),
//                  FunType(53, Top, Bot)
//                ),
//                TypeDecl(55, TypeProj(9, 10), TypeProj(9, 10))
//              )
//            ),
//            TypeDecl(
//              44,
//              AndType(
//                Bot,
//                RecType(
//                  45,
//                  AndType(
//                    FieldDecl(46, Top),
//                    AndType(
//                      FieldDecl(47, Top),
//                      AndType(
//                        AndType(
//                          FieldDecl(48, Top),
//                          FieldDecl(49, TypeProj(7, 8))
//                        ),
//                        FieldDecl(50, FieldDecl(51, TypeProj(7, 8)))
//                      )
//                    )
//                  )
//                )
//              ),
//              TypeProj(19, 20)
//            )
//          )
//        )
//      ),
//      9 -> TypeDecl(
//        10,
//        AndType(
//          AndType(
//            Bot,
//            TypeDecl(
//              12,
//              FieldDecl(
//                13,
//                AndType(
//                  TypeProj(16, 17),
//                  FieldDecl(
//                    21,
//                    TypeDecl(
//                      22,
//                      AndType(Bot, Bot),
//                      FieldDecl(23, FunType(24, Top, Top))
//                    )
//                  )
//                )
//              ),
//              FieldDecl(13, TypeProj(14, 15))
//            )
//          ),
//          Top
//        ),
//        TypeDecl(11, Bot, Bot)
//      ),
//      7 -> TypeDecl(8, TypeProj(9, 10), TypeProj(9, 10)),
//      3 -> TypeDecl(
//        4,
//        AndType(
//          FunType(5, FieldDecl(6, TypeProj(7, 8)), Bot),
//          AndType(
//            TypeDecl(25, TypeProj(14, 15), TypeProj(14, 15)),
//            FieldDecl(
//              26,
//              TypeDecl(
//                27,
//                TypeDecl(
//                  28,
//                  AndType(
//                    AndType(Bot, Bot),
//                    FieldDecl(
//                      30,
//                      FunType(
//                        31,
//                        TypeProj(16, 17),
//                        FunType(
//                          32,
//                          TypeDecl(
//                            33,
//                            TypeDecl(
//                              34,
//                              TypeDecl(35, Bot, Bot),
//                              TypeDecl(35, Bot, Bot)
//                            ),
//                            TypeDecl(
//                              34,
//                              TypeDecl(35, Bot, Bot),
//                              TypeDecl(35, Bot, Bot)
//                            )
//                          ),
//                          FunType(
//                            36,
//                            RecType(37, FieldDecl(38, Bot)),
//                            Bot
//                          )
//                        )
//                      )
//                    )
//                  ),
//                  TypeDecl(
//                    29,
//                    FieldDecl(18, TypeProj(19, 20)),
//                    TypeProj(16, 17)
//                  )
//                ),
//                TypeDecl(
//                  28,
//                  AndType(
//                    AndType(Bot, Bot),
//                    FieldDecl(
//                      30,
//                      FunType(
//                        31,
//                        TypeProj(16, 17),
//                        FunType(
//                          32,
//                          TypeDecl(
//                            33,
//                            TypeDecl(
//                              34,
//                              TypeDecl(35, Bot, Bot),
//                              TypeDecl(35, Bot, Bot)
//                            ),
//                            TypeDecl(
//                              34,
//                              TypeDecl(35, Bot, Bot),
//                              TypeDecl(35, Bot, Bot)
//                            )
//                          ),
//                          FunType(
//                            36,
//                            RecType(37, FieldDecl(38, Bot)),
//                            Bot
//                          )
//                        )
//                      )
//                    )
//                  ),
//                  TypeDecl(
//                    29,
//                    FieldDecl(18, TypeProj(19, 20)),
//                    TypeProj(16, 17)
//                  )
//                )
//              )
//            )
//          )
//        ),
//        FunType(5, FieldDecl(6, TypeProj(7, 8)), Bot)
//      ),
//      16 -> TypeDecl(17, FieldDecl(18, TypeProj(19, 20)), Top),
//      19 -> TypeDecl(20, Top, Top)
//    ),
//    56
//  ),
//  0,
//  AndType(
//    FunType(40, Bot, TypeDecl(43, Bot, TypeDecl(44, Bot, Top))),
//    AndType(
//      AndType(
//        AndType(
//          FunType(5, FieldDecl(6, TypeProj(7, 8)), Bot),
//          FunType(39, Bot, Bot)
//        ),
//        TypeProj(16, 17)
//      ),
//      FunType(
//        40,
//        FunType(41, TypeProj(9, 10), FunType(42, Bot, Bot)),
//        TypeDecl(
//          43,
//          AndType(
//            AndType(Bot, TypeProj(16, 17)),
//            AndType(
//              TypeDecl(
//                52,
//                AndType(Bot, FunType(54, Bot, Bot)),
//                FunType(53, Top, Bot)
//              ),
//              TypeDecl(55, TypeProj(9, 10), TypeProj(9, 10))
//            )
//          ),
//          TypeDecl(
//            44,
//            AndType(
//              Bot,
//              RecType(
//                45,
//                AndType(
//                  FieldDecl(46, Top),
//                  AndType(
//                    FieldDecl(47, Top),
//                    AndType(
//                      AndType(
//                        FieldDecl(48, Top),
//                        FieldDecl(49, TypeProj(7, 8))
//                      ),
//                      FieldDecl(50, FieldDecl(51, TypeProj(7, 8)))
//                    )
//                  )
//                )
//              )
//            ),
//            TypeProj(19, 20)
//          )
//        )
//      )
//    )
//  ),
//  AndType(
//    TypeProj(3, 4),
//    FunType(40, Bot, TypeDecl(43, Bot, TypeDecl(44, Bot, Top)))
//  )
//)

//( // TODO
//  GlobalContext(
//    Map(
//      14 -> TypeDecl(15, TypeProj(16, 17), TypeProj(16, 17)),
//      1 -> AndType(
//        AndType(
//          AndType(TypeProj(3, 4), FunType(39, Bot, Bot)),
//          TypeProj(16, 17)
//        ),
//        FunType(
//          40,
//          FunType(41, TypeProj(9, 10), FunType(42, Bot, Bot)),
//          TypeDecl(
//            43,
//            AndType(
//              AndType(Bot, TypeProj(16, 17)),
//              AndType(
//                TypeDecl(
//                  52,
//                  AndType(Bot, FunType(54, Bot, Bot)),
//                  FunType(53, Top, Bot)
//                ),
//                TypeDecl(55, TypeProj(9, 10), TypeProj(9, 10))
//              )
//            ),
//            TypeDecl(
//              44,
//              AndType(
//                Bot,
//                RecType(
//                  45,
//                  AndType(
//                    FieldDecl(46, Top),
//                    AndType(
//                      FieldDecl(47, Top),
//                      AndType(
//                        AndType(
//                          FieldDecl(48, Top),
//                          FieldDecl(49, TypeProj(7, 8))
//                        ),
//                        FieldDecl(50, FieldDecl(51, TypeProj(7, 8)))
//                      )
//                    )
//                  )
//                )
//              ),
//              TypeProj(19, 20)
//            )
//          )
//        )
//      ),
//      9 -> TypeDecl(
//        10,
//        AndType(
//          AndType(
//            Bot,
//            TypeDecl(
//              12,
//              FieldDecl(
//                13,
//                AndType(
//                  TypeProj(16, 17),
//                  FieldDecl(
//                    21,
//                    TypeDecl(
//                      22,
//                      AndType(Bot, Bot),
//                      FieldDecl(23, FunType(24, Top, Top))
//                    )
//                  )
//                )
//              ),
//              FieldDecl(13, TypeProj(14, 15))
//            )
//          ),
//          Top
//        ),
//        TypeDecl(11, Bot, Bot)
//      ),
//      7 -> TypeDecl(8, TypeProj(9, 10), TypeProj(9, 10)),
//      3 -> TypeDecl(
//        4,
//        AndType(
//          FunType(5, FieldDecl(6, TypeProj(7, 8)), Bot),
//          AndType(
//            TypeDecl(25, TypeProj(14, 15), TypeProj(14, 15)),
//            FieldDecl(
//              26,
//              TypeDecl(
//                27,
//                TypeDecl(
//                  28,
//                  AndType(
//                    AndType(Bot, Bot),
//                    FieldDecl(
//                      30,
//                      FunType(
//                        31,
//                        TypeProj(16, 17),
//                        FunType(
//                          32,
//                          TypeDecl(
//                            33,
//                            TypeDecl(
//                              34,
//                              TypeDecl(35, Bot, Bot),
//                              TypeDecl(35, Bot, Bot)
//                            ),
//                            TypeDecl(
//                              34,
//                              TypeDecl(35, Bot, Bot),
//                              TypeDecl(35, Bot, Bot)
//                            )
//                          ),
//                          FunType(
//                            36,
//                            RecType(37, FieldDecl(38, Bot)),
//                            Bot
//                          )
//                        )
//                      )
//                    )
//                  ),
//                  TypeDecl(
//                    29,
//                    FieldDecl(18, TypeProj(19, 20)),
//                    TypeProj(16, 17)
//                  )
//                ),
//                TypeDecl(
//                  28,
//                  AndType(
//                    AndType(Bot, Bot),
//                    FieldDecl(
//                      30,
//                      FunType(
//                        31,
//                        TypeProj(16, 17),
//                        FunType(
//                          32,
//                          TypeDecl(
//                            33,
//                            TypeDecl(
//                              34,
//                              TypeDecl(35, Bot, Bot),
//                              TypeDecl(35, Bot, Bot)
//                            ),
//                            TypeDecl(
//                              34,
//                              TypeDecl(35, Bot, Bot),
//                              TypeDecl(35, Bot, Bot)
//                            )
//                          ),
//                          FunType(
//                            36,
//                            RecType(37, FieldDecl(38, Bot)),
//                            Bot
//                          )
//                        )
//                      )
//                    )
//                  ),
//                  TypeDecl(
//                    29,
//                    FieldDecl(18, TypeProj(19, 20)),
//                    TypeProj(16, 17)
//                  )
//                )
//              )
//            )
//          )
//        ),
//        FunType(5, FieldDecl(6, TypeProj(7, 8)), Bot)
//      ),
//      16 -> TypeDecl(17, FieldDecl(18, TypeProj(19, 20)), Top),
//      19 -> TypeDecl(20, Top, Top)
//    ),
//    56
//  ),
//  0,
//  AndType(
//    AndType(
//      AndType(
//        FunType(5, FieldDecl(6, TypeProj(7, 8)), Bot),
//        FunType(39, Bot, Bot)
//      ),
//      TypeProj(16, 17)
//    ),
//    FunType(
//      40,
//      FunType(41, TypeProj(9, 10), FunType(42, Bot, Bot)),
//      TypeDecl(43, Bot, TypeDecl(44, Bot, TypeProj(19, 20)))
//    )
//  ),
//  AndType(
//            TypeDecl(25, TypeProj(14, 15), TypeProj(14, 15)),
//            FieldDecl(
//              26,
//              TypeDecl(
//                27,
//                TypeDecl(
//                  28,
//                  AndType(
//                    AndType(Bot, Bot),
//                    FieldDecl(
//                      30,
//                      FunType(
//                        31,
//                        TypeProj(16, 17),
//                        FunType(
//                          32,
//                          TypeDecl(
//                            33,
//                            TypeDecl(
//                              34,
//                              TypeDecl(35, Bot, Bot),
//                              TypeDecl(35, Bot, Bot)
//                            ),
//                            TypeDecl(
//                              34,
//                              TypeDecl(35, Bot, Bot),
//                              TypeDecl(35, Bot, Bot)
//                            )
//                          ),
//                          FunType(
//                            36,
//                            RecType(37, FieldDecl(38, Bot)),
//                            Bot
//                          )
//                        )
//                      )
//                    )
//                  ),
//                  TypeDecl(
//                    29,
//                    FieldDecl(18, TypeProj(19, 20)),
//                    TypeProj(16, 17)
//                  )
//                ),
//                TypeDecl(
//                  28,
//                  AndType(
//                    AndType(Bot, Bot),
//                    FieldDecl(
//                      30,
//                      FunType(
//                        31,
//                        TypeProj(16, 17),
//                        FunType(
//                          32,
//                          TypeDecl(
//                            33,
//                            TypeDecl(
//                              34,
//                              TypeDecl(35, Bot, Bot),
//                              TypeDecl(35, Bot, Bot)
//                            ),
//                            TypeDecl(
//                              34,
//                              TypeDecl(35, Bot, Bot),
//                              TypeDecl(35, Bot, Bot)
//                            )
//                          ),
//                          FunType(
//                            36,
//                            RecType(37, FieldDecl(38, Bot)),
//                            Bot
//                          )
//                        )
//                      )
//                    )
//                  ),
//                  TypeDecl(
//                    29,
//                    FieldDecl(18, TypeProj(19, 20)),
//                    TypeProj(16, 17)
//                  )
//                )
//              )
//            )
//        )
//)

//( // TODO
//  GlobalContext(
//    Map(
//      14 -> TypeDecl(15, TypeProj(16, 17), TypeProj(16, 17)),
//      1 -> AndType(
//        AndType(
//          AndType(TypeProj(3, 4), FunType(39, Bot, Bot)),
//          TypeProj(16, 17)
//        ),
//        FunType(
//          40,
//          FunType(41, TypeProj(9, 10), FunType(42, Bot, Bot)),
//          TypeDecl(
//            43,
//            AndType(
//              AndType(Bot, TypeProj(16, 17)),
//              AndType(
//                TypeDecl(
//                  52,
//                  AndType(Bot, FunType(54, Bot, Bot)),
//                  FunType(53, Top, Bot)
//                ),
//                TypeDecl(55, TypeProj(9, 10), TypeProj(9, 10))
//              )
//            ),
//            TypeDecl(
//              44,
//              AndType(
//                Bot,
//                RecType(
//                  45,
//                  AndType(
//                    FieldDecl(46, Top),
//                    AndType(
//                      FieldDecl(47, Top),
//                      AndType(
//                        AndType(
//                          FieldDecl(48, Top),
//                          FieldDecl(49, TypeProj(7, 8))
//                        ),
//                        FieldDecl(50, FieldDecl(51, TypeProj(7, 8)))
//                      )
//                    )
//                  )
//                )
//              ),
//              TypeProj(19, 20)
//            )
//          )
//        )
//      ),
//      9 -> TypeDecl(
//        10,
//        AndType(
//          AndType(
//            Bot,
//            TypeDecl(
//              12,
//              FieldDecl(
//                13,
//                AndType(
//                  TypeProj(16, 17),
//                  FieldDecl(
//                    21,
//                    TypeDecl(
//                      22,
//                      AndType(Bot, Bot),
//                      FieldDecl(23, FunType(24, Top, Top))
//                    )
//                  )
//                )
//              ),
//              FieldDecl(13, TypeProj(14, 15))
//            )
//          ),
//          Top
//        ),
//        TypeDecl(11, Bot, Bot)
//      ),
//      7 -> TypeDecl(8, TypeProj(9, 10), TypeProj(9, 10)),
//      3 -> TypeDecl(
//        4,
//        AndType(
//          FunType(5, FieldDecl(6, TypeProj(7, 8)), Bot),
//          AndType(
//            TypeDecl(25, TypeProj(14, 15), TypeProj(14, 15)),
//            FieldDecl(
//              26,
//              TypeDecl(
//                27,
//                TypeDecl(
//                  28,
//                  Bot,
//                  TypeDecl(
//                    29,
//                    FieldDecl(18, TypeProj(19, 20)),
//                    TypeProj(16, 17)
//                  )
//                ),
//                TypeDecl(
//                  28,
//                  Bot,
//                  TypeDecl(
//                    29,
//                    FieldDecl(18, TypeProj(19, 20)),
//                    TypeProj(16, 17)
//                  )
//                )
//              )
//            )
//          )
//        ),
//        FunType(5, FieldDecl(6, TypeProj(7, 8)), Bot)
//      ),
//      16 -> TypeDecl(17, FieldDecl(18, TypeProj(19, 20)), Top),
//      19 -> TypeDecl(20, Top, Top)
//    ),
//    56
//  ),
//  0,
//  AndType(
//    AndType(
//      AndType(TypeProj(3, 4), FunType(39, Bot, Bot)),
//      TypeProj(16, 17)
//    ),
//    FunType(
//      40,
//      FunType(41, TypeProj(9, 10), FunType(42, Bot, Bot)),
//      TypeDecl(
//        43,
//        Bot,
//        TypeDecl(
//          44,
//          Bot,
//          TypeProj(19, 20)
//        )
//      )
//    )
//  ),
//  TypeProj(3, 4)
//)

//(
//  GlobalContext(
//    Map(
//      14 -> TypeDecl(15, TypeProj(16, 17), TypeProj(16, 17)),
//      1 -> AndType(
//        AndType(
//          AndType(TypeProj(3, 4), FunType(39, Bot, Bot)),
//          TypeProj(16, 17)
//        ),
//        FunType(
//          40,
//          FunType(41, TypeProj(9, 10), FunType(42, Bot, Bot)),
//          TypeDecl(
//            43,
//            AndType(
//              AndType(Bot, TypeProj(16, 17)),
//              AndType(
//                TypeDecl(
//                  52,
//                  AndType(Bot, FunType(54, Bot, Bot)),
//                  FunType(53, Top, Bot)
//                ),
//                TypeDecl(55, TypeProj(9, 10), TypeProj(9, 10))
//              )
//            ),
//            TypeDecl(
//              44,
//              AndType(
//                Bot,
//                RecType(
//                  45,
//                  AndType(
//                    FieldDecl(46, Top),
//                    AndType(
//                      FieldDecl(47, Top),
//                      AndType(
//                        AndType(
//                          FieldDecl(48, Top),
//                          FieldDecl(49, TypeProj(7, 8))
//                        ),
//                        FieldDecl(50, FieldDecl(51, TypeProj(7, 8)))
//                      )
//                    )
//                  )
//                )
//              ),
//              TypeProj(19, 20)
//            )
//          )
//        )
//      ),
//      9 -> TypeDecl(
//        10,
//        AndType(
//          AndType(
//            Bot,
//            TypeDecl(
//              12,
//              FieldDecl(
//                13,
//                AndType(
//                  TypeProj(16, 17),
//                  FieldDecl(
//                    21,
//                    TypeDecl(
//                      22,
//                      AndType(Bot, Bot),
//                      FieldDecl(23, FunType(24, Top, Top))
//                    )
//                  )
//                )
//              ),
//              FieldDecl(13, TypeProj(14, 15))
//            )
//          ),
//          Top
//        ),
//        TypeDecl(11, Bot, Bot)
//      ),
//      7 -> TypeDecl(8, TypeProj(9, 10), TypeProj(9, 10)),
//      3 -> TypeDecl(
//        4,
//        AndType(
//          FunType(5, FieldDecl(6, TypeProj(7, 8)), Bot),
//          AndType(
//            TypeDecl(25, TypeProj(14, 15), TypeProj(14, 15)),
//            FieldDecl(
//              26,
//              TypeDecl(
//                27,
//                TypeDecl(
//                  28,
//                  AndType(
//                    AndType(Bot, Bot),
//                    FieldDecl(
//                      30,
//                      FunType(
//                        31,
//                        TypeProj(16, 17),
//                        FunType(
//                          32,
//                          TypeDecl(
//                            33,
//                            TypeDecl(
//                              34,
//                              TypeDecl(35, Bot, Bot),
//                              TypeDecl(35, Bot, Bot)
//                            ),
//                            TypeDecl(
//                              34,
//                              TypeDecl(35, Bot, Bot),
//                              TypeDecl(35, Bot, Bot)
//                            )
//                          ),
//                          FunType(
//                            36,
//                            RecType(37, FieldDecl(38, Bot)),
//                            Bot
//                          )
//                        )
//                      )
//                    )
//                  ),
//                  TypeDecl(
//                    29,
//                    FieldDecl(18, TypeProj(19, 20)),
//                    TypeProj(16, 17)
//                  )
//                ),
//                TypeDecl(
//                  28,
//                  AndType(
//                    AndType(Bot, Bot),
//                    FieldDecl(
//                      30,
//                      FunType(
//                        31,
//                        TypeProj(16, 17),
//                        FunType(
//                          32,
//                          TypeDecl(
//                            33,
//                            TypeDecl(
//                              34,
//                              TypeDecl(35, Bot, Bot),
//                              TypeDecl(35, Bot, Bot)
//                            ),
//                            TypeDecl(
//                              34,
//                              TypeDecl(35, Bot, Bot),
//                              TypeDecl(35, Bot, Bot)
//                            )
//                          ),
//                          FunType(
//                            36,
//                            RecType(37, FieldDecl(38, Bot)),
//                            Bot
//                          )
//                        )
//                      )
//                    )
//                  ),
//                  TypeDecl(
//                    29,
//                    FieldDecl(18, TypeProj(19, 20)),
//                    TypeProj(16, 17)
//                  )
//                )
//              )
//            )
//          )
//        ),
//        FunType(5, FieldDecl(6, TypeProj(7, 8)), Bot)
//      ),
//      16 -> TypeDecl(17, FieldDecl(18, TypeProj(19, 20)), Top),
//      19 -> TypeDecl(20, Top, Top)
//    ),
//    56
//  ),
//  0,
//  AndType(
//    AndType(
//      AndType(
//        FunType(5, FieldDecl(6, TypeProj(7, 8)), Bot),
//        FunType(39, Bot, Bot)
//      ),
//      TypeProj(16, 17)
//    ),
//    FunType(
//      40,
//      FunType(41, TypeProj(9, 10), FunType(42, Bot, Bot)),
//      TypeDecl(
//        43,
//        AndType(
//          AndType(Bot, TypeProj(16, 17)),
//          AndType(
//            TypeDecl(
//              52,
//              AndType(Bot, FunType(54, Bot, Bot)),
//              FunType(53, Top, Bot)
//            ),
//            TypeDecl(55, TypeProj(9, 10), TypeProj(9, 10))
//          )
//        ),
//        TypeDecl(
//          44,
//          AndType(
//            Bot,
//            RecType(
//              45,
//              AndType(
//                FieldDecl(46, Top),
//                AndType(
//                  FieldDecl(47, Top),
//                  AndType(
//                    AndType(FieldDecl(48, Top), FieldDecl(49, TypeProj(7, 8))),
//                    FieldDecl(50, FieldDecl(51, TypeProj(7, 8)))
//                  )
//                )
//              )
//            )
//          ),
//          TypeProj(19, 20)
//        )
//      )
//    )
//  ),
//  AndType(
//    AndType(Top, TypeProj(3, 4)),
//    FunType(
//      40,
//      Bot,
//      TypeDecl(
//        43,
//        AndType(
//          AndType(Bot, TypeProj(16, 17)),
//          AndType(
//            TypeDecl(
//              52,
//              AndType(Bot, FunType(54, Bot, Bot)),
//              FunType(53, Top, Bot)
//            ),
//            TypeDecl(55, TypeProj(9, 10), TypeProj(9, 10))
//          )
//        ),
//        TypeDecl(
//          44,
//          AndType(
//            Bot,
//            RecType(
//              45,
//              AndType(
//                FieldDecl(46, Top),
//                AndType(
//                  FieldDecl(47, Top),
//                  AndType(
//                    AndType(FieldDecl(48, Top), FieldDecl(49, TypeProj(7, 8))),
//                    FieldDecl(50, FieldDecl(51, TypeProj(7, 8)))
//                  )
//                )
//              )
//            )
//          ),
//          Top
//        )
//      )
//    )
//  )
//)

( // TODO
  GlobalContext(
    Map(
      14 -> TypeDecl(15, TypeProj(16, 17), TypeProj(16, 17)),
      1 -> AndType(
        AndType(
          AndType(TypeProj(3, 4), FunType(39, Bot, Bot)),
          TypeProj(16, 17)
        ),
        FunType(
          40,
          FunType(41, TypeProj(9, 10), FunType(42, Bot, Bot)),
          TypeDecl(
            43,
            AndType(
              AndType(Bot, TypeProj(16, 17)),
              AndType(
                TypeDecl(
                  52,
                  AndType(Bot, FunType(54, Bot, Bot)),
                  FunType(53, Top, Bot)
                ),
                TypeDecl(55, TypeProj(9, 10), TypeProj(9, 10))
              )
            ),
            TypeDecl(
              44,
              AndType(
                Bot,
                RecType(
                  45,
                  AndType(
                    FieldDecl(46, Top),
                    AndType(
                      FieldDecl(47, Top),
                      AndType(
                        AndType(
                          FieldDecl(48, Top),
                          FieldDecl(49, TypeProj(7, 8))
                        ),
                        FieldDecl(50, FieldDecl(51, TypeProj(7, 8)))
                      )
                    )
                  )
                )
              ),
              TypeProj(19, 20)
            )
          )
        )
      ),
      9 -> TypeDecl(
        10,
        AndType(
          AndType(
            Bot,
            TypeDecl(
              12,
              FieldDecl(
                13,
                AndType(
                  TypeProj(16, 17),
                  FieldDecl(
                    21,
                    TypeDecl(
                      22,
                      AndType(Bot, Bot),
                      FieldDecl(23, FunType(24, Top, Top))
                    )
                  )
                )
              ),
              FieldDecl(13, TypeProj(14, 15))
            )
          ),
          Top
        ),
        TypeDecl(11, Bot, Bot)
      ),
      7 -> TypeDecl(8, TypeProj(9, 10), TypeProj(9, 10)),
      3 -> TypeDecl(
        4,
        AndType(
          FunType(5, FieldDecl(6, TypeProj(7, 8)), Bot),
          AndType(
            TypeDecl(25, TypeProj(14, 15), TypeProj(14, 15)),
            FieldDecl(
              26,
              TypeDecl(
                27,
                TypeDecl(
                  28,
                  Bot,
                  TypeDecl(
                    29,
                    FieldDecl(18, TypeProj(19, 20)),
                    TypeProj(16, 17)
                  )
                ),
                TypeDecl(
                  28,
                  Bot,
                  TypeDecl(
                    29,
                    FieldDecl(18, TypeProj(19, 20)),
                    TypeProj(16, 17)
                  )
                )
              )
            )
          )
        ),
        FunType(5, FieldDecl(6, TypeProj(7, 8)), Bot)
      ),
      16 -> TypeDecl(17, FieldDecl(18, TypeProj(19, 20)), Top),
      19 -> TypeDecl(20, Top, Top)
    ),
    56
  ),
  0,
  AndType(
    AndType(AndType(TypeProj(3, 4), FunType(39, Bot, Bot)), TypeProj(16, 17)),
    FunType(
      40,
      FunType(41, TypeProj(9, 10), FunType(42, Bot, Bot)),
      TypeDecl(43, Bot, TypeDecl(44, Bot, TypeProj(19, 20)))
    )
  ),
  AndType(
    TypeProj(3, 4),
    FunType(40, Bot, TypeDecl(43, Bot, TypeDecl(44, Bot, Top)))
  )
)

val varASubB = varIsSubtypeOf(scope + (z -> a), z, b)
println(s"(z:a ==> z:b) = $varASubB")
val varBSubA = varIsSubtypeOf(scope + (z -> b), z, a)
println(s"(z:a <== z:b) = $varBSubA")

val aSubB = isSubtypeOf(scope, a, b)
println(s"(a <: b) = $aSubB")
val bSubA = isSubtypeOf(scope, b, a)
println(s"(a >: b) = $bSubA")

val varAEqB = varEqualTypes(scope + (z -> a), z, b)
println(s"varEqualTypes(z:a,b) = $varAEqB")

val aRigidEqualB = rigidEqualTypes(a, b)
println(s"rigidEqualTypes(a, b) = $aRigidEqualB")
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


//(
//  GlobalContext(
//    Map(
//      6 -> TypeDecl(7, Bot, Bot),
//      8 -> TypeDecl(
//        9,
//        FunType(10, Top, Bot),
//        FunType(
//          10,
//          Top,
//          FunType(
//            11,
//            Top,
//            TypeProj(6, 7)
//          )
//        )
//      )
//    ),
//    12
//  ),
//  0,
//  FunType(
//    1,
//    FieldDecl(
//      2,
//      RecType(
//        3,
//        AndType(
//          TypeDecl(4, Top, Top),
//          FieldDecl(
//            5,
//            TypeProj(6, 7)
//          )
//        )
//      )
//    ),
//    TypeProj(8, 9)
//  ),
//  TypeProj(8, 9)
//)


//(
//  GlobalContext(
//    Map(
//      5 -> TypeDecl(6, Top, Top),
//      1 -> TypeDecl(
//        2,
//        AndType(
//          TypeDecl(
//            3,
//            AndType(Bot, FunType(7, TypeProj(5, 6), TypeDecl(8, Top, Top))),
//            TypeDecl(4, TypeProj(5, 6), TypeProj(5, 6))
//          ),
//          RecType(9, AndType(FieldDecl(10, Top), FieldDecl(11, Top)))
//        ),
//        TypeDecl(
//          3,
//          AndType(Bot, FunType(7, TypeProj(5, 6), TypeDecl(8, Top, Top))),
//          TypeDecl(4, TypeProj(5, 6), TypeProj(5, 6))
//        )
//      )
//    ),
//    12
//  ),
//  0,
//  AndType(
//    TypeDecl(
//      3,
//      AndType(Bot, FunType(7, TypeProj(5, 6), TypeDecl(8, Top, Top))),
//      TypeDecl(4, TypeProj(5, 6), TypeProj(5, 6))
//    ),
//    RecType(9, AndType( FieldDecl(10, Top), FieldDecl(11, Top)))
//  ),
//  TypeProj(1, 2)
//)

//(
//  GlobalContext(Map(1 -> TypeDecl(2, Bot, Bot)), 5),
//  0,
//  TypeProj(1, 2),
//  RecType(3, FieldDecl(4, TypeProj(1, 2)))
//)

//(
//  GlobalContext(Map(1 -> TypeDecl(2, Top, Top)), 5),
//  0,
//  RecType(3, RecType(4, Top)),
//  TypeProj(1, 2)
//)

( // TODO need symboluniverse...
  GlobalContext(
    Map(
      8 -> TypeDecl(9, Bot, Bot),
      18 -> TypeDecl(19, Bot, RecType(20, TypeProj(8, 9))),
      3 -> TypeDecl(
        4,
        RecType(
          5,
          AndType(
            TypeDecl(
              6,
              AndType(
                AndType(
                  TypeProj(8, 9),
                  RecType(
                    10,
                    AndType(
                      FieldDecl(11, Top),
                      TypeDecl(12, Bot, Bot)
                    )
                  )
                ),
                Bot
              ),
              FunType(7, Bot, Bot)
            ),
            AndType(
              AndType(
                AndType(
                  FieldDecl(13, RecType(14, Top)),
                  FieldDecl(15, Bot)
                ),
                AndType(
                  TypeDecl(16, Bot, Bot),
                  AndType(
                    FieldDecl(17, TypeProj(18, 19)),
                    FieldDecl(21, Bot)
                  )
                )
              ),
              FieldDecl(22, TypeProj(18, 19))
            )
          )
        ),
        RecType(
          5,
          AndType(
            TypeDecl(
              6,
              AndType(
                AndType(
                  TypeProj(8, 9),
                  RecType(
                    10,
                    AndType(
                      FieldDecl(11, Top),
                      TypeDecl(12, Bot, Bot)
                    )
                  )
                ),
                Bot
              ),
              FunType(7, Bot, Bot)
            ),
            AndType(
              AndType(
                AndType(
                  FieldDecl(13, RecType(14, Top)),
                  FieldDecl(15, Bot)
                ),
                AndType(
                  TypeDecl(16, Bot, Bot),
                  AndType(
                    FieldDecl(17, TypeProj(18, 19)),
                    FieldDecl(21, Bot)
                  )
                )
              ),
              FieldDecl(22, TypeProj(18, 19))
            )
          )
        )
      ),
      1 -> TypeDecl(
        2,
        AndType(TypeProj(3, 4), RecType(23, TypeProj(3, 4))),
        TypeProj(3, 4)
      )
    ),
    24
  ),
  0,
  TypeProj(1, 2),
  TypeProj(1, 2)
)

def lub(left: Type, right: Type) = leastCommonSupertype(scope, left, right)
def glb(left: Type, right: Type) = greatestCommonSubtype(scope, left, right)



val lub_ab = lub(a, b)
P.namedln("lub(a, b)", lub_ab)

val lub_ba = lub(b, a)
P.namedln("lub(b, a)", lub_ba)

val glb_ab = glb(a, b)
P.namedln("glb(a, b)", glb_ab)

val lubRes = lub(a, glb_ab)
P.namedln("lub(a, glb(a, b))", lubRes)

val glbRes = glb(a, lub_ab)
P.namedln("glb(lub(a, b), a)", glbRes)

P.namedln("glb(a, b) == a", varEqualTypes(scope + (z -> glb_ab), z, a))
P.namedln("glb(a, b) == b", varEqualTypes(scope + (z -> glb_ab), z, b))

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


//(
//  GlobalContext(
//    Map(
//      10 -> TypeDecl(
//        11,
//        Bot,
//        FunType(
//          12,
//          FieldDecl(13, Bot),
//          Bot
//        )
//      ),
//      5 -> TypeDecl(
//        6,
//        Bot,
//        FunType(
//          7,
//          FieldDecl(
//            8,
//            FieldDecl(9, Bot)
//          ),
//          TypeProj(10, 11)
//        )
//      ),
//      1 -> TypeDecl(
//        2,
//        FunType(3, Bot, Bot),
//        FunType(
//          3,
//          Bot,
//          FieldDecl(
//            4,
//            TypeProj(5, 6)
//          )
//        )
//      )
//    ),
//    14
//  ),
//  4711,
//  FunType(
//    0,
//    TypeProj(1, 2),
//    TypeProj(5, 6)
//  ),
//  TypeProj(5, 6),
//  TypeProj(10, 11)
//)

//(
//  GlobalContext(
//    Map(
//      42 -> TypeDecl(
//        43,
//        FunType(39, TypeProj(40, 41), Top),
//        FunType(39, TypeProj(40, 41), Top)
//      ),
//      24 -> TypeDecl(
//        25,
//        FieldDecl(21, AndType(Bot, TypeProj(15, 17))),
//        FieldDecl(21, Bot)
//      ),
//      9 -> TypeDecl(
//        10,
//        AndType(
//          Bot,
//          FieldDecl(
//            11,
//            FieldDecl(12, FieldDecl(13, FunType(14, Top, Top)))
//          )
//        ),
//        Bot
//      ),
//      18 -> TypeDecl(19, Bot, Bot),
//      40 -> TypeDecl(41, TypeProj(24, 25), Top),
//      36 -> TypeDecl(
//        37,
//        FieldDecl(21, AndType(Bot, TypeProj(15, 17))),
//        TypeProj(24, 25)
//      )
//    ),
//    44
//  ),
//  0,
//  TypeDecl(
//    1,
//    AndType(TypeProj(9, 10), TypeProj(9, 10)),
//    TypeDecl(
//      2,
//      Bot,
//      AndType(
//        AndType(TypeDecl(3, Bot, Bot), TypeDecl(4, Bot, Bot)),
//        AndType(
//          AndType(
//            FieldDecl(5, Bot),
//            AndType(TypeDecl(6, Bot, Top), FieldDecl(7, Bot))
//          ),
//          FieldDecl(8, Top)
//        )
//      )
//    )
//  ),
//  RecType(
//    15,
//    AndType(
//      AndType(
//        FieldDecl(16, Bot),
//        AndType(
//          FieldDecl(16, Bot),
//          AndType(
//            TypeDecl(17, TypeProj(18, 19), TypeProj(18, 19)),
//            TypeDecl(
//              20,
//              TypeProj(24, 25),
//              FieldDecl(
//                21,
//                FunType(22, Top, FunType(23, TypeProj(15, 17), Top))
//              )
//            )
//          )
//        )
//      ),
//      AndType(
//        FieldDecl(26, Bot),
//        FieldDecl(
//          27,
//          RecType(
//            28,
//            FieldDecl(
//              29,
//              FunType(
//                30,
//                FieldDecl(
//                  31,
//                  FunType(
//                    32,
//                    Top,
//                    FunType(
//                      33,
//                      TypeProj(9, 10),
//                      FunType(34, Bot, Bot)
//                    )
//                  )
//                ),
//                TypeProj(15, 17)
//              )
//            )
//          )
//        )
//      )
//    )
//  ),
//  AndType(
//    TypeDecl(35, TypeProj(36, 37), Top),
//    TypeDecl(
//      38,
//      TypeProj(42, 43),
//      FunType(39, TypeProj(40, 41), Top)
//    )
//  )
//)

(
  GlobalContext(
    Map(
      10 -> TypeDecl(11, Bot, TypeProj(12, 13)),
      29 -> TypeDecl(
        30,
        AndType(AndType(Bot, Bot), TypeProj(35, 36)),
        FunType(
          31,
          FunType(32, TypeProj(33, 34), TypeProj(35, 36)),
          Top
        )
      ),
      1 -> TypeDecl(2, TypeProj(3, 4), Top),
      33 -> TypeDecl(34, Bot, Top),
      27 -> TypeDecl(28, Top, Top),
      12 -> TypeDecl(
        13,
        AndType(
          AndType(AndType(Bot, Bot), Top),
          AndType(
            TypeDecl(
              39,
              FieldDecl(40, TypeProj(29, 30)),
              FieldDecl(40, TypeProj(29, 30))
            ),
            TypeDecl(
              41,
              AndType(
                Bot,
                FieldDecl(45, TypeDecl(46, Bot, TypeProj(29, 30)))
              ),
              AndType(
                TypeDecl(42, Bot, Bot),
                TypeDecl(43, Bot, FunType(44, Top, Bot))
              )
            )
          )
        ),
        TypeDecl(
          14,
          AndType(
            Bot,
            FieldDecl(
              18,
              FunType(
                19,
                FunType(
                  20,
                  TypeDecl(
                    21,
                    FieldDecl(22, Bot),
                    FieldDecl(22, TypeDecl(23, Bot, Bot))
                  ),
                  FunType(
                    24,
                    RecType(25, FieldDecl(26, TypeProj(27, 28))),
                    TypeProj(29, 30)
                  )
                ),
                RecType(37, TypeDecl(38, Bot, Top))
              )
            )
          ),
          RecType(
            15,
            AndType(FieldDecl(16, Top), FieldDecl(17, Top))
          )
        )
      ),
      3 -> TypeDecl(
        4,
        AndType(
          AndType(
            TypeDecl(5, Bot, Top),
            FieldDecl(6, FieldDecl(7, TypeProj(8, 9)))
          ),
          Top
        ),
        AndType(
          TypeDecl(5, Bot, Top),
          FieldDecl(6, FieldDecl(7, TypeProj(8, 9)))
        )
      ),
      35 -> TypeDecl(36, Bot, Bot),
      8 -> TypeDecl(9, AndType(Top, TypeProj(10, 11)), Top)
    ),
    48
  ),
  0,
    AndType(
      TypeDecl(5, Bot, Top),
      FieldDecl(6, FieldDecl(7, TypeProj(8, 9)))
    ),
  AndType(TypeProj(1, 2), FunType(47, Bot, TypeProj(8, 9))),
  TypeProj(1, 2)
)

def lub(left: Type, right: Type) = leastCommonSupertype(scope, left, right)

val ab   = lub(a, b)
P.namedln("lub(a, b)", ab)
//val bc   = lub(b, c)
//P.namedln("lub(b, c)", bc)
//val a_bc = lub(a, bc)
//P.namedln("lub(a, lub(b, c))", a_bc)
//val ab_c = lub(ab, c)
//P.namedln("lub(lub(a, b), c)", ab_c)
//
//P.namedln("lub(a, lub(b, c)) == lub(lub(a, b), c)", varEqualTypes(scope + (z -> a_bc), z, ab_c))

P.namedln("lub(a, b) <: c", varIsSubtypeOf(scope + (z -> ab), z, c))
}


def debugproj(): Unit = {

val (GlobalContext(scope, nextSymbol), z, x, a, aLowerType): (GlobalContext, Symbol, Symbol, Symbol, Type) =

(
  GlobalContext(
    Map(
      5 -> TypeDecl(
        6,
        Bot,
        RecType(
          7,
          AndType(TypeDecl(8, Top, Top), FieldDecl(9, Bot))
        )
      ),
      13 -> TypeDecl(
        14,
        FunType(
          15,
          RecType(
            7,
            AndType(TypeDecl(8, Top, Top), FieldDecl(9, Bot))
          ),
          AndType(Bot, TypeProj(15, 8))
        ),
        FunType(
          15,
          TypeProj(5, 6),
          TypeDecl(16, FunType(17, Bot, Bot), FunType(17, Bot, Bot))
        )
      ),
      24 -> TypeDecl(25, Bot, TypeProj(13, 14)),
      1 -> TypeDecl(
        2,
        FunType(
          3,
          TypeDecl(4, TypeProj(5, 6), TypeProj(5, 6)),
          RecType(
            10,
            AndType(
              FieldDecl(
                11,
                TypeDecl(
                  12,
                  FunType(15, Top, AndType(Bot, TypeProj(15, 8))), // NOTE: Top is not a subtype of any TypeDecl. The bug is in the generator.
                  TypeProj(13, 14)
                )
              ),
              AndType(
                FieldDecl(
                  18,
                  FieldDecl(
                    19,
                    FunType(
                      20,
                      TypeDecl(21, Bot, Bot),
                      RecType(22, FieldDecl(23, TypeProj(24, 25)))
                    )
                  )
                ),
                FieldDecl(26, Bot)
              )
            )
          )
        ),
        FunType(3, TypeDecl(4, TypeProj(5, 6), TypeProj(5, 6)), Top)
      )
    ),
    27
  ),
  0,
  1,
  2,
  FunType(
    3,
    TypeDecl(4, TypeProj(5, 6), TypeProj(5, 6)),
    RecType(
      10,
      AndType(
        FieldDecl(
          11,
          TypeDecl(
            12,
            FunType(15, Top, AndType(Bot, TypeProj(15, 8))),
            TypeProj(13, 14)
          )
        ),
        AndType(
          FieldDecl(
            18,
            FieldDecl(
              19,
              FunType(
                20,
                TypeDecl(21, Bot, Bot),
                RecType(22, FieldDecl(23, TypeProj(24, 25)))
              )
            )
          ),
          FieldDecl(26, Bot)
        )
      )
    )
  )
)

val res = NoFuture.typeProjectLower(scope, x, a)
P.namedln("res", res)

P.namedln("res == expected", varEqualTypes(scope + (z -> res.get), z, aLowerType))

}






def debugelim(): Unit = {
val (GlobalContext(scope, nextSymbol), localScope, killSet, z, a): (GlobalContext, Scope, Set[Symbol], Symbol, Type) =

( // TODO fun(x:rec)x.T --> fun(x: Top)x.T
  GlobalContext(
    Map(),
    20),
  Map(1 -> TypeDecl(4, Bot, FunType(3, Top, Top))),
  Set(1),
  0,
  FunType(3, RecType(5, TypeDecl(4, FunType(3, Top, Bot), TypeProj(1, 4))), TypeProj(3, 4))
)

//(
//  GlobalContext(Map(7 -> TypeDecl(8, Bot, Bot)), 11),
//  Map(
//    1 -> FunType(2, TypeDecl(3, Bot, Bot), FunType(4, TypeProj(2, 3), Top))
//  ),
//  Set(1),
//  0,
//  AndType(
//    RecType(
//      5,
//      AndType(
//        TypeDecl(6, TypeProj(7, 8), TypeProj(7, 8)),
//        FieldDecl(9, TypeProj(5, 6))
//      )
//    ),
//    FieldDecl(10, Bot)
//  )
//  //RecType(
//  //  5,
//  //  AndType(
//  //    TypeDecl(6, TypeProj(7, 8), TypeProj(7, 8)),
//  //    FieldDecl(9, TypeProj(5, 6))
//  //  )
//  //)
//)

  val res = eliminateVars(scope, killSet, Some(z), a, variance=Contravariant)
  P.namedln("res", res)

  P.namedln("valid(res)", NoFuture.validTypeInScope(scope, res))
}















} // end object Main
