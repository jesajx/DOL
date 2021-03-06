package exjobb

import Dol._

import DolGenerators._
import Pretty._

import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import scala.annotation.tailrec

/** For debugging. */
object TestMain {
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
//debugelim()
//debugtc()
debugpartc()
//debuggen()
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

//(
//  GlobalContext(
//    Map(0 -> Top, 1 -> TypeProj(2, 3), 2 -> TypeDecl(3, Top, Top), 6 -> TypeDecl(7, Top, Top)),
//    8
//  ),
//  -2,
//  4,
//  RecType(5, TypeProj(6, 7)),
//  Que
//)


//(
//  GlobalContext(
//    Map(
//      10 -> TypeDecl(11, Top, Top),
//      8 -> TypeDecl(9, TypeProj(10, 11), TypeProj(10, 11)),
//2 -> RecType(
//      3,
//      RecType(
//        4,
//        TypeDecl(
//          5,
//          AndType(
//            FieldDecl(6, Bot),
//            TypeDecl(7, TypeProj(10, 11), TypeProj(8, 9))
//          ),
//          AndType(
//            FieldDecl(6, Bot),
//            TypeDecl(7, TypeProj(10, 11), TypeProj(8, 9))
//          )
//        )
//      )
//    )
//    ),
//    16
//  ),
//  1,
//  0,
//  AndType(
//    AndType(
//      FunType(
//        12,
//        TypeDecl(13, Bot, Bot),
//        RecType(14, AndType(Bot, TypeProj(12, 13)))
//      ),
//      Bot
//    ),
//    FunType(15, Top, Bot)
//  ),
//  AndType(Que, FunType(15, Top, Que))
//  //Que
//)

//(
//  GlobalContext(
//    Map(
//      4 -> TypeDecl(5, Bot, Bot),
//      1 -> AndType(
//        FunType(3, TypeProj(4, 5), AndType(Bot, Top)),
//        FunType(6, Bot, Bot)
//      )
//    ),
//    7
//  ),
//  2,
//  0,
//  AndType(
//    FunType(3, TypeProj(4, 5), AndType(Bot, Top)),
//    FunType(6, Bot, Bot)
//  ),
//  AndType(
//    FunType(3, TypeProj(4, 5), AndType(Bot, Top)),
//    FunType(6, Bot, Bot)
//  )
//  //AndType(Que, FunType(6, Bot, Que))
//)

//(
//  GlobalContext(
//    Map(
//      14 -> TypeDecl(15, Bot, Bot),
//      11 -> TypeDecl(12, TypeProj(14, 15), TypeDecl(13, Bot, Bot)),
//      8 -> TypeDecl(9, Bot, RecType(10, TypeProj(11, 12))),
//    2 -> FunType(3, Top, Bot),
//    4 -> TypeDecl(5, AndType(AndType(Bot, Bot), FieldDecl(6, Top)), Bot)
//    ),
//    19
//  ),
//  1,
//  0,
//  AndType(
//    FunType(7, TypeProj(4, 5), TypeProj(8, 9)),
//    FunType(16, RecType(17, FieldDecl(18, Top)), Bot)
//  ),
//  //AndType(Que, FunType(16, RecType(17, FieldDecl(18, Top)), Que))
//  AndType(Que, FunType(16, RecType(17, FieldDecl(18, Top)), Que))
//)

(
  GlobalContext(
    Map(
      11 -> TypeDecl(12, Bot, Top),
      4 -> TypeDecl(
        5,
        Bot,
        RecType(
          6,
          RecType(
            7,
            AndType(
              FieldDecl(8, FieldDecl(9, Bot)),
              AndType(
                FieldDecl(10, TypeProj(11, 12)),
                FieldDecl(13, Bot)
              )
            )
          )
        )
      ),
  2 -> AndType(TypeDecl(3, Bot, Bot), TypeProj(4, 5))
    ),
    20
  ),
  1,
  0,
  FunType(
    14,
    Bot,
    AndType(
      RecType(15, RecType(16, Top)),
      RecType(17, RecType(18, RecType(19, Top)))
    )
  ),
  FunType(
    14,
    Bot,
    AndType(Que, RecType(17, RecType(18, RecType(19, Top))))
  )
)

//(
//  GlobalContext(
//    Map(
//    ),
//    20
//  ),
//  1,
//  0,
//  RecType(2,
//    FunType(
//      3,
//      Bot,
//      Top
//    )
//  ),
//  FunType( // NOTE: not allowed since lower always uses zOption=None.
//    3,
//    Bot,
//    Top
//  )
//)

//P.namedln("simplify(p)", simplify(p))

val scope = scope1 + (z -> a)

//val res1 = NoFuture.varLower(scope, r, z, p)
//P.namedln("res1", res1)


//val (numQues, labeledPrototype) = prepMatch(r, simplify(unwrapRecTypes(p, z)))
val (numQues, labeledPrototype) = prepMatch(r, simplify(p))

//P.namedln("labeledPrototype", labeledPrototype)

val solveSet = (0 until numQues).map{TypeProj(r, _)}.toSet
val solveSetVariance = gatherVariance(solveSet, labeledPrototype, Contravariant)


//val constraint = gatherConstraints(scope, solveSet, Some(z), labeledPrototype, a) // NOTE: varLower never makes sense. since proving x:T ==> x:D is done through supertyping in DOT (T <: D), not the other way around (D <: T).
val constraint = gatherConstraints(scope, solveSet, None, labeledPrototype, a, patternIsLeft=true)


P.namedln("constraint", constraint)

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
val res2 = solveConstraint(scope, None, solveSet, solveSetVariance, constraint, labeledPrototype, Contravariant)
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


//( // TODO fun(x: Que)x.T
//  GlobalContext(
//    Map(
//      11 -> TypeDecl(12, Bot, Bot),
//      9 -> TypeDecl(10, TypeProj(11, 12), TypeProj(11, 12)),
//      14 -> TypeDecl(15, Bot, Bot),
//      0 -> Top,
//      1 -> Top,
//      6 -> FunType(7, Bot, FieldDecl(8, TypeProj(9, 10))),
//      13 -> TypeProj(14, 15),
//      2 -> RecType(3, FieldDecl(4, TypeDecl(5, Top, Top))),
//      16 -> Bot
//    ),
//    29
//  ),
//  18,
//  17,
//  FunType(
//    19,
//    AndType(
//      FieldDecl(20, TypeDecl(21, Top, Top)),
//      TypeDecl(
//        22,
//        Bot,
//        AndType(FieldDecl(23, TypeProj(14, 15)), Bot)
//      )
//    ),
//    AndType(
//      FunType(24, TypeProj(19, 22), Bot),
//      FunType(
//        25,
//        FunType(26, RecType(27, Bot), TypeProj(11, 12)),
//        RecType(28, Bot)
//      )
//    )
//  ),
//  FunType(
//    19,
//    Que,
//    AndType(
//      FunType(24, TypeProj(19, 22), Bot),
//      FunType(25, Que, RecType(28, Bot))
//    )
//  )
//)

//(
//  GlobalContext(
//    Map(
//      0 -> AndType(
//        RecType(1, RecType(2, FieldDecl(3, TypeProj(4, 5)))),
//        TypeProj(7, 8)
//      ),
//      4 -> TypeDecl(5, Bot, RecType(6, Bot)),
//      7 -> TypeDecl(8, Bot, TypeProj(4, 5))
//    ),
//    11
//  ),
//  9,
//  10,
//  AndType(
//        RecType(1, RecType(2, FieldDecl(3, TypeProj(4, 5)))),
//        TypeProj(7, 8)
//      ),
//  FieldDecl(3, Que)
//)

//(
//  GlobalContext(
//    Map(
//      1 -> FieldDecl(
//        2,
//        AndType(
//          Bot,
//          RecType(
//            3,
//            AndType(
//              TypeDecl(4, Bot, Bot),
//              FieldDecl(5, RecType(6, TypeProj(7, 8)))
//            )
//          )
//        )
//      ),
//      7 -> TypeDecl(8, Bot, Bot)
//    ),
//    14
//  ),
//  12,
//  13,
//  FieldDecl(
//        2,
//        AndType(
//          Bot,
//          RecType(
//            3,
//            AndType(
//              TypeDecl(4, Bot, Bot),
//              FieldDecl(5, RecType(6, TypeProj(7, 8)))
//            )
//          )
//        )
//      ),
//  FieldDecl(2, Que)
//)

//(
//  GlobalContext(
//    Map(
//      2 -> AndType(
//        AndType(
//          FieldDecl(
//            3,
//            FunType(5, TypeProj(6, 7), FieldDecl(10, Bot))
//          ),
//          TypeDecl(
//            11,
//            RecType(12, TypeProj(6, 7)),
//            RecType(12, TypeProj(6, 7))
//          )
//        ),
//        AndType(
//          AndType(
//            TypeDecl(13, TypeProj(6, 7), TypeProj(6, 7)),
//            TypeDecl(14, TypeProj(2, 13), TypeProj(2, 13))
//          ),
//          FieldDecl(
//            15,
//            RecType(
//              16,
//              AndType(
//                TypeDecl(17, Bot, Bot),
//                FieldDecl(
//                  18,
//                  FunType(
//                    24,
//                    AndType(
//                      FieldDecl(25, Top),
//                      TypeDecl(26, Bot, TypeProj(2, 13))
//                    ),
//                    AndType(Top, TypeProj(31, 32))
//                  )
//                )
//              )
//            )
//          )
//        )
//      ),
//      6 -> TypeDecl(
//        7,
//        FieldDecl(8, Bot),
//        FieldDecl(8, FieldDecl(9, AndType(Top, Top)))
//      ),
//      31 -> TypeDecl(32, TypeProj(6, 7), TypeProj(6, 7))
//    ),
//    35
//  ),
//  33,
//  34,
//  AndType(
//        AndType(
//          FieldDecl(
//            3,
//            FunType(5, TypeProj(6, 7), FieldDecl(10, Bot))
//          ),
//          TypeDecl(
//            11,
//            RecType(12, TypeProj(6, 7)),
//            RecType(12, TypeProj(6, 7))
//          )
//        ),
//        AndType(
//          AndType(
//            TypeDecl(13, TypeProj(6, 7), TypeProj(6, 7)),
//            TypeDecl(14, TypeProj(2, 13), TypeProj(2, 13))
//          ),
//          FieldDecl(
//            15,
//            RecType(
//              16,
//              AndType(
//                TypeDecl(17, Bot, Bot),
//                FieldDecl(
//                  18,
//                  FunType(
//                    24,
//                    AndType(
//                      FieldDecl(25, Top),
//                      TypeDecl(26, Bot, TypeProj(2, 13))
//                    ),
//                    AndType(Top, TypeProj(31, 32))
//                  )
//                )
//              )
//            )
//          )
//        )
//      ),
//  Que
//)

//(
//  GlobalContext(
//    Map(
//      5 -> TypeDecl(6, TypeProj(7, 8), Top),
//      25 -> TypeDecl(26, Bot, Bot),
//      13 -> TypeDecl(14, TypeProj(15, 16), TypeProj(15, 16)),
//      17 -> TypeDecl(18, Bot, Bot),
//      7 -> TypeDecl(
//        8,
//        FunType(
//          9,
//          TypeDecl(10, Bot, TypeProj(11, 12)),
//          RecType(19, Bot)
//        ),
//        Top
//      ),
//      11 -> TypeDecl(12, Bot, TypeProj(13, 14)),
//      15 -> TypeDecl(16, Bot, TypeProj(17, 18)),
//      2 -> RecType(
//        3,
//        AndType(
//          TypeDecl(
//            4,
//            AndType(
//              AndType(Bot, Top),
//              AndType(
//                FieldDecl(20, TypeProj(11, 12)),
//                TypeDecl(21, Bot, TypeProj(11, 12))
//              )
//            ),
//            TypeProj(5, 6)
//          ),
//          FieldDecl(
//            22,
//            FunType(23, Top, RecType(24, TypeProj(25, 26)))
//          )
//        )
//      )
//    ),
//    32
//  ),
//  1,
//  0,
//  FunType(
//    27,
//    AndType(TypeProj(7, 8), FunType(28, Top, FunType(29, Top, Bot))),
//    AndType(
//      RecType(
//        30,
//        TypeDecl(
//          31,
//          AndType(Bot, Bot),
//          AndType(TypeProj(7, 8), Top)
//        )
//      ),
//      Top
//    )
//  ),
//  FunType(27, AndType(Que, FunType(28, Top, Que)), Que)
//)

//(
//  GlobalContext(
//    Map(
//      8 -> TypeDecl(9, Bot, RecType(10, Bot)),
//      20 -> TypeDecl(21, Top, Top),
//      18 -> TypeDecl(
//        19,
//        AndType(TypeProj(20, 21), RecType(22, TypeProj(20, 21))),
//        AndType(TypeProj(20, 21), RecType(22, TypeProj(20, 21)))
//      ),
//    2 -> Top,
//    3 -> RecType(
//      4,
//      FunType(5, Bot, RecType(6, FieldDecl(7, TypeProj(8, 9))))
//    ),
//    11 -> RecType(12, Top)
//    ),
//    23
//  ),
//  1,
//  0,
//  FunType(
//    13,
//    AndType(
//      FunType(14, Bot, Bot),
//      FunType(15, Top, AndType(TypeProj(8, 9), Top))
//    ),
//    RecType(16, FieldDecl(17, AndType(Bot, TypeProj(18, 19))))
//  ),
//  FunType(
//    13,
//    AndType(FunType(14, Que, Que), Que),
//    RecType(16, FieldDecl(17, AndType(Bot, TypeProj(18, 19))))
//  )
//)

//(
//  GlobalContext(
//    Map(
//      25 -> TypeDecl(26, TypeProj(27, 28), AndType(Bot, Top)),
//      21 -> TypeDecl(22, Bot, Bot),
//      27 -> TypeDecl(28, Bot, AndType(Bot, Bot)),
//      7 -> TypeDecl(8, Bot, Bot),
//      23 -> TypeDecl(24, Bot, TypeProj(25, 26)),
//      4 -> TypeDecl(5, Bot, Top),
//      15 -> TypeDecl(16, Top, Top),
//    2 -> AndType(
//      Bot,
//      FunType(3, TypeProj(4, 5), FunType(6, Top, TypeProj(7, 8)))
//    )
//    ),
//    29
//  ),
//  1,
//  0,
//  //TypeProj(23, 24),
//  FunType(
//    9,
//    AndType(
//      RecType(
//        10,
//        FunType(11, TypeDecl(12, Bot, Bot), TypeProj(11, 12))
//      ),
//      FunType(
//        13,
//        AndType(
//          RecType(14, TypeProj(15, 16)),
//          AndType(Top, TypeProj(15, 16))
//        ),
//        TypeDecl(17, AndType(Bot, TypeProj(7, 8)), Bot)
//      )
//    ),
//    RecType(
//      18,
//      AndType(
//        FunType(19, Bot, Bot),
//        AndType(FieldDecl(20, TypeProj(21, 22)), Top)
//      )
//    )
//  ),
//  FunType(
//    9,
//    AndType(Que, FunType(13, Que, Que)),
//    RecType(
//      18,
//      AndType(
//        FunType(19, Bot, Bot),
//        AndType(FieldDecl(20, TypeProj(21, 22)), Top)
//      )
//    )
//  )
//)

//(
//  GlobalContext(
//    Map(
//      5 -> TypeDecl(6, TypeProj(7, 8), TypeProj(7, 8)),
//      33 -> TypeDecl(
//        34,
//        AndType(Bot, TypeProj(31, 32)),
//        AndType(
//          Bot,
//          FieldDecl(
//            28,
//            TypeDecl(
//              29,
//              TypeProj(31, 32),
//              AndType(Bot, RecType(30, TypeProj(18, 19)))
//            )
//          )
//        )
//      ),
//      9 -> TypeDecl(10, Bot, Top),
//      12 -> TypeDecl(13, Bot, Top),
//      7 -> TypeDecl(8, Bot, TypeProj(9, 10)),
//      18 -> TypeDecl(19, Bot, TypeProj(12, 13)),
//      31 -> TypeDecl(32, Bot, Bot),
//      26 -> TypeDecl(
//        27,
//        TypeProj(33, 34),
//        AndType(
//          AndType(Top, Bot),
//          FieldDecl(
//            28,
//            TypeDecl(
//              29,
//              TypeProj(31, 32),
//              AndType(Bot, RecType(30, TypeProj(18, 19)))
//            )
//          )
//        )
//      ),
//    2 -> RecType(
//      3,
//      AndType(
//        TypeDecl(
//          4,
//          AndType(AndType(Bot, TypeProj(7, 8)), TypeProj(9, 10)),
//          AndType(TypeProj(5, 6), Bot)
//        ),
//        FieldDecl(11, TypeProj(12, 13))
//      )
//    ),
//    14 -> AndType(TypeProj(5, 6), Top)
//    ),
//    35
//  ),
//  1,
//  0,
//  //FunType(
//  //  15,
//  //  AndType(
//  //    RecType(16, FunType(17, TypeProj(18, 19), TypeProj(18, 19))),
//  //    FunType(
//  //      20,
//  //      AndType(
//  //        FieldDecl(21, AndType(Top, FieldDecl(22, Bot))),
//  //        FunType(
//  //          23,
//  //          AndType(FieldDecl(24, Top), Top),
//  //          FieldDecl(25, Bot)
//  //        )
//  //      ),
//  //      TypeProj(12, 13)
//  //    )
//  //  ),
//  //  TypeProj(26, 27)
//  //),
//  FunType(
//    15,
//    AndType(
//      RecType(16, FunType(17, TypeProj(18, 19), TypeProj(18, 19))),
//      FunType(20, AndType(Que, Que), Que)
//    ),
//    TypeProj(26, 27)
//  ),
//  FunType(15, AndType(Que, FunType(20, Que, Que)), TypeProj(26, 27))
//)

//(
//  GlobalContext(
//    Map(
//    ),
//    20
//  ),
//  1,
//  0,
//  AndType(
//    FieldDecl(7, Top),
//    AndType(
//      FieldDecl(8, Top),
//      FunType(
//        3,
//        Bot,
//        Top
//      )
//    )
//  ),
//  RecType(2,
//    AndType(
//      FieldDecl(7, Top),
//      FunType(
//        3,
//        Bot,
//        Top
//      )
//    )
//  )
//)

//( // TODO
//  GlobalContext(
//    Map(
//    2 -> RecType(
//      3,
//      AndType(
//        TypeDecl(4, Bot, Bot),
//        AndType(
//          AndType(
//            FieldDecl(5, RecType(6, Top)),
//            FieldDecl(7, TypeProj(3, 4))
//          ),
//          AndType(FieldDecl(8, Bot), FieldDecl(9, Bot))
//        )
//      )
//    )
//    ),
//  14),
//  1,
//  0,
//  AndType(
//    FunType(10, Top, RecType(11, Bot)),
//    FunType(12, Top, FunType(13, Bot, Top))
//  ),
//  //FunType(10, Top, RecType(11, Bot))
//  FunType(10, Que, Que) // TODO "inconsistent"
//)

//( // TODO
//  GlobalContext(
//    Map(
//      8 -> TypeDecl(9, Bot, Top),
//      10 -> TypeDecl(
//        11,
//        AndType(AndType(Bot, FieldDecl(15, Bot)), Top),
//        AndType(FieldDecl(12, Bot), FieldDecl(13, RecType(14, Bot)))
//      ),
//      5 -> TypeDecl(
//        6,
//        AndType(Bot, TypeProj(10, 11)),
//        FunType(7, TypeProj(8, 9), TypeProj(10, 11))
//      ),
//      20 -> TypeDecl(21, Bot, Bot),
//    2 -> AndType(TypeDecl(3, Bot, Bot), FieldDecl(4, TypeProj(5, 6))),
//    16 -> TypeProj(5, 6)
//    ),
//    22
//  ),
//  1,
//  0,
//  AndType(
//    FunType(17, Top, RecType(18, Top)),
//    FunType(19, AndType(Bot, TypeProj(20, 21)), Bot)
//  ),
//  //FunType(17, Top, RecType(18, Top))
//  FunType(17, Que, Que) // TODO "inconsistent"
//)

//( // TODO
//  GlobalContext(
//    Map(
//      6 -> TypeDecl(7, Bot, Bot),
//      12 -> TypeDecl(13, Bot, Bot),
//      10 -> TypeDecl(11, TypeProj(12, 13), Bot),
//      2 -> Top
//    ),
//    15
//  ),
//  1,
//  0,
//  AndType(
//    FunType(
//      3,
//      AndType(
//        FieldDecl(4, Top),
//        TypeDecl(5, TypeProj(6, 7), TypeProj(6, 7))
//      ),
//      AndType(
//        FieldDecl(8, TypeProj(6, 7)),
//        FieldDecl(9, TypeProj(10, 11))
//      )
//    ),
//    FunType(14, TypeProj(10, 11), TypeProj(10, 11))
//  ),
//  //FunType(
//  //  3,
//  //  AndType(
//  //    FieldDecl(4, Top),
//  //    TypeDecl(5, TypeProj(6, 7), TypeProj(6, 7))
//  //  ),
//  //  AndType(
//  //    FieldDecl(8, TypeProj(6, 7)),
//  //    FieldDecl(9, TypeProj(10, 11))
//  //  )
//  //),
//  FunType(3, Que, AndType(Que, FieldDecl(9, Que))) // TODO "inconsistent"
//)


//( // TODO
//  GlobalContext(
//    Map(
//      25 -> TypeDecl(
//        26,
//        AndType(
//          AndType(Bot, Bot),
//          RecType(27, TypeDecl(28, Bot, TypeProj(18, 19)))
//        ),
//        AndType(
//          AndType(Bot, Bot),
//          RecType(27, TypeDecl(28, Bot, TypeProj(18, 19)))
//        )
//      ),
//      21 -> TypeDecl(
//        22,
//        TypeProj(23, 24),
//        AndType(
//          FunType(
//            5,
//            FunType(6, TypeProj(7, 8), Bot),
//            AndType(
//              TypeDecl(
//                12,
//                TypeProj(16, 17),
//                RecType(
//                  13,
//                  AndType(
//                    AndType(
//                      TypeDecl(14, Bot, Top),
//                      TypeDecl(15, TypeProj(7, 8), TypeProj(7, 8))
//                    ),
//                    TypeProj(7, 8)
//                  )
//                )
//              ),
//              TypeDecl(20, Bot, TypeProj(16, 17))
//            )
//          ),
//          TypeProj(7, 8)
//        )
//      ),
//      7 -> TypeDecl(
//        8,
//        Bot,
//        FunType(
//          9,
//          RecType(10, TypeDecl(11, Bot, Bot)),
//          TypeProj(9, 11)
//        )
//      ),
//      18 -> TypeDecl(19, Bot, Bot),
//      16 -> TypeDecl(17, TypeProj(18, 19), TypeProj(18, 19)),
//      23 -> TypeDecl(24, Bot, TypeProj(25, 26)),
//  2 -> RecType(3, AndType(Bot, RecType(4, Top)))
//    ),
//    29
//  ),
//  1,
//  0,
//  TypeProj(21, 22),
//  //FunType(
//  //  5,
//  //  FunType(6, TypeProj(7, 8), Bot),
//  //  AndType(
//  //    TypeDecl(
//  //      12,
//  //      TypeProj(16, 17),
//  //      RecType(
//  //        13,
//  //        AndType(
//  //          AndType(
//  //            TypeDecl(14, Bot, Top),
//  //            TypeDecl(15, TypeProj(7, 8), TypeProj(7, 8))
//  //          ),
//  //          TypeProj(7, 8)
//  //        )
//  //      )
//  //    ),
//  //    TypeDecl(20, Bot, TypeProj(16, 17))
//  //  )
//  //),
//  FunType(5, Que, Que) // TODO "inconsistent"
//)

( // TODO
  GlobalContext(
    Map(
      28 -> TypeDecl(
        29,
        TypeProj(30, 31),
        FunType(27, AndType(Top, Top), Bot)
      ),
      38 -> TypeDecl(39, Bot, Bot),
      9 -> TypeDecl(10, Bot, Bot),
      32 -> TypeDecl(33, AndType(Bot, Top), TypeProj(34, 35)),
      34 -> TypeDecl(35, TypeProj(36, 37), AndType(Bot, TypeProj(9, 10))),
      7 -> TypeDecl(8, AndType(Bot, Bot), AndType(Top, TypeProj(9, 10))),
      3 -> TypeDecl(
        4,
        AndType(TypeProj(32, 33), FieldDecl(40, Top)),
        AndType(
          AndType(
            FieldDecl(5, Bot),
            TypeDecl(6, Bot, TypeProj(7, 8))
          ),
          AndType(
            FieldDecl(
              11,
              RecType(
                12,
                RecType(
                  13,
                  AndType(
                    FieldDecl(
                      14,
                      FieldDecl(
                        15,
                        TypeDecl(
                          16,
                          Bot,
                          FunType(
                            17,
                            FieldDecl(18, TypeProj(19, 20)),
                            RecType(21, TypeProj(7, 8))
                          )
                        )
                      )
                    ),
                    FieldDecl(
                      22,
                      AndType(
                        FunType(
                          23,
                          Bot,
                          FieldDecl(24, RecType(25, TypeProj(7, 8)))
                        ),
                        TypeProj(19, 20)
                      )
                    )
                  )
                )
              )
            ),
            TypeDecl(
              26,
              TypeProj(28, 29),
              FunType(27, AndType(Top, Top), Bot)
            )
          )
        )
      ),
      36 -> TypeDecl(37, TypeProj(38, 39), TypeProj(38, 39)),
      30 -> TypeDecl(31, Bot, AndType(Bot, Bot)),
      19 -> TypeDecl(20, Bot, TypeProj(9, 10)),
      2 -> TypeProj(3, 4)
    ),
    70
  ),
  1,
  0,
  AndType(
    FunType(
      41,
      FieldDecl(
        42,
        FunType(
          43,
          AndType(FieldDecl(44, Top), FieldDecl(45, TypeProj(3, 4))),
          RecType(
            46,
            AndType(
              AndType(FieldDecl(47, Bot), FieldDecl(48, Bot)),
              AndType(
                AndType(
                  AndType(FieldDecl(49, Bot), FieldDecl(50, Top)),
                  FieldDecl(
                    51,
                    AndType(
                      RecType(52, AndType(Bot, TypeProj(36, 37))),
                      Top
                    )
                  )
                ),
                FieldDecl(53, Bot)
              )
            )
          )
        )
      ),
      RecType(
        54,
        AndType(
          FieldDecl(55, TypeProj(9, 10)),
          FieldDecl(
            56,
            FunType(
              57,
              TypeProj(28, 29),
              FunType(
                58,
                TypeDecl(59, Bot, Bot),
                AndType(
                  RecType(60, TypeProj(32, 33)),
                  FieldDecl(61, RecType(62, Bot))
                )
              )
            )
          )
        )
      )
    ),
    RecType(
      63,
      RecType(
        64,
        FunType(
          65,
          RecType(
            66,
            AndType(
              TypeDecl(67, Bot, TypeProj(30, 31)),
              FieldDecl(68, Top)
            )
          ),
          AndType(FunType(69, Bot, Bot), Top)
        )
      )
    )
  ),
  //FunType(
  //  41,
  //  FieldDecl(
  //    42,
  //    FunType(
  //      43,
  //      AndType(FieldDecl(44, Top), FieldDecl(45, TypeProj(3, 4))),
  //      RecType(
  //        46,
  //        AndType(
  //          AndType(FieldDecl(47, Bot), FieldDecl(48, Bot)),
  //          AndType(
  //            AndType(
  //              AndType(FieldDecl(49, Bot), FieldDecl(50, Top)),
  //              FieldDecl(
  //                51,
  //                AndType(
  //                  RecType(52, AndType(Bot, TypeProj(36, 37))),
  //                  Top
  //                )
  //              )
  //            ),
  //            FieldDecl(53, Bot)
  //          )
  //        )
  //      )
  //    )
  //  ),
  //  RecType(
  //    54,
  //    AndType(
  //      FieldDecl(55, TypeProj(9, 10)),
  //      FieldDecl(
  //        56,
  //        FunType(
  //          57,
  //          TypeProj(28, 29),
  //          FunType(
  //            58,
  //            TypeDecl(59, Bot, Bot),
  //            AndType(
  //              RecType(60, TypeProj(32, 33)),
  //              FieldDecl(61, RecType(62, Bot))
  //            )
  //          )
  //        )
  //      )
  //    )
  //  )
  //),
  FunType(41, Que, Que)
)

val scope = globalScope + (z -> a)

//val res1 = NoFuture.varRaise(scope, r, z, p)
//P.namedln("res1", res1)

//val res3 = NoFuture.raise(scope, r, None, a, p)
//P.namedln("res3", res3)

import NoFuture._

val (numQues, labeledPrototype) = prepMatch(r, simplify(p))

val solveSet = (0 until numQues).map{TypeProj(r, _)}.toSet

val constraint = gatherConstraints(scope, solveSet, Some(z), a, labeledPrototype, patternIsLeft=false)


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


val solveSetVariance = gatherVariance(solveSet, labeledPrototype, Covariant)

val startTime = System.nanoTime()
val res2 = solveConstraint(scope, Some(z), solveSet, solveSetVariance, constraint, labeledPrototype, Covariant)
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
//    AndType(AndType(TypeProj(3, 4), FunType(39, Bot, Bot)), TypeProj(16, 17)),
//    FunType(
//      40,
//      FunType(41, TypeProj(9, 10), FunType(42, Bot, Bot)),
//      TypeDecl(43, Bot, TypeDecl(44, Bot, TypeProj(19, 20)))
//    )
//  ),
//  AndType(
//    TypeProj(3, 4),
//    FunType(40, Bot, TypeDecl(43, Bot, TypeDecl(44, Bot, Top)))
//  )
//)

//( // "1 already in scope"
//  GlobalContext(Map(), 4),
//  0,
//  NoFuture.greatestCommonSubtype(Map(),
//    AndType(Top, RecType(1, RecType(2, FieldDecl(3, Bot)))),
//    RecType(1, RecType(2, FieldDecl(3, Bot)))),
//  AndType(Top, RecType(1, RecType(2, FieldDecl(3, Bot))))
//)

//( // None.get
//  GlobalContext(Map(), 7),
//  0,
//  FunType(1, TypeDecl(2, Bot, Bot), TypeProj(1, 2)),
//  FunType(
//    3,
//    TypeDecl(4, RecType(5, Top), Top),
//    TypeDecl(6, Top, Top)
//  )
//)

//(
//  GlobalContext(Map(), 3),
//  0,
//  RecType(1, RecType(2, Top)),
//  RecType(2, RecType(1, Top))
//)

//( // TODO
//  GlobalContext(
//    Map(
//      3 -> TypeDecl(4, Bot, Bot),
//      1 -> TypeDecl(2, TypeProj(3, 4), Bot),
//      5 -> TypeDecl(
//        6,
//        Bot,
//        FunType(
//          7,
//          FieldDecl(
//            8,
//            RecType(
//              9,
//              AndType(
//                FieldDecl(10, RecType(11, Top)),
//                TypeDecl(12, Bot, Top)
//              )
//            )
//          ),
//          FunType(
//            13,
//            RecType(14, RecType(15, TypeProj(1, 2))),
//            AndType(
//              AndType(
//                FieldDecl(16, TypeProj(3, 4)),
//                FieldDecl(17, FunType(18, Bot, TypeProj(3, 4)))
//              ),
//              TypeProj(1, 2)
//            )
//          )
//        )
//      )
//    ),
//    21
//  ),
//  0,
//FunType(
//  7,
//  AndType(
//    FieldDecl(
//      8,
//      RecType(
//        9,
//        AndType(FieldDecl(10, RecType(11, Top)), TypeDecl(12, Bot, Top))
//      )
//    ),
//    TypeDecl(20, Top, Top)
//  ),
//  FunType(
//    13,
//    AndType(
//      RecType(14, RecType(15, TypeProj(1, 2))),
//      FieldDecl(
//        8,
//        RecType(
//          9,
//          AndType(FieldDecl(10, RecType(11, Top)), TypeDecl(12, Bot, Top))
//        )
//      )
//    ),
//    FunType(
//      13, // TODO How does this happen? 13 shadows 13 above.
//      RecType(14, RecType(15, TypeProj(1, 2))),
//      AndType(
//        AndType(
//          FieldDecl(16, TypeProj(3, 4)),
//          FieldDecl(17, FunType(18, Bot, TypeProj(3, 4)))
//        ),
//        TypeProj(1, 2)
//      )
//    )
//  )
//),
//FunType(
//  7,
//  AndType(
//    FieldDecl(
//      8,
//      RecType(
//        9,
//        AndType(FieldDecl(10, RecType(11, Top)), TypeDecl(12, Bot, Top))
//      )
//    ),
//    TypeDecl(20, Top, Top)
//  ),
//  FunType(
//    13,
//    AndType(
//      RecType(14, RecType(15, TypeProj(1, 2))),
//      FieldDecl(
//        8,
//        RecType(
//          9,
//          AndType(FieldDecl(10, RecType(11, Top)), TypeDecl(12, Bot, Top))
//        )
//      )
//    ),
//    FunType(
//      13,
//      RecType(14, RecType(15, TypeProj(1, 2))),
//      AndType(
//        AndType(
//          FieldDecl(16, TypeProj(3, 4)),
//          FieldDecl(17, FunType(18, Bot, TypeProj(3, 4)))
//        ),
//        TypeProj(1, 2)
//      )
//    )
//  )
//)
//)

//(
//  GlobalContext(
//    Map(
//      25 -> TypeDecl(26, Bot, TypeProj(27, 28)),
//      14 -> TypeDecl(15, TypeProj(17, 18), TypeDecl(16, Top, Top)),
//      1 -> TypeDecl(
//        2,
//        AndType(
//          TypeProj(3, 4),
//          TypeDecl(
//            21,
//            AndType(Bot, TypeProj(3, 4)),
//            TypeProj(22, 23)
//          )
//        ),
//        TypeProj(3, 4)
//      ),
//      17 -> TypeDecl(18, TypeProj(19, 20), TypeProj(19, 20)),
//      22 -> TypeDecl(23, Bot, Bot),
//      27 -> TypeDecl(28, TypeProj(1, 2), TypeProj(1, 2)),
//      3 -> TypeDecl(
//        4,
//        AndType(
//          Bot,
//          RecType(11, RecType(12, FieldDecl(13, TypeProj(14, 15))))
//        ),
//        FunType(
//          5,
//          FunType(
//            6,
//            FunType(7, Top, FieldDecl(8, Top)),
//            FunType(9, RecType(10, Top), Bot)
//          ),
//          AndType(Bot, Top)
//        )
//      ),
//      19 -> TypeDecl(20, Bot, Bot)
//    ),
//    29
//  ),
//  0,
//  //TypeProj(25, 26),
//  //AndType(
//  //  TypeProj(3, 4),
//  //  TypeDecl(21, AndType(Bot, TypeProj(3, 4)), TypeProj(22, 23))
//  //)
//  TypeProj(1, 2),
//  TypeProj(3, 4)
//)


( // TODO
  GlobalContext(
    Map(
      28 -> TypeDecl(
        29,
        TypeProj(30, 31),
        FunType(27, AndType(Top, Top), Bot)
      ),
      38 -> TypeDecl(39, Bot, Bot),
      9 -> TypeDecl(10, Bot, Bot),
      32 -> TypeDecl(33, AndType(Bot, Top), TypeProj(34, 35)),
      34 -> TypeDecl(35, TypeProj(36, 37), AndType(Bot, TypeProj(9, 10))),
      7 -> TypeDecl(8, AndType(Bot, Bot), AndType(Top, TypeProj(9, 10))),
      3 -> TypeDecl(
        4,
        AndType(TypeProj(32, 33), FieldDecl(40, Top)),
        AndType(
          AndType(
            FieldDecl(5, Bot),
            TypeDecl(6, Bot, TypeProj(7, 8))
          ),
          AndType(
            FieldDecl(
              11,
              RecType(
                12,
                RecType(
                  13,
                  AndType(
                    FieldDecl(
                      14,
                      FieldDecl(
                        15,
                        TypeDecl(
                          16,
                          Bot,
                          FunType(
                            17,
                            FieldDecl(18, TypeProj(19, 20)),
                            RecType(21, TypeProj(7, 8))
                          )
                        )
                      )
                    ),
                    FieldDecl(
                      22,
                      AndType(
                        FunType(
                          23,
                          Bot,
                          FieldDecl(24, RecType(25, TypeProj(7, 8)))
                        ),
                        TypeProj(19, 20)
                      )
                    )
                  )
                )
              )
            ),
            TypeDecl(
              26,
              TypeProj(28, 29),
              FunType(27, AndType(Top, Top), Bot)
            )
          )
        )
      ),
      36 -> TypeDecl(37, TypeProj(38, 39), TypeProj(38, 39)),
      30 -> TypeDecl(31, Bot, AndType(Bot, Bot)),
      19 -> TypeDecl(20, Bot, TypeProj(9, 10)),
      2 -> TypeProj(3, 4)
    ),
    70
  ),
  0,
  FunType(
    41,
    AndType(
      FieldDecl(
        42,
        FunType(
          43,
          AndType(FieldDecl(44, Top), FieldDecl(45, TypeProj(3, 4))),
          RecType(
            46,
            AndType(
              AndType(FieldDecl(47, Bot), FieldDecl(48, Bot)),
              AndType(
                AndType(
                  AndType(FieldDecl(49, Bot), FieldDecl(50, Top)),
                  FieldDecl(
                    51,
                    AndType(
                      RecType(52, AndType(Bot, TypeProj(36, 37))),
                      Top
                    )
                  )
                ),
                FieldDecl(53, Bot)
              )
            )
          )
        )
      ),
      RecType(
        66,
        AndType(
          TypeDecl(67, Bot, TypeProj(30, 31)),
          FieldDecl(68, Top)
        )
      )
    ),
    Top
  ),
  FunType(
    41,
    FieldDecl(
      42,
      FunType(
        43,
        AndType(FieldDecl(44, Top), FieldDecl(45, TypeProj(3, 4))),
        RecType(
          46,
          AndType(
            AndType(FieldDecl(47, Bot), FieldDecl(48, Bot)),
            AndType(
              AndType(
                AndType(FieldDecl(49, Bot), FieldDecl(50, Top)),
                FieldDecl(
                  51,
                  AndType(
                    RecType(52, AndType(Bot, TypeProj(36, 37))),
                    Top
                  )
                )
              ),
              FieldDecl(53, Bot)
            )
          )
        )
      )
    ),
    RecType(
      54,
      AndType(
        FieldDecl(55, TypeProj(9, 10)),
        FieldDecl(
          56,
          FunType(
            57,
            TypeProj(28, 29),
            FunType(
              58,
              TypeDecl(59, Bot, Bot),
              AndType(
                RecType(60, TypeProj(32, 33)),
                FieldDecl(61, RecType(62, Bot))
              )
            )
          )
        )
      )
    )
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

//(
//  GlobalContext(
//    Map(
//      8 -> TypeDecl(9, Bot, Bot),
//      18 -> TypeDecl(19, Bot, RecType(20, TypeProj(8, 9))),
//      3 -> TypeDecl(
//        4,
//        RecType(
//          5,
//          AndType(
//            TypeDecl(
//              6,
//              AndType(
//                AndType(
//                  TypeProj(8, 9),
//                  RecType(
//                    10,
//                    AndType(
//                      FieldDecl(11, Top),
//                      TypeDecl(12, Bot, Bot)
//                    )
//                  )
//                ),
//                Bot
//              ),
//              FunType(7, Bot, Bot)
//            ),
//            AndType(
//              AndType(
//                AndType(
//                  FieldDecl(13, RecType(14, Top)),
//                  FieldDecl(15, Bot)
//                ),
//                AndType(
//                  TypeDecl(16, Bot, Bot),
//                  AndType(
//                    FieldDecl(17, TypeProj(18, 19)),
//                    FieldDecl(21, Bot)
//                  )
//                )
//              ),
//              FieldDecl(22, TypeProj(18, 19))
//            )
//          )
//        ),
//        RecType(
//          5,
//          AndType(
//            TypeDecl(
//              6,
//              AndType(
//                AndType(
//                  TypeProj(8, 9),
//                  RecType(
//                    10,
//                    AndType(
//                      FieldDecl(11, Top),
//                      TypeDecl(12, Bot, Bot)
//                    )
//                  )
//                ),
//                Bot
//              ),
//              FunType(7, Bot, Bot)
//            ),
//            AndType(
//              AndType(
//                AndType(
//                  FieldDecl(13, RecType(14, Top)),
//                  FieldDecl(15, Bot)
//                ),
//                AndType(
//                  TypeDecl(16, Bot, Bot),
//                  AndType(
//                    FieldDecl(17, TypeProj(18, 19)),
//                    FieldDecl(21, Bot)
//                  )
//                )
//              ),
//              FieldDecl(22, TypeProj(18, 19))
//            )
//          )
//        )
//      ),
//      1 -> TypeDecl(
//        2,
//        AndType(TypeProj(3, 4), RecType(23, TypeProj(3, 4))),
//        TypeProj(3, 4)
//      )
//    ),
//    24
//  ),
//  0,
//  TypeProj(1, 2),
//  TypeProj(1, 2)
//)

(
  GlobalContext(
    Map(
      8 -> TypeDecl(9, Bot, AndType(Bot, Bot)),
      6 -> TypeDecl(7, Bot, TypeProj(8, 9)),
      1 -> TypeDecl(
        2,
        TypeProj(6, 7),
        FunType(
          3,
          Top,
          FunType(4, Top, AndType(FieldDecl(5, Top), Bot))
        )
      )
    ),
    12
  ),
  0,
  TypeProj(1, 2),
  FunType(10, TypeDecl(11, Bot, TypeProj(8, 9)), TypeProj(1, 2))
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

//(
//  GlobalContext(
//    Map(
//      10 -> TypeDecl(11, Bot, TypeProj(12, 13)),
//      29 -> TypeDecl(
//        30,
//        AndType(AndType(Bot, Bot), TypeProj(35, 36)),
//        FunType(
//          31,
//          FunType(32, TypeProj(33, 34), TypeProj(35, 36)),
//          Top
//        )
//      ),
//      1 -> TypeDecl(2, TypeProj(3, 4), Top),
//      33 -> TypeDecl(34, Bot, Top),
//      27 -> TypeDecl(28, Top, Top),
//      12 -> TypeDecl(
//        13,
//        AndType(
//          AndType(AndType(Bot, Bot), Top),
//          AndType(
//            TypeDecl(
//              39,
//              FieldDecl(40, TypeProj(29, 30)),
//              FieldDecl(40, TypeProj(29, 30))
//            ),
//            TypeDecl(
//              41,
//              AndType(
//                Bot,
//                FieldDecl(45, TypeDecl(46, Bot, TypeProj(29, 30)))
//              ),
//              AndType(
//                TypeDecl(42, Bot, Bot),
//                TypeDecl(43, Bot, FunType(44, Top, Bot))
//              )
//            )
//          )
//        ),
//        TypeDecl(
//          14,
//          AndType(
//            Bot,
//            FieldDecl(
//              18,
//              FunType(
//                19,
//                FunType(
//                  20,
//                  TypeDecl(
//                    21,
//                    FieldDecl(22, Bot),
//                    FieldDecl(22, TypeDecl(23, Bot, Bot))
//                  ),
//                  FunType(
//                    24,
//                    RecType(25, FieldDecl(26, TypeProj(27, 28))),
//                    TypeProj(29, 30)
//                  )
//                ),
//                RecType(37, TypeDecl(38, Bot, Top))
//              )
//            )
//          ),
//          RecType(
//            15,
//            AndType(FieldDecl(16, Top), FieldDecl(17, Top))
//          )
//        )
//      ),
//      3 -> TypeDecl(
//        4,
//        AndType(
//          AndType(
//            TypeDecl(5, Bot, Top),
//            FieldDecl(6, FieldDecl(7, TypeProj(8, 9)))
//          ),
//          Top
//        ),
//        AndType(
//          TypeDecl(5, Bot, Top),
//          FieldDecl(6, FieldDecl(7, TypeProj(8, 9)))
//        )
//      ),
//      35 -> TypeDecl(36, Bot, Bot),
//      8 -> TypeDecl(9, AndType(Top, TypeProj(10, 11)), Top)
//    ),
//    48
//  ),
//  0,
//    AndType(
//      TypeDecl(5, Bot, Top),
//      FieldDecl(6, FieldDecl(7, TypeProj(8, 9)))
//    ),
//  AndType(TypeProj(1, 2), FunType(47, Bot, TypeProj(8, 9))),
//  TypeProj(1, 2)
//)

(
  GlobalContext(
    Map(
      25 -> TypeDecl(26, Bot, TypeProj(27, 28)),
      14 -> TypeDecl(15, TypeProj(17, 18), TypeDecl(16, Top, Top)),
      1 -> TypeDecl(
        2,
        AndType(
          TypeProj(3, 4),
          TypeDecl(
            21,
            AndType(Bot, TypeProj(3, 4)),
            TypeProj(22, 23)
          )
        ),
        TypeProj(3, 4)
      ),
      17 -> TypeDecl(18, TypeProj(19, 20), TypeProj(19, 20)),
      22 -> TypeDecl(23, Bot, Bot),
      27 -> TypeDecl(28, TypeProj(1, 2), TypeProj(1, 2)),
      3 -> TypeDecl(
        4,
        AndType(
          Bot,
          RecType(11, RecType(12, FieldDecl(13, TypeProj(14, 15))))
        ),
        FunType(
          5,
          FunType(
            6,
            FunType(7, Top, FieldDecl(8, Top)),
            FunType(9, RecType(10, Top), Bot)
          ),
          AndType(Bot, Top)
        )
      ),
      19 -> TypeDecl(20, Bot, Bot)
    ),
    29
  ),
  0,
  //AndType(
    AndType(
      TypeProj(3, 4),
      TypeDecl(21, AndType(Bot, TypeProj(3, 4)), TypeProj(22, 23))
    ),
  //  RecType(24, TypeProj(14, 15))
  //),
  TypeProj(25, 26),
  TypeProj(1, 2)
)

//( // TODO sort of works using hack (hack: subtyping fails if there is shadowing). Consider letting raise and lub use SymbolUniverse?
//  GlobalContext(
//    Map(
//      3 -> TypeDecl(4, Bot, Bot),
//      1 -> TypeDecl(2, TypeProj(3, 4), Bot),
//      5 -> TypeDecl(
//        6,
//        Bot,
//        FunType( // TODO will end up comparing fun(arg,res) with res, which is weird. Also, 13 will already be in scope, causing shadowing.
//          7,
//          FieldDecl(
//            8,
//            RecType(
//              9,
//              AndType(
//                FieldDecl(10, RecType(11, Top)),
//                TypeDecl(12, Bot, Top)
//              )
//            )
//          ),
//          FunType(
//            13,
//            RecType(14, RecType(15, TypeProj(1, 2))),
//            AndType(
//              AndType(
//                FieldDecl(16, TypeProj(3, 4)),
//                FieldDecl(17, FunType(18, Bot, TypeProj(3, 4)))
//              ),
//              TypeProj(1, 2)
//            )
//          )
//        )
//      )
//    ),
//    21
//  ),
//  0,
//  TypeProj(1, 2),
//  TypeProj(5, 6),
//  FunType(19, TypeDecl(20, Top, Top), TypeProj(5, 6))
//)

def lub(left: Type, right: Type) = leastCommonSupertype(scope, left, right)

val ab   = lub(a, b)
P.namedln("lub(a, b)", ab)
val bc   = lub(b, c)
P.namedln("lub(b, c)", bc)
val a_bc = lub(a, bc)
P.namedln("lub(a, lub(b, c))", a_bc)
val ab_c = lub(ab, c)
P.namedln("lub(lub(a, b), c)", ab_c)

P.namedln("lub(a, lub(b, c)) == lub(lub(a, b), c)", varEqualTypes(scope + (z -> a_bc), z, ab_c))

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

//( // TODO fun(x:rec)x.T --> fun(x: Top)x.T
//  GlobalContext(
//    Map(),
//    20),
//  Map(1 -> TypeDecl(4, Bot, FunType(3, Top, Top))),
//  Set(1),
//  0,
//  FunType(3, RecType(5, TypeDecl(4, FunType(3, Top, Bot), TypeProj(1, 4))), TypeProj(3, 4))
//)

(
  GlobalContext(Map(7 -> TypeDecl(8, Bot, Bot)), 11),
  Map(
    1 -> FunType(2, TypeDecl(3, Bot, Bot), FunType(4, TypeProj(2, 3), Top))
  ),
  Set(1),
  0,
  AndType(
    RecType(
      5,
      AndType(
        TypeDecl(6, TypeProj(7, 8), TypeProj(7, 8)),
        FieldDecl(9, TypeProj(5, 6))
      )
    ),
    FieldDecl(10, Bot)
  )
  //RecType(
  //  5,
  //  AndType(
  //    TypeDecl(6, TypeProj(7, 8), TypeProj(7, 8)),
  //    FieldDecl(9, TypeProj(5, 6))
  //  )
  //)
)


//( // TODO
//  GlobalContext(Map(), 7),
//  Map(
//    1 -> AndType(TypeDecl(2, Bot, Bot), Bot),
//    3 -> Top,
//    4 -> Bot,
//    5 -> FunType(6, TypeProj(1, 2), Bot)
//  ),
//  Set(1,3,4,5),
//  0,
//  Top, // TODO odd. Top is not a subtype of TypeProj(1, 2)...
//  TypeProj(1, 2)
//)

//( // TODO
//  GlobalContext(Map(3 -> TypeDecl(4, Bot, Top)), 7),
//  Map(
//    1 -> AndType(
//      TypeDecl(2, TypeProj(3, 4), TypeProj(3, 4)),
//      RecType(5, Bot)
//    )
//  ),
//  0,
//  AndType(AndType(Top, TypeProj(3, 4)), RecType(6, Top)),
//  TypeProj(1, 2)
//)

( // TODO
  GlobalContext(
    Map(
      7 -> TypeDecl(8, Bot, AndType(Top, Bot)),
      11 -> TypeDecl(12, Top, Top),
    1 -> AndType(
      TypeDecl(
        2,
        Bot,
        FunType(
          3,
          TypeDecl(4, FieldDecl(5, Bot), FieldDecl(5, Top)),
          Top
        )
      ),
      RecType(6, TypeProj(7, 8))
    ),
    9 -> TypeProj(1, 2),
    10 -> Top
    ),
    18
  ),
  0,
  AndType(
    AndType(
      TypeProj(11, 12),
      FunType(
        13,
        FieldDecl(14, FunType(15, Top, RecType(16, Top))),
        FieldDecl(17, TypeProj(11, 12))
      )
    ),
    TypeProj(11, 12)
  ),
  TypeProj(1, 2)
)

( // TODO
  GlobalContext(Map(3 -> TypeDecl(4, Bot, Bot)), 7),
  Map(1 -> AndType(TypeDecl(2, Bot, Top), TypeProj(3, 4))),
  0,
  AndType(Top, RecType(5, FieldDecl(6, Bot))),
  AndType(TypeProj(1, 2), RecType(5, FieldDecl(6, Bot)))
)

  val res = eliminateVars(scope, killSet, Some(z), a, variance=Contravariant)
  P.namedln("res", res)

  P.namedln("valid(res)", NoFuture.validTypeInScope(scope, res))
}


def debugtc(): Unit = {

val p: InferenceProblem =

//InferenceProblem(GlobalContext(Map(0 -> Top), 1), Var(0), Que, TypedVar(0) :- Top)

//InferenceProblem(
//  GlobalContext(
//    Map(
//      25 -> TypeDecl(26, TypeProj(27, 28), Top),
//      21 -> TypeDecl(
//        22,
//        AndType(
//          Bot,
//          RecType(23, TypeDecl(24, Bot, TypeProj(25, 26)))
//        ),
//        Bot
//      ),
//      27 -> TypeDecl(28, Top, Top),
//      12 -> TypeProj(21, 22),
//      11 -> FunType(
//        13,
//        AndType(
//          Top,
//          TypeDecl(
//            14,
//            AndType(FieldDecl(15, Bot), Bot),
//            FieldDecl(15, Bot)
//          )
//        ),
//        FunType(
//          16,
//          Bot,
//          FunType(
//            17,
//            FunType(
//              18,
//              RecType(19, FieldDecl(20, Bot)),
//              TypeProj(13, 14)
//            ),
//            Bot
//          )
//        )
//      )
//    ),
//    30
//  ),
//  App(11, 12),
//  Que,
//  TypedApp(11, 12) :- FunType(
//    16,
//    Bot,
//    FunType(
//      17,
//      FunType(18, RecType(19, FieldDecl(20, Bot)), TypeProj(12, 14)),
//      Bot
//    )
//  )
//)

//InferenceProblem(
//  GlobalContext(
//    Map(
//      25 -> TypeDecl(26, TypeProj(27, 28), Top),
//      29 -> Bot,
//      21 -> TypeDecl(
//        22,
//        AndType(Bot, RecType(23, TypeDecl(24, Bot, TypeProj(25, 26)))),
//        Bot
//      ),
//      9 -> FieldDecl(10, Bot),
//      27 -> TypeDecl(28, Top, Top),
//      12 -> TypeProj(21, 22),
//      11 -> FunType(
//        13,
//        AndType(
//          Top,
//          TypeDecl(14, AndType(FieldDecl(15, Bot), Bot), FieldDecl(15, Bot))
//        ),
//        FunType(
//          16,
//          Bot,
//          FunType(
//            17,
//            FunType(18, RecType(19, FieldDecl(20, Bot)), TypeProj(13, 14)),
//            Bot
//          )
//        )
//      )
//    ),
//    30
//  ),
//  Let(
//    0,
//    Fun(
//      1,
//      FieldDecl(2, FunType(3, TypeDecl(4, Top, Top), Top)),
//      Let(5, Fun(6, RecType(7, FieldDecl(8, Top)), Var(9)), App(11, 12))
//    ),
//    App(11, 29)
//  ),
//  Que,
//  TypedLet(
//    0,
//    TypedFun(
//      1,
//      FieldDecl(2, FunType(3, TypeDecl(4, Top, Top), Top)),
//      TypedLet(
//        5,
//        TypedFun(
//          6,
//          RecType(7, FieldDecl(8, Top)),
//          TypedVar(9) :- FieldDecl(10, Bot)
//        ) :- FunType(6, RecType(7, FieldDecl(8, Top)), FieldDecl(10, Bot)),
//        TypedApp(11, 12) :- FunType(
//          16,
//          Bot,
//          FunType(
//            17,
//            FunType(18, RecType(19, FieldDecl(20, Bot)), TypeProj(12, 14)),
//            Bot
//          )
//        )
//      ) :- FunType(
//        16,
//        Bot,
//        FunType(
//          17,
//          FunType(18, RecType(19, FieldDecl(20, Bot)), TypeProj(12, 14)),
//          Bot
//        )
//      )
//    ) :- FunType(
//      1,
//      FieldDecl(2, FunType(3, TypeDecl(4, Top, Top), Top)),
//      FunType(
//        16,
//        Bot,
//        FunType(
//          17,
//          FunType(18, RecType(19, FieldDecl(20, Bot)), TypeProj(12, 14)),
//          Bot
//        )
//      )
//    ),
//    TypedApp(11, 29) :- FunType(
//      16,
//      Bot,
//      FunType(
//        17,
//        FunType(18, RecType(19, FieldDecl(20, Bot)), TypeProj(29, 14)),
//        Bot
//      )
//    )
//  ) :- FunType(
//    16,
//    Bot,
//    FunType(
//      17,
//      FunType(18, RecType(19, FieldDecl(20, Bot)), TypeProj(29, 14)),
//      Bot
//    )
//  )
//)

//InferenceProblem(
//  GlobalContext(
//    Map(
//      6 -> TypeDecl(7, Bot, Bot),
//      0 -> FunType(
//        2,
//        TypeDecl(
//          3,
//          FieldDecl(4, Bot),
//          FieldDecl(4, RecType(5, TypeProj(6, 7)))
//        ),
//        TypeProj(2, 3)
//      ),
//      1 -> Bot
//    ),
//    8
//  ),
//  App(0, 1),
//  Que,
//  TypedApp(0, 1) :- TypeProj(1, 3)
//)

//InferenceProblem(
//  GlobalContext(
//    Map(
//      5 -> TypeDecl(7, TypeProj(8, 9), TypeProj(8, 9)),
//      4 -> FunType(
//        6,
//        TypeDecl(7, TypeProj(8, 9), TypeProj(8, 9)),
//        FieldDecl(10, RecType(11, FieldDecl(12, TypeProj(6, 7))))
//      ),
//      8 -> TypeDecl(9, Bot, Bot)
//    ),
//    14
//  ),
//  Obj(
//    2,
//    FieldDecl(
//      3,
//      FieldDecl(10, RecType(11, FieldDecl(12, TypeProj(5, 7))))
//    ),
//    FieldDef(3, App(4, 5))
//  ),
//  Que,
//  TypedObj(
//    2,
//    FieldDecl(
//      3,
//      FieldDecl(10, RecType(11, FieldDecl(12, TypeProj(5, 7))))
//    ),
//    TypedFieldDef(
//      3,
//      TypedApp(4, 5) :- FieldDecl(10, RecType(11, FieldDecl(12, TypeProj(5, 7))))
//    ) :- FieldDecl(
//      3,
//      FieldDecl(10, RecType(11, FieldDecl(12, TypeProj(5, 7))))
//    )
//  ) :- FieldDecl(
//    3,
//    FieldDecl(10, RecType(11, FieldDecl(12, TypeProj(5, 7))))
//  )
//)


//InferenceProblem(
//  GlobalContext(
//    Map(
//      12 -> TypeDecl(13, Bot, TypeProj(14, 15)),
//      18 -> FieldDecl(19, AndType(Top, Top)),
//      14 -> TypeDecl(15, Bot, Top)
//    ),
//    20
//  ),
//  Obj(
//    8,
//    AndType(
//      TypeDecl(
//        9,
//        FieldDecl(
//          10,
//          FunType(
//            11,
//            TypeProj(12, 13),
//            FieldDecl(16, TypeProj(12, 13))
//          )
//        ),
//        FieldDecl(
//          10,
//          FunType(
//            11,
//            TypeProj(12, 13),
//            FieldDecl(16, TypeProj(12, 13))
//          )
//        )
//      ),
//      FieldDecl(17, AndType(Top, Top))
//    ),
//    AndDef(
//      TypeDef(
//        9,
//        FieldDecl(
//          10,
//          FunType(
//            11,
//            TypeProj(12, 13),
//            FieldDecl(16, TypeProj(12, 13))
//          )
//        )
//      ),
//      FieldDef(17, Sel(18, 19))
//    )
//  ),
//  Que,
//  TypedObj(
//    8,
//    AndType(
//      TypeDecl(
//        9,
//        FieldDecl(
//          10,
//          FunType(
//            11,
//            TypeProj(12, 13),
//            FieldDecl(16, TypeProj(12, 13))
//          )
//        ),
//        FieldDecl(
//          10,
//          FunType(
//            11,
//            TypeProj(12, 13),
//            FieldDecl(16, TypeProj(12, 13))
//          )
//        )
//      ),
//      FieldDecl(17, AndType(Top, Top))
//    ),
//    TypedAndDef(
//      TypedTypeDef(
//        9,
//        FieldDecl(
//          10,
//          FunType(
//            11,
//            TypeProj(12, 13),
//            FieldDecl(16, TypeProj(12, 13))
//          )
//        )
//      ) :- TypeDecl(
//        9,
//        FieldDecl(
//          10,
//          FunType(
//            11,
//            TypeProj(12, 13),
//            FieldDecl(16, TypeProj(12, 13))
//          )
//        ),
//        FieldDecl(
//          10,
//          FunType(
//            11,
//            TypeProj(12, 13),
//            FieldDecl(16, TypeProj(12, 13))
//          )
//        )
//      ),
//      TypedFieldDef(17, TypedSel(18, 19) :- AndType(Top, Top)) :- FieldDecl(
//        17,
//        AndType(Top, Top)
//      )
//    ) :- AndType(
//      TypeDecl(
//        9,
//        FieldDecl(
//          10,
//          FunType(
//            11,
//            TypeProj(12, 13),
//            FieldDecl(16, TypeProj(12, 13))
//          )
//        ),
//        FieldDecl(
//          10,
//          FunType(
//            11,
//            TypeProj(12, 13),
//            FieldDecl(16, TypeProj(12, 13))
//          )
//        )
//      ),
//      FieldDecl(17, AndType(Top, Top))
//    )
//  ) :- RecType(
//    8,
//    AndType(
//      TypeDecl(
//        9,
//        FieldDecl(
//          10,
//          FunType(
//            11,
//            TypeProj(12, 13),
//            FieldDecl(16, TypeProj(12, 13))
//          )
//        ),
//        FieldDecl(
//          10,
//          FunType(
//            11,
//            TypeProj(12, 13),
//            FieldDecl(16, TypeProj(12, 13))
//          )
//        )
//      ),
//      FieldDecl(17, AndType(Top, Top))
//    )
//  )
//)

//InferenceProblem( // TODO gives `TypedSel(0, 3) :- Bot` instead of `... :- TypeProj(4,5)`. but looks like a generator bug since Bot is a valid raise here.
//  GlobalContext(
//    Map(
//      0 -> AndType(
//        RecType(1, RecType(2, FieldDecl(3, TypeProj(4, 5)))),
//        TypeProj(7, 8)
//      ),
//      4 -> TypeDecl(5, Bot, RecType(6, Bot)),
//      7 -> TypeDecl(8, Bot, TypeProj(4, 5))
//    ),
//    9
//  ),
//  Sel(0, 3),
//  Que,
//  TypedSel(0, 3) :- TypeProj(4, 5)
//)


//InferenceProblem(
//  GlobalContext(
//    Map(4 -> TypeDecl(5, Bot, Bot), 8 -> TypeProj(4, 5)),
//    9
//  ),
//  Obj(
//    0,
//    AndType(
//      AndType(
//        TypeDecl(1, Bot, Bot),
//        TypeDecl(2, TypeProj(0, 1), TypeProj(0, 1))
//      ),
//      AndType(
//        AndType(
//          AndType(
//            TypeDecl(1, Bot, Bot),
//            TypeDecl(2, TypeProj(0, 1), TypeProj(0, 1))
//          ),
//          FieldDecl(3, TypeDecl(5, Bot, Bot))
//        ),
//        FieldDecl(6, TypeProj(4, 5))
//      )
//    ),
//    AndDef(
//      AndDef(TypeDef(1, Bot), TypeDef(2, TypeProj(0, 1))),
//      AndDef(
//        FieldDef(3, Var(4)),
//        FieldDef(6, Let(7, Var(8), Var(8)))
//      )
//    )
//  ),
//  Que,
//  TypedObj(
//    0,
//    AndType(
//      AndType(
//        TypeDecl(1, Bot, Bot),
//        TypeDecl(2, TypeProj(0, 1), TypeProj(0, 1))
//      ),
//      AndType(
//        AndType(
//          AndType(
//            TypeDecl(1, Bot, Bot),
//            TypeDecl(2, TypeProj(0, 1), TypeProj(0, 1))
//          ),
//          FieldDecl(3, TypeDecl(5, Bot, Bot))
//        ),
//        FieldDecl(6, TypeProj(4, 5))
//      )
//    ),
//    TypedAndDef(
//      TypedAndDef(
//        TypedTypeDef(1, Bot) :- TypeDecl(1, Bot, Bot),
//        TypedTypeDef(2, TypeProj(0, 1)) :- TypeDecl(2, TypeProj(0, 1), TypeProj(0, 1))
//      ) :- AndType(
//        TypeDecl(1, Bot, Bot),
//        TypeDecl(2, TypeProj(0, 1), TypeProj(0, 1))
//      ),
//      TypedAndDef(
//        TypedFieldDef(3, TypedVar(4) :- TypeDecl(5, Bot, Bot)) :- FieldDecl(
//          3,
//          TypeDecl(5, Bot, Bot)
//        ),
//        TypedFieldDef(
//          6,
//          TypedLet(
//            7,
//            TypedVar(8) :- TypeProj(4, 5),
//            TypedVar(8) :- TypeProj(4, 5)
//          ) :- TypeProj(4, 5)
//        ) :- FieldDecl(6, TypeProj(4, 5))
//      ) :- AndType(
//        FieldDecl(3, TypeDecl(5, Bot, Bot)),
//        FieldDecl(6, TypeProj(4, 5))
//      )
//    ) :- AndType(
//      AndType(
//        TypeDecl(1, Bot, Bot),
//        TypeDecl(2, TypeProj(0, 1), TypeProj(0, 1))
//      ),
//      AndType(
//        AndType(
//          AndType(
//            TypeDecl(1, Bot, Bot),
//            TypeDecl(2, TypeProj(0, 1), TypeProj(0, 1))
//          ),
//          FieldDecl(3, TypeDecl(5, Bot, Bot))
//        ),
//        FieldDecl(6, TypeProj(4, 5))
//      )
//    )
//  ) :- RecType(
//    0,
//    AndType(
//      AndType(
//        TypeDecl(1, Bot, Bot),
//        TypeDecl(2, TypeProj(0, 1), TypeProj(0, 1))
//      ),
//      AndType(
//        AndType(
//          AndType(
//            TypeDecl(1, Bot, Bot),
//            TypeDecl(2, TypeProj(0, 1), TypeProj(0, 1))
//          ),
//          FieldDecl(3, TypeDecl(5, Bot, Bot))
//        ),
//        FieldDecl(6, TypeProj(4, 5))
//      )
//    )
//  )
//)

//InferenceProblem( // TODO
//  GlobalContext(
//    Map(
//      5 -> TypeDecl(
//        6,
//        TypeProj(11, 12),
//        FunType(
//          7,
//          TypeDecl(8, TypeProj(9, 10), AndType(Top, Top)),
//          TypeProj(7, 8)
//        )
//      ),
//      9 -> TypeDecl(10, Bot, Bot),
//      32 -> TypeProj(11, 12),
//      31 -> FunType(
//        3,
//        RecType(4, TypeProj(5, 6)),
//        RecType(
//          13,
//          FieldDecl(14, FunType(15, TypeProj(11, 12), Top))
//        )
//      ),
//      11 -> TypeDecl(
//        12,
//        Bot,
//        FunType(
//          7,
//          TypeDecl(8, TypeProj(9, 10), AndType(Top, Top)),
//          TypeProj(7, 8)
//        )
//      )
//    ),
//    33
//  ),
//  Obj(
//    1,
//    AndType(
//      FieldDecl(
//        2,
//        FunType(
//          3,
//          RecType(4, TypeProj(5, 6)),
//          RecType(
//            13,
//            FieldDecl(14, FunType(15, TypeProj(11, 12), Top))
//          )
//        )
//      ),
//      AndType(
//        AndType(
//          FieldDecl(
//            2,
//            FunType(
//              3,
//              RecType(4, TypeProj(5, 6)),
//              RecType(
//                13,
//                FieldDecl(14, FunType(15, TypeProj(11, 12), Top))
//              )
//            )
//          ),
//          TypeDecl(
//            20,
//            FunType(
//              21,
//              TypeDecl(22, Bot, RecType(23, TypeProj(5, 6))),
//              TypeDecl(24, Bot, FieldDecl(25, Bot))
//            ),
//            FunType(
//              21,
//              TypeDecl(22, Bot, RecType(23, TypeProj(5, 6))),
//              TypeDecl(24, Bot, FieldDecl(25, Bot))
//            )
//          )
//        ),
//        FieldDecl(26, TypeProj(11, 12))
//      )
//    ),
//    AndDef(
//      FieldDef(2, Var(31)),
//      AndDef(
//        TypeDef(
//          20,
//          FunType(
//            21,
//            TypeDecl(22, Bot, RecType(23, TypeProj(5, 6))),
//            TypeDecl(24, Bot, FieldDecl(25, Bot))
//          )
//        ),
//        FieldDef(26, Var(32))
//      )
//    )
//  ),
//  Que,
//  TypedObj(
//    1,
//    AndType(
//      FieldDecl(
//        2,
//        FunType(
//          3,
//          RecType(4, TypeProj(5, 6)),
//          RecType(
//            13,
//            FieldDecl(14, FunType(15, TypeProj(11, 12), Top))
//          )
//        )
//      ),
//      AndType(
//        AndType(
//          FieldDecl(
//            2,
//            FunType(
//              3,
//              RecType(4, TypeProj(5, 6)),
//              RecType(
//                13,
//                FieldDecl(14, FunType(15, TypeProj(11, 12), Top))
//              )
//            )
//          ),
//          TypeDecl(
//            20,
//            FunType(
//              21,
//              TypeDecl(22, Bot, RecType(23, TypeProj(5, 6))),
//              TypeDecl(24, Bot, FieldDecl(25, Bot))
//            ),
//            FunType(
//              21,
//              TypeDecl(22, Bot, RecType(23, TypeProj(5, 6))),
//              TypeDecl(24, Bot, FieldDecl(25, Bot))
//            )
//          )
//        ),
//        FieldDecl(26, TypeProj(11, 12))
//      )
//    ),
//    TypedAndDef(
//      TypedFieldDef(
//        2,
//        TypedVar(31) :- FunType(
//          3,
//          RecType(4, TypeProj(5, 6)),
//          RecType(
//            13,
//            FieldDecl(14, FunType(15, TypeProj(11, 12), Top))
//          )
//        )
//      ) :- FieldDecl(
//        2,
//        FunType(
//          3,
//          RecType(4, TypeProj(5, 6)),
//          RecType(
//            13,
//            FieldDecl(14, FunType(15, TypeProj(11, 12), Top))
//          )
//        )
//      ),
//      TypedAndDef(
//        TypedTypeDef(
//          20,
//          FunType(
//            21,
//            TypeDecl(22, Bot, RecType(23, TypeProj(5, 6))),
//            TypeDecl(24, Bot, FieldDecl(25, Bot))
//          )
//        ) :- TypeDecl(
//          20,
//          FunType(
//            21,
//            TypeDecl(22, Bot, RecType(23, TypeProj(5, 6))),
//            TypeDecl(24, Bot, FieldDecl(25, Bot))
//          ),
//          FunType(
//            21,
//            TypeDecl(22, Bot, RecType(23, TypeProj(5, 6))),
//            TypeDecl(24, Bot, FieldDecl(25, Bot))
//          )
//        ),
//        TypedFieldDef(26, TypedVar(32) :- TypeProj(11, 12)) :- FieldDecl(26, TypeProj(11, 12))
//      ) :- AndType(
//        AndType(
//          FieldDecl(
//            2,
//            FunType(
//              3,
//              RecType(4, TypeProj(5, 6)),
//              RecType(
//                13,
//                FieldDecl(14, FunType(15, TypeProj(11, 12), Top))
//              )
//            )
//          ),
//          TypeDecl(
//            20,
//            FunType(
//              21,
//              TypeDecl(22, Bot, RecType(23, TypeProj(5, 6))),
//              TypeDecl(24, Bot, FieldDecl(25, Bot))
//            ),
//            FunType(
//              21,
//              TypeDecl(22, Bot, RecType(23, TypeProj(5, 6))),
//              TypeDecl(24, Bot, FieldDecl(25, Bot))
//            )
//          )
//        ),
//        FieldDecl(26, TypeProj(11, 12))
//      )
//    ) :- AndType(
//      FieldDecl(
//        2,
//        FunType(
//          3,
//          RecType(4, TypeProj(5, 6)),
//          RecType(
//            13,
//            FieldDecl(14, FunType(15, TypeProj(11, 12), Top))
//          )
//        )
//      ),
//      AndType(
//        AndType(
//          FieldDecl(
//            2,
//            FunType(
//              3,
//              RecType(4, TypeProj(5, 6)),
//              RecType(
//                13,
//                FieldDecl(14, FunType(15, TypeProj(11, 12), Top))
//              )
//            )
//          ),
//          TypeDecl(
//            20,
//            FunType(
//              21,
//              TypeDecl(22, Bot, RecType(23, TypeProj(5, 6))),
//              TypeDecl(24, Bot, FieldDecl(25, Bot))
//            ),
//            FunType(
//              21,
//              TypeDecl(22, Bot, RecType(23, TypeProj(5, 6))),
//              TypeDecl(24, Bot, FieldDecl(25, Bot))
//            )
//          )
//        ),
//        FieldDecl(26, TypeProj(11, 12))
//      )
//    )
//  ) :- RecType(
//    1,
//    AndType(
//      FieldDecl(
//        2,
//        FunType(
//          3,
//          RecType(4, TypeProj(5, 6)),
//          RecType(
//            13,
//            FieldDecl(14, FunType(15, TypeProj(11, 12), Top))
//          )
//        )
//      ),
//      AndType(
//        AndType(
//          FieldDecl(
//            2,
//            FunType(
//              3,
//              RecType(4, TypeProj(5, 6)),
//              RecType(
//                13,
//                FieldDecl(14, FunType(15, TypeProj(11, 12), Top))
//              )
//            )
//          ),
//          TypeDecl(
//            20,
//            FunType(
//              21,
//              TypeDecl(22, Bot, RecType(23, TypeProj(5, 6))),
//              TypeDecl(24, Bot, FieldDecl(25, Bot))
//            ),
//            FunType(
//              21,
//              TypeDecl(22, Bot, RecType(23, TypeProj(5, 6))),
//              TypeDecl(24, Bot, FieldDecl(25, Bot))
//            )
//          )
//        ),
//        FieldDecl(26, TypeProj(11, 12))
//      )
//    )
//  )
//)

//InferenceProblem(
//  GlobalContext(Map(2 -> FunType(4, Top, Top), 3 -> Top), 5),
//  Obj(0, FieldDecl(1, Top), FieldDef(1, App(2, 3))),
//  Que,
//  TypedObj(
//    0,
//    FieldDecl(1, Top),
//    TypedFieldDef(1, TypedApp(2, 3) :- Top) :- FieldDecl(1, Top)
//  ) :- RecType(0, FieldDecl(1, Top))
//)

//InferenceProblem(
//  GlobalContext(
//    Map(
//      74 -> FunType(
//        51,
//        RecType(
//          52,
//          RecType(
//            53,
//            RecType(
//              54,
//              AndType(
//                TypeDecl(55, Bot, Bot),
//                FieldDecl(56, TypeProj(18, 19))
//              )
//            )
//          )
//        ),
//        TypeDecl(40, Bot, Top)
//      ),
//      73 -> RecType(
//        34,
//        TypeDecl(
//          35,
//          RecType(
//            36,
//            FunType(
//              37,
//              FieldDecl(38, TypeProj(39, 40)),
//              TypeDecl(41, Bot, Top)
//            )
//          ),
//          RecType(
//            36,
//            FunType(
//              37,
//              FieldDecl(38, TypeProj(39, 40)),
//              TypeDecl(41, Bot, Top)
//            )
//          )
//        )
//      ),
//      71 -> FieldDecl(23, AndType(Top, Bot)),
//      12 -> TypeDecl(
//        13,
//        Bot,
//        RecType(
//          14,
//          AndType(
//            TypeDecl(15, AndType(Bot, TypeDecl(16, Top, Top)), Bot),
//            FieldDecl(17, TypeProj(14, 15))
//          )
//        )
//      ),
//      39 -> TypeDecl(40, Bot, Top),
//      18 -> TypeDecl(
//        19,
//        AndType(Bot, RecType(20, RecType(21, TypeProj(12, 13)))),
//        Bot
//      ),
//      72 -> FieldDecl(30, RecType(31, Top))
//    ),
//    75
//  ),
//  Obj(
//    2,
//    AndType(
//      AndType(
//        AndType(
//          FieldDecl(3, FieldDecl(23, AndType(Top, Bot))),
//          FieldDecl(26, FieldDecl(30, RecType(31, Top)))
//        ),
//        FieldDecl(
//          32,
//          FunType(
//            42,
//            RecType(
//              36,
//              FunType(
//                37,
//                FieldDecl(38, TypeProj(39, 40)),
//                TypeDecl(41, Bot, Top)
//              )
//            ),
//            FunType(
//              43,
//              AndType(Top, TypeProj(18, 19)),
//              FunType(
//                44,
//                RecType(
//                  45,
//                  RecType(
//                    46,
//                    AndType(
//                      FieldDecl(47, RecType(48, Top)),
//                      TypeDecl(49, Bot, Top)
//                    )
//                  )
//                ),
//                Top
//              )
//            )
//          )
//        )
//      ),
//      TypeDecl(59, Top, Top)
//    ),
//    AndDef(
//      AndDef(
//        AndDef(FieldDef(3, Var(71)), FieldDef(26, Var(72))),
//        FieldDef(
//          32,
//          Let(
//            33,
//            Var(73),
//            Fun(
//              42,
//              TypeProj(33, 35),
//              Fun(
//                43,
//                AndType(Top, TypeProj(18, 19)),
//                Fun(
//                  44,
//                  RecType(
//                    45,
//                    RecType(
//                      46,
//                      AndType(
//                        FieldDecl(47, RecType(48, Top)),
//                        TypeDecl(49, Bot, Top)
//                      )
//                    )
//                  ),
//                  Let(
//                    50,
//                    Var(74),
//                    Obj(
//                      57,
//                      TypeDecl(
//                        58,
//                        TypeProj(33, 35),
//                        TypeProj(33, 35)
//                      ),
//                      TypeDef(58, TypeProj(33, 35))
//                    )
//                  )
//                )
//              )
//            )
//          )
//        )
//      ),
//      TypeDef(59, Top)
//    )
//  ),
//  Que,
//  TypedObj(
//    2,
//    AndType(
//      AndType(
//        AndType(
//          FieldDecl(3, FieldDecl(23, AndType(Top, Bot))),
//          FieldDecl(26, FieldDecl(30, RecType(31, Top)))
//        ),
//        FieldDecl(
//          32,
//          FunType(
//            42,
//            RecType(
//              36,
//              FunType(
//                37,
//                FieldDecl(38, TypeProj(39, 40)),
//                TypeDecl(41, Bot, Top)
//              )
//            ),
//            FunType(
//              43,
//              AndType(Top, TypeProj(18, 19)),
//              FunType(
//                44,
//                RecType(
//                  45,
//                  RecType(
//                    46,
//                    AndType(
//                      FieldDecl(47, RecType(48, Top)),
//                      TypeDecl(49, Bot, Top)
//                    )
//                  )
//                ),
//                Top
//              )
//            )
//          )
//        )
//      ),
//      TypeDecl(59, Top, Top)
//    ),
//    TypedAndDef(
//      TypedAndDef(
//        TypedAndDef(
//          TypedFieldDef(
//            3,
//            TypedVar(71) :- FieldDecl(23, AndType(Top, Bot))
//          ) :- FieldDecl(3, FieldDecl(23, AndType(Top, Bot))),
//          TypedFieldDef(
//            26,
//            TypedVar(72) :- FieldDecl(30, RecType(31, Top))
//          ) :- FieldDecl(26, FieldDecl(30, RecType(31, Top)))
//        ) :- AndType(
//          FieldDecl(3, FieldDecl(23, AndType(Top, Bot))),
//          FieldDecl(26, FieldDecl(30, RecType(31, Top)))
//        ),
//        TypedFieldDef(
//          32,
//          TypedLet(
//            33,
//            TypedVar(73) :- RecType(
//              34,
//              TypeDecl(
//                35,
//                RecType(
//                  36,
//                  FunType(
//                    37,
//                    FieldDecl(38, TypeProj(39, 40)),
//                    TypeDecl(41, Bot, Top)
//                  )
//                ),
//                RecType(
//                  36,
//                  FunType(
//                    37,
//                    FieldDecl(38, TypeProj(39, 40)),
//                    TypeDecl(41, Bot, Top)
//                  )
//                )
//              )
//            ),
//            TypedFun(
//              42,
//              TypeProj(33, 35),
//              TypedFun(
//                43,
//                AndType(Top, TypeProj(18, 19)),
//                TypedFun(
//                  44,
//                  RecType(
//                    45,
//                    RecType(
//                      46,
//                      AndType(
//                        FieldDecl(47, RecType(48, Top)),
//                        TypeDecl(49, Bot, Top)
//                      )
//                    )
//                  ),
//                  TypedLet(
//                    50,
//                    TypedVar(74) :- FunType(
//                      51,
//                      RecType(
//                        52,
//                        RecType(
//                          53,
//                          RecType(
//                            54,
//                            AndType(
//                              TypeDecl(55, Bot, Bot),
//                              FieldDecl(56, TypeProj(18, 19))
//                            )
//                          )
//                        )
//                      ),
//                      TypeDecl(40, Bot, Top)
//                    ),
//                    TypedObj(
//                      57,
//                      TypeDecl(
//                        58,
//                        TypeProj(33, 35),
//                        TypeProj(33, 35)
//                      ),
//                      TypedTypeDef(58, TypeProj(33, 35)) :- TypeDecl(58, TypeProj(33, 35), TypeProj(33, 35))
//                    ) :- RecType(
//                      57,
//                      TypeDecl(
//                        58,
//                        TypeProj(33, 35),
//                        TypeProj(33, 35)
//                      )
//                    )
//                  ) :- RecType(
//                    57,
//                    TypeDecl(58, TypeProj(33, 35), TypeProj(33, 35))
//                  )
//                ) :- FunType(
//                  44,
//                  RecType(
//                    45,
//                    RecType(
//                      46,
//                      AndType(
//                        FieldDecl(47, RecType(48, Top)),
//                        TypeDecl(49, Bot, Top)
//                      )
//                    )
//                  ),
//                  RecType(
//                    57,
//                    TypeDecl(58, TypeProj(33, 35), TypeProj(33, 35))
//                  )
//                )
//              ) :- FunType(
//                43,
//                AndType(Top, TypeProj(18, 19)),
//                FunType(
//                  44,
//                  RecType(
//                    45,
//                    RecType(
//                      46,
//                      AndType(
//                        FieldDecl(47, RecType(48, Top)),
//                        TypeDecl(49, Bot, Top)
//                      )
//                    )
//                  ),
//                  RecType(
//                    57,
//                    TypeDecl(58, TypeProj(33, 35), TypeProj(33, 35))
//                  )
//                )
//              )
//            ) :- FunType(
//              42,
//              TypeProj(33, 35),
//              FunType(
//                43,
//                AndType(Top, TypeProj(18, 19)),
//                FunType(
//                  44,
//                  RecType(
//                    45,
//                    RecType(
//                      46,
//                      AndType(
//                        FieldDecl(47, RecType(48, Top)),
//                        TypeDecl(49, Bot, Top)
//                      )
//                    )
//                  ),
//                  RecType(
//                    57,
//                    TypeDecl(58, TypeProj(33, 35), TypeProj(33, 35))
//                  )
//                )
//              )
//            )
//          ) :- FunType(
//            42,
//            RecType(
//              36,
//              FunType(
//                37,
//                FieldDecl(38, TypeProj(39, 40)),
//                TypeDecl(41, Bot, Top)
//              )
//            ),
//            FunType(
//              43,
//              AndType(Top, TypeProj(18, 19)),
//              FunType(
//                44,
//                RecType(
//                  45,
//                  RecType(
//                    46,
//                    AndType(
//                      FieldDecl(47, RecType(48, Top)),
//                      TypeDecl(49, Bot, Top)
//                    )
//                  )
//                ),
//                Top
//              )
//            )
//          )
//        ) :- FieldDecl(
//          32,
//          FunType(
//            42,
//            RecType(
//              36,
//              FunType(
//                37,
//                FieldDecl(38, TypeProj(39, 40)),
//                TypeDecl(41, Bot, Top)
//              )
//            ),
//            FunType(
//              43,
//              AndType(Top, TypeProj(18, 19)),
//              FunType(
//                44,
//                RecType(
//                  45,
//                  RecType(
//                    46,
//                    AndType(
//                      FieldDecl(47, RecType(48, Top)),
//                      TypeDecl(49, Bot, Top)
//                    )
//                  )
//                ),
//                Top
//              )
//            )
//          )
//        )
//      ) :- AndType(
//        AndType(
//          FieldDecl(3, FieldDecl(23, AndType(Top, Bot))),
//          FieldDecl(26, FieldDecl(30, RecType(31, Top)))
//        ),
//        FieldDecl(
//          32,
//          FunType(
//            42,
//            RecType(
//              36,
//              FunType(
//                37,
//                FieldDecl(38, TypeProj(39, 40)),
//                TypeDecl(41, Bot, Top)
//              )
//            ),
//            FunType(
//              43,
//              AndType(Top, TypeProj(18, 19)),
//              FunType(
//                44,
//                RecType(
//                  45,
//                  RecType(
//                    46,
//                    AndType(
//                      FieldDecl(47, RecType(48, Top)),
//                      TypeDecl(49, Bot, Top)
//                    )
//                  )
//                ),
//                Top
//              )
//            )
//          )
//        )
//      ),
//      TypedTypeDef(59, Top) :- TypeDecl(59, Top, Top)
//    ) :- AndType(
//      AndType(
//        AndType(
//          FieldDecl(3, FieldDecl(23, AndType(Top, Bot))),
//          FieldDecl(26, FieldDecl(30, RecType(31, Top)))
//        ),
//        FieldDecl(
//          32,
//          FunType(
//            42,
//            RecType(
//              36,
//              FunType(
//                37,
//                FieldDecl(38, TypeProj(39, 40)),
//                TypeDecl(41, Bot, Top)
//              )
//            ),
//            FunType(
//              43,
//              AndType(Top, TypeProj(18, 19)),
//              FunType(
//                44,
//                RecType(
//                  45,
//                  RecType(
//                    46,
//                    AndType(
//                      FieldDecl(47, RecType(48, Top)),
//                      TypeDecl(49, Bot, Top)
//                    )
//                  )
//                ),
//                Top
//              )
//            )
//          )
//        )
//      ),
//      TypeDecl(59, Top, Top)
//    )
//  ) :- RecType(
//    2,
//    AndType(
//      AndType(
//        AndType(
//          FieldDecl(3, FieldDecl(23, AndType(Top, Bot))),
//          FieldDecl(26, FieldDecl(30, RecType(31, Top)))
//        ),
//        FieldDecl(
//          32,
//          FunType(
//            42,
//            RecType(
//              36,
//              FunType(
//                37,
//                FieldDecl(38, TypeProj(39, 40)),
//                TypeDecl(41, Bot, Top)
//              )
//            ),
//            FunType(
//              43,
//              AndType(Top, TypeProj(18, 19)),
//              FunType(
//                44,
//                RecType(
//                  45,
//                  RecType(
//                    46,
//                    AndType(
//                      FieldDecl(47, RecType(48, Top)),
//                      TypeDecl(49, Bot, Top)
//                    )
//                  )
//                ),
//                Top
//              )
//            )
//          )
//        )
//      ),
//      TypeDecl(59, Top, Top)
//    )
//  )
//)

//InferenceProblem(
//  GlobalContext(
//    Map(
//      1 -> FieldDecl(
//        2,
//        AndType(
//          Bot,
//          RecType(
//            3,
//            AndType(
//              TypeDecl(4, Bot, Bot),
//              FieldDecl(5, RecType(6, TypeProj(7, 8)))
//            )
//          )
//        )
//      ),
//      7 -> TypeDecl(8, Bot, Bot)
//    ),
//    12
//  ),
//  Sel(1, 2),
//  Que,
//  TypedSel(1, 2) :- Bot
//)

//InferenceProblem(
//  GlobalContext(
//    Map(
//      35 -> RecType(
//        25,
//        TypeDecl(
//          26,
//          TypeDecl(27, AndType(Bot, RecType(28, Top)), Bot),
//          TypeDecl(27, AndType(Bot, RecType(28, Top)), Bot)
//        )
//      )
//    ),
//    36
//  ),
//  Obj(
//    21,
//    AndType(TypeDecl(22, Top, Top), FieldDecl(23, Top)),
//    AndDef(
//      TypeDef(22, Top),
//      FieldDef( // NOTE: p=Top, but tc gives res=RecType(...). // TODO generator needs to handle prototypes.
//        23,
//        Let(
//          24,
//          Var(35),
//          Obj(
//            29,
//            TypeDecl(30, TypeProj(24, 26), TypeProj(24, 26)),
//            TypeDef(30, TypeProj(24, 26))
//          )
//        )
//      )
//    )
//  ),
//  Que,
//  TypedObj(
//    21,
//    AndType(TypeDecl(22, Top, Top), FieldDecl(23, Top)),
//    TypedAndDef(
//      TypedTypeDef(22, Top) :- TypeDecl(22, Top, Top),
//      TypedFieldDef(
//        23,
//        TypedLet(
//          24,
//          TypedVar(35) :- RecType(
//            25,
//            TypeDecl(
//              26,
//              TypeDecl(27, AndType(Bot, RecType(28, Top)), Bot),
//              TypeDecl(27, AndType(Bot, RecType(28, Top)), Bot)
//            )
//          ),
//          TypedObj(
//            29,
//            TypeDecl(30, TypeProj(24, 26), TypeProj(24, 26)),
//            TypedTypeDef(30, TypeProj(24, 26)) :- TypeDecl(30, TypeProj(24, 26), TypeProj(24, 26))
//          ) :- RecType(
//            29,
//            TypeDecl(30, TypeProj(24, 26), TypeProj(24, 26))
//          )
//        ) :- Top
//      ) :- FieldDecl(23, Top)
//    ) :- AndType(TypeDecl(22, Top, Top), FieldDecl(23, Top))
//  ) :- RecType(21, AndType(TypeDecl(22, Top, Top), FieldDecl(23, Top)))
//)


//InferenceProblem(
//  GlobalContext(
//    Map(
//      2 -> AndType(
//        AndType(
//          FieldDecl(
//            3,
//            FunType(5, TypeProj(6, 7), FieldDecl(10, Bot))
//          ),
//          TypeDecl(
//            11,
//            RecType(12, TypeProj(6, 7)),
//            RecType(12, TypeProj(6, 7))
//          )
//        ),
//        AndType(
//          AndType(
//            TypeDecl(13, TypeProj(6, 7), TypeProj(6, 7)),
//            TypeDecl(14, TypeProj(2, 13), TypeProj(2, 13))
//          ),
//          FieldDecl(
//            15,
//            RecType(
//              16,
//              AndType(
//                TypeDecl(17, Bot, Bot),
//                FieldDecl(
//                  18,
//                  FunType(
//                    24,
//                    AndType(
//                      FieldDecl(25, Top),
//                      TypeDecl(26, Bot, TypeProj(2, 13))
//                    ),
//                    AndType(Top, TypeProj(31, 32))
//                  )
//                )
//              )
//            )
//          )
//        )
//      ),
//      6 -> TypeDecl(
//        7,
//        FieldDecl(8, Bot),
//        FieldDecl(8, FieldDecl(9, AndType(Top, Top)))
//      ),
//      31 -> TypeDecl(32, TypeProj(6, 7), TypeProj(6, 7))
//    ),
//    33
//  ),
//  Var(2),
//  Que,
//  TypedVar(2) :- AndType( // NOTE: expected is wrong. FieldDecl(15, ...) is missing.
//    AndType(
//      FieldDecl(3, FunType(5, TypeProj(6, 7), FieldDecl(10, Bot))),
//      TypeDecl(
//        11,
//        RecType(12, TypeProj(6, 7)),
//        RecType(12, TypeProj(6, 7))
//      )
//    ),
//    AndType(
//      TypeDecl(13, TypeProj(6, 7), TypeProj(6, 7)),
//      TypeDecl(14, TypeProj(2, 13), TypeProj(2, 13))
//    )
//  )
//)

//InferenceProblem(
//  GlobalContext(
//    Map(
//      6 -> TypeDecl(7, FieldDecl(8, Bot), FieldDecl(8, FieldDecl(9, AndType(Top, Top)))),
//      28 -> Bot,
//      27 -> FunType(
//        29,
//        FunType(30, TypeProj(31, 32), Top),
//        AndType(Top, TypeProj(31, 32))
//      ),
//      31 -> TypeDecl(32, TypeProj(6, 7), TypeProj(6, 7)),
//      23 -> TypeProj(6, 7),
//      4 -> FunType(5, TypeProj(6, 7), FieldDecl(10, Bot))
//    ),
//    33
//  ),
//  Obj(
//    0,
//    FieldDecl(
//      1,
//      RecType(
//        2,
//        AndType(
//          AndType(
//            FieldDecl(3, FunType(5, TypeProj(6, 7), FieldDecl(10, Bot))),
//            TypeDecl(
//              11,
//              RecType(12, TypeProj(6, 7)),
//              RecType(12, TypeProj(6, 7))
//            )
//          ),
//          AndType(
//            AndType(
//              TypeDecl(13, TypeProj(6, 7), TypeProj(6, 7)),
//              TypeDecl(14, TypeProj(2, 13), TypeProj(2, 13))
//            ),
//            FieldDecl(
//              15,
//              RecType(
//                16,
//                AndType(
//                  TypeDecl(17, Bot, Bot),
//                  FieldDecl(
//                    18,
//                    FunType(
//                      24,
//                      AndType(
//                        FieldDecl(25, Top),
//                        TypeDecl(26, Bot, TypeProj(2, 13))
//                      ),
//                      AndType(Top, TypeProj(31, 32))
//                    )
//                  )
//                )
//              )
//            )
//          )
//        )
//      )
//    ),
//    FieldDef(
//      1,
//      Obj(
//        2,
//        AndType(
//          AndType(
//            FieldDecl(3, FunType(5, TypeProj(6, 7), FieldDecl(10, Bot))),
//            TypeDecl(
//              11,
//              RecType(12, TypeProj(6, 7)),
//              RecType(12, TypeProj(6, 7))
//            )
//          ),
//          AndType(
//            AndType(
//              TypeDecl(13, TypeProj(6, 7), TypeProj(6, 7)),
//              TypeDecl(14, TypeProj(2, 13), TypeProj(2, 13))
//            ),
//            FieldDecl(
//              15,
//              RecType(
//                16,
//                AndType(
//                  TypeDecl(17, Bot, Bot),
//                  FieldDecl(
//                    18,
//                    FunType(
//                      24,
//                      AndType(
//                        FieldDecl(25, Top),
//                        TypeDecl(26, Bot, TypeProj(2, 13))
//                      ),
//                      AndType(Top, TypeProj(31, 32))
//                    )
//                  )
//                )
//              )
//            )
//          )
//        ),
//        AndDef(
//          AndDef(
//            FieldDef(3, Var(4)),
//            TypeDef(11, RecType(12, TypeProj(6, 7)))
//          ),
//          AndDef(
//            AndDef(TypeDef(13, TypeProj(6, 7)), TypeDef(14, TypeProj(2, 13))),
//            FieldDef(
//              15,
//              Obj(
//                16,
//                AndType(
//                  TypeDecl(17, Bot, Bot),
//                  FieldDecl(
//                    18,
//                    FunType(
//                      24,
//                      AndType(
//                        FieldDecl(25, Top),
//                        TypeDecl(26, Bot, TypeProj(2, 13))
//                      ),
//                      AndType(Top, TypeProj(31, 32))
//                    )
//                  )
//                ),
//                AndDef(
//                  TypeDef(17, Bot),
//                  FieldDef(
//                    18,
//                    Let(
//                      19,
//                      Var(2),
//                      Let(
//                        20,
//                        Obj(
//                          21,
//                          FieldDecl(22, FieldDecl(10, Bot)),
//                          FieldDef(22, App(4, 23))
//                        ),
//                        Fun(
//                          24,
//                          AndType(
//                            FieldDecl(25, Top),
//                            TypeDecl(26, Bot, TypeProj(2, 13))
//                          ),
//                          App(27, 28)
//                        )
//                      )
//                    )
//                  )
//                )
//              )
//            )
//          )
//        )
//      )
//    )
//  ),
//  Que,
//  TypedObj(
//    0,
//    FieldDecl(
//      1,
//      RecType(
//        2,
//        AndType(
//          AndType(
//            FieldDecl(3, FunType(5, TypeProj(6, 7), FieldDecl(10, Bot))),
//            TypeDecl(
//              11,
//              RecType(12, TypeProj(6, 7)),
//              RecType(12, TypeProj(6, 7))
//            )
//          ),
//          AndType(
//            AndType(
//              TypeDecl(13, TypeProj(6, 7), TypeProj(6, 7)),
//              TypeDecl(14, TypeProj(2, 13), TypeProj(2, 13))
//            ),
//            FieldDecl(
//              15,
//              RecType(
//                16,
//                AndType(
//                  TypeDecl(17, Bot, Bot),
//                  FieldDecl(
//                    18,
//                    FunType(
//                      24,
//                      AndType(
//                        FieldDecl(25, Top),
//                        TypeDecl(26, Bot, TypeProj(2, 13))
//                      ),
//                      AndType(Top, TypeProj(31, 32))
//                    )
//                  )
//                )
//              )
//            )
//          )
//        )
//      )
//    ),
//    TypedFieldDef(
//      1,
//      TypedObj(
//        2,
//        AndType(
//          AndType(
//            FieldDecl(3, FunType(5, TypeProj(6, 7), FieldDecl(10, Bot))),
//            TypeDecl(
//              11,
//              RecType(12, TypeProj(6, 7)),
//              RecType(12, TypeProj(6, 7))
//            )
//          ),
//          AndType(
//            AndType(
//              TypeDecl(13, TypeProj(6, 7), TypeProj(6, 7)),
//              TypeDecl(14, TypeProj(2, 13), TypeProj(2, 13))
//            ),
//            FieldDecl(
//              15,
//              RecType(
//                16,
//                AndType(
//                  TypeDecl(17, Bot, Bot),
//                  FieldDecl(
//                    18,
//                    FunType(
//                      24,
//                      AndType(
//                        FieldDecl(25, Top),
//                        TypeDecl(26, Bot, TypeProj(2, 13))
//                      ),
//                      AndType(Top, TypeProj(31, 32))
//                    )
//                  )
//                )
//              )
//            )
//          )
//        ),
//        TypedAndDef(
//          TypedAndDef(
//            TypedFieldDef(
//              3,
//              TypedVar(4) :- FunType(5, TypeProj(6, 7), FieldDecl(10, Bot))
//            ) :- FieldDecl(3, FunType(5, TypeProj(6, 7), FieldDecl(10, Bot))),
//            TypedTypeDef(11, RecType(12, TypeProj(6, 7))) :- TypeDecl(
//              11,
//              RecType(12, TypeProj(6, 7)),
//              RecType(12, TypeProj(6, 7))
//            )
//          ) :- AndType(
//            FieldDecl(3, FunType(5, TypeProj(6, 7), FieldDecl(10, Bot))),
//            TypeDecl(
//              11,
//              RecType(12, TypeProj(6, 7)),
//              RecType(12, TypeProj(6, 7))
//            )
//          ),
//          TypedAndDef(
//            TypedAndDef(
//              TypedTypeDef(13, TypeProj(6, 7)) :- TypeDecl(13, TypeProj(6, 7), TypeProj(6, 7)),
//              TypedTypeDef(14, TypeProj(2, 13)) :- TypeDecl(14, TypeProj(2, 13), TypeProj(2, 13))
//            ) :- AndType(
//              TypeDecl(13, TypeProj(6, 7), TypeProj(6, 7)),
//              TypeDecl(14, TypeProj(2, 13), TypeProj(2, 13))
//            ),
//            TypedFieldDef(
//              15,
//              TypedObj(
//                16,
//                AndType(
//                  TypeDecl(17, Bot, Bot),
//                  FieldDecl(
//                    18,
//                    FunType(
//                      24,
//                      AndType(
//                        FieldDecl(25, Top),
//                        TypeDecl(26, Bot, TypeProj(2, 13))
//                      ),
//                      AndType(Top, TypeProj(31, 32))
//                    )
//                  )
//                ),
//                TypedAndDef(
//                  TypedTypeDef(17, Bot) :- TypeDecl(17, Bot, Bot),
//                  TypedFieldDef(
//                    18,
//                    TypedLet(
//                      19,
//                      TypedVar(2) :- AndType(
//                        AndType(
//                          FieldDecl(
//                            3,
//                            FunType(5, TypeProj(6, 7), FieldDecl(10, Bot))
//                          ),
//                          TypeDecl(
//                            11,
//                            RecType(12, TypeProj(6, 7)),
//                            RecType(12, TypeProj(6, 7))
//                          )
//                        ),
//                        AndType(
//                          TypeDecl(13, TypeProj(6, 7), TypeProj(6, 7)),
//                          TypeDecl(14, TypeProj(2, 13), TypeProj(2, 13))
//                        )
//                      ),
//                      TypedLet(
//                        20,
//                        TypedObj(
//                          21,
//                          FieldDecl(22, FieldDecl(10, Bot)),
//                          TypedFieldDef(
//                            22,
//                            TypedApp(4, 23) :- FieldDecl(10, Bot)
//                          ) :- FieldDecl(22, FieldDecl(10, Bot))
//                        ) :- RecType(21, FieldDecl(22, FieldDecl(10, Bot))),
//                        TypedFun(
//                          24,
//                          AndType(
//                            FieldDecl(25, Top),
//                            TypeDecl(26, Bot, TypeProj(2, 13))
//                          ),
//                          TypedApp(27, 28) :- AndType(Top, TypeProj(31, 32))
//                        ) :- FunType(
//                          24,
//                          AndType(
//                            FieldDecl(25, Top),
//                            TypeDecl(26, Bot, TypeProj(2, 13))
//                          ),
//                          AndType(Top, TypeProj(31, 32))
//                        )
//                      ) :- FunType(
//                        24,
//                        AndType(
//                          FieldDecl(25, Top),
//                          TypeDecl(26, Bot, TypeProj(2, 13))
//                        ),
//                        AndType(Top, TypeProj(31, 32))
//                      )
//                    ) :- FunType(
//                      24,
//                      AndType(
//                        FieldDecl(25, Top),
//                        TypeDecl(26, Bot, TypeProj(2, 13))
//                      ),
//                      AndType(Top, TypeProj(31, 32))
//                    )
//                  ) :- FieldDecl(
//                    18,
//                    FunType(
//                      24,
//                      AndType(
//                        FieldDecl(25, Top),
//                        TypeDecl(26, Bot, TypeProj(2, 13))
//                      ),
//                      AndType(Top, TypeProj(31, 32))
//                    )
//                  )
//                ) :- AndType(
//                  TypeDecl(17, Bot, Bot),
//                  FieldDecl(
//                    18,
//                    FunType(
//                      24,
//                      AndType(
//                        FieldDecl(25, Top),
//                        TypeDecl(26, Bot, TypeProj(2, 13))
//                      ),
//                      AndType(Top, TypeProj(31, 32))
//                    )
//                  )
//                )
//              ) :- RecType(
//                16,
//                AndType(
//                  TypeDecl(17, Bot, Bot),
//                  FieldDecl(
//                    18,
//                    FunType(
//                      24,
//                      AndType(
//                        FieldDecl(25, Top),
//                        TypeDecl(26, Bot, TypeProj(2, 13))
//                      ),
//                      AndType(Top, TypeProj(31, 32))
//                    )
//                  )
//                )
//              )
//            ) :- FieldDecl(
//              15,
//              RecType(
//                16,
//                AndType(
//                  TypeDecl(17, Bot, Bot),
//                  FieldDecl(
//                    18,
//                    FunType(
//                      24,
//                      AndType(
//                        FieldDecl(25, Top),
//                        TypeDecl(26, Bot, TypeProj(2, 13))
//                      ),
//                      AndType(Top, TypeProj(31, 32))
//                    )
//                  )
//                )
//              )
//            )
//          ) :- AndType(
//            AndType(
//              TypeDecl(13, TypeProj(6, 7), TypeProj(6, 7)),
//              TypeDecl(14, TypeProj(2, 13), TypeProj(2, 13))
//            ),
//            FieldDecl(
//              15,
//              RecType(
//                16,
//                AndType(
//                  TypeDecl(17, Bot, Bot),
//                  FieldDecl(
//                    18,
//                    FunType(
//                      24,
//                      AndType(
//                        FieldDecl(25, Top),
//                        TypeDecl(26, Bot, TypeProj(2, 13))
//                      ),
//                      AndType(Top, TypeProj(31, 32))
//                    )
//                  )
//                )
//              )
//            )
//          )
//        ) :- AndType(
//          AndType(
//            FieldDecl(3, FunType(5, TypeProj(6, 7), FieldDecl(10, Bot))),
//            TypeDecl(
//              11,
//              RecType(12, TypeProj(6, 7)),
//              RecType(12, TypeProj(6, 7))
//            )
//          ),
//          AndType(
//            AndType(
//              TypeDecl(13, TypeProj(6, 7), TypeProj(6, 7)),
//              TypeDecl(14, TypeProj(2, 13), TypeProj(2, 13))
//            ),
//            FieldDecl(
//              15,
//              RecType(
//                16,
//                AndType(
//                  TypeDecl(17, Bot, Bot),
//                  FieldDecl(
//                    18,
//                    FunType(
//                      24,
//                      AndType(
//                        FieldDecl(25, Top),
//                        TypeDecl(26, Bot, TypeProj(2, 13))
//                      ),
//                      AndType(Top, TypeProj(31, 32))
//                    )
//                  )
//                )
//              )
//            )
//          )
//        )
//      ) :- RecType(
//        2,
//        AndType(
//          AndType(
//            FieldDecl(3, FunType(5, TypeProj(6, 7), FieldDecl(10, Bot))),
//            TypeDecl(
//              11,
//              RecType(12, TypeProj(6, 7)),
//              RecType(12, TypeProj(6, 7))
//            )
//          ),
//          AndType(
//            AndType(
//              TypeDecl(13, TypeProj(6, 7), TypeProj(6, 7)),
//              TypeDecl(14, TypeProj(2, 13), TypeProj(2, 13))
//            ),
//            FieldDecl(
//              15,
//              RecType(
//                16,
//                AndType(
//                  TypeDecl(17, Bot, Bot),
//                  FieldDecl(
//                    18,
//                    FunType(
//                      24,
//                      AndType(
//                        FieldDecl(25, Top),
//                        TypeDecl(26, Bot, TypeProj(2, 13))
//                      ),
//                      AndType(Top, TypeProj(31, 32))
//                    )
//                  )
//                )
//              )
//            )
//          )
//        )
//      )
//    ) :- FieldDecl(
//      1,
//      RecType(
//        2,
//        AndType(
//          AndType(
//            FieldDecl(3, FunType(5, TypeProj(6, 7), FieldDecl(10, Bot))),
//            TypeDecl(
//              11,
//              RecType(12, TypeProj(6, 7)),
//              RecType(12, TypeProj(6, 7))
//            )
//          ),
//          AndType(
//            AndType(
//              TypeDecl(13, TypeProj(6, 7), TypeProj(6, 7)),
//              TypeDecl(14, TypeProj(2, 13), TypeProj(2, 13))
//            ),
//            FieldDecl(
//              15,
//              RecType(
//                16,
//                AndType(
//                  TypeDecl(17, Bot, Bot),
//                  FieldDecl(
//                    18,
//                    FunType(
//                      24,
//                      AndType(
//                        FieldDecl(25, Top),
//                        TypeDecl(26, Bot, TypeProj(2, 13))
//                      ),
//                      AndType(Top, TypeProj(31, 32))
//                    )
//                  )
//                )
//              )
//            )
//          )
//        )
//      )
//    )
//  ) :- RecType(
//    0,
//    FieldDecl(
//      1,
//      RecType(
//        2,
//        AndType(
//          AndType(
//            FieldDecl(3, FunType(5, TypeProj(6, 7), FieldDecl(10, Bot))),
//            TypeDecl(
//              11,
//              RecType(12, TypeProj(6, 7)),
//              RecType(12, TypeProj(6, 7))
//            )
//          ),
//          AndType(
//            AndType(
//              TypeDecl(13, TypeProj(6, 7), TypeProj(6, 7)),
//              TypeDecl(14, TypeProj(2, 13), TypeProj(2, 13))
//            ),
//            FieldDecl(
//              15,
//              RecType(
//                16,
//                AndType(
//                  TypeDecl(17, Bot, Bot),
//                  FieldDecl(
//                    18,
//                    FunType(
//                      24,
//                      AndType(
//                        FieldDecl(25, Top),
//                        TypeDecl(26, Bot, TypeProj(2, 13))
//                      ),
//                      AndType(Top, TypeProj(31, 32))
//                    )
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

//InferenceProblem(
//  GlobalContext(
//    Map(
//      37 -> TypeDecl(38, Bot, TypeProj(7, 8)),
//      57 -> RecType(30, Bot),
//      9 -> TypeDecl(10, Bot, TypeProj(11, 12)),
//      7 -> TypeDecl(8, TypeProj(9, 10), Top),
//      11 -> TypeDecl(12, Top, Top),
//      23 -> Top,
//      58 -> TypeDecl(36, TypeProj(37, 38), TypeProj(7, 8))
//    ),
//    59
//  ),
//  Obj(
//    26,
//    AndType(
//      FieldDecl(27, RecType(30, Bot)),
//      FieldDecl(
//        31,
//        FunType(
//          41,
//          AndType(
//            RecType(42, RecType(43, Top)),
//            AndType(
//              FieldDecl(44, TypeDecl(45, Bot, Bot)),
//              AndType(
//                AndType(
//                  FieldDecl(46, RecType(47, Bot)),
//                  TypeDecl(48, Bot, TypeProj(37, 38))
//                ),
//                TypeDecl(
//                  49,
//                  AndType(Bot, RecType(50, TypeProj(9, 10))),
//                  Bot
//                )
//              )
//            )
//          ),
//          Top
//        )
//      )
//    ),
//    AndDef(
//      FieldDef(27, Var(57)),
//      FieldDef(
//        31,
//        Let(
//          32,
//          Var(58),
//          Fun(
//            41,
//            AndType(
//              RecType(42, RecType(43, Top)),
//              AndType(
//                FieldDecl(44, TypeDecl(45, Bot, Bot)),
//                AndType(
//                  AndType(
//                    FieldDecl(46, RecType(47, Bot)),
//                    TypeDecl(48, Bot, TypeProj(32, 36))
//                  ),
//                  TypeDecl(
//                    49,
//                    AndType(Bot, RecType(50, TypeProj(9, 10))),
//                    Bot
//                  )
//                )
//              )
//            ),
//            Var(23)
//          )
//        )
//      )
//    )
//  ),
//  Que,
//  TypedObj(
//    26,
//    AndType(
//      FieldDecl(27, RecType(30, Bot)),
//      FieldDecl(
//        31,
//        FunType(
//          41,
//          AndType(
//            RecType(42, RecType(43, Top)),
//            AndType(
//              FieldDecl(44, TypeDecl(45, Bot, Bot)),
//              AndType(
//                AndType(
//                  FieldDecl(46, RecType(47, Bot)),
//                  TypeDecl(48, Bot, TypeProj(37, 38))
//                ),
//                TypeDecl(
//                  49,
//                  AndType(Bot, RecType(50, TypeProj(9, 10))),
//                  Bot
//                )
//              )
//            )
//          ),
//          Top
//        )
//      )
//    ),
//    TypedAndDef(
//      TypedFieldDef(27, TypedVar(57) :- RecType(30, Bot)) :- FieldDecl(27, RecType(30, Bot)),
//      TypedFieldDef(
//        31,
//        TypedLet(
//          32,
//          TypedVar(58) :- TypeDecl(36, TypeProj(37, 38), TypeProj(7, 8)),
//          TypedFun(
//            41,
//            AndType(
//              RecType(42, RecType(43, Top)),
//              AndType(
//                FieldDecl(44, TypeDecl(45, Bot, Bot)),
//                AndType(
//                  AndType(
//                    FieldDecl(46, RecType(47, Bot)),
//                    TypeDecl(48, Bot, TypeProj(32, 36))
//                  ),
//                  TypeDecl(
//                    49,
//                    AndType(Bot, RecType(50, TypeProj(9, 10))),
//                    Bot
//                  )
//                )
//              )
//            ),
//            TypedVar(23) :- Top
//          ) :- FunType(
//            41,
//            AndType(
//              RecType(42, RecType(43, Top)),
//              AndType(
//                FieldDecl(44, TypeDecl(45, Bot, Bot)),
//                AndType(
//                  AndType(
//                    FieldDecl(46, RecType(47, Bot)),
//                    TypeDecl(48, Bot, TypeProj(32, 36)) // NOTE: tc will get rid of 32 right away using prototypes.
//                  ),
//                  TypeDecl(
//                    49,
//                    AndType(Bot, RecType(50, TypeProj(9, 10))),
//                    Bot
//                  )
//                )
//              )
//            ),
//            Top
//          )
//        ) :- FunType(
//          41,
//          AndType(
//            RecType(42, RecType(43, Top)),
//            AndType(
//              FieldDecl(44, TypeDecl(45, Bot, Bot)),
//              AndType(
//                AndType(
//                  FieldDecl(46, RecType(47, Bot)),
//                  TypeDecl(48, Bot, TypeProj(37, 38))
//                ),
//                TypeDecl(
//                  49,
//                  AndType(Bot, RecType(50, TypeProj(9, 10))),
//                  Bot
//                )
//              )
//            )
//          ),
//          Top
//        )
//      ) :- FieldDecl(
//        31,
//        FunType(
//          41,
//          AndType(
//            RecType(42, RecType(43, Top)),
//            AndType(
//              FieldDecl(44, TypeDecl(45, Bot, Bot)),
//              AndType(
//                AndType(
//                  FieldDecl(46, RecType(47, Bot)),
//                  TypeDecl(48, Bot, TypeProj(37, 38))
//                ),
//                TypeDecl(
//                  49,
//                  AndType(Bot, RecType(50, TypeProj(9, 10))),
//                  Bot
//                )
//              )
//            )
//          ),
//          Top
//        )
//      )
//    ) :- AndType(
//      FieldDecl(27, RecType(30, Bot)),
//      FieldDecl(
//        31,
//        FunType(
//          41,
//          AndType(
//            RecType(42, RecType(43, Top)),
//            AndType(
//              FieldDecl(44, TypeDecl(45, Bot, Bot)),
//              AndType(
//                AndType(
//                  FieldDecl(46, RecType(47, Bot)),
//                  TypeDecl(48, Bot, TypeProj(37, 38))
//                ),
//                TypeDecl(
//                  49,
//                  AndType(Bot, RecType(50, TypeProj(9, 10))),
//                  Bot
//                )
//              )
//            )
//          ),
//          Top
//        )
//      )
//    )
//  ) :- RecType(
//    26,
//    AndType(
//      FieldDecl(27, RecType(30, Bot)),
//      FieldDecl(
//        31,
//        FunType(
//          41,
//          AndType(
//            RecType(42, RecType(43, Top)),
//            AndType(
//              FieldDecl(44, TypeDecl(45, Bot, Bot)),
//              AndType(
//                AndType(
//                  FieldDecl(46, RecType(47, Bot)),
//                  TypeDecl(48, Bot, TypeProj(37, 38))
//                ),
//                TypeDecl(
//                  49,
//                  AndType(Bot, RecType(50, TypeProj(9, 10))),
//                  Bot
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


//InferenceProblem(
//  GlobalContext(
//    Map(
//      52 -> RecType(48, FunType(49, RecType(50, Top), Bot)),
//      9 -> TypeDecl(10, TypeProj(19, 20), TypeProj(11, 12)),
//      13 -> TypeDecl(14, AndType(AndType(Top, Top), Bot), Top),
//      34 -> TypeDecl(35, TypeProj(13, 14), TypeProj(13, 14)),
//      17 -> TypeDecl(18, AndType(Bot, Bot), Bot),
//      31 -> TypeDecl(32, FieldDecl(33, TypeProj(34, 35)), Top),
//      11 -> TypeDecl(12, TypeProj(15, 16), TypeProj(13, 14)),
//      51 -> RecType(
//        3,
//        AndType(
//          AndType(
//            TypeDecl(4, Top, Top),
//            TypeDecl(
//              5,
//              FunType(
//                6,
//                AndType(AndType(Top, Top), TypeDecl(7, Bot, Top)),
//                TypeProj(6, 7)
//              ),
//              FunType(
//                6,
//                AndType(AndType(Top, Top), TypeDecl(7, Bot, Top)),
//                TypeProj(6, 7)
//              )
//            )
//          ),
//          AndType(
//            TypeDecl(
//              8,
//              AndType(Top, TypeProj(9, 10)),
//              AndType(Top, TypeProj(9, 10))
//            ),
//            TypeDecl(21, Bot, Bot)
//          )
//        )
//      ),
//      19 -> TypeDecl(20, TypeProj(15, 16), TypeProj(15, 16)),
//      15 -> TypeDecl(16, AndType(Bot, Bot), TypeProj(17, 18))
//    ),
//    53
//  ),
//  Obj(
//    0,
//    FieldDecl(
//      1,
//      FunType(
//        22,
//        Top,
//        FunType(
//          23,
//          FunType(24, Bot, Bot),
//          FunType(
//            25,
//            AndType(
//              TypeDecl(
//                26,
//                AndType(
//                  AndType(
//                    RecType(27, TypeProj(15, 16)),
//                    RecType(
//                      28,
//                      TypeDecl(
//                        29,
//                        TypeProj(15, 16),
//                        TypeProj(15, 16)
//                      )
//                    )
//                  ),
//                  TypeProj(13, 14)
//                ),
//                RecType(27, TypeProj(15, 16))
//              ),
//              AndType(
//                TypeDecl(30, TypeProj(31, 32), Top),
//                TypeDecl(
//                  36,
//                  AndType(Bot, AndType(Top, TypeProj(9, 10))),
//                  TypeProj(34, 35)
//                )
//              )
//            ),
//            FunType(
//              37,
//              FunType(
//                38,
//                Top,
//                FunType(
//                  39,
//                  TypeDecl(40, Bot, Top),
//                  TypeDecl(44, Bot, Bot)
//                )
//              ),
//              RecType(48, FunType(49, RecType(50, Top), Bot))
//            )
//          )
//        )
//      )
//    ),
//    FieldDef(
//      1,
//      Let(
//        2,
//        Var(51),
//        Fun(
//          22,
//          Top,
//          Fun(
//            23,
//            FunType(24, Bot, Bot),
//            Fun(
//              25,
//              AndType(
//                TypeDecl(
//                  26,
//                  AndType(
//                    AndType(
//                      RecType(27, TypeProj(15, 16)),
//                      RecType(
//                        28,
//                        TypeDecl(
//                          29,
//                          TypeProj(15, 16),
//                          TypeProj(15, 16)
//                        )
//                      )
//                    ),
//                    TypeProj(13, 14)
//                  ),
//                  RecType(27, TypeProj(15, 16))
//                ),
//                AndType(
//                  TypeDecl(30, TypeProj(31, 32), Top),
//                  TypeDecl(
//                    36,
//                    AndType(Bot, TypeProj(2, 8)),
//                    TypeProj(34, 35)
//                  )
//                )
//              ),
//              Fun(
//                37,
//                FunType(
//                  38,
//                  Top,
//                  FunType(
//                    39,
//                    TypeDecl(
//                      40,
//                      Bot,
//                      RecType(
//                        41,
//                        TypeDecl(
//                          42,
//                          FunType(43, TypeProj(2, 21), Top),
//                          FunType(43, TypeProj(2, 21), Top)
//                        )
//                      )
//                    ),
//                    TypeDecl(44, Bot, Bot)
//                  )
//                ),
//                Var(52)
//              )
//            )
//          )
//        )
//      )
//    )
//  ),
//  Que,
//  TypedObj(
//    0,
//    FieldDecl(
//      1,
//      FunType(
//        22,
//        Top,
//        FunType(
//          23,
//          FunType(24, Bot, Bot),
//          FunType(
//            25,
//            AndType(
//              TypeDecl(
//                26,
//                AndType(
//                  AndType(
//                    RecType(27, TypeProj(15, 16)),
//                    RecType(
//                      28,
//                      TypeDecl(
//                        29,
//                        TypeProj(15, 16),
//                        TypeProj(15, 16)
//                      )
//                    )
//                  ),
//                  TypeProj(13, 14)
//                ),
//                RecType(27, TypeProj(15, 16))
//              ),
//              AndType(
//                TypeDecl(30, TypeProj(31, 32), Top),
//                TypeDecl(
//                  36,
//                  AndType(Bot, AndType(Top, TypeProj(9, 10))),
//                  TypeProj(34, 35)
//                )
//              )
//            ),
//            FunType(
//              37,
//              FunType(
//                38,
//                Top,
//                FunType(
//                  39,
//                  TypeDecl(40, Bot, Top),
//                  TypeDecl(44, Bot, Bot)
//                )
//              ),
//              RecType(48, FunType(49, RecType(50, Top), Bot))
//            )
//          )
//        )
//      )
//    ),
//    TypedFieldDef(
//      1,
//      TypedLet(
//        2,
//        TypedVar(51) :- RecType(
//          3,
//          AndType(
//            AndType(
//              TypeDecl(4, Top, Top),
//              TypeDecl(
//                5,
//                FunType(
//                  6,
//                  AndType(AndType(Top, Top), TypeDecl(7, Bot, Top)),
//                  TypeProj(6, 7)
//                ),
//                FunType(
//                  6,
//                  AndType(AndType(Top, Top), TypeDecl(7, Bot, Top)),
//                  TypeProj(6, 7)
//                )
//              )
//            ),
//            AndType(
//              TypeDecl(
//                8,
//                AndType(Top, TypeProj(9, 10)),
//                AndType(Top, TypeProj(9, 10))
//              ),
//              TypeDecl(21, Bot, Bot)
//            )
//          )
//        ),
//        TypedFun(
//          22,
//          Top,
//          TypedFun(
//            23,
//            FunType(24, Bot, Bot),
//            TypedFun(
//              25,
//              AndType(
//                TypeDecl(
//                  26,
//                  AndType(
//                    AndType(
//                      RecType(27, TypeProj(15, 16)),
//                      RecType(
//                        28,
//                        TypeDecl(
//                          29,
//                          TypeProj(15, 16),
//                          TypeProj(15, 16)
//                        )
//                      )
//                    ),
//                    TypeProj(13, 14)
//                  ),
//                  RecType(27, TypeProj(15, 16))
//                ),
//                AndType(
//                  TypeDecl(30, TypeProj(31, 32), Top),
//                  TypeDecl(
//                    36,
//                    AndType(Bot, TypeProj(2, 8)),
//                    TypeProj(34, 35)
//                  )
//                )
//              ),
//              TypedFun(
//                37,
//                FunType(
//                  38,
//                  Top,
//                  FunType(
//                    39,
//                    TypeDecl(
//                      40,
//                      Bot,
//                      RecType(
//                        41,
//                        TypeDecl(
//                          42,
//                          FunType(43, TypeProj(2, 21), Top),
//                          FunType(43, TypeProj(2, 21), Top)
//                        )
//                      )
//                    ),
//                    TypeDecl(44, Bot, Bot)
//                  )
//                ),
//                TypedVar(52) :- RecType(48, FunType(49, RecType(50, Top), Bot))
//              ) :- FunType(
//                37,
//                FunType(
//                  38,
//                  Top,
//                  FunType(
//                    39,
//                    TypeDecl(
//                      40,
//                      Bot,
//                      RecType(
//                        41,
//                        TypeDecl(
//                          42,
//                          FunType(43, TypeProj(2, 21), Top),
//                          FunType(43, TypeProj(2, 21), Top)
//                        )
//                      )
//                    ),
//                    TypeDecl(44, Bot, Bot)
//                  )
//                ),
//                RecType(48, FunType(49, RecType(50, Top), Bot))
//              )
//            ) :- FunType(
//              25,
//              AndType(
//                TypeDecl(
//                  26,
//                  AndType(
//                    AndType(
//                      RecType(27, TypeProj(15, 16)),
//                      RecType(
//                        28,
//                        TypeDecl(
//                          29,
//                          TypeProj(15, 16),
//                          TypeProj(15, 16)
//                        )
//                      )
//                    ),
//                    TypeProj(13, 14)
//                  ),
//                  RecType(27, TypeProj(15, 16))
//                ),
//                AndType(
//                  TypeDecl(30, TypeProj(31, 32), Top),
//                  TypeDecl(
//                    36,
//                    AndType(Bot, TypeProj(2, 8)),
//                    TypeProj(34, 35)
//                  )
//                )
//              ),
//              FunType(
//                37,
//                FunType(
//                  38,
//                  Top,
//                  FunType(
//                    39,
//                    TypeDecl(
//                      40,
//                      Bot,
//                      RecType(
//                        41,
//                        TypeDecl(
//                          42,
//                          FunType(43, TypeProj(2, 21), Top),
//                          FunType(43, TypeProj(2, 21), Top)
//                        )
//                      )
//                    ),
//                    TypeDecl(44, Bot, Bot)
//                  )
//                ),
//                RecType(48, FunType(49, RecType(50, Top), Bot))
//              )
//            )
//          ) :- FunType(
//            23,
//            FunType(24, Bot, Bot),
//            FunType(
//              25,
//              AndType(
//                TypeDecl(
//                  26,
//                  AndType(
//                    AndType(
//                      RecType(27, TypeProj(15, 16)),
//                      RecType(
//                        28,
//                        TypeDecl(
//                          29,
//                          TypeProj(15, 16),
//                          TypeProj(15, 16)
//                        )
//                      )
//                    ),
//                    TypeProj(13, 14)
//                  ),
//                  RecType(27, TypeProj(15, 16))
//                ),
//                AndType(
//                  TypeDecl(30, TypeProj(31, 32), Top),
//                  TypeDecl(
//                    36,
//                    AndType(Bot, TypeProj(2, 8)),
//                    TypeProj(34, 35)
//                  )
//                )
//              ),
//              FunType(
//                37,
//                FunType(
//                  38,
//                  Top,
//                  FunType(
//                    39,
//                    TypeDecl(
//                      40,
//                      Bot,
//                      RecType(
//                        41,
//                        TypeDecl(
//                          42,
//                          FunType(43, TypeProj(2, 21), Top),
//                          FunType(43, TypeProj(2, 21), Top)
//                        )
//                      )
//                    ),
//                    TypeDecl(44, Bot, Bot)
//                  )
//                ),
//                RecType(48, FunType(49, RecType(50, Top), Bot))
//              )
//            )
//          )
//        ) :- FunType(
//          22,
//          Top,
//          FunType(
//            23,
//            FunType(24, Bot, Bot),
//            FunType(
//              25,
//              AndType(
//                TypeDecl(
//                  26,
//                  AndType(
//                    AndType(
//                      RecType(27, TypeProj(15, 16)),
//                      RecType(
//                        28,
//                        TypeDecl(
//                          29,
//                          TypeProj(15, 16),
//                          TypeProj(15, 16)
//                        )
//                      )
//                    ),
//                    TypeProj(13, 14)
//                  ),
//                  RecType(27, TypeProj(15, 16))
//                ),
//                AndType(
//                  TypeDecl(30, TypeProj(31, 32), Top),
//                  TypeDecl(
//                    36,
//                    AndType(Bot, TypeProj(2, 8)),
//                    TypeProj(34, 35)
//                  )
//                )
//              ),
//              FunType(
//                37,
//                FunType(
//                  38,
//                  Top,
//                  FunType(
//                    39,
//                    TypeDecl(
//                      40,
//                      Bot,
//                      RecType(
//                        41,
//                        TypeDecl(
//                          42,
//                          FunType(43, TypeProj(2, 21), Top),
//                          FunType(43, TypeProj(2, 21), Top)
//                        )
//                      )
//                    ),
//                    TypeDecl(44, Bot, Bot)
//                  )
//                ),
//                RecType(48, FunType(49, RecType(50, Top), Bot))
//              )
//            )
//          )
//        )
//      ) :- FunType(
//        22,
//        Top,
//        FunType(
//          23,
//          FunType(24, Bot, Bot),
//          FunType(
//            25,
//            AndType(
//              TypeDecl(
//                26,
//                AndType(
//                  AndType(
//                    RecType(27, TypeProj(15, 16)),
//                    RecType(
//                      28,
//                      TypeDecl(
//                        29,
//                        TypeProj(15, 16),
//                        TypeProj(15, 16)
//                      )
//                    )
//                  ),
//                  TypeProj(13, 14)
//                ),
//                RecType(27, TypeProj(15, 16))
//              ),
//              AndType(
//                TypeDecl(30, TypeProj(31, 32), Top),
//                TypeDecl(
//                  36,
//                  AndType(Bot, AndType(Top, TypeProj(9, 10))),
//                  TypeProj(34, 35)
//                )
//              )
//            ),
//            FunType(
//              37,
//              FunType(
//                38,
//                Top,
//                FunType(
//                  39,
//                  TypeDecl(40, Bot, Top),
//                  TypeDecl(44, Bot, Bot)
//                )
//              ),
//              RecType(48, FunType(49, RecType(50, Top), Bot))
//            )
//          )
//        )
//      )
//    ) :- FieldDecl(
//      1,
//      FunType(
//        22,
//        Top,
//        FunType(
//          23,
//          FunType(24, Bot, Bot),
//          FunType(
//            25,
//            AndType(
//              TypeDecl(
//                26,
//                AndType(
//                  AndType(
//                    RecType(27, TypeProj(15, 16)),
//                    RecType(
//                      28,
//                      TypeDecl(
//                        29,
//                        TypeProj(15, 16),
//                        TypeProj(15, 16)
//                      )
//                    )
//                  ),
//                  TypeProj(13, 14)
//                ),
//                RecType(27, TypeProj(15, 16))
//              ),
//              AndType(
//                TypeDecl(30, TypeProj(31, 32), Top),
//                TypeDecl(
//                  36,
//                  AndType(Bot, AndType(Top, TypeProj(9, 10))),
//                  TypeProj(34, 35)
//                )
//              )
//            ),
//            FunType(
//              37,
//              FunType(
//                38,
//                Top,
//                FunType(
//                  39,
//                  TypeDecl(40, Bot, Top),
//                  TypeDecl(44, Bot, Bot)
//                )
//              ),
//              RecType(48, FunType(49, RecType(50, Top), Bot))
//            )
//          )
//        )
//      )
//    )
//  ) :- RecType(
//    0,
//    FieldDecl(
//      1,
//      FunType(
//        22,
//        Top,
//        FunType(
//          23,
//          FunType(24, Bot, Bot),
//          FunType(
//            25,
//            AndType(
//              TypeDecl(
//                26,
//                AndType(
//                  AndType(
//                    RecType(27, TypeProj(15, 16)),
//                    RecType(
//                      28,
//                      TypeDecl(
//                        29,
//                        TypeProj(15, 16),
//                        TypeProj(15, 16)
//                      )
//                    )
//                  ),
//                  TypeProj(13, 14)
//                ),
//                RecType(27, TypeProj(15, 16))
//              ),
//              AndType(
//                TypeDecl(30, TypeProj(31, 32), Top),
//                TypeDecl(
//                  36,
//                  AndType(Bot, AndType(Top, TypeProj(9, 10))),
//                  TypeProj(34, 35)
//                )
//              )
//            ),
//            FunType(
//              37,
//              FunType(
//                38,
//                Top,
//                FunType(
//                  39,
//                  TypeDecl(40, Bot, Top),
//                  TypeDecl(44, Bot, Bot)
//                )
//              ),
//              RecType(48, FunType(49, RecType(50, Top), Bot))
//            )
//          )
//        )
//      )
//    )
//  )
//)


//InferenceProblem(
//  GlobalContext(
//    Map(
//      88 -> FunType(
//        45,
//        Bot,
//        FunType(
//          46,
//          FieldDecl(47, TypeProj(16, 17)),
//          FunType(
//            48,
//            Top,
//            FunType(
//              49,
//              AndType(
//                TypeDecl(
//                  50,
//                  Bot,
//                  RecType(
//                    51,
//                    FunType(
//                      52,
//                      Top,
//                      AndType(
//                        Bot,
//                        FunType(53, TypeProj(54, 55), Bot)
//                      )
//                    )
//                  )
//                ),
//                TypeDecl(
//                  56,
//                  Bot,
//                  AndType(
//                    FieldDecl(
//                      57,
//                      RecType(
//                        58,
//                        RecType(
//                          59,
//                          AndType(
//                            FieldDecl(60, TypeProj(4, 5)),
//                            FieldDecl(61, Bot)
//                          )
//                        )
//                      )
//                    ),
//                    FieldDecl(62, TypeProj(4, 5))
//                  )
//                )
//              ),
//              RecType(
//                65,
//                AndType(
//                  TypeDecl(66, Top, Top),
//                  FieldDecl(
//                    67,
//                    RecType(
//                      68,
//                      FieldDecl(69, TypeDecl(19, Bot, Bot))
//                    )
//                  )
//                )
//              )
//            )
//          )
//        )
//      ),
//      10 -> TypeDecl(
//        11,
//        FieldDecl(
//          12,
//          AndType(RecType(13, TypeProj(4, 5)), TypeProj(4, 5))
//        ),
//        FieldDecl(12, RecType(13, TypeProj(4, 5)))
//      ),
//      22 -> FieldDecl(14, AndType(TypeProj(10, 11), FunType(15, Bot, Bot))),
//      54 -> TypeDecl(55, Bot, Top),
//      39 -> TypeDecl(40, Bot, Bot),
//      18 -> TypeDecl(19, Bot, Bot),
//      16 -> TypeDecl(17, TypeProj(18, 19), TypeProj(18, 19)),
//      31 -> TypeDecl(32, AndType(Bot, FieldDecl(33, Bot)), Bot),
//      87 -> RecType(
//        27,
//        TypeDecl(
//          28,
//          AndType(
//            FunType(29, Bot, TypeDecl(30, TypeProj(31, 32), Bot)),
//            Bot
//          ),
//          AndType(
//            FunType(29, Bot, TypeDecl(30, TypeProj(31, 32), Bot)),
//            Bot
//          )
//        )
//      ),
//      4 -> TypeDecl(5, Bot, Top)
//    ),
//    89
//  ),
//  Obj(
//    23,
//    FieldDecl(
//      24,
//      FunType(
//        34,
//        Bot,
//        FunType(
//          45,
//          Bot,
//          FunType(
//            46,
//            FieldDecl(47, TypeProj(16, 17)),
//            FunType(
//              48,
//              Top,
//              FunType(
//                49,
//                AndType(
//                  TypeDecl(
//                    50,
//                    Bot,
//                    RecType(
//                      51,
//                      FunType(
//                        52,
//                        Top,
//                        AndType(
//                          Bot,
//                          FunType(53, TypeProj(54, 55), Bot)
//                        )
//                      )
//                    )
//                  ),
//                  TypeDecl(
//                    56,
//                    Bot,
//                    AndType(
//                      FieldDecl(
//                        57,
//                        RecType(
//                          58,
//                          RecType(
//                            59,
//                            AndType(
//                              FieldDecl(60, TypeProj(4, 5)),
//                              FieldDecl(61, Bot)
//                            )
//                          )
//                        )
//                      ),
//                      FieldDecl(62, TypeProj(4, 5))
//                    )
//                  )
//                ),
//                RecType(
//                  65,
//                  AndType(
//                    TypeDecl(66, Top, Top),
//                    FieldDecl(
//                      67,
//                      RecType(
//                        68,
//                        FieldDecl(69, TypeDecl(19, Bot, Bot))
//                      )
//                    )
//                  )
//                )
//              )
//            )
//          )
//        )
//      )
//    ),
//    FieldDef(
//      24,
//      Let(
//        25,
//        Var(22),
//        Let(
//          26,
//          Var(87),
//          Fun(
//            34,
//            RecType(
//              35,
//              AndType(
//                AndType(
//                  TypeDecl(
//                    36,
//                    AndType(Bot, RecType(43, TypeProj(26, 28))),
//                    RecType(
//                      37,
//                      AndType(
//                        FieldDecl(38, TypeProj(39, 40)),
//                        FieldDecl(41, RecType(42, Bot))
//                      )
//                    )
//                  ),
//                  FieldDecl(44, Top)
//                ),
//                Top
//              )
//            ),
//            Var(88)
//          )
//        )
//      )
//    )
//  ),
//  Que,
//  TypedObj(
//    23,
//    FieldDecl(
//      24,
//      FunType(
//        34,
//        Bot,
//        FunType(
//          45,
//          Bot,
//          FunType(
//            46,
//            FieldDecl(47, TypeProj(16, 17)),
//            FunType(
//              48,
//              Top,
//              FunType(
//                49,
//                AndType(
//                  TypeDecl(
//                    50,
//                    Bot,
//                    RecType(
//                      51,
//                      FunType(
//                        52,
//                        Top,
//                        AndType(
//                          Bot,
//                          FunType(53, TypeProj(54, 55), Bot)
//                        )
//                      )
//                    )
//                  ),
//                  TypeDecl(
//                    56,
//                    Bot,
//                    AndType(
//                      FieldDecl(
//                        57,
//                        RecType(
//                          58,
//                          RecType(
//                            59,
//                            AndType(
//                              FieldDecl(60, TypeProj(4, 5)),
//                              FieldDecl(61, Bot)
//                            )
//                          )
//                        )
//                      ),
//                      FieldDecl(62, TypeProj(4, 5))
//                    )
//                  )
//                ),
//                RecType(
//                  65,
//                  AndType(
//                    TypeDecl(66, Top, Top),
//                    FieldDecl(
//                      67,
//                      RecType(
//                        68,
//                        FieldDecl(69, TypeDecl(19, Bot, Bot))
//                      )
//                    )
//                  )
//                )
//              )
//            )
//          )
//        )
//      )
//    ),
//    TypedFieldDef(
//      24,
//      TypedLet(
//        25,
//        TypedVar(22) :- FieldDecl(14, AndType(TypeProj(10, 11), FunType(15, Bot, Bot))),
//        TypedLet(
//          26,
//          TypedVar(87) :- RecType(
//            27,
//            TypeDecl(
//              28,
//              AndType(
//                FunType(
//                  29,
//                  Bot,
//                  TypeDecl(30, TypeProj(31, 32), Bot)
//                ),
//                Bot
//              ),
//              AndType(
//                FunType(
//                  29,
//                  Bot,
//                  TypeDecl(30, TypeProj(31, 32), Bot)
//                ),
//                Bot
//              )
//            )
//          ),
//          TypedFun(
//            34,
//            RecType(
//              35,
//              AndType(
//                AndType(
//                  TypeDecl(
//                    36,
//                    AndType(Bot, RecType(43, TypeProj(26, 28))),
//                    RecType(
//                      37,
//                      AndType(
//                        FieldDecl(38, TypeProj(39, 40)),
//                        FieldDecl(41, RecType(42, Bot))
//                      )
//                    )
//                  ),
//                  FieldDecl(44, Top)
//                ),
//                Top
//              )
//            ),
//            TypedVar(88) :- FunType(
//              45,
//              Bot,
//              FunType(
//                46,
//                FieldDecl(47, TypeProj(16, 17)),
//                FunType(
//                  48,
//                  Top,
//                  FunType(
//                    49,
//                    AndType(
//                      TypeDecl(
//                        50,
//                        Bot,
//                        RecType(
//                          51,
//                          FunType(
//                            52,
//                            Top,
//                            AndType(
//                              Bot,
//                              FunType(53, TypeProj(54, 55), Bot)
//                            )
//                          )
//                        )
//                      ),
//                      TypeDecl(
//                        56,
//                        Bot,
//                        AndType(
//                          FieldDecl(
//                            57,
//                            RecType(
//                              58,
//                              RecType(
//                                59,
//                                AndType(
//                                  FieldDecl(60, TypeProj(4, 5)),
//                                  FieldDecl(61, Bot)
//                                )
//                              )
//                            )
//                          ),
//                          FieldDecl(62, TypeProj(4, 5))
//                        )
//                      )
//                    ),
//                    RecType(
//                      65,
//                      AndType(
//                        TypeDecl(66, Top, Top),
//                        FieldDecl(
//                          67,
//                          RecType(
//                            68,
//                            FieldDecl(69, TypeDecl(19, Bot, Bot))
//                          )
//                        )
//                      )
//                    )
//                  )
//                )
//              )
//            )
//          ) :- FunType(
//            34,
//            Bot,
//            //RecType(
//            //  35,
//            //  AndType(
//            //    AndType(
//            //      TypeDecl(
//            //        36,
//            //        AndType(Bot, RecType(43, TypeProj(26, 28))), // TODO because of prototypes 26 should be eliminated early?
//            //        RecType(
//            //          37,
//            //          AndType(
//            //            FieldDecl(38, TypeProj(39, 40)),
//            //            FieldDecl(41, RecType(42, Bot))
//            //          )
//            //        )
//            //      ),
//            //      FieldDecl(44, Top)
//            //    ),
//            //    Top
//            //  )
//            //),
//            FunType(
//              45,
//              Bot,
//              FunType(
//                46,
//                FieldDecl(47, TypeProj(16, 17)),
//                FunType(
//                  48,
//                  Top,
//                  FunType(
//                    49,
//                    AndType(
//                      TypeDecl(
//                        50,
//                        Bot,
//                        RecType(
//                          51,
//                          FunType(
//                            52,
//                            Top,
//                            AndType(
//                              Bot,
//                              FunType(53, TypeProj(54, 55), Bot)
//                            )
//                          )
//                        )
//                      ),
//                      TypeDecl(
//                        56,
//                        Bot,
//                        AndType(
//                          FieldDecl(
//                            57,
//                            RecType(
//                              58,
//                              RecType(
//                                59,
//                                AndType(
//                                  FieldDecl(60, TypeProj(4, 5)),
//                                  FieldDecl(61, Bot)
//                                )
//                              )
//                            )
//                          ),
//                          FieldDecl(62, TypeProj(4, 5))
//                        )
//                      )
//                    ),
//                    RecType(
//                      65,
//                      AndType(
//                        TypeDecl(66, Top, Top),
//                        FieldDecl(
//                          67,
//                          RecType(
//                            68,
//                            FieldDecl(69, TypeDecl(19, Bot, Bot))
//                          )
//                        )
//                      )
//                    )
//                  )
//                )
//              )
//            )
//          )
//        ) :- FunType(
//          34,
//          Bot,
//          FunType(
//            45,
//            Bot,
//            FunType(
//              46,
//              FieldDecl(47, TypeProj(16, 17)),
//              FunType(
//                48,
//                Top,
//                FunType(
//                  49,
//                  AndType(
//                    TypeDecl(
//                      50,
//                      Bot,
//                      RecType(
//                        51,
//                        FunType(
//                          52,
//                          Top,
//                          AndType(
//                            Bot,
//                            FunType(53, TypeProj(54, 55), Bot)
//                          )
//                        )
//                      )
//                    ),
//                    TypeDecl(
//                      56,
//                      Bot,
//                      AndType(
//                        FieldDecl(
//                          57,
//                          RecType(
//                            58,
//                            RecType(
//                              59,
//                              AndType(
//                                FieldDecl(60, TypeProj(4, 5)),
//                                FieldDecl(61, Bot)
//                              )
//                            )
//                          )
//                        ),
//                        FieldDecl(62, TypeProj(4, 5))
//                      )
//                    )
//                  ),
//                  RecType(
//                    65,
//                    AndType(
//                      TypeDecl(66, Top, Top),
//                      FieldDecl(
//                        67,
//                        RecType(
//                          68,
//                          FieldDecl(69, TypeDecl(19, Bot, Bot))
//                        )
//                      )
//                    )
//                  )
//                )
//              )
//            )
//          )
//        )
//      ) :- FunType(
//        34,
//        Bot,
//        FunType(
//          45,
//          Bot,
//          FunType(
//            46,
//            FieldDecl(47, TypeProj(16, 17)),
//            FunType(
//              48,
//              Top,
//              FunType(
//                49,
//                AndType(
//                  TypeDecl(
//                    50,
//                    Bot,
//                    RecType(
//                      51,
//                      FunType(
//                        52,
//                        Top,
//                        AndType(
//                          Bot,
//                          FunType(53, TypeProj(54, 55), Bot)
//                        )
//                      )
//                    )
//                  ),
//                  TypeDecl(
//                    56,
//                    Bot,
//                    AndType(
//                      FieldDecl(
//                        57,
//                        RecType(
//                          58,
//                          RecType(
//                            59,
//                            AndType(
//                              FieldDecl(60, TypeProj(4, 5)),
//                              FieldDecl(61, Bot)
//                            )
//                          )
//                        )
//                      ),
//                      FieldDecl(62, TypeProj(4, 5))
//                    )
//                  )
//                ),
//                RecType(
//                  65,
//                  AndType(
//                    TypeDecl(66, Top, Top),
//                    FieldDecl(
//                      67,
//                      RecType(
//                        68,
//                        FieldDecl(69, TypeDecl(19, Bot, Bot))
//                      )
//                    )
//                  )
//                )
//              )
//            )
//          )
//        )
//      )
//    ) :- FieldDecl(
//      24,
//      FunType(
//        34,
//        Bot,
//        FunType(
//          45,
//          Bot,
//          FunType(
//            46,
//            FieldDecl(47, TypeProj(16, 17)),
//            FunType(
//              48,
//              Top,
//              FunType(
//                49,
//                AndType(
//                  TypeDecl(
//                    50,
//                    Bot,
//                    RecType(
//                      51,
//                      FunType(
//                        52,
//                        Top,
//                        AndType(
//                          Bot,
//                          FunType(53, TypeProj(54, 55), Bot)
//                        )
//                      )
//                    )
//                  ),
//                  TypeDecl(
//                    56,
//                    Bot,
//                    AndType(
//                      FieldDecl(
//                        57,
//                        RecType(
//                          58,
//                          RecType(
//                            59,
//                            AndType(
//                              FieldDecl(60, TypeProj(4, 5)),
//                              FieldDecl(61, Bot)
//                            )
//                          )
//                        )
//                      ),
//                      FieldDecl(62, TypeProj(4, 5))
//                    )
//                  )
//                ),
//                RecType(
//                  65,
//                  AndType(
//                    TypeDecl(66, Top, Top),
//                    FieldDecl(
//                      67,
//                      RecType(
//                        68,
//                        FieldDecl(69, TypeDecl(19, Bot, Bot))
//                      )
//                    )
//                  )
//                )
//              )
//            )
//          )
//        )
//      )
//    )
//  ) :- RecType(
//    23,
//    FieldDecl(
//      24,
//      FunType(
//        34,
//        Bot,
//        FunType(
//          45,
//          Bot,
//          FunType(
//            46,
//            FieldDecl(47, TypeProj(16, 17)),
//            FunType(
//              48,
//              Top,
//              FunType(
//                49,
//                AndType(
//                  TypeDecl(
//                    50,
//                    Bot,
//                    RecType(
//                      51,
//                      FunType(
//                        52,
//                        Top,
//                        AndType(
//                          Bot,
//                          FunType(53, TypeProj(54, 55), Bot)
//                        )
//                      )
//                    )
//                  ),
//                  TypeDecl(
//                    56,
//                    Bot,
//                    AndType(
//                      FieldDecl(
//                        57,
//                        RecType(
//                          58,
//                          RecType(
//                            59,
//                            AndType(
//                              FieldDecl(60, TypeProj(4, 5)),
//                              FieldDecl(61, Bot)
//                            )
//                          )
//                        )
//                      ),
//                      FieldDecl(62, TypeProj(4, 5))
//                    )
//                  )
//                ),
//                RecType(
//                  65,
//                  AndType(
//                    TypeDecl(66, Top, Top),
//                    FieldDecl(
//                      67,
//                      RecType(
//                        68,
//                        FieldDecl(69, TypeDecl(19, Bot, Bot))
//                      )
//                    )
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


//InferenceProblem(
//  GlobalContext(
//    Map(
//      40 -> TypeDecl(41, Bot, Top),
//      33 -> TypeDecl(
//        34,
//        Bot,
//        TypeDecl(
//          35,
//          Bot,
//          FunType(
//            36,
//            FunType(
//              37,
//              RecType(38, Bot),
//              AndType(
//                AndType(RecType(39, TypeProj(40, 41)), Top),
//                RecType(42, TypeProj(40, 41))
//              )
//            ),
//            Top
//          )
//        )
//      ),
//      67 -> RecType(61, FieldDecl(62, RecType(28, Bot)))
//    ),
//    68
//  ),
//  Obj(
//    43,
//    FieldDecl(44, Top),
//    FieldDef(
//      44,
//      Let(
//        45,
//        Var(40),
//        Obj(
//          46,
//          FieldDecl(
//            47,
//            FunType(
//              48,
//              AndType(
//                FieldDecl(
//                  49,
//                  TypeDecl(50, TypeProj(40, 41), TypeProj(40, 41))
//                ),
//                TypeDecl(
//                  51,
//                  AndType(AndType(Bot, FieldDecl(60, Bot)), Top),
//                  RecType(
//                    52,
//                    AndType(
//                      AndType(
//                        TypeDecl(53, Bot, Bot),
//                        AndType(
//                          FieldDecl(54, TypeProj(45, 41)),
//                          AndType(
//                            FieldDecl(
//                              55,
//                              TypeDecl(
//                                56,
//                                RecType(
//                                  57,
//                                  FieldDecl(58, TypeProj(33, 34))
//                                ),
//                                RecType(
//                                  57,
//                                  FieldDecl(58, TypeProj(33, 34))
//                                )
//                              )
//                            ),
//                            FieldDecl(59, TypeProj(33, 34))
//                          )
//                        )
//                      ),
//                      Bot
//                    )
//                  )
//                )
//              ),
//              RecType(61, FieldDecl(62, RecType(28, Bot)))
//            )
//          ),
//          FieldDef(
//            47,
//            Fun(
//              48,
//              AndType(
//                FieldDecl(
//                  49,
//                  TypeDecl(50, TypeProj(40, 41), TypeProj(40, 41))
//                ),
//                TypeDecl(
//                  51,
//                  AndType(AndType(Bot, FieldDecl(60, Bot)), Top),
//                  RecType(
//                    52,
//                    AndType(
//                      AndType(
//                        TypeDecl(53, Bot, Bot),
//                        AndType(
//                          FieldDecl(54, TypeProj(45, 41)),
//                          AndType(
//                            FieldDecl(
//                              55,
//                              TypeDecl(
//                                56,
//                                RecType(
//                                  57,
//                                  FieldDecl(58, TypeProj(33, 34))
//                                ),
//                                RecType(
//                                  57,
//                                  FieldDecl(58, TypeProj(33, 34))
//                                )
//                              )
//                            ),
//                            FieldDecl(59, TypeProj(33, 34))
//                          )
//                        )
//                      ),
//                      Bot
//                    )
//                  )
//                )
//              ),
//              Var(67)
//            )
//          )
//        )
//      )
//    )
//  ),
//  Que,
//  TypedObj(
//    43,
//    FieldDecl(44, Top),
//    TypedFieldDef(
//      44,
//      TypedLet(
//        45,
//        TypedVar(40) :- TypeDecl(41, Bot, Top),
//        TypedObj(
//          46,
//          FieldDecl(
//            47,
//            FunType(
//              48,
//              AndType(
//                FieldDecl(
//                  49,
//                  TypeDecl(50, TypeProj(40, 41), TypeProj(40, 41))
//                ),
//                TypeDecl(
//                  51,
//                  AndType(AndType(Bot, FieldDecl(60, Bot)), Top),
//                  RecType(
//                    52,
//                    AndType(
//                      AndType(
//                        TypeDecl(53, Bot, Bot),
//                        AndType(
//                          FieldDecl(54, TypeProj(45, 41)),
//                          AndType(
//                            FieldDecl(
//                              55,
//                              TypeDecl(
//                                56,
//                                RecType(
//                                  57,
//                                  FieldDecl(58, TypeProj(33, 34))
//                                ),
//                                RecType(
//                                  57,
//                                  FieldDecl(58, TypeProj(33, 34))
//                                )
//                              )
//                            ),
//                            FieldDecl(59, TypeProj(33, 34))
//                          )
//                        )
//                      ),
//                      Bot
//                    )
//                  )
//                )
//              ),
//              RecType(61, FieldDecl(62, RecType(28, Bot)))
//            )
//          ),
//          TypedFieldDef(
//            47,
//            TypedFun(
//              48,
//              AndType(
//                FieldDecl(
//                  49,
//                  TypeDecl(50, TypeProj(40, 41), TypeProj(40, 41))
//                ),
//                TypeDecl(
//                  51,
//                  AndType(AndType(Bot, FieldDecl(60, Bot)), Top),
//                  RecType(
//                    52,
//                    AndType(
//                      AndType(
//                        TypeDecl(53, Bot, Bot),
//                        AndType(
//                          FieldDecl(54, TypeProj(45, 41)),
//                          AndType(
//                            FieldDecl(
//                              55,
//                              TypeDecl(
//                                56,
//                                RecType(
//                                  57,
//                                  FieldDecl(58, TypeProj(33, 34))
//                                ),
//                                RecType(
//                                  57,
//                                  FieldDecl(58, TypeProj(33, 34))
//                                )
//                              )
//                            ),
//                            FieldDecl(59, TypeProj(33, 34))
//                          )
//                        )
//                      ),
//                      Bot
//                    )
//                  )
//                )
//              ),
//              TypedVar(67) :- RecType(61, FieldDecl(62, RecType(28, Bot)))
//            ) :- FunType(
//              48,
//              AndType(
//                FieldDecl(
//                  49,
//                  TypeDecl(50, TypeProj(40, 41), TypeProj(40, 41))
//                ),
//                TypeDecl(
//                  51,
//                  AndType(AndType(Bot, FieldDecl(60, Bot)), Top),
//                  RecType(
//                    52,
//                    AndType(
//                      AndType(
//                        TypeDecl(53, Bot, Bot),
//                        AndType(
//                          FieldDecl(54, TypeProj(45, 41)),
//                          AndType(
//                            FieldDecl(
//                              55,
//                              TypeDecl(
//                                56,
//                                RecType(
//                                  57,
//                                  FieldDecl(58, TypeProj(33, 34))
//                                ),
//                                RecType(
//                                  57,
//                                  FieldDecl(58, TypeProj(33, 34))
//                                )
//                              )
//                            ),
//                            FieldDecl(59, TypeProj(33, 34))
//                          )
//                        )
//                      ),
//                      Bot
//                    )
//                  )
//                )
//              ),
//              RecType(61, FieldDecl(62, RecType(28, Bot)))
//            )
//          ) :- FieldDecl(
//            47,
//            FunType(
//              48,
//              AndType(
//                FieldDecl(
//                  49,
//                  TypeDecl(50, TypeProj(40, 41), TypeProj(40, 41))
//                ),
//                TypeDecl(
//                  51,
//                  AndType(AndType(Bot, FieldDecl(60, Bot)), Top),
//                  RecType(
//                    52,
//                    AndType(
//                      AndType(
//                        TypeDecl(53, Bot, Bot),
//                        AndType(
//                          FieldDecl(54, TypeProj(45, 41)),
//                          AndType(
//                            FieldDecl(
//                              55,
//                              TypeDecl(
//                                56,
//                                RecType(
//                                  57,
//                                  FieldDecl(58, TypeProj(33, 34))
//                                ),
//                                RecType(
//                                  57,
//                                  FieldDecl(58, TypeProj(33, 34))
//                                )
//                              )
//                            ),
//                            FieldDecl(59, TypeProj(33, 34))
//                          )
//                        )
//                      ),
//                      Bot
//                    )
//                  )
//                )
//              ),
//              RecType(61, FieldDecl(62, RecType(28, Bot)))
//            )
//          )
//        ) :-
//        Top
//        //RecType(
//        //  46,
//        //  FieldDecl(
//        //    47,
//        //    FunType(
//        //      48,
//        //      AndType(
//        //        FieldDecl(
//        //          49,
//        //          TypeDecl(50, TypeProj(40, 41), TypeProj(40, 41))
//        //        ),
//        //        TypeDecl(
//        //          51,
//        //          AndType(AndType(Bot, FieldDecl(60, Bot)), Top),
//        //          RecType(
//        //            52,
//        //            AndType(
//        //              AndType(
//        //                TypeDecl(53, Bot, Bot),
//        //                AndType(
//        //                  FieldDecl(54, TypeProj(45, 41)),
//        //                  AndType(
//        //                    FieldDecl(
//        //                      55,
//        //                      TypeDecl(
//        //                        56,
//        //                        RecType(
//        //                          57,
//        //                          FieldDecl(58, TypeProj(33, 34))
//        //                        ),
//        //                        RecType(
//        //                          57,
//        //                          FieldDecl(58, TypeProj(33, 34))
//        //                        )
//        //                      )
//        //                    ),
//        //                    FieldDecl(59, TypeProj(33, 34))
//        //                  )
//        //                )
//        //              ),
//        //              Bot
//        //            )
//        //          )
//        //        )
//        //      ),
//        //      RecType(61, FieldDecl(62, RecType(28, Bot)))
//        //    )
//        //  )
//        //)
//      ) :- Top
//    ) :- FieldDecl(44, Top)
//  ) :- RecType(43, FieldDecl(44, Top))
//)

//InferenceProblem(
//  GlobalContext(
//    Map(
//      5 -> TypeDecl(6, Bot, TypeProj(7, 8)),
//      21 -> TypeDecl(22, Bot, Top),
//      9 -> TypeDecl(10, Bot, Top),
//      2 -> TypeDecl(
//        3,
//        FieldDecl(4, AndType(Bot, FunType(11, Bot, Top))),
//        FieldDecl(4, TypeProj(5, 6))
//      ),
//      7 -> TypeDecl(8, TypeProj(9, 10), TypeProj(9, 10)),
//      23 -> FunType(
//        0,
//        RecType(
//          0,
//          AndType(
//            TypeDecl(1, TypeProj(2, 3), Top),
//            FieldDecl(
//              12,
//              FunType(
//                13,
//                FieldDecl(14, FieldDecl(15, Top)),
//                FunType(
//                  16,
//                  FunType(
//                    17,
//                    AndType(TypeProj(2, 3), Top),
//                    RecType(18, TypeProj(7, 8))
//                  ),
//                  TypeProj(19, 20)
//                )
//              )
//            )
//          )
//        ),
//        Bot
//      ),
//      19 -> TypeDecl(20, TypeProj(21, 22), Top)
//    ),
//    24
//  ),
//  TApp(
//    Var(23),
//    AndDef(TypeDef(1, TypeProj(2, 3)), FieldDef(12, Sel(24, 25)))
//  ),
//  Que,
//  TypedTApp(
//    TypedVar(23) :- FunType(
//      0,
//      RecType(
//        0,
//        AndType(
//          TypeDecl(1, TypeProj(2, 3), Top),
//          FieldDecl(
//            12,
//            FunType(
//              13,
//              FieldDecl(14, FieldDecl(15, Top)),
//              FunType(
//                16,
//                FunType(
//                  17,
//                  AndType(TypeProj(2, 3), Top),
//                  RecType(18, TypeProj(7, 8))
//                ),
//                TypeProj(19, 20)
//              )
//            )
//          )
//        )
//      ),
//      Bot
//    ),
//    TypedAndDef(
//      TypedTypeDef(1, TypeProj(2, 3)) :- TypeDecl(1, TypeProj(2, 3), TypeProj(2, 3)),
//      TypedFieldDef(
//        12,
//        TypedSel(24, 25) :- FunType(
//          13,
//          FieldDecl(14, FieldDecl(15, Top)),
//          FunType(
//            16,
//            FunType(
//              17,
//              AndType(TypeProj(2, 3), Top),
//              RecType(18, TypeProj(7, 8))
//            ),
//            TypeProj(19, 20)
//          )
//        )
//      ) :- FieldDecl(
//        12,
//        FunType(
//          13,
//          FieldDecl(14, FieldDecl(15, Top)),
//          FunType(
//            16,
//            FunType(
//              17,
//              AndType(TypeProj(2, 3), Top),
//              RecType(18, TypeProj(7, 8))
//            ),
//            TypeProj(19, 20)
//          )
//        )
//      )
//    ) :- AndType(
//      TypeDecl(1, TypeProj(2, 3), TypeProj(2, 3)),
//      FieldDecl(
//        12,
//        FunType(
//          13,
//          FieldDecl(14, FieldDecl(15, Top)),
//          FunType(
//            16,
//            FunType(
//              17,
//              AndType(TypeProj(2, 3), Top),
//              RecType(18, TypeProj(7, 8))
//            ),
//            TypeProj(19, 20)
//          )
//        )
//      )
//    )
//  ) :- Bot
//)


//InferenceProblem(
//  GlobalContext(
//    Map(
//      23 -> FunType(
//        20,
//        TypeDecl(14, Top, Top),
//        FunType(21, TypeDecl(12, Top, Top), TypeProj(8, 9))
//      ),
//      8 -> TypeDecl(9, Bot, Bot)
//    ),
//    24
//  ),
//  TApp(Var(23), 13, TypeDef(14, Top)),
//  Que,
//  TypedTApp(
//    TypedVar(23) :- FunType(
//      20,
//      TypeDecl(14, Top, Top),
//      FunType(21, TypeDecl(12, Top, Top), TypeProj(8, 9))
//    ),
//    13,
//    TypedTypeDef(14, Top) :- TypeDecl(14, Top, Top)
//  ) :- FunType(21, TypeDecl(12, Top, Top), TypeProj(8, 9))
//)


//InferenceProblem(
//  GlobalContext(Map(10 -> Top), 51),
//  Obj(7, Top, FieldDef(9, Var(10))),
//  Que,
//  TypedObj(7, Top, TypedFieldDef(9, TypedVar(10) :- Top) :- Top) :- RecType(7, Top)
//)

//InferenceProblem(
//  GlobalContext(
//    Map(
//      14 -> FunType(
//        1,
//        TypeDecl(
//          2,
//          AndType(Bot, TypeProj(4, 5)),
//          RecType(3, TypeProj(4, 5))
//        ),
//        TypeDecl(8, Bot, Top)
//      ),
//      4 -> TypeDecl(5, Bot, Top)
//    ),
//    15
//  ),
//  TApp(Var(14), TypeDef(2, AndType(Bot, TypeProj(4, 5)))),
//  Que,
//  TypedTApp(
//    TypedVar(14) :- FunType(
//      1,
//      TypeDecl(
//        2,
//        AndType(Bot, TypeProj(4, 5)),
//        RecType(3, TypeProj(4, 5))
//      ),
//      TypeDecl(8, Bot, Top)
//    ),
//    TypedTypeDef(2, AndType(Bot, TypeProj(4, 5))) :- TypeDecl(
//      2,
//      AndType(Bot, TypeProj(4, 5)),
//      RecType(3, TypeProj(4, 5))
//      //AndType(Bot, TypeProj(4, 5))
//    )
//  ) :- TypeDecl(8, Bot, Top)
//)

//InferenceProblem(
//  GlobalContext(
//    Map(
//      16 -> AndType(Bot, AndType(TypeProj(10, 11), Top)),
//      10 -> TypeDecl(11, TypeProj(12, 13), Top),
//      17 -> FunType(0, RecType(1, TypeDecl(2, Top, Top)), Top),
//      12 -> TypeDecl(13, Bot, Bot)
//    ),
//    18
//  ),
//  TApp(Let(3, Var(16), Var(17)), TypeDef(2, Top)),
//  Que,
//  TypedTApp(
//    TypedLet(
//      3,
//      TypedVar(16) :- AndType(Bot, AndType(TypeProj(10, 11), Top)),
//      TypedVar(17) :- FunType(0, RecType(1, TypeDecl(2, Top, Top)), Top)
//    ) :- FunType(0, RecType(1, TypeDecl(2, Top, Top)), Top),
//    TypedTypeDef(2, Top) :- TypeDecl(2, Top, Top)
//  ) :- Top
//)

//InferenceProblem(
//  GlobalContext(
//    Map(
//      32 -> FunType(
//        20,
//        TypeDecl(
//          16,
//          Bot,
//          RecType(
//            17,
//            RecType(18, FunType(19, TypeProj(11, 12), Bot))
//          )
//        ),
//        FunType(
//          7,
//          RecType(
//            8,
//            AndType(
//              TypeDecl(9, Bot, Bot),
//              AndType(
//                FieldDecl(10, TypeProj(11, 12)),
//                FieldDecl(14, Top)
//              )
//            )
//          ),
//          Bot
//        )
//      ),
//      11 -> TypeDecl(12, RecType(13, Top), RecType(13, Top))
//    ),
//    33
//  ),
//  TApp(Var(32), TypeDef(16, Bot)),
//  Que,
//  TypedTApp(
//    TypedVar(32) :- FunType(
//      20,
//      TypeDecl(
//        16,
//        Bot,
//        RecType(17, RecType(18, FunType(19, TypeProj(11, 12), Bot)))
//      ),
//      FunType(
//        7,
//        RecType(
//          8,
//          AndType(
//            TypeDecl(9, Bot, Bot),
//            AndType(
//              FieldDecl(10, TypeProj(11, 12)),
//              FieldDecl(14, Top)
//            )
//          )
//        ),
//        Bot
//      )
//    ),
//    TypedTypeDef(16, Bot) :- TypeDecl(16,
//      Bot,
//      RecType(17, RecType(18, FunType(19, TypeProj(11, 12), Bot)))
//      //Bot
//      )
//  ) :- FunType(
//    7,
//    RecType(
//      8,
//      AndType(
//        TypeDecl(9, Bot, Bot),
//        AndType(FieldDecl(10, TypeProj(11, 12)), FieldDecl(14, Top))
//      )
//    ),
//    Bot
//  )
//)

//InferenceProblem(
//  GlobalContext(
//    Map(
//      20 -> TypeDecl(21, Top, Top),
//      13 -> TypeDecl(14, Bot, FieldDecl(15, TypeProj(16, 17))),
//      71 -> TypeProj(20, 21),
//      66 -> FunType(
//        61,
//        RecType(
//          62,
//          AndType(
//            TypeDecl(63, TypeProj(13, 14), TypeProj(13, 14)),
//            AndType(
//              FieldDecl(64, Top),
//              FieldDecl(65, TypeProj(20, 21))
//            )
//          )
//        ),
//        RecType(67, Bot)
//      ),
//      18 -> TypeDecl(19, TypeProj(20, 21), TypeProj(20, 21)),
//      16 -> TypeDecl(17, TypeProj(18, 19), TypeProj(18, 19)),
//      72 -> Top
//    ),
//    73
//  ),
//  TApp(
//    Var(66),
//    AndDef(
//      AndDef(FieldDef(64, Var(72)), FieldDef(65, Var(71))),
//      TypeDef(63, TypeProj(13, 14))
//    )
//  ),
//  Que,
//  TypedTApp(
//    TypedVar(66) :- FunType(
//      61,
//      RecType(
//        62,
//        AndType(
//          TypeDecl(63, TypeProj(13, 14), TypeProj(13, 14)),
//          AndType(
//            FieldDecl(64, Top),
//            FieldDecl(65, TypeProj(20, 21))
//          )
//        )
//      ),
//      RecType(67, Bot)
//    ),
//    TypedAndDef(
//      TypedAndDef(
//        TypedFieldDef(64, TypedVar(72) :- Top) :- FieldDecl(64, Top),
//        TypedFieldDef(65, TypedVar(71) :- TypeProj(20, 21)) :- FieldDecl(65, TypeProj(20, 21))
//      ) :- AndType(FieldDecl(64, Top), FieldDecl(65, TypeProj(20, 21))),
//      TypedTypeDef(63, TypeProj(13, 14)) :- TypeDecl(63, TypeProj(13, 14), TypeProj(13, 14))
//    ) :- AndType(
//      AndType(FieldDecl(64, Top), FieldDecl(65, TypeProj(20, 21))),
//      TypeDecl(63, TypeProj(13, 14), TypeProj(13, 14))
//    )
//  ) :- RecType(67, Bot)
//)

//InferenceProblem(
//  GlobalContext(Map(19 -> Top), 20),
//  Fun(5, Bot, Var(19)),
//  Que,
//  TypedFun(5, Bot, TypedVar(19) :- Top) :-
//    FunType(5, Bot, Top)
//    //Top
//)

//InferenceProblem(
//  GlobalContext(Map(), 8),
//  Obj(5, Top, TypeDef(6, FunType(7, Bot, Top))),
//  Que,
//  TypedObj(5, Top, TypedTypeDef(6, FunType(7, Bot, Top)) :- Top) :- Top
//)

//InferenceProblem.assemble(GlobalContext(Map(),8),:?(:%(IFun(4,FunType(1,RecType(2,FieldDecl(3,Bot)),Bot),:?(:%(IObj(5,Top,:?(:%(ITypeDef(6,FunType(7,Bot,Top)),Top),Top)),Top),Top)),Que),FunType(4,FunType(1,RecType(2,FieldDecl(3,Bot)),Bot),Top)))


//InferenceProblem.assemble(GlobalContext(Map(5 -> TypeDecl(6,Bot,TypeProj(7,8)), 29 -> FieldDecl(4,TypeProj(5,6)), 9 -> TypeDecl(10,Bot,Top), 13 -> FieldDecl(15,Bot), 27 -> FieldDecl(28,FunType(25,TypeDecl(26,AndType(TypeProj(7,8),TypeProj(7,8)),Top),Top)), 12 -> FunType(14,FieldDecl(15,Bot),FunType(0,RecType(1,AndType(FieldDecl(2,Top),AndType(FieldDecl(3,FieldDecl(4,TypeProj(5,6))),FieldDecl(11,Top)))),AndType(AndType(FieldDecl(16,Top),AndType(FieldDecl(17,Bot),TypeDecl(18,Bot,TypeProj(19,20)))),TypeDecl(21,RecType(22,Bot),RecType(22,Bot))))), 7 -> TypeDecl(8,TypeProj(9,10),TypeProj(9,10)), 30 -> Top, 19 -> TypeDecl(20,Bot,Bot)),31),:?(:%(ITApp(:?(:%(IApp(12,13),FunType(0,RecType(1,AndType(FieldDecl(2,Top),AndType(FieldDecl(3,FieldDecl(4,TypeProj(5,6))),FieldDecl(11,Top)))),Que)),FunType(0,RecType(1,AndType(FieldDecl(2,Top),AndType(FieldDecl(3,FieldDecl(4,TypeProj(5,6))),FieldDecl(11,Top)))),AndType(AndType(FieldDecl(16,Top),AndType(FieldDecl(17,Bot),TypeDecl(18,Bot,TypeProj(19,20)))),TypeDecl(21,RecType(22,Bot),RecType(22,Bot))))),:?(:%(IAndDef(:?(:%(IAndDef(:?(:%(IFieldDef(2,:?(:%(IFun(24,Bot,:?(:%(ITApp(:?(:%(ISel(27,28),FunType(25,TypeDecl(26,AndType(TypeProj(7,8),TypeProj(7,8)),Top),Top)),FunType(25,TypeDecl(26,AndType(TypeProj(7,8),TypeProj(7,8)),Top),Top)),:?(:%(ITypeDef(26,AndType(TypeProj(7,8),TypeProj(7,8))),TypeDecl(26,AndType(TypeProj(7,8),TypeProj(7,8)),Top)),TypeDecl(26,AndType(TypeProj(7,8),TypeProj(7,8)),Top))),Top),Top)),Top),Top)),FieldDecl(2,Top)),FieldDecl(2,Top)),:?(:%(IFieldDef(3,:?(:%(IVar(29),FieldDecl(4,TypeProj(5,6))),FieldDecl(4,TypeProj(5,6)))),FieldDecl(3,FieldDecl(4,TypeProj(5,6)))),FieldDecl(3,FieldDecl(4,TypeProj(5,6))))),AndType(FieldDecl(2,Top),FieldDecl(3,FieldDecl(4,TypeProj(5,6))))),AndType(FieldDecl(2,Top),FieldDecl(3,FieldDecl(4,TypeProj(5,6))))),:?(:%(IFieldDef(11,:?(:%(IVar(30),Top),Top)),FieldDecl(11,Top)),FieldDecl(11,Top))),AndType(AndType(FieldDecl(2,Top),FieldDecl(3,FieldDecl(4,TypeProj(5,6)))),FieldDecl(11,Top))),AndType(AndType(FieldDecl(2,Top),FieldDecl(3,FieldDecl(4,TypeProj(5,6)))),FieldDecl(11,Top)))),Que),AndType(AndType(FieldDecl(16,Top),AndType(FieldDecl(17,Bot),TypeDecl(18,Bot,TypeProj(19,20)))),TypeDecl(21,RecType(22,Bot),RecType(22,Bot)))))

//InferenceProblem.assemble(GlobalContext(Map(2 -> FunType(0,FieldDecl(1,Top),Bot), 5 -> FieldDecl(6,Top)),7),:?(:%(ITApp(:?(:%(IVar(2),FunType(0,FieldDecl(1,Top),Que)),FunType(0,FieldDecl(1,Top),Bot)),:?(:%(IFieldDef(1,:?(:%(IFun(4,Bot,:?(:%(ISel(5,6),Top),Top)),Top),Top)),FieldDecl(1,Top)),FieldDecl(1,Top))),Que),Bot))

//InferenceProblem(
//  GlobalContext(
//    Map(
//      59 -> FunType(
//        4,
//        RecType(
//          1,
//          AndType(
//            TypeDecl(2, Bot, Top),
//            FieldDecl(3, TypeProj(1, 2))
//          )
//        ),
//        FunType(
//          6,
//          Top,
//          FunType(
//            52,
//            FieldDecl(
//              53,
//              AndType(FieldDecl(54, Top), TypeDecl(55, Bot, Bot))
//            ),
//            TypeDecl(57, Bot, Bot)
//          )
//        )
//      ),
//      58 -> Bot
//    ),
//    60
//  ),
//  TApp(Var(59), AndDef(FieldDef(3, Var(58)), TypeDef(2, Bot))),
//  Que,
//  TypedTApp(
//    TypedVar(59) :- FunType(
//      4,
//      RecType(
//        1,
//        AndType(TypeDecl(2, Bot, Top), FieldDecl(3, TypeProj(1, 2)))
//      ),
//      FunType(
//        6,
//        Top,
//        FunType(
//          52,
//          FieldDecl(
//            53,
//            AndType(FieldDecl(54, Top), TypeDecl(55, Bot, Bot))
//          ),
//          TypeDecl(57, Bot, Bot)
//        )
//      )
//    ),
//    TypedAndDef(
//      TypedFieldDef(3, TypedVar(58) :- Bot) :- FieldDecl(3, Bot),
//      TypedTypeDef(2, Bot) :- TypeDecl(2, Bot, Top)
//    ) :- AndType(FieldDecl(3, Bot), TypeDecl(2, Bot, Top))
//  ) :- FunType(
//    6,
//    Top,
//    FunType(
//      52,
//      FieldDecl(
//        53,
//        AndType(FieldDecl(54, Top), TypeDecl(55, Bot, Bot))
//      ),
//      TypeDecl(57, Bot, Bot)
//    )
//  )
//)

//InferenceProblem(
//  GlobalContext(
//    Map(
//      10 -> TypeDecl(11, Top, Top),
//      116 -> FunType(
//        32,
//        RecType(
//          33,
//          AndType(
//            TypeDecl(
//              34,
//              RecType(35, RecType(36, Bot)),
//              RecType(35, RecType(36, Bot))
//            ),
//            AndType(
//              FieldDecl(
//                37,
//                FieldDecl(
//                  38,
//                  FieldDecl(39, RecType(40, TypeProj(33, 34)))
//                )
//              ),
//              FieldDecl(41, RecType(42, Bot))
//            )
//          )
//        ),
//        FunType(
//          24,
//          Top,
//          FunType(
//            15,
//            FieldDecl(16, Top),
//            FunType(
//              12,
//              FieldDecl(13, FunType(14, Bot, Top)),
//              FunType(
//                3,
//                TypeDecl(
//                  4,
//                  Bot,
//                  AndType(
//                    FunType(5, FunType(6, Bot, Top), Bot),
//                    FieldDecl(7, AndType(TypeProj(8, 9), Bot))
//                  )
//                ),
//                TypeProj(3, 4)
//              )
//            )
//          )
//        )
//      ),
//      117 -> FieldDecl(38, FieldDecl(39, Bot)),
//      118 -> RecType(42, Bot),
//      8 -> TypeDecl(9, Bot, TypeProj(10, 11))
//    ),
//    119
//  ),
//  TApp(
//    Var(116),
//    AndDef(
//      AndDef(FieldDef(37, Var(117)), FieldDef(41, Var(118))),
//      TypeDef(34, RecType(35, RecType(36, Bot)))
//    )
//  ),
//  Que,
//  TypedTApp(
//    TypedVar(116) :- FunType(
//      32,
//      RecType(
//        33,
//        AndType(
//          TypeDecl(
//            34,
//            RecType(35, RecType(36, Bot)),
//            RecType(35, RecType(36, Bot))
//          ),
//          AndType(
//            FieldDecl(
//              37,
//              FieldDecl(
//                38,
//                FieldDecl(39, RecType(40, TypeProj(33, 34)))
//              )
//            ),
//            FieldDecl(41, RecType(42, Bot))
//          )
//        )
//      ),
//      FunType(
//        24,
//        Top,
//        FunType(
//          15,
//          FieldDecl(16, Top),
//          FunType(
//            12,
//            FieldDecl(13, FunType(14, Bot, Top)),
//            FunType(
//              3,
//              TypeDecl(
//                4,
//                Bot,
//                AndType(
//                  FunType(5, FunType(6, Bot, Top), Bot),
//                  FieldDecl(7, AndType(TypeProj(8, 9), Bot))
//                )
//              ),
//              TypeProj(3, 4)
//            )
//          )
//        )
//      )
//    ),
//    TypedAndDef(
//      TypedAndDef(
//        TypedFieldDef(
//          37,
//          TypedVar(117) :- FieldDecl(38, FieldDecl(39, Bot))
//        ) :- FieldDecl(37, FieldDecl(38, FieldDecl(39, Bot))),
//        TypedFieldDef(41, TypedVar(118) :- RecType(42, Bot)) :- FieldDecl(41, RecType(42, Bot))
//      ) :- AndType(
//        FieldDecl(37, FieldDecl(38, FieldDecl(39, Bot))),
//        FieldDecl(41, RecType(42, Bot))
//      ),
//      TypedTypeDef(34, RecType(35, RecType(36, Bot))) :- TypeDecl(
//        34,
//        RecType(35, RecType(36, Bot)),
//        RecType(35, RecType(36, Bot))
//      )
//    ) :- AndType(
//      AndType(
//        FieldDecl(37, FieldDecl(38, FieldDecl(39, Bot))),
//        FieldDecl(41, RecType(42, Bot))
//      ),
//      TypeDecl(
//        34,
//        RecType(35, RecType(36, Bot)),
//        RecType(35, RecType(36, Bot))
//      )
//    )
//  ) :- FunType(
//    24,
//    Top,
//    FunType(
//      15,
//      FieldDecl(16, Top),
//      FunType(
//        12,
//        FieldDecl(13, FunType(14, Bot, Top)),
//        FunType(
//          3,
//          TypeDecl(
//            4,
//            Bot,
//            AndType(
//              FunType(5, FunType(6, Bot, Top), Bot),
//              FieldDecl(7, AndType(TypeProj(8, 9), Bot))
//            )
//          ),
//          TypeProj(3, 4)
//        )
//      )
//    )
//  )
//)

//InferenceProblem(
//  GlobalContext(
//    Map(
//      18 -> FunType(
//        5,
//        Top,
//        FunType(7, Bot, FunType(9, Top, TypeProj(11, 12)))
//      ),
//      11 -> TypeDecl(12, Bot, Bot),
//      13 -> Top
//    ),
//    19
//  ),
//  Let(3, Var(18), Var(13)),
//  Que,
//  TypedLet(
//    3,
//    TypedVar(18) :- FunType(
//      5,
//      Top,
//      FunType(7, Bot, FunType(9, Top, TypeProj(11, 12)))
//    ),
//    TypedVar(13) :- Top
//  ) :- Top
//)

//InferenceProblem(
//  GlobalContext(
//    Map(
//      0 -> FieldDecl(
//        1,
//        RecType(
//          2,
//          AndType(
//            TypeDecl(
//              3,
//              Bot,
//              RecType(
//                4,
//                TypeDecl(
//                  5,
//                  AndType(TypeProj(6, 7), Top),
//                  AndType(TypeProj(6, 7), Top)
//                )
//              )
//            ),
//            AndType(
//              AndType(
//                FieldDecl(
//                  19,
//                  TypeDecl(
//                    20,
//                    AndType(TypeProj(34, 35), Top),
//                    AndType(
//                      TypeDecl(
//                        21,
//                        AndType(Bot, TypeProj(10, 11)),
//                        TypeProj(8, 9)
//                      ),
//                      FunType(
//                        22,
//                        Top,
//                        TypeDecl(
//                          23,
//                          Bot,
//                          AndType(
//                            RecType(24, Bot),
//                            FunType(
//                              25,
//                              RecType(
//                                26,
//                                AndType(
//                                  FieldDecl(
//                                    27,
//                                    FunType(
//                                      28,
//                                      Top,
//                                      TypeDecl(29, Bot, TypeProj(10, 11))
//                                    )
//                                  ),
//                                  FieldDecl(30, TypeProj(6, 7))
//                                )
//                              ),
//                              RecType(31, RecType(32, RecType(33, Bot)))
//                            )
//                          )
//                        )
//                      )
//                    )
//                  )
//                ),
//                AndType(
//                  AndType(
//                    FieldDecl(45, RecType(46, Top)),
//                    AndType(
//                      FieldDecl(47, RecType(48, Top)),
//                      FieldDecl(
//                        49,
//                        FieldDecl(
//                          50,
//                          AndType(
//                            FunType(
//                              51,
//                              AndType(
//                                Top,
//                                AndType(
//                                  TypeProj(6, 7),
//                                  RecType(52, TypeProj(43, 44))
//                                )
//                              ),
//                              Bot
//                            ),
//                            TypeProj(36, 37)
//                          )
//                        )
//                      )
//                    )
//                  ),
//                  FieldDecl(53, TypeProj(10, 11))
//                )
//              ),
//              AndType(
//                AndType(
//                  FieldDecl(54, RecType(55, TypeProj(40, 41))),
//                  AndType(
//                    AndType(
//                      AndType(
//                        FieldDecl(
//                          56,
//                          RecType(
//                            57,
//                            FieldDecl(
//                              58,
//                              AndType(
//                                TypeDecl(
//                                  59,
//                                  Bot,
//                                  FunType(
//                                    60,
//                                    RecType(
//                                      61,
//                                      RecType(
//                                        62,
//                                        AndType(
//                                          FunType(63, Top, Bot),
//                                          AndType(
//                                            FunType(
//                                              64,
//                                              FieldDecl(
//                                                65,
//                                                AndType(
//                                                  FieldDecl(
//                                                    66,
//                                                    RecType(
//                                                      67,
//                                                      RecType(
//                                                        68,
//                                                        FieldDecl(
//                                                          69,
//                                                          AndType(
//                                                            TypeProj(40, 41),
//                                                            AndType(
//                                                              RecType(
//                                                                70,
//                                                                TypeDecl(
//                                                                  71,
//                                                                  Bot,
//                                                                  RecType(
//                                                                    72,
//                                                                    TypeProj(
//                                                                      2,
//                                                                      3
//                                                                    )
//                                                                  )
//                                                                )
//                                                              ),
//                                                              Top
//                                                            )
//                                                          )
//                                                        )
//                                                      )
//                                                    )
//                                                  ),
//                                                  FieldDecl(73, Bot)
//                                                )
//                                              ),
//                                              Bot
//                                            ),
//                                            FieldDecl(
//                                              74,
//                                              TypeDecl(
//                                                75,
//                                                AndType(
//                                                  TypeProj(76, 77),
//                                                  TypeProj(6, 7)
//                                                ),
//                                                Bot
//                                              )
//                                            )
//                                          )
//                                        )
//                                      )
//                                    ),
//                                    Bot
//                                  )
//                                ),
//                                FieldDecl(78, Bot)
//                              )
//                            )
//                          )
//                        ),
//                        FieldDecl(79, TypeProj(76, 77))
//                      ),
//                      AndType(FieldDecl(80, Bot), FieldDecl(81, Top))
//                    ),
//                    AndType(
//                      AndType(
//                        AndType(
//                          AndType(
//                            AndType(
//                              AndType(
//                                FieldDecl(82, Bot),
//                                FieldDecl(
//                                  83,
//                                  AndType(
//                                    RecType(84, TypeProj(17, 18)),
//                                    TypeProj(85, 86)
//                                  )
//                                )
//                              ),
//                              FieldDecl(87, RecType(88, FieldDecl(89, Bot)))
//                            ),
//                            FieldDecl(90, Top)
//                          ),
//                          FieldDecl(
//                            91,
//                            RecType(92, FieldDecl(93, TypeProj(17, 18)))
//                          )
//                        ),
//                        AndType(
//                          FieldDecl(94, FieldDecl(95, TypeProj(13, 14))),
//                          FieldDecl(96, FunType(97, Bot, TypeProj(40, 41)))
//                        )
//                      ),
//                      FieldDecl(98, Bot)
//                    )
//                  )
//                ),
//                AndType(
//                  FieldDecl(99, RecType(100, FunType(101, Bot, Bot))),
//                  AndType(
//                    AndType(
//                      AndType(
//                        AndType(FieldDecl(102, Top), FieldDecl(103, Bot)),
//                        FieldDecl(104, AndType(Bot, TypeProj(105, 106)))
//                      ),
//                      FieldDecl(
//                        108,
//                        AndType(
//                          FieldDecl(109, Top),
//                          TypeDecl(
//                            110,
//                            TypeProj(115, 116),
//                            FunType(
//                              111,
//                              FieldDecl(
//                                112,
//                                FunType(
//                                  113,
//                                  Top,
//                                  TypeDecl(
//                                    114,
//                                    TypeProj(105, 106),
//                                    TypeProj(105, 106)
//                                  )
//                                )
//                              ),
//                              TypeProj(40, 41)
//                            )
//                          )
//                        )
//                      )
//                    ),
//                    AndType(
//                      FieldDecl(
//                        119,
//                        FunType(
//                          120,
//                          AndType(
//                            AndType(
//                              AndType(
//                                TypeProj(115, 116),
//                                RecType(121, TypeProj(76, 77))
//                              ),
//                              Bot
//                            ),
//                            AndType(
//                              TypeDecl(122, Bot, Top),
//                              FieldDecl(123, TypeProj(13, 14))
//                            )
//                          ),
//                          RecType(
//                            124,
//                            RecType(
//                              125,
//                              FunType(
//                                126,
//                                TypeProj(10, 11),
//                                AndType(
//                                  AndType(TypeProj(43, 44), Bot),
//                                  RecType(127, TypeProj(76, 77))
//                                )
//                              )
//                            )
//                          )
//                        )
//                      ),
//                      FieldDecl(128, Top)
//                    )
//                  )
//                )
//              )
//            )
//          )
//        )
//      ),
//      115 -> TypeDecl(116, TypeProj(117, 118), Bot),
//      10 -> TypeDecl(
//        11,
//        Bot,
//        AndType(
//          TypeDecl(12, Bot, TypeProj(13, 14)),
//          FieldDecl(15, FunType(16, Top, TypeProj(17, 18)))
//        )
//      ),
//      6 -> TypeDecl(7, AndType(Bot, TypeProj(8, 9)), AndType(Bot, TypeProj(8, 9))),
//      117 -> TypeDecl(118, Bot, Bot),
//      85 -> TypeDecl(86, Top, Top),
//      38 -> TypeDecl(39, AndType(Bot, TypeProj(8, 9)), Bot),
//      13 -> TypeDecl(14, Bot, Top),
//      105 -> TypeDecl(106, AndType(Bot, RecType(107, Bot)), Bot),
//      34 -> TypeDecl(
//        35,
//        AndType(
//          AndType(
//            AndType(TypeProj(36, 37), TypeProj(17, 18)),
//            AndType(TypeProj(40, 41), FieldDecl(42, Top))
//          ),
//          TypeProj(43, 44)
//        ),
//        AndType(
//          AndType(
//            AndType(TypeProj(36, 37), TypeProj(17, 18)),
//            AndType(TypeProj(40, 41), FieldDecl(42, Top))
//          ),
//          TypeProj(43, 44)
//        )
//      ),
//      17 -> TypeDecl(18, Bot, Bot),
//      76 -> TypeDecl(77, Bot, Bot),
//      43 -> TypeDecl(44, Bot, Bot),
//      40 -> TypeDecl(41, Bot, Bot),
//      8 -> TypeDecl(9, AndType(Bot, TypeProj(10, 11)), Bot),
//      36 -> TypeDecl(37, TypeProj(38, 39), Bot)
//    ),
//    129
//  ),
//  Sel(0, 1),
//  Que,
//  TypedSel(0, 1) :- RecType(
//    2,
//    AndType(
//      TypeDecl(
//        3,
//        Bot,
//        RecType(
//          4,
//          TypeDecl(
//            5,
//            AndType(TypeProj(6, 7), Top),
//            AndType(TypeProj(6, 7), Top)
//          )
//        )
//      ),
//      AndType(
//        AndType(
//          FieldDecl(
//            19,
//            TypeDecl(
//              20,
//              AndType(TypeProj(34, 35), Top),
//              AndType(
//                TypeDecl(21, AndType(Bot, TypeProj(10, 11)), TypeProj(8, 9)),
//                FunType(
//                  22,
//                  Top,
//                  TypeDecl(
//                    23,
//                    Bot,
//                    AndType(
//                      RecType(24, Bot),
//                      FunType(
//                        25,
//                        RecType(
//                          26,
//                          AndType(
//                            FieldDecl(
//                              27,
//                              FunType(
//                                28,
//                                Top,
//                                TypeDecl(29, Bot, TypeProj(10, 11))
//                              )
//                            ),
//                            FieldDecl(30, TypeProj(6, 7))
//                          )
//                        ),
//                        RecType(31, RecType(32, RecType(33, Bot)))
//                      )
//                    )
//                  )
//                )
//              )
//            )
//          ),
//          AndType(
//            AndType(
//              FieldDecl(45, RecType(46, Top)),
//              AndType(
//                FieldDecl(47, RecType(48, Top)),
//                FieldDecl(
//                  49,
//                  FieldDecl(
//                    50,
//                    AndType(
//                      FunType(
//                        51,
//                        AndType(
//                          Top,
//                          AndType(
//                            TypeProj(6, 7),
//                            RecType(52, TypeProj(43, 44))
//                          )
//                        ),
//                        Bot
//                      ),
//                      TypeProj(36, 37)
//                    )
//                  )
//                )
//              )
//            ),
//            FieldDecl(53, TypeProj(10, 11))
//          )
//        ),
//        AndType(
//          AndType(
//            FieldDecl(54, RecType(55, TypeProj(40, 41))),
//            AndType(
//              AndType(
//                AndType(
//                  FieldDecl(
//                    56,
//                    RecType(
//                      57,
//                      FieldDecl(
//                        58,
//                        AndType(
//                          TypeDecl(
//                            59,
//                            Bot,
//                            FunType(
//                              60,
//                              RecType(
//                                61,
//                                RecType(
//                                  62,
//                                  AndType(
//                                    FunType(63, Top, Bot),
//                                    AndType(
//                                      FunType(
//                                        64,
//                                        FieldDecl(
//                                          65,
//                                          AndType(
//                                            FieldDecl(
//                                              66,
//                                              RecType(
//                                                67,
//                                                RecType(
//                                                  68,
//                                                  FieldDecl(
//                                                    69,
//                                                    AndType(
//                                                      TypeProj(40, 41),
//                                                      AndType(
//                                                        RecType(
//                                                          70,
//                                                          TypeDecl(
//                                                            71,
//                                                            Bot,
//                                                            RecType(
//                                                              72,
//                                                              TypeProj(2, 3)
//                                                            )
//                                                          )
//                                                        ),
//                                                        Top
//                                                      )
//                                                    )
//                                                  )
//                                                )
//                                              )
//                                            ),
//                                            FieldDecl(73, Bot)
//                                          )
//                                        ),
//                                        Bot
//                                      ),
//                                      FieldDecl(
//                                        74,
//                                        TypeDecl(
//                                          75,
//                                          AndType(
//                                            TypeProj(76, 77),
//                                            TypeProj(6, 7)
//                                          ),
//                                          Bot
//                                        )
//                                      )
//                                    )
//                                  )
//                                )
//                              ),
//                              Bot
//                            )
//                          ),
//                          FieldDecl(78, Bot)
//                        )
//                      )
//                    )
//                  ),
//                  FieldDecl(79, TypeProj(76, 77))
//                ),
//                AndType(FieldDecl(80, Bot), FieldDecl(81, Top))
//              ),
//              AndType(
//                AndType(
//                  AndType(
//                    AndType(
//                      AndType(
//                        AndType(
//                          FieldDecl(82, Bot),
//                          FieldDecl(
//                            83,
//                            AndType(
//                              RecType(84, TypeProj(17, 18)),
//                              TypeProj(85, 86)
//                            )
//                          )
//                        ),
//                        FieldDecl(87, RecType(88, FieldDecl(89, Bot)))
//                      ),
//                      FieldDecl(90, Top)
//                    ),
//                    FieldDecl(
//                      91,
//                      RecType(92, FieldDecl(93, TypeProj(17, 18)))
//                    )
//                  ),
//                  AndType(
//                    FieldDecl(94, FieldDecl(95, TypeProj(13, 14))),
//                    FieldDecl(96, FunType(97, Bot, TypeProj(40, 41)))
//                  )
//                ),
//                FieldDecl(98, Bot)
//              )
//            )
//          ),
//          AndType(
//            FieldDecl(99, RecType(100, FunType(101, Bot, Bot))),
//            AndType(
//              AndType(
//                AndType(
//                  AndType(FieldDecl(102, Top), FieldDecl(103, Bot)),
//                  FieldDecl(104, AndType(Bot, TypeProj(105, 106)))
//                ),
//                FieldDecl(
//                  108,
//                  AndType(
//                    FieldDecl(109, Top),
//                    TypeDecl(
//                      110,
//                      TypeProj(115, 116),
//                      FunType(
//                        111,
//                        FieldDecl(
//                          112,
//                          FunType(
//                            113,
//                            Top,
//                            TypeDecl(
//                              114,
//                              TypeProj(105, 106),
//                              TypeProj(105, 106)
//                            )
//                          )
//                        ),
//                        TypeProj(40, 41)
//                      )
//                    )
//                  )
//                )
//              ),
//              AndType(
//                FieldDecl(
//                  119,
//                  FunType(
//                    120,
//                    AndType(
//                      AndType(
//                        AndType(
//                          TypeProj(115, 116),
//                          RecType(121, TypeProj(76, 77))
//                        ),
//                        Bot
//                      ),
//                      AndType(
//                        TypeDecl(122, Bot, Top),
//                        FieldDecl(123, TypeProj(13, 14))
//                      )
//                    ),
//                    RecType(
//                      124,
//                      RecType(
//                        125,
//                        FunType(
//                          126,
//                          TypeProj(10, 11),
//                          AndType(
//                            AndType(TypeProj(43, 44), Bot),
//                            RecType(127, TypeProj(76, 77))
//                          )
//                        )
//                      )
//                    )
//                  )
//                ),
//                FieldDecl(128, Top)
//              )
//            )
//          )
//        )
//      )
//    )
//  )
//)

//InferenceProblem(
//  GlobalContext(
//    Map(
//      142 -> TypeDecl(143, TypeProj(119, 120), Top),
//      42 -> TypeDecl(43, Bot, TypeProj(11, 12)),
//      164 -> FieldDecl(165, Top),
//      1 -> FieldDecl(2, TypeProj(3, 4)),
//      117 -> TypeDecl(
//        118,
//        Bot,
//        AndType(TypeProj(119, 120), TypeDecl(121, Bot, Bot))
//      ),
//      137 -> TypeDecl(138, Bot, Top),
//      109 -> TypeDecl(110, Bot, Bot),
//      105 -> TypeDecl(106, TypeProj(107, 108), TypeProj(107, 108)),
//      166 -> FunType(
//        168,
//        RecType(169, Bot),
//        RecType(
//          170,
//          AndType(
//            TypeDecl(171, Bot, TypeProj(49, 50)),
//            AndType(
//              FieldDecl(
//                172,
//                TypeDecl(
//                  173,
//                  Bot,
//                  FunType(
//                    174,
//                    FunType(175, FieldDecl(176, Top), Bot),
//                    TypeProj(130, 131)
//                  )
//                )
//              ),
//              AndType(
//                FieldDecl(177, TypeDecl(178, Bot, TypeProj(42, 43))),
//                AndType(
//                  FieldDecl(179, TypeProj(94, 95)),
//                  FieldDecl(180, Bot)
//                )
//              )
//            )
//          )
//        )
//      ),
//      161 -> TypeDecl(
//        162,
//        AndType(Bot, Bot),
//        AndType(Bot, FieldDecl(163, Top))
//      ),
//      49 -> TypeDecl(
//        50,
//        AndType(
//          AndType(
//            AndType(Bot, Bot),
//            FunType(
//              51,
//              Top,
//              TypeDecl(
//                52,
//                FieldDecl(53, TypeProj(7, 8)),
//                FieldDecl(53, TypeProj(7, 8))
//              )
//            )
//          ),
//          TypeProj(11, 12)
//        ),
//        Bot
//      ),
//      7 -> TypeDecl(
//        8,
//        Bot,
//        AndType(
//          FieldDecl(
//            9,
//            TypeDecl(
//              10,
//              AndType(
//                TypeProj(11, 12),
//                RecType(
//                  28,
//                  AndType(
//                    AndType(
//                      TypeDecl(
//                        29,
//                        AndType(
//                          AndType(
//                            Bot,
//                            FunType(31, TypeProj(11, 12), Bot)
//                          ),
//                          TypeDecl(32, Bot, Top)
//                        ),
//                        RecType(30, TypeProj(11, 12))
//                      ),
//                      FieldDecl(33, Top)
//                    ),
//                    FieldDecl(
//                      34,
//                      FunType(
//                        35,
//                        FunType(
//                          36,
//                          Top,
//                          TypeDecl(
//                            37,
//                            AndType(
//                              AndType(
//                                Bot,
//                                FunType(31, TypeProj(11, 12), Bot)
//                              ),
//                              TypeDecl(32, Bot, Top)
//                            ),
//                            TypeProj(28, 29)
//                          )
//                        ),
//                        AndType(
//                          FieldDecl(
//                            38,
//                            FunType(
//                              39,
//                              Bot,
//                              FunType(
//                                40,
//                                Top,
//                                FunType(
//                                  41,
//                                  TypeProj(42, 43),
//                                  TypeProj(11, 12)
//                                )
//                              )
//                            )
//                          ),
//                          TypeDecl(
//                            44,
//                            FunType(45, TypeProj(16, 17), Bot),
//                            FunType(45, TypeProj(16, 17), Bot)
//                          )
//                        )
//                      )
//                    )
//                  )
//                )
//              ),
//              TypeProj(11, 12)
//            )
//          ),
//          RecType(46, TypeProj(42, 43))
//        )
//      ),
//      130 -> TypeDecl(131, Bot, TypeDecl(129, Bot, Bot)),
//      3 -> TypeDecl(
//        4,
//        AndType(
//          AndType(
//            AndType(
//              Bot,
//              FunType(
//                54,
//                RecType(
//                  55,
//                  AndType(
//                    RecType(56, FieldDecl(57, TypeProj(7, 8))),
//                    FunType(
//                      58,
//                      FieldDecl(59, Bot),
//                      TypeDecl(60, Bot, Bot)
//                    )
//                  )
//                ),
//                FunType(
//                  61,
//                  Bot,
//                  TypeDecl(
//                    62,
//                    AndType(
//                      AndType(
//                        Bot,
//                        AndType(
//                          Bot,
//                          RecType(
//                            77,
//                            AndType(
//                              AndType(
//                                FieldDecl(78, Top),
//                                FieldDecl(79, TypeProj(75, 76))
//                              ),
//                              FieldDecl(80, TypeProj(16, 17))
//                            )
//                          )
//                        )
//                      ),
//                      FieldDecl(
//                        81,
//                        FieldDecl(
//                          82,
//                          FieldDecl(
//                            83,
//                            RecType(
//                              84,
//                              RecType(
//                                85,
//                                RecType(86, TypeProj(75, 76))
//                              )
//                            )
//                          )
//                        )
//                      )
//                    ),
//                    FunType(
//                      63,
//                      RecType(
//                        64,
//                        AndType(
//                          TypeDecl(
//                            65,
//                            TypeProj(11, 12),
//                            TypeProj(11, 12)
//                          ),
//                          AndType(
//                            AndType(
//                              FieldDecl(66, Top),
//                              FieldDecl(67, Top)
//                            ),
//                            AndType(
//                              FieldDecl(
//                                68,
//                                RecType(69, FieldDecl(70, Bot))
//                              ),
//                              FieldDecl(71, TypeProj(64, 65))
//                            )
//                          )
//                        )
//                      ),
//                      RecType(
//                        72,
//                        TypeDecl(
//                          73,
//                          TypeProj(75, 76),
//                          FieldDecl(74, Bot)
//                        )
//                      )
//                    )
//                  )
//                )
//              )
//            ),
//            Top
//          ),
//          Top
//        ),
//        AndType(
//          RecType(
//            5,
//            RecType(6, AndType(TypeProj(7, 8), TypeProj(42, 43)))
//          ),
//          FunType(
//            47,
//            Bot,
//            TypeDecl(48, TypeProj(49, 50), TypeProj(11, 12))
//          )
//        )
//      ),
//      167 -> RecType(169, Bot),
//      16 -> TypeDecl(17, RecType(18, Bot), RecType(18, Bot)),
//      11 -> TypeDecl(
//        12,
//        AndType(
//          AndType(
//            Bot,
//            FunType(25, Bot, RecType(26, TypeDecl(27, Top, Top)))
//          ),
//          TypeDecl(
//            19,
//            FunType(
//              20,
//              FunType(21, TypeDecl(22, Bot, Bot), Bot),
//              RecType(23, FunType(24, TypeProj(16, 17), Top))
//            ),
//            Top
//          )
//        ),
//        AndType(
//          FunType(
//            13,
//            Top,
//            FunType(
//              14,
//              AndType(RecType(15, Bot), Top),
//              AndType(Bot, TypeProj(16, 17))
//            )
//          ),
//          TypeDecl(
//            19,
//            FunType(
//              20,
//              FunType(21, TypeDecl(22, Bot, Bot), Bot),
//              RecType(23, FunType(24, TypeProj(16, 17), Top))
//            ),
//            Top
//          )
//        )
//      ),
//      75 -> TypeDecl(76, FieldDecl(74, Bot), FieldDecl(74, Bot)),
//      119 -> TypeDecl(120, Bot, Bot),
//      107 -> TypeDecl(
//        108,
//        AndType(TypeProj(109, 110), FieldDecl(111, Bot)),
//        AndType(AndType(Bot, Bot), TypeProj(49, 50))
//      ),
//      94 -> TypeDecl(
//        95,
//        TypeProj(161, 162),
//        FunType(
//          96,
//          Bot,
//          RecType(
//            97,
//            AndType(
//              TypeDecl(
//                98,
//                TypeProj(117, 118),
//                RecType(
//                  99,
//                  AndType(
//                    TypeDecl(
//                      100,
//                      TypeProj(105, 106),
//                      TypeDecl(
//                        101,
//                        AndType(Bot, Bot),
//                        RecType(
//                          102,
//                          FieldDecl(
//                            103,
//                            AndType(
//                              AndType(Top, Top),
//                              FunType(104, Bot, Top)
//                            )
//                          )
//                        )
//                      )
//                    ),
//                    FieldDecl(
//                      112,
//                      AndType(
//                        TypeDecl(113, Bot, Bot),
//                        AndType(
//                          FieldDecl(114, Bot),
//                          AndType(
//                            FieldDecl(115, AndType(Top, Top)),
//                            FieldDecl(116, Top)
//                          )
//                        )
//                      )
//                    )
//                  )
//                )
//              ),
//              AndType(
//                FieldDecl(
//                  122,
//                  AndType(
//                    AndType(
//                      AndType(
//                        FieldDecl(123, Bot),
//                        TypeDecl(
//                          124,
//                          Bot,
//                          TypeDecl(
//                            125,
//                            RecType(18, Bot),
//                            TypeProj(16, 17)
//                          )
//                        )
//                      ),
//                      FunType(
//                        126,
//                        RecType(
//                          127,
//                          AndType(
//                            TypeDecl(
//                              128,
//                              TypeProj(130, 131),
//                              TypeDecl(129, Bot, Bot)
//                            ),
//                            FieldDecl(
//                              132,
//                              TypeDecl(133, TypeProj(107, 108), Top)
//                            )
//                          )
//                        ),
//                        FieldDecl(
//                          134,
//                          FunType(
//                            135,
//                            TypeProj(117, 118),
//                            RecType(136, TypeProj(137, 138))
//                          )
//                        )
//                      )
//                    ),
//                    Bot
//                  )
//                ),
//                FieldDecl(
//                  139,
//                  AndType(
//                    AndType(
//                      AndType(
//                        FieldDecl(140, Bot),
//                        TypeDecl(
//                          141,
//                          AndType(
//                            AndType(
//                              TypeProj(142, 143),
//                              AndType(
//                                FunType(
//                                  144,
//                                  TypeProj(7, 8),
//                                  AndType(
//                                    TypeProj(119, 120),
//                                    FieldDecl(
//                                      145,
//                                      FieldDecl(
//                                        146,
//                                        TypeDecl(147, Bot, Bot)
//                                      )
//                                    )
//                                  )
//                                ),
//                                RecType(148, Top)
//                              )
//                            ),
//                            Bot
//                          ),
//                          Top
//                        )
//                      ),
//                      FieldDecl(
//                        149,
//                        AndType(
//                          FieldDecl(
//                            150,
//                            AndType(
//                              AndType(
//                                TypeProj(7, 8),
//                                FieldDecl(151, Bot)
//                              ),
//                              TypeDecl(
//                                152,
//                                AndType(Bot, Bot),
//                                RecType(153, TypeProj(42, 43))
//                              )
//                            )
//                          ),
//                          TypeDecl(154, Bot, RecType(155, Bot))
//                        )
//                      )
//                    ),
//                    AndType(
//                      TypeDecl(
//                        156,
//                        Bot,
//                        AndType(TypeProj(137, 138), Bot)
//                      ),
//                      FieldDecl(
//                        157,
//                        FieldDecl(
//                          158,
//                          FieldDecl(
//                            159,
//                            FieldDecl(160, TypeProj(137, 138))
//                          )
//                        )
//                      )
//                    )
//                  )
//                )
//              )
//            )
//          )
//        )
//      )
//    ),
//    181
//  ),
//  Let(
//    0,
//    Sel(1, 2),
//    Fun(
//      88,
//      FieldDecl(89, Bot),
//      Fun(
//        91,
//        TypeProj(16, 17),
//        Let(92, Let(93, Var(94), Sel(164, 165)), App(166, 167))
//      )
//    )
//  ),
//  Que,
//  TypedLet(
//    0,
//    TypedSel(1, 2) :- TypeProj(3, 4),
//    TypedFun(
//      88,
//      FieldDecl(89, Bot),
//      TypedFun(
//        91,
//        TypeProj(16, 17),
//        TypedLet(
//          92,
//          TypedLet(
//            93,
//            TypedVar(94) :- TypeDecl(
//              95,
//              TypeProj(161, 162),
//              FunType(
//                96,
//                Bot,
//                RecType(
//                  97,
//                  AndType(
//                    TypeDecl(
//                      98,
//                      TypeProj(117, 118),
//                      RecType(
//                        99,
//                        AndType(
//                          TypeDecl(
//                            100,
//                            TypeProj(105, 106),
//                            TypeDecl(
//                              101,
//                              AndType(Bot, Bot),
//                              RecType(
//                                102,
//                                FieldDecl(
//                                  103,
//                                  AndType(
//                                    AndType(Top, Top),
//                                    FunType(104, Bot, Top)
//                                  )
//                                )
//                              )
//                            )
//                          ),
//                          FieldDecl(
//                            112,
//                            AndType(
//                              TypeDecl(113, Bot, Bot),
//                              AndType(
//                                FieldDecl(114, Bot),
//                                AndType(
//                                  FieldDecl(115, AndType(Top, Top)),
//                                  FieldDecl(116, Top)
//                                )
//                              )
//                            )
//                          )
//                        )
//                      )
//                    ),
//                    AndType(
//                      FieldDecl(
//                        122,
//                        AndType(
//                          AndType(
//                            AndType(
//                              FieldDecl(123, Bot),
//                              TypeDecl(
//                                124,
//                                Bot,
//                                TypeDecl(
//                                  125,
//                                  RecType(18, Bot),
//                                  TypeProj(16, 17)
//                                )
//                              )
//                            ),
//                            FunType(
//                              126,
//                              RecType(
//                                127,
//                                AndType(
//                                  TypeDecl(
//                                    128,
//                                    TypeProj(130, 131),
//                                    TypeDecl(129, Bot, Bot)
//                                  ),
//                                  FieldDecl(
//                                    132,
//                                    TypeDecl(
//                                      133,
//                                      TypeProj(107, 108),
//                                      Top
//                                    )
//                                  )
//                                )
//                              ),
//                              FieldDecl(
//                                134,
//                                FunType(
//                                  135,
//                                  TypeProj(117, 118),
//                                  RecType(136, TypeProj(137, 138))
//                                )
//                              )
//                            )
//                          ),
//                          Bot
//                        )
//                      ),
//                      FieldDecl(
//                        139,
//                        AndType(
//                          AndType(
//                            AndType(
//                              FieldDecl(140, Bot),
//                              TypeDecl(
//                                141,
//                                AndType(
//                                  AndType(
//                                    TypeProj(142, 143),
//                                    AndType(
//                                      FunType(
//                                        144,
//                                        TypeProj(7, 8),
//                                        AndType(
//                                          TypeProj(119, 120),
//                                          FieldDecl(
//                                            145,
//                                            FieldDecl(
//                                              146,
//                                              TypeDecl(
//                                                147,
//                                                Bot,
//                                                Bot
//                                              )
//                                            )
//                                          )
//                                        )
//                                      ),
//                                      RecType(148, Top)
//                                    )
//                                  ),
//                                  Bot
//                                ),
//                                Top
//                              )
//                            ),
//                            FieldDecl(
//                              149,
//                              AndType(
//                                FieldDecl(
//                                  150,
//                                  AndType(
//                                    AndType(
//                                      TypeProj(7, 8),
//                                      FieldDecl(151, Bot)
//                                    ),
//                                    TypeDecl(
//                                      152,
//                                      AndType(Bot, Bot),
//                                      RecType(153, TypeProj(42, 43))
//                                    )
//                                  )
//                                ),
//                                TypeDecl(
//                                  154,
//                                  Bot,
//                                  RecType(155, Bot)
//                                )
//                              )
//                            )
//                          ),
//                          AndType(
//                            TypeDecl(
//                              156,
//                              Bot,
//                              AndType(TypeProj(137, 138), Bot)
//                            ),
//                            FieldDecl(
//                              157,
//                              FieldDecl(
//                                158,
//                                FieldDecl(
//                                  159,
//                                  FieldDecl(160, TypeProj(137, 138))
//                                )
//                              )
//                            )
//                          )
//                        )
//                      )
//                    )
//                  )
//                )
//              )
//            ),
//            TypedSel(164, 165) :- Top
//          ) :- Top,
//          TypedApp(166, 167) :- RecType(
//            170,
//            AndType(
//              TypeDecl(171, Bot, TypeProj(49, 50)),
//              AndType(
//                FieldDecl(
//                  172,
//                  TypeDecl(
//                    173,
//                    Bot,
//                    FunType(
//                      174,
//                      FunType(175, FieldDecl(176, Top), Bot),
//                      TypeProj(130, 131)
//                    )
//                  )
//                ),
//                AndType(
//                  FieldDecl(
//                    177,
//                    TypeDecl(178, Bot, TypeProj(42, 43))
//                  ),
//                  AndType(
//                    FieldDecl(179, TypeProj(94, 95)),
//                    FieldDecl(180, Bot)
//                  )
//                )
//              )
//            )
//          )
//        ) :- RecType(
//          170,
//          AndType(
//            TypeDecl(171, Bot, TypeProj(49, 50)),
//            AndType(
//              FieldDecl(
//                172,
//                TypeDecl(
//                  173,
//                  Bot,
//                  FunType(
//                    174,
//                    FunType(175, FieldDecl(176, Top), Bot),
//                    TypeProj(130, 131)
//                  )
//                )
//              ),
//              AndType(
//                FieldDecl(177, TypeDecl(178, Bot, TypeProj(42, 43))),
//                AndType(
//                  FieldDecl(179, TypeProj(94, 95)),
//                  FieldDecl(180, Bot)
//                )
//              )
//            )
//          )
//        )
//      ) :- FunType(
//        91,
//        TypeProj(16, 17),
//        RecType(
//          170,
//          AndType(
//            TypeDecl(171, Bot, TypeProj(49, 50)),
//            AndType(
//              FieldDecl(
//                172,
//                TypeDecl(
//                  173,
//                  Bot,
//                  FunType(
//                    174,
//                    FunType(175, FieldDecl(176, Top), Bot),
//                    TypeProj(130, 131)
//                  )
//                )
//              ),
//              AndType(
//                FieldDecl(177, TypeDecl(178, Bot, TypeProj(42, 43))),
//                AndType(
//                  FieldDecl(179, TypeProj(94, 95)),
//                  FieldDecl(180, Bot)
//                )
//              )
//            )
//          )
//        )
//      )
//    ) :- FunType(
//      88,
//      FieldDecl(89, Bot),
//      FunType(
//        91,
//        TypeProj(16, 17),
//        RecType(
//          170,
//          AndType(
//            TypeDecl(171, Bot, TypeProj(49, 50)),
//            AndType(
//              FieldDecl(
//                172,
//                TypeDecl(
//                  173,
//                  Bot,
//                  FunType(
//                    174,
//                    FunType(175, FieldDecl(176, Top), Bot),
//                    TypeProj(130, 131)
//                  )
//                )
//              ),
//              AndType(
//                FieldDecl(177, TypeDecl(178, Bot, TypeProj(42, 43))),
//                AndType(
//                  FieldDecl(179, TypeProj(94, 95)),
//                  FieldDecl(180, Bot)
//                )
//              )
//            )
//          )
//        )
//      )
//    )
//  ) :- FunType(
//    88,
//    FieldDecl(89, Bot),
//    FunType(
//      91,
//      TypeProj(16, 17),
//      RecType(
//        170,
//        AndType(
//          TypeDecl(171, Bot, TypeProj(49, 50)),
//          AndType(
//            FieldDecl(
//              172,
//              TypeDecl(
//                173,
//                Bot,
//                FunType(
//                  174,
//                  FunType(175, FieldDecl(176, Top), Bot),
//                  TypeProj(130, 131)
//                )
//              )
//            ),
//            AndType(
//              FieldDecl(177, TypeDecl(178, Bot, TypeProj(42, 43))),
//              AndType(
//                FieldDecl(179, TypeProj(94, 95)),
//                FieldDecl(180, Bot)
//              )
//            )
//          )
//        )
//      )
//    )
//  )
//)

InferenceProblem.assemble(GlobalContext(Map(5 -> TypeDecl(6,TypeProj(7,8),Bot), 120 -> RecType(121,AndType(AndType(TypeDecl(122,Bot,Bot),TypeDecl(123,RecType(124,Top),RecType(124,Top))),FieldDecl(125,RecType(126,TypeDecl(127,TypeProj(137,138),AndType(TypeProj(107,108),TypeDecl(128,AndType(AndType(AndType(FieldDecl(129,FieldDecl(130,Top)),TypeDecl(131,Bot,Bot)),TypeProj(7,8)),RecType(132,AndType(FieldDecl(133,TypeProj(33,34)),FieldDecl(134,AndType(TypeDecl(135,Bot,Top),FieldDecl(136,TypeProj(7,8))))))),AndType(AndType(AndType(FieldDecl(129,FieldDecl(130,Top)),TypeDecl(131,Bot,Bot)),TypeProj(7,8)),RecType(132,AndType(FieldDecl(133,TypeProj(33,34)),FieldDecl(134,AndType(TypeDecl(135,Bot,Top),FieldDecl(136,TypeProj(7,8)))))))))))))), 247 -> Top, 174 -> FieldDecl(175,TypeProj(7,8)), 14 -> TypeDecl(15,Bot,FieldDecl(16,FunType(17,RecType(18,Bot),Top))), 184 -> TypeDecl(185,AndType(FieldDecl(186,TypeDecl(187,Bot,AndType(Bot,Bot))),RecType(188,Bot)),FieldDecl(186,TypeDecl(187,Bot,AndType(Bot,Bot)))), 20 -> TypeDecl(21,Bot,FieldDecl(22,Bot)), 84 -> TypeDecl(85,RecType(72,AndType(FieldDecl(73,RecType(74,Bot)),FieldDecl(75,RecType(76,AndType(FunType(77,RecType(78,Bot),RecType(79,Bot)),FieldDecl(80,FieldDecl(81,FunType(82,Top,RecType(83,Top))))))))),RecType(72,AndType(FieldDecl(73,RecType(74,Bot)),FieldDecl(75,RecType(76,AndType(FunType(77,RecType(78,Bot),RecType(79,Bot)),FieldDecl(80,FieldDecl(81,FunType(82,Top,RecType(83,Top)))))))))), 89 -> TypeDecl(90,Bot,Top), 243 -> Top, 60 -> FieldDecl(61,RecType(62,FieldDecl(63,RecType(64,AndType(TypeDecl(65,TypeProj(107,108),FunType(66,AndType(AndType(FieldDecl(67,Bot),RecType(68,FieldDecl(69,TypeProj(70,71)))),FieldDecl(86,FunType(87,TypeDecl(88,TypeProj(89,90),Top),FieldDecl(91,Top)))),AndType(TypeDecl(92,AndType(Bot,Bot),FunType(93,FunType(94,RecType(95,AndType(FieldDecl(96,Top),FieldDecl(97,TypeDecl(98,Bot,AndType(TypeProj(20,21),Top))))),Top),Bot)),TypeDecl(99,AndType(TypeProj(100,101),AndType(FieldDecl(104,TypeDecl(105,Bot,Bot)),FieldDecl(106,TypeProj(70,71)))),TypeProj(47,48))))),AndType(AndType(FieldDecl(109,Bot),FieldDecl(110,FunType(111,Bot,AndType(FieldDecl(112,Top),AndType(FieldDecl(113,Top),FieldDecl(114,Top)))))),FieldDecl(115,RecType(116,Top)))))))), 117 -> FieldDecl(118,Bot), 102 -> TypeDecl(103,Bot,TypeProj(47,48)), 70 -> TypeDecl(71,TypeProj(84,85),RecType(72,AndType(FieldDecl(73,RecType(74,Bot)),FieldDecl(75,RecType(76,AndType(FunType(77,RecType(78,Bot),RecType(79,Bot)),FieldDecl(80,FieldDecl(81,FunType(82,Top,RecType(83,Top)))))))))), 137 -> TypeDecl(138,AndType(Bot,AndType(Bot,FunType(154,TypeProj(100,101),AndType(AndType(FieldDecl(155,RecType(156,TypeProj(84,85))),AndType(FieldDecl(157,TypeProj(141,142)),AndType(FieldDecl(158,Top),TypeDecl(159,Bot,Bot)))),AndType(TypeDecl(160,Top,Top),AndType(FieldDecl(161,Bot),FieldDecl(162,RecType(163,TypeProj(5,6))))))))),AndType(TypeProj(139,140),TypeDecl(128,AndType(AndType(AndType(FieldDecl(129,FieldDecl(130,Top)),TypeDecl(131,Bot,Bot)),TypeProj(7,8)),RecType(132,AndType(FieldDecl(133,TypeProj(33,34)),FieldDecl(134,AndType(TypeDecl(135,Bot,Top),FieldDecl(136,TypeProj(7,8))))))),AndType(AndType(AndType(FieldDecl(129,FieldDecl(130,Top)),TypeDecl(131,Bot,Bot)),TypeProj(7,8)),RecType(132,AndType(FieldDecl(133,TypeProj(33,34)),FieldDecl(134,AndType(TypeDecl(135,Bot,Top),FieldDecl(136,TypeProj(7,8)))))))))), 33 -> TypeDecl(34,AndType(Bot,AndType(Top,TypeProj(20,21))),Bot), 141 -> TypeDecl(142,AndType(AndType(Bot,Bot),RecType(143,Bot)),TypeProj(107,108)), 225 -> TypeDecl(226,AndType(TypeProj(227,228),Top),TypeProj(227,228)), 193 -> TypeDecl(194,AndType(Bot,AndType(TypeProj(146,147),TypeProj(20,21))),AndType(Bot,Top)), 2 -> FunType(4,TypeProj(5,6),FunType(51,Top,FunType(52,FunType(53,Top,AndType(Top,FieldDecl(54,RecType(55,FieldDecl(56,Bot))))),FieldDecl(57,FunType(58,Top,TypeProj(7,8)))))), 148 -> TypeDecl(149,AndType(Bot,Top),AndType(Bot,Top)), 191 -> TypeDecl(192,AndType(TypeProj(193,194),TypeProj(195,196)),AndType(TypeProj(193,194),TypeProj(195,196))), 144 -> TypeDecl(145,AndType(AndType(TypeProj(146,147),Bot),FunType(150,Top,AndType(TypeProj(47,48),TypeDecl(151,Bot,AndType(FunType(152,Top,Top),RecType(153,TypeProj(20,21))))))),AndType(Bot,Bot)), 7 -> TypeDecl(8,AndType(Bot,AndType(FieldDecl(9,Bot),AndType(TypeDecl(10,AndType(Bot,Bot),TypeProj(11,12)),AndType(FieldDecl(35,FunType(36,TypeDecl(37,AndType(Top,RecType(38,RecType(39,AndType(FieldDecl(40,Bot),AndType(FieldDecl(41,TypeProj(20,21)),FieldDecl(42,TypeDecl(43,Bot,Top))))))),Top),Bot)),FieldDecl(44,RecType(45,AndType(TypeDecl(46,Bot,TypeProj(47,48)),FieldDecl(50,Bot)))))))),Bot), 240 -> TypeDecl(241,Bot,Bot), 3 -> TypeProj(5,6), 11 -> TypeDecl(12,TypeProj(33,34),TypeDecl(13,AndType(AndType(AndType(Bot,Bot),RecType(27,Top)),FieldDecl(28,RecType(29,FunType(30,TypeDecl(31,Top,Top),TypeDecl(32,Bot,Top))))),AndType(AndType(TypeProj(14,15),AndType(FunType(19,TypeProj(20,21),Top),AndType(RecType(23,RecType(24,FunType(25,Top,RecType(26,Top)))),Bot))),Top))), 139 -> TypeDecl(140,TypeProj(144,145),TypeProj(141,142)), 146 -> TypeDecl(147,TypeProj(148,149),Bot), 107 -> TypeDecl(108,Bot,Bot), 242 -> FunType(244,Top,FunType(245,TypeProj(20,21),RecType(246,Top))), 195 -> TypeDecl(196,AndType(Bot,RecType(205,AndType(TypeDecl(206,AndType(Bot,Top),Bot),FieldDecl(207,AndType(Bot,RecType(208,FieldDecl(209,TypeProj(137,138)))))))),FieldDecl(197,AndType(AndType(FieldDecl(198,Bot),TypeDecl(199,Bot,TypeDecl(200,Bot,Top))),AndType(FieldDecl(201,AndType(FieldDecl(202,Top),FieldDecl(203,Top))),TypeDecl(204,Top,Top))))), 47 -> TypeDecl(48,Bot,FieldDecl(49,Bot)), 254 -> AndType(AndType(Bot,Bot),TypeProj(102,103)), 227 -> TypeDecl(228,Bot,Bot), 100 -> TypeDecl(101,Bot,TypeProj(102,103))),255),:?(:%(ILet(0,:?(:%(ILet(1,:?(:%(IApp(2,3),Que),FunType(51,Top,FunType(52,FunType(53,Top,AndType(Top,FieldDecl(54,RecType(55,FieldDecl(56,Bot))))),FieldDecl(57,FunType(58,Top,TypeProj(7,8)))))),:?(:%(ILet(59,:?(:%(IVar(60),Que),FieldDecl(61,RecType(62,FieldDecl(63,RecType(64,AndType(TypeDecl(65,TypeProj(107,108),FunType(66,AndType(AndType(FieldDecl(67,Bot),RecType(68,FieldDecl(69,TypeProj(70,71)))),FieldDecl(86,FunType(87,TypeDecl(88,TypeProj(89,90),Top),FieldDecl(91,Top)))),AndType(TypeDecl(92,AndType(Bot,Bot),FunType(93,FunType(94,RecType(95,AndType(FieldDecl(96,Top),FieldDecl(97,TypeDecl(98,Bot,AndType(TypeProj(20,21),Top))))),Top),Bot)),TypeDecl(99,AndType(TypeProj(100,101),AndType(FieldDecl(104,TypeDecl(105,Bot,Bot)),FieldDecl(106,TypeProj(70,71)))),TypeProj(47,48))))),AndType(AndType(FieldDecl(109,Bot),FieldDecl(110,FunType(111,Bot,AndType(FieldDecl(112,Top),AndType(FieldDecl(113,Top),FieldDecl(114,Top)))))),FieldDecl(115,RecType(116,Top))))))))),:?(:%(ISel(117,118),Que),Bot)),Que),Bot)),Que),Bot),:?(:%(ILet(119,:?(:%(IVar(120),Que),RecType(121,AndType(AndType(TypeDecl(122,Bot,Bot),TypeDecl(123,RecType(124,Top),RecType(124,Top))),FieldDecl(125,RecType(126,TypeDecl(127,TypeProj(137,138),AndType(TypeProj(107,108),TypeDecl(128,AndType(AndType(AndType(FieldDecl(129,FieldDecl(130,Top)),TypeDecl(131,Bot,Bot)),TypeProj(7,8)),RecType(132,AndType(FieldDecl(133,TypeProj(33,34)),FieldDecl(134,AndType(TypeDecl(135,Bot,Top),FieldDecl(136,TypeProj(7,8))))))),AndType(AndType(AndType(FieldDecl(129,FieldDecl(130,Top)),TypeDecl(131,Bot,Bot)),TypeProj(7,8)),RecType(132,AndType(FieldDecl(133,TypeProj(33,34)),FieldDecl(134,AndType(TypeDecl(135,Bot,Top),FieldDecl(136,TypeProj(7,8))))))))))))))),:?(:%(ILet(164,:?(:%(IFun(166,AndType(TypeProj(146,147),FunType(167,FunType(168,FunType(169,FieldDecl(170,FunType(171,Top,Top)),Top),TypeProj(84,85)),RecType(172,AndType(AndType(FieldDecl(173,Top),Top),Top)))),:?(:%(ISel(174,175),Que),TypeProj(7,8))),Que),FunType(166,AndType(TypeProj(146,147),FunType(167,FunType(168,FunType(169,FieldDecl(170,FunType(171,Top,Top)),Top),TypeProj(84,85)),RecType(172,AndType(AndType(FieldDecl(173,Top),Top),Top)))),TypeProj(7,8))),:?(:%(IFun(177,AndType(Bot,Top),:?(:%(IFun(179,FunType(180,RecType(181,AndType(AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(TypeDecl(210,AndType(Bot,TypeDecl(216,Top,Top)),RecType(211,AndType(AndType(FieldDecl(212,Bot),FieldDecl(213,TypeProj(70,71))),FieldDecl(214,FieldDecl(215,Bot))))),TypeDecl(217,FieldDecl(218,Top),Top))),FieldDecl(219,Bot)))),FieldDecl(220,RecType(221,AndType(FieldDecl(222,Bot),FieldDecl(223,AndType(TypeDecl(224,TypeProj(225,226),Bot),FieldDecl(229,Bot)))))))),RecType(230,Top)),:?(:%(ILet(231,:?(:%(ILet(232,:?(:%(IFun(234,FunType(235,Top,TypeDecl(236,FunType(237,RecType(238,TypeProj(146,147)),TypeDecl(239,TypeProj(240,241),Bot)),FunType(237,RecType(238,TypeProj(146,147)),TypeDecl(239,TypeProj(240,241),Bot)))),:?(:%(IApp(242,243),Que),FunType(245,TypeProj(20,21),RecType(246,Top)))),Que),FunType(234,FunType(235,Top,TypeDecl(236,FunType(237,RecType(238,TypeProj(146,147)),TypeDecl(239,TypeProj(240,241),Bot)),FunType(237,RecType(238,TypeProj(146,147)),TypeDecl(239,TypeProj(240,241),Bot)))),FunType(245,TypeProj(20,21),RecType(246,Top)))),:?(:%(IVar(247),Que),Top)),Que),Top),:?(:%(IFun(249,FunType(250,AndType(Top,AndType(FunType(251,Top,TypeProj(33,34)),FunType(252,Top,FieldDecl(253,TypeProj(120,122))))),Bot),:?(:%(IVar(254),Que),AndType(AndType(Bot,Bot),TypeProj(102,103)))),Que),FunType(249,FunType(250,AndType(Top,AndType(FunType(251,Top,TypeProj(33,34)),FunType(252,Top,FieldDecl(253,TypeProj(120,122))))),Bot),AndType(AndType(Bot,Bot),TypeProj(102,103))))),Que),FunType(249,FunType(250,AndType(Top,AndType(FunType(251,Top,TypeProj(33,34)),FunType(252,Top,FieldDecl(253,TypeProj(120,122))))),Bot),AndType(AndType(Bot,Bot),TypeProj(102,103))))),Que),FunType(179,FunType(180,RecType(181,AndType(AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(TypeDecl(210,AndType(Bot,TypeDecl(216,Top,Top)),RecType(211,AndType(AndType(FieldDecl(212,Bot),FieldDecl(213,TypeProj(70,71))),FieldDecl(214,FieldDecl(215,Bot))))),TypeDecl(217,FieldDecl(218,Top),Top))),FieldDecl(219,Bot)))),FieldDecl(220,RecType(221,AndType(FieldDecl(222,Bot),FieldDecl(223,AndType(TypeDecl(224,TypeProj(225,226),Bot),FieldDecl(229,Bot)))))))),RecType(230,Top)),FunType(249,FunType(250,AndType(Top,AndType(FunType(251,Top,TypeProj(33,34)),FunType(252,Top,FieldDecl(253,TypeProj(120,122))))),Bot),AndType(AndType(Bot,Bot),TypeProj(102,103)))))),Que),FunType(177,AndType(Bot,Top),FunType(179,FunType(180,RecType(181,AndType(AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(TypeDecl(210,AndType(Bot,TypeDecl(216,Top,Top)),RecType(211,AndType(AndType(FieldDecl(212,Bot),FieldDecl(213,TypeProj(70,71))),FieldDecl(214,FieldDecl(215,Bot))))),TypeDecl(217,FieldDecl(218,Top),Top))),FieldDecl(219,Bot)))),FieldDecl(220,RecType(221,AndType(FieldDecl(222,Bot),FieldDecl(223,AndType(TypeDecl(224,TypeProj(225,226),Bot),FieldDecl(229,Bot)))))))),RecType(230,Top)),FunType(249,FunType(250,AndType(Top,AndType(FunType(251,Top,TypeProj(33,34)),FunType(252,Top,FieldDecl(253,TypeProj(120,122))))),Bot),AndType(AndType(Bot,Bot),TypeProj(102,103))))))),Que),FunType(177,AndType(Bot,Top),FunType(179,FunType(180,RecType(181,AndType(AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(TypeDecl(210,AndType(Bot,TypeDecl(216,Top,Top)),RecType(211,AndType(AndType(FieldDecl(212,Bot),FieldDecl(213,TypeProj(70,71))),FieldDecl(214,FieldDecl(215,Bot))))),TypeDecl(217,FieldDecl(218,Top),Top))),FieldDecl(219,Bot)))),FieldDecl(220,RecType(221,AndType(FieldDecl(222,Bot),FieldDecl(223,AndType(TypeDecl(224,TypeProj(225,226),Bot),FieldDecl(229,Bot)))))))),RecType(230,Top)),FunType(249,FunType(250,AndType(Top,AndType(FunType(251,Top,TypeProj(33,34)),FunType(252,Top,FieldDecl(253,TypeProj(120,122))))),Bot),AndType(AndType(Bot,Bot),TypeProj(102,103))))))),Que),FunType(177,AndType(Bot,Top),FunType(179,FunType(180,RecType(181,AndType(AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(TypeDecl(210,AndType(Bot,TypeDecl(216,Top,Top)),RecType(211,AndType(AndType(FieldDecl(212,Bot),FieldDecl(213,TypeProj(70,71))),FieldDecl(214,FieldDecl(215,Bot))))),TypeDecl(217,FieldDecl(218,Top),Top))),FieldDecl(219,Bot)))),FieldDecl(220,RecType(221,AndType(FieldDecl(222,Bot),FieldDecl(223,AndType(TypeDecl(224,TypeProj(225,226),Bot),FieldDecl(229,Bot)))))))),RecType(230,Top)),FunType(249,FunType(250,AndType(Top,AndType(FunType(251,Top,TypeProj(33,34)),FunType(252,Top,FieldDecl(253,TypeProj(120,122))))),Bot),AndType(AndType(Bot,Bot),TypeProj(102,103))))))),Que),FunType(177,AndType(Bot,Top),FunType(179,FunType(180,RecType(181,AndType(AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(TypeDecl(210,AndType(Bot,TypeDecl(216,Top,Top)),RecType(211,AndType(AndType(FieldDecl(212,Bot),FieldDecl(213,TypeProj(70,71))),FieldDecl(214,FieldDecl(215,Bot))))),TypeDecl(217,FieldDecl(218,Top),Top))),FieldDecl(219,Bot)))),FieldDecl(220,RecType(221,AndType(FieldDecl(222,Bot),FieldDecl(223,AndType(TypeDecl(224,TypeProj(225,226),Bot),FieldDecl(229,Bot)))))))),RecType(230,Top)),FunType(249,FunType(250,AndType(Top,AndType(FunType(251,Top,TypeProj(33,34)),FunType(252,Top,FieldDecl(253,TypeProj(120,122))))),Bot),AndType(AndType(Bot,Bot),TypeProj(102,103)))))))

//InferenceProblem.assemble(GlobalContext(Map(42 -> TypeDecl(43,Bot,Top), 125 -> TypeDecl(126,TypeProj(128,129),AndType(TypeProj(123,124),RecType(127,Top))), 196 -> TypeDecl(197,Bot,TypeProj(198,199)), 189 -> TypeDecl(190,Bot,Bot), 216 -> AndType(RecType(218,AndType(AndType(FieldDecl(219,TypeProj(119,120)),TypeDecl(220,Bot,TypeDecl(221,Bot,Bot))),AndType(FieldDecl(222,AndType(AndType(AndType(TypeDecl(223,TypeProj(224,225),Bot),FieldDecl(228,TypeProj(189,190))),TypeDecl(229,TypeProj(238,239),RecType(230,AndType(TypeDecl(231,Bot,RecType(232,AndType(TypeProj(125,126),Top))),FieldDecl(233,TypeDecl(234,TypeProj(235,236),Top)))))),FieldDecl(240,AndType(FunType(241,AndType(AndType(RecType(242,TypeDecl(243,TypeProj(98,99),TypeProj(98,99))),FunType(244,TypeProj(114,115),RecType(245,RecType(246,AndType(Bot,AndType(FieldDecl(247,Top),Top)))))),RecType(248,AndType(Top,Bot))),TypeProj(238,239)),Bot)))),AndType(AndType(TypeDecl(249,Bot,TypeDecl(250,RecType(251,TypeProj(40,41)),RecType(251,TypeProj(40,41)))),AndType(TypeDecl(252,AndType(TypeProj(253,254),AndType(AndType(TypeDecl(257,AndType(Bot,Bot),RecType(258,FunType(259,Top,FieldDecl(260,Top)))),AndType(AndType(TypeDecl(261,TypeProj(264,265),FunType(262,Top,RecType(263,TypeProj(235,236)))),FieldDecl(268,Top)),AndType(FieldDecl(269,TypeDecl(270,AndType(Bot,Bot),TypeProj(255,256))),AndType(FieldDecl(271,AndType(RecType(272,FieldDecl(273,Bot)),Bot)),FieldDecl(274,TypeProj(238,239)))))),FieldDecl(275,FieldDecl(276,RecType(277,FieldDecl(278,RecType(279,AndType(FieldDecl(280,Top),AndType(FieldDecl(281,Top),FieldDecl(282,FieldDecl(283,Bot))))))))))),Top),FieldDecl(284,Top))),FieldDecl(285,RecType(286,Top)))))),TypeProj(128,129)), 253 -> TypeDecl(254,TypeProj(255,256),TypeProj(255,256)), 238 -> TypeDecl(239,RecType(230,AndType(TypeDecl(231,Bot,RecType(232,AndType(TypeProj(125,126),Top))),FieldDecl(233,TypeDecl(234,TypeProj(235,236),Top)))),RecType(230,AndType(TypeDecl(231,Bot,RecType(232,AndType(TypeProj(125,126),Top))),FieldDecl(233,TypeDecl(234,TypeProj(235,236),Top))))), 121 -> TypeDecl(122,TypeProj(123,124),Bot), 293 -> FieldDecl(294,Bot), 116 -> TypeDecl(117,AndType(TypeProj(119,120),Bot),TypeDecl(118,Bot,TypeProj(75,76))), 1 -> Bot, 312 -> TypeDecl(313,TypeProj(314,315),Top), 381 -> TypeProj(238,239), 224 -> TypeDecl(225,AndType(Bot,RecType(226,RecType(227,TypeProj(116,117)))),Bot), 96 -> TypeDecl(97,TypeProj(98,99),Bot), 73 -> TypeDecl(74,AndType(Bot,Top),TypeProj(75,76)), 128 -> TypeDecl(129,Bot,Bot), 266 -> TypeDecl(267,Bot,Bot), 166 -> TypeDecl(167,AndType(TypeProj(168,169),FunType(170,RecType(171,FieldDecl(172,FieldDecl(173,RecType(174,Top)))),TypeProj(73,74))),TypeProj(168,169)), 264 -> TypeDecl(265,TypeProj(266,267),Bot), 296 -> FieldDecl(297,Top), 71 -> TypeDecl(72,TypeProj(73,74),Top), 382 -> FunType(384,Top,Bot), 98 -> TypeDecl(99,Bot,Bot), 198 -> TypeDecl(199,Bot,Bot), 306 -> TypeDecl(307,Bot,RecType(305,Bot)), 299 -> FieldDecl(300,FunType(301,TypeProj(123,124),RecType(302,AndType(AndType(AndType(AndType(FieldDecl(303,AndType(TypeDecl(304,TypeProj(306,307),RecType(305,Bot)),FieldDecl(308,FunType(309,FunType(310,AndType(Bot,Bot),Top),AndType(TypeDecl(311,AndType(TypeProj(312,313),RecType(316,TypeProj(224,225))),Top),Top))))),AndType(FieldDecl(303,AndType(TypeDecl(304,TypeProj(306,307),RecType(305,Bot)),FieldDecl(308,FunType(309,FunType(310,AndType(Bot,Bot),Top),AndType(TypeDecl(311,AndType(TypeProj(312,313),RecType(316,TypeProj(224,225))),Top),Top))))),AndType(AndType(FieldDecl(303,AndType(TypeDecl(304,TypeProj(306,307),RecType(305,Bot)),FieldDecl(308,FunType(309,FunType(310,AndType(Bot,Bot),Top),AndType(TypeDecl(311,AndType(TypeProj(312,313),RecType(316,TypeProj(224,225))),Top),Top))))),AndType(TypeDecl(317,Bot,Bot),TypeDecl(318,FieldDecl(319,Bot),FieldDecl(319,RecType(320,FunType(321,RecType(322,Bot),TypeProj(116,117))))))),FieldDecl(323,TypeDecl(324,AndType(Bot,TypeDecl(339,AndType(Bot,RecType(340,Bot)),Bot)),FunType(325,FunType(326,TypeProj(73,74),FieldDecl(327,RecType(328,RecType(329,RecType(330,AndType(TypeDecl(331,AndType(Bot,RecType(332,Top)),TypeProj(168,169)),AndType(AndType(FieldDecl(333,FieldDecl(334,TypeDecl(335,Bot,Bot))),FieldDecl(336,Top)),AndType(FieldDecl(337,Bot),FieldDecl(338,Top))))))))),Top)))))),FieldDecl(341,FunType(342,Top,TypeProj(216,252)))),FieldDecl(343,RecType(344,AndType(TypeDecl(345,RecType(346,Top),RecType(346,Top)),AndType(FieldDecl(347,Top),AndType(FieldDecl(348,TypeProj(224,225)),FieldDecl(349,Top))))))),FieldDecl(350,FunType(351,RecType(352,TypeDecl(353,Bot,FunType(354,Bot,AndType(TypeDecl(355,AndType(Bot,Top),RecType(356,AndType(AndType(FieldDecl(357,AndType(Top,AndType(AndType(Top,FieldDecl(358,Top)),AndType(Top,Top)))),FieldDecl(359,FunType(360,TypeDecl(361,AndType(Bot,TypeProj(264,265)),TypeProj(73,74)),TypeProj(96,97)))),FieldDecl(362,Top)))),FieldDecl(363,RecType(364,AndType(TypeDecl(365,FunType(366,Top,Bot),FunType(366,Top,Bot)),FieldDecl(367,TypeDecl(368,FieldDecl(369,Bot),FieldDecl(369,Bot)))))))))),FieldDecl(370,FunType(371,FunType(372,TypeDecl(373,TypeProj(96,97),TypeProj(96,97)),AndType(RecType(374,FunType(375,TypeProj(119,120),TypeProj(264,265))),FieldDecl(376,TypeDecl(377,Bot,FunType(378,AndType(TypeProj(235,236),RecType(379,Top)),FieldDecl(380,TypeProj(168,169))))))),TypeProj(302,317))))))))), 255 -> TypeDecl(256,Bot,Bot), 123 -> TypeDecl(124,Bot,Bot), 314 -> TypeDecl(315,Top,Top), 40 -> TypeDecl(41,Bot,TypeProj(42,43)), 114 -> TypeDecl(115,Bot,AndType(Top,Bot)), 75 -> TypeDecl(76,Bot,Bot), 290 -> FieldDecl(291,RecType(292,Bot)), 119 -> TypeDecl(120,TypeProj(125,126),TypeProj(121,122)), 235 -> TypeDecl(236,RecType(237,TypeProj(128,129)),Top), 383 -> Top, 168 -> TypeDecl(169,Bot,Bot), 4 -> TypeDecl(5,AndType(Bot,Bot),Top), 215 -> FunType(217,AndType(RecType(218,AndType(AndType(FieldDecl(219,TypeProj(119,120)),TypeDecl(220,Bot,TypeDecl(221,Bot,Bot))),AndType(FieldDecl(222,AndType(AndType(AndType(TypeDecl(223,TypeProj(224,225),Bot),FieldDecl(228,TypeProj(189,190))),TypeDecl(229,TypeProj(238,239),RecType(230,AndType(TypeDecl(231,Bot,RecType(232,AndType(TypeProj(125,126),Top))),FieldDecl(233,TypeDecl(234,TypeProj(235,236),Top)))))),FieldDecl(240,AndType(FunType(241,AndType(AndType(RecType(242,TypeDecl(243,TypeProj(98,99),TypeProj(98,99))),FunType(244,TypeProj(114,115),RecType(245,RecType(246,AndType(Bot,AndType(FieldDecl(247,Top),Top)))))),RecType(248,AndType(Top,Bot))),TypeProj(238,239)),Bot)))),AndType(AndType(TypeDecl(249,Bot,TypeDecl(250,RecType(251,TypeProj(40,41)),RecType(251,TypeProj(40,41)))),AndType(TypeDecl(252,AndType(TypeProj(253,254),AndType(AndType(TypeDecl(257,AndType(Bot,Bot),RecType(258,FunType(259,Top,FieldDecl(260,Top)))),AndType(AndType(TypeDecl(261,TypeProj(264,265),FunType(262,Top,RecType(263,TypeProj(235,236)))),FieldDecl(268,Top)),AndType(FieldDecl(269,TypeDecl(270,AndType(Bot,Bot),TypeProj(255,256))),AndType(FieldDecl(271,AndType(RecType(272,FieldDecl(273,Bot)),Bot)),FieldDecl(274,TypeProj(238,239)))))),FieldDecl(275,FieldDecl(276,RecType(277,FieldDecl(278,RecType(279,AndType(FieldDecl(280,Top),AndType(FieldDecl(281,Top),FieldDecl(282,FieldDecl(283,Bot))))))))))),Top),FieldDecl(284,Top))),FieldDecl(285,RecType(286,Top)))))),TypeProj(128,129)),Bot)),385),:?(:%(ILet(0,:?(:%(IVar(1),Que),Bot),:?(:%(IFun(3,AndType(TypeProj(4,5),RecType(6,TypeDecl(7,Top,Top))),:?(:%(ILet(8,:?(:%(ILet(9,:?(:%(ILet(10,:?(:%(IFun(12,Top,:?(:%(IFun(14,FunType(15,FunType(16,Top,RecType(17,FunType(18,AndType(Bot,FieldDecl(19,RecType(20,AndType(AndType(TypeDecl(21,Bot,Top),FieldDecl(22,Top)),AndType(FieldDecl(23,RecType(24,AndType(AndType(TypeDecl(25,AndType(FieldDecl(26,RecType(27,AndType(AndType(TypeDecl(28,TypeProj(20,21),TypeProj(20,21)),FieldDecl(29,FieldDecl(30,Bot))),FieldDecl(31,Top)))),TypeDecl(32,AndType(Bot,Bot),AndType(Bot,Bot))),FieldDecl(26,RecType(27,AndType(AndType(TypeDecl(28,TypeProj(20,21),TypeProj(20,21)),FieldDecl(29,FieldDecl(30,Bot))),FieldDecl(31,Top))))),FieldDecl(33,Bot)),AndType(AndType(FieldDecl(34,FunType(35,FieldDecl(36,Bot),TypeProj(3,7))),AndType(AndType(FieldDecl(37,RecType(38,AndType(Bot,AndType(Bot,RecType(39,TypeProj(40,41)))))),FieldDecl(44,Bot)),FieldDecl(45,Bot))),AndType(AndType(FieldDecl(46,Bot),FieldDecl(47,Top)),FieldDecl(48,Bot)))))),AndType(FieldDecl(49,FieldDecl(50,RecType(51,FieldDecl(52,AndType(Bot,FunType(53,FieldDecl(54,FunType(55,Bot,FunType(56,Top,Top))),TypeProj(40,41))))))),FieldDecl(57,TypeProj(40,41)))))))),RecType(58,Bot)))),FieldDecl(59,AndType(RecType(60,AndType(FieldDecl(61,AndType(FunType(62,Bot,FieldDecl(63,AndType(RecType(64,AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(92,FunType(93,Top,AndType(Top,TypeProj(40,41)))),TypeDecl(94,AndType(Bot,Bot),FieldDecl(95,TypeProj(96,97))))),AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(92,FunType(93,Top,AndType(Top,TypeProj(40,41)))),TypeDecl(94,AndType(Bot,Bot),FieldDecl(95,TypeProj(96,97)))))),AndType(FieldDecl(100,Top),AndType(AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(92,FunType(93,Top,AndType(Top,TypeProj(40,41)))),TypeDecl(94,AndType(Bot,Bot),FieldDecl(95,TypeProj(96,97)))))),FieldDecl(100,Top)),AndType(AndType(AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(92,FunType(93,Top,AndType(Top,TypeProj(40,41)))),TypeDecl(94,AndType(Bot,Bot),FieldDecl(95,TypeProj(96,97)))))),FieldDecl(100,Top)),AndType(AndType(AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(92,FunType(93,Top,AndType(Top,TypeProj(40,41)))),TypeDecl(94,AndType(Bot,Bot),FieldDecl(95,TypeProj(96,97)))))),FieldDecl(100,Top)),AndType(FieldDecl(101,Top),TypeDecl(102,Bot,Bot))),FieldDecl(103,FunType(104,RecType(105,Top),Top)))),TypeDecl(106,Bot,TypeDecl(107,Bot,RecType(108,TypeProj(40,41))))))))))),FieldDecl(109,TypeDecl(110,AndType(TypeProj(64,102),Top),TypeProj(64,102))))),TypeDecl(111,FunType(112,TypeDecl(113,TypeProj(114,115),Top),TypeProj(116,117)),FunType(112,TypeDecl(113,TypeProj(114,115),Top),TypeProj(116,117)))))),AndType(RecType(130,FieldDecl(131,RecType(132,AndType(AndType(AndType(FieldDecl(133,AndType(Top,Top)),TypeDecl(134,AndType(Bot,TypeProj(96,97)),FunType(135,FieldDecl(136,RecType(137,Bot)),RecType(138,FieldDecl(139,TypeProj(40,41)))))),FieldDecl(140,RecType(141,TypeProj(96,97)))),AndType(FieldDecl(142,FunType(143,FieldDecl(144,FunType(145,Top,RecType(146,TypeProj(98,99)))),FieldDecl(147,TypeProj(125,126)))),FieldDecl(148,AndType(Bot,FieldDecl(149,FieldDecl(150,RecType(151,FieldDecl(152,TypeProj(40,41)))))))))))),TypeDecl(153,FieldDecl(154,FunType(155,Top,TypeProj(123,124))),FieldDecl(154,FunType(155,Top,TypeProj(123,124))))))),FieldDecl(156,AndType(FieldDecl(157,FunType(158,Bot,TypeProj(98,99))),FieldDecl(159,AndType(RecType(160,FunType(161,RecType(162,AndType(AndType(FieldDecl(163,TypeProj(40,41)),TypeDecl(164,FieldDecl(165,TypeProj(166,167)),FieldDecl(165,TypeProj(125,126)))),FieldDecl(175,AndType(AndType(FunType(176,RecType(177,AndType(FieldDecl(178,TypeProj(42,43)),AndType(AndType(AndType(AndType(FieldDecl(179,Bot),FieldDecl(180,Top)),FieldDecl(181,TypeProj(71,72))),FieldDecl(182,AndType(FieldDecl(183,Bot),FieldDecl(184,Bot)))),AndType(FieldDecl(185,RecType(186,AndType(TypeProj(168,169),AndType(Bot,TypeDecl(187,TypeDecl(188,TypeProj(189,190),Top),TypeDecl(188,TypeProj(189,190),Top)))))),FieldDecl(191,Bot))))),AndType(TypeDecl(192,AndType(Bot,Top),RecType(193,AndType(FieldDecl(194,FunType(195,TypeProj(196,197),AndType(AndType(FieldDecl(200,FunType(201,TypeProj(4,5),Top)),TypeDecl(202,RecType(203,AndType(TypeDecl(204,Bot,FunType(205,AndType(Bot,Top),TypeProj(128,129))),AndType(FieldDecl(206,Top),FieldDecl(207,Bot)))),RecType(203,AndType(TypeDecl(204,Bot,FunType(205,AndType(Bot,Top),TypeProj(128,129))),AndType(FieldDecl(206,Top),FieldDecl(207,Bot)))))),FieldDecl(208,TypeProj(42,43))))),AndType(FieldDecl(209,Bot),FieldDecl(210,Top))))),Top)),RecType(211,Bot)),Top)))),RecType(212,Bot))),Top)))))),TypeDecl(213,RecType(214,TypeProj(3,7)),RecType(214,TypeProj(3,7)))))),:?(:%(IApp(215,216),Que),Bot)),Que),FunType(14,FunType(15,FunType(16,Top,RecType(17,FunType(18,AndType(Bot,FieldDecl(19,RecType(20,AndType(AndType(TypeDecl(21,Bot,Top),FieldDecl(22,Top)),AndType(FieldDecl(23,RecType(24,AndType(AndType(TypeDecl(25,AndType(FieldDecl(26,RecType(27,AndType(AndType(TypeDecl(28,TypeProj(20,21),TypeProj(20,21)),FieldDecl(29,FieldDecl(30,Bot))),FieldDecl(31,Top)))),TypeDecl(32,AndType(Bot,Bot),AndType(Bot,Bot))),FieldDecl(26,RecType(27,AndType(AndType(TypeDecl(28,TypeProj(20,21),TypeProj(20,21)),FieldDecl(29,FieldDecl(30,Bot))),FieldDecl(31,Top))))),FieldDecl(33,Bot)),AndType(AndType(FieldDecl(34,FunType(35,FieldDecl(36,Bot),TypeProj(3,7))),AndType(AndType(FieldDecl(37,RecType(38,AndType(Bot,AndType(Bot,RecType(39,TypeProj(40,41)))))),FieldDecl(44,Bot)),FieldDecl(45,Bot))),AndType(AndType(FieldDecl(46,Bot),FieldDecl(47,Top)),FieldDecl(48,Bot)))))),AndType(FieldDecl(49,FieldDecl(50,RecType(51,FieldDecl(52,AndType(Bot,FunType(53,FieldDecl(54,FunType(55,Bot,FunType(56,Top,Top))),TypeProj(40,41))))))),FieldDecl(57,TypeProj(40,41)))))))),RecType(58,Bot)))),FieldDecl(59,AndType(RecType(60,AndType(FieldDecl(61,AndType(FunType(62,Bot,FieldDecl(63,AndType(RecType(64,AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(92,FunType(93,Top,AndType(Top,TypeProj(40,41)))),TypeDecl(94,AndType(Bot,Bot),FieldDecl(95,TypeProj(96,97))))),AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(92,FunType(93,Top,AndType(Top,TypeProj(40,41)))),TypeDecl(94,AndType(Bot,Bot),FieldDecl(95,TypeProj(96,97)))))),AndType(FieldDecl(100,Top),AndType(AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(92,FunType(93,Top,AndType(Top,TypeProj(40,41)))),TypeDecl(94,AndType(Bot,Bot),FieldDecl(95,TypeProj(96,97)))))),FieldDecl(100,Top)),AndType(AndType(AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(92,FunType(93,Top,AndType(Top,TypeProj(40,41)))),TypeDecl(94,AndType(Bot,Bot),FieldDecl(95,TypeProj(96,97)))))),FieldDecl(100,Top)),AndType(AndType(AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(92,FunType(93,Top,AndType(Top,TypeProj(40,41)))),TypeDecl(94,AndType(Bot,Bot),FieldDecl(95,TypeProj(96,97)))))),FieldDecl(100,Top)),AndType(FieldDecl(101,Top),TypeDecl(102,Bot,Bot))),FieldDecl(103,FunType(104,RecType(105,Top),Top)))),TypeDecl(106,Bot,TypeDecl(107,Bot,RecType(108,TypeProj(40,41))))))))))),FieldDecl(109,TypeDecl(110,AndType(TypeProj(64,102),Top),TypeProj(64,102))))),TypeDecl(111,FunType(112,TypeDecl(113,TypeProj(114,115),Top),TypeProj(116,117)),FunType(112,TypeDecl(113,TypeProj(114,115),Top),TypeProj(116,117)))))),AndType(RecType(130,FieldDecl(131,RecType(132,AndType(AndType(AndType(FieldDecl(133,AndType(Top,Top)),TypeDecl(134,AndType(Bot,TypeProj(96,97)),FunType(135,FieldDecl(136,RecType(137,Bot)),RecType(138,FieldDecl(139,TypeProj(40,41)))))),FieldDecl(140,RecType(141,TypeProj(96,97)))),AndType(FieldDecl(142,FunType(143,FieldDecl(144,FunType(145,Top,RecType(146,TypeProj(98,99)))),FieldDecl(147,TypeProj(125,126)))),FieldDecl(148,AndType(Bot,FieldDecl(149,FieldDecl(150,RecType(151,FieldDecl(152,TypeProj(40,41)))))))))))),TypeDecl(153,FieldDecl(154,FunType(155,Top,TypeProj(123,124))),FieldDecl(154,FunType(155,Top,TypeProj(123,124))))))),FieldDecl(156,AndType(FieldDecl(157,FunType(158,Bot,TypeProj(98,99))),FieldDecl(159,AndType(RecType(160,FunType(161,RecType(162,AndType(AndType(FieldDecl(163,TypeProj(40,41)),TypeDecl(164,FieldDecl(165,TypeProj(166,167)),FieldDecl(165,TypeProj(125,126)))),FieldDecl(175,AndType(AndType(FunType(176,RecType(177,AndType(FieldDecl(178,TypeProj(42,43)),AndType(AndType(AndType(AndType(FieldDecl(179,Bot),FieldDecl(180,Top)),FieldDecl(181,TypeProj(71,72))),FieldDecl(182,AndType(FieldDecl(183,Bot),FieldDecl(184,Bot)))),AndType(FieldDecl(185,RecType(186,AndType(TypeProj(168,169),AndType(Bot,TypeDecl(187,TypeDecl(188,TypeProj(189,190),Top),TypeDecl(188,TypeProj(189,190),Top)))))),FieldDecl(191,Bot))))),AndType(TypeDecl(192,AndType(Bot,Top),RecType(193,AndType(FieldDecl(194,FunType(195,TypeProj(196,197),AndType(AndType(FieldDecl(200,FunType(201,TypeProj(4,5),Top)),TypeDecl(202,RecType(203,AndType(TypeDecl(204,Bot,FunType(205,AndType(Bot,Top),TypeProj(128,129))),AndType(FieldDecl(206,Top),FieldDecl(207,Bot)))),RecType(203,AndType(TypeDecl(204,Bot,FunType(205,AndType(Bot,Top),TypeProj(128,129))),AndType(FieldDecl(206,Top),FieldDecl(207,Bot)))))),FieldDecl(208,TypeProj(42,43))))),AndType(FieldDecl(209,Bot),FieldDecl(210,Top))))),Top)),RecType(211,Bot)),Top)))),RecType(212,Bot))),Top)))))),TypeDecl(213,RecType(214,TypeProj(3,7)),RecType(214,TypeProj(3,7)))))),Bot))),Que),FunType(12,Top,FunType(14,FunType(15,FunType(16,Top,RecType(17,FunType(18,AndType(Bot,FieldDecl(19,RecType(20,AndType(AndType(TypeDecl(21,Bot,Top),FieldDecl(22,Top)),AndType(FieldDecl(23,RecType(24,AndType(AndType(TypeDecl(25,AndType(FieldDecl(26,RecType(27,AndType(AndType(TypeDecl(28,TypeProj(20,21),TypeProj(20,21)),FieldDecl(29,FieldDecl(30,Bot))),FieldDecl(31,Top)))),TypeDecl(32,AndType(Bot,Bot),AndType(Bot,Bot))),FieldDecl(26,RecType(27,AndType(AndType(TypeDecl(28,TypeProj(20,21),TypeProj(20,21)),FieldDecl(29,FieldDecl(30,Bot))),FieldDecl(31,Top))))),FieldDecl(33,Bot)),AndType(AndType(FieldDecl(34,FunType(35,FieldDecl(36,Bot),TypeProj(3,7))),AndType(AndType(FieldDecl(37,RecType(38,AndType(Bot,AndType(Bot,RecType(39,TypeProj(40,41)))))),FieldDecl(44,Bot)),FieldDecl(45,Bot))),AndType(AndType(FieldDecl(46,Bot),FieldDecl(47,Top)),FieldDecl(48,Bot)))))),AndType(FieldDecl(49,FieldDecl(50,RecType(51,FieldDecl(52,AndType(Bot,FunType(53,FieldDecl(54,FunType(55,Bot,FunType(56,Top,Top))),TypeProj(40,41))))))),FieldDecl(57,TypeProj(40,41)))))))),RecType(58,Bot)))),FieldDecl(59,AndType(RecType(60,AndType(FieldDecl(61,AndType(FunType(62,Bot,FieldDecl(63,AndType(RecType(64,AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(92,FunType(93,Top,AndType(Top,TypeProj(40,41)))),TypeDecl(94,AndType(Bot,Bot),FieldDecl(95,TypeProj(96,97))))),AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(92,FunType(93,Top,AndType(Top,TypeProj(40,41)))),TypeDecl(94,AndType(Bot,Bot),FieldDecl(95,TypeProj(96,97)))))),AndType(FieldDecl(100,Top),AndType(AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(92,FunType(93,Top,AndType(Top,TypeProj(40,41)))),TypeDecl(94,AndType(Bot,Bot),FieldDecl(95,TypeProj(96,97)))))),FieldDecl(100,Top)),AndType(AndType(AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(92,FunType(93,Top,AndType(Top,TypeProj(40,41)))),TypeDecl(94,AndType(Bot,Bot),FieldDecl(95,TypeProj(96,97)))))),FieldDecl(100,Top)),AndType(AndType(AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(92,FunType(93,Top,AndType(Top,TypeProj(40,41)))),TypeDecl(94,AndType(Bot,Bot),FieldDecl(95,TypeProj(96,97)))))),FieldDecl(100,Top)),AndType(FieldDecl(101,Top),TypeDecl(102,Bot,Bot))),FieldDecl(103,FunType(104,RecType(105,Top),Top)))),TypeDecl(106,Bot,TypeDecl(107,Bot,RecType(108,TypeProj(40,41))))))))))),FieldDecl(109,TypeDecl(110,AndType(TypeProj(64,102),Top),TypeProj(64,102))))),TypeDecl(111,FunType(112,TypeDecl(113,TypeProj(114,115),Top),TypeProj(116,117)),FunType(112,TypeDecl(113,TypeProj(114,115),Top),TypeProj(116,117)))))),AndType(RecType(130,FieldDecl(131,RecType(132,AndType(AndType(AndType(FieldDecl(133,AndType(Top,Top)),TypeDecl(134,AndType(Bot,TypeProj(96,97)),FunType(135,FieldDecl(136,RecType(137,Bot)),RecType(138,FieldDecl(139,TypeProj(40,41)))))),FieldDecl(140,RecType(141,TypeProj(96,97)))),AndType(FieldDecl(142,FunType(143,FieldDecl(144,FunType(145,Top,RecType(146,TypeProj(98,99)))),FieldDecl(147,TypeProj(125,126)))),FieldDecl(148,AndType(Bot,FieldDecl(149,FieldDecl(150,RecType(151,FieldDecl(152,TypeProj(40,41)))))))))))),TypeDecl(153,FieldDecl(154,FunType(155,Top,TypeProj(123,124))),FieldDecl(154,FunType(155,Top,TypeProj(123,124))))))),FieldDecl(156,AndType(FieldDecl(157,FunType(158,Bot,TypeProj(98,99))),FieldDecl(159,AndType(RecType(160,FunType(161,RecType(162,AndType(AndType(FieldDecl(163,TypeProj(40,41)),TypeDecl(164,FieldDecl(165,TypeProj(166,167)),FieldDecl(165,TypeProj(125,126)))),FieldDecl(175,AndType(AndType(FunType(176,RecType(177,AndType(FieldDecl(178,TypeProj(42,43)),AndType(AndType(AndType(AndType(FieldDecl(179,Bot),FieldDecl(180,Top)),FieldDecl(181,TypeProj(71,72))),FieldDecl(182,AndType(FieldDecl(183,Bot),FieldDecl(184,Bot)))),AndType(FieldDecl(185,RecType(186,AndType(TypeProj(168,169),AndType(Bot,TypeDecl(187,TypeDecl(188,TypeProj(189,190),Top),TypeDecl(188,TypeProj(189,190),Top)))))),FieldDecl(191,Bot))))),AndType(TypeDecl(192,AndType(Bot,Top),RecType(193,AndType(FieldDecl(194,FunType(195,TypeProj(196,197),AndType(AndType(FieldDecl(200,FunType(201,TypeProj(4,5),Top)),TypeDecl(202,RecType(203,AndType(TypeDecl(204,Bot,FunType(205,AndType(Bot,Top),TypeProj(128,129))),AndType(FieldDecl(206,Top),FieldDecl(207,Bot)))),RecType(203,AndType(TypeDecl(204,Bot,FunType(205,AndType(Bot,Top),TypeProj(128,129))),AndType(FieldDecl(206,Top),FieldDecl(207,Bot)))))),FieldDecl(208,TypeProj(42,43))))),AndType(FieldDecl(209,Bot),FieldDecl(210,Top))))),Top)),RecType(211,Bot)),Top)))),RecType(212,Bot))),Top)))))),TypeDecl(213,RecType(214,TypeProj(3,7)),RecType(214,TypeProj(3,7)))))),Bot))),:?(:%(IFun(288,Bot,:?(:%(ILet(289,:?(:%(ISel(290,291),Que),RecType(292,Bot)),:?(:%(ISel(293,294),Que),Bot)),Que),Bot)),Que),FunType(288,Bot,Bot))),Que),FunType(288,Bot,Bot)),:?(:%(ILet(295,:?(:%(ISel(296,297),Que),Top),:?(:%(ILet(298,:?(:%(ISel(299,300),Que),FunType(301,TypeProj(123,124),RecType(302,AndType(AndType(AndType(AndType(FieldDecl(303,AndType(TypeDecl(304,TypeProj(306,307),RecType(305,Bot)),FieldDecl(308,FunType(309,FunType(310,AndType(Bot,Bot),Top),AndType(TypeDecl(311,AndType(TypeProj(312,313),RecType(316,TypeProj(224,225))),Top),Top))))),AndType(FieldDecl(303,AndType(TypeDecl(304,TypeProj(306,307),RecType(305,Bot)),FieldDecl(308,FunType(309,FunType(310,AndType(Bot,Bot),Top),AndType(TypeDecl(311,AndType(TypeProj(312,313),RecType(316,TypeProj(224,225))),Top),Top))))),AndType(AndType(FieldDecl(303,AndType(TypeDecl(304,TypeProj(306,307),RecType(305,Bot)),FieldDecl(308,FunType(309,FunType(310,AndType(Bot,Bot),Top),AndType(TypeDecl(311,AndType(TypeProj(312,313),RecType(316,TypeProj(224,225))),Top),Top))))),AndType(TypeDecl(317,Bot,Bot),TypeDecl(318,FieldDecl(319,Bot),FieldDecl(319,RecType(320,FunType(321,RecType(322,Bot),TypeProj(116,117))))))),FieldDecl(323,TypeDecl(324,AndType(Bot,TypeDecl(339,AndType(Bot,RecType(340,Bot)),Bot)),FunType(325,FunType(326,TypeProj(73,74),FieldDecl(327,RecType(328,RecType(329,RecType(330,AndType(TypeDecl(331,AndType(Bot,RecType(332,Top)),TypeProj(168,169)),AndType(AndType(FieldDecl(333,FieldDecl(334,TypeDecl(335,Bot,Bot))),FieldDecl(336,Top)),AndType(FieldDecl(337,Bot),FieldDecl(338,Top))))))))),Top)))))),FieldDecl(341,FunType(342,Top,TypeProj(216,252)))),FieldDecl(343,RecType(344,AndType(TypeDecl(345,RecType(346,Top),RecType(346,Top)),AndType(FieldDecl(347,Top),AndType(FieldDecl(348,TypeProj(224,225)),FieldDecl(349,Top))))))),FieldDecl(350,FunType(351,RecType(352,TypeDecl(353,Bot,FunType(354,Bot,AndType(TypeDecl(355,AndType(Bot,Top),RecType(356,AndType(AndType(FieldDecl(357,AndType(Top,AndType(AndType(Top,FieldDecl(358,Top)),AndType(Top,Top)))),FieldDecl(359,FunType(360,TypeDecl(361,AndType(Bot,TypeProj(264,265)),TypeProj(73,74)),TypeProj(96,97)))),FieldDecl(362,Top)))),FieldDecl(363,RecType(364,AndType(TypeDecl(365,FunType(366,Top,Bot),FunType(366,Top,Bot)),FieldDecl(367,TypeDecl(368,FieldDecl(369,Bot),FieldDecl(369,Bot)))))))))),FieldDecl(370,FunType(371,FunType(372,TypeDecl(373,TypeProj(96,97),TypeProj(96,97)),AndType(RecType(374,FunType(375,TypeProj(119,120),TypeProj(264,265))),FieldDecl(376,TypeDecl(377,Bot,FunType(378,AndType(TypeProj(235,236),RecType(379,Top)),FieldDecl(380,TypeProj(168,169))))))),TypeProj(302,317))))))))),:?(:%(IVar(381),Que),TypeProj(238,239))),Que),TypeProj(238,239))),Que),TypeProj(238,239))),Que),TypeProj(238,239)),:?(:%(IApp(382,383),Que),Bot)),Que),Bot)),Que),FunType(3,AndType(TypeProj(4,5),RecType(6,TypeDecl(7,Top,Top))),Bot))),Que),FunType(3,AndType(TypeProj(4,5),RecType(6,TypeDecl(7,Top,Top))),Bot)))

//P.namedln("problem", p)

val InferenceProblem(GlobalContext(scope, nextSymbol), term, prototype, expected) = p

val su = new SymbolUniverse(nextSymbol)

//P.namedln("scope", scope)
//P.namedln("term", term)
//P.namedln("expected", expected)

val startTime = System.nanoTime()
val resTerm = typecheckTerm(su, term, prototype, scope)
val endTime = System.nanoTime()

P.namedln("resTerm", resTerm)



P.namedln("scope", scope)
P.namedln("eqcheck", eqcheck(scope, resTerm, expected))
P.namedln("res == expected", equalTerms(scope, resTerm, expected))

P.namedln("TIME", s"${(endTime - startTime)*1E-9}s")
}

def debugpartc(): Unit = {

val p: InferenceProblem =


//InferenceProblem(
//  GlobalContext(Map(0 -> Top), 1),
//  Var(0),
//  Que,
//  TypedVar(0) :- Top
//)

//InferenceProblem(
//  GlobalContext(
//    Map(
//      10 -> TypeProj(2, 3),
//      6 -> TypeDecl(7, TypeProj(8, 9), Top),
//      2 -> TypeDecl(3, TypeProj(4, 5), Top),
//      8 -> TypeDecl(9, Top, Top),
//      4 -> TypeDecl(5, Top, TypeProj(6, 7))
//    ),
//    11
//  ),
//  Var(10),
//  Que,
//  TypedVar(10) :- TypeProj(2, 3)
//)


//InferenceProblem(
//  GlobalContext(Map(7 -> FunType(9, Top, Top), 8 -> Top), 10),
//  App(7, 8),
//  Que,
//  TypedApp(7, 8) :- Top
//)

//InferenceProblem(
//  GlobalContext(
//    Map(
//      38 -> Bot,
//      39 -> FunType(24, TypeProj(8, 10), FieldDecl(27, Top)),
//      8 -> TypeDecl(10, Top, Top)
//    ),
//    40
//  ),
//  Let(18, Var(38), Var(39)),
//  Que,
//  TypedLet(
//    18,
//    TypedVar(38) :- Bot,
//    TypedVar(39) :- FunType(24, TypeProj(8, 10), FieldDecl(27, Top))
//  ) :- FunType(24, TypeProj(8, 10), FieldDecl(27, Top))
//)

//InferenceProblem(
//  GlobalContext(
//    Map(
//      18 -> FunType(
//        5,
//        Top,
//        FunType(7, Bot, FunType(9, Top, TypeProj(11, 12)))
//      ),
//      11 -> TypeDecl(12, Bot, Bot),
//      13 -> Top
//    ),
//    19
//  ),
//  Let(3, Var(18), Var(13)),
//  Que,
//  TypedLet(
//    3,
//    TypedVar(18) :- FunType(
//      5,
//      Top,
//      FunType(7, Bot, FunType(9, Top, TypeProj(11, 12)))
//    ),
//    TypedVar(13) :- Top
//  ) :- Top
//)

//InferenceProblem(
//  GlobalContext(
//    Map(
//      0 -> FieldDecl(
//        1,
//        RecType(
//          2,
//          AndType(
//            TypeDecl(
//              3,
//              Bot,
//              RecType(
//                4,
//                TypeDecl(
//                  5,
//                  AndType(TypeProj(6, 7), Top),
//                  AndType(TypeProj(6, 7), Top)
//                )
//              )
//            ),
//            AndType(
//              AndType(
//                FieldDecl(
//                  19,
//                  TypeDecl(
//                    20,
//                    AndType(TypeProj(34, 35), Top),
//                    AndType(
//                      TypeDecl(
//                        21,
//                        AndType(Bot, TypeProj(10, 11)),
//                        TypeProj(8, 9)
//                      ),
//                      FunType(
//                        22,
//                        Top,
//                        TypeDecl(
//                          23,
//                          Bot,
//                          AndType(
//                            RecType(24, Bot),
//                            FunType(
//                              25,
//                              RecType(
//                                26,
//                                AndType(
//                                  FieldDecl(
//                                    27,
//                                    FunType(
//                                      28,
//                                      Top,
//                                      TypeDecl(29, Bot, TypeProj(10, 11))
//                                    )
//                                  ),
//                                  FieldDecl(30, TypeProj(6, 7))
//                                )
//                              ),
//                              RecType(31, RecType(32, RecType(33, Bot)))
//                            )
//                          )
//                        )
//                      )
//                    )
//                  )
//                ),
//                AndType(
//                  AndType(
//                    FieldDecl(45, RecType(46, Top)),
//                    AndType(
//                      FieldDecl(47, RecType(48, Top)),
//                      FieldDecl(
//                        49,
//                        FieldDecl(
//                          50,
//                          AndType(
//                            FunType(
//                              51,
//                              AndType(
//                                Top,
//                                AndType(
//                                  TypeProj(6, 7),
//                                  RecType(52, TypeProj(43, 44))
//                                )
//                              ),
//                              Bot
//                            ),
//                            TypeProj(36, 37)
//                          )
//                        )
//                      )
//                    )
//                  ),
//                  FieldDecl(53, TypeProj(10, 11))
//                )
//              ),
//              AndType(
//                AndType(
//                  FieldDecl(54, RecType(55, TypeProj(40, 41))),
//                  AndType(
//                    AndType(
//                      AndType(
//                        FieldDecl(
//                          56,
//                          RecType(
//                            57,
//                            FieldDecl(
//                              58,
//                              AndType(
//                                TypeDecl(
//                                  59,
//                                  Bot,
//                                  FunType(
//                                    60,
//                                    RecType(
//                                      61,
//                                      RecType(
//                                        62,
//                                        AndType(
//                                          FunType(63, Top, Bot),
//                                          AndType(
//                                            FunType(
//                                              64,
//                                              FieldDecl(
//                                                65,
//                                                AndType(
//                                                  FieldDecl(
//                                                    66,
//                                                    RecType(
//                                                      67,
//                                                      RecType(
//                                                        68,
//                                                        FieldDecl(
//                                                          69,
//                                                          AndType(
//                                                            TypeProj(40, 41),
//                                                            AndType(
//                                                              RecType(
//                                                                70,
//                                                                TypeDecl(
//                                                                  71,
//                                                                  Bot,
//                                                                  RecType(
//                                                                    72,
//                                                                    TypeProj(
//                                                                      2,
//                                                                      3
//                                                                    )
//                                                                  )
//                                                                )
//                                                              ),
//                                                              Top
//                                                            )
//                                                          )
//                                                        )
//                                                      )
//                                                    )
//                                                  ),
//                                                  FieldDecl(73, Bot)
//                                                )
//                                              ),
//                                              Bot
//                                            ),
//                                            FieldDecl(
//                                              74,
//                                              TypeDecl(
//                                                75,
//                                                AndType(
//                                                  TypeProj(76, 77),
//                                                  TypeProj(6, 7)
//                                                ),
//                                                Bot
//                                              )
//                                            )
//                                          )
//                                        )
//                                      )
//                                    ),
//                                    Bot
//                                  )
//                                ),
//                                FieldDecl(78, Bot)
//                              )
//                            )
//                          )
//                        ),
//                        FieldDecl(79, TypeProj(76, 77))
//                      ),
//                      AndType(FieldDecl(80, Bot), FieldDecl(81, Top))
//                    ),
//                    AndType(
//                      AndType(
//                        AndType(
//                          AndType(
//                            AndType(
//                              AndType(
//                                FieldDecl(82, Bot),
//                                FieldDecl(
//                                  83,
//                                  AndType(
//                                    RecType(84, TypeProj(17, 18)),
//                                    TypeProj(85, 86)
//                                  )
//                                )
//                              ),
//                              FieldDecl(87, RecType(88, FieldDecl(89, Bot)))
//                            ),
//                            FieldDecl(90, Top)
//                          ),
//                          FieldDecl(
//                            91,
//                            RecType(92, FieldDecl(93, TypeProj(17, 18)))
//                          )
//                        ),
//                        AndType(
//                          FieldDecl(94, FieldDecl(95, TypeProj(13, 14))),
//                          FieldDecl(96, FunType(97, Bot, TypeProj(40, 41)))
//                        )
//                      ),
//                      FieldDecl(98, Bot)
//                    )
//                  )
//                ),
//                AndType(
//                  FieldDecl(99, RecType(100, FunType(101, Bot, Bot))),
//                  AndType(
//                    AndType(
//                      AndType(
//                        AndType(FieldDecl(102, Top), FieldDecl(103, Bot)),
//                        FieldDecl(104, AndType(Bot, TypeProj(105, 106)))
//                      ),
//                      FieldDecl(
//                        108,
//                        AndType(
//                          FieldDecl(109, Top),
//                          TypeDecl(
//                            110,
//                            TypeProj(115, 116),
//                            FunType(
//                              111,
//                              FieldDecl(
//                                112,
//                                FunType(
//                                  113,
//                                  Top,
//                                  TypeDecl(
//                                    114,
//                                    TypeProj(105, 106),
//                                    TypeProj(105, 106)
//                                  )
//                                )
//                              ),
//                              TypeProj(40, 41)
//                            )
//                          )
//                        )
//                      )
//                    ),
//                    AndType(
//                      FieldDecl(
//                        119,
//                        FunType(
//                          120,
//                          AndType(
//                            AndType(
//                              AndType(
//                                TypeProj(115, 116),
//                                RecType(121, TypeProj(76, 77))
//                              ),
//                              Bot
//                            ),
//                            AndType(
//                              TypeDecl(122, Bot, Top),
//                              FieldDecl(123, TypeProj(13, 14))
//                            )
//                          ),
//                          RecType(
//                            124,
//                            RecType(
//                              125,
//                              FunType(
//                                126,
//                                TypeProj(10, 11),
//                                AndType(
//                                  AndType(TypeProj(43, 44), Bot),
//                                  RecType(127, TypeProj(76, 77))
//                                )
//                              )
//                            )
//                          )
//                        )
//                      ),
//                      FieldDecl(128, Top)
//                    )
//                  )
//                )
//              )
//            )
//          )
//        )
//      ),
//      115 -> TypeDecl(116, TypeProj(117, 118), Bot),
//      10 -> TypeDecl(
//        11,
//        Bot,
//        AndType(
//          TypeDecl(12, Bot, TypeProj(13, 14)),
//          FieldDecl(15, FunType(16, Top, TypeProj(17, 18)))
//        )
//      ),
//      6 -> TypeDecl(7, AndType(Bot, TypeProj(8, 9)), AndType(Bot, TypeProj(8, 9))),
//      117 -> TypeDecl(118, Bot, Bot),
//      85 -> TypeDecl(86, Top, Top),
//      38 -> TypeDecl(39, AndType(Bot, TypeProj(8, 9)), Bot),
//      13 -> TypeDecl(14, Bot, Top),
//      105 -> TypeDecl(106, AndType(Bot, RecType(107, Bot)), Bot),
//      34 -> TypeDecl(
//        35,
//        AndType(
//          AndType(
//            AndType(TypeProj(36, 37), TypeProj(17, 18)),
//            AndType(TypeProj(40, 41), FieldDecl(42, Top))
//          ),
//          TypeProj(43, 44)
//        ),
//        AndType(
//          AndType(
//            AndType(TypeProj(36, 37), TypeProj(17, 18)),
//            AndType(TypeProj(40, 41), FieldDecl(42, Top))
//          ),
//          TypeProj(43, 44)
//        )
//      ),
//      17 -> TypeDecl(18, Bot, Bot),
//      76 -> TypeDecl(77, Bot, Bot),
//      43 -> TypeDecl(44, Bot, Bot),
//      40 -> TypeDecl(41, Bot, Bot),
//      8 -> TypeDecl(9, AndType(Bot, TypeProj(10, 11)), Bot),
//      36 -> TypeDecl(37, TypeProj(38, 39), Bot)
//    ),
//    129
//  ),
//  Sel(0, 1), // TODO wait, what!?
//  Que,
//  TypedSel(0, 1) :- RecType(
//    2,
//    AndType(
//      TypeDecl(
//        3,
//        Bot,
//        RecType(
//          4,
//          TypeDecl(
//            5,
//            AndType(TypeProj(6, 7), Top),
//            AndType(TypeProj(6, 7), Top)
//          )
//        )
//      ),
//      AndType(
//        AndType(
//          FieldDecl(
//            19,
//            TypeDecl(
//              20,
//              AndType(TypeProj(34, 35), Top),
//              AndType(
//                TypeDecl(21, AndType(Bot, TypeProj(10, 11)), TypeProj(8, 9)),
//                FunType(
//                  22,
//                  Top,
//                  TypeDecl(
//                    23,
//                    Bot,
//                    AndType(
//                      RecType(24, Bot),
//                      FunType(
//                        25,
//                        RecType(
//                          26,
//                          AndType(
//                            FieldDecl(
//                              27,
//                              FunType(
//                                28,
//                                Top,
//                                TypeDecl(29, Bot, TypeProj(10, 11))
//                              )
//                            ),
//                            FieldDecl(30, TypeProj(6, 7))
//                          )
//                        ),
//                        RecType(31, RecType(32, RecType(33, Bot)))
//                      )
//                    )
//                  )
//                )
//              )
//            )
//          ),
//          AndType(
//            AndType(
//              FieldDecl(45, RecType(46, Top)),
//              AndType(
//                FieldDecl(47, RecType(48, Top)),
//                FieldDecl(
//                  49,
//                  FieldDecl(
//                    50,
//                    AndType(
//                      FunType(
//                        51,
//                        AndType(
//                          Top,
//                          AndType(
//                            TypeProj(6, 7),
//                            RecType(52, TypeProj(43, 44))
//                          )
//                        ),
//                        Bot
//                      ),
//                      TypeProj(36, 37)
//                    )
//                  )
//                )
//              )
//            ),
//            FieldDecl(53, TypeProj(10, 11))
//          )
//        ),
//        AndType(
//          AndType(
//            FieldDecl(54, RecType(55, TypeProj(40, 41))),
//            AndType(
//              AndType(
//                AndType(
//                  FieldDecl(
//                    56,
//                    RecType(
//                      57,
//                      FieldDecl(
//                        58,
//                        AndType(
//                          TypeDecl(
//                            59,
//                            Bot,
//                            FunType(
//                              60,
//                              RecType(
//                                61,
//                                RecType(
//                                  62,
//                                  AndType(
//                                    FunType(63, Top, Bot),
//                                    AndType(
//                                      FunType(
//                                        64,
//                                        FieldDecl(
//                                          65,
//                                          AndType(
//                                            FieldDecl(
//                                              66,
//                                              RecType(
//                                                67,
//                                                RecType(
//                                                  68,
//                                                  FieldDecl(
//                                                    69,
//                                                    AndType(
//                                                      TypeProj(40, 41),
//                                                      AndType(
//                                                        RecType(
//                                                          70,
//                                                          TypeDecl(
//                                                            71,
//                                                            Bot,
//                                                            RecType(
//                                                              72,
//                                                              TypeProj(2, 3)
//                                                            )
//                                                          )
//                                                        ),
//                                                        Top
//                                                      )
//                                                    )
//                                                  )
//                                                )
//                                              )
//                                            ),
//                                            FieldDecl(73, Bot)
//                                          )
//                                        ),
//                                        Bot
//                                      ),
//                                      FieldDecl(
//                                        74,
//                                        TypeDecl(
//                                          75,
//                                          AndType(
//                                            TypeProj(76, 77),
//                                            TypeProj(6, 7)
//                                          ),
//                                          Bot
//                                        )
//                                      )
//                                    )
//                                  )
//                                )
//                              ),
//                              Bot
//                            )
//                          ),
//                          FieldDecl(78, Bot)
//                        )
//                      )
//                    )
//                  ),
//                  FieldDecl(79, TypeProj(76, 77))
//                ),
//                AndType(FieldDecl(80, Bot), FieldDecl(81, Top))
//              ),
//              AndType(
//                AndType(
//                  AndType(
//                    AndType(
//                      AndType(
//                        AndType(
//                          FieldDecl(82, Bot),
//                          FieldDecl(
//                            83,
//                            AndType(
//                              RecType(84, TypeProj(17, 18)),
//                              TypeProj(85, 86)
//                            )
//                          )
//                        ),
//                        FieldDecl(87, RecType(88, FieldDecl(89, Bot)))
//                      ),
//                      FieldDecl(90, Top)
//                    ),
//                    FieldDecl(
//                      91,
//                      RecType(92, FieldDecl(93, TypeProj(17, 18)))
//                    )
//                  ),
//                  AndType(
//                    FieldDecl(94, FieldDecl(95, TypeProj(13, 14))),
//                    FieldDecl(96, FunType(97, Bot, TypeProj(40, 41)))
//                  )
//                ),
//                FieldDecl(98, Bot)
//              )
//            )
//          ),
//          AndType(
//            FieldDecl(99, RecType(100, FunType(101, Bot, Bot))),
//            AndType(
//              AndType(
//                AndType(
//                  AndType(FieldDecl(102, Top), FieldDecl(103, Bot)),
//                  FieldDecl(104, AndType(Bot, TypeProj(105, 106)))
//                ),
//                FieldDecl(
//                  108,
//                  AndType(
//                    FieldDecl(109, Top),
//                    TypeDecl(
//                      110,
//                      TypeProj(115, 116),
//                      FunType(
//                        111,
//                        FieldDecl(
//                          112,
//                          FunType(
//                            113,
//                            Top,
//                            TypeDecl(
//                              114,
//                              TypeProj(105, 106),
//                              TypeProj(105, 106)
//                            )
//                          )
//                        ),
//                        TypeProj(40, 41)
//                      )
//                    )
//                  )
//                )
//              ),
//              AndType(
//                FieldDecl(
//                  119,
//                  FunType(
//                    120,
//                    AndType(
//                      AndType(
//                        AndType(
//                          TypeProj(115, 116),
//                          RecType(121, TypeProj(76, 77))
//                        ),
//                        Bot
//                      ),
//                      AndType(
//                        TypeDecl(122, Bot, Top),
//                        FieldDecl(123, TypeProj(13, 14))
//                      )
//                    ),
//                    RecType(
//                      124,
//                      RecType(
//                        125,
//                        FunType(
//                          126,
//                          TypeProj(10, 11),
//                          AndType(
//                            AndType(TypeProj(43, 44), Bot),
//                            RecType(127, TypeProj(76, 77))
//                          )
//                        )
//                      )
//                    )
//                  )
//                ),
//                FieldDecl(128, Top)
//              )
//            )
//          )
//        )
//      )
//    )
//  )
//)

//InferenceProblem(
//  GlobalContext(
//    Map(
//      142 -> TypeDecl(143, TypeProj(119, 120), Top),
//      42 -> TypeDecl(43, Bot, TypeProj(11, 12)),
//      164 -> FieldDecl(165, Top),
//      1 -> FieldDecl(2, TypeProj(3, 4)),
//      117 -> TypeDecl(
//        118,
//        Bot,
//        AndType(TypeProj(119, 120), TypeDecl(121, Bot, Bot))
//      ),
//      137 -> TypeDecl(138, Bot, Top),
//      109 -> TypeDecl(110, Bot, Bot),
//      105 -> TypeDecl(106, TypeProj(107, 108), TypeProj(107, 108)),
//      166 -> FunType(
//        168,
//        RecType(169, Bot),
//        RecType(
//          170,
//          AndType(
//            TypeDecl(171, Bot, TypeProj(49, 50)),
//            AndType(
//              FieldDecl(
//                172,
//                TypeDecl(
//                  173,
//                  Bot,
//                  FunType(
//                    174,
//                    FunType(175, FieldDecl(176, Top), Bot),
//                    TypeProj(130, 131)
//                  )
//                )
//              ),
//              AndType(
//                FieldDecl(177, TypeDecl(178, Bot, TypeProj(42, 43))),
//                AndType(
//                  FieldDecl(179, TypeProj(94, 95)),
//                  FieldDecl(180, Bot)
//                )
//              )
//            )
//          )
//        )
//      ),
//      161 -> TypeDecl(
//        162,
//        AndType(Bot, Bot),
//        AndType(Bot, FieldDecl(163, Top))
//      ),
//      49 -> TypeDecl(
//        50,
//        AndType(
//          AndType(
//            AndType(Bot, Bot),
//            FunType(
//              51,
//              Top,
//              TypeDecl(
//                52,
//                FieldDecl(53, TypeProj(7, 8)),
//                FieldDecl(53, TypeProj(7, 8))
//              )
//            )
//          ),
//          TypeProj(11, 12)
//        ),
//        Bot
//      ),
//      7 -> TypeDecl(
//        8,
//        Bot,
//        AndType(
//          FieldDecl(
//            9,
//            TypeDecl(
//              10,
//              AndType(
//                TypeProj(11, 12),
//                RecType(
//                  28,
//                  AndType(
//                    AndType(
//                      TypeDecl(
//                        29,
//                        AndType(
//                          AndType(
//                            Bot,
//                            FunType(31, TypeProj(11, 12), Bot)
//                          ),
//                          TypeDecl(32, Bot, Top)
//                        ),
//                        RecType(30, TypeProj(11, 12))
//                      ),
//                      FieldDecl(33, Top)
//                    ),
//                    FieldDecl(
//                      34,
//                      FunType(
//                        35,
//                        FunType(
//                          36,
//                          Top,
//                          TypeDecl(
//                            37,
//                            AndType(
//                              AndType(
//                                Bot,
//                                FunType(31, TypeProj(11, 12), Bot)
//                              ),
//                              TypeDecl(32, Bot, Top)
//                            ),
//                            TypeProj(28, 29)
//                          )
//                        ),
//                        AndType(
//                          FieldDecl(
//                            38,
//                            FunType(
//                              39,
//                              Bot,
//                              FunType(
//                                40,
//                                Top,
//                                FunType(
//                                  41,
//                                  TypeProj(42, 43),
//                                  TypeProj(11, 12)
//                                )
//                              )
//                            )
//                          ),
//                          TypeDecl(
//                            44,
//                            FunType(45, TypeProj(16, 17), Bot),
//                            FunType(45, TypeProj(16, 17), Bot)
//                          )
//                        )
//                      )
//                    )
//                  )
//                )
//              ),
//              TypeProj(11, 12)
//            )
//          ),
//          RecType(46, TypeProj(42, 43))
//        )
//      ),
//      130 -> TypeDecl(131, Bot, TypeDecl(129, Bot, Bot)),
//      3 -> TypeDecl(
//        4,
//        AndType(
//          AndType(
//            AndType(
//              Bot,
//              FunType(
//                54,
//                RecType(
//                  55,
//                  AndType(
//                    RecType(56, FieldDecl(57, TypeProj(7, 8))),
//                    FunType(
//                      58,
//                      FieldDecl(59, Bot),
//                      TypeDecl(60, Bot, Bot)
//                    )
//                  )
//                ),
//                FunType(
//                  61,
//                  Bot,
//                  TypeDecl(
//                    62,
//                    AndType(
//                      AndType(
//                        Bot,
//                        AndType(
//                          Bot,
//                          RecType(
//                            77,
//                            AndType(
//                              AndType(
//                                FieldDecl(78, Top),
//                                FieldDecl(79, TypeProj(75, 76))
//                              ),
//                              FieldDecl(80, TypeProj(16, 17))
//                            )
//                          )
//                        )
//                      ),
//                      FieldDecl(
//                        81,
//                        FieldDecl(
//                          82,
//                          FieldDecl(
//                            83,
//                            RecType(
//                              84,
//                              RecType(
//                                85,
//                                RecType(86, TypeProj(75, 76))
//                              )
//                            )
//                          )
//                        )
//                      )
//                    ),
//                    FunType(
//                      63,
//                      RecType(
//                        64,
//                        AndType(
//                          TypeDecl(
//                            65,
//                            TypeProj(11, 12),
//                            TypeProj(11, 12)
//                          ),
//                          AndType(
//                            AndType(
//                              FieldDecl(66, Top),
//                              FieldDecl(67, Top)
//                            ),
//                            AndType(
//                              FieldDecl(
//                                68,
//                                RecType(69, FieldDecl(70, Bot))
//                              ),
//                              FieldDecl(71, TypeProj(64, 65))
//                            )
//                          )
//                        )
//                      ),
//                      RecType(
//                        72,
//                        TypeDecl(
//                          73,
//                          TypeProj(75, 76),
//                          FieldDecl(74, Bot)
//                        )
//                      )
//                    )
//                  )
//                )
//              )
//            ),
//            Top
//          ),
//          Top
//        ),
//        AndType(
//          RecType(
//            5,
//            RecType(6, AndType(TypeProj(7, 8), TypeProj(42, 43)))
//          ),
//          FunType(
//            47,
//            Bot,
//            TypeDecl(48, TypeProj(49, 50), TypeProj(11, 12))
//          )
//        )
//      ),
//      167 -> RecType(169, Bot),
//      16 -> TypeDecl(17, RecType(18, Bot), RecType(18, Bot)),
//      11 -> TypeDecl(
//        12,
//        AndType(
//          AndType(
//            Bot,
//            FunType(25, Bot, RecType(26, TypeDecl(27, Top, Top)))
//          ),
//          TypeDecl(
//            19,
//            FunType(
//              20,
//              FunType(21, TypeDecl(22, Bot, Bot), Bot),
//              RecType(23, FunType(24, TypeProj(16, 17), Top))
//            ),
//            Top
//          )
//        ),
//        AndType(
//          FunType(
//            13,
//            Top,
//            FunType(
//              14,
//              AndType(RecType(15, Bot), Top),
//              AndType(Bot, TypeProj(16, 17))
//            )
//          ),
//          TypeDecl(
//            19,
//            FunType(
//              20,
//              FunType(21, TypeDecl(22, Bot, Bot), Bot),
//              RecType(23, FunType(24, TypeProj(16, 17), Top))
//            ),
//            Top
//          )
//        )
//      ),
//      75 -> TypeDecl(76, FieldDecl(74, Bot), FieldDecl(74, Bot)),
//      119 -> TypeDecl(120, Bot, Bot),
//      107 -> TypeDecl(
//        108,
//        AndType(TypeProj(109, 110), FieldDecl(111, Bot)),
//        AndType(AndType(Bot, Bot), TypeProj(49, 50))
//      ),
//      94 -> TypeDecl(
//        95,
//        TypeProj(161, 162),
//        FunType(
//          96,
//          Bot,
//          RecType(
//            97,
//            AndType(
//              TypeDecl(
//                98,
//                TypeProj(117, 118),
//                RecType(
//                  99,
//                  AndType(
//                    TypeDecl(
//                      100,
//                      TypeProj(105, 106),
//                      TypeDecl(
//                        101,
//                        AndType(Bot, Bot),
//                        RecType(
//                          102,
//                          FieldDecl(
//                            103,
//                            AndType(
//                              AndType(Top, Top),
//                              FunType(104, Bot, Top)
//                            )
//                          )
//                        )
//                      )
//                    ),
//                    FieldDecl(
//                      112,
//                      AndType(
//                        TypeDecl(113, Bot, Bot),
//                        AndType(
//                          FieldDecl(114, Bot),
//                          AndType(
//                            FieldDecl(115, AndType(Top, Top)),
//                            FieldDecl(116, Top)
//                          )
//                        )
//                      )
//                    )
//                  )
//                )
//              ),
//              AndType(
//                FieldDecl(
//                  122,
//                  AndType(
//                    AndType(
//                      AndType(
//                        FieldDecl(123, Bot),
//                        TypeDecl(
//                          124,
//                          Bot,
//                          TypeDecl(
//                            125,
//                            RecType(18, Bot),
//                            TypeProj(16, 17)
//                          )
//                        )
//                      ),
//                      FunType(
//                        126,
//                        RecType(
//                          127,
//                          AndType(
//                            TypeDecl(
//                              128,
//                              TypeProj(130, 131),
//                              TypeDecl(129, Bot, Bot)
//                            ),
//                            FieldDecl(
//                              132,
//                              TypeDecl(133, TypeProj(107, 108), Top)
//                            )
//                          )
//                        ),
//                        FieldDecl(
//                          134,
//                          FunType(
//                            135,
//                            TypeProj(117, 118),
//                            RecType(136, TypeProj(137, 138))
//                          )
//                        )
//                      )
//                    ),
//                    Bot
//                  )
//                ),
//                FieldDecl(
//                  139,
//                  AndType(
//                    AndType(
//                      AndType(
//                        FieldDecl(140, Bot),
//                        TypeDecl(
//                          141,
//                          AndType(
//                            AndType(
//                              TypeProj(142, 143),
//                              AndType(
//                                FunType(
//                                  144,
//                                  TypeProj(7, 8),
//                                  AndType(
//                                    TypeProj(119, 120),
//                                    FieldDecl(
//                                      145,
//                                      FieldDecl(
//                                        146,
//                                        TypeDecl(147, Bot, Bot)
//                                      )
//                                    )
//                                  )
//                                ),
//                                RecType(148, Top)
//                              )
//                            ),
//                            Bot
//                          ),
//                          Top
//                        )
//                      ),
//                      FieldDecl(
//                        149,
//                        AndType(
//                          FieldDecl(
//                            150,
//                            AndType(
//                              AndType(
//                                TypeProj(7, 8),
//                                FieldDecl(151, Bot)
//                              ),
//                              TypeDecl(
//                                152,
//                                AndType(Bot, Bot),
//                                RecType(153, TypeProj(42, 43))
//                              )
//                            )
//                          ),
//                          TypeDecl(154, Bot, RecType(155, Bot))
//                        )
//                      )
//                    ),
//                    AndType(
//                      TypeDecl(
//                        156,
//                        Bot,
//                        AndType(TypeProj(137, 138), Bot)
//                      ),
//                      FieldDecl(
//                        157,
//                        FieldDecl(
//                          158,
//                          FieldDecl(
//                            159,
//                            FieldDecl(160, TypeProj(137, 138))
//                          )
//                        )
//                      )
//                    )
//                  )
//                )
//              )
//            )
//          )
//        )
//      )
//    ),
//    181
//  ),
//  Let(
//    0,
//    Sel(1, 2),
//    Fun(
//      88,
//      FieldDecl(89, Bot),
//      Fun(
//        91,
//        TypeProj(16, 17),
//        Let(92, Let(93, Var(94), Sel(164, 165)), App(166, 167))
//      )
//    )
//  ),
//  Que,
//  TypedLet(
//    0,
//    TypedSel(1, 2) :- TypeProj(3, 4),
//    TypedFun(
//      88,
//      FieldDecl(89, Bot),
//      TypedFun(
//        91,
//        TypeProj(16, 17),
//        TypedLet(
//          92,
//          TypedLet(
//            93,
//            TypedVar(94) :- TypeDecl(
//              95,
//              TypeProj(161, 162),
//              FunType(
//                96,
//                Bot,
//                RecType(
//                  97,
//                  AndType(
//                    TypeDecl(
//                      98,
//                      TypeProj(117, 118),
//                      RecType(
//                        99,
//                        AndType(
//                          TypeDecl(
//                            100,
//                            TypeProj(105, 106),
//                            TypeDecl(
//                              101,
//                              AndType(Bot, Bot),
//                              RecType(
//                                102,
//                                FieldDecl(
//                                  103,
//                                  AndType(
//                                    AndType(Top, Top),
//                                    FunType(104, Bot, Top)
//                                  )
//                                )
//                              )
//                            )
//                          ),
//                          FieldDecl(
//                            112,
//                            AndType(
//                              TypeDecl(113, Bot, Bot),
//                              AndType(
//                                FieldDecl(114, Bot),
//                                AndType(
//                                  FieldDecl(115, AndType(Top, Top)),
//                                  FieldDecl(116, Top)
//                                )
//                              )
//                            )
//                          )
//                        )
//                      )
//                    ),
//                    AndType(
//                      FieldDecl(
//                        122,
//                        AndType(
//                          AndType(
//                            AndType(
//                              FieldDecl(123, Bot),
//                              TypeDecl(
//                                124,
//                                Bot,
//                                TypeDecl(
//                                  125,
//                                  RecType(18, Bot),
//                                  TypeProj(16, 17)
//                                )
//                              )
//                            ),
//                            FunType(
//                              126,
//                              RecType(
//                                127,
//                                AndType(
//                                  TypeDecl(
//                                    128,
//                                    TypeProj(130, 131),
//                                    TypeDecl(129, Bot, Bot)
//                                  ),
//                                  FieldDecl(
//                                    132,
//                                    TypeDecl(
//                                      133,
//                                      TypeProj(107, 108),
//                                      Top
//                                    )
//                                  )
//                                )
//                              ),
//                              FieldDecl(
//                                134,
//                                FunType(
//                                  135,
//                                  TypeProj(117, 118),
//                                  RecType(136, TypeProj(137, 138))
//                                )
//                              )
//                            )
//                          ),
//                          Bot
//                        )
//                      ),
//                      FieldDecl(
//                        139,
//                        AndType(
//                          AndType(
//                            AndType(
//                              FieldDecl(140, Bot),
//                              TypeDecl(
//                                141,
//                                AndType(
//                                  AndType(
//                                    TypeProj(142, 143),
//                                    AndType(
//                                      FunType(
//                                        144,
//                                        TypeProj(7, 8),
//                                        AndType(
//                                          TypeProj(119, 120),
//                                          FieldDecl(
//                                            145,
//                                            FieldDecl(
//                                              146,
//                                              TypeDecl(
//                                                147,
//                                                Bot,
//                                                Bot
//                                              )
//                                            )
//                                          )
//                                        )
//                                      ),
//                                      RecType(148, Top)
//                                    )
//                                  ),
//                                  Bot
//                                ),
//                                Top
//                              )
//                            ),
//                            FieldDecl(
//                              149,
//                              AndType(
//                                FieldDecl(
//                                  150,
//                                  AndType(
//                                    AndType(
//                                      TypeProj(7, 8),
//                                      FieldDecl(151, Bot)
//                                    ),
//                                    TypeDecl(
//                                      152,
//                                      AndType(Bot, Bot),
//                                      RecType(153, TypeProj(42, 43))
//                                    )
//                                  )
//                                ),
//                                TypeDecl(
//                                  154,
//                                  Bot,
//                                  RecType(155, Bot)
//                                )
//                              )
//                            )
//                          ),
//                          AndType(
//                            TypeDecl(
//                              156,
//                              Bot,
//                              AndType(TypeProj(137, 138), Bot)
//                            ),
//                            FieldDecl(
//                              157,
//                              FieldDecl(
//                                158,
//                                FieldDecl(
//                                  159,
//                                  FieldDecl(160, TypeProj(137, 138))
//                                )
//                              )
//                            )
//                          )
//                        )
//                      )
//                    )
//                  )
//                )
//              )
//            ),
//            TypedSel(164, 165) :- Top
//          ) :- Top,
//          TypedApp(166, 167) :- RecType(
//            170,
//            AndType(
//              TypeDecl(171, Bot, TypeProj(49, 50)),
//              AndType(
//                FieldDecl(
//                  172,
//                  TypeDecl(
//                    173,
//                    Bot,
//                    FunType(
//                      174,
//                      FunType(175, FieldDecl(176, Top), Bot),
//                      TypeProj(130, 131)
//                    )
//                  )
//                ),
//                AndType(
//                  FieldDecl(
//                    177,
//                    TypeDecl(178, Bot, TypeProj(42, 43))
//                  ),
//                  AndType(
//                    FieldDecl(179, TypeProj(94, 95)),
//                    FieldDecl(180, Bot)
//                  )
//                )
//              )
//            )
//          )
//        ) :- RecType(
//          170,
//          AndType(
//            TypeDecl(171, Bot, TypeProj(49, 50)),
//            AndType(
//              FieldDecl(
//                172,
//                TypeDecl(
//                  173,
//                  Bot,
//                  FunType(
//                    174,
//                    FunType(175, FieldDecl(176, Top), Bot),
//                    TypeProj(130, 131)
//                  )
//                )
//              ),
//              AndType(
//                FieldDecl(177, TypeDecl(178, Bot, TypeProj(42, 43))),
//                AndType(
//                  FieldDecl(179, TypeProj(94, 95)),
//                  FieldDecl(180, Bot)
//                )
//              )
//            )
//          )
//        )
//      ) :- FunType(
//        91,
//        TypeProj(16, 17),
//        RecType(
//          170,
//          AndType(
//            TypeDecl(171, Bot, TypeProj(49, 50)),
//            AndType(
//              FieldDecl(
//                172,
//                TypeDecl(
//                  173,
//                  Bot,
//                  FunType(
//                    174,
//                    FunType(175, FieldDecl(176, Top), Bot),
//                    TypeProj(130, 131)
//                  )
//                )
//              ),
//              AndType(
//                FieldDecl(177, TypeDecl(178, Bot, TypeProj(42, 43))),
//                AndType(
//                  FieldDecl(179, TypeProj(94, 95)),
//                  FieldDecl(180, Bot)
//                )
//              )
//            )
//          )
//        )
//      )
//    ) :- FunType(
//      88,
//      FieldDecl(89, Bot),
//      FunType(
//        91,
//        TypeProj(16, 17),
//        RecType(
//          170,
//          AndType(
//            TypeDecl(171, Bot, TypeProj(49, 50)),
//            AndType(
//              FieldDecl(
//                172,
//                TypeDecl(
//                  173,
//                  Bot,
//                  FunType(
//                    174,
//                    FunType(175, FieldDecl(176, Top), Bot),
//                    TypeProj(130, 131)
//                  )
//                )
//              ),
//              AndType(
//                FieldDecl(177, TypeDecl(178, Bot, TypeProj(42, 43))),
//                AndType(
//                  FieldDecl(179, TypeProj(94, 95)),
//                  FieldDecl(180, Bot)
//                )
//              )
//            )
//          )
//        )
//      )
//    )
//  ) :- FunType(
//    88,
//    FieldDecl(89, Bot),
//    FunType(
//      91,
//      TypeProj(16, 17),
//      RecType(
//        170,
//        AndType(
//          TypeDecl(171, Bot, TypeProj(49, 50)),
//          AndType(
//            FieldDecl(
//              172,
//              TypeDecl(
//                173,
//                Bot,
//                FunType(
//                  174,
//                  FunType(175, FieldDecl(176, Top), Bot),
//                  TypeProj(130, 131)
//                )
//              )
//            ),
//            AndType(
//              FieldDecl(177, TypeDecl(178, Bot, TypeProj(42, 43))),
//              AndType(
//                FieldDecl(179, TypeProj(94, 95)),
//                FieldDecl(180, Bot)
//              )
//            )
//          )
//        )
//      )
//    )
//  )
//)

//InferenceProblem.assemble(GlobalContext(Map(5 -> TypeDecl(6,TypeProj(7,8),Bot), 120 -> RecType(121,AndType(AndType(TypeDecl(122,Bot,Bot),TypeDecl(123,RecType(124,Top),RecType(124,Top))),FieldDecl(125,RecType(126,TypeDecl(127,TypeProj(137,138),AndType(TypeProj(107,108),TypeDecl(128,AndType(AndType(AndType(FieldDecl(129,FieldDecl(130,Top)),TypeDecl(131,Bot,Bot)),TypeProj(7,8)),RecType(132,AndType(FieldDecl(133,TypeProj(33,34)),FieldDecl(134,AndType(TypeDecl(135,Bot,Top),FieldDecl(136,TypeProj(7,8))))))),AndType(AndType(AndType(FieldDecl(129,FieldDecl(130,Top)),TypeDecl(131,Bot,Bot)),TypeProj(7,8)),RecType(132,AndType(FieldDecl(133,TypeProj(33,34)),FieldDecl(134,AndType(TypeDecl(135,Bot,Top),FieldDecl(136,TypeProj(7,8)))))))))))))), 247 -> Top, 174 -> FieldDecl(175,TypeProj(7,8)), 14 -> TypeDecl(15,Bot,FieldDecl(16,FunType(17,RecType(18,Bot),Top))), 184 -> TypeDecl(185,AndType(FieldDecl(186,TypeDecl(187,Bot,AndType(Bot,Bot))),RecType(188,Bot)),FieldDecl(186,TypeDecl(187,Bot,AndType(Bot,Bot)))), 20 -> TypeDecl(21,Bot,FieldDecl(22,Bot)), 84 -> TypeDecl(85,RecType(72,AndType(FieldDecl(73,RecType(74,Bot)),FieldDecl(75,RecType(76,AndType(FunType(77,RecType(78,Bot),RecType(79,Bot)),FieldDecl(80,FieldDecl(81,FunType(82,Top,RecType(83,Top))))))))),RecType(72,AndType(FieldDecl(73,RecType(74,Bot)),FieldDecl(75,RecType(76,AndType(FunType(77,RecType(78,Bot),RecType(79,Bot)),FieldDecl(80,FieldDecl(81,FunType(82,Top,RecType(83,Top)))))))))), 89 -> TypeDecl(90,Bot,Top), 243 -> Top, 60 -> FieldDecl(61,RecType(62,FieldDecl(63,RecType(64,AndType(TypeDecl(65,TypeProj(107,108),FunType(66,AndType(AndType(FieldDecl(67,Bot),RecType(68,FieldDecl(69,TypeProj(70,71)))),FieldDecl(86,FunType(87,TypeDecl(88,TypeProj(89,90),Top),FieldDecl(91,Top)))),AndType(TypeDecl(92,AndType(Bot,Bot),FunType(93,FunType(94,RecType(95,AndType(FieldDecl(96,Top),FieldDecl(97,TypeDecl(98,Bot,AndType(TypeProj(20,21),Top))))),Top),Bot)),TypeDecl(99,AndType(TypeProj(100,101),AndType(FieldDecl(104,TypeDecl(105,Bot,Bot)),FieldDecl(106,TypeProj(70,71)))),TypeProj(47,48))))),AndType(AndType(FieldDecl(109,Bot),FieldDecl(110,FunType(111,Bot,AndType(FieldDecl(112,Top),AndType(FieldDecl(113,Top),FieldDecl(114,Top)))))),FieldDecl(115,RecType(116,Top)))))))), 117 -> FieldDecl(118,Bot), 102 -> TypeDecl(103,Bot,TypeProj(47,48)), 70 -> TypeDecl(71,TypeProj(84,85),RecType(72,AndType(FieldDecl(73,RecType(74,Bot)),FieldDecl(75,RecType(76,AndType(FunType(77,RecType(78,Bot),RecType(79,Bot)),FieldDecl(80,FieldDecl(81,FunType(82,Top,RecType(83,Top)))))))))), 137 -> TypeDecl(138,AndType(Bot,AndType(Bot,FunType(154,TypeProj(100,101),AndType(AndType(FieldDecl(155,RecType(156,TypeProj(84,85))),AndType(FieldDecl(157,TypeProj(141,142)),AndType(FieldDecl(158,Top),TypeDecl(159,Bot,Bot)))),AndType(TypeDecl(160,Top,Top),AndType(FieldDecl(161,Bot),FieldDecl(162,RecType(163,TypeProj(5,6))))))))),AndType(TypeProj(139,140),TypeDecl(128,AndType(AndType(AndType(FieldDecl(129,FieldDecl(130,Top)),TypeDecl(131,Bot,Bot)),TypeProj(7,8)),RecType(132,AndType(FieldDecl(133,TypeProj(33,34)),FieldDecl(134,AndType(TypeDecl(135,Bot,Top),FieldDecl(136,TypeProj(7,8))))))),AndType(AndType(AndType(FieldDecl(129,FieldDecl(130,Top)),TypeDecl(131,Bot,Bot)),TypeProj(7,8)),RecType(132,AndType(FieldDecl(133,TypeProj(33,34)),FieldDecl(134,AndType(TypeDecl(135,Bot,Top),FieldDecl(136,TypeProj(7,8)))))))))), 33 -> TypeDecl(34,AndType(Bot,AndType(Top,TypeProj(20,21))),Bot), 141 -> TypeDecl(142,AndType(AndType(Bot,Bot),RecType(143,Bot)),TypeProj(107,108)), 225 -> TypeDecl(226,AndType(TypeProj(227,228),Top),TypeProj(227,228)), 193 -> TypeDecl(194,AndType(Bot,AndType(TypeProj(146,147),TypeProj(20,21))),AndType(Bot,Top)), 2 -> FunType(4,TypeProj(5,6),FunType(51,Top,FunType(52,FunType(53,Top,AndType(Top,FieldDecl(54,RecType(55,FieldDecl(56,Bot))))),FieldDecl(57,FunType(58,Top,TypeProj(7,8)))))), 148 -> TypeDecl(149,AndType(Bot,Top),AndType(Bot,Top)), 191 -> TypeDecl(192,AndType(TypeProj(193,194),TypeProj(195,196)),AndType(TypeProj(193,194),TypeProj(195,196))), 144 -> TypeDecl(145,AndType(AndType(TypeProj(146,147),Bot),FunType(150,Top,AndType(TypeProj(47,48),TypeDecl(151,Bot,AndType(FunType(152,Top,Top),RecType(153,TypeProj(20,21))))))),AndType(Bot,Bot)), 7 -> TypeDecl(8,AndType(Bot,AndType(FieldDecl(9,Bot),AndType(TypeDecl(10,AndType(Bot,Bot),TypeProj(11,12)),AndType(FieldDecl(35,FunType(36,TypeDecl(37,AndType(Top,RecType(38,RecType(39,AndType(FieldDecl(40,Bot),AndType(FieldDecl(41,TypeProj(20,21)),FieldDecl(42,TypeDecl(43,Bot,Top))))))),Top),Bot)),FieldDecl(44,RecType(45,AndType(TypeDecl(46,Bot,TypeProj(47,48)),FieldDecl(50,Bot)))))))),Bot), 240 -> TypeDecl(241,Bot,Bot), 3 -> TypeProj(5,6), 11 -> TypeDecl(12,TypeProj(33,34),TypeDecl(13,AndType(AndType(AndType(Bot,Bot),RecType(27,Top)),FieldDecl(28,RecType(29,FunType(30,TypeDecl(31,Top,Top),TypeDecl(32,Bot,Top))))),AndType(AndType(TypeProj(14,15),AndType(FunType(19,TypeProj(20,21),Top),AndType(RecType(23,RecType(24,FunType(25,Top,RecType(26,Top)))),Bot))),Top))), 139 -> TypeDecl(140,TypeProj(144,145),TypeProj(141,142)), 146 -> TypeDecl(147,TypeProj(148,149),Bot), 107 -> TypeDecl(108,Bot,Bot), 242 -> FunType(244,Top,FunType(245,TypeProj(20,21),RecType(246,Top))), 195 -> TypeDecl(196,AndType(Bot,RecType(205,AndType(TypeDecl(206,AndType(Bot,Top),Bot),FieldDecl(207,AndType(Bot,RecType(208,FieldDecl(209,TypeProj(137,138)))))))),FieldDecl(197,AndType(AndType(FieldDecl(198,Bot),TypeDecl(199,Bot,TypeDecl(200,Bot,Top))),AndType(FieldDecl(201,AndType(FieldDecl(202,Top),FieldDecl(203,Top))),TypeDecl(204,Top,Top))))), 47 -> TypeDecl(48,Bot,FieldDecl(49,Bot)), 254 -> AndType(AndType(Bot,Bot),TypeProj(102,103)), 227 -> TypeDecl(228,Bot,Bot), 100 -> TypeDecl(101,Bot,TypeProj(102,103))),255),:?(:%(ILet(0,:?(:%(ILet(1,:?(:%(IApp(2,3),Que),FunType(51,Top,FunType(52,FunType(53,Top,AndType(Top,FieldDecl(54,RecType(55,FieldDecl(56,Bot))))),FieldDecl(57,FunType(58,Top,TypeProj(7,8)))))),:?(:%(ILet(59,:?(:%(IVar(60),Que),FieldDecl(61,RecType(62,FieldDecl(63,RecType(64,AndType(TypeDecl(65,TypeProj(107,108),FunType(66,AndType(AndType(FieldDecl(67,Bot),RecType(68,FieldDecl(69,TypeProj(70,71)))),FieldDecl(86,FunType(87,TypeDecl(88,TypeProj(89,90),Top),FieldDecl(91,Top)))),AndType(TypeDecl(92,AndType(Bot,Bot),FunType(93,FunType(94,RecType(95,AndType(FieldDecl(96,Top),FieldDecl(97,TypeDecl(98,Bot,AndType(TypeProj(20,21),Top))))),Top),Bot)),TypeDecl(99,AndType(TypeProj(100,101),AndType(FieldDecl(104,TypeDecl(105,Bot,Bot)),FieldDecl(106,TypeProj(70,71)))),TypeProj(47,48))))),AndType(AndType(FieldDecl(109,Bot),FieldDecl(110,FunType(111,Bot,AndType(FieldDecl(112,Top),AndType(FieldDecl(113,Top),FieldDecl(114,Top)))))),FieldDecl(115,RecType(116,Top))))))))),:?(:%(ISel(117,118),Que),Bot)),Que),Bot)),Que),Bot),:?(:%(ILet(119,:?(:%(IVar(120),Que),RecType(121,AndType(AndType(TypeDecl(122,Bot,Bot),TypeDecl(123,RecType(124,Top),RecType(124,Top))),FieldDecl(125,RecType(126,TypeDecl(127,TypeProj(137,138),AndType(TypeProj(107,108),TypeDecl(128,AndType(AndType(AndType(FieldDecl(129,FieldDecl(130,Top)),TypeDecl(131,Bot,Bot)),TypeProj(7,8)),RecType(132,AndType(FieldDecl(133,TypeProj(33,34)),FieldDecl(134,AndType(TypeDecl(135,Bot,Top),FieldDecl(136,TypeProj(7,8))))))),AndType(AndType(AndType(FieldDecl(129,FieldDecl(130,Top)),TypeDecl(131,Bot,Bot)),TypeProj(7,8)),RecType(132,AndType(FieldDecl(133,TypeProj(33,34)),FieldDecl(134,AndType(TypeDecl(135,Bot,Top),FieldDecl(136,TypeProj(7,8))))))))))))))),:?(:%(ILet(164,:?(:%(IFun(166,AndType(TypeProj(146,147),FunType(167,FunType(168,FunType(169,FieldDecl(170,FunType(171,Top,Top)),Top),TypeProj(84,85)),RecType(172,AndType(AndType(FieldDecl(173,Top),Top),Top)))),:?(:%(ISel(174,175),Que),TypeProj(7,8))),Que),FunType(166,AndType(TypeProj(146,147),FunType(167,FunType(168,FunType(169,FieldDecl(170,FunType(171,Top,Top)),Top),TypeProj(84,85)),RecType(172,AndType(AndType(FieldDecl(173,Top),Top),Top)))),TypeProj(7,8))),:?(:%(IFun(177,AndType(Bot,Top),:?(:%(IFun(179,FunType(180,RecType(181,AndType(AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(TypeDecl(210,AndType(Bot,TypeDecl(216,Top,Top)),RecType(211,AndType(AndType(FieldDecl(212,Bot),FieldDecl(213,TypeProj(70,71))),FieldDecl(214,FieldDecl(215,Bot))))),TypeDecl(217,FieldDecl(218,Top),Top))),FieldDecl(219,Bot)))),FieldDecl(220,RecType(221,AndType(FieldDecl(222,Bot),FieldDecl(223,AndType(TypeDecl(224,TypeProj(225,226),Bot),FieldDecl(229,Bot)))))))),RecType(230,Top)),:?(:%(ILet(231,:?(:%(ILet(232,:?(:%(IFun(234,FunType(235,Top,TypeDecl(236,FunType(237,RecType(238,TypeProj(146,147)),TypeDecl(239,TypeProj(240,241),Bot)),FunType(237,RecType(238,TypeProj(146,147)),TypeDecl(239,TypeProj(240,241),Bot)))),:?(:%(IApp(242,243),Que),FunType(245,TypeProj(20,21),RecType(246,Top)))),Que),FunType(234,FunType(235,Top,TypeDecl(236,FunType(237,RecType(238,TypeProj(146,147)),TypeDecl(239,TypeProj(240,241),Bot)),FunType(237,RecType(238,TypeProj(146,147)),TypeDecl(239,TypeProj(240,241),Bot)))),FunType(245,TypeProj(20,21),RecType(246,Top)))),:?(:%(IVar(247),Que),Top)),Que),Top),:?(:%(IFun(249,FunType(250,AndType(Top,AndType(FunType(251,Top,TypeProj(33,34)),FunType(252,Top,FieldDecl(253,TypeProj(120,122))))),Bot),:?(:%(IVar(254),Que),AndType(AndType(Bot,Bot),TypeProj(102,103)))),Que),FunType(249,FunType(250,AndType(Top,AndType(FunType(251,Top,TypeProj(33,34)),FunType(252,Top,FieldDecl(253,TypeProj(120,122))))),Bot),AndType(AndType(Bot,Bot),TypeProj(102,103))))),Que),FunType(249,FunType(250,AndType(Top,AndType(FunType(251,Top,TypeProj(33,34)),FunType(252,Top,FieldDecl(253,TypeProj(120,122))))),Bot),AndType(AndType(Bot,Bot),TypeProj(102,103))))),Que),FunType(179,FunType(180,RecType(181,AndType(AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(TypeDecl(210,AndType(Bot,TypeDecl(216,Top,Top)),RecType(211,AndType(AndType(FieldDecl(212,Bot),FieldDecl(213,TypeProj(70,71))),FieldDecl(214,FieldDecl(215,Bot))))),TypeDecl(217,FieldDecl(218,Top),Top))),FieldDecl(219,Bot)))),FieldDecl(220,RecType(221,AndType(FieldDecl(222,Bot),FieldDecl(223,AndType(TypeDecl(224,TypeProj(225,226),Bot),FieldDecl(229,Bot)))))))),RecType(230,Top)),FunType(249,FunType(250,AndType(Top,AndType(FunType(251,Top,TypeProj(33,34)),FunType(252,Top,FieldDecl(253,TypeProj(120,122))))),Bot),AndType(AndType(Bot,Bot),TypeProj(102,103)))))),Que),FunType(177,AndType(Bot,Top),FunType(179,FunType(180,RecType(181,AndType(AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(TypeDecl(210,AndType(Bot,TypeDecl(216,Top,Top)),RecType(211,AndType(AndType(FieldDecl(212,Bot),FieldDecl(213,TypeProj(70,71))),FieldDecl(214,FieldDecl(215,Bot))))),TypeDecl(217,FieldDecl(218,Top),Top))),FieldDecl(219,Bot)))),FieldDecl(220,RecType(221,AndType(FieldDecl(222,Bot),FieldDecl(223,AndType(TypeDecl(224,TypeProj(225,226),Bot),FieldDecl(229,Bot)))))))),RecType(230,Top)),FunType(249,FunType(250,AndType(Top,AndType(FunType(251,Top,TypeProj(33,34)),FunType(252,Top,FieldDecl(253,TypeProj(120,122))))),Bot),AndType(AndType(Bot,Bot),TypeProj(102,103))))))),Que),FunType(177,AndType(Bot,Top),FunType(179,FunType(180,RecType(181,AndType(AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(TypeDecl(210,AndType(Bot,TypeDecl(216,Top,Top)),RecType(211,AndType(AndType(FieldDecl(212,Bot),FieldDecl(213,TypeProj(70,71))),FieldDecl(214,FieldDecl(215,Bot))))),TypeDecl(217,FieldDecl(218,Top),Top))),FieldDecl(219,Bot)))),FieldDecl(220,RecType(221,AndType(FieldDecl(222,Bot),FieldDecl(223,AndType(TypeDecl(224,TypeProj(225,226),Bot),FieldDecl(229,Bot)))))))),RecType(230,Top)),FunType(249,FunType(250,AndType(Top,AndType(FunType(251,Top,TypeProj(33,34)),FunType(252,Top,FieldDecl(253,TypeProj(120,122))))),Bot),AndType(AndType(Bot,Bot),TypeProj(102,103))))))),Que),FunType(177,AndType(Bot,Top),FunType(179,FunType(180,RecType(181,AndType(AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(TypeDecl(210,AndType(Bot,TypeDecl(216,Top,Top)),RecType(211,AndType(AndType(FieldDecl(212,Bot),FieldDecl(213,TypeProj(70,71))),FieldDecl(214,FieldDecl(215,Bot))))),TypeDecl(217,FieldDecl(218,Top),Top))),FieldDecl(219,Bot)))),FieldDecl(220,RecType(221,AndType(FieldDecl(222,Bot),FieldDecl(223,AndType(TypeDecl(224,TypeProj(225,226),Bot),FieldDecl(229,Bot)))))))),RecType(230,Top)),FunType(249,FunType(250,AndType(Top,AndType(FunType(251,Top,TypeProj(33,34)),FunType(252,Top,FieldDecl(253,TypeProj(120,122))))),Bot),AndType(AndType(Bot,Bot),TypeProj(102,103))))))),Que),FunType(177,AndType(Bot,Top),FunType(179,FunType(180,RecType(181,AndType(AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(AndType(AndType(AndType(TypeDecl(182,Bot,Top),TypeDecl(183,TypeProj(184,185),Top)),FieldDecl(189,TypeDecl(190,TypeProj(191,192),TypeProj(20,21)))),AndType(TypeDecl(210,AndType(Bot,TypeDecl(216,Top,Top)),RecType(211,AndType(AndType(FieldDecl(212,Bot),FieldDecl(213,TypeProj(70,71))),FieldDecl(214,FieldDecl(215,Bot))))),TypeDecl(217,FieldDecl(218,Top),Top))),FieldDecl(219,Bot)))),FieldDecl(220,RecType(221,AndType(FieldDecl(222,Bot),FieldDecl(223,AndType(TypeDecl(224,TypeProj(225,226),Bot),FieldDecl(229,Bot)))))))),RecType(230,Top)),FunType(249,FunType(250,AndType(Top,AndType(FunType(251,Top,TypeProj(33,34)),FunType(252,Top,FieldDecl(253,TypeProj(120,122))))),Bot),AndType(AndType(Bot,Bot),TypeProj(102,103)))))))


InferenceProblem(
  GlobalContext(
    Map(0 -> Top),
    181
  ),
  Obj(1, Top, meh4(0, 16)),
  Que,
  TypedVar(0) :- Bot
)


//InferenceProblem.assemble(GlobalContext(Map(42 -> TypeDecl(43,Bot,Top), 125 -> TypeDecl(126,TypeProj(128,129),AndType(TypeProj(123,124),RecType(127,Top))), 196 -> TypeDecl(197,Bot,TypeProj(198,199)), 189 -> TypeDecl(190,Bot,Bot), 216 -> AndType(RecType(218,AndType(AndType(FieldDecl(219,TypeProj(119,120)),TypeDecl(220,Bot,TypeDecl(221,Bot,Bot))),AndType(FieldDecl(222,AndType(AndType(AndType(TypeDecl(223,TypeProj(224,225),Bot),FieldDecl(228,TypeProj(189,190))),TypeDecl(229,TypeProj(238,239),RecType(230,AndType(TypeDecl(231,Bot,RecType(232,AndType(TypeProj(125,126),Top))),FieldDecl(233,TypeDecl(234,TypeProj(235,236),Top)))))),FieldDecl(240,AndType(FunType(241,AndType(AndType(RecType(242,TypeDecl(243,TypeProj(98,99),TypeProj(98,99))),FunType(244,TypeProj(114,115),RecType(245,RecType(246,AndType(Bot,AndType(FieldDecl(247,Top),Top)))))),RecType(248,AndType(Top,Bot))),TypeProj(238,239)),Bot)))),AndType(AndType(TypeDecl(249,Bot,TypeDecl(250,RecType(251,TypeProj(40,41)),RecType(251,TypeProj(40,41)))),AndType(TypeDecl(252,AndType(TypeProj(253,254),AndType(AndType(TypeDecl(257,AndType(Bot,Bot),RecType(258,FunType(259,Top,FieldDecl(260,Top)))),AndType(AndType(TypeDecl(261,TypeProj(264,265),FunType(262,Top,RecType(263,TypeProj(235,236)))),FieldDecl(268,Top)),AndType(FieldDecl(269,TypeDecl(270,AndType(Bot,Bot),TypeProj(255,256))),AndType(FieldDecl(271,AndType(RecType(272,FieldDecl(273,Bot)),Bot)),FieldDecl(274,TypeProj(238,239)))))),FieldDecl(275,FieldDecl(276,RecType(277,FieldDecl(278,RecType(279,AndType(FieldDecl(280,Top),AndType(FieldDecl(281,Top),FieldDecl(282,FieldDecl(283,Bot))))))))))),Top),FieldDecl(284,Top))),FieldDecl(285,RecType(286,Top)))))),TypeProj(128,129)), 253 -> TypeDecl(254,TypeProj(255,256),TypeProj(255,256)), 238 -> TypeDecl(239,RecType(230,AndType(TypeDecl(231,Bot,RecType(232,AndType(TypeProj(125,126),Top))),FieldDecl(233,TypeDecl(234,TypeProj(235,236),Top)))),RecType(230,AndType(TypeDecl(231,Bot,RecType(232,AndType(TypeProj(125,126),Top))),FieldDecl(233,TypeDecl(234,TypeProj(235,236),Top))))), 121 -> TypeDecl(122,TypeProj(123,124),Bot), 293 -> FieldDecl(294,Bot), 116 -> TypeDecl(117,AndType(TypeProj(119,120),Bot),TypeDecl(118,Bot,TypeProj(75,76))), 1 -> Bot, 312 -> TypeDecl(313,TypeProj(314,315),Top), 381 -> TypeProj(238,239), 224 -> TypeDecl(225,AndType(Bot,RecType(226,RecType(227,TypeProj(116,117)))),Bot), 96 -> TypeDecl(97,TypeProj(98,99),Bot), 73 -> TypeDecl(74,AndType(Bot,Top),TypeProj(75,76)), 128 -> TypeDecl(129,Bot,Bot), 266 -> TypeDecl(267,Bot,Bot), 166 -> TypeDecl(167,AndType(TypeProj(168,169),FunType(170,RecType(171,FieldDecl(172,FieldDecl(173,RecType(174,Top)))),TypeProj(73,74))),TypeProj(168,169)), 264 -> TypeDecl(265,TypeProj(266,267),Bot), 296 -> FieldDecl(297,Top), 71 -> TypeDecl(72,TypeProj(73,74),Top), 382 -> FunType(384,Top,Bot), 98 -> TypeDecl(99,Bot,Bot), 198 -> TypeDecl(199,Bot,Bot), 306 -> TypeDecl(307,Bot,RecType(305,Bot)), 299 -> FieldDecl(300,FunType(301,TypeProj(123,124),RecType(302,AndType(AndType(AndType(AndType(FieldDecl(303,AndType(TypeDecl(304,TypeProj(306,307),RecType(305,Bot)),FieldDecl(308,FunType(309,FunType(310,AndType(Bot,Bot),Top),AndType(TypeDecl(311,AndType(TypeProj(312,313),RecType(316,TypeProj(224,225))),Top),Top))))),AndType(FieldDecl(303,AndType(TypeDecl(304,TypeProj(306,307),RecType(305,Bot)),FieldDecl(308,FunType(309,FunType(310,AndType(Bot,Bot),Top),AndType(TypeDecl(311,AndType(TypeProj(312,313),RecType(316,TypeProj(224,225))),Top),Top))))),AndType(AndType(FieldDecl(303,AndType(TypeDecl(304,TypeProj(306,307),RecType(305,Bot)),FieldDecl(308,FunType(309,FunType(310,AndType(Bot,Bot),Top),AndType(TypeDecl(311,AndType(TypeProj(312,313),RecType(316,TypeProj(224,225))),Top),Top))))),AndType(TypeDecl(317,Bot,Bot),TypeDecl(318,FieldDecl(319,Bot),FieldDecl(319,RecType(320,FunType(321,RecType(322,Bot),TypeProj(116,117))))))),FieldDecl(323,TypeDecl(324,AndType(Bot,TypeDecl(339,AndType(Bot,RecType(340,Bot)),Bot)),FunType(325,FunType(326,TypeProj(73,74),FieldDecl(327,RecType(328,RecType(329,RecType(330,AndType(TypeDecl(331,AndType(Bot,RecType(332,Top)),TypeProj(168,169)),AndType(AndType(FieldDecl(333,FieldDecl(334,TypeDecl(335,Bot,Bot))),FieldDecl(336,Top)),AndType(FieldDecl(337,Bot),FieldDecl(338,Top))))))))),Top)))))),FieldDecl(341,FunType(342,Top,TypeProj(216,252)))),FieldDecl(343,RecType(344,AndType(TypeDecl(345,RecType(346,Top),RecType(346,Top)),AndType(FieldDecl(347,Top),AndType(FieldDecl(348,TypeProj(224,225)),FieldDecl(349,Top))))))),FieldDecl(350,FunType(351,RecType(352,TypeDecl(353,Bot,FunType(354,Bot,AndType(TypeDecl(355,AndType(Bot,Top),RecType(356,AndType(AndType(FieldDecl(357,AndType(Top,AndType(AndType(Top,FieldDecl(358,Top)),AndType(Top,Top)))),FieldDecl(359,FunType(360,TypeDecl(361,AndType(Bot,TypeProj(264,265)),TypeProj(73,74)),TypeProj(96,97)))),FieldDecl(362,Top)))),FieldDecl(363,RecType(364,AndType(TypeDecl(365,FunType(366,Top,Bot),FunType(366,Top,Bot)),FieldDecl(367,TypeDecl(368,FieldDecl(369,Bot),FieldDecl(369,Bot)))))))))),FieldDecl(370,FunType(371,FunType(372,TypeDecl(373,TypeProj(96,97),TypeProj(96,97)),AndType(RecType(374,FunType(375,TypeProj(119,120),TypeProj(264,265))),FieldDecl(376,TypeDecl(377,Bot,FunType(378,AndType(TypeProj(235,236),RecType(379,Top)),FieldDecl(380,TypeProj(168,169))))))),TypeProj(302,317))))))))), 255 -> TypeDecl(256,Bot,Bot), 123 -> TypeDecl(124,Bot,Bot), 314 -> TypeDecl(315,Top,Top), 40 -> TypeDecl(41,Bot,TypeProj(42,43)), 114 -> TypeDecl(115,Bot,AndType(Top,Bot)), 75 -> TypeDecl(76,Bot,Bot), 290 -> FieldDecl(291,RecType(292,Bot)), 119 -> TypeDecl(120,TypeProj(125,126),TypeProj(121,122)), 235 -> TypeDecl(236,RecType(237,TypeProj(128,129)),Top), 383 -> Top, 168 -> TypeDecl(169,Bot,Bot), 4 -> TypeDecl(5,AndType(Bot,Bot),Top), 215 -> FunType(217,AndType(RecType(218,AndType(AndType(FieldDecl(219,TypeProj(119,120)),TypeDecl(220,Bot,TypeDecl(221,Bot,Bot))),AndType(FieldDecl(222,AndType(AndType(AndType(TypeDecl(223,TypeProj(224,225),Bot),FieldDecl(228,TypeProj(189,190))),TypeDecl(229,TypeProj(238,239),RecType(230,AndType(TypeDecl(231,Bot,RecType(232,AndType(TypeProj(125,126),Top))),FieldDecl(233,TypeDecl(234,TypeProj(235,236),Top)))))),FieldDecl(240,AndType(FunType(241,AndType(AndType(RecType(242,TypeDecl(243,TypeProj(98,99),TypeProj(98,99))),FunType(244,TypeProj(114,115),RecType(245,RecType(246,AndType(Bot,AndType(FieldDecl(247,Top),Top)))))),RecType(248,AndType(Top,Bot))),TypeProj(238,239)),Bot)))),AndType(AndType(TypeDecl(249,Bot,TypeDecl(250,RecType(251,TypeProj(40,41)),RecType(251,TypeProj(40,41)))),AndType(TypeDecl(252,AndType(TypeProj(253,254),AndType(AndType(TypeDecl(257,AndType(Bot,Bot),RecType(258,FunType(259,Top,FieldDecl(260,Top)))),AndType(AndType(TypeDecl(261,TypeProj(264,265),FunType(262,Top,RecType(263,TypeProj(235,236)))),FieldDecl(268,Top)),AndType(FieldDecl(269,TypeDecl(270,AndType(Bot,Bot),TypeProj(255,256))),AndType(FieldDecl(271,AndType(RecType(272,FieldDecl(273,Bot)),Bot)),FieldDecl(274,TypeProj(238,239)))))),FieldDecl(275,FieldDecl(276,RecType(277,FieldDecl(278,RecType(279,AndType(FieldDecl(280,Top),AndType(FieldDecl(281,Top),FieldDecl(282,FieldDecl(283,Bot))))))))))),Top),FieldDecl(284,Top))),FieldDecl(285,RecType(286,Top)))))),TypeProj(128,129)),Bot)),385),:?(:%(ILet(0,:?(:%(IVar(1),Que),Bot),:?(:%(IFun(3,AndType(TypeProj(4,5),RecType(6,TypeDecl(7,Top,Top))),:?(:%(ILet(8,:?(:%(ILet(9,:?(:%(ILet(10,:?(:%(IFun(12,Top,:?(:%(IFun(14,FunType(15,FunType(16,Top,RecType(17,FunType(18,AndType(Bot,FieldDecl(19,RecType(20,AndType(AndType(TypeDecl(21,Bot,Top),FieldDecl(22,Top)),AndType(FieldDecl(23,RecType(24,AndType(AndType(TypeDecl(25,AndType(FieldDecl(26,RecType(27,AndType(AndType(TypeDecl(28,TypeProj(20,21),TypeProj(20,21)),FieldDecl(29,FieldDecl(30,Bot))),FieldDecl(31,Top)))),TypeDecl(32,AndType(Bot,Bot),AndType(Bot,Bot))),FieldDecl(26,RecType(27,AndType(AndType(TypeDecl(28,TypeProj(20,21),TypeProj(20,21)),FieldDecl(29,FieldDecl(30,Bot))),FieldDecl(31,Top))))),FieldDecl(33,Bot)),AndType(AndType(FieldDecl(34,FunType(35,FieldDecl(36,Bot),TypeProj(3,7))),AndType(AndType(FieldDecl(37,RecType(38,AndType(Bot,AndType(Bot,RecType(39,TypeProj(40,41)))))),FieldDecl(44,Bot)),FieldDecl(45,Bot))),AndType(AndType(FieldDecl(46,Bot),FieldDecl(47,Top)),FieldDecl(48,Bot)))))),AndType(FieldDecl(49,FieldDecl(50,RecType(51,FieldDecl(52,AndType(Bot,FunType(53,FieldDecl(54,FunType(55,Bot,FunType(56,Top,Top))),TypeProj(40,41))))))),FieldDecl(57,TypeProj(40,41)))))))),RecType(58,Bot)))),FieldDecl(59,AndType(RecType(60,AndType(FieldDecl(61,AndType(FunType(62,Bot,FieldDecl(63,AndType(RecType(64,AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(92,FunType(93,Top,AndType(Top,TypeProj(40,41)))),TypeDecl(94,AndType(Bot,Bot),FieldDecl(95,TypeProj(96,97))))),AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(92,FunType(93,Top,AndType(Top,TypeProj(40,41)))),TypeDecl(94,AndType(Bot,Bot),FieldDecl(95,TypeProj(96,97)))))),AndType(FieldDecl(100,Top),AndType(AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(92,FunType(93,Top,AndType(Top,TypeProj(40,41)))),TypeDecl(94,AndType(Bot,Bot),FieldDecl(95,TypeProj(96,97)))))),FieldDecl(100,Top)),AndType(AndType(AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(92,FunType(93,Top,AndType(Top,TypeProj(40,41)))),TypeDecl(94,AndType(Bot,Bot),FieldDecl(95,TypeProj(96,97)))))),FieldDecl(100,Top)),AndType(AndType(AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(92,FunType(93,Top,AndType(Top,TypeProj(40,41)))),TypeDecl(94,AndType(Bot,Bot),FieldDecl(95,TypeProj(96,97)))))),FieldDecl(100,Top)),AndType(FieldDecl(101,Top),TypeDecl(102,Bot,Bot))),FieldDecl(103,FunType(104,RecType(105,Top),Top)))),TypeDecl(106,Bot,TypeDecl(107,Bot,RecType(108,TypeProj(40,41))))))))))),FieldDecl(109,TypeDecl(110,AndType(TypeProj(64,102),Top),TypeProj(64,102))))),TypeDecl(111,FunType(112,TypeDecl(113,TypeProj(114,115),Top),TypeProj(116,117)),FunType(112,TypeDecl(113,TypeProj(114,115),Top),TypeProj(116,117)))))),AndType(RecType(130,FieldDecl(131,RecType(132,AndType(AndType(AndType(FieldDecl(133,AndType(Top,Top)),TypeDecl(134,AndType(Bot,TypeProj(96,97)),FunType(135,FieldDecl(136,RecType(137,Bot)),RecType(138,FieldDecl(139,TypeProj(40,41)))))),FieldDecl(140,RecType(141,TypeProj(96,97)))),AndType(FieldDecl(142,FunType(143,FieldDecl(144,FunType(145,Top,RecType(146,TypeProj(98,99)))),FieldDecl(147,TypeProj(125,126)))),FieldDecl(148,AndType(Bot,FieldDecl(149,FieldDecl(150,RecType(151,FieldDecl(152,TypeProj(40,41)))))))))))),TypeDecl(153,FieldDecl(154,FunType(155,Top,TypeProj(123,124))),FieldDecl(154,FunType(155,Top,TypeProj(123,124))))))),FieldDecl(156,AndType(FieldDecl(157,FunType(158,Bot,TypeProj(98,99))),FieldDecl(159,AndType(RecType(160,FunType(161,RecType(162,AndType(AndType(FieldDecl(163,TypeProj(40,41)),TypeDecl(164,FieldDecl(165,TypeProj(166,167)),FieldDecl(165,TypeProj(125,126)))),FieldDecl(175,AndType(AndType(FunType(176,RecType(177,AndType(FieldDecl(178,TypeProj(42,43)),AndType(AndType(AndType(AndType(FieldDecl(179,Bot),FieldDecl(180,Top)),FieldDecl(181,TypeProj(71,72))),FieldDecl(182,AndType(FieldDecl(183,Bot),FieldDecl(184,Bot)))),AndType(FieldDecl(185,RecType(186,AndType(TypeProj(168,169),AndType(Bot,TypeDecl(187,TypeDecl(188,TypeProj(189,190),Top),TypeDecl(188,TypeProj(189,190),Top)))))),FieldDecl(191,Bot))))),AndType(TypeDecl(192,AndType(Bot,Top),RecType(193,AndType(FieldDecl(194,FunType(195,TypeProj(196,197),AndType(AndType(FieldDecl(200,FunType(201,TypeProj(4,5),Top)),TypeDecl(202,RecType(203,AndType(TypeDecl(204,Bot,FunType(205,AndType(Bot,Top),TypeProj(128,129))),AndType(FieldDecl(206,Top),FieldDecl(207,Bot)))),RecType(203,AndType(TypeDecl(204,Bot,FunType(205,AndType(Bot,Top),TypeProj(128,129))),AndType(FieldDecl(206,Top),FieldDecl(207,Bot)))))),FieldDecl(208,TypeProj(42,43))))),AndType(FieldDecl(209,Bot),FieldDecl(210,Top))))),Top)),RecType(211,Bot)),Top)))),RecType(212,Bot))),Top)))))),TypeDecl(213,RecType(214,TypeProj(3,7)),RecType(214,TypeProj(3,7)))))),:?(:%(IApp(215,216),Que),Bot)),Que),FunType(14,FunType(15,FunType(16,Top,RecType(17,FunType(18,AndType(Bot,FieldDecl(19,RecType(20,AndType(AndType(TypeDecl(21,Bot,Top),FieldDecl(22,Top)),AndType(FieldDecl(23,RecType(24,AndType(AndType(TypeDecl(25,AndType(FieldDecl(26,RecType(27,AndType(AndType(TypeDecl(28,TypeProj(20,21),TypeProj(20,21)),FieldDecl(29,FieldDecl(30,Bot))),FieldDecl(31,Top)))),TypeDecl(32,AndType(Bot,Bot),AndType(Bot,Bot))),FieldDecl(26,RecType(27,AndType(AndType(TypeDecl(28,TypeProj(20,21),TypeProj(20,21)),FieldDecl(29,FieldDecl(30,Bot))),FieldDecl(31,Top))))),FieldDecl(33,Bot)),AndType(AndType(FieldDecl(34,FunType(35,FieldDecl(36,Bot),TypeProj(3,7))),AndType(AndType(FieldDecl(37,RecType(38,AndType(Bot,AndType(Bot,RecType(39,TypeProj(40,41)))))),FieldDecl(44,Bot)),FieldDecl(45,Bot))),AndType(AndType(FieldDecl(46,Bot),FieldDecl(47,Top)),FieldDecl(48,Bot)))))),AndType(FieldDecl(49,FieldDecl(50,RecType(51,FieldDecl(52,AndType(Bot,FunType(53,FieldDecl(54,FunType(55,Bot,FunType(56,Top,Top))),TypeProj(40,41))))))),FieldDecl(57,TypeProj(40,41)))))))),RecType(58,Bot)))),FieldDecl(59,AndType(RecType(60,AndType(FieldDecl(61,AndType(FunType(62,Bot,FieldDecl(63,AndType(RecType(64,AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(92,FunType(93,Top,AndType(Top,TypeProj(40,41)))),TypeDecl(94,AndType(Bot,Bot),FieldDecl(95,TypeProj(96,97))))),AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(92,FunType(93,Top,AndType(Top,TypeProj(40,41)))),TypeDecl(94,AndType(Bot,Bot),FieldDecl(95,TypeProj(96,97)))))),AndType(FieldDecl(100,Top),AndType(AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(92,FunType(93,Top,AndType(Top,TypeProj(40,41)))),TypeDecl(94,AndType(Bot,Bot),FieldDecl(95,TypeProj(96,97)))))),FieldDecl(100,Top)),AndType(AndType(AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(92,FunType(93,Top,AndType(Top,TypeProj(40,41)))),TypeDecl(94,AndType(Bot,Bot),FieldDecl(95,TypeProj(96,97)))))),FieldDecl(100,Top)),AndType(AndType(AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(92,FunType(93,Top,AndType(Top,TypeProj(40,41)))),TypeDecl(94,AndType(Bot,Bot),FieldDecl(95,TypeProj(96,97)))))),FieldDecl(100,Top)),AndType(FieldDecl(101,Top),TypeDecl(102,Bot,Bot))),FieldDecl(103,FunType(104,RecType(105,Top),Top)))),TypeDecl(106,Bot,TypeDecl(107,Bot,RecType(108,TypeProj(40,41))))))))))),FieldDecl(109,TypeDecl(110,AndType(TypeProj(64,102),Top),TypeProj(64,102))))),TypeDecl(111,FunType(112,TypeDecl(113,TypeProj(114,115),Top),TypeProj(116,117)),FunType(112,TypeDecl(113,TypeProj(114,115),Top),TypeProj(116,117)))))),AndType(RecType(130,FieldDecl(131,RecType(132,AndType(AndType(AndType(FieldDecl(133,AndType(Top,Top)),TypeDecl(134,AndType(Bot,TypeProj(96,97)),FunType(135,FieldDecl(136,RecType(137,Bot)),RecType(138,FieldDecl(139,TypeProj(40,41)))))),FieldDecl(140,RecType(141,TypeProj(96,97)))),AndType(FieldDecl(142,FunType(143,FieldDecl(144,FunType(145,Top,RecType(146,TypeProj(98,99)))),FieldDecl(147,TypeProj(125,126)))),FieldDecl(148,AndType(Bot,FieldDecl(149,FieldDecl(150,RecType(151,FieldDecl(152,TypeProj(40,41)))))))))))),TypeDecl(153,FieldDecl(154,FunType(155,Top,TypeProj(123,124))),FieldDecl(154,FunType(155,Top,TypeProj(123,124))))))),FieldDecl(156,AndType(FieldDecl(157,FunType(158,Bot,TypeProj(98,99))),FieldDecl(159,AndType(RecType(160,FunType(161,RecType(162,AndType(AndType(FieldDecl(163,TypeProj(40,41)),TypeDecl(164,FieldDecl(165,TypeProj(166,167)),FieldDecl(165,TypeProj(125,126)))),FieldDecl(175,AndType(AndType(FunType(176,RecType(177,AndType(FieldDecl(178,TypeProj(42,43)),AndType(AndType(AndType(AndType(FieldDecl(179,Bot),FieldDecl(180,Top)),FieldDecl(181,TypeProj(71,72))),FieldDecl(182,AndType(FieldDecl(183,Bot),FieldDecl(184,Bot)))),AndType(FieldDecl(185,RecType(186,AndType(TypeProj(168,169),AndType(Bot,TypeDecl(187,TypeDecl(188,TypeProj(189,190),Top),TypeDecl(188,TypeProj(189,190),Top)))))),FieldDecl(191,Bot))))),AndType(TypeDecl(192,AndType(Bot,Top),RecType(193,AndType(FieldDecl(194,FunType(195,TypeProj(196,197),AndType(AndType(FieldDecl(200,FunType(201,TypeProj(4,5),Top)),TypeDecl(202,RecType(203,AndType(TypeDecl(204,Bot,FunType(205,AndType(Bot,Top),TypeProj(128,129))),AndType(FieldDecl(206,Top),FieldDecl(207,Bot)))),RecType(203,AndType(TypeDecl(204,Bot,FunType(205,AndType(Bot,Top),TypeProj(128,129))),AndType(FieldDecl(206,Top),FieldDecl(207,Bot)))))),FieldDecl(208,TypeProj(42,43))))),AndType(FieldDecl(209,Bot),FieldDecl(210,Top))))),Top)),RecType(211,Bot)),Top)))),RecType(212,Bot))),Top)))))),TypeDecl(213,RecType(214,TypeProj(3,7)),RecType(214,TypeProj(3,7)))))),Bot))),Que),FunType(12,Top,FunType(14,FunType(15,FunType(16,Top,RecType(17,FunType(18,AndType(Bot,FieldDecl(19,RecType(20,AndType(AndType(TypeDecl(21,Bot,Top),FieldDecl(22,Top)),AndType(FieldDecl(23,RecType(24,AndType(AndType(TypeDecl(25,AndType(FieldDecl(26,RecType(27,AndType(AndType(TypeDecl(28,TypeProj(20,21),TypeProj(20,21)),FieldDecl(29,FieldDecl(30,Bot))),FieldDecl(31,Top)))),TypeDecl(32,AndType(Bot,Bot),AndType(Bot,Bot))),FieldDecl(26,RecType(27,AndType(AndType(TypeDecl(28,TypeProj(20,21),TypeProj(20,21)),FieldDecl(29,FieldDecl(30,Bot))),FieldDecl(31,Top))))),FieldDecl(33,Bot)),AndType(AndType(FieldDecl(34,FunType(35,FieldDecl(36,Bot),TypeProj(3,7))),AndType(AndType(FieldDecl(37,RecType(38,AndType(Bot,AndType(Bot,RecType(39,TypeProj(40,41)))))),FieldDecl(44,Bot)),FieldDecl(45,Bot))),AndType(AndType(FieldDecl(46,Bot),FieldDecl(47,Top)),FieldDecl(48,Bot)))))),AndType(FieldDecl(49,FieldDecl(50,RecType(51,FieldDecl(52,AndType(Bot,FunType(53,FieldDecl(54,FunType(55,Bot,FunType(56,Top,Top))),TypeProj(40,41))))))),FieldDecl(57,TypeProj(40,41)))))))),RecType(58,Bot)))),FieldDecl(59,AndType(RecType(60,AndType(FieldDecl(61,AndType(FunType(62,Bot,FieldDecl(63,AndType(RecType(64,AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(92,FunType(93,Top,AndType(Top,TypeProj(40,41)))),TypeDecl(94,AndType(Bot,Bot),FieldDecl(95,TypeProj(96,97))))),AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(92,FunType(93,Top,AndType(Top,TypeProj(40,41)))),TypeDecl(94,AndType(Bot,Bot),FieldDecl(95,TypeProj(96,97)))))),AndType(FieldDecl(100,Top),AndType(AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(92,FunType(93,Top,AndType(Top,TypeProj(40,41)))),TypeDecl(94,AndType(Bot,Bot),FieldDecl(95,TypeProj(96,97)))))),FieldDecl(100,Top)),AndType(AndType(AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(92,FunType(93,Top,AndType(Top,TypeProj(40,41)))),TypeDecl(94,AndType(Bot,Bot),FieldDecl(95,TypeProj(96,97)))))),FieldDecl(100,Top)),AndType(AndType(AndType(AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(65,AndType(AndType(FunType(66,Bot,Top),AndType(RecType(67,AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),AndType(FieldDecl(80,TypeDecl(81,Bot,Bot)),AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),AndType(TypeDecl(82,Bot,RecType(83,Bot)),AndType(AndType(AndType(AndType(FieldDecl(68,FunType(69,RecType(70,TypeProj(71,72)),FunType(77,Bot,Bot))),TypeDecl(78,Bot,TypeDecl(79,Bot,Bot))),FieldDecl(80,TypeDecl(81,Bot,Bot))),TypeDecl(82,Bot,RecType(83,Bot))),AndType(FieldDecl(84,Top),FieldDecl(85,TypeProj(67,78))))))))),FieldDecl(86,TypeDecl(87,AndType(AndType(Bot,TypeProj(73,74)),FunType(89,FunType(90,TypeProj(71,72),Bot),TypeDecl(91,Top,Top))),RecType(88,TypeProj(71,72)))))),TypeProj(75,76))),Bot)),AndType(FieldDecl(92,FunType(93,Top,AndType(Top,TypeProj(40,41)))),TypeDecl(94,AndType(Bot,Bot),FieldDecl(95,TypeProj(96,97)))))),FieldDecl(100,Top)),AndType(FieldDecl(101,Top),TypeDecl(102,Bot,Bot))),FieldDecl(103,FunType(104,RecType(105,Top),Top)))),TypeDecl(106,Bot,TypeDecl(107,Bot,RecType(108,TypeProj(40,41))))))))))),FieldDecl(109,TypeDecl(110,AndType(TypeProj(64,102),Top),TypeProj(64,102))))),TypeDecl(111,FunType(112,TypeDecl(113,TypeProj(114,115),Top),TypeProj(116,117)),FunType(112,TypeDecl(113,TypeProj(114,115),Top),TypeProj(116,117)))))),AndType(RecType(130,FieldDecl(131,RecType(132,AndType(AndType(AndType(FieldDecl(133,AndType(Top,Top)),TypeDecl(134,AndType(Bot,TypeProj(96,97)),FunType(135,FieldDecl(136,RecType(137,Bot)),RecType(138,FieldDecl(139,TypeProj(40,41)))))),FieldDecl(140,RecType(141,TypeProj(96,97)))),AndType(FieldDecl(142,FunType(143,FieldDecl(144,FunType(145,Top,RecType(146,TypeProj(98,99)))),FieldDecl(147,TypeProj(125,126)))),FieldDecl(148,AndType(Bot,FieldDecl(149,FieldDecl(150,RecType(151,FieldDecl(152,TypeProj(40,41)))))))))))),TypeDecl(153,FieldDecl(154,FunType(155,Top,TypeProj(123,124))),FieldDecl(154,FunType(155,Top,TypeProj(123,124))))))),FieldDecl(156,AndType(FieldDecl(157,FunType(158,Bot,TypeProj(98,99))),FieldDecl(159,AndType(RecType(160,FunType(161,RecType(162,AndType(AndType(FieldDecl(163,TypeProj(40,41)),TypeDecl(164,FieldDecl(165,TypeProj(166,167)),FieldDecl(165,TypeProj(125,126)))),FieldDecl(175,AndType(AndType(FunType(176,RecType(177,AndType(FieldDecl(178,TypeProj(42,43)),AndType(AndType(AndType(AndType(FieldDecl(179,Bot),FieldDecl(180,Top)),FieldDecl(181,TypeProj(71,72))),FieldDecl(182,AndType(FieldDecl(183,Bot),FieldDecl(184,Bot)))),AndType(FieldDecl(185,RecType(186,AndType(TypeProj(168,169),AndType(Bot,TypeDecl(187,TypeDecl(188,TypeProj(189,190),Top),TypeDecl(188,TypeProj(189,190),Top)))))),FieldDecl(191,Bot))))),AndType(TypeDecl(192,AndType(Bot,Top),RecType(193,AndType(FieldDecl(194,FunType(195,TypeProj(196,197),AndType(AndType(FieldDecl(200,FunType(201,TypeProj(4,5),Top)),TypeDecl(202,RecType(203,AndType(TypeDecl(204,Bot,FunType(205,AndType(Bot,Top),TypeProj(128,129))),AndType(FieldDecl(206,Top),FieldDecl(207,Bot)))),RecType(203,AndType(TypeDecl(204,Bot,FunType(205,AndType(Bot,Top),TypeProj(128,129))),AndType(FieldDecl(206,Top),FieldDecl(207,Bot)))))),FieldDecl(208,TypeProj(42,43))))),AndType(FieldDecl(209,Bot),FieldDecl(210,Top))))),Top)),RecType(211,Bot)),Top)))),RecType(212,Bot))),Top)))))),TypeDecl(213,RecType(214,TypeProj(3,7)),RecType(214,TypeProj(3,7)))))),Bot))),:?(:%(IFun(288,Bot,:?(:%(ILet(289,:?(:%(ISel(290,291),Que),RecType(292,Bot)),:?(:%(ISel(293,294),Que),Bot)),Que),Bot)),Que),FunType(288,Bot,Bot))),Que),FunType(288,Bot,Bot)),:?(:%(ILet(295,:?(:%(ISel(296,297),Que),Top),:?(:%(ILet(298,:?(:%(ISel(299,300),Que),FunType(301,TypeProj(123,124),RecType(302,AndType(AndType(AndType(AndType(FieldDecl(303,AndType(TypeDecl(304,TypeProj(306,307),RecType(305,Bot)),FieldDecl(308,FunType(309,FunType(310,AndType(Bot,Bot),Top),AndType(TypeDecl(311,AndType(TypeProj(312,313),RecType(316,TypeProj(224,225))),Top),Top))))),AndType(FieldDecl(303,AndType(TypeDecl(304,TypeProj(306,307),RecType(305,Bot)),FieldDecl(308,FunType(309,FunType(310,AndType(Bot,Bot),Top),AndType(TypeDecl(311,AndType(TypeProj(312,313),RecType(316,TypeProj(224,225))),Top),Top))))),AndType(AndType(FieldDecl(303,AndType(TypeDecl(304,TypeProj(306,307),RecType(305,Bot)),FieldDecl(308,FunType(309,FunType(310,AndType(Bot,Bot),Top),AndType(TypeDecl(311,AndType(TypeProj(312,313),RecType(316,TypeProj(224,225))),Top),Top))))),AndType(TypeDecl(317,Bot,Bot),TypeDecl(318,FieldDecl(319,Bot),FieldDecl(319,RecType(320,FunType(321,RecType(322,Bot),TypeProj(116,117))))))),FieldDecl(323,TypeDecl(324,AndType(Bot,TypeDecl(339,AndType(Bot,RecType(340,Bot)),Bot)),FunType(325,FunType(326,TypeProj(73,74),FieldDecl(327,RecType(328,RecType(329,RecType(330,AndType(TypeDecl(331,AndType(Bot,RecType(332,Top)),TypeProj(168,169)),AndType(AndType(FieldDecl(333,FieldDecl(334,TypeDecl(335,Bot,Bot))),FieldDecl(336,Top)),AndType(FieldDecl(337,Bot),FieldDecl(338,Top))))))))),Top)))))),FieldDecl(341,FunType(342,Top,TypeProj(216,252)))),FieldDecl(343,RecType(344,AndType(TypeDecl(345,RecType(346,Top),RecType(346,Top)),AndType(FieldDecl(347,Top),AndType(FieldDecl(348,TypeProj(224,225)),FieldDecl(349,Top))))))),FieldDecl(350,FunType(351,RecType(352,TypeDecl(353,Bot,FunType(354,Bot,AndType(TypeDecl(355,AndType(Bot,Top),RecType(356,AndType(AndType(FieldDecl(357,AndType(Top,AndType(AndType(Top,FieldDecl(358,Top)),AndType(Top,Top)))),FieldDecl(359,FunType(360,TypeDecl(361,AndType(Bot,TypeProj(264,265)),TypeProj(73,74)),TypeProj(96,97)))),FieldDecl(362,Top)))),FieldDecl(363,RecType(364,AndType(TypeDecl(365,FunType(366,Top,Bot),FunType(366,Top,Bot)),FieldDecl(367,TypeDecl(368,FieldDecl(369,Bot),FieldDecl(369,Bot)))))))))),FieldDecl(370,FunType(371,FunType(372,TypeDecl(373,TypeProj(96,97),TypeProj(96,97)),AndType(RecType(374,FunType(375,TypeProj(119,120),TypeProj(264,265))),FieldDecl(376,TypeDecl(377,Bot,FunType(378,AndType(TypeProj(235,236),RecType(379,Top)),FieldDecl(380,TypeProj(168,169))))))),TypeProj(302,317))))))))),:?(:%(IVar(381),Que),TypeProj(238,239))),Que),TypeProj(238,239))),Que),TypeProj(238,239))),Que),TypeProj(238,239)),:?(:%(IApp(382,383),Que),Bot)),Que),Bot)),Que),FunType(3,AndType(TypeProj(4,5),RecType(6,TypeDecl(7,Top,Top))),Bot))),Que),FunType(3,AndType(TypeProj(4,5),RecType(6,TypeDecl(7,Top,Top))),Bot)))

//P.namedln("problem", p)

val InferenceProblem(GlobalContext(scope, nextSymbol), term, prototype, expected) = p

val su = new SymbolUniverse(nextSymbol)

//P.namedln("scope", scope)
//P.namedln("term", term)
//P.namedln("expected", expected)

val parallelism = 4
val startTime = System.nanoTime()
val resTerm = typecheckInParallel(su, term, prototype, scope, parallelism)
val endTime = System.nanoTime()

//P.namedln("resTerm", resTerm)
P.namedln("resTerm.totNumNodes", resTerm.map{_.totNumNodes})

//if (resTerm != None) {
//  P.namedln("scope", scope)
//  P.namedln("eqcheck", eqcheck(scope, resTerm.get, expected))
//  P.namedln("res == expected", equalTerms(scope, resTerm.get, expected))
//}

P.namedln("TIME", s"${(endTime - startTime)*1E-9}s")
}

@tailrec
def meh(n: Int, acc: Term = Var(0)): Term =
  if (n == 0)
    acc
  else
    meh(n-1, Let(n, Var(0), acc))

@tailrec
def meh2(n: Int, acc: Term = Var(0)): Term =
  if (n == 0)
    acc
  else
    meh2(n-1, Let(n, acc, acc))

@tailrec
def meh3(n: Int, acc: Def = FieldDef(0, Var(0))): Def =
  if (n == 0)
    acc
  else
    meh3(n-1, AndDef(acc, FieldDef(n, Var(0))))

def meh4(a: Int, b: Int): Def =
  if (a >= b)
    FieldDef(a, Var(0))
  else
    AndDef(
      meh4(a, a+(b-a)/2),
      meh4(a+(b-a)/2+1, b)
    )



def debuggen(): Unit = {
  val seed: Long =
//-9223372036854775808L
//-2741381128833295964L
//-5168474992618704866L
//-6626922213819125528L
//8433069248733135538L
//6600269935819868943L
//5360966813463307211L
//-8770115349614841094L
//641025681197646900L
//-295046L
//-6754862362974092920L
//4263077685207510612L
//-1497104463425051576L
0L

  val (ctx, p) = genInferenceProblemFromPrototype(GlobalContext(), Map(), Que)(Gen.Parameters.default.withSize(50000), Seed(seed)).get
  //P.namedln("ctx", ctx)
  //P.namedln("p", p)
  P.namedln("assemble(p)", InferenceProblem.assemble(ctx, p))
  val wf = InferenceProblem.wellFormed(ctx, p)
  P.namedln("wf", wf)


  if (wf) {
    val InferenceProblem(GlobalContext(scope, nextSymbol), term, prototype, expected) = InferenceProblem.assemble(ctx, p)
    val su = new SymbolUniverse(nextSymbol)

    val resTerm = typecheckTerm(su, term, prototype, scope)
    P.namedln("expected", expected)
    P.namedln("resTerm", resTerm)

    P.namedln("scope", scope)
    P.namedln("eqcheck", eqcheck(scope, resTerm, expected))
    P.namedln("res == expected", equalTerms(scope, resTerm, expected))
  }
}



} // end object Main
