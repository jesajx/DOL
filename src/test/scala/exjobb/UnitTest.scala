package exjobb

import org.scalatest._

import Dol._
import DolGenerators._

class UnitTest extends UnitSpec {

  test("bound vars should not matter in RecType for subtyping with var") {
    val scope = Scope()
    val z = 0
    val a = RecType(2, RecType(1, Top))
    val b = RecType(1, RecType(2, Top))
    assert(NoFuture.raise(scope + (z -> a), -1, Some(z), a, b) == Some(b))
    assert(NoFuture.raise(scope + (z -> b), -1, Some(z), b, a) == Some(a))
  }

  test("bound vars should not matter in RecType for subtyping without var") {
    val scope = Scope()
    val a = RecType(1, RecType(2, Top))
    val b = RecType(2, RecType(1, Top))
    assert(NoFuture.raise(scope, -1, None, a, b) == Some(b))
    assert(NoFuture.raise(scope, -1, None, b, a) == Some(a))
  }

  test("bound vars should not matter in FunType for subtyping") {
    val scope = Scope()
    val a = FunType(1, Top, Bot)
    val b = FunType(2, Bot, Top)
    assert(NoFuture.raise(scope, -1, None, a, b) == Some(b))
  }

  test("bound vars should not matter in FunType for matching") {
    val scope = Scope()
    val a = FunType(1, Top, Bot)
    val p = FunType(2, Que, Top)
    assert(NoFuture.raise(scope, -1, None, a, p) == Some(FunType(2, Top, Top)))
  }

  test("bound vars should properly be renamed when matching FunType") {
    val scope = Scope()
    val a = FunType(1, Top, TypeProj(1, 3))
    val p = FunType(2, Top, Que)
    assert(NoFuture.raise(scope, -1, None, a, p) == Some(FunType(2, Top, TypeProj(2, 3))))
  }

  test("bound vars should properly be renamed when raising FunType Contravariant") {
    val scope = Scope()
    val a = FunType(4, FunType(1, TypeDecl(3, Bot, Top), TypeProj(1, 3)), Top)
    val p = FunType(4, FunType(2, TypeDecl(3, Bot, Top), Que), Top)
    assert(NoFuture.raise(scope, -1, None, a, p) == Some(FunType(4, FunType(2, TypeDecl(3, Bot, Top), TypeProj(2, 3)), Top)))
  }

  test("bound vars should properly be renamed when lowering FunType") {
    val scope = Scope()
    val a = FunType(4, FunType(1, TypeDecl(3, Bot, Top), TypeProj(1, 3)), Top)
    val p = FunType(4, FunType(2, TypeDecl(3, Bot, Top), Que), Top)
    assert(NoFuture.lower(scope, -1, None, p, a) == Some(FunType(4, FunType(2, TypeDecl(3, Bot, Top), TypeProj(2, 3)), Top)))
  }

  //test("boom1.2") {
  //  val r = 1
  //  val z = 0
  //  val scope = Map(
  //    5 -> TypeDecl(6, Bot, Bot),
  //    9 -> AndType(Top, Bot),
  //    2 -> FieldDecl(3, Bot),
  //    7 -> TypeProj(5, 6),
  //    8 -> Top,
  //    4 -> TypeProj(5, 6)
  //  )
  //  val aRaiseP2 =
  //    FunType(
  //      10,
  //      Top,
  //      FunType(
  //        11,
  //        FieldDecl(
  //          12,
  //          AndType(
  //            RecType(13, Top),
  //            RecType(14, RecType(15,
  //              FieldDecl(16, FieldDecl(17, Bot)))))),
  //        Bot))
  //  val aRaiseP =
  //    FunType(
  //      10,
  //      Top,
  //      FunType(
  //        11,
  //        FieldDecl(
  //          12,
  //          AndType(
  //            Top, // NOTE: We can't wrap this in RecType since it is nested.
  //          RecType(14, RecType(15,
  //            FieldDecl(16, FieldDecl(17, Bot)))))),
  //        Bot))

  //  assert(NoFuture.raise(scope, r, Some(z), aRaiseP2, aRaiseP) == Some(aRaiseP))
  //}

//  test("boom2") {
//    val ((globalScope, nextSymbol), localScope, r, z, a, p) =
//(
//  (
//    Map(
//      6 -> TypeDecl(7, Bot, RecType
//  (8, Bot)),
//      11 -> TypeDecl(12, Bot, Top),
//      18 -> TypeDecl(19, Bot, Bot),
//      28 -> TypeDecl(29, RecType(
//30, Bot), RecType(30, Bot))
//    ),
//    31
//  ),
//  Map(
//    14 -> FunType(15, Bot, Top),
//    13 -> Bot,
//    2 -> Bot,
//    3 -> FunType(
//      4,
//      AndType(
//        FieldDecl(5, TypeProj(6,
//7)),
//        TypeDecl(
//          9,
//          Bot,
//          TypeDecl(10, AndType(Bot, Top),
//TypeProj(11, 12))
//        )
//      ),
//      TypeProj(11, 12)
//    ),
//    16 -> TypeDecl(17, TypeProj(
//18, 19), AndType(Top, TypeProj(
//  11, 12)))
//  ),
//  1,
//  0,
//  FunType(
//    20,
//    AndType(
//      FunType(
//        21,
//        RecType(
//          22,
//          FieldDecl(
//            23,
//            RecType(24, TypeDecl(25
//  , Bot, TypeProj(6, 7)))
//          )
//        ),
//        AndType(Bot, Bot)
//      ),
//      RecType(26, Top)
//    ),
//    FunType(27, TypeProj(28, 29), TypeProj(11, 12))
//  ),
//  FunType(
//    20,
//    AndType(
//      FunType(
//        21,
//        RecType(
//          22,
//          FieldDecl(
//            23,
//            RecType(24, TypeDecl(25, Bot, TypeProj(6, 7)))
//          )
//        ),
//        Que
//      ),
//      Que
//    ),
//    FunType(27, TypeProj(28, 29), TypeProj(11, 12))
//  )
//)
//
//    val scope = globalScope ++ localScope
//    val res = NoFuture.raise(scope + (z -> a), r, Some(z), a, p)
//    pprint.pprintln(res)
//    assert(res != None)
//    assert(NoFuture.raise(scope + (z -> a), r, Some(z), a, res.get) == res)
//    assert(NoFuture.raise(scope + (z -> res.get), r, Some(z), res.get, a) == Some(a))
//  }
//
//  test("boom3") {
//    val r = 1
//    val z = 0
//    val scope = Map(
//      6 -> TypeDecl(7, Bot, RecType
//  (8, Bot)),
//      11 -> TypeDecl(12, Bot, Top),
//      18 -> TypeDecl(19, Bot, Bot),
//      28 -> TypeDecl(29, RecType(
//30, Bot), RecType(30, Bot)),
//    14 -> FunType(15, Bot, Top),
//    13 -> Bot,
//    2 -> Bot,
//    3 -> FunType(
//      4,
//      AndType(
//        FieldDecl(5, TypeProj(6,
//7)),
//        TypeDecl(
//          9,
//          Bot,
//          TypeDecl(10, AndType(Bot, Top),
//TypeProj(11, 12))
//        )
//      ),
//      TypeProj(11, 12)
//    ),
//    16 -> TypeDecl(17, TypeProj(
//18, 19), AndType(Top, TypeProj(
//  11, 12)))
//  )
//    val a =
//  FunType(
//    20,
//    AndType(
//      FunType(
//        21,
//        RecType(
//          22,
//          FieldDecl(
//            23,
//            RecType(24, TypeDecl(25
//  , Bot, TypeProj(6, 7)))
//          )
//        ),
//        AndType(Bot, Bot)
//      ),
//      RecType(26, Top)
//    ),
//    FunType(27, TypeProj(28, 29), TypeProj(11, 12))
//  )
//
//val res =
//  AndType(
//    FunType(
//      20,
//      AndType(
//        FunType(
//          21,
//          RecType(
//            22,
//            FieldDecl(
//              23,
//              RecType(24, TypeDecl(25, Bot, TypeProj(6, 7)))
//            )
//          ),
//          Bot
//        ),
//        FunType(
//          21,
//          RecType(
//            22,
//            FieldDecl(
//              23,
//              RecType(24, TypeDecl(25, Bot, TypeProj(6, 7)))
//            )
//          ),
//          Top
//        )
//      ),
//      FunType(27, TypeProj(28, 29), TypeProj(11, 12))
//    ),
//    FunType(
//      20,
//      FunType(
//        21,
//        RecType(
//          22,
//          FieldDecl(
//            23,
//            RecType(24, TypeDecl(25, Bot, TypeProj(6, 7)))
//          )
//        ),
//        Bot
//      ),
//      FunType(27, TypeProj(28, 29), TypeProj(11, 12))
//    )
//  )
//
//    assert(NoFuture.raise(scope + (z -> a), r, Some(z), a, res) == res)
//    assert(NoFuture.raise(scope + (z -> res), r, Some(z), res, a) == Some(a))
//  }

} // end UnitTest
