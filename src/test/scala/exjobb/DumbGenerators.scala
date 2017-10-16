package exjobb

import Dol._

import scala.annotation.tailrec
import scala.util.control.NonFatal
import org.scalacheck.Properties
import org.scalacheck.Prop
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.Gen
import org.scalacheck.Gen.const
import org.scalacheck.Gen.oneOf
import org.scalacheck.Gen.someOf
import org.scalacheck.Gen.listOf
import org.scalacheck.Shrink

import ScalaCheckUtils._

object DumbGenerators {

  def genNonsenseType(count: Int = 0, root: Boolean = false): Gen[(Int, Type)] = Gen.sized{size =>
    if (size < 1)
      if (root) Gen.fail else ???
    else {
      // size == 1
      val genBot = const((count, Bot))
      val genTop = const((count, Top))
      val genProj = for {
        x <- const(count)
        a <- const(count+1)
      } yield (count+2, TypeProj(x, a))
      val min1 = Seq(genBot, genTop, genProj)

      if (size <= 1)
        oneOfGens(min1)
      else {
        // size >= 2
        val genRecType = for {
          x     <- const(count)
          (count2, xType) <- Gen.resize(size - 1, genNonsenseType(count+1))
        } yield (count2, RecType(x, xType))
        val genFieldDecl = for {
          a     <- const(count)
          (count2, aType) <- Gen.resize(size - 1, genNonsenseType(count+1))
        } yield (count2, FieldDecl(a, aType))
        val min2 = Seq(genRecType, genFieldDecl)

        if (size <= 2)
          oneOfGens(min1 ++ min2)
        else {
          // size >= 3
          val genAndType = for {
            (leftSize, rightSize) <- splitSizeNonZero(size - 1)
            (count2, left)  <- Gen.resize(leftSize, genNonsenseType(count))
            (count3, right) <- Gen.resize(rightSize, genNonsenseType(count2))
          } yield (count3, AndType(left, right))
          val genFunType = for {
            x <- const(count)
            (argSize, resSize) <- splitSizeNonZero(size - 1)
            (count2, argType) <- Gen.resize(argSize, genNonsenseType(count+1))
            (count3, resType) <- Gen.resize(resSize, genNonsenseType(count2))
          } yield (count3, FunType(x, argType, resType))
          val genTypeDecl = for {
            a <- const(count)
            (aLowerSize, aUpperSize) <- splitSizeNonZero(size - 1)
            (count2, aLowerType) <- Gen.resize(aLowerSize, genNonsenseType(count+1))
            (count3, aUpperType) <- Gen.resize(aUpperSize, genNonsenseType(count2))
          } yield (count3, TypeDecl(a, aLowerType, aUpperType))

          val min3 = Seq(genAndType, genFunType, genTypeDecl)

          oneOfGens(min1 ++ min2 ++ min3)
        }
      }
    }
  }


}
