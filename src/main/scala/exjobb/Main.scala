package exjobb

import java.util.concurrent.atomic.AtomicBoolean
import Dol._
import cell._

object Main {
  def main(args: Array[String]): Unit = {
//    val problem = (new SymbolUniverse(20), Obj(16,TypeDecl(17,Top,Top),TypeDef(17,Top)), Que, Map():Scope, Obj(16, TypeDecl(17,Top,Top), TypeDef(17,Top).withType(TypeDecl(17,Top,Top))).withType(TypeDecl(17,Top,Top)))
//    val (su, term, prototype, scope, expected) = problem
//    val res = typecheckSequentially(su, term, prototype, scope)
//
//    pprint.pprintln(problem)
//    println(s"res = $res")


val ((scope1, nextSymbol), z, r, a, b, p) =

(
  (
    Map(
      10 -> TypeDecl(11, TypeProj(12, 13), TypeProj(12, 13)),
      14 -> TypeDecl(15, Bot, Top),
      6 -> TypeDecl(7, Bot, TypeProj(8, 9)),
      12 -> TypeDecl(13, Bot, TypeProj(14, 15)),
      23 -> TypeDecl(24, Bot, Top),
      8 -> TypeDecl(9, TypeProj(10, 11), TypeProj(10, 11)),
      4 -> TypeDecl(5, TypeProj(6, 7), TypeProj(6, 7))
    ): Scope,
    31
  ),
  1,
  0,
  Bot,
  AndType(
    AndType(
      AndType(
        FieldDecl(2, Top),
        TypeDecl(3, TypeProj(4, 5), TypeProj(4, 5))
      ),
      AndType(
        FieldDecl(16, TypeProj(4, 5)),
        FieldDecl(17, TypeProj(4, 5))
      )
    ),
    FieldDecl(
      18,
      FieldDecl(
        19,
        RecType(
          20,
          AndType(
            TypeDecl(21, Bot, TypeProj(10, 11)),
            AndType(
              AndType(
                AndType(
                  AndType(
                    FieldDecl(22, TypeProj(23, 24)),
                    FieldDecl(25, TypeProj(6, 7))
                  ),
                  FieldDecl(26, TypeProj(4, 5))
                ),
                FieldDecl(27, Top)
              ),
              AndType(
                AndType(FieldDecl(28, Top), FieldDecl(29, Top)),
                FieldDecl(30, Top)
              )
            )
          )
        )
      )
    )
  ),
  AndType(
    AndType(
      AndType(FieldDecl(2, Que), TypeDecl(3, TypeProj(4, 5), Que)),
      AndType(Que, Que)
    ),
    Que
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
