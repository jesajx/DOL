package exjobb

import java.util.concurrent.atomic.AtomicBoolean
import Dol._
import cell._

object Main {
  def main(args: Array[String]): Unit = {
    val startTime = System.nanoTime()


    val su = new SymbolUniverse()
    def cast(t: Term, tType: Type): Term = {
      val x = su.newSymbol()
      val y = su.newSymbol()
      val z = su.newSymbol()
      Let(x, Fun(z, tType, Var(z)), Let(y, t, App(x, y)))
    }
    def explicitFun(x: Symbol, xType: Type, res: Term, resType: Type): Fun = {
      Fun(x, xType, cast(res, resType))
    }
    def andType(xs: Type*): Type = {
      xs.reduce((a,b) => AndType(a,b))
    }
    def andDef(xs: Def*): Def = {
      xs.reduce((a,b) => AndDef(a,b))
    }
    def badRange() = {
      val x = su.newSymbol()
      val t = su.newSymbol()
      Obj(x,
        TypeDecl(t, Top, Bot),
        TypeDef(t, Top))
    }
    def andFunExample() = {
      val x = su.newSymbol()
      val f = su.newSymbol()
      Obj(x,
        FieldDecl(f, AndType(FunType(x, Top, Top), FunType(x, Bot, Top))),
        FieldDef(f, Fun(x, Top, Var(x))))
    }
    def projGlbExample() = {
      val x = su.newSymbol()
      val y = su.newSymbol()
      val z = su.newSymbol()
      val a = su.newSymbol()
      val b = su.newSymbol()
      val p0 = su.newSymbol()
      val p1 = su.newSymbol()
      val p2 = su.newSymbol()
      val p3 = su.newSymbol()
      Obj(x,
        RecType(y,
          andType(
            FieldDecl(a, FunType(z, TypeProj(y, p0), TypeProj(y, p2))),
            FieldDecl(a, FunType(z, TypeProj(y, p1), TypeProj(y, p3))),
            TypeDecl(p0, TypeProj(y, p1), Top),
            TypeDecl(p1, Bot, Top),
            TypeDecl(p2, RecType(z, Bot), Top),
            TypeDecl(p3, Bot, Top))),
        andDef(
          FieldDef(a, Fun(z, Top, Var(z))),
          TypeDef(p0, Top),
          TypeDef(p1, Top),
          TypeDef(p2, Top),
          TypeDef(p3, Top)))
      //Obj(x,
      //  RecType(y,
      //    andType(
      //      FieldDecl(a, FunType(z, TypeProj(y, p0), TypeProj(y, p2))),
      //      FieldDecl(a, FunType(z, TypeProj(y, p1), TypeProj(y, p3))),
      //      TypeDecl(p0, TypeProj(y, p1), Top),
      //      TypeDecl(p1, Bot, Top),
      //      TypeDecl(p2, RecType(z, Bot), Top),
      //      TypeDecl(p3, Bot, Top))),
      //  andDef(
      //    FieldDef(a, Fun(z, Top, Var(z))),
      //    TypeDef(p0, Top),
      //    TypeDef(p1, Top),
      //    TypeDef(p2, Top),
      //    TypeDef(p3, Top)))
    }
    def idIdExample() = {
      val id = su.newSymbol()
      val idId = su.newSymbol()
      val self = su.newSymbol()
      val a = su.newSymbol()
      val b = su.newSymbol()
      val c = su.newSymbol()
      val d = su.newSymbol()
      val x = su.newSymbol()
      val y = su.newSymbol()
      val z = su.newSymbol()
      val u = su.newSymbol()
      val t = su.newSymbol()
      val idT = su.newSymbol()
      val sym2str = Map(id -> "id", x -> "x", y -> "y", t -> "t", idT -> "idT")
      //val e =
      //  Let(id,
      //    Fun(y, TypeDecl(t, Bot, Top),
      //      Fun(x, TypeProj(y, t), Var(x))),
      //    Var(id))
      //val e =
      //  Let(id,
      //    Fun(y, TypeDecl(t, Bot, Top),
      //      Fun(u, TypeProj(y, t), Var(u))),
      //    Fun(x, TypeDecl(t, Bot, Top),
      //      Let(b,
      //        Obj(self,
      //          TypeDecl(t, TypeProj(x, t), TypeProj(x, t)),
      //          TypeDef(t, TypeProj(x, t))),
      //        App(id, b))))
      Let(id,
        Fun(y, TypeDecl(t, Bot, Top),
          Fun(u, TypeProj(y, t), Var(u))),
        Let(idId,
          Fun(x, TypeDecl(t, Bot, Top),
            Let(a,
              Obj(self,
                TypeDecl(t,
                  FunType(z, TypeProj(x, t), TypeProj(x, t)),
                  FunType(z, TypeProj(x, t), TypeProj(x, t))),
                TypeDef(t, FunType(z, TypeProj(x, t), TypeProj(x, t)))),
              Let(b,
                Obj(self,
                  TypeDecl(t, TypeProj(x, t), TypeProj(x, t)),
                  TypeDef(t, TypeProj(x, t))),
                Let(c, App(id, a),
                  Let(d, App(id, b),
                    App(c, d)))))),
          Var(idId)))
    }
    def simpleObj() = {
      val x = su.newSymbol()
      val a = su.newSymbol()
      Obj(x,
        TypeDecl(a, Top, Top),
        TypeDef(a, Top))
    }
    def simpleObj2() = {
      val x = su.newSymbol()
      val a = su.newSymbol()
      Obj(x,
        TypeDecl(a, Bot, Top),
        TypeDef(a, Top))
    }
    def infrec() = {
      val x = su.newSymbol()
      val y = su.newSymbol()
      val t = su.newSymbol()
      Obj(x,
        RecType(y, TypeDecl(t, TypeProj(x, t), TypeProj(x, t))),
        TypeDef(t, TypeProj(x, t)))
    }
    def hairyExample() = {
      val x = su.newSymbol()
      val y = su.newSymbol()
      val z = su.newSymbol()
      val t = su.newSymbol()
      val a = su.newSymbol()
      Fun(z, Bot,
        Obj(x,
          RecType(y,
            AndType(
              TypeProj(y, t),
              TypeDecl(t,
                FieldDecl(a, Bot),
                FieldDecl(a, Top)))),
          AndDef(
            FieldDef(a, Var(z)),
            TypeDef(t, FieldDecl(a, Bot)))))
    }
    def numberExample() = {
      val w = su.newSymbol()
      val self = su.newSymbol()
      val pred = su.newSymbol()
      val zero = su.newSymbol()
      val succ = su.newSymbol()
      val add = su.newSymbol()
      val number = su.newSymbol()
      val one = su.newSymbol()
      val two = su.newSymbol()
      val p = su.newSymbol()
      val x = su.newSymbol()
      def app(a: Term, b: Term): Term = {
        val tmp1 = su.newSymbol()
        val tmp2 = su.newSymbol()
        Let(tmp1, a,
          Let(tmp2, b,
            App(tmp1, tmp2)))
      }
      def sel(e: Any, path: Symbol*): Term = {
        (e, path) match {
          case (x: Symbol, _) => sel(Var(x), path: _*)
          case (e: Term, Nil) => e
          case (e: Term, a +: rest) =>
            val tmp = su.newSymbol()
            Let(tmp, e, sel(Sel(tmp, a), rest: _*))
          case _ =>
            ???
        }
      }
      val numberDecl = andType(
        FieldDecl(pred, TypeProj(self, number)),
        FieldDecl(add, FunType(x, TypeProj(self, number), TypeProj(self, number)))
      )
      Obj(w,
        RecType(self,
          andType(
            TypeDecl(number, numberDecl, numberDecl),
            FieldDecl(succ, FunType(x, TypeProj(self, number), TypeProj(self, number))),
            FieldDecl(zero, TypeProj(self, number)),
            FieldDecl(one, TypeProj(self, number)),
            FieldDecl(two, TypeProj(self, number)))),
        andDef(
          TypeDef(number,
            andType(
              FieldDecl(pred, TypeProj(w, number)),
              FieldDecl(add, FunType(x, TypeProj(w, number), TypeProj(w, number))))),
          FieldDef(zero,
            Obj(self,
              TypeProj(w, number),
              andDef(
                FieldDef(pred, Var(self)),
                FieldDef(add, Fun(x, TypeProj(w, number), Var(x)))
                ))),
          FieldDef(succ, Fun(p, TypeProj(w, number),
            Obj(self,
              TypeProj(w, number),
              andDef(
                FieldDef(pred, Var(p)),
                FieldDef(add, Fun(x, TypeProj(w, number),
                  app(sel(w, pred, add), app(sel(w, succ), Var(x))))))))),
          FieldDef(one, app(sel(w, succ), sel(w, zero))),
          FieldDef(two, app(sel(w, one, add), sel(w, one)))))
    }

    val pool = new HandlerPool(1) // TODO
    val tc  = new Typechecker(su, pool, new AtomicBoolean(false))

    //val e = idIdExample()
    //val e = numberExample()
    //val e = hairyExample()
    //val e = infrec()
    val e = simpleObj()
    //val e = simpleObj2()
    //val e = projGlbExample()
    //val e = badRange()
    //val e = andFunExample()


    val p = CanonicalQue
    val scope = Map[Symbol, CanonicalType]()

    val res = tc.run[Term]{
      tc.fullTypecheck(e, p, scope)(_)
    }
    println("smurf")
    println(s"$e --->          $res")

    //val eType = tc.run{cont =>
    //  tc.fullInfer(e)(cont)
    //}

    //tc.run[CanonicalType]{cont =>
    //  val x = su.newSymbol()
    //  val a = su.newSymbol()
    //  //val aPrototype = CanonicalObjType(x, Map(), Map(a -> (CanonicalBot, CanonicalTop)), Set())
    //  //tc.canonicalRaise(Map(), CanonicalObjType(x, Map(), Map(a -> (CanonicalTop, CanonicalTop)), Set()), aPrototype)(cont)

    //  val aPrototype = CanonicalObjType(x, Map(), Map(a -> (CanonicalBot, CanonicalTop)), Set())
    //  tc.canonicalRaise(Map(), CanonicalObjType(x, Map(), Map(a -> (CanonicalTop, CanonicalTop)), Set()), aPrototype) {b =>
    //    tc.expandCanonicalFutureTypes(b){b =>
    //      println(s"PHASERES: $b")
    //    }
    //  }

    //  //tc.canonicalLower(scope, CanonicalTop, CanonicalBot){l =>
    //  //  tc.canonicalRaise(scope, CanonicalTop, CanonicalTop){ u =>
    //  //    println(s"PHASERES: $l, $u")
    //  //  }
    //  //}

    //  cont(CanonicalErrorType)
    //}


    val endTime = System.nanoTime()
    println(s"RUN: ${(endTime-startTime)/1E6}ms")
  }
}
