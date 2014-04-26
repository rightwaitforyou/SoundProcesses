//package de.sciss.synth.proc
//
//import de.sciss.lucre.expr.Expr
//import de.sciss.ConfluentEventSpec
//import de.sciss.lucre
//
///*
//  To run only this suite:
//
//  test-only de.sciss.synth.proc.ElemSpec
//
//  */
//class ElemSpec extends ConfluentEventSpec {
//  import imp._
//
//  "Elem" should "serialize and de-serialize" in { system =>
//    val pH = system.step { implicit tx =>
//      val p = Proc[S]
//      p.attr.put("foo", IntElem(lucre.expr.Int.newVar(1234)))
//      tx.newHandle(p)
//    }
//
//    system.step { implicit tx =>
//      val p     = pH()
//      // println(s"Keys found: ${p.attributes.keys.mkString(", ")}")
//      val expr  = p.attr.expr[Int]("foo")
//      val v     = expr match {
//        case Some(Expr.Var(vr)) => vr().value
//        case _ => -1
//      }
//      assert(v == 1234, s"Did not fine an Expr.Var: $expr")
//    }
//  }
//}