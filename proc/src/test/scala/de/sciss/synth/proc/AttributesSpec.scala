package de.sciss.synth.proc

import de.sciss.lucre.expr.Expr
import de.sciss.ConfluentEventSpec
import de.sciss.lucre

/*
  To run only this suite:

  test-only de.sciss.synth.proc.AttributesSpec

  */
class AttributesSpec extends ConfluentEventSpec {
  import imp._

  "Attrs" should "serialize and de-serialize" in { system =>
    val pH = system.step { implicit tx =>
      val p = Proc[S]
      p.attributes.put("foo", Elem.Int(lucre.expr.Int.newVar(1234)))
      tx.newHandle(p)
    }

    system.step { implicit tx =>
      val p     = pH()
      // println(s"Keys found: ${p.attributes.keys.mkString(", ")}")
      val expr  = p.attributes[Elem.Int]("foo")
      val v     = expr match {
        case Some(Expr.Var(vr)) => vr().value
        case _ => -1
      }
      assert(v == 1234, s"Did not fine an Expr.Var: $expr")
    }
  }
}