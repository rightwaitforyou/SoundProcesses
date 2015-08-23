package de.sciss.synth.proc

import de.sciss.lucre.artifact.ArtifactLocation
import de.sciss.lucre.expr
import de.sciss.lucre.expr.Expr
import de.sciss.ConfluentEventSpec
import de.sciss.lucre
import de.sciss.synth.Curve
import scala.collection.immutable.{IndexedSeq => Vec}
import de.sciss.file._
import de.sciss.synth.io.AudioFileSpec

import TransitoryAPI._

/*
  To run only this suite:

  test-only de.sciss.synth.proc.AttributesSpec

  */
class AttributesSpec extends ConfluentEventSpec {
  import imp._

  "Attrs" should "serialize and de-serialize" in { system =>
    val pH = system.step { implicit tx =>
      val p     = Proc[S]
      val obj   = p // Obj(peer)
      import expr.Ops._
      obj.attrPut("foo", lucre.expr.Int.newVar(1234))
      tx.newHandle(obj)
    }

    system.step { implicit tx =>
      val p     = pH()
      // println(s"Keys found: ${p.attributes.keys.mkString(", ")}")
      val expr  = p.attr[Expr[S, Int]]("foo")
      val v     = expr match {
        case Some(Expr.Var(vr)) => vr().value
        case _ => -1
      }
      assert(v == 1234, s"Did not find an Expr.Var: $expr")
    }

    // ---- now for other types ----

    val fade = FadeSpec(1234L, Curve.parametric(7.8f), 0.56f)
    val spec = AudioFileSpec(numChannels = 3, numFrames = 45L, sampleRate = 678.9)

    val pgH = system.step { implicit tx =>
      val p = pH()
      p.attrPut("int"    , lucre.expr.Int    .newConst(5678 ))
      val d = lucre.expr.Double .newConst[S](123.4)
      p.attrPut("double" , d)
      val n = lucre.expr.Long   .newConst[S](1234L)
      p.attrPut("long"   , n)
      p.attrPut("boolean", lucre.expr.Boolean.newConst(true ))
      p.attrPut("string" , lucre.expr.String .newConst("123"))

      p.attrPut("fade"   , FadeSpec.Expr.newConst(fade))
      p.attrPut("d-vec"  , DoubleVec.newConst(Vec(1.2, 3.4, 5.6)))
      val loc = ArtifactLocation[S](file("foo"))
      val art = loc.add(file("foo") / "bar")
      p.attrPut("audio"  , Grapheme.Expr.Audio(art, spec, offset = n, gain = d))
      p.attrPut("loc",     loc)
      val group = Timeline[S]
      p.attrPut("group",   group)
      // implicit val groupSer = ProcGroup.Modifiable.serializer[S]
      tx.newHandle(group)
    }

    system.step { implicit tx =>
      val p = pH()
      assert(p.attr[Expr[S, Int    ]]("int"    ).map(_.value) === Some(5678))
      assert(p.attr[Expr[S, Double ]]("double" ).map(_.value) === Some(123.4))
      assert(p.attr[Expr[S, Long   ]]("long"   ).map(_.value) === Some(1234L))
      assert(p.attr[Expr[S, Boolean]]("boolean").map(_.value) === Some(true))
      assert(p.attr[Expr[S, String ]]("string" ).map(_.value) === Some("123"))
      assert(p.attr[FadeSpec.Expr[S]]("fade").map(_.value) === Some(fade))
      assert(p.attr[Expr[S, Vec[Double]]]("d-vec").map(_.value) === Some(Vec(1.2, 3.4, 5.6)))
      assert(p.attr[Grapheme.Expr.Audio[S]]("audio").map(_.value) ===
        Some(Grapheme.Value.Audio(file("foo") / "bar", spec, 1234L, 123.4)))
      assert(p.attr[ArtifactLocation[S]]("loc").map(_.directory) === Some(file("foo")))
      val group = pgH()
      assert(p.attr[Timeline[S]]("group") === Some(group))
    }
  }
}
