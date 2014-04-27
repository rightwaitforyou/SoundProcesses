package de.sciss.synth.proc

import de.sciss.lucre.expr.Expr
import de.sciss.ConfluentEventSpec
import de.sciss.lucre
import de.sciss.synth.Curve
import de.sciss.lucre.synth.expr.DoubleVec
import scala.collection.immutable.{IndexedSeq => Vec}
import de.sciss.file._
import de.sciss.synth.io.AudioFileSpec

/*
  To run only this suite:

  test-only de.sciss.synth.proc.AttributesSpec

  */
class AttributesSpec extends ConfluentEventSpec {
  import imp._

  "Attrs" should "serialize and de-serialize" in { system =>
    val pH = system.step { implicit tx =>
      val p     = Proc[S]
      val peer  = ProcElem(p)
      val obj   = Obj(peer)
      obj.attr.put("foo", IntElem(lucre.expr.Int.newVar(1234)))
      tx.newHandle(obj)
    }

    system.step { implicit tx =>
      val p     = pH()
      // println(s"Keys found: ${p.attributes.keys.mkString(", ")}")
      val expr  = p.attr.expr[Int]("foo")
      val v     = expr match {
        case Some(Expr.Var(vr)) => vr().value
        case _ => -1
      }
      assert(v == 1234, s"Did not find an Expr.Var: $expr")
    }

    // ---- now for other types ----

    val fade = FadeSpec.Value(1234L, Curve.parametric(7.8f), 0.56f)
    val spec = AudioFileSpec(numChannels = 3, numFrames = 45L, sampleRate = 678.9)

    val pgH = system.step { implicit tx =>
      val p = pH()
      p.attr.put("int"    , IntElem    (lucre.expr.Int    .newConst(5678 )))
      val d = lucre.expr.Double .newConst[S](123.4)
      p.attr.put("double" , DoubleElem (d))
      val n = lucre.expr.Long   .newConst[S](1234L)
      p.attr.put("long"   , LongElem   (n))
      p.attr.put("boolean", BooleanElem(lucre.expr.Boolean.newConst(true )))
      p.attr.put("string" , StringElem (lucre.expr.String .newConst("123")))

      p.attr.put("fade"   , FadeSpecElem(FadeSpec.Elem.newConst(fade)))
      p.attr.put("d-vec"  , DoubleVecElem(DoubleVec.newConst(Vec(1.2, 3.4, 5.6))))
      val loc = Artifact.Location.Modifiable[S](file("foo"))
      val art = loc.add(file("foo") / "bar")
      p.attr.put("audio"  , AudioGraphemeElem(Grapheme.Elem.Audio(art, spec, offset = n, gain = d)))
      p.attr.put("loc",     ArtifactLocationElem(loc))
      val group = ProcGroup.Modifiable[S]
      p.attr.put("group",   ProcGroupElem(group))
      implicit val groupSer = ProcGroup.Modifiable.serializer[S]
      tx.newHandle(group)
    }

    system.step { implicit tx =>
      val p = pH()
      assert(p.attr.expr[Int    ]("int"    ).map(_.value) === Some(5678))
      assert(p.attr.expr[Double ]("double" ).map(_.value) === Some(123.4))
      assert(p.attr.expr[Long   ]("long"   ).map(_.value) === Some(1234L))
      assert(p.attr.expr[Boolean]("boolean").map(_.value) === Some(true))
      assert(p.attr.expr[String ]("string" ).map(_.value) === Some("123"))
      assert(p.attr.expr[FadeSpec.Value]("fade").map(_.value) === Some(fade))
      assert(p.attr.expr[Vec[Double]]("d-vec").map(_.value) === Some(Vec(1.2, 3.4, 5.6)))
      assert(p.attr.expr[Grapheme.Value.Audio]("audio").map(_.value) ===
        Some(Grapheme.Value.Audio(file("foo") / "bar", spec, 1234L, 123.4)))
      assert(p.attr[Artifact.Location]("loc").map(_.directory) === Some(file("foo")))
      val group = pgH()
      assert(p.attr[ProcGroup]("group") === Some(group))
    }
  }
}