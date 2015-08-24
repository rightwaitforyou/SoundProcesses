package de.sciss.synth.proc

import de.sciss.ConfluentEventSpec
import de.sciss.file._
import de.sciss.lucre.artifact.ArtifactLocation
import de.sciss.lucre.expr.{DoubleVector, BooleanObj, DoubleObj, IntObj, LongObj, StringObj}
import de.sciss.synth.Curve
import de.sciss.synth.io.AudioFileSpec
import de.sciss.synth.proc.TransitoryAPI._

/*
  To run only this suite:

  test-only de.sciss.synth.proc.AttributesSpec

  */
class AttributesSpec extends ConfluentEventSpec {
  // import imp._
  // import ExprImplicits._

  "Attrs" should "serialize and de-serialize" in { system =>
    val pH = system.step { implicit tx =>
      val p     = Proc[S]
      val obj   = p // Obj(peer)
      obj.attrPut("foo", IntObj.newVar[S](1234))
      tx.newHandle(obj)
    }

    system.step { implicit tx =>
      val p     = pH()
      // println(s"Keys found: ${p.attributes.keys.mkString(", ")}")
      val expr  = p.attr[IntObj]("foo")
      val v     = expr match {
        case Some(IntObj.Var(vr)) => vr().value
        case _ => -1
      }
      assert(v == 1234, s"Did not find an Expr.Var: $expr")
    }

    // ---- now for other types ----

    val fade = FadeSpec(1234L, Curve.parametric(7.8f), 0.56f)
    val spec = AudioFileSpec(numChannels = 3, numFrames = 45L, sampleRate = 678.9)

    val pgH = system.step { implicit tx =>
      val p = pH()
      p.attrPut("int"    , IntObj.newConst[S](5678 ))
      val d = DoubleObj.newConst[S](123.4)
      p.attrPut("double" , d)
      val n = LongObj.newConst[S](1234L)
      p.attrPut("long"   , n)
      p.attrPut("boolean", BooleanObj.newConst[S](true ))
      p.attrPut("string" , StringObj .newConst[S]("123"))

      p.attrPut("fade"   , FadeSpec.Obj.newConst(fade))
      p.attrPut("d-vec"  , DoubleVector.newConst(Vector(1.2, 3.4, 5.6)))
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
      assert(p.attr[IntObj      ]("int"    ).map(_.value) === Some(5678))
      assert(p.attr[DoubleObj   ]("double" ).map(_.value) === Some(123.4))
      assert(p.attr[LongObj     ]("long"   ).map(_.value) === Some(1234L))
      assert(p.attr[BooleanObj  ]("boolean").map(_.value) === Some(true))
      assert(p.attr[StringObj   ]("string" ).map(_.value) === Some("123"))
      assert(p.attr[FadeSpec.Obj]("fade").map(_.value) === Some(fade))
      assert(p.attr[DoubleVector]("d-vec").map(_.value) === Some(Vector(1.2, 3.4, 5.6)))
      assert(p.attr[Grapheme.Expr.Audio]("audio").map(_.value) ===
        Some(Grapheme.Value.Audio(file("foo") / "bar", spec, 1234L, 123.4)))
      assert(p.attr[ArtifactLocation]("loc").map(_.directory) === Some(file("foo")))
      val group = pgH()
      assert(p.attr[Timeline]("group") === Some(group))
    }
  }
}
