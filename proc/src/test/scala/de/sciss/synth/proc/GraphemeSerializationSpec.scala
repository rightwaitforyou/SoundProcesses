package de.sciss
package synth
package proc

import collection.immutable.{IndexedSeq => Vec}
import span.Span
import de.sciss.synth.Curve.step

/**
 * To test only this suite:
 *
 * test-only de.sciss.synth.proc.GraphemeSerializationSpec
 */
class GraphemeSerializationSpec extends ConfluentEventSpec {
  "Grapheme" should "serialize and deserialize" in { system =>
    val gH = system.step { implicit tx =>
      val g = Grapheme.Modifiable[S]
      tx.newHandle(g)(Grapheme.Modifiable.serializer[S])
    }

    import imp._

    system.step { implicit tx =>
      val g = gH()
      g.add(1234L -> Grapheme.Value.Curve(5678.9 -> step))
    }

    system.step { implicit tx =>
      val g = gH()
      assert(g.segment(0L) === None)
      assert(g.segment(2222L) === Some(Grapheme.Segment.Const(Span.from(1234L), Vec(5678.9))))
    }
  }
}
