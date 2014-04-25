package de.sciss.synth.proc

import de.sciss.lucre.event.Sys

/** This object should compile and thereby confirm
  * that serializer resolution works for elements and objects.
  */
object TypeInferenceAndImplicitResolution {
  def test[S <: Sys[S]]()(implicit tx: S#Tx): Unit = {
    val graph: Grapheme.Elem.Audio[S] = sys.error("Not necessary for compilation")
    val peer  = AudioGraphemeElem(graph)
    val obj   = Obj(peer)
    val ph    = tx.newHandle(peer)
    val oh    = tx.newHandle(obj)

    val pr: AudioGraphemeElem[S] = ph()
    val or: Obj.T[S, AudioGraphemeElem] = oh()

    println(s"If this compiles, $pr and $or are fine.")
  }
}
