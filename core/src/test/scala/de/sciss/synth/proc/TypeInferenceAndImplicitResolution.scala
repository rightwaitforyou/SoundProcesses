package de.sciss.synth.proc

import de.sciss.lucre.stm.Sys

/** This object should compile and thereby confirm
  * that serializer resolution works for elements and objects.
  */
object TypeInferenceAndImplicitResolution {
  def test[S <: Sys[S]]()(implicit tx: S#Tx): Unit = {
    val graph: AudioCue.Obj[S] = sys.error("Not necessary for compilation")
    val peer  = graph // AudioGraphemeElem(graph)
    val obj   = peer // Obj(peer)

    val ph    = tx.newHandle(peer)
    val oh    = tx.newHandle(obj)

    val pr: AudioCue.Obj[S] /* AudioGraphemeElem[S] */ = ph()
    val or: AudioCue.Obj[S] /* Obj.T[S, AudioGraphemeElem] */ = oh()

    println(s"If this compiles, $pr and $or are fine.")
  }
}
