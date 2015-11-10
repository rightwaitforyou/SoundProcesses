/*
 *  AuralOutputImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2015 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package impl

import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.stm
import de.sciss.lucre.synth.{AudioBus, NodeRef, Sys}
import de.sciss.synth.proc.AuralObj.ProcData
import de.sciss.synth.proc.{logAural => logA}

object AuralOutputImpl {
  def apply[S <: Sys[S]](data: ProcData[S], output: Output[S], bus: AudioBus)
                        (implicit tx: S#Tx, context: AuralContext[S]): AuralOutput.Owned[S] = {
    val id    = output.id
    val key   = output.key
    val view  = new Impl[S](data = data, key = key, bus = bus, idH = tx.newHandle(id))
    logA(s"AuralOutput(${data.procCached()}, $key, bus = $bus)")
    context.putAux[AuralOutput /* .Proxy */[S]](id, view)
    view
  }

  // ----------------------------------

  // note: it is crucial that we use `stm.Source[S#Tx, S#ID]` instead of just `S#ID`, because if
  // the view is created in the same transaction as the scan, the id's path will be empty, causing
  // an error in `dispose()` when trying to remove the entry from the ID map!
  private final class Impl[S <: Sys[S]](val data: ProcData[S], val key: String, val bus: AudioBus,
                                        idH: stm.Source[S#Tx, S#ID])
    extends AuralOutput.Owned[S] with ObservableImpl[S, AuralOutput.Update] {

    override def toString: String = s"AuralOutput($data, $key, $bus)"

    def play(n: NodeRef)(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      logA(s"AuralOutput play; ${data.procCached()}, $key")
      fire(AuralOutput.Play(n))
    }

    def stop()(implicit tx: S#Tx): Unit = {
      logA(s"AuralOutput stop; ${data.procCached()}, $key")
      fire(AuralOutput.Stop)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      logA(s"AuralOutput dispose; ${data.procCached()}, $key")
      implicit val itx = tx.peer
      data.context.removeAux(idH())
    }
  }
}