/*
 *  AuralOutput.scala
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

import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.synth.{NodeRef, AudioBus, Sys}
import impl.{AuralOutputImpl => Impl}

sealed trait AuralScan[S <: Sys[S]] extends Disposable[S#Tx] {
  def key : String
  def bus : AudioBus
}

object AuralOutput {
  sealed trait Proxy[S <: Sys[S]]
  final class Incomplete[S <: Sys[S]](val data: AuralObj.ProcData[S], val key: String) extends Proxy[S] {
    override def toString = s"Proxy($data, $key)"
  }

  /** Creates a new aural scan view and registers it with the context under `scan.id`. */
  def apply[S <: Sys[S]](data: AuralObj.ProcData[S], output: Output[S], bus: AudioBus)
                        (implicit tx: S#Tx, context: AuralContext[S]): AuralOutput.Owned[S] =
    ??? // SCAN Impl(data = data, key = key, scan = scan, bus = bus, isInput = isInput)

  trait Owned[S <: Sys[S]] extends AuralOutput[S] {

    def stop()(implicit tx: S#Tx): Unit
    def play(n: NodeRef)(implicit tx: S#Tx): Unit
  }
}

trait AuralOutput[S <: Sys[S]] extends AuralScan[S] with AuralOutput.Proxy[S] {
  // def numChannels(implicit tx: S#Tx): Int
  // def numChannels_=(value: Int)(implicit tx: S#Tx): Unit

  def data: AuralObj.ProcData[S]

  def addSink      (view: AuralInput [S])(implicit tx: S#Tx): Unit
  def removeSink   (view: AuralInput [S])(implicit tx: S#Tx): Unit

  def sinkStopped  (view: AuralInput [S])(implicit tx: S#Tx): Unit
  def sinkPlaying  (view: AuralInput [S])(implicit tx: S#Tx): Unit
}

object AuralInput {

}
trait AuralInput[S <: Sys[S]] extends AuralScan[S] {
   def addSource   (view: AuralOutput[S])(implicit tx: S#Tx): Unit
   def removeSource(view: AuralOutput[S])(implicit tx: S#Tx): Unit

  // def sourceUpdated(view: AuralOutput[S])(implicit tx: S#Tx): Unit
}