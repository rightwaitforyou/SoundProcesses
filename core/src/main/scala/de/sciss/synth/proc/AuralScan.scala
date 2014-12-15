/*
 *  AuralScan.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2014 Hanns Holger Rutz. All rights reserved.
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
import impl.{AuralScanImpl => Impl}

object AuralScan {
  sealed trait Proxy[S <: Sys[S]]
  final class Incomplete[S <: Sys[S]](val data: AuralObj.ProcData[S], val key: String) extends Proxy[S] {
    override def toString = s"Proxy($data, $key)"
  }

  /** Creates a new aural scan view and registers it with the context under `scan.id`. */
  def apply[S <: Sys[S]](data: AuralObj.ProcData[S], key: String, scan: Scan[S], bus: AudioBus)
                        (implicit tx: S#Tx, context: AuralContext[S]): AuralScan.Owned[S] =
    Impl(data = data, key = key, scan = scan, bus = bus)

  trait Owned[S <: Sys[S]] extends AuralScan[S] {

    def stop()(implicit tx: S#Tx): Unit
    def play(n: NodeRef)(implicit tx: S#Tx): Unit
  }
}
trait AuralScan[S <: Sys[S]] extends AuralScan.Proxy[S] with Disposable[S#Tx] {
  // def numChannels(implicit tx: S#Tx): Int
  // def numChannels_=(value: Int)(implicit tx: S#Tx): Unit

  def key: String

  def bus: AudioBus

  def addSource   (view: AuralScan[S])(implicit tx: S#Tx): Unit
  def addSink     (view: AuralScan[S])(implicit tx: S#Tx): Unit

  def removeSource(view: AuralScan[S])(implicit tx: S#Tx): Unit
  def removeSink  (view: AuralScan[S])(implicit tx: S#Tx): Unit

  def data: AuralObj.ProcData[S]

  // def sourceUpdated(view: AuralScan[S])(implicit tx: S#Tx): Unit
  def sinkStopped (view: AuralScan[S])(implicit tx: S#Tx): Unit
  def sinkPlaying (view: AuralScan[S])(implicit tx: S#Tx): Unit
}