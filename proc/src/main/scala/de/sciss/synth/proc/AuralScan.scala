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
  def apply[S <: Sys[S]](view: AuralObj.Proc[S], key: String, scan: Scan[S], numChannels: Int)
                        (implicit tx: S#Tx, context: AuralContext[S]): AuralScan[S] =
    Impl(view = view, key = key, scan = scan, numChannels = numChannels)

  trait Owned[S <: Sys[S]] extends AuralScan[S] {
    def node_=(value: Option[NodeRef])(implicit tx: S#Tx): Unit
  }

  //  sealed trait Update[S <: Sys[S]] {
  //    def view: AuralScan[S]
  //  }
  //  case class NodeChanged[S <: Sys[S]](view: AuralScan[S], change: Change[Option[NodeRef]]) extends Update[S]
}
trait AuralScan[S <: Sys[S]] extends Disposable[S#Tx] /* with Observable[S#Tx, AuralScan.Update[S]] */ {
  // def numChannels(implicit tx: S#Tx): Int
  // def numChannels_=(value: Int)(implicit tx: S#Tx): Unit

  def key: String

  def bus: AudioBus

  def addSource   (view: AuralScan[S])(implicit tx: S#Tx): Unit
  def addSink     (view: AuralScan[S])(implicit tx: S#Tx): Unit

  def removeSource(view: AuralScan[S])(implicit tx: S#Tx): Unit
  def removeSink  (view: AuralScan[S])(implicit tx: S#Tx): Unit

  def node(implicit tx: S#Tx): Option[NodeRef]

  //  def sourceUpdated(view: AuralScan[S])(implicit tx: S#Tx): Unit
  //  def sinkUpdated  (view: AuralScan[S])(implicit tx: S#Tx): Unit
}