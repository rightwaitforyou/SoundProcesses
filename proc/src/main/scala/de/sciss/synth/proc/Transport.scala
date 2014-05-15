/*
 *  Transport.scala
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

package de.sciss.synth
package proc

import de.sciss.lucre.{bitemp, stm, data}
import bitemp.BiGroup
import stm.{Disposable, Cursor}
import collection.immutable.{IndexedSeq => Vec}
import data.Iterator
import impl.{TransportImpl => Impl}
import de.sciss.span.SpanLike
import de.sciss.lucre.event.{Observable, Sys}
import proc.{Proc => _Proc}
import de.sciss.span.Span.SpanOrVoid

object Transport {
  /** Creates a new realtime transport. The transport is positioned at time zero. */
  def apply[S <: Sys[S], I <: stm.Sys[I]](group: ProcGroup[S], sampleRate: Double = 44100)
                                         (implicit tx: S#Tx, cursor: Cursor[S],
                                          bridge: S#Tx => I#Tx)
  : Realtime[S, Obj.T[S, _Proc.Elem], Transport.Proc.Update[S]] = Impl[S, I](group, sampleRate)

  //   implicit def serializer[ S <: evt.Sys[ S ]]( implicit cursor: Cursor[ S ]): Serializer[ S#Tx, S#Acc, ProcTransport[ S ]] =
  //      Impl.serializer( cursor )

  sealed trait Update[S <: Sys[S], Elem, U] {
    def transport: Transport[S, Elem, U]

    def time: Long
  }

  /** Creates a new offline transport. The transport is not positioned. */
  def offline[S <: Sys[S], I <: stm.Sys[I]](group: ProcGroup[S], sampleRate: Double = 44100)
                                           (implicit tx: S#Tx, cursor: Cursor[S], bridge: S#Tx => I#Tx)
  : Offline[S, Obj.T[S, _Proc.Elem], Transport.Proc.Update[S]] = Impl.offline[S, I](group, sampleRate)

  /** A transport sub-type which does not automatically advance in accordance
    * to a real-time clock, but awaits manually stepping through. This can be
    * used for offline-bouncing, debugging or unit testing purposes.
    */
  trait Offline[S <: Sys[S], Elem, U] extends Transport[S, Elem, U] {
    /** Advances the transport to the next position (if there is any) */
    def step()(implicit tx: S#Tx): Unit

    /** Queries the logical time target of the next step.
      *
      * @return the logical time in sample frames at which the next event occurs, or `None` if there
      *         are no further events. If the offline logical clock has never been elapsed
      *         (by calling `elapse`), its base is zero, and therefore the number of frames returned
      *         by this method are the number of frames from the beginning of the timeline.
      */
    def stepTarget(implicit tx: S#Tx): Option[Long]

    /** Advances the offline logical clock by a given amount of seconds.
      * This is important if the objects of the group being transported change,
      * as their change will be associated with the offline logical clock.
      * For a bouncing operation, this method should not be used.
      */
    def elapse(seconds: Double)(implicit tx: S#Tx): Unit

    /** Last frame position at which the transport stopped to evaluate an event. */
    def position(implicit tx: S#Tx): Long
  }

  trait Realtime[S <: Sys[S], Elem, U] extends Transport[S, Elem, U] {
    def loop  (implicit tx: S#Tx): SpanOrVoid
    def loop_=(value: SpanOrVoid)(implicit tx: S#Tx): Unit
  }

  final case class Advance[S <: Sys[S], Elem, U](transport: Transport[S, Elem, U], time: Long,
                                                 isSeek: Boolean, isPlaying: Boolean,
                                                 added:   Vec[ BiGroup.TimedElem[S, Elem]]      = Vec.empty,
                                                 removed: Vec[ BiGroup.TimedElem[S, Elem]]      = Vec.empty,
                                                 changes: Vec[(BiGroup.TimedElem[S, Elem], U)]  = Vec.empty)
    extends Update[S, Elem, U] {

    override def toString = {
      val addedSt   = if (added  .nonEmpty) added  .mkString(", added = [",   ",", "]") else ""
      val removedSt = if (removed.nonEmpty) removed.mkString(", removed = [", ",", "]") else ""
      val changesSt = if (changes.nonEmpty) changes.mkString(", changes = [", ",", "]") else ""

      s"Advance($transport, $time, isSeek = $isSeek, isPlaying = $isPlaying$addedSt$removedSt$changesSt)"
    }
  }

  final case class Play[S <: Sys[S], Elem, U](transport: Transport[S, Elem, U], time: Long) extends Update[S, Elem, U]
  final case class Stop[S <: Sys[S], Elem, U](transport: Transport[S, Elem, U], time: Long) extends Update[S, Elem, U]

  // particular update for ProcTransport
  object Proc {
    sealed trait Update[S <: Sys[S]]

    final case class AttrChanged     [S <: Sys[S]](peer: Obj.AttrUpdate[S])                 extends Update[S]
    final case class ElemChanged     [S <: Sys[S]](peer: _Proc.Change[S])                   extends Update[S]
    final case class GraphemesChanged[S <: Sys[S]](map: Map[String, Vec[Grapheme.Segment]]) extends Update[S]
  }
}

trait Transport[S <: Sys[S], Elem, U] extends Disposable[S#Tx] with Observable[S#Tx, Transport.Update[S, Elem, U]] {

  def play()(implicit tx: S#Tx): Unit
  def stop()(implicit tx: S#Tx): Unit

  def seek(time: Long)(implicit tx: S#Tx): Unit
  def time(implicit tx: S#Tx): Long

  def isPlaying(implicit tx: S#Tx): Boolean

  def sampleRate: Double

  /** Iterator over all processes which intersect with the current time. */
  def iterator(implicit tx: S#Tx): Iterator[S#Tx, (SpanLike, BiGroup.TimedElem[S, Elem])]

  def cursor: stm.Cursor[S]
}