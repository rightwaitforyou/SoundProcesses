/*
 *  AuralObjImpl.scala
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
package impl

import de.sciss.synth.proc.AuralObj.{State, Factory}
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.{event => evt, stm}
import de.sciss.model.Model.Listener
import de.sciss.processor.GenericProcessor
import de.sciss.processor.Processor.Update
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth.proc
import de.sciss.synth.proc.Obj.T

import scala.concurrent.{Future, ExecutionContext, CanAwait}
import scala.concurrent.duration.Duration
import scala.util.{Success, Try}

object AuralObjImpl {
  import proc.{Proc => _Proc, Folder => _Folder, Timeline => _Timeline}

  private val sync = new AnyRef

  def addFactory(f: Factory): Unit = sync.synchronized {
    val tid = f.typeID
    if (map.contains(tid)) throw new IllegalArgumentException(s"View factory for type $tid already installed")
    map += tid -> f
  }

  def factories: Iterable[Factory] = map.values

  def apply[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj[S] = {
    val tid = obj.elem.typeID
    map.get(tid).fold(Generic(obj))(f => f(obj.asInstanceOf[Obj.T[S, f.E]]))
  }

  private var map = scala.Predef.Map[Int, Factory](
    // AudioGrapheme   .typeID -> AudioGrapheme,
    // Folder          .typeID -> Folder,
    Proc            .typeID -> AuralObj.Proc, // AuralProcImpl
    Timeline        .typeID -> AuralObj.Timeline
    // Code            .typeID -> Code,
  )

  // -------- Generic --------

  object Generic {
    def apply[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx): AuralObj[S] =
      new Impl(tx.newHandle(obj))

    //    private object dummyPrep extends GenericProcessor[Unit] {
    //      private val peer = Future.successful(())
    //
    //      def abort(): Unit = ()
    //      def progress: Double = 1.0
    //
    //      def removeListener(pf: Listener[Update[Unit, GenericProcessor[Unit]]]): Unit    = ()
    //      def addListener   (pf: Listener[Update[Unit, GenericProcessor[Unit]]]): pf.type = pf
    //
    //      def isCompleted: Boolean = peer.isCompleted
    //
    //      def onComplete[U](f: Try[Unit] => U)(implicit executor: ExecutionContext): Unit =
    //        peer.onComplete(f)
    //
    //      def value: Option[Try[Unit]] = peer.value
    //
    //      def result(atMost: Duration)(implicit permit: CanAwait): Unit = peer.result(atMost)
    //      def ready (atMost: Duration)(implicit permit: CanAwait): this.type = {
    //        peer.ready(atMost)
    //        this
    //      }
    //    }

    private final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj[S]])
      extends AuralObj[S] with DummyObservableImpl[S] {

      def typeID: Int = 0

      def isPrepared(implicit tx: S#Tx): Boolean = true

      def play(timeRef: TimeRef)(implicit tx: S#Tx): Unit = ()
      def stop(/* time: Long */)(implicit tx: S#Tx): Unit = ()

      // def latencyEstimate(implicit tx: S#Tx): Long = 0L

      def prepare()(implicit tx: S#Tx): Unit = () // Generic.dummyPrep

      def dispose()(implicit tx: S#Tx): Unit = ()

      def state(implicit tx: S#Tx): AuralObj.State = AuralObj.Stopped
    }
  }

//  // -------- AudioGrapheme --------
//
//  object AudioGrapheme extends Factory {
//    type E[S <: evt.Sys[S]] = AudioGraphemeElem[S]
//
//    def typeID = ElemImpl.AudioGrapheme.typeID
//
//    def apply[S <: Sys[S]](obj: T[S, E])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj[S] = ...
//  }
//
//  // -------- Folder --------
//
//  object Folder extends Factory {
//    type E[S <: evt.Sys[S]] = FolderElem[S]
//
//    def typeID = FolderElemImpl.typeID
//
//    def apply[S <: Sys[S]](obj: T[S, E])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj[S] = ...
//  }
}