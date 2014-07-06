/*
 *  AuralObj.scala
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

import de.sciss.lucre.event.Observable
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.synth.{NodeRef, AudioBus, Sys}
import de.sciss.lucre.{event => evt, stm}
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth.proc
import de.sciss.synth.proc.Obj.T
import language.higherKinds
import de.sciss.synth.proc.impl.{AuralObjImpl => Impl, AuralProcImpl}

object AuralObj {
  import proc.{Proc => _Proc}

  trait Factory {
    def typeID: Int

    type E[~ <: evt.Sys[~]] <: Elem[~]

    def apply[S <: Sys[S]](obj: Obj.T[S, E])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj[S]
  }

  def addFactory(f: Factory): Unit = Impl.addFactory(f)

  def factories: Iterable[Factory] = Impl.factories

  def apply[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj[S] = Impl(obj)

  // --------------

  trait ProcData[S <: Sys[S]] extends Disposable[S#Tx] {
    def obj: stm.Source[S#Tx, Obj.T[S, _Proc.Elem]]

    def nodeOption(implicit tx: S#Tx): Option[NodeRef]

    /** Queries the number of channel associated with a scanned input.
      * Throws a control throwable when no value can be determined, making
      * the ugen graph builder mark the querying graph element as incomplete
      * (missing information).
      *
      * @param key          the scan key
      * @param numChannels  a given number of channels if `>= 0`, or `-1` to accept whatever the scan in provides
      *
      * @return             the number of channels for the scan input at the given time
      */
    def scanInNumChannels(key: String, numChannels: Int)(implicit tx: S#Tx): Int

    /** Queries the number of channels associated with an attribute input.
      * @param key          the attribute key
      *
      * @return             the number of channels for the attribute input
      */
    def attrNumChannels(key: String)(implicit tx: S#Tx): Int

    def state(implicit tx: S#Tx): UGenGraphBuilder.State[S]

    def procCached()(implicit tx: S#Tx): Obj.T[S, _Proc.Elem]

    def scanInBusChanged(key: String, bus: AudioBus)(implicit tx: S#Tx): Unit

    def getScanBus(key: String)(implicit tx: S#Tx): Option[AudioBus]

    //    def getScanInBus (key: String)(implicit tx: S#Tx): Option[AudioBus]
    //    def getScanOutBus(key: String)(implicit tx: S#Tx): Option[AudioBus]

    def addInstanceView   (view: AuralObj.Proc[S])(implicit tx: S#Tx): Unit
    def removeInstanceView(view: AuralObj.Proc[S])(implicit tx: S#Tx): Unit

    def addInstanceNode   (n: NodeRef)(implicit tx: S#Tx): Unit
    def removeInstanceNode(n: NodeRef)(implicit tx: S#Tx): Unit
  }

  object Proc extends AuralObj.Factory {
    type E[S <: evt.Sys[S]] = _Proc.Elem[S]

    def typeID = _Proc.typeID

    def apply[S <: Sys[S]](obj: _Proc.Obj[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Proc[S] =
      AuralProcImpl(obj)
  }
  trait Proc[S <: Sys[S]] extends AuralObj[S] {
    // def data: ProcData[S]
    override def obj: stm.Source[S#Tx, _Proc.Obj[S]]

    def targetState(implicit tx: S#Tx): AuralObj.State
  }

  sealed trait State
  case object Stopped   extends State
  case object Preparing extends State
  case object Prepared  extends State
  case object Playing   extends State
}
trait AuralObj[S <: Sys[S]] extends Observable[S#Tx, AuralObj.State] with Disposable[S#Tx] {
  def typeID: Int

  /** The view must store a handle to its underlying model. */
  def obj: stm.Source[S#Tx, Obj[S]]

  // def latencyEstimate(implicit tx: S#Tx): Long

  // def prepare()(implicit tx: S#Tx): Unit // GenericProcessor[Unit]

  // def isPrepared(implicit tx: S#Tx): Boolean

  def state(implicit tx: S#Tx): AuralObj.State

  def play(/* time: SpanLike = Span.Void */)(implicit tx: S#Tx): Unit
  def stop(/* time: Long                 */)(implicit tx: S#Tx): Unit
}