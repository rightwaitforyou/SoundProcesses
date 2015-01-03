/*
 *  AuralObj.scala
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

import de.sciss.lucre.event.Observable
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.synth.{AudioBus, NodeRef, Sys}
import de.sciss.lucre.{stm, event => evt}
import de.sciss.synth.proc.impl.{AuralActionImpl, AuralEnsembleImpl, AuralProcImpl, AuralTimelineImpl, AuralObjImpl => Impl}

import scala.language.higherKinds

object AuralObj {
  import de.sciss.synth.proc.{Action => _Action, Ensemble => _Ensemble, Proc => _Proc, Timeline => _Timeline}

  trait Factory {
    def typeID: Int

    type E[~ <: evt.Sys[~]] <: Elem[~]

    def apply[S <: Sys[S]](obj: Obj.T[S, E])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj[S]
  }

  def addFactory(f: Factory): Unit = Impl.addFactory(f)

  def factories: Iterable[Factory] = Impl.factories

  def apply[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj[S] = Impl(obj)

  sealed trait State
  case object Stopped   extends State
  case object Preparing extends State
  case object Prepared  extends State
  case object Playing   extends State

  // -------------- sub-types --------------

  // ---- proc ----

  trait ProcData[S <: Sys[S]] extends Disposable[S#Tx] {
    def obj: stm.Source[S#Tx, _Proc.Obj[S]]

    /** The node reference associated with the process. A `Some` value indicates that
      * at least one instance view is playing, whereas a `None` value indicates that
      * there is no actively playing instance view at the moment.
      */
    def nodeOption(implicit tx: S#Tx): Option[NodeRef]

    def getScan(key: String)(implicit tx: S#Tx): Option[Either[AudioBus, AuralScan[S]]]

    //    /** Queries the number of channel associated with a scanned input.
    //      * Throws a control throwable when no value can be determined, making
    //      * the ugen graph builder mark the querying graph element as incomplete
    //      * (missing information).
    //      *
    //      * @param key          the scan key
    //      * @param numChannels  a given number of channels if `>= 0`, or `-1` to accept whatever the scan in provides
    //      *
    //      * @return             the number of channels for the scan input at the given time
    //      */
    //    def scanInNumChannels(key: String, numChannels: Int)(implicit tx: S#Tx): Int

    //    /** Queries the number of channels associated with an attribute input.
    //      * @param key          the attribute key
    //      *
    //      * @return             the number of channels for the attribute input
    //      */
    //    def attrNumChannels(key: String)(implicit tx: S#Tx): Int

    def state(implicit tx: S#Tx): UGenGraphBuilder.State[S]

    /* The proc object may be needed multiple times during a transaction.
     * The data instance is thus asked to provide a transaction-local
     * cache for resolving the proc from its `stm.Source`.
     */
    def procCached()(implicit tx: S#Tx): _Proc.Obj[S]

    // def scanInBusChanged(key: String, bus: AudioBus)(implicit tx: S#Tx): Unit

    def getScanBus(key: String)(implicit tx: S#Tx): Option[AudioBus]

    //    def getScanInBus (key: String)(implicit tx: S#Tx): Option[AudioBus]
    //    def getScanOutBus(key: String)(implicit tx: S#Tx): Option[AudioBus]

    def addInstanceView   (view: AuralObj.Proc[S])(implicit tx: S#Tx): Unit
    def removeInstanceView(view: AuralObj.Proc[S])(implicit tx: S#Tx): Unit

    def addInstanceNode   (n: NodeRef.Full)(implicit tx: S#Tx): Unit
    def removeInstanceNode(n: NodeRef.Full)(implicit tx: S#Tx): Unit

    //    /** Converts an attribute key and a value, given as an `Elem`, to a
    //      * control-set entry for a synth. Currently throws an exception if
    //      * the attribute value cannot be cast into a scalar control value.
    //      *
    //      * A scalar audio grapheme is not supported right now.
    //      */
    //    def attrControlSet(key: String, value: Elem[S])(implicit tx: S#Tx): ControlSet

    def buildAttrInput(b: NodeDependencyBuilder[S], key: String, value: UGenGraphBuilder.Value)
                      (implicit tx: S#Tx): Unit

      // called from scan-view if source is not materialized yet
    def sinkAdded(key: String, view: AuralScan[S])(implicit tx: S#Tx): Unit

    implicit def context: AuralContext[S]
  }

  object Proc extends AuralObj.Factory {
    type E[S <: evt.Sys[S]] = _Proc.Elem[S]

    def typeID = _Proc.typeID

    def apply[S <: Sys[S]](obj: _Proc.Obj[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Proc[S] =
      AuralProcImpl(obj)
  }
  trait Proc[S <: Sys[S]] extends AuralObj[S] {
    def data: ProcData[S]

    override def obj: stm.Source[S#Tx, _Proc.Obj[S]]

    def targetState(implicit tx: S#Tx): AuralObj.State

    private[proc] def stopForRebuild  ()(implicit tx: S#Tx): Unit
    private[proc] def playAfterRebuild()(implicit tx: S#Tx): Unit
  }

  // ---- timeline ----

  object Timeline extends AuralObj.Factory {
    type E[S <: evt.Sys[S]] = _Timeline.Elem[S]

    def typeID = _Timeline.typeID

    def apply[S <: Sys[S]](obj: _Timeline.Obj[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Timeline[S] =
      AuralTimelineImpl(obj)

    sealed trait Update[S <: Sys[S]] {
      def timeline: Timeline[S]
    }

    final case class ViewAdded[S <: Sys[S]](timeline: Timeline[S], timed: S#ID, view: AuralObj[S])
      extends Update[S]

    final case class ViewRemoved[S <: Sys[S]](timeline: Timeline[S], view: AuralObj[S])
      extends Update[S]
  }
  trait Timeline[S <: Sys[S]] extends AuralObj[S] {
    override def obj: stm.Source[S#Tx, _Timeline.Obj[S]]

    /** Monitors the _active_ views, i.e. views which are
      * intersecting with the current transport position.
      */
    def contents: Observable[S#Tx, Timeline.Update[S]]

    /** Returns the set of _active_ views, i.e. views which are intersecting
      * with the current transport position.
      */
    def views(implicit tx: S#Tx): Set[AuralObj[S]]

    def getView(timed: _Timeline.Timed[S])(implicit tx: S#Tx): Option[AuralObj[S]]
  }

  // ---- ensemble ----

  object Ensemble extends AuralObj.Factory {
    type E[S <: evt.Sys[S]] = _Ensemble.Elem[S]

    def typeID = _Ensemble.typeID

    def apply[S <: Sys[S]](obj: _Ensemble.Obj[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Ensemble[S] =
      AuralEnsembleImpl(obj)
  }
  trait Ensemble[S <: Sys[S]] extends AuralObj[S] {
    override def obj: stm.Source[S#Tx, _Ensemble.Obj[S]]

    def views(implicit tx: S#Tx): Set[AuralObj[S]]
  }

  // ---- action ----

  object Action extends AuralObj.Factory {
    type E[S <: evt.Sys[S]] = _Action.Elem[S]

    def typeID = _Action.typeID

    def apply[S <: Sys[S]](obj: _Action.Obj[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Action[S] =
      AuralActionImpl(obj)
  }
  trait Action[S <: Sys[S]] extends AuralObj[S] {
    override def obj: stm.Source[S#Tx, _Action.Obj[S]]

    // def views(implicit tx: S#Tx): Set[AuralObj[S]]
  }
}
trait AuralObj[S <: Sys[S]] extends Observable[S#Tx, AuralObj.State] with Disposable[S#Tx] {
  def typeID: Int

  /** The view must store a handle to its underlying model. */
  def obj: stm.Source[S#Tx, Obj[S]]

  // def latencyEstimate(implicit tx: S#Tx): Long

  // def isPrepared(implicit tx: S#Tx): Boolean

  def state(implicit tx: S#Tx): AuralObj.State

  def prepare()                                    (implicit tx: S#Tx): Unit
  def play   (timeRef: TimeRef = TimeRef.Undefined)(implicit tx: S#Tx): Unit
  def stop   (/* time: Long*/                     )(implicit tx: S#Tx): Unit
}