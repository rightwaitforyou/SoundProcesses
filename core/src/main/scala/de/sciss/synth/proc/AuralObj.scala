/*
 *  AuralObj.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import de.sciss.lucre.event.Observable
import de.sciss.lucre.expr.SpanLikeObj
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Obj, Sys, TxnLike}
import de.sciss.lucre.synth.{NodeRef, Sys => SSys}
import de.sciss.synth.proc.impl.{AuralActionImpl, AuralEnsembleImpl, AuralFolderImpl, AuralObjImpl => Impl, AuralProcImpl, AuralTimelineImpl}

import scala.language.higherKinds

object AuralObj {
  import de.sciss.synth.proc.{Action => _Action, Ensemble => _Ensemble, Folder => _Folder, Proc => _Proc, Timeline => _Timeline}

  trait Factory {
    def typeID: Int

    type Repr[~ <: Sys[~]] <: Obj[~]

    def apply[S <: SSys[S]](obj: Repr[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj[S]
  }

  def addFactory(f: Factory): Unit = Impl.addFactory(f)

  def factories: Iterable[Factory] = Impl.factories

  def apply[S <: SSys[S]](obj: Obj[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj[S] = Impl(obj)

//  /* The current state a view is in. */
//  sealed trait State
//  case object Stopped   extends State
//  case object Preparing extends State
//  case object Prepared  extends State
//  case object Playing   extends State

  /* The target state indicates the eventual state the process should have,
     independent of the current state which might not yet be ready.
   */
  sealed trait TargetState {
    def completed: AuralView.State
  }
  case object TargetStop extends TargetState {
    def completed = AuralView.Stopped
  }
  case object TargetPrepared extends TargetState {
    def completed = AuralView.Prepared
  }
  final case class TargetPlaying(wallClock: Long, timeRef: TimeRef) extends TargetState {
    def completed = AuralView.Playing

    def shiftTo(newWallClock: Long): TimeRef = {
      val delta = newWallClock - wallClock
      timeRef.shift(delta)
    }

    override def toString = s"TargetPlaying(wallClock = $wallClock, timeRef = $timeRef)"
  }

  // -------------- sub-types --------------

  // ---- proc ----

  object Proc extends AuralObj.Factory {
    type Repr[S <: Sys[S]] = _Proc[S]

    def typeID = _Proc.typeID

    def apply[S <: SSys[S]](obj: _Proc[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Proc[S] =
      AuralProcImpl(obj)
  }
  trait Proc[S <: Sys[S]] extends AuralObj[S] {
    override def obj: stm.Source[S#Tx, _Proc[S]]

    /** The node reference associated with the process. A `Some` value indicates that
      * at least one instance view is playing, whereas a `None` value indicates that
      * there is no actively playing instance view at the moment.
      */
    def nodeOption(implicit tx: TxnLike): Option[NodeRef]

    def targetState(implicit tx: S#Tx): AuralView.State

    implicit def context: AuralContext[S]
  }

  // ---- timeline ----

  object Timeline extends AuralObj.Factory {
    type Repr[S <: Sys[S]] = _Timeline[S]

    def typeID = _Timeline.typeID

    def apply[S <: SSys[S]](obj: _Timeline[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Timeline[S] =
      AuralTimelineImpl(obj)

    /** Creates an empty view that can be manually populated by calling `addObject`. */
    def empty[S <: SSys[S]](obj: _Timeline[S])(implicit tx: S#Tx, context: AuralContext[S]): Manual[S] =
      AuralTimelineImpl.empty(obj)

    trait Manual[S <: Sys[S]] extends Timeline[S] {
      // def addObject   (timed: _Timeline.Timed[S])(implicit tx: S#Tx): Unit
      // def removeObject(timed: _Timeline.Timed[S])(implicit tx: S#Tx): Unit
      def addObject   (id: S#ID, span: SpanLikeObj[S], obj: Obj[S])(implicit tx: S#Tx): Unit
      def removeObject(id: S#ID, span: SpanLikeObj[S], obj: Obj[S])(implicit tx: S#Tx): Unit
    }

    sealed trait Update[S <: Sys[S]] {
      def timeline: Timeline[S]
    }
    final case class ViewAdded[S <: Sys[S]](timeline: Timeline[S], timed: S#ID, view: AuralObj[S])
      extends Update[S]

    final case class ViewRemoved[S <: Sys[S]](timeline: Timeline[S], view: AuralObj[S])
      extends Update[S]
  }
  trait Timeline[S <: Sys[S]] extends AuralObj[S] {
    override def obj: stm.Source[S#Tx, _Timeline[S]]

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

  object FolderLike {
    sealed trait Update[S <: Sys[S], Repr <: FolderLike[S, Repr]] {
      def parent: Repr
    }
    final case class ViewAdded[S <: Sys[S], Repr <: FolderLike[S, Repr]](parent: Repr, view: AuralObj[S])
      extends Update[S, Repr]

    final case class ViewRemoved[S <: Sys[S], Repr <: FolderLike[S, Repr]](parent: Repr, view: AuralObj[S])
      extends Update[S, Repr]
  }
  trait FolderLike[S <: Sys[S], Repr <: FolderLike[S, Repr]] extends AuralObj[S] {
    def folder(implicit tx: S#Tx): _Folder[S]

    def contents: Observable[S#Tx, FolderLike.Update[S, Repr]]

    def views(implicit tx: S#Tx): Set[AuralObj[S]]

    def getView(obj: Obj[S])(implicit tx: S#Tx): Option[AuralObj[S]]
  }

  object Ensemble extends AuralObj.Factory {
    type Repr[S <: Sys[S]] = _Ensemble[S]

    def typeID = _Ensemble.typeID

    def apply[S <: SSys[S]](obj: _Ensemble[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Ensemble[S] =
      AuralEnsembleImpl(obj)
  }
  trait Ensemble[S <: Sys[S]] extends FolderLike[S, Ensemble[S]] {
    override def obj: stm.Source[S#Tx, _Ensemble[S]]
  }

  // ---- folder ----

  object Folder extends AuralObj.Factory {
    type Repr[S <: Sys[S]] = _Folder[S]

    def typeID = _Folder.typeID

    def apply[S <: SSys[S]](obj: _Folder[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Folder[S] =
      AuralFolderImpl(obj)
  }
  trait Folder[S <: Sys[S]] extends FolderLike[S, Folder[S]] {
    override def obj: stm.Source[S#Tx, _Folder[S]]
  }

  // ---- action ----

  object Action extends AuralObj.Factory {
    type Repr[S <: Sys[S]] = _Action[S]

    def typeID = _Action.typeID

    def apply[S <: SSys[S]](obj: _Action[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Action[S] =
      AuralActionImpl(obj)
  }
  trait Action[S <: Sys[S]] extends AuralObj[S] {
    override def obj: stm.Source[S#Tx, _Action[S]]
  }
}
trait AuralObj[S <: Sys[S]] extends AuralView[S, Unit] {
  def play()(implicit tx: S#Tx): Unit = play(TimeRef.Undefined, ())
}