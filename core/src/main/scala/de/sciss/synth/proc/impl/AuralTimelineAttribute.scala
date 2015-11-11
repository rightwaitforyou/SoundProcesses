/*
 *  AuralTimelineAttribute.scala
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

import de.sciss.lucre.stm
import de.sciss.lucre.stm.{TxnLike, Disposable}
import de.sciss.lucre.synth.{Sys, Txn}
import de.sciss.synth.proc.AuralAttribute.{Factory, Instance, Observer, Target}

import scala.annotation.tailrec
import scala.concurrent.stm.Ref

object AuralTimelineAttribute extends Factory {
  type Repr[S <: stm.Sys[S]] = Timeline[S]

  def typeID = Timeline.typeID

  def apply[S <: Sys[S]](key: String, value: Timeline[S], observer: Observer[S])
                        (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
    new AuralTimelineAttribute(key, observer).init(value)
}
final class AuralTimelineAttribute[S <: Sys[S]](val key: String,
                                              observer: Observer[S])
                                             (implicit context: AuralContext[S])
  extends AuralAttribute[S] with Observer[S] { attr =>

  import TxnLike.peer
  import context.{scheduler => sched}

  private[this] val childAttrRef = Ref.make[Vector[AuralAttribute[S]]]

  private[this] final class PlayTime(val wallClock: Long,
                                     val timeRef: TimeRef.Apply, val target: Target)
    extends Instance {

    def shiftTo(newWallClock: Long): TimeRef.Apply = timeRef.shift(newWallClock - wallClock)

    def dispose()(implicit tx: Txn): Unit = {
      playRef.transform(_.filterNot(_ == this))
      target.remove(this)
      // childViews.swap(Vector.empty).foreach(_.dispose())
    }
  }

  private[this] val playRef = Ref(List.empty[PlayTime])
  private[this] var obs: Disposable[S#Tx] = _

  def preferredNumChannels(implicit tx: S#Tx): Int = {
    @tailrec
    def loop(views: Vector[AuralAttribute[S]], res: Int): Int = views match {
      case head +: tail =>
        val ch = head.preferredNumChannels
        // if there is any child with `-1` (undefined), we have to return
        if (ch == -1) ch else loop(tail, math.max(res, ch))
      case _ => res
    }

    loop(childAttrRef(), -1)
  }

  // simply forward, for now we don't go into the details of checking
  // `preferredNumChannels`
  def attrNumChannelsChanged(attr: AuralAttribute[S])(implicit tx: S#Tx): Unit =
    observer.attrNumChannelsChanged(attr)

  def init(timeline: Timeline[S])(implicit tx: S#Tx): this.type = {
    val time0 = timeline.eventAfter(0L).getOrElse(-1L)

    if (time0 >= 0L) {
      val elemViews = timeline.intersect(time0).flatMap { case (_, elems) =>
        elems.map { elem =>
          AuralAttribute(key, elem, attr)
        }
      } .toVector
      childAttrRef() = elemViews
    }

    // views.foreach(_.init())
    obs = timeline.changed.react { implicit tx => upd => upd.changes.foreach {
      case Timeline.Added(span, entry) =>
        ???
//        val childAttr = AuralAttribute(key, child, attr)
//        childAttrRef.transform(_.patch(idx, childAttr :: Nil, 0))
//        playRef().foreach { p =>
//          // p.addChild(childAttr)
//          val tForce    = p.shiftTo(sched.time)
//          childAttr.play(tForce, p.target)
//        }

      case Timeline.Removed(span, entry) =>
        ???
//        childAttrRef.transform { in =>
//          val childAttr = in(idx)
//          childAttr.dispose()
//          in.patch(idx, Nil, 1)
//        }
      case Timeline.Moved(change, entry) =>
        ???
    }}
    this
  }

  def play(timeRef: TimeRef, target: Target)(implicit tx: S#Tx): Unit /* Instance */ = {
    val tForce  = timeRef.force
    // require(playRef.swap(Some(p)).isEmpty)
    val childAttrs  = childAttrRef()
    childAttrs.foreach { childAttr =>
      childAttr.play(tForce, target)
    }
    val p = new PlayTime(sched.time, tForce, target /* , Ref(childViews) */)
    playRef.transform(p :: _)
    target.add(p)
    // p
  }

  //    def prepare(timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
  //      val views = elemViewsRef()
  //      views.foreach(_.prepare(timeRef))
  //    }

  def dispose()(implicit tx: S#Tx): Unit = {
    obs.dispose()
    playRef.swap(Nil).foreach(_.dispose())
    val views = childAttrRef()
    views.foreach(_.dispose())
  }
}