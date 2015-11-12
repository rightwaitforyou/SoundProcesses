/*
 *  AuralFolderAttribute.scala
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

import de.sciss.lucre.{expr, stm}
import de.sciss.lucre.stm.{TxnLike, Disposable}
import de.sciss.lucre.synth.{Txn, Sys}
import de.sciss.synth.proc.AuralAttribute.{Target, Observer, Factory}
import de.sciss.synth.proc.AuralView.{Prepared, Preparing, Stopped, Playing}

import scala.annotation.tailrec
import scala.concurrent.stm.Ref

object AuralFolderAttribute extends Factory {
  type Repr[S <: stm.Sys[S]] = Folder[S]

  def typeID = Folder.typeID

  def apply[S <: Sys[S]](key: String, value: Folder[S], observer: Observer[S])
                        (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
    new AuralFolderAttribute(key, tx.newHandle(value), observer).init(value)
}
final class AuralFolderAttribute[S <: Sys[S]](val key: String, val obj: stm.Source[S#Tx, Folder[S]],
                                              observer: Observer[S])
                                             (implicit context: AuralContext[S])
  extends AuralAttributeImpl[S] with AuralAttribute.Observer[S] { attr =>

  import TxnLike.peer

  import context.{scheduler => sched}

  def typeID = Folder.typeID

  private[this] val childAttrRef = Ref.make[Vector[AuralAttribute[S]]]

  private[this] final class PlayTime(val wallClock: Long,
                                     val timeRef: TimeRef.Apply, val target: Target[S]) {

    def shiftTo(newWallClock: Long): TimeRef.Apply = timeRef.shift(newWallClock - wallClock)

//    def dispose()(implicit tx: Txn): Unit = {
//      playRef.transform(_.filterNot(_ == this))
//      target.remove(this)
//      // childViews.swap(Vector.empty).foreach(_.dispose())
//    }

    //      def addChild(child: AuralAttribute[S])(implicit tx: S#Tx): Unit = {
    //        val tForce    = shiftTo(sched.time)
    //        val childView = child.play(tForce, target)
    //        // childViews.transform(_ :+ childView)
    //      }
  }

  private[this] val playRef = Ref(Option.empty[PlayTime])
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

  def init(folder: Folder[S])(implicit tx: S#Tx): this.type = {
    val elemViews = folder.iterator.map { elem =>
      AuralAttribute(key, elem, attr)
    } .toVector
    childAttrRef() = elemViews

    // views.foreach(_.init())
    obs = folder.changed.react { implicit tx => upd => upd.changes.foreach {
      case expr.List.Added  (idx, child) =>
        val childAttr = AuralAttribute(key, child, attr)
        childAttrRef.transform(_.patch(idx, childAttr :: Nil, 0))
        playRef().foreach { p =>
          // p.addChild(childAttr)
          val tForce    = p.shiftTo(sched.time)
          childAttr.play(tForce, p.target)
        }

      case expr.List.Removed(idx, child) =>
        childAttrRef.transform { in =>
          val childAttr = in(idx)
          childAttr.dispose()
          in.patch(idx, Nil, 1)
        }
    }}
    this
  }

  def prepare(timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
    state = Preparing
    ???
    state = Prepared
  }

  def play(timeRef: TimeRef, target: Target[S])(implicit tx: S#Tx): Unit /* Instance */ = {
    val tForce  = timeRef.force
    // require(playRef.swap(Some(p)).isEmpty)
    val childAttrs  = childAttrRef()
    childAttrs.foreach { childAttr =>
      childAttr.play(tForce, target)
    }
    val p = new PlayTime(sched.time, tForce, target /* , Ref(childViews) */)
    require(playRef.swap(Some(p)).isEmpty)
    target.add(this)
    state = Playing
    // p
  }

  def stop()(implicit tx: S#Tx): Unit = {
    ???
    state = Stopped
  }

  //    def prepare(timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
  //      val views = elemViewsRef()
  //      views.foreach(_.prepare(timeRef))
  //    }

  def dispose()(implicit tx: S#Tx): Unit = {
    obs.dispose()
    playRef.swap(None).foreach(_.target.remove(this))
    val views = childAttrRef()
    views.foreach(_.dispose())
  }
}
