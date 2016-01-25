/*
 *  AuralTimelineImpl.scala
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
package impl

import de.sciss.lucre.bitemp.BiGroup
import de.sciss.lucre.data.SkipOctree
import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.geom.LongSpace
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{IdentifierMap, Obj}
import de.sciss.lucre.synth.Sys
import de.sciss.span.SpanLike

object AuralTimelineImpl {
   private type Leaf[S <: Sys[S]] = AuralTimelineBase.Leaf[S, AuralObj[S]]

  import AuralTimelineBase.spanToPoint

  def apply[S <: Sys[S]](timeline: Timeline[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Timeline[S] = {
    val system  = tx.system
    val res     = prepare[S, system.I](timeline, system)
    res.init(timeline)
  }

  /** An empty view that does not listen for events on the timeline. */
  def empty[S <: Sys[S]](tlObj: Timeline[S])
                        (implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Timeline.Manual[S] = {
    val system = tx.system
    val res = prepare[S, system.I](tlObj, system)
    res
  }

  private def prepare[S <: Sys[S], I1 <: stm.Sys[I1]](tlObj: Timeline[S], system: S { type I = I1 })
                                                     (implicit tx: S#Tx, context: AuralContext[S]): Impl[S, I1] = {
    implicit val iSys     = system.inMemoryTx _
    implicit val itx      = iSys(tx)
    implicit val pointView = (l: Leaf[S], tx: I1#Tx) => spanToPoint(l._1)
    implicit val dummyKeySer = DummySerializerFactory[system.I].dummySerializer[Leaf[S]]
    val tree = SkipOctree.empty[I1, LongSpace.TwoDim, Leaf[S]](BiGroup.MaxSquare)

    val viewMap = tx.newInMemoryIDMap[(stm.Source[S#Tx, S#ID], SpanLike, AuralObj[S])]
    val res = new Impl[S, I1](tx.newHandle(tlObj), tree, viewMap)
    res
  }

  private final class Impl[S <: Sys[S], I <: stm.Sys[I]](val obj: stm.Source[S#Tx, Timeline[S]],
                                                         protected val tree: SkipOctree[I, LongSpace.TwoDim, Leaf[S]],
                                                         protected val viewMap: IdentifierMap[S#ID, S#Tx, (stm.Source[S#Tx, S#ID], SpanLike, AuralObj[S])])
                                                        (implicit protected val context: AuralContext[S],
                                                         protected val iSys: S#Tx => I#Tx)
    extends AuralTimelineBase[S, I, Unit, AuralObj[S]] with AuralObj.Timeline.Manual[S] { impl =>

    protected def makeView(obj: Obj[S])(implicit tx: S#Tx): AuralObj[S] = AuralObj(obj)

    object contents extends ObservableImpl[S, AuralObj.Timeline.Update[S]] {
      def viewAdded(timed: S#ID, view: AuralObj[S])(implicit tx: S#Tx): Unit =
        fire(AuralObj.Timeline.ViewAdded(impl, timed, view))

      def viewRemoved(view: AuralObj[S])(implicit tx: S#Tx): Unit =
        fire(AuralObj.Timeline.ViewRemoved(impl, view))
    }

    protected def viewPlaying(h: ElemHandle)(implicit tx: S#Tx): Unit = contents.viewAdded  (h._1(), h._3)
    protected def viewStopped(h: ElemHandle)(implicit tx: S#Tx): Unit = contents.viewRemoved(h._3)
  }
}
