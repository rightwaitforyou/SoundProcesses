/*
 *  AuralAttributeImpl.scala
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

import de.sciss.lucre.expr.{BooleanObj, DoubleObj, Expr, IntObj}
import de.sciss.lucre.stm.{Disposable, Obj}
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.{expr, stm}
import de.sciss.synth.Curve
import de.sciss.synth.proc.AuralAttribute.Factory
import de.sciss.synth.proc.AuralContext.AuxAdded

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.Ref

object AuralAttributeImpl {
  private[this] val sync = new AnyRef

  def addFactory(f: Factory): Unit = sync.synchronized {
    val tid = f.typeID
    if (map.contains(tid)) throw new IllegalArgumentException(s"View factory for type $tid already installed")
    map += tid -> f
  }

  def factories: Iterable[Factory] = map.values

  def apply[S <: Sys[S]](value: Obj[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] = {
    val tid     = value.tpe.typeID
    val factory = map.getOrElse(tid, throw new IllegalArgumentException(s"No AuralAttribute available for $value"))
    factory(value.asInstanceOf[factory.Repr[S]])
  }

  private[this] var map = Map[Int, Factory](
    IntObj              .typeID -> IntAttribute,
    DoubleObj           .typeID -> DoubleAttribute,
    BooleanObj          .typeID -> BooleanAttribute,
    FadeSpec.Obj        .typeID -> FadeSpecAttribute,
//    DoubleVector        .typeID -> ...,
//    Grapheme.Expr.Audio .typeID -> ...,
    Output              .typeID -> OutputAttribute,
    Folder              .typeID -> FolderAttribute
//    Timeline            .typeID -> ...
  )

  private[this] final class PlayRef[S <: Sys[S]](val builder: AuralAttributeTarget[S], val numChannels: Int)

  private[this] final class PlayTime[S <: Sys[S]](val wallClock: Long,
                                                  val timeRef: TimeRef.Apply, val builder: AuralAttributeTarget[S],
                                                  val numChannels: Int) {
    def shiftTo(newWallClock: Long): TimeRef.Apply = timeRef.shift(newWallClock - wallClock)
  }

  private[this] trait ExprImpl[S <: Sys[S], A] extends AuralAttribute[S] {
    // ---- abstract ----

    protected def exprH: stm.Source[S#Tx, Expr[S, A]]

    // protected def controlSet(ctlName: String, value: A, numChannels: Int)(implicit tx: S#Tx): ControlSet
    protected def floatValues(in: A): Vec[Float]

    // ---- impl ----

    private[this] val obsRef  = Ref.make[Disposable[S#Tx]]
    private[this] val playRef = Ref.make[PlayRef[S]]

    def play(timeRef: TimeRef, b: AuralAttributeTarget[S], numChannels: Int)(implicit tx: S#Tx): Unit = {
      val p = new PlayRef(b, numChannels)
      require(playRef.swap(p)(tx.peer) == null)
      update(p, exprH().value)
    }

    private[this] def update(p: PlayRef[S], value: A)(implicit tx: S#Tx): Unit = {
      // val ctlVal  = floatValue(value)
      // val ctlName = graph.Attribute.controlName(key)
      import p.builder
      // val ctl: ControlSet = controlSet(ctlName, value, numChannels)
      // builder.setControl(ctl)
      val ctlVal = floatValues(value)
      builder.add(this, ctlVal)
    }

    def accept()(implicit tx: S#Tx): Unit = {
      val obs = exprH().changed.react { implicit tx => change =>
        update(playRef.get(tx.peer), change.now)
      }
      require(obsRef.swap(obs)(tx.peer) == null)
    }

    def prepare(timeRef: TimeRef)(implicit tx: S#Tx): Unit = ()

    def dispose()(implicit tx: S#Tx): Unit = {
      val obs = obsRef.swap(null)(tx.peer)
      if (obs != null) obs.dispose()
      playRef.set(null)(tx.peer)
    }
  }

  private[this] trait NumberImpl[S <: Sys[S], A] extends ExprImpl[S, A] {
    protected def floatValue(in: A): Float

    final def preferredNumChannels(implicit tx: S#Tx): Int = 1



//    final protected def controlSet(ctlName: String, value: A, numChannels: Int)(implicit tx: S#Tx): ControlSet = {
//      val f = floatValue(value)
//      if (numChannels == 1) ctlName -> f else ctlName -> Vector.fill(numChannels)(f)
//    }
    // protected def controlSet(ctlName: String, value: A, numChannels: Int)(implicit tx: S#Tx): ControlSet

    final protected def floatValues(in: A): Vec[Float] = Vector(floatValue(in))
  }
  
  // ------------------- IntObj ------------------- 

  private[this] object IntAttribute extends Factory {
    type Repr[S <: stm.Sys[S]] = IntObj[S]

    def typeID = IntObj.typeID

    def apply[S <: Sys[S]](value: IntObj[S])
                          (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
      new IntAttribute(tx.newHandle(value))
  }
  private[this] final class IntAttribute[S <: Sys[S]](protected val exprH: stm.Source[S#Tx, IntObj[S]])
    extends NumberImpl[S, Int] {

    protected def floatValue(value: Int): Float = value.toFloat
  }

  // ------------------- DoubleObj ------------------- 

  private[this] object DoubleAttribute extends Factory {
    type Repr[S <: stm.Sys[S]] = DoubleObj[S]

    def typeID = DoubleObj.typeID

    def apply[S <: Sys[S]](value: DoubleObj[S])
                          (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
      new DoubleAttribute(tx.newHandle(value))
  }
  private[this] final class DoubleAttribute[S <: Sys[S]](protected val exprH: stm.Source[S#Tx, DoubleObj[S]])
    extends NumberImpl[S, Double] {

    protected def floatValue(value: Double): Float = value.toFloat
  }

  // ------------------- BooleanObj ------------------- 

  private[this] object BooleanAttribute extends Factory {
    type Repr[S <: stm.Sys[S]] = BooleanObj[S]

    def typeID = BooleanObj.typeID

    def apply[S <: Sys[S]](value: BooleanObj[S])
                          (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
      new BooleanAttribute(tx.newHandle(value))
  }
  private[this] final class BooleanAttribute[S <: Sys[S]](protected val exprH: stm.Source[S#Tx, BooleanObj[S]])
    extends NumberImpl[S, Boolean] {

    protected def floatValue(value: Boolean): Float = if (value) 1f else 0f
  }

  // ------------------- FadeSpec.Obj ------------------- 

  private[this] object FadeSpecAttribute extends Factory {
    type Repr[S <: stm.Sys[S]] = FadeSpec.Obj[S]

    def typeID = FadeSpec.Obj.typeID

    def apply[S <: Sys[S]](value: FadeSpec.Obj[S])
                          (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
      new FadeSpecAttribute(tx.newHandle(value))
  }
  private[this] final class FadeSpecAttribute[S <: Sys[S]](protected val exprH: stm.Source[S#Tx, FadeSpec.Obj[S]])
    extends ExprImpl[S, FadeSpec] {

    def preferredNumChannels(implicit tx: S#Tx): Int = 4

    protected def floatValues(spec: FadeSpec): Vec[Float] = Vector(
      (spec.numFrames / Timeline.SampleRate).toFloat, spec.curve.id.toFloat, spec.curve match {
        case Curve.parametric(c)  => c
        case _                    => 0f
      }, spec.floor
    )
  }

  // ------------------- Output ------------------- 

  private[this] object OutputAttribute extends Factory {
    type Repr[S <: stm.Sys[S]] = Output[S]

    def typeID = Output.typeID

    def apply[S <: Sys[S]](value: Output[S])
                          (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
      new OutputAttribute(tx.newHandle(value.id))
  }
  private[this] final class OutputAttribute[S <: Sys[S]](idH: stm.Source[S#Tx, S#ID])
                                                        (implicit context: AuralContext[S])
    extends AuralAttribute[S] {

    private[this] val auralRef  = Ref.make[AuralOutput[S]]
    private[this] val obsRef    = Ref.make[Disposable[S#Tx]]

    def preferredNumChannels(implicit tx: S#Tx): Int = {
      val a = auralRef.get(tx.peer)
      if (a == null) -1 else a.bus.numChannels
    }

    def accept()(implicit tx: S#Tx): Unit = {
      val id  = idH()
      val obs = context.observeAux[AuralOutput[S]](id) { implicit tx => {
        case AuxAdded(_, auralOutput) => auralSeen(auralOutput)
      }}
      context.getAux[AuralOutput[S]](id).foreach(auralSeen)
      require(obsRef.swap(obs)(tx.peer) == null)
    }

    private[this] def auralSeen(auralOutput: AuralOutput[S])(implicit tx: S#Tx): Unit = {
      ???
    }

    def play(timeRef: TimeRef, builder: AuralAttributeTarget[S], numChannels: Int)(implicit tx: S#Tx): Unit = {

      ???
    }

    def prepare(timeRef: TimeRef)(implicit tx: S#Tx): Unit = ()

    def dispose()(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      auralRef.set(null)
      val obs = obsRef.swap(null)
      if (obs != null) obs.dispose()
    }
  }

  // ------------------- Folder ------------------- 

  private[this] object FolderAttribute extends Factory {
    type Repr[S <: stm.Sys[S]] = Folder[S]

    def typeID = Folder.typeID

    def apply[S <: Sys[S]](value: Folder[S])
                          (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] = {

      val elemViews = value.iterator.map { elem =>
        AuralAttribute(elem)
      } .toVector

      new FolderAttribute(tx.newHandle(value), Ref(elemViews))
    }
  }
  private[this] final class FolderAttribute[S <: Sys[S]](folderH: stm.Source[S#Tx, Folder[S]],
                                                         elemViewsRef: Ref[Vector[AuralAttribute[S]]])
                                                        (implicit context: AuralContext[S])
    extends AuralAttribute[S] {

    import context.{scheduler => sched}

    private[this] val playRef = Ref.make[PlayTime[S]]
    private[this] val obsRef  = Ref.make[Disposable[S#Tx]]

    def preferredNumChannels(implicit tx: S#Tx): Int = {
      def loop(views: Vector[AuralAttribute[S]], res: Int): Int = views match {
        case head +: tail =>
          val ch = head.preferredNumChannels
          if (ch == -1) ch else loop(tail, math.max(res, ch))
        case _ => res
      }

      loop(elemViewsRef.get(tx.peer), -1)
    }

    def accept()(implicit tx: S#Tx): Unit = {
      val views = elemViewsRef.get(tx.peer)
      views.foreach(_.accept())
      val obs = folderH().changed.react { implicit tx => upd => upd.changes.foreach {
        case expr.List.Added  (idx, child) =>
          val view = AuralAttribute(child)
          elemViewsRef.transform(_.patch(idx, view :: Nil, 0))(tx.peer)
          val p = playRef.get(tx.peer)
          if (p != null) {
            import p.{builder, numChannels}
            val tForce = p.shiftTo(sched.time)
            view.play(tForce, builder, numChannels)
          }

        case expr.List.Removed(idx, child) =>
          elemViewsRef.transform { in =>
            val view = in(idx)
            view.dispose()
            in.patch(idx, Nil, 1)
          } (tx.peer)
      }}
      require(obsRef.swap(obs)(tx.peer) == null)
    }

    def play(timeRef: TimeRef, builder: AuralAttributeTarget[S], numChannels: Int)(implicit tx: S#Tx): Unit = {
      val tForce  = timeRef.force
      val p       = new PlayTime(sched.time, tForce, builder, numChannels)
      require(playRef.swap(p)(tx.peer) == null)
      val views = elemViewsRef.get(tx.peer)
      views.foreach(_.play(tForce, builder ,numChannels))
    }

    def prepare(timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
      val views = elemViewsRef.get(tx.peer)
      views.foreach(_.prepare(timeRef))
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      val obs = obsRef.swap(null)(tx.peer)
      if (obs != null) obs.dispose()
      playRef.set(null)(tx.peer)
      val views = elemViewsRef.get(tx.peer)
      views.foreach(_.dispose())
    }
  }
}