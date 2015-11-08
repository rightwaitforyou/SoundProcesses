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
import de.sciss.lucre.synth.{NodeRef, AudioBus, Sys}
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
//    DoubleVector        .typeID -> DoubleVectorAttribute,
//    Grapheme.Expr.Audio .typeID -> AudioGraphemeAttribute,
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

    private[this] var obs: Disposable[S#Tx] = _
    private[this] val playRef = Ref.make[PlayRef[S]]

    def play(timeRef: TimeRef, b: AuralAttributeTarget[S], numChannels: Int)(implicit tx: S#Tx): Unit = {
      val p = new PlayRef(b, numChannels)
      require(playRef.swap(p)(tx.peer) == null)
      update(p, exprH().value)
    }

    private[this] def update(p: PlayRef[S], value: A)(implicit tx: S#Tx): Unit = {
      import p.builder
      val ctlVal = floatValues(value)
      builder.add(this, ctlVal)
    }

    def init()(implicit tx: S#Tx): this.type = {
      obs = exprH().changed.react { implicit tx => change =>
        val p = playRef.get(tx.peer)
        if (p != null) update(p, change.now)
      }
      this
    }

    def prepare(timeRef: TimeRef)(implicit tx: S#Tx): Unit = ()

    def dispose()(implicit tx: S#Tx): Unit = {
      obs.dispose()
      val p = playRef.swap(null)(tx.peer)
      if (p != null) p.builder.remove(this)
    }
  }

  private[this] trait NumberImpl[S <: Sys[S], A] extends ExprImpl[S, A] {
    protected def floatValue(in: A): Float

    final def preferredNumChannels(implicit tx: S#Tx): Int = 1

    final protected def floatValues(in: A): Vec[Float] = Vector(floatValue(in))
  }
  
  // ------------------- IntObj ------------------- 

  private[this] object IntAttribute extends Factory {
    type Repr[S <: stm.Sys[S]] = IntObj[S]

    def typeID = IntObj.typeID

    def apply[S <: Sys[S]](value: IntObj[S])
                          (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
      new IntAttribute(tx.newHandle(value)).init()
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
      new DoubleAttribute(tx.newHandle(value)).init()
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
      new BooleanAttribute(tx.newHandle(value)).init()
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
      new FadeSpecAttribute(tx.newHandle(value)).init()
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
      new OutputAttribute().init(value)
  }
  private[this] final class OutputAttribute[S <: Sys[S]]()(implicit context: AuralContext[S])
    extends AuralAttribute[S] {

    private[this] val auralRef  = Ref.make[AuralOutput[S]]
    private[this] var obs: Disposable[S#Tx] = _
    private[this] val playRef   = Ref.make[PlayRef[S]]
    private[this] val aObsRef   = Ref.make[Disposable[S#Tx]]

    def preferredNumChannels(implicit tx: S#Tx): Int = {
      val a = auralRef.get(tx.peer)
      if (a == null) -1 else a.bus.numChannels
    }

    def init(output: Output[S])(implicit tx: S#Tx): this.type = {
      val id  = output.id // idH()
      obs = context.observeAux[AuralOutput[S]](id) { implicit tx => {
        case AuxAdded(_, auralOutput) => auralSeen(auralOutput)
      }}
      context.getAux[AuralOutput[S]](id).foreach(auralSeen)
      this
    }

    private[this] def auralSeen(auralOutput: AuralOutput[S])(implicit tx: S#Tx): Unit = {
      auralRef.set(auralOutput)(tx.peer)
      val aObs = auralOutput.react { implicit tx => {
        case AuralOutput.Play(n) =>
          val p = playRef.get(tx.peer)
          if (p != null) update(p, auralOutput)
        case AuralOutput.Stop => // XXX TODO: ignore?
      }}
      val aObsOld = aObsRef.swap(aObs)(tx.peer)
      if (aObsOld != null) aObsOld.dispose()
      val p = playRef.get(tx.peer)
      if (p != null) update(p, auralOutput)
    }

    def play(timeRef: TimeRef, builder: AuralAttributeTarget[S], numChannels: Int)(implicit tx: S#Tx): Unit = {
      val p = new PlayRef(builder, numChannels)
      require(playRef.swap(p)(tx.peer) == null)
      val a = auralRef.get(tx.peer)
      if (a != null) update(p, a)
    }

    private[this] def update(p: PlayRef[S], audioOutput: AuralOutput[S])(implicit tx: S#Tx): Unit =
      audioOutput.data.nodeOption.foreach(update(p, _, audioOutput.bus))

    private[this] def update(p: PlayRef[S], nodeRef: NodeRef, bus: AudioBus)(implicit tx: S#Tx): Unit = {
      import p.builder
      builder.add(this, nodeRef, bus)
    }

    def prepare(timeRef: TimeRef)(implicit tx: S#Tx): Unit = ()

    def dispose()(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      auralRef.set(null)
      val aObs = aObsRef.swap(null)(tx.peer)
      if (aObs != null) aObs.dispose()
      obs.dispose()
      val p = playRef.swap(null)(tx.peer)
      if (p != null) p.builder.remove(this)
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

      new FolderAttribute(Ref(elemViews)).init(value)
    }
  }
  private[this] final class FolderAttribute[S <: Sys[S]](elemViewsRef: Ref[Vector[AuralAttribute[S]]])
                                                        (implicit context: AuralContext[S])
    extends AuralAttribute[S] {

    import context.{scheduler => sched}

    private[this] val playRef = Ref.make[PlayTime[S]]
    private[this] var obs: Disposable[S#Tx] = _

    def preferredNumChannels(implicit tx: S#Tx): Int = {
      def loop(views: Vector[AuralAttribute[S]], res: Int): Int = views match {
        case head +: tail =>
          val ch = head.preferredNumChannels
          if (ch == -1) ch else loop(tail, math.max(res, ch))
        case _ => res
      }

      loop(elemViewsRef.get(tx.peer), -1)
    }

    def init(folder: Folder[S])(implicit tx: S#Tx): this.type = {
      // val views = elemViewsRef.get(tx.peer)
      // views.foreach(_.init())
      obs = folder.changed.react { implicit tx => upd => upd.changes.foreach {
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
      this
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
      obs.dispose()
      playRef.set(null)(tx.peer)
      val views = elemViewsRef.get(tx.peer)
      views.foreach(_.dispose())
    }
  }

  // ------------------- AudioGrapheme ------------------- 

//  private[this] object AudioGraphemeAttribute extends Factory {
//    type Repr[S <: stm.Sys[S]] = Grapheme.Expr.Audio[S]
//
//    def typeID = Grapheme.Expr.Audio.typeID
//
//    def apply[S <: Sys[S]](value: Grapheme.Expr.Audio[S])
//                          (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
//      new AudioGraphemeAttribute().init(value)
//  }
//  private[this] final class AudioGraphemeAttribute[S <: Sys[S]]()
//                                                               (implicit context: AuralContext[S])
//    extends AuralAttribute[S] {
//
//    def preferredNumChannels(implicit tx: S#Tx): Int = audioH().value.numChannels
//
//    def init(audio: Grapheme.Expr.Audio[S])(implicit tx: S#Tx): this.type = {
//      ...
//    }
//
//    def play(timeRef: TimeRef, builder: AuralAttributeTarget[S], numChannels: Int)(implicit tx: S#Tx): Unit = {
//      val ctlName   = graph.Attribute.controlName(key)
//        val audioVal  = a.value
//        val spec      = audioVal.spec
//        if (spec.numFrames != 1) {
//          sys.error(s"Audio grapheme $a must have exactly 1 frame to be used as scalar attribute")
//          // Console.err.println(s"Audio grapheme $a must have exactly 1 frame to be used as scalar attribute")
//          // throw MissingIn(AttributeKey(key))
//        }
//        val numCh = spec.numChannels // numChL.toInt
//        if (numCh > 4096) sys.error(s"Audio grapheme size ($numCh) must be <= 4096 to be used as scalar attribute")
//        chanCheck(numCh)
//        val bus = Bus.control(server, numCh)
//        val res = BusNodeSetter.mapper(ctlName, bus, b.node)
//        b.addUser(res)
//        val w = AudioArtifactScalarWriter(bus, audioVal)
//        b.addResource(w)
//      ...
//    }
//
//    def prepare(timeRef: TimeRef)(implicit tx: S#Tx): Unit = ...
//
//    def dispose()(implicit tx: S#Tx): Unit = {
//      ...
//    }
//  }
}