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

import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.expr.{BooleanObj, DoubleObj, Expr, IntObj}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Obj, TxnLike}
import de.sciss.lucre.synth.Sys
import de.sciss.synth.Curve
import de.sciss.synth.proc.AuralAttribute.{Factory, Observer, Target}
import de.sciss.synth.proc.AuralView.{Playing, Prepared, State, Stopped}

import scala.concurrent.stm.Ref

object AuralAttributeImpl {
  private[this] val sync = new AnyRef

  import TxnLike.peer

  def addFactory(f: Factory): Unit = sync.synchronized {
    val tid = f.typeID
    if (map.contains(tid)) throw new IllegalArgumentException(s"View factory for type $tid already installed")
    map += tid -> f
  }

  def factories: Iterable[Factory] = map.values

  def apply[S <: Sys[S]](key: String, value: Obj[S], observer: Observer[S])
                        (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] = {
    val tid     = value.tpe.typeID
    val factory = map.getOrElse(tid, throw new IllegalArgumentException(s"No AuralAttribute available for $value"))
    factory(key, value.asInstanceOf[factory.Repr[S]], observer)
  }

  private[this] var map = Map[Int, Factory](
    IntObj              .typeID -> IntAttribute,
    DoubleObj           .typeID -> DoubleAttribute,
    BooleanObj          .typeID -> BooleanAttribute,
    FadeSpec.Obj        .typeID -> FadeSpecAttribute,
//    DoubleVector        .typeID -> DoubleVectorAttribute,
//    Grapheme.Expr.Audio .typeID -> AudioGraphemeAttribute,
    Output              .typeID -> AuralOutputAttribute,
    Folder              .typeID -> AuralFolderAttribute,
    Timeline            .typeID -> AuralTimelineAttribute
  )

  // private[this] final class PlayRef[S <: Sys[S]](val target: Target)

  // private[this] type PlayRef[S <: Sys[S]] = Target

  private[this] trait ExprImpl[S <: Sys[S], A]
    extends AuralAttributeImpl[S] { attr =>

    // ---- abstract ----

    /* override */ def obj: stm.Source[S#Tx, Expr[S, A]]

    protected def mkValue(in: A): AuralAttribute.Value

    // ---- impl ----

    private[this] var obs: Disposable[S#Tx] = _
    private /* [this] */ val playRef = Ref[Option[Target[S]]](None)  // private[this] crashes Scala 2.10 !

    final def prepare(timeRef: TimeRef)(implicit tx: S#Tx): Unit = state = Prepared

    final def play(timeRef: TimeRef, target: Target[S])(implicit tx: S#Tx): Unit /* Instance */ = {
      require(playRef.swap(Some(target))(tx.peer).isEmpty)
      // target.add(this)
      state = Playing
      update(target, obj().value)
    }

    final def stop()(implicit tx: S#Tx): Unit = {
      stopNoFire()
      state = Stopped
    }

    private[this] def update(target: Target[S], value: A)(implicit tx: S#Tx): Unit = {
      // import p.target
      val ctlVal = mkValue(value)
      target.put(this, ctlVal)
    }

    def init(expr: Expr[S, A])(implicit tx: S#Tx): this.type = {
      obs = expr.changed.react { implicit tx => change =>
        playRef().foreach(update(_, change.now))
      }
      this
    }

//    def prepare(timeRef: TimeRef)(implicit tx: S#Tx): Unit = ()

    private[this] def stopNoFire()(implicit tx: S#Tx): Unit =
      playRef.swap(None).foreach(_.remove(this))

    def dispose()(implicit tx: S#Tx): Unit = {
      obs.dispose()
      stopNoFire()
    }
  }

  private[this] trait NumberImpl[S <: Sys[S], A] extends ExprImpl[S, A] {
    final def preferredNumChannels(implicit tx: S#Tx): Int = 1
  }
  
  // ------------------- IntObj ------------------- 

  private[this] object IntAttribute extends Factory {
    type Repr[S <: stm.Sys[S]] = IntObj[S]

    def typeID = IntObj.typeID

    def apply[S <: Sys[S]](key: String, value: IntObj[S], observer: Observer[S])
                          (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
      new IntAttribute(key, tx.newHandle(value)).init(value)
  }
  private[this] final class IntAttribute[S <: Sys[S]](val key: String,
                                                      val obj: stm.Source[S#Tx, IntObj[S]])
    extends NumberImpl[S, Int] {

    def typeID = IntObj.typeID

    protected def mkValue(value: Int): AuralAttribute.Value = value.toFloat
  }

  // ------------------- DoubleObj ------------------- 

  private[this] object DoubleAttribute extends Factory {
    type Repr[S <: stm.Sys[S]] = DoubleObj[S]

    def typeID = DoubleObj.typeID

    def apply[S <: Sys[S]](key: String, value: DoubleObj[S], observer: Observer[S])
                          (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
      new DoubleAttribute(key, tx.newHandle(value)).init(value)
  }
  private[this] final class DoubleAttribute[S <: Sys[S]](val key: String,
                                                         val obj: stm.Source[S#Tx, DoubleObj[S]])
    extends NumberImpl[S, Double] {

    def typeID = DoubleObj.typeID

    protected def mkValue(value: Double): AuralAttribute.Value = value.toFloat
  }

  // ------------------- BooleanObj ------------------- 

  private[this] object BooleanAttribute extends Factory {
    type Repr[S <: stm.Sys[S]] = BooleanObj[S]

    def typeID = BooleanObj.typeID

    def apply[S <: Sys[S]](key: String, value: BooleanObj[S], observer: Observer[S])
                          (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
      new BooleanAttribute(key, tx.newHandle(value)).init(value)
  }
  private[this] final class BooleanAttribute[S <: Sys[S]](val key: String,
                                                          val obj: stm.Source[S#Tx, BooleanObj[S]])
    extends NumberImpl[S, Boolean] {

    def typeID = BooleanObj.typeID

    protected def mkValue(value: Boolean): AuralAttribute.Value = if (value) 1f else 0f
  }

  // ------------------- FadeSpec.Obj ------------------- 

  private[this] object FadeSpecAttribute extends Factory {
    type Repr[S <: stm.Sys[S]] = FadeSpec.Obj[S]

    def typeID = FadeSpec.Obj.typeID

    def apply[S <: Sys[S]](key: String, value: FadeSpec.Obj[S], observer: Observer[S])
                          (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
      new FadeSpecAttribute(key, tx.newHandle(value)).init(value)
  }
  private[this] final class FadeSpecAttribute[S <: Sys[S]](val key: String,
                                                           val obj: stm.Source[S#Tx, FadeSpec.Obj[S]])
    extends ExprImpl[S, FadeSpec] {

    def typeID = FadeSpec.Obj.typeID

    def preferredNumChannels(implicit tx: S#Tx): Int = 4

    protected def mkValue(spec: FadeSpec): AuralAttribute.Value = Vector[Float](
      (spec.numFrames / TimeRef.SampleRate).toFloat, spec.curve.id.toFloat, spec.curve match {
        case Curve.parametric(c)  => c
        case _                    => 0f
      }, spec.floor
    )
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
//    def play(timeRef: TimeRef, builder: AuralAttributeTarget, numChannels: Int)(implicit tx: S#Tx): Unit = {
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
trait AuralAttributeImpl[S <: Sys[S]] extends AuralAttribute[S] with ObservableImpl[S, AuralView.State] {
  import TxnLike.peer

  private[this] final val stateRef = Ref[State](Stopped)

  final def state(implicit tx: S#Tx): State = stateRef()
  final protected def state_=(value: State)(implicit tx: S#Tx): Unit = {
    val prev = stateRef.swap(value)
    if (value != prev) fire(value)
  }
}