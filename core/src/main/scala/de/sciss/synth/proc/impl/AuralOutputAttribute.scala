/*
 *  AuralOutputAttribute.scala
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

package de.sciss.synth.proc.impl

import de.sciss.lucre.stm
import de.sciss.lucre.stm.{TxnLike, Disposable}
import de.sciss.lucre.synth.{AudioBus, NodeRef, Txn, Sys}
import de.sciss.synth.proc.AuralAttribute.{Instance, Target, Observer, Factory}
import de.sciss.synth.proc.AuralContext.AuxAdded
import de.sciss.synth.proc.{TimeRef, AuralOutput, AuralAttribute, AuralContext, Output}

import scala.concurrent.stm.Ref

// ------------------- Output ------------------- 

object AuralOutputAttribute extends Factory {
  type Repr[S <: stm.Sys[S]] = Output[S]

  def typeID = Output.typeID

  def apply[S <: Sys[S]](key: String, value: Output[S], observer: Observer[S])
                        (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
    new AuralOutputAttribute(key, observer).init(value)
}
final class AuralOutputAttribute[S <: Sys[S]](val key: String, observer: Observer[S])
                                                      (implicit context: AuralContext[S])
  extends AuralAttribute[S] { attr =>

  import TxnLike.peer

  private[this] final class PlayRef(val target: Target)
    extends Instance {

    def dispose()(implicit tx: Txn): Unit = {
      playRef.transform(_.filterNot(_ == this))
      target.remove(this)
    }
  }

  private[this] val auralRef  = Ref(Option.empty[AuralOutput[S]])
  private[this] var obs: Disposable[S#Tx] = _
  private[this] val playRef   = Ref(List.empty[PlayRef])
  private[this] val aObsRef   = Ref(Option.empty[Disposable[S#Tx]])

  def preferredNumChannels(implicit tx: S#Tx): Int =
    auralRef().fold(-1)(_.bus.numChannels)

  def init(output: Output[S])(implicit tx: S#Tx): this.type = {
    val id  = output.id // idH()
    obs = context.observeAux[AuralOutput[S]](id) { implicit tx => {
        case AuxAdded(_, auralOutput) =>
        auralSeen(auralOutput)
        playRef().foreach(update(_, auralOutput))
        observer.attrNumChannelsChanged(this)
      }}
    context.getAux[AuralOutput[S]](id).foreach(auralSeen)
    this
  }

  private[this] def auralSeen(auralOutput: AuralOutput[S])(implicit tx: S#Tx): Unit = {
    auralRef() = Some(auralOutput)
    val aObs = auralOutput.react { implicit tx => {
      case AuralOutput.Play(n) =>
        playRef().foreach(update(_, auralOutput))
      case AuralOutput.Stop => // XXX TODO: ignore?
    }}
    aObsRef.swap(Some(aObs)).foreach(_.dispose())
    playRef().foreach(update(_, auralOutput))
  }

  def play(timeRef: TimeRef, target: Target)(implicit tx: S#Tx): Unit /* Instance */ = {
    val p = new PlayRef(target)
    playRef.transform(p :: _)
    target.add(p)
    auralRef().foreach(update(p, _))
    // p
  }

  private[this] def update(p: PlayRef, audioOutput: AuralOutput[S])(implicit tx: S#Tx): Unit = {
    val nodeRefOpt = audioOutput.data.nodeOption
    nodeRefOpt.foreach(update1(p, _, audioOutput.bus))
  }

  private[this] def update1(p: PlayRef, nodeRef: NodeRef, bus: AudioBus)(implicit tx: S#Tx): Unit = {
    import p.target
    target.put(p, AuralAttribute.Stream(nodeRef, bus))
  }

  //    def prepare(timeRef: TimeRef)(implicit tx: S#Tx): Unit = ()

  def dispose()(implicit tx: S#Tx): Unit = {
    auralRef.set(None)
    aObsRef.swap(None).foreach(_.dispose())
    obs.dispose()
    playRef().foreach(_.dispose())
  }
}
  