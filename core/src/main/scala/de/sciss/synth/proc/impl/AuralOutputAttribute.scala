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
import de.sciss.synth.proc.AuralAttribute.{Target, Observer, Factory}
import de.sciss.synth.proc.AuralContext.AuxAdded
import de.sciss.synth.proc.AuralView.{Stopped, Playing, Preparing, Prepared}
import de.sciss.synth.proc.{TimeRef, AuralOutput, AuralAttribute, AuralContext, Output}

import scala.concurrent.stm.Ref

// ------------------- Output ------------------- 

object AuralOutputAttribute extends Factory {
  type Repr[S <: stm.Sys[S]] = Output[S]

  def typeID = Output.typeID

  def apply[S <: Sys[S]](key: String, value: Output[S], observer: Observer[S])
                        (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
    new AuralOutputAttribute(key, tx.newHandle(value), observer).init(value)
}
final class AuralOutputAttribute[S <: Sys[S]](val key: String, val obj: stm.Source[S#Tx, Output[S]],
                                              observer: Observer[S])
                                             (implicit context: AuralContext[S])
  extends AuralAttributeImpl[S] { attr =>

  import TxnLike.peer

  def typeID = Output.typeID

  private[this] val auralRef  = Ref(Option.empty[AuralOutput[S]])
  private[this] var obs: Disposable[S#Tx] = _
  private[this] val playRef   = Ref(Option.empty[Target[S]])
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

  def prepare(timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
    state = Preparing
    ???
    state = Prepared
  }

  def play(timeRef: TimeRef, target: Target[S])(implicit tx: S#Tx): Unit /* Instance */ = {
    require (playRef.swap(Some(target)).isEmpty)
    // target.add(this)
    auralRef().foreach(update(target, _))
    state = Playing
    // p
  }

  def stop()(implicit tx: S#Tx): Unit = {
    ???
    state = Stopped
  }

  private[this] def update(target: Target[S], audioOutput: AuralOutput[S])(implicit tx: S#Tx): Unit = {
    val nodeRefOpt = audioOutput.view.nodeOption
    nodeRefOpt.foreach { nodeRef =>
      target.put(this, AuralAttribute.Stream(nodeRef, audioOutput.bus))
    }
  }

  //    def prepare(timeRef: TimeRef)(implicit tx: S#Tx): Unit = ()

  def dispose()(implicit tx: S#Tx): Unit = {
    auralRef.set(None)
    aObsRef.swap(None).foreach(_.dispose())
    obs.dispose()
    playRef.swap(None).foreach(_.remove(this))
  }
}
  