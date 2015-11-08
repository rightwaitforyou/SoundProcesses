/*
 *  AuralAttributeTargetImpl.scala
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

import de.sciss.lucre.synth.{AudioBus, NodeRef, Sys}
import de.sciss.synth
import de.sciss.synth.SynthGraph
import de.sciss.synth.proc.AuralAttribute.Value

import scala.concurrent.stm.TMap

class AuralAttributeTargetImpl[S <: Sys[S]](target: NodeRef, key: String, targetBus: AudioBus)
  extends AuralAttribute.Target[S] {

  private[this] val map = TMap.empty[AuralAttribute[S], Any]

  def put(source: AuralAttribute[S], value: Value)(implicit tx: S#Tx): Unit = {
    implicit val itx = tx.peer

    // cases
    // - map was empty | map size was one and value replaces previous value
    //   - value is scalar -> stay scalar
    //   - value is stream -> establish stream
    // - map size was one and value is new
    //   -

    map.put(source, value)

    value match {
      case sc: AuralAttribute.Scalar =>
        target.node.set(sc.toControl(key))
      case AuralAttribute.Stream(sourceNode, sourceBus) =>
    }

    ???
  }

  private[this] def lift(in: AuralAttribute.Scalar)(implicit tx: S#Tx): AuralAttribute.Stream = {
    val g = SynthGraph {
      import synth._
      import ugen._

    }
    ???
  }

  def remove(source: AuralAttribute[S])(implicit tx: S#Tx): Unit = {

    ???
  }
}
