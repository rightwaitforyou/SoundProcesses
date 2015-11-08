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
import de.sciss.synth.proc.AuralAttribute.Value

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.TMap

class AuralAttributeTargetImpl[S <: Sys[S]](target: NodeRef, key: String)
  extends AuralAttribute.Target[S] {

  private[this] val map = TMap.empty[AuralAttribute[S], Any]

  def put(source: AuralAttribute[S], value: Value)(implicit tx: S#Tx): Unit = {
    ???
  }

  def remove(source: AuralAttribute[S])(implicit tx: S#Tx): Unit = {
    ???
  }
}
