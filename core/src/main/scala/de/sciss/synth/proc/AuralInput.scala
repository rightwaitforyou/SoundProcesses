/*
 *  AuralInput.scala
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

import de.sciss.lucre.synth.{NodeRef, DynamicUser, Sys}
import impl.{AuralInputImpl => Impl}

object AuralInput {
  def attr[S <: Sys[S]](nodeRef: NodeRef, key: String, source: AuralOutput[S])(implicit tx: S#Tx): AuralInput[S] =
    Impl.attr(nodeRef, key, source)
}
trait AuralInput[S <: Sys[S]] extends /* Disposable[S#Tx] with */ DynamicUser {
  //   def addSource   (view: AuralOutput[S])(implicit tx: S#Tx): Unit
  //   def removeSource(view: AuralOutput[S])(implicit tx: S#Tx): Unit

  // def sourceUpdated(view: AuralOutput[S])(implicit tx: S#Tx): Unit

  def nodeRef: NodeRef
}